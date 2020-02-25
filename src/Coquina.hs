{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Coquina where

import Control.Concurrent (MVar, newEmptyMVar, forkIO, putMVar, takeMVar, killThread)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Concurrent.STM
import Control.DeepSeq (rnf)
import Control.Exception (SomeException, evaluate, mask, try, throwIO, onException)
import Control.Monad.Except (MonadError, ExceptT, throwError, runExceptT)
import Control.Monad.Writer
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (fold)
import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T (unpack)
import qualified Data.Text.Encoding as T (decodeUtf8)
import GHC.IO.Handle (Handle, hSetBuffering, BufferMode(..), hIsOpen, hIsReadable, hClose, hGetContents)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process

-- | A class that supports reading and writing stdout and stderr
class Monad m => MonadShell m where
  tellOutput :: (String, String) -> m ()
  readOutput :: m a -> m ((String, String), a)

-- | Write to stdout
tellStdout :: MonadShell m => String -> m ()
tellStdout s = tellOutput (s, mempty)

-- | Write to stderr
tellStderr :: MonadShell m => String -> m ()
tellStderr s = tellOutput (mempty, s)

-- | Read the stdout of a command
readStdout :: MonadShell m => m a -> m (String, a)
readStdout f = do
  ((out, _), a) <- readOutput f
  return (out, a)

-- | Read the stderr of a command
readStderr :: MonadShell m => m a -> m (String, a)
readStderr f = do
  ((_, err), a) <- readOutput f
  return (err, a)

-- | An action that supports running commands, reading their output, and emmitting output
newtype Shell m a = Shell { unShell :: ExceptT Int (WriterT (String, String) m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Int, MonadThrow, MonadCatch, MonadMask)

instance MonadTrans Shell where
  lift = Shell . lift . lift

instance Monad m => MonadShell (Shell m) where
  tellOutput = Shell . tell
  readOutput f = Shell $ do
    (a, out) <- listen $ unShell f
    return (out, a)

instance MonadWriter w m => MonadWriter w (Shell m) where
  tell = lift . tell
  -- NB: If the Shell action fails, the listen fails as well
  listen x = do
    ((out, err, r), w) <- lift $ listen $ runShell x
    tellOutput (out, err)
    case r of
      Left ec -> throwError ec
      Right v -> return (v, w)
  pass a = do
    (out, err, e) <- lift $ pass $ do
      runShell a >>= \case
        (out, err, Left ec) -> return ((out, err, Left ec), id)
        (out, err, Right (x, f)) -> return ((out, err, Right x), f)
    tellOutput (out, err)
    case e of
      Left ec -> throwError ec
      Right v -> return v

-- | Run a shell action, producing stdout, stderr, and a result.
runShell :: Monad m => Shell m a -> m (String, String, Either Int a)
runShell (Shell s) = do
  (e, (out, err)) <- runWriterT $ runExceptT s
  return (out, err, e)

-- | Run a shell action, producing an exit code, stdout, and stderr
execShell :: Monad m => Shell m a -> m (ExitCode, String, String)
execShell s = do
  (out, err, r) <- runShell s
  case r of
    Left ec -> return (ExitFailure ec, out, err)
    Right _ -> return (ExitSuccess, out, err)

-- | Run a 'CreateProcess' in a 'Shell'
shellCreateProcess :: MonadIO m => CreateProcess -> Shell m ()
shellCreateProcess = shellCreateProcessWithEnv mempty

-- | Run a 'CreateProcess' in a 'Shell'
run :: MonadIO m => CreateProcess -> Shell m ()
run = shellCreateProcess

-- | Represents a process that is running and whose incremental output can
-- be retrieved before it completes. The '_streamingProcess_waitForProcess'
-- finalizer can be called to get the exit status of the process and to get
-- the final output.
data StreamingProcess m = StreamingProcess
  { _streamingProcess_stdout :: STM ByteString
  , _streamingProcess_stderr :: STM ByteString
  , _streamingProcess_waitForProcess :: Shell m ExitCode
  , _streamingProcess_processHandle :: ProcessHandle
  }

-- | A process whose output can be inspected while it is still running.
shellStreamableProcess
  :: MonadIO m
  => CreateProcess
  -> Shell m (StreamingProcess m)
shellStreamableProcess p = do
  (_, mout, merr, ph) <- liftIO $ createProcess $ p
    { std_out = CreatePipe
    , std_err = CreatePipe
    }
  stdout <- liftIO newTChanIO
  stderr <- liftIO newTChanIO
  stdoutAcc <- liftIO $ newIORef mempty
  stderrAcc <- liftIO $ newIORef mempty
  case (mout, merr) of
    (Just hout, Just herr) -> do
    -- TODO: This code is basically the same as that in Reflex.Process.createProcess, except for the action to take when new output is received
      let handleReader h c r = do
            hSetBuffering h LineBuffering
            fix $ \go -> do
              open <- hIsOpen h
              when open $ do
                readable <- hIsReadable h
                when readable $ do
                  out <- BS.hGetSome h (2^(15 :: Int))
                  if BS.null out
                    then return ()
                    else do
                      atomically $ writeTChan c out
                      atomicModifyIORef' r (\v -> (v <> BS.byteString out, ()))
                      go
      _ <- liftIO $ forkIO $ handleReader hout stdout stdoutAcc -- TODO: Proper thread resource handling
      _ <- liftIO $ forkIO $ handleReader herr stderr stderrAcc -- TODO: Proper thread resource handling
      let finalize = do
            exitCode <- liftIO $ waitForProcess ph
            stdoutFinal <- liftIO $ LBS.toStrict . BS.toLazyByteString <$> readIORef stdoutAcc
            stderrFinal <- liftIO $ LBS.toStrict . BS.toLazyByteString <$> readIORef stderrAcc
            tellOutput (unpack stdoutFinal, unpack stderrFinal)
            return exitCode
      return $ StreamingProcess
        { _streamingProcess_stdout = fold <$> drainTChan stdout
        , _streamingProcess_stderr = fold <$> drainTChan stderr
        , _streamingProcess_waitForProcess = finalize
        , _streamingProcess_processHandle = ph
        }
    _ -> error "shellStreamingProcess: Created pipes were not returned"
    where
      unpack = T.unpack . T.decodeUtf8
      drainTChan chan = flip fix mempty $ \loop acc -> tryReadTChan chan >>= \case
        Nothing -> pure acc
        Just x -> loop $ acc Seq.:|> x

-- | Run a shell process using the given runner function
shellCreateProcess'
  :: MonadIO m
  => (CreateProcess -> IO (ExitCode, String, String))
  -> CreateProcess
  -> Shell m ()
shellCreateProcess' f p = do
  (ex, out, err) <- liftIO $ f p
  tellOutput (out, err)
  case ex of
    ExitFailure c -> do
      liftIO $ putStrLn $ mconcat $
        [ "Command failed: "
        , showCommand p
        , "\n"
        , err
        ]
      throwError c
    ExitSuccess -> return ()

-- | Run a shell process with the given environment variables added to the existing environment
shellCreateProcessWithEnv
  :: MonadIO m
  => Map String String
  -> CreateProcess
  -> Shell m ()
shellCreateProcessWithEnv envOverrides = shellCreateProcess' f
  where
    f cmd = do
      envWithOverrides <- liftIO $ if Map.null envOverrides
        then return $ env cmd
        else Just . Map.toList . Map.union envOverrides . Map.fromList <$> getEnvironment
      readCreateProcessWithExitCode (cmd { env = envWithOverrides}) ""

runCreateProcessWithEnv :: Map String String -> CreateProcess -> IO (ExitCode, String, String)
runCreateProcessWithEnv menv p = execShell $ shellCreateProcessWithEnv menv p

runCreateProcess :: CreateProcess -> IO (ExitCode, String, String)
runCreateProcess = runCreateProcessWithEnv mempty

-- | Run a shell process with stdout directed to the provided handle
shellCreateProcessWithStdOut
  :: MonadIO m
  => Handle
  -> CreateProcess
  -> Shell m ()
shellCreateProcessWithStdOut hndl cp = do
  let cp' = cp { std_out = UseHandle hndl, std_err = CreatePipe }
  shellCreateProcess' f cp'
  where
    f cmd = withCreateProcess cmd $ \_ _ merr p -> case merr of
      Just errh -> do
        err <- waitReadHandle errh
        ec <- waitForProcess p
        hClose hndl
        return (ec, "", err)
      _ -> error "shellCreateProcessWithStdOut: Failed to get std_err handle"
    waitReadHandle :: Handle -> IO String
    waitReadHandle h = do
      c <- hGetContents h
      withForkWait (evaluate $ rnf c) $ \wait -> wait >> hClose h
      return c
    -- The code below is taken from System.Process which unfortunately does not export this function
    withForkWait :: IO () -> (IO () ->  IO a) -> IO a
    withForkWait async body = do
      waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
      mask $ \restore -> do
        tid <- forkIO $ try (restore async) >>= putMVar waitVar
        let wait = takeMVar waitVar >>= either throwIO return
        restore (body wait) `onException` killThread tid

-- | Run a shell command with access to a temporary directory
inTempDirectory
  :: MonadIO m
  => String
  -> (FilePath -> Shell IO a)
  -> Shell m a
inTempDirectory label f = do
  (out, err, r) <- liftIO $ withSystemTempDirectory label $ \fp -> runShell $ f fp
  tellOutput (out, err)
  case r of
    Left ec -> throwError ec
    Right x -> return x

-- | Print a shell command
logCommand :: CreateProcess -> IO ()
logCommand = putStrLn . showCommand

-- | Convert a shell command to a string
showCommand :: CreateProcess -> String
showCommand p = case cmdspec p of
  ShellCommand str -> str
  RawCommand exe args -> mconcat $ intersperse " " $ exe : args
