{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Coquina where

import Coquina.Internal (withForkWait, readAndDecodeCreateProcess)

import qualified Control.Concurrent.Async as Async
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, finally)
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad.Except (MonadError, ExceptT, throwError, runExceptT)
import Control.Monad.Writer
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.IO as T (putStrLn)
import GHC.Generics (Generic)
import GHC.IO.Handle (Handle, hSetBuffering, BufferMode(..), hIsOpen, hIsReadable, hClose)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process

-- | A class that supports reading and writing stdout and stderr
class Monad m => MonadShell m where
  tellOutput :: (Text, Text) -> m ()
  readOutput :: m a -> m ((Text, Text), a)

-- | Write to stdout
tellStdout :: MonadShell m => Text -> m ()
tellStdout s = tellOutput (s, mempty)

-- | Write to stderr
tellStderr :: MonadShell m => Text -> m ()
tellStderr s = tellOutput (mempty, s)

-- | Read the stdout of a command
readStdout :: MonadShell m => m a -> m (Text, a)
readStdout f = do
  ((out, _), a) <- readOutput f
  return (out, a)

-- | Read the stderr of a command
readStderr :: MonadShell m => m a -> m (Text, a)
readStderr f = do
  ((_, err), a) <- readOutput f
  return (err, a)

-- | An action that supports running commands, reading their output, and emitting output
newtype Shell m a = Shell { unShell :: ExceptT Int (WriterT (Text, Text) m) a }
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
    (out, err, e) <- lift $ pass $
      runShell a >>= \case
        (out, err, Left ec) -> return ((out, err, Left ec), id)
        (out, err, Right (x, f)) -> return ((out, err, Right x), f)
    tellOutput (out, err)
    case e of
      Left ec -> throwError ec
      Right v -> return v

-- | Run a shell action, producing stdout, stderr, and a result.
runShell :: Monad m => Shell m a -> m (Text, Text, Either Int a)
runShell (Shell s) = do
  (e, (out, err)) <- runWriterT $ runExceptT s
  return (out, err, e)

-- | Run a shell action, producing an exit code, stdout, and stderr
execShell :: Monad m => Shell m a -> m (ExitCode, Text, Text)
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
  { _streamingProcess_waitForProcess :: !(Shell m ExitCode)
  , _streamingProcess_terminateProcess :: !(Shell m ())
  , _streamingProcess_getProcessExitCode :: !(Shell m (Maybe ExitCode))
  } deriving Generic

-- | A process whose output can be inspected while it is still running.
shellStreamableProcess
  :: (MonadIO m, MonadMask m)
  => (ByteString -> IO ()) -- ^ Handle stdout
  -> (ByteString -> IO ()) -- ^ Handle stderr
  -> CreateProcess
  -> Shell m (StreamingProcess m)
shellStreamableProcess handleStdout handleStderr p = do
  (_, mout, merr, ph) <- liftIO $ createProcess $ p
    { std_out = CreatePipe
    , std_err = CreatePipe
    }
  case (mout, merr) of
    (Just hout, Just herr) -> do
    -- TODO: This code is basically the same as that in Reflex.Process.createProcess, except for the action to take when new output is received
      let
        handleReader h (handler :: ByteString -> IO ()) = do
          hSetBuffering h LineBuffering
          fix $ \go -> do
            open <- hIsOpen h
            when open $ do
              readable <- hIsReadable h
              when readable $ do
                out <- BS.hGetSome h (2^(15 :: Int))
                unless (BS.null out) $ do
                  handler out
                  go

        appendIORef r out = atomicModifyIORef' r (\v -> (v <> BS.byteString out, ()))

      stdoutAcc <- liftIO $ newIORef mempty
      stderrAcc <- liftIO $ newIORef mempty
      outThread <- liftIO $ Async.async $ handleReader hout $ \out ->
        appendIORef stdoutAcc out *> handleStdout out
      errThread <- liftIO $ Async.async $ handleReader herr $ \out ->
        appendIORef stderrAcc out *> handleStderr out
      let finalize f =
            liftIO f
              `finally` liftIO (Async.uninterruptibleCancel outThread)
              `finally` liftIO (Async.uninterruptibleCancel errThread)
              `finally` do
                stdoutFinal <- liftIO $ builderToStrictBS <$> readIORef stdoutAcc
                stderrFinal <- liftIO $ builderToStrictBS <$> readIORef stderrAcc
                tellOutput (T.decodeUtf8 stdoutFinal, T.decodeUtf8 stderrFinal)
      return $ StreamingProcess
        { _streamingProcess_waitForProcess = finalize $ waitForProcess ph
        , _streamingProcess_terminateProcess = finalize $ terminateProcess ph
        , _streamingProcess_getProcessExitCode = finalize $ getProcessExitCode ph
        }
    _ -> error "shellStreamingProcess: Created pipes were not returned"
    where
      builderToStrictBS = LBS.toStrict . BS.toLazyByteString

-- | Like 'shellStreamableProcess' but instead of taking handlers for each
-- stream, it automatically buffers the output of each stream and returns
-- 'IO' actions to read and clear the buffer.
shellStreamableProcessBuffered
  :: (MonadIO m, MonadMask m)
  => CreateProcess
  -> Shell m (StreamingProcess m, IO ByteString, IO ByteString) -- ^ ('StreamProcess', stdout, stderr)
shellStreamableProcessBuffered p = do
  stdoutBuf <- liftIO $ newIORef mempty
  stderrBuf <- liftIO $ newIORef mempty
  sp <- shellStreamableProcess (updateBuf stdoutBuf) (updateBuf stderrBuf) p
  pure (sp, eatBuf stdoutBuf, eatBuf stderrBuf)
  where
    updateBuf buf new = atomicModifyIORef' buf $ \old -> (old <> BS.byteString new, ())
    eatBuf buf = atomicModifyIORef' buf $ \out -> (mempty, LBS.toStrict $ BS.toLazyByteString out)


-- | Run a shell process using the given runner function
shellCreateProcess'
  :: MonadIO m
  => (CreateProcess -> IO (ExitCode, Text, Text))
  -> CreateProcess
  -> Shell m ()
shellCreateProcess' f p = do
  (ex, out, err) <- liftIO $ f p
  tellOutput (out, err)
  case ex of
    ExitFailure c -> do
      liftIO $ T.putStrLn $ mconcat
        [ "Command failed: "
        , T.pack $ showCommand p
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
      readAndDecodeCreateProcess $ cmd { env = envWithOverrides }

runCreateProcessWithEnv :: Map String String -> CreateProcess -> IO (ExitCode, Text, Text)
runCreateProcessWithEnv menv p = execShell $ shellCreateProcessWithEnv menv p

runCreateProcess :: CreateProcess -> IO (ExitCode, Text, Text)
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
    waitReadHandle :: Handle -> IO Text
    waitReadHandle h = do
      c <- fmap T.decodeUtf8 $ BS.hGetContents h
      withForkWait (evaluate $ rnf c) $ \wait -> wait >> hClose h
      return c

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
  RawCommand exe args -> mconcat $ L.intersperse " " $ exe : args
