module Coquina.Internal where

import Control.Concurrent (MVar, forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (rnf)
import Control.Exception (SomeException, evaluate, mask, onException, throwIO, try)
import qualified Control.Exception as C
import Control.Monad
import Data.ByteString (hGetContents)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import Foreign.C.Error (Errno(..), ePIPE)
import GHC.IO.Exception (IOErrorType(..), IOException(..))
import System.Exit
import System.IO (hClose)
import System.Process

-- | Like readCreateProcess
readAndDecodeCreateProcess :: CreateProcess -> Text -> IO (ExitCode, Text, Text)
readAndDecodeCreateProcess cp input =
  withCreateProcess (cp { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }) $ \mstdin mouth merrh ph -> case (mstdin, mouth, merrh) of
    (Just inh, Just outh, Just errh) -> do
      out <- fmap decodeUtf8 $ hGetContents outh
      err <- fmap decodeUtf8 $ hGetContents errh
      withForkWait (evaluate $ rnf out) $ \waitOut ->
        withForkWait (evaluate $ rnf err) $ \waitErr -> do
          -- Write stdin
          unless (T.null input) $
            ignoreSigPipe $ T.hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh
          waitOut
          waitErr
          hClose outh
          hClose errh
      exitCode <- waitForProcess ph
      return (exitCode, out, err)
    (Nothing, _, _) -> error "readAndDecodeCreateProcess: Failed to get std_in handle"
    (_, Nothing, _) -> error "readAndDecodeCreateProcess: Failed to get std_out handle"
    (_, _, Nothing) -> error "readAndDecodeCreateProcess: Failed to get std_err handle"

-- * The code below is taken from System.Process which unfortunately does not export these functions

-- | From System.Process
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid

-- | From System.Process
ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \e -> case e of
  IOError { ioe_type  = ResourceVanished, ioe_errno = Just ioe } | Errno ioe == ePIPE -> return ()
  _ -> throwIO e
