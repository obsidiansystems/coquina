module Coquina.Internal where

import Control.Concurrent (MVar, newEmptyMVar, forkIO, putMVar, takeMVar, killThread)
import Control.DeepSeq (rnf)
import Control.Exception (SomeException, evaluate, mask, try, throwIO, onException)
import Data.ByteString (hGetContents)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.Process
import System.Exit
import System.IO (hClose)

-- The code below is taken from System.Process which unfortunately does not export this function
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid

-- Like readCreateProcess, but ignores stdin and decodes bytes, assuming utf-8
readAndDecodeCreateProcess :: CreateProcess -> IO (ExitCode, Text, Text)
readAndDecodeCreateProcess cp =
  withCreateProcess (cp { std_out = CreatePipe, std_err = CreatePipe }) $ \_ mouth merrh ph -> case (mouth, merrh) of
    (Just outh, Just errh) -> do
      out <- fmap decodeUtf8 $ hGetContents outh
      err <- fmap decodeUtf8 $ hGetContents errh
      withForkWait (evaluate $ rnf out) $ \waitOut ->
        withForkWait (evaluate $ rnf err) $ \waitErr -> do
          waitOut
          waitErr
          hClose outh
          hClose errh
      exitCode <- waitForProcess ph
      return (exitCode, out, err)
    (Nothing, _) -> error "readAndDecodeCreateProcess: Failed to get std_out handle"
    (Just _, Nothing) -> error "readAndDecodeCreateProcess: Failed to get std_err handle"
