module Coquina.Internal where

import Control.Concurrent (MVar, newEmptyMVar, forkIO, putMVar, takeMVar, killThread)
import Control.Exception (SomeException, mask, try, throwIO, onException)

-- The code below is taken from System.Process which unfortunately does not export this function
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid
