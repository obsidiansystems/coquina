{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.STM
import Control.Lens (_1, _2, (%~))
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import System.IO (hFlush)
import System.IO.Temp (withSystemTempFile)
import System.Process (proc)
import System.Timeout (timeout)
import System.Which (staticWhich)
import Test.Hspec (describe, hspec, it, shouldBe)

import Coquina (StreamingProcess (..), runShell, shellStreamableProcess)

main :: IO ()
main = hspec $
  describe "shellStreamableProcess" $
    it "reads stdout incrementally" $ do
      (allStdout, allStderr, result :: Either Int ([Text], [Text])) <- withSystemTempFile @IO "temp" $ \path h -> runShell $ do
        stdoutListeners <- liftIO $ newTVarIO (0 :: Int)
        stderrListeners <- liftIO $ newTVarIO (0 :: Int)

        procOuts <- liftIO $ newIORef ([], [])
        let
          waitForStdout = hFlush h *> waitForListeners stdoutListeners 1

          recordOutput listener setter x = do
            atomicModifyIORef' procOuts $ \vs ->
              (vs & setter %~ (T.decodeUtf8 x :), ())
            proceed listener

        sp <- shellStreamableProcess
          (recordOutput stdoutListeners _1)
          (recordOutput stderrListeners _2)
          (proc $(staticWhich "tail") ["-f", path])

        (maybe (error "Test timed out") pure =<<) $ liftIO $ timeout (5*10^(6::Int)) $ do
          T.hPutStrLn h "line1" *> waitForStdout
          T.hPutStrLn h "line2" *> waitForStdout
          T.hPutStrLn h "line3" *> waitForStdout

        _streamingProcess_terminateProcess sp

        liftIO $ readIORef procOuts

      allStdout `shouldBe` "line1\nline2\nline3\n"
      allStderr `shouldBe` ""
      result `shouldBe` Right (["line3\n", "line2\n", "line1\n"], [])
  where
    -- Help synchronize test with threads
    proceed var = atomically $ modifyTVar var (+1)

    waitForListeners var n = atomically $ do
      newStep <- readTVar var
      if newStep < n
        then retry
        else writeTVar var 0
