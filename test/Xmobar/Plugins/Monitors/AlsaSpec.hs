{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}

module Xmobar.Plugins.Monitors.AlsaSpec
  ( main
  , spec
  ) where

#ifdef ALSA
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.FilePath
import System.IO
import System.IO.Temp
import System.Posix.Files
import System.Process
import Test.Hspec

import Xmobar.Plugins.Monitors.Alsa

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Alsa.getWaitMonitor" $
    it "produces the expected timeline (using a fake alsactl)"
       runFakeAlsactlTest

  describe "Alsa.parseOptsIncludingMonitorArgs" $ do
    it "works with empty args" $ do
      opts <- parseOptsIncludingMonitorArgs []
      aoAlsaCtlPath opts `shouldBe` Nothing

    it "parses --alsactl=foo" $ do
      opts <- parseOptsIncludingMonitorArgs ["--", "--alsactl=foo"]
      aoAlsaCtlPath opts `shouldBe` Just "foo"


runFakeAlsactlTest :: Expectation
runFakeAlsactlTest =
      withSystemTempDirectory "xmobar-test" $ \tmpDir -> do

        let fifoPath = tmpDir </> "fifo"
            fakeAlsactlPath = tmpDir </> "fake-alsactl"

        writeFile fakeAlsactlPath $
          unlines
            [ "#!/bin/bash"
            , "[[ $1 == monitor ]] || exit 99"
            , "exec cat \"$2\""
            ]

        setFileMode fakeAlsactlPath ownerModes

        withFifoWriteHandle fifoPath $ \fifo -> do

            timeline <- newMVar [] :: IO (MVar [TimelineEntry])
            runVolumeCompleted <- newEmptyMVar :: IO (MVar Bool) -- True -> quit
            waiterTaskIsRunning <- newEmptyMVar :: IO (MVar ())
            waiterTaskIsWaiting <- newEmptyMVar :: IO (MVar ())

            let outputCallback msg = fail ("Did not expect the output callback to be invoked (message: "++show msg++")")

            withMonitorWaiter fifoPath (Just fakeAlsactlPath) outputCallback $ \waitFunc -> do

              let addToTimeline e =  modifyMVar_ timeline (pure . (e :))

                  emitEvent = do
                    addToTimeline EventEmitted
                    hPutStrLn fifo "#17 (2,0,0,Master Playback Volume,0) VALUE"
                    hFlush fifo

                  putNow mv val = do
                    ok <- tryPutMVar mv val
                    unless ok $ expectationFailure "Expected the MVar to be empty"

                  simulateRunVolumeCompleted = putNow runVolumeCompleted False

                  quitWaiter = putNow runVolumeCompleted True

                  waiterTaskMain = do
                    addToTimeline RunVolume
                    putNow waiterTaskIsRunning ()
                    q <- takeMVar runVolumeCompleted
                    unless q $ do
                      addToTimeline Waiting
                      putNow waiterTaskIsWaiting ()
                      waitFunc

                      waiterTaskMain

                  delay_ms = threadDelay . (* 1000)

              withAsync waiterTaskMain $ \waiterTask -> do

                takeMVar waiterTaskIsRunning
                simulateRunVolumeCompleted
                takeMVar waiterTaskIsWaiting
                takeMVar waiterTaskIsRunning -- Waiter returns instantly once
                simulateRunVolumeCompleted
                takeMVar waiterTaskIsWaiting

                emitEvent
                takeMVar waiterTaskIsRunning
                simulateRunVolumeCompleted
                takeMVar waiterTaskIsWaiting

                let iters = 3
                    burstSize = 5

                replicateM_ iters $ do
                  emitEvent
                  takeMVar waiterTaskIsRunning
                  -- Now more events start to accumulate during runVolume
                  replicateM_ burstSize emitEvent
                  delay_ms 250 -- Give the events time to go through the pipe
                  simulateRunVolumeCompleted
                  -- runVolume completed and should run once more due to
                  -- accumulated events
                  takeMVar waiterTaskIsWaiting
                  takeMVar waiterTaskIsRunning
                  simulateRunVolumeCompleted
                  takeMVar waiterTaskIsWaiting

                emitEvent
                takeMVar waiterTaskIsRunning
                quitWaiter

                wait waiterTask

                timelineValue <- reverse <$> readMVar timeline

                timelineValue `shouldBe`
                  [RunVolume, Waiting, RunVolume, Waiting, EventEmitted, RunVolume, Waiting]
                  ++ concat
                      (replicate iters $
                        [EventEmitted, RunVolume]
                        ++ replicate burstSize EventEmitted
                        ++ [Waiting, RunVolume, Waiting])
                  ++ [EventEmitted, RunVolume]

data TimelineEntry = EventEmitted | Waiting | RunVolume
              deriving(Eq)

instance Show TimelineEntry where
  show x =
    case x of
      EventEmitted -> "E"
      Waiting -> "W"
      RunVolume -> "R"


withFifoWriteHandle :: FilePath -> (Handle -> IO b) -> IO b
withFifoWriteHandle fifoPath body = do
    createNamedPipe fifoPath ownerModes
    -- Can't seem to get the writing to the FIFO to work internally
    withCreateProcess
      (proc "bash" ["-c", "cat >> \"$0\"", fifoPath]) {std_in = CreatePipe}
      $ \(Just h) _ _ _ -> do
        hSetBuffering h LineBuffering
        body h
#else
-- These No-Op values are required for HSpec's test discovery.
main :: IO ()
main = return ()

spec :: Monad m => m ()
spec = return ()
#endif
