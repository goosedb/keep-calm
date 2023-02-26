{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (forkIO, threadDelay)
import KeepCalm (
  ThrottleEvent (OnThrottle),
  ThrottlerConfig (..),
  newThrottler,
  throttle,
 )
import Test.Hspec (describe, hspec, it, shouldBe, shouldSatisfy)

secondsToNanos :: Rational -> Word64
secondsToNanos = floor . (* 1000000000)

secondsToMicros :: Rational -> Int
secondsToMicros = floor . (* 1000000)

main :: IO ()
main = hspec do
  describe "Single thread" do
    let createThrottler i r e = do
          newThrottler
            ThrottlerConfig
              { interval = i
              , allowedNumberOfRequests = fromIntegral r
              , onEvent = e
              }
    it "Shouldn't allow to call action too fast" do
      let interval = secondsToNanos 0.5
          allowedNumber = 3
          actions = 10
      duration <- liftIO do
        throttler <- createThrottler interval allowedNumber (\_ -> pure ())
        start <- getMonotonicTimeNSec
        for_ [1 .. actions] \_ -> throttle throttler (pure ())
        finish <- getMonotonicTimeNSec
        pure (finish - start)

      duration `shouldSatisfy` (> ceiling (fromIntegral $ actions `div` allowedNumber))

    it "Shouldn't throttle if rate limit isn't beaten" do
      let interval = secondsToNanos 0.5
          allowedNumber = 3
          actions = 10
      hasBeenThrottledRef <- liftIO $ newIORef False
      duration <- liftIO do
        throttler <- createThrottler
          interval
          allowedNumber
          \case
            OnThrottle -> writeIORef hasBeenThrottledRef True
            _ -> pure ()
        start <- getMonotonicTimeNSec
        for_ [1 .. actions] \_ -> throttle throttler (threadDelay (secondsToMicros 0.3))
        finish <- getMonotonicTimeNSec
        pure (finish - start)

      hasBeenThrottled <- liftIO $ readIORef hasBeenThrottledRef
      hasBeenThrottled `shouldBe` False

  describe "Multi thread" do
    let createThrottler i r e = do
          newThrottler
            ThrottlerConfig
              { interval = i
              , allowedNumberOfRequests = fromIntegral r
              , onEvent = e
              }
    it "Shouldn't allow to call action too fast" do
      let interval = secondsToNanos 0.5
          allowedNumber = 3
          actions = 4
          threads = 3
      duration <- liftIO do
        throttler <- createThrottler interval allowedNumber (\_ -> pure ())
        start <- getMonotonicTimeNSec
        for_ [1 .. threads] \_ -> forkIO do
          for_ [1 .. actions] \_ -> throttle throttler (pure ())
        finish <- getMonotonicTimeNSec
        pure (finish - start)

      duration `shouldSatisfy` (> ceiling (fromIntegral (actions * threads `div` allowedNumber)))

    it "Shouldn't throttle if rate limit isn't beaten" do
      let interval = secondsToNanos 0.5
          allowedNumber = 3
          actions = 4
          threads = 3
      hasBeenThrottledRef <- liftIO $ newIORef False
      duration <- liftIO do
        throttler <- createThrottler
          interval
          allowedNumber
          \case
            OnThrottle -> writeIORef hasBeenThrottledRef True
            _ -> pure ()
        start <- getMonotonicTimeNSec
        for_ [1 .. threads] \_ -> forkIO do
          for_ [1 .. actions] \_ -> throttle throttler (threadDelay (secondsToMicros 0.6))
        finish <- getMonotonicTimeNSec
        pure (finish - start)

      hasBeenThrottled <- liftIO $ readIORef hasBeenThrottledRef
      hasBeenThrottled `shouldBe` False
