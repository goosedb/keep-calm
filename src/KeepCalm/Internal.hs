{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KeepCalm.Internal where

import Control.Concurrent (MVar, modifyMVar_, newMVar)
import Control.Monad (when)
import Data.Foldable (for_)
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (threadDelay)

data Throttler = Throttler
  { config :: !ThrottlerConfig
  , runTimestamps :: MVar (Seq.Seq Word64)
  }

data ThrottlerConfig = ThrottlerConfig
  { interval :: !Word64
  -- ^ The time interval in nanoseconds
  , allowedNumberOfRequests :: !Int
  , onEvent :: ThrottleEvent -> IO ()
  -- ^ You can log throttler's actions to debug
  }

data ThrottleEvent
  = -- | Fired when you beat rate limit
    OnThrottle
  | -- | Fired when thread is trying to lock throttler
    TryingToTakeThrottler
  | -- | Fired when thread locked throttler
    ThrottlerHasBeenTook

newThrottler :: ThrottlerConfig -> IO Throttler
newThrottler config = do
  runTimestamps <- newMVar mempty
  pure Throttler{..}

throttle :: Throttler -> IO a -> IO a
throttle Throttler{..} action = do
  onEvent TryingToTakeThrottler
  modifyMVar_ runTimestamps \rt -> do
    onEvent ThrottlerHasBeenTook
    now <- getMonotonicTimeNSec
    let filteredTimestamps = Seq.dropWhileL ((> interval) . (now -)) rt
    case filteredTimestamps of
      Seq.Empty -> pure ()
      (oldest Seq.:<| _) -> do
        let calledTimesInInterval = Seq.length filteredTimestamps
        when (calledTimesInInterval >= allowedNumberOfRequests) do
          onEvent OnThrottle
          throttledAt <- getMonotonicTimeNSec
          let sleep = interval - min interval (throttledAt - oldest)
          threadDelay (nanoToMicro sleep)
    calledAt <- getMonotonicTimeNSec
    pure (filteredTimestamps Seq.:|> calledAt)
  action
 where
  ThrottlerConfig{..} = config
  nanoToMicro = fromIntegral . (`div` 1000)
