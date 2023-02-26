module KeepCalm 
  ( I.ThrottleEvent(..)
  , I.Throttler
  , I.ThrottlerConfig(..)
  , I.newThrottler
  , I.throttle
  ) where

import KeepCalm.Internal as I
    ( ThrottleEvent(..),
      ThrottlerConfig(..),
      Throttler,
      throttle,
      newThrottler )