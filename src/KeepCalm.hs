module KeepCalm (
  I.ThrottleEvent (..),
  I.Throttler,
  I.ThrottlerConfig (..),
  I.newThrottler,
  I.throttle,
) where

import KeepCalm.Internal as I (
  ThrottleEvent (..),
  Throttler,
  ThrottlerConfig (..),
  newThrottler,
  throttle,
 )
