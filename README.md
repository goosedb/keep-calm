# Keep Calm
`keep-calm` is a simple library to throttle _some io_.

### How to use

```haskell
import KeepCalm

callSomeHttpService :: String -> IO ()
callSomeHttpService = ...

main :: IO ()
main = do
  throttler <- newThrottler
    ThrottlerConfig
      { interval = 1000000000 -- in nanoseconds (1 second)
      , allowedNumberOfRequests = 5 -- so we should call _action_ 
                                    -- not more often than 5 times per second
      , onEvent = \_ -> pure () -- now we don't care
      }
  
  let requests = ["Lorem", "ipsum", "dolor", "sit", "amet"]
  

  -- You also can use throttler concurrently
  for_ requests \req -> do
    throttle throttler (callSomeHttpService req) 

```
### It's important to understand
`keep-calm` doesn't guarantee that _your action_ will be ran the maximum possible number of times, but it guarantees that action won't be ran more times than allowed