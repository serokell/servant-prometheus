{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Servant middleware for observing requests.
module Servant.Prometheus.Internal.Middleware
    ( observeRequests
    ) where

import           Control.Exception                       (bracket)
import           Data.Proxy                              (Proxy (Proxy))
import           Data.Text                               (Text)
import           Data.Time.Clock.System                  (SystemTime (systemNanoseconds, systemSeconds),
                                                          getSystemTime)
import           Network.Wai                             (Middleware, Response,
                                                          ResponseReceived,
                                                          responseStatus)
import           Prometheus                              (decGauge, incGauge,
                                                          observe, withLabel)

import           Servant.Prometheus.Internal.Core        (Meters (..))
import           Servant.Prometheus.Internal.Endpoints   (Endpoint, HasEndpoints (getEndpoint),
                                                          endpointToLabels)
import           Servant.Prometheus.Internal.StatusCodes (updateStatusVector)


-- | Middleware that observes servant requests.
observeRequests :: forall api. HasEndpoints api => Meters api -> Middleware
observeRequests Meters{..} app req respond
  = bracket pre post (const $ app req respond')
  where
    endpoint :: Maybe (Endpoint)
    endpoint = getEndpoint (Proxy :: Proxy api) req

    labels :: (Text, Text)
    labels = maybe ("", "") endpointToLabels endpoint

    pre :: IO SystemTime
    pre = do
      -- 1. Increment the number of inflight requests
      withLabel mInflight labels incGauge
      -- 2. Start measuring time
      getSystemTime

    post :: SystemTime -> IO ()
    post t0 = do
      -- 1. Stop measuring time
      t1 <- getSystemTime
      -- 2. Decrement the number of inflight requests
      withLabel mInflight labels decGauge
      -- 3. Record the observed latency
      let latency = t1 `diffSystemTimeS` t0
      withLabel mLatencyH labels (flip observe latency)

    -- Our tweaked response continuation that counts status codes.
    respond' :: Response -> IO ResponseReceived
    respond' resp
      = respond resp <*
        withLabel mStatusCodes labels (updateStatusVector $ responseStatus resp)


-- | Compute the difference of two 'SystemTime' in seconds.
diffSystemTimeS :: SystemTime -> SystemTime -> Double
diffSystemTimeS t2 t1
  = fromIntegral diffSeconds + (fromIntegral diffNanoseconds) / 1000000000
  where
    diffSeconds = systemSeconds t2 - systemSeconds t1
    diffNanoseconds = systemNanoseconds t2 - systemNanoseconds t1
