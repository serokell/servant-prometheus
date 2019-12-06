{-# LANGUAGE OverloadedStrings #-}

-- | Measure request latency.
module Servant.Prometheus.Internal.Latency
    ( Bucket
    , defaultBuckets

    , LatencyHistogram
    , latencyHistogram
    ) where

import           Prometheus (Histogram, Info (Info), Metric, histogram)


-- | Same as in prometheus-client (it is not exported from there).
type Bucket = Double

-- | Default histogram buckets.
--
-- Should be reasonably good for a service with response time in 10s of ms,
-- but you'll probaby want to customise them anyway.
defaultBuckets :: [Bucket]
defaultBuckets = map (/ 1000)
 -- These are in ms
  $ [5]
 ++ [10, 20 .. 90]
 ++ [100, 125 .. 275]
 ++ [300, 400 .. 900]
 ++ [1000, 2000 .. 5000]

-- | Type alias for clarity.
--
-- The histogram collects latency measurements into predefined buckets.
-- This is slightly less precise than computing quantiles directly, but
-- allows for more flexibility in processing and moves the work from the
-- application to the metrics display server.
type LatencyHistogram = Histogram

-- | Construct a latency histogram.
latencyHistogram :: [Bucket] -> Metric LatencyHistogram
latencyHistogram = histogram info
  where
    info = Info "http_request_duration_seconds" "Request latency in seconds"
