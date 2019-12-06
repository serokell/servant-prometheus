{-# LANGUAGE OverloadedStrings #-}

-- | Count inflight requests.
module Servant.Prometheus.Internal.Inflight
    ( InflightGauge
    , inflightGauge
    ) where

import           Prometheus (Gauge, Info (Info), Metric, gauge)


-- | Type alias for clarity.
type InflightGauge = Gauge

-- | Construct a single vector that counts statuses.
inflightGauge :: Metric InflightGauge
inflightGauge = gauge info
  where
    info = Info "http_requests_in_flight" "Total http requests in flight."
