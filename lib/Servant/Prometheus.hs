module Servant.Prometheus
    ( Meters
    , meters

    , monitorServant
    , servePrometheusMetrics
    ) where

import           Network.HTTP.Types                     (status200)
import           Network.Wai                            (Application,
                                                         Middleware,
                                                         responseLBS)
import           Prometheus                             (exportMetricsAsText)

import           Servant.Prometheus.Internal.Core       (Meters, meters)
import           Servant.Prometheus.Internal.Endpoints  (HasEndpoints)
import           Servant.Prometheus.Internal.Middleware (observeRequests)

monitorServant :: HasEndpoints api => Meters api -> Middleware
monitorServant = observeRequests

-- | An application which will always return prometheus metrics with status 200.
-- This can be added to a Servant API using the RAW type, or may be run in a
-- second webserver on a different port to keep metrics reporting separate from
-- your application.
servePrometheusMetrics :: Application
servePrometheusMetrics = \_req respond ->
    respond . responseLBS status200 [] =<< exportMetricsAsText
