{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeOperators #-}

-- | Utilities for adding a @/metrics@ endpoint to your application.
module Servant.Prometheus.Export
  ( MetricsApi
  , serveMetrics

  , WithMetrics
  , withMetrics
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy)
import Servant.API ((:<|>) ((:<|>)), (:>), Get)
import Servant.API.ContentTypes (MimeRender (mimeRender), PlainText)
import Servant.Server (ServerT)

import qualified Prometheus as P


-- | A simple wrapper around already rendered metrics.
newtype PrometheusMetrics
  = PrometheusMetrics { getPrometheusMetrics :: ByteString }

instance MimeRender PlainText PrometheusMetrics where
   mimeRender _ = getPrometheusMetrics


-- | Servant API containing just the Prometheus @/metrics@ endpoint.
type MetricsApi = "metrics" :> Get '[PlainText] PrometheusMetrics

-- | Server/handler for the 'MetricsApi' type.
--
-- The type is equivalent to:
--
-- > serveMetrics :: MonadIO m => m PrometheusMetrics
serveMetrics :: MonadIO m => ServerT MetricsApi m
serveMetrics = PrometheusMetrics <$> liftIO P.exportMetricsAsText


-- | A wrapper around your API type that adds the metrics endpoint.
type WithMetrics api = api :<|> MetricsApi

-- | A wrapper around your server that handles the metrics endpiont.
withMetrics
  :: MonadIO m
  => Proxy api
  -> ServerT api m -> ServerT (WithMetrics api) m
withMetrics _ server = server :<|> serveMetrics
