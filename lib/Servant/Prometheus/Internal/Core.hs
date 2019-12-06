{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Servant.Prometheus.Internal.Core
    ( ServantVector

    , Meters (..)
    , meters
    ) where


import           Control.Monad                           (forM_)
import           Data.Proxy                              (Proxy)
import           Data.Text                               (Text)
import           Prometheus                              (Metric (Metric),
                                                          Vector, vector,
                                                          withLabel)

import           Servant.Prometheus.Internal.Endpoints   (Endpoint, HasEndpoints (getEndpoints),
                                                          endpointToLabels)
import           Servant.Prometheus.Internal.Inflight    (InflightGauge,
                                                          inflightGauge)
import           Servant.Prometheus.Internal.Latency     (LatencyHistogram,
                                                          defaultBuckets,
                                                          latencyHistogram)
import           Servant.Prometheus.Internal.StatusCodes (StatusVector,
                                                          statusVector)

-- | Vector indexed by two labels: path, method.
type ServantVector m = Vector (Text, Text) m

-- | Make a new vector and initialise it with zeroes.
servantVector :: [Endpoint] -> Metric m -> Metric (ServantVector m)
servantVector es m = Metric $ do
    let Metric constrVec = vector ("path", "method") m
    (vec, collectVec) <- constrVec
    -- Initialise all of them with zeroes
    forM_ es $ \endpoint ->
        withLabel vec (endpointToLabels endpoint) (const $ pure ())
    pure (vec, collectVec)


-- | Collection of meters that we maintain.
data Meters api = Meters
    { mInflight :: ServantVector InflightGauge
    , mStatusCodes :: ServantVector StatusVector
    , mLatencyH :: ServantVector LatencyHistogram
    }

meters :: HasEndpoints api => Proxy api -> Metric (Meters api)
meters proxy = Metric $ do
    let es = getEndpoints proxy

    let Metric constrInflight = servantVector es inflightGauge
    (mInflight, collectInflight) <- constrInflight
    let Metric constrStatusCodes = servantVector es statusVector
    (mStatusCodes, collectStatusCodes) <- constrStatusCodes
    let Metric constrLatencyH = servantVector es $ latencyHistogram defaultBuckets
    (mLatencyH, collectLatencyH) <- constrLatencyH

    let collectAll = sequence [collectInflight, collectStatusCodes, collectLatencyH]
    pure (Meters{..}, concat <$> collectAll)
