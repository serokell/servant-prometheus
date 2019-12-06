{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Count response status codes.
module Servant.Prometheus.Internal.StatusCodes
    ( StatusBucket (..)
    , statusToBucket

    , StatusVector
    , statusVector
    , updateStatusVector
    ) where

import           Control.Monad      (forM_, void)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Network.HTTP.Types (Status (Status, statusCode))
import           Prometheus         (Counter, Info (Info), Metric (Metric),
                                     MonadMonitor, Vector, counter, incCounter,
                                     vector, withLabel)


-- | Status codes grouped into buckets.
data StatusBucket = Status2XX | Status4XX | Status5XX | StatusXXX
  deriving (Bounded, Enum, Eq, Ord)

instance Show StatusBucket where
  show Status2XX = "2XX"
  show Status4XX = "4XX"
  show Status5XX = "5XX"
  show StatusXXX = "XXX"

-- | Assign a response status to a bucket.
statusToBucket :: Status -> StatusBucket
statusToBucket Status{ statusCode = sc }
    | 200 <= sc && sc < 300 = Status2XX
    | 400 <= sc && sc < 500 = Status4XX
    | 500 <= sc && sc < 600 = Status5XX
    | otherwise             = StatusXXX


-- | Vector of counters indexed by status code bucket.
type StatusVector = Vector Text Counter

-- | Construct a single vector that counts statuses.
statusVector :: Metric StatusVector
statusVector = Metric $ do
    let Metric constructVec = vector "status" $ counter info
    (vec, collectVec) <- constructVec
    -- Initialise all of them with zeroes
    forM_ [minBound .. maxBound] $ \(status :: StatusBucket) ->
        withLabel vec (T.pack . show $ status) (const $ pure ())
    pure (vec, collectVec)
  where
    info = Info "http_responses_total" "Total number of http responses by their statuses."

-- | Record a new observed status in the vector.
updateStatusVector :: MonadMonitor m => Status -> StatusVector -> m ()
updateStatusVector status vec
  = withLabel vec (T.pack . show . statusToBucket $ status) (void . incCounter)
