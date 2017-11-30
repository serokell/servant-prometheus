{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.Prometheus where

import           Control.Exception
import           Control.Monad
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.HashMap.Strict  as H
import           Data.Monoid
import           Data.Proxy
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Data.Time.Clock
import           GHC.TypeLits
import           Network.HTTP.Types   (Method, Status (..), status200)
import           Network.Wai
import           Servant.API

import           Prometheus           as Prom


gaugeInflight :: Metric Gauge -> Middleware
gaugeInflight inflight application request respond =
    bracket_ (incGauge inflight)
             (decGauge inflight)
             (application request respond)

-- | Count responses with 2XX, 4XX, 5XX, and XXX response codes.
countResponseCodes
    :: Metric (Vector Label1 Counter)
    -> Middleware
countResponseCodes codes application request respond =
    application request respond'
  where
    respond' res = count (responseStatus res) >> respond res
    count Status{statusCode = sc }
        | 200 <= sc && sc < 300 = withLabel "2XX" incCounter codes
        | 400 <= sc && sc < 500 = withLabel "4XX" incCounter codes
        | 500 <= sc && sc < 600 = withLabel "5XX" incCounter codes
        | otherwise             = withLabel "XXX" incCounter codes

responseTimeDistribution :: MeasureQuantiles -> Metric Histogram -> Metric Summary -> Middleware
responseTimeDistribution qants hist qant application request respond =
    bracket getCurrentTime stop $ const $ application request respond
  where
    stop t1 = do
        t2 <- getCurrentTime
        let dt = diffUTCTime t2 t1
            t = fromRational $ (*1000) $ toRational dt
        observe t hist
        case qants of
            WithQuantiles -> observe t qant
            NoQuantiles   -> pure ()

data Meters = Meters
    { metersInflight     :: Metric Gauge
    , metersResponses    :: Metric (Vector Label1 Counter)
    , metersTime         :: Metric Histogram
    , metersTimeQant     :: Metric Summary
    , metersRecordQuants :: MeasureQuantiles
    }

-- | Measuring quantiles can add significant overgead to your application if your
-- requests are often small. You should benchmark your app with and without
-- quantiles to decide if the overhead is acceptable for you application.
data MeasureQuantiles = WithQuantiles | NoQuantiles deriving (Show, Eq)


makeMeters :: HasEndpoints api => Proxy api -> MeasureQuantiles -> IO (H.HashMap Text Meters)
makeMeters proxy metersRecordQuants = do
    let eps = "unknown" : map (\(ps,method) -> T.intercalate "." $ ps <> [T.decodeUtf8 method])
                              (getEndpoints proxy)
    ms <- forM eps $ \path -> do
        let prefix = "servant.path." <> path <> "."
            info :: Text -> Text -> Text -> Info
            info prfx name help = Info (T.unpack $ prfx <> name) (T.unpack $ help <> prfx)
        metersInflight <- gauge $ info prefix  "in_flight" "Number of in flight requests for "
        metersResponses <- vector "status_code" $ counter (info prefix "http_status" "Counters for status codes")
        metersTime     <- histogram (info prefix "time_ms" "Distribution of query times for ")
                                    [10,50,100,150,200,300,500,1000,1500,2500,5000,7000,10000,50000]
        metersTimeQant <- summary (info prefix "time_ms" "Summary of query times for ") defaultQuantiles
        let m = Meters{..}
        _ <- register metersInflight
        _ <- register metersResponses
        _ <- register metersTime
        _ <- case metersRecordQuants of
            NoQuantiles   -> pure metersTimeQant
            WithQuantiles -> register metersTimeQant
        pure (path,m)
    pure $ H.fromList ms

monitorServant
    :: HasEndpoints api
    => Proxy api
    -> H.HashMap Text Meters
    -> Middleware
monitorServant proxy ms application = \request respond -> do
    let path = case getEndpoint proxy request of
            Nothing -> "unknown"
            Just (ps,method) -> T.intercalate "." $ ps <> [T.decodeUtf8 method]
    let Meters{..} = ms H.! path
        application' =
            responseTimeDistribution metersRecordQuants metersTime metersTimeQant .
            countResponseCodes metersResponses .
            gaugeInflight metersInflight $
            application
    application' request respond

-- | An application which will always return prometheus metrics with status 200.
-- This can be added to a Servant API using the RAW type, or may be run in a
-- second webserver on a different port to keep metrics reporting separate from
-- your application.
servePrometheusMetrics :: Application
servePrometheusMetrics = \_req respond ->
    respond . responseLBS status200 [] . fromStrict =<< exportMetricsAsText



class HasEndpoints a where
    getEndpoints :: Proxy a -> [([Text], Method)]
    getEndpoint :: Proxy a -> Request -> Maybe ([Text], Method)

instance (HasEndpoints (a :: *), HasEndpoints (b :: *)) => HasEndpoints (a :<|> b) where
    getEndpoints _ =
        getEndpoints (Proxy :: Proxy a) ++ getEndpoints (Proxy :: Proxy b)
    getEndpoint _ req =
        getEndpoint (Proxy :: Proxy a) req `mplus`
        getEndpoint (Proxy :: Proxy b) req

instance (KnownSymbol (path :: Symbol), HasEndpoints (sub :: *))
    => HasEndpoints (path :> sub) where
    getEndpoints _ = do
        (end, method) <- getEndpoints (Proxy :: Proxy sub)
        return (T.pack (symbolVal (Proxy :: Proxy path)):end, method)
    getEndpoint _ req =
        case pathInfo req of
            p:ps | p == T.pack (symbolVal (Proxy :: Proxy path)) -> do
                (end, method) <- getEndpoint (Proxy :: Proxy sub) req{ pathInfo = ps }
                return (p:end, method)
            _ -> Nothing

instance (KnownSymbol (capture :: Symbol), HasEndpoints (sub :: *))
    => HasEndpoints (Capture capture a :> sub) where
    getEndpoints _ = do
        (end, method) <- getEndpoints (Proxy :: Proxy sub)
        let p = T.pack $ (':':) $ symbolVal (Proxy :: Proxy capture)
        return (p:end, method)
    getEndpoint _ req =
        case pathInfo req of
            _:ps -> do
                (end, method) <- getEndpoint (Proxy :: Proxy sub) req{ pathInfo = ps }
                let p = T.pack $ (':':) $ symbolVal (Proxy :: Proxy capture)
                return (p:end, method)
            _ -> Nothing

instance HasEndpoints (sub :: *) => HasEndpoints (Header h a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (QueryParam (h :: Symbol) a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (QueryParams (h :: Symbol) a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (QueryFlag h :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (ReqBody cts a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (RemoteHost :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (IsSecure :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (HttpVersion :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (Vault :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (WithNamedContext x y sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance ReflectMethod method => HasEndpoints (Verb method status cts a) where
    getEndpoints _ = [([], method)]
      where method = reflectMethod (Proxy :: Proxy method)
    getEndpoint _ req = case pathInfo req of
        [] | requestMethod req == method -> Just ([], method)
        _  -> Nothing
      where method = reflectMethod (Proxy :: Proxy method)
instance HasEndpoints Raw where
    getEndpoints _ = pure ([],"RAW")
    getEndpoint _ _ = Just ([],"RAW")

instance HasEndpoints EmptyAPI where
    getEndpoints _ = pure ([],"EmptyAPI")
    getEndpoint _ _ = Just ([],"EmptyAPI")

#if MIN_VERSION_servant(0,8,1)
instance HasEndpoints (sub :: *) => HasEndpoints (CaptureAll (h :: Symbol) a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
#endif
