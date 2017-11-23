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

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified Data.HashMap.Strict     as H
import           Data.Monoid
import           Data.Proxy
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Time.Clock
import           GHC.TypeLits
import           Network.HTTP.Types      (Method, Status (..))
import           Network.Wai
import           Servant.API


import           Prometheus              as Prom


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

responseTimeDistribution :: Metric Histogram -> Metric Summary -> Middleware
responseTimeDistribution hist qant application request respond =
    bracket getCurrentTime stop $ const $ application request respond
  where
    stop t1 = do
        t2 <- getCurrentTime
        let dt = diffUTCTime t2 t1
            t = fromRational $ (*1000) $ toRational dt
        observe t hist
        observe t qant

data Meters = Meters
    { metersInflight  :: Metric Gauge
    , metersResponses :: Metric (Vector Label1 Counter)
    , metersTime      :: Metric Histogram
    , metersTimeQant  :: Metric Summary
    }

monitorEndpoints
    :: HasEndpoint api
    => Proxy api
    -> MVar (H.HashMap Text Meters)
    -> Middleware
monitorEndpoints proxy meters application = \request respond -> do
    let path = case getEndpoint proxy request of
            Nothing -> "unknown"
            Just (ps,method) -> T.intercalate "." $ ps <> [T.decodeUtf8 method]
    Meters{..} <- modifyMVar meters $ \ms -> case H.lookup path ms of
        Nothing -> do
            let prefix = "servant.path." <> path <> "."
                info :: Text -> Text -> Text -> Info
                info prfx name help = Info (T.unpack $ prfx <> name) (T.unpack $ help <> prfx)
            metersInflight <- registerIO . gauge $ info prefix  "in_flight" "Number of in flight requests for "
            metersResponses <- registerIO . vector "status_code" $ counter (info prefix "http_status" "Counters for status codes")
            withLabel "2XX" (unsafeAddCounter 0) metersResponses
            withLabel "4XX" (unsafeAddCounter 0) metersResponses
            withLabel "5XX" (unsafeAddCounter 0) metersResponses
            withLabel "XXX" (unsafeAddCounter 0) metersResponses
            metersTime     <- registerIO . histogram (info prefix "time_ms" "Distribution of query times for ")
                                            $ [1,5,10,50,100,150,200,300,500,1000,1500,2500,5000,7000,10000,50000]
            metersTimeQant <- registerIO . summary (info prefix "time_ms" "Summary of query times for ") $ defaultQuantiles
            let m = Meters{..}
            return (H.insert path m ms, m)
        Just m -> return (ms,m)
    let application' =
            responseTimeDistribution metersTime metersTimeQant .
            countResponseCodes metersResponses .
            gaugeInflight metersInflight $
            application
    application' request respond

class HasEndpoint a where
    getEndpoint :: Proxy a -> Request -> Maybe ([Text], Method)

instance (HasEndpoint (a :: *), HasEndpoint (b :: *)) => HasEndpoint (a :<|> b) where
    getEndpoint _ req =
        getEndpoint (Proxy :: Proxy a) req `mplus`
        getEndpoint (Proxy :: Proxy b) req

instance (KnownSymbol (path :: Symbol), HasEndpoint (sub :: *))
    => HasEndpoint (path :> sub) where
    getEndpoint _ req =
        case pathInfo req of
            p:ps | p == T.pack (symbolVal (Proxy :: Proxy path)) -> do
                (end, method) <- getEndpoint (Proxy :: Proxy sub) req{ pathInfo = ps }
                return (p:end, method)
            _ -> Nothing

instance (KnownSymbol (capture :: Symbol), HasEndpoint (sub :: *))
    => HasEndpoint (Capture capture a :> sub) where
    getEndpoint _ req =
        case pathInfo req of
            _:ps -> do
                (end, method) <- getEndpoint (Proxy :: Proxy sub) req{ pathInfo = ps }
                let p = T.pack $ (':':) $ symbolVal (Proxy :: Proxy capture)
                return (p:end, method)
            _ -> Nothing

instance HasEndpoint (sub :: *) => HasEndpoint (Header h a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryParam (h :: Symbol) a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryParams (h :: Symbol) a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryFlag h :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (ReqBody cts a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (RemoteHost :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (IsSecure :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (HttpVersion :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (Vault :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (WithNamedContext x y sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance ReflectMethod method => HasEndpoint (Verb method status cts a) where
    getEndpoint _ req = case pathInfo req of
        [] | requestMethod req == method -> Just ([], method)
        _  -> Nothing
      where method = reflectMethod (Proxy :: Proxy method)

instance HasEndpoint Raw where
    getEndpoint _ _ = Just ([],"RAW")

instance HasEndpoint EmptyAPI where
    getEndpoint _ _ = Just ([],"EmptyAPI")

#if MIN_VERSION_servant(0,8,1)
instance HasEndpoint (sub :: *) => HasEndpoint (CaptureAll (h :: Symbol) a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
#endif
