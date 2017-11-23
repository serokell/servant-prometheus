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
    :: (Metric Counter, Metric Counter, Metric Counter, Metric Counter)
    -> Middleware
countResponseCodes (c2XX, c4XX, c5XX, cXXX) application request respond =
    application request respond'
  where
    respond' res = count (responseStatus res) >> respond res
    count Status{statusCode = sc }
        | 200 <= sc && sc < 300 = incCounter c2XX
        | 400 <= sc && sc < 500 = incCounter c4XX
        | 500 <= sc && sc < 600 = incCounter c5XX
        | otherwise             = incCounter cXXX

responseTimeDistribution :: Metric Histogram -> Middleware
responseTimeDistribution hist application request respond =
    bracket getCurrentTime stop $ const $ application request respond
  where
    stop t1 = do
        t2 <- getCurrentTime
        let dt = diffUTCTime t2 t1
        observe (fromRational $ (*1000) $ toRational dt) hist

data Meters = Meters
    { metersInflight :: Metric Gauge
    , metersC2XX     :: Metric Counter
    , metersC4XX     :: Metric Counter
    , metersC5XX     :: Metric Counter
    , metersCXXX     :: Metric Counter
    , metersTime     :: Metric Histogram
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
            metersInflight <- gauge $ info prefix  "in_flight" "Number of in flight requests for "
            metersC2XX <- counter $ info prefix  "responses.2XX" "Number of 2XX requests for "
            metersC4XX <- counter $ info prefix  "responses.4XX" "Number of 4XX requests for "
            metersC5XX <- counter $ info prefix  "responses.5XX" "Number of 5XX requests for "
            metersCXXX <- counter $ info prefix  "responses.XXX" "Number of XXX requests for "
            metersTime <- histogram (info prefix "time_ms" "Distribution of query times for ")
                                    [0.01,0.05,0.1,0.25,0.5,0.75,1,1.5,2,3,5,10,20,30,60]
            let m = Meters{..}
            return (H.insert path m ms, m)
        Just m -> return (ms,m)
    let application' =
            responseTimeDistribution metersTime .
            countResponseCodes (metersC2XX, metersC4XX, metersC5XX, metersCXXX) .
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
