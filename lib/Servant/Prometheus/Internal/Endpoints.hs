{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Prometheus.Internal.Endpoints
  ( HasEndpoints (..)

  , Endpoint
  , endpointToLabels
  ) where

import           Servant.API        as Servant
import           Servant.Auth       (Auth)

import           Control.Monad      (mplus)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           GHC.TypeLits       (KnownSymbol, Symbol, symbolVal)
import           Network.HTTP.Types (Method, StdMethod)
import           Network.Wai        (Request (pathInfo), requestMethod)

import           Data.Proxy         (Proxy (Proxy))


type Endpoint = ([Text], Method)

-- | Convert an 'Endpoint' to (path, method).
endpointToLabels :: Endpoint -> (Text, Text)
endpointToLabels (path, method)
  = (T.intercalate "/" path, T.decodeUtf8 method)


class HasEndpoints a where
    getEndpoints :: Proxy a -> [Endpoint]
    getEndpoint :: Proxy a -> Request -> Maybe Endpoint

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


#if MIN_VERSION_servant(0,13,0)
#define CAPTURE Capture' mods
#define HEADER Header' mods
#define QUERY_PARAM QueryParam' mods
#define REQ_BODY ReqBody' mods
#else
#define CAPTURE Capture
#define HEADER Header
#define QUERY_PARAM QueryParam
#define REQ_BODY ReqBody
#endif

instance (KnownSymbol (capture :: Symbol), HasEndpoints (sub :: *))
    => HasEndpoints (CAPTURE capture a :> sub) where
    getEndpoints _ = do
        (end, method) <- getEndpoints (Proxy :: Proxy sub)
        let p = T.pack . (':':) $ symbolVal (Proxy :: Proxy capture)
        return (p:end, method)
    getEndpoint _ req =
        case pathInfo req of
            _:ps -> do
                (end, method) <- getEndpoint (Proxy :: Proxy sub) req{ pathInfo = ps }
                let p = T.pack . (':':) $ symbolVal (Proxy :: Proxy capture)
                return (p:end, method)
            _ -> Nothing

instance HasEndpoints (sub :: *) => HasEndpoints (Auth l a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (AuthProtect t :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (BasicAuth r a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (HEADER h a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (QUERY_PARAM (h :: Symbol) a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (QueryParams (h :: Symbol) a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (QueryFlag h :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (REQ_BODY cts a :> sub) where
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

instance ReflectMethod method => HasEndpoints (Verb (method :: StdMethod) status cts a) where
    getEndpoints _ = [([], method)]
      where method = reflectMethod (Proxy :: Proxy method)
    getEndpoint _ req = case pathInfo req of
        [] | requestMethod req == method -> Just ([], method)
        _  -> Nothing
      where method = reflectMethod (Proxy :: Proxy method)

instance HasEndpoints Raw where
    getEndpoints _ = []
    getEndpoint _ _ = Nothing

instance HasEndpoints EmptyAPI where
    getEndpoints _ = []
    getEndpoint _ _ = Nothing

#if MIN_VERSION_servant(0,8,1)
instance HasEndpoints (sub :: *) => HasEndpoints (CaptureAll (h :: Symbol) a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
#endif

#if MIN_VERSION_servant(0,13,0)
instance HasEndpoints (sub :: *) => HasEndpoints (Servant.Description s :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (Servant.Summary s :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
#endif

#if MIN_VERSION_servant(0,15,0)
instance HasEndpoints (sub :: *) => HasEndpoints (Servant.StreamBody' m f c a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance ReflectMethod method => HasEndpoints (Servant.Stream (method :: StdMethod) s f c a) where
    getEndpoints _ = [([], method)]
      where method = reflectMethod (Proxy :: Proxy method)
    getEndpoint _ req = case pathInfo req of
        [] | requestMethod req == method -> Just ([], method)
        _  -> Nothing
      where method = reflectMethod (Proxy :: Proxy method)
#endif

#if MIN_VERSION_servant(0,17,0)
instance ReflectMethod method => HasEndpoints (Servant.NoContentVerb (method :: StdMethod)) where
    getEndpoints _ = [([], method)]
      where method = reflectMethod (Proxy :: Proxy method)
    getEndpoint _ req = case pathInfo req of
        [] | requestMethod req == method -> Just ([], method)
        _  -> Nothing
      where method = reflectMethod (Proxy :: Proxy method)
#endif

#if MIN_VERSION_servant(0,18,2)
instance HasEndpoints (sub :: *) => HasEndpoints (Servant.Fragment a :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
#endif

#if MIN_VERSION_servant(0,20,0)
instance HasEndpoints (sub :: *) => HasEndpoints (Servant.WithResource res :> sub) where
    getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
#endif
