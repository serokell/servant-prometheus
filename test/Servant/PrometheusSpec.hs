{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Servant.PrometheusSpec (spec) where

import           Data.Aeson
import qualified Data.HashMap.Strict                        as H
import           Data.List                                  (sort)
import           Data.Monoid
import           Data.Proxy
import           Data.Text                                  as T
import qualified Data.Text.Encoding                         as T
import           GHC.Generics
import           Network.HTTP.Client                        (defaultManagerSettings,
                                                             newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API.Internal.Test.ComprehensiveAPI (comprehensiveAPI)
import           Servant.Client
import           Test.Hspec

import           Prometheus                                 (getCounter,
                                                             getVectorWith)
import           Servant.Prometheus


-- * Spec

spec :: Spec
spec = describe "servant-prometheus" $ do

  let getEp :<|> postEp :<|> deleteEp = client testApi
  let t q = describe (show q) $ do
        it "collects number of request" $
          withApp q $ \port m -> do
            mgr <- newManager defaultManagerSettings
            let runFn :: ClientM a -> IO (Either ServantError a)
#if MIN_VERSION_servant_client(0,13,0)
                env = ClientEnv mgr (BaseUrl Http "localhost" port "") Nothing
#else
                env = ClientEnv mgr (BaseUrl Http "localhost" port "")
#endif
                runFn fn = runClientM fn env
            _ <- runFn $ getEp "name" Nothing
            _ <- runFn $ postEp (Greet "hi")
            _ <- runFn $ deleteEp "blah"
            case H.lookup "hello.:name.GET" m of
              Nothing -> fail "Expected some value"
              Just v -> do
                r <- getVectorWith getCounter (metersResponses v)
                Prelude.lookup "2XX" r `shouldBe` Just 1.0
            case H.lookup "greet.POST" m of
              Nothing -> fail "Expected some value"
              Just v  -> do
                r <- getVectorWith getCounter (metersResponses v)
                Prelude.lookup "2XX" r `shouldBe` Just 1.0
            case H.lookup "greet.:greetid.DELETE" m of
              Nothing -> fail "Expected some value"
              Just v  -> do
                r <- getVectorWith getCounter (metersResponses v)
                Prelude.lookup "2XX" r `shouldBe` Just 1.0
        it "has all endpoints" $
          withApp q $ \_ m ->
            sort (H.keys m) `shouldBe` sort ("unknown":Prelude.map
                                                (\(ps,method) -> T.intercalate "." $ ps <> [T.decodeUtf8 method])
                                                (getEndpoints testApi))
        -- TODO: Figure out how to test quantiles are being made with WithQuantiles


  t NoQuantiles
  t WithQuantiles
  it "is comprehensive" $ do
    let _typeLevelTest = monitorServant comprehensiveAPI undefined undefined undefined
    True `shouldBe` True


-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent


testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT (Int, String) IO' monad.
server :: Server TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH

  where helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH = return

        deleteGreetH _ = return NoContent

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

withApp :: MeasureQuantiles -> (Port -> H.HashMap Text Meters -> IO a) -> IO a
withApp qs a = do
  ms <- makeMeters testApi qs
  withApplication (return $ monitorServant testApi ms test) $ \p -> a p ms
