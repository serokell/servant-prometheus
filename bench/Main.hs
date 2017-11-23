{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main (main) where

import           Data.IORef
import           Data.Text                (Text)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Prometheus
import           System.Process


type BenchApi = "hello" :> Capture "name" Text :> Get '[JSON] Text

benchApi :: Proxy BenchApi
benchApi = Proxy

server :: Server BenchApi
server = return

servantPrometheusServer :: MeasureQantiles -> IO Application
servantPrometheusServer qants = do
  ms <- newIORef mempty
  return $ monitorEndpoints benchApi qants ms (serve benchApi server)

benchApp :: IO Application -> IO ()
benchApp app = withApplication app $ \port ->
  callCommand $ "wrk -c 30 -d 20s --latency -s bench/wrk.lua -t 2 'http://localhost:" ++ show port ++ "'"

main :: IO ()
main = do
  putStrLn "Using IORefs"
  putStrLn "Benchmarking servant-prometheus (no qantiles)"
  benchApp (servantPrometheusServer NoQantiles)
  putStrLn "Benchmarking servant-prometheus (with qantiles)"
  benchApp (servantPrometheusServer WithQantiles)
  putStrLn "Benchmarking without servant-prometheus"
  benchApp . return $ serve benchApi server
