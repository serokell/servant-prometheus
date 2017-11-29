{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main (main) where

import           Data.Text                (Text)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp
import           Prometheus               (unregisterAll)
import           Servant
import           Servant.Prometheus
import           System.Process


type BenchApi = "hello" :> Capture "name" Text :> Get '[JSON] Text

benchApi :: Proxy BenchApi
benchApi = Proxy

server :: Server BenchApi
server = return

servantPrometheusServer :: MeasureQuantiles -> IO Application
servantPrometheusServer qants = do
  unregisterAll
  ms <- makeMeters benchApi qants
  return $ monitorServant benchApi ms (serve benchApi server)

benchApp :: IO Application -> IO ()
benchApp app = withApplication app $ \port ->
  callCommand $ "wrk -c 30 -d 20s --latency -s bench/wrk.lua -t 2 'http://localhost:" ++ show port ++ "'"

main :: IO ()
main = do
  putStrLn "Benchmarking servant-prometheus (no quantiles)"
  benchApp (servantPrometheusServer NoQuantiles)
  putStrLn "\nBenchmarking servant-prometheus (with quantiles)"
  benchApp (servantPrometheusServer WithQuantiles)
  putStrLn "\nBenchmarking without servant-prometheus"
  benchApp . return $ serve benchApi server
