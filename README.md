# servant-prometheus

Servant-prometheus allows you to record metrics about your servant applications on a per endpoint basis. It uses the information contained in the API's type to produce counters for all endpoints, and adds very little overhead (the included benchmarks show the benchmarked app can sustain 40k req/sec with monitoring, and 41k without, when quantiles are not measured).

In the example below, run time system metrics are also reported on using the [prometheus-metrics-ghc](https://hackage.haskell.org/package/prometheus-metrics-ghc) package. If using GHC metrics, make sure that your app is run with `+RTS -T` to allow your application to have access to the runtime stats.

The library is based on the [servant-ekg](https://hackage.haskell.org/package/servant-ekg) package, but differs in its preallocation of all meters at app launch, to avoid contention between all endpoint waiting on a single MVar. Due to the design of `prometheus-client`, there is still contention between threads responding to the same endpoint but the overhead is minimal, except in the case of Quantiles (Summaries in prometheus-client terminology), [see note below](#a-note-on-quantiles).

## Example

```haskell
import Network.Wai.Handler.Warp (run)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant (serve)

import qualified Servant.Prometheus as SP
...

appAPi :: Proxy AppAPI
appApi = Proxy

app :: Server AppAPI
app = ...


main = do
  register ghcMetrics
  -- Allocate the counters necessary for all app endpoints.
  meters <- register $ SP.meters appApi
  -- Fork a separate server for serving nothing but metrics,
  -- which you will point Prometheus at.
  forkIO $ run monitoringPort servePrometheusMetrics
  -- Run your app with metric monitoring.
  run port $ SP.monitorServant meters $ serve appApi app

```

## Benchmarking
The benchmarks for this library require the [wrk](https://github.com/wg/wrk) http benchmarking tool to be installed.

## A note on Quantiles
It is possible to estimate response time metrics per endpoint (50%, 95% and 99% latency for requests), but there is significant overhead when the response time for your endpoints is low. You should benchmark your application with and without qantiles enabled to decide whether they are worth it for your application. The output below shows the impact that quantiles can impose for very cheap endpoints:

```
$ bench +RTS -N
Benchmarking servant-prometheus (no quantiles)
Running 20s test @ http://localhost:52649
  2 threads and 30 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   658.38us    1.05ms  36.02ms   96.64%
    Req/Sec    20.57k     3.57k   28.41k    76.37%
  Latency Distribution
     50%  494.00us
     75%  760.00us
     90%    1.11ms
     99%    2.95ms
  822628 requests in 20.10s, 134.73MB read
  Socket errors: connect 0, read 56, write 2, timeout 0
Requests/sec:  40923.58
Transfer/sec:      6.70MB

Benchmarking servant-prometheus (with quantiles)
Running 20s test @ http://localhost:52743
  2 threads and 30 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     6.84ms    6.96ms 113.14ms   87.25%
    Req/Sec     2.66k   266.29     4.69k    75.50%
  Latency Distribution
     50%    5.05ms
     75%    9.57ms
     90%   15.35ms
     99%   31.31ms
  105890 requests in 20.02s, 17.25MB read
  Socket errors: connect 0, read 49, write 15, timeout 0
Requests/sec:   5290.48
Transfer/sec:      0.86MB

Benchmarking without servant-prometheus
Running 20s test @ http://localhost:52841
  2 threads and 30 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   690.92us  593.77us  23.68ms   93.29%
    Req/Sec    20.77k     2.45k   26.30k    78.50%
  Latency Distribution
     50%  570.00us
     75%  794.00us
     90%    1.11ms
     99%    2.73ms
  826638 requests in 20.00s, 135.38MB read
  Socket errors: connect 0, read 45, write 12, timeout 0
Requests/sec:  41322.76
Transfer/sec:      6.77MB
```
