0.2.0.0
--------

* Minor changes in the interface of the library.
* Significant changes in names of the exposed metrics.
* Latency summaries (quantiles computed on the client side) are temporary
  not available. Histograms (quntiles computer on the server side) are
  not affected.
* New module: `Servant.Prometheus.Export`.

### Interface

* `makeMeters` is not simply `meters`. It does not register anything
   but returns a Prometheus metric that you’ll need to register and
   then pass the result to `monitorServant`.
* `monitorServant` no longer takes a proxy that specifies the server API
  as the API can be extracted from the type of the value that `meters`
  returns.

This brings the interface more in line with what `prometheus-client`
provides for its own metric types.

In short, change:

```haskell
meters <- SP.makeMeters appApi NoQuantiles
run {- ... -} $ SP.monitorServant appApi meters app
```

to

```haskell
meters <- P.register $ SP.meters appApi
run {- ... -} $ SP.monitorServant meters app
```

### Metric names

* `servant_path_<path>_<method>_http_status{status_code=<status>}` →
  `http_responses_total{path=<path>, method=<method>, status=<status>}`
* `servant_path_<path>_<method>_in_flight` →
  `http_requests_in_flight{path=<path>, method=<method>}`
* `servant_path_<path>_<method>_time_ms` →
  `http_request_duration_seconds{path=<path>, method=<method>}`
  (the value is now in seconds rather than ms)

These names are more in line with the examples given in the Prometheus
documentation. The `path` and `method` components have been moved
from the metric name to labels to allow for aggregation over endpoints.

Example of a new metric:

```
http_responses_total{path="profile/:username",method="GET",status="2XX"} 5.0
```

Also now all metrics with all possible (according to the API definition) labels
are initialised on start of the application in line with the [recommendations
from the Prometheus documentation][avoid-missing-metrics].

[avoid-missing-metrics]: https://prometheus.io/docs/practices/instrumentation/#avoid-missing-metrics

### Qunatiles

* Histograms (buckets) are now computed unconditionally as their are cheap.
* Default histogram buckets are slightly more fine-grained. They will become
  configurable in a future version.
* Summaries are temporary unavilable. They will be brought in a future
  version and will be configurable per-endpoint as before.

### `Servant.Prometheus.Export`

This new module containts utilities that allow you to easily export your app’s
metrics through Servant.

If you use `prometheus-client` to keep track of your metrics, your servant
app has some API (let’s call it `yourApi`), and you have implemented a server
for it, that is, you have `yourServer :: ServerT m yourApi`, you can just
wrap `WithMetrics` around `yourApi` and `withMetrics yourApiProxy`
around `yourServer` and get the `/metrics` endpoint handled for you.

### Internal changes

* Bump LTS to 14.
* Support servant 0.15 and 0.16.
* Use `getSystemTime` instead of `getCurrentTime`.
* Test are temporarily broken (please, help fix them!).
