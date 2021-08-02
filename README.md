# prometheus_statsd


## 使用

Register metrics:

```erlang

prometheus_gauge:new([{name, pool_size}, {help, "MongoDB Connections pool size"}]),
prometheus_counter:new([{name, http_requests_total}, {help, "Http request count"}]).
prometheus_summary:new([{name, orders}, {help, "Track orders count/total sum"}]).
prometheus_histogram:new([{name, http_request_duration_milliseconds},
                          {labels, [method]},
                          {bounds, [100, 300, 500, 750, 1000]},
                          {help, "Http Request execution time"}]).

```

Use metrics:

```erlang

prometheus_gauge:set(pool_size, 365),
prometheus_counter:inc(http_requests_total).
prometheus_summary:observe(orders, 10).
prometheus_summary:observe(orders, 15).
prometheus_histogram:observe(http_request_duration_milliseconds, [get], 95).
prometheus_histogram:observe(http_request_duration_milliseconds, [get], 100).
prometheus_histogram:observe(http_request_duration_milliseconds, [get], 102).
prometheus_histogram:observe(http_request_duration_milliseconds, [get], 150).
prometheus_histogram:observe(http_request_duration_milliseconds, [get], 250).
prometheus_histogram:observe(http_request_duration_milliseconds, [get], 75).
prometheus_histogram:observe(http_request_duration_milliseconds, [get], 350).
prometheus_histogram:observe(http_request_duration_milliseconds, [get], 550).
prometheus_histogram:observe(http_request_duration_milliseconds, [get], 950).
prometheus_histogram:observe(http_request_duration_milliseconds, [post], 500),
prometheus_histogram:observe(http_request_duration_milliseconds, [post], 150).
prometheus_histogram:observe(http_request_duration_milliseconds, [post], 450).
prometheus_histogram:observe(http_request_duration_milliseconds, [post], 850).
prometheus_histogram:observe(http_request_duration_milliseconds, [post], 750).
prometheus_histogram:observe(http_request_duration_milliseconds, [post], 1650).

```

Export metrics as text:

```erlang

io:format(prometheus_text_format:format()).

```


```erlang
prometheus_metrics:new(?MODULE, my_registry, [
    prometheus_mnesia_collector,
    prometheus_vm_memory_collector,
    prometheus_vm_statistics_collector,
    prometheus_vm_system_info_collector
]).
```

则priv目录下，文件名为my_registry.json为
```json
[
  {
    "name": "node",
    "help": "节点信息,CPU,内存",
    "type": "gauge",
    "labels": [
      {
        "label": "key",
        "values": [
          "load1",
          "load5",
          "load15",
          "max_fds",
          "allocated_memory",
          "total_memory",
          "used_memory"
        ]
      }
    ]
  }
]
```


License
-------

Apache License Version 2.0

Author
------

zhengweixing.

