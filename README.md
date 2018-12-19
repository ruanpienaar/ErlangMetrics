# ErlangMetrics
A helper repo for metrics in Erlang

### Getting Started

###### Notes:
- Assumed O.S. : Mac ( Double check links )
- Docker Service ( docker-compse ) still TODO, the below steps installs everything on 1 machine

## 1. Erlang Web Server

```bash
$ git clone https://github.com/ruanpienaar/erlang_metrics
$ cd erlang_metrics && make
$ /_build/default/rel/myproject_release/bin/myproject_release console
```

Test erlang_metrics
```
$ curl localhost:54321/metrics
```


## 2. Prometheus

### 2.1. Prometheus Server

#### 2.1.1. Prometheus Server download and install

Download [link](https://prometheus.io/download/)
```bash
$ wget https://github.com/prometheus/prometheus/releases/download/v2.6.0/prometheus-2.6.0.darwin-amd64.tar.gz
$ tar -xvzf prometheus-2.6.0.darwin-amd64.tar.gz
$ cd prometheus-2.6.0.darwin-amd64
$ 
```

#### 2.1.2. Prometheus server config using erlang_metrics 

create/edit prometheus.yml:
```
global:
  scrape_interval:     15s
  evaluation_interval: 15s

rule_files:
  # - "first.rules"
  # - "second.rules"

scrape_configs:
  - job_name: prometheus
    static_configs:
      - targets: ['localhost:9090']
  - job_name: ’erlang_metrics’
    static_configs:
      - targets: ['localhost:54321']
```

#### 2.1.3. Prometheus server start

```
$ ./prometheus --config.file=prometheus.yml
```

- Prometheus server will expose it's own metrics on Port 9090 and /metrics

Test the prometheus server metrics running ok:
```
$ curl localhost:9090/metrics
```

Check that both targets are ok:
```
$ http://localhost:9090/targets
```

### 2.2 Prometheus expression browser

Visit the [expression-browser](http://localhost:9090/graph) to create graphs.

More Info at this [link](https://prometheus.io/docs/prometheus/latest/querying/basics/)

#### 2.2.1 Prometheus graph browser, and some example queries
Add
```
erlang_vm_memory_system_bytes_total
```
![prom_erlang_vm_memory_system_bytes_total_example](misc/prom_erlang_vm_memory_system_bytes_total_example.png?raw=true "prom_erlang_vm_memory_system_bytes_total_example")

To the query textbox on the graph web page


## 3. Grafana

## 4. Elastic

## 5. Kibana
