[TOC]

# 从本篇开始我们开始探讨的是Devops相关工具链的具体知识体系 :

在我们整个的Devops工具链里面，我们需要重点掌握以下几种工具：
 1.Jenkins：持续集成工具
 2.Ansible：持续部署工具
 3.SonarQbe：代码检查工具
 4.Terraform：基础设施管理工具
 5.GitHub：代码管理工具
 6.Prometheus：监控工具
 7.Kubernetes：容器编排工具
 8.Docker：容器
 这些工具贯穿于整个Devops系统的各个环节，我们需要对其有深刻的理解才能更好的搭建一套高可用的Devops工具链。
 工欲善其事，必先利其器。在所有的工作开始之前，我们需要一个比较完整的Infra层面的支撑，我们可以使用基于云平台的VM，或者是本地的物理机。在这里，我们还是依旧拿AWS作为基础平台来搭建整个的知识体系，所以对于工具链我们首先来熟悉一下 IT 基础架构自动化编排工具——Terraform 。

# Terraform 基本概念：

Terraform 是一个 IT 基础架构自动化编排工具，它的口号是 "Write, Plan, and create Infrastructure as Code", 基础架构即代码。Terraform 几乎可以支持所有市面上能见到的云服务，比如AWS ，阿里云，Azure，Google Cloud Platform ，Oracle Cloud Platform等，还有些私有的云比如OpenSatck等等。
 下面我们根据步骤来一步步使用Terraform：

##### 1.安装TerraForm：

首先我们必须在机器上安装terraform,下载地址：
 [terraform](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.terraform.io%2Fdownloads.html)
 Terraform打包为zip存档。下载Terraform后，解压缩包。Terraform作为单个二进制文件运行terraform。可以安全地删除包中的任何其他文件，Terraform仍然可以运行。



```ruby
curl https://releases.hashicorp.com/terraform/0.11.13/terraform_0.11.13_linux_amd64.zip -o terraform.zip
```

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6bvu1eoj60q901y3yq02.jpg)

1.download terraform



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6by7ah2j60lb02k3yv02.jpg)

2.解压terraform



```undefined
查看terraform版本：terraform --version
```

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6bz12buj60la053wfa02.jpg)

3.terraform安装成功

##### 2.Terraform 基本命令：



```css
1.mkdir：创建一个干净的工作目录，为后续操作做准备，该目录就像git的仓库

2.example.tf：需要创建一个.tf文件，指定provider等信息，工作目录下需要有至少一个tf文件，否则后续命令无法进行,在本文的例子中，我们默认为example.tf

3.terraform init：类似于git init，对当前目录做初始化，下载tf中的provider，并为后续的操作准备必要的环境条件

4.terraform plan：预览执行计划，不是必须的，但是强烈建议，主要是展示此次操作的具体内容。

5.terraform apply：真正执行编排计划

6.terraform show：显示当前状态。

7.terraform destroy：销毁云服务，将tf中的云服务清理干净，此操作跟AWS中CloudFormation的销毁操作类似，会回收所有的按照此tf文件创建出来的那内容
```

##### 3.基础环境搭建：

安装Terraform后，让我们直接进入它并开始创建一些基础架构：
 用于描述Terraform中基础结构的文件集简称为Terraform 配置，这边大家可以先了解下，下面会一个个讲解：



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6c7xdamj60fx05bjs002.jpg)

4.terraform配置文件

- **example.tf**
   第一个配置来启动单个**AWS EC2**实例:
   首先必须强调一下的是，对于每个terraform配置组来说，provider是必须的，在这里，为了演示方便，我们使用的是dafault的VPC：

```bash
provider "aws" {
  access_key = "${var.access_key}"
  secret_key = "${var.secret_key}"
  region     = "${var.region}"
}
resource "aws_instance" "gtxy_ec2_1" {
  ami           = "ami-053617c9d818c1189"
  instance_type = "t2.micro"
  provisioner "local-exec" {
    command = "echo ${aws_instance.gtxy_ec2_1.public_ip} > ip_address.txt"
  }
}
resource "aws_eip" "ip" {
  instance = "${aws_instance.gtxy_ec2_1.id}"
}
```



```undefined
provider：IaaS层提供商，例如AWS，Auzre等
resource：IaaS层资源类型，我们这里定义的是一个AWS Instance
    - aws_instance ：instance 类型
    - gtxy_ec2_1 ：instance 名称
```

- **variables.tf**
   有些参数我们不想通过硬编码的方式写入到tf中，我们就会采用变量方式来搞定这种场景,一般我们会把所有的变量都单独拿到一个tf文件里去声明，例如variables.tf，虽然不是必须要命名成variables.tf，但是我们约定俗成这么做。



```cpp
variable "access_key" {}
variable "secret_key" {}
variable "region" {
  default = "cn-north-1"
}
```

很好理解，脚本里region我们给了默认值cn-north-1，但是其他两个值均未给出，所以我们需要动态赋值。在其他tf中引用方式如下：



```csharp
方式一：命令参数设置
$ terraform apply -var 'access_key=foo' -var 'secret_key=bar'
```



```bash
方式二：默认参数文件
Terraform默认会加载terraform.tfvars的文件为初始化参数的文件，文件内容是键值对的方式：
access_key = "foo"
secret_key = "bar"
```



```csharp
方式三：命令制定参数文件
如果不按照方式二的命名规则，而是自己自定义文件名，可以采用方式一和方式二结合的方式指定参数文件：
$ terraform apply -var-file="secret.tfvars" -var-file="production.tfvars"
```

- backend.tf

  将Terraform的state文件维护在云端或远程服务器，这样既可以保证高可用性，也可以方便多名编排人员共同维护

  ![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6cwgcpjj60d604laa302.jpg)

  5.backend配置文件

  这样在执行terraform init时就会在本地和remote端各维护一份状态文件

- ip_address.txt

  当resource创建完毕后可以将EIP写到ip_address.txt文件中

  ![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6czzx7xj60o501wglm02.jpg)

  6.将EIP写到本地

- outputs.tf

  控制台输出，也方便后续资源使用该资源：

  ![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6d1v7faj60bq01zmx102.jpg)

  7.控制台输出ip

- **小技巧**
   我们可以将整个的堆栈以图形化的方式展现：



```undefined
yum install graphviz
terraform graph | dot -Tsvg > graph.svg
```

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6d3qoq7j60g205vdgl02.jpg)

image.png

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh65vjgz7j60vg0m70ud02.jpg)

8.terraform构建栈

##### 4.小结：

Terraform在Iaas基础维护方面的侧重点，只是对云平台实例级别的管理，如果要对实例内部进行更复杂的编排需要配合Ansible组件。下一章我们会详细介绍下Ansible相关内容~



7人点赞



[深度剖析Devops系列]()





作者：小E的私房菜
链接：https://www.jianshu.com/p/2252d28ec681
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---



# 一、prometheus简介：

Prometheus是由SoundCloud开发的开源监控报警系统和时序列数据库(TSDB)。Prometheus使用Go语言开发，是Google BorgMon监控系统的开源版本。

#### 1.Prometheus的特点

- 多维度数据模型。
- 灵活的查询语言。
- 不依赖分布式存储，单个服务器节点是自主的。
- 通过基于HTTP的pull方式采集时序数据。
- 可以通过中间网关进行时序列数据推送。
- 通过服务发现或者静态配置来发现目标服务对象。
- 支持多种多样的图表和界面展示，比如**Grafana**等。

需要指出的是，由于数据采集可能会有丢失，所以 Prometheus 不适用对采集数据要 100% 准确的情形。但如果用于记录时间序列数据，Prometheus 具有很大的查询优势，此外，Prometheus 适用于微服务的体系架构。

#### 2.Prometheus 组成及架构

Prometheus生态系统由多个组件组成，它们中的一些是可选的。

- Prometheus Server：主要负责数据采集和存储，提供PromQL查询语言的支持

- 客户端SDK：官方提供的客户端类库有go、java、scala、python、ruby，其他还有很多第三方开发的类库，支持nodejs、php、erlang等。

- Push Gateway：支持临时性Job主动推送指标的中间网关。

- PromDash：可视化指标数据

- alertmanager：警告管理器，用来进行报警。

- prometheus_cli：命令行工具，可以用来安装插件等

  ![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6dcrfcsj60lm0cqjsj02.jpg)

  1.prometheus-架构

  Prometheus的

  基本原理是通过HTTP协议周期性抓取被监控组件的状态，任意组件只要提供对应的HTTP接口就可以接入监控。不需要任何SDK或者其他的集成过程。

  这样做非常适合做虚拟化环境监控系统，比如VM、Docker、Kubernetes等。输出被监控组件信息的HTTP接口被叫做exporter 。目前互联网公司常用的组件大部分都有exporter可以直接使用，比如Varnish、Haproxy、Nginx、MySQL、Linux系统信息(包括磁盘、内存、CPU、网络等等)。

# 二、基础环境

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6dgie0ij60na037t9402.jpg)

2.Prometheus-基础环境

------

# 三、Prometheus安装

#### 1.安装：



```ruby
下载：wget https://github.com/prometheus/prometheus/releases/download/v1.6.3/prometheus-1.6.3.linux-amd64.tar.gz
解压：tar xzvf prometheus-1.6.3.linux-amd64.tar.gz$ mv prometheus-1.6.3.linux-amd64 /usr/local/prometheus
```

#### 2.安装验证：

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6diq532j60rb04kjry02.jpg)

3.prometheus-安装验证

#### 3.配置Prometheus



```php
# my global config
global:
  scrape_interval:     15s # Set the scrape interval to every 15 seconds. Default is every 1 minute.
  evaluation_interval: 15s # Evaluate rules every 15 seconds. The default is every 1 minute.
  # scrape_timeout is set to the global default (10s).

  # Attach these labels to any time series or alerts when communicating with
  # external systems (federation, remote storage, Alertmanager).
  external_labels:
      monitor: 'codelab-monitor'

# Load rules once and periodically evaluate them according to the global 'evaluation_interval'.
rule_files:
  # - "first.rules"
  # - "second.rules"

# Attach these labels to any time series or alerts when communicating with
  # external systems (federation, remote storage, Alertmanager).
  external_labels:
      monitor: 'codelab-monitor'

# Load rules once and periodically evaluate them according to the global 'evaluation_interval'.
rule_files:
  # - "first.rules"
  # - "second.rules"

# A scrape configuration containing exactly one endpoint to scrape:
# Here it's Prometheus itself.
scrape_configs:
  # The job name is added as a label `job=<job_name>` to any timeseries scraped from this config.
  - job_name: 'prometheus'

    # metrics_path defaults to '/metrics'
    # scheme defaults to 'http'.

    static_configs:
      - targets: ['localhost:9090','localhost:9100']
        labels:
          instance: Prometheus
```

1. global：全局配置

- scrape_interval : 默认抓取间隔, 15秒向目标抓取一次数据evaluation_interval:每15秒评估一次规则

- external_labels: 这个标签是在本机上每一条时间序列上都会默认产生的，主要可以用于联合查询、远程存储、Alertmanger时使用。

- rule_files:加载规则一次，并根据全局'evaluation_interval'定期评估它们。

- scrape_configs:抓取promethues自身的配置

- job name：这个配置是表示在这个配置内的时间序例，每一条都会自动添加上这个{job_name:"prometheus"}的标签。

- scrape_interval:重写了全局抓取间隔时间，由15秒重写成5秒。
   2.启动promethues：

  ![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6dm5afsj60wj08hwgf02.jpg)

  4.promethues-启动promethues

  ![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6dnygrrj60xc0ejdh202.jpg)

  5.promethues-Dashboard

  由于promethues自带的DashBoard不是十分友善，因此，我们强烈推荐使用Grafana：

# 四、Grafana：

1.安装：



```ruby
下载：wget https://dl.grafana.com/oss/release/grafana-6.1.6-1.x86_64.rpm
安装：rpm -ivh grafana-6.1.6-1.x86_64.rpm
```

grafana 默认在工作在3000端口



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6dwgmdjj60xc0h475g02.jpg)

6.grafana-Dashboard



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6e14gb1j60xc0cxdgk02.jpg)

7.grafana-配置数据源为prometheus



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6dy78v0j60xc0g7mz902.jpg)

8.grafana-dashboard for prometheus

2.优化grafana（插件安装）：



```dart
dashboard json：
{
  "__inputs": [
    {
      "name": "DS_PROMETHEUS_111",
      "label": "prometheus",
      "description": "",
      "type": "datasource",
      "pluginId": "prometheus",
      "pluginName": "Prometheus"
    }
  ],
  "__requires": [
    {
      "type": "grafana",
      "id": "grafana",
      "name": "Grafana",
      "version": "5.3.2"
    },
    {
      "type": "panel",
      "id": "grafana-piechart-panel",
      "name": "Pie Chart",
      "version": "1.3.3"
    },
    {
      "type": "panel",
      "id": "graph",
      "name": "Graph",
      "version": "5.0.0"
    },
    {
      "type": "datasource",
      "id": "prometheus",
      "name": "Prometheus",
      "version": "5.0.0"
    },
    {
      "type": "panel",
      "id": "singlestat",
      "name": "Singlestat",
      "version": "5.0.0"
    },
    {
      "type": "panel",
      "id": "table",
      "name": "Table",
      "version": "5.0.0"
    },
    {
      "type": "panel",
      "id": "text",
      "name": "Text",
      "version": "5.0.0"
    }
  ],
  "annotations": {
    "list": [
      {
        "builtIn": 1,
        "datasource": "-- Grafana --",
        "enable": true,
        "hide": true,
        "iconColor": "rgba(0, 211, 255, 1)",
        "name": "Annotations & Alerts",
        "type": "dashboard"
      }
    ]
  },
  "description": "使用 Node Exporter v0.16，精简优化重要指标展示。\r\n包含：CPU 内存 磁盘 IO 网络 温度等监控指标。\r\nhttps://github.com/starsliao/Prometheus",
  "editable": true,
  "gnetId": 8919,
  "graphTooltip": 0,
  "id": null,
  "iteration": 1542357033300,
  "links": [],
  "panels": [
    {
      "content": "",
      "editable": true,
      "error": false,
      "gridPos": {
        "h": 1,
        "w": 24,
        "x": 0,
        "y": 0
      },
      "id": 11,
      "links": [],
      "minSpan": 4,
      "mode": "html",
      "repeat": "node",
      "repeatDirection": "h",
      "style": {},
      "title": "$node",
      "type": "text"
    },
    {
      "cacheTimeout": null,
      "colorBackground": false,
      "colorValue": false,
      "colors": [
        "rgba(245, 54, 54, 0.9)",
        "rgba(237, 129, 40, 0.89)",
        "rgba(50, 172, 45, 0.97)"
      ],
      "datasource": "${DS_PROMETHEUS_111}",
      "decimals": 1,
      "description": "",
      "format": "s",
      "gauge": {
        "maxValue": 100,
        "minValue": 0,
        "show": false,
        "thresholdLabels": false,
        "thresholdMarkers": true
      },
      "gridPos": {
        "h": 5,
        "w": 2,
        "x": 0,
        "y": 1
      },
      "hideTimeOverride": true,
      "id": 15,
      "interval": null,
      "links": [],
      "mappingType": 1,
      "mappingTypes": [
        {
          "name": "value to text",
          "value": 1
        },
        {
          "name": "range to text",
          "value": 2
        }
      ],
      "maxDataPoints": 100,
      "nullPointMode": "null",
      "nullText": null,
      "postfix": "",
      "postfixFontSize": "50%",
      "prefix": "",
      "prefixFontSize": "50%",
      "rangeMaps": [
        {
          "from": "null",
          "text": "N/A",
          "to": "null"
        }
      ],
      "sparkline": {
        "fillColor": "rgba(31, 118, 189, 0.18)",
        "full": false,
        "lineColor": "rgb(31, 120, 193)",
        "show": false
      },
      "tableColumn": "",
      "targets": [
        {
          "expr": "time() - node_boot_time_seconds{instance=~\"$node\"}",
          "format": "time_series",
          "hide": false,
          "instant": true,
          "intervalFactor": 2,
          "refId": "A",
          "step": 40
        }
      ],
      "thresholds": "",
      "title": "系统运行时间",
      "transparent": false,
      "type": "singlestat",
      "valueFontSize": "100%",
      "valueMaps": [
        {
          "op": "=",
          "text": "N/A",
          "value": "null"
        }
      ],
      "valueName": "current"
    },
    {
      "cacheTimeout": null,
      "colorBackground": false,
      "colorValue": false,
      "colors": [
        "rgba(245, 54, 54, 0.9)",
        "rgba(237, 129, 40, 0.89)",
        "rgba(50, 172, 45, 0.97)"
      ],
      "datasource": "${DS_PROMETHEUS_111}",
      "description": "",
      "format": "short",
      "gauge": {
        "maxValue": 100,
        "minValue": 0,
        "show": false,
        "thresholdLabels": false,
        "thresholdMarkers": true
      },
      "gridPos": {
        "h": 2,
        "w": 2,
        "x": 2,
        "y": 1
      },
      "id": 14,
      "interval": null,
      "links": [],
      "mappingType": 1,
      "mappingTypes": [
        {
          "name": "value to text",
          "value": 1
        },
        {
          "name": "range to text",
          "value": 2
        }
      ],
      "maxDataPoints": 100,
      "minSpan": 4,
      "nullPointMode": "null",
      "nullText": null,
      "postfix": "",
      "postfixFontSize": "50%",
      "prefix": "",
      "prefixFontSize": "50%",
      "rangeMaps": [
        {
          "from": "null",
          "text": "N/A",
          "to": "null"
        }
      ],
      "sparkline": {
        "fillColor": "rgba(31, 118, 189, 0.18)",
        "full": false,
        "lineColor": "rgb(31, 120, 193)",
        "show": false
      },
      "tableColumn": "",
      "targets": [
        {
          "expr": "count(count(node_cpu_seconds_total{instance=~\"$node\", mode='system'}) by (cpu))",
          "format": "time_series",
          "instant": true,
          "intervalFactor": 1,
          "legendFormat": "",
          "refId": "A",
          "step": 20
        }
      ],
      "thresholds": "",
      "title": "CPU 核数",
      "type": "singlestat",
      "valueFontSize": "100%",
      "valueMaps": [
        {
          "op": "=",
          "text": "N/A",
          "value": "null"
        }
      ],
      "valueName": "current"
    },
    {
      "cacheTimeout": null,
      "colorBackground": false,
      "colorValue": true,
      "colors": [
        "rgba(50, 172, 45, 0.97)",
        "rgba(237, 129, 40, 0.89)",
        "rgba(245, 54, 54, 0.9)"
      ],
      "datasource": "${DS_PROMETHEUS_111}",
      "decimals": 2,
      "description": "",
      "format": "percent",
      "gauge": {
        "maxValue": 100,
        "minValue": 0,
        "show": true,
        "thresholdLabels": false,
        "thresholdMarkers": true
      },
      "gridPos": {
        "h": 5,
        "w": 3,
        "x": 4,
        "y": 1
      },
      "id": 167,
      "interval": null,
      "links": [],
      "mappingType": 1,
      "mappingTypes": [
        {
          "name": "value to text",
          "value": 1
        },
        {
          "name": "range to text",
          "value": 2
        }
      ],
      "maxDataPoints": 100,
      "minSpan": 2,
      "nullPointMode": "null",
      "nullText": null,
      "postfix": "",
      "postfixFontSize": "50%",
      "prefix": "",
      "prefixFontSize": "50%",
      "rangeMaps": [
        {
          "from": "null",
          "text": "N/A",
          "to": "null"
        }
      ],
      "sparkline": {
        "fillColor": "rgba(31, 118, 189, 0.18)",
        "full": false,
        "lineColor": "rgb(31, 120, 193)",
        "show": true
      },
      "tableColumn": "",
      "targets": [
        {
          "expr": "100 - (avg(irate(node_cpu_seconds_total{instance=~\"$node\",mode=\"idle\"}[5m])) * 100)",
          "format": "time_series",
          "hide": false,
          "interval": "",
          "intervalFactor": 1,
          "legendFormat": "",
          "refId": "A",
          "step": 20
        }
      ],
      "thresholds": "50,80",
      "title": "CPU使用率（5m）",
      "type": "singlestat",
      "valueFontSize": "80%",
      "valueMaps": [
        {
          "op": "=",
          "text": "N/A",
          "value": "null"
        }
      ],
      "valueName": "current"
    },
    {
      "cacheTimeout": null,
      "colorBackground": false,
      "colorValue": true,
      "colors": [
        "rgba(50, 172, 45, 0.97)",
        "rgba(237, 129, 40, 0.89)",
        "rgba(245, 54, 54, 0.9)"
      ],
      "datasource": "${DS_PROMETHEUS_111}",
      "decimals": 2,
      "description": "",
      "format": "percent",
      "gauge": {
        "maxValue": 100,
        "minValue": 0,
        "show": true,
        "thresholdLabels": false,
        "thresholdMarkers": true
      },
      "gridPos": {
        "h": 5,
        "w": 3,
        "x": 7,
        "y": 1
      },
      "id": 20,
      "interval": null,
      "links": [],
      "mappingType": 1,
      "mappingTypes": [
        {
          "name": "value to text",
          "value": 1
        },
        {
          "name": "range to text",
          "value": 2
        }
      ],
      "maxDataPoints": 100,
      "minSpan": 2,
      "nullPointMode": "null",
      "nullText": null,
      "postfix": "",
      "postfixFontSize": "50%",
      "prefix": "",
      "prefixFontSize": "50%",
      "rangeMaps": [
        {
          "from": "null",
          "text": "N/A",
          "to": "null"
        }
      ],
      "sparkline": {
        "fillColor": "rgba(31, 118, 189, 0.18)",
        "full": false,
        "lineColor": "rgb(31, 120, 193)",
        "show": true
      },
      "tableColumn": "",
      "targets": [
        {
          "expr": "avg(irate(node_cpu_seconds_total{instance=~\"$node\",mode=\"iowait\"}[5m])) * 100",
          "format": "time_series",
          "hide": false,
          "interval": "",
          "intervalFactor": 1,
          "legendFormat": "",
          "refId": "A",
          "step": 20
        }
      ],
      "thresholds": "20,50",
      "title": "CPU iowait（5m）",
      "type": "singlestat",
      "valueFontSize": "80%",
      "valueMaps": [
        {
          "op": "=",
          "text": "N/A",
          "value": "null"
        }
      ],
      "valueName": "current"
    },
    {
      "cacheTimeout": null,
      "colorBackground": false,
      "colorValue": true,
      "colors": [
        "rgba(50, 172, 45, 0.97)",
        "rgba(237, 129, 40, 0.89)",
        "rgba(245, 54, 54, 0.9)"
      ],
      "datasource": "${DS_PROMETHEUS_111}",
      "decimals": 0,
      "description": "",
      "format": "percent",
      "gauge": {
        "maxValue": 100,
        "minValue": 0,
        "show": true,
        "thresholdLabels": false,
        "thresholdMarkers": true
      },
      "gridPos": {
        "h": 5,
        "w": 3,
        "x": 10,
        "y": 1
      },
      "hideTimeOverride": false,
      "id": 172,
      "interval": null,
      "links": [],
      "mappingType": 1,
      "mappingTypes": [
        {
          "name": "value to text",
          "value": 1
        },
        {
          "name": "range to text",
          "value": 2
        }
      ],
      "maxDataPoints": 100,
      "minSpan": 4,
      "nullPointMode": "null",
      "nullText": null,
      "postfix": "",
      "postfixFontSize": "50%",
      "prefix": "",
      "prefixFontSize": "50%",
      "rangeMaps": [
        {
          "from": "null",
          "text": "N/A",
          "to": "null"
        }
      ],
      "sparkline": {
        "fillColor": "rgba(31, 118, 189, 0.18)",
        "full": false,
        "lineColor": "rgb(31, 120, 193)",
        "show": true
      },
      "tableColumn": "",
      "targets": [
        {
          "expr": "((node_memory_MemTotal_bytes{instance=~\"$node\"} - node_memory_MemFree_bytes{instance=~\"$node\"} - node_memory_Buffers_bytes{instance=~\"$node\"} - node_memory_Cached_bytes{instance=~\"$node\"}) / (node_memory_MemTotal_bytes{instance=~\"$node\"} )) * 100",
          "format": "time_series",
          "hide": false,
          "interval": "10s",
          "intervalFactor": 1,
          "refId": "A",
          "step": 20
        }
      ],
      "thresholds": "80,90",
      "title": "内存使用率",
      "type": "singlestat",
      "valueFontSize": "80%",
      "valueMaps": [],
      "valueName": "current"
    },
    {
      "cacheTimeout": null,
      "colorBackground": false,
      "colorPostfix": false,
      "colorPrefix": false,
      "colorValue": true,
      "colors": [
        "rgba(50, 172, 45, 0.97)",
        "rgba(237, 129, 40, 0.89)",
        "rgba(245, 54, 54, 0.9)"
      ],
      "datasource": "${DS_PROMETHEUS_111}",
      "decimals": 2,
      "description": "",
      "format": "short",
      "gauge": {
        "maxValue": 10000,
        "minValue": null,
        "show": true,
        "thresholdLabels": false,
        "thresholdMarkers": true
      },
      "gridPos": {
        "h": 5,
        "w": 3,
        "x": 13,
        "y": 1
      },
      "hideTimeOverride": false,
      "id": 16,
      "interval": null,
      "links": [],
      "mappingType": 1,
      "mappingTypes": [
        {
          "name": "value to text",
          "value": 1
        },
        {
          "name": "range to text",
          "value": 2
        }
      ],
      "maxDataPoints": 100,
      "minSpan": 4,
      "nullPointMode": "null",
      "nullText": null,
      "postfix": "",
      "postfixFontSize": "50%",
      "prefix": "",
      "prefixFontSize": "50%",
      "rangeMaps": [
        {
          "from": "null",
          "text": "N/A",
          "to": "null"
        }
      ],
      "sparkline": {
        "fillColor": "rgba(31, 118, 189, 0.18)",
        "full": false,
        "lineColor": "rgb(31, 120, 193)",
        "show": true
      },
      "tableColumn": "",
      "targets": [
        {
          "expr": "node_filefd_allocated{instance=~\"$node\"}",
          "format": "time_series",
          "interval": "10s",
          "intervalFactor": 1,
          "refId": "B"
        }
      ],
      "thresholds": "7000,9000",
      "title": "当前打开的文件描述符",
      "transparent": false,
      "type": "singlestat",
      "valueFontSize": "70%",
      "valueMaps": [],
      "valueName": "current"
    },
    {
      "cacheTimeout": null,
      "colorBackground": false,
      "colorValue": true,
      "colors": [
        "rgba(50, 172, 45, 0.97)",
        "rgba(237, 129, 40, 0.89)",
        "rgba(245, 54, 54, 0.9)"
      ],
      "datasource": "${DS_PROMETHEUS_111}",
      "decimals": null,
      "description": "",
      "format": "percent",
      "gauge": {
        "maxValue": 100,
        "minValue": 0,
        "show": true,
        "thresholdLabels": false,
        "thresholdMarkers": true
      },
      "gridPos": {
        "h": 5,
        "w": 4,
        "x": 16,
        "y": 1
      },
      "id": 166,
      "interval": null,
      "links": [],
      "mappingType": 1,
      "mappingTypes": [
        {
          "name": "value to text",
          "value": 1
        },
        {
          "name": "range to text",
          "value": 2
        }
      ],
      "maxDataPoints": 100,
      "minSpan": 4,
      "nullPointMode": "null",
      "nullText": null,
      "postfix": "",
      "postfixFontSize": "50%",
      "prefix": "",
      "prefixFontSize": "50%",
      "rangeMaps": [
        {
          "from": "null",
          "text": "N/A",
          "to": "null"
        }
      ],
      "repeatDirection": "h",
      "sparkline": {
        "fillColor": "rgba(31, 118, 189, 0.18)",
        "full": false,
        "lineColor": "rgb(31, 120, 193)",
        "show": true
      },
      "tableColumn": "",
      "targets": [
        {
          "expr": "100 - ((node_filesystem_avail_bytes{instance=~\"$node\",mountpoint=\"/\",fstype=~\"ext4|xfs\"} * 100) / node_filesystem_size_bytes {instance=~\"$node\",mountpoint=\"/\",fstype=~\"ext4|xfs\"})",
          "format": "time_series",
          "interval": "10s",
          "intervalFactor": 1,
          "refId": "A",
          "step": 20
        }
      ],
      "thresholds": "70,90",
      "title": "根分区使用率",
      "type": "singlestat",
      "valueFontSize": "80%",
      "valueMaps": [
        {
          "op": "=",
          "text": "N/A",
          "value": "null"
        }
      ],
      "valueName": "current"
    },
    {
      "cacheTimeout": null,
      "colorBackground": false,
      "colorValue": true,
      "colors": [
        "rgba(50, 172, 45, 0.97)",
        "rgba(237, 129, 40, 0.89)",
        "rgba(245, 54, 54, 0.9)"
      ],
      "datasource": "${DS_PROMETHEUS_111}",
      "decimals": null,
      "description": "通过变量maxmount获取最大的分区。",
      "format": "percent",
      "gauge": {
        "maxValue": 100,
        "minValue": 0,
        "show": true,
        "thresholdLabels": false,
        "thresholdMarkers": true
      },
      "gridPos": {
        "h": 5,
        "w": 4,
        "x": 20,
        "y": 1
      },
      "id": 154,
      "interval": null,
      "links": [],
      "mappingType": 1,
      "mappingTypes": [
        {
          "name": "value to text",
          "value": 1
        },
        {
          "name": "range to text",
          "value": 2
        }
      ],
      "maxDataPoints": 100,
      "minSpan": 4,
      "nullPointMode": "null",
      "nullText": null,
      "postfix": "",
      "postfixFontSize": "50%",
      "prefix": "",
      "prefixFontSize": "50%",
      "rangeMaps": [
        {
          "from": "null",
          "text": "N/A",
          "to": "null"
        }
      ],
      "repeat": null,
      "repeatDirection": "h",
      "sparkline": {
        "fillColor": "rgba(31, 118, 189, 0.18)",
        "full": false,
        "lineColor": "rgb(31, 120, 193)",
        "show": true
      },
      "tableColumn": "",
      "targets": [
        {
          "expr": "100 - ((node_filesystem_avail_bytes{instance=~\"$node\",mountpoint=\"$maxmount\",fstype=~\"ext4|xfs\"} * 100) / node_filesystem_size_bytes {instance=~\"$node\",mountpoint=\"$maxmount\",fstype=~\"ext4|xfs\"})",
          "format": "time_series",
          "interval": "10s",
          "intervalFactor": 1,
          "refId": "A",
          "step": 20
        }
      ],
      "thresholds": "70,90",
      "title": "最大分区($maxmount)使用率",
      "type": "singlestat",
      "valueFontSize": "80%",
      "valueMaps": [
        {
          "op": "=",
          "text": "N/A",
          "value": "null"
        }
      ],
      "valueName": "current"
    },
    {
      "cacheTimeout": null,
      "colorBackground": false,
      "colorValue": false,
      "colors": [
        "rgba(245, 54, 54, 0.9)",
        "rgba(237, 129, 40, 0.89)",
        "rgba(50, 172, 45, 0.97)"
      ],
      "datasource": "${DS_PROMETHEUS_111}",
      "decimals": null,
      "description": "",
      "format": "bytes",
      "gauge": {
        "maxValue": 100,
        "minValue": 0,
        "show": false,
        "thresholdLabels": false,
        "thresholdMarkers": true
      },
      "gridPos": {
        "h": 3,
        "w": 2,
        "x": 2,
        "y": 3
      },
      "id": 75,
      "interval": null,
      "links": [],
      "mappingType": 1,
      "mappingTypes": [
        {
          "name": "value to text",
          "value": 1
        },
        {
          "name": "range to text",
          "value": 2
        }
      ],
      "maxDataPoints": 100,
      "minSpan": 4,
      "nullPointMode": "null",
      "nullText": null,
      "postfix": "",
      "postfixFontSize": "70%",
      "prefix": "",
      "prefixFontSize": "50%",
      "rangeMaps": [
        {
          "from": "null",
          "text": "N/A",
          "to": "null"
        }
      ],
      "sparkline": {
        "fillColor": "rgba(31, 118, 189, 0.18)",
        "full": false,
        "lineColor": "rgb(31, 120, 193)",
        "show": false
      },
      "tableColumn": "",
      "targets": [
        {
          "expr": "node_memory_MemTotal_bytes{instance=~\"$node\"}",
          "format": "time_series",
          "instant": true,
          "intervalFactor": 1,
          "legendFormat": "{{instance}}",
          "refId": "A",
          "step": 20
        }
      ],
      "thresholds": "",
      "title": "内存总量",
      "type": "singlestat",
      "valueFontSize": "80%",
      "valueMaps": [
        {
          "op": "=",
          "text": "N/A",
          "value": "null"
        }
      ],
      "valueName": "current"
    },
    {
      "aliasColors": {
        "15分钟": "#6ED0E0",
        "1分钟": "#BF1B00",
        "5分钟": "#CCA300"
      },
      "bars": false,
      "dashLength": 10,
      "dashes": false,
      "datasource": "${DS_PROMETHEUS_111}",
      "editable": true,
      "error": false,
      "fill": 1,
      "grid": {},
      "gridPos": {
        "h": 6,
        "w": 11,
        "x": 0,
        "y": 6
      },
      "height": "300",
      "id": 13,
      "legend": {
        "alignAsTable": true,
        "avg": true,
        "current": true,
        "max": true,
        "min": false,
        "rightSide": true,
        "show": true,
        "total": false,
        "values": true
      },
      "lines": true,
      "linewidth": 2,
      "links": [],
      "minSpan": 4,
      "nullPointMode": "null as zero",
      "percentage": false,
      "pointradius": 5,
      "points": false,
      "renderer": "flot",
      "repeat": null,
      "seriesOverrides": [],
      "spaceLength": 10,
      "stack": false,
      "steppedLine": false,
      "targets": [
        {
          "expr": "node_load1{instance=~\"$node\"}",
          "format": "time_series",
          "instant": false,
          "interval": "10s",
          "intervalFactor": 2,
          "legendFormat": "1m",
          "metric": "",
          "refId": "A",
          "step": 20,
          "target": ""
        },
        {
          "expr": "node_load5{instance=~\"$node\"}",
          "format": "time_series",
          "instant": false,
          "interval": "10s",
          "intervalFactor": 2,
          "legendFormat": "5m",
          "refId": "B",
          "step": 20
        },
        {
          "expr": "node_load15{instance=~\"$node\"}",
          "format": "time_series",
          "instant": false,
          "interval": "10s",
          "intervalFactor": 2,
          "legendFormat": "15m",
          "refId": "C",
          "step": 20
        }
      ],
      "thresholds": [],
      "timeFrom": null,
      "timeShift": null,
      "title": "系统平均负载",
      "tooltip": {
        "msResolution": false,
        "shared": true,
        "sort": 0,
        "value_type": "cumulative"
      },
      "type": "graph",
      "xaxis": {
        "buckets": null,
        "mode": "time",
        "name": null,
        "show": true,
        "values": []
      },
      "yaxes": [
        {
          "format": "short",
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        },
        {
          "format": "short",
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        }
      ],
      "yaxis": {
        "align": false,
        "alignLevel": null
      }
    },
    {
      "aliasColors": {
        "/": "#eab839",
        "/boot": "#bf1b00",
        "/data": "#1f78c1"
      },
      "breakPoint": "100%",
      "cacheTimeout": null,
      "combine": {
        "label": "Others",
        "threshold": ""
      },
      "datasource": "${DS_PROMETHEUS_111}",
      "decimals": 1,
      "fontSize": "50%",
      "format": "bytes",
      "gridPos": {
        "h": 6,
        "w": 5,
        "x": 11,
        "y": 6
      },
      "hideTimeOverride": false,
      "id": 171,
      "interval": null,
      "legend": {
        "header": "",
        "percentage": false,
        "percentageDecimals": 0,
        "show": true,
        "sideWidth": 142,
        "values": true
      },
      "legendType": "Right side",
      "links": [],
      "maxDataPoints": 3,
      "nullPointMode": "connected",
      "pieType": "pie",
      "strokeWidth": "2",
      "targets": [
        {
          "expr": "node_filesystem_size_bytes {instance=~\"$node\",fstype=~\"ext4|xfs\"}",
          "format": "time_series",
          "instant": true,
          "interval": "10s",
          "intervalFactor": 1,
          "legendFormat": "{{mountpoint}}",
          "refId": "A"
        }
      ],
      "title": "磁盘总空间",
      "type": "grafana-piechart-panel",
      "valueName": "current"
    },
    {
      "columns": [],
      "datasource": "${DS_PROMETHEUS_111}",
      "fontSize": "120%",
      "gridPos": {
        "h": 6,
        "w": 8,
        "x": 16,
        "y": 6
      },
      "id": 164,
      "links": [],
      "pageSize": null,
      "scroll": true,
      "showHeader": true,
      "sort": {
        "col": 11,
        "desc": true
      },
      "styles": [
        {
          "alias": "Time",
          "dateFormat": "YYYY-MM-DD HH:mm:ss",
          "pattern": "Time",
          "type": "hidden"
        },
        {
          "alias": "分区",
          "colorMode": null,
          "colors": [
            "rgba(50, 172, 45, 0.97)",
            "rgba(237, 129, 40, 0.89)",
            "rgba(245, 54, 54, 0.9)"
          ],
          "dateFormat": "YYYY-MM-DD HH:mm:ss",
          "decimals": 2,
          "mappingType": 1,
          "pattern": "mountpoint",
          "thresholds": [
            ""
          ],
          "type": "string",
          "unit": "bytes"
        },
        {
          "alias": "可用空间",
          "colorMode": "value",
          "colors": [
            "rgba(245, 54, 54, 0.9)",
            "rgba(237, 129, 40, 0.89)",
            "rgba(50, 172, 45, 0.97)"
          ],
          "dateFormat": "YYYY-MM-DD HH:mm:ss",
          "decimals": 2,
          "mappingType": 1,
          "pattern": "Value #A",
          "thresholds": [
            "10000000000",
            "20000000000"
          ],
          "type": "number",
          "unit": "bytes"
        },
        {
          "alias": "使用率",
          "colorMode": "cell",
          "colors": [
            "rgba(50, 172, 45, 0.97)",
            "rgba(237, 129, 40, 0.89)",
            "rgba(245, 54, 54, 0.9)"
          ],
          "dateFormat": "YYYY-MM-DD HH:mm:ss",
          "decimals": 2,
          "mappingType": 1,
          "pattern": "Value #B",
          "thresholds": [
            "70",
            "90"
          ],
          "type": "number",
          "unit": "percentunit"
        },
        {
          "alias": "总空间",
          "colorMode": null,
          "colors": [
            "rgba(245, 54, 54, 0.9)",
            "rgba(237, 129, 40, 0.89)",
            "rgba(50, 172, 45, 0.97)"
          ],
          "dateFormat": "YYYY-MM-DD HH:mm:ss",
          "decimals": 1,
          "link": false,
          "mappingType": 1,
          "pattern": "Value #C",
          "thresholds": [],
          "type": "number",
          "unit": "bytes"
        },
        {
          "alias": "文件系统",
          "colorMode": null,
          "colors": [
            "rgba(245, 54, 54, 0.9)",
            "rgba(237, 129, 40, 0.89)",
            "rgba(50, 172, 45, 0.97)"
          ],
          "dateFormat": "YYYY-MM-DD HH:mm:ss",
          "decimals": 2,
          "link": false,
          "mappingType": 1,
          "pattern": "fstype",
          "thresholds": [],
          "type": "number",
          "unit": "short"
        },
        {
          "alias": "",
          "colorMode": null,
          "colors": [
            "rgba(245, 54, 54, 0.9)",
            "rgba(237, 129, 40, 0.89)",
            "rgba(50, 172, 45, 0.97)"
          ],
          "decimals": 2,
          "pattern": "/.*/",
          "preserveFormat": true,
          "sanitize": false,
          "thresholds": [],
          "type": "hidden",
          "unit": "short"
        }
      ],
      "targets": [
        {
          "expr": "node_filesystem_size_bytes{instance=~'$node',fstype=~\"ext4|xfs\"}",
          "format": "table",
          "hide": true,
          "instant": true,
          "intervalFactor": 1,
          "legendFormat": "",
          "refId": "C"
        },
        {
          "expr": "node_filesystem_avail_bytes {instance=~'$node',fstype=~\"ext4|xfs\"}",
          "format": "table",
          "hide": false,
          "instant": true,
          "interval": "10s",
          "intervalFactor": 1,
          "legendFormat": "",
          "refId": "A"
        },
        {
          "expr": "1-(node_filesystem_free_bytes{instance=~'$node',fstype=~\"ext4|xfs\"} / node_filesystem_size_bytes{instance=~'$node',fstype=~\"ext4|xfs\"})",
          "format": "table",
          "hide": false,
          "instant": true,
          "intervalFactor": 1,
          "legendFormat": "",
          "refId": "B"
        },
        {
          "expr": "",
          "format": "table",
          "interval": "10s",
          "intervalFactor": 1,
          "legendFormat": "",
          "refId": "D"
        }
      ],
      "title": "各分区可用空间",
      "transform": "table",
      "transparent": false,
      "type": "table"
    },
    {
      "aliasColors": {
        "Idle - Waiting for something to happen": "#052B51",
        "guest": "#9AC48A",
        "idle": "#052B51",
        "iowait": "#EAB839",
        "irq": "#BF1B00",
        "nice": "#C15C17",
        "sdb_每秒I/O操作%": "#d683ce",
        "softirq": "#E24D42",
        "steal": "#FCE2DE",
        "system": "#508642",
        "user": "#5195CE",
        "磁盘花费在I/O操作占比": "#ba43a9"
      },
      "bars": false,
      "dashLength": 10,
      "dashes": false,
      "datasource": "${DS_PROMETHEUS_111}",
      "decimals": 2,
      "description": "node_disk_io_time_seconds_total：\n磁盘花费在输入/输出操作上的毫秒数。该值为累加值。（Milliseconds Spent Doing I/Os）\n\nirate(node_disk_io_time_seconds_total[1m])：\n计算每秒的速率：(last值-last前一个值)/时间戳差值，即：1秒钟内磁盘花费在I/O操作的时间占比。",
      "fill": 1,
      "gridPos": {
        "h": 7,
        "w": 16,
        "x": 0,
        "y": 12
      },
      "id": 7,
      "legend": {
        "alignAsTable": true,
        "avg": true,
        "current": true,
        "hideEmpty": true,
        "hideZero": true,
        "max": true,
        "min": false,
        "rightSide": true,
        "show": true,
        "sideWidth": null,
        "sort": null,
        "sortDesc": null,
        "total": false,
        "values": true
      },
      "lines": true,
      "linewidth": 1,
      "links": [],
      "minSpan": 4,
      "nullPointMode": "null",
      "percentage": false,
      "pointradius": 5,
      "points": false,
      "renderer": "flot",
      "repeat": null,
      "seriesOverrides": [],
      "spaceLength": 10,
      "stack": false,
      "steppedLine": false,
      "targets": [
        {
          "expr": "avg(irate(node_cpu_seconds_total{instance=~\"$node\",mode=\"system\"}[1m]))",
          "format": "time_series",
          "interval": "",
          "intervalFactor": 2,
          "legendFormat": "System",
          "refId": "A",
          "step": 20
        },
        {
          "expr": "avg(irate(node_cpu_seconds_total{instance=~\"$node\",mode=\"user\"}[1m]))",
          "format": "time_series",
          "intervalFactor": 2,
          "legendFormat": "User",
          "refId": "B",
          "step": 240
        },
        {
          "expr": "avg(irate(node_cpu_seconds_total{instance=~\"$node\",mode=\"idle\"}[1m]))",
          "format": "time_series",
          "hide": false,
          "intervalFactor": 2,
          "legendFormat": "Idle",
          "refId": "F",
          "step": 240
        },
        {
          "expr": "avg(irate(node_cpu_seconds_total{instance=~\"$node\",mode=\"iowait\"}[1m]))",
          "format": "time_series",
          "intervalFactor": 2,
          "legendFormat": "Iowait",
          "refId": "D",
          "step": 240
        },
        {
          "expr": "irate(node_disk_io_time_seconds_total{instance=~\"$node\"}[1m])",
          "format": "time_series",
          "intervalFactor": 1,
          "legendFormat": "{{device}}_每秒I/O操作%",
          "refId": "C"
        }
      ],
      "thresholds": [],
      "timeFrom": null,
      "timeShift": null,
      "title": "CPU使用率、磁盘每秒的I/O操作耗费时间（%）",
      "tooltip": {
        "shared": true,
        "sort": 0,
        "value_type": "individual"
      },
      "transparent": false,
      "type": "graph",
      "xaxis": {
        "buckets": null,
        "mode": "time",
        "name": null,
        "show": true,
        "values": []
      },
      "yaxes": [
        {
          "decimals": null,
          "format": "percentunit",
          "label": "",
          "logBase": 1,
          "max": "1",
          "min": null,
          "show": true
        },
        {
          "format": "short",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": null,
          "show": false
        }
      ],
      "yaxis": {
        "align": false,
        "alignLevel": null
      }
    },
    {
      "aliasColors": {
        "内存_Avaliable": "#6ED0E0",
        "内存_Cached": "#EF843C",
        "内存_Free": "#629E51",
        "内存_Total": "#6d1f62",
        "内存_Used": "#eab839"
      },
      "bars": false,
      "dashLength": 10,
      "dashes": false,
      "datasource": "${DS_PROMETHEUS_111}",
      "decimals": 2,
      "fill": 1,
      "gridPos": {
        "h": 7,
        "w": 8,
        "x": 16,
        "y": 12
      },
      "height": "300",
      "id": 156,
      "legend": {
        "alignAsTable": false,
        "avg": false,
        "current": true,
        "max": false,
        "min": false,
        "rightSide": false,
        "show": true,
        "total": false,
        "values": true
      },
      "lines": true,
      "linewidth": 2,
      "links": [],
      "nullPointMode": "null",
      "percentage": false,
      "pointradius": 5,
      "points": false,
      "renderer": "flot",
      "seriesOverrides": [],
      "spaceLength": 10,
      "stack": false,
      "steppedLine": false,
      "targets": [
        {
          "expr": "node_memory_MemTotal_bytes{instance=~\"$node\"}",
          "format": "time_series",
          "hide": false,
          "intervalFactor": 2,
          "legendFormat": "总内存",
          "refId": "A",
          "step": 4
        },
        {
          "expr": "node_memory_MemTotal_bytes{instance=~\"$node\"} - (node_memory_Cached_bytes{instance=~\"$node\"} + node_memory_Buffers_bytes{instance=~\"$node\"} + node_memory_MemFree_bytes{instance=~\"$node\"})",
          "format": "time_series",
          "hide": false,
          "intervalFactor": 2,
          "legendFormat": "已用",
          "refId": "B",
          "step": 4
        },
        {
          "expr": "node_memory_MemFree_bytes{instance=~\"$node\"}",
          "format": "time_series",
          "hide": true,
          "intervalFactor": 2,
          "legendFormat": "内存_Free",
          "refId": "C",
          "step": 4
        },
        {
          "expr": "node_memory_Buffers_bytes{instance=~\"$node\"}",
          "format": "time_series",
          "hide": true,
          "intervalFactor": 2,
          "legendFormat": "内存_Buffers",
          "refId": "D",
          "step": 4
        },
        {
          "expr": "node_memory_Cached_bytes{instance=~\"$node\"}",
          "format": "time_series",
          "hide": true,
          "intervalFactor": 2,
          "legendFormat": "内存_Cached",
          "refId": "E",
          "step": 4
        },
        {
          "expr": "node_memory_MemAvailable_bytes{instance=~\"$node\"}",
          "format": "time_series",
          "hide": false,
          "interval": "",
          "intervalFactor": 2,
          "legendFormat": "可用",
          "refId": "F",
          "step": 4
        }
      ],
      "thresholds": [],
      "timeFrom": null,
      "timeShift": null,
      "title": "内存信息",
      "tooltip": {
        "shared": true,
        "sort": 0,
        "value_type": "individual"
      },
      "type": "graph",
      "xaxis": {
        "buckets": null,
        "mode": "time",
        "name": null,
        "show": true,
        "values": []
      },
      "yaxes": [
        {
          "format": "decbytes",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": "0",
          "show": true
        },
        {
          "format": "short",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        }
      ],
      "yaxis": {
        "align": false,
        "alignLevel": null
      }
    },
    {
      "aliasColors": {
        "vda_write": "#6ED0E0"
      },
      "bars": true,
      "dashLength": 10,
      "dashes": false,
      "datasource": "${DS_PROMETHEUS_111}",
      "description": "Reads completed: 每个磁盘分区每秒读完成次数\n\nWrites completed: 每个磁盘分区每秒写完成次数\n\nIO now 每个磁盘分区每秒正在处理的输入/输出请求数",
      "fill": 2,
      "gridPos": {
        "h": 8,
        "w": 8,
        "x": 0,
        "y": 19
      },
      "height": "300",
      "id": 161,
      "legend": {
        "alignAsTable": false,
        "avg": false,
        "current": true,
        "hideEmpty": true,
        "hideZero": true,
        "max": true,
        "min": false,
        "show": true,
        "total": false,
        "values": true
      },
      "lines": false,
      "linewidth": 1,
      "links": [],
      "nullPointMode": "null",
      "percentage": false,
      "pointradius": 5,
      "points": false,
      "renderer": "flot",
      "seriesOverrides": [
        {
          "alias": "/.*_读取$/",
          "transform": "negative-Y"
        }
      ],
      "spaceLength": 10,
      "stack": false,
      "steppedLine": false,
      "targets": [
        {
          "expr": "irate(node_disk_reads_completed_total{instance=~\"$node\"}[1m])",
          "format": "time_series",
          "hide": false,
          "interval": "",
          "intervalFactor": 2,
          "legendFormat": "{{device}}_读取",
          "refId": "A",
          "step": 10
        },
        {
          "expr": "irate(node_disk_writes_completed_total{instance=~\"$node\"}[1m])",
          "format": "time_series",
          "hide": false,
          "intervalFactor": 2,
          "legendFormat": "{{device}}_写入",
          "refId": "B",
          "step": 10
        },
        {
          "expr": "node_disk_io_now{instance=~\"$node\"}",
          "format": "time_series",
          "hide": true,
          "interval": "",
          "intervalFactor": 1,
          "legendFormat": "{{device}}",
          "refId": "C"
        }
      ],
      "thresholds": [],
      "timeFrom": null,
      "timeShift": null,
      "title": "磁盘读写速率（IOPS）",
      "tooltip": {
        "shared": true,
        "sort": 0,
        "value_type": "individual"
      },
      "type": "graph",
      "xaxis": {
        "buckets": null,
        "mode": "time",
        "name": null,
        "show": true,
        "values": []
      },
      "yaxes": [
        {
          "decimals": null,
          "format": "iops",
          "label": "读取（-）/写入（+）I/O ops/sec",
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        },
        {
          "format": "short",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        }
      ],
      "yaxis": {
        "align": false,
        "alignLevel": null
      }
    },
    {
      "aliasColors": {
        "vda_write": "#6ED0E0"
      },
      "bars": true,
      "dashLength": 10,
      "dashes": false,
      "datasource": "${DS_PROMETHEUS_111}",
      "description": "Read bytes 每个磁盘分区每秒读取的比特数\nWritten bytes 每个磁盘分区每秒写入的比特数",
      "fill": 2,
      "gridPos": {
        "h": 8,
        "w": 8,
        "x": 8,
        "y": 19
      },
      "height": "300",
      "id": 168,
      "legend": {
        "alignAsTable": false,
        "avg": false,
        "current": true,
        "hideEmpty": true,
        "hideZero": true,
        "max": true,
        "min": false,
        "show": true,
        "total": false,
        "values": true
      },
      "lines": false,
      "linewidth": 1,
      "links": [],
      "nullPointMode": "null",
      "percentage": false,
      "pointradius": 5,
      "points": false,
      "renderer": "flot",
      "seriesOverrides": [
        {
          "alias": "/.*_读取$/",
          "transform": "negative-Y"
        }
      ],
      "spaceLength": 10,
      "stack": false,
      "steppedLine": false,
      "targets": [
        {
          "expr": "irate(node_disk_read_bytes_total{instance=~\"$node\"}[1m])",
          "format": "time_series",
          "interval": "",
          "intervalFactor": 2,
          "legendFormat": "{{device}}_读取",
          "refId": "A",
          "step": 10
        },
        {
          "expr": "irate(node_disk_written_bytes_total{instance=~\"$node\"}[1m])",
          "format": "time_series",
          "hide": false,
          "intervalFactor": 2,
          "legendFormat": "{{device}}_写入",
          "refId": "B",
          "step": 10
        }
      ],
      "thresholds": [],
      "timeFrom": null,
      "timeShift": null,
      "title": "磁盘读写容量大小",
      "tooltip": {
        "shared": true,
        "sort": 0,
        "value_type": "individual"
      },
      "type": "graph",
      "xaxis": {
        "buckets": null,
        "mode": "time",
        "name": null,
        "show": true,
        "values": []
      },
      "yaxes": [
        {
          "decimals": null,
          "format": "Bps",
          "label": "读取（-）/写入（+）",
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        },
        {
          "format": "short",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": null,
          "show": false
        }
      ],
      "yaxis": {
        "align": false,
        "alignLevel": null
      }
    },
    {
      "aliasColors": {
        "vda": "#6ED0E0"
      },
      "bars": false,
      "dashLength": 10,
      "dashes": false,
      "datasource": "${DS_PROMETHEUS_111}",
      "description": "Read time ms 每个磁盘分区读操作花费的秒数\n\nWrite time ms 每个磁盘分区写操作花费的秒数\n\nIO time ms 每个磁盘分区输入/输出操作花费的秒数\n\nIO time weighted 每个磁盘分区输入/输出操作花费的加权秒数",
      "fill": 3,
      "gridPos": {
        "h": 8,
        "w": 8,
        "x": 16,
        "y": 19
      },
      "height": "300",
      "id": 160,
      "legend": {
        "alignAsTable": false,
        "avg": false,
        "current": true,
        "hideEmpty": true,
        "hideZero": true,
        "max": true,
        "min": false,
        "show": true,
        "total": false,
        "values": true
      },
      "lines": true,
      "linewidth": 1,
      "links": [],
      "nullPointMode": "null",
      "percentage": false,
      "pointradius": 5,
      "points": false,
      "renderer": "flot",
      "seriesOverrides": [
        {
          "alias": "/,*_读取$/",
          "transform": "negative-Y"
        }
      ],
      "spaceLength": 10,
      "stack": false,
      "steppedLine": false,
      "targets": [
        {
          "expr": "irate(node_disk_io_time_seconds_total{instance=~\"$node\"}[1m])",
          "format": "time_series",
          "hide": true,
          "interval": "",
          "intervalFactor": 2,
          "legendFormat": "{{device}}",
          "refId": "A",
          "step": 10
        },
        {
          "expr": "irate(node_disk_io_time_weighted_seconds_total{instance=~\"$node\"}[1m])",
          "format": "time_series",
          "hide": true,
          "intervalFactor": 1,
          "legendFormat": "{{device}}_加权",
          "refId": "D"
        },
        {
          "expr": "irate(node_disk_read_time_seconds_total{instance=~\"$node\"}[1m])",
          "format": "time_series",
          "hide": false,
          "interval": "",
          "intervalFactor": 1,
          "legendFormat": "{{device}}_读取",
          "refId": "B"
        },
        {
          "expr": "irate(node_disk_write_time_seconds_total{instance=~\"$node\"}[1m])",
          "format": "time_series",
          "hide": false,
          "intervalFactor": 1,
          "legendFormat": "{{device}}_写入",
          "refId": "C"
        }
      ],
      "thresholds": [],
      "timeFrom": null,
      "timeShift": null,
      "title": "磁盘IO读写时间",
      "tooltip": {
        "shared": true,
        "sort": 0,
        "value_type": "individual"
      },
      "type": "graph",
      "xaxis": {
        "buckets": null,
        "mode": "time",
        "name": null,
        "show": true,
        "values": []
      },
      "yaxes": [
        {
          "format": "s",
          "label": "读取（-）/写入（+）",
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        },
        {
          "format": "short",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": null,
          "show": false
        }
      ],
      "yaxis": {
        "align": false,
        "alignLevel": null
      }
    },
    {
      "aliasColors": {},
      "bars": false,
      "dashLength": 10,
      "dashes": false,
      "datasource": "${DS_PROMETHEUS_111}",
      "fill": 1,
      "gridPos": {
        "h": 8,
        "w": 12,
        "x": 0,
        "y": 27
      },
      "height": "300",
      "id": 157,
      "legend": {
        "alignAsTable": false,
        "avg": false,
        "current": true,
        "hideEmpty": true,
        "hideZero": true,
        "max": false,
        "min": false,
        "show": true,
        "total": false,
        "values": true
      },
      "lines": true,
      "linewidth": 2,
      "links": [],
      "nullPointMode": "null",
      "percentage": false,
      "pointradius": 5,
      "points": false,
      "renderer": "flot",
      "seriesOverrides": [
        {
          "alias": "/.*_out上传$/",
          "transform": "negative-Y"
        }
      ],
      "spaceLength": 10,
      "stack": false,
      "steppedLine": false,
      "targets": [
        {
          "expr": "irate(node_network_receive_bytes_total{instance=~'$node',device!~'tap.*'}[5m])*8",
          "format": "time_series",
          "intervalFactor": 2,
          "legendFormat": "{{device}}_in下载",
          "refId": "A",
          "step": 4
        },
        {
          "expr": "irate(node_network_transmit_bytes_total{instance=~'$node',device!~'tap.*'}[5m])*8",
          "format": "time_series",
          "intervalFactor": 2,
          "legendFormat": "{{device}}_out上传",
          "refId": "B",
          "step": 4
        }
      ],
      "thresholds": [],
      "timeFrom": null,
      "timeShift": null,
      "title": "网络流量",
      "tooltip": {
        "shared": true,
        "sort": 0,
        "value_type": "individual"
      },
      "type": "graph",
      "xaxis": {
        "buckets": null,
        "mode": "time",
        "name": null,
        "show": true,
        "values": []
      },
      "yaxes": [
        {
          "format": "bps",
          "label": "上传（-）/下载（+）",
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        },
        {
          "format": "short",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": null,
          "show": false
        }
      ],
      "yaxis": {
        "align": false,
        "alignLevel": null
      }
    },
    {
      "aliasColors": {
        "TCP": "#6ED0E0"
      },
      "bars": false,
      "dashLength": 10,
      "dashes": false,
      "datasource": "${DS_PROMETHEUS_111}",
      "description": "CurrEstab - 当前状态为 ESTABLISHED 或 CLOSE-WAIT 的 TCP 连接数\n\nActiveOpens - 已从 CLOSED 状态直接转换到 SYN-SENT 状态的 TCP 平均连接数(1分钟内)\n\nPassiveOpens - 已从 LISTEN 状态直接转换到 SYN-RCVD 状态的 TCP 平均连接数(1分钟内)\n\nTCP_alloc - 已分配（已建立、已申请到sk_buff）的TCP套接字数量\n\nTCP_inuse - 正在使用（正在侦听）的TCP套接字数量\n\nTCP_tw - 等待关闭的TCP连接数",
      "fill": 0,
      "gridPos": {
        "h": 8,
        "w": 12,
        "x": 12,
        "y": 27
      },
      "height": "300",
      "id": 158,
      "legend": {
        "alignAsTable": true,
        "avg": false,
        "current": true,
        "max": true,
        "min": false,
        "rightSide": true,
        "show": true,
        "total": false,
        "values": true
      },
      "lines": true,
      "linewidth": 1,
      "links": [],
      "nullPointMode": "null",
      "percentage": false,
      "pointradius": 5,
      "points": false,
      "renderer": "flot",
      "seriesOverrides": [],
      "spaceLength": 10,
      "stack": false,
      "steppedLine": false,
      "targets": [
        {
          "expr": "node_netstat_Tcp_CurrEstab{instance=~'$node'}",
          "format": "time_series",
          "hide": false,
          "interval": "10s",
          "intervalFactor": 1,
          "legendFormat": "ESTABLISHED",
          "refId": "A",
          "step": 20
        },
        {
          "expr": "node_sockstat_TCP_tw{instance=~'$node'}",
          "format": "time_series",
          "intervalFactor": 1,
          "legendFormat": "TCP_tw",
          "refId": "D"
        },
        {
          "expr": "irate(node_netstat_Tcp_ActiveOpens{instance=~'$node'}[1m])",
          "format": "time_series",
          "hide": false,
          "intervalFactor": 1,
          "legendFormat": "ActiveOpens",
          "refId": "B"
        },
        {
          "expr": "irate(node_netstat_Tcp_PassiveOpens{instance=~'$node'}[1m])",
          "format": "time_series",
          "intervalFactor": 1,
          "legendFormat": "PassiveOpens",
          "refId": "C"
        },
        {
          "expr": "node_sockstat_TCP_alloc{instance=~'$node'}",
          "format": "time_series",
          "intervalFactor": 1,
          "legendFormat": "TCP_alloc",
          "refId": "E"
        },
        {
          "expr": "node_sockstat_TCP_inuse{instance=~'$node'}",
          "format": "time_series",
          "intervalFactor": 1,
          "legendFormat": "TCP_inuse",
          "refId": "F"
        }
      ],
      "thresholds": [],
      "timeFrom": null,
      "timeShift": null,
      "title": "TCP 连接情况",
      "tooltip": {
        "shared": true,
        "sort": 0,
        "value_type": "individual"
      },
      "transparent": false,
      "type": "graph",
      "xaxis": {
        "buckets": null,
        "mode": "time",
        "name": null,
        "show": true,
        "values": []
      },
      "yaxes": [
        {
          "format": "short",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        },
        {
          "format": "short",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        }
      ],
      "yaxis": {
        "align": false,
        "alignLevel": null
      }
    },
    {
      "aliasColors": {},
      "bars": false,
      "dashLength": 10,
      "dashes": false,
      "datasource": "${DS_PROMETHEUS_111}",
      "fill": 0,
      "gridPos": {
        "h": 10,
        "w": 24,
        "x": 0,
        "y": 35
      },
      "id": 169,
      "legend": {
        "alignAsTable": true,
        "avg": true,
        "current": true,
        "hideEmpty": true,
        "hideZero": true,
        "max": true,
        "min": false,
        "rightSide": true,
        "show": true,
        "total": false,
        "values": true
      },
      "lines": true,
      "linewidth": 1,
      "links": [],
      "nullPointMode": "null as zero",
      "percentage": false,
      "pointradius": 0.5,
      "points": false,
      "renderer": "flot",
      "seriesOverrides": [],
      "spaceLength": 10,
      "stack": false,
      "steppedLine": false,
      "targets": [
        {
          "expr": "node_hwmon_temp_celsius{instance=\"$node\"}",
          "format": "time_series",
          "intervalFactor": 1,
          "legendFormat": "{{chip}} {{sensor}}",
          "refId": "A"
        }
      ],
      "thresholds": [],
      "timeFrom": null,
      "timeShift": null,
      "title": "硬件温度",
      "tooltip": {
        "shared": true,
        "sort": 0,
        "value_type": "individual"
      },
      "type": "graph",
      "xaxis": {
        "buckets": null,
        "mode": "time",
        "name": null,
        "show": true,
        "values": []
      },
      "yaxes": [
        {
          "format": "celsius",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        },
        {
          "format": "short",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        }
      ],
      "yaxis": {
        "align": false,
        "alignLevel": null
      }
    }
  ],
  "refresh": false,
  "schemaVersion": 16,
  "style": "dark",
  "tags": [
    "StarsL",
    "Prometheus"
  ],
  "templating": {
    "list": [
      {
        "auto": true,
        "auto_count": 30,
        "auto_min": "10s",
        "current": {
          "text": "1m",
          "value": "1m"
        },
        "hide": 0,
        "label": "interval",
        "name": "interval",
        "options": [
          {
            "selected": false,
            "text": "auto",
            "value": "$__auto_interval_interval"
          },
          {
            "selected": true,
            "text": "1m",
            "value": "1m"
          },
          {
            "selected": false,
            "text": "10m",
            "value": "10m"
          },
          {
            "selected": false,
            "text": "30m",
            "value": "30m"
          },
          {
            "selected": false,
            "text": "1h",
            "value": "1h"
          },
          {
            "selected": false,
            "text": "6h",
            "value": "6h"
          },
          {
            "selected": false,
            "text": "12h",
            "value": "12h"
          },
          {
            "selected": false,
            "text": "1d",
            "value": "1d"
          },
          {
            "selected": false,
            "text": "7d",
            "value": "7d"
          },
          {
            "selected": false,
            "text": "14d",
            "value": "14d"
          },
          {
            "selected": false,
            "text": "30d",
            "value": "30d"
          }
        ],
        "query": "1m,10m,30m,1h,6h,12h,1d,7d,14d,30d",
        "refresh": 2,
        "skipUrlSync": false,
        "type": "interval"
      },
      {
        "allFormat": "glob",
        "allValue": null,
        "current": {},
        "datasource": "${DS_PROMETHEUS_111}",
        "hide": 0,
        "includeAll": false,
        "label": "环境",
        "multi": false,
        "multiFormat": "regex values",
        "name": "env",
        "options": [],
        "query": "label_values(node_exporter_build_info,env)",
        "refresh": 1,
        "regex": "",
        "skipUrlSync": false,
        "sort": 1,
        "tagValuesQuery": "",
        "tags": [],
        "tagsQuery": "",
        "type": "query",
        "useTags": false
      },
      {
        "allFormat": "glob",
        "allValue": null,
        "current": {},
        "datasource": "${DS_PROMETHEUS_111}",
        "hide": 0,
        "includeAll": false,
        "label": "主机名",
        "multi": false,
        "multiFormat": "regex values",
        "name": "name",
        "options": [],
        "query": "label_values(node_exporter_build_info{env='$env'},name)",
        "refresh": 1,
        "regex": "",
        "skipUrlSync": false,
        "sort": 1,
        "tagValuesQuery": "",
        "tags": [],
        "tagsQuery": "",
        "type": "query",
        "useTags": false
      },
      {
        "allFormat": "glob",
        "allValue": null,
        "current": {},
        "datasource": "${DS_PROMETHEUS_111}",
        "hide": 0,
        "includeAll": false,
        "label": "节点",
        "multi": false,
        "multiFormat": "regex values",
        "name": "node",
        "options": [],
        "query": "label_values(node_exporter_build_info{name='$name'},instance)",
        "refresh": 1,
        "regex": "",
        "skipUrlSync": false,
        "sort": 1,
        "tagValuesQuery": "",
        "tags": [],
        "tagsQuery": "",
        "type": "query",
        "useTags": false
      },
      {
        "allValue": null,
        "current": {},
        "datasource": "${DS_PROMETHEUS_111}",
        "hide": 2,
        "includeAll": false,
        "label": "",
        "multi": false,
        "name": "maxmount",
        "options": [],
        "query": "query_result(topk(1,sort_desc (max(node_filesystem_size_bytes{instance=~'$node',fstype=~\"ext4|xfs\"}) by (mountpoint))))",
        "refresh": 1,
        "regex": "/.*\\\"(.*)\\\".*/",
        "skipUrlSync": false,
        "sort": 0,
        "tagValuesQuery": "",
        "tags": [],
        "tagsQuery": "",
        "type": "query",
        "useTags": false
      }
    ]
  },
  "time": {
    "from": "now-24h",
    "to": "now"
  },
  "timepicker": {
    "now": true,
    "refresh_intervals": [
      "5s",
      "10s",
      "30s",
      "1m",
      "5m",
      "15m",
      "30m",
      "1h",
      "2h",
      "1d"
    ],
    "time_options": [
      "5m",
      "15m",
      "1h",
      "6h",
      "12h",
      "24h",
      "2d",
      "7d",
      "30d"
    ]
  },
  "timezone": "browser",
  "title": "1 Node Exporter 0.16+ 监控展示看板 for Prometheus",
  "uid": "9CWBz0bik",
  "version": 28
}
```

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6ekpmpbj60k30buq3b02.jpg)

9.新建manage



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6enlwjej60xc0g2jsj02.jpg)

10.Import 导入上述json文件



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6eov9ihj60xc0jbq4e02.jpg)

11.设置数据源为promethues



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh6eraoh4j60xc0ddta002.jpg)

12.优化后展示界面



如果到这里还是没有显示，那么说明是缺少插件，我们可以用grafana-cli安装以下插件：



```undefined
sudo grafana-cli plugins install grafana-piechart-panel
```

重启下grafana服务即可。

# 五、prometheus小结：

目前监控工具很多，包括当下流行的Zabbix等，每种监控工具都有最适合的场景，prometheus更偏向于于Docker容器的监控，当然具体使用哪种工具还需要视情况而定。
 参考：
 [Grafana](https://links.jianshu.com/go?to=https%3A%2F%2Fgrafana.com)
 [监控神器普罗米修斯Prometheus安装配置](https://links.jianshu.com/go?to=https%3A%2F%2Fblog.csdn.net%2Fywd1992%2Farticle%2Fdetails%2F85989259)



2人点赞



[深度剖析Devops系列]()





作者：小E的私房菜
链接：https://www.jianshu.com/p/36635a44edd1
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---



