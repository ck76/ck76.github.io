[TOC]

# **Docker-compose介绍**

前几篇文章和小伙伴们，分享了使用`Dockerfile`来构建镜像，使用`docker run`等命令来手动启动镜像、`docker stop`停止镜像、`docker kill`杀死镜像进程。这种情况只适用于镜像服务不多的情况。然而，现实情况是我们可能同时启停操作成百上千的服务，而且还要在启动之前分析各个服务之间的前后依赖关系，如果此时还是使用手动的方式来操作显然是不现实的。于是`docker-compose`应运而生。接下来我们就来看看`docker-compose`。

## **什么是`docker-compose`**

`Compose` 是用于定义和运行多容器 `Docker`应用程序的工具。通过`Compose`，我们可以使用`YML` 文件来配置应用程序需要的所有服务。然后，使用一个命令，就可以从 `YML` 文件配置中创建并启动所有服务。

简单来说，就是在一个叫做`YML`文件中，将你的镜像服务按照一定的规则，进行定义、排列、配置。然后使用`compose`提供的命令，一键启动整个服务群的工具。

## **安装**

### 执行如下命令进行安装`docker-compose`，此命令会根据你的系统匹配合适的`docker-compose`

```
curl -L "https://github.com/docker/compose/releases/download/1.26.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
```

### 给安装的脚本赋予执行权限

```
chmod +x /usr/local/bin/docker-compose 
```

### 检查安装结果

```
docker-compose --version
```

终端输出如下内容表示安装成功

```
docker-compose version 1.26.2, build eefe0d31
```

> ❝
>
> MacOs和windows的小伙伴不用单独安装`Compose`,因为安装`docker`桌面版的时候已经自带了`docker-compose`
>
> ❞

## **`docker-compose`命令**

`docker-compose`和`docker`一样也有自己的一套命令，下面我们就经常用到的进行说明。

- docker-compose up`: 启动所有容器（在docker-compose中定义）

> ❝
>
> 参数说明 `docker-compose up -d`:在后台启动所有容器
>
>  `docker-compose -f docker-compose模板 up -d`:指定 `docker-compose` 并在后台启动
>
> ❞

- `docker-compose ps`: 查看项目中运行着的容器
- `docker-compose stop` : 停止正在运行的容器
- `docker-compose start`：启动`stop`了的容器
- `docker-compose down`: 停止并删除容器、网络、卷、镜像

> ❝
>
> 参数说明 `docker-compose down`-v: 删除已经在`compose`文件中定义的和匿名的附在容器上的数据卷
>
> ❞

- `docker-compose logs`: 查看服务容器输出的日志
- `docker-compose restart`: 重启项目中的服务
- `docker-compose rm`： 删除所有（停止状态的）服务容器

> ❝
>
> 强调：首先需要执行`docker-compose stop`,将服务停止后，在执行该命令
>
> ❞

- `docker-compose config`: 查看项目配置
- `docker-compose version`: 打印版本信息

## **`compose`版本**

下图是在`docker`官网中的一张`compose`和`docker`版本对应图,大家可以看到不同的`docker`引擎的版本对应不同的`Compose`版本，那么这个有什么用呢？这个在我们编写`docker-compose.yml`文件大有用处。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvgfsmfcqqj31090u0myg.jpg)

### 何为服务编排

`Compose`允许用户通过一个`docker-compose.yml`模板文件（`YAML` 格式）来定义一组相关联的应用容器为一个项目（`project`）,即将不同服务通过一个模本文件组装为一个项目。

`Compose`模板文件是一个定义服务、网络和卷的`YAML`文件

`Docker-Compose`标准模板文件应该包含`version`、`services`、`networks`三大部分

## **编写模板文件**

上文提到，一个标准的`docker-compose`文件，应该包含`version`、`services`、`networks`三大部分，下来看一个`docker-compose.yml`样例。下文样例是借助极客时间专栏es作者阮一鸣老师的模板，我稍加改造后的。下篇文章我们会编写自己的`docker-compose.yml`文件，编排我们自己的服务。

```yaml
version: '3.8'
services:
  cerebro:
    image: lmenezes/cerebro:0.8.3
    container_name: cerebro
    ports:
      - "9000:9000"
    command:
      - -Dhosts.0.host=http://elasticsearch:9200
    networks:
      - es7net
  kibana:
    image: docker.elastic.co/kibana/kibana:7.8.0
    container_name: kibana7
    environment:
      - I18N_LOCALE=zh-CN
      - XPACK_GRAPH_ENABLED=true
      - TIMELION_ENABLED=true
      - XPACK_MONITORING_COLLECTION_ENABLED="true"
    ports:
      - "5601:5601"
    networks:
      - es7net
  elasticsearch:
    image: docker.elastic.co/elasticsearch/elasticsearch:7.8.0
    container_name: es7_01
    environment:
      - cluster.name=triumphxx
      - node.name=es7_01
      - bootstrap.memory_lock=true
      - "ES_JAVA_OPTS=-Xms512m -Xmx512m"
      - discovery.seed_hosts=es7_01,es7_02
      - cluster.initial_master_nodes=es7_01,es7_02
    ulimits:
      memlock:
        soft: -1
        hard: -1
    volumes:
      - es7data1:/usr/share/elasticsearch/data
    ports:
      - 9200:9200
    networks:
      - es7net
  elasticsearch2:
    image: docker.elastic.co/elasticsearch/elasticsearch:7.8.0
    container_name: es7_02
    environment:
      - cluster.name=triumphxx
      - node.name=es7_02
      - bootstrap.memory_lock=true
      - "ES_JAVA_OPTS=-Xms512m -Xmx512m"
      - discovery.seed_hosts=es7_01,es7_02
      - cluster.initial_master_nodes=es7_01,es7_02
    ulimits:
      memlock:
        soft: -1
        hard: -1
    volumes:
      - es7data2:/usr/share/elasticsearch/data
    networks:
      - es7net
      
volumes:
  es7data1:
    driver: local
  es7data2:
    driver: local

networks:
  es7net:
    driver: bridge
```

> ❝
>
> 可以看到，这个模板文件的主体部分包含：`version`: 兼容`docker`版本的`Compose`文件格式版本， `services`：需要编排的服务 `volumes`：数据卷挂载路径设置 `networks`：网络定义。
>
> `service`下首先需要指定，服务的名称，服务所用到的镜像以及定义启动容器的名称、映射端口、容器启动后执行的命令和连接的网络。
>
> `elasticsearch`服务配置稍微复杂一些，还设置了运行环境，包括：集群名称、节点名称、`jvm`参数、以及`ES`集群配置，限制设置，这部分我们会在在讨论`es`的文章中专题讨论，小伙伴们，没有明白也不用着急。
>
> `volumes`定义了俩个数据卷挂载路径，分别给俩个`es`服务使用
>
> `networks`定义网络模式为`bridge`名称为`es7net`的网络，启动的四个服务都会连接上这个网络
>
> ❞

## **启动项目**

`docker-compose.yml`文件编写完成后，我们就可以执行进入到`yml`文件目录下执行`docker-compose up`目命令来启动项目。

> ❝
>
> 在启动的过程中，`Compose`会检查本地是否有项目中使用的镜像，如果没有，会自动去`docker hub`拉取镜像。
>
> ❞

## **小结**

好啦，小伙伴们，本文我们介绍了，说明是`docker-compose`以及常用的命令，并且分析了一个`Compose`文件应该怎么书写。希望大家有所收获。下篇文章进行实战，将一个`SpringCloud`的服务进行编排。欢迎持续关注。



- https://www.cnblogs.com/triumph-wyp-com/p/13418504.html