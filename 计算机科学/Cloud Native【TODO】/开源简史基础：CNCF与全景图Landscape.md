[TOC]



这篇文章介绍一下在云原生领域极为重要的一个组织CNCF，以及CNCF目前最新的Landscape的全景图的详细信息。

基本概念
CNCF是什么
什么是云原生（Cloud Native）
TOC（Technical Oversight Committee）
CNCF路线图（Trail Map）
关于上述基本概念和路线图的详细说明可参看：

https://liumiaocn.blog.csdn.net/article/details/100653635
CNCF全景图（Landscape）
Trail Map只是对云原生应用做一个大体的梳理，而每个阶段所关联的服务商和产品等详细信息则需要从全景图Landscape中确认。CNCF有一个很大的愿景，而且包含的内容也越来越丰富，截止到2019年9月10号，全景图已经包含了云原生应用所关联的方方面面，详细如下所示。

![在这里插入图片描述](https://tva1.sinaimg.cn/large/008i3skNly1gsnql53ttoj31ax0u04de.jpg)

可以参看如下连接获得更加详细的内容和及时更新的内容：

- https://landscape.cncf.io/images/landscape.png

也可以通过如下链接交互式地确认各个部分的内容：

- https://landscape.cncf.io/

Landscape介绍
目前，在整个Landscape中按照功能或者模块分为了29个部分，分别归属于9种类型或者称为分层（包括生态中的培训等部分，不是特别严谨），包括当前CNCF在孵化中和已经毕业的22个项目，相关的详细信息如下所示：

序号	分层	功能模块	CNCF项目数量	CNCF项目详细
1	应用定义与开发	数据库	2	TiKV（孵化中）、Vitess（孵化中）
2	应用定义与开发	流与消息	1	NATS（孵化中）
3	应用定义与开发	应用定义与镜像构建	1	HELM
4	应用定义与开发	持续集成与持续交付	-	-
5	编排管理	编排调度	1	Kubernetes（已毕业）
6	编排管理	协同与服务发现	2	CoreDNS（已毕业）、etcd（孵化中）
7	编排管理	RPC远程调用	1	gRPC（孵化中）
8	编排管理	服务代理	1	envoy（已毕业）
9	编排管理	服务网关	-	-
10	编排管理	服务网格	1	LINKERD（孵化中）
11	运行环境	云原生存储	1	ROOK（孵化中）
12	运行环境	云原生网络	1	CNI（孵化中）
13	运行环境	容器运行环境	2	containerd（已毕业）、cri-o（孵化中）
14	配置管理	自动化与配置	-	-
15	配置管理	容器私库	1	HARBOR（孵化中）
16	配置管理	安全与合规	3	Notary（孵化中）、Open Policy Agent（孵化中）、TUF（孵化中）
17	配置管理	私钥管理	-	-
18	平台管理	通过认证的K8S平台提供商	-	-
19	平台管理	通过认证的K8S托管服务提供商	-	-
20	平台管理	通过认证的K8S安装提供商	-	-
21	平台管理	PaaS/容器服务提供商	-	-
22	监控分析	监控	1	Prometheus（已毕业）
23	监控分析	日志	1	fluentd（已毕业）
24	监控分析	调用链追踪	2	JAEGER（孵化中）、OPENTRACING（孵化中）
25	监控分析	混沌工程	-	-
26	无服务（Serverless）	注：另幅图说明	-	-
27	合作伙伴（Special）	-	-	-
28	合作伙伴（Special）	-	-	-
29	成员构成	注：另幅图说明	-	-
分层解读
对于上述Landscape的各分层进行简单解读如下：



1：应用定义与开发
解读：
此层为容器平台上应用开发相关的部分，主要分为数据库服务、消息队列服务、应用的定义与镜像构建服务、持续集成和部署能力等四个方面进行展开。这一层主要是聚集了与应用相关的通用的架构、工具以提供相关能力。
结合CNCF孵化项目，可以考虑使用TiKV或者Vitess做相关的数据库服务、NATS提供消息队列能力，使用HELM进行应用编排。
而在实际的项目，考虑到工具或者框架的通用性以及功能性等诸多因素，在数据库方面，显然MySQL/MariaDB/mongoDB/Cassadra/redis/PostgreSQL等更能得到项目开发者和使用者在容器化的路上选型的青睐。而关于流或者消息机制方面，实际上kafka和RabbitMQ在实际的项目中被使用的也往往更多一些。而在应用编排方面，HELM和Operator确实是很多项目现在是实践选型中较多的类型。而关于持续集成和持续部署的能力，虽然没有孵化项目，根据业界的使用情况，Jenkins作为最为主流的相关工具，甚至没有之一，围绕Jenkins已然产生了大量的生态环境，Jenkins已经有超过1300+的插件支持，几乎大部分主流的项目都提供了Jenkins插件的支持，另外Gitlab出现在这里也是因为Gitlab的功能中基本上可以完成软件生命周期的很多部分，远远不只是进行代码仓库的管理。

2: 编排管理
解读：
此层为容器平台上编排管理相关的部分，主要分为等容器编排与调度、协作与服务发现，RPC远程调用、服务代理、API网关与服务网格等六个方面进行展开。这一层主要是聚集了与容器的编排管理的通用的架构、工具以提供相关能力。
结合CNCF孵化项目，可以考虑使用Kubernetes做相关的容器编排服务，使用CoreDNS和etcd做服务发现，使用gRPC做远程调用，envoy进行服务代理，使用LINKERD用于构建服务网格。
而在实际的项目，显然Kubernetes已然是容器编排的标准选择，目前的阶段即使选择MESOS/SWARM等框架，实际上这些也都已经转向于支持Kubernetes了，可谓所向披靡，一敌难求。另外，CoreDNS和etcd也算是Kubernetes的标准组成部分了，但是在实际的项目中，早期的使用ZooKeeper进行服务发现的仍有不少，国产使用NACOS的也逐渐增多，另外剩余的基本都在使用NETFLIX 的Eureka。而远程调用，在目前CNCF所给选项中gRPC确实是一个重要的选型。而在服务代理方面envoy虽然是CNCF为数不多的毕业的项目之一，显然它并不能像毕业的Kubernetes那样有影响力，相反而言，硬件方式的F5是土豪们的最爱，基于Nginx的OPENRESTY或者Nginx本身即使很多务实的开发者的选择，HAPROXY和traefix也有不少的用户再进行使用，所以在微服务的路上想要一统天下，还需要看后续的服务网格的部分。在API网关部分，CNCF没有相关的孵化中或者已毕业的项目，根据实际的使用情况务实的开发者可能会直接使用Kong，Kong本身也是在Nginx上产生的一个框架，难能可贵的是已经形成了一部分生态。而服务网格Service Mesh显然是后续的重头戏，相较于孵化中的LINKERD，显然Istio后来居上，俨然有成为微服务管控的集大成者之势，而在实际的项目中反而比LINKERD和envoy更具竞争性，而对于新型的微服务架构希望进行尝试的用户也有很多直接一面导向了Istio，而NETFLIX的spring cloud相关的组件仍然继续不温不火，慢慢地错失良机。

3: 运行环境
解读：
此层为容器平台上运行环境相关的部分，主要分为等云原生存储、云原生网络和容器运行环境三个方面进行展开。这一层主要是聚集了与运行环境相关的通用的架构、工具以提供相关能力。
结合CNCF孵化项目，可以考虑使用ROOK做相关的云原生存储服务，使用CNI进行云原生网络管理，使用containerd或cri-o来进行容器运行环境的管理。
而在实际的项目，关于云原生存储，一般的项目可能直接使用NFS或者本地存储类型，除此之外可能会在ceph和glusterfs之间进行选型；而关于云原生网络，CNI作为一种通用的网络解决的框架，选择确实很多，但是最终还是会落到flannel、calico与Weave net三大主流框架之间的选型。

4: 配置管理
解读：
此层为容器平台上配置管理相关的部分，主要分为等自动化与配置、镜像私库、安全与合规等四个方面进行展开。这一层主要是聚集了与配置管理相关的通用的架构、工具以提供相关能力。
结合CNCF孵化项目，可以考虑使用Harbor做相关的镜像私库服务，Notary、Open Policy Agent与TUF进行安全和合规的检查。
而在实际的项目，自动化和配置方面虽然CNCF没有孵化中或者已毕业的项目，由于这是传统运维中非常常见和重要的一个环节，使用Ansible、CHEF、SALTSTACK、PUPPET以及Rundeck的用户都很多，而Ansible由于其轻便小巧无需客户端安装，在可以使用ssh前提下的项目中得到了很多的实践。而镜像私库，HARBOR的功能确实较为不错，是一个选择项之一，在实际的项目中，简单的情况直接使用docker Registry的也有不少，另外在HARBOR进入CNCF之前portus等也是选择项之一，考虑到其他功能的组合，使用商业的JFROG Artifactory的企业也有一部分，另外暂未加入CNCF的Nexus由于其统一的解决方案以及开源版本与价格亲民的商业版本的结合，也得到了很多用户的青睐。而关于安全与合规性，显然clair和anchore聚焦于镜像安全确实是目前很不错的一个切入点，很多用户在此方面进行了功能的强化，而Sonatype Nexus与snyk也有一部分的用户在进行使用。而关于私钥管理的功能，此部分功能往往作为辅助的定制化内容在其他的自动化和配置中进行了管控，目前CNCF还没有孵化中的项目，实际的项目中也似乎不存在一个让人非常期待使用的项目。

5: 平台管理
解读：
此层主要是由CNCF提供了一些官方的建议名单，企业有相关的资质和能力，通过CNCF的认证，与其合作，出现在此部分中，作为可供使用者选择的推荐列表。主要有四个部分组成：通过认证的K8S平台提供商、通过认证的K8S托管平台提供商、通过认证的K8S安装工具提供商、PaaS/容器服务提供商。

6: 监控分析
解读：
此层为容器平台上监控与分心相关的部分，主要分为等监控、日志、服务链追踪与混沌工程等四个方面进行展开。这一层主要是聚集了与监控与分心相关的通用的架构、工具以提供相关能力。
结合CNCF孵化项目，可以考虑使用Prometheus做相关的监控告警服务，使用fluentd进行日志管理，使用JAEGER或OPENTRACING进行服务调用链分析。
而在实际的项目，已经毕业的Prometheus确实是一个不错的选择项，但是由于监控是传统运维的重要作业，所以在存量的解决方案中已经有很多实际使用的方式，在这些方案中，使用graphite或者influxdata提供时序列数据存储功能，使用Grafana进行展示的项目不在少数，结合ZABBIX和Nagios进行监控的项目也有不少。而进行日志的管理，显然ElasticSearch和Logstash的ELK或者EFK是很多用户的选择，除此之外，商业的版本使用splunk的用不也不在少数。而关于服务链路跟踪，显然使用Dapper论文原理实现的ZIPKIN和PINPOINT得到的实践最多，而在SpringCloud的解决方案中，Spring Cloud Sleuth作为调用链的解决方案，随着Spring Cloud的使用也得到了很多地关注，成为Spring Cloud下理所应当的服务调用链解决方案。

7: 无服务（Serverless）

解读：
Serverless作为一个重要的方面，而且各家公司也都出了不少的产品，理念非常新颖，虽然也有不少公司开始尝试，短时间内应该仍是以Service Mesh等微服务解决方案为中心的阶段，相关的平台、框架与工具以及安全相关的组件信息如下图所示。

![在这里插入图片描述](https://tva1.sinaimg.cn/large/008i3skNly1gsnqm7dsynj31b50u0gt6.jpg)

8: 合作伙伴（Special）
解读：
此部分主要分成两部分，一部分是Kubernetes认证的服务提供商，另外一部分则是提供Kubernetes的培训的合作伙伴，可以看到这里不是Cloud Native的Training Partner，而是Kubernetes的Training Partner。不管如何，在其中也已经可以看到不少的国内公司的身影。

![在这里插入图片描述](https://tva1.sinaimg.cn/large/008i3skNly1gsnqm5rf8tj320i0iiaji.jpg)

9: 成员构成

> 解读：
> CNCF的成员也同样分为白金、黄金和白银级的会员，目前阶段的信息如下所示。

![在这里插入图片描述](https://tva1.sinaimg.cn/large/008i3skNly1gsnqm4dusvj312h0u0wk5.jpg)



总结
CNCF的Landscape已经提供了非常之多的云原生相关的工具、平台与框架进行选择，如果是路线图给出的是大体方向的指引，而Landscape则主要是提供具体的可供落地实施的工具方法，在企业推行云原生应用落地的时候，应该作为首选参照的内容之一。

参考内容
https://github.com/cncf/landscape
https://landscape.cncf.io/images/landscape.png
https://github.com/cncf/toc/blob/master/DEFINITION.md
https://github.com/cncf/toc



- https://blog.csdn.net/liumiaocn/article/details/100713072

