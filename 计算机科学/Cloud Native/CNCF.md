[TOC]

CNCF（Cloud Native Compute Foundation） 是 Linux 基金会旗下的一个组织，旨在推动以容器为中心的云原生系统。从 2016 年 11 月，CNCF 开始维护了一个名为 Cloud Native Landscape [1]的 repo，汇总目前比较流行的云原生技术，并加以分类，希望能为企业构建云原生体系提供参考。



2017 年 12 月 06 日，Landscape 的 v1.0 版本发布，本文就按照下面这种图介绍云原生系统的大致情况。



![img](https://tva1.sinaimg.cn/large/008i3skNly1gvh7l6tj5sj61hc0u04bz02.jpg)



云原生以容器为核心技术，分为运行时（Runtime）和 Orchestration 两层，Runtime 负责容器的计算、存储、网络；Orchestration 负责容器集群的调度、服务发现和资源管理。



往下是基础设施和配置管理，作为容器底层的基石。容器可以运行在各种系统上，包括公有云、私有云、物理机等；容器还依赖自动化部署工具、容器镜像工具、安全工具等运维系统才能工作。



往上是容器平台上的应用层，类似于手机的 App Store，图中分为数据库和数据分析、流处理、SCM 工具、CI/CD 和应用定义几类，每个公司根据业务需求会有不同的应用体系。



右边有两块：平台和观察分析。平台是指基于容器技术提供的平台级的服务，比如常见的 PaaS 服务和 Serverless 服务。观察分析是容器平台的运维，从日志和监控方面给出容器集群当前的运行情况，方便分析和 debug。



NOTE：因为图中给出的软件很多，所以文中会挑选一些比较有名的以及本人比较熟悉的介绍，会略过一些信息；此外，也因为个人的水平有限，并没有对所有产品都一一使用过，因此有些内容未免有偏颇或者错误之处，如果读者发现，还望能不吝指出。



\1. Cloud（云）

![Image](https://mmbiz.qpic.cn/mmbiz_png/b2YlTLuGbKDsbJzupnILVFhPtMaRjmvPKYRqTMjibE9pnd8oiawLVrQbOHQe4wBXkBQkzpKCWPKBqWgOLgwccBug/?tp=webp&wxfrom=5&wx_lazy=1&wx_co=1)



容器需要运行在操作系统上，系统可以运行在虚拟机或者物理机上。从使用方式上来分，操作系统这层（Iaas）可以分为公有云和私有云。



**公有云**



公有云国外以亚马逊 AWS、微软 Azure、谷歌 GCP、DigitalOcean 为代表，国内有阿里云、腾讯云、华为云，此外 IBM、Oracle、Fujitsu 都有自己的云产品，Joyent 也是国外很有名的云计算公司；Packet 是物理机云服务商，直接为用户提供物理机资源。



企业一般会选择其中一个平台来使用，也有不少企业同时选择两种或者多种云服务商，以提高可用性和避免厂商锁定。



**私有云**



私有云是指用户在自己的数据中心搭建的云服务，除了自己研发之外，常见搭建私有云的方法有 VMware（商业化的虚拟化软件） 和 OpenStack（Python 编写的开源 IaaS 平台软件）；此外 MaaS 提供物理机自动安装和管理服务，分为免费版和收费版；Foreman 是虚拟机和物理机的系统配置工具。



建设私有云的成本很高，但是当公司成长到一定规模的时候，私有云建设也是必要的一件事。除了能缩减成本，也能提高技术实力，而且也有更多的灵活性满足内部的各种需求。



\2. Provisioning（部署）

![Image](https://mmbiz.qpic.cn/mmbiz_png/b2YlTLuGbKDsbJzupnILVFhPtMaRjmvPKYRqTMjibE9pnd8oiawLVrQbOHQe4wBXkBQkzpKCWPKBqWgOLgwccBug/?tp=webp&wxfrom=5&wx_lazy=1&wx_co=1)



有了物理机和虚拟机，在运行容器服务之前，我们还需要为容器准备标准化的基础环境，以及保证基础设施的自动化，拿盖房子来比较，IaaS 和这部分共同组成了容器平台的地基。



**Host Management / Tooling**



自动化配置工具，保证容器运行的系统配置的一致性，并提供升级、补丁等功能，一般也可以用来 Bootstrap 容器服务。



这里的几家软件功能大同小异：



- **Ansible** 比较简洁，用 SSH 来自动化部署，使用 Python 编写
- **CFEngine** 是这个领域非常老的工具，可以说是配置管理的元老，用 C 编写，因此性能会更好，但是学习曲线也更曲折
- **Chef** 用 Ruby 编写，而且配置文件格式也是 ruby DSL，因此对于 Ruby 程序员非常亲切友好
- **SaltStack** 采用 ZeroMQ 作为消息队列，实现 master-salve 模式，兼具性能和灵活性，但同时整个系统也更加复杂
- **Puppet** 是这个领域的老大哥，以成熟稳定著称，社区文档也更丰富



这篇博客[2]和这篇文章[3]比较了 CFEngine vs Puppet vs Chef vs Ansible vs Salt 这几个工具的异同，如果纠结如何选型，推荐阅读。



其实，对于大多数需求，根据开发语言、配置文件风格等选择其中一种就行。



**Infrastructure Automation**



IaaS 平台提供了基础设施服务，但是对于复杂的场景来说，直接使用这些服务提供的接口还是会很麻烦，所以有了基础设施自动化。这部分做的事情就是能够让基础设施的配置自动化，一次完成多个资源的部署，提高效率。



- **Bosh**：CloudFoundry 旗下的产品
- **Cloudify**：云应用编排系统，能够让用户定义软件，然后部署到不同的云环境中
- **CloudFormation**：AWS 提供的基础配置服务，能够通过配置文件定义要创建的各种 AWS 服务，然后一键完成集群或者系统的搭建
- **Ubuntu Juju**：Ubuntu 提供的管理工具，能够自动化把几百种服务部署到不同的平台
- **Terraform**：HashiCorp 旗下的基础设施配置工具，通过定义一份配置文件，Terraform 能够在不同云提供商运行服务，是 Infrastructure as Code 的信奉者
- **Manage IQ**：统一管理云主机、容器、网络、存储的 IT 平台
- **Kubicorn**：管理多个 Kubernetes 集群的工具，集群可以在不同的云上
- **Helm**：Kubernetes 软件包安装工具，能够安装多个 Kubernetes 资源，类似于 Ubuntu 的 APT 工具



总的来说，这些工具就是在云平台或者 Kubernetes 平台上再封装一层，让用户能够通过一次定义，在不同平台部署多个资源或者服务，并做到版本升级和跟踪。如果云平台提供相关服务（比如 AWS 的 CloudFormation）直接使用即可，如果是混合云，则需要选择 Juju、Terraform 这样的管理工具。



**Container Registries**



容器的镜像 Registry 是容器平台的基础需求，毕竟所有的容器应用就是通过镜像来定义的，镜像服务分为自建和公有服务两种。



很多公司提供了它们公开的容器 Registr 服务，比如 Docker 官方的 Registry，亚马逊 ECR（Elastic Container Registry）、Azure 和 Google 云 Registry、此外 Quay、Project Atomic、JFrog Artifactory 也是比较著名的容器镜像服务提供商。



Harbor 是开源的企业级容器镜像管理平台，Portus 专门为 Docker Registry 提供授权服务。



国内一般企业会选择自建 Registry，因为国外的 Registry 访问速度都很慢，而国内并没有非常流行的 Registry 服务（当然很多容器服务公司都会提供 Registry 服务），另一方面自建 Registry 的技术并不复杂。



**Secure Images**



随着镜像和容器标准化的完善，镜像和容器的安全也越来越受到企业的关注。虽然在大多数情况下，安全往往是软件开发者最后才关心的事情，但是容器安全却是不容忽视的一个环节。



Notary 和 TUF（the update framework） 是 CNCF 旗下的两个项目，TUF 是开源的安全和验证标准，Notary 是它的一个实现，Notary 可以用来验证镜像的安全性，也可以用来安全地发布软件包。



- Clair：CoreOS 开源的容器安全性分析工具
- Twistlock 是云原生系统的安全性平台
- NeuVector 是网络安全分析工具
- Aqua 也是容器安全平台，提供镜像、容器、用户认证、网络流量限制等多种安全功能
- Anchore 提供了一系列容器环境安全分析、审查和扫描工具



**Key Management**



和安全相关的另一个问题是机密信息，包括密码数据、密钥等。



Keywhiz、Pinterest 开源的 Knox、Lyft 开源的密码存储工具 Confidant 和 HashiCorp 开源的 Vault 想要解决机密信息的存储，它们通过加密的方式把内容保存到后端存储中，而且提供了 Auditing 等额外功能。



SPIFFE 和 SPIRE 是一对的，SPIFFE 定义了服务的认证标准和认证信息的标准，SPIRE 是它的一个实现，但是目前还没有达到生产可用。



\3. Runtime（运行时）

![Image](https://mmbiz.qpic.cn/mmbiz_png/b2YlTLuGbKDsbJzupnILVFhPtMaRjmvPKYRqTMjibE9pnd8oiawLVrQbOHQe4wBXkBQkzpKCWPKBqWgOLgwccBug/?tp=webp&wxfrom=5&wx_lazy=1&wx_co=1)



容器运行时这块是容器核心的技术，关注的是主机容器的技术模块，分为计算、存储、网络三块。



**Container Runtime（容器运行时）**



我们知道，整个容器技术就是因为 Docker 的出现才变得炙手可热，可以说 Docker 重新定义了容器，也成为了容器技术的代名词。但是随着容器的标准化，Docker 把核心组件抽象出 containerd，作为容器运行时，而更多公司也推出自己的容器实现，容器一词的含义开始扩展，而且也逐渐标准化了。



随着容器运行时的稳定，普通用户对其关注会逐渐下降。如果把运行时比作内核，那么容器调度系统就是操作系统，开发者应该更关心操作系统的功能和性能，只有遇到特殊需求或者问题时才会关注内核。



OCI（Open Container Initiative）是一个促进容器标准化的组织，主要工作是容器 Runtime 和镜像的标准化协议，这部分内容可以参考我之前的介绍文章[4]。



- **Containerd**：Docker 公司从原来的 Docker 引擎中剥离出来的容器核心功能，具有镜像管理和容器隔离两大功能，底层使用 runC
- **Rkt**：CoreOS 公司提出的容器引擎技术，一直作为 Docker 的直接竞争对手存在，对于促进容器标准化贡献很大
- **LXD**：基于 Linux 老牌容器管理工具 LXC，旨在提供更好用的接口，最大的特色是使用容器技术提供类似虚拟机的服务
- **runV**：以兼容 OCI 接口的方式管理虚拟机，支持 KVM、Xen、QEMU 等虚拟化技术。换句话说，可以直接把虚拟机作为 Runtime 运行在容器集群管理平台上
- **Intel Clear Containers**：Intel 公司推出的容器技术，因为爸爸的缘故最近也开始出现在容器圈各种文章里



CRI-O 是 Kubernetes 推出的东西，它是 kubelet 和 OCI Runtime 之间的桥梁，它内部采用 OCI 标准，因此可以兼容各种 Runtime（比如 runC、Clear Container等），对上它提供 CRI 接口供 kubelet 调用。这样的话，CRI-O 的存在能够让 kubelet 使用任何 OCI 兼容的 Runtime，从而绕过 Docker、Rkt 这种完整容器管理工具。



**Cloud Native Storage（云原生存储）**



容器从一出现就非常契合微服务中无状态服务的设计理念，因此初期甚至给了一些人容器只适合无状态服务的印象，但是随着容器技术的成熟和用户理念的变化，容器目前已经全面进入有状态服务领域。因为容器存活时间很短的特点，容器的状态（存储的数据）必须独立于容器的生命周期，也因为此，容器的存储变得非常重要，云原生存储这块介绍了很多相关的技术。



作为 IT 领域的核心技术，存储早在容器火起来之前就已经有发展了很多年，从单机的各种文件系统、到网络存储，再到现在比较热门的分布式存储、以及云计算催生的块存储、文件存储、对象存储，不同需求不同分类的存储早就五花八门了：



- **Ceph**：分布式存储系统，同时支持块存储、文件存储和对象存储，发展时间很久，稳定性也得到了验证。之前在 OpenStack 社区被广泛使用，目前在容器社区也是很好的选项。
- **GlusterFS**：RedHat 旗下的产品，部署简单，扩展性强。
- **Hadoop HDFS**：Apache 基金会下的开源文件系统项目，在大数据领域广泛使用，基于 GFS 理念的开源版本。主节点保存元数据，并负责数据分布、复制、备份决策，工作节点存储数据，每个数据会有多个副本，保存在不同的工作节点。
- **SheepDog**：起源于日本的块存储开源项目，设计之初是为了用于虚拟化平台 QEMU。
- **Swift**：OpenStack 项目提供的对象存储工具。
- **LeoFS**：高可用、分布式的对象存储系统，海量数据支持比较好，提供 HTTP、S3、NFS 等接口访问。
- **Minio**：开源的对象存储软件，提供和 AWS S3 兼容的接口，简单易用。



除了这些开源的存储技术之外，还有很多容器存储圈的技术公司：



- **DELL EMC**：商业存储的典范，提供企业级各种需求的存储解决方案，作为商业存储的大哥，自然也会在容器存储上发力。
- **NetApp**：致力于混合云的存储方案，是一家老牌的公司，在存储行业深耕多年。
- **Datera**：一家存储创业公司，主要产品是 EDF（Elastic Data Fabric），提供 API 优先的企业级存储方案，有纯软件和一体机两种不同的版本。
- **Diamanti**：Diamanti 一家超融合基础设施初创公司，主要为企业数据中心提供基于容器的硬件及软件支持服务。
- **Hedvig**：为私有云和混合云提供统一的数据存储服务，为虚拟机和容器提供软件定义存储。
- **Infinit**：开源的软件定义存储公司，之前是做文件跨平台传输的。产品也是统一的存储平台，为各种计算平台提供块存储、对象存储和文件存储等接口。已经被 Docker 收购。
- **Pure Storage**：一家明星存储创业公司，最大的特定是对闪存的支持
- **StorageOS**：为容器提供分布式存储的统一视图，对上层提供 API 实现存储的自动化管理，作为容器部署。产品也分为免费版和收费版。
- **Quobyte**：数据中心文件系统，被 Kubernetes Volume 插件直接支持。



因为不同用户对存储需求不同，采取的存储方案也不同，不管是开源方案、商业方案还是自研方案，或者是文件存储、对象存储还是块存储，怎么把这些技术用到容器平台，以及保证标准化和统一化的接口，是非常有挑战性的事情，目前也有很多努力：



- **CSI（Container Storage Interface）**：定义云应用调度平台和各种存储服务接口的项目，核心的目标就是存储 provider 只需要编写一个 driver，就能集成到任何的容器平台上。
- **libStorage**：EMC 旗下研发的一个存储开发框架，旨在开发与容器平台无关的存储框架，大致的思想是 libStorage 来处理和容器平台的交互，存储框架只需要接入到该框架就行。
- **REX-Ray**：基于 libStorage 实现的存储管理平台，支持大部分的存储提供商，也能运行在大多数操作系统上。
- **OpenSDS**：开放的软件定义存储标准，集合各大存储厂商，提供统一管理的存储标准，隶属于 Linux 基金会。
- **Rook**：基于 Ceph 作为后台存储技术，深度集成到 Kubernetes 容器平台的容器项目，因为选择了 Ceph 和 Kubernetes 这两个都很热门的技术，并且提供自动部署、管理、扩展、升级、迁移、灾备和监控等功能，所以很受关注。
- **Portworx**：针对容器技术打造的，把每个节点的存储资源组成一个存储池，每个数据自动进行备份，并通过和容器平台调度深度集成保证数据高可用。分为免费版和商业版。



**Cloud Native Network**



网络最重要的功能是提供不同计算资源的连通，随着虚拟化和容器技术的发展，传统的网络方案已经无法满足云计算快速增长、不断变化的网络需求。不同用户对网络的要求也越来越高：



- **安全性**：保证私有和公有云网络的安全，网络流量能够加密，不被窃听和修改
- **多租户**：云计算需要同时为多个租户提供网络服务，不同租户之间互相独立而且隔离
- **快速响应**：容器的生命周期大大缩短，集群的网络在实时动态变化，网络方案需要感知网络的变化，并快速提供服务
- **网络迁移**：虚拟机和容器会在集群上迁移和调度，网络方案需要支持计算资源跨主机迁移后的连通
- **监控和调试**：云上的网络环境，让整个网络的流量变得更加复杂，我们需要新的工具让网络可视化，并做到自动化运维
- ……



因此，在云计算和容器这块涌现出很多网络解决方案和厂商，试图解决这些问题：



- **CNI（Container Network Interface）**：Kubernetes 和 CoreOS 提出的容器网络接口标准，旨在为容器平台提供统一的网络访问模式，目前很多网络方案都已经集成进来。
- **Calico**：基于 BGP 的纯三层网络方案，性能很高，并且提供强大的网络防火墙功能，以满足用户对安全性的需求。
- **Canal**：基于 Flannel 和 Calico 提供 Kubernetes Pod 之间网络防火墙的项目。
- **Contiv**：思科推出的网络方案，支持 VXLAN 和 VLAN 方案，提供了多租户和主机访问控制策略功能。
- **Cilium**：利用 Linux 原生技术提供的网络方案，支持 L7 和 L3、L4 层的访问策略。
- **Flannel**：CoreOS 主要的容器网络项目，主要用于 Kubernetes 平台提供 Pod 之间的连通性，提供多种连网方案，部署和管理简单。
- **Midokura**：日本 SDN 网络公司，主要产品是开源的 MidoNet，之前广泛用于 OpenStack 中，目前有很多把它应用到容器领域的尝试。
- **OpenContrail**：Juniper 收购的开源网络虚拟化平台，目前已经加入 Linux 基金会。OpenContrail 是一个可扩展的网络虚拟化控制层，提供了丰富的软件定义网络功能和安全性。
- **Open vSwitch**：Linux 平台的虚拟交换机软件，除了提供和 Linux 虚拟网桥类似功能外，还支持 OpenFlow 协议。
- **Weave Net**：Weaveworks 公司开源的 Docker 跨主机网络方案，安装和使用都比较简单，内部也是通过 Overlay 网络实现的
- **Romana**：Panic Networks 推出的网络开源方案，基于 L3 实现的网络连通，因此没有 Overlay 网络带来的性能损耗，但是只能通过 IP 网段规划来实现租户划分，不够灵活
- **Tigera**：网络方案初创公司，主推的方案是把 Flannel 和 Calico 集成到一起的 Canal，应用 Calico 的网络策略到 Flannel 中。



也有很多的商业公司为企业提供网络解决方案：



- **Aviatrix**：混合云网络解决方案提供商，集成 AWS、Azure、Google 等公有云网络，在同一平台管理公司混合云网络。
- **Big Switch**：下一代数据中心网络公司，提供 SDN 可编程的网络方案，主要有 Big Cloud Fabric 和 Big Monitoring Fabric 两种产品方案。
- **VMware NSX**：虚拟化厂商 vmware 提供虚拟化网络方案。
- **Cumulus**：主要产品是 Cumulus 操作系统，继承了众多的网络软件，提供丰富的网络功能。能够解除数据中心网络设备硬件和软件锁定的局面，为网络硬件带来软件的灵活特性。
- **NuageNetworks**：致力于数据中心 SDN 网络的公司，提供解决方案



\4. Orchestration & Management（编排和管理）

![Image](https://mmbiz.qpic.cn/mmbiz_png/b2YlTLuGbKDsbJzupnILVFhPtMaRjmvPKYRqTMjibE9pnd8oiawLVrQbOHQe4wBXkBQkzpKCWPKBqWgOLgwccBug/?tp=webp&wxfrom=5&wx_lazy=1&wx_co=1)



当在生产环境中使用容器时，单台主机上的容器已经不能满足需求，需要管理多主机容器集群，也就需要有个工具能够提供资源管理、容器调度和服务发现等功能，保证多主机容器能够正常工作。可以说，对于云原生系统，Orchestration 才是最核心的东西。



**Scheduling & Orchestration**



调度和集群管理一直是容器技术的热点领域，竞争也非常激烈。打个可能不那么恰当的比喻，如果把容器 Runtime 比作引擎，那么容器集群管理工具就是汽车。用户购买的是汽车，尽管引擎非常重要，但是它终归只是个可以替换的零件。



集群管理竞争还在，并没有最终的唯一胜利者，但总的来说 Google 公司的 Kubernetes 处于绝对的领先状态，也是目前社区发展最快的平台，随着 Docker 官方支持 Kubernetes，以及 Azure 和 AWS 。目前社区三个主流的容器调度平台是：



- **Kubernetes**：起源于 Google 内部的 Borg 系统，率先提出 Pod 的概念，提供自动化管理、服务发现、服务升级、有状态服务等等特性
- **Docker Swarm**：Docker 公司官方的容器管理平台，最大的特点是和 Docker 兼容的 API 和操作命令
- **Mesos**：Apache 旗下的任务调度平台，后来应用于容器调度



对于公有云上的容器服务，各大云服务商也有对应的产品：



- **Amazon ECS**：亚马逊推出的容器服务，特点是虚拟机收费，容器免费
- **Azure Service Fabric**：微软 Azure 的容器服务调度平台
- **Nomad**：HashiCorp 旗下的数据中心调度平台，同时支持服务和任务两种 Job，也已经支持 Docker 容器



**Coordination & Service Discovery**



有了容器集群管理工具，容器的规模逐渐变多，另外一个需要解决的问题是服务之间怎么互相发现对方。因为集群的容器是不断变化的，IP 地址也是不稳定的。这个问题再往下思考，就是集群的状态应该怎么保存，才能让所有节点能当前集群自己想要的信息，而且保证不会发生冲突和错误。



目前，集群的状态都会保存在一个分布式的键值存储里，这个存储保证数据的一致性，目前三款常用的键值存储工具是：



- **ZooKeeper**：Hadoop 的一个子项目，本来是作为 Hadoop 集群管理的数据存储，目前也被应用到容器领域，开发语言是 Java。一个缺点是使用和维护比较复杂
- **Consul**：HashiCorp 开发的分布式服务发现和配置管理工具，Docker Swarm 集群之前默认使用的就是这个
- **Etcd**：CoreOS 旗下的键值存储工具，是 Kubernetes 默认的选择，因此使用范围很广



有了分布式键值存储保证一致性，还需要工具把集群数据自动写入到里面，并且需要格式化地读取和解析数据。围绕这一话题，周边也有很多工具：



- **Registrator**：自动监控 Docker 容器，把每个容器的信息（IP、端口等）写入到键值存储中，支持 etcd、Consul
- **SkyDNS**：基于 etcd 中的数据，对外提供 DNS 数据查询，是对 etcd 的一层封装。因为使用 etcd，所以 DNS 查询是实时的，避免了缓存导致的问题
- **CoreDNS**：SkyDNS 继承者，主要特点是插件系统能完成各种各样的功能
- **ContainerPilot**：Joyent 开源的容器服务发现工具，作为容器的 init 系统运行，通过定义一个 JSON 文件，它会把容器相关的信息更新到 Consul 中、进行健康检查、运行用户定义的代码等



除外，还有两个公司开源的服务发现工具要提一下：



- **SmartStack**：Airbnb 开源的服务发现工具，由 Nerve 和 Synapse 组成，安装和运维相对复杂了些
- **Netflix OSS Eureka**：Netflix 开源的用于 AWS 的服务发现工具



总的来说，这些工具保证这个集群的动态信息能统一保存，并保证一致性，这样每个节点和容器就能正确地获取到集群当前的信息。



**Service Management**



伴随着容器技术而变得火热的一个话题是微服务，每个服务作为容器或者 Pod 运行，不同服务之间通过服务发现知道对方的地址进行通信。随着集群规模的增大、服务数量的增多，用户的需求也不断增加，微服务架构也面临更多的问题：



- **认证和安全**：为了安全，调用方需要进行身份认证，而且不同的微服务只能运行不同的用户访问
- **统计**：每个微服务需要知道它的使用情况，什么人在什么时候调用了什么接口，方便监控和排查错误
- **健康检查和自动恢复**：系统能自动检测服务的可用性，一旦不可用就重启恢复或者从调用链中删除
- **自动重试**：如果调用某个服务发生错误，可以自动按照特定算法重试
- **限速**：服务应该限制它能接收请求的速率，以保证它不会被过量的请求压垮
- **服务可用性和雪崩**：每个服务的可用性都不可能是 100% 的，简单的串联调用会降低整个集群的可用性。如何保证每个服务不可用不会导致调用方的僵死
- **负载均衡**：怎么自动把请求分配到不同的后端进行处理，调度算法能否满足各种各样的需求
- **升级发布**：每个微服务的升级怎么做到不影响其他服务，怎么进行灰色发布，出错怎么快速回滚
- **测试**：单个服务可以独立测试，但是整个集群怎么进行功能和性能测试
- ……



这些东西都是每个微服务平台必须要考虑的，如果放在每个服务代码中实现某些功能，不仅增加了每个服务的复杂性，也会导致重复的工作，所以，微服务需要更好的框架和平台统一提供这些功能。总的来说，微服务虽然降低了单个服务的复杂性，但是把复杂性下沉到微服务管理平台层面。



针对这些问题，有很多软件和方案。对于负载均衡来说，HAProxy、Nginx 和 F5 都是常用的方案，Traefik 是后起之秀，专门为微服务设计；RPC 框架用来在微服务内部进行通信，因为比 HTTP API 效率高而被大量使用，常用的用 Google 开源的 GRPC 、Apache 旗下的 Thrift 框架、Netflix 开源的自带负载均衡的 Ribbon 和 Avra 数据序列化框架。



微服务 Gateway 是统一化管理 API 注册接入、权限认证、限速、日志等功能，是微服务对外的接口。



- **Kong**：Mashape 开源的项目，基于 OpenResty（Nginx + Lua） 的微服务网关，以插件为中心组织功能

- **Netflix Zuul**：Netflix 微服务网关，使用 Java 开发，因此适用于 Java 应用，需要添加代码来使用 Zuul 提供的功能

- **Nginx**：Nginx Plus 产品为企业提供负载均衡、代理、微服务网关的各种功能

  3scale：红帽公司的 API 网关工具



这个领域也有一些公司在提供产品，比如 Datawire 就专门为 Kubernetes 应用提供 API Gateway 和自动化源码部署的工具。



微服务开发框架 Hystrix 是 Netflix 开源的项目，能够帮助程序处理微服务交互的超时处理和容错的问题，提供熔断、隔离、降级等功能，但是只能用于 Java 语言项目，需要在程序中修改代码。



特别要强调一下微服务领域最近比较热门的概念：**Service Mesh**，它的主要想法是把微服务通用的功能单独抽象为一层，部署在容器管理平台中，对应用透明，并且通过简单自动化的接口来控制整个微服务的连通、灰度访问、升级、降级等功能。



- **Linkerd**：开源的网络代理服务，使用 Scala 语言编写，最早提出了 Service Mesh 的概念
- **Envoy**：C++ 编写的网络代理工具，和 Linkerd 的定位相同，Turbine Labs 公司专门提供 Envoy 的部署和管理工作
- **Istio**：Google、IBM 和 Lyft 联合发布的微服务管理、配置和监控框架，使用 Envoy 或者 Linkerd 作为内部 worker，控制层面负责配置和管理，深度集成到 Kubernetes 平台



Service Mesh 相较于之前微服务框架的最大优势是它对业务是透明的，不需要像 Netflix 提供的很多微服务工具那样对应用有侵入性，因此就不再和任何语言绑定，可以看做整个网络栈的另一个抽象层。



\5. Platform（平台）

![Image](data:image/gif;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWNgYGBgAAAABQABh6FO1AAAAABJRU5ErkJggg==)



平台这块主要是基于容器技术做的更上面一层的封装，有直接是接管公有云或者私有云的容器平台，也有 FaaS 这一类服务。



**PaaS/Container Service**



因为容器技术的隔离性，以及对应用非常友好，因此可以直接拿来做 PaaS 服务，当然也有种叫法是 CaaS（Container as a service）。很多初创公司的业务也就是这块，基于容器提供应用的发布、升级、运维等管理工作。大部分公司做的事情都大同小异，因为最终的需求是一样的：让应用的开发、部署、扩展、升级、运维更轻松，用户无需关心基础架构，只需要考虑如何去实现业务逻辑就行，主要的区别在于侧重点，有些侧重私有的数据中心部署和管理，有的侧重 Docker 容器的管理，有的测试 Kubernetes 等容器集群的维护，有的提供应用平台，有的和公有云平台深度集成……



Heroku 是老牌的公有云类型的 PaaS 平台，界面友好，成熟稳定，所有操作提供命令行实现自动化，提供完整的监控和日志方案。



Cloud Foundry 和 OpenShift 是两款重要的开源 PaaS 平台，其中 Cloud Foundry 是 Pivotal 开源，支持多种语言、框架、云平台，旨在让开发者能够忽略架构问题，快速部署和扩展应用；OpenShift 是 Red Hat 开源的，功能和 Cloud Foundry 差不多，网络上有很多两者的对比文章，这里不再赘述。目前两者都已经开始拥抱容器、Docker、Kubernetes 技术，希望能和容器深度集成。它们的特点是功能强大，可扩展性强，但是相应的，复杂程度也高。



随着 Kubernetes 的快速发展，很多以 Kubernetes 为容器管理平台和应用管理的公司也都出来了，Datawire 基于 Kubernetes，侧重于微服务的管理；Containership 也是 Kubernetes 的管理平台，可以在多个云平台自动化部署和统一管理；Giant Swarm 提供混合云和多租户的 Kubernetes 管理；Kubermatic 能够给用户提供一键 Kubernetes 集群安装和多集群管理服务；Gravitational 提供多 Regions 的 Kubernetes 集群管理。



Atomist 和 Cloud 66 侧重于 DevOps 流程；Flynn 是基于 Docker 容器技术的开源 PaaS 软件，相比 Cloud Foundry 算是轻量级的实现；Hyper.sh 比较有趣，它们以容器接口来提供虚拟机服务。



其他一些平台提供的服务如下：



- **Apcera**：一个企业级的容器管理平台，包括了运行容器所需编排、网络和安全功能。Apcera 的一个特点是支持传统的应用，同时兼容传统应用和云原生应用，支持把前者迁移到云上
- **Apprenda**：PaaS 云平台软件公司，基于 Kubernetes 打造的应用管理平台，目前的商业版本 ACP（Apprenda Cloud Platform）提供了 Kerberos 身份认证、应用审计等额外功能
- **Convox**：基于 AWS 的应用部署、管理、监控的平台服务，提供了命令行实现任务的自动化
- **DC/OS**：Mesos 的企业级产品，是一套开源项目，基于 Mesos 分布式系统和 Marathon，提供了编排、应用商店、GUI 界面等功能
- **Diamanti** 也是一家解决方案公司，基于 Kubernetes 调度平台，同时支持 OpenShift PaaS 平台
- **Docker**：没有看错，这里的 Docker 指的是 Docker 公司，而不是容器技术。作为一家商业化的公司，Docker 也提供了商业化的产品和解决方案，开源的部分称为 Docker CE（community edition），商业化产品为 Docker EE（Enterprise Edition）
- **Mirantis Cloud Platform**：原来有名的 OpenStack 公司，目前也逐渐接纳 Kubernetes，一起构建云平台
- **Moby project**：Docker 公司把开源组件命名成 Moby，意在把多个开源技术组件按照需求组合成满足用户需求的产品，Docker CE 就是其中的产出
- **Platform9**：同时支持 OpenStack 和 Kubernetes 为核心的 PaaS 服务
- **Portainer.io**：Docker 的界面化管理工具
- **Rancher**：容器管理平台，之前同时支持 Swarm、Mesos 和 Kubernetes，目前把重心逐渐迁移到 Kubernetes 上
- **Tectonic**：CoreOS 推出的 Kubernetes 集群服务，集成了 Quay 镜像服务、CoreOS 系统、和 Prometheus 监控等
- **Ubuntu**：Ubuntu 系统也内嵌了 LXD 容器技术，提供更多的容器技术



这块内容主要是容器创业公司，提供的都是基于 Docker、Kubernetes 或者其他容器技术的方案，因此做的事情大同小异，就不再一一介绍了，感兴趣的可以根据图中列出的公司自行了解。



**Serverless/Event-based**



容器技术把微服务的概念吵得火热，随后也让 Serverless 这个词出现了大众的面前。既然容器能够屏蔽基础设施，让开发者只关心交付的应用（容器镜像），那么我们可不可以更进一步，让开发者也不要关心交付的镜像，只关注业务的核心逻辑呢？这就是 Serverless 的想法，开发者定义好基于事件触发的业务逻辑，其他一切都交给平台，当用户发出请求或者其他事件发生时，平台会根据事先的配置自动运行响应的业务逻辑代码，从而实现真正的按需服务。如果说容器关心的是应用，那么 Serverless 关心的则是函数。Serverless 不是没有服务器，而是不用关心服务器和系统。



Serverless 是一个很新颖的技术，虽然理念非常好，但现阶段还不适用于所有的应用，主要是因为它的性能问题，以及距离成熟使用还缺少很多工具和方案，另外开发流程要接纳这种理念还需要一段时间。



AWS Lambda 服务算是商业产品中比较成熟的，它的出现让 Serverless 从概念化和实验化的东西变成了可行的方案。微软家的云平台也推出了 Azure functions；Google 家对应的产品叫做 Cloud Functions，从命名来看亚马逊略胜一筹。



OpenFaaS、Fission 和 Kubeless 都是基于 Docker 和 Kubernetes 开源的 Serverless 开发框架，如果要想打造自己的 Serverless 平台可以参考。



- Apex：帮助构建、管理和运行 AWS Lambda 的工具

- NStack 和 Nuclio 都是专门用作数据分析的 FaaS 软件工具

- OpenWhisk：Apache 旗下的 Serverless 框架，目前还是孵化项目

  Oracle Application Container Cloud

  PubNub BLOCKS：PubNub 提供的 Serverless 服务，用于集成到自家的服务推送中

- Serverless 是一个集成工具，它能帮助开发者在 Serverless 应该部署到 AWS Lambda、Azure Functions、GCP Cloud Functions、kubeless 等平台，也就是说它封装了这些平台差异，提供了一致的接口，方便迁移和管理多 Serverless 平台应用



\6. Observability & Analysis（观察和分析）

![Image](data:image/gif;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWNgYGBgAAAABQABh6FO1AAAAABJRU5ErkJggg==)



基于云原生的平台建立起来之后，我们要保证整个平台能够正常工作，保证运行在其上的服务不会因为平台错误而导致不可用，而且还要知道应用整体的运行情况，提前对某些可能出错的地方进行报警，一旦出错能够提供合适的信息便于调试和修复……这些都是观察（observability）和分析（analysis）要做的工作，为了方便下面统一使用监控（monitoring）一词。



**NOTE**：对于监控、观察、分析、日志等词语的使用并没有非常严格的定义，监控是 IT 行业比较传统的叫法，表示对应用和系统运行情况的数据统计和展示。目前有个叫法是观察，分为metrics/monitoring、logging 和 tracing 三个部分。为了容易理解，我们使用监控一词来代替，它广义地包含了以上所有内容。



监控对于系统来说非常重要，没有监控的平台就像没有仪表盘的飞机。监控的目标有几点：



- 了解系统的使用情况，可以用于容量规划、性能调优
- 提前或者及时发现问题，因为硬件总是会有故障的软件总是有 bug 的，及时发现能够更快处理，不影响到应用的正常运行。这些问题包括网卡不能工作、硬盘老化、或者软件异常等
- 帮助排查错误：当发现软件bug或者硬件故障时，监控能够帮忙分析哪个组件在什么时候出现了问题，方便定位问题



**Monitoring**

**
**

简单来说，监控就是了解应用和系统运行情况。



我们最常见的监控是对主机的监控，了解 CPU、内存、磁盘 IO、网络等资源的使用数据，以此了解主机是否正常，是否需要加硬盘或者扩展集群，是否有内存泄露等等。另外一个监控是应用的监控，不管是数据库、缓存服务器、消息队列、代理和缓存服务器，还是自己编写的应用，我们需要知道它们的运行情况，这个监控依据每个应用而定，监控方法、监控的数据以及解读方法对不同的应用来说会千差万别。



而容器的出现，让监控出现了另外一个维度：容器和平台的监控。我们不仅要知道每个主机的运行数据，还需要知道每个容器的数据，这个容器使用的 CPU、内存、磁盘 IO 和网络等，这个新的需求也就催生了新的监控思想和工具。



Zabbix 是老牌的监控工具，功能强大，最近界面也改进了不少；Nagios 和 Graphite 是另外两个经典的监控工具。Sensu 是一款较新的监控工具，Riemann 也能够进行分布式系统的集中式监控处理。



InfluxDB 和 OpenTSDB 都是时序数据库，后者是基于 HBase 的。



AWS CloudWatch 是AWS 自家的监控工具，当然是负责 AWS 云上的服务监控；Azure Log Analytics 是微软家日志监控工具。很多商业公司也提供各种各样的监控产品：AppNeta、Axibase、APPDynamics、Datadog、Dynatrace、NewRelic 和 Splunk 都提供应用级别的监控和数据分析业务。



再介绍一下经常听到的监控工具：



- **Prometheus**：时序数据库，提供了工具能读取 HTTP 接口暴露的数据保存起来，提供了丰富的查询接口以及一个 Web 界面
- **Grafana**：监控管理界面，能够从多个数据源导入数据，提供优美的界面和丰富的面板配置
- **CoScale**：专门为容器和微服务优化的监控平台
- **Sentry**：错误收集工具，能够集中式地查看应用的 Crash Report 和 Traceback 信息
- **Server Density**：专注于服务监控的 SaaS 服务平台
- **StatsD**：Etsy 开源的数据统计信息，可以把数据继续发到后端的监控平台，比如 Graphite
- **Sysdig**：容器和 Kubernetes 监控工具，同时提供了付费的监控服务



图中列出的监控公司和工具还有很多，这块的创业公司也很多，因为监控的数据不像业务数据那样机密，因此很多公司愿意使用 SaaS 监控服务。



**Logging**



日志容易理解，就是程序运行输出的信息，它们是调试的利器，当程序出错的时候，有效的日志分析能够快速定位问题。同时日志也能承担一部分的监控功能，反应系统运行是否正常。



Fluented 是一个开源的基于数据流（stream）的日志收集框架；Graylog 是另外一个开源的选择，它们的思想都是把日志从系统各处收集起来进行统一分析、过滤和管理；Elastic 提供 ELK（Elasticsearch、Logstash、Kibana） 技术栈负责日志收集，也提供在线的企业级 SaaS 服务。



除了使用开源的组件自己搭建日志服务平台，还可以使用一些公司提供的在线日志服务，只要把日志数据导入到它们的平台，不用关心日志服务的维护工作。Loggly 是一个在线的日志分析服务，需要付费使用；logz.io 提供管理的 ELK 在线服务，提供 ELK as a service，并且可以基于 AI 对数据进行分析；另外一家声称支持 AI 分析的日志服务公司是 loom systems；Splunk 这家公司也提供日志分析服务；PaperTrail、Sematext、SumoLogic 也都提供类似的日志分析服务。



**Tracing**



随着微服务的采用，用户的一个请求可以中间会经过多个不同的微服务才最终得到应答。传统的监控、日志都是针对单个组件的分析，用来了解当前组件的健康和运行状态，并不能给我们一个整体的动态的情况。



对于微服务来说，我们需要知道一个请求到底经过了哪些组件，每个组件耗费了多少时间，错误发生在中间的哪个步骤，每次调用的网络延迟是多少等等。对于使用不同语言、开发框架、数据库、和系统的微服务，我们需要有统一的跟踪标准，这就是 tracing 要做的事情。分布式的 Tracing，一般都受到 Google Daper 系统的设计影响。



OpenTracing是一个开放的 Tracing 接口标准和文档，提供了各种语言客户端的实现，支持的 Tracing 工具包括 Jaeger、Appdash 和 Zipkin（Twitter 开源）；Cloud Sleuth 是 Spring Cloud 全家桶中一员，主要负责 Tracing 功能。



这些 Tracing 工具都需要在应用中编写对应的代码，和 Logging 和 Metrics 类似，用户可以自定义要 Tracing 代码块的范围和父子关系。此外，也有很多工具会自动化嵌入服务组件之间的 Tracing 数据，比如之前讲过的 Istio。



Tracing 可以和 Metrics 结合一起使用，Tracing 负责组件和微服务之间的数据分析，Metrics 负责单个组件内的性能数据监控。



\7. Application（应用）

![Image](https://mmbiz.qpic.cn/mmbiz_png/b2YlTLuGbKDsbJzupnILVFhPtMaRjmvPKYRqTMjibE9pnd8oiawLVrQbOHQe4wBXkBQkzpKCWPKBqWgOLgwccBug/?tp=webp&wxfrom=5&wx_lazy=1&wx_co=1)



容器平台最终还是要跑应用的，最主要的应用当然是各个公司的业务应用，除此之外还有一些比较通用的应用，这个表格里也有列举，可以根据需求提供类似应用市场的功能。



**Database & Data Analytics**



数据库一直是软件领域的核心组件，所以包括了很多老牌的数据库软件，比如 MySQL、DB2、Oracle DB、PostgreSQL、MariaDB 等关系型数据库。基于这些传统的数据库也有很多周边的工具和软件，比如 Vitess 这种数据库集群工具；阿里巴巴开源的 SQL 数据库连接池工具 Druid，它还同时提供了监控功能。



NoSQL 的发展也让很多新型的数据库出现在了我们的视野：有 Redis、Couchbase 这一类的 KV 数据库；也有 MongoDB、RethinkDB、RavenDB 为代表的文档类数据库；Cassandra、Hbase、Vertica（列式关系数据库）、Scylla（KVM 之父的作品，旨在成为下一代的 Cassandra 数据库） 这样的 Column 数据库。它们最重要的特点是能够轻松进行集群扩容，不支持 SQL 查询，因此接口上都以其他形式满足用户各种各样的数据需求。



后来 newSQL 的概念出现，旨在结合 SQL 和 NoSQL 的优势，还是以 SQL 方式使用，但同时支持快速横向扩容和分布式事务，Google 内部的 F1/Spanner 是这方面的先驱，发过论文但是没有把项目开源，CockroachDB 和 TiDB 是这类数据库的代表，都是开源项目。



其他相关的数据库产品包括：



- **BigchainDB**：区块链数据库
- **CarbonData**：面向大数据平台的列数据文件存储格式，由国内的华为公司贡献给 Apache 基金会
- **Crate.io**：基于 Elasticsearch 的分布式 SQL 数据库，适用于需要快速搜索和聚合的查询场景
- **MemSQL**：从名字也能看出来，这是一款内存数据库，特点自然是性能高，
- **Noms**：采用 Git 思想的支持版本控制、fork和同步的数据库
- **Pachyderm**：旨在成为一个更现代的大数据处理平台，资源调度基于 Docker 和 Kubernetes，底层是自己的分布式存储系统
- **iguazio 和 Qubole**：自动化数据分析公司
- **SQL Data Warehouse**：Azure SQL 数据库产品



数据库是整个软件技术的基础，现在有个很流行的观念是：数据是一家公司最重要的资产之一。我们对数据库的要求也是更快、更多、更好，所以会有很多数据库相关的产品来解决各种各样的数据存储和处理需求。



**Streaming**



流处理与批处理对应，要求对海量数据的实时采集、计算和查询，很多业务场景要求尽可能快得对数据进行分析，从而做出决策，比如传感器、流量监控、股市行情、游戏数据分析等，对此类需求也催生了很多实时处理工具。



Kinesis 是亚马逊官方的流数据处理平台，是其云计算产品的一部分；Cloud Dataflow 是 Google 的流处理产品；开源方面，Apex 是 Apache 旗下开源的新型实时数据处理平台；Heron 是 Twitter 开源的数据处理平台，是 Apache Storm 的继承者；Spark 和 Flink 支持流处理的同时也支持批处理操作，两者定位非常相似，但内部实现的差距挺大。



Kafka 和 RabbitMQ 这类消息队列可以做到数据的快速收集；NiFi 是另一个 Apache 项目，是一个数据整合和分发系统，专门为数据流设计。



和大数据一样，流处理这个领域也是 Apache 占据了大半的江山。



**SCM（Source Code Management）**



虽然容器让产品以镜像的作为产出，但是代码还是要维护的，最有名的社区源码管理平台自然是 GitHub，GitLab 是开源自建 SCM 的推荐选择，Bitbucket 和 GitHub 定义类似，提供免费也提供商业版本的服务。



**Application Definition**



应用定义并没有明确的定义，大概要做的事情是屏蔽底层基础设置、云平台以及容器运行时，封装一个标准化的应用定义，从而实现应用自动化运行在任何地方。



- **Apache Brooklyn**：应用管理平台，可以通过简单的操作把应用部署到常用的云平台
- **Docker Compose**：定义多个容器的运行关系，Docker Compose 可以自动化管理这些容器的构建、生命周期、和网络连通等问题
- **Habitat**：应用自动化管理平台，可以定义应用，并且提供 Supervisor 来保证应用的正确运行
- **KubeVirt**：用于 Kubernetes 的虚拟机运行时标准接口
- **Packer**：通过一个 yaml 文件，生成各种虚拟化平台的镜像
- **OpenAPI**：标准化的 API 接口，规范化应用和服务之间的调用



**CI/CD**



持续集成和持续部署主要用于自动化地处理所有的 ops 的工作，包括从代码提交一直到应用部署到线上的整个过程的自动化。CI 侧重于构建和测试，CD 侧重于部署。



- Jenkins 算是这个领域的翘楚，非常经典的一款软件，功能强大稳定，拥有很丰富的插件，算是开源界使用比较广泛的工具
- Travis CI 为开源的 GitHub 项目免费，对私有项目收费，因此很多 GitHub 上项目能看到它的身影
- CircleCI、 Codeship、Shippable、Semaphore 都是 PaaS 产品，提供在线的 CI、CD 服务，一般提供免费和企业收费两种版本
- Bamboo 是 Atlassian 公司（开发了著名的 Jira 和 Confluence）旗下的产品，当然也是商业化的，需要付费才能使用
- Drone：原生支持 Docker 的 CI 开源产品，使用 Go 编写，整个工作流都是基于 Docker 的，最终也会自动化构建 Docker 镜像，push 到 Registry 上
- GitLab Runner：GitLab 提供的 CI 工具，GitLab CI 和 GitLab Runner 一起工作，前者做控制，后者实际执行任务
- Spinnaker：开源的 CI 软件，可以在多个云平台运行构建和部署过程



网络也有很多文章对 CI/CD 的软件进行比较，比如这篇 《Jenkins vs travis vs teamcity vs codeship vs gitlab vs bamboo[5]》，其他的工具就不一一介绍了，感兴趣可以自行搜索。



总结

![Image](https://mmbiz.qpic.cn/mmbiz_png/b2YlTLuGbKDsbJzupnILVFhPtMaRjmvPKYRqTMjibE9pnd8oiawLVrQbOHQe4wBXkBQkzpKCWPKBqWgOLgwccBug/?tp=webp&wxfrom=5&wx_lazy=1&wx_co=1)



如果做个总结的话，2017 年 CNCF 云原生生态目前有如下的趋势：



- 容器的标准化工作正在逐步完善，不会有太多新的功能出现，但是这方面的东西作为整个体系的核心仍旧非常重要
- 容器调度和编排平台的核心作用日渐突出，目前看来 Kubernetes 是领先者，未来和其他竞争产品差距会进一步拉大。个人看法，**Kubernetes 将会窃取 Docker 的果实，成为整个系统最大的受益者**
- 网络和存储有大量的产品和技术存在，但是目前没有统一的标准和绝对的领先者。如何把这两块功能更好地和容器结合，需要新的突破
- 监控和日志是容器平台运维的重中之重，云原生建设降低了应用部署、升级、构建、测试的难度，但是把难度下沉到容器平台这块，原来的运维工具和思路需要变化以适应新的容器平台，了解集群中正在发生的事情、及时发现可能出现的问题，才能保证业务应用稳定高效地运行
- 微服务怎么更好地和容器集群技术结合，是目前非常热门的一个话题，因此 Istio、Convoy、Linkerd 这些技术发展迅速，也被很多人看好，认为是下一代微服务
- Serverless 作为容器更高一层的封装，将会逐步进入我们的视野，继容器（Container）之后，应用（Application）的概念将会发生新的变化和革命
- 应用仍旧是最终目的。保证应用快速稳定地测试、构建、部署、升级，支持；减少代码开发时间人力成本；快速响应业务需求；降低应用的运维和使用成本……这些目标多久都不会变化



CNCF 列出的生态只是一个参考，很多软件和公司并没有出现在这里，在构建云原生应用时不必拘谨于此。构建云原生架构是一个过程，不同的阶段会选择不同的工具和平台，并没有完全"标准"的做法。



另外一个没有出现在这里，但是随平台规模增长变得越发重要的话题是性能分析和优化，因为这个话题并没有统一的标准和方案，只能具体问题具体分析，而且整个过程比较复杂，所以很少有提供一站式方案的软件和公司。



每个特定的问题都有多个工具和解决方案，这样的情况就要求我们必须做出选择，知道每个工具要解决什么问题，有哪些取舍，和其他组件是否容易集成，社区活跃度……然后根据自身的情况和需求，找到最适合自己当前发展的工具。切不可一心求新求全，不然会带来严重的后果：



- 社区并不友好或者活跃，对用户需求响应很慢
- 选择没过一年就停止了开发，只能重新选择新的工具
- 工具因为开发过快或者组件复杂等原因不够稳定，在使用过程中遇到很多问题，维护成本很高
- 选择的工具栈过大，完全超过了自己现在的问题，导致需要额外的人员来维护这些多余的功能
- ……



推荐的方式是循序渐进，以满足最核心需求为主去选择合适的技术，优先使用比较稳定、文档丰富和社区活跃的。充分了解选择版本哪些功能是非常稳定可以直接使用的；哪些功能不太稳定，但是可以在开发和测试环境中小规模使用的；哪些功能是不稳定，需要测试开发或者等待社区进度的。如果有应用需要从旧环境迁移，编写自动化工具，并提供回滚的功能，以灰度发布的方式逐步迁移到容器集群。对于新技术，可以花时间去跟进，在合适的时候及时引进。



切不可听到别的公司使用了某个技术，自己就一定要用。如果没有问题要解决，引入新工具只会带来新的问题而已。



**参考资料**



- Cloud Native Landscape Celebrates First Anniversary[6]
- Monitoring in the time of cloud native[7]
- Introducing Redpoint’s FaaS Landscape[8]



相关链接：



1. https://github.com/cncf/landscape
2. https://www.intigua.com/blog/puppet-vs.-chef-vs.-ansible-vs.-saltstack
3. https://www.upguard.com/articles/the-7-configuration-management-tools-you-need-to-know
4. http://cizixs.com/2017/11/05/oci-and-runc
5. https://blog.takipi.com/jenkins-vs-travis-ci-vs-circle-ci-vs-teamcity-vs-codeship-vs-gitlab-ci-vs-bamboo/
6. https://medium.com/memory-leak/cloud-native-landscape-celebrates-first-anniversary-69a4eb829505
7. https://medium.com/@copyconstruct/monitoring-in-the-time-of-cloud-native-c87c7a5bfa3e
8. https://medium.com/memory-leak/this-year-gartner-added-serverless-to-its-hype-cycle-of-emerging-technologies-reflecting-the-5dfe43d818f0



原文链接：http://cizixs.com/2017/12/30/cncf-cloud-native-landscape

---

CNCF（Cloud Native Compute Foundation） 是 Linux 基金会旗下的一个组织，旨在推动以容器为中心的云原生系统。从 2016 年 11 月，CNCF 开始维护了一个名为 [Cloud Native Landscape](https://github.com/cncf/landscape) 的 repo，汇总目前比较流行的云原生技术，并加以分类，希望能为企业构建云原生体系提供参考。

2017 年 12 月 06 日，Landscape 的 v1.0 版本发布，本文就按照下面这种图介绍云原生系统的大致情况。

[![006tNc79gy1fmctola4a6j31kw0w0npf.jpg](http://dockone.io/uploads/article/20171229/0f10e302758a2be3ed85ea5ff047b003.jpg)](http://dockone.io/uploads/article/20171229/0f10e302758a2be3ed85ea5ff047b003.jpg)


云原生以容器为核心技术，分为运行时（Runtime）和 Orchestration 两层，Runtime 负责容器的计算、存储、网络；Orchestration 负责容器集群的调度、服务发现和资源管理。

往下是基础设施和配置管理，作为容器底层的基石。容器可以运行在各种系统上，包括公有云、私有云、物理机等；容器还依赖自动化部署工具、容器镜像工具、安全工具等运维系统才能工作。

往上是容器平台上的应用层，类似于手机的 App Store，图中分为数据库和数据分析、流处理、SCM 工具、CI/CD 和应用定义几类，每个公司根据业务需求会有不同的应用体系。

右边有两块：平台和观察分析。平台是指基于容器技术提供的平台级的服务，比如常见的 PaaS 服务和 Serverless 服务。观察分析是容器平台的运维，从日志和监控方面给出容器集群当前的运行情况，方便分析和 debug。

**NOTE**：因为图中给出的软件很多，所以文中会挑选一些比较有名的以及本人比较熟悉的介绍，会略过一些信息；此外，也因为个人的水平有限，并没有对所有产品都一一使用过，因此有些内容未免有偏颇或者错误之处，如果读者发现，还望能不吝指出。

### 1. Cloud（云）

容器需要运行在操作系统上，系统可以运行在虚拟机或者物理机上。从使用方式上来分，操作系统这层（Iaas）可以分为公有云和私有云。

#### 公有云

公有云国外以亚马逊 AWS、微软 Azure、谷歌 GCP、DigitalOcean 为代表，国内有阿里云、腾讯云、华为云，此外 IBM、Oracle、Fujitsu 都有自己的云产品，Joyent 也是国外很有名的云计算公司；Packet 是物理机云服务商，直接为用户提供物理机资源。

企业一般会选择其中一个平台来使用，也有不少企业同时选择两种或者多种云服务商，以提高可用性和避免厂商锁定。

#### 私有云

私有云是指用户在自己的数据中心搭建的云服务，除了自己研发之外，常见搭建私有云的方法有 VMware（商业化的虚拟化软件） 和 OpenStack（Python 编写的开源 IaaS 平台软件）；此外 MaaS 提供物理机自动安装和管理服务，分为免费版和收费版；Foreman 是虚拟机和物理机的系统配置工具。

建设私有云的成本很高，但是当公司成长到一定规模的时候，私有云建设也是必要的一件事。除了能缩减成本，也能提高技术实力，而且也有更多的灵活性满足内部的各种需求。

### 2. Provisioning（部署）

有了物理机和虚拟机，在运行容器服务之前，我们还需要为容器准备标准化的基础环境，以及保证基础设施的自动化，拿盖房子来比较，IaaS 和这部分共同组成了容器平台的地基。

#### Host Management / Tooling

自动化配置工具，保证容器运行的系统配置的一致性，并提供升级、补丁等功能，一般也可以用来 Bootstrap 容器服务。

这里的几家软件功能大同小异：

- `Ansible` 比较简洁，用 SSH 来自动化部署，使用 Python 编写
- `CFEngine` 是这个领域非常老的工具，可以说是配置管理的元老，用 C 编写，因此性能会更好，但是学习曲线也更曲折
- `Chef` 用 Ruby 编写，而且配置文件格式也是 ruby DSL，因此对于 Ruby 程序员非常亲切友好
- `SaltStack` 采用 ZeroMQ 作为消息队列，实现 master-salve 模式，兼具性能和灵活性，但同时整个系统也更加复杂
- `Puppet` 是这个领域的老大哥，以成熟稳定著称，社区文档也更丰富


[这篇博客](https://www.intigua.com/blog/puppet-vs.-chef-vs.-ansible-vs.-saltstack)和[这篇文章](https://www.upguard.com/articles/the-7-configuration-management-tools-you-need-to-know)比较了 CFEngine vs Puppet vs Chef vs Ansible vs Salt 这几个工具的异同，如果纠结如何选型，推荐阅读。

其实，对于大多数需求，根据开发语言、配置文件风格等选择其中一种就行。

#### Infrastructure Automation

IaaS 平台提供了基础设施服务，但是对于复杂的场景来说，直接使用这些服务提供的接口还是会很麻烦，所以有了基础设施自动化。这部分做的事情就是能够让基础设施的配置自动化，一次完成多个资源的部署，提高效率。

- `Bosh`：CloudFoundry 旗下的产品
- `Cloudify`：云应用编排系统，能够让用户定义软件，然后部署到不同的云环境中
- `CloudFormation`：AWS 提供的基础配置服务，能够通过配置文件定义要创建的各种 AWS 服务，然后一键完成集群或者系统的搭建
- `Ubuntu Juju`：Ubuntu 提供的管理工具，能够自动化把几百种服务部署到不同的平台
- `Terraform`：HashiCorp 旗下的基础设施配置工具，通过定义一份配置文件，Terraform 能够在不同云提供商运行服务，是 Infrastructure as Code 的信奉者
- `Manage IQ`：统一管理云主机、容器、网络、存储的 IT 平台
- `Kubicorn`：管理多个 Kubernetes 集群的工具，集群可以在不同的云上
- `Helm`：Kubernetes 软件包安装工具，能够安装多个 Kubernetes 资源，类似于 Ubuntu 的 APT 工具


总的来说，这些工具就是在云平台或者 Kubernetes 平台上再封装一层，让用户能够通过一次定义，在不同平台部署多个资源或者服务，并做到版本升级和跟踪。如果云平台提供相关服务（比如 AWS 的 CloudFormation）直接使用即可，如果是混合云，则需要选择 Juju、Terraform 这样的管理工具。

#### Container Registries

容器的镜像 Registry 是容器平台的基础需求，毕竟所有的容器应用就是通过镜像来定义的，镜像服务分为自建和公有服务两种。

很多公司提供了它们公开的容器 Registr 服务，比如 Docker 官方的 Registry，亚马逊 ECR（Elastic Container Registry）、Azure 和 Google 云 Registry、此外 Quay、Project Atomic、JFrog Artifactory 也是比较著名的容器镜像服务提供商。

Harbor 是开源的企业级容器镜像管理平台，Portus 专门为 Docker Registry 提供授权服务。

国内一般企业会选择自建 Registry，因为国外的 Registry 访问速度都很慢，而国内并没有非常流行的 Registry 服务（当然很多容器服务公司都会提供 Registry 服务），另一方面自建 Registry 的技术并不复杂。

#### Secure Images

随着镜像和容器标准化的完善，镜像和容器的安全也越来越受到企业的关注。虽然在大多数情况下，安全往往是软件开发者最后才关心的事情，但是容器安全却是不容忽视的一个环节。

[Notary](https://github.com/theupdateframework/notary) 和 [TUF](https://github.com/theupdateframework/tuf)（the update framework） 是 CNCF 旗下的两个项目，TUF 是开源的安全和验证标准，Notary 是它的一个实现，Notary 可以用来验证镜像的安全性，也可以用来安全地发布软件包。

- [Clair](https://github.com/coreos/clair)：CoreOS 开源的容器安全性分析工具
- Twistlock 是云原生系统的安全性平台
- NeuVector 是网络安全分析工具
- Aqua 也是容器安全平台，提供镜像、容器、用户认证、网络流量限制等多种安全功能
- Anchore 提供了一系列容器环境安全分析、审查和扫描工具



#### Key Management

和安全相关的另一个问题是机密信息，包括密码数据、密钥等。

Keywhiz、Pinterest 开源的 Knox、Lyft 开源的密码存储工具 Confidant 和 HashiCorp 开源的 [Vault](https://www.vaultproject.io/) 想要解决机密信息的存储，它们通过加密的方式把内容保存到后端存储中，而且提供了 Auditing 等额外功能。

[SPIFFE](https://spiffe.io/) 和 [SPIRE](https://spiffe.io/spire/) 是一对的，SPIFFE 定义了服务的认证标准和认证信息的标准，SPIRE 是它的一个实现，但是目前还没有达到生产可用。

### 3. Runtime（运行时）

容器运行时这块是容器核心的技术，关注的是主机容器的技术模块，分为计算、存储、网络三块。

#### Container Runtime（容器运行时）

我们知道，整个容器技术就是因为 Docker 的出现才变得炙手可热，可以说 Docker 重新定义了容器，也成为了容器技术的代名词。但是随着容器的标准化，Docker 把核心组件抽象出 containerd，作为容器运行时，而更多公司也推出自己的容器实现，**容器**一词的含义开始扩展，而且也逐渐标准化了。

随着容器运行时的稳定，普通用户对其关注会逐渐下降。如果把运行时比作内核，那么容器调度系统就是操作系统，开发者应该更关心操作系统的功能和性能，只有遇到特殊需求或者问题时才会关注内核。

OCI（Open Container Initiative）是一个促进容器标准化的组织，主要工作是容器 Runtime 和镜像的标准化协议，这部分内容可以参考我[之前的介绍文章](http://cizixs.com/2017/11/05/oci-and-runc)。

- Containerd：Docker 公司从原来的 Docker 引擎中剥离出来的容器核心功能，具有镜像管理和容器隔离两大功能，底层使用 runC
- Rkt：CoreOS 公司提出的容器引擎技术，一直作为 Docker 的直接竞争对手存在，对于促进容器标准化贡献很大
- [LXD](https://github.com/lxc/lxd)：基于 Linux 老牌容器管理工具 LXC，旨在提供更好用的接口，最大的特色是使用容器技术提供类似虚拟机的服务
- [runV](https://github.com/hyperhq/runv)：以兼容 OCI 接口的方式管理虚拟机，支持 KVM、Xen、QEMU 等虚拟化技术。换句话说，可以直接把虚拟机作为 Runtime 运行在容器集群管理平台上
- Intel Clear Containers：Intel 公司推出的容器技术，因为爸爸的缘故最近也开始出现在容器圈各种文章里


CRI-O 是 Kubernetes 推出的东西，它是 kubelet 和 OCI Runtime 之间的桥梁，它内部采用 OCI 标准，因此可以兼容各种 Runtime（比如 runC、Clear Container等），对上它提供 CRI 接口供 kubelet 调用。这样的话，CRI-O 的存在能够让 kubelet 使用任何 OCI 兼容的 Runtime，从而绕过 Docker、Rkt 这种完整容器管理工具。

#### Cloud Native Storage（云原生存储）

容器从一出现就非常契合微服务中无状态服务的设计理念，因此初期甚至给了一些人容器只适合无状态服务的印象，但是随着容器技术的成熟和用户理念的变化，容器目前已经全面进入有状态服务领域。因为容器存活时间很短的特点，容器的状态（存储的数据）必须独立于容器的生命周期，也因为此，容器的存储变得非常重要，**云原生存储**这块介绍了很多相关的技术。

作为 IT 领域的核心技术，存储早在容器火起来之前就已经有发展了很多年，从单机的各种文件系统、到网络存储，再到现在比较热门的分布式存储、以及云计算催生的块存储、文件存储、对象存储，不同需求不同分类的存储早就五花八门了：

- Ceph：分布式存储系统，同时支持块存储、文件存储和对象存储，发展时间很久，稳定性也得到了验证。之前在 OpenStack 社区被广泛使用，目前在容器社区也是很好的选项。
- GlusterFS：RedHat 旗下的产品，部署简单，扩展性强。
- Hadoop HDFS：Apache 基金会下的开源文件系统项目，在大数据领域广泛使用，基于 GFS 理念的开源版本。主节点保存元数据，并负责数据分布、复制、备份决策，工作节点存储数据，每个数据会有多个副本，保存在不同的工作节点。
- SheepDog：起源于日本的块存储开源项目，设计之初是为了用于虚拟化平台 QEMU。
- Swift：OpenStack 项目提供的对象存储工具。
- LeoFS：高可用、分布式的对象存储系统，海量数据支持比较好，提供 HTTP、S3、NFS 等接口访问。
- Minio：开源的对象存储软件，提供和 AWS S3 兼容的接口，简单易用。


除了这些开源的存储技术之外，还有很多容器存储圈的技术公司：

- DELL EMC：商业存储的典范，提供企业级各种需求的存储解决方案，作为商业存储的大哥，自然也会在容器存储上发力。
- NetApp：致力于混合云的存储方案，是一家老牌的公司，在存储行业深耕多年。
- Datera：一家存储创业公司，主要产品是 EDF（Elastic Data Fabric），提供 API 优先的企业级存储方案，有纯软件和一体机两种不同的版本。
- Diamanti：Diamanti 一家超融合基础设施初创公司，主要为企业数据中心提供基于容器的硬件及软件支持服务。
- Hedvig：为私有云和混合云提供统一的数据存储服务，为虚拟机和容器提供软件定义存储。
- [Infinit](https://infinit.sh/)：开源的软件定义存储公司，之前是做文件跨平台传输的。产品也是统一的存储平台，为各种计算平台提供块存储、对象存储和文件存储等接口。已经被 Docker 收购。
- Pure Storage：一家明星存储创业公司，最大的特定是对闪存的支持
- StorageOS：为容器提供分布式存储的统一视图，对上层提供 API 实现存储的自动化管理，作为容器部署。产品也分为免费版和收费版。
- Quobyte：数据中心文件系统，被 Kubernetes Volume 插件直接支持。


因为不同用户对存储需求不同，采取的存储方案也不同，不管是开源方案、商业方案还是自研方案，或者是文件存储、对象存储还是块存储，怎么把这些技术用到容器平台，以及保证标准化和统一化的接口，是非常有挑战性的事情，目前也有很多努力：

- [CSI](https://github.com/container-storage-interface/spec)（Container Storage Interface）：定义云应用调度平台和各种存储服务接口的项目，核心的目标就是存储 provider 只需要编写一个 driver，就能集成到任何的容器平台上。
- libStorage：EMC 旗下研发的一个存储开发框架，旨在开发与容器平台无关的存储框架，大致的思想是 libStorage 来处理和容器平台的交互，存储框架只需要接入到该框架就行。
- REX-Ray：基于 libStorage 实现的存储管理平台，支持大部分的存储提供商，也能运行在大多数操作系统上。
- OpenSDS：开放的软件定义存储标准，集合各大存储厂商，提供统一管理的存储标准，隶属于 Linux 基金会。
- [Rook](https://rook.io/)：基于 Ceph 作为后台存储技术，深度集成到 Kubernetes 容器平台的容器项目，因为选择了 Ceph 和 Kubernetes 这两个都很热门的技术，并且提供自动部署、管理、扩展、升级、迁移、灾备和监控等功能，所以很受关注。
- Portworx：针对容器技术打造的，把每个节点的存储资源组成一个存储池，每个数据自动进行备份，并通过和容器平台调度深度集成保证数据高可用。分为免费版和商业版。



#### Cloud Native Network

网络最重要的功能是提供不同计算资源的连通，随着虚拟化和容器技术的发展，传统的网络方案已经无法满足云计算快速增长、不断变化的网络需求。不同用户对网络的要求也越来越高：

- 安全性：保证私有和公有云网络的安全，网络流量能够加密，不被窃听和修改
- 多租户：云计算需要同时为多个租户提供网络服务，不同租户之间互相独立而且隔离
- 快速响应：容器的生命周期大大缩短，集群的网络在实时动态变化，网络方案需要感知网络的变化，并快速提供服务
- 网络迁移：虚拟机和容器会在集群上迁移和调度，网络方案需要支持计算资源跨主机迁移后的连通
- 监控和调试：云上的网络环境，让整个网络的流量变得更加复杂，我们需要新的工具让网络可视化，并做到自动化运维
- ……


因此，在云计算和容器这块涌现出很多网络解决方案和厂商，试图解决这些问题：

- CNI（Container Network Interface）：Kubernetes 和 CoreOS 提出的容器网络接口标准，旨在为容器平台提供统一的网络访问模式，目前很多网络方案都已经集成进来。
- Calico：基于 BGP 的纯三层网络方案，性能很高，并且提供强大的网络防火墙功能，以满足用户对安全性的需求。
- Canal：基于 Flannel 和 Calico 提供 Kubernetes Pod 之间网络防火墙的项目。
- Contiv：思科推出的网络方案，支持 VXLAN 和 VLAN 方案，提供了多租户和主机访问控制策略功能。
- Cilium：利用 Linux 原生技术提供的网络方案，支持 L7 和 L3、L4 层的访问策略。
- Flannel：CoreOS 主要的容器网络项目，主要用于 Kubernetes 平台提供 Pod 之间的连通性，提供多种连网方案，部署和管理简单。
- Midokura：日本 SDN 网络公司，主要产品是开源的 MidoNet，之前广泛用于 OpenStack 中，目前有很多把它应用到容器领域的尝试。
- OpenContrail：Juniper 收购的开源网络虚拟化平台，目前已经加入 Linux 基金会。OpenContrail 是一个可扩展的网络虚拟化控制层，提供了丰富的软件定义网络功能和安全性。
- Open vSwitch：Linux 平台的虚拟交换机软件，除了提供和 Linux 虚拟网桥类似功能外，还支持 OpenFlow 协议。
- Weave Net：Weaveworks 公司开源的 Docker 跨主机网络方案，安装和使用都比较简单，内部也是通过 Overlay 网络实现的
- Romana：Panic Networks 推出的网络开源方案，基于 L3 实现的网络连通，因此没有 Overlay 网络带来的性能损耗，但是只能通过 IP 网段规划来实现租户划分，不够灵活
- Tigera：网络方案初创公司，主推的方案是把 Flannel 和 Calico 集成到一起的 Canal，应用 Calico 的网络策略到 Flannel 中。


也有很多的商业公司为企业提供网络解决方案：

- [Aviatrix](http://www.aviatrix.com/)：混合云网络解决方案提供商，集成 AWS、Azure、Google 等公有云网络，在同一平台管理公司混合云网络。
- Big Switch：下一代数据中心网络公司，提供 SDN 可编程的网络方案，主要有 Big Cloud Fabric 和 Big Monitoring Fabric 两种产品方案。
- VMware NSX：虚拟化厂商 vmware 提供虚拟化网络方案。
- Cumulus：主要产品是 Cumulus 操作系统，继承了众多的网络软件，提供丰富的网络功能。能够解除数据中心网络设备硬件和软件锁定的局面，为网络硬件带来软件的灵活特性。
- NuageNetworks：致力于数据中心 SDN 网络的公司，提供解决方案



### 4. Orchestration & Management（编排和管理）

当在生产环境中使用容器时，单台主机上的容器已经不能满足需求，需要管理多主机容器集群，也就需要有个工具能够提供资源管理、容器调度和服务发现等功能，保证多主机容器能够正常工作。可以说，对于云原生系统，Orchestration 才是最核心的东西。

#### Scheduling & Orchestration

调度和集群管理一直是容器技术的热点领域，竞争也非常激烈。打个可能不那么恰当的比喻，如果把容器 Runtime 比作引擎，那么容器集群管理工具就是汽车。用户购买的是汽车，尽管引擎非常重要，但是它终归只是个可以替换的零件。

集群管理竞争还在，并没有最终的唯一胜利者，但总的来说 Google 公司的 Kubernetes 处于绝对的领先状态，也是目前社区发展最快的平台，随着 Docker 官方支持 Kubernetes，以及 Azure 和 AWS 。目前社区三个主流的容器调度平台是：

- Kubernetes：起源于 Google 内部的 Borg 系统，率先提出 Pod 的概念，提供自动化管理、服务发现、服务升级、有状态服务等等特性
- Docker Swarm：Docker 公司官方的容器管理平台，最大的特点是和 Docker 兼容的 API 和操作命令
- Mesos：Apache 旗下的任务调度平台，后来应用于容器调度


对于公有云上的容器服务，各大云服务商也有对应的产品：

- Amazon ECS：亚马逊推出的容器服务，特点是虚拟机收费，容器免费
- Azure Service Fabric：微软 Azure 的容器服务调度平台
- Nomad：HashiCorp 旗下的数据中心调度平台，同时支持服务和任务两种 Job，也已经支持 Docker 容器



#### Coordination & Service Discovery

有了容器集群管理工具，容器的规模逐渐变多，另外一个需要解决的问题是服务之间怎么互相发现对方。因为集群的容器是不断变化的，IP 地址也是不稳定的。这个问题再往下思考，就是集群的状态应该怎么保存，才能让所有节点能当前集群自己想要的信息，而且保证不会发生冲突和错误。

目前，集群的状态都会保存在一个分布式的键值存储里，这个存储保证数据的一致性，目前三款常用的键值存储工具是：

- ZooKeeper：Hadoop 的一个子项目，本来是作为 Hadoop 集群管理的数据存储，目前也被应用到容器领域，开发语言是 Java。一个缺点是使用和维护比较复杂
- Consul：HashiCorp 开发的分布式服务发现和配置管理工具，Docker Swarm 集群之前默认使用的就是这个
- Etcd：CoreOS 旗下的键值存储工具，是 Kubernetes 默认的选择，因此使用范围很广


有了分布式键值存储保证一致性，还需要工具把集群数据自动写入到里面，并且需要格式化地读取和解析数据。围绕这一话题，周边也有很多工具：

- Registrator：自动监控 Docker 容器，把每个容器的信息（IP、端口等）写入到键值存储中，支持 etcd、Consul
- SkyDNS：基于 etcd 中的数据，对外提供 DNS 数据查询，是对 etcd 的一层封装。因为使用 etcd，所以 DNS 查询是实时的，避免了缓存导致的问题
- CoreDNS：SkyDNS 继承者，主要特点是插件系统能完成各种各样的功能
- ContainerPilot：Joyent 开源的容器服务发现工具，作为容器的 init 系统运行，通过定义一个 JSON 文件，它会把容器相关的信息更新到 Consul 中、进行健康检查、运行用户定义的代码等


除外，还有两个公司开源的服务发现工具要提一下：

- SmartStack：Airbnb 开源的服务发现工具，由 Nerve 和 Synapse 组成，安装和运维相对复杂了些
- Netflix OSS Eureka：Netflix 开源的用于 AWS 的服务发现工具


总的来说，这些工具保证这个集群的动态信息能统一保存，并保证一致性，这样每个节点和容器就能正确地获取到集群当前的信息。

#### Service Management

伴随着容器技术而变得火热的一个话题是微服务，每个服务作为容器或者 Pod 运行，不同服务之间通过服务发现知道对方的地址进行通信。随着集群规模的增大、服务数量的增多，用户的需求也不断增加，微服务架构也面临更多的问题：

- 认证和安全：为了安全，调用方需要进行身份认证，而且不同的微服务只能运行不同的用户访问
- 统计：每个微服务需要知道它的使用情况，什么人在什么时候调用了什么接口，方便监控和排查错误
- 健康检查和自动恢复：系统能自动检测服务的可用性，一旦不可用就重启恢复或者从调用链中删除
- 自动重试：如果调用某个服务发生错误，可以自动按照特定算法重试
- 限速：服务应该限制它能接收请求的速率，以保证它不会被过量的请求压垮
- 服务可用性和雪崩：每个服务的可用性都不可能是 100% 的，简单的串联调用会降低整个集群的可用性。如何保证每个服务不可用不会导致调用方的僵死
- 负载均衡：怎么自动把请求分配到不同的后端进行处理，调度算法能否满足各种各样的需求
- 升级发布：每个微服务的升级怎么做到不影响其他服务，怎么进行灰色发布，出错怎么快速回滚
- 测试：单个服务可以独立测试，但是整个集群怎么进行功能和性能测试
- ……


这些东西都是每个微服务平台必须要考虑的，如果放在每个服务代码中实现某些功能，不仅增加了每个服务的复杂性，也会导致重复的工作，所以，微服务需要更好的框架和平台统一提供这些功能。总的来说，微服务虽然降低了单个服务的复杂性，但是把复杂性下沉到微服务管理平台层面。

针对这些问题，有很多软件和方案。对于负载均衡来说，HAProxy、Nginx 和 F5 都是常用的方案，Traefik 是后起之秀，专门为微服务设计；RPC 框架用来在微服务内部进行通信，因为比 HTTP API 效率高而被大量使用，常用的用 Google 开源的 GRPC 、Apache 旗下的 Thrift 框架、Netflix 开源的自带负载均衡的 Ribbon 和 Avra 数据序列化框架。

微服务 Gateway 是统一化管理 API 注册接入、权限认证、限速、日志等功能，是微服务对外的接口。

- Kong：Mashape 开源的项目，基于 OpenResty（Nginx + Lua） 的微服务网关，以插件为中心组织功能
- Netflix Zuul：Netflix 微服务网关，使用 Java 开发，因此适用于 Java 应用，需要添加代码来使用 Zuul 提供的功能
- Nginx：Nginx Plus 产品为企业提供负载均衡、代理、微服务网关的各种功能
- 3scale：红帽公司的 API 网关工具


这个领域也有一些公司在提供产品，比如 Datawire 就专门为 Kubernetes 应用提供 API Gateway 和自动化源码部署的工具。

微服务开发框架 Hystrix 是 Netflix 开源的项目，能够帮助程序处理微服务交互的超时处理和容错的问题，提供熔断、隔离、降级等功能，但是只能用于 Java 语言项目，需要在程序中修改代码。

特别要强调一下微服务领域最近比较热门的概念：**Service Mesh**，它的主要想法是把微服务通用的功能单独抽象为一层，部署在容器管理平台中，对应用透明，并且通过简单自动化的接口来控制整个微服务的连通、灰度访问、升级、降级等功能。

- Linkerd：开源的网络代理服务，使用 Scala 语言编写，最早提出了 Service Mesh 的概念
- Envoy：C++ 编写的网络代理工具，和 Linkerd 的定位相同，Turbine Labs 公司专门提供 Envoy 的部署和管理工作
- Istio：Google、IBM 和 Lyft 联合发布的微服务管理、配置和监控框架，使用 Envoy 或者 Linkerd 作为内部 worker，控制层面负责配置和管理，深度集成到 Kubernetes 平台


Service Mesh 相较于之前微服务框架的最大优势是它对业务是透明的，不需要像 Netflix 提供的很多微服务工具那样对应用有侵入性，因此就不再和任何语言绑定，可以看做整个网络栈的另一个抽象层。

### 5. Platform（平台）

平台这块主要是基于容器技术做的更上面一层的封装，有直接是接管公有云或者私有云的容器平台，也有 FaaS 这一类服务。

#### PaaS/Container Service

因为容器技术的隔离性，以及对应用非常友好，因此可以直接拿来做 PaaS 服务，当然也有种叫法是 CaaS（Container as a service）。很多初创公司的业务也就是这块，基于容器提供应用的发布、升级、运维等管理工作。大部分公司做的事情都大同小异，因为最终的需求是一样的：让应用的开发、部署、扩展、升级、运维更轻松，用户无需关心基础架构，只需要考虑如何去实现业务逻辑就行，主要的区别在于侧重点，有些侧重私有的数据中心部署和管理，有的侧重 Docker 容器的管理，有的测试 Kubernetes 等容器集群的维护，有的提供应用平台，有的和公有云平台深度集成……

Heroku 是老牌的公有云类型的 PaaS 平台，界面友好，成熟稳定，所有操作提供命令行实现自动化，提供完整的监控和日志方案。

Cloud Foundry 和 OpenShift 是两款重要的开源 PaaS 平台，其中 Cloud Foundry 是 Pivotal 开源，支持多种语言、框架、云平台，旨在让开发者能够忽略架构问题，快速部署和扩展应用；OpenShift 是 Red Hat 开源的，功能和 Cloud Foundry 差不多，网络上有很多两者的对比文章，这里不再赘述。目前两者都已经开始拥抱容器、Docker、Kubernetes 技术，希望能和容器深度集成。它们的特点是功能强大，可扩展性强，但是相应的，复杂程度也高。

随着 Kubernetes 的快速发展，很多以 Kubernetes 为容器管理平台和应用管理的公司也都出来了，Datawire 基于 Kubernetes，侧重于微服务的管理；Containership 也是 Kubernetes 的管理平台，可以在多个云平台自动化部署和统一管理；Giant Swarm 提供混合云和多租户的 Kubernetes 管理；Kubermatic 能够给用户提供一键 Kubernetes 集群安装和多集群管理服务；[Gravitational](https://gravitational.com/) 提供多 Regions 的 Kubernetes 集群管理。

Atomist 和 Cloud 66 侧重于 DevOps 流程；Flynn 是基于 Docker 容器技术的开源 PaaS 软件，相比 Cloud Foundry 算是轻量级的实现；Hyper.sh 比较有趣，它们以容器接口来提供虚拟机服务。

其他一些平台提供的服务如下：

- Apcera：一个企业级的容器管理平台，包括了运行容器所需编排、网络和安全功能。Apcera 的一个特点是支持传统的应用，同时兼容传统应用和云原生应用，支持把前者迁移到云上
- Apprenda：PaaS 云平台软件公司，基于 Kubernetes 打造的应用管理平台，目前的商业版本 ACP（Apprenda Cloud Platform）提供了 Kerberos 身份认证、应用审计等额外功能
- Convox：基于 AWS 的应用部署、管理、监控的平台服务，提供了命令行实现任务的自动化
- DC/OS：Mesos 的企业级产品，是一套开源项目，基于 Mesos 分布式系统和 Marathon，提供了编排、应用商店、GUI 界面等功能
- Diamanti 也是一家解决方案公司，基于 Kubernetes 调度平台，同时支持 OpenShift PaaS 平台
- Docker：没有看错，这里的 Docker 指的是 Docker 公司，而不是容器技术。作为一家商业化的公司，Docker 也提供了商业化的产品和解决方案，开源的部分称为 Docker CE（community edition），商业化产品为 Docker EE（Enterprise Edition）
- Mirantis Cloud Platform：原来有名的 OpenStack 公司，目前也逐渐接纳 Kubernetes，一起构建云平台
- Moby project：Docker 公司把开源组件命名成 Moby，意在把多个开源技术组件按照需求组合成满足用户需求的产品，Docker CE 就是其中的产出
- Platform9：同时支持 OpenStack 和 Kubernetes 为核心的 PaaS 服务
- Portainer.io：Docker 的界面化管理工具
- Rancher：容器管理平台，之前同时支持 Swarm、Mesos 和 Kubernetes，目前把重心逐渐迁移到 Kubernetes 上
- Tectonic：CoreOS 推出的 Kubernetes 集群服务，集成了 Quay 镜像服务、CoreOS 系统、和 Prometheus 监控等
- Ubuntu：Ubuntu 系统也内嵌了 LXD 容器技术，提供更多的容器技术


这块内容主要是容器创业公司，提供的都是基于 Docker、Kubernetes 或者其他容器技术的方案，因此做的事情大同小异，就不再一一介绍了，感兴趣的可以根据图中列出的公司自行了解。

#### Serverless/Event-based

容器技术把微服务的概念吵得火热，随后也让 Serverless 这个词出现了大众的面前。既然容器能够屏蔽基础设施，让开发者只关心交付的应用（容器镜像），那么我们可不可以更进一步，让开发者也不要关心交付的镜像，只关注业务的核心逻辑呢？这就是 Serverless 的想法，开发者定义好基于事件触发的业务逻辑，其他一切都交给平台，当用户发出请求或者其他事件发生时，平台会根据事先的配置自动运行响应的业务逻辑代码，从而实现真正的按需服务。如果说容器关心的是应用，那么 Serverless 关心的则是函数。Serverless 不是没有服务器，而是不用关心服务器和系统。

Serverless 是一个很新颖的技术，虽然理念非常好，但现阶段还不适用于所有的应用，主要是因为它的性能问题，以及距离成熟使用还缺少很多工具和方案，另外开发流程要接纳这种理念还需要一段时间。

AWS Lambda 服务算是商业产品中比较成熟的，它的出现让 Serverless 从概念化和实验化的东西变成了可行的方案。微软家的云平台也推出了 Azure functions；Google 家对应的产品叫做 Cloud Functions，从命名来看亚马逊略胜一筹。

OpenFaaS、Fission 和 Kubeless 都是基于 Docker 和 Kubernetes 开源的 Serverless 开发框架，如果要想打造自己的 Serverless 平台可以参考。

- Apex：帮助构建、管理和运行 AWS Lambda 的工具
- NStack 和 Nuclio 都是专门用作数据分析的 FaaS 软件工具
- OpenWhisk：Apache 旗下的 Serverless 框架，目前还是孵化项目
- Oracle Application Container Cloud
- PubNub BLOCKS：PubNub 提供的 Serverless 服务，用于集成到自家的服务推送中
- [Serverless](https://serverless.com/) 是一个集成工具，它能帮助开发者在 Serverless 应该部署到 AWS Lambda、Azure Functions、GCP Cloud Functions、kubeless 等平台，也就是说它封装了这些平台差异，提供了一致的接口，方便迁移和管理多 Serverless 平台应用



### 6. Observability & Analysis（观察和分析）

基于云原生的平台建立起来之后，我们要保证整个平台能够正常工作，保证运行在其上的服务不会因为平台错误而导致不可用，而且还要知道应用整体的运行情况，提前对某些可能出错的地方进行报警，一旦出错能够提供合适的信息便于调试和修复……这些都是观察（observability）和分析（analysis）要做的工作，为了方便下面统一使用**监控**（monitoring）一词。

**NOTE**：对于监控、观察、分析、日志等词语的使用并没有非常严格的定义，监控是 IT 行业比较传统的叫法，表示对应用和系统运行情况的数据统计和展示。目前有个叫法是观察，分为metrics/monitoring、logging 和 tracing 三个部分。为了容易理解，我们使用**监控**一词来代替，它广义地包含了以上所有内容。

监控对于系统来说非常重要，没有监控的平台就像没有仪表盘的飞机。监控的目标有几点：

- 了解系统的使用情况，可以用于容量规划、性能调优
- 提前或者及时发现问题，因为硬件总是会有故障的软件总是有 bug 的，及时发现能够更快处理，不影响到应用的正常运行。这些问题包括网卡不能工作、硬盘老化、或者软件异常等
- 帮助排查错误：当发现软件bug或者硬件故障时，监控能够帮忙分析哪个组件在什么时候出现了问题，方便定位问题



#### Monitoring

简单来说，监控就是了解应用和系统运行情况。

我们最常见的监控是对主机的监控，了解 CPU、内存、磁盘 IO、网络等资源的使用数据，以此了解主机是否正常，是否需要加硬盘或者扩展集群，是否有内存泄露等等。另外一个监控是应用的监控，不管是数据库、缓存服务器、消息队列、代理和缓存服务器，还是自己编写的应用，我们需要知道它们的运行情况，这个监控依据每个应用而定，监控方法、监控的数据以及解读方法对不同的应用来说会千差万别。

而容器的出现，让监控出现了另外一个维度：容器和平台的监控。我们不仅要知道每个主机的运行数据，还需要知道每个容器的数据，这个容器使用的 CPU、内存、磁盘 IO 和网络等，这个新的需求也就催生了新的监控思想和工具。

Zabbix 是老牌的监控工具，功能强大，最近界面也改进了不少；Nagios 和 Graphite 是另外两个经典的监控工具。Sensu 是一款较新的监控工具，Riemann 也能够进行分布式系统的集中式监控处理。

InfluxDB 和 OpenTSDB 都是时序数据库，后者是基于 HBase 的。

AWS CloudWatch 是AWS 自家的监控工具，当然是负责 AWS 云上的服务监控；Azure Log Analytics 是微软家日志监控工具。很多商业公司也提供各种各样的监控产品：AppNeta、Axibase、APPDynamics、Datadog、Dynatrace、NewRelic 和 Splunk 都提供应用级别的监控和数据分析业务。

再介绍一下经常听到的监控工具：

- Prometheus：时序数据库，提供了工具能读取 HTTP 接口暴露的数据保存起来，提供了丰富的查询接口以及一个 Web 界面
- Grafana：监控管理界面，能够从多个数据源导入数据，提供优美的界面和丰富的面板配置
- CoScale：专门为容器和微服务优化的监控平台
- Sentry：错误收集工具，能够集中式地查看应用的 Crash Report 和 Traceback 信息
- Server Density：专注于服务监控的 SaaS 服务平台
- StatsD：Etsy 开源的数据统计信息，可以把数据继续发到后端的监控平台，比如 Graphite
- Sysdig：容器和 Kubernetes 监控工具，同时提供了付费的监控服务


图中列出的监控公司和工具还有很多，这块的创业公司也很多，因为监控的数据不像业务数据那样机密，因此很多公司愿意使用 SaaS 监控服务。

#### Logging

日志容易理解，就是程序运行输出的信息，它们是调试的利器，当程序出错的时候，有效的日志分析能够快速定位问题。同时日志也能承担一部分的监控功能，反应系统运行是否正常。

Fluented 是一个开源的基于数据流（stream）的日志收集框架；Graylog 是另外一个开源的选择，它们的思想都是把日志从系统各处收集起来进行统一分析、过滤和管理；Elastic 提供 ELK（Elasticsearch、Logstash、Kibana） 技术栈负责日志收集，也提供在线的企业级 SaaS 服务。

除了使用开源的组件自己搭建日志服务平台，还可以使用一些公司提供的在线日志服务，只要把日志数据导入到它们的平台，不用关心日志服务的维护工作。Loggly 是一个在线的日志分析服务，需要付费使用；logz.io 提供管理的 ELK 在线服务，提供 ELK as a service，并且可以基于 AI 对数据进行分析；另外一家声称支持 AI 分析的日志服务公司是 loom systems；Splunk 这家公司也提供日志分析服务；PaperTrail、Sematext、SumoLogic 也都提供类似的日志分析服务。

#### Tracing

随着微服务的采用，用户的一个请求可以中间会经过多个不同的微服务才最终得到应答。传统的监控、日志都是针对单个组件的分析，用来了解当前组件的健康和运行状态，并不能给我们一个整体的动态的情况。

对于微服务来说，我们需要知道一个请求到底经过了哪些组件，每个组件耗费了多少时间，错误发生在中间的哪个步骤，每次调用的网络延迟是多少等等。对于使用不同语言、开发框架、数据库、和系统的微服务，我们需要有统一的跟踪标准，这就是 tracing 要做的事情。分布式的 Tracing，一般都受到 Google Daper 系统的设计影响。

OpenTracing是一个开放的 Tracing 接口标准和文档，提供了各种语言客户端的实现，支持的 Tracing 工具包括 Jaeger、Appdash 和 Zipkin（Twitter 开源）；Cloud Sleuth 是 Spring Cloud 全家桶中一员，主要负责 Tracing 功能。

这些 Tracing 工具都需要在应用中编写对应的代码，和 Logging 和 Metrics 类似，用户可以自定义要 Tracing 代码块的范围和父子关系。此外，也有很多工具会自动化嵌入服务组件之间的 Tracing 数据，比如之前讲过的 Istio。

Tracing 可以和 Metrics 结合一起使用，Tracing 负责组件和微服务之间的数据分析，Metrics 负责单个组件内的性能数据监控。

### 7. Application（应用）

容器平台最终还是要跑应用的，最主要的应用当然是各个公司的业务应用，除此之外还有一些比较通用的应用，这个表格里也有列举，可以根据需求提供类似应用市场的功能。

#### Database & Data Analytics

数据库一直是软件领域的核心组件，所以包括了很多老牌的数据库软件，比如 MySQL、DB2、Oracle DB、PostgreSQL、MariaDB 等关系型数据库。基于这些传统的数据库也有很多周边的工具和软件，比如 Vitess 这种数据库集群工具；阿里巴巴开源的 SQL 数据库连接池工具 Druid，它还同时提供了监控功能。

NoSQL 的发展也让很多新型的数据库出现在了我们的视野：有 Redis、Couchbase 这一类的 KV 数据库；也有 MongoDB、RethinkDB、RavenDB 为代表的文档类数据库；Cassandra、Hbase、Vertica（列式关系数据库）、Scylla（KVM 之父的作品，旨在成为下一代的 Cassandra 数据库） 这样的 Column 数据库。它们最重要的特点是能够轻松进行集群扩容，不支持 SQL 查询，因此接口上都以其他形式满足用户各种各样的数据需求。

后来 newSQL 的概念出现，旨在结合 SQL 和 NoSQL 的优势，还是以 SQL 方式使用，但同时支持快速横向扩容和分布式事务，Google 内部的 F1/Spanner 是这方面的先驱，发过论文但是没有把项目开源，CockroachDB 和 TiDB 是这类数据库的代表，都是开源项目。

其他相关的数据库产品包括：

- BigchainDB：区块链数据库
- CarbonData：面向大数据平台的列数据文件存储格式，由国内的华为公司贡献给 Apache 基金会
- Crate.io：基于 Elasticsearch 的分布式 SQL 数据库，适用于需要快速搜索和聚合的查询场景
- MemSQL：从名字也能看出来，这是一款内存数据库，特点自然是性能高，
- Noms：采用 Git 思想的支持版本控制、fork和同步的数据库
- Pachyderm：旨在成为一个更现代的大数据处理平台，资源调度基于 Docker 和 Kubernetes，底层是自己的分布式存储系统
- iguazio 和 Qubole：自动化数据分析公司
- SQL Data Warehouse：Azure SQL 数据库产品


数据库是整个软件技术的基础，现在有个很流行的观念是：数据是一家公司最重要的资产之一。我们对数据库的要求也是更快、更多、更好，所以会有很多数据库相关的产品来解决各种各样的数据存储和处理需求。

#### Streaming

流处理与批处理对应，要求对海量数据的实时采集、计算和查询，很多业务场景要求尽可能快得对数据进行分析，从而做出决策，比如传感器、流量监控、股市行情、游戏数据分析等，对此类需求也催生了很多实时处理工具。

Kinesis 是亚马逊官方的流数据处理平台，是其云计算产品的一部分；Cloud Dataflow 是 Google 的流处理产品；开源方面，Apex 是 Apache 旗下开源的新型实时数据处理平台；Heron 是 Twitter 开源的数据处理平台，是 Apache Storm 的继承者；Spark 和 Flink 支持流处理的同时也支持批处理操作，两者定位非常相似，但内部实现的差距挺大

Kafka 和 RabbitMQ 这类消息队列可以做到数据的快速收集；NiFi 是另一个 Apache 项目，是一个数据整合和分发系统，专门为数据流设计。

和大数据一样，流处理这个领域也是 Apache 占据了大半的江山。

#### SCM（Source Code Management）

虽然容器让产品以镜像的作为产出，但是代码还是要维护的，最有名的社区源码管理平台自然是 GitHub，GitLab 是开源自建 SCM 的推荐选择，Bitbucket 和 GitHub 定义类似，提供免费也提供商业版本的服务。

#### Application Definition

应用定义并没有明确的定义，大概要做的事情是屏蔽底层基础设置、云平台以及容器运行时，封装一个标准化的应用定义，从而实现应用自动化运行在任何地方。

- Apache Brooklyn：应用管理平台，可以通过简单的操作把应用部署到常用的云平台
- Docker Compose：定义多个容器的运行关系，Docker Compose 可以自动化管理这些容器的构建、生命周期、和网络连通等问题
- Habitat：应用自动化管理平台，可以定义应用，并且提供 Supervisor 来保证应用的正确运行
- KubeVirt：用于 Kubernetes 的虚拟机运行时标准接口
- Packer：通过一个 yaml 文件，生成各种虚拟化平台的镜像
- OpenAPI：标准化的 API 接口，规范化应用和服务之间的调用



#### CI/CD

持续集成和持续部署主要用于自动化地处理所有的 ops 的工作，包括从代码提交一直到应用部署到线上的整个过程的自动化。CI 侧重于构建和测试，CD 侧重于部署。

- Jenkins 算是这个领域的翘楚，非常经典的一款软件，功能强大稳定，拥有很丰富的插件，算是开源界使用比较广泛的工具
- Travis CI 为开源的 GitHub 项目免费，对私有项目收费，因此很多 GitHub 上项目能看到它的身影
- CircleCI、 Codeship、Shippable、Semaphore 都是 PaaS 产品，提供在线的 CI、CD 服务，一般提供免费和企业收费两种版本
- Bamboo 是 Atlassian 公司（开发了著名的 Jira 和 Confluence）旗下的产品，当然也是商业化的，需要付费才能使用
- Drone：原生支持 Docker 的 CI 开源产品，使用 Go 编写，整个工作流都是基于 Docker 的，最终也会自动化构建 Docker 镜像，push 到 Registry 上
- GitLab Runner：GitLab 提供的 CI 工具，GitLab CI 和 GitLab Runner 一起工作，前者做控制，后者实际执行任务
- Spinnaker：开源的 CI 软件，可以在多个云平台运行构建和部署过程


网络也有很多文章对 CI/CD 的软件进行比较，比如这篇 《[Jenkins vs travis vs teamcity vs codeship vs gitlab vs bamboo](https://blog.takipi.com/jenkins-vs-travis-ci-vs-circle-ci-vs-teamcity-vs-codeship-vs-gitlab-ci-vs-bamboo/)》，其他的工具就不一一介绍了，感兴趣可以自行搜索。

### 总结

如果做个总结的话，2017 年 CNCF 云原生生态目前有如下的趋势：

- 容器的标准化工作正在逐步完善，不会有太多新的功能出现，但是这方面的东西作为整个体系的核心仍旧非常重要
- 容器调度和编排平台的核心作用日渐突出，目前看来 Kubernetes 是领先者，未来和其他竞争产品差距会进一步拉大。个人看法，**Kubernetes 将会窃取 Docker 的果实，成为整个系统最大的受益者**
- 网络和存储有大量的产品和技术存在，但是目前没有统一的标准和绝对的领先者。如何把这两块功能更好地和容器结合，需要新的突破
- 监控和日志是容器平台运维的重中之重，云原生建设降低了应用部署、升级、构建、测试的难度，但是把难度下沉到容器平台这块，原来的运维工具和思路需要变化以适应新的容器平台，了解集群中正在发生的事情、及时发现可能出现的问题，才能保证业务应用稳定高效地运行
- **微服务**怎么更好地和容器集群技术结合，是目前非常热门的一个话题，因此 Istio、Convoy、Linkerd 这些技术发展迅速，也被很多人看好，认为是下一代微服务
- **Serverless** 作为容器更高一层的封装，将会逐步进入我们的视野，继容器（Container）之后，应用（Application）的概念将会发生新的变化和革命
- 应用仍旧是最终目的。保证应用快速稳定地测试、构建、部署、升级，支持；减少代码开发时间人力成本；快速响应业务需求；降低应用的运维和使用成本……这些目标多久都不会变化


CNCF 列出的生态只是一个参考，很多软件和公司并没有出现在这里，在构建云原生应用时不必拘谨于此。构建云原生架构是一个过程，不同的阶段会选择不同的工具和平台，并没有完全"标准"的做法。

另外一个没有出现在这里，但是随平台规模增长变得越发重要的话题是**性能分析和优化**，因为这个话题并没有统一的标准和方案，只能具体问题具体分析，而且整个过程比较复杂，所以很少有提供一站式方案的软件和公司。

每个特定的问题都有多个工具和解决方案，这样的情况就要求我们必须做出选择，知道每个工具要解决什么问题，有哪些取舍，和其他组件是否容易集成，社区活跃度……然后根据自身的情况和需求，找到最适合自己当前发展的工具。切不可一心求新求全，不然会带来严重的后果：

- 社区并不友好或者活跃，对用户需求响应很慢
- 选择没过一年就停止了开发，只能重新选择新的工具
- 工具因为开发过快或者组件复杂等原因不够稳定，在使用过程中遇到很多问题，维护成本很高
- 选择的工具栈过大，完全超过了自己现在的问题，导致需要额外的人员来维护这些多余的功能
- ……


推荐的方式是循序渐进，以满足最核心需求为主去选择合适的技术，优先使用比较稳定、文档丰富和社区活跃的。充分了解选择版本哪些功能是非常稳定可以直接使用的；哪些功能不太稳定，但是可以在开发和测试环境中小规模使用的；哪些功能是不稳定，需要测试开发或者等待社区进度的。如果有应用需要从旧环境迁移，编写自动化工具，并提供回滚的功能，以灰度发布的方式逐步迁移到容器集群。对于新技术，可以花时间去跟进，在合适的时候及时引进。

切不可听到别的公司使用了某个技术，自己就一定要用。如果没有问题要解决，引入新工具只会带来新的问题而已。

#### 参考资料

- [Cloud Native Landscape Celebrates First Anniversary](https://medium.com/memory-leak/cloud-native-landscape-celebrates-first-anniversary-69a4eb829505)
- [Monitoring in the time of cloud native](https://medium.com/@copyconstruct/monitoring-in-the-time-of-cloud-native-c87c7a5bfa3e)
- [Introducing Redpoint’s FaaS Landscape](https://medium.com/memory-leak/this-year-gartner-added-serverless-to-its-hype-cycle-of-emerging-technologies-reflecting-the-5dfe43d818f0)


**原文链接：[http://cizixs.com/2017/12/30/c ... scape](http://cizixs.com/2017/12/30/cncf-cloud-native-landscape)**