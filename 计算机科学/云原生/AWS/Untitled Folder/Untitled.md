[TOC]

https://zhuanlan.zhihu.com/p/360701963

## **简介**

长文多图预警，看结论可以直接拖到“总结”部分

本文及下一篇文章介绍以下AWS基础概念或服务。

- Region
- AZ（Availability Zone）
- VPC（Virtual Private Cloud）
- Subnet
- Security Group
- VPC Endpoint
- EC2
- IGW（Internet Gateway）
- Route Table（RT）
- EIP （Elistic IP）
- NAT gateway
- SSM （System manager）
- Security Group（SG）

以上概念除EC2，SSM和VPC Endpoint外，都是AWS中的基础概念，在大多数AWS应用中都会遇到这些内容。

比如之前几篇关于 Fargate 的文章中就反复提到了 VPC, Subnet，Security Grup 等概念。

然而在生产环境中，按功能分离，权责分离的原则，VPC 的创建，Subnet 的划分，Security Group 的设定，很多时候是由专门的小组来完成的。

一般用户可能只会直接利用已经配好的环境进行操作，导致对上述基本概念并不是很清楚。

这非常不利于以后进一步学习利用 AWS 服务。

本文及随后的文章，面向对 AWS 有兴趣或者刚入门 AWS 的朋友，力图解释清楚这些概念之间的关系。

通过实战创建 VPC，Subnet，Security group 等常用组件来加深理解。

在 Pulic Subnet, Private Subnet 中分别创建 EC2，然后通过SSM登录到 EC2 中实际测试不同的网络配置。

本文包含以下内容，主要是搭建环境。测试部分将在下一篇文章里介绍。

- Region
- AZ（Availability Zone）
- VPC（Virtual Private Cloud）
- Subnet
- Security Group
- VPC Endpoint

### **目录**

- 环境(配置)
- 实战步骤

1. Region, AZ, VPC, Subnet 之间的关系
2. 选择 Region
3. 创建 VPC
4. 创建 Subnet
5. 创建 Security Group（SG）
6. 创建 VPC Endpoint

- 总结
- 引申
- 后记

## **环境(配置)**

- AWS 中国或 Global 帐号，可在官网申请，一年内使用指定资源免费

## **实战步骤**

### **1. Region, AZ, VPC, Subnet 之间的关系**

首先我们先了解一下 Region 和 AZ（Availability Zone）的概念。

AWS 服务分布在世界各地，Region 是 AWS 服务中一个划分区域的概念，也是一个具体的地理位置。

一个 Region 就是多个数据中心的集群，到 2021 年为止 AWS 全球有 25 个 Region。

图 5

![img](https://pic4.zhimg.com/80/v2-348c4832dec9972c15d752782bc4fa07_1440w.jpg)



每个 Region 中包含数个独立的，物理分隔开的 AZ（Availability Zone），每个 AZ 有独立的供电，制冷，安保。

同一 Region 内 AZ 之间由高带宽，极低延时的光纤网络相连，数据以加密形式传输。

每个 AZ 下包含一个或多个数据中心。每个数据中心有独立的供 电，制冷，网络。

Region 与 AZ 的关系如下图

图 6

![img](https://pic2.zhimg.com/80/v2-0f057cc48c820504fd481be32b50cf2d_1440w.jpg)



VPC（Amazon Virtual Private Cloud）是用户在 Region 中自定义的虚拟网络，是一个整体概念。用户可以在一个 Region 中创建多个 VPC。

我们可以在 VPC 中选择 IP 网段，创建 Subnet，指定 Route Table，控制 ACL（Access Control list），设置网关等。

大部分 AWS 服务都需要以 VPC 为基础进行构建，比如最常用的 EC2，ALB，及无服务器服务 ECS Fargate。

当我们在一个 VPC 中创建 Subnet 时需要给 Subnet 选择一个 AZ（Availability Zone），一个 Subnet 只能选择建在一个 AZ 中。

Region，VPC，AZ，Subnet 关系如下图所示

图 7

![img](https://pic2.zhimg.com/80/v2-34b778cbe2d6059a704916194bf95565_1440w.jpg)



说明：

- 在图中的 Region 中建了一个 VPC
- 在 VPC 中又建了两个 Subnet
- Subnet 1 建在 Availability Zone A 中
- Subnet 2 建在 Availability Zone B 中

### **2. 选择 Region**

因为国内政策法规原因，AWS 在中国的服务与 AWS Global 服务略有不同。

AWS Global 的 Region 之间是通过主干网相连的，AWS 中国区的服务没有通过主干网与 AWS Global 相连，只有中国区内部两个 Region，北京和宁夏是相连接的。

中国区的服务相比 Global 区的服务要少一些，不过还在不断增加中。

如果是中国区帐号，可以在 AWS 中国控制台右上角帐号旁边点击地区，选北京或者宁夏区。

图 8

![img](https://pic1.zhimg.com/80/v2-0cb1b40017dde155f2119a54e26c0df8_1440w.jpg)



如果是 Global 帐号，可以在 AWS Global 控制台右上角帐号旁边点击地区，选择国际区。

图 9

![img](https://pic3.zhimg.com/80/v2-f13171bb0992a3ce181e2132d33beaca_1440w.jpg)



选择好 Region 后，之后的所有操作，创建的服务都在此 Region 内。

### **3. 创建 VPC**

在 AWS 中国区控制台选择 VPC，在 VPC 界面选择左边列表中的“Your VPCs”，然后点击“Create VPC”。

*提示：我们也可以在 VPC Dashboard 中选择“Start VPC Wizard”来创建 VPC，“Start VPC Wizard”在创建中会一起创建 Subnet，而我们为演示选择分步创建 Subnet*

图 14

![img](https://pic2.zhimg.com/80/v2-cc635c6a6106db62cf5b9e1d18857549_1440w.jpg)



添加 VPC 名称“tstest”，和 VPC 选择的 IP 网段，可以加 Tag 标明这个是测试环境

图 15

![img](https://pic2.zhimg.com/80/v2-55ee9cc7a70b1624dcf022ab66f56c8d_1440w.jpg)



说明：

可以看到我们在创建 VPC 时并不需要添写 AZ（Availability Zone）信息，VPC 只与 Region 有关。

图 16

![img](https://pic2.zhimg.com/80/v2-d66acf42f42e1f492ec6121050d0fc31_1440w.jpg)



VPC 创建很快完成，Main route table 和 Main network ACL 为 AWS 自动创建。

之后我们一般添加自己的 Route Table 和 Network ACL 进行网络控制。

### **4. 创建 Subnet**

VPC 创建完成之后，下一步我们来创建 Subnet。

Subnet 是 VPC 中的子网络，建立在特定的 AZ（Availability Zone）中。

Subnet 是最终承载大部分 AWS 服务的组件，比如 EC2， ECS Fargate，RDS。

如下图所示，两个 EC2 是分别运行在两个 Subnet 中，这两个 Subnet 分别处在不同的 AZ 中。

图 7

![img](https://pic2.zhimg.com/80/v2-34b778cbe2d6059a704916194bf95565_1440w.jpg)



Subnet 分为两种 Private Subnet 和 Public Subnet。

简单来说，不能直接访问 internet 的 Subnet 就是 Private Subnet，能直接访问 internet 的就是 Public Subnet。

当然 Private Subnet 也可以通过 NAT 的方式访问 internet，这点我们后面再谈。

下面我们先建一个 2 个 Subnet

VPC 界面选择“Subnets”，点击“Create Subnet”

图 17

![img](https://pic1.zhimg.com/80/v2-a498ba4334928419348c9805f9616990_1440w.jpg)



选择上面建好的 VPC“tstest”，给 Subnet1 名称，选择 AZ，设置子网网段，然后点击“Add new subnet”

图 18

![img](https://pic2.zhimg.com/80/v2-73f20482c53237252340de8831500639_1440w.jpg)



填写第二个 Subnet 的信息，然后点击“Create subnet”

图 19

![img](https://pic3.zhimg.com/80/v2-491d52fb4491603f66eff7f5f035c4fe_1440w.jpg)



两个 Subnet 创建成功

图 20

![img](https://pic3.zhimg.com/80/v2-8f3984fa5ee9179f61faaccc5432a992_1440w.jpg)



我们先把上面的信息记下来

![img](https://pic1.zhimg.com/80/v2-40451a1f0cda73f54190bb760fdc0a4c_1440w.jpeg)

我们在 AWS 中国北京区（cn-north-1）中创建了 VPC“tstest”，然后在 VPC 中建了两个 Subnet，这两个 Subnet 选择了同一个 AZ（cn-north-1a）。

现在这两个 Subnet 虽然一个叫 Private 另一个叫 Public，但其实两个都是 Private Subnet，因为这两个 Subnet 还都不能访问外网。

我们将在下一篇中，在另一个 AZ 中再新建两个 Subnet，并讲述如何把 Private Subnet 变为 Public Subnet

### **5. 创建 Security Group（SG）**

Security Group（SG）通过控制IP和端口来控制出站入站规则，可以用于EC2，RDS及下面将要用到的VPC Endpoint。

我们先创建一个Security Group，用来控制VPC Endpoint。

在AWS控制台选择VPC，进入VPC界面。选择“Security Groups”，点击“Create security group”

图21

![img](https://pic1.zhimg.com/80/v2-0125a1f9d66536a262a4817f6daf68c8_1440w.jpg)



填加Security Group名称“tstestVPCendpoint”,选择我们建好的VPC。

填加一条入站规则，打开443端口，允许从10.0.0.0/8的IP访问。出站规则保持原样即可。

点击“Create security group”

图22

![img](https://pic3.zhimg.com/80/v2-d2df5d545aafc582f26fced7f932067a_1440w.jpg)



创建成功

图23

![img](https://pic3.zhimg.com/80/v2-e4df8787801e977309d754613c404a1e_1440w.jpg)



### **6. 创建 VPC Endpoint**

VPC Endpoint用来直接连接VPC与AWS相关服务，比如RDS AIP,S3。

当系统安全要求比较高时，EC2处于的Subnet可能被限制，无法访问internet，这时EC2就无法访问AWS的一些服务，比如SSM。

这时我们可以利用VPC Endpoint把VPC和所需要访问的服务连接起来，然后EC2就可以不经internet访问到所需的服务。

下面我们配置到SSM的VPC Endpoint为下一步测试做准备。

进入VPC控制界面

选择“Endpoints”，点击“Create Endpoint”

图12

![img](https://pic1.zhimg.com/80/v2-6fd151645604d0c4ef825624e23b1bc4_1440w.jpg)



选择“AWS Services”

- 搜索“com.amazonaws.cn-north-1.ssm”，选择我们创建的VPC
- Subnets里选择“ts-private-1”
- 在Security Group中选择上面建的“tstestVPCendpoint”
- 点击“Create endpoint”

*提示：本文测试环境是中国AWS，在AWS Global中AWS Services的名称可能略有不同*

图24

![img](https://pic3.zhimg.com/80/v2-39197165316e86c265a2873287f0e562_1440w.jpg)



结果报错了，提示我们要启用DNS支持和DNS hostname，我们需要修改VPC，启用DNS。报错界面不需要关闭，新开一个VPC页面。

图28

![img](https://pic2.zhimg.com/80/v2-5fa2b980f60eab660271694c1be8ca81_1440w.jpg)



选择“Your VPCs”，点击我们的VPC id

图25

![img](https://pic2.zhimg.com/80/v2-2c08ac78652211571279ef1722a62d11_1440w.jpg)



进入我们的VPC界面后，点击“Action”中的“Edit DNS hostnames”

图26

![img](https://pic2.zhimg.com/80/v2-60165e6da41088d90237f49d50847a41_1440w.jpg)



勾选“Enable”，点击“Save changes”

图27

![img](https://pic4.zhimg.com/80/v2-9b88c7e43d74db13b65e8fbd14d901f7_1440w.jpg)



*提示：你需要在Action中点击“Edit DNS hostnames”并启用*

现在回到刚才创建VPC Endpoint报错的页面，点击“Retry”

图28

![img](https://pic2.zhimg.com/80/v2-5fa2b980f60eab660271694c1be8ca81_1440w.jpg)



创建成功

图29

![img](https://pic4.zhimg.com/80/v2-fdac8b32f9ad4152028dd0643226f4c7_1440w.jpg)



新endpoint处于“pending”状态，过一段时间会变成“avaiable”

图30

![img](https://pic3.zhimg.com/80/v2-f87bd5ff2f44379ffacc5d5e756309b6_1440w.jpg)



我们重复上面的步骤添加以下VPC endpoint

- com.amazonaws.cn-north-1.ssm
- com.amazonaws.cn-north-1.ec2messages
- com.amazonaws.cn-north-1.ssmmessages
- cn.com.amazonaws.cn-north-1.ec2

创建完成，结果如下

图31

![img](https://pic3.zhimg.com/80/v2-a6c22ad406e5b00bb385d15ce1a9c78e_1440w.jpg)



说明：设置以上VPC Endpoint是为了使SSM（System manager）可以配置EC2，然后就可以在网页上登录EC2服务器进行操作

我们将在下一篇文章中创建EC2，配置EIP，利用SSM登录EC2进行测试。

## **总结**

1. 首先我们选择Region，随后所有创建的内容都是存在此Region中
2. 创建VPC，一个虚拟网络，在里面设置IP段，VPC是一个逻辑结构，并不和AZ（Availability Zone）直接相关
3. 在VPC中创建Subnet，需指定IP段，并且指定所在的AZ，一个Subnet只能指定一个AZ，一个AZ可以容纳多个Subnet
4. 创建Security Group时，只需指定VPC。之后可以把SG与EC2， RDS, VPC Endpoint相关联，用来控制这些服务的出入站IP和端口

## **引申**

### **VPC, Subnet 如何在 AWS 资源中使用（关联）**

在创建了 VPC 和 Subnet 后，我们一般会把 AWS 资源和 Subnet 直接相连。

比如新建 EC2 时需要选择 EC2 所在的 Subnet,在定好 Subnet 之后，也就定好了 EC2 所在的 AZ，VPC。

在运行 ECS 时，如果是 EC2 模式则与上面例子相同，如果是 Fargat 模式，我们需要在 Service 中为 task 实例定义运行时所在的 Subnet。

在运行 RDS 时，需要选择 Subnet Group，Subnet Group 中包含多个 Subnet，RDS 实例最终也会运行在 Subnet 中。

在使用 Lambda 函数时，我们也可以把 Lambda 函数运行在指定的 Subnet 中，比如 Private Subnet，用来提供安全性。

在使用 EKS（k8s）时，集群中的 EC2 也要指定运行的 Subnet。

VPC,Subnet 紧密地集成于 AWS 的各种服务中，只能真正搞清楚了这些概念，才能更有效地组织利用 AWS 的服务。

---

## ------------------------------------------------------------------



---



## **简介**

长文多图预警，看结论可以直接拖到“总结”部分

本文承接上一篇文章介绍以下AWS基础概念或服务

- EC2
- IGW（Internet Gateway）
- Route Table（RT）
- EIP （Elistic IP）
- NAT gateway
- SSM （System manager）
- Security Group（SG）

在上一篇文章中我们创建了一个VPC，两个Subnet，配置了从VPC到SSM的endproint。

本文中我们先创建EC2，然后在VPC中添加IGW，再添加Route table 把Subnet转变为Public Subnet。

然后分配EIP并关联EC2，测试Public Subnet中的EC2可访问internet。

接下来创建NAT gateway，设置Private Subnet的路由使得Private Subnet中的EC2也可以访问internet。

最后修改Security Group（SG）测试EC2间的连通。

### **目录**

- 环境(配置)
- 实战步骤

1. 创建测试用EC2
2. 通过SSM配置EC2
3. 网页上登录EC2
4. 改为Public Subnet

- 添加IGW（Internet gateway）
- 设置RT（Route Table）
- 为EC2增加EIP（Elasitc IP）
- 利用NAT使Private Subnet中的EC2访问internet
- Security Group
- 总结
- 引申
- 后记
- 更正

## **环境(配置)**

- AWS 中国或 Global 帐号，可在官网申请，一年内使用指定资源免费

## **实战步骤**

### **1. 创建测试用EC2**

### **创建EC2**

Amazon EC2（Amazon Elastic Compute Cloud）是AWS提供的基本服务，可以理解为一台VM（虚拟服务器）。

图0

![img](https://pic3.zhimg.com/80/v2-0df14f846d9352408ee35885f4b0d1d6_1440w.jpg)



EC2支持Windows和Linux两个环境，linux中还分Amazon Linux及Ubuantu，Redhat等其它Linux版本。

实际上，目前很多公司的上云项目，就是把原来本地VM上的项目直接搬到了EC2上。

本文中，我们利用最小的Amzon Linux版本EC2进行网络测试。

上一篇建好的Subnet信息抄写如下，我们将在每个Subnet中各建一个EC2

![img](https://pic1.zhimg.com/80/v2-40451a1f0cda73f54190bb760fdc0a4c_1440w.jpeg)

在AWS中控台，选择EC2，进入EC2界面。选择“Instances”，点击“Lanuch instances”

图1

![img](https://pic3.zhimg.com/80/v2-7a656c21b337a9e8a910042b6233a0da_1440w.jpg)



选择第一个AMI（Amazon Linux版本）就可以，点击“select”

图2

![img](https://pic4.zhimg.com/80/v2-a9c353ade267455a8f9af834e646148b_1440w.jpg)



选t2的最小类型，点击“Next：Configure Instance Details ”

图3

![img](https://pic1.zhimg.com/80/v2-b6782f27b70506d79f96ba7812988988_1440w.jpg)



选择上一篇建好的VPC “tstest”和Subnet“ts-public-1”,注意IAM Role选择“AmazonSSMroleForInstancesQuickSetup”，这个在后面配置SSM时需要。点击“Next: Add Storage”。

图4

![img](https://pic1.zhimg.com/80/v2-9542d0283e07c07f5c07d49c664ccbe0_1440w.jpg)

说明：这里“Auto-assign Public IP”中选项是“Using subnet setting（Disable）”是指默认创建的Subnet不会为Subnet中新建的Network interface分配Public IP，所以新建EC2的网卡中也没有分配Public IP。

不需要改Storage，点击“Next: Add Tags”

图5

![img](https://pic1.zhimg.com/80/v2-09cb904bb468a54ab277ae053a9e21fc_1440w.jpg)



填加一个Tag方便后面查找，然后点击“Next：Configure Security Group”

图6

![img](https://pic3.zhimg.com/80/v2-a380e81ccd077722816c5d2b54cee006_1440w.jpg)



我们新建一个Security Group “tstest-pub1-sg”并加入一条规则，然后点击“Review and Launch”

图7

![img](https://pic4.zhimg.com/80/v2-c150c049572ca585c5da0f623405c21f_1440w.jpg)



说明：可以看到Security Group用来控制EC2的出站入站规则，控制IP和端口。

点击“Launch”

图8

![img](https://pic2.zhimg.com/80/v2-b142d47ed054d9815badf20e640ff9d5_1440w.jpg)



在出现的界面中选择一个Key Pair，然后点击“Launch Instances”

图9

![img](https://pic3.zhimg.com/80/v2-4a4e280304d6e524fed482864d91ffde_1440w.jpg)



说明：Key Pair是SSH Key用来远程无密码登录EC2 Linux服务器，与“AWS ECS Fargate容器调试”一文创建的Key Pair功能相同。本文后面用SSM网页登录EC2，所以这里可以选择Key Pair也可以留空。

过一会儿后，可以查看EC2已处于运行状态，可以看到EC2只有Private IP，Public IP为空

图10

![img](https://pic1.zhimg.com/80/v2-f9621a0e9b983758286b2c5661ad6724_1440w.jpg)



到目前为止的关系图

图11

![img](https://pic1.zhimg.com/80/v2-63e4940a734ea886a62b707ce2b611e8_1440w.jpg)



说明：我们在“ts-public-1”的Subnet中创建了一个EC2 “tstest-pub1”，这个EC2只有Private IP，无法从外部直接访问。

### **2. 通过SSM配置EC2**

SSM（System Manager）是AWS提供用来统一管理AWS资源架构的服务，可以给资源分组，定制任务，提供操作数据等。

我们这里利用SSM在EC2上配置SSM agent，然后我们就可以通过网页登录Linux系统。

在AWS中控台搜索SSM，进入System Manager界面。第一次使用的话会出现以下界面，点击“Get Started with Systems Manager”

图12

![img](https://pic2.zhimg.com/80/v2-2c81fe9c0aeb82df0748f3e26b43fd9d_1440w.jpg)



进入Quick Setup界面后，点击“Edit all”

图13

![img](https://pic4.zhimg.com/80/v2-5503d8628732c5c77f10a47e5ea0fa07_1440w.jpg)



我们选择“Choose instances manually”，然后选择刚创建的EC2 “tstest-pub1”,点击“Reset”

图14

![img](https://pic2.zhimg.com/80/v2-80d7c6797789c45f230be5433689abf9_1440w.jpg)



说明：这里SSM会在EC2上安装必要的软件用来监控。

然后我们点击左边列表“Managed Instances”，然后查看我们的EC2 “SSM Agent ping status”状态为Online。

图15

![img](https://pic3.zhimg.com/80/v2-8526e15c2d2cc40bf9b444afb81e91ce_1440w.jpg)



说明：当使用SSM控制EC2时需要注意以下几点

- EC2的instance profile需要设成“AmazonSSMRoleForInstancesQuickSetup”，这个profile用来赋予EC2使用SSM核心功能的权限

图16

![img](https://pic1.zhimg.com/80/v2-158dd462528d64200e3dc36983b0cae0_1440w.jpg)



- EC2能访问internet，或者EC2所在的VPC存在到SSM的endpoint（上一篇文章中建的VPC endpoint）
- 如果用VPC endpoint，需确保这个VPC endpoint配置的Security group的入站规则允许源EC2的IP或SG访问443端口

*提示：如果在创建EC2时选择了“AmazonSSMRoleForInstancesQuickSetup”做为IAM Role，并且网络满足上面后两个条件，EC2创建好SSM会自动配置agent*

### **3. 网页上登录EC2**

在EC2界面左边列表选择“Instances”，选择EC2 “tstest-pub1”，点击“Connect”

图17

![img](https://pic4.zhimg.com/80/v2-b50c527398a03b91f81cf4f35efedddf_1440w.jpg)



选择“Session Manager”，点击“Connect”

图18

![img](https://pic3.zhimg.com/80/v2-40959835dd9d65467765fafcfe16dec6_1440w.jpg)



弹出新网页进入EC2命令行，sudo su切换到root用户

图19

![img](https://pic2.zhimg.com/80/v2-1de4d4f7c486698843f7aa262d5b612d_1440w.jpg)



准备工作就绪，接下来我们还会创建EC2并用网页登录，方法相同，就不再重复。

到目前为止的关系图

图11

![img](https://pic1.zhimg.com/80/v2-63e4940a734ea886a62b707ce2b611e8_1440w.jpg)



说明：我们在“ts-public-1”的Subnet中创建了一个EC2 “tstest-pub1”，这个EC2只有Private IP，无法从外部直接访问。

### **4. 改为Public Subnet**

### **添加IGW（Internet gateway）**

IGW（Internet gateway）是AWS提供的，用来实现VPC和Internet之间相互通信的高可用组件。

IGW提供以下两项功能

- Subnet中的流量通过IGW访问Internet
- 为配置了Public IP的Instance提供网址转换

在没有配置IGW之前，用SSM网页登录EC2 “tstest-pub1”，然后用wget命令测试internet连接，如果可以连通到internet，wget命令可以下载到这个网站的默认首页

```text
wget www.baidu.com
```

结果如下图，下载不了，表示连不通internet

图21

![img](https://pic1.zhimg.com/80/v2-b00625f5464526241a0c3bd73c571594_1440w.jpg)



下面我们在VPC中加入IGW（Internet Gateway），并配置路由把Subnet “ts-public-1”变成Public Subnet

进入VPC界面，选择“Internet Gateways”，点击“Create internet gateway”

图22

![img](https://pic1.zhimg.com/80/v2-f2e00fae02b1c32f7fc93e6ff64dee80_1440w.jpg)



填加IGW名称，点击“Create internet gateway”

图23

![img](https://pic2.zhimg.com/80/v2-d51b43cfbb6db511b7b5b9e358320409_1440w.jpg)



创建成功，点击“Attache to a VPC”

图24

![img](https://pic4.zhimg.com/80/v2-0d215e44be53c2024b7909352ece8cdf_1440w.jpg)



选择我们建的VPC，点击“Attach internet gateway”

图25

![img](https://pic2.zhimg.com/80/v2-f0d6d4c574a4abf884c374f05d4b7d9d_1440w.jpg)



结果如下

图26

![img](https://pic1.zhimg.com/80/v2-7d968ad9e58feff74d9bab3e68f9d984_1440w.jpg)



现在VPC中配置好IGW了。

### **设置RT（Route Table）**

RT（Route Table）与Subnet相关连，用来描述网络路由。

我们给VPC加了IGW之后，需要修改Subnet相关的路由，确保访问Internet的请求发送到IGW。

每个VPC中有一个默认的主RT，自动关联VPC内的每一个Subnet。我们现在为Subnet “ts-public-1”单独创建一个新的RT。

在VPC界面，选择“Route Tables”，点击“Create route table”

图27

![img](https://pic3.zhimg.com/80/v2-6b029539f917efcfb5296cc594deef8a_1440w.jpg)



填写RT名称，选择我们的VPC，点击“Create”

图28

![img](https://pic3.zhimg.com/80/v2-a4e802c2b6800aa3aa9e00c73d157b7e_1440w.jpg)



然后我们把建好的RT和Subnet “ts-public-1”相关联

选择我们的RT,选中“Subnet Associations”，点击“Edit subnet associaions”

图29

![img](https://pic3.zhimg.com/80/v2-de1061862b94cbba44912efec4b6c55a_1440w.jpg)



选择Subnet “ts-public-1”，点击“Save”。我们把新RT和Subnet相关联。一个RT可以关联多个Subnet。

图30

![img](https://pic2.zhimg.com/80/v2-b6e343b8bb0831cb3f1fa6e3d2328815_1440w.jpg)



把RT和Subnet关联后，下面添加到IGW的路由。

选择我们的RT,选中“Routes”，点击“Edit routes”

图31

![img](https://pic1.zhimg.com/80/v2-852ff4a8127dc50fcb46d3ddaac49d70_1440w.jpg)



新填加一条路由如下，点击“Save routes”

- Destination： 0.0.0.0/0
- Target： tstestIGW

图32

![img](https://pic4.zhimg.com/80/v2-e58a6b4a44b758f00f0e0b433478a88f_1440w.jpg)



结果如下

图33

![img](https://pic1.zhimg.com/80/v2-e3acd7a8dcef0d1cc77a701ed818abb0_1440w.jpg)



说明：

- 第一条表示到10.0.*.*的请求会发送至VPC中
- 第二条表示到其它IP的请求会发送至IGW

现在Subnet “ts-public-1”已经成为Public Subnet。但是，如果在EC2上测试还是会连不上Internet，我们还需要为EC2加上一个Public IP或者EIP。

EC2 Public IP来源于EC2-VPC公共IP池。每次停止EC2再启动后，EC2的Public IP会重新分配，变成一个新的IP。

*注意：如果直接Restart EC2（不是停止，再启动）则Public IP不会变*

EIP是不会因为EC2停止后再启动而改变的，下面我们给EC2配置一个EIP

### **5. 为EC2增加EIP（Elasitc IP）**

EIP（Elasitc IP）是AWS提供的静态公共IP，可以从internet上访问到。

默认一个VPC只能申请5个，可以向AWS Support提请求增加数量。

我们先申请一个EIP，然后把它关联到我们在Public Subnet的EC2上

在VPC界面，选择“Elastic IPs”，点击“Allocate Elastic IP address”

图34

![img](https://pic2.zhimg.com/80/v2-c542a2d91d4b1d5388ace8b8706b417d_1440w.jpg)



填入Tag，方便查找，点击“Allocate”

图35

![img](https://pic2.zhimg.com/80/v2-e2dda4587053efa27fa3e0275c39f7d9_1440w.jpg)



*提示：分配EIP后要和EC2或者网络接口相关联，不然空闲的EIP会收费，关联后则不收费*

关联EC2，点击“Associate This Elastic IP address”或者选Action中相同项

图36

![img](https://pic3.zhimg.com/80/v2-ff6fb6236da5cda7122df342136de7ba_1440w.jpg)



选择instance，然后在下拉列表中选择EC2 “tstest-pub1”，点击“Associate”

图37

![img](https://pic3.zhimg.com/80/v2-e69adbd15d98d3a29ec791cee55ccdf2_1440w.jpg)



结果如下

图38

![img](https://pic4.zhimg.com/80/v2-238ddda7e171c068190002c596b114cb_1440w.jpg)



我们再去测试EC2 “tstest-pub1”，可以看到网页可以下载成功，说明现在已经可以连通到internet。

图39

![img](https://pic1.zhimg.com/80/v2-7a3e6a5f3cfd777faa200a37b9d86c2c_1440w.jpg)



在EC2的命令行中，用root用户运行下列命令，安装telnet，方便后续测试

```text
yum install telnet
```

小结：

- 新建的Subnet就是Private Subnet
- 在Private Subnet中配置了到IGW的路由后，就变成Public Subnet
- Public Subnet中的EC2还要再配置一个Public IP或者EIP就可以访问Internet
- 如果EC2可以访问internet，其关联的Security Group入站规则如果允许从internet访问，那么这个EC2就可以从internet中直接访问到。

### **6. 利用NAT使Private Subnet中的EC2访问internet**

我们用上面的相同方法，新建如下内容

1. Security Group “tstest-pri1-sg”
2. 在”Subnet “ts-private-1”中创建EC2 “tstest-pri1”并与Security Group “tstest-pri1-sg”关联

EC2 “tstest-pri1”建好如下

图40

![img](https://pic3.zhimg.com/80/v2-50630cc025d73ba229f18d3b8f233692_1440w.jpg)



现在关系图如下

图20

![img](https://pic3.zhimg.com/80/v2-38efd33e1111d89c276d5984101ff866_1440w.jpg)



我们用SSM网页登录EC2 “tstest-pri1”测试，其无法访问internet。

图41

![img](https://pic4.zhimg.com/80/v2-b9d05c05fed7d48efe674cd2d37760c7_1440w.jpg)



下面我们增加NAT组件，并给Private Subnet创建RT，在其中增加到NAT的路由，使得Private Subnet中的EC2可以访问internet。

NAT（network address translation），AWS有NAT instance和NAT gateway提供地址转换功能。

NAT instance需要启动一个专门的EC2，我们这里用轻量级的NAT gateway。

NAT gateway允许Private Subnet中的实例连接到 Internet，但不允许 Internet 与这些实例的连接。

### **创建NAT Gateway**

在VPC界面选择“NAT Gateways”，点击“Create NAT gateway”

图42

![img](https://pic2.zhimg.com/80/v2-9835d68c166c6c566501d75d582d6029_1440w.jpg)



填入NAT名称，选择Public Subnet，选择一个EIP或者点击“Alocate Elastic IP”分配一个新的。这里我把绑定到之前EC2的EIP释放了，在这里重用，然后点击“Create NAT gateway”

图43

![img](https://pic2.zhimg.com/80/v2-8e510f7b37376e12f6c7ad99ed3717e9_1440w.jpg)



说明：NAT Gateway需要建在Public Subnet中并且配置一个EIP

创建成功如下图

图44

![img](https://pic4.zhimg.com/80/v2-e7843bae7840b8131f1221b5ba5b697b_1440w.jpg)



下一步我们用上面同样的方法创建新RT “ts-private-1-rt”，并与Private Subnet“ts-private-1”相关联，并加入到NAT的路由。

图45

![img](https://pic2.zhimg.com/80/v2-a73c3e9132646b5dd5d8a978db91ea69_1440w.jpg)



在配置了到NAT路由后，可以发现Private Subnet中的EC2 “tstest-pri1”也可以访问internet了

图46

![img](https://pic2.zhimg.com/80/v2-08b2cd50b76c74af4eb6e3442c0f70f9_1440w.jpg)



### **7. Security Group**

最后我们简单测一下Security Group的作用。

Security Group（SG）用来控制其关联的实例的IP和端口出入站规则。

我们在EC2 “tstest-pri1”测试到EC2 “tstest-pub1”的22端口是否可以访问。

图47

![img](https://pic4.zhimg.com/80/v2-31f367310c9160d5a15f21a2359021a7_1440w.jpg)



结果显示超时，因为EC2 “tstest-pub1”的SG “tstest-pub1-sg”中没有允许任何IP/端口访问22端口。

现在我们增加一条入站规则。 在EC2界面，选择“Security Groups”，选中“tstest-pub1-sg”，选择“Inbound Rules”，点击“Edit inbound rules”

图48

![img](https://pic3.zhimg.com/80/v2-903f3fa8e2a01d1200b234ebf6e447fa_1440w.jpg)



这里列出了三条入站Rule，只填加其中任一条即可

1. 把发起请求的EC2所在的SG添入允许的源
2. 把发起请求的EC2所在的IP段添入允许的源
3. 把发起请求的EC2的具体IP添入允许的源

图49

![img](https://pic2.zhimg.com/80/v2-910a622154cc39726686851a8501acb9_1440w.jpg)



我们在EC2 “tstest-pri1”测试到EC2 “tstest-pub1”的22端口的访问成功了。

图50

![img](https://pic1.zhimg.com/80/v2-e3608fc1303f57a8c71e556c7ee6508c_1440w.jpg)



## **总结**

这里把上一篇的总结再汇总一下

1. 首先我们选择Region，随后所有创建的内容都是存在此Region中
2. 创建VPC，一个虚拟网络，在里面设置IP段，VPC是一个逻辑结构，并不和AZ（Availability Zone）直接相关
3. 在VPC中创建Subnet，需指定IP段，并且指定所在的AZ，一个Subnet只能指定一个AZ，一个AZ可以容纳多个Subnet。VPC中Subnet默认是可以相互访问的
4. 创建Security Group时，只需指定VPC。之后可以把SG与EC2， RDS, VPC Endpoint相关连，用来控制这些服务的出入站IP和端口
5. IGW是一个独立的组件配置在VPC上，使得VPC可以访问internet
6. 配置了到IGW路由的Subnet就是Public Subnet，Public Subnet中的EC2可以访问interent，但需要给EC2配一个公共IP或者EIP
7. NAT gateway需要一个EIP（Elastic IP）并且把NAT配置在Public Subnet中
8. Privte Subnet不能直接访问interent，配置了到NAT路由的Private Subnet中EC2可以访问internet，但不能被internet访问到

## **引申**

1. 实践中我们把应用程序，数据库放在Private Subnet中，阻止从internet访问。把堡垒机和ALB（Application Load balancer）放在Public Subnet，允许从internet访问。
2. 一般我们会建两套Public Subnet和Private Subnet，分别放在不同的AZ中，防止其中一个AZ出问题。这时如果配置NAT，也需要在两个Public Subnet中各配置一个NAT。
3. 前面几篇关于Fargate的文章中，我们把Task运行在Public Subnet中，生产中这些Task也是运行在Private Subnet中，以阻止从internet访问。
4. 当创建Subnet时，自动分配Public IP的功能是关闭的，此功能可以在Subnet界面打开。打开此功能后，在这个Subnet创建EC2时，其Network Interface会自动分配一个Public IP。
5. 篇幅的关系这里没有讲到Network Interface。给EC2配置EIP，实际上是给EC2上的Network Interface配置EIP。 我们也可以给EC2增加多个Network Interface。

## **后记**

一开始想的很好，这样的基础文章应该很容易写，一篇就够了。

因为是面向新入门的读者，所以想把实战中用到的每个概念都提到，这样读者就不需要碰到一个词不懂，就得去查。

结果越写越多，越写越累，最后不得不拆成两篇，还总感觉一开始想的内容没加进去。

这两篇文章字多图也多，也比较枯燥。但我自认为把基础的概念都覆盖了，应该可以清楚描述这些概念之前的关系，而且里面的实验也比较细，照着做应该问题不大。

VPC相关内容又多又杂，这两篇文章也只讲了其中一小部分。希望能给刚入门的朋友提供一个大的框架，方便以后深入学习。

## **更正**

“一文搞懂AWS Region, VPC, VPC endpoint，AZ, Subnet 基础篇上”中，VPC endpoint连接的是RDS Api，不是直接连RDS。