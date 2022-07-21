[TOC]

接上 [AWS 考试认证心得(SAA)（上）](https://www.jianshu.com/p/56fa483e01bc)

这章主要就是AWS-SAA相关知识点的干货啦，不过这不一定适用于每个人，最好的办法还是自己按照自己的熟悉薄弱的地方去归纳总结AWS-SAA的考点知识：

##### 1.考点总览

个人总结AWS-SAA的考试有八大考点方向，分别是：

> 1. AWS Security, Identity & Compliance
>     AWS IAM
>     --- User&Group
>     --- EC2 instance profile for IAM
>     AWS Cross-account access
>     AWS KMS
>     AWS CloudHSM
>     AWS Directory Services
>     AWS Web Application FireWall - WAF
>     AWS Shield
>     AWS Secrets Manager
>     Amazon Cognito

> 1. AWS Networking & Content Delivery
>     AWS VPC
>     --- Create VPC NACLs
>     --- VPC Endpoint, VPC Peering, VPN Connections, VPN CloudHub , VPC NAT,
>     --- VPC Flow Logs
>     --- Security Group vs NACLs,
>     --- Bastion Host
>     NAT Instance vs NAT Gateway vs Bastion
>     AWS ELB
>     --- ALB, NLB, Classic Load Balancer vs Application Load Balancer,
>     --- ELB Monitoring
>     AWS Route 53
>     --- Rout 53 Modify DNS
>     -- Routing Policy:
>     (简单路由策略（simple routing policy）
>     加权路由策略（weighted routing policy）
>     延时路由策略（Latency Routing Policy）
>     故障转移路由策略（Failover Routing Policy）
>     地理位置路由策略（Geolocation Routing Policy) )
>     AWS API GateWay
>     AWS CloudFront (CloudFront, CloudFront CDN)
>     AWS Direct Connect --DX
>     AWS PrivateLink
>     AWS Transit Gateway
>     AWS Global Accelerator
>     Elastic IP vs Public IP vs Private IP

> 3.AWS Computer Services
>  AWS EC2
>  --- EC2 AMI,
>  --- EC2 Instance Types,
>  --- AWS Auto Scaling & ELB
>  --- EC2 Instance Lifecycle,
>  --- EC2 Security Group
>  --- EC2 Instance Storage & EBS
>  --- EC2 ELB & Health Check
>  --- EC2 ALB & NLB
>  --- EC2 EC2 Monitoring (CloudWatch & CloudTrail)
>  --- EC2 Instance Metadata – Userdata
>  --- EC2 – Placement Groups
>  --- EC2 Network – Enhanced Networking
>  --- EC2 Troubleshooting
>  --- EC2 Purchase Options (on-demand, reserved),
>  AWS EC2 Auto Scaling
>  AWS Lightsail

> 4.AWS Storage Services
>  AWS EC2 Storage
>  --- EC2 Instance Store Storage
>  --- EBS (EBS Volume Type, Snapshot, Performance)
>  --- EBS vs AWS Instance Store
>  AWS S3
>  --- S3 Consistency Model
>  --- S3 Storage Types (S3 Standard, S3 Intelligent-Tiering, S3 Standard-IA, S3 One Zone-IA)
>  --- S3 Object Version Control
>  --- S3 Lifecycle Management
>  --- S3 Permission
>  --- S3 Data Protection
>  AWS Storage Gateway
>  AWS S3 Glacier ( Glasier, Glasier DeepArchive，Archive Retrieval--Expedited retrievals )
>  AWS Snow Family (snowball, snowmobile)
>  AWS EFS
>  AWS S3 vs EBS vs EFS
>  AWS FSx ( FSx for windows file server & lustre )

> 5.AWS Container Service
>  AWS ECR (Elastic Container Registry)
>  AWS ECS
>  AWS EKS (K8s)
>  AWS Fargate
>  AWS Elastic Beanstalk

> 6.AWS DataBase Services
>  AWS RDS
>  --- AWS RDS Replica - Multi-AZ HA& Read Replica
>  --- RDS Storage
>  --- RDS Snapshots, Backup  & Restore
>  --- RDS Security
>  --- RDS Maintenance & Notification
>  --- RDS Monitoring & Notification
>  AWS DynamoDB
>  --- AWS DynamoDB Secondary Indexes
>  --- AWS DynamoDB Throughput Capacity
>  --- AWS DynamoDB Advanced
>  --- Amazon DynamoDB Accelerator (DAX)
>  --- AWS DynamoDB Global Tables
>  AWS Elasticache
>  AWS Aurora
>  AWS Redshift
>  AWS Athena
>  AWS DocumentDB

> 7.AWS Application Services & Data Analysis
>  AWS SQS
>  AWS SNS
>  AWS SES
>  AWS SWF
>  AWS API Gateway
>  Elastic transcoder
>  AWS Kinesis
>  --- Kinesis Data Streams
>  --- Kinesis Firehose
>  --- Kinesis Video Streams (AWS Media Services)
>  AWS Elastic Map Reduce – EMR
>  AWS Lambda

> 8.Other Important Services
>  AWS CloudWatch
>  AWS CloudTrail
>  AWS CloudFormation
>  AWS Organizations (组织账单服务)
>  AWS Billing and Cost Management
>  AWS Cross Account Acess (跨账号访问权限)
>  AWS Cloud Migration Services
>  AWS Import/Export
>  AWS DATA Transfer Services (Storage Gateway, Direct Connect，Data sync)
>  AWS Cognito
>  AWS X-Ray
>  AWS Key Pairs
>  AWS Config rule

先按照大纲弄清每个服务是做什么的，然后再深入去了解，再对比相似的服务，比如对于存储这一块儿就会考S3，EBS，EFS的区别，格子在什么情况下用，再比如题目会问在某情况下怎么cost最经济高效？Spot，Reserved，还是On-demand。建议在这个过程中建议观看英文文档，因为有很多专业的词汇还是要用英文才能描述的更准确。

##### 2.答题技巧

AWS的考试是十分有技巧的，相信各位同学经历了中国大大小小的考试，见过大风大浪，这个考试也应该不在话下，哈哈。总的来说就是做英文阅读！没错，就是做阅读。学会抓关键字法，排除法灵活运用。比如这一道题目：

> Question：
>  During a review of business applications, a Solutions Architect identifies a critical application with a relational database that was built by a business user and is running on the user's desktop. To reduce the risk of a business interruption, the Solutions Architect wants to migrate the application to a **highly available**, multi-tiered solution in AWS. What should the Solutions Architect do to accomplish this with the LEAST amount of disruption to the business?
>
> A. Use AWS DMS to migrate the backend database to an Amazon RDS Single-AZ DB instance.Migrate the application code to AWS Elastic Beanstalk
>
> B. Create an import package of the application code for upload to AWS Lambda, and include a function to create another Lambda function to migrate data into an Amazon RDS database
>
> C. Create an image of the user's desktop, migrate it to Amazon EC2 using VM Import, and place the EC2 instance in an Auto Scaling group
>
> D. Pre-stage new Amazon EC2 instances running the application code on AWS behind an Application Load Balancer and an Amazon RDS Multi-AZ DB instance

这是道是非常经典的AWS的题目类型，虽然题目很长，但是按照找关键字法的话就很快能做出来。快速浏览题目找到关键词 **highly available**  ，既然题目要求是HA的，那答案就是D。因为只有D有对应关键字的答案----**Multi-AZ** 即：多个可用区。（看到HA那么90%以上选Multi-AZ，因为要保证高可用，必须要在多个可用区部署，反之如果只是单个可用区，那么服务挂了就不能保证高可用)。所以有很多考题就要学会抓关键字去做，这样就能事半功倍。

##### 3.总结小知识：

1.SMB + on- premises  找 AWS Storage GateWay File gateway (注意是 file )

1. 需要高速IO的磁盘：Amazon EBS provisioned IOPS SSD volume

3.对象存储： S3 ，S3也可以放static web，常与cloudfront搭配分发

4.“multi-instance，HFC，并行” = EFS

5.I/O较低，偶尔会有小峰值：EBS通用SSD(gp2) General Purpose

1. Amazon Athena 数据库查询服务是一种无服务器交互式sql查询服务，让您能够轻松使用标准 SQL 进行 S3 数据库查询工作已经复杂的sql查询。

7.吞吐量经过优化的HDD（st1）卷提供了低成本的磁存储，该磁存储根据吞吐量而不是IOPS定义了性能。此卷类型非常适合大型连续工作负载，例如Amazon EMR，ETL，数据仓库和日志处理。

8.冷HDD（sc1）卷提供了低成本的磁存储，该磁存储通过吞吐量而不是IOPS来定义性能。 sc1具有比st1低的吞吐量限制，非常适合大型连续的冷数据工作负载

1. Glacier: 数据不可变，创建存档后便无法对其进行更新。从S3到Glacier
2. SNS+SQS。 SQS分stand和FIFO队列，FIFO才能保证顺序，SNS要subscribe
3. 移动/网络应用程序身份验证，想想 cognito和MFA
    gateway VPC endpoint只用于DynamoDB和S3，剩下的其他服务都是用interface
    VPC endpoint
4. VPC flow logs:记录网络流量
5. CloudWatch解决CPU,以及管理CloudTrail
6. Cloudtrail 用来记录API调用的,可以用来记录AWS的任何资源的行为审核日志
7. Elastic Beanstalk 将自动处理容量预配置、负载均衡、自动扩展及应用程序运行状况监控等部署详细工作。（还是会有EC2实例，只不过是自动管理）是非常好的web解决方案,比如node.js Cloudwatch指标将仅向您显示特定于ELB本身的指标。 而不是哪个src IP/客户端访问了该ELB上的特定服务。 为此，您需要在ELB上启用访问日志。
8. 有多个app调用消费，并且公开为RESTful calls，那么就需要Kinesis Data Stream。 SQS一般用来内部应用
9. 如果您将一个或多个负载均衡或目标组附加到您的弹性伸缩组，则默认情况下，该组不会认为某个实例不健康，如果该实例未通过负载均衡健康检查，则会将其替换。但是，您可以选择将Auto Scaling组配置为使用弹性负载平衡运行状况检查。 这样可以确保组可以根据负载均衡器提供的其他测试来确定实例的健康状况。 负载均衡器定期发送ping、尝试连接或发送请求来测试EC2实例。 这些测试被称为健康检查。
10. 一般EC2和RDS都是走内网，外网通过ELB和NAT Gateway
11. 每个AWS的EBS卷的最小大小是1 GiB
12. 

4 iGB = 200 IOPS
 Volume(iGB) : IOPS = 1：50，
 EBS  4 IOPS = 1024 kb I/O
 I/O : IOPS = 256 : 1

21.Amazon EBS 可看成外接移动硬盘

1. 可以通过在EC2上安装CloudWatch agent来实现监视EC2的内存使用情况，默认CloudWatch不包含内存监控

23.NLB 用于 layer 4

24.ALB 用于layer 7

1. VPC endpoint 端点是虚拟设备。可以用于将受保护的信息传输到不同的使用者中，并向特定服务使用者授予权限以创建链接。它们是横向扩展、冗余且高度可用的VPC组件，允许您的VPC中的实例与服务之间进行通信，而不会对您的网络流量造成可用性风险或带宽限制。
2. AWS PrivateLink 可用于访问AWS所有服务
3. 容器容，cluster，task definition实例是运行ECS代理并已注册到集群的EC2实例。
4. 放置群组（置放群组）是单个可用区内的实例的逻辑分区，不能跨越多个可用区。但是如果置放群组中启动了10个EC2实例，那么可以跨越同一区域中的多个可用区，因为每个群组在每个可用区中最多有7个正在运行的实例，10个EC2实例无法在一个可用区内放置，需要跨可用区
    如果要在策略声明中指定资源，可以在EC2中使用其Amazon Resource Name(ARN)
5. Amazon EBS快照的最大大 小为1TB。 因此,如果设备映像大于1TB,则会对映像进行分块并将其存储在 Amazon S3上。 2TB的话则分到两个1TB对象的S3上
6. 可以使用Amazon Simple Email Service(Amazon SES)通过电子邮件将文件更改发送给用户
7. 密钥对仅用于Amazon EC2和CloudFront
8. AWS Import/Export不支持从EBS和Glacier导出
9. 安全组的规则可以随时修改
10. 每个AWS账户每个区域的所有AWS账户限制为5个EIP（弹性IP地址）
11. aws允许创建的Auto Scaling组的最大数量是20
12. 部分实例时间（partial instance-hours） 即不足一个计费时间单位 按 一个满计费单位（full hours）计算
     如果您的I/O延迟高于您的要求，请检查平均队列长度，以确保您的应用程序不会驱动比您预配置的更多的IOPS。您可以通过保持较低的平均队列长度（通过为卷配置更多的IOPS来实现）来保持较高的IOPS，同时降低延迟。
13. Cloudfront不是任何加速都最快,如果访问者离S3最近, router53可以判断出直接去S3,不用去CDN绕行
14. EBS 加密 想到KMS
15. S3可以用版本控制来支持跨区域复制
16. 用VPC flow logs记录通过VPN网路流量
17. AWS Config可以查询AWS资源配置的详细信息
18. 扩展更大的RDS实例类型 和 将读取查询重定向到RDS读取副本 来提高数据库性能，Auto Scaling并不能增加数据库性能
19. 通过修改Auto Scaling组的冷却计时器 和 修改触发AutoScaling缩减策略的CloudWatch警报期限可以防止AutoScaling组同一时间放大和缩小
20. 利用 CDN 做区域权限隔离（限制某个地区访问资源）
21. 看到用户桌面就想到桌面映像 和 VM import
22. Amazon DynamoDB支持递增和递减原子操作
23. Amazon EBS支持的实例可能立即终止的一些原因：您已达到容量上限。AMI缺少必需的部分。快照已损坏。
24. 一个VPC可以跨越多个可用区。 相反，子网必须位于单个可用区中

总之要善于总结这些知识点，之后要做的事就是Mapping，找到关键字及匹配答案就能快速完成答题，不然130分钟65道题时间还是有点紧张。

最后祝大家考试顺利，早日通过认证考试！



作者：烧杰
链接：https://www.jianshu.com/p/c79ad40f52c9
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。]