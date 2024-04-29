[TOC]

# 前言

不久前考過 AWS SAA-C02，想看[心得文](https://medium.com/cloud-guru-的征途/考試篇-三個月考過-aws-solution-architect-associate-saa-c02-考試重點與心得-線上考試注意事項-f01a9bc29c4a)可以左轉，其中在「三、準備方法」的區塊有提到，我認為整個考試脈絡可以分成三個階段：

![img](https://miro.medium.com/max/1400/1*Qcjqsxvs8esZX0MR0E4Xuw.png)

1. 內容階段：第一次學習 AWS 的所有內容
2. 吸收階段：看完所有內容後，快速檢驗自己懂得多少，並持續從題目中吸收知識，抓到考試的主要題型
3. 衝刺階段：對於考試的重點已有概念，頻繁模擬考，針對自己不熟悉的地方加強

這篇文章適用「吸收階段」，幫助你快速導覽服務重點，並連結到 AWS 中文官方文件，這篇文章包含了 AWS SAA-C02 七成以上的內容，並將 A Cloud Guru 每個章節的重點翻譯整理於此，希望對你有幫助。

## 大綱

1. [Identity and Access Management (IAM)](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#73b2)
2. [S3](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0129)
3. [EC2](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#fa81)
4. [Database](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#66c1)
5. [Route 53](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#015c)
6. [VPC](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#7d1f)
7. [HA(High Available) Architecture](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#560d)
8. [Application](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0fdb)
9. [Security](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#3a9f)
10. [Serverless](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#cd69)

## 使用方法

![img](https://miro.medium.com/max/1400/1*Oi1zAVxSY5y81vrl9NvNTw.gif)

此外，每個章節一開始都有 ⏫ 🔼 🔽 三個輔助符號按鈕：
⏫ 回到大綱
🔼 前一章節
🔽 後一章節

那我們正式開始！

# 一、Identity and Access Management (IAM)

[⏫](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0838) [🔼](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0838) [🔽](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0129)

## [IAM Basics](https://docs.aws.amazon.com/zh_tw/IAM/latest/UserGuide/introduction.html)

IAM 讓我們可以管理 users 及其對 AWS Console 的訪問權限。

## IAM Basics 考試重點

- Users: End Users 像是人、員工、組織等
- Groups: Users 的集合，每個在 group 的 user 會繼承 group 的 permissions
- Policies: Policies 是由 documents，稱為 Policy documents，所組成，這些 documents 是 JSON 格式，且他們提出 permissions 作為 what a User/Group/Role 可以做的事。
- Roles: 你創立 roles 和指派他們給 AWS Resources

## [AWS Directory Service](https://docs.aws.amazon.com/zh_tw/directoryservice/latest/admin-guide/what_is.html)

- Managed services 家族
- 將 AWS resources 與 on-premises AD 做 connect
  \> AD Connector: On-premises AD 的 Directory gateway (proxy)
- 在 cloud 中的 Standalone directory
- User existing corporate credentials
- SSO 於任何 domain-joined 的 EC2 instance

## [IAM Policies](https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html)

```
Example:
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "SpecificTable",
            "Effect": "Allow", // Allow or Deny
            "Action": [
                "dynamodb:Get*",
                "dynamodb:CreateTable"
            ],
            "Resource": "arn:aws:dynamodb:*:*:table/MyTable"
        }
    ]
}
```

沒 allowed 等於 **denied**，deny > 任何其他

## [AWS Resource Access Manager (RAM)](https://aws.amazon.com/tw/ram/)

允許 Account 之間的 resource sharing

## [AWS Single Sign-On](https://aws.amazon.com/tw/single-sign-on/)

集中管理對多個 AWS Account 和商業 application 的存取。如果看到 SAML 2.0 就找 SSO 相關選項。

## [AWS Config](https://aws.amazon.com/tw/config/)

評量、稽核和評估 AWS 資源組態的服務。

# 二、S3

[⏫](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0838) [🔼](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#73b2) [🔽](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#fa81)

## S3 Basics

S3 提供開發者和 IT 團隊一個安全、耐久、高度擴展的 object storage。

## [S3](https://aws.amazon.com/tw/s3/) Basics 考試重點

- S3 是 **Object-based** — i.e. 允許你上傳檔案。
- 檔案大小：0 Bytes ~ 5 TB
- 無限 Storage
- Files 儲存在 Buckets
- **S3 是 universal namespace**，也就是名字必須是 unique globally
- **不適合安裝 operating system**
- 成功上傳會回傳 HTTP 200 status code
- 你可以開啟 **MFA Delete**（二階段驗證刪除）
- Key 基礎
  \> Key: Object 的 Name
  \> Value: Data，由 sequence of bytes 組成
  \> Version ID: versioning 時需要
  \> Metadata: Data about 所儲存的 data
  \> Subsources: Access Control List、Torrent
- **Read after Write consistency** for **PUTS** of new Objects: 也就是當你 write 一個新的 file，並立刻 read，你可以看到這個 data。
- **Eventual Consistency** for overwrite **PUTS and DELETES** (can take some time to propagate): 也就是當你 update 或 delete 一個 Existing file，並立刻 read，你可能會拿到舊的資料。
- Storage 種類：

1. **S3 Standard**
   **99.99% availability, 99.999999999% durability** (11 x 9s)，儲存多份遍佈在 multiple facilities，且被設計可以承擔 2 個以上 facilities 同時 loss。
2. **S3 — IA** (Infrequently Accessed)
   用於訪問 **less frequently** 的資料，但需要 **rapid access**，相較 Standard 較便宜。
3. **S3 One Zone — IA**
   用於更便宜的訪問 **less frequently** 的資料，但沒有 multi-AZ 的 data resilience。
4. **S3 — Intelligent Tiering**
   設計來優化成本，基於其**自動**移動 data 到相對便宜的 tier，不影響 performance，也不需提前處理。
5. **S3 Glacier**
   Glacier 是一個提供 data archiving 安全、耐久、低成本的 storage。
   \> Retrieval Time: Expedited(1–5 min), Standard(3–5 h), Bulk(5–12 h)
6. **S3 Glacier Deep Archive**
   Glacier Deep Archive 是**最便宜**的 class，Retrieval Time: **12 h**

- Storage 價格高到低
  Standard > IA > Intelligent > IA One Zone > Glacier > Glacier Deep Archive
- 務必閱讀 S3 FAQs

## S3 Notification

支援 Publish 到 SNS、SQS 和 Lambda。

## S3 Security 和 Encryption

- Encryption In Transit
  \> **SSL/TLS
  \> Client Side Encryption**
- Encryption At Rest (Server Side)
  \> **S3 Managed Keys — SSE-S3
  \> AWS KMS-managed keys— SSE-KMS
  \> Customer Provided Keys — SSE-C**
- Client Side Encryption
  \> **AWS KMS-managed customer master keys**
  \> **Customer-supplied client-side master keys**

## [S3 Transfer Acceleration](https://docs.aws.amazon.com/AmazonS3/latest/userguide/transfer-acceleration.html)

在 End User 跟 S3 之間建立快速安全轉移檔案的橋樑，利用 CloudFront Edge Network。

## [Cross Region Replication](https://docs.aws.amazon.com/AmazonS3/latest/userguide/replication.html)

自動、異步 copy 到其他 region 的 S3 buckets。

## [DataSync](https://aws.amazon.com/tw/datasync/?whats-new-cards.sort-by=item.additionalFields.postDateTime&whats-new-cards.sort-order=desc)

- 從 on-premises 到 AWS，移動 **large amounts** 的 data
- 跟 **NFS-** 和 **SMB-compatible** file systems 一起使用
- **Replication** 可以 hourly, daily, 或 weekly 完成
- 安裝 **DataSync agent** 才能監測 custom metrics 或做 replication
- 可以被用來 replicate **EFS to EFS**.

## [CloudFront](https://docs.aws.amazon.com/zh_tw/AmazonCloudFront/latest/DeveloperGuide/Introduction.html)

一種 Web 服務，可將 .html、.css、.js 和影像檔案等靜態與動態內容加速發佈給使用者。其中，Origin access identity 可以確保使用者只用 CloudFront 的內容，也就是在區分付費與一般使用者時的服務。

## [Snowball](https://aws.amazon.com/tw/getting-started/hands-on/migrate-petabyte-scale-data/services-costs/#:~:text=Description%3A Snowball is a petabyte,transfer times%2C and security concerns.)

Large amount 資料轉移進 and 出 AWS S3。

- Snowball: Petabyte-scale data transport，目前有 50TB 或 80TB size
- Snowball Edge: 100TB data transfer，還有其他強化版 Snowball 的功能，請參 [Snowball vs Snowball Edge](https://docs.aws.amazon.com/snowball/latest/ug/device-differences.html)
- Snowmobile: Exabyte-scale data transfer

## [Storage Gateway](https://aws.amazon.com/tw/storagegateway/?whats-new-cards.sort-by=item.additionalFields.postDateTime&whats-new-cards.sort-order=desc)

混合雲 (Hybrid) 儲存服務。

- File Gateway — flat files，直接存取在 S3
- Volume Gateway
  \> Stored Volumes — 整個 Dataset 就地儲存，且易步 backup 到 S3。
  \> Cached Volumes — 整個 Dataset 儲存在 S3，最常使用的資料就地快取。
- Gateway Virtual Tape Library

## [Athena](https://aws.amazon.com/tw/athena/?nc1=h_ls&whats-new-cards.sort-by=item.additionalFields.postDateTime&whats-new-cards.sort-order=desc) vs. [Macie](https://aws.amazon.com/tw/macie/)

- Athena: 互動式查詢服務，SQL, Serverless，常用來分析 log
- Macie: 資料安全與隱私服務，利用 AI 處理 [PII](https://zh.wikipedia.org/wiki/個人可識別資訊)

# 三、EC2

[⏫](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0838) [🔼](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0129) [🔽](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#66c1)

## [EC2](https://docs.aws.amazon.com/zh_tw/AWSEC2/latest/UserGuide/concepts.html) Basics

EC2 提供可擴展的運算容量

## **EC2 Basics 考試要點**

- EC2 提供可擴展的運算容量
- EC2 價格模型

1. On Demand
   允許我們給付基於每小時（甚或每秒）固定 rate，無須 commitment。
2. Reserved
   提供我們 1 年或 3 年期的容量 reservation，並有大量折扣
   \> 分成 Standard & Convertible，差別在 Convertible 可以改變 Instance Type，Standard 不行。
   \> 可以利用 Reserved Instance Market 來賣用不到的 instance。
3. Spot
   允許我們 bid 價格，價格上便宜很多，但可能會隨時被 terminate，適用於可以被中斷的服務。
   \> 想要降低被 terminate 的可能，可以提高 Spot Price
   \> 可以藉由 Spot Block 來阻止 instance 被 terminate
   \> Spot fleet 是 Spot Instance 和有時 Demand Instance 的 collection
4. Dedicated Hosts
   專用 Physical EC2 server。

## [EBS](https://aws.amazon.com/tw/ebs/?trkCampaign=acq_paid_search_brand&sc_channel=ps&sc_campaign=acquisition_TW&sc_publisher=Google&sc_category=Storage&sc_country=TW&sc_geo=CHNA&sc_outcome=acq&sc_detail=aws block storage&sc_content={adgroup}&sc_matchtype=e&sc_segment=490432929100&sc_medium=nACQ-P|PS-GO|Brand|Desktop|SU|Storage|Solution|TW|EN|Sitelink&s_kwcid=AL!4422!3!490432929100!e!!g!!aws block storage&ef_id=Cj0KCQjwhr2FBhDbARIsACjwLo3Lxmcj4zSZisHXZOSJaKHQtdTqW4LNy9EWv75usc3MZBzQKJlKF64aAgvmEALw_wcB:G:s&s_kwcid=AL!4422!3!490432929100!e!!g!!aws block storage&ebs-whats-new.sort-by=item.additionalFields.postDateTime&ebs-whats-new.sort-order=desc)

高效能區塊儲存服務，專為 EC2 搭配使用而設計，能以任何規模同時用於輸送量和交易密集型工作負載。

## **EBS 考試要點**

- EBS Storage 種類
  \> General Purpose (SSD): 大部分情況
  \> Provisioned IOPS (SSD): High Performance — Databases
  \> Throughput Optimized (HDD): Streaming Workloads — Big data, data warehouse
  \> Cold (HDD): Less frequently Access
  \> Magnetic: 不常訪問的資料
- SSD vs HDD
  \> SSD: small, random I/O operation
  \> HDD: large, sequential I/O operation
- Volume 存在於 EBS，想像 EBS 是虛擬硬碟。
- Snapshots 存在於 S3，想像 Snapshots 是硬碟的照片。
- Snapshots 是 Volume 某個時刻的 copy，且 Snapshots 是遞增的，也就是只有跟上次 snapshot 的 block 不一樣的部分會被移到 S3。如果這是第一次 snapshot，會花比較多時間建立。
- 如果要替 EBS 建立作為 root devices 的 snapshot，最好 stop instance 再建立。然而，可以在 running instance 的情境下 take snapshot
- 可以由 Snapshot 建立 AMI
- 可以快速改變 EBS volume size，包含改變 size 或 storage type。
- Volume 會 ALWAYS 跟 EC2 instance 在同一個 AZ。
- 如果要將 EC2 Volume 從一個 AZ 移到另一個 AZ，take 一個 snapshot，由該 snapshot 建立 AMI，由 AMI 在新的 AZ launch 一個 EC2 instance。
  \> 三部曲：拍照 → AMI → EC2(new AZ)
- 如果要將 EC2 Volume 從一個 Region 移到另一個 Region，take 一個 snapshot，由該 snapshot 建立 AMI，將 AMI 複製到新的 Region，由複製的 AMI 在新的 Region launch 一個 EC2 instance。
  \> 四部曲：拍照 → AMI → 複製 AMI 到新 Region → EC2
- [AMI](https://docs.aws.amazon.com/zh_tw/AWSEC2/latest/UserGuide/ec2-instances-and-amis.html)
  種類：
  \> EBS: 非暫時的 Storage，但長時間儲存還是較適合放在 S3。
  \> Instance Storage: 又稱 Ephemeral Storage，暫時的 Storage。
  Instance 狀態
  \> Reboot instance: 兩者皆不會丟失資料；
  \> Stop instance: EBS 會保留、Instance Storage 資料會遺失。
  \> Terminate instance: 預設來說，兩者的 ROOT volume 都會被刪除，然而，EBS 可以選擇要不要保留。
- Amazon Data Lifecycle Manager (Amazon DLM)：可以處理 EBS 的 back-up (snapshot)，也可以處理其他 AWS Resource 的 Life Cycle。

## ENI vs EN vs EFA

- [Elastic Network Interface (ENI)](https://docs.aws.amazon.com/zh_tw/AWSEC2/latest/UserGuide/using-eni.html)：基本的 networking
- [Enhanced Networking (EN)](https://docs.aws.amazon.com/zh_tw/AWSEC2/latest/UserGuide/enhanced-networking.html)：速度 10~100Gbps，high throughput
- [Elastic Fabric Adapter (EFA)](https://aws.amazon.com/tw/hpc/efa/)：加速 HPC 和 machine learning 的應用

## EC2 [Hibernate](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html)

EC2 Hibernate 將 instance memory(RAM) 的內容儲存到 EBS。

## [CloudWatch](https://aws.amazon.com/tw/cloudwatch/) vs [CloudTrail](https://aws.amazon.com/tw/cloudtrail/)

- CloudWatch: 監控服務，監控 AWS Resources 以及 Application，如 db instance 的 CPU Utilization，主要用於**監控 performance**。
  Custom Metrics:
  \> Memory utilization
  \> Disk swap utilization
  \> Disk space utilization
  \> Page file utilization
  \> Log collection
- CloudTrail: 檢視 user 跟 resource 之間的 API 的運作，可以辨識出哪個 user 和 account 呼叫 AWS，主要用於**監控 (audit) 在 AWS 平台的 API calls**。

## [EFS](https://aws.amazon.com/tw/efs/)

File storage 服務，讓你可以共享檔案資料，資料可以儲存 across multi-AZ（單一 region），[POSIX](https://zh.wikipedia.org/zh-tw/可移植操作系统接口)。

## [FSx for Windows](https://aws.amazon.com/tw/fsx/windows/) vs [FSx for Lustre](https://aws.amazon.com/tw/fsx/lustre/)

- [EFS](https://aws.amazon.com/tw/efs/): File 相關 storage、Linux
- Amazon FSx for Windows: Window 相關 storage
- FSx for Lustre: 聯想 HPC(High Performance Computing)、parallel file system storage、machine learning

## [EC2 Placement Groups](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html)

- Clustered Placement Group: Low Network Latency / High Network Throughput
- Spread Placement Group: 獨立 Critical EC2 instances
- Partitioned: Multiple EC2 instance HDFS, HBase 和 Cassandra

## [HPC(high-performance computing) on AWS](https://aws.amazon.com/tw/hpc/)

- Data transfer
  \> Snowball, Snowmobile (**terabytes/petabytes** 的資料)
  \> **AWS DataSync** 來儲存於 S3, EFS, FSx for Windows 等.
  \> Direct Connect
- Compute and networking
  \> EC2 instances 且為 **GPU** 或 **CPU** optimized
  \> **EC2 fleets** (Spot Instances 或 Spot Fleets)
  \> Placement groups (cluster placement groups)
  \> Enhanced networking single root I/O virtualization (SR-IOV)
  \> Elastic Network Adapters or Inter 82599 Virtual Function (VF) interface
  \> Elastic Fabric Adapters
- Storage
  \> Instance-attach Storage: EBS, Instance Storage
  \> Network Storage: S3, EFS, FSx for Lustre
- Orchestration and automation:
  \> Batch
  \> Parallel Cluster

## [WAF](https://aws.amazon.com/tw/waf/)

Web 應用程式防火牆，防範 Hacker，考試常將 WAF 跟 Network ACL 放在一起討論，皆為用於 block malicious IP addresses。

# 四、Database

[⏫](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0838) [🔼](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#fa81) [🔽](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#015c)

## [Database](https://aws.amazon.com/tw/products/databases/) Basics

- AWS Database Types
  \> RDS（口訣記憶：SM-POAM，唸起來有點像 SM Poem，SM 詩）: SQL, MySQL, PostgreSQL, Oracle, Aurora, MariaDB
  \> DynamoDB (No SQL)
  \> RedShift OLAP
- RedShift: Business Intelligence 或 Data Warehousing，只能在 1 個 AZ。
- [ElastiCache](https://aws.amazon.com/tw/elasticache/) 用來加速現有 DB 的效能(frequent identical queries)，in-memory cache。
  \> Memcached: 若想 scale horizontally，選 Memcached。
  \> Redis: Multi-AZ，可以做 backups 和 restore。

## Multi-AZ vs Read Replica

- [Multi-AZ](https://aws.amazon.com/tw/rds/features/multi-az/): 災難處理 → Disaster Recovery, DR
- [Read Replica](https://aws.amazon.com/tw/rds/features/read-replicas/): 提升效能，異步 → Performance、Asynchronous

## DynamoDB

- No SQL, key-value, schema
- 儲存在 SSD Storage
- 分佈 3 geographically distinct data centers
- Eventual Consistent Reads (Default)：效能佳
- Strongly Consistent Reads：降低效能，但是最 up-to-date 的資料
- Dynamo Stream：開啟後，可以偵測到 DynamoDB 的改變
- 可以跟 Lambda 一起應用
- 跟 CloudFront 不相容

## DynamoDB Accelerator

- Fully managed, highly available, in-memory cache
- millisecond 到 microsecond

## [Database Migration Service (DMS)](https://aws.amazon.com/tw/dms/)

將資料庫移到 AWS

## Caching Strategies on AWS

- CloudFront
- API Gateway
- ElastiCache — **Memcached** and **Redis**
- DynamoDB Accelerator (DAX)

## [EMR](https://docs.aws.amazon.com/zh_tw/emr/latest/ManagementGuide/emr-what-is-emr.html)

EMR 用於簡化大數據的處理（例如Apache Hadoop 和 Apache Spark），像是分析 log files、ML、data transformations (ETL) 等。

# 五、Route 53

[⏫](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0838) [🔼](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#66c1) [🔽](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#7d1f)

## [Route 53](https://aws.amazon.com/tw/route53/#:~:text=Amazon Route 53 是一種,網際網路應用程式。) Basics

Route 53 是 DNS 服務（將我們一般看到的網址轉換成電腦懂的 IP 地址）

## CName vs Alias

- CName: 又稱 Canonical Name 用來解決一個 domain 到另一個 domain，如 mobile 版的網址可能為 http://m.domain.com 或 http://mobile.domain.com，兩者會指向同一個。
- Alias: 跟 CName 很像，差別在於 CName 不能用於 naked domain name。兩者之間一定要選一個的話，選擇 Alias。

## Route 53 種類

- Simple Routing: 只能有一個 record 對應多個 IP Address，隨機分配，沒有 health check（相比 Multivalue Answer Routing 為有 health check 的 Simple Routing）。
- Weighted Routing: 可以選擇你想要的比例，比如 A: 30%, B: 70 %。
- Latency-based Routing: 依照延遲判斷，往延遲低的地方送。
- Failover Routing: 當設定為 Passive/Active 時，比如：你的 primary site 是EU-EAST-1，Secondary 的 [DR](https://zh.wikipedia.org/wiki/灾难恢复) site 是 AP-SOUTHEAST-2，Route 53 會確認 Primary Site 的 health check，如果壞了會自動導到 Secondary Site。
- Geolocation Routing: 依照地理位置。
- Geoproximity Routing (Traffic Flow Only): 強化版的 Geoloaction Routing
- Multivalue Answer Routing: 有Health Check 的 Simple Routing，可以說是強化版的 Simple Routing。

## Route 53 vs ELB

Route53 和 ELB 都是用來處理 network traffic。

- ELB 處理 Multiple AZ 但沒有 Multiple Regions；Route53 處理 Multiple Regions。簡單來說，ELB 目的在處理在同一個 Region 不同 EC2 instances 的 load balance；Route 53 又稱 DNS load balancing 目的在幫助平衡 Multiple Region 之間的 traffic
- Route 53 和 ELB 都可以 health check 和 route traffic 到健康的 resource。Route 53 的 weighted routing 有 health checks 和可以移除不健康的 target。然而，DNS 會快取，所以不健康的 target 仍有時會出現在使用者的快取。另一方面，ELB 不會快取，且會立即移除不健康的 target。

# 六、VPC

[⏫](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0838) [🔼](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#015c) [🔽](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#560d)

## [VPC](https://aws.amazon.com/tw/vpc/?vpc-blogs.sort-by=item.additionalFields.createdDate&vpc-blogs.sort-order=desc) Basics

- 想像 VPC 是一個在 AWS 的 logical datacenter。
- 包含 Internet Gateways (或 Virtual Private Gateways)、Route Tables，Network Access Control Lists、Subnets 和 Security Groups。
- 一個 Subnet 只能在一個 AZ
- Security Groups 是 stateful；Network Access Control Lists 是 stateless。
- 不能 TRANSITIVE PEERING
- AWS 每個 Subnet 會保留 5 個 IP Address
- 一個 VPC 只有一個 Internet Gateway (簡稱 IGW)
- Security Group 不能跨 VPCs
- 如果碰到 EC2 Instance 無法跟 Internet 連結，一般考慮兩件事：
  \> 有 EIP 或 Public IP 嗎
  \> Route Table 有設定好嗎
- 務必要學會自己從頭到尾建立 Custom VPC

## NAT Instances and NAT Gateways

- [NAT Instances
  ](https://docs.aws.amazon.com/vpc/latest/userguide/VPC_NAT_Instance.html)> 需要 Disable Source/Destination Check
  \> 必須為 Public Subnet
  \> 效能出問題提升 instance size（跟 EC2 概念有點像）
  \> 提高 availability：Autoscaling Groups, 多個 subnets 在不同 AZs 利用 script 自動化 failover。
  \> 在 Security Group 後面
- [NAT Gateways](https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html)
  \> 在 AZ 裡面
  \> 建議企業使用
  \> 不用 patch
  \> 不用關聯 Security Group
  \> 自動指派 Public IP
  \> 記得更新 Route Tables
  \> 提高 availability：在每個 AZ 建立一個 NAT gateway，設定 routing 確保資源在同個 AZ 裡面使用該 NAT gateway。

## [Network Access Control Lists (NACL)](https://docs.aws.amazon.com/vpc/latest/userguide/vpc-network-acls.html) vs. [Security Groups (SG)](https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html)

- 你的 VPC 會自動產生 default ACL，並在預設情況下會 ALLOW all inbound 跟 outbound traffic.
- 你可以建立 custom ACL，並在預設情況下會 DENIES 所有 inbound 跟 outbound traffic，直到你新增 rules。
- 每個 VPC 內的 Subnet 必須跟 ACL 關聯，如果你沒有明確關聯 subnet 跟 ACL，subnet 會自動與 default ACL 關聯。
- Block IP 用 ACL，而不是 SG。
- 你可以讓多個 Subnet 跟一個 ACL 關聯。
- ACL 包含 Number List 的 rules，從小的數字開始。
- ACL 有分開的 inbound 跟 outbound rule。
- ACL 是 Stateless，SG 是 Stateful。
- ACL 是在 Subnet 層面（管理進出 subnet），SG 是在 Instance 層面（管理進出 instance）。

## [VPC Flow Logs](https://docs.aws.amazon.com/zh_tw/vpc/latest/userguide/flow-logs.html)

擷取傳入及傳出 VPC 中網路界面的 IP 流量相關資訊

## [Direct Connect](https://aws.amazon.com/tw/directconnect/)

Dedicate network connect 從 on-premise 到 AWS 的專用網路連線

## [Global Accelerator](https://aws.amazon.com/tw/global-accelerator/?blogs-global-accelerator.sort-by=item.additionalFields.createdDate&blogs-global-accelerator.sort-order=desc&aws-global-accelerator-wn.sort-by=item.additionalFields.postDateTime&aws-global-accelerator-wn.sort-order=desc)

Accelerate 來優化 availability & performance，自動將流量重新路由至最近的狀況良好的可用端點。適用於 non-HTTP 案例，如 gaming(UDP), IoT(MQTT) 或 Voice over IP，沒有直接方法跟 S3 一起應用。

## [VPC Endpoint](https://docs.aws.amazon.com/zh_tw/vpc/latest/privatelink/vpc-endpoints.html)

能夠將 VPC 私密地連接到中端節點服務

- [VPC Gateway Endpoint](https://docs.aws.amazon.com/vpc/latest/privatelink/vpce-gateway.html): 支援 S3 & DynamoDB
- [VPC Interface Endpoint](https://docs.aws.amazon.com/vpc/latest/privatelink/vpce-interface.html): 允許連接 PrivateLink 授權的服務

## [PrivateLink](https://aws.amazon.com/tw/privatelink/?privatelink-blogs.sort-by=item.additionalFields.createdDate&privatelink-blogs.sort-order=desc)

- 看到 Peering VPC 讓成千讓萬 Customer VPC 使用，就聯想 PrivateLink
- 不需要 VPC Peering，不用 route tables、NAT、IGW 等。
- 需要 Network Load Balancer 在 service VPC 和 ENI 在 customer VPC

## [AWS Transit Gateway](https://aws.amazon.com/tw/transit-gateway/?whats-new-cards.sort-by=item.additionalFields.postDateTime&whats-new-cards.sort-order=desc)

用於簡化網路與複雜架構。

## [AWS VPN CloudHub](https://docs.aws.amazon.com/zh_tw/vpn/latest/s2svpn/VPN_CloudHub.html)

如果有多條 AWS 站台對站台 VPN 連接，可使用 AWS VPN CloudHub 提供站台間的安全通訊。[Hub-and-spoke model](https://inboundmarketing.com.tw/content-marketing/the-hub-and-spoke內容經營.html#:~:text=內容行銷效率！-,什麼是Hub-and-Spoke 軸輻式放射狀,運輸成本的商業模式。)，經過 public internet，但所有 customer gateway 和 VPN CloudHub 的 traffic 都是加密的狀態。

## AWS Network Costs

- Private IP 比 Public IP 便宜
- 如果想要節省 network 的費用，可以將所有的 EC2 instances 放在同一個 AZ，並使用 Private IP Address，但要注意 failure 問題。

# 七、HA(High Available) Architecture

[⏫](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0838) [🔼](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#7d1f) [🔽](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0fdb)

## [Elastic Load Balancers](https://aws.amazon.com/tw/elasticloadbalancing/?whats-new-cards-elb.sort-by=item.additionalFields.postDateTime&whats-new-cards-elb.sort-order=desc)

Regional resources，不能 span across region

- Application: Path-based, Layer 7 → Path-based
- Network: 要求效能，Layer 4 → High performance or traffic
- Classic: Sticky session, 過時
- 504 Error 是 gateway 逾時
- 如果需要 IPv4，尋找 **X-Forwarded-For** header。

## [Auto Scaling](https://aws.amazon.com/tw/autoscaling/)

監控應用程式並自動調整容量，Scale Option：

- 維持現有 instance level
- Scale 手動
- Scale 根據 schedule
- Scale 根據 demand
- 預測 scaling

如果想要 launch Auto Scaling Group，你可以使用 Launch Configuration（注意：一旦建立了 Launch Configuration 就不能改變），如果想要更新 Auto Scaling Group，必須重新建立一個 Launch Configuration，然後利用新的 Launch Configuration 更新 Auto Scaling Group。

## HA Architecture

- 永遠準備好 Failure
- 盡可能使用 Multi AZs 和 Multi Regions
- 知道 RDS 中 Multi-AZ 和 Read Replicas 的差別
  Multi AZ: Disaster Recovery
  Read Replicas: 增加效能
- [知道 scaling out 和 scaling up 的差別](https://jaminzhang.github.io/architecture/Server-Scalability-Sacle-Out-Sacle-Up/)
- 知道 S3 Storage classes 的差別
- 仔細閱讀題目並記得考慮成本因素

## [Elastic Beanstalk](https://aws.amazon.com/tw/elasticbeanstalk/)

自動處理部署，包括容量佈建、負載平衡、自動調整規模，以及應用程式運作狀態監控。

## High Availability with Bastion Hosts

- 2 hosts 在 2 個不同的 AZ，用 Network Load Balancer 與 Static IP 和 Health check 處理 failover 從一個 host 導到另一個 host。
- 不能用 Application Load Balancer，因 ALB 為 layer 7，這裡需要 layer 4。
- 1 個 host 在 1 個 AZ，且在 Auto Scaling group 後面，搭配 health check 和 EIP。如果 host fail 了，health check 會 fail，Auto Scaling Group 會在不同的 AZ 產出新的 instance，可以用 script 處理 EIP 指向新的 Host，會是最便宜的選擇，但不是 100% fault tolerant。

## On-Premises 策略

- Database Migration Service (DMS)
- Server Migration Service (SMS)
- AWS Application Discovery Service
- VM Import/Export
- Download Amazon Linux 2 作為 ISO

# 八、Application

[⏫](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0838) [🔼](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#560d) [🔽](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#3a9f)

## [Simple Queue Service (SQS)](https://docs.aws.amazon.com/zh_tw/AWSSimpleQueueService/latest/SQSDeveloperGuide/welcome.html)

- SQS 是 pull-based，不是 pushed-based
- [Visibility timeout](https://docs.aws.amazon.com/zh_tw/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html)、[Long polling/Short polling](https://docs.aws.amazon.com/zh_tw/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-short-and-long-polling.html) 概念常考
- 如果看到 decouple 相關字眼，想到 SQS

## [Simple Workflow Service (SWF)](https://docs.aws.amazon.com/zh_tw/amazonswf/latest/developerguide/swf-welcome.html)

建立在 distributed 元件間協調工作的應用程式。使用情境：應用程式的步驟需要 500 毫秒以上才能完成，需要追蹤處理的狀態，並在 failure 時恢復或重試。

## [Simple Notification Service (SNS)](https://aws.amazon.com/tw/sns/?whats-new-cards.sort-by=item.additionalFields.postDateTime&whats-new-cards.sort-order=desc)

瞬間, push-based delivery (no polling)

## [Elastic Transcoder](https://aws.amazon.com/tw/elastictranscoder/)

雲端中的 media transcoder，將 media 原始 source 轉成 smartphone, tablet 等讀得出來的格式。

## [API Gateway](https://aws.amazon.com/tw/api-gateway/)

建立、維護和保護任何規模的 API

- 想像 API Gateway 如門口
- API Gateway 有 caching 功能來增加效能
- API Gateway 是低成本且自動 scale
- 可以 throttle API Gateway 來抑制攻擊
- 可以將 result 存到 CloudWatch
- 如果用 Javascript/AJAX，記得在 Enable CORS

## [Kinesis](https://aws.amazon.com/tw/kinesis/)

Streaming Data，可以搭配 Lambda 應用，因為 Lambda 具有處理 patch 的彈性。

- Kinesis Streams: 即時資料串流服務
- Kinesis Firehose: 串流資料載入資料湖 (data lake)，資料存放、分析服務 → Store streaming data
- Kinesis Analytics: 即時轉換、分析串流資料 → Process streaming data

## [Web Identity Federation](https://docs.aws.amazon.com/zh_tw/IAM/latest/UserGuide/id_roles_providers_oidc.html) and [Cognito](https://aws.amazon.com/tw/cognito/)

- Federation 允許 user 經由 Web Identity Provider 如 Google, FB, Amazon 驗證
- User 先跟 Web ID Provider 驗證並收到一個驗證 token，會交換一個暫時的 AWS Credential 允許他們成為 IAM role。
- Cognito 是一個 Identity Broker 處理 application 跟 Web ID Provider 之間的作用。
- User Pool 是 user based，處理 user 註冊、驗證、帳號回復。
- Identity Pool 授權 AWS resources 的使用權。

# 九、Security

[⏫](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0838) [🔼](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0fdb) [🔽](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#cd69)

## 降低 Security 的威脅

- Network Access Control Lists (NACL)
- Application Load Balancer (ALB)
- Network Load Balancer (NLB)
- Web Application Firewall (WAF)
- WAF + CloudFront

## [Key Management Service (KMS)](https://aws.amazon.com/tw/kms/)

建立和控制用來加密和數位簽署資料的金鑰

- Customer Managed CMK：允許 key **rotation**（一年一次），由 key polices 控制 enabled / disabled（預設為 disabled）。
- AWS Managed CMK：免費，當選擇 encryption 時此為預設選項，不允許管理，自動 rotate（三年一次）。
- AWS Owned CMK：被 AWS 共享於眾多帳號，基本上我們不會看到。

## [CloudHSM](https://aws.amazon.com/tw/cloudhsm/)

AWS 雲端的受管硬體安全模組 (HSM)。FIPS 140–2 Level 3

## [System Manager Parameter Store](https://docs.aws.amazon.com/zh_tw/systems-manager/latest/userguide/systems-manager-parameter-store.html)

提供安全的階層式儲存空間，進行組態資料管理和秘密管理。

## [Secrets Manager](https://aws.amazon.com/tw/secrets-manager/)

在生命週期過程中輪換、管理和擷取資料庫登入資料、API 金鑰及其他機密。可以開啟自動 rotate。

## [AWS Shield](https://aws.amazon.com/tw/shield/?whats-new-cards.sort-by=item.additionalFields.postDateTime&whats-new-cards.sort-order=desc)

防護 DDoS 的攻擊

## [Web Application Firewall (WAF)](https://aws.amazon.com/tw/waf/)

防護 Hacker 的攻擊，如：SQL injection, cross-site scripting attack，監測 forward 到 API Gateway, CloudFront, ALB 的 HTTP/HTTPS request，也可以應對 web application layer 的 DDoS 攻擊。

# 十、Serverless

[⏫](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#0838) [🔼](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#3a9f) [🔽](https://medium.com/cloud-guru-的征途/考試篇-aws-saa-c02-大統整-一篇文章講完所有內容重點-868be49ae9ed#5175)

## [Lambda](https://aws.amazon.com/tw/lambda/)

- 自動 scale out (not up)
- Serverless
- 哪些 Service 是 Serverless
  DynamoDB, S3, Lambda, API Gateway, Aurora — Serverless
  EC2, RDS — Not Serverless
- Lambda function 可以觸發其他 Lambda function
- 架構變得太複雜，可以利用 [AWS X-ray](https://aws.amazon.com/tw/xray/) 來 debug 和分析 microservice 的 application
- RDS 不能觸發 Lambda
- AWS Step Functions 是 serverless 函數協調器，可將 AWS Lambda 函數和多個 AWS 服務排序至業務關鍵應用程式。

## [Elastic Container Service (ECS)](https://aws.amazon.com/tw/ecs/?whats-new-cards.sort-by=item.additionalFields.postDateTime&whats-new-cards.sort-order=desc&ecs-blogs.sort-by=item.additionalFields.createdDate&ecs-blogs.sort-order=desc)

高度安全、可靠和可擴展的容器執行方式。

## [Fargate](https://aws.amazon.com/tw/fargate/?whats-new-cards.sort-by=item.additionalFields.postDateTime&whats-new-cards.sort-order=desc&fargate-blogs.sort-by=item.additionalFields.createdDate&fargate-blogs.sort-order=desc)

Serverless compute engine，搭配 ECS 與 EKS 使用的容器。

## [Elastic Kubernetes Service (EKS)](https://aws.amazon.com/tw/eks/?whats-new-cards.sort-by=item.additionalFields.postDateTime&whats-new-cards.sort-order=desc&eks-blogs.sort-by=item.additionalFields.createdDate&eks-blogs.sort-order=desc)

在 AWS 處理、執行 Kubernetes。