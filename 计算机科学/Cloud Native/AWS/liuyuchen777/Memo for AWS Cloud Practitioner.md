[TOC]

- https://liuyuchen777.github.io/2021/10/04/AWS-CP-Study-Note/

# Memo for AWS Cloud Practitioner

## 【Cloud Concepts】

### Why cloud computing?

On-premise:

- You own the servers
- You hire the IT people
- You pay or rent the real-estate
- You take all the risk

Cloud providers:

- Someone else owns the servers
- Someone lese hires It people
- Someone else pays or rents the real-estate
- You are responsible for **your configuring cloud services and code** , and someone else takes care of the rest

### **Six advantages** and benefits of cloud computing

- trade capital expense for variable expense:

**no upfront cost**

**pay on-demand**

- benefit from massive economies of scale

sharing the cost with other customers

当AWS业务扩大的时候，经济规模效应将普及每一位用户

- stop guessing capacity

instead of paying of idle or underutilized servers

- increase speed and agility

launch resources within a few clicks in minutes

- stop spending money on running and maintaining data centers

focus on your own customers

- go global in minutes

multiple regions around the world with a few clicks

### Types of cloud computing

- SaaS: software as a service (for customers)

Office 365, Gmail

- PaaS: platform as a service (for developers)

Heroku

- IaaS: infrastructure as a service (for admins)

AWS, Google Cloud Platform, Azure

### Cloud computing development models

( Cloud

Startups

SaaS offerings

New projects for company

(2) Hybrid

Bank

Fintech

Large professional service providers

(3) On-Premise

Large enterprise

Public sector

Super sensitive data (ex: hospital)

## 【AWS Global Infrastructure】

### AWS global infrastructure

- Regions: physical location in the world with multiple availability zones

  - every region is **physically isolated** from and independent of every other region in terms of location, power, water supply
  - each region has at least **2 AZs**
  - not all services are available in every region
  - **US-EAST (the region)** include almost all the services
  - AWS largest region is **US-EAST**
  - **US-EAST- * is the region where you see all your billing information

- Availability Zones (AZ): one or more discrete data centers

  - AZs are represented by a region code, followed by a letter identifier: US-EAST-
  - Multi-AZ: distributing your instances across multiple AZs allows failover configuration for handling requests when one goes down
  - **< ms** latency between AZs

- Edge Location: datacenter owned by a

   

  trusted partner

   

  of AWS (much more than AZ)

  - get data fast or upload data fast to AWS
  - the locations serve requests for **CloudFront** and **Route 53**. Requests going to either of these services will be routed to the nearest edge location automatically
  - **S3 Transfer Acceleration traffic** and **API Gateway** endpoint traffic also use the AWS Edge Network
  - this allows for low latency no matter where the end user is geographically located

### GovCloud (Only for US from now)

- AWS GovCloud Regions allow customers to host **sensitive** Controlled Unclassified Information and other types of regulated workloads
- GovCloud Regions are only operated by employees who are US citizens, on US soil
- They are only accessible to US entities and root account holders who pass a screening process
- Service sell for US government

## 【AWS Services】In detail

### Compute

#### EC2

##### Cost Optimization: EC2 Right Sizing

EC2 Right Sizing utilizes managed services to execute right-sizing analysis to provide detailed recommendations for more cost-saving builds and implementations of Amazon EC2 instances

##### Amazon EC2 Dedicated Hosts vs Instances

- An important difference between a Dedicated Host and a Dedicated instance is that a **Dedicated Host gives you additional visibility and control over how instances are placed on a physical server, and you can consistently deploy your instances to the same physical server over time**.
- As a result, Dedicated Hosts enable you to use your existing server-bound software licenses and address corporate compliance and regulatory requirements.
- 只有dedicated host才能使用已有的software licenses

#### EC2 Auto-Scaling

- monitor your applications and automatically adjusts capacity to maintain steady, predictable performance at the lowest possible cost
- scalability would scale-out (increase the number of instances) or scale in (decrease the number of instances)
- when one of the instances is with the least load it will divert the traffic to the least loaded instance

#### Amazon Elastic Container Service (ECS)

A highly scalable, high-performance container orchestration service that supports Docker containers and allows you to easily run and scale containerized applications on AWS. Amazon ECS eliminates the need for you to install and operate your own container orchestration.

##### Amazon Elastic Container Registry (ECR)

- A fully-managed Docker container registry that makes it easy for developers to store, manage, and deploy Docker container images.
- You pay only for the amount of data you store in your repositories and data transferred to the Internet.

##### Amazon Elastic Container Service for Kubernetes (EKS)

- makes it easy to deploy, manage, and scale containerized applications using Kubernetes on AWS.
- Amazon EKS runs the Kubernetes management infrastructure for you across multiple AWS availability zones to eliminate a single point of failure.

#### AWS Lambda

- **a serverless service** that makes it possible to run event-triggered functions on Edge Locations within the AWS Content Delivery Network
- an administrator can introduce decision-making and compute processing closer to the viewer's location, thereby improving on their browsing experience

#### Elastic Beanstalk (部署)

- make it even easier for developers to quickly deploy and manage applications in the AWS Cloud
- simply upload your code, and AWS Elastic Beanstalk will automatically handles the deployment
- upload your code and AWS Elastic Beanstalk **automatically handles the deployment** , from capacity provisioning, load balancing and auto scaling to application health monitoring.

#### Amazon Lightsail

Lightsail is an easy-to-use cloud platform that **offers you everything needed to build an application or website, plus a cost-effective, monthly plan**. Whether you're new to the cloud or looking to get on the cloud quickly with AWS infrastructure you trust, we've got you covered.

类似于集成方案

#### AWS Batch

Enables developers, scientists, and engineers to easily and efficiently run hundreds of thousands of **batch computing jobs** on AWS.

- no need to install and manage batch computing software or server clusters that you use to run your jobs, allowing you to focus on analyzing results and solving problems

#### AWS Fragate (去虚拟机，直接操作container)

- AWS Fargate is a compute engine for Amazon ECS that allows you to run containers without having to manage servers or clusters.
- With AWS Fargate, you no longer have to provision, configure, and scale clusters of virtual machines to run containers.
- AWS Fargate removes the need for you to interact with or think about servers or clusters. Fargate lets you focus on designing and building your applications instead of managing the infrastructure that runs them.

### Developer Tools

#### AWS CodeCommit

- AWS CodeCommit is a fully-managed [source control](https://aws.amazon.com/devops/source-control/) service that hosts **secure Git-based repositories**. It makes it easy for teams to collaborate on code in a secure and highly scalable ecosystem. CodeCommit eliminates the need to operate your own source control system or worry about scaling its infrastructure. You can use CodeCommit to securely store anything from source code to binaries, and it works seamlessly with your existing Git tools.

#### AWS CodeDeploy

- a deployment service that **automates application deployments** to Amazon EC2 instance, on-premises instance, serverless Lambda function, or Amazon ECS services
- AWS CodeDeploy is a fully managed deployment service that automates software deployments to a variety of compute services such as Amazon EC2, AWS Fargate, AWS Lambda, and your on-premises servers. AWS CodeDeploy makes it easier for you to rapidly release new features, helps you avoid downtime during application deployment, and handles the complexity of updating your applications. You can use AWS CodeDeploy to automate software deployments, eliminating the need for error-prone manual operations. The service scales to match your deployment needs.

#### AWS CodeStar

Unified user interface, manage software deployment in one place

- provide a unified user interface, enabling you to easily **manage your software development activities** in on place
- you can set up your entire continuous **delivery toolchain in minutes** , allowing you to start releasing code faster
- make it easy for your whole team to work together securely, allowing you to easily manage access and add owners, contributors, and viewers to your projects

#### AWS CodeBuild

- a fully managed service that primarily **compiles source code, runs unit tests and generate software package** with the output being artifacts that will be ready for deployment

#### AWS CodePipeline

- a fully managed continuous delivery service that helps you automate your release pipelines for fast and reliable application and infrastructure updates.
- CodePipeline automates the **build, test, and deploy** phases of your release process every time **there is a code change** , based on the release model you define.

#### AWS X-Ray

- AWS X-Ray helps developers analyze and debug production, distributed applications, such as those built using a microservices architecture.
- With X-Ray, you can understand how your application and its underlying services are performing to identify and troubleshoot the root cause of performance issues and errors. X-Ray provides an end-to-end view of requests as they travel through your application, and shows a map of your application's underlying components.
- You can use X-Ray to analyze **both applications in development and in production** , from simple three-tier applications to complex microservices applications consisting of thousands of services.
- Thousands of application

#### AWS Cloud9

- AWS Cloud9 is a **cloud-based integrated development environment (\**\*\*IDE\*\**\*)** that lets you write, run, and debug your code with just a browser. It includes a code editor, debugger, and terminal.
- Cloud9 comes prepackaged with essential tools for popular programming languages, including JavaScript, Python, PHP, and more, so you don't need to install files or configure your development machine to start new projects. Since your Cloud9 IDE is cloud-based, you can work on your projects from your office, home, or anywhere using an internet-connected machine.
- Cloud9 also provides a seamless experience for developing **serverless applications** enabling you to easily define resources, debug, and switch between local and remote execution of serverless applications. With Cloud9, you can quickly share your development environment with your team, enabling you to pair program and track each other's inputs in real time.

### Storage

#### Amazon Simple Storage Service (S3)

an object storage service that offers industry-leading scalability, data availability, security, and performance

for use cases such as website, mobile applications, backup and restore, archive, enterprise applications, IoT devices and big data analytics.

99.999999999% ( 9) durability

##### S3 Storage Class

【mission-critical production data, for frequent access】

S3 Standard

S3 Intelligent-Tiering

【save costs by storing infrequently accessed data】

S3 Standard-Infrequent Access (S3 Standard-IA)

S3 One Zone-Infrequent Access (S3 One Zone-IA)

【archive data at the lowest costs in the archival storage classes】

Amazon S3 Glacier (S3 Glacier)

Amazon S3 Glacier Deep Archive (S3 Glacier Deep Archive)

S3 Outposts: you can use the S3 Outposts storage class to store your S3 data on-premises using S3 on Outposts

##### Amazon S3 Intelligent-Tiering

- S3 Intelligent-Tiering is a new Amazon S3 storage class designed for customers who want to optimize storage costs **automatically when data access patterns change** , without performance impact or operational overhead.
- S3 Intelligent-Tiering is the first cloud object storage class that delivers automatic cost savings by moving data between two access tiers — **frequent access and infrequent access** — when access patterns change, and is ideal for data with unknown or changing access patterns.
- S3 Intelligent-Tiering stores objects in two access tiers: one tier that is optimized for **frequent access and another lower-cost** tier that is optimized for infrequent access. For a small monthly monitoring and automation fee per object, S3 Intelligent-Tiering monitors access patterns and moves objects that have not been accessed for 30 consecutive days to the infrequent access tier. There are no retrieval fees in S3 Intelligent-Tiering. If an object in the infrequent access tier is accessed later, it is automatically moved back to the frequent access tier. No additional tiering fees apply when objects are moved between access tiers within the S3 Intelligent-Tiering storage class. S3 Intelligent-Tiering is designed for 99.9% availability and 99.999999999% durability, and offers the same low latency and high throughput performance of S3 Standard.

##### Amazon S3 Lifecycle policies

- are used to automatically transition objects through different storage classes in accordance to a preconfigured rule
- will typically move the object regardless of how frequently it is accessed
- are automatically triggered when the 'days after creation' period lapses
- can be configured to permanently delete objects

##### Amazon S3 Object Versioning

- use versioning to keep multiple versions of an object in on bucket
- protect you from the consequences of unintended overwrites and deletions

##### S3 Storage Class Analysis

monitor access patterns across objects to discover data that should be moved to lower-cost storage classes

#### Amazon S3 Glacier Deep Archive

- S3 Glacier Deep Archive is a new [Amazon S3 storage class](https://aws.amazon.com/s3/storage-classes/) that provides secure and durable object storage for **long-term retention of data** that is accessed once or twice in a year. From just $0.00099 per GB-month (less than one-tenth of one cent, or about $ per TB-month), S3 Glacier Deep Archive offers the lowest cost storage in the cloud, at prices significantly lower than storing and maintaining data in on-premises magnetic tape libraries or archiving data off-site.

#### Amazon Elastic Block Store (EBS)

provides persistent block storage volumes for **use with Amazon EC2 instances** in the AWS Cloud.

- Each Amazon EBS volume is **automatically replicated within its Availability Zone** (Same AZ) to protect you from component failure, offering high availability and durability.
- Amazon EBS volumes offer the **consistent and low-latency performance** needed to run your workloads.

#### Amazon Elastic File System (EFS)

provides a simple, scalable, elastic file system for **Linux-based workloads** for use with AWS Cloud services and on-premises resources.

- You can access your file systems across AZs and regions and share files between thousands of Amazon EC2 instances and on-premises servers via AWS Direct Connect or AWS VPN.

#### Amazon S3 Transfer Acceleration

- enable fast, easy, and secure transfers of files over long distances between your client and an S3 bucket
- take advantage of Amazon CloudFront's globally distributed edge locations
- as the data arrives at an edge location, data is routed to Amazon S3 over an optimized network path

#### AWS Storage Gateway

a hybrid storage service that enables your on-premises applications to seamlessly use AWS cloud storage.

### Analytics

#### Amazon Elastic Map Reduce (EMR) -&gt; for big data

- Amazon EMR is the industry leading **cloud-native big data platform** for processing vast amounts of data quickly and cost-effectively at scale.
- Using open source tools such as [Apache Spark](https://aws.amazon.com/emr/features/spark/), [Apache Hive](https://aws.amazon.com/emr/features/hive/), [Apache HBase](https://aws.amazon.com/emr/features/hbase/), [Apache Flink](https://aws.amazon.com/blogs/big-data/use-apache-flink-on-amazon-emr/), [Apache Hudi (Incubating)](https://aws.amazon.com/emr/features/hudi/), and [Presto](https://aws.amazon.com/emr/features/presto/), coupled with the dynamic scalability of [Amazon EC2](https://aws.amazon.com/ec2/) and scalable storage of [Amazon S3](https://aws.amazon.com/s3/), EMR gives analytical teams the engines and elasticity to run Petabyte-scale analysis for a fraction of the cost of traditional on-premises clusters.
- EMR gives teams the flexibility to run use cases on single-purpose, short lived clusters that automatically scale to meet demand, or on long running highly available clusters using the new multi-master deployment mode. If you have existing on-premises deployments of open source tools such as Apache Spark and Apache Hive, you can also run [EMR clusters on AWS Outposts](https://aws.amazon.com/emr/features/outposts/), giving you both the ability to scale out on-premises via Outposts or in the cloud.

#### Amazon Athena

DB Search Service over S3 storage using SQL

- a serverless query service used to analyze Big Data stored in S3 **using standard SQL**
- a serverless query service that does not need to build databases on dedicated Elastic Block Store (EBS) volumes, instead, it builds tables from data read directly from Amazon S3 buckets
- does not store any of the data, is compatible with the regular data formats that include CSV, JSON, ORC, AVRO and Parquet
- Amazon Athena is an interactive query service that makes it easy to analyze data in Amazon S3 using standard SQL.
- Athena is serverless, so there is no infrastructure to manage, and you pay only for the queries that you run.

#### Amazon Kinesis

Amazon Kinesis makes it easy to collect, process, and analyze **real-time, streaming data** so you can get timely insights and react quickly to new information.

- Kinesis Video Streams
  - make it easy to securely stream video from connected devices to AWS for analytics, ML, and other processing
- Kinesis Data Streams
  - a scalable and durable real-time data streaming service that can continuously capture gigabytes of data per second from hundreds of thousands of sources
- Kinesis Data Firehose
  - the easiest way to capture, transform, and load data streams into AWS data stores for near real-time analytics with existing business intelligence tools
- Kinesis Data Analytics
  - the easiest way to process data streams in real time with SQL or Java without having to learn new programming languages or processing frameworks

#### Amazon Redshift

A fast, scalable **data warehouse** (仓库) that makes it simple and cost-effective to analyze all your data across your data warehouse and data lake.

- Redshift delivers ten times faster performance than other data warehouses by using machine learning, massively parallel query execution, and columnar storage on high-performance disk.
- You can setup and deploy a new data warehouse in minutes, and run queries across petabytes of data in your Redshift data warehouse, and exabytes of data in your data lake built on Amazon S3.

#### AWS CloudSearch

set up, manage, and scale a **search solution** for your website or application.

#### AWS Elasticsearch Service

makes it easy to deploy, secure, operate, and scale Elasticsearch to search, analyze, and visualize data in real-time.

Integrated with other service

#### Amazon QuickSight

A fast, cloud-powered **business intelligence (BI) service** that makes it easy for you to deliver insights to everyone in your organization.

#### AWS Glue

fully managed extract, transform, and load (ETL) service that makes it easy for customers to prepare and load their data for analytics.

### Database

#### Amazon RDS

makes it easy to set up, operate, and scale a relational database in the cloud. It provides cost-efficient and resizable capacity

- while **automating time-consuming administration tasks such as hardware provisioning, database setup, patching and backups**. (patch is automatically)
- 【Auto Replica】enhance the database performance and durability by allowing for automated distribution of load amongst several database instances with the exact copy of the parent database
- is best suited in scenarios where the dataset and forms are consistent such that their data schema is persistently valid
- it is best to deploy in an environment where the load can be anticipated and is somewhat finite

#### Amazon Aurora (SQL) -&gt; fully managed

- **a fully managed** (don't need customer to upgrade service), MySQL- and PostgreSQL-compatible, relational database engine
- combine the speed and reliability of high-end commercial databases with the simplicity and cost-effectiveness of open-source databases
- deliver up to five times the throughput of MySQL and up to three times the throughput of PostgreSQL without requiring changes to most of your existing applications

#### DynamoDB (NoSQL)

A key-value and document database that delivers single-digit millisecond performance at any scale. It's a fully managed, multiregion, multimaster database with built-in security, backup and restore, and in-memory caching for internet-scale applications.

#### ElastiCache

A web service that makes it easy to deploy, operate, and scale an **in-memory cache in the cloud**. The service improves the performance of web applications by allowing you to retrieve information from fast, managed, in-memory caches, instead of relying entirely on slower disk-based databases.

Amazon ElastiCache supports two open-source in-memory caching engines:

- Redis
- Memcached

### Network and Content Distribution

#### AWS VPC

- Amazon Virtual Private Cloud (Amazon VPC) lets you provision a logically isolated section of the AWS Cloud where you can launch AWS resources in a virtual network that you define.
- You have complete control over your virtual networking environment, including selection of your own IP address range, creation of subnets, and configuration of route tables and network gateways.
- You can use both IPv4 and IPv6 in your VPC for secure and easy access to resources and applications.

#### AWS PrivateLink

- simplifies the security of data shared with cloud-based applications by eliminating the exposure of data to the public Internet.
- AWS PrivateLink provides **private connectivity between VPCs, AWS services, and on-premises applications** , securely on the Amazon network.
- AWS PrivateLink makes it easy to connect services across **different accounts** and VPCs to significantly simplify the network architecture.

#### VPC Peering

- A VPC peering connection is a **networking connection between two VPCs** that enables you to route traffic between them using private IPv4 addresses or IPv6 addresses.

#### CloudFront

- a web service that speeds up **distribution of your static and dynamic web content** , such as .html, .css, .js, and image files, to your users
- deliver your content through a worldwide network of data centers called edge locations
- Amazon CloudFront is a fast **content delivery network (CDN) service** that securely delivers data, videos, applications, and APIs to customers globally with low latency, high transfer speeds, all within a developer-friendly environment. CloudFront is integrated with AWS – both physical locations that are directly connected to the AWS global infrastructure, as well as other AWS services.

#### Amazon Route53 (DNS)

- Amazon Route 53 is a highly available and scalable cloud [Domain Name System (DNS)](https://aws.amazon.com/route53/what-is-dns/) web service.
- It is designed to give developers and businesses an extremely reliable and cost effective way to route end users to Internet applications by translating names like [www.example.com](http://www.example.com/) into the numeric IP addresses like 2.0.2. that computers use to connect to each other.
- Amazon Route 53 is fully compliant with IPv6 as well.
- CloudFront works seamlessly with services including AWS Shield for DDoS mitigation, Amazon S3, Elastic Load Balancing or Amazon EC2 as origins for your applications, and Lambda@Edge to run custom code closer to customers' users and to customize the user experience. Lastly, if you use AWS origins such as Amazon S3, Amazon EC2 or Elastic Load Balancing, you don't pay for any data transferred between these services and CloudFront.

##### Route 53 routing policy

- **【**** Simple routing policy ****】** – Use for a **single resource** that performs a given function for your domain, for example, a web server that serves content for the example.com website.
- **【**** Failover routing policy ****】** – Use when you want to configure active-passive failover.
- **【**** Geolocation routing policy ****】** – Use when you want to route traffic based on the **location of your users**.
- **【**** Geoproximity routing policy ****】** – Use when you want to route traffic based on the location of your resources and, optionally, shift traffic from resources in one location to resources in another.
- **【**** Latency routing policy ** 】– Use when you have resources in multiple AWS Regions and you want to route traffic to the region that provides the** best latency**.
- **【**** Multivalue answer routing policy ****】** – Use when you want Route 53 to respond to DNS queries with up to eight healthy records selected at random.
- **【**** Weighted routing policy ****】** – Use to route traffic to multiple resources in **proportions that you specify**.

#### AWS Global Accelerator

A networking service that improves the availability and performance of the applications that you offer to your global users.

Using edge location

AWS Global Accelerator uses the highly available and **congestion-free AWS global network to direct internet traffic from your users to your applications on AWS** , making your users' experience more consistent

#### AWS API Gateway

A fully managed service that makes it easy for developers to create, publish, maintain, monitor, and secure APIs at any scale

#### AWS Transit Gateway

A service that enables customers to connect their Amazon Virtual Private Clouds (VPCs) and their on-premises networks to **a single gateway.**

#### Elastic Load Balancing

- automatically distributes incoming application traffic across multiple targets, such as Amazon EC2 instances, containers, and IP addresses.
- It can handle the varying load of your application traffic in a single Availability Zone or across multiple Availability Zones.
- Three types

##### Application Load Balancer (Layer7)

best suited for load balancing of **HTTP and HTTPS traffic** and provides advanced request routing targeted at the delivery of modern application architectures, including microservices and containers.

VPC

##### Network Load Balancer (Layer4)

best suited for load balancing of TCP traffic where extreme performance is required.

##### Classical Load Balancer

provides basic load balancing across multiple Amazon EC2 instances and operates at both the request level and connection level.

For EC2 classical network.

### Mobile

#### AWS Cognito (Sign-in Service)

- Amazon Cognito lets you add user **sign-up, sign-in, and access control to your web and mobile apps quickly and easily**.
- Amazon Cognito scales to millions of users and supports sign-in with social identity providers, such as Facebook, Google, and Amazon, and enterprise identity providers via SAML 2.0.

### Management and Governance

#### AWS CloudTrail

- is a service that primarily **tracks** governance, compliance, operational auditing, and risk auditing of your AWS account.
- CloudTrail logs continuously monitors and retains account activity related to actions across all AWS infrastructure. CloudTrail provides **event history of AWS account activity** , including actions taken through the AWS Management Console, AWS SDKs, command line tools, and other AWS services. This event history simplifies security analysis, resource change tracking and troubleshooting

#### Amazon CloudWatch

A **monitoring and management service** built for developers, system operators, site reliability engineers (SRE), and IT managers.

- CloudWatch collects monitoring and operational data in the form of logs, metrics, and events, providing you with a unified view of AWS resources, applications and services that run on AWS, and on-premises servers.

#### AWS Service Catalog

- AWS Service Catalog allows organizations to **create and manage catalogs of IT services that are approved for use on AWS**.
- These IT services can include everything from virtual machine images, servers, software, and databases to complete multi-tier application architectures.
- AWS Service Catalog allows you to centrally manage commonly deployed IT services, and helps you achieve consistent governance and meet your compliance requirements, while enabling users to quickly deploy only the approved IT services they need.

#### Amazon Auto Scaling

(Compare to EC2 Auto Scaling)

monitors your applications and automatically adjusts capacity to maintain steady, predictable performance at the lowest possible cost.

Available to: Amazon EC2, Amazon ECS, Amazon DynamoDB, Amazon Aurora

#### AWS CloudFormation

gives developers and systems administrators an easy way to create and manage a collection of related AWS resources, provisioning and updating them in an orderly and predictable fashion.

CloudFormation Template

#### AWS Well-Architected Tool

Machine Learning based tool that analyses metrics of historic utilization and makes recommendations of compute services to be used for workload.

Review the state of your workloads and compares them to the latest AWS architectural best practices.

#### AWS Systems Manager

AWS Systems Manager gives you visibility and control of your infrastructure on AWS.

- Systems Manager provides a unified user interface so you can **view operational data** from multiple AWS services and allows you to **automate operational tasks** across your AWS resources.
- With Systems Manager, you can group resources, like [Amazon EC2](https://aws.amazon.com/ec2/) instances, [Amazon S3](https://aws.amazon.com/s3/) buckets, or [Amazon RDS](https://aws.amazon.com/rds/) instances, by application, view operational data for monitoring and troubleshooting, and take action on your groups of resources.
- Systems Manager simplifies resource and application management, shortens the time to detect and resolve operational problems, and makes it easy to operate and manage your infrastructure securely at scale.

##### Resource Group

##### Insights Dashboard

##### Run Command

##### State Manager

##### Automation

##### Parameter Store

##### Distributor

##### Session Manager

#### AWS Config (change of configuration)

- used to audit and monitor configuration changes
- AWS Config is a service that enables you to **assess, audit, and evaluate the configurations of your AWS resources**.
- Config continuously **monitors and records** your AWS resource configurations and allows you to automate the evaluation of recorded configurations against desired configurations.
- With Config, you can review changes in configurations and relationships between AWS resources, dive into detailed resource configuration histories, and determine your overall compliance against the configurations specified in your internal guidelines. This enables you to simplify compliance auditing, security analysis, change management, and operational troubleshooting.

#### AWS OpsWorks

- AWS OpsWorks is a configuration management service that provides managed instances of Chef and Puppet.
- Chef and Puppet are **automation platforms that allow you to use code to automate the configurations of your servers**.
- OpsWorks lets you use Chef and Puppet to automate how servers are configured, deployed, and managed **across your** [**Amazon EC2**](https://aws.amazon.com/ec2/) **instances or on-premises** compute environments.

#### AWS Control Tower

● helps Enterprises quickly set-up a secure, AWS multi-account

● provides you with a baseline environment to get started with a multi-account architecture

● AWS Account Vending Machine (AVM)

Automatically provisions and configure new accounts via Service Catalog Template

Uses Single Sign-on (SSO) for managing and accessing accounts

【Architecture Diagram】

![](RackMultipart202 004-4- udwvv_html_3b37f 4c69387ab.png)

#### AWS Personal Health Dashboard

Technology and tools to monitor, manage, and optimize your AWS environment

- a tool that **shows the status of AWS services** that are running user-specific resources
- a graphical representation that **sends alerts, notifications of any personal pending issues, planned changes and scheduled activities**

#### Service Health Dashboard

To monitor AWS is norm or not

### Migration and Transfer

#### AWS DataSync

- an online **data transfer service** that simplifies, automates, and accelerates copying large amounts of data to and from AWS storage services over the internet or AWS Direct Connect. DataSync can copy data between Network File System (NFS) or Server Message Block (SMB) file servers, Amazon S3 buckets, and Amazon Elastic File System file systems

#### AWS Snowball

- Snowball is a petabyte-scale data transport solution that uses devices designed to be secure to transfer large amounts of data into and out of the AWS Cloud. Using Snowball addresses common challenges with large-scale data transfers including high network costs, long transfer times, and security concerns. Customers today use Snowball to migrate analytics data, genomics data, video libraries, image repositories, backups, and to archive part of data center shutdowns, tape replacement or application migration projects. Transferring data with Snowball is simple, fast, more secure, and can be as little as one-fifth the cost of transferring data via high-speed Internet.

### Machine Learning

#### Amazon Rekognition

- Amazon Rekognition makes it easy to add image and video analysis to your applications using proven, highly scalable, deep learning technology that requires no machine learning expertise to use.
- With Amazon Rekognition, you can identify objects, people, text, scenes, and activities in images and videos, as well as detect any inappropriate content. Amazon Rekognition also provides highly accurate facial analysis and facial search capabilities that you can use to detect, analyze, and compare faces for a wide variety of user verification, people counting, and public safety use cases.
- make it easy to set up, operate, and scale a relational database in the cloud
- provide cost-efficient and resizable capacity while automating time-consuming administration tasks such as hardware provisioning, database setup, patching and backups
- free you to focus on your applications so you can give them the fast performance, high availability, security and compatibility they need

#### Amazon SageMaker

- Amazon SageMaker is a fully-managed platform that enables developers and data scientists to quickly and easily build, train, and deploy machine learning models at any scale.
- Amazon SageMaker includes modules that can be used together or independently to build, train, and deploy your machine learning models

#### Amazon SageMaker Ground Truth

helps you build highly accurate training datasets for machine learning quickly.

- SageMaker Ground Truth offers easy access to public and private human labelers and provides them with built-in workflows and interfaces for common labeling tasks.
- Additionally, SageMaker Ground Truth can lower your labeling costs by up to 70% using automatic labeling, which works by training Ground Truth from data labeled by humans so that the service learns to label data independently.

#### Amazon Comprehend

A natural language processing (NLP) service that uses machine learning to find insights and relationships in text. No machine learning experience required.

- Amazon Comprehend uses machine learning to help you uncover the insights and relationships in your unstructured data.

#### Amazon Lex

a service for building conversational interfaces into any application using voice and text.

- Lex provides the advanced deep learning functionalities of automatic speech recognition (ASR) for converting speech to text, and natural language understanding (NLU) to recognize the intent of the text

#### Amazon Polly

a service that turns text into lifelike speech

## 【Billing/Price and Service Intro】

### EC2 (Four Types)

Main cloud computing service of AWS

#### Pricing Model

##### On-Demand (least commitment)

Norm service

- charged by the hour or by the minute
- is for applications where workload is for short-term, spikey or unpredictable

##### Spot (biggest savings)

- AWS wants to maximize the utility of those idle servers (like hotel discount to fill vacant seats)

- Design for applications that have flexible start and end times or applications that are only feasible at very low compute costs

- provide a discount of **90% compared to on-demand pricing**

- Termination Conditions:

   

  can be terminated if the computing capacity is needed by on-demand customers

  - instances can be terminated by AWS at anytime
  - if your instance is terminated by AWS, you don't get charged for a partial hour of usage
  - if you terminate an instance, you will still be charged for any hour that it ran

##### Reserved (Best Long-term) RI

Design for application that have **steady-state, predicable usage** or require reserved capacity

Convertible Reserved Instances: have the flexibility to change families, operating system types, and tenancies while benefitting from Reserved Instance pricing

- pricing is based on Term x Class Offering x Payment Option
- Offering Class:
  - standard: **up to 75% reduced pricing** compared to on-demand
  - convertible: up to 54% reduced pricing
  - scheduled: reserve instance for specific time periods, savings vary
- Term:
  - commit to a ear or 3 Year contract
  - the longer the term, the greater savings
- Payment Options:
  - all upfront
  - partial upfront
  - no upfront

【Amazon EC2 RI Types】

- Standard RIs: These provide the most significant discount (up to 72% off On-Demand) and are best suited for steady-state usage.
- Convertible RIs: These provide a discount (up to 54% off On-Demand) and the capability to **change the attributes of the RI as long as the exchange results in the creation of Reserved Instances of equal or greater value**. Like Standard RIs, Convertible RIs are best suited for steady-state usage.
- Scheduled RIs: These are available to launch within the time windows you reserve. This option allows you to **match your capacity reservation to a predictable recurring schedule that only requires a fraction of a day, a week, or a month**.

##### Dedicated (most expensive)

Don't share hardware

- designed to meet regulatory requirements
  - when you have strict server-bound licensing that won't support multi-tenancy or cloud deployments
- multi-tenancy
  - multi-tenant
    - when multiple customers are running workloads on the same hardware
    - **virtual isolation** is what separate customers
  - single tenant
    - when a single customer has dedicated hardware
    - **physical isolation** is what separates customers
- ffered in both on-demand and reserved (70% off on-demand pricing)
- enterprises and large organization may have **security concerns** or obligations about against the same hardware with other AWS customers

#### **Cheat Sheet (Summary)**

- EC2 has 4 pricing models On-Demand, Spot, Reserved Instances (RI) and Dedicated
- On-Demand (least commitment)
  - low cost and flexible
  - nly pay per hour
  - Use case: short-term, spiky, unpredictable workloads, first time apps
  - ideal when your workloads cannot be interrupted
- Reserved Instances up to 75% off (best long-term value)
  - Use case: steady state or predictable usage
  - can resell unused reserved instances (reserved instance marketplace)
  - reduced pricing is based on **Term x Class Offering x Payment Option**
  - Payment Terms: year or 3 years
  - Payment Options: All Upfront, Partial Upfront, and No Upfront
  - Class Offerings
    - Standard: up to 75% reduced pricing compared to on-demand. Cannot change RI attributes
    - Convertible: up to 54% reduced pricing compared to on-demand. Allows you to change RI attributes if greater or equal in value
    - Scheduled: you reserve instances for specific time periods, eg. once a week for a few hours. Savings vary
- Spot pricing up to 90% off (biggest savings)
  - request spare computing capacity
  - flexible start and end times
  - use case:
    - can handle interruptions (server randomly stopping and starting)
    - for non-critical background jobs
  - instances can be terminated by AWS at anytime
  - if your instance is **terminated by AWS** , you don't get charged for a partial hour of usage
  - if you terminate an instance you will still be charged for any hour it ran
- Dedicated Hosting (most expensive)
  - dedicated servers
  - can be on-demand or reserved (up to 70% off)
  - use case
    - when you need a guarantee of isolate hardware (enterprise requirements)

### Free Services

- certain services are free themselves, but the resources they setup will cost you
  - IAM - identity access management
  - Amazon VPC
  - services are free, but they can provision AWS services which cost money
    - Auto Scaling
    - CloudFormation
    - Elastic Beanstalk
    - Opsworks
    - Amplify
    - AppSync
    - CodeStar
  - Organizations & Consolidated Billing
  - AWS Cost Explorer

### AWS Support Plans (Important)

- Basic $0USD/month
  - email support only for billing and account
  - 7 trusted advisor checks
- Developer $20USD/month
  - tech support via email ~24 hours until reply
  - no third-party support
  - general guidance: 24 hours
  - system impaired: hours
  - 7 trusted advisor checks
- Business $ 0USD/month
  - tech support via email ~24 hours until reply
  - tech support via chat, phone anytime 24/7
  - general guidance: 24 hours
  - system impaired: hours
  - production system impaired: 4 hours
  - production system down: hours
  - third-part support
  - all trusted advisor checks
- Enterprise $ ,000USD/month
  - all that Business has
  - business-critical system down: minutes
  - Personal Concierge
  - TAM (technique account manager)
  - all trusted advisor checks

Example: how to create a support case

- technical support
- service limit increase
- account and billing support

!!! free plan doesn’t have technique support !!!

Web Chat Phone

### AWS Marketplace

- a curated digital catalogue with thousands of software listings from independent software vendors
- easily find, buy, test, and deploy software that already runs on AWS
- the product can be **free to use** or can **have an associated charge**
- products can be offered as
  - Amazon Machine Images (AMIs)
  - AWS CloudFormation templates
  - Software as a service (SaaS) offerings
  - Web ACL
  - AWS WAF rules

Example: AWS marketplace subscription

Popular categories

Usage information: EC2/…

Manage subscriptions

### AWS Trusted Advisor

- **Advise** you on **security, saving money, performance, service limits and fault tolerance**
- Seven Category of Trusted Advisor: **cost optimization, performance, service limits, fault tolerance, security**
- AWS Basic Support and AWS Developer Support customers get access to **6 security checks** (S3 Bucket Permissions, Security Groups - Specific Ports Unrestricted, IAM Use, MFA on Root Account, EBS Public Snapshots, RDS Public Snapshots) and **50 service limit checks**.
- Enterprise and business for all trusted advisor
- Cost Optimization
  - idle load balancers
  - unassociated elastic IP addresses
- Performance
  - high utilization Amazon EC2 instances
- Security
  - MFA on Root Account
  - IAM access key rotation
- Fault Tolerance
  - Amazon RDS backups
- Service Limits
  - VPC

Example: how trusted advisor works?

Cost optimization

### Consolidated Billing

- **one bill for all of your accounts**
- treat all the accounts in an organization as if they were one account (create AWS organization)
- you can designate one master account that pays the charges of all the other member accounts
- consolidated billing is offered at no additional cost
- volume discounts (for many services) &lt;- motivation to use consolidate billing
  - the more you use, the more you save
  - consolidated billing lets you take advantage of volume discounts

### AWS Cost Explorer

- let you **visualize, understand,** and **manage** your AWS costs and usage over time
- default reports help you gain insight into your cost drives and usage trends

![](RackMultipart202 004-4- udwvv_html_6df6753e8ede89a7.png)

Example: how to use cost explorer?

![](RackMultipart202 004-4- udwvv_html_5dd444afca3d8 e.png)

- AWS Cost Explorer has an easy-to-use interface that lets you visualize, understand, and manage your AWS costs and usage over time.
- monthly costs by AWS service
- hourly and resource level granularity
- forecast future costs and usage

### AWS Budgets

Like a billing alarms on steroids

- plan your **service usage, service costs and instance reservations**
- first two budges are free
- each budget is $0.02 per day ~ 0.60 USD / mo 20,000 budgets limit
- give you the ability to setup alerts if you exceed or are approaching your defined budget
- create **Cost, Usage or Reservation budgets**
- can be tracked at the monthly, quarterly, or yearly levels, with customizable start and end dates
- alerts support **EC2, RDS, Redshift, and ElastiCache** reservations
- can be easily manage from AWS Budgets dashboard or via the Budgets API
- Get notified by providing an email or chatbot and threshold how close to the current or forecasted budget

### TCO Calculator

- Total Cost of Ownership
- estimate how much you would save when moving to AWS from on-premise
- provides detailed set of **reports that can be used in executive presentation**
- tool is for approximation purposes only!

### AWS Resource Groups and Tagging

**Tags** are words or phrases that act as metadata for organizing your AWS resources

**Resources Groups** are a collection of resources that share one or more **tags**

RG can display details about a group of resource based on

- Metrics
- Alarms
- Configuration settings

Help you to organize consolidate resource

### AWS Quick Starts ( **AWS CloudFormation** )

**Prebuilt templates** by AWS and AWS partners to help you deploy **popular stacks** on AWS

Reduce hundreds of manual procedures into just few steps

3 parts:

- A reference architecture for the deployment
- **AWS CloudFormation** templates that automate and configure the deployment
- A deployment guide explaining the architecture and implementation in detail

### AWS Cost and Usage Report

Generate detailed spreadsheet, enabling you to better analyze and understand your AWS costs

Example:

A single location for accessing comprehensive information about your AWS costs and usage. (in hour or daily line items)

## 【Technology Overview】

### Organizations and Accounts

A AWS organization have only one root account, and an account of this organization can only be in one Organization Unit (OU)

- Organizations
  - ffer policy-based management for multiple AWS accounts
  - you can create group of accounts and then apply policies to those groups
  - enable you to centrally manage policies across multiple accounts, without requiring custom scripts and manual processes
- Root Account User

A single sign-in identity that has complete access to all AWS services and resources in an account

- Organization Units

A group of AWS accounts within an organization which can also contains other organizational units – creating a hierarchy

- Service Control Policies

Give central control over the allowed permissions for all accounts in your organization, helping to ensure your accounts stay with your organization's guideline

Example:

Create organization

Invite account / create organization account

Setup new account need to forget password…

### AWS Networking

- Region
  - the geographical location of your network
- AZ
  - the data center of your AWS resources
- VPC
  - a logically isolated section of the AWS Cloud where you can launch AWS resources
- Internet Gateway
  - enable access to the internet
- Route Tables
  - determine where network traffic from your subnets are directed
- NACLs
  - acts as a firewalls at the subnet level
- Security Groups
  - acts as firewall at the instance level
- Subnets
  - a logical partition of an IP network into multiple, smaller network segments

### Database Services

- DynamoDB
  - NoSQL key/value database
  - fully managed
  - cassandra
- DocumentDB
  - NoSQL **Document database** that is MongoDB compatible
  - mongoDB
- RDS
  - Relational Database Service
  - support multiple engines
  - MySQL, Postgres, MariaDB, Oracle, Microsoft SQL Server, Aurora
- Aurora
  - MySQL (5x faster) and PSQL (3x faster) database **fully managed**
- Aurora Serverless
  - nly runs when you need it, like AWS **Lambda**
- Neptune
  - Managed Graph Database
- Redshift
  - Columnar database, petabype warehouse ( b = 00 tb)
  - Work with huge amount of data
  - Data Warehouse
- ElastiCache
  - Redis, or Memcached database
  - a web service that makes it easy to deploy, operate, and scale an in-memory data store or cache in the cloud
  - improve the performance of web applications by allowing you to retrieve information from fast, managed, in-memory data stores, instead of relying entirely on slower disk-based databases

### Provisioning Services

The allocation or creation of resources and services to a customer

- Elastic Beanstalk
  - service for deploying and scaling web applications and services developed with Java, .NET, PHP, Node.js, Python, Ruby, Go, and Docker
  - upload code and work
- OpsWorks
  - configuration management service that provides managed instances of Chef and Puppet (automatic)
- CloudFormation
  - infrastructure as code, JSON or YAML
  - define by JSON and automatic generate code, flexible
- AWS QuickStart
  - pre-made packages that can launch and configure your AWS compute, network, storage, and other services required to deploy a workload on AWS
- AWS Marketplace
  - a digital catalogue of **thousands of** software listings from independent software vendors you can use to find, buy, test, and deploy software
  - managed EC2 instance

### AWS Computing

- EC2: Elastic Compute Cloud, highly configurable server eg. CPU, memory, network, OS
  - ECS: Elastic Container Service
    - Docker as a Service
    - highly scalable, high-performance container orchestration service that supports Docker containers, pay for EC2 instances
  - Fargate
    - microservices where you don't think about the infrastructure
    - pay per task
    - like lambda
  - EKS
    - Kubernetes as a Service
    - easy to **deploy, manage, and scale** containerized applications using Kubernetes
  - Lambda
    - serverless functions
    - run code without provisioning or managing servers
    - pay only for the compute time you consume
  - Elastic Beanstalk
    - rchestrates various AWS services, including EC2, S3, SNS, CloudWatch, autoscaling, and Elastic load balancers
    - just upload your code
  - AWS Batch (batch processing)
    - plans, schedules, and executes your batch computing workloads across the full range of AWS compute services and features, such Amazon EC2 and Spot Instances

### Storage Services (hard drive on the cloud)

- S3: **S** imple **S** torage **S** ervice
  - bject storage
- S3 Glacier
  - low cost storage for archiving and long-term backup
- Storage Gateway
  - hybrid cloud storage with local caching
  - File Gateway, Volume Gateway, Tape Gateway
- EBS: Elastic Block Storage
  - hard drive in the cloud you attach to EC2 instances
  - SSD, IOPS SSD, Throughput HHD, Cold HHD
  - when you create an EBS volume in an Availability Zone, it is automatically replicated within that zone to prevent data loss due to failure of any single hardware component
- EFS: Elastic File Storage
  - file storage mountable to multiple EC2 instances at the same time
- Snowball
  - physically migrate lots of data via a computer suitcase (50-80 TB)
  - Snowball Edge
    - a better version of Snowball ( 0 TB)
  - Snowmobile (cargo)
    - shipping container, pulled by a semi-trailer truck ( 0 PB)

### Business Centric Services

- Amazon Connect
  - call center
  - Cloud-based call center service you can setup in just a few clicks
  - based on the same proven system used by the Amazon customer service teams
- WorkSpaces
  - virtual remote desktop
  - secure managed service for provisioning either Windows or Linux desktops in just a few minutes which quickly scales up to thousands of desktops
- WorkDocs
  - a content creation and collaboration service
  - easily create, edit, and share content saved centrally in AWS
  - (the AWS version of Sharepoint)
- Chime
  - AWS platform for **online meetings, video conferencing, and business calling** which elastically scales to meet your capacity needs
- WorkMail
  - managed business email, contacts, and calendar service with support for existing desktop and mobile email client applications (IMAP)
  - like Gmail on AWS
- Pinpoint
  - marketing campaign management system you can use for sending targeted email, SMS, push notifications, and voice messages
- SES
  - Simple Email Service
  - a cloud-based email sending service designed for marketers and application developers to send marketing, notification, and emails
  - advertisement email
  - html component
- QuickSight
  - a Business Intelligence (BI) service
  - connect multiple data source and quickly visualize data in the form of graphs with little to no programming knowledge

### Enterprise Integration

- Direct Connect
  - dedicated Gigabit network connection from your premises to AWS
  - imagine having a direct fiber optic cable running straight to AWS
  - no latency, dedicated connection
- VPN
  - establish a **secure** connection to your AWS network
  - site-to-site VPN
    - connecting your on-premise to your AWS network
  - client VPN
    - connecting a client (a laptop) to your AWS network
- Storage Gateway
  - a hybrid storage service that enables your **on-premise applications** to use AWS cloud storage (S3)
  - you can use this for backup and archiving, disaster recovery, cloud data processing, storage tiering, and migration
- Active Directory
  - the AWS directory service for Microsoft Active Directory also known as AWS Managed Microsoft AD
  - enable your directory-aware workloads and AWS resources to use managed Active Directory in the AWS Cloud

### Logging Services (记录服务)

- CloudTrail

  - logs all API calls (SDK, CLI) between AWS services (who we can blame)
    - who created this bucket?
    - who spun up that expensive EC2 instance?
    - who launched the SageMaker Notebook?
  - detect developer misconfiguration
  - detect malicious actors
  - automate responses

- CloudWatch: a collection of multiple services

  - CloudWatch

     

    Logs

    - performance data about AWS Services
      - CPU utilization, memory, network
    - application logs
    - lambda logs

  - CloudWatch

     

    Metrics

    - represent a time-ordered set of data points
    - a variable to monitor

  - CloudWatch

     

    Events

    - trigger an event based on a condition
    - every hour take a snapshot of server

  - CloudWatch

     

    Alarms

    - trigger notifications based on metrics

  - CloudWatch

     

    Dashboard

    - create visualizations based on metrics

## 【Security and Compliance】

What is Compliance?

Compliance with something, such as law, treaty or agreement

### Identity and Access Management (IAM)

AWS Identity and Access Management (IAM) 是一种 Web 服务，可以帮助您安全地控制对 AWS 资源的访问。您可以使用 IAM 控制对哪个用户进行身份验证 (登录) 和授权 (具有权限) 以使用资源。

当您首次创建 AWS 账户时，最初使用的是一个对账户中所有 AWS 服务和资源有完全访问权限的单点登录身份。此身份称为 AWS 账户 根用户，可使用您创建账户时所用的电子邮件地址和密码登录来获得此身份。强烈建议您不使用 根用户 执行日常任务，即使是管理任务。

![](RackMultipart202 004-4- udwvv_html_64b 48e3076ec9e.png)

![](RackMultipart202 004-4- udwvv_html_524000e4397dca8e.png)

IAM Policies are JSON documents used to describe permissions within AWS
\2. Three steps of using IAM:

- Create IAM group

- Create IAM policies

- Add IAM users

  MFA 多重验证：您可以向您的账户和各个用户添加双重身份验证以实现更高安全性。借助 MFA，您或您的用户不仅必须提供使用账户所需的密码或访问密钥，还必须提供来自经过特殊配置的设备的代码。

1. IAM role: An IAM role is an IAM entity that defines a set of [permissions](https://aws.amazon.com/iam/details/manage-permissions/) for making AWS service requests. IAM roles **are not associated with a specific user or group**. Instead, trusted entities assume roles, such as IAM users, applications, or AWS services such as EC2.

IAM group: easy way to grant /revoke/manage permissions on collection of IAM users

IAM tag: adding custom attributes to IAM users and roles, IAM tags uses key-value pair

IAM users: assign them individual security credentials (access key, passwords and MFA), or request temporary security credentials.

### Shared Responsibility Model (very important, one of key point)

##### Overview

- customers are responsible for security in the Cloud
  - data
  - configuration
- AWs is responsible for security of the Cloud
  - hardware
  - operation of managed services
  - global infrastructure

##### full model

##### **Shared Controls** - Controls which apply to both the infrastructure layer and customer layers, but in completely separate contexts or perspectives. In a shared control, AWS provides the requirements for the infrastructure and the customer must provide their own control implementation within their use of AWS services.

- Patch Management – AWS is responsible for patching and fixing flaws within the infrastructure, but customers are responsible for patching their guest OS and applications.
- Configuration Management – AWS maintains the configuration of its infrastructure devices, but a customer is responsible for configuring their own guest operating systems, databases, and applications.
- Awareness & Training - AWS trains AWS employees, but a customer must train their own employees.

### Compliance Programs

- **a set of internal policies** and procedures of a company to comply with laws, rules, and regulations or to uphold business reputation
- example
  - HIPAA
    - Health Insurance Portability and Accountability Act
    - United States legislation that provides **data privacy and security provisions** for safeguarding medical information
  - PCI DSS
    - The Payment Card Industry Data Security Standard
    - when you want to sell things online and you need to handle credit card information

### Artifact

Generate report to meet compliance

A comprehensive resource center for access to AWS' auditor issued reports as well as security and compliance documentation from several renowned independent standards organizations.

- how do we prove AWS meets a compliance
  - no cost, self-service portal for on-demand access to AWS' compliance reports
  - n-demand **access to AWS' security and compliance reports** and select online agreements
  - these checks are based on **global compliance** frameworks

### Amazon Inspector

- how do we prove an EC2 instance is harden
  - hardening: the act of **eliminating as many security risks** as possible
  - AWS Inspector runs a security benchmark against specific EC2 instances
  - you can run a variety of security benchmarks, for Center for Internet Security (CIS), **699 checks**
  - can perform both Network and Host Assessments
    - install the AWS agent on your EC2 instances
    - run an assessment for your assessment target
    - review your findings and remediate security issues

### AWS WAF (web application)

- Web Application Firewall
  - protect your **web applications** from common web exploits
  - write your own rules to ALLOW or DENY traffic based on the contents of an HTTP requests
  - use a **ruleset** from a trusted AWS security partner in the AWS WAF Rules Marketplace
  - WAF can be attached to either **CloudFront** (distributed edge server)or an **Application Load Balancer**

Protect web applications from attack covered in the OWASP Top most dangerous attacks:

Injection
\2. Broken authentication
\3. Sensitive data exposure
\4. XML external entities (XXE)
\5. Broken access control
\6. Security misconfiguration
\7. Cross Site Scripting (XSS)
\8. Insecure deserialization
\9. Using components with known vulnerabilities
. Insufficient logging and monitoring

- WAF includes Shield

### AWS Shield

- a managed **DDoS** protection service that safeguards applications running on AWS

what is DDOS attack?

A malicious attempt to disrupt normal traffic by flooding a website a large amount of fake traffic

- all AWS customers benefit from the automatic protections of AWS Shield Standard, at no additional charge
- when you route your traffic through Route53 or CloudFront, your are using AWS Shield Standard
- protect you against Layer 3, 4, and 7 attacks
  - 7 application
  - 4 transport
  - 3 network
- Shield Standard
  - free
  - for protection against most common DDoS attacks
- Shield Advanced
  - $3000 / year
  - for **additional protection against larger and more sophisticated attacks**
  - visibility into attacks
  - 24x7 access to DDoS experts for complex cases
  - available on
    - Route 53
    - CloudFront
    - ELB
    - Global Accelerator
    - Elastic IP (Amazon Elastic Compute Cloud and Network Load Balancer)

##### DDoS Mitigate

### Penetration Testing

- PenTesting
  - an authorized simulated cyberattack on a computer system
  - performed to evaluate the security of the system

need approval from AWS, can be done by cusomer

- we can perform PenTesting on AWS
- permitted services
  - EC2 intances, NAT gateways, and ELB
  - RDS
  - CloudFront
  - Aurora
  - API gateways
  - Lambda and Lambda@Edge functions
  - Lightsail resources
  - Elastic Beanstalk environments
- Prohibited Activities
  - DNS zone walking via Amazon **Route 53** Hosted Zones
  - Denial of Service (DoS), Distributed Denial of Service (DDoS), simulated DoS, simulated DDoS
  - Port flooding
  - Protocol flooding
  - Request flooding (login request flooding, API request flooding)
- for some simulated events, you need to submit a request to AWS. A replay could take up to 7 days

### GuardDuty

- a threat detection service that **continuously** monitors for malicious, suspicious activity and unauthorized behavior
- IDS/IPS
- it uses Machine Learning to analyze the following AWS logs
  - CloudTrail logs
  - VPC flow logs
  - DNS logs
- it will alert you of Findings which you can automate an incident response via CloudWatch Events or with 3rd Party Services

### Key Management Service (KMS)

- a managed service that makes it easy for you to create and control the encryption keys used to encrypt your data
  - KMS is a multi-tenant HMS (hardware security module)
  - many AWS services are integrated to use KMS to encrypt your data with a simple checkbox
  - KMS uses Envelope Encryption

Envelope Encryption:

When you encrypt your data, your data is protected, but you have to protect your encryption key. When you encrypt your data key with a master key as an additional layer of security.

### Amazon Macie

- a fully managed service that continuously monitors **S3 data access** activity for anomalies, and generates detailed alerts when it detects risk of unauthorized access or inadvertent data leaks
- use **Machine Learning** to analyze CloudTrail logs

![](RackMultipart202 004-4- udwvv_html_d3fad5cecb93899.png)

Macie's will identify your **most at-risk users** which could lead to a compromise

### Security Groups vs NACLs

- Security Groups
  - act as a firewall at the **instance** level
  - implicitly deny all traffic, you create allow rules
  - allow an EC2 instance access on port 22 for SSH
- Network Access Control Lists (NACLs)
  - act as a firewall at the **subnet** level
  - you create allow and deny rules
  - block a specific IP address known for abuse

### AWS VPN

- let you establish a secure and **private tunnel** from your network or device to the AWS global network
- AWS Site-to-Site VPN

Securely connect on-premises network or branch office site to VPC

- AWS Client VPN

Securely connect users to AWS or on-premises networks

### AWS Secrets Manager (用户证书管理)

Helps in securely storing encrypted credentials and ensures retrieval when required. Use ASM eliminates the need of hard-coding credentials in the application.

### AWS Security Hub

Gives you a comprehensive view of your high-priority security alerts and security posture across your AWS accounts.

### AWS Encryption SDK

The encryption library that makes client-side encryption best-practice easier. The encryption libraries facilitate cryptographic services and **do not require** AWS or any AWS service.

### AWS Certificate Manager (ACM)

AWS Certificate Manager is a service that lets you easily provision, manage, and d **eploy public and private Secure Sockets Layer/Transport Layer Security** (SSL/TLS) certificates for use with AWS services and your internal connected resources. SSL/TLS certificates are used to secure network communications and establish the identity of websites over the Internet as well as resources on private networks. AWS Certificate Manager removes the time-consuming manual process of purchasing, uploading, and renewing SSL/TLS certificates.

### AWS Directory Service

- AWS Directory Service for Microsoft Active Directory, also known as AWS Managed Microsoft AD, enables your **directory-aware workloads** and AWS resources to use managed Active Directory in the AWS Cloud. AWS Managed Microsoft AD is built on actual [Microsoft Active Directory](https://aws.amazon.com/directoryservice/active-directory/) and does not require you to synchronize or replicate data from your existing Active Directory to the cloud. You can use standard Active Directory administration tools and take advantage of built-in Active Directory features, such as Group Policy and single sign-on (SSO).
- With AWS Managed Microsoft AD, you can easily join [Amazon EC2](https://aws.amazon.com/ec2/) and [Amazon RDS for SQL Server](https://aws.amazon.com/rds/sqlserver/) instances to your domain, and use [AWS Enterprise IT applications](https://aws.amazon.com/enterprise-applications/) such as [Amazon WorkSpaces](https://aws.amazon.com/workspaces/) with Active Directory users and groups.

### IAM Identity Federation

- allow access to the AWS environment using a central SSO set of credentials from third-party or corporate active directory
- use open standards such as SAML2.0 to transact identity information between identity provider (IdP)

### AWS CloudHSM (hardware)

- Cloud-based Hardware Security Mode (HSM)
- Easily generate and use your own encryption security key on AWS
- CloudHSM is standards-compliant and enables you to export all of your keys to most other commercially-available HSMs, subject to your configurations.

## 【Variation Study】Other Stuff

### Cloud* Services

Similar names, completely different services

- CloudFormation
  - **infrastructure as code** , set up services via templating script
- CloudTrail
  - logs all API calls between AWS services
- CloudFront
  - content distribution network
  - create a cached copy of your website and copy to servers located near people trying download website
- CloudWatch
  - a collection of CloudWatch Logs/Metrics/Events/Alarms/Dashboard
- CloudSearch
  - search engine
  - you have an ecommerce website and you want to add a search bar

### Connect Services

- Direct Connect
  - dedicated fiber optics connections from datacenter to AWS
  - a large enterprise has their own datacenter and they need an insanely fast connection directly to AWS. If you need security, you can apply a VPN connect on top of Direct Connect
- Amazon Connect
  - **call center service**
  - get a toll free number, accept inbound and outbound calls, setup automated phone systems
- Media Connect
  - new version of elastic transcoder, converts videos to different video types
  - example: you have 00 of videos you and you need to transcode them into different videos format, maybe you need to apply watermarks, or insert introduction video in front of every video

### Elastic Transcoder vs Media Convert

- both services **transcodes** videos
- Elastic Transcoder (the old way)
  - transcode videos to streaming formats
- AWS Elemental Media Convert (the new way)
  - transcode videos to streaming formats
  - verlay images
  - insert videos clips
  - extract captions data
  - robust UI

### SNS vs SQS

Both **connect apps** via Messages

- Simple Notifications Service (SNS)
  - pass along messages
  - send notifications to **subscribers of topics** via multiple protocol (HTTP, email, SQS, SMS)
  - is generally used for sending **plain text emails** , which is triggered via other AWS services
  - can retry sending in case of failure for **HTTPS**
  - really good for webhooks, simple internal emails, triggering lambda functions
- Simple Queue Service (SQS)
  - queue up messages, guaranteed delivery
  - place messages into a **queue**
  - applications pull queue using **AWS SDK**
  - can retain a message for up to days
  - can send them in sequential order or in parallel
  - can ensure only one message is sent
  - can ensure messages are delivered at least once
  - really good for delayed task, queueing up emails

### Inspector vs Trusted Advisor

- both are **security tools** and both perform audits
- Inspector (for security)
  - audit **a single EC2 instance** that you have selected
  - generate a report form a long list of security checks (699 checks)
- Trusted Advisor (multiple things, according to five pillars)
  - **doesn't generate a PDF report**
  - give you a **holistic view** of recommendations across multiple services and best practices
  - You have open ports on these security groups / you should enable MFA on your root account when using trusted advisor

### ALB vs NLB vs CLB (Elastic Load Balancer)

Three types of load balancer

- all can attach Amazon Certification Manager (ACM) SSL Certificate
- Application
  - **Layer 7** requests
  - **HTTP and HTTPS** traffic
  - routing rules, more usability from one load balancer
  - can attach WAF
- Network
  - **Layer 4** IP protocol data
  - **TCP and TLS** traffic where extreme performance is required
  - capable of handling millions of requests per second while maintaining **ultra-low latencies**
  - ptimized for **sudden and volatile** traffic patterns while using a single static IP address per Availability Zone
- Classic
  - ld
  - Intended for applications that were built within the **EC-2 classical network**
  - Doesn't use target groups

### SNS vs SES

- both send emails
- SNS
  - practical and internal
  - send notifications to subscribers of topics via multiple protocol
  - is generally used for sending **plain text emails** which is triggered via other AWS services
  - you need to know what are **Topics** and **Subscriptions** regarding **SNS**
- SES
  - **professional** , marketing emails
  - a cloud based email service (eg. SendGrid)
  - send **html emails**
  - can receive inbound emails
  - can create email templates
  - custom domain name email
  - monitor your email reputation

### Artifact vs Inspector

Both **compile out PDFs**

- Artifact
  - why should an enterprise trust AWS
  - generate a security report that's based on **global compliance frameworks**
- Inspector
  - how do we know this EC2 instance is secure
  - run a script that analyzes your EC2 instance, then generate a PDF report telling you which security checks passed
  - **audit tool for security of EC2 instances**

### The Five Pillars of the Framework

Incorporating these pillars into your architecture will help you produce stable and efficient systems. This will allow you to focus on the other aspects of design, such as functional requirements.

- **Operational Excellence**

【Design Principles】

Perform operations as code

Make frequent small reversible changes

Refine operations procedures frequently

Anticipate failure

Learn from all operational failures

- **Security**

【Design Principles】

Implement a strong identity foundation

Enable traceability

Apply security best practices

Protect data in transit and at rest

Keep people away from data

Prepare for security events

- **Reliability**

【Design Principles】

Automatically recover from failure

Test recovery procedures

Scale horizontally to increase aggregate availability

Stop guessing capacity

Manage change in automation

- **Performance Efficiency**

【Design Principles】

Democratize advanced technologies

Go global in minutes

Use serverless architectures

Experiment more often

Consider mechanical sympathy

- **Cost Optimization**

【Design Principles】

Implement cloud financial management

Adopt a consumption model

Measure overall efficiency

Stop spending money on undifferentiated heavy lifting

Analyze and attribute expenditure

### Serverless Service

- Serverless computing is a cloud computing execution model in which the cloud provider runs the server, and dynamically manages the allocation of machine resources.
- Pricing is based on the actual amount of resources consumed by an application, rather than on pre-purchased units of capacity

Summary:

- AWS Lambda
- Amazon API Gateway
- Amazon DynamoDB
- Amazon S3
- Amazon Kinesis
- Amazon Aurora
- AWS Fargate
- Amazon SNS
- Amazon SQS
- Amazon EFS
- Amazon RDS Proxy
- AWS AppSync
- AWS Step Function
- Lambda@Edge
- Amazon Athena
- Amazon EventBridge

### Stateless Service

- Stateless Service is a design principle that is applied within the **service-orientation** design paradigm, in order to design scalable services by separating them from their state data whenever possible

### Data Warehouse System Architecture

- leader node
  - manage communications with client programs and all communication with compute nodes
  - parse and develop execution plans to carry out database operations, in particular, the series of steps necessary to obtain results for complex queries
  - based on the execution plan, the leader node complies code, distributes the compiled code to the compute nodes, and assigns a portion of the data to each compute node
  - the leader node distributes SQL statements to the compute nodes only when a query references tables that are stored on the compute nodes. All other queries run exclusively on the leader node
- compute nodes
  - the leader node compiles code for individual elements of the execution plan and assigns the code to individual compute nodes. The compute nodes execute the compiled code and send intermediate results back to t he leader node for final aggregation

### AWS Resource Access Manager (AWS RAM)

- allow users to share resources with other AWS accounts or via AWS Organizations
- can be used to collate a set of AWS resources across multiple AWS accounts in order to share capacity

### AWS Tagging Strategies

- Amazon Web Services (AWS) allows customers to assign **metadata to their AWS resources in the form of** _ **tags** _ **.** (configuration of instances)
- Each tag is a simple label consisting of a customer-defined key and an optional value that can make it easier to manage, search for, and filter resources. Although there are no inherent types of tags, they enable customers to categorize resources by purpose, owner, environment, or other criteria. This webpage describes commonly used tagging categories and strategies to help AWS customers implement a consistent and effective tagging strategy. The following sections assume basic knowledge of AWS resources, tagging, detailed billing, and AWS Identity and Access Management (IAM).
- placement groups does not support tags

### AWS Multi Region (Disaster Recovery)

- from slow to fast:
  - backup & restore
  - pilot light
  - warm standby
  - multi site

### AWS AMI

- provide the information required to launch an instance, which is a virtual server in the cloud
- you can specify an AMI when you launch an instance, and you can launch as many instances from the AMI as you need
- you can also launch instances from as many different AMIs as you need