[TOC]

http://www.cloudbin.cn/?p=1948

## EC2

Which of the following strategies can be used to control access to your Amazon EC2 instances?

1. DB security groups
2. IAM policies
3. None of these
4. **EC2 security groups**

A user has launched 10 EC2 instances inside a placement group. Which of the below mentioned statements is true with respect to the placement group?

1. All instances must be in the same AZ
2. All instances can be across multiple regions
3. The placement group cannot have more than 5 instances
4. **All instances must be in the same region**

What would be the best way to retrieve the public IP address of your EC2 instance using the CLI?

1. Using tags
2. Using traceroute
3. Using ipconfig
4. **Using instance metadata**

Which of the following is NOT a characteristic of Amazon Elastic Compute Cloud (Amazon EC2)?

1. It can be used to launch as many or as few virtual servers as you need.
2. **It increases the need to forecast traffic by providing dynamic IP addresses for static cloud computing.**
3. It eliminates your need to invest in hardware up front, so you can develop and deploy applications faster.
4. It offers scalable computing capacity in the Amazon Web Services (AWS) cloud.

A user is accessing an EC2 instance on the SSH port for IP 10.20.30.40. Which one is a secure way to configure that the instance can be accessed only from this IP?

1. In the security group, open port 22 for IP 10.20.30.40
2. **In the security group, open port 22 for IP 10.20.30.40/32**
3. In the security group, open port 22 for IP 10.20.30.40/24
4. In the security group, open port 22 for IP 10.20.30.40/0

Which of the following statements is true of creating a launch configuration using an EC2 instance?

1. The launch configuration can be created only using the Query APIs.
2. **Auto Scaling automatically creates a launch configuration directly from an EC2 instance.**
3. A user should manually create a launch configuration before creating an Auto Scaling group.
4. The launch configuration should be created manually from the AWS CLI.

Do you need to shutdown your EC2 instance when you create a snapshot of EBS volumes that serve as root devices?

1. No, you only need to shutdown an instance before deleting it.
2. **Yes**
3. No, the snapshot would turn off your instance automatically.
4. No

Select the correct statement: Within Amazon EC2, when using Linux instances, the device name /dev/sda1 is _____.

1. reserved for EBS volumes
2. recommended for EBS volumes
3. recommended for instance store volumes
4. **reserved for the root device**

After setting up an EC2 security group with a cluster of 20 EC2 instances, you find an error in the security group settings. You quickly make changes to the security group settings. When will the changes to the settings be effective?

1. **The settings will be effective immediately for all the instances in the security group.**
2. The settings will be effective only when all the instances are restarted.
3. The settings will be effective for all the instances only after 30 minutes.
4. The settings will be effective only for the new instances added to the security group.

You need a solution to distribute traffic evenly across all of the containers for a task running on Amazon ECS. Your task definitions define dynamic host port mapping for your containers. What AWS feature provides this functionally?

1. **Application Load Balancers support dynamic host port mapping.**
2. CloudFront custom origins support dynamic host port mapping.
3. All Elastic Load Balancing instances support dynamic host port mapping.
4. Classic Load Balancers support dynamic host port mapping.

## IAM

You log in to IAM on your AWS console and notice the following message. “Delete your root access keys.” Why do you think IAM is requesting this?

1. Because the root access keys will expire as soon as you log out.
2. Because the root access keys expire after 1 week
3. Because the root access keys are the same for all users
4. **Because they provide unrestricted access to your AWS resources**

What does the following policy for Amazon EC2 do? { “Statement”:[{ “Effect”: “Allow”, “Action”:”ec2: Describe*”, “Resource”:”*” }] }

1. **Allow users to use actions that start with “Describe” over all the EC2 resources.**
2. Share an AMI with a partner
3. Share an AMI within the account
4. Allow a group to only be able to describe, run, stop, start, and terminate instances

You have been asked to tighten up the password policies in your organization after a serious security breach, so you need to consider every possible security measure. Which of the following is not an account password policy for IAM Users that can be set?

1. Force IAM users to contact an account administrator when the user has allowed his or her password to expire.
2. A minimum password length.
3. **Force IAM users to contact an account administrator when the user has entered his password incorrectly.**
4. Prevent IAM users from reusing previous passwords.

A company is building software on AWS that requires access to various AWS services. Which configuration should be used to ensure that AWS credentials (i.e ., Access Key ID/Secret Access Key combination) are not compromised?

1. Enable Multi-Factor Authentication for your AWS root account.
2. **Assign an lAM role to the Amazon EC2 instance.**
3. Store the AWS Access Key ID/Secret Access Key combination in software comments.
4. Assign an lAM user to the Amazon EC2 Instance.

After creating a new lAM user which of the following must be done before they can successfully make API calls?

1. Add a password to the user.
2. Enable Multi-Factor Authentication for the user.
3. Assign a Password Policy to the user.
4. **Create a set of Access Keys for the user.**

Your system recently experienced down time during the troubleshooting process. You found that a new administrator mistakenly terminated several production EC2 instances. Which of the following strategies will help prevent a similar situation in the future? The administrator still must be able to: launch, start stop, and terminate development resources. launch and start production instances.

1. Create an lAM user, which is not allowed to terminate instances by leveraging production EC2 termination protection.
2. **Leverage resource based tagging along with an lAM user, which can prevent specific users from terminating production EC2 resources.**
3. Leverage EC2 termination protection and multi-factor authentication, which together require users to authenticate before terminating EC2 instances
4. Create an lAM user and apply an lAM role which prevents users from terminating production EC2 instances.

Which service enables AWS customers to manage users and permissions in AWS?

1. AWS Access Control Service (ACS}
2. **AWS Identity and Access Management (lAM}**
3. AWS Identity Manager (AIM}

Through which of the following interfaces is AWS Identity and Access Management available?

A） AWS Management Console

B） Command line interface (CLI)

C) lAM Query API

D) Existing libraries

1. Only through Command line interface (CLI)
2. A, B and C
3. A and C
4. **All of the above**

## S3

One of the criteria for a new deployment is that the customer wants to use AWS Storage Gateway. However you are not sure whether you should use gateway-cached volumes or gateway-stored volumes or even what the differences are. Which statement below best describes those differences?

1. **Gateway-cached lets you store your data in Amazon Simple Storage Service (Amazon S3) and retain a copy of frequently accessed data subsets locally. Gateway-stored enables you to configure your on-premises gateway to store all your data locally and then asynchronously back up point-in-time snapshots of this data to Amazon S3.**
2. Gateway-cached is free whilst gateway-stored is not.
3. Gateway-cached is up to 10 times faster than gateway-stored.
4. Gateway-stored lets you store your data in Amazon Simple Storage Service (Amazon S3) and retain a copy of frequently accessed data subsets locally. Gateway-cached enables you to configure your on-premises gateway to store all your data locally and then asynchronously back up point-in-time snapshots of this data to Amazon S3.

A client of yours has a huge amount of data stored on Amazon S3, but is concerned about someone stealing it while it is in transit. You know that all data is encrypted in transit on AWS, but which of the following is wrong when describing server-side encryption on AWS?

1. Amazon S3 server-side encryption employs strong multi-factor encryption.
2. Amazon S3 server-side encryption uses one of the strongest block ciphers available, 256-bit Advanced Encryption Standard (AES-256), to encrypt your data.
3. **In server-side encryption, you manage encryption/decryption of your data, the encryption keys, and related tools.**
4. Server-side encryption is about data encryption at rest—that is, Amazon S3 encrypts your data as it writes it to disks.

You are signed in as root user on your account but there is an Amazon S3 bucket under your account that you cannot access. What is a possible reason for this?

1. **An IAM user assigned a bucket policy to an Amazon S3 bucket and didn’t specify the root user as a principal.**
2. The S3 bucket is full.
3. The S3 bucket has reached the maximum number of objects allowed.
4. You are in the wrong availability zone

Which one of the below is not an AWS Storage Service?

1. Amazon S3
2. Amazon Glacier
3. **Amazon CloudFront**
4. Amazon EBS

A user is hosting a website in the US West-1 region. The website has the highest client base from the Asia-Pacific (Singapore / Japan) region. The application is accessing data from S3 before serving it to client. Which of the below mentioned regions gives a better performance for S3 objects?

1. Japan
2. Singapore
3. US East
4. **US West-1**

You have just discovered that you can upload your objects to Amazon S3 using Multipart Upload API. You start to test it out but are unsure of the benefits that it would provide. Which of the following is not a benefit of using multipart uploads?

1. You can begin an upload before you know the final object size.
2. Quick recovery from any network issues.
3. Pause and resume object uploads
4. **It’s more secure than normal upload.**

Which of the following would you use to list your AWS Import/Export jobs?

1. Amazon RDS
2. AWS Import/Export Web Service Tool
3. **Amazon S3 REST API**
4. AWS Elastic Beanstalk

You have set up an S3 bucket with a number of images in it and you have decided that you want anybody to be able to access these images, even anonymous users. To accomplish this you create a bucket policy. You will need to use an Amazon S3 bucket policy that specifies a __________ in the principal element, which means anyone can access the bucket.

1. hash tag (#)
2. anonymous user
3. **wildcard (\*)**
4. S3 user

An organization has a statutory requirement to protect the data at rest for the S3 objects. Which of the below mentioned options need not be enabled by the organization to achieve data security?

1. MFA delete for S3 objects
2. Client side encryption
3. Bucket versioning
4. **Data replication**

注释：AWS S3提供了多种选项来实现REST数据的保护。 选项包括权限（策略），加密（客户端和服务器端），存储桶版本控制和基于MFA的删除。 用户可以启用任何这些选项来实现数据保护。 数据复制是AWS的内部工具，S3在所有可用区域中复制每个对象，在这种情况下组织无需启用它 http://media.amazonwebservices.com/AWS_Security_Best_Practices.pdf

Content and Media Server is the latest requirement that you need to meet for a client. The client has been very specific about his requirements such as low latency, high availability, durability, and access control. Potentially there will be millions of views on this server and because of “spiky” usage patterns, operations teams will need to provision static hardware, network, and management resources to support the maximum expected need. The Customer base will be initially low but is expected to grow and become more geographically distributed. Which of the following would be a good solution for content distribution?

1. Amazon S3 as both the origin server and for caching
2. AWS Storage Gateway as the origin server and Amazon EC2 for caching
3. AWS CloudFront as both the origin server and for caching
4. **Amazon S3 as the origin server and Amazon CloudFront for caching**

解析：随着您的客户群的增长和地理位置的分布，使用Amazon CloudFront等高性能边缘缓存可以显着提高延迟，容错和成本。通过使用Amazon S3作为Amazon CloudFront分配的源服务器，您可以获得快速的网络内数据传输速率，简单的发布/缓存工作流和统一的安全框架等优势。 Amazon S3和Amazon CloudFront可以由Web服务，AWS管理控制台或许多第三方管理工具进行配置 http://media.amazonwebservices.com/architecturecenter/AWS_ac_ra_media_02.pdf

You have been given a scope to set up an AWS Media Sharing Framework for a new start up photo sharing company similar to flickr. The first thing that comes to mind about this is that it will obviously need a huge amount of persistent data storage for this framework. Which of the following storage options would be appropriate for persistent storage?

1. Amazon Glacier or Amazon S3
2. Amazon Glacier or AWS Import/Export
3. AWS Import/Export or Amazon CloudFront
4. **Amazon EBS volumes or Amazon S3**

Just when you thought you knew every possible storage option on AWS you hear someone mention Reduced Redundancy Storage (RRS) within Amazon S3. What is the ideal scenario to use Reduced Redundancy Storage (RRS)?

1. Huge volumes of data
2. Sensitve data
3. **Non-critical or reproducible data**
4. Critical data

You have some very sensitive data stored on AWS S3 and want to try every possible alternative to keeping it secure in regards to access control. What are the mechanisms available for access control on AWS S3?

1. (**IAM) policies, Access Control Lists (ACLs), bucket policies, and query string authentication.**
2. (IAM) policies, Access Control Lists (ACLs) and bucket policies.
3. Access Control Lists (ACLs), bucket policies, and query string authentication
4. (IAM) policies, Access Control Lists (ACLs), bucket policies, query string authentication and encryption.

Which one of the following can’t be used as an origin server with Amazon CloudFront?

1. A web server running in your infrastructure
2. Amazon S3
3. **Amazon Glacier**
4. A web server running on Amazon EC2 instances

分析: Amazon CloudFront旨在与Amazon S3一起作为您的源服务器，客户还可以将Amazon CloudFront与在Amazon EC2实例上运行的源服务器或任何其他自定义源一起使用 [http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web.html ](http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web.html)

Which of the following are valid statements about Amazon S3? Choose 2 answers

1. S3 provides read-after-write consistency for any type of PUT or DELETE
2. Consistency is not guaranteed for any type of PUT or DELETE
3. **A successful response to a PUT request only occurs when a complete object is saved.**
4. Partially saved objects are immediately readable with a GET after an overwrite PUT.
5. **S3 provides eventual consistency for overwrite PUTS and DELETES.**

## VPC

True or false: A VPC contains multiple subnets, where each subnet can span multiple Availability Zones.

1. This is true only if requested during the set-up of VPC.
2. This is true
3. **This is false**
4. This is true only for US regions

正确或错误：VPC包含多个子网，其中每个子网可以跨越多个可用区。

You are setting up a VPC and you need to set up a public subnet within that VPC. Which following requirement must be met for this subnet to be considered a public subnet?

1. Subnet’s traffic is not routed to an internet gateway but has its traffic routed to a virtual private gateway.
2. **Subnet’s traffic is routed to an internet gateway.**
3. Subnet’s traffic is not routed to an internet gateway.
4. None of these answers can be considered a public subnet.

解析：虚拟私有云（VPC）是专用于您的AWS账户的虚拟网络。它在逻辑上与AWS云中的其他虚拟网络隔离。您可以将AWS资源（例如Amazon EC2实例）启动到您的VPC中。您可以配置VPC：您可以选择其IP地址范围，创建子网以及配置路由表，网络网关和安全设置。子网是VPC中的一系列IP地址。您可以将AWS资源启动到您选择的子网中。将公有子网用于必须连接到Internet的资源，将私有子网用于不连接到Internet的资源。

  如果子网的流量路由到Internet网关，则子网称为公有子网。
  如果子网没有到Internet网关的路由，则子网称为私有子网。
  如果子网没有到Internet网关的路由，但其流量路由到虚拟专用网关，则该子网称为仅VPN子网。

You are setting up your first Amazon Virtual Private Cloud (Amazon VPC) so you decide to use the VPC wizard in the AWS console to help make it easier for you. Which of the following statements is correct regarding instances that you launch into a default subnet via the VPC wizard?

1. Instances that you launch into a default subnet receive a public IP address and 10 private IP addresses.
2. **Instances that you launch into a default subnet receive both a public IP address and a private IP address.**
3. Instances that you launch into a default subnet don’t receive any ip addresses and you need to define them manually.
4. Instances that you launch into a default subnet receive a public IP address and 5 private IP addresses.

解析：您启动到默认子网的实例会同时接收公共IP地址和私有IP地址。默认子网中的实例还接收公用和专用DNS主机名。您在默认VPC中启动到非默认子网的实例不会收到公共IP地址或DNS主机名。您可以更改子网的默认公共IP寻址行为

You need to set up security for your VPC and you know that Amazon VPC provides two features that you can use to increase security for your VPC: security groups and network access control lists (ACLs). You have already looked into security groups and you are now trying to understand ACLs. Which statement below is incorrect in relation to ACLs?

1. Supports allow rules and deny rules.
2. **Is stateful: Return traffic is automatically allowed, regardless of any rules.**
3. Processes rules in number order when deciding whether to allow traffic.
4. Operates at the subnet level (second layer of defense).

You are configuring a new VPC for one of your clients for a cloud migration project, and only a public VPN will be in place. After you created your VPC, you created a new subnet, a new internet gateway, and attached your internet gateway to your VPC. When you launched your first instance into your VPC, you realized that you aren’t able to connect to the instance, even if it is configured with an elastic IP. What should be done to access the instance?

1. **A route should be created as 0.0.0.0/0 and your internet gateway as target.**
2. Attach another ENI to the instance and connect via new ENI.
3. A NAT instance should be created and all traffic should be forwarded to NAT instance.
4. A NACL should be created that allows all outbound traffic.

解析：所有流量都应通过Internet Gateway进行路由。 因此，应创建一个路径，将0.0.0.0/0作为源，并将Internet Gateway作为目标

Doug has created a VPC with CIDR 10.201.0.0/16 in his AWS account. In this VPC he has created a public subnet with CIDR block 10.201.31.0/24. While launching a new EC2 from the console, he is not able to assign the private IP address 10.201.31.6 to this instance. Which is the most likely reason for this issue?

- Private IP address 10.201.31.6 is blocked via ACLs in Amazon infrastructure as a part of platform security
- **Private address IP 10.201.31.6 is currently assigned to another interface.**
- Private IP address 10.201.31.6 is not part of the associated subnet’s IP address range.
- Private IP address 10.201.31.6 is reserved by Amazon for IP networking purposes.

You have been setting up an Amazon Virtual Private Cloud (Amazon VPC) for your company, including setting up subnets. Security is a concern, and you are not sure which is the best security practice for securing subnets in your VPC. Which statement below is correct in describing the protection of AWS resources in each subnet?

1. **You can use multiple layers of security, including security groups and network access control lists (ACL).**
2. You can only use access control lists (ACL).
3. You don’t need any security in subnets.
4. You can use multiple layers of security, including security groups, network access control lists (ACL) and CloudHSM.

You need to set up security for your VPC and you know that Amazon VPC provides two features that you can use to increase security for your VPC: Security groups and network access control lists (ACLs). You start to look into security groups first. Which statement below is incorrect in relation to security groups?

1. Are stateful: Return traffic is automatically allowed, regardless of any rules.
2. Evaluate all rules before deciding whether to allow traffic.
3. **Support allow rules and deny rules.**
4. Operate at the instance level (first layer of defense).

解析：？？？

Any person or application that interacts with AWS requires security credentials. AWS uses these credentials to identify who is making the call and whether to allow the requested access. You have just set up a VPC network for a client and you are now thinking about the best way to secure this network. You set up a security group called vpcsecuritygroup. Which following statement is true in respect to the initial settings that will be applied to this security group if you choose to use the default settings for this group?

1. Allow all inbound traffic and allow no outbound traffic.
2. **Allow no inbound traffic and allow all outbound traffic.**
3. Allow inbound traffic on port 80 only and allow all outbound traffic.
4. Allow all inbound traffic and allow all outbound traffic.

## Route53

Can resource record sets in a hosted zone have a different domain suffix (for example, www.blog. acme.com and www.acme.ca)?

1. Yes, it can have for a maximum of three different TLDs
2. Yes
3. Yes, it can have depending on the TLD
4. **No**

Having set up a website to automatically be redirected to a backup website if it fails, you realize that there are different types of failovers that are possible. You need all your resources to be available the majority of the time. Using Amazon Route 53 which configuration would best suit this requirement?

1. **Active-active failover.**
2. None. Route 53 can’t failover.
3. Active-passive failover.
4. Active-active-passive and other mixed configurations

**解析：** You can set up a variety of failover configurations using Amazon Route 53 alias: weighted, latency, geolocation routing, and failover resource record sets.

- Active-active failover: Use this failover configuration when you want all of your resources to be available the majority of the time. When a resource becomes unavailable, Amazon Route 53 can detect that it’s unhealthy and stop including it when responding to queries.
- Active-passive failover: Use this failover configuration when you want a primary group of resources to be available the majority of the time and you want a secondary group of resources to be on standby in case all of the primary resources become unavailable. When responding to queries, Amazon Route 53 includes only the healthy primary resources. If all of the primary resources are unhealthy, Amazon Route 53 begins to include only the healthy secondary resources in response to DNS queries.
- Active-active-passive and other mixed configurations: You can combine alias and non-alias resource record sets to produce a variety of Amazon Route 53 behaviors.

Reference: http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover.html

Regarding Amazon Route 53, if your application is running on Amazon EC2 instances in two or more Amazon EC2 regions and if you have more than one Amazon EC2 instance in one or more regions, you can use _______ to route traffic to the correct region and then use ________to route traffic to instances within the region, based on probabilities that you specify.

1. weighted-based routing; alias resource record sets
2. **latency-based routing; weighted resource record sets**
3. weighted-based routing; weighted resource record sets
4. latency-based routing; alias resource record sets

解析：关于Amazon Route 53，如果您的应用程序在两个或多个Amazon EC2区域中的Amazon EC2实例上运行，并且您在一个或多个区域中有多个Amazon EC2实例，则可以使用基于延迟的路由来路由流量 到正确的区域，然后使用加权资源记录集根据您指定的权重将流量路由到区域内的实例

You have created a Route 53 latency record set from your domain to a machine in Northern Virginia and a similar record to a machine in Sydney. When a user located in US visits your domain he will be routed to:

1. **Northern Virginia**
2. Sydney
3. Both, Northern Virginia and Sydney
4. Depends on the Weighted Resource Record Sets

解析：如果您的应用程序在两个或多个Amazon EC2区域中的Amazon EC2实例上运行，并且您在一个或多个区域中有多个Amazon EC2实例，则可以使用基于延迟的路由将流量路由到正确的区域 然后使用加权资源记录集根据您指定的权重将流量路由到区域内的实例。

If your application is running on Amazon EC2 instances in two or more Amazon EC2 regions, and if you have more than one Amazon EC2 instance in one or more regions, you can use latency-based routing to route traffic to the correct region and then use weighted resource record sets to route traffic to instances within the region based on weights that you specify. Which of the following statements is true in regards to transferring domain names to AWS?

1. You can’t transfer existing domains to AWS.
2. **You can transfer existing domains into Amazon Route 53’s management.**
3. You can transfer existing domains via AWS Direct Connect.
4. You can transfer existing domains via AWS Import/Export.

解析：使用Amazon Route 53，您可以使用AWS管理控制台或易于使用的API创建和管理公共DNS记录。如果您需要域名，您可以找到可用的名称并使用Amazon Route 53进行注册。您还可以将现有域名转移到Amazon Route 53的管理中

In Route 53, what does a Hosted Zone refer to?

1. A hosted zone is a collection of geographical load balancing rules for Route 53.
2. **A hosted zone is a collection of resource record sets hosted by Route 53.**
3. A hosted zone is a selection of specific resource record sets hosted by CloudFront for distribution to Route 53.
4. A hosted zone is the Edge Location that hosts the Route 53 records for a user.

解析：托管区域是指由Route 53托管的一组资源记录集

You have deployed a web application targeting a global audience across multiple AWS Regions under the domain name.example.com. You decide to use Route53 Latency-Based Routing to serve web requests to users from the region closest to the user. To provide business continuity in the event of server downtime you configure weighted record sets associated with two web servers in separate Availability Zones per region. Dunning a DR test you notice that when you disable all web servers in one of the regions Route53 does not automatically direct all users to the other region. What could be happening? {Choose 2 answers)

1. Latency resource record sets cannot be used in combination with weighted resource record sets.
2. **You did not setup an HTTP health check to one or more of the weighted resource record sets associated with the disabled web servers.**
3. The value of the weight associated with the latency alias resource record set in the region with the disabled servers is higher than the weight for the other region.
4. One of the two working web servers in the other region did not pass its HTTP health check.
5. **You did not set “Evaluate Target Health” to “Yes” on the latency alias resource record set associated with example com in the region where you disabled the servers.**

解析：如何在复杂的Amazon Route 53配置中运行健康检查在复杂配置中检查资源的健康状况与在简单配置中的工作方式大致相同。但是，在复杂配置中，您可以结合使用别名资源记录集（包括加权别名，延迟别名和故障转移别名）和非别名资源记录集来构建决策树，从而更好地控制Amazon Route 53如何响应请求。有关更多信息，请参阅简单Amazon Route 53配置中的运行状况检查的工作原理。

You are designing Internet connectivity for your VPC. The Web servers must be available on the Internet. The application must have a highly available architecture. Which alternatives should you consider? (Choose 2 answers)

1. Configure a NAT instance in your VPC. Create a default route via the NAT instance and associate it with all subnets. Configure a DNS A record that points to the NAT instance public IP address.
2. Configure a CloudFront distribution and configure the origin to point to the private IP addresses of your Web servers. Configure a Route53 CNAME record to your CloudFront distribution.
3. **Place all your web servers behind ELB. Configure a Route53 CNAME to point to the ELB DNS name.**
4. **Assign EIPs to all web servers. Configure a Route53 record set with all EIPs with health checks and DNS failover.**
5. Configure ELB with an EIP. Place all your Web servers behind ELB. Configure a Route53 A record that points to the EIP.

What does Amazon Route53 provide?

1. A global Content Delivery Network.
2. None of these.
3. **A scalable Domain Name System.**
4. An SSH endpoint for Amazon EC2.

Which of the following statements are rue about Amazon Route 53 resource records? Choose 2 answers

1. **An Alias record can map one DNS name to another Amazon Route 53 DNS name.** 
2. A CNAME record can be created for your zone apex.
3. **An Amazon Route 53 CNAME record can point to any DNS record hosted anywhere.**
4. TTL can be set for an Alias record in Amazon Route 53.
5. An Amazon Route 53 Alias record can point to any DNS record hosted anywhere.

## DB

A client needs you to import some existing infrastructure from a dedicated hosting provider to AWS to try and save on the cost of running his current website. He also needs an automated process that manages backups, software patching, automatic failure detection, and recovery. You are aware that his existing set up currently uses an Oracle database. Which of the following AWS databases would be best for accomplishing this task?

1. **Amazon RDS**
2. Amazon Redshift
3. Amazon SimpleDB
4. Amazon ElastiCache

解析：Amazon RDS使您可以访问熟悉的MySQL，Oracle，SQL Server或PostgreSQL数据库引擎的功能。这意味着您现在已经使用现有数据库的代码，应用程序和工具可以与Amazon RDS一起使用。 Amazon RDS自动修补数据库软件并备份数据库，将备份存储在用户定义的保留期内并启用时间点恢复。

You are building infrastructure for a data warehousing solution and an extra request has come through that there will be a lot of business reporting queries running all the time and you are not sure if your current DB instance will be able to handle it. What would be the best solution for this?

1. DB Parameter Groups
2. **Read Replicas**
3. Multi-AZ DB Instance deployment
4. Database Snapshots

解析：Read Replicas可以轻松利用MySQL的内置复制功能，弹性扩展超出单个数据库实例的容量限制，从而实现读取大量数据库工作负载。在各种情况下，为给定的源数据库实例部署一个或多个只读副本可能有意义。部署只读副本的常见原因包括：
  扩展超出单个数据库实例的计算或I / O容量，用于读取大量数据库工作负载。这种过量的读取流量可以被定向到一个或多个只读副本。
  在源数据库实例不可用时提供读取流量。如果源数据库实例无法接收I / O请求（例如，由于I / O暂停进行备份或计划维护），则可以将读取流量定向到只读副本。对于此用例，请记住，由于源数据库实例不可用，因此只读副本上的数据可能是“陈旧的”。
  业务报告或数据仓库方案;您可能希望业务报告查询针对只读副本而不是主要生产数据库实例运行。

In Amazon RDS, security groups are ideally used to:

1. Define maintenance period for database engines
2. Launch Amazon RDS instances in a subnet
3. Create, describe, modify, and delete DB instances
4. **Control what IP addresses or EC2 instances can connect to your databases on a DB instance**

解析：在Amazon RDS中，安全组用于控制IP地址或EC2实例可以连接到数据库实例上的数据库。 首次创建数据库实例时，其防火墙会阻止任何数据库访问，除非通过关联的安全组指定的规则.

You are running PostgreSQL on Amazon RDS and it seems to be all running smoothly deployed in one availability zone. A database administrator asks you if DB instances running PostgreSQL support Multi-AZ deployments. What would be a correct response to this question?

1. **Yes.**
2. Yes but only for small db instances.
3. No.
4. Yes but you need to request the service from AWS.

解析：Amazon RDS支持运行多个PostgreSQL版本的数据库实例。目前我们支持PostgreSQL版本9.3.1,9.3.2和9.3.3。您可以创建数据库实例和数据库快照，时间点还原和备份。运行PostgreSQL的数据库实例支持多可用区部署，预配置IOPS，并且可以在VPC内创建。您还可以使用SSL连接到运行PostgreSQL的数据库实例。您可以使用任何标准SQL客户端应用程序从客户端计算机运行该实例的命令。这些应用程序包括pgAdmin，一种流行的开源管理和PostgreSQL开发工具，或psql，一种命令行实用程序，是PostgreSQL安装的一部分。为了提供托管服务体验，Amazon RDS不提供对数据库实例的主机访问，并且它限制对需要高级权限的某些系统过程和表的访问。 Amazon RDS支持使用任何标准SQL客户端应用程序访问数据库实例上的数据库。 Amazon RDS不允许通过Telnet或Secure Shell（SSH）直接访问数据库实例。

An online gaming site asked you if you can deploy a database that is a fast, highly scalable NoSQL database service in AWS for a new site that he wants to build. Which database should you recommend?

1. **Amazon DynamoDB**
2. Amazon RDS
3. Amazon Redshift
4. Amazon SimpleDB

解析：Amazon DynamoDB非常适合需要任何规模的低延迟和可预测性能但不需要复杂的查询功能（如连接或事务）的数据库应用程序。 Amazon DynamoDB是一个完全托管的NoSQL数据库服务，可提供高性能，可预测的吞吐量和低成本。它易于设置，操作和扩展。使用Amazon DynamoDB，您可以从小规模开始，指定所需的吞吐量和存储，并轻松扩展容量需求。 Amazon DynamoDB会自动对多个服务器上的数据进行分区，以满足您的请求容量。此外，DynamoDB会自动跨AWS区域内的多个可用区域同步复制数据，以确保高可用性和数据持久性。

Amazon RDS provides high availability and failover support for DB instances using _______.

1. Customized deployments
2. Appstream customizations
3. Log events
4. **Multi-AZ deployments**

解析：Amazon RDS使用多可用区部署为数据库实例提供高可用性和故障转移支持。 Oracle，PostgreSQL，MySQL和MariaDB数据库实例的多可用区部署使用亚马逊技术，而SQL Server数据库实例使用SQL Server镜像

You have just set up a large site for a client which involved a huge database which you set up with Amazon RDS to run as a Multi-AZ deployment. You now start to worry about what will happen if the database instance fails. Which statement best describes how this database will function if there is a database failure?

1. **Updates to your DB Instance are synchronously replicated across Availability Zones to the standby in order to keep both in sync and protect your latest database updates against DB Instance failure.**
2. Your database will not resume operation without manual administrative intervention.
3. Updates to your DB Instance are asynchronously replicated across Availability Zones to the standby in order to keep both in sync and protect your latest database updates against DB Instance failure.
4. Updates to your DB Instance are synchronously replicated across S3 to the standby in order to keep both in sync and protect your latest database updates against DB Instance failure.

**解析：**

- Amazon Relational Database Service (Amazon RDS) is a managed service that makes it easy to set up, operate, and scale a relational database in the cloud. It provides cost-efficient and resizable capacity, while managing time-consuming database administration tasks, freeing you up to focus on your applications and business.
- When you create or modify your DB Instance to run as a Multi-AZ deployment, Amazon RDS automatically provisions and maintains a synchronous “standby” replica in a different Availability Zone.
- Updates to your DB Instance are **synchronously** replicated across Availability Zones to the standby in order to keep both in sync and protect your latest database updates against DB Instance failure. During certain types of planned maintenance, or in the unlikely event of DB Instance failure or Availability Zone failure, Amazon RDS will automatically failover to the standby so that you can resume database writes and reads as soon as the standby is promoted. Since the name record for your DB Instance remains the same, you application can resume database operation without the need for manual administrative intervention.
- With Multi-AZ deployments, replication is transparent: you do not interact directly with the standby, and it cannot be used to serve read traffic. If you are using Amazon RDS for MySQL and are looking to scale read traffic beyond the capacity constraints of a single DB Instance, you can deploy one or more Read Replicas.

Reference: http://aws.amazon.com/rds/faqs/

A favored client needs you to quickly deploy a database that is a relational database service with minimal administration as he wants to spend the least amount of time administering it. Which database would be the best option?

1. Amazon SimpleDB
2. Your choice of relational AMIs on Amazon EC2 and EBS.
3. **Amazon RDS**
4. Amazon Redshift

Your application is using an ELB in front of an Auto Scaling group of web/application servers deployed across two AZs and a Multi-AZ RDS Instance for data persistence. The database CPU is often above 80% usage and 90% of I/O operations on the database are reads. To improve performance you recently added a single-node Memcached ElastiCache Cluster to cache frequent DB query results. In the next weeks the overall workload is expected to grow by 30%. Do you need to change anything in the architecture to maintain the high availability or the application with the anticipated additional load? Why?

- **Yes, you should deploy two Memcached ElastiCache Clusters in different AZs because the RDS instance will not be able to handle the load if the cache node fails.**
- No, if the cache node fails you can always get the same data from the DB without having any availability impact.
- No, if the cache node fails the automated ElastiCache node recovery feature will prevent any availability impact.
- Yes, you should deploy the Memcached ElastiCache Cluster with two nodes in the same AZ as the RDS DB master instance to handle the load if one cache node fails.

解析：缓存的主要目标通常是从数据库或其他主数据源卸载读取。在大多数应用程序中，您都会定期查询数据热点，但只会定期更新。想想博客或新闻网站的首页，或在线游戏的前100名排行榜。在这种情况下，您的应用可以在再次更新之前接收数十，数百甚至数千个相同数据的请求。让缓存层处理这些查询有几个优点。
  首先，添加内存缓存比扩展到更大的数据库集群要便宜得多。
  其次，内存缓存也更容易扩展，因为它比关系数据库更容易水平分布内存缓存。
  最后，缓存层在使用突然激增时提供请求缓冲区。如果您的应用或游戏最终出现在Reddit或App Store的首页上，那么看到峰值是正常应用程序负载的10到100倍并非闻所未闻。即使您自动扩展应用程序实例，10倍的请求峰值也可能使您的数据库非常不满意。

After you recommend Amazon Redshift to a client as an alternative solution to paying data warehouses to analyze his data, your client asks you to explain why you are recommending Redshift. Which of the following would be a reasonable response to his request?

1. It has high performance at scale as data and query complexity grows.
2. It prevents reporting and analytic processing from interfering with the performance of OLTP workloads.
3. You don’t have the administrative burden of running your own data warehouse and dealing with setup, durability, monitoring, scaling, and patching.
4. **All answers listed are a reasonable response to his question**

解析：Amazon Redshift通过使用列式存储技术提高I / O效率并跨多个节点并行化查询，从而提供快速查询性能。 Redshift使用标准的PostgreSQL JDBC和ODBC驱动程序，允许您使用各种熟悉的SQL客户端。数据加载速度与群集大小呈线性关系，与Amazon S3，Amazon DynamoDB，Amazon ElasticMapReduce，Amazon Kinesis或任何支持SSH的主机集成。 AWS为具有多种需求组合的客户推荐Amazon Redshift，例如：
  随着数据和查询复杂性的增加，大规模的高性能
  希望防止报告和分析处理干扰OLTP工作负载的性能
  使用标准SQL和现有BI工具持久保存和查询大量结构化数据
  希望运行自己的数据仓库并处理设置，持久性，监控，扩展和修补的管理负担

## Application Integration

A user has created photo editing software and hosted it on EC2. The software accepts requests from the user about the photo format and resolution and sends a message to S3 to enhance the picture accordingly. Which of the below mentioned AWS services will help make a scalable software with the AWS infrastructure in this scenario?

1. AWS Simple Notification Service
2. **AWS Simple Queue Service**
3. AWS Elastic Transcoder
4. AWS Glacier

A user is making a scalable web application with compartmentalization. The user wants the log module to be able to be accessed by all the application functionalities in an asynchronous way. Each module of the application sends data to the log module, and based on the resource availability it will process the logs. Which AWS service helps this functionality?

1. **AWS Simple Queue Service**
2. AWS Simple Notification Service
3. AWS Simple Workflow Service
4. AWS Simple Email Service

Your website is serving on-demand training videos to your workforce. Videos are uploaded monthly in high resolution MP4 format. Your workforce is distributed globally often on the move and using company-provided tablets that require the HTTP Live Streaming (HLS) protocol to watch a video. Your company has no video transcoding expertise and it required you may need to pay for a consultant. How do you implement the most cost-efficient architecture without compromising high availability and quality of video delivery’?

1. A video transcoding pipeline running on EC2 using SQS to distribute tasks and Auto Scaling to adjust the number of nodes depending on the length of the queue. EBS volumes to host videos and EBS snapshots to incrementally backup original files after a few days. CloudFront to serve HLS transcoded videos from EC2.
2. Elastic Transcoder to transcode original high-resolution MP4 videos to HLS. EBS volumes to host videos and EBS snapshots to incrementally backup original files after a few days. CloudFront to serve HLS transcoded videos from EC2.
3. **Elastic Transcoder to transcode original high-resolution MP4 videos to HLS. S3 to host videos with Lifecycle Management to archive original files to Glacier after a few days. CloudFront to serve HLStranscoded videos from S3.**
4. A video transcoding pipeline running on EC2 using SQS to distribute tasks and Auto Scaling to adjust the number of nodes depending on the length of the queue. 53 to host videos with Lifecycle Management to archive all files to Glacier after a few days. CloudFront to serve HLS transcoded videos from Glacier.

A company is running a batch analysis every hour on their main transactional DB running on an RDS MySQL instance to populate their central Data Warehouse running on Redshift. During the execution of the batch their transactional applications are very slow. When the batch completes they need to update the top management dashboard with the new data. The dashboard is produced by another system running on-premises that is currently started when a manually-sent email notifies that an update is required. The on-premises system cannot be modified because is managed by anotherteam. How would you optimize this scenario to solve performance issues and automate the process as much as possible?

1. Replace RDS with Redshift for the batch analysis and SNS to notify the on-premises system to update the dashboard
2. Replace RDS with Redshift for the batch analysis and SQS to send a message to the on-premises system to update the dashboard
3. **Create an RDS Read Replica for the batch analysis and SNS to notify me on-premises system to update the dashboard**
4. Create an RDS Read Replica for the batch analysis and SQS to send a message to the on-premises system to update the dashboard.

Amazon SWF is designed to help users …

1. Design graphical user interface interactions
2. Manage user identification and authorization
3. Store Web content
4. **Coordinate synchronous and asynchronous tasks which are distributed and fault tolerant.**

For which of the following use cases are Simple Workflow Service (SWF) and Amazon EC2 an appropriate solution? Choose 2 answers

1. Using as an endpoint to collect thousands of data points per hour from a distributed fleet of sensors
2. **Managing a multi-step and multi-decision checkout process of an e-commerce website**
3. **Orchestrating the execution of distributed and auditable business processes**
4. Using as an SNS (Simple Notification Service) endpoint to trigger execution of video transcoding jobs
5. Using as a distributed session store for your web application

A user has deployed an application on his private cloud. The user is using his own monitoring tool. He wants to configure it so that whenever there is an error, the monitoring tool will notify him via SMS. Which of the below mentioned AWS services will help in this scenario?

1. AWS SES
2. **AWS SNS**
3. None because the user infrastructure is in the private cloud
4. AWS SMS

**解析：** Amazon Simple Notification Service (Amazon SNS) is a fast, flexible, and fully managed push messaging service. Amazon SNS can be used to make push notifications to mobile devices. Amazon SNS can deliver notifications by SMS text message or email to the Amazon Simple Queue Service (SQS) queues or to any HTTP endpoint. In this case user can use the SNS apis to send SMS. Reference: http://aws.amazon.com/sns/

You require the ability to analyze a customer’s clickstream data on a website so they can do behavioral analysis. Your customer needs to know what sequence of pages and ads their customer clicked on. This data will be used in real time to modify the page layouts as customers click through the site to increase stickiness and advertising click-through. Which option meets the requirements for captioning and analyzing this data?

1. Log clicks in weblogs by URL store to Amazon S3, and then analyze with Elastic MapReduce
2. **Push web clicks by session to Amazon Kinesis and analyze behavior using Kinesis workers**
3. Write click events directly to Amazon Redshift and then analyze with SQL
4. Publish web clicks by session to an Amazon SQS queue men periodically drain these events to Amazon RDS and analyze with sql

Your company is in the process of developing a next generation pet collar that collects biometric information to assist families with promoting healthy lifestyles for their pets. Each collar will push 30kb of biometric data in JSON format every 2 seconds to a collection platform that will process and analyze the data providing health trending information back to the pet owners and veterinarians via a web portal. Management has tasked you to architect the collection platform ensuring the following requirements are met. Provide the ability for real-time analytics of the inbound biometric data. Ensure processing of the biometric data is highly durable. Elastic and parallel The results of the analytic processing should be persisted for data mining. Which architecture outlined below win meet the initial requirements for the collection platform?

- Utilize S3 to collect the inbound sensor data analyze the data from S3 with a daily scheduled Data Pipeline and save the results to a Redshift Cluster.
- **Utilize Amazon Kinesis to collect the inbound sensor data, analyze the data with Kinesis clients and save the results to a Red shift cluster using EMR.**
- Utilize SQS to collect the inbound sensor data analyze the data from SQS with Amazon Kinesis and save the results to a Microsoft SQL Server RDS instance.
- Utilize EMR to collect the inbound sensor data, analyze the data from EUR with Amazon Kinesis and save me results to Dynamo DB.