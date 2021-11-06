[TOC]

- 

```c
Section 1 - AWS Basics
(00:00:53) Introduction
(00:01:19) Amazon Web Services Overview
(00:04:26) AWS Global Infrastructure
(00:07:35) AWS Pricing
(00:15:37) Setup your AWS Free Tier Account
(00:21:19) Create a Billing Alarm
(00:27:34) IAM Overview
(00:32:47) Create IAM User and Group
(00:37:38) Amazon Virtual Private Cloud (VPC)
(00:48:36) Security Groups and Network ACLs
(00:58:40) AWS Public and Private Services
(01:00:51) Install the AWS Command Line Interface
 
Section 2 - Amazon Elastic Compute Cloud (EC2)
(01:02:08) Introduction
(01:02:36) Amazon EC2 Overview
(01:08:00) Launching an Amazon EC2 Instance
(01:16:42) Connecting to Amazon EC2 Instances
(01:27:48) Create a Website Using User Data
(01:33:52) Using Access Keys with EC2
(01:34:50) Using IAM Roles with EC2
(01:39:05) Scale Elastically with Amazon EC2 Auto Scaling
(01:47:47) Create a Target Tracking Scaling Policy
(01:55:43) Add Load Balancing with Amazon ELB
 
Section 3 - AWS Storage Services
(02:09:52) Introduction
(02:10:28) AWS Storage Services Overview
(02:12:40) Create an Attach EBS Volume
(02:20:47) Instance Store Volumes
(02:23:34) EBS Snapshots and AMIs
(02:31:38) Create Amazon EFS File System
(02:40:26) Amazon S3 Create Bucket and Make Public
(02:47:32) Working with S3 Objects from the AWS CLI
 
Section 4 - AWS Databases
(02:52:49) Introduction
(02:53:28) Amazon RDS Overview
(02:59:52) Create Amazon RDS Multi-AZ
(03:10:35) Add an Amazon RDS Read Replica
(03:16:18) Install WordPress on EC2 with RDS Database
(03:23:56) Amazon DynamoDB
 
Section 5 - Automation on AWS
(03:34:34) Introduction
(03:35:34) How to Deploy Infrastructure Using AWS CloudFormation
(03:37:36) Create Simple Stacks with AWS CloudFormation
(03:47:52) Create Complex Stack with AWS CloudFormation

(03:56:45) Deploy an Application Using AWS Elastic Beanstalk
 
Section 6 - DevOps on AWS
(04:02:00) Introduction
(04:02:43) Continuous Integration and Continuous Delivery (CI/CD)
(04:05:55) AWS CodePipeline with AWS Elastic Beanstalk
(04:15:26) Create AWS CodeStar Project
 
Section 7 - DNS Services and Content Delivery
(04:25:47) Introduction
(04:26:23) Amazon Route 53 Overview and Routing Policies
(04:30:22) Register Domain Using Route 53
(04:35:44) Create Amazon CloudFront Distribution with S3 Static Website
(04:44:30) Add an SSL/TLS Certificate and Route 53 Alias Record
 
Section 8 - Docker Containers and Serverless Computing
(04:53:35) Introduction
(04:54:34) Docker Containers on Amazon ECS
(05:06:06) Serverless with AWS Lambda
 
Section 9 - Application Integration and Loose Coupling
(05:13:12) Introduction
(05:14:01) Amazon SNS and Amazon SQS
(05:16:32) AWS Lambda to Amazon SQS Event Source Mapping
(05:22:49) Serverless Application - Amazon SQS, SNS, and Lambda

🔗 Digital Cloud Training: https://digitalcloud.training/
🔗 LinkedIn: https://www.linkedin.com/in/nealkdavis/
🔗 Twitter: https://twitter.com/nealkdavis
🔗 Instagram: https://www.instagram.com/digitalclou...
```



# AWS Basics for Beginners - Full Course



#### VPC

![image-20211105200536834](https://tva1.sinaimg.cn/large/008i3skNly1gw4j2ss9spj31hn0u043y.jpg)



![image-20211105200637257](https://tva1.sinaimg.cn/large/008i3skNly1gw4j3u8pvtj31hn0u0tdz.jpg)



- 路由转发表

![image-20211105200953817](https://tva1.sinaimg.cn/large/008i3skNly1gw4j78x786j31ht0u078m.jpg)

- 三个subnet都在这个范围之内

![image-20211105201020441](https://tva1.sinaimg.cn/large/008i3skNly1gw4j7poijdj31gu0u0aev.jpg)



#### Security Groups and Network ACLs

- 两个东西处在不同的level

![image-20211105201222175](https://tva1.sinaimg.cn/large/008i3skNly1gw4j9tmczkj31d60u0436.jpg)



- 有状态防火墙和无状态防火墙

![image-20211105201820780](https://tva1.sinaimg.cn/large/008i3skNly1gw4jg1u5maj31ju0u0gqq.jpg)

![image-20211105201937632](https://tva1.sinaimg.cn/large/008i3skNly1gw4jhdolklj31oo0owwht.jpg)



#### (00:58:40) AWS Public and Private Services

![image-20211105202117356](https://tva1.sinaimg.cn/large/008i3skNly1gw4jj3vnhij31pp0u0wjs.jpg)



#### Section 2 - Amazon Elastic Compute Cloud (EC2)

(01:02:08) Introduction

##### (01:02:36) Amazon EC2 Overview

![image-20211105233910352](https://tva1.sinaimg.cn/large/008i3skNgy1gw4p96nmo5j31l60u0wj7.jpg)



![image-20211105234050353](https://tva1.sinaimg.cn/large/008i3skNgy1gw4pasb5phj31dg0u041q.jpg)



![image-20211105234303302](https://tva1.sinaimg.cn/large/008i3skNgy1gw4pd3sz5sj31kw0u0wj9.jpg)



(01:08:00) Launching an Amazon EC2 Instance

![image-20211105234508104](https://tva1.sinaimg.cn/large/008i3skNgy1gw4pf9lxgvj31qx0u0tci.jpg)



![image-20211105235324285](https://tva1.sinaimg.cn/large/008i3skNgy1gw4pnvf9h8j31j50u0wjb.jpg)



##### (01:16:42) Connecting to Amazon EC2 Instances

![image-20211105235551467](https://tva1.sinaimg.cn/large/008i3skNgy1gw4pqhes2rj31rk0u047u.jpg)



![image-20211105235806254](https://tva1.sinaimg.cn/large/008i3skNgy1gw4psrf7tfj31wy0awq5x.jpg)



##### (01:27:48) Create a Website Using User Data

![image-20211106000100931](https://tva1.sinaimg.cn/large/008i3skNgy1gw4pvs00f8j31940dw0ua.jpg)

- 打开防火墙，然后通过IP访问即可



##### (01:33:52) Using Access Keys with EC2

![image-20211106000543470](https://tva1.sinaimg.cn/large/008i3skNgy1gw4q0o3h8nj31i90u040w.jpg)



##### (01:34:50) Using IAM Roles with EC2

![image-20211106000518727](https://tva1.sinaimg.cn/large/008i3skNgy1gw4q098oa5j31hz0u0776.jpg)

- 给ec2实例分配安全role，然后就可访问s3

![image-20211106000834554](https://tva1.sinaimg.cn/large/008i3skNgy1gw4q3o1527j31t40c0dku.jpg)



##### (01:39:05) Scale Elastically with Amazon EC2 Auto Scaling

![image-20211106001045709](https://tva1.sinaimg.cn/large/008i3skNgy1gw4q5xtr14j31e20u042n.jpg)

![image-20211106001142855](https://tva1.sinaimg.cn/large/008i3skNgy1gw4q6wymt7j31gu0tsq6l.jpg)



![image-20211106001309686](https://tva1.sinaimg.cn/large/008i3skNgy1gw4q8faiyhj31ig0u0q6n.jpg)



![image-20211106001325655](https://tva1.sinaimg.cn/large/008i3skNgy1gw4q8pabq6j31jm0u0wkp.jpg)

![image-20211106001446363](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qa3w6qej320w0gy78b.jpg)



##### (01:47:47) Create a Target Tracking Scaling Policy

![image-20211106001651649](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qca0tnvj31h50u0wja.jpg)

![image-20211106001741937](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qd6383fj31nu0b8dhz.jpg)

![image-20211106001800429](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qdftzo9j316s06idgk.jpg)

![image-20211106001849430](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qefsrhbj31ii0u049v.jpg)

![image-20211106001934583](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qf2sl9wj32060fe789.jpg)



##### (01:55:43) Add Load Balancing with Amazon ELB

![image-20211106002044048](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qgbvcj6j31mb0u0wjd.jpg)

![image-20211106002054315](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qgmrhb7j31nr0u0q7p.jpg)

![image-20211106002116161](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qgtu246j310002mq3d.jpg)

![image-20211106002427063](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qk67v7hj31bu0cu0uf.jpg)



![image-20211106002610124](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qlyzuodj318l0u0q6u.jpg)

![image-20211106002835981](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qojwotmj31h40u0jw4.jpg)



#### Section 3 - AWS Storage Services

##### (02:09:52) Introduction

![image-20211106003131422](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qrm8nn2j31ox0u0gqk.jpg)



##### (02:10:28) AWS Storage Services Overview





##### (02:12:40) Create an Attach EBS Volume

![image-20211106003217560](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qsc1kdjj31dy0u041x.jpg)

![image-20211106003312225](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qtb8ngqj31lv0u0tdn.jpg)





##### (02:20:47) Instance Store Volumes

![image-20211106003901196](https://tva1.sinaimg.cn/large/008i3skNgy1gw4qzcxy6qj31lf0u043r.jpg)



##### (02:23:34) EBS Snapshots and AMIs

![image-20211106004020374](https://tva1.sinaimg.cn/large/008i3skNgy1gw4r0sf293j31og0u0dkn.jpg)



##### (02:31:38) Create Amazon EFS File System

![image-20211106004315264](https://tva1.sinaimg.cn/large/008i3skNgy1gw4r3qmqscj316m0u0410.jpg)

![image-20211106004621528](https://tva1.sinaimg.cn/large/008i3skNgy1gw4r6ymjhej319a06cmxy.jpg)



##### (02:40:26) Amazon S3 Create Bucket and Make Public

![image-20211106004847150](https://tva1.sinaimg.cn/large/008i3skNgy1gw4r9i6uh7j31gg0u0q7x.jpg)



##### (02:47:32) Working with S3 Objects from the AWS CLI

![image-20211106005353343](https://tva1.sinaimg.cn/large/008i3skNgy1gw4rev1xg0j31rq0f6448.jpg)



#### Section 4 - AWS Databases

##### (02:52:49) Introduction



##### (02:53:28) Amazon RDS Overview

![image-20211106005526534](https://tva1.sinaimg.cn/large/008i3skNgy1gw4rgf4b40j318w0u0mzw.jpg)

![image-20211106005623662](https://tva1.sinaimg.cn/large/008i3skNgy1gw4rhe7ekvj31ms0tu0vn.jpg)



![image-20211106105702275](https://tva1.sinaimg.cn/large/008i3skNly1gw58ubchmij31k20u0afy.jpg)



##### (02:59:52) Create Amazon RDS Multi-AZ

![image-20211106110103698](https://tva1.sinaimg.cn/large/008i3skNgy1gw58yladmfj31hg0u0adm.jpg)



##### (03:10:35) Add an Amazon RDS Read Replica





##### (03:16:18) Install WordPress on EC2 with RDS Database

![image-20211106110852216](https://tva1.sinaimg.cn/large/008i3skNgy1gw596onsszj318u0ewjt4.jpg)

![image-20211106110931750](https://tva1.sinaimg.cn/large/008i3skNgy1gw597d0mkoj31jz0u0wia.jpg)

![image-20211106111016747](https://tva1.sinaimg.cn/large/008i3skNgy1gw5984y00ij31ij0u0tcq.jpg)

![image-20211106111046629](https://tva1.sinaimg.cn/large/008i3skNgy1gw598qppvfj318q0my0uo.jpg)



##### (03:23:56) Amazon DynamoDB

![image-20211106111226439](https://tva1.sinaimg.cn/large/008i3skNgy1gw59ae6tq1j31gi0u0gpd.jpg)

![image-20211106111517815](https://tva1.sinaimg.cn/large/008i3skNgy1gw59dehwruj31pu0ouad6.jpg)

```sh
# Import data
aws dynamodb batch-write-item --request-items file://mystore.json

#### SCANS ####

# Perform scan of ProductOrders table:
aws dynamodb scan --table-name mystore

#### QUERIES ####

# Use Key-Conditions Parameter:
aws dynamodb query  --table-name mystore --key-conditions '{ "clientid":{ "ComparisonOperator":"EQ", "AttributeValueList": [ {"S": "chris@example.com"} ] } }'

# Use Key-Condition-Expression Parameter:
aws dynamodb query --table-name mystore --key-condition-expression "clientid = :name" --expression-attribute-values '{":name":{"S":"chris@example.com"}}'
```



```json
{
    "mystore": [
        {
            "PutRequest": {
                "Item": {
                    "clientid": {
                        "S": "john@example.com"
                    },
                    "created": {
                        "S": "2020-03-9T08:12Z"
                    },
                    "sku": {
                        "S": "SKU-S523"
                    },
                    "category": {
                        "S": "T-Shirt"
                    },
                    "size": {
                        "S": "Small"
                    },
                    "colour": {
                        "S": "Red"
                    },
                    "qty": {
                        "N": "1"
                    },
                    "price": {
                        "N": "30"
                    },
                    "weight": {
                        "S": "Light"
                    } 
                  
                }
            }
        }
      ]
}
```

![image-20211106111906638](https://tva1.sinaimg.cn/large/008i3skNgy1gw59hckv9gj31ic0u0wju.jpg)









#### Section 5 - Automation on AWS

##### (03:35:34) How to Deploy Infrastructure Using AWS CloudFormation

![image-20211106112347200](https://tva1.sinaimg.cn/large/008i3skNgy1gw59m759wej31dt0u0jvt.jpg)



![image-20211106112420030](https://tva1.sinaimg.cn/large/008i3skNgy1gw59ms1orej318c0u0q6b.jpg)



##### (03:37:36) Create Simple Stacks with AWS CloudFormation

```yml
Resources:
  MyInstance:
    Type: AWS::EC2::Instance
    Properties:
      AvailabilityZone: us-east-1a
      ImageId: ami-0a887e401f7654935
      InstanceType: t2.micro
```

- 类似于case

```yaml
Mappings:
  InstanceMap:
    us-east-1:
      dev: t2.micro
      prod: m1.small
    us-west-1: 
      dev: m1.small
      prod: m1.large
  AMIMap:
    us-east-1:
      dev: ami-0a887e401f7654935
      prod: ami-0a887e401f7654935
    us-west-1:
      dev: ami-01c94064639c71719
      prod: ami-01c94064639c71719
Resources:
  MyInstance:
    Type: AWS::EC2::Instance
    Properties:
      ImageId: !FindInMap [AMIMap, !Ref "AWS::Region", dev]
      InstanceType: !FindInMap [InstanceMap,!Ref "AWS::Region", dev]
```

- Value param

```yaml
Parameters: 
  InstanceTypeParameter: 
    Type: String
    Default: t2.micro
    AllowedValues: 
      - t2.micro
      - m1.small
      - m1.large
    Description: Enter t2.micro, m1.small, or m1.large. Default is t2.micro.
Resources:
  MyInstance:
    Type: AWS::EC2::Instance
    Properties:
      AvailabilityZone: us-east-1a
      ImageId: ami-0a887e401f7654935
      InstanceType: 
        Ref: InstanceTypeParameter
```

- S3

```yaml
Resources:
  DigitalCloud:
    Type: AWS::S3::Bucket
    Properties:
      AccessControl: PublicRead
      WebsiteConfiguration:
        IndexDocument: index.html
        ErrorDocument: error.html
```



##### (03:47:52) Create Complex Stack with AWS CloudFormation

![image-20211106113141509](https://tva1.sinaimg.cn/large/008i3skNgy1gw59ugptomj31gu0u00yf.jpg)



##### (03:56:45) Deploy an Application Using AWS Elastic Beanstalk

![image-20211106121918411](https://tva1.sinaimg.cn/large/008i3skNgy1gw5b80gztqj31k40u0wje.jpg)



#### Section 6 - DevOps on AWS

##### (04:02:00) Introduction

##### (04:02:43) Continuous Integration and Continuous Delivery (CI/CD)

![image-20211106122728442](https://tva1.sinaimg.cn/large/008i3skNgy1gw5bghehy4j31kt0u0whm.jpg)

![image-20211106122814672](https://tva1.sinaimg.cn/large/008i3skNgy1gw5bh9e1ufj31qj0u0q7s.jpg)

![image-20211106122845639](https://tva1.sinaimg.cn/large/008i3skNgy1gw5bhtdkqjj31c20u0adf.jpg)



##### (04:05:55) AWS CodePipeline with AWS Elastic Beanstalk





##### (04:15:26) Create AWS CodeStar Project

![image-20211106123143385](https://tva1.sinaimg.cn/large/008i3skNgy1gw5bkxgajpj31ay0u0acr.jpg)



#### Section 7 - DNS Services and Content Delivery

##### (04:25:47) Introduction

##### (04:26:23) Amazon Route 53 Overview and Routing Policies

![image-20211106124540695](https://tva1.sinaimg.cn/large/008i3skNgy1gw5bzf0v5jj31n20r877g.jpg)



![image-20211106124608326](https://tva1.sinaimg.cn/large/008i3skNgy1gw5bzwv1xfj31js0u0gq4.jpg)

![image-20211106124634284](https://tva1.sinaimg.cn/large/008i3skNgy1gw5c0dgo1ej31ls0u0wij.jpg)

![image-20211106124643201](https://tva1.sinaimg.cn/large/008i3skNgy1gw5c0inx9lj31u00si44d.jpg)



##### (04:30:22) Register Domain Using Route 53

![image-20211106124839221](https://tva1.sinaimg.cn/large/008i3skNgy1gw5c2irfq5j31sg0u0gqx.jpg)



##### (04:35:44) Create Amazon CloudFront Distribution with S3 Static Website

![image-20211106125216539](https://tva1.sinaimg.cn/large/008i3skNgy1gw5c6bcteuj31kd0u0gpt.jpg)

![image-20211106125455156](https://tva1.sinaimg.cn/large/008i3skNgy1gw5c9136rmj32380qkgpn.jpg)



##### (04:44:30) Add an SSL/TLS Certificate and Route 53 Alias Record



#### Section 8 - Docker Containers and Serverless Computing

##### (04:53:35) Introduction



##### (04:54:34) Docker Containers on Amazon ECS

![image-20211106131037066](https://tva1.sinaimg.cn/large/008i3skNgy1gw5cpe67vkj31jr0u0jwm.jpg)

![image-20211106131927456](https://tva1.sinaimg.cn/large/008i3skNgy1gw5cyk7za9j31fr0u0n0i.jpg)

![image-20211106132259009](https://tva1.sinaimg.cn/large/008i3skNgy1gw5d292yw6j31dm0u077q.jpg)



##### (05:06:06) Serverless with AWS Lambda

![image-20211106132439752](https://tva1.sinaimg.cn/large/008i3skNgy1gw5d3ztpeuj31le0rgq5l.jpg)

![image-20211106132501668](https://tva1.sinaimg.cn/large/008i3skNgy1gw5d4chiy9j31s20togof.jpg)

![image-20211106132716361](https://tva1.sinaimg.cn/large/008i3skNgy1gw5d6ppmy9j31iw0u0teb.jpg)

![image-20211106132741208](https://tva1.sinaimg.cn/large/008i3skNgy1gw5d74n4s9j31qy0u0dm6.jpg)



#### Section 9 - Application Integration and Loose Coupling

##### (05:13:12) Introduction

##### (05:14:01) Amazon SNS and Amazon SQS

![image-20211106132840189](https://tva1.sinaimg.cn/large/008i3skNgy1gw5d85bq30j31cg0u0wgw.jpg)

![image-20211106132946956](https://tva1.sinaimg.cn/large/008i3skNgy1gw5d9cz1q2j31ds0u0jv5.jpg)



##### (05:16:32) AWS Lambda to Amazon SQS Event Source Mapping

![image-20211106133047928](https://tva1.sinaimg.cn/large/008i3skNgy1gw5dadgxljj31kz0u041r.jpg)



##### (05:22:49) Serverless Application - Amazon SQS, SNS, and Lambda

![image-20211106133453338](https://tva1.sinaimg.cn/large/008i3skNgy1gw5demx1vpj31560u0mzd.jpg)





---

- https://www.youtube.com/watch?v=ulprqHHWlng&t=238s