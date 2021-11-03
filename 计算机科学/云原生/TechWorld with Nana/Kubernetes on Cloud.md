[TOC]

https://www.youtube.com/watch?v=p6xDCz00TxU&list=PLy7NrYWoggjxqLwqmbE-gGuxpo0nWZqCi&index=3

- 1和2在Kubernetes的教程里有了Linode赞助的那两节

![image-20211027001244000](https://tva1.sinaimg.cn/large/008i3skNly1gvt60uf6xlj31pi0u0tju.jpg)



### Managed Kubernetes Cluster explained | Kubernetes on Cloud (1/2)

### Step by Step Application Deployment on LKE using Helm | Kubernetes on Cloud (2/2)

- 1和2在Kubernetes的教程里有了Linode赞助的那两节



---

### AWS EKS - Create Kubernetes cluster on Amazon EKS | the easy way

```c
--------  How to use EKS? -------- 
To create a K8s cluster in EKS you need to do following steps:
1) Setup or preparation steps
 - create AWS account
 - create a VPC - virtual private space
 - create an IAM role with Security Group (or in other words: create AWS user with list of permissions)
2) Create Cluster Control Plane - Master Nodes
 - choose basic information like cluster name and k8s version
 - choose region and VPC for your cluster
 - set security
3) Create Worker Nodes and connect to cluster
The Worker Nodes are some EC2 instances with CPU and storage resources.
 - Create as a Node Group
 - Choose cluster it will attach to
 - Define Security Group, select instance type etc.

With NodeGroup you have autoscaling, which means based on your needs depending on how much load the cluster has new Worker Nodes will automatically added or removed in the cluster.

 - For that you need to define max and minimum number of Nodes.

--------  Complex, but powerful and popular -------- 
You're right, that's a lot of effort for just creating a simple Kubernetes cluster. Compared to other managed Kubernetes services, like DigitalOcean or Linode it's more complex.

So, how to do it, when you just want to create a cluster and start deploying your containers inside as fast as possible. 

Instead of doing all those steps manually, there is a faster and more efficient way.

--------  eksctl to the rescue  -------- 
eksctl is a simple CLI tool for creating clusters on EKS. With just one command you create a cluster in minutes.
I'm a huge fan of understanding the concepts of how a technology works, but if there are tools that make working with this technology much easier then I like to use them. Usually those tools are built by community, which specialized in this technology. So it makes sense to use this knowledge.

So in the demo we will create the Kubernetes cluster using eksctl.

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:36 - What is EKS?
1:20 - How to use EKS? 3 steps
4:46 - eksctl
7:22 - Demo: Create K8s cluster on AWS EKS using eksctl
15:04 - Demo: Delete K8s cluster and all its resources
16:02 - Recap
```









----

### Containers on AWS Overview: ECS | EKS | Fargate | ECR

```c
Overview of container services on AWS | AWS ECS Tutorial | AWS EKS Tutorial | AWS Fargate | ECR
► Part of the DevOps Bootcamp 🚀  More infos here: https://www.techworld-with-nana.com/d...

In this video I will give you a high-level overview of all the different container services you can use on AWS. If you want to run a containerized application on AWS you have multiple options, depending on your application requirements.

We will see what Elastic Container Service (ECS) is, what it's used for and how it works.
Then we will compare it and talk about EKS, which is Elastic Kubernetes Service.
We will also see different ways of running containers with EC2 or AWS Fargate.

And finally we will see the ECR Service, which stands for Elastic Container Registry.

#ecs #eks #aws #techworldwithnana


▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:48 - Why Container Orchestration?
3:00 - Container Orchestration Tools
3:28 - What is ECS - Elastic Container Service?
4:04 - How does ECS work?
5:59 - ECS with EC2 instances
7:34 - ECS with AWS Fargate
11:48 - Integration with other AWS Services
12:26 - What is EKS - Elastic Kubernetes Service?
13:20 - EKS vs ECS
16:36 - How does EKS work?
19:28 - Worker Nodes options: EC2 vs Nodegroup vs Fargate
22:33 - What is ECR - Elastic Container Registry?
24:25 - Summary
24:49 - CI/CD Pipeline we will build (DevOps Bootcamp)
```



