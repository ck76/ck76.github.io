[TOC]

https://www.youtube.com/watch?v=1id6ERvfozo&list=PLy7NrYWoggjzmPAyfDQgthSgx6VEVlS0-

![image-20211027001939479](https://tva1.sinaimg.cn/large/008i3skNly1gvt681k576j31pz0u0dlh.jpg)



### What is Infrastructure as Code? Difference of Infrastructure as Code Tools

```c
Infrastructure as Code explained | Difference of Infrastructure as Code Tools, like Terraform, Ansible or Puppet
Terraform in detail         ►  https://youtu.be/l5k1ai_GBDE
Ansible in detail              ►  https://youtu.be/1id6ERvfozo

To understand the Infrastructure as Code concept better, I explain how DevOps tasks were done
 - before automation and
 - after automation.

Infrastructure as Code is a way to automate all these DevOps tasks end to end instead of doing it manually. All the knowledge and expertise of system administrators or DevOps engineers are packed into programs and applications that carry out those tasks.

So, Infrastructure as Code or IaC is a concept and there are Infrastructure as Code tools, like Ansible, Puppet, Terraform or Cloudformation etc  that you can use for different tasks.

Why do we have so many different tools, can't we just use one IaC tool? 🙄
Well, no. Because no tool can do everything and each one is good in a specific area. IaC tools automate tasks in different categories for different phases:

3 main task categories:
 1) infrastructure provisioning
 2) configuration of provisioned infrastructure
 3) deployment of application

Distinction of phases:
 1) initial setup phase
 2) maintaining phase

In most cases you would use a combination of 2 or more IaC tools.

#infrastructureascode #iac #devops #techworldwithnana

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
0:33 - DevOps tasks BEFORE automation
1:52 - DevOps tasks AFTER automation
2:07 - What is Infrastructure as Code - IaC 
3:05 - 3 main task categories
4:38 - Distinction of 2 phases - initial setup and maintaining
5:17 - Difference of IaC tools
6:30 - Difference of IaC tools in HOW they work
```

![image-20211103150932756](https://tva1.sinaimg.cn/large/008i3skNly1gw1za4kwt3j31go0u078a.jpg)



- 工具链

![image-20211103150953831](https://tva1.sinaimg.cn/large/008i3skNly1gw1zaho1hij31au0u0q5q.jpg)



![image-20211103151048023](https://tva1.sinaimg.cn/large/008i3skNly1gw1zbfytwaj31i80u0ad4.jpg)



- 都需要手动做

![image-20211103151117622](https://tva1.sinaimg.cn/large/008i3skNly1gw1zby7amtj31ko0u077f.jpg)



![image-20211103151442787](https://tva1.sinaimg.cn/large/008i3skNly1gw1zfih4brj31e10u0dj2.jpg)

- IaC可以理解为一些端到端的自动化任务

![image-20211103151553785](https://tva1.sinaimg.cn/large/008i3skNly1gw1zgqaqsrj31fh0u0tby.jpg)



- 很多工具可以执行这些任务
- ![image-20211103151626083](https://tva1.sinaimg.cn/large/008i3skNly1gw1zhakxe7j31f00u0jv1.jpg)



- 没有全能工具啊

![image-20211103151718024](https://tva1.sinaimg.cn/large/008i3skNly1gw1zi6wk4hj31ij0u0gp1.jpg)



- infrastructure provisioning

![image-20211103151754415](https://tva1.sinaimg.cn/large/008i3skNly1gw1zitqdbnj31q70u0gp7.jpg)

- 配置infrastructure

![image-20211103151826485](https://tva1.sinaimg.cn/large/008i3skNly1gw1zjdudruj31p10u0n1c.jpg)

- 第三部，部署

![image-20211103151850443](https://tva1.sinaimg.cn/large/008i3skNly1gw1zjst0qyj31ke0pagp4.jpg)



- 有了Dokcer之后，2和3可以合并
  - 所以只需要在第一步的基础上，配置Docker的运行时，然后就可以2-3合一了

![image-20211103151917204](https://tva1.sinaimg.cn/large/008i3skNly1gw1zk965n1j31b90u0go2.jpg)



- 关键phases
- ![image-20211103152042737](https://tva1.sinaimg.cn/large/008i3skNly1gw1zlqntfqj31l00u00wi.jpg)

- Terraform

![image-20211103152112438](https://tva1.sinaimg.cn/large/008i3skNly1gw1zm9bbfwj31ir0u0783.jpg)

- Ansible

![image-20211103152131558](https://tva1.sinaimg.cn/large/008i3skNly1gw1zml58fcj31i80u0n0k.jpg)

- 大体上用这两个

![image-20211103152150543](https://tva1.sinaimg.cn/large/008i3skNly1gw1zmxapi4j31l70u0td5.jpg)



- 结果声明-达到这样的结果状态即为正确

![image-20211103152212227](https://tva1.sinaimg.cn/large/008i3skNly1gw1znawz1tj31jj0u0djd.jpg)

- 可变的和不可变的
  - 可变就改变，不可变就替换

![image-20211103152320908](https://tva1.sinaimg.cn/large/008i3skNly1gw1zohczszj31dy0rs76x.jpg)



![image-20211103152347987](https://tva1.sinaimg.cn/large/008i3skNly1gw1zoydy72j31nc0u0dk3.jpg)

