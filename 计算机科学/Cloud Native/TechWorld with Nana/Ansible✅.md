[TOC]

### What is Ansible | Ansible Playbook explained | Ansible Tutorial for Beginners

```c
Ansible is an automation tool for IT tasks. It is popular and widely accepted, because 1) it's usage of simple YAML language and 2) support for all types of infrastructure starting from Clouds to Virtual to bare metal servers.

In this video I explain examples of various IT tasks and how Ansible helps automate them to make daily DevOps tasks more efficient and less time consuming. 🙌🏼

I explain HOW Ansible does all this by going through its architecture:
 👉🏼 Ansible Modules (small programs that get executed on target machines)
 👉🏼 Ansible Playbook (instructions on HOW these programs get executed)
 👉🏼 Ansible Inventory (list of hosts, WHERE those programs gets executed)

In the end I also show how Ansible makes working with Docker more efficient and powerful and how it compares to similar automation tools like Puppet and Chef.

T I M E S T A M P S
0:00 - Start
0:26 - What is Ansible?
0:45 - Why use Ansible?
4:13 - Ansible is agentless
5:20 - Ansible Modules explained
7:00 - YAML Syntax
7:48 - Ansible Playbook explained
11:50 - Ansible Inventory
12:47 - Ansible for Docker
14:40 - Ansible Tower
15:05 - Ansible vs Puppet and Chef
  
  https://docs.ansible.com/ansible/latest/collections/index.html
```

![image-20211103152424245](https://tva1.sinaimg.cn/large/008i3skNly1gw1zplcvgqj31fv0u076l.jpg)

- 两个问题
  - Ansible是什么，它能自动执行IT Tasks
  - 为什么自动化是好的捏

![image-20211103152448236](https://tva1.sinaimg.cn/large/008i3skNly1gw1zq04y5dj31bi0u00vd.jpg)



- 想想一个场景，有10台云服务器，想要在10台服务器上部署从应用1.0到2.0，但是需要升级服务器上的docker环境，就得一个一个手动操作

![image-20211103152609275](https://tva1.sinaimg.cn/large/008i3skNly1gw1zrer2eij31br0u077d.jpg)



- 一些升级、备份、创建用户、同步等任务

![image-20211103152702395](https://tva1.sinaimg.cn/large/008i3skNly1gw1zsc0ylhj312u0m4ta4.jpg)



- 一些好处

![image-20211103152848829](https://tva1.sinaimg.cn/large/008i3skNly1gw1zu6n86zj31l50u0n0w.jpg)



- Ansible是无代理的

![image-20211103152914459](https://tva1.sinaimg.cn/large/008i3skNly1gw1zumbkmkj31a10u0goc.jpg)

- Ansible不需要再服务器上安装Ansible的代理
  - 远程管理

![image-20211103152945129](https://tva1.sinaimg.cn/large/008i3skNly1gw1zv5fch2j31860u0acg.jpg)



- Ansible的架构
  - 由一些小Modules做实际性的工作

![image-20211103153044359](https://tva1.sinaimg.cn/large/008i3skNly1gw1zw6wh4vj315f0u0dir.jpg)



![image-20211103153125652](https://tva1.sinaimg.cn/large/008i3skNly1gw1zwwijy1j31e60u0q5r.jpg)



- 官方文档里有很多module

![image-20211103153137301](https://tva1.sinaimg.cn/large/008i3skNly1gw1zx3egvtj31gn0u0wj8.jpg)



- 一个jenkins module 的例子

![image-20211103153220155](https://tva1.sinaimg.cn/large/008i3skNly1gw1zxu8qsbj31gh0u0n17.jpg)



- 一个docker module的例子

![image-20211103153242143](https://tva1.sinaimg.cn/large/008i3skNly1gw1zy8byc5j31gi0u0jvv.jpg)



- Modules是很颗粒的；粒状的，所以就需要有一个东西来管理Module，这时候playbooks登场了

![image-20211103153312976](https://tva1.sinaimg.cn/large/008i3skNly1gw1zyrh93rj31gl0u077l.jpg)



- task

![image-20211103153418899](https://tva1.sinaimg.cn/large/008i3skNly1gw1zzwkyxmj318c0u077p.jpg)

![image-20211103153429911](https://tva1.sinaimg.cn/large/008i3skNly1gw200344qtj318f0u0tbo.jpg)



- 一个task是一个configuration

![image-20211103153518029](https://tva1.sinaimg.cn/large/008i3skNly1gw200xalwuj31940u0786.jpg)



- 在哪里，谁执行呢？

![image-20211103153602508](https://tva1.sinaimg.cn/large/008i3skNly1gw201pn4rvj31fo0u0tcv.jpg)



![image-20211103153632178](https://tva1.sinaimg.cn/large/008i3skNly1gw2027f8bsj31e90u0dks.jpg)



- 这些东西统称play

![image-20211103153652348](https://tva1.sinaimg.cn/large/008i3skNly1gw202k0venj31f70u0gp8.jpg)



- 所以什么事playbook

![image-20211103153728903](https://tva1.sinaimg.cn/large/008i3skNly1gw20373wj0j31bt0u0q7f.jpg)



- 这些hose哪里来的呢

![image-20211103153804956](https://tva1.sinaimg.cn/large/008i3skNly1gw203tp9mlj31av0u0td3.jpg)



![image-20211103153827964](https://tva1.sinaimg.cn/large/008i3skNly1gw2047wagqj31ks0u0ae3.jpg)



- 还可以分组

![image-20211103153857325](https://tva1.sinaimg.cn/large/008i3skNly1gw204qmy2oj31hz0u00w1.jpg)



- 不只是Docker容器，
- Ansibel可以把资源打包成容器、也可以部署到纯物理机，也可以放到云服务器……

![image-20211103153947037](https://tva1.sinaimg.cn/large/008i3skNly1gw205l38e4j31jh0u00x2.jpg)



- Ansible能够同时管理host，例如存储和网络

![image-20211103154014087](https://tva1.sinaimg.cn/large/008i3skNly1gw2061xe1bj31de0u041d.jpg)



![image-20211103154140491](https://tva1.sinaimg.cn/large/008i3skNly1gw207jygbqj31gt0u0783.jpg)



![image-20211103154205562](https://tva1.sinaimg.cn/large/008i3skNly1gw207zv2w0j31ry0tcjue.jpg)



![image-20211103154219706](https://tva1.sinaimg.cn/large/008i3skNly1gw2088j0voj31di0u0q5d.jpg)



