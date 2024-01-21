

- Multipass虚拟机

```c
Welcome to Ubuntu 20.04.3 LTS (GNU/Linux 5.4.0-81-generic x86_64)

 * Documentation:  https://help.ubuntu.com
 * Management:     https://landscape.canonical.com
 * Support:        https://ubuntu.com/advantage

  System information as of Fri Oct 29 14:12:08 CST 2021

  System load:             0.0
  Usage of /:              14.3% of 193.66GB
  Memory usage:            20%
  Swap usage:              0%
  Processes:               114
  Users logged in:         1
  IPv4 address for enp0s2: 192.168.64.3
  IPv6 address for enp0s2: fd88:43d9:4226:347e:742f:26ff:fee8:2ee7


28 updates can be applied immediately.
To see these additional updates run: apt list --upgradable


The list of available updates is more than a week old.
To check for new updates run: sudo apt update

Last login: Fri Oct 29 13:40:21 2021 from 192.168.64.1
ubuntu@satisfying-fowl:~$ ifconfig
enp0s2: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet 192.168.64.3  netmask 255.255.255.0  broadcast 192.168.64.255
        inet6 fe80::742f:26ff:fee8:2ee7  prefixlen 64  scopeid 0x20<link>
        inet6 fd88:43d9:4226:347e:742f:26ff:fee8:2ee7  prefixlen 64  scopeid 0x0<global>
        ether 76:2f:26:e8:2e:e7  txqueuelen 1000  (Ethernet)
        RX packets 7046  bytes 9409078 (9.4 MB)
        RX errors 0  dropped 0  overruns 0  frame 0
        TX packets 1647  bytes 158410 (158.4 KB)
        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0

lo: flags=73<UP,LOOPBACK,RUNNING>  mtu 65536
        inet 127.0.0.1  netmask 255.0.0.0
        inet6 ::1  prefixlen 128  scopeid 0x10<host>
        loop  txqueuelen 1000  (Local Loopback)
        RX packets 112  bytes 9148 (9.1 KB)
        RX errors 0  dropped 0  overruns 0  frame 0
        TX packets 112  bytes 9148 (9.1 KB)
        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
```

-  inet 192.168.64.3
- host机器可以ping的通



- systemctl start sshd.service 或者 service sshd start启动服务器的ssh服务



```sh
ubuntu@satisfying-fowl:~$ sudo apt-get install openssh-server
Reading package lists... Done
Building dependency tree       
Reading state information... Done
openssh-server is already the newest version (1:8.2p1-4ubuntu0.3).
0 upgraded, 0 newly installed, 0 to remove and 41 not upgraded.
```



```
CentOS：
 
开启远程连接服务：service sshd start
添加到系统启动项：chkconfig sshd on
客户端工具：windows下连接工具putty
 
=============================================
 
Ubuntu：
 
安装命令：$ sudo apt-get install openssh-server
查看openssh-server是否启动
$ ps -e | grep ssh
进程ssh-agent是客户端，sshd为服务器端，如果结果中有sshd的进程说明openssh-server已经启动，如果没有则需运行命令启动。
启动、停止和重启openssh-server的命令如下
/etc/init.d/ssh start
/etc/init.d/ssh stop
/etc/init.d/ssh restart
 
配置openssh-server
 
openssh-server配置文件位于/etc/ssh/sshd_config，在这里可以配置SSH的服务端口等，例如：默认端口是22，可以自定义为其他端口号，如222，然后需要重启SSH服务。
Ubuntu中配置openssh-server开机自动启动
打开/etc/rc.local文件，在exit 0语句前加入：
/etc/init.d/ssh start
关于客户端连接
客户端可以用putty、SecureCRT、SSH Secure Shell Client等SSH 客户端软件，输入您服务器的IP地址，并且输入登录的用户和密码就可以登录了。我常选择的客户端软件是putty。

```



![image-20211029143100913](https://tva1.sinaimg.cn/large/008i3skNly1gvw62i3l63j313e0amacr.jpg)

- https://www.cnblogs.com/zhoading/p/11526908.html
- https://www.cnblogs.com/dspace/p/6147928.html



```c
The authenticity of host '192.168.64.3 (192.168.64.3)' can't be established.
ECDSA key fingerprint is SHA256:k3.
Are you sure you want to continue connecting (yes/no/[fingerprint])? yes
Warning: Permanently added '192.168.64.3' (ECDSA) to the list of known hosts.
root@192.168.64.3: Permission denied (publickey).
```

