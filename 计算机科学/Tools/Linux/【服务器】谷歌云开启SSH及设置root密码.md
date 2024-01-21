

```c
# 切换为 root 用户
sudo -i

# 更改 root 用户的密码
passwd

# 允许 ssh 密码登录
sed -i 's/PermitRootLogin no/PermitRootLogin yes/g' /etc/ssh/sshd_config

sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config

sed -i 's/PasswordAuthentication no/PasswordAuthentication yes/g' /etc/ssh/sshd_config

# 重启 ssh 服务
service ssh restart
systemctl restart sshd.service
```



登录进服务器后，切换root用户

sudo -i
为root用户设置密码，输入两次密码即可

passwd
然后修改sshd配置文件

vim /etc/ssh/sshd_config
将如下两个参数的值修改为yes

PermitRootLogin yes 
PasswordAuthentication yes
然后重启sshd服务即可

systemctl restart sshd.service

```sh
ubuntu@satisfying-fowl:~$ su -
Password: 
root@satisfying-fowl:~# vi /etc/ssh/sshd_config
root@satisfying-fowl:~# ssh localhost
The authenticity of host 'localhost (127.0.0.1)' can't be established.
ECDSA key fingerprint is SHA256:k3dZyrYNYqNfwrAU1VGgwwfnpvdlhq+XPICAEZZc2us.
Are you sure you want to continue connecting (yes/no/[fingerprint])? yes
Warning: Permanently added 'localhost' (ECDSA) to the list of known hosts.
root@localhost's password: 
Welcome to Ubuntu 20.04.3 LTS (GNU/Linux 5.4.0-81-generic x86_64)

 * Documentation:  https://help.ubuntu.com
 * Management:     https://landscape.canonical.com
 * Support:        https://ubuntu.com/advantage

  System information as of Fri Oct 29 15:53:46 CST 2021

  System load:                      0.24
  Usage of /:                       15.2% of 193.66GB
  Memory usage:                     25%
  Swap usage:                       0%
  Processes:                        131
  Users logged in:                  1
  IPv4 address for br-bde94744d8a4: 172.18.0.1
  IPv4 address for docker0:         172.17.0.1
  IPv4 address for enp0s2:          192.168.64.3
  IPv6 address for enp0s2:          fd75:3638:5ba5:c518:742f:26ff:fee8:2ee7

 * Super-optimized for small spaces - read how we shrank the memory
   footprint of MicroK8s to make it the smallest full K8s around.

   https://ubuntu.com/blog/microk8s-memory-optimisation

41 updates can be applied immediately.
13 of these updates are standard security updates.
To see these additional updates run: apt list --upgradable



The programs included with the Ubuntu system are free software;
the exact distribution terms for each program are described in the
individual files in /usr/share/doc/*/copyright.

Ubuntu comes with ABSOLUTELY NO WARRANTY, to the extent permitted by
applicable law.

root@satisfying-fowl:~# 
```



sudo apt-get install openssh-server





# **host，本地机器也需要在su模式**

```sh

sh-3.2# ssh 192.168.64.3
root@192.168.64.3's password:
Welcome to Ubuntu 20.04.3 LTS (GNU/Linux 5.4.0-81-generic x86_64)

 * Documentation:  https://help.ubuntu.com
 * Management:     https://landscape.canonical.com
 * Support:        https://ubuntu.com/advantage

  System information as of Fri Oct 29 15:56:20 CST 2021

  System load:                      0.12
  Usage of /:                       15.2% of 193.66GB
  Memory usage:                     26%
  Swap usage:                       0%
  Processes:                        133
  Users logged in:                  2
  IPv4 address for br-bde94744d8a4: 172.18.0.1
  IPv4 address for docker0:         172.17.0.1
  IPv4 address for enp0s2:          192.168.64.3
  IPv6 address for enp0s2:          fd75:3638:5ba5:c518:742f:26ff:fee8:2ee7

 * Super-optimized for small spaces - read how we shrank the memory
   footprint of MicroK8s to make it the smallest full K8s around.

   https://ubuntu.com/blog/microk8s-memory-optimisation

41 updates can be applied immediately.
13 of these updates are standard security updates.
To see these additional updates run: apt list --upgradable


Last login: Fri Oct 29 15:53:48 2021 from 127.0.0.1
root@satisfying-fowl:~#
```



![image-20211029155933921](https://tva1.sinaimg.cn/large/008i3skNly1gvw8mmwfaoj31g40u0qbd.jpg)