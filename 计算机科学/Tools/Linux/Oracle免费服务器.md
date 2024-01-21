- ssh -i /Users/chengkun02/.ssh/id_rsa opc@140.83.36.74

```sh
Last login: Mon Nov  8 23:27:25 on ttys000
/Users/chengkun02/.zprofile:2: no such file or directory: /opt/homebrew/bin/brew
chengkun02@B000000407241Y ~ % su
Password:
sh-3.2# ssh 140.83.36.74
ssh: connect to host 140.83.36.74 port 22: Connection refused
sh-3.2# ssh 140.83.36.74
The authenticity of host '140.83.36.74 (140.83.36.74)' can't be established.
ECDSA key fingerprint is SHA256:hxXFNya7UMf0YUX4/qpCdlmJQTxyZoE6aTuU3uCYzgw.
Are you sure you want to continue connecting (yes/no/[fingerprint])? 
Host key verification failed.
sh-3.2# ssh 140.83.36.74
The authenticity of host '140.83.36.74 (140.83.36.74)' can't be established.
ECDSA key fingerprint is SHA256:hxXFNya7UMf0YUX4/qpCdlmJQTxyZoE6aTuU3uCYzgw.
Are you sure you want to continue connecting (yes/no/[fingerprint])? yes
Warning: Permanently added '140.83.36.74' (ECDSA) to the list of known hosts.
root@140.83.36.74: Permission denied (publickey,gssapi-keyex,gssapi-with-mic).
sh-3.2# ssh 140.83.36.74
root@140.83.36.74: Permission denied (publickey,gssapi-keyex,gssapi-with-mic).
sh-3.2# ssh 140.83.36.74
root@140.83.36.74: Permission denied (publickey,gssapi-keyex,gssapi-with-mic).
sh-3.2# ssh 140.83.36.74 opc
root@140.83.36.74: Permission denied (publickey,gssapi-keyex,gssapi-with-mic).
sh-3.2# ssh 140.83.36.74 opc
root@140.83.36.74: Permission denied (publickey,gssapi-keyex,gssapi-with-mic).
sh-3.2# ssh 140.83.36.74
root@140.83.36.74: Permission denied (publickey,gssapi-keyex,gssapi-with-mic).
sh-3.2# ping 140.83.36.74
PING 140.83.36.74 (140.83.36.74): 56 data bytes
64 bytes from 140.83.36.74: icmp_seq=0 ttl=44 time=288.775 ms
64 bytes from 140.83.36.74: icmp_seq=1 ttl=44 time=237.928 ms
Request timeout for icmp_seq 2
64 bytes from 140.83.36.74: icmp_seq=3 ttl=44 time=103.890 ms
64 bytes from 140.83.36.74: icmp_seq=4 ttl=44 time=101.976 ms
64 bytes from 140.83.36.74: icmp_seq=5 ttl=44 time=95.778 ms
64 bytes from 140.83.36.74: icmp_seq=6 ttl=44 time=103.297 ms
^C
--- 140.83.36.74 ping statistics ---
7 packets transmitted, 6 packets received, 14.3% packet loss
round-trip min/avg/max/stddev = 95.778/155.274/288.775/77.864 ms
sh-3.2# ssh opc@140.83.36.74
opc@140.83.36.74: Permission denied (publickey,gssapi-keyex,gssapi-with-mic).
sh-3.2# ssh -i /Users/chengkun02/.ssh/id_rsa opc@140.83.36.74



-bash: warning: setlocale: LC_CTYPE: cannot change locale (UTF-8): No such file or directory
[opc@instance-20211109-1219 ~]$ 
[opc@instance-20211109-1219 ~]$ 
[opc@instance-20211109-1219 ~]$ 
[opc@instance-20211109-1219 ~]$ 
[opc@instance-20211109-1219 ~]$ ls
[opc@instance-20211109-1219 ~]$ sudo -isudo -i
sudo: you may not specify both the `-i' and `-s' options
usage: sudo -h | -K | -k | -V
usage: sudo -v [-AknS] [-g group] [-h host] [-p prompt] [-u user]
usage: sudo -l [-AknS] [-g group] [-h host] [-p prompt] [-U user] [-u user] [command]
usage: sudo [-AbEHknPS] [-r role] [-t type] [-C num] [-g group] [-h host] [-p prompt] [-T timeout] [-u user] [VAR=value] [-i|-s] [<command>]
usage: sudo -e [-AknS] [-r role] [-t type] [-C num] [-g group] [-h host] [-p prompt] [-T timeout] [-u user] file ...
[opc@instance-20211109-1219 ~]$ sudo -i
[root@instance-20211109-1219 ~]# passwd
Changing password for user root.
New password: 
Retype new password: 
Sorry, passwords do not match.
New password: 
BAD PASSWORD: The password is shorter than 7 characters
Retype new password: 
[root@instance-20211109-1219 ~]# passwd
Changing password for user root.
New password: 
Retype new password: 
passwd: all authentication tokens updated successfully.
[root@instance-20211109-1219 ~]# sed -i 's/PermitRootLogin no/PermitRootLogin yes/g' /etc/ssh/sshd_config
[root@instance-20211109-1219 ~]# 
[root@instance-20211109-1219 ~]# sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config
[root@instance-20211109-1219 ~]# 
[root@instance-20211109-1219 ~]# sed -i 's/PasswordAuthentication no/PasswordAuthentication yes/g' /etc/ssh/sshd_config
[root@instance-20211109-1219 ~]# 
[root@instance-20211109-1219 ~]# service ssh restart
Redirecting to /bin/systemctl restart ssh.service
Failed to restart ssh.service: Unit not found.
[root@instance-20211109-1219 ~]# systemctl restart sshd.serviceservice ssh restart
Failed to restart sshd.serviceservice.service: Unit not found.
Failed to restart ssh.service: Unit not found.
Failed to restart restart.service: Unit not found.
[root@instance-20211109-1219 ~]# systemctl restart sshd.serviceservice ssh restart
Failed to restart sshd.serviceservice.service: Unit not found.
Failed to restart ssh.service: Unit not found.
Failed to restart restart.service: Unit not found.
[root@instance-20211109-1219 ~]# systemctl restart sshd.service
```



```sh
安装相关依赖
CentOS如下
yum -y install wget
yum update -y && yum install curl -y
```



```v
wget https://git.io/vpnsetup -O vpnsetup.sh && sudo \
VPN_IPSEC_PSK='' \
VPN_USER='' \
VPN_PASSWORD='' sh vpnsetup.sh
```

```
================================================

IPsec VPN server is now ready for use!

Connect to your new VPN with these details:

Server IP: 
IPsec PSK: 
Username: 
Password: 

Write these down. You'll need them to connect!

Important notes:   https://git.io/vpnnotes
Setup VPN clients: https://git.io/vpnclients
IKEv2 guide:       https://git.io/ikev2

================================================
```









- https://zhuanlan.zhihu.com/p/352736372



























- id_rsa

```c
-----BEGIN OPENSSH PRIVATE KEY-----
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
-----END OPENSSH PRIVATE KEY-----
```

