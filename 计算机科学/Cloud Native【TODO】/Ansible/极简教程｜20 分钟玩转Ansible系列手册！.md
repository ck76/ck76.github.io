[TOC]

# [极简教程｜20 分钟玩转Ansible系列手册！](https://segmentfault.com/a/1190000038230424)

## **一、基本部署**

### 安装Ansible

```apache
# yum -y install epel-release
# yum list all *ansible*
# yum info ansible
# yum -y install ansible
```

### Ansible配置文件

```awk
/etc/ansible/ansible.cfg    主配置文件
/etc/ansible/hosts          Inventory
/usr/bin/ansible-doc        帮助文件
/usr/bin/ansible-playbook   指定运行任务文件
```

### 定义Inventory

```accesslog
# cd /etc/ansible/
# cp hosts{,.bak}
# > hosts
# cat hosts
[webserver]
127.0.0.1
192.168.10.149
[dbserver]
192.168.10.113
```

### 使用秘钥方式连接

```gradle
# ssh-keygen -t rsa
# ssh-copy-id -i /root/.ssh/id_rsa.pub root@192.168.10.149
# ssh-copy-id -i /root/.ssh/id_rsa.pub root@192.168.10.113
# ssh-copy-id -i /root/.ssh/id_rsa.pub root@127.0.0.1
```

### 使用帮助

```1c
# ansible-doc -l                列出ansible所有的模块
# ansible-doc -s MODULE_NAME    查看指定模块具体适用
```

### Ansible命令应用基础

```vim
语法：ansible <host-pattern> [-f forks] [-m module_name] [-a args]
<host-pattern>  这次命令对哪些主机生效的
   inventory group name
   ip
   all
-f forks        一次处理多少个主机
-m module_name  要使用的模块
-a args         模块特有的参数
# ansible 192.168.10.113 -m command -a 'date'
# ansible webserver -m command -a 'date'
# ansible all -m command -a 'date'
```

## **二、常见模块**

```vim
command     命令模块(默认模块)用于在远程主机执行命令；不能使用变量，管道等
   # ansible all -a 'date'
cron        计划任务    
   month   指定月份
   minute  指定分钟
   job     指定任务
   day     表示那一天
   hour    指定小时
   weekday 表示周几
   state   表示是添加还是删除
       present：安装
       absent：移除
   # ansible webserver -m cron -a 'minute="*/10" job="/bin/echo hello" name="test cron job"'   #不写默认都是*，每个任务都必须有一个名字
   # ansible webserver -a 'crontab -l'
   # ansible webserver -m cron -a 'minute="*/10" job="/bin/echo hello" name="test cron job" state=absent'  #移除任务
user    用户账号管理
   name    用户名
   uid     uid
   state   状态  
   group   属于哪个组
   groups  附加组
   home    家目录
   createhome  是否创建家目录
   comment 注释信息
   system  是否是系统用户
   
   # ansible all -m user -a 'name="user1"'
   # ansible all -m user -a 'name="user1" state=absent'
group   组管理
   gid     gid      
   name    组名              
   state   状态          
   system  是否是系统组
   # ansible webserver -m group -a 'name=mysql gid=306 system=yes'
   # ansible webserver -m user -a 'name=mysql uid=306 system=yes group=mysql'
copy    复制文件(复制本地文件到远程主机的指定位置)
   src     定义本地源文件路径
   dest    定义远程目录文件路径(绝对路径)
   owner   属主
   group   属组
   mode    权限
   content 取代src=,表示直接用此处的信息生成为文件内容
   # yum -y install libselinux-python
   # ansible all -m copy -a 'src=/etc/fstab dest=/tmp/fstab.ansible owner=root mode=640'
   # ansible all -m copy -a 'content="hello ansiblenHi ansible" dest=/tmp/test.ansible'
file    设置文件的属性
   path|dest|name  对那个文件做设定
   
   创建文件的符号链接：
       src：    指定源文件
       path：   指明符号链接文件路径
   # ansible all -m file -a 'owner=mysql group=mysql mode=644 path=/tmp/fstab.ansible'
   # ansible all -m file -a 'path=/tmp/fstab.link src=/tmp/fstab.ansible state=link'
ping    测试指定主机是否能连接
   # ansible all -m ping
service 管理服务运行状态
   enabled 是否开机自动启动
   name    指定服务名
   state   指定服务状态
       started     启动服务
       stoped      停止服务
       restarted   重启服务
   arguments   服务的参数
   # ansible webserver -m service -a 'enabled=true name=httpd state=started'
shell   在远程主机上运行命令
   尤其是用到管道变量等功能的复杂命令
   # ansible all -m shell -a 'echo magedu | passwd --stdin user1'
script  将本地脚本复制到远程主机并运行之
   # ansible all -m script -a '/tmp/test.sh'
yum     安装程序包
   name    程序包名称(不指定版本就安装最新的版本latest)
   state   present,latest表示安装，absent表示卸载
   # ansible webserver -m yum -a 'name=httpd'
   # ansible all -m yum -a 'name=ntpdate'  #默认就是安装
   # ansible all -m yum -a 'name=ntpdate state=absent'
setup   收集远程主机的facts
   每个被管理节点在接受并运行管理命令之前，会将自己主机相关信息，如操作系统版本，IP地址等报告给远程的ansible主机
   # ansible all -m setup
```

## **三、Ansible playbook**

组成结构：

```livecodeserver
inventory       #以下操作应用的主机
modules         #调用哪些模块做什么样的操作
ad hoc commands #在这些主机上运行哪些命令
playbooks  
   tasks       #任务,即调用模块完成的某操作
   variable    #变量
   templates   #模板
   handlers    #处理器，由某事件触发执行的操作
   roles       #角色
```

## **四、YAML**

### **4.1 YAML介绍**

YAML是一个可读性高的用来表达资料序列的格式。YAML参考了其它多种语言，包括：XML、C语言、Python、Perl以及电子邮件格式RFC2822等。ClarkEvans在2001年首次发表了这种语言，另外Ingy dot Net与Oren Ben-Kiki也是这语言的共同设计者。

YAML Ain't Markup Language,即YAML不是XML，不过，在开发这种语言时，YAML的意思其实是："Yet Another Markup Language"(仍是一种标记语言)，其特性：

- YAML的可读性好
- YAML和脚本语言的交互性好
- YAML使用实现语言的数据类型
- YAML有一个一致的信息模型
- YAML易于实现
- YAML可以基于流来处理
- YAML表达能力强，扩展性好

> 更多的内容及规范参见[http://www.yaml.org](https://link.segmentfault.com/?enc=ipcrRQjZvmXVqSt4ryfWbg%3D%3D.jFAVF0pobjhRSeCB76YV0PX4NHGuNldqDdLhJuRPv2s%3D)

### **4.2 YAML语法**

YAML的语法和其他高阶语言类似，并且可以简单表达清单、散列表、标量等数据结构，其结构(structure)通过空格来展示，序列(sequence)里的项用"-"来表示，Map里面的键值对用":"分割，下面是一个示例。

```dts
name: john smith
age: 41
gender: male
spouse:
   name:jane smith
   age:37
   gender: female
children:
   -   name:jimmy smith
       age:17
       gender: male
   -   name:jenny smith
       age: 13
       gender: female
```

YAML文件扩展名通常为.yaml，如example.yaml

### **4.2.1 list**

列表的所有元素均使用"-"打头，例如：

```markdown
# A list of testy fruits
- Apple
- Orange
- Strawberry
- Mango
```

### **4.2.2 dictionary**

字典通过key与value进行标识，例如：

```yaml
---
# An employee record
name: Example Developer
job: Developer
skill: Elite
```

也可以将key:value放置于{}中进行表示，例如：

```yaml
---
#An exmloyee record
{name: Example Developer, job: Developer, skill: Elite}
```

## **五、Ansible基础元素**

### **5.1 变量**

#### **5.1.1 变量命名**

变量名仅能由字母、数字和下划线组成，且只能以字母开头。

#### 5.1.2 facts

facts是由正在通信的远程目标主机发回的信息，这些信息被保存在ansible变量中。要获取指定的远程主机所支持的所有facts，可使用如下命令进行：

```1c
#ansible hostname -m setup
```

#### **5.1.3 register**

把任务的输出定义为变量，然后用于其他任务，实例如下：

```yaml
tasks:
   - shell: /usr/bin/foo
     register: foo_result
     ignore_errors: True
```

#### **5.1.4 通过命令行传递变量**

在运行playbook的时候也可以传递一些变量供playbook使用，示例如下：

```stylus
#ansible-playbook test.yml --extra-vars "hosts=www user=mageedu"
```

#### **5.1.5 通过roles传递变量**

当给一个主机应用角色的时候可以传递变量，然后在角色内使用这些变量，示例如下：

```yaml
- hosts: webserver
 roles:
   - common
   - {role: foo_app_instance, dir: '/web/htdocs/a.com', port: 8080}
```

### **5.2 Inventory**

ansible的主要功用在于批量主机操作，为了便捷的使用其中的部分主机，可以在inventory file中将其分组命名，默认的inventory file为/etc/ansible/hosts

inventory file可以有多个，且也可以通过Dynamic Inventory来动态生成。

#### **5.2.1 inventory文件格式**

inventory文件遵循INI文件风格，中括号中的字符为组名。可以将同一个主机同时归并到多个不同的组中；此外，当如若目标主机使用非默认的SSH端口，还可以在主机名称之后使用冒号加端口号来表明。

```stylus
ntp.magedu.com
[webserver]
www1.magedu.com:2222
www2.magedu.com
[dbserver]
db1.magedu.com
db2.magedu.com
db3.magedu.com
如果主机名遵循相似的命名模式，还可使用列表的方式标识个主机，例如：
[webserver]
www[01:50].example.com
[databases]
db-[a:f].example.com
```

#### **5.2.2 主机变量**

可以在inventory中定义主机时为其添加主机变量以便于在playbook中使用，例如：

```routeros
[webserver]
www1.magedu.com http_port=80 maxRequestsPerChild=808
www2.magedu.com http_port=8080 maxRequestsPerChild=909
```

#### **5.2.3 组变量**

组变量是指赋予给指定组内所有主机上的在playbook中可用的变量。例如：

```stylus
[webserver]
www1.magedu.com
www2.magedu.com
[webserver:vars]
ntp_server=ntp.magedu.com
nfs_server=nfs.magedu.com
```

**5.2.4 组嵌套**

inventory中，组还可以包含其它的组，并且也可以向组中的主机指定变量。不过，这些变量只能在ansible-playbook中使用，而ansible不支持。例如：

```stylus
[apache]
httpd1.magedu.com
httpd2.magedu.com
[nginx]
ngx1.magedu.com
ngx2.magedu.com
[webserver:children]    #固定格式
apache
nginx
[webserver:vars]
ntp_server=ntp.magedu.com
```

#### **5.2.5 inventory参数**

ansible基于ssh连接inventory中指定的远程主机时，还可以通过参数指定其交互方式，这些参数如下所示：

```asciidoc
ansible_ssh_host
ansible_ssh_port
ansible_ssh_user
ansible_ssh_pass
ansible_sudo_pass
ansible_connection
ansible_ssh_private_key_file
ansible_shell_type
ansible_python_interpreter
```

### **5.3 条件测试**

如果需要根据变量、facts或此前任务的执行结果来做为某task执行与否的前提时要用到条件测试。

#### **5.3.1 when语句**

在task后添加when字句即可使用条件测试；when语句支持jinja2表达式语句，例如：

```nestedtext
tasks:
 - name: 'shutdown debian flavored system"
   command: /sbin/shutdown -h now
   when: ansible_os_family == "Debian"
```

when语句中还可以使用jinja2的大多"filter",例如果忽略此前某语句的错误并基于其结果(failed或success)运行后面指定的语句，可使用类似如下形式；

```dts
tasks:
 - command:/bin/false
   register: result
   ignore_errors: True
 - command: /bin/something
   when: result|failed
 - command: /bin/something_else
   when: result|success
 - command: /bin/still/something_else
   when: result|skipped
```

此外，when语句中还可以使用facts或playbook中定义的变量

```nestedtext
# cat cond.yml
- hosts: all
 remote_user: root
 vars:
 - username: user10
 tasks:
 - name: create {{ username }} user
   user: name={{ username }}
   when: ansible_fqdn == "node1.exercise.com"
```

### **5.4 迭代**

当有需要重复性执行的任务时，可以使用迭代机制。其使用格式为将需要迭代的内容定义为item变量引用，并通过with_items语句来指明迭代的元素列表即可。例如：

```pf
- name: add server user
 user: name={{ item }} state=persent groups=wheel
 with_items:
   - testuser1
   - testuser2
```

上面语句的功能等同于下面的语句：

```routeros
- name: add user testuser1
 user: name=testuser1 state=present group=wheel
- name: add user testuser2
 user: name=testuser2 state=present group=wheel
```

事实上，with_items中可以使用元素还可为hashes，例如：

```xquery
- name: add several users
 user: name={{ item.name}} state=present groups={{ item.groups }}
 with_items:
   - { name: 'testuser1', groups: 'wheel'}
   - { name: 'testuser2', groups: 'root'}
```

> Ansible的循环机制还有更多的高级功能，具体请参考官方文档[http://docs.ansible.com/playb...](https://link.segmentfault.com/?enc=fvwnoRtR%2B7agPydEk9CCNg%3D%3D.V0PZRi3OwL2WOFQaofzHwrtjcAMQdO6uc90ABGUGvN4bffIkavDxvqLV8%2BoJKowf)

## **六、模板示例：**

```routeros
# grep '{{' conf/httpd.conf
MaxClients       {{ maxClients }}
Listen {{ httpd_port }}
# cat /etc/ansible/hosts
[webserver]
127.0.0.1 httpd_port=80 maxClients=100
192.168.10.149 httpd_port=8080 maxClients=200
# cat apache.yml
- hosts: webserver
 remote_user: root
 vars:
 - package: httpd
 - service: httpd
 tasks:
 - name: install httpd package
   yum: name={{ package }} state=latest
 - name: install configuration file for httpd
   template: src=/root/conf/httpd.conf dest=/etc/httpd/conf/httpd.conf
   notify:
   - restart httpd
 - name: start httpd service
   service: enabled=true name={{ service }} state=started
 
 handlers:
 - name: restart httpd
   service: name=httpd state=restarted
```

## **七、Ansible playbooks**

playbook是由一个或多个"play"组成的列表。play的主要功能在于将事先归并为一组的主机装扮成事先通过ansible中的task定义好的角色。从根本上来讲，所有task无非是调用ansible的一个module。将多个play组织在一个playbook中，即可以让他们连同起来按事先编排的机制同唱一台大戏。下面是一个简单示例。

```pf
- hosts: webserver
 vars:
   http_port: 80
   max_clients: 256
 remote_user: root
 tasks:
 - name: ensure apache is at the latest version
   yum: name=httpd state=latest
 - name: ensure apache is running
   service: name=httpd state=started
 handlers:
   - name: restart apache
     service: name=httpd state=restarted
```

### **7.1 playbook基础组件**

#### **7.1.1 Hosts和Users**

playbook中的每一个play的目的都是为了让某个或某些主机以某个指定的用户身份执行任务。hosts用于指定要执行指定任务的主机，其可以使一个或多个由冒号分隔主机组；remote_user则用于指定远程主机的执行任务的用户，如上面的实例中的

```nestedtext
- hosts: webserver
 remote_user: root
```

不过，remote_user也可用于各task中，也可以通过指定其通过sudo的方式在远程主机上执行任务，其可用于play全局或其任务；此外，甚至可以在sudo时使用sudo_user指定sudo时切换的用户。

```yaml
- hosts: webserver
 remote_user: magedu
 tasks:
  - name: test connection
    ping:
    remote_user: magedu
    sudo: yes
```

#### **7.1.2 任务列表和action**

play的主题部分是task list。task list中的各任务按次序逐个在hosts中指定的所有主机上执行，即在所有主机上完成第一个任务后再开始第二个。在运行自上而下某playbook时，如果中途发生错误，所有已执行任务都可能回滚，在更正playbook后重新执行一次即可。

taks的目的是使用指定的参数执行模块，而在模块参数中可以使用变量。模块执行是幂等的。这意味着多次执行是安全的，因为其结果均一致。

每个task都应该有其name，用于playbook的执行结果输出，建议其内容尽可能清晰地描述任务执行步骤，如果为提供name，则action的结果将用于输出。

定义task可以使用"action: module options"或”module：options“的格式推荐使用后者以实现向后兼容。如果action一行的内容过多，也中使用在行首使用几个空白字符进行换行。

```pf
tasks:
 - name:make sure apache is running
   service: name=httpd state=started
tasks:
 - name: run this command and ignore the result
   shell: /usr/bin/somecommand || /bin/true
```

在众多的模块中，只有command和shell模块仅需要给定一个列表而无需使用"key=value"格式，例如：

```dts
tasks:
 - name: disable selinux
   command: /sbin/setenforce 0
```

如果命令或脚本的退出码不为零，可以使用如下方式替代：

或者使用ignore_errors来忽略错误信息：

```yaml
tasks:
 - name: run this command and ignore the result
   shell: /usr/bin/somecommand
   ignore_errors: True
```

#### **7.1.3、handlers**

用于当关注的资源发生变化时采取一定的操作。

"notify"这个action可用于在每个play的最后被触发，这样可以避免多次有改变发生时每次都执行执行的操作，取而代之，仅在所有的变化发生完成后一次性地执行指定操作，在notify中列出的操作称为handlers，也即notify中调用handlers中定义的操作。

```nestedtext
- name: template configuration file
 template: src=template.j2 dest=/etc/foo.conf
 notify:
   - restart memcached
   - restart apache
```

handlers是task列表，这些task与前述的task并没有本质上的不同。

```pf
handlers：
 - name: restart memcached
   service: name=memcached state=restarted
 - name: restart apache
   service: name=apache state=restarted
```

#### 简单示例1：

```routeros
# cat nginx.yml
- hosts: webserver
 remote_user: root
 tasks:
 - name: create nginxn group
   group: name=nginx system=yes gid=208
 - name: create nginx user
   user: name=nginx uid=208 group=nginx system=yes
- hosts: dbserver
 remote_user: root
 tasks:
 - name: copy file to dbserver
   copy: src=/etc/inittab dest=/tmp/inittab.ans
   
# ansible-playbook nginx.yml
```

#### 简单示例2：

```routeros
# cat apache.yml
- hosts: webserver
 remote_user: root
 tasks:
 - name: install httpd package
   yum: name=httpd state=latest
 - name: install configuration file for httpd
   copy: src=/root/conf/httpd.conf dest=/etc/httpd/conf/httpd.conf
 - name: start httpd service
   service: enabled=true name=httpd state=started
# ansible-playbook apache.yml
```

#### handlers 示例：

```routeros
# cat apache.yml
- hosts: webserver
 remote_user: root
 tasks:
 - name: install httpd package
   yum: name=httpd state=latest
 - name: install configuration file for httpd
   copy: src=/root/conf/httpd.conf dest=/etc/httpd/conf/httpd.conf
   notify:
   - restart httpd
 - name: start httpd service
   service: enabled=true name=httpd state=started
 
 handlers:
 - name: restart httpd
   service: name=httpd state=restarted
#  ansible-playbook apache.yml
```

#### variable 示例1：

```nestedtext
# cat apache.yml
- hosts: webserver
 remote_user: root
 vars:
 - package: httpd
 - service: httpd
 tasks:
 - name: install httpd package
   yum: name={{ package }} state=latest
 - name: install configuration file for httpd
   copy: src=/root/conf/httpd.conf dest=/etc/httpd/conf/httpd.conf
   notify:
   - restart httpd
 - name: start httpd service
   service: enabled=true name={{ service }} state=started
 
 handlers:
 - name: restart httpd
   service: name=httpd state=restarted
```

#### variable 示例2：(在playbook中可以使用所有的变量)

```applescript
# cat facts.yml
- hosts: webserver
 remote_user: root
 tasks:
 - name: copy file
   copy: content="{{ ansible_all_ipv4_addresses }} " dest=/tmp/vars.ans
```

## **八、roles**

ansible自1.2版本引入的新特性，用于层次性、结构化地组织playbook。roles能够根据层次型结构自动转载变量文件、tasks以及handlers等。要使用roles只需要在playbook中使用include指令即可。简单来讲，roles就是通过分别将变量、文件、任务、模板以及处理器放置于单独的目录中，并可以便捷地include他们的一种机制。角色一般用于基于主机构建服务的场景中，但也可以使用于构建守护进程的场景中

一个roles的案例如下所示：

```stylus
site.yml
webserver.yml
fooserver.yml
roles/
   common/
       files/
       templates/
       tasks/
       handlers/
       vars/
       meta/
   webserver/
       files/
       templates/
       tasks/
       handlers/
       vars/
       meta/
```

而在playbook中，可以这样使用role：

```nestedtext
- hosts: webserver
 roles:
   - common  
   - webserver
```

也可以向roles传递参数，例如：

```nestedtext
- hosts: webserver
 roles:
   - common
   - { role: foo_app_instance, dir:'/opt/a',port:5000}
   - { role: foo_app_instance, dir:'/opt/b',port:5001}
```

甚至也可以条件式地使用roles，例如：

```nestedtext
- hosts：webserver
 roles:
   - { role: some_role, when: "ansible_so_family == 'RedHat" }
```

### **8.1 、创建role的步骤**

- 创建以roles命名的目录：
- 在roles目录中分别创建以各角色命名的目录，如webserver等
- 在每个角色命名的目录中分别创建files、handlers、meta、tasks、templates和vars目录；用不到的目录可以创建为空目录，也可以不创建
- 在playbook文件中，调用各角色

### **8.2、 role内各目录中可应用的文件**

- task目录：至少应该包含一个为main.yml的文件，其定义了此角色的任务列表；此文件可以使用include包含其它的位于此目录中的task文件；
- file目录：存放由copy或script等模板块调用的文件；
- template目录：template模块会自动在此目录中寻找jinja2模板文件；
- handlers目录：此目录中应当包含一个main.yml文件，用于定义此角色用到的各handlers，在handler中使用inclnude包含的其它的handlers文件也应该位于此目录中；
- vars目录：应当包含一个main.yml文件，用于定义此角色用到的变量
- meta目录：应当包含一个main.yml文件，用于定义此角色的特殊设定及其依赖关系；ansible1.3及其以后的版本才支持；
- default目录：应当包含一个main.yml文件,用于为当前角色设定默认变量时使用此目录；

```routeros
# mkdir -pv ansible_playbooks/roles/{webserver,dbserver}/{tasks,files,templates,meta,handlers,vars}
# cp /etc/httpd/conf/httpd.conf files/ 
# pwd
/root/ansible_playbooks/roles/webserver
# cat tasks/main.yml
- name: install httpd package
 yum: name=httpd state=present
- name: install configuretion file
 copy: src=httpd.conf dest=/etc/httpd/conf/httpd.conf
 tags:
 - conf
 notify:
 - restart httpd
- name: start httpd
 service: name=httpd state=started
# cat handlers/main.yml
- name: restart httpd
 service: name=httpd state=restarted
   
# pwd;ls
/root/ansible_playbooks
roles  site.yml
# cat site.yml
- hosts: webserver
 remote_user: root
 roles:
 - webserver
# ansible-playbook site.yml
```

## **九、Tags**

tags用于让用户选择运行或跳过playbook中的部分代码。ansible具有幂等性，因此会自动跳过没有变化的部分，即便如此，有些代码为测试其确实没有发生变化的时间依然会非常的长。此时，如果确信其没有变化，就可以通过tags跳过此些代码片段。

tags：在playbook可以为某个或某些任务定义一个"标签"，在执行此playbook时，通过为ansible-playbook命令使用--tags选项能耐实现仅运行指定的tasks而非所有的；

```nestedtext
# cat apache.yml
- hosts: webserver
 remote_user: root
 vars:
 - package: httpd
 - service: httpd
 tasks:
 - name: install httpd package
   yum: name={{ package }} state=latest
 - name: install configuration file for httpd
   template: src=/root/conf/httpd.conf dest=/etc/httpd/conf/httpd.conf
   tags:
   - conf
   notify:
   - restart httpd
 - name: start httpd service
   service: enabled=true name={{ service }} state=started
 
 handlers:
 - name: restart httpd
   service: name=httpd state=restarted
# ansible-playbook apache.yml --tags='conf'
```

特殊tags：always #无论如何都会运行

> 作者：kangvcar
> 来源：[https://my.oschina.net/kangvc...](https://link.segmentfault.com/?enc=sZETz46ps7Ci6%2BKEDOPVMw%3D%3D.Y%2FsRiqnBWd2kf%2Bt5C2wBxZsQP0M890g2dWBnUFP9Rs9eYWsz8WYGQFDeoZWI%2F%2Ben)