[TOC]

# 非常好的Ansible入门教程（超简单）

Ansible是一个配置管理和配置工具，类似于Chef，Puppet或Salt。这是一款很简单也很容易入门的部署工具，它使用SSH连接到服务器并运行配置好的任务，服务器上不用安装任何多余的软件，只需要开启ssh，所有工作都交给client端的ansible负责。
关于Ansible的一个好处是，将bash脚本转换为可执行任务是非常容易的。我们可以编写自己的配置程序，但是Ansible更加干净，因为它可以自动在执行任务之前获取上下文。ansible任务是幂等的，没有大量额外的编码，ansible可以一次又一次地安全运，而bash命令这种幂等性。
ansible使用“facts”来确保任务的幂等安全运行， 它是在运行任务之前收集的系统和环境信息。ansible使用这些facts来检查状态，看看是否需要改变某些东西以获得所需的结果。这使得ansible可以让服务器一次又一次地运行可复制的任务。

# 1 安装

当然我们需要先安装Ansible。任务可以从任何可安装的机器上运行。

### 1.1 Ubuntu

在Ubuntu 16.04上安装Ansible的方法。

```
sudo apt-get install -y ansible1
```

apt-get安装的ansible版本很低，建议使用pip方式安装

```
sudo pip install ansible1
```

# 2 配置

ansible的默认配置文件路径为 /etc/ansible，然而，一个常见的用途是将其安装在一个virtualenv中，在这种情况下，我们一般不会使用这些默认文件。我们可以根据需要在本地目录中创建配置文件。

## 2.1 管理服务器：Inventory文件

您可以创建一个inventory文件，用于定义将要管理的服务器。这个文件可以命名为任何名字，但我们通常会命名为hosts或者项目的名称。
在hosts文件中，我们可以定义一些要管理的服务器。这里我们将定义我们可能要在“web”标签下管理的两个服务器。标签是任意的。

```
[web]
192.168.22.10
192.168.22.11123
```

现在已经够好了，如果需要，我们可以定义主机范围，多个组，可重用变量，并使用其他花哨的设置，包括创建动态的inventory。
当我们在本地机器运行ansible时，我们不需要关心inventory文件中的内容，我将告诉您在本地和远程服务器上运行ansible。现在，让我们将hosts文件设置为指向本地主机local和remote虚拟远程主机。
hosts文件：

```
[local]
127.0.0.1

[remote]
192.168.1.212345
```

与本地主机和远程服务器连接的命令。

## 2.2 基础：运行命令

我们开始对服务器运行任务。ansible会假定你的服务器具有SSH访问权限，通常基于SSH-Key。因为Ansible使用SSH，所以它需要能够SSH连接到服务器。但是，ansible将尝试以正在运行的当前用户身份进行连接。如果我正在运行ansible的用户是ubuntu，它将尝试以ubuntu连接其他服务器。

```
# Run against localhost
$ ansible -i ./hosts --connection=local local -m ping

# Run against remote server
$ ansible -i ./hosts remote -m ping
127.0.0.1 | success >> {
    "changed": false,
    "ping": "pong"
}123456789
```

如果你是在cygwin下运行，遇到了“Failed to connect to the host via ssh: mux_client_request_session: read from master failed”的错误，可以执行:

```
ansible -i ./hosts remote -v -m ping -u root --private-key=~/.ssh/id_rsa1
```

使用–connection=local告诉ansible不尝试通过SSH运行命令，因为我们只是影响本地主机。但是，我们仍然需要一个hosts文件，告诉我们连接到哪里。
在任何情况下，我们可以看到从ansible得到的输出是一些JSON，它告诉我们Task（我们对ping模块的调用）是否进行了任何更改和结果。

命令说明：

```
-i ./hosts - 设置库存文件，命名为 hosts
remote，local，all-使用这个标签的下定义的服务器hosts清单文件。“all”是针对文件中定义的每个服务器运行的特殊关键字
-m ping- 使用“ping”模块，它只是运行ping命令并返回结果
-c local| --connection=local - 在本地服务器上运行命令，而不是SSH

一些常用命令：
-i PATH --inventory=PATH 指定host文件的路径，默认是在/etc/ansible/hosts
--private-key=PRIVATE_KEY_FILE_PATH 使用指定路径的秘钥建立认证连接
-m DIRECTORY --module-path=DIRECTORY 指定module的目录来加载module，默认是/usr/share/ansible
-c CONNECTION --connection=CONNECTION 指定建立连接的类型，一般有ssh ，local12345678910
```

### 2.2.1 模块（Modules）

ansible使用“模块”来完成大部分的任务。模块可以做安装软件，复制文件，使用模板等等。

#### 模块是使用Ansible 的方法

因为它们可以使用可用的上下文（“Facts”），以便确定要完成任务需要做什么操作。
如果我们没有模块，我们将运行任意的shell命令，我们也可以使用bash脚本。这是一个任意shell命令看起来像在Ansible（它使用的shell模块！）：

```
# Run against a local server
ansible -i ./hosts local --connection=local -b --become-user=root \
    -m shell -a 'apt-get install nginx'

# Run against a remote server
ansible -i ./hosts remote -b --become-user=root all \
    -m shell -a 'apt-get install nginx'1234567
```

这里，sudo apt-get install nginx命令将使用“shell”模块运行。
命令说明:

```
-b - “成为”，在运行命令时告诉可以成为另一个用户。
--become-user=root - 以用户“root”运行以下命令（例如，使用命令使用“sudo”）。我们可以在此定义任何现有的用户。
-a 用于将任何参数传递给定义的模块 -m123
```

但是这并不是特别强大。尽管能够一次在所有服务器上运行这些命令，但是我们仍然只能完成任何bash脚本可能执行的操作。如果我们使用了更合适的模块，我们可以运行命令来保证结果。可靠的模块确保我们可以一次又一次地运行相同的任务，而不会影响最终结果。
要在Debian / Ubuntu服务器上安装软件，“apt”模块将运行相同的命令，但确保幂等。

```
# Run against a local server
ansible -i ./hosts local --connection=local -b --become-user=root \
    -m apt -a 'name=nginx state=installed update_cache=true'

127.0.0.1 | success >> {
    "changed": false
}

# Run against a remote server
ansible -i ./hosts remote -b --become-user=root \
    -m apt -a 'name=nginx state=installed update_cache=true'

127.0.0.1 | success >> {
    "changed": false
}

or：
ansible -i ./hosts remote -v -m apt -a 'name=nginx state=installed update_cache=true' -u test -s -K --private-key=~/.ssh/id_rsa
12345678910111213141516171819
```

这将使用apt模块来更新存储库缓存并安装Nginx（如果没有安装）。
运行任务的结果是”changed”: false。这表明没有变化; 我已经使用该shell模块安装了Nginx 。好的是，我可以一遍又一遍地运行这个命令，而不用担心它会改变预期的结果 - Nginx已经安装，Ansible知道，并且不尝试重新安装它。
命令说明:

```
-i ./hosts - 设置inventory文件，命名为 hosts
-b - “成”，告诉可以成为另一个用户来运行命令
--become-user=root - 以用户“root”运行以下命令（例如，使用“sudo”命令）
local| remote - 从库存文件中的本地或远程定义的主机上运行
-m apt- 使用apt模块
-a 'name=nginx state=installed update_cache=true' - 提供apt模块的参数，包括软件包名称，所需的结束状态以及是否更新软件包存储库缓存

常用命令：
-u USERNAME --user=USERNAME 指定移动端的执行用户
-U SUDO_USERNAME --sudo-user=USERNAME
-s --sudo -u指定用户的时候，使用sudo获得root权限
-k --ask-pass  提示输入ssh的密码，而不是使用基于ssh的密钥认证
-K --ask-sudo-pass 提示输入sudo密码，与--sudo一起使用12345678910111213
```

我们可以通过这种特殊方式运行我们所需要的所有任务（通过模块），但是让我们来做这个更具管理性。我们将把这个任务移动到一个Playbook中，它可以运行和协调多个Tasks。

## 2.3 剧本（Playbooks）

Playbook可以运行多个任务，并提供一些更高级的功能。让我们将上述任务移到一本剧本中。在ansible中剧本（playbooks）和角色（roles）都使用Yaml文件定义。
创建文件nginx.yml：

```
---
# hosts could have been "remote" or "all" as well
- hosts: local
  connection: local
  become: yes
  become_user: root
  tasks:
   - name: Install Nginx
     apt:
       name: nginx
       state: installed
       update_cache: true123456789101112
```

此任务与我们的ad-hoc命令完全相同，包括设置本地连接的使用。
这将使用inventory文件中[local]标签下的服务器hosts。
如果我们没有使用本地连接，我们会这样做：

```
---
- hosts: remote
  become: yes
  become_user: root
  tasks:
   - name: Install Nginx
     apt:
       name: nginx
       state: installed
       update_cache: true12345678910
```

这将使用inventory文件中[remote]标签下的服务器hosts。

在我们的Tasks文件中使用become并become_user再次使用Ansible来sudo以root用户身份运行命令，然后传递Playbook文件。

使用一个yaml playbook文件，我们需要使用这个ansible-playbook命令，现在就更容易运行：

```
$ ansible-playbook -i ./hosts nginx.yml

PLAY [local] ******************************************************************

GATHERING FACTS ***************************************************************
ok: [127.0.0.1]

TASK: [Install Nginx] *********************************************************
ok: [127.0.0.1]

PLAY RECAP ********************************************************************
127.0.0.1                  : ok=2    changed=0    unreachable=0    failed=0123456789101112
```

我们在运行过程中获得了一些有用的反馈，包括“可执行任务”运行及其结果。在这里我们看到所有运行都OK，但没有改变。我已经安装了Nginx

### 2.3.1 处理程序（Handlers）

处理程序与任务完全相同（它可以做task可以做的任何事），但只有当另一个任务调用它时才会运行。您可以将其视为事件系统的一部分; 处理程序将通过其侦听的事件调用进行操作。
这对于运行任务后可能需要的“辅助”操作非常有用，例如在配置更改后安装或重新加载服务后启动新服务。

```
---
# Example shows using the local machine still
# Remove 'connection' and set hosts to 'remote' for a remote connection
- hosts: local
  connection: local
  become: yes
  become_user: root
  tasks:
   - name: Install Nginx
     apt:
       name: nginx
       state: installed
       update_cache: true
     notify:
      - Start Nginx

  handlers:
   - name: Start Nginx
     service:
       name: nginx
       state: started123456789101112131415161718192021
```

这里我们添加一个notify指令到安装任务。这将在任务运行后通知名为“Start Nginx”的处理程序。

然后我们可以创建名为“Start Nginx”的处理程序。此处理程序是通知“Start Nginx”时调用的任务。
这个特定的处理程序使用服务模块，它可以启动，停止，重启，重新加载（等等）系统服务。在这种情况下，我们告诉Ansible，我们要启动Nginx。
让我们再次运行这本Playbook：

```
$ ansible-playbook -i ./hosts nginx.yml

PLAY [local] ******************************************************************

GATHERING FACTS ***************************************************************
ok: [127.0.0.1]

TASK: [Install Nginx] *********************************************************
ok: [127.0.0.1]

NOTIFIED: [nginx | Start Nginx] ***********************************************
ok: [127.0.0.1]

PLAY RECAP ********************************************************************
127.0.0.1                  : ok=2    changed=0    unreachable=0    failed=0123456789101112131415
```

我们得到类似的输出，但是这次Handler是运行的。
通知程序只在运行任务时运行。

##### Note：如果我已经安装了Nginx，则安装Nginx任务将不会运行，通知程序也将不会被调用。

我们可以使用Playbook来运行多个任务，添加变量，定义其他设置，甚至包括其他的剧本。

### 2.3.2 更多的任务（More Tasks）

接下来，我们可以为此Playbook添加更多的任务，并探索其他一些功能。

```
---
# Example shows using the local machine still
# Remove 'connection' and set hosts to 'remote' for a remote connection
- hosts: local
  connection: local
  become: yes
  become_user: root
  vars:
   - docroot: /var/www/serversforhackers.com/public
  tasks:
   - name: Add Nginx Repository
     apt_repository:
       repo: ppa:nginx/stable
       state: present
     register: ppastable

   - name: Install Nginx
     apt:
       pkg: nginx
       state: installed
       update_cache: true
     when: ppastable|success
     notify:
      - Start Nginx

   - name: Create Web Root
     file:
      path: '{{ docroot }}'
      mode: 775
      state: directory
      owner: www-data
      group: www-data
     notify:
      - Reload Nginx

  handlers:
   - name: Start Nginx
     service:
       name: nginx
       state: started

    - name: Reload Nginx
      service:
        name: nginx
        state: reloaded123456789101112131415161718192021222324252627282930313233343536373839404142434445
```

现在有三个任务：

```
Add Nginx Repository- 使用apt_repository模块添加Nginx稳定PPA以获取最新的稳定版本的Nginx 。
Install Nginx - 使用Apt模块安装Nginx。
Create Web Root - 最后创建一个Web根目录。123
```

新的register和when指令，可以实现在某些事情发生后让ansible执行任务的功能。

##### Note: 您还可以注册模块操作的结果，并使用定义的变量根据注册（register）的变量值有条件（when）地执行操作。例如，注册通过shell模块运行命令的结果可以让您访问该命令的stdout。

同时还使用了一个变量。docroot变量在定义vars部分。然后将其用作创建定义目录的文件模块的目标参数。

需要注意的是，path配置使用括号{{ var-name }}，这是Jinja2的模板。为了使Ansible能够在括号内解析Jinja2模板变量，该行必须是单引号或双引号 - 例如，path: ‘{{ docroot }}’而不是path: {{ docroot }}。不使用引号将导致错误。
这个playbook可以用通常的命令运行：

```
ansible-playbook -i ./hosts nginx.yml1
```

所以，我们已经运行了一些ad-hoc命令，使用了可复制的模块，并将一些相关任务组织到一个手册中。

接下来，我们将通过将Playbook组织成一个角色进一步获得可靠性，这有助于我们组织相关项目，如文件和模板，同时还帮助我们组织更复杂的相关任务和操作。

## 2.4 角色（roles）

角色很适合组织多个相关任务并封装完成这些任务所需的数据。例如，安装Nginx可能涉及添加软件包存储库，安装软件包和设置配置。
此外，真实的配置通常需要额外的数据，如变量，文件，动态模板等等。这些工具可以与Playbook一起使用，但是我们可以通过将相关任务和数据组织成一个角色（role， 相关的结构）很快就能做得更好。
角色有一个这样的目录结构：

```
roles
  rolename
   - files
   - handlers
   - meta
   - templates
   - tasks
   - vars12345678
```

在每个子目录中（eg： files，handlers等等），Ansible将自动搜索并读取叫做main.yml的yaml文件。
接下来我们将分解nginx.yml文件内容为不同的组件，并将每个组件放在相应的目录中，以创建一个更干净，更完整的配置工具集。

### 2.4.1 创建角色（Creating a Role）

我们可以使用ansible-galaxy命令来创建一个新角色。此工具可用于将角色保存到Ansible的公共注册表，但是我通常只是使用它来在本地创建role的基础目录结构。

我们来看看如何设置：

```
# Head to our previously created directory
cd ~/ansible-example

# In case we left our virtualenv at some point 
source .venv/bin/activate

# Create a roles directory
mkdir roles
cd roles

# Bootstrap a new role named "nginx"
ansible-galaxy init nginx123456789101112
```

目录名称roles是一种惯例，在运行一个playbook时可以用来查找角色。该目录应该始终被命名roles，但并不强制。在roles目录中运行 ansible-galaxy init nginx 命令将创建新角色所需的目录和文件。

我们来看看我们新建的nginx角色的每个部分~/ansible-example/roles/nginx。

### 2.4.2 文件（files）

首先，在files目录中，我们可以添加我们要复制到我们的服务器中的文件。对于nginx，我经常复制H5BP的Nginx组件配置。我只需从Github下载最新的信息，进行一些调整，并将它们放入files目录中。

```
~/ansible-example
 - roles
 - - nginx
 - - - files
 - - - - h5bp12345
```

我们稍后会看到，H5BP配置文件将通过复制模块添加到服务器。

### 2.4.3 处理程序（handlers）

我们可以把曾经在nginx.yml 剧本中的定义的所有处理程序放入到handlers目录中。约定必须包含main.yml文件。

handlers/main.yml 内容：

```
---
- name: Start Nginx
  service:
    name: nginx
    state: started

- name: Reload Nginx
  service:
    name: nginx
    state: reloaded12345678910
```

一旦handlers/main.yml中的处理程序定义好了，我们可以自由地从其他的yaml配置中引用它们。

### 2.4.4 元（meta）

meta目录中的main.yml文件包含Role元数据，包含的依赖关系。如果这个角色依赖于另一个角色，我们可以在这里定义。例如，nginx角色取决于安装SSL证书的ssl角色。约定必须包含main.yml文件。
meta/main.yml 内容：

```
---
dependencies:
  - { role: ssl }123
```

如果我调用了“nginx”角色，它将尝试首先运行“ssl”角色。
否则我们可以省略此文件，或将角色定义为没有依赖关系：

```
---
dependencies: []12
```

### 2.4.5 模板（templates）

基于Python的Jinja2模板引擎（和django的模板引擎很类似），模板文件可以包含模板变量。这里的文件应该以.j2为类型后缀（eg.uwsgi.j2），提倡但是不强制，也可以取其他的名字。类似于files，在templates目录中没有main.yml文件，只包含.j2后缀的模板文件。
这是一个Nginx服务器（“虚拟主机”）配置的例子。请注意，它使用了稍后在vars/main.yml文件中定义的一些变量。
我们的示例中的Nginx配置文件位于templates/serversforhackers.com.conf.j2：

```
server {
    # Enforce the use of HTTPS
    listen 80 default_server;
    server_name {{ domain }};
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl default_server;

    root /var/www/{{ domain }}/public;
    index index.html index.htm index.php;

    access_log /var/log/nginx/{{ domain }}.log;
    error_log  /var/log/nginx/{{ domain }}-error.log error;

    server_name {{ domain }};

    charset utf-8;

    include h5bp/basic.conf;

    ssl_certificate           {{ ssl_crt }};
    ssl_certificate_key       {{ ssl_key }};
    include h5bp/directive-only/ssl.conf;

    location / {
        try_files $uri $uri/ /index.php$is_args$args;
    }

    location = /favicon.ico { log_not_found off; access_log off; }
    location = /robots.txt  { log_not_found off; access_log off; }

    location ~ \.php$ {
        include snippets/fastcgi.conf;
        fastcgi_pass unix:/var/run/php7.1-fpm.sock;
    }
}1234567891011121314151617181920212223242526272829303132333435363738
```

这是一个相当标准的用于PHP应用程序的Nginx配置。这里有三个变量：

域
ssl_crt
ssl_key
这三个变量将在变量部分（vars）中定义。

### 2.4.6 变量（vars）

在使用任务集成所有事情之前，让我们来看看变量。该vars目录包含一个main.yml文件（如handlers和meta目录一样），在main.yml中我们可以列出将要使用的所有变量。
以下是该vars/main.yml文件的内容：

```
---
domain: serversforhackers.com
ssl_key: /etc/ssl/sfh/sfh.key
ssl_crt: /etc/ssl/sfh/sfh.crt1234
```

我们可以在这个角色的其他地方使用这三个变量。我们在上面的模板中看到它们的使用，但是我们也可以在我们定义的任务中看到它们。

##### Note:如果您有敏感信息添加到变量文件中，则可以使用ansible-vault加密文件，下面将对此进行说明。

### 2.4.7 任务（tasks）

终于到了将一切都是放在一系列的任务中的时候了。
使用角色时运行的主文件是tasks/main.yml文件。看看我们的用例将会是什么样的：

```
---
- name: Add Nginx Repository
  apt_repository:
    repo: ppa:nginx/stable
    state: present

- name: Install Nginx
  apt:
    pkg: nginx
    state: installed
    update_cache: true
  notify:
    - Start Nginx

- name: Add H5BP Config
  copy:
    src: h5bp
    dest: /etc/nginx
    owner: root
    group: root

- name: Disable Default Site Configuration
  file:
    dest: /etc/nginx/sites-enabled/default
    state: absent

# `dest` in quotes as a variable is used!
- name: Add SFH Site Config
  register: sfhconfig
  template:
    src: serversforhackers.com.j2
    dest: '/etc/nginx/sites-available/{{ domain }}.conf' 
    owner: root
    group: root

# `src`/`dest` in quotes as a variable is used!
- name: Enable SFH Site Config
  file:
    src: '/etc/nginx/sites-available/{{ domain }}.conf'
    dest: '/etc/nginx/sites-enabled/{{ domain }}.conf'
    state: link

# `dest` in quotes as a variable is used!
- name: Create Web root
  file:
    dest: '/var/www/{{ domain }}/public'
    mode: 775
    state: directory
    owner: www-data
    group: www-data
  notify:
    - Reload Nginx

# `dest` in quotes as a variable is used!
- name: Web Root Permissions
  file:
   dest: '/var/www/{{ domain }}'
   mode: 775
   state: directory
   owner: www-data
   group: www-data
   recurse: yes
  notify:
    - Reload Nginx12345678910111213141516171819202122232425262728293031323334353637383940414243444546474849505152535455565758596061626364
```

这一系列任务使得Nginx能被完整的安装。任务按照出现的顺序完成以下工作：

```
1 添加nginx / stable库
2 安装并启动Nginx
3 添加H5BP配置文件
4 从sites-enabled目录中删除文件的符号链接来禁用默认的Nginx配置
5 将serversforhackers.com.conf.j2虚拟主机模板复制到Nginx配置中，渲染模板
6 通过将其符号链接到sites-enabled目录来启用Nginx服务器配置
7 创建Web根目录
8 更改项目根目录的权限（递归），该目录位于之前创建的Web根目录之上12345678
```

有一些新的模块（和一些我们已经涵盖的新用途），包括复制，模板和文件模块。通过设置每个模块的参数，我们可以做一些有趣的事情，例如确保文件“不存在”（如果存在则删除它们）的state: absent，或者通过创建一个文件作为符号链接的state: link。您应该检查每个模块的文档，以查看可以用它们完成哪些有趣和有用的事情。

### 2.4.8 运行角色（Running the Role）

要对服务器运行一个或多个角色，我们将重新使用另一个playbook。该playbook与roles目录位于同一个目录中，同一层级。当我们用ansible-playbook命令运行的时候需要先cd进入到该目录中。
让我们创建一个“主”的yaml文件（被ansible-playbook命令执行的文件），该文件定义要使用的角色以及运行它们的主机：
文件~/ansible-example/server.yml位于与roles目录相同的目录中：

```
---
# run locally here, yadda yadda yadda
- hosts: local
  connection: local
  roles:
    - nginx123456
```

所以，我们只是定义角色，而不是在本Playbook文件中定义所有的变量和任务。角色负责具体细节。

然后我们可以运行角色：

```
ansible-playbook -i ./hosts server.yml1
```

以下是运行Nginx角色的Playbook文件的输出：

```
PLAY [all] ********************************************************************

GATHERING FACTS ***************************************************************
ok: [127.0.0.1]

TASK: [nginx | Add Nginx Repository] ******************************************
changed: [127.0.0.1]

TASK: [nginx | Install Nginx] *************************************************
changed: [127.0.0.1]

TASK: [nginx | Add H5BP Config] ***********************************************
changed: [127.0.0.1]

TASK: [nginx | Disable Default Site] ******************************************
changed: [127.0.0.1]

TASK: [nginx | Add SFH Site Config] *******************************************
changed: [127.0.0.1]

TASK: [nginx | Enable SFH Site Config] ****************************************
changed: [127.0.0.1]

TASK: [nginx | Create Web root] ***********************************************
changed: [127.0.0.1]

TASK: [nginx | Web Root Permissions] ******************************************
ok: [127.0.0.1]

NOTIFIED: [nginx | Start Nginx] ***********************************************
ok: [127.0.0.1]

NOTIFIED: [nginx | Reload Nginx] **********************************************
changed: [127.0.0.1]

PLAY RECAP ********************************************************************
127.0.0.1                  : ok=8   changed=7   unreachable=0    failed=012345678910111213141516171819202122232425262728293031323334353637
```

我们将所有各种组件放在一起，形成一致的角色，现在已经安装并配置了Nginx！

## 2.5 事实(Facts)

请注意，运行剧本时的第一行总是“收集事实”。
在运行任何任务之前，Ansible将收集有关其配置的系统的信息。这些被称为事实，并且包括广泛的系统信息，如CPU核心数量，可用的ipv4和ipv6网络，挂载的磁盘，Linux发行版等等。

事实在“任务”或“模板”配置中通常很有用。例如，Nginx通常设置为使用与CPU内核一样多的工作处理器。知道这一点，您可以选择如下设置nginx.conf.j2文件的模板：

```
user www-data;
worker_processes {{ ansible_processor_cores }};
pid /var/run/nginx.pid;

# And other configurations...12345
```

或者如果你具有多个CPU的服务器，则可以使用：

```
user www-data;
worker_processes {{ ansible_processor_cores * ansible_processor_count }};
pid /var/run/nginx.pid;

# And other configurations...12345
```

所有的ansible facts全局变量都是以“anisble_”为前缀，并且可以在其他任何地方使用。
尝试对你的本地机器运行以下内容以查看可用的事实：

```
# Run against a local server
# Note that we say to use "localhost" instead of defining a hosts file here!
ansible -m setup --connection=local localhost

# Run against a remote server
ansible -i ./hosts remote -m setup123456
```

## 2.6 加密（Vault）

我们经常需要将敏感数据存储在我们的模板，文件或变量文件中; 这样安全性有一定要求的情况是不可避免的（当我们将这些敏感数据文件推送到远程Git仓库时，这是一个痛苦的事情）。Ansible有一个叫做Ansible Vault的解决方案。
Vault允许您加密任何Yaml文件，通常将其作用与变量文件，Vault不会加密文件和模板，只能使用Yaml文件。
在创建加密文件时，系统会询问您必须使用的密码，以便稍后在调用角色或Playbook时进行编辑。
将密码保存在安全的地方。

例如我们可以创建一个新的变量文件：

```
ansible-vault create vars/main.yml
Vault Password:12
```

输入加密密码后，该文件将在您的默认编辑器（通常是Vim或Nano）中打开。
默认使用的编辑器由EDITOR环境变量定义。默认值通常是Vim。如果您不是Vim用户，可以通过设置环境变量来快速更改：

```
EDITOR=nano ansible-vault edit vars/main.yml1
```

在大多数情况下，我们将使用ansible-vault create|edit /path/to/file.yml。更多可用的命令如下：

```
create - 创建一个新文件并进行加密
decrypt - 从加密文件创建明文文件
edit - 编辑已经存在的加密文件
encrypt - 加密现有的纯文本文件
rekey - 在加密文件中设置新密码12345
```

如果你有一个现有的配置文件要加密，请使用 ansible-vault encrypt /path/to/file.yml。

### 示例： users角色

我们创建一个名为“users”的角色：

```
cd ~/ansible-example/roles
ansible-galaxy init users12
```

创建新用户并设置密码时，我使用Vault 。在用户角色中，您可以设置带有用户密码和公钥的变量文件，以添加到用户的authorized_keys文件（从而提供SSH访问权限）。公共SSH密钥在技术上是安全的，一般公众可以看到 - 所有人都可以使用它来允许你访问自己的服务器。在没有配对私钥的情况下，公钥是不能获得系统访问权限的，我们没有将密钥加入此角色。
以下是可以使用Vault创建和加密的示例变量文件。在编辑它时，它是纯文本。

~/ansible-example/roles/users/vars/main.yml：

```
admin_password: $6$lpQ1DqjZQ25gq9YW$mHZAmGhFpPVVv0JCYUFaDovu8u5EqvQi.Ih
deploy_password: $6$edOqVumZrYW9$d5zj1Ok/G80DrnckixhkQDpXl0fACDfNx2EHnC
common_public_key: ssh-rsa ALongSSHPublicKeyHere123
```

请注意，用户的密码也是散列的。您可以阅读Ansible有关生成加密密码的文档，用户模块需要设置用户密码。作为一个快速入门，它在Ubuntu上看起来像这样：

```
# The whois package makes the mkpasswd
# command available on Ubuntu
$ sudo apt-get install -y whois

# Create a password hash
$ mkpasswd --method=SHA-512
Password:1234567
```

这将生成一个散列密码供你与user模块一起使用。

##### Note：变量文件中的密码是散列的，但我仍然喜欢加密包含散列密码的yaml文件。这些文件通常包含未标记的数据，如API令牌或SSH私钥，使加密非常重要。

一旦你设置了用户密码并将公钥添加到变量文件中，我们就可以加密此文件，然后在任务中使用这些加密变量。

```
ansible-vault encrypt roles/users/vars/main.yml1
```

然后我们可以编辑我们的任务文件，使用（加密）变量添加新用户：

这是文件~/ansible-example/roles/users/tasks/main.yml：

```
---
- name: Create Admin User
  user:
    name: admin
    password: '{{ admin_password }}'
    groups: sudo
    append: yes
    shell: /bin/bash

- name: Add Admin Authorized Key
  authorized_key:
    user: admin
    key: '{{ common_public_key }}'
    state: present

- name: Create Deploy User
  user:
    name: deploy
    password: '{{ deploy_password }}'
    groups: www-data
    append: yes
    shell: /bin/bash

- name: Add Deployer Authorized Key
  authorized_key:
    user: deploy
    key: '{{ common_public_key }}'
    state: present12345678910111213141516171819202122232425262728
```

这些任务使用该user模块来创建新用户，传递变量文件中设置的密码。
它还使用该authorized_key模块将SSH公钥作为SSH授权密钥添加到每个用户的服务器中。
加密变量的使用像在常规任务文件中使用一样。但是，为了运行此角色，我们需要告诉Ansible请求输入vault密码，以便它可以解密变量。
编辑我们的server.ymlPlaybook文件，调用user角色：

```
---
# Local connection here, yadda yadda yadda
- hosts: local
  connection: local
  sudo: yes
  roles:
    - nginx
    - user12345678
```

要运行此Playbook，我们需要告知Ansible请求vault的密码，因为我们正在运行包含加密文件的角色：

```
ansible-playbook --ask-vault-pass -i ./hosts server.yml1
```

# 3 总结

本篇文章带着做了如下工作：

1. 安装了ansible
2. 配置了ansible inventory文件（仅在不使用connection: local 时才需要）
3. 同时在多个服务器上执行幂等的 ad-hoc命令
4. 创建一个基本的Playbook来运行多个任务（tasks），并使用了处理程序（handlers）
5. 将多个任务抽象为一个角色，以保持所有Nginx相关的操作在一个角色内
   - 展示了如何设置依赖关系
   - 展示了如何注册任务的“依赖”执行关系，当一个任务执行成功后再执行另一个任务
   - 展示了如何在我们的任务中使用更多的模板，文件和变量
6. 展示了如何整合使用ansible事实(facts)
7. 展示了如何使用ansible的vault来增加我们的变量的安全性

参考： https://serversforhackers.com/c/an-ansible2-tutorial



- https://blog.csdn.net/pushiqiang/article/details/78126063