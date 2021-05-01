[TOC]

### git配置

/etc/gitconfig 文件：系统中对所有用户都普遍适用的配置。若使用 git config 时用 –system 选项，读写的就是这个文件。 
~/.gitconfig 文件：用户目录下的配置文件只适用于该用户。若使用 git config 时用 –global 选项，读写的就是这个文件。 
当前项目的 Git 目录中的配置文件（也就是工作目录中的 .git/config 文件）：这里的配置仅仅针对当前项目有效。每一个级别的配置都会覆盖上层的相同配置，所以 .git/config 里的配置会覆盖 /etc/gitconfig 中的同名变量。

**查看配置信息** 
要检查已有的配置信息，可以使用 git config -l 命令： 
有时候会看到重复的变量名，那就说明它们来自不同的配置文件（比如 /etc/gitconfig 和 ~/.gitconfig），不过最终 Git 实际采用的是最后一个。

**配置命令**

```shell
#1. 设置用户名邮箱 
$ git config --global user.name "Your Name"
$ git config --global user.email "email@example.com"

#2. https替代git协议
$ git config --global url."https://".insteadof "git://"
$ git config --global url."https://github.com/".insteadof "git@github.com:"

#3. 设置代理 
$ git config --global http.proxy socks5://127.0.0.1:1080
$ git config --global https.proxy socks5://127.0.0.1:1080
取消:
$ git config --global --unset http.proxy
$ git config --global --unset https.proxy

#4. 取消crlf自动转换为lf
$ git config --global core.autocrlf false

#5. 使用simple模式（只推送当前分支到远端）而不是matching（推送本地所有分支到远端）
$ git config --global push.default simple

#6. Git提交时发生SSL certificate problem错误的解决方法
$ git config --global http.sslVerify false

#7. Git提交时发生 error: RPC failed;
解决办法，把git的buffer扩充到500M：
$ git config http.postBuffer 524288000

#8. 使用以下命令生成SSH Key：
$ ssh-keygen -t rsa -C "youremail@example.com"
后面的your_email@example.com改为你在github上注册的邮箱，
之后会要求确认路径和输入密码，我们这使用默认的一路回车就行。
成功的话会在~/下生成.ssh文件夹，进去，打开id_rsa.pub，复制里面的key。
-->
回到github上，进入 Account Settings（账户配置），
左边选择SSH Keys，Add SSH Key,title随便填，粘贴在你电脑上生成的key。
```

#### **git提交小技巧**

**git add -A = git add . + git add -u**

```shell
git add -A # stages All
git add .  # stages new and modified, without deleted
git add -u # stages modified and deleted, without new123
```

### **关于Crlf和lf的那些事:**

**(fatal: CRLF would be replaced by LF )**

CR回车 LF换行Windows/Dos CRLF \r\n 
Linux/Unix LF \n 
MacOS CR \r 
解决方法是：打开命令行，进行设置，如果你是在Windows下开发，建议设置autocrlf为true。 
2014/08/20 补充：如果你文件编码是UTF8并且包含中文文字，那还是把autocrlf设置为false，并且把所有文件转换为Linux编码（即LF\n），开启safecrlf检查。

一、AutoCRLF

```shell
#提交时转换为LF，检出时转换为CRLF
git config --global core.autocrlf true   

#提交时转换为LF，检出时不转换
git config --global core.autocrlf input   

#提交检出均不转换
git config --global core.autocrlf false12345678
```

二、SafeCRLF

```shell
#拒绝提交包含混合换行符的文件
git config --global core.safecrlf true   

#允许提交包含混合换行符的文件
git config --global core.safecrlf false   

#提交包含混合换行符的文件时给出警告
git config --global core.safecrlf warn
123456789
```

### bower

**1. 代理** 
在工程或用户主目录下，新建一个.bowerrc文件，文件内容是JSON格式：

```shell
"proxy" : "socks5://127.0.0.1:1080",
"https-proxy": "socks5://127.0.0.1:1080"12
```

**2. shorthand-resolver** 
默认值：

```shell
git://github.com/{{owner}}/{{package}}.git1
```

如果网络不通，或不能使用git，可以将其修改为

```shell
"shorthand-resolver": "https://github.com/{{owner}}/{{package}}.git"1
```

### **综合**

.bowerrc

```shell
"proxy" : "socks5://127.0.0.1:1080",
"https-proxy": "socks5://127.0.0.1:1080",
"shorthand-resolver": "https://github.com/{{owner}}/{{package}}.git"
```