[TOC]



> 作为开源盛世的产物，Git可以说是程序员必会工具之一，**Git的强大在于越学越不会，哈哈**，但是想要使用GitHub等交友网站，当然得先过了Git这一关。：）
>
> 这是一篇长文，是我在学习Git时候的总结，夹杂着GitHub的使用和IDE中Git的操作，会随着学习不断进行补充。

![Git思维导图](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/2fsR0J2GNcn6zgaRqIEYQi9t2gDR7T9sJw4vaYQ08V0!/r/dEgBAAAAAAAA)



# Git

## 基本原理

### Git文件的四种状态

- **说是四种状态其实也可以说是三种状态，Unmodified和Modified都是Unstaged状态**![Git的四种状态](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/brc6Ajg2E6siXOPC8lKifjT4vkjAUJvTY*2vMEpi884!/r/dDUBAAAAAAAA)

Untracked状态是Git根本就不知道我们有这个文件，add后变成Staged状态，就可以提交了。提交之后就变成Unstaged状态中的Unmodified状态，修改文件后变成Modified状态，再次add又变成Staged状态可提交。往复循环。



### 哈希算法简介

哈希是一个系列的加密算法，各个不同的哈希算法虽然加密强度不同，但都有以下几个共同点

- 不管输入数据量多大，用同一个哈希算法加密，得到的加密结果长度固定
- 哈希算法固定，输入数据固定，输出数据不会有变化。
- 哈希算法固定，输入数据有变，输出数据一定变化，并且通常变化很大。

**Gtt底层采用的是SHA-1算法。**



### Git如何保证数据完整性？

原始文件经过SHA-1算法加密后得到加密结果。后期校验的时候再次通过SHA-1算法得到加密结果进行对比。以验证数据完整性。



### SVN等集中式版本控制工具保存机制

SVN等集中版本控制工具是只在每个版本保存有变化的文件，好处就是能节省存储空间。

![SVN](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/phegLHchNlqV*Ci03k1vkk2sPRHk8oE37vXzZ1fIfno!/r/dEgBAAAAAAAA)



### Git保存版本的机制

但是**Git**把数据看做是小型文件系统的一组**快照**， 每个版本都有全部文件的快照，基于上一版本，如果文件没有改动，文件**指针**就指向上一文件，这种工作方式称为**快照流**。

![Git](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Q7Mv6tS3BQBiit9y6jLnvp8xFg3hQsH2vRh4qBCI7Uw!/r/dDQBAAAAAAAA)



#### Git版本保存机制细节

![细节](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/JSoiMexH.MEVjaW80lIa8IwYDxsl8.62t4lYeM*9zg0!/r/dDcBAAAAAAAA)

Git的提交对象每一个文件都会产生一个哈希值，如上图的**5b1d3**和**911e7**这些所有文件哈希值构成一个**树**，树本身又会产生一个哈希值，在每次提交的时候就会带上这个树。



#### 提交对象及其父对象形成的链条

![链条](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/ibUX1Ne7hxiVYajMMzMVPP8.gdoARBHy0OzNtkJv7lM!/r/dFQBAAAAAAAA)

**一次一次的commit形成一张张快照**，如上快照ABC中提交信息中parent指向父节点。



### Git分支管理机制

> Git创建一个分支并不会像SVN那样将所有文件复制一套，只是创建了一个指针，并不涉及文件操作，所以创建分支操作很快，很高效。如下就是创建了一个**testing**指针

![分支创建](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/t9BUDvaS7pAjHjImSFki4NOY3rTwcJmkiGaCCGkjh54!/r/dDQBAAAAAAAA)

> Git分支切换操作只是将头指针的位置进行切换

![分支切换](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Ox55Vd8DQAzHxOSXDIgNQtMOwjBYa*rxIaixmhbJMGA!/r/dFMBAAAAAAAA)

> 在testing分支上进行一次提交

![提交一次](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/lSh3oAgwpAWtMORarELqMwxgqraQ5GG14QhxwhJxgcs!/r/dDIBAAAAAAAA)

> 在master分支上进行一次提交，可以看出来区别了。

![提交两次](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/zzoXiBXNFGX6AypLHnL9*PaWAkPwkivZDBzYMH2Plpk!/r/dDYBAAAAAAAA)



## 安装和配置

### linux

```
sudo apt-get install git-core
```

### 配置姓名和邮箱

- 带不带--global是区分项目级别和全局级别的区别
- 在~家目录下.gitconfig文件中可以找到该内容

```git
[filter "lfs"]
	clean = git-lfs clean -- %f
	smudge = git-lfs smudge -- %f
	process = git-lfs filter-process
	required = true
[user]
	name = ck
	email = 276128749@qq.com
```

```vim
git config --global user.name "ChengKun"
```

```vim
git config --global user.email "276128749@qq.com"
```

### 创建SSHKey

```vim
ssh-keygen -t rsa -C "youremail@example.com"
```

### 检验是否成功

```vim
ssh -T git@github.com
```



## 基本操作

### 初始化本地仓库

```vim
git init
```

### 添加到缓存区

```vim
git add 文件名
```

### 缓存区中移除

```vim
git reset 文件名
```

### 状态查看

```vim
git status
```

### 文件比较diff

比较多个文件

```vim
git diff  a  b
```

比较工作区和缓存区

```vim
git diff
```

比较工作区和本地库中的文件

```vim
git diff HEAD
//和某个历史版本进行比较
git diff HEAD~3
```

### 提交日志log命令

```vim
git log
```

只带--oneline参数就只会显示头指针

```vim
git log --oneline 
git log  --graph --oneline //直观显示
```

带上--pretty=oneline就会显示哈希值

```vim
git log --pretty=oneline
```

带上reflog参数会在--oneline**的基础上多显示(HEAD -> master) HEAD@{0}**，告诉我们移动到某个版本需要走几步

```vim
git reflog
```

### 基于索引值的版本前进后退

> 置于这里后退用到的数字大家可以自己去捋一下，如果不太懂就干脆用索引。

#### reset

- **在任何情况下，HEAD都是指向最近一次提交的一个指针** 

```vim
git reset --【参数】 索引值        //用法
```

还有利用**^ 和 ~**，但这两个都只能后退。

```vim
git reset HEAD              //最近一次提交
git reset HEAD^             //上一次提交
git reset HEAD~0            //最近一次提交
git reset HEAD~1            //上一次提交
git reset --hard HEAD~3     //回退三步
git reset --hard HEAD^^^    //回退三步
```

#### reset的三种参数比较

![reset三种参数比较](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/6ZQLHDceFVDs*DHm59wr8SsrPfL79aGBPhRzkZm.Thk!/r/dEgBAAAAAAAA)

--soft

> 只会使指针在本地库移动
>
> 如果只是轻微改动推荐使用此参数

- 如果只是想更改commit的提交信息使用`git commit --amend `即可。

--mix(默认)

> 会改变本地库和暂存区

**--hard(不推荐使用)**

> 本地库，缓存区，工作区都会改变。

再说一件事儿：**假设你销毁了一个commit**，但最后发现仍然需要它？很倒霉，是不是？

没那么严重，仍然有办法将它找回来。输入`git reflog`，然后你会看到一个提交信息列表，这里保存着所有之前的提交SHA（译者注：一种Hash算法）值。找到你销毁的commit，然后这么干:

```vim
git checkout -b someNewBranchName shaYouDestroyed
```

好了，现在你成功复活了那个提交。commits不会真正在Git中被销毁（大概90天），所以你通常能够回退并且拯救你之前除掉的那一个commit。 

### 删除文件找回

- 前提：删除前，文件存在时的状态提交到了本地库
- 操作：git reset --hard【指针位置】
  - 删除操作提交到本地库：指针指向历史版本（删除文件存在的版本）
  - 删除操作未提交到本地库，指针指向当前HEAD就行，因为之前就提交过文件，--hard对本地库，工作区，缓存区都会进行刷新。



## 分支管理

**什么是分支？**

在版本控制中，使用多条线同时推进多个任务。

#### 查看分支

```vim
git branch -v
git branch -l  :查看本地分支
git branch -r  :查看远程分支
git branch -a :查看全部分支，包括远程的和本地的（ -a 其实就是all 的意思）
```

#### 新建分支

```vim
git branch 分支名
```

#### 切换分支

```vim
切换分支： $ git checkout mybranch
创建并切换分支： $ git checkout -b mybranch
```

#### 删除分支

```java
git branch -d 分支名 //删除本地分支
git push origin --delete <BranchName> //删除远程分支
```

#### 合并分支

- 切换到被合并分支上，一般是切换到主分支上
- 执行merge命令

```vim
git merge 分支名

合并远程 dev 分支到当前分支
git merge origin/dev
```

#### 本地分支与远程分支关联

**一般我们就用`git push --set-upstream origin branch_name`来在远程创建一个与本地`branch_name`同名的分支并跟踪；利用`git checkout --track origin/branch_name`来在本地创建一个与`branch_name`同名分支跟踪远程分支。**

#### 冲突

##### 冲突表现

打开冲突文件会找到

- <<<<<<<<<< 

  <下面当前分支内容

- \====

  一排等号分割线

- \>>>>>>>>>>

  一排\>上面是另一分支内容

##### 冲突的解决

- 编辑文件，删除特殊符号
- 把冲突文件内容修改到满意，保存退出
- git add 文件名
- git commit -m ""
  - 此时commit 后不能带文件名



## 与远程仓库关联

### 创建SSH KEY

```vim
ssh-keygen -t rsa -C "youremail@example.com"
```

上面的用户是git 配置时候的用户，然后一路回车。这时你就会在用户下的**.ssh**目录里找到id_rsa和id_rsa.pub这两个文件。

### 在GitHub中添加SSH KEY

在setting中将**pub中的公开密钥**添加到里面去。

### 本地仓库和远程GitHub连接

```vim
$ git remote add 【远程仓库别名】 远程仓库地址
$ git remote add origin https://github.com/ck76/test
git remote rm 别名//删除本地远程地址
```

我们不能每次提交都带一大长串的远程仓库地址，所以就使用git remote add命令将远程仓库地址添加到本地，以方便使用。

- git remote -v命令查看当前所有远程地址别名

![remote -v](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/02sucAdu6LHHwgn7*eidvVajKsLTJ7C6bj827Tu0ULU!/r/dDUBAAAAAAAA)

### 代码提交

```java
origin是远程仓库地址，master是远程仓库地址的分支名，这里是主分支。
git push origin [本地分支名]:[远程分支名]//自定义提交分支
//如果直接写git push，是push当前分支到当前分支的追踪关系分支。 
//一般本地master分支，push到的是远程仓库的master分支。 
git push -u origin master
```

- **这时候有个坑，比如远端创建了个readme文件，直接提交会报错，先将本地与远端同步一下就OK啦~**

```vim
git pull --rebase origin master
```

### 拉取操作pull

>  **pull其实fetch+merge的合体**

- fetch只是将远程仓库文件下载下来，并**不会更改本地库中的文件内容**。如果想查看下载下来的内容，就用`git checkout origin/master`命令切换到远程仓库的主分支，然后cat一下该文件的内容。
- merge操作在确认好文件内容之后进行，切换回本地库的主分支用`git merge origin/master`命令执行合并操作

其实可以直接用`git pull origin master`，但是为了保险，进行复杂文件操作的时候推荐fetch和merge分开进行。



## Git工作流

### GitFlow工作流(最常用)

![Git工作流](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/x1qXGPR1K1u9rYegMTweEHnuVLFS9rMOcDqJBboWDHg!/r/dFIBAAAAAAAA)

**以下是对上面的图进行一下解释：**

首先创建一个**master**分支，创建**develop**分支进行开发工作，再创建各个分支用来同时开发实现不同的功能，如上图的**feature_**开头的两个分支，这时线上产品出问题了，创建热修复分支**hotfix**分支进行修复BUG并将hotfix分支分别合并到master分支和develop分支以解决两个分支上的问题，版本变更到V1.1，这时**feature_goldstyle**完成开发合并到develop分支，这时需要创建**release**分支进行上线前最后测试，有BUG再次修复，将修复后的release分支合并到主分支和develop分支上，game功能完成，顺利发布。



---

# GitHub

## 团队成员邀请

>  如果想要邀请其他人员加入项目的开发，如下几步就可以

- 被邀请人克隆项目到本地
- 项目管理人员在GitHub上该项目的settings中找到Collaborators搜索想要邀请的人的GitHub账号点击邀请
- GitHub会向管理人员发送一个地址，管理人员将改地址发送给被邀请人
- 被邀请人通过浏览器访问该地址，点击接受邀请即可加入团队

## 协同开发时冲突的解决

>  协同开发时也会产生冲突，其实解决方法和本地冲突解决方法很像

- 首先`git pull origin master`，将远程仓库内容抓取下来，此时进入(master|MERGIN)状态
- 修改冲突文件到满意，然后提交。就OK啦。

## 跨团队协作

> 跨团队协作在GitHub上很常见的

- 其余人fork一份
- 然后修改
- 发起pull request请求
- 原始仓库主人审核一下，进行merge

----

# 未完待续，后续补充IDEA中和Android Studio中操作.....

----

# GitLab

**GitLab是局域网环境内的代码托管中心，局域网不受网络状态的影响，即使连接不上外网也可进行版本控制。**

> GitLab 是一个用于仓库管理系统的开源项目，使用Git作为代码管理工具，并在此基础上搭建起来的web服务。安装方法是参考GitLab在GitHub上的Wiki页面。                     -----来自百度百科
>
> 官网地址： **[首页](https://about.gitlab.com/)      [安装说明](https://about.gitlab.com/)** 

## 搭建GitLab

- 此过程为CentOS7虚拟机搭建，安装说明里可以找到各种系统的搭建方法。

### **1.安装并配置必要的依赖项**

```vim
sudo yum install -y curl policycoreutils-python openssh-server
sudo systemctl enable sshd
sudo systemctl start sshd
sudo firewall-cmd --permanent --add-service=http
sudo systemctl reload firewalld
```

### 2.安装Postfix以发送邮件

(可选)2.接下来，安装Postfix以发送通知电子邮件。如果要使用其他解决方案发送电子邮件，请跳过此步骤并在安装GitLab后配置外部SMTP服务器

```vim
sudo yum install postfix
sudo systemctl enable postfix
sudo systemctl start postfix
```

### **3.添加GitLab软件包存储库并安装软件包**

- 添加GitLab包存储库。

```vim
curl https://packages.gitlab.com/install/repositories/gitlab/gitlab-ee/script.rpm.sh | sudo bash
```

- 接下来，安装GitLab包。将`http：// gitlab.example.com`更改为您要访问GitLab实例的URL。安装将自动配置并启动该URL的GitLab。HTTPS 在安装后需要**[其他配置](https://docs.gitlab.com/omnibus/settings/nginx.html#enable-https)**。

```vim
sudo EXTERNAL_URL =“http://gitlab.example.com”yum install -y gitlab-ee
```

###  **还可以将上面命令摘要写成一个脚本文件**

```vim
sudo yum install -y curl policycoreutils-python openssh-server cronie
sudo lokkit -s http -s ssh
sudo yum install postfix
sudo service postfix start
sudo chkconfig postfix on
curl https://packages.gitlab.com/install/repositories/gitlab/gitlab-ee/script.rpm.sh | sudo bash
sudo EXTERNAL_URL="http://gitlab.example.com" yum -y install gitlab-ee
```

### 启动

- CentOS可能需要关闭一下防火墙

```vim
service firewall stop
```

- 重启机器`reboot`
- GitLab配置

```vim
lab 服务操作
 初始化配置 gitlab
gitlab-ctl reconfigure
 启动 gitlab 服务
gitlab-ctl start
 停止 gitlab 服务
gitlab-ctl stop
```

- 浏览器访问Linux的IP地址就可以了

![GitLab搭建成功](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/IJND6IKN7iY*wIJjfMHhvpaXEaY6SQtisjpiKgoxw6U!/r/dFYBAAAAAAAA)