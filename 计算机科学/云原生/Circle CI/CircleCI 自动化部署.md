[TOC]

## 什么是 CircleCI

CircleCI 是一个持续集成/持续部署的服务，开源项目可以免费使用，他的价格取决于你需要并发构建实例的数量，单个实例是免费的。

## 添加 ssh 密钥

```
$ ssh host_name
$ cd ~/.ssh/
```

`$ ssh-keygen -t rsa `创建公钥(.pub)和私钥

`$ cat id_rsa` 复制私钥所有内容

要使 `CircleCI` 能够将文件发送到服务器，单击"构建"屏幕上项目名称旁边的 `cog` 来访问项目的构建设置。在设置屏幕上，找到权限部分下的 `SSH` 权限链接。单击“ 添加 `SSH` 密钥”按钮，然后添加服务器的主机名和私钥的内容



![img](https://pic1.zhimg.com/80/v2-7a51cae53ac473000bd3453148028378_1440w.jpg)gifhome_2878x1580



要将此密钥导入 `CI` 环境，我们使用 `add_ssh_keys` 添加 SSH `密钥后，CircleCI` 在设置屏幕上显示密钥的指纹。复制此指纹并将其插入上述命令，我们的构建环境现在可以使用该密钥。



![img](https://pic3.zhimg.com/80/v2-f1b210af7d73efff0e943a8e6ee192f6_1440w.jpg)image



## 提供 Hostkey 进行验证

当 `CircleCI` 服务器尝试连接到您的主机时，它可能会提示您验证主机密钥。提示是自动化的氪星石：`CI` 环境将无限期挂起，等待用户给出响应。可以禁用此验证，但将正确的密钥添加到 `known_hosts` 文件是一个更好的解决方案。`CircleCI` 没有用于添加主机密钥的语义解决方案，但一种选择是将主机密钥添加为环境变量。要添加环境变量，请转到项目设置，然后单击"构建设置"下的"环境变量”链接。单击“ 添加变量”按钮，然后添加一个以 `REMOTE_HOSTKEY` 服务器主机密钥内容命名的新值。要获取主机密钥，请在终端中运行以下



![img](https://pic1.zhimg.com/80/v2-dffacf227118df85232eb00c29a10e9c_1440w.jpg)image



复制下`ip ecdsa-sha2-nistp256` 这个开头的所有内容，`$ cd ~/.ssh/`粘贴到 `known_hosts`

## 新建环境变量

在这个页面进行新建环境变量



![img](https://pic1.zhimg.com/80/v2-2371d421fdd5daac467f2b0a0affe884_1440w.jpg)image



```text
$REMOTE_HOSTKEY   //刚复制的主机密钥
$SSH_USER     // 服务器用户名
$SSH_IP           // 服务器ip
```



![img](https://pic4.zhimg.com/80/v2-8d210ddba26ca5516cc7c23128ed22c3_1440w.jpg)image



然后我们可以在构建环境中使用此变量将主机密钥添加到 `known_hosts` 文件中

## 部署命令



![img](https://pic4.zhimg.com/80/v2-88d0ad198ba531cddba01f0023c68757_1440w.jpg)image



剩下的就是将文件传输到远程服务器上
针对 `master` 分支进行

## 最终配置文件

```text
version: 2.0
jobs:
  build:
    docker:
      - image: circleci/node:latest
    working_directory: ~/circleci-demo-workflows
    steps:
      - checkout
      - run: sudo npm install -g npm@6
      - run: npm install
      - save_cache:
          key: v1-dependencies-{{ checksum "package.json" }}
          paths:
            - node_modules
      - run: npm run build
      - run: echo '部署开始'
      - run: sudo apt-get update && sudo apt-get install rsync
      - restore_cache:
          keys:
            - v1-dependencies-{{ checksum "package.json" }}
            - v1-dependencies
      - add_ssh_keys:
          fingerprints:
            - "bd:dd:23:90:d7:86:80:d8:92:31:1b:41:09:09:27:87"
      - run: echo $REMOTE_HOSTKEY >> ~/.ssh/known_hosts
      - deploy:
          name: deploy
          command: |
            if [ "${CIRCLE_BRANCH}" = "master" ]; then
              rsync -avce ssh build $SSH_USER@$SSH_IP:/data/corki-ui-web/
            else
              echo "Not master branch, dry run only"
            fi
      - run: echo '部署完毕'
workflows:
  version: 2
  scheduled-workflow:
    triggers:
      - schedule:
          cron: "0 0 * * *"
          filters:
            branches:
              only: master
    jobs:
      - build
```

代码流程：

- 安装特定版本 `npm`， `sudo npm install -g npm@6`
- 安装 `npm`包 `npm install`
- 缓存文件 `save_cache`
- 打包 `npm run build`
- 安装传输命令 `sudo apt-get update && sudo apt-get install rsync`
- 恢复缓存 `key``restore_cache`，需先缓存
- 添加 `ssh` 密钥 `add_ssh_keys`
- 推送文件到服务器 `deploy`

## 部署



![img](https://pic3.zhimg.com/80/v2-7ace47937e7d0e0d7997aa03e7146022_1440w.jpg)image



- 选择环境
- 选择 `docker` 镜像(这里以 `node` 为例)
- 开始构建

## 例子



![img](https://pic3.zhimg.com/80/v2-ef277450d9d7d76952e195c7b2e1e576_1440w.jpg)image



## 总结

超级简单有木有，部署完成后，只要 `master` 分支有了改动，就会自动发布，哈哈哈。再也不用去手动发布了。



---

# CircleCI 配置第一章: Docker镜像 Image 配置 - 使用 CircleCI Convenience Image

# 前言

在使用 CI 工具进行持续集成的时候, 免不了要定义配置文件, 并在配置文件中定义项目的工作流程. Circle 是一个对开源项目友好的 CI 工具, 本文将以 **CircleCI** 入手, 解析配置文件中定义 Docker 镜像的配置项: **image**字段.

对于大多数初次尝试使用 CircleCI 的用户, 最难以拿捏的莫过于使用 docker 镜像时, 配置中 docker 字段下 image 的定义, 本文将介绍**如何配置 image 选项 - 使用 CircleCI Convenience Image**, 以及与之相关配置的**最佳实践**.

------

# Convenience Image 的两种类型



![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/5/29/16b00cdc694717e9~tplv-t2oaga2asx-watermark.awebp)

CircleCI 为我们提供了**语言类镜像**和**服务类镜像**两种类型的 Convenience Image, 在这些镜像中预设了很多依赖, 可以为我们进行持续集成提供便利.



### 语言类镜像

语言类镜像内置了某种语言相关的依赖和工具.

**语言类镜像需要在 docker 字段下的第一个 image 字段中定义**, 作为执行时的**首要容器**.

##### 语言类镜像支持的语言列表:

- Android
- Clojure
- Elixir
- Go (Golang)
- JRuby
- Node.js
- OpenJDK (Java)
- PHP
- Python
- Ruby
- Rust

##### 格式

```
docker: 
    -image: circleci/language:version[-tag]
复制代码
```

##### 配置示例

```
docker:
    -image: circleci/golang:1.9
复制代码
```

### 语言类镜像变体

在语言类镜像的基础上, CircleCI 还提供了这类镜像的变体, 即在提供了某种语言依赖的基础上增加了更多的依赖, 预装了其他的语言和工具

通过添加以下后缀, 就可以使用在某种 Convenience Image 的基础上预置了更多依赖的镜像

- -node
  - 添加了 Nodejs 相关依赖
- -browsers
  - 添加了 浏览器 相关依赖
  - *includes Chrome, Firefox, Java 8, and Geckodriver*
- -browsers-legacy
  - 大致同上, 略有区别
  - *includes Chrome, Firefox, Java 8, and PhantomJS*
- -node-browsers
  - 同时添加了 -node 变体 和 -browser 镜像变体所提供的依赖
- -node-browsers-legacy
  - 同时添加了 -node 变体 和 -browsers-legacy 镜像变体所提供的依赖

##### 配置示例

```
- image: circleci/node:10-jessie-browsers
复制代码
```

### 服务类镜像

服务类镜像主要是为项目提供服务 (例如数据库服务) 时使用的.

它们必须要在语言类镜像后面定义, 作为**次要镜像**.

CircleCI 为以下服务提供了服务类镜像

- buildpack-deps
- DynamoDB
- MariaDB
- MongoDB
- MySQL
- PostgreSQL
- Redis

##### 配置示例

```
- image: circleci/mongo:4.1.7-xenial
复制代码
```

### 服务类镜像变体

CircleCI 仅仅为服务类镜像提供了一种变体, -ram

##### 配置示例

```
- image: circleci/postgres:9.5-postgis-ram
复制代码
```

------

# 配置镜像的最佳实践 Best Practices



![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/5/29/16b00ccbce173a40~tplv-t2oaga2asx-watermark.awebp)

配置 CircleCI Convenience Image 的最佳实践是**定义最精确版本的镜像**.



这是因为 CircleCI Convenience Image 是基于最新版本的上游镜像制作的, 例如镜像 `circleci/ruby:2.4-node` 是基于最新的 `Ruby 2.4-node` 容器镜像制作的.

使用最精确的镜像可以避免由于上游的镜像更新而导致的 Convenience Image 的更新从而所带来的不稳定性, 从而保证项目运行环境的稳定性.

**配置 Image 的最佳实践就是通过一个额外的标签来固定镜像的版本.**

也就是说, 与其使用 circleci/ruby:2.4-node 镜像, 不如加上一个标签 -jessie 或 -stretch 来固定镜像所用的操作系统, 来确保项目使用的镜像是基于某个特定版本的 Debian 系统 ( 例如 circleci/ruby:2.3.7-jessie )

我们也可以通过指定镜像的 SHA 版本来使用最精确的镜像.

### 使用精确镜像的两种方法:

1. 使用一个标签来固定镜像的系统类型
2. 使用一个明确的 Docker 镜像 ID

```
# 最佳实践示例
# 示例 1
# 使用固定系统的镜像
- image: circleci/ruby:2.4.2-jessie-node



# 示例 2
# 使用固定版本镜像
- image:circleci/ruby@sha256:df1808e61a9c32d0ec110960fed213ab2339451ca88941e9be01a03adc98396e
复制代码
```

# 所有 Convenience Image 镜像的可用后缀

请查询该文件: [circleci.com/docs/2.0/do…](https://link.juejin.cn/?target=https%3A%2F%2Fcircleci.com%2Fdocs%2F2.0%2Fdocker-image-tags.json)

# 示例配置

一个应用了 Node.js 语言镜像和 MongDB 服务镜像的 CircleCI 项目配置文件

```
# .circleci/config.yml
version: 2.1

jobs:
  build:
    docker:
      - image: circleci/node:10.13-jessie-browsers
      - image: circleci/mongo:4.0.4-xenial-ram

    steps:
      - run: echo "A first hello"
复制代码
```



----

## CircleCI 入门

最近在完成老师的作业：使用 CircleCI 进行持续集成，现将过程书写下来，与大家一起学习进步。

------

### A. What is CircleCI？

##### 1. 什么是持续集成

持续集成（Continuous Integration）通常缩写为 CI，
持续集成指的是，当代码有变更时，立即进行构建和测试，反馈运行结果，我们可以根据测试结果，确定新代码是否可以和原有代码正确的集成在一起。
让你能够在开发中随时发现问题，在快速的产品迭代中还可以保持很高的质量。因为修复问题的成本随着时间的推移而增长，越早发现，修复成本越低。

##### 2. 什么是持续部署

持续部署（Continuous Deployment）通常缩写为 CD，
持续部署指的是，当代码有变更时，自动进行测试和构建，如果一切顺利则自动部署到服务器上。

##### 3. 什么是 CircleCI

CircleCI 是一个持续集成/持续部署的服务，开源项目可以免费使用，他的价格取决于你需要并发构建实例的数量，单个实例是免费的。

[官网地址](https://circleci.com/)

##### 4. CircleCI 能做什么

他可以绑定 GitHub/Bitbucket，只要你的代码有变更，就会自动抓取，根据你的配置，提供运行环境，执行测试、构建和部署。

##### 5. CircleCI 的工作流程

![img](https://upload-images.jianshu.io/upload_images/9197037-58db3da8ab8da8a9.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

------

### B. 使用 CircleCI 持续集成和自动部署到 GitHub，进行 junit 单元测试

建议完成该实验最好在 Linux 环境下进行，能避免很多问题，CircleCI 的构建环境也只有 Linux 和 MacOS，并不包含 Windows。

如果是 Windows 系统，这里我建议使用 WSL，简单方便快捷，还特别好用，毕竟是最强的 Linux 发行版（滑稽）。

##### 1. 创建测试目录

创建一个名为 `circleTest` 的测试文件夹，移动至该文件夹下。

```
mkdir circleTest
cd circleTest
```

##### 2. 单元测试

我们先在本地进行单元测试。

首先导入 maven 依赖：

```xml
<dependencies>
    <dependency>
        <groupId>junit</groupId>
        <artifactId>junit</artifactId>
        <version>4.12</version>
    </dependency>
</dependencies>
```

建立测试文件：

```java
package test;

import org.junit.Assert;
import org.junit.Test;

public class TestClass {

    @Test
    public void testName() {
        Assert.assertEquals(1, 1);
    }

}
```

本地执行一下：

![img](https://upload-images.jianshu.io/upload_images/9197037-46cd2ab9b222b0ca.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

可以看到没有问题，我们将 java 项目导出为 jar 包。运行命令：

```
mvn package
```

复制 target 目录下的 jar 文件到之前创建好的 circleciTest 目录下：

![img](https://upload-images.jianshu.io/upload_images/9197037-f0a0cb36fc07e1ee.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

然后在 circleciTest 目录下执行命令：

```
java -cp ./RunUt-1.0-SNAPSHOT.jar:RunUt/* org.junit.runner.JUnitCore test.TestClass
```

查看测试用例通过。

##### 3. CircleCI 本地配置

要使用 CircleCI，首先在项目 circleciTest 目录下创建一个名为 `.circleci` 的文件夹，并新建 `config.yml` 文件。

config.yml：

```yml
version: 2
jobs:
  build:
    docker:
      - image: circleci/openjdk:8u181-jdk
    steps:
      - checkout
      - run:
          name: Testing application
          command: java -cp ./RunUt-1.0-SNAPSHOT.jar:RunUt/* org.junit.runner.JUnitCore test.TestClass
```

关于该配置文件的详细信息，可以参考 [官方文档](https://circleci.com/docs/2.0/#section=welcome)

##### 4. 使用 GitHub 实现持续集成

进入 CircleCI 官网，点击 Explore Integrations，跳转后选择 Start Building for Free，然后选择 Sign Up With GitHub，与自己的GitHub 账号进行相关联。

关联后便进入控制界面。

![img](https://upload-images.jianshu.io/upload_images/9197037-4fa64890d0d35317.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

##### 5. 上传测试文件到 GitHub

在 GitHub 建立一个新的 repository ，关联本地仓库，进入本地的 circleciTest 目录，执行以下命令：

```
git init
git add ./
git commit -m "first commit"
git push origin master
```

##### 6. 更改 GitHub 设置选项

将本地代码上传至远程仓库后，我们点击 Settings，选择 Webhooks 选项，再点击 Edit 按钮：

![img](https://upload-images.jianshu.io/upload_images/9197037-6ecf8c39b7569eb9.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

选择 Just the push event：

![img](https://upload-images.jianshu.io/upload_images/9197037-953c7f6e31e8c20e.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

##### 7. 使用 CircleCI 进行单元测试

在 CircleCI 控制面板选择 Add Project，然后关联之前上传的 circleciTest 项目：

![img](https://upload-images.jianshu.io/upload_images/9197037-1b24509273c4fdc6.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

如果我们重新提交了代码，可以在 GitHub 仓库下的 commits 界面下看到我们的提交记录，并且 CircleCI 会实现自动部署，可以很直观的查看到提交的代码是否正确通过运行：

![img](https://upload-images.jianshu.io/upload_images/9197037-1424f9b4e5a1d4d7.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

点击 Details 进行 CircleCI 该项目的设置界面，我们便能完整的看到运行情况了：

![img](https://upload-images.jianshu.io/upload_images/9197037-10b47288a60915a0.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

测试通过：Success!

------

### C. 测试用例

[circleciTest](https://github.com/weixuqin/circleciTest)

------

### 参考

https://www.jianshu.com/p/36af6af74dfc

---

## 什么是持续集成

持续集成（Continuous Integration）通常缩写为 CI，
 持续集成指的是，当代码有变更时，立即进行构建和测试，反馈运行结果，我们可以根据测试结果，确定新代码是否可以和原有代码正确的集成在一起。
 让你能够在开发中随时发现问题，在快速的产品迭代中还可以保持很高的质量。因为修复问题的成本随着时间的推移而增长，越早发现，修复成本越低。

## 什么是持续部署

持续部署（Continuous Deployment）通常缩写为 CD，
 持续部署指的是，当代码有变更时，自动进行测试和构建，如果一切顺利则自动部署到服务器上。

## CircleCI 是什么

CircleCI 是一个持续集成/持续部署的服务，开源项目可以免费使用，他的价格取决于你需要并发构建实例的数量，单个实例是免费的。

## CircleCI 能做什么

他可以绑定 GitHub/Bitbucket，只要你的代码有变更，就会自动抓取，根据你的配置，提供运行环境，执行测试、构建和部署。

## CircleCI 的工作流程

![img](https:////upload-images.jianshu.io/upload_images/1374890-e20a246ec932c050.png?imageMogr2/auto-orient/strip|imageView2/2/w/1023/format/webp)

image.png

# PHP 项目 使用CircleCI 自动部署到 AWS ECS

## 一. 前期准备

> - GitHub/Bitbucket 帐号
> - 该帐号下面有一个项目

## 二. CircleCI 配置

### 1.添加配置文件

要使用 CircleCI，首先在你项目的根目录创建一个名为`.circleci`的文件夹，并新建`config.yml`文件

### 2.在 CircleCI 设置你的构建环境

打开 [CircleCI  控制台](https://circleci.com/dashboard) 选择左侧的 **Add Project** , CircleCI 会列出你的 GitHub/Bitbucket 的所有项目，选择项目并单击 Set Up Project 按钮

![img](https:////upload-images.jianshu.io/upload_images/1374890-a7e2a8dca11ff5ec.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

选择合适的操作系统(Operating System)、合适的编程语言(Language) (本文使用的是 Linux/PHP)

复制下面的示例配置文件到你自己的配置文件中，推送到 git 仓库，然后就可以点击 **Start building** 进行第一次构建了。

![img](https:////upload-images.jianshu.io/upload_images/1374890-faef104e62b3d4f1.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

## 三. CircleCI 配置文件

CircleCI 配置文件一般由三部分组成

- 版本 (version)
  - 你要使用的 CircleCI 版本
- 工作 (jobs)
  - 你要执行的 job 清单，集合中的键为 job 的名称，值是具体执行 job 的内容，如果你使用工作流(workflows)，则 job 的名称在配置文件中必须唯一，如果你不使用 工作流(workflows)，则必须包含名称为`build`的 job 来作为用户提交代码时的默认 job。
- [工作流(workflows)](#工作流)

### 工作 (jobs)

首先我们来先写一个基础的配置文件



```yaml
# PHP CircleCI 2.0 configuration file
#
# Check https://circleci.com/docs/2.0/language-php/ for more details
#
version: 2
jobs:
  build:
    docker:
    - image: circleci/php:7.1-browsers
    # 必要时可以在这里指定依赖镜像
    # - image: circleci/mysql:9.4

    working_directory: ~/repo

    steps:
    - checkout
    - run: composer install -n --prefer-dist

    # 运行单元测试
    - run: phpunit
  deploy:
    docker:
    - image: circleci/python:3.6.1

    working_directory: ~/repo

    steps:
    
```

在上面的列子中，有两个 job 叫 build/deploy，现在讲解一下 job 里面的属性

- docker 键是用来指定 CircleCI 当前 job 使用 docker, 其值`image`是指 docker 所使用的镜像，必要时你可以同时指定多个镜像，比如你的项目需要依赖 mysql 或者 redis。 第一个列出的容器为主容器，`steps` 都会在主容器中进行。
  - CircleCI 预先定义了很多镜像，您可以在 [此处](https://circleci.com/docs/2.0/circleci-images/) 找到它们
  - 如果不想使用 docker 也可以使用 machine / macos 具体使用方法参考 [这里](https://circleci.com/docs/2.0/configuration-reference/#machine)
  - 关于 docker 键的其他属性与用法 你可以在 [此处](https://circleci.com/docs/2.0/configuration-reference/#docker) 找到
- working_directory 属性是用来定义`steps` 在哪个目录运行
- steps 当前 job 要运行的 命令 (command) 列表

### 步骤 (steps)

steps 将负责对环境的初始化，与项目的构建、部署和测试:

#### 一. 构建、测试

1. 检出代码

   

   ```yaml
   # 将分支中的代码检出到 working_directory 
   - checkout
   # 让步骤中可以调用其他 docker
   - setup_remote_docker
   ```

2. 从缓存中恢复 composer 依赖目录

   

   ```yaml
   - restore_cache:
       keys:
       - v1-dependencies-{{ checksum "composer.json" }}
       # 如果没有匹配的缓存则使用最新的缓存
       - v1-dependencies-
   ```

   我们可以使用缓存功能来避免每次都重新 composer install，可以节约大量时间

   

   ```yaml
   {{ checksum "filename" }} 
   ```

   这部分指的是，给 filename 这个文件的文件内容 Base64 后取 SHA256 hash
    其他模版语法请查看 [官方文档](https://circleci.com/docs/2.0/configuration-reference/#save_cache)

   - 我们使用了两个缓存的 key 第一个是精确匹配设置的缓存 key，第二个是当用户修改 composer.json 文件时，我们不能精确匹配缓存，这时候恢复最近的一次缓存

3. 安装依赖

   

   ```yaml
   - run:
       name: Install local dependencies
       command: composer install -n --prefer-dist
   ```

   如果上一步恢复缓存的时候已经恢复了这些依赖项，则这步将非常快。

4. 缓存依赖

   

   ```yaml
   - save_cache:
       paths:
       - ./vendor
       key: v1-dependencies-{{ checksum "composer.json" }}
   ```

   经历了上一步,我们就会有一份当前版本完整的依赖目录，路径为`vendor`,这时我们把它缓存起来方便下次使用

5. 测试

   

   ```yaml
   - run:
       name: Testing
       command: phpunit
   ```

   我们运行测试的命令，如果测试有任何不通过则本次构建将失败。

6. 打包 docker 镜像

   

   ```yaml
   - run:
       name: Build image
       command: |
           docker build -t $FULL_IMAGE_NAME .
           mkdir docker-image
           docker save -o docker-image/image.tar $FULL_IMAGE_NAME
   ```

   打包 docker 镜像并命名为 `$FULL_IMAGE_NAME`，并将镜像 保存成 tar 归档文件

7. 运行并简单测试镜像

   

   ```yaml
   - run:
       name: Test image
       command: |
         docker run -d -p 8080:80 --name built-image $FULL_IMAGE_NAME
         sleep 10
         docker run --network container:built-image byrnedo/alpine-curl -I --retry 10 --retry-connrefused http://localhost
   ```

   运行刚才打包好的镜像，然后使用 curl 对打包好的镜像进行简单测试

8. 保存镜像到是临时文件

   

   ```yaml
   - persist_to_workspace:
       root: .
       paths:
       - docker-image
   ```

   保存刚才镜像 tar 归档文件到 `workspace`，以便 build job 使用

#### 二. 部署

1. 检出代码

2. 加载构建好的 docker 镜像

   

   ```yaml
   - attach_workspace:
       at: workspace
   - run:
       name: Load image
       command: |
         docker load --input workspace/docker-image/image.tar
   ```

   挂载 `workspace` 到当前 job, 挂载后 当前 job 的 `workspace/docker-image/image.tar` 为上一步打包出的 docker 镜像

   使用 `docker load` 导入镜像

3. 安装 aws cli

   

   ```yaml
   - restore_cache:
       key: v1-{{ checksum "requirements.txt" }}
   - run:
       name: Get Aws-cli
       command: |
       python3 -m venv venv
       . venv/bin/activate
       pip install -r requirements.txt
   - save_cache:
       key: v1-{{ checksum "requirements.txt" }}
       paths:
       - "venv"    # Download and cache dependencies
   ```

   安装 aws cli 到 python venv 环境

4. 推送镜像到 aws ecr

   

   ```yaml
   - run:
       name: Push Docker Image
       command: |
         . venv/bin/activate
         $(aws ecr get-login --no-include-email)
         docker tag mobingi-api-cn $AWS_ACCOUNT_ID.dkr.ecr.$AWS_DEFAULT_REGION.amazonaws.com.cn/mobingi-api-dev:$CIRCLE_SHA1
         docker push $AWS_ACCOUNT_ID.dkr.ecr.$AWS_DEFAULT_REGION.amazonaws.com.cn/mobingi-api-dev:$CIRCLE_SHA1
   ```

   推送 docker 镜像到 aws ecr

   为了使 aws 能正常登陆，docker 镜像能正确 push 到 ecr，你需要在 CircleCI 当前项目中设置环境变量([设置方法](#设置项目环境变量))

   | 变量名称              | 变量值                 |
   | --------------------- | ---------------------- |
   | AWS_ACCOUNT_ID        | AWS_ACCOUNT_ID         |
   | AWS_ACCESS_KEY_ID     | AWS_ACCESS_KEY_ID      |
   | AWS_DEFAULT_REGION    | AWS CLI 默认使用的地区 |
   | AWS_SECRET_ACCESS_KEY | AWS_SECRET_ACCESS_KEY  |

**更多 steps 信息请查看 [官方文档](https://circleci.com/docs/2.0/configuration-reference/#steps)**

# 工作流 (workflows)

用于编排所有 job。假设您需要在特定分支或特定时间运行job，或者您希望某些 job 并行运行，而某些 job 依次运行。

在工作流配置中，使用工作流名称作为配置的键。工作流名称在当前配置文件中必须全局唯一。

下面是工作流使用的例子



```yaml
workflows:
  version: 2
  nightly:
    triggers:
    - schedule:
        cron: "0 0 * * *"
        filters:
          branches:
            only:
            - master
            - beta
    jobs:
    - test
```

`nightly` 为工作流名称

`schedule` 可以指定工作流在指定时间工作

`cron`  使用POSIX定义crontab语法

`filters->branches` 过滤的分支

- 任何符合`only`条件的分支都会运行该工作流
- 任何符合`ignore` 条件的分支都不会运行该工作流
- 如果未定义 `only` 和 `ignore` 则所有分支豆浆运行该工作流
- 如果同时符合`only` 和 `ignore` 优先考虑 `ignore`

根据上面的介绍我们可以将我们前面的两个 job build 和 deploy 编排成一个工作流 (workflows) 并命名为：build-deploy



```yaml
workflows:
  version: 2
  build-deploy:
    jobs:
    - build
    - deploy:
        requires:
        - build
```

更多信息请查看 [官方文档](https://circleci.com/docs/2.0/workflows/#)

# 设置项目环境变量

因为有很多值不方便出现在 CircleCI 的配置文件中，例如一些密匙、服务 IP 等等。这时候我们就可以在 CircleCI 的管理面板中设置环境变量，然后在 job 获取这些变量。

1.在项目的的左上角，选择这个⚙️按钮

![img](https:////upload-images.jianshu.io/upload_images/1374890-4b4c402986c192b2.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

2.选择 **Environment Variables** -> **Add Variable**

在弹出的框中输入环境变量的 name 和 value 即可

# 使用 ssh 调试

当出现问题时，我们可能需要对问题进行调试，这时后我们可以 ssh 到 job 中对问题进行排查。

在出现问题的 job 右上角，在 Rebuild 下拉菜单中选择 Rebuild with SSH



![img](https:////upload-images.jianshu.io/upload_images/1374890-ce9d62ecd76a6c5c.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png

稍等几分钟后

![img](https:////upload-images.jianshu.io/upload_images/1374890-fbc96a3e0695bcda.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

image.png



使用红圈中的命令即可使用 SSH 登陆当前 job 环境

SSH 将保持可用状态10分钟,然后自动关闭。

更多信息请查看 [官方文档](https://circleci.com/docs/2.0/ssh-access-jobs/#section=jobs)

# 本地测试 CircleCI 配置文件

1. [创建个人API令牌](https://circleci.com/docs/2.0/managing-api-tokens/#creating-a-personal-api-token)  (**dashboard**->**User Settings**->**Personal API Tokens**->**Create New Token**)
2. 设置环境变量 export CIRCLE_TOKEN=<你刚刚创建的 token>
3. 收集以下信息：

- 提交构建的哈希值
- 用户名
- 项目来源
- 项目名
- 从哪个分支建立

1. 在`.circleci` 目录,创建 shell 脚本`run-build-locally.sh` 文件，文件内容为



```bash
#!/usr/bin/env bash
curl --user ${CIRCLE_TOKEN}: \
    --request POST \
    --form revision=<commit hash>\
    --form config=@config.yml \
    --form notify=false \
        https://circleci.com/api/v1.1/project/<source, eg. github>/<user name>/<project name>/tree/<branch name>
```

更多信息请查看 [官方文档](https://circleci.com/docs/2.0/examples/)

# 其他资料

[最终配置](https://gist.github.com/hooklife/e2ec16aaddd69bf69a9dffc8a75e8b3e)

[CircleCI 官方入门简介](https://circleci.com/docs/2.0/getting-started/)

[CircleCI 官方各种语言例子](https://circleci.com/docs/2.0/tutorials/)

[CircleCI 官方配置说明](https://circleci.com/docs/2.0/configuration-reference/)

[CircleCI官方例子 circleci-demo-aws-ecs-ecr](https://circleci.com/docs/2.0/ecs-ecr/#section=deployment)

[第三方参考例子 docker-circleci-ecr-ecs](https://github.com/ricktbaker/docker-circleci-ecr-ecs/blob/master/.circleci/config.yml)

# 参考文章

[如何理解持续集成、持续交付、持续部署？](https://www.zhihu.com/question/23444990)

[持续集成服务 Travis CI 教程](http://www.ruanyifeng.com/blog/2017/12/travis_ci_tutorial.html)

[How CircleCI Works](https://circleci.com/product/)

[How we used CircleCI 2.0 to build and deploy an Angular app to AWS S3](https://circleci.com/product/)



作者：hooklife
链接：https://www.jianshu.com/p/36af6af74dfc
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

----

### 前期

我是使用Bitbucket开发的，也不知道需要不需要过墙，反正我开电脑就开着梯子。如果有需要了解github的同学也可以瞧瞧，不妨碍。
 添加项目的一些操作就不聊了，下面主要说说配置文件的事情。

### 概念

##### Orbs

orbs是项目之间共享配置包。

##### Jobs

Jobs是一系列的步骤，job里的所有步骤都是circleCI容器中的独立单元。

##### Steps

Steps是在job运行期间的可执行命令的集合。

### 安全策略

SSH和HTTPS技术来加密网络请求和服务。所有jobs都在一个沙箱内工作，外网无法访问。通过SSH来获取执行代码，特定的测试和配置文件会从外网获取到沙箱内供代码使用。job完成后会销毁相关的容器，下个任务会重新构建。环境变量通过[Hashicorp Vault](https://www.vaultproject.io/)加密，采用AES256-GCM96加密变量，CircleCI无法获取。

> 提示：用户可以完全访问在构建容器中运行的任何文件或进程，因此仅向那些信任您的源代码的用户提供对CircleCI的访问权限。

##### 采用的第三方技术

采用[Pusher](https://pusher.com/)来完成服务跟游览器的交互任务。在安装Pusher时采用了[slanger](https://github.com/stevegraham/slanger)封装，所以Pusher服务无法访问CircleCI或者您的代码。
 采用[Replicated](http://www.replicated.com/)管理CircleCI的安装向导，许可密钥，系统审核日志，软件更新以及其他维护和系统任务

##### 审核日志事件

这部分内容是部署到自己的服务或者私有云上的，这里就不谈了。

### 开始   （适用于2.0以上版本）

具体的等级结构：

- [**`version`**](https://circleci.com/docs/2.0/configuration-reference/#version)
- [**`orbs`** (requires version: 2.1)](https://circleci.com/docs/2.0/configuration-reference/#orbs-requires-version-21)
- [**`commands`** (requires version: 2.1)](https://circleci.com/docs/2.0/configuration-reference/#commands-requires-version-21)
- [**`executors`** (requires version: 2.1)](https://circleci.com/docs/2.0/configuration-reference/#executors-requires-version-21)
- **`jobs`**
  - **<`job_name`>**
    - [`environment`](https://circleci.com/docs/2.0/configuration-reference/#environment)
    - [`parallelism`](https://circleci.com/docs/2.0/configuration-reference/#parallelism)
    - [**`docker`** / **`machine`** / **`macos`**(*executor*)](https://circleci.com/docs/2.0/configuration-reference/#docker--machine--macosexecutor)
    - [**`branches`**](https://circleci.com/docs/2.0/configuration-reference/#branches)
    - [**`resource_class`**](https://circleci.com/docs/2.0/configuration-reference/#resource_class)
    - **`steps`**
      - **`run`**
        - [*Default shell options*](https://circleci.com/docs/2.0/configuration-reference/#default-shell-options)
        - [*Background commands*](https://circleci.com/docs/2.0/configuration-reference/#background-commands)
        - [*Shorthand syntax*](https://circleci.com/docs/2.0/configuration-reference/#shorthand-syntax)
        - [The `when` Attribute](https://circleci.com/docs/2.0/configuration-reference/#the-when-attribute)
        - [*Example*](https://circleci.com/docs/2.0/configuration-reference/#example)
      - **The `when` Step** (requires version: 2.1)
        - [*Example*](https://circleci.com/docs/2.0/configuration-reference/#example-1)
      - [**`checkout`**](https://circleci.com/docs/2.0/configuration-reference/#checkout)
      - [**`setup_remote_docker`**](https://circleci.com/docs/2.0/configuration-reference/#setup_remote_docker)
      - **`save_cache`**
        - [*Example*](https://circleci.com/docs/2.0/configuration-reference/#example-2)
      - **`restore_cache`**
        - [*Example*](https://circleci.com/docs/2.0/configuration-reference/#example-3)
      - **`deploy`**
        - [*Example*](https://circleci.com/docs/2.0/configuration-reference/#example-4)
      - **`store_artifacts`**
        - [*Example*](https://circleci.com/docs/2.0/configuration-reference/#example-5)
      - **`store_test_results`**
        - [*Example*](https://circleci.com/docs/2.0/configuration-reference/#example-6)
      - [**`persist_to_workspace`**](https://circleci.com/docs/2.0/configuration-reference/#persist_to_workspace)
      - *Example for root Key*
        - [*Example for paths Key*](https://circleci.com/docs/2.0/configuration-reference/#example-for-paths-key)
      - **`attach_workspace`**
        - [*Example*](https://circleci.com/docs/2.0/configuration-reference/#example-7)
      - [**`add_ssh_keys`**](https://circleci.com/docs/2.0/configuration-reference/#add_ssh_keys)
- **`workflows`**
  - [**`version`**](https://circleci.com/docs/2.0/configuration-reference/#version-1)
  - **<`workflow_name`>**
    - **`triggers`**
      - **`schedule`**
        - [**`cron`**](https://circleci.com/docs/2.0/configuration-reference/#cron)
        - [**`filters`**](https://circleci.com/docs/2.0/configuration-reference/#filters)
    - **`jobs`**
      - **<`job_name`>**
        - [**`requires`**](https://circleci.com/docs/2.0/configuration-reference/#requires)
        - [**`context`**](https://circleci.com/docs/2.0/configuration-reference/#context)
        - [**`type`**](https://circleci.com/docs/2.0/configuration-reference/#type)
        - [**`filters`**](https://circleci.com/docs/2.0/configuration-reference/#filters-1)
        - [*Example*](https://circleci.com/docs/2.0/configuration-reference/#example-8)

------

1.项目根目录下需要如下格式的配置文件



```undefined
mkdir .circleci && touch .circleci/config.yml
```

2.版本声明



```cpp
version: 2.1  //版本号
```

3.orbs 配置包管理 这个包管理是2.1的特性 2.1以下无法使用



```bash
version: 2.1
orbs:
    hello: circleci/hello-build@0.0.5
workflows:
    "Hello Workflow":
        jobs:
          - hello/hello-build
```

4.commands sh指令 版本2.1以上



```go
commands:
  sayhello:
    description: "A very simple command for demonstration purposes"
    parameters:
      to:
        type: string
        default: "Hello World"
    steps:
      - run: echo << parameters.to >>
```

1. executors 执行单元 自定义的执行沙盒（容器） 2.1以上



```bash
version: 2.1
executors:
  my-executor:
    docker:
      - image: circleci/ruby:2.5.1-node-browsers

jobs:
  my-job:
    executor: my-executor
    steps:
      - run: echo outside the executor
```

1. jobs 工作单元



```bash
jobs:
  build:
    docker:
      - image: buildpack-deps:trusty
    environment:
      FOO: bar
    parallelism: 3
    resource_class: large
    working_directory: ~/my-app
    branches:
      only:
        - master
        - /rc-.*/
    steps:
      - run: make test
      - run: make
```

Jobs 是用map指定的，job的名字就是map的key值，如果你使用[Workflows](https://circleci.com/docs/2.0/workflows/)，jobs的名字必须保持唯一。如果没有使用workflows，就必须包含build这个key。build是版本控制push时的默认入口，然后再开始其他的工作。

> jobs的最大工作时间是5小时，超过这个时间你就得考虑使用并行模式。

------

###### job的参数说明：

`docker`:  指定容器的配置。
 `machine`: 虚拟机。
 `macos`: iOS运行机。

> `executor`: `docker`, `machine`, `macos` 这三个只能设置一个，多个会报错。

`shell`: 所有steps都需要运行的shell指令，会被steps里的shell覆盖。
 `steps`: 需要执行的步骤列表。
 `working_directory`: 在哪个目录下运行你的`steps`。
 `parallelism`: 要运行的此job的并行实例数，一般情况下是1，更多就得花钱买。
 `environment`: 环境变量。
 `branches`: 分支管理，白名单和黑名单。
 `resource_class`: 资源分配，分配多少CPU等。只有付费用户才有的特性。

###### `docker`

`executor`就是`steps`运行的地方。CircleCI可以一次性运行多个容器，或者使用整个虚拟机。
 `docker`使用的key包括：
 `image`: 容器的镜像。
 `name`: 容器的名字 可以通过名字来访问其他容器 默认是localhost。
 `entrypoint`: 容器启动时执行的指令。
 `command`: 作为为entrypoint的参数或者没有entrypoint直接执行命令 参考docker设计理念。
 `user`: 容器内执行程序的用户名称。
 `environment`: 环境变量 这个优先级高于外面job声明的。
 `auth`: `docker login` 的登录凭证。
 `aws_auth`: AWS EC2容器仓库的登录凭证。
 典型的栗子



```bash
jobs:
  build:
    docker:
      - image: buildpack-deps:trusty # primary container
        environment:
          ENV: CI

      - image: mongo:2.6.8
        command: [--smallfiles]

      - image: postgres:9.4.1
        environment:
          POSTGRES_USER: root

      - image: redis@sha256:54057dd7e125ca41afe526a877e8bd35ec2cdd33b9217e022ed37bdcf7d09673
```

如果你使用私人镜像，提供登录凭证就行了,这个参数的值可以在CircleCI的后台游览器里进行设置。



```php
jobs:
  build:
    docker:
      - image: acme-private/private-image:321
        auth:
          username: mydockerhub-user  # can specify string literal values
          password: $DOCKERHUB_PASSWORD  # or project UI env-var reference
```

在容器内部可以重用外部的commands指令。



```bash
jobs:
  myjob:
    docker:
      - image: "circleci/node:9.6.1"
    steps:
      - sayhello:
          to: "Lev"
```

###### `machine` 不谈了，说白了就是主机模式，就是运行了一个ubuntu镜像的容器，容器里有docker、docker-compose。详细了解的可以到 [machine executor](https://circleci.com/docs/2.0/executor-types)。



```bash
version: 2
jobs:
  build:
    machine:
      image: circleci/classic:201708-01
```

###### `macos` iOS macOS  tvOS watchOS平台所用的



```bash
jobs:
  build:
    macos:
      xcode: "9.0"
```

##### `branches`

定义分支的黑白名单（没有使用Workflows并且是2.0）如果你使用Workflows，只会执行workflows中定义的branches，如果你使用2.1，你必须得用workflows。
 栗子：



```bash
jobs:
  build:
    branches:
      only:
        - master
        - /rc-.*/
```



```bash
jobs:
  build:
    branches:
      ignore:
        - develop
        - /feature-.*/
```

###### `resource_class` (付费节目 不谈)。

------

###### `steps`

先把栗子甩出来：



```bash
jobs:
  build:
    working_directory: ~/canary-python
    environment:
      FOO: bar
    steps:
      - run:
          name: Running tests
          command: make test
```

steps应该是单一的键值对，key是代表step类型。这里的run只是一个step类型（step_type下面会讲更多的step类型），name是为了说明这个step是干嘛的，会在后台ui上输出，command就是sh指令。



```bash
jobs:
  build:
    steps:
      - run: make test
```

这个是缩写模式，name就是make test。还有一种缩写模式。



```bash
jobs:
  build:
    steps:
      - checkout
```

###### step_type: `run`

`command` 通过shell运行的指令。
 `name` step的名称。
 `shell` 执行指令的shell。
 `environment` command的环境变量。
 `background` 是否后台运行。
 `working_directory` 目录 默认是job的。
 `no_output_timeout` 没有输出情况下运行的时间 就是延时。
 `when` 什么时候开启和关闭step 值有`always`, `on_success`, `on_fail` 。
 每一个run都会生成一个shell，一个run的多个command都是在一个shell工作的



```bash
- run:
    command: |
      echo Running test
      mkdir -p /tmp/test-results
      make test
```

> 默认的shell是这个：`/bin/bash -eo pipefail` 如果命令返回非0值，会跳出报错，steps也走不下去。如果需要忽略指令的结果，可以主动修改shell，或者按照如下的set指令来修改，不过推荐使用默认的，因为这样你就能看到错误信息了。



```bash
- run:
    command: |
      echo Running test
      set +e
      mkdir -p /tmp/test-results
      make test

- run:
    shell: /bin/sh
    command: |
      echo Running test
      mkdir -p /tmp/test-results
      make test
```

一些简单的缩写格式



```bash
- run: make test

# shorthanded command can also have multiple lines
- run: |
    mkdir -p /tmp/test-results
    make test
```

`when`这个值很有意思，如果前面的step出现了错误，后面的step就不会执行（on_success）。如果你指明when为always的话，则这个step怎么都会执行的，on_fail只会前面有错误才执行，这对错误收集有作用。



```bash
steps:
  - run:
      name: Testing application
      command: make test
      shell: /bin/bash
      working_directory: ~/my-app
      no_output_timeout: 30m
      environment:
        FOO: bar

  - run: echo 127.0.0.1 devhost | sudo tee -a /etc/hosts

  - run: |
      sudo -u root createuser -h localhost --superuser ubuntu &&
      sudo createdb -h localhost test_db

  - run:
      name: Upload Failed Tests
      command: curl --data fail_tests.log http://example.com/error_logs
      when: on_fail
```

###### `when`注意这个when是跟run同级别的 上面那个when是run的子key（2.1的新特性，2.1干了很多控制方面的事情）

先睹栗子



```bash
version: 2.1

jobs: # conditional steps may also be defined in `commands:`
  job_with_optional_custom_checkout:
    parameters:
      custom_checkout:
        type: string
        default: \"\"
    machine: true
    steps:
      - when:
          condition: <<parameters.custom_checkout>>
          steps:
            - run: echo \"my custom checkout\"
      - unless:
          condition: <<parameters.custom_checkout>>
          steps:
            - checkout
workflows:
  build-test-deploy:
    jobs:
      - job_with_optional_custom_checkout:
          custom_checkout: \"any non-empty string is truthy\"
      - job_with_optional_custom_checkout            
```

这个不聊了 看栗子吧

###### `checkout`

直接拉取代码，通过SSH方式



```dart
- checkout
- run: git submodule sync
- run: git submodule update --init
```

由于checkout不支持子模块，如果需要拉子模块跑git指令

###### `setup_remote_docker` 创建配置为执行Docker命令的远程Docker环境（付费节目）

###### `save_cache`

在我们的对象存储中生成并存储文件或文件目录的缓存，例如依赖项或源代码

###### `restore_cache`

基于key恢复以前保存的缓存

###### `deploy`

deploy使用与run步骤相同的配置映射和语义

###### `store_artifacts`

store_artifacts可在Web应用程序中或通过API提供的工件（例如日志，二进制文件等）



```bash
- store_artifacts:
    path: /code/test-results
    destination: prefix
```

###### `store_test_results`

用于上传测试结果的特殊步骤，以便在构建中显示

###### `persist_to_workspace`

用于持久保存临时文件以供工作流中的另一个作业使用的特殊步骤。

###### `attach_workspace`

用于将工作流的工作空间附加到当前容器的特殊步骤

###### `add_ssh_keys`

将SSH密钥从项目设置添加到容器的特殊步骤。还配置SSH以使用这些密钥

------

#### `workflows`

工作流程 编排jobs
 整个配置文件



```bash
version: 2
jobs:
  build:
    docker:
      - image: ubuntu:14.04

      - image: mongo:2.6.8
        command: [mongod, --smallfiles]

      - image: postgres:9.4.1
        # some containers require setting environment variables
        environment:
          POSTGRES_USER: root

      - image: redis@sha256:54057dd7e125ca41afe526a877e8bd35ec2cdd33b9217e022ed37bdcf7d09673

      - image: rabbitmq:3.5.4

    environment:
      TEST_REPORTS: /tmp/test-reports

    working_directory: ~/my-project

    branches:
      ignore:
        - develop
        - /feature-.*/

    steps:
      - checkout

      - run:
          command: echo 127.0.0.1 devhost | sudo tee -a /etc/hosts

      # Create Postgres users and database
      # Note the YAML heredoc '|' for nicer formatting
      - run: |
          sudo -u root createuser -h localhost --superuser ubuntu &&
          sudo createdb -h localhost test_db

      - restore_cache:
          keys:
            - v1-my-project-{{ checksum "project.clj" }}
            - v1-my-project-

      - run:
          environment:
            SSH_TARGET: "localhost"
            TEST_ENV: "linux"
          command: |
            set -xu
            mkdir -p ${TEST_REPORTS}
            run-tests.sh
            cp out/tests/*.xml ${TEST_REPORTS}

      - run: |
          set -xu
          mkdir -p /tmp/artifacts
          create_jars.sh ${CIRCLE_BUILD_NUM}
          cp *.jar /tmp/artifacts

      - save_cache:
          key: v1-my-project-{{ checksum "project.clj" }}
          paths:
            - ~/.m2

      # Save artifacts
      - store_artifacts:
          path: /tmp/artifacts
          destination: build

      # Upload test results
      - store_test_results:
          path: /tmp/test-reports
          
  deploy-stage:
    docker:
      - image: ubuntu:14.04
    working_directory: /tmp/my-project  
    steps:
      - run:
          name: Deploy if tests pass and branch is Staging
          command: ansible-playbook site.yml -i staging          
          
  deploy-prod:
    docker:
      - image: ubuntu:14.04
    working_directory: /tmp/my-project  
    steps:
      - run:
          name: Deploy if tests pass and branch is Master
          command: ansible-playbook site.yml -i production

workflows:
  version: 2
  build-deploy:
    jobs:
      - build
      - deploy-stage:
          requires:
            - build
          filters:
            branches:
              only: staging
      - deploy-prod:
          requires:
            - build
          filters:
            branches:
              only: master
```

写的比较粗糙，后期有时间再完善。



作者：你家旭哥
链接：https://www.jianshu.com/p/2abb9db80d6c
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---



```yaml
version: 2.1
jobs: # a collection of steps
  # 定义job的名字为【build】 runs not using Workflows must have a `build` job as entry point，
  build:
    # 配置工作目录
    working_directory: ~/repo
    # 执行环境为docker  run the steps with Docker
    docker:
      - image: circleci/openjdk:8-jdk-stretch
      # a collection of executable commands ，当前jobs要执行的具体步骤
    steps:
      # check out source code to working directory 检出源码到工作目录
      - checkout

      - restore_cache: # restore the saved cache after the first run or if `pom.xml` has changed
          # Read about caching dependencies: https://circleci.com/docs/2.0/caching/
          key: v1-repo-{{ checksum "pom.xml" }}
      
      - run:
          name: get dependency
          # 拉取依赖 gets the project dependencies
          command: mvn dependency:go-offline
      
      - save_cache: # saves the project dependencies
          paths:
            - ~/.m2
          key: v1-repo-{{ checksum "pom.xml" }}
      - run:
          name: build start
          command: echo "build start"

      - run:
          name: build project
          # 这里进行打包操作
          command: mvn package
      # 成功时的运行命令
      - run:
          name: bulid success
          command: echo "build success"
          when: on_success
      # 失败时的运行命令
      - run:
          name: build fail
          command: echo "build failur"
          when: on_fail
      # 存储测试结果
      - store_test_results: # uploads the test metadata from the `target/surefire-reports` directory so that it can show up in the CircleCI dashboard. 
        # Upload test results for display in Test Summary: https://circleci.com/docs/2.0/collect-test-data/
            path: target/surefire-reports
        
      - store_artifacts: # store the uberjar as an artifact
        # Upload test summary for display in Artifacts: https://circleci.com/docs/2.0/artifacts/
            path: target/griantBaby-0.0.1-SNAPSHOT.jar
        # See https://circleci.com/docs/2.0/deployment-integrations/ for deploy examples 
workflows:
  version: 2
  # 定义了三个工作流

  # 每次提交时触发
#  commit-workflow:
#    jobs:
#      - build:

  # 当被他人fork时，他人提交PR时进行,使用filter定义 要匹配的branch，only 参数接收一个正则表达式，
  # 一般会和commit-workflow重复，所以这里只保留这个工作流程即可
  fork-commit-workfolw:
    jobs:
      - build:
          filters:
            branches:
              only: /^[A-Za-z0-9].*/
  # 定时触发，由两个部分组成，triggers、要执行的jobs
  scheduled-workfolw:
    triggers:
      - schedule:
           cron: "0 0 * * *"
           filters:
             branches:
               only:
                 - master
    jobs:
      - build
```



---





- https://zhuanlan.zhihu.com/p/370550987
- https://juejin.cn/post/6844903855658909704
- https://www.cnblogs.com/weixuqin/p/11002175.html
- https://www.jianshu.com/p/36af6af74dfc
- https://www.jianshu.com/p/2abb9db80d6c