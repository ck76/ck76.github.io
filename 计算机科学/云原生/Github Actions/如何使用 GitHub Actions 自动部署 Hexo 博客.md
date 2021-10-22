[TOC]

## 什么是 GitHub Actions

GitHub Actions 是 GitHub 的持续集成服务。持续集成由很多操作组成，比如抓取代码、运行测试、登录远程服务器，发布到第三方服务等等。GitHub 把这些操作就称为 actions。

很多操作在不同项目里面是类似的，完全可以共享。GitHub 允许开发者把每个操作写成独立的脚本文件，存放到代码仓库，使得其他开发者可以引用。

如果你需要某个 action，不必自己写复杂的脚本，直接引用他人写好的 action 即可，整个持续集成过程，就变成了一个 actions 的组合。这就是 GitHub Actions 最特别的地方。

> 本例就使用由 [theme-keep](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Ftheme-keep) 提供的 `action` [hexo-deploy-github-pages-action](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Ftheme-keep%2Fhexo-deploy-github-pages-action) 来自动部署 Hexo。

## 使用 GitHub Actions 自动部署的好处

- 可以直接在线编辑 `md` 文件，立即生效。假设你已发布一篇文章，过几天你在别的电脑上浏览发现有几个明显的错别字，这是完全不能容忍的。但此时你电脑上又没有 `hexo + node.js + git` 等完整的开发环境，重新配置开发环境明显不现实。如果使用 CI，你可以直接用浏览器访问 GitHub 上的项目仓库，直接编辑带错别字的 `md` 文章，改完，在线提交，稍等片刻，你的网站就自动更新了。
- 如果手动部署，需要先执行 `hexo g` 编译生成静态文件， 然后推送 `public` 整个文件夹到 GitHub 上，当后期网站文章、图片较多时候，很多时候连接 GitHub 不是那么顺畅，经常要傻等很长的上传时间。使用 GitHub Actions 自动部署，你只需 `push` _post 文件里单独的 `md` 文件即可，其他不用管，效率瞬间高了许多，其中的好处，谁用谁知道。
- 使用 GitHub Actions，你还可以一次性将这些静态博客页面部署到多个服务器上，例如：GitHub Pages、Gitee pages、七牛云、阿里云、腾讯云等等。
- ...

## 准备工作

本文以 Hexo + [Keep](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2FXPoet%2Fhexo-theme-keep) 主题搭建博客为例，教你如何使用 GitHub Actions 将博客自动部署到 GitHub Pages。

### 创建 GitHub 仓库

创建两个 [GitHub 仓库](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Fnew)，一个**公共仓库**和一个**私有仓库**。

- **私有仓库**用来存储 Hexo 项目源代码。（保证你的重要信息不泄露）

  ![image](https://tva1.sinaimg.cn/large/008i3skNly1gvojaxg05hj618e0hggna02.jpg)

- **公共仓库**用来存储编译之后的静态页面。

  ![image](https://tva1.sinaimg.cn/large/008i3skNly1gvojawz0xcj619i0i640e02.jpg)

本例：

- 用私有仓库的 `master` 分支来存储项目源代码。
- 用公共仓库的 `gh-pages 分支` 来存储静态页面。

当私有仓库的 `master` 有内容 `push` 进来时（例如：主题文件，文章 md 文件、图片等）， 会触发 GitHub Actions 自动编译并部署到公共仓库的 `gh-pages 分支`。

### 创建 GitHub Token

创建一个有 **repo** 和 **workflow** 权限的 [GitHub Token](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Fsettings%2Ftokens%2Fnew) 。

![image](https://tva1.sinaimg.cn/large/008i3skNly1gvojaumilcj619a0kwdhz02.jpg)

新生成的 Token 只会显示一次，如有遗失，重新生成即可。

![image](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/484558ed11fa4b5e8042c65f398ae6ae~tplv-k3u1fbpfcp-watermark.awebp)

### 创建 repository secret

将上面生成的 Token 添加到私有仓库的 `Secrets` 里，并将这个新增的 `secret` 命名为 `HEXO_DEPLOY` （名字无所谓，看你喜欢）。

步骤：私有仓库 -> `settings` -> `Secrets` -> `New repository secret`。

![image](https://tva1.sinaimg.cn/large/008i3skNly1gvojat7mfqj61bi0u0n0702.jpg)

> 新创建的 secret `HEXO_DEPLOY` 在 Actions 配置文件要用到，需跟配置文件保持一致！

### 添加 Actions 配置文件

1. 在你的 Hexo 项目根目录下创建 `.github` 文件夹。
2. 在 `.github` 文件夹下创建 `workflows` 文件夹。
3. 在 `workflows` 文件夹下创建 `hexo-deploy.yml` 文件。

![image](https://tva1.sinaimg.cn/large/008i3skNly1gvojas8pbgj60u00ewjsu02.jpg)

`hexo-deploy.yml` 文件的内容如下：

```yaml
name: deploying Hexo project to GitHub pages
on:
  push:
    branches:
      - master # master 分支有 push 行为时就触发这个 action

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@master

      - name: Build and Deploy
        uses: theme-keep/hexo-deploy-github-pages-action@master # 使用专门部署 Hexo 到 GitHub pages 的 action
        env:
          PERSONAL_TOKEN: ${{ secrets.HEXO_DEPLOY }} # secret 名
          PUBLISH_REPOSITORY: XPoet/keep-blog # 公共仓库，格式：GitHub 用户名/仓库名
          BRANCH: gh-pages # 分支，填 gh-pages 就行
          PUBLISH_DIR: ./public # 部署 public 目录下的文件
复制代码
```

> 在上面的配置文件中，使用了 [theme-keep](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Ftheme-keep) 组织封装的 **[hexo-deploy-github-pages-action](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Ftheme-keep%2Fhexo-deploy-github-pages-action)** ，是一款专门部署 Hexo 博客到 GitHub pages 的 action，欢迎大家 [Star](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Ftheme-keep%2Fhexo-deploy-github-pages-action) 和 [Fork](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Ftheme-keep%2Fhexo-deploy-github-pages-action)

至此，准备工作完毕~

## 自动部署触发流程

1. 修改你的 Hexo 博客源代码（例如：增加文章、修改文章、更改主题、修改主题配置文件等等）。

2. 把你修改过的 Hexo 项目内容（只提交修改过的那部分内容） `push` 到 GitHub 私有仓库（本例：keep-site-source）的 `master` 分支。

3. GitHub Actions 检测到 `master` 分支有内容 `push` 进来，会自动执行 action 配置文件的命令，将 Hexo 项目编译成静态页面，然后部署到公共仓库的 `gh-pages` 分支。

   > `gh-pages` 分支是 GitHub Pages 服务的固定分支，你只需在 `hexo-deploy.yml` 文件正确填写，会自动创建。

4. 在私有仓库的 Actions 可以查看到你配置的 action。 ![image](https://tva1.sinaimg.cn/large/008i3skNly1gvojar0g66j31fk0msq5q.jpg)

5. 通过 GitHub Actions 自动部署成功的效果图： ![image](https://tva1.sinaimg.cn/large/008i3skNly1gvojaplutnj61gc0re43a02.jpg)

## 类似工具

除了 GitHub Actions，我们还可以使用 **Travis CI** 来实现 Hexo 自动部署，效果是一样的，任君选择。

**教程：[使用 Travis CI 自动部署 Hexo 静态博客](https://juejin.cn/post/6943628312933564452)**

> 


作者：XPoet
链接：https://juejin.cn/post/6943895271751286821
来源：稀土掘金
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

- https://juejin.cn/post/6943895271751286821