[TOC]

## [#](https://didiheng.com/front/2019-12-11.html#github-action配置)Github Action配置

下面的内容默认你已经会创建Github Action，官方提供了很多Github Action 配置模版，可以根据自己的需求随意选择，不是太了解的可以先参考[阮一峰 GitHub Actions入门教程](http://www.ruanyifeng.com/blog/2019/09/getting-started-with-github-actions.html)，了解一下Github Action。

我们来看看Github Action配置文件的基本构成，配置文件格式是`.yml`，示例如下：

```yml
# main.yml
# 一个 workflow，名字为Github Action Example
name: Github Action Example

# 触发 workflow 的事件
on:
  push:
    # 分支随意
    branches:
      - master

# 一个workflow由执行的一项或多项job
jobs:
    # 一个job任务，任务名为build
    build:
        # runs-on 指定job任务运行所需要的虚拟机环境(必填字段)
        runs-on: ubuntu-latest
        # steps是每个Job的运行步骤，可以包含一个或多个步骤
        steps:
            # action命令，切换分支获取源码
            - name: Checkout
                # 使用action库  actions/checkout获取源码
                uses: actions/checkout@master
            # action命令，安装Node10
            - name: use Node.js 10
                # 使用action库  actions/setup-node安装node
                uses: actions/setup-node@v1
                with:
                    node-version: 10
            # action命令，install && test
            - name: npm install and test
                # 运行的命令或者 action
                run: |
                    npm install
                    npm run test
                # 环境变量
                env:
                    CI: true
```

- Action是工作流中最小的可移植构建块。你可以创建自己的动作，使用从[GitHub社区共享的action库](https://github.com/marketplace?utf8=✓&type=actions&query=deploy)，以及自定义公共action库。
- Step是Job执行的一组任务。Job中的每个步骤都在同一运行程序中执行，从而允许该Job中的操作使用文件系统共享信息，Step可以运行命令或action。
- Job由Step构成。你可以定义工作流文件中Job的运行方式的依赖关系规则。Job可以同时并行运行，也可以依赖于先前Job的状态依次运行。
- Workflow由一个或多个Job组成，可以通过[事件](https://help.github.com/cn/actions/automating-your-workflow-with-github-actions/events-that-trigger-workflows)进行计划或激活。你可以在存储库中设置一个可配置的自动化过程，以在GitHub上构建，测试，打包，发布或部署任何项目。

[Github: Github Action插件查询库](https://github.com/marketplace?utf8=✓&type=actions&query=deploy)，可以查询你需要的action库，这些都是共享的，如果满足不了需求也可以自己定义。

## [#](https://didiheng.com/front/2019-12-11.html#github-action发布阿里云ecs)Github Action发布阿里云ECS

下面就是我发布到阿里云ECS的 Github Action配置文件

> [我的Github Action服务配置](https://github.com/HerryLo/BlogPress/blob/master/.github/workflows/main.yml)

```yml
# main.yml
name: deploy to aliyun
on:
  push:
    branches:
      - master
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      # 切换分支
      - name: Checkout
        uses: actions/checkout@master
      # 下载 git submodule
      - uses: srt32/git-actions@v0.0.3
        with:
          args: git submodule update --init --recursive
      # 使用 node:10
      - name: use Node.js 10
        uses: actions/setup-node@v1
        with:
          node-version: 10
      # npm install
      - name: npm install and build
        run: |
          npm install
          npm run build
        env:
          CI: true
      # Deploy
      - name: Deploy
        uses: easingthemes/ssh-deploy@v2.0.7
        env:
          SSH_PRIVATE_KEY: ${{ secrets.ACCESS_TOKEN }}
          ARGS: "-avz --delete"
          SOURCE: "[Current File Dir]"
          REMOTE_HOST: "[Domain]"
          REMOTE_USER: "[UserName]"
          TARGET: "[Server Dir]"
```

以上是我的配置文件，action插件请根据自己的需求合理选择。我是要博客网站，发布到阿里云服务上，所以采用以上配置，而最后的action Deploy中action插件的选择，也是根据需求，在[Github: action插件库](https://github.com/marketplace?utf8=✓&type=actions&query=deploy)中选择的。

## [#](https://didiheng.com/front/2019-12-11.html#更多)更多

[使用GitHub Actions发布Hexo网站到GitHub Pages](https://juejin.im/post/5da03d5e6fb9a04e046bc3a2)

[GitHub Actions发布博客到阿里云OSS](https://juejin.im/post/5ddb2cabe51d45232250b8b3#heading-12)

[GitHub Actions自动构建镜像并推送到阿里云容器镜像服务](https://athorx.com/posts/聊技术/20191007-使用github-actions自动构建镜像并推送到阿里云容器镜像服务.html)

## [#](https://didiheng.com/front/2019-12-11.html#参考)参考

[Core concepts for GitHub Actions](https://help.github.com/en/actions/automating-your-workflow-with-github-actions/core-concepts-for-github-actions)

[阮一峰: GitHub Actions 入门教程](http://www.ruanyifeng.com/blog/2019/09/getting-started-with-github-actions.html)