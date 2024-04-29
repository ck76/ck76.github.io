[TOC]

GitHub 推出了 Actions 之后，我们可以直接在 GitHub 上实现自动集成和部署，而不需要依赖第三方 CI 工具，例如 TravisCI、CircleCI 等。

## 曾经踩过的坑

这篇博客部署在了 [GitHub Pages](https://pages.github.com/) 上。因为此网站使用了 [Hugo](https://gohugo.io/) 把 Markdown 文档生成静态页面，所以 Markdown 文章加上 Hugo 的配置文件（以下简称“源码”）需要与生成的 HTML 代码分开存放。

GitHub Pages 支持部署 *指定分支* 下的 *指定文件夹* 内的 HTML 内容（在仓库的 `Settings` 可以设置）。如果 *只考虑* 手动部署，有以下三种管理代码的方案（下文假定编译完生成的 HTML 的目标路径为 `public/` 文件夹）：

1. 把源码和 `public/` 放在同一个分支（master），并指定 master 分支下的 `public/` 文件夹为部署的路径
2. 把 `public/` 的内容单独放在主仓库，把源码放在另一个仓库，并添加 `public/` 文件夹为其 [submodule](https://git-scm.com/docs/git-submodule)
3. 源码和 `public/` 存放在同一个仓库下，源码放在 `master` 分支，`public/` 内容放在另一个分支（如 `gh-pages`），并指定部署 `gh-pages` 分支的内容

方案一因为要把 `public/` 和源码同时纳入版本控制，因此会使得代码仓库的 commit 历史同时包含源码与 `public/` 的改动，commit 历史比较杂乱，不可取。

之前一直使用的方案二，我给源码单独开了一个[仓库](https://github.com/yuqingc/homepage-src)。但是需要同时管理 2 个代码仓库，并且当 submodule 的 HEAD 发生改变时，还需要同步更新源码仓库的 Git 历史。

后来，我采用了方案三，把源码迁移到了主仓库。如果需要手动部署，我们可以使用 [git worktree](https://git-scm.com/docs/git-worktree)，把同一个仓库的其他分支映射为当前工作区的一个文件夹下。这样就避免了频繁切换分支。具体操作可以参考我的的手动部署[脚本](https://github.com/yuqingc/yuqingc.github.io/blob/master/bin/publish_to_ghpages.sh)。或者参考 Hugo 关于部署的 [文档](https://gohugo.io/hosting-and-deployment/hosting-on-github/#deployment-of-project-pages-from-your-gh-pages-branch)。

在方案三下，我们可以使用 GitHub Actions 自动构建 `master` 分支的源码，并且把生成的 HTML 部署到 `gh-pages` 分支。每当我们往 master 推送新的内容时，就会自动触发编译。并且，Workflow 会把编译生成的 HTML 自动部署到指定的分支。这样，我们就无需关心构建与部署的过程，每次更新文章，只需要在 `master` 分支修改 Markdown 即可。

在开始行动之前，先了解一下 GitHub Action 中的一些基本概念，有助于我们了解和使用 GitHub Action。

## 一些基础概念

在玩 Action 之前，需要先了解一下基础概念：

```
                        +-------------+
                        |             |
                        |  Workflow   |
                        |             |
                        +------+------+
                               |
                               |
                               |
      +-------------------------------------------------+
      |                        |                        |
      |                        |                        |
      |                        |                        |
 +----v-----+             +----v-----+             +----v-----+
 |          +------------>+          +------------>+          |
 |   Job    |             |   Job    |             |    Job   |
 |          +------------>+          +------------>+          |
 +----------+             +----+-----+             +----------+
                               |
                               |
                               |
                               |
      +---------------------------------------------------+
      |                        |                          |
      |                        |                          |
      |                        |                          |
      |                        |                          |
+-----v-----+     +------------v--------------+      +----v-----+
|           |     |                           |      |          |
|   Step    +---->+          Step             +----->+  Step    |
|           |     | +---------+ +----------+  |      |          |
|           +---->+ | Command | |  Action  |  +----->+          |
+-----------+     | |         | |          |  |      +----------+
                  | +---------+ +----------+  |
                  +---------------------------+
```

*（流程图由 http://asciiflow.com/ 生成）*

### TL;DR

Workflow 可以在仓库根目录的 `.github/workflows` 目录下配置。一个 Workflow 至少需要包含一个 Job。一个 Job 包含了多个 Step。每个 Step 可以执行命令和脚本，也可以使用 Action。可以自己创建 Action，也可以使用社区开源的 Action。

### CI

CI 为持续集成（Continuous integration）。持续提交到代码仓库的代码，可以在每次提交的时候触发一些 CI 操作，例如构建、测试，以保证代码的质量。可以在 CI 提供的控制台查看日志等信息。

### CD

CD 为持续部署（Continuous deployment）。利用 CD 工作流，可以在代码通过 CI 测试之后，自动部署到生产环境。

### Workflow

Workflow 是你为你的代码仓库配置的一套流水线的流程，包括了构建、测试、打包、发布、部署等工作。一个 Workflow 可以由若干个 [Job](https://yuqingc.github.io/posts/2020/github-actions/#job) 组成。Workflow 可以定时触发，也可以通过 [GitHub Event](https://yuqingc.github.io/posts/2020/github-actions/#event) 触发.

Workflow 包含两个概念

- Workflow file：位于代码仓库根目录的 `.github/workflows` 下的 YAML 配置文件
- Workflow run：一次 Workflow 运行的实例

### Runner

Runner 是安装了 GitHub Actions runner 应用程序的机器（可以是虚拟机）。每个 Runner 从等待被执行的 Job 队列中选择 Job 来执行。当 Runner 选择了一个 Job 时，Runner 会执行 Job 内的 [Action](https://yuqingc.github.io/posts/2020/github-actions/#action) 并上报执行过程、日志和结果。Runner 每次只执行一个 Job。

Runner 分为 GitHub 自带的 Runner 和用户自己的私人 Runner。

### Job

Job 可以由若干个 [Step](https://yuqingc.github.io/posts/2020/github-actions/#step) 组成。这些 Step 在同一个 Runner 上执行。你可以在 Workflow 的配置文件定义 Job 的执行顺序和依赖关系。不同的 Job 可以同时并行运行，也可以按顺序依次执行。

### Step

每个 Step 是一个独立的任务，可以是执行命令，也可以执行 [Action](https://yuqingc.github.io/posts/2020/github-actions/#action)。每个 Job 可以配置一个或多个 Step。在一个 Job 中的所有的 Step 都在一个 Runner 内执行。这样就可以使得 Job 内的 Action 可以通过文件系统来共享信息了。

### Action

Action 是一个独立的任务，可以把多个 Action 组装成一个 Job。Action 是 Workflow 的最小组成单元。你可以自己创建 Action，也可以使用 Action 商店内开源的 Action。*必须*要把 Action 封装在 Step 中使用。

### Event

Event 是一些 GitHub 的事件。比如当用户 push 到代码仓库，或者发起 PR 或 Issue 时，会触发特定的 Event，从而触发 Workflow 的执行。

### Artifact

Artifact 是 Workflow run 产出的文件。比如打包生成的二进制文件、测试报告、快照、日志等。产生的 Artifact 可以给另一个 Job 使用，也可以用来部署。

## Workflow

### 创建配置文件

1. 在仓库根目录创建 `.github/workflows` 文件夹用以存放配置文件
2. 配置文件必须是 YAML 文件。后缀名是 `.yml` 或 `.yaml`
3. 配置文件的语法和格式 API 参考 [语法文档](https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions)
4. 保存提交代码

配置文件示例

```yaml
# .github/workflows/continuous-integration-workflow.yml

name: Greet Everyone
# push 到仓库会触发此 Workflow
on: [push]

jobs:
  build:
    # 设置 Job 名称
    # Job name is Greeting
    name: Greeting
    # This job runs on Linux
    runs-on: ubuntu-latest
    steps:
      # 使用第三方 Action
      # This step uses GitHub's hello-world-javascript-action: https://github.com/actions/hello-world-javascript-action
      - name: Hello world
        uses: actions/hello-world-javascript-action@v1
        with:
          who-to-greet: 'Mona the Octocat'
        id: hello
      # This step prints an output (time) from the previous step's action.
      - name: Echo the greeting's time
        run: echo 'The time was ${{ steps.hello.outputs.time }}.'
```

以下是基础配置，完整配置，查看 [语法文档](https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions)

### 触发 Workflow

- 可以监听 GitHub Event，如 push

  ```yaml
  on: push
  ```

- 可以配置 Cron 来定时触发

  ```yaml
  on:
    schedule:
      - cron: '0 * * * *'
  ```

- 可以手动触发（可以点击 GitHub UI 上的按钮，也可以调用 webhook 来触发），请参考文档

### 指定分支、Tag 或路径发生改变时触发

```yaml
on:
  push:
    branches:
      - master
    tags:
      - v1
    # 默认监听所有的文件
    paths:
      - 'test/*'
```

### 指定运行环境

```yaml
runs-on: ubuntu-latest
```

### 配 Matrix

使用 Matrix 可以使 Workflow 同时在不同的机器、系统、版本、语言下运行。详情请参考文档。

### 使用 `checkout` Action

GitHub 内置了一些标准的 Action。在下列情况下必须在你的其他 Action 之前使用 `checkout` Action

- Workflow 需要使用代码的副本，比如需要构建、测试或使用 CI 时
- 至少有一个该仓库定义的 Action

```yaml
- uses: actions/checkout@v2
```

### 选择 Workflow 类型

可以选择以下类型的 Workflow。详情参考文档。

- Docker container actions
- JavaScript actions
- Composite run steps actions

### 引用 Action

可以在 Workflow 中引用已经定义好的 Action，如

- 一个 public 仓库
  - 语法 `{owner}/{repo}@{ref}` 或 `{owner}/{repo}/{path}@{ref}`
- 当前 Workflow 文件所在仓库
  - 同一个仓库下，可以使用相对路径，如 `./.github/actions/hello-world-action`
- 在 Docker Hub 上的 Docker 镜像
  - 语法 `docker://{image}:{tag}`

### 给仓库添加 Workflow 状态的图标（Badge）

- 如果 name 使用了空格等特殊字符，需要转义，如空格对应 `%20`
- 注意 `<WORKFLOW_NAME>` 为 YAML 文件中的 name，而非配置文件的文件名

```
https://github.com/<OWNER>/<REPOSITORY>/workflows/<WORKFLOW_NAME>/badge.svg
```

示例

```md
![example workflow name](https://github.com/actions/hello-world/workflows/Greet%20Everyone/badge.svg)
```

更多示例和参数，请参考文档

## 使用第三方 Action 部署博客

我们可以在 Workflow 中使用 GitHub [官方的 Action](https://github.com/actions/)。也可以在 GitHub 的[应用商店](https://github.com/marketplace?type=actions) 找到开源的 Action。

该网站使用了两个第三方的 Action：

- [Hugo setup](https://github.com/marketplace/actions/hugo-setup)：依赖构建代码
- [GitHub Pages action](https://github.com/marketplace/actions/github-pages-action)：用来把构建完成的内容部署到 GitHub Pages

项目配置非常简单。首先在仓库的根目录创建 `.github/workflows/gh-pages.yml` 文件。如果看不懂这个文件，可以先回头去阅读[上文](https://yuqingc.github.io/posts/2020/github-actions/#一些基础概念)。

```yaml
name: GitHubPages

on:
  push:
    branches:
      - master  # 指定了用于部署的源码所在的分支
    paths-ignore:
      - 'README.md'
      - '.env'
      - '.gitignore'
      - '.gitmodules'
      - 'bin'

jobs:
  deploy:
    runs-on: ubuntu-18.04
    steps:
      - uses: actions/checkout@v2 # 上文提到了第一步必须使用 checkout Action
        with:
          submodules: recursive  # 获取 Hugo 的主题，主题放在 submodule 中 (true OR recursive)
          fetch-depth: 0    # Fetch all history for .GitInfo and .Lastmod

      - name: Read .env
        id: hugo-version
        run: | # 从 .env 文件中读取 HUGO 的版本号
          . ./.env
          echo "::set-output name=HUGO_VERSION::${HUGO_VERSION}"

      - name: Setup Hugo
        uses: peaceiris/actions-hugo@v2 # 使用第三方 Action 进行构建
        with:
          hugo-version: '${{ steps.hugo-version.outputs.HUGO_VERSION }}'
          # extended: true

      - name: Build
        run: hugo --minify

      - name: Deploy
        uses: peaceiris/actions-gh-pages@v3 # 使用第三方 Action 把构建完成的 Artifact 部署到指定分支
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_branch: gh-pages
```

需要注意一点。在 `Deploy` 这个 Step 中使用了 `github_token`。`github_token` 是 GitHub Action 在 Workflow 中自动创建的用于权限认证的 token。使用 `${{ secrets.GITHUB_TOKEN }}` 变量可以读取到该 token，不需要额外进行配置。

权限认证除了 `github_token`，还可以使用 `deploy_key` 和 `personal_token`。这些都可以在代码仓库进行配置。

具体配置和操作步骤可以参考文档。

配置完成之后，就可以把代码 push 到 `master` 分支了。下次当 GitHub 检测到 `master` 上代码更新的时候，就会自动触发 Workflow 了。这样就不用每次手动部署了。我们需要做的，仅仅是写文档、push。

- https://yuqingc.github.io/posts/2020/github-actions/