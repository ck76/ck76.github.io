[TOC]

- 官网链接 https://docs.travis-ci.com/user/tutorial/



## 前言

本文主要讲如何用 Travis 来实现自动化部署，并参照 Github 真实项目开发简单场景来介绍。

## CI/CD

在说 Travis 之前，先了解一下 CI/CD 的概念。

### CI

**CI（Continuous integration）—— 持续集成**。持续集成是指频繁地（一天多次）将代码集成到主干，注重将各个开发者的工作集合到一个代码仓库中，在源代码变更后会自动检测、拉取、构建和进行单元测试，其目的在于让产品快速迭代，同时保证高质量。

比如日常工作中，向`development`分支提交一个 PR，配置好 Travis 就会进行自动测试等等工作，通过并被 review 后才能允许合并到开发分支。一旦自动测试脚本有错误，则不允许合并。

### CD

CD 可对应多个英文名称，一个是**持续交付（Continuous delivery）**，一个是**持续部署（Continuous deployment）**。

**持续交付**指频繁地将软件的新版本交付给质量团队或者用户，以供评审。如果评审通过，代码就进入生产阶段。
目标是拥有一个可随时部署到生产环境的代码库。好处在于，每次代码的小幅变更，就能看到运行结果，从而不断累积小的变更，而不是在开发周期结束时，一下子合并一大块代码。

**持续部署**指持续交付的下一步，指的是代码通过评审以后，自动部署到生产环境。目标是代码在任何时刻都是可部署的，可以进入生产阶段。

持续交付并不是指软件每一个改动都要尽快部署到产品环境中，它指的是任何的代码修改都可以在任何时候实施部署。

持续交付表示的是一种能力，而持续部署表示的则一种方式。持续部署是持续交付的最高阶段

## Travis 是什么

**Travis CI** 提供的是持续集成服务，它可以绑定 Github 上面的项目，只要有新的代码，就会自动抓取。然后，提供一个运行环境，执行测试，完成构建，还能部署到服务器。当然这只是其中一种 CI 工具，另外还有`Jenkins`等。

本文仅仅做的抛砖引玉，让你对这些概念和 Travis 有初步了解和使用，不再陌生，剩余的就靠你自己去学习了。也许你觉得这是公司企业级项目才配上的东西，但其实 Travis 是免费的，个人开发者也可以进行使用，而且是挺好用。

## 注册 Travis

进入[https://travis-ci.com](https://link.segmentfault.com/?enc=qY9nrJznVKAYNBFFNq1l4A%3D%3D.D5NfOWaRftsQ8NXfphGPIvrfgOYgCmse7rqzuBNX9ao%3D)，

比如我选择 Github 方式登录

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoit8otbuj60kc0dqwfg02.jpg)

登录之后 Github 的 repo 授权给 travis，我直接选 All，开启对项目的监控
![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoit7mijrj60hi0fc75r02.jpg)

选完之后回到主页就能看到你的仓库页面了，可以看到有一个项目我是配置了 travis 的，所以显示绿色状态。
![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoit68mg0j60kc08yq3d02.jpg)

这项目是最近在自己动手做的 React 组件库项目 [monki-ui](https://link.segmentfault.com/?enc=QiBgVjGlVPOZE5yrZWtWJQ%3D%3D.kUTpAyMMjlqMA1LpG9FTbUwg4%2FrG3uCZ41J9ZM3UwxXpx%2BAtEdCeWr0o0USaKiZU)，尽量参照企业级标准搭建的，技术栈是`React + TypeScript + Dumi + Jest`，觉得不错可供借鉴学习的话可以 ✨ star 一下哦...

## 获取 Github Access Token

接着进入 `Github -> Developer settings -> Personal access tokens -> Generate new token`

勾选`repo`下的所有项，以及`user`下的`user:email`后，会生成一个 token，然后记得复制保存好，因为生成后以后就不会再显示了，如果忘记那必须重新生成`token`才可以

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoit5b7vrj60kc04i74n02.jpg)

## 配置 Travis

我自己用 create-react-app 初始化了一个 Github 项目 [travis-demo](https://link.segmentfault.com/?enc=TlMLlwxjLfIItyNJC9ZzJA%3D%3D.Vqc%2B0kEmdcPdL7JHrO6PgfU2cG0gySYjjQhclP6wgw2nn61gAp%2BaUBVv6XUYV%2Bgf)，删除文件到最简单的项目结构。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoit3zo15j3058086glq.jpg)

然后模拟一下日常开发场景，让大家体会工作中的应用，我先是从`master`分支 checkout 出`development`分支，并把`development`分支设置为`default`分支，这个就当做我们开发中的 dev 分支，日常 PR 都会合并到该分支上。

然后从`development`分支 checkout 出一条分支`docs/update-readme`，接着修改 readme 的内容，git add 和 git commit 后就待 push 了。

我们上面生成的 token 已经说了要先保存好的，接着去[travis-ci 官网](https://link.segmentfault.com/?enc=pmfNmRaPUKH0je5TNCOl%2BQ%3D%3D.83j6g4dMMjP0pEYMAS5YAskZOpOFp2VWmu%2BFIkdG%2FUg%3D)，找到刚才创建的项目`Jacky-Summer/travis-demo`，点进去选择`Settings`

- 设置全局变量(Environment Variables)

`NAME`一般规范是大写和下划线来命名，这里我取为`GITHUB_TOKEN`，`VALUE`为我们保存的 token，然后选择`Add`添加

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoit32jdjj60kc02q0so02.jpg)

## 项目新建配置文件

在项目根目录配置`.travis.yml`配置文件（当前在`docs/udpate-readne`）

```yaml
language: node_js  # 设置语言

node_js:
  - 12 # 指定node.js版本

cache: yarn # 开启缓存

install:
  - yarn
script:
  - yarn build
```

接着就 push 到远程分支，此时打开 github 项目仓库，点击`Create pull request`，

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoit17ny6j60kc06zaan02.jpg)

就会看到此时 Travis 正在运行了，它在干什么事，可以点进`Details`去看看，它就是从头来一遍我们项目，`git clone`开始，然后`yarn install`，`yarn build`，整个过程没有错误，那就通过了，通过之后看到此时 PR 这样显示的：

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoit053wnj60kc04sjrs02.jpg)

通过说明我们可以 Merge 了，这里我选择`Squash and merge`合并，合并后会再次跑一次 Travis（但日常开发我司是要先有其他同事 review 通过才能 merge 的）。此时我们的 PR 就合并到`development`分支上了，你的邮箱整个过程中也会有邮件通知你。

但其实你会发现你不用等它 Travis 运行完成，我们就可以点`Merge`按钮，按道理它应该暂时禁用才对，等到 Travis 通过才允许按。这个在`Github该项目目录 -> Settings -> Branches -> Add rules`，这里只是简单举个例子如下：

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoisxvelbj30kc09o0tl.jpg)

仅仅这样可能并感受不到什么作用，现在我们切换回`development`分支，`git pull`最新代码，再 checkout 一条新分支`test/test-case-example`，添加个测试文件`src/index.test.js`

如果不知道什么是单元测试的可以看我这篇文章：[一文带你了解 Jest 单元测试](https://link.segmentfault.com/?enc=7r092R%2F61D6dUJ7Fv4p35A%3D%3D.oSw25qb98mXm5%2BYoggD1XKrufm80iDT8s%2FrKf2YgLrFBS6LA1JEnLUZ%2FgS%2BlMz0d)

```javascript
describe('单元测试', () => {
  it('测试用例', () => {
    const foo = true
    expect(foo).toBeTruthy() // 期望 foo 变量的值为 true
  })
})
```

当然这是个白痴没有意义的测试，运行`yarn test`，测试用例通过，如果我们修改代码

```javascript
describe('单元测试', () => {
  it('测试用例', () => {
    const foo = true
    expect(foo).toBeFalsy() // 测试不会通过
  })
})
```

此时`yarn test`不会通过，这就对应我们日常修改代码后，假设你没有重新运行测试（但测试用例实际是运行错误的），你却 push 上去了，又合并了就麻烦了，因为测试不过说明你代码可能存在问题。

接着需要 Travis 帮忙了，修改`.travis.yml`文件

```yaml
language: node_js

node_js:
  - 12

cache: yarn

install:
  - yarn
script:
  - yarn test # 运行测试
  - yarn build
```

- 接着同上步骤，开个 PR，此时显示如下，此时就看到 Travis 显示`Required`，但因为我是项目唯一拥有者兼管理员，所以依然可以直接 merge，但如果其他人 PR 上来，则是不行的要等 Travis 通过才允许按 merge 按钮

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoiswkaggj60kc06zmxv02.jpg)

等一会你会发现它挂了，这里就显示`×`号了，此时失败不能合并（非管理员）。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoisvlahrj60kc047t8w02.jpg)

我们去`Details`里面看看哪里出错了，看到是测试用例出错导致 Travis 无法通过：

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvoistr83uj60kc0hu75b02.jpg)

- 于是此时我们应该是去修改代码，再提交，这就是 Travis 的其中一个常见的作用了。
- 其后它还有很多其它可以玩的，比如自动化部署到 Github page 页面，在我现在的 React 开源组件库 [monki-ui - travis 配置](https://link.segmentfault.com/?enc=i23AOOQgFebuEA7C4FLtbg%3D%3D.4%2B4KB0ksOWFlbUmvWoI4NK6tGAate1ps3ieEEJhN7dy9JWwIsATzrtO7Hb5GgoGI79QDizGcgJ3YCVKjTMoUMc%2BXX5a40rHlMobBY2rA61g%3D) 作为例子

```yaml
deploy:
  provider: pages  # 指定部署到Github Pages，即 gh-pages分支
  github_token: $GITHUB_TOKEN # 名字对应我们上方的 token
  skip_cleanup: true # 指定保留构建后的文件
  keep-history: true # 指定每次部署会新增一个提交记录再推送，而不是使用 git push --force
  local_dir: docs-dist # 指定构建后要部署的目录
  on:
    branch: master # 指定 master 分支有提交行为时，将触发构建后部署
```

- 本文介绍到这里就结束了，Travis 还有很多其他有用的配置，就需要日常开发自己去了解了。
- 本文案例代码地址[travis-demo](https://link.segmentfault.com/?enc=YjOLRgCNS2hhYFLiLekKlQ%3D%3D.8J1u%2FD%2BnwfT1k%2FXA5i2WGClFGdVZdfbSc%2BkL5j2Y6OEjdaLW8qCGlfzXqMroO1mp)
  参考文章：
- [持续集成服务 Travis CI 教程](https://link.segmentfault.com/?enc=qKduNyxT1x2uAbbIj%2BB8uQ%3D%3D.gmE3xQ2wzVXASLhF4hW3d4MFrselmwvo4hXtVj2EmjWhFs%2FHlFd2UzI%2BsD9%2FL4tPXhJIAZE8NlgFiK9N%2F6iTyg%3D%3D)