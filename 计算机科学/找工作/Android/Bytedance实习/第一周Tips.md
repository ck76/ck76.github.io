- **aapt** :

  -  <https://www.jianshu.com/p/af528e3ae55c>

  - <https://www.jianshu.com/p/37e31d924be9>

- **Build Variant(变种)**

  - https://www.jianshu.com/p/98ee75dd49f4
  - https://www.jianshu.com/p/cce65d1352c1

- **Instant run(立即允许)**

- **[aar是什么](https://blog.csdn.net/qq_32452623/article/details/79220522)：**

  在使用 Eclipse 开发 Android 的那个时代（其实也就几年前而已），如果想代码打包，只有 JAR 包一个方法，但是 JAR 只能把 Java 文件代码打包进去，如果要使用一个有布局和资源的库的话，除了将 JAR 放入 libs 外,还需要引入相关的资源和配置文件，十分不优雅。Android Studio 出来之后，出现了一个新的方法，打包 AAR 文件 ，这个**不仅可以把 Java 文件给打进去，还包含 AndroidManifest.xml 和资源文件等，使用的话，直接引入一个 AAR 文件就足够了，非常方便。**

- **DI(依赖注入) Dependency injection**

- **noop(空操作)**

- **[灰度测试](http://www.appadhoc.com/blog/what-is-grey-release/) ：**

  其实灰度测试就是指如果软件要在不久的将来推出一个全新的功能，或者做一次比较重大的改版的话，要先进行一个小范围的尝试工作，然后再慢慢放量，直到这个全新的功能覆盖到所有的系统用户，也就是说在新功能上线的黑白之间有一个灰，所以这种方法也通常被称为灰度测试。

- **AB测试：**

  AB测试是为[Web](https://baike.baidu.com/item/Web/150564)或[App](https://baike.baidu.com/item/App/6133292)界面或流程制作两个（A/B）或多个（A/B/n）版本，在同一时间维度，分别让组成成分相同（相似）的访客群组（目标人群）随机的访问这些版本，收集各群组的用户体验数据和业务数据，最后分析、评估出最好版本，正式采用。

- **编译速度慢：**

  现状 想必每个做Android的都有一个痛：编译时间太长，编译时电脑呼呼作响，期间电脑卡的动不了...为此，业界出了很多解决办法，大致可以分为以下几类：

  1. 修改配置 通过修改Android studio和Gradle配置，加快构建。具体配置方法可参考官方的建议[优化您的构建速度](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Fstudio%2Fbuild%2Foptimize-your-build%3Fhl%3Dzh-cn),效果还是挺明显的。
  2. 优化代码 编译时间长，一方面也是因为项目规模较大。通过移除无用代码，模块化，引用aar等手段，也可以有效降低编译速度。
  3. 增量编译 增量编译的解决方案目前有很多：Instant-Run、Freeline、buck、LayoutCast等，但基本上都有一些问题，对于资源改动、databinding、kotlin等支持都不是很完美。经常有一些莫名其妙的奇怪问题。
  4. 提升硬件配置 这个是最有效且最简单的方法。 如果你是土豪，请随意加内存、加固态硬盘，然后本文就到此结束。 else 看看下面的解决方案。

- **[加快apk的构建速度，如何把编译时间从130秒降到17秒](https://www.jianshu.com/p/53923d8f241c)**

- **[互联网行业职位介绍——PM,RD,FE,UE,UI,QA,OP,DBA,BRD,MRD, PRD,FSD等：](https://blog.csdn.net/dr_guo/article/details/50616086)**

  - <https://wiki.bytedance.net/pages/viewpage.action?pageId=201209577>

  - PM项目经理( Project Manager )
  - PR（Public relations）公共关系师
  - HR（HR-Human Resource）人力资源
  - HRBP(HR BUSINESS PARTNER) 人力资源业务合作伙伴
  - FE前端（Front-End）；前端开发（Front-End Development）
  - BE(Back-End)(Server) 后端
  - Android 安卓
  - IOS 苹果
  - QA测试（QUALITY ASSURANCE，中文意思是“质量保证”）
  - RD研发（Research and Development）
  - UE用户体验（User Experience，简称UX或 UE）
  - UI用户界面（User Interface）
  - OP运维（Operations）
  - DBA数据库管理员（Database Administrator，简称DBA）
  - MRD市场需求文档（Market Requirements Document）
  - PRD产品需求文档（Product Requirements Document）

- **git 克隆时http和ssh（Secure Shell）区别**

  在管理Git项目上，很多时候都是直接使用https url克隆到本地，当然也有有些人使用SSH url克隆到本地。这两种方式的主要区别在于：使用https url克隆对初学者来说会比较方便，复制https url然后到git Bash里面直接用clone命令克隆到本地就好了，但是每次fetch和push代码都需要输入账号和密码，这也是https方式的麻烦之处。而使用SSH url克隆却需要在克隆之前先配置和添加好SSH key，因此，如果你想要使用SSH url克隆的话，你必须是这个项目的拥有者。否则你是无法添加SSH key的，另外ssh默认是每次fetch和push代码都不需要输入账号和密码，如果你想要每次都输入账号密码才能进行fetch和push也可以另外进行设置

- [**Java8(RxJava)双冒号::**](https://www.jianshu.com/p/6da1601a3b15)

  - 双冒号 **:: ，**是对 () -> { } 方式的进一步简化，也又是一种语法糖

  ```java
  mButton.setOnClickListener( v  ->  onButtonClick( v ) );
  //改变后
  mButton.setOnClickListener(this::onButtonClick);
  ```

- **AntiSpam**

  - 反作弊

- **git 命令**

  - commit -s

  - add -u：文件删除后，在用git add -u后，就能看见文件已经被提交到暂存区了 
    这个就可以以备不时之需，假如文件删错了，还能恢复回来
  - add -A：提交所有包括被删除的文件信息，被替换的文件信息、被修改的文件以及新增的文件到暂存区 
    其中删除。修改以及新增文件到缓存区和git add .和git add -u是相同的，同时具有他们的功能，还具有替换的文件的功能

- **[Jenkins-Continuous integration（CI）](https://www.cnblogs.com/jimmy-xuli/p/9020825.html)**

  持续集成是一种软件开发实践，即团队开发成员经常集成他们的工作，通常每个成员至少集成一次，也就意味着每天可能会发生多次集成。每次集成都通过自动化的构建（包括编译，发布，自动化测试）来验证，从而尽快地发现集成错误。许多团队发现这个过程可以大大减少集成的问题，让团队能够更快的开发内聚的软件。

- **火山编译失败：**
  - 拉错仓库
  - 编译选项将instant run取消
  - 所有模块Build Variant改为cnFullDebug


