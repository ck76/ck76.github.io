https://blog.csdn.net/lanxian837820149/article/details/88786036

https://blog.csdn.net/weixin_34414196/article/details/91441948



[TOC]



通过最近对 Flutter 开发的大致了解，感受最深的简单概括就是：Widget 就是一切外加组合和响应式，我们开发的界面，通过组合其他的 Widget 来实现，当界面发生变化时，不会像我们原来 iOS 或者 Andriod 开发一样去直接修改 UI 元素，而是去去更新状态，根据新的状态来完成 UI 的构建，这一点是不是跟 Vue、React 很像。

关于 Widget 的具体使用，我也不打算去写，如果需要对某个知识点需要深入探索下的话，可能会记录下，其实对 Widget 学习大家可以参考 Flutter Api 文档地址如下：[api.flutter.dev/](https://link.juejin.im/?target=https%3A%2F%2Fapi.flutter.dev%2F) ，如果看英文吃力的话也可以中文网站 [flutterchina.club/docs/](https://link.juejin.im/?target=https%3A%2F%2Fflutterchina.club%2Fdocs%2F) 。

要理解 Flutter 跨平台实现的原因，先了解下屏幕显示图像的基本原理。

屏幕显示器都是有一个个的物理显示单元构成，每个物理单元称之为一个像素点，每一个像素点可以承载多种颜色的显示，屏幕显示器能够显示图像就是因为不同的像素点上呈现的不同的颜色，最终呈现出来一个完整的图像。

通常说的同一面积屏幕显示器，屏幕分辨率越高，显示器可以呈现的元素就会越多，我们看到的画面就会越清晰。

我们知道手机屏幕刷新频率是 60Hz ，当一帧画面显示完成后，准备下一帧时，显示器会发出一个垂直同步信号，这样在 1s 内显示就完成了 60 次这样的操作，而每次操作中，都是完成 CPU 将计算好的内容，同步到 GPU ，GPU 对要显示的内容进过渲染操作，放入到了缓冲区中，等待显示器去显示，整个操作都是有操作系统的硬件系统来完成的。

iOS 、Andriod移动设备的 GUI 显示都是这样一个原理，其实不同操作系统 UI 控件，只是操作系统去操作硬件系统 API 的一层封装，如果我们直接操作底层 API 去完成 GUI 开发是一件非常痛苦的事。

Flutter 只是用一种编程语言，也就是采用一套Dart API ，底层通过 OpenGL（操作系统 API 的一个封装库）这种跨平台的绘制库，来实现一套代码跨端使用，也就是说 Flutter 所谓跨平台只是 Dart 调用 OpenGL ，然后 OpenGL 再去调用操作系统底层的 API 。跟 ReactNative 、weex 不同的是，他们需要依赖 JavaScriptCore 引擎去跟原生应用的通信。

当然我们不去谈论方案的优劣，本身他们选择的方向就不同

---

一直以来，跨平台开发都是困扰移动客户端开发的难题。

在马蜂窝旅游 App 很多业务场景里，我们尝试过一些主流的跨平台开发解决方案， 比如WebView 和 React Native，来提升开发效率和用户体验。但这两种方式也带来了新的问题。

比如使用 WebView 跨平台方式，优点确实非常明显。基于 WebView 的框架集成了当下 Web 开发的诸多优势：丰富的控件库、动态化、良好的技术社区、测试自动化等等。但是缺点也同样明显：渲染效率和 JavaScript 的执行能力都比较差，使页面的加载速度和用户体验都不尽如人意。

而使用以 React Native（简称 RN）为代表的框架时，维护又成了大难题。RN 使用类 HTML+JS 的 UI 创建逻辑，生成对应的原生页面，将页面的渲染工作交给了系统，所以渲染效率有很大的优势。但由于 RN 代码是通过 JS 桥接的方式转换为原生的控件，所以受各个系统间的差异影响非常大，虽然可以开发一套代码，但对各个平台的适配却非常的繁琐和麻烦。

## 为什么是 Flutter

2018 年 12 月初，Google 正式发布了开源跨平台 UI 框架 Flutter 1.0 Release 版本，马蜂窝电商客户端团队进行了调研与实践，发现Flutter能很好的帮助我们解决开发中遇到的问题。

跨平台开发，针对 Android 与 iOS 的风格设计了两套设计语言的控件实现（Material & Cupertino）。这样不但能够节约人力成本，而且在用户体验上更好的适配 App 运行的平台。

重写了一套跨平台的 UI 框架，渲染引擎是依靠 Skia 图形库实现。Flutter 中的控件树直接由渲染引擎和高性能本地 ARM 代码直接绘制，不需要通过中间对象（Web 应用中的虚拟 DOM 和真实 DOM，原生 App 中的虚拟控件和平台控件）来绘制，使它有接近原生页面的性能，帮助我们提供更好的用户体验。

同时支持 JIT 和 AOT 编译。JIT编译方式使其在开发阶段有个备受欢迎的功能——热重载（HotReload），这样在开发时可以省去构建的过程，提高开发效率。而在 Release 运行阶段采用 AOT 的编译方式，使执行效率非常高，让 Release 版本发挥更好的性能。

于是，电商客户端团队决定探索 Flutter 在跨平台开发中的新可能，并率先应用于商家端 App 中。在本文中，我们将结合 Flutter 在马蜂窝商家端 App 中的应用实践，探讨 Flutter 架构的实现原理，有何优势，以及如何帮助我们解决问题。

## Flutter 架构和实现原理

Flutter 使用 Dart 语言开发，主要有以下几点原因：

- Dart 一般情况下是运行 DartVM 上，但是也可以编译为 ARM 代码直接运行在硬件上。
- Dart 同时支持 AOT 和 JIT 两种编译方式，可以更好的提高开发以及 App 的执行效率。
- Dart 可以利用独特的隔离区（Isolate）实现多线程。而且不共享内存，可以实现无锁快速分配。
- 分代垃圾回收，非常适合 UI 框架中常见的大量 Widgets 对象创建和销毁的优化。
- 在为创建的对象分配内存时，Dart 是在现有的堆上移动指针，保证内存的增长是程线性的，于是就省了查找可用内存的过程。

Dart 主要由 Google 负责开发和维护。目前 Dart 最新版本已经是 2.2，针对 App 和 Web 开发做了很多优化。并且对于大多数的开发者而言，Dart 的学习成本非常低。

Flutter 架构也是采用的分层设计。从下到上依次为：Embedder（嵌入器）、Engine、Framework。

![Flutter 分层架构图 ](https://img-blog.csdnimg.cn/20190325091054837.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

**Embedder** 是嵌入层，做好这一层的适配 Flutter 基本可以嵌入到任何平台上去；

**Engine** 层主要包含 Skia、Dart 和 Text。Skia 是开源的二位图形库；Dart 部分主要包括 runtime、Garbage Collection、编译模式支持等；Text 是文本渲染。

Framework 在最上层。我们的应用围绕 Framework 层来构建，因此也是本文要介绍的重点。

### Framework

1.【Foundation】在最底层，主要定义底层工具类和方法，以提供给其他层使用。

2.【Animation】是动画相关的类，可以基于此创建补间动画（Tween Animation）和物理原理动画（Physics-based Animation），类似 Android 的 ValueAnimator 和 iOS 的 Core Animation。

3.【Painting】封装了 Flutter Engine 提供的绘制接口，例如绘制缩放图像、插值生成阴影、绘制盒模型边框等。

4.【Gesture】提供处理手势识别和交互的功能。

5.【Rendering】是框架中的渲染库。控件的渲染主要包括三个阶段：布局（Layout）、绘制（Paint）、合成（Composite）。

从下图可以看到，Flutter 流水线包括 7 个步骤。

![Flutter 流水线](https://img-blog.csdnimg.cn/20190325091137281.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

首先是获取到用户的操作，然后你的应用会因此显示一些动画，接着 Flutter 开始构建 Widget 对象。

Widget 对象构建完成后进入渲染阶段，这个阶段主要包括三步：

- 布局元素：决定页面元素在屏幕上的位置和大小；
- 绘制阶段：将页面元素绘制成它们应有的样式；
- 合成阶段：按照绘制规则将之前两个步骤的产物组合在一起。

最后的光栅化由 Engine 层来完成。

在渲染阶段，控件树（widget）会转换成对应的渲染对象（RenderObject）树，在 Rendering 层进行布局和绘制。

在布局时 Flutter 深度优先遍历渲染对象树。数据流的传递方式是从上到下传递约束，从下到上传递大小。也就是说，父节点会将自己的约束传递给子节点，子节点根据接收到的约束来计算自己的大小，然后将自己的尺寸返回给父节点。整个过程中，位置信息由父节点来控制，子节点并不关心自己所在的位置，而父节点也不关心子节点具体长什么样子。

![数据流传递方式](https://img-blog.csdnimg.cn/2019032509124379.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

为了防止因子节点发生变化而导致的整个控件树重绘，Flutter 加入了一个机制——**Relayout Boundary**，在一些特定的情形下Relayout Boundary会被自动创建，不需要开发者手动添加。

例如，控件被设置了固定大小（tight constraint）、控件忽略所有子视图尺寸对自己的影响、控件自动占满父控件所提供的空间等等。很好理解，就是控件大小不会影响其他控件时，就没必要重新布局整个控件树。有了这个机制后，无论子树发生什么样的变化，处理范围都只在子树上。

![Relayout Boundary 机制](https://img-blog.csdnimg.cn/201903250913137.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

在确定每个空间的位置和大小之后，就进入绘制阶段。绘制节点的时候也是深度遍历绘制节点树，然后把不同的 RenderObject 绘制到不同的图层上。

这时有可能出现一种特殊情况，如下图所示节点 2 在绘制子节点 4 时，由于其节点4需要单独绘制到一个图层上（如 video），因此绿色图层上面多了个黄色的图层。之后再需要绘制其他内容（标记 5）就需要再增加一个图层（红色）。再接下来要绘制节点 1 的右子树（标记 6），也会被绘制到红色图层上。所以如果 2 号节点发生改变就会改变红色图层上的内容，因此也影响到了毫不相干的 6 号节点。

![绘制节点与图层的关系](https://img-blog.csdnimg.cn/20190325091332374.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

为了避免这种情况，Flutter 的设计者这里基于 Relayout Boundary 的思想增加了 Repaint Boundary。在绘制页面时候如果遇见 Repaint Boundary 就会强制切换图层。

如下图所示，在从上到下遍历控件树遇到 Repaint Boundary 会重新绘制到新的图层（深蓝色），在从下到上返回的时候又遇到 Repaint Boundary，于是又增加一个新的图层（浅蓝色）。

![Repaint Boundary 机制](https://img-blog.csdnimg.cn/20190325091348545.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

这样，即使发生重绘也不会对其他子树产生影响。比如在 Scrollview 上，当滚动的时候发生内容重绘，如果在 Scrollview 以外的地方不需要重绘就可以使用 Repaint Boundary。Repaint Boundary 并不会像 Relayout Boundary 一样自动生成，而是需要我们自己来加入到控件树中。

6.【Widget】控件层。所有控件的基类都是 Widget，Widget 的数据都是只读的, 不能改变。所以每次需要更新页面时都需要重新创建一个新的控件树。每一个 Widget 会通过一个 RenderObjectElement 对应到一个渲染节点（RenderObject），可以简单理解为 Widget 中只存储了页面元素的信息，而真正负责布局、渲染的是 RenderObject。

在页面更新重新生成控件树时，RenderObjectElement 树会尽量保持重用。由于 RenderObjectElement 持有对应的 RenderObject，所有 RenderObject 树也会尽可能的被重用。如图所示就是三棵树之间的关系。在这张图里我们把形状当做渲染节点的类型，颜色是它的属性，即形状不同就是不同的渲染节点，而颜色不同只是同一对象的属性的不同。

![ Widget、Element 和 Render 之间的关系](https://img-blog.csdnimg.cn/20190325091405460.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

如果想把方形的颜色换成黄色，将圆形的颜色变成红色，由于控件是不能被修改的，需要重新生成两个新的控件 Rectangle yellow 和 Circle red。由于只是修改了颜色属性，所以 Element 和 RenderObject 都被重用，而之前的控件树会被释放回收。

![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325091421603.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

那么如果把红色圆形变成三角形又会怎样呢？由于这里发生变化的是类型，所以对应的 Element 节点和 RenderObject 节点都需要重新创建。但是由于黄色方形没有发生改变，所以其对应的 Element 节点和 RenderObject 节点没有发生变化。

![图9: 示例](https://img-blog.csdnimg.cn/20190325091433430.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

1. 最后是【Material】 & 【Cupertino】，这是在 Widget 层之上框架为开发者提供的基于两套设计语言实现的 UI 控件，可以帮助我们的 App 在不同平台上提供接近原生的用户体验。

![马蜂窝商家端使用 Flutter 开发的页面](https://img-blog.csdnimg.cn/20190325091455820.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

### 开发方式：Flutter + Native

由于商家端已经是一款成熟的 App，不可能创建一个新的 Flutter 工程全部重新开发，因此我们选择 Native 与 Flutter 混编的方案来实现。

在了解 Native 与 Flutter 混编方案前，首先我们需要了解在 Flutter 工程中，通常有以下 4 种工程类型：

1. Flutter Application
   标准的 Flutter App 工程，包含标准的 Dart 层与 Native 平台层。
2. Flutter Module
   Flutter 组件工程，仅包含 Dart 层实现，Native 平台层子工程为通过 Flutter 自动生成的隐藏工程（.ios / .android）。
3. Flutter Plugin
   Flutter 平台插件工程，包含 Dart 层与 Native 平台层的实现。
4. Flutter Package
   Flutter 纯 Dart 插件工程，仅包含 Dart 层的实现，往往定义一些公共 Widget。

了解了 Flutter 工程类型后，我们来看下官方提供的一种混编方案（https://github.com/flutter/flutter/wiki/Add-Flutter-to-existing-apps），即在现有工程下创建 Flutter Module 工程，以本地依赖的方式集成到现有的 Native 工程中。

官方集成方案（以 iOS 为例）

a. 在工程目录创建 FlutterModule，创建后，工程目录大致如下：
![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325091545223.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

b. 在 Podfile 文件中添加以下代码：

```
flutter_application_path = '../flutter_Moudule/' 
eval(File.read(File.join(flutter_application_path, '.ios', 'Flutter', 'podhelper.rb')), binding)
12
```

该脚本主要负责：

- pod 引入 Flutter.Framework 以及 FlutterPluginRegistrant 注册入口
- pod 引入 Flutter 第三方 plugin
- 在每一个 pod 库的配置文件中写入对 Generated.xcconfig 文件的导入
- 修改 pod 库的 ENABLE_BITCODE = NO（因为 Flutter 现在不支持 bitcode）

c. 在 iOS 构建阶段 Build Phases 中注入构建时需要执行的 xcode_backend.sh (位于 FlutterSDK/packages/flutter_tools/bin) 脚本:

```
"$FLUTTER_ROOT/packages/flutter_tools/bin/xcode_backend.sh" build
"$FLUTTER_ROOT/packages/flutter_tools/bin/xcode_backend.sh" embed
12
```

该脚本主要负责：

- 构建 App.framework 以及 Flutter.framework 产物
- 根据编译模式（debug/profile/release）导入对应的产物
- 编译 flutter_asset 资源
- 把以上产物 copy 到对应的构建产物中

d. 与 Native 通信

- 方案一：改造 AppDelegate 继承自 FlutterAppDelegate
- 方案二：AppDelegate 实现 FlutterAppLifeCycleProvider 协议，生命周期由 FlutterPluginAppLifeCycleDelegate 传递给 Flutter

以上就是官方提供的集成方案。我们最终没有选择此方案的原因，是它直接依赖于 FlutterModule 工程以及 Flutter 环境，使 Native 开发同学无法脱离 Flutter 环境开发，影响正常的开发流程，团队合作成本较大；而且会影响正常的打包流程。（目前 Flutter 团队正在重构嵌入 Native 工程的方式）

最终我们选择另一种方案来解决以上的问题：远端依赖产物。

![图11 ：远端依赖产物](https://img-blog.csdnimg.cn/20190325091655675.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

### iOS 集成方案

通过对官方混编方案的研究，我们了解到 iOS 工程最终依赖的其实是 FlutterModule 工程构建出的产物（Framework，Asset，Plugin），只需将产物导出并 push 到远端仓库，iOS 工程通过远端依赖产物即可。

依赖产物目录结构如下：

![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325091714650.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

- App.framework : Flutter 工程产物（包含 Flutter 工程的代码，Debug 模式下它是个空壳，代码在 flutter_assets 中）。
- Flutter.framework: Flutter 引擎库。与编译模式（debug/profile/release）以及 CPU 架构（arm*, i386, x86_64）相匹配。
- lib*.a & .h 头文件 : FlutterPlugin 静态库（包含在 iOS 端的实现）。
- flutter_assets: 包含 Flutter 工程字体，图片等资源。在 Flutter1.2 版本中，被打包到 App.framework 中。

### Android 集成方案

Android Nativite 集成是通过 Gradle 远程依赖 Flutter 工程产物的方式完成的，以下是具体的集成流程。

a. 创建 Flutter 标准工程

```
$ flutter create flutter_demo
1
```

默认使用 Java 代码，如果增加 Kotlin 支持，使用如下命令：

```
$ flutter create -a kotlin flutter_demo
1
```

b. 修改工程的默认配置
![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325091809660.png)

i. 修改 app module 工程的 build.gradle 配置 apply plugin: ‘com.android.application’ => apply plugin: ‘com.android.library’，并移除 applicationId 配置

ii. 修改 root 工程的 build.gradle 配置

在集成过程中 Flutter 依赖了三方 Plugins 后，遇到 Plugins 的代码没有被打进 Library 中的问题。通过以下配置解决（这种方式略显粗暴，后续的优化方案正在调研）。

```
subprojects {
   project.buildDir = "${rootProject.buildDir}/app"
}
123
```

iii. app module 增加 maven 打包配置

c. 生成 Android Flutter 产物

```
$ cd android
$ ./gradlew uploadArchives
12
```

官方默认的构建脚本在 Flutter 1.0.0 版本存在 Bug——最终的产物中会缺少 flutter_shared/icudtl.dat 文件，导致 App Crash。目前的解决方式是将这个文件复制到工程的 assets 下（ 在 Flutter 最新 1.2.1 版本中这个 Bug 已被修复，但是 1.2.1 版本又出现了一个 UI 渲染的问题，所以只能继续使用 1.0.0 版本）。

d. Android Native 平台工程集成，增加下面依赖配置即可，不会影响 Native 平台开发的同学

implementation ‘com.mfw.app:MerchantFlutter:0.0.5-beta’

### Flutter 和 iOS、Android 的交互

使用平台通道（Platform Channels）在 Flutter 工程和宿主（Native 工程）之间传递消息，主要是通过 MethodChannel 进行方法的调用，如下图所示：

![图12 ：Flutter与iOS、Android交互](https://img-blog.csdnimg.cn/20190325091909607.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

为了确保用户界面不会挂起，消息和响应是异步传递的，需要用 async 修饰方法，await 修饰调用语句。Flutter 工程和宿主工程通过在 Channel 构造函数中传递 Channel 名称进行关联。单个应用中使用的所有 Channel 名称必须是唯一的; 可以在 Channel 名称前加一个唯一的「域名前缀」。

### Flutter 与 Native 性能对比

我们分别使用 Native 和 Flutter 开发了两个列表页，以下是页面效果和性能对比：

iOS 对比（机型 6P 系统 10.3.3）：
Flutter 页面：
![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325091928459.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)
![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325091940441.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

iOS Native 页面：
![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325092014702.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)
![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325092019932.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

可以看到，从使用和直观感受都没有太大的差别。于是我们采集了一些其他方面的数据。

Flutter 页面：

![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325092026770.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

iOS Native 页面：

![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325092033752.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

![在这里插入图片描述](https://img-blog.csdnimg.cn/2019032509204122.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

另外我们还对比了商家端接入 Flutter 前后包体积的大小：39Mb → 44MB

![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325092046911.png)

在 iOS 机型上，流畅度上没有什么差异。从数值上来看，Flutter 在 内存跟 GPU/CPU 使用率上比原生略高。 Demo 中并没有对 Flutter 做更多的优化，可以看出 Flutter 整体来说还是可以做出接近于原生的页面。

**下面是 Flutter 与 Android 的性能对比。**

Flutter 页面：
![在这里插入图片描述](https://img-blog.csdnimg.cn/2019032509210383.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

Android Native 页面：
![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325092109271.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

从以上两张对比图可以看出，不考虑其他因素，单纯从性能角度来说， 原生要优于 Flutter，但是差距并不大，而且 Flutter 具有的跨平台开发和热重载等特点极大地节省了开发效率。并且，未来的热修复特性更是值得期待。

## 混合栈管理

首先先介绍下 Flutter 路由的管理：

- Flutter 管理页面有两个概念：Route 和 Navigator。
- Navigator 是一个路由管理的 Widget（Flutter 中万物皆 Widget），它通过一个栈来管理一个路由 Widget 集合。通常当前屏幕显示的页面就是栈顶的路由。
- 路由 (Route) 在移动开发中通常指页面（Page），这跟 web 开发中单页应用的 Route 概念意义是相同的，Route 在 Android 中通常指一个 Activity，在 iOS 中指一个 ViewController。所谓路由管理，就是管理页面之间如何跳转，通常也可被称为导航管理。这和原生开发类似，无论是 Android 还是 iOS，导航管理都会维护一个路由栈，路由入栈 (push) 操作对应打开一个新页面，路由出栈 (pop) 操作对应页面关闭操作，而路由管理主要是指如何来管理路由栈。

![图14 ：Flutter 路由管理](https://img-blog.csdnimg.cn/20190325092135118.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

如果是纯 Flutter 工程，页面栈无需我们进行管理，但是引入到 Native 工程内，就需要考虑如何管理混合栈。并且需要解决以下几个问题：

1. 保证 Flutter 页面与 Native 页面之间的跳转从用户体验上没有任何差异
2. 页面资源化（马蜂窝特有的业务逻辑）
3. 保证生命周期完整性，处理相关打点事件上报
4. 资源性能问题

参考了业界内的解决方法，以及项目自身的实际场景，我们选择类似于 H5 在 Navite 中嵌入的方式，统一通过 openURL 跳转到一个 Native 页面（FlutterContainerVC），Native 页面通过 addChildViewController 方式添加 FlutterViewController（负责 Flutter 页面渲染），同时通过 channel 同步 Native 页面与 Flutter 页面。

![在这里插入图片描述](https://img-blog.csdnimg.cn/20190325092151344.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2xhbnhpYW44Mzc4MjAxNDk=,size_16,color_FFFFFF,t_70)

- 每一次的 push/pop 由 Native 发起，同时通过 channel 保持 Native 与 Flutter 页面同步——在 Native 中跳转 Flutter 页面与跳转原生无差异
- 一个 Flutter 页面对应一个 Native 页面（FlutterContainerVC) ——解决页面资源化
- FlutterContainerVC 通过 addChildViewController 对单例 FlutterViewController 进行复用——保证生命周期完整性，处理相关打点事件上报
- 由于每一个 FlutterViewController（提供 Flutter 视图的实现）会启动三个线程，分别是 UI 线程、GPU 线程和 IO 线程，使用单例 FlutterViewController 可以减少对资源的占用——解决资源性能问题

## Flutter 应用总结

Flutter 一经发布就很受关注，除了 iOS 和 Android 的开发者，很多前端工程师也都非常看好 Flutter 未来的发展前景。相信也有很多公司的团队已经投入到研究和实践中了。不过 Flutter 也有很多不足的地方，值得我们注意：

- 虽然 1.2 版本已经发布，但是目前没有达到完全稳定状态，1.2 发布完了就出现了控件渲染的问题。加上 Dart 语言生态小，学习资料可能不够丰富。
- 关于动态化的支持，目前 Flutter 还不支持线上动态性。如果要在 Android 上实现动态性相对容易些，iOS 由于审核原因要实现动态性可能成本很高。
- Flutter 中目前拿来就用的能力只有 UI 控件和 Dart 本身提供能力，对于平台级别的能力还需要通过 channel 的方式来扩展。
- 已有工程迁移比较复杂，以前沉淀的 UI 控件，需要重新再实现一套。
- 最后一点比较有争议，Flutter 不会从程序中拆分出额外的模板或布局语言，如 JSX 或 XM L，也不需要单独的可视布局工具。有的人认为配合 HotReload 功能使用非常方便，但我们发现这样代码会有非常多的嵌套，阅读起来有些吃力。

目前阿里的闲鱼开发团队已经将 Flutter 用于大型实践，并应用在了比较重要的场景（如产品详情页），为后来者提供了良好的借鉴。马蜂窝的移动客户端团队关于 Flutter 的探索才刚刚起步，前面还有很多的问题需要我们一点一点去解决。不过无论从 Google 对其的重视程度，还是我们从实践中看到的这些优点，都让我们对 Flutter 充满信心，也希望在未来我们可以利用它创造更多的价值和奇迹。

路途虽远，犹可期许。

**本文作者**：马蜂窝电商研发客户端团队。

---

本文主要介绍Flutter相关的东西，包括Fuchsia、Dart、Flutter特性、安装以及整体架构等内容。
**简介**
Flutter作为谷歌最近推出的跨平台开发框架，一经推出便吸引了不少注意。关于Flutter，目前我们知道它是一个跨平台开发框架。但是它本身并不止于此，例如Fuchsia、Dart等，我们也都需要去了解。
**Fuchsia**
说到Flutter，绝对绕不开Fuchsia，这个是谷歌开发的一款全新的操作系统，[GitHub地址](https://link.zhihu.com/?target=https%3A//github.com/fuchsia-mirror)以及[Google source主页](https://link.zhihu.com/?target=https%3A//fuchsia.googlesource.com/)。Fuchsia内核是Magenta Kernel，一个基于[LittleKernel](https://link.zhihu.com/?target=https%3A//github.com/littlekernel/lk)的项目。该系统与Android相比，无论是存储器还是内存之类的硬件要求都大幅降低，外界推论是一款面向物联网的系统。笔者倒是没有查到谷歌开发这款操作系统的目的，如果有知晓的，也烦请告知。
就像很多博客主说的那样，如果只是取代Android，那无疑是一种很不好的做法。任何技术的推动，都得靠背后的商业驱动，尤其是这种涉及到手机厂商利益的技术。
**Flutter**
Flutter是Fuchsia的开发框架，是一套移动UI框架，可以快速在iOS、Android以及Fuchsia上构建高质量的原生用户界面。 目前Flutter是完全免费、开源的，[GitHub地址](https://link.zhihu.com/?target=https%3A//github.com/flutter/flutter)。其官方编程语言为Dart，也是一门全新的语言。所以说，上手成本比较高，对于移动端开发人员，语言以及框架都是全新的，整个技术栈的积累也都得从头开始。
可以看下其官方介绍的特性：

- 快速开发：Flutter的热重载可以快速地进行测试、构建UI、添加功能并更快地修复错误。
- 富有表现力，漂亮的用户界面：自带的Material Design和Cupertino（iOS风格）widget、丰富的motion API、平滑而自然的滑动效果。
- 响应式框架：使用Flutter的现代、响应式框架，和一系列基础widget，轻松构建您的用户界面。
- 访问本地功能和SDK：Flutter可以复用现有的Java、Swift或ObjC代码，访问iOS和Android上的原生系统功能和系统SDK。
- 统一的应用开发体验：Flutter拥有丰富的工具和库，可以帮助开发者轻松地同时在iOS和Android系统中实现想法和创意。
- 原生性能：Flutter包含了许多核心的widget，如滚动、导航、图标和字体等，这些都可以在iOS和Android上达到原生应用一样的性能。

其实从官方特性来看，唯一有点吸引力的就是统一的应用开发体验。一套代码运行在多个平台，做到真正的跨平台。像热加载，目前Android开发本身就支持了，响应式框架以及访问本地功能和SDK，对于Native来说，本身并没有多大的吸引。至于漂亮的用户界面，国内的商业项目，哪一个会去按照Material Design去设计。
跨平台本身，往往意味着性能受损，通用性解决不了的问题，又得回到Native去实现。所以这些因素也是跨平台从移动端诞生之初就开始提，到现在也没有被很好解决的一个原因。至于谷歌能够做到什么程度，或者说开发者该保持什么期许，我觉得都不好说，或许谷歌解决了一个多年的难题，或者又像被谷歌放弃掉的其他项目一样。抛开商业层面，对于技术人员，我们更多的是应该去关注它的思想，谷歌是如何去解决这些实际存在很久的问题的，至于技术本身的发展，这个是个人开发者无法去左右的，技术的更迭，保持一种学习的状态，然后努力锻炼身体，就能够保证不被淘汰掉。
**Dart**
Dart是谷歌开发的计算机编程语言，于2011年10月份发布，可以被用于web、服务器、移端和物联网等领域的开发。Flutter采用Dart，原因很多，抛开商业层面的Java版权问题，单纯从技术层面：

- Dart是AOT（Ahead Of Time）编译的，编译成快速、可预测的本地代码，使Flutter几乎都可以使用Dart编写；
- Dart也可以JIT（Just In Time）编译，开发周期快；
- Dart可以更轻松地创建以60fps运行的流畅动画和转场；
- Dart使Flutter不需要单独的声明式布局语言；
- Dart容易学习，具有静态和动态语言用户都熟悉的特性。

Dart最初设计是为了取代JavaScript成为web开发的首选语言，最后的结果可想而知，到Dart 2的发布，专注于改善构建客户端应用程序的体验，可以看出定位的转变。用过Java、Kotlin的人，可以很快的上手Dart。
一门语言的成败，抛开背后的商业推动，我想很重要的一点在于其生态，生态的好坏，主要包括开发者以及第三方库的数目，目前看，Dart的生态还是比较差。对于个人开发者，可以根据心情来选择，但是对于商业应用，有更复杂的考量标准。Dart背后有谷歌的推动，能发展到什么程度，还得看其商业运作能力了。
**配置**
此部分针对Mac平台，[Windows平台的安装配置](https://link.zhihu.com/?target=https%3A//flutterchina.club/setup-windows/)，[Linux平台的安装配置](https://link.zhihu.com/?target=https%3A//flutterchina.club/setup-linux/)。由于笔者主要做移动端开发，如果想使用Flutter进行iOS和Android全平台的开发，环境也建议是Mac平台，毕竟iOS只能在Mac下进行模拟调试。
**安装Flutter**

```text
git clone -b beta https://github.com/flutter/flutter.git 
export PUB_HOSTED_URL=https://pub.flutter-io.cn //国内用户需要设置 
export FLUTTER_STORAGE_BASE_URL=https://storage.flutter-io.cn //国内用户需要设置 
export PATH=`pwd`/flutter/bin:$PATH
```

**iOS设置**

```text
 brew update 
brew install --HEAD libimobiledevice 
brew install ideviceinstaller ios-deploy cocoapods 
pod setup 
```


**Android设置**
下载Android Studio，安装Flutter插件，会将Dart插件也一起安装。
**体验Flutter**
IDE建议选择Android Studio，安装了Flutter插件后，Flutter的开发跟Android 开发类似，附带三种模版工程、断点调试等。
在Android Studio里面新建一个Flutter Application的项目，选择模拟器或者直接连接真机运行，就可以看到一个简单的Flutter应用了，可以在Android和iOS不同平台下看看差异。
**Flutter架构**
Flutter是一款移动应用程序SDK，一份代码可以同时生成iOS和Android两个高性能、高保真的应用程序。

![img](https://pic2.zhimg.com/v2-5b34685f095e3facae5dcaffa8453495_r.jpg)


Flutter对于移动开发人员，最诱惑的能力是其完全的跨平台特性，不同于RN这种一处学到处写，它是一处写到出跑，但是他跟其他的跨平台有何区别呢？
**跨平台解决方案**
市面上的跨平台解决方案，可以大致归结为两类：

- 使用平台支持的web技术：这些解决方案基本上加载了应用程序中的移动浏览器，并在该浏览器中执行所有的逻辑，例如PhoneGap。
- 本地跨平台：程序员编写的代码自动转换为Native代码，这种方式的优点是近乎原生的性能，例如RN、Weex、Xamarin等。

这些方案是否真正的解决了跨平台问题呢？从目前的状况来看，很显然是没有的，因为它们都始终逃不开性能、包大小、流畅性、内存、平台特性等问题。

![img](https://pic3.zhimg.com/v2-77112a183800e1f92e7db55ce96a2126_r.jpg)


RN单独拧出来说，是因为它们并不是追求的一次写到处跑，FB自己也知道不现实，所以把口号改成一次学到处写，去考虑平台的特性，去考虑这个被跨平台方案经常忽略的问题。但是RN也并没有被广泛的接纳，从阿里开始使用到放弃，里面的很多坑都绕不过去。写一次到处跑确实很诱人，从企业角度讲，可以节省大量的人力，但是却忽略了一个很基础的问题，不同平台是否希望如此，苹果是否会愿意自己的生态被打破，不同平台的特性是否应该被归为一致。
**Flutter的跨平台解决方案**
上面简单说了传统跨平台解决方案，我们再回过头看看Flutter的解决方案，Flutter跨平台最核心的部分，是它的高性能渲染引擎（Flutter Engine）。Flutter不使用浏览器技术，也不使用Native的原生控件，它使用自己的渲染引擎来绘制widget。
说到widget，就要说一句Flutter的`一切皆为widget`理念。widget是Flutter应用程序用户界面的基本构建块。每个widget都是用户界面一部分的不可变声明。与其他将视图、控制器、布局和其他属性分离的框架不同，Flutter具有一致的统一对象模型：widget。在更新widget的时候，框架能够更加的高效。
对于Android平台，Flutter引擎的C/C++代码是由NDK编译，在iOS平台，则是由LLVM编译，两个平台的Dart代码都是AOT编译为本地代码，Flutter应用程序使用本机指令集运行。
Flutter正是是通过使用相同的渲染器、框架和一组widget，来同时构建iOS和Android应用，而无需维护两套独立的代码库。

![img](https://pic2.zhimg.com/v2-56a721746c53ecfbc6ac81e3cfda4b4d_r.jpg)


Flutter将UI组件和渲染器从平台移动到应用程序中，这使得它们可以自定义和可扩展。Flutter唯一要求系统提供的是canvas，以便定制的UI组件可以出现在设备的屏幕上。
Flutter框架
Flutter框架是一个分层的结构，每个层都建立在前一层之上。

![img](https://pic3.zhimg.com/v2-f103669c5188961753cff8f66bc3a06e_r.jpg)


框架没什么可介绍的（主要是详细介绍我也没找到啥资料，大写的尴尬），看着很简单，就分为两个部分，Framework和Engine部分，其中Framework提供了各种基础的组件库，Engine部分渲染各种widget，两者共同作用，使得运行性能高效稳定。
**Flutter调研**
**生态**
在Flutter官方的[Pub](https://link.zhihu.com/?target=https%3A//pub.dartlang.org/)平台上，纯Flutter Package大概有两千多个，基本上常见的库还是都有的，例如网络、图片、音视频播放等等。但是对于目前Android以及iOS的生态，还是远远的不足的，对于一些复杂的UI或者一些不是特别通用的功能，就得自己去实现。
**包大小**
根据官网的介绍，一个最小的Android版本的Flutter应用。release版本大小约6.7MB，其中核心引擎大约3.3MB，框架+应用程序代码大约是1.25MB，LICENSE文件（包含在app.flx中）是55k，必需的Java代码.dex为40k，并且约有2.1MB的ICU数据。考虑到目前网络环境，包大小的增加，还也在可以接受的范围。
**Crash**
iOS运行官方的例子，会有时候crash掉，因此我们将一个[开源的Flutter应用](https://link.zhihu.com/?target=https%3A//github.com/roughike/inKino)，添加了Bugly上报，在Android平台进行了众测。

![img](https://pic2.zhimg.com/v2-941da88caca21d5895b93d1f56844431_r.jpg)


参与人次大概150人左右，启动次数大概500次左右，没有出现一次Crash数据上报，由于app很简单，并不能说明很多问题，但是众测用户反馈了约12条信息，其中1条是类似于ANR，无法操作，其余的部分则是卡顿相关的反馈。
流畅性
将官方的例子发给测试同学，让在iOS以及Android平台的不同机子上运行了下。在iOS上基本上流畅运行，没有出现卡顿的现象，在Android部分设备上，出现了卡顿的现象。

![img](https://pic4.zhimg.com/v2-ae1340096002fc72e9a5662722b63b4b_r.jpg)

![img](https://pic3.zhimg.com/v2-25c7a7472747bc76f2495372e4e943c2_r.jpg)

由于没有复杂的例子，其实这个流畅性的测试，意义不是特别大，官方简单的控件展示demo程序，本身就很简单，但是在Android上还是出现了不少问题，只能说明整体还有非常大的优化空间。
**编写复杂程度**
试着照着一张设计稿进行了简单的纯布局代码工作，初次接触用起来还是比较复杂，尤其是那恐怖的嵌套层级，对代码维护来说绝对是个问题，而且由于Flutter的widget机制，很多组件只支持最基本的操作，例如一些扩展的属性，都得自己去实现，况且现在组件库还不是非常的丰富。代码量也比较多，整个代码大概有500行左右，还只是不涉及到一些交互以及数据绑定等。

![img](https://pic3.zhimg.com/v2-ccae2ccb38824ea80b08a64b3bcb5f6e_r.jpg)


从运行效果看，还是比较的不错，两者还原的效果都挺不错的。
**结论**
如果是个人而言，我觉得可以放心大胆的去学习尝试，独立开发app，可以写一套代码，在个平台运行发布。
如果是商业团队，这个就要自行取舍，目前而言，Flutter生态还是非常的不完善，相关的资料也非常少。目前处于beta 3阶段，多久能到release，能否到release，都是个未知数，而且，用Flutter，最大的风险，就是项目整体的不可把控，一旦出现一些坑，如果能填好，那还行，如果涉及到无法解决的问题，就只能放弃。因此看自己团队人力以及时间合理安排比较合适。目前看阿里的咸鱼团队在研究Flutter。
如果单纯从Flutter本身能够解决的问题的方面出发，使用Flutter确实能够产生一定的收益，节省开发成本，如果考虑到目前坑比较多的状况，加上踩坑的时间，可能就无法去评估了。
总体来说，Flutter确实是一个比较不错的东西，如果谷歌能够把它发展的比较完善，对于个人以及小团队来说，确实是个福音。
**参考**

1. [Flutter中文网](https://link.zhihu.com/?target=https%3A//flutterchina.club/)

2. [Google 悄悄开发的全新操作系统 Fuchsia 被发现了！](https://link.zhihu.com/?target=http%3A//osp.io/archives/2540)

3. [为什么Flutter会选择 Dart ？](https://link.zhihu.com/?target=http%3A//www.infoq.com/cn/articles/why-flutter-uses-dart)

4. [Flutter教程(二) 了解Dart语言](https://link.zhihu.com/?target=https%3A//juejin.im/post/5aebc5fb518825670c45c91b)

5. [为什么移动端跨平台开发不靠谱？](https://link.zhihu.com/?target=https%3A//juejin.im/post/59f2346df265da430d573fd8)

6. [为什么说Flutter是革命性的？](https://link.zhihu.com/?target=http%3A//www.infoq.com/cn/articles/why-is-flutter-revolutionary)

   作者：吹个大气球
   链接：[https://juejin.im/post/5afd77466fb9a07aab2a12da](https://link.zhihu.com/?target=https%3A//juejin.im/post/5afd77466fb9a07aab2a12da)
   来源：掘金

---

> ### 前言
>
> 2015年， Google 内部开始测试另一种高性能的编程方式，那就 Google 的 Sky 项目。Sky 项目使用网页开发语言Dart开发原生Android 应用，强调应用的运行速度和与 Web 的高度集成。Sky将其Web后端也带到了移动开发领域。Sky不依赖于平台，其代码可以运行在Android、iOS，或是任何包含Dart虚拟机的平台上。
> 可以说sky是Flutter框架的前身。
> 在 2017年的谷歌 I/O大会上，Google推出了Flutter —— 一款新的用于创建移动应用的开源库。
> 在2018年初世界移动大会上发布了 Flutter的第一个Beta版本，2018年5月的 I/O大会上更新到了Beta3版本，向正式版又迈进了一步。一时间业内对这个框架的关注度越来越高。
> Flutter 是 Google 开源的跨平台移动开发框架。 它允许从单个代码库为 iOS 和 Android 构建高性能，美观的应用程序。它也是 Google 即将推出的 Fuchsia 操作系统的开发平台。此外，它的架构可以通过定制的 Flutter 引擎将其引入其他平台。

# 1、Flutter诞生的原因

跨平台工具包历来采用以下两种方法之一：
将 Web 视图包装在本机应用程序中，并将应用程序构建为网页。
包装原生平台控件并提供对它们的一些跨平台抽象。
Flutter 采取不同的方法，试图使移动开发更好。 它提供了一个开发人员使用的应用程序框架和一个可移植的运行时引擎。该框架建立在 Skia 图形库上，提供实际呈现的部件，而不仅仅是原生控件的包装。
Flutter 做跨平台开发能够像 web 一样灵活，但同时提供流畅的性能。
Flutter 附带的部件库以及开源部件使其成为一个功能丰富的平台。简单地说，Flutter 是最接近移动开发人员用于跨平台开发的理想平台，灵活性、性能几乎毫无妥协。
Flutter 使用谷歌开发的 Dart 语言进行开发。 Dart 是一种面向对象的语言，同时支持提前编译和即时编译，非常适合用于构建本地应用程序，同时 Flutter 的热加载有效的提高了开发效率。 Flutter 最近也转向了 Dart 2.0 版本。
Dart 提供了许多其他语言中的功能，包括垃圾回收，异步，强类型，泛型以及丰富的标准库。

# 2、跨平台框架的发展史

#### 1、使用WebApp的一些框架

1、Ruby on Rails
2、AngularJS
3、JQuery Mobile
4、Cordova
5、PWA
6、Instant App
优势：
一套HTML5APP 即可同时适用iOS、Android平台，适配性和体验较好。
版本服务器更新，用户永远看到的是最新APP端的信息。
技术难度较低，开发工作量小，开发成本低。
劣势：
用户体验大幅落后于原生APP，操作的流畅度，程序的执行效率，与原生APP都有较大差距；
HTML5 APP 受网速的影响较大，在网络情况较差的情况下，HTML5 APP 往往连打开都困难，而原生程序，基本都能顺利打开运行（只是速度较慢）。

#### 2、 适合移动端App的一些框架

1、React Native
特点：Facebook 出品的一个移动端开发框架，可以最大限度的接近原生框架，就能够在Javascript和React的基础上获得完全一致的开发体验，构建原生App，仅需学习一次，编写任何平台。（Learn once， write anywhere）
缺点：初次学习成本高，必须不同平台写两套代码，依赖暴露的接口

2、Weex
特点：weex 能够完美兼顾性能与动态性，让移动开发者通过简捷的前端语法写出Native级别的性能体验，并支持iOS、Android、YunOS及web等多端部署。
缺点：控件太少，基本只能实现最基本的效果；上手难度打，如果是前段和移动端都懂的话上手很快；随着项目变大，编译速度会呈指数型上升。

3、Flutter
1、免费开源；
2、利用保持状态的热重载（hot reload）、全新的响应式框架、丰富的控件以及集成的开发工具，这些特点进行快速开发；
3、通过可组合的控件集合、丰富的动画库以及可扩展的框架来实现富有感染力的灵活界面设计；
4、借助可移植的GPU加速的渲染引擎以及高性能本地ARM代码运行时以达到跨设备的高品质用户体验；
5、提高开发效率：使用一套代码同时开发iOS和Android；
6、可扩展性强：Flutter框架本身提供了丰富的Material Design和Cupertino(iOS-flavor)风格的控件，可自由扩展控件，不受手机平台控件的限制。

# Flutter框架

Flutter框架是一个分层的结构，每个层都建立在前一层之上。



![img](https://upload-images.jianshu.io/upload_images/1801706-1b8814b177622ced.png?imageMogr2/auto-orient/strip|imageView2/2/w/580)

image.png

Flutter跨平台最核心的部分，是它的高性能渲染引擎（Flutter Engine）。Flutter不使用浏览器技术，也不使用Native的原生控件，它使用自己的渲染引擎来绘制widget。
说到widget，就要说一句Flutter的一切皆为widget理念。widget是Flutter应用程序用户界面的基本构建块。每个widget都是用户界面一部分的不可变声明。与其他将视图、控制器、布局和其他属性分离的框架不同，Flutter具有一致的统一对象模型：widget。在更新widget的时候，框架能够更加的高效。
对于Android平台，Flutter引擎的C/C++代码是由NDK编译，在iOS平台，则是由LLVM编译，两个平台的Dart代码都是AOT编译为本地代码，Flutter应用程序使用本机指令集运行。Flutter正是是通过使用相同的渲染器、框架和一组widget，来同时构建iOS和Android应用，而无需维护两套独立的代码库。



![img](https://upload-images.jianshu.io/upload_images/1801706-6b00bc2cc5907b1d.png?imageMogr2/auto-orient/strip|imageView2/2/w/561)

image.png



Flutter将UI组件和渲染器从平台移动到应用程序中，这使得它们可以自定义和可扩展。Flutter唯一要求系统提供的是canvas，以便定制的UI组件可以出现在设备的屏幕上。

# Flutter安装 [-- #官网链接>>> --](https://flutterchina.club/get-started/install/)**

![img](https://upload-images.jianshu.io/upload_images/1801706-efee3b4d1c0b12c4.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

image.png



![img](https://upload-images.jianshu.io/upload_images/1801706-97230795617c48fa.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

image.png



![img](https://upload-images.jianshu.io/upload_images/1801706-26e66461b4f71339.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

image.png



![img](https://upload-images.jianshu.io/upload_images/1801706-9cec33ecfaa8b2ba.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

image.png

# 3、开发工具

Flutter 在开发工具的选择上很灵活。 应用程序可以通过命令行以及任何编辑器轻松开发，这些编辑器来自受支持的 IDE，如 VS Code，Android Studio 或 IntelliJ。 使用哪种 IDE 取决于用户的偏好。
Android Studio 提供了最多的功能，例如 Flutter Inspector 来分析正在运行的应用程序的窗口部件以及监视应用程序性能。 还提供了开发部件层次结构时很方便的几个重构。
VS Code 提供了更轻松的开发体验，因此它的启动速度往往比 Android Studio / IntelliJ 更快。 每个 IDE 都提供内置的编辑助手，如代码补全，接口定义跳转以及良好的调试支持。
Flutter 也很好的支持命令行，这使得创建，更新和启动应用程序变得容易，除了编辑器之外没有任何其他工具依赖性。

# 4、热加载

无论采用何种工具，Flutter 都能为应用程序的热加载提供出色的支持。 这允许在许多情况下修改正在运行的应用程序，维护状态，而不必停止应用程序，重新构建和重新部署。

# 5、测试

Flutter 包含一个 WidgetTester 实用程序，用于与测试中的部件进行交互。 新的应用程序模板包含一个示例测试，用于演示在创建测试时如何使用它。

# 6、包管理和插件 [pub链接](https://pub.dartlang.org/)

Flutter 已经有了一个丰富的开发者生态系统：开发人员已有大量可以使用的包和插件。
要添加包或插件，只需在应用程序的根目录下的 pubspec.yaml 文件中包含依赖项即可。 然后从命令行或 IDE 运行 flutter packages get，Flutter 的工具将引入所有必需的依赖关系。
要将流行的图像选择器插件用于 Flutter，只需在 pubspec.yaml 中将其列为依赖项
然后运行 flutter packages get 拉取所有依耐项，然后可以在 Dart 中导入和使用它



![img](https://upload-images.jianshu.io/upload_images/1801706-db9ad5ec96aee166.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

image.png

# 6、部件

Flutter 里的所有东西都是一个一个的部件。 这包括用户界面元素，例如 ListView，TextBox 和 Image，以及框架的其他部分，包括布局，动画，手势识别和主题等。
通过将所有内容都设置为窗口部件，整个应用程序可以在窗口部件层次结构中表示。 拥有一个所有内容都是部件的架构，可以清楚地了解作用于某一部分的属性和行为的来源。 这与大多数其他应用程序框架不同，它们将属性和行为不一致地关联起来，有时将属性和行为从层次结构中的其他组件附加到控件本身，有时自身控制属性和行为。

# 7、运行第一个Flutter应用

![img](https://upload-images.jianshu.io/upload_images/1801706-6f3a7a400d56724f.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

image.png



![img](https://upload-images.jianshu.io/upload_images/1801706-2e18c7a0672e4dac.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

image.png

### 部件示例

Flutter 应用程序的入口点是 main 函数。 要在屏幕上放置用户界面元素的部件，在 main（）中调用 runApp（）并将部件层次结构根部的部件作为参数传递。



![img](https://upload-images.jianshu.io/upload_images/1801706-cfa8d06052799f6d.png?imageMogr2/auto-orient/strip|imageView2/2/w/468)

image.png

### 无状态 VS 有状态

部件有两种形式：无状态和有状态。 无状态部件在创建和初始化后不会更改它们的内容，而有状态部件维护一些程序运行时可变的状态，例如，响应用户交互。



![img](https://upload-images.jianshu.io/upload_images/1801706-646cd79660167f2e.png?imageMogr2/auto-orient/strip|imageView2/2/w/660)

image.png



在实例中无状态的Widget继承自StatelessWidget，其初始化之后，不会更改内部内容。
有状态的Widget继承自StatefulWidge,有状态的部件返回一个负责为给定状态构建部件树的 State 类。 状态更改时，将重建窗口部件树的关联部分。通过setState（）方法改变部件状态。状态在传递给 setState（）的函数中更新。 当调用 setState（）时，该函数可以设置任何内部状态。

### 布局

默认情况下，runApp 函数会使窗口填充整个屏幕。 为了控制窗口部件布局，Flutter 提供了各种布局窗口部件。 一些布局部件用于子部件的垂直或水平对齐，扩展部件以填充特定空间，将部件限制到特定区域，将它们在屏幕上居中，并允许部件相互重叠。
两个常用的部件是行和列。这些部件执行布局以水平（行）或垂直（列）显示其子部件。
使用这些布局部件只需将它们包装在子部件列表中。mainAxisAlignment 用于控制部件如何沿布局轴线摆放，无论是居中，左对齐，右对齐还是使用各种间距对齐。
以下代码显示如何对齐行或列中的多个子部件：



![img](https://upload-images.jianshu.io/upload_images/1801706-2a8c27e0e16ecd93.png?imageMogr2/auto-orient/strip|imageView2/2/w/336)

image.png

### 响应触摸事件

触摸交互是使用手势处理的，手势被封装在 GestureDetector 类中。由于它也是一个部件，因此添加手势识别只需要将子部件封装在 GestureDetector 部件中。
例如，要向 Icon 添加触摸处理，请将其设置为 GestureDetector 的子项，并设置检测器的回调以捕获所需的手势。



![img](https://upload-images.jianshu.io/upload_images/1801706-ed8e46a21c685883.png?imageMogr2/auto-orient/strip|imageView2/2/w/530)

image.png



在这种情况下，当点击图片，双击或长按时，会打印对应输出。
除了简单的点击手势外，还有丰富的识别器，适用于从平移和缩放到拖动的所有内容。 这使得构建交互式应用程序变得非常简单。

### 绘制

Flutter 还提供了各种部件用于绘制，包括修改不透明度，设置剪切路径和应用装饰。 它甚至通过 CustomPaint 部件以及相关的 CustomPainter 和 Canvas 类支持自定义绘制。
绘制部件的一个示例是 DecorativeBox，它可以将 BoxDecoration 绘制到屏幕上。 以下示例显示如何使用它通过渐变来填充屏幕：



![img](https://upload-images.jianshu.io/upload_images/1801706-d2402f194bd123f6.png?imageMogr2/auto-orient/strip|imageView2/2/w/575)

image.png

### 动画

Flutter 包含一个 AnimationController 类，用于控制动画播放，包括开始和停止动画，以及改变动画的值。 此外，还有一个 AnimatedBuilder 部件，允许与 AnimationController 一起构建动画。
任何部件（例如前面显示的装饰星形）都可以对其属性进行动画处理。 例如，将代码重构为 StatefulWidget，因为动画是状态更改，并且将 AnimationController 传递给 State 类允许在构建部件时使用动画值。



![img](https://upload-images.jianshu.io/upload_images/1801706-45f0ffee0adb0aee.png?imageMogr2/auto-orient/strip|imageView2/2/w/721)

image.png



在这种情况下，该值用于改变窗口部件的大小。 只要动画值发生变化就会调用 build 函数，从而导致星形的大小在 750 毫秒内变化，从而创建一个缩放效果。

# Flutter与原生交互

为了在 Android 和 iOS 上提供对本机平台 API 的访问，Flutter 应用程序可以使用平台通道。 这允许 Dart 代码将消息发送到 iOS 或 Android 宿主应用。 许多可用的开源插件都是使用平台通道上的消息传递构建的。
Flutter界面调用包名
定义渠道名设置对应方法名，并监听方法名，获取对应方法返回结果



![img](https://upload-images.jianshu.io/upload_images/1801706-eabe0245cb42bab2.png?imageMogr2/auto-orient/strip|imageView2/2/w/311)

image.png



![img](https://upload-images.jianshu.io/upload_images/1801706-15a6d0221f38a5f2.png?imageMogr2/auto-orient/strip|imageView2/2/w/688)

image.png

iOS AppDelegate设置监听对象，并实现对应方法



![img](https://upload-images.jianshu.io/upload_images/1801706-0e54716161ee1308.png?imageMogr2/auto-orient/strip|imageView2/2/w/651)

image.png

Android 的MainActivity设置监听对象，并实现对应方法



![img](https://upload-images.jianshu.io/upload_images/1801706-e12702d1f11f5f4a.png?imageMogr2/auto-orient/strip|imageView2/2/w/444)

image.png

# Flutter调研

### 生态

在Flutter官方的[Pub](https://link.juejin.im/?target=https%3A%2F%2Fpub.dartlang.org%2F)平台上，纯Flutter Package大概有两千多个，基本上常见的库还是都有的，例如网络、图片、音视频播放等等。但是对于目前Android以及iOS的生态，还是远远的不足的，对于一些复杂的UI或者一些不是特别通用的功能，就得自己去实现。

### 包大小

根据官网的介绍，一个最小的Android版本的Flutter应用。release版本大小约6.7MB，其中核心引擎大约3.3MB，框架+应用程序代码大约是1.25MB，LICENSE文件（包含在app.flx中）是55k，必需的Java代码.dex为40k，并且约有2.1MB的ICU数据。考虑到目前网络环境，包大小的增加，还也在可以接受的范围。

### Crash

iOS运行官方的例子，会有时候crash掉，因此我们将一个[开源的Flutter应用](https://link.juejin.im/?target=https%3A%2F%2Fgithub.com%2Froughike%2FinKino)，添加了Bugly上报，在Android平台进行了众测。

![img](https://upload-images.jianshu.io/upload_images/1801706-ea794dd39c0158e1.png?imageMogr2/auto-orient/strip|imageView2/2/w/506)

image.png



参与人次大概150人左右，启动次数大概500次左右，没有出现一次Crash数据上报，由于app很简单，并不能说明很多问题，但是众测用户反馈了约12条信息，其中1条是类似于ANR，无法操作，其余的部分则是卡顿相关的反馈。

### 流畅性

将官方的例子发给测试同学，让在iOS以及Android平台的不同机子上运行了下。在iOS上基本上流畅运行，没有出现卡顿的现象，在Android部分设备上，出现了卡顿的现象。



![img](https://upload-images.jianshu.io/upload_images/1801706-6ee6edc364f53a72.png?imageMogr2/auto-orient/strip|imageView2/2/w/470)

image.png

# 结论

即使在测试版中，Flutter 也为构建跨平台应用程序提供了一个很好的解决方案。凭借其出色的工具和热加载，它带来了非常愉快的开发体验。
个人而言，我觉得可以放心大胆的去学习尝试，独立开发app，可以写一套代码，在多个平台运行发布。
如果单纯从Flutter本身能够解决的问题的方面出发，使用Flutter确实能够产生一定的收益，节省开发成本，如果考虑到目前坑比较多的状况，加上踩坑的时间，可能就无法去评估了。
丰富的开源软件包和出色的文档使得开始使用起来非常容易。展望未来，除了 iOS 和 Android 之外，Flutter 开发人员还可以针对 Fuchsia。 考虑到引擎架构的可扩展性，Flutter 出现在其他平台上并不令人意外。 随着社区日益壮大，现在是一个很好的时机入手。
总体来说，Flutter确实是一个比较不错的东西，如果谷歌能够把它发展的比较完善，对于个人以及小团队来说，确实是个福音。



作者：Kean_Qi
链接：https://www.jianshu.com/p/5c0d1255cbf3
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

----

