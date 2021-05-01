[TOC]

# 前言

- Flutter 作为Google出品的一个**新兴的跨平台移动客户端UI开发框架**，正在被越来越多的开发者和组织使用，包括阿里的咸鱼、腾讯的微信等。

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwga1phxmj30jc09wgmi.jpg)

  示意图

  

- 今天，我将献上一份**《全面 & 详细的Flutter学习指南》**，希望你们会喜欢。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9ywb96j30em0ca750.jpg)

示意图

------

# 目录

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9vi54zj30lm0qsmy8.jpg)

示意图

------

# 1. 简介

- 定义：一款Google出品&开源的移动客户端UI开发框架（SDK）
- 作用：用**一套代码**同时在Android、iOS上**快速构建高质量、高性能**的原生用户界面
- 开发语言：Dart语言（高开发效率、高性能等）

------

# 2. 特点

- Flutter的主要特点包括：使用自身的高性能渲染引擎进行渲染 & Dart编程语言语言
- 具体介绍如下：

### 2.1 高性能渲染引擎

- Flutter进行UI绘制时，使用的是自带的**高性能渲染引擎**进行绘制渲染（不使用WebView & 原生控件）
- 好处：保证在Android和iOS上UI的一致性 & 避免对原生控件依赖而带来的限制和维护成本。
- 组成：C、C ++、Dart、Skia（2D渲染引擎），具体描述如下：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9tnzqjj30xc0kkwik.jpg)

示意图

特别注意：

- Flutter依靠Flutter Engine虚拟机在iOS和Android上运行
- Flutter Engine使用C/C++编写 = 低延迟输入 + 高帧速率
- 开发人员可通过Flutter框架和API在内部进行交互

下面，简单介绍一下Flutter的2D渲染引擎：Skia



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9rsvpij30xc0g1djg.jpg)

示意图

### 2.2 Dart语言

- 介绍Dart语言前先介绍两个概念：JIT和AOT。程序主要有两种运行方式：静态编译 & 动态编译，具体如下：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9qw89lj30xc0cymz7.jpg)

  示意图

特别注意：

- JIT 和 AOT指的是程序运行方式，和编程语言并非强关联的。
- 有些语言可以以JIT方式 & AOT方式一起运行，如Java，它可在第一次执行时编译成中间字节码、然后在之后执行时可以直接执行字节码
- 通常区分是否为AOT的标准就是看代码在执行之前是否需要编译，只要需要编译，无论其编译产物是字节码还是机器码，都属于AOT

#### Dart语言的特点

- Dart语言具备开发效率高、高性能 & 类型安全的特点

- 具体如下：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9oy0pxj30t60iygq1.jpg)

  示意图

------

# 3. 原理解析

### 3.1 框架结构

- Flutter框架主要分为两层：FrameWork层、Engine层，如下图所示：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9n27frj30xc0hfq5j.jpg)

  示意图

- 说明：开发时，主要基于Framework层；运行时，则是运行在 Engine上。每层的具体介绍如下：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9lrt5rj30xc0j8gsw.jpg)

  示意图

### 3.2 原理概述

- 开发时，主要基于Framework层；运行时，则是运行在 Engine上

- Engine是Flutter的独立虚拟机，由它适配 & 提供跨平台支持；因为其存在，Flutter不使用移动端系统的原生控件， 而是使用自己 Engine 来绘制 Widget （Flutter的显示单元）； Dart 代码是通过 AOT 编译为平台的原生代码，所以 Flutter可直接与平台通信，不需要JS引擎的桥接。

- 同时 Flutter 唯一要求系统提供的是 canvas，以实现UI的绘制。

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9kqxd4j30xc0gen27.jpg)

  示意图

- 编译时，具体如下：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9jbr8rj30xc0kkwik.jpg)

  示意图

### 3.3 关于widget

Flutter理念：“一切皆为Widget”，Widget是Flutter应用程序用户界面的基本构建块，具备以下特点：

- 属于具有一致性的统一对象模型，与其他将视图、控制器、布局和其他属性分离的框架不同。更新widget时更加高效
- 不可变的，仅支持一帧，且每一帧上不会直接更新，要更新而必须使用Widget的状态。无状态和有状态Widget 的核心特性相同，每一帧都会重新构建；
- 有一个State对象，用于跨帧存储状态数据 & 恢复

------

# 4. 特点

- Flutter具备跨平台、开发效率高 & 高性能的特点
- 具体说明如下：

### 4.1 跨平台 & 开发效率高

Flutter通过使用上述所述的自身渲染引擎、原理框架 & Widget运行，使用一套代码即可同时构建iOS和Android应用，从而实现跨平台的特性，最终提高开发的效率

### 4.2 高性能

- 原因1：通过其自带的高性能渲染引擎进行渲染

- 原因2：Dart语言本身的语言特性

- 原因3：编译过程特点，具体如下：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9i114dj30xc0frgob.jpg)

  示意图

------

# 5. 对比

- 跨平台开发的本质是为了：增加代码复用、减少不同平台差异适配的工作量 & 提高开发效率。
- 目前主流的跨平台开发框架有：React-Native、Weex和本文提及的Flutter。下面，我先简单介绍React-Native和Weex，再进行三者的对比。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9gl1xqj30xc0jwaf3.jpg)

示意图

### 5.1 React-Native

- 简介
  由Facebook出品，采用了JavaScript语言、JSCore引擎和通过原生渲染的跨平台框架

- 实现原理
  通过编写JavaScript语言代码，通过 React Native 的中间层来调用 Native端的组件，最终实现相应的功能。JS端中所写控件的作用类似 Map中的key 值，对应着Native端的对应控件（如 Android 中\<view> 标签对应 ViewGroup 控件）。JS端会通过多个key 组合成Dom，最后交由Native端进行解析，最终渲染出Native端的控件。

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9ct47xj30js0kc74k.jpg)

  示意图

- 实现框架
  React Native的架构主要由三层实现。其中最重要的是由C++ 实现的中间适配层，此处最主要封装了JavaScriptCore用于执行JS的解析，最终实现了JS端与原生端的双向通信交互。而React Native运行在JavaScriptCore中。（在iOS上直接使用内置的javascriptcore、在Android则使用webkit.org官方开源的jsc.so）

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg9b0my2j30xc0s343o.jpg)

示意图

### 5.2 Weex

- 简介
  由Alibaba出品，采用了JavaScript语言、JS V8引擎和通过原生渲染的跨平台框架

- 实现原理
  与React-Native类似，JS端会通过多个key组合成Dom，最后交由Native端进行解析，最终渲染出Native端的控件，但区别在于：Weex是可以跨三端的 = Android、iOS、Web，其原因在于在开发过程中，代码模式、编译过程、模板组件、数据绑定、生命周期等上层语法是一致，不同的是Web端和Native端对Virtual DOM 执行的解析方法有所区别。

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg99hyi4j30l409i3yl.jpg)

  示意图

- 实现框架
  weex的架构主要分为三部分，具体如下：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg98aimjj30vw0ee40d.jpg)

  示意图

### 5.3 三者对比

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwg96y9fzj30xc0acwhl.jpg)

示意图

注：对于性能的对比，从理论上来说Flutter应该是最接近原生性能 & 最好的，但就目前实际应用&体验中并没具备很明显的差异化，后续需进行进一步的验证。

------

# 6. 学习方式 & 资料

- 官网：[https://flutter.dev/](https://links.jianshu.com/go?to=https%3A%2F%2Fflutter.dev%2F)
  快速入门 & 学习最好的方式是：阅读Flutter官网的资源，同时官网也是了解最新Flutter发展动态的地方
- Flutter中文网社区：[https://flutterchina.club](https://links.jianshu.com/go?to=https%3A%2F%2Fflutterchina.club)
  目前Flutter最大的中文资源社区，提供了：Flutter官网文档翻译、开源项目 & 案例等学习资源
- StackOverflow：[https://stackoverflow.com/](https://links.jianshu.com/go?to=https%3A%2F%2Fstackoverflow.com%2F)
  活跃度最高的Flutter问答社区，Flutter开发团队的成员也经常会在上面回答问题
- 源码：[https://flutter.dev/docs/development/tools/sdk/releases](https://links.jianshu.com/go?to=https%3A%2F%2Fflutter.dev%2Fdocs%2Fdevelopment%2Ftools%2Fsdk%2Freleases)
  Flutter SDK的源码具备以下特点：开源、示例 & 详细注释，Gallery是Flutter官方示例APP，其源码在Flutter源码“examples”目录下

------

# 7. 总结

本文全面介绍了Flutter入门学习知识，接下来推出的文章，我将继续讲解Flutter的相关知识，包括使用语法、实战等，感兴趣的读者可以继续关注我的博客哦：[Carson_Ho的Android博客](https://www.jianshu.com/users/383970bef0a0/latest_articles)

------

# 请点赞！因为你们的赞同/鼓励是我写作的最大动力！

> **相关文章阅读**
> [Android开发：最全面、最易懂的Android屏幕适配解决方案](https://www.jianshu.com/p/ec5a1a30694b)
> [Android开发：史上最全的Android消息推送解决方案](https://www.jianshu.com/p/b61a49e0279f)
> [Android开发：最全面、最易懂的Webview详解](https://www.jianshu.com/p/3c94ae673e2a)
> [Android开发：JSON简介及最全面解析方法!](https://www.jianshu.com/p/b87fee2f7a23)
> [Android四大组件：Service服务史上最全面解析](https://www.jianshu.com/p/d963c55c3ab9)
> [Android四大组件：BroadcastReceiver史上最全面解析](https://www.jianshu.com/p/ca3d87a4cdf3)

------

### 



作者：Carson_Ho
链接：https://www.jianshu.com/p/0a19cf96dbe3
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。