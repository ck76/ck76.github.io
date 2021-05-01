

[TOC]

### 简介

Jetpack是一个Android软件组件的集合，可以让你更容易地开发优秀的Android应用程序。这些组件帮助您遵循最佳实践，使您不必编写样板代码，并简化复杂的任务，这样您就可以专注于您关心的代码。

Jetpack包括androidx。*包库，从平台api中分离出来。这意味着它提供了向后兼容性，并且比Android平台更新得更频繁，确保您始终能够访问Jetpack组件的最新和最好版本。



### 优点

- 加快发展

组件可单独采用，也可以一起使用，同时利用Kotlin语言功能，提高您的工作效率。

- 消除样板代码

Android Jetpack管理繁琐的活动，如后台任务，导航和生命周期管理，因此您可以专注于使您的应用变得更好的原因。

- 构建高质量，强大的应用程序

Android Jetpack组件以现代设计实践为基础，可以减少崩溃，减少内存泄漏，并提供向后兼容性。



### 组件库

Android Jetpack组件是一组库的集合，这些库可以单独使用，并且构建起来可以协同工作，同时利用Kotlin语言特性，使您的工作效率更高。全部使用或混合搭配。

- Foundation(基础)

基础组件提供核心系统功能、Kotlin扩展和对multidex和自动化测试的支持。

- Architecture(建筑)

体系结构组件具有帮助管理UI组件生命周期、处理数据持久性等的类。

- Behavior(行为)

行为组件帮助您设计健壮、可测试和可维护的应用程序。

- UI(UI)

UI组件使您的应用程序不仅简单，而且易于使用。



### 使用

#### 添加谷歌Maven仓库

Android Studio项目默认情况下并没有配置该仓库。

为了添加其至你的项目中，打开项目的`build.gradle`（而不是模块的gradle文件），添加以下内容：

```groovy
allprojects {
    repositories {
        jcenter()
        maven { url 'https://maven.google.com' }//所要添加的语句
    }
}
```

#### 添加架构组件

打开模块下的`build.gradle`文件，添加你所需要构件的依赖：

对于`Lifecycles`，`LiveData`，`ViewModel`：

- `compile "android.arch.lifecycle:runtime:1.0.0-alpha1"`
- `compile "android.arch.lifecycle:extensions:1.0.0-alpha1"`
- `annotationProcessor "android.arch.lifecycle:compiler:1.0.0-alpha1"`

对于Room：

- `compile "android.arch.persistence.room:runtime:1.0.0-alpha1"`
- `annotationProcessor "android.arch.persistence.room:compiler:1.0.0-alpha1"`

对于Room迁移测试：

- `testCompile "android.arch.persistence.room:testing:1.0.0-alpha1"`

对于Room RxJava支持：

- `compile "android.arch.persistence.room:rxjava2:1.0.0-alpha1"`



### 链接

- [sunflower](https://github.com/googlesamples/android-sunflower)

- [官方文档翻译](https://www.jianshu.com/p/13a855ceaf2b)

- https://blog.csdn.net/biezhihua/article/details/81147871
- https://developer.android.google.cn/topic/libraries/architecture/viewmodel  //谷歌中国

- [系列](https://juejin.im/post/593df980ac502e006c049607)

- [B站视频](https://space.bilibili.com/64169458/channel/detail?cid=58384)

- [玉刚说一个](https://space.bilibili.com/64169458/channel/detail?cid=58384)

---

## 3.文章推荐

部分框架的使用我这边就不介绍了，我提供一些优质的文章，希望读者好好学习。

参考项目

[https://github.com/KunMinX/Jetpack-MVVM-Best-Practice](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FKunMinX%2FJetpack-MVVM-Best-Practice)

是 难得一见 的 Jetpack MVVM 最佳实践！

https://www.jianshu.com/p/06be9f651ae6

学习Android Jetpack? 实战和教程这里全都有！

https://www.jianshu.com/p/f32c8939338d

Android Jetpack系列——Android Jetpack介绍

https://www.jianshu.com/p/8e260577bb4c

LiveData 概述

https://www.jianshu.com/p/fc7f9efb2e52

简单使用LiveData+ViewModel

https://www.jianshu.com/p/fade79561568

Android官方架构组件Paging：分页库的设计美学

https://www.jianshu.com/p/10bf4bf59122

Android—Room数据库（介绍）

https://www.jianshu.com/p/cfde3535233d

初步了解Android Navigation（附源码）

https://www.jianshu.com/p/62874cb57401

PagerBottomTabStrip

[https://github.com/tyzlmjj/PagerBottomTabStrip](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2Ftyzlmjj%2FPagerBottomTabStrip)



作者：Jade_4c8c
链接：https://www.jianshu.com/p/b5634241544d
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。