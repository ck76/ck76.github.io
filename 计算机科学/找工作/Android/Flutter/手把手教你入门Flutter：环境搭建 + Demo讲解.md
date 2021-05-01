[TOC]

https://www.jianshu.com/p/c434d1cd4799

# 目录

本文主要分文两大部分：环境搭建 & 示例讲解

------

# Flutter环境搭建

此处搭建主要是基于Mac OS。

### 1. 安装brew

- 定义：包管理工具，具体介绍可参考：[官网地址](https://links.jianshu.com/go?to=%5Bhttps%3A%2F%2Fbrew.sh%2F%5D(https%3A%2F%2Fbrew.sh%2F))
- 作用：方便进行安装 / 卸载 / 更新各种软件包，如：本文提及的flutter、nodejs等等
- 应用场景：快速搭建本地开发环境
- 安装：



```bash
命令行下运行：
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

### 2. 下载Flutter SDK

- 下载地址：[官网链接](https://links.jianshu.com/go?to=https%3A%2F%2Fflutter.dev%2Fdocs%2Fget-started%2Finstall%2Fmacos)

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgblu42sj30xc0mkgqd.jpg)

示意图

- 将下载的SDK解压到本地目录

### 3. 环境变量配置

命令行输入以下文件



```cpp
// 1. 打开配置文件
open ~/.bash_profile
// 若不存在此文件，则直接新创建再打开即可，终端输入：
cd ~
touch .bash_profile
open ~/.bash_profile

// 2. 在打开的配置文件下添加以下语句 & 保存
export PATH=/Users/carson.ho/AndroidStudioProjects/flutter/bin:$PATH // 注：.../bin 前需填写的是你刚才解压flutter SDK的目录
export PUB_HOSTED_URL=https://pub.flutter-io.cn
export FLUTTER_STORAGE_BASE_URL=https://storage.flutter-io.cn

// 3. 生效配置
source ~/.bash_profile
```

### 4. 安装Flutter插件（Flutter Plugin）

打开Android Studio，按如图下安装Flutter Plugin：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgbjgc4ij30xc0jhame.jpg)

示意图

### 5. 环境检查

终端命令行输入



```undefined
flutter doctor
```

若是用于Android场景，那么安装成功提示如下：（若是iOS场景，则需安装标红的地方）



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgbi1gxpj30xc0hin5c.jpg)

示意图

错误处理：若存在报错，则按项进行检查即可：安装对应版本Android SDK & Android Studio。

> 可到此处进行快速下载：[http://www.androiddevtools.cn/](https://links.jianshu.com/go?to=http%3A%2F%2Fwww.androiddevtools.cn%2F)

至此，关于Flutter的环境搭建讲解完毕。

------

# 入门示例讲解

此处将手把手带你创建一个Flutter工程并进行Demo展示，让你快速入门Flutter。

### 步骤1：Android Studio创建Flutter工程

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgbgd4i3j30m20my0v7.jpg)

示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgber0lpj30xc0oeadb.jpg)

示意图

简单介绍每项：

- Flutter Application：标准的Flutter App工程，包含标准的Dart层与Native平台层；
- Flutter Module：Flutter组件工程，仅包含Dart层实现，Native平台层子工程为通过Flutter自动生成的隐藏工程；
- Flutter Plugin：Flutter平台插件工程，包含Dart层与Native平台层的实现；
- Flutter Package：Flutter纯Dart插件工程，仅包含Dart层的实现，往往定义一些公共Widget。

此处选择Flutter Application进行展示。

### 步骤2：工程结构解析

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgbcvzexj30jm0qadkl.jpg)

示意图

在Flutter上使用的代码，主要是写在 `main.dart`文件上，即工程一旦运行，是先执行`main.dart`文件。

### 步骤3：实例代码解析

- 本文直接采用Flutter官方Demo说明，从而对Flutter的开发进行快速入门。
- 点击main.dart即可看到Demo代码，具体解析如下：



```dart
/**
 *  1. 导入Material UI组件库
 **/
import 'package:flutter/material.dart';
// 定义：Material是Flutter 实现 Material Design设计风格(谷歌推出的一套视觉设计语言)的基础包，
// 作用：包含文本输入框(Text)、图标( Icon)、图片(Image)、行排列布局( Row)、 列排列布局(Column)、 Decoration(装饰器)、动画等组件
// 因此基本上每个flutter程序的代码，第一行代码都会引入这个包


/**
 *  2. Flutter应用的入口：runApp（），即首先执行该函数
 *  作用：将给定的组件(widget)显示在屏幕上
 *  注：若不使用runApp（），程序仍会正常运行，但屏幕上什么都不会显示，相当于一个Dart控制台程序
 **/
void main() => runApp(MyApp());

// 等价于
// void main() {
// return runApp(MyApp());
// }
// 下面，将执行MyApp -> 跳转到3

/**
 *  3. 自己定义的组件类MyApp，继承自StatelessWidget
 *  作用：整个应用的底层Widget
 *  注：StatelessWidget是无状态组件，具体介绍请跳出看附录1
 **/
class MyApp extends StatelessWidget {

  // build()：Widget中的生命周期方法
  // 作用：描述如何构建UI界面
  // 关于其他生命周期方法，请看附录2
  @override
  Widget build(BuildContext context) {
    // Material App是一个使用Material Design设计风格的应用，具体介绍请看附录3
    // 此处设置了标题、主题 & 要显示的界面 -> MyHomePage -> 跳转4
    return MaterialApp(
      title: 'Flutter Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: MyHomePage(title: 'Flutter Demo Home Page'),
    );
  }
}

/**
 *  4. 自己定义的组件类，继承自StatefulWidget
 *  作用：设置应用打开的显示界面
 *  注：StatefulWidget是有状态组件，具体介绍请跳出看附录4
 **/
class MyHomePage extends StatefulWidget {
  MyHomePage({Key key, this.title}) : super(key: key);

  final String title;

  @override
  _MyHomePageState createState() => _MyHomePageState();
  // 调用父类StatefulWidget的createState()，用于创建和StatefulWidget相关的状态 -> 跳转5
}

/**
 *  5. 继承自State类
 *  作用：实现一系列Widget生命周期方法 & 更新Widget的状态，
 *  注：StatefulWidget是有状态组件，具体介绍请跳出看附录4
 **/

class _MyHomePageState extends State<MyHomePage> {

  // 用于后续点击按钮统计
  int _counter = 0;

  void _incrementCounter() {
    setState(() {
      _counter++;
    });
  }

  // build()：描述如何构建UI界面
  @override
  Widget build(BuildContext context) {

  // 此处直接采用Scaffold
  // Scaffold实现了基本的 Material Dsign布局
  // 只要是在 Material Design中定义过的单个界面显示的布局组件元素，都可以使用Scaffold绘制 
    return Scaffold(
      appBar: AppBar( // 显示在界面顶部的AppBar
      title: Text(widget.title),
      ),
      body: Center( // 当前界面显示的主要内容
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            Text(
              'You have pushed the button this many times:',
            ),
            Text(
              '$_counter', // 显示的内容是_counter变量
              style: Theme.of(context).textTheme.display1,
            ),
          ],
        ),
      ),
      floatingActionButton: FloatingActionButton( // 即FAB，最常用的Widget组件 = 圆形按钮
        onPressed: _incrementCounter, // 设置点击事件->调用_incrementCounter()
        tooltip: 'Increment', // 长按按钮提示
        child: Icon(Icons.add), // 按钮的子视图：“+”样式
        // 关于FAB还有更多的样式 & 属性设置，此处不做过多介绍
      ), 
    );
  }
}
```

##### 附录1：StatelessWidget

- 定义：无状态组件，继承自widget类。
- 特点：无状态、不可变，属性不能改变
- 示例：例如Icon、IconButton、和Text

##### 附录2：Widget生命周期方法介绍

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgba3jstj30go0p8js1.jpg)

示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgb87ztuj30x80mg7aw.jpg)

示意图

##### 附录3：Material App

- 定义：使用Material Design设计风格的应用
- 作用：包含了Material Design设计风格的应用所需要的基本控件
- 常用属性：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgb6sozbj30q40gidhx.jpg)

示意图

此处详细列出主题（Theme）的设置

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgb5ech7j30m80dmtdj.jpg)

示意图

##### 附录4：StatefulWidget

- 定义：有状态组件，继承自widget类。
- 特点：存在状态 & 随着Widget生命周期发生变化
- 示例：如Checkbox、Radio、Slider、InkWell、Form、and、TextField
- 自定义：需继承 & 实现两个类

1. StatefulWidget类：实现StatefulWidget的createState() = 创建和Statefulwidget相关的状态
2. State类：实现一系列Widget的生命周期方法，用于更新Widget的状态，具体方法同附录2：Widget生命周期方法介绍

> 请回到原代码处

### 特别说明：使用第三方功能库

在实际开发过程中，会使用到第三方功能库，在Flutter中，使用第三方功能库的步骤如下：

- 步骤1：使用pubspec.yaml文件进行配置

> 在iOS中，使用CocoaPods；在Android中使用Gradle

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgb45lomj30to0fp13d.jpg)

示意图

- 步骤2：获取所需要的库到工程中，通过终端命令行执行：



```csharp
// 步骤1：进入到工程目录
cd 工程目录

// 步骤2：下载Flutter包
flutter packages get
```

至此，关于Flutter的开发入门讲解完毕。

------

# 总结

- 本文全面介绍了Flutter入门开发学习知识，包括：环境搭建、关键语法和示例讲解
- 接下来推出的文章，我将继续讲解Flutter的相关知识，包括使用语法、实战等，感兴趣的读者可以继续关注我的博客哦：[Carson_Ho的Android博客](https://www.jianshu.com/users/383970bef0a0/latest_articles)

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

### 欢迎关注[Carson_Ho](https://www.jianshu.com/users/383970bef0a0/latest_articles)的简书！

不定期分享关于**安卓开发**的干货，追求**短、平、快**，但**却不缺深度**。



作者：Carson_Ho
链接：https://www.jianshu.com/p/c434d1cd4799
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。