[TOC]

今天，我主要讲解Flutter中布局方面的顶部导航栏`TabBar + TabBarView + TabController`，希望你们会喜欢。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgjss63eg307m0b046i.gif)

示意图

------

# 目录

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgjqxvt7j30v80pqq34.jpg)

------

# 1. 简介

Flutter中用于快速实现顶部导航栏的组件库（Widget）



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgjpzh0ag307m0b046i.gif)

示意图

------

# 2. 作用

- TabBar：导航栏标题内容
- TabBarView：描述导航栏当前所对应的内容区（容器）
- TabController：关联TabBar和TabBarView，即 哪个Tab对应哪个内容区
  示意图

------

# 3. 具体介绍

参数说明如下：



```kotlin
/**
  *  TabBar
  **/
const TabBar({
    Key key,
    @required this.tabs,// 子标签，一般使用Tab对象,当然也可以是其他的Widget
    this.controller,// 控制器，即TabController对象
    this.isScrollable = false,// 能否滑动。false：每个tab的宽度则等比，true：tab宽度则包裹item
    this.indicatorColor,// 指示器颜色
    this.indicatorWeight = 2.0, // 指示器厚度
    this.indicatorPadding = EdgeInsets.zero, // 底部指示器的Padding
    this.indicator, // 指示器decoration，如边框等
    this.indicatorSize,// 指示器大小计算方式 - TabBarIndicatorSize.label：indicator与文字同宽，TabBarIndicatorSize.tab：与tab同宽
    this.labelColor,// 选中标签颜色
    this.labelStyle,// 选中标签样式
    this.labelPadding, // 选中标签的padding
    this.unselectedLabelColor,// 未选中标签颜色
    this.unselectedLabelStyle, // 未选中Tab中文字style
    this.dragStartBehavior = DragStartBehavior.down,
    this.onTap,// 点击事件
  })

/**
  *  TabBarView
  **/
 const TabBarView({
    Key key,
    @required this.children,//子widget
    this.controller,//控制器
    this.physics,
    this.dragStartBehavior = DragStartBehavior.down,
  })

 /**
  *  TabController
  **/
TabController({ int initialIndex = 0, @required this.length, @required TickerProvider vsync })
    _index = initialIndex, // 当前选中Tab的下标。注：改变index也会更新 previousIndex、设置animation's的value值、重置indexIsChanging为false并且通知监听器
    _previousIndex = initialIndex, // 上一个选中tab的index值，最初与index相同
    _animationController = AnimationController.unbounded( 
      value: initialIndex.toDouble(),
      vsync: vsync,
    ); // 该动画值表示当前TabBar选中的指示器位置以及TabBar和TabBarView的scrollOffsets

// 构造函数的参数：
// 参数1：初始显示的tab位置
// 参数2：tab的个数
// 参数3：动画效果的异步处理，默认格式

// 特别注意：参数indexIsChanging（bool）
// true：当动画从上一个跳到下一个时
```

------

# 4. 使用说明



```dart
/**
*  TabController
*  作用：关联TabBar和TabBarView，即 哪个Tab对应哪个内容区
**/
TabController mController = TabController(initialIndex: 0,length: 3, vsync: this);
// 初始化TabController
// 参数1：初始显示的tab位置
// 参数2：tab的个数
// 参数3：动画效果的异步处理，默认格式

// 添加监听器
mController.addListener(() => _onTabChanged());

// 监听函数
_onTabChanged() {
print(mController.index);
}

/**
*  TabBar
*  导航栏标题内容
**/
// 1. 设置要显示的list内容（要与TabController的长度对应）
final List<Tab> titleTabs = <Tab>[
    Tab(
      text: 'Android',
    ),
    Tab(
      text: 'iOS',
    ),
    Tab(
      text: 'Web',
    ),
   ...
  ];

// 2. 创建TabBar组件 & 属性
new TabBar(
  controller: mController,// 设置控制器
  labelColor: Colors.green, //选中的颜色
  labelStyle: TextStyle(fontSize: 16), //选中的样式
  unselectedLabelColor: Colors.black, //未选中的颜色
  unselectedLabelStyle: TextStyle(fontSize: 14), //未选中的样式
  indicatorColor: Colors.green, //下划线颜色
  isScrollable: false, //是否可滑动，设置不可滑动，则是tab的宽度等长
  //tab标签
  tabs: titleTabs, // 设置标题（上面设置的内容）

  // 同样是点击事件（i是当前点击Tap的下标数）
  onTap: (int i) {
    print(i);
  },
),

/**
*  TabBarView
*  导航栏每个标题对饮的内容
**/
new TabBarView(
      controller: mController, // 设置控制器
      children:  <Widget>[ // 每个空间对应的页面，此处根据需求进行返回（（要与TabController的长度对应））
    Center(child:Text('Android')),
    Center(child:Text('iOS')),
    Center(child:Text('Web')),
    ...
    ],
    ),
  );
```

------

# 5. 实例说明

本次采用的实例 = 3个Tab

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgjmdjh6g307m0b046i.gif)

示意图

具体代码如下：

*main.dart*



```java
// Flutter入口
void main() => runApp(MyApp());

class MyApp extends StatelessWidget {
  
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: TabViewDemo(),// 自己设置的TabView类
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
    );
  }
}
```

*TabViewDemo.dart*



```dart
import 'package:flutter/material.dart'; // Material UI组件库
import 'dart:ui';

class TabViewDemo extends StatefulWidget {
  @override
  State<StatefulWidget> createState() {
    return new _TabViewState();
  }
}

class _TabViewState extends State<TabViewDemo> with SingleTickerProviderStateMixin {

  TabController mController;// tab控制器

  final List<Tab> titleTabs = <Tab>[
    Tab(
      text: 'Android',
    ),
    Tab(
      text: 'iOS',
    ),
    Tab(
      text: 'Web',
    ),

  ];

  @override
  void initState() {
    super.initState();

    mController = TabController(initialIndex: 0,length: 3, vsync: this);
    // 初始化TabController
    // 参数1：初试显示的tab位置
    // 参数2：tab的个数
    // 参数3：动画效果的异步处理，默认格式

    // 添加监听器
    mController.addListener(() => _onTabChanged());

  }

  @override
  Widget build(BuildContext context) {
    return  new Scaffold(
        // 创建TabBar & 设置属性
        appBar: new TabBar(
          controller: mController,// 设置控制器
          labelColor: Colors.green, //选中的颜色
          labelStyle: TextStyle(fontSize: 16), //选中的样式
          unselectedLabelColor: Colors.black, //未选中的颜色
          unselectedLabelStyle: TextStyle(fontSize: 14), //未选中的样式
          indicatorColor: Colors.green, //下划线颜色
          isScrollable: false, //是否可滑动，设置不可滑动，则是tab的宽度等长
          //tab标签
            tabs: titleTabs, // 设置标题

          //点击事件
          onTap: (int i) {
            print(i+10);
          },
        ),
        body: new TabBarView(
          controller: mController,
          children:  <Widget>[ // 每个空间对应的页面
        Center(child:Text('Android')),
        Center(child:Text('iOS')),
        Center(child:Text('Web')),
        ],
        ),
      );
  }

  @override
  void dispose() {
    super.dispose();
    mController.dispose(); // 当整个页面dispose时，记得把控制器也dispose掉，释放内存
  }

  // 点击监听函数
  _onTabChanged() {
    print(mController.index);
  }
}
```

# 6. 总结

- 本文全面介绍了`Flutter`的布局组件中顶部导航栏的实现：`TabBar + TabBarView + TabController`
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
链接：https://www.jianshu.com/p/aa1e26bff092
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。