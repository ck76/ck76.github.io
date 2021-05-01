[TOC]

今天，我主要讲解Flutter中布局方面的底部导航栏：`BottomNavigationBar`，希望你们会喜欢。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgmd1bs4g307u0ck1c8.gif)

------

# 目录

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgmbcoabj30oc0j4wel.jpg)

示意图

------

# 1. 简介

底部导航栏控件，属于 Scaffold 组件。

> 配合使用 BottomNavigationBarItem ：底部导航栏要显示的Item = 图标 + 标题

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgm9sqn8g307u0ck1c8.gif)

------

# 2. 属性介绍



```kotlin
BottomNavigationBar({
    Key key,
    @required this.items,  // 底部导航栏的显示项 = BottomNavigationBarItem类型的List  
    this.onTap, // 点击导航栏子项时的回调 = ValueChanged < int >
    this.currentIndex = 0, // 当前显示项的下标 = int
    BottomNavigationBarType type, // 底部导航栏的类型，有fixed和shifting两个类型，显示效果不一样 = BottomNavigationBarType 
    this.fixedColor, // 底部导航栏type为fixed时导航栏的颜色，如果为空的话默认使用ThemeData.primaryColor = Color
    this.iconSize = 24.0, // BottomNavigationBarItem icon的大小 = double
  })

const BottomNavigationBarItem({
  @required this.icon, // 要显示的图标控件，一般是Iocn
  this.title, // 要显示的标题控件，一般是Text
  Widget activeIcon, // 选中时要显示的icon，一般是Icon
  this.backgroundColor, // BottomNavigationBarType为shifting时的背景颜色
})
```

------

# 3. 使用步骤



```csharp
// 需要在Scaffold内部使用
Scaffold(
      appBar: AppBar(
        title: Text("底部导航栏"),
      ),
      bottomNavigationBar: BottomNavigationBar(
        items: bottomNavItems, // 导航栏下标item
        currentIndex: currentIndex, // 当前下标
        type: BottomNavigationBarType.fixed, // 点击导航栏样式
        fixedColor: Colors.green, // 点击icon颜色
        iconSize: 15, // icon大小
        onTap: (index) {
          _changePage(index); // 点击回调
        },
      ),
      body: pages[currentIndex], // 每个item的回调页面
    );

// 回调页面
  void _changePage(int index) {
    // 若点击的导航项不是当前项，则切换
    if (index != currentIndex) {
      setState(() {
        currentIndex = index;
      });
    }
  }
```

------

# 4. 实例讲解

### 4.1 设置要回调的页面

为了方便显示，仅显示一个文本



```java
// 页面1
import 'package:flutter/material.dart';

class PageA extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Center(
      child: Text("PageA"),
    );
  }
}

// 页面2
import 'package:flutter/material.dart';

class PageB extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Center(
      child: Text("PageB"),
    );
  }
}

// 页面3
import 'package:flutter/material.dart';

class PageC extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Center(
      child: Text("PageC"),
    );
  }
}

// 页面4
import 'package:flutter/material.dart';

class PageD extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Center(
      child: Text("PageD"),
    );
  }
}
```

### 4.2 核心代码

```
main.dart
```



```java
/**
 *  导入库
 **/
import 'package:flutter/material.dart';

import 'PageA.dart';
import 'PageB.dart';
import 'PageC.dart';
import 'PageD.dart'; // Material UI组件库

void main() => runApp(MyApp());

// 无状态控件显示
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
// 主界面入口返回的组件 = MaterialApp
    return MaterialApp(
      title: 'Widget_Demo', //标题
      theme: ThemeData(primarySwatch: Colors.blue), //主题色
      home: MyWidget(), // 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

class MyWidget extends StatefulWidget {
  @override
  State<StatefulWidget> createState() {
    return new _MyWidgetState();
  }
}

class _MyWidgetState extends State<MyWidget> {

  // 导航栏的item
  final List<BottomNavigationBarItem> bottomNavItems = [
    BottomNavigationBarItem(
      backgroundColor: Colors.blue,
      icon: Icon(Icons.home),
      title: Text("首页"),
    ),
    BottomNavigationBarItem(
      backgroundColor: Colors.green,
      icon: Icon(Icons.message),
      title: Text("消息"),
    ),
    BottomNavigationBarItem(
      backgroundColor: Colors.amber,
      icon: Icon(Icons.shopping_cart),
      title: Text("购物车"),
    ),
    BottomNavigationBarItem(
      backgroundColor: Colors.red,
      icon: Icon(Icons.person),
      title: Text("个人中心"),
    ),
  ];

  int currentIndex; // 当前下标

  final pages = [PageA(), PageB(), PageC(), PageD()];// 下标对应显示的界面

  @override
  void initState() {
    super.initState();
    currentIndex = 0;
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text("底部导航栏"),
      ),
      bottomNavigationBar: BottomNavigationBar(
        items: bottomNavItems, // 导航栏下标item
        currentIndex: currentIndex, // 当前下标
        type: BottomNavigationBarType.fixed,
        fixedColor: Colors.green,
        iconSize: 15,
        onTap: (index) {
          _changePage(index); // 点击回调
        },
      ),
      body: pages[currentIndex],
    );
  }

  // 切换页面
  void _changePage(int index) {
    // 若点击的导航项不是当前项切换
    if (index != currentIndex) {
      setState(() {
        currentIndex = index;
      });
    }
  }
}
```

### 4.3 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgm6nu90g307u0ck1c8.gif)

------

# 总结

- 本文全面介绍了`Flutter`的布局组件中顶部导航栏的实现：`BottomNavigationBar`
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



作者：Carson_Ho
链接：https://www.jianshu.com/p/81dcfcc9e5da
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。