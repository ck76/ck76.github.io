[TOC]

今天，我主要讲解Flutter中按钮方面的Widget，包括：

- RaisedButton：凸起、有阴影 - 继承自MaterialButton
- FlatButton：扁平、无阴影 - 继承自MaterialButton
- OutlineButton：带边框 - 继承自MaterialButton
- IconButton：带图标 - 继承自StatelessWidget

希望你们会喜欢。



示意图

------

# 1. RaisedButton

- 核心属性



```dart
 const RaisedButton({
    Key key,
    @required VoidCallback onPressed, // 按下按钮的回调
    Widget child, // 子控件，一般是传入一个TextWidget
    Color textColor, // 文本颜色
    Color disabledTextColor, /// 禁用时文本的颜色
    Color color, // 按钮颜色
    Color disabledColor,// 禁用时的按钮颜色
    Color highlightColor, // 点击 / 长按的颜色
    Color splashColor, // 点击时水波纹颜色
    double elevation, // 阴影范围，值大 - 范围越大
    double highlightElevation,
    double disabledElevation,
    EdgeInsetsGeometry padding, // 内边距，接受的类型是：EdgeInsetsGeometry类型
    ShapeBorder shape, // 按钮形状，主要分为：BeveledRectangleBorder（带斜角的长方形边框）、CircleBorder（圆形边框）、RoundedRectangleBorder（圆角矩形）、StadiumBorder（两端是半圆的边框）
    Clip clipBehavior = Clip.none,
    MaterialTapTargetSize materialTapTargetSize,
    Duration animationDuration,
  })
```

- 具体使用



```dart
/**
 *  导入库
 **/
import 'package:flutter/material.dart';// Material UI组件库

void main() => runApp(MyApp());

// 无状态控件显示
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
// 主界面入口返回的组件 = MaterialApp
    return MaterialApp(
      title: 'Widget_Demo', //标题
      theme: ThemeData(primarySwatch: Colors.blue), //主题色
      home: MyHomePage(), // 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

// 返回的Widget对象
class MyHomePage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      //设置appbar
      appBar: new AppBar(
        title: new Text('Flutter Demo'),
      ),
      //主体
      body: new RaisedButton(
        color: Colors.red, // 按钮颜色
        splashColor: Colors.black, // 水波纹颜色
        highlightColor: Colors.green, // 长按样式
        textColor: Colors.white, // 文本颜色
        child: Text("RaiseButton"), // 按钮字样
        padding: EdgeInsets.fromLTRB(0,5,6,7),// 设置四个方向的内边距
        shape: RoundedRectangleBorder( // 设置按钮形状为圆角矩形
          borderRadius: BorderRadius.all(Radius.circular(10)),
        ),
        onPressed: _pressCallBack, // 点击事件
      ),
    );
  }

  _pressCallBack() {
    print("点击了按钮");
  }

}
```

- 效果图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgg85z9vg307o0b4wnt.gif)

------

# 2. FlatButton

- 定义
  扁平、无阴影 - 继承自MaterialButton

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgg6pyttg307o0b4aks.gif)

- 具体使用
  与RaisedButton的属性设置十分类似，此处仅作展示



```java
import 'package:flutter/material.dart';// Material UI组件库

void main() => runApp(MyApp());

// 无状态控件显示
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
// 主界面入口返回的组件 = MaterialApp
    return MaterialApp(
      title: 'Widget_Demo', //标题
      theme: ThemeData(primarySwatch: Colors.blue), //主题色
      home: MyHomePage(), // 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

// 返回的Widget对象
class MyHomePage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      //设置appbar
      appBar: new AppBar(
        title: new Text('Flutter Demo'),
      ),
      //主体
      body: new FlatButton(
        color: Colors.red, // 按钮颜色
        splashColor: Colors.black, // 水波纹颜色
        highlightColor: Colors.green, // 长按样式
        textColor: Colors.white, // 文本颜色
        child: Text("FlatButton"), // 按钮字样
        padding: EdgeInsets.fromLTRB(0,5,6,7),// 设置四个方向的内边距
        shape: RoundedRectangleBorder( // 设置按钮形状为圆角矩形
          borderRadius: BorderRadius.all(Radius.circular(10)),
        ),
        onPressed: _pressCallBack, // 点击事件
      ),
    );
  }

  _pressCallBack() {
    print("点击了按钮");
  }

}
```

# 3. OutlineButton

- 定义
  带边框 - 继承自MaterialButton

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgg5brp8g307o0b4qd2.gif)

- 具体使用
  与RaisedButton的属性设置十分类似，此处仅作展示



```java
import 'package:flutter/material.dart';// Material UI组件库

void main() => runApp(MyApp());

// 无状态控件显示
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
// 主界面入口返回的组件 = MaterialApp
    return MaterialApp(
      title: 'Widget_Demo', //标题
      theme: ThemeData(primarySwatch: Colors.blue), //主题色
      home: MyHomePage(), // 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

// 返回的Widget对象
class MyHomePage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      //设置appbar
      appBar: new AppBar(
        title: new Text('Flutter Demo'),
      ),
      //主体
      body: new OutlineButton(
        borderSide:new BorderSide(color: Colors.red), // 自定义边框颜色&样式
        splashColor: Colors.black, // 水波纹颜色
        highlightColor: Colors.green, // 长按样式
        textColor: Colors.red, // 文本颜色
        child: Text("OutlineButton"), // 按钮字样
        padding: EdgeInsets.fromLTRB(0,5,6,7),// 设置四个方向的内边距
        shape: RoundedRectangleBorder( // 设置按钮形状为圆角矩形
          borderRadius: BorderRadius.all(Radius.circular(10)),
        ),
        onPressed: _pressCallBack, // 点击事件
      ),
    );
  }

  _pressCallBack() {
    print("点击了按钮");
  }

}
```

------

# 4. IconButton

- 定义
  带图标 - 继承自StatelessWidget

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgg3lnd7g307o0b4qaw.gif)

- 具体使用
  与RaisedButton的属性设置十分类似，此处仅作展示



```java
import 'package:flutter/material.dart';// Material UI组件库

void main() => runApp(MyApp());

// 无状态控件显示
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
// 主界面入口返回的组件 = MaterialApp
    return MaterialApp(
      title: 'Widget_Demo', //标题
      theme: ThemeData(primarySwatch: Colors.blue), //主题色
      home: MyHomePage(), // 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

// 返回的Widget对象
class MyHomePage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      //设置appbar
      appBar: new AppBar(
        title: new Text('Flutter Demo'),
      ),
      //主体
      body: new IconButton(
        icon:Icon(Icons.add),
        color: Colors.blue, // 按钮颜色
        splashColor: Colors.black, // 水波纹颜色
        highlightColor: Colors.green, // 长按样式
        onPressed: _pressCallBack, // 点击事件
      ),
    );
  }

  _pressCallBack() {
    print("点击了按钮");
  }
}
```

------

# 5. 总结

- 本文主要讲解了Flutter中按钮组件方面的Widget，包括RaisedButton、FlatButton、OutlineButton、IconButton
- 接下来推出的文章，我将继续讲解Flutter的相关知识，包括更多的Widget用法、实例应用等，感兴趣的读者可以继续关注我的博客哦：[Carson_Ho的Android博客](https://www.jianshu.com/users/383970bef0a0/latest_articles)

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
链接：https://www.jianshu.com/p/dac0764c4d19
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。