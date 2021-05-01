[TOC]

- 今天，我主要讲解Flutter中完整页面方面的Widget，包括`Material App`、`Scaffold`、`AppBar`，希望你们会喜欢。

示意图

------

# 1. Material App

- 定义：使用Material Design设计风格的应用的顶层主界面入口，包含主题颜色、标题、主颜色等。
- 作用：包含了Material Design设计风格的应用所需要的基本控件
- 常用属性：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgh3x56fj30q40gidhx.jpg)

示意图

此处详细列出主题（Theme）的设置

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgh200bvj30m80dmtdj.jpg)

示意图

- 代码演示



```java
import 'package:flutter/material.dart'; // Material UI组件库

void main() => runApp(MyApp());

// 无状态控件显示
class MyApp extends StatelessWidget{

  @override
  Widget build(BuildContext context){
// 主界面入口返回的组件 = MaterialApp
    return MaterialApp(
      title:'Widget_Demo',//标题
      theme:ThemeData(primarySwatch: Colors.blue),//主题色
      home:MyHomePage(),// 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

// 返回的Widget对象
class MyHomePage extends StatelessWidget{

  @override
  Widget build(BuildContext context){
    return Scaffold(
      //设置appbar
      appBar:new AppBar(
        title:new Text('Flutter Demo'),
      ),
      //主体
      body:new Center(
        //在屏幕中央显示一个文本
        child:new Text('carsonho demo'),
      ),
    );
  }
}
```

- 效果

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgh05yihj307j0b3dfr.jpg)

示意图

------

# 2. Scaffold

实现了基本的 Material Dsign布局，含AppBar、Body主题内容等。

- 设置属性



```kotlin
Scaffold({
    Key key,
    this.appBar,// 设置应用栏，显示在脚手架顶部
    this.body,// 设置脚手架内容区域控件
    this.floatingActionButton,//设置显示在上层区域的按钮，默认位置位于右下角
    this.floatingActionButtonLocation,// 设置floatingActionButton的位置
    this.floatingActionButtonAnimator,// floatingActionButtonAnimator 动画
    this.persistentFooterButtons,// 一组显示在脚手架底部的按钮(在bottomNavigationBar底部导航栏的上面)
    this.drawer,// 设置左边侧边栏
    this.endDrawer,// 设置右边侧边栏
    this.bottomNavigationBar,// 设置底部导航栏
    this.bottomSheet,// 底部抽屉栏
    this.backgroundColor,// 设置脚手架内容区域的颜色
    this.resizeToAvoidBottomPadding = true,// 控制界面内容 body 是否重新布局来避免底部被覆盖，比如当键盘显示的时候，重新布局避免被键盘盖住内容。
  })
```

- 具体使用



```java
void main() => runApp(MyApp());

//用无状态控件显示
class MyApp extends StatelessWidget{

  @override
  Widget build(BuildContext context){
    return MaterialApp(
      title:'Widget_Demo',//标题
      theme:ThemeData(primarySwatch: Colors.blue),//主题色
      home:MyHomePage(),// 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

// 返回的Widget对象
class MyHomePage extends StatelessWidget{

  @override
  Widget build(BuildContext context){
    return Scaffold(
      //设置appbar
      appBar:new AppBar(
        title:new Text('Flutter Demo'),
      ),
      //主体
      body:new Center(
        //在屏幕中央显示一个文本
        child:new Text('carsonho demo'),
      ),
      // 设置左侧抽屉，添加一个空的ListView
      drawer:Drawer(
        child:new Center(
          //在屏幕中央显示一个文本
          child:new Text('Scafflod Drawer'),
        ),
      ),
    );
  }
}
```

- 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwggxdr98g307m0b012b.gif)

示意图

------

# 3. AppBar

- 定义：Material风格应用栏，具备工具栏 & 其他小型Widget
- 应用场景：用于Scaffold.appBar属性
- 属性设置



```kotlin
AppBar({
    Key key,
    this.leading, // Widget，title前面的组件，一般是图标按钮
    this.automaticallyImplyLeading = true, // 配合leading使用，取决于automaticallyImplyLeading == true && leading ==null
    this.title, // Widget，AppBar的标题，类型为Widget，一般显示文本
    this.actions, // Widget List，一个 Widget 列表，代表 Toolbar 中所显示的菜单，对于常用的菜单，通常使用 IconButton 来表示；对于不常用的菜单通常使用 PopupMenuButton 来显示为三个点，点击后弹出二级菜单。
    this.flexibleSpace, //  Widget，一个显示在 AppBar 下方的控件，高度和 AppBar 高度一样，可以实现一些特殊的效果，该属性通常在 SliverAppBar 中使用。
    this.bottom, // PreferredSizeWidget，出现在应用程序栏底部的组件，通常是一个标签栏（TarBar）
    this.elevation, // 控制下方阴影栏的坐标
    this.backgroundColor, // 背景颜色
    this.brightness, // Brightness，亮度，有白色和黑色两种主题，默认值为 ThemeData.primaryColorBrightness。
    this.iconTheme, // IconThemeData - Appbar 上图标的颜色、透明度、和尺寸信息。默认值为 ThemeData.primaryIconTheme。
    this.textTheme, // TextTheme，Appbar 上的文字样式。
    this.primary = true, // appbar是否显示在任务栏顶部
    this.centerTitle, // bool，标题是否居中显示，默认值根据不同的操作系统，显示方式不一样。
    this.titleSpacing = NavigationToolbar.kMiddleSpacing,
    this.toolbarOpacity = 1.0, // 顶部栏的透明度：值1.0 = 完全不透明，值0.0 = 完全透明
    this.bottomOpacity = 1.0, // 底部栏的透明度：值1.0 = 完全不透明，值0.0 = 完全透明
  })
```

- 代码演示



```dart
void main() => runApp(MyApp());

//用无状态控件显示
class MyApp extends StatelessWidget{

  @override
  Widget build(BuildContext context){
    return MaterialApp(
      title:'Widget_Demo',//标题
      theme:ThemeData(primarySwatch: Colors.blue),//主题色
      home:MyHomePage(),// 返回一个Widget对象，用来定义当前应用打开的时候，所显示的界面
    );
  }
}

// 返回的Widget对象
class MyHomePage extends StatelessWidget{

  @override
  Widget build(BuildContext context){
    return Scaffold(
      //设置appbar
      appBar:new AppBar(
        title: new Text('首页'),
        leading: new Icon(Icons.home),
        backgroundColor: Colors.blue,
        centerTitle: true,
        actions: <Widget>[ // 设置title后的WidgetList
          // 非隐藏的菜单
          new IconButton(
              icon: new Icon(Icons.add_alarm),
              tooltip: 'Add Alarm',
              onPressed: () {}
          ),
          // 隐藏的菜单
          new PopupMenuButton<String>(
            itemBuilder: (BuildContext context) => <PopupMenuItem<String>>[
              gethideItem(Icons.message, '聊天', '1'),
              gethideItem(Icons.group_add, '添加', '2'),
              gethideItem(Icons.cast_connected, '连接', '3'),
            ],
            onSelected: (String action) {
              // 点击选项的时候
              switch (action) {
                case '1':
                  print(1);
                  break;
                case '2':
                  print(2);
                  break;
                case '3':
                  print(3);
                  break;
              }
            },
          ),
        ],
      ),
      //主体
      body:new Center(
        //在屏幕中央显示一个文本
        child:new Text('carsonho demo'),
      ),
      // 设置左侧抽屉，添加一个空的ListView
    );
  }

// 方便返回每个隐藏的菜单项
  gethideItem(IconData icon, String text, String id) {
    return new PopupMenuItem<String>(
        value: id,
        child: new Row(
          mainAxisAlignment: MainAxisAlignment.spaceEvenly,
          children: <Widget>[
            new Icon(icon, color: Colors.blue),
            new Text(text),
          ],
        )
    );
  }
}
```

- 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwggv85g5g307m0b07eb.gif)

------

# 4. 总结

- 本文主要讲解了Flutter中完整页面方面的Widget，包括`Material App`、`Scaffold`、`AppBar`
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
链接：https://www.jianshu.com/p/9ddc3ec3c752
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。