[TOC]

# 前言

- `Flutter ListView`在日常开发中非常常见
- 今天，carson全面介绍`Flutter ListView`的使用，包括基础使用 & 进阶使用（下拉刷新、上拉加载），希望你们会喜欢。

示意图

------

# 目录

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgkxhx2sj30s80js0st.jpg)

示意图

------

# 1. 简介

- 定义：滚动型控件
- 作用：可滚动显示超出屏幕的内容
- 应用场景：显示超过一屏的内容

------

# 2. 基础使用

ListView的基础创建使用有三种方式：

- 默认构造函数：ListView（）
- ListView.build()
- ListView.separated()

# 使用方式1：ListView（）

### a. 简介

通过默认构造函数来创建列表，应用场景 = 短列表

### b. 构造方法



```dart
ListView({
  Axis scrollDirection = Axis.vertical, // 列表的滚动方向，可选值：Axis的horizontal、vertical
  ScrollController controller, // 控制器，与列表滚动相关，比如监听列表的滚动事件
  ScrollPhysics physics, // 列表滚动至边缘后继续拖动的物理效果，Android与iOS效果不同：Android = 波纹状（ClampingScrollPhysics），而iOS = 回弹的弹性效果（BouncingScrollPhysics）。若想不同的平台上呈现各自的效果可使用AlwaysScrollableScrollPhysics，它会根据不同平台自动选用各自的物理效果。若想禁用在边缘的拖动效果，可使用NeverScrollableScrollPhysics
  bool shrinkWrap = false, // 决定列表的长度是否仅包裹其内容的长度。当ListView嵌在一个无限长的容器组件中时，shrinkWrap必须为true，否则Flutter会给出警告；
  EdgeInsetsGeometry padding, // 列表内边距
  this.itemExtent, // 子元素长度。当列表中的每一项长度是固定的情况下可以指定该值，有助于提高列表的性能（因为它可以帮助ListView在未实际渲染子元素之前就计算出每一项元素的位置）；
  double cacheExtent, // 预渲染区域长度，ListView会在其可视区域的两边留一个cacheExtent长度的区域作为预渲染区域（对于ListView.build或ListView.separated构造函数创建的列表，不在可视区域和预渲染区域内的子元素不会被创建或会被销毁）；
  List<Widget> children = const <Widget>[], // 容纳子元素的组件数组
})
```

### c. 具体使用



```java
import 'package:flutter/material.dart'; // Material UI组件库

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
  ScrollController _controller = new ScrollController();

  @override
  void initState() {
    super.initState();
    // 设置监听方法
    _controller.addListener(() {
      print('_controller Listener');
      print(_controller.offset); // 打印滚动位置
    });
  }
  
  @override
  Widget build(BuildContext context) {
    return ListView(
      scrollDirection: Axis.vertical,
      // 设置方向

      itemExtent: 50,
      // 当滚动方向为垂直方向时，那么itemExtent = 子控件的高度
      // 当滚动方向为水平方向时，那么itemExtent = 子控件的宽度
      padding: EdgeInsets.all(1.0),
      // 设置边距

      physics: new AlwaysScrollableScrollPhysics(),
      // 设置到边缘后的效果

      controller: _controller,
      // 添加监听器
      
      // 设置子控件
      children: <Widget>[
        Text("test1"),
        Text("test2"),
        Text("test3"),
        Text("test4"),
        Text("test5"),
        Text("test6"),
        Text("test7"),
        Text("test8"),
        Text("test9"),
        Text("test10"),
        Text("test11"),
        Text("test12"),
        Text("test13"),
        Text("test14"),
        Text("test15"),
      ],
    );
  }
}
```

### d. 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgkuqqcij30f60og3ze.jpg)

这种方式创建的列表存在一个问题：对于那些长列表或者需要较昂贵渲染开销的子组件，即使还没有出现在屏幕中但仍然会被ListView所创建，这将是一项较大的开销，使用不当可能引起性能问题甚至卡顿。

# 使用方式2：ListView.build()

### a. 应用场景

长列表

### b. 构造函数



```cpp
ListView.builder({
  ...// 和ListView默认构造函数一样
  int itemCount, // 列表中元素的数量
  IndexedWidgetBuilder itemBuilder, // 子元素的渲染方法，允许自定义子元素组件
})

// 特别注意：不同于ListView默认构造函数通过children参数指定子元素的这种方式，ListView.build通过暴露统一的itemBuilder方法将渲染子元素的控制权交还给调用方。
```

### c. 具体使用



```dart
import 'package:flutter/material.dart'; // Material UI组件库

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
  ScrollController _controller = new ScrollController(); // 定义控制器
  List<String> litems = ["test 1","test 2","test 3","test 4","test 5","test 6","test 7","test 8"
                         ,"test 9","test 10","test 11","test 12","test 13","test 14","test 15"]; // 定义要显示的元素列表
  @override
  void initState() {
    super.initState();
    // 设置监听方法
    _controller.addListener(() {
      print('_controller Listener');
      print(_controller.offset); // 打印滚动位置
    });
  }

  @override
  Widget build(BuildContext context) {
    return ListView.builder
      (
        scrollDirection: Axis.vertical,
        // 设置方向

        itemExtent: 50,
        // 当滚动方向为垂直方向时，那么itemExtent = 子控件的高度
        // 当滚动方向为水平方向时，那么itemExtent = 子控件的宽度
        padding: EdgeInsets.all(1.0),
        // 设置边距

        physics: new AlwaysScrollableScrollPhysics(),
        // 设置到边缘后的效果

        controller: _controller, // 设置控制器
        
         // 设置元素数量 & 具体元素
        itemCount: litems.length,
        itemBuilder: (BuildContext ctxt, int index) {
          return new Text(litems[index]);
        }
    );
  }
}
```

### d. 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgksfdg4j30fc0oogmi.jpg)

# 使用方式3：ListView.separated()

### a. 应用场景

列表子项之间需要分割线

### b. 构造函数



```dart
ListView.separated({
  ... // 同ListView.build()的参数
  @required IndexedWidgetBuilder separatorBuilder
// 相比于ListView.build构造函数，ListView.separated多出的参数：暴露给调用方自定义分割线组件的回调方法，可自定义每个子元素之间的分割线
})
```

### c. 具体使用



```dart
import 'package:flutter/material.dart'; // Material UI组件库

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
  ScrollController _controller = new ScrollController();
  List<String> litems = [
    "test 1",
    "test 2",
    "test 3",
    "test 4",
    "test 5",
    "test 6",
    "test 7",
    "test 8",
    "test 9",
    "test 10",
    "test 11",
    "test 12",
    "test 13",
    "test 14",
    "test 15"
  ];

  @override
  void initState() {
    super.initState();
    // 设置监听方法
    _controller.addListener(() {
      print('_controller Listener');
      print(_controller.offset); // 打印滚动位置
    });
  }

  @override
  Widget build(BuildContext context) {
    return ListView.separated(
      scrollDirection: Axis.vertical,
      // 设置方向

      padding: EdgeInsets.all(1.0),
      // 设置边距

      physics: new AlwaysScrollableScrollPhysics(),
      // 设置到边缘后的效果

      controller: _controller,
      // 设置控制器

      // 设置元素数量 & 具体元素
      itemCount: litems.length,
      itemBuilder: (BuildContext ctxt, int index) {
        return new Text(litems[index]);
      },

      // 设置分割线
      separatorBuilder: (context, index) {
        return Divider(
          height: 0.5,
          indent: 75,
          color: Color(0xFF968493),
        );
      },
    );
  }
}
```

------

# 3. 进阶使用

ListView的进阶使用主要包括：下拉刷新 & 上拉加载

# 3.1 下拉刷新

### a. 简介

在Flutter中，ListView结合RefreshIndicator组件实现下拉刷新

### b. 原理

通过包裹一层RefreshIndicator，自定义onRefresh回调方法实现

### c. 构造方法



```kotlin
const RefreshIndicator({
    Key key,
    @required this.child, // 传入ListView组件
    this.displacement = 40.0, // 下拉距离，根据这个距离判定刷新执行。默认40.0
    @required this.onRefresh, // 刷新回调方法，返回类型必须为Future
    this.color, // 刷新进度条颜色，默认当前主题颜色
    this.backgroundColor, // 背景颜色
    this.color, // 刷新进度条颜色，默认当前主题颜色
    this.backgroundColor, // 背景颜色
    
// ...
  }) 
```

### d. 具体使用



```dart
// 使用RefreshIndicator实现下拉刷新
return RefreshIndicator(

  // 步骤1：设置组件ListView
  child: ListView.builder(
    // ...
   }),
  // 步骤2：设置属性
  displacement: 30, // 设置下拉距离为30时，进行刷新
  color: Colors.red,// 设置颜色
  backgroundColor: Colors.green,
  onRefresh: _handleRefresh, // 设置刷新后的方法
);
}

// 步骤3：设置下拉刷新方法：返回类型必须为Future
Future<Null> _handleRefresh() async {
  // ...
}
```

### e. 使用示例



```dart
import 'package:flutter/material.dart'; // Material UI组件库

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
  
  // 列表ListView数据
  List<String> litems = [
    "test 1",
    "test 2",
    "test 3",
    "test 4",
    "test 5",
    "test 6",
    "test 7",
    "test 8",
    "test 9",
    "test 10",
    "test 11",
    "test 12",
    "test 13",
    "test 14",
    "test 15"
  ];

  List<String> upgradeLitems = [
    " test A",
    " test B",
    " test C",
    " test D",
    " test E",
    " test F",
    " test G",
    " test I",
    " test J",
    " test K",
    " test L",
    " test M",
    " test N",
    " test O",
    " test P"
  ];

  @override
  void initState() {
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    
    // 使用RefreshIndicator实现下拉刷新
    return RefreshIndicator(

      // 设置组件ListView
      child: ListView.builder(
          scrollDirection: Axis.vertical,
          // 设置方向

          // 设置元素数量 & 具体元素
          itemCount: litems.length,
          itemBuilder: (BuildContext ctxt, int index) {
            return new Text(litems[index]);
          }),

      displacement: 30, // 设置下拉距离为30时，进行刷新
      color: Colors.red,// 设置颜色
      backgroundColor: Colors.green,

      onRefresh: _handleRefresh, // 设置刷新后的方法
    );
  }

  // 下拉刷新方法
  Future<Null> _handleRefresh() async {
    // 模拟数据的延迟加载
    await Future.delayed(Duration(seconds: 2), () {
      setState(() {
        print('refresh');
        // 添加更新的数据
        litems = upgradeLitems;
      });
    });
  }
}
```

### f. 测试效果

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgkonmstg307w0fcqv5.gif)

示意图

------

# 3.2 上拉加载

方式有两种：

1. ListView.controller属性：通过ScrollController可以判断ListView是否滑动到了底部，再进行上拉加载
2. NotificationListener：监听ListVIew的滑动状态，当ListView滑动到底部时，进行上拉加载

### 方式1：ListView.controller属性

### a. 原理

通过ListView.controller属性可以判断ListView是否滑动到了底部，再进行上拉加载

### b. 具体实现



```cpp
// 步骤1：初始化控制器
ScrollController _controller = ScrollController();

// 步骤2：对控制器添加监听：监听ListView是否滚动到底部
  _controller.addListener(() {
    // _scrollController.position.pixels表示ListView当前滑动的距离
    // _scrollController.position.maxScrollExtent表示ListView可以滑动的最大距离
    // 因此pixels >= maxScrollExtent就表示ListView已经滑动到了底部
    if (_controller.position.pixels >=
        _controller.position.maxScrollExtent) {
      
      // 执行上拉加载逻辑
    }
  });

  // 步骤3：使用后要移除监听
  _controller.dispose(); 
```

### c. 使用示例



```dart
import 'package:flutter/material.dart'; // Material UI组件库

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
  // 初始化控制器
  ScrollController _controller = ScrollController();
  // 列表ListView数据
  List<String> litems = [
    "test 1",
    "test 2",
    "test 3",
    "test 4",
    "test 5",
    "test 6",
    "test 7",
    "test 8",
    "test 9",
    "test 10",
    "test 11",
    "test 12",
    "test 13",
    "test 14",
    "test 15"
  ];

  List<String> upgradeLitems = [
    " 上拉加载的新数据",
    " test B",
    " test C",
    " test D",
    " test E",
    " test F",
    " test G",
    " test I",
    " test J",
    " test K",
    " test L",
    " test M",
    " test N",
    " test O",
    " test P"
  ];

  @override
  void initState() {
    super.initState();

    // 对控制器添加监听：监听ListView是否滚动到底部
    _controller.addListener(() {
      // _scrollController.position.pixels表示ListView当前滑动的距离
      // _scrollController.position.maxScrollExtent表示ListView可以滑动的最大距离
      // 因此pixels >= maxScrollExtent就表示ListView已经滑动到了底部
      if (_controller.position.pixels >=
          _controller.position.maxScrollExtent) {
        print('滑动到了底部');
        // 执行上拉加载逻辑
        _loadMore();
      }
    });
  }

 @override
  void dispose() {
    super.dispose();
    _controller.dispose(); // 使用后要移除监听
  }

  @override
  Widget build(BuildContext context) {

    return ListView.builder
      (
        scrollDirection: Axis.vertical,
        // 设置方向

        itemExtent: 50,
        // 当滚动方向为垂直方向时，那么itemExtent = 子控件的高度
        // 当滚动方向为水平方向时，那么itemExtent = 子控件的宽度
        padding: EdgeInsets.all(1.0),
        // 设置边距

        physics: new AlwaysScrollableScrollPhysics(),
        // 设置到边缘后的效果

        controller: _controller, // 设置控制器

        // 设置元素数量 & 具体元素
        itemCount: litems.length,
        itemBuilder: (BuildContext ctxt, int index) {
          return new Text(litems[index]);
        }
    );
  }

  // 上拉加载
  Future<Null> _loadMore() async {
    // 模拟数据的延迟加载
    await Future.delayed(Duration(seconds: 2), () {
      setState(() {
        litems.addAll(upgradeLitems);
      });
    });
  }
}
```

### d. 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgkm3l9qg307s0d8kjm.gif)

### 方式2：NotificationListener

### a. 简介

NotificationListener是一个Widget，可监听子Widget发出的Notification

### b. 原理

ListView在滑动时中会发出ScrollNotification类型的通知，可通过监听该通知得到ListView的滑动状态，判断是否滑动到了底部，从而进行上拉加载

### c. 具体实现



```kotlin
// 使用NotificationListener的onNotification属性进行监听
return  NotificationListener<ScrollNotification>(
      onNotification: (ScrollNotification scrollNotification) {
        // _scrollController.position.pixels表示ListView当前滑动的距离
        // _scrollController.position.maxScrollExtent表示ListView可以滑动的最大距离
        // 因此pixels >= maxScrollExtent就表示ListView已经滑动到了底部
        if (scrollNotification.metrics.pixels >=
            scrollNotification.metrics.maxScrollExtent) {
          // 执行上拉加载逻
        }
        return false;
      },
      child: ListView.builder
        (
          // 构建ListView
      )
    );
```

### d. 使用示例

NotificationListener有一个onNotification属性，定义了监听的回调方法，通过它来处理加载更多逻辑



```dart
import 'package:flutter/material.dart'; // Material UI组件库

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
  // 列表ListView数据
  List<String> litems = [
    "test 1",
    "test 2",
    "test 3",
    "test 4",
    "test 5",
    "test 6",
    "test 7",
    "test 8",
    "test 9",
    "test 10",
    "test 11",
    "test 12",
    "test 13",
    "test 14",
    "test 15"
  ];

  List<String> upgradeLitems = [
    " 上拉加载的新数据",
    " test B",
    " test C",
    " test D",
    " test E",
    " test F",
    " test G",
    " test I",
    " test J",
    " test K",
    " test L",
    " test M",
    " test N",
    " test O",
    " test P"
  ];

  @override
  Widget build(BuildContext context) {

    // 使用NotificationListener的onNotification属性进行监听
    return  NotificationListener<ScrollNotification>(
          onNotification: (ScrollNotification scrollNotification) {
            // _scrollController.position.pixels表示ListView当前滑动的距离
            // _scrollController.position.maxScrollExtent表示ListView可以滑动的最大距离
            // 因此pixels >= maxScrollExtent就表示ListView已经滑动到了底部
            if (scrollNotification.metrics.pixels >=
                scrollNotification.metrics.maxScrollExtent) {
              print('滑动到了底部');
              // 执行上拉加载逻辑
              _loadMore();
            }
            return false;
          },
          child: ListView.builder
            (
              scrollDirection: Axis.vertical,
              // 设置方向

              itemExtent: 50,
              // 当滚动方向为垂直方向时，那么itemExtent = 子控件的高度
              // 当滚动方向为水平方向时，那么itemExtent = 子控件的宽度
              padding: EdgeInsets.all(1.0),
              // 设置边距

              physics: new AlwaysScrollableScrollPhysics(),
              // 设置到边缘后的效果

              // 设置元素数量 & 具体元素
              itemCount: litems.length,
              itemBuilder: (BuildContext ctxt, int index) {
                return new Text(litems[index]);
              }
          )
        );
  }

  // 上拉加载
  Future<Null> _loadMore() async {
    // 模拟数据的延迟加载
    await Future.delayed(Duration(seconds: 2), () {
      setState(() {
        litems.addAll(upgradeLitems);
      });
    });
  }
}
```

------

# 4. 总结

- 本文全面介绍了Flutter ListView组件的使用
- 接下来推出的文章，我将继续讲解Flutter的相关知识，包括使用语法、实战等，感兴趣的读者可以继续关注我的博客哦：[Carson_Ho的Android博客](https://www.jianshu.com/users/383970bef0a0/latest_articles)



作者：Carson_Ho
链接：https://www.jianshu.com/p/f48b65a111ad
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。