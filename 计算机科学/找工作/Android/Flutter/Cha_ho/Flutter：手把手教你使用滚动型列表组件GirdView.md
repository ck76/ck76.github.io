[TOC]

# 前言

主要讲解Flutter常用的滚动型组件`GridView`组件，希望你们会喜欢。

> 滚动型组件的用法十分相似，可参考学习`ListView`组件，因为两者都继承自`BoxScrollView`



------

# 目录

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgivio4cj30xc0lqtb0.jpg)

------

# 1. 属性介绍



```dart
GridView({
  Key key,
  Axis scrollDirection = Axis.vertical, // 列表的滚动方向，可选值：Axis的horizontal、vertical
  bool reverse = false,
  ScrollController controller, // 控制器，与列表滚动相关，比如监听列表的滚动事件
  ScrollPhysics physics, // 列表滚动至边缘后继续拖动的物理效果，Android与iOS效果不同：Android = 波纹状（ClampingScrollPhysics），而iOS = 回弹的弹性效果（BouncingScrollPhysics）。若想不同的平台上呈现各自的效果可使用AlwaysScrollableScrollPhysics，它会根据不同平台自动选用各自的物理效果。若想禁用在边缘的拖动效果，可使用NeverScrollableScrollPhysics
  bool shrinkWrap = false, // 决定列表的长度是否仅包裹其内容的长度。当ListView嵌在一个无限长的容器组件中时，shrinkWrap必须为true，否则Flutter会给出警告；
  EdgeInsetsGeometry padding, // 列表内边距
  double cacheExtent,// 预渲染区域长度，ListView会在其可视区域的两边留一个cacheExtent长度的区域作为预渲染区域（对于ListView.build或ListView.separated构造函数创建的列表，不在可视区域和预渲染区域内的子元素不会被创建或会被销毁）；
  List<Widget> children = const <Widget>[], // 容纳子元素的组件数组
  @required this.gridDelegate, // 控制排列子元素的一个委托
})
```

下面主要讲解属性 `gridDelegate`：类型 = `SliverGridDelegate`，是一个抽象类 & 有2个实现类：

- 类型1：`SliverGridDelegateWithFixedCrossAxisCount` = 用于固定列数场景
- 类型2：`SliverGridDelegateWithMaxCrossAxisExtent` = 用于子元素有最大宽度限制场景

下面将详细介绍



```kotlin
// 类型1：SliverGridDelegateWithFixedCrossAxisCount
 // 应用场景：布局中每一行的列数 = 固定
 // 常用属性（构造函数）
  SliverGridDelegateWithFixedCrossAxisCount({
    @required this.crossAxisCount, // 列数，即一行有几个子元素；
    this.mainAxisSpacing = 0.0, // 主轴方向上的空隙间距；
    this.crossAxisSpacing = 0.0, // 次轴方向上的空隙间距；
    this.childAspectRatio = 1.0, // 子元素的宽高比例。
  })

// 类型2：SliverGridDelegateWithMaxCrossAxisExtent
  // 应用场景：子元素有最大宽度限制
  // 常用属性（构造函数）
  SliverGridDelegateWithMaxCrossAxisExtent({
    this.mainAxisSpacing = 0.0, // 主轴方向上的空隙间距；
    this.crossAxisSpacing = 0.0, // 次轴方向上的空隙间距；
    this.childAspectRatio = 1.0, // 子元素的宽高比例
    @required this.maxCrossAxisExtent, // 子元素的最大宽度可能是多少，然后计算得到合适的列宽
  // 示例：假如手机屏宽375，crossAxisSpacing值为0，
  // maxCrossAxisExtent值 = 125时，网格列数 = 3。因为125 * 3 = 375，刚好，每一列的宽度就是375/3。
  // maxCrossAxisExtent值 = 126时，网格列数 = 3。因为126 * 3 > 375，显示不下，每一列的宽度将是375/3。
  // maxCrossAxisExtent值为124时，网格列数 = 4。因为124 * 3 < 375，仍有多余，每一列的宽度将是375/4。
  })
```

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgisuoeij30kg0jwmxn.jpg)

------

# 2. 基础使用（含示例讲解）

### 2.1 示例1

使用：`SliverGridDelegateWithFixedCrossAxisCount` = 用于固定列数场景

- 示例代码



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
    return GridView(
      scrollDirection: Axis.vertical, // 排列方向
      controller: _controller, // 设置控制器
      padding: EdgeInsets.all(10), // 内边距
      physics: new AlwaysScrollableScrollPhysics(), // 设置到边缘后的效果
      children: getWidgetList(), // 表格数据（仅作demo展示，代码在下方）

      // 设置控制排列子元素的一个委托：固定列数
      gridDelegate: SliverGridDelegateWithFixedCrossAxisCount(
        crossAxisCount: 3, // 每列3个
        childAspectRatio: 0.5,//宽高比为1:2
        mainAxisSpacing: 10, // 主轴、次轴方向间隔
        crossAxisSpacing: 10,
      ),
    );
  }

  // GridView数据（仅作demo展示）
  List<String> litems = [
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15"
  ];

  List<Widget> getWidgetList() {
    return litems.map((item) => getItemContainer(item)).toList();
  }

  Widget getItemContainer(String item) {
    return Container(
      alignment: Alignment.center,
      child: Text(
        item,
        style: TextStyle(color: Colors.white, fontSize: 20),
      ),
      color: Colors.blue,
    );
  }
}
```

- 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgiqqz8ig30ju0d8e84.gif)

### 2.2 示例2

使用：`SliverGridDelegateWithMaxCrossAxisExtent` = 用于子元素有最大宽度限制场景

- 示例代码



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
    return GridView(
      scrollDirection: Axis.vertical, // 排列方向
      controller: _controller, // 设置控制器
      padding: EdgeInsets.all(10), // 内边距
      physics: new AlwaysScrollableScrollPhysics(), // 设置到边缘后的效果
      children: getWidgetList(), // 表格数据（仅作demo展示，代码在下方）

      // 设置控制排列子元素的一个委托：限制子元素的最大宽度
      gridDelegate: SliverGridDelegateWithMaxCrossAxisExtent(
        maxCrossAxisExtent: 200, //子控件最大宽度为200
        childAspectRatio: 0.5,//宽高比为1:2
        mainAxisSpacing: 10, // 主轴、次轴方向间隔
        crossAxisSpacing: 10,
      ),
    );
  }

  // GridView数据（仅作demo展示）
  List<String> litems = [
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15"
  ];

  List<Widget> getWidgetList() {
    return litems.map((item) => getItemContainer(item)).toList();
  }

  Widget getItemContainer(String item) {
    return Container(
      alignment: Alignment.center,
      child: Text(
        item,
        style: TextStyle(color: Colors.white, fontSize: 20),
      ),
      color: Colors.blue,
    );
  }
}
```

- 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgimlc8gg30f60d8npg.gif)

------

# 3. 进阶使用

`GridView`组件除了上面说的基础使用方式（默认构造函数），还有一些进阶使用：

- `GridView.builder`
- `GridView.count`
- `GridView.extent`
- `GridView.custom`

下面，我将详细介绍其使用。

### 类型1：GridView.builder

#### a. 特点

根据子元素是否出现在屏幕内而动态创建销毁，减少内存消耗，更高效渲染

#### b. 应用场景

长列表、数据量不确定（如网络请求）

#### c. 属性（构造函数）



```dart
 GridView.builder({
  Key key,
  Axis scrollDirection = Axis.vertical, // 列表的滚动方向，可选值：Axis的horizontal、vertical
  ScrollController controller, // 控制器，与列表滚动相关，比如监听列表的滚动事件
  ScrollPhysics physics, // 列表滚动至边缘后继续拖动的物理效果，Android与iOS效果不同：Android = 波纹状（ClampingScrollPhysics），而iOS = 回弹的弹性效果（BouncingScrollPhysics）。若想不同的平台上呈现各自的效果可使用AlwaysScrollableScrollPhysics，它会根据不同平台自动选用各自的物理效果。若想禁用在边缘的拖动效果，可使用NeverScrollableScrollPhysics
  bool shrinkWrap = false, // 决定列表的长度是否仅包裹其内容的长度。当ListView嵌在一个无限长的容器组件中时，shrinkWrap必须为true，否则Flutter会给出警告；
  EdgeInsetsGeometry padding, // 列表内边距
  double cacheExtent,// 预渲染区域长度，ListView会在其可视区域的两边留一个cacheExtent长度的区域作为预渲染区域（对于ListView.build或ListView.separated构造函数创建的列表，不在可视区域和预渲染区域内的子元素不会被创建或会被销毁）；
  List<Widget> children = const <Widget>[], // 容纳子元素的组件数组
  @required this.gridDelegate, // 控制排列子元素的一个委托

  // 上面属性同默认构造函数，主要关注下面两个特别的属性
  @required IndexedWidgetBuilder itemBuilder,// 列表项构造器，返回一个Widget
  int itemCount, // 列表的数量，一般都是集合的长度
  })
```

### d. 示例代码



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

  // GridView数据（仅作demo展示）
  List<String> litems = [
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15"
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
    return GridView.builder(
      scrollDirection: Axis.vertical, // 排列方向
      controller: _controller, // 设置控制器
      padding: EdgeInsets.all(10), // 内边距
      physics: new AlwaysScrollableScrollPhysics(), // 设置到边缘后的效果
      gridDelegate: SliverGridDelegateWithFixedCrossAxisCount( // 设置控制排列子元素的一个委托：固定列数
        crossAxisCount: 3, // 每列3个
        childAspectRatio: 0.5,//宽高比为1:2
        mainAxisSpacing: 10, // 主轴、次轴方向间隔
        crossAxisSpacing: 10,
      ),

        // 设置元素数量 & 具体元素（仅作demo展示，代码在下方）
        itemCount: litems.length,
        itemBuilder: (BuildContext ctxt, int index) {
          // 当数据加载完毕时 追加数据
          if (index == litems.length - 1 && litems.length < 200) {
            _addIndex();
          }
          return Container(
            alignment: Alignment.center,
            child: Text(
              litems[index],
              style: TextStyle(color: Colors.white, fontSize: 20),
            ),
            color: Colors.blue,
          );
        }
    );
  }

  void _addIndex() {
    // 此处需延时加载  否则会抱The widget on which setState() or markNeedsBuild() was called was:错误
    Future.delayed(Duration(milliseconds: 200)).then((e) {
      setState(() {
        litems.add("add");
      });
    });
  }
}
```

#### e. 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgiimxwwg30f60d8npg.gif)

------

### 类型2：GridView.count

#### a. 特点

封装了SliverGridDelegateWithFixedCrossAxisCount

#### b. 应用场景

布局中每一行的列数 = 固定

#### c. 属性（构造函数）



```kotlin
GridView.count({
   Key key,
   Axis scrollDirection = Axis.vertical, // 列表的滚动方向，可选值：Axis的horizontal、vertical
   @required this.crossAxisCount, // 列数，即一行有几个子元素；
   this.mainAxisSpacing = 0.0, // 主轴方向上的空隙间距；
   this.crossAxisSpacing = 0.0, // 次轴方向上的空隙间距；
   this.childAspectRatio = 1.0, // 子元素的宽高比例
}
```

#### d. 示例代码



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
    return GridView.count(
      scrollDirection: Axis.vertical, // 排列方向
      controller: _controller, // 设置控制器
      padding: EdgeInsets.all(10), // 内边距
      physics: new AlwaysScrollableScrollPhysics(), // 设置到边缘后的效果
      crossAxisCount: 3, // 每列3个
      childAspectRatio: 0.5,//宽高比为1:2
      mainAxisSpacing: 10, // 主轴、次轴方向间隔
      crossAxisSpacing: 10,
      children: getWidgetList(), // 表格数据（仅作demo展示，代码在下方）
    );
  }

  // GridView数据（仅作demo展示）
  List<String> litems = [
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15"
  ];

  List<Widget> getWidgetList() {
    return litems.map((item) => getItemContainer(item)).toList();
  }

  Widget getItemContainer(String item) {
    return Container(
      alignment: Alignment.center,
      child: Text(
        item,
        style: TextStyle(color: Colors.white, fontSize: 20),
      ),
      color: Colors.blue,
    );
  }
}
```

#### e. 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgiduvplj30f40ow74g.jpg)

------

### 类型3：GridView.extent

#### a. 特点

封装了SliverGridDelegateWithMaxCrossAxisExtent

#### b. 应用场景

子元素有最大宽度限制

#### c. 属性（构造函数）



```kotlin
 GridView.extent({
   Key key,
   Axis scrollDirection = Axis.vertical, // 列表的滚动方向，可选值：Axis的horizontal、vertical
   this.mainAxisSpacing = 0.0, // 主轴方向上的空隙间距；
   this.crossAxisSpacing = 0.0, // 次轴方向上的空隙间距；
   this.childAspectRatio = 1.0, // 子元素的宽高比例
   @required this.maxCrossAxisExtent, // 子元素的最大宽度可能是多少，然后计算得到合适的列宽
  // 示例：假如手机屏宽375，crossAxisSpacing值为0，
  // maxCrossAxisExtent值 = 125时，网格列数 = 3。因为125 * 3 = 375，刚好，每一列的宽度就是375/3。
  // maxCrossAxisExtent值 = 126时，网格列数 = 3。因为126 * 3 > 375，显示不下，每一列的宽度将是375/3。
  // maxCrossAxisExtent值为124时，网格列数 = 4。因为124 * 3 < 375，仍有多余，每一列的宽度将是375/4。
  })
```

#### d. 示例代码



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
    return GridView.extent(
      scrollDirection: Axis.vertical, // 排列方向
      controller: _controller, // 设置控制器
      padding: EdgeInsets.all(10), // 内边距
      physics: new AlwaysScrollableScrollPhysics(), // 设置到边缘后的效果
      maxCrossAxisExtent: 200, // 子元素宽度最大是200
      childAspectRatio: 0.5,//宽高比为1:2
      mainAxisSpacing: 10, // 主轴、次轴方向间隔
      crossAxisSpacing: 10,
      children: getWidgetList(), // 表格数据（仅作demo展示，代码在下方）
    );
  }

  // GridView数据（仅作demo展示）
  List<String> litems = [
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15"
  ];

  List<Widget> getWidgetList() {
    return litems.map((item) => getItemContainer(item)).toList();
  }

  Widget getItemContainer(String item) {
    return Container(
      alignment: Alignment.center,
      child: Text(
        item,
        style: TextStyle(color: Colors.white, fontSize: 20),
      ),
      color: Colors.blue,
    );
  }
}
```

#### e. 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgiafvjdj30f80ocdfz.jpg)

### 类型4：GridView.custom

#### a. 特点

可自定义SliverGridDelegate 和 SliverChildBuilderDelegate

#### b. 应用场景

更高程度的自定义监听 & 实现效果

#### c. 属性（构造函数）



```kotlin
 GridView.custom({
   Key key,
   Axis scrollDirection = Axis.vertical, // 列表的滚动方向，可选值：Axis的horizontal、vertical
   this.mainAxisSpacing = 0.0, // 主轴方向上的空隙间距；
   this.crossAxisSpacing = 0.0, // 次轴方向上的空隙间距；
   this.childAspectRatio = 1.0, // 子元素的宽高比例
   @required SliverGridDelegate gridDelegate, // 可传入自定义的SliverGridDelegate
  @required SliverChildDelegate childrenDelegate,  // 可传入自定义的SliverChildDelegate childrenDelegate
  })
```

#### d. 示例代码



```dart
/**
 *  导入库
 **/
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
    return GridView.custom(
      scrollDirection: Axis.vertical, // 排列方向
      controller: _controller, // 设置控制器
      padding: EdgeInsets.all(10), // 内边距
      physics: new AlwaysScrollableScrollPhysics(), // 设置到边缘后的效果

        gridDelegate:SliverGridDelegateWithFixedCrossAxisCount(
          crossAxisCount: 3,// 列表数量是3
          childAspectRatio: 0.5,//宽高比为1:2
          mainAxisSpacing: 10, // 主轴、次轴方向间隔
          crossAxisSpacing: 10,
        ),

        childrenDelegate: MyGridChildrenDelegate( // 更高程度的自定义
              (BuildContext context, int i) {
                return Container(
                  alignment: Alignment.center,
                  child: Text(
                    litems[i],
                    style: TextStyle(color: Colors.white, fontSize: 20),
                  ),
                  color: Colors.blue,
                );
          },
          childCount: litems.length,
        ),


//      children: getWidgetList(), // 表格数据（仅作demo展示，代码在下方）
    );
  }

  // GridView数据（仅作demo展示）
  List<String> litems = [
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15"
  ];

  List<Widget> getWidgetList() {
    return litems.map((item) => getItemContainer(item)).toList();
  }

  Widget getItemContainer(String item) {
    return Container(
      alignment: Alignment.center,
      child: Text(
        item,
        style: TextStyle(color: Colors.white, fontSize: 20),
      ),
      color: Colors.blue,
    );
  }
}

/**
 * 自定义有SliverChildBuilderDelegate
 * 可实现对列表的监听
 */
class MyGridChildrenDelegate extends SliverChildBuilderDelegate {
  MyGridChildrenDelegate(
      Widget Function(BuildContext, int) builder, {
        int childCount,
        bool addAutomaticKeepAlive = true,
        bool addRepaintBoundaries = true,
      }) : super(builder,
      childCount: childCount,
      addAutomaticKeepAlives: addAutomaticKeepAlive,
      addRepaintBoundaries: addRepaintBoundaries);

  //监听列表
  // 在可见的列表中 显示的第一个位置和最后一个位置
  @override
  void didFinishLayout(int firstIndex, int lastIndex) {
    print('firstIndex: $firstIndex, lastIndex: $lastIndex');
  }

  // 判断添加进来的实例与之前的实例是否相同
  // 相同返回true；反之false
  // 可不重写，默认是true
  @override
  bool shouldRebuild(SliverChildBuilderDelegate oldDelegate) {
    // TODO: implement shouldRebuild
    print("oldDelegate$oldDelegate");
    return super.shouldRebuild(oldDelegate);
  }
}
```

#### e. 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgi7a01lj30fa0pswes.jpg)

示意图

至此，关于`GridView`的使用讲解完毕。

------

# 4. 总结

本文全面介绍了`Flutter GridView`组件的使用，总结如下：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgi6ev1yj30xc0l8gsg.jpg)



接下来推出的文章，我将继续讲解 `Flutter` 的相关知识，包括使用语法、实战等，感兴趣的读者可以继续关注我的博客哦：[Carson_Ho的Android博客](https://www.jianshu.com/users/383970bef0a0/latest_articles)

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



作者：Carson_Ho
链接：https://www.jianshu.com/p/00dcc397d5e3
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。