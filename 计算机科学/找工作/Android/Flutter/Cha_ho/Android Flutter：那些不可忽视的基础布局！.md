[TOC]

今天，我主要讲解`Flutter`中布局方面的基础布局组件，主要包括：

- Container
- Row
- Column
- Expanded
- Center

------

# 1. Container

### 1.1 定义

布局容器，属于组合widget，内部有绘制widget、定位widget、尺寸widget

### 1.2 布局原理

由于Container结合了许多其他Widget；而每个Widget都有自己的布局行为，因此Container的布局行为十分复杂，具体介绍如下：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgpyqzyrj30xc0kgqc9.jpg)

### 1.3 属性说明



```kotlin
Container({
    Key key, // 控制框架在widget重建时与哪些其他widget匹配
    this.alignment, // 子Widget对齐，生效范围：父Widget尺寸 > child Widget尺寸
    this.padding, // 内边距，即本Widget边框和内容区之间距离
    this.margin, // 外边距：本Widget与父边框的距离。
    Color color, // Container背景色
    Decoration decoration, // 绘制背景图案，注：container背景色和decoration不能同时设置
    this.foregroundDecoration, // 前景。设置了foregroundDecoration可能会遮盖child内容，一般半透明遮盖（蒙层）效果使用！
    double width, // container的宽度，设置为double.infinity可以强制在宽度上撑满，不设置，则根据child和父节点两者一起布局。
    double height, // container的高度，设置为double.infinity可以强制在高度上撑满。
    BoxConstraints constraints, // 添加到child上额外的约束条件，用于设置child的宽高范围值
    this.child, // 控件内容widget。
  })
```

# 关于Decoratiton的使用，具体请看文章：

### 1.4 具体使用



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
        body: new Container(
          alignment: Alignment.center,// 对齐
          padding: const EdgeInsets.all(5.0),// 内边距
          margin: const EdgeInsets.all(10.0),// 外边距
          color: Colors.grey,// 背景色
          // 装饰(无法和color一起设置)
//          decoration: new BoxDecoration(
//            border: new Border.all(width: 2.0, color: Colors.red),
//            borderRadius: new BorderRadius.all(new Radius.circular(20.0)),
//            image: new DecorationImage(
//              image: new AssetImage('assetImage/photo.jpg'),
//              centerSlice: new Rect.fromLTRB(270.0, 180.0, 1360.0, 730.0),
//            ),
//          ),
          // 添加到child上额外的约束条件，用于设置child的宽高范围值
          constraints: new BoxConstraints.expand(
            height: Theme
                .of(context)
                .textTheme
                .display1
                .fontSize * 1.1 + 200.0,
          ),

          // 设置子空间
          child: Text("carson ho Demo",),
        )
    );
  }
}
```

### 1.5 测试效果

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgpvz8r7j30f00m6jri.jpg)

------

# 2. Row

### 2.1 作用

水平展示多个子控件的控件，即将一系列控件排成一行显示

> 注：该控件无法滚动。若超过一行则会报错，应考虑使用ListView类。

### 2.2 概念解析：主轴 & 纵轴

对于线性布局：

- 若布局沿水平方向（如Row），那么主轴 = 水平方向、纵轴 = 垂直方向
- 若布局沿垂直方向（如Colum），那么主轴 = 垂直方向、纵轴 = 水平方向

### 2.3 属性说明



```swift
 // 属性说明
 Row({
    Key key,  // 全局key来唯一标识子widget
    MainAxisAlignment mainAxisAlignment = MainAxisAlignment.start,  // 子控件放置方式
    MainAxisSize mainAxisSize = MainAxisSize.max,  // 子控件应该如何沿着主轴放置
    CrossAxisAlignment crossAxisAlignment = CrossAxisAlignment.center, // 子控件对齐方式
    TextDirection textDirection, // 排列方式：从左到右，还是从右到左排序
    VerticalDirection verticalDirection = VerticalDirection.down,  // 垂直排序
    TextBaseline textBaseline, // 对齐文本的水平线
    List<Widget> children = const <Widget>[], // 子控件
  })

 // 属性详解
 // 1. mainAxisAlignment
 // 说明：子控件根据主轴的对齐方式，枚举对象（默认值为start）
 enum MainAxisAlignment {
  start,  // 内部的子组件将从主轴起始位置开始排列（正向排列）
   end, // 内部的子组件将从主轴末尾位置开始排列（反向排列）
  center, // 内部的子组件将从主轴中间位置开始排列（居中）
  spaceBetween, // 内部的首尾子组件靠近首尾两端，其余子组件居中排列且组件间的间距一样
  spaceAround,  // 内部的子组件居中排列，且每个子组件的左右边距一样大
  spaceEvenly,  // 内部的子组件居中显示，每个空间的左边和右边都有相同的间距
}

// 2. mainAxisSize
// 说明：子控件如何放置，枚举对象（默认值为max）
enum MainAxisSize {
  min, // 控件尽可能小，相当于wrap_content（取此值时，上面的MainAxisAlignment 无效），
  max, //  控件尽可能大，相当于match_parent
}

// 3. crossAxisAlignment
// 说明：子控件对于纵轴的对齐方式，当子控件高度不一样时，如何被放置在主轴（中心轴），而MainAxisAlignment 决定了子控件间的间隔
enum CrossAxisAlignment {
  start,  // 内部的子组件将从非主轴起始位置开始排列（正向排列）
  end, // 内部的子组件将从非主轴末尾位置开始排列（反向排列）
  center, // 内部的子组件将从非主轴中间位置开始排列（居中）
  stretch, // 内部的子组件将从非主轴中间位置开始排列，并且完全填充非主轴方向
  baseline, // 内部的子组件将从baseline的方向对齐，这个需要设置textBaseline属性，不然的话会报错
}

// 4. textBaseline
// 说明：对齐文本的水平线
enum TextBaseline {
  alphabetic, // 用于对齐字母字符的字形底部的水平线
  ideographic,  // 用于对齐表意文字的水平线
}

// 5. textDirection
// 说明：子控件排序方向 （一般不用设置，除非想反转子控件排序）
// Row 使用 TextDirection
// Column 使用 VerticalDirection
enum VerticalDirection {
  up,  //  子控件从下到上排序
  down, //  子控件从上到下排序
}
enum TextDirection {
  rtl, // 子控件从右到左排序
  ltr, // 子控件从左到右排序
}

// 6. children
// 设置子控件
```

### 2.4 具体使用



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
      body: new Row(
        mainAxisAlignment: MainAxisAlignment.start,
        mainAxisSize: MainAxisSize.min,
        crossAxisAlignment: CrossAxisAlignment.start,
        textDirection: TextDirection.ltr,
        children: <Widget>[
          Text(" Carson Ho "),
          Text(" Kobe Bryant "),
          Text(" LeBorn James  "),
          Text(" Michael Jordan  "),
        ],
      ),
    );
  }
}
```

### 2.5 效果图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgptjmt3j30f40m8mxg.jpg)

------

# 3. Column

### 3.1 作用

垂直方向展示多个子控件的控件，即将一系列控件排成一列显示

### 3.2 属性说明

属性类似于Row的属性，主要包括：



```swift
 // 属性说明
 Column({
    Key key,  // 全局key来唯一标识子widget
    MainAxisAlignment mainAxisAlignment = MainAxisAlignment.start,  // 子控件放置方式
    MainAxisSize mainAxisSize = MainAxisSize.max,  // 子控件应该如何沿着主轴放置
    CrossAxisAlignment crossAxisAlignment = CrossAxisAlignment.center, // 子控件对齐方式
    TextDirection textDirection, // 排列方式：从左到右，还是从右到左排序
    VerticalDirection verticalDirection = VerticalDirection.down,  // 垂直排序
    TextBaseline textBaseline, // 对齐文本的水平线
    List<Widget> children = const <Widget>[], // 子控件
  })

// 属性详解
 // 1. mainAxisAlignment
 // 说明：子控件根据主轴的对齐方式，枚举对象（默认值为start）
 enum MainAxisAlignment {
  start,  // 内部的子组件将从主轴起始位置开始排列（正向排列）
   end, // 内部的子组件将从主轴末尾位置开始排列（反向排列）
  center, // 内部的子组件将从主轴中间位置开始排列（居中）
  spaceBetween, // 内部的首尾子组件靠近首尾两端，其余子组件居中排列且组件间的间距一样
  spaceAround,  // 内部的子组件居中排列，且每个子组件的左右边距一样大
  spaceEvenly,  // 内部的子组件居中显示，每个空间的左边和右边都有相同的间距
}

// 2. mainAxisSize
// 说明：子控件如何放置，枚举对象（默认值为max）
enum MainAxisSize {
  min, // 控件尽可能小，相当于wrap_content（取此值时，上面的MainAxisAlignment 无效），
  max, //  控件尽可能大，相当于match_parent
}

// 3. crossAxisAlignment
// 说明：子控件对于纵轴的对齐方式，当子控件高度不一样时，如何被放置在主轴（中心轴），而MainAxisAlignment 决定了子控件间的间隔
enum CrossAxisAlignment {
  start,  // 内部的子组件将从非主轴起始位置开始排列（正向排列）
  end, // 内部的子组件将从非主轴末尾位置开始排列（反向排列）
  center, // 内部的子组件将从非主轴中间位置开始排列（居中）
  stretch, // 内部的子组件将从非主轴中间位置开始排列，并且完全填充非主轴方向
  baseline, // 内部的子组件将从baseline的方向对齐，这个需要设置textBaseline属性，不然的话会报错
}

// 4. textBaseline
// 说明：对齐文本的水平线
enum TextBaseline {
  alphabetic, // 用于对齐字母字符的字形底部的水平线
  ideographic,  // 用于对齐表意文字的水平线
}

// 5. textDirection
// 说明：子控件排序方向 （一般不用设置，除非想反转子控件排序）
// Row 使用 TextDirection
// Column 使用 VerticalDirection
enum VerticalDirection {
  up,  //  子控件从下到上排序
  down, //  子控件从上到下排序
}
enum TextDirection {
  rtl, // 子控件从右到左排序
  ltr, // 子控件从左到右排序
}

// 6. children
// 设置子控件
```

### 3.3 具体使用



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
      body: new Column(
        mainAxisAlignment: MainAxisAlignment.start,
        mainAxisSize: MainAxisSize.min,
        crossAxisAlignment: CrossAxisAlignment.start,
        verticalDirection: VerticalDirection.down,
        children: <Widget>[
          Text(" Carson Ho "),
          Text(" Kobe Bryant "),
          Text(" LeBorn James  "),
          Text(" Michael Jordan  "),
        ],
      ),
    );
  }
}
```

### 3.4 具体效果

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgpr8d4nj30f80m8dg5.jpg)

------

# 4. Expanded

### 4.1 作用

按比例伸缩 / 扩展 Row、Column和Flex子组件所占用的空间大小

### 4.2 属性说明



```dart
const Expanded({
    Key key, // 唯一标识符
    int flex = 1, // 弹性系数，默认值 = 1
                 // 若为 0 或 null，则 child 是没有弹性的，即不会被扩伸占用的空间。
                 // 若大于 0，所有的Expanded按照其flex的比例来分割主轴的全部空闲空间。
    @required Widget child, // 子控件
}) 
```

### 4.3 具体使用



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
      //主体：配合Row使用（采用两个容器Container来展示）
      body: new Row(
        children: <Widget>[
          Expanded(
              flex: 2, // 占2份空间
              child: Container(
                color: Colors.red,
              )),
          Expanded(
              flex: 1, // 占1份空间
              child: Container(
                color: Colors.green,
              )),
        ],
      ),
    );
  }
}
```

### 4.4 测试效果

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgpp1wgkj30f60ma0ss.jpg)

------

# 5. Center

中心定位控件，能够将子控件放在其内部中心



```java
class Page extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Center(
      child: Text("正中央"), // 将text放到文本中央
    );
  }
}
```

------

# 总结

本文全面介绍了`Flutter`常用的基础布局组件，总结如下：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgpn6jkbj30uo0mqq5d.jpg)

接下来推出的文章，我将继续讲解Flutter的相关知识，包括使用语法、实战等，感兴趣的读者可以继续关注我的博客哦：[Carson_Ho的Android博客](https://www.jianshu.com/users/383970bef0a0/latest_articles)



作者：Carson_Ho
链接：https://www.jianshu.com/p/b19078a45488
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。