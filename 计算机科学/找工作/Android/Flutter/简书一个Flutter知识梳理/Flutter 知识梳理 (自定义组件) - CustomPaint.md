[TOC]

# 一、背景知识

在`Android`中，当我们需要自定义`View`的时候，需要重写`View.draw(Canvas canvas)`方法，并通过`Painter`结合`Canvas`的`API`实现绘制的逻辑，`Flutter`的核心实现跟这个很类似。

在`Flutter`自定义组件中，有两个重要的概念：

- `CustomPaint`：它是`SingleChildRenderObjectWidget`的子类，我们将它放在`Widget`树的节点中，而`CustomPainter`是它的一个属性，负责实现具体的绘制逻辑。



```dart
class SimplePainterWidget extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return Center(
      child: CustomPaint(
        painter: SimplePainter(),
      ),
    );
  }
}
```

- `CustomPainter`：通过继承该类，并重`void paint(Canvas canvas, Size size)`方法，使用`Painter`和`Canvas`提供的`API`，完成绘制的过程。

下面，我们分别介绍一下这两个概念。

## 1.1 CustomPaint

首先，`CustomPaint`的定义如下：



```dart
const CustomPaint({
  Key key,
  this.painter, 
  this.foregroundPainter,
  this.size = Size.zero, 
  this.isComplex = false, 
  this.willChange = false, 
  Widget child, 
})
```

- `painter`：画笔，显示在子节点后面。
- `foregroundPainter`：前景画笔，显示在子节点前面。
- `size`：当`child`为`null`，代表默认大小，如果有`child`则为`child`大小。如果希望在有`child`的时候限制大小，那么可以用`SizeBox`包裹`CustomPaint`。
- `isComplex`：是否复杂绘制，如果是，会应用一些缓存策略来减少重复渲染的次数。
- `willChange`：和`isComplex`配合使用，当启用缓存时，该属性代表在下一帧中绘制是否会改变。

在有子节点时，为了避免子节点不必要的重绘并提高性能，通常会将子节点包裹在`RepaintBoundary Widget`中。

这样会在绘制时创建一个新的绘制层，其子`Widget`将在新的`Layer`绘制，父`Widget`将会在原来的`Layer`上绘制。



```dart
CustomPaint(
  size: Size(300, 300),
  painter: MyPainter(),
  child: RepaintBoundary(child:...))
)
```

## 1.2 CustomPainter

通过实现`CustomPainter`的`void paint(Canvas canvas, Size size)`方法，并结合`Paint`的属性，完成绘制操作，如果之前有过`Android`中自定义`View`的经验，那么上手很快。

`CustomPainter`涉及到的知识点有以下四个方面：

- `Paint`提供的属性：圆角、颜色、线宽等等。
- `Canvas.drawXXX`方法：点、弧形、圆形、正方形、长方形、路径等等。
- `Canvas`的变换：`scale`、`translate`、`rotate`、`skew`。
- `Canvas`的`save`和`restore`方法运用：`save`方法作用是保存画布当前状态，`restore`则是取出，例如要对画布进行多个动作处理，第一个动作进行了缩放，如果没有在缩放动作处理前保存一下，那么在执行第二个动作时也会有缩放动作的影响。

## 1.3 一些绘制 API 的参考资料

我做练习的时候分为了以下两步，大家可以参考：

- 先过一遍`API`，对于支持哪些方法有一个基本的认识。
- 在`Github`上，找一个`Android`中自定义`View`的案例，通过`Flutter`的`API`去实现一下，由于`Android`和`Flutter`的实现很相似，因此有想不明白的地方也可以去参考。

下面是一些参考的资料：

- [Flutter 34: 图解自定义 View 之 Canvas (一)](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.cnblogs.com%2FFree-Thinker%2Fp%2F10824971.html)
- [Flutter 35: 图解自定义 View 之 Canvas (二)](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.cnblogs.com%2FFree-Thinker%2Fp%2F10824987.html)
- [Flutter 36: 图解自定义 View 之 Canvas (三)](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.cnblogs.com%2FFree-Thinker%2Fp%2F10824992.html)
- [Flutter自定义绘制（1）- 绘制基础](https://links.jianshu.com/go?to=https%3A%2F%2Fjuejin.im%2Fpost%2F5c67a6a0f265da2dae510fa2)
- [Canvas](https://links.jianshu.com/go?to=https%3A%2F%2Fapi.flutter.dev%2Fflutter%2Fdart-ui%2FCanvas-class.html)
- [Paint](https://links.jianshu.com/go?to=https%3A%2F%2Fapi.flutter.dev%2Fflutter%2Fdart-ui%2FPaint-class.html)

# 二、示例

以下是三个例子：

- 简单示例：`CustomPaint`和`CustomPainter`的基本结构。
- 棋盘示例：掌握基本的`API`。
- 仪表盘：`Canvas`变换、`save/restore`、弧形、绘制文字，结合动画。

## 2.1 简单示例

一个最简单的`CustomPainter`如下所示：



```dart
import 'package:flutter/material.dart';

void main() => runApp(CustomPainterWidget());

class CustomPainterWidget extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: Scaffold(
        appBar: AppBar(title: Text("SimplePainter")),
        body: SimplePainterWidget(),
      ),
    );
  }
}

class SimplePainterWidget extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return Center(
      child: CustomPaint(
        painter: SimplePainter(),
      ),
    );
  }
}

class SimplePainter extends CustomPainter {

  Paint painter = Paint()..color = Colors.blue
    ..style = PaintingStyle.fill
    ..strokeCap = StrokeCap.butt
    ..isAntiAlias = true;

  @override
  void paint(Canvas canvas, Size size) {
    canvas.drawCircle(Offset(0, 0), 40, painter);
  }

  @override
  bool shouldRepaint(CustomPainter oldDelegate) => true;
}
```

效果图：

![img](https://upload-images.jianshu.io/upload_images/1949836-4a97ee743755fc67.png?imageMogr2/auto-orient/strip|imageView2/2/w/399)

image.png

## 2.2 棋盘示例



```dart
import 'package:flutter/material.dart';
import 'dart:math';

class GoCustomPainterWidget extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return Center(
      child: CustomPaint(
        painter: GoCustomPainter(),
        size: Size(300, 300),
      ),
    );
  }
}

class GoCustomPainter extends CustomPainter {

  static const GRID_NUM = 12;

  @override
  void paint(Canvas canvas, Size size) {
    var goWidth = min(size.width, size.height);
    //1.绘制背景。
    Paint paint = new Paint()
      ..color = Colors.yellow
      ..style = PaintingStyle.fill;
    canvas.drawRect(Rect.fromLTWH(0, 0, goWidth, goWidth), paint);
    paint.color = Colors.black;
    for (int i = 0; i <= GRID_NUM; i++) {
      var index = goWidth / GRID_NUM * i;
      //绘制横线。
      canvas.drawLine(Offset(0, index), Offset(goWidth, index), paint);
      //绘制竖线。
      canvas.drawLine(Offset(index, 0), Offset(index, goWidth), paint);
    }
    for (int i = 0; i < 8; i++) {
      _drawDots(canvas, paint..color = Colors.white, goWidth);
      _drawDots(canvas, paint..color = Colors.black, goWidth);
    }
  }

  _drawDots(Canvas canvas, Paint paint, var goWidth) {
    Random random = new Random();
    var unit = goWidth / GRID_NUM;
    var whiteX = unit * (1 + random.nextInt(GRID_NUM - 2));
    var whiteY = unit * (1 + random.nextInt(GRID_NUM - 2));
    canvas.drawCircle(Offset(whiteX, whiteY), 5, paint);
  }

  @override
  bool shouldRepaint(CustomPainter oldDelegate) => true;
}
```

在主工程中引用：



```dart
import 'package:flutter/material.dart';
import 'go_custom_painter.dart';

void main() => runApp(CustomPainterWidget());

class CustomPainterWidget extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: Scaffold(
        appBar: AppBar(title: Text("SimplePainter")),
        body: GoCustomPainterWidget(),
      ),
    );
  }
}
```

效果图：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymw412opj307c0eoweh.jpg)

image.png

## 2.3 仪表盘



```dart
import 'package:flutter/material.dart';
import 'dart:math';
import 'dart:ui';

class CanvasAnimateWidget extends StatefulWidget {

  @override
  State<StatefulWidget> createState() {
    return _CanvasAnimateWidgetState();
  }
}

class _CanvasAnimateWidgetState extends State<CanvasAnimateWidget> with SingleTickerProviderStateMixin {

  static const MAX_VALUE = 750.0;
  static const VALUE = 500.0;

  AnimationController controller;
  Animation<double> animation;
  var value = VALUE;

  @override
  void initState() {
    super.initState();
    controller = AnimationController(duration : Duration(seconds: 1), vsync: this);
    animation = Tween(begin: 0.0, end : VALUE).animate(controller)
      ..addListener(() { setState(() {});});
    controller.forward();
  }

  @override
  Widget build(BuildContext context) {
    return Center(
      child: CustomPaint(
        painter: DashBoardPainter(value: animation.value, maxValue: MAX_VALUE),
        size: Size(300, 300),
      ),
    );
  }

}

class DashBoardPainter extends CustomPainter {

  static const int GRID_NUM = 24;

  var maxValue;
  var value;

  DashBoardPainter({this.maxValue, this.value});

  @override
  void paint(Canvas canvas, Size size) {
    Paint paint = new Paint();
    //1.绘制背景。
    _drawBg(canvas, paint, size);
    //2.绘制圆弧。
    _drawArc(canvas, paint, size);
  }

  _drawBg(Canvas canvas, Paint paint, Size size) {
    paint..color = Colors.blue
      ..style = PaintingStyle.fill;
    canvas.drawRect(Rect.fromLTWH(0, 0, size.width, size.height), paint);
  }

  _drawArc(Canvas canvas, Paint paint, Size size) {
    var padding = 10.0;
    var width = size.width - 2*padding;
    var height = size.height - padding;
    canvas.save();
    canvas.translate(padding, padding);

    //1.绘制灰色的外环。
    paint..color = Colors.white10
      ..strokeWidth = 2.0
      ..style = PaintingStyle.stroke;
    canvas.drawArc(Rect.fromCircle(center: Offset(width/2, height/2), radius: min(height, width)/2), pi, pi, false, paint);

    //2.根据比例绘制白色的外环。
    paint..color = Colors.white;
    var faction = value / maxValue;
    canvas.drawArc(Rect.fromCircle(center: Offset(width/2, height/2), radius: min(height, width)/2), pi, pi * faction, false, paint);

    //3.绘制刻度的环。
    var arcX = 10.0;
    var arcWidth = 10.0;
    paint..strokeWidth = arcWidth..color = Colors.white10;
    canvas.drawArc(Rect.fromCircle(center: Offset(width/2, height/2), radius: width/2 - arcX - arcWidth/2), pi, pi, false, paint);

    //4.绘制刻度的横线，已经跨过的部分是白色，否则为浅色。
    paint.strokeWidth = 2.0;
    var threadHold = (value / (maxValue / GRID_NUM));
    for (var i = 0; i <= GRID_NUM; i++) {
      canvas.save();
      paint.color = i <= threadHold ? Colors.white : Colors.white24;
      canvas.translate(width/2, height/2);
      canvas.rotate(pi*i/GRID_NUM);
      canvas.translate(-width/2, -height/2);
      canvas.drawLine(Offset(arcX, height/2), Offset(arcX+arcWidth, height/2), paint);
      canvas.restore();
    }

    //5.绘制文字。
    TextSpan textSpan = TextSpan(
      style: TextStyle(
        color: Colors.white,
        fontSize: 50
      ),
      text: '${(value as double).toStringAsFixed(0)}'
    );
    TextPainter textPainter = TextPainter(
      text: textSpan,
      textDirection: TextDirection.ltr
    );
    textPainter.layout();
    textPainter.paint(canvas, Offset(width/2 - textPainter.width/2, height/3));
    canvas.restore();
  }


  @override
  bool shouldRepaint(DashBoardPainter oldDelegate) =>
      (maxValue != oldDelegate.maxValue || value != oldDelegate.value);


}
```

实现文件。



```dart
import 'package:flutter/material.dart';
import 'dash_board_painter.dart';

void main() => runApp(CustomPainterWidget());

class CustomPainterWidget extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: Scaffold(
        appBar: AppBar(title: Text("SimplePainter")),
        body: CanvasAnimateWidget(),
      ),
    );
  }
}
```

效果图：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymw1opygj307a0elwek.jpg)

image.png

# 参考文章

- [Canvas](https://links.jianshu.com/go?to=https%3A%2F%2Fapi.flutter.dev%2Fflutter%2Fdart-ui%2FCanvas-class.html)
- [Paint](https://links.jianshu.com/go?to=https%3A%2F%2Fapi.flutter.dev%2Fflutter%2Fdart-ui%2FPaint-class.html)
- [Flutter自定义绘制（1）- 绘制基础](https://links.jianshu.com/go?to=https%3A%2F%2Fjuejin.im%2Fpost%2F5c67a6a0f265da2dae510fa2)
- [Flutter 34: 图解自定义 View 之 Canvas (一)](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.cnblogs.com%2FFree-Thinker%2Fp%2F10824971.html)
- [Flutter 35: 图解自定义 View 之 Canvas (二)](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.cnblogs.com%2FFree-Thinker%2Fp%2F10824987.html)
- [Flutter 36: 图解自定义 View 之 Canvas (三)](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.cnblogs.com%2FFree-Thinker%2Fp%2F10824992.html)
- [CustomPaint 与 Canvas](https://links.jianshu.com/go?to=https%3A%2F%2Fbook.flutterchina.club%2Fchapter13%2Fcustom_paint.html)
- [实例：圆形渐变进度条(自绘)](https://links.jianshu.com/go?to=https%3A%2F%2Fbook.flutterchina.club%2Fchapter13%2Fgradient_circular_progress_demo.html)



6人点赞



[Flutter]()





作者：泽毛
链接：https://www.jianshu.com/p/b9e57313cbc3
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。