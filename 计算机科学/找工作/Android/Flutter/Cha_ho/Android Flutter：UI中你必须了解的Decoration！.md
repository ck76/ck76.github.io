[TOC]

# 前言

- `Decoration`是一个背景装饰对象，相当于Android中的shape.xml
- 今天，carson将全面介绍`Decoration`的使用，包括其作用、定义 & 使用，希望你们会喜欢。



示意图

------

# 目录

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwglkgcuxj30q40jq74q.jpg)

------

# 1. 定义

一个背景装饰对象，相当于`Android`中的`shape.xml`

------

# 2. 作用

定制各种各样的背景（边框、圆角、阴影、形状、渐变、背景图像）

------

# 3. 类型

`Flutter Decoration`的类型主要有4种：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgliqfzqj30xc0dftan.jpg)

### 类型1：BoxDecoration

##### a. 特点

实现边框、圆角、阴影、形状、渐变、背景图像

##### b. 属性说明



```kotlin
 const BoxDecoration({
    this.color, // 底色
    this.image, // 图片
    this.border, // 边色
    this.borderRadius, // 圆角度
    this.boxShadow, // 阴影
    this.gradient, // 渐变
    this.backgroundBlendMode, // 混合Mode
    this.shape = BoxShape.rectangle,  // 形状
  })
```

##### c. 使用示例



```csharp
decoration: new BoxDecoration(
  border: new Border.all(color: Color(0xFFFF0000), width: 0.5), // 边色与边宽度
  color: Color(0xFF9E9E9E), // 底色
  borderRadius: new BorderRadius.circular((20.0)), // 圆角度
  shape: BoxShape.rectangle, // 默认值是矩形
  // shape: BoxShape.circle, // 圆形，使用圆形时不可以使用borderRadius

  // 阴影：此处采用两层阴影说明
  boxShadow: [BoxShadow(color: Color(0x99FFFF00), 
    offset: Offset(5.0, 5.0),  // 阴影位置由offset决定
    blurRadius: 10.0, // 阴影模糊层度由blurRadius大小决定（大就更透明更扩散）
    spreadRadius: 2.0), // 阴影模糊大小由spreadRadius决定
    BoxShadow(color: Color(0x9900FF00), offset: Offset(1.0, 1.0)), BoxShadow(color: Color(0xFF0000FF))],

  // 渐变
    // 类型1：环形
      gradient: RadialGradient(colors: [Color(0xFFFFFF00), Color(0xFF00FF00), Color(0xFF00FFFF)],radius: 1, tileMode: TileMode.mirror)
    // 类型2：扫描式
    // gradient: SweepGradient(colors: [Color(0xFFFFFF00), Color(0xFF00FF00), Color(0xFF00FFFF)], startAngle: 0.0, endAngle: 1*3.14)
    // 类型3：线性
    // gradient: LinearGradient(colors: [Color(0xFFFFFF00), Color(0xFF00FF00), Color(0xFF00FFFF)], begin: FractionalOffset(1, 0), end: FractionalOffset(0, 1))
  
  // 背景图像
  image: new DecorationImage(
        image: new AssetImage('graphics/background.png'), // 加载本地图片，还有其他加载方式，如网络、文件等
        centerSlice: new Rect.fromLTRB(10.0, 20.0, 30.0, 40.0),// 设置图片大小
        fit: BoxFit.fill // 设置填充方式
  ),
),
```

### 类型2：ShapeDecoration

##### a. 特点

实现四个边，分别指定颜色、宽度、底部线、矩形边色、圆形边色等

##### b. 属性说明



```kotlin
const ShapeDecoration({
  this.color, // 底色
  this.image, // 图片
  this.gradient, // 渐变
  this.shadows, // 阴影
  @required this.shape, // 形状
})
```

此处的属性除了shape，其余与BoxDecoration相同，所以此处主要讲解shape属性

##### c. 具体使用



```cpp
decoration: new ShapeDecoration(
  // 统一四边颜色和宽度
  shape: Border.all(color: Color(0xFF00FFFF),style: BorderStyle.solid,width: 2)

  // 四个边分别指定颜色 & 宽度，当只给bottom时与UnderlineInputBorder一致效果
  // shape: Border(top: b, bottom: b, right: b, left: b)

  // 设置矩形边色
  // shape: RoundedRectangleBorder(borderRadius: BorderRadius.all(Radius.circular(10)), side: BorderSide(color: Color(0xFFFFFFFF), style: BorderStyle.solid, width: 2))
  
  // 设置圆形边色
  // shape: CircleBorder(side: BorderSide(color: Color(0xFFFFFF00), style: BorderStyle.solid, width: 2))
  
  // 设置竖向椭圆边色（类似体育场）
  // shape: StadiumBorder(side: BorderSide(width: 2, style: BorderStyle.solid, color: Color(0xFF00FFFF))
  
  // 设置角形（八边角）边色
  // shape: BeveledRectangleBorder(borderRadius: BorderRadius.all(Radius.circular(10)), side: BorderSide(color: Color(0xFFFFFFFF), style: BorderStyle.solid, width: 2))
),
```

------

# 类型3：UnderlineTabindicator

##### a. 特点

下划线，可控制下划高底 & 左右边距

##### b. 属性说明



```kotlin
const UnderlineTabIndicator({
  this.borderSide = const BorderSide(width: 2.0, color: Colors.white), // 控制线的长度 & 颜色
  this.insets = EdgeInsets.zero, // 控制下划高底，左右边距
})
```

##### c. 具体使用



```css
decoration: new UnderlineTabIndicator(
  borderSide: BorderSide(width: 2.0, color: Colors.white), 
  insets: EdgeInsets.fromLTRB(0,0,0,10)
),
```

------

# 类型4：FlutterLogoDecoration

##### a. 特点

实现Flutter logo 图片

##### b. 属性构造



```kotlin
const FlutterLogoDecoration({
  this.lightColor = const Color(0xFF42A5F5), // Colors.blue[400]
  this.darkColor = const Color(0xFF0D47A1), // Colors.blue[900]
  this.textColor = const Color(0xFF616161),
  this.style = FlutterLogoStyle.markOnly,
  this.margin = EdgeInsets.zero,
})
```

在日常开发中，基本不会用到，所以此处不作过多讲解。

------

# 4. 总结

- 本文全面介绍了`Flutter Decoration`的使用
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
链接：https://www.jianshu.com/p/0e3d594cee18
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。