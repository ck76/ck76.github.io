[TOC]

# 1. Image

### 1.1 作用

显示图片，主要支持的加载方式：本地图片、资源图片 & 网络图片。

### 1.2 常用属性



```kotlin
const Image({
    Key key,
    @required this.image,// ImageProvider，必填参数，接收一个ImageProvider 类型的值
    this.semanticLabel, // String，图片的描述
    this.excludeFromSemantics = false, // bool，是否从语义上排除该图片，默认值为false
    this.width, // double，图片的宽度
    this.height, // double，图片的高度
    this.color, // Color，图片的前景色，一般不设置或设置透明色，会覆盖掉图片，一般会和colorBlendMode结合使用
    this.colorBlendMode, // BlendMode，一般和color结合使用，设置color的混合模式
    this.fit, // BoxFit，设置图片的显示模式
    this.alignment = Alignment.center, // AlignmentGeometry，用于设置图片的对齐方式，默认值：Alignment.center
    this.repeat = ImageRepeat.noRepeat, // ImageRepeat，图片的重复方式，当图片没有完全覆盖掉容器时使用。默认值：ImageRepeat.noRepeat
    ...
  })
```

下面，将详细讲解Image的属性。

### 1.3 属性image

- 接收一个ImageProvider类型的值。ImageProvider是一个抽象类
- 实现类主要包括：**AssetImage、MemoryImage、NetworkImage、FileImage**，分别表示可从资源、内存、网络 & 文件中获取图片



```kotlin
// 加载网络图片
Image.network(String src, {
    Key key,
    double scale = 1.0,
    this.semanticLabel,
    this.excludeFromSemantics = false,
    this.width,
    this.height,
    this.color,
    this.colorBlendMode,
    this.fit,
    this.alignment = Alignment.center,
    this.repeat = ImageRepeat.noRepeat,
    this.centerSlice,
    this.matchTextDirection = false,
    this.gaplessPlayback = false,
    this.filterQuality = FilterQuality.low,
    Map<String, String> headers,
  })

// 加载本地文件
  Image.file(File file, {
    Key key,
    double scale = 1.0,
    this.semanticLabel,
    this.excludeFromSemantics = false,
    this.width,
    this.height,
    this.color,
    this.colorBlendMode,
    this.fit,
    this.alignment = Alignment.center,
    this.repeat = ImageRepeat.noRepeat,
    this.centerSlice,
    this.matchTextDirection = false,
    this.gaplessPlayback = false,
    this.filterQuality = FilterQuality.low,
  })

// 从项目资源中加载
Image.asset(String name, {
    Key key,
    AssetBundle bundle,
    this.semanticLabel,
    this.excludeFromSemantics = false,
    double scale,
    this.width,
    this.height,
    this.color,
    this.colorBlendMode,
    this.fit,
    this.alignment = Alignment.center,
    this.repeat = ImageRepeat.noRepeat,
    this.centerSlice,
    this.matchTextDirection = false,
    this.gaplessPlayback = false,
    String package,
    this.filterQuality = FilterQuality.low,
  })

// 从内存中加载
 Image.memory(Uint8List bytes, {
    Key key,
    double scale = 1.0,
    this.semanticLabel,
    this.excludeFromSemantics = false,
    this.width,
    this.height,
    this.color,
    this.colorBlendMode,
    this.fit,
    this.alignment = Alignment.center,
    this.repeat = ImageRepeat.noRepeat,
    this.centerSlice,
    this.matchTextDirection = false,
    this.gaplessPlayback = false,
    this.filterQuality = FilterQuality.low,
  })
```

为了方便使用，在Image的构造方法里也加入了快速从各种不同途径获得图片的方式，Image的构造方法包括：



```cpp
Image()  // 通用方法，使用ImageProvider实现，如下方法本质上也是使用的这个方法
Image.asset // 加载资源图片
Image.file  // 加载本地图片文件
Image.network  // 加载网络图片
Image.memory   // 加载Uint8List资源图片
```



```csharp
// 获得资源图片
new Image.asset('imgs/logo.jpeg')

// 获得网络图片
new Image.network(
  'https://flutter.io/images/homepage/header-illustration.png')

// 获得本地文件图片
new Image.file(new File("/Users/gs/Downloads/1.jpeg"))

// 获得Uint8List图片
new Image.memory(bytes)

// 获得使用ImageProvider加载图片
new Image(image: new NetworkImage(
  "https://flutter.io/images/homepage/screenshot-2.png"),)
```

此处特别说明加载本地的资源图片

### 步骤1：创建一个文件夹存放图片

此处创建的是文件夹名 = assetImage，放了一个名为photo.jpg的图片

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgffg8ljj30xc0hj131.jpg)

### 步骤2：在pubspec.yaml中声明资源

注：声明时路径和前面的- 存在间隔

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgfez7wzj30xc0gr131.jpg)

### 步骤3：加载时填写正确资源路径



```csharp
// 获得资源图片
new Image.asset('assetImage/photo.jpg')
```

- 实际测试代码

*main.dart*



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
      body: new Image.asset('assetImage/photo.jpg')
    );
  }

}
```

- 测试效果

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgfc8sywj30bs0hs0wz.jpg)

### 1.4 属性width & height、fit

- 属性说明



```kotlin
// 代表Image显示区域的宽度和高度设置
this.width, // double，图片的宽度
this.height, // double，图片的高度

this.fit, // BoxFit，设置图片的显示模式，具体设置如下图

// 此处将两个属性一起说明是因为：
// 图片的容器Image Widget的宽高 & 图片本身的大小不一样（需区分开来）
// 所以结合图片的显示模式讲解
```

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgfacxr5j30u00uxn6j.jpg)

- 使用
  *main.dart*



```java
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
        body: new Image.asset(
          'assetImage/photo.jpg',
          width: 200.0,
          height: 100.0,
          fit: BoxFit.fill,
        ));
  }
}
```

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgf8f7uoj30fc0oewg6.jpg)

### 1.5 属性color、colorBlendMode

- 属性说明



```kotlin
this.color
// Color，图片的前景色，一般不设置或设置透明色，会覆盖掉图片，一般会和colorBlendMode结合使用

this.colorBlendMode
// BlendMode，一般和color结合使用，设置color的混合模式，具体设置属性如下：
```

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgf72qmyj30fs0kydhi.jpg)

- 使用
  *main.dart*



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
        body: new Image.asset(
          'assetImage/photo.jpg',
          width: 200.0,
          height: 100.0,
          fit: BoxFit.fill,
          color: Colors.red,
          colorBlendMode: BlendMode.colorDodge,
        ));
  }
}
```

- 测试效果

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgf561rsj30ae0i0jrs.jpg)

### 1.6 属性alignment

- 属性说明



```kotlin
this.alignment = Alignment.center
// AlignmentGeometry，用于设置图片的对齐方式，默认值：Alignment.center，可设置的属性如下：
```

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgf3qjfij30t20bm76x.jpg)

- 具体使用
  为了方便展示，此处加入一个布局容器Container方便展示。

*main.dart*



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
        // 主体
        body: Container(
          width: 500,
          height: 1000,
          color: Colors.red,
          child: Image.asset(
            'assetImage/photo.jpg',
            width: 50.0,
            height: 50.0,
            alignment: Alignment.bottomCenter,
            fit: BoxFit.contain,
          ),
        ));
  }
}
```

- 测试效果

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgf1z1ilj30f60metio.jpg)

### 1.7 属性Repeat

- 属性说明



```kotlin
this.repeat = ImageRepeat.noRepeat
// ImageRepeat，图片的重复方式，当图片没有完全覆盖掉容器时使用。默认值：ImageRepeat.noRepeat

// 枚举类型
repeat // 1. 在x轴和y轴重复
repeatX // 2. 在x轴重复
repeatY // 3. 在y轴重复
noRepeat // 4. 不重复
```

- 具体使用



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
        body: Container(
          width: 500,
          height: 1000,
          color: Colors.red,
          child: Image.asset(
            'assetImage/photo.jpg',
            width: 50.0,
            height: 50.0,
            alignment: Alignment.bottomCenter,
            fit: BoxFit.contain,
            repeat: ImageRepeat.noRepeat,
          ),
        ));
  }
}
```

- 测试效果

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgf060oij30b20fwtcp.jpg)

至此，关于Image Widget讲解完毕。

------

# 2. Icon

### 2.1 作用

显示图标图片

### 2.2 优势（相对于Image）

- 体积小：可以减小安装包大小。
- 矢量：矢量图标，放大不会影响其清晰度。
- 可应用文本样式：可以像文本一样改变字体图标的颜色、大小对齐等 & 可通过TextSpan和文本混用

### 2.3 构造方法



```kotlin
 const Icon(this.icon, {
    Key key, // 设置使用的图标
    this.size, // 图标大小
    this.color, // 颜色
    this.textDirection, // 渲染图标的文本方向
  })
```

### 2.4 具体使用



```java
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
        body: Icon(
          Icons.camera,
          size: 50,
          color: Colors.red,
          textDirection: TextDirection.ltr,
        ));
  }
}
```

### 2.5 测试效果

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgexn7xoj30f00m2mx9.jpg)

### 2.6 特别说明

Flutter默认包含了一套Material Design的字体图标，在pubspec.yaml文件中的配置如下



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgewef3xj30xc0flwpk.jpg)

------

# 3. ImageIcon

### 3.1 作用

自定义图形图标，来自于图片绘制。图标不可交互式。

### 3.2 构造函数



```kotlin
  const ImageIcon(this.image, {
    Key key, 
    this.size, // 图标大小
    this.color, // 颜色
  })
```

- 区别于Icon，ImageIcon是采用自定义ImageProvider来定义图标
- 自定义ImageProvider分别是上面讲解Image的：AssetImage、MemoryImage、NetworkImage、FileImage，即分别表示可从资源、内存、网络 & 文件中获取图片



```kotlin
// 加载网络图片
Image.network(String src, {
    Key key,
    double scale = 1.0,
    this.semanticLabel,
    this.excludeFromSemantics = false,
    this.width,
    this.height,
    this.color,
    this.colorBlendMode,
    this.fit,
    this.alignment = Alignment.center,
    this.repeat = ImageRepeat.noRepeat,
    this.centerSlice,
    this.matchTextDirection = false,
    this.gaplessPlayback = false,
    this.filterQuality = FilterQuality.low,
    Map<String, String> headers,
  })

// 加载本地文件
  Image.file(File file, {
    Key key,
    double scale = 1.0,
    this.semanticLabel,
    this.excludeFromSemantics = false,
    this.width,
    this.height,
    this.color,
    this.colorBlendMode,
    this.fit,
    this.alignment = Alignment.center,
    this.repeat = ImageRepeat.noRepeat,
    this.centerSlice,
    this.matchTextDirection = false,
    this.gaplessPlayback = false,
    this.filterQuality = FilterQuality.low,
  })

// 从项目资源中加载
Image.asset(String name, {
    Key key,
    AssetBundle bundle,
    this.semanticLabel,
    this.excludeFromSemantics = false,
    double scale,
    this.width,
    this.height,
    this.color,
    this.colorBlendMode,
    this.fit,
    this.alignment = Alignment.center,
    this.repeat = ImageRepeat.noRepeat,
    this.centerSlice,
    this.matchTextDirection = false,
    this.gaplessPlayback = false,
    String package,
    this.filterQuality = FilterQuality.low,
  })

// 从内存中加载
 Image.memory(Uint8List bytes, {
    Key key,
    double scale = 1.0,
    this.semanticLabel,
    this.excludeFromSemantics = false,
    this.width,
    this.height,
    this.color,
    this.colorBlendMode,
    this.fit,
    this.alignment = Alignment.center,
    this.repeat = ImageRepeat.noRepeat,
    this.centerSlice,
    this.matchTextDirection = false,
    this.gaplessPlayback = false,
    this.filterQuality = FilterQuality.low,
  })

// 为了方便使用，在Image的构造方法里也加入了快速从各种不同途径获得图片的方式，Image的构造方法包括：
Image()  // 通用方法，使用ImageProvider实现，如下方法本质上也是使用的这个方法
Image.asset // 加载资源图片
Image.file  // 加载本地图片文件
Image.network  // 加载网络图片
Image.memory   // 加载Uint8List资源图片
```

### 3.3 具体使用



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
        body: ImageIcon(AssetImage(
            'assetImage/plane.png'), // 注：此处的图片格式必须是.png哦！！
          size: 300,
          color: Colors.red,)
    );
  }
}
```

### 3.4 测试效果图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgeslr1lj30f00lymxd.jpg)

至此，关于图片方面的Widget讲解完毕。

------

# 4. 总结

- 本文主要讲解了Flutter中图片组件方面的Widget，包括Image、Icon、ImageIcon
- 接下来推出的文章，我将继续讲解Flutter的相关知识，包括更多的Widget用法、实例应用等，感兴趣的读者可以继续关注我的博客哦：[Carson_Ho的Android博客](https://www.jianshu.com/users/383970bef0a0/latest_articles)



作者：Carson_Ho
链接：https://www.jianshu.com/p/d984f396d7f9
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。