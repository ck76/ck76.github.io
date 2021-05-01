[TOC]

今天，我主要讲解`Flutter`中布局方面的边距属性：`padding`、`margin`、`alignment`，希望你们会喜欢。

------

# 1. padding属性

### 1.1 简介

内边距，即本Widget边框和内容区之间距离

### 1.2 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgmxlq5gj30j00gaglq.jpg)

### 1.3 具体使用

采用EdgeInsets类



```csharp
// 1. 所有方向均使用相同数值的填充。
all(double value) 
// 示例：4个方向各添加16像素补白
padding: EdgeInsets.all(16.0)

// 2. 分别指定四个方向的不同填充
fromLTRB(double left, double top, double right, double bottom)
// 示例：
padding: const EdgeInsets.fromLTRB(10,20,30,40)

// 3. 设置具体某个方向的填充(可以同时指定多个方向)
only({left, top, right ,bottom })
// 示例：在左边添加8像素补白
padding: const EdgeInsets.only(left: 8.0),

// 4. 设置对称方向的填充
// vertical：针对垂直方向top、bottom
// horizontal：针对横向方向left、right
symmetric({ vertical, horizontal })
// 示例：垂直方向上下各添加8像素补白
padding: const EdgeInsets.symmetric(vertical: 8.0)
```

------

# 2. margin属性

### 2.1 简介

外边距，即本Widget与父边框的距离

### 2.2 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgmvyyhqj30i40fwdg0.jpg)

### 2.3 简介

采用EdgeInsets类



```csharp
// 1. 所有方向均使用相同数值的填充。
all(double value) 
// 示例：4个方向各添加16像素补白
margin: EdgeInsets.all(16.0)

// 2. 分别指定四个方向的不同填充
fromLTRB(double left, double top, double right, double bottom)
// 示例：
margin：const EdgeInsets.fromLTRB(10,20,30,40)

// 3. 设置具体某个方向的填充(可以同时指定多个方向)
only({left, top, right ,bottom })
// 示例：在左边添加8像素补白
margin：const EdgeInsets.only(left: 8.0),

// 4. 设置对称方向的填充
// vertical：针对垂直方向top、bottom
// horizontal：针对横向方向left、right
symmetric({ vertical, horizontal })
// 示例：垂直方向上下各添加8像素补白
margin：const EdgeInsets.symmetric(vertical: 8.0)
```

------

# 3. alignment

### 3.1 简介

子Widget对齐，生效范围：父Widget尺寸 > child Widget尺寸

### 3.2 具体使用



```cpp
// 居中 & 各方向对齐
center
centerLeft
centerRight

// 底部对齐 & 各方向对齐
bottomCenter
bottomLeft
bottomRight

// 顶部对齐 & 各方向对齐
topCenter
topLeft
topRight

// 示例
alignment：Alignment.center
```

# 总结

- 本文全面介绍了`Flutter`常用的边距属性：`padding`、`margin`、`alignment`

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgmu1h9rj30mo0g4abf.jpg)

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
链接：https://www.jianshu.com/p/eebf7f7b1cfd
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。