[TOC]

今天，我主要讲解Flutter中布局方面的组件Widget，类型如下，希望你们会喜欢。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgqr9ra6j30s80qijt0.jpg)

示意图

------

# 1. 边距属性

主要包括：padding、margin、alignment

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgqpy5ynj30mo0g4abf.jpg)

### 1.1 padding属性

- 简介：内边距，即本Widget边框和内容区之间距离
- 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgqo2q3oj30j00gaglq.jpg)

- 使用：采用EdgeInsets类



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

### 1.2 margin属性

- 定义：外边距，即本Widget与父边框的距离
- 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgqm4gf2j30i40fwdg0.jpg)

- 使用：采用EdgeInsets类



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

### 1.3 alignment

- 简介：子Widget对齐，生效范围：父Widget尺寸 > child Widget尺寸
- 使用



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

------

# 2. 基础布局组件

主要包括：

- Container
- Row
- Column
- Expanded
- center

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgqjseizj30uo0mqq5d.jpg)

具体请看文章：[Android Flutter：那些不可忽视的基础布局！](https://www.jianshu.com/p/b19078a45488)

------

# 3. 列表布局组件

`Flutter`常用的滚动型列表组件包括：`GridView`组件 + `ListView`组件

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgqih0n0j30xc0l4gm2.jpg)

具体请看文章：

- [Flutter：手把手教你使用滚动型列表组件：ListView](https://www.jianshu.com/p/f48b65a111ad)
- [Flutter：手把手教你使用滚动型列表组件GirdView](https://www.jianshu.com/p/00dcc397d5e3)

------

# 4. 导航栏组件

主要包括：

- 顶部导航栏：TabBar + TabBarView + TabController
- 底部导航栏：BottomNavigationBar

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgqh2cmsj30xc0nnjtg.jpg)

具体请看文章：

- [Flutter 顶部导航栏实现：TabBar + TabBarView + TabController](https://www.jianshu.com/p/aa1e26bff092)
- [Android Flutter BottomNavigationBar：该如何优雅实现底部导航栏？](https://www.jianshu.com/p/81dcfcc9e5da)

------

# 5. 总结

- 本文全面介绍了`Flutter`的布局组件使用，包括边距常用属性、基础布局等
- 接下来推出的文章，我将继续讲解Flutter的相关知识，包括使用语法、实战等，感兴趣的读者可以继续关注我的博客哦：[Carson_Ho的Android博客](https://www.jianshu.com/users/383970bef0a0/latest_articles)



作者：Carson_Ho
链接：https://www.jianshu.com/p/e56696697cd0
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。