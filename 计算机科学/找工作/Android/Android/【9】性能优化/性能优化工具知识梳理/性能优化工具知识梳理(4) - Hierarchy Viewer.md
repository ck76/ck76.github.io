[TOC]

# 一、概述

`Hierarchy Viewer`是我们平时开发中常用的工具，通过它我们可以得到某个界面中的布局层次，今天我们来介绍一下如何使用这个工具来优化布局。

`Hierarchy`工具位于`SDK/tools`目录下，直
接运行之后我们会得到下面这个界面：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn7zf6gxj30xc0c1adu.jpg)


它提供了下面两个功能：



- `Load View Hierarchy`
- `Inspect Screenshot`

因为主要是讨论如何用它来分析性能问题，所以我们主要介绍第一个功能。

# 二、`Hierarchy`分析

## 2.1 整体布局分析

在`Hierarchy Viewer`的根界面，它列出了当前手机或者模拟器中可见的`Window`，假如我们要分析某个`Activity`，那么点击它，再点击`Load View Hierarchy`后，就会得到下面这个界面：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn7xyptdj30xc0qfdjz.jpg)


这个界面一共分为四个部分，它们分别是：



- ```
  Tree View
  ```

  这里面将

  ```
  Activity
  ```

  的所有

  ```
  View
  ```

  呈现成为一个树形结构，它提供了以下几种操作方式：

  - 通过拖动条来放大或缩小可视范围。
  - 通过输入类名或者`View`的`id`来迅速定位到需要分析的`View`
  - 通过上方的`Save as PNG`或者`Capture Layers`保存当前的树形结构。

- `Tree Overview`
  这相当于是`Tree View`的缩小版，可以通过它快速查看或者定位。

- `Properties View`
  当我们选取了一个`View`的时候，它会列出对应`View`的属性。

- `Layout View`
  当我们选取了一个`View`之后，会在这个区域内，显示出不同颜色的矩形框，其中红色就是我们选取的`View`的范围，而淡红色则是该`View`对应的父容器的范围，其它的白色矩形框既不是该`View`的父容器，也不是该`View`的子控件。

需要注意，当`Window`的布局变化时，是不会主动刷新的，如果我们想要查看最新的布局情况，那么就要点击上方的`Load View Hierarchy`来刷新，而如果我们想要去查看别的`Window`布局，那么就需要点击下方操作栏最左边的按钮，重新进入选择`Window`的窗口。

## 2.2 单个布局元素分析

对于`Tree View`中的每个节点，我们可以获得以下信息：

- `View`的类名
- `View`所存储的地址
- `View`的`id`属性值
- `View`绘制的**相对耗时分析**，它会根据`measure/layout/draw`这三个阶段，分为三个小点展示，而小点的颜色就代表了**它在这个阶段相对于`View`树中的其他`View`在同一阶段的耗时对比结果**：
- 绿色：在前`50%`
- 黄色：在后`50%`
- 红色：耗时最长
- `View`处于它的父容器的`index`。

如果我们选择了一个`View`，那么会有更加详细的信息：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn7vcdzjj30na0t2dm4.jpg)



- `View`和它的子`View`在当前在屏幕上的结果。
- `View`的子`View`个数。
- `View`各绘制阶段耗时，单位为`ms`，包括了它自己和它的子`View`的耗时。

## 2.3 如何调试

上面我们看到`Hierarchy View`可以得到绘制阶段的耗时，这些耗时是在上一次绘制的时候计算出的，我们也可以通过这个工具主动地触发重绘，以得到最新的结果，操作步骤为：

- 选取一个`View`
- 点击窗口上方的`invalidate`把这个`View`标记为需要重绘，那么在下次请求绘制的时候就会重绘它。
- 点击窗口上方的`request layout`，来发起一次请求，这样它和它的子`View`以及需要重绘的`View`都会重新执行三个过程。

# 2.4 如何优化

`Hierarchy View`对于我们平时开发中的作用主要是以下几点：

- 查看整个布局的深度，是否可以去掉一些不必要的层级，以尽量使`View`树保持扁平。
- 查看`measure、layout、draw`当中那些红色或者黄色的节点，但是由于这些节点的颜色是包含了它和它的子`View`的时间，因此我们分析的时候，需要根据具体情况看究竟是由于`ViewGroup`的子`View`过多，还是`View`过于复杂引起的这一问题。

# 三、小结

今天主要介绍了`Hierarchy Viewer`和分析性能有关的用法，如果对于`Inspect Screenshot`有兴趣的同学可以查看下面官方文档的其它部分：

> [`http://android.xsoftlab.net/tools/debugging/debugging-ui.html`](https://link.jianshu.com/?t=http://android.xsoftlab.net/tools/debugging/debugging-ui.html)

------

## 更多文章，欢迎访问我的 **Android** 知识梳理系列：

- **Android** 知识梳理目录：[http://www.jianshu.com/p/fd82d18994ce](https://www.jianshu.com/p/fd82d18994ce)
- 个人主页：[http://lizejun.cn](https://link.jianshu.com/?t=http://lizejun.cn)
- 个人知识总结目录：[http://lizejun.cn/categories/](https://link.jianshu.com/?t=http://lizejun.cn/categories/)



2人点赞



[性能优化工具]()





作者：泽毛
链接：https://www.jianshu.com/p/7ac6a2b8d740
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。