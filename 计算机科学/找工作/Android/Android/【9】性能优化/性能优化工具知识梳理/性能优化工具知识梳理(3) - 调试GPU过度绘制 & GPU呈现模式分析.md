[TOC]

# 一、概述

今天，介绍两个比较简单的性能优化工具：

- 调试`GPU`过度绘制
- `GPU`呈现模式分析

其实这两个工具所解决的问题并不相同，之所以把它们放在一起，是因为它们都是`Android`手机自带的分析工具，我们只要在设置中对应的开关，就可以实时获得分析的结果，下面，我们就一起来看一下如何使用它们。

# 二、调试`GPU`过度绘制

## 2.1 应用场景

这个工具主要是用来**检查布局中是否存在布局层次过深**的问题。
首先，说明一下什么叫过度绘制，**过度绘制指的是屏幕中同一个像素点被绘制了多次**，举个例子，我们有一个红色`ViewB`，它先被绘制了一次，也就是说它所在区域的每个像素点都被绘制成了红色，这时候有一个蓝色`ViewA`，它盖在`ViewB`的上面，所以我们需要再把每个像素点都绘制成蓝色，这其实是不必要的，出现这种情况的时候，我们就可以通过这个工具来避免这种情况的发生。

## 2.2 使用方式

使用方式很简单，进入`设置/辅助功能/开发者选项/`，点击**调试`GPU`过度绘制**选项，在弹出框中选择第二项：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn7fc4vpj30co09f0tg.jpg)


打开开关之后，可以看到界面当中会出现各种颜色的矩形，这就对应着过度绘制的等级，颜色越深，表明过度绘制越严重，也就是说同一个像素点被重复绘制的次数过多，下面，我们用一个简单的例子，来看一下过度绘制的等级。
首先看一下我们的布局：





```xml
<?xml version="1.0" encoding="utf-8"?>
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    tools:context="browser.android.com.repoperformance.OverDrawActivity">
    <FrameLayout
        android:id="@+id/fl_1"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:layout_marginBottom="100dp"
        android:background="@android:color/white">
        <FrameLayout
            android:id="@+id/fl_2"
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            android:layout_marginBottom="100dp"
            android:background="@android:color/white">
            <FrameLayout
                android:id="@+id/fl_3"
                android:layout_width="match_parent"
                android:layout_height="match_parent"
                android:layout_marginBottom="100dp"
                android:background="@android:color/white">
                <FrameLayout
                    android:id="@+id/fl_4"
                    android:layout_width="match_parent"
                    android:layout_height="match_parent"
                    android:layout_marginBottom="100dp"
                    android:background="@android:color/white">
                    <FrameLayout
                        android:id="@+id/fl_5"
                        android:layout_width="match_parent"
                        android:layout_height="match_parent"
                        android:layout_marginBottom="100dp"
                        android:background="@android:color/white"/>
                </FrameLayout>
            </FrameLayout>
        </FrameLayout>
    </FrameLayout>
</RelativeLayout>
```

我们布局呈现为逐级嵌套的层次，并且从`fl_1`开始都有一个白色的背景，那么我们看一下打开了调试`GPU`过度绘制开关之后的结果：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn7df0q5j308x0f60kn.jpg)



从上面的图中可以看到，`fl_1`的白色绘制是正常的，而`fl_2`和`fl_1`在由于有`100dp`的重叠区域，因此这部区域被检测成为了蓝色，而`fl_3`和`fl_1/fl_2`都有重叠，因此这部分区域被检测成为了绿色，`fl_4`和`fl_5`也是同理。
通过上面的例子，可以看到检测的结果分为四个等级，从低到高分别是：

- 蓝色
- 绿色
- 浅红
- 深红

在开发当中，我们在设计完界面之后，就应当通过这个工具来检测一下，看能否在保证实现功能的前提下，避免出现过度绘制。

# 三、`GPU`呈现模式分析

## 3.1 应用场景

当打开这个工具之后，会在屏幕的底端展现当前界面的绘制情况，`GPU`呈现模式分析有以下几点作用：

- 查看每一帧的渲染是否达到`16ms`的要求
- 分析一帧的渲染过程各阶段的耗时
- 因为它是实时展现的，因此我们可以快速到问题产生的原因

这个工具的使用方法和上面类似，同样是进入开发者选项中，然后点击`GPU`呈现模式分析，选择“在屏幕上显示为条形图”。

## 3.2 使用详解

在`Android 6.0`之前和之后，`GPU`呈现的模式会有所不同，区别在于`6.0`之后，它将整个绘制的阶段更加细分，让开发者能够更方便的定位问题。

- 基础
  无论是在`6.0`之前还是之后，这个工具的原理都是相同的。因为系统每隔`16ms`就会发出一次`VSYNC`信号，通知刷新`UI`，如果在`16ms`之内没有完成绘制，那么就必须等到下一次，这就会导致在很长一段时间内，看到的都是同一个画面，也就是我们所说的”卡顿”，界面的展示就是基于这个原理：

- 柱状图：柱状图的每一根的高度就表示渲染这一帧的耗时，当渲染的时间越长，则柱状图的高度越高。

- 基准线：我们在界面上并不能看到柱状图对应的时间，而是通过在柱状图上方的基准线来判断是否超过了标准的时间，基准线对应的就是`16ms`，如果柱状图的高度在基准线的下方，那么就表示这一帧绘制的时间小于`16ms`。

- `Android 6.0`之前
  在这个`6.0`版本之前，我们将柱状图分为以下几个部分：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn7azwl1j30xc0jwgxx.jpg)

  

- 蓝色`Update`
  这部分代表`View`创建和更新`DisplayList`的时间，如果这部分很高，那么表示我们有很多自定义的`View`，或者在`onDraw`当中进行了过于复杂的操作。

- 紫色`XFer`
  这部分在`Android 4.0`之后才有，表示将资源传递到渲染线程所花的时间。

- 红色`Execute`
  这部分代表`Android 2D`渲染器向`OpenGL`发送命令来绘制和重绘的时间，这些命令就是来自于前面生成的`Display List`，如果这部分很高，那么说明执行`Display Lists`中的命令花费了很多的时间。

- 黄色`Process`
  这部分代表了`CPU`等待`GPU`完成操作所花的时间，如果这部分很高，那么说明`GPU`当前很忙碌。

从上面的解释当中，我们可以发现，虽然说这个工具的名字叫做**GPU呈现模式分析**，但是我们获得的所有信息都是来自`CPU`的，也就是说，我们是从`CPU`的角度来间接地分析出当前渲染需要处理的信息。

整个渲染的过程是通过`CPU`向`GPU`发出命令，再由`GPU`去异步地渲染屏幕。在某些情况下，由于`GPU`有太多的工作要做，那么就会导致`CPU`需要一直等待才能发出新的命令，而当这种情况发生的时候，我们就会看到橙色和红色的部分特别长。

关于`6.0`之前各颜色的解释来自于官方文档：

> [`http://android.xsoftlab.net/tools/performance/profile-gpu-rendering/index.html`](https://link.jianshu.com/?t=http://android.xsoftlab.net/tools/performance/profile-gpu-rendering/index.html)

- ```
  Android 6.0
  ```

  之后

  在

  ```
  Android 6.0
  ```

   

  之后，变为了现在的八个部分：

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn7951l9j30n00vmgqs.jpg)

  其中，如果新增部分的图形高度较高，那么表示：

  - `Misc/Vsync Delay`
    我们在主线程当中执行了过多的操作，导致跟不上`VSYNC`信号。
  - `InputHandling`
    我们在处理用户输入的地方做了过多的操作。
  - `Animation`
    我们在执行动画的过程中进行了耗时的操作。
  - `Measure & Layout`：
    我们的布局过于复杂，以至于在测量和布局的过程中耗费了过多的时间。
  - `Sync & Upload`：
    准备当前界面中有待绘制的图片所耗费的时间过长。

`6.0`之前所保留下来的对应关系为：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyn77g682j30rm0fu787.jpg)



关于`6.0`之后各颜色的解释来源于：

> [`http://hukai.me/android-performance-patterns-season-5/`](https://link.jianshu.com/?t=http://hukai.me/android-performance-patterns-season-5/)

# 四、小结

这篇文章，主要还是着重于介绍如何使用这两个工具，因此，关于整个渲染的原理也只是一笔带过，之后会专门详细的分析。

------

## 更多文章，欢迎访问我的 **Android** 知识梳理系列：

- **Android** 知识梳理目录：[http://www.jianshu.com/p/fd82d18994ce](https://www.jianshu.com/p/fd82d18994ce)
- 个人主页：[http://lizejun.cn](https://link.jianshu.com/?t=http://lizejun.cn)
- 个人知识总结目录：[http://lizejun.cn/categories/](https://link.jianshu.com/?t=http://lizejun.cn/categories/)



6人点赞



[性能优化工具]()





作者：泽毛
链接：https://www.jianshu.com/p/ac2d58666106
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。