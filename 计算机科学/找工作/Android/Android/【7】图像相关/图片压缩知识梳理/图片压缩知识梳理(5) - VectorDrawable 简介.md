[TOC]

# 一、为什么要使用 VectorDrawable

虽然目前已经有很多针对于`PNG/JPG`的压缩算法，但是这些算法最终的压缩效果仍然是有限的，因此，人们开始尝试寻找一种新的图片格式来替代它们，今天我们所介绍的`VectorDrawable`就是`Android`平台上一种新的图片存储格式，它采用了一种**矢量图形**的思想。

# 二、光栅图像（Raster Image）

在介绍`VectorDrawable`之前，我们先简单了解一下最常见的光栅图像。

**光栅图像**也叫做位图、点阵图、像素图，它是有一个个小的像素点组成，而每个像素点由通过一系列的颜色通道（`RGB`）来组合成它的颜色，我们平时谈到的`PNG`、`JPG`就是光栅图像的子类。

当我们在应用中使用这种类型的图片时，会面临如下几个问题：

- 图片的大小会随着像素个数的增加而线性增大
- 图片的压缩率会受到图片中各像素点的自相似性影响，其表现为：当图片中相同颜色的像素点越多，那么压缩率就越高。
- 为了适应多种机型的分辨率，我们还需要在`APK`中内置多种分辨率的图片，这无形中就增加了`APK`的大小。

# 三、矢量图形（Vector Image）

## 3.1 矢量图形的基本概念

为了解决以上的几个问题，**矢量图形**的想法就被提出了：它包含的不是图片中的各个像素点的描述，而是对于图片的绘制命令，当我们解析图片的时候，会运行这一系列的绘制命令来最终得到光栅图像。

## 3.2 SVG 和 VectorDrawable

`SVG`就是矢量图形的一种具体实现，它包含了一系列的绘制命令，例如下图，我们使用`xml`文件的多个标签进行组合，最终把图像描述出来：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynd925dzj30hb074myc.jpg)


然而`SVG`的缺点是它包含了许多在`Android`中并不必要的组件，而`Android`平台又非常在意`APK`的大小，因此它对`SVG`的协议进行了裁剪，定义了自己的矢量图形格式 - `VectorDrawable`。



`VectorDrawable`的工作原理和`SVG`类似，但是它只包含`SVG`绘制命令的一小部分。其整个处理过程为加载`VD`，对其进行栅格化处理，使用栅格化处理后的`Bitmap`，下图就是一个`VectorDrawable`的例子：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynd78l7yj30hb0cfgnv.jpg)


`VectorDrawable`和光栅图像相比有以下几点优势：



- 在`aapt`打包的过程中会把`VectorDrawable`所对应的`xml`文件转换为二进制流的格式，这样就可以进一步缩减文件的大小
- 由于`VectorDrawable`是通过绘制的命令来描述一个图像，因此它能做到一个文件，适配多种分辨率的机型
- 可以对`VectorDrawable`中的各个部分添加动画，也就是`AnimatedVectorDrawable`

# 四、VectorDrawable 的适用范围

虽然说`VectorDrawable`有很多的优点，但是也有它的局限性，例如下面这两点：

## 4.1 适用于较小的图片

在`Android`当中，`VectorDrawable`到光栅图像的栅格化处理是通过`CPU`进行的。而这一光栅处理的时间和图片的大小成正比，因此，如果图片很大，那么加载的时间将会很长，这种情况就不适合使用`VectorDrawable`了。

## 4.2 适用于较为简单的图片

`VectorDrawable`还有一点限制，就是你只能通过一系列简单的图形（矩形、圆形、弧形）和连线来组成最终的图像，因此，如果图形很复杂，那么就需要更多的命令来描述这个图形，最终就导致了文件的增大和加载时间的增加，因此，尽量只用`VectorDrawable`来表示较为简单的图形。

# 五、简化 VectorDrawable 的 Path

在使用`VectorDrawable`的时候，我们一般是先产生一个原始的`SVG`图像，再通过第三方工具将它转换成为`Vector`格式。在这一转换的过程当中，最需要关注的一点就是转换后所生成的`vector`文件中的`pathData`属性：

- 当需要描述一个较为复杂的图像时，`pathData`通常会变得非常大。有的时候，仅仅对`SVG`图像进行一些微小的调整，都可能使得`pathData`发生很大的变化。
- 应用程序在需要将一个`VectorDrawable`所描述的图像展现在屏幕上之前，首先会通过`Path`进行栅格化处理，这一处理过程所耗费的时间而资源将会随着`pathData`的增大而增加。

也就是说，使用`VectorDrawable`所耗费的成本而`pathData`的大小是成正比的，因此我们应该**尽可能地简化 `VectorDrawable`的`Path`数据**。

# 六、在必要时使用 ShapeDrawable

对于某些图像，如果我们能用原始的图形（圆形、矩形、椭圆等等）来替换`Path`，那么将有可能进一步的减小栅格化过程所耗费的性能，以及图片文件的大小。

`ShapeDrawable`就是一种很好的替代方式，它使用一系列简单的图形来描述图像，例如下面这样：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynd5yk6bj30ha07t3zl.jpg)



# 七、文献

[Image compression - How VectorDrawable works](https://link.jianshu.com/?t=https://medium.com/@duhroach/how-vectordrawable-works-fed96e110e35)



3人点赞



[图片压缩]()





作者：泽毛
链接：https://www.jianshu.com/p/3297caac89f8
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。