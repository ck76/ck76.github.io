SurfaceView和TextureView均继承于android.view.View

与其它View不同的是，两者都能在独立的线程中绘制和渲染，在专用的GPU线程中大大提高渲染的性能。

**一、SurfaceView专门提供了嵌入视图层级的绘制界面，开发者可以控制该界面像Size等的形式，能保证界面在屏幕上的正确位置。**

但也有局限：

由于是独立的一层View，更像是独立的一个Window，不能加上动画、平移、缩放；

两个SurfaceView不能相互覆盖。

**二、TextureView更像是一般的View，像TextView那样能被缩放、平移，也能加上动画。**

TextureView只能在开启了硬件加速的Window中使用，并且消费的内存要比SurfaceView多，并伴随着1-3帧的延迟。

**三、TextureView和SurfaceView都是继承自View类的，但是TextureView在Andriod4.0之后的API中才能使用。**

SurfaceView可以通过SurfaceHolder.addCallback方法在子线程中更新UI，TextureView则可以通过TextureView.setSurfaceTextureListener在子线程中更新UI，个人认为能够在子线程中更新UI是上述两种View相比于View的最大优势。

 但是，两者更新画面的方式也有些不同，由于SurfaceView的双缓冲功能，可以是画面更加流畅的运行，但是由于其holder的存在导致画面更新会存在间隔（不太好表达，直接上图）![img](https://files.jb51.net/file_images/article/201804/201843162917669.jpg?201833162945)。并且，由于holder的存在，SurfaceView也不能进行像View一样的setAlpha和setRotation方法，但是对于一些类似于坦克大战等需要不断告诉更新画布的游戏来说，SurfaceView绝对是极好的选择。但是比如视频播放器或相机应用的开发，TextureView则更加适合。





## SurfaceView

Android中 View是通过刷新来重绘视图，系统通过发出`VSYNC`信号来进行屏幕的重绘，刷新的时间间隔是`16ms`,如果我们可以在16ms以内将绘制工作完成，则没有任何问题，**如果我们绘制过程逻辑很复杂，并且我们的界面更新还非常频繁，这时候就会造成界面的卡顿**，影响用户体验，为此Android提供了`SurfaceView`来解决这一问题

SurfaceView 继承自View，是 Android 中一种比较特殊的视图（View），

- 它跟普通View最大的区别是它有自己的Surface，在WMS中有对应的WindowState，在SurfaceFlinger中有Layer
- 一般的Activity包含的多个View会组成View hierachy的树形结构，**只有最顶层的DecorView，也就是根结点视图，才是对WMS可见的**。这个**DecorView在WMS中有一个对应的WindowState**。相应地，**在SF中对应的Layer**。
- 而**SurfaceView自带一个Surface，这个Surface在WMS中有自己对应的WindowState**，在SF中也会有自己的Layer。虽然在App端它仍在View hierachy中，但在Server端（WMS和SF）中，**它与宿主窗口是分离的**。这样的好处是对这个Surface的渲染可以放到单独线程去做，渲染时可以有自己的GL context。这对于一些游戏、视频等性能相关的应用非常有益，因为它不会影响主线程对事件的响应。

综合这些特点，SurfaceView 一般用在游戏、视频、摄影等一些复杂 UI 且高效的图像的显示，这类的图像处理都需要开单独的线程来处理。它的优点如下

- SurfaceView 通过子线程中进行画面更新，View 则在主线程中进行画面更新。
- SurfaceView 用于被动更新，如频繁画面更新，View 则用于主动更新，如触摸点击等事件响应等。
- SurfaceView 在底层实现了双缓冲机制，效率大大提升了，View 则没有。

下面清晰说明了SurfaceView的原理

![Surface原理](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/w*28aHjfpLK5YxjRWkehvdW3.XcVi*H8CyhedovmHkY!/r/dL4AAAAAAAAA)

如果当前画面需要不停绘制或者数据处理量较大时，为避免 UI 线程堵塞，就用 SurfaceView 代替 View。
SurfaceView拥有独立的绘图表面，即它不与其宿主窗口共享同一个绘图表面，由于拥有独立的绘图表面，因此SurfaceView的UI就可以在一个独立的线程中进行行绘制，由于不占用主线程资源,使得它可以实现大多复杂而高效的界面绘制，如视频播放 **VideoView** 和OpenGl es的 **GLSurfaceView**
**直播软件的 不停地点赞动效、天气软件的全屏雨雪动效、游戏中的流水、云之类的变化等等**





| View                     | SurfaceView                      |
| ------------------------ | -------------------------------- |
| 适用于主动更新           | 适用于被动刷新                   |
| 在主线程中进行画面更新   | 通常通过一个子线程来进行画面更新 |
| 绘图中没有使用双缓冲机制 | 在底层实现中就实现了双缓冲机制   |





```
Surface
MediaPlayer
SurfaceView
TextureView
SurfaceTexture
FixedTextureView
```