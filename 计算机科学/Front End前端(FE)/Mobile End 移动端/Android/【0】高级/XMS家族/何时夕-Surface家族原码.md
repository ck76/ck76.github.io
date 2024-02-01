**阅读须知**

- 1.进入微信公众号 **世界上有意思的事** 发送消息：**Android绘制机制以及Surface家族源码全解析，**即可获取本文的 pdf 版。
- 2.本文分析的源码版本是 **Android 7.0**，建议结合源码阅读本文
- 3.推荐一个 Android 源码阅读网站：[Android 源码阅读网站](http://androidxref.com/7.0.0_r1/) 
- 4.因为很多 java 层和 c++ 层的类命名都相同，所以后续例如：Surface.lockCanvas 表示 java 层的调用，Surface::lockCanvas 表示 c++ 层的调用
- 5.本文的一些缩写：**SF——>SurfaceFlinger、WMS——>WindowManagerService、MessageQueue——>MQ、GL——>OpenGL、BQ——>BufferQueue、ST——>SurfaceTexture、TV——>TextureView、SV——>SurfceView** 
- 6.本文是**视频编辑SDK开发的重要前置知识，建议读透** 

## 一、Android绘制机制概览

> 在深入各种源码之前，我们需要了解 Android 绘制机制的整体架构，以及一些概念

### 1.Android屏幕刷新机制

![Android屏幕刷新机制](https:////upload-images.jianshu.io/upload_images/2911038-aa6147e18bbecef3.jpg?imageMogr2/auto-orient/strip%7CimageView2/2/w/553/format/webp)

图1：屏幕刷新.jpg

**图1就是 Android 屏幕显示的抽象示意图，这里我来解释一下：**

- 1.首先图的横轴是时间，纵轴从下到上分别表示：CPU 处理、GPU 处理、屏幕显示，这三个步骤也就是我们写的代码到图像显示在屏幕上的流程。

- 2.我们都知道要让手机不卡顿一个显而易见的标准就是：**屏幕上每隔一定的 ms 就能显示下一帧图像。**在这里这个时间由底层控制，也就是图中两个 **VSync** 的间隔时间——**16ms**。

- 3.Android 中引入了下面这些特性来保证屏幕上的数据每隔 

  16ms

   来刷新一次。 

  - 1.一个固定的脉冲信号——**VSync**，这个东西由底层保证，每一次 **VSync** 信号来了 CPU 就开始运行绘制代码(**例如运行 View.draw 之类的方法**)，当 CPU 的数据准备好了，就将这些数据交给 GPU 让其在一块内存缓冲区上进行图像的绘制。当 GPU 绘制好了就将图像显示到屏幕上。
  - 2.**三缓冲**，图中的 A、B、C 表示的是三块内存缓冲区。因为不是每次 CPU、GPU 的数据处理都能在16ms 内完成的，所以为了不浪费时间而且不丢弃之前做完的工作。CPU、GPU 的工作会依次反应在 A、B、C 三块内存缓冲区中。而屏幕每次都取当前已经准备好的内存缓冲区。**三缓冲较双缓冲的问题就是：我们的操作最终显示到屏幕上的时候会延迟16ms，这可能也是 Android 不如 ios ”跟手“的一个原因** 

- 4.由我们可以得出两个简单的结论： 

  - 1.ui线程太忙了，使得 CPU 16ms 内没有处理好数据会导致丢帧。
  - 2.需要绘制的图像太复杂，导致 GPU 16ms 没有绘制好图像也会导致丢帧。

### 2.Android图像绘制方式

> 问大家一个问题：平时我们开发过程中可以用哪些工具在屏幕上绘制图像？大家一定可以回答出很多东西：View、Drawable、xml、SV、GLSurfaceView、TV、Canvas等等。其实 Android 上面一共只有两种常用的绘图机制，上面列举出来的东西都是由这两种机制演变而来的。这一节我就简单归纳介绍一下。

**Android 的两种常用绘图机制：**

- 1.**Skia图形库：**[Skia官网](https://skia.org/index_zh)，Skia是一个开源的二维图形库，提供各种常用的API，并可在多种软硬件平台上运行。在**没开启硬件加速**的时候用到 Canvas 的地方在底层都会调用到 Skia 库中去。在上面我们列举的方式里面：View、Drawable、xml、SV、Canvas 最终使用的都是 Skia 库。**另外 Skia 最终是使用 CPU 来对图像进行最终绘制，所以效率比较低。** 
- 2.**GL：**GL 是用于渲染 2D、3D 矢量图形的跨语言、跨平台的应用程序编程接口。在**开启硬件加速**的时候用到 Canvas 的地方最终会调用到 **GL ES** 库中。没开启硬件加速的时候上面我们列举的方式中：GLSurfaceView、TV 最终使用的是 **GL ES**，**另外 GL 使用的是 GPU 来对图像进行绘制，所以效率比较高。** 
- 3.**在后续的章节里我会从源代码上分析上面各种绘图方式的调用链。** 
- 4.**其实在 7.0 之后 Android 中添加了 Vulkan。Vulkan 是用于高性能 3D 图形的低开销、跨平台 API。与 GL ES 一样，Vulkan 提供多种用于在应用中创建高质量的实时图形的工具。不过目前很少用到，所以本篇文章中我们不讨论它。** 

### 3.Android绘制中的生产者和消费者

> android 的绘制机制中存在着一系列**生产者**和**消费者**，这一节我将介绍一下这个机制中相关的概念。

- 1.BQ：如图2所示，BQ 是一个存储着

  内存块

  的队列。 

  - 1.

    生产者

    ：它可以使用两个 api，

    queue

     和 

    dequeue

    。 

    - 1.**dequeue：**需要一块内存来绘制图像的时候，可以从队列的尾部拿出一块内存进行绘制。
    - 2.**queue：**当图像绘制完毕的时候，可以将该内存添加到队列的头部

  - 2.

    消费者：

    它也可以使用两个 api，

    acquire

     和 

    release

    。 

    - 1.**acquire：**当需要一块已经绘制完成的内存再对其进行处理的时候，可以从队列的头部拿出一块内存。
    - 2.**release：**当对图像内存处理完毕的时候，可以将内存重置然后放回队列的尾部



![img](https:////upload-images.jianshu.io/upload_images/2911038-6b7cfbd9641f3b30.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/481/format/webp)

图2：BufferQueue.png

- 2.图像内存的生产者： 
  - 1.**Surface**：Surface 是 BQ 的生产者。当我们使用 **lockCanvas** 和 **unlockCanvasAndPost** 的时候，就是先从 BQ 中取出一块内存然后调用 Canvas/GL 的 api 对内存进行绘制，最后将内存放回 BQ 中。
  - 2.我们知道了 Surface 是生产者，那么像 View、SV、GLSurfaceView 这些间接或者直接用到了 Surface 的东西就都是生产者了。
- 3.图像内存的消费者： 
  - 1.**SF**：它有自己的**进程**，在 Android 系统启动的时候就像其他各种 Service 一样被创建了。它的作用是接受来自多个来源的内存缓冲区，对它们进行合成，然后发送到显示设备。大多数应用通常在屏幕上有三个层：屏幕顶部的状态栏、底部或侧面的导航栏以及应用的界面。所以应用的 Surface 生产的内存就会被它所消耗。
  - 2.**ST：**这个东西是常常用在 TV 中。它可以在我们的应用中使用。它在创建的时候会建立一个自己的 BQ。我们可以通过 ST 来创建一个 Surface 然后通过 Surface 向 BQ 中提供图像内存。此时 ST 就可以消耗这些图像内存。它可以使用 GL 来对这些被消耗的图像内存进行二次处理，然后让这些被处理之后的图像内存在通过 GLSurfaceView 之类的东西显示到屏幕上。

## 二、Android绘制机制源码分析

> 这一章我们来从源码上分析 View 是如何绘制到屏幕上面的，前面的 measure、layout、draw 等等 framework 层的东西我不会着重分析，我主要分析 cpp 层的东西。



![img](https:////upload-images.jianshu.io/upload_images/2911038-e167cc04b6df9e8b.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1000/format/webp)

图3：Android绘制机制.png

**其实源码的主要流程都在图3中，我下面讲的东西算是对图3的补充和说明。另外强烈建议结合 Android 源码阅读本章节。**

- 1.首先我们的入口是 ViewRootImpl.scheduleTraversals。看过源码的同学应该知道，类似 invalidate、requestLayout 等等需要改变 View 的显示的操作，最终都会层层向上调用最终调用到这个方法上。 

  - 1.在这个方法里主要调用到的方法就是 Choreographer.postCallback 这个方法传入了 mTraversalRunnable，表示在某个时间点会调用这个 Runnable 中的 run()。我们在第一章中讲过 VSync 的相关知识，而 Choreographer 就是 VSync 在代码层面的实现。 
    - 1.postCallback 根据 scheduleFrameLocked——>scheduleVsyncLocked——>scheduleVsyncLocked 的调用链，最终会调用到 DisplayEventReceiver.nativeScheduleVsync 向native 层申请下一个 VSync 信号。

- 2.16ms 后 VSync 信号会从 native 层向上触发，这里是想 ui 的 loop 中添加了一个 msg。这样的话调用链就是：DisplayEventReceiver.dispatchVsync——>DisplayEventReceiver.onVsync——>Choreographer.doFrame。 

  - 1.到了 doFrame 这里就会调用前面 postCallback 中传入的 mTraversalRunnable 的 run()。我们知道 run() 中会调用 ViewRootImpl.doTraversal 方法。 

    - 1.这样就会调用到 ViewRootImpl.performTraversals 中去。我想大家应该对这个方法很熟悉，这个方法就是调用 measure、layout、draw 的方法。已经分析烂了的东西这里我就不说了。

    - 2.我们直接看 ViewRootImpl.draw 方法，这里会有两种绘制方式。就像我们在第一章中说的那样。如果

      没有开启硬件加速

      那么就使用 Skia 库以 CPU 来绘制图像，这里的入口方法是 drawSoftware。如果

      开启了硬件加速

      那么就是用 GL ES 来以 GPU 绘制图像，这里入口方法是 ThreadedRenderer.draw。 

      - 1.drawSoftware：这个方法比较简单就是创建一个 Surface，然后 lockCanvas 得到一个 Canvas，然后在各个层级的 View 中调用 Canvas 最终调用 Skia 库来在 Surface 上提供的图像内存中绘制图像，画完之后调用 unlockCanvasAndPost 来提交图像内存。这里更详细的流程我会在**第三章中分析 Surface 的时候分析。** 

      - 2.ThreadedRenderer.draw：这个方法是硬件加速下的图像绘制入口，里面最终都是调用到 GL 的 api。在深入之前我们先了解一下硬件加速绘制的几个阶段： 

        - 1.硬件加速绘制的五个阶段： 

          - 1.APP在UI线程使用 Canvas 递归构建 GL 渲染需要的命令及数据
          - 2.CPU 将准备好的数据共享给 GPU
          - 3.CPU 通知GPU渲染，这里一般不会阻塞等待GPU渲染结束，因为效率很低。CPU 通知结束后就返回继续执行其他任务。当然使用 glFinish 可以阻塞执行。
          - 4.swapBuffers，并通知 SF 图层合成
          - 5.SF 开始合成图层，如果之前提交的GPU渲染任务没结束，则等待GPU渲染完成，再合成(Fence机制)，合成依然是依赖GPU

        - 2.硬件加速绘制代码分析： 

          - 1.我们先看 draw() 里面调用的 updateRootDisplayList： 

            - 1.这个方法的第一个调用链是这样的 updateViewTreeDisplayList——>View.updateDisplayListIfDirty——>View.draw。如图4，这里聪明的同学一看就知道是一个递归操作。View 的 draw 会递归到子 View 中。然后各个 View 会调用 Canvas 的 api 将绘制操作储存在 Canvas 中。那么这里的 Canvas 是怎么来的呢？其实在每个 View 创建的时候内部会创建一个 RenderNode 。这个对象可以创建一个 DisplayListCanvas 来作为 Canvas 给各个 View 在绘制的时候使用。而每个子 View 调用 draw(Canvas, ViewGroup, long) 的时候都会得到 parentView 传递下来的 DisplayListCanvas，然后在本 View.draw(Canvas) 调用结束之后，将DisplayListCanvas 的操作储存到本 View 的 RenderNode 中。最后调用 parentView 的 DisplayListCanvas.drawRenderNode 将本 View 的 RenderNode 存入 parentView 的 RenderNode 中。如此递归，最终将所有绘制操作存入 RootView 的 RenderNode 中。至此 RootView 中就包含了一个 DrawOp 树。

            - 

              ![img](https:////upload-images.jianshu.io/upload_images/2911038-ba00e2e679a02caf.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/879/format/webp)

              图4：硬件加速下的Canvas绘制流程.png

            - 2.我们回到 updateRootDisplayList 这里后续就是将 RootView 的 DrawOp 树 交给 ViewRootImpl 的 RenderNode 方便后面进行操作。

          - 2.再回到 draw() 中，这里下一个调用的重要的方法是 nSyncAndDrawFrame： 

            - 1.这个方法最终会调用到 c++ 层的 RenderProxy::syncAndDrawFrame 方法。在了解这里的调用链之前。我先介绍一下 Android 5.0 之后出现的渲染线程的概念，再来讲解调用链。 
              - 1.首先渲染线程的实现类是 RenderThread.cpp 它是和 ui 线程类似，是一个事件驱动的 Loop。它的也有队列，队列中储存着 DrawFrameTask.cpp 对象。RenderProxy.cpp 是 RenderThread.cpp 给 java 层的代理。ThreadRender 所有关于渲染线程的请求都会交给 RenderProxy.cpp 然后由它向 RenderThread.cpp 提交 task。
              - 2.了解了渲染线程的概念，我们再来讲讲调用链：syncAndDrawFrame——>  DrawFrameTask::drawFrame——>DrawFrameTask::postAndWait。这里做的事情很简单，向 RenderTheas.cpp 的队列中插入一个 DrawFrameTask.cpp，然后阻塞当前的 ui 线程。

          - 3.ui 线程被阻塞之后，渲染线程会调用到上一次插入到队列里的 DrawFrameTask.run 方法。 

            - 1.run() 这里会先调用  syncFrameState，这个方法主要是用于同步 java 层的各种数据。 
              - 1.第一步是先用 mLayers::apply 来同步数据，这个 mLayers 在 java 层的体现是 TV。这里的分析我们会在第三章中着重分析，这里先略过。
              - 2.第二步是调用 CanvasContext::prepareTree 来将前面在 java 层构建的 DrawOp 树同步到 c++ 层，以便后续运行 OpengGL 的命令。这里关键的调用链是：CanvasContext::prepareTree——>RenderNode::prepareTree——>RenderNode::prepareTreeImpl。由前面我们可以知道 RenderNode.java 已经构建了一个 DrawOp 树。但是之前只是调用 RenderNode::setStagingDisplayList 暂存在 RenderNode::mStagingDisplayListData 中的。因为 java 层在运行过程中还会出现多次meausre、layout的，还有数据还可能发生改变。所以当走到这里的时候数据已经确定了，所以可以开始同步数据。prepareTreeImpl 同步数据主要有三个步骤： 
                - 1.调用 pushStagingDisplayListChanges 同步当前 RenderNode.cpp 的属性，也就是把 mStagingDisplayListData 赋值给 mDisplayListData
                - 2.调用 prepareSubTree 递归处理子 RenderNode.cpp。
                - 3.这里会有一个同步成功和同步失败的问题，一般来说这里的数据都会同步成功的。但是在 RenderNode::prepareSubTree 中会有一个步骤是把 RenderNode 用到的 Bitmap 封装成纹理，一旦这里 Bitmap 太大或者数量太多那么同步就会失败。**注意这里同步失败只是会与 Bitmap 有关，其他的 DrawOp 数据无论如何都会同步成功的。** 
              - 3.如果这里同步成功了的话，那么 ui thread 就会被唤醒，反之则暂时不唤醒。
            - 2.run() 中将数据同步完成之后，就会调用 CanvasContext.draw，这个方法主要有三个操作： 
              - 1.mEglManager::beginFrame，其实是标记当前上下文，并且申请绘制内存，因为一个进程中可能存在多个window，也就是多个 EglSurface，那么我们首先需要标记处理哪个，然后向 SF 申请内存，这里 EglSurface 是生产者，SF 是消费者。
              - 2.根据前面的 RenderNode 的 DrawOp 树，递归调用 OpenGLRender 中的 GL API，进行绘制。
              - 3.通过 swapBuffers 将绘制好的数据提交给 SF 去合成，**值得注意的是此时可能  GPU 并没有完成当前的渲染任务，但是为了提高效率，这里可以不用阻塞渲染线程。** 
            - 3.当所有的绘制操作都通过 GL 提交给了 GPU 的时候，如果前面数据同步失败了，那么这个时候需要唤醒 ui thread。

## 三、Surface家族源码全解析

> 上一章我们讲了 Android 的整个绘制机制，但是其中涉及到 Surface 的部分都是简单带过。所以这一章我就来解析一下 Surface 家族中各个成员的源码。

### 1.Surface的创建与绘制

#### (1).Surface的创建



![img](https:////upload-images.jianshu.io/upload_images/2911038-9759d6e212ac0a12.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1000/format/webp)

图5：Surface 创建.png

**这里我们以 View 的创建流程为例，讲述一下 Surface 在这个过程中的创建流程，Surface 的创建流程如图5所示。 **

- 1.首先我们需要知道的是 ViewRootImpl 在创建的时候会使用 **new Surface** 来创建一个 java 层的空壳。这个空壳会在后面被初始化。
- 2.然后入口的调用链是这样的：ViewRootImpl.performTraversals——>ViewRootImpl.relayoutWindow——>WindowManagerService.relayoutWindow——>WindowManagerService.createSurfaceControl。 
  - 1.createSurfaceControl 这个方法里首先会 WindowStateAnimator.createSurfaceLocked——>new WindowSurfaceController——>new SurfaceControlWithBackground——>new SurfaceControl——>SurfaceControl.nativeCreate——>android_view_SurfaceControl::nativeCreate 来创建一个SurfaceControl.cpp 这个东西是初始化 Surface 的参数。 
    - 1.nativeCreate 的第一步是创建一个 SurfaceComposerClient.cpp 它其实是 SF 所在进程的代理，我们可以通过这个类对 SF 进行操作。在调用 new SurfaceComposerClient.cpp 的构造函数之后，首先会触发 onFirstRef，这里面则会使用 **ComposerService::getComposerService()** 获取 SF 的服务。然后远程调用 ComposerService::createConnection 也就是 SF::createConnection 来创建一个 ISurfaceComposer 作为 SurfaceComposerClient 与 SF 进程交互的接口，**这里用到的是 Binder 机制**。
    - 2.SurfaceComposerClient 创建完毕之后，就可以调用 SurfaceComposerClient::createSurface——>Client::createSurface 来向 SF 进程发送创建 Surface 的请求了。SF 进程也是事件驱动模式，所以 Client::createSurface 中调用 SF::postMessageSync 发送了一个调用 SF::createLayer 方法的消息给事件队列。 而  createLayer 最终会调用 createNormalLayer 中。这个方法会返回一个 IGraphicBufferProducer 给 SurfaceControl.cpp。**记得我们在前面讲的 生产者——消费者 模式吗？**这里的 IGraphicBufferProducer 就是 SF 的 BQ 分离出来的生产者，我们后续就可以通过这个  IGraphicBufferProducer 向 SF 的 BQ 中添加通过 Surface 绘制好的图像内存了。
  - 2.回到 createSurfaceControl 这里创建了一个 SurfaceControl.java 之后，下一步就是初始化 Surface.java。这里就比较简单了，就是通过调用链：SurfaceController.getSurface(Surface)——>Surface.copyFrom(SurfaceControl)——>Surface.nativeCreateFromSurfaceControl——>SurfaceControl::getSurface。将 SurfaceControl.cpp 中的 IGraphicBufferProducer 作为参数创建一个 Surface.cpp 交给 Surface.java。

#### (2).Surface的绘制

> 我们在第二章里面说到 View 的绘制根据**是否硬件加速**分为，**软件绘制**与**硬件绘制**两种。当时我们分析了硬件绘制，软件绘制略过了。其实软件绘制与硬件绘制的区别就在于是使用 CPU 进行绘制计算还是使用 GPU 进行绘制计算。这一小节的 Surface 绘制其实就是软件绘制，也就是 ViewRootImpl.drawSoftware 中的内容。



![img](https:////upload-images.jianshu.io/upload_images/2911038-6d7f7a741abb29e5.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1000/format/webp)

图6：Surface 绘制.png

**我们都知道 Surface 可以通过 lockCanvas 和 unlockCanvasAndPost 这两个 api 来再通过 Canvas 来绘制图像，这一节我就通过这两个 api 来讲讲 Surface 的绘制流程，整个流程如图6所示。**

- 1.首先我们从 lockCanvas 这个入口开始调用链是：lockCanvas——>nativeLockCanvas——>Surface::lock——>Surface::dequeueBuffer，这里最终会使用我们在 Surface 创建的时候得到的 BufferQueueProducer(IGraphicBufferProducer) 来想 SF 请求一块空白的图像内存。得到了图像内存之后，将内存传入 new SkBitmap.cpp 中创建对应对象，一共后面使用 Skia 库绘制图像。这里的 SkBitmap.cpp 会被交给 SkCanvas.cpp 而 SkCanvas.cpp 对象就是 Canvas.java 在 c++ 层的操作对象。
- 2.上面我们通过 lockCanvas 获取到了一个 Canvas 对象。当我们调用 Canvas 的各种 api 的时候其实最终会代用到 c++ 层的 Skia 库，通过 cpu 对图像内存进行绘制。
- 3.当我们绘制完之后就可以调用 unlockCanvasAndPost 来通知 SF 合成图像，调用链是：unlockCanvasAndPost——>nativeUnlockCanvasAndPost——>Surface::queueBuffer，与 lockCanvas 中相反这里是最终是通过 BufferQueueProducer::queueBuffer 将图像内存放回到队列中，除此之外这里还调用 IConsumerListener::onFrameAvailable 来通知 SF 进程来刷新图像，调用链是：onFrameAvailable——>SF.signalLayerUpdate——>MQ.invalidate——>MQ.Handler.dispatchInvalidate——>MQ.Handler.handleMessage——>onMessageReceived：因为 SF 进程采用的也是事件驱动模型，所以这里和 ui thread 类似也是通过 looper + 事件 的形式触发 SurfaceFinger 对图像的刷新的。**注意：这里的 IConsumerListener 是在 createNormalLayer 的时候创建的 Layer.cpp**。

### 2.SurfaceView的创建与使用

> 其实只要了解了 Surface 的创建与使用，那么 SV 就很简单了，SV.updateSurface 中会创建或者更新 Surface。在 SV 上绘制也是调用 Surface 的两个 api。这里我就简单将 View 与 SV 比较一下。

- 1.原理：一般的 Activity 包含的多个 View 会组成 View Hierachy 的树形结构，只有最顶层的 DecorView，也就是根结点视图，才是对 WMS 可见的。这个 DecorView 在 WMS 中有一个对应的 WindowState。相应地，在 SF 中有对应的 Layer。而 SV 自带一个 Surface，这个 Surface 在 WMS 中有自己对应的 WindowState，在 SF 中也会有自己的 Layer。
- 2.好处：在 App 端 Surface 仍在 View Hierachy 中，但在 Server 端(WMS 和 SF)中，它与宿主窗口是分离的。这样的好处是对这个 Surface 的渲染可以放到单独线程去做，渲染时可以有自己的 GL context。这对于一些游戏、视频等性能相关的应用非常有益，因为它不会影响主线程对事件的响应。
- 3.坏处：因为这个 Surface 不在 View Hierachy 中，它的显示也不受 View 的属性控制，所以不能进行平移，缩放等变换，也不能放在其它 ViewGroup 中，一些 View 中的特性也无法使用。

### 3.SurfaceTexture的创建与使用

> ST 我们平时可能用的不是很多，但是其实它是 TV 的核心组件。它可以将 Surface 提供的图像流转换为 GL 的纹理，然后对该内容进行二次处理，最终被处理的内容可以被交给其他消费者消费。这一节我们就来从源码层次解析一下 ST。



![img](https:////upload-images.jianshu.io/upload_images/2911038-2f0d6a72cdf03394.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1000/format/webp)

图7：SurfaceTexture概览图.png

**图7是 ST 与 Surface、SV、TV 等等组件结合的概览图，我这里简单解释一下：**

- 1.最左边表示原始图像流的生成方式：Video(本地/网络视频流)、Camera(摄像头拍摄的视频流)、Software/Hardware Render(使用 Skia/GL 绘制的图像流)。
- 2.最左边的原始图像流可以被交给 Surface，而 Surface 是 BQ 的生产者，GLConsumer(java层的体现就是 ST) 是 BQ 的消费者。
- 3.GLConsumer 拿到了 Surface 的原始图像流，可以通过 GL 转化为 texture。最终以合理的方式消耗掉。
- 4.消耗 GLConsumer 中的 texture 的方式就是使用 SV、TV 等等方式或者显示在屏幕上或者用于其他地方。



![img](https:////upload-images.jianshu.io/upload_images/2911038-3142297d0d99230d.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1000/format/webp)

图8：SurfaceTexture创建以及使用.png

**我将根据图8的流程来讲解 ST 的创建与使用**

- 1.首先我们从 ST.java 的创建开始，也就是图中的黄色方框。注意 ST 的创建需要传入一个 GLES20.glGenTextures 创建的 Texture 的 Id。Surface 提供的图像内存最终也是会被挂靠在这个 Texture 中的。

  - 1.创建的的调用链：SurfaceTexture.nativeInit——>SurfaceTexture::SurfaceTexture_init。
  - 2.SurfaceTexture_init 这个方法里关键步骤如下： 
    - 1.创建 IGraphicBufferProducer 和 IGraphicBufferConsumer 根据前面我们知道这两个类是 BQ 的生产者和消费者。
    - 2.调用 BQ.createBufferQueue 将上面创建的生产者和消费者传入，创建一个 BQ。
    - 3.创建一个 GLConsumer 这个东西相当于 IGraphicBufferConsumer 封装类也是 ST 在  c++ 层的最终实现类。
    - 4.调用  SurfaceTexture_setSurfaceTexture 为 ST.java 的 mSurfaceTexture 赋值，这里被赋值的对象就是上面创建的 GLConsumer。
    - 5.调用 SurfaceTexture_setProducer 同样为 ST 的 mProducer 赋值。
    - 6.调用 SurfaceTexture_setFrameAvailableListener 为 GLConsumer 添加一个 java 层的 OnFrameAvailableListener 作为监听图像流的回调。这个 OnFrameAvailableListener 是调用 ST.setOnFrameAvailableListener 添加的。

- 2.ST 初始化完毕之后，我们此时需要一个 Surface 来作为生产者为 ST 提供图像流。

  - 1.还记得 Surface.java 有个构造函数是需要以 ST 作为参数的吗？我们就从这个函数入手，调用链如下：new Surface(ST)——>Surface.nativeCreateFromSurfaceTexture——>android_view_Surface::

    nativeCreateFromSurfaceTexture。

  - 2.nativeCreateFromSurfaceTexture 中的关键步骤如下：

    - 1.调用 SurfaceTexture_getProducer 获取 ST.java 中储存的 mProducer 对象在 c++ 层的指针。
    - 2.通过上面获取的 IGraphicBufferProducer 对象来创建 Surface.cpp 对象。

- 3.创建了一个负责提供图像流的 Surface 之后我们就可以使用它的两个 api 来绘制图像以提供图像流了。

  - 1.lockCanvas 和 unlockCanvasAndPost 这两个 api 我在这里就不重复解析了。
  - 2.根据前面讲解的 **Surface 的绘制** 我们知道调用了 unlockCanvasAndPost 之后会触发一个 IConsumerListener.onFrameAvailable 的回调。在 View 的绘制流程中我们知道这里的回调最终会触发 SF 的图像合成。那么这里的回调的实现类是谁呢？你猜的没错此时的实现类是 GLConsumer 中被设置的 java 层的 OnFrameAvailableListener，我们在 Surface.cpp 创建的时候会传入一个 IGraphicBufferProducer 它最终会通过调用链： BQ::ProxyConsumerListener::onFrameAvailable——>ConsumerBase::onFrameAvailable——>FrameAvailableListener::onFrameAvailable——>JNISurfaceTextureContext::onFrameAvailable——>OnFrameAvailableListener.onFrameAvailable 调用到 java 层。
  - 3.当然 OnFrameAvailableListener 回调的东西是由我们自己来写的。 
    - 1.我们知道 SurfaceTexture 的最终目的是将图像流绑定到我们最开始定义的 texture 中去。SurfaceTexture 正好提供了 updateTexImage 方法来刷新 texture。
    - 2.那么我们就在 OnFrameAvailableListener 中直接调用 updateTexImage 吗？这里其实是不一定的。因为我们知道 Surface 的绘制可以在任意线程，也就是说  OnFrameAvailableListener 的回调也是在任意线程中触发的。而 **texture 的更新需要在当初创建这个 texture 的 GL 环境中进行**。此外 Android 中 **GL 环境和线程是一一对应的**。所以只有 Surface 绘制的线程与 GL 环境线程为同一个的时候，我们才能在回调中调用 updateTexImage。
    - 3.不管如何最终还是要调用 updateTexImage 的，我们再来看看它内部是怎么个实现。首先调用链是这样的：SurfaceTexture.updateTexImage——>SurfaceTexture::nativeUpdateTexImage——>GLConsumer::updateTexImage。GLConsumer::updateTexImage 中重要的步骤如下： 
      - 1.调用 acquireBufferLocked 获取 BQ 头部最新的图像内存。
      - 2.调用  glBindTexture 将获取到的图像内存绑定到 texture 中。
      - 3.调用 updateAndReleaseLocked 重置前面获取到的图像内存，然后将其放回到 BQ 的尾部
    - 4.至此我们在 Surface 中绘制的图像流就被 SurfaceTexture 转化成了一个 texture。至于继续对 texture 进行显示和处理之类的事情就可以交给 TV 或者 GLSurfaceView 了。

### 4.TextureView和创建与使用

> 它可以将内容流直接投影到 View 中，可以用于实现视频预览等功能。和 SV 不同，它不会在WMS中单独创建窗口，而是作为 View Hierachy 中的一个普通 View，因此可以和其它普通 View 一样进行移动，旋转，缩放，动画等变化。值得注意的是 TV 必须在硬件加速的窗口中。它显示的内容流数据可以来自 App 进程或是远端进程。这一节我们就来从源码上分析它。**因为 TV 需要硬件加速，所以它最终也是由渲染线程绘制的，而我们在第一章中讲述了 ThreadRender.java、RenderProxy.cpp、RenderThread.cpp 等等类在渲染线程中的作用，所以这一小节关于渲染线程的东西就直接使用而不再赘述了。**



![img](https:////upload-images.jianshu.io/upload_images/2911038-3d955e5ec0726e29.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1000/format/webp)

图9：TextureView创建和使用.png

**和前面一样，本小节接下来的分析也都是顺着图9来的**

- 1.因为 TV 我们可以将其看成一个普通的 View，所以这里我们可以直接从 TV 的 draw 方法开始分析。draw 中有下面这些关键的步骤： 

  - 1.调用 getHardwareLayer 来获取硬件加速层： 
    - 1.ThreadRender.createTextureLayer——>ThreadRender.nCreateTextureLayer——>RenderProxt::createTextureLayer——>new DeferredLayerUpdater.cpp：这里第一个调用链创建了一个 DeferredLayerUpdater.cpp 对象，这个对象会在后面用于 ST 的 texture 更新。
    - 2.ThreadRender,createTextureLayer 中创建完 DeferredLayerUpdater.cpp 还需要调用 HardwareLayer.adoptTextureLayer 将 ThreadRender,java 和 DeferredLayerUpdater.cpp 在 java层封装一下，以便后续使用。
    - 3.创建了 HardwareLayer 后，如果 ST 还没创建的话，那么就会去创建一个 ST。这里的创建流程上一节讲过了也不再赘述。同理这里会调用 nCreateNativeWindow 在 c++ 层创建一个 ANativeWindow(也就是 java 层的 Surface.java)。这个 Surface 主要是为了让 TV 能够提供 lockCanvas 和 unlockCanvasAndPost 这里两个 Api 而创建的。
    - 4.除了初始化 ST 的一系列操作，这里还会调用 HardwareLayer.setSurfaceTexture 为 DeferredLayerUpdater 设置 ST。因为后面 DeferredLayerUpdater 更新 ST 的 texture 的时候需要用到 ST。
    - 5.接下来会调用 applyUpdate 但是因为是初次创建，所以这里先略过。
    - 6.然后会调用 ST.setOnFrameAvailableListener(mUpdateListener) 来为图形流添加监听，而这里的 mUpdateListener 实现就在 TV 里面，这里的实现等我们调用到了再说。
    - 7.至此 ST 其实已经初始化完成了，所以可以调用 mListener.onSurfaceTextureAvailable 来回调外部的代码了。
  - 2.调用 DisplayListCanvas.drawHardwareLayer 在当前 View Hierachay 的 Surface 上面添加绘制一个独立的 layer(简单来说就是一个绘图的区域)。**注意：这里的 layer 与 我们在前面说的 SV 在 SF 中存在的 Layer 是两个不同的概念。 **

- 2.TV 创建好了，接下来我们就可以为其提供图像流。这里我们就用 lockCanvas 和 unlockCanvasAndPost 来提供吧。

  注意：其实 ST 和这里的 TV 不仅仅可以使用上面两个 api 来提供图像流，还可以将 Surface 转化成 EGLSurface 使用 GL 来提供图像流，更常见还有摄像头提供的图像流，这些我在这里就不展开了，交给读者自己去探索。

  - 1.首先 lockCanvas 和 unlockCanvasAndPost 这两个 api 的源码流程和回调流程我就不再赘述了，在 ST 和 Surface 的解析中都有。

  - 2.我们直接看 TV 中 OnFrameAvailableListener 的实现代码。 

    - 1.这里先调用 updateLayer 将 mUpdateLayer 置为 true，表示下一次 VSync 信号过来的时候 TV 需要刷新。**注意 TV 是在 View Hierarchy 中的，所以需要与 View 一起刷新。** 
    - 2.调用 invalidate 触发 ViewRootImpl 的重绘流程。

  - 3.16ms 之后 VSync 信号来了，经过一系列方法之后调用又回到了 TV.draw 中。此时因为 ST 已经创建，所以最主要的代码就是 TV.applyUpdate 方法。 

    - 1.先调用 HardwareLayer.nPrerare——>RenderProxt::pushLayerUpdate 将前面创建的 DeferredLayerUpdater 存入 RenderProxt.cpp 中。
    - 2.调用 HardwareLayer.nUpdateSurfaceTexture 将 DeferredLayerUpdater 中的  mUpdateTexImage 置为 true。**为啥不在这里就直接更新 texture 呢？因为此时的线程还是 ui thread，而 texture 的更新需要在渲染线程做。** 

  - 4.我们回忆一下第二章分析 

    Android绘制机制

     源码，当时我们讲过当 View Hierarchy 的 draw 操作完成之后，经过一系列调用就会进入到渲染线程中。这里我们也不再赘述了，我们直接看 RenderProxt::syncAndDrawFrame——>DrawFrameTask::drawFrame——>DrawFrameTask::syncFrameState 就是在渲染线程中运行的。它会触发我们存储在 RenderProxy.cpp 中的 DeferredLayerUpdater::apply 方法，这个方法有这些关键步骤 

    - 1.GLConsumer::attachToContext，将当前的 ST 与 texture 绑定。
    - 2.GLConsumer::updateTexImage，像前面讲的 ST 一样将图像流更新到 texture 中
    - 3.LayerRenderer::updateTextureLayer

- 3.到这里 TV 从创建到图像流转换的源代码都解析完了，剩下的就是 **Android绘制机制** 中讲的那样使用 GL 进行真正的绘制了。

## 四、总结

> 这篇文章真是写死我了！！过年有三分之二的时间花在这个上面，感觉这篇文章真的是笔者有史以来写过的最有干货的一篇文章了，希望能够对大家有所帮助。

## 连载文章

- [1.从零开始仿写一个抖音app——开始](https://www.jianshu.com/p/e92bd896ac35)
- [4.从零开始仿写一个抖音App——日志和埋点以及后端初步架构](https://www.jianshu.com/p/a957098fe9ea)
- [5.从零开始仿写一个抖音App——app架构更新与网络层定制](https://www.jianshu.com/p/c47eff0c57a7)
- [6.从零开始仿写一个抖音App——音视频开篇](https://www.jianshu.com/p/3f1ca704ba37)
- [7.从零开始仿写一个抖音App——基于FFmpeg的极简视频播放器](https://www.jianshu.com/p/9cf8442fc640)
- [8.从零开始仿写一个抖音App——跨平台视频编辑SDK项目搭建](https://www.jianshu.com/p/8b2f7e20859a)

## 参考文献

- 1.[Android绘制优化——系统显示原理](https://zhuanlan.zhihu.com/p/27344882) 
- 2.[Android的图形概览](https://source.android.com/devices/graphics/arch-sf-hwc) 
- 3.[Choreographer 解析](https://www.jianshu.com/p/dd32ec35db1d) 
- 4.[理解Android硬件加速原理的小白文](https://www.jianshu.com/p/40f660e17a73) 
- 5.[Android硬件加速（二）-RenderThread与OpenGL GPU渲染](https://www.jianshu.com/p/dd800800145b) 
- 6.[Android 5.0(Lollipop)中的SurfaceTexture，TextureView, SurfaceView和GLSurfaceView](https://blog.csdn.net/jinzhuojun/article/details/44062175)