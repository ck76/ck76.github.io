[TOC]

**“可以毫不夸张的说，Android的framework层主要是由WMS、AMS还有View所构成，这三个模块穿插交互在整个framework中，掌握了它们之间的关系和每一个逻辑步骤，你对framework的了解至少有百分之五十”。这是《Android源码与设计模式》作者的原话，最近学习WMS和AMS相关知识，的确非常复杂，这篇文章是在学习之初的初步整理，虽然内容不够细致，但对其也算有个宏观的了解。难点是其中具体的代码逻辑，也都在相应框架下给出了参考链接，日后的工作就是详细学习其中的代码逻辑，让整个框架在脑海中越来越细致、清晰。**

# AMS与WMS

在没有深入了解AMS与WMS前，对它俩负责的功能一直很模糊，一直搞不清区别。所以在深入理解其中之一时，先对它们在Android中各自负责的任务有个基本的了解和区分。

此外，AMS和WMS都属于Android中的系统服务，系统服务有很多，它们在Android体系架构中都属于同一层次，所以最好在深入了解它们各自的运行机理前，对系统的宏观架构能够有所掌握，也就是在自己的脑海中能够搭建一个简单的模型和框架，搞清楚它们在系统中所扮演的角色。每当我们学习了一个新的知识点，都是对这个模型的填充和细化，并能够较好的与之前所学的知识结合起来，既有利于理解，也有利于记忆，而且还能够感受到自己的积累与进步。

下面先简单对AMS与WMS作个简单的介绍，以解我之前的疑惑。

**Activity与WIndow：**

- Activity只负责生命周期和事件处理
- Window只控制视图
- 一个Activity包含一个Window，如果Activity没有Window，那就相当于Service

**AMS与WMS：**

- AMS统一调度所有应用程序的Activity
- WMS控制所有Window的显示与隐藏以及要显示的位置

在视图层次中，Activity在WIndow之上，如下图，直接截取自我的另一篇博客：



![img](https:////upload-images.jianshu.io/upload_images/2412005-b52e5a25eaf57fbc.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/504)

Android应用程序窗口模型

下面进入到对WMS的介绍。

# WMS

## 基础了解

WindowManagerService服务的实现是相当复杂的，毕竟它要管理的整个系统所有窗口的UI，而在任何一个系统中，窗口管理子系统都是极其复杂的。

### 作用

- 为所有窗口分配Surface。客户端向WMS添加一个窗口的过程，其实就是WMS为其分配一块Suiface的过程，一块块Surface在WMS的管理下有序的排布在屏幕上。Window的本质就是Surface。
- 管理Surface的显示顺序、尺寸、位置
- 管理窗口动画
- 输入系统相关：WMS是派发系统按键和触摸消息的最佳人选，当接收到一个触摸事件，它需要寻找一个最合适的窗口来处理消息，而WMS是窗口的管理者，系统中所有的窗口状态和信息都在其掌握之中，完成这一工作不在话下。

### 什么是Window

“Window”表明它是和窗口相关的，“窗口”是一个抽象的概念：

- 从用户的角度来讲，它是一个“界面”；
- 从SurfaceFlinger的角度来看，它是一个Layer，承载着和界面有关的数据和属性；
- 从WMS角度来看，它是一个WIndowState，用于管理和界面有关的状态。

在《深入理解Android内核设计思想》一书中看到一个比喻非常好，整个界面就像由N个演员参与的话剧：SurfaceFling是摄像机，它只负责客观的捕捉当前的画面，然后真实的呈现给观众；WMS就是导演，它要负责话剧的舞台效果、演员站位；ViewRoot就是各个演员的长相和表情，取决于它们各自的条件与努力。可见，WMS与SurfaceFling的一个重要区别就是——后者只做与“显示”相关的事情，而WMS要处理对输入事件的派发。

Android支持的窗口类型很多，统一可以分为三大类，另外各个种类下还细分为若干子类型，且都在`WindowManager.java`中有定义。

- **Application Window**
- **SystemWindow**
- **Sub Window**

## 类间关系

上面那些都是对WMS相关功能的介绍，对WMS有个感性的认识，现在开始进入相关类。

### 使用的设计模式

- **桥接模式**

关于桥接模式这里就不再讲解，参考《Android源码设计模式》第24章。

### UML



![img](https:////upload-images.jianshu.io/upload_images/2412005-963e4a2a5070a624.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1000)



## 代码逻辑

### 启动流程

………………
………………
………………

### 工作流程

- **窗口大小和位置（X轴和Y轴）的计算过程**
- **窗口的组织方式**
- **输入法窗口的调整过程**
- **壁纸窗口的调整过程**
- **窗口Z轴位置的计算和调整过程**
- **Activity窗口的启动窗口的显示过程**
- **Activity窗口的切换过程**
- **Activity窗口的动画显示过程**

## 解惑

Q：WMS是系统服务，有SystemServer负责启动，启动时机相对较晚，那么在WMS运行之前，终端显示屏就一团黑？

A：在WMS启动之前，系统只需显示开机动画，它们都有特殊的方式来向屏幕输出图像，比如直接通过OpenGL ES与SurfaceFling的配合来完成。这也从侧面告诉我们，要想在Android上显示UI，并不一定要通过WMS。

# AMS

## 基础了解

### 作用

- 统一调度所有应用程序的Activity的生命周期
- 启动或杀死应用程序的进程
- 启动并调度Service的生命周期
- 注册BroadcastReceiver，并接收和分发Broadcast
- 启动并发布ContentProvider
- 调度task
- 处理应用程序的Crash
- 查询系统当前运行状态

## 类间关系

### 设计模式

- **代理模式**

关于代理模式这里就不再讲解，参考《Android源码设计模式》第18章。

**WMS与AMS设计模式的不同的思考…………**

### UML（类间关系）



![img](https:////upload-images.jianshu.io/upload_images/2412005-0efd7de9446583c1.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/756)



从类图可以看出，`ActivityManagerProxy`和`ActivityManagerNative`都实现了`IActivityManager`，`ActivityManagerProxy`就是代理部分，`ActivityManagerNative`就是实现部分，但`ActivityManagerNative`是个抽象类，并不处理过多的具体逻辑，大部分具体逻辑是由`ActivityManagerService`承担，这就是为什么我们说真实部分应该为`ActivityManagerService`。

## 代码逻辑

### 启动流程

- **第一阶段：**启动`ActivityManagerService`。
- **第二阶段：**调用`setSystemProcess`。
- **第三阶段：**调用`installSystemProviders`方法。
- **第四阶段：**调用`systemReady`方法。

这里只给出了非常粗糙的整体流程，也没有深入的去了解，是因为觉得目前还用不着去理解这些内容，工作时用不到，而且即使现在明白了，如果不用，很快也会忘记。

如果想深入了解，可以参考《Android的设计与实现》，讲的挺好的，思路非常清晰。

### 工作流程

AMS的工作流程，其实就是Activity的启动和调度的过程，所有的启动方式，最终都是通过Binder机制的Client端，调用Server端的AMS的`startActivityXXX()`系列方法。所以可见，工作流程又包括Client端和Server端两个。

#### Client端流程

- Launcher主线程捕获`onClick()`点击事件后，调用`Launcher.startActivitySafely()`方法。`Launcher.startActivitySafely()`内部调用了Launcher.startActivity()方法，`Launcher.startActivity()`内部调用了Launcher的父类Activity的`startActivity()`方法。
- `Activity.startActivity()`调用`Activity.startActivityForResult()`方法，传入该方法的requestCode参数若为-1，则表示Activity启动成功后，不需要执行`Launcher.onActivityResult()`方法处理返回结果。
- 启动Activity需要与系统ActivityManagerService交互，必须纳入Instrumentation的监控，因此需要将启动请求转交instrumentation，即调用`Instrumentation.execStartActivity()`方法。
- `Instrumentation.execStartActivity()`首先通过ActivityMonitor检查启动请求，然后调用`ActivityManagerNative.getDefault()`得到ActivityManagerProxy代理对象，进而调用该代理对象的`startActivity()`方法。
- ActivityManagerProxy是ActivityManagerService的代理对象，因此其内部存储的是BinderProxy，调用`ActivityManagerProxy.startActivity()`实质是调用`BinderProxy.transact()`向Binder驱动发送START_ACTIVITY_TRANSACTION命令。Binder驱动将处理逻辑从Launcher所在进程切换到ActivityManagerService所在进程。

#### Server端流程

启动Activity的请求从Client端传递给Server端后，便进入了启动应用的七个阶段，这里也是整理出具体流程，细节可以参考《Android的设计与实现》第十一章内容。

**1）预启动**

- ActivityManagerService.startActivity()
- ActivityStack.startActivityMayWait()
- ActivityStack.startActivityLocked()
- ActivityStack.startActivityUncheckedLocked()
- ActivityStack.startActivityLocked()（重载）
- ActivityStack.resumeTopActivityLocked()

**2）暂停**

- ActivityStack.startPausingLocked()
- ApplicationThreadProxy.schedulePauseActivity()
- ActivityThread.handlePauseActivity()
- ActivityThread.performPauseActivity()
- ActivityManagerProxy.activityPaused()
- completePausedLocked()

**3）启动应用程序进程**

- 第二次进入ActivityStack.resumeTopActivityLocked()
- ActivityStack.startSpecificActivityLocked()
- startProcessLocked()
- startProcessLocked()（重载）
- Process.start()

**4）加载应用程序Activity**

- ActivityThread.main()
- ActivityThread.attach()
- ActivityManagerService.attachApplication()
- ApplicationThread.bindApplication()
- ActivityThread.handleBindApplication()

**5）显示Activity**

- ActivityStack.realStartActivityLocked()
- ApplicationThread.scheduleLaunchActivity()
- ActivityThead.handleLaunchActivity()
- ActivityThread.performLaunchActivity()
- ActivityThread.handleResumeActivity()
- ActivityThread.performResumeActivity()
- Activity.performResume()
- ActivityStack.completeResumeLocked()

**6）Activity Idle状态的处理**

**7）停止源Activity**

- ActivityStack.stopActivityLocked()
- ApplicationThreadProxy.scheduleStopActivity()
- ActivityThread.handleStopActivity()
- ActivityThread.performStopActivityInner()

作者：thinkChao

链接：https://www.jianshu.com/p/47eca41428d6

來源：简书

简书著作权归作者所有，任何形式的转载都请联系作者获得授权并注明出处。