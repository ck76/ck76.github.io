[TOC]

#### Service（服务）是Android中实现程序后台运行的解决方案，它非常适合用于去执行那些不需要和用户交互而且还要求长期运行的任务。Service主要负责与UI无关的工作，比如耗时操作。本篇学习Service相关知识点有：

- Service概要
  - 开启子线程
  - 异步消息处理机制
- Service生命周期
- Service的基本用法
  - 普通Service
  - 前台Service
  - 系统Service
    - 例子：后台定时任务
  - IntentService
- Service与Activity的通信

**1.Service概要**

Service的运行不依赖于任何用户界面，因此即便程序被切换到后台或者用户打开了另一个应用程序，Service仍能够保持正常运行。但当某个应用程序进程被杀掉时，所有依赖于该进程的服务也会停止运行。

实际上Service默认并不会运行在子线程中，也不运行在一个独立的进程中，**它同样执行在主线程中（UI线程）**。换句话说，不要在Service里执行耗时操作，除非你手动打开一个子线程，否则有可能出现主线程被阻塞（ANR）的情况。首先来学习如何打开一个子线程。

（1）开启**子线程**

常用方法是，用Thread类的匿名类的形式并且实现**Runnable接口**，再调用它的**start()**方法，就使得被重写的**run()**方法中的耗时操作运行在子线程当中了。代码如下：



```python
new Thread(new Runnable() {
          @Override
          public void run() {
          //耗时操作的逻辑
          }
}).start();
```

（2）**异步消息处理机制**

还要注意一点：Android不允许在子线程中进行UI操作。但有时候，在子线程里执行一些耗时任务之后需要根据任务的执行结果来更新相应的UI控件，在这里Android提供了一套**异步消息处理机制**，它可以很好地解决在子线程中更新UI的问题。主要用到两个类：**Handler**（处理者，主要用于发送和处理消息）和**Message**（信息，可携带少量信息用于在不同线程之间交换）。下图展示了如何用它们实现从子线程到主线程的转换：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05z32sbgj30s70bktd0.jpg)

可以看到，只要在需要转换到主线程进行UI操作的子线程中实例化一个Message对象并携带相关数据，再由Handler的**sendMessage()**将它发送出去，之后这个数据就会被在主线程中实例化的Handler对象的重写方法**handleMessage()**收到并处理。现在在子线程中更新UI就很容易了。

现在来个具体的例子感受一下，新建布局，这里就放一个文本和按钮：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05z22oixj30tb0a43zj.jpg)

在主活动中按钮的点击事件里开启一个子线程，但又希望点击按钮改变文本内容，此时就用异步消息处理机制，代码如下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05z12e0kj30ss0n0wgg.jpg)

效果如图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05yypx56j30nm06o0td.jpg)

**2.Service生命周期**

官方文档提供的Sevice生命周期图如下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05yyatz0j30bg0ei0u6.jpg)

先来看这几种回调方法含义：
**onCreate（）**：服务第一次被创建时调用
**onStartComand（）**：服务启动时调用
**onBind（）**：服务被绑定时调用
**onUnBind（）**：服务被解绑时调用
**onDestroy（）**：服务停止时调用

从上图可看到有两种方法可以启动Service，下面分别介绍：
第一种：其他组件调用Context的**startService()**方法可以启动一个Service，并回调服务中的onStartCommand()。如果该服务之前还没创建，那么回调的顺序是onCreate()->onStartCommand()。服务启动了之后会一直保持运行状态，直到**stopService()**或**stopSelf()**方法被调用，服务停止并回调onDestroy()。另外，无论调用多少次startService()方法，只需调用**一次**stopService()或stopSelf()方法，服务就会停止了。

第二种：其它组件调用Context的**bindService()**可以绑定一个Service，并回调服务中的onBind()方法。类似地，如果该服务之前还没创建，那么回调的顺序是onCreate()->onBind()。之后，调用方可以获取到onBind()方法里返回的**IBinder对象的实例**，从而实现和服务进行通信。只要调用方和服务之间的连接没有断开，服务就会一直保持运行状态，直到调用了**unbindService()**方法服务会停止，回调顺序onUnBind()->onDestroy()。

注意，这两种启动方法并不冲突，当使用startService()启动Service之后，还可再使用bindService()绑定，只不过需要同时调用 stopService()和 unbindService()方法才能让服务销毁掉。

**3.Service的基本用法**

介绍完Service生命周期和启动方法之后，下面来具体学习一下如何在Activity中启动一个Service。

（1）**普通Service**

第一步：新建类并继承Service且必须重写onBind()方法，有选择的重写onCreate()、onStartCommand()及onDestroy()方法。
第二步：在配置文件中进行注册。学到现在会发现，四大组件除了广播接收器可用动态注册，定义好组件之后都要完成在配置文件注册的这一步。
第三步：在活动中利用Intent可实现Service的启动，代码如下：



```java
 Intent intent = new Intent(this, MyService.class);// MyService是刚刚定义好的Service
 startService(intent);
```

对应的，停止Service方法：



```java
Intent intent = new Intent(this, MyService.class);
stopService(intent);
```

来实战一下！定义一个MyService，重写以下四种方法并都打印一行日志：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05yumfb4j30o40e7t9u.jpg)

在配置文件对MyService进行注册：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05yu0yevj30el01h749.jpg)

准备主活动布局，就放两个按钮用来开启和停止Service，然后设置相应的点击事件：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05ysmvdvj30m707lgm7.jpg)

现在运行程序，顺便检验一下之前学过的Service在启动和停止时调用的方法是不是对的，结果如图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05yrsj0qj30sn09mq54.jpg)

（2）**前台Service**

前台服务和普通服务最大的区别是，前者会一直有一个正在运行的图标在系统的状态栏显示，下拉状态栏后可以看到更加详细的信息，非常类似于通知的效果。使用前台服务或者为了防止服务被回收掉，比如听歌，或者由于特殊的需求，比如实时天气状况。

想要实现一个前台服务非常简单，它和之前学过的发送一个通知非常类似，只不过在构建好一个Notification之后，不需要NotificationManager将通知显示出来，而是调用了**startForeground()**方法。

修改MyService的onCreate()方法：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05yptgwcj30l509b0tj.jpg)

现在重新运行程序，然后点击START SERVICE的按钮，一个前台服务就出现了：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05ylm0uuj30de0deabq.jpg)

（3）**系统Service**

除了自定义一个Service，当然还有现有的系统服务，比如之前接触过的NotificationManage。通过**getSyetemService()**方法并传入一个Name就可以得到相应的服务对象了，常用的系统服务如下表：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05ykcq9zj30s90ffdq5.jpg)

现在再学习一个系统服务AlarmManager，来实现一个后台定时任务。非常简单，调用AlarmManager的**set()**方法就可以设置一个定时任务，并提供三个参数（工作类型，定时任务触发的时间，PendingIntent对象）。下面一一解释以上三个参数：

1）工作类型：有四个值可选，见下图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05yj96q6j30xc0520wj.jpg)

2）定时任务触发的时间：以毫秒为单位，传入值和第一个参数对应关系是：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05yijduhj30o7052t8m.jpg)

3）PendingIntent对象：一般会调用它的**getBroadcast()**方法来获取一个能够执行广播的PendingIntent。这样当定时任务被触发的时候，广播接收器的onReceive()方法就可以得到执行。

接着实战，修改MyService，将前台服务代码都删掉，重写onStartCommand()方法，这里先是获取到了AlarmManager的实例，然后定义任务的触发时间为10秒后，再使用PendingIntent指定处理定时任务的广播接收器为MyReceiver，最后调用 set()方法完成设定，代码如图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05yg3kfaj30qh0bvmya.jpg)

然后定义一个广播接收器为MyReceiver，这里利用Intent对象去启动MyService这个服务。这样做的目的是，一旦启动MyService，就会在onStartCommand()方法里设定一个定时任务，10秒后MyReceiver的onReceive()方法将得到执行，紧接着又启动MyService，反复循环。从而一个能长期在后台进行定时任务的服务就完成了。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05yfl82kj30jm057q38.jpg)

MyReceiver也在配置文件中注册好之后，重新运行，点击START SERVICE的按钮，观察日志的情况：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05ye89q0j30oi08lt9h.jpg)

另外，从Android 4.4版本开始，由于系统在耗电性方面进行了优化使得Alarm任务的触发时间会变得不准确。如果一定要求Alarm任务的执行时间精确，把AlarmManager的**setExact()**方法替代 set()方法就可以了。

（4）**IntentService**

为了可以简单地创建一个**异步的、会自动停止**的服务，Android 专门提供了一个IntentService类。它的使用和普通Service非常像，下面来学习一下：

第一步：新建类并继承IntentService，在这里需要提供一个无参的构造函数且必须在其内部调用父类的有参构造函数，然后具体实现 **onHandleIntent()**方法，在里可以去处理一些耗时操作而不用担心 ANR的问题，因为这个方法已经是在子线程中运行的了。
第二步：在配置文件中进行注册。
第三步：在活动中利用Intent实现IntentService的启动，和Service用的方法是完全一样的。

再来实战，定义一个MyIntentService，准备好无参构造函数，并重写onHandleIntent()方法，这里打印了一行日志，为了证实这个方法确实已经在子线程，又打印了当前线程的id。另外，根据IntentService的特性，这个服务在运行结束后应该是会自动停止的，所以重写onDestroy()方法，在这里也打印了一行日志，以证实服务是不是停止掉了。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05ycsiy4j30o10c5gmf.jpg)

在配置文件对MyIntentService进行注册：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05ybg1igj30dx01iq2q.jpg)

现在在主活动布局再准备一个按钮用来开启这个IntentService，其点击事件代码如图，在这里打印了一下主线程的 id，稍后用于和IntentService进行比对：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05y9yl9mj30ls03dwep.jpg)

运行程序，打印的日志结果，证实了IntentService异步和自动停止：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05y9178pj30s9062jst.jpg)

**4.Service与Activity的通信**

最后来学习如何让Service与Activity进行通信。这就需要借助服务的**onBind()**方法了。比如希望在MyService里提供一个下载功能，然后在活动中可以决定何时开始下载，以及随时查看下载进度。一起学习一下：

第一步：在MyService里自定义一个类MyBinder并继承**Binder**，在它的内部提供了开始下载以及查看下载进度的方法，为了模拟一下，这里就分别打印了一行日志。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05y88jglj30h10bydgh.jpg)

第二步：在MyService的**onBind()**方法里返回刚刚定义好的MyBinder类。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05y658ppj30g706wq38.jpg)

第三步：在活动中实例化一个ServiceConnection类，并重写它的**onServiceConnection()**和**onServiceDisconnection()**方法，这两个方法分别会在活动与服务成功绑定以及解除绑定的时候调用。在 onServiceConnected()方法中，又通过向下转型得到了MyBinder 的实例，有了它活动和服务之间的关系就变得非常紧密了。现在可以在活动中根据具体的场景来调用MyBinder中的任何非private方法了。这里简单调用MyBinder中的两个模拟方法。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05y4yowsj30na0adgmb.jpg)

第四步：在活动布局里再准备两个按钮用于绑定和解绑服务，在它们的点击事件里利用Intent对象实现活动和服务的绑定和解绑。方法是：**bindService()**实现绑定，它接收三个参数（Intent对象，ServiceConnection对象，标志位），这里传入**BIND_AUTO_CREATE**表示在活动和服务进行绑定后自动创建服务。**unbindService()**实现解绑，传入ServiceConnection对象就可以了。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05y425vgj30kf04umxi.jpg)

运行程序，点击一下Bind Service 按钮：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm05y5esplj30uz022glv.jpg)

可以看到MyService的两个模拟方法都得到了执行，说明确实已经在活动里成功调用了服务里提供的方法了。

现在学完四大组件现在可以总结一下了，Activity提供UI界面管理，Service提供与UI无关的服务，ContentProvider用于存储和数据共享，Broadcast解决组件和应用的通信问题，而Intent是将四大组件联结在一起的粘结剂，但彼此之间几乎没有耦合。这种组件化的设计思想使得Android变得非常灵活。

**到这里安卓开发入门整篇已经接近尾声了，小菜鸟终于打开安卓世界的大门，但安卓的精彩远不止此，深入探索之旅也才刚刚开始。纵观全篇，整个入门过程中学习到的基础知识还只停留在会用的阶段，很多要点只是浅谈没有深究，对于基本原理几乎只字未提，所以在入门之后，小菜鸟还会继续深入学习，不定时分享学习笔记，最后感谢和我一起进步的你们！**



作者：厘米姑娘
链接：https://www.jianshu.com/p/1959eb5c99f5
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。