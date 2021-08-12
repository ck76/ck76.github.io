[TOC]

> Looer.loop()方法可能会引起主线程的阻塞，但只要它的消息循环没有被阻塞，能一直处理事件就不会产生ANR异常。
>
>  阻塞是有消息，但是处理事件好事太长。 而这个loop 中的for循环是在没有消息情况下进入一种休眠状态，来了消息是马上就可以处理，所以不算是阻塞
>
> Android应用程序的主线程在进入消息循环过程前，会在内部创建一个Linux管道（Pipe），这个管道的作用是使得Android应用程序主线程在消息队列为空时可以进入空闲等待状态，并且使得当应用程序的消息队列有消息需要处理时唤醒应用程序的主线程。



### 一、为什么会ANR

正如我们所知，在android中如果主线程中进行耗时操作会引发ANR（Application Not Responding）异常。

> 造成ANR的原因一般有两种：
>
> 1. 当前的事件没有机会得到处理（即主线程正在处理前一个事件，没有及时的完成或者looper被某种原因阻塞住了）
> 2. 当前的事件正在处理，但没有及时完成

为了避免ANR异常，android使用了Handler消息处理机制。让耗时操作在子线程运行。

**因此产生了一个问题，主线程中的Looper.loop()一直无限循环检测消息队列中是否有新消息为什么不会造成ANR？**



### 二、消息循环必要性

#### 1、ActivityThread.main()

```java
 public static final void main(String[] args) {
        ... //创建Looper和MessageQueue
        Looper.prepareMainLooper();
        ...//轮询器开始轮询
        Looper.loop();
        ...
    }
```

#### 2、Looper.loop()

```java
while (true) {      
    //取出消息队列的消息，可能会阻塞
       Message msg = queue.next(); 
        // might block
       ... //解析消息，分发消息
       msg.target.dispatchMessage(msg);
       ...
    }
```

显而易见的，如果main方法中没有looper进行循环，那么主线程一运行完毕就会退出。Android应用程序是基于事件驱动的，这样显然不可行。

**总结：ActivityThread的main方法主要就是做消息循环，一旦退出消息循环，那么你的应用也就退出了。**



### 三、为什么这个死循环不会造成ANR

我们知道了消息循环的必要性，那为什么这个死循环不会造成ANR异常呢？

因为Android 的是由事件驱动的，looper.loop() 不断地接收事件、处理事件，每一个点击触摸或者说Activity的生命周期都是运行在 Looper.loop() 的控制之下，如果它停止了，应用也就停止了。**只能是某一个消息或者说对消息的处理阻塞了 Looper.loop()**，而不是 Looper.loop() 阻塞它。

**也就说我们的代码其实就是在这个循环里面去执行的，当然不会阻塞了。**

handleMessage部分源码

```java
		public void handleMessage(Message msg) {
            if (DEBUG_MESSAGES) Slog.v(TAG, ">>> handling: " + codeToString(msg.what));
            switch (msg.what) {
                case LAUNCH_ACTIVITY: {
                    Trace.traceBegin(Trace.TRACE_TAG_ACTIVITY_MANAGER, "activityStart");
                    ActivityClientRecord r = (ActivityClientRecord)msg.obj;

                    r.packageInfo = getPackageInfoNoCheck(
                            r.activityInfo.applicationInfo, r.compatInfo);
                    handleLaunchActivity(r, null);
                    Trace.traceEnd(Trace.TRACE_TAG_ACTIVITY_MANAGER);
                } break;
                case RELAUNCH_ACTIVITY: {
                    Trace.traceBegin(Trace.TRACE_TAG_ACTIVITY_MANAGER, "activityRestart");
                    ActivityClientRecord r = (ActivityClientRecord)msg.obj;
                    handleRelaunchActivity(r);
                    Trace.traceEnd(Trace.TRACE_TAG_ACTIVITY_MANAGER);
                } break;
                case PAUSE_ACTIVITY:
            //........
```

可以看见Activity的生命周期都是依靠主线程的Looper.loop，当收到不同Message时则采用相应措施。

如果某个消息处理时间过长，比如你在onCreate(),onResume()里面处理耗时操作，那么下一次的消息比如用户的点击事件不能处理了，整个循环就会产生卡顿，时间一长就成了ANR。

让我们再看一遍造成ANR的原因，你可能就懂了。

> 造成ANR的原因一般有两种：
>
> 1. 当前的事件没有机会得到处理（即主线程正在处理前一个事件，没有及时的完成或者looper被某种原因阻塞住了）
> 2. 当前的事件正在处理，但没有及时完成

而且主线程Looper从消息队列读取消息，当读完所有消息时，主线程阻塞。子线程往消息队列发送消息，并且往管道文件写数据，主线程即被唤醒，从管道文件读取数据，主线程被唤醒只是为了读取消息，当消息读取完毕，再次睡眠。因此loop的循环并不会对CPU性能有过多的消耗。

**总结：Looer.loop()方法可能会引起主线程的阻塞，但只要它的消息循环没有被阻塞，能一直处理事件就不会产生ANR异常。**



### 知乎总结1：

> 简单一句话是：Android应用程序的主线程在进入消息循环过程前，会在内部创建一个Linux管道（Pipe），这个管道的作用是使得Android应用程序主线程在消息队列为空时可以进入空闲等待状态，并且使得当应用程序的消息队列有消息需要处理时唤醒应用程序的主线程。

这一题是需要从消息循环、消息发送和消息处理三个部分理解Android应用程序的消息处理机制了，这里我对一些要点作一个总结：

A. Android应用程序的消息处理机制由消息循环、消息发送和消息处理三个部分组成的。

B. Android应用程序的主线程在进入消息循环过程前，会在内部创建一个Linux管道（Pipe），这个管道的作用是使得Android应用程序主线程在消息队列为空时可以进入空闲等待状态，并且使得当应用程序的消息队列有消息需要处理时唤醒应用程序的主线程。

C. Android应用程序的主线程进入空闲等待状态的方式实际上就是在管道的**读端等待管道中有新的内容可读**，具体来说就是是通过Linux系统的Epoll机制中的epoll_wait函数进行的。

D. 当往Android应用程序的消息队列中加入新的消息时，**会同时往管道中的写端写入内容**，通过这种方式就可以唤醒正在等待消息到来的应用程序主线程。

E. 当应用程序主线程在进入空闲等待前，会认为当前线程处理空闲状态，于是就会调用那些已经注册了的IdleHandler接口，使得应用程序有机会在空闲的时候处理一些事情



### 知乎总结2：

1.handler机制是使用pipe来实现的 

2.主线程没有消息处理时阻塞在管道的读端 

3.binder线程会往主线程消息队列里添加消息，然后往管道写端写一个字节，这样就能唤醒主线程从管道读端返回，也就是说queue.next()会调用返回

4.dispatchMessage()中调用onCreate, onResume



### 知乎总结3：

了解下linux的epoll你就知道为什么不会被卡住了，先说结论：阻塞是有的，但是不会卡住
主要原因有2个

1，epoll模型
当没有消息的时候会epoll.wait，等待句柄写的时候再唤醒，这个时候其实是阻塞的。

2，所有的ui操作都通过handler来发消息操作。

比如屏幕刷新16ms一个消息，你的各种点击事件，所以就会有句柄写操作，唤醒上文的wait操作，所以不会被卡死了。



### 链接

- [GitYuan大神的解答](https://www.zhihu.com/question/34652589)