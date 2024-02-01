[TOC]







### 流程图1

![activity启动流程](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/MUYzKb2VMc60pv0UeSNXJee8PH.E4th2Ja3VFKxq*ps!/r/dLYAAAAAAAAA)

---

### 流程图2

![1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Yt*P3LCgEBUm6eu3jdG8KN3f6n*irjs5TVCL6UPo3w8!/r/dFYBAAAAAAAA)

----

### 流程图3

![2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/PeNENp.aDT*rOp1QW5cS9ElPNpv*LgsjB2cCzFcRnYE!/r/dL8AAAAAAAAA)

-----

### 流程图4

![3](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/2jWXA7Dpt5YFpq0rarkDMOYzrzcEkGtZqPZ0063PKcU!/r/dFIBAAAAAAAA)

-----

### 流程图5

![4](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/28iDtv6imOdWfM*DG6lfJsE7FHrH.jeW7IoTq27MMKw!/r/dDUBAAAAAAAA)

----



### 主要对象功能介绍

- ActivityManagerServices，简称AMS，服务端对象，负责系统中所有Activity的生命周期
- ActivityThread，App的真正入口。当开启App之后，会调用main()开始运行，开启消息循环队列，这就是传说中的UI线程或者叫主线程。与ActivityManagerServices配合，一起完成Activity的管理
- ApplicationThread，用来实现ActivityManagerService与ActivityThread之间的交互
- ActivityManagerService需要管理相关Application中的Activity的生命周期时，通过ApplicationThread的代理对象与ActivityThread通讯
- ApplicationThreadProxy，是ApplicationThread在服务器端的代理，负责和客户端的ApplicationThread通讯。AMS就是通过该代理与ActivityThread进行通信的。
- Instrumentation，每一个应用程序只有一个Instrumentation对象，每个Activity内都有一个对该对象的引用。Instrumentation可以理解为应用进程的管家，ActivityThread要创建或暂停某个Activity时，都需要通过Instrumentation来进行具体的操作。
- ActivityStack，Activity在AMS的栈管理，用来记录已经启动的Activity的先后关系，状态信息等。通过ActivityStack决定是否需要启动新的进程。
- ActivityRecord，ActivityStack的管理对象，每个Activity在AMS对应一个ActivityRecord，来记录Activity的状态以及其他的管理信息。其实就是服务器端的Activity对象的映像。
- TaskRecord，AMS抽象出来的一个“任务”的概念，是记录ActivityRecord的栈，一个“Task”包含若干个ActivityRecord。AMS用TaskRecord确保Activity启动和退出的顺序。如果你清楚Activity的4种launchMode，那么对这个概念应该不陌生。



### 链接

- https://blog.csdn.net/zhaokaiqiang1992/article/details/49428287