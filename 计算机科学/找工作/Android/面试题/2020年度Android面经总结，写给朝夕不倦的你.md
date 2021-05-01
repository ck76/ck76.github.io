## 前言

转眼间2020就接近尾声了，年后有跳槽想法的小伙伴们心里应该也有自己的决定了。金三银四青铜五，明年形势严峻，切勿临时抱佛脚。在博主认为，对于Android面试以及进阶的最佳学习方法莫过于刷题+博客+书籍+总结，前三者博主将淋漓尽致地挥毫于这篇博客文章中，至于总结在于个人，实际上越到后面你会发现面试并不难，其次就是在刷题的过程中有没有去思考，刷题只是次之，这又是一个层次了，这里暂时不提后面再谈。

其实Android开发的知识点就那么多，面试问来问去还是那么点东西。所以面试没有其他的诀窍，只看你对这些知识点准备的充分程度。so，出去面试时先看看自己复习到了哪个阶段就好。

下面开始进入正文，以下是我进阶学习所积累的历年腾讯、头条、阿里、美团、字节跳动等公司2020年度的高频面试题，希望对你有帮助。

# 一、计算机网络

1、Tcp和Udp的区别？
2、TCP可靠传输原理实现（滑动窗口）。
3、描述TCP三次握手与四次挥手的过程与意义。
4、Http与Https的关系是什么？
5、Http1.1和Http1.0及2.0的区别
6、Http的报文结构。
7、HTTPS 如何防范中间人攻击？
8、Http的请求方法。
9、Https加密原理。
10、网络请求缓存处理，okhttp如何处理网络缓存的
11、权限管理系统（底层的权限是如何进行 grant 的）？
12、Cookie与Session的作用和原理。
13、client如何确定自己发送的消息被server收到
14、浏览器输入地址到反馈结果发生了什么？



![img](https:////upload-images.jianshu.io/upload_images/22976303-7b704531a6da6cb5.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

# 二、关于数据结构与算法

算法可以说是现在找工作必须的知识储备，具体得看公司的业务。以我的面试经验来看，总体来说问的不多，还有些公司基本不问算法。

但是如果去面试字节，网易，快手这种每轮必问算法的公司，因为算法题拿不到offer就很可惜了。

算法题就好像高考语文的古诗词默写一样，分不多，但丢了就很可惜了。

主要还是平时力扣的刷题积累

大多公司业务涉及到的算法其实不多，但大厂的面试官很喜欢问算法，其实就是想考验/判断，面试者是否聪明、勤奋、逻辑性怎么样、培养潜力大不大。（知识面广、开源库源码看的多）

#### 刷题技巧

1、多刷题，所有题目只靠自己写出来，不是看代码写出来，而是自己不看代码能写出来。

2、多总结，重点总结如下两个方面： 笔试考的多算法：数组、链表、队列、栈、排序、查找、串、图（图形比较难，问的不多）、二叉树 面试问的多算法：Hashmap、LinkedList、ArrayList

## 大厂经典算法题

#### 数组

如何在一个1到100的整数数组中找到丢失的数字？（Google、腾讯）

#### 链表

一个数组插入删除查找和链表的效率对比？如果一个数组要反复插入删除怎能优化降低时间复杂度？（腾讯）

如何在不使用递归的情况下逆转单链表？（小米、快手）

#### 队列&堆栈

一个送礼的场景，礼物各有权重属性，怎么根据权重对礼物进行处理，然后再排队分发，每次取一个礼物，怎么设计数据结构（喜马拉雅、美团）

#### 二叉树

已知前序遍历为{1,2,4,7,3,5,6,8}，中序遍历为{4,7,2,1,5,3,8,6}，他的二叉树是怎样的（58同城）

#### 排序

top-k排序（堆排序，位图法）美团

#### 查找

设计一个算法，已知某年某月某日是星期几，求另外年月日对应的是星期几（华为）

#### 串

编写一个函数来查找字符串数组中的最长公共前缀

#### Hashmap常问

1.HashMap的底层原理？线程安全吗？（百度）
2.HashMap中的put是怎么实现的？（滴滴）
3.讲一下HashMap中什么时候需要进行扩容，扩容resize()是如何实现的？（阿里）
4.什么是哈希碰撞？怎么解决（美团）
5.HashMap和HashTable的区别（小米）
6.hashmap concurrenthashmap原理（美团）
7.ArrayList和hashmap的区别，为什么取数更快？

**以上真题节选自《一线互联网大厂面试数据结构与算法经典70题》 需要的下伙伴可以在[我的GitHub](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FTimdk857%2FAndroid-Architecture-knowledge-2-)免费下载获取**

![img](https:////upload-images.jianshu.io/upload_images/22976303-b1b1f2c6ef9afe2b.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

# 三、Java面试题

## Java基础

1、介绍下GC回收机制与分代回收策略。
2、如果让你写一段栈溢出的代码你会什么写，一个栈大概有多大，为什么？每个线程都有这样大小的一个栈吗？
3、Java中有几种引用关系，它们的区别是什么？
4、什么情况下会发生栈内存溢出？
5、JVM中一次完整的GC流程是怎样的，对象如何晋升到老年代？
6、Jvm内存 结构说一下。
7、GC收集[算法]()有哪些？它们的特点是什么？
8、描述GC机制。Class会不会回收？用不到的Class怎么回收？(东方头条)
9、如何判断一个对象是否被回收，有哪些GC[算法]()，实际虚拟机使用最多的是什么GC[算法]()？
10、JVM DVM ART的区别（360）
11、请描述new一个对象的流程。
12、StackOverFlow与OOM的区别？分别发生在什么时候，JVM栈中存储的是什么，堆存储的是什么？
13、String， Stringbuffer， StringBuilder 的区别是什么？（东方头条）
14、Java虚拟机和Dalvik虚拟机的区别？
15、Java对象会不会分配到栈中？
16、抽象类和接口的区别。
17、String为什么是不可变的？
18、什么是值传递和引用传递，Java 是值传递还是引用传递？
19、final 、finally、finalize 区别。
20、重载和重写的区别 （[京东]()）
21、try-catch-finally，try里有return，finally还执行么？
22、String s = new String(“”);创建了几个对象?
23、Static class 与non static class的区别。
24、java里 equals和== 区别。
25、Excption与Error区别。
26、描述JVM类加载过程。
27、.PathClassLoader与DexClassLoader的区别是什么？
28、动态代理的方法怎么初始化的？
29、什么是双亲委托机制，为什么需要双亲委托机制？
30、动态代理是什么？如何实现？
......

## 并发编程

1、HandlerThread是什么？
2、AQS原理 （[小米](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.nowcoder.com%2Fjump%2Fsuper-jump%2Fword%3Fword%3D%E5%B0%8F%E7%B1%B3) [京东](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.nowcoder.com%2Fjump%2Fsuper-jump%2Fword%3Fword%3D%E4%BA%AC%E4%B8%9C)）
3、volatile关键字干了什么？（什么叫指令重排）
4、Synchronized的原理以及与ReentrantLock的区别。（360）
5、Synchronized在JDK1.8之后做了哪些优化
6、什么是守护线程？你是如何退出一个线程的？
7、实现非阻塞式生产者消费者
8、AsyncTask中的任务是串行的还是并行的？
9、AyncTask的原理是什么。
10、有三个线程T1，T2，T3，怎么确保它们按顺序执行？
11、如何开启一个线程，开启大量线程会有什么问题，如何优化？
12、volatile和synchronize有什么区别？
13、sleep 、wait、yield 的区别，wait 的线程如何唤醒它？
14、ReentrantLock的实现原理。
15、假如只有一个cpu，单核，多线程还有用吗 ？
16、Synchronized static与非static锁的区别和范围
17、Android中操作多线程的方式有哪些？
18、怎样获取当前线程是否是主线程
19、HandlerThread是什么？
20、线程间如何通信？
21、RxJava线程切换原理，RxJava1和RxJava2的区别有哪些？

![img](https:////upload-images.jianshu.io/upload_images/22976303-c6eea73ad729a702.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

# 四、Android相关

## Android基础

1、Acitvity的生命周期是什么样的？
2、Activity的4大启动模式，与开发中需要注意的问题，如onNewIntent() 的调用；
3、Intent显示跳转与隐式跳转，如何使用？
4、Activity A跳转B，B跳转C，A不能直接跳转到C，A如何传递消息给C？
5、Activity如何保存状态的？
6、Fragment add与replace的区别，分别对Fragment的生命周期影响
7、 Fragment的构造函数为啥不让传参？（B站）
8、Fragment的生命周期？
9、Application、Activity、Service中context的区别？能否启动一个activity、dialog?
10、什么是有序广播？
11、请描诉Activity的启动流程，从点击图标开始。(B站)
12、 Service的生命周期是什么样的？
13、你会在什么情况下使用Service？
14、startServer和bindServier的区别？
15、Service和Thread的区别？
16、可以再onReceive中开启线程么，会有什么问题？
17、 广播的分类与工作原理
18、 BroadcastReciver的静态注册与动态注册的区别？
19、 ContentProvider如何自定义与使用场景是什么？
20、IntentService与Service的区别？

## view相关

1、如何优化自定义View
2、Android属性动画实现原理，补间动画实现原理
3、View绘制流程与自定义View注意点。
4、在onResume中可以测量宽高么
5、事件分发机制是什么过程？（东方头条）
6、View分发反向制约的方法？
7、自定义Behavior，NestScroll，NestChild。（东方头条）
8、View.inflater过程与异步inflater（东方头条）
9、.inflater为什么比自定义View慢？（东方头条）
10、onTouchListener onTouchEvent onClick的执行顺序。
11、Requestlayout，onlayout，onDraw，DrawChild区别与联系
12、一个ListView或者一个RecyclerView在显示新闻数据的时候，出现图片错位，可能的原因有哪些 & 如何解决？
13、ScrollView下嵌套一个RecycleView通常会出现什么问题？
14、如何对ListView & RecycleView进行局部刷新的？
15、如何给ListView & RecyclerView加上拉刷新 & 下拉加载更多机制
16、怎么拦截事件 onTouchEvent如果返回false onClick还会执行么？
17、事件的分发机制，责任链模式的优缺点
18、动画的分类以及区别
19、属性动画与普通的动画有什么区别？
20、插值器 估值器的区别
21、RecyclerView与ListView的对比，缓存策略，优缺点。
22、RecyclerView的回收复用机制
23、RecyclerView是什么？如何使用？如何返回不一样的Item
24、WindowMangerService中token到底是什么？有什么区别
25、为什么Dialog不能用Application的Context？
26、如何通过WindowManager添加Window(代码实现)？
27、DecorView, ViewRootImpl,View之间的关系，ViewGroup.add()会多添加一个ViewrootImpl吗
28、自定义View执行invalidate()方法,为什么有时候不会回调onDraw()
29、Android中的动画有哪些? 动画占用大量内存，如何优化
30、.ViewHolder为什么要被声明成静态内部类
31、.ListView卡顿的原因以及优化策略
32、.如何实现Activity窗口快速变暗
33、Activity,Window,View三者的联系和区别
34、View的绘制流程是从Activity的哪个生命周期方法开始执行的
35、invalidate() 和 postInvalicate() 区别
36、View的滑动方式
37、RecyclerView与ListView的对比，缓存策略，优缺点
.......

## Android Framework

1、LiveData的生命周期如何监听的?(B站)
2、打开多个页面，如何实现一键退出?
3、如果需要在Activity间传递大量的数据怎么办？
4、Android中多进程通信的方式有哪些？
5、描述下Binder机制原理？（东方头条）
6、Binder线程池的工作过程是什么样？
7、Handler怎么进行线程通信，原理是什么？（东方头条）
8、Handler如果没有消息处理是阻塞的还是非阻塞的？
9、handler.post(Runnable) runnable是如何执行的？
10、handler的Callback和handlemessage都存在，但callback返回true handleMessage还会执行么？
11、Handler的sendMessage和postDelay的区别？
12、IdleHandler是什么？怎么使用，能解决什么问题？
13、为什么Looper.loop不阻塞主线程？Looper无限循环为啥没有ANR（B站）
14、Looper如何在子线程中创建？
15、Looper、handler、线程间的关系。例如一个线程可以有几个Looper可以对应几个Handler？
16、如何更新UI，为什么子线程不能更新UI？
17、ThreadLocal的原理，以及在Looper是如何应用的？
18、Android 有哪些存储数据的方式？
19、SharedPreference原理，commit与apply的区别是什么？使用时需要有哪些注意？
20、如何判断一个 APP 在前台还是后台？
21、一张图片100x100在内存中的大小？
.......

## Android第三方库源码

1、网络底层框架：OkHttp实现原理
2、OKhttp针对网络层有哪些优化？
3、网络请求缓存处理，okhttp如何处理网络缓存的？
4、从网络加载一个10M的图片，说下注意事项？
5、网络封装框架：Retrofit实现原理
6、响应式编程框架：RxJava实现原理
7、图片加载框架：Glide实现原理
8、Glide如何确定图片加载完毕？
9、Glide内存缓存如何控制大小？
10、加载bitmap过程（怎样保证不产生内存溢出）
11、Android中软引用与弱引用的应用场景。
12、LruCache原理
13、Fresco与Glide的对比：
14、Bitmap如何处理大图，如一张30M的大图，如何预防OOM?
15、事件总线框架EventBus实现原理
16、内存泄漏检测框架：LeakCanary实现原理
17、leakCannary中如何判断一个对象是否被回收？
18、依赖注入框架：ButterKnife实现原理
19、依赖全局管理框架：Dagger2实现原理
20、数据库框架：GreenDao实现原理
…...

## Android APP性能优化

这个主要结合你所做过的[项目]()问，一般面试官会问你[项目]()做过哪些方面的优化，常见的问法就是：
优化的思路是什么
用到了哪些技术
遇到了哪些困难（问题）
如何解决
有什么心得
主要结合自己[项目]()回答。切记不要自己给自己挖坑，比如没有做过这方面优化，你为了表现自己，说做过，结果面试官往下问回答不出来，这关乎诚信问题，你们懂的



作者：Android高级架构
链接：https://www.jianshu.com/p/8237bc503ea3
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。