https://www.jianshu.com/p/2dd855aa1938

- [Animation](https://www.jianshu.com/p/10dc575896d3)

```
Q：Android中有哪几种类型的动画？
```

> - 技术点：动画类型
> - 参考回答： 常见三类动画
>   - View动画（View Animation）/补间动画（Tween animation）：对View进行平移、缩放、旋转和透明度变化的动画，不能真正的改变view的位置。应用如布局动画、Activity切换动画
>   - 逐帧动画（Drawable Animation）：是View动画的一种，它会按照顺序播放一组预先定义好的图片
>   - 属性动画（Property Animation）：对该类对象进行动画操作，真正改变了对象的属性

```
Q：帧动画在使用时需要注意什么？
```

> - 技术点：帧动画
> - 参考回答：使用祯动画要注意不能使用尺寸过大的图片，否则容易造成OOM

```
Q：View动画和属性动画的区别？
```

> - 技术点：View动画、属性动画
>
> - 
>
>   
>
>   ![image-20201208233239499](https://tva1.sinaimg.cn/large/0081Kckwly1glgvby4qc4j30xm0mg7gv.jpg)

```
Q：View动画为何不能真正改变View的位置？而属性动画为何可以？
```

> - 技术点：View动画
> - 参考回答：View动画改变的只是View的显示，而没有改变View的响应区域；而属性动画会通过反射技术来获取和执行属性的get、set方法，从而改变了对象位置的属性值。

```
Q：属性动画插值器和估值器的作用？
```

> - 技术点：属性动画
>
> - 参考回答：
>
>   - 插值器(Interpolator)
>
>     ：根据
>
>     时间流逝的百分比
>
>     计算出当前
>
>     属性值改变的百分比
>
>     。确定了动画效果变化的模式，如匀速变化、加速变化等等。View动画和属性动画均可使用。常用的系统内置插值器：
>
>     - 线性插值器(LinearInterpolator)：匀速动画
>     - 加速减速插值器(AccelerateDecelerateInterpolator)：动画两头慢中间快
>     - 减速插值器(DecelerateInterpolator)：动画越来越慢
>
>   - 类型估值器(TypeEvaluator)
>
>     ：根据当前
>
>     属性改变的百分比
>
>     计算出
>
>     改变后的属性值
>
>     。针对于属性动画，View动画不需要类型估值器。常用的系统内置的估值器：
>
>     - 整形估值器(IntEvaluator)
>     - 浮点型估值器(FloatEvaluator)
>     - Color属性估值器(ArgbEvaluator)

- [Window](https://www.jianshu.com/p/ed03aed9a4db)

```
Q：Activity、View、Window三者之间的关系？
```

> - 技术点：Activity、View、Window联系
> - 思路：围绕Window是Activity和View的桥梁展开
> - 参考回答：在Activity启动过程其中的**attach()方法**中初始化了PhoneWindow，而PhoneWindow是Window的唯一实现类，然后Activity通过setContentView将View设置到了PhoneWindow上，而View通过**WindowManager**的**addView()、**removeView()、updateViewLayout()对View进行管理。

```
Q：Window有哪几种类型？
```

> - 技术点：Window类型
> - 参考回答：Window有三种类型：
>   - **应用Window**：对应一个Activity。
>   - **子Window**：不能单独存在，需附属特定的父Window。如Dialog。
>   - **系统Window**： 需申明权限才能创建。如Toast。

```
Q：Activity创建和Dialog创建过程的异同？
```

> - 技术点：Window创建
>
> - 参考回答：
>
>   Dialog
>
>   的Window创建过程：
>
>   - 创建WindowDialog。和Activity类似，同样是通过**PolicyManager.makeNewWindow()**  来实现。
>   - 初始化DecorView并将Dialog的视图添加到DecorView中去。和Activity类似，同样是通过**Window.setContentView()** 来实现。
>   - 将DecorView添加到Window中显示。和Activity一样，都是在自身要出现在前台时才会将添加Window。
>     - **Dialog.show()** 方法：完成DecorView的显示。
>     - **WindowManager.remoteViewImmediate()** 方法：当Dialog被dismiss时移除DecorView。

- [Handler](https://www.jianshu.com/p/1c79fb5296b6)

```
Q：谈谈消息机制Hander？作用？有哪些要素？流程是怎样的？
```

> - 技术点：消息机制
>
> - 参考回答：
>
>   - 作用：**跨线程通信**。当子线程中进行耗时操作后需要更新UI时，通过Handler将有关UI的操作切换到主线程中执行。
>
>   - 四要素：
>
>     - **Message（消息）**：需要被传递的消息，其中包含了消息ID，消息处理对象以及处理的数据等，由MessageQueue统一列队，最终由Handler处理。
>     - **MessageQueue（消息队列）**：用来存放Handler发送过来的消息，内部通过**单链表**的数据结构来维护消息列表，等待Looper的抽取。
>     - **Handler（处理者）**：负责Message的发送及处理。通过 Handler.sendMessage() 向消息池发送各种消息事件；通过 Handler.handleMessage() 处理相应的消息事件。
>     - **Looper（消息泵）**：通过Looper.loop()不断地从MessageQueue中抽取Message，按分发机制将消息分发给目标处理者。
>
>   - 具体流程如图
>
>     ![image-20201208233258871](https://tva1.sinaimg.cn/large/0081Kckwly1glgvcadu3dj31gs0u0kh8.jpg)
>
>     - `Handler.sendMessage()`发送消息时，会通过`MessageQueue.enqueueMessage()`向MessageQueue中添加一条消息；
>     - 通过`Looper.loop()`开启循环后，不断轮询调用`MessageQueue.next()`；
>     - 调用目标`Handler.dispatchMessage()`去传递消息，目标Handler收到消息后调用`Handler.handlerMessage()`处理消息。

```
Q：为什么系统不建议在子线程访问UI？
```

> - 技术点：UI线程、子线程
>
> - 参考回答：系统不建议在子线程访问UI的原因是，UI控件
>
>   非线程安全
>
>   ，在多线程中并发访问可能会导致UI控件处于不可预期的状态。而不对UI控件的访问加上锁机制的原因有：
>
>   - 上锁会让UI控件变得复杂和低效
>   - 上锁后会阻塞某些进程的执行

```
Q：一个Thread可以有几个Looper？几个Handler？
```

> - 技术点：Looper、Handler
> - 参考回答：一个Thread只能有一个Looper，可以有多个Handler
> - 引申：更多数量关系：Looper有一个MessageQueue，可以处理来自多个Handler的Message；MessageQueue有一组待处理的Message，这些Message可来自不同的Handler；Message中记录了负责发送和处理消息的Handler；Handler中有Looper和MessageQueue；

```
Q：如何将一个Thread线程变成Looper线程？Looper线程有哪些特点？
```

> - 技术点：Looper
> - 参考回答：通过Looper.prepare()可将一个Thread线程转换成Looper线程。Looper线程和普通Thread不同，它通过MessageQueue来存放消息和事件、Looper.loop()进行消息轮询。

```
Q：可以在子线程直接new一个Handler吗？那该怎么做？
```

> - 技术点：Handler
> - 参考回答：不同于主线程直接new一个Handler，由于子线程的Looper需要手动去创建，在创建Handler时需要多一些方法：

```java
new Thread(new Runnable() {
            @Override
            public void run() {
                Looper.prepare();//为子线程创建Looper  
                new Handler() {
                @Override
                public void handleMessage(Message msg) {
                    super.handleMessage(msg);
                    //子线程消息处理
                }
            };
                Looper.loop(); //开启消息轮询
            }
        }).start();
Q：Message可以如何创建？哪种效果更好，为什么？
```

> - 技术点：Message
> - 参考回答：创建Message对象的几种方式：
>   - Message msg = new Message();
>   - Message msg = Message.obtain();
>   - Message msg = handler1.obtainMessage();
>      后两种方法都是从整个**Messge池**中返回一个新的Message实例，能有效避免重复Message创建对象，因此更鼓励这种方式创建Message

```
Q：这里的ThreadLocal有什么作用？
```

> - 技术点：ThreadLocal
> - 参考回答：ThreadLocal类可实现**线程本地存储的功能**，把共享数据的可见范围限制在同一个线程之内，无须同步就能保证线程之间不出现数据争用的问题，这里可理解为ThreadLocal帮助Handler找到本线程的Looper。
>   - 底层数据结构：每个线程的Thread对象中都有一个ThreadLocalMap对象，它存储了一组以==ThreadLocal.threadLocalHashCode为key、以本地线程变量为value的键值对==，而ThreadLocal对象就是当前线程的ThreadLocalMap的访问入口，也就包含了一个独一无二的**threadLocalHashCode**值，通过这个值就可以在线程键值值对中找回对应的本地线程变量。

```
Q：主线程中Looper的轮询死循环为何没有阻塞主线程？
```

> - 技术点：Looper
> - 参考回答：**Android是依靠事件驱动的，通过Loop.loop()不断进行消息循环，可以说Activity的生命周期都是运行在 Looper.loop()的控制之下，一旦退出消息循环，应用也就退出了。而所谓的导致ANR多是因为某个事件在主线程中处理时间太耗时，因此只能说是对某个消息的处理阻塞了Looper.loop()，反之则不然。**

```
Q：使用Hanlder的postDealy()后消息队列会发生什么变化？
```

> - 技术点：Handler
> - 参考回答：post delay的Message并不是先等待一定时间再放入到MessageQueue中，而是直接进入并阻塞当前线程，然后**将其delay的时间和队头的进行比较，按照触发时间进行排序**，如果触发时间更近则放入队头，保证队头的时间最小、队尾的时间最大。此时，如果队头的Message正是被delay的，则将当前线程堵塞一段时间，直到等待足够时间再唤醒执行该Message，否则唤醒后直接执行。

- [线程](https://www.jianshu.com/p/ab77a2e83c52)

```
Q：Android中还了解哪些方便线程切换的类？
```

> - 技术点：线程通信
> - 参考回答：对Handler进一步的封装的几个类：
>   - **AsyncTask**：底层封装了线程池和**Handler**，便于执行后台任务以及在子线程中进行UI操作。
>   - **HandlerThread**：一种具有**消息循环**的线程，其内部可使用Handler。
>   - **IntentService**：是一种**异步、会自动停止**的服务，内部采用**HandlerThread**。
> - 引申：更多是对消息机制的理解

```
Q：AsyncTask相比Handler有什么优点？不足呢？
```

> - 技术点：AsyncTask、Handler
> - 参考回答：
>   - Handler机制存在的**问题**：多任务同时执行时不易精确控制线程。
>   - 引入AsyncTask的**好处**：创建异步任务更简单，直接继承它可方便实现后台异步任务的执行和进度的回调更新UI，而无需编写任务线程和Handler实例就能完成相同的任务。

```
Q：使用AsyncTask需要注意什么？
```

> - 技术点：AsyncTask
> - 参考回答：
>   - 不要直接调用onPreExecute()、**doInBackground()**、onProgressUpdate()、onPostExecute()和onCancelled()方法
>   - 一个异步对象只能调用一次execute()方法
> - 引申：谈谈AsyncTask初始化、五个核心方法如何配合进而体现Handler的作用

```
Q：AsyncTask中使用的线程池大小？
```

> - 技术点：AsyncTask
> - 参考回答：在AsyncTask内部实现有两个线程池：
>   - **SerialExecutor**：用于任务的排队，默认是**串行**的线程池，在3.0以前核心线程数为5、线程池大小为128，而3.0以后变为同一时间只能处理一个任务
>   - **THREAD_POOL_EXECUTOR**：用于真正执行任务。
> - 引申：谈谈对线程池的理解

```
Q：HandlerThread有什么特点？
```

> - 技术点：HandlerThread
> - 参考回答：HandlerThread是一个线程类，它继承自Thread。与普通Thread不同，HandlerThread具有**消息循环**的效果，这是因为它内部`HandlerThread.run()`方法中有Looper，能通过`Looper.prepare()`来创建消息队列，并通过`Looper.loop()`来开启消息循环。

```
Q：快速实现子线程使用Handler
```

> - 技术点：HandlerThread
> - 思路：不同于之前手动在子线程创建Looper再构建Handler的想法，这里从HandlerThread角度去快速实现在子线程使用Handler
> - 参考回答：HandlerThread实现方法
>   - 实例化一个HandlerThread对象，参数是该线程的名称；
>   - 通过 HandlerThread.start()开启线程；
>   - 实例化一个Handler并传入HandlerThread中的looper对象，使得与HandlerThread绑定；
>   - 利用Handler即可执行异步任务；
>   - 当不需要HandlerThread时，通过HandlerThread.quit()/quitSafely()方法来终止线程的执行。

```
Q：IntentService的特点？
```

> - 技术点：IntentService
> - 思路：和普通线程和普通Service比较突出其特点
> - 参考回答： 不同于线程，IntentService是**服务，优先级比线程高，更不容易被系统杀死**，因此较适合执行一些**高优先级**的后台任务；不同于普通Service，IntentService可**自动创建**子线程来执行任务，且任务执行完毕后**自动退出**。

```
Q：为何不用bindService方式创建IntentService？
```

> - 技术点：IntentService
> - 思路：从底层实现出发
> - 参考回答：IntentService的工作原理是，在IntentService的onCreate()里会创建一个**HandlerThread**，并利用其内部的Looper实例化一个**ServiceHandler**对象；而这个ServiceHandler用于处理消息的handleMessage()方法会去调用IntentService的onHandleIntent()，这也是为什么可在该方法中处理后台任务的逻辑；当有Intent任务请求时会把Intent封装到Message，然后ServiceHandler会把消息发送出，而发送消息是在**onStartCommand**()完成的，只能通过startService()才可走该生命周期方法，因此不能通过bindService创建IntentService。

```
Q：线程池的好处、原理、类型？
```

> - 技术点：线程池
> - 参考回答：
> - （1）线程池的好处：
>   - **重用**线程池中的线程，避免线程的创建和销毁带来的性能消耗；
>   - 有效控制线程池的**最大并发数**，避免大量的线程之间因互相抢占系统资源而导致阻塞现象；
>   - 进行**线程管理**，提供定时/循环间隔执行等功能
> - （2）线程池的分类：
>   - **FixThreadPool**：线程数量固定的线程池，所有线程都是**核心线程**，当线程空闲时**不会**被回收；能**快速**响应外界请求。
>   - **CachedThreadPool**：线程数量不定的线程池（最大线程数为**Integer.MAX_VALUE**），只有**非核心线程**，空闲线程有超时机制，超时回收；适合于执行大量的**耗时较少**的任务
>   - **ScheduledThreadPool**：核心线程数量**固定**，非核心线程数量**不定**；可进行**定时**任务和**固定**周期的任务。
>   - **SingleThreadExecutor**：只有**一个核心线程**，可确保所有的任务都在同一个线程中**按顺序**执行；好处是无需处理**线程同步**问题。
> - （3）线程池的原理：实际上通过ThreadPoolExecutor并通过一系列参数来配置各种各样的线程池，具体的参数有：
>   - **corePoolSize**核心线程数：一般会在线程中一直存活
>   - **maximumPoolSize**最大线程数：当活动线程数达到这个数值后，后续的任务将会被阻塞
>   - **keepAliveTime**非核心线程超时时间：超过这个时长，闲置的非核心线程就会被回收
>   - **unit**：用于指定keepAliveTime参数的时间单位
>   - **workQueue**任务队列：通过线程池的`execute()`方法提交的Runnable对象会存储在这个参数中。
>   - **threadFactory**：线程工厂，可创建新线程
>   - **handler**：在线程池无法执行新任务时进行调度
> - 引申：[使用Executors各个方法创建线程池的弊端](https://www.jianshu.com/p/4b89d681c5a0)

```
Q：ThreadPoolExecutor的工作策略？
```

> - 技术点：线程池
>
> - 参考回答：ThreadPoolExecutor的
>
>   默认工作策略
>
>   ：
>
>   - 若程池中的线程数量**未达到**核心线程数，则会直接启动一个核心线程执行任务。
>
>   - 若线程池中的线程数量
>
>     已达到
>
>     或者超过核心线程数量，则任务会被插入到任务列表等待执行。
>
>     - 若任务
>
>       无法插入
>
>       到任务列表中，往往由于任务列表已满，此时如果
>
>       - 线程数量**未达到**线程池最大线程数，则会启动一个非核心线程执行任务；
>       - 线程数量**已达到**线程池规定的最大值，则拒绝执行此任务，ThreadPoolExecutor会调用RejectedExecutionHandler的rejectedExecution方法来通知调用者。
>
> - 引申：ThreadPoolExecutor的拒绝策略

```
Q：什么是ANR？什么情况会出现ANR？如何避免？在不看代码的情况下如何快速定位出现ANR问题所在？
```

> - 技术点：ANR
> - 思路：
> - 参考回答：
>   - ANR（Application Not Responding，应用无响应）：当操作在一段时间内系统无法处理时，会在系统层面会弹出ANR对话框
>   - 产生ANR可能是因为5s内无响应用户输入事件、10s内未结束BroadcastReceiver、20s内未结束Service
>   - 想要避免ANR就不要在主线程做耗时操作，而是通过开子线程，方法比如继承Thread或实现Runnable接口、使用AsyncTask、IntentService、HandlerThread等
> - 引申：快读定位ANR方法：使用命令导出ANR日志，并分析关键信息，详见[ANR问题一般解决思路](https://www.jianshu.com/p/3959a601cea6)

- [Bitmap](https://www.jianshu.com/p/aaafcd72c127)

```
Q：加载图片的时候需要注意什么？
```

> - 技术点：Bitmap高效加载
> - 参考回答：
>   - 直接加载大容量的高清Bitmap很容易出现显示不完整、内存溢出OOM的问题，所以最好按一定的**采样率**将图片缩小后再加载进来
>   - 为减少流量消耗，可对图片采用内存缓存策略，又为了避免图片占用过多内存导致内存溢出，最好以软引用方式持有图片
>   - 如果还需要网上下载图片，注意要开子线程去做下载的耗时操作

```
Q：LRU算法的原理？
```

> - 技术点：LRU算法
> - 参考回答：为减少流量消耗，可采用缓存策略。常用的缓存算法是LRU(Least Recently Used)：
>   - 核心思想：当缓存满时, 会优先淘汰那些近期最少使用的缓存对象。主要是两种方式：
>     - LruCache(内存缓存)：LruCache类是一个线程安全的**泛型类**：内部采用一个`LinkedHashMap`以**强引用**的方式存储外界的缓存对象，并提供`get`和`put`方法来完成缓存的获取和添加操作，当缓存满时会移除较早使用的缓存对象，再添加新的缓存对象。
>     - DiskLruCache(磁盘缓存)： 通过将缓存对象**写入文件**系统从而实现缓存效果
> - 引申：感兴趣可了解具体实现算法

- [性能优化](https://www.jianshu.com/p/81485e65c2c8)

```
Q：项目中如何做性能优化的？
```

> - 技术点：性能优化实例【布、绘、泄、线】
> - 思路：举例说明项目注意了哪些方面的性能优化，如**布局优化、绘制优化、内存泄漏优化**、 响应速度优化、列表优化、Bitmap优化、 **线程优化......**

```
Q：了解哪些性能优化的工具？
```

> - 技术点：性能优化工具
> - 思路：做项目时是否使用过的系统自带的性能优化工具？公司是否有自己的性能优化工具？实现原理怎样的？

```
Q：布局上如何优化？
```

> - 技术点：布局优化
>
> - 参考回答：布局优化的核心就是尽量减少布局文件的
>
>   层级
>
>   ，常见的方式有：
>
>   - 多嵌套情况下可使用RelativeLayout减少嵌套。
>   - 布局层级相同的情况下使用LinearLayout，它比RelativeLayout更高效。
>   - 使用`<include>`标签重用布局、`<merge>`标签减少层级、`<ViewStub>`标签懒加载。

```
Q：内存泄漏是什么？为什么会发生？常见哪些内存泄漏的例子？都是怎么解决的？
```

> - 技术点：内存泄漏
>
> - 参考回答：内存泄漏(Memory Leak)是指程序在申请内存后，
>
>   无法释放
>
>   已申请的内存空间。简单地说，发生内存泄漏是由于长周期对象持有对短周期对象的引用，使得短周期对象不能被及时回收。常见的几个例子和解决办法：
>
>   - 单例模式导致的内存泄漏：单例传入参数this来自Activity，使得持有对Activity的引用。
>
>     - 解决办法：传参context.getApplicationContext()
>
>   - Handler导致的内存泄漏：Message持有对Handler的引用，而非静态内部类的Handler又隐式持有对外部类Activity的引用，使得引用关系会保持至消息得到处理，从而阻止了Activity的回收。
>
>     - 解决办法：使用静态内部类+WeakReference弱引用；当外部类结束生命周期时清空消息队列。
>
>   - 线程导致的内存泄漏：AsyncTask/Runnable以
>
>     匿名内部类
>
>     的方式存在，会隐式持有对所在Activity的引用。
>
>     - 解决办法：将AsyncTask和Runnable设为静态内部类或独立出来；在线程内部采用弱引用保存Context引用
>
>   - 资源未关闭导致的内存泄漏：未及时注销资源导致内存泄漏，如BraodcastReceiver、File、Cursor、Stream、Bitmap等。
>
>     - 解决办法：在Activity销毁的时候要及时关闭或者注销。
>       - BraodcastReceiver：调用`unregisterReceiver()`注销；
>       - Cursor，Stream、File：调用`close()`关闭；
>       - 动画：在`Activity.onDestroy()`中调用`Animator.cancel()`停止动画
>
> - 引申：谈谈项目中是如何注意内存泄漏的问题

```
Q：内存泄漏和内存溢出的区别
```

> - 技术点：内存泄漏、内存溢出
> - 参考回答：
>   - 内存泄漏(Memory Leak)是指程序在申请内存后，**无法释放**已申请的内存空间。是造成应用程序OOM的主要原因之一。
>   - 内存溢出(out of memory)是指程序在申请内存时，没有足够的内存空间供其使用。

```
Q：什么情况会导致内存溢出？
```

> - 技术点：内存溢出
> - 参考回答：内存泄漏是导致内存溢出的主要原因；直接加载大图片也易造成内存溢出
> - 引申：谈谈如何避免内存溢出（如何避免内存泄漏、避免直接加载大图片）

- 开源框架（略）
- 谷歌新动态

```
Q：是否了解和使用过谷歌推出的新技术？`
 `Q：有了解刚发布的Androidx.0的特性吗？`
 `Q：Kotlin对Java做了哪些优化？
```

> - 可能意图：了解候选者对谷歌&安卓的关注度、共同探讨对新技术的看法、学习主动性、平时学习习惯
> - 思路：谷歌的安卓官方网站（中文版）：[https://developer.android.google.cn](https://links.jianshu.com/go?to=https%3A%2F%2Fdeveloper.android.google.cn)，了解最新动态

**1.2 Java**

- 基础

```
Q：面向对象编程的四大特性及其含义？
```

> - 技术点：面向对象编程特点
> - 思路：分条简述每个特性的含义
> - 参考回答：
>   - 抽象：对现实世界的事物进行概括，抽象为在计算机虚拟世界中有意义的实体
>   - 封装：将某事物的属性和行为包装到对象中，构成一个不可分割的独立实体，数据被保护在抽象数据类型的内部，并且尽可能地隐藏内部的细节，只保留一些对外接口使之与外部发生联系
>   - 继承：子类继承父类，不仅可以有父类原有的方法和属性，也可以增加自己的或者重写父类的方法及属性
>   - 多态：允许不同类的对象对同一消息做出各自的响应

```
Q：String、StringBuffer和StringBuilder的区别？
```

> - 技术点：String
> - 参考回答：
>   - String是字符串常量，而StringBuffer、StringBuilder都是字符串变量，即String对象一创建后不可更改，而后两者的对象是可更改的：
>   - StringBuffer是线程安全的，而StringBuilder是非线程安全的，这是由于StringBuffer对方法加了同步锁或者对调用的方法加了同步锁
>   - String更适用于少量的字符串操作的情况，StringBuilder适用于单线程下在字符缓冲区进行大量操作的情况，StringBuffer适用于多线程下在字符缓冲区进行大量操作的情况

```
Q：String a=""和String a=new String("")的的关系和异同？
```

> - 技术点：String
> - 参考回答：
>   - 通过String a=""直接赋值的方式得到的是一个字符串常量，存在于常量池；注意，相同内容的字符串在常量池中只有一个，即如果池已包含内容相等的字符串会返回池中的字符串，反之会将该字符串放入池中
>   - 通过new String("")创建的字符串不是常量是实例对象，会在堆内存开辟空间并存放数据，且每个实例对象都有自己的地址空间
> - 引申：对于用String a=""和String a=new String("")两种方式定义的字符串，判断使用equals()、"=="比较结果是什么

```
Q：Object的equal()和==的区别？
```

> - 技术点：equal()、==
> - 参考回答：
>   - equals()：是Object的公有方法，具体含义取决于如何重写，比如String的equals()比较的是两个字符串的内容是否相同
>   - "==" ：对于基本数据类型来说，比较的是两个变量值是够是否相等，对于引用类型来说，比较的是两个对象的内存地址是否相同
> - 引申：对于用String a=""和String a=new String("")两种方式定义的字符串，判断使用equals()、"=="比较结果是什么

```
Q：装箱、拆箱什么含义？
```

> - 技术点：装箱、拆箱
> - 参考回答：装箱就是自动将基本数据类型转换为包装器类型，拆箱就是自动将包装器类型转换为基本数据类型

```
Q：int和Integer的区别？
```

> - 技术点：基本数据类型、引用类型
> - 参考回答：
>   - Integer是int的包装类，int则是java的一种基本数据类型
>   - Integer变量必须实例化后才能使用，而int变量不需要
>   - Integer实际是对象的引用，当new一个Integer时，实际上是生成一个指针指向此对象；而int则是直接存储数据值
>   - Integer的默认值是null，int的默认值是0

```
Q：遇见过哪些运行时异常？异常处理机制知道哪些？
```

> - 技术点：Java异常机制
>
> - 思路：对Throwable异常进行分类说明每种异常的特点和常见问题，简述几种常见异常处理机制，详见[Java基础之异常机制](https://www.jianshu.com/p/3718766df5ba)
>
> - 参考回答：
>
> - （1） Throwable继承层次结构，可见分成两大类Error和Exception：
>
>   - Error（错误）:指程序**无法**恢复的异常情况，表示运行应用程序中较严重的问题；发生于虚拟机自身、或者在虚拟机试图执行应用时，如Virtual MachineError（Java虚拟机运行错误）、NoClassDefFoundError（类定义错误）；属于**不可查**异常，即不强制程序员必须处理，即使不处理也不会出现语法错误。
>
>   - Exception（异常）:指程序
>
>     有可能
>
>     恢复的异常情况，表示程序本身可以处理的异常。又分两大类：
>
>     - RuntimeException（运行时异常）：由程序**自身**的问题导致产生的异常；如NullPointerException（空指针异常）、IndexOutOfBoundsException（下标越界异常）；属于**不可查**异常。
>
>     - 非运行时异常：由程序
>
>       外部
>
>       的问题引起的异常；除了RuntimeException以外的异常，如FileNotFoundException（文件不存在异常）；属于
>
>       可查
>
>       异常，即强制程序员必须进行处理，如果不进行处理则会出现语法错误。
>
>       ![image-20201208233331320](https://tva1.sinaimg.cn/large/0081Kckwly1glgvcvr5fvj32l60koh9k.jpg)
>
> - （2）常见的异常处理机制有：
>
>   - 捕捉异常：由系统自动抛出异常，即try捕获异常->catch处理异常->finally 最终处理
>   - 抛出异常：在方法中将异常对象**显性**地抛出，之后异常会沿着调用层次向上抛出，交由调用它的方法来处理。配合throws声明抛出的异常和throw抛出异常
>   - 自定义异常：继承Execption类或其子类

```
Q：什么是反射，有什么作用和应用？
```

> - 技术点：反射
> - 思路：简述反射的定义、功能和应用，详见[Java基础之泛型&反射](https://www.jianshu.com/p/fcdfb8234b66)
> - 参考回答：
>   - **含义**：在运行状态中，对于任意一个类都能知道它的所有属性和方法，对于任何一个对象都能够调用它的任何一个方法和属性。
>   - **功能**：动态性，体现在：在运行时判断任意一个类所具有的属性和方法； 在运行时判断任意一个对象所属的类；在运行时构造任意一个类的对象；在运行时调用任意一个对象的方法；生成动态代理
>   - 应用：反射&泛型
> - 引申：是否在项目中使用过反射机制，有什么优缺点

```
Q：什么是内部类？有什么作用？静态内部类和非静态内部类的区别？
```

> - 技术点：内部类
> - 思路：
> - 参考回答：内部类就是定义在另外一个类里面的类。它隐藏在外部类中，封装性更强，不允许除外部类外的其他类访问它；但它可直接访问外部类的成员。静态内部类和非静态内部类的区别有：
>   - 静态内部类是指被声明为static的内部类，可不依赖外部类实例化；而非静态内部类需要通过生成外部类来间接生成。
>   - 静态内部类只能访问外部类的静态成员变量和静态方法，而非静态内部类由于持有对外部类的引用，可以访问外部类的所用成员
> - 引申：谈谈匿名内部类

```
Q：final、finally、finalize()分别表示什么含义
```

> - 技术点：final、finally、finalize()
> - 参考回答：
>   - final关键字表示不可更改，具体体现在：
>     - final修饰的变量必须要初始化，且赋初值后不能再重新赋值
>     - final修饰的方法不能被子类重写
>     - final修饰的类不能被继承
>   - finally：和try、catch成套使用进行异常处理，无论是否捕获或处理异常，finally块里的语句都会被执行，在以下4种特殊情况下，finally块才不会被执行：
>     - 在finally语句块中发生了异常
>     - 在前面的代码中用了`System.exit()`退出程序
>     - 程序所在的线程死亡
>     - 关闭CPU
>   - finalize()：是Object中的方法，当垃圾回收器将回收对象从内存中清除出去之前会调用finalize()，但此时并不代表该回收对象一定会“死亡”，还有机会“逃脱”

```
Q：重写和重载的区别？
```

> - 技术点：重写、重载
> - 参考回答：重写表示子类重写父类的方法；重载表示有多个同名函数同时存在，区别在于有不同的参数个数或类型
> - 引申：谈谈动态分派和静态分派

```
Q：抽象类和接口的异同？
```

> - 技术点：抽象类、接口
> - 参考回答：
>   - 使用上的区别：一个类只能继承一个抽象类却可以实现多个接口
>   - 设计上的区别：接口是对行为的抽象，无需有子类的前提，是自上而下的设计理念；抽象类是对类的抽象，建立于相似子类之上，是自下而上的设计理念

```
Q：为什么匿名内部类中使用局部变量要用final修饰？
```

> - 技术点：匿名内部类
> - 参考回答：一方面，由于方法中的局部变量的生命周期很短，一旦方法结束变量就要被销毁，为了保证在内部类中能找到外部局部变量，通过final关键字可得到一个外部变量的引用；另一方面，通过final关键字也不会在内部类去做修改该变量的值，保护了数据的一致性。

```
Q：Object有哪些公有方法？
```

> - 技术点：Object
> - 思路：列举常见的几个公有方法
> - 参考回答：
>   - equals()： 和==作用相似
>   - hashCode()：==用于哈希查找，重写了equals()一般都要重写该方法==
>   - getClass()： 获取Class对象
>   - wait()：让当前线程进入等待状态，并释放它所持有的锁
>   - notify()&notifyAll()： 唤醒一个（所有）正处于等待状态的线程
>   - toString()：转换成字符串
> - 引申：equals()和==的不同、在synchronized 同步代码块里wait()和notify()&notifyAll()如何配合、hashCode()和equals()的关系、获取Class对象还有什么方法

- [集合](https://www.jianshu.com/p/7b9abda70c8f)

```
Q：Java集合框架中有哪些类？都有什么特点
```

> - 技术点：集合框架
> - 思路：分条解释每种类的特点
> - 参考回答：可将Java集合框架大致可分为Set、List、Queue 和Map四种体系
>   - Set：代表**无序、不可重复**的集合，常见的类如HashSet、TreeSet
>   - List：代表**有序、可重复**的集合，常见的类如动态数组ArrayList、双向链表LinkedList、可变数组Vector
>   - Map：代表具有**映射关系**的集合，常见的类如HashMap、LinkedHashMap、TreeMap
>   - Queue：代表一种**队列**集合

```
Q：集合、数组、泛型的关系，并比较
```

> - 技术点：集合、数组、泛型
> - 参考回答：
> - （1）集合和数组的区别：
>   - 数组元素可以是基本类型，也可以是对象；数组长度限定；数组只能存储一种类型的数据元素
>   - 集合元素只能是对象；集合长度可变；集合可存储不同种的数据元素
> - （2）泛型相比与集合的好处在于它**安全简单**。具体体现在提供编译时的强类型检查，而不用等到运行；可避免类类型强制转换

```
Q：ArrayList和LinkList的区别？
```

> - 技术点：List对比
> - 参考回答：
>   - **ArrayList**的底层结构是**数组**，可用索引实现快速查找；是动态数组，相比于数组容量可实现动态增长
>   - **LinkedList**底层结构是**链表**，增删速度快；是一个**双向循环**链表，也可以被当作堆栈、队列或双端队列

```
Q：ArrayList和Vector的区别？
```

> - 技术点：List对比
> - 参考回答：
>   - **ArrayList**非线程安全，建议在单线程中才使用ArrayList，而在多线程中可以选择Vector或者CopyOnWriteArrayList；默认初始容量为10，每次扩容为原来的1.5倍
>   - **Vector**使用了synchronized关键字，是**线程安全**的，比ArrayList开销更大，访问更慢；默认初始容量为10，默认每次扩容为原来的2倍，可通过**capacityIncrement**属性设置

```
Q：HashSet和TreeSet的区别？
```

> - 技术点：Set对比
> - 参考回答：
>   - **HashSet**不能保证元素的排列顺序；使用**Hash算法**来存储集合中的元素，有良好的存取和查找性能；通过`equal()`判断两个元素是否相等，并两个元素的`hashCode()`返回值也相等
>   - **TreeSet**是SortedSet接口的实现类，根据元素**实际值的大小**进行排序；采用**红黑树**的数据结构来存储集合元素；支持两种排序方法：**自然排序**（默认情况）和**定制排序**。前者通过实现**Comparable接口**中的`compareTo()`比较两个元素之间大小关系，然后按升序排列；后者通过实现**Comparator接口**中的`compare()`比较两个元素之间大小关系，实现定制排列

```
Q：HashMap和Hashtable的区别？
```

> - 技术点：Map对比
> - 参考回答：
>   - **HashMap**基于AbstractMap类，实现了Map、**Cloneable**（能被克隆）、**Serializable**（支持序列化）接口； **非线程安全**；允许存在一个为null的key和任意个为null的value；采用**链表散列**的数据结构，即数组和链表的结合；初始容量为16，填充因子默认为0.75，扩容时是当前容量翻倍，即2capacity
>   - **Hashtable**基于Map接口和Dictionary类；**线程安全**，开销比HashMap大，如果多线程访问一个Map对象，使用Hashtable更好；不允许使用null作为key和value；底层基于哈希表结构；初始容量为11，填充因子默认为0.75，扩容时是容量翻倍+1，即2capacity+1

```
Q：HashMap在put、get元素的过程？体现了什么数据结构？
```

> - 技术点：HashMap
> - 参考回答：
>   - 向Hashmap中put元素时，首先判断key是否为空，为空则直接调用putForNullKey()，不为空则计算key的hash值得到该元素在数组中的下标值；如果数组在该位置处没有元素，就直接保存；如果有，还要比较是否存在相同的key，存在的话就覆盖原来key的value，否则将该元素保存在**链头**，先保存的在链尾。
>   - 从Hashmap中get元素时，计算key的hash值找到在数组中的对应的下标值，返回该key对应的value即可，如果有冲突就遍历该位置链表寻找key相同的元素并返回对应的value
>   - 由此可看出HashMap采用**链表散列**的数据结构，即数组和链表的结合，在Java8后又结合了红黑树，当链表元素超过8个将链表转换为红黑树

```
Q：如何解决Hash冲突？
```

> - 技术点：Hash冲突
> - 参考回答：
>   - 开放定址法：常见的线性探测方式，在冲突发生时，顺序查看表中下一单元，直到找出一个空单元或查遍全表
>   - 链地址法：将有冲突数组位置生出链表
>   - 建立公共溢出区：将哈希表分为基本表和溢出表两部分，和基本表发生冲突的元素一律填入溢出表
>   - 再哈希法：构造多个不同的哈希函数，有冲突使用下一个哈希函数计算hash值

```
Q：如何保证HashMap线程安全？什么原理？
```

> - 技术点：ConcurrentHashMap
> - 思路：这里回答一种办法，使用ConcurrentHashMap
> - 参考回答：ConcurrentHashMap是线程安全的HashMap，它采取锁分段技术，将数据分成一段一段的存储，然后给每一段数据配一把锁，当一个线程占用锁访问其中一个段数据的时候，其他段的数据也能被其他线程访问。在JDK1.8中对ConcurrentHashmap做了两个改进：
>   - 取消segments字段，直接采用`transient volatile HashEntry<K,V>[] table`保存数据，将数组元素作为锁，对每一行数据进行加锁，可减少并发冲突的概率
>   - 数据结构由“数组＋单向链表”变为“数组＋单向链表＋红黑树”，使得查询的时间复杂度可以降低到O(logN)，改进一定的性能。
> - 引申：LinkHashMap线程安全的底层实现

```
Q：HashMap是有序的吗？如何实现有序？
```

> - 技术点：LinkHashMap
> - 思路：这里回答一种办法，使用LinkedHashMap
> - 参考回答：HashMap是无序的，而LinkedHashMap是有序的HashMap，默认为插入顺序，还可以是访问顺序，基本原理是其内部通过Entry维护了一个【双向链表】，负责维护Map的迭代顺序
> - 引申：LinkHashMap有序的底层实现

```
Q：HashMap是如何扩容的？如何避免扩容？
```

> - 技术点：HashMap
> - 参考回答：
>   - HashMap几个默认值，初始容量为16、填充因子默认为0.75、扩容时【容量翻倍】。也就是说当HashMap中元素个数超过`16*0.75=12`时会把数组的大小扩展为`2*16=32`，然后重新计算每个元素在数组中的位置
>   - 由于每次扩容还需要【重新计算元素Hash值，损耗性能】，所以建议在使用HashMap时，最好先估算Map的大小，设置初始值，避免频繁扩容

```
Q：hashcode()的作用，与equal()有什么区别？
```

> - 技术点：Hash值
> - 参考回答：hashCode()用于计算对象的Hash值，确认对象在散列存储结构中的存储地址。和equal()的区别：
>   - equals()比较两个对象的地址值是否相等 ；hashCode()得到的是对象的存储位置，可能不同对象会得到相同值
>   - 有两个对象，若equals()相等，则hashcode()一定相等；hashcode()不等，则equals()一定不相等；hashcode()相等，equals()可能相等、可能不等
>   - 使用equals()比较两个对象是否相等效率较低，最快办法是先用hashCode()比较，如果hashCode()不相等，则这两个对象肯定不相等；如果hashCode()相等，此时再用equal()比较，如果equal()也相等，则这两个对象的确相等，反之

- 并发

```
Q：同步和非同步、阻塞和非阻塞的概念
```

> - 技术点：同步、阻塞
> - 参考回答：
>   - 同步和异步体现的是消息的通知机制：所谓同步，方法A调用方法B后必须等到方法B返回结果才能继续后面的操作；所谓异步，方法A调用方法B后可让方法B在调用结束后通过回调等方式通知方法A
>   - 阻塞和非阻塞侧重于等待消息时的状态：所谓阻塞，就是在结果返回之前让当前线程挂起；所谓非阻塞，就是在等待时可做其他事情，通过轮询去询问是否已返回结果

```
Q：Thread的join()有什么作用？
```

> - 技术点：线程相关方法
> - 参考回答：Thread的join()的含义是等待该线程终止，即将挂起调用线程的执行，直到被调用的对象完成它的执行。比如存在两个线程t1和t2，下述代码表示先启动t1，直到t1的任务结束，**才轮到t2启动。**



```java
t1.start();
t1.join(); 
t2.start();
Q：线程的有哪些状态？
```

> - 技术点：线程状态
>
> - 思路：可分条解释每种状态的特点以及如何转换。详见[要点提炼| 理解JVM之内存模型&线程](https://www.jianshu.com/p/90a036212cb4)
>
> - 参考回答：在任意一个时间点，一个线程只能有且只有其中的一种状态：
>
>   - **新建**（New）：线程创建后尚未启动
>
>   - **运行**（Runable）：包括正在执行（Running）和等待着CPU为它分配执行时间（Ready）两种
>
>   - 无限期等待
>
>     （Waiting）：该线程不会被分配CPU执行时间，要等待被其他线程显式地唤醒。以下方法会让线程陷入无限期等待状态：
>
>     - 没有设置Timeout参数的`Object.wait()`
>     - 没有设置Timeout参数的`Thread.join()`
>     - `LockSupport.park()`
>
>   - 限期等待
>
>     （Timed Waiting）：该线程不会被分配CPU执行时间，但在一定时间后会被系统自动唤醒。以下方法会让线程进入限期等待状态：
>
>     - `Thread.sleep()`
>     - 设置了Timeout参数的`Object.wai()`
>     - 设置了Timeout参数的`Thread.join()`
>     - `LockSupport.parkNanos()`
>     - `LockSupport.parkUntil()`
>
>   - **阻塞**（Blocked）：线程被阻塞。和等待状态不同的是，阻塞状态表示在等待获取到一个**排他锁**，在另外一个线程放弃这个锁的时候发生；而等待状态表示在等待一段**时间**或者**唤醒动作**的发生，在程序等待进入同步区域的时候发生。
>
>   - 结束
>
>     （Terminated）：线程已经结束执行
>
>     ![image-20201208233355535](https://tva1.sinaimg.cn/large/0081Kckwly1glgvd9jft9j30wm0jewlf.jpg)

```
Q：什么是线程安全？保障线程安全有哪些手段？
```

> - 技术点：线程安全
>
> - 思路：详见[要点提炼| 理解JVM之线程安全&锁优化](https://www.jianshu.com/p/ca8801044352)
>
> - 参考回答：线程安全就是当多个线程访问一个对象时，如果不用考虑这些线程在运行时环境下的调度和交替执行，也不需要进行额外的同步，或者在调用方进行任何其他的协调操作，调用这个对象的行为都可以获得正确的结果，那这个对象是线程安全的。保证线程安全可从多线程三特性出发：
>
>   - 原子性
>
>     （Atomicity）：单个或多个操作是要么全部执行，要么都不执行
>
>     - Lock：保证同时只有一个线程能拿到锁，并执行申请锁和释放锁的代码
>     - synchronized：对线程加独占锁，被它修饰的类/方法/变量只允许一个线程访问
>
>   - 可见性
>
>     （Visibility）：当一个线程修改了共享变量的值，其他线程能够立即得知这个修改
>
>     - volatile：保证新值能**立即**同步到主内存，且每次使用前立即从主内存刷新；
>     - synchronized：在释放锁之前会将工作内存新值更新到主存中
>
>   - 有序性
>
>     （Ordering）：程序代码按照指令顺序执行
>
>     - volatile： 本身就包含了禁止指令重排序的语义
>     - synchronized：保证一个变量在同一个时刻只允许一条线程对其进行lock操作，使得持有同一个锁的两个同步块只能串行地进入

```
Q：ReentrantLock和synchronized的区别?
```

> - 技术点：线程安全（ReentrantLock、synchronized）
>
> - 思路：详见[要点提炼| 理解JVM之线程安全&锁优化](https://www.jianshu.com/p/ca8801044352)
>
> - 参考回答： ReentrantLock与synchronized的
>
>   不同
>
>   在于ReentrantLock：
>
>   - **等待可中断**：当持有锁的线程长期不释放锁的时候，正在等待的线程可以选择放弃等待，改为处理其他事情。
>   - **公平锁**：多个线程在等待同一个锁时，必须按照申请锁的时间顺序来依次获得锁。而**synchronized是非公平的**，即在锁被释放时，任何一个等待锁的线程都有机会获得锁。ReentrantLock默认情况下也是非公平的，但可以通过带布尔值的构造函数改用公平锁。
>   - **锁绑定多个条件**：一个ReentrantLock对象可以通过多次调用newCondition()**同时绑定多个Condition对象**。而在synchronized中，锁对象wait()和notify()或notifyAl()**只能实现一个隐含的条件**，若要和多于一个的条件关联不得不额外地添加一个锁。

```
Q：synchronized和volatile的区别？
```

> - 技术点：线程安全（synchronized、volatile）
> - 参考回答：
>   - synchronized能保证操作的原子性，而[volatile不可以](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.cnblogs.com%2Fkubidemanong%2Fp%2F9505944.html)，假设线程A和线程B同时读取到变量a值，A修改a后将值更新到主内存，同时B也修改a值会覆盖A的修改操作
>   - synchronized可修饰变量、方法和类，而volatile只能修饰变量
>   - synchronized可能会造成线程阻塞，而volatile不会造成线程的阻塞

```
Q：synchronized同步代码块还有同步方法本质上锁住的是谁？为什么？
```

> - 技术点：线程安全（synchronized）
> - 参考回答：本质上锁住的是对象。在java虚拟机中，每个对象和类在逻辑上都和一个监视器相关联，synchronized本质上是对一个**对象监视器的获取**。当执行同步代码块或同步方法时，执行方法的线程必须先获得该对象的监视器，才能进入同步代码块或同步方法；而没有获取到的线程将会进入阻塞队列，直到成功获取对象监视器的线程执行结束并释放锁后，才会唤醒阻塞队列的线程，使其重新尝试对对象监视器的获取。

```
Q：sleep()和wait()的区别？
```

> - 技术点：sleep()、wait()
> - 参考回答：
>   - sleep()来自Thread类；wait()来自Object类
>   - sleep()用于线程控制**自身流程**；而wait()用于**线程间通信**，配合notify()/notifyAll()在同步代码块或同步方法里使用
>   - sleep()的线程不会释放对象锁；wait()会释放对象锁进入等待状态，使得其他线程能使用同步代码块或同步方法

- Java新动态

```
Q：是否了解Java1.x的特性吗？`
 `Q：谈谈对面向过程编程、面向对象编程还有面向切面编程的理解
```

> - 可能意图：了解候选者对Java和其他语言的关注度和看法、学习主动性、平时学习习惯
> - 思路：Oracle技术网（Java）：[https://www.oracle.com/technetwork/cn/java/index.html](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.oracle.com%2Ftechnetwork%2Fcn%2Fjava%2Findex.html) 、开源中国：[https://www.oschina.net](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.oschina.net) ，了解最新动态



作者：厘米姑娘
链接：https://www.jianshu.com/p/2dd855aa1938
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。