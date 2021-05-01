[TOC]

> 异步、基于事件、库

![思维导图](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/idQFnXhZhTK5mHU8dWHxf.OHrWLHfPTuszdKnpaYI5o!/r/dFQBAAAAAAAA)

---

### 一、简介

- 传统的面向对象编程是通过**抽象出的对象关系**解决问题，函数式编程是通过**函数的组合来**解决问题，响应式编程是**通过函数式编程解决回调地狱**的问题
- 函数响应式编程：是结合了函数式和响应式的优点，把函数范式里的一套思路和响应式编程结合起来就是函数响应式编程
- 用传统的面向对象处理异步事件不是很直观，并发处理也十分麻烦，所以才产生了函数响应式编程
- RxJava是Reactive Extensions 的Java实现，用于通过使用Observable、Flowable等序列来构建**异步和基于事件的程序的库**
- RxJava是Reactive Extensions 在JVM平台上的一个实现，通过使用观察者序列来构建异步、基于事件的程序。
- RxJava可以说是观察者模式的一个扩展，支持不同的数据、事件流，添加了一些操作符，让我们可以用**声明的方式来组合这些序列**，而无需关注底层实现，如线程、同步、线程安全、并发数据结构和非阻塞I/O

------



### 二、基础知识

#### 1、五种关系

| 类型        | 描述                                                         |
| ----------- | ------------------------------------------------------------ |
| Observable  | 能够发送0个或n个数据，并以发送成功或错误事件终止             |
| Flowable    | 能够发送0个或n个数据，并以发送成功或错误事件终止。**支持背压，可以控制数据源发射的速度** |
| Single      | 能够发送单个数据或者错误事件                                 |
| Completable | 从来不发射数据，只处理onComplete和onError事件,除非调用connect。可以看成Ex的Runnable |
| Maybe       | 能够发送0或者1个数据，要么成功，要么失败。有点类似于Optional |

- RxJava的使用通常需要三步
  - Observable：；类似于上游发送命令，决定什么时候触发事件以及触发什么样的事件
  - Observer：观察者，可以再不同的线程中执行任务，他想一个处于等待状态的烧饼，可以再未来某个时刻相应Observable通知，而不需要阻塞等待Observable发射数据
  - subscribe()：将二者进行连接
- Action：无参数类型、Consumer：单一参数类型
- doOn/doAfterXXX操作符可以给Observable的生命周期各个阶段加上一系列的回调监听，当Observable执行到这个阶段时，这些回调会被触发
- Observable分类：
  - Hot Observable与订阅者是**一对多**关系
    - 可以想象成一个广播电台，多有在此刻收听的听众都会听到同一首歌
    - 无论有没有观察者进行订阅，事件始终都会发生
  - Cold Observable与订阅者是**一对一**关系
    - 可以想象成一张CD，人们可以单独购买收听他
    - 只有观察者订阅了，才开始执行发送数据的代码
- Cold Observable
  - Observable的just、creat、range、fromXXX等都可以产生Cold Observable
- Cold -> Hot
  - 使用publish操作符
    - 使用publish操作符可以让Cold Observable转换成Hot Observable，它将原来的Observable转换成ConnectableObservable
    - 生成的ConnectableObservable需要调用connect()才会真正的执行
    - 多个订阅的subscriber共享同一事件，**ConnectableObservable是线程安全的**
  - 使用Subject/Processor
    - Subject和Processor的作用相同。
    - Processor是RxJava2.x新增的类，继承自Flowable，支持背压，而Subject则不支持
    - Subject既是Observable又是Observer，可以从源码得出，Subject继承自Observable，实现了Observer
    - Subject并**不是线程安全的**，如果想要其安全，西药调用toSerialized()方法

```java
PublishSubject<String> publishSubject=PublishSubject.create();
        observable.subscribe(publishSubject);
        publishSubject.subscribe(new Consumer<String>() {
            @Override
            public void accept(String s) throws Exception {

            }
        });
```

- Hot -> Cold

```java
 ConnectableObservable<String> mConnectableObservable = Observable.create(new ObservableOnSubscribe<String>() {
            @Override
            public void subscribe(ObservableEmitter<String> emitter) throws Exception {

            }
        }).publish();
        //通过ConnectableObservable将Cold Observable转化成Hot Observable
        mConnectableObservable.connect();
        //将Hot Observable 转换成Cold Observable
        mConnectableObservable.refCount();

        //或者用Observable的share操作符
        Observable observable=Observable.create(new ObservableOnSubscribe<String>() {
            @Override
            public void subscribe(ObservableEmitter<String> emitter) throws Exception {

            }
        }).share();
```

- Flowable
  - 在RxJava2.x中，Observable不再支持背压，而改由Flowable来支持非阻塞式背压
  - Flowable的所有操作符都是强制支持背压
- Single
  - 只有onSuccess和onError，没有onComplete，这是最大的区别
- Completable
  - 创建后不会发送任何数据
  - 只有onComplete和onError方法
- Maybe
  - 可以看成是Single和Completable的结合体
  - 没有onNext方法，需要通过onSuccess来发送数据
  - 只能发送0或1个数据

#### 2、Subject和Processor

- Subject
  - 可以将Subject看做一个桥梁或者代理
  - Subject不是线程安全的，如果想要其安全，西药调用toSerialized()方法

|     Subject     |                  发射行为                  |
| :-------------: | :----------------------------------------: |
|  AsyncSubject   | 不论订阅发生在什么时候，只发射最后一个数据 |
| BehaviorSubject | 发送订阅之前的一个数据和订阅之后的全部数据 |
|  ReplaySubject  |   不论订阅发生在生么时候，都发射全部数据   |
| PublishSubject  |           发送订阅之后的全部数据           |

- Processor
  - Processor 和 Subject作用相同，Processor是Rx2.0新增
  - 他是一个接口，继承自Subscriber。Publisher，能够支持背压，这是二者最大的区别
- Reactive Streams JVM接口由以下四个接口组成
  - Publisher：消息发布者
  - Subscriber：消息订阅者
  - Subscription：一个订阅关系
  - Processor：Publisher+Subscriber的结合体
- Subject灵活但是伴随风险，Subject不是线程安全的，但是很多开源库都在用，例如RxLifecycle使用了BehaviorSubject

---



### 三、操作符 

#### 【0】操作符大全

![操作符总览](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/NIbuFFS4Eks0ERejjqdEa0OuZ9HOD5KbjZ.NU6yGX44!/r/dDYBAAAAAAAA)

ReactiveX的每种编程语言的实现都实现了一组操作符的集合。不同的实现之间有很多重叠的部分，也有一些操作符只存在特定的实现中。每种实现都倾向于用那种编程语言中他们熟悉的上下文中相似的方法给这些操作符命名。

本文首先会给出ReactiveX的核心操作符列表和对应的文档链接，后面还有一个决策树用于帮助你根据具体的场景选择合适的操作符。最后有一个语言特定实现的按字母排序的操作符列表。

如果你想实现你自己的操作符，可以参考这里：[`实现自定义操作符`](https://mcxiaoke.gitbooks.io/rxdocs/content/topics/Implementing-Your-Own-Operators.html)

#### 【1】创建操作

用于创建Observable的操作符

- [`Create`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Create.html) — 通过调用观察者的方法从头创建一个Observable
- [`Defer`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Defer.html) — 在观察者订阅之前不创建这个Observable，为每一个观察者创建一个新的Observable
- [`Empty/Never/Throw`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Empty.html) — 创建行为受限的特殊Observable
- [`From`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/From.html) — 将其它的对象或数据结构转换为Observable
- [`Interval`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Interval.html) — 创建一个定时发射整数序列的Observable
- [`Just`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Just.html) — 将对象或者对象集合转换为一个会发射这些对象的Observable
- [`Range`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Range.html) — 创建发射指定范围的整数序列的Observable
- [`Repeat`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Repeat.html) — 创建重复发射特定的数据或数据序列的Observable
- [`Start`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Start.html) — 创建发射一个函数的返回值的Observable
- [`Timer`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Timer.html) — 创建在一个指定的延迟之后发射单个数据的Observable

#### 【3】变换操作

这些操作符可用于对Observable发射的数据进行变换，详细解释可以看每个操作符的文档

- [`Buffer`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Buffer.html) — 缓存，可以简单的理解为缓存，它定期从Observable收集数据到一个集合，然后把这些数据集合打包发射，而不是一次发射一个
- [`FlatMap`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/FlatMap.html) — 扁平映射，将Observable发射的数据变换为Observables集合，然后将这些Observable发射的数据平坦化的放进一个单独的Observable，可以认为是一个将嵌套的数据结构展开的过程。
- [`GroupBy`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/GroupBy.html) — 分组，将原来的Observable分拆为Observable集合，将原始Observable发射的数据按Key分组，每一个Observable发射一组不同的数据
- [`Map`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Map.html) — 映射，通过对序列的每一项都应用一个函数变换Observable发射的数据，实质是对序列中的每一项执行一个函数，函数的参数就是这个数据项
- [`Scan`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Scan.html) — 扫描，对Observable发射的每一项数据应用一个函数，然后按顺序依次发射这些值
- [`Window`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Window.html) — 窗口，定期将来自Observable的数据分拆成一些Observable窗口，然后发射这些窗口，而不是每次发射一项。类似于Buffer，但Buffer发射的是数据，Window发射的是Observable，每一个Observable发射原始Observable的数据的一个子集

#### 【4】过滤操作

这些操作符用于从Observable发射的数据中进行选择

- [`Debounce`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Debounce.html) — 只有在空闲了一段时间后才发射数据，通俗的说，就是如果一段时间没有操作，就执行一次操作
- [`Distinct`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Distinct.html) — 去重，过滤掉重复数据项
- [`ElementAt`](https://mcxiaoke.gitbooks.io/rxdocs/content/ElementAt.md) — 取值，取特定位置的数据项
- [`Filter`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Filter.html) — 过滤，过滤掉没有通过谓词测试的数据项，只发射通过测试的
- [`First`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/First.html) — 首项，只发射满足条件的第一条数据
- [`IgnoreElements`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/IgnoreElements.html) — 忽略所有的数据，只保留终止通知(onError或onCompleted)
- [`Last`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Last.html) — 末项，只发射最后一条数据
- [`Sample`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Sample.html) — 取样，定期发射最新的数据，等于是数据抽样，有的实现里叫ThrottleFirst
- [`Skip`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Skip.html) — 跳过前面的若干项数据
- [`SkipLast`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/SkipLast.html) — 跳过后面的若干项数据
- [`Take`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Take.html) — 只保留前面的若干项数据
- [`TakeLast`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/TakeLast.html) — 只保留后面的若干项数据

#### 【5】组合操作

组合操作符用于将多个Observable组合成一个单一的Observable

- [`And/Then/When`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/And.html) — 通过模式(And条件)和计划(Then次序)组合两个或多个Observable发射的数据集
- [`CombineLatest`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/CombineLatest.html) — 当两个Observables中的任何一个发射了一个数据时，通过一个指定的函数组合每个Observable发射的最新数据（一共两个数据），然后发射这个函数的结果
- [`Join`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Join.html) — 无论何时，如果一个Observable发射了一个数据项，只要在另一个Observable发射的数据项定义的时间窗口内，就将两个Observable发射的数据合并发射
- [`Merge`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Merge.html) — 将两个Observable发射的**数据组合**并成一个
- [`StartWith`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/StartWith.html) — 在发射原来的Observable的数据序列之前，先发射一个指定的数据序列或数据项
- [`Switch`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Switch.html) — 将一个发射Observable序列的Observable转换为这样一个Observable：它逐个发射那些Observable最近发射的数据
- [`Zip`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Zip.html) — 打包，使用一个指定的函数将多个Observable发射的**数据组合**在一起，然后将这个函数的结果作为单项数据发射

#### 【6】错误处理

这些操作符用于从错误通知中恢复

- [`Catch`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Catch.html) — 捕获，继续序列操作，将错误替换为正常的数据，从onError通知中恢复
- [`Retry`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Retry.html) — 重试，如果Observable发射了一个错误通知，重新订阅它，期待它正常终止

#### 【7】辅助操作

一组用于处理Observable的操作符

- [`Delay`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Delay.html) — 延迟一段时间发射结果数据
- [`Do`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Do.html) — 注册一个动作占用一些Observable的生命周期事件，相当于Mock某个操作
- [`Materialize/Dematerialize`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Materialize.html) — 将发射的数据和通知都当做数据发射，或者反过来
- [`ObserveOn`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/ObserveOn.html) — 指定观察者观察Observable的调度程序（工作线程）
- [`Serialize`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Serialize.html) — 强制Observable按次序发射数据并且功能是有效的
- [`Subscribe`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Subscribe.html) — 收到Observable发射的数据和通知后执行的操作
- [`SubscribeOn`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/SubscribeOn.html) — 指定Observable应该在哪个调度程序上执行
- [`TimeInterval`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/TimeInterval.html) — 将一个Observable转换为发射两个数据之间所耗费时间的Observable
- [`Timeout`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Timeout.html) — 添加超时机制，如果过了指定的一段时间没有发射数据，就发射一个错误通知
- [`Timestamp`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Timestamp.html) — 给Observable发射的每个数据项添加一个时间戳
- [`Using`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Using.html) — 创建一个只在Observable的生命周期内存在的一次性资源

#### 【8】条件和布尔操作

这些操作符可用于单个或多个数据项，也可用于Observable

- [`All`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Conditional.html#All) — 判断Observable发射的所有的数据项是否都满足某个条件
- [`Amb`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Conditional.html#Amb) — 给定多个Observable，只让第一个发射数据的Observable发射全部数据
- [`Contains`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Conditional.html#Contains) — 判断Observable是否会发射一个指定的数据项
- [`DefaultIfEmpty`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Conditional.html#DefaultIfEmpty) — 发射来自原始Observable的数据，如果原始Observable没有发射数据，就发射一个默认数据
- [`SequenceEqual`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Conditional.html#SequenceEqual) — 判断两个Observable是否按相同的数据序列
- [`SkipUntil`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Conditional.html#SkipUntil) — 丢弃原始Observable发射的数据，直到第二个Observable发射了一个数据，然后发射原始Observable的剩余数据
- [`SkipWhile`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Conditional.html#SkipWhile) — 丢弃原始Observable发射的数据，直到一个特定的条件为假，然后发射原始Observable剩余的数据
- [`TakeUntil`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Conditional.html#TakeUntil) — 发射来自原始Observable的数据，直到第二个Observable发射了一个数据或一个通知
- [`TakeWhile`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Conditional.html#TakeWhile) — 发射原始Observable的数据，直到一个特定的条件为真，然后跳过剩余的数据

#### 【9】算术和聚合操作

这些操作符可用于整个数据序列

- [`Average`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Mathematical.html#Average) — 计算Observable发射的数据序列的平均值，然后发射这个结果
- [`Concat`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Mathematical.html#Concat) — 不交错的连接多个Observable的数据
- [`Count`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Mathematical.html#Count) — 计算Observable发射的数据个数，然后发射这个结果
- [`Max`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Mathematical.html#Max) — 计算并发射数据序列的最大值
- [`Min`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Mathematical.html#Min) — 计算并发射数据序列的最小值
- [`Reduce`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Mathematical.html#Reduce) — 按顺序对数据序列的每一个应用某个函数，然后返回这个值
- [`Sum`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Mathematical.html#Sum) — 计算并发射数据序列的和

#### 【10】连接操作

一些有精确可控的订阅行为的特殊Observable

- [`Connect`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Connect.html) — 指示一个可连接的Observable开始发射数据给订阅者
- [`Publish`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Publish.html) — 将一个普通的Observable转换为可连接的
- [`RefCount`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/RefCount.html) — 使一个可连接的Observable表现得像一个普通的Observable
- [`Replay`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Replay.html) — 确保所有的观察者收到同样的数据序列，即使他们在Observable开始发射数据之后才订阅

#### 【11】转换操作

- [`To`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/To.html) — 将Observable转换为其它的对象或数据结构
- [`Blocking`](https://mcxiaoke.gitbooks.io/rxdocs/content/operators/Blocking-Observable-Operators.html) 阻塞Observable的操作符

#### 【12】操作符决策树

几种主要的需求

- 直接创建一个Observable（创建操作）
- 组合多个Observable（组合操作）
- 对Observable发射的数据执行变换操作（变换操作）
- 从Observable发射的数据中取特定的值（过滤操作）
- 转发Observable的部分值（条件/布尔/过滤操作）
- 对Observable发射的数据序列求值（算术/聚合操作）

#### 【13】链接

- https://www.jianshu.com/p/904c14d253ba

----



### 四、线程操作

#### 1、调度器(Scheduler)种类

- RxJava线程介绍：

  - RxJava是一个为异步编程而实现的库，异步是其重要特色，但是异步也会带来线程安全问题，而且**异步不等于并发**，与异步概念相对应的事同步

  - 默认情况下，RxJava只在当前线程中运行，它是单线程的，然而，函数响应式的实际应用是大部分操作都在后台处理，前台相应的一个过程
  - Observable生成发射数据流，Operators加工数据流在后台线程中进行，Observer在前台线程中接收并响应数据，此时会涉及到多线程来来操作RxJava，我们可以使用RxJava的调度器Scheduler来实现
- Scheduler
  - Schedulers是一个静态工厂类，他有很多不同类型的Scheduler
  - Scheduler是RxJava的线程任务带哦肚脐，Worker是线程任务的具体执行者‘Scheduler创建Worker，然后调用worker的schedule()、……方法来执行任务
  - Worker也是一个抽象类，每种Scheduler会对应一种具体的Worker
  - 如果内置的Scheduler不能满足需求，还可以自定义Executor作为调度器来满足个性化需求

| 调度器类型                     | 说明                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| Schedulers.computation()       | 用于计算任务（**CPU密集型任务**），如事件循环或和回调处理，不要用于IO操作(IO操作请使用Schedulers.io())；默认线程数等于处理器的数量 |
| Schedulers.io()                | 用于**IO密集型任务**，如异步阻塞IO操作，这个调度器的线程池会根据需要增长；对于普通的计算任务，请使用Schedulers.computation()；Schedulers.io( )默认是一个CachedThreadScheduler，很像一个有线程缓存的新线程调度器,io的内部实现是用一个无数量上限的线程池，可以重用空闲的线程，因此多数环境下，io()，比newThread()，更高效 |
| Schedulers.from(executor)      | 使用指定的Executor作为调度器                                 |
| Schedulers.newThread()         | 为每个任务创建一个新线程                                     |
| Schedulers.immediate()         | 在当前线程立即开始执行任务                                   |
| Schedulers.trampoline()        | 当其它排队的任务完成后，在当前线程排队开始执行               |
| Schedulers.single()            | 使用定长为1的线程池(new Scheduled Thread Pool(1))，重复利用这个线程 |
| AndroidSchedulers.mainThread() | Android主线程，RxAndroid特有                                 |

#### 2、subscribeOn & observeOn

- subscribeOn**【上游】**

  ```java
  Observable<T> subscribeOn(Scheduler scheduler) 
  ```

  - subscribeOn通过接收一个Scheduler参数，来指定**对数据的处理**运行在特定的线程调度器Scheduler上。
    若多次设定，则**只有一次起作用**。
  - **在Rxava的链式操作中，数据的处理是【自下而上】的，这点与数据发射正好相反，如果多次调用subscribeOn，则注意上面的线程切换最晚执行，所以就变成了只有第一次切换有效**

- observeOn**【下游】**

  ```java
  Observable<T> observeOn(Scheduler scheduler)
  ```

  - observeOn同样接收一个Scheduler参数，来指定**下游操作**运行在特定的线程调度器Scheduler上。
    若多次设定，**每次均起作用，线程会一直切换**。

- example

```java
如果我们有一段这样的序列
Observable
.map                    // 操作1
.flatMap                // 操作2
.subscribeOn(io)
.map                    //操作3
.flatMap                //操作4
.observeOn(main)
.map                    //操作5
.flatMap                //操作6
.subscribeOn(io)        //!!特别注意
.subscribe(handleData)

假设这里我们是在主线程上调用这段代码，那么
操作1，操作2是在io线程上，因为之后subscribeOn切换了线程
操作3，操作4也是在io线程上，因为在subscribeOn切换了线程之后，并没有发生改变。
操作5，操作6是在main线程上，因为在他们之前的observeOn切换了线程。
特别注意那一段，对于操作5和操作6是无效的
再简单点总结就是:
subscribeOn的调用切换之前的线程。
observeOn的调用切换之后的线程。
observeOn之后，不可再调用subscribeOn 切换线程
```

#### 3、线程切换原理

- https://www.jianshu.com/p/a9ebf730cd08



### 五、RxJava背压

> 【==异步、生产速度大于消耗==】

https://www.jianshu.com/p/ff8167c1d191/

- 同步异步：观察者和被观察者是否处于同一线程里

- 在同一线程中只会因为处理不及时产生**ANR**，二者不在同一线程中时才会出现背压，因为在RxJava1.x中，Buffer的大小只有16
- RxJava1.x中Hot Observable是不支持背压的，Cold Observable也有一部分不支持
- RxJava1.x：
  - 过滤限流：通过使用限流操作符将被观察者产生的大部分事件过滤并抛弃间接降低事件发送速度，达到限流目的
  - 打包缓存：；来不及处理，打包缓存，慢慢处理
  - 使用操作符来转化
- RxJava2.x：
  - **Observable不再支持背压，而是改用Flowable来专门支持背压**，默认队列大小128，所有操作符都支持背压
  - BackPressureStrategy的源码可以看到，Flowable支持5钟背压策略

```java
		Observable
                .create(new ObservableOnSubscribe<Integer>() {
                    @Override
                    public void subscribe(ObservableEmitter<Integer> e) throws Exception {
                        int i = 0;
                        while (true) {
                            i++;
                            e.onNext(i);
                        }
                    }
                })
                .subscribeOn(Schedulers.newThread())
                .observeOn(Schedulers.newThread())
                .subscribe(new Consumer<Integer>() {
                    @Override
                    public void accept(Integer integer) throws Exception {
                        Thread.sleep(5000);
                        System.out.println(integer);
                    }
                });
```

- 创建一个可观察对象Observable在Schedulers.newThread()的线程中不断发送数据，而观察者Observer在Schedulers.newThread()的另一个线程中每隔5秒接收打印一条数据。

- 由于上游通过Observable发射数据的速度大于下游通过Consumer接收处理数据的速度，而且上下游分别运行在不同的线程中，下游对数据的接收处理不会堵塞上游对数据的发射，造成上游数据积压，内存不断增加，最后便会导致内存溢出。

- **只有在需要处理背压问题时，才需要使用Flowable。否则影响性能**

- 类似于Observable,在使用Flowable时，也可以通过create操作符创建发射数据流，代码如下：

```java
		Flowable
                .create(new FlowableOnSubscribe<Integer>() {
                    @Override
                    public void subscribe(FlowableEmitter<Integer> e) throws Exception {
                        System.out.println("发射----> 1");
                        e.onNext(1);
                        System.out.println("发射----> 2");
                        e.onNext(2);
                        System.out.println("发射----> 3");
                        e.onNext(3);
                        System.out.println("发射----> 完成");
                        e.onComplete();
                    }
                }, BackpressureStrategy.BUFFER) //create方法中多了一个BackpressureStrategy类型的参数
                .subscribeOn(Schedulers.newThread())//为上下游分别指定各自的线程
                .observeOn(Schedulers.newThread())
                .subscribe(new Subscriber<Integer>() {
                    @Override
                    public void onSubscribe(Subscription s) {   //onSubscribe回调的参数不是Disposable而是Subscription
                        s.request(Long.MAX_VALUE);            //注意此处，暂时先这么设置
                    }

                    @Override
                    public void onNext(Integer integer) {
                        System.out.println("接收----> " + integer);
                    }

                    @Override
                    public void onError(Throwable t) {
                    }

                    @Override
                    public void onComplete() {
                        System.out.println("接收----> 完成");
                    }
                });

System.out: 发射----> 1
System.out: 发射----> 2
System.out: 发射----> 3
System.out: 发射----> 完成
System.out: 接收----> 1
System.out: 接收----> 2
System.out: 接收----> 3
System.out: 接收----> 完成
```

- 发射与处理数据流在形式上与Observable大同小异，发射器中均有onNext，onError，onComplete方法，订阅器中也均有onSubscribe，onNext，onError，onComplete方法。

- 但是在细节方面还是有三点不同

  - create方法中多了一个**BackpressureStrategy**类型的参数。
  - 订阅器Subscriber中，方法onSubscribe回调的参数不是Disposable而是**Subscription**，多了行代码：

  ```java
  s.request(Long.MAX_VALUE);
  ```

  - Flowable发射数据时，使用特有的发射器**FlowableEmitter**，不同于Observable的ObservableEmitter

    正是这三点不同赋予了Flowable不同于Observable的特性。

- 背压策略：
  - **BackpressureStrategy.MISSING**
    - 通过creat创建Flowable并没有指定背压策略，需要下游通过背压操作符指定背压策略
  - **BackpressureStrategy.ERROR**
    - 超出requested（128）的值报错MissingBackpressureException
  - **BackpressureStrategy.BUFFER**
    - 放大requested的值，可以无限制的添加数据，不会报异常，但是会OOM
  - **BackpressureStrategy.DROP**
    - 超出request的的值，会被丢弃
  - **BackpressureStrategy.LATEST**
    - 超出request的的值，同上，但是不管缓存池的状态如何，会将最后一条数据放入缓存池中
    - 循环999次发送数据，会打印0~127 ，999



### 六、Disposable和Transformer的使用

#### 1、Disposable

- **RxJava1.x中：**

  - Subscription的接口可以用来取消订阅
  - Observable.subscribe()方法会返回一个Subscription的UI想，也就是每次订阅都会返回Subscription，只需调用Subscription.unSubscribe()就可以解除订阅
- **RxJava2.x中：**
  - Subscription被改名为Disposable
  - RxJava1.x中有一个符合订阅(composite Subscription)的概念。在RxJava2.x中，RxJava也内置了一个类似符合订阅的容器**CompositeDisposable**，每得到医德Disposable时就add到CompositeDisposable，退出时调用clear()方法
  - 可以使用Disposable管理一个或者使用CompositeDisposable来管理多个订阅
- RxLifecycle
  - 当涉及到绑定Observable到Activity或者Fragment的生命周期时，要么指定Observable应该终止的生命周期事件，要么让RxLifecycle自己决定何时终止Observable序列
  - 默认RxLifecycle在辅助生命周期事件终止Observable
  - Activity的OnCreate()订阅，OnDestory终止Observable序列
  - Fragment的OnAttach()，订阅，OnDetach终止Observable序列

- AutoDispose

#### 2、Transformer

- Transformer是转换器的意思，Transformer能够将一个**Observable/Flowable……转换成另一个Observable/Flowable……对象**，与调用一系列的内敛操作符一样

```java
//封装一下线程切换 
public static <T> ObservableTransformer<T, T> transformerThread() {
        return new ObservableTransformer<T, T>() {
            @Override
            public ObservableSource<T> apply(Observable<T> upstream) {
                return upstream.subscribeOn(Schedulers.io())
                        .observeOn(Schedulers.mainThread());
            }
        };
    }
```

- [【译】避免打断链式结构：使用.compose( )操作符](https://www.jianshu.com/p/e9e03194199e)

- 不同于map、flatMap等lift操作改变Observable发布的事件及序列，compose操作符是直接对当前Observable进行操作（可简单理解为不停地.方法名（）.方法名（）链式操作当前Observable），所以我们自然可以把切换线程的操作加入这里。

