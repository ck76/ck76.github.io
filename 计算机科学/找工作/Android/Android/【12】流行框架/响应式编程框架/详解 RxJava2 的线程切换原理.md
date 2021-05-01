### 读了这篇文章你将会收获什么

- RxJava2 基本的运行流程(并不会详述)
- RxJava2 线程切换原理
- 为什么 subscribeOn() 只有第一次切换有效
- RxAndroid 简单分析

> PS：建议您对 RxJava 有一些了解或使用经验再看此文章，推荐结合源码品尝
> RxJava入门文章 [给 Android 开发者的 RxJava 详解-扔物线([http://gank.io/post/560e15be2dca930e00da1083](https://links.jianshu.com/go?to=http%3A%2F%2Fgank.io%2Fpost%2F560e15be2dca930e00da1083))

然后贴一下本篇文章分析的示例代码



```tsx
CompositeDisposable comDisposable = new CompositeDisposable();

protected void test() {
        Observable<String> observable = Observable
                .create(new ObservableOnSubscribe<String>() {
                    @Override
                    public void subscribe(@NonNull ObservableEmitter<String> emitter) throws
                            Exception {
                        emitter.onNext("hello");
                    }
                })
                .map(new Function<String, String>() {
                    @Override
                    public String apply(String s) throws Exception {
                        return s;
                    }
                })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread());

        observable.subscribe(new Observer<String>() {
            @Override
            public void onSubscribe(Disposable d) {
                comDisposable.add(d);
            }

            @Override
            public void onNext(String s) {
                Log.i(TAG, s);
            }

            @Override
            public void onError(Throwable e) {

            }

            @Override
            public void onComplete() {

            }
        });
}
```

------

### RxJava2 基本的运行流程

根据上述源码分析出流程图，这里颜色相同的代表同一对象。根据流程图看一遍源码基本流程就能理通

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyr4vrv3ij30ve0u041x.jpg)

RxJava2 线程切换原理流程图

### RxJava2 线程切换原理

> RxJava 切换线程怎么用我就不多说了请参考我的另一篇文章 [Android:随笔——RxJava的线程切换](https://www.jianshu.com/p/d9da64774f7b)

#### 一、observeOn() 的线程切换原理

根据运行流程来看 observeOn() 执行后是得到 ObservableObserveOn 对象，那么当 ObservableObserveOn 绑定监听者的时候要运行 subscribe() 方法



```java
public final void subscribe(Observer<? super T> observer) {
    ObjectHelper.requireNonNull(observer, "observer is null");
    try {
        observer = RxJavaPlugins.onSubscribe(this, observer);
        ObjectHelper.requireNonNull(observer, "Plugin returned null Observer");
        //调用 subscribeActual()
        subscribeActual(observer);
    } catch (NullPointerException e) { // NOPMD
        throw e;
    } catch (Throwable e) {
        ...
    }
}
```

接下来我们看一下 subscribeActual() 方法



```java
protected void subscribeActual(Observer<? super T> observer) {
    if (scheduler instanceof TrampolineScheduler) {
        source.subscribe(observer);
    } else {
        //scheduler 是传进来的线程调度对象，如 Schedulers.io() 、AndroidSchedulers.mainThread() 等,这里调用了 createWorker() 方法暂时看一下就好稍后分析 RxAndroid 会说明 
        Scheduler.Worker w = scheduler.createWorker();
        //我们看到他把 w 参数传进去了
        source.subscribe(new ObserveOnObserver<T>(observer, w, delayError, bufferSize));
    }
}
```

从上述代码我们可以看到 ObservableObserveOn 是被 ObserveOnObserver 监听的，所以收到通知也是由 ObserveOnObserver 作出响应，接下来我们假设当 Rxjava 发送 onNext 通知时会调用 ObserveOnObserver 的 onNext() 方法 ( PS:当然如果是 onComplete()、onError() 等也是一样的逻辑 )，然后我们来看一看 ObserveOnObserver 的 onNext() 方法，



```cpp
@Override
public void onNext(T t) {
    if (done) {
        return;
    }
    if (sourceMode != QueueDisposable.ASYNC) {
        queue.offer(t);
    }
    //切换线程
    schedule();
}

void schedule() {
    if (getAndIncrement() == 0) {
        //直接调用了 worker 的 schedule 方法，需要注意的是这里他把自己传了进去
        worker.schedule(this);
    }
}
```

现在我先把把 schedule(Runnable run) 贴出来



```kotlin
public Disposable schedule(@NonNull Runnable run) {
    return schedule(run, 0L, TimeUnit.NANOSECONDS);
}
```

1. 我们看到这个他接收的参数是一个 Runnable，这是怎么回事呢，我们看一下 ObserveOnObserver 对象，他不但实现了 Observer 接口并且也实现了 Runnable 接口
2. 接下看，继续调用 schedule( Runnable action, long delayTime, TimeUnit unit) 方法，但是这个方法是个抽象方法，这里我们就假设这里这个 worker 是 IO 线程，所以我直接贴 IoScheduler 的代码了



```kotlin
public Disposable schedule(@NonNull Runnable action, long delayTime, @NonNull TimeUnit unit) {
     if (tasks.isDisposed()) {
         // don't schedule, we are unsubscribed
         return EmptyDisposable.INSTANCE;
     }
     return threadWorker.scheduleActual(action, delayTime, unit, tasks);
}
```

然后再贴一下 scheduleActual 的方法



```php
public ScheduledRunnable scheduleActual(final Runnable run, long delayTime, @NonNull TimeUnit unit, @Nullable DisposableContainer parent) {
    Runnable decoratedRun = RxJavaPlugins.onSchedule(run);
    //就是个 Runnable
    ScheduledRunnable sr = new ScheduledRunnable(decoratedRun, parent);
        
    if (parent != null) {
        if (!parent.add(sr)) {
            return sr;
        }
    }

    Future<?> f;
    try {
        //判断延迟时间，然后使用线程池运行 Runnable
        if (delayTime <= 0) {
            f = executor.submit((Callable<Object>)sr);
        } else {
            f = executor.schedule((Callable<Object>)sr, delayTime, unit);
        }
        sr.setFuture(f);
    } catch (RejectedExecutionException ex) {
        if (parent != null) {
            parent.remove(sr);
        }
        RxJavaPlugins.onError(ex);
    }
    return sr;
}
```

这样一来就会在相应的线程中运行 ObserveOnObserver 的 run 方法



```cpp
public void run() {
    //这个地方具体的我还没有搞明白，大概就是在这个方法里调用 onNext() ，然后 observeOn() 操作符之后的监听者的运行线程就变了
    if (outputFused) {
        drainFused();
    } else {
        drainNormal();
        }
    }
```

##### 二、subscribeOn() 的线程切换原理

> PS:这个切换原理其实和 observeOn() 原理很像

跟 observeOn() 一样，只不过这个操作的对象是 ObservableSubscribeOn， 这个对象也是同样的代码逻辑，运行 subscribe() 方法，然后调用 subscribeActual() 方法，所以就直接贴 subscribeActual() 的代码



```php
public void subscribeActual(final Observer<? super T> s) {
    //创建与之绑定的 SubscribeOnObserver
    final SubscribeOnObserver<T> parent = new SubscribeOnObserver<T>(s);
    s.onSubscribe(parent);
    //1. 创建 SubscribeTask 实际上就是个 Runnable
    //2. 然后调用 scheduler.scheduleDirect 方法
    parent.setDisposable(scheduler.scheduleDirect(new SubscribeTask(parent)));
}
```

我们看一下 scheduleDirect 的方法



```kotlin
public Disposable scheduleDirect(@NonNull Runnable run) {
    return scheduleDirect(run, 0L, TimeUnit.NANOSECONDS);
}

public Disposable scheduleDirect(@NonNull Runnable run, long delay, @NonNull TimeUnit unit) {
    final Worker w = createWorker();
    final Runnable decoratedRun = RxJavaPlugins.onSchedule(run);
    //一个 Runnable 具体作用没分析
    DisposeTask task = new DisposeTask(decoratedRun, w);
    //这个代码看着熟悉吗  没错上面 observeOn 提到过，知道它是运行 Runnable 我们就直接看 Runnable 里面的 run() 了
    w.schedule(task, delay, unit);
    return task;
}
```

我们看一下 DisposeTask 的 run()



```csharp
public void run() {
    runner = Thread.currentThread();
    try {
        decoratedRun.run();
    } finally {
        dispose();
        runner = null;
    }
}
```

调来调去我们又回到了 SubscribeTask 的 run()



```cpp
public void run() {
    source.subscribe(parent);
}
```

这个地方的运行线程已经被切换了，他又开始往上一层层的去订阅，所以 create(new ObservableOnSubscribe<String>(){}）这个匿名实现接口运行 subscribe 的线程运行环境都被改变了，再去调用 onNext() 等方法线程环境也是被改变的

##### 为什么 subscribeOn() 只有第一次切换有效

写到这里我们这个问题也就能回答了
因为 RxJava 最终能影响 ObservableOnSubscribe 这个匿名实现接口的运行环境的只能是最后一次运行的 subscribeOn() ，又因为 RxJava 订阅的时候是从下往上订阅，所以从上往下第一个 subscribeOn() 就是最后运行的，这就造成了写多个 subscribeOn() 并没有什么乱用的现象。

------

### 分析一下 RxAndroid

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyr4s5f5zj30gm07a0tf.jpg)

RxAndroid 源码



其实 RxAndroid 里面并没有什么复杂的代码，他其实只是提供一个能切换到 Android 主线程线程调度器。

其实它的原理和 RxJava 自带的那些线程调度器一样，如果你想了解 RxJava 的 IO 线程池，什么的可以自己看一看，我这里分析 RxAndroid 主要有以下几点原因

1. 弄清楚 RxAndroid 这个库的具体作用
2. 弄清楚他是怎么就能把线程切换到主线程（他是怎么提供的主线程环境）
3. 弄清楚线程调度器的运行原理
4. 最重要的是它相对于 RxJava 自带的那些调度器，他比较简单容易分析

正文开始

首先我们找一下入口 AndroidSchedulers.mainThread() 这个地方应该是就是入口了，我们看一下 AndroidSchedulers 这个类的源码吧，总共也没几行



```java
private static final class MainHolder {
    static final Scheduler DEFAULT = new HandlerScheduler(new Handler(Looper.getMainLooper()));
    }

    private static final Scheduler MAIN_THREAD = RxAndroidPlugins.initMainThreadScheduler(
        new Callable<Scheduler>() {
            @Override public Scheduler call() throws Exception {
                return MainHolder.DEFAULT;
            }
        }
    );

    public static Scheduler mainThread() {
        return RxAndroidPlugins.onMainThreadScheduler(MAIN_THREAD);
    }

    public static Scheduler from(Looper looper) {
        if (looper == null) throw new NullPointerException("looper == null");
        return new HandlerScheduler(new Handler(looper));
    }
```

这个应该不用我多说大家都能看明白，看到这里我们基本上明白了 RxAndroid 就是通过 Handler 来拿到主线程的

我们拿 subscribeOn() 中的一些流程来说



```java
public Disposable scheduleDirect(@NonNull Runnable run, long delay, @NonNull TimeUnit unit) {
    final Worker w = createWorker();
    final Runnable decoratedRun = RxJavaPlugins.onSchedule(run);
    DisposeTask task = new DisposeTask(decoratedRun, w);
    w.schedule(task, delay, unit);
    return task;
}
```

首先我们看到调用了 createWorker() 这是个抽象方法我们找到具体实现类 HandlerScheduler



```cpp
public Worker createWorker() {
    return new HandlerWorker(handler);
}
```

单纯的创建一个 Worker 并把主线程的 Handler 传进去，然后调用 Worker 的 schedule() 方法



```cpp
public Disposable schedule(Runnable run, long delay, TimeUnit unit) {
    /**忽略一些代码**/
    run = RxJavaPlugins.onSchedule(run);

    ScheduledRunnable scheduled = new ScheduledRunnable(handler, run);

    Message message = Message.obtain(handler, scheduled);
    message.obj = this; // Used as token for batch disposal of this worker's runnables.

    handler.sendMessageDelayed(message, unit.toMillis(delay));

    if (disposed) {
          handler.removeCallbacks(scheduled);
          return Disposables.disposed();
    }
    return scheduled;
}
```

到这里看明白 RxJava 如何通过 RxAndroid 来切换到主线程运行，其实 RxAndroid 的核心就是 Handler

------

### 总结

本篇参考 RxJava 2.1.12 与 RxAndroid:2.0.2 源码

1. observeOn() 只是在收到 onNext() 等消息的时候改变了从下一个开始的操作符的线程运行环境。
2. subscribeOn() 线程切换是在 subscribe() 订阅的时候切换，他会切换他下面订阅的操作符的运行环境，因为订阅的过程是自下而上的，所以第一个出现的 subscribeOn() 操作符反而是最后一次运行的。

observeOn()、subscribeOn() 没有任何先后顺序的问题。

> 不得不说 Handler 在安卓中的地位真的是很牛逼
> 见解不到的地方欢迎大家指出



作者：QuincySx
链接：https://www.jianshu.com/p/a9ebf730cd08
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。