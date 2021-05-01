[TOC]

本文基于com.squareup.leakcanary:leakcanary-android:1.3

# 用法



```java
build.gradle 配置
 debugCompile 'com.squareup.leakcanary:leakcanary-android:1.3'
    releaseCompile 'com.squareup.leakcanary:leakcanary-android-no-op:1.3'

public class ExampleApplication extends Application {

    private RefWatcher mRefWatcher;

    @Override
    public void onCreate() {
        super.onCreate();
        mRefWatcher = LeakCanary.install(this);
    }
}
```

# 概述

LeakCanary==通过application注册了一个的lifecycleCallbacks==，在activity生命周期的destory时，将activity对象通过set集合、弱引用和引用队列记录起来，五秒之后当主线程空闲时检查，循环引用队列将为空对象的key从set集合中删除，然后判断==集合中==是否有activity对象，有就是没被回收，如果没回收调用gc，重新检测还没被回收，就提示内存泄漏。==这里主要用到了引用队列(引用队列和弱引用联合使用，当弱引用的对象被回收，java虚拟机会将这个对象的引用%%%%%%%%%添加到对应的引用队列中去，这样就有了被回收的对象队列)==。

# 原理

LeakCanary.install(this)方法

```java
 /**
   * Creates a {@link RefWatcher} that works out of the box, and starts watching activity
   * references (on ICS+).
   */
  public static RefWatcher install(Application application) {
    return install(application, DisplayLeakService.class);
  }

  /**
   * Creates a {@link RefWatcher} that reports results to the provided service, and starts watching
   * activity references (on ICS+).
   */
  public static RefWatcher install(Application application,
      Class<? extends AbstractAnalysisResultService> listenerServiceClass) {
    if (isInAnalyzerProcess(application)) {
      return RefWatcher.DISABLED;
    }
    enableDisplayLeakActivity(application);
    HeapDump.Listener heapDumpListener =
        new ServiceHeapDumpListener(application, listenerServiceClass);
    RefWatcher refWatcher = androidWatcher(application, heapDumpListener);
    ActivityRefWatcher.installOnIcsPlus(application, refWatcher);
    return refWatcher;
  }
```

DisplayLeakService.class是通知服务，内部是一个Notification。
 isInAnalyzerProcess 判断进程是否running。
 enableDisplayLeakActivity方法最后调用



```java
static void setEnabled(Context context, Class<?> componentClass, boolean enabled) {
    ComponentName component = new ComponentName(context, componentClass);
    PackageManager packageManager = context.getPackageManager();
    int newState = enabled ? COMPONENT_ENABLED_STATE_ENABLED : COMPONENT_ENABLED_STATE_DISABLED;
    // Blocks on IPC.
    packageManager.setComponentEnabledSetting(component, newState, DONT_KILL_APP);
  }
```

这个方法主要是 packageManager.setComponentEnabledSetting，不杀该组件。
 heapDumpListener用于弹出提醒。

主要内容在ActivityRefWatcher.installOnIcsPlus之中。调用了Application.registerActivityLifecycleCallbacks(Application.ActivityLifecycleCallbacks),注册了一个生命周期回调.在Activity的destory的时候调用refWatcher.watch(activity);refWatcher是前面传进来的。

RefWatcher的构造器



```kotlin
  private final Executor watchExecutor;
  private final DebuggerControl debuggerControl;
  private final GcTrigger gcTrigger;
  private final HeapDumper heapDumper;
  private final Set<String> retainedKeys;
  private final ReferenceQueue<Object> queue;
  private final HeapDump.Listener heapdumpListener;
public RefWatcher(Executor watchExecutor, DebuggerControl debuggerControl, GcTrigger gcTrigger,
      HeapDumper heapDumper, HeapDump.Listener heapdumpListener) {
    this.watchExecutor = checkNotNull(watchExecutor, "watchExecutor");
    this.debuggerControl = checkNotNull(debuggerControl, "debuggerControl");
    this.gcTrigger = checkNotNull(gcTrigger, "gcTrigger");
    this.heapDumper = checkNotNull(heapDumper, "heapDumper");
    this.heapdumpListener = checkNotNull(heapdumpListener, "heapdumpListener");
    retainedKeys = new CopyOnWriteArraySet<>();
    queue = new ReferenceQueue<>();
  }
```

watchExecutor是（AndroidWatchExecutor implements Executor）里面创建了两个Handler，一个拿了主线程的looper，一个拿了自主创建的HandlerThread的looper。AndroidWatchExecutor主要方法是



```java
 private void executeDelayedAfterIdleUnsafe(final Runnable runnable) {
    // This needs to be called from the main thread.
    Looper.myQueue().addIdleHandler(new MessageQueue.IdleHandler() {
      @Override public boolean queueIdle() {
        backgroundHandler.postDelayed(runnable, DELAY_MILLIS);
        return false;
      }
    });
  }
```

这个方法在execute中执行。这时候用到了mainHandler，不在主线程的时候，将此函数放到主线程中（ mainHandler.post(new Runnable()），Looper.myQueue().addIdleHandler这段的意思是在主线程空闲的时候执行queueIdle()
 接下来分析watch（）



```java
 /**
   * Identical to {@link #watch(Object, String)} with an empty string reference name.
   * @see #watch(Object, String)
   */
  public void watch(Object watchedReference) {
    watch(watchedReference, "");
  }

  /**
   * Watches the provided references and checks if it can be GCed. This method is non blocking,
   * the check is done on the {@link Executor} this {@link RefWatcher} has been constructed with.
   *
   * @param referenceName An logical identifier for the watched object.
   */
  public void watch(Object watchedReference, String referenceName) {
    checkNotNull(watchedReference, "watchedReference");
    checkNotNull(referenceName, "referenceName");
    if (debuggerControl.isDebuggerAttached()) {
      return;
    }
    final long watchStartNanoTime = System.nanoTime();
    String key = UUID.randomUUID().toString();
    retainedKeys.add(key);
    final KeyedWeakReference reference =
        new KeyedWeakReference(watchedReference, key, referenceName, queue);

    watchExecutor.execute(new Runnable() {
      @Override public void run() {
        ensureGone(reference, watchStartNanoTime);
      }
    });
  }

  void ensureGone(KeyedWeakReference reference, long watchStartNanoTime) {
    long gcStartNanoTime = System.nanoTime();

    long watchDurationMs = NANOSECONDS.toMillis(gcStartNanoTime - watchStartNanoTime);
    removeWeaklyReachableReferences();
    if (gone(reference) || debuggerControl.isDebuggerAttached()) {
      return;
    }
    gcTrigger.runGc();
    removeWeaklyReachableReferences();
    if (!gone(reference)) {
      long startDumpHeap = System.nanoTime();
      long gcDurationMs = NANOSECONDS.toMillis(startDumpHeap - gcStartNanoTime);

      File heapDumpFile = heapDumper.dumpHeap();

      if (heapDumpFile == null) {
        // Could not dump the heap, abort.
        return;
      }
      long heapDumpDurationMs = NANOSECONDS.toMillis(System.nanoTime() - startDumpHeap);
      heapdumpListener.analyze(
          new HeapDump(heapDumpFile, reference.key, reference.name, watchDurationMs, gcDurationMs,
              heapDumpDurationMs));
    }
  }

  private boolean gone(KeyedWeakReference reference) {
    return !retainedKeys.contains(reference.key);
  }

  private void removeWeaklyReachableReferences() {
    // WeakReferences are enqueued as soon as the object to which they point to becomes weakly
    // reachable. This is before finalization or garbage collection has actually happened.
    KeyedWeakReference ref;
    while ((ref = (KeyedWeakReference) queue.poll()) != null) {
      retainedKeys.remove(ref.key);
    }
  }
```

这里是核心，把destroy的activity编号(UUID.randomUUID().toString())保存到Set<String>集合retainedKeys中，作为它的key，然后把activity也就是watchedReference放入到弱引用KeyedWeakReference中。

###### 这里引出了第一个知识点，弱引用和引用队列ReferenceQueue联合使用时，如果弱引用持有的对象被垃圾回收，Java虚拟机就会把这个弱引用加入到与之关联的引用队列中。即 KeyedWeakReference持有的Activity对象如果被垃圾回收，该对象就会加入到引用队列queue

这个方法挺巧妙的，retainedKeys集合了所有destoryed了的但没有被回收的Activity的key，这个集合可以用来判断一个Activity有没有被回收，但是判断之前需要用removeWeaklyReachableReferences()这个方法更新一下。



作者：我只是一个果壳
链接：https://www.jianshu.com/p/2c94eeab1c07
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---

LeakCanary是一款开源的内存泄漏检查工具，在项目中，可以使用它来检测Activity是否能够被GC及时回收。github的地址为[https://github.com/square/leakcanary](https://link.jianshu.com/?t=https%3A%2F%2Fgithub.com%2Fsquare%2Fleakcanary)

# 使用方式解析

将LeakCanary引入AS，在Application中调用如下方法，可以跟踪Activity是否被GC回收。

![img](https:////upload-images.jianshu.io/upload_images/5328002-b7f2bbb04b53abd2.png?imageMogr2/auto-orient/strip|imageView2/2/w/432)

入口函数

LeakCanary.install()方法的调用流程如下所示：

![img](https:////upload-images.jianshu.io/upload_images/5328002-a48c1a888e934342.png?imageMogr2/auto-orient/strip|imageView2/2/w/709)

install方法调用流程

Install方法如下：

![img](https:////upload-images.jianshu.io/upload_images/5328002-3cf66b659fa4ab67.png?imageMogr2/auto-orient/strip|imageView2/2/w/463)

install方法

其中listenerServiceClass方法传入了展示分析结果的Service(DisplayLeakService)；excludedRefs方法排除了开发中可以忽略的泄漏路径；buildAndInstall是主要的函数，实现了activity是否能被释放的监听。

![img](https:////upload-images.jianshu.io/upload_images/5328002-f11ae74f7a937b99.png?imageMogr2/auto-orient/strip|imageView2/2/w/403)

buildAndInstall

buildAndInstall会调用ActivityRefWatcher.install来监测Activity。

![img](https:////upload-images.jianshu.io/upload_images/5328002-90bf4db434fd8c2c.png?imageMogr2/auto-orient/strip|imageView2/2/w/430)

install

最终调用了watchActivities()：

![img](https:////upload-images.jianshu.io/upload_images/5328002-a8e36a07ca17d8a3.png?imageMogr2/auto-orient/strip|imageView2/2/w/441)

watchActivities

通过registerActivityLifecycleCallbacks来监听Activity的生命周期：

![img](https:////upload-images.jianshu.io/upload_images/5328002-550c4e2150e15b3b.png?imageMogr2/auto-orient/strip|imageView2/2/w/457)

lifecycleCallbacks

lifecycleCallbacks监听Activity的onDestroy方法，正常情况下activity在onDestroy后需要立即被回收，onActivityDestroyed方法最终会调用RefWatcher.watch方法:

![img](https:////upload-images.jianshu.io/upload_images/5328002-c38e11f15b36e959.png?imageMogr2/auto-orient/strip|imageView2/2/w/554)

watch

监测机制利用了Java的WeakReference和ReferenceQueue，通过将Activity包装到WeakReference中，被WeakReference包装过的Activity对象如果被回收，该WeakReference引用会被放到ReferenceQueue中，通过监测ReferenceQueue里面的内容就能检查到Activity是否能够被回收。检查方法如下：

![img](https:////upload-images.jianshu.io/upload_images/5328002-e82c2d39f483b025.png?imageMogr2/auto-orient/strip|imageView2/2/w/532)

ensureGone

1、 首先通过removeWeaklyReachablereference来移除已经被回收的Activity引用

2、 通过gone(reference)判断当前弱引用对应的Activity是否已经被回收，如果已经回收说明activity能够被GC，直接返回即可。

3、 如果Activity没有被回收，调用GcTigger.runGc方法运行GC，GC完成后在运行第1步，然后运行第2步判断Activity是否被回收了，如果这时候还没有被回收，那就说明Activity可能已经泄露。

4、 如果Activity泄露了，就抓取内存dump文件(Debug.dumpHprofData)

![img](https:////upload-images.jianshu.io/upload_images/5328002-497f6e9b76d10eb8.png?imageMogr2/auto-orient/strip|imageView2/2/w/415)

dumpHeap

5、 之后通过HeapAnalyzerService.runAnalysis进行分析内存文件分析

![img](https:////upload-images.jianshu.io/upload_images/5328002-9cb9bdb0a4c9345a.png?imageMogr2/auto-orient/strip|imageView2/2/w/481)

分析dump

接着通过HeapAnalyzer(checkForLeak—findLeakingReference---findLeakTrace)来进行内存泄漏分析。

6、 最后通过DisplayLeakService进行内存泄漏的展示。



作者：JasmineBen
链接：https://www.jianshu.com/p/261e70f3083f
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



---

## 一.引言

LeakCanary我相信大家都不会陌生的，在我们的开发过程中，为了避免内存泄漏的问题，我们可以在我们的项目中集成LeakCanary，来观察我们的应用程序是存在内存泄漏的问题。今天我们就来看看LeakCanary的原理，了解它是怎么去判断我们的应用程序是发生了内存泄漏的。

## 二.源码解析

在LeakCanary的源码中使用到了一个非常关键的数据结构，这个数据结构是LeakCanary判断应用程序是否发生了内存泄漏泄漏的关键。这个数据结构就是ReferenceQueue。

**1.ReferenceQueue**
从名字就可以听出来，ReferenceQueue是一种存放引用的队列。我们知道在Java中有四种引用。
1.强引用（当我们创建一个对象时，默认创建的就是强引用。只要强引用还存在，垃圾回收器就算抛出OOM,也不会回收强引用引用的对象。）
2.软引用（SoftReference,当内存不足时，垃圾回收器会回收被引用的对象。）
3.弱引用（WeakReference，当GC时垃圾回收器会回收掉被引用的对象。）
4.虚引用 (PhantomReference,基本不会用到。)
我们的ReferenceQueue既然是一个存放引用的队列，那它原理是什么呢？我们先来看下WeakReference的源码：

```
public class WeakReference<T> extends Reference<T> {
    public WeakReference(T referent) {
        super(referent);
    }

    
    public WeakReference(T referent, ReferenceQueue<? super T> q) {
        super(referent, q);
    }

}
1234567891011
```

可以看到WeakReference中有两个构造方法，第二个构造方法要求我们传入一个ReferenceQueue类型的对象。这个ReferenceQueue作用是什么呢？其实呢，这个ReferenceQueue对象，会在垃圾收集器即将回收引用对象指向的对象时，将这个引用对象加入这个队列。注意了，引用指向的对象是说的我们在构造WeakReference时构造方法中传的对象，引用对象说的就是我们这个引用本身，两者的概念不要弄混淆了。
举个例子：

```
 ReferenceQueue<Activity> mQueue = new ReferenceQueue<>();
 WeakReference<Activity> mWeakReference = new WeakReference<Activity>(mActivity,mQueue);
12
```

以上面代码举例，如果GC时将mWeakReference指向的mActivity回收的话，同时也会向我们的mQueue中加入我们的mWeakReference。关于ReferenceQueue暂时我们就了解这么多就可以了，有兴趣的朋友可以自己去了解下。

**2.LeakCanary.install(this);**
关于LeakCanary的使用，一般我们都是在Application的onCreate的方法中，调用LeakCanary.install(this)，下面我们就来分析分析通过这一行代码，LeakCanary究竟为我们做了哪些事。

```
  public static RefWatcher install(Application application) {
    return refWatcher(application).listenerServiceClass(DisplayLeakService.class)
        .excludedRefs(AndroidExcludedRefs.createAppDefaults().build())
        .buildAndInstall();
  }
12345
```

（1）调用refWatcher()创建了一个AndroidRefWatcherBuilder。

```
  public static AndroidRefWatcherBuilder refWatcher(Context context) {
    return new AndroidRefWatcherBuilder(context);
  }
1234
```

（2）调用AndroidRefWathcerBuilder的listenerServiceClass方法注册一个回调。

```
 public AndroidRefWatcherBuilder listenerServiceClass(
      Class<? extends AbstractAnalysisResultService> listenerServiceClass) {
      //包装了两层
    return heapDumpListener(new ServiceHeapDumpListener(context, listenerServiceClass));
  }
12345
```

（3）调用AndroidRefWatcherBuilder的excludedRefs方法

```
  @Override protected ExcludedRefs defaultExcludedRefs() {
    return AndroidExcludedRefs.createAppDefaults().build();
  }
1234
```

这里主要是为了排除系统导致的内存泄漏。
（4）buildAndInstall（）

```
public RefWatcher buildAndInstall() {
    //做了判断install方法只能被调用一次
    if (LeakCanaryInternals.installedRefWatcher != null) {
      throw new UnsupportedOperationException("buildAndInstall() should only be called once.");
    }
    //创建一个RefWacher观察对象
    RefWatcher refWatcher = build();
    if (refWatcher != DISABLED) {
      if (watchActivities) {
        ActivityRefWatcher.install(context, refWatcher);
      }
      if (watchFragments) {
        FragmentRefWatcher.Helper.install(context, refWatcher);
      }
    }
    LeakCanaryInternals.installedRefWatcher = refWatcher;
    return refWatcher;
  }
123456789101112131415161718
```

主要作用创建了一个RefWatcher对象并返回，这个方法里面有两个比较重要的方法我们看下
ActivityRefWatcher.install(context, refWatcher)和FragmentRefWatcher.Helper.install(context, refWatcher);

（5） ActivityRefWatcher.install(context, refWatcher);

```
  public static void install(Context context, RefWatcher refWatcher) {
    Application application = (Application) context.getApplicationContext();
    ActivityRefWatcher activityRefWatcher = new ActivityRefWatcher(application, refWatcher);
    //为当前Application注册了所有Activity的生命周期的回调
    application.registerActivityLifecycleCallbacks(activityRefWatcher.lifecycleCallbacks);
  }

  private final Application.ActivityLifecycleCallbacks lifecycleCallbacks =
      new ActivityLifecycleCallbacksAdapter() {
        @Override public void onActivityDestroyed(Activity activity) {
         //当Activity销毁时调用refWathcer的watch方法观察这个对象
          refWatcher.watch(activity);
        }
      };

123456789101112131415
```

（6） FragmentRefWatcher.Helper.install(context, refWatcher);

```
 public static void install(Context context, RefWatcher refWatcher) {
      //创建一个集合
      List<FragmentRefWatcher> fragmentRefWatchers = new ArrayList<>();

     //如果当前API版本大于26，创建一个AndroidOFragmentRefWatcher对象加入到集合中
      if (SDK_INT >= O) {
        fragmentRefWatchers.add(new AndroidOFragmentRefWatcher(refWatcher));
      }

      try {
        //找到SupportFragmentRefWatcher这个类
        Class<?> fragmentRefWatcherClass = Class.forName(SUPPORT_FRAGMENT_REF_WATCHER_CLASS_NAME);
        //找到它的构造方法
        Constructor<?> constructor =
            fragmentRefWatcherClass.getDeclaredConstructor(RefWatcher.class);
            //创建SupportFragmentRefWatcher实例，并添加到集合中
        FragmentRefWatcher supportFragmentRefWatcher =
            (FragmentRefWatcher) constructor.newInstance(refWatcher);
        fragmentRefWatchers.add(supportFragmentRefWatcher);
      } catch (Exception ignored) {
      }

      if (fragmentRefWatchers.size() == 0) {
        return;
      }

      Helper helper = new Helper(fragmentRefWatchers);

      Application application = (Application) context.getApplicationContext();
      application.registerActivityLifecycleCallbacks(helper.activityLifecycleCallbacks);
    }
12345678910111213141516171819202122232425262728293031
```

这个方法的作用是当当前API版本大于26的时候,在Activity销毁是它会调用AndroidOFragmentRefWatcher的watchFragments方法观察当前Activity中的所有销毁的Fragments。也就是说如果当前API版本是大于26的，我们不需要在在Fragment的onDestory方法中自己调用RefWatcher的watch方法了。

下面我们来分析RefWatcher的watch方法,这个方法是LeakCanary的核心逻辑所在，我们一起来看看吧。

**3.RefWatcher.watch();**

```
  public void watch(Object watchedReference) {
    watch(watchedReference, "");
  }

  public void watch(Object watchedReference, String referenceName) {
    if (this == DISABLED) {
      return;
    }
    checkNotNull(watchedReference, "watchedReference");
    checkNotNull(referenceName, "referenceName");
    //获取时间，纳秒级别
    final long watchStartNanoTime = System.nanoTime();
    //生成一个随机数作为key
    String key = UUID.randomUUID().toString();
    
    //retainedKeys是一个Set集合，存储所有的key
    retainedKeys.add(key);
    
    //使用KeyedWeakReference包装当前传过来的引用，代指Activity和Fragment
    //注意了这里传了一个queue，即ReferenceQueue,所以如果KeyedWeakReference引用的Activity或者
    //Fragment即将被销毁时，就会向这个queue中添加reference这个引用对象。
    final KeyedWeakReference reference =
        new KeyedWeakReference(watchedReference, key, referenceName, queue);

    ensureGoneAsync(watchStartNanoTime, reference);
  }
1234567891011121314151617181920212223242526
```

**4.ensureGoneAsync()**

```
  private void ensureGoneAsync(final long watchStartNanoTime, final KeyedWeakReference reference) {
    watchExecutor.execute(new Retryable() {
      @Override public Retryable.Result run() {
        return ensureGone(reference, watchStartNanoTime);
      }
    });
  }
1234567
```

这里主要就是调用watchExecutor的execute添加了一个Retryable任务，有点类似与线程池。这边的watchExecutor是通过RefWathcer的构造方法传过来的。通过上面的分析，我们知道在一开始创建了一个AndroidRefWatcherBuilder对象，它会调用的buildAndInstall方法，这个方法中我们调用了build()方法创建了RefWatcher对象，现在我们去看看这个RefWathcer是怎么创建的。

**5.build（）**

```
 public final RefWatcher build() {
    if (isDisabled()) {
      return RefWatcher.DISABLED;
    }

    if (heapDumpBuilder.excludedRefs == null) {
      heapDumpBuilder.excludedRefs(defaultExcludedRefs());
    }

    HeapDump.Listener heapDumpListener = this.heapDumpListener;
    if (heapDumpListener == null) {
      heapDumpListener = defaultHeapDumpListener();
    }

    DebuggerControl debuggerControl = this.debuggerControl;
    if (debuggerControl == null) {
      debuggerControl = defaultDebuggerControl();
    }

    HeapDumper heapDumper = this.heapDumper;
    if (heapDumper == null) {
     //调用的是AndroidRefWatcherBuilder中的方法。所以这里创建的是一个AndroidHeapDumper对象
      heapDumper = defaultHeapDumper();
    }

    WatchExecutor watchExecutor = this.watchExecutor;
    if (watchExecutor == null) {
      //调用的是AndroidRefWatcherBuilder中的方法。所以这里创建的是一个AndroidWatchExecutor对象
      watchExecutor = defaultWatchExecutor();
    }

    GcTrigger gcTrigger = this.gcTrigger;
    if (gcTrigger == null) {
      //创建的是GcTrigger DEFAULT这个对象
      gcTrigger = defaultGcTrigger();
    }

    if (heapDumpBuilder.reachabilityInspectorClasses == null) {
      heapDumpBuilder.reachabilityInspectorClasses(defaultReachabilityInspectorClasses());
    }

    return new RefWatcher(watchExecutor, debuggerControl, gcTrigger, heapDumper, heapDumpListener,
        heapDumpBuilder);
  }
1234567891011121314151617181920212223242526272829303132333435363738394041424344
```

**这里注意了，AndroidRefWatcherBuilder类中没用实现build方法，所有是调用父类RefWatcherBuilder中的build方法，但是AndroidRefWatcherBuilder实现了defaultWatchExecutor，defaultHeapDumpListener这些方法，所以这些方法应该调用的是AndroidRefWatcherBuilder中的，而不是RefWatcherBuilder的。**

**6.AndroidWatchExecutor.execute()**
通过5的分析我们知道RefWatcher中的watchExecutor对象其实是AndroidWatchExecutor类型的，下面我们来看看它的execute方法做了什么事。

```
  @Override public void execute(Retryable retryable) {
    //如果当前线程是主线程
    if (Looper.getMainLooper().getThread() == Thread.currentThread()) {
      waitForIdle(retryable, 0);
    } else {
      postWaitForIdle(retryable, 0);
    }
  }
12345678
```

我们看到，这里分了两种情况去做处理。如果当前线程是主线程调用 waitForIdle(retryable, 0);否则调用postWaitForIdle(retryable, 0);。其实不管怎么样最终调用的都是waitForIdle这个方法。

1.waitForIdle(retryable, 0);

```
private void waitForIdle(final Retryable retryable, final int failedAttempts) {
    //向主线程的消息队列中里添加了一个对象
    //当我们注册了IdleHandler的时候，当主线程空闲时，会发送一个空闲消息来执行IdleHandler中的回调
    //注意了，queueIdle这个返回为false是，表示只会执行一次，如果为true代表，如果主线程每次空闲时都会
    //执行这个方法
    Looper.myQueue().addIdleHandler(new MessageQueue.IdleHandler() {
      @Override public boolean queueIdle() {
        //当主线程空闲时调用postToBackgroundWithDelay方法
        postToBackgroundWithDelay(retryable, failedAttempts);
        return false;
      }
    });
  }

  private void postToBackgroundWithDelay(final Retryable retryable, final int failedAttempts) {
    long exponentialBackoffFactor = (long) Math.min(Math.pow(2, failedAttempts), maxBackoffFactor);
    long delayMillis = initialDelayMillis * exponentialBackoffFactor;
    //backgroundHandler是一个与异步线程绑定的Handler，我们可以看下AndroidWatchExecutor的构造方法中
    //创建了一个HandlerThread，自己内部构建了一个异步的消息循环
    //所以retryable.run()方法是执行在异步线程中的
    backgroundHandler.postDelayed(new Runnable() {
      @Override public void run() {
        Retryable.Result result = retryable.run();
        if (result == RETRY) {
          postWaitForIdle(retryable, failedAttempts + 1);
        }
      }
    }, delayMillis);
  }
1234567891011121314151617181920212223242526272829
```

**7.retryable.run() ->ensureGone**
上面分析了那么多，其实这里才是我们的主要逻辑，我们先把涉及到的关键代码，贴出来，一步步来分析。

```
 @SuppressWarnings("ReferenceEquality") // Explicitly checking for named null.
  Retryable.Result ensureGone(final KeyedWeakReference reference, final long watchStartNanoTime) {
    long gcStartNanoTime = System.nanoTime();
    long watchDurationMs = NANOSECONDS.toMillis(gcStartNanoTime - watchStartNanoTime);
    
    //如果对象GC时被回收了，就移除retainedKeys中对应的key值
    removeWeaklyReachableReferences();

    if (debuggerControl.isDebuggerAttached()) {
      // The debugger can create false leaks.
      return RETRY;
    }
    
    //如果retainedKeys还存在观察对象的key值代表没有被回收
    if (gone(reference)) {
      return DONE;
    }
    
    //第一次检查没有被回收手动触发GC
    gcTrigger.runGc();

   //GC完毕之后再去检查
    removeWeaklyReachableReferences();
    
   //如果GC后retainedKeys还存在观察对象的key。则表示发生了内存泄漏。
    if (!gone(reference)) {
      long startDumpHeap = System.nanoTime();
      long gcDurationMs = NANOSECONDS.toMillis(startDumpHeap - gcStartNanoTime);

     //dump生成文件
      File heapDumpFile = heapDumper.dumpHeap();
      if (heapDumpFile == RETRY_LATER) {
        // Could not dump the heap.
        return RETRY;
      }
      long heapDumpDurationMs = NANOSECONDS.toMillis(System.nanoTime() - startDumpHeap);

      //创建了HeapDump对象
      HeapDump heapDump = heapDumpBuilder.heapDumpFile(heapDumpFile).referenceKey(reference.key)
          .referenceName(reference.name)
          .watchDurationMs(watchDurationMs)
          .gcDurationMs(gcDurationMs)
          .heapDumpDurationMs(heapDumpDurationMs)
          .build();
          
     //回调ServiceHeapDumpListener的analyze方法，然后调用HeapAnalyzerService.runAnalysis的静态方法
     //启动自身去分析HeapDump，使用HaHa库。不在做分析，感兴趣的朋友可以自己去研究研究。
      heapdumpListener.analyze(heapDump);
    }
    return DONE;
  }


  private boolean gone(KeyedWeakReference reference) {
    //判断是否还存在当前引用的key
    //如果存在代表没有被回收
    //不存在代表被回收了
    return !retainedKeys.contains(reference.key);
  }

  private void removeWeaklyReachableReferences() {
    //如果队列中有元素，代表GC是已经将对象给回收掉
    //每从队列中去除一个元素，同时在retainedKeys这个集合中移除相应的key值。
    KeyedWeakReference ref;
    while ((ref = (KeyedWeakReference) queue.poll()) != null) {
      retainedKeys.remove(ref.key);
    }
  }


  GcTrigger DEFAULT = new GcTrigger() {
    @Override public void runGc() {
      // Code taken from AOSP FinalizationTest:
      // https://android.googlesource.com/platform/libcore/+/master/support/src/test/java/libcore/
      // java/lang/ref/FinalizationTester.java
      // System.gc() does not garbage collect every time. Runtime.gc() is
      // more likely to perfom a gc.
      //这里为什没有调用System.gc()呢，注释已经给了我们说明
      //Runtime.gc()比起System.gc()更有可能去触发一次GC，注意并不是百分百触发GC
      Runtime.getRuntime().gc();
      enqueueReferences();
      //建议在GC时执行对象的finalize方法，不是百分百
      System.runFinalization();
    }

    private void enqueueReferences() {
      // Hack. We don't have a programmatic way to wait for the reference queue daemon to move
      // references to the appropriate queues.
      try {
        Thread.sleep(100);
      } catch (InterruptedException e) {
        throw new AssertionError();
      }
    }
  };


12345678910111213141516171819202122232425262728293031323334353637383940414243444546474849505152535455565758596061626364656667686970717273747576777879808182838485868788899091929394959697
```

到这里我们的LeakCanary如何去判断Activity和Fragment是否发生内存泄漏的主要逻辑就分析完了。我们来做一个总结。

## 三.总结

LeakCanary实现内存泄漏的主要判断逻辑是这样的。当我们**观察的Activity或者Fragment销毁时**，我们会使用一个**弱引用**去包装当前销毁的Activity或者Fragment,并且将它与本地的一个**ReferenceQueue队列关联**。我们知道如果GC触发了，系统会将==当前的引用对象==存入队列中。
==如果没有被回收，队列中则没有当前的引用对象==。所以LeakCanary会去判断，ReferenceQueue是否有当前观察的Activity或者Fragment的引用对象，

- 第一次判断如果不存在，就去==手动触发==一次GC，
- 然后做第二次判断，如果还是不存在，则表明出现了内存泄漏。



最后再总结一下整个流程：

1. 首先注册 Activity/Fragment（ViewModel 不需要注册） 生命周期的监听。
2. 然后每个对象销毁的时候将它们添加到==观察集合==中（watchedObjects），并且将该对象与弱引用（WeakReference）和引用队列（ReferenceQueque）进行关联。这样每当发生 GC 时，弱引用所持有的对象就会被回收，并加入到引用队列。
3. 然后遍历引用队列中保存的观察对象，从观察集合中删除这些对象，最后观察集合中剩下的就是发生内存泄漏的对象了。
4. 最后生成 hprof 文件，并用 Shark 开源库去分析该文件。





https://www.jianshu.com/p/74eed4f9e41c

https://www.jianshu.com/p/70de36ea8b31?utm_campaign=maleskine

https://www.jianshu.com/p/70de36ea8b31?utm_campaign=maleskine

https://blog.csdn.net/braintt/article/details/99685243