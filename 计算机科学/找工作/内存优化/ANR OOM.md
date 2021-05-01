##### App上线后用户使用时卡顿怎么查看是什么原因

##### 内存泄漏的

## Android 基础

#### 1.内存泄漏

一个程序中，已经不需要使用某个对象，但是仍然有引用指向它，垃圾回收器无法回收它，该对象占用的内存无法被回收时，就容易造成内存泄漏。

#### 一、单例造成的内存泄漏

Android的单例模式非常受开发者的喜爱，不过使用的不恰当的话也会造成内存泄漏。因为单例的静态特性使得单例的生命周期和应用的生命周期一样长，这就说明了如果一个对象已经不需要使用了，而单例对象还持有该对象的引用，那么这个对象将不能被正常回收，这就导致了内存泄漏。

```csharp
public class AppManager {
    private static AppManager instance;
    private Context context;
    private AppManager(Context context) {
        this.context = context;
    }
    public static AppManager getInstance(Context context) {
        if (instance != null) {
            instance = new AppManager(context);
        }
        return instance;
    }
}
```

这是一个普通的单例模式，当创建这个单例的时候，由于需要传入一个Context，所以这个Context的生命周期的长短至关重要：
 1、传入的是Application的Context：这将没有任何问题，因为单例的生命周期和Application的一样长 ；
 2、传入的是Activity的Context：当这个Context所对应的Activity退出时，由于该Context和Activity的生命周期一样长（Activity间接继承于Context），所以当前Activity退出时它的内存并不会被回收，因为单例对象持有该Activity的引用。

正确的单例应该修改为下面这种方式：



```csharp
public class AppManager {
    private static AppManager instance;
    private Context context;
    private AppManager(Context context) {
        this.context = context.getApplicationContext();
    }
    public static AppManager getInstance(Context context) {
        if (instance != null) {
            instance = new AppManager(context);
        }
        return instance;
    }
}
```

这样不管传入什么Context最终将使用Application的Context，而单例的生命周期和应用的一样长，这样就防止了内存泄漏。

#### 二、非静态内部类创建静态实例造成的内存泄漏

有的时候我们可能会在启动频繁的Activity中，为了避免重复创建相同的数据资源，会出现这种写法：



```java
public class MainActivity extends AppCompatActivity {
   private static TestResource mResource = null;
   @Override
   protected void onCreate(Bundle savedInstanceState) {
       super.onCreate(savedInstanceState);
       setContentView(R.layout.activity_main);
       if(mManager == null){
           mManager = new TestResource();
       }
       //...
   }
   class TestResource {
       //...
   }
}
```

这样就在Activity内部创建了一个非静态内部类的单例，每次启动Activity时都会使用该单例的数据，这样虽然避免了资源的重复创建，不过这种写法却会造成内存泄漏，因为非静态内部类默认会持有外部类的引用，而又使用了该非静态内部类创建了一个静态的实例，该实例的生命周期和应用的一样长，这就导致了该静态实例一直会持有该Activity的引用，导致Activity的内存资源不能正常回收。正确的做法为：
 将该内部类设为静态内部类或将该内部类抽取出来封装成一个单例，如果需要使用Context，请使用ApplicationContext 。

#### 三、Handler造成的内存泄漏



```java
public class MainActivity extends AppCompatActivity {
    private Handler mHandler = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            //...
        }
    };
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        loadData();
    }
    private void loadData(){
        //...request
        Message message = Message.obtain();
        mHandler.sendMessage(message);
    }
}
```

mHandler是Handler的非静态匿名内部类的实例，所以它持有外部类Activity的引用，我们知道消息队列是在一个Looper线程中不断轮询处理消息，那么当这个Activity退出时消息队列中还有未处理的消息或者正在处理消息，而消息队列中的Message持有mHandler实例的引用，mHandler又持有Activity的引用，所以导致该Activity的内存资源无法及时回收，引发内存泄漏，所以另外一种做法为：



```java
public class MainActivity extends AppCompatActivity {
   private MyHandler mHandler = new MyHandler(this);
   private TextView mTextView ;
   private static class MyHandler extends Handler {
       private WeakReference<Context> reference;
       public MyHandler(Context context) {
           reference = new WeakReference<>(context);
       }
       @Override
       public void handleMessage(Message msg) {
           MainActivity activity = (MainActivity) reference.get();
           if(activity != null){
               activity.mTextView.setText("");
           }
       }
   }

   @Override
   protected void onCreate(Bundle savedInstanceState) {
       super.onCreate(savedInstanceState);
       setContentView(R.layout.activity_main);
       mTextView = (TextView)findViewById(R.id.textview);
       loadData();
   }

   private void loadData() {
       //...request
       Message message = Message.obtain();
       mHandler.sendMessage(message);
   }
}
```

创建一个静态Handler内部类，然后对Handler持有的对象使用弱引用，这样在回收时也可以回收Handler持有的对象，这样虽然避免了Activity泄漏，不过Looper线程的消息队列中还是可能会有待处理的消息，所以我们在Activity的Destroy时或者Stop时应该移除消息队列中的消息，更准确的做法如下：



```java
public class MainActivity extends AppCompatActivity {
   private MyHandler mHandler = new MyHandler(this);
   private TextView mTextView ;
   private static class MyHandler extends Handler {
       private WeakReference<Context> reference;
       public MyHandler(Context context) {
           reference = new WeakReference<>(context);
       }
       @Override
       public void handleMessage(Message msg) {
           MainActivity activity = (MainActivity) reference.get();
           if(activity != null){
               activity.mTextView.setText("");
           }
       }
   }

   @Override
   protected void onCreate(Bundle savedInstanceState) {
       super.onCreate(savedInstanceState);
       setContentView(R.layout.activity_main);
       mTextView = (TextView)findViewById(R.id.textview);
       loadData();
   }

   private void loadData() {
       //...request
       Message message = Message.obtain();
       mHandler.sendMessage(message);
   }

   @Override
   protected void onDestroy() {
       super.onDestroy();
       mHandler.removeCallbacksAndMessages(null);
   }
}
```

使用mHandler.removeCallbacksAndMessages(null);是移除消息队列中所有消息和所有的Runnable。当然也可以使用mHandler.removeCallbacks();或mHandler.removeMessages();来移除指定的Runnable和Message。

#### 四、线程造成的内存泄漏

```java
//——————test1
        new AsyncTask<Void, Void, Void>() {
            @Override
            protected Void doInBackground(Void... params) {
                SystemClock.sleep(10000);
                return null;
            }
        }.execute();
//——————test2
        new Thread(new Runnable() {
            @Override
            public void run() {
                SystemClock.sleep(10000);
            }
        }).start();
```

上面的异步任务和Runnable都是一个匿名内部类，因此它们对当前Activity都有一个隐式引用。如果Activity在销毁之前，任务还未完成， 那么将导致Activity的内存资源无法回收，造成内存泄漏。正确的做法还是使用静态内部类的方式，如下：



```java
   static class MyAsyncTask extends AsyncTask<Void, Void, Void> {
       private WeakReference<Context> weakReference;

       public MyAsyncTask(Context context) {
           weakReference = new WeakReference<>(context);
       }

       @Override
       protected Void doInBackground(Void... params) {
           SystemClock.sleep(10000);
           return null;
       }

       @Override
       protected void onPostExecute(Void aVoid) {
           super.onPostExecute(aVoid);
           MainActivity activity = (MainActivity) weakReference.get();
           if (activity != null) {
               //...
           }
       }
   }
   static class MyRunnable implements Runnable{
       @Override
       public void run() {
           SystemClock.sleep(10000);
       }
   }
//——————
   new Thread(new MyRunnable()).start();
   new MyAsyncTask(this).execute();
```

这样就避免了Activity的内存资源泄漏，当然在Activity销毁时候也应该取消相应的任务AsyncTask::cancel()，避免任务在后台执行浪费资源。

#### 五、资源未关闭造成的内存泄漏

对于使用了BraodcastReceiver，ContentObserver，File，Cursor，Stream，Bitmap等资源的使用，应该在Activity销毁时及时关闭或者注销，否则这些资源将不会被回收，造成内存泄漏。

### 2.anr (应用程序无响应):

> 1.只有主线程才会产生ANR，主线程就是UI线程；
>  2.必须发生某些输入事件或特定操作，比如按键或触屏等输入事件，在BroadcastReceiver或Service的各个生命周期调用函数；
>  3.上述事件响应超时，不同的context规定的上限时间不同
>  a.主线程对输入事件5秒内没有处理完毕
>  b.主线程在执行BroadcastReceiver的onReceive()函数时10秒内没有处理完毕
>  c.主线程在Service的各个生命周期函数时20秒内没有处理完毕。

ANR的根本原因是什么呢？简单的总结有以下两点：

> 1.主线程执行了耗时操作，比如数据库操作或网络编程
>  2.其他进程（就是其他程序）占用CPU导致本进程得不到CPU时间片，比如其他进程的频繁读写操作可能会导致这个问题。
>  细分的话，导致ANR的原因有如下几点：
>  1.耗时的网络访问
>  2.大量的数据读写
>  3.数据库操作
>  4.硬件操作（比如camera)
>  5.调用thread的join()方法、sleep()方法、wait()方法或者等待线程锁的时候
>  6.service binder的数量达到上限
>  7.system server中发生WatchDog ANR
>  8.service忙导致超时无响应
>  9.其他线程持有锁，导致主线程等待超时
>  10.其它线程终止或崩溃导致主线程一直等待

那么如何避免ANR的发生呢或者说ANR的解决办法是什么呢？

> 1.避免在主线程执行耗时操作，所有耗时操作应新开一个子线程完成，然后再在主线程更新UI。
>  2.BroadcastReceiver要执行耗时操作时应启动一个service，将耗时操作交给service来完成。
>  3.避免在Intent Receiver里启动一个Activity，因为它会创建一个新的画面，并从当前用户正在运行的程序上抢夺焦点。如果你的应用程序在响应Intent广 播时需要向用户展示什么，你应该使用Notification Manager来实现。
>  导出anr日志命令行：



```bash
adb shell 
cat  /data/anr/traces.txt   >/mnt/sdcard/traces.txt   
exit
```

### 3.内存溢出:

> 当对象的内存占用已经超出分配内存的空间大小，这时未经处理的异常就会抛出。比如常见的内存溢出情况有：bitmap过大；引用没释放；资源对象没关闭
>  内存溢出的原因
>  1、内存泄露导致
>  由于我们程序的失误，长期保持某些资源（如Context）的引用，垃圾回收器就无法回收它，当然该对象占用的内存就无法被使用，这就造成内存泄露。
>  Android 中常见就是Activity被引用在调用finish之后却没有释放，第二次打开activity又重新创建，这样的内存泄露不断的发生,则会导致内存的溢出。
>  Android的每个应用程序都会使用一个专有的Dalvik虚拟机实例来运行，它是由Zygote服务进程孵化出来的，也就是说每个应用程序都是在属于自己的进程中运行的。Android为不同类型的进程分配了不同的内存使用上限，如果程序在运行过程中出现了内存泄漏的而造成应用进程使用的内存超过了这个上限，则会被系统视为内存泄漏，从而被kill掉，这使得仅仅自己的进程被kill掉，而不会影响其他进程.
>  2、占用内存较多的对象
>  保存了多个耗用内存过大的对象（如Bitmap）或加载单个超大的图片，造成内存超出限制。
>  内存泄漏（memory leak）

有些对象只有有限的生命周期。当它们的任务完成之后，它们将被垃圾回收。如果在对象的生命周期本该结束的时候，这个对象还被一系列的引用，这就会导致内存泄漏。随着泄漏的累积，app将消耗完内存。
 比如，在Activity.onDestroy()被调用之后，view树以及相关的bitmap都应该被垃圾回收。如果一个正在运行的后台线程继续持有这个Activity的引用，那么相关的内存将不会被回收，这最终将导致OutOfMemoryError崩溃。
 memory leak会最终会导致out of memory！

#### 4.内存溢出和内存泄漏的区别

> 内存溢出 out of memory，是指程序在申请内存时，没有足够的内存空间供其使用，出现out of memory；比如申请了一个integer,但给它存了long才能存下的数，那就是内存溢出。
>  内存泄露 memory leak，是指程序在申请内存后，无法释放已申请的内存空间，一次内存泄露危害可以忽略，但内存泄露堆积后果很严重，无论多少内存,迟早会被占光。

#### 5.Android学习之 内存管理机制与应用内存优化:

Random Access Memory(RAM)在任何软件开发环境中都是一个很宝贵的资源。这一点在物理内存通常很有限的移动操作系统上，显得尤为突出。尽管Android的Dalvik虚拟机扮演了常规的垃圾回收的角色，但这并不意味着你可以忽视app的内存分配与释放的时机与地点。于大多数apps来说，Dalvik的GC会自动把离开活动线程的对象进行回收。

##### 一、Android系统是如何管理内存的

Android并没有提供内存的交换区(Swap space)，但是它有使用paging（内存分页）与memory-mapping(mmapping)的机制来管理内存。这意味着任何你修改的内存(无论是通过分配新的对象还是访问到mmaped pages的内容)都会贮存在RAM中，而且不能被paged out。因此唯一完整释放内存的方法是释放那些你可能hold住的对象的引用，这样使得它能够被GC回收。只有一种例外是：如果系统想要在其他地方reuse这个对象。

> - 1. 内存共享
>       Android通过下面几个方式在不同的Process中来共享RAM:
>       1.每一个app的process都是从同一个被叫做Zygote的进程中fork出来的。Zygote进程在系统启动并且载入通用的framework的代码与资源之后开始启动。为了启动一个新的程序进程，系统会fork Zygote进程生成一个新的process，然后在新的process中加载并运行app的代码。这使得大多数的RAM pages被用来分配给framework的代码与资源，并在应用的所有进程中进行共享。
>       2.大多数static的数据被mmapped到一个进程中。这不仅仅使得同样的数据能够在进程间进行共享，而且使得它能够在需要的时候被paged out。例如下面几种static的数据:
>       ① Dalvik code (by placing it in a pre-linked .odex file for direct mmapping
>       ② App resources (by designing the resource table to be a structure that can be mmapped and by aligning the zip entries of the APK)
>       ③ Traditional project elements like native code in .so files.
>       3.在许多地方，Android通过显式的分配共享内存区域(例如ashmem或者gralloc)来实现一些动态RAM区域的能够在不同进程间的共享。例如，window surfaces在app与screen compositor之间使用共享的内存，cursor buffers在content provider与client之间使用共享的内存。

> - 1. 分配与回收内存
>       这里有下面几点关于Android如何分配与回收内存的事实：
>       1.每一个进程的Dalvik heap都有一个限制的虚拟内存范围。这就是逻辑上讲的heap size，它可以随着需要进行增长，但是会有一个系统为它所定义的上限。
>       2.逻辑上讲的heap size和实际物理上使用的内存数量是不等的，Android会计算一个叫做Proportional Set Size(PSS)的值，它记录了那些和其他进程进行共享的内存大小。（假设共享内存大小是10M，一共有20个Process在共享使用，根据权重，可能认为其中有0.3M才能真正算是你的进程所使用的）
>       3.Dalvik heap与逻辑上的heap size不吻合，这意味着Android并不会去做heap中的碎片整理用来关闭空闲区域。Android仅仅会在heap的尾端出现不使用的空间时才会做收缩逻辑heap size大小的动作。但是这并不是意味着被heap所使用的物理内存大小不能被收缩。在垃圾回收之后，Dalvik会遍历heap并找出不使用的pages，然后使用madvise把那些pages返回给kernal。因此，成对的allocations与deallocations大块的数据可以使得物理内存能够被正常的回收。然而，回收碎片化的内存则会使得效率低下很多，因为那些碎片化的分配页面也许会被其他地方所共享到。

> - 1. 限制应用的内存
>       为了维持多任务的功能环境，Android为每一个app都设置了一个硬性的heap size限制。准确的heap size限制随着不同设备的不同RAM大小而各有差异。如果你的app已经到了heap的限制大小并且再尝试分配内存的话，会引起OutOfMemoryError的错误。

在一些情况下，你也许想要查询当前设备的heap size限制大小是多少，然后决定cache的大小。可以通过getMemoryClass()来查询。这个方法会返回一个整数，表明你的app heap size限制是多少megabates。

> - 1. 切换应用
>       Android并不会在用户切换不同应用时候做交换内存的操作。Android会把那些不包含foreground组件的进程放到LRU cache中。例如，当用户刚开始启动了一个应用，这个时候为它创建了一个进程，但是当用户离开这个应用，这个进程并没有离开。系统会把这个进程放到cache中，如果用户后来回到这个应用，这个进程能够被resued，从而实现app的快速切换。

如果你的应用有一个当前并不需要使用到的被缓存的进程，它被保留在内存中，这会对系统的整个性能有影响。因此当系统开始进入低内存状态时，它会由系统根据LRU的规则与其他因素选择杀掉某些进程。

##### 二、该如何管理/优化你的应用内存

你应该在开发过程的每一个阶段都考虑到RAM的有限性，甚至包括在开发开始之前的设计阶段。有许多种设计与实现方式，他们有着不同的效率，尽管是对同样一种技术的不断组合与演变。

为了使得你的应用效率更高，你应该在设计与实现代码时，遵循下面的技术要点：

> *1、珍惜Services资源
>  如果你的app需要在后台使用service，除非它被触发执行一个任务，否则其他时候都应该是非运行状态。同样需要注意当这个service已经完成任务后停止service失败引起的泄漏。
>  你启动一个service，系统会倾向为了这个Service而一直保留它的Process。这使得process的运行代价很高，因为系统没有办法把Service所占用的RAM让给其他组件或者被Paged out。这减少了系统能够存放到LRU缓存当中的process数量，它会影响app之间的切换效率。它甚至会导致系统内存使用不稳定，从而无法继续Hold住 所有目前正在运行的Service。
>  限制你的service的最好办法是使用IntentService, 它会在处理完扔给它的intent任务之后尽快结束自己。

> *2、当你的应用UI隐藏时 释放内存
>  当用户切换到其它app且你的app UI不再可见时，你应该释放应用视图所占的资源。在这个时候释放UI资源可以显著的增加系统cached process的能力，它会对用户的质量体验有着直接的影响。
>  为了能够接收到用户离开你的UI时的通知，你需要实现Activtiy类里面的onTrimMemory()回调方法。你应该使用这个方法来监听到TRIM_MEMORY_UI_HIDDEN级别, 它意味着你的UI已经隐藏，你应该释放那些仅仅被你的UI使用的资源。请注意：你的app仅仅会在所有UI组件的被隐藏的时候接收到onTrimMemory()的回调并带有参数TRIM_MEMORY_UI_HIDDEN。这与onStop()的回调是不同的，onStop会在activity的实例隐藏时会执行，例如当用户从你的app的某个activity跳转到另外一个activity时onStop会被执行。因此你应该实现onStop回调，所以说你虽然实现了onStop()去释放 activity 的资源例如网络连接或者未注册的广播接收者, 但是应该直到你收到 onTrimMemory(TRIM_MEMORY_UI_HIDDEN)才去释放视图资源否则不应该释放视图所占用的资源。这确保了用户从其他activity切回来时，你的UI资源仍然可用，并且可以迅速恢复activity。

> *3、当内存紧张时 释放部分内存
>  在你的app生命周期的任何阶段，onTrimMemory回调方法同样可以告诉你设备内存资源紧张。根据onTrimMemory方法的内存级别来进一步决定释放哪些资源。
>  ① TRIM_MEMORY_RUNNING_MODERATE:你的app正在运行并且不会被列为可杀死的。但是设备正运行于低内存状态下，系统开始激活杀死LRU Cache中的Process的机制。
>  ② TRIM_MEMORY_RUNNING_LOW:你的app正在运行且没有被列为可杀死的。但是设备正运行于更低内存的状态下，你应该释放不用的资源用来提升系统性能，这会直接影响了你的app的性能。
>  ③ TRIM_MEMORY_RUNNING_CRITICAL:你的app仍在运行，但是系统已经把LRU Cache中的大多数进程都已经杀死，因此你应该立即释放所有非必须的资源。如果系统不能回收到足够的RAM数量，系统将会清除所有的LRU缓存中的进程，并且开始杀死那些之前被认为不应该杀死的进程，例如那个进程包含了一个运行中的Service。

> *4、检查你应该使用多少内存
>  通过调用getMemoryClass()来获取你的app的可用heap大小。
>  在一些特殊的情景下，你可以通过在manifest的application标签下添加largeHeap=true的属性来声明一个更大的heap空间。如果你这样做，你可以通过getLargeMemoryClass())来获取到一个更大的heap size。

然而，能够获取更大heap的设计本意是为了一小部分会消耗大量RAM的应用(例如一个大图片的编辑应用)。不要轻易的因为你需要使用大量的内存而去请求一个大的heap size。只有当你清楚的知道哪里会使用大量的内存并且为什么这些内存必须被保留时才去使用large heap. 因此请尽量少使用large heap。使用额外的内存会影响系统整体的用户体验，并且会使得GC的每次运行时间更长。在任务切换时，系统的性能会变得大打折扣。

> *5、避免Bitmap的浪费
>  当你加载一个bitmap时，仅仅需要保留适配当前屏幕设备分辨率的数据即可，如果原图高于你的设备分辨率，需要做缩小的动作。请记住，增加bitmap的尺寸会对内存呈现出2次方的增加，因为X与Y都在增加。
>  使用完Bitmap后 确定不会再使用 则需要注意Bitmap的内存回收（recycle()）处理。

> *6、使用优化的数据容器
>  利用Android Framework里面优化过的容器类，例如SparseArray,SparseBooleanArray, 与LongSparseArray. 通常的HashMap的实现方式更加消耗内存，因为它需要一个额外的实例对象来记录Mapping操作。另外，SparseArray更加高效在于他们避免了对key与value的autobox自动装箱，并且避免了装箱后的解箱。

> *7、请注意内存开销（资源的开启关闭 如数据库查询过程游标的关闭、IO流的关闭、线程池的使用）

> *8、请注意代码编写优化 （Java代码的性能优化）

> *9、Avoid dependency injection frameworks
>  使用类似Guice或者RoboGuice等framework injection包是很有效的，因为他们能够简化你的代码。然而，那些框架会通过扫描你的代码执行许多初始化的操作，这会导致你的代码需要大量的RAM来map代码。但是mapped pages会长时间的被保留在RAM中。

> *10、谨慎使用external libraries
>  很多External library的代码都不是为移动网络环境而编写的，在移动客户端则显示的效率不高。至少，当你决定使用一个external library的时候，你应该针对移动网络做繁琐的porting与maintenance的工作。

> *11、优化整体性能
>  官方有列出许多优化整个app性能的文章：Best Practices for Performance. 这篇文章就是其中之一。有些文章是讲解如何优化app的CPU使用效率，有些是如何优化app的内存使用效率。
>  其他 如 layout布局优化。同样还应该关注lint工具所提出的建议，进行优化。

> *12、使用ProGuard来剔除不需要的代码
>  ProGuard能够通过移除不需要的代码，重命名类，域与方法等方对代码进行压缩,优化与混淆。使用ProGuard可以是的你的代码更加紧凑，这样能够使用更少mapped代码所需要的RAM。

> *13、对最终的APK使用zipalign
>  在编写完所有代码，并通过编译系统生成APK之后，你需要使用zipalign对APK进行重新校准。如果你不做这个步骤，会导致你的APK需要更多的RAM，因为一些类似图片资源的东西不能被mapped。

> *14、分析你的RAM使用情况
>  一旦你获取到一个相对稳定的版本后，需要分析你的app整个生命周期内使用的内存情况，并进行优化，更多细节请参考Investigating Your RAM Usage.

> *15、使用多进程（注意是多进程，别看成多线程了啊！！！）
>  通过把你的app组件切分成多个组件，运行在不同的进程中。这个技术必须谨慎使用，大多数app都不应该运行在多个进程中。因为如果使用不当，它会显著增加内存的使用，而不是减少。当你的app需要在后台运行与前台一样的大量的任务的时候，可以考虑使用这个技术。一个典型的例子是创建可以长时间后台播放的Music Player。如果整个app运行在一个进程中，当后台播放的时候，前台的那些UI资源也没有办法得到释放。类似这样的app可以切分成2个进程：一个用来操作UI，另外一个用来后台的Service.在各个应用的 manifest 文件中为各个组件申明 android:process 属性就可以分隔为不同的进程.例如你可以指定你一运行的服务从主进程中分隔成一个新的进程来并取名为"background"(当然名字可以任意取)
>  <service android:name=".PlaybackService"
>  android:process=":background" />
>  进程名字必须以冒号开头":"以确保该进程属于你应用中的私有进程。



作者：Gambol_r
链接：https://www.jianshu.com/p/fcb13999558b
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。