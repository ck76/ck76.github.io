[TOC]

Application和Activity,Service一样是android框架的一个系统组件，当android程序启动时系统会创建一个 application对象，用来存储系统的一些信息。通常我们是不需要指定一个Application的，这时系统会自动帮我们创建， SDK中的描述：Application类是为了那些需要保存全局变量设计的基本类，你可以在AndroidManifest.xml的&lt;application>标签中进行自己的实现，如果需要创建自己 的Application，也很简单创建一个类继承 Application并在manifest的application标签中进行注册(只需要给Application标签增加个name属性把自己的 Application的名字定入即可)。  

android系统会为每个程序运行时创建一个Application类的对象且仅创建一个，所以Application可以说是单例 (singleton)模式的一个类.且application对象的生命周期是整个程序中最长的，它的生命周期就等于这个程序的生命周期。因为它是全局 的单例的，所以在不同的Activity,Service中获得的对象都是同一个对象。所以通过Application来进行一些，数据传递，数据共享 等,数据缓存等操作。 

![Context结构](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/rB9ul8iger4I2.TTTEsp.ifX8UomaOoYncRsaVeXaLE!/r/dFMBAAAAAAAA)

Context的继承结构从图中可以看到，直系子类有两个，一个是ContextWrapper，一个是ContextImpl。那么从名字上就可以看出，ContextWrapper是上下文功能的封装类，而ContextImpl则是上下文功能的实现类。而ContextWrapper又有三个直接的子类，ContextThemeWrapper、Service和Application。其中，ContextThemeWrapper是一个带主题的封装类，而它有一个直接子类就是Activity。

在这里我们至少看到了几个所比较熟悉的面孔，Activity、Service、还有Application。由此，其实我们就已经可以得出结论了，Context一共有三种类型，分别是Application、Activity和Service。这三个类虽然分别各种承担着不同的作用，但它们都属于Context的一种，而它们具体Context的功能则是由ContextImpl类去实现的。

一种比较特殊的情况是Dialog要求在Activity的Context中弹出，而一些全局的例如网络终端的Dialog需要什么特殊权限。

## 1:Application是什么？

**Application和Activity,Service一样,是android框架的一个系统组件，当android程序启动时系统会创建一个 application对象，用来存储系统的一些信息。通常我们是不需要指定一个Application的，这时系统会自动帮我们创建，如果需要创建自己 的Application，也很简单创建一个类继承 Application并在manifest的application标签中进行注册(只需要给Application标签增加个name属性把自己的 Application的名字定入即可)。** 

android系统会为每个程序运行时创建一个Application类的对象且仅创建一个，所以Application可以说是单例 (singleton)模式的一个类.且application对象的生命周期是整个程序中最长的，它的生命周期就等于这个程序的生命周期。因为它是全局 的单例的，所以在不同的Activity,Service中获得的对象都是同一个对象。所以通过Application来进行一些，数据传递，数据共享 等,数据缓存等操作。

## 2:通过Application传递数据

**假如有一个Activity A, 跳转到 Activity B ,并需要推荐一些数据，通常的作法是Intent.putExtra() 让Intent携带，或者有一个Bundle把信息加入Bundle让Intent推荐Bundle对象，实现传递。但这样作有一个问题在 于，Intent和Bundle所能携带的数据类型都是一些基本的数据类型，如果想实现复杂的数据传递就比较麻烦了，通常需要实现 Serializable或者Parcellable接口。这其实是Android的一种IPC数据传递的方法。如果我们的两个Activity在同一个 进程当中为什么还要这么麻烦呢，只要把需要传递的对象的引用传递过去就可以了。     基本思路是这样的。在Application中创建一个HashMap ，以字符串为索引，Object为value这样我们的HashMap就可以存储任何类型的对象了。在Activity A中把需要传递的对象放入这个HashMap，然后通过Intent或者其它途经再把这索引的字符串传递给Activity B ,Activity B 就可以根据这个字符串在HashMap中取出这个对象了。只要再向下转个型 ，就实现了对象的传递。**

## 3:Application数据缓存

　　我一般会习惯在application中建立两个HashMap一个用于数据的传递，一个用于缓 存一些数据。比如有一个Activity需要从网站获取一些数据，获取完之后我们就可以把这个数据cache到Application 当中，当页面设置到其它Activity再回来的时候，就可以直接使用缓存好的数据了。但如果需要cache一些大量的数据，最好是cache一些 (软引用)SoftReference ，并把这些数据cache到本地rom上或者sd卡上。如果在application中的缓存不存在，从本地缓存查找，如果本地缓存的数据也不存在再从网 络上获取。

## 4:PitFalls(汉语：易犯的错误)

使用Application如果保存了一些不该保存的对象很容易导致内存泄漏。如果在Application的oncreate中执行比较 耗时的操作，将直接影响的程序的启动时间。不些清理工作不能依靠onTerminate完成，因为android会尽量让你的程序一直运行，所以很有可能 onTerminate不会被调用。

## 5：MemoryLeak

在Java中内存泄漏是只，某个(某些)对象已经不在被使用应该被gc所回收，但有一个对象持有这个对象的引用而阻止这个对象被回收。比如我 们通常会这样创建一个View TextView tv = new TextView(this);这里的this通常都是Activity。所以这个TextView就持有着这个Activity的引用。下面看张图 (Google IO 2011 ppt中抄得)
通常情况下，当用户转动手机的时候，android会重新调用OnCreate()方法生成一个新的Activity，原来的 Activity应该被GC所回收。但如果有个对象比如一个View的作用域超过了这个Activity(比如有一个static对象或者我们把这个 View的引用放到了Application当中)，这时候原来的Activity将不能被GC所回收，Activity本身又持有很多对象的引用，所以 整个Activity的内存被泄漏了。

| `1 2 3` | `  备注：经常导致内存泄漏核心原因：    keeping a long-lived reference to a Context.持有一个context的对象，从而gc不能回收。   情况如下： ` |
| ------- | ------------------------------------------------------------ |
|         |                                                              |

- 一个View的作用域超出了所在的Activity的作用域，比如一个static的View或者把一个View cache到了application当中 etc

理解：内存：注意静态的数据和缓存中的数据；注意释放；

- 某些与View关联的Drawable的作用域超出了Activity的作用域。
- Runnable对象：比如在一个Activity中启用了一个新线程去执行一个任务，在这期间这个Activity被系统回收了， 但Runnalbe的 任务还没有执行完毕并持有Activity的引用而泄漏，但这种泄漏一般来泄漏一段时间，只有Runnalbe的线程执行完闭，这个 Activity又可以被正常回收了。
- 内存类的对象作用域超出Activity的范围：比如定义了一个内存类来存储数据，又把这个内存类的对象传给了其它Activity 或者Service等。因为内部类的对象会持有当前类的引用，所以也就持有了Context的引用。解决方法是如果不需要当前的引用把内部类写成static或者，把内部类抽取出来变成一个单独的类，或者把避免内部对象作用域超出Activity的作用域。out Of Memery Error 在android中每一个程序所分到的内存大小是有限的，如果超过了这个数就会报Out Of Memory Error。 android给程序分配的内存大小与手机硬件有关，以下是一些手机的数据：

G1:16M Droid:24 Nexus One:32M Xoom:48Ms
所以尽量把程序中的一些大的数据cache到本地文件。以免内存使用量超标。
记得数据传递完成之后，把存放在application的HashMap中的数据remove掉，以免发生内存的泄漏

## 6：生命周期：

onCreate 在创建应用程序时创建
onTerminate 当终止应用程序对象时调用，不保证一定被调用，当程序是被内核终止以便为其他应用程序释放资源，那
么将不会提醒，并且不调用应用程序的对象的onTerminate方法而直接终止进 程
onLowMemory 当后台程序已经终止资源还匮乏时会调用这个方法。好的应用程序一般会在这个方法里面释放一些不必
要的资源来应付当后台程序已经终止，前台应用程序内存还不够时的情况。
onConfigurationChanged 配置改变时触发这个方法

备注:application 被杀死的情况分析：
为了决定在内存较低的时候杀掉哪个进程, Android会根据运行在这些进程内的组件及他们的状态把进程划分成一个”重要程度层次”. 其重要的程度按以下规则排序:

- 前端进程可以是一个持有运行在屏幕最前端并与用户交互的Activity的进程(onResume方法被调用时)，也可以是持有一个正在运行的IntentReceiver(也就是说他正在执行自己的onReceiveIntent方法)的进程. 在系统中, 只会有少数这样的进程, 并且除非内存已经低到不够这些进程运行, 否则系统不会主动杀掉这些进程. 这时, 设备通常已经达到了需要内存整理的状态, 所以杀掉这些进程是为了不让用户界面停止响应.
- 可视进程是持有一个被用户可见, 但没有显示在最前端 (onPause方法被调用时) 的Activity的进程. 举例来说, 这种进程通常出现在一个前端Activity以一个对话框出现并保持前一个Activity可见时. 这种进程被系统认为是极其重要的, 并且通常不会被杀掉, 除非为了保持所有前端进程正常运行不得不杀掉这些可见进程.

- 服务进程是持有一个Service的进程, 该Service是由startService()方法启动的, 尽管这些进程用户不能直接看到, 但是通常他们做的工作用户是十分关注的(例如, 在后台播放mp3或是在后台下载 上传文件), 所以, 除非为了保持所有的前端进程和可视进程正常运行外, 系统是不会杀掉服务进程的.
- 后台进程是持有一个不再被用户可见的Activity(onStop()方法被调用时)的进程. 这些进程不会直接影响用户体验. 加入这些进程已经完整的,正确的完成了自己的生命周期(访问Activity查看更多细节), 系统会在为前三种进程释放内存时随时杀掉这些后台进程. 通常会有很多的后台进程在运行, 所以这些进程被存放在一个LRU列表中, 以保证在低内存的时候, 最近一个被用户看到的进程会被最后杀掉.
- 空进程是没有持有任何活动应用组件的进程. 保留这种进程的唯一理由是为了提供一种缓存机制, 缩短他的应用下次运行时的启动时间. 就其本身而言, 系统杀掉这些进程的目的是为了在这些空进程和底层的核心缓存之间平衡整个系统的资源. [www.2cto.com](http://www.2cto.com/)

当需要给一个进程分类的时候, 系统会在该进程中处于活动状态的所有组件里掉选一个重要等级最高作为分类依据. 查看Activity, Service,和IntentReceiver的文档, 了解每个组件在进程整个生命周期中的贡献. 每一个classes的文档详细描述他们在各自应用的生命周期中所起得作用.

##  7：application 的context 

- 它描述的是一个应用程序环境的信息，即上下文。

- 该类是一个抽象(abstract class)类，Android提供了该抽象类的具体实现类(后面我们会讲到是ContextIml类)。

- 通过它我们可以获取应用程序的资源和类，也包括一些应用级别操作，例如：启动一个Activity，发送广播，接受Intent

信息 等。。



## Application类在项目开发中的使用

首先在项目目录下一个Java类继承Application类，实现是onCreate()方法。这个类可以做APP的全局初始化工作，比如图片加载框架的全局配置信息设置。

```java
public class AndroidApplication extends Application {

    private static AndroidApplication instance;

    @Override
    public void onCreate() {
        super.onCreate();
        instance = this;
    }

    public static AndroidApplication getInstance(){
        return instance;
    }
}
```

然后千万不要忘了在Android项目的Manifest文件中指定Application的实现类，不然系统会创建一个默认的Application类。

```xml
<application
        android:name=".AndroidApplication"
        android:allowBackup="true"
        android:icon="@mipmap/ic_launcher"
        android:label="@string/app_name"
        android:supportsRtl="true"
        android:theme="@style/AppTheme">
        <activity android:name=".MainActivity">
            <intent-filter>
                <action android:name="android.intent.action.MAIN"/>

                <category android:name="android.intent.category.LAUNCHER"/>
            </intent-filter>
        </activity>
    </application>
```

## Application类或Context类的误用情况

- 不能用Application缓存数据！！！

因为Application会因为进入background后内存不足被系统干掉，进入后系统会重现创建一个Application类，而导致缓存在Application类里的数据全部初始化而丢失。

- 错误的获取全局Context对象的方式

```java
public class AnddroidApplication extends Application {  
      
    private static AnddroidApplication app;  
      
    public static AnddroidApplication getInstance() {
        if (app == null) {  
            app = new AnddroidApplication();  
        }  
        return app;  
    }  
      
} 
```

上面这种方式如果是单纯的Java工程可能没有问题，但是在Android里这样说大错特错的。因为Application是系统组件，系统组件实例是要由系统去创建的，如果我们这里直接创建一个，不过是简单的Java对象而已，不具备任何Context能力，也无法进行任何Context操作。标准的写法就本文的第一段示意代码那样。