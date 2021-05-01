[TOC]

### 目录

```java
一、基础回顾
   - 定义特点
   - 注意事项
   - 应用场景举例
   - 类型
二、生命周期
    startService
    bindService
三、使用方法
   1、通过startService方式定义一个Service（继承Service类）
   2、通过startService方式定义一个Service（继承IntentService类）
   3、通过bindService方式定义一个Service：（使用Bind Service完成Service和Activity之间的通信）
   4、bindService和startService混合使用
四、startService、bindService区别大总结
五、在AndroidManifest.xml里Service元素常见选项
六、扩展：进程间通信及小例子
```

### 一、基础回顾

- **定义、特点：**

- Service是可以在**后台执行长时间（长生命周期）**而又**不与用户产生UI交互（没有用户界面）**的操作。

------

- **注意事项：**
  **1、**只能在后台运行，即便用户切换了其他应用，启动的Service仍可在后台运行。
  **2、**可以和其他组件进行Service绑定并与之交互，甚至是跨进程通信（IPC）。
  **3、**不能运行在一个独立的进程当中，而是依赖与创建服务时所在的应用组件进程。
  **4、**服务不会自动开启线程，我们需要在服务的内部手动创建子线程，并在这里执行具体的任务。

------

- **应用场景举例：**
  **音乐播放：**播放多媒体的时候用户启动了其他Activity，此时要在后台继续播放。
  **记录检测：**比如检测SD卡上文件的变化；在后台记录你的地理信息位置的改变等。
  **其他操作：**网络请求、执行文件读写操作或者与 content provider交互。

------

- **类型：**
  **本地服务与远程服务**

  **本地服务**依附在主进程上，在一定程度上节约了资源。本地服务因为是在同一进程，因此不需要IPC，也不需要AIDL。相应bindService会方便很多。缺点是主进程被kill后，服务变会终止。

  **远程服务**是独立的进程，对应进程名格式为所在包名加上你指定的android:process字符串。由于是独立的进程，因此在Activity所在进程被kill的是偶，该服务依然在运行。缺点是该服务是独立的进程，会占用一定资源，并且使用AIDL进行IPC稍微麻烦一点。本文第六部分将会简单的讲述这一进程间通信方式。

  对于startService来说，不管是本地服务还是远程服务，我们需要做的工作都一样简单。

- **等级**

![等级](http://s191.photo.store.qq.com/psb?/V14L47VC0w3vOf/*EPUBd08qY3Etq9iq9XBQ8OVfWsxGWnScZZTAsTpi30!/b/dL8AAAAAAAAA)



### 二、生命周期

从Service的启动到销毁，有两种路径（两种生命周期）：
**startService、bindService**

![生命周期](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/rkIR*s4rfk4TE18CPFNtoWFJDufdMfIpv7DD83UIHCE!/r/dL8AAAAAAAAA)



### 三、使用方法

#### 1、通过startService方式定义一个Service（继承Service类）：

- **核心步骤和代码：**创建一个类继承android.app.Service类，实现抽象方法onBind()，重写onCreate()、onStartCommand()、onDestry()。

```java
public class MyService extends Service {  
        
       public static final String TAG = "MyService";  

       //创建服务时调用
       @Override  
       public void onCreate() {  
           super.onCreate();  
           Log.d(TAG, "onCreate");  
       }  

       //服务执行的操作
       @Override  
       public int onStartCommand(Intent intent, int flags, int startId) {  
           Log.d(TAG, "onStartCommand");  
            new Thread(new Runnable() {
             public void run() {
                  //在子线程中处理具体的逻辑
                  //在这里我们只做打印子线程id的操作
                 Log.i("MyService",Thread.currentThread().getId()+"");
                 stopSelf();  //服务执行完毕后自动停止
             }
         }).start();        
         return super.onStartCommand(intent, flags, startId);  
       }  

       //销毁服务时调用
       @Override  
       public void onDestroy() {  
           super.onDestroy();  
           Log.d(TAG, "onDestroy");  
       }  

       //onBind()方法是Service中唯一的一个抽象方法，所以必须要在子类里实现。
       //Service有两种启动方式：一种是startService()，另一种是bindService()。第二种启动方式才会用到onBind()方法。
       //我们这先用第一种方式定义Service，所以暂时忽略onBind()方法。
       @Override  
       public IBinder onBind(Intent intent) {  
           return null;  
       }  
```

- 在清单文件中配置Service,和Activity标签并列。

```
<service android:name=".MyService"></service>
```

*请注意：为了保证应用的安全，请使用显式Intent启动或绑定一个Service，请不要在/<service>标签中配置intent-filter。*

- 在Activity组件中通过onCreate()声明“启动Service和停止Service”代码。

```java
public class MainActivity extends Activity implements OnClickListener {  
       private Button button1_start_service;  
       private Button button2_stop_service; 
       @Override  
       protected void onCreate(Bundle savedInstanceState) {  
           super.onCreate(savedInstanceState);  
           setContentView(R.layout.activity_main);  
           button1_start_service = (Button)findViewById(R.id.button1_start_service);  
           button2_stop_service = (Button)findViewById(R.id.button2_stop_service);  
           button1_start_service.setOnClickListener(this);  
           button2_stop_service.setOnClickListener(this);  
       }  
   
       @Override  
       public void onClick(View v) {  
           switch (v.getId()) {  
           case R.id.button1_start_service:  
               Intent startIntent = new Intent(this, MyService.class);  
               startService(startIntent);  
               break;  
           case R.id.button2_stop_service:  
               Intent stopIntent = new Intent(this, MyService.class);  
               stopService(stopIntent);  
               break;  
           default:  
               break;  
           }  
       }  
   }
```

- 启动和停止服务：
  startService()和stopService()方法都是定义在Context类当中的，所以可以在MainActivity中直接调用这两个方法。
  运行上面的程序，点击button1_start_service按钮，通过Intent实现启动服务，后台打印日志如下：

```groovy
MyService : OnCreate
MyService : OnStartCommand
```

后面再点击启动按钮只会打印OnStartCommand



#### 2、IntentService

如果我们不手动开启线程，I/MyService: 177将会变成它依赖的主线程1，这就不能做耗时操作了。虽说上面的这种写法并不复杂，但总会有一些程序猿忘记开启线程，或者忘记调用stopSelf()方法。

为了可以简单地创建一个可开启单独线程、会自动停止的服务，Android专门提供了一个IntentService类，这个类就很好的解决了上面所提到的两种尴尬。

- IntentService 的优点：子线程中执行，可以自动结束，不容易被系统杀死(普通线程脱离四大组件后优先级非常低)
- HandlerThread 继承自 Thread，是一种可以使用 Handler 的 Thread，内部创建了消息队列。外界需要通过 Handler 的消息方式来通知 HandlerThread 执行一个具体的任务。具体使用场景是 IntentService。由于 HandlerThread 的 run 方法是一个无限循环，因此当明确不需要再使用 HandlerThread 时，可以通过它的 quit 或 quitSafely 方法来终止线程的执行。
- **IntentService 第一次被启动时，onCreate 方法会被调用，其中会创建一个 HandlerThread，然后使用它的 Looper 来构造一个 Handler 对象 mServiceHandler，这样通过 mServiceHandler 发送的消息最终都会在 HandlerThread 中执行。**
- 另外，由于每执行一个后台任务就必须启动一次 IntentService，而 IntentService 内部则通过消息的方式向 HandlerThread 请求执行任务，Handler 中的 Looper 是顺序处理消息的，这就意味着 IntentService 也是顺序执行后台任务的，当有多个后台任务同时存在时，这些后台任务会按照外界发起的顺序排队执行。

- **IntentService的作用：**
  当我们需要这样**一次性**完成的任务时，就可以使用IntentService来完成。
- **IntentService的用法：**
  1）新建一个MyIntentService类，继承自IntentService，并重写父类的onHandleIntent()方法，代码如下：

```java
public class MyIntentService extends IntentService{
      public MyIntentService() {
         //第一步：重写父类的onHandleIntent()方法，这里首先要提供一个无参的构造方法，
         //并且必须在其内部调用父类的有参构造方法，这里我们手动给服务起个名字为：MyIntentService
         super("MyIntentService");
     }
 
       //第二步：重写父类的onHandleIntent()方法，该方法在会在一个单独的线程中执行，
       //来完成工作任务。任务结束后，该Service自动停止
     @Override
     protected void onHandleIntent(Intent intent) {
         for(int i = 0;i<3;i++) {
             //Service要执行的逻辑
             //这里我们只打印当前线程的id
             Log.d("MyIntentService","IntentService线程的id是："+Thread.currentThread().getId());
             try {
                 //线程睡眠一秒钟
                 Thread.sleep(1000);
             } catch (InterruptedException e) {
                 e.printStackTrace();
             }
         }        
     }
 
     @Override
     public void onDestroy() {
         super.onDestroy();
         Log.d("MyIntentService","onDestroy");
     }
 }
```

2）在清单文件中对服务进行注册服务：
`<service android:name=".MyIntentService"></service>`
3)在MainActivity里面加入启动IntentService的逻辑，核心代码如下：

```java
case R.id.button3_stop_intentservice:
             Log.d("MainActivity","主线程的id是："+Thread.currentThread().getId());
             Intent intentService = new Intent(this,MyIntentService.class);
             startService(intentService);
```

日志打印结果是执行完任务后service自动destory。



#### 3、startService小结

**1、启动一个IntentService和启动一个普通的Service，步骤是相似的。2、与直接继承Service不同在于：通过继承IntentService运行，自动开启了单独线程，而且完成任务后自动销毁了Service。**

**【补充】Service和Thread的关系：**
不少Android初学者都可能会有这样的疑惑，Service和Thread到底有什么关系呢？什么时候应该用Service，什么时候又应该用Thread？答案可能会有点让你吃惊，因为Service和Thread之间没有任何关系！
之所以有不少人会把它们联系起来，主要就是因为Service的后台概念。Android的后台就是指，它的运行是完全不依赖UI的。即使Activity被销毁，或者程序被关闭，只要进程还在，Service就可以继续运行。Thread我们大家都知道，是用于开启一个子线程，在这里去执行一些耗时操作就不会阻塞主线程的运行。而Service我们最初理解的时候，总会觉得它是用来处理一些后台任务的，一些比较耗时的操作也可以放在这里运行，这就会让人产生混淆了。但是，Service其实是运行在主线程里的，一些比较耗时的操作需要开启单独线程。



#### 4、通过bindService方式定义一个Service

> （使用Bind Service完成Service和Activity之间的通信）

**Bind Service的引入：**
有没有什么办法能让Service与组件的关联更多一些呢？比如说在Activity中指挥Service去干什么，Service就去干什么。当然可以，只需要让Activity和Service建立关联就好了。这时我们就可以通过bindService方式定义一个Service。

**Bind Service的实现原理：**
应用程序组件(客户端）通过调用bindService()方法能够绑定服务，然后Android系统会调用服务的onBind()回调方法，则个方法会返回一个跟服务器端交互的Binder对象。

bindService()方法立即返回，并且不给客户端返回IBinder对象。要接收IBinder对象，客户端必须创建一个ServiceConnection类的实例，并且把这个实例传递给bindService()方法。ServiceConnection对象包含了一个系统调用的传递IBinder对象的回调方法。



#### 5、总结

1、生命周期不同。（详见二）

2、多次启动，前者会多次执行onStartCommand()方法，后者什么都不执行。多次停止，前者只会执行一次onDestroy()方法，后者报异常信息。

3、当启动Service的组件已被Destroy的时候，前者不停止，后者会停止。

4、前者停止直接执行onDestroy()方法（Service中的），后者则先解除绑onUnbind()定再执行onDestroy()方法（Service中的）。

5、当手机屏幕在“横”“竖”变换时，前者创建的Service不会停止，后者会随着Activity的重建而停止。

6、后者的onBind回调方法将返回给客户端一个IBinder接口实例，IBinder允许客户端回调服务的方法，比如得到Service运行的状态或其他操作。而这些操作前者启动的Service是没有的。



### 四、在AndroidManifest.xml里Service元素常见选项

```
android:name   --　服务类名
android:label  --  服务的名字，如果此项不设置，那么默认显示的服务名则为类名
android:icon　--　服务的图标
android:permission　--　申明此服务的权限，这意味着只有提供了该权限的应用才能控制或连接此服务
android:process　--　表示该服务是否运行在另外一个进程，如果设置了此项，那么将会在包名后面加上这段字符串表示另一进程的名字
android:enabled　--表示是否能被系统实例化，为true表示可以，为false表示不可以，默认为true
android:exported　--　表示该服务是否能够被其他应用程序所控制或连接，不设置默认此项为 false
```



### 五、AIDL

- aidl文件

```java
interface IMyAidlInterface { 
    void testMethod(String str); 
}
```

- Binder

```java
public class AidlBinder extends IMyAidlInterface.Stub { 

    private MyService mService; 

    public AidlBinder(MyService service) { 
        this.mService = service; 
    } 

    @Override 
    public void testMethod(String str) throws RemoteException { 
        mService.serviceMethod(str); 
    } 
}
```

- Service

```java
public class AidlBinder extends IMyAidlInterface.Stub { 

    private MyService mService; 

    public AidlBinder(MyService service) { 
        this.mService = service; 
    } 

    @Override 
    public void testMethod(String str) throws RemoteException { 
        mService.serviceMethod(str); 
    } 
}
```

- Activity

```java
public class MainActivity extends Activity { 

    private static final String TAG = "zjy"; 
    public IMyAidlInterface mService; 

    private ServiceConnection mConnection = new ServiceConnection() { 
        @Override 
        public void onServiceConnected(ComponentName componentName, IBinder iBinder) { 
            mService = IMyAidlInterface.Stub.asInterface(iBinder); 
        } 

        @Override 
        public void onServiceDisconnected(ComponentName componentName) { 

        } 
    }; 

    @Override 
    protected void onCreate(Bundle savedInstanceState) { 
        super.onCreate(savedInstanceState); 
        setContentView(R.layout.activity_main); 
        Intent intent = new Intent(MainActivity.this, MyService.class); 
        bindService(intent, mConnection, BIND_AUTO_CREATE); 

        findViewById(R.id.test_bt).setOnClickListener(new View.OnClickListener() { 
            @Override 
            public void onClick(View view) { 
                try { 
                    mService.testMethod("hi, service."); 
                } catch (RemoteException e) { 
                    e.printStackTrace(); 
                } 
            } 
        }); 
    } 
}
```



### 六、Messenger

**Service代码**

```java
public class MyService extends Service { 
    private static final String TAG = "zjy"; 

    //1.Service里面实现一个Handler用来接收消息用 
    private Handler mHandler = new Handler() { 
        @Override 
        public void handleMessage(Message msg) { 
            //6.Service里面的Handler收到消息并处理 
            if (msg.what==1) { 
                Bundle bundle = msg.getData(); 
                Log.d(TAG, "receive message from activity: "+bundle.getString("string")); 

                //9.取出消息中的Messenger对象 
                Messenger replyMessenger = msg.replyTo; 

                Message  replyMsg= new Message(); 
                replyMsg.what = 2; 
                Bundle b = new Bundle(); 
                b.putString("string", "hi, activity"); 
                replyMsg.setData(b); 
                try { 
                    //10.使用Messenger给Activity发消息 
                    replyMessenger.send(replyMsg); 
                } catch (RemoteException e) { 
                    e.printStackTrace(); 
                } 
            } 
        } 
    }; 

    // 2.使用这个Handler创建一个Messenger对象 
    private Messenger mMessenger = new Messenger(mHandler); 

    @Override 
    public void onCreate() { 
        super.onCreate(); 
    } 

    @Nullable 
    @Override 
    public IBinder onBind(Intent intent) { 
        //3.使用这个Messenger对象创建一个Binder对象，并在onBind方法返回 
        return mMessenger.getBinder(); 
    } 
}
```

**Activity代码**

```java
public class MainActivity extends Activity { 

    private static final String TAG = "zjy"; 

    private Messenger mMessenger; 

    private ServiceConnection mConnection = new ServiceConnection() { 
        @Override 
        public void onServiceConnected(ComponentName name, IBinder service) { 
            //4.Activity里面绑定Service的时候使用传过来的Binder创建一个Messenger对象 
            mMessenger = new Messenger(service); 
        } 

        @Override 
        public void onServiceDisconnected(ComponentName name) { 

        } 
    }; 

    @Override 
    protected void onCreate(Bundle savedInstanceState) { 
        super.onCreate(savedInstanceState); 
        setContentView(R.layout.activity_main); 
        Intent intent = new Intent(MainActivity.this, MyService.class); 
        bindService(intent,mConnection,BIND_AUTO_CREATE); 

        findViewById(R.id.bt1).setOnClickListener(new View.OnClickListener() { 
            @Override 
            public void onClick(View v) { 
                Message msg = new Message(); 
                msg.what = 1; 

                Bundle bundle = new Bundle(); 
                bundle.putString("string", "hi, service"); 
                msg.setData(bundle); 
                //8.发送消息的时候携带一个Messenger对象 
                msg.replyTo = new Messenger(mGetReplyMsg); 

                try { 
                    //5.Activity里面使用这个Messenger对象给Service发消息 
                    mMessenger.send(msg); 
                } catch (RemoteException e) { 
                    e.printStackTrace(); 
                } 
            } 
        }); 
    } 

    //7.Activity里面实现一个Handler用来接收Service回复的消息 
    private Handler mGetReplyMsg = new Handler(){ 
        @Override 
        public void handleMessage(Message msg) { 
            //11.处理Service回复的消息 
            if (msg.what==2) { 
                Bundle bundle = msg.getData(); 
                Log.d(TAG, "receive message from service: "+bundle.getString("string")); 
            } 
        } 
    }; 
}
```

**需要注意的问题**

Messenger发送的消息是Message对象，组装Message消息的时候不要使用Message的obj字段，而是借用Bundle来组装数据。下面是《Android开发艺术探索》里面的一段话：

> 使用Messenger来传输Message，Message中能使用的载体只有what, arg1, arg2, Bundle以及replyTo。Message中的另一字段obj在同一个进程中很实用，但是在进程间通信的时候，在android2.2以前obj不支持跨进程，即便是2.2以后，也仅仅是系统提供的实现了Parcelable接口的对象才能通过它来传输。这就意味着自定义的Parcelable对象是无法通过obj字段来传输的。

在接收端的代码中，取消息的时候是先从Message里面取出Bundle，然后直接从Bundle取数据。如果数据是自定义的Parcelable对象，是不能直接从Bundle里面取的，需要在取数据之前先给Bundle设置一个ClassLoader。“取数据之前”的意思不单单是指取自定义的Parcelable对象，而是包括基本数据类型和系统提供的Parcelable对象等所有数据之前。示例代码如下：

```java
Bundle bundle = msg.getData(); 
bundle.setClassLoader(getClassLoader());//设置ClassLoader 
bundle.getxxx(key);//取数据
```



### 链接

- [谷歌开发者](https://mp.weixin.qq.com/s/tCI5Y_BJBX1yok3oocKPWw)

- https://blog.csdn.net/javazejian/article/details/52709857

- [guolin](https://mp.weixin.qq.com/s/LItTE7Gu80FYMPp0VKlZQw)

- https://www.jianshu.com/p/eeb2bd59853f

