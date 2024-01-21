[TOC]

### 目录

```java
前言
  1.广播基础知识
  2.广播实现流程
一、广播定义及类型
  1.广播定义
  2.广播类型
    1)普通广播
    2)有序广播
    3)系统广播
    4)异步广播
二、广播接收者。
  1.广播接收者定义
      第一步：定义广播接收者，继承BroadcastReceiver，并重写onReceive()方法。
     【关于接收的广播类型的说明】
      第二步：注册感兴趣的广播Intent。
      1)动态注册
      2)静态注册
     【关于优先级别的说明】
        - 实现方法
        - 设置方法
        - 特别说明
     【关于动、静态注册方式特别说明】
  2.生命周期。
三、广播实例。
【附录】
常见系统广播
```



### 前言

- ##### 基础知识。

1. Android广播分为两个方面：广播发送者和广播接收者，通常情况下，BroadcastReceiver指的就是广播接收者（广播接收器）。**广播可以跨进程甚至跨App直接通信。**

2. 你的APP可以接收广播(如当电话呼入时，或者数据网络可用时)进行接收并做出响应。

3. 广播接收器没有用户界面。然而，它们可以**启动一个Activity或Service服务**来响应它们收到的信息，或者**用NotificationManager 来通知用户**。通知可以用很多种方式来吸引用户的注意力──闪动背灯、震动、播放声音等。一般来说是在状态栏上放一个持久的图标，用户可以打开它并获取消息。

- ##### 实现流程。

Android中的广播将广播的发送者和接受者极大程度上解耦，使得系统能够方便集成，更易扩展。具体实现流程要点粗略概括如下：

1. 广播接收者BroadcastReceiver通过Binder机制向AMS(Activity Manager Service)进行注册；
2. 广播发送者通过Binder机制向AMS发送广播；
3. AMS查找符合相应条件（IntentFilter/Permission等）的BroadcastReceiver，将广播发送到BroadcastReceiver（一般情况下是Activity）相应的消息循环队列中；
4. 消息循环执行拿到此广播，回调BroadcastReceiver中的onReceive()方法。

**Activity Manager、NotificationManager位于安卓架构图Framework层**



### 一、广播定义及类型。

**广播定义：**Android系统在运行的过程中，会产生很多系统广播，比如开机、电量改变、收发短信、拨打电话、屏幕解锁。当然我们也可以发送自定义普通或有序广播。

------

**广播类型：**
**1.普通广播：**通过Context.sendBroadcast(Intent intent)发送，可以在同一时刻（逻辑上）被所有广播接收者无需等待的接收到。
**优点：**消息传递的效率比较高。
**缺点：**1.接收者不能修改该广播。2.无法终止广播Intent的传播，即无法阻止其他接收者的接收动作。

```java
Intent intent = new Intent("One");
intent.putExtra("msg", "这是一条普通广播");
sendBroadcast(intent);
```

**2.有序广播：**通过Context.sendOrderedBroadcast(intent, receiverPermission)发送，是按照接收者声明的优先级别，被接收者依次接收广播。
**优点：**1、广播可以通过接收者调用abortBroadcast()方法截断广播（被截断的广播不能再继续传递该广播）。2、接收者能修改处理结果（比如通过传递Bundle）传递给下一个接收者（一般情况下，不建议对有序广播进行此类操作，尤其是针对系统中的有序广播）。
**缺点：**消息传递的效率比普通广播低。

```java
Intent intent2 = new Intent("Two");
intent2.putExtra("msg", "这是一条有序广播");
sendOrderedBroadcast(intent2, null);
```

**3.系统广播：**只要涉及到手机的基本操作，基本上都会发出相应的系统广播。如：开机启动，网络状态改变，拍照，屏幕关闭与开启，点亮不足等等。每个系统广播都具有特定的intent-filter，其中主要包括具体的action，系统广播发出后，将被相应的BroadcastReceiver接收。系统广播在系统内部当特定事件发生时，由系统自动发出。**详见文章末尾附录**

**4.异步广播：**通过mContext.sendStickyBroadcast(intent)发送，此广播会一直滞留（等待），以便有人注册这则广播消息后能尽快的收到这条广播。其他功能与sendBroadcast相同。但是使用sendStickyBroadcast 发送广播需要获得BROADCAST_STICKY permission，如果没有这个permission则会抛出异常。
**优点：**广播先发送，接收者后注册依然能收到广播。

```java
Intent intent3 = new Intent("Three");
intent3.putExtra("msg", "这是一条异步广播");
sendStickyBroadcast(intent3);
```



### 二、广播接收者。

- **广播接收者定义：**BroadcastReceiver也就是“广播接收者”的意思，顾名思义，它就是用来接收来自系统和应用中的广播，且可以做出相关操作。

------

- ###### 实现方法：

**第一步：**定义广播接收者，继承BroadcastReceiver，并重写onReceive()方法。

```java
public class MyReceiver01 extends BroadcastReceiver {  
    @Override public void onReceive(Contextcontext, Intentintent) {  
        //abortBroadcast();//接到的广播为有序的广播则可截断
        String s = intent.getStringExtra("msg");
        System.out.println("MyReceiver01收到消息："+s);
    }  
}  
```

**【关于接收的广播类型的说明】**

- **接收的广播为普通广播：**
  1、不可以在onReceive里面截断广播，否则会打印异常。
  2、不可以处理广播。
- **接收的广播为有序广播：**
  1、可以在onReceive里面通过abortBroadcast()截断广播，使广播不再传播。
  2、可以在onReceive里面接收广播Intent携带的数据（String字符串、Bundle对象、或者实现Serializable接口、Parcelable接口的Object对象）。
  3、可向低优先级接收者传递新的数据，如下代码

```java
//我是高优先级接收者
@Override public void onReceive(Contextcontext, Intentintent) {  
        Bundle bundle = new Bundle();  
        bundle.putString("next_receiver", "下一个广播接收者");  
        setResultExtras(bundle);
  }  
//我是低优先级接收者
@Override  
    public void onReceive(Context context, Intent intent) {  
        Bundle bundle = getResultExtras(true);  
        String content = bundle.getString("next_receiver"); 
    }  
```

**第二步：**注册感兴趣的广播Intent，注册方法有两种：
**1、动态注册**（在onCreate代码中进行注册）。

```javascript
IntentFilter filter = new IntentFilter("感兴趣的广播名、例如上面发送的有序广播：Two");  
MyReceiver01  receiver = new MyReceiver01();  
intentfilter.setPriority(200);//设置优先级
registerReceiver(receiver, filter);  
```

**2、静态注册**（在AndroidManifest.xml配置文件中注册）。

```java
<!--prioritys设置优先级-->
<receiver android:name=". MyReceiver01">  
   <intent-filter android:priority="100">  
        <action android:name="感兴趣的广播名、例如上面发送的有序广播：Two"/>  
   </intent-filter>  
</receiver>
```

**【关于优先级别的说明】**

- ** 设置方法**（如上面注册代码所示）：
  **1.静态设置：**声明在静态注册的广播接收者的intent-filter元素的android:priority属性中。
  **2.动态设置：**调用IntentFilter对象的setPriority()进行声明。
- **特别说明**
  **1.**静态注册时设置优先级。数越大优先级别越高,取值范围:-1000到1000。就会先接受到消息。
  **2.**动态注册优先级高于任何静态注册。
  **3.**如果动态注册和静态注册都有注册，则会执行两次onReceive。
  **4.**同级别的接收顺序是随机的。

**【关于动、静态注册方式特别说明】**

- **静态注册广播：**又叫常驻型广播，当你的应用程序关闭了，如果有广播信息来，你写的广播接收器同样的能接受到，他的注册方式就是在你的应用程序中的AndroidManifast.xml进行订阅的。
- **动态注册广播：**又叫非常驻型广播，当应用程序结束了，广播自然就没有了，比如你在Activity中的onCreate或者onResume中订阅广播，同时你必须在onDestory或者onPause中取消广播订阅。不然会报异常，这样你的广播接收器就一个非常驻型的了。

```java
protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        //动态注册BC2 接收者
        IntentFilter intentfilter = new IntentFilter("One");
        BC2 bc2 = new BC2();
        registerReceiver(bc2, intentfilter);
    }//必须在onDestory取消广播订阅
protected void onDestroy() {
        super.onDestroy();
        unregisterReceiver(bc2);
    }
```

- **注意：**
  1、动态注册的时候使用的是隐式Intent方式，所以在发送广播的时候需要使用隐式Intent去发送，不然是广播接收者是接收不到广播。

  2、静态注册的时候，因为在AndroidMainfest.xml中订阅的，所以在发送广播的时候使用显示Intent和隐式Intent都可以
  **所以以防万一，我们一般都采用隐式Intent去发送广播。**

  3、对于静态注册的ContextReceiver，回调onReceive(context, intent)中的context具体指的是ReceiverRestrictedContext；

  4、对于动态注册的ContextReceiver，回调onReceive(context, intent)中的context具体指的是Activity Context；

------

- ##### 生命周期。

- 它并不像 Activity 一样复杂，运行原理很简单

  调用对象  -》 实现onReceive -》 结束

  生命周期

- 广播接收者生命周期**只有十秒**左右，如果在 onReceive() 内做超过十秒内的事情，就会报**ANR(Application No Response) 程序无响应的错误信息**，如果需要完成一项比较耗时的工作 , 应该通过发送 Intent 给 **Service**， 由Service 来完成 。

- 这里不能使用子线程来解决 , 因为 BroadcastReceiver 的生命周期很短 , 子线程可能还没有结束BroadcastReceiver 就先结束了 .BroadcastReceiver 一旦结束 , 此时 BroadcastReceiver 的所在进程很容易在系统需要内存时被优先杀死 , 因为它属于空进程 ( 没有任何活动组件的进程 ). 如果它的宿主进程被杀死 , 那么正在工作的子线程也会被杀死 . 所以采用子线程来解决是不可靠的



### 三、广播实例

*为了帮助大家更好的理解，以下写了一个接收系统系统电量广播并处理的小例子。*

```java
//第一步，定义广播接收者
public class BatteryChangedReceiver extends BroadcastReceiver {  
private static final String TAG = "BatteryChangedReceiver";  
  @Override  
  public void onReceive(Context context, Intent intent) {  
      int currLevel =   intent.getIntExtra(BatteryManager.EXTRA_LEVEL, 0);  //当前电量  
      int total = intent.getIntExtra(BatteryManager.EXTRA_SCALE,   1);      //总电量  
      int percent = currLevel * 100 / total;  
      Log.i(TAG, "battery: " + percent + "%");  
   }  
}  
//第二步，注册广播接收者
protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        //动态注册电量广播接收者
        IntentFilter intentfilter = new IntentFilter(Intent.ACTION_BATTERY_CHANGED;);
        BatteryChangedReceiver batteryChangedReceiver = new BatteryChangedReceiver();
        registerReceiver(bc2, intentfilter);
    }//必须在onDestory取消广播订阅
protected void onDestroy() {
        super.onDestroy();
        unregisterReceiver(batteryChangedReceiver);
    }
```



### 【附录】

- 常见系统广播

```java
//关闭或打开飞行模式时的广播
Intent.ACTION_AIRPLANE_M;

//充电状态，或者电池的电量发生变化;//电池的充电状态、电荷级别改变，不能通过组建声;
Intent.ACTION_BATTERY_CH;

//表示电池电量低
Intent.ACTION_BATTERY_LO;

//表示电池电量充足
Intent.ACTION_BATTERY_OK;

//关闭或打开飞行模式时的广播
Intent.ACTION_AIRPLANE_MODE_CHANGED;

//充电状态，或者电池的电量发生变化//电池的充电状态、电荷级别改变，不能通过组建声明接收这个广播，只有通过Context.registerReceiver()注册
Intent.ACTION_BATTERY_CHANGED;

//表示电池电量低
Intent.ACTION_BATTERY_LOW;

//表示电池电量充足，即从电池电量低变化到饱满时会发出广播
Intent.ACTION_BATTERY_OKAY;

//在系统启动完成后，这个动作被广播一次（只有一次）。
Intent.ACTION_BOOT_COMPLETED;

//按下照相时的拍照按键(硬件按键)时发出的广播
Intent.ACTION_CAMERA_BUTTON;

//当屏幕超时进行锁屏时,当用户按下电源按钮,长按或短按(不管有没跳出话框)，进行锁屏时,android系统都会广播此Action消息
Intent.ACTION_CLOSE_SYSTEM_DIALOGS;

//设备当前设置被改变时发出的广播(包括的改变:界面语言，设备方向，等，请参考Configuration.java)
Intent.ACTION_CONFIGURATION_CHANGED;

//设备日期发生改变时会发出此广播
Intent.ACTION_DATE_CHANGED;

//设备内存不足时发出的广播,此广播只能由系统使用，其它APP不可用
Intent.ACTION_DEVICE_STORAGE_LOW;

//设备内存从不足到充足时发出的广播,此广播只能由系统使用，其它APP不可用
Intent.ACTION_DEVICE_STORAGE_OK;

//发出此广播的地方frameworks\base\services\java\com\android\server\DockObserver.java
Intent.ACTION_DOCK_EVENT;

//移动APP完成之后，发出的广播(移动是指:APP2SD)
Intent.ACTION_EXTERNAL_APPLICATIONS_AVAILABLE;

//正在移动APP时，发出的广播(移动是指:APP2SD)
Intent.ACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE;

//Gtalk已建立连接时发出的广播
Intent.ACTION_GTALK_SERVICE_CONNECTED;

//Gtalk已断开连接时发出的广播
Intent.ACTION_GTALK_SERVICE_DISCONNECTED;

//在耳机口上插入耳机时发出的广播
Intent.ACTION_HEADSET_PLUG;

//改变输入法时发出的广播
Intent.ACTION_INPUT_METHOD_CHANGED;

//设备当前区域设置已更改时发出的广播
Intent.ACTION_LOCALE_CHANGED;

//表示用户和包管理所承认的低内存状态通知应该开始。
Intent.ACTION_MANAGE_PACKAGE_STORAGE;

//未正确移除SD卡(正确移除SD卡的方法:设置--SD卡和设备内存--卸载SD卡)，但已把SD卡取出来时发出的广播 ,扩展介质（扩展卡）已经从 SD 卡插槽拔出，但是挂载点 (mount point) 还没解除 (unmount)
Intent.ACTION_MEDIA_BAD_REMOVAL;

//按下"Media Button" 按键时发出的广播,假如有"Media Button" 按键的话(硬件按键)
Intent.ACTION_MEDIA_BUTTON;

//插入外部储存装置，比如SD卡时，系统会检验SD卡，此时发出的广播?
Intent.ACTION_MEDIA_CHECKING;

//已拔掉外部大容量储存设备发出的广播（比如SD卡，或移动硬盘）,不管有没有正确卸载都会发出此广播, 用户想要移除扩展介质（拔掉扩展卡）。
Intent.ACTION_MEDIA_EJECT;

//插入SD卡并且已正确安装（识别）时发出的广播, 扩展介质被插入，而且已经被挂载。
Intent.ACTION_MEDIA_MOUNTED;

//拓展介质存在，但使用不兼容FS（或为空）的路径安装点检查介质包含在Intent.mData领域。
Intent.ACTION_MEDIA_NOFS;

//外部储存设备已被移除，不管有没正确卸载,都会发出此广播， 扩展介质被移除。
Intent.ACTION_MEDIA_REMOVED;

//广播：已经扫描完介质的一个目录
Intent.ACTION_MEDIA_SCANNER_FINISHED;

//请求媒体扫描仪扫描文件并将其添加到媒体数据库。
Intent.ACTION_MEDIA_SCANNER_SCAN_FILE;

//广播：开始扫描介质的一个目录
Intent.ACTION_MEDIA_SCANNER_STARTED;

// 广播：扩展介质的挂载被解除 (unmount)，因为它已经作为 USB 大容量存储被共享。
Intent.ACTION_MEDIA_SHARED;

Intent.ACTION_MEDIA_UNMOUNTABLE;//

// 广播：扩展介质存在，但是还没有被挂载 (mount)
Intent.ACTION_MEDIA_UNMOUNTED

Intent.ACTION_NEW_OUTGOING_CALL;

//成功的安装APK之后//广播：设备上新安装了一个应用程序包。//一个新应用包已经安装在设备上，数据包括包名（最新安装的包程序不能接收到这个广播）
Intent.ACTION_PACKAGE_ADDED;

//一个已存在的应用程序包已经改变，包括包名
Intent.ACTION_PACKAGE_CHANGED;

//清除一个应用程序的数据时发出的广播(在设置－－应用管理－－选中某个应用，之后点清除数据时?)//用户已经清除一个包的数据，包括包名（清除包程序不能接收到这个广播）
Intent.ACTION_PACKAGE_DATA_CLEARED;

//触发一个下载并且完成安装时发出的广播，比如在电子市场里下载应用？
Intent.ACTION_PACKAGE_INSTALL;

//成功的删除某个APK之后发出的广播, 一个已存在的应用程序包已经从设备上移除，包括包名（正在被安装的包程序不能接收到这个广播）
Intent.ACTION_PACKAGE_REMOVED;

//替换一个现有的安装包时发出的广播（不管现在安装的APP比之前的新还是旧，都会发出此广播？）
Intent.ACTION_PACKAGE_REPLACED;

//用户重新开始一个包，包的所有进程将被杀死，所有与其联系的运行时间状态应该被移除，包括包名（重新开始包程序不能接收到这个广播）
Intent.ACTION_PACKAGE_RESTARTED;

//插上外部电源时发出的广播
Intent.ACTION_POWER_CONNECTED;

//已断开外部电源连接时发出的广播
Intent.ACTION_POWER_DISCONNECTED;

Intent.ACTION_PROVIDER_CHANGED;//

//重启设备时的广播
Intent.ACTION_REBOOT;

//屏幕被关闭之后的广播
Intent.ACTION_SCREEN_OFF;

//屏幕被打开之后的广播
Intent.ACTION_SCREEN_ON;

//关闭系统时发出的广播
Intent.ACTION_SHUTDOWN;

//时区发生改变时发出的广播
Intent.ACTION_TIMEZONE_CHANGED;

//时间被设置时发出的广播
Intent.ACTION_TIME_CHANGED;

//广播：当前时间已经变化（正常的时间流逝）， 当前时间改变，每分钟都发送，不能通过组件声明来接收
，只有通过Context.registerReceiver()方法来注册
Intent.ACTION_TIME_TICK;

//一个用户ID已经从系统中移除发出的广播
Intent.ACTION_UID_REMOVED;

//设备已进入USB大容量储存状态时发出的广播？
Intent.ACTION_UMS_CONNECTED;

//设备已从USB大容量储存状态转为正常状态时发出的广播？
Intent.ACTION_UMS_DISCONNECTED;

Intent.ACTION_USER_PRESENT;//

//设备墙纸已改变时发出的广播
Intent.ACTION_WALLPAPER_CHANGED;
```