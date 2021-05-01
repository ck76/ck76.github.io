[TOC]

- 在日常开发中，Android Native端与Flutter端通信交互的应用场景十分常用
- 今天，**我将全面讲解Android Native端与Flutter端通信的交互的方式**，旨在让你熟练掌握Android Native端与Flutter端的通信交互，包括：原理、架构、通信方式等，希望你们会喜欢。



示意图

------

# 目录

<img src="https://tva1.sinaimg.cn/large/008eGmZEly1gmwgsajhrwj30wq0ru418.jpg" alt="img" style="zoom: 50%;" />

示意图

------

# 1. 通信原理

### 1.1 通信架构

Android 与 Flutter之间的通信消息传递媒介：平台通道（PlantformChannel）



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgs89t94j30xc05haaq.jpg)

示意图

平台通道（PlantformChannel）主要包括三种：（下面会详细介绍）

- 基本信息通道（BasicMessageChannel）
- 方法通道（MethodChannel）
- 数据流通道（EventChannel）

### 1.2 整体设计

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgs7a1p5j30xc0bigq2.jpg)

示意图

### 1.3 详细说明

- 数据载体：ByteBuffer
- 传递媒介：BinaryMessenger。在Android侧，BinaryMessenger是一个接口，在FlutterView中实现了该接口，通过JNI来与系统底层通信。在Flutter侧，BinaryMessenger是一个类，该类的作用 = 与类window通信，而类window才真正与系统底层沟通
- 消息传递方式：异步
- 线程切换：在系统底层实现，系统底层屏蔽了线程切换、数据拷贝等大量复杂操作，使得Android侧与flutter侧能方便通信

更加详细的底层原理可参考：[咸鱼团队的技术文章](https://links.jianshu.com/go?to=https%3A%2F%2Fjuejin.im%2Fpost%2F5b84ff6a6fb9a019f47d1cc9)

------

# 2. 通信交互方式

### 2.1 简介

Flutter定义了三种类型的通信交互传递方式，对应三种平台通道（PlantformChannel） ：

- 基本信息通道（BasicMessageChannel）
- 方法通道（MethodChannel）
- 数据流通道（EventChannel）

### 2.2 设计原理

三种通道各有用途，但设计上相近，均有三个重要成员变量：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgs5ty7ij30xc0jvk2p.jpg)

示意图

附录：Flutter定义了两种Codec：MessageCodec、MethodCodec，介绍如下：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgs4fhbxj30xc0fkn7t.jpg)

示意图

# 2.3 应用场景

针对Flutter给出的三种通道方式，我们对于Android 与 Flutter相互通信的应用场景主要包括：

- 基本信息通道（BasicMessageChannel）：用于传递字符串&半结构化的信息
- 方法通道（MethodChannel）：用于传递方法调用（method invocation）
- 数据流通道（EventChannel）： 用于数据流（event streams）的通信

下面，我将详细讲解。

------

# 3. 准备工作

在讲解上述三种通道前，我们需要将Flutter集成到当前的Android目录中

### 步骤1：创建 flutter module 模块



```cpp
// 步骤1： cd到Android 工程目录

// 步骤2：命令行执行
flutter create -t module 模块名称
// 示例：flutter create -t module flutter_plugin
```

打开项目工程目录会发现，Flutter作为Module集成到Android工程中



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgs2ndsrj30sm0dtdm8.jpg)

示意图

### 步骤2：添加flutter module模块到当前项目



```dart
// 步骤1：在项目根目录的settings.gradle中添加：
setBinding(new Binding([gradle: this]))
evaluate(new File(
        settingsDir.parentFile,
        "AndroidxFlutter/flutter_plugin/.android/include_flutter.groovy"
))
// 注：”工程名/flutter模块名/.android/include_flutter.groovy“

// 步骤2：在app/build.gradle文件中的dependencies添加 flutter依赖
dependencies {
    implementation fileTree(dir: 'libs', include: ['*.jar'])
    implementation 'com.android.support:appcompat-v7:27.1.1'
    implementation 'com.android.support.constraint:constraint-layout:1.1.3'
 
    ......
 
    implementation project(':flutter')
}

// 步骤3：在app/build.gradle文件中的android添加如下代码
android{
....
compileOptions {
        sourceCompatibility JavaVersion.VERSION_1_8
        targetCompatibility JavaVersion.VERSION_1_8
    }

{
```

至此，Flutter已经集成到当前Android工程项目中

------

# 4. 详解讲解（含Demo）

下面，我将手把手带你们详细分析上述三个通道，并结合示例Demo

## 通道1：基本信息通道（BasicMessageChannel）

作用：传递字符串 & 半结构化的信息

### 步骤1：自定义BasicMessageChannel工具类

（Native端）`BasicMessageChannelPlugin.java`

- 创建BasicMessageChannel对象（需传入FlutterView、channel name和codec）
- 注册Channel对象处理的Handler
- 定义要发送到Flutter的消息的函数
- 接受到Flutter消息时进行回应接受的函数



```tsx
// 此处以发送的数据类型是String为例
public class BasicMessageChannelPlugin implements BasicMessageChannel.MessageHandler<String> {

    private Activity activity;
    private BasicMessageChannel<String> messageChannel;

    // 步骤1：创建BasicMessageChannelPlugin实例
    static BasicMessageChannelPlugin registerWith(FlutterView flutterView) {
        return new BasicMessageChannelPlugin(flutterView); 
    }

    private BasicMessageChannelPlugin(FlutterView flutterView) {
        this.activity = (Activity) flutterView.getContext();
        // 创建BasicMessageChannel对象（需传入FlutterView、channel name和codec）
        this.messageChannel = new BasicMessageChannel<String>(flutterView, "BasicMessageChannelPlugin", StringCodec.INSTANCE);
        // 注册处理的Handler
        messageChannel.setMessageHandler(this);
    }

    // 步骤2：向Flutter发送消息
    // 传入参数：需发送的消息 & 回调处理
    void send(String str, BasicMessageChannel.Reply<String> reply) {
        messageChannel.send(str, reply);
    }

    // 步骤3：复写回调函数：接受到Flutter消息时进行回应
    @Override
    public void onMessage(String s, BasicMessageChannel.Reply<String> reply) {
        // s即为Flutter发送过来的消息
        System.out.println("Native：收到了"+s);
        // 接受到Flutter信息后，采用reply实例将返回值返回到Flutter层
        reply.reply("Native确认了" + s);
    }
}
```

### 步骤2：定义Flutter要发送到Native端的消息 & 接受消息的函数方法，及其对应消息内容

（Flutter端）`main.dart`



```dart
/**
 *  导入库
 **/
import 'package:flutter/material.dart'; // Material UI组件库
import 'dart:ui';

import 'package:flutter/services.dart'; // 引入后可以使用window对象

void main() => runApp(MyApp());

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: _buildWidgetForNativeRoute(window.defaultRouteName),// Native传来的route = window.defaultRouteName
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
    );
  }
}

/// 该方法用于判断原生界面传递过来的路由值，加载不同的页面
Widget _buildWidgetForNativeRoute(String route) {
  switch (route) {
    case 'flutterView': // 当route值为flutterView时显示
      return FlutterContactPage();

    default:  // 默认的路由值为 '/'，所以在default情况也需返回页面，否则dart会报错
      return Container(
        child: Center(child: Text('路由值 = deafult',style: TextStyle(fontSize: 20.0, color: Colors.black),)),
        color: Colors.red,
      );
  }
}

class FlutterContactPage extends StatelessWidget {
  // 注册对应的channel，即自定义的BasicMessageChannel
  // 注：要保证channel name、codec与原生层一致
  BasicMessageChannel<String> _messageChannel =
  BasicMessageChannel("BasicMessageChannelPlugin", StringCodec());

  @override
  Widget build(BuildContext context) {

    // 向Native发送消息
    return Scaffold(
      appBar: AppBar(
        title: Text('Flutter Page'),
      ),
      body: RaisedButton(// 为了展示使用按钮，通过channel传输消息出去，同时将原生层返回的消息打印出来
        onPressed: () {
          _messageChannel
              .send('Flutter发起第二次握手') // 发送的消息
              .then((str) { // Native针对该消息返回的消息
            print('Flutter:收到了 $str');
          });
        },
        child: Text('Send Message to Native'),
      ),
    );

    // 接受Native发送过来的消息
    _messageChannel.setMessageHandler((message) => Future<String>(() {
      print("Flutter：接受到了：" + message); // message即为Native发送过来的消息
      return "Flutter确认的"+ message; // Flutter针对Native发送的消息进行返回
    }));
  }
}
```

### 步骤3：（Native端）连接Native和Flutter的中间层

```
MainActivity.java
```

- 创建FlutterView组件
- 创建 & 注册MethodChannel
- 发送到Flutter的消息 & 接受消息的消息内容
- 发起要调用Flutter端的请求



```java
public class MainActivity extends AppCompatActivity {

    private ViewGroup.LayoutParams layoutParams;
    private Button btn;
    private BasicMessageChannelPlugin mBasicMessageChannelPlugin;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // 通过Flutter.createView()创建FlutterView组件方式
        FlutterView flutterView = Flutter.createView(this, getLifecycle(), "flutterView");

        // 将Flutter视图添加到原生布局中的Fragment中(为了方便显示，此处采用按钮触发形式)
        btn = findViewById(R.id.btn);
        btn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                layoutParams = new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
                addContentView(flutterView, layoutParams); // 将flutter添加到布局中
                mBasicMessageChannelPlugin = BasicMessageChannelPlugin.registerWith(flutterView); // 关联通道
                // 向Flutter发送消息
                mBasicMessageChannelPlugin.send("Native发起第一次握手",new BasicMessageChannel.Reply<String>(){
                    @Override
                    public void reply(String s){
                        System.out.println("Native：收到了" + s);
                    }
                });
            }
        });
    }
}
```

### 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgrzz4nyg30p20bcb2c.gif)

示意图

------

## 通道2：方法通道（MethodChannel）

作用：传递方法调用（method invocation），即Native与Flutter相互调用对方的方法（具备返回值）

### 步骤1：自定义MethodChannel工具类（Native端）

*MethodChannelPlugin.java*：

- 创建MethodChannel实例（传入channel name）
- 注册需处理的对应Handler
- 定义要通知Flutter端调用的方法
- 接受Flutter端要调用的方法



```java
public class MethodChannelPlugin implements MethodChannel.MethodCallHandler {

    private Activity activity;
    private MethodChannel channel;

    // 1. 创建MethodChannel实例（传入channel name）
    public static MethodChannelPlugin registerWith(FlutterView flutterView) {
        MethodChannel channel = new MethodChannel(flutterView, "MethodChannelPlugin");
        MethodChannelPlugin methodChannelPlugin = new MethodChannelPlugin((Activity) flutterView.getContext(), channel);
        channel.setMethodCallHandler(methodChannelPlugin);// 2. 注册处理的Handler
        return methodChannelPlugin;
    }

    private MethodChannelPlugin(Activity activity, MethodChannel channel) {
        this.activity = activity;
        this.channel = channel;

    }
    // 2. 用于调用Flutter端方法，无返回值
    // method为需调用的方法名
    public void invokeMethod(String method, Object o) {
        channel.invokeMethod(method, o);
    }

    // 3. 用于调用Flutter端方法，有返回值
    // method为需调用的方法名、返回值在result内
    public void invokeMethod(String method, Object o, MethodChannel.Result result) {
        channel.invokeMethod(method, o, result);
    }

    // 4. 复写onMethodCall（）：根据Flutter的要求，调用Native方法
    @Override
    public void onMethodCall(MethodCall methodCall, MethodChannel.Result result) {
        switch (methodCall.method) {
            case "FlutterInvokeFlutter":// Flutter要求Native调用的方法是FlutterInvokeFlutter

                System.out.println("Native收到了Flutter的请求方式是："+methodCall.method);
                System.out.println("Native收到了Flutter的请求参数是："+methodCall.arguments);
                result.success("Native收到了Flutter的请求方法：" + methodCall.method);// 给flutter端的返回值
                break;
            default:
                result.notImplemented(); // 若无找到对应的方法名，则通过该方法返回异常
                break;
        }
    }
}
```

### 步骤2：（Flutter端）定义要通知Native调用的方法 & 接受Native端要调用的方法

*main.dart*



```dart
/**
 *  导入库
 **/
import 'package:flutter/material.dart'; // Material UI组件库
import 'dart:ui';

import 'package:flutter/services.dart'; // 引入后可以使用window对象

void main() => runApp(MyApp());

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: _buildWidgetForNativeRoute(window.defaultRouteName),
      // Native传来的route = window.defaultRouteName
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
    );
  }
}

// 该方法用于判断原生界面传递过来的路由值，加载不同的页面
Widget _buildWidgetForNativeRoute(String route) {
  switch (route) {
    case 'flutterView': // 当route值为flutterView时显示
      return FlutterContactPage();

    default: // 默认的路由值为 '/'，所以在default情况也需返回页面，否则dart会报错
      return Container(
        child: Center(
            child: Text(
          '路由值 = deafult',
          style: TextStyle(fontSize: 20.0, color: Colors.black),
        )),
        color: Colors.red,
      );
  }
}

class FlutterContactPage extends StatelessWidget {
  // 注册对应的MethodChannel
  // 注：要保证channel name、codec与原生层一致
  MethodChannel _methodChannel = MethodChannel("MethodChannelPlugin");

  @override
  Widget build(BuildContext context) {
    // 1. 根据Native端的要求，调用对应方法
    _methodChannel.setMethodCallHandler((handler) => Future<String>(() {
          print("Native端要调用的方法和参数是：${handler}");
          // 监听native发送的方法名及参数
          switch (handler.method) {
            case "AndroidInvokeFlutter": // Native要求Flutter调用的方法是send（）
              _send(handler.method, handler.arguments); //handler.arguments表示native传递的方法参数
              break;
          }
          return "Flutter确认消息";
        }));

    // 2. 通知Native端要调用哪个方法
    return Scaffold(
      appBar: AppBar(
        title: Text('Flutter Page'),
      ),
      body: RaisedButton(
        // 为了展示所以使用按钮，通过channel告诉Native要调用哪个方法
        onPressed: () {
          _methodChannel
              .invokeMethod("FlutterInvokeFlutter", "carsonho") // 参数1=告诉Native要调用的方法名，参数2 = 传递的参数
              .then((result) { // invokeMethod().then() 来处理正常结束的逻辑（获得返回值）
            print('$result'); 
                 // 成功：通过result.success 返回值
                // 异常：通过 result.error 返回异常信息，可通过catchError 处理异常
          });
        },
        child: Text('Send Message to Native'),
      ),
    );
  }

  // 需发送的方法
  void _send(method, arg) {
    print('Flutter根据Native端的要求调用了方法$method');
    print('该方法的参数是：$arg');
  }
}
```

### 步骤3：（Native端）连接Native和Flutter的中间层

```
MainActivity.java
```

- 创建FlutterView组件
- 创建 & 注册MethodChannel
- Native端定义要求Flutter端调用的方法
- 发起要调用Flutter端的请求



```java
public class MainActivity extends AppCompatActivity {

    private ViewGroup.LayoutParams layoutParams;
    private Button btn;
    private MethodChannelPlugin mMethodChannelPlugin;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // 1. 通过Flutter.createView()创建FlutterView组件方式
        FlutterView flutterView = Flutter.createView(this, getLifecycle(), "flutterView");

        // 将Flutter视图添加到原生布局中的Fragment中(为了方便显示，此处采用按钮触发形式)
        btn = findViewById(R.id.btn);
        btn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                layoutParams = new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
                addContentView(flutterView, layoutParams); // 将flutter添加到布局中
                mMethodChannelPlugin = MethodChannelPlugin.registerWith(flutterView); // 关联通道
                // Native告诉Flutter要调用的方法是send（）
                mMethodChannelPlugin.invokeMethod("AndroidInvokeFlutter","carsonho");
            }
        });
    }
}
```

### 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgrxl561g30p20bc4qt.gif)

示意图

------

## 通道3：数据流通道（EventChannel）

作用：用于数据流（event streams）的通信，即：

- 原生层：通过 sink 不断添加数据 & 发送多个信息到 Flutter 层
- Flutter层：接收到数据的变化就会作出新相应的处理，表现为一个stream

### 步骤1：自定义EventChannel工具类（Native端）

`EventChannelPlugin.java`：

- 创建EventChannel实例（传入channel name）
- 定义Native发送数据、停止发送 & 发送失败函数
- 复写Flutter端开始监听时的回调函数onListen（）
- 复写Flutter端不再接受监听时的回调函数onCancel（）



```java
public class EventChannelPlugin implements EventChannel.StreamHandler {

    private EventChannel.EventSink eventSink;
    private Activity activity;

    // 1. 创建 & 注册EventChannel
    static EventChannelPlugin registerWith(FlutterView flutterView) {
        EventChannel channel = new EventChannel(flutterView, "EventChannelPlugin");
        EventChannelPlugin plugin = new EventChannelPlugin(flutterView);
        channel.setStreamHandler(plugin);//设置对应Handler
        return plugin;
    }

    private EventChannelPlugin(FlutterView flutterView) {
        this.activity = (Activity) flutterView.getContext();
    }

    // Native端开始发送数据
    void send(Object params) {
        if (eventSink != null) {
            eventSink.success(params);
            System.out.println("sink success");
        }
    }
    // Native端停止发送数据
    void cancel() {
        if (eventSink != null) {
            eventSink.endOfStream();
        }
    }

    // Native端发送数据失败
    void sendError(String str1, String str2, Object params) {
        if (eventSink != null) {
            eventSink.error(str1, str2, params);
        }
    }

    // 回调时机：Flutter端开始监听该channel时
    // 说明通道已经建立好，Native可以开始发送数据了
    // 参数1 = Flutter端初始化EventChannel时返回的值，仅此一次
    // 参数2 = 传数据的载体
    @Override
    public void onListen(Object o, EventChannel.EventSink eventSink) {
        this.eventSink = eventSink; //此处注意时序，必须得该方法回调后，Native端才允许发送数据
        System.out.println( "onListen()：eventSink = " + eventSink);
    }

    // Flutter端不再接收数据时回调
    @Override
    public void onCancel(Object o) {
        System.out.println("onCancel()");
        this.eventSink = null;
    }
}
```

### 步骤2：Flutter要展示的布局（Flutter）

`main.dart`：

- 设置展示的布局
- 监听Native什么时候发送数据
- 设置正常接受数据、错误接受数据等方法回调



```dart
/**
 *  导入库
 **/
import 'package:flutter/material.dart'; // Material UI组件库
import 'dart:ui';
import 'dart:async';
import 'package:flutter/services.dart'; // 引入后可以使用window对象

void main() => runApp(MyApp());

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: _buildWidgetForNativeRoute(window.defaultRouteName),
      // Native传来的route = window.defaultRouteName
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
    );
  }
}

// 该方法用于判断原生界面传递过来的路由值，加载不同的页面
Widget _buildWidgetForNativeRoute(String route) {
  switch (route) {
    case 'flutterView': // 当route值为flutterView时显示
      return FlutterContactPage();

    default: // 默认的路由值为 '/'，所以在default情况也需返回页面，否则dart会报错
      return Container(
        child: Center(
            child: Text(
          '路由值 = deafult',
          style: TextStyle(fontSize: 20.0, color: Colors.black),
        )),
        color: Colors.red,
      );
  }
}

class FlutterContactPage extends StatefulWidget {
  @override
  _FlutterContactPageState createState() => _FlutterContactPageState();
}

class _FlutterContactPageState extends State<FlutterContactPage> {
  // 注册对应的MethodChannel
  // 注：要保证channel name、codec与原生层一致
  EventChannel _eventChannelPlugin = EventChannel("EventChannelPlugin");
  StreamSubscription _streamSubscription;

  // 在initState状态下设置监听Native端发送
  @override
  void initState() {
    _streamSubscription = _eventChannelPlugin
        .receiveBroadcastStream() // 对应Native端onListen（）的第一个参数，可不传
        .listen(_onToDart, onError: _onToDartError, onDone: _onDone);
    // 开启监听，并分别传入：
    // _onToDart方法：正常接收到Native数据时调用
    // _onToDartError方法：接收Native数据异常时调用
    // _onDone方法：发送数据完成时调用
    super.initState();
  }

  // Native端发送正常数据回调方法，每一次发送都会调用
  void _onToDart(message) {
    print('正常接收：$message');
  }
  // Native出错时回调方法
  void _onToDartError(error) {
    print('错误接收：$error');
  }
  // 当native发送数据完成时调用的方法
  void _onDone() {
    print("消息传递完毕");
  }

  @override
  Widget build(BuildContext context) {
    // 2. 通知Native端要调用哪个方法
    return Scaffold(
      appBar: AppBar(
        title: Text('Flutter Page'),
      ),
      body: RaisedButton(
        child: Text('begin counting'),
      ),
    );
  }
}
```

### 步骤3：连接Native和Flutter的中间层

`MainActivity.java`：

- 创建FlutterView组件
- 创建 & 注册EventChannel
- Native端定义要求Flutter端调用的方法
- 发起要调用Flutter端的请求



```java
public class MainActivity extends AppCompatActivity {

    private ViewGroup.LayoutParams layoutParams;
    private Button btn;
    private EventChannelPlugin mEventChannelPlugin;
    private int count;
    private Timer mTimer;
    private TimerTask mTimertask;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // 1. 通过Flutter.createView()创建FlutterView组件方式
        FlutterView flutterView = Flutter.createView(this, getLifecycle(), "flutterView");
        // 2. 关联通道
        mEventChannelPlugin = EventChannelPlugin.registerWith(flutterView);

        // 3. 将Flutter视图添加到原生布局中的Fragment中(为了方便显示，此处采用按钮触发形式)
        btn = findViewById(R.id.btn);
        btn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                layoutParams = new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
                addContentView(flutterView, layoutParams); // 将flutter添加到布局中

                // 4. 为了方便展示，采用计时器Timer发送一系列数据到Flutter
                count = 0;
                mTimer = new Timer(true);
                mTimertask = new TimerTask() {
                    public void run() {

                        // 回到主线程后Native发送数据
                        Handler mainHandler1 = new Handler(Looper.getMainLooper());
                        mainHandler1.post(new Runnable() {
                            @Override
                            public void run() {
                                mEventChannelPlugin.send(count++);
                            }
                        });

                        // 数到5时停止
                        while (count == 5) {

                            // 回到主线程后Native停止发送数据
                            Handler mainHandler2 = new Handler(Looper.getMainLooper());
                            mainHandler2.post(new Runnable() {
                                @Override
                                public void run() {
                                    mEventChannelPlugin.cancel();
                                }
                            });
                            // 关闭计时器
                            mTimer.cancel();
                            mTimer = null;
                            mTimertask.cancel();
                            mTimertask = null;
                        }
                    }
                };
                // 开启计时器（发送数据）
                mTimer.schedule(mTimertask, 1, 1000);
            }
        });
    }
}
```

### 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgrudvkfg30p20bcb2c.gif)

示意图

- 至此，关于Android通过三种通道与Flutter通信讲解完毕。
- 下面，我再用讲解一个较为基础的场景：在Android中显示Flutter界面

------

# 5. 基础场景：在Android中显示Flutter界面

此处分两种方式：

1. Flutter界面显示在Activity ：Flutter.createView（）
2. Flutter界面显示在Fragment：使用Flutter.createFragment（）

### 方式1：显示在Activity

使用Flutter.createView（），步骤如下：

### 步骤1：Android端设置好跳转 & 显示逻辑

*MainActivity.java*



```java
public class MainActivity extends AppCompatActivity {

    private ViewGroup.LayoutParams layoutParams;
    private Button btn;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        // 1. 通过Flutter.createView()创建FlutterView组件方式
        FlutterView flutterView = Flutter.createView(this, getLifecycle(), "flutterView");
        // 参数说明：
        // 参数1：Activity activity，Activity实例
        // 参数2：final Lifecycle lifecycle:，定义具有Android生命周期的对象
        // 参数3：final String initialRoute:，初始化的视图路由名称，后续会根据该路由进行显示Flutter视图

        // 2. 将Flutter视图添加到原生布局中(为了方便显示，此处采用按钮触发形式)
        btn = findViewById(R.id.btn);
        btn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                layoutParams = new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
                addContentView(flutterView, layoutParams); // 将flutter添加到布局中
            }
        });
    }
}
```

### 步骤2：在Flutter端中设置好要显示的布局

*flutter_plugin / lib / main.dart*



```dart
/**
 *  导入库
 **/
import 'package:flutter/material.dart'; // Material UI组件库
import 'dart:ui'; // 引入后可以使用window对象

void main() => runApp(MyApp());

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: _buildWidgetForNativeRoute(window.defaultRouteName),// Native传来的route = window.defaultRouteName
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
    );
  }
}

// 该方法用于判断原生界面传递过来的路由值，加载不同的页面
Widget _buildWidgetForNativeRoute(String route) {
  switch (route) {
    case 'flutterView': // 当route值为flutterView时显示
      return Container(
        child: Center(child: Text('路由值 = flutterView',style: TextStyle(fontSize: 20.0, color: Colors.black),)),
        color: Colors.green,
      );

    default:  // 默认的路由值为 '/'，所以在default情况也需返回页面，否则dart会报错
      return Container(
        child: Center(child: Text('路由值 = deafult',style: TextStyle(fontSize: 20.0, color: Colors.black),)),
        color: Colors.red,
      );
  }
}
```

### 效果图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgrs2fvig307w0ci49s.gif)

示意图

### 方式2：显示在Fragment

使用Flutter.createFragment（），步骤如下

### 步骤1：Android端设置好跳转 & 显示逻辑

*MainActivity.java*



```java
public class MainActivity extends AppCompatActivity {

    private ViewGroup.LayoutParams layoutParams;
    private Button btn;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // 将Flutter视图添加到原生布局中的Fragment中(为了方便显示，此处采用按钮触发形式)
        btn = findViewById(R.id.btn);
        btn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                getSupportFragmentManager().beginTransaction()
                        .add(R.id.fragment_container, Flutter.createFragment("flutterView"))
                        .commit();
                
                btn.setVisibility(View.GONE);
            }
        });
    }
}
```

*activity_main.xml*



```xml
<?xml version="1.0" encoding="utf-8"?>
<android.support.constraint.ConstraintLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:app="http://schemas.android.com/apk/res-auto"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    tools:context=".MainActivity">

    <Button
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:id="@+id/btn"
        android:text="start"
        app:layout_constraintBottom_toBottomOf="parent"
        app:layout_constraintLeft_toLeftOf="parent"
        app:layout_constraintRight_toRightOf="parent"
        app:layout_constraintTop_toTopOf="parent" />

    <!-- 用于加载 fragment -->
    <FrameLayout
        android:id="@+id/fragment_container"
        android:layout_width="match_parent"
        android:layout_height="match_parent" />

</android.support.constraint.ConstraintLayout>
```

### 步骤2：在Flutter端设置好布局显示

*flutter_plugin / lib / main.dart*



```dart
/**
 *  导入库
 **/
import 'package:flutter/material.dart'; // Material UI组件库
import 'dart:ui'; // 引入后可以使用window对象

void main() => runApp(MyApp());

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: _buildWidgetForNativeRoute(window.defaultRouteName),// Native传来的route = window.defaultRouteName
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
    );
  }
}

// 该方法用于判断原生界面传递过来的路由值，加载不同的页面
Widget _buildWidgetForNativeRoute(String route) {
  switch (route) {
    case 'flutterView': // 当route值为flutterView时显示
      return Container(
        child: Center(child: Text('路由值 = flutterView',style: TextStyle(fontSize: 20.0, color: Colors.black),)),
        color: Colors.green,
      );

    default:  // 默认的路由值为 '/'，所以在default情况也需返回页面，否则dart会报错
      return Container(
        child: Center(child: Text('路由值 = deafult',style: TextStyle(fontSize: 20.0, color: Colors.black),)),
        color: Colors.red,
      );
  }
}
```

至此，关于Android 与 Flutter的相互通信讲解完毕。

------

# 6. 总结

- 本文全面介绍了Android Native端与Flutter端的通信方式。
- 接下来推出的文章，我将继续讲解Flutter的相关知识，包括使用语法、实战等，感兴趣的读者可以继续关注我的博客哦：[Carson_Ho的Android博客](https://www.jianshu.com/users/383970bef0a0/latest_articles)

------

# 请点赞！因为你们的赞同/鼓励是我写作的最大动力！

> **相关文章阅读**
> [Android开发：最全面、最易懂的Android屏幕适配解决方案](https://www.jianshu.com/p/ec5a1a30694b)
> [Android开发：史上最全的Android消息推送解决方案](https://www.jianshu.com/p/b61a49e0279f)
> [Android开发：最全面、最易懂的Webview详解](https://www.jianshu.com/p/3c94ae673e2a)
> [Android开发：JSON简介及最全面解析方法!](https://www.jianshu.com/p/b87fee2f7a23)
> [Android四大组件：Service服务史上最全面解析](https://www.jianshu.com/p/d963c55c3ab9)
> [Android四大组件：BroadcastReceiver史上最全面解析](https://www.jianshu.com/p/ca3d87a4cdf3)



作者：Carson_Ho
链接：https://www.jianshu.com/p/b75486c67547
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。