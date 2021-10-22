[TOC]

##### DeepLink 是什么？

Deep Link，又叫deep linking，中文译为深层链接。简单地从用户体验来讲，Deep Link，就是可以让你在手机的浏览器/Google Search上点击搜索的结果，便能直接跳转到已安装的应用中的某一个页面的技术。

例如我们收到淘宝店家发送的短信，我们点击短信中的短链接，然后在浏览器里面打开短链接，然后浏览器里面会弹出“网页请求打开淘宝”的对话框，然后我们点击打开，就可以打开淘宝并跳转到响应的界面。

我们先用Android原生的 scheme来实现一个类似的功能。关于scheme相关的知识可以参考 [Scheme详解](https://links.jianshu.com/go?to=https%3A%2F%2Fblog.csdn.net%2Fwangkeke1860%2Farticle%2Fdetails%2F49850997)

#### 首先新建一个Activity，然后在AndroidManifest.xml文件中,为Activity添加响应的scheme信息。



```xml
         <activity
                android:name=".SecondActivity"
                android:exported="true"
                android:label="SecondActivity">
            <intent-filter>
                <data
                        android:host="www.dmw.com"
                        android:scheme="com.dmw.android.wakeupappdemo"/>

                <!-- 必须加上该项,对一段数据执行的“标准”操作-->
                <action android:name="android.intent.action.VIEW"/>
                <!-- 必须加上该项 -->
                <category android:name="android.intent.category.DEFAULT"/>
                <!-- 如果希望该应用可以通过浏览器的连接启动，则添加该项 -->
                <category android:name="android.intent.category.BROWSABLE"/>
            </intent-filter>

            <intent-filter>
                <data
                        android:host="www.dmwdmw.com"
                        android:scheme="com.dmwdmw.android.wakeupappdemo"/>

                <!-- 必须加上该项,对一段数据执行的“标准”操作-->
                <action android:name="android.intent.action.VIEW"/>
                <!-- 必须加上该项 -->
                <category android:name="android.intent.category.DEFAULT"/>
                <!-- 如果希望该应用可以通过浏览器的连接启动，则添加该项 -->
                <category android:name="android.intent.category.BROWSABLE"/>
            </intent-filter>
        </activity>
```

注意：如果你有多个data标签的话，你需要使用多个 <intent-filter>标签，并确保每个 <intent-filter>标签里面只有一个data标签。不然会出现`Error: Activity not started, unable to resolve Intent`这样的问题。
 参考 [deep-linking-intent-does-not-work](https://links.jianshu.com/go?to=https%3A%2F%2Fstackoverflow.com%2Fquestions%2F24808777%2Fdeep-linking-intent-does-not-work)

#### 然后在Activity中接收启动Activity的时候传递的参数。



```kotlin
class SecondActivity : AppCompatActivity() {

    companion object {

        val TYPE_INTENT = "type"
        val URL_INTENT = "url"
        val NAME_INTENT = "name"

        fun launch(context: Context) {
            val intent = Intent(context, SecondActivity::class.java)
            context.startActivity(intent)
        }
    }


    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_second)

        if (intent.data != null) {
            val uri = intent.data
            tvScheme.text = uri?.scheme ?: "no scheme"
            tvHost.text = uri?.host ?: "no host"
            tvAuthority.text = uri?.authority ?: "no authority"

            val type = uri.getQueryParameter(TYPE_INTENT)
            val url = uri.getQueryParameter(URL_INTENT)
            val name = uri.getQueryParameter(NAME_INTENT)

            tvParams.text = "type=$type,url=$url,name=$name"

        }
    }
}
```

然后在Android studio自带的终端里面通过 am 命令启动 Activity。



```bash
adb shell
am start -d "com.dmw.android.wakeupappdemo://www.dmw.com:8080?type=green&url=123"
```

- 匹配的intent-filter



```xml
<intent-filter>
                <data
                        android:host="www.dmw.com"
                        android:scheme="com.dmw.android.wakeupappdemo"/>
                <!-- 必须加上该项,对一段数据执行的“标准”操作-->
                <action android:name="android.intent.action.VIEW"/>
                <!-- 必须加上该项 -->
                <category android:name="android.intent.category.DEFAULT"/>
                <!-- 如果希望该应用可以通过浏览器的连接启动，则添加该项 -->
                <category android:name="android.intent.category.BROWSABLE"/>
            </intent-filter>
```

如下图所示。

![img](https:////upload-images.jianshu.io/upload_images/3611193-2e8082db35950ca7.png?imageMogr2/auto-orient/strip|imageView2/2/w/1036/format/webp)

am-start.png



```bash
adb shell
 am start -d "com.dmwdmw.android.wakeupappdemo://www.dmwdmw.com:8080?type=green&url=123&name=hello"
```

匹配的intent-filter



```xml
<intent-filter>
                <data
                        android:host="www.dmwdmw.com"
                        android:scheme="com.dmwdmw.android.wakeupappdemo"/>
                <!-- 必须加上该项,对一段数据执行的“标准”操作-->
                <action android:name="android.intent.action.VIEW"/>
                <!-- 必须加上该项 -->
                <category android:name="android.intent.category.DEFAULT"/>
                <!-- 如果希望该应用可以通过浏览器的连接启动，则添加该项 -->
                <category android:name="android.intent.category.BROWSABLE"/>
            </intent-filter>
```

如下图所示

![img](https:////upload-images.jianshu.io/upload_images/3611193-fabf58e37e8a25a4.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

intent-start-another.png

如果你能用浏览器加载如下所示的HTML，或者使用HTML查看器类似的应用打开这段HTML。然后点击页面上的超链接，也是可以打开我们的App的。



```xml
<!DOCTYPE html>
<html>
<head>
    <title>Android短信测试</title>
</head>
<body>
    <a href="com.dmw.android.wakeupappdemo:">启动主界面</a>
    <a href="com.dmw.android.wakeupappdemo://www.dmw.com:8080?type=red&url=111&name=红色">启动红色程序/a>
    <a href="com.dmw.android.wakeupappdemo://www.dmw.com:8080?type=yellow&name=黄色">启动黄色程序，url为空</a>
    <a href="com.dmw.android.wakeupappdemo://www.dmw.com:8080?type=green&url=111">启动绿色程序，name为空</a>
    
    <a href="com.dmwdmw.android.wakeupappdemo://www.dmwdmw.com:8080?type=green&url=111">启动绿色程序，name为空</a>
    
</body>
</html>
```

上面例子中的完整代码请移步 github [WakeUpAppDemo](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2Fhumanheima%2FWakeUpAppDemo) 。

### DeepLinkDispatch的使用

DeepLinkDispatch是一个简单的基于注解的库，可以更好的处理让Android平台上的deep link 跳转问题。

**使用** 添加依赖



```bash
implementation 'com.airbnb:deeplinkdispatch:3.1.1'
annotationProcessor 'com.airbnb:deeplinkdispatch-processor:3.1.1'
```

创建你自己的link module(s)。每一个用`DeepLinkModule`注解的类，DeepLinkDispatch都会生成一个对应的`Loader`类，`Loader`类里面包含所有你使用`@DeepLink`注解注册的信息。

这个是在项目中其他library中创建的一个LibraryDeepLinkModule类，会生成一个LibraryDeepLinkModuleLoader 类



```css
package com.airbnb.deeplinkdispatch.sample.library;

import com.airbnb.deeplinkdispatch.DeepLinkModule;

@DeepLinkModule
public class LibraryDeepLinkModule {
}
```

这是在主项目中创建的SampleModule类，会生成一个SampleModuleLoader类



```css
package com.airbnb.deeplinkdispatch.sample;

import com.airbnb.deeplinkdispatch.DeepLinkModule;

@DeepLinkModule
public class SampleModule {
}
```

然后创建一个DeepLinkActivity 用来转发所有的 deep link 跳转信息。并在`AndroidManifest.xml`文件中声明你想要处理的`scheme`



```java
//使用@DeepLinkHandler注解DeepLinkActivity ，并提供一个用@DeepLinkModule注解的类的列表。
@DeepLinkHandler({ SampleModule.class, LibraryDeepLinkModule.class })
public class DeepLinkActivity extends Activity {
  @Override protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    //DeepLinkDelegate，LibraryDeepLinkModuleLoader 和SampleModuleLoader都是编译期间生成的。
    DeepLinkDelegate deepLinkDelegate = new DeepLinkDelegate(
        new SampleModuleLoader(), new LibraryDeepLinkModuleLoader());
   //把 deep link 处理转发个DeepLinkDispatch。DeepLinkDispatch会根据传入的Intent URI 启动正确的 Activity
    deepLinkDelegate.dispatchFrom(this);
    //结束当前Activity 因为已经启动了正确的Activity了。
    finish();
  }
}
```

在`AndroidManifest.xml`文件中声明你想要处理的`scheme`信息。



```xml
<activity
    android:name="com.example.DeepLinkActivity"
    android:theme="@android:style/Theme.NoDisplay">
    <intent-filter>
        <action android:name="android.intent.action.VIEW" />
        <category android:name="android.intent.category.DEFAULT" />
        <category android:name="android.intent.category.BROWSABLE" />
        <data android:scheme="foo" />
    </intent-filter>
</activity>
```

注意：

1. 从v3版本开始，你需要自己声明一个用@DeepLinkHandler注解的类。（就是上面我们声明的DeepLinkActivity ）
2. 在`AndroidManifest.xml`文件中每个<intent-filter>下面只能包含一个data标签，如果是多个data标签的话，你需要声明多个<intent-filter>标签。如下所示。



```xml
<intent-filter>
    ...
    <data android:scheme="http"
          android:host="example.com"/>
</intent-filter>

<intent-filter>
    ...
    <data android:scheme="http"
          android:host="example.com"
          android:pathPrefix="/gizmos"/>
</intent-filter>
```



```java
@DeepLink({ "dld://classDeepLink", "http://example.com/foo{arg}", "dld://example.com/deepLink" })
public class MainActivity extends AppCompatActivity {

  ...
 @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        tvParams = findViewById(R.id.tvParams);
        Intent intent = getIntent();
        if (intent.getBooleanExtra(DeepLink.IS_DEEP_LINK, false)) {
            String message;
            Bundle parameters = intent.getExtras();
            Log.d(TAG, "Deeplink params: " + parameters);

            if (ACTION_DEEP_LINK_METHOD.equals(intent.getAction())) {
                message = "method with param1:" + parameters.getString("param1");
            } else if (ACTION_DEEP_LINK_COMPLEX.equals(intent.getAction())) {
                message = parameters.getString("arbitraryNumber");
            } else if (parameters.containsKey("arg")) {
                message = "class and found arg:" + parameters.getString("arg");
            } else {
                message = "class";
            }

            // You can pass a query parameter with the URI, and it's also in parameters, like
            // dld://classDeepLink?qp=123
            if (parameters.containsKey("qp")) {
                message += " with query parameter " + parameters.getString("qp");
            }
            Uri referrer = ActivityCompat.getReferrer(this);
            if (referrer != null) {
                message += " and referrer: " + referrer.toString();
            }
            tvParams.setText(message);
        }
    }
}
```

MainActivity 使用@DeepLink注解，表明只要scheme是注解中的任意一个，都可以打开MainActivity 。

然后我们使用下面的命令在Android studio 的终端启动App，能正确打开MainActivity ，并且我们可以在onCreate()方法中，获取Uri中的信息。



```cpp
adb shell
//对应注解的第一个值
am start -W -a android.intent.action.VIEW -d "dld:classDeepLink"

//对应注解第二个值
am start -W -a android.intent.action.VIEW -d "http://example.com/foodumingwei"

//对应注解第三个值，我们启动的时候也可以在后面加上要启动的app的包名。
am start -W -a android.intent.action.VIEW -d "dld://example.com/deepLink" com.airbnb.deeplinkdispatch.sample
```

注意上面第二种注解



```cpp
http://example.com/foo{arg}
```

用花括号括起来的是参数名，如果我们传递了相应的参数，就会添加到intent的extra中，我们就可以在Activity中获取我们传递的参数。



```bash
am start -W -a android.intent.action.VIEW -d "http://example.com/foodumingwei"
```

用上面的启动方式启动Activity以后，我们打印intent的 extras。



```dart
 if (intent.getBooleanExtra(DeepLink.IS_DEEP_LINK, false)) {
            String message;
            Bundle parameters = intent.getExtras();
            Log.d(TAG, "Deeplink params: " + parameters);
}
```



```csharp
Deeplink params: Bundle[{is_deep_link_flag=true, android.intent.extra.REFERRER=http://example.com/foodumingwei, arg=dumingwei, deep_link_uri=http://example.com/foodumingwei}]
```

可以看到传递的 arg的实参是`dumingwei`。

#### 方法注解

你可以使用`@DeepLink`注解任何`public static`的方法。DeepLinkDispatch 会调用这些方法来创建intent，使用它来启动你的`Activity`。



```java
    @DeepLink("dld://methodDeepLink/{param1}")
    public static Intent intentForDeepLinkMethod(Context context) {
        return new Intent(context, MainActivity.class).setAction(ACTION_DEEP_LINK_METHOD);
    }
```



```bash
am start -W -a android.intent.action.VIEW -d "dld://methodDeepLink/hello world"
```

当我们在终端输入上面的命令后，可以成功启动`MainActivity`。

如果你需要获取`Intent`中的extras，只需要为你的方法添加一个`Bundle`参数。例如：



```java
@DeepLink("dld://host/somePathOne/{arbitraryNumber}/otherPath")
    public static Intent intentForComplexMethod(Context context, Bundle bundle) {
        if (bundle != null && bundle.containsKey("qp")) {
            Log.d(TAG, "found new parameter :with query parameter :" + bundle.getString("qp"));
        }
        return new Intent(context, MainActivity.class).setAction(ACTION_DEEP_LINK_COMPLEX);
    }
```



```cpp
 am start -W -a android.intent.action.VIEW -d dld://host/somePathOne/123/otherPath?qp=dmw
```

当我们在终端输入上面的命令后，可以成功启动`MainActivity`。并且打印出了qp的实参`dmw`。



```css
MainActivity: found new parameter :with query parameter :dmw
```

如果你需要自定义Activity返回栈，那么方法可以返回一个TaskStackBuilder 。



```java
@DeepLink("http://example.com/deepLink/{id}/{name}/{place}")
    public static TaskStackBuilder intentForTaskStackBuilderMethods(Context context, Bundle bundle) {
        Log.d(TAG, "without query parameter :");
        if (bundle != null && bundle.containsKey("qp")) {
            Log.d(TAG, "found new parameter :with query parameter :" + bundle.getString("qp"));
        }
        Intent detailsIntent =
                new Intent(context, SecondActivity.class).setAction(ACTION_DEEP_LINK_COMPLEX);
        Intent parentIntent =
                new Intent(context, MainActivity.class).setAction(ACTION_DEEP_LINK_COMPLEX);
        TaskStackBuilder taskStackBuilder = TaskStackBuilder.create(context);
        taskStackBuilder.addNextIntent(parentIntent);
        taskStackBuilder.addNextIntent(detailsIntent);
        return taskStackBuilder;
    }
```



```cpp
am start -W -a android.intent.action.VIEW -d http://example.com/deepLink/2018/dumingwei/Shanghai?qp=dmw
```

当我们在终端输入上面的命令后，会先启动`SecondActivity`。点击返回键，会返回`MainActivity`。

#### 查询参数

查询参数会被自动解析和传递并且像其他参数一样可以被获取。例如我们可以获取在URI`foo://example.com/deepLink?qp=123`中传递的查询参数。



```java
@DeepLink("foo://example.com/deepLink")
public class MainActivity extends Activity {
  @Override protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    Intent intent = getIntent();
    if (intent.getBooleanExtra(DeepLink.IS_DEEP_LINK, false)) {
      Bundle parameters = intent.getExtras();
      if (parameters != null && parameters.getString("qp") != null) {
        String queryParameter = parameters.getString("qp");
        // Do something with the query parameter...
      }
    }
  }
}
```

传递实参`123`



```cpp
am start -W -a android.intent.action.VIEW -d foo://example.com/deepLink?qp=123
```

#### 回调

你可以选择性的注册一个广播接收器，每当有deep link 请求打开你的App的时候，广播接收器都会被调用。无论deep link 请求成功还是失败，DeepLinkDispatch 会使用`LocalBroadcastManager`来广播一个`Intent`。`Intent`会携带一些信息：

- `DeepLinkHandler.EXTRA_URI`：deep link的URI
- `DeepLinkHandler.EXTRA_SUCCESSFUL`：deep link 请求是否被成功处理。
- `DeepLinkHandler.EXTRA_ERROR_MESSAGE`：如果 deep link 处理失败时候的错误信息。



```java
public class DeepLinkReceiver extends BroadcastReceiver {
    private static final String TAG = DeepLinkReceiver.class.getSimpleName();

    @Override
    public void onReceive(Context context, Intent intent) {
        String deepLinkUri = intent.getStringExtra(DeepLinkHandler.EXTRA_URI);

        if (intent.getBooleanExtra(DeepLinkHandler.EXTRA_SUCCESSFUL, false)) {
            Log.i(TAG, "Success deep linking: " + deepLinkUri);
        } else {
            String errorMessage = intent.getStringExtra(DeepLinkHandler.EXTRA_ERROR_MESSAGE);
            Log.e(TAG, "Error deep linking: " + deepLinkUri + " with error message +" + errorMessage);
        }
    }
}
```

然后在Application中注册广播接收器。



```java
public class SampleApplication extends Application {
  @Override public void onCreate() {
    super.onCreate();
    IntentFilter intentFilter = new IntentFilter(DeepLinkHandler.ACTION);
    LocalBroadcastManager.getInstance(this).registerReceiver(new DeepLinkReceiver(), intentFilter);
  }
}
```

#### 自定义注解

你可以创建自定义注解提供通用的前缀，可以减少 deep links的重复。这些前缀会自动应用到每个使用自定义注解注解的类和方法。一个流行的使用场景是web和app的之间的deep link 请求。



```csharp
// 为每一个deep link URI 添加一个 "app://airbnb"前缀。
@DeepLinkSpec(prefix = { "app://airbnb" })
public @interface AppDeepLink {
  String[] value();
}
```



```kotlin
//  为所有的web deep links 添加 "http://airbnb.com" and "https://airbnb.com"前缀
@DeepLinkSpec(prefix = { "http://airbnb.com", "https://airbnb.com" })
@Retention(RetentionPolicy.CLASS)
public @interface WebDeepLink {
  String[] value();
}
```



```java
@AppDeepLink({"/view_users"})
@WebDeepLink({"/users", "/user/{id}"})
public class CustomPrefixesActivity extends AppCompatActivity {

    private static final String TAG = CustomPrefixesActivity.class.getSimpleName();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        if (getIntent().getBooleanExtra(DeepLink.IS_DEEP_LINK, false)) {
            Bundle parameters = getIntent().getExtras();
            Log.d(TAG, "Deeplink params: " + parameters);
    }
}
```

如上所示，上面的Activity能够处理的deep link：



```bash
// "app://airbnb/view_users"
// "http://airbnb.com/users"
// "http://airbnb.com/user/{id}"
// "https://airbnb.com/users"
// "https://airbnb.com/user/{id}"
```

我们在控制台试一试。



```cpp
am start -W -a android.intent.action.VIEW -d app://airbnb/view_users

 am start -W -a android.intent.action.VIEW -d http://airbnb.com/users

am start -W -a android.intent.action.VIEW -d https://airbnb.com/users

 am start -W -a android.intent.action.VIEW -d http://airbnb.com/user/123

 am start -W -a android.intent.action.VIEW -d https://airbnb.com/user/123
```

都可以成功启动App。

我们在方法上试一试自定义注解



```java
@AppDeepLink("/method/view_users")
    public static Intent intentForCustomDeepLinkMethod(Context context) {
        return new Intent(context, SecondActivity.class).setAction(ACTION_DEEP_LINK_METHOD);
}
```

在控制台启动，可以正常启动`Activity`。



```cpp
am start -W -a android.intent.action.VIEW -d app://airbnb/method/view_users
```

结束。

参考链接:

1. [DeepLinkDispatch](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2Fairbnb%2FDeepLinkDispatch)
2. [Android 短信链接跳浏览器打开APP](https://www.jianshu.com/p/2ace28e1b616)
3. [Deep Link是什么](https://www.jianshu.com/p/964f871dc716)
4. [Android 跨应用间调用: URL Scheme](https://www.jianshu.com/p/7b09cbac1df4)



作者：leilifengxingmw
链接：https://www.jianshu.com/p/c4db84d6b13c
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



- https://www.jianshu.com/p/7b09cbac1df4
- https://www.jianshu.com/p/562be216d8a5
- https://www.jianshu.com/p/1c63bbd75554
- https://blog.csdn.net/yuzhengfei7/article/details/93468914

