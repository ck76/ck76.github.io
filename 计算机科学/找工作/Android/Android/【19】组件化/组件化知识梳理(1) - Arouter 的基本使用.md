[TOC]

# 组件化知识梳理目录

[组件化知识梳理(1) - Arouter 的基本使用](https://www.jianshu.com/p/5c109c51d7ba)
[组件化知识梳理(2) - Arouter 源码分析之 Complier SDK](https://www.jianshu.com/p/a1f6db686b17)
[组件化知识梳理(3) - Arouter 源码分析之运行时 SDK](https://www.jianshu.com/p/a662c0b8edd2)

# 一、前言

放假几天在家看了些关于组件化的文章，目前市面上已经有许多开源的组件化框架，大家可以看一下这篇文章 [总结一波组件化的实现方案优缺点](https://link.jianshu.com/?t=https%3A%2F%2Fjuejin.im%2Fentry%2F5aa1240b6fb9a028dc409a34)，里面对目前的组件化框架做了基本的介绍。

今天，我们就以阿里巴巴开源的组件化框架`Arouter`为例，先对组件化有一个基本的了解，主要看的是下面的官方文档和基本思想的介绍。

- `Github`地址：[https://github.com/alibaba/ARouter](https://link.jianshu.com/?t=https%3A%2F%2Fgithub.com%2Falibaba%2FARouter)
- `Arouter`基本思想：[开源最佳实践：Android 平台页面路由框架 ARouter](https://link.jianshu.com/?t=https%3A%2F%2Fyq.aliyun.com%2Farticles%2F71687%3Ft%3Dt1)

本文中的所有代码都可以通过 [ArouterDemo](https://link.jianshu.com/?t=https%3A%2F%2Fgithub.com%2FimZeJun%2FArouterDemo) 下载并调试。

# 二、实践

## 2.1 导入依赖包

在使用`Arouter`的时候，需要导入两个`SDK`，分别针对运行时和编译时：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymhch36jj30dh0143ye.jpg)

运行时依赖 & 编译时依赖


不同的组件类型，需要导入不同的依赖包，一般来说，一个最简单的组件化框架由以下几部分构成，由上至下可以分为四个部分，其中需要导入`Arouter`依赖的有两种类型的组件：



- 业务组件：导入编译时的依赖。
- 路由基础组件：导入运行时的依赖，还需要导入编译时的依赖。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymha0e1pj30ml0iomyc.jpg)

基本的组件化框架

实际的项目结构：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymh8o6apj309p04z74e.jpg)

实际的项目结构

### 2.1.1 统一各依赖库版本

在这之前，我们需要做一些前期准备。由于组件化之后，项目中会存在多个`Android Library`，为了保证各`Library`中`compileSdkVersion`、`buildToolsVersion`，以及第三方库版本的统一性，我们最好在同一个地方声明这些信息，然后在各`module`的`build.gradle`文件中引入这些变量，步骤如下：

- 第一步：在 **工程的根目录** 下创建`config.gradle`文件，该文件中声明各版本的信息：



```java
ext {
    android = [
            compileSdkVersion : 25,
            buildToolsVersion : "25.0.2",
            minSdkVersion : 14,
            targetSdkVersion : 25
    ]
    dependencies = [
            "support-v4"  : 'com.android.support:support-v4:25.3.1',
            "support-v7"  : 'com.android.support:appcompat-v7:25.3.1',
            "junit" : 'junit:junit:4.12',
            "arouter-api" : 'com.alibaba:arouter-api:1.2.4',
            "arouter-compiler" : 'com.alibaba:arouter-compiler:1.1.4',
            "event-bus" : 'org.simple:androideventbus:1.0.5.1',
            "fastjson" : 'com.alibaba:fastjson:1.2.9'
    ]
}
```

- 第二步：在 **工程的根目录** 的`build.gradle`的最上面导入该文件：



```java
//统一依赖库版本。
apply from : "config.gradle"
```

- 第三步：在 **各模块** 中引入第一步声明的变量，我们以`App`壳工程为例看一下，`SdkVersion`和依赖包需要通过不同的方式导入：



```java
apply plugin: 'com.android.application'
//1. 定义 config 变量
def config = rootProject.ext

android {
    //2. SdkVersion 的导入方式，直接在各关键字后面加上定义的变量名
    compileSdkVersion config.android.compileSdkVersion
    buildToolsVersion config.android.buildToolsVersion
    defaultConfig {
        applicationId "com.lizejun.demo.arouterdemo"
        minSdkVersion config.android.minSdkVersion
        targetSdkVersion config.android.targetSdkVersion
        versionCode 1
        versionName "1.0"
        testInstrumentationRunner "android.support.test.runner.AndroidJUnitRunner"
    }
    buildTypes {
        release {
            minifyEnabled false
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
        }
    }
}

dependencies {
    compile fileTree(dir: 'libs', include: ['*.jar'])
    androidTestCompile('com.android.support.test.espresso:espresso-core:2.2.2', {
        exclude group: 'com.android.support', module: 'support-annotations'
    })
    //3. 依赖包的引入方式，在关键字后面加上 config.dependencies，最后用括号包裹定义的变量名。
    compile config.dependencies["support-v7"]
    testCompile config.dependencies["junit"]

    if (isNeedHomeModule.toBoolean()) {
        compile project(":module-home")
    }
    if (isNeedOtherModule.toBoolean()) {
        compile project(":module-other")
    }
    if (isNeedStoreModule.toBoolean()) {
        compile project(":module-store")
    }
}
```

### 2.1.2 各项目引入的依赖

下面我们来看一下各个组件是如何引入`Arouter`的依赖。

#### (1) App 壳

在`App`壳中不需要引入`SDK`的依赖，只需要引入各 **业务组件** 即可。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymh6qrc9j30lp09gab4.jpg)

App 壳



#### (2) 业务组件

在业务组件中，除了需要引入`Arouter`的 **编译期** 依赖库，以及 **路由基础组件**，还需要在`android`界面下进行`javaCompileOptions`的声明。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymh5c40gj30nx0lyaci.jpg)

业务组件



#### (3) 路由基础组件

在路由基础组件中，需要同时引入`Arouter`的 **编译期 & 运行时** 依赖库，也需要在`android`界面下进行`javaCompileOptions`的声明，对于 **基础组件的依赖** 也在这里面声明，这样 **业务组件** 通过依赖 **路由基础组件**，也可以间接地调用 **基础组件** 中的功能。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymh3lndyj30pz0k0q5b.jpg)

路由基础组件



#### (4) 基础组件

基础组件不需要引入`Arouter`相关的依赖，只需要负责实现好与业务无关的功能就好。

## 2.2 如何让业务组件独立运行

组件化最重要的目的就是需要 **保证各业务组件能够独立调试 & 运行**，下面我们就来介绍一下如何实现，在我们的项目中有三个业务组件，`module-main`、`module-other`和`module-store`，以`module-main`为例看一下如何实现：

- 第一步：在 **工程根目录** 下的`gradle.properties`下声明对应`module`的属性，该属性为`false`时表示其可以 **独立运行**，不依赖于`App`壳：



```java
isNeedHomeModule = true
```

- 第二步：在`module-main`的`build.gradle`文件中加上红框内的三个部分：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymh2kvoqj30mp0mj40n.jpg)

module-main 的 build.gradle 文件

这么做的 **原理** 是：当组件需要 **独立运行**（`isNeedHomeModule`标志位为`false`）的时候，需要保证以下三点：

- 需要被声明为`com.android.application`。
- 需要指定`applicationId`。
- 需要为其在`AndroidManifest.xml`中指定一个默认启动的`Activity`，我们通过`sourceSets`的方式，为其在这两种情况下指定不同的`AndroidManifest.xml`文件，这样就不用每次都去修改了，但是要记得，如果在其中一个`AndroidManifest.xml`增加了声明，那么在对应的文件中也需要加上。

其在`project`视图下的位置如下所示：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymh14vv8j309r071q33.jpg)

project 视图下的位置



- 作为`Library`时，`/src/main`下的`AndroidManifest.xml`文件：



```xml
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="com.demo.lizejun.module.home">

    <application
        android:allowBackup="true"
        android:label="@string/app_name"
        android:supportsRtl="true">
        <activity android:name="com.lizejun.demo.module.home.ResultClientActivity"/>
    </application>

</manifest>
```

- 作为独立运行的模块时，`/src/debug`下的`AndroidManifest.xml`文件：



```xml
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="com.demo.lizejun.module.home">

    <application
        android:allowBackup="true"
        android:label="@string/app_name"
        android:supportsRtl="true"
        android:theme="@style/Theme.AppCompat">
        <activity android:name="com.lizejun.demo.module.home.HomeActivity">
            <intent-filter>
                <action android:name="android.intent.action.MAIN" />
                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
        </activity>
        <activity android:name="com.lizejun.demo.module.home.ResultClientActivity"/>
    </application>

</manifest>
```

当需要独立运行一个组件的时候，将`gradle.properties`中的`isNeedHomeModule`改为`false`，然后`sync`一下工程，在最上面选择`module-home`，点击绿色箭头运行即可：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymgz8dp4j307201odfo.jpg)

选择 module-home 运行



## 2.3 Arouter 的使用场景

经过了前面两步，整个组件化的框架就搭建起来了，下面再来介绍一下`Arouter`的使用场景。

### 2.3.1 简单的跨模块 Activity 跳转

对跳转的 **目标界面** 用`@Route`进行注解，该目标界面在另一个业务组件`module-other`当中，`RouterMap.NO_RESULT_ACTIVITY`是在路由基础组件`lib-base`中定义的常量，其基本思想为：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymgxxxckj30i20a8mxh.jpg)

页面跳转基本思想





```java
public class RouterMap {

    public static final String HOME_FRAGMENT = "/home/main";
    public static final String NO_RESULT_ACTIVITY = "/other/no_result";
    public static final String RESULT_SERVER_ACTIVITY = "/other/result_server";
    public static final String EVENT_BUS_ACTIVITY = "/other/event_bus";
    public static final String INTERCEPT_GROUP = "intercept_group";
    public static final String INTER_MIDDLE_ACTIVITY = "/other/inter_middle";
    public static final String INTER_TARGET_ACTIVITY = "/other/inter_target";
    public static final String STORE_MODULE_SERVICE = "/store/service";
    public static final String INJECT_ACTIVITY = "/other/inject";
    public static final String JSON_SERVICE = "/base/json_service";

}
```

对`module-other`中的目标界面，使用`@Route`注解进行声明：



```java
@Route(path = RouterMap.NO_RESULT_ACTIVITY)
public class NoResultActivity extends AppCompatActivity {

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Utils.toast(this, "NoResultActivity onCreate");
    }
}
```

在`module-main`中通过`ARouter`提供的方法进行跨模块的跳转，`build`的参数就是`@Route`所指定的名称：



```java
ARouter.getInstance().build(RouterMap.NO_RESULT_ACTIVITY).navigation();
```

## 2.3.2 带 ActivityResult 的跨模块 Activity 跳转

当需要获得目标界面的返回结果时，调用模块需要在`navigation()`方法调用时，声明额外的`ConstantMap.FOR_RESULT_CODE`作为`reququestCode`，这些常量和`RouterMap`类似，都放在路由基础模块`lib-base`中进行声明，方便其它的模块访问：



```java
public class ResultClientActivity extends AppCompatActivity {

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_result_client);
        Button button = (Button) findViewById(R.id.bt_result_client);
        button.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                ARouter.getInstance().build(RouterMap.RESULT_SERVER_ACTIVITY).navigation(ResultClientActivity.this, ConstantMap.FOR_RESULT_CODE);
            }
        });
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        switch (requestCode) {
            case ConstantMap.FOR_RESULT_CODE:
                Utils.toast(ResultClientActivity.this, "receive=" + data.getStringExtra(ConstantMap.FOR_RESULT_KEY));
                break;
            default:
                break;
        }
    }
}
```

跳转目标模块通过`setResult`方法回传数据：



```java
@Route(path = RouterMap.RESULT_SERVER_ACTIVITY)
public class ResultServerActivity extends AppCompatActivity {

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_result_server);
        Button button = (Button) findViewById(R.id.bt_for_result);
        button.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                Intent intent = new Intent();
                intent.putExtra(ConstantMap.FOR_RESULT_KEY, "返回数据给 Client");
                setResult(ConstantMap.FOR_RESULT_CODE, intent);
                finish();
            }
        });
    }

}
```

## 2.3.3 在 Fragment 中接收返回结果

`2.3.2`中的接收返回值的方式，只适用于在`Activity`中启动，并通过`onActivityResult`接收返回结果，如果希望在`Fragment`中启动目标界面，并接收返回结果，那么需要使用`EventBus`作为桥梁，在调用模块的`Fragment`中对`EventBus`进行注册，并通过`@Subscriber`声明回调的方法：



```java
    @Override
    public void onAttach(Context context) {
        super.onAttach(context);
        EventBus.getDefault().register(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        EventBus.getDefault().unregister(this);
    }

    @Subscriber(tag = ConstantMap.EVENT_BUS_KEY)
    public void onEvent(String s) {
        Utils.toast(getContext(), s);
    }
```

调用模块的启动方法和前面类似：



```java
ARouter.getInstance().build(RouterMap.EVENT_BUS_ACTIVITY)
    .withInt(ConstantMap.EVENT_BUS_DATA, 1000)
    .navigation();
```

在目标模块中，通过`EventBus`发送数据：



```java
@Route(path = RouterMap.EVENT_BUS_ACTIVITY)
public class EventBusActivity extends AppCompatActivity {

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_event_bus);
        final int revValue = getIntent().getIntExtra(ConstantMap.EVENT_BUS_DATA, 0);
        Button button = (Button) findViewById(R.id.bt_event_bus);
        button.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                EventBus.getDefault().post("通过 EventBus 返回数据=" + revValue, ConstantMap.EVENT_BUS_KEY);
                finish();
            }
        });
    }
}
```

## 2.3.4 服务声明

除了跳转以外，我们有时候还需要在一个业务组件中，用到其它业务组件提供的功能，那么这时候就需要用到 **服务声明**，其基本思想如下：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymguorl6j30i907xaaa.jpg)

服务声明的基本思想



- 在路由基础框架中声明服务的接口，需要继承于`IProvider`接口



```java
public interface IStoreModuleService extends IProvider {

    /**
     * 获取是否登录的状态。
     * @return 是否登录。
     */
    boolean isLogin();

    /**
     * 设置是否登录。
     * @param login 是否登录。
     */
    void setLogin(boolean login);
}
```

- 在提供服务的业务模块`module-store`，实现在路由基础框架中声明的接口，并使用`@Route`注解



```java
@Route(path = RouterMap.STORE_MODULE_SERVICE)
public class StoreModuleServiceImpl implements IStoreModuleService {

    private static final String FILE = "account";
    private static final String IS_LOGIN = "is_login";


    private Context mContext;

    @Override
    public void init(Context context) {
        mContext = context;
    }

    @Override
    public boolean isLogin() {
        return mContext.getSharedPreferences(FILE, Context.MODE_PRIVATE).getBoolean(IS_LOGIN, false);
    }

    @Override
    public void setLogin(boolean login) {
        mContext.getSharedPreferences(FILE, Context.MODE_PRIVATE).edit().putBoolean(IS_LOGIN, login).apply();
    }
}
```

- 在路由基础组件中，提供静态方法给业务组件调用：



```java
public class StoreModuleRouterService {

    public static boolean isLogin() {
        IStoreModuleService chatModuleService = (IStoreModuleService) ARouter.getInstance().build(RouterMap.STORE_MODULE_SERVICE).navigation();
        return chatModuleService != null && chatModuleService.isLogin();
    }

    public static void setLogin(boolean login) {
        IStoreModuleService chatModuleService = (IStoreModuleService) ARouter.getInstance().build(RouterMap.STORE_MODULE_SERVICE).navigation();
        if (chatModuleService != null) {
            chatModuleService.setLogin(login);
        }
    }
}
```

- `module-store`外的业务组件要使用到它所提供的服务时，调用`StoreModuleRouterService`提供的静态方法即可。

### 2.3.5 获取 Fragment

为要发现的`Fragment`添加注解：



```java
@Route(path = RouterMap.HOME_FRAGMENT)
public class HomeFragment extends Fragment {
    //....
}
```

通过下面的方法，获得`Fragment`，并添加到布局当中。



```java
public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        addFragment();
    }

    private void addFragment() {
        Fragment homeFragment = getHomeFragment();
        FragmentUtils.addFragment(this, homeFragment, R.id.fl_container);
    }

    private Fragment getHomeFragment() {
        return (Fragment) ARouter.getInstance().build(RouterMap.HOME_FRAGMENT).navigation();
    }

}
```

### 2.3.6 拦截器

拦截器用于同一管理页面之间的跳转，其基本思想如下图所示，例如`module-main`要启动`module-other`中的`B`界面，但是`B`界面要求先登录，那么我们就可以在拦截器中判断用户是否登录，如果登录了，那么可以跳转到`B`界面，否则就先跳转到`A`界面进行登录。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymgsfq1ej30k10c274r.jpg)

拦截器的基本思想


其基本的实现方式为：



- `A`界面用于登录：



```java
@Route(path = RouterMap.INTER_MIDDLE_ACTIVITY)
public class InterMiddleActivity extends AppCompatActivity {

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_inter_middle);
        updateStatus();
        switchStatus();
    }

    private void updateStatus() {
        TextView tvStatus = (TextView) findViewById(R.id.tv_login_status);
        tvStatus.setText("登录状态=" + StoreModuleRouterService.isLogin());
    }

    private void switchStatus() {
        final Switch switchStatus = (Switch) findViewById(R.id.sw_login);
        switchStatus.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {

            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                StoreModuleRouterService.setLogin(isChecked);
                updateStatus();
            }
        });
    }
}
```

- `B`表示需要在登录状态下才能跳转的界面，注意这里我们在`@Route`注解中，新增了一个`extras`属性，它表示该界面是需要登录的，我们在拦截器中会用到它。



```java
@Route(path = RouterMap.INTER_TARGET_ACTIVITY, extras = ConstantMap.LOGIN_EXTRA)
public class InterTargetActivity extends AppCompatActivity {


}
```

- 在路由基础组件中定义拦截器，在拦截器中，我们通过`Postcard`的`getExtra`方法来获得目标界面`@Route`中声明的`extras`属性，该属性是一个`int`类型，我们可以根据拦截的需求，在路由基础组件中定义不同的常量。



```java
@Interceptor(priority = 1, name = "重新分组进行拦截")
public class BaseInterceptor implements IInterceptor {

    @Override
    public void process(Postcard postcard, InterceptorCallback callback) {
        //getExtra() 对应目标 Activity 中的 @Route 声明。
        if (postcard.getExtra() == ConstantMap.LOGIN_EXTRA) {
            //判断是否登录。
            boolean isLogin = postcard.getExtras().getBoolean(ConstantMap.IS_LOGIN);
            if (!isLogin) {
                //如果没有登录，那么跳转到登录界面。
                ARouter.getInstance().build(RouterMap.INTER_MIDDLE_ACTIVITY).navigation();
            } else {
                //否则继续放行。
                postcard.withString(ConstantMap.IS_LOGIN_EXTRA, "登录了!");
                callback.onContinue(postcard);
            }
        } else {
            //对于其他不需要登录的界面直接放行。
            callback.onContinue(postcard);
        }
    }

    @Override
    public void init(Context context) {

    }
}
```

### 2.3.7 依赖注入

当我们通过`navigation`方法启动目标页面的时候，会通过`withXXX`方法传递额外的数据，例如下面这样：



```java
SerialBean bean = new SerialBean();
bean.setName("SerialBean");
bean.setAge(18);
ARouter.getInstance().build(RouterMap.INJECT_ACTIVITY)
    .withInt(ConstantMap.INJECT_AGE, 18)
    .withObject(ConstantMap.INJECT_OBJECT, bean)
    .navigation();
```

这些数据最终会放在调起目标页面的`intent`当中。在目标页面里，可以通过`getIntent()`方法来取出里面的值，当然，还有更简便的方法，就是使用 **依赖注入** 的方式，使得 **目标界面中的成员变量可以自动被赋值**，被赋值的变量需要使用`@AutoWired`注解来修饰，在目标界面中，一定要加上下面这句：



```java
ARouter.getInstance().inject(this);
```

目标界面的代码如下：



```java
@Route(path = RouterMap.INJECT_ACTIVITY)
public class InjectActivity extends AppCompatActivity {
    
    //使用 AutoWired 注解。
    @Autowired(name = ConstantMap.INJECT_AGE)
    int age;

    //使用 AutoWired 注解。
    @Autowired(name = ConstantMap.INJECT_OBJECT)
    SerialBean bean;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //需要额外调用这句话来进行依赖注入。
        ARouter.getInstance().inject(this);
        Utils.toast(this, "age=" + age + ",bean.age=" + bean.getAge() + ",bean.name=" + bean.getName());
    }
}
```

如果我们传递的数据中包含了`Object`，那么还需要在路由基础组件中定义序列化的服务，例如上面的`SerialBean`，服务器的定义如下：



```java
@Route(path = RouterMap.JSON_SERVICE, name = "序列化")
public class JsonServiceImpl implements SerializationService {

    @Override
    public void init(Context context) {

    }

    @Override
    public <T> T json2Object(String text, Class<T> clazz) {
        return JSON.parseObject(text, clazz);
    }

    @Override
    public String object2Json(Object instance) {
        return JSON.toJSONString(instance);
    }

    @Override
    public <T> T parseObject(String input, Type clazz) {
        return JSON.parseObject(input, clazz);
    }
}
```

# 三、小结

以上就是`Arouter`的基本使用方式，完整的工程可以查看 [ArouterDemo](https://link.jianshu.com/?t=https%3A%2F%2Fgithub.com%2FimZeJun%2FArouterDemo)。

哎，每天都感觉要学的东西太多，搞着搞着又凌晨一点多了，看着`To do List`，[待学习 Android 知识点](https://link.jianshu.com/?t=http%3A%2F%2Fnote.youdao.com%2Fnoteshare%3Fid%3Dd82cb8510f944d60ac30c42410c62b02)，陷入了茫茫的焦虑当中。



作者：泽毛
链接：https://www.jianshu.com/p/5c109c51d7ba
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。