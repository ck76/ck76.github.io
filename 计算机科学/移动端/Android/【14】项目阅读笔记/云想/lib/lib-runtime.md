[TOC]

### 一、模块结构

```java
.
├── AppRuntime.java						//App运行环境
├── AppRuntimeInit.java					//运行环境初始化类
└── base
    ├── BackHandlerHelper.java			//Fragment处理返回键工具类
    ├── FragmentBackHandler.java		//ragment处理Back事件(接口)
    ├── activity
    │   ├── AbsActivity.java			//Activity 基类
    │   └── AbsBaseActivity.java		//Activity抽象类
    └── fragment
        └── AbsFragment.java			//
```



### 二、App

#### 1、AppRuntimeInit

```java
public class AppRuntimeInit {
    /**
     * 设置运行环境
     */
    public static void onApplicationAttachBaseContext(Application application, boolean debug) {
        AppRuntime.sApplication = application;
        AppRuntime.sDebug = debug;
    }
}
```

#### 2、AppRuntime

```java
public class AppRuntime {

    /** Application */
    static Application sApplication;
    /** Debug标识 */
    static boolean sDebug;

    //get/set方法
}
```

#### 3、App.java进行初始化

```java
	@Override
    protected void attachBaseContext(Context base) {
        super.attachBaseContext(base);
        AppRuntimeInit.onApplicationAttachBaseContext(this, BuildConfig.DEBUG);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        initHttpClient();
        initScheme();
        initRuntime();
    }
```



### 三、BackHandler

#### 1、BackHandlerHelper.java

Fragment处理返回键工具类

```java
/**
 * Fragment处理返回键工具类
 */
public class BackHandlerHelper {
    /**
     * 将back事件分发给 FragmentManager 中管理的子Fragment，
     * 如果该 FragmentManager 中的所有Fragment都
     * 没有处理back事件，则尝试 FragmentManager.popBackStack()
     *
     * @return 如果处理了back键则返回 true
     * @see #handleBackPress(Fragment)
     * @see #handleBackPress(FragmentActivity)
     */
    public static boolean handleBackPress(FragmentManager fragmentManager) {
        List<Fragment> fragments = fragmentManager.getFragments();
        if (fragments == null) {
            return false;
        }
        //遍历FragmentManager下的所有子fragment
        for (int i = fragments.size() - 1; i >= 0; i--) {
            Fragment child = fragments.get(i);
            if (isFragmentBackHandled(child)) {
                return true;
            }
        }
        //如果上面的不处理就交给fragmentmanager的返回栈
        if (fragmentManager.getBackStackEntryCount() > 0) {
            fragmentManager.popBackStack();
            return true;
        }
        return false;
    }
	
    public static boolean handleBackPress(Fragment fragment) {
        return handleBackPress(fragment.getChildFragmentManager());
    }

    public static boolean handleBackPress(FragmentActivity fragmentActivity) {
        return handleBackPress(fragmentActivity.getSupportFragmentManager());
    }

    /**
     * 判断Fragment是否处理了Back键
     *
     * @return 如果处理了back键则返回 <b>true</b>
     */
    private static boolean isFragmentBackHandled(Fragment fragment) {
        return fragment != null
                && fragment.isVisible()
                && fragment.getUserVisibleHint() //for ViewPager
                && fragment instanceof FragmentBackHandler
                && ((FragmentBackHandler) fragment).onBackPressed();
    }
}
```



#### 2、FragmentBackHandler.java

ragment处理Back事件(接口)，由AbsFragment实现

```java
public interface FragmentBackHandler {
    /**
     * 返回键被按下
     *
     * @return true: 消费返回键
     */
    boolean onBackPressed();
}
```

#### 3、AbsFragment

```java
	//子类按需重写
	@Override
    public boolean onBackPressed() {
        return false;
    }
```

#### 4、AbsBaseActivity

```java
	//如果Fragment没有重写就自己处理
	@Override
    public void onBackPressed() {
        if (!isFragmentHandleBackPressed()) {
            super.onBackPressed();
        }
    }

    /**
     * Fragment是否处理了返回键
     */
    protected boolean isFragmentHandleBackPressed() {
        return BackHandlerHelper.handleBackPress(this);
    }
```

#### 5、FragmentActivity是什么

- Fragment是Android 3.0以后的东西，为了在低版本中使用Fragment就要用到android-support-v4.jar兼容包,而FragmentActivity就是这个兼容包里面的，它提供了操作Fragment的一些方法，其功能跟3.0及以后的版本的Activity的功能一样。
  下面是API中的原话：

- FragmentActivity is a special activity provided in the Support Library to handle fragments on system versions older than API level 11. If the lowest system version you support is API level 11 or higher, then you can use a regular Activity.

主要区别如下：
1、FragmentActivity 继承自Activity，用来解决Android 3.0之前无法使用Fragment的问题，所以在使用的时候需要导入android-support-v4.jar兼容包，同时继承 FragmentActivity，这样在Activity中就能嵌入Fragment来实现你想要的布局效果。 
2、当然Android 3.0之后你就可以直接继承自Activity，并且在其中嵌入使用Fragment。 
3、获得FragmentManager的方式也不同 
Android 3.0以下：getSupportFragmentManager() 
Android 3.0以上：getFragmentManager()