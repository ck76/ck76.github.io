Taskgo是洪宇大佬在工作室的遗留项目

[TaskgoのGitHub](https://github.com/fhyPayaso/TaskGo-Android)

## 1、CountdownTimer使用

**基本使用方法**

```java
new CountDownTimer(10000, 1000) {

    public void onTick(long millisUntilFinished) {
        LogUtil.i(TAG, "seconds remaining: " + millisUntilFinished / 1000);
    }

    public void onFinish() {
        LogUtil.i(TAG, "done!");
    }
}.start();
```

**构造函数**

```java
CountDownTimer (long millisInFuture, long countDownInterval)
```

- 参数1: 你要倒计时的总时间, 单位ms.
- 参数2: 你要倒计时的间隔时间, 单位ms.

**方法**

```java
public final void cancel ()

public abstract void onFinish ()

public abstract void onTick (long millisUntilFinished)

public final CountDownTimer start ()
```

- 方法1`cancel()`: 取消当前的任务
- 方法2`onFinish()`: 当前任务完成的时候回调
- 方法3`onTick(long millisUntilFinished)`: 当前任务每完成一次倒计时间隔时间时回调
- 方法4`start()`: 开始当前的任务



## 2、根据不同Type动态创建Fragment与Tablayout

在Activity中根据Type初始化不同的FragmentList传入

- MyTaskListActivity.java

```java
 /**
     * 根据taskType初始化不同的view开头的地方
     */
    @Override
    protected void initView() {
        switch (mTaskType) {
            case MyTaskConfig.MY_ACCEPT_TASK:
                initAcceptView();
                break;
            case MyTaskConfig.MY_RELEASE_TASK:
                initReleaseView();
                break;
            default:
                break;
        }
        MyTaskPagerAdapter myTaskPagerAdapter = new MyTaskPagerAdapter(getSupportFragmentManager(), mTaskType, mFragmentList);
        mVpMyTask.setAdapter(myTaskPagerAdapter);
        mTabMyTask.setupWithViewPager(mVpMyTask);
    }

    private void initAcceptView() {
        setActivityTitle("我接受的任务");
        mVpMyTask.setOffscreenPageLimit(2);
        mFragmentList.add(MyTaskListFragment.getNewInstance(MyTaskConfig.MY_ACCEPT_NOT_FINISHED));
        mFragmentList.add(MyTaskListFragment.getNewInstance(MyTaskConfig.MY_ACCEPT_HAS_FINISHED));
    }

    private void initReleaseView() {
        setActivityTitle("我发布的任务");
        mVpMyTask.setOffscreenPageLimit(3);
        mFragmentList.add(MyTaskListFragment.getNewInstance(MyTaskConfig.MY_RELEASE_NOT_ACCEPTED));
        mFragmentList.add(MyTaskListFragment.getNewInstance(MyTaskConfig.MY_RELEASE_HAS_ACCEPTED));
        mFragmentList.add(MyTaskListFragment.getNewInstance(MyTaskConfig.MY_RELEASE_HAS_FINISHED));
    }
```

- Adapter中适配

```java
public class MyTaskPagerAdapter extends FragmentPagerAdapter {


    private String[] mReleaseTitles = {"未接受", "被接受", "已完成"};

    private String[] mAcceptTitles = {"未完成", "已完成"};

    private String mTaskType;

    private List<MyTaskListFragment> mFragmentList;


    public MyTaskPagerAdapter(FragmentManager fm, String taskType, List<MyTaskListFragment> fragmentList) {
        super(fm);
        this.mTaskType = taskType;
        this.mFragmentList = fragmentList;
    }

    @Override
    public Fragment getItem(int position) {
        return mFragmentList.get(position);
    }

    @Override
    public CharSequence getPageTitle(int position) {
        if (MyTaskConfig.MY_ACCEPT_TASK.equals(mTaskType)) {
            return mAcceptTitles[position];
        } else {
            return mReleaseTitles[position];
        }
    }

    @Override
    public int getCount() {
        return MyTaskConfig.MY_ACCEPT_TASK.equals(mTaskType) ? 2 : 3;
    }
}
```



## 3、Activity生命周期管理

```java
 @Override
    public void onCreate() {
        super.onCreate();
        sInstance = this;
        registerActivityLifecycleCallbacks(getActivityLifecycleCallbacks());//这里
```

写一个getXXX方法，返回匿名类

```java
 private static List<Activity> sActivityList = new ArrayList<>();

 private ActivityLifecycleCallbacks getActivityLifecycleCallbacks() {

        return new ActivityLifecycleCallbacks() {
            @Override
            public void onActivityCreated(Activity activity, Bundle savedInstanceState) {
                sActivityList.add(activity);
            }

            @Override
            public void onActivityStarted(Activity activity) {

            }

            @Override
            public void onActivityResumed(Activity activity) {

            }

            @Override
            public void onActivityPaused(Activity activity) {

            }

            @Override
            public void onActivityStopped(Activity activity) {

            }

            @Override
            public void onActivitySaveInstanceState(Activity activity, Bundle outState) {

            }

            @Override
            public void onActivityDestroyed(Activity activity) {
                sActivityList.remove(activity);
            }
        };
    }
```



## 4、退出应用

```java
 /**
     * 移除Activity
     *
     * @param activity act
     */
    public static void removeActivity(Activity activity) {
        if (activity != null && !activity.isFinishing()) {
            activity.finish();
        }
    }

    /**
     * 清除所有Activity
     */
    public void removeAllActivity() {
        for (Activity activity : sActivityList) {
            if (activity != null && !activity.isFinishing()) {
                activity.finish();
            }
        }
        ToastUtil.cancelToast();
    }

    /**
     * 退出应用
     */
    public void exitApp() {
        removeAllActivity();
    }

    /**
     * 双击退出应用
     */
    public void exitAppWithTwiceClick() {
        //获取当前时间戳
        long currentTime = System.currentTimeMillis();
        if ((currentTime - sTimeMillis) > EXIT_TIME) {
            ToastUtil.showToast(R.string.toast_twice_click_exit);
            //更新当前时间戳
            sTimeMillis = currentTime;
        } else {
            removeAllActivity();
        }
    }

    /**
     * 切换账户
     */
    public void exitAccount() {
        removeAllActivity();
        getCacheUtil().clear();
        //保证版本信息不被清空
        getCacheUtil().putSerializableObj(CacheKey.APP_INFORMATION, new AppInfoModel(Config.APP_VERSION));
    }
```



## 5、FragmentUtil

```java
public class FragmentUtil {

    /**
     * 添加Fragment
     */
    public static void addFragment(BaseActivity context, int viewId, Fragment fragment, @Nullable String tag) {
        context.getSupportFragmentManager()
                .beginTransaction()
                .add(viewId, fragment, tag)
                .commit();
    }

    /**
     * 替换Fragment
     */
    public static void replaceFragment(BaseActivity context, int viewId, Fragment fragment, @Nullable String tag) {
        context.getSupportFragmentManager()
                .beginTransaction()
                .replace(viewId, fragment, tag)
                .commit();
    }

    /**
     * 隐藏Fragment
     */
    public static void hideFragment(BaseActivity context, Fragment fragment) {
        context.getSupportFragmentManager()
                .beginTransaction()
                .hide(fragment)
                .commit();
    }

    /**
     * 展示Fragment
     */
    public static void showFragment(BaseActivity context, Fragment fragment) {
        context.getSupportFragmentManager()
                .beginTransaction()
                .show(fragment)
                .commit();
    }
}
```



6、注解绑定layout

- 定义注解

```java
@Target(ElementType.TYPE)//在类的作用范围内
@Retention(RetentionPolicy.RUNTIME)//运行时注解
public @interface ContentView {
    @LayoutRes
    int value();
}
```

- 注解工具类

```java
public class InjectUtil {


    public static int getContentViewId(Class clazz) {
        //拿到注解
        ContentView contentView = (ContentView) clazz.getAnnotation(ContentView.class);
        if (contentView == null) {
            throw new NullPointerException("请绑定布局文件");
        }
        return contentView.value();
    }

    public static IBaseContract.IBasePresenter registerPresenter(Class clazz) throws IllegalAccessException, InstantiationException {
        RegisterPresenter registerPresenter = (RegisterPresenter) clazz.getAnnotation(RegisterPresenter.class);
        if(registerPresenter != null) {
            Class presenterClass = registerPresenter.value();
            return (IBaseContract.IBasePresenter) presenterClass.newInstance();
        } else {
            throw new NullPointerException("请在V层注册Presenter");
        }
    }

}
```

- 使用

```java
直接在Activity上@ContentView(R.layout.XXX)    //第一步
    
 @Override									//第二步在BaseActivity中调用InjectUtil
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(InjectUtil.getContentViewId(this.getClass()));
        ButterKnife.bind(this);
        initActivity(savedInstanceState);
    }
```

