[TOC]

### 类图

![类图](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/DQ*RBhTsg.0UqZZ7KeQJpPbyawK6OrFzkB*zaApbH0o!/r/dDQBAAAAAAAA)

------

### 一、模块结构

```java
.
├── ISplashContext.java					//Splash引导Ioc接口
├── SplashManager.java					//引导页管理
├── SplashRuntime.java					//Runtime
├── SplashRuntimeInit.java				//Runtime初始化入口
├── adapter		//Adapter
│   └── SplashAdapter.java				//启动页面Adapter
├── model		//各种数据model
│   ├── Ad.java							//广告页面数据model
│   ├── Guide.java						//引导页面Model
│   ├── Slogan.java						//Slogan页面数据
│   └── Splash.java						//导页数据Model(接口) 供其他model实现implements
└── ui			//界面
    ├── GuideFragment.java					//引导页面
    ├── OnEnterHomeListener.java			//点击按钮进入首页接口
    ├── SloganFragment.java					//Slogan 启动页面
    ├── SplashActivity.java					//启动页
    └── SplashAdFragment.java				//开屏广告页面
```



### 二、common

#### 1、ISplashContext

由app/ioc下的SplashContext实现，注入到`SplashRuntime`的`sSplashContext`。

```java
public interface ISplashContext {
    /**
     * 进入主页面
     */
    void startHomeActivity(Context context);
}
```

#### 2、￥￥SplashManager 数据管理类

```java
public class SplashManager {

    public static final String SP_NAME = "sp_splash";//SharedPreference名字
    public static final String KEY_FIRST_USE = "key_first_use:";//是不是第一次使用
    public static final String KEY_SPLASH_AD = "key_splash_ad";//广告Json，Key

    private static int sSloganRes = R.drawable.bg_guide_three;
    private static List<Integer> mGuideRes;

    static {
        mGuideRes = new ArrayList<>();
        mGuideRes.add(R.drawable.bg_guide_one);
        mGuideRes.add(R.drawable.bg_guide_two);
    }

    /**
     * 获取引导页数据
     */
    public static List<Splash> getSplashData() {
        List<Splash> splashList = new ArrayList<>();
        //如果是第一次进入
        if (isFirstUse() && mGuideRes != null && !mGuideRes.isEmpty()) {
            //引导页有多少张图片就都加进去
            for (int i = 0; i < mGuideRes.size(); i++) {
                Guide guide = new Guide(mGuideRes.get(i));
                //如果是最后一个就显示进入首页按钮
                if (i == mGuideRes.size() - 1) {
                    guide.setShowInter(true);
                }
                splashList.add(guide);
            }
            //更新状态，不是第一次进入
            updateFirstUseStatus(false);
            return splashList;
        }
        //如果广告数据不为空，广告的持续时间进行个判断
        Ad ad;
        if ((ad = getSplashAd()) != null
                && ad.getStartTime() < System.currentTimeMillis()
                && ad.getEndTime() > System.currentTimeMillis()) {
            splashList.add(ad);
            //删除广告数据
            deleteSplashAd();
            return splashList;
        }
        //默认显示SloganFragment页面。
        splashList.add(new Slogan(sSloganRes));
        return splashList;
    }

	//更新首次使用状态
    /**
     * 当前版本是否首次使用
     */
    private static boolean isFirstUse() {
        return SharedPreferencesUtil.getInstance(SP_NAME).getBoolean(KEY_FIRST_USE
                + ApplicationInfo.getVersionName(AppRuntime.getAppContext()), true);
    }

    //存储开屏广告数据
    /**
     * 获取开屏广告数据
     */
    private static Ad getSplashAd() {
        String json = SharedPreferencesUtil.getInstance(SP_NAME).getString(KEY_SPLASH_AD);
        if (TextUtils.isEmpty(json)) {
            return null;
        }
        return new Gson().fromJson(json, Ad.class);
    }
    //删除开屏广告数据
}

```

#### 3、SplashRuntime

```java
public class SplashRuntime {
    static ISplashContext sSplashContext;
    public static ISplashContext getContext() {
        return sSplashContext;
    }
}
```

#### 4、SplashRuntimeInit

```java
public class SplashRuntimeInit {
    public static void init(ISplashContext context) {
        SplashRuntime.sSplashContext = context;
    }
}
```



### 三、model

- Ad 广告页

开始时间，结束时间和ImgPath

- Guide 引导页

ImgRes 是否展示Inter

- Slogan 启动页

ImgRes



### 四、Adapter

#### 1、SplashAdapter

```java
public class SplashAdapter extends FragmentPagerAdapter {

    private List<Splash> mSplashList;

    //构造函数

    @Override
    public Fragment getItem(int position) {
        Splash splash = mSplashList.get(position);
        Bundle bundle = new Bundle();
        if (splash instanceof Guide) {
            bundle.putInt(GuideFragment.KEY_IMG_RES, ((Guide) splash).getImgRes());
            bundle.putBoolean(GuideFragment.KEY_SHOW_ENTER, ((Guide) splash).isShowInter());
            GuideFragment guideFragment = new GuideFragment();
            guideFragment.setArguments(bundle);
            return guideFragment;
        } else if (splash instanceof Slogan) {
            bundle.putInt(SloganFragment.KEY_IMG_RES, ((Slogan) splash).getImgRes());
            SloganFragment sloganFragment = new SloganFragment();
            sloganFragment.setArguments(bundle);
            return sloganFragment;
        } else if (splash instanceof Ad) {
            bundle.putString(GuideFragment.KEY_IMG_RES, ((Ad) splash).getImgPath());
            SplashAdFragment adFragment = new SplashAdFragment();
            adFragment.setArguments(bundle);
            return adFragment;
        } else {
            throw new IllegalStateException("不支持的引导页类型");
        }
    }

    @Override
    public int getCount() {
        return mSplashList != null ? mSplashList.size() : 0;
    }
}
```



### 五、UI

#### 1、引导页Guide

#### 2、启动页面Slogan

其余两个fragment都一样，所以就分析这一个。

```java
public class SloganFragment extends AbsFragment {

    private static final int DELAY = 1500;
    public static final String KEY_IMG_RES = "key_img_res";

    @DrawableRes
    private int mImgRes;
    private OnEnterHomeListener mEnterHomeListener;

    @Override
    protected void initVariable() {
        Bundle bundle = getArguments();
        if (bundle == null) {
            return;
        }
        mImgRes = bundle.getInt(KEY_IMG_RES);
    }

    @Override
    protected void initView() {
        sloganImg.setImageResource(mImgRes);
    }

    @Override
    public void onResume() {
        super.onResume();
        new Handler().postDelayed(new Runnable() {
            @Override
            public void run() {
                if (mEnterHomeListener != null) {
                    mEnterHomeListener.enterHome();
                }
            }
        }, DELAY);
    }

    @Override
    public void onAttach(Context context) {
        super.onAttach(context);
        if (context instanceof OnEnterHomeListener) {
            mEnterHomeListener = (OnEnterHomeListener) context;
        }
    }
}
```

#### 3、开屏广告页 Ad

#### 4、SplashActivity

```java
public class SplashActivity extends AbsActivity implements OnEnterHomeListener {

	//...

    @Override
    protected void initVariable() {
        mSplashAdapter = new SplashAdapter(getSupportFragmentManager(), SplashManager.getSplashData());
    }

    @Override
    protected void initView() {
        viewPager.setAdapter(mSplashAdapter);
    }

    @Override
    public void enterHome() {
        SplashRuntime.getContext().startHomeActivity(SplashActivity.this);
        finish();
    }

   //..
}
```



### 六、初始化

- SplashContext----app/ioc包

```java
/**
 * 引导页Ioc实现
 */
public class SplashContext implements ISplashContext {

    @Override
    public void startHomeActivity(Context context) {
        Intent intent = new Intent(context, MainActivity.class);
        context.startActivity(intent);
    }
}
```

- App

```java
	/**
     * 初始化Ioc接口实例
     */
    private void initRuntime() {
        //依赖注入
        SplashRuntimeInit.init(new SplashContext());
    }	
```

