[TOC]

### 一、模块结构

```java
├── ConfigDispatch.java		//配置信息分发器
├── ConfigManager.java		//配置管理器，拉取配置信息，交由业务类实现处理
├── ConfigService.java		//配置API接口 用来从服务端获取数据
└── consumer
    ├── ConfigConsumer.java			//Config配置数据接口 各个具体的配置实现此接口
    ├── HomeActivityConfig.java		//首页活动配置
    ├── HomeBannerConfig.java		//首页Banner配置
    ├── HomeCategoryConfig.java		//首页分类配置
    └── NewsTabConfig.java			//资讯Tab配置
```



### 二、获取配置-ConfigManager

ConfigManager只有一个getConfigData方法用来获取配置数据

```java
	/**
     * 获取云端配置并分发解析
     */
    public void getConfigData() {
    HttpClient.getInstance().createService(ConfigService.class).getAppConfig().enqueue(new Callback<String>() {
            @Override
            public void onResponse(Call<String> call, Response<String> response) {
                if (response != null && response.code() == HttpConstants.HTTP_SUCCESS) {
                    //对获取的配置信息进行分发
                    new ConfigDispatch().dispatch(response.body());
                }
            }
        //.....
        });
    }
```



### 三、分发配置-ConfigDispatch

#### 1、后端返回数据格式

```json
{
    "code": "0",
    "data": {
        "type": 1,
        "internal": "3000",
        "banner_list": [
            {
                "title": "bnaner 标题1",
                "img_url": "banner 图片链接",
                "cmd": "banner点击跳转协议"
            },
            {
                "title": "bnaner 标题2",
                "img_url": "banner 图片链接",
                "cmd": "banner点击跳转协议"
            }
        ]
    }
}
```

#### 2、ConfigDispatch.java

这里面的核心方法就是`dispatch(String config)`方法，根据keyset找到对应的consumer.class，newInstance()创建实例并进行配置分发。

```java
public class ConfigDispatch {
    //Consumers
    private HashMap<String, Class<? extends ConfigConsumer>> mConfigConsumerMap;
    
    public ConfigDispatch() {
        mConfigConsumerMap = new HashMap<>();
        initDispatchMap();
    }

    /**
     * 分发处理
     */
    public void dispatch(String config) {
        if (TextUtils.isEmpty(config)) {
            return;
        }
        JsonObject configJson = GsonUtil.parseString(config);
        Set<String> nameSet = configJson.keySet();
        Class<? extends ConfigConsumer> configConsumerCls;
        //根据key获取各个consumer的class
        for (String name : nameSet) {
            if ((configConsumerCls = getConfigConsumer(name)) != null) {
                try {
                    //新建consumer实例并进行配置分发
                    ConfigConsumer consumer = configConsumerCls.newInstance();
                    consumer.onReceiveConfig(configJson.getAsJsonObject(name));
                } catch (IllegalAccessException | InstantiationException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * 由Config信息节点名称，获取对象处理对象
     */
    private Class<? extends ConfigConsumer> getConfigConsumer(String name) {
        if (mConfigConsumerMap == null) {
            return null;
        }
        return mConfigConsumerMap.get(name);
    }

    /**
     * 初始化分发器集合
     */
    private void initDispatchMap() {
        mConfigConsumerMap.clear();
        mConfigConsumerMap.put(HomeBannerConfig.NAME, HomeBannerConfig.class);
        mConfigConsumerMap.put(HomeCategoryConfig.NAME, HomeCategoryConfig.class);
        mConfigConsumerMap.put(HomeActivityConfig.NAME, HomeCategoryConfig.class);
        mConfigConsumerMap.put(NewsTabConfig.NAME, NewsTabConfig.class);
    }
}
```



### 四、具体分析-拿HomeBanner举例

#### 1、HomeBannerConfig

具体的配置consumer

```java
public class HomeBannerConfig implements ConfigConsumer {

    public static final String NAME = "home_banner";

    @Override
    public void onReceiveConfig(JsonObject config) {
        if (config == null) {
            return;
        }
        HomeBannerModel model = GsonUtil.get().fromJson(config, HomeBannerModel.class);
        if (model != null) {
            //调用HomeDataManager的saveXXX方法存储配置信息
            HomeDataManager.getInstance().saveHomeBanner(model);
        }
    }
}
```

#### 2、HomeDataManager

datamanager是数据管理者

```java
public class HomeDataManager {

    private static final String HOME_DATA_BANNER_CACHE_NAME = "home_data_banner_cache";
    //...
    
    //单例和cache初始化

    public HomeBannerModel getHomeBanner() {
        Object data = mCache.getAsObject(HOME_DATA_BANNER_CACHE_NAME);
        if (data instanceof HomeBannerModel) {
            return (HomeBannerModel) data;
        }
        return HomePresetData.getHomeBannerModel();
    }

    public void saveHomeBanner(HomeBannerModel banner) {
        if (banner == null) {
            return;
        }
        mCache.put(HOME_DATA_BANNER_CACHE_NAME, banner);
    }
	//category和honeActivity
}
```

#### 3、HomePresetData

预设值数据类

```java
public class HomePresetData {

    private static final int BANNER_TYPE = BannerConfig.NUM_INDICATOR_TITLE;
    private static final String BANNER_TITLE = "默认标题，可以加活动介绍";
    private static final String BANNER_URL = "https://ss0.bdstatic.com/70cFuHSh_Q1YnxGkpoWK1HF6hhy/it/u=2275563804,3196691498&fm=26&gp=0.jpg";
    private static final String BANNER_CMD = "";

    private static final String CATEGORY_URL = "https://timgsa.baidu" +
            ".com/timg?image&quality=80&size=b9999_10000&sec=1539714578381&di=e8aeac86b9f6cca216a1f25849b7da63&imgtype=0&src=http%3A%2F%2Fimg62.nipic.com%2Ffile%2F20150314%2F4949133_101345674000_1.jpg";

    private static HomeBannerModel sHomeBannerModel;
    private static HomeCategoryModel sHomeCategoryModel;
    private static HomeActivityModel sHomeActivityModel;

    static {
        List<HomeBannerModel.BannerModel> bannerModelList = new ArrayList<>();
        HomeBannerModel.BannerModel bannerModel = new HomeBannerModel.BannerModel(BANNER_TITLE, BANNER_URL, BANNER_CMD);
        bannerModelList.add(bannerModel);
        bannerModelList.add(bannerModel);
        sHomeBannerModel = new HomeBannerModel(BANNER_TYPE, bannerModelList);

        // 设置首页分类数据
        List<HomeCategoryModel.CategoryModel> categoryList = new ArrayList<>();
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "玛瑙", CATEGORY_URL, ""));
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "砖石", CATEGORY_URL, ""));
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "碧玉", CATEGORY_URL, ""));
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "欧珀", CATEGORY_URL, ""));
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "翡翠", CATEGORY_URL, ""));
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "和田玉", CATEGORY_URL, ""));
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "蜜蜡", CATEGORY_URL, ""));
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "其它", CATEGORY_URL, ""));
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "其它", CATEGORY_URL, ""));
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "其它", CATEGORY_URL, ""));
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "其它", CATEGORY_URL, ""));
        categoryList.add(new HomeCategoryModel.CategoryModel(0, "其它", CATEGORY_URL, ""));
        sHomeCategoryModel = new HomeCategoryModel(categoryList);
    }

    public static HomeBannerModel getHomeBannerModel() {
        return sHomeBannerModel;
    }

    public static HomeCategoryModel getHomeCategoryModel() {
        return sHomeCategoryModel;
    }

    public static HomeActivityModel getHomeActivityModel() {
        return sHomeActivityModel;
    }
}
```

