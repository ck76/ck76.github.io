[TOC]

### 类图

![s](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/2xQPYJ9amAxHLWYyeJtY8FGAnNbIFa0NQXvhsSP4Hos!/r/dLkAAAAAAAAA)

----

### 一、模块结构

```java
.
├── HttpClient.java				//Client,对外暴露的入口
├── alone						
│   └── Alone.java				//单独配置请求，一般不建议使用，因此不做过多设计
├── exception
│   ├── ApiException.java		//异常响应体
│   ├── ExceptionEngine.java	//错误/异常处理工具
│   └── ServerException.java	//服务器定义错误
├── global
│   ├── Global.java				//全局
│   ├── GlobalRetrofit.java		//全局Retrofit类
│   └── InitException.java		//初始化状态错误
└── response
    └── ApiResponse.java		//正确响应体
```



### 二、关键类

#### 1、HttpCLient.java

```java
public class HttpClient {

    private Application mApplication;

    /**
     * 单独配置请求
     */
    public static Alone alone() {
        return new Alone();
    }

    /**
     * 全局配置HttpClient
     */
    public void globalConfig(Retrofit.Builder builder) {
        GlobalRetrofit.getInstance().init(builder);
    }

    /**
     * 使用全局参数创建请求
     */
    public <K> K createService(Class<K> service) {
        return Global.getInstance().createService(service);
    }
}
```

#### 2、Global.java

```java
public class Global {

    /**
     * 使用全局参数创建请求
     */
    public <K> K createService(Class<K> service) {
        return GlobalRetrofit.getInstance().createService(service);
    }
}
```

#### 3、GlobalRetrofit.java 具体配置类

```java
public class GlobalRetrofit {

    private Retrofit mRetrofit;
    private Retrofit.Builder mBuilder;
    private HashMap<String, Object> mServiceCache;

    public void init(Retrofit.Builder builder) {
        if (mBuilder != null) {
            throw new InitException("重复初始化");
        }
        mBuilder = builder;
    }

    private Retrofit getRetrofit() {
        if (mRetrofit != null) {
            return mRetrofit;
        }
        if (mBuilder == null) {
            throw new InitException("未进行初始化");
        }
        mRetrofit = mBuilder.build();
        return mRetrofit;
    }

    /**
     * 使用全局参数创建请求
     */
    public <K> K createService(Class<K> service) {
        K retrofitService = (K) mServiceCache.get(service.getCanonicalName());
        if (retrofitService == null) {
            retrofitService = getRetrofit().create(service);
            mServiceCache.put(service.getCanonicalName(), retrofitService);
        }
        return retrofitService;
    }
}
```



### 三、配置

#### 1、App.java

```java
/**
     * 初始化网络库配置
     */
    private void initHttpClient() {
        // 当前为最简单配置，后续按需添加其他配置
        HttpClient.getInstance().onApplicationCreate(this);
        OkHttpClient.Builder clientBuilder = new OkHttpClient.Builder();
        HttpLoggingInterceptor logInterceptor = new HttpLoggingInterceptor();
        if (BuildConfig.DEBUG) {
            logInterceptor.setLevel(HttpLoggingInterceptor.Level.BODY);
        } else {
            logInterceptor.setLevel(HttpLoggingInterceptor.Level.NONE);
        }
        clientBuilder.addInterceptor(logInterceptor);
        clientBuilder.addNetworkInterceptor(new TokenInterceptor());
        //通过globalConfig进行配置
        HttpClient.getInstance().globalConfig(new Retrofit.Builder()
                .baseUrl(BuildConfig.API_BASE_URL)
                .addConverterFactory(GsonConverterFactory.create())
                .client(clientBuilder.build()));
    }
```

