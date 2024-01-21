[TOC]

> 通过动态代理和反射技术，将带有注解的本地接口转化为网络请求

### 一、使用实例

```java
public class LearnRetrofit {
    public static final String API_URL = "https://api.github.com";

      //创建接口
      public interface GitHub {
        @GET("/repos/{owner}/{repo}/contributors")
        Call<ResponseBody> contributors(
            @Path("owner") String owner,
            @Path("repo") String repo);
      }

        public static void main(String[] args) throws IOException {
        //创建Retrofit对象
        Retrofit retrofit = new Retrofit.Builder()
                .baseUrl(API_URL)
                .addCallAdapterFactory(RxJava2CallAdapterFactory.create())
                .addConverterFactory(GsonConverterFactory.create())
                .build();

        //动态生成一个代理对象
        GitHub github = retrofit.create(GitHub.class);

        //生成一个OKHttpCall的代理对象
        Call<ResponseBody> call = github.contributors("square", "retrofit");

        //返回结果
        Response<ResponseBody> response = call.execute();
        
        //打印数据
        System.out.println(response.body().string());
      }
}
```



### 二、创建Retrofit实例源码

**成功建立一个Retrofit对象的标准：配置好Retrofit类里的成员变量**，即配置好：

-  `serviceMethod`：包含所有网络请求信息的对象
-  `baseUrl`：网络请求的url地址
-  `callFactory`：网络请求工厂
-  `adapterFactories`：网络请求适配器工厂的集合
-  `converterFactories`：数据转换器工厂的集合
-  `callbackExecutor`：回调方法执行器

所谓`xxxFactory`、“xxx工厂”其实是设计模式中**工厂模式**的体现：将“类实例化的操作”与“使用对象的操作”分开，使得使用者不用知道具体参数就可以实例化出所需要的“产品”类。



#### 1、Builder类

Builder调用自己的有参构造函数

```java
public Builder() {
      this(Platform.get());
    }
// Retrofit2.0支持3个平台：Android平台、Java平台、IOS平台
// 最后返回一个Platform对象（指定了Android平台）给Builder的有参构造方法public Builder(Platform platform) 
// 说明Builder指定了运行平台为Android
```

##### 作用

- 平台类型对象：Android
- 网络请求适配器工厂：CallAdapterFactory
  - CallAdapter用于对原始Call进行再次封装，如Call<R>到Observable<R>

- 数据转换器工厂： converterFactory
- 回调执行器：callbackExecutor

**特别注意，这里只是设置了默认值，但未真正配置到具体的Retrofit类的成员变量当中**



#### 2、baseurl

```java
<-- 步骤1 -->
public Builder baseUrl(String baseUrl) {

      // 把String类型的url参数转化为适合OKhttp的HttpUrl类型
      HttpUrl httpUrl = HttpUrl.parse(baseUrl);     

    // 最终返回带httpUrl类型参数的baseUrl（）
    // 下面继续看baseUrl(httpUrl) ->> 步骤2
      return baseUrl(httpUrl);
    }


<-- 步骤2 -->
    public Builder baseUrl(HttpUrl baseUrl) {

      //把URL参数分割成几个路径碎片
      List<String> pathSegments = baseUrl.pathSegments();   

      // 检测最后一个碎片来检查URL参数是不是以"/"结尾
      // 不是就抛出异常    
      if (!"".equals(pathSegments.get(pathSegments.size() - 1))) {
        throw new IllegalArgumentException("baseUrl must end in /: " + baseUrl);
      }     
      this.baseUrl = baseUrl;
      return this;
    }
```

- 至此，步骤3分析完毕
- 总结：**baseUrl（）用于配置Retrofit类的网络请求url地址** 

> 将传入的String类型url转化为适合OKhttp的HttpUrl类型的url



#### 3、addCallAdapterFactory

```java
// 将上面创建的GsonConverterFactory放入到 converterFactories数组
// 在第二步放入一个内置的数据转换器工厂BuiltInConverters(）后又放入了一个GsonConverterFactory
  public Builder addConverterFactory(Converter.Factory factory) {
      converterFactories.add(checkNotNull(factory, "factory == null"));
      return this;
    }
```

GsonConverterFactory方法

```java
public final class GsonConverterFactory extends Converter.Factory {

<-- 步骤1 -->
  public static GsonConverterFactory create() {
    // 创建一个Gson对象
    return create(new Gson()); ->>步骤2
  }

<-- 步骤2 -->
  public static GsonConverterFactory create(Gson gson) {
    // 创建了一个含有Gson对象实例的GsonConverterFactory
    return new GsonConverterFactory(gson); ->>步骤3
  }

  private final Gson gson;

<-- 步骤3 -->
  private GsonConverterFactory(Gson gson) {
    if (gson == null) throw new NullPointerException("gson == null");
    this.gson = gson;
  }
```



#### 4、build方法

> 在最后一步中，通过前面步骤设置的变量，将Retrofit类的所有成员变量都配置完毕。所以，成功创建了Retrofit的实例

- 配置网络请求执行器
- 配置网络请求适配器工厂
- 配置数据转换器工厂

```java
public Retrofit build() {
 
 <--  配置网络请求执行器（callFactory）-->
      okhttp3.Call.Factory callFactory = this.callFactory;
      // 如果没指定，则默认使用okhttp
      // 所以Retrofit默认使用okhttp进行网络请求
      if (callFactory == null) {
        callFactory = new OkHttpClient();
      }

 <--  配置回调方法执行器（callbackExecutor）-->
      Executor callbackExecutor = this.callbackExecutor;
      // 如果没指定，则默认使用Platform检测环境时的默认callbackExecutor
      // 即Android默认的callbackExecutor
      if (callbackExecutor == null) {
        callbackExecutor = platform.defaultCallbackExecutor();
      }

 <--  配置网络请求适配器工厂（CallAdapterFactory）-->
      List<CallAdapter.Factory> adapterFactories = new ArrayList<>(this.adapterFactories);
      // 向该集合中添加了步骤2中创建的CallAdapter.Factory请求适配器（添加在集合器末尾）
      adapterFactories.add(platform.defaultCallAdapterFactory(callbackExecutor));
    // 请求适配器工厂集合存储顺序：自定义1适配器工厂、自定义2适配器工厂...默认适配器工厂（ExecutorCallAdapterFactory）

 <--  配置数据转换器工厂：converterFactory -->
      // 在步骤2中已经添加了内置的数据转换器BuiltInConverters(）（添加到集合器的首位）
      // 在步骤4中又插入了一个Gson的转换器 - GsonConverterFactory（添加到集合器的首二位）
      List<Converter.Factory> converterFactories = new ArrayList<>(this.converterFactories);
      // 数据转换器工厂集合存储的是：默认数据转换器工厂（ BuiltInConverters）、自定义1数据转换器工厂（GsonConverterFactory）、自定义2数据转换器工厂....

// 注：
//1. 获取合适的网络请求适配器和数据转换器都是从adapterFactories和converterFactories集合的首位-末位开始遍历
// 因此集合中的工厂位置越靠前就拥有越高的使用权限

      // 最终返回一个Retrofit的对象，并传入上述已经配置好的成员变量
      return new Retrofit(callFactory, baseUrl, converterFactories, adapterFactories,
          callbackExecutor, validateEagerly);
    }
```





#### 5、总结

Retrofit**使用建造者模式通过Builder类**建立了一个Retrofit实例，具体创建细节是配置了：

- 平台类型对象（Platform - Android）
- 网络请求的url地址（baseUrl）
- 网络请求工厂（callFactory）
  - 默认使用OkHttpCall

- 网络请求适配器工厂的集合（adapterFactories）
  - 本质是配置了网络请求适配器工厂- 默认是ExecutorCallAdapterFactory

- 数据转换器工厂的集合（converterFactories）
  - 本质是配置了数据转换器工厂

- 回调方法执行器（callbackExecutor）
  - 默认回调方法执行器作用是：切换线程（子线程 - 主线程）



### 三、创建网络接口请求实例涉及源码

#### 1、\<T> T create(final Class\<T> service) 

> 使用外观模式进行访问，里面用了代理模式

```java
public <T> T create(final Class<T> service) {

       if (validateEagerly) {  
      // 判断是否需要提前验证
      eagerlyValidateMethods(service); 
      // 具体方法作用：
      // 1. 给接口中每个方法的注解进行解析并得到一个ServiceMethod对象
      // 2. 以Method为键将该对象存入LinkedHashMap集合中
     // 特别注意：如果不是提前验证则进行动态解析对应方法（下面会详细说明），得到一个ServiceMethod对象，最后存入到LinkedHashMap集合中，类似延迟加载（默认）
    }  


        // 创建了网络请求接口的动态代理对象，即通过动态代理创建网络请求接口的实例 （并最终返回）
        // 该动态代理是为了拿到网络请求接口实例上所有注解
    return (T) Proxy.newProxyInstance(
          service.getClassLoader(),      // 动态生成接口的实现类 
          new Class<?>[] { service },    // 动态创建实例
          new InvocationHandler() {     // 将代理类的实现交给 InvocationHandler类作为具体的实现（下面会解释）
          private final Platform platform = Platform.get();

         // 在 InvocationHandler类的invoke（）实现中，除了执行真正的逻辑（如再次转发给真正的实现类对象），还可以进行一些有用的操作
         // 如统计执行时间、进行初始化和清理、对接口调用进行检查等。
          @Override 
           public Object invoke(Object proxy, Method method, Object... args)
              throws Throwable {
          
            // 下面会详细介绍 invoke（）的实现
            // 即下面三行代码
            ServiceMethod serviceMethod = loadServiceMethod(method);     
            OkHttpCall okHttpCall = new OkHttpCall<>(serviceMethod, args);
            return serviceMethod.callAdapter.adapt(okHttpCall);
          }
        });
  }

// 特别注意
// return (T) roxy.newProxyInstance(ClassLoader loader, Class<?>[] interfaces,  InvocationHandler invocationHandler)
// 可以解读为：getProxyClass(loader, interfaces) .getConstructor(InvocationHandler.class).newInstance(invocationHandler);
// 即通过动态生成的代理类，调用interfaces接口的方法实际上是通过调用InvocationHandler对象的invoke（）来完成指定的功能
// 先记住结论，在讲解步骤4的时候会再次详细说明


<-- 关注点1：eagerlyValidateMethods（） -->
private void eagerlyValidateMethods(Class<?> service) {  
    Platform platform = Platform.get();  
    for (Method method : service.getDeclaredMethods()) {  
      if (!platform.isDefaultMethod(method)) {  loadServiceMethod(method); } 
      // 将传入的ServiceMethod对象加入LinkedHashMap<Method, ServiceMethod>集合
     // 使用LinkedHashMap集合的好处：lruEntries.values().iterator().next()获取到的是集合最不经常用到的元素，提供了一种Lru算法的实现
    }  
}
```



#### 2、使用动态代理的好处？

- 当`Service`对象调用接口中方法时会进行**拦截**，调用都会集中转发到 InvocationHandler#invoke （），可集中进行处理
- 获得网络请求接口实例上的所有**注解**
- 更**方便**封装ServiceMethod



#### 3、invoke方法

```java
new InvocationHandler() {   
          private final Platform platform = Platform.get();

  @Override 
           public Object invoke(Object proxy, Method method, Object... args)
              throws Throwable {

            // 将详细介绍下面代码
            // 关注点1
            // 作用：读取网络请求接口里的方法，并根据前面配置好的属性配置serviceMethod对象
            ServiceMethod serviceMethod = loadServiceMethod(method);     
           
            // 关注点2
            // 作用：根据配置好的serviceMethod对象创建okHttpCall对象 
            OkHttpCall okHttpCall = new OkHttpCall<>(serviceMethod, args);

            // 关注点3
            // 作用：调用OkHttp，并根据okHttpCall返回rejava的Observe对象或者返回Call
            return serviceMethod.callAdapter.adapt(okHttpCall);
          }
}
```

#### 4、读取网络请求接口里的方法并配置servicemethod对象

##### ServiceMethod函数

> ServiceMethod对象包含了访问网络的所有基本信息

- 一个 ServiceMethod 对象对应于网络请求接口里的一个方法
- loadServiceMethod（method）负责加载 ServiceMethod，
- ServiceMethod类对象采用了单例模式进行创建，
- 即创建ServiceMethod对象前，先看serviceMethodCache有没有缓存之前创建过的网络请求实例，若没缓存，则通过建造者模式创建 serviceMethod 对象
- 这里就是上面说的创建实例的缓存机制：采用单例模式从而实现一个 ServiceMethod 对象对应于网络请求接口里的一个方法

```java
// 注：由于每次获取接口实例都是传入 class 对象
// 而 class 对象在进程内单例的，所以获取到它的同一个方法 Method 实例也是单例的，所以这里的缓存是有效的。
```

##### ServiceMethod.Builder()方法

```java
public Builder(Retrofit retrofit, Method method) {
      this.retrofit = retrofit;
      this.method = method;

      // 获取网络请求接口方法里的注解
      this.methodAnnotations = method.getAnnotations();
      // 获取网络请求接口方法里的参数类型       
      this.parameterTypes = method.getGenericParameterTypes();  
      //获取网络请求接口方法里的注解内容    
      this.parameterAnnotationsArray = method.getParameterAnnotations();    
    }
}
```

##### ServiceMethod的build()

> 控制ServiceMethod对象的生成流程

```java
//根据网络请求接口方法的返回值和注解类型，从Retrofit对象中获取对应的网络请求适配器 
//根据网络请求接口方法的返回值和注解类型，从Retrofit对象中获取该网络适配器返回的数据类型
// 根据网络请求接口方法的返回值和注解类型，从Retrofit对象中获取对应的数据转换器  -->关注点3
// 构造 HTTP 请求时，我们传递的参数都是String
// Retrofit 类提供 converter把传递的参数都转化为 String 
// 其余类型的参数都利用 Converter.Factory 的stringConverter 进行转换
// @Body 和 @Part 类型的参数利用Converter.Factory 提供的 requestBodyConverter 进行转换
// 这三种 converter 都是通过“询问”工厂列表进行提供，而工厂列表我们可以在构造 Retrofit 对象时进行添加。

// 解析网络请求接口中方法的注解
// 主要是解析获取Http请求的方法
// 注解包括：DELETE、GET、POST、HEAD、PATCH、PUT、OPTIONS、HTTP、retrofit2.http.Headers、Multipart、FormUrlEncoded
// 处理主要是调用方法 parseHttpMethodAndPath(String httpMethod, String value, boolean hasBody) ServiceMethod中的httpMethod、hasBody、relativeUrl、relativeUrlParamNames域进行赋值

// 为方法中的每个参数创建一个ParameterHandler<?>对象并解析每个参数使用的注解类型
// 该对象的创建过程就是对方法参数中注解进行解析
// 这里的注解包括：Body、PartMap、Part、FieldMap、Field、Header、QueryMap、Query、Path、Url


<-- 总结 -->
// 1. 根据返回值类型和方法标注从Retrofit对象的的网络请求适配器工厂集合和内容转换器工厂集合中分别获取到该方法对应的网络请求适配器和Response内容转换器；
// 2. 根据方法的标注对ServiceMethod的域进行赋值
// 3. 最后为每个方法的参数的标注进行解析，获得一个ParameterHandler<?>对象
// 该对象保存有一个Request内容转换器——根据参数的类型从Retrofit的内容转换器工厂集合中获取一个Request内容转换器或者一个String内容转换器。
```



#### 5、根据配置好的serviceMethod对象创建okHttpCall对象 

> 根据第一步配置好的`ServiceMethod`对象和输入的请求参数创建`okHttpCall`对象

```java
<--OkHttpCall类 -->
public class OkHttpCall {
    private final ServiceMethod<T> serviceMethod; // 含有所有网络请求参数信息的对象  
    private final Object[] args; // 网络请求接口的参数 
    private okhttp3.Call rawCall; //实际进行网络访问的类  
    private Throwable creationFailure; //几个状态标志位  
    private boolean executed;  
    private volatile boolean canceled;  
  
<--OkHttpCall构造函数 -->
  public OkHttpCall(ServiceMethod<T> serviceMethod, Object[] args) {  
    // 传入了配置好的ServiceMethod对象和输入的请求参数
    this.serviceMethod = serviceMethod;  
    this.args = args;  
}
```



#### 6、调用OkHttp，并根据okHttpCall返回Rxjava的Observe对象或者返回Call

> return serviceMethod.callAdapter.adapt(okHttpCall);

将第二步创建的`OkHttpCall`对象传给第一步创建的`serviceMethod`对象中对应的网络请求适配器工厂的`adapt（）`

> 返回对象类型：Android默认的是`Call<>`；若设置了RxJavaCallAdapterFactory，返回的则是`Observable<>`

```java
<--  adapt（）详解-->
public <R> Call<R> adapt(Call<R> call) {
        return new ExecutorCallbackCall<>(callbackExecutor, call);  
      }

   ExecutorCallbackCall(Executor callbackExecutor, Call<T> delegate) {
      this.delegate = delegate; 
      // 把上面创建并配置好参数的OkhttpCall对象交给静态代理delegate
      // 静态代理和动态代理都属于代理模式
     // 静态代理作用：代理执行被代理者的方法，且可在要执行的方法前后加入自己的动作，进行对系统功能的拓展
      
      this.callbackExecutor = callbackExecutor;
      // 传入上面定义的回调方法执行器
      // 用于进行线程切换   
    }
```

- 采用了**装饰模式**：ExecutorCallbackCall = 装饰者，而里面真正去执行网络请求的还是OkHttpCall
- 使用装饰模式的原因：希望在OkHttpCall发送请求时做一些额外操作。这里的额外操作是线程转换，即将子线程切换到主线程

- OkHttpCall的enqueue()是进行网络异步请求的：当你调用OkHttpCall.enqueue（）时，回调的callback是在子线程中，需要通过Handler转换到主线程进行回调。ExecutorCallbackCall就是用于线程回调；
- 当然以上是原生Retrofit使用的切换线程方式。如果你用Rxjava，那就不会用到这个ExecutorCallbackCall而是RxJava的Call。



#### 7、总结

Retrofit采用了外观模式统一调用创建网络请求接口实例和网络请求参数配置的方法，具体细节是：

- 动态创建网络请求接口的实例**（代理模式 - 动态代理）** 
- 创建 `serviceMethod` 对象**（建造者模式 & 单例模式（缓存机制））** 
- 对 `serviceMethod` 对象进行网络请求参数配置：通过解析网络请求接口方法的参数、返回值和注解类型，从Retrofit对象中获取对应的网络请求的url地址、网络请求执行器、网络请求适配器 & 数据转换器。**（策略模式）** 
- 对 `serviceMethod` 对象加入线程切换的操作，便于接收数据后通过Handler从子线程切换到主线程从而对返回数据结果进行处理**（装饰模式）** 
- 最终创建并返回一个`OkHttpCall`类型的网络请求对象



### 四、执行网络请求涉及源码

#### 1、Call\<JavaBean> call = XXXService.getXXX();

`XXXService`对象实际上是动态代理对象`Proxy.newProxyInstance（）`（步骤3中已说明），并不是真正的网络请求接口创建的对象

当`XXXService`对象调用`getXXX（）`时会被动态代理对象`Proxy.newProxyInstance（）`拦截，然后调用自身的`InvocationHandler # invoke（）` 

 `invoke(Object proxy, Method method, Object... args)`会传入3个参数：`Object proxy:`（代理对象）、
 `Method method`（调用的`getCall()`）
 `Object... args`（方法的参数，即`getXXX（*）`中的*）

接下来利用Java反射获取到`getXXX（）`的注解信息，配合args参数创建`ServiceMethod对象`。



**最终创建并返回一个OkHttpCall类型的Call对象**

> 1. `OkHttpCall`类是`OkHttp`的包装类
> 2. 创建了`OkHttpCall`类型的Call对象还不能发送网络请求，需要创建`Request`对象才能发送网络请求



#### 2、执行请求

- `Retrofit`默认使用`OkHttp`，即`OkHttpCall类`（实现了 `retrofit2.Call<T>`接口）
  - 但可以自定义选择自己需要的Call类

- OkHttpCall提供了两种网络请求方式： 
  - 同步请求：`OkHttpCall.execute()` 
  - 异步请求：`OkHttpCall.enqueue()` 



#### 3、同步请求OkHttpCall.execute()

##### 步骤

**步骤1：**对网络请求接口的方法中的每个参数利用对应`ParameterHandler`进行解析，再根据`ServiceMethod`对象创建一个`OkHttp`的`Request`对象

 **步骤2：**使用`OkHttp`的`Request`发送网络请求；

 **步骤3：**对返回的数据使用之前设置的数据转换器（GsonConverterFactory）解析返回的数据，最终得到一个`Response<T>`对象



##### 使用

```java
Response<JavaBean> response = call.execute();  
//上面简单的一行代码，其实包含了整个发送网络同步请求的三个步骤。

- createRawCall()
- parseResponse()
- 生成Response类()
```

- `ServiceMethod`几乎保存了一个网络请求所需要的数据
- 发送网络请求时，`OkHttpCall`需要从`ServiceMethod`中获得一个Request对象
- 解析数据时，还需要通过`ServiceMethod`使用`Converter`（数据转换器）转换成Java对象进行数据解析

> 为了提高效率，Retrofit还会对解析过的请求`ServiceMethod`进行缓存，存放在`Map<Method, ServiceMethod> serviceMethodCache = new LinkedHashMap<>();`对象中，即第二步提到的单例模式



#### 4、异步请求OkHttpCall.enqueue()

##### 步骤

**步骤1：**对网络请求接口的方法中的每个参数利用对应`ParameterHandler`进行解析，再根据`ServiceMethod`对象创建一个`OkHttp`的`Request`对象

 **步骤2：**使用`OkHttp`的`Request`发送网络请求；

 **步骤3：**对返回的数据使用之前设置的数据转换器（GsonConverterFactory）解析返回的数据，最终得到一个`Response<T>`对象

 **步骤4：**进行线程切换从而在主线程处理返回的数据结果

> 若使用了RxJava，则直接回调到主线程

异步请求的过程跟同步请求类似，**唯一不同之处在于：异步请求会将回调方法交给回调执行器在指定的线程中执行。**

> 指定的线程此处是指主线程（UI线程）



##### 使用

```java
call.enqueue(new Callback<JavaBean>() {
            @Override
            public void onResponse(Call<JavaBean> call, Response<JavaBean> response) {
                System.out.println(response.isSuccessful());
                if (response.isSuccessful()) {
                    response.body().show();
                }
                else {
                    try {
                        System.out.println(response.errorBody().string());
                    } catch (IOException e) {
                        e.printStackTrace();
                    } ;
                }
            }
    
/*
从上面分析有：call是一个静态代理
使用静态代理的作用是：在okhttpCall发送网络请求的前后进行额外操作
这里的额外操作是：线程切换，即将子线程切换到主线程，从而在主线程对返回的数据结果进行处理
*/
```



##### call.enqueue源代码

```java
<--  call.enqueue（）解析  -->
@Override 
public void enqueue(final Callback<T> callback) {

      delegate.enqueue(new Callback<T>() {
     // 使用静态代理 delegate进行异步请求 ->>分析1
     // 等下记得回来
        @Override 
        public void onResponse(Call<T> call, final Response<T> response) {
          // 步骤4：线程切换，从而在主线程显示结果
          callbackExecutor.execute(new Runnable() {
          // 最后Okhttp的异步请求结果返回到callbackExecutor
          // callbackExecutor.execute（）通过Handler异步回调将结果传回到主线程进行处理（如显示在Activity等等），即进行了线程切换
          // 具体是如何做线程切换 ->>分析2
              @Override 
               public void run() {
              if (delegate.isCanceled()) {
                callback.onFailure(ExecutorCallbackCall.this, new IOException("Canceled"));
              } else {
                callback.onResponse(ExecutorCallbackCall.this, response);
              }
            }
          });
        }

        @Override 
        public void onFailure(Call<T> call, final Throwable t) {
          callbackExecutor.execute(new Runnable() {
            @Override public void run() {
              callback.onFailure(ExecutorCallbackCall.this, t);
            }
          });
        }
      });
    }


<-- 分析1：delegate.enqueue（）解析 -->
@Override 
public void enqueue(final Callback<T> callback) {
   
    okhttp3.Call call;
    Throwable failure;

// 步骤1：创建OkHttp的Request对象，再封装成OkHttp.call
     // delegate代理在网络请求前的动作：创建OkHttp的Request对象，再封装成OkHttp.call
    synchronized (this) {
      if (executed) throw new IllegalStateException("Already executed.");
      executed = true;

      call = rawCall;
      failure = creationFailure;
      if (call == null && failure == null) {
        try {
         
          call = rawCall = createRawCall(); 
          // 创建OkHttp的Request对象，再封装成OkHttp.call
         // 方法同发送同步请求，此处不作过多描述  
        } catch (Throwable t) {
          failure = creationFailure = t;
        }
      }

// 步骤2：发送网络请求
    // delegate是OkHttpcall的静态代理
    // delegate静态代理最终还是调用Okhttp.enqueue进行网络请求
    call.enqueue(new okhttp3.Callback() {
      @Override 
        public void onResponse(okhttp3.Call call, okhttp3.Response rawResponse)
          throws IOException {
        Response<T> response;
        try {
        
          // 步骤3：解析返回数据
          response = parseResponse(rawResponse);
        } catch (Throwable e) {
          callFailure(e);
          return;
        }
        callSuccess(response);
      }

      @Override 
         public void onFailure(okhttp3.Call call, IOException e) {
        try {
          callback.onFailure(OkHttpCall.this, e);
        } catch (Throwable t) {
          t.printStackTrace();
        }
      }

      private void callFailure(Throwable e) {
        try {
          callback.onFailure(OkHttpCall.this, e);
        } catch (Throwable t) {
          t.printStackTrace();
        }
      }

      private void callSuccess(Response<T> response) {
        try {
          callback.onResponse(OkHttpCall.this, response);
        } catch (Throwable t) {
          t.printStackTrace();
        }
      }
    });
  }

// 请回去上面分析1的起点

<-- 分析2：异步请求后的线程切换-->
// 线程切换是通过一开始创建Retrofit对象时Platform在检测到运行环境是Android时进行创建的：（之前已分析过）
// 采用适配器模式
static class Android extends Platform {

    // 创建默认的回调执行器工厂
    // 如果不将RxJava和Retrofit一起使用，一般都是使用该默认的CallAdapter.Factory
    // 后面会对RxJava和Retrofit一起使用的情况进行分析
    @Override
      CallAdapter.Factory defaultCallAdapterFactory(Executor callbackExecutor) {
      return new ExecutorCallAdapterFactory(callbackExecutor);
    }

    @Override 
      public Executor defaultCallbackExecutor() {
      // 返回一个默认的回调方法执行器
      // 该执行器负责在主线程（UI线程）中执行回调方法
      return new MainThreadExecutor();
    }

    // 获取主线程Handler
    static class MainThreadExecutor implements Executor {
      private final Handler handler = new Handler(Looper.getMainLooper());


      @Override 
      public void execute(Runnable r) {
        // Retrofit获取了主线程的handler
        // 然后在UI线程执行网络请求回调后的数据显示等操作。
        handler.post(r);
      }
    }

// 切换线程的流程：
// 1. 回调ExecutorCallAdapterFactory生成了一个ExecutorCallbackCall对象
// 2. 通过调用ExecutorCallbackCall.enqueue(CallBack)从而调用MainThreadExecutor的execute()通过handler切换到主线程处理返回结果（如显示在Activity等等）
  }
```



#### 5、CallAdapter介绍

定义：网络请求执行器（Call）的适配器

Call在Retrofit里默认是`OkHttpCall` 

在Retrofit中提供了四种CallAdapterFactory：

-  ExecutorCallAdapterFactory（默认）、
- GuavaCallAdapterFactory、
- Java8CallAdapterFactory、
- RxJavaCallAdapterFactory

作用：将默认的网络请求执行器（OkHttpCall）转换成适合被不同平台来调用的网络请求执行器形式

> 1. 如：一开始`Retrofit`只打算利用`OkHttpCall`通过`ExecutorCallbackCall`切换线程；但后来发现使用`Rxjava`更加方便（不需要Handler来切换线程）。想要实现`Rxjava`的情况，那就得使用`RxJavaCallAdapterFactoryCallAdapter`将`OkHttpCall`转换成`Rxjava(Scheduler)`

好处：用最小代价兼容更多平台，即能适配更多的使用场景



### 五、总结

`Retrofit` 本质上是一个 `RESTful` 的`HTTP` 网络请求框架的封装，即通过 大量的设计模式 封装了 `OkHttp` ，使得简洁易用。具体过程如下：

1.  `Retrofit` 将 `Http`请求 抽象 成 `Java`接口
2. 在接口里用 注解 描述和配置 网络请求参数
3. 用动态代理 的方式，动态将网络请求接口的注解 解析 成`HTTP`请求
4. 最后执行`HTTP`请求



#### 1、请求流程

![1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/QGy7YsDrA9Y.qLHuub1StfTkhI8GY*aX*e7kEmCzLwc!/r/dLgAAAAAAAAA)

-----

![2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/DzkXuX6TdjHtCeKzKX.Sx*j8gwEz418d22U6pppcWq4!/r/dDcBAAAAAAAA)

----



![retrofit](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/af3t*gEG7lxpHHz*IgGeOs20i1BIk5l6B0r2SPjdFm0!/r/dLYAAAAAAAAA)



#### 2、总结图

![retrofit](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/EETFlcUMHGNuwIz2cNniM.wR*N2hzuuyhPvA9bja7kU!/r/dFMBAAAAAAAA)



#### 3、解耦套路

![解耦](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/e3MtlBml.NrzaC4Yuxkgv3geHE2XSLgfwyGhhz755fg!/r/dLgAAAAAAAAA)

---





我们要是用`Retrofit`进行HTTP网络请求的时候，首先要创建一个接口进行HTTP动作描述，之所以用接口是为了后面动态创建一个对应的接口代理类（java的动态代理只能支持接口）；

接口描述创建完成后，通过建造者模式可以配置出一个`retrofit`实例

由`retrofit`的`create函数就可以创建出一个接口的动态代理类

调用动态代理类的接口方法时，就会被动态代理类拦截，拦截的主要原因就是为了隐式的创建出一个用于HTTP网络请求的`Call`对象，具体做了3件事:
 1. 解析接口注解内容并保存，功能类为`ServiceMethod`。
 2. 创建一个`OkHttpCall`对象，`OkHttpCall`内部持有`okhttp3.Call`实例。`OkHttpCall`的作用就是从`ServiceMethod`和当前参数获取得到一个`Request`，并通过`OkHttpClient`根据这个`Request`创建出一个`okhttp3.Call`对象。
 3. 最后从`ServiceMethod`中拿到一个可以处理接口方法返回类型的`CallAdapter`，由其`adapt`返回得到一个可以对HTTP请求结果进行线程调度的`retrofit2.Call`对象（该`Call`对象是`OkHttpCall`的代理类）。



### 参考文章

- [codeKK的Retrofit源码解析](https://github.com/android-cn/android-open-project-analysis/tree/master/tool-lib/network/retrofit)
- [Carson_Ho手把手带你 深入读懂 Retrofit 2.0 源码](https://www.jianshu.com/p/0c055ad46b6c)
- [Retrofit源码分析（超详细）](https://www.jianshu.com/p/097947afddaf)
- [retrofit2源码分析](https://www.jianshu.com/p/80304bb9200e?utm_campaign=maleskine&utm_content=note&utm_medium=seo_notes&utm_source=recommendation)

- [Retrofit 2.0源码解析](https://www.jianshu.com/p/67aad48668d8?utm_campaign=maleskine&utm_content=note&utm_medium=seo_notes&utm_source=recommendation)

- [流程解析](https://www.jianshu.com/p/855fce462242)