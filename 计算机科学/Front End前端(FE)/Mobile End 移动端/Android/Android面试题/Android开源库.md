[TOC]

见Android目录下的流行开源库



### Retrofit

> 通过动态代理和反射技术，将带有注解的本地接口转化为网络请求

- 构建Retrofit实例

  > Retrofit**使用建造者模式通过Builder类**建立了一个Retrofit实例，具体创建细节是配置了：

  - 平台类型对象（Platform - Android）
  - 网络请求的url地址（baseUrl）
  - **网络请求工厂（callFactory）**
    - 默认使用OkHttpCall
  - 网络请求适**配器工厂**的集合（adapterFactories）
    - 本质是配置了网络请求适配器工厂- 默认是ExecutorCallAdapterFactory
  - 数据转**换器工厂**的集合（converterFactories）
    - 本质是配置了数据转换器工厂
  - **回调方法执行器（callbackExecutor）**
    - 默认回调方法执行器作用是：切换线程（子线程 - 主线程）



- 创建网络接口请求实例

  - 当`Service`对象调用接口中方法时会进行**拦截**，调用都会集中转发到 InvocationHandler#invoke （），可集中进行处理
  - 获得网络请求接口实例上的所有**注解**
  - 更**方便**封装ServiceMethod

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

  Retrofit采用了外观模式统一调用创建网络请求接口实例和网络请求参数配置的方法，具体细节是：

  - 动态创建网络请求接口的实例**（代理模式 - 动态代理）** 
  - 创建 `serviceMethod` 对象**（建造者模式 & 单例模式（缓存机制））** 
  - 对 `serviceMethod` 对象进行网络请求参数配置：通过解析网络请求接口方法的参数、返回值和注解类型，从Retrofit对象中获取对应的网络请求的url地址、网络请求执行器、网络请求适配器 & 数据转换器。**（策略模式）** 
  - 对 `serviceMethod` 对象加入线程切换的操作，便于接收数据后通过Handler从子线程切换到主线程从而对返回数据结果进行处理**（装饰模式）** 
  - 最终创建并返回一个`OkHttpCall`类型的网络请求对象



- 执行网络请求涉及源码

  - Call\<JavaBean> call = XXXService.getXXX();

  - **最终创建并返回一个OkHttpCall类型的Call对象**

    > 1. `OkHttpCall`类是`OkHttp`的包装类
    > 2. 创建了`OkHttpCall`类型的Call对象还不能发送网络请求，需要创建`Request`对象才能发送网络请求

  - `Retrofit`默认使用`OkHttp`，即`OkHttpCall类`（实现了 `retrofit2.Call<T>`接口）

    - 但可以自定义选择自己需要的Call类

  - OkHttpCall提供了两种网络请求方式： 

    - 同步请求：`OkHttpCall.execute()` 
    - 异步请求：`OkHttpCall.enqueue()` 

  - **`ServiceMethod`几乎保存了一个网络请求所需要的所有数据**
  - **发送网络请求时，`OkHttpCall`需要从`ServiceMethod`中获得一个Request对象**
  - 解析数据时，还需要通过`ServiceMethod`使用`Converter`（数据转换器）转换成Java对象进行数据解析

  > 为了提高效率，Retrofit还会对解析过的请求`ServiceMethod`进行缓存，存放在`Map<Method, ServiceMethod> serviceMethodCache = new LinkedHashMap<>();`对象中，即第二步提到的单例模式



- `Retrofit` 本质上是一个 `RESTful` 的`HTTP` 网络请求框架的封装，即通过 大量的设计模式 封装了 `OkHttp` ，使得简洁易用。具体过程如下：
  1. `Retrofit` 将 `Http`请求 抽象 成 `Java`接口
  2. 在接口里用 注解 描述和配置 网络请求参数
  3. 用动态代理 的方式，动态将网络请求接口的注解 解析 成`HTTP`请求
  4. 最后执行`HTTP`请求
- ![retrofit](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/af3t*gEG7lxpHHz*IgGeOs20i1BIk5l6B0r2SPjdFm0!/r/dLYAAAAAAAAA)
- ![retrofit](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/EETFlcUMHGNuwIz2cNniM.wR*N2hzuuyhPvA9bja7kU!/r/dFMBAAAAAAAA)
- ![解耦](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/e3MtlBml.NrzaC4Yuxkgv3geHE2XSLgfwyGhhz755fg!/r/dLgAAAAAAAAA)



### Butterknife

- http://www.360doc.com/content/16/0715/17/8279768_575749596.shtml



### RxJava

看Android分类下文章