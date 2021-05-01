[TOC]

> 如果还不了解RxJava，请看文章：[Android：这是一篇 清晰 & 易懂的Rxjava 入门教程](https://www.jianshu.com/p/a406b94f3188)

- `RxJava`如此受欢迎的原因，在于其**提供了丰富 & 功能强大的操作符，几乎能完成所有的功能需求**
- 本文主要讲解的是：
  1. `RxJava`中的常见开发应用场景： **线程控制（也称为调度 / 切换）**，即讲解功能性操作符中的：`subscribeOn（） & observeOn（）`
  2. **`Retrofit` 结合 `RxJava`的实例Demo教学**

希望你们会喜欢。

> 1. 本系列文章主要基于 `Rxjava 2.0`
> 2. 接下来的时间，**我将持续推出 `Android`中 `Rxjava 2.0` 的一系列文章，包括原理、操作符、应用场景、背压等等** ，有兴趣可以继续关注[Carson_Ho的安卓开发笔记](https://www.jianshu.com/users/383970bef0a0/latest_articles)！！

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm04dntf8hj30xc0rgwie.jpg)

示意图

------

# 目录

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm04dmh938j30kd0gr3zr.jpg)

示意图

------

# 1. RxJava线程控制（调度 / 切换）的作用是什么？

指定 被观察者 `（Observable）` / 观察者`（Observer）` 的工作线程类型。

------

# 2. 为什么要进行RxJava线程控制（调度 / 切换）？

### 2.1 背景

- 在 `RxJava`模型中，**被观察者 `（Observable）` / 观察者`（Observer）`的工作线程 = 创建自身的线程**

> 即，若被观察者 `（Observable）` / 观察者`（Observer）`在主线程被创建，那么他们的工作（生产事件 / 接收& 响应事件）就会发生在主线程

- 因为创建被观察者 `（Observable）` / 观察者`（Observer）`的线程 = 主线程
- 所以生产事件 / 接收& 响应事件都发生在主线程

> 下面请看1个RxJava的基础使用



```java
public class MainActivity extends AppCompatActivity {

    private static final String TAG = "Rxjava";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // 步骤1：创建被观察者 Observable & 发送事件
        // 在主线程创建被观察者 Observable 对象
        // 所以生产事件的线程是：主线程

        Observable<Integer> observable = Observable.create(new ObservableOnSubscribe<Integer>() {
            @Override
            public void subscribe(ObservableEmitter<Integer> emitter) throws Exception {

                Log.d(TAG, " 被观察者 Observable的工作线程是: " + Thread.currentThread().getName());
                // 打印验证
                emitter.onNext(1);
                emitter.onComplete();
            }
        });

// 步骤2：创建观察者 Observer 并 定义响应事件行为
        // 在主线程创建观察者 Observer 对象
        // 所以接收 & 响应事件的线程是：主线程
        Observer<Integer> observer = new Observer<Integer>() {

            @Override
            public void onSubscribe(Disposable d) {
                Log.d(TAG, "开始采用subscribe连接");
                Log.d(TAG, " 观察者 Observer的工作线程是: " + Thread.currentThread().getName());
                // 打印验证

            }
            @Override
            public void onNext(Integer value) {
                Log.d(TAG, "对Next事件"+ value +"作出响应"  );
            }
            @Override
            public void onError(Throwable e) {
                Log.d(TAG, "对Error事件作出响应");
            }
            @Override
            public void onComplete() {
                Log.d(TAG, "对Complete事件作出响应");
            }
        };

        // 步骤3：通过订阅（subscribe）连接观察者和被观察者
        observable.subscribe(observer);
    }
}
```

- 测试结果

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm04dk199tj30wq05mac8.jpg)

示意图

### 2.2 冲突

- 对于一般的需求场景，需要在子线程中实现耗时的操作；然后回到主线程实现 `UI`操作

- 应用到

   

  ```
  RxJava
  ```

  模型中，可理解为：

  1. 被观察者 `（Observable）` 在 **子线程** 中生产事件（如实现耗时操作等等）
  2. 观察者`（Observer）`在 **主线程** 接收 & 响应事件（即实现UI操作）

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm04di23lcj30xc0boq42.jpg)

示意图

### 2.3 解决方案

所以，为了解决上述冲突，即实现 **真正的异步操作**，我们需要对`RxJava`进行 **线程控制（也称为调度 / 切换）**

------

# 3. 实现方式

采用 `RxJava`内置的**线程调度器**（ `Scheduler` ），即通过 **功能性操作符`subscribeOn（）` & `observeOn（）`**实现

### 3.1 功能性操作符subscribeOn（） & observeOn（）简介

- 作用
  线程控制，即指定 被观察者 `（Observable）` / 观察者`（Observer）` 的工作线程类型
- 线程类型
  在 `RxJava`中，内置了多种用于调度的线程类型

| 类型                           |         含义          |                         应用场景 |
| ------------------------------ | :-------------------: | -------------------------------: |
| Schedulers.immediate()         | 当前线程 = 不指定线程 |                             默认 |
| AndroidSchedulers.mainThread() |     Android主线程     |                           操作UI |
| Schedulers.newThread()         |      常规新线程       |                       耗时等操作 |
| Schedulers.io()                |      io操作线程       | 网络请求、读写文件等io密集型操作 |
| Schedulers.computation()       |    CPU计算操作线程    |                     大量计算操作 |

- 注：`RxJava`内部使用 **线程池** 来维护这些线程，所以线程的调度效率非常高。

### 3.2 具体使用

- 具体是在 （上述步骤3）**通过订阅（subscribe）连接观察者和被观察者**中实现



```rust
<-- 使用说明 -->
  // Observable.subscribeOn（Schedulers.Thread）：指定被观察者 发送事件的线程（传入RxJava内置的线程类型）
  // Observable.observeOn（Schedulers.Thread）：指定观察者 接收 & 响应事件的线程（传入RxJava内置的线程类型）

<-- 实例使用 -->
// 步骤3：通过订阅（subscribe）连接观察者和被观察者
        observable.subscribeOn(Schedulers.newThread()) // 1. 指定被观察者 生产事件的线程
                  .observeOn(AndroidSchedulers.mainThread())  // 2. 指定观察者 接收 & 响应事件的线程
                  .subscribe(observer); // 3. 最后再通过订阅（subscribe）连接观察者和被观察者
```

- 测试结果

  ![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm04dfu6o4j30xc05l76s.jpg)

  示意图

- 特别注意

###### 1. 若`Observable.subscribeOn（）`多次指定被观察者 生产事件的线程，则只有第一次指定有效，其余的指定线程无效



```cpp
// 步骤3：通过订阅（subscribe）连接观察者和被观察者
        observable.subscribeOn(Schedulers.newThread()) // 第一次指定被观察者线程 = 新线程
                  .subscribeOn(AndroidSchedulers.mainThread()) // 第二次指定被观察者线程 = 主线程
                  .observeOn(AndroidSchedulers.mainThread())
                  .subscribe(observer);
```

- 测试结果：被观察者的线程 = 第一次指定的线程 = 新的工作线程，第二次指定的线程（主线程）无效

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm04detimcj30w203x40u.jpg)

示意图

###### 2. 若Observable.observeOn（）多次指定观察者 接收 & 响应事件的线程，则每次指定均有效，即每指定一次，就会进行一次线程的切换



```java
// 步骤3：通过订阅（subscribe）连接观察者和被观察者
        observable.subscribeOn(Schedulers.newThread())
                  .observeOn(AndroidSchedulers.mainThread()) // 第一次指定观察者线程 = 主线程
                  .doOnNext(new Consumer<Integer>() { // 生产事件
                    @Override
                    public void accept(Integer integer) throws Exception {
                        Log.d(TAG, "第一次观察者Observer的工作线程是： " + Thread.currentThread().getName());
                    }
                })
                .observeOn(Schedulers.newThread()) // 第二次指定观察者线程 = 新的工作线程
                .subscribe(observer); // 生产事件


// 注：
// 1. 整体方法调用顺序：观察者.onSubscribe（）> 被观察者.subscribe（）> 观察者.doOnNext（）>观察者.onNext（）>观察者.onComplete() 
// 2. 观察者.onSubscribe（）固定在主线程进行
```

- 测试结果：每调用一次`observeOn()`，观察者的线程就会切换一次

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm04ddlatkj30ww04ln02.jpg)

示意图

------

# 4. 具体实例

下面，我将采用最常见的 `Retrofit + RxJava` 实现 网络请求 的功能，从而说明 `RxJava`的线程控制的具体应用

### 4.1 功能说明

- 实现功能：将中文翻译成英文 - > 显示到界面
- 实现方案：采用`Get`方法对 金山词霸API 发送网络请求

> 1. 先切换到工作线程 发送网络请求
> 2. 再切换到主线程进行 `UI`更新

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm04dbnraqj30xc08nwey.jpg)

金山词典

### 4.2 步骤说明

1. 添加依赖
2. 创建 接收服务器返回数据 的类
3. 创建 用于描述网络请求 的接口（区别于传统形式）
4. 创建 Retrofit 实例
5. 创建 网络请求接口实例 并 配置网络请求参数（区别于传统形式）
6. 发送网络请求（区别于传统形式）
7. 发送网络请求
8. 对返回的数据进行处理

> 本实例侧重于说明 `RxJava` 的线程控制，关于`Retrofit`的使用请看文章：[这是一份很详细的 Retrofit 2.0 使用教程（含实例讲解）](https://www.jianshu.com/p/a3e162261ab6)

### 4.3 步骤实现

#### 步骤1： 添加依赖

a. 在 `Gradle`加入`Retrofit`库的依赖

*build.gradle*



```csharp
dependencies {

// Android 支持 Rxjava
// 此处一定要注意使用RxJava2的版本
compile 'io.reactivex.rxjava2:rxjava:2.0.1'
compile 'io.reactivex.rxjava2:rxandroid:2.0.1'

// Android 支持 Retrofit
compile 'com.squareup.retrofit2:retrofit:2.1.0'

// 衔接 Retrofit & RxJava
// 此处一定要注意使用RxJava2的版本
compile 'com.jakewharton.retrofit:retrofit2-rxjava2-adapter:1.0.0'

// 支持Gson解析
compile 'com.squareup.retrofit2:converter-gson:2.1.0'

}
```

b. 添加 网络权限
*AndroidManifest.xml*



```xml
<uses-permission android:name="android.permission.INTERNET"/>
```

##### 步骤2：创建 接收服务器返回数据 的类

- 金山词霸`API` 的数据格式说明如下：



```cpp
// URL模板
http://fy.iciba.com/ajax.php

// URL实例
http://fy.iciba.com/ajax.php?a=fy&f=auto&t=auto&w=hello%20world

// 参数说明：
// a：固定值 fy
// f：原文内容类型，日语取 ja，中文取 zh，英语取 en，韩语取 ko，德语取 de，西班牙语取 es，法语取 fr，自动则取 auto
// t：译文内容类型，日语取 ja，中文取 zh，英语取 en，韩语取 ko，德语取 de，西班牙语取 es，法语取 fr，自动则取 auto
// w：查询内容
```

- 示例

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm04d9pisyj30cn0c440c.jpg)

API格式说明

- 根据 金山词霸API 的数据格式，创建 接收服务器返回数据 的类：

*Translation.java*



```csharp
public class Translation {
    private int status;

    private content content;
    private static class content {
        private String from;
        private String to;
        private String vendor;
        private String out;
        private int errNo;
    }

    //定义 输出返回数据 的方法
    public void show() {
        System.out.println( "Rxjava翻译结果：" + status);
        System.out.println("Rxjava翻译结果：" + content.from);
        System.out.println("Rxjava翻译结果：" + content.to);
        System.out.println("Rxjava翻译结果：" + content.vendor);
        System.out.println("Rxjava翻译结果：" + content.out);
        System.out.println("Rxjava翻译结果：" + content.errNo);
    }
}
```

##### 步骤3：创建 用于描述网络请求 的接口

采用 **注解** + `Observable<...>`接口描述 网络请求参数

*GetRequest_Interface.java*



```java
public interface GetRequest_Interface {

    @GET("ajax.php?a=fy&f=auto&t=auto&w=hi%20world")
    Observable<Translation> getCall();
     // 注解里传入 网络请求 的部分URL地址
    // Retrofit把网络请求的URL分成了两部分：一部分放在Retrofit对象里，另一部分放在网络请求接口里
    // 如果接口里的url是一个完整的网址，那么放在Retrofit对象里的URL可以忽略
    // 采用Observable<...>接口 
    // getCall()是接受网络请求数据的方法
}
```

##### 接下来的步骤均在*MainActivity.java*内实现（请看注释）

*MainActivity.java*



```java
public class MainActivity extends AppCompatActivity {

    private static final String TAG = "Rxjava";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        //步骤4：创建Retrofit对象
        Retrofit retrofit = new Retrofit.Builder()
                .baseUrl("http://fy.iciba.com/") // 设置 网络请求 Url
                .addConverterFactory(GsonConverterFactory.create()) //设置使用Gson解析(记得加入依赖)
                .addCallAdapterFactory(RxJava2CallAdapterFactory.create()) // 支持RxJava
                .build();

        // 步骤5：创建 网络请求接口 的实例
        GetRequest_Interface request = retrofit.create(GetRequest_Interface.class);

        // 步骤6：采用Observable<...>形式 对 网络请求 进行封装
        Observable<Translation> observable = request.getCall();

        // 步骤7：发送网络请求
        observable.subscribeOn(Schedulers.io())               // 在IO线程进行网络请求
                  .observeOn(AndroidSchedulers.mainThread())  // 回到主线程 处理请求结果
                  .subscribe(new Observer<Translation>() {
                    @Override
                    public void onSubscribe(Disposable d) {
                        Log.d(TAG, "开始采用subscribe连接");
                    }

                    @Override
                    public void onNext(Translation result) {
                        // 步骤8：对返回的数据进行处理
                        result.show() ;
                    }

                    @Override
                    public void onError(Throwable e) {
                        Log.d(TAG, "请求失败");
                    }

                    @Override
                    public void onComplete() {
                        Log.d(TAG, "请求成功");
                    }
                });
    }
}
```

### 4.4 测试结果

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm04d6wy9dj30wp04pdk6.jpg)

示意图

### 4.5 Demo地址

[Carson_Ho的Github地址 = RxJava2实战系列：线程控制](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FCarson-Ho%2FRxJava_Operators)

------

# 5. 注意事项

### 5.1 依赖包问题

- 问题说明

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm04d63j6nj30wo03xq6k.jpg)

示意图

- 解决方法
  通过在`Gradle`使用`packageOptions`解决

*build.gradle*



```bash
android {
    ...
    packagingOptions {
        exclude 'META-INF/rxjava.properties'
    }
}
```

### 5.2 应用程序崩溃问题

- 背景：在发送网络请求时 退出当前`Activity`
- 冲突：此时如果回到主线程更新 `UI`，`App`会崩溃
- 解决方案：当 `Activity`退出时，调用 `Disposable.dispose()`切断观察者和被观察者的连接，使得观察者无法收到事件 & 响应事件

> 当出现多个`Disposable`时，可采用`RxJava`内置容器`CompositeDisposable`进行统一管理



```csharp
// 添加Disposable到CompositeDisposable容器
CompositeDisposable.add()

// 清空CompositeDisposable容器
CompositeDisposable.clear() 
```

------

# 6. 总结

- 本文主要对 `Rxjava` 中的线程调度、功能性操作符`subscribeOn（）` & `observeOn（）`进行讲解
- 下面我将继续对 `Android`中 `Rxjava` 的其他知识进行深入讲解 ，感兴趣的同学可以继续关注本人运营的`Wechat Public Account`：
- [我想给你们介绍一个与众不同的Android微信公众号（福利回赠）](https://www.jianshu.com/p/2e92908af6ec)
- [我想邀请您和我一起写Android（福利回赠）](https://www.jianshu.com/p/2c5d57fb054d)

------

# 请点赞！因为你的鼓励是我写作的最大动力！

> **相关文章阅读**
>
> - 操作符使用
>   [Android：这是一篇 清晰 & 易懂的Rxjava 入门教程](https://www.jianshu.com/p/a406b94f3188)
>   [Android RxJava：最基础的操作符详解 - 创建操作符](https://www.jianshu.com/p/e19f8ed863b1)
>   [Android RxJava：图文详解 变换操作符](https://www.jianshu.com/p/904c14d253ba)
>   [Android RxJava：组合 / 合并操作符 详细教程](https://www.jianshu.com/p/c2a7c03da16d)
>   [Android RxJava：功能性操作符 全面讲解](https://www.jianshu.com/p/b0c3669affdb)
> - 实际应用讲解
>   [Android RxJava 实际应用讲解：（无条件）网络请求轮询](https://www.jianshu.com/p/11b3ec672812)
>   [Android RxJava 实际应用讲解：（有条件）网络请求轮询](https://www.jianshu.com/p/dbeaaa4afad5)
>   [Android RxJava 实际应用讲解：网络请求嵌套回调](https://www.jianshu.com/p/5f5d61f04f96)
>   [Android RxJava 实际应用讲解：合并数据源](https://www.jianshu.com/p/fc2e551b907c)
>   [Android RxJava 实际应用讲解：从磁盘 / 内存缓存中 获取缓存数据](https://www.jianshu.com/p/6f3b6b934787)
>   [Android RxJava 实际应用讲解：联合判断](https://www.jianshu.com/p/2becc0eaedab)
>   [Android RxJava：细说 线程控制（切换 / 调度 ）（含Retrofit实例讲解）](https://www.jianshu.com/p/5225b2baaecd)
>   [Android RxJava 实际应用讲解：网络请求出错重连（结合Retrofit）](https://www.jianshu.com/p/508c30aef0c1)



作者：Carson_Ho
链接：https://www.jianshu.com/p/5225b2baaecd
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。