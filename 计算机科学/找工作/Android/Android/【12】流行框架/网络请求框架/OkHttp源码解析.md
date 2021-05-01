[TOC]

### 一、示例

#### 1、GET

```java
//1.http客户端
OkHttpClient client = new OkHttpClient();

String run(String url) throws IOException {
     
//2.构造请求
  Request request = new Request.Builder()
      .url(url)
      .build();
    
//3.执行请求，获取响应数据
  Response response = client.newCall(request).execute();
  return response.body().string();
}
```

#### 2、POST


```java
public static final MediaType JSON
    = MediaType.parse("application/json; charset=utf-8");

OkHttpClient client = new OkHttpClient();

String post(String url, String json) throws IOException {
  RequestBody body = RequestBody.create(JSON, json);
  Request request = new Request.Builder()
      .url(url)
      .post(body)
      .build();
  Response response = client.newCall(request).execute();
  return response.body().string();
}
```

#### 3、几个重要接口

- `OKHttpClient` ： 它代表着 `http` 客户端
- `Request`：它封装了请求对象，可以构造一个 `http` 请求对象
- `Response`：封装了响应结果
- `Call` ： `client.newCall`调用后生成一个请求执行对象 `Call`，它封装了请求执行过程。



### 二、源码

#### 1、Call接口

```java
package okhttp3;

import java.io.IOException;

public interface Call extends Cloneable {
    Request request();
	//同步请求
    Response execute() throws IOException;
	//异步请求
    void enqueue(Callback var1);

    void cancel();

    boolean isExecuted();

    boolean isCanceled();

    Call clone();

    public interface Factory {
        Call newCall(Request var1);
    }
}
```

从源码注释知道， `Call` 是一个准备请求的执行对象，它可以被取消，代表一个 “请求/响应” 对，不能执行两次。



#### 2、RealCall

> `final class RealCall implements Call`该类是Call的实现类。

```java
@Override public Response execute() throws IOException {
    synchronized (this) {
      if (executed) throw new IllegalStateException("Already Executed");
      executed = true;
    }
    captureCallStackTrace();
    eventListener.callStart(this);
    try {
      client.dispatcher().executed(this);	//关注点1#######
      Response result = getResponseWithInterceptorChain();	//关注点2#####
      if (result == null) throw new IOException("Canceled");
      return result;
    } catch (IOException e) {
      eventListener.callFailed(this, e);
      throw e;
    } finally {
      client.dispatcher().finished(this);
    }
  }
```

这个方法也不是很长，逻辑很简单：

- 同步锁检查该请求是否已经执行，如果没有则标记 `executed=ture`，否则抛出异常
- 调用了回调函数 `callStart`
- `okhttp`客户端调用 `dispatcher` 将执行请求对象
- 调用了 `getResponseWithInterceptorChain` 方法获取到响应数据 `Response`，这个方法很重要，后面会继续跟进
- 然后是对请求失败的回调 `callFailed`
- 最后还是使用 `dispather`对象调用 `finished`方法，完成请求

这里的逻辑还是比较清晰的，出现两个重要的方法

1. `dispatcher.execute`
2. `getResponseWithInterceptorChain`



#### 3、Dispatcher

```java
public final class Dispatcher {
  private int maxRequests = 64;
  private int maxRequestsPerHost = 5;
  private @Nullable Runnable idleCallback;

  //线程池
  private @Nullable ExecutorService executorService;
    
 //异步请求等待
  private final Deque<AsyncCall> readyAsyncCalls = new ArrayDeque<>();

  //异步请求执行
  private final Deque<AsyncCall> runningAsyncCalls = new ArrayDeque<>();

  //同步请求执行
  private final Deque<RealCall> runningSyncCalls = new ArrayDeque<>();
  public Dispatcher(ExecutorService executorService) {
    this.executorService = executorService;
  }
    //异步请求
  synchronized void enqueue(AsyncCall call) {
    if (runningAsyncCalls.size() < maxRequests && runningCallsForHost(call) < maxRequestsPerHost) {
      runningAsyncCalls.add(call);
      executorService().execute(call);
    } else {
      readyAsyncCalls.add(call);
    }
  }
    
  //同步请求
  synchronized void executed(RealCall call) {
    runningSyncCalls.add(call);
  }
    
    /** Used by {@code AsyncCall#run} to signal completion. */
  void finished(AsyncCall call) {
    finished(runningAsyncCalls, call, true);
  }

  /** Used by {@code Call#execute} to signal completion. */
  void finished(RealCall call) {
    finished(runningSyncCalls, call, false);
  }

  private <T> void finished(Deque<T> calls, T call, boolean promoteCalls) {
    int runningCallsCount;
    Runnable idleCallback;
    synchronized (this) {
      if (!calls.remove(call)) throw new AssertionError("Call wasn't in-flight!");
      if (promoteCalls) promoteCalls();
      runningCallsCount = runningCallsCount();
      idleCallback = this.idleCallback;
    }

    if (runningCallsCount == 0 && idleCallback != null) {
      idleCallback.run();
    }
  }
}

```

可以看出 `Dispatcher` 是一个调度器，它内部有一个线程池 `executorService` ,还有三个队列，分别代表同步请求进行队列、异步请求等待队列、异步请求执行队列。

我们发现**调用 execute方法时就是将 Call对象加入到同步请求进行队列 runningSyncCalls中，而调用 finished 方法则是将 Call请求从队列中移除**



#### 4、getResponseWithInterceptorChain()

```java
Response getResponseWithInterceptorChain() throws IOException {
    // Build a full stack of interceptors.
    List<Interceptor> interceptors = new ArrayList<>();
    //添加程序员自定义的的拦截器
    interceptors.addAll(client.interceptors());
    //重试和重定向拦截器
    interceptors.add(retryAndFollowUpInterceptor);
    //处理cookie的拦截器
    interceptors.add(new BridgeInterceptor(client.cookieJar()));
    //处理缓存的拦截器
    interceptors.add(new CacheInterceptor(client.internalCache()));
    //负责连接的拦截器
    interceptors.add(new ConnectInterceptor(client));
    if (!forWebSocket) {
        //添加程序员自定义的network拦截器
      interceptors.addAll(client.networkInterceptors());
    }
    //调用服务拦截器
    interceptors.add(new CallServerInterceptor(forWebSocket));
	
    //构造一个责任链
    Interceptor.Chain chain = new RealInterceptorChain(interceptors, null, null, null, 0,
        originalRequest, this, eventListener, client.connectTimeoutMillis(),
        client.readTimeoutMillis(), client.writeTimeoutMillis());

    return chain.proceed(originalRequest);
  }
```

在添加了一系列的拦截器之后，又构造了一个拦截器责任链，这个 `RealInterceptorChain` 包含了所有的拦截器对象。然后调用 `chain.proceed`方法开始执行请求，这时就到了 `RealInterceptorChain` 这个类中。



#### 5、RealInterceptorChain

```java
@Override public Response proceed(Request request) throws IOException {
    return proceed(request, streamAllocation, httpCodec, connection);
  }

public Response proceed(Request request, StreamAllocation streamAllocation, HttpCodec httpCodec,
      RealConnection connection) throws IOException {
    if (index >= interceptors.size()) throw new AssertionError();

    calls++;

    //省略...
   
	//1. 执行拦截器责任链中的下一个拦截器
    RealInterceptorChain next = new RealInterceptorChain(interceptors, streamAllocation, httpCodec,
        connection, index + 1, request, call, eventListener, connectTimeout, readTimeout,
        writeTimeout);
    //2. 获取当前的拦截器
    Interceptor interceptor = interceptors.get(index);
    //3. 执行拦截，并返回响应
    Response response = interceptor.intercept(next);

    //省略......
    return response;
  }
```

可以看到，在 `proceed`方法，又构造了 `RealInterceptorChain`并且调用了 `interceptor.intercept`方法，

而这个方法中又会调用 `next.proceed`方法，直至返回 `response`。这个过程有点像递归调用。



#### 6、Interceptor

拦截器，它是一个接口，内部还有一个 `Chain`接口

所有的拦截器都需要实现这个接口。

```java
public interface Interceptor {
  Response intercept(Chain chain) throws IOException;
  interface Chain {
    Request request();
    Response proceed(Request request) throws IOException;
    /**
     * Returns the connection the request will be executed on. This is only available in the chains
     * of network interceptors; for application interceptors this is always null.
     */
    @Nullable Connection connection();
    Call call();
    int connectTimeoutMillis();
    Chain withConnectTimeout(int timeout, TimeUnit unit);
    int readTimeoutMillis();
    Chain withReadTimeout(int timeout, TimeUnit unit);
    int writeTimeoutMillis();
    Chain withWriteTimeout(int timeout, TimeUnit unit);
  }
}
```



### 三、异步情况

#### 1、`RealCall`中的 `enqueue`方法

```java
  @Override public void enqueue(Callback responseCallback) {
    synchronized (this) {
      if (executed) throw new IllegalStateException("Already Executed");
      executed = true;
    }
    captureCallStackTrace();
    eventListener.callStart(this);
    //最终执行力度dispatcher的enqueue方法
    client.dispatcher().enqueue(new AsyncCall(responseCallback));
  }
```

#### 2、Diapatcher的enqueue方法

```java
 synchronized void enqueue(AsyncCall call) {
    if (runningAsyncCalls.size() < maxRequests && runningCallsForHost(call) < maxRequestsPerHost) {
      runningAsyncCalls.add(call);
      executorService().execute(call);
    } else {
      readyAsyncCalls.add(call);
    }
  }
```

在 `dispatcher`中通过线程池来执行 `AsyncCall`对象，因此跟进到 `AsyncCall`中的 `execute`方法

#### 3、AsyncCall的exexute方法

```java
@Override protected void execute() {
      boolean signalledCallback = false;
      try {
        //#################################################  
       //最终还是调用了getResponseWithInterceptorChain()！！！
       //##################################################
        Response response = getResponseWithInterceptorChain();
        if (retryAndFollowUpInterceptor.isCanceled()) {
          signalledCallback = true;
          responseCallback.onFailure(RealCall.this, new IOException("Canceled"));
        } else {
          signalledCallback = true;
          responseCallback.onResponse(RealCall.this, response);
        }
      } catch (IOException e) {
        if (signalledCallback) {
          // Do not signal the callback twice!
          Platform.get().log(INFO, "Callback failure for " + toLoggableString(), e);
        } else {
          eventListener.callFailed(RealCall.this, e);
          responseCallback.onFailure(RealCall.this, e);
        }
      } finally {
        client.dispatcher().finished(this);
      }
    }
  }
```

**发现最终还是执行了 getResponseWithInterceptorChain**，因此不管是同步还是异步、最终的流程还是一样。



### 四、总结

- `OKHttpClient`

  这是一个 `http` 客户端。构建很简单，可以使用无参构造函数。其内部是通过 `Builder` 对象进行构建的。也可以通过其内部静态类 `Builder` 来构建，然后通过 `builder` 设置 `OkHttpClient` 构造参数。

- `Request`

  请求对象。其内部也是使用 `Builder` 模式封装了构造的过程，通过 `Builder`使用链式调用也是目前很多开源库中常见的模式。

- `Response`

  响应结果。客户端执行后返回响应结果，通过 `Response` 可以很方便的获取到响应数据。

- `Call`

  请求执行。可以执行同步或者异步的请求，分别将请求发送到 `dispatcher`

- `Dispatcher`

  调度器。其内部有一个==线程池==，并维护了三个队列：==同步进行请求队列、异步请求等待队列、异步请求进行队列==。

- 还有两个重要的方法 `execute`和 `enqueue`方法，分别代表同步、异步的方法。这两个方法的最终的执行流程都是一样的

- `Interceptor`

  拦截器。拦截器在 `OKHttpClient`中使是用==责任链模式==来实现的。 `Okhttp` 中的关键的流程是==通过拦截器责任链来完成的==。



### 五、请求流程图

<img src="http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/hQnecanZcu8T6.zBXNY0S2SyY57N6n0WpQfeXc7DZ1o!/r/dDQBAAAAAAAA" style="zoom:50%;" />

