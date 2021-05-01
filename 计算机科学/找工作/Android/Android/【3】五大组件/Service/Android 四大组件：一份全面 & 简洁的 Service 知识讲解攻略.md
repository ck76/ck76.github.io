[TOC]

# 前言

- `Service`作为 `Android`四大组件之一，应用非常广泛
- 本文将提供一份全面 & 简洁的 `Service`知识讲解攻略，希望你们会喜欢

------

# 目录

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgp1gpf6j30f80ec0u7.jpg)

目录

------

# 1. 简介

- 定义：服务，是`Android`四大组件之一， 属于 **计算型组件**
- 作用：提供 需在后台长期运行的服务

> 如：复杂计算、音乐播放、下载等

- 特点：无用户界面、在后台运行、生命周期长

------

# 2. 生命周期

具体请文章：[Android：Service生命周期最全面解析](https://www.jianshu.com/p/8d0cde35eb10)

------

# 3. 类型

`Service`可按照运行地点、运行类型 & 功能进行分类，具体如下：

### 3.1 具体分类

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgp07220j30xc0i9gnq.jpg)

示意图

### 3.2 详细介绍

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwgoyfqgnj30u00ukjwb.jpg)

示意图

------

# 4. 使用讲解

- 下面，我将介绍每种`Service`的具体使用
- 具体请看文章：[Android：（本地、可通信的、前台、远程）Service使用全面介绍](https://www.jianshu.com/p/e04c4239b07e)

------

# 5. 其他思考

### 5.1 Service 与 Thread的区别

- 结论：`Service` 与 `Thread` 无任何关系
- 之所以有不少人会把它们联系起来，主要因为`Service`的后台概念

> 后台：后台任务运行完全不依赖`UI`，即使Activity被销毁 / 程序被关闭，只要进程还在，后台任务就可继续运行

- 关于二者的异同，具体如下图：

![img](https://upload-images.jianshu.io/upload_images/944365-ad8ff95781d19451.png?imageMogr2/auto-orient/strip|imageView2/2/w/910)

示意图

- 注：一般会将 `Service` 和 `Thread`联合着用，即在`Service`中再创建一个子线程（工作线程）去处理耗时操作逻辑，如下代码：



```java
@Override  
public int onStartCommand(Intent intent, int flags, int startId) {  
//新建工作线程
    new Thread(new Runnable() {  
        @Override  
        public void run() {  
            // 开始执行后台任务  
        }  
    }).start();  
    return super.onStartCommand(intent, flags, startId);  
}  
  
class MyBinder extends Binder {  
    public void service_connect_Activity() {  
  //新建工作线程
        new Thread(new Runnable() {  
            @Override  
            public void run() {  
                // 执行具体的下载任务  
            }  
        }).start();  
    }  
  
}  
```

### 5.2 Service和IntentService的区别

具体请看文章：[Android多线程：IntentService用法&源码](https://www.jianshu.com/p/8a3c44a9173a)

------

# 6.总结

- 本文 全面解析了 `Service`的所有知识（含：基础认识、生命周期、使用 & 应用场景）
- 若还想了解关于`Service`的其他知识，请浏览文章：

> [Android四大组件：Service史上最全面解析](https://www.jianshu.com/p/d963c55c3ab9)
> [Android：Service生命周期最全面解析](https://www.jianshu.com/p/8d0cde35eb10)
> [Android：（本地、可通信的、前台、远程）Service使用全面介绍](https://www.jianshu.com/p/e04c4239b07e)
> [Android：远程服务Service（含AIDL & IPC讲解）](https://www.jianshu.com/p/34326751b2c6)
> [Android多线程全面解析：IntentService用法&源码](https://www.jianshu.com/p/8a3c44a9173a)

- 接下来，会继续介绍`Android`开发中的相关知识，感兴趣的同学可以继续关注本人运营的`Wechat Public Account`：
- [我想给你们介绍一个与众不同的Android微信公众号（福利回赠）](https://www.jianshu.com/p/2e92908af6ec)
- [我想邀请您和我一起写Android（福利回赠）](https://www.jianshu.com/p/2c5d57fb054d)

------

#### 请点赞！因为你们的赞同/鼓励是我写作的最大动力！

> **相关文章阅读**
> [Android开发：最全面、最易懂的Android屏幕适配解决方案](https://www.jianshu.com/p/ec5a1a30694b)
> [Android事件分发机制详解：史上最全面、最易懂](https://www.jianshu.com/p/38015afcdb58)
> [Android开发：史上最全的Android消息推送解决方案](https://www.jianshu.com/p/b61a49e0279f)
> [Android开发：最全面、最易懂的Webview详解](https://www.jianshu.com/p/3c94ae673e2a)
> [Android开发：JSON简介及最全面解析方法!](https://www.jianshu.com/p/b87fee2f7a23)
> [Android四大组件：Service服务史上最全面解析](https://www.jianshu.com/p/d963c55c3ab9)
> [Android四大组件：BroadcastReceiver史上最全面解析](https://www.jianshu.com/p/ca3d87a4cdf3)

------

### 



作者：Carson_Ho
链接：https://www.jianshu.com/p/d963c55c3ab9
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。