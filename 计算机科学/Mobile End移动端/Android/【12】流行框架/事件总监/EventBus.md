[TOC]

### 一、EventBus 简介

EventBus是一种用于Android的事件发布-订阅总线，由GreenRobot开发，Gihub地址是：[EventBus](https://github.com/greenrobot/EventBus)。它简化了应用程序内各个组件之间进行通信的复杂度，尤其是碎片之间进行通信的问题，可以避免由于使用广播通信而带来的诸多不便。

#### 1、三个角色

1.  **Event**：事件，它可以是任意类型，EventBus会根据事件类型进行全局的通知。
2.  **Subscriber**：事件订阅者，在EventBus 3.0之前我们必须定义以onEvent开头的那几个方法，分别是`onEvent`、`onEventMainThread`、`onEventBackgroundThread`和`onEventAsync`，而在3.0之后事件处理的方法名可以随意取，不过需要加上注解`@subscribe`，并且指定线程模型，默认是`POSTING`。
3.  **Publisher**：事件的发布者，可以在任意线程里发布事件。一般情况下，使用`EventBus.getDefault()`就可以得到一个EventBus对象，然后再调用`post(Object)`方法即可。

#### 2、四种线程模型

EventBus3.0有四种线程模型，分别是：

1.  **POSTING**：默认，表示事件处理函数的线程跟**发布事件**的线程在**同一个线程。**
2.  **MAIN**：表示事件处理函数的线程在**主线程(UI)线程**，因此在这里**不能进行耗时操作。**
3.  **BACKGROUND**：表示事件处理函数的线程在后台线程，因此**不能进行UI操作**。如果发布事件的线程是主线程(UI线程)，那么事件处理函数将会开启一个后台线程，如果果发布事件的线程是在后台线程，那么事件处理函数就使用该线程。
4.  **ASYNC**：表示无论事件发布的线程是哪一个，事件处理函数始**终会新建一个子线程运行，**同样不能进行UI操作。



### 二、基本使用

EventBus的使用非常简单，主要分为3个步骤：

1. 定义事件。
2. 订阅事件。
3. 发布事件。

```java
//订阅和取消订阅
EventBus.getDefault().register(this);
EventBus.getDefault().unregister(this);

//定义事件
public class MessageWrap {

    public final String message;

    public static MessageWrap getInstance(String message) {
        return new MessageWrap(message);
    }

    private MessageWrap(String message) {
        this.message = message;
    }
}
//订阅事件
@Subscribe(threadMode = ThreadMode.MAIN ,sticky = true)
    public void onGetMessage(MessageWrap message) {
        tvEventbusMessage.setText(message.message);
        Log.i("ck","收到的消息： "+message.message);
    }
//发布事件
@OnClick(R.id.btn_eventbus_sent_msg)
    public void onViewClicked() {
        String msg=edtInputMsg.getText().toString();
        EventBus.getDefault().postSticky( MessageWrap.getInstance(msg));
    }
```



### 三、源码分析

#### 1、核心架构

![核心架构](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/rGXUpQRUbhNI48lE53TPqlIVK2Y4STsfPX2LFjWKWQE!/r/dDQBAAAAAAAA)



#### 2、Register

> 订阅的代码主要就做了两件事
>
> - 第一件事是将我们的订阅方法和订阅者封装到subscriptionsByEventType和typesBySubscriber中，subscriptionsByEventType是我们投递订阅事件的时候，就是根据我们的EventType找到我们的订阅事件，从而去分发事件，处理事件的；typesBySubscriber在调用unregister(this)的时候，根据订阅者找到EventType，又根据EventType找到订阅事件，从而对订阅者进行解绑。
> - 第二件事，如果是粘性事件的话，就立马投递、执行。

![](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/tZNrvLB03cbDD8zmz5APqlWRo*.teM4sI5eyhz5NUiU!/r/dFQBAAAAAAAA)

#### 3、Post



![](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/OfzodSN.u0Qn6.M9alkVyx..1Q*U6i9kRFBqe9lS7PU!/r/dC4BAAAAAAAA)

#### 4、UnRegister

> typesBySubscriber我们在订阅者注册的过程中讲到过这个属性，他根据订阅者找到EventType，然后根据EventType和订阅者来得到订阅事件来对订阅者进行解绑。



![](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/nkkLXE0p1rG0Pehs.2a.6jv2Z*0qelGIFnKgpMC.kAI!/r/dFYBAAAAAAAA)

### 参考文章

- [Burgly的EventBus解析](https://www.cnblogs.com/bugly/p/5475034.html)

- [刘望舒的EventBus总结](http://liuwangshu.cn/application/eventbus/2-eventbus-sourcecode.html)