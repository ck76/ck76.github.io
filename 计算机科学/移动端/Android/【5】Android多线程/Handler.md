[TOC]

在Android开发中，我们经常会遇到这样一种情况：在UI界面上进行某项操作后要执行一段很耗时的代码，比如我们在界面上点击了一个”下载“按钮，那么我们需要执行网络请求，这是一个耗时操作，因为不知道什么时候才能完成。为了保证不影响UI线程，所以我们会创建一个新的线程去执行我们的耗时的代码。当我们的耗时操作完成时，我们需要更新UI界面以告知用户操作完成了。

## 一、定义

一套 `Android` 消息传递机制 

> 如果你想要让一个Android的应用程序反应灵敏，那么你必须防止它的UI线程被阻塞。同样地，将这些阻塞的或者计算密集型的任务转到工作线程去执行也会提高程序的响应灵敏性。然而，这些任务的执行结果通常需要重新更新UI组件的显示，但该操作只能在UI线程中去执行。有一些方法解决了UI线程的阻塞问题，例如阻塞对象，共享内存以及管道技术。Android为了解决这个问题，提供了一种自有的消息传递机制——Handler。Handler是Android Framework架构中的一个基础组件，它实现了一种非阻塞的消息传递机制，在消息转换的过程中，消息的生产者和消费者都不会阻塞。



## 二、作用

在多线程的应用场景中，**将工作线程中需更新UI的操作信息 传递到 UI主线程**，从而实现 工作线程对`UI`的更新处理，最终实现异步消息的处理 。



## 三、为什么要用 `Handler`消息传递机制

- 答：**多个线程并发更新UI的同时 保证线程安全**
- 具体描述如下

  ![Handler2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/mwyEGu8B1tLnklIM*v3KYYSyp31cnM*GwQWN.5s1lk8!/r/dDIBAAAAAAAA)

## 四、相关概念

关于 `Handler` 异步通信机制中的相关概念如下： 

​	![Handler3](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/yELBsjZrAP3JSOD8oQFWOtLhxeplBr0mEuPFvqRXTTY!/r/dFMBAAAAAAAA)

----

![相关方法](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/TJfWUWT3sU5chk1OPKxIjJ3sSpNkJl6E54LBGvrvO5s!/r/dFMBAAAAAAAA)



## 五、使用方式

### 1.使用 Handler.sendMessage()

```java
/** 
  * 方式1：新建Handler子类（内部类）
  */

    // 步骤1：自定义Handler子类（继承Handler类） & 复写handleMessage（）方法
    class mHandler extends Handler {

        // 通过复写handlerMessage() 从而确定更新UI的操作
        @Override
        public void handleMessage(Message msg) {
         ...// 需执行的UI操作
            
        }
    }

    // 步骤2：在主线程中创建Handler实例
        private Handler mhandler = new mHandler();

    // 步骤3：创建所需的消息对象
        Message msg = Message.obtain(); // 实例化消息对象
        msg.what = 1; // 消息标识
        msg.obj = "AA"; // 消息内容存放

    // 步骤4：在工作线程中 通过Handler发送消息到消息队列中
    // 可通过sendMessage（） / post（）
    // 多线程可采用AsyncTask、继承Thread类、实现Runnable
        mHandler.sendMessage(msg);

    // 步骤5：开启工作线程（同时启动了Handler）
    // 多线程可采用AsyncTask、继承Thread类、实现Runnable


/** 
  * 方式2：匿名内部类
  */
   // 步骤1：在主线程中 通过匿名内部类 创建Handler类对象
            private Handler mhandler = new  Handler(){
                // 通过复写handlerMessage()从而确定更新UI的操作
                @Override
                public void handleMessage(Message msg) {
                        ...// 需执行的UI操作
                    }
            };

  // 步骤2：创建消息对象
    Message msg = Message.obtain(); // 实例化消息对象
  msg.what = 1; // 消息标识
  msg.obj = "AA"; // 消息内容存放
  
  // 步骤3：在工作线程中 通过Handler发送消息到消息队列中
  // 多线程可采用AsyncTask、继承Thread类、实现Runnable
   mHandler.sendMessage(msg);

  // 步骤4：开启工作线程（同时启动了Handler）
  // 多线程可采用AsyncTask、继承Thread类、实现Runnable
```



### 2.使用Handler.post()

```java
// 步骤1：在主线程中创建Handler实例
    private Handler mhandler = new Handler();

    // 步骤2：在工作线程中 发送消息到消息队列中 & 指定操作UI内容
    // 需传入1个Runnable对象
    mHandler.post(new Runnable() {
            @Override
            public void run() {
                ... // 需执行的UI操作 
            }

    });

    // 步骤3：开启工作线程（同时启动了Handler）
    // 多线程可采用AsyncTask、继承Thread类、实现Runnable
```



### 3.具体使用

```java
public class MainActivity extends AppCompatActivity {
    
    public TextView mTextView;
    public Handler mHandler;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        mTextView = (TextView) findViewById(R.id.show);

        // 步骤1：在主线程中 通过匿名内部类 创建Handler类对象
        mHandler = new Handler(){
            // 通过复写handlerMessage()从而确定更新UI的操作
            @Override
            public void handleMessage(Message msg) {
                // 根据不同线程发送过来的消息，执行不同的UI操作
                switch (msg.what) {
                    case 1:
                        mTextView.setText("执行了线程1的UI操作");
                        break;
                    case 2:
                        mTextView.setText("执行了线程2的UI操作");
                        break;
                }
            }
        };
        // 采用继承Thread类实现多线程演示
        new Thread() {
            @Override
            public void run() {
                try {
                    Thread.sleep(3000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                 // 步骤3：创建所需的消息对象
                 Message msg = Message.obtain();
                 msg.what = 1; // 消息标识
                 msg.obj = "A"; // 消息内存存放

                 // 步骤4：在工作线程中 通过Handler发送消息到消息队列中
                 mHandler.sendMessage(msg);
            }
        }.start();
        // 步骤5：开启工作线程（同时启动了Handler）

        // 此处用2个工作线程展示
        new Thread() {
            @Override
            public void run() {
                try {
                    Thread.sleep(6000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                // 通过sendMessage（）发送
                 // a. 定义要发送的消息
                 Message msg = Message.obtain();
                 msg.what = 2; //消息的标识
                 msg.obj = "B"; // 消息的存放
                 // b. 通过Handler发送消息到其绑定的消息队列
                 mHandler.sendMessage(msg);
            }
        }.start();

    }

}
```



### 4.Handler的post和sendMessage的区别

post和sendMessage功能其实差不多，**post其实也是通过sendMessage来实现的**，都是发送消息到**Handler所在**的线程的消息队列中
post的用法更方便，经常会post一个Runnable，处理的代码直接写在Runnable的run方法中，其实就是将这个Runnable发送到Handler所在线程（一般是主线程）的消息队列中。sendMessage方法主线程处理方法一般则是写在handleMessage中。代码示例如下：
**post**

```java
final Handler handler = new Handler();
new Thread(new Runnable() {
            @Override
            public void run() {
                final String response = get(url);
                handler.post(new Runnable() {
                    @Override
                    public void run() {
                        //doSomeThing
                    }
                });
            }
        }).start();
```



**sendMessage**

```java
final Handler handler = new Handler(){
            @Override
            public void handleMessage(Message msg) {
                super.handleMessage(msg);
                if (msg.what == 1) {
                    //doSomeThing
                }
            }
        };
    new Thread(new Runnable() {
        @Override
        public void run() {
            Message msg = new Message();
            msg.what = 1;
            handler.sendMessage(msg);
        }
    });
```

**view.post和handler.post区别**

view.post其实内部是获取到了view所在线程（即ui线程）的handler，并且调用了handler的post方法

![post](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/AlTRUL4QLR01C4hxCZIarXPJdwTm0cDCxBbRIEL9qYA!/r/dL4AAAAAAAAA)

---

![sendmessage](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/wqtv3g6ud6OgA3UFAlwlT7Y4y9dxW29.qVB9TFrje90!/r/dFQBAAAAAAAA)

---



### 5.写在主线程和子线程区别

- 在主线程中,因为系统已经初始化了一个Looper对象,所以我们直接创建Handler对象,就可以进行信息的发送与处理了！ 
- 如果是Handler写在了子线程中的话,我们就需要自己创建一个Looper对象了!创建的流程如下: 
  - 直接调用Looper.prepare()方法即可为当前线程创建Looper对象,而它的构造器会创建配套的MessageQueue; 
  - 创建Handler对象,重写handleMessage( )方法就可以处理来自于其他线程的信息了! 
  - 调用Looper.loop()方法启动Looper 



## 六、工作原理

`Handler`机制的工作流程主要包括4个步骤：

1. 异步通信准备
2. 消息发送
3. 消息循环
4. 消息处理

### 1.工作流程解析

​	![Handler4](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/PpQxRlKIiQu3f0zhWiNQnm0gY1CT2q4vBkq*C8uBfgY!/r/dDcBAAAAAAAA)



### 2.工作流程图

​	![Handler5](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/qP1EWsyAwuDZ.Svw1V64tgSDrIB5bcGlr6pB1ukx.Tg!/r/dDIBAAAAAAAA)



### 3.示意图

![Handler6](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/kzMe5uWx8M4XEjZ4.yC49cWX32PQ0pp7ugljQIxPw9w!/r/dDQBAAAAAAAA)



### 4.特别注意

![Handler7](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/4dtst4VUeBGFzkZvKD3vSzJmJUf59ydmNWAql3iRh40!/r/dFIBAAAAAAAA)

线程`（Thread）`、循环器`（Looper）`、处理者`（Handler）`之间的对应关系如下：

- 1个线程`（Thread）`只能绑定 1个循环器`（Looper）`，但可以有多个处理者`（Handler）` 
- 1个循环器`（Looper）` 可绑定1个处理者`（MessageQueue）` 
- 1个循环器`（Looper）` 可绑定多个处理者`（Handler）` 
- 1个处理者`（Handler）` 只能绑定1个循环器`（Looper）`



### 一图胜千言

![handler](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Immqw*.p5FYkG7Xykm5TTRxPd5q5kSKSzg6.zF9.Wmk!/r/dFQBAAAAAAAA)



## 七、Handler机制的思考

- 先提一个问题哈，如果让你设计一个操作系统，你会怎么设计？

> 我们一般操作系统都会有一个消息系统，里面有个死循环，不断的轮训处理其他各种输入设备输入的信息，比如你在在键盘的输入，鼠标的移动等。这些输入信息最终都会进入你的操作系统，然后由操作系统的内部轮询机制挨个处理这些信息。

- 那Android系统那？它内部是怎么实现的? 如果让你设计，你会怎么设计？

> 1 设计一个类，里面有一个死循环去做循环操作；
>
> 2 用一个类来抽象代表各种输入信息/消息；这个信息/消息应该还有一个唯一标识符字段；如果这个信息里面有个对象来保存对应的键值对；方便其他人往这个信息/消息 存放信息；这个信息/消息应该有个字段标明消息产生的时间；
>
> 3 而上面的这些 信息/消息 又组成一个集合。常用集合很多，那是用ArrayList好还是LinkedList或者Map好那？因为前面说了是一个死循环去处理，所以这个集合最好是"线性和排序的"比较好，因为输入有先后，一般都是按照输入的时间先后来构成。既然这样就排除了Map，那么就剩下来了ArrayList和LinkedList。我们知道一个操作系统的事件是很多的，也就是说对应的信息/消息很多，所以这个集合肯定会面临大量的"插入"操作，而在"插入"效能这块，LinkedList有着明显的优势，所以这个集合应该是一个链表，但是链表又可以分为很多种，因为是线性排序的，所以只剩下"双向链表"和"单向链表”，但是由于考虑下手机的性能问题，大部分人肯定会倾向于选择"单向链表"，因为"单项链表"在增加和删除上面的复杂度明显低于"双向链表"。
>
> 4、最后还应该有两个类，一个负责生产这个输入信息，一个负责消费这些信息。因为涉及到消费端，所以上面2中说的信息/消息应该有一个字段负责指向消费端。

**经过上面的思考，大家是不是发现和其实我们Handler的机制基本上一致。Looper负责轮询；Message代表消息，为了区别对待，用what来做为标识符，when表示时间，data负责存放键值对；MessageQueue则代表Message的集合，Message内部同时也是单项链表的。通过上面的分析，希望大家对Handler机制的总体设计有不一样的感悟。**



## 链接

https://blog.csdn.net/iispring/article/details/47180325

https://www.jianshu.com/p/b4d745c7ff7a