大佬问我: notify()是随机唤醒线程么?

我的内心戏: 这不是显而易见么! 肯定是啊! jdk关于notify()注释都写的很清楚!

不过这么简单的问题?

机智如我, 决定再次装小小白, 回答: 不是!

大佬: 很好, 小伙子你真的让我刮目相看了!!

我:

大佬: 说说为什么?

我: ………………

牢不可破的知识点被大佬一问, 瞬间感觉哪里有点问题!

于是, 咸鱼君开启了求证模式.

(大佬问我不懂的也就算了, 问这种“共识”的, 我一定举出例子驳倒他!)

#### 代码求证

身为码农, 我决定写代码先验证下!



```csharp
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class NotifyTest{

    //等待列表, 用来记录等待的顺序
    private static List<String> waitList = new LinkedList<>();
    //唤醒列表, 用来唤醒的顺序
    private static List<String> notifyList = new LinkedList<>();

    private static Object lock = new Object();


    public static void main(String[] args) throws InterruptedException{

        //创建50个线程
        for(int i=0;i<50;i++){
            String threadName = Integer.toString(i);
            new Thread(() -> {
                synchronized (lock) {
                    String cthreadName = Thread.currentThread().getName();
                    System.out.println("线程 ["+cthreadName+"] 正在等待.");
                    waitList.add(cthreadName);
                    try {
                        lock.wait();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    System.out.println("线程 ["+cthreadName+"] 被唤醒了.");
                    notifyList.add(cthreadName);
                }
            },threadName).start();
            
            TimeUnit.MILLISECONDS.sleep(50);
        }

        TimeUnit.SECONDS.sleep(1);

        for(int i=0;i<50;i++){
            synchronized (lock) {
                lock.notify();
                TimeUnit.MILLISECONDS.sleep(10);
            }
        }
        TimeUnit.SECONDS.sleep(1);
        System.out.println("wait顺序:"+waitList.toString());
        System.out.println("唤醒顺序:"+notifyList.toString());
    }
}
```

代码很简单, 创建了50个线程, 对其wait()和notify(), 同时使用waitList和notifyList来记录各自的顺序!

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm7g40jiz5j30p003ewgf.jpg)

image.png

没任何悬念, 结果不就是证明了notify()是随机唤醒线程的么?!!

我信心爆棚, 喊着大佬来看(虽然没啥炫耀的, 但是能圆下“指导”大佬的梦想!)

大佬看了下代码, 然后看着我, 微微一笑

我内心一慌: 难道有问题?

只见大佬默默的拿起我的鼠标, 剪切,粘贴了一行代码



```csharp
for(int i=0;i<50;i++){
   synchronized (lock) {
       lock.notify();
       //大佬把这行代码移出了synchronized{}
       //TimeUnit.MILLISECONDS.sleep(10);
    }
   TimeUnit.MILLISECONDS.sleep(10);
  }
TimeUnit.SECONDS.sleep(1);
```

大佬再次微微一笑: 你再跑跑看!

看到大佬的自信从容, 我越来越慌,

赶紧运行下



```css
wait顺序:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
唤醒顺序:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
```

看到这不可置信的结果, 我彻底慌了

什么?!! 这到底这么回事? 改动了一行代码, notify()居然有序了?!!!

看着结果, 我沉思, 连大佬走了都没注意.

究竟哪里出了问题? 难道notify()真是有序唤醒的?

#### 代码问题分析

我们先分析下求证的代码.

只是移动了下sleep()语句, 结果居然天差地别?!

其实问题就出在sleep()上, 准确的是sleep()在synchronized里面还是外面.

** 当我们执行notify之后,由于sleep在symchronized内部, 因此没有释放锁!**

lock.wait后 被通知到的线程，就会进入waitSet队列;
之后我们循环时



```csharp
for(int i=0;i<50;i++){
   synchronized (lock) {
     lock.notify();
     TimeUnit.MILLISECONDS.sleep(10);
   }
  TimeUnit.SECONDS.sleep(1);
}
```

lock.notify();去唤醒等待线程, 我们假设唤醒了线程A;

但是因为后面还要执行TimeUnit.MILLISECONDS.sleep(10)所以lock锁并没有被释放!

当TimeUnit.MILLISECONDS.sleep(10)执行完毕后, lock被释放,

此时被唤醒的线程A想获取lock,

但是我们的for循环中synchronized (lock)也想继续获取lock,

于是两者发生了锁竞争.

由于synchronized实际上不是公平锁，其锁竞争的机制具有随机性

这就导致了最终, 我们看到的结果好像是随机的!

当我们把TimeUnit.MILLISECONDS.sleep(10);移出synchronized同步块后



```csharp
for(int i=0;i<50;i++){
    synchronized (lock) {
       lock.notify();
     }
    TimeUnit.MILLISECONDS.sleep(10);
  }
TimeUnit.SECONDS.sleep(1);
```

lock锁立即被释放了,

并且紧跟的 TimeUnit.SECONDS.sleep(1)确保被唤醒的线程能够获得lock锁立刻执行,

所以, 我们看到的结果才是正确的!

#### 理论求证

想通了代码, 得到了“notify是顺序唤醒”的结果后,

不禁疑惑,

既然“notify是顺序唤醒”的, 那为什么广为流传的, 深入人心的确实“notify()是随机唤醒线程”,

**JDK开发大佬不可能犯这样的错吧?!**

带着这样的疑惑, 咸鱼君选择了看JDK源码来求证!

这里以常用的JDK1.8源码为例

找到“notify()”源码, 看到了这段源码注释

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm7g3xrnouj30xc07vteg.jpg)

image.png

翻译一下, 大致意思就是:

> notify在源码的注释中说到notify选择唤醒的线程是任意的，但是依赖于具体实现的jvm.

看完后, 顿时茅塞顿开!

我们都知道, JVM有很多实现, 比较流行的就是hotspot!

带着质疑, 我们不妨接下去看看jdk1.8, hotspot中对于notify()究竟是如何实现的

synchronized的wait和notify是位于ObjectMonitor.cpp中

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm7g3w9wn1j30s40kw12p.jpg)

image.png

notify过程调用的是DequeueWaiter方法：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm7g3v13fbj30ro08egoy.jpg)

image.png

这里实际上是将_WaitSet中的第一个元素进行出队操作,
这也说明了notify是个顺序操作, 具有公平性.

看完源码, 我们不难得出结论,

原来hotspot对notofy()的实现并不是我们以为的随机唤醒, 而是“先进先出”的顺序唤醒!



作者：tracy_668
链接：https://www.jianshu.com/p/3bba64487922
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。