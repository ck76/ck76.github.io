[TOC]

#### Synchronized底层是如何实现的？什么是锁的升级降级

- 偏向锁、轻量级锁、重量级锁
- 监视对象monitor，字节码monitorenter、monitorexit



#### synchronized 和ReentrantLock的区别

- 见《码出高效》



#### synchronized和volatile的区别

- synchronized可修饰变量、方法和类，而volatile只能修饰变量

- synchronized可能会造成线程阻塞，而volatile不会造成线程的阻塞
- volatile保证可见性、有序性、不保证原子性volatile都保证
- 讲一下Java内存模型以及原子性、可见性、有序性
- 为什么不是原子性，为什么又是可见性和有序性
- 读写内存屏障，禁止指令重排，指令重拍时不能把后面的指令重新排序到内存屏障之前的位置
- 编译时**指令重排**通过JVM内存屏障约束，运行时依靠CPU屏障指令来阻止重排。
- **可见性**的保证是基于CPU的内存屏障指令。
- 对于long和double的特殊规则
- 顺带可以讲一下DCL双重检查锁
- JVM底层通过监视锁来实现synchronized同步的，监视即monitor，是每个对象与生俱来的一个隐藏字段，使用synchronized时，JVM会找到对象的monitor，再根据monitor的状态进行加锁，解锁的判断

- 什么情况下使用volatile，什么情况下使用synchronized



#### 原子性、可见性、有序性

- 原子性：整个程序中的所有操作，要么全部完成，要么全部不完成，不可能停滞在中间某个环节。

- 可见性：指当前一个线程修改了共享变量的值，其他线程能够理解得知这个修改，在变量读取前从主内存刷新变量值这种依赖主内存作为传递媒介的方式实现可见性，无论是普通变量还是volatile变量都是如此，普通变量与volatile变量的区别就是volition变量的特殊规则保证新值能立即同步到主内存，以及每次用前立即从主内存刷新，synchronized和final关键字同样保证可见性

- 有序性：happens-before保证天然有序性可以总结为一句话：如果在本地线程内观察，所有的操作都是有序的，如果在另一个县城内观察另一个线程，所有的操作都是无序的，前半句：县城内表现为串行

  后半句：指令重拍和工作内存与主内存的同步延迟现象



#### 先行发生原则(happens-before)

- Java内存模型中定义的两项操作间的偏序关西
- 如果说操作A发生于操作B之前，其实就是说在发生操作B之前操作A产生的影响能被操作B观察到
- 影响：修改了内存中共享变量的值，发送了消息，调用了方法等



#### 怎么强制停止一个线程

- https://blog.csdn.net/zhanjichun_2008/article/details/6612980
- stop和interrupt



#### 怎么避免死锁？

- https://www.cnblogs.com/bopo/p/9228834.html



#### 链接

- [一个优秀的实习生总结](https://www.jianshu.com/p/2dd855aa1938)

- http://teachcourse.cn/2613.html



# [Java多线程学习之wait、notify/notifyAll 详解](https://www.cnblogs.com/moongeek/p/7631447.html)

1、wait()、notify/notifyAll() 方法是Object的本地final方法，无法被重写。

2、wait()使当前线程阻塞，前提是 必须先获得锁，一般配合synchronized 关键字使用，即，一般在synchronized 同步代码块里使用 wait()、notify/notifyAll() 方法。

3、 由于 wait()、notify/notifyAll() 在synchronized 代码块执行，说明当前线程一定是获取了锁的。

当线程执行wait()方法时候，会释放当前的锁，然后让出CPU，进入等待状态。

只有当 notify/notifyAll() 被执行时候，才会唤醒一个或多个正处于等待状态的线程，然后继续往下执行，直到执行完synchronized 代码块的代码或是中途遇到wait() ，再次释放锁。

也就是说，notify/notifyAll() 的执行只是唤醒沉睡的线程，而不会立即释放锁，锁的释放要看代码块的具体执行情况。所以在编程中，尽量在使用了notify/notifyAll() 后立即退出临界区，以唤醒其他线程让其获得锁

4、wait() 需要被try catch包围，以便发生异常中断也可以使wait等待的线程唤醒。

5、notify 和wait 的顺序不能错，如果A线程先执行notify方法，B线程在执行wait方法，那么B线程是无法被唤醒的。

6、notify 和 notifyAll的区别

notify方法只唤醒一个等待（对象的）线程并使该线程开始执行。所以如果有多个线程等待一个对象，这个方法只会唤醒其中一个线程，选择哪个线程取决于操作系统对多线程管理的实现。notifyAll 会唤醒所有等待(对象的)线程，尽管哪一个线程将会第一个处理取决于操作系统的实现。如果当前情况下有多个线程需要被唤醒，推荐使用notifyAll 方法。比如在生产者-消费者里面的使用，每次都需要唤醒所有的消费者或是生产者，以判断程序是否可以继续往下执行。

7、在多线程中要测试某个条件的变化，使用if 还是while？

　　要注意，notify唤醒沉睡的线程后，线程会接着上次的执行继续往下执行。所以在进行条件判断时候，可以先把 wait 语句忽略不计来进行考虑；显然，要确保程序一定要执行，并且要保证程序直到满足一定的条件再执行，要使用while进行等待，直到满足条件才继续往下执行。如下代码：

```java
public class K {
    //状态锁
    private Object lock;
    //条件变量
    private int now,need;
    public void produce(int num){
        //同步
        synchronized (lock){
           //当前有的不满足需要，进行等待，直到满足条件
            while(now < need){
                try {
                    //等待阻塞
                    lock.wait();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                System.out.println("我被唤醒了！");
            }
           // 做其他的事情
        }
    }
}
```

显然，只有当前值满足需要值的时候，线程才可以往下执行，所以，必须使用while 循环阻塞。注意，wait() 当被唤醒时候，只是让while循环继续往下走.如果此处用if的话，意味着if继续往下走，会跳出if语句块。