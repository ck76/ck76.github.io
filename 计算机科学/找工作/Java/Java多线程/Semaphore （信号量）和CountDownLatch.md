**一、Semaphore （信号量）**

​    说简单点，Semaphore维护了一个许可集合，在创建Semaphore的时候，设置上许可数，每条线程在只有在获得一个许可的时候才可以继续往下执行逻辑（申请一个许可，则Semaphore的许可池中减少一个许可），没有获得许可的线程会进入阻塞状态。

举个栗子：

```java
public static void main(String[] args) {
        //创建一个Semaphore 有5条许可
        final Semaphore semaphore = new Semaphore(5);
        for (int i = 0; i < 10; i++) {
            final int finalI = i;
            new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        //申请一个许可  许可池-1   若许可池为0则申请许可失败，阻塞线程
                        semaphore.acquire();
                        System.out.print(finalI);
                        //模仿耗时操作
                        Thread.sleep(1000);
                        //释放一个许可  许可池+1
                        semaphore.release();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }).start();
        }
    }
```

然后看log的打印：

```java
12-19 11:06:52.720 13842-13978/lbx.myapplication E/lbxlog: 0
12-19 11:06:52.720 13842-13981/lbx.myapplication E/lbxlog: 3
12-19 11:06:52.720 13842-13979/lbx.myapplication E/lbxlog: 1
12-19 11:06:52.720 13842-13980/lbx.myapplication E/lbxlog: 2
12-19 11:06:52.720 13842-13982/lbx.myapplication E/lbxlog: 4
//这里注意一下  仔细对比时间
12-19 11:06:53.720 13842-13983/lbx.myapplication E/lbxlog: 5
12-19 11:06:53.720 13842-13985/lbx.myapplication E/lbxlog: 7
12-19 11:06:53.720 13842-13987/lbx.myapplication E/lbxlog: 9
12-19 11:06:53.720 13842-13986/lbx.myapplication E/lbxlog: 8
12-19 11:06:53.720 13842-13984/lbx.myapplication E/lbxlog: 6
```

​    看了上面的log，发现前五个是52秒左右打印出来的，而后五个是53秒左右打印出来的，因为许可池里只有5个许可，前五条线程把5个许可占用了，后五条线程需要等到有许可后才会继续执行逻辑。

接下来介绍几个常用的方法：

```java
//释放1个许可
semaphore.release();
//使acquire()后正在等待的线程不可被终止
semaphore.acquireUninterruptibly();
//获取当前可用的许可数量，并把可用的许可数置0
semaphore.drainPermits();
//是否有正在等待的线程
semaphore.hasQueuedThreads();
//获取正在等待的线程数量
semaphore.getQueueLength();
//获取当前可用的许可数量
semaphore.availablePermits();
```

**二、信号量的另一种形式：非公平信号量**

​    非公平信号量，先运行的线程不一定可以申请到许可，后运行的线程不一定不可以申请到许可。

```java
//创建5个公平信号量
Semaphore semaphore = new Semaphore(5,true);
//尝试获得许可，并返回获取结果   if(acquireSuc)...else...
boolean acquireSuc = semaphore.tryAcquire();
boolean acquireSuc = semaphore.tryAcquire(2);
//在3秒内尝试获得1个许可，并返回获取结果
boolean acquireSuc = semaphore.tryAcquire(3, TimeUnit.SECONDS);
//在3秒内尝试获得2个许可，并返回获取结果
boolean acquireSuc = semaphore.tryAcquire(2, 3, TimeUnit.SECONDS);
 
```

**三、CountDownLatch的应用**

  这个东西和信号量正相反，CountDownLatch的内部维护着一个计数器，同时只有一个线程才可对计数器进行操作，当计数器为0的时候，释放所有阻塞的线程。

举个栗子：

```java
public static void main(String[] args) {
        //4条线程
        int threadCount = 4;
        //创建一个拥有5个计数器的countDownLatch
        final CountDownLatch countDownLatch = new CountDownLatch(5);
        for (int i = 0; i < threadCount; i++) {
            new Thread(new Runnable() {
                @Override
                public void run() {
                    //使计数器-1
                    countDownLatch.countDown();
                    System.out.println("计数器还剩" + countDownLatch.getCount());
                }
            }).start();
        }
 
        try {
            //在这里阻塞  只有计数器为0的时候这行代码才会释放
             countDownLatch.await();
            } catch (InterruptedException e) {
             e.printStackTrace();
            }
 
  System.out.println("执行完成啦！"); 
}
 
```

log：


```java
12-19 11:52:25.350 22929-23074/lbx.myapplication I/System.out: 计数器还剩4
12-19 11:52:25.350 22929-23073/lbx.myapplication I/System.out: 计数器还剩3
12-19 11:52:25.350 22929-23075/lbx.myapplication I/System.out: 计数器还剩2
12-19 11:52:25.350 22929-23076/lbx.myapplication I/System.out: 计数器还剩1
```

因为计数器没有到0，所以“执行完成”这句话没有打印，将线程改成5条：

```java
//5线程
        int threadCount = 5;
```

log：

```java
12-19 11:58:46.570 30347-30471/lbx.myapplication I/System.out: 计数器还剩4
12-19 11:58:46.570 30347-30473/lbx.myapplication I/System.out: 计数器还剩3
12-19 11:58:46.570 30347-30472/lbx.myapplication I/System.out: 计数器还剩2
12-19 11:58:46.570 30347-30474/lbx.myapplication I/System.out: 计数器还剩1
12-19 11:58:46.570 30347-30475/lbx.myapplication I/System.out: 计数器还剩0
12-19 11:58:46.570 30347-30347/lbx.myapplication I/System.out: 执行完成啦！
```

这时候完整的流程就执行完了。常用方法：

```java
//使线程进入阻塞状态，计数为0的时候释放线程
countDownLatch.await();
//使线程进入阻塞状态，计数为0的时候释放线程或者阻塞到3秒的时候自动释放线程
countDownLatch.await(3, TimeUnit.SECONDS);
//使计数减1
countDownLatch.countDown();
//获取当前的计数
countDownLatch.getCount();
```



---

CountDownLatch和CyclicBarrier的功能看起来很相似，不易区分，有一种谜之的神秘。本文将通过通俗的例子并结合代码讲解两者的使用方法和区别。

CountDownLatch和CyclicBarrier都是java.util.concurrent包下面的多线程工具类。

从字面上理解，CountDown表示减法计数，Latch表示门闩的意思，计数为0的时候就可以打开门闩了。Cyclic Barrier表示循环的障碍物。两个类都含有这一个意思：对应的线程都完成工作之后再进行下一步动作，也就是大家都准备好之后再进行下一步。然而两者最大的区别是，进行下一步动作的动作实施者是不一样的。这里的“动作实施者”有两种，一种是主线程（即执行main函数），另一种是执行任务的其他线程，后面叫这种线程为“其他线程”，区分于主线程。对于CountDownLatch，当计数为0的时候，下一步的动作实施者是main函数；对于CyclicBarrier，下一步动作实施者是“其他线程”。

下面举例说明：

对于CountDownLatch，其他线程为游戏玩家，比如英雄联盟，主线程为控制游戏开始的线程。在所有的玩家都准备好之前，主线程是处于等待状态的，也就是游戏不能开始。当所有的玩家准备好之后，下一步的动作实施者为主线程，即开始游戏。

我们使用代码模拟这个过程，我们模拟了三个玩家，在三个玩家都准备好之后，游戏才能开始。代码的输出结果为：



```
正在等待所有玩家准备好
player0 已经准备好了, 所使用的时间为 1.235s
player2 已经准备好了, 所使用的时间为 1.279s
player3 已经准备好了, 所使用的时间为 1.358s
player1 已经准备好了, 所使用的时间为 2.583s
开始游戏
123456
```

CountDownLatch的代码：



```
import java.util.Random;
import java.util.concurrent.CountDownLatch;

public class CountDownLatchTest {

    public static void main(String[] args) throws InterruptedException {
        CountDownLatch latch = new CountDownLatch(4);
        for(int i = 0; i < latch.getCount(); i++){
            new Thread(new MyThread(latch), "player"+i).start();
        }
        System.out.println("正在等待所有玩家准备好");
        latch.await();
        System.out.println("开始游戏");
    }

    private static class MyThread implements Runnable{
        private CountDownLatch latch ;

        public MyThread(CountDownLatch latch){
            this.latch = latch;
        }

        @Override
        public void run() {
            try {
                Random rand = new Random();
                int randomNum = rand.nextInt((3000 - 1000) + 1) + 1000;//产生1000到3000之间的随机整数
                Thread.sleep(randomNum);
                System.out.println(Thread.currentThread().getName()+" 已经准备好了, 所使用的时间为 "+((double)randomNum/1000)+"s");
                latch.countDown();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

        }
    }
}
12345678910111213141516171819202122232425262728293031323334353637
```

对于CyclicBarrier，假设有一家公司要全体员工进行团建活动，活动内容为翻越三个障碍物，每一个人翻越障碍物所用的时间是不一样的。但是公司要求所有人在翻越当前障碍物之后再开始翻越下一个障碍物，也就是所有人翻越第一个障碍物之后，才开始翻越第二个，以此类推。类比地，每一个员工都是一个“其他线程”。当所有人都翻越的所有的障碍物之后，程序才结束。而主线程可能早就结束了，这里我们不用管主线程。

我们使用代码来模拟上面的过程。我们设置了三个员工和三个障碍物。可以看到所有的员工翻越了第一个障碍物之后才开始翻越第二个的，下面是运行结果：



```
main function is finished.
队友1, 通过了第0个障碍物, 使用了 1.432s
队友0, 通过了第0个障碍物, 使用了 1.465s
队友2, 通过了第0个障碍物, 使用了 2.26s
队友1, 通过了第1个障碍物, 使用了 1.542s
队友0, 通过了第1个障碍物, 使用了 2.154s
队友2, 通过了第1个障碍物, 使用了 2.556s
队友1, 通过了第2个障碍物, 使用了 1.426s
队友2, 通过了第2个障碍物, 使用了 2.603s
队友0, 通过了第2个障碍物, 使用了 2.784s
12345678910
```

代码：



```
package com.huai.thread;

import java.util.Random;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;

public class CyclicBarrierTest {
    public static void main(String[] args) {
        CyclicBarrier barrier = new CyclicBarrier(3);
        for(int i = 0; i < barrier.getParties(); i++){
            new Thread(new MyRunnable(barrier), "队友"+i).start();
        }
        System.out.println("main function is finished.");
    }


    private static class MyRunnable implements Runnable{
        private CyclicBarrier barrier;

        public MyRunnable(CyclicBarrier barrier){
            this.barrier = barrier;
        }

        @Override
        public void run() {
            for(int i = 0; i < 3; i++) {
                try {
                    Random rand = new Random();
                    int randomNum = rand.nextInt((3000 - 1000) + 1) + 1000;//产生1000到3000之间的随机整数
                    Thread.sleep(randomNum);
                    System.out.println(Thread.currentThread().getName() + ", 通过了第"+i+"个障碍物, 使用了 "+((double)randomNum/1000)+"s");
                    this.barrier.await();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                } catch (BrokenBarrierException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
1234567891011121314151617181920212223242526272829303132333435363738394041
```

总结：CountDownLatch和CyclicBarrier都有让多个线程等待同步然后再开始下一步动作的意思，但是CountDownLatch的下一步的动作实施者是主线程，具有不可重复性；而CyclicBarrier的下一步动作实施者还是“其他线程”本身，具有往复多次实施动作的特点。

----

JAVA并发包中有三个类用于同步一批线程的行为，分别是CountDownLatch、Semaphore和CyclicBarrier。

***1\***|***0\*****CountDownLatch**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm7hw1nkwtj30bi0aqgnz.jpg)

CountDownLatch是一个计数器闭锁，通过它可以完成类似于阻塞当前线程的功能，即：一个线程或多个线程一直等待，直到其他线程执行的操作完成。CountDownLatch用一个给定的计数器来初始化，该计数器的操作是原子操作，即同时只能有一个线程去操作该计数器。调用该类await方法的线程会一直处于阻塞状态，直到其他线程调用countDown方法使当前计数器的值变为零，每次调用countDown计数器的值减1。当计数器值减至零时，所有因调用await()方法而处于等待状态的线程就会继续往下执行。这种现象只会出现一次，因为计数器不能被重置，如果业务上需要一个可以重置计数次数的版本，可以考虑使用CycliBarrier。

在某些业务场景中，程序执行需要等待某个条件完成后才能继续执行后续的操作；典型的应用如并行计算，当某个处理的运算量很大时，可以将该运算任务拆分成多个子任务，等待所有的子任务都完成之后，父任务再拿到所有子任务的运算结果进行汇总。



```
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Slf4j
public class CountDownLatchExample1 {

    private final static int threadCount = 200;

    public static void main(String[] args) throws Exception {

        ExecutorService exec = Executors.newCachedThreadPool();

        final CountDownLatch countDownLatch = new CountDownLatch(threadCount);

        for (int i = 0; i < threadCount; i++) {
            final int threadNum = i;
            exec.execute(() -> {
                try {
                    test(threadNum);
                } catch (Exception e) {
                    log.error("exception", e);
                } finally {
                    countDownLatch.countDown();
                }
            });
        }
        countDownLatch.await();
        log.info("finish");
        exec.shutdown();
    }

    private static void test(int threadNum) throws Exception {
        Thread.sleep(100);
        log.info("{}", threadNum);
        Thread.sleep(100);
    }
}
```

结果：



```
20:18:32.917 [pool-1-thread-7] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample1 - 6
20:18:32.917 [pool-1-thread-6] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample1 - 5
20:18:32.919 [pool-1-thread-5] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample1 - 4
20:18:32.918 [pool-1-thread-1] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample1 - 0
20:18:32.918 [pool-1-thread-3] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample1 - 2
20:18:32.916 [pool-1-thread-9] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample1 - 8
20:18:32.918 [pool-1-thread-4] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample1 - 3
20:18:32.916 [pool-1-thread-10] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample1 - 9
20:18:32.916 [pool-1-thread-8] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample1 - 7
20:18:32.917 [pool-1-thread-2] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample1 - 1
20:18:33.032 [main] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample1 - finish
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

@Slf4j
public class CountDownLatchExample2 {

    private final static int threadCount = 200;

    public static void main(String[] args) throws Exception {

        ExecutorService exec = Executors.newCachedThreadPool();

        final CountDownLatch countDownLatch = new CountDownLatch(threadCount);

        for (int i = 0; i < threadCount; i++) {
            final int threadNum = i;
            exec.execute(() -> {
                try {
                    test(threadNum);
                } catch (Exception e) {
                    log.error("exception", e);
                } finally {
                    countDownLatch.countDown();
                }
            });
        }
        countDownLatch.await(10, TimeUnit.MILLISECONDS);
        log.info("finish");
        exec.shutdown();
    }

    private static void test(int threadNum) throws Exception {
        Thread.sleep(100);
        log.info("{}", threadNum);
    }
}
```

结果： 超过指定时间跳过等待



```
20:19:34.878 [main] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample2 - finish
20:19:34.964 [pool-1-thread-3] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample2 - 2
20:19:34.965 [pool-1-thread-10] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample2 - 9
20:19:34.964 [pool-1-thread-1] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample2 - 0
20:19:34.965 [pool-1-thread-8] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample2 - 7
20:19:34.964 [pool-1-thread-2] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample2 - 1
20:19:34.965 [pool-1-thread-5] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample2 - 4
20:19:34.965 [pool-1-thread-7] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample2 - 6
20:19:34.964 [pool-1-thread-4] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample2 - 3
20:19:34.965 [pool-1-thread-9] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample2 - 8
20:19:34.965 [pool-1-thread-6] INFO com.mmall.concurrency.example.aqs.CountDownLatchExample2 - 5
```

***2\***|***0\*****Semaphore**

[![img](https://img2020.cnblogs.com/blog/2004895/202006/2004895-20200624111312083-1012492872.png)](https://img2020.cnblogs.com/blog/2004895/202006/2004895-20200624111312083-1012492872.png)

Semaphore与CountDownLatch相似，不同的地方在于Semaphore的值被获取到后是可以释放的，并不像CountDownLatch那样一直减到底。它也被更多地用来**限制流量**，类似阀门的 功能。如果限定某些资源最多有N个线程可以访问，那么超过N个主不允许再有线程来访问，同时当现有线程结束后，就会释放，然后允许新的线程进来。有点类似于锁的lock与 unlock过程。相对来说他也有两个主要的方法：

用于获取权限的acquire(),其底层实现与CountDownLatch.countdown()类似;
用于释放权限的release()，其底层实现与acquire()是一个互逆的过程。



```java
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;

@Slf4j
public class SemaphoreExample1 {

    private final static int threadCount = 20;

    public static void main(String[] args) throws Exception {

        ExecutorService exec = Executors.newCachedThreadPool();
       // 每次最多三个线程获取许可
        final Semaphore semaphore = new Semaphore(3);

        for (int i = 0; i < threadCount; i++) {
            final int threadNum = i;
            exec.execute(() -> {
                try {
                    semaphore.acquire(); // 获取一个许可
                    test(threadNum);
                    semaphore.release(); // 释放一个许可
                } catch (Exception e) {
                    log.error("exception", e);
                }
            });
        }
        exec.shutdown();
    }

    private static void test(int threadNum) throws Exception {
        log.info("{}", threadNum);
        Thread.sleep(1000);
    }
}
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;

@Slf4j
public class SemaphoreExample2 {

    private final static int threadCount = 20;

    public static void main(String[] args) throws Exception {

        ExecutorService exec = Executors.newCachedThreadPool();

        final Semaphore semaphore = new Semaphore(3);

        for (int i = 0; i < threadCount; i++) {
            final int threadNum = i;
            exec.execute(() -> {
                try {
                    semaphore.acquire(3); // 获取多个许可
                    test(threadNum);
                    semaphore.release(3); // 释放多个许可
                } catch (Exception e) {
                    log.error("exception", e);
                }
            });
        }
        exec.shutdown();
    }

    private static void test(int threadNum) throws Exception {
        log.info("{}", threadNum);
        Thread.sleep(1000);
    }
}
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

@Slf4j
public class SemaphoreExample3 {

    private final static int threadCount = 20;

    public static void main(String[] args) throws Exception {

        ExecutorService exec = Executors.newCachedThreadPool();

        final Semaphore semaphore = new Semaphore(3);

        for (int i = 0; i < threadCount; i++) {
            final int threadNum = i;
            exec.execute(() -> {
                try {
                    if (semaphore.tryAcquire()) { // 尝试获取一个许可
                        test(threadNum);
                        semaphore.release(); // 释放一个许可
                    }
                } catch (Exception e) {
                    log.error("exception", e);
                }
            });
        }
        exec.shutdown();
    }

    private static void test(int threadNum) throws Exception {
        log.info("{}", threadNum);
        Thread.sleep(1000);
    }
}
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

@Slf4j
public class SemaphoreExample4 {

    private final static int threadCount = 20;

    public static void main(String[] args) throws Exception {

        ExecutorService exec = Executors.newCachedThreadPool();

        final Semaphore semaphore = new Semaphore(3);

        for (int i = 0; i < threadCount; i++) {
            final int threadNum = i;
            exec.execute(() -> {
                try {
                    if (semaphore.tryAcquire(5000, TimeUnit.MILLISECONDS)) { // 尝试获取一个许可
                        test(threadNum);
                        semaphore.release(); // 释放一个许可
                    }
                } catch (Exception e) {
                    log.error("exception", e);
                }
            });
        }
        exec.shutdown();
    }

    private static void test(int threadNum) throws Exception {
        log.info("{}", threadNum);
        Thread.sleep(1000);
    }
}
```

***3\***|***0\*****CyclicBarrier**


![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm7hwq2fjaj30bw0b50v2.jpg)

CyclicBarrier也是一个同步辅助类，它允许一组线程相互等待，直到到达某个公共屏障点（common barrier point）。通过它可以完成多个线程之间相互等待，只有当每个线程都准备就绪后，才能各自继续往下执行后面的操作。类似于CountDownLatch，它也是通过计数器来实现的。当某个线程调用await方法时，该线程进入等待状态，且计数器加1，当计数器的值达到设置的初始值时，所有因调用await进入等待状态的线程被唤醒，继续执行后续操作。因为CycliBarrier在释放等待线程后可以重用，所以称为循环barrier。CycliBarrier支持一个可选的Runnable，在计数器的值到达设定值后（但在释放所有线程之前），该Runnable运行一次，注，Runnable在每个屏障点只运行一个。

**使用场景类似于CountDownLatch与CyclicBarrier的区别**

[详情请看另一篇博客](https://www.cnblogs.com/sweetorangezzz/p/13186599.html)

- CountDownLatch主要是实现了1个或N个线程需要等待其他线程完成某项操作之后才能继续往下执行操作，描述的是1个线程或N个线程等待其他线程的关系。CyclicBarrier主要是实现了多个线程之间相互等待，直到所有的线程都满足了条件之后各自才能继续执行后续的操作，描述的多个线程内部相互等待的关系。
- CountDownLatch是一次性的，而CyclicBarrier则可以被重置而重复使用。



```
@Slf4j
public class CyclicBarrierExample1 {

    private static CyclicBarrier barrier = new CyclicBarrier(5);

    public static void main(String[] args) throws Exception {

        ExecutorService executor = Executors.newCachedThreadPool();

        for (int i = 0; i < 10; i++) {
            final int threadNum = i;
            Thread.sleep(1000);
            executor.execute(() -> {
                try {
                    race(threadNum);
                } catch (Exception e) {
                    log.error("exception", e);
                }
            });
        }
        executor.shutdown();
    }

    private static void race(int threadNum) throws Exception {
        Thread.sleep(1000);
        log.info("{} is ready", threadNum);
        barrier.await();
        log.info("{} continue", threadNum);
    }
}
```

结果： ready ready .. go



```
20:24:34.616 [pool-1-thread-1] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 0 is ready
20:24:35.610 [pool-1-thread-2] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 1 is ready
20:24:36.610 [pool-1-thread-3] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 2 is ready
20:24:37.611 [pool-1-thread-4] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 3 is ready
20:24:38.612 [pool-1-thread-5] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 4 is ready
20:24:38.612 [pool-1-thread-1] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 0 continue
20:24:38.612 [pool-1-thread-2] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 1 continue
20:24:38.612 [pool-1-thread-5] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 4 continue
20:24:38.612 [pool-1-thread-4] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 3 continue
20:24:38.612 [pool-1-thread-3] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 2 continue
20:24:39.614 [pool-1-thread-6] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 5 is ready
20:24:40.613 [pool-1-thread-5] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 6 is ready
20:24:41.614 [pool-1-thread-2] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 7 is ready
20:24:42.615 [pool-1-thread-4] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 8 is ready
20:24:43.615 [pool-1-thread-3] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 9 is ready
20:24:43.615 [pool-1-thread-3] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 9 continue
20:24:43.615 [pool-1-thread-6] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 5 continue
20:24:43.615 [pool-1-thread-5] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 6 continue
20:24:43.615 [pool-1-thread-2] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 7 continue
20:24:43.615 [pool-1-thread-4] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample1 - 8 continue
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

@Slf4j
public class CyclicBarrierExample2 {

    private static CyclicBarrier barrier = new CyclicBarrier(5);

    public static void main(String[] args) throws Exception {

        ExecutorService executor = Executors.newCachedThreadPool();

        for (int i = 0; i < 10; i++) {
            final int threadNum = i;
            Thread.sleep(1000);
            executor.execute(() -> {
                try {
                    race(threadNum);
                } catch (Exception e) {
                    log.error("exception", e);
                }
            });
        }
        executor.shutdown();
    }

    private static void race(int threadNum) throws Exception {
        Thread.sleep(1000);
        log.info("{} is ready", threadNum);
        try {
            barrier.await(2000, TimeUnit.MILLISECONDS);
        } catch (Exception e) {
            log.warn("BarrierException", e);
        }
        log.info("{} continue", threadNum);
    }
}
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Slf4j
public class CyclicBarrierExample3 {

    private static CyclicBarrier barrier = new CyclicBarrier(5, () -> {
        log.info("callback is running");
    });

    public static void main(String[] args) throws Exception {

        ExecutorService executor = Executors.newCachedThreadPool();

        for (int i = 0; i < 10; i++) {
            final int threadNum = i;
            Thread.sleep(1000);
            executor.execute(() -> {
                try {
                    race(threadNum);
                } catch (Exception e) {
                    log.error("exception", e);
                }
            });
        }
        executor.shutdown();
    }

    private static void race(int threadNum) throws Exception {
        Thread.sleep(1000);
        log.info("{} is ready", threadNum);
        barrier.await();
        log.info("{} continue", threadNum);
    }
}
```

结果：



```
20:28:32.790 [pool-1-thread-1] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 0 is ready
20:28:33.785 [pool-1-thread-2] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 1 is ready
20:28:34.786 [pool-1-thread-3] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 2 is ready
20:28:35.787 [pool-1-thread-4] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 3 is ready
20:28:36.787 [pool-1-thread-5] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 4 is ready
20:28:36.787 [pool-1-thread-5] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - callback is running
20:28:36.787 [pool-1-thread-5] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 4 continue
20:28:36.788 [pool-1-thread-1] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 0 continue
20:28:36.788 [pool-1-thread-2] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 1 continue
20:28:36.788 [pool-1-thread-3] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 2 continue
20:28:36.788 [pool-1-thread-4] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 3 continue
20:28:37.788 [pool-1-thread-6] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 5 is ready
20:28:38.789 [pool-1-thread-4] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 6 is ready
20:28:39.789 [pool-1-thread-5] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 7 is ready
20:28:40.790 [pool-1-thread-2] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 8 is ready
20:28:41.791 [pool-1-thread-3] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 9 is ready
20:28:41.791 [pool-1-thread-3] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - callback is running
20:28:41.791 [pool-1-thread-3] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 9 continue
20:28:41.791 [pool-1-thread-6] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 5 continue
20:28:41.791 [pool-1-thread-4] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 6 continue
20:28:41.818 [pool-1-thread-2] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 8 continue
20:28:41.818 [pool-1-thread-5] INFO com.mmall.concurrency.example.aqs.CyclicBarrierExample3 - 7 continue
```



__EOF__

---

https://www.cnblogs.com/dolphin0520/p/3920397.html