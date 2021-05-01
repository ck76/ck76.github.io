https://www.cnblogs.com/aheizi/p/6851416.html

[TOC]

前一篇文章[Java中实现多线程关键词整理](http://www.cnblogs.com/aheizi/p/6843399.html)中介绍了Java中创建多线程的各种办法，里面提到了线程池，这里对Java中的线程池做一个总结。



## 1. 关于ThreadPoolExecutor

为了更好地控制多线程，JDK提供了一套Executor框架，帮助开发人员有效的进行线程控制，其本质就是一个线程池。其中ThreadPoolExecutor是线程池中最核心的一个类，后面提到的四种线程池都是基于ThreadPoolExecutor实现的。

ThreadPoolExecutor提供了四个构造方法，我们看下最重要的一个构造函数：

```java
public class ThreadPoolExecutor extends AbstractExecutorService {
    public ThreadPoolExecutor(int corePoolSize,
                              int maximumPoolSize,
                              long keepAliveTime,TimeUnit unit,
                              BlockingQueue<Runnable> workQueue,
                              ThreadFactory threadFactory,
                              RejectedExecutionHandler handler);
}
```

函数的参数含义如下：

- corePoolSize： 线程池维护线程的最少数量
- maximumPoolSize：线程池维护线程的最大数量
- keepAliveTime： 线程池维护线程所允许的空闲时间
- unit： 线程池维护线程所允许的空闲时间的单位
- workQueue： 线程池所使用的缓冲队列
- handler： 线程池对拒绝任务的处理策略

线程池执行的过程：

1. 线程池刚创建时，里面没有一个线程。任务队列是作为参数传进来的。不过，就算队列里面有任务，线程池也不会马上执行它们。
2. 当调用 execute() 方法添加一个任务时，线程池会做如下判断：
   ​ a. 如果正在运行的线程数量小于 corePoolSize，那么马上创建线程运行这个任务；
   ​ b. 如果正在运行的线程数量大于或等于 corePoolSize，那么将这个任务放入队列。
   ​ c. 如果这时候队列满了，而且正在运行的线程数量小于 maximumPoolSize，那么还是要创建线程运行这个任务；
   ​ d. 如果队列满了，而且正在运行的线程数量大于或等于 maximumPoolSize，那么线程池会抛出异常，告诉调用者“我不能再接受任务了”。
3. 当一个线程完成任务时，它会从队列中取下一个任务来执行。
4. 当一个线程无事可做，超过一定的时间（keepAliveTime）时，线程池会判断，如果当前运行的线程数大于corePoolSize，那么这个线程就被停掉。所以线程池的所有任务完成后，它最终会收缩到 corePoolSize 的大小。

**ThreadPoolExecutor的继承关系：**

![](http://images2015.cnblogs.com/blog/658141/201705/658141-20170514011255769-720738172.png)

**ThreadPoolExecutor中的队列：**

ThreadPoolExecutor内部应用了任务缓存队列，即workQueue，它用来存放等待执行的任务。

workQueue的类型为BlockingQueue，通常可以取下面三种类型：

1. ArrayBlockingQueue：基于数组的先进先出队列，此队列创建时必须指定大小；
2. LinkedBlockingQueue：基于链表的先进先出队列，如果创建时没有指定此队列大小，则默认为Integer.MAX_VALUE；
3. synchronousQueue：这个队列比较特殊，它不会保存提交的任务，而是将直接新建一个线程来执行新来的任务。

**任务拒绝策略：**

当线程池的任务缓存队列已满并且线程池中的线程数目达到maximumPoolSize，如果还有任务到来就会采取任务拒绝策略，通常有以下四种策略：

```java
ThreadPoolExecutor.AbortPolicy:丢弃任务并抛出RejectedExecutionException异常。
ThreadPoolExecutor.DiscardPolicy：也是丢弃任务，但是不抛出异常。
ThreadPoolExecutor.DiscardOldestPolicy：丢弃队列最前面的任务，然后重新尝试执行任务（重复此过程）
ThreadPoolExecutor.CallerRunsPolicy：由调用线程处理该任务
```

**扩展线程池（记录任务执行日志）：**

在默认的ThreadPoolExecutor实现中，提供了空的beforeExecutor和afterExecutor的实现，在实际应用中可以对其进行扩展来实现对线程池运行状态的追踪，输出一些有用的调试信息，以帮助系统故障诊断，这对于多线程程序错误排查是很有帮助的。

ThreadPoolExecutor例子：

```java
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

public class ThreadPool {
	private int corePoolSize = 1; // 线程池维护线程的最少数量
	private int maximumPoolSize = 10;// 线程池维护线程的最大数量
	private long keepAliveTime = 3; // 线程池维护线程所允许的空闲时间
	private TimeUnit unit = TimeUnit.SECONDS;// 线程池维护线程所允许的空闲时间的单位
	private BlockingQueue<Runnable> workQueue; // 线程池所使用的缓冲队列
	private RejectedExecutionHandler handler; // 线程池对拒绝任务的处理策略
	private static AtomicLong along = new AtomicLong(0);

	public void run() throws InterruptedException {
		ThreadPoolExecutor pool = new ThreadPoolExecutor(corePoolSize,
				maximumPoolSize, keepAliveTime, unit,
				new LinkedBlockingQueue<Runnable>(),
				new ThreadPoolExecutor.DiscardOldestPolicy()) {

			// 线程执行之前运行
			@Override
			protected void beforeExecute(Thread t, Runnable r) {
				System.out.println("...............beforeExecute");
			}

			// 线程执行之后运行
			@Override
			protected void afterExecute(Runnable r, Throwable t) {
				System.out.println("...............afterExecute");
			}

			// 整个线程池停止之后
			protected void terminated() {
				System.out.println("...............thread stop");
			}
		};
		for (int i = 1; i <= 10; i++) {
			pool.execute(new ThreadPoolTask(i, along));
		}
		for (int i = 1; i <= 10; i++) {
			pool.execute(new ThreadPoolTask(-i, along));
		}
		pool.shutdown();
		Thread.sleep(25000);
		System.out.println(along.get());

	}

	public static void main(String[] args) {
		try {
			new ThreadPool().run();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}

class ThreadPoolTask implements Runnable {
	private int i = 0;
	private AtomicLong along;

	ThreadPoolTask(int i, AtomicLong along) {
		this.i = i;
		this.along = along;
	}
	
	@Override
	public void run() {
		try {
			// 模拟业务逻辑
			Thread.sleep(1000);
			along.addAndGet(i);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		System.out.println(Thread.currentThread().getName() + "  " + i);
	}
}
```

我们可以利用这个特性实现在线程池中打印出异常堆栈信息（正常是不会打印出来的），这里就不演示了。



## 2. 关于Executors提供的四种线程池

Executors 提供了一系列工厂方法用于创先线程池，返回的线程池都实现了 ExecutorService 接口。

```
// 创建固定数目线程的线程池。
public static ExecutorService newFixedThreadPool(int nThreads)

// 创建一个可缓存的线程池，调用execute将重用以前构造的线程（如果线程可用）。
// 如果现有线程没有可用的，则创建一个新线 程并添加到池中。
// 终止并从缓存中移除那些已有 60 秒钟未被使用的线程。
public static ExecutorService newCachedThreadPool()

// 创建一个单线程化的Executor。
public static ExecutorService newSingleThreadExecutor()

// 创建一个支持定时及周期性的任务执行的线程池，多数情况下可用来替代Timer类。
public static ScheduledExecutorService newScheduledThreadPool(int corePoolSize)
```

这四种方法都是用的 Executors 中的 ThreadFactory 建立的线程。

**newCachedThreadPool()**

- 缓存型池子，先查看池中有没有以前建立的线程，如果有，就 reuse 如果没有，就建一个新的线程加入池中
- 缓存型池子通常用于执行一些生存期很短的异步型任务 因此在一些面向连接的 daemon 型 SERVER 中用得不多。但对于生存期短的异步任务，它是 Executor 的首选。
- 能 reuse 的线程，必须是 timeout IDLE 内的池中线程，缺省 timeout 是 60s,超过这个 IDLE 时长，线程实例将被终止及移出池。

**newFixedThreadPool(int)**

- newFixedThreadPool 与 cacheThreadPool 差不多，也是能 reuse 就用，但不能随时建新的线程。
- 其独特之处:任意时间点，最多只能有固定数目的活动线程存在，此时如果有新的线程要建立，只能放在另外的队列中等待，直到当前的线程中某个线程终止直接被移出池子。
- 和 cacheThreadPool 不同，FixedThreadPool 没有 IDLE 机制（可能也有，但既然文档没提，肯定非常长，类似依赖上层的 TCP 或 UDP IDLE 机制之类的），所以 FixedThreadPool 多数针对一些很稳定很固定的正规并发线程，多用于服务器。
- 从方法的源代码看，cache池和fixed 池调用的是同一个底层 池，只不过参数不同:
  - fixed 池线程数固定，并且是0秒IDLE（无IDLE）。
  - cache 池线程数支持 0-Integer.MAX_VALUE(显然完全没考虑主机的资源承受能力），60 秒 IDLE 。

**newScheduledThreadPool(int)**

- 调度型线程池
- 这个池子里的线程可以按 schedule 依次 delay 执行，或周期执行

**SingleThreadExecutor()**

- 单例线程，任意时间池中只能有一个线程
- 用的是和 cache 池和 fixed 池相同的底层池，但线程数目是 1-1,0 秒 IDLE（无 IDLE）

> 一般来说，CachedTheadPool 在程序执行过程中通常会创建与所需数量相同的线程，然后在它回收旧线程时停止创建新线程，因此它是合理的 Executor 的首选，只有当这种方式会引发问题时（比如需要大量长时间面向连接的线程时），才需要考虑用 FixedThreadPool。
>
> ----《Thinking in Java》第四版

以上引用自[极客学院](http://wiki.jikexueyuan.com/project/java-concurrency/executor.html)，总结的太精彩了。



## 3. Spring中的线程池管理

Spring的TaskExecutor接口等同于java.util.concurrent.Executor接口。 实际上，它存在的主要原因是为了在使用线程池的时候，将对Java 5的依赖抽象出来。 这个接口只有一个方法execute(Runnable task)，它根据线程池的语义和配置，来接受一个执行任务。最初创建TaskExecutor是为了在需要时给其他Spring组件提供一个线程池的抽象。例如ApplicationEventMulticaster组件、JMS的 AbstractMessageListenerContainer和对Quartz的整合都使用了TaskExecutor抽象来提供线程池。 当然，如果你的bean需要线程池行为，你也可以使用这个抽象层。

介绍下使用比较多的ThreadPoolTaskExecutor 类，这个实现只能在Java 5以上环境使用（现在应该没有低于1.5的老环境了吧~），它暴露的bean properties可以用来配置一个java.util.concurrent.ThreadPoolExecutor，把它包装到一个TaskExecutor中。

spring中ThreadPoolTaskExecutor最常用方式就是做为BEAN注入到容器中,其暴露的各个属性其实是ThreadPoolExecutor的属性，而且这体现了DI容器的优势：

```xml
<bean id="threadPoolTaskExecutor" class="org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor">  
    <property name="corePoolSize" value="2"/>  
    <property name="keepAliveSeconds" value="200"/>  
    <property name="maxPoolSize" value="10"/>  
    <property name="queueCapacity" value="60"/>  
</bean>
```



## 4. 优化线程池线程数量

线程池的理想大小取决于被提交任务的类型以及所部署系统的特性。在代码中不会固定线程池的大小，而应该通过某种配置机制来来提供，或者根据Runtime.getRuntime().availableProcessors()来动态计算。

如果一台服务器上只部署这一个应用并且只有一个线程池（N为CPU总核数）：

- 如果是CPU密集型应用，则线程池大小设置为N+1
- 如果是IO密集型应用，则线程池大小设置为2N+1

**线程等待时间所占比例越高，需要越多线程。线程CPU时间所占比例越高，需要越少线程。**

【黄金公式】最佳线程数目 = （线程等待时间与线程CPU时间之比 + 1）* CPU数目

一个实际的计算过程（[慕课网](http://www.imooc.com/article/5887)）：

**假设值**

- tasks ：每秒的任务数，假设为500~1000
- taskcost：每个任务花费时间，假设为0.1s
- responsetime：系统允许容忍的最大响应时间，假设为1s

**计算**

- **corePoolSize** = 每秒需要多少个线程处理？

 threadcount = tasks/(1/taskcost) =tasks*taskcout = (500~1000)*0.1 = 50~100 个线程。corePoolSize设置应该大于50

 根据8020原则，如果80%的每秒任务数小于800，那么corePoolSize设置为80即可

- **queueCapacity** = (coreSizePool/taskcost)*responsetime

 计算可得 queueCapacity = 80/0.1*1 = 80。意思是队列里的线程可以等待1s，超过了的需要新开线程来执行

 切记不能设置为Integer.MAX_VALUE，这样队列会很大，线程数只会保持在corePoolSize大小，当任务陡增时，不能新开线程来执行，响应时间会随之陡增。

- **maxPoolSize** = (max(tasks)- queueCapacity)/(1/taskcost)

 计算可得 maxPoolSize = (1000-80)/10 = 92

 (最大任务数-队列容量)/每个线程每秒处理能力 = 最大线程数

- **rejectedExecutionHandler**：根据具体情况来决定，任务不重要可丢弃，任务重要则要利用一些缓冲机制来处理
- **keepAliveTime**和**allowCoreThreadTimeout**采用默认通常能满足

参考博文：

[Java并发编程：线程池的使用](http://www.cnblogs.com/dolphin0520/p/3932921.html)

[线程池ThreadPoolExecutor使用简介](https://my.oschina.net/jielucky/blog/157250)

参考书籍：

《Java并发编程实战》

《Java高并发程序设计》