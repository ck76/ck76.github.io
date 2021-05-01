[TOC]

---

# [一文彻底搞懂CAS实现原理 & 深入到CPU指令](https://www.cnblogs.com/ldws/p/11970087.html)

#### **本文导读：**

- 前言
- 如何保障线程安全
- CAS原理剖析
- CPU如何保证原子操作
- 解密CAS底层指令
- 小结

------

> 朋友，文章优先发布在公众号上，如果你愿意，可以扫右侧二维码支持一下下~，谢谢！

#### **前言**

日常编码过程中，基本不会直接用到 CAS 操作，都是通过一些JDK 封装好的并发工具类来使用的，在 java.util.concurrent 包下。

但是面试时 CAS 还是个高频考点，所以呀，你还不得不硬着头皮去死磕一下这块的技能点，总比一问三不知强吧？

一般都是先针对一些简单的并发知识问起，还有的面试官，比较直接：

> 面试官：Java并发工具类中的 CAS 机制讲一讲？

当然 CAS 你若真不懂，你可以引导面试官到你擅长的技术点上，用你的其他技能亮点扳回一局。

接下来，我们通过一个示例代码来说：

```
// 类的成员变量
static int data = 0;
// main方法内代码
IntStream.range(0, 2).forEach((i) -> {
		new Thread(() -> {
				try {
						Thread.sleep(20);
				} catch (InterruptedException e) {
						e.printStackTrace();
				}
				IntStream.range(0, 100).forEach(y -> {
						data++;
				});
		}).start();
});

	try {
			Thread.sleep(2000);
	} catch (InterruptedException e) {
			e.printStackTrace();
	}

System.out.println(data);
}
```

结合图示理解：

![线程不安全](https://tva1.sinaimg.cn/large/0081Kckwly1gm1juo1y9cj30gu0cgt9n.jpg)

上述代码，问题很明显，data 是类中的成员变量，int 类型，即共享的资源。当多个线程同时
执行 `data++` 操作时，结果可能不等于 200，为了模拟出效果，线程中 sleep 了 20 毫秒，让线程就绪，代码运行多次，结果都不是 200 。

#### **如何保障线程安全**

示例代码执行结果表明了，多个线程同时操作共享变量导致了结果不准确，线程是不安全的。如何解决呢？

**方案一：使用 synchronized 关键字**

使用 synchronized 关键字，线程内使用同步代码块，由JVM自身的机制来保障线程的安全性。

synchronized 关键代码：

```
// 类中定义的Object锁对象
Object lock = new Object();
 
 // synchronized 同步块 () 中使用 lock 对象锁定资源
IntStream.range(0, 100).forEach(y -> {
		synchronized (lock.getClass()) {
				data++;
		}
});
```

![synchronized保障线程安全](https://tva1.sinaimg.cn/large/0081Kckwly1gm1jumkqu2j30cq0ig3zm.jpg)

**方案二：使用 Lock 锁**

高并发场景下，使用 Lock 锁要比使用 synchronized 关键字，在性能上得到极大的提高。
因为 Lock 底层是通过 AQS + CAS 机制来实现的。关于 AQS 机制可以参见往期文章 <<通过通过一个生活中的案例场景，揭开并发包底层AQS的神秘面纱>> 。CAS 机制会在文章中下面讲到。

使用 Lock 的关键代码：

```
// 类中定义成员变量  
Lock lock = new ReentrantLock();

// 执行 lock() 方法加锁，执行 unlock() 方法解锁
IntStream.range(0, 100).forEach(y -> {
		lock.lock();
		data++;
		lock.unlock();
});
```

结合图示理解：

![Lock锁保障线程安全](https://tva1.sinaimg.cn/large/0081Kckwly1gm1jujt485j30bs0i4ab6.jpg)

**方案三：使用 Atomic 原子类**

除上面两种方案还有没有更为优雅的方案？synchronized 的使用在 JDK1.6 版本以后做了很多优化，如果并发量不大，相比 Lock 更为安全，性能也能接受，因其得益于 JVM 底层机制来保障，自动释放锁，无需硬编码方式释放锁。而使用 Lock 方式，一旦 unlock() 方法使用不规范，可能导致死锁。

JDK 并发包所有的原子类如下所示：

![并发包原子类](https://tva1.sinaimg.cn/large/0081Kckwly1gm1juhiy2wj30hj0m875k.jpg)

使用 AtomicInteger 工具类实现代码：

```
// 类中成员变量定义原子类
AtomicInteger atomicData = new AtomicInteger();

// 代码中原子类的使用方式
IntStream.range(0, 2).forEach((i) -> {
	new Thread(() -> {
			try {
					Thread.sleep(20);
			} catch (InterruptedException e) {
					e.printStackTrace();
			}
			IntStream.range(0, 100).forEach(y -> {
				  // 原子类自增
					atomicData.incrementAndGet();
			});
	}).start();
});

try {
		Thread.sleep(2000);
} catch (InterruptedException e) {
		e.printStackTrace();
}

// 通过 get () 方法获取结果
System.out.println(atomicData.get());
```

结合图示理解：

![AtomicInteger实现](https://tva1.sinaimg.cn/large/0081Kckwly1gm1juf7dduj30bm0iemyi.jpg)

之所以推荐使用 Atomic 原子类，因为其底层基于 CAS 乐观锁来实现的，下文会详细分析。

**方案四：使用 LongAdder 原子类**

LongAdder 原子类在 JDK1.8 中新增的类， 跟方案三中提到的 AtomicInteger 类似，都是在 java.util.concurrent.atomic 并发包下的。

LongAdder 适合于高并发场景下，特别是写大于读的场景，相较于 AtomicInteger、AtomicLong 性能更好，代价是消耗更多的空间，以空间换时间。

使用 LongAdder 工具类实现代码：

```
// 类中成员变量定义的LongAdder
LongAdder longAdderData = new LongAdder();

// 代码中原子类的使用方式
IntStream.range(0, 2).forEach((i) -> {
		new Thread(() -> {
				try {
						Thread.sleep(20);
				} catch (InterruptedException e) {
						e.printStackTrace();
				}
				IntStream.range(0, 100).forEach(y -> {
					  // 使用 increment() 方法自增
						longAdderData.increment();
				});
		}).start();
});

try {
		Thread.sleep(2000);
} catch (InterruptedException e) {
		e.printStackTrace();
}
// 使用 sum() 获取结果
System.out.println(longAdderData.sum());
```

结合图示理解：

![LongAdder实现](https://tva1.sinaimg.cn/large/0081Kckwly1gm1juddjf1j30ck0j0dh8.jpg)

但是，如果使用了 LongAdder 原子类，当然其底层也是基于 CAS 机制实现的。LongAdder 内部维护了 base 变量和 Cell[] 数组，当多线程并发写的情况下，各个线程都在写入自己的 Cell 中，LongAdder 操作后返回的是个近似准确的值，最终也会返回一个准确的值。

换句话说，使用了 LongAdder 后获取的结果并不是实时的，对实时性要求高的还是建议使用其他的原子类，如 AtomicInteger 等。

**volatile 关键字方案？**

可能还有朋友会说，还想到另外一种方案：使用** `volatile`** 关键字啊。

![volatile不能保障原子性](https://tva1.sinaimg.cn/large/0081Kckwly1gm1jubxlo5j30gw0d2my5.jpg)

经过验证，是不可行的，大家可以试试，就本文给出的示例代码直接执行，结果都不等于 200，说明线程仍然是不安全的。

data++ 自增赋值并不是原子的，跟 Java内存模型有关。

在非线程安全的图示中有标注执行线程本地，会有个内存副本，即本地的工作内存，实际执行过程会经过如下几个步骤：

（1）执行线程从本地工作内存读取 data，如果有值直接获取，如果没有值，会从主内存读取，然后将其放到本地工作内存当中。

（2）执行线程在本地工作内存中执行 +1 操作。

（3）将 data 的值写入主内存。

**结论：请记住！**

一个变量简单的读取和赋值操作是原子性的，将一个变量赋值给另外一个变量不是原子性的。

Java内存模型（JMM）仅仅保障了变量的基本读取和赋值操作是原子性的，其他均不会保证的。如果想要使某段代码块要求具备原子性，就需要使用 synchronized 关键字、并发包中的 Lock 锁、并发包中 Atomic 各种类型的原子类来实现，即上面我们提到的**四种方案都是可行的**。

而 `volatile` 关键字修饰的变量，恰恰是不能保障原子性的，仅能保障可见性和有序性。

#### **CAS原理剖析**

CAS 被认为是一种乐观锁，有乐观锁，相对应的是悲观锁。

在上述示例中，我们使用了 synchronized，如果在线程竞争压力大的情况下，synchronized 内部会升级为重量级锁，此时仅能有一个线程进入代码块执行，如果这把锁始终不能释放，其他线程会一直阻塞等待下去。此时，可以认为是悲观锁。

悲观锁会因线程一直阻塞导致系统上下文切换，系统的性能开销大。

那么，我们可以用乐观锁来解决，所谓的乐观锁，其实就是一种思想。

乐观锁，会以一种更加乐观的态度对待事情，认为自己可以操作成功。当多个线程操作同一个共享资源时，仅能有一个线程同一时间获得锁成功，在乐观锁中，其他线程发现自己无法成功获得锁，并不会像悲观锁那样阻塞线程，而是直接返回，可以去选择再次重试获得锁，也可以直接退出。

CAS 正是乐观锁的核心算法实现。

在示例代码的方案中都提到了 AtomicInteger、LongAdder、Lock锁底层，此外，当然还包括 java.util.concurrent.atomic 并发包下的所有原子类都是基于 CAS 来实现的。

以 AtomicInteger 原子整型类为例，一起来分析下 CAS 底层实现机制。

```
atomicData.incrementAndGet()
```

源码如下所示：

```
// 提供自增易用的方法，返回增加1后的值
public final int incrementAndGet() {
		return unsafe.getAndAddInt(this, valueOffset, 1) + 1;
}

// 额外提供的compareAndSet方法
public final boolean compareAndSet(int expect, int update) {
		return unsafe.compareAndSwapInt(this, valueOffset, expect, update);
}

// Unsafe 类的提供的方法
public final int getAndAddInt (Object o,long offset, int delta){
		int v;
		do {
				v = getIntVolatile(o, offset);
		} while (!weakCompareAndSetInt(o, offset, v, v + delta));
		return v;
}
```

我们看到了 AtomicInteger 内部方法都是基于 Unsafe 类实现的，Unsafe 类是个跟底层硬件CPU指令通讯的复制工具类。

由这段代码看到：

```
unsafe.compareAndSwapInt(this, valueOffset, expect, update)
```

所谓的 CAS，其实是个简称，全称是 Compare And Swap，对比之后交换数据。
上面的方法，有几个重要的参数：

（1）this，Unsafe 对象本身，需要通过这个类来获取 value 的内存偏移地址。

（2）valueOffset，value 变量的内存偏移地址。

（3）expect，期望更新的值。

（4）update，要更新的最新值。

如果原子变量中的 value 值等于 expect，则使用 update 值更新该值并返回 true，否则返回 false。

再看如何获得 valueOffset的：

```
// Unsafe实例
private static final Unsafe unsafe = Unsafe.getUnsafe();
private static final long valueOffset;

static {
		try {
			  // 获得value在AtomicInteger中的偏移量
				valueOffset = unsafe.objectFieldOffset
						(AtomicInteger.class.getDeclaredField("value"));
		} catch (Exception ex) { throw new Error(ex); }
}
// 实际变量的值
private volatile int value;
```

这里看到了 value 实际的变量，是由 **volatile** 关键字修饰的，为了保证在多线程下的**内存可见性**。

为何能通过 Unsafe.getUnsafe() 方法能获得 Unsafe 类的实例？其实因为 AtomicInteger 类也在 **rt.jar **包下面的，所以 AtomicInteger 类就是通过 **Bootstrap 根类加载器**进行加载的。

源码如下所示：

```
@CallerSensitive
public static Unsafe getUnsafe() {
		Class var0 = Reflection.getCallerClass();
		// Bootstrap 类加载器是C++的，正常返回null，否则就抛异常。
		if (!VM.isSystemDomainLoader(var0.getClassLoader())) {
				throw new SecurityException("Unsafe");
		} else {
				return theUnsafe;
		}
}
```

类加载器委托关系：

![类加载器](https://tva1.sinaimg.cn/large/0081Kckwly1gm1ju9ovxtj30m8092jrx.jpg)

#### **CPU如何实现原子操作**

CPU 处理器速度远远大于在主内存中的，为了解决速度差异，在他们之间架设了多级缓存，如 L1、L2、L3 级别的缓存，这些缓存离CPU越近就越快，将频繁操作的数据缓存到这里，加快访问速度 ，如下图所示：

![CPU架构](https://tva1.sinaimg.cn/large/0081Kckwly1gm1ju9zvpbj30m80dsq3q.jpg)

现在都是多核 CPU 处理器，每个 CPU 处理器内维护了一块字节的内存，每个内核内部维护着一块字节的缓存，当多线程并发读写时，就会出现缓存数据不一致的情况。

此时，处理器提供：

- **总线锁定**

当一个处理器要操作共享变量时，在 BUS 总线上发出一个 Lock 信号，其他处理就无法操作这个共享变量了。

缺点很明显，总线锁定在阻塞其它处理器获取该共享变量的操作请求时，也可能会导致大量阻塞，从而增加系统的性能开销。

- **缓存锁定 **

后来的处理器都提供了缓存锁定机制，也就说当某个处理器对缓存中的共享变量进行了操作，其他处理器会有个嗅探机制，将其他处理器的该共享变量的缓存失效，待其他线程读取时会重新从主内存中读取最新的数据，基于 MESI 缓存一致性协议来实现的。

现代的处理器基本都支持和使用的缓存锁定机制。

**注意：**

有如下两种情况处理器不会使用缓存锁定：

（1）当操作的数据跨多个缓存行，或没被缓存在处理器内部，则处理器会使用总线锁定。

（2）有些处理器不支持缓存锁定，比如：Intel 486 和 Pentium 处理器也会调用总线锁定。

#### **解密CAS底层指令**

其实，掌握以上内容，对于 CAS 机制的理解相对来说算是比较清楚了。

当然，如果感兴趣，也可以继续深入学习用到了哪些硬件 CPU 指令。

底层硬件通过将 CAS 里的多个操作在硬件层面语义实现上，通过一条处理器指令保证了原子性操作。这些指令如下所示：

（1）测试并设置（Tetst-and-Set）

（2）获取并增加（Fetch-and-Increment）

（3）交换（Swap）

（4）比较并交换（Compare-and-Swap）

（5）加载链接/条件存储（Load-Linked/Store-Conditional）

前面三条大部分处理器已经实现，后面的两条是现代处理器当中新增加的。而且根据不同的体系结构，指令存在着明显差异。

在IA64，x86 指令集中有 **cmpxchg** 指令完成 CAS 功能，在 sparc-TSO 也有 **casa** 指令实现，而在 ARM 和 PowerPC 架构下，则需要使用一对 **ldrex/strex** 指令来完成 LL/SC 的功能。在精简指令集的体系架构中，则通常是靠一对儿指令，如：**load and reserve** 和 **store conditional ** 实现的，在大多数处理器上 CAS 都是个非常轻量级的操作，这也是其优势所在。

sun.misc.Unsafe 中 CAS 的核心方法：

```
public final native boolean compareAndSwapObject(Object var1, long var2, Object var4, Object var5);

public final native boolean compareAndSwapInt(Object var1, long var2, int var4, int var5);

public final native boolean compareAndSwapLong(Object var1, long var2, long var4, long var6);
```

这三个方法可以对应去查看 openjdk 的 hotspot 源码：

源码位置：`hotspot/src/share/vm/prims/unsafe.cpp`

```
#define FN_PTR(f) CAST_FROM_FN_PTR(void*, &f)

{CC"compareAndSwapObject", CC"("OBJ"J"OBJ""OBJ")Z",  FN_PTR(Unsafe_CompareAndSwapObject)},

{CC"compareAndSwapInt",  CC"("OBJ"J""I""I"")Z",      FN_PTR(Unsafe_CompareAndSwapInt)},

{CC"compareAndSwapLong", CC"("OBJ"J""J""J"")Z",      FN_PTR(Unsafe_CompareAndSwapLong)},
```

上述三个方法，最终在 hotspot 源码实现中都会调用统一的 cmpxchg 函数，可以在 hotspot 源码中找到核心代码。

源码地址：`hotspot/src/share/vm/runtime/Atomic.cpp`

**cmpxchg 函数源码：**

```
jbyte Atomic::cmpxchg(jbyte exchange_value, volatile jbyte*dest, jbyte compare_value) {
		 assert (sizeof(jbyte) == 1,"assumption.");
		 uintptr_t dest_addr = (uintptr_t) dest;
		 uintptr_t offset = dest_addr % sizeof(jint);
		 volatile jint*dest_int = ( volatile jint*)(dest_addr - offset);
		 // 对象当前值
		 jint cur = *dest_int;
		 // 当前值cur的地址
		 jbyte * cur_as_bytes = (jbyte *) ( & cur);
		 // new_val地址
		 jint new_val = cur;
		 jbyte * new_val_as_bytes = (jbyte *) ( & new_val);
		  // new_val存exchange_value，后面修改则直接从new_val中取值
		 new_val_as_bytes[offset] = exchange_value;
		 // 比较当前值与期望值，如果相同则更新，不同则直接返回
		 while (cur_as_bytes[offset] == compare_value) {
		  // 调用汇编指令cmpxchg执行CAS操作，期望值为cur，更新值为new_val
			 jint res = cmpxchg(new_val, dest_int, cur);
			 if (res == cur) break;
			 cur = res;
			 new_val = cur;
			 new_val_as_bytes[offset] = exchange_value;
		 }
		 // 返回当前值
		 return cur_as_bytes[offset];
}
```

源码中具体变量添加了注释，因为都是 C++ 代码，所以作为了解即可 ~

```
jint res = cmpxchg(new_val, dest_int, cur);
```

这里就是调用了汇编指令 cmpxchg 了，其中也是包含了三个参数，跟CAS上的参数能对应上。

#### **总结**

任何技术都要找到适合的场景，都不是万能的，CAS 机制也一样，也有副作用。

**问题1：**

作为乐观锁的一种实现，当多线程竞争资源激烈的情况下，而且锁定的资源处理耗时，那么其他线程就要考虑自旋的次数限制，避免过度的消耗 CPU。

另外，可以考虑上文示例代码中提到的 LongAdder 来解决，LongAdder 以空间换时间的方式，来解决 CAS 大量失败后长时间占用 CPU 资源，加大了系统性能开销的问题。

**问题2：**

**A-->B--->A** 问题，假设有一个变量 A ，修改为B，然后又修改为了 A，实际已经修改过了，但 CAS 可能无法感知，造成了不合理的值修改操作。

整数类型还好，如果是对象引用类型，包含了多个变量，那怎么办？加个版本号或时间戳呗，没问题！

JDK 中 java.util.concurrent.atomic 并发包下，提供了 **AtomicStampedReference**，通过为引用建立个 Stamp 类似版本号的方式，确保 CAS 操作的正确性。

希望此文大家收藏消化，CAS 在JDK并发包底层实现中是个非常重要的算法。

**撰文不易，文章中有什么问题还请指正！**

欢迎关注我的公众号，扫二维码关注获得更多精彩文章，与你一同成长~

![Java爱好者社区](https://img2018.cnblogs.com/blog/836787/201912/836787-20191202133232747-429791575.png)

---

# [Java并发之AQS详解](https://www.cnblogs.com/waterystone/p/4920797.html)

# 一、概述

　　谈到并发，不得不谈ReentrantLock；而谈到ReentrantLock，不得不谈AbstractQueuedSynchronizer（AQS）！

　　类如其名，抽象的队列式的同步器，AQS定义了一套多线程访问共享资源的同步器框架，许多同步类实现都依赖于它，如常用的ReentrantLock/Semaphore/CountDownLatch...。

　　以下是本文的目录大纲：

1. 1. 概述
   2. 框架
   3. 源码详解
   4. 简单应用

　　若有不正之处，请谅解和批评指正，不胜感激。

　　请尊重作者劳动成果，转载请标明原文链接（原文持续更新，建议阅读原文）：http://www.cnblogs.com/waterystone/p/4920797.html

# 二、框架

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm1jvs2o2pj30no0b5aad.jpg)

　　它维护了一个volatile int state（代表共享资源）和一个FIFO线程等待队列（多线程争用资源被阻塞时会进入此队列）。这里volatile是核心关键词，具体volatile的语义，在此不述。state的访问方式有三种:

- getState()
- setState()
- compareAndSetState()

　　AQS定义两种资源共享方式：Exclusive（独占，只有一个线程能执行，如ReentrantLock）和Share（共享，多个线程可同时执行，如Semaphore/CountDownLatch）。

　　不同的自定义同步器争用共享资源的方式也不同。**自定义同步器在实现时只需要实现共享资源state的获取与释放方式即可**，至于具体线程等待队列的维护（如获取资源失败入队/唤醒出队等），AQS已经在顶层实现好了。自定义同步器实现时主要实现以下几种方法：

- isHeldExclusively()：该线程是否正在独占资源。只有用到condition才需要去实现它。
- tryAcquire(int)：独占方式。尝试获取资源，成功则返回true，失败则返回false。
- tryRelease(int)：独占方式。尝试释放资源，成功则返回true，失败则返回false。
- tryAcquireShared(int)：共享方式。尝试获取资源。负数表示失败；0表示成功，但没有剩余可用资源；正数表示成功，且有剩余资源。
- tryReleaseShared(int)：共享方式。尝试释放资源，如果释放后允许唤醒后续等待结点返回true，否则返回false。

　　以ReentrantLock为例，state初始化为0，表示未锁定状态。A线程lock()时，会调用tryAcquire()独占该锁并将state+1。此后，其他线程再tryAcquire()时就会失败，直到A线程unlock()到state=0（即释放锁）为止，其它线程才有机会获取该锁。当然，释放锁之前，A线程自己是可以重复获取此锁的（state会累加），这就是可重入的概念。但要注意，获取多少次就要释放多么次，这样才能保证state是能回到零态的。

　　再以CountDownLatch以例，任务分为N个子线程去执行，state也初始化为N（注意N要与线程个数一致）。这N个子线程是并行执行的，每个子线程执行完后countDown()一次，state会CAS减1。等到所有子线程都执行完后(即state=0)，会unpark()主调用线程，然后主调用线程就会从await()函数返回，继续后余动作。

　　一般来说，自定义同步器要么是独占方法，要么是共享方式，他们也只需实现tryAcquire-tryRelease、tryAcquireShared-tryReleaseShared中的一种即可。但AQS也支持自定义同步器同时实现独占和共享两种方式，如ReentrantReadWriteLock。

# 三、源码详解

　　本节开始讲解AQS的源码实现。依照acquire-release、acquireShared-releaseShared的次序来。

## 3.0 结点状态waitStatus

   这里我们说下Node。Node结点是对每一个等待获取资源的线程的封装，其包含了需要同步的线程本身及其等待状态，如是否被阻塞、是否等待唤醒、是否已经被取消等。变量waitStatus则表示当前Node结点的等待状态，共有5种取值CANCELLED、SIGNAL、CONDITION、PROPAGATE、0。

- **CANCELLED**(1)：表示当前结点已取消调度。当timeout或被中断（响应中断的情况下），会触发变更为此状态，进入该状态后的结点将不会再变化。
- **SIGNAL**(-1)：表示后继结点在等待当前结点唤醒。后继结点入队时，会将前继结点的状态更新为SIGNAL。
- **CONDITION**(-2)：表示结点等待在Condition上，当其他线程调用了Condition的signal()方法后，CONDITION状态的结点将**从等待队列转移到同步队列中**，等待获取同步锁。
- **PROPAGATE**(-3)：共享模式下，前继结点不仅会唤醒其后继结点，同时也可能会唤醒后继的后继结点。
- **0**：新结点入队时的默认状态。

注意，**负值表示结点处于有效等待状态，而正值表示结点已被取消。所以源码中很多地方用>0、<0来判断结点的状态是否正常**。

## 3.1 acquire(int)

　　此方法是独占模式下线程获取共享资源的顶层入口。如果获取到资源，线程直接返回，否则进入等待队列，直到获取到资源为止，且整个过程忽略中断的影响。这也正是lock()的语义，当然不仅仅只限于lock()。获取到资源后，线程就可以去执行其临界区代码了。下面是acquire()的源码：

```
1 public final void acquire(int arg) {
2     if (!tryAcquire(arg) &&
3         acquireQueued(addWaiter(Node.EXCLUSIVE), arg))
4         selfInterrupt();
5 }
```

 

　　函数流程如下：

1. 1. tryAcquire()尝试直接去获取资源，如果成功则直接返回（这里体现了非公平锁，每个线程获取锁时会尝试直接抢占加塞一次，而CLH队列中可能还有别的线程在等待）；
   2. addWaiter()将该线程加入等待队列的尾部，并标记为独占模式；
   3. acquireQueued()使线程阻塞在等待队列中获取资源，一直获取到资源后才返回。如果在整个等待过程中被中断过，则返回true，否则返回false。
   4. 如果线程在等待过程中被中断过，它是不响应的。只是获取资源后才再进行自我中断selfInterrupt()，将中断补上。

　　这时单凭这4个抽象的函数来看流程还有点朦胧，不要紧，看完接下来的分析后，你就会明白了。就像《大话西游》里唐僧说的：等你明白了舍生取义的道理，你自然会回来和我唱这首歌的。

### 3.1.1 tryAcquire(int)

　　此方法尝试去获取独占资源。如果获取成功，则直接返回true，否则直接返回false。这也正是tryLock()的语义，还是那句话，当然不仅仅只限于tryLock()。如下是tryAcquire()的源码：

```
1     protected boolean tryAcquire(int arg) {
2         throw new UnsupportedOperationException();
3     }
```

 

　　什么？直接throw异常？说好的功能呢？好吧，**还记得概述里讲的AQS只是一个框架，具体资源的获取/释放方式交由自定义同步器去实现吗？**就是这里了！！！AQS这里只定义了一个接口，具体资源的获取交由自定义同步器去实现了（通过state的get/set/CAS）！！！至于能不能重入，能不能加塞，那就看具体的自定义同步器怎么去设计了！！！当然，自定义同步器在进行资源访问时要考虑线程安全的影响。

　　这里之所以没有定义成abstract，是因为独占模式下只用实现tryAcquire-tryRelease，而共享模式下只用实现tryAcquireShared-tryReleaseShared。如果都定义成abstract，那么每个模式也要去实现另一模式下的接口。说到底，Doug Lea还是站在咱们开发者的角度，尽量减少不必要的工作量。

### 3.1.2 addWaiter(Node)

　　此方法用于将当前线程加入到等待队列的队尾，并返回当前线程所在的结点。还是上源码吧：

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
 1 private Node addWaiter(Node mode) {
 2     //以给定模式构造结点。mode有两种：EXCLUSIVE（独占）和SHARED（共享）
 3     Node node = new Node(Thread.currentThread(), mode);
 4     
 5     //尝试快速方式直接放到队尾。
 6     Node pred = tail;
 7     if (pred != null) {
 8         node.prev = pred;
 9         if (compareAndSetTail(pred, node)) {
10             pred.next = node;
11             return node;
12         }
13     }
14     
15     //上一步失败则通过enq入队。
16     enq(node);
17     return node;
18 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 不用再说了，直接看注释吧。

#### 3.1.2.1 enq(Node)

 　此方法用于将node加入队尾。源码如下：

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
 1 private Node enq(final Node node) {
 2     //CAS"自旋"，直到成功加入队尾
 3     for (;;) {
 4         Node t = tail;
 5         if (t == null) { // 队列为空，创建一个空的标志结点作为head结点，并将tail也指向它。
 6             if (compareAndSetHead(new Node()))
 7                 tail = head;
 8         } else {//正常流程，放入队尾
 9             node.prev = t;
10             if (compareAndSetTail(t, node)) {
11                 t.next = node;
12                 return t;
13             }
14         }
15     }
16 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 

如果你看过AtomicInteger.getAndIncrement()函数源码，那么相信你一眼便看出这段代码的精华。**CAS自旋volatile变量**，是一种很经典的用法。还不太了解的，自己去百度一下吧。

### 3.1.3 acquireQueued(Node, int)

　　OK，通过tryAcquire()和addWaiter()，该线程获取资源失败，已经被放入等待队列尾部了。聪明的你立刻应该能想到该线程下一部该干什么了吧：**进入等待状态休息，直到其他线程彻底释放资源后唤醒自己，自己再拿到资源，然后就可以去干自己想干的事了**。没错，就是这样！是不是跟医院排队拿号有点相似~~acquireQueued()就是干这件事：**在等待队列中排队拿号（中间没其它事干可以休息），直到拿到号后再返回**。这个函数非常关键，还是上源码吧：

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
 1 final boolean acquireQueued(final Node node, int arg) {
 2     boolean failed = true;//标记是否成功拿到资源
 3     try {
 4         boolean interrupted = false;//标记等待过程中是否被中断过
 5         
 6         //又是一个“自旋”！
 7         for (;;) {
 8             final Node p = node.predecessor();//拿到前驱
 9             //如果前驱是head，即该结点已成老二，那么便有资格去尝试获取资源（可能是老大释放完资源唤醒自己的，当然也可能被interrupt了）。
10             if (p == head && tryAcquire(arg)) {
11                 setHead(node);//拿到资源后，将head指向该结点。所以head所指的标杆结点，就是当前获取到资源的那个结点或null。
12                 p.next = null; // setHead中node.prev已置为null，此处再将head.next置为null，就是为了方便GC回收以前的head结点。也就意味着之前拿完资源的结点出队了！
13                 failed = false; // 成功获取资源
14                 return interrupted;//返回等待过程中是否被中断过
15             }
16             
17             //如果自己可以休息了，就通过park()进入waiting状态，直到被unpark()。如果不可中断的情况下被中断了，那么会从park()中醒过来，发现拿不到资源，从而继续进入park()等待。
18             if (shouldParkAfterFailedAcquire(p, node) &&
19                 parkAndCheckInterrupt())
20                 interrupted = true;//如果等待过程中被中断过，哪怕只有那么一次，就将interrupted标记为true
21         }
22     } finally {
23         if (failed) // 如果等待过程中没有成功获取资源（如timeout，或者可中断的情况下被中断了），那么取消结点在队列中的等待。
24             cancelAcquire(node);
25     }
26 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

　 

到这里了，我们先不急着总结acquireQueued()的函数流程，先看看shouldParkAfterFailedAcquire()和parkAndCheckInterrupt()具体干些什么。

#### 3.1.3.1 shouldParkAfterFailedAcquire(Node, Node)

　　此方法主要用于检查状态，看看自己是否真的可以去休息了（进入waiting状态，如果线程状态转换不熟，可以参考本人上一篇写的[Thread详解](http://www.cnblogs.com/waterystone/p/4920007.html)），万一队列前边的线程都放弃了只是瞎站着，那也说不定，对吧！

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
 1 private static boolean shouldParkAfterFailedAcquire(Node pred, Node node) {
 2     int ws = pred.waitStatus;//拿到前驱的状态
 3     if (ws == Node.SIGNAL)
 4         //如果已经告诉前驱拿完号后通知自己一下，那就可以安心休息了
 5         return true;
 6     if (ws > 0) {
 7         /*
 8          * 如果前驱放弃了，那就一直往前找，直到找到最近一个正常等待的状态，并排在它的后边。
 9          * 注意：那些放弃的结点，由于被自己“加塞”到它们前边，它们相当于形成一个无引用链，稍后就会被保安大叔赶走了(GC回收)！
10          */
11         do {
12             node.prev = pred = pred.prev;
13         } while (pred.waitStatus > 0);
14         pred.next = node;
15     } else {
16          //如果前驱正常，那就把前驱的状态设置成SIGNAL，告诉它拿完号后通知自己一下。有可能失败，人家说不定刚刚释放完呢！
17         compareAndSetWaitStatus(pred, ws, Node.SIGNAL);
18     }
19     return false;
20 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 

整个流程中，如果前驱结点的状态不是SIGNAL，那么自己就不能安心去休息，需要去找个安心的休息点，同时可以再尝试下看有没有机会轮到自己拿号。

#### 3.1.3.2 parkAndCheckInterrupt()

　　如果线程找好安全休息点后，那就可以安心去休息了。此方法就是让线程去休息，真正进入等待状态。

```
1 private final boolean parkAndCheckInterrupt() {
2     LockSupport.park(this);//调用park()使线程进入waiting状态
3     return Thread.interrupted();//如果被唤醒，查看自己是不是被中断的。
4 }
```

 　park()会让当前线程进入waiting状态。在此状态下，有两种途径可以唤醒该线程：1）被unpark()；2）被interrupt()。（再说一句，如果线程状态转换不熟，可以参考本人写的[Thread详解](http://www.cnblogs.com/waterystone/p/4920007.html)）。需要注意的是，Thread.interrupted()会清除当前线程的中断标记位。 

#### 3.1.3.3 小结

　　OK，看了shouldParkAfterFailedAcquire()和parkAndCheckInterrupt()，现在让我们再回到acquireQueued()，总结下该函数的具体流程：

1. 结点进入队尾后，检查状态，找到安全休息点；
2. 调用park()进入waiting状态，等待unpark()或interrupt()唤醒自己；
3. 被唤醒后，看自己是不是有资格能拿到号。如果拿到，head指向当前结点，并返回从入队到拿到号的整个过程中是否被中断过；如果没拿到，继续流程1。

 

### 3.1.4 小结

　　OKOK，acquireQueued()分析完之后，我们接下来再回到acquire()！再贴上它的源码吧：

```
1 public final void acquire(int arg) {
2     if (!tryAcquire(arg) &&
3         acquireQueued(addWaiter(Node.EXCLUSIVE), arg))
4         selfInterrupt();
5 }
```

再来总结下它的流程吧：

1. 调用自定义同步器的tryAcquire()尝试直接去获取资源，如果成功则直接返回；
2. 没成功，则addWaiter()将该线程加入等待队列的尾部，并标记为独占模式；
3. acquireQueued()使线程在等待队列中休息，有机会时（轮到自己，会被unpark()）会去尝试获取资源。获取到资源后才返回。如果在整个等待过程中被中断过，则返回true，否则返回false。
4. 如果线程在等待过程中被中断过，它是不响应的。只是获取资源后才再进行自我中断selfInterrupt()，将中断补上。

由于此函数是重中之重，我再用流程图总结一下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm1jvo8tf9j30tw06g74r.jpg)

至此，acquire()的流程终于算是告一段落了。这也就是ReentrantLock.lock()的流程，不信你去看其lock()源码吧，整个函数就是一条acquire(1)！！！

 

## 3.2 release(int)

 　上一小节已经把acquire()说完了，这一小节就来讲讲它的反操作release()吧。此方法是独占模式下线程释放共享资源的顶层入口。它会释放指定量的资源，如果彻底释放了（即state=0）,它会唤醒等待队列里的其他线程来获取资源。这也正是unlock()的语义，当然不仅仅只限于unlock()。下面是release()的源码：

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
1 public final boolean release(int arg) {
2     if (tryRelease(arg)) {
3         Node h = head;//找到头结点
4         if (h != null && h.waitStatus != 0)
5             unparkSuccessor(h);//唤醒等待队列里的下一个线程
6         return true;
7     }
8     return false;
9 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 

　　逻辑并不复杂。它调用tryRelease()来释放资源。有一点需要注意的是，**它是根据tryRelease()的返回值来判断该线程是否已经完成释放掉资源了！所以自定义同步器在设计tryRelease()的时候要明确这一点！！**

### 3.2.1 tryRelease(int)

　　此方法尝试去释放指定量的资源。下面是tryRelease()的源码：

```
1 protected boolean tryRelease(int arg) {
2     throw new UnsupportedOperationException();
3 }
```

 

　　跟tryAcquire()一样，这个方法是需要独占模式的自定义同步器去实现的。正常来说，tryRelease()都会成功的，因为这是独占模式，该线程来释放资源，那么它肯定已经拿到独占资源了，直接减掉相应量的资源即可(state-=arg)，也不需要考虑线程安全的问题。但要注意它的返回值，上面已经提到了，**release()是根据tryRelease()的返回值来判断该线程是否已经完成释放掉资源了！**所以自义定同步器在实现时，如果已经彻底释放资源(state=0)，要返回true，否则返回false。

### 3.2.2 unparkSuccessor(Node)

　　此方法用于唤醒等待队列中下一个线程。下面是源码：

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
 1 private void unparkSuccessor(Node node) {
 2     //这里，node一般为当前线程所在的结点。
 3     int ws = node.waitStatus;
 4     if (ws < 0)//置零当前线程所在的结点状态，允许失败。
 5         compareAndSetWaitStatus(node, ws, 0);
 6 
 7     Node s = node.next;//找到下一个需要唤醒的结点s
 8     if (s == null || s.waitStatus > 0) {//如果为空或已取消
 9         s = null;
10         for (Node t = tail; t != null && t != node; t = t.prev) // 从后向前找。
11             if (t.waitStatus <= 0)//从这里可以看出，<=0的结点，都是还有效的结点。
12                 s = t;
13     }
14     if (s != null)
15         LockSupport.unpark(s.thread);//唤醒
16 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 

　　这个函数并不复杂。一句话概括：**用unpark()唤醒等待队列中最前边的那个未放弃线程**，这里我们也用s来表示吧。此时，再和acquireQueued()联系起来，s被唤醒后，进入if (p == head && tryAcquire(arg))的判断（即使p!=head也没关系，它会再进入shouldParkAfterFailedAcquire()寻找一个安全点。这里既然s已经是等待队列中最前边的那个未放弃线程了，那么通过shouldParkAfterFailedAcquire()的调整，s也必然会跑到head的next结点，下一次自旋p==head就成立啦），然后s把自己设置成head标杆结点，表示自己已经获取到资源了，acquire()也返回了！！And then, DO what you WANT!

### 3.2.3 小结

　　release()是独占模式下线程释放共享资源的顶层入口。它会释放指定量的资源，如果彻底释放了（即state=0）,它会唤醒等待队列里的其他线程来获取资源。

 

   74楼的朋友提了一个非常有趣的问题：如果获取锁的线程在release时异常了，没有unpark队列中的其他结点，这时队列中的其他结点会怎么办？是不是没法再被唤醒了？

   答案是**YES**（测试程序详见76楼）！！！这时，队列中等待锁的线程将永远处于park状态，无法再被唤醒！！！但是我们再回头想想，获取锁的线程在什么情形下会release抛出异常呢？？

1. 线程突然死掉了？可以通过thread.stop来停止线程的执行，但该函数的执行条件要严苛的多，而且函数注明是非线程安全的，已经标明Deprecated；
2. 线程被interupt了？线程在运行态是不响应中断的，所以也不会抛出异常；
3. release代码有bug，抛出异常了？目前来看，Doug Lea的release方法还是比较健壮的，没有看出能引发异常的情形（如果有，恐怕早被用户吐槽了）。**除非自己写的tryRelease()有bug，那就没啥说的，自己写的bug只能自己含着泪去承受了**。

## 3.3 acquireShared(int)

　　此方法是共享模式下线程获取共享资源的顶层入口。它会获取指定量的资源，获取成功则直接返回，获取失败则进入等待队列，直到获取到资源为止，整个过程忽略中断。下面是acquireShared()的源码：

```
1 public final void acquireShared(int arg) {
2     if (tryAcquireShared(arg) < 0)
3         doAcquireShared(arg);
4 }
```

 

　　这里tryAcquireShared()依然需要自定义同步器去实现。但是AQS已经把其返回值的语义定义好了：负值代表获取失败；0代表获取成功，但没有剩余资源；正数表示获取成功，还有剩余资源，其他线程还可以去获取。所以这里acquireShared()的流程就是：

1. 1. tryAcquireShared()尝试获取资源，成功则直接返回；
   2. 失败则通过doAcquireShared()进入等待队列，直到获取到资源为止才返回。

### 3.3.1 doAcquireShared(int)

　　此方法用于将当前线程加入等待队列尾部休息，直到其他线程释放资源唤醒自己，自己成功拿到相应量的资源后才返回。下面是doAcquireShared()的源码：

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
 1 private void doAcquireShared(int arg) {
 2     final Node node = addWaiter(Node.SHARED);//加入队列尾部
 3     boolean failed = true;//是否成功标志
 4     try {
 5         boolean interrupted = false;//等待过程中是否被中断过的标志
 6         for (;;) {
 7             final Node p = node.predecessor();//前驱
 8             if (p == head) {//如果到head的下一个，因为head是拿到资源的线程，此时node被唤醒，很可能是head用完资源来唤醒自己的
 9                 int r = tryAcquireShared(arg);//尝试获取资源
10                 if (r >= 0) {//成功
11                     setHeadAndPropagate(node, r);//将head指向自己，还有剩余资源可以再唤醒之后的线程
12                     p.next = null; // help GC
13                     if (interrupted)//如果等待过程中被打断过，此时将中断补上。
14                         selfInterrupt();
15                     failed = false;
16                     return;
17                 }
18             }
19             
20             //判断状态，寻找安全点，进入waiting状态，等着被unpark()或interrupt()
21             if (shouldParkAfterFailedAcquire(p, node) &&
22                 parkAndCheckInterrupt())
23                 interrupted = true;
24         }
25     } finally {
26         if (failed)
27             cancelAcquire(node);
28     }
29 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 

　　有木有觉得跟acquireQueued()很相似？对，其实流程并没有太大区别。只不过这里将补中断的selfInterrupt()放到doAcquireShared()里了，而独占模式是放到acquireQueued()之外，其实都一样，不知道Doug Lea是怎么想的。

　　跟独占模式比，还有一点需要注意的是，这里只有线程是head.next时（“老二”），才会去尝试获取资源，有剩余的话还会唤醒之后的队友。那么问题就来了，假如老大用完后释放了5个资源，而老二需要6个，老三需要1个，老四需要2个。老大先唤醒老二，老二一看资源不够，他是把资源让给老三呢，还是不让？答案是否定的！老二会继续park()等待其他线程释放资源，也更不会去唤醒老三和老四了。独占模式，同一时刻只有一个线程去执行，这样做未尝不可；但共享模式下，多个线程是可以同时执行的，现在因为老二的资源需求量大，而把后面量小的老三和老四也都卡住了。当然，这并不是问题，只是AQS保证严格按照入队顺序唤醒罢了（保证公平，但降低了并发）。

 

#### 3.3.1.1 setHeadAndPropagate(Node, int)

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
 1 private void setHeadAndPropagate(Node node, int propagate) {
 2     Node h = head; 
 3     setHead(node);//head指向自己
 4      //如果还有剩余量，继续唤醒下一个邻居线程
 5     if (propagate > 0 || h == null || h.waitStatus < 0) {
 6         Node s = node.next;
 7         if (s == null || s.isShared())
 8             doReleaseShared();
 9     }
10 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 

　　此方法在setHead()的基础上多了一步，就是自己苏醒的同时，如果条件符合（比如还有剩余资源），还会去唤醒后继结点，毕竟是共享模式！

　　doReleaseShared()我们留着下一小节的releaseShared()里来讲。

 

### 3.3.2 小结

　　OK，至此，acquireShared()也要告一段落了。让我们再梳理一下它的流程：

1. 

2. 1. tryAcquireShared()尝试获取资源，成功则直接返回；
   2. 失败则通过doAcquireShared()进入等待队列park()，直到被unpark()/interrupt()并成功获取到资源才返回。整个等待过程也是忽略中断的。

　　其实跟acquire()的流程大同小异，只不过多了个**自己拿到资源后，还会去唤醒后继队友的操作（这才是共享嘛）**。

## 3.4 releaseShared()

　　上一小节已经把acquireShared()说完了，这一小节就来讲讲它的反操作releaseShared()吧。此方法是共享模式下线程释放共享资源的顶层入口。它会释放指定量的资源，如果成功释放且允许唤醒等待线程，它会唤醒等待队列里的其他线程来获取资源。下面是releaseShared()的源码：

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
1 public final boolean releaseShared(int arg) {
2     if (tryReleaseShared(arg)) {//尝试释放资源
3         doReleaseShared();//唤醒后继结点
4         return true;
5     }
6     return false;
7 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 

　　此方法的流程也比较简单，一句话：释放掉资源后，唤醒后继。跟独占模式下的release()相似，但有一点稍微需要注意：独占模式下的tryRelease()在完全释放掉资源（state=0）后，才会返回true去唤醒其他线程，这主要是基于独占下可重入的考量；而共享模式下的releaseShared()则没有这种要求，共享模式实质就是控制一定量的线程并发执行，那么拥有资源的线程在释放掉部分资源时就可以唤醒后继等待结点。例如，资源总量是13，A（5）和B（7）分别获取到资源并发运行，C（4）来时只剩1个资源就需要等待。A在运行过程中释放掉2个资源量，然后tryReleaseShared(2)返回true唤醒C，C一看只有3个仍不够继续等待；随后B又释放2个，tryReleaseShared(2)返回true唤醒C，C一看有5个够自己用了，然后C就可以跟A和B一起运行。而ReentrantReadWriteLock读锁的tryReleaseShared()只有在完全释放掉资源（state=0）才返回true，所以自定义同步器可以根据需要决定tryReleaseShared()的返回值。

### 3.4.1 doReleaseShared()

　　此方法主要用于唤醒后继。下面是它的源码：

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
 1 private void doReleaseShared() {
 2     for (;;) {
 3         Node h = head;
 4         if (h != null && h != tail) {
 5             int ws = h.waitStatus;
 6             if (ws == Node.SIGNAL) {
 7                 if (!compareAndSetWaitStatus(h, Node.SIGNAL, 0))
 8                     continue;
 9                 unparkSuccessor(h);//唤醒后继
10             }
11             else if (ws == 0 &&
12                      !compareAndSetWaitStatus(h, 0, Node.PROPAGATE))
13                 continue;
14         }
15         if (h == head)// head发生变化
16             break;
17     }
18 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 

 

## 3.5 小结

　　本节我们详解了独占和共享两种模式下获取-释放资源(acquire-release、acquireShared-releaseShared)的源码，相信大家都有一定认识了。值得注意的是，acquire()和acquireShared()两种方法下，线程在等待队列中都是忽略中断的。AQS也支持响应中断的，acquireInterruptibly()/acquireSharedInterruptibly()即是，相应的源码跟acquire()和acquireShared()差不多，这里就不再详解了。

 

# 四、简单应用

　　通过前边几个章节的学习，相信大家已经基本理解AQS的原理了。这里再将“框架”一节中的一段话复制过来：

　　不同的自定义同步器争用共享资源的方式也不同。**自定义同步器在实现时只需要实现共享资源state的获取与释放方式即可**，至于具体线程等待队列的维护（如获取资源失败入队/唤醒出队等），AQS已经在顶层实现好了。自定义同步器实现时主要实现以下几种方法：

- isHeldExclusively()：该线程是否正在独占资源。只有用到condition才需要去实现它。
- tryAcquire(int)：独占方式。尝试获取资源，成功则返回true，失败则返回false。
- tryRelease(int)：独占方式。尝试释放资源，成功则返回true，失败则返回false。
- tryAcquireShared(int)：共享方式。尝试获取资源。负数表示失败；0表示成功，但没有剩余可用资源；正数表示成功，且有剩余资源。
- tryReleaseShared(int)：共享方式。尝试释放资源，如果释放后允许唤醒后续等待结点返回true，否则返回false。

　　OK，下面我们就以AQS源码里的Mutex为例，讲一下AQS的简单应用。

## 4.1 Mutex（互斥锁）

　　Mutex是一个不可重入的互斥锁实现。锁资源（AQS里的state）只有两种状态：0表示未锁定，1表示锁定。下边是Mutex的核心源码：

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
 1 class Mutex implements Lock, java.io.Serializable {
 2     // 自定义同步器
 3     private static class Sync extends AbstractQueuedSynchronizer {
 4         // 判断是否锁定状态
 5         protected boolean isHeldExclusively() {
 6             return getState() == 1;
 7         }
 8 
 9         // 尝试获取资源，立即返回。成功则返回true，否则false。
10         public boolean tryAcquire(int acquires) {
11             assert acquires == 1; // 这里限定只能为1个量
12             if (compareAndSetState(0, 1)) {//state为0才设置为1，不可重入！
13                 setExclusiveOwnerThread(Thread.currentThread());//设置为当前线程独占资源
14                 return true;
15             }
16             return false;
17         }
18 
19         // 尝试释放资源，立即返回。成功则为true，否则false。
20         protected boolean tryRelease(int releases) {
21             assert releases == 1; // 限定为1个量
22             if (getState() == 0)//既然来释放，那肯定就是已占有状态了。只是为了保险，多层判断！
23                 throw new IllegalMonitorStateException();
24             setExclusiveOwnerThread(null);
25             setState(0);//释放资源，放弃占有状态
26             return true;
27         }
28     }
29 
30     // 真正同步类的实现都依赖继承于AQS的自定义同步器！
31     private final Sync sync = new Sync();
32 
33     //lock<-->acquire。两者语义一样：获取资源，即便等待，直到成功才返回。
34     public void lock() {
35         sync.acquire(1);
36     }
37 
38     //tryLock<-->tryAcquire。两者语义一样：尝试获取资源，要求立即返回。成功则为true，失败则为false。
39     public boolean tryLock() {
40         return sync.tryAcquire(1);
41     }
42 
43     //unlock<-->release。两者语文一样：释放资源。
44     public void unlock() {
45         sync.release(1);
46     }
47 
48     //锁是否占有状态
49     public boolean isLocked() {
50         return sync.isHeldExclusively();
51     }
52 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 

　　同步类在实现时一般都将自定义同步器（sync）定义为内部类，供自己使用；而同步类自己（Mutex）则实现某个接口，对外服务。当然，接口的实现要直接依赖sync，它们在语义上也存在某种对应关系！！而sync只用实现资源state的获取-释放方式tryAcquire-tryRelelase，至于线程的排队、等待、唤醒等，上层的AQS都已经实现好了，我们不用关心。

　　除了Mutex，ReentrantLock/CountDownLatch/Semphore这些同步类的实现方式都差不多，不同的地方就在获取-释放资源的方式tryAcquire-tryRelelase。掌握了这点，AQS的核心便被攻破了！

　　OK，至此，整个AQS的讲解也要落下帷幕了。希望本文能够对学习Java并发编程的同学有所借鉴，中间写的有不对的地方，也欢迎讨论和指正~

- 作者：[水岩](http://www.cnblogs.com/waterystone)
- 出处：http://www.cnblogs.com/waterystone
- 本博客中未标明转载的文章归作者[水岩](http://www.cnblogs.com/waterystone/)和博客园共有，欢迎转载，但未经作者同意必须保留此段声明，且在文章页面明显位置给出原文连接，否则保留追究法律责任的权利。

**如果您觉得本文对您的学习有所帮助，可通过支付宝（左） 或者 微信（右） 来打赏博主，增加博主的写作动力**

![img](https://images.cnblogs.com/cnblogs_com/waterystone/1457939/t_%E6%94%AF%E4%BB%98%E5%AE%9D.png) ![img](https://www.cnblogs.com/images/cnblogs_com/waterystone/1457939/t_%E5%BE%AE%E4%BF%A1.png)

---



### 一、概述

**Lock 有三个实现类，一个是 ReentrantLock, 另两个是 ReentrantReadWriteLock 类中的两个静态内部类 ReadLock 和 WriteLock。**
![在这里插入图片描述](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lp7j062j30q40g7afd.jpg)

LOCK 的实现类其实都是构建在 **AbstractQueuedSynchronizer** 上，为何图中没有用 UML 线表示呢，这是每个 Lock 实现类都持有自己**内部类 Sync 的实例**，而这个 Sync 就是继承 AbstractQueuedSynchronizer (AQS)。为何要实现不同的 Sync 呢？这和每种 Lock 用途相关。另外还有 AQS 的 State 机制。

FairSync 与 NonfairSync 的区别在于，是不是**保证获取锁的公平性**，因为默认是 NonfairSync（非公平性）

------

### 二、AQS

可以看到Lock锁的**底层实现是AQS**

**1.定义**

AQS（AbstractQuenedSynchronizer ），**抽象的队列式同步器**，除了 java 自带的 synchronized 关键字之外的锁机制。

**2.AQS 的核心思想**

如果被请求的共享资源空闲，则将当前请求资源的线程设置为有效的工作线程，并将共享资源设置为锁定状态，如果被请求的共享资源被占用，那么就需要一套**线程阻塞等待**以及**被唤醒**时锁分配的机制，这个机制 AQS 是用 **CLH 队列锁（CLH 锁是一个自旋锁。能确保无饥饿性。提供先来先服务的公平性）\**实现的，即将暂时获取不到锁的线程加入到\**队列**中。

AQS 是将每一条请求共享资源的**线程**封装成一个 CLH 锁队列的一个**结点（Node）**，来实现锁的分配。

**3.实现**

AQS 基于 **CLH 队列**，用 **volatile 修饰共享变量 state**，线程通过 CAS 去改变状态符，成功则获取锁成功，失败则进入等待队列，等待被唤醒。

**4.总结**

- lock 的存储结构：一个 **int 类型状态值（用于锁的状态变更）**，一个**双向链表**（用于存储等待中的线程）
- lock 获取锁的过程：本质上是通过 **CAS 来获取状态值修改**，如果当场没获取到，会将该线程放在线程**等待链表**中。
- lock 释放锁的过程：修改**状态值**，调整等待链表。

可以看到在整个实现过程中，lock 大量使用 CAS + 自旋。因此根据 CAS 特性，lock 建议使用在低锁冲突的情况下。目前 **java1.6 以后，官方对 synchronized 做了大量的锁优化（偏向锁、自旋、轻量级锁）**。因此在非必要的情况下，建议使用 synchronized 做同步操作。



---

https://blog.csdn.net/qq_29373285/article/details/85964460

当多个线程需要访问某个公共资源的时候，我们知道需要通过加锁来保证资源的访问不会出问题。java提供了**两种方式来加锁**，

一种是关键字：synchronized，一种是concurrent包下的lock锁。

synchronized是**java底层支持的**，而concurrent包则是**jdk实现**。

 

关于synchronized的原理可以阅读再有人问你synchronized是什么，就把这篇文章发给他。

在这里，我会用尽可能少的代码，尽可能轻松的文字，尽可能多的图来看看lock的原理。

我们以ReentrantLock为例做分析，其他原理类似。

我把这个过程比喻成一个做菜的过程，有什么菜，做法如何？

**我先列出lock实现过程中的几个关键词：计数值、双向链表、CAS+自旋**

 

我们以**ReentrantLock**为例做分析，其他原理类似。

   可以实现公平锁和非公平锁（ 当有线程竞争锁时，当前线程会首先尝试获得锁而不是在队列中进行排队等候，这对于那些已经在队列中排队的线程来说显得不公平，这也是**非公平锁**的由来），默认情况下为非公平锁。

**实现原理**

**ReentrantLock() 干了啥**

```java
  public ReentrantLock() {
 
        sync = new NonfairSync();
 
    }
```

在lock的构造函数中，定义了一个NonFairSync，

static final class NonfairSync extends Sync

NonfairSync 又是继承于Sync

abstract static class Sync extends AbstractQueuedSynchronizer

 

一步一步往上找，找到了

这个鬼AbstractQueuedSynchronizer（简称AQS），最后这个鬼，又是继承于AbstractOwnableSynchronizer(AOS)，AOS主要是保存获取当前锁的线程对象，代码不多不再展开。最后我们可以看到几个主要类的继承关系：

​                      ![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lohm7s4j30g70cq3z8.jpg)

   FairSync 与 NonfairSync的区别在于，是不是保证获取锁的公平性，因为默认是NonfairSync（**非公平性**），我们以这个为例了解其背后的原理。

其他几个类代码不多，最后的主要代码都是在**AQS**中，我们先看看这个类的主体结构。

**看看AbstractQueuedSynchronizer是个什么**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0log8xiqj30mm0ittcr.jpg)

再看看Node是什么？

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lof6u0lj30j40g4wkg.jpg)

看到这里的同学，是不是有种热泪盈眶的感觉，这尼玛，不就是**双向链表**么？我还记得第一次写这个数据结构的时候，发现居然还有这么神奇的一个东西。

最后我们可以发现锁的存储结构就两个东西:**"****双向链表****" + "****int类型状态****"。**

简单来说，ReenTrantLock的实现是一种**自旋锁**，通过循环调用**CAS操作来实现加锁**。它的性能比较好也是因为**避免了使线程进入内核态的阻塞状态。**想尽办法避免线程进入内核的阻塞状态是我们去分析和理解锁设计的关键钥匙。

需要注意的是，他们的变量都被"**transient**和**volatile**修饰。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lod3ne9j30ln08sgoe.jpg)

 

一个int值，一个双向链表是如何烹饪处理锁这道菜的呢，Doug Lea大神就是大神，

我们接下来看看，**如何获取锁？**

 

**lock.lock()怎么获取锁？**

```java
public void lock() {
 
    sync.lock();
 
}
```

可以看到调用的是，**NonfairSync.lock()**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lo8al73j30yh0ixq77.jpg)

看到这里，我们基本有了一个大概的了解，还记得之前AQS中的**int类型的state值，**

这里就是通过**CAS**（乐观锁）去修改state的值(**锁状态值**)。lock的基本操作还是通过**乐观锁**来实现的。

**获取锁通过CAS**，那么没有获取到锁，**等待获取锁**是如何实现的？我们可以看一下else分支的逻辑，acquire方法：

```java
public final void acquire(int arg) {
 
    if (!tryAcquire(arg) &&
 
        acquireQueued(addWaiter(Node.EXCLUSIVE), arg))
 
        selfInterrupt();
 
}
```

这里干了三件事情：

- tryAcquire：会尝试再次通过CAS获取一次锁。
- addWaiter：将当前线程加入上面锁的双向链表（等待队列）中
- acquireQueued：通过自旋，判断当前队列节点是否可以获取锁。

 

**addWaiter()** **添加当前线程到等待链表中**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lo0u9pbj30tx0fgn0k.jpg)

可以看到，通过CAS确保能够在线程安全的情况下，将当前线程加入到链表的**尾部。**

enq是个自旋+上述逻辑，有兴趣的可以翻翻源码。

 

**acquireQueued()  自旋+CAS****尝试获取锁**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lnyx0gxj30kl0d0q7q.jpg)

可以看到，当当前**线程到头部的时候，尝试CAS更新锁状态，如果更新成功表示该等待线程获取成功。从头部移除。**

 

**每一个线程都在 自旋+CAS**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lnxwp5tj31420kg795.jpg)

最后简要概括一下，获取锁的一个流程

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lnv9c7aj30hb0ihq6f.jpg)

 

**lock.unlock() 释放锁**

```java
public void unlock() {
    sync.release(1);
}
```

可以看到调用的是，**NonfairSync.release()**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lntdz54j30ke0bm44f.jpg)

最后又调用了**NonfairSync.tryRelease()**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lnrse6dj30tt0byq5h.jpg)

**基本可以确认，释放锁就是对AQS中的状态值State进行修改。同时更新下一个链表中的线程等待节点**。

 

 

**总结**

- lock的存储结构：一个int类型状态值（用于锁的状态变更），一个双向链表（用于存储等待中的线程）
- lock获取锁的过程：本质上是通过CAS来获取状态值修改，如果当场没获取到，会将该线程放在线程等待链表中。
- lock释放锁的过程：修改状态值，调整等待链表。

可以看到在整个实现过程中，lock大量使用CAS+自旋。因此根据CAS特性，lock建议使用在低锁冲突的情况下。目前java1.6以后，官方对synchronized做了大量的锁优化（偏向锁、自旋、轻量级锁）。因此在非必要的情况下，建议使用synchronized做同步操作。

最后，希望我的分析，能对你理解锁的实现有所帮助。

____________________________________________________________________________

**锁实现**

   简单说来，AbstractQueuedSynchronizer会把所有的请求线程构成一个CLH队列，当一个线程执行完毕（lock.unlock()）时会激活自己的后继节点，但正在执行的线程并不在队列中，而那些等待执行的线程全 部处于**阻塞状态**，经过调查线程的显式阻塞是通过调用LockSupport.park()完成，而LockSupport.park()则调用 sun.misc.Unsafe.park()本地方法，再进一步，HotSpot在Linux中中通过调用pthread_mutex_lock函数把 线程交给系统内核进行阻塞。

   与synchronized相同的是，这也是一个虚拟队列，不存在队列实例，仅存在节点之间的前后关系。令人疑惑的是为什么采用CLH队列呢？原生的CLH队列是用于自旋锁，但Doug Lea把其改造为阻塞锁。

   当有线程竞争锁时，该线程会首先尝试获得锁，这对于那些已经在队列中排队的线程来说显得不公平，这也是非公平锁的由来，与synchronized实现类似，这样会极大提高吞吐量。 如果已经存在Running线程，则新的竞争线程会被追加到队尾，具体是采用基于CAS的Lock-Free算法，因为线程并发对Tail调用CAS可能会 导致其他线程CAS失败，解决办法是循环CAS直至成功。AQS的实现非常精巧，令人叹为观止，不入细节难以完全领会其精髓，下面详细说明实现过程：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0lnqivntj30fa084gnf.jpg)

   AbstractQueuedSynchronizer通过构造一个基于阻塞的CLH队列容纳所有的阻塞线程，而对该队列的操作均通过Lock-Free（CAS）操作，但对已经获得锁的线程而言，ReentrantLock实现了偏向锁的功能。

synchronized 的底层也是一个基于CAS操作的等待队列，但JVM实现的更精细，把等待队列分为ContentionList和EntryList，目的是为了降低线程的出列速度；当然也实现了偏向锁，从数据结构来说二者设计没有本质区别。但synchronized还实现了自旋锁，并针对不同的系统和硬件体系进行了优 化，而Lock则完全依靠系统阻塞挂起等待线程。

当然Lock比synchronized更适合在应用层扩展，可以继承 AbstractQueuedSynchronizer定义各种实现，比如实现读写锁（ReadWriteLock），公平或不公平锁；同时，Lock对 应的Condition也比wait/notify要方便的多、灵活的多。

 

state值，若为0，意味着此时没有线程获取到资源

**简述总结：**

  总体来讲线程获取锁要经历以下过程(非公平)：

  1、调用lock方法，会先进行cas操作看下可否设置同步状态1成功，如果成功执行临界区代码

  2、如果不成功获取同步状态，如果状态是0那么cas设置为1.

  3、如果同步状态既不是0也不是自身线程持有会把当前线程构造成一个节点。

  4、把当前线程节点CAS的方式放入队列中，行为上线程阻塞，内部自旋获取状态。

   （acquireQueued的主要作用是把已经追加到队列的线程节点进行**阻塞**，但阻塞前又通过tryAccquire重试是否能获得锁，如果重试成功能则无需阻塞，直接返回。）

  5、线程释放锁，唤醒队列第一个节点，参与竞争。重复上述。

 

------

**synchronized和lock的底层区别\***

**synchronized的底层也是一个基于CAS操作的等待队列**，但JVM实现的更精细，把等待队列分为ContentionList和EntryList，目的是为了降低线程的出列速度；当然也实现了偏向锁，从数据结构来说二者设计没有本质区别。但synchronized还实现了**自旋锁**，并针对不同的系统和硬件体系进行了优化，而**Lock则完全依靠系统阻塞挂起等待线程。\****

 

当然Lock比synchronized更适合在应用层扩展，可以继承AbstractQueuedSynchronizer定义各种实现，比如实现读写锁（ReadWriteLock），公平或不公平锁；同时，Lock对应的Condition也比wait/notify要方便的多、灵活的多。

ReentrantLock是一个可重入的互斥锁，ReentrantLock由最近成功获取锁，还没有释放的线程所拥有

ReentrantLock与synchronized的区别

--ReentrantLock的lock机制有2种，忽略中断锁和响应中断锁

 

--synchronized实现的锁机制是可重入的，主要区别是中断控制和竞争锁公平策略

------

两者区别：

1.首先synchronized是java内置关键字，在jvm层面，Lock是个java类；

2.synchronized无法判断是否获取锁的状态，Lock可以判断是否获取到锁；

3.synchronized会自动释放锁(a 线程执行完同步代码会释放锁 ；b 线程执行过程中发生异常会释放锁)，Lock需在finally中手工释放锁（unlock()方法释放锁），否则容易造成线程死锁；

4.用synchronized关键字的两个线程1和线程2，如果当前线程1获得锁，线程2线程等待。如果线程1阻塞，线程2则会一直等待下去，而Lock锁就不一定会等待下去，如果尝试获取不到锁，线程可以不用一直等待就结束了；

5.synchronized的锁可重入、不可中断、非公平，而Lock锁可重入、可判断、可公平（两者皆可）

6.Lock锁适合大量同步的代码的同步问题，synchronized锁适合代码少量的同步问题。

 

**synchronized底层实现**

synchronized 属于重量级锁，效率低下，因为监视器锁（monitor）是依赖于底层操作系统的 Mutex Lock 来实现的，而操作系统实现线程之间的切换时需要从用户态转换到核心态，这个状态之间的转换需要相对比较长的时间，时间成本相对较高，这也是为什么早期的 synchronized 效率低的原因。庆幸的是在 Java 6 之后 Java 官方从 JVM 层面对 synchronized 进行了较大优化，所以现在的 synchronized 锁效率也优化得很不错了。Java 6 之后，**为了减少获得锁和释放锁所带来的性能消耗**，引入了轻量级锁和偏向锁，

 

**Lock底层实现**

Lock底层实现基于AQS实现，采用线程独占的方式，在硬件层面依赖特殊的CPU指令（CAS）。

简单来说，ReenTrantLock的实现是一种**自旋锁**，通过循环调用CAS操作来实现加锁。它的性能比较好也是因为**避免了使线程进入内核态的阻塞状态。**想尽办法避免线程进入内核的阻塞状态是我们去分析和理解锁设计的关键钥匙。

 

**volatile底层实现**

在JVM底层volatile是采用“**内存屏障**”来实现的。

 

lock和Monitor的区别

一、lock的底层本身是Monitor来实现的，所以Monitor可以实现lock的所有功能。

二、Monitor有TryEnter的功能，可以防止出现死锁的问题，lock没有。

---

# 1. Lock 的简介及使用

​     Lock是java 1.5中引入的线程同步工具，它主要用于多线程下共享资源的控制。本质上Lock仅仅是一个接口（位于源码包中的java\util\concurrent\locks中），它包含以下方法

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
//尝试获取锁，获取成功则返回，否则阻塞当前线程
void lock(); 

//尝试获取锁，线程在成功获取锁之前被中断，则放弃获取锁，抛出异常 
void lockInterruptibly() throws InterruptedException; 

//尝试获取锁，获取锁成功则返回true，否则返回false 
boolean tryLock(); 

//尝试获取锁，若在规定时间内获取到锁，则返回true，否则返回false，未获取锁之前被中断，则抛出异常 
boolean tryLock(long time, TimeUnit unit) 
                                   throws InterruptedException; 

//释放锁
void unlock(); 

//返回当前锁的条件变量，通过条件变量可以实现类似notify和wait的功能，一个锁可以有多个条件变量
Condition newCondition();
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

​    Lock有三个实现类，一个是ReentrantLock,另两个是ReentrantReadWriteLock类中的两个静态内部类ReadLock和WriteLock。

​     使用方法：多线程下访问（互斥）共享资源时， 访问前加锁，访问结束以后解锁，解锁的操作推荐放入finally块中。

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
Lock l = ...; //根据不同的实现Lock接口类的构造函数得到一个锁对象 
l.lock(); //获取锁位于try块的外面 
try { 
      // access the resource protected by this lock 
} finally { 
     l.unlock(); 
}
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 注意：加锁位于对资源访问的try块的外部，特别是使用lockInterruptibly方法加锁时就必须要这样做，这为了防止线程在获取锁时被中断，这时就不必（也不能）释放锁。

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
try {
     l.lockInterruptibly();//获取锁失败时不会执行finally块中的unlock语句
      try{
          // access the resource protected by this lock
     }finally{
          l.unlock();
     }
} catch (InterruptedException e) {
     // TODO Auto-generated catch block
     e.printStackTrace();
}
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

# 2. 实现Lock接口的基本思想

​     需要实现锁的功能，两个必备元素:

- 一个是表示（锁）状态的变量（我们假设0表示没有线程获取锁，1表示已有线程占有锁）,该变量必须声明为voaltile类型;
- 另一个是队列，队列中的节点表示因未能获取锁而阻塞的线程。

为了解决多核处理器下多线程缓存不一致的问题，表示状态的变量必须声明为voaltile类型，并且对表示状态的变量和队列的某些操作要保证原子性和可见性。原子性和可见性的操作主要通过Atomic包中的方法实现。

###  

###    线程获取锁的大致过程（这里没有考虑可重入和获取锁过程被中断或超时的情况）

​     \1. 读取表示锁状态的变量

​     \2. 如果表示状态的变量的值为0，那么当前线程尝试将变量值设置为1（通过CAS操作完成），当多个线程同时将表示状态的变量值由0设置成1时，仅一个线程能成功，其它线程都会失败:

​      2.1 若成功，表示获取了锁，

​         2.1.1 如果该线程（或者说节点）已位于在队列中，则将其出列（并将下一个节点则变成了队列的头节点）

​         2.1.2 **如果该线程未入列，则不用对队列进行维护**

​         然后当前线程从lock方法中返回，对共享资源进行访问。      

​       2.2 若失败，则当前线程将自身放入等待（锁的）队列中并阻塞自身，此时线程一直被阻塞在lock方法中，没有从该方法中返回**（被唤醒后仍然在****lock****方法中，并从下一条语句继续执行，这里又会回到第1****步重新开始）**。

​    \3. 如果表示状态的变量的值为1，那么将当前线程放入等待队列中，然后将自身阻塞**（被唤醒后仍然在****lock****方法中，并从下一条语句继续执行，这里又会回到第1****步重新开始）**

###      **注意: 唤醒并不表示线程能立刻运行，而是表示线程处于就绪状态，仅仅是可以运行而已**

 

###    **线程释放锁的大致过程**

​    \1. 释放锁的线程将状态变量的值从1设置为0，并唤醒等待（锁）队列中的**队首节点**，释放锁的线程从就从unlock方法中返回，继续执行线程后面的代码

​    \2. 被唤醒的线程（队列中的队首节点）和可能和未进入队列并且准备获取的线程竞争获取锁，重复获取锁的过程

​    注意：可能有多个线程同时竞争去获取锁，但是一次只能有一个线程去释放锁，队列中的节点都需要它的前一个节点将其唤醒，例如有队列A<-B-<C ，即由A释放锁时唤醒B，B释放锁时唤醒C

 

# 3. 公平锁和非公平锁

​     锁可以分为公平锁和不公平锁，重入锁和非重入锁（关于重入锁的介绍会在ReentrantLock源代码分析中介绍），以上过程实际上是非公平锁的获取和释放过程。

公平锁严格按照先来后到的顺去获取锁，而非公平锁允许插队获取锁。

​     公平锁获取锁的过程上有些不同，在使用公平锁时，某线程想要获取锁，不仅需要判断当前表示状态的变量的值是否为0，还要判断队列里是否还有其他线程，若队列中还有线程则说明当前线程需要排队，进行入列操作，并将自身阻塞；若队列为空，才能尝试去获取锁。而对于非公平锁，当表示状态的变量的值是为0，就可以尝试获取锁，不必理会队列是否为空，这样就实现了插队获取锁的特点。通常来说非公平锁的吞吐率比公平锁要高，我们一般常用非公平锁。

​      这里需要解释一点，什么情况下才会出现，表示锁的状态的变量的值是为0而且队列中仍有其它线程等待获取锁的情况。

​      假设有三个线程A、B、C。A线程为正在运行的线程并持有锁，队列中有一个C线程，位于队首。现在A线程要释放锁，具体执行的过程操作可分为两步：

​      \1. 将表示锁状态的变量值由1变为0，

​      \2. C线程被唤醒，这里要明确两点：

​       **（1）**C线程被唤醒并不代表C线程开始执行，C线程此时是处于就绪状态，要等待操作系统的调度

​       **（2）**C线程目前还并未出列，C线程要进入运行状态，并且通过竞争获取到锁以后才会出列。

​      如果C线程此时还没有进入运行态，同时**未在队列中**的B线程进行获取锁的操作，B就会发现虽然当前没有线程持有锁，但是队列不为空（C线程仍然位于队列中），要满足先来后到的特点（B在C之后执行获取锁的操作），B线程就不能去尝试获取锁，而是进行入列操作。

 

# 4. 实现Condition接口的基本思想

​     Condition 本质是一个接口，它包含如下方法

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
// 让线程进入等通知待状态 
void await() throws InterruptedException; 
void awaitUninterruptibly();
 
//让线程进入等待通知状态，超时结束等待状态，并抛出异常  
long awaitNanos(long nanosTimeout) throws InterruptedException; 
boolean await(long time, TimeUnit unit) throws InterruptedException; 
boolean awaitUntil(Date deadline) throws InterruptedException; 

//将条件队列中的一个线程，从等待通知状态转换为等待锁状态 
void signal(); 

//将条件队列中的所有线程，从等待通知阻塞状态转换为等待锁阻塞状态
void signalAll();
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

​      一个Condition实例的内部实际上维护了一个队列，队列中的节点表示由于（某些条件不满足而）线程自身调用await方法阻塞的线程。Condition接口中有两个重要的方法，即 await方法和 signal方法。线程调用这个方法之前该线程必须已经获取了Condition实例所依附的锁。这样的原因有两个，（1）对于await方法，它内部会执行释放锁的操作，所以使用前必须获取锁。（2）对于signal方法，是为了避免多个线程同时调用同一个Condition实例的singal方法时引起的（队列）出列竞争。下面是这两个方法的执行流程。

​     await方法：

​              \1. 入列到**条件队列（注意**这里不是等待锁的队列**）**

​              \2. 释放锁

​              \3. 阻塞自身线程

​               ------------被唤醒后执行-------------

​              \4. 尝试去获取锁（执行到这里时线程已不在条件队列中，而是**位于等待（锁的）队列中**，参见signal方法）

​                4.1 成功，从await方法中返回，执行线程后面的代码

​                4.2 失败，阻塞自己（等待前一个节点释放锁时将它唤醒）

​     注意：await方法时自身线程调用的，线程在await方法中阻塞，并没有从await方法中返回，当唤醒后继续执行await方法中后面的代码（也就是获取锁的代码）。可以看出await方法释放了锁，又尝试获得锁。当获取锁不成功的时候当前线程仍然会阻塞到await方法中，等待前一个节点释放锁后再将其唤醒。

 

​     signal方法：

​              \1. 将条件队列的队首节点取出，放入等待锁队列的队尾

​              \2. 唤醒该节点对应的线程

​     注意：signal是由其它线程调用

[![condition](https://images2015.cnblogs.com/blog/834468/201511/834468-20151116133743374-365756176.png)](http://images2015.cnblogs.com/blog/834468/201511/834468-20151116133741515-1609987065.png)

# Lock和Condition的使用例程

​      下面这个例子，就是利用lock和condition实现B线程先打印一句信息后，然后A线程打印两句信息（不能中断），交替十次后结束

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
public class ConditionDemo {
    volatile int key = 0;
    Lock l = new ReentrantLock();
    Condition c = l.newCondition();
    
    public static  void main(String[] args){
        ConditionDemo demo = new ConditionDemo();
        new Thread(demo.new A()).start();
        new Thread(demo.new B()).start();
    }
    
    class A implements Runnable{
        @Override
        public void run() {
            int i = 10;
            while(i > 0){
                l.lock();
                try{
                    if(key == 1){
                        System.out.println("A is Running");
                        System.out.println("A is Running");
                        i--;
                        key = 0;
                        c.signal();
                    }else{
                     c.awaitUninterruptibly();                        
                    }
                    
                }
                finally{
                    l.unlock();
                }
            }
        }
        
    }
    
    class B implements Runnable{
        @Override
        public void run() {
            int i = 10;
            while(i > 0){
                l.lock();
                try{
                    if(key == 0){
                        System.out.println("B is Running");
                        i--;
                        key = 1;
                        c.signal();
                    }else{
                     c.awaitUninterruptibly();                        
                    }
                    
                }
                finally{
                    l.unlock();
                }
            }
        }    
    }
}
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

# 5. Lock与synchronized的区别

​     \1. Lock的加锁和解锁都是由java代码配合native方法（调用操作系统的相关方法）实现的，而synchronize的加锁和解锁的过程是由JVM管理的

​     \2. 当一个线程使用synchronize获取锁时，若锁被其他线程占用着，那么当前只能被阻塞，直到成功获取锁。而Lock则提供超时锁和可中断等更加灵活的方式，在未能获取锁的   条件下提供一种退出的机制。

​     \3. 一个锁内部可以有多个Condition实例，即有多路条件队列，而synchronize只有一路条件队列；同样Condition也提供灵活的阻塞方式，在未获得通知之前可以通过中断线程以  及设置等待时限等方式退出条件队列。

​     \4. synchronize对线程的同步仅提供独占模式，而Lock即可以提供独占模式，也可以提供共享模式

https://www.cnblogs.com/shoshana-kong/p/10772679.html

---

https://www.jianshu.com/p/9c9086c9fe46?utm_campaign=maleskine&utm_content=note&utm_medium=seo_notes&utm_source=recommendation

---

