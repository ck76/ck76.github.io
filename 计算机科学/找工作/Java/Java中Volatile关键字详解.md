https://www.cnblogs.com/aimei/p/12192943.html

[星云留水](https://www.cnblogs.com/aimei/)

## [Java中Volatile关键字详解](https://www.cnblogs.com/aimei/p/12192943.html)

**阅读目录**

- [一、基本概念](https://www.cnblogs.com/zhengbin/p/5654805.html#_label0)
- [二、Volatile原理](https://www.cnblogs.com/zhengbin/p/5654805.html#_label1)

## **一、基本概念**

------

### **先补充一下概念：Java 内存模型中的可见性、原子性和有序性。**

**可见性：**

　　可见性是一种复杂的属性，因为可见性中的错误总是会违背我们的直觉。通常，我们无法确保执行读操作的线程能适时地看到其他线程写入的值，有时甚至是根本不可能的事情。为了确保多个线程之间对内存写入操作的可见性，必须使用同步机制。

　　**可见性，是指线程之间的可见性，一个线程修改的状态对另一个线程是可见的。**也就是一个线程修改的结果。另一个线程马上就能看到。比如：用volatile修饰的变量，就会具有可见性。**volatile修饰的变量不允许线程内部缓存和重排序，即直接修改内存**。所以对其他线程是可见的。但是这里需要注意一个问题，volatile只能让被他修饰内容具有可见性，但不能保证它具有原子性。比如 volatile int a = 0；之后有一个操作 a++；这个变量a具有可见性，但是a++ 依然是一个非原子操作，也就是这个操作同样存在线程安全问题。

　　在 Java 中 volatile、synchronized 和 final 实现可见性。

**原子性：**

　　**原子是世界上的最小单位，具有不可分割性。**比如 a=0；（a非long和double类型） 这个操作是不可分割的，那么我们说这个操作时原子操作。再比如：a++； 这个操作实际是a = a + 1；是可分割的，所以他不是一个原子操作。**非原子操作都会存在线程安全问题**，需要我们使用同步技术（sychronized）来让它变成一个原子操作。一个操作是原子操作，那么我们称它具有原子性。java的concurrent包下提供了一些原子类，我们可以通过阅读API来了解这些原子类的用法。比如：AtomicInteger、AtomicLong、AtomicReference等。

　　在 Java 中 synchronized 和在 lock、unlock 中操作保证原子性。

**有序性：**

　　Java 语言提供了 volatile 和 synchronized 两个关键字来保证线程之间操作的有序性，volatile 是因为其本身包含“禁止指令重排序”的语义，synchronized 是由“一个变量在同一个时刻只允许一条线程对其进行 lock 操作”这条规则获得的，**此规则决定了持有同一个对象锁的两个同步块只能串行执行。**

**下面内容摘录自《Java Concurrency in Practice》：**

　　下面一段代码在多线程环境下，将存在问题。

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

![复制代码](https://common.cnblogs.com/images/copycode.gif)+ View code

```java
 1 /**
 2  * @author zhengbinMac
 3  */
 4 public class NoVisibility {
 5     private static boolean ready;
 6     private static int number;
 7     private static class ReaderThread extends Thread {
 8         @Override
 9         public void run() {
10             while(!ready) {
11                 Thread.yield();
12             }
13             System.out.println(number);
14         }
15     }
16     public static void main(String[] args) {
17         new ReaderThread().start();
18         number = 42;
19         ready = true;
20     }
21 }
```

![复制代码](https://common.cnblogs.com/images/copycode.gif)

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

　　NoVisibility可能会持续循环下去，因为读线程可能永远都看不到ready的值。甚至NoVisibility可能会输出0，因为读线程可能看到了写入ready的值，但却没有看到之后写入number的值，这种现象被称为“重排序”。只要在某个线程中无法检测到重排序情况（即使在其他线程中可以明显地看到该线程中的重排序），那么就无法确保线程中的操作将按照程序中指定的顺序来执行。当主线程首先写入number，然后在没有同步的情况下写入ready，那么读线程看到的顺序可能与写入的顺序完全相反。

　　在没有同步的情况下，编译器、处理器以及运行时等都可能对操作的执行顺序进行一些意想不到的调整。在缺乏足够同步的多线程程序中，要想对内存操作的执行春旭进行判断，无法得到正确的结论。

　　这个看上去像是一个失败的设计，但却能使JVM充分地利用现代多核处理器的强大性能。例如，在缺少同步的情况下，Java内存模型允许编译器对操作顺序进行重排序，并将数值缓存在寄存器中。此外，它还允许CPU对操作顺序进行重排序，并将数值缓存在处理器特定的缓存中。

## 二、Volatile原理

------

　　Java语言提供了一种稍弱的同步机制，即volatile变量，用来确保将变量的更新操作通知到其他线程。当把变量声明为volatile类型后，编译器与运行时都会注意到这个变量是共享的，因此不会将该变量上的操作与其他内存操作一起重排序。**volatile变量不会被缓存在寄存器或者对其他处理器不可见的地方，因此在读取volatile类型的变量时总会返回最新写入的值。**

　　在访问volatile变量时不会执行加锁操作，因此也就不会使执行线程阻塞，因此volatile变量是一种比sychronized关键字更轻量级的同步机制。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glmpnq0ufmj30fa0bxwes.jpg)

　　当对非 volatile 变量进行读写的时候，每个线程先从内存拷贝变量到CPU缓存中。如果计算机有多个CPU，每个线程可能在不同的CPU上被处理，这意味着每个线程可以拷贝到不同的 CPU cache 中。

　　而声明变量是 volatile 的，**JVM 保证了每次读变量都从内存中读，跳过 CPU cache 这一步。**

### 当一个变量定义为 volatile 之后，将具备两种特性：

　　1.保证此变量对所有的线程的**可见性**，这里的“可见性”，如本文开头所述，当一个线程修改了这个变量的值，volatile 保证了新值能立即同步到主内存，以及每次使用前立即从主内存刷新。但普通变量做不到这点，普通变量的值在线程间传递均需要通过主内存（详见：[Java内存模型](http://www.cnblogs.com/zhengbin/p/6407137.html)）来完成。

　　2.**禁止指令重排序优化**。有volatile修饰的变量，赋值后多执行了一个“load addl $0x0, (%esp)”操作，这个操作相当于一个**内存屏障**（指令重排序时不能把后面的指令重排序到内存屏障之前的位置），只有一个CPU访问内存时，并不需要内存屏障；（什么是指令重排序：是指CPU采用了允许将多条指令不按程序规定的顺序分开发送给各相应电路单元处理）。

### volatile 性能：

　　volatile 的读性能消耗与普通变量几乎相同，但是**写操作稍慢**，因为它需要在本地代码中插入许多内存屏障指令来保证处理器不发生乱序执行。

分类: [Java](https://www.cnblogs.com/aimei/category/1632634.html)

---

https://www.cnblogs.com/dolphin0520/p/3920373.html

---

