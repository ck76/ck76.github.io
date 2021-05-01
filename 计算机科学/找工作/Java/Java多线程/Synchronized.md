[TOC]

### 一、对象锁

对象锁也叫方法锁，是针对一个对象实例的，它只在该对象的某个内存位置声明一个标识该对象是否拥有锁，所有它只会锁住当前的对象，而并不会对其他对象实例的锁产生任何影响。

当synchronized锁住一个对象后，别的线程如果也想拿到这个对象的锁，就必须等待这个线程执行完成释放锁，**才能再次给对象加锁**，这样才达到线程同步的目的。即使两个不同的代码段，都要锁同一个对象，那么这两个代码段也不能在多线程环境下同时运行。

所以我们在用synchronized关键字的时候，能缩小代码段的范围就尽量缩小，能在代码段上加同步就不要再整个方法上加同步。这叫减小锁的粒度，使代码更大程度的并发。

**核心思想：线程想给对象加锁，就必须等对象上没有其他锁，如果线程想执行非synchronized方法，也就是说该线程不想给对象加锁，所以可以立即执行，不管其他线程是否给对象加锁了。**

1. 不同对象访问同一个被synchronized修饰的方法的时候不会阻塞
2. 不同线程访问同一个对象的非synchronized方法不会阻塞，即线程1访问对象的synchronized方法，线程2可以访问对象的其他非synchronized方法。

**这种机制确保了同一时刻对于每一个类的实例，其所有声明为synchronized的成员函数中之多只有一个处于可执行状态，从而有效避免了类成员变量的访问冲突。**

#### 1、两种形式

```java
- synchronized void method()
- synchronized (this) { }

public void sayHello() {
        synchronized (this){
            System.out.println("hello");  
        }
    }

 public synchronized void sayHello() {
            System.out.println("hello"); 
    }
```



### 二、类锁

**核心思想：同上面的对象锁，只不过现在是给类加锁，内存中总共有一份static内容，如果不同线程想访问一个类的static synchronized方法，都要先询问是不是当前类被加锁了。执行非synchronized方法则没有问题。**

由于一个class不论被实例化多少次，其中的**静态方法**和**静态变量**在内存中都**只有一份**。所以，一旦一个静态的方法被声明为synchronized。此类所有的实例对象在调用此方法，共用同一把锁，我们称之为类锁。

**对象锁是用来控制实例方法之间的同步**，而**类锁是用来控制静态方法（或者静态变量互斥体）之间的同步的。**

类锁只是一个概念上的东西，并不是真实存在的，他只是用来帮助我们理解锁定实例方法和静态方法的区别的。
java类可能会有很多对象，但是只有一个Class(字节码)对象，也就是说类的不同实例之间共享该类的Class对象。Class对象其实也仅仅是1个java对象，只不过有点特殊而已。
由于每个java对象都有1个互斥锁，而类的静态方法是需要Class对象。所以所谓的类锁，只不过是Class对象的锁而已。

获取类的Class对象的方法有好几种，最简单的是[类名.class]的方式。

#### 1、两种形式

```java
- static synchronized void method(){}
- synchronized (XXX.class) { }

class SynchronizedTest {
    static  String ck="ck";
    public void ssayHello() throws {
        synchronized(SynchronizedTest.class){
             System.out.println("ck");
        } 
    }

    public static synchronized void sayck() {
        System.out.println("ck");
    }
}
```



### 三、原理

1. 依赖 `JVM` 实现同步
2. 底层通过一个监视器对象`（monitor）`完成， `wait（）`、`notify（）` 等方法也依赖于 monitor 对象

> 监视器锁（monitor）的本质 依赖于 底层操作系统的互斥锁（Mutex Lock）实现



### 四、特点

- 原子性
- 顺序性
- 可见性

Volatile与之相比并不保证原子性

![synchronized](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/AnYAO0TA8uiTHjQkkfRuZktyDCWr1WIjuEzi2X0UmLQ!/r/dDMBAAAAAAAA)



### 五、其他控制并发 / 线程同步方式

![1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/U2ZQ9JDkKyCBmoF33jFyzrWwrbwjdMN*qz29dplLK9g!/r/dMAAAAAAAAAA)

---

![2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/r85eDPVyVGgivS7S5npbsdYxK33JqlP8CvZyVGh8dPY!/r/dLgAAAAAAAAA)



### 六、volatile和synchronized的区别

1. volatile本质是在告诉jvm当前变量在寄存器（工作内存）中的值是不确定的，需要从主存中读取；  synchronized则是锁定当前变量，只有当前线程可以访问该变量，其他线程被阻塞住。
2. volatile仅能使用在变量级别；synchronized则可以使用在变量、方法、和类级别的
3. volatile仅能实现变量的修改可见性，不能保证原子性；而synchronized则可以保证变量的修改可见性和原子性
4. volatile不会造成线程的阻塞；synchronized可能会造成线程的阻塞。
5. volatile标记的变量不会被编译器优化；synchronized标记的变量可以被编译器优化



首先需要理解线程安全的两个方面：**执行控制**和**内存可见**。

**执行控制**的目的是控制代码执行（顺序）及是否可以并发执行。

**内存可见**控制的是线程执行结果在内存中对其它线程的可见性。根据[Java内存模型](http://blog.csdn.net/suifeng3051/article/details/52611310)的实现，线程在具体执行时，会先拷贝主存数据到线程本地（CPU缓存），操作完成后再把结果从线程本地刷到主存。

`synchronized`关键字解决的是执行控制的问题，它会阻止其它线程获取当前对象的监控锁，这样就使得当前对象中被`synchronized`关键字保护的代码块无法被其它线程访问，也就无法并发执行。更重要的是，`synchronized`还会创建一个**内存屏障**，内存屏障指令保证了所有CPU操作结果都会直接刷到主存中，从而保证了操作的内存可见性，同时也使得先获得这个锁的线程的所有操作，都**happens-before**于随后获得这个锁的线程的操作。

`volatile`关键字解决的是内存可见性的问题，会使得所有对`volatile`变量的读写都会直接刷到主存，即保证了变量的可见性。这样就能满足一些对变量可见性有要求而对读取顺序没有要求的需求。

使用`volatile`关键字仅能实现对原始变量(如boolen、 short 、int 、long等)操作的原子性，但需要特别注意， `volatile`不能保证复合操作的原子性，即使只是`i++`，实际上也是由多个原子操作组成：`read i; inc; write i`，假如多个线程同时执行`i++`，`volatile`只能保证他们操作的`i`是同一块内存，但依然可能出现写入脏数据的情况。

在Java 5提供了原子数据类型`atomic wrapper classes`，对它们的`increase`之类的操作都是原子操作，不需要使用`sychronized`关键字。

对于`volatile`关键字，当且仅当满足以下所有条件时可使用：

```
1. 对变量的写入操作不依赖变量的当前值，或者你能确保只有单个线程更新变量的值。
2. 该变量没有包含在具有其他变量的不变式中。
```

　1、同步(synchronized)简单说可以理解为共享的意思，如果资源不是共享的，就没必要进行同步。设置共享资源为同步的话，可以避免一些脏读情况。

　2、异步(asynchronized)简单说可以理解为独立不受到其他任何制约。

①volatile轻量级，只能修饰变量。synchronized重量级，还可修饰方法，**线程不安全**

②volatile只能保证数据的可见性，不能用来同步，因为多个线程并发访问volatile修饰的变量不会阻塞。**线程安全**

synchronized不仅保证可见性，而且还保证原子性，因为，只有获得了锁的线程才能进入临界区，从而保证临界区中的所有语句都全部执行。多个线程争抢synchronized锁对象时，会出现阻塞。

####  线程安全性包括两个方面，**①可见性。②原子性。**

仅仅使用volatile并不能保证线程安全性。而synchronized则可实现线程的安全性。