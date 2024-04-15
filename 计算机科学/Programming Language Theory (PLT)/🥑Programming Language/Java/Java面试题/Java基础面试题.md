[TOC]

### Java中==和equals和hashCode的区别

```java
对于==，如果作用于基本数据类型的变量，则直接比较其存储的 “值”是否相等；

　　　　如果作用于引用类型的变量，则比较的是所指向的对象的地址

对于equals方法，注意：equals方法不能作用于基本数据类型的变量

　　　　如果没有对equals方法进行重写，则比较的是引用类型的变量所指向的对象的地址；

　　　　诸如String、Date等类对equals方法进行了重写的话，比较的是所指向的对象的内容。
对于hashCode方法
		hash方法返回的值与地址关联，但并不是地址。
		可以继续谈hashcode的作用
```



### String、StringBuffer、StringBuilder区别

```java
String 字符串常量StringBuffer 字符串变量（线程安全）StringBuilder 字符串变量（非线程安全） 简要的说， String 类型和 StringBuffer 类型的主要性能区别其实在于 String 是不可变的对象, 因此在每次对 String 类型进行改变的时候其实都等同于生成了一个新的 String 对象，然后将指针指向新的 String 对象，所以经常改变内容的字符串最好不要用 String ，因为每次生成对象都会对系统性能产生影响，特别当内存中无引用对象多了以后， JVM 的 GC 就会开始工作，那速度是一定会相当慢的。

而如果是使用 StringBuffer 类则结果就不一样了，每次结果都会对 StringBuffer 对象本身进行操作，而不是生成新的对象，再改变对象引用。所以在一般情况下我们推荐使用 StringBuffer ，特别是字符串对象经常改变的情况下。而在某些特别情况下， String 对象的字符串拼接其实是被 JVM 解释成了 StringBuffer 对象的拼接，所以这些时候 String 对象的速度并不会比 StringBuffer 对象慢，而特别是以下的字符串对象生成中， String 效率是远要比 StringBuffer 快的： String S1 = “This is only a” + “ simple” + “ test”; StringBuffer Sb = new StringBuilder(“This is only a”).append(“ simple”).append(“ test”); 你会很惊讶的发现，生成 String S1 对象的速度简直太快了，而这个时候 StringBuffer 居然速度上根本一点都不占优势。其实这是 JVM 的一个把戏，在 JVM 眼里，这个 String S1 = “This is only a” + “ simple” + “test”; 其实就是： String S1 = “This is only a simple test”; 所以当然不需要太多的时间了。但大家这里要注意的是，如果你的字符串是来自另外的 String 对象的话，速度就没那么快了，譬如：String S2 = “This is only a”;String S3 = “ simple”;String S4 = “ test”;String S1 = S2 +S3 + S4;这时候 JVM 会规规矩矩的按照原来的方式去做

在大部分情况下 StringBuffer > StringStringBufferJava.lang.StringBuffer线程安全的可变字符序列。一个类似于 String 的字符串缓冲区，但不能修改。虽然在任意时间点上它都包含某种特定的字符序列，但通过某些方法调用可以改变该序列的长度和内容。可将字符串缓冲区安全地用于多个线程。可以在必要时对这些方法进行同步，因此任意特定实例上的所有操作就好像是以串行顺序发生的，该顺序与所涉及的每个线程进行的方法调用顺序一致。StringBuffer 上的主要操作是 append 和 insert 方法，可重载这些方法，以接受任意类型的数据。每个方法都能有效地将给定的数据转换成字符串，然后将该字符串的字符追加或插入到字符串缓冲区中。append 方法始终将这些字符添加到缓冲区的末端；而 insert 方法则在指定的点添加字符。例如，如果 z 引用一个当前内容是“start”的字符串缓冲区对象，则此方法调用 z.append("le") 会使字符串缓冲区包含“startle”，而 z.insert(4, "le") 将更改字符串缓冲区，使之包含“starlet”。在大部分情况下 StringBuilder > StringBuffer


JVM内部采用了StringBuffer来连接字符串了，那么我们自己就不用用StringBuffer，直接用”+“就行了吧！“。是么？当然不是了。俗话说”存在既有它的理由”，让我们继续看后面的循环对应的字节码。

因为每次执行“+”操作时jvm都要new一个StringBuffer对象来处理字符串的连接，这在涉及很多的字符串连接操作时开销会很大。所以短的字符串连接可以用+++++++++++
```



### 为什么StringBuffer同步，StringBuidler不同步

```java
1、StringBuffer 与 StringBuilder 中的方法和功能完全是等价的，

2、只是StringBuffer 中的方法大都采用了 synchronized 关键字进行修饰，因此是线程安全的，

而 StringBuilder 没有这个修饰，可以被认为是线程不安全的。 

3、在单线程程序下，StringBuilder效率更快，因为它不需要加锁，不具备多线程安全

而StringBuffer则每次都需要判断锁，效率相对更低
```



### string 转换成 integer的方式及原理

```java
https://blog.csdn.net/itboy_libing/article/details/80393530
```



### 闭包和局部内部类的区别



### 接口的意义

- 抽象类和接口的区别，为啥有了抽象类还要有接口

- 谈谈对接口与回调的理解

- 回调的原理

- 写一个回调demo



### final，finally，finalize的区别

```java
- final修饰符（关键字
- finally是在异常处理时提供finally块来执行任何清除操作。
- java技术允许使用finalize方法在垃圾收集器将对象从内存中清除出去之前做必要的清理工作。这个方法是由垃圾收集器在确定这个对象没有被引用时对这个对象调用的
```



### 谈谈对Java多态的理解

多态就是指程序中定义的引用变量所指向的具体类型和通过该引用变量发出的方法调用在编程时并不确定，而是在程序运行期间才确定，即一个引用变量倒底会指向哪个类的实例对象，该引用变量发出的方法调用到底是哪个类中实现的方法，必须在由程序运行期间才能决定。因为在程序运行时才确定具体的类，这样，不用修改源程序代码，就可以让引用变量绑定到各种不同的类实现上，从而导致该引用调用的具体方法随之改变，即不修改程序代码就可以改变程序运行时所绑定的具体代码，让程序可以选择多个运行状态，这就是多态性。

- 多态存在的三个必要条件：
  1、继承
  2、重写
  3、父类引用指向子类对象（向上转型）

- 多态的优点
  - 消除类型之间的耦合关系
  - 可替换性
  - 可扩充性
  - 接口性
  - 灵活性
  - 简化性



### 说说你对Java反射的理解

- https://www.jianshu.com/p/8dc84550296f

```java
Java反射机制主要提供了以下功能： 
- 在运行时判断任意一个对象所属的类；
- 在运行时构造任意一个类的对象；
- 在运行时判断任意一个类所具有的成员变量和方法；
- 在运行时调用任意一个对象的方法；
- 生成动态代理。
利用反射机制能获得什么信息?
    一句话，类中有什么信息，它就可以获得什么信息，不过前提是得知道类的名字，要不就没有后文了
    首先得根据传入的类的全名来创建Class对象。
    Class c=Class.forName("className");注明：className必须为全名，也就是得包含包名，比如，cn.netjava.pojo.UserInfo;
    Object obj=c.newInstance();//创建对象的实例
    OK，有了对象就什么都好办了，想要什么信息就有什么信息了。  
```



### 说说你对Java注解的理解

1.@Target

2.@Retention

3.@Documented

4.@Inherited 是继承的意思

5.@Repeatable 自然是可重复的意思。1.8 才加进来的，所以算是一个新的特性。



### String为什么是不变的

- 字符串常量池的需要
- 允许String对象缓存HashCode

- 安全性



### 动态代理基于什么原理

- **java.lang.reflect.Proxy:**动态代理机制的主类，提供一组静态方法为一组接口动态的生成对象和代理类。

```java
public static Object newProxyInstance(ClassLoader loader,
 Class<?>[] interfaces,InvocationHandler h)
```

- **java.lang.reflect.InvocationHandler**：调用处理器接口，自定义invokle方法，用于实现对于真正委托类的代理访问。

```java
/**
 该方法负责集中处理动态代理类上的所有方法调用。
 第一个参数既是代理类实例，
 第二个参数是被调用的方法对象
 第三个方法是调用参数。
 调用处理器根据这三个参数进行预处理或分派到委托类实例上发射执行
*/
public Object invoke(Object proxy, Method method, Object[] args)
    throws Throwable;
```

- 机制

  - 通过实现 InvocationHandler 接口创建自己的调用处理器；

  ```java
  // InvocationHandlerImpl 实现了 InvocationHandler 接口，并能实现方法调用从代理类到委托类的分派转发
  // 其内部通常包含指向委托类实例的引用，用于真正执行分派转发过来的方法调用
  InvocationHandler handler = new InvocationHandlerImpl(..);
  ```

  - 通过为 Proxy 类指定 ClassLoader 对象和一组 interface 来创建动态代理类；

  ```java
  // 通过 Proxy 为包括 Interface 接口在内的一组接口动态创建代理类的类对象
  Class clazz = Proxy.getProxyClass(classLoader, new Class[] { Interface.class, ... }); 
  ```

  - 通过反射机制获得动态代理类的构造函数，其唯一参数类型是调用处理器接口类型；
  ```java
  // 通过反射从生成的类对象获得构造函数对象
  Constructor constructor = clazz.getConstructor(new Class[] { InvocationHandler.class });
  ```

  - 通过构造函数创建动态代理类实例，构造时调用处理器对象作为参数被传入。
  ```java
  // 通过构造函数对象创建动态代理类实例
  Interface Proxy = (Interface)constructor.newInstance(new Object[] { handler });
  ```


- 为了简化对象创建过程，Proxy类中的newProxyInstance方法封装了2~4，只需两步即可完成代理对象的创建。

```java
为了简化对象创建过程，Proxy类中的newProxyInstance方法封装了2~4，只需两步即可完成代理对象的创建。

// InvocationHandlerImpl 实现了 InvocationHandler 接口，并能实现方法调用从代理类到委托类的分派转发
InvocationHandler handler = new InvocationHandlerImpl(..); 

// 通过 Proxy 直接创建动态代理类实例
Interface proxy = (Interface)Proxy.newProxyInstance( 
    classLoader, //classLoader
     new Class[] { Interface.class }, //委托类实现接口
     handler );//InvokeHandler
```



- https://blog.csdn.net/Scplove/article/details/52451899

