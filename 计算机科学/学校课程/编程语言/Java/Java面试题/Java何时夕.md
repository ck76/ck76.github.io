[TOC]



### 1.重载函数的签名(区别是否是重载函数)

方法名+参数类型+参数顺序(返回值不是)

### 2.finalize的工作原理

一旦垃圾收集器准备好释放对象占用的存储空间，它首先调用finalize()，而且只有在下一次垃圾收集过程中，才会真正回收对象的内存.所以如果使用finalize()，就可以在垃圾收集期间进行一些重要的清除或清扫工作。此外系统进行垃圾回收时并不保证一定会调用它，所以可以说它和对象被回收没有必然关系。

### 3.一个对象的创建过程

例如有一个Dog类：

- 1.即使没有static，构造器其实也是静态方法，所以当首次创建Dog时或者Dog的静态域或者静态方法被访问的时候，Dog的class对象会被加载。
- 2.在加载Dog的class的时候可能会发现其有基类，此时先暂停Dog的加载，转而去加载其基类。所以基类的static域比子类先初始化。
- 3.载入class文件之后，会创建一个该该类的class对象，所有关于静态初始化的动作会被执行，所以静态初始化只在Class对象首次加载时进行一次。
- 4.当使用new的时候，首先为Dog对象在堆上分配足够内存空间。这块内存控件会被清零，所有实例都被初始化成了默认值，包括基类。
- 5.执行所有字段定义处的初始化，从基类开始。
- 6.执行构造器。(在第一行会执行基类的构造器)

### 4.对象的销毁是按照创建的逆序来进行的。

### 5.final的方法和类都不允许覆盖和继承。private默认实现了final。

### 6.简述内部类的特点

- 1.内部类只能在与其外围类的对象关联的情况下才能被创建(非static)
- 2.内部类在创建的时候，捕获了一个外围类的对象，所以能访问外围类的private对象。(非static)
- 3.如果其他类对内部类有访问权限的话（如public），那么可以通过  外围类对象.new内部类()，这样的方式创建内部对象，注意如果没有外围类的对象，内部类是不允许创建的。(非static)
- 4.多层嵌套的内部类能够透明的访问所有外围类的所有成员。
- 5.内部类不能被覆盖
- 6.内部类让java模拟了多继承，让外围类继承一个类，然后内部类继承一个类，又要有内部类对象必须有对应的外围类对象。
- 7.static的内部类，和普通的类基本相同。

### 7.形式参数可被视为local variable，也就是说形式参数相当于在方法中定义了一个局部变量a，当传入c时，只是将c指向的对象给a###

### 8.finally 语句块是在 try 或者 catch 中的 return 语句之前执行的

### 9.简述jvm中默认的classLoader与功能

- 1.Bootstrap ClassLoader：负责加载java基础类，主要是 %JRE_HOME/lib/ 目录下的rt.jar、resources.jar、charsets.jar和class等
- 2.Extension ClassLoader：负责加载java扩展类，主要是 %JRE_HOME/lib/ext 目录下的jar和class
- 3.App ClassLoader：负责加载当前java应用的classpath中的所有类。
- 4.classloader 加载类用的是全盘负责双亲委托机制。 
  - 1.所谓全盘负责，即是当一个classloader加载一个Class的时候，这个Class所依赖的和引用的所有 Class也由这个classloader负责载入，除非是显式的使用另外一个classloader载入。所以，当我们自定义的classloader加载成功了 com.company.MyClass以后，MyClass里所有依赖的class都由这个classLoader来加载完成。
  - 2.双亲委托，是指子类加载器如果没有加载过该目标类，就先委托父类加载器加载该目标类，只有在父类加载器找不到字节码文件的情况下才从自己的类路径中查找并装载目标类。

### 10.switch语句后的控制表达式只能是short、char、int、long整数类型和枚举类型，不能是float，double和boolean类型。String类型是java7开始支持

### 11.重写的特性

- 1.方法名相同，参数类型和顺序相同
- 2.子类返回类型小于等于父类方法返回类型。即：子类返回的类型要为父类的子类
- 3.子类抛出异常小于等于父类方法抛出异常。即：子类抛出的异常类型要为父类的子类
- 4.子类访问权限大于等于父类方法访问权限。

### 12.!=和==，当用于基本类型时候，是比较值是否相同；当用于引用类型的时候，是比较对象是否相同，比较内存地址

### 13.null可以被强制类型转换成任意类型的对象，于是通过它来执行静态方法

### 14.如何终止一个线程###

- 1.如果线程中是循环或者线程已经sleep()，可以通过Excutor产生一个中断，
- 2.处于循环中的话，还可以设置一个标志位，一旦为false就让循环退出此时线程也会接着终止。
- 3.如果线程处于io之中，那么可以通过关闭io来使线程结束。

### 15.静态属性和静态方法是否可以被继承

非静态属性、静态属性、静态方法  都可以被子类继承，但是不会被覆盖，转为哪个类，调用的就是那个类的东西。也就是说这几个东西不具有多态。

### 16.列出各种List，Set，Map，Queue并说出他们的同与不同。

- 1.List：ArrayList、LinkedList、Vector、CopyOnWriteArrayList 
  - 1.ArrayList:常用的List，非线程安全、内部使用数组来实现、对于随机访问get和set，ArrayList优于LinkedList
  - 2.LinkedList：线程非安全、内部用链表来实现、对于新增和删除操作add和remove，LinedList比较占优势，因为ArrayList要移动数据。
  - 3.Vector：Vector的方法都是同步的、是线程安全的、当Vector或ArrayList中的元素超过它的初始大小时,Vector会将它的容量翻倍,而ArrayList只增加50%的大小，这样,ArrayList就有利于节约内存空间。
  - 4.CopyOnWriteArrayList：ArrayList 的一个线程安全的变体，其中所有可变操作（add、set 等等）都是通过对底层数组进行一次新的复制来实现的，在CopyOnWriteArrayList中，写入将导致创建整个底层数组的副本，而源数组将保留在原地，使得复制的数组在被修改时，读取操作可以安全的执行。当修改完成时，一个原子性的操作将把新的数组换入，使得新的读取操作可以看到这个新的修改。这对于读操作远远多于写操作的应用非常适合
- 2.Set：HashSet、TreeSet、LinkedHashSet、CopyOnWriteArraySet、EnumSet 
  - 1.HashSet：非线程安全、放入的元素需要重写equals()和hashCode()方法，才能避免放入元素重复，并且难以被发现。
  - 2.TreeSet：非线程安全、其内部使用红黑树储存元素、TreeSet储存的类型必须实现comparable接口。
  - 3.LinkedHashSet：非线程安全、以元素插入的顺序来维护集合的链接表，其他地方和HashMap类似。
  - 1. CopyOnWriteArraySet：线程安全、CopyOnWriteArraySet基于CopyOnWriteArrayList实现。
  - 1. EnumSet是一个专门为枚举类设计的集合类，EnumSet中所有元素都必须是指定枚举类型的枚举值，该枚举类型在创建EnumSet时显式、或隐式地指定。EnumSet的集合元素也是有序的，它们以枚举值在Enum类内的定义顺序来决定集合元素的顺序。
- 3.Map：HashMap、TreeMap、LinkedHashMap、HashTable、ConcurrentHashMap、WeakHashMap、EnumMap 
  - 1.HashMap:非线程安全、其内部使用链表法解决哈希冲突、key需要重写equals()和hashCode()方法，其内部在获取元素的时候先通过hashCode()获取到某条链表，再通过equals()方法在这条链表中找到该元素。
  - 2.TreeMap：非线程安全、TreeMap的实现是红黑树算法的实现，key必须实现comparable接口。
  - 3.LinkedHashMap：非线程安全、这个map是按插入顺序遍历该map，以散列插入。还可以在构造函数中设置参数，使其以LRU算法排序。其获取的Set能体现出排序的方式。
  - 4.HashTable：线程安全、所有的的方法都是同步的、key和value不能为null。
  - 5.ConcurrentHashMap：线程安全、ConcurrentHashMap融合了hashtable和hashmap二者的优势、实现了细粒度的锁，也就是不是所有地方都上锁，性能比HashTable好。
  - 6.WeakHashMap与HashMap的用法基本相似。区别在于，HashMap的key保留了对实际对象的"强引用"，这意味着只要该HashMap对象不被销毁，该HashMap所引用的对象就不会被垃圾回收。但WeakHashMap的key只保留了对实际对象的弱引用，这意味着如果WeakHashMap对象的key所引用的对象没有被其他强引用变量所引用，则这些key所引用的对象可能被垃圾回收，当垃圾回收了该key所对应的实际对象之后，WeakHashMap会自动删除这些key所对应的key-value对
  - 7.EnumMap：EnumMap是一个与枚举类一起使用的Map实现，EnumMap中的所有key都必须是单个枚举类的枚举值。创建EnumMap时必须显式或隐式指定它对应的枚举类。EnumMap根据key的自然顺序(即枚举值在枚举类中的定义顺序)。
- 4.Queue：ArrayBlockingQueue、LinkedBlockingQueue、DelayQueue、PriorityBlockingQueue、PriorityQueue。 
  - 1.ArrayBlockingQueue：线程安全、基于数组的阻塞队列实现，在ArrayBlockingQueue内部，维护了一个定长数组，以便缓存队列中的数据对象。同一时间只能有一个线程进行操作。
  - 2.LinkedBlockingQueue：基于链表的阻塞队列，同ArrayListBlockingQueue类似，其内部也维持着一个数据缓冲队列，当生产者往队列中放入一个数据时，队列会从生产者手中获取数据，并缓存在队列内部，而生产者立即返回、只有当队列缓冲区达到最大值缓存容量时（LinkedBlockingQueue可以通过构造函数指定该值），才会阻塞生产者队列，直到消费者从队列中消费掉一份数据，生产者线程会被唤醒。可以有多个线程进行同时操作。
  - 3.DelayQueue： DelayQueue里面放的是实现了Delayed的接口，Delayed接口需要实现两个方法：getDelay(TimeUnit unit)是为了返回该任务锁需延长的时间，compareTo(Delayed o)是为了比较到底那个任务需要排在前面，当使用queue.take()的时候延时开始，此时本线程挂起，直到延时结束开始运行run()中的任务。
  - 4.PriorityBlockingQueue： 基于优先级的阻塞队列（优先级的判断通过构造函数传入的Compator对象来决定），但需要注意的是PriorityBlockingQueue并不会阻塞数据生产者，而只会在没有可消费的数据时，阻塞数据的消费者。因此使用的时候要特别注意，生产者生产数据的速度绝对不能快于消费者消费数据的速度，否则时间一长，会最终耗尽所有的可用堆内存空间。
  - 5.PriorityQueue：非线程安全的优先队列。

### 17.列出几种线程池

- 1.ThreadPoolExecutor：通过Executors可以构造单线程池、固定数目线程池、不固定数目线程池。
- 2.ScheduledThreadPoolExecutor：可以延时调用线程或者延时重复调度线程。

### 18.解释一下java内存模型

- 1.每个线程有自己的共享变量副本(实例域、静态域、数组元素)
- 2.Java线程之间的通讯由java内存模型控制(JMM)，JMM决定了一个线程对共享变量的写入在什么时候对另一个线程可见。
- 3.当一个A线程改变了一个共享变量，此时只是改变了自己本地共享变量的副本，所以线程之间要实现通讯需要先将A线程的本地副本，刷入到主存之中，然后线程B去主存中读取线程A刷入的改变的变量。

### 19.解释一下java代码的原子性

- 1.除了long和double，其他基本类型的简单操作都是原子性操作：读取、写入数字。
- 2.java1.5之后使用volatile关键字能够让long和double像其他基本类型一样。
- 3.不能因为有原子性，就放弃同步。

### 20.解释一下volatile字段###

- 1.在了解了java内存模型之后，我们知道volatile关键字保证了，某个共享变量改变之后，另一个线程中本地共享变量的副本也会立即刷新。
- 2.当某个域的值依赖于它之前的值，如计数递增那么volatile就会失效。
- 3.使用volatile而不使用synchronized的唯一情况就是类中只有一个可变域，其他时候都应该使用synchronized。

### 21.解释一下synchronized字段

- 1.synchronized可以用于修饰方法，在某线程从某个用synchronized修饰的方法返回之前，其他所有要调用这个对象中任意使用synchronized修饰的方法的线程都会被阻塞。即同一时刻某个对象中只能有一个synchronized修饰的方法被调用。
- 2.对于某个对象，其所有synchronized方法共享一把锁，也就是说一个对象中含有一把锁(也叫监视器)。
- 3.一个线程可以多次获取某个对象的锁，比如一个synchronized方法中调用了该对象中另一个synchronized方法。那么这个锁的计数就会变成2，每从一个synchronized方法离开都会使计数减一，每进入一个synchronized方法都会使计数加一。
- 4.每个类也有一把锁(属于类的Class的一部分)，所以synchronized static可以防止类范围内对于static数据的并发访问。

### 22.Lock的使用

- 1.在lock()和unlock()之间我们可以创建一个临界资源，同时一时刻只能有一个线程访问这个资源。。
- 2.我们可以对临界区域进行异常捕捉，注意return必须在try里面，防止过早释放临界资源。
- 3.使用Lock的好处就在于我们能处理某些异常，而使用synchronized我们无法对异常进行清理工作。

### 23.wait()和sleep()区别

- 1.该方法在Object中，可以将当前线程挂起，需要在synchronized控制的块中，因为wait需要获取需要挂起的线程所要处理对象的锁，否则会报错。在wait()中锁是被释放的。
- 2.该方法是属于Thread类中、sleep()方法导致了程序暂停执行指定的时间，让出cpu该其他线程，线程不会释放对象锁。

### 24.死锁产生的条件

- 1.至少有一个资源是不能被共享的
- 2.至少有一个任务持有一个资源并且在等待另一个资源
- 3.资源不能被抢占
- 4.必须有循环等待



----



### 链接

- <https://www.jianshu.com/p/6006a3284f55>