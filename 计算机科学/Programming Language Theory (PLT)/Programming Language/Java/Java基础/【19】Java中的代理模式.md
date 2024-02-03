[TOC]

## 一、代理模式的实现

代理模式可以有两种实现的方式，一种是静态代理类，另一种是各大框架都喜欢的动态代理。下面我们主要讲解一下这两种代理模式



## 二、静态代理

我们先看针对上面UML实现的例子，再看静态代理的特点。
Subject接口的实现

```java
public interface Subject {

    void visit();

}
```

实现了Subject接口的两个类：

```java
public class RealSubject implements Subject {

    private String name = "byhieg";
    @Override
    public void visit() {
        System.out.println(name);
    }
}
public class ProxySubject implements Subject{

    private Subject subject;

    public ProxySubject(Subject subject) {
        this.subject = subject;
    }

    @Override
    public void visit() {
        subject.visit();
    }
}
```

具体的调用如下：

```java
public class Client {

    public static void main(String[] args) {
        ProxySubject subject = new ProxySubject(new RealSubject());
        subject.visit();
    }
}
```

通过上面的代理代码，我们可以看出代理模式的特点，代理类接受一个Subject接口的对象，任何实现该接口的对象，都可以通过代理类进行代理，增加了通用性。但是也有缺点，每一个代理类都必须实现一遍委托类（也就是realsubject）的接口，如果接口增加方法，则代理类也必须跟着修改。其次，代理类每一个接口对象对应一个委托对象，如果委托对象非常多，则静态代理类就非常臃肿，难以胜任。



## 三、动态代理

动态代理有别于静态代理，是根据代理的对象，动态创建代理类。这样，就可以避免静态代理中代理类接口过多的问题。动态代理是实现方式，是通过反射来实现的，借助Java自带的`java.lang.reflect.Proxy`,通过固定的规则生成。
其步骤如下：

1. 编写一个委托类的接口，即静态代理的（Subject接口）
2. 实现一个真正的委托类，即静态代理的（RealSubject类）
3. 创建一个动态代理类，实现`InvocationHandler`接口，并重写该`invoke`方法
4. 在测试类中，生成动态代理的对象。

第一二步骤，和静态代理一样，不过说了。第三步，代码如下：

```java
public class DynamicProxy implements InvocationHandler {
    private Object object;
    public DynamicProxy(Object object) {
        this.object = object;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        Object result = method.invoke(object, args);
        return result;
    }
}
```

第四步，创建动态代理的对象

```java
Subject realSubject = new RealSubject();
DynamicProxy proxy = new DynamicProxy(realSubject);
ClassLoader classLoader = realSubject.getClass().getClassLoader();
Subject subject = (Subject) Proxy.newProxyInstance(classLoader, new  Class[]{Subject.class}, proxy);
subject.visit();
```

创建动态代理的对象，需要借助`Proxy.newProxyInstance`。该方法的三个参数分别是：

- ClassLoader loader表示当前使用到的appClassloader。
- Class<?>[] interfaces表示目标对象实现的一组接口。
- InvocationHandler h表示当前的InvocationHandler实现实例对象。


## 四、提供一个工厂方法的写法...

### 1.静态代理(重复了)

**接口**

```java
public interface IUserDao {
    public void add();
}
```

**目标对象**

```java
public class UserDao implements IUserDao {
    @Override
     public void add() {
         System.out.println("添加方法");
     }
}
```

**代理对象**

```java
public class UserProxy implements IUserDao{
    private IUserDao userDao;
     
     public UserProxy(IUserDao userDao) {
         super();
         this.userDao = userDao;
     }
    @Override
     public void add() {
         System.out.println("开始事务");
         userDao.add();//执行目标对象的方法
         System.out.println("提交事务");
     }
}
```

**测试类**

```java
public static void main(String[] args) {
         User user = new User();
         //目标对象
         UserDao userDao = new UserDao();
         //把目标对象传给代理对象
         UserProxy proxy = new UserProxy(userDao);
         //执行的是代理的方法
         proxy.add();
         
     }
```

**静态代理优缺点：**

优点：可以做到在不修改目标对象的功能前提下,对目标功能扩展.

缺点：因为代理对象需要与目标对象实现一样的接口,所以会有很多代理类,类太多.同时,一旦接口增加方法,目标对象与代理对象都要维护

要克服静态代理的缺点就需要用到动态代理





## 五、CodeKK

#### 实现动态代理包括三步：

(1). 新建委托类；
(2). 实现`InvocationHandler`接口，这是负责连接代理类和委托类的中间类必须实现的接口；
(3). 通过`Proxy`类新建代理类对象。 

下面通过实例具体介绍，假如现在我们想统计某个类所有函数的执行时间，传统的方式是在类的每个函数前打点统计，动态代理方式如下： 

#### 2.1 新建委托类

```java
public interface Operate {

    public void operateMethod1();

    public void operateMethod2();

    public void operateMethod3();
}

public class OperateImpl implements Operate {

    @Override
    public void operateMethod1() {
        System.out.println("Invoke operateMethod1");
        sleep(110);
    }

    @Override
    public void operateMethod2() {
        System.out.println("Invoke operateMethod2");
        sleep(120);
    }

    @Override
    public void operateMethod3() {
        System.out.println("Invoke operateMethod3");
        sleep(130);
    }

    private static void sleep(long millSeconds) {
        try {
            Thread.sleep(millSeconds);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
```

`Operate`是一个接口，定了了一些函数，我们要统计这些函数的执行时间。
`OperateImpl`是委托类，实现`Operate`接口。每个函数简单输出字符串，并等待一段时间。
动态代理要求委托类必须实现了某个接口，比如这里委托类`OperateImpl`实现了`Operate`，原因会后续在微博公布。 

#### 2.2. 实现 InvocationHandler 接口

```java
public class TimingInvocationHandler implements InvocationHandler {

    private Object target;

    public TimingInvocationHandler() {}

    public TimingInvocationHandler(Object target) {
        this.target = target;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        long start = System.currentTimeMillis();
        Object obj = method.invoke(target, args);
        System.out.println(method.getName() + " cost time is:" + (System.currentTimeMillis() - start));
        return obj;
    }
}
```

`target`属性表示委托类对象。 

`InvocationHandler`是负责连接代理类和委托类的中间类必须实现的接口。其中只有一个 

```java
public Object invoke(Object proxy, Method method, Object[] args)
```

函数需要去实现，参数：
`proxy`表示下面`2.3 通过 Proxy.newProxyInstance() 生成的代理类对象`。
`method`表示代理对象被调用的函数。
`args`表示代理对象被调用的函数的参数。 

调用代理对象的每个函数实际最终都是调用了`InvocationHandler`的`invoke`函数。这里我们在`invoke`实现中添加了开始结束计时，其中还调用了委托类对象`target`的相应函数，这样便完成了统计执行时间的需求。
`invoke`函数中我们也可以通过对`method`做一些判断，从而对某些函数特殊处理。 

#### 2.3. 通过 Proxy 类静态函数生成代理对象

```java
public class Main {
    public static void main(String[] args) {
        // create proxy instance
        TimingInvocationHandler timingInvocationHandler = new TimingInvocationHandler(new OperateImpl());
        Operate operate = (Operate)(Proxy.newProxyInstance(Operate.class.getClassLoader(), new Class[] {Operate.class},
                timingInvocationHandler));

        // call method of proxy instance
        operate.operateMethod1();
        System.out.println();
        operate.operateMethod2();
        System.out.println();
        operate.operateMethod3();
    }
}
```

这里我们先将委托类对象`new OperateImpl()`作为`TimingInvocationHandler`构造函数入参创建`timingInvocationHandler`对象；
然后通过`Proxy.newProxyInstance(…)`函数新建了一个代理对象，实际代理类就是在这时候动态生成的。我们调用该代理对象的函数就会调用到`timingInvocationHandler`的`invoke`函数(是不是有点类似静态代理)，而`invoke`函数实现中调用委托类对象`new OperateImpl()`相应的 method(是不是有点类似静态代理)。 

```java
public static Object newProxyInstance(ClassLoader loader, Class<?>[] interfaces, InvocationHandler h)
```

- `loader`表示类加载器
- `interfaces`表示委托类的接口，生成代理类时需要实现这些接口
- `h`是`InvocationHandler`实现类对象，负责连接代理类和委托类的中间类 

我们可以这样理解，如上的动态代理实现实际是双层的静态代理，开发者提供了委托类 B，程序动态生成了代理类 A。开发者还需要提供一个实现了`InvocationHandler`的子类 C，子类 C 连接代理类 A 和委托类 B，它是代理类 A 的委托类，委托类 B 的代理类。用户直接调用代理类 A 的对象，A 将调用转发给委托类 C，委托类 C 再将调用转发给它的委托类 B。



动态代理中，代理类并不是在Java代码中实现，而是在运行时期生成，相比静态代理，动态代理可以很方便的对委托类的方法进行统一处理，如添加方法调用次数、添加日志功能等等



> 反射生成代理类，InvokeHandler作为参数传入，执行响应的方法其实都是调用InvokeHandler的invoke方法

> 从中我们可以看出动态生成的代理类是以`$Proxy`为类名前缀，继承自`Proxy`，并且实现了`Proxy.newProxyInstance(…)`第二个参数传入的所有接口的类。
> 如果代理类实现的接口中存在非 public 接口，则其包名为该接口的包名，否则为`com.sun.proxy`。
> 其中的`operateMethod1()`、`operateMethod2()`、`operateMethod3()`函数都是直接交给`h`去处理，`h`在父类`Proxy`中定义为 
>
> ```java
> protected InvocationHandler h;
> ```
>
> 即为`Proxy.newProxyInstance(…)`第三个参数。
> 所以`InvocationHandler`的子类 C 连接代理类 A 和委托类 B，它是代理类 A 的委托类，委托类 B 的代理类

> 从中可以看出它先调用`getProxyClass(loader, interfaces)`得到动态代理类，然后将`InvocationHandler`作为代理类构造函数入参新建代理类对象。



### 链接

- [CodeKK](http://a.codekk.com/detail/Android/Caij/%E5%85%AC%E5%85%B1%E6%8A%80%E6%9C%AF%E7%82%B9%E4%B9%8B%20Java%20%E5%8A%A8%E6%80%81%E4%BB%A3%E7%90%86)