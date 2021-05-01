# 导语

>  我们知道J2EE框架的Spring就是通过动态代理优雅地实现了AOP编程，结果极大地提升了Web开发效率，因而使用代理机制进行API Hook可以让“方法增强”，也是框架的常用手段。同样，插件框架也广泛使用了代理机制来增强系统API从而达到插件化的目的。本文将带大家一起了解代理机制及Hook机制。

## ***\*一、\**\**认识\**代理模式**

1、代理是什么？

>   代理也称“委托”，分为静态代理和动态代理，代理模式也是常用的设计模式之一，具有方法增强、高扩展性的设计优势。其设计就是限制直接访问真实对象，而是访问该对象的代理类。这样的话，我们就保护了内部对象，如果有一天内部对象因为某个原因换了个名或者换了个方法字段等等，那对访问者来说一点不影响，因为他拿到的只是代理类而已，从而使该访问对象具有高扩展性。然而，代理类可以实现拦截方法，修改原方法的参数和返回值，满足了代理自身需求和目的，也就是是代理的方法增强性。

2、静态代理：

创建一个Image接口显示方法

```java
public interface Image {
   void display();
}
```

创建一个RealImage类实现Image接口

```java
public class RealImage implements Image {
 
   private String fileName;
 
   public RealImage(String fileName){
      this.fileName = fileName;
      loadFromDisk(fileName);
   }
 
   @Override
   public void display() {
      System.out.println("Displaying " + fileName);
   }
 
   private void loadFromDisk(String fileName){
      System.out.println("Loading " + fileName);
   }
}
```

创建代理类Proxy实现Image接口

```java
public class ProxyImage implements Image{
 
   private RealImage realImage;
   private String fileName;
 
   public ProxyImage(String fileName){
      this.fileName = fileName;
   }
 
   @Override
   public void display() {
      if(realImage == null){
         realImage = new RealImage(fileName);
      }
      realImage.display();
   }
}
```

当被请求时，使用 *ProxyImage* 来获取 *RealImage* 类的对象。

```java
public class ProxyPatternDemo {
   
   public static void main(String[] args) {
      Image image = new ProxyImage("test_10mb.jpg");
 
      // 图像将从磁盘加载
      image.display(); 
      System.out.println("");
      // 图像不需要从磁盘加载
      image.display();  
   }
}
```

 

3、动态代理：

> 是JDK提供的代理方式且只支持接口，在JVM虚拟机运行时动态生成一系列代理，主要利用反射机制在运行时创建的代理类。当静态代理类特别多的时候，我们就可以不用手写每一个静态代理类了，也不需要制定代理谁，在运行的时候指定一个代理类就行了，变得更灵活了。

实现步骤：

1. 写一个类实现InvocationHanlder接口；
2. 重写接口的invoke方法；
3. 调用Proxy.newProxyInstance()返回一个代理对象;

### ***\*案\**例：**

### Subject.java

```java
public interface Subject {
    void operation();
}
```

 **RealSubject.java**

```java
public class RealSubject implements Subject{
    @Override
    public void operation() {
        System.out.println("Hello Aiyang!");
    }
}
```

**ProxySubject.java**

```java
public class ProxySubject implements InvocationHandler{
     protected Object subject;
    public ProxySubject(Object subject) {
        this.subject = subject;
    }
 
    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
       System.out.println("Before invoke "  + method.getName());
       method.invoke(subject, args);
       System.out.println("After invoke " + method.getName());
       return null;
    }
}
```

**执行动态代理：**

```java
 
 
    public static void main(String[] args) {
        Subject subject = new RealSubject();
        
        InvocationHandler handler = new ProxySubject(subject);
 
        Subject sub = (Subject) Proxy.newProxyInstance(subject.getClass().getClassLoader(),
                                subject.getClass().getInterfaces(),
                                handler);
        
        sub.operation();
    }
 
    输出：
    Before invoke operation
    Hello Aiyang!
    After invoke operation
```

既然生成代理对象是用的Proxy类的静态方newProxyInstance，我们可以大致看一下这个方法原理：

***\*newProxyInstance.java\****

```java
public static Object newProxyInstance(ClassLoader loader, Class<?>[]interfaces,InvocationHandler h) throws IllegalArgumentException { 
    // 检查 h 不为空，否则抛异常
    if (h == null) { 
        throw new NullPointerException(); 
    } 
    // 获得与指定类装载器和一组接口相关的代理类类型对象
    Class cl = getProxyClass(loader, interfaces); 
    // 通过反射获取构造函数对象并生成代理类实例
    try { 
        Constructor cons = cl.getConstructor(constructorParams); 
        return (Object) cons.newInstance(new Object[] { h }); 
    } catch (NoSuchMethodException e) {...
}
```

## 二、认识Hook机制

 

>   Hook又叫“钩子”，它可以在事件传送的过程中截获并监控事件的传输，将自身的代码与系统方法进行融入。这样当这些方法被调用时，也就可以执行我们自己的代码，这也是面向切面编程的思想（AOP）。当然，根据 Hook 对象与 Hook 后处理的事件方式不同，Hook 还分为不同的种类，比如消息 Hook、API Hook 等。

 

**1、Java API Hook：**

  通过对 Android 平台的虚拟机注入与 Java 反射的方式，来改变 Android 虚拟机调用函数的方式（ClassLoader），从而达到 Java 函数重定向的目的，这里我们将此类操作称为 Java API Hook。

 

2、Hook动态代理：

代理可以增强原始对象的方法能力，比如帮人跑腿买东西，可以坑钱坑货；那么很自然，如果我们动态创建代理对象，然后把原始对象替换为我们的代理对象，对这个代理对象为所欲为。这样动态拦截对象修改参数，替换返回值，我们称之为Hook动态代理。

**如何使用：**

1、Hook 的选择点：静态变量和单例，因为一旦创建对象，它们不容易变化，非常容易定位。

2、Hook 过程：

- 寻找 Hook 点，原则是静态变量或者单例对象，尽量 Hook public 的对象和方法。
- 选择合适的代理方式，如果是接口可以用动态代理。
- 偷梁换柱——用代理对象替换原始对象。

3、Android 的 API 版本比较多，方法和类可能不一样，所以要做好 API 的兼容工作。


 

## 三、Hook使用实例

 

### 1、简单案例: 使用 Hook 修改 View.OnClickListener 事件

 

 

首先，我们先分析 View.setOnClickListener 源码，找出合适的Hook点。可以看到 OnClickListener 对象被保存在了一个叫做 ListenerInfo 的内部类里，其中 mListenerInfo 是 View 的成员变量。ListeneInfo 里面保存了 View 的各种监听事件。

 

```java
public void setOnClickListener(@Nullable OnClickListener l) {
    if (!isClickable()) {
        setClickable(true);
    }
    getListenerInfo().mOnClickListener = l;
}
ListenerInfo getListenerInfo() {
    if (mListenerInfo != null) {
        return mListenerInfo;
    }
    mListenerInfo = new ListenerInfo();
    return mListenerInfo;
}
```

接下里，创建一个Hook代理，当给 View 设置监听事件后，替换 OnClickListener 对象，添加自己的操作。

```java
class HookedProxyClass implements View.OnClickListener {
        private View.OnClickListener origin;
        HookedProxyClass(View.OnClickListener origin) {
            this.origin = origin;
        }
        @Override
        public void onClick(View v) {
            Toast.makeText(MainActivity.this, "搞事情！", Toast.LENGTH_SHORT).show();
            if (origin != null) {
                origin.onClick(v);
            }
        }
    }

public void hookOnClickListener(View view) throws Exception  {
        // 反射 得到 ListenerInfo 对象
        Method getListenerInfo = View.class.getDeclaredMethod("getListenerInfo");
        getListenerInfo.setAccessible(true);
        Object listenerInfo = getListenerInfo.invoke(view);
        // 得到 原始的 OnClickListener事件方法
        Class<?> listenerInfoClz = Class.forName("android.view.View$ListenerInfo");
        Field mOnClickListener = listenerInfoClz.getDeclaredField("mOnClickListener");
        mOnClickListener.setAccessible(true);
        View.OnClickListener originOnClickListener = (View.OnClickListener) mOnClickListener.get(listenerInfo);
        // 用Hook代理类 替换原始的 OnClickListener
        View.OnClickListener hookedOnClickListener = new HookedProxyClass(originOnClickListener);
        mOnClickListener.set(listenerInfo, hookedOnClickListener);
    }

 TextView click_me = (TextView) findViewById(R.id.click_me);
  click_me.setOnClickListener(new View.OnClickListener() {
            @Override
       public void onClick(View v) {
                Log.d("log", "原始点击事件内容");
            }
   });
     
   hookOnClickListener(click_me);    
```

 

最后，我们触发点击事件，下面是日志打印结果：

 

```java
D/log: 拦截了原始事件，并且新加了本条内容
 
D/log: 原始点击事件内容
```

**2、案例升级：****Hook 修改 startActivity 启动其他页面**

  首先，我们依然先去分析一下startActivity的源代码寻找合适的Hook点。由于Context.startActivity与Activity.startActivity有所不同，我们先来看Context的实现是 ContextImpl 类的startActivity方法源码。实际上是使用了ActivityThread类的mMainThread来获取mInstrumententation对象，调用execStartActivity方法执行。因为ActivityThread是主线程，进程中的唯一线程，因此这里是一个合适的Hook点。

```java
@Override
public void startActivity(Intent intent, Bundle options) {
    warnIfCallingFromSystemProcess();
    if ((intent.getFlags()&Intent.FLAG_ACTIVITY_NEW_TASK) == 0) {
        throw new AndroidRuntimeException(".....省略");
    }
    mMainThread.getInstrumentation().execStartActivity(//这里主线程执行启动
        getOuterContext(), mMainThread.getApplicationThread(), null,
        (Activity)null, intent, -1, options);
}
```

  接下来，我们就是把这个mMainThread的mInstrumententation对象给替换成新的代理对象。由于JDK动态代理只支持接口，而这个Instrumentation是一个类，没办法，我们只有手动写静态代理类，覆盖掉原始的方法即可。

 

```java
public static class HookedInstrumentation extends Instrumentation {
 
        private Instrumentation origin;
 
        public HookedInstrumentation(Instrumentation base) {
            origin = base;
        }
 
        public ActivityResult execStartActivity(
                Context who, IBinder contextThread,
                IBinder token, Activity target,
                Intent intent, int requestCode, Bundle options) {
 
            Log.d("log", "Hook拦截了"+who+"执行的startActivity。跳转到页面"+target);
 
            try {
                // 使用反射调用execStartActivity构造方法
                Method execStartActivity = Instrumentation.class.getDeclaredMethod(
                        "execStartActivity", Context.class, IBinder.class,
                        IBinder.class, Activity.class,
                        Intent.class, int.class, Bundle.class);
                execStartActivity.setAccessible(true);
 
                return (ActivityResult) execStartActivity.invoke(origin, who,
                        contextThread, token, target, intent, requestCode, options);
                
            } catch (Exception e) {
                Log.d("log", "某该死的rom修改了  需要手动适配 ");
                throw new RuntimeException("do not support!!! pls adapt it");
            }
        }
    }
```

我们运行测试看看结果。

```java
  public static void hookContextStartActivity() throws Exception{
        // 获取当前 ActivityThread 对象
        Class<?> activityThreadClass = Class.forName("android.app.ActivityThread");
        Method currentActivityThreadMethod = activityThreadClass.getDeclaredMethod("currentActivityThread");
        currentActivityThreadMethod.setAccessible(true);
        Object currentActivityThread = currentActivityThreadMethod.invoke(null);
 
        // 获取原始 mInstrumentation 字段
        Field mInstrumentationField = activityThreadClass.getDeclaredField("mInstrumentation");
        mInstrumentationField.setAccessible(true);
        Instrumentation mInstrumentation = (Instrumentation) mInstrumentationField.get(currentActivityThread);
 
        // 创建代理对象
        Instrumentation evilInstrumentation = new HookedInstrumentation(mInstrumentation);
 
        // 偷梁换柱
        mInstrumentationField.set(currentActivityThread, evilInstrumentation);
    }

private void mStartActivity(Context context){
        context.startActivity(new Intent(MainActivity.this,MainActivity.class));
        try {
            hookContextStartActivity();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

D/log: Hook拦截了com.example.javafanshe.MainActivity@420715a0执行的startActivity。跳转到页面com.example.javafanshe.MainActivity@420715a0
```

 

**总结****整个Hook过程：**

- 寻找Hook点，原则是静态变量或者单例对象，尽量Hook pulic的对象和方法；
- 选择合适的代理方式，如果是接口可以用动态代理；如果是类可以手动写代理；
- 偷梁换柱——用代理对象替换原始对象。