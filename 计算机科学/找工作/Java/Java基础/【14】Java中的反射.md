[TOC]

> 作用，动态语言

## 什么是反射（Reflection ）？

主要是指程序可以访问、检测和修改它本身状态或行为的一种能力

## Java反射？

在Java运行时环境中，对于任意一个类，能否知道这个类有哪些属性和方法？对于任意一个对象，能否调用它的任意一个方法

Java反射机制主要提供了以下功能：

 * **1.在运行时判断任意一个对象所属的类。**
 * **2.在运行时构造任意一个类的对象。**
 * **3.在运行时判断任意一个类所具有的成员变量和方法。**
 * **4.在运行时调用任意一个对象的方法。** 

## Reflection

　Reflection是Java被视为动态（或准动态）语言的一个关键性质。

　　这个机制允许程序在运行时透过Reflection APIs取得任何一个已知名称的class的内部信息。

　　包括其modifiers（诸如public、static等）、 superclass（例如Object）、实现了的 interfaces （例如Serializable）、也包括其fields和methods的所有信息，并可于运行时改变fields内容或调用methods。

## 动态语言

　　动态语言的定义“**程序运行时，允许改变程序结构或者变量类型**，这种语言称为动态语言”。

　　从这个观点看，Perl，Python，Ruby是动态语言，C++，Java，C#不是动态语言。

　　**尽管在这样的定义与分类下Java不是动态语言，它却有着一个非常突出的动态相关机制：Reflection。**这个字的意思是：反射、映像、倒影，用在Java身上指的是我们可以于运行时加载、探知、使用编译期间完全未知的classes。

　　换句话说，Java程序可以加载一个运行时才得知名称的class，获悉其完整构造（但不包括methods定义），并生成其对象实体、或对其fields设值、或唤起其methods。

　　这种“看透”class的能力（the ability of the program to examine itself）被称为introspection（内省、内观、反省）。Reflection和introspection是常被并提的两个术语。

## Java Reflection API简介

　　在JDK中，主要由以下类来实现Java反射机制，这些类（除了第一个）都位于java.lang.reflect包中

　　Class类：代表一个类，位于java.lang包下。

　　Field类：代表类的成员变量（成员变量也称为类的属性）。

　　Method类：代表类的方法。

　　Constructor类：代表类的构造方法。

　　Array类：提供了动态创建数组，以及访问数组的元素的静态方法。

## Class对象

　　要想使用反射，首先需要获得待操作的类所对应的Class对象。

　　Java中，无论生成某个类的多少个对象，这些对象都会对应于同一个Class对象。

　　这个Class对象是由JVM生成的，通过它能够获悉整个类的结构。

　　常用的获取Class对象的3种方式：

　　1.使用Class类的静态方法。例如:　　

Class.forName(“java.lang.String”);

　　2.使用类的.class语法。如:

String.class;

　　3.使用对象的getClass()方法。如：

String str = “aa”; 
Class clazz=str.getClass();

## 例程1：获取方法

　　例程DumpMethods类演示了Reflection API的基本作用，它读取命令行参数指定的类名，然后打印这个类所具有的方法信息。　

```java
import java.lang.reflect.Method;

public class DumpMethods
{
    public static void main(String[] args) throws Exception //在方法后加上这句，异常就消失了
    {
        //获得字符串所标识的类的class对象
        Class<?> classType = Class.forName("java.lang.String");//在此处传入字符串指定类名，所以参数获取可以是一个运行期的行为，可以用args[0]

        //返回class对象所对应的类或接口中，所声明的所有方法的数组（包括私有方法）
        Method[] methods = classType.getDeclaredMethods();

        //遍历输出所有方法声明
        for(Method method : methods)
        {
            System.out.println(method);
        }
    }

}
```

例程2：通过反射调用方法 
　　通过反射调用方法。详情见代码及注释：

```java
import java.lang.reflect.Method;

public class InvokeTester
{
    public int add(int param1, int param2)
    {
        return param1 + param2;

    }

    public String echo(String message)
    {
        return "Hello: " + message;
    }

    public static void main(String[] args) throws Exception
    {

        // 以前的常规执行手段
        InvokeTester tester = new InvokeTester();
        System.out.println(tester.add(1, 2));
        System.out.println(tester.echo("Tom"));
        System.out.println("---------------------------");

        // 通过反射的方式

        // 第一步，获取Class对象
        // 前面用的方法是：Class.forName()方法获取
        // 这里用第二种方法，类名.class
        Class<?> classType = InvokeTester.class;

        // 生成新的对象：用newInstance()方法
        Object invokeTester = classType.newInstance();
        System.out.println(invokeTester instanceof InvokeTester); // 输出true

        // 通过反射调用方法
        // 首先需要获得与该方法对应的Method对象
        Method addMethod = classType.getMethod("add", new Class[] { int.class,
                int.class });
        // 第一个参数是方法名，第二个参数是这个方法所需要的参数的Class对象的数组

        // 调用目标方法
        Object result = addMethod.invoke(invokeTester, new Object[] { 1, 2 });
        System.out.println(result); // 此时result是Integer类型

        //调用第二个方法
        Method echoMethod = classType.getDeclaredMethod("echo", new Class[]{String.class});
        Object result2 = echoMethod.invoke(invokeTester, new Object[]{"Tom"});
        System.out.println(result2);

    }
}
```

**newInstance用来生成对象，invoke用来调用方法。**

## 生成对象

　　若想通过类的不带参数的构造方法来生成对象，我们有两种方式：

　　1.先获得Class对象，然后通过该Class对象的newInstance()方法直接生成即可：

```java
 Class<?> classType = String.class;

 Object obj = classType.newInstance();
```

　　2.先获得Class对象，然后通过该对象获得对应的Constructor对象，再通过该Constructor对象的newInstance()方法生成

　　（其中Customer是一个自定义的类，有一个无参数的构造方法，也有带参数的构造方法）：

```java
    Class<?> classType = Customer.class;

    // 获得Constructor对象,此处获取第一个无参数的构造方法的
    Constructor cons = classType.getConstructor(new Class[] {});

    // 通过构造方法来生成一个对象
    Object obj = cons.newInstance(new Object[] {});
```

　　若想通过类的带参数的构造方法生成对象，只能使用下面这一种方式：

　　（Customer为一个自定义的类，有无参数的构造方法，也有一个带参数的构造方法，传入字符串和整型）

```java
    Class<?> classType = Customer.class;

    Constructor cons2 = classType.getConstructor(new Class[] {String.class, int.class});

    Object obj2 = cons2.newInstance(new Object[] {"ZhangSan",20});
```

　　可以看出调用构造方法生成对象的方法和调用一般方法的类似，不同的是从Class对象获取Constructor对象时不需要指定名字，而获取Method对象时需要指定名字。



### 链接

- [code kk java反射](http://a.codekk.com/detail/Android/Mr.Simple/%E5%85%AC%E5%85%B1%E6%8A%80%E6%9C%AF%E7%82%B9%E4%B9%8B%20Java%20%E5%8F%8D%E5%B0%84%20Reflection)

---

# 导语

> 反射是一种具有与Java类进行动态交互能力的一种机制，在Java和Android开发中，一般需要访问隐藏属性或者调用方法改变程序原来的逻辑时会用到，这个比较常见，由于一些原因，系统并没有开放一些接口出来，这个时候利用反射是一个有效的解决方法，这个下文会有案例去讲；另外常见的注解框架也是在运行时利用反射机制来获取的。在模块化和插件化开发中更离不开反射，离开了反射它什么也做不了。

## 一、定义

关于反射的定义，已经成文的是这样介绍：

> **Java的反射机制是在运行的状态中，对于任意一个类，都能够这个类的所有属性和方法；对于任意一个对象，都能够调用任何方法和属性；像这种动态获取信息以及动态调用对象方法的功能成为Java的反射机制。**

这段话说的是简介精辟，算上对反射的总结了。那我们发现有几个强调词，“运行”、“任意”、“动态”等，为什么用这几个词来形容呢?我换句话来解释，因为是在程序运行状态下才会把类加载到Java虚拟机的内存中，这属于类的动态加载，并非发生在编译时候，所以，也就可以把内存中不存在的任意类动态加载进来，并且可以获取和修改任意类的信息。总之，在运行状态下利用反射可以动态的和任意类进行交互。

## 二、组成使用

关系组成图：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxkv5f65vj30vc0fkgpk.jpg)

- java.lang.Class.java：  类对象；
- java.lang.reflect.Constructor.java： 类构造器；（注意：反射的类必须有构造函数！！！）
- java.lang.reflect.Method.java：  类方法；
- java.lang.reflect.Field.java：  类属性；

1、反射获取类的实例化对象：（三种方法）

> ```java
> Class<?> cls=Class.forName("com.fanshe.Person"); //forName(包名.类名)
> Person p=(Person)cls.newInstance();
> 或
> Person p = new Person();
> Class<?> cls=p.getClass();
> Person p2=(Person)cls.newInstance();
> 或
> Class<?> cls=Person.Class();
> Person p=(Person)cls.newInstance(); 
> ```

2、反射获取类的构造器：

> ```java
> Constructor constructors = cls.getDeclaredConstructor(String.class);//参数根据类的构造，这里假设包含String类型
>  
> Object newclass = constructors.newInstance("构造实例并赋值");
> ```

3、反射获取类的方法和返回值： 

> ```java
> Method getMethod = cls.getDeclaredMethod(methodName);//传入方法名
> Object invokeMethod =getMethod.invoke(newclass);//传入实例值
> 静态方法不用借助实例 
> Method getMethod = cls.getDeclaredMethod(methodName);
> Object invokeMethod =getMethod.invoke(null);
> ```

4、反射获取类的属性的值： 

> ```java
> Field getField = cls.getDeclaredField(fieldName);//传入属性名
> Object getValue =getField.get(newclass);//传入实例值
>        静态属性不用借助实例 
> Field getField = cls.getDeclaredField(fieldName);
> Object getValue =getField.get(null);
> ```

## 三、工作原理

> 我们知道，Java的每个.class文件里承载了类的所有信息内容，包含基佬父类、接口、属性、构造、方法等；这些class文件会在程序运行状态下通过ClassLoad类机制加载到虚拟机内存中并产生一个对象实例，然后这个对象就可以对这块内存地址进行引用；一般我们通过New一个对象创建就行，而利用反射我们通过JVM查找并指定加载的类名，就可以创建对象实例，而在java中，类的对象被实例化可以在内存中被引用时，就可以获取该类的所有信息，就也就是不成文的规矩吧。

## 四、小案例

看完了前面的讲解，我们再来看一个小案例来加深认识，一起实践一下。

```java
public class Student {
    private int age;
    private String name;
    private static String mStatic;//静态属性
    /**
     * 无参构造
     */
    public Student() {
        throw new IllegalAccessError("使用默认构造函数会报错");
    }
    /**
     * 有参构造
     * @param age 年龄
     * @param name 姓名
     */
    private Student(int age, String name) {
        this.age = age;
        this.name = name;
        mStatic = "静态反射";
    }
    private String getName() {
        return name;
    }
    /**
     * 静态带返回值方法
     * @return String
     */
    private static String getTest() {
        return mStatic;
    }
}
```

这个类用到了Private私有修饰，包含带参构造函数、普通属性和静态属性，以及静态方法。

```java
public void Student() throws Exception{
    Class<?> clazz = Class.forName("com.example.javafanshe.Student");
    Constructor constructors = clazz.getDeclaredConstructor(int.class, String.class);
    constructors.setAccessible(true);//可访问私有为true
    //利用构造器生成对象
    Object mStudent = constructors.newInstance(22, "大头");
    Log.d("log","构造器是否报错："+mStudent.toString());
    //获取隐藏的int属性
    Field mField = clazz.getDeclaredField("age");
    mField.setAccessible(true);
    int age = (int) mField.get(mStudent);
    Log.d("log","年龄为:"+age);
    //调用隐藏的方法
    Method getMethod = clazz.getDeclaredMethod("getName");
    getMethod.setAccessible(true);
    String newname = (String) getMethod.invoke(mStudent);
    Log.d("log","名字为:"+newname);
    //调用静态方法
    Method getStaticMethod = clazz.getDeclaredMethod("mStatic");
    getStaticMethod.setAccessible(true);
    String result = (String) getStaticMethod.invoke(null);
    Log.d("log","调用静态方法:"+result);

```

结果日志Log如下。

> ```apache
> 01-12 19:17:00.282 31564-31564/? D/log: 构造器是否报错：com.example.javafanshe.Student@42055ca0
> 01-12 19:17:00.282 31564-31564/? D/log: 年龄为:22
> 01-12 19:17:00.282 31564-31564/? D/log: 名字为:大头
> 01-12 19:17:00.282 31564-31564/? D/log: 调用静态方法:静态反射
> ```

## 五、总结

>    写到这里，关于反射的介绍和使用都讲的差不多了，算是适合初学者比较轻松的认识反射吧，当然反射的使用远没有上面案例那么简单，实际使用还得需要多规范的组织一下，不能只为了实现功能，不顾代码的可读性和可维护性。另外，最好多用public来修饰，以防兼容版本风险，还有第三方开源的项目也要注意，不知道哪里突然维护修理了源码，也会出现兼容问题。

---

### 1. 了解 Java 中的反射

#### 1.1 什么是 Java 的反射

Java 反射是可以让我们在运行时获取类的函数、属性、父类、接口等 Class 内部信息的机制。通过反射还可以让我们在运行期实例化对象，调用方法，通过调用 get/set 方法获取变量的值，即使方法或属性是私有的的也可以通过反射的形式调用，这种“看透 class”的能力被称为内省，这种能力在框架开发中尤为重要。 有些情况下，我们要使用的类在运行时才会确定，这个时候我们不能在编译期就使用它，因此只能通过反射的形式来使用在运行时才存在的类(该类符合某种特定的规范，例如 JDBC)，这是反射用得比较多的场景。
还有一个比较常见的场景就是编译时我们对于类的内部信息不可知，必须得到运行时才能获取类的具体信息。比如 ORM 框架，在运行时才能够获取类中的各个属性，然后通过反射的形式获取其属性名和值，存入数据库。这也是反射比较经典应用场景之一。

#### 1.2 Class 类

那么既然反射是操作 Class 信息的，Class 又是什么呢?
![这里写图片描述](https://a.codekk.com/raw.githubusercontent.com/android-cn/android-open-project-analysis/master/tech/reflection/image/arch.png)
当我们编写完一个 Java 项目之后，所有的 Java 文件都会被编译成一个.class 文件，这些 Class 对象承载了这个类型的父类、接口、构造函数、方法、属性等原始信息，这些 class 文件在程序运行时会被 ClassLoader 加载到虚拟机中。当一个类被加载以后，Java 虚拟机就会在内存中自动产生一个 Class 对象。我们通过 new 的形式创建对象实际上就是通过这些 Class 来创建，只是这个过程对于我们是不透明的而已。
下面的章节中我们会为大家演示反射的一些常用 api，从代码的角度理解反射。

### 2. 反射 Class 以及构造对象

#### 2.1 获取 Class 对象

在你想检查一个类的信息之前，你首先需要获取类的 Class 对象。Java 中的所有类型包括基本类型，即使是数组都有与之关联的 Class 类的对象。如果你在编译期知道一个类的名字的话，那么你可以使用如下的方式获取一个类的 Class 对象。

```
Class<?> myObjectClass = MyObject.class;
```

如果你已经得到了某个对象，但是你想获取这个对象的 Class 对象，那么你可以通过下面的方法得到:

```
Student me = new Student("mr.simple");
Class<?> clazz = me.getClass();
```

如果你在编译期获取不到目标类型，但是你知道它的完整类路径，那么你可以通过如下的形式来获取 Class 对象:

```
Class<?> myObjectClass = Class.forName("com.simple.User");
```

在使用 Class.forName()方法时，你必须提供一个类的全名，这个全名包括类所在的包的名字。例如 User 类位于 com.simple 包，那么他的完整类路径就是 com.simple.User。
如果在调用 Class.forName()方法时，没有在编译路径下(classpath)找到对应的类，那么将会抛出 ClassNotFoundException。

**接口说明**

```
// 加载指定的 Class 对象，参数 1 为要加载的类的完整路径，例如"com.simple.Student". ( 常用方式 )
public static Class<?> forName (String className)

// 加载指定的 Class 对象，参数 1 为要加载的类的完整路径，例如"com.simple.Student";
// 参数 2 为是否要初始化该 Class 对象，参数 3 为指定加载该类的 ClassLoader.
public static Class<?> forName (String className, boolean shouldInitialize, ClassLoader classLoader)
```

### 2.2 通过 Class 对象构造目标类型的对象

一旦你拿到 Class 对象之后，你就可以为所欲为了！当你善用它的时候它就是神兵利器，当你心怀鬼胎之时它就会变成恶魔。但获取 Class 对象只是第一步，我们需要在执行那些强大的行为之前通过 Class 对象构造出该类型的对象，然后才能通过该对象释放它的能量。 我们知道，在 java 中要构造对象，必须通过该类的构造函数，那么其实反射也是一样一样的。但是它们确实有区别的，通过反射构造对象，我们首先要获取类的 Constructor(构造器)对象，然后通过 Constructor 来创建目标类的对象。还是直接上代码的。

```
    private static void classForName() {
        try {
            // 获取 Class 对象
            Class<?> clz = Class.forName("org.java.advance.reflect.Student");
            // 通过 Class 对象获取 Constructor，Student 的构造函数有一个字符串参数
            // 因此这里需要传递参数的类型 ( Student 类见后面的代码 )
            Constructor<?> constructor = clz.getConstructor(String.class);
            // 通过 Constructor 来创建 Student 对象
            Object obj = constructor.newInstance("mr.simple");
            System.out.println(" obj :  " + obj.toString());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
```

通过上述代码，我们就可以在运行时通过完整的类名来构建对象。

**获取构造函数接口**

```
// 获取一个公有的构造函数，参数为可变参数，如果构造函数有参数，那么需要将参数的类型传递给 getConstructor 方法
public Constructor<T> getConstructor (Class...<?> parameterTypes)
// 获取目标类所有的公有构造函数
public Constructor[]<?> getConstructors ()
```

注意，当你通过反射获取到 Constructor、Method、Field 后，在反射调用之前将此对象的 accessible 标志设置为 true，以此来提升反射速度。值为 true 则指示反射的对象在使用时应该取消 Java 语言访问检查。值为 false 则指示反射的对象应该实施 Java 语言访问检查。例如 :

```
   Constructor<?> constructor = clz.getConstructor(String.class);
   // 设置 Constructor 的 Accessible
   constructor.setAccessible(true);

   // 设置 Methohd 的 Accessible
   Method learnMethod = Student.class.getMethod("learn"， String.class);
   learnMethod.setAccessible(true);
```

由于后面还会用到 Student 以及相关的类，我们在这里就先给出它们的代码吧。
**Person.java**

```
public class Person {
    String mName;

    public Person(String aName) {
        mName = aName;
    }

    private void sayHello(String friendName) {
        System.out.println(mName + " say hello to " + friendName);
    }

    protected void showMyName() {
        System.out.println("My name is " + mName);
    }

    public void breathe() {
        System.out.println(" take breathe ");
    }
}
```

**Student.java**

```
public class Student extends Person implements Examination {
    // 年级
    int mGrade;

    public Student(String aName) {
        super(aName);
    }

    public Student(int grade, String aName) {
        super(aName);
        mGrade = grade;
    }

    private void learn(String course) {
        System.out.println(mName + " learn " + course);
    }

    public void takeAnExamination() {
        System.out.println(" takeAnExamination ");
    }

    public String toString() {
        return " Student :  " + mName;
    }
```

**Breathe.java**

```
// 呼吸接口
public interface Breathe {
    public void breathe();
}
```

**Examination.java**

```
// 考试接口
public interface Examination {
    public void takeAnExamination();
}
```

### 3 反射获取类中函数

#### 3.1 获取当前类中定义的方法

要获取当前类中定义的所有方法可以通过 Class 中的 getDeclaredMethods 函数，它会获取到当前类中的 public、default、protected、private 的所有方法。而 getDeclaredMethod(String name, Class...<?> parameterTypes)则是获取某个指定的方法。代码示例如下 :

```
 private static void showDeclaredMethods() {
      Student student = new Student("mr.simple");
        Method[] methods = student.getClass().getDeclaredMethods();
        for (Method method : methods) {
            System.out.println("declared method name : " + method.getName());
        }

        try {
            Method learnMethod = student.getClass().getDeclaredMethod("learn", String.class);
            // 获取方法的参数类型列表
            Class<?>[] paramClasses = learnMethod.getParameterTypes() ;
            for (Class<?> class1 : paramClasses) {
                System.out.println("learn 方法的参数类型 : " + class1.getName());
            }
            // 是否是 private 函数，属性是否是 private 也可以使用这种方式判断
            System.out.println(learnMethod.getName() + " is private "
                    + Modifier.isPrivate(learnMethod.getModifiers()));
            learnMethod.invoke(student, "java ---> ");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
```

#### 3.2 获取当前类、父类中定义的公有方法

要获取当前类以及父类中的所有 public 方法可以通过 Class 中的 getMethods 函数，而 getMethod 则是获取某个指定的方法。代码示例如下 :

```
    private static void showMethods() {
        Student student = new Student("mr.simple");
        // 获取所有方法
        Method[] methods = student.getClass().getMethods();
        for (Method method : methods) {
            System.out.println("method name : " + method.getName());
        }

        try {
            // 通过 getMethod 只能获取公有方法，如果获取私有方法则会抛出异常，比如这里就会抛异常
            Method learnMethod = student.getClass().getMethod("learn", String.class);
            // 是否是 private 函数，属性是否是 private 也可以使用这种方式判断
            System.out.println(learnMethod.getName() + " is private " + Modifier.isPrivate(learnMethod.getModifiers()));
            // 调用 learn 函数
            learnMethod.invoke(student, "java");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
```

**接口说明**

```
// 获取 Class 对象中指定函数名和参数的函数，参数一为函数名，参数 2 为参数类型列表
public Method getDeclaredMethod (String name, Class...<?> parameterTypes)

// 获取该 Class 对象中的所有函数( 不包含从父类继承的函数 )
public Method[] getDeclaredMethods ()

// 获取指定的 Class 对象中的**公有**函数，参数一为函数名，参数 2 为参数类型列表
public Method getMethod (String name, Class...<?> parameterTypes)

// 获取该 Class 对象中的所有**公有**函数 ( 包含从父类和接口类集成下来的函数 )
public Method[] getMethods ()
```

这里需要注意的是 getDeclaredMethod 和 getDeclaredMethods 包含 private、protected、default、public 的函数，并且通过这两个函数获取到的只是在自身中定义的函数，从父类中集成的函数不能够获取到。而 getMethod 和 getMethods 只包含 public 函数，父类中的公有函数也能够获取到。

### 4 反射获取类中的属性

获取属性和章节 3 中获取方法是非常相似的，只是从 getMethod 函数换成了 getField，从 getDeclaredMethod 换成了 getDeclaredField 罢了。

#### 4.1 获取当前类中定义的属性

要获取当前类中定义的所有属性可以通过 Class 中的 getDeclaredFields 函数，它会获取到当前类中的 public、default、protected、private 的所有属性。而 getDeclaredField 则是获取某个指定的属性。代码示例如下 :

```
    private static void showDeclaredFields() {
        Student student = new Student("mr.simple");
        // 获取当前类和父类的所有公有属性
        Field[] publicFields = student.getClass().getDeclaredFields();
        for (Field field : publicFields) {
            System.out.println("declared field name : " + field.getName());
        }

        try {
            // 获取当前类和父类的某个公有属性
            Field gradeField = student.getClass().getDeclaredField("mGrade");
            // 获取属性值
            System.out.println(" my grade is : " + gradeField.getInt(student));
            // 设置属性值
            gradeField.set(student, 10);
            System.out.println(" my grade is : " + gradeField.getInt(student));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
```

#### 4.2 获取当前类、父类中定义的公有属性

要获取当前类以及父类中的所有 public 属性可以通过 Class 中的 getFields 函数，而 getField 则是获取某个指定的属性。代码示例如下 :

```
    private static void showFields() {
        Student student = new Student("mr.simple");
        // 获取当前类和父类的所有公有属性
        Field[] publicFields = student.getClass().getFields();
        for (Field field : publicFields) {
            System.out.println("field name : " + field.getName());
        }

        try {
            // 获取当前类和父类的某个公有属性
            Field ageField = student.getClass().getField("mAge");
            System.out.println(" age is : " + ageField.getInt(student));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
```

**接口说明**

```
// 获取 Class 对象中指定属性名的属性，参数一为属性名
public Method getDeclaredField (String name)

// 获取该 Class 对象中的所有属性( 不包含从父类继承的属性 )
public Method[] getDeclaredFields ()

// 获取指定的 Class 对象中的**公有**属性，参数一为属性名
public Method getField (String name)

// 获取该 Class 对象中的所有**公有**属性 ( 包含从父类和接口类集成下来的公有属性 )
public Method[] getFields ()
```

这里需要注意的是 getDeclaredField 和 getDeclaredFields 包含 private、protected、default、public 的属性，并且通过这两个函数获取到的只是在自身中定义的属性，从父类中集成的属性不能够获取到。而 getField 和 getFields 只包含 public 属性，父类中的公有属性也能够获取到。

### 5 反射获取父类与接口

#### 5.1 获取父类

获取 Class 对象的父类。

```
    Student student = new Student("mr.simple");
    Class<?> superClass = student.getClass().getSuperclass();
    while (superClass != null) {
        System.out.println("Student's super class is : " + superClass.getName());
        // 再获取父类的上一层父类，直到最后的 Object 类，Object 的父类为 null
        superClass = superClass.getSuperclass();
    }
```

#### 5.2 获取接口

获取 Class 对象中实现的接口。

```
    private static void showInterfaces() {
        Student student = new Student("mr.simple");
        Class<?>[] interfaceses = student.getClass().getInterfaces();
        for (Class<?> class1 : interfaceses) {
            System.out.println("Student's interface is : " + class1.getName());
        }
    }
```

### 6 获取注解信息

在框架开发中，注解加反射的组合使用是最为常见形式的。关于注解方面的知识请参考[公共技术点之 Java 注解 Annotation](http://a.codekk.com/detail/Android/Trinea/公共技术点之 Java 注解 Annotation)，定义注解时我们会通过@Target 指定该注解能够作用的类型，看如下示例:

```
    @Target({
            ElementType.METHOD, ElementType.FIELD, ElementType.TYPE
    })
    @Retention(RetentionPolicy.RUNTIME)
    static @interface Test {

    }
```

上述注解的@target 表示该注解只能用在函数上，还有 Type、Field、PARAMETER 等类型，可以参考上述给出的参考资料。通过反射 api 我们也能够获取一个 Class 对象获取类型、属性、函数等相关的对象，通过这些对象的 getAnnotation 接口获取到对应的注解信息。 首先我们需要在目标对象上添加上注解，例如 :

```
@Test(tag = "Student class Test Annoatation")
public class Student extends Person implements Examination {
    // 年级
    @Test(tag = "mGrade Test Annotation ")
    int mGrade;

    // ......
}
```

然后通过相关的注解函数得到注解信息，如下所示 :

```
    private static void getAnnotationInfos() {
        Student student = new Student("mr.simple");
        Test classTest = student.getClass().getAnnotation(Test.class);
        System.out.println("class Annotatation tag = " + classTest.tag());

        Field field = null;
        try {
            field = student.getClass().getDeclaredField("mGrade");
            Test testAnnotation = field.getAnnotation(Test.class);
            System.out.println("属性的 Test 注解 tag : " + testAnnotation.tag());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
```

输出结果为 : >

```
class Annotatation tag = Student class Test Annoatation
属性的 Test 注解 tag : mGrade Test Annotation
```

**接口说明**

```
// 获取指定类型的注解
public <A extends Annotation> A getAnnotation(Class<A> annotationClass) ;
// 获取 Class 对象中的所有注解
public Annotation[] getAnnotations() ;
```

## 杂谈

反射作为 Java 语言的重要特性，在开发中有着极为重要的作用。很多开发框架都是基于反射来实现对目标对象的操作，而反射配合注解更是设计开发框架的主流选择，例如 ActiveAndroid，因此深入了解反射的作用以及使用对于日后开发和学习必定大有益处。