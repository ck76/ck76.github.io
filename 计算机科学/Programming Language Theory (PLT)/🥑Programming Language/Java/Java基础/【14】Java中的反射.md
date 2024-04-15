[TOC]

> 作用，动态语言

## 什么是反射（Reflection ）？

主要是指程序可以访问、检测和修改它本身状态或行为的一种能力

## Java反射？

在Java运行时环境中，对于任意一个类，能否知道这个类有哪些属性和方法？对于任意一个对象，能否调用它的任意一个方法

Java反射机制主要提供了以下功能：

 * 1.在运行时判断任意一个对象所属的类。
 * 2.在运行时构造任意一个类的对象。
 * 3.在运行时判断任意一个类所具有的成员变量和方法。
 * 4.在运行时调用任意一个对象的方法。 

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