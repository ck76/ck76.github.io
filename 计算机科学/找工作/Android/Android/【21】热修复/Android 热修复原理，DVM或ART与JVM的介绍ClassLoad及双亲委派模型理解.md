# 导语

 

热修复说白了就是”打补丁”，通过事先设定的接口从网上下载无Bug的代码来替换有Bug的代码。这样就省事多了,用户体验也好。这样带来的优势就是成本低、效率高。热修复的特点：无需重新发版，实时高效热修复；用户无感知修复，无需下载新的应用，代价小；修复成功率高，把损失降到最低。但是，Android是如何实现热修复的呢？这一次要从DVM（Dalvik虚拟机）与 JVM（JAVA虚拟机）的加载类原理讲起。

 

相关文章连接：

[Android热修复开源方案阿里、微信、美团等](http://blog.csdn.net/csdn_aiyang/article/details/76190969)

[深刻理解JVM（JAVA虚拟机）及GC机制等](https://blog.csdn.net/csdn_aiyang/article/details/78652993)

##  

## 一、JVM机制

JVM（Java Virtual Machine）即Java虚拟机，它可以通过 类加载器 把 Class文件 加载到自己 运行时内存中去执行。虚拟机是运行在操作系统中的，而进程又是操作系统的执行单位，所以当java虚拟机运行的时候，它就是操作系统中的进程实例单位，当它没运行时，可以把它叫做程序。总而言之，Java程序在运行的时候，JVM通过类加载机制(ClassLoader)把.class文件加载到内存中。只有class文件被载入内存，才能被其他class引用，使程序正确运行起来。

**JVM类加载机制(ClassLoader)**

1. Bootstrap ClassLoader ：启动类加载器，负责加载java基础类，对应的文件是%JRE_HOME/lib/ 目录下的rt.jar、resources.jar、charsets.jar和class等；
2. Extension ClassLoader：扩展类加载器,对应的文件是 %JRE_HOME/lib/ext 目录下的jar和class等；
3. App ClassLoader：系统类加载器，对应的文件是应用程序classpath目录下的所有jar和class等。

<img src="https://tva1.sinaimg.cn/large/008eGmZEly1gmxiawdlj6j30ui0hin1d.jpg" alt="img" style="zoom:50%;" />

**工作原理：双亲委派机制**

 三种ClassLoader存在父子关系，App ClassLoader的父类加载器是Extension ClassLoader，Extension ClassLoader的父类加载器是Bootstrap ClassLoader。注意这里的父子并不是继承关系。Java的类加载使用双亲委托机制来搜索类，即当这三者中的某个ClassLoader要加载一个类时，会先委托它的父类加载器尝试加载，一直往上，如果最顶级的父类加载器没有找到该类，那么委托者则亲自到特定的地方加载，如果没找到，那么就抛出异常ClassNotFoundException。

案例验证：

```java
public class Test {  
  
    public static void main(String[] args) {  
        ClassLoader ClassLoader1 = Test.class.getClassLoader();  
        ClassLoader ClassLoader2 = ClassLoader1.getParent();  
        ClassLoader ClassLoader3 = ClassLoader2.getParent();  
  
        System.out.println(ClassLoader1);  
        System.out.println(ClassLoader2);  
        System.out.println(ClassLoader3);  
    }  
}  
```

输出结果：

**[java]** [view plain](http://blog.csdn.net/qq_31530015/article/details/51768937#) [copy](http://blog.csdn.net/qq_31530015/article/details/51768937#)



1. sun.misc.Launcher$AppClassLoader@1bbf1ca 
2. sun.misc.Launcher$ExtClassLoader@1ff0dde 
3. null

 

 

## 二、DVM机制 

Dalvik是Google为Android平台设计的虚拟机，以.dex（Dalvik Executable）格式作为虚拟机的压缩格式，适用于内存和处理器有限的操作系统。Delvik允许同时运行多个虚拟机实例，但是Delvik环境下每次运行应用都需要通过及时编译器（JIT）将字节码转为机器码，这虽然安装过程比较快，但是会拖慢应用每次启动的效率，并且会重复的JIT（Just-In-Time）会增加CPU的工作造成损耗。于是，2014年6月Google IO大会推出ART（Android Runtime）代替Delvik，在ART环境下应用安装时，通过预编译（AOT:Ahead-Of-Time）将字节码转为机器码，在溢出解释代码这一过程后，这样就提高了每次应用启动的运行效率，减少CPU的重复耗损工作。但是是以空间换时间，可能会增加10%-20%的存储空间和更长时间的应用安装。

**DVM类加载机制**

主要是由BaseDexClassLoader的两个子类 PathClassLoader、DexClassLoader 来完成。继承自Java的ClassLoader。

PathClassLoader ：用来加载系统类和应用类。只能加载已安装的apk。

DexClassLoader  ：用来加载jar、apk、dex文件。从SD卡中加载未安装的apk。

<img src="https://tva1.sinaimg.cn/large/008eGmZEly1gmxkoqpovmj30su0dkdk0.jpg" alt="img" style="zoom:67%;" />

PathClassLoader.Class 源代码

```java
public class PathClassLoader extends BaseDexClassLoader {
 
    public PathClassLoader(String dexPath, ClassLoader parent) {
        super(dexPath, null, null, parent);
    }
 
    public PathClassLoader(String dexPath, String libraryPath,
            ClassLoader parent) {
        super(dexPath, null, libraryPath, parent);
    }
} 
```

DexClassLoader.Class 源代码

```java
public class DexClassLoader extends BaseDexClassLoader {
 
    public DexClassLoader(String dexPath, String optimizedDirectory, String libraryPath, ClassLoader parent) {
        super(dexPath, new File(optimizedDirectory), libraryPath, parent);
    }
}
```

BaseDexClassLoader.Class 源代码  

```java
public class BaseDexClassLoader extends ClassLoader {
    private final DexPathList pathList;
 
    public BaseDexClassLoader(String dexPath, File optimizedDirectory,
            String libraryPath, ClassLoader parent) {
        super(parent);
        //这里创建了一个DexPathList对象实例
        this.pathList = new DexPathList(this, dexPath, libraryPath, optimizedDirectory);
    }
 
    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
        List<Throwable> suppressedExceptions = new ArrayList<Throwable>();
        Class c = pathList.findClass(name, suppressedExceptions);
        if (c == null) {
            ClassNotFoundException cnfe = new ClassNotFoundException("Didn't find class \"" + name + "\" on path: " + pathList);
            for (Throwable t : suppressedExceptions) {
                cnfe.addSuppressed(t);
            }
            throw cnfe;
        }
        return c;
    }
```

 

##  

## 三、热修复原理

  分析上面DVM加载类机制的BaseDexClassLoader.Class 代码  ，它类中代码：

  首先看到声明了DexPathList类名为pathList，然后看它的构造函数中创建了DexPathList类的实例，看一下这个方法：

 

```java
public DexPathList(ClassLoader definingContext, String dexPath, String libraryPath, File optimizedDirectory) {
        ... 
        this.definingContext = definingContext;
        ArrayList<IOException> suppressedExceptions = new ArrayList<IOException>();
        //创建一个数组
        this.dexElements = makeDexElements(splitDexPath(dexPath), optimizedDirectory, suppressedExceptions);
        ... 
    }
```

可以看到，DexPathList的构造函数中创建一个dexElements 数组。

然后再回头看，BaseDexClassLoader.Class 代码的findClass方法。调用了pathList.findClass。

```java
/* package */final class DexPathList {
    ...
    public Class findClass(String name, List<Throwable> suppressed) {
            //遍历该数组
        for (Element element : dexElements) {
            //初始化DexFile
            DexFile dex = element.dexFile;
            if (dex != null) {
                //调用DexFile类的loadClassBinaryName方法返回Class实例
                Class clazz = dex.loadClassBinaryName(name, definingContext, suppressed);
                if (clazz != null) {
                    return clazz;
                }
            }
        }       
        return null;
    }
    ...
} 
```

会遍历这个dexElements 数组。然后初始化DexFile，如果DexFile不为空那么调用DexFile类的loadClassBinaryName方法返回Class实例。总而言之，ClassLoader会遍历这个数组,然后加载这个数组中的dex文件， 而ClassLoader在加载到正确的类之后,就不会再去加载有Bug的那个类了,我们把这个正确的类放在Dex文件中，让这个Dex文件排在dexElements数组前面即可。

但是，这里有个问题：

如果引用者和被引用者的类(直接引用关系)在同一个Dex时,那么在虚拟机启动时，被引用类就会被打上CLASS_ISPREVERIFIED标志。这样被引用的类就不能进行热修复操作了。怎么办？

原因是：那么我们就要阻止被引用类打上CLASS_ISPREVERIFIED标志，QQ空间的方法是在所有引用到该类的构造函数中插入一段代码,代码引用到别的类。这个可参考QQ空间团队的 [安卓App热补丁动态修复技术介绍](https://mp.weixin.qq.com/s?__biz=MzI1MTA1MzM2Nw==&mid=400118620&idx=1&sn=b4fdd5055731290eef12ad0d17f39d4a&scene=1&srcid=1106Imu9ZgwybID13e7y2nEi#wechat_redirect   da) 

------

为了实现补丁方案，防止类被打上CLASS_ISPREVERIFIED标志。最终QQ空间的方案是往所有类的构造函数里面插入了一段代码，代码如下：

```java
if (ClassVerifier.PREVENT_VERIFY) {
 
System.out.println(AntilazyLoad.class);
 
}
```

 

其中AntilazyLoad类会被打包成单独的hack.dex，这样当安装apk的时候，classes.dex内的类都会引用一个在不相同dex中的AntilazyLoad类，这样就防止了类被打上CLASS_ISPREVERIFIED的标志了，只要没被打上这个标志的类都可以进行打补丁操作。