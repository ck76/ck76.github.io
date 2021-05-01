

***\*DexClassLoader\****

可以指定apk包路径和dex文件解压路径

所以所有用户都可以使用。

***\*PathClassLoader\****

只能指定加载apk包路径，不能指定dex文件解压路径。

该路径是写死的在/data/dalvik-cache/路径下。

所以只能用于加载已安装的APk。

其实如果你有权限可以在/data/dalvik-cache/路径下创建文件的话，使用他也是可以的。


---
先说一下为什么要抛出这个问题吧？

最近在看插件化相关的技术,因此会涉及到插件中的类如何加载,根据我以前的了解，再加上在网上查了解的知识,认为他们的区别是:

- DexClassLoader : 可加载jar、apk和dex，可以SD卡中加载
- PathClassLoader : ==只能==加载已安裝到系統中（即/data/app目录下）的apk文件

有这两个区别是因为`DexClassLoader`在构造的时候多传了一个`optimizedDirectory`参数，因此造成了这个区别:

```
 public DexClassLoader(String dexPath, String optimizedDirectory,String librarySearchPath, ClassLoader parent) {        super(dexPath, new File(optimizedDirectory), librarySearchPath, parent);
    }
```

但我在看源码的时候发现了一个问题 : 我发现在最新的源码中这个参数已经被`deprecated`了。而且源码好像真没有表现出他们俩有什么不同

> DexClassLoader.java

```
public class DexClassLoader extends BaseDexClassLoader {    /**     @param optimizedDirectory this parameter is deprecated and has no effect since API level 26.
     */
    public DexClassLoader(String dexPath, String optimizedDirectory, String librarySearchPath, ClassLoader parent) {
        super(dexPath, null, librarySearchPath, parent);
    }
}
```

> PathClassLoader.java

```
public class PathClassLoader extends BaseDexClassLoader {    public PathClassLoader(String dexPath, ClassLoader parent) {        super(dexPath, null, null, parent);
    }    public PathClassLoader(String dexPath, String librarySearchPath, ClassLoader parent) {        super(dexPath, null, librarySearchPath, parent);
    }
}
```

即，`DexClassLoader`传的`optimizedDirectory` 参数根本没用。 官方已经标注了`is deprecated` & `no effect since API level 26`。 那 *DexClassLoader相比于PathClassLoader可以加载SD卡上的apk* 是怎么得出的呢?

在最新源码中，这两者构造函数的能力是一样的。并且 *基类是不可能强判子类做相关处理逻辑的吧？*, 因此，再看一下官方文档对这两个类的解释:

- PathClassLoader

> 提供ClassLoader在本地文件系统中的文件和目录列表上运行的简单实现，但不尝试从网络加载类。Android将此类用于其系统类加载器及其应用程序类加载器。

- DexClassLoader

> 它可以加载 .jar、.apk和dex文件。这可用于执行未作为应用程序的一部分安装的代码。在API级别26之前，此类加载器需要一个应用程序专用的可写目录来缓存优化的类。使用Context.getCodeCacheDir()创建这样一个目录：
> `File dexOutputDir = context.getCodeCacheDir();`自`API 26`后不要在外部存储上缓存优化的类。 外部存储不提供保护应用程序免受代码注入攻击所必需的访问控制。

看官方文档，好像说的也不明白。但是在26以前`optimizedDirectory`参数是用来指明缓存优化后的加载的类的目录。26以后就废弃了。

我对这两个类做了一个测试发现: `PathClassLoader`也是可以加载`SD卡`上的apk的。

下面是测试代码:

```
    private void loadClassTest() {
        File apk = new File(Environment.getExternalStorageDirectory(), "Test1.apk");
        PathClassLoader pathClassLoader = new PathClassLoader(apk.getAbsolutePath(), null, this.getApplication().getClassLoader());
        DexClassLoader dexClassLoader = new DexClassLoader(apk.getAbsolutePath(), null, null, this.getApplication().getClassLoader());

        String classNameInTestApk = "com.susion.myapplication.modle2.Module2";        try {
            Class loadByPathClassLoader = pathClassLoader.loadClass(classNameInTestApk);
            Log.e("susion", " PathClassLoader  load success : " + loadByPathClassLoader.getName());

            Class loadByDexClassLoader = dexClassLoader.loadClass(classNameInTestApk);
            Log.e("susion", " DexClassLoader load success : " + loadByDexClassLoader.getName());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
```

*跑这段代码前注意申请相关存储权限。* 这个`Test1.apk`是我用另一个工程打的包，放在了sd卡的根目录。并没有安装在手机上。

首先我在 `API Platform 27`上跑了这段代码(即 compileSdkVersion = 27),打印的log如下:

```
  PathClassLoader  load success : com.susion.myapplication.modle2.Module2
  DexClassLoader load success : com.susion.myapplication.modle2.Module2
```

这段代码在 21、18上跑的效果是一样的。即，都能加载成功。那这两个ClassLoader到底有什么区别呢？

---

## 前言

一般说起 PathClassLoader 和 DexClassLoader ，大家都会说，前者只能加载内存中已经安装的apk中的dex，而后者可以加载sd卡中的apk/jar ，因此 DexClassLoader 是热修复和插件化的基础。但是具体为什么DexClassLoader能加载sd卡中的类，很多文章都只是一笔带过 ，于是研究了下源码，做个记录。

【注意】本文所参考的源码基于Android 7.1.2_r36 及以前版本，在Android 8.0.0_r4 之后，BaseDexClassLoader 、DexClassLoader 源码有所变动

### PathClassLoader

```Java
//类路径：/libcore/dalvik/src/main/java/dalvik/system/PathClassLoader.java
public PathClassLoader(String dexPath, String librarySearchPath, ClassLoader parent) {
    super(dexPath, null, librarySearchPath, parent);
}1234
```

### DexClassLoader

```Java
//类路径：/libcore/dalvik/src/main/java/dalvik/system/DexClassLoader.java
 public DexClassLoader(String dexPath, String optimizedDirectory,
            String librarySearchPath, ClassLoader parent) {
    super(dexPath, new File(optimizedDirectory), librarySearchPath, parent);
}12345
```

可以看到，两者都是继承自`BaseDexClassLoader` ，构造方法的具体逻辑在父类中实现，唯一不同的是一个参数：`optimizedDirectory`

### BaseDexClassLoader 构造方法

```Java
//类路径： /libcore/dalvik/src/main/java/dalvik/system/BaseDexClassLoader.java
public BaseDexClassLoader(String dexPath, File optimizedDirectory,
            String librarySearchPath, ClassLoader parent) {
    super(parent);
    this.pathList = new DexPathList(this, dexPath, librarySearchPath, optimizedDirectory);
}123456
```

`BaseDexClassLoader`的构造方法，只是用这些参数，创建了一个`DexPathList`的实例，DexPathList 使用 Element 数组存储了所有的 dex 信息，在 dex 被存到 Element 数组后，所有的类都会在 Element 数组中寻找，不会再次从文件中加载

```Java
//类路径：/libcore/dalvik/src/main/java/dalvik/system/BaseDexClassLoader.java
 @Override
protected Class<?> findClass(String name) throws ClassNotFoundException {
    List<Throwable> suppressedExceptions = new ArrayList<Throwable>();
    //从pathList的Element数组中找类，找不到就报ClassNotFoundException
    Class c = pathList.findClass(name, suppressedExceptions);
    if (c == null) {
        ClassNotFoundException cnfe = new ClassNotFoundException("Didn't find class \"" + name + "\" on path: " + pathList);
        for (Throwable t : suppressedExceptions) {
            cnfe.addSuppressed(t);
        }
        throw cnfe;
    }
    return c;
}123456789101112131415
```

### DexPathList

DexPathList 通过 `makeDexElements` 得到了一个 `Element[]`类型的 `dexElements`对象数组，里面存放了app的所有dex相关信息。这里仍然看不出 PathClassLoader 和 DexClassLoader 的具体差别，只知道前者参数中的 `optimizedDirectory` 传的是 null 。

```java
//类路径：/libcore/dalvik/src/main/java/dalvik/system/DexPathList.java
public DexPathList(ClassLoader definingContext, String dexPath,
            String librarySearchPath, File optimizedDirectory) {
    //...略
    this.definingContext = definingContext;
    ArrayList<IOException> suppressedExceptions = new ArrayList<IOException>();
    // save dexPath for BaseDexClassLoader
    this.dexElements = makeDexElements(splitDexPath(dexPath), optimizedDirectory,suppressedExceptions,definingContext);
    //...略
}12345678910
```

在 `makeElements` 中，使用for，循环调用了 loadDexFile 来加载 dexPath 中每个目录中的 dex

```Java
 dex = loadDexFile(file, optimizedDirectory, loader, elements);1
```

再看 loadDexFile 函数：

```Java
//类路径：/libcore/dalvik/src/main/java/dalvik/system/DexPathList.java
private static DexFile loadDexFile(File file, File optimizedDirectory, ClassLoader loader,
                                       Element[] elements)
            throws IOException {
        if (optimizedDirectory == null) {
            return new DexFile(file, loader, elements);
        } else {
            String optimizedPath = optimizedPathFor(file, optimizedDirectory);
            return DexFile.loadDex(file.getPath(), optimizedPath, 0, loader, elements);
        }
    }1234567891011
```

根据 `optimizedDirectory` 参数是否为空，执行的方法不同，`PathClassLoader` 执行的是 `new DexFile()`， 而 `DexClassLoader` 执行的是 `DexFile.loadDex()` , 然而 loadDex 最终也还是会调用 `new DexFile()` 来创建实例。

### DexFile

```java
//类路径：/libcore/dalvik/src/main/java/dalvik/system/DexFile.java
static DexFile loadDex(String sourcePathName, String outputPathName,
        int flags, ClassLoader loader, DexPathList.Element[] elements) throws IOException {
    return new DexFile(sourcePathName, outputPathName, flags, loader, elements);
}
private DexFile(String sourceName, String outputName, int flags, ClassLoader loader,DexPathList.Element[] elements) throws IOException {
        mCookie = openDexFile(sourceName, outputName, flags, loader, elements);
        mFileName = sourceName;
}
private static Object openDexFile(String sourceName, String outputName, int flags,
            ClassLoader loader, DexPathList.Element[] elements) throws IOException {
    return openDexFileNative(new File(sourceName).getAbsolutePath(),
                                 (outputName == null)
                                     ? null
                                     : new File(outputName).getAbsolutePath(),
                                 flags,
                                 loader,
                                 elements);
}
private static native Object openDexFileNative(String sourceName, String outputName, int flags,
            ClassLoader loader, DexPathList.Element[] elements);123456789101112131415161718192021
```

在 DexFile 的构造方法中，调用了 `openDexFile` 去生成一个 `mCookie`，可以看到，不管是哪个类型的ClassLoader，最终都会调用 native 方法 `openDexFileNative` 来实现具体的加载逻辑。

### Native 层的源码追踪

#### DexFile_openDexFileNative

```C++
static jint DexFile_openDexFileNative(JNIEnv* env, jclass, jstring javaSourceName, jstring javaOutputName, jint) {    
    //...略
    const DexFile* dex_file;    
    if (outputName.c_str() == NULL) {// 如果outputName为空，则dex_file由sourceName确定
        dex_file = linker->FindDexFileInOatFileFromDexLocation(dex_location, dex_location_checksum); 
    } else {// 如果outputName不为空，则在outputName目录中去寻找dex_file
        std::string oat_location(outputName.c_str());    
        dex_file = linker->FindOrCreateOatFileForDexLocation(dex_location, dex_location_checksum, oat_location);  
    }    
    //...略
    return static_cast<jint>(reinterpret_cast<uintptr_t>(dex_file));    
}123456789101112
```

判断传入的 `outputName` 是否为空，分别执行不同的方法，这个 outputName 就是 BaseDexClassLoader 构造方法中传入的 `optimizedDirectory` 参数，辗转来到这里。

- 当 outputName **不为空**时【DexClassLoader】
  执行`FindOrCreateOatFileForDexLocation`函数，通过 `outputName`拿到 `oat_location` ，然后尝试调用 `FindDexFileInOatLocation` 从 oat_location 中寻找到 dex ，这就是我们经常用到到热修复的原理了，通过在sd卡中存放新的补丁dex/jar/apk替代旧的，来实现更新。

```C++
const DexFile* ClassLinker::FindOrCreateOatFileForDexLocation(const std::string& dex_location,uint32_t dex_location_checksum,const std::string& oat_location) {
    WriterMutexLock mu(Thread::Current(), dex_lock_);   // 互锁
    return FindOrCreateOatFileForDexLocationLocked(dex_location, dex_location_checksum, oat_location);
}

const DexFile* ClassLinker::FindOrCreateOatFileForDexLocationLocked(const std::string& dex_location,uint32_t dex_location_checksum,const std::string& oat_location) {
    const DexFile* dex_file = FindDexFileInOatLocation(dex_location,dex_location_checksum,oat_location);
    if (dex_file != NULL) {
        // 如果顺利打开，则返回
        return dex_file;
    }
    const OatFile* oat_file = OatFile::Open(oat_location, oat_location, NULL,!Runtime::Current()->IsCompiler());
    if (oat_file == NULL) {
        return NULL;
    }
    const OatFile::OatDexFile* oat_dex_file = oat_file->GetOatDexFile(dex_location, &dex_location_checksum);
    if (oat_dex_file == NULL) {
        return NULL;
    }
    const DexFile* result = oat_dex_file->OpenDexFile();
    return result;
}12345678910111213141516171819202122
```

- 当 outputName 为空时【PathClassLoader】
  执行 `FindDexFileInOatFileFromDexLocation` 函数，从 dex_location 中拿到 dex 文件，这个 dex_location 也就是 BaseDexClassLoader 的 `dexPath` 参数中分割出来的某个存放文件的路径。在 Android 中，系统使用 PathClassLoader 来加载apk中的dex存放到Element数组中，因此apk中的classes.dex都是通过它来加载的。

## 总结

Android 中，apk 安装时，系统会使用 PathClassLoader 来加载apk文件中的dex，PathClassLoader的构造方法中，调用父类的构造方法，实例化出一个 DexPathList ，DexPathList 通过 makePathElements 在所有传入的dexPath 路径中，找到DexFile，存入 Element 数组，在应用启动后，所有的类都在 Element 数组中寻找，不会再次加载。

在热更新时，实现 DexClassLoader 子类，传入要更新的dex/apk/jar补丁文件路径(如sd卡路径中存放的patch.jar)，通过反射拿到 DexPathList，得到补丁 Element 数组，再从Apk原本安装时使用的 PathClassLoader 中拿到旧版本的 Element 数组，合并新旧数组，将补丁放在数组最前面，这样一个类一旦在补丁 Element 中找到，就不会再次加载，这样就能替换旧 Element 中的旧类，实现热更新。

---

### 预备知识

1. 了解 android 基本 ClassLoader 知识

### 看完本文可以达到什么程度

1. 了解 PathClassLoader 和 DexClassLoader 区别

### 文章概览



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmykw8g9k3j30lx0e9mxz.jpg)



### 一、起因

说起 Android 中的 PathClassLoader 和 DexClassLoader，先提出一个疑问，PathClassLoader 和 DexClassLoader 有什么区别呢？
关于答案，我斗胆猜测一下，大家心中的回答一定是 PathClassLoader 是用来加载已经安装的 apk 的，DexClassLoader 是用来加载存储空间的 dex / apk 文件的。为什么这样说呢，因为之前我也一直这样理解的，而且网上大部分文章中也都是这样讲解的。
那为何突然又谈起 PathClassLoader 和 DexClassLoader 呢？起因是我在前段时间写了一些插件化的 demo，当时忘记了 PathClassLoader 和 DexClassLoader 这回事，直接用 PathClassLoader 去加载插件了，竟然也可以加载成功？？？一丝丝的困惑浮现在我英俊帅气的脸庞上，聪明的小脑瓜里打上了一个小小的问号。于是乎去翻了一下源码，就有了这篇文章。

### 二、先放结论

先放结论，PathClassLoader 和 DexClassLoader **都能加载外部的 dex／apk**，只不过区别是 DexClassLoader 可以**指定 optimizedDirectory**，也就是 dex2oat 的产物 .odex 存放的位置，而 PathClassLoader 只能使用系统默认位置。但是这个 optimizedDirectory 在 Android 8.0 以后也被舍弃了，只能使用系统默认的位置了。

我们这里先基于 android 5.0 代码来分析，然后再看看其他系统版本的一些区别。（选取 5.0 是因为此时 art 的源码还比较简单～）

### 三、ClassLoader 的构造函数



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmykw6if67j30ej0bgwf3.jpg)



### 3.1 BaseDexClassLoader 构造函数

PathClassLoader 和 DexClassLoader 都是继承了 BaseDexClassLoader，这里先看一下。 [BaseDexClassLoader](https://link.zhihu.com/?target=https%3A//www.androidos.net.cn/android/5.0.1_r1/xref/libcore/dalvik/src/main/java/dalvik/system/BaseDexClassLoader.java) 的构造函数。

```text
public class BaseDexClassLoader extends ClassLoader {
    private final DexPathList pathList;

    /**
     * Constructs an instance.
     *
     * @param dexPath the list of jar/apk files containing classes and
     * resources, delimited by {@code File.pathSeparator}, which
     * defaults to {@code ":"} on Android
     * @param optimizedDirectory directory where optimized dex files
     * should be written; may be {@code null}
     * @param libraryPath the list of directories containing native
     * libraries, delimited by {@code File.pathSeparator}; may be
     * {@code null}
     * @param parent the parent class loader
     */
    public BaseDexClassLoader(String dexPath, File optimizedDirectory,
            String libraryPath, ClassLoader parent) {
        super(parent);
        this.pathList = new DexPathList(this, dexPath, libraryPath, optimizedDirectory);
    }
}
```

BaseDexClassLoader 构造函数有四个参数，含义如下：

- **dexPath**: 需要加载的文件列表，文件可以是包含了 classes.dex 的 JAR/APK/ZIP，也可以直接使用 classes.dex 文件，多个文件用 “:” 分割
- **optimizedDirectory**: 存放优化后的 dex，可以为空
- **libraryPath**: 存放需要加载的 native 库的目录
- **parent**: 父 ClassLoader

通过构造函数我们大概可以了解到 BaseDexClassLoader 的运行方式，传入 dex 文件，然后进行优化，保存优化后的 dex 文件到 optimizedDirectory 目录。

### 3.2 PathClassLoader 构造函数

接着我们再看 [PathClassLoader](https://link.zhihu.com/?target=https%3A//www.androidos.net.cn/android/5.0.1_r1/xref/libcore/dalvik/src/main/java/dalvik/system/PathClassLoader.java) 的构造函数。

```text
/**
 * Provides a simple {@link ClassLoader} implementation that operates on a list
 * of files and directories in the local file system, but does not attempt to
 * load classes from the network. Android uses this class for its system class
 * loader and for its application class loader(s).
 */
public class PathClassLoader extends BaseDexClassLoader {
    public PathClassLoader(String dexPath, ClassLoader parent) {
        super(dexPath, null, null, parent);
    }

    /**
     * Creates a {@code PathClassLoader} that operates on two given
     * lists of files and directories. The entries of the first list
     * should be one of the following:
     *
     * <ul>
     * <li>JAR/ZIP/APK files, possibly containing a "classes.dex" file as
     * well as arbitrary resources.
     * <li>Raw ".dex" files (not inside a zip file).
     * </ulyanzheng>
     *
     * The entries of the second list should be directories containing
     * native library files.
     *
     * @param dexPath the list of jar/apk files containing classes and
     * resources, delimited by {@code File.pathSeparator}, which
     * defaults to {@code ":"} on Android
     * @param libraryPath the list of directories containing native
     * libraries, delimited by {@code File.pathSeparator}; may be
     * {@code null}
     * @param parent the parent class loader
     */
    public PathClassLoader(String dexPath, String libraryPath,
            ClassLoader parent) {
        super(dexPath, null, libraryPath, parent);
    }
}
```

关于 PathClassLoader 有一点稍微注意一下，代码注释中对 PathClassLoader 的介绍是，用来操作文件系统上的一系列文件和目录 的 ClassLoader 实现。其中并没有提到只能加载安装后的 apk 文件。
PathClassLoader 有两个构造函数，区别在于传给 BaseDexClassLoader 的 libraryPath 是否为空。最终调用 BaseDexClassLoader 构造函数时，传入的 optimizedDirectory 为空。

### 3.3 DexClassLoader 构造函数

再来看看 [DexClassLoader](https://link.zhihu.com/?target=https%3A//www.androidos.net.cn/android/5.0.1_r1/xref/libcore/dalvik/src/main/java/dalvik/system/DexClassLoader.java) 的构造函数。和 BaseDexClassLoader 构造函数的参数是一样的。

```text
public class DexClassLoader extends BaseDexClassLoader {
    /**
     * Creates a {@code DexClassLoader} that finds interpreted and native
     * code.  Interpreted classes are found in a set of DEX files contained
     * in Jar or APK files.
     *
     * <p>The path lists are separated using the character specified by the
     * {@code path.separator} system property, which defaults to {@code :}.
     *
     * @param dexPath the list of jar/apk files containing classes and
     *     resources, delimited by {@code File.pathSeparator}, which
     *     defaults to {@code ":"} on Android
     * @param optimizedDirectory directory where optimized dex files
     *     should be written; must not be {@code null}
     * @param librarySearchPath the list of directories containing native
     *     libraries, delimited by {@code File.pathSeparator}; may be
     *     {@code null}
     * @param parent the parent class loader
     */
    public DexClassLoader(String dexPath, String optimizedDirectory,
            String librarySearchPath, ClassLoader parent) {
        super(dexPath, new File(optimizedDirectory), librarySearchPath, parent);
    }
}
```

通过上面对构造函数的分析，我们可以明白，PathClassLoader 和 DexClassLoader 关键不同点，在 optimizedDirectory 参数上，PathClassLoader 传入的是 null，而 DexClassLoader 传入的是用户指定的目录。

### 四、optimizedDirectory 参数的处理

既然知道了区别在 optimizedDirectory，那就来看看 BaseDexClassLoader 里是怎么处理 optimizedDirectory 的。

### 4.1 DexPathList 处理

在 BaseDexClassLoader 里，直接将 optimizedDirectory 透传给了 DexPathList。 这里先简单介绍一下 DexPathList。 DexPathList 里有两个成员变量，dexElements 用来保存 dex 和资源列表，nativeLibraryDirectories 用来保存 native 库列表。

```text
class DexPathList {
    private final Element[] dexElements;
    private final File[] nativeLibraryDirectories;
}
```

在 DexPathList 中，使用 optimizedDirectory 的路径是：

```text
DexPathList -> makeDexElements -> loadDexFile
```

这里要看一下 loadDexFile 方法。

```text
class DexPathList {
    private static DexFile loadDexFile(File file, File optimizedDirectory)
            throws IOException {
        if (optimizedDirectory == null) {
            return new DexFile(file);
        } else {
            String optimizedPath = optimizedPathFor(file, optimizedDirectory);
            return DexFile.loadDex(file.getPath(), optimizedPath, 0);
        }
    }
}
```

在 DexPathList 中，会为每一个 DEX 文件创建一个 DexFile 对象，创建方式有两种，optimizedDirectory 为空时，调用 DexFile(file) 创建，否则调用 DexFile.loadDex()。
这样对于 optimizedDirectory 的处理就流转到 DexFile 里了。

### 4.2 DexFile 处理

其实在 DexFile.loadDex 里，也是直接调用了 [DexFile](https://link.zhihu.com/?target=https%3A//www.androidos.net.cn/android/5.0.1_r1/xref/libcore/dalvik/src/main/java/dalvik/system/DexFile.java) 的构造函数

```text
class DexFile {
       public DexFile(File file) throws IOException {
        this(file.getPath());
    }

    public DexFile(String fileName) throws IOException {
        // 调用 openDexFile 处理 dex
        mCookie = openDexFile(fileName, null, 0);
        mFileName = fileName;
        guard.open("close");
    }

    private DexFile(String sourceName, String outputName, int flags) throws IOException {
        // ...
        // 调用 openDexFile 处理 dex
        mCookie = openDexFile(sourceName, outputName, flags);
        mFileName = sourceName;
        guard.open("close");
    }

    static public DexFile loadDex(String sourcePathName, String outputPathName,
        int flags) throws IOException {
        return new DexFile(sourcePathName, outputPathName, flags);
    }

    private static long openDexFile(String sourceName, String outputName, int flags) throws IOException {
        // 最终调用 native 方法
        return openDexFileNative(new File(sourceName).getAbsolutePath(),
                                 (outputName == null) ? null : new File(outputName).getAbsolutePath(),
                                 flags);
    }

    private static native long openDexFileNative(String sourceName, String outputName, int flags);
}
```

DexFile 代码不多，上面基本上就是主要代码了。我们可以看到，不管调用 DexFile 哪个构造函数，最后都会通过 openDexFileNative 进行处理，区别就在于 outputName 参数是否为空，而 outputName 参数，就是上面一路传递下来的 optimizeDirectory 参数。
我们再回顾一下调用的链路：

```text
PathClassLoader.constructor / DexClassLoader.constructor -> BaseDexClassLoader.constructor -> DexPathList.constructor -> DexPathList.makeDexElements -> DexPathList.loadDexFile -> DexFile.constructor / DexFile.loadDex -> DexFile.openDexFile -> DexFile.openDexFileNative
```

再继续往下看，就走到了 native 逻辑。native 逻辑可以下载 [art 源码](https://link.zhihu.com/?target=https%3A//android.googlesource.com/platform/art/)对照查看。

### 4.3 native 处理

openDexFileNative 对应的 native 逻辑在 [http://dalvik_system_DexFile.cc](https://link.zhihu.com/?target=http%3A//dalvik_system_DexFile.cc) 里的 DexFile_openDexFileNative 方法。
在 DexFile_openDexFileNative 里主要做事情是处理 DEX 文件，并生成 .odex 文件到 optimizedDirectory 里。
这里关于 optimizedDirectory 的处理路径是：

```text
DexFile_openDexFileNative -> ClassLinker::OpenDexFilesFromOat
```

在 OpenDexFilesFromOat 里有这样一段处理逻辑：

```text
ClassLinker::OpenDexFilesFromOat() {
  // ...
  if (oat_location == nullptr) {
    // 如果 oat_location 为空，就使用默认的 dalvikcache 
    const std::string dalvik_cache(GetDalvikCacheOrDie(GetInstructionSetString(kRuntimeISA)));
    cache_location = GetDalvikCacheFilenameOrDie(dex_location, dalvik_cache.c_str());
    oat_location = cache_location.c_str();
  }
  // ...
  if (Runtime::Current()->IsDex2OatEnabled() && has_flock && scoped_flock.HasFile()) {
    // Create the oat file.
    open_oat_file.reset(CreateOatFileForDexLocation(dex_location, scoped_flock.GetFile()->Fd(),
                                                    oat_location, error_msgs));
  }
}
```

上面方法里的 oat_location 就是 optimizeDirectory 传入到 native 中的化身。这里有一个判断逻辑，如果 oat_location 为空的话，就采用默认的 dalvikcache 路径。之后调用 CreateOatFileForDexLocation 去优化 DEX 文件了。
而 dalvikcache 是通过 GetDalvikCacheOrDie 获取的。

```text
// art/runtime/utils.cc
std::string GetDalvikCacheOrDie(const char* subdir, const bool create_if_absent) {
  CHECK(subdir != nullptr);
  // 这里的 AndroidData 就是 /data 目录
  const char* android_data = GetAndroidData();
  const std::string dalvik_cache_root(StringPrintf("%s/dalvik-cache/", android_data));
  const std::string dalvik_cache = dalvik_cache_root + subdir;
  if (create_if_absent && !OS::DirectoryExists(dalvik_cache.c_str())) {
    // Don't create the system's /data/dalvik-cache/... because it needs special permissions.
    if (strcmp(android_data, "/data") != 0) {
      int result = mkdir(dalvik_cache_root.c_str(), 0700);
      if (result != 0 && errno != EEXIST) {
        PLOG(FATAL) << "Failed to create dalvik-cache directory " << dalvik_cache_root;
        return "";
      }
      result = mkdir(dalvik_cache.c_str(), 0700);
      if (result != 0) {
        PLOG(FATAL) << "Failed to create dalvik-cache directory " << dalvik_cache;
        return "";
      }
    } else {
      LOG(FATAL) << "Failed to find dalvik-cache directory " << dalvik_cache;
      return "";
    }
  }
  return dalvik_cache;
}
```

GetDalvikCacheOrDie 获取的就是 /data/dalvik-cache/ 目录。
这里我们回顾一下之前提出的问题，避免迷失在茫茫代码中。
我们的问题是 optimizedDirectory 参数传空和不为空有什么区别，PathClassLoader 传入的 optmizedDirectory 为空，而 DexClassLoader 传入的 optimizedDirectory 是用户自定义的目录。
回看一下调用链路。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmykw297o6j30dz0qg75c.jpg)



```text
PathClassLoader.constructor / DexClassLoader.constructor -> BaseDexClassLoader.constructor -> DexPathList.constructor -> DexPathList.makeDexElements -> DexPathList.loadDexFile -> DexFile.constructor / DexFile.loadDex -> DexFile.openDexFile -> DexFile.openDexFileNative -> DexFile_openDexFileNative -> ClassLinker::OpenDexFilesFromOat
```

到这里我们就可以得出结论了，optmizedDirectory 不为空时，使用用户定义的目录作为 DEX 文件优化后产物 .odex 的存储目录，为空时，会使用默认的 /data/dalvik-cache/ 目录。
所以印证了开头的结论，PathClassLoader 其实并不是只能加载安装后的 APK，也可以加载其他 DEX/JAR/APK 文件，只不过生成的 .odex 文件只能存储在系统默认路径下。
被误导多年的谜题终于解开了。耳边不禁响起柯南破案的 BGM。

### 五、其他系统版本上进行验证

不过上述的分析是在 5.0 源码下进行的，我们再选取 4.4 和 8.0 看一下。
为什么选取这两个版本呢？首先 4.4 和 5.0 是 ART 和 Dalvik 的分水岭，而 8.0 以后对 PathClassLoader 有些改动。

### 5.1 Android 4.4

有了上面的分析基础，我们分析 4.4 的代码就顺畅的多了。一路从 Java 分析到 native。 Java 层代码没有什么变动，native 的入口还是 DexFile_openDexFileNative。之后的代码就有了些许不一样。

```text
DexFile_openDexFileNative() {
  // ...
  if (outputName.c_str() == NULL) {
    dex_file = linker->FindDexFileInOatFileFromDexLocation(dex_location, dex_location_checksum);
  } else {
    std::string oat_location(outputName.c_str());
    dex_file = linker->FindOrCreateOatFileForDexLocation(dex_location, dex_location_checksum, oat_location);
  }
  // ...
}
```

这里和 5.0 的区别就是 根据 outputName 也就是 optimizedDirectory 是否为空，调用了两个不同的函数。 而 FindDexFileInOatFileFromDexLocation 里的逻辑就又有些熟悉了。

```text
ClassLinker::FindDexFileInOatFileFromDexLocation() {
  // ...
  std::string oat_cache_filename(GetDalvikCacheFilenameOrDie(dex_location));
  return FindOrCreateOatFileForDexLocationLocked(dex_location, dex_location_checksum, oat_cache_filename);
}
```

默认也是获取到 dalvikcache 目录作为 .odex 文件的存储路径。

### 5.2 Android 8.0

在 8.0 系统上，事情发生了一些微弱的变化，我们看看 BaseDexClassLoader 的构造函数。

```text
class BaseDexClassLoader {
    /**
     * Constructs an instance.
     * Note that all the *.jar and *.apk files from {@code dexPath} might be
     * first extracted in-memory before the code is loaded. This can be avoided
     * by passing raw dex files (*.dex) in the {@code dexPath}.
     *
     * @param dexPath the list of jar/apk files containing classes and
     * resources, delimited by {@code File.pathSeparator}, which
     * defaults to {@code ":"} on Android.
     * @param optimizedDirectory this parameter is deprecated and has no effect
     * @param librarySearchPath the list of directories containing native
     * libraries, delimited by {@code File.pathSeparator}; may be
     * {@code null}
     * @param parent the parent class loader
     */
    public BaseDexClassLoader(String dexPath, File optimizedDirectory,
            String librarySearchPath, ClassLoader parent) {
        super(parent);
        this.pathList = new DexPathList(this, dexPath, librarySearchPath, null);

        if (reporter != null) {
            reporter.report(this.pathList.getDexPaths());
        }
    }
}
```

一个很明显的变化就是，optimizedDirectory 被弃用了，传给 DexPathList 的 optimizedDirectory 直接为空，不管外面传进来什么值。 也就是说，在 8.0 上，PathClassLoader 和 DexClassLoader 其实已经没有什么区别了。DexClassLoader 也不能指定 optimizedDirectory 了。

而在 DexFile_openDexFileNative 中，可以看到，javaOutputName 参数也已经被弃用了。

```text
static jobject DexFile_openDexFileNative(JNIEnv* env,
                                         jclass,
                                         jstring javaSourceName,
                                         jstring javaOutputName ATTRIBUTE_UNUSED,
                                         jint flags ATTRIBUTE_UNUSED,
                                         jobject class_loader,
                                        jobjectArray dex_elements) {
}
```

之后对 DEX 文件的处理链路如下：

```text
DexFile_openDexFileNative -> DexLocationToOdexNames -> OatFileManager::OpenDexFilesFromOat -> OatFileAssistant::OatFileAssistant -> OatFileAssistant::DexLocationToOdexFilename -> DexLocationToOdexNames
```

在 DexLocationToOdexNames 方法里，对 .odex 文件的路径做了处理。

```text
static bool DexLocationToOdexNames(const std::string& location,
                                   InstructionSet isa,
                                   std::string* odex_filename,
                                   std::string* oat_dir,
                                   std::string* isa_dir,
                                   std::string* error_msg) {
  CHECK(odex_filename != nullptr);
  CHECK(error_msg != nullptr);

  // The odex file name is formed by replacing the dex_location extension with
  // .odex and inserting an oat/<isa> directory. For example:
  //   location = /foo/bar/baz.jar
  //   odex_location = /foo/bar/oat/<isa>/baz.odex

  // Find the directory portion of the dex location and add the oat/<isa>
  // directory.
  size_t pos = location.rfind('/');
  if (pos == std::string::npos) {
    *error_msg = "Dex location " + location + " has no directory.";
    return false;
  }
  std::string dir = location.substr(0, pos+1);
  // Add the oat directory.
  dir += "oat";
  if (oat_dir != nullptr) {
    *oat_dir = dir;
  }
  // Add the isa directory
  dir += "/" + std::string(GetInstructionSetString(isa));
  if (isa_dir != nullptr) {
    *isa_dir = dir;
  }

  // Get the base part of the file without the extension.
  std::string file = location.substr(pos+1);
  pos = file.rfind('.');
  if (pos == std::string::npos) {
    *error_msg = "Dex location " + location + " has no extension.";
    return false;
  }
  std::string base = file.substr(0, pos);

  *odex_filename = dir + "/" + base + ".odex";
  return true;
}
```

看到上面的处理就是在 DEX 文件同级目录下添加一个 oat/ 文件作为 .odex 的存储目录。

### 总结



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmykw03xqyj311o0flgnc.jpg)