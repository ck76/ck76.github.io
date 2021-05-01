# [DexClassLoader和PathClassLoader](https://www.cnblogs.com/mingfeng002/p/7852643.html)

# Android的ClassLoader体系

# ![img](https://images2017.cnblogs.com/blog/363274/201711/363274-20171117193115999-53838087.png)

 在Android中可以跟java一样实现动态加载jar，但是Android使用Dalvik VM，不能直接加载java打包jar的byte code，需要通过dx工具来优化Dalvik byte code
 Android在API中给出可动态加载的有：DexClassLoader 和 PathClassLoader。

**DexClassLoader：**可加载jar、apk和dex，可以从SD卡中加载
**PathClassLoader：**只能加载**已安装到系统中**（即/data/app目录下）的apk文件

 

**为什么PathClassLoader只能加载apk的文件?**

从上图可以明显知道他们都继承**BaseDexClassLoader** 在看看他们源码有什么区别

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
// DexClassLoader.java
public class DexClassLoader extends BaseDexClassLoader {
    public DexClassLoader(String dexPath, String optimizedDirectory,
            String libraryPath, ClassLoader parent) {
        super(dexPath, new File(optimizedDirectory), libraryPath, parent);
    }
}

// PathClassLoader.java
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

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

主要的区别在于PathClassLoader的optimizedDirectory参数只能是null，那么optimizedDirectory是做什么用的呢？需要去父类看BaseDexClassLoader

```
public BaseDexClassLoader(String dexPath, File optimizedDirectory,
            String libraryPath, ClassLoader parent) {
        super(parent);
        this.originalPath = dexPath;
        this.pathList = new DexPathList(this, dexPath, libraryPath, optimizedDirectory);
    }
```

 

他们的父类构造方法中 是new 一个DexPathList实例把optimizedDirectory参数传入

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
 public DexPathList(ClassLoader definingContext, String dexPath,
            String libraryPath, File optimizedDirectory) {
        ……
        this.dexElements = makeDexElements(splitDexPath(dexPath), optimizedDirectory);
    }

    private static Element[] makeDexElements(ArrayList<File> files,
            File optimizedDirectory) {
        ArrayList<Element> elements = new ArrayList<Element>();
        for (File file : files) {
            ZipFile zip = null;
            DexFile dex = null;
            String name = file.getName();
            if (name.endsWith(DEX_SUFFIX)) {
                dex = loadDexFile(file, optimizedDirectory);
            } else if (name.endsWith(APK_SUFFIX) || name.endsWith(JAR_SUFFIX)
                    || name.endsWith(ZIP_SUFFIX)) {
                zip = new ZipFile(file);
            }
            ……
            if ((zip != null) || (dex != null)) {
                elements.add(new Element(file, zip, dex));
            }
        }
        return elements.toArray(new Element[elements.size()]);
    }

    private static DexFile loadDexFile(File file, File optimizedDirectory)
            throws IOException {
        if (optimizedDirectory == null) {
            return new DexFile(file);
        } else {
            String optimizedPath = optimizedPathFor(file, optimizedDirectory);
            return DexFile.loadDex(file.getPath(), optimizedPath, 0);
        }
    }

    /**
     * Converts a dex/jar file path and an output directory to an
     * output file path for an associated optimized dex file.
     */
    private static String optimizedPathFor(File path,
            File optimizedDirectory) {
        String fileName = path.getName();
        if (!fileName.endsWith(DEX_SUFFIX)) {
            int lastDot = fileName.lastIndexOf(".");
            if (lastDot < 0) {
                fileName += DEX_SUFFIX;
            } else {
                StringBuilder sb = new StringBuilder(lastDot + 4);
                sb.append(fileName, 0, lastDot);
                sb.append(DEX_SUFFIX);
                fileName = sb.toString();
            }
        }
        File result = new File(optimizedDirectory, fileName);
        return result.getPath();
    }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 optimizedDirectory是用来缓存我们需要加载的dex文件的，并创建一个DexFile对象，如果它为null，那么会直接使用dex文件原有的路径来创建DexFile 对象。

optimizedDirectory必须是一个内部存储路径，无论哪种动态加载，加载的可执行文件一定要存放在内部存储。DexClassLoader可以指定自己的optimizedDirectory，所以它可以加载外部的dex，因为这个dex会被复制到内部路径的optimizedDirectory；而PathClassLoader没有optimizedDirectory，所以它只能加载存在系统中已经安装过的apk里面的内部的dex

 **怎么\**缓存经过优化的classes\**（odex文件）?**

 使用Context.getDir(String, int)方法可以创建一个这样的目录，例如：

```
File dexOutputDir = context.getDir("dex", 0);
```

 

**DexClassLoader**

```
public DexClassLoader (String dexPath, String dexOutputDir, String libPath, ClassLoader parent)
```

dexPath：dex文件路径列表，多个路径使用”:”分隔 
dexOutputDir：经过优化的dex文件（odex）文件输出目录 
libPath：动态库路径（将被添加到app动态库搜索路径列表中） 
parent：这是一个ClassLoader，这个参数的主要作用是保留java中ClassLoader的委托机制（优先父类加载器加载classes，由上而下的加载机制，防止重复加载类字节码）

 

**PathClassLoader**

PathClassLoader提供两个常用构造方法

```
public PathClassLoader (String path, ClassLoader parent)

public PathClassLoader (String path, String libPath, ClassLoader parent)
```

path：文件或者目录的列表 
libPath：包含lib库的目录列表 
parent：父类加载器

PathClassLoader提供一个简单的ClassLoader实现，可以操作在本地文件系统的文件列表或目录中的classes，但不可以从网络中加载classes。

 

# DexClassloader

 

 编写接口：Dynamic

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
package com.smilegames.dynamic.interfaces;

public interface Dynamic {
    public String helloWorld();
    
    public String smileGames();
    
    public String fyt();
}
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 编写实现类：DynamicTest

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
package com.smilegames.dynamic.impl;

import com.smilegames.dynamic.interfaces.Dynamic;

public class DynamicTest implements Dynamic {

    @Override
    public String helloWorld() {
        return "Hello Word!";
    }

    @Override
    public String smileGames() {
        return "Smile Games";
    }

    @Override
    public String fyt() {
        return "fengyoutian";
    }

}
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

打包并编译成dex
    将接口打包成jar：dynamic.jar（只打包这Dynamic.java这一个接口）
    将实现类打包成jar：dynamic_test.jar（只打包DynamicTest.java这一个实现类）
    将打包后的实现类（dynamic_test.jar）编译成dex：dynamic_impl.jar
       1、将dynamic_test.jar拷贝到SDK安装目录android-sdk-windows\platform-tools下（ps：如果platform-tools没有dx.bat，可拷贝到build-tools目录下有dx.bat的子目录）
       2、执行以下命令：

```
 dx --dex --output=dynamic_impl.jar dynamic_test.jar
```

​       3、将dynamic.jar引入测试实例
​       4、将dynamic_impl.jar放到模拟器或真机的sdcard

修改onCreate例子
    

 

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

//        或许activity按钮
        helloWorld = (Button) findViewById(R.id.helloWorld);
        smileGames = (Button) findViewById(R.id.smileGames);
        fyt = (Button) findViewById(R.id.fyt);

        /*使用DexCkassLoader方式加载类*/
        // dex压缩文件的路径（可以是apk,jar,zip格式）
        String dexPath = Environment.getExternalStorageDirectory().toString() + File.separator + "dynamic_impl.jar";

        // dex解压释放后的目录
        //String dexOutputDirs = Environment.getExternalStorageDirectory().toString();
        File dexOutputDir = context.getDir("dex", 0);
        DexClassLoader dexClassLoader = new DexClassLoader(dexPath,dexOutputDir.getAbsolutePath(),null,getClassLoader());
        //主意这里目录必须是应用内的目录
        //如果是应用外的目录例如String dexOutputDirs = Environment.getExternalStorageDirectory().toString()+"XXX";
        //会报：java.lang.IllegalArgumentException: Optimized data directory /storage/sdcard0 is not owned by the current user. Shared storage cannot protect your application from code injection attacks
        //原因之前都已经讲过

        // 定义DexClassLoader
        // 第一个参数：是dex压缩文件的路径
        // 第二个参数：是dex解压缩后存放的目录
        // 第三个参数：是C/C++依赖的本地库文件目录,可以为null
        // 第四个参数：是上一级的类加载器
        DexClassLoader dexClassLoader = new DexClassLoader(dexPath,dexOutputDirs,null,getClassLoader());

        Class libProvierClazz = null;
        // 使用DexClassLoader加载类
        try {
            libProvierClazz = dexClassLoader.loadClass("com.smilegames.dynamic.impl.DynamicTest");
            // 创建dynamic实例
            dynamic = (Dynamic) libProvierClazz.newInstance();
        } catch (Exception e) {
            e.printStackTrace();
        }

        helloWorld.setOnClickListener(new HelloWorldOnClickListener());
        smileGames.setOnClickListener(new SmileGamesOnClickListener());
        fyt.setOnClickListener(new FytOnClickListener());
    }

   private final class HelloWorldOnClickListener implements View.OnClickListener {
        @Override
        public void onClick(View v) {
            if (null != dynamic) {
                Toast.makeText(getApplicationContext(), dynamic.helloWorld(), 1500).show();
            } else {
                Toast.makeText(getApplicationContext(), "类加载失败", 1500).show();
            }
        }
    }

    private final class SmileGamesOnClickListener implements View.OnClickListener {
        @Override
        public void onClick(View v) {
            if (null != dynamic) {
                Toast.makeText(getApplicationContext(), dynamic.smileGames(), 1500).show();
            } else {
                Toast.makeText(getApplicationContext(), "类加载失败", 1500).show();
            }
        }
    }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)