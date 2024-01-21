[TOC]

## BuildConfig是什么？

BuildConfig是android在编译过程中自动生成的一个配置文件。

在不同的编译模式下会生成不同的变量，我们可以利用这些变量来方便不同编译环境下的开发，比如日志的打印（开发环境下可以打印Verbose一级，发布环境下可以打印Warn一级）。

## BuildConfig有哪些变量？

没有自己变动过gradle文件的话，自动生成的BuildConfig一般如下文所示。

```java
public final class BuildConfig {
  public static final boolean DEBUG = Boolean.parseBoolean("true");
  public static final String APPLICATION_ID = "com.xxxx.xxx.xx";
  public static final String BUILD_TYPE = "debug";
  public static final String FLAVOR = "";
  public static final int VERSION_CODE = 1;
  public static final String VERSION_NAME = "1.0";
}
```

## 如何定义自己的BuildConfig

```groovy
buildTypes {

        release {
            //是否混淆
            //混淆文件在proguardFiles后面配置,默认为app目录下的 “proguard-rules.pro”
            minifyEnabled true
            //内存对齐
            zipAlignEnabled true
            // 移除无用的resource文件
            shrinkResources true
            // 服务器配置
            buildConfigField("boolean", "LOG_DEBUG", "false")
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
            //签名
            signingConfig signingConfigs.release
        }

        debug {
            minifyEnabled false
            zipAlignEnabled false
            shrinkResources false
            //在BuildConfig里面生成一个布尔型变量可以直接引用
            buildConfigField("boolean", "LOG_DEBUG", "true")
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
            signingConfig signingConfigs.debug

        }
    }
```

**buildConfigField("boolean", "LOG_DEBUG", "false")**

**buildConfigField("boolean", "LOG_DEBUG", "true")**

这两行之后会在BuildConfig文件里生成

```java
  // Fields from build type: debug
  public static final boolean LOG_DEBUG = true;
```

然后可以在Debug的时候使用

```java
/**
 * Created by nicolite on 17-6-24.
 * 用来控制Log输出，、、
 */

public class LogUtils {

    private static final int VERBOSE = 1;
    private static  final int DEBUG = 2;
    private static final int INFO = 3;
    private static final int WARN = 4;
    private static final int  ERROR = 5;
    private static final int NOTHING = 6;
    private static final int LEVEL = VERBOSE; //将LEVEL设置为NOTHING就不会输出任何日志

    public static void v(String tag, String msg) {
        if (BuildConfig.LOG_DEBUG && LEVEL <= VERBOSE) {
            Log.v(tag, msg);
        }
    }

    public static void d(String tag, String msg) {
        if (BuildConfig.LOG_DEBUG && LEVEL <= DEBUG) {
            Log.d(tag, msg);
        }
    }

    public static void i(String tag, String msg) {
        if (BuildConfig.LOG_DEBUG && LEVEL <= INFO) {
            Log.i(tag, msg);
        }
    }

    public static void w(String tag, String msg) {
        if (BuildConfig.LOG_DEBUG && LEVEL <= WARN) {
            Log.w(tag, msg);
        }
    }

    public static void e(String tag, String msg) {
        if (BuildConfig.LOG_DEBUG && LEVEL <= ERROR) {
            Log.e(tag, msg);
        }
    }

    public static void v(String tag, String msg, Throwable throwable) {
        if (BuildConfig.LOG_DEBUG && LEVEL <= VERBOSE) {
            Log.v(tag, msg, throwable);
        }
    }

    public static void d(String tag, String msg, Throwable throwable) {
        if (BuildConfig.LOG_DEBUG && LEVEL <= DEBUG) {
            Log.d(tag, msg, throwable);
        }
    }

    public static void i(String tag, String msg, Throwable throwable) {
        if (BuildConfig.LOG_DEBUG && LEVEL <= INFO) {
            Log.i(tag, msg, throwable);
        }
    }

    public static void w(String tag, String msg, Throwable throwable) {
        if (BuildConfig.LOG_DEBUG && LEVEL <= WARN) {
            Log.w(tag, msg, throwable);
        }
    }

    public static void e(String tag, String msg, Throwable throwable) {
        if (BuildConfig.LOG_DEBUG && LEVEL <= ERROR) {
            Log.e(tag, msg, throwable);
        }
    }
}

```

