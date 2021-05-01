[TOC]

## 启用混淆

通过工程下的`build.gradle`文件中的开启混淆开关和配置混淆规则文件

- minifyEnabled：混淆开关
- proguard-android.txt：SDK中默认proguard的配置规则
- proguard-rules.pro：自定义proguard的配置规则

```groovy
buildTypes {
    debug {
        minifyEnabled true
        proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
    }
}
```

## 工作流程

![这里写图片描述](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/nF9Uof0cilzMK70A4XpFd3AZNdOb1FFChZabpOawVcI!/r/dDUBAAAAAAAA)

Proguard的工作流程由Shrink、Optimize、Obfuscate、Preverify四个步骤组成，每个步骤都是可选的，我们可以通过配置脚本决定执行其中的哪几个步骤。这里引入一个EntryPoint概念，EntryPoint是在ProGuard过程中保存不会被处理的类或方法。在压缩过程中，Proguard从EntryPoint出发，递归检索，删除那些没有使用到的类和类的成员。在优化过程中，那些非EntryPoint的类和方法会被设置成private，static或final，没有使用到的参数会被移除，有些方法可能会被标记为内联的。在混淆过程中，会对非EntryPoint的类和类的成员进行重命名，也就是用其它无意义的名称代替。我们在配置文件中用-keep保留的部分属于EntryPoint，所以不会被重命名

**1、压缩**

压缩会移除未被使用的类和成员，并且会在优化动作执行之后再次执行

```shell
# 关闭压缩，默认打开
-dontshrink 
```

**2、优化**

优化会在字节码级别上做优化，让应用运行的更快

```shell
# 关闭优化，默认打开
-dontoptimize
# 表示对代码优化的次数，一般为5
-optimizationpasses n
# 指定更精细级别的优化
-optimizations !code/simplification/arithmetic,!field/*,!class/merging/*
```

**3、混淆**

混淆会将简短的无意义的名称，对类，字段和方法进行重命名

```shell
# 关闭混淆，默认打开
-dontobfuscate
```

**4、预验证**

预验证将对Java class进行预验证，Android中没有预验证过程

```shell
# 关闭预验证，默认关闭
-dontpreverify
```

## 混淆基础

**1、类名**

对类名进行keep操作只是将类名keep住，但方法和变量仍然会被混淆

```shell
# 一颗星表示keep当前本包下的类名，子包下的类名是会被混淆的
-keep class com.example.hensen.*
# 两颗星表示keep当前本包下的类名和子包下的类名
-keep class com.example.hensen.**
# 表示keep当前类名
-keep class com.example.hensen.net.NetWorkCache
# 表示keep当前类的内部类的类名
-keep class com.example.hensen.net.NetWorkCache$NetWorkBean
```

**2、内容**

对内容进行keep操作不仅可以将类名keep住，还可以对方法和变量keep住

```shell
# 一颗星表示keep当前本包下的类名、类的内容
-keep class com.example.hensen.*{*;}
# 两颗星表示keep当前本包下的类名、类的内容和子包下的类名、类的内容
-keep class com.example.hensen.**{*;}
# 表示keep当前类名、类的内容
-keep class com.example.hensen.net.NetWorkCache{*;}
# 表示keep当前类的内部类的类名、内部类的内容
-keep class com.example.hensen.net.NetWorkCache$NetWorkBean{*;}
```

**3、特定内容**

对特定的内容进行keep操作

```shell
-keep class com.example.hensen.net.NetWorkCache{
    <init>;# 匹配所有构造器
    <fields>;# 匹配所有变量
    <methods>;# 匹配所有方法

    public <methods>;# 匹配所有共有的方法
    private <methods>;# 匹配所有私有的方法
    public *;# 匹配所有共有的内容
    private *;# 匹配所有私有的内容
    public <init>(java.lang.String);# 匹配特定参数的构造函数
    public void getCache(...);# 匹配任意长度类型参数的方法
}123456789101112
```

**4、类成员**

对类名不需要keep，只需要对类下的方法进行keep操作

```shell
# 表示keep特定类下的特定参数的方法，但类名不会被keep
-keepclassmembernames class com.example.hensen.net.NetWorkCache{
    public void getCache(java.lang.String);
}
```

**5、类和类成员**

| 作用范围                     | keep所指定类、成员      | keep所指定类、成员(前提是在压缩阶段没有被删除) |
| ---------------------------- | ----------------------- | ---------------------------------------------- |
| 类和类成员                   | -keep                   | -keepnames                                     |
| 仅类成员                     | -keepclassmembers       | -keepclassmembernames                          |
| 类和类成员(前提是成员都存在) | -keepclasseswithmembers | -keepclasseswithmembernames                    |

## 应用场景

**1、安卓底层组件和类名不可混淆**

将底层的keep住，插件化才能准确的hook到底层组件

```shell
-keep public class * extends android.app.Activity
-keep public class * extends android.app.Application
-keep public class * extends android.app.Service
-keep public class * extends android.content.ContentProvider
-keep public class * extends android.content.BroadcastReceiver
-keep public class * extends android.view.View
-keep public class * extends android.preference.Preference1234567
```

**2、jni方法不可混淆**

native方法要完整的包名类名方法来定义，不可修改，否则找不到

```shell
-keepclasswithmembernames class *{
    native <methods>;
}
```

**3、反射用到的类名和方法不可混淆**

反射要根据类名才能拿到反射的实体

```shell
-keep public class com.example.hensen.** {
    public void set*(***);
    public *** get*();
    public *** is*();
}
```

**4、自定义View不可混淆**

只要是继承自系统组件，都要keep住

```groovy
-keep public class * extend android.view.View{
    *** get*();
    void set*(***);
    public <init>(android.content.Context);
    public <init>(android.content.Context, android.util.AttributeSet);
    public <init>(android.content.Context, android.util.AttributeSet, int);
}
```

**5、第三方框架不可混淆**

将第三方框架当作为系统组件即可

```shell
-keep class android.support.** { *; }
-keep class android.support.v4.** { *; }
-keep class android.support.v7.** { *; }
-keep class * extends android.support.v4.**
-keep class * extends android.support.v7.**
-keep class * extends android.support.annotation.**
```

**6、WebView和Js互调接口不可混淆**

```groovy
-keepclassmembers class ** {
    @android.webkit.JavascriptInterface public *;
}
```

**7、序列化的类不可混淆**

```groovy
-keepclassmembers class * implements android.os.Parcelable {
    static ** CREATOR;
    <fields>;
    <methods>;
}

-keepclassmembers class * implements java.io.Serializable {
    static final long serialVersionUID;
    private static final java.io.ObjectStreamField[] serialPersistentFields;
    private void writeObject(java.io.ObjectOutputStream);
    private void readObject(java.io.ObjectInputStream);
    java.lang.Object writeReplace();
    java.lang.Object readResolve();
}
```

**8、enum类的特殊性**

以下方法会被发射调用

```groovy
-keepclassmembers enum * {
    public static **[] values();
    public static ** valueOf(java.lang.String);
    public static ** valueOf(int);
}
```

**9、其他场景**

```shell
# 指定文件为映射文件，包括类和类成员的混淆名称，文件未提及的类和类成员将生成新的名称
-applymapping mapping.txt
# 指定一个文本文件，其中所有有效字词都用作混淆字段和方法名称
-obfuscationdictionary obfuscationdictionary.txt
# 指定一个文本文件，其中所有有效词都用作混淆类名
-classobfuscationdictionary obfuscationdictionary.txt

# 混淆时不生成大小写混合的类名
-dontusemixedcaseclassnames
# 不忽略指定jars中的非public calsses
-dontskipnonpubliclibraryclasses
# 不忽略指定类库的public类成员（变量和方法）
-dontskipnonpubliclibraryclassmembers

# 混淆过程中打印详细信息，如果异常终止则打印整个堆栈信息
-verbose
# 忽略警告继续处理
-ignorewarnings

# 不对指定的类、包中的不完整的引用发出警告
-dontwarn android.support.v4.**
-dontwarn All

# 避免混淆内部类、泛型、匿名类
-keepattributes InnerClasses,Signature,EnclosingMethod
# 抛出异常时保留代码行号    
-keepattributes SourceFile,LineNumberTable
# 保留注释成员变量，如Activity被@Override注释的方法onCreate、onDestroy方法
-keepattributes *Annotation*
# 资源类变量需要保留
-keep public class **.R$*{
   public static final int *;
}
```

## 语法

```shell
-libraryjars class_path 应用的依赖包，如android-support-v4  
-keep [,modifier,...] class_specification 不混淆某些类  
-keepclassmembers [,modifier,...] class_specification 不混淆类的成员  
-keepclasseswithmembers [,modifier,...] class_specification 不混淆类及其成员  
-keepnames class_specification 不混淆类及其成员名  
-keepclassmembernames class_specification 不混淆类的成员名  
-keepclasseswithmembernames class_specification 不混淆类及其成员名  
-assumenosideeffects class_specification 假设调用不产生任何影响，在proguard代码优化时会将该调用remove掉。如system.out.println和Log.v等等  
-dontwarn [class_filter] 不提示warnning  
```



## 模板

```shell
#-------------------------------------------定制化区域----------------------------------------------
#---------------------------------1.实体类---------------------------------



#-------------------------------------------------------------------------

#---------------------------------2.第三方包-------------------------------



#-------------------------------------------------------------------------

#---------------------------------3.与js互相调用的类------------------------



#-------------------------------------------------------------------------

#---------------------------------4.反射相关的类和方法-----------------------



#----------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

#-------------------------------------------基本不用动区域--------------------------------------------
#---------------------------------基本指令区----------------------------------
-optimizationpasses 5
-dontusemixedcaseclassnames
-dontskipnonpubliclibraryclasses
-dontskipnonpubliclibraryclassmembers
-dontpreverify
-verbose
-printmapping proguardMapping.txt
-optimizations !code/simplification/cast,!field/*,!class/merging/*
-keepattributes *Annotation*,InnerClasses
-keepattributes Signature
-keepattributes SourceFile,LineNumberTable
#----------------------------------------------------------------------------

#---------------------------------默认保留区---------------------------------
-keep public class * extends android.app.Activity
-keep public class * extends android.app.Application
-keep public class * extends android.app.Service
-keep public class * extends android.content.BroadcastReceiver
-keep public class * extends android.content.ContentProvider
-keep public class * extends android.app.backup.BackupAgentHelper
-keep public class * extends android.preference.Preference
-keep public class * extends android.view.View
-keep public class com.android.vending.licensing.ILicensingService
-keep class android.support.** {*;}

-keepclasseswithmembernames class * {
    native <methods>;
}
-keepclassmembers class * extends android.app.Activity{
    public void *(android.view.View);
}
-keepclassmembers enum * {
    public static **[] values();
    public static ** valueOf(java.lang.String);
}
-keep public class * extends android.view.View{
    *** get*();
    void set*(***);
    public <init>(android.content.Context);
    public <init>(android.content.Context, android.util.AttributeSet);
    public <init>(android.content.Context, android.util.AttributeSet, int);
}
-keepclasseswithmembers class * {
    public <init>(android.content.Context, android.util.AttributeSet);
    public <init>(android.content.Context, android.util.AttributeSet, int);
}
-keep class * implements android.os.Parcelable {
  public static final android.os.Parcelable$Creator *;
}
-keepclassmembers class * implements java.io.Serializable {
    static final long serialVersionUID;
    private static final java.io.ObjectStreamField[] serialPersistentFields;
    private void writeObject(java.io.ObjectOutputStream);
    private void readObject(java.io.ObjectInputStream);
    java.lang.Object writeReplace();
    java.lang.Object readResolve();
}
-keep class **.R$* {
 *;
}
-keepclassmembers class * {
    void *(**On*Event);
}
#----------------------------------------------------------------------------

#---------------------------------webview------------------------------------
-keepclassmembers class fqcn.of.javascript.interface.for.Webview {
   public *;
}
-keepclassmembers class * extends android.webkit.WebViewClient {
    public void *(android.webkit.WebView, java.lang.String, android.graphics.Bitmap);
    public boolean *(android.webkit.WebView, java.lang.String);
}
-keepclassmembers class * extends android.webkit.WebViewClient {
    public void *(android.webkit.WebView, jav.lang.String);
}
#----------------------------------------------------------------------------
#------
```



## DqHelper混淆

```shell
# Add project specific ProGuard rules here.
# By default, the flags in this file are appended to flags specified
# in /home/nicolite/Android/Sdk/tools/proguard/proguard-android.txt
# You can edit the include path and order by changing the proguardFiles
# directive in build.gradle.
#
# For more details, see
#   http://developer.android.com/guide/developing/tools/proguard.html

# Add any project specific keep options here:

# If your project uses WebView with JS, uncomment the following
# and specify the fully qualified class name to the JavaScript interface
# class:
#-keepclassmembers class fqcn.of.javascript.interface.for.webview {
#   public *;
#}

# Uncomment this to preserve the line number information for
# debugging stack traces.
#-keepattributes SourceFile,LineNumberTable

# If you keep the line number information, uncomment this to
# hide the original source file name.
#-renamesourcefileattribute SourceFile

#Luban 开始
-keep class top.zibin.luban.** {*;}
#Luban 结束

#PhotoView 开始
-keep class com.github.chrisbanes.photoview.** {*;}
#PhotoView 结束

#leakcanary 开始
-dontwarn com.squareup.haha.guava.**
-dontwarn com.squareup.haha.perflib.**
-dontwarn com.squareup.haha.trove.**
-dontwarn com.squareup.leakcanary.**
-keep class com.squareup.haha.** { *; }
-keep class com.squareup.leakcanary.** { *; }

# Marshmallow removed Notification.setLatestEventInfo()
-dontwarn android.app.Notification
#leakcanary 结束

#AndPermission 开始
-keepclassmembers class ** {
    @com.yanzhenjie.permission.PermissionYes <methods>;
}
-keepclassmembers class ** {
    @com.yanzhenjie.permission.PermissionNo <methods>;
}
#AndPermission 结束

#greenDao 开始
-keepclassmembers class * extends org.greenrobot.greendao.AbstractDao {
 public static java.lang.String TABLENAME;
 public static void dropTable(org.greenrobot.greendao.database.Database, boolean);
 public static void createTable(org.greenrobot.greendao.database.Database, boolean);
}
-keep class **$Properties
-keepclassmembers class **$Properties {*;}

# If you do not use SQLCipher:
-dontwarn org.greenrobot.greendao.database.**
# If you do not use Rx:
-dontwarn rx.**
#greenDao 结束

#Rxlifecycle 开始
-keep class com.trello.rxlifecycle2.**{*;}
#Rxlifecycle 结束

#butterknife 开始
-keep class butterknife.** { *; }
-dontwarn butterknife.internal.**
-keep class **$$ViewBinder { *; }

-keepclasseswithmembernames class * {
    @butterknife.* <fields>;
}
-keepclasseswithmembernames class * {
    @butterknife.* <methods>;
}
#butterknife 结束

# OkHttp3 开始
-dontwarn com.squareup.okhttp3.**
-keep class com.squareup.okhttp3.** { *;}
-dontwarn okio.**
# OkHttp3 结束

# Okio 开始
-dontwarn com.squareup.**
-dontwarn okio.**
-keep public class org.codehaus.* { *; }
-keep public class java.nio.* { *; }
# Okio 结束

#retrofit 开始
# Platform calls Class.forName on types which do not exist on Android to determine platform.
-dontnote retrofit2.Platform
# Platform used when running on RoboVM on iOS. Will not be used at runtime.
-dontnote retrofit2.Platform$IOS$MainThreadExecutor
# Platform used when running on Java 8 VMs. Will not be used at runtime.
-dontwarn retrofit2.Platform$Java8

-dontwarn retrofit2.adapter.rxjava.CompletableHelper$CompletableCallAdapter
-dontwarn retrofit2.adapter.rxjava.CompletableHelper$CompletableCallOnSubscribe
-dontwarn retrofit2.adapter.rxjava.CompletableHelper$CompletableCallOnSubscribe$1

# Retain generic type information for use by reflection by converters and adapters.
-keepattributes Signature
# Retain declared checked exceptions for use by a Proxy instance.
-keepattributes Exceptions
#retrofit 结束

## ----------------------------------
##   ########## Gson混淆    ##########
## ----------------------------------
##---------------Begin: proguard configuration for Gson  ----------
# Gson uses generic type information stored in a class file when working with fields. Proguard
# removes such information by default, so configure it to keep all of it.
-keepattributes Signature

# For using GSON @Expose annotation
-keepattributes *Annotation*

# Gson specific classes
-keep class sun.misc.Unsafe { *; }
#-keep class com.google.gson.stream.** { *; }

# Application classes that will be serialized/deserialized over Gson
-keep class com.google.gson.examples.android.model.** { *; }

# Prevent proguard from stripping interface information from TypeAdapterFactory,
# JsonSerializer, JsonDeserializer instances (so they can be used in @JsonAdapter)
-keep class * implements com.google.gson.TypeAdapterFactory
-keep class * implements com.google.gson.JsonSerializer
-keep class * implements com.google.gson.JsonDeserializer

# RxJava RxAndroid 开始
-dontwarn sun.misc.**
-keepclassmembers class rx.internal.util.unsafe.*ArrayQueue*Field* {
    long producerIndex;
    long consumerIndex;
}
-keepclassmembers class rx.internal.util.unsafe.BaseLinkedQueueProducerNodeRef {
    rx.internal.util.atomic.LinkedQueueNode producerNode;
}
-keepclassmembers class rx.internal.util.unsafe.BaseLinkedQueueConsumerNodeRef {
    rx.internal.util.atomic.LinkedQueueNode consumerNode;
}
-dontwarn okio.**
# RxJava RxAndroid 结束


# LRecyclerview 开始
-dontwarn com.github.jdsjlzx.**
-keep class com.github.jdsjlzx.progressindicator.indicators.** { *; }
# LRecyclerview 结束

# Java Bean 开始
-keep class cn.nicolite.huthelper.model.bean.**{*;}
# Java Bean 结束

#Slidr 开始
-dontwarn com.r0adkll.slidr
-keep class com.r0adkll.slidr.** {*;}
#Slidr 结束

#Glide 开始
-keep public class * implements com.bumptech.glide.module.GlideModule
-keep public class * extends com.bumptech.glide.AppGlideModule
-keep public enum com.bumptech.glide.load.resource.bitmap.ImageHeaderParser$** {
  **[] $VALUES;
  public *;
}

# for DexGuard only
#-keepresourcexmlelements manifest/application/meta-data@value=GlideModule
#Glide 结束

#信鸽推送 开始
-keep public class * extends android.app.Service
-keep public class * extends android.content.BroadcastReceiver
-keep class com.tencent.android.tpush.** {* ;}
-keep class com.tencent.mid.** {* ;}
-keep class com.qq.taf.jce.** {*;}
#信鸽推送 结束

#腾讯mta 开始
-keep class com.tencent.stat.* {*;}
-keep class com.tencent.mid.* {*;}
#腾讯mta 结束

#腾讯bugly 开始
-dontwarn com.tencent.bugly.**
-keep public class com.tencent.bugly.**{*;}
-keep class android.support.**{*;}
#腾讯bugly 结束

#Jsoup 开始
-keep class org.jsoup.nodes.** {*;}
-keep class org.jsoup.nodes.** {*;}
-keep class org.jsoup.select.** {*;}
#Jsoup 结束

#指定压缩级别
-optimizationpasses 5

#不跳过非公共的库的类成员
-dontskipnonpubliclibraryclassmembers

#混淆时采用的算法
-optimizations !code/simplification/arithmetic,!field/*,!class/merging/*

#把混淆类中的方法名也混淆了
-useuniqueclassmembernames

#优化时允许访问并修改有修饰符的类和类的成员
-allowaccessmodification

#将文件来源重命名为“SourceFile”字符串
-renamesourcefileattribute SourceFile
#保留行号
-keepattributes SourceFile,LineNumberTable

#保持所有实现 Serializable 接口的类成员
-keepclassmembers class * implements java.io.Serializable {
    static final long serialVersionUID;
    private static final java.io.ObjectStreamField[] serialPersistentFields;
    private void writeObject(java.io.ObjectOutputStream);
    private void readObject(java.io.ObjectInputStream);
    java.lang.Object writeReplace();
    java.lang.Object readResolve();
}

#Fragment不需要在AndroidManifest.xml中注册，需要额外保护下
-keep public class * extends android.support.v4.app.Fragment
-keep public class * extends android.app.Fragment

# 保持测试相关的代码
-dontnote junit.framework.**
-dontnote junit.runner.**
-dontwarn android.test.**
-dontwarn android.support.test.**
-dontwarn org.junit.**

-keep public class * extends android.app.Activity
-keep public class * extends android.app.Application
-keep public class * extends android.app.Service
-keep public class * extends android.content.BroadcastReceiver
-keep public class * extends android.content.ContentProvider
-keep public class * extends android.app.backup.BackupAgentHelper
-keep public class * extends android.preference.Preference
-keep public class com.android.vending.licensing.ILicensingService
-keep public class * extends android.preference.Preference
-keep public class * extends android.view.View
-keep public class * extends android.database.sqlite.SQLiteOpenHelper{*;}

#保持 native 方法不被混淆
-keepclasseswithmembernames class * {
    native <methods>;
}

#保持自定义控件类不被混淆
-keepclasseswithmembers class * {
    public <init>(android.content.Context, android.util.AttributeSet);
}

#保持自定义控件类不被混淆
-keepclassmembers class * extends android.app.Activity {
   public void *(android.view.View);
}

-keep public class * extends android.view.View {
    public <init>(android.content.Context);
    public <init>(android.content.Context, android.util.AttributeSet);
    public <init>(android.content.Context, android.util.AttributeSet, int);
    public void set*(...);
}

#保持 Parcelable 不被混淆
-keep class * implements android.os.Parcelable {
  public static final android.os.Parcelable$Creator *;
}


#保持枚举 enum 类不被混淆
-keepclassmembers enum * {
  public static **[] values();
  public static ** valueOf(java.lang.String);
}

-keepclassmembers class * {
    public void *ButtonClicked(android.view.View);
}

#不混淆资源类
-keepclassmembers class **.R$* {
    public static <fields>;
}

# 对于带有回调函数的onXXEvent、**On*Listener的，不能被混淆
-keepclassmembers class * {
    void *(**On*Event);
    void *(**On*Listener);
}

# webView处理，项目中没有使用到webView忽略即可
-keepclassmembers class fqcn.of.javascript.interface.for.webview {
    public *;
}
-keepclassmembers class * extends android.webkit.webViewClient {
    public void *(android.webkit.WebView, java.lang.String, android.graphics.Bitmap);
    public boolean *(android.webkit.WebView, java.lang.String);
}
-keepclassmembers class * extends android.webkit.webViewClient {
    public void *(android.webkit.webView, jav.lang.String);
}

-keepattributes EnclosingMethod

#------------------  下方是android平台自带的排除项，这里不要动         ----------------

-keep public class * extends android.app.Activity{
    public <fields>;
    public <methods>;
}
-keep public class * extends android.app.Application{
    public <fields>;
    public <methods>;
}
-keep public class * extends android.app.Service
-keep public class * extends android.content.BroadcastReceiver
-keep public class * extends android.content.ContentProvider
-keep public class * extends android.app.backup.BackupAgentHelper
-keep public class * extends android.preference.Preference

-keepclassmembers enum * {
    public static **[] values();
    public static ** valueOf(java.lang.String);
}

-keepclasseswithmembers class * {
    public <init>(android.content.Context, android.util.AttributeSet);
}

-keepclasseswithmembers class * {
    public <init>(android.content.Context, android.util.AttributeSet, int);
}

-keepattributes *Annotation*

-keepclasseswithmembernames class *{
    native <methods>;
}

-keep class * implements android.os.Parcelable {
  public static final android.os.Parcelable$Creator *;
}

#------------------  下方是共性的排除项目         ----------------
# 方法名中含有“JNI”字符的，认定是Java Native Interface方法，自动排除
# 方法名中含有“JRI”字符的，认定是Java Reflection Interface方法，自动排除

-keepclasseswithmembers class * {
    ... *JNI*(...);
}

-keepclasseswithmembernames class * {
    ... *JRI*(...);
}

-keep class **JNI* {*;}

-keep class android.support.v8.renderscript.** { *; }

```



## 链接

https://www.jianshu.com/p/f3455ecaa56e

https://blog.csdn.net/u013100574/article/details/53304411