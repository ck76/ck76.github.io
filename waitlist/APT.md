[TOC]



### APT

APT(Annotation Processing Tool)即注解处理器，是一种处理注解的工具，确切的说它是javac的一个工具，它用来在编译时扫描和处理注解。注解处理器以Java代码(或者编译过的字节码)作为输入，生成.java文件作为输出。简单来说就是在编译期，通过注解生成.java文件。

　　**android-apt和annotationProcessor功能是一样的，都是apt的实现，前者比较早，后者是google官方开发的内置在gradle里的apt**

APT的处理要素
　　注解处理器（AbstractProcess）+ 代码处理（javaPoet）+ 处理器注册（AutoService）+ apt

使用APT来处理annotation的流程
　　1.  定义注解（如@automain） 
　　2.  定义注解处理器，自定义需要生成代码 
　　3.  使用处理器 
　　4.  APT自动完成生成新的java文件。



#### 反射

这是一个多数框架都会使用的技术，只要知道类或对象，我们可以解析出关于此类的一切信息，为所欲为。一提反射，很多人会觉得性能消耗不会很大吗？肯定你也带着同样的想法吧？那么我来声明一下，反射存在性能消耗是有的，但是没有想象中的夸张；如果结合缓存使用的话，可以极大的抵消带来的性能的消耗，从而更好的享受它所带来的便利。



#### Javapoet

这是一个很神奇的技术，借助它，我们可以便捷的生成我们想要的代码。至少目前来看，GreenDao生成的Bean与Dao, ButterKnife生成的Binding以及ARouter生成的Router&&Group&&xx等，都是使用了Poet技术。



### android-apt

android-apt是由一位开发者自己开发的apt框架，随着Android Gradle 插件 2.2 版本的发布，Android Gradle 插件**提供了名为 annotationProcessor 的功能来完全代替 android-apt** ，自此android-apt 作者在官网发表声明最新的Android Gradle插件现在已经支持annotationProcessor，并警告和或阻止android-apt ，并推荐大家使用 Android 官方插件annotationProcessor。

但是很多项目目前还是使用android-apt，如果想替换为annotationProcessor，那就要知道android-apt是如何使用的。

```java
//添加android-apt到Project下的build.gradle中
dependencies {
        //替换成最新的 gradle版本
        classpath 'com.android.tools.build:gradle:1.3.0'
        //替换成最新android-apt版本
        classpath 'com.neenbedankt.gradle.plugins:android-apt:1.8'
    }

//在Module中build.gradle的配置
apply plugin: 'com.neenbedankt.android-apt'
dependencies {
 apt 'com.squareup.dagger:dagger-compiler:1.1.0'
 compile 'com.squareup.dagger:dagger:1.1.0'
}
```



### annotationProcessor

只在**编译的时候**执行依赖的库，但是库最终不打包到apk中，

编译库中的代码没有直接使用的意义，也没有提供开放的api调用，**最终的目的是得到编译库中生成的文件**，供我们调用。

![annotationProcessor处理过程](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/d5.nZD0POZu0k6gI0JLspOlRUTERF1vMX0GtjexeWy4!/r/dL4AAAAAAAAA)



#### 注解处理器的声明

每一个注解处理器都需要承`AbstractProcessor`类，具体代码如下所示：

```java
class MineProcessor extends AbstractProcessor {

    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {}
    
    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        return false;
    }

    @Override
    public SourceVersion getSupportedSourceVersion() { }

    @Override
    public Set<String> getSupportedAnnotationTypes() { }
}
```

- `init(ProcessingEnvironment processingEnv)`：每个注解处理器被初始化的时候都会被调用，该方法会被传入ProcessingEnvironment 参数。ProcessingEnvironment 能提供很多有用的工具类，Elements、Types和Filer。后面我们将会看到详细的内容。
-  `process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv)`：注解处理器实际处理方法，一般要求子类实现该抽象方法，你可以在在这里写你的扫描与处理注解的代码，以及生成Java文件。其中参数RoundEnvironment ，可以让你查询出包含特定注解的被注解元素，后面我们会看到详细的内容。
-  `getSupportedAnnotationTypes()`: 返回当前注解处理器处理注解的类型，返回值为一个字符串的集合。其中字符串为处理器需要处理的注解的`合法全称`。
-  `getSupportedSourceVersion()`:用来指定你使用的Java版本，通常这里返回`SourceVersion.latestSupported()`。如果你有足够的理由指定某个Java版本的话，你可以返回SourceVersion.RELAEASE_XX。但是还是推荐使用前者。

在Java1.6版本中提供了`SupportedAnnotationTypes`与`SupportedSourceVersion`两个注解来替代`getSupportedSourceVersion`与`getSupportedAnnotationTypes`两个方法，也就是这样：

```java
@SupportedSourceVersion(SourceVersion.RELEASE_6)
@SupportedAnnotationTypes({"合法注解的名称"})
class MineProcessor extends AbstractProcessor {

    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        return false;
    }
}
```



#### 注册注解处理器

在使用注解处理器需要先声明，步骤：

- 需要在 processors 库的 main 目录下新建 resources 资源文件夹；
- 在 resources文件夹下建立 META-INF/services 目录文件夹；
- 在 META-INF/services 目录文件夹下创建 javax.annotation.processing.Processor 文件；
- 在 javax.annotation.processing.Processor 文件写入注解处理器的全称，包括包路径；

这样声明下来也太麻烦了？这就是用引入auto-service的原因。

通过auto-service中的@AutoService可以自动生成AutoService注解处理器是Google开发的，用来生成 META-INF/services/javax.annotation.processing.Processor 文件的



#### 注解处理器的扫描

在注解处理过程中，我们需要扫描所有的Java源文件，源代码的每一个部分都是一个特定类型的`Element`，也就是说Element代表源文件中的元素，例如包、类、字段、方法等。整体的关系如下图所示：

![elem继承关系](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Xx2X.mEiKkED8hrOqt7jdmjiAMpnmlm.inoU6wIUHVM!/r/dL8AAAAAAAAA)

- Parameterizable：表示混合类型的元素（不仅只有一种类型的Element)
- TypeParameterElement：带有泛型参数的类、接口、方法或者构造器。
- VariableElement：表示字段、常量、方法或构造函数。参数、局部变量、资源变量或异常参数。
- QualifiedNameable：具有限定名称的元素
- ExecutableElement：表示类或接口的方法、构造函数或初始化器（静态或实例），包括注释类型元素。
- TypeElement :表示类和接口
- PackageElement：表示包



#### 文件生成

到了现在我们已经基本了解整个APT的基础知识。现在来讲讲APT技术如何`生成新的类的定义（也就是创建新的源文件）`。对于创建新的文件，我们并不用像基本文件操作一样，通过调用IO流来进行读写操作。而是通过[JavaPoet](https://github.com/square/javapoet)来构造源文件。（当然当你使用`JavaPoet`时，在gradle中你需要添加依赖`compile 'com.google.auto.service:auto-service:1.0-rc2'`)，`JavaPoet`的使用也非常简单



### AAPT、AAPT2





### 链接

- [APT注解处理器，让你的代码变得更简单](http://m.imooc.com/article/34181)
- [Android 注解系列之APT工具（三）](https://www.jianshu.com/p/fcba7013b0b0)
- [自定义注解处理器（APT）](https://www.cnblogs.com/wondertwo/p/6017403.html)

- [android注解处理技术APT](https://www.cnblogs.com/linghu-java/p/10118182.html)

- <https://www.jianshu.com/p/8c3437006e79>
- [kapt+butterknife](https://www.jianshu.com/p/cb5795d7847c)

- https://www.jianshu.com/p/9616f4a462bd

- https://blog.csdn.net/xx326664162/article/details/68490059

- https://www.jianshu.com/p/472e66632ed0

- [aapt2](https://www.jianshu.com/p/839969887e2c)

- [官方文档](https://developer.android.com/studio/command-line/aapt2)