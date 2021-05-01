```
类加载`、`资源加载`、`组件生命周期管理
```

### 简介

#### 成为一名优秀的Android开发，需要一份完备的[知识体系](https://github.com/JsonChao/Awesome-Android-Exercise)，在这里，让我们一起成长为自己所想的那样~。

2012年，Android插件化技术诞生，从最初只支持动态加载Activity到完全模拟app运行时的沙箱系统，历经了快6年的时间，本文，旨在从全方面的角度解析Android插件化技术。

#### 实现Android插件化需要解决的问题

```
插件中代码的加载和主工程的相互调用
插件中资源的加载和主工程的相互调用
Android四大组件的生命周期管理
```

#### Android插件化的发展历程

第一代：dynamic-load-apk和DroidPlugin

```
dynamic-load-apk:
早期使用ProxyActivity的代理技术，由ProxyActivity去控制插件Activity的生命周期。
缺点：
每一个Activity都必须继承插件Activity，处理Context时必须小心。

DroidPlugin：
通过hook系统服务的方式启动插件Activity，来达到和使用普通方式开发app的效果。
缺点：
过多的hook系统服务，这一过程十分复杂且不够稳定。
```

第二代：VirtulApk、Small、RePlugin

```
原理：实现原理上都选择尽量少的hook，通过在manifest上预埋一些组件实现四大组件的插件化。其中Small更形成了一个跨平台、组件化的框架。
```

第三代：VirtulApp、Atlas

```
VirtulApp：
能够完全模拟app的运行环境，能够实现免安装应用和双开技术。
Atlas：
阿里出品，号称是一个容器化框架，结合了组件化和热更新技术。
```

#### Android插件化的实现原理

一、类加载

1.外部apk中的类加载

```
Android中有两种类加载器，DexClassLoader和PathClassLoader，它们都继承于BaseDexClassLoader。

两者的区别：DexClassLoader多了一个optimizedDirectory的路径参数，这个目录必须是内部存储路径，用于缓存系统创建的Dex文件。

所以我们可以使用DexClassLoader去加载外部Apk中的类。
```

2.双亲委托机制

```
ClassLoader调用loadClass方法加载类采用了双亲委托机制来避免重复加载类。
首先，ClassLoader会查看自身已经加载的类中是否已经存在此类，如不存在，然后，则会使用父类来加载此类，如不能成功加载，则会使用自身重载于BaseDexClassLoader的findClass()方法来加载此类。

DexClass的DexPathList在DexClass的构造器中生成，findClass()方法则是从DexPathList下面找出对应的DexFile，循环DexElements，通过dexElement.dexFile取出对应的DexFile，再通过DexFile.loadClassBinaryName()加载对应的类。
```

二、单DexClassLoader和多DexClassLoader

作用：使用插件DexClassLoader加载出需要的类。

1.多DexClassLoader（Replugin）

```
通过每一个插件的DexClassLoader加载出自身所需要的类，当每一个插件需要加载相同的类库时，可采用该类库的不同版本来使用。
```

2.单DexClassLoader（Small）

```
通过把每一个插件的pathList（DexFile）合并到主app的DexClassLoader上，来使各个插件和主app直接能够相互调用类和方法，并且各个插件中相同的功能可以抽取出来作为一个Common插件供其它插件使用。
```

3.互相调用

插件调用主工程

```
在ClassLoader构造时指定主工程的DexClassLoader为父加载器即可直接调用主工程中的类和方法。
```

主工程调用插件

```
如果是多DexClassLoader的情况，则需要通过插件的DexClassLoader加载对应的类并反射调用其方法。此种情况，主工程一般会在一个统一的地方对访问插件中的类和方法做一些访问权限的管理及配置。

如果是单DexClassLoader的情况，则可以直接调用插件中的类和方法。但是当多个插件引用的库的版本不同时，会出现错误，因此，建议采用Gradle版本依赖管理统一处理主工程及各个插件的库依赖。
```

三、资源加载

Android通过Resource来加载资源，只要有插件apk，就可以使用assertManager.addAssertPath（apkPath）的方式来生成assertManager，再使用其new出对应的Resource对象即可。

注意：由于AssertManager并不是Public，所以需要通过反射的方式去调用它。并且由于一些Rom对Resource的处理，所以，需要兼容处理。

1.资源路径的处理

有2种处理方式：

```
合并式：利用assertManager.addAssetPath()将主工程和各个插件的apk路径一起加入。
优势：资源共用。
逆势：需要处理资源id冲突。

独立式：主工程和插件都生成各自独立的Resource。
优势：不需要处理资源id冲突。
逆势：各个插件需要通过某些方式去获取其它插件的Resource。
```

2.Context的处理

```
1.创建主工程的Resource
2.hook主工程的Resource
3.将activity与Resource关联
```

3.资源冲突

```
产生的原因：由于主工程和各个插件引用的Resource id重复产生的冲突。
解决思路：Android中的资源在系统中是以8位16进制0XPPTTRRRR的方式存在，其中PP即是资源区分的区域（Android系统只用它来区分系统资源和应用资源），只要让每一个插件的PP段取不同的值即可解决资源id冲突的问题。
具体解决方式：
1.修改aapt源码，编译期修改PP段。
2.修改Resource的arsc文件，其中的每一条都包含了资源id和映射路径。
```

#### 四大组件支持（Activity）

```
Activity的处理最为复杂，有两种处理方式：
1.ProxyActivity的方式。
2.预埋StubActivity，hook系统启动Activity的过程。

原理：VirtualAPK通过替换了系统的Instrumentation，hook了Activity的启动和创建，省去了手动管理插件Activity生命周期的繁琐，让插件Activity像正常的Activity一样被系统管理，并且插件Activity在开发时和常规一样，即能独立运行又能作为插件被主工程调用。
```

#### Android插件化的发展方向

```
Android插件化方向主要有2个方向：
1.结合组件化技术，成为一个大中型app的基础框架。
2.完全模拟app运行环境的沙盒系统。
```