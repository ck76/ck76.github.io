[TOC]

# 什么是AOP

AOP(Aspect Oriented Program的首字母缩写)是一种面向切面编程的思想。这种编程思想是相对于OOP(ObjectOriented Programming即面向对象编程)来说的。

先来说一下大家熟悉的面向对象编程:面向对象的特点是**继承、多态和封装**。而封装就要求将功能分散到不同的对象中去，这在软件设计中往往称为职责分配。实际上也就是说，让不同的类设计不同的方法。这样代码就分散到一个个的类中去了。这样做的好处是降低了代码的复杂程度，使类可重用。但是面向对象的编程天生有个**缺点就是分散代码的同时，也增加了代码的重复性**。
比如我希望在项目里面所有的模块都增加日志统计模块,按照OOP的思想,我们需要在各个模块里面都添加统计代码,但是如果按照AOP的思想,可以将统计的地方抽象成切面,只需要在切面里面添加统计代码就OK了。

![切面](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/HvczgUSyZAG8fhxymz2PPx0fj2msbszH915ksgPFDOw!/r/dFMBAAAAAAAA)

## 1、APT + JavaPoet

APT(Annotation Processing Tool 的简称)，可以在**代码编译期解析注解**，结合JavaPoet 生成新的 Java 文件，减少手动的代码输入。现在有很多主流库都用上了 APT，比如 Dagger2, ButterKnife, EventBus3 等

### 1、定义注解

例如

```java
@Retention(RetentionPolicy.CLASS)
@Target(ElementType.FIELD)
public @interface BindView {
int value();
}
```

### 2、定义Processor派生自AbstractProcessor 并且使用@AutoService标注

AutoService会自动在META-INF文件夹下生成Processor配置信息文件，该文件里就是实现该服务接口的具体实现类。而当外部程序装配这个模块的时候，
就能通过该jar包META-INF/services/里的配置文件找到具体的实现类名，并装载实例化，完成模块的注入

### 3、提取注解信息

在Processor中提取注解信息 结合一些手段去处理，比如[JavaPoet](https://github.com/square/javapoet)，或者 [JavaPoet使用指南](https://juejin.im/post/584d4b5b0ce463005c5dc444)

### 核心点

- 处理Processor中的 注解信息的套路
- javapoet中的代码生成 [Elements](https://docs.oracle.com/javase/7/docs/api/javax/lang/model/element/Element.html)



## 2、AspectJ

### 简单介绍

- AspectJ 是使用最为广泛的 AOP 实现方案，适用于 Java 平台，官网地址：[http://www.eclipse.org/aspectj/](http://www.eclipse.org/aspectj/) 其中AspectJ 是在静态织入代码，**即在编译期注入代码的**。

- AspectJ 提供了一套全新的语法实现，完全兼容 Java（跟 Java 之间的区别，只是多了一些关键词而已）。同时，还提供了纯 Java 语言的实现，通过注解的方式，完成代码编织的功能。因此我们在使用 AspectJ 的时候有以下

  两种方式：

  - 使用 AspectJ 的语言进行开发
- 通过 AspectJ 提供的注解在 Java 语言上开发
  
- 因为最终的目的其实都是需要在字节码文件中织入我们自己定义的切面代码，不管使用哪种方式接入 AspectJ，都需要使用 AspectJ 提供的代码编译工具 ajc 进行编译。

- 在 Android Studio 上一般使用注解的方式使用 AspectJ，因为 Android Studio 没有 AspectJ 插件，无法识别 AspectJ 的语法（不过在 Intellij IDEA 收费版上可以使用 AspectJ 插件），所以后面的语法说明和示例都是以注解的实现方式

### 术语

- JoinPoints（连接点）

JoinPoints（连接点），程序中可能作为代码注入目标的特定的点。在AspectJ中可以作为JoinPoints的地方包括
![AspectJ术语表](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/qLzlgSbKjM**Rb2hEgKFXkvBK8T8yyRkw6oUIMYi0h0!/r/dL8AAAAAAAAA)

- PointCuts(切入点)

PointCuts(切入点)，其实就是代码注入的位置。与前面的JoinPoints不同的地方在于，其实PointCuts是有条件限定的JoinPoints。比如说，在一个Java源文件中，会有很多的JoinPoints，但是我们只希望对其中带有@debug注解的地方才注入代码。所以，PointCuts是通过语法标准给JoinPoints添加了筛选条件限定

- Advice(通知)

Advice(通知)，其实就是注入到class文件中的代码片。典型的 Advice 类型有 **before、after 和 around**，分别表示在目标方法执行之前、执行后和完全替代目标方法执行的代码

- Aspect(切面)

Aspect(切面)，Pointcut 和 Advice 的组合看做切面。

- Weaving

注入代码（advices）到目标位置（joint points）的过程



## 3、Javassit

### 简介

[Javassist](https://www.javassist.org/)作用是在**编译器间修改class文件**，与之相似的ASM（热修复框架女娲）也有这个功能，可以让我们直接修改编译后的class二进制代码，

首先我们得知道什么时候编译完成，并且我们要赶在class文件被转化为dex文件之前去修改。在Transfrom这个api出来之前，想要在项目被打包成dex之前对class进行操作，必须自定义一个Task，然后插入到predex或者dex之前，在自定义的Task中可以使用javassist或者asm对class进行操作。而Transform则更为方便，Transfrom会有他自己的执行时机，不需要我们插入到某个Task前面。Tranfrom一经注册便会自动添加到Task执行序列中，并且正好是项目被打包成dex之前。
![Android构建流程](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/dMvJcjBjLwfT.*vUk8UHKUBAgD88h6te*UHBfde6LP8!/r/dFMBAAAAAAAA)

### 常用方式

需要配合自定义 GradlePlugin、TransForm,往往在TransForm期间使用Javassit 去完成一些需要的aop，关于自定义Gradle插件这里就不继续展开了

ClassPool、CtClass、CtMethod核心类的使用在这里展示的很详细。

> 1、初始化ClassPool设置
> 2、通过包名取到对应的CtClass
> 3、CtMethodi插入代码块，写文件，释放，结束整个注入代码过程。

### 结语

这几种AOP方式各自最大的特点就是编织代码的时机不同，这个需要在使用的时候根据实际需要来，

![aop框架对比](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Y1qdwsNEtv6Z3flJEVV7NA8ySMMUUK8FgcQeTO6V2x8!/r/dDIBAAAAAAAA)

通常aop可以用来处理以下问题

- 日志、监控
- 登陆
- 修复第三方代码，比如第三方jar包
- hook 某些代码为封装过的安全性更好的代码， 比如hook系统toast或者Gson等等
- ……



### 链接

- [Android Annotation](https://juejin.im/post/5a771b8b6fb9a0633c65e947#heading-9)[扫盲笔记](https://juejin.im/post/5a771b8b6fb9a0633c65e947#heading-9)
- [Android AOP](https://www.jianshu.com/p/e66e8926c01d)[三剑客之](https://www.jianshu.com/p/e66e8926c01d)[APT](https://www.jianshu.com/p/e66e8926c01d)

 