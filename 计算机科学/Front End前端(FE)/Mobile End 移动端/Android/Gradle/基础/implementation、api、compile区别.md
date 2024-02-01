[TOC]

> 2017 年google 后，Android studio版本更新至3.0，更新中，连带着com.android.tools.build:gradle 工具也升级到了3.0.0，在3.0.0中使用了最新的Gralde 4.0 里程碑版本作为gradle的编译版本，该版本gradle编译速度有所加速，更加欣喜的是，完全支持Java8。
>  当然，对于Kotlin的支持，在这个版本也有所体现，Kotlin插件默认是安装的。



Android Studio 3.0开始使用了新的指令，原来的很多被弃用了，总的来说是为了加快构建编译速度。

下面是一个总结表格：

| Android Studio 2.X | Android Studio 3.X        |
| ------------------ | ------------------------- |
| apk                | runtimeOnly               |
| provided           | compileOnly               |
| compile            | api                       |
| 没有对应           | implementation            |
| debugCompile       | debugImplementation       |
| releaseCompile     | releaseImplementation     |
| androidTestCompile | androidTestImplementation |

需要解释的主要是implementation系列指令：

implementation：注意compile是和api对应的，效果相同。implementation的区别在于对外可见性，而且可以加快编译速度（原理在于减少不必要的重复编译过程）。举个例子如下：

```java
A module 依赖 B module，B 依赖 C module。
Android Studio 2.X使用compile：
A compile B
B compile C
A module不仅可以引用B module，还可以引用C module的接口和类。
Android Studio 3.X使用implementation：
A implementation B
B implementation C
A module只可以引用B module，不可以引用C module。C 对 A 是不可见的！
```

简单来说，从Android Studio 3.X开始，依赖首先应该设置为implement，如果没有错，那就用implement，如果有错，那么使用api指令，这样会使编译速度有所增快。（就这样理解够了，很多文章又是画图又是长篇大论的，完全没有必要，本来就不是多么复杂的东西）。



### compile（api）

这种是我们最常用的方式，使用该方式依赖的库将会**参与编译和打包**。 
当我们依赖一些第三方的库时，可能会遇到com.android.support冲突的问题，就是因为开发者使用的compile依赖的com.android.support包，而他所依赖的包与我们本地所依赖的com.android.support包版本不一样，所以就会报All com.android.support libraries must use the exact same version specification (mixing versions can lead to runtime crashes这个错误。



### provided（compileOnly）

**只在编译时有效，不会参与打包** 
可以在自己的moudle中使用该方式依赖一些比如com.android.support，gson这些使用者常用的库，避免冲突。



### apk（runtimeOnly）

只在生成apk的时候参与打包，编译时不会参与，很少用。



### testCompile（testImplementation）

testCompile 只在单元测试代码的编译以及最终打包测试apk时有效。



### debugCompile（debugImplementation）

debugCompile 只在debug模式的编译和最终的debug apk打包时有效



### releaseCompile（releaseImplementation）

Release compile 仅仅针对Release 模式的编译和最终的Release apk打包。