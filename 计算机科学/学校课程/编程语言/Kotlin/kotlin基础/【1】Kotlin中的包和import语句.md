[TOC]

### 一、概念

Kotlin 包与 Java 包的概念是一样的，都表示文件结构（语法上并不严格要求），可以起到区分同名源代码文件、组织项目结构的作用。

不过，由于允许包级函数和包级属性的存在，Kotlin 包的内容更加丰富，不像 Java 包只包含类和接口。

与 Java 源文件相同，Kotlin 源文件至多有一个 package 语句，必须放在第一行（除注解外），多个文件层次间使用点号分隔：

一个源文件需要以包声明为开头：

```kotlin
package foo.bar
fun baz() {}
class Goo {}
// ...
```

源文件的所有内容（如类和函数）都包含在声明的包中。 所以，在上面的例子中，baz（）的全称是foo.bar.baz，而Goo的全称是foo.bar.Goo。

如果没有指定包，则这样的文件的内容属于没有名称的“default”包。



### 二、文件注解

此外，Kotlin 的 package 语句前可以有注解，这些注解不是针对 package 语句的，而是**针对整个 Kotlin 源文件**的，要在注解前加上 `@file: 标识符`。主要有两个： 
`@kotlin.jvm.JvmName(val name: String)：` 
这个注解可以用来修饰文件和函数，用来指定目标元素编译后的名称。对于一个 Kotlin 源文件来说 ，使用这个注解可以**指定以 Kt 结尾的“文件类”的名称。** 
`@kotlin.jvm.JvmMultiFileClass：` 

这个注解只能用来修饰文件，用来告诉 Kotlin 编译器：这个文件里定义的包级函数和包级属性，都只是一个 class 文件的一部分，这个 class 文件还有一部分定义在其他文件里。**这个注释一般与上面的 JvmName 配合使用：**

```kotlin
// kotlin.util.Synchronized.kt
@file:kotlin.jvm.JvmMultiFileClass
@file:kotlin.jvm.JvmName("StandardKt")
package kotlin
inline fun <R> synchronized(lock: Any, block: () -> R): R {/*...*/}
```

Synchronized.kt 文件用这两个注解表示这里的 synchronized() 函数应该包含在 StandardKt.class 文件里，这个 class 的另一部分在 Standard.kt 文件里。



### 三、默认导入（Default Imports）

下述包默认情况下会被导入每一个Kotlin文件中：

1. kotlin.*
2. kotlin.annotation.*
3. kotlin.collections.*
4. kotlin.comparisons.* (since 1.1)
5. kotlin.io.*
6. kotlin.ranges.*
7. kotlin.sequences.*
8. kotlin.text.*

根据目标平台的不同会相应导入需要的其他包：

1. JVM：
   - java.lang.*
   - kotlin.jvm.*
2. JS：
   - kotlin.js.*



### 四、导入（Imports）

除了默认导入，每个文件可能包含属于该文件本身的导入指令。

我们可以导入单个名称，如：

```kotlin
import foo.Bar // Bar is now accessible without qualification
```

或范围的所有可访问内容（包，类，对象等）：

```kotlin
import foo.* // everything in 'foo' becomes accessible
```

如果有重名冲突，我们可以通过使用as关键字来本地重命名来消除歧义：

```kotlin
import foo.Bar // Bar is accessible
import bar.Bar as bBar // bBar stands for 'bar.Bar'
```

import关键字不限于导入类; 您也可以使用它来导入其他声明：

1. 顶级函数和属性
2. 在对象声明中声明的函数和属性
3. 枚举常数

与Java不同，Kotlin没有单独的“import static”语法; 所有这些声明都使用常规import关键字导入。

```kotlin
// Test.kt
package test
class TestKotlin {
    companion object {
        fun printWorld() = println("World")
    }
}

// Test.java
package test;
public class TestJava {
    public static void printHello() {
        System.out.println("Hello");
    }
}

//导包语句
import test.TestKotlin.Companion.printWorld
import test.TestJava.printHello

fun main(vararg args: String) {
  printHello()
  printWorld()
}
```

