[TOC]

### Kotlin相比于Java的优势

- 它更加易表现：这是它最重要的优点之一。你可以编写少得多的代码。
- 它更加安全：Kotlin是空安全的，也就是说在我们编译时期就处理了各种null的情况，避免了执行时异常。如果一个对象可以是null，则我们需要明确地指定它，然后在使用它之前检查它是否是null。你可以节约很多调试空指针异常的时间，解决掉null引发的bug。
- 它是函数式的：Kotlin是基于面向对象的语言。但是就如其他很多现代的语言那样，它使用了很多函数式编程的概念，比如，使用lambda表达式来更方便地解决问题。其中一个很棒的特性就是Collections的处理方式。
- 它可以扩展函数：这意味着我们可以扩展类的更多的特性，甚至我们没有权限去访问这个类中的代码。
- 它是高度互操作性的：你可以继续使用所有的你用Java写的代码和库，因为两个语言之间的互操作性是完美的。甚至可以在一个项目中使用Kotlin和Java两种语言混合编程。
- 最后构建应用程序，无论是在虚拟机或连接的设备上运行。，所有这些工作与 Java 并无区别，也采用类似于 Java 编写的 Android 应用程序的方式进行签名。更重要的是Kotlin有着极小的运行时文件体积（整个库的大小约 859KB），Kotlin 编译器所生成的字节码看上去也几乎毫无差异，对 apk 文件大小影响微乎其微。



### 空类型安全

```java
var ck : String ="chengkun"
var ck2 : String =null   //错误
var ck3 : String ? =null //声明为可空，注意可空String与正常String不是一种类型，不可直接赋值

ck = ck2  //错误，非空类型不可直接赋值给正常类型
ck = ck2 !! //加两个感叹号表示强转

ck2 = ck //但是反过来，将非空的ck赋值给可空的ck2是可行的
```



### Kotlin 中如何比较对象

- java中：
  - 使用 == 比较对象
  - 使用 equals 比较值
- Kotlin中：
  - 使用 === 比较对象
  - 使用 == 比较值



### 扩展函数和扩展属性

Kotlin的扩展函数可以让你作为一个类成员进行调用的函数，但是是定义在这个类的外部。这样可以很方便的扩展一个已经存在的类，为它添加额外的方法。在Kotlin源码中，有大量的扩展函数来扩展java，这样使得Kotlin比java更方便使用，效率更高。通常在java中，我们是以各种XXXUtils的方式来对已经存在的类进行功能的扩展。但是有了扩展函数，我们就能丢弃让人讨厌的XXXUtils方法工具类。下面举个例子，假如我们需要为String类型添加一个返回这个字符串最后一个字符的方法：

```
package com.dengyin2000.kotlintest1

fun String.lastChar(): Char = this.get(this.length - 1)

fun main(args: Array<String>) {
    println("Kotlin".lastChar())
}
```

- 对一些不可控的第三方类库添加，并不是修改原类，而是编译时动态添加的方法



### 什么是闭包

闭包就是能够读取其他函数内部变量的函数。例如在javascript中，只有函数内部的子函数才能读取[局部变量](https://baike.baidu.com/item/%E5%B1%80%E9%83%A8%E5%8F%98%E9%87%8F/9844788)，所以闭包可以理解成“定义在一个[函数](https://baike.baidu.com/item/%E5%87%BD%E6%95%B0/301912)内部的函数“。在本质上，**闭包是将函数内部和函数外部连接起来的桥梁。**

一段程序代码通常由常量、变量和表达式组成，然后使用一对花括号“{}”来表示闭合，并包裹着这些代码，由这对花括号包裹着的代码块就是一个闭包。

Kotlin语言中有三种闭包形式：全局函数、自嵌套函数、匿名函数体。

1.函数运行的环境

2.持有函数运行状态

3.函数内部可以定义函数

4.函数内部也可以定义类

闭包是Kotlin语言的众多特性之一，对多数习惯使用了Java语言的开发者来说是一个很难理解的东西（实际上Java8也开始支持闭包特性），Kotlin中的闭包是一个功能性自包含模块，可以再代码中被当做参数传递或者直接使用。这个描述可能不太直观，你可能还是想问：“那么到底什么是闭包呢？闭包在函数中是以什么形式出现和使用的呢？”下面向大家介绍。

一段程序代码通常由常量、变量和表达式组成，然后使用一对花括号“{}”来表示闭合，并包裹着这些代码，由这对花括号包裹着的代码块就是一个闭包。其实在签名我们也介绍了全局和嵌套函数就是一种特殊的闭包。这里，我们总结了一下，Kotlin语言中有三种闭包形式：全局函数、自嵌套函数、匿名函数体。

听着名词解释是挺让人费解，下面我们举个例子：

```kotlin
fun main(args: Array<String>) {
    // 执行test闭包的内容
    test
}

// 定义一个比较测试闭包
val test = if (5 > 3) {
    println("yes")
} else {
    println("no")
}
```

先不说闭包的结构，从代码层面来看，上述逻辑我们都知道这段代码永远都只会输出“yes”。那么你可能会问： 

- 为什么能够将一个if逻辑语句赋值给test呢？ 
- 为什么在main函数中单独写一个test就能执行test所指向的if逻辑呢？ 
- 如果一个if逻辑快块是一个闭包？那么还有什么逻辑块可以是闭包呢？

下面，我们会一一给你解答。

为什么会设计闭包这种结构？

从上述的例子来说，我们可以看出来，其实定义一个函数就好了，为什么设计编程语言的人要设计闭包这么一个结构呢？这就得从作用域开始说起。变量的作用域无非就是两种：全局变量和局部变量。

- 全局变量
  就Kotlin语言而言，函数内部可以直接读取全局变量。
- 局部变量
  另一方面，在函数外部自然无法读取函数内的局部变量。
- 那么，如何在外部调取局部的变量呢？答案就是——闭包。

这里，我们给闭包下一个定义：闭包就是能够读取其他函数内部变量的函数。有没有发现闭包这点的好处，闭包就是在函数被创建的时候，存在的一个私有作用域，并且能够访问所有的父级作用域。每个功能模块我们都能够拆解到不同fun里，不同fun里的变量保持相互调用的可能性，相互独立还彼此不影响。我们可以函数式编程了！

广义上来说，在Kotlin语言之中，函数、条件语句、控制流语句、花括号逻辑块、Lambda表达式都可以称之为闭包，但通常情况下，我们所指的闭包都是在说Lambda表达式。



### Kotlin代理模式

- kotlin的动态代理是编译后转换为静态代理去调用，所以比java的动态代理效率高
- java本质是反射动态创建代理类来实现的
- kotlin本质是静态代理

1.只能实现对接口方法的代理，即Base类不能为抽象类。
2.不方便对所有的代理方法进行统一处理。比如说在执行每个方法前都执行相同的逻辑，而java动态代理可以方便的实现这个功能。
3.方法名称有冲突时，代理类方法优先级较高。
4.编译期自动生成代理模式。不会影响运行效率。

```java
class Derived(b: Base) : Base by b
```



### var和val

-  var是一个可变变量，这是一个可以通过重新分配来更改为另一个值的变量。这种声明变量的方式和java中声明变量的方式一样。
- val是一个只读变量，这种声明变量的方式相当于java中的final变量。一个val创建的时候必须初始化，因为以后不能被改变。

一般kotlin用到的是var和val 

- var：变量(读写) #####提供get/set方法
- val：常量(只读)######提供get方法

但同时提供了一个const，val其实具有了大部分const拥有的功能。

const只能修饰val，不能修饰var

- 正确  `const val testName = "ZhangSan"`
- 报错  `const var testName = "ZhangSan"`

从上面java引用进过const修饰的constValPara变量，与在java中定义的常量引用方式一样，而只用val修饰的常量，引用方式变成getter的模式，所以： 
**实质上经过const修饰的常量，才是java中理解的常量**



### 伴生对象

- 因为kotlin没有静态方法
  - JvmStatic
  - companion object(伴生对象)
  - 伴生对象一定要写在一个类的类里
  - 伴生对象实际上在编译好以后会在类内部生成一个静态对象叫Companion



### 数据类Model

- 自动生成getter和setter方法
- 以及toSting(),hashCode(),equals(),copy()等方法
- 只需要在类声明前面加上**data**去声明就可以了
- 数据类是final类型的不能再添加open去修饰他



### 枚举类和密闭类

- kotlin中枚举类和Java 的枚举类一样
- 但是通常使用密闭类
- sealed关键字声明
- 密闭类可扩展

```kotlin
sealed class SuperCommand{
    object A :SupperCommand()
    object B :SupperCommand()
}

fun(ck:SuperCommand)= when (ck){
    SupperCommand.A -> {
        
    }
    SupperCommand.B -> {
        
    }
}
```



### 解构

> kotlin的解构就是将一个对象拆解赋值

```kotlin
class User constructor(var age: Int, var name: String) {
    operator fun component1() = age  //componentX 格式固定
    operator fun component2() = name
}

fun main(args: Array<String>) {
    var ck = User(18, "ck")
    var (age, name) = ck
    print(age)
    print(name)
}

//例如遍历一个map
val map :Map<String,String> =mapOf<String,String> {"key" to "key","value" to "value"}
for((k,v) in map){
    print("$k----$v")
}
```



### 遍历集合

```kotlin
//数组
for (i in array.indices) {
    print(array[i])
}

//链表
val list = arrayListOf<String>{"a","b","c"}
for(item in list){
   print(item) 
}
//解构形式
for((index,item) in list.withIndex()){
    println("第${index}个元素是${item}")
}
list需要调用一个withIndex函数返回一个迭代器
```



### 集合操作符

> 类似于rxjava，通过一步一步的操作符对数据进行加工，得到我们想要的数据

```kotlin
var list = arrayListOf<Char>('a', 'b', 'c', 'd','e')
var result = list.map { it - 'a' }
        .filter { it > 0 }
        //.find { it > 1 }//find只会返回符合lambda闭条件的第一个值，对应的。。。

fun main(args: Array<String>) {
    for (item in result) {
        print(item)
    }
}
```



### 作用域函数

> 作用域函数是kotlin内置的可以对数组做变换的函数，与集合操作符类似，但是这个操作符作用域更广
>
> - 它们都有自己的作用域
> - 它们作用域中的接收者是this或者it
> - 它们都有一个返回值，返回最后一个对象(this)或者调用者自身(itself)

- run {...}
- with(T) {... }
- let {...}
- apply {...}
- also {...}

- **函数选择**

现在思路变清晰了，根据这三大特性，我们可以对函数进行分类。基于此可以构建一个决策树来帮助我们根据需要来选择使用哪一个函数。

![如何选择](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/rFML1MUN1pqcIl6qMKTo7Uulws0jkLNby*MrWHi*Y7s!/r/dEgBAAAAAAAA)

```
需要返回自身么？
	是：
		参数：
			this:  T.apply()
			it:    T.also()
	否：
		拓展函数？
			是：
				参数：
					this:  T.run()
					it:    T.let()
			否：
				参数是否为this?
					是:  run()
					否:  with(T)
```



### 什么是协程

协程，英文Coroutines，是一种比线程更加轻量级的存在。**正如一个进程可以拥有多个线程一样，一个线程也可以拥有多个协程。

最重要的是，协程不是被操作系统内核所管理，而完全是由程序所控制（也就是在用户态执行）

> 协程通过将复杂性放入库来简化异步编程。程序的逻辑可以在协程中顺序地表达，而底层库会为我们解决其异步性。该库可以将用户代码的相关部分包装为回调、订阅相关事件、在不同线程（甚至不同机器）上调度执行，而代码则保持如同顺序执行一样简单。

协程的开发人员 Roman Elizarov 是这样描述协程的：**协程就像非常轻量级的线程**。线程是由系统调度的，线程切换或线程阻塞的开销都比较大。而**协程依赖于线程，但是协程挂起时不需要阻塞线程**，几乎是无代价的，协程是由开发者控制的。所以**协程也像用户态的线程**，非常轻量级，一个线程中可以创建任意个协程。

总而言之：协程可以简化异步编程，可以顺序地表达程序，协程也提供了一种避免阻塞线程并用更廉价、更可控的操作替代线程阻塞的方法 -- 协程挂起。

#### 1、挂起函数

协程中不能被随机指令挂起，我们必须在「挂起点」才能挂起一个协程。被 `suspend`修饰的函数成为挂起函数，我们在调用挂起函数的时候，会决定是否需要挂起协程还是继续执行。一般我们会通过一个匿名的挂起函数（lambda 表达式）来启动协程。

```kotlin
suspend fun requestToken(): Token { ... }   // 挂起函数
suspend fun createPost(token: Token, item: Item): Post { ... }  // 挂起函数
fun processPost(post: Post) { ... }
```

`requestToken`和`createPost`函数前面有`suspend`修饰符标记，这表示两个函数都是挂起函数。挂起函数能够以与普通函数相同的方式获取参数和返回值，但是调用函数可能挂起协程（如果相关调用的结果已经可用，库可以决定继续进行而不挂起），**挂起函数挂起协程时，不会阻塞协程所在的线程**。挂起函数执行完成后会恢复协程，后面的代码才会继续执行。**但是挂起函数只能在协程中或其他挂起函数中调用,事实上，要启动协程，至少要有一个挂起函数，它通常是一个挂起 lambda 表达式。所以`suspend`修饰符可以标记普通函数、扩展函数和 lambda 表达式。**

挂起函数只能在协程中或其他挂起函数中调用，上面例子中`launch`函数就创建了一个协程。

```kotlin
fun postItem(item: Item) {
    GlobalScope.launch { // 创建一个新协程
        val token = requestToken()
        val post = createPost(token, item)
        processPost(post)
        // 需要异常处理，直接加上 try/catch 语句即可
    }
}
```

`launch`函数：

```kotlin
public fun CoroutineScope.launch(
    context: CoroutineContext = EmptyCoroutineContext,
    start: CoroutineStart = CoroutineStart.DEFAULT,
    block: suspend CoroutineScope.() -> Unit
): Job
```



### 链接

- [Kotlin提高生产力](https://github.com/ck76/Blog/blob/master/Kotlin/Kotlin%E7%89%B9%E6%80%A7/%E7%94%A8Kotlin%E6%8F%90%E9%AB%98%E7%94%9F%E4%BA%A7%E5%8A%9B.md)

- [不要用Java的语法思想写Kotlin](https://github.com/ck76/Blog/blob/master/Kotlin/Kotlin%E7%89%B9%E6%80%A7/%E4%B8%8D%E8%A6%81%E7%94%A8Java%E7%9A%84%E8%AF%AD%E6%B3%95%E6%80%9D%E6%83%B3%E5%86%99Kotlin.md)