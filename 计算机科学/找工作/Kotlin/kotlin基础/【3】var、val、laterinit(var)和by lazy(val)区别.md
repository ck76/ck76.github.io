[TOC]

### 一、var和val的区别

Kotlin中有两个关键字定义变量，这两个关键字外形看着差别很小就只差了一个字母，但实际差别很大的。

var是一个变量，这是一个可以通过重新分配来更改为另一个值的变量。这种声明变量的方式和java中声明变量的方式一样。

val是一个不可变变量，这种声明变量的方式相当于java中的final变量。一个val创建的时候必须初始化，因为以后不能被改变。

```kotlin
var name = "zhang san"
println(name)
name = "li si"
println(name)
val finalValue = "我是不可改变的";
println(finalValue);
```

### 二、lateinit （var）和 by lazy（val）

lateinit 和 lazy 是 Kotlin 中的两种不同的【延迟初始化】的实现

1.lateinit 只用于变量 var(且不能是int等基本类型)，而 lazy 只用于常量 val

2.lateinit不能用在可空的属性上和java的基本类型上

```kotlin
val name: String by lazy { "****" }//正确
val name: Int by lazy { 1 }//正确
lateinit var test:String //正确

lateinit val test:String //错误---var才对
lateinit var test:Float //错误--Float
```

- ##### lazy 应用于单例模式(if-null-then-init-else-return)，而且当且仅当变量被第一次调用的时候，委托方法才会执行。

- `lazy()`是接受一个 lambda 并返回一个 `Lazy <T>` 实例的函数，返回的实例可以作为实现延迟属性的委托： 第一次调用 `get()` 会执行已传递给 `lazy()` 的 lambda 表达式并记录结果， 后续调用 `get()` 只是返回记录的结果。

```kotlin
val lazyValue: String by lazy {
    println("computed!")
    "Hello"
}

fun main(args: Array<String>) {
    println(lazyValue)
    println(lazyValue)
}

//打印结果
computed！
Hello

Hello
```



### 三、!!. 与 ?. 的区别

?. 与 !!. 都是Kotlin提供的检测空指针的方法。

？”来明确指定一个对象，或者一个属性变量是否可以为空。

```kotlin
private var mContext: Context? = null
```

"?"加在变量名后，系统在任何情况不会报它的空指针异常。
"!!"加在变量名后，如果对象为null，那么系统一定会报异常！

```kotlin
//?
a?.run()
//与java相同:
if(a!=null){
	a.run();
}		//不会空指针

//!!
a!!.run()
//与java相同: 
if(a!=null){
	a.run();
}else{	//会空指针
	throw new KotlinNullPointException();
}
```



### 四、NULL检查机制

Kotlin的空安全设计对于声明可为空的参数，在使用时要进行空判断处理，有两种处理方式，
字段后加!!像Java一样抛出空异常，
另一种字段后加?可不做处理返回值为 null或配合?:做空判断处理

```kotlin
//类型后面加?表示可为空
var age: String? = "23" 

//抛出空指针异常
val ages = age!!.toInt()

//不做处理返回 null
val ages1 = age?.toInt()

//age为空返回-1
val ages2 = age?.toInt() ?: -1
```



### 五、如何才能生成真正的常量呢

想要实现真正的常量其实不难，方法有两种，一种是const，另一个使用@JvmField注解

#### 1、const

const，顾名思义，就是常量的单词简写，使用它可以声明常量，不过仅限于在top-level和object中。

```kotlin
  //top-level
    const val name = "Kotlin"

  //object中
    class DemoConstant {
          companion object {
              const val subName = ""
          }
    }
```

- 所谓的top-level就是位于代码文件的最外部，比如常见的类（非内部类和嵌套类）就是在top-level。**意思是在结构上常量不属于任何的类，而是属于文件。**
- object中可以指的是最外部的object也可以指的是companion object.

#### 2、@JvmField

- 在val常量前面增加一个@JvmField就可以将它变成常量。
- 其内部作用是抑制编译器生成相应的getter方法
- 是用该注解修饰后则无法重写val的get方法

示例如下

```kotlin
    @JvmField 
	val NAME = "89757
```

