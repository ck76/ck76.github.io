[TOC]

### 1、Kotlin委托

- [委托模式](http://www.runoob.com/w3cnote/delegate-mode.html)是软件设计模式中的一项基本技巧。
- 在委托模式中，有两个对象参与处理同一个请求，接受请求的对象将请求委托给另一个对象来处理。
- Kotlin 直接支持委托模式，更加优雅，简洁。
- Kotlin 通过关键字 by 实现委托



### 2、类委托 by

- 实际上就是代理模式，不方便对所有的代理方法进行统一处理。比如说在执行每个方法前都执行相同的逻辑，而java动态代理可以方便的实现这个功能。
- 编译期自动生成代理模式。不会影响运行效率

```kotlin
// 创建接口
interface Base {   
    fun print()
}

// 实现此接口的被委托的类
class BaseImpl(val x: Int) : Base {
    override fun print() { print(x) }
}

// 通过关键字 by 建立委托类
class Derived(b: Base) : Base by b

fun main(args: Array<String>) {
    val b = BaseImpl(10)
    Derived(b).print() // 输出 10
}
```



### 3、属性委托 by Delegate

- 属性委托指的是一个类的**某个属性值不是在类中直接进行定义**，而是将其**托付给一个代理类**，从而实现对该类的属性统一管理

- `val/var <属性名>: <类型> by <表达式>`

- by 关键字之后的表达式就是委托, 属性的 **get() 方法(以及set() 方法)**将被**委托**给这个对象的 **getValue() 和 setValue() 方法**。属性委托**不必实现任何接口**, 但必须提供 getValue() 函数(对于 var属性,还需要 setValue() 函数)

  ```kotlin
  import kotlin.reflect.KProperty
  // 定义包含属性委托的类
  class Example {
      var p: String by Delegate()
  }
  
  // 委托的类
  class Delegate {
      operator fun getValue(thisRef: Any?, property: KProperty<*>): String {
          return "$thisRef, 这里委托了 ${property.name} 属性"
      }
  
      operator fun setValue(thisRef: Any?, property: KProperty<*>, value: String) {
          println("$thisRef 的 ${property.name} 属性赋值为 $value")
      }
  }
  fun main(args: Array<String>) {
      val e = Example()
      println(e.p)     // 访问该属性，调用 getValue() 函数
  
      e.p = "Runoob"   // 调用 setValue() 函数
      println(e.p)
  }
  
  //输出结果
  Example@433c675d, 这里委托了 p 属性
  Example@433c675d 的 p 属性赋值为 Runoob
  Example@433c675d, 这里委托了 p 属性
  ```



### 4、标准委托 by lazy

- Kotlin 的标准库中已经内置了很多工厂方法来实现属性的委托

- 延迟属性 Lazy：

  - lazy() 是一个函数, 接受一个 **Lambda** 表达式作为参数，返回一个 Lazy \<T> 实例的函数
  - **返回的实例**可以作为实现**延迟属性的委托**： 
  - 第一次调用 get() 会执行已传递给 lazy() 的 lamda 表达式并记录结果
  -  后续调用 get() 只是返回记录的结果

  ```kotlin
  val lazyValue: String by lazy {
      println("computed!")     // 第一次调用输出，第二次调用不执行
      "Hello"
  }
  
  fun main(args: Array<String>) {
      println(lazyValue)   // 第一次执行，执行两次输出表达式
      println(lazyValue)   // 第二次执行，只输出返回值
  }
  
  //执行输出结果：
  computed!
  Hello
  Hello
  ```



### 5、可观察属性 Observable  (Delegates)

- observable 可以用于**实现观察者模式**
- **Delegates**.observable() 函数接受两个参数:
  -  第一个是初始化值
  - 第二个是属性值变化事件的响应器(handler)。
- 在属性赋值后会执行事件的响应器(handler)，它有三个参数：**被赋值的属性、旧值和新值：**

```kotlin
import kotlin.properties.Delegates

class User {
    var name: String by Delegates.observable("初始值") {
        prop, old, new ->
        println("旧值：$old -> 新值：$new")
    }
}

fun main(args: Array<String>) {
    val user = User()
    user.name = "第一次赋值"
    user.name = "第二次赋值"
}

//执行输出结果：
旧值：初始值 -> 新值：第一次赋值
旧值：第一次赋值 -> 新值：第二次赋值
```



### 6、把属性储存在映射中 

- 一个常见的用例是在一个映射（map）里存储属性的值。
-  这经常出现在像解析 JSON 或者做其他"动态"事情的应用中。
-  在这种情况下，你可以使用**映射实例自身作为委托来实现委托属性**。