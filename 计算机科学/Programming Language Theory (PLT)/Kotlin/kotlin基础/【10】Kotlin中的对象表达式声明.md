> Kotlin 用对象表达式和对象声明来实现创建一个对某个类做了**轻微改动的类的对象**，且不需要去声明一个新的子类。

- **对象表达式**

  - 就像 Java 匿名内部类一样，对象表达式中的代码可以访问包含它的作用域的变量（这些变量不仅仅限制于final变量，Java匿名内部类只能访问final修饰的局部变量或者外部类的成员变量）
  - 对象可以继承于某个基类，或者实现其他接口:,如果超类型有⼀个构造函数，则必须传递适当的构造函数参数给它。 多个超类型可以由跟在冒号后⾯的逗号分隔的列表指定(只能有一个父类)：

  ```kotlin
  open class A(x: Int) {
      public open val y: Int = x
  }
  open class A2(x: Int) {
      public open val y: Int = x
  }
  interface B {}
  val ab: A = object : A(1), B /**,A2(1) 可以有多个接口，只能有一个父类，和Java一样，不要被迷惑了*/{
      override val y = 20
  }
  ```

  - 匿名对象类型只能放在私有作用域中`private` 中或者参数中，放在公有作用域中`public`导致访问不到
  - 匿名对象可以用作只在**本地和私有作用域**中声明的类型。如果你使用匿名对象作为**公有函数**的返回类型或者用作**公有属性**的类型，那么该函数或属性的**实际类型**会是匿名对象声明的**超类型**，如果你没有声明任何超类型，**就会是 Any** 。在**匿名对象中添加的成员将无法访问**

  ```kotlin
  class C  {
      // 私有函数，所以其返回类型是匿名对象类型
      private fun foo() = object {
          val x: String = "x"
      }
      // 公有函数，所以其返回类型是 Any
      fun publicFoo() = object {
          val x: String = "x"
      }
      private var student1 = object {
          val name1:String = "n1"
          val score1:String = "s1"
      }
      public var student2 = object {
          val name2:String = "n1"
          val score2:String = "s1"
      }
  
      /**
       * 测试公有函数和私有函数返回匿名对象的处理：公有方法无法访问匿名对象里面的属性
       */
      fun bar() {
          val x1 = foo().x // 没问题
          //可能是向上转型为Any导致属性无法访问(猜测...)
          val x2 = publicFoo().x // 错误：未能解析的引⽤“x”
      }
  
      /**
       * 测试共有属性与私有属性值都是匿名对象的处理区别：公有有属性无法访问匿名对象里面的属性
       */
      fun testField(){
          student1.name1
          student2.name2 //错误：未能解析的引用name2
      }
  
      //测试伴生对象 每个类里面只有一个伴生对象
      companion object MM{
  
      }
  }
  ```



- **对象声明**

  - Kotlin 使用 object 关键字来声明一个对象。
  - Kotlin 中我们可以方便的通过对象声明来获得一个单例
  - **与对象表达式不同**，当对象声明在另**一个类的内部**时，这个对象并**不能通过外部类的实例**访问到该对象，而**只能通过类名**来访问，**同样该对象也不能直接访问到外部类的方法和变量**
  - 简而言之，外不看里，里不看外
  - 注意：对象声明**不能**在**局部作⽤域（即直接嵌套在函数内部）**，但是它们可以嵌套到其他对象声明或⾮内部类中。

  ```kotlin
  class Site {
      var name = "菜鸟教程"
      object DeskTop{
          var url = "www.runoob.com"
          fun showName(){
              print{"desk legs $name"} // 错误，不能访问到外部类的方法和变量
          }
      }
  }
  fun main(args: Array<String>) {
      var site = Site()
      site.DeskTop.url // 错误，不能通过外部类的实例访问到该对象
      Site.DeskTop.url // 正确
  }
  ```



- **对象表达式和对象声明之间的语义差异**
  - 对象表达式是在**使用**他们的地方立即执行的
  - 对象声明是在**第一次被访问**到时延迟初始化的
  - 伴生对象的初始化是在相应的**类被加载（解析）时**，与 Java 静态初始化器的语义相匹配



- **伴生对象**

  - 类内部的对象声明可以用 companion 关键字标记，这样它就与外部类关联在一起，我们就可以直接通过外部类访问到对象的内部元素
  - 一个类里面只能声明一个内部关联对象，即关键字 companion 只能使用一次
  - 伴生对象的成员看起来像其他语言的静态成员，但在运行时他们仍然是真实对象的实例成员
  - 在 JVM 平台，如果使⽤ `@JvmStatic` 注解，你可以将伴⽣对象的成员⽣成为真正的静态⽅法和字段
  - 例如还可以实现接口：

  ```kotlin
  interface Factory<T> {
      fun create(): T
  }
  
  
  class MyClass {
      companion object : Factory<MyClass> {
          override fun create(): MyClass = MyClass()
      }
  }
  //我们可以省略掉该对象的对象名，然后使用 Companion 替代需要声明的对象名：
  class MyClass {
      companion object {
      }
  }
  
  val x = MyClass.Companion
  ```