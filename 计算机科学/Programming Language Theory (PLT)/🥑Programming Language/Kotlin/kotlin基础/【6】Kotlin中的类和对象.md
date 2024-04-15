[TOC]

### 类和对象

- **类定义**

  - Koltin 中的类可以有一个 主构造器，以及一个或多个次构造器，主构造器是类头部的一部分，位于类名称之后:如果主构造器没有任何注解，也没有任何可见度修饰符，那么constructor关键字可以省略。

  ```kotlin
  class Person constructor(firstName: String) {}
  ```

  - val声明为可空的话必须在构造函数中进行初始化

  ```kotlin
  var allByDefault: Int? // 错误: 需要一个初始化语句, 默认实现了 getter 和 setter 方法
  var initialized = 1    // 类型为 Int, 默认实现了 getter 和 setter
  val simple: Int?       // 类型为 Int ，默认实现 getter ，但必须在构造函数中初始化
  val inferredType = 1   // 类型为 Int 类型,默认实现 getter
  ```

  - Kotlin 中类不能有字段。提供了 Backing Fields(后端变量) 机制,备用字段使用field关键字声明,field 关键词只能用于属性的访问器，如以上实例：

  ```kotlin
  class Person {
  
      var lastName: String = "zhang"
          get() = field.toUpperCase()   // 将变量赋值后转换为大写
          set
  
      var no: Int = 100
          get() = field                // 后端变量
          set(value) {
              if (value < 10) {       // 如果传入的值小于 10 返回该值
                  field = value
              } else {
                  field = -1         // 如果传入的值大于等于 10 返回 -1
              }
          }
  
      var heiht: Float = 145.4f
          private set
  }
  ```

- **field关键字**

  - 在 Kotlin 中，任何时候当你写出“一个变量后边加等于号”这种形式的时候，比如我们定义 **var no: Int**变量，当你写出 **no = ...** 这种形式的时候，这个等于号都会被编译器翻译成调用 **setter** 方法；而同样，在任何位置引用变量时，只要出现 **no** 变量的地方都会被编译器翻译成 **getter** 方法。那么问题就来了，当你在 **setter** 方法内部写出 **no = ...** 时，相当于在 **setter** 方法中调用 **setter** 方法，形成递归，进而形成死循环，例如文中的例子：

  ```kotlin
  var no: Int = 100
      get() = field                // 后端变量
      set(value) {
          if (value < 10) {       // 如果传入的值小于 10 返回该值
              field = value
          } else {
              field = -1         // 如果传入的值大于等于 10 返回 -1
          }
      }
  /////不使用field关键字
  var no: Int = 100
      get() = no
      set(value) {
          if (value < 10) {       // 如果传入的值小于 10 返回该值
              no = value
          } else {
              no = -1         // 如果传入的值大于等于 10 返回 -1
          }
      }
  //翻译成 Java 代码
  int no = 100;
  public int getNo() {
      return getNo();// Kotlin中的get() = no语句中出来了变量no，直接被编译器理解成“调用getter方法”
  }
  
  public void setNo(int value) {
      if (value < 10) {
          setNo(value);// Kotlin中出现“no =”这样的字样，直接被编译器理解成“这里要调用setter方法”
      } else {
          setNo(-1);// 在setter方法中调用setter方法，这是不正确的
      }
  }
  //翻译成 Java 代码之后就很直观了，在 getter 方法和 setter 方法中都形成了递归调用，显然是不正确的，最终程序会出现内存溢出而异常终止。
  ```

  

- **主构造函数**

  - 主构造器中不能包含任何代码，初始化代码可以放在初始化代码段中

  ```kotlin
  class Person constructor(firstName: String) {
      init {
          println("FirstName is $firstName")
      }
  }
  ```

  - 主构造器的参数可以在初始化代码段中使用，也可以在类主体n定义的属性初始化代码中使用。 一种简洁语法，可以通过主构造器来定义属性并初始化属性值（可以是var或val）

  ```kotlin
  //注意和上面的构造函数区别，这里直接定义了变量，更加简洁方便
  class People(val firstName: String, val lastName: String) {
      //...
  }
  ```

  - 如果构造器有注解，或者有可见度修饰符，这时constructor关键字是必须的，注解和修饰符要放在它之前

- **次级构造函数**

  - 类也可以有二级构造函数，需要加前缀 constructor:

  ```kotlin
  class Person { 
      constructor(parent: Person) {
          parent.children.add(this) 
      }
  }
  ```

  - 如果类有主构造函数，每个次构造函数都要，或直接或间接通过另一个次构造函数代理主构造函数。在同一个类中代理另一个构造函数使用 this 关键字：

  ```kotlin
  class Person(val name: String) {
      constructor (name: String, age:Int) : this(name) {
          // 初始化...
      }
  }
  ```

  - 如果一个非抽象类没有声明构造函数(主构造函数或次构造函数)，**它会产生一个没有参数的构造函数。构造函数是 public 。**如果你不想你的类有公共的构造函数，你就得声明一个空的主构造函数：

  ```kotlin
  class DontCreateMe private constructor () {
  }
  ```

- **抽象类**

  - 抽象是面向对象编程的特征之一，类本身，或类中的部分成员，都可以声明为abstract的。抽象成员在类中不存在具体的实现。

    注意：无需对抽象类或抽象成员标注open注解。

  ```kotlin
  open class Base {
      open fun f() {}
  }
  
  abstract class Derived : Base() {
      override abstract fun f()
  }
  ```

- **嵌套类和内部类**

  - 内部类使用 inner 关键字来表示。内部类会带有一个对外部类的对象的引用，所以内部类可以访问外部类成员属性和成员函数。

  ```kotlin
  class Outer {                  // 外部类
      val bar: Int = 1
      class Nested {             // 嵌套类
          fun foo() = 2
          //var o=bar            //编译错误
      }
  
      private val bar2: Int = 1
      var v = "成员属性"
      /**嵌套内部类**/
      inner class Inner {
          fun foo() = bar2  // 访问外部类成员
          fun innerTest() {
              var o = this@Outer //获取外部类的成员变量
              println("内部类可以引用外部类的成员，例如：" + o.v)
          }
      }
  }
  
  fun main(args: Array<String>) {
      val demo = Outer.Nested().foo() // 调用格式：外部类.嵌套类.嵌套类方法/属性
      println(demo)    // == 2
      val demo2 = Outer().Inner().foo()
      println(demo2) //   1
      val demo3 = Outer().Inner().innerTest()
      println(demo3)   // 内部类可以引用外部类的成员，例如：成员属性
  }
  
  //打印结果
  2
  1
  内部类可以引用外部类的成员，例如：成员属性
  kotlin.Unit
  ```

  - 为了消除歧义，要访问来自外部作用域的 this，我们使用this@label，其中 @label 是一个 代指 this 来源的标签

- **嵌套类和内部类区别**

  - 创建对象的区别
    - 要想构造内部类的对象，必须先构造外部类的对象，而嵌套类则不需要；

  ```kotlin
  var demo = Outter.Nested()// 嵌套类，Outter后边没有括号
  var demo = Outter().Inner();// 内部类，Outter后边有括号
  ```

  - 引用外部类的成员变量的方式不同

    - 嵌套类

    ```kotlin
    class Outer {                  // 外部类
        private val bar: Int = 1
        class Nested {             // 嵌套类
            var ot: Outer = Outer()
            println(ot.bar) // 嵌套类可以引用外部类私有变量，但要先创建外部类的实例，不能直接引用
            fun foo() = 2
        }
    }
    ```

    - 内部类

    ```kotlin
    //直接通过 this@ 外部类名 的形式引用外部类的成员变量，不需要创建外部类对象
    class Outer {
        private val bar: Int = 1
        var v = "成员属性"
        /**嵌套内部类**/
        inner class Inner {
            fun foo() = bar  // 访问外部类成员
            fun innerTest() {
                var o = this@Outer //获取外部类的成员变量
                println("内部类可以引用外部类的成员，例如：" + o.v)
            }
        }
    }
    ```

- **匿名内部类**

  - 使用对象表达式来创建匿名内部类：

  ```kotlin
      test.setInterFace(object : View.OnClickListener {
          override fun OnClick(v:View) {
              println("对象表达式创建匿名内部类的实例")
          }
      })
  ```

- **类的修饰符**

  - classModifier: 类属性修饰符，标示类本身特性。

  ```kotlin
  abstract    // 抽象类  
  final       // 类不可继承，默认属性
  enum        // 枚举类
  open        // 类可继承，类默认是final的
  annotation  // 注解类
  ```

  - accessModifier: 访问权限修饰符

  ```kotlin
  private    // 仅在同一个文件中可见
  protected  // 同一个文件中或子类可见
  public     // 所有调用的地方都可见
  internal   // 同一个模块中可见
  ```



### 继承

- **Kotlin继承**

  - Kotlin 中所有类都继承该 Any 类，它是所有类的超类，对于没有超类型声明的类是默认超类：

  ```kotlin
  class Example // 从 Any 隐式继承
  //Any提供了三个函数
  equals()
  hashCode()
  toString()
  
  //注意：Any 不是 java.lang.Object。
  //如果一个类要被继承，可以使用 open 关键字进行修饰,默认是final不可继承的，无须用open修饰abstract类
  ```

- **构造函数**

  - **子类有**主构造函数：

    - 如果子类有主构造函数， 则基类必须在主构造函数中立即初始化。

    ```kotlin
    open class Person(var name : String, var age : Int){// 基类
    }
    
    class Student(name : String, age : Int, var no : String, var score : Int) : Person(name, age) {
    }
    ```

  - **子类没有**主构造函数

    - 则必须在每一个二级构造函数中用 super 关键字初始化基类，或者在代理另一个构造函数。初始化基类时，可以调用基类的不同构造方法

    ```kotlin
    class Student : Person {
    
        constructor(ctx: Context) : super(ctx) {
        } 
    
        constructor(ctx: Context, attrs: AttributeSet) : super(ctx,attrs) {
        }
    }
    ```

  - 实例

  ```kotlin
  /**用户基类**/
  open class Person(name:String){
      /**次级构造函数**/
      constructor(name:String,age:Int):this(name){
          //初始化
          println("-------基类次级构造函数---------")
      }
  }
  
  /**子类继承 Person 类**/
  class Student:Person{
  
      /**次级构造函数**/
      constructor(name:String,age:Int,no:String,score:Int):super(name,age){
      }
  }
  ```

- **重写**

  - 在基类中，使用fun声明函数时，此函数默认为final修饰，不能被子类重写。如果允许子类重写该函数，那么就要手动添加 open 修饰它, 子类重写方法使用 override 关键词：

  - 如果有多个相同的方法（继承或者实现自其他类，如A、B类），则必须要重写该方法，使用super范型去选择性地调用父类的实现。

  ```kotlin
  open class A {
      open fun f () { print("A") }
      fun a() { print("a") }
  }
  
  interface B {
      fun f() { print("B") } //接口的成员变量默认是 open 的
      fun b() { print("b") }
  }
  
  class C() : A() , B{
      override fun f() {
          super<A>.f()//调用 A.f()
          super<B>.f()//调用 B.f()
          //或者不调用如上方法，写成下面的形式也不会报错
          		[
           		// super<A>.f()
          		// super<B>.f()
         			 println("C")
          		]
      }
  }
  
  fun main(args: Array<String>) {
      val c =  C()
      c.f();
  
  }
  ```

- **属性重写**

  - 属性重写使用 override 关键字，属性必须具有兼容类型，每一个声明的属性都可以通过初始化程序或者getter方法被重写：

  ```kotlin
  open class Foo {
      open val x: Int get { …… }
  }
  
  class Bar1 : Foo() {
      override val x: Int = ……
  }
  ```

  - **你可以用一个var属性重写一个val属性，但是反过来不行[相当于缩小了范围]**
  - 因为val属性本身定义了getter方法，重写为var属性会在衍生类中额外声明一个setter方法
  - 可以在主构造函数中使用 override 关键字作为属性声明的一部分:

  ```kotlin
  interface Foo {
      val count: Int
  }
  class Bar1(override val count: Int) : Foo
  ```

  - 子类继承父类时，**不能有跟父类同名的变量**，父类中该变量为 **open 并且子类用 override 关键字重写**:

  ```kotlin
  open class Person(var name: String, var age: Int) {    
      open var sex: String = "unknow"    
      init {        
          println("基类初始化")    
      }
  }
  // 子类的主构造方法的 name 前边也加了 var，这是不允许的，会报'name' hides member of supertype and needs 'override' modifier
  class Student(var name: String, age: Int, var no: String, var score: Int) : Person(name, age) {
      override var sex: String = "male"
  }
  ```



### 接口

- Kotlin 接口与 Java 8 类似，使用 interface 关键字定义接口，允许方法有默认实现：

- 接口中的属性只能是抽象的，不允许初始化值，接口不会保存属性值，实现接口时，必须重写属性：