- Kotlin 可以对一个类的属性和方法进行扩展，且不需要继承或使用 Decorator 模式。

  扩展是一种静态行为，对被扩展的类代码本身不会造成任何影响。

- **扩展函数**

  - 下面代码为 MutableList 添加一个swap 函数：

  ```kotlin
  // 扩展函数 swap,调换不同位置的值
  fun MutableList<Int>.swap(index1: Int, index2: Int) {
      val tmp = this[index1]     //  this 对应该列表
      //this关键字指代接收者对象(receiver object)
      //(也就是调用扩展函数时, 在点号之前指定的对象实例)。
      this[index1] = this[index2]
      this[index2] = tmp
  }
  ```

- **扩展函数是静态解析的**

  - 扩展函数是静态解析的，并不是接收者类型的虚拟成员，在调用扩展函数时，具体被调用的的是哪一个函数，由调用函数的的对象表达式来决定的，而不是动态的类型决定的:
  - 因为扩展函数在编译后会编译成静态方法

  ```kotlin
  open class C
  
  class D: C()
  
  fun C.foo() = "c"   // 扩展函数 foo
  
  fun D.foo() = "d"   // 扩展函数 foo
  
  fun printFoo(c: C) {
      println(c.foo())  // 类型是 C 类
  }
  
  fun main(arg:Array<String>){
      printFoo(D())
  }
  //实例执行输出结果为：
  c
  ```

  - 若扩展函数和成员函数一致，则使用该函数时，会优先使用成员函数。

- **扩展一个空对象**

  - 在扩展函数内， 可以通过 this 来判断接收者是否为 NULL,这样，即使接收者为 NULL,也可以调用扩展函数。例如:

  ```kotlin
  fun Any?.toString(): String {
      if (this == null) return "null"
      // 空检测之后，“this”会自动转换为非空类型，所以下面的 toString()
      // 解析为 Any 类的成员函数
      return toString()
  }
  fun main(arg:Array<String>){
      var t = null
      println(t.toString())
  }
  //实例执行输出结果为：
  null
  ```

- **扩展属性**

  - 扩展属性允许定义在**类或者kotlin文件中**，**不允许**定义在函数中。初始化属性因为属性**没有后端字段**（backing field），所以**不允许被初始化**，只能由显式提供的 getter/setter 定义。

    ```kotlin
    val Foo.bar = 1 // 错误：扩展属性不能有初始化器
    //显示提供 getter/setter 定义
    val <T> List<T>.lastIndex: Int
        get() = size - 1
    ```

  - 扩展属性**只能**被声明为 val

- **伴生对象的扩展**

  - 伴生对象通过"类名."形式调用伴生对象，伴生对象声明的扩展函数，通过用类名限定符来调用：

  ```kotlin
  class MyClass {
      companion object { }  // 将被称为 "Companion"
  }
  
  fun MyClass.Companion.foo() {
      println("伴随对象的扩展函数")
  }
  
  val MyClass.Companion.no: Int
      get() = 10
  
  fun main(args: Array<String>) {
      println("no:${MyClass.no}")
      MyClass.foo()
  }
  //实例执行输出结果为：
  no:10
  伴随对象的扩展函数
  ```

- **扩展的作用域**

  - 在一个类内部定义扩展函数会有一些影响，以后再学………

- **伴生对象**[扣的太细了可以以后看]

  - 伴生对象内的成员相当于 Java 中的静态成员，其生命周期伴随类始终，在伴生对象内部可以定义变量和函数，这些变量和函数可以直接用类名引用

  对于伴生对象扩展函数，有两种形式，一种是在类内扩展，一种是在类外扩展，这两种形式扩展后的函数互不影响（甚至名称都可以相同），即使名称相同，它们也完全是两个不同的函数，并且有以下特点：

  -  （1）类内扩展的伴随对象函数和类外扩展的伴随对象可以同名，它们是两个独立的函数，互不影响；
  -  （2）当类内扩展的伴随对象函数和类外扩展的伴随对象同名时，类内的其它函数优先引用类内扩展的伴随对象函数，即对于类内其它成员函数来说，类内扩展屏蔽类外扩展；
  -  （3）类内扩展的伴随对象函数只能被类内的函数引用，不能被类外的函数和伴随对象内的函数引用；
  -  （4）类外扩展的伴随对象函数可以被伴随对象内的函数引用，；

  例如以下代码：

  ```kotlin
  class MyClass {
      companion object {
          val myClassField1: Int = 1
          var myClassField2 = "this is myClassField2"
          fun companionFun1() {
              println("this is 1st companion function.")
              foo()
          }
          fun companionFun2() {
              println("this is 2st companion function.")
              companionFun1()
          }
      }
      fun MyClass.Companion.foo() {
          println("伴随对象的扩展函数（内部）")
      }
      fun test2() {
          MyClass.foo()
      }
      init {
          test2()
      }
  }
  val MyClass.Companion.no: Int
      get() = 10
  fun MyClass.Companion.foo() {
      println("foo 伴随对象外部扩展函数")
  }
  fun main(args: Array<String>) {
      println("no:${MyClass.no}")
      println("field1:${MyClass.myClassField1}")
      println("field2:${MyClass.myClassField2}")
      MyClass.foo()
      MyClass.companionFun2()
  }
  ```

  运行结果：

  ```kotlin
  no:10
  field1:1
  field2:this is myClassField2
  foo 伴随对象外部扩展函数
  this is 2st companion function.
  this is 1st companion function.
  foo 伴随对象外部扩展函数
  ```