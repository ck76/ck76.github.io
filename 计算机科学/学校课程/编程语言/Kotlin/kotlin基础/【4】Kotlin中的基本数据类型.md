- **基本数据类型**
  - Kotlin 的基本数值类型包括 Byte、Short、Int、Long、Float、Double 等
  - 不同于Java的是，**字符不属于数值类型**，是一个独立的数据类型
  - 通常情况下一个字节占8位

| 类型   | 位宽度 |
| ------ | ------ |
| Double | 64     |
| Float  | 32     |
| Long   | 64     |
| Int    | 32     |
| Short  | 16     |
| Byte   | 8      |

- **字面常量**

  - 16进制0x
  - 2进制0b
  - 不支持8进制

  - 长整型以大写的 L 结尾：123L
  - Doubles 默认写法: `123.5`, `123.5e10`
  - Floats 使用 f 或者 F 后缀：`123.5f`

- **字符**

  - 和 Java 不一样，Kotlin 中的 Char 不能直接和数字操作，Char 必需是单引号 **'** 包含起来的。比如普通字符 '0'，'a'

- **布尔**

  - 布尔用 Boolean 类型表示，它有两个值：true 和 false
  - **若需要可空引用布尔会被装箱**

- **数组**

  - 数组用类 Array 实现，并且还有一个 size 属性及 get 和 set 方法，由于使用 [] 重载了 get 和 set 方法，所以我们可以通过下标很方便的获取或者设置数组对应位置的值
  - 注意: 与 Java 不同的是，Kotlin 中数组是**不型变的（invariant）**
  - 除了类Array，还有ByteArray, ShortArray, IntArray，用来表示各个类型的数组，省去了装箱操作，因此效率更高，其用法同Array一样

- **字符串**

  - 和 Java 一样，String 是不可变的
  - Kotlin 支持三个引号 """ 扩起来的字符串，支持多行字符串，类似python

- **装箱与拆箱**

  ```kotlin
  val a: Int = 100
  println(a === a) // true，值相等，对象地址相等
  
  //经过了装箱，创建了两个不同的对象
  val boxedA: Int? = a
  val anotherBoxedA: Int? = a
  
  //虽然经过了装箱，但是值是相等的，都是100
  println(boxedA === anotherBoxedA) //  true，值相等，128 之前对象地址一样
  println(boxedA == anotherBoxedA) // true，值相等
  ```

  - 这里我把 a 的值换成 100，这里应该跟 Java 中是一样的，在范围是 [-128, 127] 之间并不会创建新的对象，比较输出的都是 true，从 128 开始，比较的结果才为 false。
  - 类型是 **Int?** 可空类型，**类型不同所以必须装箱，导致产生一个新对象,但也只在 [-128, 127] 范围之外有效**

- **不显示声明不会封装新对象**

  ```kotlin
  val a = 10000//不显示声明类型不会封装新对象
  val boxedA = a
  val anotherBoxedA = a
  println(boxedA === anotherBoxedA) //  true
  ```

- **菜鸟上一个完整的例子**

  ```kotlin
  // Int? Int? Int?
  val a1: Int? = 128
  var boxedA: Int? = a1
  var boxedB: Int? = a1
  log(" boxedA === boxedB ? " + (boxedA === boxedB))//true
  log(" boxedA == boxedB ? " + (boxedA == boxedB))//true
  
  // Int Int? Int?
  val a2: Int = 128
  boxedA = a2
  boxedB = a2
  log(" boxedA === boxedB ? " + (boxedA === boxedB))//false
  //值相等
  log(" boxedA == boxedB ? " + (boxedA == boxedB))//true
  
  // Int? Int? Int?
  val a3: Int? = 127
  boxedA = a3
  boxedB = a3
  log(" boxedA === boxedB ? " + (boxedA === boxedB))//true
  //值相等
  log(" boxedA == boxedB ? " + (boxedA == boxedB))//true
  
  // Int Int? Int?
  val a4: Int = 127
  boxedA = a4
  boxedB = a4
  log(" boxedA === boxedB ? " + (boxedA === boxedB))//true
  //值相等
  log(" boxedA == boxedB ? " + (boxedA == boxedB))//true
  
  // Int Int Int
  val a5: Int = 128
  val boxedA2: Int = a5
  val boxedB2: Int = a5
  
  log(" boxedA === boxedB ? " + (boxedA2 === boxedB2))//true
  //值相等
  log(" boxedA == boxedB ? " + (boxedA2 == boxedB2))//true
  ```

  