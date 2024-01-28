

- **包声明：**

  - kotlin源文件不需要相匹配的目录和包，源文件可以放在任何文件目录。
  - 如果没有指定包，默认为 **default** 包。
  - 默认会多个Kotlin默认包会导入到每个 Kotlin 文件中

- **函数定义：**

  - 可变长参数可以用 **vararg** 关键字进行标识：

  ```kotlin
  fun vars(vararg v:Int){
      for(vt in v){
          print(vt)
      }
  }
  ```

- **定义常量与变量**

  - 可变变量定义：var 关键字
  - 不可变变量定义：val 关键字，只能赋值一次的变量(类似Java中final修饰的变量)

  - 常量与变量都可以没有初始化值,但是在引用前必须初始化
  - **编译器**支持自动类型判断,即声明时可以不指定类型,由编译器判断。!!注意编译器判断

- **NULL检查机制**

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

  - 在进行过 null 值检查之后, x 和 y 的类型会被自动转换为非 null 变量

  ```kotlin
    val x = parseInt(args[0])
    val y = parseInt(args[1])
    // 直接使用 `x * y` 会导致错误, 因为它们可能为 null.
    if (x != null && y != null) {
      // 在进行过 null 值检查之后, x 和 y 的类型会被自动转换为非 null 变量
      print(x * y)
    }
  ```


- **类型检测及自动类型转换**

  - 我们可以使用 is 运算符检测一个表达式是否某类型的一个实例(类似于Java中的instanceof关键字)
  - is 检查后会智能转换
  - !is检查后会保持原引用类型
  - as 手动转换
  - 甚至还可以

  ```kotlin
  fun getStringLength(obj: Any): Int? {
    // 在 `&&` 运算符的右侧, `obj` 的类型会被自动转换为 `String`
    if (obj is String && obj.length > 0)
      return obj.length
    return null
  }
  ```

- **区间**

  - in和!in