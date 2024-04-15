### 条件控制

- **IF 表达式**

  - kotlin没有java ? :的三元运算符，取而代之是`val c = if (condition) a else b`

- **使用区间**

  - in 和 !in

- **when表达式**

  - 在 when 中，else 同 switch 的 default。如果其他分支都不满足条件将会求值 else 分支
  - 如果很多分支需要用相同的方式处理，则可以把多个分支条件放在一起，用逗号分隔
  - 也可以检测一个值在（in）或者不在（!in）一个区间或者集合中
  - 检测一个值是（is）或者不是（!is）一个特定类型的值，由于智能转换，你可以访问该类型的方法和属性而无需 任何额外的检测

  ```kotlin
  when (x) {
      in 1..10 -> print("x is in the range")
      0, 1 -> print("x == 0 or x == 1")
      else -> print("otherwise")
  }
  ```



### 循环控制

- **For循环**

  - for 循环可以对任何提供迭代器（iterator）的对象进行遍历

  ```kotlin
  for (item in collection) print(item)
  ```

  - 如果你想要通过索引遍历一个数组或者一个 list，你可以这么做：

  ```kotlin
  for (i in array.indices) {
      print(array[i])
  }
  ```

  - 注意这种"在区间上遍历"会编译成优化的实现而不会创建额外对象。或者你可以用库函数 withIndex：

    ```kotlin
  for ((index, value) in array.withIndex()) {
      println("the element at $index is $value")
  }
    ```



