- **Kotlin泛型**

  - 泛型，即 "参数化类型"，将类型参数化，可以用在类，接口，方法上
  - 与 Java 一样，Kotlin 也提供泛型，为**类型安全**提供保证，**消除类型强转**的烦恼
  - 定义泛型类型变量，可以完整地写明类型参数，如果编译器可以自动推定类型参数，也可以省略类型参数，编译器会进行类型判断，所以有时候不需要指明泛型具体类型

  ```kotlin
  fun <T> boxIn(value: T) = Box(value)
  
  // 以下都是合法语句
  val box4 = boxIn<Int>(1)
  val box5 = boxIn(1)     // 编译器会进行类型推断
  ```

- **泛型约束**

  - 我们可以使用泛型约束来设定一个给定参数允许使用的类型。
  - Kotlin 中使用 : 对泛型的的类型上限进行约束。
  - 最常见的约束是上界(upper bound)：

  ```kotlin
  fun <T : Comparable<T>> sort(list: List<T>) {
      // ……
  }
  
  //Comparable 的子类型可以替代 T。 例如:
  sort(listOf(1, 2, 3)) // OK。Int 是 Comparable<Int> 的子类型
  sort(listOf(HashMap<Int, String>())) // 错误：HashMap<Int, String> 不是 Comparable<HashMap<Int, String>> 的子类型
  
  //默认的上界是 Any?。
  //对于多个上界约束条件，可以用 where 子句：
  fun <T> copyWhenGreater(list: List<T>, threshold: T): List<String>
      where T : CharSequence,
            T : Comparable<T> {
      return list.filter { it > threshold }.map { it.toString() }
  }
  ```

- **型变**

  - Kotlin 中没有通配符类型，它有两个其他的东西：声明处型变（declaration-site variance）与类型投影（type projections）

  - 声明处型变

    - extends 对应 out  【协变】
    - super 对应 in      【逆变】
    - consumer in ，producer out
    - 类似java，是通过编译器来限制的

  - 星号投射

    - 有些时候, 你可能想表示你并**不知道类型参数的任何信息**, 但是仍然希望能够安全地使用它. 这里所谓"安全地使用"是指, 对泛型类型定义一个类型投射, 要求这个**泛型类型的所有的实体实例**, 都是这个**投射的子类型**
    - **关于星号投射，其实就是*代指了所有类型，相当于Any?**

    

- **再说类型投影**

  “类型投影”是Kotlin中的使用处型变，跟Java类似，只是将“? extends T”换成了“out T”，代表协变；“? super T”换成了“in T”，代表逆变。

  以Kotlin中的Array为例：

  ```kotlin
  class Array<T>(val size: Int) {
      fun get(index: Int): T { ///* …… */ }
      fun set(index: Int, value: T) { ///* …… */ }
  }
  ```

  该类在 T 上既不是协变的也不是逆变的。这造成了⼀些不灵活性。考虑下述函数：

  ```kotlin
  fun copy(from: Array<Any>, to: Array<Any>) {
      assert(from.size == to.size)
      for (i in from.indices)
          to[i] = from[i]
  }
  ```

  那么我们将不能像如下这么使用：

  ```kotlin
  val ints: Array<Int> = arrayOf(1, 2, 3)
  val any: Array<Any> = Array<Any>(3) { "" }
  copy(ints, any) // 错误：期望 (Array<Any>, Array<Any>)
  ```

  这里我们遇到同样熟悉的问题：Array\<T> 在 T 上是不型变的，因此 Array\<Int> 和 Array\<Any> 之间没有关系。为什么？ 再次重复，因为 copy 可能“做坏事”，例如它可能尝试写⼀个 String 到 ints 中，这可能会引发ClassCastException 异常。尽管这里的 copy 方法并没有“做坏事”，但这不能仅凭我们的自觉，还需要编译器来限制我们的行为。我们将copy方法修改为：

  ```kotlin
  fun copy(from: Array<out Any>, to: Array<Any>) {
  // ……
  }
  
  val ints: Array<Int> = arrayOf(1, 2, 3)
  val any: Array<Any> = Array<Any>(3) { "" }
  copy(ints, any) //OK！
  ```

  这里发生的事情称为**类型投影**： **from 不仅仅是⼀个Array，而是⼀个受限制的（ 投影的）Array**，我们只可以调用返回类型为 T 的方法（当然，与类型参数 T 无关的方法也能调用），如上，这意味着我们只能调用 get() 。这就是我们的使用处型变的用法，这对应于 Java 的 Array<? extends Object> ， 但使用方式更简单。

  同样，也有逆变的方式：

  ```kotlin
  fun fill(dest: Array<in String>, value: String) {
      if (dest.size > 0)
          dest[0] = value
  }
  ```

  Array\<in String> 对应于 Java 的 Array<? super String> ，也就是说，你可以传递⼀个 Array\<CharSequence> 或⼀个 Array\<Object> 对象给 fill() 函数。



```kotlin
class JavaMonster<T extends Animal & Food> {
    public <K extends Animal & Food> void haha() {
    }

    public <F> void printIfTypeMatch(Object item){
        //报错
        if (item instanceof F){
            System.out.println();
        }
    }
}

class Monster<T : Animal> {
}

class Monster2<T> where  T : Animal, T : Food {
    fun <F> printIfTypeMatch(item: Any) {
        //报错
        if (item is F) {
            println()
        }
    }

    //refied需要inlinefun使用
    fun <reified F> printIfTypeMatchReified(item: Any) {
        //报错
        if (item is F) {
            println()
        }
    }

    //reified 翻译一下就是: 具体化
    //reified 的意思是具体化。而作为 Kotlin 的一个方法 泛型 关键字，
    // 它代表你可以在方法体内访问泛型指定的JVM类对象。必须以 inline 内联方式声明这个方法才有效。
    inline fun <reified F> printIfTypeMatchReifiedInline(item: Any) {
        //报错
        if (item is F) {
            println()
        }
    }
}

open class Animal {
}

interface Food {
}

```

