[TOC]

### Companion functions--@JvmStatic

伴生对象中的公共函数必须用@ Jvm Static 注解才能作为静态方法曝光。 没有注释，这些函数只能作为静态 Companion 字段上的实例方法可用。

```kotlin
//错误写法
class KotlinClass {
    companion object {
        fun doWork() {
            /* … */
        }
    }
}

public final class JavaClass {
    public static void main(String... args) {
        KotlinClass.Companion.doWork();
    }
}
```

```kotlin
//正确写法
class KotlinClass {
    companion object {
        @JvmStatic fun doWork() {
            /* … */
        }
    }
}

public final class JavaClass {
    public static void main(String... args) {
        KotlinClass.doWork();
    }
}
```



### Companion constants--@JvmField

公共的，非 const 属性，在伴生对象中是有效常量，必须用@ JvmField 注释，**以暴露为静态字段**。 没有注释，这些属性只能作为奇怪名称的实例可用 " 盖特特斯 " 在静态伴侣领域。 使用@ JvmStatic 而不是@ JvmField 移动奇怪名称 " 盖特特斯 " 到类上的静态方法，这仍然是不正确的。

```kotlin
//错误写法--没有注解
class KotlinClass {
    companion object {
        const val INTEGER_ONE = 1
        val BIG_INTEGER_ONE = BigInteger.ONE
    }
}

public final class JavaClass {
    public static void main(String... args) {
        System.out.println(KotlinClass.INTEGER_ONE);
        System.out.println(KotlinClass.Companion.getBIG_INTEGER_ONE());
    }
}

//错误写法--@JvmStatic
class KotlinClass {
    companion object {
        const val INTEGER_ONE = 1
        @JvmStatic val BIG_INTEGER_ONE = BigInteger.ONE
    }
}

public final class JavaClass {
    public static void main(String... args) {
        System.out.println(KotlinClass.INTEGER_ONE);
        //这实际是调用了get()方法
        System.out.println(KotlinClass.getBIG_INTEGER_ONE());
    }
}
```

```kotlin
//正确写法
class KotlinClass {
    companion object {
        const val INTEGER_ONE = 1
        @JvmField val BIG_INTEGER_ONE = BigInteger.ONE
    }
}

public final class JavaClass {
    public static void main(String... args) {
        System.out.println(KotlinClass.INTEGER_ONE);
        System.out.println(KotlinClass.BIG_INTEGER_ONE);
    }
}
```



### Idiomatic naming--@JvmName

Kotlin 与 Java 有不同的调用约定，它可以改变您命名函数的方式。 使用@ JvmName 来设计名称，这样他们就会感觉两种语言的惯例都很习惯，或者匹配各自的标准库命名。 对于扩展函数和扩展属性来说，这种情况最常见，因为接收器类型的位置不同。

```kotlin
sealed class Optional
data class Some(val value: T): Optional()
object None : Optional()

@JvmName("ofNullable")
fun  T?.asOptional() = if (this == null) None else Some(this)

// FROM KOTLIN:
fun main(vararg args: String) {
    val nullableString: String? = "foo"
    val optionalString = nullableString.asOptional()
}

// FROM JAVA:
public static void main(String... args) {
    String nullableString = "Foo";
    Optional optionalString =
  				//注意这里，改变了Java调用该方法时的方法名
          Optionals.ofNullable(nullableString);
}
```



### Function overloads for defaults--@JvmOverloads

```kotlin
//错误写法
class Greeting {
    fun sayHello(prefix: String = "Mr.", name: String) {
        println("Hello, $prefix $name")
    }
}
//导致不能省略默认参数
public class JavaClass {
    public static void main(String... args) {
        Greeting greeting = new Greeting();
        greeting.sayHello("Mr.", "Bob");
    }
}
```

```kotlin
//正确写法
class Greeting {
    @JvmOverloads
    fun sayHello(prefix: String = "Mr.", name: String) {
        println("Hello, $prefix $name")
    }
}

public class JavaClass {
    public static void main(String... args) {
        Greeting greeting = new Greeting();
        greeting.sayHello("Bob");
    }
}
```





## Kotlin和Java映射类型

Kotlin和Java类型的映射方式不同，但它们都能映射到相应的类型。 这些类型的映射仅在编译时才有意义，运行时保持不变。

Java的原始类型对应的Kotlin类型

| Java类型  | Kotlin类型       |
| --------- | ---------------- |
| `byte`    | `kotlin.Byte`    |
| `short`   | `kotlin.Short`   |
| `int`     | `kotlin.Int`     |
| `long`    | `kotlin.Long`    |
| `char`    | `kotlin.Char`    |
| `double`  | `kotlin.Double`  |
| `boolean` | `kotlin.Boolean` |

Java非原始类型对应的Kotlin类型

| Java类型                 | Kotlin类型             |
| ------------------------ | ---------------------- |
| `java.lang.Object`       | `kotlin.Any!`          |
| `java.lang.Cloneable`    | `kotlin.Cloneable!`    |
| `java.lang.Comparable`   | `kotlin.Comparable!`   |
| `java.lang.Enum`         | `kotlin.Enum!`         |
| `java.lang.Annotation`   | `kotlin.Annotation!`   |
| `java.lang.Deprecated`   | `kotlin.Deprecated!`   |
| `java.lang.CharSequence` | `kotlin.CharSequence!` |
| `java.lang.String`       | `kotlin.String!`       |
| `java.lang.Number`       | `kotlin.Number!`       |
| `java.lang.Throwable`    | `kotlin.Throwable!`    |

Java盒装原始类型和相应的可空Kotlin类型

| Java类型              | Kotlin类型        |
| --------------------- | ----------------- |
| `java.lang.Byte`      | `kotlin.Byte?`    |
| `java.lang.Short`     | `kotlin.Short?`   |
| `java.lang.Integer`   | `kotlin.Int?`     |
| `java.lang.Long`      | `kotlin.Long?`    |
| `java.lang.Character` | `kotlin.Char?`    |
| `java.lang.Float`     | `kotlin.Float?`   |
| `java.lang.Double`    | `kotlin.Double?`  |
| `java.lang.Boolean`   | `kotlin.Boolean?` |

Java集合类型和相应的只读或可变Kotlin类型

| Java类型          | Kotlin只读类型    | Kotlin可变类型                  |
| ----------------- | ----------------- | ------------------------------- |
| `Iterator<T>`     | `Iterator<T>`     | `MutableIterator<T>`            |
| `Iterable<T>`     | `Iterable<T>`     | `MutableIterable<T>`            |
| `Collection<T>`   | `Collection<T>`   | `MutableCollection<T>`          |
| `Set<T>`          | `MutableSet<T>`   | `MutableSet<T>`                 |
| `List<T>`         | `MutableList<T>`  | `MutableList<T>`                |
| `ListIterator<T>` | `ListIterator<T>` | `MutableListIterator<T>`        |
| `Map<K, V>`       | `Map<K, V>`       | `MutableMap<K, V>`              |
| `Map.Entry<K, V>` | `Map.Entry<K, V>` | `MutableMap.MutableEntry<K, V>` |