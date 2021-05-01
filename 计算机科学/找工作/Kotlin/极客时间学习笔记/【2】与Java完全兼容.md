[TOC]

### 语法变化

#### 函数定义

函数可以直接写在文件里，而不用写在类里，因为会生成`public static` 方法

```kotlin
fun sayHello(str: String){
    print("hello : $str")
}

//java调用
public class MainJava {
    public static void main(String[] args) {
        UtilsCkKt.sayHello("ck");
    }
}

//kotlin调用
fun main(args: Array<String>) {
    sayHello("ck")
}
```

#### 与Java交互

Test在此是一个匿名内部类，object关键字在此是创建一个匿名内部类，构造函数被改写成私有，这是在kotlin中声明单例的一种方法

```kotlin
object Test{
    fun sayHello(str :String){
        print(str)
    }
}

//kotlin
Test.sayHello("ck")
//java
Test.INSTANCE.sayHello("ck");
```

因为kotlin与java的**class文件格式不一样**，所以互相调用的时候也不是简单的.class

```kotlin
TestMain.class		//java传class
TestMain::class.java	//kotlin传class
//JavaMain.java
public class JavaMain {
}
//KotlinMain.kt
class KotlinMain {

}
//ck.kt
import kotlin.reflect.KClass

fun main(args: Array<String>) {
    testMain(JavaMain::class.java)
    testMain(KotlinMain::class)
}

fun testMain(clazz:Class<JavaMain>){
    print(clazz.simpleName)
}

fun testMain(clazz: KClass<KotlinMain>){
    print(clazz.simpleName)
}
/////////输出
JavaMain
KotlinMain
```





### 关键字处理

> 例如in在kotlin中是关键字，这几句会引起冲突

```kotlin
//java定义
public static final String in="in";
//kotlin使用
JavaMain.`in`
```



### 基本数据类型的处理

- kotlin**没有封装类**，Java封装类在Kotlin中都只有唯一一种对应关系

Kotlin 的基本数值类型包括 Byte、Short、Int、Long、Float、Double 等。不同于Java的是，字符不属于数值类型，是一个独立的数据类型。 

| 类型   | 位宽度 |
| ------ | ------ |
| Double | 64     |
| Float  | 32     |
| Long   | 64     |
| Int    | 32     |
| Short  | 16     |
| Byte   | 8      |

Kotlin 中没有基础数据类型，只有封装的数字类型，你每定义的一个变量，其实 Kotlin 帮你封装了一个对象，这样可以保证不会出现空指针。数字类型也一样，所有在比较两个数字的时候，就有比较数据大小和比较两个对象是否相同的区别了。

在 Kotlin 中，三个等号 === 表示比较对象地址，两个 == 表示比较两个值大小。

```kotlin
fun main(args: Array<String>) {
    val a: Int = 10000
    println(a === a) // true，值相等，对象地址相等

    //经过了装箱，创建了两个不同的对象
    val boxedA: Int? = a
    val anotherBoxedA: Int? = a

    //虽然经过了装箱，但是值是相等的，都是10000
    println(boxedA === anotherBoxedA) //  false，值相等，对象地址不一样
    println(boxedA == anotherBoxedA) // true，值相等
}
```

