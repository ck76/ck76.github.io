[TOC]

### 1.尽可能的少用 `!!`

首先需要介绍是`!!`操作符。

`!!` 操作符：`这是为空指针爱好者准备的`，非空断言运算符（!!）将任何值转换为非空类型，若该值为空则抛出异常。我们可以写 a!! ，这会返回一个非空的 a 值 （例如：在我们例子中的 String）或者如果 a 为空，就会抛出一个 空指针 异常：

```kotlin
val b = a!!.length
```

> 所以，我们能不用`!!`操作符就不要用。。。

下面介绍几种方式避免使用`!!`操作符

#### 1).多用 val 而不是 var

在 Kotlin 中 `val`代表只读，`var`代表可变。建议尽可能多的使用`val`。`val`是线程安全的，并且必须在定义时初始化，所以不需要担心 null 的问题。只需要注意 val 在某些情况下也是可变的就行了。

> val 和 var 是用于表示属性是否有 getter/setter：
>
> - var：同时有 getter 和 setter
> - val：只有 getter

所以，强烈推荐能用`val`的地方就用`val`。

#### 2).使用 lateinit

有些时候并不能用val，比如在`spring boot`接口测试中就不能使用`val`,对于这种情况，可以使用 lateinit 关键字。。

> 依赖倒转，对象的创建是通过spring完成的，而val要求定义的时候初始化

```kotlin
@RunWith(SpringRunner::class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class ApplicationTests {
    val log = LogFactory.getLog(ApplicationTests::class.java)!!
    @Autowired
    lateinit var restTemplate: TestRestTemplate
    
    @Test
    fun `GET when given quanke then returns "Hello, quanke"`() {
        // Given
        val name = "quanke"
        // When
        val body = restTemplate.getForObject("/users/hello/{name}", String::class.java, name)
        // Then
        assertThat(body).isEqualTo("Hello, $name")
    }
}
```

> 注意:lateinit很好用，但也有坑
>
> - 访问未初始化的 lateinit 属性会导致 UninitializedPropertyAccessException。
> - lateinit 不支持基础数据类型，比如 Int。对于基础数据类型，我们可以这样：

```kotlin
private var mNumber: Int by Delegates.notNull<Int>()
>
```

#### 3).Elvis 操作符

当b为可空引用时，我们可以使用if表达式处理

```kotlin
val l: Int = if (b != null) b.length else -1
```

但更加优雅的方式是使用Elvis 操作符`?:`

```kotlin
val l = b?.length ?: -1
```

如果 ?: 左侧表达式非空，elvis 操作符就返回其左侧表达式，否则返回右侧表达式。

> 注意:当且仅当左侧为空时，才会对右侧表达式求值。

#### 4).也许可以尝试一下let函数

```kotlin
let`函数一般与安全调用操作符一起使用,我们首先介绍安全调用操作`?.
b?.length
```

如果 b 非空，就返回 b.length，否则返回 null，这个表达式的类型是 Int?。

安全调用在链式调用中很有用。例如，如果一个员工Quanke可能会（或者不会）分配给一个部门， 并且可能有另外一个员工是该部门的负责人，那么获取 Quanke 所在部门负责人（如果有的话）的名字，我们写作：

```kotlin
quanke?.department?.head?.name
```

如果任意一个属性（环节）为空，这个链式调用就会返回 null。

如果要只对非空值执行某个操作，安全调用操作符可以与 let 一起使用：

```kotlin
val listWithNulls: List<String?> = listOf("A", null)
for (item in listWithNulls) {
     item?.let { println(it) } // 输出 A 并忽略 null
}
```

还有一种常见的错误（放ide里面试试就知道什么错误了）：

```kotlin
private var a: String? = null
fun aLetDemo() {
    if (a != null) {
        test(a)
    }
}
```

我们可以这样：

```kotlin
private var a: String? = null
fun aLetDemo() {
    if (a != null) {
        test(a!!)
    }
}
```

但是这样的后果就是你还是需要在test函数里处理空指针。

我们充分利用`?.`加`let`的特点，更加优雅的解决这个编译错误，如下

```kotlin
private var a: String? = null
fun aLetDemo() {
    if (a != null) {
        a?.let {
            test(it)
        }
    }
}
```

### 2.少写点Util类和继承

很多时候框架提供给我的方法是比较原子，或者某些常用的方法框架并没有提供，Java一般是写一个工具类：

```kotlin
public final class StringUtil {
    /**
     * 删除所有的标点符号
     *
     * @param str 处理的字符串
     */
    public  static String trimPunct(String str) {
        if(isEmpty(str)){
            return "";
        }
        return str.replaceAll("[\\pP\\p{Punct}]", "");
    }
}
```

Kotlin可以通过扩展函数的形式实现：

```kotlin
/**
 * 删除所有的标点符号
 *
 * @param str 处理的字符串
 */
fun String.trimPunct(): String {
    return if (this.isEmpty()) {
        ""
    } else this.replace("[\\pP\\p{Punct}]".toRegex(), "")
}
```

调用：

```kotlin
fun main(args: Array<String>) {
    val a = "把我的标点符号去掉吧，全科。"
    print(a.trimPunct())
}
```

打印：

```kotlin
把我的标点符号去掉吧全科
Process finished with exit code 0
```

### 3.别再用+号拼接字符串

无论是Java还是Android开发，我们都会用到字符串拼接，比如进行日志输出等等。在Kotlin中，支持字符串模板，我们可以很轻松的完成一个字符串数的拼接，当然你可能会说使用StringBuilder性能更好，比如：

```kotlin
val site = "http://woquanke.com"
val sb: StringBuilder = StringBuilder()
sb.append("我的博客名字叫《我全科》，我的博客地址是：")
sb.append(site)
println(sb.toString())
```

但kotlin的字符串模版可以优雅的做这个事情：

```kotlin
val site = "http://woquanke.com"
println("我的博客名字叫《我全科》，我的博客地址是：$site")
```

### 4.也许可以忘记getters/setters了

我们经常创建一些只保存数据的类。在这些类中，一些标准函数往往是操作一下ide生成的。在 Kotlin 中，这叫做 数据类 并标记为 data：

```kotlin
data class User(val name: String, val age: Int)
```

data class 自动生成getter，setting，hashcode和equals等方法

### 5.请忘记三元运算符

在 Kotlin 中，if是一个表达式，即它会返回一个值。因此就不需要三元运算符（条件 ? 然后 : 否则），因为普通的 if 就能胜任这个角色。

```kotlin
// 作为表达式
val max = if (a > b) a else b
```

### 6.哪里还有switch

when 取代了类java 语言的 switch 操作符。其最简单的形式如下：

```kotlin
when (x) {
    1 -> print("x == 1")
    2 -> print("x == 2")
    else -> { // 注意这个块
        print("x is neither 1 nor 2")
    }
}
```

如果很多分支需要用相同的方式处理，则可以把多个分支条件放在一起，用逗号分隔：

```kotlin
when (x) {
    0, 1 -> print("x == 0 or x == 1")
    else -> print("otherwise")
}
```

可以用任意表达式（而不只是常量）作为分支条件

```kotlin
when (x) {
    parseInt(s) -> print("s encodes x")
    else -> print("s does not encode x")
}
```

也可以检测一个值在（in）或者不在（!in）一个区间或者集合中：

```kotlin
when (x) {
    in 1..10 -> print("x is in the range")
    in validNumbers -> print("x is valid")
    !in 10..20 -> print("x is outside the range")
    else -> print("none of the above")
}
```

另一种可能性是检测一个值是（is）或者不是（!is）一个特定类型的值。注意： 由于智能转换，你可以访问该类型的方法和属性而无需任何额外的检测。

```kotlin
fun hasPrefix(x: Any) = when(x) {
    is String -> x.startsWith("prefix")
    else -> false
}
```

when 也可以用来取代 if-else if链。 如果不提供参数，所有的分支条件都是简单的布尔表达式，而当一个分支的条件为真时则执行该分支：

```kotlin
when {
    x.isOdd() -> print("x is odd")
    x.isEven() -> print("x is even")
    else -> print("x is funny")
}
```

### 7.去你的ClassCastException

#### Kotlin智能转换(Smart Casts)

对于不可变的值,Kotlin一般不需要显式转换对象类型,编译器能跟踪is检查类型,在需要时会自动插入类型转换代码(安全):

```kotlin
fun classCast(a: Any) {
    if (a is String) {
        print(a.length) //编译器自动把a转换为String类型
    }
}
```

Kotlin编译器很聪明,能识别反向检查类型!is操作符,会自动插入类型转换代码：

```kotlin
if (a !is String) return
    print(a.length) //编译器自动把x转换为String类型：
    // ||右侧, a自动转换为String类型
    if (a !is String || a.length == 0) return
    // &&右侧, a自动转换为String类型
    if (a is String && a.length > 0) {
        print(a.length) // a 自动转换为字符串
    }
    //智能转换(smart casts)也用于when表达式和while循环
    when (a) {
        is Int -> print(a + 1)
        is String -> print(a.length + 1)
        is IntArray -> print(a.sum())
    }
```

如果不能保证变量在类型检查`is`/`!is`操作符和变量使用之间不可改变时,智能转换不能用。智能转换的适用条件或规则:

- val局部变量-总是适用!
- val属性-适用于private或internal,或者类型检查is/!is在声明属性的同一模块中执行;
- 不适用于open的属性,或者具有自定义getter的属性!
- var局部变量—适用于变量在类型检查和使用之间没有修改,且不在修改它的lambda中捕获!
- var属性-不适用(因为该变量可随时被修改)

#### 安全(可空)转换-操作符as?

为避免抛出异常,可用安全转换操作符`as?`,在失败时返回null

```
val a: String? = b as? String
```

尽管`as?`右边是一个非空类型String,但是`as?`转换失败时返回可空(null)，换句话说就是,`as?`函数参数String不能为null,但是as?函数的返回值可以是null

### 8.真的要习惯Koltin的for循环，太强大了

Kotlin没有Java中的for(初始值;条件；增减步长)这个规则。但是Kotlin中对于for循环语句新增了其他的规则，来满足刚提到的规则。

- for循环提供迭代器用来遍历任何东西
- for循环数组被编译为一个基于索引的循环，它不会创建一个迭代器对象

#### 新增的规则，去满足for(初始值;条件;增减步长)这个规则

##### 递增

关键字：until
范围：until[n,m) => 即大于等于n,小于m

例：

```kotlin
// 循环5次，且步长为1的递增
for (i in 0 until 5){
  print("i => $i \t")
}
```

输出结果为

```kotlin
i => 0  i => 1  i => 2  i => 3  i => 4
```

##### 递减

关键字：downTo
范围：downTo[n,m] => 即小于等于n,大于等于m ,n > m
例：

```kotlin
// 循环5次，且步长为1的递减
for (i in 15 downTo 11){
    print("i => $i \t")
}
```

输出结果为：

```kotlin
i => 15     i => 14     i => 13     i => 12     i => 11
```

##### 符号（’ .. ‘） 表示递增的循环的另外一种操作

使用符号( ‘..’).
范围：..[n,m]=> 即大于等于n，小于等于m
和until的区别，一是简便性。二是范围的不同。

```kotlin
print("使用 符号`..`的打印结果\n")
for (i in 20 .. 25){
    print("i => $i \t")
}
println()
print("使用until的打印结果\n")
for (i in 20 until 25){
    print("i => $i \t")
}
```

输出结果为：

使用 符号`..`的打印结果

```kotlin
i => 20     i => 21     i => 22     i => 23     i => 24     i => 25
```

使用until的打印结果

```kotlin
i => 20     i => 21     i => 22     i => 23     i => 24
```

##### 设置步长

关键字：step

例：

```kotlin
for (i in 10 until 16 step 2){
    print("i => $i \t")
}
```

输出结果为：

```kotlin
i => 10     i => 12     i => 14
```

#### 迭代

for循环提供一个迭代器用来遍历任何东西。
for循环数组被编译为一个基于索引的循环，它不会创建一个迭代器对象

##### 遍历字符串

此用法在数据类型章节中的字符串类型中用到过。还不甚清楚的可以查看 Kotlin——最详细的数据类型介绍。

例：

```kotlin
for (i in "abcdefg"){
    print("i => $i \t")
}
```

输出结果为：

```kotlin
i => a  i => b  i => c  i => d  i => e  i => f  i => g
```

##### 遍历数组

此用法在数据类型章节中的数组类型中用到过。还不甚清楚的可以查看 Kotlin——最详细的数据类型介绍。

例：

```kotlin
var arrayListOne = arrayOf(10,20,30,40,50)
for (i in arrayListOne){
    print("i => $i \t")
}
```

输出结果为：

```kotlin
i => 10     i => 20     i => 30     i => 40     i => 50
```

##### 使用数组的indices属性遍历

例：

```kotlin
var arrayListTwo = arrayOf(1,3,5,7,9)
for (i in arrayListTwo.indices){
    println("arrayListTwo[$i] => " + arrayListTwo[i])
}
```

输出结果为：

```kotlin
arrayListTwo[0] => 1
arrayListTwo[1] => 3
arrayListTwo[2] => 5
arrayListTwo[3] => 7
arrayListTwo[4] => 9
```

##### 使用数组的withIndex()方法遍历

例：

```kotlin
var arrayListTwo = arrayOf(1,3,5,7,9)
for ((index,value) in arrayListTwo.withIndex()){
    println("index => $index \t value => $value")
}
```

输出结果为：

```kotlin
index => 0   value => 1
index => 1   value => 3
index => 2   value => 5
index => 3   value => 7
index => 4   value => 9
```

##### 使用列表或数组的扩展函数遍历

数组或列表有一个成员或扩展函数iterator()实现了Iterator接口，且该接口提供了next()与hasNext()两个成员或扩展函数
其一般和while循环一起使用
可以查看Array.kt这个类。可以看见其中的iterator()函数，而这个函数实现了Iterator接口。

```kotlin
/**
  *   Creates an iterator for iterating over the elements of the array.
  */
public operator fun iterator(): Iterator<T>
```

查看Iterator.kt这个接口类，这个接口提供了hasNext()函数和next()函数。

```kotlin
public interface Iterator<out T> {
/**
  * Returns the next element in the iteration.
  */
public operator fun next(): T
/**
  * Returns `true` if the iteration has more elements.
  */
public operator fun hasNext(): Boolean
}
 
```

例：

```kotlin
var arrayListThree = arrayOf(2,'a',3,false,9)
var iterator: Iterator<Any> = arrayListThree.iterator()
while (iterator.hasNext()){
    println(iterator.next())
}
```

输出结果为：

```kotlin
2
a
3
false
9
```

关于for循环的内容来自[《Kotlin——最详细的控制语句详解》](https://www.cnblogs.com/Jetictors/archive/2017/10/24/7721886.html)

### 9.kotlin stream 真心可以

流式处理给我们的集合操作带来了很大的方便，其实Java 8 一样支持流式处理，我只是想在这里推广一下 stream。

下面举例：

```kotlin
val names = arrayOf("Amy", "Alex", "Bob", "Cindy", "Jeff", "Jack", "Sunny", "Sara", "Steven")  
  
//筛选S开头的人名  
val sName = names.filter { it.startsWith("S") }.toList()  
  
//按首字母分组并排序  
val nameGroup = names.groupBy { it[0] }  
        .toSortedMap( Comparator { key1, key2 -> key1.compareTo(key2) })
```

关于更多流式处理，请自行搜索Java stream

### 10.少写点方法重载

因为kotlin支持默认参数，所以在封装方法时会少很多的方法重载的。

如果没有默认参数的需要实现下面的日志打印，需要写多个方法：

```kotlin
fun log(tag: String, content: String) {
    println("tag:$tag-->$content")
}
fun log( content: String) {
    log("quanke","")
}
```

使用默认参数只需要一个方法：

```kotlin
fun log(tag: String="quanke", content: String) {
    println("tag:$tag-->$content")
}
```