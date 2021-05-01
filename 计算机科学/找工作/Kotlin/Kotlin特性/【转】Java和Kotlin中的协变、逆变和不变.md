[TOC]

## 前言

泛型（Generics）的型变是Java中比较难以理解和使用的部分，“神秘”的通配符，让我看了几遍《Java编程思想》之后仍不明所以，直到最近学习了Kotlin，才对泛型型变有了更多的理解。这篇文章包括以下内容：

- 什么是泛型的型变（协变、逆变、不型变）
- 为什么需要泛型的型变
- Java和Kotlin分别是如何处理泛型型变的

如果你不了解Kotlin也没有关系，只看Java部分也可以。

------

## 0. 几个概念

1. **type variance（型变）**【类型之间的关系】

> Type variance refers to the techniques by which we can allow, or not allow, subtyping in our parameterized types.

型变是指我们是否允许对参数类型进行子类型转换。不明白没关系，以上仅是为了提升文章逼格的。举个例子你就明白了，假设Orange类是Fruit类的子类，Crate\<T> 是一个泛型类，那么，Crate\<Orange> 是 Crate\<Fruit> 的子类型吗？直觉可能告诉你，Yes。但是，答案是No。对于Java而言，两者没有关系。对于Kotlin而言，Crate\<Orange>可能是Crate\<Fruit>的子类型，或者其超类型，或者两者没有关系，**这取决于Crate\<T>中的 T 在类Crate中是如何使用的**。简单来说，型变就是指 Crate\<Orange> 和 Crate\<Fruit> 是什么关系这个问题，对于不同的答案，有如下几个术语。

1. **invariance（不型变）**：也就是说，Crate\<Orange> 和 Crate\<Fruit> 之间没有关系。
2. **covariance（协变）**：也就是说，Crate\<Orange> 是 Crate\<Fruit> 的子类型。
3. **contravariance（逆变）**：也就是说，Crate\<Fruit> 是 Crate\<Orange> 的子类型。



![img](https:////upload-images.jianshu.io/upload_images/4803763-49e1b847646a94db.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/599)

不型变、协变和逆变

```java
注意
- 上面在解释协变、逆变概念时的说法只是为了帮助理解，这种说法对于Java而言并不准确。在Java中，Crate<Orange> 和 Crate<Fruit> 永远没有关系，对于协变应该这么说， Crate<Orange> 是 Crate<? extends Fruit> 的子类型，逆变则是，Crate<Fruit> 是 Crate<? super Orange> 的子类型。
- **子类（subclass）**和 **子类型（subtype）**不是一个概念，子类一定是子类型，子类型不一定是子类，例如，Crate<Orange> 是 Crate<? extends Fruit> 的子类型，但是Crate<Orange> 并不是 Crate<? extends Fruit> 的子类。
```

那么为什么需要型变呢？根本目的是在保证泛型类 类型安全的基础上，提高API的灵活性，手段是通过编译器限制泛型类上某些方法的调用。（看完这篇文章后，你会理解这句话在说什么。）

------

## 1. Java的做法

Java处理型变的做法概括起来是：**Java中的泛型类在正常使用时是不型变的，要想型变必须在使用处通过通配符进行（称为使用处型变）。**

Java中的泛型是不型变的。看如下代码，在Java中是**无法编译**的：

```java
List<String> strs = new ArrayList<String>();
List<Object> objs = strs; // ！！！即将来临的问题的原因就在这里。Java 禁止这样！
objs.add(1); // 这里我们把一个整数放入一个字符串列表
String s = strs.get(0); // ！！！ ClassCastException：无法将整数转换为字符串
```

Java禁止这么做，理由如代码中所述，**主要目的是为了保证运行时的类型安全**。但是这么一棒子打死，禁止型变，也会带来一些别的影响。如上例，如果我们只是在objs上调用get方法，而不调用add方法（只读取数据不写入数据），这显然不会有类型安全的问题。问题在于如何保证我们只调用get而不调用add呢，不能只靠我们的自觉吧，最好有编译器的限制。通配符就是干这件事的，通知编译器，限制我们对于某些方法的调用，以保证运行时的类型安全。

### 1.1 Java中的协变

以最常用的List类为例，协变如下：

```java
List<? extends Fruit> fruits = new ArrayList<Orange>();
//编译错误:不能添加任何类型的对象
//fruits.add(new Orange());
//fruits.add(new Fruit());
//fruits.add(new Object());
fruits.add(null);//可以这么做，但是没有意义
//我们知道，返回值肯定是Fruit
Fruit f = fruits.get(0);
```

```java
fruits的类型是List<? extends Fruit>，代表Fruit类型或者从Fruit继承的类型的List，fruits可以引用诸如Fruit或Orange这样类型的List，然后向上转型为了List<? extends Fruit>。我们并不关心fruits具体引用的是ArrayList<Orange>()，还是ArrayList<Fruit>()，对于类型 List<? extends Fruit> 我们所能知道的就是：调用一个返回Fruit的方法是安全的，因为你知道，这个List中的任何对象至少具有Fruit类型。

我们之所以可以安全地将 ArrayList\<Orange> 向上转型为 List<? extends Fruit>，是因为编译器限制了我们对于 List<? extends Fruit> 类型部分方法的调用。例如void add(T t)方法，以及一切参数中含有 T 的方法（称为消费者方法），因为这些方法可能会破坏类型安全，只要限制这些方法的调用，就可以安全地将 ArrayList<Orange> 转型为 List<? extends Fruit>。这就是所谓的协变，通过限制对于消费者方法的调用，使得像 List<? extends Fruit> 这样的类型成为单纯的“生产者”，以保证运行时的类型安全。
```

那么编译器是如何决定哪些方法可以调用，哪些方法不可以呢？例如，我们显然可以在fruits上调用contains方法：

```
fruits.contains(new Orange());//OK！因为contains的参数是Object
```

显然，List中的contains方法并没有修改List中的对象，那是不是说编译器帮我们检查了某个方法是否修改了泛型类中的对象，然后决定我们是否可以在协变中调用它？答案是编译器并没有那么聪明，一切取决于方法的签名。对于List中void add(T t)方法，当你指定类型是List<? extends Fruit>时，add(T t)的参数就变成了“? extends Fruit”，从这个参数中，编译器并不知道需要哪个具体的Fruit子类型，Orange、Banana甚至Fruit都可以，因此，为了保证类型安全，编译器拒绝任何类型。而boolean contains(Object o)的参数其实是Object，编译器当然不会限制对它的调用。

这里面其实还有个问题，contains方法其实是可以写成更具体的形式：boolean contains(T t)，毕竟List最主要的特点就是类型安全，有什么理由去询问一个类型不一样的对象是不是包含在List中的呢？但是，如果这么做，编译器会像拒绝add一样拒绝contains方法的调用，尽管我们确信在contains方法内部并不会修改List中的对象（因此不会有类型安全的问题）。在Java中我们没有办法解决这个问题，因此，只能写成boolean contains(Object o)。不过，没关系，不是还有Kotlin么。

### 1.2 Java中的逆变

协变的反方向是逆变，在协变中我们可以安全地从泛型类中读取（从一个方法中返回），而在逆变中我们可以安全地向泛型类中写入（传递给一个方法）。

```java
List<Object> objs = new ArrayList<Object>();
objs.add(new Object());
List<? super Fruit> canContainFruits = objs;
//没有问题，可以写入Fruit类及其子类
canContainFruits.add(new Orange());
canContainFruits.add(new Banana());
canContainFruits.add(new Fruit());
//无法安全地读取,canContainFruits完全可能包含Fruit基类的对象，比如这里的Object
//Fruit f = canContainFruits.get(0);

//总是可以读取为Object，然而这并没有太多意义
Object o = canContainFruits.get(1);
```

canContainFruits的类型是List<? super Fruit>，代表Fruit类型或者Fruit基类型的List，canContainFruits可以引用诸如Fruit或Object这样类型的List，然后向上转型为了List<? super Fruit>。
再次考虑，为什么编译器会“半拒绝”在 List<? super Fruit> 上调用get方法。对于List中的 T get(int pos) 方法，当指定类型是 “？ super Fruit” 时，get方法的返回类型就变成了 “？ super Fruit”，也就是说，返回类型可能是Fruit或者任意Fruit的基类型，我们不能确定，因此编译器拒绝调用任何返回类型为 T 的方法（除非我们只是读取为Object类）。**注意**，这次拒绝的理由跟协变中是不一样的。get方法并不会破坏泛型类的类型安全，主要原因在于我们不能确定get的返回类型。
对于类型 List<? super Fruit> 我们所能知道的就是：向一个方法传入Fruit及其子类（Orange、Banana）是安全的，因为你知道，这个List包含的是Fruit或者Fruit基类的对象。

> 类似的，编译器限制了我们对于 List<? super Fruit> 类型部分方法的调用。例如T get(int pos)方法，以及一切返回类型为 T 的方法（称为生产者方法），因为我们不能确定这些方法的返回类型，只要限制这些方法的调用，就可以安全地将 ArrayList\<Object> 转型为 List<? super Fruit>。这就是所谓的逆变，通过限制对于生产者方法的调用，使得像 List<? super Fruit> 这样的类型成为单纯的“消费者”。

### 1.3 Java型变总结

extends限定了通配符类型的上界，所以我们可以安全地从其中读取；而super限定了通配符类型的下界，所以我们可以安全地向其中写入。
我们可以把那些只能从中**读取**的对象称为**生产者（Producer）**，我们可以从生产者中安全地读取；只能**写入**的对象称为**消费者（Consumer）**。因此可以这么说：*Producer-Extends, Consumer-Super*。

------

## 2. Kotlin的做法

Kotlin处理型变的做法概括起来是：Kotlin中的泛型类在定义时即可标明型变类型（协变或逆变，当然也可以不标明，那就是不型变的），在使用处可以直接型变（称为声明处型变）。因为Kotlin与Java是100%兼容的，你自己在Kotlin中定义的泛型类当然可以享受声明处型变的方便，但是，如果引入Java库呢？又或者你自己在Kotlin中定义的泛型类恰好是不型变的，然而你又想像Java那样在使用处型变，该这么办呢？Kotlin使用一种称为 **类型投影（type projections）**的方式来处理这种型变。这种方式其实跟Java处理型变的方式类似，只是换了一种说法，还是使用处型变。

|                    | 协变                             | 逆变                           | 不变                 |
| ------------------ | -------------------------------- | ------------------------------ | -------------------- |
| 基本结构           | `Producer<out E>`                | `Consumer<in T>`               | `MutableList<T>`     |
| 子类型化关系       | 保留子类型化关系                 | 反转子类型化关系               | 无子类型化关系       |
| 有无型变点         | 协变点out                        | 逆变点in                       | 无型变点             |
| 类型形参存在的位置 | 修饰只读属性类型和函数返回值类型 | 修饰可变属性类型和函数形参类型 | 都可以，没有约束     |
| 角色               | 生产者输出为泛型形参类型         | 消费者输入为泛型形参类型       | 既是生产者也是消费者 |
| 表现特征           | 内部操作只读                     | 内部操作只写                   | 内部操作可读可写     |

### 2.1 Kotlin的小聪明

Java处理型变的问题在于：把一切都推给了使用处，增加了不明所以的通配符，代码可读性变差，并且很丑陋。那么，除了使用处，我们还可以在哪做点改进呢？请看下面的例子：

```java
// Java
interface Source<T> {
    T nextT();
}
```

这个接口是一个“生产者”，泛型参数 T 只作为返回类型，没有任何把 T 作为参数的“消费者”方法。那么，将Source\<Orange> 转型为 Source\<Fruit> ⽤是极为安全的，毕竟没有消费者方法可以调用。但是 Java 并不知道这⼀点，并且仍然禁止这样操作：

```java
// Java
void demo(Source<Orange> oranges) {
    Source<Fruit> fruits = oranges; // ！！！在 Java 中不允许
}
```

为了修正这⼀点，我们必须声明对象的类型为 Source<? extends Fruit>，这是毫无意义的，因为我们可以像以前⼀样在该对象上调用所有相同的方法，所以更复杂的类型并没有带来价值。问题的关键在于，Java中的泛型是不型变的，原因是类中的方法可能会“干坏事”，破坏了类型安全，但是，这是不是有点矫枉过正了呢？！就如同上述接口一样，我们定义的时候就可以保证它不会“干坏事”（保证它只是生产者或者消费者），问题是编译器并不知道啊！
这么说，问题就简单了，我们只需要告诉编译器，我们定义的类是协变的还是逆变的，或者两者都不是（即不型变的）。这样就可以在声明处定义型变，使用处不需要额外的处理直接使用。为此，Kotlin提供了in和out修饰符。

### 2.2 Kotlin中的协变

```kotlin
//kotlin
abstract class Source<out T> {
    abstract fun nextT(): T
}

fun demo(oranges: Source<Orange>) {
    val fruits: Source<Fruit> = oranges // 没问题，因为 T 是一个 out-参数，Source<T>是协变的
    val oneFruit: Fruit = fruits.nextT() //可以安全读取
}
```

使用out修饰符，表明类型参数 T 在泛型类中仅作为方法的返回值，不作为方法的参数，因此，这个泛型类是个协变的。回报是，使用时Source\<Orange>可以作为Source\<Fruit>的子类型。

还记不记得我们在 *Java中的协变*最后提出的问题，在协变的泛型类中，如果有方法需要将类型参数 T 用作参数，但是可以确定在该方法内部并没有向泛型类写入数据（因此该泛型类仍然只是生产者，不会有类型安全的问题），我们是否仍然可以将该泛型类标记为协变的？ 换种简单的说法，类型参数 T 标记为out，那么 T 是否可以既作为方法的返回值，也作为方法的参数呢？ 这其实是可以的。如下：

```kotlin
//kotlin
public interface Collection<out E> : Iterable<E> { 
    ... 
    public operator fun contains(element: @UnsafeVariance E): Boolean 
    ... 
 } 
```

使用注解 @UnsafeVariance 可以让编译器放我们一马，它是在告诉编译器，我保证这个方法不会向泛型类写入数据，你放心。

### 2.3 Kotlin中的逆变

逆变类的⼀个很好的例子是Kotlin中的Comparable：

```kotlin
//kotlin
abstract class Comparable<in T> {
    abstract fun compareTo(other: T): Int
}

fun demo(x: Comparable<Number>) {
    val y: Comparable<Double> = x // OK！逆变，Comparable<Number>可以作为Comparable<Double>的子类型
    y.compareTo(1.0) //1.0 拥有类型 Double
}
```

y的声明类型是Comparable\<Double>，引用的实际类型是Comparable\<Number>，向compareTo(Number)中传入Double当然没什么问题。Double是Number的子类，但是对于泛型类而言，Comparable\<Number> 却是 Comparable\<Double> 的子类型，所以这称为**逆变**。
使用in修饰符，表明类型参数 T 在泛型类中仅作为方法的参数，不作为方法的返回值，因此，这个泛型类是个逆变的。回报是，使用时Comparable\<Number>可以作为Comparable\<Double>的子类型。

**总结**：out和in修饰符是自解释的。out代表泛型类中，类型参数 T 只能存在于方法的返回值中，即是作为**输出**，因此，泛型类是**生产者/协变的**；in代表泛型类中，类型参数T只能存在于方法的参数中，即是作为**输入**，因此，泛型类是**消费者/逆变的**。如果在泛型类中，类型参数 T 既存在于方法的参数中，又存在于方法的返回值中，那么我们不能对 T 做标记（除了上面提到的 @UnsafeVariance 的情况），也就是说该泛型类是不型变的，这跟Java类似。

**注意**：以上所说的in/out修饰符对于类型参数 T 的限制，仅适用于非private（public, protected, internal）函数，对于private函数，类型参数 T 可以存在于任意位置，毕竟private函数仅用于内部调用，不会对泛型类的协变、逆变性产生影响。还有一点例外就是，如果类型参数 T 标记为out，我们仍可以在构造函数的参数中使用它，因为构造函数仅用于实例化，之后不能被调用，所以也不会破坏泛型类的协变性。

> 对于Kotlin而言，可以这么说：Consumer in, Producer out

### 2.4 Kotlin中的集合类

在Kotlin中，可以在泛型类定义时就标明其是协变的、逆变的还是不型变的。这也就是为什么Kotlin中的集合类分为可变的（例如MutableList）和不可变的（例如List），因为只有不可变的集合我们才能标明是协变（out）的。看如下定义：

```kotlin
public interface Collection<out E> : Iterable<E> { 
    public val size: Int
    public fun isEmpty(): Boolean
    public operator fun contains(element: @UnsafeVariance E): Boolean
    override fun iterator(): Iterator<E>
    public fun containsAll(elements: Collection<@UnsafeVariance E>): Boolean
 }
```

因为Collection是不变的，也就是没有方法向其中写入数据，所以我们可以将其定义为协变的。因此，Collection\<Orange> 可以作为 Collection\<Fruit> 的子类型：

```kotlin
fun demo(oranges: Collection<Orange>) {
    val fruits: Collection<Fruit> = oranges
}
```

而可变的集合的定义如下：

```kotlin
public interface MutableCollection<E> : Collection<E>, MutableIterable<E> {
    override fun iterator(): MutableIterator<E>
    //向集合中添加或删除元素，显然 MutableCollection不能是协变的
    public fun add(element: E): Boolean
    public fun remove(element: E): Boolean
    
    public fun addAll(elements: Collection<E>): Boolean
    public fun removeAll(elements: Collection<E>): Boolean
    public fun retainAll(elements: Collection<E>): Boolean
    public fun clear(): Unit
}
```

可变集合增加了以 E 作为参数的方法，因此不再是协变的了（当然也不是逆变的）。也就是说，MutableCollection\<Orange> 和 MutableCollection\<Fruit> 之间没有关系。

### 2.5 类型投影

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

### 2.6 类型参数的命名约定

类型参数一般使用一个大写的字母表示，经常使用的类型参数的名称有：

- E: Element（广泛的用于Java Collection中）
- K: Key
- N: Number
- T: Type
- V: Value
- S,U,V: 第2, 3, 4个类型参数

## 3. 对比与总结

总体而言，Java和Kotlin中的泛型还是比较相像的。对于使用处型变，两者几乎等价，只是表现形式不同，Kotlin看上去更加简洁一些，它们都是通过编译器限制我们对一些方法的调用来实现的。Kotlin相较于Java中的泛型，最主要的提升在于，声明处型变，即在泛型类定义时就可以把其声明为协变（out）的，或者逆变（in）的。总而言之，Kotlin是以更加简洁、灵活而严格的方式实现了泛型。

