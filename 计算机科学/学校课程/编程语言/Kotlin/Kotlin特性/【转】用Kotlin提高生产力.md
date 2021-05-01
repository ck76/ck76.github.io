[TOC]

#### 1、更简洁的字符串

Kotlin中的字符串基本Java中的类似，有一点区别是加入了三个引号"""来方便长篇字符的编写。 而在Java中，这些都需要转义，先看看java中的式例

```java
    public void testString1() {
        String str1 = "abc";
        String str2 = "line1\n" +
                "line2\n" +
                "line3";
        System.out.println(str1);
        System.out.println(str2);
    }
```

```kotlin
/*
* kotlin对字符串的加强，三个引号"""中可以包含换行、反斜杠等等特殊字符
* */
fun testString() {
    val str1 = "abc"
    val str2 = """line1\n
        line2
        line3
        """
    println(str1)
    println(str2)
}
```

值得注意的是，在Kotlin中，美元符号$是特殊字符，在字符串中不能直接显示，必须经过转义，方法1是用反斜杠，方法二是${'$'}

```kotlin
/*
*Kotlin中，美元符号$是特殊字符，在字符串中不能直接显示，必须经过转义，方法1是用反斜杠，方法二是${'$'}
* */
fun testString3() {
    println("First content is \$strings")
    println("First content is ${'$'}strings")
}
```

---

#### 2、Kotlin中大多数控制结构都是表达式

语句和表达式是什么？

- 表达式有值，并且能作为另一个表达式的一部分使用
- 语句总是包围着它的代码块中的顶层元素，并且没有自己的值

Kotlin与Java的区别

- Java中，所有的控制结构都是语句，也就是控制结构都没有值
- Kotlin中，除了循环（for、do和do/while）以外，大多数控制结构都是表达式(if/when等)

##### if语句

```kotlin
/*
* kotlin中，if 是表达式，不是语句，类似于java中的三木运算符a > b ? a : b
* */
fun max(a: Int, b: Int): Int {
    return if (a > b) a else b
}
```

kotlin中if是表达式有值，完全可以替代，**故kotlin中已没有三元运算符了**，用if来替代。 上面的max函数还可以简化成下面的形式

```kotlin
/*
* kotlin简化版本
* */
fun max2(a: Int, b: Int) = if (a > b) a else b
```

##### when语句

```kotlin
/*
* kotlin中，when是表达式，可以取代Java 中的switch，when的每个分支的最后一行为当前分支的值
* */
fun getPoint(grade: Char) = when (grade) {
    'A' -> "GOOD"
    'B', 'C' -> {
        println("test when")
        "OK"
    }
    'D' -> "BAD"
    else -> "UN_KNOW"
}

/*
* kotlin中，when是表达式，可以取代java的if/else，when的每个分支的最后一行为当前分支的值
* */
fun getPoint2(grade: Int) = when {
    grade > 90 -> "GOOD"
    grade > 60 -> "OK"
    grade.hashCode() == 0x100 -> "STH"
    else -> "UN_KNOW"
}
```

---

#### 3、更好调用的函数：显示参数名/默认参数值

```kotlin
/*
* 打印列表的内容
* */
fun <T> joinToString(collection: Collection<T>,
                     separator: String,
                     prefix: String,
                     postfix: String): String {
    val result = StringBuilder(prefix)
    for ((index, element) in collection.withIndex()) {
        if (index > 0) result.append(separator)
        result.append(element)
    }
    result.append(postfix)
    return result.toString()
}
/*
* 测试
* */
fun printList() {
    val list = listOf(2, 4, 0)
    /*不标明参数名*/
    println(joinToString(list, " - ", "[", "]"))
    /*显示的标明参数名称*/
    println(joinToString(list, separator = " - ", prefix = "[", postfix = "]"))//#######
}
```

---

#### 4、扩展函数和属性

```kotlin
/*
* 扩展函数
* */
fun String.lastChar(): Char = this.get(this.length - 1)
/*
* 扩展属性 
* */
val String.lastChar: Char
    get() = get(length - 1)
```

##### Why？Kotlin为什么能实现扩展函数和属性这样的特性？

在Kotlin中要理解一些语法，只要认识到**Kotlin语言最后需要编译为class字节码，Java也是编译为class执行，也就是可以大致理解为Kotlin需要转成Java一样的语法结构**， Kotlin就是一种**强大的语法糖**而已，Java不具备的功能Kotlin也不能越界的。 - 那Kotlin的扩展函数怎么实现的呢？介绍一种万能的办法去理解Kotlin的语法：**将Kotlin代码转化成Java语言**去理解，步骤如下： - 在Android Studio中选择Tools ---> Kotlin ---> Show Kotlin Bytecode 这样就把Kotlin转化为class字节码了 - class码阅读不太友好，点击左上角的Decompile就转化为Java - 再介绍一个小窍门，在前期对Kotlin语法不熟悉的时候，可以先用Java写好代码，再利用AndroidStudio工具**将Java代码转化为Kotlin代码**，步骤如下： - 在Android Studio中选中要转换的Java代码 ---> 选择Code ---> Convert Java File to Kotlin File

我们看看将上面的扩展函数转成Java后的代码

```java
/*
* 扩展函数会转化为一个静态的函数，同时这个静态函数的第一个参数就是该类的实例对象
* */
public static final char lastChar(@NotNull String $receiver) {
    Intrinsics.checkParameterIsNotNull($receiver, "$receiver");
    return $receiver.charAt($receiver.length() - 1);
}
/*
* 获取的扩展属性会转化为一个静态的get函数，同时这个静态函数的第一个参数就是该类的实例对象
* */
public static final char getLastChar(@NotNull StringBuilder $receiver) {
    Intrinsics.checkParameterIsNotNull($receiver, "$receiver");
    return $receiver.charAt($receiver.length() - 1);
}
/*
* 设置的扩展属性会转化为一个静态的set函数，同时这个静态函数的第一个参数就是该类的实例对象
* */
public static final void setLastChar(@NotNull StringBuilder $receiver, char value) {
    Intrinsics.checkParameterIsNotNull($receiver, "$receiver");
    $receiver.setCharAt($receiver.length() - 1, value);
}
```

对于扩展函数，转化为Java的时候其实就是一个静态的函数，同时这个静态函数的第一个参数就是该类的实例对象，这样把类的实例传人函数以后，函数内部就可以访问到类的公有方法。 对于扩展属性也类似，获取的扩展属性会转化为一个静态的get函数，同时这个静态函数的第一个参数就是该类的实例对象，设置的扩展属性会转化为一个静态的set函数，同时这个静态函数的第一个参数就是该类的实例对象。 函数内部可以访问公有的方法和属性。 - 从上面转换的源码其实可以看到**扩展函数和扩展属性适用的地方和缺陷**，有两点：

- 扩展函数和扩展属性内**只能访问到类的公有方法和属性**，私有的和protected是访问不了的 
- 扩展函数**不能被override**，因为Java中它是静态的函数 
- 下面再举几个扩展函数的例子，让大家感受一下扩展函数的方便：

```kotlin
/*
* show toast in activity
* */
fun Activity.toast(msg: String){
    Toast.makeText(this, msg, Toast.LENGTH_SHORT).show()
}

val Context.inputMethodManager: InputMethodManager?
    get() = getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager

/*
* hide soft input
* */
fun Context.hideSoftInput(view: View) {
    inputMethodManager?.hideSoftInputFromWindow(view.windowToken, 0)
}


/**
 * screen width in pixels
 */
val Context.screenWidth
    get() = resources.displayMetrics.widthPixels

/**
 * screen height in pixels
 */
val Context.screenHeight
    get() = resources.displayMetrics.heightPixels

/**
 * returns dip(dp) dimension value in pixels
 * @param value dp
 */
fun Context.dip2px(value: Int): Int = (value * resources.displayMetrics.density).toInt()
```

---

#### 5、懒初始化by lazy 和 延迟初始化lateinit

##### 懒初始化by lazy 

懒初始化是指推迟一个变量的初始化时机，变量在使用的时候才去实例化，这样会更加的高效。因为我们通常会遇到这样的情况，一个变量直到使用时才需要被初始化，或者仅仅是它的初始化依赖于某些无法立即获得的上下文。 

```kotlin
/*
* 懒初始化api实例
* */
val purchasingApi: PurchasingApi by lazy {
    val retrofit: Retrofit = Retrofit.Builder()
            .baseUrl(API_URL)
            .addConverterFactory(MoshiConverterFactory.create())
            .build()
    retrofit.create(PurchasingApi::class.java)
}
```

像上面的代码，retrofit生成的api实例会在首次使用到的时候才去实例化。需要注意的是**by lazy一般只能修饰val**不变的对象，不能修饰var可变对象。

```kotlin
class User(var name: String, var age: Int)

/*
* 懒初始化by lazy
* */
val user1: User by lazy {
    User("jack", 15)
}
```



##### 延迟初始化lateinit

另外，对于var的变量，如果类型是**非空的，是必须初始化的**，不然编译不通过，这时候需要用到lateinit延迟初始化，使用的时候再去实例化。

```kotlin
* 延迟初始化lateinit
* */
lateinit var user2: User

fun testLateInit() {
    user2 = User("Lily", 14)
}
```

##### by lazy 和 lateinit 的区别

- by lazy 修饰val的变量
- lateinit 修饰var的变量，且变量是非空的类型

---

#### 6、不用再手写findViewById

- 步骤1在项目的gradle中 apply plugin: 'kotlin-android-extensions' 
- 步骤2，按照原来的习惯书写布局xml文件
- 步骤3，在java代码中import对应的布局就可以开始使用了，View不用提前声明，插件会自动根据布局的id生成对应的View成员（其实没有生成属性，原理见下面）

```xml
<ImageView
        android:id="@+id/tip6Img"
        android:layout_width="match_parent"
        android:layout_height="wrap_content" />
```

```kotlin
class KotlinTip6 : Activity(){
    /*
    * 自动根据layout的id生成对应的view
    * */
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_tip6)
        tip6Tv.text = "Auto find view for TextView"
        tip6Img.setImageBitmap(null)
        tip6Btn.setOnClickListener{
            test()
        }
    }
    private fun test(){
        tip6Tv.text = "update"
    }
}
```

---

#### 7、利用局部函数抽取重复代码

Kotlin中提供了函数的嵌套，在函数内部还可以定义新的函数。这样我们可以在函数中嵌套这些提前的函数，来抽取重复代码。

```kotlin
class User(val id: Int, val name: String, val address: String, val email: String)

fun saveUser(user: User) {
    if (user.name.isEmpty()) {
        throw IllegalArgumentException("Can't save user ${user.id}: empty Name")
    }
    if (user.address.isEmpty()) {
        throw IllegalArgumentException("Can't save user ${user.id}: empty Address")
    }
    if (user.email.isEmpty()) {
        throw IllegalArgumentException("Can't save user ${user.id}: empty Email")
    }
    //save to db ...
}
```

上面的代码在判断name、address等是否为空的处理其实很类似。这时候，我们可以利用在函数内部嵌套的声明一个通用的判空函数将相同的代码抽取到一起：

```kotlin
/*
*利用局部函数抽取相同的逻辑，去除重复的代码
* */
fun saveUser2(user: User) {
    fun validate(value: String, fildName: String) {
        if (value.isEmpty()) {
            throw IllegalArgumentException("Can't save user ${user.id}: empty $fildName")
        }
    }

    validate(user.name, "Name")
    validate(user.address, "Address")
    validate(user.email, "Email")
    //save to db ...
}
```

除了利用嵌套函数去抽取，此时，其实也可以用扩展函数来抽取，如下所示：

```kotlin
/*
* 利用扩展函数抽取逻辑
* */
fun User.validateAll() {
    fun validate(value: String, fildName: String) {
        if (value.isEmpty()) {
            throw IllegalArgumentException("Can't save user $id: empty $fildName")
        }
    }

    validate(name, "Name")
    validate(address, "Address")
    validate(email, "Email")
}

fun saveUser3(user: User) {
    user.validateAll()
    //save to db ...
}
```

---

#### 8、使用数据类来快速实现model类

在java中要声明一个model类需要实现很多的代码，首先需要将变量声明为private，然后需要实现get和set方法，还要实现对应的hashcode equals toString方法等。

```java
    public static class User{

        private String name;
        private int age;
        private int gender;
        private String address;
        
        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public int getAge() {
            return age;
        }

        public void setAge(int age) {
            this.age = age;
        }

        public int getGender() {
            return gender;
        }

        public void setGender(int gender) {
            this.gender = gender;
        }

        public String getAddress() {
            return address;
        }

        public void setAddress(String address) {
            this.address = address;
        }

        @Override
        public String toString() {
            return "User{" +
                    "name='" + name + '\'' +
                    ", age=" + age +
                    ", gender=" + gender +
                    ", address='" + address + '\'' +
                    '}';
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            User user = (User) o;

            if (age != user.age) return false;
            if (gender != user.gender) return false;
            if (name != null ? !name.equals(user.name) : user.name != null) return false;
            return address != null ? address.equals(user.address) : user.address == null;
        }

        @Override
        public int hashCode() {
            int result = name != null ? name.hashCode() : 0;
            result = 31 * result + age;
            result = 31 * result + gender;
            result = 31 * result + (address != null ? address.hashCode() : 0);
            return result;
        }
    }
```

这段代码Java需要70行左右，而如果用kotlin，只需要一行代码就可以做到。

```kotlin
/*
* Kotlin会为类的参数自动实现get set方法
* */
class User(val name: String, val age: Int, val gender: Int, var address: String)

/*
* 用data关键词来声明一个数据类，除了会自动实现get set，还会自动生成equals hashcode toString
* */
data class User2(val name: String, val age: Int, val gender: Int, var address: String)
```

对于Kotlin中的类，会为它的参数自动实现get set方法。而如果加上data关键字，还会自动生成equals hashcode toString。原理其实数据类中的大部分代码都是模版代码，K**otlin聪明的将这个模版代码的实现放在了编译器处理的阶段。**

---

#### 9、用类委托来快速实现装饰器模式







---

#### 10、Lambda表达式简化代码

lambda表达式可以简化我们的代码。以Android中常见的OnClickListener来说明，在Java中我们一般这样设置：

```java
        TextView textView = new TextView(context);
        textView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                //handle click
            }
        });
		//简化版
		textView.setOnClickListener(v -> {
            //...
        });
```

Java中需要声明一个匿名内部类去处理，这种情况可以用lambda表达式来简化。 - lambda表达式一般长这样 - { x:Int, y:Int -> x+y } - 参数 -> 表达式 并且始终在大括号中 - it作为默认参数名 - lambda捕捉，当捕捉final变量时，它的值和lambda代码一起存储 - 非final变量，它的值被封装在一个特殊的包装器中，这个包装器的引用会和lambda代码一起存储

我们来看看Kotlin中的例子：

```kotlin
    val textView = TextView(context)

    /*
    * 传统方式
    * */
    textView.setOnClickListener(object : android.view.View.OnClickListener {
        override fun onClick(v: android.view.View?) {
            //handle click
        }
    })

    /*
    * lambda的方式
    * */
    textView.setOnClickListener({ v ->
        {
            //handle click
        }
    })
```

当lambda的参数没有使用时可以省略，省略的时候用it来替代

```kotlin
    /*
    * lambda的参数如果没有使用可以省略，省略的时候用it来替代
    * */
    textView.setOnClickListener({
        //handle click
    })
```

lambda在参数的最后一个的情况可以将之提出去

```kotlin
    /*
    * lambda在参数的最后一个的情况可以将之提出去
    * */
    textView.setOnClickListener() {
        //handle click
    }
```

lambda提出去之后，函数如果没有其他参数括号可以省略

```kotlin
    /*
    * lambda提出去之后，函数如果没有其他参数括号可以省略
    * */
    textView.setOnClickListener {
        //handle click
    }
```

我们再看看如果自己去实现一个带lambda参数的函数应该怎么去定义：

```kotlin
interface OnClickListener {
    fun onClick()
}

class View {
    var listener: OnClickListener? = null;

    /*
    * 传统方式
    * */
    fun setOnClickListener(listener: OnClickListener) {
        this.listener = listener
    }

    fun doSth() {
        //some case:
        listener?.onClick()
    }

    /*
    * 声明lambda方式，listener: () -> Unit
    * */
    fun setOnClickListener(listener: () -> Unit) {

    }
}
```

在函数参数中需要声明lambda的类型后，再调用该函数的时候就可以传人一个lambda表达式了。

----

#### 11、with语句来简化代码

- with 函数原型：

```kotlin
inline fun <T, R> with(receiver: T, block: T.() -> R): R = receiver.block()
```

- with函数并不是扩展函数，返回值是最后一行，可以直接调用对象的方法

Kotlin中可以用with语句来省略同一个变量的多次声明，例如下面的函数 

```kotlin
/*
*打印字母表函数，在函数内result变量在好几处有使用到
* */
fun alphabet(): String {
    val result = StringBuilder()
    result.append("START\n")
    for (letter in 'A'..'Z') {
        result.append(letter)
    }
    result.append("\nEND")
    return result.toString()
}
```

在上面的函数中，result变量出现了5次，如果用with语句，可以将这5次都不用再出现了，我们来一步一步地看是怎么实现的：

```kotlin
/*
* 通过with语句，将result作为参数传人，在内部就可以通过this来表示result变量了
* */
fun alphabet2(): String {
    val result = StringBuilder()
    return with(result) {
        this.append("START\n")
        for (letter in 'A'..'Z') {
            this.append(letter)
        }
        this.append("\nEND")
        this.toString()
    }
}
```

通过with语句，将result作为参数传人，在内部就可以通过this来表示result变量了，而且这个this是可以省略的

```kotlin
/*
* 通过with语句，将result参数作为参数，在内部this也可以省略掉
* */
fun alphabet3(): String {
    val result = StringBuilder()
    return with(result) {
        append("START\n")
        for (letter in 'A'..'Z') {
            append(letter)
        }
        append("\nEND")
        toString()
    }
}
```

在内部this省略掉后，现在只有一个result了，这个其实也是没必要的，于是出现了下面的最终版本：

```kotlin
/*
* 通过with语句，可以直接将对象传人，省掉对象的声明
* */
fun alphabet4(): String {
    return with(StringBuilder()) {
        append("START\n")
        for (letter in 'A'..'Z') {
            append(letter)
        }
        append("\nEND")
        toString()
    }
}
```

像上面这样，我们可以把同一个变量的显式调用从5次变为0次，发现Kotlin的魅力了吧。

---

#### 12、apply语句来简化代码

- apply 函数原型：

```kotlin
inline fun <T> T.apply(block: T.() -> Unit): T { block(); return this }
```

- apply函数，在函数范围内，可以任意调用该对象的任意方法，并返回该对象

除了用上面的with可以简化同一个变量的多次声明，还可以用apply关键字，我们来改造一下tip11中的函数： 详见案例代码[KotlinTip12](https://github.com/heimashi/kotlin_tips/blob/master/app/src/main/java/com/sw/kotlin/tip12/KotlinTip12.kt)

```kotlin
/*
* 用apply语句简化代码，在apply的大括号里可以访问类的公有属性和方法
* */
fun alphabet5() = StringBuilder().apply {
    append("START\n")
    for (letter in 'A'..'Z') {
        append(letter)
    }
    append("\nEND")
}.toString()
```

像上面这样的，通过apply后，在apply的大括号里可以访问类的公有属性和方法。这在对应类的初始化是非常方便的，例如下面的例子

```kotlin
/*
* 用apply语句简化类的初始化，在类实例化的时候，就可以通过apply把需要初始化的步骤全部实现，非常的简洁
* */
fun testApply(context: Context) {
    var imgView = ImageView(context).apply {
        setBackgroundColor(0)
        setImageBitmap(null)
    }

    var textView = TextView(context).apply {
        text = "content"
        textSize = 20.0f
        setPadding(10, 0, 0, 0)
    }
    
    var user = User().apply { 
        age = 15
        name = "Jack"
        val a = address
        address = "bbb"
    }
}
```

在类实例化的时候，就可以通过apply把需要初始化的步骤全部实现，非常的简洁

---

#### 13、在编译阶段避免掉NullPointerException

NullPointerException是Java程序员非常头痛的一个问题，我们知道Java 中分受检异常和非受检异常，NullPointerException是非受检异常，也就是说NullPointerException不需要显示的去catch住， 往往在运行期间，程序就可能报出一个NullPointerException然后crash掉，Kotlin作为一门高效安全的语言，它尝试在编译阶段就把空指针问题显式的检测出来，把问题留在了编译阶段，让程序更加健壮。 详见案例代码[KotlinTip13](https://github.com/heimashi/kotlin_tips/blob/master/app/src/main/java/com/sw/kotlin/tip13/KotlinTip13.kt) - Kotlin中类型分为可空类型和不可空类型，通过？代表可空，不带？代表不可为空

```kotlin
fun testNullType() {
    val a: String = "aa"
    /*
    * a是非空类型，下面的给a赋值为null将会编译不通过
    * */
    //a = null
    a.length

    /*
   * ？声明是可空类型，可以赋值为null
   * */
    var b: String? = "bb"
    b = null
    
    /*
   * b是可空类型，直接访问可空类型将编译不通过，需要通过?.或者!!.来访问
   * */
    //b.length
    b?.length
    b!!.length
}
```

- 对于一个不可为空类型：如果直接给不可为空类型赋值一个可能为空的对象就在编译阶段就不能通过
- 对于一个可空类型：通过？声明，在访问该类型的时候直接访问不能编译通过，需要通过?.或者!!.
  - ?. 代表着如果该类型为空的话就返回null不做后续的操作，如果不为空的话才会去访问对应的方法或者属性
  - !!. 代表着如果该类型为空的话就抛出NullPointerException，如果不为空就去访问对应的方法或者属性， 所以只有在很少的特定场景才用这种符号，代表着程序不处理这种异常的case了，会像java代码一样抛出NullPointerException。 而且代码中一定不用出现下面这种代码，会让代码可读性很差而且如果有空指针异常，我们也不能马上发现是哪空了：

```kotlin
    /*
    * 不用链式的连续用!!.
    * */
    val user = User()
    user!!.name!!.subSequence(0,5)!!.length
```

对应一个可空类型，每次对它的访问都需要带上?.判断

```kotlin
val user: User? = User()

    /*
    * 每次访问都用用?.判断
    * */
    user?.name
    user?.age
    user?.toString()
```

但这样多了很多代码，kotlin做了一些优化，

```kotlin
    /*
    * 或者提前判断是否为空，如果不为空在这个分支里会自动转化为非空类型就可以直接访问了
    * */
    if (user != null) {
        user.name
        user.age
        user.toString()
    }
```

通过if提前判断类型是否为空，如果不为空在这个分支里会**自动转化为非空类型**就可以直接访问了。

##### let语句简化对可空对象对访问

- let 函数原型：

```kotlin
inline fun <T, R> T.let(block: (T) -> R): R = block(this)
```

- let函数默认当前这个对象作为闭包的it参数，返回值是函数里面最后一行，或者指定return。

上面的代码还可以用?.let语句进行，如下所示：

```kotlin
    /*
    * 通过let语句，在?.let之后，如果为空不会有任何操作，只有在非空的时候才会执行let之后的操作
    * */
    user?.let {
        it.name
        it.age
        it.toString()
    }
```

通过let语句，在?.let之后，如果为空不会有任何操作，只有在非空的时候才会执行let之后的操作

##### Elvis操作符 ?: 简化对空值的处理

如果值可能为空，对空值的处理可能会比较麻烦，像下面这样：

```kotlin
/*
* 对空值的处理
* */
fun testElvis(input: String?, user: User?) {
    val a: Int?
    if (input == null) {
        a = input?.length
    } else {
        a = -1;
    }

    if (user == null) {
        var newOne = User()
        newOne.save()
    } else {
        user.save()
    }
}
```

Elvis操作符?:能够简化上面的操作，?:符号会在对于空的情况才会进行下面的处理，**跟?.let正好相反**，例如我们可以用两行代码来简化上面从操作：

```kotlin
/**
 * Elvis操作符 ?: 简化对空值的处理
 */
fun testElvis2(input: String?, user: User?) {
    val b = input?.length ?: -1;
    user?.save() ?: User().save()
}
```

#### 14、运算符重载

Kotlin支持对运算符的重载，这对于对一些对象的操作更加灵活直观。

- 使用operator来修饰plus\minus函数
- 可重载的二元算术符
  - A * B times
  - A / B div
  - A % B mod
  - A + B plus
  - A - B minus

以下面对坐标点Point的案例说明怎么去重载运算符：

```kotlin
class Point(val x: Int, val y: Int) {

    /*
    * plus函数重载对Point对象的加法运算符
    * */
    operator fun plus(other: Point): Point {
        return Point(x + other.x, y + other.y)
    }

    /*
    * minus函数重载对Point对象的减法运算符
    * */
    operator fun minus(other: Point): Point {
        return Point(x - other.x, y - other.y)
    }

    override fun toString(): String {
        return "[x:$x, y:$y]"
    }

}
```

如上所示，通过plus函数重载对Point对象的加法运算符，通过minus函数重载对Point对象的减法运算符，然后就可以用+、-号对两个对象进行操作了：

```kotlin
fun testOperator() {
    val point1 = Point(10, 10)
    val point2 = Point(4, 4)
    val point3 = point1 + point2
    println(point3)
    println(point1 - point2)
}
```

#### 15、高阶函数简化代码

- 高阶函数：以另一个函数作为参数或者返回值的函数
- 函数类型
  - (Int, String) -> Unit
  - 参数类型->返回类型 Unit不能省略

```kotlin
    val list = listOf(2, 5, 10)
    /*
    * 传人函数来过滤
    * */
    println(list.filter { it > 4 })
      
    /*
    * 定义函数类型
    * */
    val sum = { x: Int, y: Int -> x + y }
    val action = { println(42) }

    val sum2: (Int, Int) -> Int = { x: Int, y: Int -> x + y }
    val action2: () -> Unit = { println(42) }      
```

##### 函数作为参数

函数作为参数，即高阶函数中，函数的参数可以是一个函数类型，例如要定义一个函数，该函数根据传人的操作函数来对2和3做相应的处理。

```kotlin
/*
* 定义对2和3的操作函数
* */
fun twoAndThree(operator: (Int, Int) -> Int) {
    val result = operator(2, 3)
    println("Result:$result")
}

fun test03() {
    twoAndThree { a, b -> a + b }
    twoAndThree { a, b -> a * b }
}
```

operator是函数类型，函数的具体类型为(Int, Int) -> Int，即输入两个Int返回一个Int值。定义完了后就可以像上面这样使用了。 再举一个例子，实现String类的字符过滤：

```kotlin
/*
* 函数作为参数，实现String类的字符过滤
* */
fun String.filter(predicate: (Char) -> Boolean): String {
    val sb = StringBuilder()
    for (index in 0 until length) {
        val element = get(index)
        if (predicate(element)) sb.append(element)
    }
    return sb.toString()
}

fun test04() {
    println("12eafsfsfdbzzsa".filter { it in 'a'..'f' })
}
```

像上面这样predicate是函数类型，它会根据传人的char来判断得到一个Boolean值。

##### 函数作为返回值

函数作为返回值也非常实用，例如我们的需求是根据不同的快递类型返回不同计价公式，普通快递和高级快递的计价规则不一样，这时候我们可以将计价规则函数作为返回值：

```kotlin
enum class Delivery {
    STANDARD, EXPEDITED
}

/*
* 根据不同的运输类型返回不同的快递方式
* */
fun getShippingCostCalculator(delivery: Delivery): (Int) -> Double {
    if (delivery == Delivery.EXPEDITED) {
        return { 6 + 2.1 * it }
    }
    return { 1.3 * it }
}

fun test05(){
    val calculator1 = getShippingCostCalculator(Delivery.EXPEDITED)
    val calculator2 = getShippingCostCalculator(Delivery.STANDARD)
    println("Ex costs ${calculator1(5)}")
    println("St costs ${calculator2(5)}")
}
```

如果是普通快递，采用6 + 2.1 * it的规则计算价格，如果是高级快递按照6 + 2.1 * it计算价格，根据不同的类型返回不同的计价函数。

#### 16、用Lambda来简化策略模式

策略模式是常见的模式之一，java的例子如下。 详见案例代码[Tip16](https://github.com/heimashi/kotlin_tips/blob/master/app/src/main/java/com/sw/kotlin/tip16)

```java
/**
     * 定义策略接口
     */
    public interface Strategy {
        void doSth();
    }

    /**
     * A策略
     */
    public static class AStrategy implements Strategy {
        @Override
        public void doSth() {
            System.out.println("Do A Strategy");
        }
    }

    /**
     * B策略
     */
    public static class BStrategy implements Strategy {
        @Override
        public void doSth() {
            System.out.println("Do B Strategy");
        }
    }

    /**
     * 策略实施者
     */
    public static class Worker {

        private Strategy strategy;

        public Worker(Strategy strategy) {
            this.strategy = strategy;
        }

        public void work() {
            System.out.println("START");
            if (strategy != null) {
                strategy.doSth();
            }
            System.out.println("END");
        }
    }
```

如上面的例子所示，有A、B两种策略，Worker根据不同的策略做不同的工作，使用策略时：

```java
    Worker worker1 = new Worker(new AStrategy());
    Worker worker2 = new Worker(new BStrategy());
    worker1.work();
    worker2.work();
```

在java中实现这种策略模式难免需要先定义好策略的接口，然后根据接口实现不同的策略， 在Kotlin中完全可以用用Lambda来简化策略模式，上面的例子用Kotlin实现：

```kotlin
/**
 * 策略实施者
 * @param strategy lambda类型的策略
 */
class Worker(private val strategy: () -> Unit) {
    fun work() {
        println("START")
        strategy.invoke()
        println("END")
    }

}

/*
* 测试
* */
fun testStrategy() {
    val worker1 = Worker({
        println("Do A Strategy")
    })
    val bStrategy = {
        println("Do B Strategy")
    }
    val worker2 = Worker(bStrategy)
    worker1.work()
    worker2.work()
}
```

不需要先定义策略的接口，直接把策略以lambda表达式的形式传进来就行了。

