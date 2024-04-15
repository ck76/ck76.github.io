[TOC]

> Kotlin虽然是一个面向对象的编程语言，但是仍然保留了函数
>
> Java中则只有方法

### 函数的特性语法

- 默认参数

```kotlin
fun say(str:String?="hahaha"){
    print($str)
}
```



### 嵌套函数

> 与内部类相似，函数可访问外部函数的局部变量，这种写法通常写在某些条件下会触发递归的函数，不希望被外部函数访问到的函数 ##**一般情况下不推荐使用嵌套函数**，会使代码可读性降低##

- 函数体只有一个语句可以直接将语句赋值给函数

```kotlin
fun ck(){
    val str = "hello"
    fun say(count: Int =10){
        if(count>0){
            say(count-1)
        } 
    }
    say()
}
```



### 扩展函数

- 扩展成员变量的语法结构都是一模一样的
- **添加时静态添加，静态，静态的给一个类添加，不具备运行时的多态效应**

> Kotlin中的一大优势是可以静态的给一个类扩展它的成员方法 以及成员变量
>
> 主要用于第三方sdk，一些自己不能控制的类添加方法

```kotlin
//扩展方法代码，位于FilesKt.kt
public fun java.io.File.readText(charset: java.nio.charset.Charset /* = compiled code */): kotlin.String { /* compiled code */ }

//KotlinCode
fun 
var file =File()
var content= file.readText(XXX.txt)

//javaCode
String content = FilesKt.readText(file,Charsets.UTF-8)
注：添加的方法比不是添加到原类中，是添加到编译后的Kt文件中，注意第一个参数与默认参数，第一个参数是扩展类的对象，默认参数不可缺少。
```



### lambda闭包语法

```kotlin
//java
  new Thread(new Runnable() {
            @Override
            public void run() {
                
            }
        });
//java8
        new Thread(() -> {
            
        });
//kotlin
fun main(args: Array<String>) {
    Thread({ -> Unit }) 
    Thread({})     //lambda无参可以省略箭头符号
    Thread(){}     //如果lambda是函数的最后一个参数可以将大括号放在小括号的外边
    Thread{}	   //如果只有一个参数，而且参数是lanmbda可以省略小括号
    
}
```

#### 声明闭包

```kotlin
val echo = {str:String -> 
           println(str)
         }
```

lambda编译后会被编译成一个匿名的内部对象



### 高阶函数

> 高阶就是指函数或者lambda的参数又是一个函数或者lambda

- 函数是第一等公民

```kotlin
fun main(args: Array<String>) {
    var runnable= Runnable {
        print("ckaaa")
    }
    var function = runnable::run	//声明函数function等于runnable的run方法
    								//而不是像java那样直接调用.run方法，
    								//那样是将run的返回值赋给		function
    sayRun(function)
    function()		//调用函数
}
fun sayRun(ck:()-> Unit){
    ck()
}
```



### 内联函数

- **lambda表达式会被编译成一个匿名内部类的形式**
- **如果代码中有大量lambda表达式会生成许多无用的临时对象，可以使用inline修饰方法，这样方法编译时就会拆解方法的调用为语句调用，进而减少创建不必要的对象**
- 如果使用inline关键字会增加编译器的编译负担，代码块变大，查找困难
- inline通常只会用于**高阶函数**，不会随便用

```kotlin
inline fun onlyIf(isDebug:Boolean,block:()-> Unit){
    if(debug){
        block()
    }
}

//编译后
public static final void onluIf(boolean isDebug,
                                Function0<Unit> block){
    if(isDu=ebug){
        block.invoke();
    }
}
```

