[TOC]

> 在`companion object`中调用外部的成员变量会调用不到

### 一、 object关键字

`object` 关键字可以表达两种含义：一种是`对象表达式`,另一种是 `对象声明`。

#### 1、对象表达式

继承一个匿名对象

```kotlin
val textView = findViewById<TextView>(R.id.tv)
textView.setOnClickListener(object : OnClickListener {
        override fun onClick(p0: View?) {
            Toast.makeText(this@TestActivity, "点击事件生效", Toast.LENGTH_LONG)
        }

})
```

上面代码其实就是我们经常要给 `view` 设置的点击事件，`OnClickListener` 事件是一个匿名类的对象，用`object`来修饰。

#### 2、对象声明

用`object` 修饰的类为静态类，里面的方法和变量都为`静态`的。

##### 2.1 直接声明类

```kotlin
object DemoManager {
    private val TAG = "DemoManager"
        
    fun a() {
        Log.e(TAG,"此时 object 表示 声明静态内部类")
    }
    
}
```

##### 2.2 声明静态内部类

类内部的对象声明，没有被`inner` 修饰的内部类都是静态的

```kotlin
class DemoManager{
    object MyObject {
        fun a() {
            Log.e(TAG,"此时 object 表示 直接声明类")
        }
    }
}
```

如果需要调用 `a()`方法

###### kotlin中调用

```kotlin
fun init() {
    MyObject.a()
}
```

###### java中调用

```kotlin
 MyObject.INSTANCE.a();
 
```

### 二、companion object

`companion object` 修饰为伴生对象,伴生对象在类中只能存在一个，类似于java中的静态方法 Java 中使用类访问静态成员，静态方法。

```kotlin
companion object {
    private val TAG = "DemoManager"

    fun b() {
        Log.e(TAG,"此时 companion objec t表示 伴生对象")
    }
}
```

kotlin 中调用

```kotlin
fun init(){
       b()
}
```

java 中调用

```kotlin
DemoManager.Companion.b();
```

> `companion object` 相关的内容可以查阅 [Kotlin中常量和静态方法](https://www.jianshu.com/p/e8752c880088) 这篇文章，在这里不多在具体描述。

### 三、在companion object中如何调用外部的成员变量

##### 3.1 为什么`companion object` 中调用不到外部成员变量

```kotlin
class DemoManager {
    private val MY_TAG = "DemoManager"
   
    fun init(){
       b()
   }

    companion object {
        fun b() {
            Log.e(MY_TAG,"此时 companion objec t表示 伴生对象")
        }
    }
}
```

在上面代码中`MY_TAG` 是不会被调用到的。

原理很简单：

在java中我们写一个静态方法，如果需要调用成员变量，是无法调用到的

```kotlin
private String TAG = "MainActivity";
  
public static void init(){
        Log.e(TAG,"init() ");
}
```

只有将 `TAG` 修改为`静态成员变量`才能调用到

```kotlin
private static String TAG = "MainActivity";
  
public static void init(){
        Log.e(TAG,"init() ");
}
```

由此可以看出来，java中静态方法调用成员变量，要求成员变量必须是`静态的`， 在kotlin 中也是一样，所以当`companion object` 中调用非静态的成员变量也是调用不到的。

##### 3.2 怎样解决才能调用到呢？

```kotlin
companion object {
    private val MY_TAG = "DemoManager"
    fun b() {
        Log.e(MY_TAG,"此时 companion objec t表示 伴生对象")
    }
}
```

将所引用的成员变量也修饰静态的，这样就可以引用到了。