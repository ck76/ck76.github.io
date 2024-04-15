[TOC]

### 没有封装类

【2】中已提到

### 空值敏感

接收一个java对象，如果不能保证非空，就赋值为一个可空类型，这样才能保证安全

```java
public class JavaMain {

    public static String format(String str) {
        return (str == null) ? null : str;
    }
}

//KotlinMain.kt
fun main(args: Array<String>) {
    var ck1 = JavaMain.format("");
    print(ck1.length)
    var ck2: String = JavaMain.format("")
    print(ck2.length)
    var ck3: String? = JavaMain.format("")
    print(ck3?.length)
}
//输出
0 0 0
    
//类型后面加?表示可为空
var age: String? = "23" 
//抛出空指针异常
val ages = age!!.toInt()
//不做处理返回 null
val ages1 = age?.toInt()
//age为空返回-1
val ages2 = age?.toInt() ?: -1
```



### 没有静态变量和静态方法

kotlin没有静态变量与静态方法

```java
object Test{
    @JvmStatic  //加此注解，该方法编译后也会成为public static 方法
    fun sayHello(str :String){
        print(str)
    }
}

//kotlin
Test.sayHello("ck")
//java
Test.INSTANCE.sayHello("ck");  //就不用用INSTANCE调用了
Test.sayHello("ck")			//这样
```

