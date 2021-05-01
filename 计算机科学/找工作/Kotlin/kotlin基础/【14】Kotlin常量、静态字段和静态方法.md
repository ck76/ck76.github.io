[TOC]

> Kotlin定义常量、静态字段、静态方法有三种实现手段，分别是包级、对象、类的伴生对象

- 常量：由`const val`修饰，只适用于基本类型和字符串。

- 静态字段：kotlin属性添加`@JvmField`注解后，可以暴露成静态字段被Java 调用。

- 静态方法：
  - 包级函数会直接编译成静态方法；
  - Kotlin对象函数和伴生对象函数添加`@JvmStatic`注解后，编译时会额外生成静态方法。

*包级实现在包名前添加**@file:JvmName**注解，可以修改生成的 Java 类的类名。*

1. 天生有归属的，比方说静态工厂方法，建议使用伴生对象。
2. 能强行归类的，比方说Constants，建议归类到一个新对象。



### 一、总结

- @JvmField消除了变量的getter与setter方法
- @JvmField修饰的变量不能是private属性的,最后变为`public static final`
- @JvmStatic只能在object类或者伴生对象companion object中使用，而@JvmField没有这些限制
- @JvmStatic一般用于修饰方法，使方法变成真正的静态方法；如果修饰变量不会消除变量的getter与setter方法，但会使getter与setter方法和变量都变成静态

- `const`只能修饰常量`val`
- `const`只能在`object`类中或者伴生对象`companion object`中使用
- `const`的效果等于`@JvmField`，两者不能同时使用
- **总之：全是static，有的public有的private**
- 纯kotlin的话就无所谓，但是与java混合开发的话就是**const/@JvmField**修饰变量，**@JvmStatic**修饰方法



### 二、示例

```kotlin
//源代码
const val constValll = 4
var varrr = 4
val valll=4

object packgeObject {
    const val packConstVar = 3
    val packVarr = 3
    var packVar = 3
}

class ConstantTest {
    companion object {
        val noAnnotationVal = 0
        var noAnnotationVar = 0
        const val compConst = 1

        @JvmField
        val fieldVal = 2
        @JvmField
        var fieldVar = 2

        @JvmStatic
        val staticVal = 1
        @JvmStatic
        var staticVar = 1

        @JvmStatic
        fun staticFun() {
            println("staticFun")
        }
        fun hahaFun(){
            println("haha")
        }
    }
}
```



```kotlin
// ConstantTest.java
public final class ConstantTest {
   //没有注解的话只能成为私有变量，然后通过类名.Companion.getXXX调用
   private static final int noAnnotationVal = 0;
   private static int noAnnotationVar;
   //有const 关键字就是public static fianl，不提供get/set
   public static final int compConst = 1;
   //有点类似 const关键字，但是二者不能同时使用
   @JvmField
   public static final int fieldVal = 2;
   @JvmField
   public static int fieldVar = 2;
   //提供相应的get/set方法
   private static final int staticVal = 1;
   private static int staticVar = 1;
    
   public static final ConstantTest.Companion Companion = new ConstantTest.Companion((DefaultConstructorMarker)null);

   public static final int getStaticVal() {
      ConstantTest.Companion var10000 = Companion;
      return staticVal;
   }

   public static final int getStaticVar() {
      ConstantTest.Companion var10000 = Companion;
      return staticVar;
   }

   public static final void setStaticVar(int var0) {
      ConstantTest.Companion var10000 = Companion;
      staticVar = var0;
   }

   //注解方法，可以直接类名调用
   //普通方法则不能直接调
   @JvmStatic
   public static final void staticFun() {
      Companion.staticFun();
   }

   public static final class Companion {
      public final int getNoAnnotationVal() {
         return ConstantTest.noAnnotationVal;
      }

      public final int getNoAnnotationVar() {
         return ConstantTest.noAnnotationVar;
      }

      public final void setNoAnnotationVar(int var1) {
         ConstantTest.noAnnotationVar = var1;
      }

      @JvmStatic
      public static void staticVal$annotations() {
      }

      public final int getStaticVal() {
         return ConstantTest.staticVal;
      }

      @JvmStatic
      public static void staticVar$annotations() {
      }

      public final int getStaticVar() {
         return ConstantTest.staticVar;
      }

      public final void setStaticVar(int var1) {
         ConstantTest.staticVar = var1;
      }

      @JvmStatic
      public final void staticFun() {
         String var1 = "staticFun";
         System.out.println(var1);
      }

      public final void hahaFun() {
         String var1 = "haha";
         System.out.println(var1);
      }

      private Companion() {
      }

      public Companion(DefaultConstructorMarker $constructor_marker) {
         this();
      }
   }
}

// ConstantTestKt.java
// 包级变量
public final class ConstantTestKt {
   public static final int constValll = 4;
   private static int varrr = 4;
   private static final int valll = 4;

   public static final int getVarrr() {
      return varrr;
   }

   public static final void setVarrr(int var0) {
      varrr = var0;
   }

   public static final int getValll() {
      return valll;
   }
}

// packgeObject.java
// 包的object
public final class packgeObject {
   public static final int packConstVar = 3;
   private static final int packVarr = 3;
   private static int packVar;
   public static final packgeObject INSTANCE;

   public final int getPackVarr() {
      return packVarr;
   }

   public final int getPackVar() {
      return packVar;
   }

   public final void setPackVar(int var1) {
      packVar = var1;
   }

   private packgeObject() {
   }

   static {
      packgeObject var0 = new packgeObject();
      INSTANCE = var0;
      packVarr = 3;
      packVar = 3;
   }
}
```

