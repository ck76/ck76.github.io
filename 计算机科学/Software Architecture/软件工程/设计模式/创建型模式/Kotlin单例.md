[TOC]

目前java中的单例模式有多种写法，kotlin中的写法更多一点，本篇会总结全部的到单例模式写法。

### 一、懒人写法（恶汉式）

#### java中

```kotlin
  public class Singleton{
        public static final Singleton instance = new Singleton();
        public Singleton getInstance() {
              return instance;
        }
  }
```

#### kotlin中

```kotlin
最简单的写法，直接用object声明

object Singleton{}
```

### 二、基本懒加载（未实现线程同步）

```
注：这种方式实现了懒加载，但是不是线程安全的，可能在多个线程中创建多个不同的实例
```

#### java中

```kotlin
public class Singleton {
      public static Singleton instance = null;
      private Singleton (){}
      public Singleton getInstance() {
            if (instance == null) {
                instance = new Singleton();
            }
            return instance;
       }
}
```

#### kotlin中

```kotlin
class Singleton private constructor{
    companion object {
          val intance by lazy(LazyThreadSafetyMode.NONE) { Singleton() }
    }
}
```

### 三、线程同步单例1

```
注：其实kotlin这种写法可以说是java直译过来的，虽说是线程安全的，但是太影响效率，主要看下面这种
```

#### java中

```kotlin
public class Singleton {
      private static Singleton instance = null;
      private Singleton (){}
      public static synchronized  Singleton getInstance() {
            if (instance == null) {
                instance = new Singleton();
             }
            return instance;
        }
 }
```

#### kotlin中

```kotlin
class Singleton private constructor(){
        companion object {
            lateinit var instance: Singleton
            @Synchronized
            fun get(): Singleton {
                    if (instance == null) {
                          instance = Singleton();
                     }
                return instance!!
            }
      }
}
```

### 四、线程同步单例2

```kotlin
注：线程同步，懒加载，无同步引起的效率问题
```

#### java中双检锁单例

```kotlin
public class Singleton {
      private static  Singleton instance = null;
      private Singleton (){}
      public static Singleton getInstance() {
            if (instance == null) {
            synchronized (Singleton.class) {
                  if (instance == null) {
                      instance = new Singleton();
                  }
             }
      }
      return instance;
  }
}
```

#### kotlin

```kotlin
class Singleton private constructor(){
      companion object {
          val intance by lazy(LazyThreadSafetyMode.SYNCHRONIZED) { Singleton() }
  }
}
```

### 五、静态内部类单例

#### java中

```kotlin
public class Singleton {
    private Singleton (){}
    private static class Holder {
    private static Singleton instance = new Singleton();
    }
    public static Singleton getInstance(){
        return Holder.instance;
    }
}
```

#### kotlin中

```kotlin
class Singleton private constructor(){
      companion object {
          fun getInstance(): Singleton {
              return Holder.instance
          }
      }
      private object Holder {
          val instance = Singleton()
      }
  }
```