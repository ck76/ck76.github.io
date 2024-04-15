[TOC]

> 注解Annotation是java 1.5的新特性，是一种能够添加到 Java 源代码的语法元数据。类、方法、变量、参数、包都可以被注解，可用来将信息元数据与程序元素进行关联。Annotation 中文常译为“注解”。  
>
> 比如Retrofit，Butter Knife等都有些自己定义的注解；如果你对注解不了解或者不知道如何使用，那么你在用这些三方框架的时候甚至在自己写源码修改源码的时候就会变得更加的困难和举步维艰。 
>
> 反射影响性能，所以考虑好再用

# 注解的定义

注解通过 `@interface` 关键字进行定义。

```java
public @interface TestAnnotation {
}
```

它的形式跟接口很类似，不过前面多了一个 @ 符号。上面的代码就创建了一个名字为 TestAnnotaion 的注解。

你可以简单理解为创建了一张名字为 TestAnnotation 的标签。

# 注解的应用

上面创建了一个注解，那么注解的的使用方法是什么呢。

```java
@TestAnnotation
public class Test {
}
```

创建一个类 Test,然后在类定义的地方加上 @TestAnnotation 就可以用 TestAnnotation 注解这个类了。

你可以简单理解为将 TestAnnotation 这张标签贴到 Test 这个类上面。

不过，要想注解能够正常工作，还需要介绍一下一个新的概念那就是元注解。

# 元注解

元注解是什么意思呢？

元注解是可以**注解到注解上的注解**，或者说元注解是一种基本注解，但是它能够应用到其它的注解上面。

如果难于理解的话，你可以这样理解。元注解也是一张标签，但是它是一张特殊的标签，它的作用和目的就是给其他普通的标签进行解释说明的。

元标签有 @Retention、@Documented、@Target、@Inherited、@Repeatable 5 种。

## @Retention(RetentionPolicy.)

Retention 的英文意为保留期的意思。当 @Retention 应用到一个注解上的时候，它**解释说明了这个注解的的存活时间。**

它的取值如下： 

```java
RetentionPolicy.SOURCE 注解只在源码阶段保留，在编译器进行编译时它将被丢弃忽视。 

RetentionPolicy.CLASS 注解只被保留到编译进行的时候，它并不会被加载到 JVM 中。 

RetentionPolicy.RUNTIME 注解可以保留到程序运行的时候，它会被加载进入到 JVM 中，所以在程序运行时可以获取到它们。

```

根据反射的测试的问题，引出@Retention元注解的讲解：其三种取值：**RetentionPolicy.SOURCE**、**RetentionPolicy.CLASS**、**RetentionPolicy.RUNTIME**分别对应：Java源文件(.java文件)---->.class文件---->内存中的字节码 

我们可以这样的方式来加深理解，@Retention 去给一张标签解释的时候，它指定了这张标签张贴的时间。@Retention 相当于给一张标签上面盖了一张时间戳，时间戳指明了标签张贴的时间周期。

```java
@Retention(RetentionPolicy.RUNTIME)
public @interface TestAnnotation {
}
```

上面的代码中，我们指定 TestAnnotation **可以在程序运行周期被获取到**，因此它的生命周期非常的长。

## @Documented

顾名思义，这个元注解肯定是和文档有关。它的作用是能够将注解中的元素包含到 Javadoc 中去。

## @Target(ElementType.)

Target 是目标的意思，@Target 指定了注解运用的地方。

你可以这样理解，当一个注解被 @Target 注解时，这个注解就被限定了运用的场景。

类比到标签，原本标签是你想张贴到哪个地方就到哪个地方，但是因为 @Target 的存在，它张贴的地方就非常具体了，比如只能张贴到方法上、类上、方法参数上等等。@Target 有下面的取值

- ElementType.ANNOTATION_TYPE 可以给一个**注解**进行注解
- ElementType.CONSTRUCTOR 可以给**构造方法**进行注解
- ElementType.FIELD 可以给**属性**进行注解
- ElementType.LOCAL_VARIABLE 可以给**局部变量**进行注解
- ElementType.METHOD 可以给**方法**进行注解
- ElementType.PACKAGE 可以给一个**包**进行注解
- ElementType.PARAMETER 可以给一个**方法内的参数**进行注解
- ElementType.TYPE 可以给一个**类型**进行注解，比如类、接口、枚举

## @Inherited（对类来说可继承）

**允许子类继承父类中的注解** 

Inherited 是继承的意思，但是它并不是说注解本身可以继承，而是说如果一个超类被 @Inherited 注解过的注解进行注解的话，那么如果它的子类没有被任何注解应用的话，那么这个子类就继承了超类的注解。 
说的比较抽象。代码来解释。

```java
@Inherited
@Retention(RetentionPolicy.RUNTIME)
@interface Test {}

@Test
public class A {}

public class B extends A {}
```

注解 Test 被 @Inherited 修饰，之后类 A 被 Test 注解，类 B 继承 A,类 B 也拥有 Test 这个注解。

## @Repeatable

Repeatable 自然是可重复的意思。@Repeatable 是 Java 1.8 才加进来的，所以算是一个新的特性。

什么样的注解会多次应用呢？通常是注解的值可以同时取多个。

举个例子，一个人他既是程序员又是产品经理,同时他还是个画家。

```java
@interface Persons {
    Person[]  value();
}

@Repeatable(Persons.class)
@interface Person{
    String role default "";
}

@Person(role="artist")
@Person(role="coder")
@Person(role="PM")
public class SuperMan{

}
```

注意上面的代码，@Repeatable 注解了 Person。而 @Repeatable 后面括号中的类相当于一个容器注解。

什么是容器注解呢？就是用来存放其它注解的地方。它本身也是一个注解。

定义一个注解的方式：   

```java
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Test {  
}
```

# Java 预置的注解

JDK1.5之后内部提供的三个注解:

| @Deprecated          | 这个元素是用来标记过时的元素 , 如过时的方法、过时的类、过时的成员变量。 |
| -------------------- | ------------------------------------------------------------ |
| @Override            | 这个大家应该很熟悉了，提示子类要复写父类中被 @Override 修饰的方法 |
| @SuppressWarnings    | 阻止警告的意思。                                             |
| @SafeVarargs         | 参数安全类型注解。它的目的是提醒开发者不要用参数做一些不安全的操作,它的存在会**阻止编译器产生 unchecked 这样的警告**。 |
| @FunctionalInterface | 函数式接口注解，这个是 Java 1.8 版本引入的新特性。函数式编程很火，所以 Java 8 也及时添加了这个特性。 |

# 注解的提取

我通过用标签来比作注解，前面的内容是讲怎么写注解，然后贴到哪个地方去，而现在我们要做的工作就是检阅这些标签内容。 形象的比喻就是你把这些注解标签在合适的时候撕下来，然后检阅上面的内容信息。

要想正确检阅注解，离不开一个手段，那就是**反射。**

**需要注意的是，如果一个注解要在运行时被成功提取，那么 @Retention(RetentionPolicy.RUNTIME) 是必须的。** 

## 注解与反射。

注解通过反射获取。首先可以通过 Class 对象的 isAnnotationPresent() 方法判断它是否应用了某个注解

```java
public boolean isAnnotationPresent(Class<? extends Annotation> annotationClass) {}
```

然后通过 getAnnotation() 方法来获取 Annotation 对象。

```java
 public <A extends Annotation> A getAnnotation(Class<A> annotationClass) {}
```

或者是 getAnnotations() 方法。

```java
public Annotation[] getAnnotations() {}
```

前一种方法返回指定类型的注解，后一种方法返回注解到这个元素上的所有注解。

　注解可以看成是一种特殊的类，既然是类，那自然可以为类添加属性

## 添加属性

　**语法：类型 属性名();**


```java
 8 @Retention(RetentionPolicy.RUNTIME)
 9 //Retention注解决定MyAnnotation注解的生命周期
10 @Target( { ElementType.METHOD, ElementType.TYPE })
11 public @interface MyAnnotation {
12     /**
13      * 定义基本属性
14      * @return
15      */
16     String color();
17 }
```


　　其实从代码的写法上来看，注解更像是一种特殊的接口，注解的属性定义方式就和接口中定义方法的方式一样，而应用了注解的类可以认为是实现了这个特殊的接口

## 应用属性


```java
 3 @MyAnnotation(color="red")//应用MyAnnotation注解的color属性
 4 public class MyAnnotationTest {
 5     public static void main(String[] args) {
 6         /**
 7          * 用反射方式获得注解对应的实例对象后，在通过该对象调用属性对应的方法
 8          */
 9         MyAnnotation annotation = (MyAnnotation)                		MyAnnotationTest.class.getAnnotation(MyAnnotation.class);
10         System.out.println(annotation.color());//输出red
11     }
12 }
```


## **为属性指定缺省值(默认值)**

　　**语法：类型 属性名() default 默认值;**


```java
@Retention(RetentionPolicy.RUNTIME)
//Retention注解决定MyAnnotation注解的生命周期
@Target( { ElementType.METHOD, ElementType.TYPE })
public @interface MyAnnotation {
    String color() default "blue";//为属性指定缺省值
}
```


```java
 @MyAnnotation
 4 public class MyAnnotationTest {
 5     public static void main(String[] args) {
 6         /**
 7          * 用反射方式获得注解对应的实例对象后，在通过该对象调用属性对应的方法
 8          */
 9         MyAnnotation annotation = (MyAnnotation) MyAnnotationTest.class.getAnnotation(MyAnnotation.class);
10         System.out.println(annotation.color());//输出color属性的默认值：blue
11         
12     }
13 }
```


## value属性

　　如果一个注解中有一个名称为value的属性，且你只想设置value属性(即其他属性都采用默认值或者你只有一个value属性)，那么可以省略掉“value=”部分。

　　例如：@SuppressWarnings("deprecation")


```java
@Retention(RetentionPolicy.RUNTIME)
 9 //Retention注解决定MyAnnotation注解的生命周期
10 @Target( { ElementType.METHOD, ElementType.TYPE })
11 public @interface MyAnnotation {
12     String color() default "blue";//为属性指定缺省值
13     String value();//定义一个名称为value的属性
14 }
```


```java
 @MyAnnotation("程坤")//等价于@MyAnnotation(value="程坤")
 4 public class MyAnnotationTest {
 5     public static void main(String[] args) {
 6         /**
 7          * 用反射方式获得注解对应的实例对象后，在通过该对象调用属性对应的方法
 8          */
 9         MyAnnotation annotation = (MyAnnotation) MyAnnotationTest.class.getAnnotation(MyAnnotation.class);
10         System.out.println(annotation.color());//输出color属性的默认值：blue
11         System.out.println(annotation.value());
12         
13     }
14 }
```

**需要注意的是，如果一个注解要在运行时被成功提取，那么 @Retention(RetentionPolicy.RUNTIME) 是必须的。** 

# 注解的使用场景

## JUnit

JUnit 这个是一个测试框架，典型使用方法如下：

```java
public class ExampleUnitTest {
    @Test
    public void addition_isCorrect() throws Exception {
        assertEquals(4, 2 + 2);
    }
}
```

@Test 标记了要进行测试的方法 addition_isCorrect().

## ButterKnife

ButterKnife 是 Android 开发中大名鼎鼎的 IOC 框架，它减少了大量重复的代码。

```java
public class MainActivity extends AppCompatActivity {

    @BindView(R.id.tv_test)
    TextView mTv;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        ButterKnife.bind(this);
    }
}
```

## Dagger2

也是一个很有名的依赖注入框架。

## Retrofit

很牛逼的 Http 网络访问框架

```java
public interface GitHubService {
  @GET("users/{user}/repos")
  Call<List<Repo>> listRepos(@Path("user") String user);
}

Retrofit retrofit = new Retrofit.Builder()
    .baseUrl("https://api.github.com/")
    .build();

GitHubService service = retrofit.create(GitHubService.class);
```


# 总结

1. 如果注解难于理解，你就把它类同于标签，标签为了解释事物，注解为了解释代码。
2. 注解的基本语法，创建如同接口，但是多了个 @ 符号。
3. 注解的元注解。
4. 注解的属性。
5. 注解主要给编译器及工具类型的软件用的。
6. 注解的提取需要借助于 Java 的反射技术，**反射比较慢，所以注解使用时也需要谨慎计较时间成本**。



- [codekk java注解](http://a.codekk.com/detail/Android/Trinea/%E5%85%AC%E5%85%B1%E6%8A%80%E6%9C%AF%E7%82%B9%E4%B9%8B%20Java%20%E6%B3%A8%E8%A7%A3%20Annotation)