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

---

不少开源库都用到了注解的方式来简化代码提高开发效率。
本文简单介绍下 **Annotation 示例、概念及作用、分类、自定义、解析，并对几个 Android 开源库 Annotation 原理进行简析**。

### 1. Annotation 示例

Override Annotation

```
@Override
public void onCreate(Bundle savedInstanceState);
```

Retrofit Annotation

```
@GET("/users/{username}")
User getUser(@Path("username") String username);
```

Butter Knife Annotation

```
@InjectView(R.id.user) EditText username;
```

ActiveAndroid Annotation

```
@Column(name = “Name") public String name;
```

Retrofit 为符合 RESTful 规范的网络请求框架
Butter Knife 为 View 及事件等依赖注入框架
Active Android 为 ORM 框架
更多见：[Android 开源项目汇总](https://github.com/Trinea/android-open-project)

### 2. Annotation 概念及作用

#### 2.1 概念

> An annotation is a form of metadata, that can be added to Java source code. Classes, methods, variables, parameters and packages may be annotated. Annotations have no direct effect on the operation of the code they annotate.

能够添加到 Java 源代码的语法元数据。类、方法、变量、参数、包都可以被注解，可用来将信息元数据与程序元素进行关联。Annotation 中文常译为“注解”。

#### 2.2 作用

a. 标记，用于告诉编译器一些信息
b. 编译时动态处理，如动态生成代码
c. 运行时动态处理，如得到注解信息
这里的三个作用实际对应着后面自定义 Annotation 时说的 @Retention 三种值分别表示的 Annotation

```java
public class Person {

    private int    id;
    private String name;

    public Person(int id, String name) {
        this.id = id;
        this.name = name;
    }

    public boolean equals(Person person) {
        return person.id == id;
    }

    public int hashCode() {
        return id;
    }

    public static void main(String[] args) {

        Set<Person> set = new HashSet<Person>();
        for (int i = 0; i < 10; i++) {
            set.add(new Person(i, "Jim"));
        }
        System.out.println(set.size());
    }
}
```

上面的运行结果是多少？

### 3. Annotation 分类

#### 3.1 标准 Annotation，Override, Deprecated, SuppressWarnings

标准 Annotation 是指 Java 自带的几个 Annotation，上面三个分别表示重写函数，不鼓励使用(有更好方式、使用有风险或已不在维护)，忽略某项 Warning

#### 3.2 元 Annotation，@Retention, @Target, @Inherited, @Documented

元 Annotation 是指用来定义 Annotation 的 Annotation，在后面 Annotation 自定义部分会详细介绍含义

#### 3.3 自定义 Annotation

自定义 Annotation 表示自己根据需要定义的 Annotation，定义时需要用到上面的元 Annotation
这里是一种分类而已，也可以根据作用域分为源码时、编译时、运行时 Annotation，后面在自定义 Annotation 时会具体介绍

### 4. Annotation 自定义

#### 4.1 调用

```java
public class App {

    @MethodInfo(
        author = “trinea.cn+android@gmail.com”,
        date = "2014/02/14",
        version = 2)
    public String getAppName() {
        return "trinea";
    }
}
```

这里是调用自定义 Annotation——MethodInfo 的示例。
MethodInfo Annotation 作用为给方法添加相关信息，包括 author、date、version。

#### 4.2 定义

```java
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
@Inherited
public @interface MethodInfo {

    String author() default "trinea@gmail.com";

    String date();

    int version() default 1;
}
```

这里是 MethodInfo 的实现部分
(1). 通过 @interface 定义，注解名即为自定义注解名
(2). 注解配置参数名为注解类的方法名，且：
a. 所有方法没有方法体，没有参数没有修饰符，实际只允许 public & abstract 修饰符，默认为 public，不允许抛异常
b. 方法返回值只能是基本类型，String, Class, annotation, enumeration 或者是他们的一维数组
c. 若只有一个默认属性，可直接用 value() 函数。一个属性都没有表示该 Annotation 为 Mark Annotation
(3). 可以加 default 表示默认值

#### 4.3 元 Annotation

@Documented 是否会保存到 Javadoc 文档中
@Retention 保留时间，可选值 SOURCE（源码时），CLASS（编译时），RUNTIME（运行时），默认为 CLASS，SOURCE 大都为 Mark Annotation，这类 Annotation 大都用来校验，比如 Override, SuppressWarnings
@Target 可以用来修饰哪些程序元素，如 TYPE, METHOD, CONSTRUCTOR, FIELD, PARAMETER 等，未标注则表示可修饰所有
@Inherited 是否可以被继承，默认为 false

### 5. Annotation 解析

#### 5.1 运行时 Annotation 解析

(1) 运行时 Annotation 指 @Retention 为 RUNTIME 的 Annotation，可手动调用下面常用 API 解析

```java
method.getAnnotation(AnnotationName.class);
method.getAnnotations();
method.isAnnotationPresent(AnnotationName.class);
```

其他 @Target 如 Field，Class 方法类似
getAnnotation(AnnotationName.class) 表示得到该 Target 某个 Annotation 的信息，因为一个 Target 可以被多个 Annotation 修饰
getAnnotations() 则表示得到该 Target 所有 Annotation
isAnnotationPresent(AnnotationName.class) 表示该 Target 是否被某个 Annotation 修饰
(2) 解析示例如下：

```java
public static void main(String[] args) {
    try {
        Class cls = Class.forName("cn.trinea.java.test.annotation.App");
        for (Method method : cls.getMethods()) {
            MethodInfo methodInfo = method.getAnnotation(
MethodInfo.class);
            if (methodInfo != null) {
                System.out.println("method name:" + method.getName());
                System.out.println("method author:" + methodInfo.author());
                System.out.println("method version:" + methodInfo.version());
                System.out.println("method date:" + methodInfo.date());
            }
        }
    } catch (ClassNotFoundException e) {
        e.printStackTrace();
    }
}
```

以之前自定义的 MethodInfo 为例，利用 Target（这里是 Method）getAnnotation 函数得到 Annotation 信息，然后就可以调用 Annotation 的方法得到响应属性值

#### 5.2 编译时 Annotation 解析

(1) 编译时 Annotation 指 @Retention 为 CLASS 的 Annotation，甴编译器自动解析。需要做的
a. 自定义类集成自 AbstractProcessor
b. 重写其中的 process 函数
这块很多同学不理解，实际是编译器在编译时自动查找所有继承自 AbstractProcessor 的类，然后调用他们的 process 方法去处理
(2) 假设 MethodInfo 的 @Retention 为 CLASS，解析示例如下：

```java
@SupportedAnnotationTypes({ "cn.trinea.java.test.annotation.MethodInfo" })
public class MethodInfoProcessor extends AbstractProcessor {

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment env) {
        HashMap<String, String> map = new HashMap<String, String>();
        for (TypeElement te : annotations) {
            for (Element element : env.getElementsAnnotatedWith(te)) {
                MethodInfo methodInfo = element.getAnnotation(MethodInfo.class);
                map.put(element.getEnclosingElement().toString(), methodInfo.author());
            }
        }
        return false;
    }
}
```

SupportedAnnotationTypes 表示这个 Processor 要处理的 Annotation 名字。
process 函数中参数 annotations 表示待处理的 Annotations，参数 env 表示当前或是之前的运行环境
process 函数返回值表示这组 annotations 是否被这个 Processor 接受，如果接受后续子的 rocessor 不会再对这个 Annotations 进行处理

### 6. 几个 Android 开源库 Annotation 原理简析

#### 6.1 Annotation — Retrofit

(1) 调用

```java
@GET("/users/{username}")
User getUser(@Path("username") String username);
```

(2) 定义

```java
@Documented
@Target(METHOD)
@Retention(RUNTIME)
@RestMethod("GET")
public @interface GET {
  String value();
}
```

从定义可看出 Retrofit 的 Get Annotation 是运行时 Annotation，并且只能用于修饰 Method
(3) 原理

```java
private void parseMethodAnnotations() {
    for (Annotation methodAnnotation : method.getAnnotations()) {
    Class<? extends Annotation> annotationType = methodAnnotation.annotationType();
    RestMethod methodInfo = null;

    for (Annotation innerAnnotation : annotationType.getAnnotations()) {
        if (RestMethod.class == innerAnnotation.annotationType()) {
            methodInfo = (RestMethod) innerAnnotation;
            break;
        }
    }
    ……
    }
}   
```

[RestMethodInfo.java](https://github.com/square/retrofit/blob/master/retrofit/src/main/java/retrofit/RestMethodInfo.java) 的 parseMethodAnnotations 方法如上，会检查每个方法的每个 Annotation， 看是否被 RestMethod 这个 Annotation 修饰的 Annotation 修饰，这个有点绕，就是是否被 GET、DELETE、POST、PUT、HEAD、PATCH 这些 Annotation 修饰，然后得到 Annotation 信息，在对接口进行动态代理时会掉用到这些 Annotation 信息从而完成调用。

Retrofit 原理涉及到[动态代理](http://a.codekk.com/detail/Android/Caij/公共技术点之 Java 动态代理)，这里原理都只介绍 Annotation，具体原理分析请见 [Android 开源项目实现原理解析](http://a.codekk.com/)

#### 6.2 Annotation — Butter Knife

(1) 调用

```java
@InjectView(R.id.user) 
EditText username;
```

(2) 定义

```java
@Retention(CLASS) 
@Target(FIELD)
public @interface InjectView {
  int value();
}
```

可看出 Butter Knife 的 InjectView Annotation 是编译时 Annotation，并且只能用于修饰属性
(3) 原理

```java
@Override 
public boolean process(Set<? extends TypeElement> elements, RoundEnvironment env) {
    Map<TypeElement, ViewInjector> targetClassMap = findAndParseTargets(env);

    for (Map.Entry<TypeElement, ViewInjector> entry : targetClassMap.entrySet()) {
        TypeElement typeElement = entry.getKey();
        ViewInjector viewInjector = entry.getValue();

        try {
            JavaFileObject jfo = filer.createSourceFile(viewInjector.getFqcn(), typeElement);
            Writer writer = jfo.openWriter();
            writer.write(viewInjector.brewJava());
            writer.flush();
            writer.close();
        } catch (IOException e) {
            error(typeElement, "Unable to write injector for type %s: %s", typeElement, e.getMessage());
        }
    }

    return true;
}
```

[ButterKnifeProcessor.java](https://github.com/JakeWharton/butterknife/blob/master/butterknife/src/main/java/butterknife/internal/ButterKnifeProcessor.java) 的 process 方法如上，编译时，在此方法中过滤 InjectView 这个 Annotation 到 targetClassMap 后，会根据 targetClassMap 中元素生成不同的 class 文件到最终的 APK 中，然后在运行时调用 ButterKnife.inject(x) 函数时会到之前编译时生成的类中去找。 这里原理都只介绍 Annotation，具体原理分析请见 [Android 开源项目实现原理解析](http://a.codekk.com/)

#### 6.3 Annotation — ActiveAndroid

(1) 调用

```java
@Column(name = “Name") 
public String name;
```

(2) 定义

```java
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Column {
  ……
}
```

可看出 ActiveAndroid 的 Column Annotation 是运行时 Annotation，并且只能用于修饰属性。
(3) 原理

```java
Field idField = getIdField(type);
mColumnNames.put(idField, mIdName);

List<Field> fields = new LinkedList<Field>(ReflectionUtils.getDeclaredColumnFields(type));
Collections.reverse(fields);

for (Field field : fields) {
    if (field.isAnnotationPresent(Column.class)) {
        final Column columnAnnotation = field.getAnnotation(Column.class);
        String columnName = columnAnnotation.name();
        if (TextUtils.isEmpty(columnName)) {
            columnName = field.getName();
        }

        mColumnNames.put(field, columnName);
    }
}
```

[TableInfo.java](https://github.com/pardom/ActiveAndroid/blob/master/src/com/activeandroid/TableInfo.java) 的构造函数如上，运行时，得到所有行信息并存储起来用来构件表信息。

这里原理都只介绍 Annotation，具体原理分析请见 [Android 开源项目实现原理解析](http://a.codekk.com/)