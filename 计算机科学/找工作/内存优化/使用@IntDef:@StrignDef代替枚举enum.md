[TOC]

### 一、概述

Enum 是 java 中一种包含固定常数的类型
 当我们需要预先定义一些值，并限定范围时，使用 Enum，来做到编写和编译都查错

- Java 的 Enum 的实质是特殊单例的静态成员变量
- Enum 可以在编写器，编译器做到各种静态检查防呆
- Enum 在运行期，所有枚举类作为单例，全部加载到内存中

因为上述原因，Enum 增加了APK 的内存占用，比常量多5到10倍的内存占用
 所以放弃枚举，就是关于安卓应用性能的内存占用部分的最佳实践方法之一

#### Android 单例类占用内存原理

单例增加了APK 的内存占用的原因是

- apk 经常初始化加载到虚拟机，作为一个新的用户应用来使用，而服务端 java 应用不怎么初始化加载到虚拟机
- 另外每个 Android 应用的可用内存是非常有限的

因为 Android 系统下，每个 APP 就是一个独立用户，Android 系统分配给这个用户的内存非常小，一般只有 32MB 64MB 左右
 这个配置一般在文件 `/system/build.prop`中
 具体看 ROM 厂商的定制，别希望 ROM 厂商修改这个的大小

- 初始化单例太多，设备在不能处理情况下，非常容易出现 ANR 错误
- AndroidManifest.xml 中 application 节点下 android:largeHeap 开启也不会给太多额外的内存

`/system/build.prop` 中

```
dalvik.vm.heapsize=128m
dalvik.vm.heapgrowthlimit=64m
```

- heapgrowthlimit 是普通应用的内存限制
- heapsize 是开启后的内存限制，也没大多少



### 二、为什么要使用枚举

举一个例子，例如我们要为一个bean
 赋值一个person的性别属性，因为性别只有男女，所以我们通常的做法是定义两个整型int，来区分 `男女`性别

```java
public class SexTest{
private final int MAN = 101, WOMEN = 102;
private int sex;

//设置性别
public void setSex(int sex){
    this.sex = sex;
}

//获取性别
public String getSex(){
    if(MAN == sex) return "男";
    if(WOMEN == sex) return "女";
    return "未知";
}

public static void main(String[] args){
    setSex(101);
    int sex = getSex();
    System.out.println("sex: " + sex);
    //输出：sex: 男
    //设置为102入参
    setSex(102);
    String resultSex = getSex();
    System.out.println("resultSex: " + resultSex);
    //输出：resultSex: 未知
    }
}
```

由上面的例子可以看出
 当我们定义了一个男女的final整型作为入参时，不一定保证入参的都是我们想要的入参
 这里就有一个 `类型不安全` 的问题出现，而枚举就可以解决这个问题

首先定义一个枚举类，里面有男，女两个枚举常量

```java
public class SexTest{

    public static enum Sex {
        MAN, WOMEN
    }

    private Sex sex;
    //设置性别
    public void setSex(Sex sex){
        this.sex = sex;
    }

    //获取性别
    public String getSex(){
        if(Sex.MAN == sex) return "男";
        if(Sex.WOMEN == sex) return "女";
        return "未知";
    }

    public static void main(String[] args){
    //这里的入参必须为Sex枚举类中的其中一个枚举常量
    //绝对不允许输入没有再Sex枚举里面定义的常量
    setSex(Sex.MAN);
    String resultSex = getSex();
    System.out.println("resultSex: " + resultSex);
    //out：resultSex: 男
    }
}
```

- 利用枚举，在 `setSex()` 方法里面对入参做了枚举Sex的限制
- 对于想输入任何非枚举类Sex里面定义的枚举常量，编译都是不能通过的
- 这就很好的限制了入参混乱的问题

#### 使用 Enum 的缺点

- 每一个枚举值都是一个单例对象,在使用它时会增加额外的内存消耗,所以枚举相比与 Integer 和 String 会占用更多的内存
- 较多的使用 Enum 会增加 DEX 文件的大小,会造成运行时更多的IO开销,使我们的应用需要更多的空间
- 特别是分dex多的大型APP，枚举的初始化很容易导致ANR



### 三、不使用枚举类型的解决方案

既然是因为参数的类型太泛了造成的类型不安全，那么我只要将参数限定在某一个类型集合里面
 要将的`@IntDef/@StringDef + @interface`来进行限定参数

`build.gradle` 文件中添加依赖

```groovy
dependencies {
    compileOnly 'com.android.support:support-annotations:25.1.0'
}
```

> 也可以使用对应的版本 compileOnly 是不会让 support-annotations 到下一个依赖库，如果想让下一个库依赖，请使用 api
>  特别的，如果是 app 出包，依赖必须使用 implementation

然后就可以使用注解帮助检查参数，代码如下

```java
public class SexTest {
public final int MAN = 2;
public final int WOMEN = 3;

    /**
     * 只能使用 {@link #MAN} {@link #WOMEN}
     */
    @Documented // 表示开启Doc文档
    @IntDef({
        MAN,
        WOMEN，
    }) //限定为MAN,WOMEN
    @Target({
            ElementType.PARAMETER,
            ElementType.FIELD,
            ElementType.METHOD,
    }) //表示注解作用范围，参数注解，成员注解，方法注解
    @Retention(RetentionPolicy.SOURCE) //表示注解所存活的时间,在运行时,而不会存在 .class 文件中
    public @interface Sex { //接口，定义新的注解类型
    }
    public void setSex(@Sex int sex){
        this.sex = sex;
    }
    public static void main(String[] args){
        setSex(MAN);
    }
}
```

> 如果我们尝试在调用setSex()方法的时候，传入不在限定之内的值，那么编译就不会通过，有错误提示

同理，我们也可以使用@StringDef

```java
public class FlagContants {
    public static final String UNDEFINE = "undefine";
    public static final String OK = "ok";
    public static final String ERROR = "error";

    private @FlagDef
    String flag = UNDEFINE;

    @Documented // 表示开启Doc文档
    @StringDef({
            OK,
            ERROR
    }) //限定为 FlagContants.OK, FlagContants.ERROR
    @Target({
            ElementType.PARAMETER,
            ElementType.FIELD,
            ElementType.METHOD,
    }) //表示注解作用范围，参数注解，成员注解，方法注解
    @Retention(RetentionPolicy.SOURCE) //表示注解所存活的时间,在运行时,而不会存在 .class 文件中
    public @interface FlagDef { //接口，定义新的注解类型
    }

    public @FlagDef
    String getFlag() {
        return flag;
    }

    public void setFlag(@FlagDef String flag) {
        this.flag = flag;
    }
}
```



### 四、AndroidStuido @*Def 模板

自动模板，用于快速生产这种枚举类(智能模板不包括引包，引入包请手动，或者配置自动引入)

#### @IntDef 模板

- 分组 `Android` 
- 名称 Abbreviation `defInt` 
- 描述 Description `add Android IntDef Source block` 
- Template text

```java
    @IntDef({
        $END$
    })

    @Documented
    @Target({
        ElementType.FIELD,
        ElementType.METHOD,
        ElementType.PARAMETER,
    })
    @Retention(RetentionPolicy.SOURCE)
    public @interface $IntDefName$Def {
    }
```

- 生效范围 Application in `Java:declaration` 
- Edit variables

| Name       | Expression          | Default value | Skip if define |
| :--------- | :------------------ | :------------ | :------------- |
| IntDefName | classNameComplete() |               | No             |

#### @StringDef 模板

- 分组 `Android` 
- 名称 Abbreviation `defString` 
- 描述 Description `add Android StringDef Source block` 
- Template text

```java
    @StringDef({
        $END$
    })

    @Documented
    @Target({
        ElementType.FIELD,
        ElementType.METHOD,
        ElementType.PARAMETER,
    })
    @Retention(RetentionPolicy.SOURCE)
    public @interface $StringDefName$Def {
    }
```

- 生效范围 Application in `Java:declaration` 
- Edit variables

| Name          | Expression          | Default value | Skip if define |
| :------------ | :------------------ | :------------ | :------------- |
| StringDefName | classNameComplete() |               | No             |



### 五、注解生命周期

- RetentionPolicy.SOURCE 源码注解，编译成.class文件后注解就不存在，用来提示开发者
- RetentionPolicy.CLASS CLASS汇编注解，编译成.class文件后注解也还存在，用于自动生成代码
- RetentionPolicy.RUNTIME 运行时动态注解，生命周期一直程序运行时都存在，常用于自动注入



### 链接：

> 本文直接copy自底下的链接

- https://www.jianshu.com/p/80180e1728d0

