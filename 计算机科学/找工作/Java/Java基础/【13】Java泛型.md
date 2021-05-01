[TOC]

![泛型](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/s9DppOzM2labC8Qa39R7.dBTYAdWZhLaOVapAN0MbnE!/r/dD4BAAAAAAAA)

## 1.特性

泛型只在编译阶段有效。看下面的代码：

```java
List<String> stringArrayList = new ArrayList<String>();
List<Integer> integerArrayList = new ArrayList<Integer>();

Class classStringArrayList = stringArrayList.getClass();
Class classIntegerArrayList = integerArrayList.getClass();

if(classStringArrayList.equals(classIntegerArrayList)){
    Log.d("泛型测试","类型相同");
}
```

输出结果：`D/泛型测试: 类型相同`。

通过上面的例子可以证明，在编译之后程序会采取去泛型化的措施。也就是说Java中的泛型，只在编译阶段有效。在编译过程中，正确检验泛型结果后，会将泛型的相关信息擦出，并且在对象进入和离开方法的边界处添加类型检查和类型转换的方法。也就是说，泛型信息不会进入到运行时阶段。

**对此总结成一句话：泛型类型在逻辑上看以看成是多个不同的类型，实际上都是相同的基本类型。**

## 2.泛型类：

```java
//此处T可以随便写为任意标识，常见的如T、E、K、V等形式的参数常用于表示泛型
//在实例化泛型类时，必须指定T的具体类型
public class Generic<T>{ 
    //key这个成员变量的类型为T,T的类型由外部指定  
    private T key;

    public Generic(T key) { //泛型构造方法形参key的类型也为T，T的类型由外部指定
        this.key = key;
    }

    public T getKey(){ //泛型方法getKey的返回值类型为T，T的类型由外部指定
        return key;
    }
}
```

### 调用

```java
   Generic<String> ck=new Generic<String>("chengkun");
   String s= ck.getKey;
```

## 3.泛型接口

> 类定义的写法有点区别

泛型接口与泛型类的定义及使用基本相同。泛型接口常被用在各种类的生产器中，可以看一个例子：

```java
//定义一个泛型接口
public interface Generator<T> {
    public T next();
}
```

### 当实现泛型接口的类，未传入泛型实参时：

```java
/**
 * 未传入泛型实参时，与泛型类的定义相同，在声明类的时候，需将泛型的声明也一起加到类中
 * 即：class FruitGenerator<T> implements Generator<T>{
 * 如果不声明泛型，如：class FruitGenerator implements Generator<T>，编译器会报错："Unknown class"
 */
class FruitGenerator<T> implements Generator<T>{
    @Override
    public T next() {
        return null;
    }
}
```

### 当实现泛型接口的类，传入泛型实参时：

```java
/**
 * 传入泛型实参时：
 * 定义一个生产器实现这个接口,虽然我们只创建了一个泛型接口Generator<T>
 * 但是我们可以为T传入无数个实参，形成无数种类型的Generator接口。
 * 在实现类实现泛型接口时，如已将泛型类型传入实参类型，则所有使用泛型的地方都要替换成传入的实参类型
 * 即：Generator<T>，public T next();中的的T都要替换成传入的String类型。
 */
public class FruitGenerator implements Generator<String> {

    private String[] fruits = new String[]{"Apple", "Banana", "Pear"};

    @Override
    public String next() {
        Random rand = new Random();
        return fruits[rand.nextInt(3)];
    }
}
```

## 4.泛型通配符

我们知道`Ingeter`是`Number`的一个子类，同时在特性章节中我们也验证过`Generic<Ingeter>`与`Generic<Number>`实际上是相同的一种基本类型。那么问题来了，在使用`Generic<Number>`作为形参的方法中，能否使用`Generic<Ingeter>`的实例传入呢？在逻辑上类似于`Generic<Number>`和`Generic<Ingeter>`是否可以看成具有父子关系的泛型类型呢？

为了弄清楚这个问题，我们使用`Generic<T>`这个泛型类继续看下面的例子：

```java
public void showKeyValue1(Generic<Number> obj){
    Log.d("泛型测试","key value is " + obj.getKey());
}123
```

```java
Generic<Integer> gInteger = new Generic<Integer>(123);
Generic<Number> gNumber = new Generic<Number>(456);

showKeyValue(gNumber);//可以，因为是同一类型

// showKeyValue(gInteger)这个方法编译器会为我们报错：Generic<java.lang.Integer> 
// cannot be applied to Generic<java.lang.Number>
// showKeyValue(gInteger);12345678
```

通过提示信息我们可以看到`Generic<Integer>`不能被看作为``Generic<Number>`的子类。由此可以看出:**同一种泛型可以对应多个版本（因为参数类型是不确定的），不同版本的泛型类实例是不兼容的**。

回到上面的例子，如何解决上面的问题？总不能为了定义一个新的方法来处理`Generic<Integer>`类型的类，这显然与java中的多台理念相违背。因此我们需要一个在逻辑上可以表示**同时**是`Generic<Integer>`和`Generic<Number>`父类的引用类型。由此类型通配符应运而生。

我们可以将上面的方法改一下：

```java
public void showKeyValue1(Generic<?> obj){
    Log.d("泛型测试","key value is " + obj.getKey());
}
```

类型通配符一般是使用？代替具体的类型实参，注意了，**此处’？’是类型实参，而不是类型形参** 。重要说三遍！**此处’？’是类型实参，而不是类型形参** ！ **此处’？’是类型实参，而不是类型形参** ！再直白点的意思就是，此处的？和Number、String、Integer一样都是一种实际的类型，可以把？看成所有类型的父类。是一种真实的类型。

可以解决当具体类型不确定的时候，这个通配符就是 **?**  ；当操作类型时，不需要使用类型的具体功能时，只使用Object类中的功能。那么可以用 ? 通配符来表未知类型。



## 5.泛型方法

**泛型类，是在实例化类的时候指明泛型的具体类型；泛型方法，是在调用方法的时候指明泛型的具体类型** 。 

```java
  class A<T>{
        public T fun1(){}
        public void fun2(T t){}
        //以上两个都不是泛型方法，他们是泛型类里面的一个方法
        //发现方法要求需要在方法上有泛型的定义
        public <T> T fun3(){}//此为泛型方法
    }
	
/*     1）public 与 返回值中间<T>非常重要，可以理解为声明此方法为泛型方法。
 *     2）只有声明了<T>的方法才是泛型方法，泛型类中的使用了泛型的成员方法并不是泛型方法。
 *     3）<T>表明该方法将使用泛型类型T，此时才可以在方法中使用泛型类型T。
 *     4）与泛型类的定义一样，此处T可以随便写为任意标识，常见的如T、E、K、V等形式的参数常用于表示泛型
 */
```

### 泛型方法的基本用法

光看上面的例子有的同学可能依然会非常迷糊，我们再通过一个例子，把我泛型方法再总结一下。

```java
public class GenericTest {
   //这个类是个泛型类，在上面已经介绍过
   public class Generic<T>{     
        private T key;

        public Generic(T key) {
            this.key = key;
        }

        //我想说的其实是这个，虽然在方法中使用了泛型，但是这并不是一个泛型方法。
        //这只是类中一个普通的成员方法，只不过他的返回值是在声明泛型类已经声明过的泛型。
        //所以在这个方法中才可以继续使用 T 这个泛型。
        public T getKey(){
            return key;
        }

        /**
         * 这个方法显然是有问题的，在编译器会给我们提示这样的错误信息"cannot reslove symbol E"
         * 因为在类的声明中并未声明泛型E，所以在使用E做形参和返回值类型时，编译器会无法识别。
        public E setKey(E key){
             this.key = keu
        }
        */
    }

    /** 
     * 这才是一个真正的泛型方法。
     * 首先在public与返回值之间的<T>必不可少，这表明这是一个泛型方法，并且声明了一个泛型T
     * 这个T可以出现在这个泛型方法的任意位置.
     * 泛型的数量也可以为任意多个 
     *    如：public <T,K> K showKeyName(Generic<T> container){
     *        ...
     *        }
     */
    public <T> T showKeyName(Generic<T> container){
        System.out.println("container key :" + container.getKey());
        //当然这个例子举的不太合适，只是为了说明泛型方法的特性。
        T test = container.getKey();
        return test;
    }

    //这也不是一个泛型方法，这就是一个普通的方法，只是使用了Generic<Number>这个泛型类做形参而已。
    public void showKeyValue1(Generic<Number> obj){
        Log.d("泛型测试","key value is " + obj.getKey());
    }

    //这也不是一个泛型方法，这也是一个普通的方法，只不过使用了泛型通配符?
    //同时这也印证了泛型通配符章节所描述的，?是一种类型实参，可以看做为Number等所有类的父类
    public void showKeyValue2(Generic<?> obj){
        Log.d("泛型测试","key value is " + obj.getKey());
    }

     /**
     * 这个方法是有问题的，编译器会为我们提示错误信息："UnKnown class 'E' "
     * 虽然我们声明了<T>,也表明了这是一个可以处理泛型的类型的泛型方法。
     * 但是只声明了泛型类型T，并未声明泛型类型E，因此编译器并不知道该如何处理E这个类型。
    public <T> T showKeyName(Generic<E> container){
        ...
    }  
    */

    /**
     * 这个方法也是有问题的，编译器会为我们提示错误信息："UnKnown class 'T' "
     * 对于编译器来说T这个类型并未项目中声明过，因此编译也不知道该如何编译这个类。
     * 所以这也不是一个正确的泛型方法声明。
    public void showkey(T genericObj){

    }
    */

    public static void main(String[] args) {


    }
}
```

### 类中的泛型方法

当然这并不是泛型方法的全部，泛型方法可以出现杂任何地方和任何场景中使用。但是有一种情况是非常特殊的，当泛型方法出现在泛型类中时，我们再通过一个例子看一下

```java
public class GenericFruit {
    class Fruit{
        @Override
        public String toString() {
            return "fruit";
        }
    }

    class Apple extends Fruit{
        @Override
        public String toString() {
            return "apple";
        }
    }

    class Person{
        @Override
        public String toString() {
            return "Person";
        }
    }

    class GenerateTest<T>{
        public void show_1(T t){
            System.out.println(t.toString());
        }

        //在泛型类中声明了一个泛型方法，使用泛型E，这种泛型E可以为任意类型。可以类型与T相同，也可以不同。
        //由于泛型方法在声明的时候会声明泛型<E>，因此即使在泛型类中并未声明泛型，编译器也能够正确识别泛型方法中识别的泛型。
        public <E> void show_3(E t){
            System.out.println(t.toString());
        }

        //在泛型类中声明了一个泛型方法，使用泛型T，注意这个T是一种全新的类型，可以与泛型类中声明的T不是同一种类型。
        public <T> void show_2(T t){
            System.out.println(t.toString());
        }
    }

    public static void main(String[] args) {
        Apple apple = new Apple();
        Person person = new Person();

        GenerateTest<Fruit> generateTest = new GenerateTest<Fruit>();
        //apple是Fruit的子类，所以这里可以
        generateTest.show_1(apple);
        //编译器会报错，因为泛型类型实参指定的是Fruit，而传入的实参类是Person
        //generateTest.show_1(person);

        //使用这两个方法都可以成功
        generateTest.show_2(apple);
        generateTest.show_2(person);

        //使用这两个方法也都可以成功
        generateTest.show_3(apple);
        generateTest.show_3(person);
    }
}
```

### 泛型方法与可变参数

再看一个泛型方法和可变参数的例子：

```java
public <T> void printMsg( T... args){
    for(T t : args){
        Log.d("泛型测试","t is " + t);
    }
}

printMsg("111",222,"aaaa","2323.4",55.55);
```

### 静态方法与泛型

静态方法有一种情况需要注意一下，那就是在类中的静态方法使用泛型：**静态方法无法访问类上定义的泛型；如果静态方法操作的引用数据类型不确定的时候，必须要将泛型定义在方法上。**

即：**如果静态方法要使用泛型的话，必须将静态方法也定义成泛型方法** 。

```java
public class StaticGenerator<T> {
    ....
    ....
    /**
     * 如果在类中定义使用泛型的静态方法，需要添加额外的泛型声明（将这个方法定义成泛型方法）
     * 即使静态方法要使用泛型类中已经声明过的泛型也不可以。
     * 如：public static void show(T t){..},此时编译器会提示错误信息：
          "StaticGenerator cannot be refrenced from static context"
     */
    public static <T> void show(T t){

    }
}
```



### 协变与逆变

- JAVA中的上界： 泛型是用于规范代码输入的编译限制，使用<T extend 某个类> 的语法可以规定泛型的上界，但是会使得集合只能使用add方法不能使用get方法，具体原因是：

![image-20201214003103048](https://tva1.sinaimg.cn/large/0081Kckwly1glmp4dqfpwj30aq080dfx.jpg)

假如在上面得类继承关系中，我们定义 List\<T extend Father> list= List\<Child1>(),但我们操作这个List时，可以向数组中添加Child1和Child2的实例（子类能被转换成父类），却不能使用其get()方法，因为此时我们不能确定从List中获取的到底是Child1还是Child2类，只能获取到Father类，但是如果我们后面强制转换错误的话就会导致程序出错，因此集合干脆禁止了这种情况下的get方法的获取。

- JAVA中的下界：使用<T super某个类> 的语法可以规定泛型的下界，但是同样会有限制，导致集合没法调用add方法，只能使用get方法，具体原因是：

![image-20201214003110459](https://tva1.sinaimg.cn/large/0081Kckwly1glmp4cgozbj30b207ejrh.jpg)

定义 List\<T super Child> list= List\<Father1>()，此时我们调用list的get方法可以直接获得Child的实例类不用强转，但是在调用add方法时，由于父类无法转换为子类,因此无法确定是插入的是Father1还是Father2，毕竟这两个类是无法同存在一个数组中的，因此无法调用add方法（或者只能插入Object类，应为该类是所有类的共同父类）。

- 而在Kotlin中协变与逆变可以用in和out关键字定义，in表示只能输入，out表示只能输出，即对应生产者和消费者。

---

# 一、协变逆变概念

**逆变与协变用来描述类型转换（type transformation）后的继承关系：A、B表示类型，f(·)表示类型转换，A<=B表示A为B的子类，那么则存在：**

- **f(·)是协变的：当A<=B  ,f(A)<=f(B)成立**
- **f(·)是逆变的：当A<=B  ,f(A)>=f(B)成立**
- **f(·)是不变的：当A<=B  ,f(A) 和f(B)不存在继承关系**

看的有点懵逼？先别着急，等会儿回过头来再看这个。。这里介绍了协变和逆变的概念，对于java中数组是协变的，如下所示：

```java
    public static void main(String[] args) {
        String[] strings = new String[5];
        Object[] objects = strings;
    }
```

实例中创建了一个字符串数组对象，但是用Object数组同样可以引用。

实例中String类<=Object类，对应的String[]<=Object[]，所以可以得出数组是协变类型。

**现在问题来了：**

现在将string类型的数组引用赋值给了object类型的数组引用，在操作时候是不是可以赋值除了string意外的类型呢？

```java
    public static void main(String[] args) {
        String[] strings = new String[5];
        Object[] objects = strings;
        try {
            objects[0] = 1;
            System.out.println(strings[0]);
        } catch (Exception e) {
            System.out.println("出错了吧。。。。。");
            e.printStackTrace();
        }
    }
```

给objects第一个元素设置一个int类型的1，结果如下：

```
java.lang.ArrayStoreException: java.lang.Integer
出错了吧。。。。。
	at as.a.Str.main(Str.java:12)
```



看来**数组以协变方式允许类型向上转型，但是会有写入安全的问题，如上异常**

 

现在我们看下在集合中使用会是怎么样的：

```java
    public static void main(String[] args) {
        String[] strings = new String[5];
        Object[] objects = strings;
        try {
            objects[0] = 1;
            System.out.println(strings[0]);
        } catch (Exception e) {
            System.out.println("出错了吧。。。。。");
            e.printStackTrace();
        }
 
 
        List<String> strList = new ArrayList<>();
        List<Object> objList = strList;//编译错误了
    }
```

在将strList赋值给objList时候，已经出现编译错误了，错误结果如下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glmp6cbuh7j30no07ign8.jpg)

使用泛型时，在编译期间存在泛型擦除过程，取消了运行时检查，所以将泛型的错误检查提前到了编译器。并不是因为是两个不同的类型。

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmp6bjke4j316y0d8abe.jpg" alt="img" style="zoom:50%;" />

这时候用到了泛型通配符? extends T 和 ? super T了。。首先示例的继承关系如下：

 

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmp65ujobj30d80jut9b.jpg" alt="img" style="zoom:50%;" />

```java
 
class 生物 {
}
 
class 动物 extends 生物 {
}
 
class 人 extends 动物 {
}
 
class 狗 extends 动物 {
}
 
class 山顶洞人 extends 人 {
}
 
class 半坡人 extends 人 {
 
}
```

示例如下：

```java
public class Str {
    public static void main(String[] args) {
 
        List<? extends 动物> objList = new ArrayList<人>();
        动物 动物 = objList.get(0);//编译通过
        生物 动物1 = objList.get(0);//编译通过
        人 人 = objList.get(0);//编译错误
        objList.add(new 动物());//编译错误
        objList.add(new 人());//编译错误
        objList.add(new 狗());//编译错误
    }
 
}
```

示例中将动物和生物类型引用objList的元素时，编译无错误，但是将人类型引用objList元素时，编译出错了。然后，，，，，，不管什么类型，只要add就全都编译错误了。



我是这样想的，如果说他允许add T及其子类对象，那他是如何知道哪些类型的对象是应该添加的呢？举个简单的?，List<? extends 动物> 存放都是动物的子类，但是无法确定是哪一个子类，这种情况下依然会出现安全问题（如上栗中String数组中+int）；而接收引用也同样是这个道理：我存放的是你T的子类，但是无不知道啊，那我接收的引用只要是你的父类就好啦。

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmp5v51ebj30ow0i6gna.jpg" alt="img" style="zoom:50%;" />

这里简单总结一下上限通配符? extends T 的用法**，? extends T表示所存储类型都是T及其子类，但是获取元素所使用的引用类型只能是T或者其父类。使用上限通配符实现向上转型，但是会失去存储对象的能力。上限通配符为集合的协变表示**

想要存储对象，就需要下限通配符 ？super T 了，用法如下：

```java
    public static void main(String[] args) {
 
        List<? super 人> humList = new ArrayList<>();
        humList.add(new 半坡人());//编译通过
        humList.add(new 山顶洞人());//编译通过
        humList.add(new 人());//编译通过
        humList.add(new 动物());//编译失败
 
    }
```

相信大家一眼就看出来了，添加人及其子类没有错误，一旦再网上就出现编译错误了。

**下限通配符 ? super T表示 所存储类型为T及其父类，但是添加的元素类型只能为T及其子类，而获取元素所使用的类型只能是Object，因为Object为所有类的基类。下限通配符为集合的逆变表示。**

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmp5sb410j30ng0r6jt4.jpg" alt="img" style="zoom: 50%;" />

现在反过头来看一下最开始说的协变和逆变的概念：

- 当使用上限通配符时，类的等级越高，所包含的范围越大，符合协变的概念。
- 当使用下限通配符时，类的等级越高，所包含的范围越小，符合逆变的概念。

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmp5kryu3j30uq0u0tb0.jpg" alt="img" style="zoom:50%;" />

以下是笔者对以上内容的总结四句话：

【向上转型安全，向下转型不安全】

```java
?extends T 存放的类型一定为T及其子类，但是获取要用T或者其父类引用。//【转型一致性】

?super T 存放的类型一定为T的父类，但添加一定为T和其子类对象。//【转型一致性】

?extends T 进行add（T子类）编译出错：因为无法确定到底是哪个子类

?super T get（）对象，都是Object类型，因为T的最上层父类是Object，//【想要向下转型只能强转】。而向下转型是不安全的
```

```java
    public static void main(String[] args) {
        List<? extends Human> list = new ArrayList<>();
//     【之前List中就有元素，不允许后添加，会导致类型不一致】添加受限，下面三个全错
        list.add(new Human());//错误❌
        list.add(new MAN());//错误❌
        list.add(new Animal());//错误❌
        list.add(new WOMAN());//错误❌

//        获取的只能是Human及其父类
        MAN man = list.get(0);//错误❌
        WOMAN woman = list.get(0);//错误❌
        Human human = list.get(0);
        Animal animal = list.get(0);

        List<? super Human> list1 = new ArrayList<>();
        list1.add(new WOMAN());
        list1.add(new MAN());
        list1.add(new Human());
        list1.add(new Animal());//错误❌

//        获取受限【向下转型，如果没有强转就是错误】
        MAN a_human1 = (MAN) list1.get(0);//抛出转型异常
        Human human1 = (Human) list1.get(0);
        Animal animal1 = (Animal) list1.get(1);
    }
```

```java
  List<String> list=new ArrayList<>();
        list.add((String) new Object());
        list.add(new Integer(100))//不行
```

