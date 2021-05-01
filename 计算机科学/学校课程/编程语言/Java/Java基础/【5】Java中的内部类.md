[TOC]

> 匿名内部类形参为什么是final说一哈，内部类并不是直接调用方法传递的参数**，而是利用**自身的构造器**对传入的参数进行**备份**，自己内部方法调用的实际上时**自己的属性**而**不是外部方法传递进来的参数**。

## Java中的内部类

## 1.什么是内部类呢？（类级）

内部类就是在类的内部创建一个类，为什么我们要在类的内部创建一个类呢？不直接在类的外面直接创建另一个类呢？何必这么麻烦（**因为我定义的这个内部类仅仅在本类中是有用的，其他的类使用完全没有意义，所以我就定义在一个类的内部仅仅供给这个类来使用。**）



## 2.什么是匿名内部类呢？（方法级）

就更有意思了，就是所我定义的这个类在本类里面我就都认为他是没有意义的，因为我只需要提供给本类中的一个方法来使用，其他方法不需要使用嘛。（**所以我们就不在类的内部定义了，直接在一个方法中的返回符(;)之前我们就给他new ()并写出来，这样这个类就仅仅提供给这个方法使用。**)



## 3.使用场景

**就是定义的这个类如果提供给两个或者两个以上的方法使用时就是用内部类、如果仅仅提供给一个方法使用时可以使用匿名内部类** 



## 4.内部类例子

> 到后面匿名内部类统一讲解。

```java
public class InnerDemo {
 
    public int val = 12;// 变量
    public static int staticVal = 13;// 实例变量(常量)
    public final int FINALVAL = 23;
    public final static int FSVAL = 33;
 
    public static void main(String[] args) {
        InnerDemo demo = new InnerDemo();
        InnerClass innerClass = demo.new InnerClass();
        innerClass.change();
        System.out.println(demo.val);// 12 TODO 值没变
        System.out.println(staticVal);// 23 TODO 值改变了
        System.out.println(demo.FINALVAL);// 23
        System.out.println(FSVAL);// 33
    }
 
    class InnerClass {
        public void change() {
            staticVal = 23;   //可以改变实例变量的值
            // FINALVAL = 55; TODO 编译报错 The final field InnerDemo.FSVAL cannot be assigned
            // FSVAL = 44; TODO 编译报错 The final field InnerDemo.FSVAL cannot be assigned
            InnerDemo demo = new InnerDemo();
            demo.val = 33;
        }
    }
 
}
```



## 5.匿名内部类

### 5.1定义方法

匿名内部类由于没有名字，所以它的创建方式有点儿奇怪。创建格式如下： 

```java
new 父类构造器（参数列表）|实现接口（）  
    {  
     //匿名内部类的类体部分  
    }
```

### 5.2简单例子

```java
public abstract class Bird {
    private String name;
 
    public String getName() {
        return name;
    }
 
    public void setName(String name) {
        this.name = name;
    }
    
    public abstract int fly();
}
 
public class Test {
    
    public void test(Bird bird){
        System.out.println(bird.getName() + "能够飞 " + bird.fly() + "米");
    }
    
    public static void main(String[] args) {
        Test test = new Test();
        test.test(new Bird() {
            
            public int fly() {
                return 10000;
            }
            
            public String getName() {
                return "大雁";
            }
        });
    }
}
------------------
Output：
大雁能够飞 10000米
```

在Test类中，test()方法接受一个Bird类型的参数，同时我们知道一个抽象类是没有办法直接new的，我们必须要先有实现类才能new出来它的实现类实例。所以在mian方法中直接使用匿名内部类来创建一个Bird实例。

由于匿名内部类不能是抽象类，所以它必须要实现它的抽象父类或者接口里面所有的抽象方法。

对于这段匿名内部类代码其实是可以拆分为如下形式

```java
public class WildGoose extends Bird{
    public int fly() {
        return 10000;
    }
    
    public String getName() {
        return "大雁";
    }
}
 
WildGoose wildGoose = new WildGoose();
test.test(wildGoose);
```

在这里系统会创建一个继承自Bird类的匿名类的对象，该对象转型为对Bird类型的引用。

对于匿名内部类的使用它是存在一个缺陷的，就是它仅能被使用一次，创建匿名内部类时它会立即创建一个该类的实例，该类的定义会立即消失，所以匿名内部类是不能够被重复使用。对于上面的实例，如果我们需要对test()方法里面内部类进行多次使用，建议重新定义类，而不是使用匿名内部类

### 5.3注意事项

 在使用匿名内部类的过程中，我们需要注意如下几点：

      1、使用匿名内部类时，我们必须是继承一个类或者实现一个接口，但是两者不可兼得，同时也只能继承一个类或者实现一个接口。

      2、匿名内部类中是不能定义构造函数的。

      3、匿名内部类中不能存在任何的静态成员变量和静态方法。

      4、匿名内部类为局部内部类，所以局部内部类的所有限制同样对匿名内部类生效。

​      5、匿名内部类不能是抽象的，它必须要实现继承的类或者实现的接口的所有抽象方法。

 在这里我们看到使用匿名内部类我们**必须要继承一个父类或者实现一个接口**，当然也仅能只继承一个父类或者实现一个接口。同时它也是没有class关键字，这是因为匿名内部类是**直接使用new来生成一个对象的引用**。当然这个引用是隐式的。 

### 5.4使用的形参为什么要final

我们给匿名内部类传递参数的时候，若该形参在**内部类中需要被使用**，那么该形参**必须要为final**。也就是说：当**所在的方法的形参需要被内部类里面使用时，该形参必须为final。**

为什么必须要为final呢？

首先我们知道在内部类编译成功后，它会产生一个class文件，该class文件与外部类并不是同一class文件，**仅仅只保留对外部类的引用**。当外部类传入的参数需要被内部类调用时，从java程序的角度来看是直接被调用：

```java
public class OuterClass {
    public void display(final String name,String age){
        class InnerClass{
            void display(){
                System.out.println(name);
            }
        }
    }
}
```

从上面代码中看**好像**name参数应该**是被内部类直接调用**？**其实不然**，在java编译之后实际的操作如下： 

```java
public class OuterClass$InnerClass {
    public InnerClass(String name,String age){
        this.InnerClass$name = name;
        this.InnerClass$age = age;
    }
    
    public void display(){
        System.out.println(this.InnerClass$name + "----" + this.InnerClass$age );
    }
}
```

所以从上面代码来看，**内部类并不是直接调用方法传递的参数**，而是利用**自身的构造器**对传入的参数进行**备份**，自己内部方法调用的实际上时**自己的属性**而**不是外部方法传递进来的参数**。

直到这里还没有解释为什么是final？在内部类中的属性和外部方法的参数两者从**外表上**看是同一个东西，但**实际上却不是**，所以**他们两者是可以任意变化的**，也就是说在**内部类中我对属性的改变并不会影响到外部的形参**，而然这从程序员的角度来看这是不可行的，毕竟站在程序的角度来看这两个根本就是同一个，如果内部类该变了，而外部方法的形参却没有改变这是难以理解和不可接受的，所以为了保持参数的一致性，就规定使用final来避免形参的不改变。

**简单理解就是，拷贝引用，为了避免引用值发生改变，例如被外部类的方法修改等，而导致内部类得到的值不一致，于是用final来让该引用不可改变。**

故如果定义了一个匿名内部类，并且希望它使用一个其外部定义的参数，那么编译器会要求该参数引用是final的。

**在安卓开发中，在recyclerview的适配器这里用内部类比较常见啦。**

```java
    @Override
    @SuppressWarnings("unchecked")
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
       ......
    }
```

**注：在内部类中使用方法形参没问题，但是一旦进行修改编译器就会提示将参数定义成final。**



- 内部类里面使用外部类的局部变量时，其实就是内部类的对象在使用它，内部类对象生命周期中都可能调用它，而内部类试图访问外部方法中的局部变量时，外部方法的局部变量很可能已经不存在了，那么就得延续其生命，拷贝到内部类中，而拷贝会带来不一致性，从而需要使用final声明保证一致性。说白了，内部类会自动拷贝外部变量的引用，为了避免：1. 外部方法修改引用，而导致内部类得到的引用值不一致 2.内部类修改引用，而导致外部方法的参数值在修改前和修改后不一致。于是就用 final 来让该引用不可改变。

- 内部类通常都含有回调，引用那个匿名内部类的函数执行完了就没了，所以内部类中引用外面的局部变量需要是final的，这样在回调的时候才能找到那个变量，而如果是外围类的成员变量就不需要是final的，因为内部类本身都会含有一个外围了的引用（外围类.this），所以回调的时候一定可以访问到。例如：

```java
private Animator createAnimatorView(final View view, final int position) {
    MyAnimator animator = new MyAnimator();
    animator.addListener(new AnimatorListener() {
        @Override
        public void onAnimationEnd(Animator arg0) {
            Log.d(TAG, "position=" + position); 
        }
    });
    return animator;
}
```

内部类回调里访问position的时候createAnimatorView()早就执行完了，position如果不是final的，回调的时候肯定就无法拿到它的值了，因为局部变量在函数执行完了以后就被回收了。

- 我们反编译看一下，首先定义接口和匿名内部类：

```java
public interface MyInterface {
    void doSomething();
}

public class TryUsingAnonymousClass {
    public void useMyInterface() {
        final Integer number = 123;
        System.out.println(number);

        MyInterface myInterface = new MyInterface() {
            @Override
            public void doSomething() {
                System.out.println(number);
            }
        };
        myInterface.doSomething();

        System.out.println(number);
    }
}
```

```java
class TryUsingAnonymousClass$1
        implements MyInterface {
    private final TryUsingAnonymousClass this$0;
    private final Integer paramInteger;

    TryUsingAnonymousClass$1(TryUsingAnonymousClass this$0, Integer paramInteger) {
        this.this$0 = this$0;
        this.paramInteger = paramInteger;
    }

    public void doSomething() {
        System.out.println(this.paramInteger);
    }
}
```

可以看到名为number的局部变量是作为构造方法的参数传入匿名内部类的。

如果Java允许匿名内部类访问非final的局部变量的话，那我们就可以在TryUsingAnonymousClass$1中修改paramInteger，但是这不会对number的值有影响，因为它们是不同的reference。

这就会造成数据不同步的问题。

所以，**Java为了避免数据不同步的问题，做出了匿名内部类只可以访问final的局部变量的限制。**



**总结：**

- **之前是为了解决数据不同步，所以final，那么为什么final，因为java是值传递的。**