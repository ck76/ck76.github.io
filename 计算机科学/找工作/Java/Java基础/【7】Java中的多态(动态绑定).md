[TOC]

多态（Polymorphism）按字面的意思就是“多种状态”。在[面向对象语言](https://baike.baidu.com/item/面向对象语言/215742)中，接口的多种不同的实现方式即为多态。引用Charlie Calverts对多态的描述——多态性是允许你将父对象设置成为一个或更多的他的[子对象](https://baike.baidu.com/item/子对象/11001276)相等的技术，赋值之后，父对象就可以根据当前赋值给它的子对象的特性以不同的方式运作（摘自“Delphi4 [编程技术](https://baike.baidu.com/item/编程技术/2646621)内幕”）。简单的说，就是一句话：允许将子类类型的[指针](https://baike.baidu.com/item/指针/2878304)赋值给父类类型的指针。多态性在Object Pascal和C++中都是通过[虚函数](https://baike.baidu.com/item/虚函数/2912832)实现的。



**多态存在的三个必要条件**
一、要有继承；
二、要有重写；
三、父类引用指向子类对象。

**多态的好处：**
1.可替换性（substitutability）。多态对已存在代码具有可替换性。例如，多态对圆Circle类工作，对其他任何圆形几何体，如圆环，也同样工作。
2.可扩充性（extensibility）。多态对代码具有可扩充性。增加新的子类不影响已存在类的多态性、继承性，以及其他特性的运行和操作。实际上新加子类更容易获得多态功能。例如，在实现了圆锥、半圆锥以及半球体的多态基础上，很容易增添球体类的多态性。
3.接口性（interface-ability）。多态是超类通过方法签名，向子类提供了一个共同接口，由子类来完善或者覆盖它而实现的。如图8.3 所示。图中超类Shape规定了两个实现多态的接口方法，computeArea()以及computeVolume()。子类，如Circle和Sphere为了实现多态，完善或者覆盖这两个接口方法。
4.灵活性（flexibility）。它在应用中体现了灵活多样的操作，提高了使用效率。
5.简化性（simplicity）。多态简化对应用软件的代码编写和修改过程，尤其在处理大量对象的运算和操作时，这个特点尤为突出和重要。

---



### 一、区别

**绑定：**一个方法的调用与方法所在的类关联起来。java中的绑定分为静态绑定和动态绑定，又被称作前期绑定和后期绑定。

**静态绑定：**（final、static、private）在程序执行前已经被绑定，也就是说在编译过程中就已经知道这个方法是哪个类的方法，此时由编译器获取其他连接程序实现。

- 根据变量的静态类型，发生在编译器，不由JVM执行

> 应用场景：方法重载(OverLoad)

**动态绑定：**在运行根据具体对象的类型进行绑定。

- 根据变量的动态类型，发生在运行期，由JVM执行

>  应用场景：方法重载(Override)



### 二、静态绑定

```
1、方法重载：在同一个类中，方法名相同，参数列表不同的方法。
2、子类可以继承父类的除了构造方法之外的方法。
所以这个时候，在子类中就可以重载从父类继承过来的方法，但是其实继承父类的方法之后，这个方法就相当于是子类的方法，所以这个时候重载就满足在同一个类中。
```

private：不能被继承，则不能通过子类对象调用，而只能通过类本身的对象进行调用，所以可以说private方法和方法所属的类绑定；

final：final方法虽然可以被继承，但是不能被重写（覆盖），虽然子类对象可以调用，但是调用的都是父类中的final方法（因此可以看出当类中的方法声明为final的时候，一是为了防止方法被覆盖，而是为了有效关闭java的动态绑定）；

static：static方法可以被子类继承，但是不能被子类重写（覆盖），但是可以被子类隐藏。（这里意思是说如果父类里有一个static方法，它的子类里如果没有对应的方法，那么当子类对象调用这个方法时就会使用父类中的方法。而如果子类中定义了相同的方法，则会调用子类的中定义的方法。唯一的不同就是，当子类对象上转型为父类对象时，不论子类中有没有定义这个静态方法，该对象都会使用父类中的静态方法。因此这里说静态方法可以被隐藏而不能被覆盖。这与子类隐藏父类中的成员变量是一样的。隐藏和覆盖的区别在于，子类对象转换成父类对象后，能够访问父类被隐藏的变量和方法，而不能访问父类被覆盖的方法）。

- 优先级匹配

```java
public class Overload {  
      
    private static void sayHello(char arg){  
        System.out.println("hello char");  
    }  
  
    private static void sayHello(Object arg){  
        System.out.println("hello Object");  
    }  
      
    private static void sayHello(int arg){  
        System.out.println("hello int");  
    }  
      
    private static void sayHello(long arg){  
        System.out.println("hello long");  
    }  
      
// 测试代码
    public static void main(String[] args) {  
        sayHello('a');  
    }  
}  

// 运行结果
hello char
```

- 因为`‘a’`是一个`char`类型数据（即静态类型是`char`），所以会选择参数类型为`char`的重载方法。
- 若注释掉`sayHello(char arg)`方法，那么会输出

```
hello int
```
- 因为`‘a’`除了可代表字符串，还可代表数字97。因此**当没有最合适的sayHello(char arg)方式进行重载时，会选择第二合适（第二优先级）的方法重载，即sayHello(int arg)**

- 总结：当没有最合适的方法进行重载时，会选优先级第二高的的方法进行重载，如此类推。

- 优先级顺序为：`char>int>long>float>double>Character>Serializable>Object>...`

  其中`...`为变长参数，将其视为一个数组元素。变长参数的重载优先级最低。

  因为 `char`转型到 `byte`或 `short`的过程是不安全的，所以不会选择参数类型为`byte`或 `short`的方法进行重载，故优先级列表里也没有。

> 上面讲解的主要是 **基本数据类型**的优先级匹配问题
>
> 若是引用类型，则根据 **继承关系** 进行优先级匹配
>
> 注意只跟其编译时类型（即静态类型）相关



### 三、动态绑定

调用的方法依赖于隐式参数的实际类型，并且在运行时实现动态绑定。动态绑定的过程分为以下几个环节：

（1）编译器查看对象的声明类型和方法名；

（2）编译器查看调用方法时提供的参数类型。例如x.f("hello")，编译器将会挑选f(String)，而不是f(int)，由于存在类型转换（int转换为double），所以可能会更复杂。如果编译器没找到参数类型匹配的方法，或者发现有多个方法与之匹配，就会报告一个错误。

至此，编译器获得了需要调用的方法名字和参数类型。

（3）采用动态绑定调用方法的时候，一定调用与所引用对象的实际类型最合适的类的方法。如果x的实际类型是D，它是C类的子类，如果D定义了一个方法f(String)，就直接调用它，否则将在D类的超类中寻找f(String)，以此类推。

每次调用方法都要进行搜索，时间开销太大，所以虚拟机预先为每个类创建一个方法表（method table），其中列出了所有方法的签名和实际调用的方法。这样在调用方法的时候，只需要查找这个表即可。

- 实例

```java
// 定义类
    class Human { 
        public void sayHello(){ 
            System.out.println("Human say hello"); 
        } 
    } 
 
// 继承自 抽象类Human 并 重写sayHello()
    class Man extends Human { 
        @Override 
        protected void sayHello() { 
            System.out.println("man say hello"); 
        } 
    } 
 
    class Woman extends Human { 
        @Override 
        protected void sayHello() { 
            System.out.println("woman say hello"); 
        } 
    } 

// 测试代码
    public static void main(String[] args) { 

        // 情况1
        Human man = new man(); 
        man.sayHello(); 

        // 情况2
        man = new Woman(); 
        man.sayHello(); 
    } 
}

// 运行结果
man say hello
woman say hello

// 原因解析
// 1. 方法重写（Override） = 动态分派 = 根据 变量的动态类型 确定执行（重写）哪个方法
// 2. 对于情况1：根据变量（Man）的动态类型（man）确定调用man中的重写方法sayHello()
// 3. 对于情况2：根据变量（Man）的动态类型（woman）确定调用woman中的重写方法sayHello()
```

对于代码中：

```java
Human man = new Man(); 
man = new Woman(); 
man.sayHello(); 

// man称为执行sayHello()方法的所有者，即接受者。
```

- `invokevirtual`指令执行的第一步 = 确定接受者的实际类型
- `invokevirtual`指令执行的第二步 = **将 常量池中 类方法符号引用 解析到不同的直接引用上**

> 第二步即方法重写（`Override`）的本质

**总结:动态绑定（多态）：动态绑定是指在“执行期间”（而非编译期间）判断所引用的实际对象类型，根据其实际的类型调用其相应的方法。**所以实际当中找要调用的方法时是动态的去找的，new的是谁就找谁的方法，这就叫动态绑定。动态绑定帮助我们的程序的可扩展性达到了极致。

**多态的存在有三个必要的条件：**

1. **要有继承（两个类之间存在继承关系，子类继承父类）**
2. **要有重写（在子类里面重写从父类继承下来的方法）**
3. **父类引用指向子类对象**

　　这三个条件一旦满足，当你调用父类里面被重写的方法的时候，实际当中new的是哪个子类对象，就调用子类对象的方法（这个方法是从父类继承下来后重写后的方法）。

**动态绑定（多态）这种机制能帮助我们做到这一点——让程序的可扩展性达到极致。因此动态绑定是面向对象的核心，如果没有动态绑定，那么面向对象绝对不可能发展得像现在这么流行，所以动态绑定是面向对象核心中的核心。**



---

[静态多态与动态多态](https://www.cnblogs.com/blairwaldorf/p/9194588.html)

 面向对象编程的多态从绑定时间来看,可以分成静态多态和动态多态,也称为编译期多态和运行期多态。

java中overload是静态多态，即根据参数列表进行最佳匹配，在编译阶段决定要具体执行哪个方法。而与之相反，overriden methods则是在run-time进行动态检查。

举例说明：

```
public class UseAnimals {
    public void doStuff(Animal a) {
        System.out.println("Animal");
    }
    public void doStuff(Horse a) {
        System.out.println("Horse");
    }
     
 
}
class Animal{
    public void eat()
    {
    }
}
class Horse extends Animal{
    public void eat(String food) {}
}
public class TestUseAnimals {
    public static void main(String[] args) {
        UseAnimals ua = new UseAnimals();
         
        Animal animalobj = new Animal();
        Horse horseobj = new Horse();
        Animal animalRefToHorse = new Horse();
         
        ua.doStuff(animalobj);//Animal
        ua.doStuff(horseobj);//Horse
        ua.doStuff(animalRefToHorse);//Animal
         
    }
 
}
```

　　可以看见UseAnimals里面有两个 doStuff方法，但参数列表不同，属于overload,在编译阶段决定执行哪个方法，毕竟参数是Animal类型还是Horse类型，这些是编译阶段就可以判断的。但引用类型具体执行哪个方法是根据静态编译阶段来决定的。比如ua.doStuff(animalRefToHorse) ，animalRefToHorse在编译阶段是判断Animal类型，所以执行doStuff(Animal a）的方法。

```
public class UseAnimals {
    public static void main(String[] args) {
        Animal a =new Animal();
        a.eat();
         
        Horse h = new Horse();
        h.eat();
         
        Horse ah = new Horse();
        ah.eat();
         
        Horse he =new Horse();
        he.eat("Apples");
         
    //  Animal ah2=new Animal();
    //  ah2.eat("Carrots");  编译不能通过
         
    //  Animal ah2=new Horse();
    //  ah2.eat("Carrots");  编译不能通过
         
         
    }
     
 
}
class Animal{
    public void eat()
    {
        System.out.println("eat");
    }
}
class Horse extends Animal{
    public void eat(String food)
    {
        System.out.println("eat"+food);
    }
}
```

　　而override则是动态多态，执行方法是在run-time决定

```
public class UseAnimals {
    public static void main(String[] args) {
    //  Animal a=new Animal();编译不能通过
        Animal a=new Dog();
        Animal b =new Cow();
        a.vocalize();//Woof！
        b.vocalize();//Moo!
         
    }
     
 
}
interface Animal{
    void vocalize();
}
class Dog implements Animal{
    @Override
    public void vocalize() {
        System.out.println("Woof!");
    }
}
class Cow implements Animal{
    @Override
    public void vocalize() {
        System.out.println("Moo!");
    }
}
```

　　