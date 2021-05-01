https://blog.csdn.net/yibin18566767255/article/details/79763791

Static内存图解:

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glguu1fllmj30mh0aewhz.jpg" alt="img" style="zoom:150%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glguu6tpclj30mh0aewhz.jpg" alt="img" style="zoom:150%;" />

解析:

如代码:

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguv9mqp5j30dq0dsq49.jpg)



①:当程序运行,class进行加载这时在方法区则会有class文件区

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguv89ui6j305p049dfs.jpg)

而class文件区内又有两个区(代码中有两个类一个person一个personDemo)这时class文件区的第一个区产生:![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguv7r5fbj305u0203yj.jpg)

然后产生第二个区

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguv5vrx2j305r026t8n.jpg)

class区域内有成员属性与成员方法.而为什么没有静态的属性与方法呢,是因为当程序加载该class文件时,系统会自动的把他所有静态的属性与方法分配到静态区 (方法区内有一个静态区);静态区内也有两块区域一个是personDemo类标记的main(String[]args)方法,一个是person标记的变量country=null(String类型初始默认值为null)

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguv4h11pj306g086t96.jpg)

#### 截止到现在他的初始化才完毕,则得出结论:静态的内容随着class的文件加载而加载



②:接着走的main方法进栈,main是静态修饰.静态可以通过类名调用,所以 根本不需要创建对象main方法就进栈内存,那么main方法一进栈系统则为它开辟空间;

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguv2ne61j30k207vaar.jpg)




开辟空间之后接着它创建了第一个对象Person p1 = new Person("邓丽君",16,"中国");

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguv17po6j305b07hwef.jpg)

然后new Person在堆内存进堆之后系统会分配一个空间与内存地址假如是:0X0001,其实他也代表this,

同时这块内存区域内有name=null;age=0;与静态标记和方法标记:

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguuzua9ij306305tdfv.jpg);

静态标记是:静态区域有一个变量country他也有一个内存地址假如是0X01;这时他其实 是跟静态标记是有关联的,这个时候静态标记是指向静态区的country的

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguuye3rjj30i6076js9.jpg)




方法标记:系统在创建对象的时候,会自动生成一个方法区,这个方法区是属于当前对象的则Person,而这个方法区也有一个内存地址假如是:0X02;构造方法与show方法等带参不带参的方法都存储在方法区内它是由Person标记的内存地址是0X02;这是堆内存中的方法标记则与方法区产生关联,直到这时它所有的初始化才完成

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguuwmc6jj30cc0b5dgu.jpg)




③:当进行完以上步骤它的值又被改变了:Person p1 = new Person("邓丽君",16,"中国");这是给name; age;country进行赋第一个值:"邓丽君"通过走构造方法进内存,将"邓丽君给了"构造方法public Person(String name,int age,String country);中的neme;16给了age

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguuv8lq2j306a04r74f.jpg)

这时找静态变量country"中国",但此时在堆内存中并没有这个静态变量.则找静态标记,0X01;通过内存地址0X01找到静态区 0X01标记的country,这时null则被"中国替代"

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguutqu0xj30ct07jjs9.jpg)




截止此时Person p1 = new Person("邓丽君",16,"中国")的值才会赋完.

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguusc21nj30if0ajjsp.jpg)






④:运行到P1.show()方法,此时p1.show()方法调方法



public void show() {

 System.out.println("姓名："+name+",年龄："+age+",国籍："+country);


 };

在方法区找有没有show方法.有show方法则show方法进栈为show方法开辟一个空间;那么show()方法在里面输出的时候首先找name,因为是p1调用,所以通过Person p1找name值在找age值在通过静态标记进入静态区找country值,所以第一次输出结果是:

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguuqw919j306b03ogli.jpg)

当这些值在控制台显示之后.然后这个方法调用完毕就从内存中消失

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguup0wpmj30500att8s.jpg)

接着运行:Person p2 = new Person("杨幂",22);步骤:Person p2则在栈内存开辟第二个空并给出内存地址:0X0002;

然后new Person()在堆内存开辟空间.然后:Person p2 = new Person("杨幂",22)将"杨幂",22赋值给name,age;此时并没有给country值,但是它 是有值的,因为他是静态值.它的静态标记0x01指向静态区域0x01;它的值还是以前的,但是由它们两个共享一个静态区域所以不需要再次进行赋值.然后找到p2.show()方法.使show()进栈,然后show()方法通过 Person p2-0x0002进堆内存通过name age找值,在通过静态标记找country的值.然后进行输出销毁内存,P3同理:

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguuna7ggj30iq0ez0va.jpg)



在当运行到p3.country = "美国";时,"美国"通过Person p3进堆,通过静态标记对country的值进行更改.因为所以方法共享一个静态区所以p3.show();
        p1.show();
 p2.show();

​        输出的country值都是美国

  ![img](https://tva1.sinaimg.cn/large/0081Kckwly1glguulxxemj30vm0ejdk0.jpg)





static关键字(理解)

  (1)静态的意思:可以修饰成员变量和成员方法。
  (2)静态的特点：
 A:随着类的加载而加载
 B:优先与对象存在
 C:被类的所有对象共享
 这其实也是我们判断该不该使用静态的依据。
 举例：饮水机和水杯的问题思考
 D:可以通过类名调用
 既可以通过对象名调用，也可以通过类名调用，建议通过类名调用。
  (3)静态的内存图
 静态的内容在方法区的静态区
static关键字注意事项
 A:在静态方法中是没有this关键字的
 如何理解呢?
 静态是随着类的加载而加载，this是随着对象的创建而存在。
 静态比对象先存在。
 B:静态方法只能访问静态的成员变量和静态的成员方法
 静态方法：
 成员变量：只能访问静态变量
 成员方法：只能访问静态成员方法
 非静态方法：
 成员变量：可以是静态的，也可以是非静态的
 成员方法：可是是静态的成员方法，也可以是非静态的成员方法。
 简单记：
 静态只能访问静态。
  (5)静态变量和成员变量的区别
 A:所属不同
 静态变量：属于类，类变量
 成员变量：属于对象，对象变量，实例变量
 B:内存位置不同
 静态变量：方法区的静态区
 成员变量：堆内存
 C:生命周期不同
 静态变量：静态变量是随着类的加载而加载，随着类的消失而消失
 成员变量：成员变量是随着对象的创建而存在，随着对象的消失而消失
 D:调用不同
 静态变量：可以通过对象名调用，也可以通过类名调用
 成员变量：只能通过对象名调用



----

# [java中静态变量在内存中的位置](https://www.cnblogs.com/chen-jack/p/7895287.html)

java程序在内存中的存储分配情况：

**一、堆区:** 
1.存储的全部是对象，每个对象都包含一个与之对应的class的信息。(class的目的是得到操作指令) 
2.jvm只有一个堆区(heap)被所有线程共享，堆中不存放基本类型和对象引用，只存放对象本身 
栈区: 
1.每个线程包含一个栈区，栈中只保存基础数据类型的对象和自定义对象的引用(不是对象)，对象都存放在堆区中 
2.每个栈中的数据(原始类型和对象引用)都是私有的，其他栈不能访问。 
3.栈分为3个部分：基本类型变量区、执行环境上下文、操作指令区(存放操作指令)。 
方法区: 
1.又叫静态区，跟堆一样，被所有的线程共享。方法区包含所有的class和static变量。 
2.方法区中包含的都是在整个程序中永远唯一的元素，如class，static变量。

 

**JVM内存总体一共分为了 4个部分：二、内存分区**
而内存分为四个区：stack segment，heap segment，data segment，code segment；
stack 区存放函数参数和局部变量；

heap 区存放对象；

data 区存放static 的变量或者字符串常量； 

code 区存放类中的方法；因此，静态变量是存放在data区的 ！



程序运行时，我们最好对数据保存到什么地方做到心中有数。特别要注意的是内在的分配，有六个地方都可以保存数据：
1、 寄存器。这是最快的保存区域，因为它位于和其他所有保存方式不同的地方：处理器内部。然而，寄存器的数量十分有限，所以寄存器是根据需要由编译器分配。我们对此没有直接的控制权，也不可能在自己的程序里找到寄存器存在的任何踪迹。
2、 堆栈。驻留于常规RAM（随机访问存储器）区域。但可通过它的“堆栈指针”获得处理的直接支持。堆栈指针若向下移，会创建新的内存；若向上移，则会释放那些内存。这是一种特别快、特别有效的数据保存方式，仅次于寄存器。创建程序时，java编译器必须准确地知道堆栈内保存的所有数据的“长度”以及“存在时间”。这是由于它必须生成相应的代码，以便向上和向下移动指针。这一限制无疑影响了程序的灵活性，所以尽管有些java数据要保存在堆栈里——特别是对象句柄，但java对象并不放到其中。
3、 堆。一种常规用途的内存池（也在RAM区域），其中保存了java对象。和堆栈不同：“内存堆”或“堆”最吸引人的地方在于编译器不必知道要从堆里分配多少存储空间，也不必知道存储的数据要在堆里停留多长的时间。因此，用堆保存数据时会得到更大的灵活性。要求创建一个对象时，只需用new命令编制相碰的代码即可。执行这些代码时，会在堆里自动进行数据的保存。当然，为达到这种灵活性，必然会付出一定的代价：在堆里分配存储空间时会花掉更长的时间
4、 静态存储。这儿的“静态”是指“位于固定位置”。程序运行期间，静态存储的数据将随时等候调用。可用static关键字指出一个对象的特定元素是静态的。但java对象本身永远都不会置入静态存储空间。
5、 常数存储。常数值通常直接置于程序代码内部。这样做是安全的。因为它们永远都不会改变，有的常数需要严格地保护，所以可考虑将它们置入只读存储器（ROM）。
6、 非RAM存储。若数据完全独立于一个程序之外，则程序不运行时仍可存在，并在程序的控制范围之外。其中两个最主要的例子便是“流式对象”和“固定对象”。对于流式对象，对象会变成字节流，通常会发给另一台机器，而对于固定对象，对象保存在磁盘中。即使程序中止运行，它们仍可保持自己的状态不变。对于这些类型的数据存储，一个特别有用的技艺就是它们能存在于其他媒体中，一旦需要，甚至能将它们恢复成普通的、基于RAM的对象。



首先，java里面是没有静态变量这个概念的,不信你自己在方法里面定义一个static int i =0；java里只有静态成员变量。它属于类的属性。至于他放在那里？楼上说的是静态区。我不知道到底有没有这个翻译。但是 深入jvm里是是翻译为方法区的。虚拟机的体系结构：堆,方法区，本地方法栈，pc寄存器。而方法区保存的就是一个类的模板，堆是放类的实例的。栈是一般来用来函数计算的。随便找本计算机底层的书都知道了。栈里的数据，函数执行完就不会存储了。这就是为什么局部变量每一次都是一样的。就算给他加一后，下次执行函数的时候还是原来的样子。



---



https://blog.csdn.net/expect521/article/details/77505541

### JVM内存总体一共分为了 4个部分：

stack segment、
heap segment、
code segment、
data segment)

### stack segment(栈)：

局部变量：如main函数中声明的str变量。如图中，str，t存在于stack区：

![这里写图片描述](https://tva1.sinaimg.cn/large/0081Kckwly1glgv0u7l0sj308v02gmx4.jpg)

![这里写图片描述](https://tva1.sinaimg.cn/large/0081Kckwly1glgv0sa92qj3088023a9z.jpg)

栈中保存**基本数据类型的变量**和**自定义的对象的引用(不是对象)**，**对象本身**都存放在堆区中，**被执行的方法**的也是pull到栈中，当方法执行完后再push出栈。

### heap segment（堆）

当new 一个对象的时候，此对象放在了heap segment(堆)当中。t存放在stack中，而new Test()这个实实在在的对象是存在了heap中
如。代码：

![这里写图片描述](https://tva1.sinaimg.cn/large/0081Kckwly1glgv0rc326j3088023a9z.jpg)

heap中存储的全部是对象，每个对象都包含一个与之对应的class的信息。(class的目的是得到操作指令)

### code segment(代码区)

类中方法的话，是存在在 code segment(代码区)中了

### data segment(数据区)

static 的变量或者字符串常量存在数据区

### static变量与非static变量

static的变量与非static变量存放位置不一样，并且变量的访问权限也不一样。

static变量是全局的，是类的所有对象都能访问的，是所有方法都可以访问的，无论是static修饰的方法还是非static方法都可以访问，没有限制。

而非static变量是私有的，是有访问限制的，就是说是每个对象独有的特有的，并且只有非static方法才可以访问。

如图：
![这里写图片描述](https://tva1.sinaimg.cn/large/0081Kckwly1glgv0pi9ibj30cx085jrv.jpg)
**static方法只能访问static的变量，没有权限访问非static变量。**

![这里写图片描述](https://tva1.sinaimg.cn/large/0081Kckwly1glgv0oj0o2j30bz08cjrr.jpg)
**static方法中声明的变量可以与非static变量并且是类的属性重名**

![这里写图片描述](https://tva1.sinaimg.cn/large/0081Kckwly1glgv0msif8j308n09odg7.jpg)
**方法中声明的变量可以与非static变量并且是类的属性重名**

> 这是由于类的属性是存在与对象中的，是在heap中，而方法中的变量是存在与code 区中的，在不同的区中可以重名。

![这里写图片描述](https://tva1.sinaimg.cn/large/0081Kckwly1glgv0l6iegj308a06hmxf.jpg)
**非static方法中可以方访问static变量。**

> 这时因为static变量是共享的，任何方法，任何对象都可以访问

![这里写图片描述](https://tva1.sinaimg.cn/large/0081Kckwly1glgv0j9uikj309908xt92.jpg)
**static方法可以访问static属性**

![这里写图片描述](https://tva1.sinaimg.cn/large/0081Kckwly1glgv0ih0qwj308v08baae.jpg)

static方法中声明的变量name存在code区，而类的属性中的name存在对象中，而对象存在于heap区。所以不会报错

---

