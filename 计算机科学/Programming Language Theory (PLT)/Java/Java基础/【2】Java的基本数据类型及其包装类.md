[TOC]

## 一、基本类型 VS 对象类型

Java 中预定义了八种基本数据类型，包括：byte，int，long，double，float，boolean，char，short。基本类型与对象类型最大的不同点在于，**基本类型基于数值，对象类型基于引用**。

| 基本类型        | 包装类型  |
| --------------- | --------- |
| int（4字节）    | Integer   |
| byte（1字节）   | Byte      |
| short（2字节）  | Short     |
| long（8字节）   | Long      |
| float（4字节）  | Float     |
| double（8字节） | Double    |
| char（2字节）   | Character |
| boolean（未定） | Boolean   |

### 1. 整型：

#### byte：

byte数据类型是8位、有符号的，以二进制补码表示的整数；
最小值是-128（-2^7）；
最大值是127（2^7-1）；
默认值是0；
byte类型用在大型数组中节约空间，主要代替整数，因为byte变量占用的空间只有int类型的四分之一；
例子：byte a = 100，byte b = -50。

#### short：

short数据类型是16位、有符号的以二进制补码表示的整数
最小值是-32768（-2^15）；
最大值是32767（2^15 - 1）；
Short数据类型也可以像byte那样节省空间。一个short变量是int型变量所占空间的二分之一；
默认值是0；
例子：short s = 1000，short r = -20000。

#### int：

int数据类型是32位、有符号的以二进制补码表示的整数；
最小值是-2,147,483,648（-2^31）；
最大值是2,147,483,647（2^31 - 1）；
一般地整型变量默认为int类型；
默认值是0；
例子：int a = 100000, int b = -200000。

#### long：

long数据类型是64位、有符号的以二进制补码表示的整数；
最小值是-9,223,372,036,854,775,808（-2^63）；
最大值是9,223,372,036,854,775,807（2^63 -1）；
这种类型主要使用在需要比较大整数的系统上；
默认值是0L；
后面加L或者l,就表示是long长整型；
例子： long a = 100000L，Long b = -200000L。



### 2. 浮点型：

#### float：

float数据类型是单精度、32位、符合IEEE 754标准的浮点数；
float在储存大型浮点数组的时候可节省内存空间；
默认值是0.0f；
后面加F或者f,表示float类型；
浮点数不能用来表示精确的值，如货币；
例子：float f1 = 234.5f。

#### double：

double数据类型是双精度、64位、符合IEEE 754标准的浮点数；
浮点数的默认类型为double类型；
double类型同样不能表示精确的值，如货币；
默认值是0.0d；
后面加D或者d,表示double类型；
例子：double d1 = 123.4。
布尔型：

#### boolean：

boolean数据类型表示一位的信息；
只有两个取值：true和false；
这种类型只作为一种标志来记录true/false情况；
默认值是false；
例子：boolean one = true。



### 3. 字符型：

#### char：

char类型是一个单一的16位Unicode字符；
最小值是’\u0000’（即为0）；
最大值是’\uffff’（即为65,535）；
char数据类型可以储存任何字符；

例子：char letter = ‘A’。

**注意：String不是基本类型。**



## 二、自动类型转换

**整型、实型（常量）、字符型数据可以混合运算。运算中，不同类型的数据先转化为同一类型，然后进行运算。**

转换从低级到高级。

```java
低  ------------------------------------>  高

byte,short,char—> int —> long—> float —> double 
```

数据类型转换必须满足如下规则：

- 不能对boolean类型进行类型转换。

- 不能把对象类型转换成不相关类的对象。

- 在把容量大的类型转换为容量小的类型时必须使用强制类型转换。

- 转换过程中可能导致溢出或损失精度，例如：

  ```java
  int i =128;   
  byte b = (byte)i;
  ```

  因为 byte 类型是 8 位，最大值为127，所以当 int 强制转换为 byte 类型时，值 128 时候就会导致溢出。

- 浮点数到整数的转换是通过舍弃小数得到，而不是四舍五入，例如：

  ```java
  (int)23.7 == 23;        
  (int)-45.89f == -45
  ```



## 三、装箱和拆箱

### 1. 什么是装箱？什么是拆箱？

Java为每种基本数据类型都提供了对应的包装器类型，至于为什么会为每种基本数据类型提供包装器类型在此不进行阐述，有兴趣的朋友可以查阅相关资料。在Java SE5之前，如果要生成一个数值为10的Integer对象，必须这样进行：

```java
Integer i = new Integer(10);
```

而在从Java SE5开始就提供了自动装箱的特性，如果要生成一个数值为10的Integer对象，只需要这样就可以了：

```java
Integer i = 10;
```

这个过程中会自动根据数值创建对应的 Integer对象，这就是装箱。

```java
Integer i = 10;  //装箱
int n = i;   //拆箱
```

那什么是拆箱呢？顾名思义，跟装箱对应，就是自动将包装器类型转换为基本数据类型：

简单一点说，装箱就是  自动将基本数据类型转换为包装器类型；拆箱就是  自动将包装器类型转换为基本数据类型。



### 2.例子

**2.1Integer**

```java
public class Main {
    public static void main(String[] args) {
         
        Integer i1 = 100;
        Integer i2 = 100;
        Integer i3 = 200;
        Integer i4 = 200;
         
        System.out.println(i1==i2);
        System.out.println(i3==i4);
    }
}

true
false
```

**2.2Double**

```java
public class Main {
    public static void main(String[] args) {
         
        Double i1 = 100.0;
        Double i2 = 100.0;
        Double i3 = 200.0;
        Double i4 = 200.0;
         
        System.out.println(i1==i2);
        System.out.println(i3==i4);
    }
}

false
false
```

**2.3Boolean**

```java
public class Main {
    public static void main(String[] args) {
         
        Boolean i1 = false;
        Boolean i2 = false;
        Boolean i3 = true;
        Boolean i4 = true;
         
        System.out.println(i1==i2);
        System.out.println(i3==i4);
    }
}

true
true
```



在这里只解释一下为什么Double类的valueOf方法会采用与Integer类的valueOf方法不同的实现。很简单：在某个范围内的整型数值的个数是有限的，而浮点数却不是。

**注意，Integer、Short、Byte、Character、Long这几个类的valueOf方法的实现是类似的。**

**Double、Float的valueOf方法的实现是类似的。**

**2.4深入理解JVM里的一个例子**

```java
public class Main {
    public static void main(String[] args) {
         
        Integer a = 1;
        Integer b = 2;
        Integer c = 3;
        Integer d = 3;
        Integer e = 321;
        Integer f = 321;
        Long g = 3L;
        Long h = 2L;
         
        System.out.println(c==d); 
        System.out.println(e==f);  //false
        System.out.println(c==(a+b));
        System.out.println(c.equals(a+b));
        System.out.println(g==(a+b));
        System.out.println(g.equals(a+b));  //false
        System.out.println(g.equals(a+h));
    }
}
```

这里面需要注意的是：**当 "=="运算符的两个操作数都是 包装器类型的引用，则是比较指向的是否是同一个对象，而如果其中有一个操作数是表达式（即包含算术运算）则比较的是数值（即会触发自动拆箱的过程）**。另外，对于包装器类型，equals方法并不会进行类型转换。明白了这2点之后，上面的输出结果便一目了然： 

第一个和第二个输出结果没有什么疑问。第三句由于  a+b包含了算术运算，因此会触发自动拆箱过程（会调用intValue方法），因此它们比较的是数值是否相等。而对于c.equals(a+b)会先触发自动拆箱过程，再触发自动装箱过程，也就是说a+b，会先各自调用intValue方法，得到了加法运算后的数值之后，便调用Integer.valueOf方法，再进行equals比较。同理对于后面的也是这样，不过要注意倒数第二个和最后一个输出的结果（如果数值是int类型的，装箱过程调用的是Integer.valueOf；如果是long类型的，装箱调用的Long.valueOf方法） 

### 3."=="和.euqals()

- 基本数据类型，用双等号“\==”比较，比较的是他们的**值**，值类型是存储在内存中的堆栈（简称栈）也就是说“\==”比较的是两个对象的指针，也就是实际对象的地址。
- 复合数据类型中， 当他们用“==”进行比较的时候，比较的是他们在内存（堆）中的存放**地址**，(**除非该类对equals方法进行了重写**)其变量在栈中仅仅是存储引用类型变量的地址，而其本身则存储在堆中。所以，除非是同一个new出来的对象，他们的比较后的结果为true，否则比较后结果为false。
- **如果没有对equals方法进行重写，则比较的是引用类型的变量所指向的对象的地址；**
- equals操作表示的两个变量是否是对同一个对象的引用，即比较两个对象存储的堆中的内容是否相同，而“\==”比较的是2个对象的地址，所以当equals为true时，“\==”不一定为true。
- 诸如Double，Date，Integer，String等包装类，对equals方法进行了重写的话，比较的是所指向的**对象的内容**。 

例如：

对于字符串变量来说，使用“\==”和“equals()”方法比较字符串时，其比较方法不同。“\==”比较两个变量本身的值，即两个对象在内存中的首地址。

“equals()”比较字符串中所包含的内容是否相同。



- http://www.cnblogs.com/dolphin0520/p/3780005.html