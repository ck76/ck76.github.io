[TOC]

### 1、简介

- C++ 是一种静态类型的、编译式的、通用的、大小写敏感的、不规则的编程语言，支持过程化编程、面向对象编程和泛型编程。

- C++ 被认为是一种**中级**语言，它综合了高级语言和低级语言的特点。

- **标准库：**

  标准的 C++ 由三个重要部分组成：

  - 核心语言，提供了所有构件块，包括变量、数据类型和常量，等等。
  - C++ 标准库，提供了大量的函数，用于操作文件、字符串等。
  - 标准模板库（STL），提供了大量的方法，用于操作数据结构等。

- **学习** C++

  学习 C++，关键是要理解概念，而不应过于深究语言的技术细节。

  学习程序设计语言的目的是为了成为一个更好的程序员，也就是说，是为了能更有效率地设计和实现新系统，以及维护旧系统。



### 2、数据类型

**注意：**不同系统会有所差异。

| 类型               | 位            | 范围                                                    |
| ------------------ | ------------- | ------------------------------------------------------- |
| **char**           | 1 个字节      | -128 到 127 或者 0 到 255                               |
| unsigned char      | 1 个字节      | 0 到 255                                                |
| signed char        | 1 个字节      | -128 到 127                                             |
| **int**            | 4 个字节      | -2147483648 到 2147483647                               |
| unsigned int       | 4 个字节      | 0 到 4294967295                                         |
| signed int         | 4 个字节      | -2147483648 到 2147483647                               |
| short int          | 2 个字节      | -32768 到 32767                                         |
| unsigned short int | 2 个字节      | 0 到 65,535                                             |
| signed short int   | 2 个字节      | -32768 到 32767                                         |
| long int           | 8 个字节      | -9,223,372,036,854,775,808 到 9,223,372,036,854,775,807 |
| signed long int    | 8 个字节      | -9,223,372,036,854,775,808 到 9,223,372,036,854,775,807 |
| unsigned long int  | 8 个字节      | 0 to 18,446,744,073,709,551,615                         |
| **float**          | 4 个字节      | +/- 3.4e +/- 38 (~7 个数字)                             |
| **double**         | 8 个字节      | +/- 1.7e +/- 308 (~15 个数字)                           |
| **long double**    | 16 个字节     | +/- 1.7e +/- 308 (~15 个数字)                           |
| wchar_t            | 2 或 4 个字节 | 1 个宽字符                                              |

从上表可得知，变量的大小会根据编译器和所使用的电脑而有所不同。

下面实例会输出您电脑上各种数据类型的大小。

```c++
#include<iostream>  
#include<string>  
#include <limits>  
using namespace std;  
  
int main()  
{  
    cout << "type: \t\t" << "************size**************"<< endl;  
    cout << "bool: \t\t" << "所占字节数：" << sizeof(bool);  
    cout << "\t最大值：" << (numeric_limits<bool>::max)();  
    cout << "\t\t最小值：" << (numeric_limits<bool>::min)() << endl;  
    cout << "char: \t\t" << "所占字节数：" << sizeof(char);  
    cout << "\t最大值：" << (numeric_limits<char>::max)();  
    cout << "\t\t最小值：" << (numeric_limits<char>::min)() << endl;  
    cout << "signed char: \t" << "所占字节数：" << sizeof(signed char);  
    cout << "\t最大值：" << (numeric_limits<signed char>::max)();  
    cout << "\t\t最小值：" << (numeric_limits<signed char>::min)() << endl;  
    cout << "unsigned char: \t" << "所占字节数：" << sizeof(unsigned char);  
    cout << "\t最大值：" << (numeric_limits<unsigned char>::max)();  
    cout << "\t\t最小值：" << (numeric_limits<unsigned char>::min)() << endl;  
    cout << "wchar_t: \t" << "所占字节数：" << sizeof(wchar_t);  
    cout << "\t最大值：" << (numeric_limits<wchar_t>::max)();  
    cout << "\t\t最小值：" << (numeric_limits<wchar_t>::min)() << endl;  
    cout << "short: \t\t" << "所占字节数：" << sizeof(short);  
    cout << "\t最大值：" << (numeric_limits<short>::max)();  
    cout << "\t\t最小值：" << (numeric_limits<short>::min)() << endl;  
    cout << "int: \t\t" << "所占字节数：" << sizeof(int);  
    cout << "\t最大值：" << (numeric_limits<int>::max)();  
    cout << "\t最小值：" << (numeric_limits<int>::min)() << endl;  
    cout << "unsigned: \t" << "所占字节数：" << sizeof(unsigned);  
    cout << "\t最大值：" << (numeric_limits<unsigned>::max)();  
    cout << "\t最小值：" << (numeric_limits<unsigned>::min)() << endl;  
    cout << "long: \t\t" << "所占字节数：" << sizeof(long);  
    cout << "\t最大值：" << (numeric_limits<long>::max)();  
    cout << "\t最小值：" << (numeric_limits<long>::min)() << endl;  
    cout << "unsigned long: \t" << "所占字节数：" << sizeof(unsigned long);  
    cout << "\t最大值：" << (numeric_limits<unsigned long>::max)();  
    cout << "\t最小值：" << (numeric_limits<unsigned long>::min)() << endl;  
    cout << "double: \t" << "所占字节数：" << sizeof(double);  
    cout << "\t最大值：" << (numeric_limits<double>::max)();  
    cout << "\t最小值：" << (numeric_limits<double>::min)() << endl;  
    cout << "long double: \t" << "所占字节数：" << sizeof(long double);  
    cout << "\t最大值：" << (numeric_limits<long double>::max)();  
    cout << "\t最小值：" << (numeric_limits<long double>::min)() << endl;  
    cout << "float: \t\t" << "所占字节数：" << sizeof(float);  
    cout << "\t最大值：" << (numeric_limits<float>::max)();  
    cout << "\t最小值：" << (numeric_limits<float>::min)() << endl;  
    cout << "size_t: \t" << "所占字节数：" << sizeof(size_t);  
    cout << "\t最大值：" << (numeric_limits<size_t>::max)();  
    cout << "\t最小值：" << (numeric_limits<size_t>::min)() << endl;  
    cout << "string: \t" << "所占字节数：" << sizeof(string) << endl;  
    // << "\t最大值：" << (numeric_limits<string>::max)() << "\t最小值：" << (numeric_limits<string>::min)() << endl;  
    cout << "type: \t\t" << "************size**************"<< endl;  
    return 0;  
}
// mac os
type: 		************size**************
bool: 		所占字节数：1	最大值：1		最小值：0
char: 		所占字节数：1	最大值：		最小值：�
signed char: 	所占字节数：1	最大值：		最小值：�
unsigned char: 	所占字节数：1	最大值：�		最小值：
wchar_t: 	所占字节数：4	最大值：2147483647		最小值：-2147483648
short: 		所占字节数：2	最大值：32767		最小值：-32768
int: 		所占字节数：4	最大值：2147483647	最小值：-2147483648
unsigned: 	所占字节数：4	最大值：4294967295	最小值：0
long: 		所占字节数：8	最大值：9223372036854775807	最小值：-9223372036854775808
unsigned long: 	所占字节数：8	最大值：18446744073709551615	最小值：0
double: 	所占字节数：8	最大值：1.79769e+308	最小值：2.22507e-308
long double: 	所占字节数：16	最大值：1.18973e+4932	最小值：3.3621e-4932
float: 		所占字节数：4	最大值：3.40282e+38	最小值：1.17549e-38
size_t: 	所占字节数：8	最大值：18446744073709551615	最小值：0
string: 	所占字节数：24
type: 		************size**************
```

#### 可以使用 **typedef** 为一个已有的类型取一个新的名字

```c++
typedef type newname; 
//例如，下面的语句会告诉编译器，feet 是 int 的另一个名称：
typedef int feet;
//下面的声明是完全合法的，它创建了一个整型变量 distance：
feet distance;
```
- typedef 可以声明各种类型名，但**不能用来定义变量**。用 typedef 可以声明数组类型、字符串类型，使用比较方便。
- 用typedef只是对已经存在的类型增加一个类型名，而没有创造新的类型。
  当在不同源文件中用到同一类型数据（尤其是像数组、指针、结构体、共用体等类型数据）时，常用 typedef 声明一些数据类型，把它们单独放在一个头文件中，然后在需要用到它们的文件中用 ＃include 命令把它们包含进来，以提高编程效率。
- 使用 typedef 有利于程序的通用与移植。有时程序会依赖于硬件特性，用 typedef 便于移植。

#### **typedef 与 #define 的区别**

**1、执行时间不同**

```c++
关键字 typedef 在编译阶段有效，由于是在编译阶段，因此 typedef 有类型检查的功能。
#define 则是宏定义，发生在预处理阶段，也就是编译之前，它只进行简单而机械的字符串替换，而不进行任何检查。
```

**2、功能有差异**

```c++
typedef 用来定义类型的别名，定义与平台无关的数据类型，与 struct 的结合使用等。
#define 不只是可以为类型取别名，还可以定义常量、变量、编译开关等。
```

**3、作用域不同**

```c++
#define 没有作用域的限制，只要是之前预定义过的宏，在以后的程序中都可以使用。
而 typedef 有自己的作用域。
```

**4、对指针的操作**

```c++
//二者修饰指针类型时，作用不同。
typedef int * pint;
#define PINT int *
 
int i1 = 1, i2 = 2;
 
const pint p1 = &i1;    //p不可更改，p指向的内容可以更改，相当于 int * const p;
const PINT p2 = &i2;    //p可以更改，p指向的内容不能更改，相当于 const int *p；或 int const *p；
 
pint s1, s2;    //s1和s2都是int型指针
PINT s3, s4;    //相当于int * s3，s4；只有一个是指针。
 
void TestPointer()
{
    cout << "p1:" << p1 << "  *p1:" << *p1 << endl;
    //p1 = &i2; //error C3892: 'p1' : you cannot assign to a variable that is const
    *p1 = 5;
    cout << "p1:" << p1 << "  *p1:" << *p1 << endl;
 
    cout << "p2:" << p2 << "  *p2:" << *p2 << endl;
    //*p2 = 10; //error C3892: 'p2' : you cannot assign to a variable that is const
    p2 = &i1;
    cout << "p2:" << p2 << "  *p2:" << *p2 << endl;
}

//结果：
p1:00EFD094  *p1:1
p1:00EFD094  *p1:5
p2:00EFD098  *p2:2
p2:00EFD094  *p2:5
```

#### **还可以用 typedef 来定义与平台无关的类型**

```c++
//比如定义一个叫 FALSE 的浮点类型，在目标平台一上，让它表示最高精度的类型为：
typedef long double FALSE; 
//在不支持 long double 的平台二上，改为：
typedef double FALSE; 
//在连 double 都不支持的平台三上，改为：
typedef float FALSE; 
```

也就是说，当跨平台时，只要改下 typedef 本身就行，不用对其他源码做任何修改。

标准库就广泛使用了这个技巧，比如 **size_t**。

另外，因为 typedef **是定义了一种类型的新别名，不是简单的字符串替换，所以它比宏来得稳健**（虽然用宏有时也可以完成以上的用途）。

#### **枚举类型**

```c++
enum 枚举名{ 
     标识符[=整型常数], 
     标识符[=整型常数], 
... 
    标识符[=整型常数]
} 枚举变量;
//如果枚举没有初始化, 即省掉"=整型常数"时, 则从第一个标识符开始。
//例如，下面的代码定义了一个颜色枚举，变量 c 的类型为 color。最后，c 被赋值为 "blue"。
enum color { red, green, blue } c;
c = blue;
//默认情况下，第一个名称的值为 0，第二个名称的值为 1，第三个名称的值为 2，以此类推。但是，您也可以给名称赋予一个特殊的值，只需要添加一个初始值即可。例如，在下面的枚举中，green 的值为 5。
enum color { red, green=5, blue };
//在这里，blue 的值为 6，因为默认情况下，每个名称都会比它前面一个名称大 1，但 red 的值依然为 0。
```

#### **size_t**

```c++
//size_t的定义
typedef unsigned int size_t;
```

```c++
  /**
  它是一种 整型 类型，里面保存的是一个整数，就像 int, long 那样。这种整数用来记录一个大小(size)。size_t 的全称应该是 size type，就是说 一种用来记录大小的数据类型。
通常我们用 sizeof(XXX) 操作，这个操作所得到的结果就是 size_t 类型。
因为 size_t 类型的数据其实是保存了一个整数，所以它也可以做加减乘除，也可以转化为 int 并赋值给 int 类型的变量。
类似的还有 wchar_t, ptrdiff_t。
wchar_t 就是 wide char type， 一种用来记录一个宽字符的数据类型 。
ptrdiff_t 就是 pointer difference type， 一种用来记录两个指针之间的距离的数据类型 。
通常，size_t 和 ptrdiff_t 都是用 typedef 来实现的。
  **/

   int i=100;                   // 定义一个 int 类型的变量 i
   size_t size=sizeof(i);   // 用 sizeof 操作得到变量i的类型的大小
// 这是一个size_t类型的值
// 可以用来对一个size_t类型的变量做初始化

    i=(int)size;             // size_t 类型的值可以转化为 int 类型的值
    std::cout<<i<<"\n";
    char c='a';              // c 保存了字符 a，占一个字节
    wchar_t wc=L'a';         // wc 保存了宽字符 a，占两个字节
// 注意 'a' 表示字符 a，L'a' 表示宽字符 a
    std::cout<<wc<<"\n";
    int arr[]={1,2,3,4,5};   // 定义一个数组
    int *p1=&arr[0];         // 取得数组中元素的地址，赋值给指针
    int *p2=&arr[3];
    ptrdiff_t diff=p2-p1;    // 指针的减法可以计算两个指针之间相隔的元素个数
    std::cout<<diff<<"\n";
// 所得结果是一个 ptrdiff_t 类型

    i=(int)diff;             // ptrdiff_t 类型的值可以转化为 int 类型的值
    std::cout<<i<<"\n";
    return 0;
```

### 3、变量

- 变量其实只不过是**程序可操作的存储区的名称**。C++ 中每个变量都有指定的类型，类型决定了变量存储的大小和布局，该范围内的值都可以存储在内存中，运算符可应用于变量上。

#### 程序内存分配　 

1. **栈区：** 由编译器自动分配释放，像局部变量，函数参数，都是在栈区。会随着作用于退出而释放空间。
2. **堆区：**程序员分配并释放的区域，像malloc(c),new(c++) 
3. **全局数据区(静态区)：**全局变量和静态变量的存储是放在一块的，初始化的全局变量和静态变量在一块区域，未初始化的全局变量和未初始化的静态变量在相邻的另一块区域。程序结束释放。
4. **代码区**

#### **声明和定义：**

  ```java
  - 声明（declaration）：意味着告诉编译器关于变量名称、变量类型、变量大小、函数名称、结构名称、大小等等信息，并且在声明阶段不会给变量分配任何的内存。
  - 定义（definition）：定义就是在变量声明后，给它分配上内存。可以看成“定义 = 声明 + 内存分配”。
  - 声明和定义还有一种常见的，就是extern修饰的变量。当使用extern关键字修饰变量（未初始化），表示变量声明。当在另一个文件中，为extern关键字修饰的变量赋值时，表示变量定义。
  ```

  **变量声明**向编译器保证变量以给定的**类型和名称存在**，这样编译器在不需要知道变量完整细节的情况下也能继续进一步的编译。**变量声明只在编译时有它的意义**，在程序**连接时编译器需要实际的变量声明**。

  **当您使用多个文件且只在其中一个文件中定义变量时（定义变量的文件在程序连接时是可用的），变量声明就显得非常有用。**您可以使用 **extern**关键字在任何地方声明一个变量。虽然您可以在 C++ 程序中多次声明一个变量，但变量只能在某个文件、函数或代码块中被定义一次。

  定义包含了声明，但是声明不包含定义，如

  ```c++
  int a = 0;     //定义并声明了变量 a
  extern int a;  //只是声明了有一个变量 a 存在，具体 a 在哪定义的，需要编译器编译的时候去找。
  ```

  函数也是类似，定义的时候同时声明。但如果只是声明，编译器只知道有这么个函数，具体函数怎么定义的要编译器去找。

  ```c++
  void fun1();  //函数声明
  
  void fun1(){  //函数定义
      cout<<"fun1"<<endl;
  }
  ```

#### 变量的作用域

作用域是程序的一个区域，一般来说有三个地方可以定义变量：

- 在函数或一个**代码块内部**声明的变量，称为局部变量。
- 在函数参数的定义中声明的变量，称为**形式参数**。
- 在所有函数外部声明的变量，称为**全局变量**。

#### **全局变量、局部变量、静态全局变量、静态局部变量的区别**

  ```java
  1、从作用域看：
  //全局变量
  具有全局作用域。全局变量只需在一个源文件中定义，就可以作用于所有的源文件。当然，其他不包含全局变量定义的源文件需要用extern 关键字再次声明这个全局变量。
  //静态局部变量
  具有局部作用域，它只被初始化一次，自从第一次被初始化直到程序运行结束都一直存在，它和全局变量的区别在于全局变量对所有的函数都是可见的，而静态局部变量只对定义自己的函数体始终可见。
  //局部变量
  也只有局部作用域，它是自动对象（auto），它在程序运行期间不是一直存在，而是只在函数执行期间存在，函数的一次调用执行结束后，变量被撤销，其所占用的内存也被收回。
  //静态全局变量
  也具有全局作用域，它与全局变量的区别在于如果程序包含多个文件的话，它作用于定义它的文件里，不能作用到其它文件里，即被static关键字修饰过的变量具有文件作用域。这样即使两个不同的源文件都定义了相同名字的静态全局变量，它们也是不同的变量。
  
  2、从分配内存空间看：
  全局变量，静态局部变量，静态全局变量都在静态存储区分配空间，而局部变量在栈里分配空间。
  
  全局变量本身就是静态存储方式， 静态全局变量当然也是静态存储方式。这两者在存储方式上并无不同。这两者的区别虽在于非静态全局变量的作用域是整个源程序，当一个源程序由多个源文件组成时，非静态的全局变量在各个源文件中都是有效的。 而静态全局变量则限制了其作用域， 即只在定义该变量的源文件内有效，在同一源程序的其它源文件中不能使用它。由于静态全局变量的作用域局限于一个源文件内，只能为该源文件内的函数公用，因此可以避免在其它源文件中引起错误。
   1)、静态变量会被放在程序的静态数据存储区（数据段）(全局可见)中，这样可以在下一次调用的时候还可以保持原来的赋值。这一点是它与堆栈变量和堆变量的区别。
   2)、变量用static告知编译器，自己仅仅在变量的作用范围内可见。这一点是它与全局变量的区别。
  从以上分析可以看出， 把局部变量改变为静态变量后是改变了它的存储方式即改变了它的生存期。把全局变量改变为静态变量后是改变了它的作用域，限制了它的使用范围。因此static 这个说明符在不同的地方所起的作用是不同的。应予以注意。
  
  static 全局变量:改变作用范围，不改变存储位置
  static 局部变量：改变存储位置，不改变作用范围
  静态函数 ：在函数的返回类型前加上static关键字,函数即被定义为静态函数。静态函数与普通函数不同，它只能在声明它的文件当中可见，不能被其它文件使用。
  如果在一个源文件中定义的函数，只能被本文件中的函数调用，而不能被同一程序其它文件中的函数调用，这种函数也称为内部函数。定义一个内部函数，只需在函数类型前再加一个“static”关键字即可
  ```

#### C++ 中的左值（Lvalues）和右值（Rvalues）

C++ 中有两种类型的表达式：

- **左值（lvalue）：**指向内存位置的表达式被称为左值（lvalue）表达式。左值可以出现在赋值号的左边或右边。
- **右值（rvalue）：**术语右值（rvalue）指的是存储在内存中某些地址的数值。右值是不能对其进行赋值的表达式，也就是说，右值可以出现在赋值号的右边，但不能出现在赋值号的左边。

变量是左值，因此可以出现在赋值号的左边。数值型的字面值是右值，因此不能被赋值，不能出现在赋值号的左边。

```c++
//下面是一个有效的语句：
int g = 20;
//但是下面这个就不是一个有效的语句，会生成编译时错误：
10 = 20;
```

### 4、常量

常量是固定值，在程序执行期间不会改变。这些固定的值，又叫做**字面量**。
常量可以是任何的基本数据类型，可分为**整型数字、浮点数字、字符、字符串和布尔值。**
**常量就像是常规的变量，只不过常量的值在定义后不能进行修改。**

#### 宏定义 #define 和常量 const 的区别

```c++
1、类型和安全检查不同
宏定义是字符替换，没有数据类型的区别，同时这种替换没有类型安全检查，可能产生边际效应等错误；
const常量是常量的声明，有类型区别，需要在编译阶段进行类型检查

2、编译器处理不同
宏定义是一个"编译时"概念，在预处理阶段展开，不能对宏定义进行调试，生命周期结束与编译时期；
const常量是一个"运行时"概念，在程序运行使用，类似于一个只读行数据

3、存储方式不同
宏定义是直接替换，不会分配内存，存储与程序的代码段中；
const常量需要进行内存分配，存储与程序的数据段中

4、定义域不同
define是全局定义

5、定义后能否取消
宏定义可以通过#undef来使之前的宏定义失效
const常量定义后将在定义域内永久有效

6、是否可以做函数参数
宏定义不能作为参数传递给函数
const常量可以在函数的参数列表中出现，但是在函数内部不可以改变其值；
```

#### [const char*, char const*, char*const 的区别](https://www.runoob.com/w3cnote/const-char.html)

Bjarne在他的The C++ Programming Language里面给出过一个助记的方法：**把一个声明从右向左读:以*为分界线**。

```c++
char * const cp; ( * 读成 pointer to ) 
cp is a const pointer to char 

const char * p; 
p is a pointer to const char; 

char const * p; 
//同上因为C++里面没有const*的运算符，所以const只能属于前面的类型。
//C++标准规定，const关键字放在类型或变量名之前等价的。
```

```c++
int i = 100;                   // 定义一个 int 类型的变量 i
    const int n = 5;    //same as below
    int const m = 10;

    const int *p;    //same as below  const (int) * p
    int const *q;    // (int) const *p

    char **p1;
//    pointer to    pointer to    char 
    const char **p2;
//    pointer to    pointer to const char 
    char *const *p3;
//    pointer to const pointer to    char 
    const char *const *p4;
//    pointer to const pointer to const char 
    char **const p5 = nullptr;
// const pointer to    pointer to    char 
    const char **const p6 = nullptr;
// const pointer to    pointer to const char 
    char *const *const p7 = nullptr;
// const pointer to const pointer to    char 
    const char *const *const p8 = nullptr;
// const pointer to const pointer to const char
```



### 5、修饰符类型

C++ 允许在 **char、int 和 double** 数据类型前放置修饰符。修饰符用于改变基本类型的含义，所以它更能满足各种情境的需求。

下面列出了数据类型修饰符：

- signed
- unsigned
- long
- short

修饰符 **signed、unsigned、long 和 short** 可应用于整型，**signed** 和 **unsigned** 可应用于字符型，**long** 可应用于双精度型。

修饰符 **signed** 和 **unsigned** 也可以作为 **long** 或 **short** 修饰符的前缀。例如：**unsigned long int**。

C++ 允许使用速记符号来声明**无符号短整数**或**无符号长整数**。您可以不写 int，只写单词 **unsigned、short** 或 **unsigned、long**，int 是隐含的。例如，下面的两个语句都声明了无符号整型变量。

```c++
unsigned x;
unsigned int y;


#include <iostream>
using namespace std;
/* 
 * 这个程序演示了有符号整数和无符号整数之间的差别
*/
int main()
{
   short int i;           // 有符号短整数
   short unsigned int j;  // 无符号短整数
   j = 50000;
   i = j;
   cout << i << " " << j;
   return 0;
}
当上面的程序运行时，会输出下列结果：
-15536 50000
无符号短整数 50,000 的位模式被解释为有符号短整数 -15,536。
```

#### 类型限定符

| 限定符   | 含义                                                         |
| -------- | ------------------------------------------------------------ |
| const    | **const** 类型的对象在程序执行期间不能被修改改变。           |
| volatile | 修饰符 **volatile** 告诉编译器不需要优化volatile声明的变量，让程序可以直接从内存中读取变量。对于一般的变量编译器会对变量进行优化，将内存中的变量值放在寄存器中以加快读写效率。 |
| restrict | 由 **restrict** 修饰的指针是唯一一种访问它所指向的对象的方式。只有 C99 增加了新的类型限定符 restrict。 |

- [const关键字小结](https://www.runoob.com/w3cnote/cpp-const-keyword.html)



### 6、存储类

存储类定义 C++ 程序中**变量/函数**的**范围（可见性）和生命周期**。这些说明符放置在它们所修饰的类型**之前**。下面列出 C++ 程序中可用的存储类：

- **auto**
- **register**     //弃用
- **static**
- **extern**
- **mutable**
- **thread_local (C++11)**

从 C++ 11 开始，**auto 关键字不再是 C++ 存储类说明符，且 register 关键字被弃用**。

#### auto 存储类

自 C++ 11 以来，**auto** 关键字用于两种情况：**声明变量时根据初始化表达式自动推断该变量的类型、声明函数时函数返回值的占位符。**

C++98标准中auto关键字用于自动变量的声明，但由于使用极少且多余，在C++11中已删除这一用法。

根据初始化表达式自动推断被声明的变量的类型，如：

```c++
auto f=3.14;      //double
auto s("hello");  //const char*
auto z = new auto(9); // int*
auto x1 = 5, x2 = 5.0, x3='r';//错误，必须是初始化为同一类型
```

#### register 存储类

**register** 存储类用于定义存储在寄存器中而不是 RAM 中的局部变量。这意味着变量的最大尺寸等于寄存器的大小（通常是一个词），且不能对它应用一元的 '&' 运算符（因为它没有内存位置）。 c++17去掉了它

```c++
{
   register int  miles;
}
```

寄存器只用于需要快速访问的变量，比如计数器。还应注意的是，定义 'register' 并不意味着变量将被存储在寄存器中，它意味着变量可能存储在寄存器中，这取决于硬件和实现的限制。

#### static 存储类

**static** 存储类指示编译器在程序的生命周期内保持**局部变量**的存在，而不需要在每次它进入和离开作用域时进行创建和销毁。因此，使用 static 修饰局部变量可以在函数调用之间保持局部变量的值。
static 修饰符也可以应用于全局变量。**当 static 修饰全局变量时，会使变量的作用域限制在声明它的文件内。**
在 C++ 中，当 static 用在类数据成员上时，会导致仅有一个该成员的副本被类的所有对象共享。

```c++
void func(void);
 
static int count = 10; /* 全局变量 */
 
int main()
{
    while(count--)
    {
       func();
    }
    return 0;
}
// 函数定义
void func( void )
{
    static int i = 5; // 局部静态变量
    i++;
    std::cout << "变量 i 为 " << i ;
    std::cout << " , 变量 count 为 " << count << std::endl;
}
//变量 i 为 6 , 变量 count 为 9
变量 i 为 7 , 变量 count 为 8
变量 i 为 8 , 变量 count 为 7
变量 i 为 9 , 变量 count 为 6
变量 i 为 10 , 变量 count 为 5
变量 i 为 11 , 变量 count 为 4
变量 i 为 12 , 变量 count 为 3
变量 i 为 13 , 变量 count 为 2
变量 i 为 14 , 变量 count 为 1
变量 i 为 15 , 变量 count 为 0
```

#### extern 存储类

**extern** 存储类用于提供一个**全局变量的引用**，全局变量对**所有的程序文件**都是可见的。当您使用 'extern' 时，对于无法初始化的变量，会把变量名指向一个之前定义过的存储位置。

当您有多个文件且定义了一个可以在其他文件中使用的全局变量或函数时，可以在其他文件中使用 *extern* 来得到已定义的变量或函数的引用。可以这么理解，*extern* 是用来在另一个文件中声明一个全局变量或函数。

extern 修饰符通常用于当有两个或多个文件共享相同的全局变量或函数的时候，如下所示：

```c++
//第一个文件：main.cpp
#include <iostream>
int count ;
extern void write_extern();
int main()
{
   count = 5;
   write_extern();
}
//第二个文件：support.cpp
#include <iostream>
extern int count;
void write_extern(void)
{
   std::cout << "Count is " << count << std::endl;
}
```



### 7、运算符

#### 杂项运算符

| 运算符               | 描述                                                         |
| -------------------- | ------------------------------------------------------------ |
| sizeof               | [sizeof 运算符](https://www.runoob.com/cplusplus/cpp-sizeof-operator.html)返回变量的大小。例如，sizeof(a) 将返回 4，其中 a 是整数。 |
| Condition ? X : Y    | [条件运算符](https://www.runoob.com/cplusplus/cpp-conditional-operator.html)。如果 Condition 为真 ? 则值为 X : 否则值为 Y。 |
| ,                    | [逗号运算符](https://www.runoob.com/cplusplus/cpp-comma-operator.html)会顺序执行一系列运算。整个逗号表达式的值是以逗号分隔的列表中的最后一个表达式的值。 |
| .（点）和 ->（箭头） | [成员运算符](https://www.runoob.com/cplusplus/cpp-member-operators.html)用于引用类、结构和共用体的成员。 |
| Cast                 | [强制转换运算符](https://www.runoob.com/cplusplus/cpp-casting-operators.html)把一种数据类型转换为另一种数据类型。例如，int(2.2000) 将返回 2。 |
| &                    | [指针运算符 &](https://www.runoob.com/cplusplus/cpp-pointer-operators.html) 返回变量的地址。例如 &a; 将给出变量的实际地址。 |
| *                    | [指针运算符 *](https://www.runoob.com/cplusplus/cpp-pointer-operators.html) 指向一个变量。例如，*var; 将指向变量 var。 |



### 8、循环

#### 循环类型

| 循环类型                                                     | 描述                                                         |
| ------------------------------------------------------------ | ------------------------------------------------------------ |
| [while 循环](https://www.runoob.com/cplusplus/cpp-while-loop.html) | 当给定条件为真时，重复语句或语句组。它会在执行循环主体之前测试条件。 |
| [for 循环](https://www.runoob.com/cplusplus/cpp-for-loop.html) | 多次执行一个语句序列，简化管理循环变量的代码。               |
| [do...while 循环](https://www.runoob.com/cplusplus/cpp-do-while-loop.html) | 除了它是在循环主体结尾测试条件外，其他与 while 语句类似。    |
| [嵌套循环](https://www.runoob.com/cplusplus/cpp-nested-loops.html) | 您可以在 while、for 或 do..while 循环内使用一个或多个循环。  |

#### 循环控制语句

循环控制语句更改执行的正常序列。当执行离开一个范围时，所有在该范围中创建的自动对象都会被销毁。

| 控制语句                                                     | 描述                                                         |
| ------------------------------------------------------------ | ------------------------------------------------------------ |
| [break 语句](https://www.runoob.com/cplusplus/cpp-break-statement.html) | 终止 **loop** 或 **switch** 语句，程序流将继续执行紧接着 loop 或 switch 的下一条语句。 |
| [continue 语句](https://www.runoob.com/cplusplus/cpp-continue-statement.html) | 引起循环跳过主体的剩余部分，立即重新开始测试条件。           |
| [goto 语句](https://www.runoob.com/cplusplus/cpp-goto-statement.html) | 将控制转移到被标记的语句。但是不建议在程序中使用 goto 语句。 |



### 9、判断

#### 判断语句

| 语句                                                         | 描述                                                         |
| ------------------------------------------------------------ | ------------------------------------------------------------ |
| [if 语句](https://www.runoob.com/cplusplus/cpp-if.html)      | 一个 **if 语句** 由一个布尔表达式后跟一个或多个语句组成。    |
| [if...else 语句](https://www.runoob.com/cplusplus/cpp-if-else.html) | 一个 **if 语句** 后可跟一个可选的 **else 语句**，else 语句在布尔表达式为假时执行。 |
| [嵌套 if 语句](https://www.runoob.com/cplusplus/cpp-nested-if.html) | 您可以在一个 **if** 或 **else if** 语句内使用另一个 **if** 或 **else if** 语句。 |
| [switch 语句](https://www.runoob.com/cplusplus/cpp-switch.html) | 一个 **switch** 语句允许测试一个变量等于多个值时的情况。     |
| [嵌套 switch 语句](https://www.runoob.com/cplusplus/cpp-nested-switch.html) | 您可以在一个 **switch** 语句内使用另一个 **switch** 语句。   |

#### ? : 运算符

我们已经在前面的章节中讲解了 [**条件运算符 ? :**](https://www.runoob.com/cplusplus/cpp-conditional-operator.html)，可以用来替代 **if...else** 语句。它的一般形式如下：

```
Exp1 ? Exp2 : Exp3;
```



### 10、函数

#### 函数参数

如果函数要使用参数，则必须声明接受参数值的变量。这些变量称为函数的**形式参数**。

形式参数就像函数内的其他局部变量，在进入函数时被创建，退出函数时被销毁。

当调用函数时，有三种向函数传递参数的方式：

| 调用类型                                                     | 描述                                                         |
| ------------------------------------------------------------ | ------------------------------------------------------------ |
| [传值调用](https://www.runoob.com/cplusplus/cpp-function-call-by-value.html) | 该方法把参数的实际值复制给函数的形式参数。在这种情况下，修改函数内的形式参数对实际参数没有影响。 |
| [指针调用](https://www.runoob.com/cplusplus/cpp-function-call-by-pointer.html) | 该方法把参数的地址复制给形式参数。在函数内，该地址用于访问调用中要用到的实际参数。这意味着，修改形式参数会影响实际参数。 |
| [引用调用](https://www.runoob.com/cplusplus/cpp-function-call-by-reference.html) | 该方法把参数的引用复制给形式参数。在函数内，该引用用于访问调用中要用到的实际参数。这意味着，修改形式参数会影响实际参数。 |

默认情况下，C++ 使用**传值调用**来传递参数。一般来说，这意味着函数内的代码不能改变用于调用函数的参数。之前提到的实例，调用 max() 函数时，使用了相同的方法。

#### Lambda 函数与表达式

C++11 提供了对匿名函数的支持,称为 Lambda 函数(也叫 Lambda 表达式)。

```c++
[capture](parameters)->return-type{body}
```

-  **[capture]**：捕捉列表。捕捉列表总是出现在 lambda 表达式的开始处。事实上，[] 是 lambda 引出符。编译器根据该引出符判断接下来的代码是否是 lambda 函数。捕捉列表能够捕捉上下文中的变量供 lambda 函数使用。
-  **(parameters)**：参数列表。与普通函数的参数列表一致。如果不需要参数传递，则可以连同括号 () 一起省略。
-  **mutable**：mutable 修饰符。默认情况下，lambda 函数总是一个 **const 函数**，mutable 可以取消其常量性。在使用该修饰符时，参数列表不可省略（即使参数为空）。
-  **->return_type**：返回类型。用追踪返回类型形式声明函数的返回类型。出于方便，不需要返回值的时候也可以连同符号 -> 一起省略。此外，在返回类型明确的情况下，也可以省略该部分，让编译器对返回类型进行推导。
-  **{statement}**：函数体。内容与普通函数一样，不过除了可以使用参数之外，还可以使用所有捕获的变量。

```c++
//在 lambda 函数的定义式中，参数列表和返回类型都是可选部分，而捕捉列表和函数体都可能为空，C++ 中最简单的 lambda 函数只需要声明为：
[]{};
//例如：
[](int x, int y){ return x < y ; }
```

#### 闭包（Closure）**行为**[]

在Lambda表达式内可以访问当前作用域的变量，这是Lambda表达式的闭包（Closure）行为。 与JavaScript闭包不同，C++变量传递有传值和传引用的区别。可以通过前面的[]来指定：

- []：默认不捕获任何变量；使用未定义变量会引发错误。
- [=]：默认以值捕获所有变量；
- [&]：默认以引用捕获所有变量；
- [x]：仅以值捕获x，其它变量不捕获；
- [&x]：仅以引用捕获x，其它变量不捕获；
- [x, &y] // x以传值方式传入（默认），y以引用方式传入。
- [=, &x]：默认以值捕获所有变量，但是x是例外，通过引用捕获；
- [&, x]：默认以引用捕获所有变量，但是x是例外，通过值捕获；
- [this]：通过引用捕获当前对象（其实是复制指针）；
- [*this]：通过传值方式捕获当前对象；

另外有一点需要注意。对于[=]或[&]的形式，lambda 表达式可以直接使用 this 指针。但是，对于[]的形式，如果要使用 this 指针，必须显式传入：

```c++
[this]() { this->someFunc(); }();
```



### 11、数组

```c++
using namespace std;
 
#include <iomanip>
using std::setw;
 
int main ()
{
   int n[ 10 ]; // n 是一个包含 10 个整数的数组
 
   int a[3][4] = {  
     {0, 1, 2, 3} ,   /*  初始化索引号为 0 的行 */
     {4, 5, 6, 7} ,   /*  初始化索引号为 1 的行 */
     {8, 9, 10, 11}   /*  初始化索引号为 2 的行 */
    };
   return 0;
}

数组名是一个指向数组中第一个元素的常量指针。因此，在下面的声明中：

double balance[50];
balance 是一个指向 &balance[0] 的指针，即数组 balance 的第一个元素的地址。因此，下面的程序片段把 p 赋值为 balance 的第一个元素的地址：

double *p;
double balance[10];

p = balance;
```



### 12、字符串

#### C 风格字符串

C 风格的字符串起源于 C 语言，并在 C++ 中继续得到支持。字符串实际上是使用 **null** 字符 '\0' 终止的一维字符数组。因此，一个以 null 结尾的字符串，包含了组成字符串的字符。

下面的声明和初始化创建了一个 "Hello" 字符串。由于在数组的末尾存储了空字符，所以字符数组的大小比单词 "Hello" 的字符数多一个。

```c++
char greeting[6] = {'H', 'e', 'l', 'l', 'o', '\0'};
```

依据数组初始化规则，您可以把上面的语句写成以下语句：

```c++
char greeting[] = "Hello";
```

其实，您不需要把 *null* 字符放在字符串常量的末尾。C++ 编译器会在初始化数组时，自动把 '\0' 放在字符串的末尾。让我们尝试输出上面的字符串

#### 字符串函数

| 序号 | 函数 & 目的                                                  |
| ---- | ------------------------------------------------------------ |
| 1    | **strcpy(s1, s2);** 复制字符串 s2 到字符串 s1。              |
| 2    | **strcat(s1, s2);** 连接字符串 s2 到字符串 s1 的末尾。       |
| 3    | **strlen(s1);** 返回字符串 s1 的长度。                       |
| 4    | **strcmp(s1, s2);** 如果 s1 和 s2 是相同的，则返回 0；如果 s1<s2 则返回值小于 0；如果 s1>s2 则返回值大于 0。 |
| 5    | **strchr(s1, ch);** 返回一个指针，指向字符串 s1 中字符 ch 的第一次出现的位置。 |
| 6    | **strstr(s1, s2);** 返回一个指针，指向字符串 s1 中字符串 s2 的第一次出现的位置。 |

下面的实例使用了上述的一些函数：

```c++
#include <iostream>
#include <cstring>
using namespace std;

int main ()
{
   char str1[11] = "Hello";
   char str2[11] = "World";
   char str3[11];
   int  len ;

   // 复制 str1 到 str3
   strcpy( str3, str1);
   cout << "strcpy( str3, str1) : " << str3 << endl;

   // 连接 str1 和 str2
   strcat( str1, str2);
   cout << "strcat( str1, str2): " << str1 << endl;

   // 连接后，str1 的总长度
   len = strlen(str1);
   cout << "strlen(str1) : " << len << endl;

   return 0;
}
```

当上面的代码被编译和执行时，它会产生下列结果：

```c++
strcpy( str3, str1) : Hello
strcat( str1, str2): HelloWorld
strlen(str1) : 10
```

#### C++ 中的 String 类

C++ 标准库提供了 **string** 类类型，支持上述所有的操作，另外还增加了其他更多的功能

```c++
#include <iostream>
#include <string>

using namespace std;

int main ()
{
   string str1 = "Hello";
   string str2 = "World";
   string str3;
   int  len ;

   // 复制 str1 到 str3
   str3 = str1;
   cout << "str3 : " << str3 << endl;

   // 连接 str1 和 str2
   str3 = str1 + str2;
   cout << "str1 + str2 : " << str3 << endl;

   // 连接后，str3 的总长度
   len = str3.size();
   cout << "str3.size() :  " << len << endl;

   return 0;
}
//当上面的代码被编译和执行时，结果同上。
```



### 13、指针

#### 什么是指针？

**指针**是一个变量，其值为另一个变量的地址，即，内存位置的直接地址。就像其他变量或常量一样，您必须在使用指针存储其他变量地址之前，对其进行声明。指针变量声明的一般形式为：

```c++
type *var-name;
```

在这里，**type** 是指针的基类型，它必须是一个有效的 C++ 数据类型，**var-name** 是指针变量的名称。用来声明指针的星号 * 与乘法中使用的星号是相同的。

**所有指针的值的实际数据类型，不管是整型、浮点型、字符型，还是其他的数据类型，都是一样的，都是一个代表内存地址的长的十六进制数。不同数据类型的指针之间唯一的不同是，指针所指向的变量或常量的数据类型不同。**

#### C++ 指针详解

在 C++ 中，有很多指针相关的概念，这些概念都很简单，但是都很重要。下面列出了 C++ 程序员必须清楚的一些与指针相关的重要概念：

| 概念                                                         | 描述                                                         |
| ------------------------------------------------------------ | ------------------------------------------------------------ |
| [C++ Null 指针](https://www.runoob.com/cplusplus/cpp-null-pointers.html) | C++ 支持空指针。NULL 指针是一个定义在标准库中的值为零的常量。 |
| [C++ 指针的算术运算](https://www.runoob.com/cplusplus/cpp-pointer-arithmetic.html) | 可以对指针进行四种算术运算：++、--、+、-                     |
| [C++ 指针 vs 数组](https://www.runoob.com/cplusplus/cpp-pointers-vs-arrays.html) | 指针和数组之间有着密切的关系。                               |
| [C++ 指针数组](https://www.runoob.com/cplusplus/cpp-array-of-pointers.html) | 可以定义用来存储指针的数组。                                 |
| [C++ 指向指针的指针](https://www.runoob.com/cplusplus/cpp-pointer-to-pointer.html) | C++ 允许指向指针的指针。                                     |
| [C++ 传递指针给函数](https://www.runoob.com/cplusplus/cpp-passing-pointers-to-functions.html) | 通过引用或地址传递参数，使传递的参数在调用函数中被改变。     |
| [C++ 从函数返回指针](https://www.runoob.com/cplusplus/cpp-return-pointer-from-functions.html) | C++ 允许函数返回指针到局部变量、静态变量和动态内存分配。     |



### 14、引用

引用变量是一个别名，也就是说，它是某个已存在变量的另一个名字。一旦把引用初始化为某个变量，就可以使用该引用名称或变量名称来指向变量

#### C++ 引用 vs 指针

引用很容易与指针混淆，它们之间有三个主要的不同：

- 不存在空引用。引用必须连接到一块合法的内存。
- 一旦引用被初始化为一个对象，就不能被指向到另一个对象。指针可以在任何时候指向到另一个对象。
- 引用必须在创建时被初始化。指针可以在任何时间被初始化。

#### 引用作为返回值

当返回一个引用时，要注意被引用的对象不能超出作用域。所以返回一个对局部变量的引用是不合法的，但是，可以返回一个对静态变量的引用。

```c++
int& func() {
   int q;
   //! return q; // 在编译时发生错误
   static int x;
   return x;     // 安全，x 在函数作用域外依然是有效的
}
```



### 15、日期 & 时间

- <https://www.runoob.com/cplusplus/cpp-date-time.html>



### 16、基本的输入输出

- <https://www.runoob.com/cplusplus/cpp-basic-input-output.html>



### 17、数据结构

类与结构体在 C++ 中只有两点区别，除此这外无任何区别。

- （1）class 中默认的成员访问权限是 private 的，而 struct 中则是 public 的。
- （2）从 class 继承默认是 private 继承，而从 struct 继承默认是 public 继承。
- （3）class 可以定义模板，而 struct 不可以。