在Linux下使用GCC将源码编译成可执行文件的过程可以分解为4个步骤，分别是预处理（Prepressing）、编译（Compilation）、汇编（Assembly）和链接（Linking）。

一个简单的hello word程序编译过程如下：

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glfpho65d2j30oy0o8qdd.jpg" alt="image-20201207232456976" style="zoom:53%;" />

#### 1. 预处理

首先源代码文件（.c/.cpp）和相关头文件（.h/.hpp）被预处理器cpp预编译成.i文件（C++为.ii）。预处理命令为：

```
gcc –E hello.c –o hello.i
```

预编译过程主要处理那些源代码中以#开始的预编译指令，主要处理规则如下：

u 将所有的#define删除，并且展开所有的宏定义；

u 处理所有条件编译指令，如#if，#ifdef、#elif，#else、#endif等；

u 处理#include预编译指令，将被包含的文件插入到该预编译指令的位置。该过程递归进行，及被包含的文件可能还包含其他文件。

u 删除所有的注释//和 /**/；

u 添加行号和文件标识，如#2 “hello.c” 2,以便于编译时编译器产生调试用的行号信息及用于编译时产生编译错误或警告时能够显示行号信息；

u 保留所有的#pragma编译器指令，因为编译器须要使用它们。

所以如果想看宏的展开，可以使用gcc -E。

#### 2. 编译

编译过程就是把预处理完的文件进行一系列词法分析，语法分析，语义分析及优化后生成相应的汇编代码文件（.s）。

语法分析：分析表达式是否遵循语法规则

词法分析：分析关键字，标识符，立即数是否合法

语义分析：在语法分析基础上进一步分析表达式是否合法

编译的命令为：

```
gcc –S hello.i –o hello.s
```

或者从源文件直接输出汇编代码文件：

```
gcc –S hello.c –o hello.s
```

#### 3. 汇编

汇编就是将汇编代码转变成机器可以执行的命令，生成目标文件（.o），汇编器as根据汇编指令和机器指令的对照表一一翻译即可完成。汇编的命令为：

```
gcc –c hello.s –o hello.o
```

或者从源文件直接输出目标文件：

```
gcc –c hello.c –o hello.o
```

#### 4. 链接

链接就是链接器ld将各个目标文件组装在一起，解决符号依赖，库依赖关系，并生成可执行文件。

链接过程是由于汇编程序生成的目标文件并不能立即就被执行，其中可能还有许多没有解决的问题。例如，某个源文件中的函数可能引用了另一个源文件中定义的某个符号（如变量或者函数调用等）；在程序中可能调用了某个库文件中的函数，等等。所有的这些问题，都需要经链接程序的处理方能得以解决。

链接程序的主要工作就是将有关的目标文件彼此相连接，也即将在一个文件中引用的符号同该符号在另外一个文件中的定义连接起来，使得所有的这些目标文件成为一个能够按操作系统装入执行的统一整体。根据开发人员指定的同库函数的链接方式的不同，链接处理可分为两种：

- 1）静态链接
  在这种链接方式下，函数的代码将从其所在地静态链接库中被拷贝到最终的可执行程序中。这样该程序在被执行时这些代码将被装入到该进程的虚拟地址空间中。静态链接库实际上是一个目标文件的集合，其中的每个文件含有库中的一个或者一组相关函数的代码。
- 2）动态链接
  在此种方式下，函数的代码被放到称作是动态链接库或共享对象的某个目标文件中。链接程序此时所做的只是在最终的可执行程序中记录下共享对象的名字以及其它少量的登记信息。在此可执行文件被执行时，动态链接库的全部内容将被映射到运行时相应进程的虚地址空间。动态链接程序将根据可执行程序中记录的信息找到相应的函数代码。

对于可执行文件中的函数调用，可分别采用动态链接或静态链接的方法。

使用动态链接能够使最终的可执行文件比较短小（没有将函数部分拷贝），并且当共享对象被多个进程使用时能节约一些内存，因为在内存中只需要保存一份此共享对象的代码。但并不是使用动态链接就一定比使用静态链接要优越。在某些情况下动态链接可能带来一些性能上损害，例如移植性将大大降低。

我们在linux使用的gcc编译器便是把以上的几个过程进行捆绑，使用户只使用一次命令就把编译工作完成。
链接的命令为：ld。

一般我们使用一条命令就可以完成上述4个步骤：

```
gcc hello.c
```

实际上gcc只是一些其它程序的包装，它会根据不同参数去调用预编译编译程序cc1、汇编器as、链接器ld。

总结起来编译过程就上面的四个过程：预编译、编译、汇编、链接。了解了这四个过程中所做的工作，对我们理解头文件、库等的工作过程是有帮助的，而且清楚的了解编译链接过程还对我们在编程时定位错误，以及编程时尽量调动编译器的检测错误会有很大的帮助的。



- https://www.baidu.com/s?wd=c%E8%AF%AD%E8%A8%80%E7%BC%96%E8%AF%91%E8%BF%87%E7%A8%8B&rsv_spt=1&rsv_iqid=0xead546fb000ca0be&issp=1&f=8&rsv_bp=1&rsv_idx=2&ie=utf-8&rqlang=cn&tn=baiduhome_pg&rsv_enter=1&rsv_dl=tb&oq=c%25E8%25AF%25AD%25E8%25A8%2580%25E7%25BC%2596%25E8%25AF%2591%25E5%2599%25A8&rsv_btype=t&inputT=3009&rsv_t=ca71Hhu5L58qN7bjSkjEAebUJ3Amr3gugj5gU1wP0b0UH8dHoCSzcS2wbTvbRUsSmeuk&rsv_pq=c87fd642000f164a&rsv_sug3=29&rsv_sug1=23&rsv_sug7=100&rsv_sug2=0&rsv_sug4=4082

- https://blog.csdn.net/zhangluli/article/details/5153636
- https://www.cnblogs.com/CodeWorkerLiMing/p/10701568.html
- https://www.cnblogs.com/knife-king/p/11090029.html
- https://blog.csdn.net/weixin_41143631/article/details/81221777
- https://www.jianshu.com/p/d05df6601faa
- https://www.pianshen.com/article/9716882497/
- https://www.pianshen.com/article/11521001465/





## 编译原理

 

# 基础概念

### 1.编译器和解释器的区别：

- 编译器是一种程序，在源语言中读取程序，转为目标代码，并报告翻译过程中检测到源程序的任何错误；
- 解释器是直接在用户提供的输入上执行源程序中指定的操作；
- 编译器生成机器语言目标程序比解释器快，但解释器通过执行源代码语句，可以提供更好的错误诊断

### 2.编译过程

- 词法分析（识别单词）
- 语法分析（分析语法）
- 语义分析（分析语义）
- 中间代码生成（初步翻译结果）
- 代码优化（修改初步结果）
- 目标代码生成（最终结果）
  ![编译过程](https://tva1.sinaimg.cn/large/0081Kckwly1glfq09kk30j31390agaaj.jpg)



- https://www.pianshen.com/article/57788201/