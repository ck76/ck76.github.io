https://www.zhihu.com/question/49176184

糟糕，废话写太多了。请题主（和其他对这个话题感兴趣的同学）耐心看到最后…
没耐心的话请直接跳到最后一部分看Tachyon相关的介绍。

JavaScript引擎的实现多如牛毛啊。用C或C++实现的固然很多，但其它语言实现的也不少哇。随便举几个例子：

- D：[DMDScript](https://link.zhihu.com/?target=http%3A//www.digitalmars.com/dscript/)
- Java：[Rhino](https://link.zhihu.com/?target=https%3A//developer.mozilla.org/en-US/docs/Mozilla/Projects/Rhino)、[Nashorn](https://link.zhihu.com/?target=http%3A//openjdk.java.net/projects/nashorn/)、[DynJS](https://link.zhihu.com/?target=http%3A//dynjs.org/)、[Truffle/JS](https://link.zhihu.com/?target=http%3A//www.oracle.com/technetwork/oracle-labs/program-languages/overview/index-2301583.html) 等
- C#：Managed JScript、SPUR 等
- F#：IronJS
- Python：[jispy](https://link.zhihu.com/?target=https%3A//github.com/sumukhbarve/jispy)（实现了一个JavaScript子集）
- RPython：[lang-js](https://link.zhihu.com/?target=https%3A//bitbucket.org/pypy/lang-js/)（项目已挂）
- JavaScript：Narcissus、Continuum、Babel、Tachyon 等
- … 大概还有很多我没列举或者不知道的

以前收集过一些JavaScript引擎的实现的资料，请参考：[[链接帖\] 各JavaScript引擎的简介，及相关资料/博客收集帖](https://link.zhihu.com/?target=http%3A//hllvm.group.iteye.com/group/topic/37596)

别的runtime / VM相关，之前回答过一发：[是否存在Runtime System 或VM 不由C 或C++ 实现的编程语言？ - RednaxelaFX 的回答](https://www.zhihu.com/question/27875742/answer/39279855)

原问题：

> 感觉JavaScript的运行环境都是编译语言写出来的

不一定。题主看上面列举的那些实现，是不是都是用“编译语言”实现的？

> 那么对于一些与操作系统或者复杂运算等功能，是不是都是靠编译语言完成，然后由JavaScript调用这种方式实现的？

前提条件已经为假，后面这个推论就不用看了。

JavaScript在语言层面并没有暴露任何操作系统层面的功能。它的本意是要嵌在某种宿主环境里，由宿主注入它希望暴露出来的功能。例如说V8自身并没有读写文件的能力，而Node.js作为一个宿主环境注入操作文件的API给V8，在Node.js里写JavaScript就可以操作文件了。
于是宿主是不是都是“编译型语言”实现的呢？也不一定。浏览器是最常见的宿主环境，确实大多数浏览器都是C或者C++实现的，但也有用Java、C#、Rust等语言实现的；而其它类型的宿主环境也有很多，例如说用Java实现的好几种服务器端环境（Node.jar / Avatar.js、Vert.x / Nodyn）。

========================================

> 那JIT技术在编译成机器码后，编译后的机器码能否脱离解释器独自运行呢？

题主很可能把“解释器”（interpreter）跟“运行时”（runtime）混为一谈了。

“运行时”，或者全称“运行时环境”（runtime environment）、“运行时支持库”（runtime library），是为编程语言在运行的时候提供支持功能的环境/库。大多数高级编程语言都要带着一个运行时库，要么用于支持语言层面的功能，要么提供标准库的实现。

典型的运行时库的例子：

- C：msvcrt、libc、glibc、libSystem.dylib
- C++：msvcrt、libstdc++、libc++
- D：Phobos、Tango，还有抽象的DRuntime
- Go：[Go runtime](https://link.zhihu.com/?target=https%3A//golang.org/pkg/runtime/)
- Rust：[Rust Runtime](https://link.zhihu.com/?target=https%3A//doc.rust-lang.org/0.12.0/guide-runtime.html)
- C#：.NET Framework（内含CLR）、.NET Core（内含CoreCLR）、.NET Native（内含Minimal Runtime）、Mono（内含Mono） 等
- Java：Oracle JRE（内含HotSpot VM）、IBM JRE（内含J9 VM） 等
- … 等等


大家都认同C语言是“编译型语言”，但是大家会一边嫌弃Java、.NET带着个“虚拟机”（实际上不仅指VM而指整个runtime），却不在意大部分用C写的程序也要带着运行时库，特别是实现C标准库的各种CRT（C Runtime）实现。
上面列举的情况里，C、C++、Rust等语言的“特别之处”在于，当只使用语言的相当大的特定子集时，它就不需要依赖于标准运行时库——不与标准库/运行时库链接——而可以独立运行。这些个子集可以几乎包含语言的所有语法结构。
这就使得这些语言适合编写最底层的软件代码，例如操作系统内核——这里所有运行时支持都得自己实现。

- C：只要不使用标准库，基本上整个语言都可以脱离标准库运行。特别的，只用宏（macro）所实现的库功能都会在编译时展开，所以不需要额外的运行时库来提供支持。
- C++：不用有需要运行时支持的运算符和标准库，语言的很大子集可以脱离标准库/运行时库运行。特别的，标准库里有很多功能是用纯模版实现的，这些都会在编译时展开，所以不需要额外的运行时库来提供支持。
- Rust：语言自身的语法结构以及大量的标准库功能都以“zero-overhead”为设计目标，尽可能做成可内联/可展开的，因而语言的几乎所有语法和相当大量的标准库功能都不需要带上运行时库来支持。主要是线程、I/O等功能会用到运行时库。


怎样的C程序会需要带上标准运行时库呢？只要用了下列功能的任何一个就需要：

- 动态内存管理：malloc、calloc、realloc、free 等
- 文件访问：fopen、fseek、fclose 等
- 输入输出：puts、printf、scanf 等
- 字符串操作与内存块操作：strlen、strcpy / memcpy、memset、strstr 等
- 数学函数：sin、cos、tan、pow、exp、abs、div、sqrt、fma 等
- 算法：qsort、bsearch 等

（当然，优化编译器可以把上述函数中一些简单的函数在编译时彻底内联，这样就不需要在编译后带上运行时库了。但其实这可以看作一种特殊的静态链接，还是跟标准库“链接”上了）
所以一般大家写的应用层面的C语言的程序，其实都是得带着运行时库的。无论是静态链接还是动态链接，带着就是带着了，不必害羞。

C++的话，在上述C的情况之上，还有一些东西只要用了就需要标准运行时库：

- 动态内存管理：在C标准库之外，还有C++标准库里的默认operator new、operator delete 等
- RTTI：dynamic_cast<>、typeid()、std::type_info 等
- 线程API：std::thread 等
- …


怎样的Java程序需要带上运行时库呢？

- 动态内存管理：new运算符以及所有会隐式创建对象的功能，以及自动回收无用内存（GC）的功能
- 动态类加载与链接
- 反射
- 多线程支持
- （以上功能需要JVM内直接实现，是语言内建功能就需要的）
- 其它标准库功能：集合、I/O、文件、字符串，等等。这些功能可以在JVM之上的层面实现
- …

如果不使用动态类加载、反射等功能，运行时需要维护的元数据就可以大幅减少，并且可以对程序做的静态分析的精度可以大幅提升。Java ME的CLDC就是这样的子集，它的JVM就比Java SE的JVM简单得多。

如果上面这些高级功能都不用，是不是可以得到一个不需要带额外的运行时库的Java子集呢？
答案是肯定的。如果把Java语法当C语法来写，只用静态方法（因为不能new）和原始类型（因为引用类型要用都得new），不用任何复杂的标准库功能，那么这个Java的子集是可以完全彻底编译成可裸奔的目标程序，不需要跟运行时库链接。此时的这个子集其实就跟没了指针和自定义值类型的C几乎一样…
大家实现过“[MiniJava](https://link.zhihu.com/?target=http%3A//www.cs.tufts.edu/~sguyer/classes/comp181-2006/minijava.html)”的编译器不？它所实现的基本上就是这样的不需要额外运行时支持库的Java子集。所以不要笑，这是实际存在的东西。

上面Java的列表里没有提到解释器或者JIT编译器。为啥？因为它们并不是非要不可的。
如果不使用动态类加载的功能，那么可以事先把一个Java程序需要用到的代码全部编译到目标代码，也就是完全的AOT编译。这样在运行时就不需要解释器或者JIT编译器了。AOT编译、解释器、JIT编译都是实现Java程序执行的方式，挑任意一种或多种都可以。

总有同学以写程序是否需要带运行时库来衡量一门语言是否能被“彻底编译”。这种表示方式是很具误导性的。上面已经展示了C与C++也需要带上运行时库的情况。**关键还是看某个具体程序使用了多复杂的功能，越复杂的功能就越需要带上一个复杂的运行时库。
**

更具有指导意义的说法是：**一门语言，剥离了运行时库也能使用的功能，与这门语言的完整功能相比，是多大的子集。**这样就好说了：C语言剥离了运行时库也几乎可以使用整个语言所有的语法功能，只是不能用标准库函数了；而Java的话就会被限制在一个很小的子集里，写不出一般结构的Java程序了。

另外一种有指导意义的问题：**一门语言是否能只靠自己来实现自己，最终得到一个可以独立运行的程序。**换句话说，这门语言是否可以“自举”。对这个感兴趣的话请继续往下读。
注意：一门语言是否能自举，跟这门语言的实现是否需要带一个运行时库，两者是没有必然联系的。

========================================

有些同学会喜欢偷换概念，把“不需要带运行时库”实质上定义为：

- 除了CRT之外不需要带额外的运行时库：那其实只要让某个语言的实现只依赖C标准库所实现的运行时功能即可。

- - 诚然，很多操作系统都有配套的默认CRT，所以只需要依赖CRT常常意味着不用带“额外的”运行时库；但也有很多嵌入式系统有很各自特殊的设计，并不提供完整的C标准库支持，要用那些功能怎么办？还是得要么自己实现（等于自己实现了个运行时库），要么想办法带上个标准的运行时库。

- 可以把运行时库静态链接打包成单一的可执行文件：有这种想法的同学需要好好补习编译原理的基础知识。

- - 打包成单一可执行文件有很多办法。一个普通的Java程序，完全可以把其所依赖的JAR包打包到可执行文件的资源里，而把其依赖的JVM功能静态链接到可执行文件里，最终也一样可以形成一个单一的可执行文件。其它带解释器或者复杂的运行时的语言实现也可以如法炮制。

不要掉进这俩坑里了。透过现象看本质。

========================================

要说一门语言是否“可以编译”，其实[Futamura映射](https://link.zhihu.com/?target=https%3A//en.wikipedia.org/wiki/Partial_evaluation%23Futamura_projections)的第一映射已经给出了答案：**只要能实现一门语言的解释器，就可以对这门语言做编译。**
一门语言 L 如果可以被解释器 I 实现，至少说明这门语言是**可计算**的。在这个前提下，把L语言的一段程序 S 与解释器 I 一起输入到一个partial evaluator，就可以得到编译后的程序 P。

```text
S -\
    |-> [ Partial Evaluator ] -> P
I -/
```

然后当我们要运行程序的时候，运行程序 P 并提供运行时输入，就得到了运行结果Result：

```text
Input -> [ P ] -> Result
```


本来一个解释器应该接受两种输入，一个是要解释执行的程序 S ，另一个是对 S 的输入 Input，最后运行得到结果：

```text
S     -\
       |-> [ I ] -> P
Input -/
```

这里partial evaluator做的事情就是：把程序 S 看作解释器 I 的固定输入，这样就可以依据S来把I的逻辑彻底展开，最后剩下的无法继续展开的程序 P 就是解释器 I 对程序 S 的特化，也就是编译的结果。
于是给程序 P 输入Input，就能得到跟原本解释执行一样的结果Result。

对此过程的形象介绍，请跳传送门：[Compilers for Free](https://link.zhihu.com/?target=http%3A//codon.com/compilers-for-free)，这边就不展开说了。
**<- 重要的事情再说一次：对一门语言是否可以编译，可以的话如何通过实现一个解释器就达到编译的目的，请跳上面这个传送门。**

PyPy 和 Truffle 项目是此概念的非常好的例子。它们都通过partial evaluation，让用户只需要实现一门语言的解释器，就最终得到这门语言的编译器（和运行时）。

========================================

说了半天，这跟JavaScript有什么关系呢？
作为一门高级的脚本语言，JavaScript的语言内建的功能就有不少功能一般是需要运行时库的支持的：

- 动态内存管理：new以及其它动态创建对象的语法（对象字面量、数组字面量、正则表达式字面量）、无限制的闭包，以及对应的自动回收无用内存的功能
- 内建类型涉及的标准库函数实现：Object、Number、String、Boolean、Function、Date、RegExp等内建类型，以及它们的相关函数 等
- 反射：typeof，with，类型间的转换 等
- … 最后的大魔王：eval （以及Function构造函数或其它能把字符串当作程序代码来执行的功能）

就算把一个JavaScript程序事先彻底编译到目标代码，如果要支持eval的话，最终还是得把一个解释器或JIT编译器带在运行时库里，因为程序运行过程中还可能动态加载新的代码，而这未知的输入是无法事先编译为目标代码的。

那要是不用eval（以及其它能把字符串当作程序代码来执行的功能）呢？那就消除了动态加载（未被编译的）JavaScript代码的可能性。
于是就有可能事先把JavaScript程序编译到目标机器码，而与其配套的运行时里不需要带上解释器或者JIT编译器这样的执行引擎。反射、GC、标准库之类的功能还是可以由运行时库来提供。

那如果不用反射、
不用复杂的内建类型、
不用动态内存分配…

剥离到最后的“裸”的JavaScript的子集是怎样的呢？
——看看[asm.js](https://link.zhihu.com/?target=http%3A//asmjs.org/)就知道了。用它是可以写出基本脱离运行时库的程序的，只要实现合适的AOT编译器。
但它自身的功能很受限，如果不跟宿主环境注入的函数搭配使用的话，自身是做不了什么事的；而宿主环境注入了函数给asm.js用的话，对asm.js来说这个宿主环境就成为“运行时库”的一部分了。

还是回到前面提到过的：**关键还是看某个具体程序使用了多复杂的功能，越复杂的功能就越需要带上一个复杂的运行时库。**

========================================

本回答开头提到的JavaScript引擎的实现中，最特别的一个是当时是在读博士生的Maxime Chevalier-Boisvert所实现的Tachyon VM。

源码：[GitHub - Tachyon-Team/Tachyon](https://link.zhihu.com/?target=https%3A//github.com/Tachyon-Team/Tachyon)
可自举版的源码：[Tachyon/source at dls2011 · Tachyon-Team/Tachyon · GitHub](https://link.zhihu.com/?target=https%3A//github.com/Tachyon-Team/Tachyon/tree/dls2011/source)
论文：[Bootstrapping a Self-Hosted Research Virtual Machine for JavaScript](https://link.zhihu.com/?target=http%3A//www.iro.umontreal.ca/~dufour/pubs/dls2011.pdf)，发表于DLS 2011
演讲稿：[Tachyon: a Meta-circular Optimizing JavaScript Virtual Machine](https://link.zhihu.com/?target=http%3A//www.sable.mcgill.ca/~clump/cdp2010/ChevalierCDP10.pdf)

以前我在另一个回答里提到过这个JavaScript引擎：
[用 JavaScript 写成的 JavaScript 解释器，意义是什么？ - RednaxelaFX 的回答](https://www.zhihu.com/question/20004379/answer/18600484)

Tachyon最好玩的地方在于：它自身是用JavaScript实现的，并且它可以独立运行（最终不需要运行在Tachyon之外的别的JavaScript引擎上）。

它是怎么做到的？用JavaScript实现的parser有许多现成的了，其中不乏像[Esprima](https://link.zhihu.com/?target=http%3A//esprima.org/)这样高质量又流行的实现。用JavaScript实现的JavaScript解释器也有不少了。但它们都得在一个现成的JavaScript引擎上运行啊。
要做一个能独立于别的JavaScript引擎而运行的实现，还得实现对象模型和GC，这些都需要指针操作，而JavaScript语言自身并不提供指针运算以及裸内存访问的功能，咋破？

**秘方的思路就是“自举”（bootstrap），而实现机制就是“编译”。**

相当多编程语言的编译器都是用自己实现的，许多C和C++编译器都是如此。例如说老的GCC自身是用C语言实现的，新的GCC、Clang自身是用C++实现的。它们要如何“编译自己”，也是要经过一个自举的过程。
但对许多不熟悉编译原理的同学来说，JavaScript（以及Java、C#、Python等语言）也能做到这点应该还是件新奇事吧。

Tachyon作为一个JavaScript引擎，有对象模型、内建类型的库函数、GC（在DLS 2011时尚未完成但思路是明确的，后来实现了一个copying GC）等等常见的组件，而更重要的是，它的执行引擎部分是通过编译器的形式来实现的。这个编译器既可以当作AOT编译器用，也可以当作JIT编译器用。

引用Tachyon论文里的一张图来讲解它的bootstrap过程：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glsvogfuryj30i20cc752.jpg)

这张图的表述方式是所谓“T形图”：[T-diagram](https://link.zhihu.com/?target=https%3A//en.wikipedia.org/wiki/Tombstone_diagram)。每个小T形的三个顶点分别表示：

- 左边：编译器的输入语言
- 右边：编译器的输出语言
- 下面：实现编译器所用的语言

具体到Tachyon，它包含一个用JavaScript实现的、能把JavaScript源码编译到32位x86机器码的编译器。所以用T形图表示就是：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glsvoowxlsj306l04l747.jpg)

那么如果把Tachyon自身（大图左边的T形）作为Tachyon（大图下面的T形）的输入，把自己编译成x86机器码，得到的就是能直接在x86上运行的、把JavaScript源码编译为x86机器码的Tachyon（大图右边的T形）。这就是Tachyon的bootstrap过程。
注意到大图下面的T形的下面还有一个小长方形，这是在boostrap过程中用于运行Tachyon的宿主JavaScript引擎。具体来说这里用的是V8的shell。



有了自己实现的、能把JavaScript源码编译到x86机器码的编译器之后，事情就变得好玩了。要实现高性能的值表现形式、对象模型、GC等功能，必须要用到指针运算和裸内存访问。
JavaScript自身没有提供这样的功能，但现在Tachyon有自己的编译器了，就可以在编译器里做些扩展，给Tachyon所支持的的JavaScript添加私有扩展，仅用于实现Tachyon自身（而不将这些扩展暴露给上层JavaScript应用代码）。

Tachyon具体是怎么做的呢？它用的机制叫做Inlined IR，简称“IIR”。
顾名思义，IIR就是可以在普通JavaScript语法的源码里，混入指定的编译器IR来表达要执行的操作，就跟内联汇编的意图和作用类似。
例如说，要实现ES5的Object.getPrototypeOf函数，Tachyon的实现是：

```js
/**
15.2.3.2 Get the prototype of an object
*/
Object.getPrototypeOf = function (obj)
{
    assert (
        boxIsObjExt(obj),
        'non-object value in getPrototypeOf'
    );

    var proto = get_obj_proto(obj);

    return proto;
};
```

这看起来还是个普通的JavaScript函数，但里面有些奇怪的函数调用，boxIsObjExt()、get_obj_proto()。

让我们看看get_obj_proto是如何实现的：

```js
function get_obj_proto(obj)
{
  "tachyon:arg obj box";
  "tachyon:inline";
  "tachyon:noglobal";
  "tachyon:ret box";

  var offset = pint(4);
  return iir.load(IRType.box, obj, offset); 
}
```

这看起来就不那么普通了。首先，函数开头有若干Tachyon特有的directive字符串，用于在维持JavaScript基本语法的前提下，给Tachyon的编译器部分提供额外的静态声明信息，例如这里：

- tachyon:arg obj box：声明了参数列表有一个参数，名为obj，类型为box（装箱的指针类型）
- tachyon:inline：这个函数的实现应该被内联到caller一侧
- tachyon:noglobal：这个函数不会访问任何全局变量
- tachyon:ret box：返回类型为box

接下来pint(4)告诉Tachyon编译器这里要一个值为4的platform int类型的值（等同C的intptr_t）。

最后终于来到最有趣的iir.load()调用。这个调用实际的意思是：这并不是一个真的函数调用，而是一条内联IR（IIR）；这条IR的语义是：以obj指针为基地址、offset为偏移量，加载（load）一个类型为box的值出来。
要是用伪C代码来表示这个语义，那就是：

```c
/* char* obj */
*((box*) (obj + offset))
```

也就是base+offset形式的指针解引用。

上面的get_obj_proto()函数，对应最终生成的机器码，会是类似这样的：（这里按Tachyon论文用AT&T语法）

```text
movl 4(%reg_src), %reg_dst
```

就是一条简单的mov指令，实现了指针运算和裸内存访问。

通过这种IIR机制，Tachyon就可以使用普通的JavaScript语法来表达扩展的语义，从而可以实现对象模型、GC等底层功能对指针运算、裸内存访问等功能的需求。

有了这扩展语义的JavaScript，那是不是Tachyon整体都需要用到这样的扩展语义呢？并不是。
还是回到前面提到的bootstrap过程：在bootstrap过程中，

- Tachyon的编译器部分需要由V8执行，所以这个编译器无法使用任何扩展的JavaScript语义——因为V8并不理解这些扩展——而它也不需要用。
- Tachyon的运行时部分的大部分高层功能其实也不需要用到扩展的语义。
- Tachyon的运行时的底层功能，如对象模型和GC，在boostrap中并不需要运行，只需要被Tachyon自己编译成机器码。它们才真正需要使用到扩展的语义。

简单图示一下：

```c
 [ compiler ] [ stdlib ] [ ... ]           standard ES5
-----------------------------------------
 [ object model ] [ garbage collector ]   extended ES5 with IIR
```



值得一提的是，Tachyon在bootstrap结束后并不只是把自己从JavaScript源码编译到了x86机器码，还生成了运行自己所需要的初始对象（例如Object、Number等内建类型对应的构造函数对象、prototype对象等等）。
这些初始对象作为静态数据与生成的机器码一起打包成ELF格式，构成最终的可执行文件。

是不是很有趣的实现？

（注：Tachyon能自举的版本在前面提到了，是开DLS 2011会议时发布的版本。
后来作者的注意方向转到别处去了，暂时放下了Tachyon的自举功能，而专注开发别的部分去了。
后来出于她自己的兴趣与导师指导方向的不一致，她停止了Tachyon的开发，而另起炉灶用D语言写了一个新的JavaScript引擎，[Higgs](https://link.zhihu.com/?target=https%3A//github.com/higgsjs/Higgs)，用于研究对JavaScript的优化编译。
这段经历使得Tachyon在被停止开发前还不算很完善。但它已经实现的功能足以演示用JavaScript实现可自举的JavaScript引擎的可行性，以及一种实际实现的思路。）

========================================

回到题主最初的问题：

> JavaScript的功能是不是都是靠C或者C++这种编译语言提供的？

虽然并不都是靠C或C++来实现的，但如果想实现一个不需要别的宿主环境（例如别的JavaScript引擎、JVM之类）而可以独立运行的JavaScript引擎，拥有一个对应的编译器是必要的。
用C或C++来实现的话，这“对应的编译器”就是普通的C或C++编译器；
而如果用JavaScript自己来实现自己的话，这“对应的编译器”就得自己来实现，就像Tachyon那样。