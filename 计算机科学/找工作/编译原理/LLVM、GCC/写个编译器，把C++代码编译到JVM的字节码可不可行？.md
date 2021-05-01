在知乎或者很多其它问答网站（爆栈站也不例外）问这种问题被喷的可能性极其大，因为：

1. 实际有这种使用场景的人是极小众；
2. 这种做法真的也只有很窄的应用场景…而且也没办法跟原生C++的性能比；
3. 很多人根本没仔细想清楚也没有足够知识储备就开喷了。

别怕被喷。至少在知乎问题自身还不能被投反对，大不了被别人投关闭了而已（逃
上面的(1)和(2)是毫无争议的事实。当然有实际性能实验的数据更好，这里就偷懒只定性说了。题主要是有爱的话可以自己试试用下面提到的一些方案来测试一下。

题主问把C++源码编译为Java字节码之后在JVM上运行是否可行。单纯说技术上是否可以实现的话，当然是可以实现的——至少到某种颇为可用的程度上可以实现。
Java字节码是图灵完备的，可以表达用图灵机可计算的运算。在这个意义上说它肯定是可以表达所有C++可以表达的运算的，只是不一定能直接表达——但是肯定可以模拟出来。
（总会有人拿偏门的功能，例如说并不在C或C++标准里的内联汇编之类的功能为例来说肯定不能被编译到Java字节码。那种评论可以先不管。）

而真正让人困惑的是把C++程序放在JVM上运行的动机——把C++写的程序放JVM上有啥好处，不然 why are you 弄啥嘞？

===============================================

**现成方案**

这种脑洞显然不是题主最先开的。早就有前人们实践过了，挑几个稍微新一点的例子来说：

- [NestedVM](https://link.zhihu.com/?target=http%3A//nestedvm.ibex.org/)：通过GCC把C、C++、Fortran等语言编译到Java字节码。它是基于GCC的MIPS后端，在它的基础上改造出Java字节码后端的。项目已经多年没有更新，最后一个版本是2009年发布的、基于GCC 3.3.6的。
- [Cibyl](https://link.zhihu.com/?target=https%3A//code.google.com/archive/p/cibyl/wikis/Cibyl.wiki)：同样基于改造GCC的MIPS后端来把C语言编译到Java字节码。跟NestedVM有不少相似之处。
- [LLJVM](https://link.zhihu.com/?target=https%3A//github.com/davidar/lljvm)：一个LLVM的Java字节码后端以及配套的运行时库，可以支持诸如C语言在JVM上的运行。
- [Proteus Compile System](https://link.zhihu.com/?target=http%3A//proteuscc.sourceforge.net/)：一个LLVM的Java字节码后端及运行时库，可以支持C/C++。
- [Renjin.org | Introducing GCC-Bridge: A C/Fortran compiler targeting the JVM](https://link.zhihu.com/?target=http%3A//www.renjin.org/blog/2016-01-31-introducing-gcc-bridge.html)

上面的例子都是直接编译到Java字节码的。那些可以支持C但是没有说支持C++的环境，再不济也可以进一步用类似CFront的方式把C++给lower到C之后再进一步编译下去。

再举一组例子是虽然也在JVM上运行，但并不直接编译到Java字节码，而是编译到某种AST之后通过partial evaluation来运行时编译到机器码的。它们都是基于Truffle / Graal的实现：

- [TruffleC](https://link.zhihu.com/?target=http%3A//dl.acm.org/citation.cfm%3Fid%3D2647528)：在Truffle框架上实现的C语言运行环境。它有一个应用场景是给RubyTruffle加速C extension的性能：[Very High Performance C Extensions For JRuby+Truffle](https://link.zhihu.com/?target=http%3A//chrisseaton.com/rubytruffle/cext/)
- [Sulong](https://link.zhihu.com/?target=https%3A//github.com/graalvm/sulong)：在Truffle框架实现的LLVM IR运行环境。通过Sulong，可以运行基于LLVM实现的C、C++、Fortran之类的各种语言。



上面都是一些非常实在的、至少当项目还在活跃期的时候相当可用的例子。
而接下来我要帮题主进一步扩展脑洞，想想其它“好玩的”情况。

跟JVM相似的高级语言虚拟机，还有Flash VM（AVM2 / Tamarin）和各家的现代JavaScript引擎。

那么能不能在Flash VM上运行C、C++程序呢？可以的，通过[Adobe Alchemy](https://link.zhihu.com/?target=http%3A//labs.adobe.com/technologies/alchemy/)（项目现在叫做FlasCC）。该项目有开源版叫做CrossBridge：[Cross compile your C/C++ games to run in Flash Player](https://link.zhihu.com/?target=http%3A//adobe-flash.github.io/crossbridge/)
于是，把Alchemy的思路移植到JVM上，这个绝对是可行的。
但还有更现成的开脑洞的办法：把Alchemy编译出来的Flash程序，直接放在JVM上跑。例如说通过[Mozilla Shumway](https://link.zhihu.com/?target=http%3A//mozilla.github.io/shumway/)来直接运行SWF文件。请跳个传送门：[把Flash游戏发布到Web上 探讨下可行路径？ - RednaxelaFX 的回答](http://www.zhihu.com/question/37897644/answer/74062746)

那么能不能在JavaScript引擎上运行C、C++程序呢？可以的，有若干把C、C++编译到JavaScript或者Web Assembly的途径，其中最出名的一个是[Emscripten](https://link.zhihu.com/?target=https%3A//github.com/kripken/emscripten)，而另一个有趣的新晋选择是[Cheerp](https://link.zhihu.com/?target=http%3A//www.leaningtech.com/cheerp/)。大家或许都见过用Emscripten编译的DOOM了，可行性是杠杠的。
于是，把它们的思路移植到JVM上，这个绝对也是可行的。
但还有更现成的开脑洞的办法：把Emscripten或Cheerp编译出来的JavaScript程序，直接放在JVM上跑。例如说通过Oracle JDK 8 / OpenJDK 8自带的[Nashorn](https://link.zhihu.com/?target=http%3A//openjdk.java.net/projects/nashorn/) JavaScript引擎来运行，又或者是用更老的[Mozilla Rhino](https://link.zhihu.com/?target=https%3A//developer.mozilla.org/en-US/docs/Mozilla/Projects/Rhino)来运行。

最后再举一组极度脑洞大开的例子。

有一个用纯Java写的x86 PC模拟器，[JPC](https://link.zhihu.com/?target=https%3A//github.com/ianopolous/JPC)。它可以启动并运行诸如Windows 95和某些Linux版本。显然无论是C还是C++写的程序，只要能用常规的编译器编译到在JPC支持的OS上运行的话，它就可以在JPC上运行…
同一系列更脑洞的：Fabrice Bellard大大用纯JavaScript写过一个x86 PC模拟器——[Javascript PC Emulator](https://link.zhihu.com/?target=http%3A//bellard.org/jslinux/tech.html)，可以启动并运行一个定制的Linux。在demo带的Linux镜像里还带有Fabrice大大写的TCC编译器。这个模拟器也可以在JVM上的JavaScript引擎上跑，所以…

===============================================

**插曲：C++/CLI 与 .NET / CLR？**

在微软的.NET平台上，我们也可以看到C++的身影。不过在CLR上可以运行的不是原生C++，而是适配到.NET上的变种：C++/CLI（嗯还有其前身的Managed C++，不是MC++已经黑历史了，就不讨论了）。

CLR所实现的虚拟指令集，CLI VES的指令集——Common Intermediate Langauge（CIL，也叫MSIL），在设计之初就考虑到要兼容unmanaged code的执行，所以字节码指令集中包含了一个专门用于支持unmanaged code功能的子集，例如说裸内存访问 / 指针操作，通过裸函数指针的间接函数调用，等等。C++/CLI则充分利用了这一子集而得以直接高效地运行在CLR上，让程序员可以用很自然的C++语法写出managed与unmanaged混搭的程序。

但当然C++/CLI也为了迁就CLR而做了一些功能上的限制，例如说ref class的实例不能够stack allocate或者是以值的形式声明为别的类的成员；又例如说ref class不能用bitfield。如果一定要直接使用原生的C++库的话，偶尔会遇上些边角问题。
另外，在只允许verified code的配置下，使用了unsafe功能的C++/CLI程序是不能运行的。这也算是个限制吧。

虽说JVM与.NET的CLR是有不少相似之处，在许多方面它们都是在一个级别上的东西，但是在对C++的原生支持上，.NET CLR显然是远远走在JVM之前的。

放俩传送门：

- [.NET CLR怎么保证执行正确的unsafe代码不挂掉? - RednaxelaFX 的回答](https://www.zhihu.com/question/49030875/answer/113939438)
- [C#能否被编译成Java字节码？ - RednaxelaFX 的回答](https://www.zhihu.com/question/41102220/answer/89609982)



===============================================

**有啥好处？**

然而把C或C++写的程序放在JVM上运行的好处有啥嘞？

在上面列举的现成方案中，对我来说最有说服力的是TruffleC + RubyTruffle的例子。
RubyTruffle是一个非常高性能的Ruby实现（嗯，除去启动开销外…启动开销在Substrate VM上的版本会好很多），但如果它不能完美支持CRuby的众多C扩展的话那对社区来说还是不够有说服力。而TruffleC则大开脑洞解决了这个问题：有C语言的源码的C扩展，可以通过TruffleC来跟RubyTruffle跑在一起，而且通过对CRuby的C扩展API做特化实现，TruffleC + RubyTruffle可以使得该系统上运行的Ruby代码在JIT编译的时候可以穿透C扩展API的边界一直内联到C扩展的一侧，消除C扩展边界上的开销，达到远高于原生CRuby的高性能。

而Cibyl的作者创建这个项目的目的也很明确：他希望能把一些以前用C写的游戏啊啥的移植到支持J2ME的平台上。这种有明确目的的项目，就会为了这具体的目的来做对应的实现，有的放矢。这样的结果即便性能比不上原生的高，但它可以在只允许客户自己部署J2ME程序的环境上使用丰富的C程序/库。

上面提到的Alchemy、Emscripten的初衷也很相似：一个带有沙箱机制的运行时环境，希望能够更充分利用现成的各种程序/库，包括用C、C++写的库。特别是在移植老游戏的场景很流行。但这种思路要用在Java SE上，从实用角度上说感觉说服力不足。
Flash与JavaScript都是很流行的客户端平台。Flash虽然现在比较没落了，但就在几年前它都还很辉煌。在一些安全性要求高的地方可能不允许Flash程序使用native扩展，又或者一个Flash程序想尽可能简单地跨平台可运行，这些条件下在Flash程序里要想用C或C++写的库，FlasCC就是一个很现实的选择。而JavaScript在浏览器里的话则没有标准的native扩展接口，想用native库最现实的做法就是编译到JavaScript来运行。
那么Java SE呢？Java SE也曾经在浏览器里辉煌过一小段时间——以Java Applet的形式嵌入到网页里，给网页提供动态交互功能。但这市场很快就被后来居上的Flash给完全吞噬了。而一个不以Applet形式运行的Java程序，其实想通过标准的Java Native Interface去使用现成的native库也不是什么难事，并没有足够动力一定要把native库自身给编译成Java字节码跑在JVM里面。

说到要把native程序跑在沙箱里…不同层面的解决方案实在多如牛毛啊。
有开系统虚拟机的；
有开系统容器隔离的；
有在上述两者之间的；
有在应用层面构建沙箱的；
…
其中[NaCl / PNaCl](https://link.zhihu.com/?target=https%3A//developer.chrome.com/native-client/nacl-and-pnacl)就是一种在应用层面构建沙箱，以接近原生应用性能运行的解决方案。如果是为了Java带的沙箱而把C++写的程序编译到JVM上运行，大可不必啊。

题主或者其他同好有想到什么好的动机来支持把C++编译到Java字节码的需求的话，我洗耳恭听 ^_^

===============================================

**关于实现的一点讨论**

就挑几个小点来讨论一下在JVM上跑C++程序会涉及的问题或者“非问题”。

**1. 内存怎么办？**

最常见、直观的解决办法就是暴力解法：用一个大byte数组来模拟“native memory”。
其中，最简单的做法就是用一个Java层面的byte[]来充当“native memory”来给上面跑的C / C++程序用。这样C / C++的指针则表现为对这个数组的下标（index）。GC要是移动了这个数组怎么办？没关系啊，“指针”只是下标而不是裸的地址，不受GC影响。Alchemy在Flash上实现C/C++程序的native heap就是这么实现的。
而不想把这个“native memory”放在JVM的GC堆里的话，也可以用DirectByteBuffer系API来在真正的native memory里申请一大块内存来模拟JVM上运行的C / C++程序的native memory。这样C / C++的指针就表现为对这个DirectByteBuffer的offset，其实跟数组下标也没啥两样。

有一大块byte数组，想怎么安排里面的数据的内存布局那都是随便搞。没啥模拟不了的。
有两点需要注意的是：
一来，在实现的时候要注意多字节数据访问的原子性。JVM只认自己看到的Java层面的类型，如果它看到的是一个byte[]对象它就会按照byte的规则来处理其中的元素访问的原子性。如果有程序在byte[]上面模拟别的宽度的数据，例如用4个byte模拟一个int，那要需要这个访问是原子的则需要自己想办法。
二来，这种通过Java层面的byte数组模拟出来的“native memory”跟在JVM外真正自由的native memory不能简单地互操作。这是下面一点要提到的了。

**2. C/C++程序之间的互操作？**

这种在JVM上运行的C/C++程序，要跟同一个JVM通过JNI访问的C/C++库交互的话，是…比较麻烦的。基本上两者虽然都是C/C++写的，但却像是运行在两个世界里一样。
就像同样是C++写的程序，编译给不同OS上的、指针宽度不同的二进制程序之间不能直接互操作；就算是同一个OS上同样的指针宽度，遵循不同ABI的编译器编译出来的结果也不能互操作。在JVM上运行的C/C++程序无法直接跟同一JVM通过JNI访问的C/C++库互操作也是一样的道理。

我以前试过好几次想用Java写一个非常简单的教学用JVM，不要自举，只要做一个在宿主JVM上跑的解释器就行。其中一个很纠结的地方就是决定要不要自己实现内存访问 / GC / 对象布局 / ClassLoader之类的功能。这些东西一旦自己实现了，就跟在JVM上模拟“native memory”一样，要跟外面通过JNI实现的功能交互就变得麻烦了。想实现个Hello World还得跟System.out.println()打交道呢，而这个println()的底下是什么？很可能是一个用C写的函数：（[jdk8u/jdk8u/jdk: 141beb4d854d src/solaris/native/java/io/io_util_md.c](https://link.zhihu.com/?target=http%3A//hg.openjdk.java.net/jdk8u/jdk8u/jdk/file/141beb4d854d/src/solaris/native/java/io/io_util_md.c%23l160)），然而我的教学用JVM上的Java程序要写个Hello World我都得费事去做适配，实在麻烦。

**3. 基于（操作数）栈的字节码的表达能力？**

关于在JVM上实现C/C++写的程序的运行，我见过最无厘头的观点是：Java字节码是基于栈的，所以不适于实现C/C++。然而这根本不是问题。

基于（操作数）栈的虚拟指令集，与基于（虚拟）寄存器的虚拟指令集，其实主要差别在于表达式的临时值的访问方式不同。仅此而已。
对这点感兴趣的同学请跳传送门：[寄存器分配问题？ - RednaxelaFX 的回答](https://www.zhihu.com/question/29355187/answer/51935409)

大家用过或者见过IBM XL C/C++编译器，又或者是HP的aCC编译器不？它们俩在编译器前端到优化器、优化器到后端之间传递程序用的IR——WCode与UCode2，无独有偶，都是基于（操作数）栈的。在对操作数栈的使用上，两者跟Java字节码都颇为相似。
这在新一代编译器里当然算不上是流行的IR设计了，但这种设计在现在还活着的成熟产品里还能看到，也算是能反映出这是个能实现功能的设计了吧。

===============================================