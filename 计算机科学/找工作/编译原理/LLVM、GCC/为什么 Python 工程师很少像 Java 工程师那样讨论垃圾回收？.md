为什么 Python 工程师很少像 Java 工程师那样讨论垃圾回收？Java 开发的时候经常讨论垃圾回收策略，什么并行串行 G1 垃圾回收器之类的；而做 Python 开发的这几年，好像鲜有人讨论这些；两者都是自动回收内存，为何在内存回收这块会有这个差别？

---

呵这个问题挺有趣的。
“用的人少所以讨论也少”固然是因素之一，“写C++的人才讨论Java垃圾回收”的情况也不能说不存在…但我觉得还有若干别的因素更重要。

首先自然是：其实也有不少人讨论Python的内存管理或者说GC机制，不过题主之前未必留意了，自然就觉得没人讨论。当然，相对于Java开发者而言，讨论Python的内存管理/GC的开发者确实是少得多。下面说说让我感兴趣的几个因素。

另外，想学习了解Python的自动内存管理机制的同学，可以参考这两本书：

- [ガベージコレクションのアルゴリズムと実装](https://link.zhihu.com/?target=http%3A//book.douban.com/subject/4881935/)
- [Python源码剖析](https://link.zhihu.com/?target=http%3A//book.douban.com/subject/3117898/)

其中第一本既介绍了各种GC算法的基础知识，又挑了一些实际语言中GC的实现，包括CPython的。它的中文版即将上市了。详情请参考：[如何评价《垃圾回收的算法与实现》及其作者中村成洋？ - RednaxelaFX 的回答](https://www.zhihu.com/question/39337535/answer/80884071)
而第二本现在可能断货了，不过有第二版正在写作种，可以期待一下～

**1. 是否有选择，有怎样的选择？**

Java的默认实现，Oracle/Sun JDK里的HotSpot VM，向用户提供了相当丰富的GC选项，既可以在很粗的粒度上配置（例如选择GC算法搭配），也可以在很细的粒度上配置（例如配置各收集器的具体启发条件（heuristics））。而且在该JDK的核心部分作为OpenJDK开源出来之后，大家都可以跳进去把各种本来不是暴露给用户用的参数拿来（乱）配置…
一方面，这让用户可以针对自己的应用情况做有针对性的GC配置，特别是可以让对GC有深入了解的人可以做非常精确的配置，这有利于Java应用与HotSpot VM的GC之间达到更好的配合。
然而另一方面，这么多选项容易让用户感到：(a) 困惑 (b) 厌烦 (c) 乱兴奋。

HotSpot VM的主要竞争对手们，IBM J9和Oracle JRockit则采用稍微不同的做法：它们同样提供丰富的配置选项，但通常只需要用户表达清楚“意图”而无需关心底下的细节，例如J9的 -Xgcpolicy:{optthruput,optavgpause,gencon,balanced,subpool}，JRockit的 -XgcPrio:{throughput,pausetime,deterministic}，只需要把Java应用的场景表达出来，JVM就会自动选择对应的各种细节配置，这就好多了。
而像[Azul Systems](https://link.zhihu.com/?target=https%3A//www.azul.com/)的[Zing JVM](https://link.zhihu.com/?target=https%3A//www.azul.com/products/zing/)则是更进一步，只提供一种GC实现——[C4 GC](https://link.zhihu.com/?target=https%3A//www.azul.com/resources/azul-technology/azul-c4-garbage-collector/)。而这个GC实现自身就非常有弹性，可以应对各种不同的使用场景，不需要用户做多少配置就能工作得很好，用户需要担心的事情就更少了。

我也见过一些有趣的JVM实现，可以在运行时自动根据情况切换GC算法，这样就完全不需要用户去配置啥了。但这样实现起来会很麻烦，而且实际效果未必有字面上看起来那么神奇，所以主流JVM都不这么做。

显然，HotSpot VM在这方面有待改善。它的核心开发组早就意识到了这点，已经有考虑在未来版本里提供更智能/方便的配置选项，类似J9 / JRockit的那种，以方便用户减少配置GC时需要关心的细节。不过目前它就是这样，被人吐槽也是应该的。

Python方面，其默认实现CPython提供了什么GC配置选项呢？
——基本上什么也没提供。唯一能靠谱的配置的就是[禁用 (cycle-) GC](https://link.zhihu.com/?target=https%3A//docs.python.org/2/library/gc.html%23gc.disable)和[(cycle-) GC触发时机](https://link.zhihu.com/?target=https%3A//docs.python.org/2/library/gc.html%23gc.set_threshold)。
既然无可配置，这不在程序员控制范围之内的事情还担心来干嘛呢？

PyPy倒是提供了一些GC配置选项，但它还远没成为主流Python选择，大家对它的正确配置方式关注的也就没那么多。Pyston和MicroPython…回头有机会再写。
至于IronPython、Jython之类在现成的managed runtime上实现的Python，那又回到它们底层平台的GC调优问题了。

**2. 是否知道问题有多严重？**

主流JVM实现们都提供了打印GC日志的功能，而且可以配置日志的精细程序（例如 -verbose:gc vs. -XX:+PrintGCDetails ）；还有许多工具可以分析GC日志，将其可视化，有些还能提供调优建议。这就让用户可以很方便的获取到足够的数据来发现、了解和解决问题。
当然，要如何正确理解这些信息也是门技术活。如果看到了一大堆数据但不够了解其背后的意义，那就又容易陷入上面所说的 (a) 困惑 (b) 厌烦 (c) 乱兴奋 的情况…

这“乱兴奋”是啥状况呢？Java应用其实可能遇到许多不同种类的性能问题，有些可能是Java代码自身没组织好，有些可能需要更好的JIT编译器优化，有些可能是GC问题。前两者也可能是很重要的问题，但JVM自身并没有提供好办法让大家去了解状况，而GC日志却是大家都能轻易获取的。
于是很多Java程序员或许过多的、不必要的关注GC问题了。有些Java应用对响应性的要求根本没到GC需要调优的程度，它的开发们却可能在忙着要想办法把GC暂停时间降下来了…

Python的默认实现CPython，只能配置打印cycle GC的收集状况，而无法打印引用计数带来的开销。
可能很多人会觉得引用计数有啥开销了，反正每次引用计数的更新开销都很小而且还均摊在整个程序的运行过程中，根本无需担心。

其实做过编程语言实现的人都多少会知道：朴素的引用计数，除非在剩余内存非常紧迫的条件下，一般来说吞吐量性能（throughput）是低于朴素的tracing GC的。

原理也很简单：在剩余内存充裕的情况下，tracing GC的两次trace之间的间隔可以比较长。而它并不关心两次trace之间对象图到底发生了怎样的变化，只关心每次trace时发现的对象图的引用状况。所以如果在一次trace之后，有一个很大的对象图被创建了出来，然后在下一次trace之前它就死掉了的话，其实tracing GC是根本就不会发现这个对象图曾经存在过，自然也就可以不付出任何代价就回收其空间（例如不需要单独sweep步骤的copying GC）。
而朴素的引用计数需要跟踪所有引用关系的变化——每次增加引用和减少引用都要反映到引用计数里。同样是上面那种很大的对象图的情况，维持其中的引用计数的开销可以任意大（这些对象死前，相互的引用关系可以发生任意多次的改变）。
在零开销与没有bounds的开销之间，差别还是很明显的。

在剩余内存紧张的情况下，上述优劣对比就反转了：由于剩余内存不足，tracing GC会被迫经常触发收集，每次trace都要遍历整个对象图，开销相当大；而引用计数丝毫不受剩余内存量的影响，该怎么搞还是怎么搞。

然而没有数据说个啥呢。

[Hans Boehm](https://link.zhihu.com/?target=http%3A//www.hboehm.info/)在2003年专门发了篇论文提醒大家引用计数（朴素和延迟）都有值得关注的开销：[The Space Cost of Lazy Reference Counting](https://link.zhihu.com/?target=http%3A//www.hpl.hp.com/techreports/2003/HPL-2003-215.pdf)。有趣的是，其中有个例子展示了朴素引用计数（借助shared_ptr）比tracing GC的max pause time更高的情况。

有经验的C++程序员都会知道：

- 当对象的所有权（ownership）不重要时，用裸指针；
- 当对象的所有权可以唯一确定时，用unique_ptr。能用unique_ptr绝对不要用shared_ptr；
- 要处理所有权复杂的情况时，可以用shared_ptr但不要滥用；当引用关系不影响所有权时，用weak_ptr。

C++程序员大都听说过不要滥用shared_ptr，然而CPython的现实就像是一个彻底滥用了shared_ptr的C++程序一样，连 PyIntObject / PyLongObject 也是引用计数的还有啥好说呢。

CPython采用的引用计数是最朴素的实现方式：局部变量、全局变量和对象字段都参与到引用计数中，而且引用计数的更新是在锁下同步的；外加朴素的mark-sweep备份来处理循环引用。
然而在朴素引用计数的基础上有许多改进的方案。其实只要能让局部变量不参与到引用计数中，程序的吞吐量性能（throughput）就可以有不少提升——这种做法叫做“延迟引用计数”（deferred reference counting，DRC）。这种做法最初在这篇论文提出：[An Efficient, Incremental Garbage Collector](https://link.zhihu.com/?target=https%3A//www.cs.purdue.edu/homes/hosking/690M/deutsch.pdf)，发表于1976年。近年来还有些进一步优化引用计数实现的办法，例如这两篇论文所总结/创新的：[论文1](https://link.zhihu.com/?target=https%3A//users.cecs.anu.edu.au/~steveb/downloads/pdf/rc-ismm-2012.pdf)/[论文2](https://link.zhihu.com/?target=http%3A//research.microsoft.com/pubs/202163/rcix-oopsla-2013.pdf)

CPython的实现中，GIL难以有效的去除的原因之一就是为了迁就引用计数。当一个Python线程在运行时，它会获取GIL以保证它对对象引用计数的更新是全局同步的。由于引用计数的实现细节被CPython的C API暴露到了外部的C扩展，要保持这些扩展完全兼容，就得维持或模拟CPython引用计数的实现，这就麻烦了…

大家听说过对CPython的GIL的抱怨不？经常听到对不对？
有多少*一般*Python用户知道吐槽GIL其实真的在吐槽的就是CPython的引用计数及C API实现？——不知道也没关系，说明还没遇到问题。
想稍微多了解些的话，请参考Python官方wiki：[GlobalInterpreterLock](https://link.zhihu.com/?target=https%3A//wiki.python.org/moin/GlobalInterpreterLock)

说了半天，回归主题：CPython在帮Python程序自动管理内存时，引用计数到底带来了多少开销，大家心里有数么？反正CPython自身不提供相关任何日志，想弄清楚还挺麻烦的。
CPython那备份的mark-sweep GC也很朴素，一不小心也会带来问题…不知道大家关注过么？这个倒是可以打印日志的。

**3. 核心VM里其它组件的性能是否已经足够好，使内存管理的性能成为瓶颈？**

在Java世界里有个很有趣的故事，放在这个上下文里正合适。

Sun在发布JDK 1.0后，Sun Labs的Java Topics组开始研究GC在JVM里的应用的各种方面，以期为不同的场景实现高性能GC。
然后没过多久他们就发现了：当时他们基于Sun的元祖JVM来做研究，而那个JVM除GC外的核心组件（例如解释器、同步（锁）的实现）都实在太慢，GC都还不能凸显为瓶颈，根本没办法有效的做GC研究。
所以他们跟Sun Labs的Kanban Group一起先去研究了实现先进的优化JIT编译器，以及高效的锁实现，把底子给打好了然后才回过头去做GC研究。这些研究的产物就是在Classic VM上魔改而来的Exact VM，其成果包括后来移植到HotSpot VM上的GC接口、并行GC（ParNew）和并发GC（CMS）等。

我在这里写过一些Exact VM的JIT编译器的故事：[JIT编译器杂谈#1：JIT编译器的血缘（一） - 编程语言与高级语言虚拟机杂谈（仮） - 知乎专栏](http://zhuanlan.zhihu.com/hllvm/19954031)
当时Exact VM可是什么都用上了，为了快速开发出优化JIT编译器而把Sun的C/C++/Fortran编译器后端都用在了JVM里…

那么回过头来看CPython。现在的CPython解释器的性能恐怕还没有最初Sun发布的JDK 1.0里的JVM的解释器性能好，或者最多是差不多好。两者都是用C写的、很直观的字节码解释器；Java的字节码特意设计得尽可能单态以减少字节码中的动态分派开销，而Python的字节码却有大量的多态操作，所以在同等实现方法的解释器上，Sun Classic VM容易比CPython的解释器有更高的性能。
在这样的环境中，一个低效的自动内存管理器确实未必需要引起足够重视…

**4. 程序规模或程序组织方式是否容易引起GC问题？**

这个展开写起来有点麻烦…先偷个懒，抛出问题，回头有空再更新（逃

大家写Python脚本时，很多时候只是在比较少的数据量上跑很短时间，那引用计数或GC就算再怎么开销大其实也无所谓。
假如说CPython能改用DRC，免费提升大家的Python程序10%的吞吐量，这好不？想想，一个脚本本来要跑100秒，现在只要跑90秒了…好像也没啥差别。

在服务器端跑长时间运行的Python程序的就稍微惨一些。看这个案例：[An arm wrestle with Python’s garbage collector · Oyster.com Tech Blog](https://link.zhihu.com/?target=http%3A//tech.oyster.com/pythons-garbage-collector/)

现在部署量最大的Java程序的场景，不是在服务器端就是在移动设备上。两者虽然看似是两个极端，但对响应性的需求却颇为相似。
在服务器端上，我之前在[另一个问题的回答](https://www.zhihu.com/question/35109537/answer/61379522)里提到，十来GB、几百GB的GC堆也不稀罕，这种规模上GC效率低就很有问题了。
在移动设备上，应用吃内存也是越来越多，而且对响应性的要求更是高。以前Java ME还流行的时候还比较多JVM参与，现在随着Android的普及，更多Java程序是跑在Dalvik / ART上。以ART为例，为了降低GC对应用响应性的影响，最近都用上并发拷贝（concurrent copying）了，动作相当激进。

**5. 其它？**

待补充