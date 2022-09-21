[TOC]

作者：Stefno
链接：https://zhuanlan.zhihu.com/p/80853548
来源：知乎
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



书中有这样一句话：

>  在 Go 语言里，go func 是并发的单元，chan 是协调并发单元的机制，panic 和 recover 是出错处理的机制，而 defer 是神来之笔，大大简化了出错的管理。
>  

Goroutines 在同一个用户空间里同时独立执行 functions，channels 则用于 goroutines 间的通信和同步访问控制。

上一篇文章里我们讲了 channel，并且提到，goroutine 和 channel 是 Go 并发编程的两大基石，那这篇文章就聚焦到 goroutine，以及调度 goroutine 的 go scheduler。

## 前置知识

## os scheduler

从操作系统角度看，我们写的程序最终都会转换成一系列的机器指令，机器只要按顺序执行完所有的指令就算完成了任务。完成“按顺序执行指令”任务的实体就是线程，也就是说，线程是 CPU 调度的实体，线程是真正在 CPU 上执行指令的实体。

每个程序启动的时候，都会创建一个初始进程，并且启动一个线程。而线程可以去创建更多的线程，这些线程可以独立地执行，CPU 在这一层进行调度，而非进程。

OS scheduler 保证如果有可以执行的线程时，就不会让 CPU 闲着。并且它还要保证，所有可执行的线程都看起来在同时执行。另外，OS scheduler 在保证高优先级的线程执行机会大于低优先级线程的同时，不能让低优先级的线程始终得不到执行的机会。OS scheduler 还需要做到迅速决策，以降低延时。

## 线程切换

OS scheduler 调度线程的依据就是它的状态，线程有三种状态（简化模型）：`Waiting`, `Runnable` or `Executing`。

![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtpjt9aj20k004ymxe.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtsmpvij214009wwfg.jpg)

线程能做的事一般分为两种：计算型、IO 型。

计算型主要是占用 CPU 资源，一直在做计算任务，例如对一个大数做质数分解。这种类型的任务不会让线程跳到 Waiting 状态。

IO 型则是要获取外界资源，例如通过网络、系统调用等方式。内存同步访问控制原语：mutexes 也可以看作这种类型。共同特点是需要等待外界资源就绪。IO 型的任务会让线程跳到 Waiting 状态。

线程切换就是操作系统用一个处于 Runnable 的线程将 CPU 上正在运行的处于 Executing 状态的线程换下来的过程。新上场的线程会变成 Executing 状态，而下场的线程则可能变成 Waiting 或 Runnable 状态。正在做计算型任务的线程，会变成 Runnable 状态；正在做 IO 型任务的线程，则会变成 Waiting 状态。

因此，计算密集型任务和 IO 密集型任务对线程切换的“态度”是不一样的。由于计算型密集型任务一直都有任务要做，或者说它一直有指令要执行，线程切换的过程会让它停掉当前的任务，损失非常大。

相反，专注于 IO 密集型的任务的线程，如果它因为某个操作而跳到 Waiting 状态，那么把它从 CPU 上换下，对它而言是没有影响的。而且，新换上来的线程可以继续利用 CPU 完成任务。从整个操作系统来看，“工作进度”是往前的。

记住，对于 OS scheduler 来说，最重要的是不要让一个 CPU 核心闲着，尽量让每个 CPU 核心都有任务可做。

>  If you have a program that is focused on IO-Bound work, then context switches are going to be an advantage. Once a Thread moves into a Waiting state, another Thread in a Runnable state is there to take its place. This allows the core to always be doing work. This is one of the most important aspects of scheduling. Don’t allow a core to go idle if there is work (Threads in a Runnable state) to be done.
>  

## 函数调用过程分析

要想理解 Go scheduler 的底层原理，对于函数调用过程的理解是必不可少的。它涉及到函数参数的传递，CPU 的指令跳转，函数返回值的传递等等。这需要对汇编语言有一定的了解，因为只有汇编语言才能进行像寄存器赋值这样的底层操作。之前的一些文章里也有说明，这里再来复习一遍。

>  函数栈帧的空间主要由函数参数和返回值、局部变量和被调用其它函数的参数和返回值空间组成。
>  

宏观看一下，Go 语言中函数调用的规范，引用曹大博客里的一张图：



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtwtfuhj20k00j5mxy.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtzq93bj20vd0u0q4z.jpg)



Go plan9 汇编通过栈传递函数参数和返回值。

调用子函数时，先将参数在栈顶准备好，再执行 CALL 指令。CALL 指令会将 IP 寄存器的值压栈，这个值就是函数调用完成后即将执行的下一条指令。

然后，就会进入被调用者的栈帧。首先会将 caller BP 压栈，这表示栈基址，也就是栈底。栈顶和栈基址定义函数的栈帧。

>  CALL 指令类似 PUSH IP 和 JMP somefunc 两个指令的组合，首先将当前的 IP 指令寄存器的值压入栈中，然后通过 JMP 指令将要调用函数的地址写入到 IP 寄存器实现跳转。
>  而 RET 指令则是和 CALL 相反的操作，基本和 POP IP 指令等价，也就是将执行 CALL 指令时保存在 SP 中的返回地址重新载入到 IP 寄存器，实现函数的返回。
>  首先是调用函数前准备的输入参数和返回值空间。然后 CALL 指令将首先触发返回地址入栈操作。在进入到被调用函数内之后，汇编器自动插入了 BP 寄存器相关的指令，因此 BP 寄存器和返回地址是紧挨着的。再下面就是当前函数的局部变量的空间，包含再次调用其它函数需要准备的调用参数空间。被调用的函数执行 RET 返回指令时，先从栈恢复 BP 和 SP 寄存器，接着取出的返回地址跳转到对应的指令执行。
>  

上面两段描述来自《Go 语言高级编程》一书的汇编语言章节，说得很好，再次推荐阅读。

## goroutine 是怎么工作的

## 什么是 goroutine

Goroutine 可以看作对 thread 加的一层抽象，它更轻量级，可以单独执行。因为有了这层抽象，Gopher 不会直接面对 thread，我们只会看到代码里满天飞的 goroutine。操作系统却相反，管你什么 goroutine，我才没空理会。我安心地执行线程就可以了，线程才是我调度的基本单位。

## goroutine 和 thread 的区别

谈到 goroutine，绕不开的一个话题是：它和 thread 有什么区别？

参考资料【How Goroutines Work】告诉我们可以从三个角度区别：内存消耗、创建与销毀、切换。

- 内存占用

创建一个 goroutine 的栈内存消耗为 2 KB，实际运行过程中，如果栈空间不够用，会自动进行扩容。创建一个 thread 则需要消耗 1 MB 栈内存，而且还需要一个被称为 “a guard page” 的区域用于和其他 thread 的栈空间进行隔离。

对于一个用 Go 构建的 HTTP Server 而言，对到来的每个请求，创建一个 goroutine 用来处理是非常轻松的一件事。而如果用一个使用线程作为并发原语的语言构建的服务，例如 Java 来说，每个请求对应一个线程则太浪费资源了，很快就会出 OOM 错误（OutOfMermoryError）。

- 创建和销毀

Thread 创建和销毀都会有巨大的消耗，因为要和操作系统打交道，是内核级的，通常解决的办法就是线程池。而 goroutine 因为是由 Go runtime 负责管理的，创建和销毁的消耗非常小，是用户级。

- 切换

当 threads 切换时，需要保存各种寄存器，以便将来恢复：

>  16 general purpose registers, PC (Program Counter), SP (Stack Pointer), segment registers, 16 XMM registers, FP coprocessor state, 16 AVX registers, all MSRs etc.
>  

而 goroutines 切换只需保存三个寄存器：Program Counter, Stack Pointer and BP。

一般而言，线程切换会消耗 1000-1500 纳秒，一个纳秒平均可以执行 12-18 条指令。所以由于线程切换，执行指令的条数会减少 12000-18000。

Goroutine 的切换约为 200 ns，相当于 2400-3600 条指令。

因此，goroutines 切换成本比 threads 要小得多。

## M:N 模型

我们都知道，Go runtime 会负责 goroutine 的生老病死，从创建到销毁，都一手包办。Runtime 会在程序启动的时候，创建 M 个线程（CPU 执行调度的单位），之后创建的 N 个 goroutine 都会依附在这 M 个线程上执行。这就是 M:N 模型：



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtxdfgcj20k0077jrn.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdu3olutj21400eemy2.jpg)



在同一时刻，一个线程上只能跑一个 goroutine。当 goroutine 发生阻塞（例如上篇文章提到的向一个 channel 发送数据，被阻塞）时，runtime 会把当前 goroutine 调度走，让其他 goroutine 来执行。目的就是不让一个线程闲着，榨干 CPU 的每一滴油水。

## 什么是 scheduler

Go 程序的执行由两层组成：Go Program，Runtime，即用户程序和运行时。它们之间通过函数调用来实现内存管理、channel 通信、goroutines 创建等功能。用户程序进行的系统调用都会被 Runtime 拦截，以此来帮助它进行调度以及垃圾回收相关的工作。

一个展现了全景式的关系如下图：



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtqrug3j20k00hsdgi.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtrt8ytj20nw0l8gmg.jpg)



## 为什么要 scheduler

Go scheduler 可以说是 Go 运行时的一个最重要的部分了。Runtime 维护所有的 goroutines，并通过 scheduler 来进行调度。Goroutines 和 threads 是独立的，但是 goroutines 要依赖 threads 才能执行。

Go 程序执行的高效和 scheduler 的调度是分不开的。

## scheduler 底层原理

实际上在操作系统看来，所有的程序都是在执行多线程。将 goroutines 调度到线程上执行，仅仅是 runtime 层面的一个概念，在操作系统之上的层面。

有三个基础的结构体来实现 goroutines 的调度。g，m，p。

`g` 代表一个 goroutine，它包含：表示 goroutine 栈的一些字段，指示当前 goroutine 的状态，指示当前运行到的指令地址，也就是 PC 值。

`m` 表示内核线程，包含正在运行的 goroutine 等字段。

`p` 代表一个虚拟的 Processor，它维护一个处于 Runnable 状态的 g 队列，`m` 需要获得 `p` 才能运行 `g`。

当然还有一个核心的结构体：`sched`，它总览全局。

Runtime 起始时会启动一些 G：垃圾回收的 G，执行调度的 G，运行用户代码的 G；并且会创建一个 M 用来开始 G 的运行。随着时间的推移，更多的 G 会被创建出来，更多的 M 也会被创建出来。

当然，在 Go 的早期版本，并没有 p 这个结构体，`m` 必须从一个全局的队列里获取要运行的 `g`，因此需要获取一个全局的锁，当并发量大的时候，锁就成了瓶颈。后来在大神 Dmitry Vyokov 的实现里，加上了 `p` 结构体。每个 `p` 自己维护一个处于 Runnable 状态的 `g` 的队列，解决了原来的全局锁问题。

Go scheduler 的目标：

>  For scheduling goroutines onto kernel threads.
>  



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdu1yq18j20k00ak0tk.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtta3w9j21400l4jth.jpg)



Go scheduler 的核心思想是：

1. reuse threads；
2. 限制同时运行（不包含阻塞）的线程数为 N，N 等于 CPU 的核心数目；
3. 线程私有的 runqueues，并且可以从其他线程 stealing goroutine 来运行，线程阻塞后，可以将 runqueues 传递给其他线程。

为什么需要 P 这个组件，直接把 runqueues 放到 M 不行吗？

>  You might wonder now, why have contexts at all? Can't we just put the runqueues on the threads and get rid of contexts? Not really. The reason we have contexts is so that we can hand them off to other threads if the running thread needs to block for some reason.
>  An example of when we need to block, is when we call into a syscall. Since a thread cannot both be executing code and be blocked on a syscall, we need to hand off the context so it can keep scheduling.
>  

翻译一下，当一个线程阻塞的时候，将和它绑定的 P 上的 goroutines 转移到其他线程。

Go scheduler 会启动一个后台线程 sysmon，用来检测长时间（超过 10 ms）运行的 goroutine，将其调度到 global runqueues。这是一个全局的 runqueue，优先级比较低，以示惩罚。



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdu59p0bj20k00bv75a.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdu4m8bdj21400nqgo2.jpg)



## 总览

通常讲到 Go scheduler 都会提到 GPM 模型，我们来一个个地看。

下图是我使用的 mac 的硬件信息，只有 2 个核。



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdu2gwf8j20aj06k3yp.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdu2y0pwj20aj06k3yp.jpg)



但是配上 CPU 的超线程，1 个核可以变成 2 个，所以当我在 mac 上运行下面的程序时，会打印出 4。

```text
func main() {
    // NumCPU 返回当前进程可以用到的逻辑核心数
    fmt.Println(runtime.NumCPU())
}
```

因为 NumCPU 返回的是逻辑核心数，而非物理核心数，所以最终结果是 4。

Go 程序启动后，会给每个逻辑核心分配一个 P（Logical Processor）；同时，会给每个 P 分配一个 M（Machine，表示内核线程），这些内核线程仍然由 OS scheduler 来调度。

总结一下，当我在本地启动一个 Go 程序时，会得到 4 个系统线程去执行任务，每个线程会搭配一个 P。

在初始化时，Go 程序会有一个 G（initial Goroutine），执行指令的单位。G 会在 M 上得到执行，内核线程是在 CPU 核心上调度，而 G 则是在 M 上进行调度。

G、P、M 都说完了，还有两个比较重要的组件没有提到： 全局可运行队列（GRQ）和本地可运行队列（LRQ）。 LRQ 存储本地（也就是具体的 P）的可运行 goroutine，GRQ 存储全局的可运行 goroutine，这些 goroutine 还没有分配到具体的 P。



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtr9c9wj20k009dgmd.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdu11p13j21400iqjtk.jpg)



Go scheduler 是 Go runtime 的一部分，它内嵌在 Go 程序里，和 Go 程序一起运行。因此它运行在用户空间，在 kernel 的上一层。和 Os scheduler 抢占式调度（preemptive）不一样，Go scheduler 采用协作式调度（cooperating）。

>  Being a cooperating scheduler means the scheduler needs well-defined user space events that happen at safe points in the code to make scheduling decisions.
>  

协作式调度一般会由用户设置调度点，例如 python 中的 yield 会告诉 Os scheduler 可以将我调度出去了。 

但是由于在 Go 语言里，goroutine 调度的事情是由 Go runtime 来做，并非由用户控制，所以我们依然可以将 Go scheduler 看成是抢占式调度，因为用户无法预测调度器下一步的动作是什么。

和线程类似，goroutine 的状态也是三种（简化版的）：

![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtuhxtej20k004xglw.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtvz5q1j214009ugmm.jpg)

下面这张 GPM 全局的运行示意图见得比较多，可以留着，看完后面的系列文章之后再回头来看，还是很有感触的：



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtv2eb8j20k00cu3z8.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtz61v6j21400po0uh.jpg)



## goroutine 调度时机

在四种情形下，goroutine 可能会发生调度，但也并不一定会发生，只是说 Go scheduler 有机会进行调度。

![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtvh9bkj20k006r0td.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtxrk4aj21400djtap.jpg)

## work stealing

Go scheduler 的职责就是将所有处于 runnable 的 goroutines 均匀分布到在 P 上运行的 M。

当一个 P 发现自己的 LRQ 已经没有 G 时，会从其他 P “偷” 一些 G 来运行。看看这是什么精神！自己的工作做完了，为了全局的利益，主动为别人分担。这被称为 `Work-stealing`，Go 从 1.1 开始实现。

Go scheduler 使用 M:N 模型，在任一时刻，M 个 goroutines（G） 要分配到 N 个内核线程（M），这些 M 跑在个数最多为 GOMAXPROCS 的逻辑处理器（P）上。每个 M 必须依附于一个 P，每个 P 在同一时刻只能运行一个 M。如果 P 上的 M 阻塞了，那它就需要其他的 M 来运行 P 的 LRQ 里的 goroutines。



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtwfi7rj20k00ajq3b.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtqbuhpj21400l3t9w.jpg)



个人感觉，上面这张图比常见的那些用三角形表示 M，圆形表示 G，矩形表示 P 的那些图更生动形象。

实际上，Go scheduler 每一轮调度要做的工作就是找到处于 runnable 的 goroutines，并执行它。找的顺序如下：

```text
runtime.schedule() {
    // only 1/61 of the time, check the global runnable queue for a G.
    // if not found, check the local queue.
    // if not found,
    //     try to steal from other Ps.
    //     if not, check the global runnable queue.
    //     if not found, poll network.
}
```

找到一个可执行的 goroutine 后，就会一直执行下去，直到被阻塞。

当 P2 上的一个 G 执行结束，它就会去 LRQ 获取下一个 G 来执行。如果 LRQ 已经空了，就是说本地可运行队列已经没有 G 需要执行，并且这时 GRQ 也没有 G 了。这时，P2 会随机选择一个 P（称为 P1），P2 会从 P1 的 LRQ “偷”过来一半的 G。



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdts4e9bj20k009iq39.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtynoq5j21400j0t9q.jpg)



这样做的好处是，有更多的 P 可以一起工作，加速执行完所有的 G。

## 同步/异步系统调用

当 G 需要进行系统调用时，根据调用的类型，它所依附的 M 有两种情况：`同步`和`异步`。

对于同步的情况，M 会被阻塞，进而从 P 上调度下来，P 可不养闲人，G 仍然依附于 M。之后，一个新的 M 会被调用到 P 上，接着执行 P 的 LRQ 里嗷嗷待哺的 G 们。一旦系统调用完成，G 还会加入到 P 的 LRQ 里，M 则会被“雪藏”，待到需要时再“放”出来。



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdu09a8zj20k00ah3zq.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdty7q9oj20qo0dzwgf.jpg)



对于异步的情况，M 不会被阻塞，G 的异步请求会被“代理人” network poller 接手，G 也会被绑定到 network poller，等到系统调用结束，G 才会重新回到 P 上。M 由于没被阻塞，它因此可以继续执行 LRQ 里的其他 G。



![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdu1iuemj20k00a9dgy.jpg)![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h6cdtu0p0pj20r30dw763.jpg)



可以看到，异步情况下，通过调度，Go scheduler 成功地将 I/O 的任务转变成了 CPU 任务，或者说将内核级别的线程切换转变成了用户级别的 goroutine 切换，大大提高了效率。

>  The ability to turn IO/Blocking work into CPU-bound work at the OS level is where we get a big win in leveraging more CPU capacity over time. 
>  

Go scheduler 像一个非常苛刻的监工一样，不会让一个 M 闲着，总是会通过各种办法让你干更多的事。

>  In Go, it’s possible to get more work done, over time, because the Go scheduler attempts to use less Threads and do more on each Thread, which helps to reduce load on the OS and the hardware.
>  

## scheduler 的陷阱

由于 Go 语言是协作式的调度，不会像线程那样，在时间片用完后，由 CPU 中断任务强行将其调度走。对于 Go 语言中运行时间过长的 goroutine，Go scheduler 有一个后台线程在持续监控，一旦发现 goroutine 运行超过 10 ms，会设置 goroutine 的“抢占标志位”，之后调度器会处理。但是设置标志位的时机只有在函数“序言”部分，对于没有函数调用的就没有办法了。

>  Golang implements a co-operative partially preemptive scheduler. 
>  

所以在某些极端情况下，会掉进一些陷阱。下面这个例子来自参考资料【scheduler 的陷阱】。

```text
func main() {
    var x int
    threads := runtime.GOMAXPROCS(0)
    for i := 0; i < threads; i++ {
        go func() {
            for { x++ }
        }()
    }
    time.Sleep(time.Second)
    fmt.Println("x =", x)
}
```

运行结果是：在死循环里出不来，不会输出最后的那条打印语句。

为什么？上面的例子会启动和机器的 CPU 核心数相等的 goroutine，每个 goroutine 都会执行一个无限循环。

创建完这些 goroutines 后，main 函数里执行一条 `time.Sleep(time.Second)` 语句。Go scheduler 看到这条语句后，简直高兴坏了，要来活了。这是调度的好时机啊，于是主 goroutine 被调度走。先前创建的 `threads` 个 goroutines，刚好“一个萝卜一个坑”，把 M 和 P 都占满了。

在这些 goroutine 内部，又没有调用一些诸如 `channel`，`time.sleep` 这些会引发调度器工作的事情。麻烦了，只能任由这些无限循环执行下去了。

解决的办法也有，把 threads 减小 1：

```text
func main() {
    var x int
    threads := runtime.GOMAXPROCS(0) - 1
    for i := 0; i < threads; i++ {
        go func() {
            for { x++ }
        }()
    }
    time.Sleep(time.Second)
    fmt.Println("x =", x)
}
```

运行结果：

```text
x = 0
```

不难理解了吧，主 goroutine 休眠一秒后，被 go schduler 重新唤醒，调度到 M 上继续执行，打印一行语句后，退出。主 goroutine 退出后，其他所有的 goroutine 都必须跟着退出。所谓“覆巢之下 焉有完卵”，一损俱损。

至于为什么最后打印出的 x 为 0，之前的文章[《曹大谈内存重排》](http://link.zhihu.com/?target=https%3A//qcrao.com/2019/06/17/cch-says-memory-reorder/)里有讲到过，这里不再深究了。

还有一种解决办法是在 for 循环里加一句：

```text
go func() {
    time.Sleep(time.Second)
    for { x++ }
}()
```

同样可以让 main goroutine 有机会调度执行。

## 总结

这篇文章，从宏观角度来看 Go 调度器，讲到了很多方面。接下来连续的 10 篇文章，我会深入源码，层层解析。敬请期待！

参考资料里有很多篇英文博客写得很好，当你掌握了基本原理后，看这些文章会有一种熟悉的感觉，讲得真好！

## 参考资料

【知乎回答，怎样理解阻塞非阻塞与同步异步的区别】？https://www.zhihu.com/question/19732473/answer/241673170

【从零开始学架构 Reactor与Proactor】[https://book.douban.com/subject/30335935/](http://link.zhihu.com/?target=https%3A//book.douban.com/subject/30335935/)

【思否上 goalng 排名第二的大佬译文】[https://segmentfault.com/a/1190000016038785](http://link.zhihu.com/?target=https%3A//segmentfault.com/a/1190000016038785)

【ardan labs】[https://www.ardanlabs.com/blog/2018/08/scheduling-in-go-part1.html](http://link.zhihu.com/?target=https%3A//www.ardanlabs.com/blog/2018/08/scheduling-in-go-part1.html)

【论文 Analysis of the Go runtime scheduler】[http://www.cs.columbia.edu/~aho/cs6998/reports/12-12-11_DeshpandeSponslerWeiss_GO.pdf](http://link.zhihu.com/?target=http%3A//www.cs.columbia.edu/~aho/cs6998/reports/12-12-11_DeshpandeSponslerWeiss_GO.pdf)

【译文传播很广的】[https://morsmachine.dk/go-scheduler](http://link.zhihu.com/?target=https%3A//morsmachine.dk/go-scheduler)

【码农翻身文章】[https://mp.weixin.qq.com/s/BV25ngvWgbO3_yMK7eHhew](http://link.zhihu.com/?target=https%3A//mp.weixin.qq.com/s/BV25ngvWgbO3_yMK7eHhew)

【goroutine 资料合集】[https://github.com/ardanlabs/gotraining/tree/master/topics/go/concurrency/goroutines](http://link.zhihu.com/?target=https%3A//github.com/ardanlabs/gotraining/tree/master/topics/go/concurrency/goroutines)

【大彬调度器系列文章】[http://lessisbetter.site/2019/03/10/golang-scheduler-1-history/](http://link.zhihu.com/?target=http%3A//lessisbetter.site/2019/03/10/golang-scheduler-1-history/)

【Scalable scheduler design doc 2012】[https://docs.google.com/document/d/1TTj4T2JO42uD5ID9e89oa0sLKhJYD0Y_kqxDv3I3XMw/edit#heading=h.rvfa6uqbq68u](http://link.zhihu.com/?target=https%3A//docs.google.com/document/d/1TTj4T2JO42uD5ID9e89oa0sLKhJYD0Y_kqxDv3I3XMw/edit%23heading%3Dh.rvfa6uqbq68u)

【Go scheduler blog post】[https://morsmachine.dk/go-scheduler](http://link.zhihu.com/?target=https%3A//morsmachine.dk/go-scheduler)

【work stealing】[https://rakyll.org/scheduler/](http://link.zhihu.com/?target=https%3A//rakyll.org/scheduler/)

【Tony Bai 也谈goroutine调度器】[https://tonybai.com/2017/06/23/an-intro-about-goroutine-scheduler/](http://link.zhihu.com/?target=https%3A//tonybai.com/2017/06/23/an-intro-about-goroutine-scheduler/)

【Tony Bai  调试实例分析】[https://tonybai.com/2017/11/23/the-simple-analysis-of-goroutine-schedule-examples/](http://link.zhihu.com/?target=https%3A//tonybai.com/2017/11/23/the-simple-analysis-of-goroutine-schedule-examples/)

【Tony Bai goroutine 是如何工作的】[https://tonybai.com/2014/11/15/how-goroutines-work/](http://link.zhihu.com/?target=https%3A//tonybai.com/2014/11/15/how-goroutines-work/)

【How Goroutines Work】[https://blog.nindalf.com/posts/how-goroutines-work/](http://link.zhihu.com/?target=https%3A//blog.nindalf.com/posts/how-goroutines-work/)

【知乎回答 什么是阻塞，非阻塞，同步，异步？】https://www.zhihu.com/question/26393784/answer/328707302

【知乎文章 完全理解同步/异步与阻塞/非阻塞】https://zhuanlan.zhihu.com/p/22707398

【The Go netpoller】[https://morsmachine.dk/netpoller](http://link.zhihu.com/?target=https%3A//morsmachine.dk/netpoller)

【知乎专栏 Head First of Golang Scheduler】https://zhuanlan.zhihu.com/p/42057783

【鸟窝 五种 IO 模型】[https://colobu.com/2019/07/26/IO-models/](http://link.zhihu.com/?target=https%3A//colobu.com/2019/07/26/IO-models/)

【Go Runtime Scheduler】[https://speakerdeck.com/retervision/go-runtime-scheduler?slide=32](http://link.zhihu.com/?target=https%3A//speakerdeck.com/retervision/go-runtime-scheduler%3Fslide%3D32)

【go-scheduler】[https://povilasv.me/go-scheduler/#](http://link.zhihu.com/?target=https%3A//povilasv.me/go-scheduler/%23)

【追踪 scheduler】[https://www.ardanlabs.com/blog/2015/02/scheduler-tracing-in-go.html](http://link.zhihu.com/?target=https%3A//www.ardanlabs.com/blog/2015/02/scheduler-tracing-in-go.html)

【go tool trace 使用】[https://making.pusher.com/go-tool-trace/](http://link.zhihu.com/?target=https%3A//making.pusher.com/go-tool-trace/)

【goroutine 之旅】[https://medium.com/@riteeksrivastava/a-complete-journey-with-goroutines-8472630c7f5c](http://link.zhihu.com/?target=https%3A//medium.com/%40riteeksrivastava/a-complete-journey-with-goroutines-8472630c7f5c)

【介绍 concurreny 和 parallelism 区别的视频】[https://www.youtube.com/watch?v=cN_DpYBzKso&t=422s](http://link.zhihu.com/?target=https%3A//www.youtube.com/watch%3Fv%3DcN_DpYBzKso%26t%3D422s)

【scheduler 的陷阱】[http://www.sarathlakshman.com/2016/06/15/pitfall-of-golang-scheduler](http://link.zhihu.com/?target=http%3A//www.sarathlakshman.com/2016/06/15/pitfall-of-golang-scheduler)

【boya 源码阅读】[https://github.com/zboya/golang_runtime_reading/blob/master/src/runtime/proc.go](http://link.zhihu.com/?target=https%3A//github.com/zboya/golang_runtime_reading/blob/master/src/runtime/proc.go)

【阿波张调度器系列教程】[http://mp.weixin.qq.com/mp/homepage?__biz=MzU1OTg5NDkzOA==&hid=1&sn=8fc2b63f53559bc0cee292ce629c4788&scene=18#wechat_redirect](http://link.zhihu.com/?target=http%3A//mp.weixin.qq.com/mp/homepage%3F__biz%3DMzU1OTg5NDkzOA%3D%3D%26hid%3D1%26sn%3D8fc2b63f53559bc0cee292ce629c4788%26scene%3D18%23wechat_redirect)

【曹大 asmshare】[https://github.com/cch123/asmshare/blob/master/layout.md](http://link.zhihu.com/?target=https%3A//github.com/cch123/asmshare/blob/master/layout.md)

【Go调度器介绍和容易忽视的问题】[https://www.cnblogs.com/CodeWithTxT/p/11370215.html](http://link.zhihu.com/?target=https%3A//www.cnblogs.com/CodeWithTxT/p/11370215.html)

【最近发现的一位大佬的源码分析】[https://github.com/changkun/go-](http://link.zhihu.com/?target=https%3A//github.com/changkun/go-under-the-hood/blob/master/book/zh-cn/TOC.md)