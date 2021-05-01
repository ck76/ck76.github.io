# Rust 编程语言的核心部件

[Rust ](http://www.rust-lang.org/)是一门强调安全、并发、高效的系统编程语言。无 GC 实现内存安全机制、无数据竞争的并发机制、无运行时开销的抽象机制，是 Rust 独特的优越特性。它声称解决了传统 C 语言和 C++ 语言几十年来饱受责难的内存安全问题，同时还保持了很高的运行效率、很深的底层控制、很广的应用范围，在系统编程领域具有强劲的竞争力和广阔的应用前景。

从狭义的角度说，Rust 编程语言，就是其语言本身，一份以人类语言描述的计算机编程语言的规范文档。然而单单语言本身，仅具有理论价值；要发挥其实用价值，往往还要有编译器、标准库、运行环境等一系列配套设施，共同组成一套完整的生态体系。

从广义的角度说，Rust 编程语言包括了：语言规范 (reference)、编译器 (rustc)、运行时 (runtime)、标准库 (std)、核心库 (core)、库 (crates)、包管理器 (cargo)、社区 (communities) 等等。

本文将详细介绍广义上的 Rust 编程语言之各个组成部分。

**语言规范**

[Rust 语言规范](https://doc.rust-lang.org/stable/reference.html)规定了 Rust 编程语言的语法和语义。跟其他语言规范一样，充满枯燥的文字，真正愿意通读下来的人很少。大多数人通过初级教程学习语言的基本语法和语义，仅在必要时翻阅或查阅语言规范的局部内容。不过严格来说，Rust 目前提供的这份文档并不算是语言规范 (specification)，而仅仅只是参考文档。

**编译器 (rustc)**

官方的 rustc 是目前唯一的 Rust 编译器（之前的 rustboot 编译器早就被废弃了），它负责把 Rust 源代码编译为可执行文件、Rust 库 (crates) 或其他库文件 (.a/.lib/.so/.dll)。

- rustc 是跨平台的应用程序，其可执行文件名是 rustc (for Unix/Linux/…) 或 rustc.exe (for Windows)，最基本的命令行调用方法是 rustc hello.rs。
- rustc 具有交叉编译功能，可以在当前平台下编译出可运行于其他平台的应用程序和库（但需要事先编译或安装目标平台的工具链）。
- rustc 采用 LLVM 作为编译器后端，具有很好的代码生成和优化技术，支持许多目标平台。
- rustc 目前使用 gcc 作为链接器（同时也运行时依赖 glibc 运行库，今后可换用 MUSL 静态库，相关开发工作在进行中）；今后在 Windows 平台将支持使用 MSVC 作为链接器（相关开发工作在进行中）。
- rustc 编译出来的程序，支持用 GDB 和 LLDB 调试运行。用户不需要更换自己已经熟悉的调试工具，Rust 没有也不需要自己专属的调试器。
- rustc 是用 Rust 语言开发的，并且是开源的，最新源代码在[这里](https://github.com/rust-lang/rust/tree/master/src/librustc)。[ https://github.com/rust-lang/rust/tree/master/src/librustc](https://github.com/rust-lang/rust/tree/master/src/librustc)

**运行时 (runtime)**

在没有明确上下文的情况下，运行时 (runtime) 通常可被理解为“运行时库 (runtime library)”或“运行时损耗 (runtime overhead)”。下面就这两种情况分别阐述，最后得出的结论是：Rust 可以没有运行时库，且仅有很小的运行时损耗。

**运行时库 (runtime library)**

编程语言的运行时库，通常理解为，其编译出的可执行程序在运行时必须依赖的非操作系统本身的动态库。例如 C 程序必须依赖 msvcrt 或 glibc，Java 程序必须依赖 JRE，VB 程序必须依赖 msvbvm，易语言程序必须依赖 krnln.fne/fnr，等等。由于 C 运行时库往往跟操作系统紧密集成（尤其是类 Unix 系统），可以认为 C 运行时库是操作系统的一部分，进而认为 C 没有运行时库（当然这里见仁见智）。如果认同这一点，那么，经过静态编译生成的 Rust 程序，运行时仅依赖 C 运行时库，也就可以认为没有运行时库了。即使不认同这一点，等以后 Rust 支持了[静态链接MUSL 库](https://github.com/rust-lang/rust/pull/24777)（同时抛弃掉glibc），依然能够做到没有运行时库。当然，动态编译的Rust 程序中运行时还是必须依赖标准库libstd-*.so 等动态库的，这是给予程序员的额外可选项。

说Rust“可以”没有运行时库，就是说运行时库不是必需的，程序员拥有选择权（而不是被迫必须接受运行时库）。

那为什么说没有运行时库是一个优势呢？因为运行时库本身也有平台依赖性和/ 或运行时依赖性，有运行时库就意味着，你的程序只能在运行时库所支持的平台下运行，也就是说它限制了程序的部署平台。而运行时库支持哪些平台并不是程序员个体所能决定的。就算运行时库官方开发商决定向新的平台移植，也往往受诸多因素干扰，例如十多年前试图将JRE 移植到手机平台时就破费周折，甚至不得不大幅删减功能、人为制造了残缺不全的手机版JRE。再试想，在一个没有网络系统、没有文件系统，甚至没有操作系统的嵌入式平台上，你有可能在上面跑JRE 环境吗？做梦。没有了运行时库，程序的所有代码都是程序员可控的（至于标准库的影响，下文将会谈到）。

（更宽泛地说，运行时库居无定形，未必一定以独立动态库的形式存在，它也可能隐身于标准库甚至是可执行文件内部。只要它给程序本身带来了额外的且无法消除的明显的依赖和不可忽略的运行时损耗，我们就通通认为它是运行时(库)。反过来说，如果运行时(库) 的运行时损耗小到一定程度，且没有带来额外的运行时依赖，我们甚至可以认为它不是运行时(库)。此中斟酌，见仁见智。）

**运行时损耗(runtime overhead)**

程序的运行时损耗，是指程序在运行过程中所必须付出的额外的代价。例如Java 的虚拟机、C#的垃圾回收器、脚本语言的解释器等等，这些子系统本身在运行时都会消耗数量可观的内存和CPU，影响程序和系统的运行性能。而Rust 没有虚拟机、垃圾回收器和解释器，所以没有这类运行时损耗。

此外，内存管理、栈管理、调用操作系统API 和C 库等各种情况下，都有可能产生额外的运行时损耗。

Rust 运行时需要每个函数执行 morestack 检查栈溢出（[ morestack 已被取消](https://github.com/rust-lang/rust/pull/27338)），为了内存安全这是“必需的”检查，而以 C 语言的思路去看可能就是“额外的”损耗，无论如何这项运行时损耗很小。Unwinding 仅发生在 panic 之后，不视为运行时损耗。Rust 采用 jemalloc 管理内存（也可禁用），不仅没有运行时损耗，反而带来运行效率的明显提升。

Rust 的 Rc 类型以引用计数管理对象内存，Arc 类型以 Atomic 引用计数管理对象内存，这是较小的运行时损耗。但如果程序员不主动使用 Rc/Arc 类型，则无需为此付出额外的代价。

展开一下，Go 语言的协程调度器，当然也有运行时损耗，但这在某种程度上是程序实现自身功能的必要，算不上“额外的”代价，如果不需要此功能则损耗很小，故本文作者不视其为运行时损耗。而其通过 channel 共享内存、管理逐步连续增长的栈、调用 C 库和系统 API，则被视为运行时损耗，因为这些都是“非必要的”损耗，而且损耗还不小。

那 Java 的 JIT 编译器在运行时把字节码编译为机器码，算不算运行时损耗呢？损耗肯定是有的，但仅在特定条件下触发，且其带来的收益可能远大于损耗，是提升运行性能的必要步骤，故本文作者不认为它引入了“额外的”代价，不视其为运行时损耗。而 Java 的虚拟机和垃圾收集器，显然是突出的运行时损耗。

**标准库 (std)**

Rust 的标准库，为绝大多数的、常规的 Rust 程序开发提供基础支持、跨平台支持，是应用范围最广、地位最重要的库（没有之一）。其规模居中，既不像传统 C 和 C++ 标准库那么简陋，也不像 Java 和.Net 标准库那样包罗万象。

Rust 标准库的内容大致归纳如下：

- 基础的接口性数据类型
  如 Copy, Send, Sized, Sync, Drop, Deref, Clone, Iterator, IntoIterator, Debug, Display, Option, Result, Error, Eq, Ord, Fn, Cell, Hash 等等，其中多数都被包含在 std::prelude 内。这些简明扼要的类型，构成了 Rust 生态系统的基石。如果标准库不提供这些类型，让第三方库各行其是的话，整个生态系统将很难形成合力。
- 基础类型操作接口
  如 bool, char, i8/u8, i16/u16, i32/u32, i64/u64, isize/usize, f32/f64, str/array/slice/tuple/pointer 等基础类型数据的操作接口及其实现。
- 常用的功能性数据类型
  如 String, Vec, HashMap, Rc, Arc, Box, CString, OsString, SipHasher 等等。满足常见的、常用的，或特定的功能需求。
- 常用的宏定义
  如 println!, format!, assert!, try!, panic!, vec!, thread_local!, file!, line!, include! 等等。基础的或核心的宏。其中某些宏是借助编译器实现的。
- 跨平台的 I/O 相关的系统功能
  如 std::io, std::fs, std::path, std::env, std::process 等等。
- 跨平台的网络 / 多线程 / 同步相关系统功能
  如 std::net, std::thread, std::sync 等等。
- 其他的不跨平台的操作系统相关功能
  如 std::os，为各主流操作系统分别提供了专门的操作接口，便于实现系统特有的功能调用。
- 底层操作接口
  如 std::mem, std::ptr, std::intrinsics 等，操作内存、指针、调用编译器固有函数。
- 其他

**核心库 (core)**

Rust 核心库，可以理解为是经过大幅精简的标准库，它被应用在标准库不能覆盖到的某些少数特定领域，如嵌入式开发。

前面提到过，标准库应用范围很广，为绝大多数应用程序提供支持。但是在嵌入式开发、操作系统开发、裸金属 (bare metal) 环境下，标准库就无能为力了。主要有以下两个原因导致标准库的应用范围受到一定的限制：

- 标准库的“跨平台”是指“跨主流操作系统平台”，也就是跨 Windows、Unix/Linux、Mac/OSX 等少数几个操作系统。标准库内有相当数量的 API（如文件、网络、多线程等）必须依赖操作系统提供的接口，到了非主流系统尤其是嵌入式系统环境下，标准库失去了底层系统的支撑根本就不可能工作。
- 标准库内有相当数量的 API（如 String/Vec、Box、panic 等）依赖内存申请和释放功能，但是在操作系统开发、裸金属 (bare metal) 环境下，要么不存在这些功能，要么需要自己开发。

这些限制对 Rust 标准库来说其实并不是问题，跟世界上大多数编程语言的标准库一样，为主流系统的主流应用开发提供丰富的功能支持，才是最重要的。如果单纯为了提升应用范围砍掉操作系统相关的功能，那标准库也大概成了空壳子，功能性和实用性大打折扣，彻底失去了标准库的价值——谁能接受一个连文件、网络、多线程功能都没有的标准库呢？

Rust 的选择是，在标准库之外，再单独提供一个核心库，重点应对嵌入式应用开发。核心库不依赖任何操作系统，也不提供文件 / 网络 / 多线程 / 内存申请释放相关的任何功能，因而可移植性更好、应用范围更广。当用 Rust 开发一个操作系统或硬件驱动或嵌入式应用时，你总不能指望去调用别的主流操作系统接口吧？那显然是不切实际的。所以对核心库来说，它缺少的那些 OS 相关功能原本就是多余的。

在代码开头写上 #![no_std] 就代表放弃标准库，而使用核心库。核心库里面有：基础的接口性数据类型（参见上文，下同）、基础类型操作接口、常用的功能性数据类型、常用的宏定义、底层操作接口等，而且跟标准库 API 几乎是完全一致的；再配合 alloc 库（或自己定制的 alloc 库）又有了内存申请释放功能；再加上 collections 库，String/Vec/HashMap 等也有了。事实上从内部实现来说，标准库里的某些功能正是来源于核心库（以及 alloc/collections 等）。

**库 (crate)**

把多个 Rust 源代码文件（后缀名.rs）放一起编译出来，就得到一个库。库通常以静态库.rlib 或动态库.so/.dll 的形式存在。我们称 Rust 库为 crate，就像别的语言把库称为 library 或 package 差不多一个意思，只是习惯上的命名不同。

库是 Rust 程序员共享代码和功能的基本单元。编写应用程序和软件，无非就是综合利用各种库，官方的库、自己的库、第三方的库，调用它们提供的接口 (API)，再融合自己的业务逻辑，最终达成目的。

在已经编译或安装了某个库 xxx 的前提下，要想调用这个库，需首先在源代码首部加入这么一行代码：

extern crate xxx;

我们不需要像 Java 担心 CLASSPATH 一样担心 Rust 库的加载路径，因为我们有 Cargo（下面会讲到），因为我们有静态编译。

目前 Rust 已经有了大概 3000 多个公开的第三方库，全部集中在 [crates.io ](https://crates.io/)网站上（下面也会讲到）。这些库绝大多数都是 Github 上面的开源项目。极少听到有谁发布二进制的库（而不是发布源代码）。

**包管理器 (Cargo)**

Cargo 是 Rust 官方提供的包管理器（package manager），类似于 Java 界的 Gradle。Cargo 负责下载库源代码，分析库的依赖项，下载依赖项的源代码，再分析依赖项的依赖项，如此这般，最终把它们逐个编译出来。一句话，就是处理下载（源代码）、依赖（第三方库）、和编译（生成库或可执行文件）。有了 Cargo，无论多复杂的项目，无论有多复杂的依赖项，也只需在项目根目录下执行这么一条命令：

cargo build

Cargo 包管理器跟 crates.io 网站形成了完整的生态系统。crates.io 就是一个中心仓库，全世界几乎所有的 Rust 项目都被整合在此仓库中。每一个项目都包含了一个 Cargo.toml 的配置文件，指定了自身的依赖项。Cargo 就是围绕 Cargo.toml 开展工作的。

在 C 和 C++ 的世界里，如果一个开源项目没有任何依赖，往往会被当作一项优点。因为大家都知道，编译带有依赖项的源代码项目非常麻烦，尤其是当依赖项又有依赖项的时候，或者当依赖项的版本号又不明确的时候。几十年了，都没出现一个被广泛接受的基于版本的依赖管理和编译工具，颇为遗憾。Rust 不一样，它一开始就有了 Cargo。

Cargo 是一个令人骄傲的优秀工具。而且它不仅是一个工具，更是一个生态系统。

**社区 (communities)**

Rust 有相当庞大的社区。仅参与开发 Rust 系统本身的开发者就多达 1300 人，并持续增长，这类开发者中，以 Mozilla 公司员工组成的约 10 人团队为核心，以来自世界各地的贡献者为辅助。采用 Rust 开发应用的开发者人数更多，但难以统计数量。当然，作为新兴语言，Rust 社区规模相对 Java、Python 社区而言还稚嫩的很，发展潜力无限。

Rust 开发者活动轨迹主要集中在 Github 网站、IRC 在线聊天室、Reddit 论坛和 Rust 官方论坛中。此外，围绕某些颇具雄心的项目还各自形成了独立子社区，如[ Servo ](http://servo.org/)、[ Piston ](http://www.piston.rs/)、[ MaidSafe ](http://maidsafe.net/)、[ Redox ](http://www.redox-os.org/)等。

源代码仓库、设计开发讨论区：

- https://github.com/rust-lang/rust
- https://github.com/rust-lang/rfcs
- https://github.com/rust-lang/cargo
- [https://internals.rust-lang.org](https://internals.rust-lang.org/)
- https://client00.chat.mibbit.com/?server=irc.mozilla.org&channel=%23rustc

用户应用讨论提问区：

- https://www.reddit.com/r/rust
- [https://users.rust-lang.org](https://users.rust-lang.org/)
- https://stackoverflow.com/questions/tagged/rust
- https://client00.chat.mibbit.com/?server=irc.mozilla.org&channel=%23rust

官方网站：

- [https://www.rust-lang.org](https://www.rust-lang.org/)
- https://www.rust-lang.org/community.html
- [http://blog.rust-lang.org](http://blog.rust-lang.org/)
- [https://crates.io](https://crates.io/)
- [http://this-week-in-rust.org](http://this-week-in-rust.org/)

中文用户讨论区

- [http://rust.cc](http://rust.cc/)

**总结**

本文依次介绍了 Rust 编程语言及其编译器、运行时、库、工具和社区等核心部件，它们共同构成了生机勃发的 Rust 生态系统。



https://www.infoq.cn/article/rust-core-components/