



- Programming Paradigms for Dummies: What Every Programmer Should Know
- https://webperso.info.ucl.ac.be/~pvr/VanRoyChapter.pdf

![1204419550](https://p.ipic.vip/65vvn7.png)

这个图片展示了编程范式的分类（Taxonomy of Programming Paradigms）。以下是对该图中主要内容的解释：

### 编程范式的分类

1. **无名状态（Unnamed State）**：这种范式下，程序的状态变化不是显式命名的，通常包括逻辑和函数式编程范式。

    - **逻辑编程（Logic Programming）**：
        - **确定性逻辑编程（Deterministic Logic Programming）**：没有非确定性的逻辑推理，如统一（equality）。
        - **关系和逻辑编程（Relational & Logic Programming）**：包含搜索（search）功能，如Prolog和SQL嵌入。
        - **约束（逻辑）编程（Constraint (Logic) Programming）**：添加求解器（solver）功能，如CLP和ILOG Solver。
        - **并发约束编程（Concurrent Constraint Programming）**：包含线程和同步（synchronization）功能，如LIFE和AKL。

    - **函数式编程（Functional Programming）**：
        - **第一阶函数式编程（First-order Functional Programming）**：不包含闭包，如Scheme和ML。
        - **闭包（Closure）**：包含高阶函数和闭包的编程，如Haskell。
        - **懒惰函数式编程（Lazy Functional Programming）**：延迟计算，如Haskell。
        - **单一赋值的惰性数据流编程（Lazy Dataflow Programming with Single Assignment）**：如Oz、Alice和Curry。

2. **有名状态（Named State）**：这种范式下，程序的状态变化是显式命名的，通常包括命令式编程和面向对象编程范式。

    - **命令式编程（Imperative Programming）**：
        - **受控命令式编程（Guarded Command Programming）**：如Dijkstra的GCL。
        - **ADT命令式编程（ADT Imperative Programming）**：如CLU、OCaml和Oz。
        - **事件驱动编程（Event-driven Programming）**：如E。
        - **多代理编程（Multi-agent Programming）**：如Erlang和AKL。

    - **面向对象编程（Object-oriented Programming）**：
        - **顺序面向对象编程（Sequential Object-oriented Programming）**：如Java和OCaml。
        - **并发面向对象编程（Concurrent Object-oriented Programming）**：如Smalltalk和Oz。
        - **共享状态并发编程（Shared-state Concurrent Programming）**：如Java、OCaml、Erlang和AKL。

### 核心概念

- **状态表达的多样性（Expressiveness of State）**：从无名状态（Unnamed State）到有名状态（Named State）的转变，表示状态表达能力的增强。
- **可观察的非确定性（Observable Nondeterminism）**：表示编程范式是否包含可观察的非确定性行为，如并发或同步操作。

通过这张图可以看到，不同的编程范式在处理状态、并发和同步等方面有不同的特点和适用场景。这对于理解编程语言的设计和选择适合的编程范式有很大帮助。