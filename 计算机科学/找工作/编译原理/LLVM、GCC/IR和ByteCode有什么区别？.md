Bytecode（字节码）是一种IR（中间表示）的形式。

编译器所使用的IR可以有很多种形式。就其“形状”而言，可以分为：

- 基于图的，常见的情况有：

- - 基于树
  - 基于DAG（有向无环图）
  - 基于一般图

- 基于线性代码的，常见的情况有：

- - 三地址代码（四元组）
  - 二地址代码（三元组）
  - 零地址代码

- 基于图与线性代码混合的

- - 最常见的情况是控制流图（CFG）用图表示，而CFG的每个节点是基本块，每个基本块里的代码是线性代码

上面说的每种情况都还可以有许多细分的变种。

通常说的“字节码”其实就是一种线性代码，它最重要的特征是：

- 为存储、传输或直接解释执行而设计，因而指令格式一般比较紧凑

- 只使用1字节或者2字节来编码指令的操作码（opcode）。“字节码”因此而得名。

- - LLVM IR的二进制序列化形式叫做bitcode（比特码），原因是这种序列化格式更倾向最大限度的紧凑，里面可能会有窄于1字节的数据类型，所以特意不叫字节码而叫比特码。本质上并没啥特别的。


字节码中常常涉及的、但并非本质的一些点：

- 指令既可能是固定长度的（例如Android的dex字节码、Lua的字节码），也可能是可变长度的（例如JVM的Java字节码、CPython的字节码）
- 指令既可能是“基于寄存器”形式的（上面说的四地址、三地址代码），也可能是“基于栈”形式的（上面说的零地址代码）

