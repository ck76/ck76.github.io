[toc]



# 第14章 非确定性和并行性

本章介绍非确定性和并行（或并发）程序和系统，以及它们的语义和逻辑。我们将从共享变量通信开始，讨论 Dijkstra 的受限命令语言（guarded commands），然后引入与 Occam 和 Hoare 的 CSP 密切相关的语言，最后讨论 Milner 的 CCS。在后面的语言中，通信完全通过同步的值交换进行。我们将引入一个由简单的带有递归的模态逻辑组成的规范语言，并推导出一种算法，用于检查有限状态过程是否满足规范。这将引导我们研究并行系统验证工具，例如爱丁堡-萨塞克斯并发工作台（Concurrency Workbench）和奥尔堡的 TAV 系统。本章最后将介绍其他方法，以及并行进程语义和逻辑中的一些当前研究问题。

## 14.1 引言

为了介绍并行编程语言中的一些基本问题，我们可以通过添加一个**并行组合（parallel composition）**的操作来扩展第2章的简单命令式语言 IMP。对于命令 $c_0, c_1$，它们的并行组合 $c_0 \parallel c_1$ 的执行就像 $c_0$ 和 $c_1$ 一起执行，没有特别偏向其中任何一个。

**问题引入**：

- 如果 $c_0$ 和 $c_1$ 都要对同一个变量赋值，会发生什么？
  - 一个（可能是任何一个）将执行其赋值，然后可能由另一个执行。
  - 显然，一个命令的赋值可以影响另一个命令随后操作的状态。

**结论**：

- 我们无法希望使用命令配置和最终状态之间的关系来准确地建模并行命令的执行。
- 我们必须使用表示执行过程中的单个**不可中断步骤（uninterruptible steps）**的关系，从而允许一个命令影响与之并行的另一个命令的状态。

**回顾**：

- 在第2章中，我们看到对于什么被视为单个不可中断步骤是有选择的。
  - 这由为命令执行和表达式求值写下的规则所决定。

**目标**：

- 我们可以解释命令的并行组合的执行方式，通过以下规则：

  $$
  \frac{(c_0, \sigma) \rightarrow (c_0', \sigma')}{(c_0 \parallel c_1, \sigma) \rightarrow (c_0' \parallel c_1, \sigma')}
  $$

  $$
  \frac{(c_0, \sigma) \rightarrow \sigma'}{(c_0 \parallel c_1, \sigma) \rightarrow (c_1, \sigma')}
  $$

  - 第一条规则表示 $c_0$ 的执行步骤导致并行组合的执行步骤，$c_0$ 继续执行。
  - 第二条规则表示 $c_0$ 的执行步骤完成，$c_0$ 执行完毕，剩下 $c_1$。

- 对于并行组合的右侧组件 $c_1$，有对称的规则。

**通信机制**：

- 如果并行组合的两个组件 $c_0$ 和 $c_1$ 有共同的变量，它们可能会影响彼此的执行。
- 它们可以被认为通过共享变量进行通信。
- 我们的并行组合提供了一个通常称为**通过共享变量通信（communication by shared variables）**的例子。

**非确定性**：

- 规则的对称性引入了命令行为的**不可预测性（unpredictability）**。
- 例如，考虑程序 $(X := 0 \parallel X := 1)$ 从初始状态开始执行。
  - 这个程序将终止，但变量 $X$ 的值可能是 $0$ 或 $1$。
- 更一般地，程序：

  $$
  (X := 0 \parallel X := 1); \quad \text{if } X = 0 \text{ then } c_0 \text{ else } c_1
  $$

  - 将执行 $c_0$ 或 $c_1$，我们无法预测是哪一个。

**总结**：

- 这种不可预测性被称为**非确定性（nondeterminism）**。
- 我们用来说明非确定性的程序可能是人为的，可能给人一种可以避免非确定性的印象。
- 然而，非确定性是现实生活中的事实。人和计算机系统以并行方式工作，导致非确定性行为的例子，并不与我们刚才看到的程序相距甚远。
- 我们注意到，对并行性的理解需要对非确定性的理解。

---

## 14.2 受限命令（Guarded Commands）

**引言**：

- 矛盾的是，**有纪律地使用非确定性**可以导致更直接的算法表示。
- 这是因为实现一个目标可能不依赖于执行多个任务中的哪一个。
- 在日常生活中，我们可能会指示某人“做这个或那个”，而不在乎是哪一个。

**Dijkstra 的受限命令语言**：

- 使用了一种非确定性结构，帮助程序员避免过度指定解决方法。
- 该语言具有算术表达式 $a \in \text{Aexp}$ 和布尔表达式 $b \in \text{Bexp}$，我们可以将其视为与 IMP 中的相同。

**语法**：

### 命令（Commands）：

$$
\begin{align*}
c ::= \ & \text{skip} \mid \text{abort} \mid X := a \mid c_0; c_1 \mid \text{if } gc \ \text{fi} \mid \text{do } gc \ \text{od}
\end{align*}
$$

### 受限命令（Guarded Commands）：

$$
\begin{align*}
gc ::= \ & b \rightarrow c \mid gc_0 \ \Box \ gc_1
\end{align*}
$$

- 其中，$\rightarrow$ 表示受限命令的基本构造，$\Box$ 称为**选择（alternative）**或“胖条”（fatbar），用于组合多个受限命令。

**解释**：

- 受限命令通常具有如下形式：

  $$
  b_1 \rightarrow c_1 \ \Box \ b_2 \rightarrow c_2 \ \Box \ \dots \ \Box \ b_n \rightarrow c_n
  $$

- 在这种情况下，布尔表达式 $b_i$ 称为**守卫（guard）**，命令 $c_i$ 称为**命令体（command body）**。
- 受限命令的执行取决于相应守卫 $b_i$ 在当前状态下是否求值为真。

**执行规则**：

- 如果在某个状态下，没有守卫求值为真，则受限命令**失败（fail）**，不会产生最终状态。
- 否则，受限命令以**非确定性方式**执行那些守卫为真的命令之一 $c_i$。

**新命令**：

- **skip**：不改变状态，直接完成。
- **abort**：不会从任何初始状态产生最终状态，即执行会中止。
- **if gc fi**：如果受限命令 $gc$ 不失败，则执行 $gc$；否则，行为类似于 **abort**。
- **do gc od**：反复执行受限命令 $gc$，只要 $gc$ 不失败；当 $gc$ 失败时，终止。如果 $gc$ 最初就失败，则行为类似于 **skip**。

---

### 形式化执行规则

**注意**：

- 我们将继承第2章 IMP 中的算术和布尔表达式的求值关系。
- 为了以后扩展语言以处理并行性，我们描述命令和受限命令执行中的**一步（one step）**。

**配置（Configurations）**：

- **命令配置**：$(c, \sigma)$ 或 $\sigma$，其中 $c$ 是命令，$\sigma$ 是状态。
- **受限命令的初始配置**：$(gc, \sigma)$。

**执行可能导致的配置**：

- 命令配置 $(c, \sigma)$ 或状态 $\sigma$。
- 受限命令的执行可以导致命令配置，或新的配置 **fail**，表示失败。

**执行规则**：

#### 命令的规则：

1. **Skip**：

   $$
   \frac{}{( \text{skip}, \sigma ) \rightarrow \sigma}
   $$

   - **解释**：执行 **skip**，状态不变。

2. **Abort**：

   $$
   \frac{}{( \text{abort}, \sigma ) \rightarrow \text{fail}}
   $$

   - **解释**：执行 **abort**，执行失败。

3. **赋值**：

   $$
   \frac{[a]_\sigma = n}{( X := a, \sigma ) \rightarrow \sigma [ n / X ]}
   $$

   - **解释**：计算表达式 $a$ 在状态 $\sigma$ 下的值 $n$，然后更新状态，将变量 $X$ 赋值为 $n$。

4. **顺序组合**：

   - **非终止步骤**：

     $$
     \frac{( c_0, \sigma ) \rightarrow ( c_0', \sigma' )}{( c_0; c_1, \sigma ) \rightarrow ( c_0'; c_1, \sigma' )}
     $$

     - **解释**：$c_0$ 执行一步，未完成，继续执行 $c_0$。

   - **终止步骤**：

     $$
     \frac{( c_0, \sigma ) \rightarrow \sigma'}{( c_0; c_1, \sigma ) \rightarrow ( c_1, \sigma')}
     $$

     - **解释**：$c_0$ 执行完毕，继续执行 $c_1$。

5. **条件语句**：

   - **成功执行**：

     $$
     \frac{( gc, \sigma ) \rightarrow ( c, \sigma' )}{( \text{if } gc \ \text{fi}, \sigma ) \rightarrow ( c, \sigma' )}
     $$

     - **解释**：受限命令 $gc$ 在状态 $\sigma$ 下执行，产生命令 $c$ 和新状态 $\sigma'$，然后执行 $c$。

   - **失败执行**：

     $$
     \frac{( gc, \sigma ) \rightarrow \text{fail}}{( \text{if } gc \ \text{fi}, \sigma ) \rightarrow \text{fail}}
     $$

     - **解释**：如果 $gc$ 失败，则整个条件语句失败。

6. **循环**：

   - **继续循环**：

     $$
     \frac{( gc, \sigma ) \rightarrow ( c, \sigma' )}{( \text{do } gc \ \text{od}, \sigma ) \rightarrow ( c; \ \text{do } gc \ \text{od}, \sigma' )}
     $$

     - **解释**：$gc$ 执行产生命令 $c$，继续执行 $c$，然后重复循环。

   - **循环终止**：

     $$
     \frac{( gc, \sigma ) \rightarrow \text{fail}}{( \text{do } gc \ \text{od}, \sigma ) \rightarrow \sigma}
     $$

     - **解释**：$gc$ 失败，循环终止，状态不变（相当于执行了 **skip**）。

#### 受限命令的规则：

1. **守卫为真**：

   $$
   \frac{[b]_\sigma = \text{true}}{( b \rightarrow c, \sigma ) \rightarrow ( c, \sigma )}
   $$

   - **解释**：如果守卫 $b$ 为真，执行命令 $c$。

2. **守卫为假**：

   $$
   \frac{[b]_\sigma = \text{false}}{( b \rightarrow c, \sigma ) \rightarrow \text{fail}}
   $$

   - **解释**：如果守卫 $b$ 为假，受限命令失败。

3. **选择（Alternative）**：

   - **左侧执行**：

     $$
     \frac{( gc_0, \sigma ) \rightarrow ( c, \sigma' )}{( gc_0 \ \Box \ gc_1, \sigma ) \rightarrow ( c, \sigma' )}
     $$

   - **右侧执行**：

     $$
     \frac{( gc_1, \sigma ) \rightarrow ( c, \sigma' )}{( gc_0 \ \Box \ gc_1, \sigma ) \rightarrow ( c, \sigma' )}
     $$

   - **双方失败**：

     $$
     \frac{( gc_0, \sigma ) \rightarrow \text{fail} \quad ( gc_1, \sigma ) \rightarrow \text{fail}}{( gc_0 \ \Box \ gc_1, \sigma ) \rightarrow \text{fail}}
     $$

   - **解释**：选择构造引入了非确定性，$gc_0$ 或 $gc_1$ 中的一个将被执行。

---

**示例**：

- 一个将两个位置 $X$ 和 $Y$ 中的最大值赋给位置 $MAX$ 的命令：

  $$
  \text{if } \\
  \quad X \geq Y \rightarrow MAX := X \\
  \Box \ Y \geq X \rightarrow MAX := Y \\
  \text{fi}
  $$

  - **解释**：如果 $X \geq Y$，则将 $X$ 赋给 $MAX$；如果 $Y \geq X$，则将 $Y$ 赋给 $MAX$。由于选择构造的非确定性，我们不关心两个守卫都为真时执行哪一个，反正结果都是正确的最大值。

- 使用受限命令语言编写的欧几里得算法求最大公约数（GCD）：

  $$
  \text{do } \\
  \quad X > Y \rightarrow X := X - Y \\
  \Box \ Y > X \rightarrow Y := Y - X \\
  \text{od}
  $$

  - **解释**：反复执行，直到 $X = Y$，此时 $X$ 和 $Y$ 都是初始值的最大公约数。

---

**练习14.1**

**问题**：

为受限命令语言给出一个操作语义，其规则确定配置和最终状态之间的转换，形式为 $(c, \sigma) \rightarrow \sigma'$ 和 $(gc, \sigma) \rightarrow \sigma'$。

**解答思路**：

- 我们需要修改上述规则，使得受限命令和命令的执行直接产生最终状态。
- 也就是说，规则的结论应该是状态 $\sigma'$，而不是中间的命令配置。

---

**练习14.2**

**问题**：

解释为什么以下程序会终止：

$$
\text{do } \\
\quad (2 \mid X \rightarrow X := (3 \times X)/2) \\
\Box \ (3 \mid X \rightarrow X := (5 \times X)/3) \\
\text{od}
$$

- 其中，$2 \mid X$ 表示 $2$ 能整除 $X$，$(3 \times X)/2$ 表示 $3 \times X$ 除以 $2$。

**解答思路**：

- 观察 $X$ 的值如何变化，证明该循环会终止。

---

**练习14.3**

**问题**：

部分正确性断言 $\{ A \} c \{ B \}$，其中 $c$ 是命令或受限命令，$A$ 和 $B$ 是关于状态的断言，当且仅当对于任何满足 $A$ 的状态，执行 $c$ 如果终止，则在满足 $B$ 的最终状态下终止。

- 写下 Dijkstra 语言中部分正确性断言的可靠（sound）证明规则。
- 你期望这些证明规则以何种意义上是完备的？
- 作为其完备性的测试，尝试使用它们证明欧几里得算法的部分正确性（参考练习6.16）。
- 你将如何在假设初始位置持有正数的情况下证明其终止性？

**解答思路**：

- 写出霍尔逻辑的证明规则，适用于受限命令语言。
- 讨论证明规则的完备性。
- 具体证明欧几里得算法的部分正确性和终止性。

---

**练习14.4**

**问题**：

设正则命令 $c$ 的语法如下：

$$
\begin{align*}
c ::= \ & \text{skip} \mid X := e \mid b? \mid c; c \mid c + c \mid c^*
\end{align*}
$$

- 其中 $X$ 是位置，$e$ 是整数表达式，$b$ 是布尔表达式。
- 状态 $\sigma$ 被认为是从位置集合到整数的函数。
- 假设整数和布尔表达式的意义由语义函数指定，因此 $I[e]_\sigma$ 是表达式 $e$ 在状态 $\sigma$ 下的整数值，$B[b]_\sigma$ 是布尔表达式 $b$ 在状态 $\sigma$ 下的布尔值。
- 正则命令 $c$ 的意义由形式 $(c, \sigma) \rightarrow \sigma'$ 的关系给出，表示在状态 $\sigma$ 下执行 $c$ 可以导致最终状态 $\sigma'$。
- 关系由以下规则确定：

  1. **Skip**：

     $$
     \frac{}{( \text{skip}, \sigma ) \rightarrow \sigma}
     $$

  2. **测试**：

     $$
     \frac{B[b]_\sigma = \text{true}}{( b?, \sigma ) \rightarrow \sigma}
     $$

     - **解释**：如果 $b$ 在 $\sigma$ 下为真，测试通过，状态不变。

  3. **赋值**：

     $$
     \frac{I[e]_\sigma = n}{( X := e, \sigma ) \rightarrow \sigma [ n / X ]}
     $$

  4. **顺序组合**：

     $$
     \frac{( c_0, \sigma ) \rightarrow \sigma'' \quad ( c_1, \sigma'' ) \rightarrow \sigma'}{( c_0; c_1, \sigma ) \rightarrow \sigma'}
     $$

  5. **非确定性选择**：

     $$
     \frac{( c_0, \sigma ) \rightarrow \sigma'}{( c_0 + c_1, \sigma ) \rightarrow \sigma'} \quad \text{或} \quad \frac{( c_1, \sigma ) \rightarrow \sigma'}{( c_0 + c_1, \sigma ) \rightarrow \sigma'}
     $$

     - **解释**：$c_0 + c_1$ 非确定性地选择执行 $c_0$ 或 $c_1$。

  6. **循环**：

     - **基**：

       $$
       \frac{}{( c^*, \sigma ) \rightarrow \sigma}
       $$

     - **递归**：

       $$
       \frac{( c, \sigma ) \rightarrow \sigma'' \quad ( c^*, \sigma'' ) \rightarrow \sigma'}{( c^*, \sigma ) \rightarrow \sigma'}
       $$

- 问题：

  (i) 写一个正则命令，与循环 **while b do c** 有相同的效果。

  (ii) 对于两个正则命令 $c_0$ 和 $c_1$，当且仅当对于所有状态 $\sigma$ 和 $\sigma'$，$( c_0, \sigma ) \rightarrow \sigma' \iff ( c_1, \sigma ) \rightarrow \sigma'$，定义 $c_0 = c_1$。从规则证明对于任何正则命令 $c$，有 $c^* = \text{skip} + c; c^*$。

  (iii) 写出正则命令的指称语义；$c$ 的指称应等于关系 $\{ ( \sigma, \sigma' ) \mid ( c, \sigma ) \rightarrow \sigma' \}$。

  (iv) 为正则命令的部分正确性断言提出证明规则，形式包括 $b?$、$c_0 + c_1$ 和 $c^*$。

**解答思路**：

- (i) 使用正则命令构造 **while b do c** 的等价命令。
- (ii) 利用给定的规则证明等式。
- (iii) 定义指称语义，并简要描述证明其正确性的策略。
- (iv) 为给定的构造提出部分正确性的证明规则。

---

## 14.3 通信进程（Communicating Processes）

**背景**：

- 在70年代后期，Hoare 和 Milner 独立地提出了一种新颖的通信原语。
- 他们认识到，每个有自己存储器的处理器系统将变得越来越重要。
- 寻找一种独立于通信介质的通信原语，介质本身可以被建模为一个进程。

**通信机制**：

- **同步通信**：原子的同步操作，可能伴随值的交换，作为通信的核心原语。
- 进程通过**通道（channels）**与其他进程通信。
- 通道可以被隐藏，使得在特定通道上的通信仅限于两个或多个进程。

**通信原语**：

- 进程可以准备在一个通道上进行输入或输出。
- 只有在环境中有执行互补操作的进程时，输入或输出才能成功。
- 没有自动的缓冲；输入或输出通信会被延迟，直到另一个进程准备好相应的输出或输入。
- 当通信成功时，输出的值被从输出进程复制到输入进程。

---

### 语言的语法

**基本成分**：

- **位置（Locations）**：$X \in \text{Loc}$
- **布尔表达式**：$b \in \text{Bexp}$
- **算术表达式**：$a \in \text{Aexp}$

**新元素**：

- **通道名称**：$\alpha, \beta, \gamma, \dots \in \text{Chan}$
- **输入表达式**：$\alpha? X$，其中 $X \in \text{Loc}$
- **输出表达式**：$\alpha! a$，其中 $a \in \text{Aexp}$

**命令（Commands）**：

$$
\begin{align*}
c ::= \ & \text{skip} \mid \text{abort} \mid X := a \mid \alpha? X \mid \alpha! a \mid c_0; c_1 \mid \text{if } gc \ \text{fi} \mid \text{do } gc \ \text{od} \mid c_0 \parallel c_1 \mid c \setminus \alpha
\end{align*}
$$

- 其中，$c \setminus \alpha$ 表示**限制（restriction）**，隐藏通道 $\alpha$。

**受限命令（Guarded Commands）**：

$$
\begin{align*}
gc ::= \ & b \rightarrow c \mid b \land \alpha? X \rightarrow c \mid b \land \alpha! a \rightarrow c \mid gc_0 \ \Box \ gc_1
\end{align*}
$$

**注意**：

- 并行组合 $c_0 \parallel c_1$ 只有在 $c_0$ 和 $c_1$ 不包含共同的变量时才是良构的。
- 一般来说，命令是良构的，当且仅当其所有形式为 $c_0 \parallel c_1$ 的子命令都是良构的。

**限制（Restriction）**：

- $c \setminus \alpha$ 隐藏通道 $\alpha$，使得只有 $c$ 内部的通信可以发生在该通道上。

---

### 语义的形式化

**目标**：

- 形式化地描述该通信语言的预期行为。

**状态（States）**：

- 和之前一样，状态是从位置到其包含的值的函数。

**命令配置（Command Configurations）**：

- 形式为 $(c, \sigma)$ 或 $\sigma$，其中 $c$ 是命令，$\sigma$ 是状态。

**执行中的一步（One Step in Execution）**：

- 我们使用**标签（labels）**来表示执行中的一步。
- 标签集合：

  $$
  \{ \alpha? n \mid \alpha \in \text{Chan}, n \in \mathbb{N} \} \cup \{ \alpha! n \mid \alpha \in \text{Chan}, n \in \mathbb{N} \}
  $$

- **意图**：

  - 对于输入命令 $( \alpha? X; c, \sigma )$，期望有如下带标签的转换：

    $$
    \frac{}{( \alpha? X; c, \sigma ) \xrightarrow{ \alpha? n } ( c, \sigma [ n / X ] )}
    $$

    - **解释**：进程准备在通道 $\alpha$ 上接收值 $n$，并将其存储在位置 $X$ 中，然后继续执行 $c$。

  - 对于输出命令 $( \alpha! a; c, \sigma )$，期望有如下转换：

    $$
    \frac{[ a ]_\sigma = n}{( \alpha! a; c, \sigma ) \xrightarrow{ \alpha! n } ( c, \sigma )}
    $$

    - **解释**：计算表达式 $a$ 的值 $n$，准备在通道 $\alpha$ 上输出 $n$，然后继续执行 $c$。

**通信**：

- 当两个命令并行执行时，如果一个准备在通道 $\alpha$ 上接收，另一个准备在通道 $\alpha$ 上发送，则可以发生通信。
- 例如，对于并行组合：

  $$
  ( \alpha? X; c_0 ) \parallel ( \alpha! a; c_1 )
  $$

  - 期望有如下未标记的转换：

    $$
    ( ( \alpha? X; c_0 ) \parallel ( \alpha! a; c_1 ), \sigma ) \rightarrow ( c_0 \parallel c_1, \sigma [ n / X ] )
    $$

    - 其中，$[ a ]_\sigma = n$。

- 同时，进程仍然可以与环境通信，保留如下带标签的转换：

  $$
  ( ( \alpha? X; c_0 ) \parallel ( \alpha! a; c_1 ), \sigma ) \xrightarrow{ \alpha? n } ( ( c_0 \parallel ( \alpha! a; c_1 ) ), \sigma [ n / X ] )
  $$

  $$
  ( ( \alpha? X; c_0 ) \parallel ( \alpha! a; c_1 ), \sigma ) \xrightarrow{ \alpha! n } ( ( \alpha? X; c_0 ) \parallel c_1, \sigma )
  $$

  - **解释**：第一个进程与环境通信，接收值 $n$；或者第二个进程与环境通信，输出值 $n$。

---

### 执行规则

**约定**：

- 使用 $\lambda$ 来表示标签，包括 $\alpha? n$、$\alpha! n$ 以及空标签 $\tau$。
- 将状态 $\sigma$ 视为配置 $( *, \sigma )$，其中 $*$ 表示空命令。
- 认为 $*$ 满足以下定律：

  $$
  *; c = c; * = * \parallel c = c \parallel * = c
  $$

  $$
  *; * = * \parallel * = ( * \setminus \alpha ) = *
  $$

- 这些定律表达了 $*$ 的性质，例如 $* \parallel c$ 就是 $c$。

#### 命令的规则：

1. **Skip**：

   $$
   \frac{}{( \text{skip}, \sigma ) \xrightarrow{ \tau } \sigma}
   $$

2. **赋值**：

   $$
   \frac{[ a ]_\sigma = n}{( X := a, \sigma ) \xrightarrow{ \tau } \sigma [ n / X ]}
   $$

3. **输入**：

   $$
   \frac{}{( \alpha? X, \sigma ) \xrightarrow{ \alpha? n } \sigma [ n / X ]}
   $$

4. **输出**：

   $$
   \frac{[ a ]_\sigma = n}{( \alpha! a, \sigma ) \xrightarrow{ \alpha! n } \sigma}
   $$

5. **顺序组合**：

   $$
   \frac{( c_0, \sigma ) \xrightarrow{ \lambda } ( c_0', \sigma' )}{( c_0; c_1, \sigma ) \xrightarrow{ \lambda } ( c_0'; c_1, \sigma' )}
   $$

   $$
   \frac{( c_0, \sigma ) \xrightarrow{ \lambda } \sigma'}{( c_0; c_1, \sigma ) \xrightarrow{ \lambda } ( c_1, \sigma' )}
   $$

6. **条件语句**：

   $$
   \frac{( gc, \sigma ) \xrightarrow{ \lambda } ( c, \sigma' )}{( \text{if } gc \ \text{fi}, \sigma ) \xrightarrow{ \lambda } ( c, \sigma' )}
   $$

   - 以及 $gc$ 失败的情况。

7. **循环**：

   $$
   \frac{( gc, \sigma ) \xrightarrow{ \lambda } ( c, \sigma' )}{( \text{do } gc \ \text{od}, \sigma ) \xrightarrow{ \lambda } ( c; \ \text{do } gc \ \text{od}, \sigma' )}
   $$

   $$
   \frac{( gc, \sigma ) \xrightarrow{ \text{fail} }}{( \text{do } gc \ \text{od}, \sigma ) \xrightarrow{ \lambda } \sigma}
   $$

8. **并行组合**：

   - **左侧执行**：

     $$
     \frac{( c_0, \sigma ) \xrightarrow{ \lambda } ( c_0', \sigma' )}{( c_0 \parallel c_1, \sigma ) \xrightarrow{ \lambda } ( c_0' \parallel c_1, \sigma' )}
     $$

   - **右侧执行**（对称规则）。

   - **通信**：

     $$
     \frac{( c_0, \sigma ) \xrightarrow{ \alpha? n } c_0' \quad ( c_1, \sigma ) \xrightarrow{ \alpha! n } c_1'}{( c_0 \parallel c_1, \sigma ) \xrightarrow{ \tau } ( c_0' \parallel c_1', \sigma )}
     $$

     - **解释**：当 $c_0$ 准备接收 $\alpha? n$，$c_1$ 准备发送 $\alpha! n$，则通信发生。

9. **限制（Restriction）**：

   $$
   \frac{( c, \sigma ) \xrightarrow{ \lambda } ( c', \sigma' ) \quad \lambda \neq \alpha? n, \alpha! n}{( c \setminus \alpha, \sigma ) \xrightarrow{ \lambda } ( c' \setminus \alpha, \sigma' )}
   $$

   - **解释**：如果标签 $\lambda$ 不是关于通道 $\alpha$ 的输入或输出，则限制可以传递执行。

#### 受限命令的规则：

- 与前面类似，但允许在守卫中进行通信。

---

### 示例

**示例1**：一个进程反复从通道 $a$ 接收值并在通道 $\beta$ 上发送：

$$
\text{do } ( \text{true} \land a? X \rightarrow \beta! X ) \ \text{od}
$$

**示例2**：一个具有容量为2的缓冲区，从通道 $a$ 接收，从通道 $\gamma$ 发送：

$$
( \text{do } ( \text{true} \land a? X \rightarrow \beta! X ) \ \text{od} \ \parallel \ \text{do } ( \text{true} \land \beta? Y \rightarrow \gamma! Y ) \ \text{od} ) \setminus \beta
$$

- **解释**：通过隐藏通道 $\beta$，通信只能在内部发生。

**示例3**：一个进程同时“监听”两个通道，并从其中一个读取值：

$$
\text{if } ( \text{true} \land a? X \rightarrow c_0 ) \ \Box \ ( \text{true} \land \beta? Y \rightarrow c_1 ) \ \text{fi}
$$

- **解释**：如果环境在 $a$ 或 $\beta$ 通道上发送值，进程将从其中一个接收。

---

**备注**：

- 该语言与 Occam（transputer 的编程语言）密切相关。
- 但我们的语言不包括所有 Occam 的特性，例如 **prialt** 操作符。
- 我们的语言也与 Hoare 的 CSP 密切相关。

# 总结

本节详细介绍了非确定性和并行编程中的基本概念，特别是通过共享变量和通信通道的并行组合。我们讨论了 Dijkstra 的受限命令语言，如何利用非确定性来编写更简洁的算法，以及如何形式化地描述通信进程的语义。

希望这些详细的解释能够帮助你更好地理解第14章的内容。如果你有任何疑问，欢迎继续提问！

### ---------------------------

# 14.4 Milner 的 CCS

**概述**：

Robin Milner 对**通信系统演算（Calculus of Communicating Systems，CCS）**的研究对并行性研究的基础产生了深远的影响。CCS 提供了一种形式化的方法来描述并分析并发系统中的进程通信和同步行为。

**与之前语言的关系**：

- 事实上，CCS 的语言几乎可以通过从上一节的语言中**移除命令式特性**得到，参数化进程的使用使得不再需要状态的概念。
- 实际上，**位置（locations）**本身可以表示为 CCS 中的进程。

## CCS 的基本概念

**通信机制**：

- CCS 中的进程通过连接到其端口（ports）的通道（channels）与环境进行通信，方式与我们之前看到的相同。
- 进程可以准备在某些通道上进行**输入**或**输出**操作。

**举例**：

- 假设有一个进程 $p$，它准备在通道 $\alpha$ 和 $\beta$ 上输入，并在通道 $\alpha$ 和 $l$ 上输出。

- 我们可以将其可视化为：

  ```
      p
    / | \
  α? β? α!
  ```

  - 其中，$\alpha?$ 和 $\beta?$ 表示在通道 $\alpha$ 和 $\beta$ 上准备输入，$\alpha!$ 表示在通道 $\alpha$ 上准备输出。

- 如果我们有另一个进程 $q$，能够在 $\alpha$ 上输入，在 $\beta$ 和 $\delta$ 上输出，那么它的可视化为：

  ```
      q
    / | \
  α? β! δ!
  ```

- 将进程 $p$ 与进程 $q$ 并行组合，即 $p \parallel q$，可以被视为一个进程，其端口包括：

  - $\alpha?$、$\alpha!$、$\beta?$、$\beta!$、$l!$、$\delta!$

**限制（Restriction）操作**：

- **限制操作**用于**隐藏指定的端口集合**。
- 例如，对进程 $p$ 限制掉标签集合 $\{ \alpha, l \}$，得到进程 $p \setminus \{ \alpha, l \}$，这个进程只能在通道 $\beta$ 上执行输入操作。

- 可视化为：

  ```
      p \ {α, l}
        |
      β?
  ```

- 这样，所有在 $\alpha$ 和 $l$ 通道上的通信都被隐藏，只能进行内部通信。

**重命名（Relabelling）**：

- 通常，我们需要生成多个相同的进程，但对其通道进行重命名。
- **重命名函数**是对通道名称的函数。

- 例如，令重命名函数 $f$ 满足：

  - $f(\alpha) = l$
  - $f(\beta) = \delta$
  - $f(l) = l$

- 经过重命名函数 $f$ 后，进程 $p$ 变为 $p[f]$，其与环境的接口变为：

  ```
      p[f]
    / | \
  l? δ? l!
  ```

- 这意味着通道名称发生了变化，进程的行为方式保持不变。

## CCS 的语法

**基本元素**：

- **动作（Actions）**：

  - **输入动作**：$\alpha? x \rightarrow p$，表示在通道 $\alpha$ 上输入一个值 $x$，然后继续执行进程 $p$。
  - **输出动作**：$\alpha! a \rightarrow p$，表示在通道 $\alpha$ 上输出表达式 $a$ 的值，然后继续执行进程 $p$。
  - **内部动作**：$\tau \rightarrow p$，表示执行一个内部动作（如 skip），然后继续执行进程 $p$。
  - **布尔守卫**：$b \rightarrow p$，其中 $b$ 是布尔表达式，当 $b$ 为真时，继续执行进程 $p$。

- **组合操作**：

  - **和（Sum）**：$P_0 + P_1$，表示进程 $P_0$ 和 $P_1$ 的非确定性选择。
  - **并行组合**：$P_0 \parallel P_1$，表示进程 $P_0$ 和 $P_1$ 的并行执行。
  - **限制**：$p \setminus L$，隐藏进程 $p$ 中的端口集合 $L$。
  - **重命名**：$p[f]$，对进程 $p$ 的通道名进行重命名，按照函数 $f$。

- **进程标识符（Process Identifiers）**：

  - $P, Q, \dots$，用于命名进程，可以递归地定义进程的行为。

- **参数化进程**：

  - $P(a_1, \dots, a_k)$，表示带有参数 $a_1, \dots, a_k$ 的进程 $P$。

**完整的语法定义**：

$$
\begin{align*}
P ::= \ & \text{nil} \\
& \mid (\tau \rightarrow p) \\
& \mid (\alpha! a \rightarrow p) \\
& \mid (\alpha? x \rightarrow p) \\
& \mid (b \rightarrow p) \\
& \mid P_0 + P_1 \\
& \mid P_0 \parallel P_1 \\
& \mid p \setminus L \\
& \mid p[f] \\
& \mid P(a_1, \dots, a_k)
\end{align*}
$$

- 其中：

  - $\text{nil}$：空进程，不执行任何动作。
  - $a$ 和 $b$ 分别是算术表达式和布尔表达式。
  - $x$ 是变量。
  - $L$ 是通道名的子集。
  - $f$ 是重命名函数。

**变量的绑定**：

- 在 CCS 中，输入动作 $\alpha? x \rightarrow p$ 中的变量 $x$ 类似于一个**lambda 抽象**，表示对 $x$ 的绑定。
- $x$ 在 $p$ 中的所有出现都被认为是被绑定的，除非它出现在形式为 $\beta? x \rightarrow q$ 的子进程中。

- **自由变量**：未被绑定的变量。

**进程定义**：

- 进程标识符 $P$ 关联着一个定义，写作：

  $$
  P(x_1, \dots, x_k) \overset{\text{def}}{=} p
  $$

- 其中，$p$ 是一个进程，$x_1, \dots, x_k$ 是不同的变量，$p$ 的所有自由变量都在列表中。

- 进程定义可以是递归的，例如：

  $$
  \begin{cases}
  P(x_1, \dots, x_k) \overset{\text{def}}{=} p \\
  Q(y_1, \dots, y_l) \overset{\text{def}}{=} q
  \end{cases}
  $$

  - 其中，$p$ 和 $q$ 都可以引用 $P$ 和 $Q$。

## 操作语义

**假设**：

- 我们只指定**没有自由变量**的进程的转换。
- 这样，我们可以在操作语义中不使用环境（environments）来处理变量。

**表达式的求值**：

- 对于不含变量的表达式，使用关系 $a \rightarrow n$ 和 $b \rightarrow t$ 来描述算术和布尔表达式的求值。

**动作的范围**：

- $\lambda$ 表示动作，范围包括：

  - $\alpha? n$：在通道 $\alpha$ 上输入值 $n$。
  - $\alpha! n$：在通道 $\alpha$ 上输出值 $n$。
  - $\tau$：内部动作。

### 操作语义的规则

**空进程**：

- $\text{nil}$ 没有规则，因为它不执行任何动作。

**受限进程（Guarded Processes）**：

1. **内部动作**：

   $$
   \frac{}{(\tau \rightarrow p) \xrightarrow{\tau} p}
   $$

   - **解释**：执行一个内部动作，然后继续执行进程 $p$。

2. **输出动作**：

   $$
   \frac{a \rightarrow n}{(\alpha! a \rightarrow p) \xrightarrow{\alpha! n} p}
   $$

   - **解释**：计算表达式 $a$ 的值为 $n$，然后在通道 $\alpha$ 上输出 $n$，继续执行 $p$。

3. **输入动作**：

   $$
   \frac{}{(\alpha? x \rightarrow p) \xrightarrow{\alpha? n} p[n / x]}
   $$

   - **解释**：在通道 $\alpha$ 上接收值 $n$，然后将 $n$ 替换 $p$ 中的变量 $x$，继续执行。

4. **布尔守卫**：

   - **守卫为真**：

     $$
     \frac{b \rightarrow \text{true}}{(b \rightarrow p) \xrightarrow{\tau} p}
     $$

     - **解释**：如果布尔表达式 $b$ 为真，则执行内部动作 $\tau$，继续执行 $p$。

   - **守卫为假**：

     - 没有规则，表示无法执行。

**和（Sum）**：

- **左侧选择**：

  $$
  \frac{p_0 \xrightarrow{\lambda} p_0'}{p_0 + p_1 \xrightarrow{\lambda} p_0'}
  $$

- **右侧选择**：

  $$
  \frac{p_1 \xrightarrow{\lambda} p_1'}{p_0 + p_1 \xrightarrow{\lambda} p_1'}
  $$

- **解释**：非确定性地选择执行 $p_0$ 或 $p_1$。

**并行组合**：

- **独立动作**：

  - 左进程执行动作：

    $$
    \frac{p_0 \xrightarrow{\lambda} p_0'}{p_0 \parallel p_1 \xrightarrow{\lambda} p_0' \parallel p_1}
    $$

    - **条件**：$\lambda$ 不是 $\alpha? n$ 或 $\alpha! n$。

  - 右进程执行动作：

    $$
    \frac{p_1 \xrightarrow{\lambda} p_1'}{p_0 \parallel p_1 \xrightarrow{\lambda} p_0 \parallel p_1'}
    $$

    - **条件**：同上。

- **通信动作**：

  - 当一个进程准备在通道 $\alpha$ 上输入，另一个准备在同一通道上输出时，发生通信。

    $$
    \frac{p_0 \xrightarrow{\alpha? n} p_0' \quad p_1 \xrightarrow{\alpha! n} p_1'}{p_0 \parallel p_1 \xrightarrow{\tau} p_0' \parallel p_1'}
    $$

    - **解释**：进程 $p_0$ 和 $p_1$ 在通道 $\alpha$ 上通信，交换值 $n$，并继续执行各自的后续进程。

  - 反之亦然。

**限制（Restriction）**：

- 当动作 $\lambda$ 不在被限制的通道集合 $L$ 中时，限制可以传递：

  $$
  \frac{p \xrightarrow{\lambda} p'}{p \setminus L \xrightarrow{\lambda} p' \setminus L}
  $$

  - **条件**：如果 $\lambda$ 是 $\alpha? n$ 或 $\alpha! n$，则 $\alpha \notin L$。

**重命名（Relabelling）**：

- 对进程 $p$ 中的动作按照函数 $f$ 进行重命名：

  $$
  \frac{p \xrightarrow{\lambda} p'}{p[f] \xrightarrow{f(\lambda)} p'[f]}
  $$

**进程标识符**：

- 进程标识符 $P$ 的规则：

  $$
  \frac{p[ a_1 / x_1, \dots, a_k / x_k ] \xrightarrow{\lambda} p'}{P(a_1, \dots, a_k) \xrightarrow{\lambda} p'}
  $$

  - **解释**：用参数 $a_1, \dots, a_k$ 替换 $P$ 中的变量 $x_1, \dots, x_k$，然后执行 $p$ 的规则。

## 变量的消除

**目标**：

- 消除 CCS 中的变量，得到一个更基本的演算，称为**纯 CCS（pure CCS）**。

**思路**：

- 我们假设通信过程中传递的值来自一个有限集合 $V = \{ v_1, \dots, v_k \}$。
- 将输入动作扩展为特定的值，例如 $\alpha? n$。

**过程**：

- 将带有变量的输入动作替换为对所有可能值的求和。

- 例如，将 $( \alpha? x \rightarrow p )$ 替换为：

  $$
  \sum_{v \in V} ( \alpha? v \rightarrow p[ v / x ] )
  $$

- 当值集合 $V$ 是无限的，例如自然数集合 $\mathbb{N}$，我们可以引入**任意求和**（arbitrary sums）来表示。

**消除变量后的语法（纯 CCS）**：

- 动作 $\lambda$ 包括：

  - $l$，表示动作标签，例如 $\alpha$。
  - $\bar{l}$，表示 $l$ 的补动作（complementary action）。
  - $\tau$，内部动作。

- 进程的语法：

  $$
  P ::= \text{nil} \mid \lambda . p \mid \sum_{i \in I} P_i \mid P_0 \parallel P_1 \mid p \setminus L \mid p[f] \mid P
  $$

  - 其中，$\lambda . p$ 是 $\lambda$ 后跟进程 $p$ 的受限进程。

- **求和**：$\sum_{i \in I} P_i$，表示对索引集 $I$ 中的所有进程 $P_i$ 的求和。

- **补动作**：补动作 $\bar{l}$ 满足 $\bar{\bar{l}} = l$。

## 操作语义（纯 CCS）

- 规则与之前类似，但更加简洁。

**空进程**：

- $\text{nil}$ 没有规则。

**受限进程**：

- **动作执行**：

  $$
  \frac{}{\lambda . p \xrightarrow{\lambda} p}
  $$

**求和**：

- 对于索引集 $I$ 中的某个 $j$：

  $$
  \frac{P_j \xrightarrow{\lambda} q}{\sum_{i \in I} P_i \xrightarrow{\lambda} q}
  $$

**并行组合**：

- **独立动作**：

  - 左进程执行动作：

    $$
    \frac{P_0 \xrightarrow{\lambda} P_0'}{P_0 \parallel P_1 \xrightarrow{\lambda} P_0' \parallel P_1}
    $$

  - 右进程执行动作：

    $$
    \frac{P_1 \xrightarrow{\lambda} P_1'}{P_0 \parallel P_1 \xrightarrow{\lambda} P_0 \parallel P_1'}
    $$

- **通信动作**：

  - 当一个进程执行动作 $l$，另一个执行补动作 $\bar{l}$，发生通信：

    $$
    \frac{P_0 \xrightarrow{l} P_0' \quad P_1 \xrightarrow{\bar{l}} P_1'}{P_0 \parallel P_1 \xrightarrow{\tau} P_0' \parallel P_1'}
    $$

**限制（Restriction）**：

- 当动作 $\lambda$ 不在被限制的标签集合 $L$ 及其补集中时，限制可以传递：

  $$
  \frac{P \xrightarrow{\lambda} Q \quad \lambda \notin L \cup \bar{L}}{P \setminus L \xrightarrow{\lambda} Q \setminus L}
  $$

**重命名（Relabelling）**：

- 对动作按照函数 $f$ 进行重命名：

  $$
  \frac{P \xrightarrow{\lambda} Q}{P[f] \xrightarrow{f(\lambda)} Q[f]}
  $$

**进程标识符**：

- 进程定义 $P \overset{\text{def}}{=} p$：

  $$
  \frac{p \xrightarrow{\lambda} q}{P \xrightarrow{\lambda} q}
  $$

## 语言的转换

**从 CCS 到纯 CCS**：

- 我们可以将 CCS 中的闭合项 $t$ 转换为纯 CCS 中的项 $t'$，以保持它们的行为。

- **转换表**：

  | CCS                       | 纯 CCS                                                      |
  | ------------------------- | ----------------------------------------------------------- |
  | $\tau . p$                | $\tau . p$                                                  |
  | $\alpha! a \rightarrow p$ | $a_m . p$，其中 $a$ 表示值 $m$                              |
  | $\alpha? x \rightarrow p$ | $\sum_{m \in \mathbb{N}} ( a_m . p[ m / x ] )$              |
  | $b \rightarrow p$         | $p$，如果 $b$ 为真；$\text{nil}$，如果 $b$ 为假             |
  | $P_0 + P_1$               | $P_0 + P_1$                                                 |
  | $P_0 \parallel P_1$       | $P_0 \parallel P_1$                                         |
  | $p \setminus L$           | $p \setminus \{ a_m \mid \alpha \in L, m \in \mathbb{N} \}$ |
  | $P( a_1, \dots, a_k )$    | $P_{ m_1, \dots, m_k }$，其中 $a_i$ 表示值 $m_i$            |

- **进程定义的转换**：

  - CCS 中的定义：

    $$
    P( x_1, \dots, x_k ) \overset{\text{def}}{=} p
    $$

  - 转换为纯 CCS 中的定义集合：

    $$
    \{ P_{ m_1, \dots, m_k } \overset{\text{def}}{=} p[ m_1 / x_1, \dots, m_k / x_k ] \mid m_i \in \mathbb{N} \}
    $$

## 递归定义

**显式递归**：

- 在研究 CCS 时，有时方便使用显式的递归定义来替代定义方程。

- 使用形式：

  $$
  \text{rec}( P = p )
  $$

- **规则**：

  $$
  \frac{p[ \text{rec}( P = p ) / P ] \xrightarrow{\lambda} q}{\text{rec}( P = p ) \xrightarrow{\lambda} q}
  $$

  - **解释**：在 $p$ 中用 $\text{rec}( P = p )$ 替换 $P$，然后应用规则。

**示例**：

- 考虑进程：

  $$
  \text{rec}( P = a . b . P )
  $$

- **操作语义推导**：

  - 首先展开递归定义：

    $$
    \text{rec}( P = a . b . P ) \xrightarrow{a} b . \text{rec}( P = a . b . P )
    $$

  - 然后继续推导。

## 练习

**练习14.5**

**问题**：

- 证明转换表的正确性，展示对于闭合的进程项 $p, q$，如果 $p$ 转换为 $p'$，$q$ 转换为 $q'$，则 $p$ 和 $q$ 的行为一致。

**解答思路**：

- 展示转换前后的进程在操作语义下的转换是对应的。

**练习14.6**

**问题**：

- 使用操作语义推导从进程项 $\text{rec}( P = a . b . P )$ 可达的转换系统。

**解答思路**：

- 展开递归定义，逐步推导可能的转换。

- 例如：

  - $\text{rec}( P = a . b . P ) \xrightarrow{a} b . \text{rec}( P = a . b . P )$

  - $b . \text{rec}( P = a . b . P ) \xrightarrow{b} \text{rec}( P = a . b . P )$

  - 形成一个循环。

**练习14.7**

**问题**：

- 给定另一个进程语言，其语法如下：

  $$
  p ::= 0 \mid a \mid p; q \mid p + q \mid p \times q \mid \text{rec}( P = p )
  $$

  - 其中 $a$ 是动作符号。

- 执行关系 $p \rightarrow s$，表示进程 $p$ 可以执行动作序列 $s$。

- 要求：

  (i) 写出一个闭合的进程项，其含义为语言 $\{ a, b \}^*$。

  (ii) 证明对于所有闭合的进程项 $p$ 和 $q$，以及环境 $p$，有：

  $$
  [ p[ q / P ] ]_p = [ p ]_p [ [ q ]_p / P ]
  $$

  (iii) 证明如果 $p \rightarrow s$，则 $s \in [ p ]_p$。

**解答思路**：

- (i) 构造一个能够生成 $\{ a, b \}^*$ 的进程。

  - 例如：

    $$
    \text{rec}( P = a; P + b; P + 0 )
    $$

- (ii) 使用结构归纳法证明等式。

- (iii) 通过归纳证明执行关系与指称语义的一致性。

---

# 总结

在本节中，我们深入探讨了 Milner 的通信系统演算（CCS），了解了其基本概念、语法、操作语义以及如何消除变量，得到更基础的纯 CCS。通过详细的解释和示例，我们看到了如何使用 CCS 来形式化地描述并分析并发系统中的进程通信和同步行为。

希望这些详细的讲解能够帮助你更好地理解 CCS 的概念和应用。如果你有任何疑问，欢迎继续提问！

### ----------------------------

# 14.7 模态 $\mu$-演算

## 引言

在上一节（14.6）中，我们引入了一种用于描述进程行为的规范语言。本节将对该规范语言进行形式化处理，这被称为**模态 $\mu$-演算（modal $\mu$-calculus）**。

## 进程和断言

首先，让我们定义一些基本概念。

### 进程集合 $P$

- 令 $P$ 表示**纯 CCS（Calculus of Communicating Systems）**中的所有进程的集合。
- 纯 CCS 是一种用于描述并发系统的形式语言。

### 断言（Assertions）

- **断言**用于确定进程的属性。
- **属性（Property）**：对于一个进程，要么为真，要么为假。
- 因此，一个属性可以被视为使该属性为真的进程的**子集**。

**重要概念**：

- **断言作为进程集合的描述**：断言可以简单地理解为描述进程子集的符号。
- **断言的作用**：断言用于描述哪些进程满足某种特定的性质。

## 断言的构造

断言是通过以下方式构造的：

### 1. 常量（Constants）

- **定义**：任何进程的子集 $S \subseteq P$ 都被视为一个常量断言。
- **解释**：对于包含在 $S$ 中的进程，断言为真；否则为假。
- **示例**：
  - **特定的描述**：例如，我们可以使用之前定义的 `terminal` 和 `Dead` 等有限描述。

### 2. 逻辑连接词（Logical Connectives）

- **特殊常量**：
  - $T$：表示恒为真的断言（True）。
  - $F$：表示恒为假的断言（False）。
- **构造**：
  - **否定**：如果 $A$ 是一个断言，那么 $\neg A$（“非 $A$”）也是一个断言。
  - **合取**：如果 $A$ 和 $B$ 是断言，那么 $A \land B$（“$A$ 且 $B$”）也是一个断言。
  - **析取**：如果 $A$ 和 $B$ 是断言，那么 $A \lor B$（“$A$ 或 $B$”）也是一个断言。

### 3. 模态算子（Modalities）

- **动作符号**：$\alpha$ 是一个动作符号（action symbol）。
- **构造**：
  - **菱形模态**：如果 $A$ 是一个断言，那么 $( \alpha ) A$ 是一个断言。
    - **解释**：进程能够执行动作 $\alpha$，并到达一个满足 $A$ 的进程。
  - **通用菱形模态**：如果 $A$ 是一个断言，那么 $( . ) A$ 是一个断言。
    - **解释**：进程能够执行**某个**动作，并到达一个满足 $A$ 的进程。
- **盒子模态（Box Modalities）**：
  - **定义**：
    - $[ \alpha ] A \ \overset{\text{def}}{=} \ \neg ( \alpha ) \neg A$
    - $[ . ] A \ \overset{\text{def}}{=} \ \neg ( . ) \neg A$
  - **解释**：$[ \alpha ] A$ 表示进程在执行 $\alpha$ 动作后，必定到达一个满足 $A$ 的进程。

### 4. 最大不动点（Maximum Fixed Points）

- **定义**：如果在断言 $A$ 中，变量 $X$ 只以**正极性（positively）**出现（即，每次出现都在偶数个否定符号之下），那么 $\nu X . A$ 是一个断言。
  - **$\nu X . A$**：$A$ 的最大不动点。
- **最小不动点（Minimum Fixed Points）**：
  - **定义**：$\mu X . A$ 可以理解为 $\neg \nu X . \neg A[ \neg X / X ]$ 的简写。

**注释**：

- **变量的正极性**：意味着变量 $X$ 不出现在任何奇数个否定符号下，这保证了某些性质，如单调性。

---

## 断言的大小（Size of Assertions）

在推理断言时，我们经常需要使用它们的**大小**。

### 定义

- **大小的递归定义**：
  - 常量：
    $$
    \text{size}( S ) = \text{size}( T ) = \text{size}( F ) = 0
    $$
    - 其中，$S$ 是一个常量断言。
  - 复合断言：
    - 否定、模态、最大不动点：
      $$
      \text{size}( \neg A ) = \text{size}( ( \alpha ) A ) = \text{size}( \nu X . A ) = 1 + \text{size}( A )
      $$
    - 合取、析取：
      $$
      \text{size}( A \land B ) = \text{size}( A \lor B ) = 1 + \text{size}( A ) + \text{size}( B )
      $$

### 作用

- **用途**：在归纳证明中，通过断言的大小来建立递归关系。

---

## 断言对应的进程集合

断言描述了进程的子集，具体对应关系如下：

- **常量断言**：
  $$
  S \quad \text{对应} \quad S \subseteq P
  $$
- **特殊常量**：
  $$
  T \quad \text{对应} \quad P \\
  F \quad \text{对应} \quad \emptyset
  $$
- **逻辑连接词**：
  - 合取：
    $$
    A \land B \quad \text{对应} \quad A \cap B
    $$
  - 析取：
    $$
    A \lor B \quad \text{对应} \quad A \cup B
    $$
  - 否定：
    $$
    \neg A \quad \text{对应} \quad P \setminus A
    $$
- **模态算子**：
  - 菱形模态：
    $$
    ( \alpha ) A \quad \text{对应} \quad \{ p \in P \mid \exists q . \ p \xrightarrow{ \alpha } q \ \text{且} \ q \in A \}
    $$
  - 通用菱形模态：
    $$
    ( . ) A \quad \text{对应} \quad \{ p \in P \mid \exists \alpha , q . \ p \xrightarrow{ \alpha } q \ \text{且} \ q \in A \}
    $$
- **最大不动点**：
  $$
  \nu X . A \quad \text{对应} \quad \bigcup \{ S \subseteq P \mid S \subseteq A [ S / X ] \}
  $$
  - **解释**：$\nu X . A$ 是函数 $S \mapsto A [ S / X ]$ 的所有后定点（post-fixed point）的并集。

### 注释

- **后定点**：对于函数 $f$，集合 $S$ 满足 $S \subseteq f(S)$，则称 $S$ 是 $f$ 的后定点。
- **变量替换**：$A [ S / X ]$ 表示在断言 $A$ 中，用集合 $S$ 替换所有出现的 $X$。

---

## 断言的定义合理性

- 这是一个良好的定义，因为断言对应的集合是基于**严格较小大小**的断言对应的集合定义的。
- **递归性**：定义中，每个断言的集合都是基于其子断言的集合定义的。

### 示例解释

- **否定**：$\neg A$ 对应于 $P \setminus A$，即所有不满足 $A$ 的进程集合。
- **菱形模态**：$( \alpha ) A$ 对应于可以执行 $\alpha$ 动作并到达满足 $A$ 的进程的集合。

---

## 单调性和 Knaster-Tarski 定理

### 单调性

- 如果变量 $X$ 在 $A$ 中仅以正极性出现，那么函数：
  $$
  S \mapsto A [ S / X ]
  $$
  是关于包含关系 $\subseteq$ 的单调函数。

### Knaster-Tarski 定理

- **定理内容**：在完备格中，单调函数的最大不动点等于其所有后定点的并集。
- **应用**：$\nu X . A$ 是函数 $S \mapsto A [ S / X ]$ 的所有后定点的并集。

---

## 最小不动点的定义

- **最小不动点** $\mu X . A$ 可以理解为：
  $$
  \mu X . A = \bigcap \{ S \subseteq P \mid A [ S / X ] \subseteq S \}
  $$
  - **解释**：$\mu X . A$ 是函数 $S \mapsto A [ S / X ]$ 的所有**前定点（pre-fixed point）**的交集。

- **关系**：$\mu X . A$ 可以表示为：
  $$
  \mu X . A = \neg \nu X . \neg A [ \neg X / X ]
  $$
  - **证明**：见练习14.10。

---

## 练习14.10

**问题**：

证明最小不动点 $\mu X . A$，其中
$$
\mu X . A = \bigcap \{ S \subseteq P \mid A [ S / X ] \subseteq S \}
$$
等于 $\neg \nu X . \neg A [ \neg X / X ]$。

**提示**：

- 显示**否定操作**在函数 $S \mapsto A [ S / X ]$ 的前定点和函数 $S \mapsto \neg A [ \neg S / X ]$ 的后定点之间提供了一一对应。

**解答思路**：

- **目标**：证明 $\mu X . A = \neg \nu X . \neg A [ \neg X / X ]$。
- **步骤**：
  1. 考虑函数 $f(S) = A[S/X]$ 的前定点集合。
  2. 考虑函数 $g(S) = \neg A[ \neg S / X ]$ 的后定点集合。
  3. 显示 $S$ 是 $f$ 的前定点，当且仅当 $\neg S$ 是 $g$ 的后定点。
  4. 由此，$\mu X . A = \neg \nu X . \neg A[ \neg X / X ]$。

---

## 练习14.11

**问题**：

- 证明 $[ \alpha ] A = \{ p \in P \mid \forall q \in P, \ p \xrightarrow{ \alpha } q \implies q \in A \}$。
- 通过考虑一个过程 $\sum_{ n \in \omega } a . P_n$，其中 $P_n$ 是不同的，展示函数 $S \mapsto [ \alpha ] S$ 关于包含关系不是连续的（尽管它是单调的）。

**解答思路**：

- **第一部分**：证明 $[ \alpha ] A$ 的定义。
  - 根据 $[ \alpha ] A = \neg ( \alpha ) \neg A$。
  - $( \alpha ) \neg A = \{ p \in P \mid \exists q . p \xrightarrow{ \alpha } q \ \text{且} \ q \in \neg A \}$。
  - 所以 $\neg ( \alpha ) \neg A = \{ p \in P \mid \forall q . p \xrightarrow{ \alpha } q \implies q \in A \}$。

- **第二部分**：展示函数 $S \mapsto [ \alpha ] S$ 不是连续的。
  - 考虑一个过程 $\sum_{ n \in \omega } a . P_n$，其中 $P_n$ 是无限多个不同的进程。
  - 由于无限性，包含关系的无限上极限可能不等于函数值的上极限，说明了不连续性。

---

## 进程满足断言

### 定义

- **满足关系**：定义进程 $p$ **满足**断言 $A$，记为 $p \models A$，当且仅当 $p \in A$。
- **否则**：$p \not\models A$。

### 意义

- 这使得断言 $A$ 可以直接被视为满足该断言的进程集合。

---

## 自动验证有限状态进程的断言

### 可行性

- 对于**有限状态进程** $P$，可以自动检查其是否满足断言 $A$。
- **工具**：并发工作台（Concurrency Workbench）/TAV 系统的命令可以检查进程 $P$ 是否满足断言 $A$。

### 可达进程集合 $P_p$

- **定义**：
  $$
  P_p = \{ q \in P \mid p \xrightarrow{ * } q \}
  $$
  - 其中，$p \xrightarrow{ * } q$ 表示从 $p$ 可以经过零个或多个动作到达 $q$。

### 重要性

- 由于 $P_p$ 是有限的，我们只需要考虑 $P_p$ 中的进程和它们的属性。

---

## 局部定义断言

为了在有限状态进程的上下文中定义断言，我们使用**局部定义**，即在 $P_p$ 上定义断言。

### 定义

- 对于 $S \subseteq P$，定义：
  $$
  S \vert_p = S \cap P_p
  $$
- 其他断言的定义类似，但只在 $P_p$ 上考虑。

### 具体定义

- **常量断言**：
  $$
  S \vert_p = S \cap P_p
  $$
- **特殊常量**：
  $$
  T \vert_p = P_p \\
  F \vert_p = \emptyset
  $$
- **逻辑连接词**：
  - 合取：
    $$
    ( A \land B ) \vert_p = A \vert_p \cap B \vert_p
    $$
  - 析取：
    $$
    ( A \lor B ) \vert_p = A \vert_p \cup B \vert_p
    $$
  - 否定：
    $$
    ( \neg A ) \vert_p = P_p \setminus ( A \vert_p )
    $$
- **模态算子**：
  - 菱形模态：
    $$
    ( \alpha ) A \vert_p = \{ r \in P_p \mid \exists q \in P_p . \ r \xrightarrow{ \alpha } q \ \text{且} \ q \in A \vert_p \}
    $$
  - 通用菱形模态：
    $$
    ( . ) A \vert_p = \{ r \in P_p \mid \exists \alpha , q \in P_p . \ r \xrightarrow{ \alpha } q \ \text{且} \ q \in A \vert_p \}
    $$
- **最大不动点**：
  $$
  \nu X . A \vert_p = \bigcup \{ S \subseteq P_p \mid S \subseteq A [ S / X ] \vert_p \}
  $$

---

## 引理14.12

**陈述**：

对于所有的断言 $A$ 和进程 $p$，有：
$$
A \vert_p = A \cap P_p
$$

**证明**：

**目标**：证明对于任意断言 $A$，有 $A \vert_p = A \cap P_p$。

**步骤**：

1. **观察**：

   - 对于所有断言 $A$，有：
     $$
     A [ S / X ] \vert_p = A [ S \cap P_p / X ] \vert_p
     $$
   - **解释**：在 $A$ 中，用 $S$ 替换 $X$，然后在 $P_p$ 上取截断，等于在 $A$ 中用 $S \cap P_p$ 替换 $X$，然后在 $P_p$ 上取截断。

2. **归纳证明**：

   - 对于断言的大小，进行数学归纳。

3. **特殊情况**：最大不动点。

   - **需要证明**：
     $$
     \nu X . A \vert_p = ( \nu X . A ) \cap P_p
     $$
   - **假设**：对 $A$，归纳假设成立。

4. **证明方向1**：$\nu X . A \vert_p \subseteq ( \nu X . A ) \cap P_p$

   - 设 $S' \subseteq P_p$，且 $S' \subseteq A [ S' / X ] \vert_p$。
   - 由归纳假设，$S' \subseteq A [ S' / X ] \cap P_p$。
   - 因此，$S'$ 是函数 $S \mapsto A [ S / X ]$ 的后定点，即 $S' \subseteq \nu X . A$。
   - 因此，$\nu X . A \vert_p \subseteq \nu X . A$。

5. **证明方向2**：$( \nu X . A ) \cap P_p \subseteq \nu X . A \vert_p$

   - 设 $S \subseteq P$，且 $S \subseteq A [ S / X ]$。
   - 则 $S \cap P_p \subseteq A [ S / X ] \cap P_p$。
   - 由归纳假设，$A [ S / X ] \vert_p = A [ S \cap P_p / X ] \vert_p$。
   - 因此，$S \cap P_p \subseteq A [ S \cap P_p / X ] \vert_p$。
   - 因此，$S \cap P_p$ 是 $\nu X . A \vert_p$ 的后定点。
   - 因此，$( \nu X . A ) \cap P_p \subseteq \nu X . A \vert_p$。

6. **结论**：

   - $\nu X . A \vert_p = ( \nu X . A ) \cap P_p$。

---

## 最大不动点的消除

- 由于 $P_p$ 是有限集合（大小为 $n$），因此我们可以通过**有限次迭代**来计算 $\nu X . A \vert_p$。
- **计算方法**：
  $$
  \nu X . A \vert_p = \bigcup_{ 0 \leq i \leq n } A_i [ T / X ] \vert_p
  $$
  - 其中，$A_0 = T$，$A_{ i + 1 } = A [ A_i / X ]$。

- **解释**：

  - 由于函数 $S \mapsto A [ S / X ] \vert_p$ 是关于包含关系的单调连续函数，我们可以通过迭代计算其最大不动点。

---

## 检查进程是否满足断言的方法

- **思路**：通过消除断言中的最大不动点，将问题分解为检查更小的断言是否被进程满足。
- **步骤**：

  1. **消除最大不动点**：通过迭代计算，将 $\nu X . A$ 展开为有限的合取或析取形式。
  2. **递归检查**：对于形如 $( \alpha ) B$ 的结果，检查是否存在进程 $q$，使得 $P \xrightarrow{ \alpha } q$，且 $q \models B$。
  3. **合取处理**：对于 $B \land C$，分别检查 $P \models B$ 和 $P \models C$。
  4. **终止条件**：最终需要检查进程是否满足常量断言（如 $T$、$F$ 或特定的进程集合）。

- **注意**：由于最大不动点的消除可能导致断言大小的指数增长，因此在实践中需要优化算法。

---

## 本地模型检查

- **目的**：寻找一种不需要完全展开最大不动点的方法，称为**本地模型检查（local model checking）**。
- **优势**：对断言的结构更加敏感，避免了最大不动点的完全计算。
- **应用**：这是 Concurrency Workbench 和 TAV 系统中算法的基础。

---

# 练习解答

## 练习14.13

### (i) 证明最小和最大不动点的表示

**问题**：

- 设 $S$ 是大小为 $k$ 的有限集合，$\Phi : \text{Pow}( S ) \rightarrow \text{Pow}( S )$ 是单调算子。
- 证明：
  $$
  \mu x . \Phi( X ) = \bigcup_{ n \in \omega } \Phi^n( \emptyset )
  $$
  $$
  \nu X . \Phi( X ) = \bigcap_{ n \in \omega } \Phi^n( S )
  $$

**解答**：

- **解释**：

  - $\mu x . \Phi( X )$ 是 $\Phi$ 的最小不动点，可以通过从空集开始迭代 $\Phi$ 直到达到不动点。
  - $\nu X . \Phi( X )$ 是 $\Phi$ 的最大不动点，可以通过从全集 $S$ 开始逆向迭代 $\Phi$。

- **证明 $\mu x . \Phi( X )$ 部分**：

  1. **定义**：$\Phi^0( \emptyset ) = \emptyset$，$\Phi^{ n + 1 }( \emptyset ) = \Phi( \Phi^n( \emptyset ) )$。
  2. **构造递增序列**：$\emptyset \subseteq \Phi( \emptyset ) \subseteq \Phi^2( \emptyset ) \subseteq \dots$。
  3. **由于 $S$ 是有限的**，序列在有限步后稳定，即存在 $N$ 使得 $\Phi^{ N }( \emptyset ) = \Phi^{ N + 1 }( \emptyset )$。
  4. **因此**，$\mu x . \Phi( X ) = \Phi^N( \emptyset ) = \bigcup_{ n = 0 }^{ N } \Phi^n( \emptyset )$。

- **证明 $\nu X . \Phi( X )$ 部分**：

  1. **定义**：$\Phi^0( S ) = S$，$\Phi^{ n + 1 }( S ) = \Phi( \Phi^n( S ) )$。
  2. **构造递减序列**：$S \supseteq \Phi( S ) \supseteq \Phi^2( S ) \supseteq \dots$。
  3. **由于 $S$ 是有限的**，序列在有限步后稳定，即存在 $M$ 使得 $\Phi^{ M }( S ) = \Phi^{ M + 1 }( S )$。
  4. **因此**，$\nu X . \Phi( X ) = \Phi^M( S ) = \bigcap_{ n = 0 }^{ M } \Phi^n( S )$。

### (ii) 进程 $P$ 满足 $\nu X . ( ( \alpha ) X )$ 当且仅当 $P$ 可以执行无限的 $\alpha$-转换链

**问题**：

- 设 $P$ 是有限状态进程。
- 证明 $P \models \nu X . ( ( \alpha ) X )$ 当且仅当 $P$ 可以执行无限的 $\alpha$-转换链。

- **另外**，$\mu X . ( ( \alpha ) X )$ 表示什么？证明之。

**解答**：

- **证明 $P \models \nu X . ( ( \alpha ) X )$ 当且仅当 $P$ 可以执行无限的 $\alpha$-转换链**：

  1. **含义**：
     - $\nu X . ( ( \alpha ) X )$ 的直观含义是：从 $P$ 开始，可以无限次执行 $\alpha$ 动作，始终停留在集合 $X$ 中。

  2. **方向1**：若 $P$ 可以执行无限的 $\alpha$-转换链，则 $P \models \nu X . ( ( \alpha ) X )$。

     - **构造**：由于 $P$ 可以无限次执行 $\alpha$，因此对于任意$n$，存在 $\alpha$-转换序列 $P \xrightarrow{ \alpha } P_1 \xrightarrow{ \alpha } \dots \xrightarrow{ \alpha } P_n$。
     - **迭代定义**：
       - 构造递减序列 $S_n = \{ P_n, P_{ n + 1 }, \dots \}$。
     - **由于 $P$ 是有限状态进程**，因此序列最终稳定，得到不动点。
     - **因此**，$P \in \nu X . ( ( \alpha ) X )$。

  3. **方向2**：若 $P \models \nu X . ( ( \alpha ) X )$，则 $P$ 可以执行无限的 $\alpha$-转换链。

     - **假设反证**：假设 $P$ 不能执行无限的 $\alpha$-转换链，则存在最长的 $\alpha$-转换序列，长度为 $n$。
     - **因此**，在第 $n$ 步后，进程无法再执行 $\alpha$ 动作。
     - **由此**，在迭代过程中，$S_n$ 将不再包含新的元素，无法满足 $S \subseteq ( \alpha ) S$，与 $\nu X . ( ( \alpha ) X )$ 的定义矛盾。
     - **因此**，$P$ 必须能够执行无限的 $\alpha$-转换链。

- **$\mu X . ( ( \alpha ) X )$ 表示什么？**

  - **解释**：$\mu X . ( ( \alpha ) X )$ 的最小不动点表示**无法执行 $\alpha$ 动作的进程集合**。
  - **证明**：
    1. **初始化**：$S_0 = \emptyset$。
    2. **迭代**：$S_{ n + 1 } = ( \alpha ) S_n$。
    3. **由于 $S_0 = \emptyset$**，且 $( \alpha ) \emptyset = \emptyset$，因此所有 $S_n = \emptyset$。
    4. **因此**，$\mu X . ( ( \alpha ) X ) = \emptyset$。
    5. **解释**：只有无法执行 $\alpha$ 动作的进程才能满足此断言。

### (iii) 证明断言 $\nu X . ( A \land [ . ] X )$ 被那些**始终满足**断言 $A$ 的进程 $p$ 所满足

**问题**：

- 证明断言 $\nu X . ( A \land [ . ] X )$ 被那些对于所有 $q \in P_p$ 都满足 $A$ 的进程 $p$ 所满足。

**解答**：

- **含义**：

  - $\nu X . ( A \land [ . ] X )$ 表示**所有可达进程都满足 $A$**。

- **证明**：

  1. **定义**：$S = \nu X . ( A \land [ . ] X )$。
  2. **性质**：
     - $S = \bigcap_{ n } S_n$，其中 $S_0 = P_p$，$S_{ n + 1 } = \{ p \in S_n \mid p \models A \text{ 且 } \forall q . p \xrightarrow{ \alpha } q \implies q \in S_n \}$。
  3. **初始**：$S_0 = P_p$。
  4. **迭代**：在每一步，删除不满足 $A$ 或者有转移到 $S_n$ 之外的进程。
  5. **因此**，$S$ 包含所有在 $P_p$ 中**始终满足** $A$ 的进程。
  6. **结论**：$p$ 满足 $\nu X . ( A \land [ . ] X )$，当且仅当 $p$ 的所有可达进程都满足 $A$。

### (iv) 如何在模态 $\nu$-演算中表达那些**最终到达**满足 $A$ 的状态的进程

**问题**：

- 在模态 $\nu$-演算中，如何表达精确满足“最终到达满足 $A$ 的状态”的进程的属性？

**解答**：

- **断言**：

  - **定义**：
    $$
    \text{eventually}( A ) \ \overset{\text{def}}{=} \ \mu X . ( A \lor ( ( . ) T \land [ . ] X ) )
    $$
  - **解释**：进程要么现在就满足 $A$，要么可以执行某些动作后，仍满足此属性。

- **证明**：

  1. **初始**：$S_0 = \emptyset$。
  2. **迭代**：
     - $S_{ n + 1 } = A \cup \{ p \mid \exists \alpha , q . p \xrightarrow{ \alpha } q \text{ 且 } q \in S_n \}$
  3. **过程**：
     - 每一步，加入能够通过一步转移到达 $S_n$ 中的进程。
  4. **结果**：
     - $S = \mu X . ( A \lor ( ( . ) T \land [ . ] X ) )$ 包含所有能够最终到达满足 $A$ 的状态的进程。

---

**提示**：在练习14.15中有类似的定义。

---

## 练习14.14

### (i) 将“直到（until）”操作符表示为最大不动点断言

**问题**：

- **直到操作符**：进程 $P$ 满足 $A$ **直到** $B$，当且仅当对于所有转移序列 $P = P_0 \rightarrow P_1 \rightarrow \dots \rightarrow P_n$，满足：
  - 对于所有 $0 \leq i \leq n$，$P_i \models A$，或者存在某个 $i$，使得 $P_i \models B$，并且对于所有 $0 \leq j \leq i$，$P_j \models A$。

- **任务**：将“直到”操作符表示为最大不动点断言。

**解答**：

- **断言定义**：

  - 定义：
    $$
    \text{un}( A, B ) \ \overset{\text{def}}{=} \ \nu X . ( B \lor ( A \land [ . ] X ) )
    $$
  - **解释**：要么当前满足 $B$，要么当前满足 $A$，并且所有后继状态也满足 $X$。

- **证明**：

  - 这与练习14.15中的 (iii) 部分一致。

### (ii) 断言 $\mu X . ( B \lor ( A \land ( . ) T \land [ . ] X ) )$ 的含义

**问题**：

- 解释断言：
  $$
  \mu X . ( B \lor ( A \land ( . ) T \land [ . ] X ) )
  $$
- 这被称为“强直到（strong-until）”。

**解答**：

- **含义**：

  - 该断言表示存在一条路径，使得：
    - **最终**到达一个满足 $B$ 的状态。
    - 在此之前的所有状态都满足 $A$。
    - 所有转移都是通过执行某个动作（即，没有停滞）。

- **解释**：

  - $\mu$ 表示最小不动点，强调**存在性**。
  - $( A \land ( . ) T \land [ . ] X )$ 表示当前状态满足 $A$，可以执行某个动作，并且所有执行动作后的状态满足 $X$。
  - 因此，存在一条路径，沿着该路径：
    - 每个状态满足 $A$，并有后继状态。
    - 最终到达满足 $B$ 的状态。

---

## 练习14.15

**问题**：

- 解释以下断言的含义，它们涉及断言 $A$ 和 $B$。

### (i) $ \text{inv}( A ) \ \overset{\text{def}}{=} \ \nu X . ( A \land [ . ] X ) $

**解答**：

- **含义**：

  - **不变性（invariance）**：进程 $P$ **始终**满足 $A$，即对于所有可达状态，都满足 $A$。
  - **解释**：定义与练习14.13 (iii) 部分相同。

### (ii) $ \text{eV}( A ) \ \overset{\text{def}}{=} \ \mu X . ( A \lor ( ( . ) T \land [ . ] X ) ) $

**解答**：

- **含义**：

  - **最终性（eventually）**：进程 $P$ **最终**会到达一个满足 $A$ 的状态。
  - **解释**：与练习14.13 (iv) 部分一致。

### (iii) $ \text{un}( A, B ) \ \overset{\text{def}}{=} \ \nu X . ( B \lor ( A \land [ . ] X ) ) $

**解答**：

- **含义**：

  - **直到（until）**：进程 $P$ **一直**满足 $A$，直到满足 $B$。
  - **解释**：与练习14.14 (i) 部分一致。

---

## 练习14.16

### (i) 表达进程对于动作 $\alpha$ **不公平（unfair）**的属性，并证明对于有限状态进程 $P$，当且仅当 $P$ 对 $\alpha$ 不公平时，满足该断言

**问题**：

- **定义**：进程 $P$ 对于动作 $\alpha$ **不公平**，如果存在无限的转移序列：
  $$
  P = P_0 \xrightarrow{ a_0 } P_1 \xrightarrow{ a_1 } \dots \xrightarrow{ a_n } P_n \xrightarrow{ } \dots
  $$
  - 满足：
    - 对所有 $i \geq 0$，存在 $q$，使得 $P_i \xrightarrow{ \alpha } q$。
    - 对所有 $i \geq 0$，$a_i \ne \alpha$。

- **任务**：
  1. 在模态 $\nu$-演算中表达进程 $P$ 对于动作 $\alpha$ **不公平**的属性。
  2. 证明对于有限状态进程 $P$，当且仅当 $P$ 对 $\alpha$ 不公平时，$P$ 满足该断言。

**解答**：

- **断言定义**：

  - 定义：
    $$
    \text{unfair}( \alpha ) \ \overset{\text{def}}{=} \ \nu X . ( ( \neg ( \alpha ) T ) \land ( ( . ) T ) \land [ . ] X \land ( ( \alpha ) T ) )
    $$
  - **解释**：
    - $( \neg ( \alpha ) T )$：当前状态**不能**执行 $\alpha$ 动作。
    - $( ( . ) T )$：当前状态可以执行某个动作（没有死锁）。
    - $[ . ] X$：所有后继状态都满足 $X$。
    - $( ( \alpha ) T )$：存在 $\alpha$ 动作可以执行（即，总是存在 $\alpha$ 可以执行）。

- **证明**：

  1. **方向1**：若 $P$ 对于 $\alpha$ 不公平，则 $P \models \text{unfair}( \alpha )$。

     - 由于 $P$ 可以无限地不执行 $\alpha$，但始终存在 $\alpha$ 可执行，并且没有死锁。
     - 因此，$P$ 满足断言的所有条件。

  2. **方向2**：若 $P \models \text{unfair}( \alpha )$，则 $P$ 对于 $\alpha$ 不公平。

     - 由于 $P$ 满足断言，意味着存在无限的路径，不执行 $\alpha$，但始终可以执行 $\alpha$。
     - 因此，$P$ 对于 $\alpha$ 不公平。

### (ii) 表达进程对于动作 $\alpha$ **弱不公平（weakly unfair）**的属性

**问题**：

- **定义**：进程 $P$ 对于动作 $\alpha$ **弱不公平**，如果存在无限的转移序列，其中 $\alpha$ 可以无限次出现但从未实际执行。

- **任务**：在模态 $\nu$-演算中表达该属性。

**解答**：

- **断言定义**：

  - 定义：
    $$
    \text{weakly\_unfair}( \alpha ) \ \overset{\text{def}}{=} \ \nu X . ( ( \neg ( \alpha ) T ) \land ( ( . ) T ) \land [ . ] X \land \nu Y . ( ( \alpha ) T \land [ . ] Y ) )
    $$
  - **解释**：
    - 与不公平的定义类似，但强调 $\alpha$ 可以无限次出现。
    - 使用嵌套的最大不动点来表示 $\alpha$ 可以无限次出现。

---

# 总结

通过本节的学习，我们深入了解了模态 $\mu$-演算的形式化定义，以及如何使用它来描述和验证进程的属性。我们讨论了断言的构造、对应的进程集合，以及如何在有限状态进程的上下文中进行模型检查。此外，通过练习，我们实践了如何表达复杂的进程属性，如不变性、最终性和不公平性。

希望这些详细的讲解能够帮助你更好地理解本章的内容。如果你有任何疑问，欢迎继续提问！

### ---------------------------

# 14.8 本地模型检查

## 引言

在这一节中，我们关注于**有限状态进程（finite-state processes）** $p$ 是否满足一个**递归模态断言（recursive modal assertion）** $A$，即决定 $p \models A$ 的真或假。我们的目标是提供一种算法，将这样的满足断言归约为真或假。

为此，我们需要一个关键的引理，即**归约引理（Reduction Lemma）**，它源自于第 5.5 节的 Knaster-Tarski 定理。

---

## 归约引理（Lemma 14.17）

### 引理陈述

**引理 14.17（归约引理）**：

设 $\Phi$ 是定义在幂集 $\text{Pow}( S )$ 上的单调函数，对于 $S \subseteq S$，有：

$$
S \subseteq \nu X . \Phi( X ) \iff S \subseteq \Phi\left( \nu X . \left( S \cup \Phi( X ) \right) \right)
$$

- **符号说明**：
  - $\nu X . \Phi( X )$ 表示函数 $\Phi$ 的**最大不动点（maximum fixed point）**。
  - $\Phi$ 是从 $\text{Pow}( S )$ 到 $\text{Pow}( S )$ 的单调函数。

### 引理解释

- **左向右（“$\implies$”）**：

  - 如果集合 $S$ 被包含在 $\Phi$ 的最大不动点中，那么 $S$ 也被包含在一个特定的展开形式中。

- **右向左（“$\impliedby$”）**：

  - 如果 $S$ 被包含在 $\Phi$ 作用于一个包含 $S$ 的特定集合上的结果中，那么 $S$ 也被包含在 $\Phi$ 的最大不动点中。

### 引理证明

**证明**：

我们将分别证明两个方向。

#### 方向一：$S \subseteq \nu X . \Phi( X ) \implies S \subseteq \Phi\left( \nu X . \left( S \cup \Phi( X ) \right) \right)$

**解释**：

- **前提条件**：$S \subseteq \nu X . \Phi( X )$。
- **目标**：证明 $S \subseteq \Phi\left( \nu X . \left( S \cup \Phi( X ) \right) \right)$。

**证明步骤**：

1. **利用前提**：

   $$
   S \cup \Phi\left( \nu X . \Phi( X ) \right) = S \cup \nu X . \Phi( X ) = \nu X . \Phi( X )
   $$

   - **解释**：因为 $S \subseteq \nu X . \Phi( X )$，所以并集等于 $\nu X . \Phi( X )$。

2. **证明 $\nu X . \Phi( X )$ 是函数 $X \mapsto S \cup \Phi( X )$ 的后定点（post-fixed point）**：

   - 因为：
     $$
     \nu X . \Phi( X ) = S \cup \Phi\left( \nu X . \Phi( X ) \right)
     $$

3. **由于 $\nu X . \left( S \cup \Phi( X ) \right)$ 是 $X \mapsto S \cup \Phi( X )$ 的**最大**后定点**：

   $$
   \nu X . \Phi( X ) \subseteq \nu X . \left( S \cup \Phi( X ) \right)
   $$

4. **利用单调性**：

   - 由于 $\Phi$ 是单调的，且：
     $$
     \Phi\left( \nu X . \Phi( X ) \right) \subseteq \Phi\left( \nu X . \left( S \cup \Phi( X ) \right) \right)
     $$

5. **结合以上结果**：

   - 由 $S \subseteq \nu X . \Phi( X )$，得：
     $$
     S \subseteq \Phi\left( \nu X . \left( S \cup \Phi( X ) \right) \right)
     $$

#### 方向二：$S \subseteq \Phi\left( \nu X . \left( S \cup \Phi( X ) \right) \right) \implies S \subseteq \nu X . \Phi( X )$

**解释**：

- **前提条件**：$S \subseteq \Phi\left( \nu X . \left( S \cup \Phi( X ) \right) \right)$。
- **目标**：证明 $S \subseteq \nu X . \Phi( X )$。

**证明步骤**：

1. **注意到 $\nu X . \left( S \cup \Phi( X ) \right)$ 是 $X \mapsto S \cup \Phi( X )$ 的不动点**：

   - 因为：
     $$
     \nu X . \left( S \cup \Phi( X ) \right) = S \cup \Phi\left( \nu X . \left( S \cup \Phi( X ) \right) \right)
     $$

2. **根据前提条件**：

   - $\nu X . \left( S \cup \Phi( X ) \right) = \Phi\left( \nu X . \left( S \cup \Phi( X ) \right) \right)$。

3. **因此**：

   - $\nu X . \left( S \cup \Phi( X ) \right)$ 是 $\Phi$ 的不动点。

4. **由于 $\nu X . \Phi( X )$ 是 $\Phi$ 的最大不动点**：

   $$
   \nu X . \left( S \cup \Phi( X ) \right) \subseteq \nu X . \Phi( X )
   $$

5. **结合以上结果**：

   - 由于 $S \subseteq \nu X . \left( S \cup \Phi( X ) \right)$，因此：
     $$
     S \subseteq \nu X . \Phi( X )
     $$

**结论**：

- 证明了引理的两个方向，故引理成立。

---

## 引理的应用

我们特别关注于当 $S$ 是单元素集合 $\{ p \}$ 的情况，此时引理变为：

$$
p \in \nu X . \Phi( X ) \iff p \in \Phi\left( \nu X . \left( \{ p \} \cup \Phi( X ) \right) \right)
$$

- **解释**：进程 $p$ 满足递归定义的属性，当且仅当 $p$ 满足该属性的某种展开形式。

- **展开的特殊性**：在递归体中，我们替换的不仅是原始的递归定义，而是将递归体扩展为包含 $p$。

- **意义**：这种小的修改使得验证 $p \in \Phi( \dots )$ 比验证 $p \in \nu X . \Phi( X )$ 更容易，从而提供了一种决定进程是否满足递归断言的方法。

---

## 扩展断言的语法

为了利用引理并设计算法，我们需要允许进程出现在断言中。为此，我们扩展断言的语法，使其包含一种更通用的递归断言形式，即**有限进程集**可以**标记**变量的绑定出现。

### 新的断言形式

- **定义**：

  如果断言 $A$ 中变量 $X$ 仅以**正极性**出现，且 $p_1, \dots, p_n$ 是进程，那么：

  $$
  \nu X \{ p_1, \dots, p_n \} A
  $$

  是一个断言。

- **理解**：将其理解为表示与：

  $$
  \nu X . ( \{ p_1, \dots, p_n \} \lor A )
  $$

  表示相同的属性。

- **注意**：当集合 $\{ p_1, \dots, p_n \}$ 为空集时，即 $\nu X \{ \} A$，相当于 $\nu X . A$。

### 例子和练习

**练习 14.18**：

证明对于任意进程 $p$，有：

$$
\text{若 } p \in \{ p_1, \dots, p_n \}，\text{则 } p \models \nu X \{ p_1, \dots, p_n \} A \text{ 为真}
$$

- **解答思路**：

  - 根据定义，如果 $p \in \{ p_1, \dots, p_n \}$，则 $p$ 满足断言 $\nu X \{ p_1, \dots, p_n \} A$。

---

## 满足断言的归约

借助这些扩展的断言形式，我们可以提出一个算法，用于确定判断 $p \models A$ 的真伪。

### 布尔值运算

- **布尔运算**：

  - **否定**：设 $\neg t$ 表示布尔值 $t$ 的否定，即：
    - $\neg \text{true} = \text{false}$
    - $\neg \text{false} = \text{true}$

  - **合取（与）**：$t_0 \land t_1$ 为真，当且仅当 $t_0$ 和 $t_1$ 都为真。

  - **析取（或）**：$t_0 \lor t_1$ 为真，当且仅当 $t_0$ 或 $t_1$ 至少有一个为真。

- **一般化**：对于多个布尔值 $t_1, \dots, t_n$，有：

  $$
  \bigvee_{ i = 1 }^{ n } t_i
  $$

  - 当至少有一个 $t_i$ 为真时，结果为真；否则为假。

  - **空析取**被理解为假。

### 归约等式

利用归约引理，我们可以得出以下等式，用于计算 $p \models A$：

1. **常量断言**：

   - 若 $p \in S$，则 $p \models S$ 为真。
   - 若 $p \notin S$，则 $p \models S$ 为假。

2. **特殊常量**：

   - $p \models T$ 为真。
   - $p \models F$ 为假。

3. **否定**：

   $$
   p \models \neg B \iff \neg ( p \models B )
   $$

4. **合取**：

   $$
   p \models A_0 \land A_1 \iff ( p \models A_0 ) \land ( p \models A_1 )
   $$

5. **析取**：

   $$
   p \models A_0 \lor A_1 \iff ( p \models A_0 ) \lor ( p \models A_1 )
   $$

6. **菱形模态**：

   $$
   p \models ( \alpha ) B \iff \exists q, \ p \xrightarrow{ \alpha } q \text{ 且 } q \models B
   $$

7. **通用菱形模态**：

   $$
   p \models ( . ) B \iff \exists \alpha, q, \ p \xrightarrow{ \alpha } q \text{ 且 } q \models B
   $$

8. **最大不动点**：

   - 若 $p \in \{ r \}$，则 $p \models \nu X \{ r \} B$ 为真。
   - 若 $p \notin \{ r \}$，则：

     $$
     p \models \nu X \{ r \} B \iff p \models B [ \nu X \{ p, r \} B / X ]
     $$

     - **解释**：在 $B$ 中，用 $\nu X \{ p, r \} B$ 替换 $X$。

---

## 归约规则

这些等式提示了归约规则，我们可以将左侧的满足断言替换为右侧的表达式。

### 归约规则列表

1. **常量断言**：

   - 若 $p \in S$，则：

     $$
     ( p \models S ) \longrightarrow \text{true}
     $$

   - 若 $p \notin S$，则：

     $$
     ( p \models S ) \longrightarrow \text{false}
     $$

2. **特殊常量**：

   - $$
     ( p \models T ) \longrightarrow \text{true}
     $$

   - $$
     ( p \models F ) \longrightarrow \text{false}
     $$

3. **否定**：

   $$
   ( p \models \neg B ) \longrightarrow \neg ( p \models B )
   $$

4. **合取**：

   $$
   ( p \models A_0 \land A_1 ) \longrightarrow ( p \models A_0 ) \land ( p \models A_1 )
   $$

5. **析取**：

   $$
   ( p \models A_0 \lor A_1 ) \longrightarrow ( p \models A_0 ) \lor ( p \models A_1 )
   $$

6. **菱形模态**：

   - 若 $p$ 有 $n$ 个满足 $p \xrightarrow{ \alpha } q_i$ 的后继状态 $q_i$，则：

     $$
     ( p \models ( \alpha ) B ) \longrightarrow ( q_1 \models B ) \lor \dots \lor ( q_n \models B )
     $$

7. **通用菱形模态**：

   - 若 $p$ 有 $n$ 个后继状态 $q_i$，即存在 $\alpha_i$，使得 $p \xrightarrow{ \alpha_i } q_i$，则：

     $$
     ( p \models ( . ) B ) \longrightarrow ( q_1 \models B ) \lor \dots \lor ( q_n \models B )
     $$

8. **最大不动点**：

   - 若 $p \in \{ r \}$，则：

     $$
     ( p \models \nu X \{ r \} B ) \longrightarrow \text{true}
     $$

   - 若 $p \notin \{ r \}$，则：

     $$
     ( p \models \nu X \{ r \} B ) \longrightarrow ( p \models B [ \nu X \{ p, r \} B / X ] )
     $$

---

## 归约过程的描述

上述归约规则提示了一个**递归算法**，用于决定 $p \models A$ 的真伪。

### 布尔表达式的处理

- **需要明确**：在归约过程中，如何处理出现在满足表达式之间的布尔操作。

- **要求**：无论采用何种布尔表达式的求值方法，规则应当满足以下性质：

  - **否定**：

    - 若 $b \longrightarrow^* t$，则 $\neg b \longrightarrow^* \neg t$。

  - **合取**：

    - 若 $b_0 \longrightarrow^* t_0$，$b_1 \longrightarrow^* t_1$，则：

      $$
      ( b_0 \land b_1 ) \longrightarrow^* t \iff t = t_0 \land t_1
      $$

  - **析取**：

    - 类似地，对于析取操作。

- **一般化**：对于多个布尔表达式的析取，当所有分支都归约为布尔值后，结果为这些布尔值的析取。

### 完整的归约规则

1. **常量断言**：

   - 若 $p \in S$，则：

     $$
     ( p \models S ) \longrightarrow \text{true}
     $$

   - 若 $p \notin S$，则：

     $$
     ( p \models S ) \longrightarrow \text{false}
     $$

2. **特殊常量**：

   - $$
     ( p \models T ) \longrightarrow \text{true}
     $$

   - $$
     ( p \models F ) \longrightarrow \text{false}
     $$

3. **否定**：

   $$
   ( p \models \neg B ) \longrightarrow \neg ( p \models B )
   $$

4. **合取**：

   $$
   ( p \models A_0 \land A_1 ) \longrightarrow ( p \models A_0 ) \land ( p \models A_1 )
   $$

5. **析取**：

   $$
   ( p \models A_0 \lor A_1 ) \longrightarrow ( p \models A_0 ) \lor ( p \models A_1 )
   $$

6. **菱形模态**：

   - 若 $p$ 有 $n$ 个满足 $p \xrightarrow{ \alpha } q_i$ 的后继状态 $q_i$，则：

     $$
     ( p \models ( \alpha ) B ) \longrightarrow ( q_1 \models B ) \lor \dots \lor ( q_n \models B )
     $$

     - **特殊情况**：如果 $p$ 没有后继状态（即没有可执行的 $\alpha$ 动作），则结果为假。

7. **通用菱形模态**：

   - 类似地，对于通用菱形模态。

8. **最大不动点**：

   - 若 $p \in \{ r \}$，则：

     $$
     ( p \models \nu X \{ r \} B ) \longrightarrow \text{true}
     $$

   - 若 $p \notin \{ r \}$，则：

     $$
     ( p \models \nu X \{ r \} B ) \longrightarrow ( p \models B [ \nu X \{ p, r \} B / X ] )
     $$

---

## 归约算法的正确性

虽然这些规则看起来合理，但我们需要证明该归约过程总是终止并给出正确的结果。

### 定理 14.19

**定理 14.19**：

设 $p \in P$ 是一个有限状态进程，$A$ 是一个闭合断言。对于任意布尔值 $t$，有：

$$
( p \models A ) \longrightarrow^* t \iff ( p \models A ) = t
$$

- **解释**：通过归约规则，我们最终得到的布尔值 $t$，与 $p$ 实际上是否满足 $A$ 一致。

### 定理证明

**证明思路**：

- 我们使用对断言的**良基归纳（well-founded induction）**，定义一种关系 $\prec$，使得断言的归约过程在此关系下是良基的。

- **关系定义**：

  - $A' \prec A$，当且仅当：

    1. $A'$ 是 $A$ 的真子断言（proper subassertion），或

    2. $A$ 和 $A'$ 具有以下形式：

       - $A = \nu X \{ r \} B$，$A' = \nu X \{ p, r' \} B$，且 $p \notin \{ r \}$。

- **由于 $P_p$ 是有限集合**（即从 $p$ 可达的进程有限），因此关系 $\prec$ 是良基的。

- **证明目标**：对于所有的闭合断言 $A$，证明：

  $$
  \forall q \in P_p, \ \forall t \in \{ \text{true}, \text{false} \}, \ ( q \models A ) \longrightarrow^* t \iff ( q \models A ) = t
  $$

- **证明过程**：

  - **归纳假设**：对于所有 $A' \prec A$，假设命题成立。

  - **分类讨论**：对 $A$ 的不同形式进行讨论，证明在归纳假设下，命题成立。

- **关键步骤**：

  - **合取、析取、否定等情况**：直接利用归纳假设和布尔运算的性质，证明命题成立。

  - **最大不动点的情况**：需要特别处理，证明在归纳假设下，归约过程终止并得到正确的结果。

- **结论**：通过归纳，证明命题对所有的闭合断言成立，即定理成立。

---

## 算法示例

为了更好地理解算法，我们举一个例子。

### 示例

考虑 CCS 中定义的进程：

$$
\begin{cases}
P \ \overset{\text{def}}{=} \ a . Q \\
Q \ \overset{\text{def}}{=} \ a . P
\end{cases}
$$

- **解释**：进程 $P$ 可以执行动作 $a$，然后变为进程 $Q$；$Q$ 也可以执行动作 $a$，然后变为 $P$。

- **目标**：证明 $P$ 能够执行无限多次 $a$ 动作，即 $P \models \nu X . ( a ) X$。

### 归约过程

1. **初始**：

   $$
   P \models \nu X \{ \} ( a ) X
   $$

2. **展开最大不动点**（根据归约规则）：

   $$
   P \models \nu X \{ \} ( a ) X \longrightarrow P \models ( a ) X [ \nu X \{ P \} ( a ) X / X ]
   $$

   - **解释**：在 $X$ 中替换为 $\nu X \{ P \} ( a ) X$。

3. **处理菱形模态**：

   - $P$ 可以执行动作 $a$，到达 $Q$，因此：

     $$
     P \models ( a ) \nu X \{ P \} ( a ) X \longrightarrow Q \models \nu X \{ P \} ( a ) X
     $$

4. **再次展开最大不动点**：

   $$
   Q \models \nu X \{ P \} ( a ) X \longrightarrow Q \models ( a ) X [ \nu X \{ Q, P \} ( a ) X / X ]
   $$

5. **继续归约**：

   - $Q$ 可以执行动作 $a$，到达 $P$，因此：

     $$
     Q \models ( a ) \nu X \{ Q, P \} ( a ) X \longrightarrow P \models \nu X \{ Q, P \} ( a ) X
     $$

6. **观察到集合不再扩展**：

   - 集合 $\{ Q, P \}$ 包含所有可能的进程，因此归约过程终止，得到：

     $$
     P \models \nu X \{ Q, P \} ( a ) X \longrightarrow \text{true}
     $$

7. **结论**：

   - 归约过程表明 $P \models \nu X . ( a ) X$ 为真，即 $P$ 能够执行无限多次 $a$。

---

## 结论

通过上述算法，我们可以决定有限状态进程是否满足给定的递归模态断言。

- **优点**：算法在理论上是正确的，能够给出准确的结果。

- **缺点**：在最坏情况下，可能效率不高，因为没有充分利用数据共享的潜力。

- **改进**：实际应用中，需要对算法进行优化，以提高效率。

---

## 练习解答

### 练习14.18

**问题**：

证明对于任意进程 $p$，有：

$$
\text{若 } p \in \{ p_1, \dots, p_n \}，\text{则 } p \models \nu X \{ p_1, \dots, p_n \} A \text{ 为真}
$$

**解答**：

- 根据定义，当 $p \in \{ p_1, \dots, p_n \}$ 时，$p \models \nu X \{ p_1, \dots, p_n \} A$ 为真。

- **理由**：由于 $p$ 在绑定变量的集合中，因此满足断言。

---

### 练习14.20

**(i) 问题**：

对于 CCS 进程 $P \overset{\text{def}}{=} a . P$，证明 $P \models \nu X . ( a ) T \land [ a ] X$。

**解答**：

- **展开断言**：

  $$
  P \models \nu X . ( a ) T \land [ a ] X
  $$

- **归约过程**：

  1. **展开最大不动点**：

     $$
     P \models \nu X \{ \} ( a ) T \land [ a ] X \longrightarrow P \models ( a ) T \land [ a ] X [ \nu X \{ P \} ( a ) T \land [ a ] X / X ]
     $$

  2. **处理合取**：

     - 需要同时验证：

       - $P \models ( a ) T$
       - $P \models [ a ] X [ \dots ]$

  3. **验证 $P \models ( a ) T$**：

     - $P$ 可以执行 $a$，因此 $P \models ( a ) T$ 为真。

  4. **验证 $P \models [ a ] X [ \dots ]$**：

     - 对于 $P$ 的所有 $a$ 后继状态 $q$，需要验证 $q \models X [ \dots ]$。

     - $P$ 执行 $a$ 后到达自身，因此需要验证：

       $$
       P \models \nu X \{ P \} ( a ) T \land [ a ] X
       $$

     - 由于 $P \in \{ P \}$，根据归约规则，直接得到 $P \models \nu X \{ P \} ( a ) T \land [ a ] X$ 为真。

  5. **结论**：

     - 归约过程得到 $P \models \text{true}$，即原断言为真。

---

# 总结

通过本节的学习，我们详细了解了本地模型检查算法的理论基础和实际应用。关键点包括：

- **归约引理**：提供了将递归断言的验证问题转化为更易处理的形式的方法。

- **扩展的断言语法**：允许在断言中包含进程集合，增强了断言的表达能力。

- **归约规则**：为验证进程是否满足断言提供了系统的方法。

- **算法的正确性**：通过定理证明了算法的正确性和终止性。

希望这些详细的解释能够帮助你更好地理解第 14.8 节的内容。如果你有任何疑问，欢迎继续提问！

### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------