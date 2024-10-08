[toc]



# 第8章 域理论导论

**域理论（Domain Theory）**是**指称语义学（denotational semantics）**的数学基础。本章扩展了关于**完全偏序集（complete partial orders，简称 cpo）**和**连续函数（continuous functions）**的研究，介绍了在编程语言的数学描述中非常重要的 cpo 构造。它为我们后续的指称语义学工作提供了数学基础。我们还引入了一种支持语义定义的元语言；在该元语言中定义的函数保证是连续的。

## 8.1 基本定义

在指称语义学中，一个编程构造（如命令或表达式）的意义是通过将其赋予一个可能的含义域（domain of possible meanings）中的元素来实现的。编程构造被称为**指称（denote）**这个元素，而这个元素被称为该构造的**指称（denotation）**。例如，IMP（一个简单的指令式编程语言）中的命令被指称为部分函数的元素，而 IMP 中的数字可以指称自然数 $\mathbb{N}$ 的元素。正如第5章中 IMP 的指称语义学所明确的，有时需要“域”具有足够的结构，使得它们能够解决递归方程。

第5章将**完全偏序集（cpo）**作为支持递归定义的结构进行了动机解释，这些 cpo 是作为含义“域”的合理候选。当然，只有通过证明它们在一系列编程语言中的适用性，并通过表达它们与操作语义之间的关系的结果，才能证明完全偏序集的适当性。然而，经验和结果证明了它们的重要性；尽管有时需要为完全偏序集添加结构，但它们似乎是能够为编程语言提供**组合式（compositional）**语义定义的任何一般理论的基础。

**回顾定义：**

- **完全偏序集（Complete Partial Order，cpo）**：一个偏序集 $(D, \sqsubseteq)$，如果它具有任意 $\omega$-链 $d_0 \sqsubseteq d_1 \sqsubseteq \dots \sqsubseteq d_n \sqsubseteq \dots$ 的最小上界（least upper bound，lub），即 $\bigsqcup_{n \in \mathbb{N}} d_n$，那么称其为一个 cpo。

- **带底元素的 cpo**：如果一个 cpo $(D, \sqsubseteq)$ 有一个最小元素 $\bot$（称为“底（bottom）”），那么称其为带底元素的 cpo。

**注意**：有时我们会明确指定 cpo 的偏序关系和底元素，例如写作 $(D, \sqsubseteq_D)$，其中 $\sqsubseteq_D$ 和 $\bot_D$ 属于 cpo $D$。然而，更常见的是，我们直接写 $\sqsubseteq$ 和 $\bot$，因为上下文通常能明确我们所指的 cpo。

当我们写 $\bigsqcup_n d_n$ 时，如果上下文清楚，我们可能省略下标 $n \in \omega$，即默认为对所有自然数 $n$。

### 例子：

1. **离散 cpo**：任何由恒等关系（identity relation）排序的集合都形成一个离散 cpo。

2. **幂集 cpo**：任何集合 $X$ 的幂集 $\text{Pow}(X)$，按包含关系 $\subseteq$ 或反包含关系 $\supseteq$ 排序，都形成一个 cpo。实际上，任何**完全格（complete lattice）**都是一个 cpo（参见第5.5节）。

3. **两元素 cpo**：$\bot \sqsubseteq \text{T}$，称为 $O$。这样的顺序关系来自于单元素集合的幂集按 $\subseteq$ 排序。

4. **部分函数的集合**：从集合 $X$ 到 $Y$ 的部分函数集合 $X \rightharpoonup Y$，按包含关系排序，形成一个 cpo。

5. **扩展的非负整数**：将非负整数 $\omega$ 扩展为 $\infty$，并按链式顺序排序，形成一个 cpo，称为 $n$。

---

**完全偏序集只给出了半个图景。**只有确保 cpo 之间的函数**保持 $\omega$-链的最小上界**，我们才能获得支持递归定义的框架。

**定义：**

- **单调函数（monotonic function）**：在 cpo $D$ 和 $E$ 之间的函数 $f : D \rightarrow E$ 是单调的，当且仅当对于所有 $d, d' \in D$，如果 $d \sqsubseteq d'$，则 $f(d) \sqsubseteq f(d')$。

- **连续函数（continuous function）**：函数 $f : D \rightarrow E$ 是连续的，当且仅当它是单调的，并且对于 $D$ 中的任何 $\omega$-链 $d_0 \sqsubseteq d_1 \sqsubseteq \dots \sqsubseteq d_n \sqsubseteq \dots$，有：

  $$
  f\left( \bigsqcup_{n \in \mathbb{N}} d_n \right) = \bigsqcup_{n \in \mathbb{N}} f(d_n)
  $$

### 例子：

1. **从离散 cpo 到任意 cpo 的函数**：所有从离散 cpo（即集合）到 cpo 的函数都是连续的。

2. **函数 $f_n : n \rightarrow O$ 的定义**：

   对于 $n \in n$，定义 $f_n$ 为：

   $$
   f_n(x) = \begin{cases}
   \text{T} & \text{如果 } n \leq x \\
   \bot & \text{否则}
   \end{cases}
   $$

   从 $n$ 到 $O$ 的连续函数包括恒等于 $\bot$ 的函数 $\lambda x.\bot$，以及所有 $f_n$，其中 $n \in \omega$。

   注意，函数 $f_\infty$ 不是连续的（为什么？）。

---

**命题8.1：**

- cpo $D$ 上的恒等函数 $\text{Id}_D$ 是连续的。
- 如果 $f : D \rightarrow E$ 和 $g : E \rightarrow F$ 是连续函数，那么它们的组合 $g \circ f : D \rightarrow F$ 也是连续的。

**练习8.2：**

**证明上述命题。**

**解答：**

- **证明恒等函数连续**：

  - 对于任何 $d, d' \in D$，如果 $d \sqsubseteq d'$，则 $\text{Id}_D(d) = d \sqsubseteq d' = \text{Id}_D(d')$，所以 $\text{Id}_D$ 是单调的。

  - 对于任何 $\omega$-链 $d_0 \sqsubseteq d_1 \sqsubseteq \dots$，有：

    $$
    \text{Id}_D\left( \bigsqcup_{n} d_n \right) = \bigsqcup_{n} d_n = \bigsqcup_{n} \text{Id}_D(d_n)
    $$

    所以 $\text{Id}_D$ 是连续的。

- **证明连续函数的组合是连续的**：

  - 由于 $f$ 和 $g$ 都是单调的，所以对于所有 $d, d' \in D$，如果 $d \sqsubseteq d'$，则 $f(d) \sqsubseteq f(d')$，且 $g(f(d)) \sqsubseteq g(f(d'))$，因此 $g \circ f$ 是单调的。

  - 对于 $D$ 中的 $\omega$-链 $d_0 \sqsubseteq d_1 \sqsubseteq \dots$，有：

    $$
    \begin{aligned}
    (g \circ f)\left( \bigsqcup_{n} d_n \right) &= g\left( f\left( \bigsqcup_{n} d_n \right) \right) \\
    &= g\left( \bigsqcup_{n} f(d_n) \right) \quad \text{（因为 $f$ 连续）} \\
    &= \bigsqcup_{n} g(f(d_n)) \quad \text{（因为 $g$ 连续）} \\
    &= \bigsqcup_{n} (g \circ f)(d_n)
    \end{aligned}
    $$

    所以 $g \circ f$ 是连续的。

---

在第5.4节中，我们展示了具有底元素 $\bot$ 的 cpo 的一个核心性质：**任何在其上的连续函数都有最小不动点（least fixed point）**。

**定理8.3（不动点定理）**：

- 设 $f : D \rightarrow D$ 是 cpo $D$ 上的连续函数，且 $D$ 具有底元素 $\bot$。定义：

  $$
  \text{fix}(f) = \bigsqcup_{n \in \mathbb{N}} f^n(\bot)
  $$

- 那么：

  1. $\text{fix}(f)$ 是 $f$ 的不动点，即 $f(\text{fix}(f)) = \text{fix}(f)$。

  2. $\text{fix}(f)$ 是 $f$ 的最小前不动点（prefixed point），即如果 $f(d) \sqsubseteq d$，则 $\text{fix}(f) \sqsubseteq d$。

- 因此，$\text{fix}(f)$ 是 $f$ 的最小不动点。

---

## 8.2 流（Streams）——一个例子

**完全偏序集和连续函数**已经在第5章中从与有限规则相关的归纳定义的角度进行了动机解释，通过提取那些用于获得集合运算符的最小不动点的性质。鉴于操作语义通常可以用一组有限规则来表示，连续性与计算的相关性就不足为奇了。然而，连续性的意义可以更直接地理解，为此，我们将考虑序列上的计算，作为一个例子。

假设我们的输入值是由 $0$ 和 $1$ 组成的有限和无限序列，此外，我们允许（但不强制）有限序列以一个特殊符号“$”结尾。这个想法是，这些序列代表了可能的输入，可能来自另一个计算或用户；一个由 $0$ 和 $1$ 组成的序列被传递，并可以选择用“$”显式地通知序列已结束。序列的长度可以随着时间无限增长，除非它已用“$”终止。序列可以在未终止的情况下保持有限长度；可能是输入设备发生故障，或者进入了一个发散的计算，或者对于用户来说，在输入下一个元素或用“$”终止之前感到厌倦了。

这些序列有时被称为**流（streams）**、**惰性列表（lazy lists）**或“**带终止符的序列（stoppered sequences）**”（$ 是“终止符”）。它们承认一个直观的偏序关系。称一个序列 $s$ 小于等于另一个序列 $s'$，如果 $s$ 是 $s'$ 的前缀。偏序关系中的增加与包含更多信息的序列相关。相对于这个偏序关系，存在一个最小的序列，即空序列 $\epsilon$。存在“终止”的最大序列，例如：

- $0101\$$

以及无限序列，例如：

- $000\cdots00\cdots$，记为 $0^\omega$。

实际上，这些序列形成了一个以空序列 $\epsilon$ 为底元素的 cpo。我们将这个 cpo 称为 $S$。

想象一下，我们希望检测输入中是否出现了 $1$。看起来我们希望有一个函数：

$$
\text{isone} : S \rightarrow \{ \text{true}, \text{false} \}
$$

它对于给定的序列，如果序列包含 $1$，则返回 $\text{true}$，否则返回 $\text{false}$。但这有点天真。如果序列在某个阶段不包含 $1$，然后在稍后时间 $1$ 出现了，例如从空序列 $\epsilon$ 变为 $10$，我们就需要将最初的输出 $\text{false}$ 更新为 $\text{true}$。我们希望当 $\text{isone}$ 对某个输入返回 $\text{false}$ 时，它确实意味着不可能出现 $1$。而且，我们需要 $\text{isone}(000\$) = \text{false}$，因为一旦序列终止，就肯定不会再出现 $1$。但我们希望 $\text{isone}(000)$ 不同于 $\text{false}$，当然它也不应该是 $\text{true}$。

我们有两个选择：要么允许 $\text{isone}$ 是一个部分函数，要么在真值的基础上引入一个“**不知道（don't know）**”的元素，代表未定义。这种情况下，技术上更简单的是选择后者。

新的“不知道”值可以随着输入序列的更多信息而更新为 $\text{false}$ 或 $\text{true}$。我们将“不知道”值 $\bot$ 设为比 $\text{true}$ 和 $\text{false}$ 都小，如下图所示：

```
     true
      |
     ---
      |
     false
      |
     ---
      |
     ⊥ (bot)
```

我们用 $\{ \text{true}, \text{false} \}_\bot$ 表示这个简单的 cpo，其底元素为 $\bot$。现在，关于输入的更多信息反映为关于输出的更多信息。用数学术语来说，$\text{isone}$ 应该是一个从 $S$ 到 $\{ \text{true}, \text{false} \}_\bot$ 的单调函数。

决定了

$$
\text{isone} : S \rightarrow \{ \text{true}, \text{false} \}_\bot
$$

是单调的，并不能完全确定它是一个函数，即使限制条件是

- 对于任何序列 $s$：

  $$
  \text{isone}(1s) = \text{true}
  $$

  $$
  \text{isone}(0s) = \text{isone}(s)
  $$

- 且：

  $$
  \text{isone}(\$) = \text{false}
  $$

  $$
  \text{isone}(\epsilon) = \bot
  $$

那么，$\text{isone}(0^\omega)$ 呢？这些限制条件允许 $\text{isone}(0^\omega) = \text{false}$ 或 $\text{isone}(0^\omega) = \bot$。然而，前者在计算上是不可行的；输出 $\text{false}$ 涉及到检查一个无限序列并报告没有 $1$ 的存在。其计算不可行性反映在这样一个事实，即将 $\text{isone}(0^\omega)$ 取为 $\text{false}$ 会得到一个不连续的函数。

任何 $0^\omega$ 的有限前缀都具有形式 $0^n$，即由 $n$ 个 $0$ 组成。无限序列 $0^\omega$ 是链 $\bigsqcup_{n \in \mathbb{N}} 0^n$ 的最小上界。我们有 $\text{isone}(0^n) = \bot$，因此

$$
\bigsqcup_{n} \text{isone}(0^n) = \bot
$$

而连续性要求 $\text{isone}(0^\omega) = \bot$。

---

**练习8.4：**

**cpo 可以被视为拓扑空间，连续函数则是传统意义上拓扑连续的函数**（你不需要拓扑学的知识来完成这个练习）。给定一个 cpo $(D, \sqsubseteq)$，定义一个拓扑（称为 **Scott 拓扑**，以 Dana Scott 命名）如下。

称 $U \subseteq D$ 是开集，当且仅当：

1. 对于所有 $d, e \in D$，如果 $d \sqsubseteq e$ 且 $d \in U$，则 $e \in U$。

2. 对于 $D$ 中的任何链 $d_0 \sqsubseteq d_1 \sqsubseteq \dots$，如果 $\bigsqcup_{n} d_n \in U$，则存在某个 $n$，使得 $d_n \in U$。

**任务：**

(i) 证明这确实在 cpo $D$ 上确定了一个拓扑（即，空集和 $D$ 本身是开集，任意有限个开集的交集是开集，任意开集的并集是开集）。

(ii) 证明对于 cpo $D$ 的任意元素 $d$，集合 $\{ x \in D \mid x \sqsubseteq d \}$ 是开集。

(iii) 证明在 cpo 之间的函数 $f : D \rightarrow E$ 是连续函数，当且仅当 $f$ 在拓扑意义上是连续的（即，对于 $E$ 的任何开集 $V$，其逆像 $f^{-1}(V)$ 是 $D$ 的开集）。

(iv) 一般而言，证明 cpo $D$ 的开集可以精确地描述为形如 $f^{-1}(\{\text{T}\})$ 的集合，其中 $f : D \rightarrow O$ 是一个连续函数。

描述本节中讨论的流的 cpo 的开集。

**解答：**

由于篇幅限制，这里不展开具体的证明。如果您感兴趣，可以参考相关教材或资料深入学习 Scott 拓扑和 cpo 的拓扑性质。

---

## 8.3 cpo 的构造

完全偏序集可以通过丰富多样的方式构造。这种丰富性很重要，因为它意味着 cpo 可以作为许多不同类型的编程构造的含义域。本节介绍了 cpo 的各种构造，以及与这些构造相关的特定连续函数。这些将在后面为编程语言提供指称语义时非常有用。

有时，在给出这些构造时，确切地指定在构造中构建了哪些集合是很麻烦的；有许多不同的方法可以实现基本相同的构造。类似的尴尬在第一章关于基本集合论的介绍中也出现过；根据我们选择如何实现有序对的概念，可以有多种定义集合乘积的方法，并且，在形成不交并时，我们首先必须制作集合的不交副本——我们选择了一种方式，但还有许多其他方式。

在本节中，我们将采取一种更抽象的方法来进行构造。例如，在形成 cpo 的和（sum）$D_1 + \dots + D_k$ 时，直观上是通过并列放置 $D_1, \dots, D_k$ 的不交副本来实现的，我们将简单地假设存在函数 $\text{in}_i$，对于 $1 \leq i \leq k$，它们是单射（1-1 的），并确保当 $i \neq m$ 时，元素 $\text{in}_i(d_i)$ 和 $\text{in}_m(d_m)$ 是不同的。当然，重要的是我们知道这样的函数存在；在这种情况下，它们确实存在，因为一种可能性是将 $\text{in}_i(x)$ 实现为 $(i, x)$。

通过这种更抽象的方法，我们没有失去什么，因为无论我们选择如何实现函数 $\text{in}_i$，只要它们满足所需的区别条件，和构造将本质上是相同的。

表达结构“本质上相同”的数学方法是通过**同构（isomorphism）**的概念，它建立了结构何时是同构的。在 cpo 的情况下，一个连续函数 $f : D \rightarrow E$ 被称为一个同构，如果存在一个连续函数 $g : E \rightarrow D$，使得 $g \circ f = \text{Id}_D$ 且 $f \circ g = \text{Id}_E$——因此 $f$ 和 $g$ 是彼此的逆。这实际上是一个适用于一类对象和它们之间的函数的通用定义（在本例中是 cpo 和连续函数）。从定义可以看出，同构的 cpo 本质上是相同的，只是元素的命名不同。

**命题8.5：**

设 $(D, \sqsubseteq_D)$ 和 $(E, \sqsubseteq_E)$ 是两个 cpo。函数 $f : D \rightarrow E$ 是一个同构，当且仅当 $f$ 是一个一一对应，并且对于所有 $x, y \in D$，有：

$$
x \sqsubseteq_D y \quad \text{当且仅当} \quad f(x) \sqsubseteq_E f(y)
$$

---

### 8.3.1 离散 cpo

最简单的 cpo 是偏序关系为恒等关系的集合。在这种情况下，$\omega$-链必须是常量的。偏序关系为恒等关系的 cpo 称为**离散的（discrete）**。基本值，如真值或整数，形成离散 cpo，语法集合也如此。我们注意到，从离散 cpo 到 cpo 的任何函数都是连续的（因此，特别地，从语法集合到含义域的语义函数是连续的）。

**练习8.6：**

**问题**：从带有底元素 $\bot$ 的 cpo 到离散 cpo 的连续函数具有何种特性？

**解答**：

- 从带有底元素 $\bot$ 的 cpo 到离散 cpo 的连续函数，必须将 $\bot$ 映射为离散 cpo 中的某个元素。

- 由于离散 cpo 的偏序关系是恒等关系，因此唯一的 $\omega$-链是常量链。

- 因此，连续函数必须是常量或在 $\bot$ 处可能有所不同。

---

### 8.3.2 有限乘积

假设 $D_1, \dots, D_k$ 是 cpo。它们的乘积的底层集合是：

$$
D_1 \times \dots \times D_k = \{ (d_1, \dots, d_k) \mid d_1 \in D_1, \dots, d_k \in D_k \}
$$

偏序关系是**按坐标确定的**，即：

$$
(d_1, \dots, d_k) \sqsubseteq (d_1', \dots, d_k') \quad \text{当且仅当} \quad d_1 \sqsubseteq d_1' \land \dots \land d_k \sqsubseteq d_k'
$$

很容易验证，乘积的 $\omega$-链 $(d_{1n}, \dots, d_{kn})$，对于 $n \in \mathbb{N}$，其最小上界是按坐标计算的：

$$
\bigsqcup_{n} (d_{1n}, \dots, d_{kn}) = \left( \bigsqcup_{n} d_{1n}, \dots, \bigsqcup_{n} d_{kn} \right)
$$

因此，cpo 的乘积本身是一个 cpo。同样重要的是，与乘积 $D_1 \times \dots \times D_k$ 相关的有用函数。

- **投影函数**：$\pi_i : D_1 \times \dots \times D_k \rightarrow D_i$，对于 $i = 1, \dots, k$，选择元组的第 $i$ 个坐标：

  $$
  \pi_i(d_1, \dots, d_k) = d_i
  $$

  由于 $\omega$-链的最小上界是按坐标计算的，投影函数容易被证明是连续的。

- **元组化函数的扩展**：设 $f_1 : E \rightarrow D_1, \dots, f_k : E \rightarrow D_k$ 是连续函数。定义函数：

  $$
  (f_1, \dots, f_k) : E \rightarrow D_1 \times \dots \times D_k
  $$

  通过：

  $$
  (f_1, \dots, f_k)(e) = (f_1(e), \dots, f_k(e))
  $$

  该函数显然满足：

  $$
  \pi_i \circ (f_1, \dots, f_k) = f_i \quad \text{对于 } i = 1, \dots, k
  $$

  并且实际上，$(f_1, \dots, f_k)$ 是唯一满足该性质的从 $E$ 到 $D_1 \times \dots \times D_k$ 的函数。

  由于每个 $f_i$ 都是连续的，可以证明 $(f_1, \dots, f_k)$ 是连续的。

- **函数的乘积**：对于 $f_1 : E_1 \rightarrow D_1, \dots, f_k : E_k \rightarrow D_k$，定义：

  $$
  f_1 \times \dots \times f_k : E_1 \times \dots \times E_k \rightarrow D_1 \times \dots \times D_k
  $$

  通过：

  $$
  (f_1 \times \dots \times f_k)(e_1, \dots, e_k) = (f_1(e_1), \dots, f_k(e_k))
  $$

  该函数是连续的，因为它是连续函数的组合。

**例子**：

- 考虑 cpo $T_\bot \times T_\bot = T_\bot^2$，其中 $T_\bot$ 是带底元素的真值 cpo。可以从“鸟瞰”视角绘制它：

  ```
  (⊥, ⊥) --- (⊥, false) --- (⊥, true)
   |            |               |
  (false, ⊥) -- (false, false) - (false, true)
   |            |               |
  (true, ⊥) --- (true, false) -- (true, true)
  ```

  在这里，我们使用 $t$ 和 $f$ 表示真值 $\text{true}$ 和 $\text{false}$。

---

**练习8.7：**

**任务**：绘制 $O^0$、$O^1$、$O^2$ 和 $O^3$ 的乘积。

**提示**：$O$ 是 $\{ \bot, \text{T} \}$，即一个带底元素的两元素 cpo。

---

有两个易于证明但重要的乘积性质，其中一个将在后面被大量使用。

**引理8.8：**

设 $h : E \rightarrow D_1 \times \dots \times D_k$ 是一个从 cpo $E$ 到 cpo 乘积的函数。则 $h$ 是连续的，当且仅当对于所有 $i = 1, \dots, k$，函数 $\pi_i \circ h : E \rightarrow D_i$ 是连续的。

**证明**：

- “必要性”显然成立，因为连续函数的组合是连续的。

- “充分性”：假设对于所有 $i$，$\pi_i \circ h$ 是连续的。那么对于任何 $x \in E$，

  $$
  h(x) = (\pi_1(h(x)), \dots, \pi_k(h(x))) = (\pi_1 \circ h(x), \dots, \pi_k \circ h(x)) = (\pi_1 \circ h, \dots, \pi_k \circ h)(x)
  $$

  因此，$h = (\pi_1 \circ h, \dots, \pi_k \circ h)$，由于每个 $\pi_i \circ h$ 都是连续的，$h$ 是连续的。

---

**引理8.10：**

设 $f : D_1 \times \dots \times D_k \rightarrow E$ 是一个函数。则 $f$ 是连续的，当且仅当 $f$ 对每个参数分别连续，即对于所有 $i = 1, \dots, k$，对于任意固定的 $d_1, \dots, d_{i-1}, d_{i+1}, \dots, d_k$，函数：

$$
d_i \mapsto f(d_1, \dots, d_i, \dots, d_k)
$$

是连续的。

**证明**：

- “必要性”显然成立。

- “充分性”：为了简化记号，假设 $k = 2$。

  设 $(x_0, y_0) \sqsubseteq \dots \sqsubseteq (x_n, y_n) \sqsubseteq \dots$ 是 $D_1 \times D_2$ 中的链。则有：

  $$
  \begin{aligned}
  f\left( \bigsqcup_{n} (x_n, y_n) \right) &= f\left( \bigsqcup_{n} x_n, \bigsqcup_{n} y_n \right) \\
  &= \bigsqcup_{p} f\left( x_p, \bigsqcup_{n} y_n \right) \quad \text{（因为 $f$ 对第一个参数连续）} \\
  &= \bigsqcup_{p} \bigsqcup_{q} f(x_p, y_q) \quad \text{（因为 $f$ 对第二个参数连续）} \\
  &= \bigsqcup_{n} f(x_n, y_n)
  \end{aligned}
  $$

  因此，$f$ 是连续的。

---

**总结**

通过这些构造和引理，我们可以在 cpo 上定义复杂的结构，并确保相关的函数是连续的。这对于建立编程语言的指称语义是至关重要的，因为它允许我们在数学上精确地描述程序的行为，并利用不动点定理等工具来处理递归和循环等构造。

### ---------------------------

# 第8章 域理论导论（续）

## 8.3.3 函数空间（Function Space）

在域理论中，函数空间是非常重要的概念。它是指从一个 cpo（完全偏序集）到另一个 cpo 的所有**连续函数（continuous functions）**所构成的集合，并且这个集合本身可以构成一个 cpo。

设 $D$ 和 $E$ 是 cpo。所有从 $D$ 到 $E$ 的连续函数的集合可以构成一个完全偏序集。

**定义**：

- **函数空间** $[D \rightarrow E]$：
  
  $$
  [D \rightarrow E] = \{ f \mid f : D \rightarrow E \text{ 是连续的} \}
  $$
  
- **偏序关系**：对于 $f, g \in [D \rightarrow E]$，定义：
  
  $$
  f \sqsubseteq g \quad \text{当且仅当} \quad \forall d \in D, \ f(d) \sqsubseteq g(d)
  $$
  
  这称为**逐点序（pointwise order）**。

**性质**：

- 这个函数空间 $[D \rightarrow E]$ 本身是一个 cpo。
- 如果 $E$ 有一个底元素 $\bot_E$，那么 $[D \rightarrow E]$ 也有一个底元素，即**常 $\bot_E$ 函数** $\bot_{[D \rightarrow E]}$，定义为：
  
  $$
  \bot_{[D \rightarrow E]}(d) = \bot_E, \quad \forall d \in D
  $$

### $\omega$-链的最小上界

对于函数序列 $f_0 \sqsubseteq f_1 \sqsubseteq \dots \sqsubseteq f_n \sqsubseteq \dots$，它们的最小上界 $\bigsqcup_{n} f_n$ 也在 $[D \rightarrow E]$ 中，且对于所有 $d \in D$，有：

$$
\left( \bigsqcup_{n} f_n \right)(d) = \bigsqcup_{n} f_n(d)
$$

**需要验证**：

- $\bigsqcup_{n} f_n$ 是一个连续函数。

**证明**：

- 设 $d_0 \sqsubseteq d_1 \sqsubseteq \dots \sqsubseteq d_m \sqsubseteq \dots$ 是 $D$ 中的一个 $\omega$-链。
- 计算：

  $$
  \begin{aligned}
  \left( \bigsqcup_{n} f_n \right) \left( \bigsqcup_{m} d_m \right) &= \bigsqcup_{n} f_n \left( \bigsqcup_{m} d_m \right) \\
  &= \bigsqcup_{n} \left( f_n \left( \bigsqcup_{m} d_m \right) \right) \\
  &= \bigsqcup_{n} \left( \bigsqcup_{m} f_n(d_m) \right) \quad \text{（因为 $f_n$ 连续）} \\
  &= \bigsqcup_{m} \left( \bigsqcup_{n} f_n(d_m) \right) \quad \text{（因双重最小上界的交换，见命题8.9）} \\
  &= \bigsqcup_{m} \left( \left( \bigsqcup_{n} f_n \right)(d_m) \right)
  \end{aligned}
  $$

  因此，$\bigsqcup_{n} f_n$ 是连续的。

### 幂集（Power）

特殊的函数空间 $[I \rightarrow D]$，其中 $I$ 是一个集合，$D$ 是一个 cpo，被称为 $D^I$，也称为 $D$ 的**幂（power）**。当 $I$ 是有限集合时，$D^I$ 同构于 $D$ 的乘积 $D \times \dots \times D$，共 $k$ 个 $D$，通常记为 $D^k$。

**注意**：

- $D^I$ 的元素可以被视为以 $I$ 为指标集的元组 $(d_i)_{i \in I}$，按坐标排序。
- 当 $I$ 是无限集合时，这些元组可能是无限的。

### 关键操作：应用和柯里化

#### 应用（Application）

**定义**：

- **应用函数** $\text{apply} : [D \rightarrow E] \times D \rightarrow E$，定义为：
  
  $$
  \text{apply}(f, d) = f(d)
  $$
  
- **连续性**：$\text{apply}$ 是连续的。

**证明**（利用引理8.10：函数对每个参数分别连续则整体连续）：

- 对于函数参数 $f$：

  - 给定 $f_0 \sqsubseteq f_1 \sqsubseteq \dots$，有：

    $$
    \text{apply} \left( \bigsqcup_{n} f_n, d \right) = \left( \bigsqcup_{n} f_n \right)(d) = \bigsqcup_{n} f_n(d) = \bigsqcup_{n} \text{apply}(f_n, d)
    $$

- 对于参数 $d$：

  - 给定 $d_0 \sqsubseteq d_1 \sqsubseteq \dots$，有：

    $$
    \text{apply}(f, \bigsqcup_{n} d_n) = f\left( \bigsqcup_{n} d_n \right) = \bigsqcup_{n} f(d_n) = \bigsqcup_{n} \text{apply}(f, d_n)
    $$

- 因此，$\text{apply}$ 对每个参数都是连续的，所以 $\text{apply}$ 是连续的。

#### 柯里化（Currying）

柯里化是将一个多参数函数转换为一系列单参数函数的过程。

**定义**：

- 设 $F$ 是一个 cpo，$g : F \times D \rightarrow E$ 是一个连续函数。
- 定义 $\text{curry}(g) : F \rightarrow [D \rightarrow E]$，使得：

  $$
  \text{curry}(g)(v) = \lambda d \in D . g(v, d)
  $$

- **等价地**，对于所有 $v \in F$，$d \in D$，有：

  $$
  (\text{curry}(g))(v)(d) = g(v, d)
  $$

**需要验证**：

1. 对于每个 $v \in F$，$\text{curry}(g)(v)$ 是一个连续函数。
2. $\text{curry}(g)$ 本身是连续函数，即从 $F$ 到 $[D \rightarrow E]$。

**证明**：

1. **证明 $\text{curry}(g)(v)$ 连续**：

   - $g$ 对每个参数分别连续，因此固定 $v$，$d \mapsto g(v, d)$ 是连续的。

2. **证明 $\text{curry}(g)$ 连续**：

   - 设 $v_0 \sqsubseteq v_1 \sqsubseteq \dots$ 是 $F$ 中的 $\omega$-链。
   - 对于任意 $d \in D$，有：

     $$
     \begin{aligned}
     (\bigsqcup_{n} \text{curry}(g)(v_n))(d) &= \left( \bigsqcup_{n} \lambda d . g(v_n, d) \right)(d) \\
     &= \bigsqcup_{n} g(v_n, d) \\
     &= g\left( \bigsqcup_{n} v_n, d \right) \quad \text{（因为 $g$ 连续）} \\
     &= \text{curry}(g)\left( \bigsqcup_{n} v_n \right)(d)
     \end{aligned}
     $$

   - 因此，$\text{curry}(g)\left( \bigsqcup_{n} v_n \right) = \bigsqcup_{n} \text{curry}(g)(v_n)$，所以 $\text{curry}(g)$ 是连续的。

**性质**：

- $\text{curry}$ 是一个从 $[F \times D \rightarrow E]$ 到 $[F \rightarrow [D \rightarrow E]]$ 的连续函数。
- 对于所有 $v \in F$，$d \in D$，有 $\text{apply}(\text{curry}(g)(v), d) = g(v, d)$。

**练习8.11**：

**问题**：在 Lemma 8.10 中，我们证明了如果一个函数对每个参数分别连续，那么它是连续的。但是当参数空间是无限的（如 $D^I$，$I$ 无限）时，这个结论不再成立。

**提示**：考虑函数 $0^\omega \rightarrow O$。

**解答**：

- 当指数集 $I$ 是无限时，函数空间 $D^I$ 中的元素是无限元组。
- 对于这样的函数，即使它对每个参数分别连续，但整体可能不连续。
- 例如，考虑从 $0^\omega$ 到 $O$ 的函数，其中 $0^\omega$ 是无限序列组成的 cpo。

---

## 8.3.4 提升（Lifting）

在之前的章节中，我们遇到了将一个额外的元素 $\bot$ 添加到集合中以获得带底元素的 cpo 的情况（例如，第5.4节中将状态集合扩展为 $\Sigma_\bot$）。

**提升构造（Lifting Construction）**是对所有 cpo 的这种扩展的泛化。它通过在原始 cpo 下方添加一个新的底元素 $\bot$ 来构造新的 cpo。

**定义**：

- 给定一个 cpo $D$，定义其提升 $D_\bot$，其底层集合为：

  $$
  D_\bot = \{ \hat{d} \mid d \in D \} \cup \{ \bot \}
  $$

  其中 $\hat{d}$ 表示将 $d$ 映射到 $D_\bot$ 中的元素。

- **偏序关系**：

  - 对于 $d', d'' \in D_\bot$，定义 $d' \sqsubseteq d''$ 当且仅当：

    - $d' = \bot$，或者
    - 存在 $d_0, d_1 \in D$，使得 $d' = \hat{d}_0$，$d'' = \hat{d}_1$，且 $d_0 \sqsubseteq_D d_1$。

- **注入函数** $(-)^\wedge : D \rightarrow D_\bot$，定义为 $d \mapsto \hat{d}$，是连续的。

**性质**：

- $D_\bot$ 是在 $D$ 下方添加了一个新的底元素 $\bot$。
- 提升构造的图示如下：

  ```
       D
       |
      ...
       |
       d
       |
      ...
       |
      ⊥ (bot)
  ```

### 函数的扩展

给定一个连续函数 $f : D \rightarrow E$，其中 $E$ 是带底元素的 cpo，我们可以将其扩展为：

$$
f^* : D_\bot \rightarrow E_\bot
$$

定义为：

$$
f^*(d') = \begin{cases}
f(d) & \text{如果 } d' = \hat{d} \text{，其中 } d \in D \\
\bot_E & \text{如果 } d' = \bot
\end{cases}
$$

**注意**：

- $f^*$ 是连续的。
- 我们可以使用**let-记号**来表示这种扩展：

  $$
  \text{let } x \Leftarrow d' . e
  $$

  当 $d' = \hat{d}$ 时，$x$ 取值 $d$，否则（$d' = \bot$）结果为 $\bot$。

### 示例

- 我们可以将算术操作、布尔操作等扩展到提升后的 cpo 上。
- 例如，布尔或操作 $ \lor : T \times T \rightarrow T$ 可以扩展为 $ \lor_\bot : T_\bot \times T_\bot \rightarrow T_\bot$，定义为：

  $$
  x_1 \lor_\bot x_2 = \text{let } t_1 \Leftarrow x_1, t_2 \Leftarrow x_2 . t_1 \lor t_2
  $$

  这被称为**严格扩展（strict extension）**，因为只要任一输入为 $\bot$，结果就是 $\bot$。

**练习8.12**：

**问题**：以“真值表”的形式描述通常的布尔或操作 $\lor$ 的所有连续扩展。

**解答**：

- 对于布尔值 $T = \{ \text{true}, \text{false} \}$，其提升 $T_\bot = \{ \text{true}, \text{false}, \bot \}$。
- 需要列出所有满足连续性的 $\lor$ 的扩展，即函数 $f : T_\bot \times T_\bot \rightarrow T_\bot$，使得：

  - $f$ 是单调的。
  - $f$ 对 $\omega$-链的最小上界保持。

- 可能的连续扩展包括：

  | $x_1$          | $x_2$          | $x_1 \lor x_2$ |
  | -------------- | -------------- | -------------- |
  | $\bot$         | $\bot$         | $\bot$         |
  | $\bot$         | $\text{false}$ | $\bot$         |
  | $\bot$         | $\text{true}$  | $\text{true}$  |
  | $\text{false}$ | $\bot$         | $\bot$         |
  | $\text{false}$ | $\text{false}$ | $\text{false}$ |
  | $\text{false}$ | $\text{true}$  | $\text{true}$  |
  | $\text{true}$  | $\bot$         | $\text{true}$  |
  | $\text{true}$  | $\text{false}$ | $\text{true}$  |
  | $\text{true}$  | $\text{true}$  | $\text{true}$  |

- 需要检查上述定义是否满足连续性条件。

---

## 8.3.5 和（Sums）

在构造 cpo 时，通常需要形成 cpo 的不交并，例如将错误值添加到计算的通常值中。cpo 的和构造（sum construction）推广了集合上的不交并。

**定义**：

- 设 $D_1, \dots, D_k$ 是 cpo。
- 定义它们的**和** $D_1 + \dots + D_k$，其底层集合为：

  $$
  D_1 + \dots + D_k = \{ \text{in}_1(d) \mid d \in D_1 \} \cup \dots \cup \{ \text{in}_k(d) \mid d \in D_k \}
  $$

  其中 $\text{in}_i$ 是将 $D_i$ 的元素注入到和中的注入函数，满足：

  - $\text{in}_i$ 是单射。
  - 对于 $i \ne j$，$\text{in}_i(d) \ne \text{in}_j(d')$，即不同组件的元素在和中是区分的。

- **偏序关系**：

  - 对于 $d', d'' \in D_1 + \dots + D_k$，定义 $d' \sqsubseteq d''$ 当且仅当：

    - 存在 $i$，$d_0, d_1 \in D_i$，使得 $d' = \text{in}_i(d_0)$，$d'' = \text{in}_i(d_1)$，且 $d_0 \sqsubseteq_{D_i} d_1$。

**性质**：

- $D_1 + \dots + D_k$ 是一个 cpo。
- 注入函数 $\text{in}_i : D_i \rightarrow D_1 + \dots + D_k$ 是连续的。

### 结合函数

给定连续函数 $f_i : D_i \rightarrow E$，可以组合成一个函数：

$$
[f_1, \dots, f_k] : D_1 + \dots + D_k \rightarrow E
$$

定义为：

$$
[f_1, \dots, f_k](\text{in}_i(d)) = f_i(d), \quad \text{对于 } i = 1, \dots, k
$$

**性质**：

- $[f_1, \dots, f_k]$ 是连续的。
- 这是唯一的满足以下性质的函数：

  $$
  [f_1, \dots, f_k] \circ \text{in}_i = f_i, \quad \text{对于 } i = 1, \dots, k
  $$

**练习8.13**：

**问题**：证明将 $f_1, \dots, f_k$ 映射为 $[f_1, \dots, f_k]$ 的操作是连续的。

**解答**：

- 根据引理8.10，只需证明该操作对每个参数分别连续即可。
- 对于 $f_i$ 的 $\omega$-链，验证 $[f_1, \dots, f_k]$ 的连续性。

### 条件表达式和 cases 构造

**条件表达式**可以使用和构造来定义。例如，对于布尔值 $T = \{ \text{true}, \text{false} \}$，可以视为两个单元素 cpo 的和。

- 定义 $\text{cond} : T \times E \times E \rightarrow E$，其中 $E$ 是一个 cpo。
- 定义：

  $$
  \text{cond}(t, e_1, e_2) = \begin{cases}
  e_1 & \text{如果 } t = \text{true} \\
  e_2 & \text{如果 } t = \text{false}
  \end{cases}
  $$

- 这可以表示为：

  $$
  \text{cond}(t, e_1, e_2) = [\lambda x_1 . e_1, \lambda x_2 . e_2](t)
  $$

  其中 $\lambda x_1 . e_1 : \{ \text{true} \} \rightarrow E$，$\lambda x_2 . e_2 : \{ \text{false} \} \rightarrow E$。

**练习8.14**：

**问题**：验证上述定义的 $\text{cond}$ 和 $( - \rightarrow - \mid - )$ 确实如预期地作为条件表达式工作。

**解答**：

- 检查在不同输入值下，$\text{cond}$ 和条件表达式的行为是否一致。
- 验证其连续性。

**练习8.15**：

**问题**：解释为什么 cases 构造按预期工作。

**解答**：

- 当 $d = \text{in}_i(d_i)$ 时，$[\lambda x_1 . e_1, \dots, \lambda x_k . e_k](d)$ 返回 $e_i$。
- 这正是我们希望的行为。

---

## 8.4 元语言（A Metalanguage）

在定义编程语言的语义时，我们经常需要函数是连续的，以便能够取它们的最小不动点。然而，我们不希望每次在定义时都中断去检查表达式是否良定义并确实表示连续函数。

为了避免繁琐的工作，我们可以注意到，只要数学表达式符合某种非形式的语法，那么它们就将表示连续函数。这样的表达式构成了一个**元语言**，我们可以在其中描述特定编程语言的指称语义。

### Lambda 表达式

我们已经偶尔使用了 lambda 记号。在域理论中，我们将频繁使用它。

**定义**：

- 设 $e$ 是一个表达式，表示 cpo $E$ 中的一个元素，并且依赖于变量 $x$，其中 $x$ 属于 cpo $D$。
- 我们写：

  $$
  \lambda x \in D . e
  $$

  表示一个函数 $h : D \rightarrow E$，使得对于所有 $d \in D$，有 $h(d) = e[d / x]$。

- 通常，当 $x$ 的类型清楚时，我们省略类型，直接写 $\lambda x . e$。

### 保证连续性

为了自由地使用 lambda 记号，并确保我们定义的函数是连续的，我们需要建立一些规则。

**基本规则**：

1. **变量**：

   - 一个仅由变量 $x$ 组成的表达式在其变量上是连续的。
   - 对于 $y \in D$，抽象 $\lambda y . x$ 要么是恒等函数 $\lambda x . x$（如果 $y = x$），要么是常函数。

2. **常量**：

   - 我们遇到过一些 cpo 的特殊元素，例如底元素 $\bot_D \in D$、真值 $\text{true}, \text{false} \in T$、投影函数 $\pi_1 : D_1 \times D_2 \rightarrow D_1$ 等。
   - 这些常量表达式在其变量上是连续的。

3. **元组**：

   - 给定表达式 $e_1 \in E_1, \dots, e_k \in E_k$，可以形成元组 $(e_1, \dots, e_k) \in E_1 \times \dots \times E_k$。
   - 元组在其变量上是连续的，当且仅当其组成部分都是连续的。

4. **应用**：

   - 给定一个固定的连续函数 $K$，将其应用于表达式 $e$，结果 $K(e)$ 在其变量上是连续的，当且仅当 $e$ 在其变量上是连续的。

5. **lambda 抽象**：

   - 设 $e \in E$ 在其变量上是连续的。
   - 选择一个变量 $y \in D$，形成 $\lambda y . e : D \rightarrow E$。
   - 这个抽象在其变量上是连续的，当且仅当 $e$ 在其所有变量上是连续的。

6. **let 构造**：

   - 设 $e_1 \in D_\bot$，$e_2 \in E$，它们在其变量上都是连续的。
   - 可以形成表达式：

     $$
     \text{let } x \Leftarrow e_1 . e_2
     $$

     该表达式在其变量上是连续的。

7. **case 构造**：

   - 设 $e \in D_1 + \dots + D_k$，并在其变量上连续。
   - 给定表达式 $e_i \in E$，在其变量上连续。
   - cases 构造：

     $$
     \text{cases } e \text{ of } \text{in}_1(x_1) \Rightarrow e_1; \dots; \text{in}_k(x_k) \Rightarrow e_k
     $$

     在其变量上是连续的。

8. **不动点算子**：

   - 对于带底元素的 cpo $D$，有不动点算子 $\text{fix} : [D \rightarrow D] \rightarrow D$。
   - $\text{fix}$ 本身是连续的。

**注意**：

- 我们可以使用这些规则构建复杂的表达式，并确保它们在其变量上是连续的。
- 这使得我们可以在定义语义时专注于逻辑结构，而不用每次都验证连续性。

**练习8.16**：

**问题**：回忆第8.3.3节中的函数 $\text{curry} = \lambda g . \lambda v . \lambda d . g(v, d)$，它是从 $A = [F \times D \rightarrow E]$ 到 $B = [F \rightarrow [D \rightarrow E]]$ 的函数。

1. 为什么 $\text{curry}$ 是一个从 $A$ 到 $B$ 的连续函数？
2. 定义一个函数 $\text{uncurry} : B \rightarrow A$，使得 $\text{curry} \circ \text{uncurry} = \text{Id}_B$，$\text{uncurry} \circ \text{curry} = \text{Id}_A$。
3. 证明 $\text{uncurry}$ 是连续的，并且与 $\text{curry}$ 互为逆。

**解答**：

1. **证明 $\text{curry}$ 连续**：

   - $\text{curry}$ 是一个按照定义构造的函数。
   - 由于 $g$ 是连续的，$\text{curry}(g)$ 是连续的。
   - 根据之前的推导，$\text{curry}$ 保持 $\omega$-链的最小上界。

2. **定义 $\text{uncurry}$**：

   - 定义 $\text{uncurry}(h) = \lambda (v, d) . h(v)(d)$，其中 $h \in B$。

3. **证明 $\text{uncurry}$ 连续并互为逆**：

   - $\text{uncurry}$ 是连续的，因为 $h$ 和 $h(v)$ 都是连续的。
   - 验证：

     $$
     \text{curry}(\text{uncurry}(h)) = \text{curry}(\lambda (v, d) . h(v)(d)) = \lambda v . \lambda d . h(v)(d) = h
     $$

     $$
     \text{uncurry}(\text{curry}(g)) = \text{uncurry}(\lambda v . \lambda d . g(v, d)) = \lambda (v, d) . g(v, d) = g
     $$

   - 因此，$\text{curry}$ 和 $\text{uncurry}$ 是互逆的同构。

---

## 8.5 进一步阅读

本章的内容主要基于 Gordon Plotkin 的讲义（包括“比萨笔记”[80] 和他后来的工作[83]），虽然本章的呈现是基础性的，但受到了 Eugenio Moggi [67] 和 Andrew Pitts [75] 的工作的影响。这些基本思想可以追溯到 60 年代末 Dana Scott 的工作。

还要感谢 Christopher Wadsworth 的优秀的爱丁堡讲义，尽管它们未能出版。Larry Paulson 的书 [74] 提供了关于逻辑 LCF 以及在 ML 中实现的证明助手的背景。关于指称语义学的其他入门可以在 [88]、[95]、[91] 中找到。

实际上，本章介绍了 cpo 和连续函数的范畴，并展示了它是笛卡尔闭范畴（cartesian closed category），即该范畴具有积和函数空间；它还有由和构造给出的余积（coproducts）。关于范畴论的初级介绍可以在 [10]、[15] 中找到。

---

**总结**

在本节中，我们深入探讨了函数空间、提升、和构造等 cpo 的构造方法，以及如何在这些构造中定义连续函数。我们还介绍了一种元语言，帮助我们在定义编程语言的指称语义时，能够方便地构造连续函数并确保表达式的良定义和连续性。

通过这些工具，我们能够更系统地构建编程语言的语义模型，为后续的语义分析和验证奠定了坚实的数学基础。

### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------