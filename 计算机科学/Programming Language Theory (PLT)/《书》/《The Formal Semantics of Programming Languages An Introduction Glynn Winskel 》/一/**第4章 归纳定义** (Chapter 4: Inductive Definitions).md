[toc]



# 第4章 归纳定义

本章介绍了**归纳定义（inductive definitions）**集合的理论，其中语法和操作语义的表示是例子。通过规则归纳定义的集合被证明是满足这些规则的**最小集合**。因此，一个称为**规则归纳（rule induction）**的归纳原理伴随着这些构造。这一原理可以专门化为关于 IMP 操作语义的推理规则。

---

## 4.1 规则归纳

我们定义了算术表达式的语法集 $\text{Aexp}$，这是由算术表达式的构造规则得到的集合。我们已经看到有一个对应的归纳原理，即对算术表达式的**结构归纳（structural induction）**。

我们通过定义求值和执行关系来定义 while-程序的操作语义，这些关系是通过规则给出的，这些规则将项的求值或执行与其组成部分的求值或执行联系起来。例如，算术表达式的求值关系在第 2.2 节中通过规则定义为一个三元关系，它是 $\text{Aexp} \times \Sigma \times \mathbb{N}$ 中的三元组 $(a, \sigma, n)$ 的集合，使得 $(a, \sigma) \rightarrow n$。

有一个对应的归纳原理，我们可以将其视为我们称之为**规则归纳（rule induction）**的原理的一个特例。

我们感兴趣的是通过**规则（rules）**来定义一个集合。从抽象的角度看，规则的实例具有以下形式：

- 公理实例（axiom）：

  $$
  \frac{}{\ x }
  $$

  或表示为 $(\emptyset\ /\ x)$。

- 规则实例（rule instance）：

  $$
  \frac{ x_1,\ \dots,\ x_n }{ x }
  $$

  或表示为 $(\{ x_1, \dots, x_n \}\ /\ x)$。

给定一个规则实例集合 $R$，我们记 $I_R$ 为由 $R$ 定义的集合，它**恰好**由那些存在推导（derivation）的元素 $x$ 组成。换句话说，

$$
I_R = \{ x\ |\ \text{存在推导 } d,\ d\ \vdash_R\ x \}
$$

**规则归纳原理**对于证明一个性质对由一些规则定义的集合中的所有元素成立是有用的。其思想是，如果一个性质在从规则实例的前提到结论的过程中被保持，那么推导的结论就具有该性质，因此该性质对由规则定义的集合中的所有元素都成立。

### **规则归纳的一般原理**

设 $I_R$ 是由规则实例集合 $R$ 定义的集合。设 $P$ 是一个性质。那么，对于 $x \in I_R$，$P(x)$ 成立，当且仅当对于 $R$ 中的所有规则实例 $(X\ /\ y)$，其中 $X \subseteq I_R$，

$$
\left( \forall x \in X.\ P(x) \right) \Rightarrow P(y)
$$

**注意**：对于形如 $(X\ /\ y)$ 的规则实例，如果 $X = \emptyset$，则上述条件等价于 $P(y)$。此时，$\forall x \in X.\ x \in I_R\ \&\ P(x)$ 是平凡成立的，因为 $X$ 中没有元素。

因此，规则归纳的表述等价于以下内容：

对于规则实例集合 $R$，有：

- **对所有的公理实例**：

  $$
  \frac{}{\ x }
  $$

  $P(x)$ 为真。

- **对所有的规则实例**：

  $$
  \frac{ x_1,\ \dots,\ x_n }{ x }
  $$

  如果对于所有前提 $x_k$（$k$ 从 $1$ 到 $n$），有 $x_k \in I_R$ 且 $P(x_k)$ 为真，那么结论 $P(x)$ 也为真。

**规则归纳原理**相当直观。它对应于数学中更常用的一种方法，表面上看起来不同但实际上等价。（这个观察也将导致对规则归纳有效性的证明。）我们说一个集合 $Q$ **在规则实例 $R$ 下封闭**，或称为 $R$-封闭的，当且仅当对于所有规则实例 $(X\ /\ y)$，

$$
X \subseteq Q\ \Rightarrow\ y \in Q
$$

换句话说，如果一个集合在规则实例下封闭，那么当任何规则实例的前提都在集合中时，其结论也在集合中。特别地，一个 $R$-封闭的集合必须包含所有公理实例的结论。

集合 $I_R$ 是在这个意义下**在 $R$ 下封闭的最小集合**：

### **命题 4.1**

对于规则实例集合 $R$：

1. $I_R$ 是 $R$-封闭的。

2. 如果 $Q$ 是一个 $R$-封闭的集合，那么 $I_R \subseteq Q$。

**证明**：

1. **证明 $I_R$ 是 $R$-封闭的**：

   假设 $(X\ /\ y)$ 是 $R$ 中的一个规则实例，且 $X \subseteq I_R$。根据 $I_R$ 的定义，$X$ 中的每个元素都有推导。若 $X$ 非空，这些推导可以与规则实例 $(X\ /\ y)$ 组合，提供一个 $y$ 的推导；如果 $X$ 为空，则 $(\emptyset\ /\ y)$ 直接提供了一个 $y$ 的推导。在任一情况下，我们都得到了 $y$ 的推导，因此 $y \in I_R$。因此，$I_R$ 在 $R$ 下封闭。

2. **证明如果 $Q$ 是 $R$-封闭的，那么 $I_R \subseteq Q$**：

   假设 $Q$ 是 $R$-封闭的。我们需要证明 $I_R \subseteq Q$。$I_R$ 中的任何元素都是某个推导的结论。但任何推导都是由规则实例 $(X\ /\ y)$ 构成的。如果前提 $X$ 在 $Q$ 中，那么结论 $y$ 也在 $Q$ 中（特别地，任何公理的结论都在 $Q$ 中）。因此，我们可以从推导的最底层（公理）开始，逐步向上，证明其结论在 $Q$ 中。更正式地，我们可以对推导的真子推导关系 $\prec$ 进行归纳，证明：

   $$
   \forall y \in I_R.\ d\ \vdash_R\ y\ \Rightarrow\ y \in Q
   $$

   对于所有的 $R$-推导 $d$。因此，$I_R \subseteq Q$。

**证毕。**

---

**练习 4.2**

执行上述证明中提到的对推导的归纳。

**解答**：

我们需要对推导 $d$ 进行归纳，证明对于所有 $d$，如果 $d\ \vdash_R\ y$，则 $y \in Q$。

- **归纳假设**：对于所有真子推导 $d'\ \prec\ d$，若 $d'\ \vdash_R\ x$，则 $x \in Q$。

- **证明**：考虑推导 $d$：

  - 如果 $d$ 是一个公理实例 $(\emptyset\ /\ y)$，则 $d\ \vdash_R\ y$。由于 $X = \emptyset \subseteq Q$（平凡成立），且 $Q$ 是 $R$-封闭的，所以 $y \in Q$。

  - 如果 $d$ 是规则实例 $(X\ /\ y)$，且对每个前提 $x \in X$，都有对应的推导 $d_x\ \vdash_R\ x$，那么根据归纳假设，$x \in Q$。

    - 因此，$X \subseteq Q$。

    - 由于 $Q$ 是 $R$-封闭的，$(X\ /\ y) \in R$，所以 $y \in Q$。

因此，对于所有 $d$，若 $d\ \vdash_R\ y$，则 $y \in Q$。

---

假设我们希望证明一个性质 $P$ 对由规则 $R$ 定义的集合 $I_R$ 的所有元素成立。上述命题中的条件 (1) 和 (2) 提供了一种方法。

定义集合：

$$
Q = \{ x \in I_R\ |\ P(x) \}
$$

性质 $P$ 对 $I_R$ 的所有元素成立，当且仅当 $I_R \subseteq Q$。根据条件 (2)，为了证明 $I_R \subseteq Q$，只需证明 $Q$ 是 $R$-封闭的。这将成立，如果对于所有规则实例 $(X\ /\ y)$，

$$
\left( \forall x \in X.\ x \in I_R\ \&\ P(x) \right) \Rightarrow P(y)
$$

但这正是规则归纳所要求的，以证明性质 $P$ 对 $I_R$ 的所有元素成立。

上述陈述不仅足够，而且对于证明 $P$ 对 $I_R$ 的所有元素成立也是必要的。假设对于所有 $x \in I_R$，$P(x)$ 成立。令 $(X\ /\ y)$ 为一个规则实例，且

$$
\forall x \in X.\ x \in I_R\ \&\ P(x)
$$

根据 (1)，即 $I_R$ 是 $R$-封闭的，我们得到 $y \in I_R$，因此 $P(y)$。通过这种方式，我们从 (1) 和 (2) 导出了规则归纳原理，即 $I_R$ 是最小的 $R$-封闭集合。

---

**练习 4.3**

对于规则实例集合 $R$，证明：

$$
\bigcap \{ Q\ |\ Q\ \text{是 $R$-封闭的} \}
$$

是 $R$-封闭的。这个集合是什么？

**解答**：

- **证明**：交集中的每个 $Q$ 都是 $R$-封闭的。因此，对于所有规则实例 $(X\ /\ y)$，如果 $X \subseteq Q$，则 $y \in Q$。

- 交集 $A = \bigcap \{ Q\ |\ Q\ \text{是 $R$-封闭的} \}$。

- 对于所有规则实例 $(X\ /\ y)$，如果 $X \subseteq A$，则对于每个 $Q$，$X \subseteq Q$，因此 $y \in Q$。因此，$y \in A$。

- **结论**：因此，$A$ 是 $R$-封闭的。

- 这个集合是所有 $R$-封闭集合的交集，即**最小的 $R$-封闭集合**，也就是 $I_R$。

---

**练习 4.4**

设规则由 $(\emptyset\ /\ 0)$ 和 $({ n }\ /\ n + 1)$ 组成，其中 $n$ 是自然数。这个规则定义的集合是什么？在这种情况下，规则归纳是什么？

**解答**：

- **集合**：从公理 $(\emptyset\ /\ 0)$ 开始，得到 $0$。

- 通过规则 $({ n }\ /\ n + 1)$，如果 $n$ 在集合中，则 $n + 1$ 也在集合中。

- 因此，集合是自然数集 $\mathbb{N}$。

- **规则归纳**：为了证明性质 $P(n)$ 对所有自然数 $n$ 成立，只需证明：

  - **基始情况**：$P(0)$ 成立。

  - **归纳步骤**：如果 $P(n)$ 成立，那么 $P(n + 1)$ 也成立。

- 这就是**数学归纳法**。

---

在呈现规则时，我们遵循了与给出操作语义时相同的风格。当涉及到通过规则定义语法集时，BNF（巴科斯-诺尔范式）是传统的方法，尽管也可以用不同的方式。例如，传统上写作：

$$
a ::= \dots\ |\ a_0 + a_1\ |\ \dots
$$

表示如果 $a_0$ 和 $a_1$ 是算术表达式，那么 $a_0 + a_1$ 也是算术表达式。

我们可以将其改写为：

$$
\frac{ a_0 : \text{Aexp} \quad a_1 : \text{Aexp} }{ a_0 + a_1 : \text{Aexp} }
$$

这种呈现语法的方式正变得越来越普遍。

---

**练习 4.5**

当规则是 $\text{Aexp}$ 的构造规则时，规则归纳是什么？对于布尔表达式的规则又如何？（小心！参见下一节。）

**解答**：

- **对于算术表达式 $\text{Aexp}$**：

  - 规则归纳相当于对算术表达式的结构归纳。

  - 要证明一个性质 $P(a)$ 对所有 $a \in \text{Aexp}$ 成立，只需证明：

    1. **基础情况**：

       - 对于所有数字 $n$，$P(n)$ 成立。

       - 对于所有位置 $X$，$P(X)$ 成立。

    2. **归纳步骤**：

       - 如果 $P(a_0)$ 和 $P(a_1)$ 成立，那么 $P(a_0 + a_1)$ 成立。

       - 类似地，对于减法和乘法。

- **对于布尔表达式 $\text{Bexp}$**：

  - 需要注意的是，布尔表达式的规则涉及到算术表达式。

  - 因此，直接应用规则归纳会涉及到算术表达式的性质，需要更复杂的处理。

  - 这将在下一节讨论。

---

## 4.2 特殊的规则归纳

考虑布尔表达式和命令的语法集，显然有时一个语法集是由涉及另一个语法集的规则给出的。例如，命令的构造规则说明了如何从算术和布尔表达式以及其他命令构造命令。构造规则：

$$
c ::= \dots\ |\ X := a\ |\ \dots\ |\ \text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1\ |\ \dots
$$

为了统一起见，可以写成：

$$
\frac{ X : \text{Loc} \quad a : \text{Aexp} }{ X := a : \text{Com} }
$$

以及：

$$
\frac{ b : \text{Bexp} \quad c_0 : \text{Com} \quad c_1 : \text{Com} }{ \text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1 : \text{Com} }
$$

**规则归纳**通过展示性质被规则保持来工作。这意味着，如果我们要使用规则归纳来证明所有命令的某个性质，我们必须确保该性质也涵盖所有算术和布尔表达式。

因此，规则归纳原理并不能直接专门化为对命令的结构归纳，而是一个相当尴尬的证明原则，将命令的结构归纳与算术和布尔表达式的结构归纳结合起来。

为建立规则定义的集合的子集的性质，我们需要一个**规则归纳的特殊原理**。

### **规则归纳的特殊原理**

设 $I_R$ 是由规则实例集合 $R$ 定义的集合。设 $A \subseteq I_R$。设 $Q$ 是一个性质。那么，

$$
\forall a \in A.\ Q(a)
$$

当且仅当对于 $R$ 中的所有规则实例 $(X\ /\ y)$，其中 $X \subseteq I_R$ 且 $y \in A$，

$$
\left( \forall x \in X \cap A.\ Q(x) \right) \Rightarrow Q(y)
$$

**解释**：

- 我们只需要考虑那些结论在 $A$ 中的规则实例。

- 对于这些规则实例，如果其前提在 $A$ 中的元素都满足性质 $Q$，那么结论 $y$ 也满足 $Q$。

---

**规则归纳的特殊原理**实际上可以从一般原理推导出来。设 $R$ 是一组规则实例，$A$ 是 $I_R$ 的子集。假设我们感兴趣的性质 $Q(x)$ 想要证明对所有 $A$ 中的元素成立。

定义一个对应的性质 $P(x)$ 为：

$$
P(x) \quad \Leftrightarrow \quad x \in A\ \Rightarrow\ Q(x)
$$

证明对所有 $x \in I_R$，$P(x)$ 成立，等价于证明对所有 $a \in A$，$Q(a)$ 成立。根据一般的规则归纳原理，前者等价于：

$$
\forall (X\ /\ y) \in R.\ X \subseteq I_R\ \&\ \left( \forall x \in X.\ x \in A\ \Rightarrow\ Q(x) \right)\ \Rightarrow\ ( y \in A\ \Rightarrow\ Q(y) )
$$

这逻辑上等价于：

$$
\forall (X\ /\ y) \in R.\ \left( X \subseteq I_R\ \&\ y \in A\ \&\ \left( \forall x \in X.\ x \in A\ \Rightarrow\ Q(x) \right) \right)\ \Rightarrow\ Q(y)
$$

这正是特殊的规则归纳原理所要求的条件。

---

**练习 4.6**

解释命令和布尔表达式的结构归纳是如何从规则归纳的特殊原理得出的。

**解答**：

- **对于命令的结构归纳**：

  - 我们的目标是证明某个性质 $Q(c)$ 对所有命令 $c$ 成立。

  - 根据特殊的规则归纳原理，考虑所有结论在命令集 $\text{Com}$ 中的规则实例。

  - 这些规则实例的前提可能涉及到命令、算术表达式和布尔表达式。

  - 由于我们只关心命令，我们只需要在前提中涉及命令的部分应用性质 $Q$，而忽略算术和布尔表达式的部分。

  - 这样，我们就可以从规则归纳的特殊原理得出命令的结构归纳。

- **对于布尔表达式的结构归纳**：

  - 类似地，我们关注结论在布尔表达式集 $\text{Bexp}$ 中的规则实例。

  - 使用特殊的规则归纳原理，我们可以得到布尔表达式的结构归纳。

---

由于特殊原理是从一般原理推导出来的，任何使用特殊原理的证明都可以用一般规则归纳原理来替代。但在实践中，使用特殊原理可以大大减少需要考虑的规则数量，这在处理操作语义的规则归纳时是一个受欢迎的特性。

---

## 4.3 操作语义的证明规则

不出所料，规则归纳对于证明由规则呈现的操作语义的性质是一个有用的工具，尽管它通常采取表面上不同的形式，因为规则定义的集合是元组的集合。本节呈现了规则归纳的特殊情况，我们将在后面用来推理 IMP 程序的操作行为。

### 4.3.1 对算术表达式的规则归纳

对算术表达式的求值的规则归纳原理是从它们的操作语义规则得到的。它是规则归纳的一个例子。对于所有的求值 $(a, \sigma) \rightarrow n$，如果性质 $P(a, \sigma, n)$ 为真，当且仅当它被构建求值关系的规则保持。

**规则归纳原理**：

$$
\forall a \in \text{Aexp},\ \sigma \in \Sigma,\ n \in \mathbb{N}.\ (a, \sigma) \rightarrow n\ \Rightarrow\ P(a, \sigma, n)
$$

当且仅当：

- **数字**：

  $$
  \forall n \in \mathbb{N},\ \sigma \in \Sigma.\ P(n, \sigma, n)
  $$

- **位置**：

  $$
  \forall X \in \text{Loc},\ \sigma \in \Sigma.\ P(X, \sigma, \sigma(X))
  $$

- **加法**：

  $$
  \forall a_0, a_1 \in \text{Aexp},\ \sigma \in \Sigma,\ n_0, n_1 \in \mathbb{N}.\ (a_0, \sigma) \rightarrow n_0\ \&\ P(a_0, \sigma, n_0)\ \&\ (a_1, \sigma) \rightarrow n_1\ \&\ P(a_1, \sigma, n_1)\ \Rightarrow\ P(a_0 + a_1, \sigma, n_0 + n_1)
  $$

- **减法**：

  类似加法。

- **乘法**：

  类似加法。

**比较**：将此特定的原则与一般规则归纳原则进行比较。注意如何通过一次考虑一个求值规则来涵盖所有可能的规则实例。

---

### 4.3.2 对布尔表达式的规则归纳

布尔表达式的求值规则涉及到算术表达式的求值。它们共同定义了集合的一个子集：

$$
(\text{Aexp} \times \Sigma \times \mathbb{N})\ \cup\ (\text{Bexp} \times \Sigma \times T)
$$

一个对于推理布尔表达式的操作语义有用的原则是从针对 $\text{Bexp} \times \Sigma \times T$ 子集上的性质 $P(b, \sigma, t)$ 的规则归纳特殊原理得到的。

**规则归纳原理**：

$$
\forall b \in \text{Bexp},\ \sigma \in \Sigma,\ t \in T.\ (b, \sigma) \rightarrow t\ \Rightarrow\ P(b, \sigma, t)
$$

当且仅当：

- **常量**：

  $$
  \forall \sigma \in \Sigma.\ P(\text{true}, \sigma, \text{true})\ \&\ P(\text{false}, \sigma, \text{false})
  $$

- **相等比较（$=$）**：

  - 当 $m = n$ 时：

    $$
    \forall a_0, a_1 \in \text{Aexp},\ \sigma \in \Sigma,\ m, n \in \mathbb{N}.\ (a_0, \sigma) \rightarrow m\ \&\ (a_1, \sigma) \rightarrow n\ \&\ m = n\ \Rightarrow\ P(a_0 = a_1, \sigma, \text{true})
    $$

  - 当 $m \ne n$ 时：

    $$
    \forall a_0, a_1 \in \text{Aexp},\ \sigma \in \Sigma,\ m, n \in \mathbb{N}.\ (a_0, \sigma) \rightarrow m\ \&\ (a_1, \sigma) \rightarrow n\ \&\ m \ne n\ \Rightarrow\ P(a_0 = a_1, \sigma, \text{false})
    $$

- **小于等于（$\leq$）**：

  类似相等比较。

- **逻辑非（$\neg$）**：

  $$
  \forall b \in \text{Bexp},\ \sigma \in \Sigma,\ t \in T.\ (b, \sigma) \rightarrow t\ \&\ P(b, \sigma, t)\ \Rightarrow\ P(\neg b, \sigma, \neg t)
  $$

- **逻辑与（$\&$）**：

  $$
  \forall b_0, b_1 \in \text{Bexp},\ \sigma \in \Sigma,\ t_0, t_1 \in T.\ (b_0, \sigma) \rightarrow t_0\ \&\ P(b_0, \sigma, t_0)\ \&\ (b_1, \sigma) \rightarrow t_1\ \&\ P(b_1, \sigma, t_1)\ \Rightarrow\ P(b_0\ \&\ b_1, \sigma, t_0\ \&\ t_1)
  $$

- **逻辑或（$\text{or}$）**：

  类似逻辑与。

---

### 4.3.3 对命令的规则归纳

我们用于推理命令操作语义的规则归纳原则是规则归纳特殊原理的一个实例。命令的执行规则涉及到算术和布尔表达式的求值。

这些不同语法集的操作语义规则共同定义了集合的一个子集：

$$
(\text{Aexp} \times \Sigma \times \mathbb{N})\ \cup\ (\text{Bexp} \times \Sigma \times T)\ \cup\ (\text{Com} \times \Sigma \times \Sigma)
$$

我们对 $\text{Com} \times \Sigma \times \Sigma$ 子集上的性质 $P(c, \sigma, \sigma')$ 使用特殊的规则归纳原理。

**规则归纳原理**：

$$
\forall c \in \text{Com},\ \sigma, \sigma' \in \Sigma.\ (c, \sigma) \rightarrow \sigma'\ \Rightarrow\ P(c, \sigma, \sigma')
$$

当且仅当：

- **跳过（skip）**：

  $$
  \forall \sigma \in \Sigma.\ P(\text{skip}, \sigma, \sigma)
  $$

- **赋值**：

  $$
  \forall X \in \text{Loc},\ a \in \text{Aexp},\ \sigma \in \Sigma,\ m \in \mathbb{N}.\ (a, \sigma) \rightarrow m\ \Rightarrow\ P(X := a, \sigma, \sigma[m / X])
  $$

- **顺序组合**：

  $$
  \forall c_0, c_1 \in \text{Com},\ \sigma, \sigma', \sigma'' \in \Sigma.\ (c_0, \sigma) \rightarrow \sigma''\ \&\ P(c_0, \sigma, \sigma'')\ \&\ (c_1, \sigma'') \rightarrow \sigma'\ \&\ P(c_1, \sigma'', \sigma')\ \Rightarrow\ P(c_0;\ c_1, \sigma, \sigma')
  $$

- **条件语句（真分支）**：

  $$
  \forall c_0, c_1 \in \text{Com},\ b \in \text{Bexp},\ \sigma, \sigma' \in \Sigma.\ (b, \sigma) \rightarrow \text{true}\ \&\ (c_0, \sigma) \rightarrow \sigma'\ \&\ P(c_0, \sigma, \sigma')\ \Rightarrow\ P(\text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1, \sigma, \sigma')
  $$

- **条件语句（假分支）**：

  类似真分支。

- **循环语句（终止）**：

  $$
  \forall c \in \text{Com},\ b \in \text{Bexp},\ \sigma \in \Sigma.\ (b, \sigma) \rightarrow \text{false}\ \Rightarrow\ P(\text{while}\ b\ \text{do}\ c, \sigma, \sigma)
  $$

- **循环语句（继续）**：

  $$
  \forall c \in \text{Com},\ b \in \text{Bexp},\ \sigma, \sigma', \sigma'' \in \Sigma.\ (b, \sigma) \rightarrow \text{true}\ \&\ (c, \sigma) \rightarrow \sigma''\ \&\ P(c, \sigma, \sigma'')\ \&\ (\text{while}\ b\ \text{do}\ c, \sigma'') \rightarrow \sigma'\ \&\ P(\text{while}\ b\ \text{do}\ c, \sigma'', \sigma')\ \Rightarrow\ P(\text{while}\ b\ \text{do}\ c, \sigma, \sigma')
  $$

---

**示例**：

我们应用规则归纳来证明一个直观上显然的事实：如果一个位置 $Y$ 不在命令 $c$ 的赋值左侧出现，那么执行 $c$ 不会影响它的值。回想在第 3.5 节中给出的命令 $c$ 的位置集合 $\text{loc}_L(c)$ 的定义。

### **命题 4.7**

设 $Y \in \text{Loc}$。对于所有命令 $c$ 和状态 $\sigma, \sigma'$，

$$
Y \notin \text{loc}_L(c)\ \&\ (c, \sigma) \rightarrow \sigma'\ \Rightarrow\ \sigma(Y) = \sigma'(Y)
$$

**证明**：

设 $P$ 为以下性质：

$$
P(c, \sigma, \sigma') \quad \Leftrightarrow \quad Y \notin \text{loc}_L(c)\ \Rightarrow\ \sigma(Y) = \sigma'(Y)
$$

我们使用对命令的规则归纳来证明：

$$
\forall c \in \text{Com},\ \sigma, \sigma' \in \Sigma.\ (c, \sigma) \rightarrow \sigma'\ \Rightarrow\ P(c, \sigma, \sigma')
$$

- **基础情况**：

  - **跳过**：显然对于任何 $\sigma$，有 $P(\text{skip}, \sigma, \sigma)$。

- **赋值**：

  - 设 $X \in \text{Loc}$，$a \in \text{Aexp}$，$\sigma \in \Sigma$，$m \in \mathbb{N}$，且 $(a, \sigma) \rightarrow m$。

  - 如果 $Y \notin \text{loc}_L(X := a)$，那么 $Y \ne X$。

  - 因此，$\sigma(Y) = \sigma[m / X](Y)$。

  - 因此，$P(X := a, \sigma, \sigma[m / X])$。

- **顺序组合**：

  - 设 $c_0, c_1 \in \text{Com}$，$\sigma, \sigma', \sigma'' \in \Sigma$，且：

    $$
    (c_0, \sigma) \rightarrow \sigma''\ \&\ P(c_0, \sigma, \sigma'')\ \&\ (c_1, \sigma'') \rightarrow \sigma'\ \&\ P(c_1, \sigma'', \sigma')
    $$

  - 假设 $Y \notin \text{loc}_L(c_0; c_1)$。

    - 由于 $\text{loc}_L(c_0; c_1) = \text{loc}_L(c_0) \cup \text{loc}_L(c_1)$，所以 $Y \notin \text{loc}_L(c_0)$ 且 $Y \notin \text{loc}_L(c_1)$。

  - 根据归纳假设，$\sigma(Y) = \sigma''(Y)$，$\sigma''(Y) = \sigma'(Y)$。

  - 因此，$\sigma(Y) = \sigma'(Y)$，即 $P(c_0; c_1, \sigma, \sigma')$。

- **循环语句**（只考虑一个情况）：

  - 设 $c \in \text{Com}$，$b \in \text{Bexp}$，$\sigma, \sigma', \sigma'' \in \Sigma$，令 $w = \text{while}\ b\ \text{do}\ c$，且：

    $$
    (b, \sigma) \rightarrow \text{true}\ \&\ (c, \sigma) \rightarrow \sigma''\ \&\ P(c, \sigma, \sigma'')\ \&\ (w, \sigma'') \rightarrow \sigma'\ \&\ P(w, \sigma'', \sigma')
    $$

  - 假设 $Y \notin \text{loc}_L(w)$。

    - 因为 $\text{loc}_L(w) = \text{loc}_L(c)$，所以 $Y \notin \text{loc}_L(c)$。

  - 根据归纳假设，$\sigma(Y) = \sigma''(Y)$，$\sigma''(Y) = \sigma'(Y)$。

  - 因此，$\sigma(Y) = \sigma'(Y)$，即 $P(w, \sigma, \sigma')$。

- **其他情况**类似，留作练习。

**证毕。**

---

我们将在后续章节中看到更多通过规则归纳的证明。通常，它们将是流畅而直接的论证。以下是一些关于使用规则归纳的更困难的练习。正如前两个练习所表明的，有时规则归纳的应用可能会有些棘手。

---

**练习 4.8**

设 $w = \text{while}\ \text{true}\ \text{do}\ \text{skip}$。通过特殊的规则归纳证明：

$$
\forall \sigma, \sigma'.\ (w, \sigma) \not\rightarrow \sigma'
$$

**提示**：应用规则归纳的特殊原理，限制在集合：

$$
\{ (w, \sigma, \sigma')\ |\ \sigma, \sigma' \in \Sigma \}
$$

并将性质 $P(w, \sigma, \sigma')$ 定义为恒为假。

**解答**：

- **目标**：证明不存在 $\sigma, \sigma'$，使得 $(w, \sigma) \rightarrow \sigma'$。

- **方法**：使用规则归纳的特殊原理，定义性质 $P(w, \sigma, \sigma')$ 为恒假。

- **证明**：

  - **基础情况**：没有公理实例以 $(w, \sigma) \rightarrow \sigma'$ 为结论，因此无需考虑。

  - **归纳步骤**：对于所有规则实例 $(X\ /\ y)$，其中 $y = (w, \sigma) \rightarrow \sigma'$，且 $X \subseteq I_R$。

    - 由于性质 $P$ 恒为假，因此前提中的 $P$ 都是恒假。

    - 因此，无论前提如何，结论 $P(w, \sigma, \sigma')$ 仍为假。

- **结论**：根据规则归纳的特殊原理，$(w, \sigma) \rightarrow \sigma'$ 不可能存在。

---

虽然规则归纳可以用来代替对推导的归纳，但它并非万能；过度使用规则归纳有时会使证明更长、更令人困惑，这在尝试以下练习时可能会变得明显：

---

**练习 4.9**

采用简化的算术表达式语法：

$$
a ::= n\ |\ X\ |\ a_0 + a_1
$$

简化表达式的求值规则与之前相同：

1. 数字：

   $$
   \frac{}{\ (n, \sigma) \rightarrow n }
   $$

2. 位置：

   $$
   \frac{}{\ (X, \sigma) \rightarrow \sigma(X) }
   $$

3. 加法：

   $$
   \frac{ (a_0, \sigma) \rightarrow n_0 \quad (a_1, \sigma) \rightarrow n_1 }{ (a_0 + a_1, \sigma) \rightarrow n }
   $$

   其中 $n = n_0 + n_1$。

- 通过考虑推导的唯一形式，很容易看到 $(n, \sigma) \rightarrow m$ 蕴含 $m = n$。

- 你能看出这如何通过特殊的规则归纳得出吗？

- 使用规则归纳（而不是对推导的归纳）来证明表达式的求值是确定性的。

**提示**：对于后者，取归纳假设：

$$
P(a, \sigma, m) \quad \Leftrightarrow \quad \forall m' \in \mathbb{N}.\ (a, \sigma) \rightarrow m'\ \Rightarrow\ m = m'
$$

并准备在必要时进一步使用（特殊的）规则归纳。

**解答**：

- **目标**：证明对于所有 $a, \sigma$，如果 $(a, \sigma) \rightarrow m$ 且 $(a, \sigma) \rightarrow m'$，则 $m = m'$，即求值是确定性的。

- **方法**：使用规则归纳，对性质 $P(a, \sigma, m)$ 进行归纳。

- **基础情况**：

  - **数字**：

    - 对于 $(n, \sigma) \rightarrow n$，所以 $m = n$。

    - 因此，对于所有 $m'$，如果 $(n, \sigma) \rightarrow m'$，则 $m' = n$。

    - 所以 $P(n, \sigma, n)$ 成立。

  - **位置**：

    - 对于 $(X, \sigma) \rightarrow \sigma(X)$。

    - 所以 $m = \sigma(X)$。

    - 因此，对于所有 $m'$，如果 $(X, \sigma) \rightarrow m'$，则 $m' = \sigma(X)$。

    - 所以 $P(X, \sigma, \sigma(X))$ 成立。

- **归纳步骤**：

  - **加法**：

    - 假设对于 $a_0, a_1$，$P(a_0, \sigma, n_0)$，$P(a_1, \sigma, n_1)$ 成立。

    - 如果 $(a_0 + a_1, \sigma) \rightarrow m$，则存在 $n_0, n_1$，使得：

      $$
      (a_0, \sigma) \rightarrow n_0\ \&\ (a_1, \sigma) \rightarrow n_1\ \&\ m = n_0 + n_1
      $$

    - 由于 $P(a_0, \sigma, n_0)$，$P(a_1, \sigma, n_1)$，$n_0, n_1$ 是唯一确定的。

    - 因此，$m$ 也是唯一确定的。

    - 所以，$P(a_0 + a_1, \sigma, m)$ 成立。

- **结论**：根据规则归纳，表达式的求值是确定性的。

- **比较**：与第 3.3 节中使用结构归纳和考虑推导形式的证明相比，规则归纳的证明可能更繁琐。

---

**练习 4.10**（较长）

一个操作语义是第 2 章的，以 $(c, \sigma) \rightarrow \sigma'$ 为基础。另一个是先前在第 2.6 节中提到的**单步执行关系** $(c, \sigma) \rightarrow_1 (c', \sigma')$，但为简单起见，表达式的求值方式与第 2 章中完全相同。

例如，对于两个命令的顺序组合，有以下规则：

- 如果 $(c_0, \sigma) \rightarrow_1 (c_0', \sigma')$，则：

  $$
  (c_0;\ c_1,\ \sigma) \rightarrow_1 (c_0';\ c_1,\ \sigma')
  $$

- 如果 $(c_0, \sigma) \rightarrow_1 \sigma'$（即 $c_0$ 执行完毕），则：

  $$
  (c_0;\ c_1,\ \sigma) \rightarrow_1 (c_1,\ \sigma')
  $$

**任务**：

- 首先证明引理：

  $$
  \forall c_0, c_1, \sigma, \sigma'.\ (c_0, \sigma) \rightarrow^n \sigma'\ \Rightarrow\ (c_0;\ c_1, \sigma) \rightarrow^n (c_1, \sigma')
  $$

  其中 $\rightarrow^n$ 表示执行 $n$ 步。

- 证明方法：

  - **第一步**：对计算的长度 $n$ 进行数学归纳。

  - **第二步**：对从状态 $\sigma$ 开始执行 $c_0$ 的长度 $n$ 进行数学归纳。

- 然后，证明定理：

  $$
  \forall \sigma, \sigma'.\ (c, \sigma) \rightarrow^*\ \sigma'\ \text{当且仅当}\ (c, \sigma) \rightarrow \sigma'
  $$

  其中 $\rightarrow^*$ 表示执行任意多步直到终止。

- **证明方法**：

  - **“只有如果”方向**：可以对 $c$ 进行结构归纳，对于 $c$ 是 $\text{while}$ 循环的情况，对计算的长度进行归纳。

  - **“如果”方向**：可以使用规则归纳（或对推导的归纳）。

---

## 4.4 算子及其最小不动点

还有另一种看待由规则定义的集合的方法。规则实例集合 $R$ 确定了一个作用于集合上的算子 $R$，其对于给定的集合 $B$，结果为：

$$
R(B) = \{ y\ |\ \exists X \subseteq B.\ (X\ /\ y) \in R \}
$$

使用算子 $R$ 可以以另一种方式说明一个集合是 $R$-封闭的。

### **命题 4.11**

集合 $B$ 在 $R$ 下封闭，当且仅当 $R(B) \subseteq B$。

**证明**：

- 这一事实直接来自定义。

**证毕。**

---

算子 $R$ 提供了一种构建集合 $I_R$ 的方法。算子 $R$ 是**单调的（monotonic）**，即：

$$
A \subseteq B\ \Rightarrow\ R(A) \subseteq R(B)
$$

如果我们反复将 $R$ 应用于空集 $\emptyset$，我们得到一系列集合：

- $A_0 = R^0(\emptyset) = \emptyset$

- $A_1 = R^1(\emptyset) = R(\emptyset)$

- $A_2 = R^2(\emptyset) = R(R(\emptyset))$

- ...

集合 $A_1$ 包含所有公理实例的结论，一般地，集合 $A_{n+1}$ 包含所有前提在 $A_n$ 中的规则实例的结论。

显然，$\emptyset \subseteq R(\emptyset)$，即 $A_0 \subseteq A_1$。由于 $R$ 的单调性，我们得到 $R(A_0) \subseteq R(A_1)$，即 $A_1 \subseteq A_2$。类似地，我们得到 $A_2 \subseteq A_3$，等等。因此，这个序列形成一个链：

$$
A_0 \subseteq A_1 \subseteq A_2 \subseteq \dots
$$

取 $A = \bigcup_{n \in \omega} A_n$，我们有：

### **命题 4.12**

1. $A$ 是 $R$-封闭的。

2. $R(A) = A$。

3. $A$ 是最小的 $R$-封闭集合。

**证明**：

1. **证明 $A$ 是 $R$-封闭的**：

   - 假设 $(X\ /\ y) \in R$，且 $X \subseteq A$。

   - 由于 $A = \bigcup_n A_n$ 是一个递增的集合序列的并集，$X$ 是有限的集合，因此存在某个 $n$，使得 $X \subseteq A_n$。

   - 因此，$y \in R(A_n) = A_{n+1} \subseteq A$。

   - 因此，$A$ 是 $R$-封闭的。

2. **证明 $R(A) = A$**：

   - 由于 $A$ 是 $R$-封闭的，根据命题 4.11，$R(A) \subseteq A$。

   - 我们需要证明 $A \subseteq R(A)$。

   - 对于 $y \in A$，存在 $n$，使得 $y \in A_n$。

   - 如果 $n = 0$，则 $y \in \emptyset$，矛盾。

   - 因此，$n \geq 1$，$y \in A_n = R(A_{n-1})$。

   - 因此，存在 $X \subseteq A_{n-1} \subseteq A$，使得 $(X\ /\ y) \in R$。

   - 因此，$y \in R(A)$。

   - 所以，$A \subseteq R(A)$。

   - 综上，$R(A) = A$。

3. **证明 $A$ 是最小的 $R$-封闭集合**：

   - 假设 $B$ 是另一个 $R$-封闭的集合。

   - 我们通过数学归纳证明对于所有 $n$，$A_n \subseteq B$。

   - **基始情况**：$A_0 = \emptyset \subseteq B$。

   - **归纳步骤**：假设 $A_n \subseteq B$，则：

     $$
     A_{n+1} = R(A_n) \subseteq R(B) \subseteq B
     $$

     因为 $R$ 是单调的，且 $B$ 是 $R$-封闭的。

   - 因此，$A \subseteq B$。

**结论**：$A$ 是最小的 $R$-封闭集合。

---

注意在 (1) 的证明中，规则实例是有限的（即，在规则实例 $(X\ /\ y)$ 中，前提集合 $X$ 是有限的）起到了关键作用。

从 (1) 和 (3) 可以得出 $A = I_R$，即存在 $R$-推导的元素的集合。

(2) 表明 $I_R$ 正是 $R$ 的不动点，即 $R(I_R) = I_R$。

(3) 表明 $I_R$ 是 $R$ 的**最小不动点（least fixed point）**，即：

$$
I_R = \text{fix}(R) = \bigcup_{n \in \omega} R^n(\emptyset)
$$

因为如果任何其他集合 $B$ 是不动点，则它是 $R$-封闭的，所以根据命题 4.1，$I_R \subseteq B$。

最小不动点将在下一章中发挥核心作用。

---

**练习 4.13**

给定规则实例集合 $R$，定义另一个算子 $R'$，如下：

$$
R'(A) = A\ \cup\ \{ y\ |\ \exists X \subseteq A.\ (X\ /\ y) \in R \}
$$

显然，$R'$ 是单调的，并且满足性质：

$$
A \subseteq R'(A)
$$

满足这种性质的算子称为**增大的（increasing）**。展示一个单调但非增大的算子。证明对于任何集合 $A$，$R'$ 存在一个包含 $A$ 的最小不动点，并且这种性质对于单调算子可能不成立。

**解答**：

- **展示一个单调但非增大的算子**：

  - 考虑 $R(A) = \emptyset$，它是单调的，因为对于任何 $A \subseteq B$，有 $R(A) = \emptyset \subseteq \emptyset = R(B)$。

  - 但它不是增大的，因为对于任何 $A$，$R(A) = \emptyset$，而 $A \nsubseteq \emptyset$，除非 $A = \emptyset$。

- **证明 $R'$ 存在一个包含 $A$ 的最小不动点**：

  - 定义序列：

    $$
    A_0 = A,\ A_{n+1} = R'(A_n)
    $$

  - 由于 $R'$ 是增大的，$A_n \subseteq A_{n+1}$。

  - 令 $L = \bigcup_{n \in \omega} A_n$，则 $L$ 是 $R'$ 的不动点。

  - 证明 $L$ 是最小的包含 $A$ 的不动点。

- **对于单调算子，可能不存在包含 $A$ 的最小不动点**。

---

**练习 4.14**

设 $R$ 是一组规则实例。证明 $R$ 是**连续的（continuous）**，即对于任何递增的集合链 $B_0 \subseteq B_1 \subseteq \dots$，有：

$$
R\left( \bigcup_{n \in \omega} B_n \right) = \bigcup_{n \in \omega} R(B_n)
$$

**解答**：

- **证明**：

  - **左到右**：

    - 对于 $y \in R\left( \bigcup_n B_n \right)$，存在 $X \subseteq \bigcup_n B_n$，使得 $(X\ /\ y) \in R$。

    - 由于 $X$ 是有限的，所以存在 $k$，使得 $X \subseteq B_k$。

    - 因此，$y \in R(B_k) \subseteq \bigcup_n R(B_n)$。

  - **右到左**：

    - 对于 $y \in \bigcup_n R(B_n)$，存在 $n$，使得 $y \in R(B_n)$。

    - 因此，$y \in R\left( \bigcup_n B_n \right)$。

- **结论**：$R$ 是连续的。

---

## 4.5 进一步阅读

本章提供了对归纳定义的数学理论的初步介绍。一个详细但更难的论述可以在 Peter Aczel 的手册章节 [4] 中找到——我们的处理仅涉及有限的规则，避免了使用序数。术语“规则归纳”起源于作者在 1984 年的剑桥讲义，并似乎正在流行起来（这一原则是众所周知的，例如，在 [4] 中，对于规则 $R$，它被简单地称为 $R$-归纳）。

本章没有对在推理操作语义时应使用哪种论证风格提出任何建议；是使用规则归纳，还是使用通常更笨拙但概念上更直观的对推导的归纳。在许多情况下，这是一种品味问题。

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



### ---------------------------



### ----------------------------