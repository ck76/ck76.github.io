[toc]



# 第12章 信息系统

## 12.1 递归类型

**背景**：

- 在编程语言的语义中，我们经常需要处理**递归类型**（recursive types）。
- 递归类型涉及到**域方程**（domain equations），即在定义类型时，类型自身以某种方式出现在其定义中。

**示例**：

考虑以下递归域方程：

$$
X = 1 + X
$$

**解释**：

- 这里，$X$ 是一个**完全偏序集**（cpo，complete partial order），满足 $X$ 等于 $1$ 与 $X$ 的和。
- $1$ 表示具有一个元素的 cpo。

**解决方案**：

- 一个解决方案是自然数的离散 cpo，即 $\omega$，但这不是唯一的解决方案。

**编程语言中的递归类型**：

- 许多编程语言允许定义递归类型，例如链表、树等。
- 即使语言本身不直接支持递归类型，我们也可以在语义描述中使用递归定义的 cpo。

**动态绑定**：

- 编程特性如**动态绑定**（dynamic binding）也可以方便地用递归类型来建模。

**Scott 的贡献**：

- **Dana Scott** 通过发现一个 $\lambda$-演算的模型，实现了以下递归类型定义的非平凡（非单例）解：

  $$
  D \cong [D \rightarrow D]
  $$

  **解释**：

  - 这不是一个严格的等式，而是 $D$ 与 $[D \rightarrow D]$ 同构。
  - 这强调了我们不一定需要在严格意义上的解，同构关系就足够了。

---

**问题**：我们如何定义递归类型？

**方法**：

- 我们可以使用**归纳定义**（inductive definitions）来定义递归类型。

**示例**：整数的有限列表

- 列表可以被定义为满足以下等式的集合 $L$：

  $$
  L = \{()\} + (\mathbb{N} \times L)
  $$

  **解释**：

  - $()\ $ 表示空列表。
  - 将整数 $n$ 添加到列表 $l$ 的开头，可以表示为 $(n, l)$。

- 有限列表并不是唯一的解，如果 $L$ 包含了整数的有限和无限列表，那么它也是一个解。

**最小解**：

- 但如果我们将 $L$ 定义为仅包含整数的**有限列表**，那么它就是满足上述等式的**最小集合**。

**证明**：

- 这可以通过识别有限列表的定义符合第4章中讨论的归纳定义的模式来实现。
- $L$ 满足上述等式需要满足以下条件：

  - $L$ 包含空列表 $()$。
  - $L$ 对于将整数添加到列表的操作是闭合的。

- 因此，$L$ 是满足以下规则的最小集合：

  - $()\ \in L$。
  - 如果 $n \in \mathbb{N}$ 且 $l \in L$，那么 $(n, l) \in L$。

**等价表示**：

- 我们可以将有限列表视为算子 $\psi$ 的不动点，其中：

  $$
  \psi(X) = \{()\} + (\mathbb{N} \times X)
  $$

- $\psi$ 是作用于集合上的单调且连续的算子。

---

### 练习12.1

**问题**：

描述如何将具有整数叶子的二叉树类型定义为一个归纳定义。

**解答**：

**定义**：

- 定义集合 $T$，满足：

  $$
  T = \mathbb{N} + (T \times T)
  $$

  **解释**：

  - $\mathbb{N}$ 表示叶子节点，包含整数值。
  - $(T \times T)$ 表示一个内部节点，其左右子树也是树。

**归纳定义规则**：

- **基础情况**：对于每个 $n \in \mathbb{N}$，$n \in T$（叶子节点）。
- **归纳步骤**：如果 $t_1, t_2 \in T$，那么 $(t_1, t_2) \in T$（内部节点）。

**结论**：

- $T$ 是满足上述规则的最小集合，即具有整数叶子的二叉树的集合。

---

### 练习12.2

**问题**：

描述一个集合，它是域方程 $X = 1 + X$ 的解，但不与自然数 $\omega$ 同构。

**解答**：

**构造**：

- 定义集合 $X$ 包含所有有限和无限长度的序列，其中每个元素都是单位元素 $*$：

  $$
  X = \{(), (*), (*, *), (*, *, *), \dots\} + \{\text{无限序列} (*, *, *, \dots)\}
  $$

- 这里，$1$ 表示空序列 $()$。

**验证**：

- $X = 1 + X$：

  - $1$ 对应于空序列 $()$。
  - $X$ 包含所有序列，包括无限序列。

**结论**：

- 由于 $X$ 包含无限序列，所以它与自然数 $\omega$（仅包含有限序列）不同，因此不与 $\omega$ 同构。

---

**问题**：有些递归类型无法直接用归纳定义来处理，例如无限的流（streams）或“有终止符的序列”（stoppered sequences）。

**示例**：流或“有终止符的序列”

- 我们可以猜测，流是以下 cpo 方程的最小解：

  $$
  L = (\{\$\} + \mathbb{N} \times L)_\bot
  $$

  **解释**：

  - $\{\$\}$ 表示终止符。
  - $\mathbb{N} \times L$ 表示一个整数后跟随一个流。

**挑战**：

- 使用归纳定义或类似的推理方式，我们无法得到无限序列，例如：

  $$
  (n_1, (n_2, (n_3, \dots)))
  $$

- 这提示我们需要一种方法，将有限元素通过某种形式的**完备过程**（completion process）来构建出无限元素。

---

**信息系统的引入**：

- **信息系统**（Information Systems）提供了一种通过表示有限元素的符号来构建 cpo 的方法。
- 因为它们只显式地处理有限元素，所以可以使用归纳定义的技术，并可以递归地定义。

**信息系统的概念**：

- 信息系统可以被视为一个**处方**，说明如何构建一个 cpo。
- 详细来说，信息系统可以被认为是由一些断言或命题组成的集合，这些断言与**蕴含关系**（entailment）和**一致性关系**（consistency）相关。

**信息系统与 cpo 的关系**：

- 信息系统确定了一个 cpo，其元素是那些关于可能计算的真命题的集合，这些集合需要满足一致性和闭合性的条件。
- 在信息系统中，元素的 cpo 按照集合包含关系排序。

---

## 12.2 信息系统

### 概念和定义

**信息系统的组成**：

- **令牌集** $A$：表示可能关于计算的断言或命题。
- **一致性关系** $\text{Con} \subseteq \text{Fin}(A)$：表示哪些有限的令牌集合是**一致的**，即可以同时为真。
- **蕴含关系** $\vdash \subseteq (\text{Con} \setminus \{\emptyset\}) \times A$：表示哪些令牌集合可以**蕴含**其他令牌。

**符号说明**：

- $\text{Fin}(A)$：$A$ 的所有有限子集的集合。
- $X \subseteq_{\text{fin}} A$：$X$ 是 $A$ 的有限子集。

**信息系统的定义**：

- 一个**信息系统** $A = (A, \text{Con}, \vdash)$ 满足以下条件：

  1. **单调性**：

     $$
     X \subseteq Y \in \text{Con} \implies X \in \text{Con}
     $$

     **解释**：一致性的子集仍然是一致的。

  2. **原子一致性**：

     $$
     a \in A \implies \{a\} \in \text{Con}
     $$

     **解释**：单个令牌组成的集合是一致的。

  3. **蕴含的一致性**：

     $$
     X \vdash a \implies X \cup \{a\} \in \text{Con}
     $$

     **解释**：如果 $X$ 蕴含 $a$，则 $X \cup \{a\}$ 一致。

  4. **反身性**：

     $$
     X \in \text{Con},\ a \in X \implies X \vdash a
     $$

     **解释**：令牌集合蕴含其自身的元素。

  5. **传递性**：

     $$
     \begin{aligned}
     & X, Y \in \text{Con},\ \forall b \in Y,\ X \vdash b,\ Y \vdash c \\
     & \implies X \vdash c
     \end{aligned}
     $$

     **解释**：如果 $X$ 蕴含 $Y$ 的所有元素，且 $Y$ 蕴含 $c$，则 $X$ 蕴含 $c$。

**特殊约定**：

- 我们假设 $\vdash \subseteq (\text{Con} \setminus \{\emptyset\}) \times A$，即**空集不蕴含任何令牌**。

**说明**：

- **一致性关系** $\text{Con}$ 选择了哪些有限的令牌集合可以同时为真。
- **蕴含关系** $\vdash$ 指定了哪些令牌集合可以推导出其他令牌。

---

### 信息系统的元素

**直观理解**：

- 令牌表示关于计算的断言，且一旦某个令牌对计算为真，它将一直为真。
- 信息系统的元素可以被视为关于一个可能计算的**真令牌的集合**，即该计算的**信息内容**。

**元素的定义**：

- 信息系统 $A = (A, \text{Con}, \vdash)$ 的**元素**是满足以下条件的 $A$ 的子集 $x$：

  1. **非空性**：

     $$
     x \ne \emptyset
     $$

  2. **一致性**：

     $$
     \forall X \subseteq_{\text{fin}} x,\ X \in \text{Con}
     $$

     **解释**：$x$ 的所有有限子集都是一致的。

  3. **$\vdash$-闭合性**：

     $$
     \forall X \subseteq_{\text{fin}} x,\ X \vdash a \implies a \in x
     $$

     **解释**：如果 $X$ 蕴含 $a$，且 $X \subseteq x$，则 $a \in x$。

**说明**：

- 元素 $x$ 表示一个关于可能计算的真令牌的集合，需要满足一致性和蕴含闭合性。

**cpo 结构**：

- 元素按集合包含关系排序，即 $x \subseteq y$。

- 信息系统的元素集合 $|A|$ 组成了一个 cpo。

---

### 命题12.3

**陈述**：

- 一个信息系统的元素按包含关系排序，形成一个 cpo。

**证明**：

- **目标**：证明元素集合 $|A|$ 按包含关系排序，形成一个 cpo。

- **给定**：一个信息系统 $A = (A, \text{Con}, \vdash)$。

- **证明步骤**：

  1. **有向完备性**：

     - 设 $(x_n)_{n \in \mathbb{N}}$ 是 $|A|$ 中的一个链，即 $x_0 \subseteq x_1 \subseteq \dots$。
     - 定义 $x = \bigcup_{n \in \mathbb{N}} x_n$。
     - **非空性**：由于每个 $x_n$ 都非空，因此 $x$ 非空。
     - **一致性**：对任意有限集合 $X \subseteq_{\text{fin}} x$，由于 $X$ 有限，存在 $n$ 使得 $X \subseteq x_n$，因此 $X \in \text{Con}$。
     - **$\vdash$-闭合性**：对于 $X \subseteq_{\text{fin}} x$，如果 $X \vdash a$，由于 $X \subseteq x_n$，且 $x_n$ 是元素，所以 $a \in x_n \subseteq x$。
     - **结论**：$x \in |A|$，即 $\bigcup_{n \in \mathbb{N}} x_n$ 是 $|A|$ 的上确界。

  2. **因此**，$|A|$ 是一个 cpo。

---

### Lemma 12.4 引理

**陈述**：

- 设 $A = (A, \text{Con}, \vdash)$ 是一个信息系统。

- 设 $X \in \text{Con}$ 且 $X \ne \emptyset$，$Y$ 是 $A$ 的有限子集。

  1. 如果对于 $Y$ 中的每个 $b$，都有 $X \vdash b$，那么 $X \cup Y \in \text{Con}$ 且 $Y \in \text{Con}$。

  2. 集合 $x = \{a \in A \mid X \vdash a\}$ 是 $A$ 的一个元素。

**证明**：

1. **部分 (1)**：

   - **目标**：证明如果 $X \vdash b$ 对于所有 $b \in Y$，则 $X \cup Y \in \text{Con}$ 且 $Y \in \text{Con}$。

   - **证明**：

     - **归纳法**：对 $Y$ 的大小进行归纳。

     - **基础情况**：当 $Y = \emptyset$ 时，显然成立。

     - **归纳步骤**：假设 $Y$ 有 $k+1$ 个元素，记 $Y = Y' \cup \{b'\}$。

       - 假设对于 $Y'$，$X \vdash b$ 对所有 $b \in Y'$，根据归纳假设，$X \cup Y' \in \text{Con}$，且 $Y' \in \text{Con}$。

       - 由于 $X \vdash b'$，根据信息系统的公理3，$X \cup Y' \vdash b'$。

       - 根据公理3，$X \cup Y = X \cup Y' \cup \{b'\} \in \text{Con}$。

       - 根据公理1，$Y = Y' \cup \{b'\} \in \text{Con}$。

2. **部分 (2)**：

   - **目标**：证明集合 $x = \{a \in A \mid X \vdash a\}$ 是 $A$ 的一个元素。

   - **证明**：

     - **非空性**：由于 $X \ne \emptyset$，并且根据公理4，对于 $a \in X$，有 $X \vdash a$，因此 $x \ne \emptyset$。

     - **一致性**：对于任意有限 $X' \subseteq_{\text{fin}} x$，由于每个 $a \in X'$ 都满足 $X \vdash a$，根据部分 (1)，$X \cup X' \in \text{Con}$。

       - 因此，$X' \in \text{Con}$（根据公理1）。

     - **$\vdash$-闭合性**：对于任意有限 $X' \subseteq_{\text{fin}} x$，如果 $X' \vdash a$，则由于 $X \vdash X'$（因为 $X \vdash$ 每个 $a \in X'$），再加上 $X' \vdash a$，根据公理5，有 $X \vdash a$，因此 $a \in x$。

     - **结论**：$x$ 是 $A$ 的一个元素。

---

### 蕴含关系的扩展

**符号定义**：

- 对于一致的有限集合 $X, Y \in \text{Con}$，我们定义 $X \vdash^* Y$，当且仅当对于所有 $a \in Y$，有 $X \vdash a$。

- **特殊情况**：由于 $\vdash \subseteq (\text{Con} \setminus \{\emptyset\}) \times A$，因此 $\emptyset \vdash^* Y$ 当且仅当 $Y = \emptyset$。

**性质**：

- **合并**：如果 $X \vdash^* Y$ 且 $X \vdash^* Y'$，则 $X \vdash^* Y \cup Y'$。

- **传递性**：如果 $X \vdash^* Y$ 且 $Y \vdash^* Z$，则 $X \vdash^* Z$。

---

### 闭包操作

**定义**：

- 对于 $X \subseteq A$，定义 $X^\vdash = \{a \in A \mid \exists Z \subseteq_{\text{fin}} X,\ Z \vdash a\}$。

- **注意**：由于 $\emptyset \vdash a$ 从不成立（因为 $\vdash \subseteq (\text{Con} \setminus \{\emptyset\}) \times A$），因此 $\emptyset^\vdash = \emptyset$。

---

## 12.3 闭合族和 Scott 预域

**目标**：

- 这一节我们将刻画那些由信息系统的元素构成的 cpo。

- 引入 **Scott 预域**（Scott predomains）的概念。

---

### 闭合族的定义

**定义**：

- 一个**闭合族**（closed family）是一个满足以下条件的集合族 $\mathcal{F}$，其中每个元素都是一个可数集的子集：

  1. **非空性**：对于所有 $x \in \mathcal{F}$，有 $x \ne \emptyset$。

  2. **有向完备性**：对于任何链 $x_0 \subseteq x_1 \subseteq \dots$，如果对所有 $n$，$x_n \in \mathcal{F}$，则 $\bigcup_{n} x_n \in \mathcal{F}$。

  3. **有限交闭合性**：对于任何非空的 $U \subseteq \mathcal{F}$，如果 $\bigcap U \ne \emptyset$，则 $\bigcap U \in \mathcal{F}$。

---

### 定理12.5

**陈述**：

1. **(i)** 如果 $A$ 是一个信息系统，则 $|A|$ 是一个闭合族。

2. **(ii)** 如果 $\mathcal{F}$ 是一个闭合族，则可以定义一个信息系统 $I(\mathcal{F}) = (A_\mathcal{F}, \text{Con}_\mathcal{F}, \vdash_\mathcal{F})$。

3. **(iii)** 函数 $A \mapsto |A|$ 和 $\mathcal{F} \mapsto I(\mathcal{F})$ 是互逆的映射，即如果 $A$ 是一个信息系统，则 $I(|A|) = A$；如果 $\mathcal{F}$ 是一个闭合族，则 $|I(\mathcal{F})| = \mathcal{F}$。

**证明思路**：

- **(i)** 部分：证明信息系统的元素构成的集合 $|A|$ 满足闭合族的三个条件。

- **(ii)** 部分：给定闭合族 $\mathcal{F}$，构造信息系统 $I(\mathcal{F})$，并验证其满足信息系统的公理。

- **(iii)** 部分：证明映射的互逆性，即 $I(|A|) = A$ 和 $|I(\mathcal{F})| = \mathcal{F}$。

---

### (i) 证明 $|A|$ 是一个闭合族

**给定**：信息系统 $A = (A, \text{Con}, \vdash)$。

**证明**：

1. **非空性**：对于所有 $x \in |A|$，有 $x \ne \emptyset$。这是元素定义的第一条。

2. **有向完备性**：设 $(x_n)$ 是 $|A|$ 中的链，则 $x = \bigcup_{n} x_n \in |A|$。这在命题12.3中已经证明。

3. **有限交闭合性**：设 $U \subseteq |A|$ 非空，且 $\bigcap U \ne \emptyset$。需要证明 $\bigcap U \in |A|$。

   - **非空性**：由于 $\bigcap U \ne \emptyset$。

   - **一致性**：对于任意有限集合 $X \subseteq_{\text{fin}} \bigcap U$，由于 $X \subseteq u$ 对于所有 $u \in U$，而每个 $u \in |A|$，所以 $X \in \text{Con}$。

   - **$\vdash$-闭合性**：对于任意有限 $X \subseteq_{\text{fin}} \bigcap U$，如果 $X \vdash a$，则对于每个 $u \in U$，$a \in u$，因此 $a \in \bigcap U$。

   - **结论**：$\bigcap U \in |A|$。

---

### (ii) 构造 $I(\mathcal{F})$ 并证明其是信息系统

**构造**：

- **令牌集**：$A_\mathcal{F} = \bigcup \mathcal{F}$。

- **一致性关系**：

  $$
  X \in \text{Con}_\mathcal{F} \iff X = \emptyset \text{ 或 } \exists x \in \mathcal{F},\ X \subseteq_{\text{fin}} x
  $$

- **蕴含关系**：

  $$
  X \vdash_\mathcal{F} a \iff X \ne \emptyset,\ X \in \text{Con}_\mathcal{F},\ a \in A_\mathcal{F},\ \forall x \in \mathcal{F},\ X \subseteq x \implies a \in x
  $$

**验证信息系统公理**：

1. **公理1（单调性）**：如果 $X \subseteq Y \in \text{Con}_\mathcal{F}$，则 $X \in \text{Con}_\mathcal{F}$。

   - **证明**：因为 $Y \subseteq_{\text{fin}} y$ 对某个 $y \in \mathcal{F}$，且 $X \subseteq Y$，所以 $X \subseteq_{\text{fin}} y$，因此 $X \in \text{Con}_\mathcal{F}$。

2. **公理2（原子一致性）**：对于任何 $a \in A_\mathcal{F}$，有 $\{a\} \in \text{Con}_\mathcal{F}$。

   - **证明**：因为 $a \in x$ 对某个 $x \in \mathcal{F}$，所以 $\{a\} \subseteq x$，因此 $\{a\} \in \text{Con}_\mathcal{F}$。

3. **公理3（蕴含的一致性）**：如果 $X \vdash_\mathcal{F} a$，则 $X \cup \{a\} \in \text{Con}_\mathcal{F}$。

   - **证明**：对于所有 $x \in \mathcal{F}$，若 $X \subseteq x$，则 $a \in x$，因此 $X \cup \{a\} \subseteq x$，所以 $X \cup \{a\} \in \text{Con}_\mathcal{F}$。

4. **公理4（反身性）**：如果 $X \in \text{Con}_\mathcal{F}$ 且 $a \in X$，则 $X \vdash_\mathcal{F} a$。

   - **证明**：对于所有 $x \in \mathcal{F}$，如果 $X \subseteq x$，则 $a \in x$，因为 $a \in X$，因此 $X \vdash_\mathcal{F} a$。

5. **公理5（传递性）**：如果 $X, Y \in \text{Con}_\mathcal{F}$，对于所有 $b \in Y$，有 $X \vdash_\mathcal{F} b$，且 $Y \vdash_\mathcal{F} c$，则 $X \vdash_\mathcal{F} c$。

   - **证明**：对于所有 $x \in \mathcal{F}$，如果 $X \subseteq x$，则由于 $X \vdash_\mathcal{F} b$，所以 $b \in x$，因此 $Y \subseteq x$，又由于 $Y \vdash_\mathcal{F} c$，所以 $c \in x$，因此 $X \vdash_\mathcal{F} c$。

---

### (iii) 映射的互逆性

**证明**：

1. **证明 $I(|A|) = A$**：

   - **令牌集**：$A_{|A|} = \bigcup |A| = A$。

   - **一致性关系**：对于 $X \subseteq_{\text{fin}} A$，有：

     $$
     X \in \text{Con} \iff X = \emptyset \text{ 或 } X \subseteq x \text{ 对某个 } x \in |A|
     $$

     - 由于 $A$ 的元素是 $A$ 的非空、一致、$\vdash$-闭合的子集。

     - 因此，对于 $X \in \text{Con}$，要么 $X = \emptyset$，要么 $X$ 是某个元素的有限子集。

   - **蕴含关系**：

     $$
     X \vdash a \iff X \ne \emptyset,\ X \in \text{Con},\ \forall x \in |A|,\ X \subseteq x \implies a \in x
     $$

     - 这与原来的 $\vdash$ 定义一致。

2. **证明 $|I(\mathcal{F})| = \mathcal{F}$**：

   - **从 $\mathcal{F} \subseteq |I(\mathcal{F})|$**：

     - 对于 $x \in \mathcal{F}$，由于 $x$ 是非空、一致、$\vdash$-闭合的集合，因此 $x \in |I(\mathcal{F})|$。

   - **从 $|I(\mathcal{F})| \subseteq \mathcal{F}$**：

     - 设 $x \in |I(\mathcal{F})|$。

     - 对 $x$ 进行可数枚举 $x = \{a_0, a_1, \dots\}$。

     - 定义 $x_n = \{a_0, a_1, \dots, a_n\}$。

     - 每个 $x_n$ 是 $x$ 的有限子集，且由于 $x$ 是元素，$x_n \in \text{Con}_{\mathcal{F}}$。

     - 由于 $x_n \subseteq x$，且 $x \in |I(\mathcal{F})|$，所以 $x_n \in \mathcal{F}$。

     - $\mathcal{F}$ 是闭合族，因此 $\bigcup_{n} x_n = x \in \mathcal{F}$。

---

### 练习12.6

**问题**：

完成 (ii) 部分的证明。

**解答**：

- 我们已经在上面的 (ii) 部分中构造了信息系统 $I(\mathcal{F})$，并验证了信息系统的公理。

- 需要确保所有公理都得到验证，且理解这些公理在这个构造中的应用。

---

### Scott 预域

**定义**：

- 一个 cpo 的元素 $x$ 是**有限的**，如果对于任何链 $d_0 \leq d_1 \leq \dots$，只要 $x \leq \bigvee_{n} d_n$，就存在某个 $n$ 使得 $x \leq d_n$。

- 一个 cpo $D$ 是 **$\omega$-代数**的（$\omega$-algebraic），如果它的有限元素集合 $D_0$ 是可数的，并且对于每个 $x \in D$，存在有限元素的链 $e_0 \leq e_1 \leq \dots$，使得 $x = \bigvee_{n} e_n$。

- 一个 cpo 是 **有界完备**的，如果每个非空有界子集都有上确界。

- 当 cpo 有底元素时，如果它是有界完备且 $\omega$-代数的，则称为 **Scott 域**（Scott domain）。

- 如果 cpo 不一定有底元素，但满足有界完备性和 $\omega$-代数性，则称为 **Scott 预域**（Scott pre-domain）。

---

### 命题12.8

**陈述**：

- 设 $A = (A, \text{Con}, \vdash)$ 是一个信息系统。则其元素 $|A|$，按包含关系排序，构成一个 Scott 预域。

- 其有限元素是形式为 $X = \{a \in A \mid X \vdash a\}$，其中 $X \in \text{Con}$ 且 $X \ne \emptyset$。

**证明**：

1. **有界完备性**：

   - 设 $V \subseteq |A|$ 非空，且对于某个 $y \in |A|$，有 $x \subseteq y$ 对所有 $x \in V$。

   - 定义 $U = \{z \in |A| \mid x \subseteq z\ \forall x \in V\}$。

   - 由于 $V$ 非空，存在 $v \in V$，且 $v \ne \emptyset$。

   - 因此，$\bigcap U$ 非空。

   - 由于 $|A|$ 是闭合族，$\bigcap U \in |A|$，且是 $V$ 的上确界。

2. **$\omega$-代数性**：

   - 对于 $x \in |A|$，我们可以对其元素进行可数枚举 $x = \{a_0, a_1, \dots\}$。

   - 定义有限集合 $x_n = \{a_0, a_1, \dots, a_n\}$。

   - 每个 $x_n$ 是 $A$ 的有限、一致的子集。

   - 定义 $X_n = \{a \in A \mid x_n \vdash a\}$。

   - 这些 $X_n$ 是 $|A|$ 的有限元素。

   - 有 $X_0 \subseteq X_1 \subseteq \dots$，且 $x = \bigcup_{n} X_n$。

3. **有限元素的刻画**：

   - 如果 $x \in |A|$ 是有限元素，则存在某个 $n$，使得 $x = X_n$。

   - 因此，$x$ 是某个有限一致集合的闭包。

---

### Scott 预域与信息系统的关系

- 一个 Scott 预域可以自然地与一个信息系统相关联，令牌集由有限元素组成。

- 一致性和蕴含关系由原始域诱导。

- 这样，信息系统可以表示 Scott 预域。

---

### 练习12.11

**问题**：

设 $\mathbb{N}$ 和 $T$ 是自然数和真值的（离散的）Scott 预域。证明它们的函数空间 $[\mathbb{N} \rightarrow T]$ 不是一个 Scott 预域，因此不能表示为信息系统。

**提示**：

- 考虑 $[\mathbb{N} \rightarrow T]$ 的有限元素，它们的数量是多少？

- 如果有限元素的数量是不可数的，则不能是 Scott 预域。

---

**解答**：

- 在 $[\mathbb{N} \rightarrow T]$ 中，有限元素对应于有**有限支撑**的函数，即只在有限个点上非恒等于 $\bot$。

- 然而，$\mathbb{N}$ 是可数的，因此有 $2^{\aleph_0}$ 个不同的有限支撑函数。

- 因此，$[\mathbb{N} \rightarrow T]$ 的有限元素集合是不可数的。

- 因此，$[\mathbb{N} \rightarrow T]$ 不是 $\omega$-代数的，不是 Scott 预域。

---

## 12.4 总结

- 信息系统提供了一种表示 Scott 预域的方法。

- 信息系统的元素形成的 cpo 按包含关系排序，具有 Scott 预域的性质。

- 虽然不是所有的 cpo 都能由信息系统表示，但这类 cpo 已经足够丰富，可以描述许多重要的递归类型。

- 在信息系统中，我们可以使用归纳定义和递归的方法来构造复杂的类型。

---

### 练习12.12

**问题**：

- cpo 有时使用**有向集**的概念来表示，而不是 $\omega$-链。

- **有向集** $S$ 是一个非空子集，对于任意 $s, t \in S$，存在 $u \in S$，使得 $s, t \leq u$。

- **完全偏序集**有时被定义为所有有向集都有上确界的偏序集。

- 在这个框架中，cpo 的有限元素 $e$ 定义为：如果 $e \leq \bigvee S$，其中 $S$ 是有向集，则存在 $s \in S$，使得 $e \leq s$。

- $\omega$-代数的 cpo 定义为：对于任意 $x \in D$，集合 $S = \{e \leq x \mid e\ \text{是有限的}\}$ 是有向的，且 $x = \bigvee S$。如果有限元素集合是可数的，则称为 $\omega$-代数。

- 证明在这种意义下的 $\omega$-代数 cpo 与我们之前定义的 $\omega$-代数 cpo 是相同的。

- 还要证明，如果在闭合族的定义中，将条件2替换为：

  $$
  \text{如果 } S \text{ 是 } (\mathcal{F}, \subseteq) \text{ 的有向子集，则 } \bigcup S \in \mathcal{F}
  $$

  那么定义的集合族与之前的相同。

**解答**：

- **等价性**：

  - 在两种定义中，有限元素都是指那些在某种极限过程中可以被某个近似所覆盖的元素。

- **有向完备性与 $\omega$-完备性**：

  - 在 Scott 预域中，我们只需要考虑 $\omega$-链，因为所有的有向集都可以通过某种方式表示为 $\omega$-链的上确界。

- **闭合族的条件**：

  - 将条件2替换为有向集的情形后，闭合族的定义仍然确保了元素的非空性、有向完备性和有限交闭合性。

- **结论**：

  - 因此，两种定义在本质上是等价的，定义了相同的 cpo 类和集合族。

---

# 总结

通过对信息系统的详细讨论，我们了解了如何构建和表示 Scott 预域，以及如何利用信息系统来定义和解决递归类型的问题。这为下一章深入探讨递归类型的操作语义和指称语义奠定了基础。

### ---------------------------

# 第12章 信息系统

## 12.4 信息系统的 cpo（完全偏序集）

**背景**：

- 我们已经引入了**信息系统**（information systems）来表示某些 cpo，即 Scott 预域。
- 现在，我们想要解决**递归域方程**（recursive domain equations），即涉及递归类型的方程。
- 通过使用信息系统的具体表示，我们可以通过在信息系统的**完全偏序集**上构造不动点来解决这些递归方程。

**目标**：

- 定义一个信息系统的完全偏序集，使其具有最小元（bottom element）。
- 通过引入信息系统之间的**子系统关系**（subsystem relation），即偏序关系 $\sqsubseteq$。
- 研究在这个偏序集上连续的操作，以用于递归地定义信息系统。

---

### 信息系统之间的子系统关系 $\sqsubseteq$

**定义**：

- 设有两个信息系统 $A = (A, \text{Con}_A, \vdash_A)$ 和 $B = (B, \text{Con}_B, \vdash_B)$。
- 我们说 $A \sqsubseteq B$，当且仅当满足以下条件：

  1. **令牌集包含关系**：
  
     $$
     A \subseteq B
     $$
  
  2. **一致性关系的保持**：
  
     $$
     X \in \text{Con}_A \iff X \subseteq A \ \text{且} \ X \in \text{Con}_B
     $$
  
  3. **蕴含关系的保持**：
  
     $$
     X \vdash_A a \iff X \subseteq A \ \text{且} \ a \in A \ \text{且} \ X \vdash_B a
     $$
  
- 当 $A \sqsubseteq B$ 时，我们称 $A$ 是 $B$ 的一个**子系统**（subsystem）。

**解释**：

- **令牌集的包含**：$A$ 的令牌集是 $B$ 的令牌集的子集。
- **一致性和蕴含关系的继承**：$A$ 的一致性和蕴含关系是 $B$ 中对应关系的**限制**。

**性质**：

- **命题12.13**：

  如果两个信息系统 $A$ 和 $B$ 满足 $A = B$ 且 $A \sqsubseteq B$，则 $A = B$。

  **证明**：

  - 由于 $A = B$，且根据定义，$A \sqsubseteq B$，因此从定义中可知，$A$ 的一致性和蕴含关系与 $B$ 的一致性和蕴含关系完全相同。
  - 因此，$A = B$。

---

### 信息系统的 cpo 结构

**构建 cpo**：

- **最小信息系统**：存在一个最小的信息系统 $0 = (\emptyset, \{\emptyset\}, \emptyset)$。
  - 令牌集为空集。
  - 一致性关系只包含空集。
  - 蕴含关系为空集。

- **有向链的上确界**：对于信息系统的一个 $\omega$-链（即可数递增序列）$A_0 \sqsubseteq A_1 \sqsubseteq \dots \sqsubseteq A_i \sqsubseteq \dots$，其上确界可以通过取令牌集、一致性关系和蕴含关系的并集来构造。

**需要注意**：

- 信息系统的集合可能是**真类**（proper class），即不是一个集合，因此严格来说，它们不构成一个集合上的 cpo。
- 但我们可以将它们视为一个“大 cpo”，这对于我们的讨论已经足够。

---

### 定理12.14

**陈述**：

- 关系 $\sqsubseteq$ 是一个偏序关系，且 $0 = (\emptyset, \{\emptyset\}, \emptyset)$ 是其最小元。
- 对于信息系统的一个 $\omega$-链 $A_0 \sqsubseteq A_1 \sqsubseteq \dots \sqsubseteq A_i \sqsubseteq \dots$，存在一个最小的上确界 $A = (\bigcup_i A_i, \bigcup_i \text{Con}_i, \bigcup_i \vdash_i)$。

**证明**：

1. **偏序性**：

   - **自反性**：显然，对于任何信息系统 $A$，有 $A \sqsubseteq A$。
   - **传递性**：如果 $A \sqsubseteq B$ 且 $B \sqsubseteq C$，则 $A \sqsubseteq C$。
   - **反对称性**：如果 $A \sqsubseteq B$ 且 $B \sqsubseteq A$，则根据命题12.13，$A = B$。

2. **最小元**：

   - 信息系统 $0$ 的令牌集为空集，一致性关系为 $\{\emptyset\}$，蕴含关系为空集。
   - 对于任何信息系统 $A$，有 $0 \sqsubseteq A$。

3. **上确界的存在性**：

   - **构造**：设 $A_i = (A_i, \text{Con}_i, \vdash_i)$，定义 $A = (\bigcup_i A_i, \bigcup_i \text{Con}_i, \bigcup_i \vdash_i)$。
   - **验证 $A$ 是信息系统**：需要验证信息系统的五个公理。

     - **公理1（单调性）**：如果 $X \subseteq Y \in \text{Con}$，则 $X \in \text{Con}$。

       - 由于 $\text{Con} = \bigcup_i \text{Con}_i$，如果 $Y \in \text{Con}_i$，且 $X \subseteq Y$，则 $X \in \text{Con}_i$，因此 $X \in \text{Con}$。

     - **公理2（原子一致性）**：对于任何 $a \in A$，$\{a\} \in \text{Con}$。

       - 由于 $a \in A$，则存在某个 $i$，使得 $a \in A_i$，因此 $\{a\} \in \text{Con}_i$，从而 $\{a\} \in \text{Con}$。

     - **公理3（蕴含的一致性）**、**公理4（反身性）**、**公理5（传递性）**类似验证。

   - **$A$ 是上确界**：

     - 对于每个 $A_i$，显然 $A_i \sqsubseteq A$。

     - 如果 $B$ 是 $A_i$ 的上界，则 $A \sqsubseteq B$。

---

### 连续操作

**目标**：

- 我们希望定义在信息系统上的**连续操作**，以用于递归地定义信息系统。

**定义**：

- 一个作用于信息系统的操作 $F$ 是**单调的**，如果对于所有信息系统 $A$ 和 $B$，有：

  $$
  A \sqsubseteq B \implies F(A) \sqsubseteq F(B)
  $$

- $F$ 是**连续的**，如果对于任何 $\omega$-链 $(A_i)$，有：

  $$
  \bigcup_i F(A_i) = F\left( \bigcup_i A_i \right)
  $$

- **注意**：由于 $F$ 是单调的，$\bigcup_i F(A_i)$ 存在。

**应用**：

- 类似于在 cpo 上的不动点定理，我们可以得到任何连续操作 $F$ 都有一个最小的不动点 $\text{fix}(F)$，由序列 $F^i(0)$ 的上确界给出：

  $$
  \text{fix}(F) = \bigcup_i F^i(0)
  $$

---

### 引理12.15

**陈述**：

- 如果一个一元操作 $F$ 在信息系统的令牌集上是**连续的**，并且在 $\sqsubseteq$ 下是单调的，那么 $F$ 是连续的。

**定义**：

- **在令牌集上的连续性**：对于一个一元操作 $F$，如果对于任何 $\omega$-链 $(A_i)$，$F\left( \bigcup_i A_i \right)$ 的令牌集中的每个令牌都是某个 $F(A_i)$ 的令牌，则称 $F$ 在令牌集上是连续的。

**证明**：

- **必要性（“仅当”方向）**：显然，如果 $F$ 是连续的，则它在令牌集上连续。

- **充分性（“如果”方向）**：

  - 假设 $F$ 是单调的，并且在令牌集上连续。

  - 设有 $\omega$-链 $(A_i)$，则有 $F\left( \bigcup_i A_i \right) \sqsubseteq \bigcup_i F(A_i)$（因为 $F$ 是单调的）。

  - 由于 $F$ 在令牌集上连续，$\bigcup_i F(A_i)$ 和 $F\left( \bigcup_i A_i \right)$ 的令牌集相同。

  - 根据命题12.13，这两个信息系统相等，即 $F\left( \bigcup_i A_i \right) = \bigcup_i F(A_i)$。

---

### 多元操作的连续性

- 对于多个信息系统作为参数的操作，我们可以逐个参数地考虑它的单调性和连续性。

- **结论**：

  - 一个操作是连续的，当且仅当它对每个参数都是单调和连续的。

---

### 为什么选择 $\sqsubseteq$ 而不是包含关系？

- 可能有人会问，为什么不直接使用信息系统之间的包含关系作为偏序关系？

- **原因**：

  - 因为在信息系统上的**提升的函数空间**（lifted-function-space）构造在左参数下不是单调的（参见练习12.34）。

  - 使用 $\sqsubseteq$ 可以确保所有我们需要的构造都是单调和连续的。

---

### 练习12.16

**问题**：

- 这个练习将信息系统上的子系统关系 $\sqsubseteq$ 与集合族和 cpo 上的相应关系联系起来。

- 设 $A = (A, \text{Con}_A, \vdash_A)$ 和 $B = (B, \text{Con}_B, \vdash_B)$ 是信息系统。

1. **(i)** 假设 $A \sqsubseteq B$。定义映射：

   - $\theta: |A| \to |B|$，其中：

     $$
     \theta(x) = \{b \in B \mid \exists X \subseteq_{\text{fin}} x,\ X \vdash_B b\}
     $$

   - $\varphi: |B| \to |A| \cup \{\emptyset\}$，其中：

     $$
     \varphi(y) = y \cap A
     $$

   - **证明**：$\theta$ 和 $\varphi$ 关于包含关系是连续的，并且满足对于所有 $x \in |A|$ 和 $y \in |B|$：

     $$
     \varphi(\theta(x)) = x,\quad \theta(\varphi(y)) \subseteq y
     $$

2. **(ii)** 对于信息系统 $A$ 和 $B$，证明：

   $$
   A \sqsubseteq B \iff |A| = \{y \cap A \mid y \in |B|,\ y \cap A \ne \emptyset\}
   $$

   **提示**：这表明另一种通过连续函数的嵌入-投影对（embedding-projection pairs）来解决递归域方程的方法。

---

**解答**：

1. **(i) 部分**：

   **证明 $\theta$ 和 $\varphi$ 是连续的，并满足所要求的性质**。

   **首先，定义映射**：

   - **$\theta: |A| \to |B|$**，对于 $x \in |A|$，定义：

     $$
     \theta(x) = \{b \in B \mid \exists X \subseteq_{\text{fin}} x,\ X \vdash_B b\}
     $$

   - **$\varphi: |B| \to |A| \cup \{\emptyset\}$**，对于 $y \in |B|$，定义：

     $$
     \varphi(y) = y \cap A
     $$

   **证明 $\theta$ 和 $\varphi$ 是连续的**：

   - **连续性**：指映射在包含关系下是单调且保序的。

   - **$\theta$ 的连续性**：

     - 对于 $x_1 \subseteq x_2$，有 $\theta(x_1) \subseteq \theta(x_2)$。

     - 因为 $x_1 \subseteq x_2$，因此任何 $b$，如果存在 $X \subseteq_{\text{fin}} x_1$，使得 $X \vdash_B b$，则 $X \subseteq x_2$，所以 $b \in \theta(x_2)$。

   - **$\varphi$ 的连续性**：

     - 对于 $y_1 \subseteq y_2$，有 $\varphi(y_1) = y_1 \cap A \subseteq y_2 \cap A = \varphi(y_2)$。

   **证明 $\varphi(\theta(x)) = x$**：

   - 对于 $x \in |A|$，有：

     $$
     \varphi(\theta(x)) = \theta(x) \cap A
     $$

   - 由于 $A \sqsubseteq B$，$A \subseteq B$。

   - 对于 $b \in \theta(x)$，$b \in B$ 且存在 $X \subseteq_{\text{fin}} x$，使得 $X \vdash_B b$。

   - 如果 $b \in A$，则 $b \in \theta(x) \cap A$。

   - 因此：

     $$
     \varphi(\theta(x)) = \theta(x) \cap A = \{b \in A \mid \exists X \subseteq_{\text{fin}} x,\ X \vdash_B b\}
     $$

   - 由于 $A \sqsubseteq B$，对于 $X \subseteq_{\text{fin}} x$，$X \vdash_B b$ 等价于 $X \vdash_A b$。

   - 因此：

     $$
     \varphi(\theta(x)) = \{b \in A \mid \exists X \subseteq_{\text{fin}} x,\ X \vdash_A b\} = x
     $$

     - 因为 $x$ 是 $A$ 的一个元素，满足 $\vdash_A$-闭合性。

   **证明 $\theta(\varphi(y)) \subseteq y$**：

   - 对于 $y \in |B|$，有：

     $$
     \theta(\varphi(y)) = \{b \in B \mid \exists X \subseteq_{\text{fin}} y \cap A,\ X \vdash_B b\}
     $$

   - 由于 $X \subseteq y \cap A \subseteq y$，且 $y$ 是 $B$ 的一个元素，满足 $\vdash_B$-闭合性，因此 $b \in y$。

   - 因此，$\theta(\varphi(y)) \subseteq y$。

2. **(ii) 部分**：

   **证明**：

   - **“$\implies$”方向**：

     - 假设 $A \sqsubseteq B$。

     - 对于任何 $y \in |B|$，若 $y \cap A \ne \emptyset$，则 $y \cap A \in |A|$。

       - 因为 $y$ 是 $B$ 的元素，满足一致性和 $\vdash_B$-闭合性。

       - $y \cap A$ 的有限子集是 $A$ 的有限子集，且由于 $A \sqsubseteq B$，一致性和蕴含关系在 $A$ 中继承。

     - 因此，$|A| = \{y \cap A \mid y \in |B|,\ y \cap A \ne \emptyset\}$。

   - **“$\impliedby$”方向**：

     - 假设 $|A| = \{y \cap A \mid y \in |B|,\ y \cap A \ne \emptyset\}$。

     - 需要证明 $A \sqsubseteq B$。

     - **令牌集**：$A \subseteq B$ 显然成立。

     - **一致性**：

       - 对于 $X \subseteq_{\text{fin}} A$，$X \in \text{Con}_A$ 当且仅当存在 $y \in |B|$，使得 $X \subseteq y$ 且 $y \cap A \ne \emptyset$。

       - 因此，$X \in \text{Con}_B$。

     - **蕴含关系**：

       - 对于 $X \subseteq_{\text{fin}} A$，$a \in A$，有 $X \vdash_A a$ 当且仅当对于所有 $y \in |B|$，如果 $X \subseteq y$，则 $a \in y$。

       - 这与 $X \vdash_B a$ 等价。

     - 因此，$A \sqsubseteq B$。

---

## 12.5 构造

在这一节中，我们将定义信息系统的各种构造，包括：

- **提升（Lifting）**
- **和（Sum）**
- **积（Product）**
- **提升的函数空间（Lifted Function Space）**

这些构造将诱导相应的 cpo 构造。我们会仔细选择这些构造，使得它们在 $\sqsubseteq$ 下是连续的。这使得我们能够用这些构造递归地定义信息系统和 cpo。

---

### 12.5.1 提升（Lifting）

**目标**：

- 定义信息系统上的提升操作，使其反映 cpo 上的提升。

**定义**：

- 对于信息系统 $A = (A, \text{Con}, \vdash)$，定义其提升 $A^\bot = (A', \text{Con}', \vdash')$，其中：

  1. **令牌集**：

     $$
     A' = \text{Con}
     $$

  2. **一致性关系**：

     $$
     X \in \text{Con}' \iff X \subseteq \text{Con} \ \text{且} \ \bigcup X \in \text{Con}
     $$

  3. **蕴含关系**：

     $$
     X \vdash' b \iff X \ne \emptyset,\ X \in \text{Con}',\ \bigcup X \vdash b
     $$

**直观解释**：

- 提升操作扩展了原始的令牌集，包括一个新的令牌（在此构造中为空集 $\emptyset$），即使在没有原始输出值的情况下也为真。

- 提升在元素的家族前面添加了一个元素，即只包含额外令牌 $\emptyset$ 的元素。

**定义**：

- 定义信息系统 $1 = \bot^\bot$。

  - 这个信息系统只有一个令牌 $\emptyset$，一致性集合为 $\{\emptyset, \{\emptyset\}\}$，蕴含关系为 $\{\emptyset\} \vdash \emptyset$。

  - 它唯一的元素是 $\{\emptyset\}$。

---

**命题12.17**

**陈述**：

- 对于信息系统 $A$，$A^\bot$ 是一个信息系统，且其元素满足：

  $$
  y \in |A^\bot| \iff y = \{\emptyset\} \ \text{或} \ \exists x \in |A|,\ y = \{b \mid b \subseteq_{\text{fin}} x\}
  $$

**证明**：

- **验证信息系统的公理**：需要验证 $A^\bot$ 满足信息系统的五个公理。

  - **这里只验证公理5（传递性）**，其他公理的验证过程类似。

  - **公理5**：假设 $X \vdash' b$ 对于所有 $b \in Y$，且 $Y \vdash' c$，需要证明 $X \vdash' c$。

    - 首先，$X \ne \emptyset$，因为如果 $X = \emptyset$，则 $Y = \emptyset$，那么 $Y \vdash' c$ 不可能。

    - 由于 $X \vdash' b$ 对所有 $b \in Y$，根据定义，有 $\bigcup X \vdash b$。

    - 因此，$\bigcup X \vdash^* \bigcup Y$。

    - 又因为 $Y \vdash' c$，所以 $\bigcup Y \vdash c$。

    - 由于原信息系统 $A$ 满足公理5，所以有 $\bigcup X \vdash c$。

    - 因此，$X \vdash' c$。

- **证明元素的刻画**：

  - **“$\implies$”方向**：

    - 如果 $y \in |A^\bot|$，则要么 $y = \{\emptyset\}$，要么存在 $x \in |A|$，使得 $y = \{b \mid b \subseteq_{\text{fin}} x\}$。

  - **“$\impliedby$”方向**：

    - 如果 $y = \{\emptyset\}$，则显然 $y \in |A^\bot|$。

    - 如果存在 $x \in |A|$，使得 $y = \{b \mid b \subseteq_{\text{fin}} x\}$，则需要验证 $y \in |A^\bot|$。

    - **非空性**：$y \ne \emptyset$，因为 $x \ne \emptyset$。

    - **一致性**：对于任意有限 $X \subseteq y$，$\bigcup X \subseteq_{\text{fin}} x$，且 $x \in |A|$，所以 $\bigcup X \in \text{Con}$，因此 $X \in \text{Con}'$。

    - **$\vdash'$-闭合性**：类似地验证。

---

**推论12.18**

- 提升操作在信息系统的元素 cpo 上诱导了 cpo 的提升。

- 存在 cpo 的同构：

  $$
  |A^\bot| \cong |A|^\bot
  $$

- 同构映射为：

  $$
  y \mapsto \begin{cases}
  \bot, & \text{如果 } y = \{\emptyset\} \\
  x, & \text{否则，存在 } x \in |A|,\ y = \{b \mid b \subseteq_{\text{fin}} x\}
  \end{cases}
  $$

---

**定理12.19**

**陈述**：

- 操作 $A \mapsto A^\bot$ 是在 $\sqsubseteq$ 下的连续操作。

**证明**：

- **单调性**：

  - 假设 $A \sqsubseteq B$，需要证明 $A^\bot \sqsubseteq B^\bot$。

  - **令牌集的包含**：$A' = \text{Con}_A \subseteq \text{Con}_B = B'$。

  - **一致性关系**：

    - $X \in \text{Con}_A'$ 当且仅当 $X \subseteq \text{Con}_A$ 且 $\bigcup X \in \text{Con}_A$。

    - 由于 $A \sqsubseteq B$，$\text{Con}_A \subseteq \text{Con}_B$，所以 $\text{Con}_A' \subseteq \text{Con}_B'$。

  - **蕴含关系**：

    - $X \vdash_A' c$ 当且仅当 $X \ne \emptyset$，$X \in \text{Con}_A'$，$c \in A'$，且 $\bigcup X \vdash_A c$。

    - 由于 $A \sqsubseteq B$，蕴含关系保持，因此 $A^\bot \sqsubseteq B^\bot$。

- **在令牌集上的连续性**：

  - 设有 $\omega$-链 $A_0 \sqsubseteq A_1 \sqsubseteq \dots$，则：

    - $\bigcup_i A_i^\bot$ 的令牌集为 $\bigcup_i \text{Con}_{A_i}$。

    - 而 $(\bigcup_i A_i)^\bot$ 的令牌集为 $\text{Con}_{\bigcup_i A_i}$。

    - 这两个集合相等。

  - 因此，根据引理12.15，提升操作是连续的。

---

### 练习12.20

**问题**：

- 绘制信息系统 $1^\bot$ 和 $1^{\bot\bot}$ 的域。

**解答**：

- **$1$ 的信息系统**：

  - 令牌集：$\{\emptyset\}$。

  - 元素：$\{\{\emptyset\}\}$。

- **$1^\bot$**：

  - 令牌集：$\text{Con}_{1} = \{\emptyset, \{\emptyset\}\}$。

  - 元素：

    - $y = \{\emptyset\}$，对应于 $\bot$。

    - $y = \{b \mid b \subseteq_{\text{fin}} x\}$，其中 $x$ 是 $1$ 的元素，即 $x = \{\emptyset\}$，因此 $y = \{\emptyset, \{\emptyset\}\}$。

- **$1^{\bot\bot}$**：

  - 继续对 $1^\bot$ 进行提升，构造其提升信息系统。

- **绘制**：

  - 可以将这些信息系统的元素按包含关系绘制成 Hasse 图，显示其 cpo 结构。

---

### 练习12.21

**问题**：

- 因为提升是关于 $\sqsubseteq$ 的连续操作，所以它有一个最小的不动点 $N = N^\bot$。

- 求出 $N$ 的令牌集，并证明其元素 cpo $|N|$ 同构于之前见过的同名 cpo，它由一个 $\omega$-链和一个额外的“无穷”元素（作为上确界）组成。

**解答**：

- **构造**：

  - 从最小的信息系统 $0$ 开始，不断应用提升操作。

  - $N = N^\bot$，因此 $N$ 的令牌集为 $\text{Con}_N$。

- **令牌集**：

  - 令牌集是 $\text{Con}_N$，包含所有有限一致的令牌集合。

- **元素 cpo**：

  - 元素由 $\{\emptyset\}$ 以及由有限一致集生成的闭包组成。

- **同构**：

  - $|N|$ 的元素形成一个 $\omega$-链，其上有一个额外的“无穷”元素作为上确界。

  - 这与之前的 cpo 同构。

---

### 练习12.22

**问题**：

- 对于信息系统 $A$，设 $X$ 是 $A^\bot$ 的一致集，$b$ 是 $A$ 的令牌。

- 证明：

  $$
  X \vdash' b \iff \bigcup X \vdash b
  $$

**解答**：

- 根据 $A^\bot$ 的蕴含关系定义，有：

  - $X \vdash' b \iff X \ne \emptyset,\ X \in \text{Con}',\ \bigcup X \vdash b$

- 因此，直接根据定义，可以得到所要求的结果。

---

### 12.5.2 和（Sums）

**目标**：

- 定义信息系统的和操作，反映 cpo 上的和。

**构造**：

- 我们通过并列两个信息系统的不相交副本来形成和。

**定义**：

- 设 $A = (A, \text{Con}_A, \vdash_A)$ 和 $B = (B, \text{Con}_B, \vdash_B)$ 是信息系统。

- 定义它们的和 $A + B$ 为 $C = (C, \text{Con}, \vdash)$，其中：

  1. **令牌集**：

     $$
     C = A \uplus B = (\{1\} \times A) \cup (\{2\} \times B)
     $$

     - 这里，$\uplus$ 表示不相交并集。

     - 定义嵌入映射 $\text{inj}_1: A \to C$，$\text{inj}_1(a) = (1, a)$。

     - 定义嵌入映射 $\text{inj}_2: B \to C$，$\text{inj}_2(b) = (2, b)$。

  2. **一致性关系**：

     $$
     X \in \text{Con} \iff \exists Y \in \text{Con}_A,\ X = \text{inj}_1(Y) \ \text{或} \ \exists Y \in \text{Con}_B,\ X = \text{inj}_2(Y)
     $$

  3. **蕴含关系**：

     $$
     X \vdash c \iff
     \begin{cases}
     \exists Y, a,\ X = \text{inj}_1(Y),\ c = \text{inj}_1(a),\ Y \vdash_A a, \\
     \text{或} \\
     \exists Y, b,\ X = \text{inj}_2(Y),\ c = \text{inj}_2(b),\ Y \vdash_B b.
     \end{cases}
     $$

**示例**：

- 设 $T = 1 + 1$，则 $|T|$ 同构于真值的离散 cpo。

  - 令牌集：$(1, \emptyset)$ 和 $(2, \emptyset)$。

  - 元素：$\{(1, \emptyset)\}$ 和 $\{(2, \emptyset)\}$。

---

**命题12.23**

**陈述**：

- 设 $A$ 和 $B$ 是信息系统，则它们的和 $A + B$ 是一个信息系统，且其元素满足：

  $$
  x \in |A + B| \iff \exists y \in |A|,\ x = \text{inj}_1(y) \ \text{或} \ \exists y \in |B|,\ x = \text{inj}_2(y)
  $$

**证明**：

- **验证信息系统的公理**：逐个验证五个公理，利用 $A$ 和 $B$ 满足公理。

- **元素的刻画**：元素是 $A$ 和 $B$ 的元素的不相交副本。

---

**推论12.24**

- 信息系统的和的元素 cpo 同构于它们元素 cpo 的和：

  $$
  |A + B| \cong |A| + |B|
  $$

---

**定理12.25**

**陈述**：

- 操作 $+$ 在 $\sqsubseteq$ 下是连续的。

**证明**：

- **单调性**：

  - 假设 $A \sqsubseteq A'$，需要证明 $A + B \sqsubseteq A' + B$。

  - **令牌集的包含**：$A + B$ 的令牌集包含在 $A' + B$ 的令牌集内。

  - **一致性关系和蕴含关系的保持**：由于 $A \sqsubseteq A'$，$A$ 的一致性和蕴含关系在 $A'$ 中继承。

- **在令牌集上的连续性**：

  - 对于 $\omega$-链 $(A_i)$，有：

    $$
    \bigcup_i (A_i + B) = \left( \bigcup_i A_i \right) + B
    $$

  - 因此，根据引理12.15，$+$ 是连续的。

---

### 12.5.3 积（Product）

**目标**：

- 定义信息系统的积操作，反映 cpo 上的积。

**定义**：

- 设 $A = (A, \text{Con}_A, \vdash_A)$ 和 $B = (B, \text{Con}_B, \vdash_B)$ 是信息系统。

- 定义它们的积 $A \times B$ 为 $C = (C, \text{Con}, \vdash)$，其中：

  1. **令牌集**：

     $$
     C = A \times B
     $$

  2. **一致性关系**：

     $$
     X \in \text{Con} \iff \text{proj}_1(X) \in \text{Con}_A \ \text{且} \ \text{proj}_2(X) \in \text{Con}_B
     $$

     - 其中，$\text{proj}_1: A \times B \to A$，$\text{proj}_1(a, b) = a$。

     - $\text{proj}_2: A \times B \to B$，$\text{proj}_2(a, b) = b$。

  3. **蕴含关系**：

     $$
     X \vdash (a, b) \iff \text{proj}_1(X) \vdash_A a \ \text{且} \ \text{proj}_2(X) \vdash_B b
     $$

**解释**：

- 积的信息系统的元素是来自两个信息系统的元素的**对**。

---

**命题12.26**

**陈述**：

- $A \times B$ 是一个信息系统，且其元素满足：

  $$
  x \in |A \times B| \iff \exists x_1 \in |A|,\ x_2 \in |B|,\ x = x_1 \times x_2
  $$

**证明**：

- **验证信息系统的公理**：逐个验证五个公理。

- **元素的刻画**：

  - “$\implies$”方向：

    - 设 $x \in |A \times B|$，定义 $x_1 = \text{proj}_1(x)$，$x_2 = \text{proj}_2(x)$。

    - 可以验证 $x = x_1 \times x_2$。

  - “$\impliedby$”方向：

    - 如果 $x_1 \in |A|$，$x_2 \in |B|$，则 $x = x_1 \times x_2 \in |A \times B|$。

---

**推论12.27**

- 信息系统的积的元素 cpo 同构于它们元素 cpo 的积：

  $$
  |A \times B| \cong |A| \times |B|
  $$

---

**定理12.28**

**陈述**：

- 操作 $\times$ 在 $\sqsubseteq$ 下是连续的。

**证明**：

- **单调性**：

  - 假设 $A \sqsubseteq A'$，需要证明 $A \times B \sqsubseteq A' \times B$。

  - **令牌集的包含**：$A \times B$ 的令牌集包含在 $A' \times B$ 的令牌集内。

  - **一致性关系和蕴含关系的保持**：由于 $A \sqsubseteq A'$，$A$ 的一致性和蕴含关系在 $A'$ 中继承。

- **在令牌集上的连续性**：

  - 对于 $\omega$-链 $(A_i)$，有：

    $$
    \bigcup_i (A_i \times B) = \left( \bigcup_i A_i \right) \times B
    $$

  - 因此，根据引理12.15，$\times$ 是连续的。

---

### 12.5.4 提升的函数空间（Lifted Function Space）

**挑战**：

- 对于一般的 $A$ 和 $B$，无法直接表示连续函数空间 $[|A| \to |B|]$。

- 我们希望定义的是提升的函数空间 $[|A| \to |B|^\bot]$。

**定义**：

- 设 $A = (A, \text{Con}_A, \vdash_A)$ 和 $B = (B, \text{Con}_B, \vdash_B)$ 是信息系统。

- 定义它们的提升的函数空间 $A \to B^\bot$ 为 $C = (C, \text{Con}, \vdash)$，其中：

  1. **令牌集**：

     $$
     C = (\text{Con}_A \setminus \{\emptyset\}) \times \text{Con}_B \cup \{(\emptyset, \emptyset)\}
     $$

  2. **一致性关系**：

     $$
     \{(X_1, Y_1), \dots, (X_n, Y_n)\} \in \text{Con} \iff \forall I \subseteq \{1, \dots, n\},\ \bigcup_{i \in I} X_i \in \text{Con}_A \implies \bigcup_{i \in I} Y_i \in \text{Con}_B
     $$

  3. **蕴含关系**：

     $$
     \{(X_1, Y_1), \dots, (X_n, Y_n)\} \vdash (X, Y) \iff \{(X_1, Y_1), \dots, (X_n, Y_n)\} \ne \emptyset,\ \bigcup_{X_i \vdash_A X} Y_i \vdash_B Y
     $$

**直观解释**：

- 函数空间的令牌 $(X, Y)$ 表示：如果函数的输入满足 $X$，则输出满足 $Y$。

---

**引理12.29**

**陈述**：

- $A \to B^\bot$ 是一个信息系统。

- 一个关系 $r \subseteq \text{Con}_A \times \text{Con}_B$ 是 $A \to B^\bot$ 的元素，当且仅当 $r$ 满足：

  1. **(a)** $X = \emptyset \implies Y = \emptyset$。

  2. **(b)** $X r Y$ 且 $X r Y' \implies X r (Y \cup Y')$。

  3. **(c)** $X' \vdash_A X$，$X r Y$，$Y \vdash_B Y' \implies X' r Y'$。

**证明**：

- **验证信息系统的公理**，特别是公理3和公理5。

- **元素的刻画**：

  - **“仅当”方向**：假设 $r$ 是 $A \to B^\bot$ 的元素，则 $r$ 满足 (a)、(b)、(c)。

  - **“如果”方向**：假设 $r$ 满足 (a)、(b)、(c)，需要证明 $r$ 是 $A \to B^\bot$ 的元素。

    - 验证 $r$ 是非空、一致、$\vdash$-闭合的。

---

**定理12.30**

**陈述**：

- 存在一个同构：

  $$
  |A \to B^\bot| \cong [|A| \to |B|^\bot]
  $$

- 同构映射为：

  - **从 $A \to B^\bot$ 的元素 $r$ 到连续函数 $f$**：

    $$
    f(x) = \bigcup \{Y \mid \exists X \subseteq x,\ (X, Y) \in r\}
    $$

  - **从连续函数 $f$ 到 $A \to B^\bot$ 的元素 $r$**：

    $$
    r = \{(X, Y) \mid X \ne \emptyset,\ Y \subseteq f(X)\} \cup \{(\emptyset, \emptyset)\}
    $$

**证明**：

- **验证映射是良定义的**。

- **证明映射是互逆的**。

- **利用引理12.29 中的性质**。

---

**定理12.33**

**陈述**：

- 提升的函数空间是关于 $\sqsubseteq$ 的连续操作。

**证明**：

- **单调性**：

  - 假设 $A \sqsubseteq A'$，需要证明 $A \to B^\bot \sqsubseteq A' \to B^\bot$。

  - **令牌集的包含**：$A \to B^\bot$ 的令牌集包含在 $A' \to B^\bot$ 的令牌集内。

  - **一致性关系和蕴含关系的保持**：由于 $A \sqsubseteq A'$，$A$ 的一致性和蕴含关系在 $A'$ 中继承。

- **在令牌集上的连续性**：

  - 对于 $\omega$-链 $(A_i)$，有：

    $$
    \bigcup_i (A_i \to B^\bot) = \left( \bigcup_i A_i \right) \to B^\bot
    $$

  - 因此，根据引理12.15，提升的函数空间是连续的。

---

### 例子

- 操作 $X \mapsto (X \to X^\bot)$ 是一个连续操作。

- 它有一个最小的不动点 $D = (D \to D^\bot)$。

- 这个信息系统的元素 cpo $D$ 满足：

  $$
  D \cong [D \to D^\bot]
  $$

---

### 练习12.34

**问题**：

- 为什么我们使用 $\sqsubseteq$ 构造一个大 cpo，而不是基于包含关系的更简单的偏序关系？

- 验证提升的函数空间构造在包含关系下的左参数上不是单调的。

**解答**：

- **原因**：

  - 如果使用包含关系作为偏序关系，则提升的函数空间构造在左参数上可能不保持单调性。

- **验证**：

  - 构造反例，显示存在 $A \subseteq A'$，但 $A \to B^\bot$ 并不包含在 $A' \to B^\bot$ 中。

  - 因此，包含关系不适合作为偏序关系，而 $\sqsubseteq$ 能确保所有构造的单调性和连续性。

---

## 12.6 进一步阅读

- **信息系统的起源**：Dana Scott 在 [90] 中引入了信息系统。

- **通常的使用**：信息系统通常用于表示具有底元的 Scott 域。

- **相关书籍**：

  - [87] 提供了基于信息系统的域理论的介绍，适合本科生。

- **拓展**：

  - 信息系统可以被视为“无点拓扑”（pointless topology）中的特殊邻域表示。

  - 信息系统的概念可以推广，用于表示更广泛的域。

---

# 总结

在本章中，我们详细讨论了信息系统的 cpo 结构，以及如何利用信息系统解决递归域方程。我们定义了信息系统之间的子系统关系 $\sqsubseteq$，并证明了它形成了一个偏序关系。通过引入各种构造（提升、和、积、提升的函数空间），我们可以在信息系统上定义连续操作，并利用这些操作递归地定义信息系统和 cpo。这为在编程语言的形式语义中处理递归类型提供了坚实的基础。

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