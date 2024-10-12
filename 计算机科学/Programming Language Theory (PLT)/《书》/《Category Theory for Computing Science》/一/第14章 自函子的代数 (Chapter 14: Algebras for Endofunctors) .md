[toc]



好的，让我们全面、详细地解析**第14章：幂端函子的代数（Algebras for Endofunctors）**。本章深入探讨了基于幂端函子的各种构造，包括固定点、代数、递归范畴、三元组（Triples）以及斯科特域（Scott Domains）。通过具体的定义、定理和例子，您将能够理解这些高级范畴论概念及其在数学和计算机科学中的应用。

## 整体概览

### 章节背景与重要性

在范畴论中，**幂端函子**（Endofunctor）是指从一个范畴 $\mathcal{C}$ 到其自身的函子，即 $F: \mathcal{C} \to \mathcal{C}$。幂端函子的代数结构（Algebras for Endofunctors）为理解和构造各种数学和计算机科学中的结构提供了强大的工具。本章主要探讨以下几个方面：

1. **幂端函子的固定点与代数**：研究幂端函子的固定点，定义幂端函子的代数结构。
2. **递归范畴**：探讨递归范畴中的固定点构造，适用于递归数据结构的定义。
3. **三元组（Triples）**：抽象化伴随函子的概念，作为一种重要的技术工具。
4. **斯科特域（Scott Domains）**：介绍一种用于建模计算的结构，通过固定点技术实现递归。

这些内容不仅在理论范畴论中具有重要地位，还在编程语言理论、语义学以及递归数据结构的形式化中有广泛应用。

### 学习目标

通过本章的学习，您将能够：

1. **理解幂端函子的固定点与代数结构**：掌握幂端函子的固定点概念，了解幂端函子的代数结构及其性质。
2. **掌握递归范畴的构造与应用**：理解递归范畴中的固定点构造方法，应用于递归数据结构。
3. **理解三元组（Triples）的定义与性质**：掌握三元组的定义，了解其在范畴论中的作用与应用。
4. **了解斯科特域及其在计算模型中的应用**：理解斯科特域的构造方法，掌握其在建模递归与计算中的应用。
5. **通过习题巩固理论知识**：通过一系列习题，深化对幂端函子代数结构和相关概念的理解。

## 14.1 幂端函子的固定点与代数结构

### 固定点的概念

在范畴论中，**固定点**（Fixed Point）是指一个对象在幂端函子作用下不变的情况。具体来说，设 $F: \mathcal{C} \to \mathcal{C}$ 是一个幂端函子，如果存在一个对象 $A \in \mathcal{C}$，使得 $F(A) \cong A$，则称 $A$ 是 $F$ 的一个固定点。

#### 固定点的类型

1. **固定点**：满足 $F(A) \cong A$ 的对象。
2. **最小固定点**（Least Fixed Point）：在所有固定点中“最小”的固定点，通常通过某种构造方法得到。
3. **终极固定点**（Terminal Fixed Point）：满足特定终极性条件的固定点。

### 幂端函子的代数结构

为了系统地研究幂端函子的固定点，定义了**幂端函子的代数**（Algebra for an Endofunctor）。具体定义如下：

#### 定义14.1.1：幂端函子的代数

设 $F: \mathcal{C} \to \mathcal{C}$ 是一个幂端函子，一个 **$F$-代数**（$F$-algebra）是一个对象 $A \in \mathcal{C}$ 以及一个态射 $\alpha: F(A) \to A$。代数同态（Morphism of $F$-Algebras）是保持结构的态射，即对于两个 $F$-代数 $(A, \alpha)$ 和 $(B, \beta)$，一个态射 $f: A \to B$ 是代数同态，如果满足下图交换：

$$
\begin{array}{ccc}
F(A) & \xrightarrow{F(f)} & F(B) \\
\downarrow{\alpha} & & \downarrow{\beta} \\
A & \xrightarrow{f} & B \\
\end{array}
$$

#### 代数的意义

- **固定点代数**：若 $\alpha: F(A) \to A$ 是同构态射，则 $A$ 是 $F$ 的一个固定点。
- **列表代数**：在定义列表结构时，列表代数 $\alpha: F(A) \to A$ 描述了如何将列表的构造与应用结合起来。

### 最小固定点

最小固定点通常通过**初始代数**（Initial Algebra）构造得到。初始代数具有唯一到任意其他代数的代数同态，确保了它在固定点中的“最小性”。

#### 定义14.1.2：初始代数

设 $F: \mathcal{C} \to \mathcal{C}$ 是一个幂端函子，初始 $F$-代数是一个 $F$-代数 $(A, \alpha)$，满足对于任何 $F$-代数 $(B, \beta)$，存在唯一的代数同态 $f: (A, \alpha) \to (B, \beta)$。

#### 初始代数的性质

- **存在性**：在某些范畴中，通过构造方法（如递归定义、初始对象的构造等）可以保证初始代数的存在。
- **唯一性**：初始代数在范畴 $\mathcal{C}$ 中是唯一（至同构）的。

### 代数的应用

1. **列表对象**：利用初始代数构造列表对象，定义列表的基本操作（如空列表和添加元素）。
2. **状态转换机**：通过 $F$-代数定义状态转换机的状态与转换规则。
3. **递归数据结构**：利用初始代数构造递归数据结构（如树、图等）。

## 14.2 列表对象与状态转换机

### 列表对象

列表对象是计算机科学中一种基本的数据结构，用于表示元素的有序集合。在范畴论中，列表对象可以通过幂端函子的代数结构系统地定义和研究。

#### 列表函子

设 $F: \text{Set} \to \text{Set}$ 是一个幂端函子，定义为：

$$
F(X) = 1 + X \times X
$$

其中 $1$ 表示包含一个元素的集合（空列表），$X \times X$ 表示将元素与已有列表组合（添加元素）。

#### 列表代数

一个 **列表代数** 是一个对象 $L$ 和一个态射 $\alpha: F(L) \to L$，即：

$$
\alpha: 1 + L \times L \to L
$$

具体来说：

- **空列表**：对应于 $\alpha(\text{inl}(*)) = \text{empty}$。
- **添加元素**：对应于 $\alpha(\text{inr}(l, x)) = l \text{ cons } x$，其中 $l$ 是已有列表，$x$ 是新元素。

#### 初始列表代数

初始列表代数 $(\text{List}(X), \text{cons})$ 满足：

$$
\text{List}(X) = 1 + X \times \text{List}(X)
$$

即每个列表要么是空列表，要么是一个元素和一个已有列表的组合。

### 状态转换机

状态转换机（State Transition Machine）是一种用于建模系统状态及其转换的工具。在范畴论中，可以通过幂端函子的代数结构来定义和研究状态转换机。

#### 状态转换机的定义

设 $F: \text{Set} \to \text{Set}$ 是一个幂端函子，定义为：

$$
F(X) = Q \times X^S
$$

其中：

- $Q$ 是状态集。
- $S$ 是输入集。
- $X^S$ 表示从 $S$ 到 $X$ 的函数集，表示状态转换函数。

#### 状态转换代数

一个 **状态转换代数** 是一个对象 $M$ 和一个态射 $\alpha: F(M) \to M$，即：

$$
\alpha: Q \times M^S \to M
$$

具体来说：

- **状态**：由 $\alpha(q, f)$ 确定，其中 $q$ 是当前状态，$f$ 是输入集 $S$ 上的状态转换函数。
- **状态转换**：通过 $\alpha$ 将当前状态和输入转换为新的状态。

#### 应用

- **系统建模**：通过定义状态集和输入集，构造状态转换机的代数结构，用于建模复杂系统的行为。
- **程序语义学**：在编程语言的语义定义中，利用状态转换机描述程序执行过程中的状态变化。

## 14.3 三元组（Triples）

### 三元组的定义

**三元组**（Triple）是一种抽象化伴随函子的结构，旨在系统地描述各种代数结构。三元组包含一个幂端函子及其相关的单位和乘子自然变换。

#### 定义14.3.1：三元组

一个 **三元组** $(T, \eta, \mu)$ 包含：

1. **幂端函子** $T: \mathcal{C} \to \mathcal{C}$。
2. **单位自然变换** $\eta: \text{Id}_{\mathcal{C}} \to T$。
3. **乘子自然变换** $\mu: T \circ T \to T$。

#### 三元组的公理

三元组 $(T, \eta, \mu)$ 必须满足以下公理：

1. **结合律**：
   $$
   \mu \circ T(\mu) = \mu \circ \mu_T
   $$
   即 $T(T(T(X))) \xrightarrow{T(\mu_X)} T(T(X)) \xrightarrow{\mu_X} T(X)$ 与 $T(T(T(X))) \xrightarrow{\mu_{T(X)}} T(T(X)) \xrightarrow{\mu_X} T(X)$ 相同。

2. **单位律**：
   $$
   \mu \circ T(\eta) = \mu \circ \eta_T = \text{id}_T
   $$
   即 $T(X) \xrightarrow{T(\eta_X)} T(T(X)) \xrightarrow{\mu_X} T(X)$ 和 $T(X) \xrightarrow{\eta_{T(X)}} T(T(X)) \xrightarrow{\mu_X} T(X)$ 都等于恒等态射。

### 三元组与伴随函子的关系

三元组抽象化了伴随函子的构造过程，提供了一种统一的框架来定义和研究代数结构。具体来说：

- **伴随函子关系**：若 $F \dashv U$，则可以构造一个三元组 $(T, \eta, \mu)$，其中 $T = U \circ F$，$\eta$ 是单位自然变换，$\mu$ 是由伴随关系导出的乘子自然变换。

### 三元组的应用

1. **代数结构的抽象**：通过三元组，可以统一描述各种代数结构，如单子（Monads）、合子（Comonads）等。
2. **计算模型**：在编程语言的语义中，三元组用于定义计算的抽象模型，如状态变换、异常处理等。
3. **范畴论的工具**：三元组作为一种技术工具，广泛应用于范畴论的各种构造与证明中。

## 14.4 三元组的代数结构

### 三元组代数的定义

**三元组代数**（Algebra for a Triple）是对幂端函子代数的进一步抽象，结合了三元组的单位和乘子自然变换，满足特定的兼容性条件。

#### 定义14.4.1：三元组代数

设 $(T, \eta, \mu)$ 是一个三元组，一个 **$(T, \eta, \mu)$-代数** 是一个 $T$-代数 $(A, \alpha)$ ，满足以下条件：

$$
\alpha \circ T(\alpha) = \alpha \circ \mu_A
$$

即下图交换：

$$
\begin{array}{ccc}
T(T(A)) & \xrightarrow{T(\alpha)} & T(A) \\
\downarrow{\mu_A} & & \downarrow{\alpha} \\
T(A) & \xrightarrow{\alpha} & A \\
\end{array}
$$

#### 代数同态的定义

两个 $(T, \eta, \mu)$-代数 $(A, \alpha)$ 和 $(B, \beta)$ 之间的代数同态是一个 $T$-代数同态 $f: A \to B$，满足：

$$
f \circ \alpha = \beta \circ T(f)
$$

### 三元组代数的性质

1. **初始代数**：存在初始 $(T, \eta, \mu)$-代数时，它通常对应于最小的固定点或基本代数结构。
2. **自由代数**：对于任何 $\mathcal{C}$-对象，可以通过自由 $(T, \eta, \mu)$-代数构造方法生成相应的代数。
3. **代数的组合**：三元组代数支持代数结构的组合与嵌套，适用于复杂系统的构造与分析。

### 应用

1. **列表对象**：通过 $(T, \eta, \mu)$-代数定义列表对象的构造与操作。
2. **状态转换机**：利用三元组代数结构定义复杂的状态转换规则与系统行为。
3. **编程语言的语义**：在编程语言的语义模型中，三元组代数用于定义递归、异常处理、状态管理等特性。

## 14.5 斯科特域（Scott Domains）

### 斯科特域的定义与背景

**斯科特域**（Scott Domain）是计算机科学中用于建模递归与计算的数学结构，特别是在描述不动点和递归函数时。斯科特域通过特定的有序集结构，提供了稳健的框架来处理无限过程和递归定义。

#### 斯科特域的基本构造

斯科特域通常构造为一个完备偏序集（Complete Partial Order, CPO），即一个有序集，其中每个有界上链都有上确界。

### Smyth和Plotkin的构造方法

Smyth和Plotkin提出了一种通过固定点技术构造斯科特域的方法，适用于笛卡尔闭范畴（Cartesian Closed Categories）。具体步骤如下：

1. **定义幂端函子**：选择一个适当的幂端函子 $F: \mathcal{C} \to \mathcal{C}$。
2. **构造固定点**：利用初始代数或终结代数的构造方法，得到 $F$ 的固定点对象，作为斯科特域的基本元素。
3. **定义不动点操作**：通过固定点构造定义递归操作，确保斯科特域能够处理无限递归定义。
4. **构造序结构**：定义合适的序关系，使得斯科特域满足完备偏序集的性质，确保所有有界上链都有上确界。

### 斯科特域在计算模型中的应用

1. **递归定义**：通过固定点技术，斯科特域能够自然地处理递归函数和数据结构的定义。
2. **计算语义**：在编程语言的语义模型中，斯科特域用于定义程序的行为，确保递归调用的稳定性与一致性。
3. **模型构造**：通过斯科特域构造模型，能够描述和分析复杂的计算过程和递归结构。

### 示例：列表对象的斯科特域模型

1. **幂端函子**：选择列表函子 $F(X) = 1 + X \times X$。
2. **初始代数**：列表对象 $\text{List}(X)$ 是 $F$-代数的初始代数，满足 $\text{List}(X) = 1 + X \times \text{List}(X)$。
3. **固定点构造**：通过初始代数的构造，得到列表对象作为固定点，支持无限递归列表的定义。
4. **序结构**：定义包含关系作为序关系，确保列表对象满足斯科特域的性质。

## 14.6 其他内容概述

### 递归范畴（Recursive Categories）

递归范畴是指能够通过幂端函子的固定点构造实现递归数据结构和递归定义的范畴。递归范畴在计算模型和编程语言理论中具有重要应用。

### 三元组的分解（Factorizations of a Triple）

三元组的分解研究如何将一个复杂的三元组结构分解为更简单的组成部分，以便于分析和应用。这在构造复杂代数结构和理解伴随函子关系中具有重要意义。

## 14.7 习题解析

本节提供了一系列习题，旨在巩固和扩展对幂端函子代数结构及相关概念的理解。以下是部分习题的详细解析：

### 练习1

**题目**：设 $\mathcal{A}$ 和 $\mathcal{B}$ 是范畴，且 $T: \mathcal{A} \to \mathcal{B}$，$L, R: \mathcal{B} \to \mathcal{A}$ 是函子，满足 $L \dashv T \dashv R$。证明 $L \circ T \dashv R \circ T$ 作为 $\mathcal{A}$ 和 $\mathcal{B}$ 上的自同构函子。

**解析**：

**目标**：证明在 $L \dashv T \dashv R$ 的假设下，复合函子 $L \circ T \dashv R \circ T$。

**证明步骤**：

1. **伴随函子的定义**：
   - $L \dashv T$：存在单位自然变换 $\eta: \text{Id}_{\mathcal{A}} \to T \circ L$ 和余单位自然变换 $\epsilon: L \circ T \to \text{Id}_{\mathcal{B}}$。
   - $T \dashv R$：存在单位自然变换 $\eta': \text{Id}_{\mathcal{B}} \to R \circ T$ 和余单位自然变换 $\epsilon': T \circ R \to \text{Id}_{\mathcal{A}}$。

2. **构造新的自然变换**：
   - **单位自然变换** $\eta'': \text{Id}_{\mathcal{A}} \to (R \circ T) \circ (L \circ T)$ 定义为 $\eta'' = R \circ \eta \circ L \circ T$。
   - **余单位自然变换** $\epsilon'': (L \circ T) \circ (R \circ T) \to \text{Id}_{\mathcal{B}}$ 定义为 $\epsilon'' = \epsilon \circ T \circ R \circ T$。

3. **验证伴随关系**：
   - 需要验证三角恒等式：
     $$
     \epsilon'' \circ (L \circ T) \circ \eta'' = \text{id}_{L \circ T}
     $$
     $$
     (R \circ T) \circ \epsilon'' \circ \eta'' = \text{id}_{R \circ T}
     $$
   - 通过组合 $L \dashv T$ 和 $T \dashv R$ 的三角恒等式，可以验证上述等式成立。

4. **结论**：
   - 因此，复合函子 $L \circ T \dashv R \circ T$，即 $L \circ T$ 是 $R \circ T$ 的左伴随函子。

### 练习2

**题目**：
a. 设 $X$ 是一个实数集，作为一个有序集（考虑为范畴）。证明包含 $X \subseteq \mathbb{R}$ 的极限（如果存在）是 $X$ 的下确界（infimum），同理，包含的余极限是 $X$ 的上确界（supremum）。

**解析**：

**目标**：证明在有序集范畴中，包含 $X \subseteq \mathbb{R}$ 的极限是 $X$ 的下确界，余极限是上确界。

**证明步骤**：

1. **极限的定义**：
   - 在有序集范畴中，极限对应于集合的下确界（infimum）。
   - 余极限对应于集合的上确界（supremum）。

2. **证明下确界是极限**：
   - 设 $\inf X = m$，则 $m$ 满足：
     - $m \leq x$ 对所有 $x \in X$ 成立。
     - 若 $n \leq x$ 对所有 $x \in X$ 成立，则 $n \leq m$。
   - 这正是极限的普遍映射性质，即对于任意 $y \in \mathbb{R}$，若 $y \leq x$ 对所有 $x \in X$ 成立，则 $y \leq m$。

3. **证明上确界是余极限**：
   - 设 $\sup X = M$，则 $M$ 满足：
     - $x \leq M$ 对所有 $x \in X$ 成立。
     - 若 $x \leq n$ 对所有 $x \in X$ 成立，则 $M \leq n$。
   - 这正是余极限的普遍映射性质。

4. **结论**：
   - 在有序集范畴中，包含 $X \subseteq \mathbb{R}$ 的极限是 $X$ 的下确界，余极限是上确界。

**题目2b**：
参考上一节的练习4，证明天花板函数（ceiling function）保持上确界，但不一定保持下确界；地板函数（floor function）保持下确界，但不一定保持上确界。

**解析**：

**目标**：证明天花板函数 $\lceil \cdot \rceil$ 保持上确界，但不一定保持下确界；地板函数 $\lfloor \cdot \rfloor$ 保持下确界，但不一定保持上确界。

**证明步骤**：

1. **天花板函数保持上确界**：
   - 设 $X \subseteq \mathbb{R}$，且 $\sup X = M$。
   - 计算 $\lceil X \rceil = \{ \lceil x \rceil \mid x \in X \}$。
   - 因为 $x \leq M$ 对所有 $x \in X$ 成立，且 $\lceil x \rceil \leq \lceil M \rceil$，所以 $\sup \lceil X \rceil = \lceil M \rceil$。
   - 因此，天花板函数保持上确界。

2. **天花板函数不保持下确界**：
   - 设 $X = (0, 1) \subseteq \mathbb{R}$，则 $\inf X = 0$。
   - 计算 $\lceil X \rceil = \{ \lceil x \rceil \mid x \in (0, 1) \} = \{1\}$，则 $\inf \lceil X \rceil = 1 \neq 0 = \inf X$。
   - 因此，天花板函数不保持下确界。

3. **地板函数保持下确界**：
   - 设 $X \subseteq \mathbb{R}$，且 $\inf X = m$。
   - 计算 $\lfloor X \rfloor = \{ \lfloor x \rfloor \mid x \in X \}$。
   - 因为 $m \leq x$ 对所有 $x \in X$ 成立，且 $\lfloor m \rfloor \leq \lfloor x \rfloor$，所以 $\inf \lfloor X \rfloor = \lfloor m \rfloor$。
   - 因此，地板函数保持下确界。

4. **地板函数不保持上确界**：
   - 设 $X = (0, 1) \subseteq \mathbb{R}$，则 $\sup X = 1$。
   - 计算 $\lfloor X \rfloor = \{ \lfloor x \rfloor \mid x \in (0, 1) \} = \{0\}$，则 $\sup \lfloor X \rfloor = 0 \neq 1 = \sup X$。
   - 因此，地板函数不保持上确界。

**结论**：
- **天花板函数** $\lceil \cdot \rceil$ 保持上确界，但不一定保持下确界。
- **地板函数** $\lfloor \cdot \rfloor$ 保持下确界，但不一定保持上确界。

### 练习3

**题目**：证明 $\beta_{A,B}$，如定理13.3.2的证明中所定义，是一个自然变换，从 $\text{Hom}_{\mathcal{B}}(F(-), -)$ 到 $\text{Hom}_{\mathcal{A}}(-, U(-))$，作为函子 $\mathcal{A}^{\text{op}} \times \mathcal{B} \to \text{Set}$。

**解析**：

**目标**：证明 $\beta_{A,B}: \text{Hom}_{\mathcal{B}}(F(A), B) \to \text{Hom}_{\mathcal{A}}(A, U(B))$ 是一个自然变换。

**证明步骤**：

1. **定义**：
   - 对于每个 $(A, B) \in \mathcal{A}^{\text{op}} \times \mathcal{B}$，定义 $\beta_{A,B}(g) = U(g) \circ \eta_A$，其中 $\eta_A: A \to U(F(A))$ 是单位自然变换。

2. **自然性的定义**：
   - 需要验证对于任意态射 $(f: A' \to A, h: B \to B')$，下图交换：
     $$
     \begin{array}{ccc}
     \text{Hom}_{\mathcal{B}}(F(A), B) & \xrightarrow{\beta_{A,B}} & \text{Hom}_{\mathcal{A}}(A, U(B)) \\
     \downarrow{\text{Hom}(F(f), h)} & & \downarrow{\text{Hom}(f, U(h))} \\
     \text{Hom}_{\mathcal{B}}(F(A'), B') & \xrightarrow{\beta_{A',B'}} & \text{Hom}_{\mathcal{A}}(A', U(B'))
     \end{array}
     $$

3. **计算上方路径**：
   $$
   \text{Hom}(f, U(h)) \circ \beta_{A,B}(g) = \text{Hom}(f, U(h))(U(g) \circ \eta_A) = U(h) \circ U(g) \circ \eta_A
   $$

4. **计算下方路径**：
   $$
   \beta_{A',B'} \circ \text{Hom}(F(f), h)(g) = \beta_{A',B'}(h \circ g \circ F(f)) = U(h \circ g \circ F(f)) \circ \eta_{A'}
   $$

5. **利用函子 $U$ 的性质**：
   $$
   U(h \circ g \circ F(f)) = U(h) \circ U(g) \circ U(F(f))
   $$

6. **利用单位自然变换的自然性**：
   - 自然变换 $\eta$ 满足 $U(F(f)) \circ \eta_{A'} = \eta_A \circ f$。
   - 因此：
     $$
     U(h) \circ U(g) \circ U(F(f)) \circ \eta_{A'} = U(h) \circ U(g) \circ (\eta_A \circ f) = U(h) \circ U(g) \circ \eta_A \circ f
     $$
   - 而 $f: A' \to A$ 是给定的态射，且在自然同构中，$\eta_A \circ f$ 对应于 $U(F(f)) \circ \eta_{A'}$。

7. **结论**：
   - 因此，$\text{Hom}(f, U(h)) \circ \beta_{A,B}(g) = \beta_{A',B'} \circ \text{Hom}(F(f), h)(g)$，满足自然性条件。

**结论**：
- $\beta_{A,B}$ 是一个自然变换，从 $\text{Hom}_{\mathcal{B}}(F(-), -)$ 到 $\text{Hom}_{\mathcal{A}}(-, U(-))$，作为函子 $\mathcal{A}^{\text{op}} \times \mathcal{B} \to \text{Set}$。

### 练习4

**题目**：对于任何函子 $G: \mathcal{C} \to \mathcal{D}$，令 $G^{\text{op}}: \mathcal{C}^{\text{op}} \to \mathcal{D}^{\text{op}}$ 是逆变函子（即在对象和态射上与 $G$ 相同，但方向相反）。证明，对于任意函子 $F$ 和 $U$，如果 $F$ 是 $U$ 的左伴随函子，则 $F^{\text{op}}$ 是 $U^{\text{op}}$ 的右伴随函子。

**解析**：

**目标**：证明若 $F: \mathcal{A} \to \mathcal{B}$ 是 $U: \mathcal{B} \to \mathcal{A}$ 的左伴随函子（即 $F \dashv U$），则其逆变函子 $F^{\text{op}}: \mathcal{A}^{\text{op}} \to \mathcal{B}^{\text{op}}$ 是 $U^{\text{op}}: \mathcal{B}^{\text{op}} \to \mathcal{A}^{\text{op}}$ 的右伴随函子（即 $F^{\text{op}} \dashv U^{\text{op}}$）。

**证明步骤**：

1. **伴随函子的定义**：
   - $F \dashv U$ 表示存在自然同构：
     $$
     \text{Hom}_{\mathcal{B}}(F(A), B) \cong \text{Hom}_{\mathcal{A}}(A, U(B))
     $$
     对于所有 $A \in \mathcal{A}$ 和 $B \in \mathcal{B}$。

2. **取对偶**：
   - 对于逆变函子 $F^{\text{op}}: \mathcal{A}^{\text{op}} \to \mathcal{B}^{\text{op}}$ 和 $U^{\text{op}}: \mathcal{B}^{\text{op}} \to \mathcal{A}^{\text{op}}$，我们需要证明：
     $$
     \text{Hom}_{\mathcal{B}^{\text{op}}}(F^{\text{op}}(A), B) \cong \text{Hom}_{\mathcal{A}^{\text{op}}}(A, U^{\text{op}}(B))
     $$
     对于所有 $A \in \mathcal{A}^{\text{op}}$ 和 $B \in \mathcal{B}^{\text{op}}$。

3. **转换同构**：
   - $\text{Hom}_{\mathcal{B}^{\text{op}}}(F^{\text{op}}(A), B) = \text{Hom}_{\mathcal{B}}(B, F(A))$
   - $\text{Hom}_{\mathcal{A}^{\text{op}}}(A, U^{\text{op}}(B)) = \text{Hom}_{\mathcal{A}}(U(B), A)$
   
   - 原同构 $\text{Hom}_{\mathcal{B}}(F(A), B) \cong \text{Hom}_{\mathcal{A}}(A, U(B))$ 取对偶后变为：
     $$
     \text{Hom}_{\mathcal{B}}(B, F(A)) \cong \text{Hom}_{\mathcal{A}}(U(B), A)
     $$
     这正是 $F^{\text{op}} \dashv U^{\text{op}}$ 的定义。

4. **自然同构的传递**：
   - 原来的自然同构 $\beta: \text{Hom}_{\mathcal{B}}(F(-), -) \cong \text{Hom}_{\mathcal{A}}(-, U(-))$ 通过取对偶得到：
     $$
     \beta^{\text{op}}: \text{Hom}_{\mathcal{B}^{\text{op}}}(-, F^{\text{op}}(-)) \cong \text{Hom}_{\mathcal{A}^{\text{op}}}(U^{\text{op}}(-), -)
     $$
     这满足 $F^{\text{op}} \dashv U^{\text{op}}$ 的定义。

**结论**：

若 $F \dashv U$，则其逆变函子 $F^{\text{op}} \dashv U^{\text{op}}$。这展示了伴随函子关系在对偶范畴中的对称性，进一步强化了范畴论中对偶性的概念。

### 练习5

**题目**：这是一系列习题，旨在展示若范畴 $\mathcal{C}$ 具有有限极限，则函子 $P_A: \mathcal{C} \to \mathcal{C}/A$ 的存在右伴随函子 $R_A$ 等价于 $\mathcal{C}$ 中存在指数对象 $[A \to -]$，即满足 $\text{Hom}(A \times B, C) \cong \text{Hom}(B, [A \to C])$ 对所有 $B, C \in \mathcal{C}$。

**解析**：

**目标**：通过一系列步骤证明，若范畴 $\mathcal{C}$ 具有有限极限，则函子 $P_A: \mathcal{C} \to \mathcal{C}/A$ 存在右伴随函子 $R_A$ 等价于 $\mathcal{C}$ 中存在指数对象 $[A \to -]$。

**证明步骤**：

**练习5a**：

**题目**：设 $A$ 是 $\mathcal{C}$ 的一个对象，$PA: \mathcal{C} \to \mathcal{C}/A$ 是定义在13.2.6节的函子，将对象 $C$ 映射到投影态射 $p_2: C \times A \to A$，且 $LA: \mathcal{C}/A \to \mathcal{C}$ 是将态射 $f: B \to A$ 映射到对象 $B$。证明 $LA$ 是 $PA$ 的左伴随函子。

**解析**：

1. **定义**：
   - $PA: \mathcal{C} \to \mathcal{C}/A$：$PA(C) = (C \times A \xrightarrow{p_2} A)$。
   - $LA: \mathcal{C}/A \to \mathcal{C}$：$LA(f: B \to A) = B$。

2. **构造自然同构**：
   - 需要构造自然同构：
     $$
     \text{Hom}_{\mathcal{C}/A}(PA(C), f) \cong \text{Hom}_{\mathcal{C}}(C, LA(f))
     $$
   - $\text{Hom}_{\mathcal{C}/A}(PA(C), f)$ 表示从 $(C \times A \xrightarrow{p_2} A)$ 到 $(B \xrightarrow{f} A)$ 的态射，即满足 $f \circ g = p_2 \circ PA(g)$ 的态射 $g: C \times A \to B$。

3. **验证普遍映射性质**：
   - 对于每个 $g: C \times A \to B$ 满足 $f \circ g = p_2$，可以唯一地对应一个态射 $h: C \to B$ 使得 $g = h \times \text{id}_A$。
   - 因此，存在自然同构 $\text{Hom}_{\mathcal{C}/A}(PA(C), f) \cong \text{Hom}_{\mathcal{C}}(C, B)$。

4. **结论**：
   - 因此，$LA \dashv PA$，即 $LA$ 是 $PA$ 的左伴随函子。

**练习5b**：

**题目**：证明若 $PA: \mathcal{C} \to \mathcal{C}/A$ 存在右伴随函子 $RA$，则函子 $A \times - : \mathcal{C} \to \mathcal{C}$ 也存在右伴随函子。

**解析**：

1. **假设**：
   - $PA \dashv RA$。

2. **目标**：
   - 构造或证明 $A \times - \dashv RA \circ PA$。

3. **利用伴随函子的复合**：
   - 因为 $PA \dashv RA$，而 $A \times -$ 是从 $\mathcal{C}$ 到 $\mathcal{C}$ 的函子。
   - 需要证明 $A \times -$ 的右伴随函子可以通过 $RA$ 和 $PA$ 的复合得到。

4. **构造右伴随函子**：
   - 设 $RA \circ PA$ 为一个新的函子。
   - 利用逐点伴随性定理，验证 $A \times - \dashv RA \circ PA$。

5. **验证普遍映射性质**：
   - 需要验证：
     $$
     \text{Hom}_{\mathcal{C}}(A \times C, D) \cong \text{Hom}_{\mathcal{C}}(C, RA(PA(D)))
     $$
   - 通过自然同构和伴随函子的性质，可以建立上述同构关系。

6. **结论**：
   - 因此，若 $PA \dashv RA$，则 $A \times - \dashv RA \circ PA$，即 $A \times -$ 存在右伴随函子。

**练习5c**：

**题目**：假设 $\mathcal{C}$ 是笛卡尔闭范畴，证明对于每个 $PA(C) \in \mathcal{C}/A$，对象 $\Phi(PA(C)) = [A \to C]$ 满足基本的伴随同构性质：
$$
\text{Hom}(B, \Phi(PA(C))) \cong \text{Hom}_{\mathcal{C}/A}(PA(B), PA(C))
$$

**解析**：

1. **定义**：
   - $\Phi(PA(C)) = [A \to C]$，即 $A$ 到 $C$ 的指数对象。
   - 由于 $\mathcal{C}$ 是笛卡尔闭范畴，指数对象 $[A \to C]$ 存在且满足：
     $$
     \text{Hom}(A \times B, C) \cong \text{Hom}(B, [A \to C])
     $$

2. **验证基本伴随同构**：
   - $\text{Hom}(B, [A \to C]) \cong \text{Hom}(A \times B, C)$
   - 由于 $PA(B) = (B \times A \xrightarrow{p_2} A)$，则 $\text{Hom}_{\mathcal{C}/A}(PA(B), PA(C))$ 表示从 $B \times A \xrightarrow{p_2} A$ 到 $C \times A \xrightarrow{p_2} A$ 的态射，即满足 $p_2 \circ f = p_2$ 的态射 $f: B \times A \to C \times A$。

3. **建立同构**：
   - 任意 $f: B \times A \to C \times A$ 满足 $p_2 \circ f = p_2$，可以分解为 $f = (g \times \text{id}_A)$，其中 $g: B \to C$ 满足 $p_2 \circ (g \times \text{id}_A) = p_2$。
   - 因此，$g$ 对应于 $\text{Hom}(B, [A \to C])$ 中的一个态射。
   - 由指数对象的性质，存在自然同构：
     $$
     \text{Hom}(B, [A \to C]) \cong \text{Hom}(A \times B, C)
     $$
   - 这与 $\text{Hom}_{\mathcal{C}/A}(PA(B), PA(C)) \cong \text{Hom}(B, [A \to C])$ 一致。

4. **结论**：
   - 因此，$\text{Hom}(B, [A \to C]) \cong \text{Hom}_{\mathcal{C}/A}(PA(B), PA(C))$，满足基本伴随同构性质。

### 练习5d

**题目**：证明如果 $f: C \to A$ 是 $\mathcal{C}/A$ 的任意对象，则存在一个等化图：
$$
\begin{array}{ccc}
C & \xrightarrow{d_0} & PA(C) \\
\downarrow{d_1} & & \downarrow{d_1} \\
PALAPA(C) & \xrightarrow{PALAPA(f)} & PALAPA(C) \\
\end{array}
$$
（提示：使用元素。）

**解析**：

**目标**：证明对于任意 $f: C \to A$ 属于 $\mathcal{C}/A$，存在一个等化图，如上所示。

**证明步骤**：

1. **定义等化图**：
   - 等化图是指满足某些态射组合等于另一态射的图。在本题中，需构造 $d_0$ 和 $d_1$ 使得 $d_1 \circ d_0 = PALAPA(f) \circ d_1$。

2. **利用元素构造**：
   - 设 $\mathcal{C}$ 是一个具有有限极限的范畴，可以利用元素（即态射）来构造等化图。
   - 具体来说，利用等化器的定义，构造满足 $d_1 \circ d_0 = PALAPA(f) \circ d_1$ 的态射 $d_0$ 和 $d_1$。

3. **验证等化条件**：
   - 通过验证态射组合 $d_1 \circ d_0$ 和 $PALAPA(f) \circ d_1$ 相等，确保构造的图满足等化条件。

4. **结论**：
   - 因此，存在满足等化条件的等化图，完成证明。

**备注**：
- 具体构造细节依赖于范畴 $\mathcal{C}$ 的结构和等化器的存在性。通过元素构造方法，可以直观地理解等化图的构造过程。

## 本章总结

**幂端函子的代数**作为范畴论中的高级概念，通过固定点构造、代数结构的定义以及三元组的抽象化，提供了理解和构造复杂数学结构的强大工具。本章涵盖了以下主要内容：

1. **幂端函子的固定点与代数结构**：
   - 理解固定点的概念，定义幂端函子的代数结构，掌握初始代数和固定点构造方法。

2. **列表对象与状态转换机**：
   - 利用幂端函子的代数结构定义和研究列表对象及状态转换机，理解其在数据结构和系统建模中的应用。

3. **三元组（Triples）**：
   - 抽象化伴随函子的结构，定义三元组，理解其在描述代数结构和计算模型中的作用。

4. **斯科特域（Scott Domains）**：
   - 通过固定点技术构造斯科特域，了解其在建模递归和计算过程中的应用，掌握Smyth和Plotkin的构造方法。

5. **习题解析**：
   - 通过一系列习题，巩固对幂端函子代数结构、高级定理及其应用的理解，提升理论应用能力。

### 主要要点

1. **幂端函子的固定点与代数**：
   - 定义幂端函子的固定点，理解 $F$-代数的结构和初始代数的构造方法。
   
2. **列表对象与状态转换机**：
   - 通过 $F$-代数定义列表对象和状态转换机，理解其在数据结构和系统建模中的应用。
   
3. **三元组的定义与性质**：
   - 定义三元组，理解其公理和性质，掌握三元组在描述代数结构中的重要性。
   
4. **斯科特域的构造与应用**：
   - 了解斯科特域的定义，掌握通过固定点技术构造斯科特域的方法，理解其在计算模型中的应用。
   
5. **伴随函子的唯一性与对偶性**：
   - 理解伴随函子的唯一性定理，掌握伴随函子在对偶范畴中的对称性。

### 应用与扩展

- **计算机科学**：在编程语言的语义定义中，利用幂端函子的代数结构和斯科特域模型定义递归函数和数据结构。
- **逻辑与类型理论**：通过三元组和幂端函子的代数结构定义高阶类型构造和函数空间。
- **系统建模**：利用状态转换机的代数结构建模复杂系统的状态变化和行为。
- **递归数据结构的形式化**：通过初始代数和递归范畴的构造方法，系统地定义和分析递归数据结构（如树、图等）。

通过本章的学习，您将能够理解和应用幂端函子的代数结构，掌握三元组和斯科特域的构造方法，进一步深化对范畴论高级概念的理解，为后续复杂理论和应用奠定坚实的基础。

### ---------------------------

### 概览

在范畴论中，**函子（Functor）** 是连接不同范畴的重要工具。理解函子的**固定点**及其**最小固定点**对于研究代数结构、递归类型以及计算理论等领域至关重要。本节深入探讨了函子的固定点，通过引入**R-代数（R-Algebra）**的概念，阐明了固定点的结构以及最小固定点的意义。我们将逐步解析定义、示例、定理，并通过习题加深理解。

### 14.1 函子的固定点

#### 14.1.1 函子的固定点是什么？

设 $R : \mathcal{A} \to \mathcal{A}$ 是一个自函子（endofunctor），即作用在范畴 $\mathcal{A}$ 上的函子。固定点的概念仅在自函子的情况下有意义，因为我们需要函子作用后的对象仍在同一范畴内。固定点和最小固定点分别描述了在函子作用下不变的对象及其最小化的结构。

对于集合上的函数，我们熟知**固定点**的定义，即满足 $f(x) = x$ 的 $x$。若集合是一个偏序集，**最小固定点**则是所有固定点中最小的那个。对于函子而言，固定点的相关结构由**R-代数**的概念来刻画，这不仅考虑对象本身，还包括对象上的结构映射。

#### 14.1.2 为何函子的固定点定义并不显而易见？

考虑自然数集 $\mathbb{N}$ 作为函子 $R$ 的一个固定点，其中 $R$ 定义为 $R(S) = 1 + S$，这里的 $1$ 是终对象（singleton set），"+" 表示不相交并（disjoint union）。函子的固定点意味着存在一个集合 $S$，使得 $S \cong 1 + S$。

直观上，$\mathbb{N}$ 满足 $\mathbb{N} \cong 1 + \mathbb{N}$，其中映射将终对象的唯一元素映射到自然数的零，其余元素映射到后继。然而，固定点的定义不仅要求同构，还需要这种同构是唯一的（初始的）。此外，固定点的具体结构可能依赖于函子定义中对和的具体选择，这引出了定义的复杂性。

#### 14.1.3 函子上的代数

引入 **R-代数** 的概念，有助于理解函子的固定点。设 $R : \mathcal{A} \to \mathcal{A}$ 是一个自函子。一个 $R$-代数是一个对 $(A, a)$，其中 $A$ 是 $\mathcal{A}$ 中的一个对象，$a : R(A) \to A$ 是一个态射（箭头）。两个 $R$-代数 $(A, a)$ 和 $(B, b)$ 之间的同态是满足以下交换图的态射 $f : A \to B$:

$$
\begin{array}{ccc}
R(A) & \xrightarrow{R(f)} & R(B) \\
\downarrow{a} & & \downarrow{b} \\
A & \xrightarrow{f} & B \\
\end{array}
$$

所有 $R$-代数及其同态构成一个范畴，记作 $(R : \mathcal{A})$。

#### 14.1.4 定义：函子的固定点

在 $(R : \mathcal{A})$ 范畴中，如果一个 $R$-代数 $(A, a)$ 中的态射 $a$ 是同构的，则称 $A$ 是 $R$ 的一个**固定点**。即：

**定义**：若 $(A, a)$ 满足 $a : R(A) \to A$ 是同构的，则 $A$ 是函子 $R$ 的一个固定点。

这个定义强调了对象 $A$ 上的结构映射 $a$ 不仅存在，而且是可逆的，这确保了 $A$ 在函子 $R$ 的作用下保持不变。

#### 14.1.5 定义：最小固定点

基于范畴被视为广义的偏序集，我们可以定义最小固定点为 $(R : \mathcal{A})$ 范畴中的一个**初始对象**。

**定义**：函子 $R : \mathcal{A} \to \mathcal{A}$ 的**最小固定点**是 $(R : \mathcal{A})$ 范畴中的一个初始对象。

初始对象具有唯一的态射到范畴中的任何其他对象，这使得最小固定点在一定意义上是所有固定点中“最小”的。

#### 14.1.6 示例

考虑 $CPO_P$ 范畴中的函数 $\phi$。通过归纳法可以证明，唯一的 $\phi$-代数是阶乘函数本身，这个代数的结构映射 $a$ 是恒等映射。因此，这个代数显然是 $(R : \mathcal{A})$ 范畴中的初始对象，因为它是唯一的。

#### 14.1.7 定理 [Lambek, 1970]

**定理**：设 $R : \mathcal{A} \to \mathcal{A}$ 是一个自函子。如果 $(A, a)$ 是 $(R : \mathcal{A})$ 范畴中的初始对象，则态射 $a$ 是同构的。

**证明概要**：
1. 假设 $(A, a)$ 是初始对象。
2. 观察 $(R(A), R(a))$ 也是 $(R : \mathcal{A})$ 中的对象。
3. 由初始性的定义，存在唯一的态射 $f : A \to R(A)$ 使得下列图表交换：

$$
\begin{array}{ccc}
R(A) & \xrightarrow{R(f)} & R(R(A)) \\
\downarrow{a} & & \downarrow{R(a)} \\
A & \xrightarrow{f} & R(A) \\
\end{array}
$$

4. 进一步推导得出 $a \circ f = \text{id}_A$ 和 $f \circ a = \text{id}_{R(A)}$，因此 $a$ 是同构。

#### 14.1.8 示例

再次考虑函子 $R : \text{Set} \to \text{Set}$，其中 $R(S) = 1 + S$。自然数集 $\mathbb{N}$ 构成一个 $R$-代数，结构映射为 $(0; s) : 1 + \mathbb{N} \to \mathbb{N}$，其中 $0$ 取终对象的唯一元素映射到零，$s$ 是后继函数。

这个 $R$-代数是初始的，因为对于任何其他 $R$-代数 $f : 1 + S \to S$，唯一的 $R$-代数同态 $h : \mathbb{N} \to S$ 可以通过归纳定义：

- $h(0) = f(*)$ （其中 $*$ 是 $1$ 的唯一元素）
- $h(n + 1) = f(h(n))$

由于 $\mathbb{N}$ 没有任何真子集在给定的同构下仍为固定点，因此它是 $R$ 的“最小”固定点。

#### 14.1.9 凡数函子的固定点

考虑集合范畴上的函子 $R$。称 $R$ 为**有限型的（finitary）**，如果满足以下条件：

1. **有限支持性（FF–1）**：对于每个集合 $S$ 和每个元素 $x \in R(S)$，存在一个有限子集 $S_0 \subseteq S$ 和 $x_0 \in R(S_0)$ 使得 $x = R(i_0)(x_0)$，其中 $i_0 : S_0 \hookrightarrow S$ 是包含映射。
2. **合并性（FF–2）**：如果 $S_1$ 是 $S$ 的另一个有限子集，且 $x_1 \in R(S_1)$ 满足 $x = R(i_1)(x_1)$，则存在一个包含 $S_0$ 和 $S_1$ 的有限子集 $S_2$，以及包含映射 $j_0 : S_0 \hookrightarrow S_2$ 和 $j_1 : S_1 \hookrightarrow S_2$，使得 $R(j_0)(x_0) = R(j_1)(x_1)$。

**有限型的真正含义**是函子 $R$ 的作用完全由其在有限子集上的行为决定。这在计算机科学中尤为重要，因为许多计算过程本质上是有限的。

值得注意的是，在集合范畴中，条件 FF–2 是冗余的，但在更一般的范畴中可能是必需的。

#### 14.1.10 示例

函子 $R : \text{Set} \to \text{Set}$ 定义为 $R(S) = 1 + S$ 是有限型的。具体而言：

- 对于 $x \in 1 + S$，如果 $x \in 1$，则取 $S_0 = \emptyset$，因为 $R(\emptyset) = 1 + \emptyset = 1$。
- 如果 $x \in S$，则取 $S_0 = \{x\}$，因为 $R(i_0)$ 在 $S_0$ 上的限制是包含映射。

因此，$R$ 满足有限型条件。

#### 14.1.11 定理

**定理**：若 $R$ 是集合范畴上的有限型自函子，且 $Z$ 是序列

$$
\emptyset \xrightarrow{\eta} R(\emptyset) \xrightarrow{R(\eta)} R^2(\emptyset) \xrightarrow{R(R(\eta))} \cdots
$$

的余极限（colimit），则存在一个 $R$-代数结构 $z : R(Z) \to Z$，使得 $(Z, z)$ 是 $R$-代数的初始对象。

**解释**：
- 构造序列中的 $Z$ 作为这些映射的余极限，类似于自然数的构造。
- 由于 $R$ 是有限型的，它与余极限 $Z$ 交换，因此 $R(Z) \cong Z$。
- 结构映射 $z$ 就是这个同构，使得 $(Z, z)$ 成为初始 $R$-代数。

#### 14.1.12 示例

再次考虑函子 $R(S) = 1 + S$。构造序列：

$$
\emptyset \xrightarrow{\eta} 1 \xrightarrow{R(\eta)} 1 + 1 \xrightarrow{R(R(\eta))} 1 + (1 + 1) \xrightarrow{\cdots} \mathbb{N}
$$

其中每一步的箭头都是将前一阶段的集合注入到下一个阶段。最终的余极限 $Z$ 是自然数集 $\mathbb{N}$，其结构映射是后继函数 $s$。

#### 14.1.13 示例：非有限型函子

考虑函子 $P : \text{Set} \to \text{Set}$，将集合 $S$ 映射到其子集幂集 $P(S)$。对函数 $f : S \to T$，$P(f)$ 将子集 $S_0 \subseteq S$ 映射到 $f(S_0) \subseteq T$。

此函子 $P$ 不是有限型的，因为对于无限集合 $S$，并非所有子集（尤其是无限子集）都可以由有限子集的映射得到。

#### 14.1.14 为什么需要引入 $R$-代数的范畴？

读者可能会疑惑，既然我们只关心固定点，为什么要引入 $R$-代数的范畴。原因有二：

1. **完备性和初始对象**：若范畴 $\mathcal{A}$ 是完备的且存在初始对象，则 $(R : \mathcal{A})$ 也是完备的。此外，底层函子 $U : (R : \mathcal{A}) \to \mathcal{A}$ 保持所有极限。这虽然不直接证明 $U$ 有伴随函子，但增加了其存在的可能性。
   
2. **代数三元组的构造**：$(R : \mathcal{A})$ 的构造是代数三元组（triple）范畴中 $R$-代数范畴的一部分，这在后续章节中（如14.4.2）会进一步展开。

#### 14.1.15 习题

1. **证明**：在14.1.8节中定义的函数 $h$ 是从 $(1; s) : 1 + \mathbb{N} \to \mathbb{N}$ 到 $f : 1 + S \to S$ 的唯一 $R$-代数同态。
   
2. **证明**：设 $(S, \leq)$ 是一个全序集，且 $f : S \to S$ 是单调函数。定义 $(f : S)$ 范畴的对象为满足 $f(x) \leq x$ 的 $x \in S$。证明这里定义的最小固定点与前述定义一致。
   
3. **证明**：
   - (a) 任意非空集合中的单射函数 $f : S \to T$ 存在 $g : T \to S$ 使得 $g \circ f = \text{id}_S$。
   - (b) 任意单射函数在任意范畴中的函子像是单态（monomorphism）。
   - (c) 在集合范畴中，条件 FF–2 是冗余的。

### 总结

本节深入探讨了自函子固定点的概念，通过引入 $R$-代数的结构，明确了固定点的定义和性质。我们通过具体示例，如自然数集 $\mathbb{N}$ 作为 $R(S) = 1 + S$ 的固定点，直观理解了这些抽象概念。此外，有限型函子的特性保证了固定点的存在性，并通过定理证明了其作为初始 $R$-代数的重要性。最后，习题部分进一步巩固了对固定点概念的理解。这些理论在计算机科学、逻辑和代数学中有广泛应用，尤其在定义递归数据类型和处理代数结构时发挥关键作用。

### ----------------------------

### 概览

在范畴论中，**递归范畴（Recursive Categories）** 提供了一种在范畴内部建模递归过程的方法，这在函数式编程语言和计算理论中具有重要意义。本节将探讨递归范畴的定义、性质以及其在建模递归和有限状态机等方面的应用。通过引入**代数（Algebras）**和**递归对象（Recursive Objects）**的概念，我们能够在范畴论框架下理解和构造递归结构。此外，本节还将讨论**局部递归（Local Recursion）**和**多项式函子（Polynomial Functors）**等高级主题，并通过具体例子说明其在集合范畴中的应用。

### 14.2 递归范畴

#### 14.2.1 函数式编程语言中的递归

在2.2.1节中，我们描述了如何将函数式编程语言表示为一个范畴。一个重要的缺失部分是如何在这个框架内生成潜在的无限程序。传统编程语言通过`while`循环等结构实现这一点，但在笛卡尔闭范畴中，递归提供了一种更灵活的方式来实现无限结构。特别地，笛卡尔闭范畴中的自然数对象（见5.5节）支持递归。然而，函数空间对象的构造可能导致非可计算的结构。

J. R. B. Cockett 提出了在范畴中直接允许有限形式递归的公理，这些公理在附加的温和假设下保持了某种形式的可计算性（见定理14.2.9）。以下讨论基于[Cockett, 1989]和[Cockett, 1990]的工作。

#### 14.2.2 动作与 $\text{act}(A)$ 范畴

设 $\mathcal{C}$ 是一个具有有限积的范畴。对于任意对象 $A$，存在一个函子 $A \times -$ ，该函子将对象 $X$ 映射为 $A \times X$，将态射 $f: X \to Y$ 映射为 $\text{id}_A \times f : A \times X \to A \times Y$。对于这个函子，一个**代数** $x: A \times X \to X$ 被称为 $A$-动作（A-action）。

对于每个对象 $A$，存在一个范畴 $\text{act}(A)$，它是函子 $A \times -$ 的代数的范畴。存在一个底层函子 $U_A : \text{act}(A) \to \mathcal{C}$，它将代数 $x: A \times X \to X$ 映射为对象 $X$，将代数同态 $f: (x: A \times X \to X) \to (y: A \times Y \to Y)$ 映射为态射 $f: X \to Y$。

#### 14.2.3 定义：递归范畴

**定义**：一个范畴 $\mathcal{C}$ 是**递归的（Recursive）**，如果对于每个对象 $A$，底层函子 $U_A : \text{act}(A) \to \mathcal{C}$ 有一个左伴随函子 $F_A : \mathcal{C} \to \text{act}(A)$。

对于对象 $B$，记 $U_A(F_A(B))$ 为 $\text{rec}(A, B)$；对于态射 $f: B \to C$，得到态射 $\text{rec}(A, f) = U_A(F_A(f)) : \text{rec}(A, B) \to \text{rec}(A, C)$。

代数结构 $F_A(B)$ 是一个代数结构态射 $r(A, B) : A \times \text{rec}(A, B) \to \text{rec}(A, B)$。

伴随关系的单元 $\eta$ 在对象 $B$ 处的分量是态射 $r_0(A, B) : B \to \text{rec}(A, B)$。特别地，$\text{rec}(1, 1)$ 是一个自然数对象（见习题2）。

对象 $\text{rec}(A, B)$ 具有以下**普遍映射性质**：如果 $t_0 : B \to X$ 和 $t : A \times X \to X$ 是任意态射，则存在唯一的态射 $f : \text{rec}(A, B) \to X$，使得下图交换：

$$
\begin{array}{ccc}
B & \xrightarrow{t_0} & X \\
\downarrow{r_0(A, B)} & \searrow{f} & \uparrow{t} \\
\text{rec}(A, B) & \xrightarrow{f} & X \\
\end{array}
$$

#### 14.2.4 集合范畴是递归的

在集合范畴 $\text{Set}$ 中，$\text{rec}(A, B)$ （最多同构）是由形如 $(l, b)$ 的有序对组成的集合，其中 $l$ 是元素来自 $A$ 的列表（包括空列表），而 $b \in B$。代数结构态射 $r(A, B)$ 是将 $(a, (l, b))$ 映射为 $(\text{cons}(a, l), b)$ 的函数，其中 $\text{cons}$ 是将元素 $a$ 加入列表 $l$ 前端的操作。

单位态射 $r_0(A, B) : B \to \text{rec}(A, B)$ 将元素 $b$ 映射为 $(\text{hi}, b)$，其中 $\text{hi}$ 是空列表的标记。

特别地，在 $\text{Set}$ 中，$\text{rec}(A, 1)$ 表示 $A^*$，即 $A$ 的所有列表集合。$A^*$ 具有被视为任何范畴中 $A$ 的列表的正确性质；例如，Cockett [1990] 展示了如何在递归范畴中定义列表的头和尾。

#### 14.2.5 局部递归与 $c(A, B)$

对于递归范畴 $\mathcal{C}$ 中的对象 $A$ 和 $B$，投影态射 $p_2 : A \times B \to B$ 是 $A \times -$ 的一个代数。根据初始对象的普遍映射性质，存在唯一的态射 $a : \text{rec}(A, B) \to B$，使得下图交换：

$$
\begin{array}{ccc}
\text{rec}(A, B) & \xrightarrow{a} & B \\
\downarrow{r(A, B)} & & \uparrow{p_2} \\
A \times \text{rec}(A, B) & \xrightarrow{\text{id}_A \times a} & A \times B \\
\end{array}
$$

随后，定义态射：

$$
c(A, B) = \langle \text{rec}(A, \text{hi}), a_i \rangle : \text{rec}(A, B) \to \text{rec}(A, 1) \times B
$$

#### 14.2.6 定义：具有局部递归的范畴

**定义**：若对于所有对象 $A$ 和 $B$，态射 $c(A, B)$ 是同构的，则递归范畴 $\mathcal{C}$ 具有**局部递归（Local Recursion）**。一个**局部递归范畴（Locally Recursive Category）** 是一个具有局部递归且每个切片范畴 $\mathcal{C}/C$ 也是递归的范畴。此外，**一致性范畴（Locos）** 是一个一致的（见9.6.8节）且局部递归的范畴。

局部递归的定义意味着 $\text{rec}(A, B) \cong \text{rec}(A, 1) \times B = A^* \times B$。14.2.4节的讨论表明，这在 $\text{Set}$ 中是成立的。

一致性范畴的公理允许许多其他构造。具体而言，存在一个包含多项式函子（见14.2.7节）的类，这些函子的底层函子从代数范畴中有左伴随函子。

#### 14.2.7 定义：多项式函子

**定义**：在具有有限和的范畴中，多项式自函子（Polynomial Endofunctors）的类是包含常函子和恒等函子的最小类，并且在有限积和有限和下封闭。

即，多项式函子由以下基本构造生成：
- 常函子：将所有对象映射到固定对象。
- 恒等函子：每个对象和态射保持不变。
- 在有限积和有限和下的闭包：对有限数量的积和和进行构造。

#### 14.2.8 命题：Locos 与多项式函子的左伴随

**命题**：设 $\mathcal{C}$ 是一个Locos，且 $P$ 是一个多项式函子，则底层函子 $U : (P : \mathcal{C}) \to \mathcal{C}$ 有一个左伴随函子。

**解释**：
- 这个命题表明，在Locos中，多项式函子对应的代数范畴 $(P : \mathcal{C})$ 的底层函子 $U$ 具有左伴随函子 $F$。
- 这允许我们构造由对象生成的自由对象。例如，设 $D$ 是一个对象，函子 $T$ 定义为 $T(X) = D + X \times D \times X$，则终对象 $1$ 上的自由对象具有结构态射 $[D + F(1) \times D \times F(1)] \to F$。
- 这个自由对象可以解释为根节点带有数据的二叉树，结构映射则是从数据或数据与两个给定树构造这样的树。
- 因此，Locos 允许一种初始代数语义（Initial Algebra Semantics）。

此外，Locos的另一个性质是，如果起始对象是可计算的，则通过Locos的构造得到的对象也是可计算的。这需要一个定义：在任何具有有限积的范畴中，对象 $D$ 是**可判定的（Decidable）**，如果对角态射 $\Delta : D \to D \times D$ 存在补集（见9.6.2节）。这意味着对于 $D$ 中的元素，等价关系满足排中律：两个元素要么相等，要么不等。

在表示可计算对象的范畴中，可以假设每个对象都可以有效生成，即每个对象都是递归可枚举的。若对象是可判定的，则对角的补集存在，从而可以生成所有不相等元素对，并且可以决定两个元素是否相同。

#### 14.2.9 定理：Locos 中的可判定对象

**定理**：设 $S$ 是Locos中可判定对象的集合，且Locos中没有包含 $S$ 的子Locos，则Locos中的每个对象都是可判定的。

**解释**：
- 该定理表明，在满足特定条件的Locos中，所有对象都具有可判定性。
- 证明参见[Cockett, 1989]。
- Locos可以描述为FL草图（FL sketch）的模型，因此存在一个初始Locos。
- 上述定理意味着在初始Locos中，每个对象都是可判定的。

#### 14.2.10 范畴动力学

**定义**：在范畴 $\mathcal{C}$ 中，一个**输入过程（Input Process）** 是一个自函子 $R$ ，其底层函子 $(R : \mathcal{C}) \to \mathcal{C}$ 有一个左伴随函子。

**定义**：$(R : \mathcal{C})$ 范畴的对象称为**R-动力（R-Dynamic）**，而范畴 $(R : \mathcal{C})$ 本身称为**R-动力范畴**。范畴中的态射称为**动力态射（Dynamorphism）**。

**定义**：在 $\mathcal{C}$ 中，一个**机器（Machine）** 是一个七元组 $M = (R, \mathcal{C}, c, I, \tau, Y, \beta)$，其中：
- $R$ 是一个输入过程。
- $(\mathcal{C}, c)$ 是一个 $R$-动力。
- $I$ 是 $\mathcal{C}$ 的一个对象，称为**初始状态对象（Initial State Object）**。
- $\tau : I \to \mathcal{C}$ 是初始状态态射。
- $Y$ 是**输出对象（Output Object）**。
- $\beta : \mathcal{C} \to Y$ 是输出态射。

#### 14.2.11 示例：集合范畴中的有限状态机

设 $A$ 是任何递归范畴 $\mathcal{C}$ 中的一个对象。根据定义，函子 $R$ 定义为 $R(C) = A \times C$ 且 $R(f) = \text{id}_A \times f$，是一个输入过程。因此，可以在任意递归范畴中定义有限状态机。

具体来说，在 $\text{Set}$ 中，设 $A$ 是一个有限集合，视为字母表。一个 $R$-代数是一个函数 $\delta : A \times C \to C$ 。通过定义：

$$
\delta^*(\text{hi}, q) = q
$$
$$
\delta^*(a w, q) = \delta(a, \delta^*(w, q))
$$

可以得到自由幺半群 $A^*$ 在 $C$ 上的作用。

设 $I$ 为一个单元素集合，则得到的机器定义与自动机理论中的状态转换机一致。这里的可达性意味着任意状态都可以通过一系列操作从初始状态到达；如果不能，则这些状态形成包含初始状态像的真子代数。

此外，$R$-动力的概念比有限状态机更一般。例如，可以通过选择更大的 $I$ 来拥有多个“起始”状态，或使用不同的函子 $R$，例如 $R(C) = C \times C$，来定义更复杂的动力系统。关于范畴动力学的详细讨论可参考[Arbib 和 Manes, 1975]以及[Arbib 和 Manes, 1980]。

#### 14.2.12 习题

1. **证明14.2.4中的主张**：
   - 证明在 $\text{Set}$ 中，$\text{rec}(A, B)$ 是由形如 $(l, b)$ 的有序对组成的集合，其中 $l$ 是 $A$ 的列表，且 $b \in B$。
   - 证明代数结构态射 $r(A, B)$ 将 $(a, (l, b))$ 映射为 $(\text{cons}(a, l), b)$。
   - 证明单位态射 $r_0(A, B)$ 将 $b$ 映射为 $(\text{hi}, b)$，其中 $\text{hi}$ 是空列表。

2. **证明在递归范畴中，$\text{rec}(1, 1)$ 是一个自然数对象**：
   - 使用伴随关系的单元 $r_0(A, B)$ 来定义零元。
   - 通过结构态射 $r(A, B)$ 定义后继函数。
   - 验证自然数对象的普遍性质，即任何包含零元和后继函数的代数都有唯一的态射从自然数对象到该代数的态射。

### 总结

本节介绍了**递归范畴**的概念及其在范畴论中的重要性。通过定义递归范畴，我们能够在范畴内部建模递归过程，这对于理解函数式编程语言中的递归机制和构造无限结构至关重要。通过引入**代数**和**动作**的概念，我们定义了递归对象 $\text{rec}(A, B)$，并讨论了其普遍映射性质。

进一步地，我们探讨了**局部递归**和**多项式函子**，展示了在特定范畴（如集合范畴）中这些概念的具体应用。通过示例，我们理解了如何在递归范畴中定义有限状态机及其可达性。这些理论不仅在数学上具有深刻的意义，也在计算机科学中，尤其是在自动机理论和程序设计语言的语义学中，具有广泛的应用。

习题部分通过具体的证明任务，帮助巩固对递归范畴及其性质的理解。掌握这些概念为进一步研究范畴论在计算理论和代数结构中的应用奠定了坚实的基础。

### ---------------------------

### 概览

在范畴论中，**三元组（Triples）**，也常称为**单子（Monad）**，是一个基于自函子的结构，已成为研究拓扑（Toposes）及相关主题的重要技术工具。三元组是伴随对（Adjoint Pairs）概念的抽象，也是**通用代数（Universal Algebra）**概念的一种推广。通过引入三元组，我们能够更深入地理解和构造复杂的代数结构和递归过程，这对于理论计算机科学和数学中的许多领域具有重要意义。本节将详细介绍三元组的定义、来源、表示及其相关概念，并通过具体例子帮助理解。

### 14.3 三元组

#### 14.3.1 定义：三元组

**定义**：在一个范畴 $\mathcal{A}$ 上，**三元组** $T = (T, \eta, \mu)$ 由以下组成部分构成：
- **函子** $T : \mathcal{A} \to \mathcal{A}$
- **自然变换** $\eta : \text{id} \to T$（称为单元，Unit）
- **自然变换** $\mu : T^2 \to T$（称为乘法，Multiplication）

这些组成部分需要满足以下**结合律**和**单位律**，即以下两个图形必须交换：

1. **单位律（Unit Laws）**：
   
   $$
   \begin{array}{ccc}
   T & \xrightarrow{T \eta} & T^2 \\
   \downarrow{\mu} & & \downarrow{\mu} \\
   T & = & T \\
   \end{array}
   \quad
   \begin{array}{ccc}
   T & \xrightarrow{\eta T} & T^2 \\
   \downarrow{\mu} & & \downarrow{\mu} \\
   T & = & T \\
   \end{array}
   $$
   
2. **结合律（Associativity Law）**：
   
   $$
   \begin{array}{ccc}
   T^3 & \xrightarrow{T \mu} & T^2 \\
   \downarrow{\mu T} & & \downarrow{\mu} \\
   T^2 & \xrightarrow{\mu} & T \\
   \end{array}
   \quad
   \text{即} \quad \mu \circ T\mu = \mu \circ \mu T
   $$

**通俗解释**：
- **函子** $T$ 就像是一个操作，可以将范畴中的对象和态射转换为另一个对象和态射。
- **自然变换** $\eta$ 和 $\mu$ 分别提供了从恒等函子到 $T$ 的映射和从 $T^2$ 到 $T$ 的映射。
- **单位律** 确保了 $\eta$ 的作用是一个“单位”操作，不改变 $T$ 的结构。
- **结合律** 确保了连续应用 $T$ 时，$\mu$ 的作用是一致的，不会因应用顺序不同而产生差异。

三元组的命名来自于其结构类似于代数中的**单子（Monoid）**，其中 $\eta$ 类似于单位元，$\mu$ 类似于结合操作。

**注意**：虽然“三元组”常被称为“单子”，但它们与罗宾逊的无穷小量理论中的单子（Monads）无关。

#### 14.3.2 来自伴随对的三元组

**来源**：一个伴随对（Adjoint Pair）自然会产生一个三元组。

**具体说明**：
- 假设有两个函子 $U : \mathcal{B} \to \mathcal{A}$ 和 $F : \mathcal{A} \to \mathcal{B}$，其中 $F$ 是 $U$ 的左伴随（即 $F \dashv U$）。
- 伴随对的单位是自然变换 $\eta : \text{id}_{\mathcal{A}} \to U \circ F$，而伴随对的余单位是自然变换 $\epsilon : F \circ U \to \text{id}_{\mathcal{B}}$。

**通俗解释**：
- 伴随对中的左伴随 $F$ 和右伴随 $U$ 之间存在紧密的关系，通过自然变换 $\eta$ 和 $\epsilon$ 连接。
- 这种结构可以用来构造三元组，使得三元组的定义符合伴随对的性质。

#### 14.3.3 命题：伴随对产生三元组

**命题**：设 $U : \mathcal{B} \to \mathcal{A}$ 和 $F : \mathcal{A} \to \mathcal{B}$ 是一对伴随对，其中 $F \dashv U$，单位为 $\eta : \text{id}_{\mathcal{A}} \to U \circ F$，余单位为 $\epsilon : F \circ U \to \text{id}_{\mathcal{B}}$。那么 $(U \circ F, \eta, U \circ \epsilon \circ F)$ 构成了 $\mathcal{A}$ 上的一个三元组。

**通俗解释**：
- 通过组合伴随对中的函子 $U$ 和 $F$，以及自然变换 $\eta$ 和 $\epsilon$，我们可以构造出一个满足三元组定义的结构。
- 这个三元组反映了伴随对中的基本性质，并将其抽象化为范畴论中的一个重要工具。

**证明提示**：
- 需要验证三元组的单位律和结合律，这可以通过伴随对的公理来证明。
- 具体步骤留作习题。

#### 14.3.4 表示三元组：单子与M-集

**示例**：设 $M$ 是一个幺半群（Monoid）。在集合范畴 $\text{Set}$ 上，可以定义一个三元组 $T = (T, \eta, \mu)$，其中：
- $T(S) = M \times S$，即将每个集合 $S$ 映射到 $M \times S$。
- 自然变换 $\eta_S : S \to M \times S$ 定义为 $\eta_S(s) = (1, s)$，其中 $1$ 是 $M$ 的单位元。
- 自然变换 $\mu_S : M^2 \times S \to M \times S$ 定义为 $\mu_S(m_1, m_2, s) = (m_1 m_2, s)$。

**验证**：
- **单位律**：验证 $\mu \circ T \eta = \mu \circ \eta T = \text{id}_T$。
- **结合律**：验证 $\mu \circ T \mu = \mu \circ \mu T$。

**通俗解释**：
- 这里的三元组直接来源于幺半群 $M$ 的结构，$\eta$ 将每个元素映射到带有单位元的对，而 $\mu$ 则将两个幺半群元素的乘积作用于集合元素上。
- 这种表示方式表明，三元组可以用来封装幺半群的代数结构，并在范畴论中进行泛化。

**进一步说明**：
- 在 $M$-集的范畴中，自由 $M$-集由 $M \times S$ 构成，其中 $S$ 是生成元集合。
- 伴随对的左伴随函子是自由函子，右伴随函子是底层函子。
- 通过自由函子和底层函子，可以构造出与上述例子对应的三元组。

#### 14.3.5 余三元组（Cotriples）

**定义**：**余三元组（Cotriples）** $G = (G, \epsilon, \delta)$ 在一个范畴 $\mathcal{A}$ 中，是 $\mathcal{A}^{op}$ 上的一个三元组。具体来说：
- $G$ 是一个自函子 $G : \mathcal{A} \to \mathcal{A}$。
- $\epsilon : G \to \text{id}$ 和 $\delta : G \to G^2$ 是自然变换。
- 它们需要满足与三元组相似的单位律和结合律，但在对偶范畴中。

**通俗解释**：
- 余三元组是三元组的对偶概念，通过反转态射方向得到。
- 在实际应用中，余三元组常用于不同的构造和理论中，与三元组互补。

**用途**：
- 余三元组在后续章节中将被广泛使用，尤其是在拓扑和其他高级范畴论主题中。

#### 14.3.6 习题

1. **闭包算子**：
   - 设 $(P, \leq)$ 是一个偏序集，且 $T = (T, \eta, \mu)$ 是范畴 $\mathcal{C}(P, \leq)$ 上的一个三元组。
   - **证明**：对于任意 $x \in P$，有 $x \leq T(x)$ 且 $T(T(x)) = T(x)$。
   - **提示**：这样的函数 $T$ 被称为**闭包算子（Closure Operator）**，需要利用三元组的单位律和结合律来证明。

2. **伴随对产生三元组**：
   - **任务**：证明命题14.3.3中所述的结论。
   - **提示**：利用伴随对的定义和三元组的单位律及结合律，逐步验证三元组的公理是否成立。

3. **Kleene闭包的三元组**：
   - 设 $T : \text{Set} \to \text{Set}$ 是将集合 $A$ 映射到其Kleene闭包 $A^*$ 的函子。
   - $\eta_A : A \to A^*$ 将元素 $a$ 映射到单元素字符串 $(a)$。
   - $\mu_A : A^{**} \to A^*$ 将嵌套的字符串连接起来，例如 $\mu_A(((a, b), (c, d, e), (), (a, a))) = (a, b, c, d, e, a, a)$。
   - **任务**：证明 $\eta : \text{id} \to T$ 和 $\mu : T \circ T \to T$ 是自然变换，并且 $(T, \eta, \mu)$ 是一个三元组。

**通俗解释**：
- 这些习题旨在通过具体例子加深对三元组及其性质的理解。
- 通过证明这些命题，能够更好地掌握三元组的结构和它们在不同范畴中的应用。

### 总结

本节介绍了**三元组（Triples）**的概念及其在范畴论中的重要性。三元组作为一种基于自函子的结构，提供了一个强大的工具来抽象和研究伴随对及通用代数。通过具体的定义和示例，如来自幺半群的表示，我们理解了三元组如何在范畴内部封装代数结构。此外，余三元组作为三元组的对偶概念，扩展了其应用范围。

通过习题的练习，进一步巩固了对三元组的理解，尤其是如何通过伴随对构造三元组以及如何在具体范畴中验证三元组的性质。这些理论不仅在纯数学中具有深远的影响，在计算机科学中，特别是在函数式编程和类型理论中，也有广泛的应用。

掌握三元组的概念和性质，为后续学习范畴论中的更高级主题（如代数三元组、单子变换以及其在程序语义学中的应用）奠定了坚实的基础。

### ----------------------------

### 概览

在范畴论中，**三元组（Triples）**，通常称为**单子（Monads）**，是基于自函子（Endofunctor）的一种结构，广泛应用于拓扑学（Toposes）及相关领域。三元组不仅是伴随对（Adjoint Pairs）概念的抽象，还在一定程度上推广了通用代数（Universal Algebra）的思想。通过研究三元组，我们能够深入理解范畴中的代数结构、递归过程以及计算模型。本节将详细探讨三元组的定义、来源及其在范畴中的分解方法，包括Kleisli范畴和Eilenberg–Moore代数。此外，通过具体示例和习题，我们将加深对这些抽象概念的理解。

### 14.4 三元组的分解

#### 14.4.1 三元组的Kleisli范畴

**定义与构造**：

设 $T = (T, \eta, \mu)$ 是范畴 $\mathcal{C}$ 上的一个三元组。Kleisli范畴 $\mathcal{K}(T)$ 是基于这个三元组构造的一个新范畴，其对象与原范畴 $\mathcal{C}$ 相同。但在Kleisli范畴中，态射的定义有所不同：

- **对象**：与 $\mathcal{C}$ 相同。
- **态射**：从对象 $A$ 到对象 $B$ 的态射是原范畴 $\mathcal{C}$ 中的态射 $A \to T(B)$。
- **组合**：若有 $f : A \to T(B)$ 和 $g : B \to T(C)$，则在Kleisli范畴中的复合 $g \circ_K f$ 定义为：
  $$
  A \xrightarrow{f} T(B) \xrightarrow{T(g)} T^2(C) \xrightarrow{\mu_C} T(C)
  $$
- **恒等态射**：对象 $A$ 的恒等态射是自然变换 $\eta_A : A \to T(A)$。

**通俗解释**：

Kleisli范畴的构造使得我们可以在新的范畴中以“效果”封装的方式处理态射。具体来说，态射 $A \to T(B)$ 可以被看作是从 $A$ 到 $B$ 的“带有额外结构或效果”的映射。组合规则利用了三元组的乘法 $\mu$，确保了组合的合理性和一致性。

**伴随函子**：

在Kleisli范畴中，存在两个重要的函子：

- **底层函子 $U : \mathcal{K}(T) \to \mathcal{C}$**：将Kleisli范畴中的对象和态射映射回原范畴。具体来说，$U(A) = T(A)$，态射 $f : A \to T(B)$ 映射为 $\mu_B \circ T(f)$。
- **自由函子 $F : \mathcal{C} \to \mathcal{K}(T)$**：将原范畴中的对象和态射直接映射到Kleisli范畴。具体来说，$F(A) = A$，态射 $g : A \to B$ 映射为 $\eta_B \circ g$。

**伴随关系**：

自由函子 $F$ 是底层函子 $U$ 的左伴随函子，即 $F \dashv U$，并且三元组 $T$ 满足 $T = U \circ F$。

**通俗解释**：

Kleisli范畴的构造揭示了三元组作为伴随对的一个体现。通过Kleisli范畴，我们能够以更具操作性的方式理解和应用三元组的结构，尤其在理论计算机科学中具有重要应用，如处理副作用、状态和其他计算效果。

#### 14.4.2 Eilenberg–Moore代数

**定义与构造**：

Eilenberg–Moore代数提供了另一种将三元组分解为伴随对的方法，这种方法在数学中比Kleisli构造更具兴趣，但在计算机科学中应用较少。

- **$T$-代数（$T$-algebra）**：对于三元组 $T = (T, \eta, \mu)$，一个 $T$-代数是一个对象 $(A, a)$，其中 $a : T(A) \to A$ 满足以下两个条件（即两个图形交换）：
  $$
  \begin{array}{ccc}
  T(A) & \xrightarrow{T(a)} & T(A) \\
  \downarrow{\mu_A} & & \downarrow{a} \\
  A & \xrightarrow{a} & A \\
  \end{array}
  \quad
  \begin{array}{ccc}
  T(A) & \xrightarrow{\eta_A} & T(A) \\
  \downarrow{a} & & \downarrow{a} \\
  A & = & A \\
  \end{array}
  $$
- **态射**：两个 $T$-代数 $(A, a)$ 和 $(B, b)$ 之间的态射是原范畴 $\mathcal{C}$ 中的态射 $f : A \to B$，满足 $f \circ a = b \circ T(f)$。

**通俗解释**：

Eilenberg–Moore代数通过引入代数结构，使我们能够在范畴中处理更复杂的代数对象。每个 $T$-代数不仅包含一个对象，还包含一个结构映射 $a$，该映射将三元组作用后的对象映射回自身，满足一定的代数规律。

**伴随函子**：

与Kleisli构造类似，Eilenberg–Moore代数也关联了一个伴随对：

- **底层函子 $U : \mathcal{A}^T \to \mathcal{C}$**：将 $T$-代数映射回原范畴，$U(A, a) = A$，态射保持不变。
- **自由函子 $F : \mathcal{C} \to \mathcal{A}^T$**：将对象 $A$ 映射为 $(T(A), \mu_A)$，态射 $f : A \to B$ 映射为 $T(f) \circ \eta_A$。

**命题14.4.3**：

**命题**：上述自由函子 $F$ 是底层函子 $U$ 的左伴随函子，并且由这个伴随对产生的三元组正是原三元组 $T$。

**通俗解释**：

Eilenberg–Moore构造提供了从三元组到伴随对的另一种途径，强调了代数结构在范畴中的重要性。这种构造在数学中的应用比在计算机科学中更为广泛，尤其在处理代数结构和代数方程时发挥重要作用。

#### 14.4.3 命题

**命题14.4.3**：上述定义的自由函子 $F$ 是底层函子 $U$ 的左伴随函子，并且由这个伴随对产生的三元组正是原三元组 $T$。

**通俗解释**：

这个命题确认了Eilenberg–Moore代数构造与伴随对之间的紧密联系。通过自由函子和底层函子的配对，我们能够从代数结构回到原始的三元组结构，确保了代数和函子之间的对应关系。

**证明提示**：

要证明 $F \dashv U$，需要验证对于每个对象 $A$ 和 $(B, b)$，存在自然的同构：
$$
\text{Hom}_{\mathcal{A}^T}(F(A), (B, b)) \cong \text{Hom}_{\mathcal{C}}(A, U(B, b))
$$
这需要利用 $T$-代数的定义和三元组的公理。

#### 14.4.4 Kleisli范畴与自由代数

**命题与解释**：

Kleisli范畴 $\mathcal{K}(T)$ 等价于自由 $T$-代数的全子范畴。这意味着Kleisli范畴中的态射可以看作是自由代数中的替换操作。

**示例**：

考虑在习题3中定义的列表三元组。在Kleisli范畴中，一个态射 $f : A \to B$ 是一个集合函数 $A \to T(B)$，即 $A$ 到 $B$ 列表的映射。例如，设 $A = \{a, b\}$ 和 $B = \{c, d, e\}$，若 $f(a) = \text{"cddc"}$ 和 $f(b) = \text{"ec"}$，则Kleisli范畴中的态射 $f : A \to B$ 代表了将 $A$ 的元素映射为 $B$ 的字符串。

两个态射 $f : A \to T(B)$ 和 $g : B \to T(C)$ 的复合 $g \circ_K f$ 通过替换操作实现：
$$
g \circ_K f = \mu_C \circ T(g) \circ f
$$
具体来说，将 $f$ 的结果应用 $T(g)$ 后，通过 $\mu$ 将嵌套的列表展平。

**通俗解释**：

在Kleisli范畴中，态射的复合可以被视为一系列带有特定结构或效果的操作的组合。通过使用 $\mu$ 操作，我们能够将这些操作的嵌套结果合并为一个统一的结果，这在处理递归和嵌套效果时非常有用。

**进一步解释**：

Kleisli范畴允许我们以更直观的方式处理带有额外结构或效果的计算，例如列表、可能性、状态等。在计算机科学中，这种结构化的态射复合方式被广泛应用于函数式编程和效果系统中。

#### 14.4.5 习题

1. **证明**：证明 $\eta_A : A \to T(A)$ 在Kleisli范畴 $\mathcal{K}(T)$ 中是对象 $A$ 的恒等态射。

   **提示**：
   - 在Kleisli范畴中，恒等态射定义为 $\eta_A : A \to T(A)$。
   - 需要验证对于任何 $f : A \to T(B)$，有 $f \circ_K \eta_A = f$ 和 $\eta_B \circ_K f = f$。
   - 利用三元组的单位律 $\mu \circ T(\eta) = \mu \circ \eta T = \text{id}$。

2. **证明**：证明Kleisli范畴中定义的态射复合是结合的。

   **提示**：
   - 设有三个态射 $f : A \to T(B)$, $g : B \to T(C)$, 和 $h : C \to T(D)$。
   - 计算 $(h \circ_K g) \circ_K f$ 和 $h \circ_K (g \circ_K f)$。
   - 利用三元组的结合律 $\mu \circ T(\mu) = \mu \circ \mu T$，证明两者相等。

3. **习题3†**：设 $(T, \eta, \mu)$ 是在集合范畴 $\text{Set}$ 中定义的三元组，如14.3节的习题3所述。

   a. **证明**：证明该三元组的代数是幺半群。具体来说，如果 $\alpha : T(A) \to A$ 是一个代数，则定义 $ab = \alpha(a, b)$ 使得 $A$ 成为一个幺半群，并且每个幺半群（最多同构）都由这种方式产生。

      **提示**：
      - 验证 $\alpha$ 满足幺半群的结合律和单位元条件。
      - 利用三元组的结合律和单位律来证明幺半群的公理。

   b. **证明**：证明代数同态是幺半群同态，并且每个幺半群同态都由代数同态产生。

      **提示**：
      - 定义代数同态的性质，并与幺半群同态的定义进行对比。
      - 证明代数同态保持幺半群的乘法和单位元。

4. **证明**：证明命题14.4.3，即自由函子 $F$ 是底层函子 $U$ 的左伴随函子，并且由这个伴随对产生的三元组正是原三元组 $T$。

   **提示**：
   - 回顾伴随对的定义，证明 $F \dashv U$。
   - 通过验证三元组的单位律和结合律，确认由伴随对构造的三元组与原三元组一致。

### 总结

本节深入探讨了三元组（单子）在范畴论中的重要性及其分解方法。通过Kleisli范畴和Eilenberg–Moore代数的构造，我们能够将三元组视为伴随对的抽象表现。这些构造不仅在数学中具有深远的影响，在计算机科学，尤其是理论计算机科学和函数式编程中，也发挥着关键作用。

**Kleisli范畴** 提供了一种处理带有额外结构或效果的态射复合的方式，使得我们能够以更加灵活和模块化的方式构建复杂的计算过程。而 **Eilenberg–Moore代数** 则强调了代数结构在范畴中的重要性，通过引入代数结构映射，我们能够更深入地理解和操作范畴中的对象。

通过具体示例，如列表三元组，我们看到了这些抽象概念在实际应用中的体现。同时，通过习题的练习，巩固了对三元组及其相关构造的理解，为进一步研究范畴论中的高级主题打下了坚实的基础。

掌握三元组的分解方法不仅有助于理解范畴论的基本结构，还为探索更复杂的代数和计算模型提供了有力的工具。这些理论在处理递归、代数结构以及计算效果时尤为重要，是深入理解范畴论及其应用的关键步骤。

### ---------------------------

### 概览

**Scott 域（Scott Domains）** 是范畴论中用于构建计算模型的一种重要工具，特别是在研究未类型化的λ-演算（untyped λ-calculus）模型时。Scott 域通过在笛卡尔闭范畴中寻找满足 $D \cong [D \to D]$ 的对象，提供了一种在非集合范畴中建模计算的方法。然而，由于直接使用固定点构造存在困难，Scott 和 Plotkin 提出了巧妙的技巧来克服这些障碍。本节将深入探讨Scott 域的定义、构造方法及其在范畴论中的应用，并通过具体示例和习题加深理解。

### 14.5 Scott 域

#### 14.5.1 使用固定点构造对象 $D \cong [D \to D]$ 的困难

**背景**：

在[Scott, 1972]的研究中，提出了一种构造未类型化λ-演算模型的通用方法。一个关键步骤是找到一个对象 $D$ 在一个笛卡尔闭范畴中，使得 $D \cong [D \to D]$。这里 $[D \to D]$ 表示对象 $D$ 到自身的函子空间。然而，直接使用固定点构造来找到这样的对象存在困难。

**困难解析**：

1. **非函子性**：
   - 操作 $D \mapsto [D \to D]$ 不是一个函子。原因在于，对于态射 $D \to E$，它不仅会诱导态射 $[D \to D] \to [D \to E]$，还会诱导其他态射，如 $[E \to D] \to [D \to D]$ 等。
   - 由于缺乏方向性，无法直接定义 $D \cong [D \to D]$ 的固定点。

2. **Smyth 和 Plotkin 的技巧**：
   - 为了克服上述困难，Smyth 和 Plotkin 提出了一个巧妙的技巧，利用范畴的额外结构来实现对象 $D \cong [D \to D]$。

**通俗解释**：

- **固定点问题**：在集合范畴中，寻找一个集合 $D$ 使得 $D \cong [D \to D]$ 并不直接可行，因为函数空间的结构复杂且不具备函子的特性。
- **Smyth-Plotkin 解决方案**：通过引入“丰富化范畴”（Enriched Categories）的概念，使得 hom 集合具备额外的结构（如偏序），从而能够定义合适的函子和态射，进而实现 $D \cong [D \to D]$ 的等价关系。

#### 14.5.2 Smyth–Plotkin 范畴的定义

**定义**：

一个范畴 $\mathcal{C}$ 被称为**Smyth–Plotkin 范畴**，如果它满足以下条件：

1. **SP–1：笛卡尔闭**  
   $\mathcal{C}$ 是笛卡尔闭范畴，即对任意对象 $A$，存在一个内部 Hom 对象 $[A \to B]$。

2. **SP–2：Hom 集合为偏序集**  
   对于 $\mathcal{C}$ 中的任意对象对 $A, B$，Hom 集合 $\text{Hom}(A, B)$ 是一个偏序集。

3. **SP–3：单调性**  
   若 $f : A \to B$、$g, h : B \to C$，且 $g \leq h$，则 $g \circ f \leq h \circ f$ 且 $k \circ g \leq k \circ h$ 对任意 $k : C \to D$ 成立。

4. **SP–4：Hom 函子的单调性**  
   若 $g \leq h : B \to C$，则：
   - $[g \to A] \leq [h \to A] : [C \to A] \to [B \to A]$
   - $[A \to g] \leq [A \to h] : [A \to B] \to [A \to C]$

5. **SP–5：存在可数链的极限与余极限**  
   $\mathcal{C}$ 具有沿可数链的极限和余极限。

6. **SP–6：Hom 极限性质**  
   若 $A = \lim A_i$，则 $\text{Hom}(B, A) \cong \lim \text{Hom}(B, A_i)$ 对每个对象 $B$ 成立，并且这个同构是偏序同构。

7. **SP–7：Hom 余极限性质**  
   若 $A = \colim A_i$，则 $\text{Hom}(A, C) \cong \lim \text{Hom}(A_i, C)$ 对每个对象 $C$ 成立，并且这个同构是偏序同构。

**通俗解释**：

- **笛卡尔闭**：确保范畴中存在内部 Hom 对象，使得可以在范畴内部定义函子空间。
- **Hom 为偏序集**：为态射之间引入一个有序关系，允许比较态射的大小。
- **单调性**：态射的组合保持有序关系，即如果一个态射小于另一个态射，则它们的复合也保持这个关系。
- **Hom 函子的单调性**：Hom 函子在态射间的映射也是单调的，保持了有序关系。
- **极限与余极限**：确保范畴中存在可数链的极限和余极限，便于构造固定点和其他代数结构。

#### 14.5.3 关联类别 $\mathcal{L}\mathcal{A}(\mathcal{C})$ 和 $\mathcal{R}\mathcal{A}(\mathcal{C})$

**定义与构造**：

给定一个 Smyth–Plotkin 范畴 $\mathcal{C}$，我们构造两个新范畴：

1. **左伴随范畴 $\mathcal{L}\mathcal{A}(\mathcal{C})$**：
   - **对象**：与 $\mathcal{C}$ 相同。
   - **态射**：从 $A$ 到 $B$ 的态射是 $\mathcal{C}$ 中的态射对 $(f, g)$，其中 $f : A \to B$ 且 $g : B \to A$，满足 $\text{id}_A \leq g \circ f$ 且 $f \circ g \leq \text{id}_B$。

2. **右伴随范畴 $\mathcal{R}\mathcal{A}(\mathcal{C})$**：
   - **对象**：与 $\mathcal{C}$ 相同。
   - **态射**：从 $A$ 到 $B$ 的态射是 $\mathcal{C}$ 中的态射对 $(f, g)$，其中 $f : A \to B$ 且 $g : B \to A$，满足 $g \circ f \leq \text{id}_A$ 且 $\text{id}_B \leq f \circ g$。

**性质**：

- **对偶等价**：
  - $\mathcal{L}\mathcal{A}(\mathcal{C})^{op} \cong \mathcal{R}\mathcal{A}(\mathcal{C})$。
  - 具体来说，一个在 $\mathcal{L}\mathcal{A}(\mathcal{C})$ 中的态射 $(f, g) : A \to B$ 对应于在 $\mathcal{R}\mathcal{A}(\mathcal{C})$ 中的态射 $(g, f) : B \to A$。

**通俗解释**：

- **左伴随与右伴随**：
  - 在 $\mathcal{L}\mathcal{A}(\mathcal{C})$ 中，态射 $(f, g)$ 类似于左伴随函子和右伴随函子的关系，其中 $f$ 是一个左伴随，而 $g$ 是对应的右伴随。
  - 在 $\mathcal{R}\mathcal{A}(\mathcal{C})$ 中，态射 $(f, g)$ 则体现了右伴随和左伴随的对偶关系。

- **态射对的确定性**：
  - 在这两个范畴中，态射对 $(f, g)$ 和 $(g, f)$ 彼此确定，因为右伴随和左伴随函子之间存在紧密的联系。

#### 14.5.4 定义：可重tract 和 coretract

**定义**：

- **可重tract（Retract）**：在 $\mathcal{L}\mathcal{A}(\mathcal{C})$ 或 $\mathcal{R}\mathcal{A}(\mathcal{C})$ 中，态射 $(f, g) : D \to E$ 被称为**可重tract**，如果 $g \circ f = \text{id}_D$。
  
- **coretract**：在 $\mathcal{R}\mathcal{A}(\mathcal{C})$ 中，态射 $(f, g) : D \to E$ 被称为**coretract**，如果 $f \circ g = \text{id}_E$。

**通俗解释**：

- **可重tract**：
  - $(f, g)$ 是一个可重tract 意味着 $f$ 和 $g$ 互为逆态射，使得组合 $g \circ f$ 恰好是 $D$ 的恒等态射。
  
- **coretract**：
  - 类似地，$(f, g)$ 是一个 coretract 意味着组合 $f \circ g$ 是 $E$ 的恒等态射。

- **直观理解**：
  - 可重tract 和 coretract 类似于范畴论中的“retraction”和“section”，即一种态射对的部分逆关系。

#### 14.5.5 定理：可数可重tract 链的极限与余极限

**定理**：

设 $\mathcal{C}$ 是一个 Smyth–Plotkin 范畴。假设 $\mathcal{C}$ 中的 Hom 集合在可数递增链上具有最小上界，并且：

1. **可重tract 链**：
   - 设有一个可数的可重tract 链：
     $$
     A_0 \xrightarrow{(h_{01}, k_{01})} A_1 \xrightarrow{(h_{12}, k_{12})} A_2 \xrightarrow{(h_{23}, k_{23})} \cdots \xrightarrow{(h_{n, n+1}, k_{n, n+1})}} A_{n+1} \xrightarrow{} \cdots
     $$
   - $(h_{ij}, k_{ij}) : A_i \to A_j$ 是 $\mathcal{L}\mathcal{A}(\mathcal{C})$ 中的态射对。

2. **极限与余极限的存在**：
   - 设 $A = \colim A_i$ 是 $\mathcal{C}$ 中该链的余极限。

则存在唯一的态射 $g_i : A \to A_i$，使得 $(h_{ij}, k_{ij})$ 构成一个 $\mathcal{L}\mathcal{A}(\mathcal{C})$ 中的余极限。

**通俗解释**：

- **可重tract 链**：
  - 一个可重tract 链是一系列对象和态射对，其中每对态射 $(h_{ij}, k_{ij})$ 满足 $k_{ij} \circ h_{ij} = \text{id}_{A_i}$。

- **极限与余极限的关联**：
  - 当范畴 $\mathcal{C}$ 满足 Smyth–Plotkin 条件时，可重tract 链的余极限在 $\mathcal{L}\mathcal{A}(\mathcal{C})$ 中同样存在，并且与原范畴中的余极限相关联。

- **应用意义**：
  - 该定理为在 Smyth–Plotkin 范畴中构造特定的对象 $D$ 提供了方法，使得 $D \cong [D \to D]$，这是构建 Scott 域的关键步骤。

#### 14.5.6 Homomorphism 与固定点构造

**内容摘要**：

- 在 $\mathcal{L}\mathcal{A}(\mathcal{C})$ 或 $\mathcal{R}\mathcal{A}(\mathcal{C})$ 中，如果 $(f, g) : D \to E$ 是一个同态，那么可以构造一个态射 $[D \to D] \to [E \to E]$。
  
- 通过一些修改，可以获得一个更有用的结论，即在某些条件下，可以找到一个固定点对象 $D$ 使得 $D \cong [D \to D]$。

**通俗解释**：

- **同态的作用**：
  - 同态 $(f, g)$ 可以看作是在 Hom 集合上进行的某种“映射”，这种映射能够帮助我们在范畴内部找到满足特定等价关系的对象。

- **固定点构造**：
  - 通过构造一个序列，并利用极限与余极限的性质，可以最终找到一个对象 $D$，使得 $D \cong [D \to D]$，这正是我们在构建 Scott 域时所需要的。

- **实际操作**：
  - 需要通过一系列态射对和链的构造，逐步逼近最终的固定点对象。

#### 14.5.7 示例：ω-CPOs 范畴中的 Scott 域构造

**示例**：

考虑 $\mathcal{C}$ 是由ω-完备偏序集（ω-CPOs）和保持可数上确界的函数构成的范畴。

1. **笛卡尔闭性**：
   - 该范畴是笛卡尔闭的，因为存在对象 $[A \to B]$，表示从 $A$ 到 $B$ 的函数集合，并且保持可数上确界。

2. **满足 Smyth–Plotkin 条件**：
   - Hom 集合 $\text{Hom}(A, B)$ 是一个偏序集，按函数点态序定义。
   - 满足单调性和 Hom 函子的单调性。
   - 存在可数链的极限和余极限，并且这些极限与 Hom 集合的偏序结构保持一致。

3. **构造固定点对象 $D$**：
   - 选择一个对象 $A_0$（例如具有最小元素 $\bot$ 的ω-CPO）。
   - 设 $A = A_0 \times A_0 \times \cdots$ 是 $A_0$ 的可数乘积，满足 $A \cong A \times A$。
   - 构造态射 $f : A \to [A \to A]$，将每个元素映射为常函数。
   - 构造态射 $g : [A \to A] \to A$，将函数映射为其在最小元素上的值。
   - 利用上述定理，构造出一个对象 $D$，使得 $D \cong [D \to D] \cong D \times D$。

**通俗解释**：

- **ω-CPOs**：
  - ω-完备偏序集（ω-CPOs）是具有可数上确界的偏序集，广泛用于理论计算机科学中建模递归和计算过程。

- **构造过程**：
  - 通过选择一个适当的对象 $A_0$ 和构造其可数乘积 $A$，并利用态射对 $(f, g)$，我们能够逐步构造出一个满足 $D \cong [D \to D]$ 的固定点对象。

- **结果**：
  - 构造出的对象 $D$ 同时满足 $D \cong [D \to D]$ 和 $D \cong D \times D$，这使得 $D$ 成为一个 Scott 域，可以作为未类型化λ-演算的一个模型。

#### 14.5.8 习题

1. **证明**：证明由保持可数上确界的函数构成的 $\omega$-CPOs 范畴是笛卡尔闭的。
   
   **提示**：
   - 需要展示对于任意 $A, B$ 是 $\omega$-CPO 的对象，函数空间 $[A \to B]$ 也是 $\omega$-CPO。
   - 证明保持可数上确界的函数之间的点态序关系构成一个 $\omega$-CPO。

2. **证明**：证明每个偏序集都自由生成一个 $\omega$-CPO。

   **提示**：
   - 对于一个偏序集 $P$，构造其自由 $\omega$-CPO。
   - 定义 $P^b$ 为 $P$ 中所有可数递增链的集合，并定义适当的偏序关系。

3. **证明**：证明 $\omega$-CPOs 范畴具有极限（点态）和余极限。

   **提示**：
   - 利用 $\omega$-CPOs 的定义，证明极限和余极限可以逐点计算，并且保持 $\omega$-CPO 的性质。

4. **证明**：证明在一个偏序集 $P$ 中，$P$ 是可数链完备的当且仅当每个可数有向子集有一个最小上确界。

   **提示**：
   - 使用有向集和链的定义，证明两者之间的等价性。

### 总结

本节介绍了**Scott 域（Scott Domains）**在范畴论中的构造与应用，特别是在构建未类型化λ-演算模型时的重要性。通过引入**Smyth–Plotkin 范畴**，我们能够在具有丰富结构的范畴中找到满足 $D \cong [D \to D]$ 的对象，从而构建复杂的计算模型。通过Kleisli范畴和Eilenberg–Moore代数的分解方法，我们进一步理解了三元组（单子）与伴随对之间的关系，并掌握了如何利用这些工具在范畴内部进行固定点构造。

通过具体的示例，如ω-CPOs范畴中的Scott 域构造，以及丰富的习题，我们不仅加深了对理论的理解，也为实际应用奠定了基础。这些理论在理论计算机科学中，尤其是在构建递归数据类型、处理计算效果和设计类型系统时，具有广泛的应用价值。

掌握Scott 域及其相关构造方法，进一步拓展了我们在范畴论中的工具箱，为探索更复杂的代数结构和计算模型提供了强有力的支持。这些概念不仅在数学上具有深刻的理论意义，也在计算机科学中，特别是在程序语义学和类型理论中，发挥着重要作用。

### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------