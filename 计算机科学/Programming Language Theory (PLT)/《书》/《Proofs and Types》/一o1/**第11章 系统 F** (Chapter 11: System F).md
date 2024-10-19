[toc]



### **第11章 系统 F**

#### **引言**

**系统 F**（System F）是对简单类型演算（Simple Typed Calculus）的一种扩展，通过添加对**类型的抽象（abstraction on types）**操作而得。这个操作非常强大，特别是所有常用的数据类型（整数、列表等）都可以在其中定义。

系统 F 最初在证明论（Proof Theory）的背景下由 **Girard** 提出 [Gir71]，但在计算机科学领域，它被 **Reynolds** 独立地发现 [Reynolds]。

在本章中，我们将介绍系统 F 的最基本版本：它基于**蕴涵（implication）**和**全称量化（universal quantification）**。我们将满足于定义系统并展示其表达能力的一些例子。

---

### **11.1 演算**

#### **类型的定义**

类型从类型变量 $X, Y, Z, \dots$ 开始，通过以下两种操作定义：

1. **箭头类型（函数类型）**：如果 $U$ 和 $V$ 是类型，那么 $U \to V$ 是一个类型。

   - **解释**：$U \to V$ 表示从类型 $U$ 到类型 $V$ 的函数类型。

2. **全称量化类型（Π 类型）**：如果 $V$ 是一个类型，$X$ 是一个类型变量，那么 $\Pi X. V$ 是一个类型。

   - **解释**：$\Pi X. V$ 表示对类型变量 $X$ 的全称量化，类似于“对于所有类型 $X$，$V$ 成立”。

#### **项的构造**

有五种构造项的形式：

1. **变量**：$x^T, y^T, z^T, \dots$，类型为 $T$。

   - **解释**：这些是具有类型 $T$ 的变量。

2. **应用（Application）**：如果 $t$ 的类型为 $U \to V$，$u$ 的类型为 $U$，则 $t \ u$ 的类型为 $V$。

   - **解释**：将函数 $t$ 应用于参数 $u$，得到结果。

3. **λ-抽象（Lambda Abstraction）**：如果 $x^U$ 是类型为 $U$ 的变量，$v$ 是类型为 $V$ 的项，那么 $\lambda x^U. v$ 的类型为 $U \to V$。

   - **解释**：定义一个从类型 $U$ 到类型 $V$ 的函数，输入为 $x$，输出为 $v$。

4. **全称抽象（Universal Abstraction）**：如果 $v$ 是类型为 $V$ 的项，那么只要 $X$ 不在 $v$ 的自由变量的类型中自由出现，就可以构造 $\Lambda X. v$，其类型为 $\Pi X. V$。

   - **解释**：对类型 $X$ 进行抽象，得到一个对所有类型 $X$ 都适用的项。

   - **限制条件**：类型变量 $X$ 不能在 $v$ 的自由变量的类型中自由出现。这是为了避免类型的不一致性。

5. **全称应用（Universal Application）**（有时称为**抽取（extraction）**）：如果 $t$ 是类型为 $\Pi X. V$ 的项，$U$ 是一个类型，则 $t \ U$ 是类型为 $V[U/X]$ 的项。

   - **解释**：将类型 $U$ 代入 $t$ 中的类型变量 $X$，得到一个具体类型的项。

#### **转换规则**

除了应用和 λ-抽象的通常转换外，还有一个针对全称抽象和全称应用的转换：

$$
(\Lambda X. v) \ U \quad \to \quad v[U/X]
$$

- **解释**：将类型 $U$ 代入 $\Lambda X. v$ 中的 $X$，得到 $v$ 的一个实例。

#### **约定**

- 我们将 $U_1 \to U_2 \to \dots \to U_n \to V$ 写成不带括号的形式，表示：

  $$
  U_1 \to (U_2 \to (\dots \to (U_n \to V)\dots))
  $$

- 类似地，$f \ u_1 \ u_2 \ \dots \ u_n$ 表示：

  $$
  (\dots ((f \ u_1) \ u_2) \dots ) \ u_n
  $$

---

### **11.2 说明**

首先，让我们说明全称抽象中对变量的限制：

- **为什么需要限制？**

  - 如果我们能够构造 $\Lambda X. x^X$，那么在这个表达式中，自由变量 $x$ 的类型是什么呢？

  - 由于 $x$ 的类型是 $X$，而 $X$ 在全称抽象中被绑定，但 $x$ 是自由变量，这导致类型无法确定。

- **可以构造的例子**：

  - 我们可以构造 $\Lambda X. \lambda x^X. x^X$，其类型为 $\Pi X. X \to X$，这实际上是任何类型的恒等函数。

**关于“Π”类型的直观解释**：

- 一个类型为 $\Pi X. V$ 的对象可以被理解为一个函数，它将每个类型 $U$ 映射到类型 $V[U/X]$ 的一个对象。

- **问题**：

  - 这种解释会遇到**规模（size）**问题：为了理解 $\Pi X. V$，需要知道所有的 $V[U/X]$。

  - 但是在所有的 $V[U/X]$ 中，有一些（通常）比我们试图建模的类型更复杂，例如 $V[\Pi X. V / X]$。

  - 因此，在这种直观的解释中存在循环性（circularity），可能会导致问题。

- **实际上**：

  - 虽然存在这些问题，但系统实际上是可行的，但它对非逻辑性质的修改极为敏感。

**统一性（Uniformity）**：

- 我们可以稍微精确地理解对所有类型定义的函数的概念：在某种意义上，全称类型的函数必须是**“统一的”（uniform）**，即在所有类型上执行相同的操作。

- **λ-抽象**允许一定程度的非统一性，例如我们可以通过条件（if...then...else）来定义函数。

- **然而**，对于全称抽象，这种定义是不可想象的：全称类型的对象在不同类型上取的值必须本质上是“相同的”（参见附录 A.1.3）。

- **需要**通过适当的语义考虑来使这种模糊的直觉精确化。

---

### **11.3 简单类型的表示**

系统 F 的一个重要之处在于能够在其中定义常用的类型；我们将把本章的剩余部分用于此。

#### **11.3.1 布尔值**

我们定义布尔类型 **Bool**（与系统 T 中的不同）为：

$$
\text{Bool} \overset{\text{def}}{=} \Pi X. X \to X \to X
$$

- **解释**：布尔类型是对类型 $X$ 的全称量化，从 $X$ 到 $X$ 再到 $X$ 的函数。

**定义真值和假值**：

- **真值（True）**：
  $$
  T \overset{\text{def}}{=} \Lambda X. \lambda x^X. \lambda y^X. x
  $$
  - **解释**：对于任何类型 $X$，$T$ 是一个函数，取两个类型为 $X$ 的参数，返回第一个参数。

- **假值（False）**：
  $$
  F \overset{\text{def}}{=} \Lambda X. \lambda x^X. \lambda y^X. y
  $$
  - **解释**：对于任何类型 $X$，$F$ 是一个函数，取两个类型为 $X$ 的参数，返回第二个参数。

**条件选择（if-then-else）**：

- 如果 $u, v, t$ 分别是类型为 $U, U, \text{Bool}$ 的项，我们定义：
  $$
  D \ u \ v \ t \overset{\text{def}}{=} t \ U \ u \ v
  $$
  - **解释**：$D$ 是一个函数，取参数 $u, v$ 和布尔值 $t$，根据 $t$ 的值选择 $u$ 或 $v$。

**计算 $D \ u \ v \ T$ 和 $D \ u \ v \ F$**：

1. **计算 $D \ u \ v \ T$**：
   $$
   \begin{align*}
   D \ u \ v \ T &= (\Lambda X. \lambda x^X. \lambda y^X. x) \ U \ u \ v \\
   &\to (\lambda x^U. \lambda y^U. x) \ u \ v \\
   &\to (\lambda y^U. u) \ v \\
   &\to u
   \end{align*}
   $$
   - **解释**：$T$ 应用于类型 $U$，返回恒等于 $x$ 的函数，最终返回 $u$。

2. **计算 $D \ u \ v \ F$**：
   $$
   \begin{align*}
   D \ u \ v \ F &= (\Lambda X. \lambda x^X. \lambda y^X. y) \ U \ u \ v \\
   &\to (\lambda x^U. \lambda y^U. y) \ u \ v \\
   &\to (\lambda y^U. y) \ v \\
   &\to v
   \end{align*}
   $$
   - **解释**：$F$ 应用于类型 $U$，返回恒等于 $y$ 的函数，最终返回 $v$。

#### **11.3.2 乘积类型**

我们定义乘积类型 $U \times V$ 为：

$$
U \times V \overset{\text{def}}{=} \Pi X. (U \to V \to X) \to X
$$

- **解释**：这是一个对类型 $X$ 的全称量化，从 $U$ 和 $V$ 到 $X$ 的函数再到 $X$。

**定义对（Pair）**：

- **对 $⟨u, v⟩$**：
  $$
  \langle u, v \rangle \overset{\text{def}}{=} \Lambda X. \lambda x^{U \to V \to X}. x \ u \ v
  $$
  - **解释**：这是一个函数，取一个从 $U$ 和 $V$ 到 $X$ 的函数 $x$，并将 $u$ 和 $v$ 作为参数应用于 $x$。

**定义投影**：

1. **第一投影（$\pi_1$）**：
   $$
   \pi_1 \ t \overset{\text{def}}{=} t \ U \ (\lambda x^U. \lambda y^V. x)
   $$
   - **解释**：将 $t$ 应用于类型 $U$ 和一个返回第一个参数的函数，得到 $t$ 的第一分量。

2. **第二投影（$\pi_2$）**：
   $$
   \pi_2 \ t \overset{\text{def}}{=} t \ V \ (\lambda x^U. \lambda y^V. y)
   $$
   - **解释**：将 $t$ 应用于类型 $V$ 和一个返回第二个参数的函数，得到 $t$ 的第二分量。

**计算 $\pi_1 \langle u, v \rangle$ 和 $\pi_2 \langle u, v \rangle$**：

1. **计算 $\pi_1 \langle u, v \rangle$**：
   $$
   \begin{align*}
   \pi_1 \langle u, v \rangle &= (\Lambda X. \lambda x^{U \to V \to X}. x \ u \ v) \ U \ (\lambda x^U. \lambda y^V. x) \\
   &\to (\lambda x^{U \to V \to U}. x \ u \ v) \ (\lambda x^U. \lambda y^V. x) \\
   &\to (\lambda x^U. \lambda y^V. x) \ u \ v \\
   &\to (\lambda y^V. u) \ v \\
   &\to u
   \end{align*}
   $$

2. **计算 $\pi_2 \langle u, v \rangle$**：
   $$
   \begin{align*}
   \pi_2 \langle u, v \rangle &= (\Lambda X. \lambda x^{U \to V \to X}. x \ u \ v) \ V \ (\lambda x^U. \lambda y^V. y) \\
   &\to (\lambda x^{U \to V \to V}. x \ u \ v) \ (\lambda x^U. \lambda y^V. y) \\
   &\to (\lambda x^U. \lambda y^V. y) \ u \ v \\
   &\to (\lambda y^V. y) \ v \\
   &\to v
   \end{align*}
   $$

**注意**：关系 $h\pi_1 t, \pi_2 t i \equiv t$ 并不成立，即使我们允许 $λ x^U. t \ x \ t$ 和 $Λ X. t \ X \ t$。

#### **11.3.3 空类型**

我们可以定义**空类型（Emp）**为：

$$
\text{Emp} \overset{\text{def}}{=} \Pi X. X
$$

- **解释**：对所有类型 $X$，空类型是类型 $X$ 的对象。

**定义 $\varepsilon_U \ t$**：

- 如果 $t$ 是类型为 $\text{Emp}$ 的项，那么 $\varepsilon_U \ t \overset{\text{def}}{=} t \ U$。

- **解释**：将 $t$ 应用于任意类型 $U$，得到类型为 $U$ 的对象。

#### **11.3.4 和类型**

如果 $U, V$ 是类型，我们可以定义和类型 $U + V$ 为：

$$
U + V \overset{\text{def}}{=} \Pi X. (U \to X) \to (V \to X) \to X
$$

- **解释**：对所有类型 $X$，从 $U$ 到 $X$ 的函数和从 $V$ 到 $X$ 的函数，再到 $X$。

**定义注入（Injections）**：

1. **左注入（$\iota_1$）**：
   $$
   \iota_1 \ u \overset{\text{def}}{=} \Lambda X. \lambda x^{U \to X}. \lambda y^{V \to X}. x \ u
   $$
   - **解释**：将 $u$ 注入到和类型的左侧。

2. **右注入（$\iota_2$）**：
   $$
   \iota_2 \ v \overset{\text{def}}{=} \Lambda X. \lambda x^{U \to X}. \lambda y^{V \to X}. y \ v
   $$
   - **解释**：将 $v$ 注入到和类型的右侧。

**定义匹配（Case Analysis）**：

- 如果 $u, v, t$ 分别是类型为 $U, U, R + S$ 的项，我们定义：

  $$
  \delta \ x. \ u \ y. \ v \ t \overset{\text{def}}{=} t \ U \ (\lambda x^U. u) \ (\lambda y^V. v)
  $$

  - **解释**：对 $t$ 进行模式匹配，如果是左注入，绑定 $x$ 并执行 $u$；如果是右注入，绑定 $y$ 并执行 $v$。

**计算 $\delta \ x. \ u \ y. \ v \ (\iota_1 \ r)$**：

1. **计算过程**：
   $$
   \begin{align*}
   \delta \ x. \ u \ y. \ v \ (\iota_1 \ r) &= (\Lambda X. \lambda x^{R \to X}. \lambda y^{S \to X}. x \ r) \ U \ (\lambda x^R. u) \ (\lambda y^S. v) \\
   &\to (\lambda x^{R \to U}. \lambda y^{S \to U}. x \ r) \ (\lambda x^R. u) \ (\lambda y^S. v) \\
   &\to (\lambda y^{S \to U}. (\lambda x^R. u) \ r) \ (\lambda y^S. v) \\
   &\to (\lambda x^R. u) \ r \\
   &\to u[r/x]
   \end{align*}
   $$
   - **解释**：匹配到左注入，将 $r$ 绑定到 $x$，然后计算 $u[r/x]$。

2. **类似地**，可以计算 $\delta \ x. \ u \ y. \ v \ (\iota_2 \ s) = v[s/y]$。

**注意**：

- 然而，这种翻译并不能解释与和类型相关的**交换或次级转换（commuting or secondary conversions）**。

- 同样的注意事项适用于类型 $\text{Emp}$ 和类型 $\text{Bool}$，它们具有和类型的结构，并且可以为其编写交换规则。

#### **11.3.5 存在类型**

如果 $V$ 是一个类型，$X$ 是一个类型变量，那么可以定义存在类型为：

$$
\Sigma X. V \overset{\text{def}}{=} \Pi Y. (\Pi X. V \to Y) \to Y
$$

- **解释**：这与存在量化相对应，表示存在某个类型 $X$ 使得 $V$ 成立。

**定义**：

- 如果 $U$ 是一个类型，$v$ 是类型为 $V[U/X]$ 的项，那么我们定义：

  $$
  \langle U, v \rangle \overset{\text{def}}{=} \Lambda Y. \lambda x^{\Pi X. V \to Y}. x \ U \ v
  $$

  - **解释**：将类型 $U$ 和项 $v$ 封装成一个存在类型的对象。

**对应于存在的引入**，存在一个消解（elimination）：

- 如果 $w$ 是类型为 $W$ 的项，$t$ 是类型为 $\Sigma X. V$ 的项，$X$ 是类型变量，$x$ 是类型为 $V$ 的变量，并且 $w$ 的自由变量的类型中只有 $x$ 包含 $X$ 的自由出现，那么可以构造：

  $$
  \nabla X. x. w \ t \overset{\text{def}}{=} t \ W \ (\Lambda X. \lambda x^V. w)
  $$

  - **解释**：对 $t$ 进行解包，得到类型为 $V$ 的 $x$，然后在 $w$ 中使用 $x$。

**计算 $(\nabla X. x. w) \langle U, v \rangle$**：

$$
\begin{align*}
(\nabla X. x. w) \langle U, v \rangle &= (\Lambda Y. \lambda x^{\Pi X. V \to Y}. x \ U \ v) \ W \ (\Lambda X. \lambda x^V. w) \\
&\to (\lambda x^{\Pi X. V \to W}. x \ U \ v) \ (\Lambda X. \lambda x^V. w) \\
&\to (\Lambda X. \lambda x^V. w) \ U \ v \\
&\to (\lambda x^{V[U/X]}. w[U/X]) \ v \\
&\to w[U/X][v/x^{V[U/X]}]
\end{align*}
$$

- **解释**：通过消解存在类型，我们得到了 $w$，其中类型变量 $X$ 被替换为 $U$，$x$ 被替换为 $v$。

- **这给出了一个转换规则**，例如在系统的原始版本中。

---

### **11.4 自由结构的表示**

我们已经翻译了一些简单的类型；接下来，我们将继续一些**归纳类型（inductive types）**的表示：如整数、树、列表等。毫无疑问，这种可能性是无穷的，我们将给出这类问题的一般解决方案，然后专门讨论更具体的情况。

#### **11.4.1 自由结构**

假设 $\Theta$ 是由以下方式生成的形式表达式的集合：

- **一些原子（atoms）**：$c_1, c_2, \dots, c_k$，作为起点。

- **一些函数**，允许我们从已有的 $\Theta$ 项构造新的 $\Theta$ 项。

  - **最简单的情况**是从 $\Theta$ 到 $\Theta$ 的一元函数。

  - 我们还可以想象从多个参数（$\Theta, \Theta, \dots, \Theta$）到 $\Theta$ 的函数。

  - 这些函数具有类型 $\Theta \to \Theta \to \dots \to \Theta \to \Theta$。

  - 包括 0 元的情况（常量），我们有可能的 $n$ 个参数的函数，其中可能 $n = 0$。

**扩展的可能性**：

- $\Theta$ 可能在其构造中使用辅助类型，例如嵌入一个类型 $U$ 到 $\Theta$ 中，这将提供一个从 $U$ 到 $\Theta$ 的函数。

- 可能存在更复杂的情况。例如，考虑由类型 $U$ 的对象组成的列表。

  - 我们有一个常量（空列表），并且可以通过以下操作构造列表：如果 $u$ 是类型 $U$ 的对象，$t$ 是一个列表，那么 $cons \ u \ t$ 是一个列表。

  - 这里我们有一个从 $U$ 和 $\Theta$ 到 $\Theta$ 的函数。

- 更戏剧性的可能性：考虑具有分支类型 $U$ 的良基树（well-founded trees）。

  - 这种结构要么是一个叶子，要么由一个以 $U$ 为索引的树的族组成。

  - 因此，在这种情况下，我们必须考虑一个类型为 $(U \to \Theta) \to \Theta$ 的函数。

**一般情况**：

- 结构 $\Theta$ 将通过一组有限的函数（构造器）$f_1, f_2, \dots, f_n$ 描述，它们分别具有类型 $S_1, S_2, \dots, S_n$。

- 类型 $S_i$ 必须具有特定的形式：

  $$
  S_i = T^i_1 \to T^i_2 \to \dots \to T^i_{k_i} \to \Theta
  $$

  - 其中 $\Theta$ 只在 $T^i_j$ 中**正向地（positively）**出现（参见第5.2.3节）。

- 我们将隐含地要求 $\Theta$ 是由 $f_i$ 生成的**自由结构（free structure）**，即 $\Theta$ 的每个元素都可以唯一地通过一系列 $f_i$ 的应用来表示。

**构造类型 $T$**：

- 为此，我们用一个变量 $X$ 替换 $\Theta$（我们将继续写 $S_i$ 来表示 $S_i[X/\Theta]$），并引入：

  $$
  T = \Pi X. S_1 \to S_2 \to \dots \to S_n \to X
  $$

- 我们将看到 $T$ 有充分的理由来表示 $\Theta$。

#### **11.4.2 构造器的表示**

我们需要为每个类型 $S_i[T/X]$ 找到一个对象 $f_i$。

- 换句话说，我们正在寻找一个函数 $f_i$，它接受 $k_i$ 个类型为 $T^i_j[T/X]$ 的参数，返回一个类型为 $T$ 的值。

**构造方法**：

- 令 $x_1, x_2, \dots, x_{k_i}$ 为 $f_i$ 的参数。

- 由于 $X$ 在 $T^i_j$ 中正向出现，定义一个类型为 $T \to X$ 的**规范函数（canonical function）** $h_i$：

  $$
  h_i \ x = x \ X \ y^1_{S_1} \dots y^n_{S_n}
  $$

  - 其中 $X, y_1, \dots, y_n$ 是参数。

- 这诱导了一个从 $T^i_j[T/X]$ 到 $T^i_j$ 的函数 $T^i_j[h_i]$，依赖于 $X, y_1, \dots, y_n$。

- 最后，我们对每个 $j = 1, \dots, k_i$，定义：

  $$
  t_j = T^i_j[h_i] \ x_j
  $$

- 然后定义：

  $$
  f_i \ x_1 \dots x_{k_i} = \Lambda X. \lambda y^1_{S_1} \dots \lambda y^n_{S_n}. y_i \ t_1 \dots t_{k_i}
  $$

  - **解释**：$f_i$ 接受参数 $x_1, \dots, x_{k_i}$，然后通过全称抽象和 λ-抽象构造一个项，使用 $y_i$ 应用于 $t_1, \dots, t_{k_i}$。

#### **11.4.3 归纳（Induction）**

**问题**：

- 知道我们能够构造的类型为 $T$ 的对象是否确实是由 $f_i$ 生成的那些，这是一个困难的问题；答案是**几乎是的**。

- 我们将在第15.1.1节中回到这个问题。

**初步指示**：

- 这种事实的一个初步迹象是可以**基于 $\Theta$ 的构造来定义函数**的可能性。

**定义方法**：

- 我们从一个类型 $U$ 和函数 $g_1, g_2, \dots, g_n$ 开始，它们的类型为 $S_i[U/X]$（对于 $i = 1, \dots, n$）。

- 我们希望定义一个类型为 $T \to U$ 的函数 $h$，满足：

  $$
  h \ (f_i \ x_1 \dots x_{k_i}) = g_i \ u_1 \dots u_{k_i}
  $$
  
  其中，对于每个 $j = 1, \dots, k_i$，有：

  $$
  u_j = T^i_j[h] \ x_j
  $$

- 为此，我们定义：

  $$
  h \ x = x \ U \ g_1 \dots g_n
  $$

  - **解释**：$h$ 接受一个类型为 $T$ 的参数 $x$，将其应用于类型 $U$ 和函数 $g_1, \dots, g_n$。

- 上述等式显然满足。

**结论**：

- 这种归纳类型的表示法是受 **Martin-Löf** 在1970年的一份手稿启发的。

---

### **总结**

在本章中，我们介绍了 **系统 F**，它是通过添加对类型的抽象操作扩展简单类型演算而得。

- **系统 F 的主要特征**：

  - 添加了全称量化类型（$\Pi$ 类型），允许对类型进行抽象。

  - 通过五种项的构造形式，提供了强大的表达能力。

- **系统 F 的表达能力**：

  - 能够表示常用的简单类型，如布尔值、乘积类型、和类型、存在类型等。

  - 能够表示归纳类型，如整数、列表、树等。

- **统一性和语义考虑**：

  - 全称类型的对象必须是“统一的”，即在所有类型上执行相同的操作。

  - 系统 F 对非逻辑性质的修改极为敏感，需要谨慎处理。

- **归纳类型的表示**：

  - 提供了一种一般方法来表示自由结构，通过定义构造器和归纳函数，实现对复杂数据类型的表示。

---

### **后续展望**

- 在后续的章节中，我们将进一步探索系统 F 的语义学，以及如何使用一致空间和稳定函数来对其进行解释。

- 我们还将研究系统 F 中的多态性和类型系统的高级特性，以及它们在编程语言和逻辑中的应用。

---

如果您对本章的内容有任何疑问，或者需要进一步的解释和详细讲解，请随时提出来！

### ---------------------------

### **11.5 归纳类型的表示**

在 **11.3** 中给出的所有定义（除了存在类型外）都是 **11.4** 中描述的一般方案的特例；它们并非凭空而来。本节将详细说明这些类型如何符合之前讨论的自由结构的表示。

#### **1. 布尔类型**

- **构造器**：布尔类型有两个常量，分别对应于 $f_1$ 和 $f_2$，它们的类型都是布尔类型。

- **类型 $S_1$ 和 $S_2$**：因此，$S_1 = S_2 = X$。

- **布尔类型定义**：
  $$
  \text{Bool} = \Pi X. X \to X \to X
  $$
  
- **解释**：我们可以轻松地证明之前定义的 $T$ 和 $F$ 确实是 **11.4** 中定义的 0 元函数（没有参数的函数）。布尔类型的归纳操作实际上就是之前定义的 $D$。

#### **2. 乘积类型**

- **构造器**：乘积类型有一个函数 $f_1$，它有两个参数，一个类型为 $U$，另一个类型为 $V$。

- **类型 $S_1$**：因此，$S_1 = U \to V \to X$。

- **解释**：这解释了为什么乘积类型被翻译为之前的形式。

- **配对函数**：配对函数（pairing function）很好地符合 **11.4** 中一般情况，但两个投影函数（$\pi_1$ 和 $\pi_2$）在此处理之外。

  - **原因**：它们实际上比机械地应用 **11.4** 得到的间接方案更容易处理。

#### **3. 和类型**

- **构造器**：和类型有两个函数（**规范注入**，即左注入和右注入）。

- **类型 $S_1$ 和 $S_2$**：因此，$S_1 = U \to X$，$S_2 = V \to X$。

- **解释**：**11.3.4** 中的解释与一般方案完全一致。

#### **4. 空类型**

- **构造器**：空类型没有任何构造器，因此 $n = 0$。

- **归纳函数**：函数 $\varepsilon_U$ 确实是它的归纳操作符。

---

现在让我们转向一些更复杂的例子。

### **11.5.1 整数**

- **构造器**：

  1. **零（$O$）**：类型为整数（$\text{Int}$）。
  
  2. **后继函数（$S$）**：从整数到整数的函数。

- **类型 $S_1$ 和 $S_2$**：

  - $S_1 = X$
  
  - $S_2 = X \to X$

- **整数类型定义**：
  $$
  \text{Int} \overset{\text{def}}{=} \Pi X. X \to (X \to X) \to X
  $$
  
- **表示整数 $n$**：

  $$
  n = \Lambda X. \lambda x^X. \lambda y^{X \to X}. y (y (y \dots (y \ x) \dots))
  $$
  
  - **解释**：这里有 $n$ 次 $y$ 的嵌套应用，即 $y$ 被迭代应用 $n$ 次。

- **变体表示**：

  - 通过交换 $S_1$ 和 $S_2$，可以将整数类型表示为：
    $$
    \Pi X. (X \to X) \to (X \to X)
    $$
    
  - **解释**：这实际上与之前的表示基本相同。

- **立即的解释**：

  - 在这种情况下，整数 $n$ 的解释是：对于任意类型 $U$ 和类型为 $U \to U$ 的函数 $f$，$n$ 对应于将 $f$ 迭代 $n$ 次的函数，即 $f^n$。

#### **基本函数的定义**

1. **零（$O$）**：
   $$
   O \overset{\text{def}}{=} \Lambda X. \lambda x^X. \lambda y^{X \to X}. x
   $$
   - **解释**：$O$ 对应于恒等于 $x$ 的函数，不应用 $y$。

2. **后继函数（$S$）**：
   $$
   S \ t \overset{\text{def}}{=} \Lambda X. \lambda x^X. \lambda y^{X \to X}. y (t \ X \ x \ y)
   $$
   - **解释**：$S$ 接受一个整数 $t$，并返回一个新函数，其中在 $t$ 的基础上再应用一次 $y$。

- **对应关系**：

  - $O$ 表示数字 0。

  - $S \ n$ 表示 $n+1$。

#### **归纳操作符**

- **迭代器（It）**：

  - **定义**：$\text{It}$ 是一个迭代器，接受一个类型为 $U$ 的对象 $u$，一个类型为 $U \to U$ 的函数 $f$，并返回一个类型为 $U$ 的结果。

  - **定义形式**：
    $$
    \text{It} \ u \ f \ t = t \ U \ u \ f
    $$

- **计算 $\text{It}$**

  1. **计算 $\text{It} \ u \ f \ O$**：
     $$
     \begin{align*}
     \text{It} \ u \ f \ O &= (\Lambda X. \lambda x^X. \lambda y^{X \to X}. x) \ U \ u \ f \\
     &= (\lambda x^U. \lambda y^{U \to U}. x) \ u \ f \\
     &= (\lambda y^{U \to U}. u) \ f \\
     &= u
     \end{align*}
     $$
     - **解释**：对于零，迭代器返回初始值 $u$。

  2. **计算 $\text{It} \ u \ f \ (S \ t)$**：
     $$
     \begin{align*}
     \text{It} \ u \ f \ (S \ t) &= (\Lambda X. \lambda x^X. \lambda y^{X \to X}. y (t \ X \ x \ y)) \ U \ u \ f \\
     &= (\lambda x^U. \lambda y^{U \to U}. y (t \ U \ x \ y)) \ u \ f \\
     &= (\lambda y^{U \to U}. y (t \ U \ u \ y)) \ f \\
     &= f \ (t \ U \ u \ f) \\
     &= f \ (\text{It} \ u \ f \ t)
     \end{align*}
     $$
     - **解释**：对于后继，迭代器返回 $f$ 应用于前一次迭代的结果。

- **等价性**

  - **注意**：$\text{It} \ u \ f \ (n+1)$ 并不等于 $f (\text{It} \ u \ f \ n)$，但两者都化简为 $f^{n+1} \ u$。

  - **符号 $\sim$**：表示“可化简到相同的项”。

  - **结论**：至少有 $\text{It} \ u \ f \ (n+1) \sim f (\text{It} \ u \ f \ n)$。

- **Church-Rosser 性质**

  - **解释**：由于“化简”（$\to$）满足 **Church-Rosser** 性质，因此两个项等价当且仅当它们可以化简到一个共同的项。

#### **递归的定义**

- **目标**：展示如何用迭代定义递归。

- **设定**

  - $u$ 是类型为 $U$ 的项。

  - $f$ 是类型为 $U \to \text{Int} \to U$ 的函数。

- **构造 $g$**

  - **定义**：
    $$
    g = \lambda x^{U \times \text{Int}}. \langle f (\pi_1 \ x) (\pi_2 \ x), S (\pi_2 \ x) \rangle
    $$
    - **解释**：$g$ 接受一个元组 $x$，返回一个新元组，其中第一个元素是 $f$ 应用于 $x$ 的第一个和第二个分量，第二个元素是 $\pi_2 \ x$ 的后继。

- **性质**

  - 特别地，$g \ \langle u, n \rangle = \langle f \ u \ n, n+1 \rangle$。

- **迭代 $g$**

  - 如果 $\text{It} \ \langle u, 0 \rangle \ g \ n \sim \langle t_n, n \rangle$，那么：
    $$
    \begin{align*}
    \text{It} \ \langle u, 0 \rangle \ g \ (n+1) &\sim g (\text{It} \ \langle u, 0 \rangle \ g \ n) \\
    &\sim g \ \langle t_n, n \rangle \\
    &\sim \langle f \ t_n \ n, n+1 \rangle
    \end{align*}
    $$

- **定义递归函数 $R$**

  - **定义**：
    $$
    R \ u \ f \ t \overset{\text{def}}{=} \pi_1 (\text{It} \ \langle u, 0 \rangle \ g \ t)
    $$

- **性质**

  1. **初始值**：
     $$
     R \ u \ f \ 0 \sim u
     $$
  
  2. **递推关系**：
     $$
     R \ u \ f \ (n+1) \sim f (R \ u \ f \ n) \ n
     $$
     - **解释**：递归公式在数值上成立，即对于每个具体的 $n$。

- **系统 F 的缺陷**

  - **问题**：递归的第二个等式仅在具体值上成立，而不是在一般情况下。

  - **例如**：如果我们编写前驱函数（predecessor function）：
    $$
    \begin{cases}
    \text{pred} \ O = O \\
    \text{pred} \ (S \ x) = x
    \end{cases}
    $$
    - 第二个等式仅在 $x$ 形如 $n$ 时成立，即程序必须完全将参数 $x$ 分解为 $S \ S \ S \ \dots \ S \ O$ 的形式，然后重构它，省略最后一个 $S$。

  - **优化**：当然，更经济的方法是移除第一个 $S$，而不是最后一个。

---

### **11.5.2 列表**

- **目标**：给定一个类型 $U$，我们希望构造类型 $\text{List} \ U$，其对象是类型为 $U$ 的有限序列 $(u_1, u_2, \dots, u_n)$。

- **构造器**：

  1. **空序列**：类型为 $\text{List} \ U$，因此 $S_1 = X$。

  2. **构造函数**：将类型为 $U$ 的对象 $u$ 和序列 $(u_1, \dots, u_n)$ 映射为 $(u, u_1, \dots, u_n)$。因此，$S_2 = U \to X \to X$。

- **应用一般方案**

  - **列表类型定义**：
    $$
    \text{List} \ U \overset{\text{def}}{=} \Pi X. X \to (U \to X \to X) \to X
    $$
  
  - **空列表（nil）**：
    $$
    \text{nil} \overset{\text{def}}{=} \Lambda X. \lambda x^X. \lambda y^{U \to X \to X}. x
    $$
  
  - **构造（cons）**：
    $$
    \text{cons} \ u \ t \overset{\text{def}}{=} \Lambda X. \lambda x^X. \lambda y^{U \to X \to X}. y \ u \ (t \ X \ x \ y)
    $$

- **表示序列**

  - 序列 $(u_1, u_2, \dots, u_n)$ 表示为：
    $$
    \Lambda X. \lambda x^X. \lambda y^{U \to X \to X}. y \ u_1 \ (y \ u_2 \ (\dots (y \ u_n \ x) \dots))
    $$
  
  - **解释**：将 $y$ 替换为 $\text{cons}$，$x$ 替换为 $\text{nil}$，可识别为：
    $$
    \text{cons} \ u_1 \ (\text{cons} \ u_2 \ (\dots (\text{cons} \ u_n \ \text{nil}) \dots))
    $$
  
  - **进一步说明**：这个最后的项可以通过化简 $(u_1, \dots, u_n) \ (\text{List} \ U) \ \text{nil} \ \text{cons}$ 得到。

#### **列表的行为**

- **与整数的相似性**：列表的行为与整数非常相似。

- **列表上的迭代**：

  - **定义**：如果 $W$ 是一个类型，$w$ 是类型为 $W$ 的项，$f$ 是类型为 $U \to W \to W$ 的函数，对于类型为 $\text{List} \ U$ 的 $t$，可以定义：
    $$
    \text{It} \ w \ f \ t \overset{\text{def}}{=} t \ W \ w \ f
    $$
  
  - **性质**：

    1. **空列表**：
       $$
       \text{It} \ w \ f \ \text{nil} = w
       $$
    
    2. **非空列表**：
       $$
       \text{It} \ w \ f \ (\text{cons} \ u \ t) = f \ u \ (\text{It} \ w \ f \ t)
       $$

#### **示例**

1. **恒等迭代**：

   - 定义 $\text{It} \ \text{nil} \ \text{const} \ t = \text{nil}$，对于所有形式为 $(u_1, \dots, u_n)$ 的 $t$。

2. **映射函数**：

   - 设 $W = \text{List} \ V$，其中 $V$ 是另一个类型，$f = \lambda x^U. \lambda y^{\text{List} \ V}. \text{cons} (g \ x) \ y$，其中 $g$ 是类型为 $U \to V$ 的函数。

   - 则有：
     $$
     \text{It} \ \text{nil} \ f \ (u_1, \dots, u_n) = (g \ u_1, \dots, g \ u_n)
     $$
     - **解释**：这实际上是列表的映射操作，将 $g$ 应用于列表的每个元素。

#### **递归操作符**

- **通过值的递归操作符 $R$**：

  - **定义**：

    1. **空列表**：
       $$
       R \ v \ f \ \text{nil} \sim v
       $$
    
    2. **非空列表**：
       $$
       R \ v \ f \ (u_1, \dots, u_n) \sim f \ u_1 \ (u_2, \dots, u_n) \ (R \ v \ f \ (u_2, \dots, u_n))
       $$
  
  - **其中**：
    
    - $v$ 是类型为 $V$ 的项。
    
    - $f$ 是类型为 $U \to \text{List} \ U \to V \to V$ 的函数。

- **应用**：

  - 这使我们能够定义，例如，通过移除列表的第一个元素来截断列表的函数，类似于前驱函数。

  - **定义**：
    $$
    \begin{cases}
    \text{tail} \ \text{nil} = \text{nil} \\
    \text{tail} \ (\text{cons} \ u \ t) = t
    \end{cases}
    $$
  
  - **注意**：第二个等式仅在 $t$ 形如 $(u_1, \dots, u_n)$ 时成立。

#### **练习**

- **通过迭代定义以下函数**：

  1. **连接（concatenation）**：
     $$
     (u_1, \dots, u_n) \ @ \ (v_1, \dots, v_m) = (u_1, \dots, u_n, v_1, \dots, v_m)
     $$
     - **提示**：使用列表的迭代，将第二个列表追加到第一个列表的末尾。

  2. **反转（reversal）**：
     $$
     \text{reverse} \ (u_1, \dots, u_n) = (u_n, \dots, u_1)
     $$
     - **提示**：在迭代过程中，将元素插入到累积结果的前面。

#### **抽象**

- **列表类型的统一性**

  - **观察**：$\text{List} \ U$ 依赖于 $U$，但我们给出的定义实际上在 $U$ 上是统一的。

  - **因此，可以定义**：

    - $\text{Nil} = \Lambda X. \text{nil}[X]$，类型为 $\Pi X. \text{List} \ X$。

    - $\text{Cons} = \Lambda X. \text{cons}[X]$，类型为 $\Pi X. X \to \text{List} \ X \to \text{List} \ X$。

---

### **11.5.3 二叉树**

- **目标**：我们关注有限的二叉树。

- **构造器**：

  1. **空树（只有根节点）**：$S_1 = X$。

  2. **由两个树构造新树**：$S_2 = X \to X \to X$。

- **二叉树类型定义**：
  $$
  \text{Bintree} \overset{\text{def}}{=} \Pi X. X \to (X \to X \to X) \to X
  $$
  
- **空树（nil）**：
  $$
  \text{nil} \overset{\text{def}}{=} \Lambda X. \lambda x^X. \lambda y^{X \to X \to X}. x
  $$
  
- **构造（couple）**：
  $$
  \text{couple} \ u \ v \overset{\text{def}}{=} \Lambda X. \lambda x^X. \lambda y^{X \to X \to X}. y \ (u \ X \ x \ y) \ (v \ X \ x \ y)
  $$

#### **树上的迭代**

- **定义**：对于类型为 $\text{Bintree}$ 的 $t$，迭代定义为：
  $$
  \text{It} \ w \ f \ t \overset{\text{def}}{=} t \ W \ w \ f
  $$
  - **其中**：

    - $W$ 是一个类型。

    - $w$ 是类型为 $W$ 的项。

    - $f$ 是类型为 $W \to W \to W$ 的函数。

- **性质**：

  1. **空树**：
     $$
     \text{It} \ w \ f \ \text{nil} = w
     $$
  
  2. **非空树**：
     $$
     \text{It} \ w \ f \ (\text{couple} \ u \ v) = f \ (\text{It} \ w \ f \ u) \ (\text{It} \ w \ f \ v)
     $$

---

### **11.5.4 具有分支类型 $U$ 的树**

- **目标**：构造具有任意分支类型 $U$ 的树。

- **构造器**：

  1. **空树（只有根节点）**：$S_1 = X$。

  2. **从一族树构造新树**：$S_2 = (U \to X) \to X$。

- **树类型定义**：
  $$
  \text{Tree} \ U \overset{\text{def}}{=} \Pi X. X \to ((U \to X) \to X) \to X
  $$
  
- **空树（nil）**：
  $$
  \text{nil} \overset{\text{def}}{=} \Lambda X. \lambda x^X. \lambda y^{(U \to X) \to X}. x
  $$
  
- **收集（collect）**：
  $$
  \text{collect} \ f \overset{\text{def}}{=} \Lambda X. \lambda x^X. \lambda y^{(U \to X) \to X}. y \ (\lambda z^U. f \ z \ X \ x \ y)
  $$
  - **解释**：构造一棵树，其子树由函数 $f$ 生成，$f$ 是从 $U$ 到树的函数。

#### **迭代（可能是超限的）**

- **定义**：对于类型为 $\text{Tree} \ U$ 的 $t$，迭代定义为：
  $$
  \text{It} \ w \ h \ t \overset{\text{def}}{=} t \ W \ w \ h
  $$
  - **其中**：

    - $W$ 是一个类型。

    - $w$ 是类型为 $W$ 的项。

    - $h$ 是类型为 $(U \to W) \to W$ 的函数。

- **性质**：

  1. **空树**：
     $$
     \text{It} \ w \ h \ \text{nil} = w
     $$
  
  2. **非空树**：
     $$
     \text{It} \ w \ h \ (\text{collect} \ f) = h \ (\lambda x^U. \text{It} \ w \ h \ (f \ x))
     $$
     - **解释**：对每个子树递归应用迭代函数。

#### **注意**

- **二叉树的特殊情况**：二叉树可以被视为具有布尔分支类型的树，基本上不需要实质性的修改。

#### **抽象和模块化**

- **抽象化**：正如我们可以在 $\text{List} \ U$ 中对 $U$ 进行抽象，在树的情况下也可以这样做。

- **模块化的体现**：这种抽象的潜力很好地展示了系统 F 的模块化。例如，我们可以定义模块：
  $$
  \text{Collect} = \Lambda X. \text{collect}[X]
  $$
  - **应用**：随后可以通过指定类型 $X$ 来使用它。

- **价值**：在更复杂的情况下，我们看到这样做的价值：我们只需要编写一次程序，但可以在各种情况下应用（插入到其他模块中）。

---

### **11.6 Curry-Howard 对应（同构）**

- **观察**：系统 F 中的类型正是第二阶（量化到类型上的）命题，我们已经为箭头类型建立的同构扩展到了这些量化符号。

#### **全称引入规则（Universal Introduction）**

- **形式化**：
  $$
  \frac{\vdots \quad A}{\forall X. A} \ (\forall_2 \text{I})
  $$
  - **解释**：如果我们可以证明 $A$，那么可以得出 $\forall X. A$。

#### **全称消除规则（Universal Elimination）**

- **形式化**：
  $$
  \frac{\vdots \quad \forall X. A}{A[B/X]} \ (\forall_2 \text{E})
  $$
  - **解释**：如果我们有 $\forall X. A$，那么对于任意类型 $B$，可以得到 $A[B/X]$。

- **对应关系**：

  - 这些规则正好对应于全称抽象（$\Lambda X. t$）和全称应用（$t \ U$）。

- **推导的表示**：

  - 如果类型为 $A$ 的项 $t$ 表示 $\forall_2 \text{I}$ 之上的部分推导，那么 $\Lambda X. t$ 表示整个推导。

- **变量限制**：

  - 自然演绎中对变量的通常限制（$X$ 不在假设中自由出现）正好对应于全称抽象的构造限制。

- **全称消除的精确性**：

  - 类似地，$\forall_2 \text{E}$ 对应于对类型 $B$ 的应用。

  - **精确说明**：在 $X$ 不出现在 $A$ 中的情况下，应该指定替换了什么类型 $B$。

#### **转换规则**

- **形式化**：
  $$
  (\Lambda X. v) \ U \quad \to \quad v[U/X]
  $$
  - **解释**：这正是我们在自然演绎中所需要的转换。

- **对应的推导转换**：

  - **原始推导**：
    $$
    \frac{\vdots \quad A \quad \forall_2 \text{I} \quad \forall X. A \quad \forall_2 \text{E} \quad A[B/X]}{}
    $$
  
  - **转换为**：
    $$
    \vdots \quad A[B/X]
    $$

- **解释**：这表明在逻辑推导中，全称量化的引入和随后的消除可以直接转换为对具体类型的实例化。

---

### **总结**

在本节中，我们详细探讨了系统 F 中归纳类型的表示方法，以及它们如何符合之前讨论的自由结构的通用方案。通过具体的示例，如整数、列表和树，我们展示了如何使用系统 F 强大的类型系统来表示复杂的数据结构。

- **归纳类型的表示**：

  - **布尔类型、乘积类型、和类型、空类型**都可以被视为自由结构的特例。

  - **整数**：通过定义零和后继函数，整数类型被表示为一种归纳类型，可以实现迭代和递归。

  - **列表**：列表类型被构造为归纳类型，支持迭代和递归操作，能够定义诸如映射、连接和反转等函数。

  - **树**：二叉树和具有任意分支类型的树都可以通过自由结构的方案来表示，支持迭代和递归。

- **Curry-Howard 对应**：

  - 系统 F 中的类型与二阶逻辑中的命题对应，类型系统与自然演绎中的逻辑规则密切相关。

  - 全称量化的引入和消除在类型系统中对应于全称抽象和应用。

  - 转换规则体现了逻辑推导与类型操作之间的深刻联系。

---

### **后续展望**

- **深入理解**：通过对系统 F 的深入研究，我们可以更好地理解类型系统与逻辑之间的关系，以及它们在编程语言中的应用。

- **扩展应用**：系统 F 的强大表达能力为我们提供了构建复杂类型和模块化程序的工具，促进了泛型编程和类型安全性的提高。

- **进一步研究**：可以探索系统 F 的高级特性，如多态性、类型推理，以及它在证明助理和函数式编程语言中的应用。

---

如果您对本章的内容有任何疑问，或者需要进一步的解释和详细讲解，请随时提出来！

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