[toc]



# 第10章 递归技术

本章提供了一些用于证明连续函数的最小不动点性质的技术。我们将介绍如何利用最小不动点作为最小前不动点来进行证明，这种方法有时被称为**Park 归纳法**。我们将利用它来证明一个重要的结果，即 **Bekić 定理**，它提供了在 cpo（完备偏序集）的乘积中获得最小不动点的不同方法。

此外，我们将介绍 Scott 的不动点归纳法，以及它所依赖的 **包容性质（inclusive property）** 的概念；并提供构造包容性质的方法。然后，我们将给出一些关于良基归纳法（well-founded induction）的例子，扩展我们之前的工作，特别是展示如何构建良基关系。我们还将介绍一种称为 **良基递归（well-founded recursion）** 的通用方法，用于在具有良基关系的集合上定义函数。

本章的最后，我们将通过一个小但非平凡的练习，使用多种技术来证明两个列表上的递归函数的相等性。

## 10.1 Bekić 定理

第5章的 **不动点定理（定理5.11）** 告诉我们，如果 $D$ 是一个带有底元素 $\bot$ 的 cpo，且 $F : D \rightarrow D$ 是一个连续函数，那么 $ \text{fix}(F) $ 是 $F$ 的最小前不动点，即：

$$
F(d) \sqsubseteq d \implies \text{fix}(F) \sqsubseteq d
$$

对于任意 $d \in D$。

当然，$\text{fix}(F)$ 是 $F$ 的不动点，即：

- **(fix1)**：$F(\text{fix}(F)) = \text{fix}(F)$

- **(fix2)**：$F(\text{fix}(F)) \sqsubseteq \text{fix}(F)$

这些事实（(fix1) 和 (fix2)）刻画了 $\text{fix}(F)$，并且在一般情况下，对于证明不动点的性质非常有用。事实 (fix1) 表示了一种证明原则，有时被称为 **Park 归纳法**，以 David Park 命名。

我们将使用 (fix1) 和 (fix2) 来建立一个有趣的结果，即 **Bekić 定理**。基本上，Bekić 定理说明了如何将一个同时的递归定义替换为一次一个坐标的递归定义。

### 定理10.1（Bekić）

**定理**：

设 $D$ 和 $E$ 是带有底元素的 cpo，$F : D \times E \rightarrow D$ 和 $G : D \times E \rightarrow E$ 是连续函数。$(F, G) : D \times E \rightarrow D \times E$ 的最小不动点是具有以下坐标的对 $(j, g)$：

$$
\begin{align*}
j &= \mu f. \ F(f, \ \mu g. \ G(f, g)) \\
g &= \mu g. \ G\left( \mu f. \ F(f, g), \ g \right)
\end{align*}
$$

**证明**：

我们首先证明 $(j, g)$ 是 $(F, G)$ 的不动点。

1. **定义 $j$**：

   $$
   j = \mu f. \ F(f, g)
   $$

   换句话说，$j$ 是函数 $\lambda f. \ F(f, g)$ 的最小不动点。因此，$j = F(j, g)$。

2. **定义 $g$**：

   $$
   g = \mu g. \ G(j, g)
   $$

   因此，$g = G(j, g)$。

   **符号说明**：

   - $\mu f$ 表示函数 $f$ 的最小不动点。
   - $\lambda f$ 表示一个函数，参数为 $f$。

3. **因此**，$(j, g)$ 是 $(F, G)$ 的不动点，因为：

   $$
   (j, g) = (F(j, g), \ G(j, g)) = (F, G)(j, g)
   $$

4. **设 $(f_0, g_0)$ 是 $(F, G)$ 的最小不动点**，因此：

   $$
   (f_0, g_0) \sqsubseteq (j, g)
   $$

   也就是说：

   - $f_0 \sqsubseteq j$
   - $g_0 \sqsubseteq g$

5. **需要证明反向的不等式**，即 $j \sqsubseteq f_0$ 和 $g \sqsubseteq g_0$。

   由于 $f_0 = F(f_0, g_0)$，因此：

   $$
   \mu f. \ F(f, g_0) \sqsubseteq f_0
   $$

   **解释**：

   - 因为 $F$ 是单调的，$\mu f. \ F(f, g_0)$ 是函数 $\lambda f. \ F(f, g_0)$ 的最小不动点，因此 $\mu f. \ F(f, g_0) \sqsubseteq f_0$。

6. **由于 $G$ 的单调性**，有：

   $$
   G\left( \mu f. \ F(f, g_0), \ g_0 \right) \sqsubseteq G(f_0, g_0) = g_0
   $$

   因此：

   $$
   g \sqsubseteq g_0
   $$

   因为 $g$ 是 $\lambda g. \ G(\mu f. \ F(f, g), g)$ 的最小不动点。

7. **利用 $F$ 的单调性**，有：

   $$
   F(f_0, g) \sqsubseteq F(f_0, g_0) = f_0
   $$

   因此：

   $$
   j \sqsubseteq f_0
   $$

   因为 $j$ 是 $\lambda f. \ F(f, g)$ 的最小不动点。

8. **结合以上结果**（步骤 4、5、6、7），我们有：

   $$
   (j, g) = (f_0, g_0)
   $$

   **因此**，$(j, g)$ 是 $(F, G)$ 的最小不动点。

$\blacksquare$

---

**解释**：

- **目的**：证明 $(F, G)$ 的最小不动点可以通过分别求解 $j$ 和 $g$ 来得到，其中 $j$ 依赖于 $g$，而 $g$ 也依赖于 $j$。

- **关键步骤**：

  - 证明 $(j, g)$ 是 $(F, G)$ 的不动点。
  - 证明 $(j, g)$ 是最小的不动点。

- **利用了哪些性质**：

  - 连续函数的单调性。
  - 最小不动点的定义和性质。

---

**Bekić 定理的对称形式**：

我们可以推导出一个对称形式的结论：同时最小不动点 $(j, g)$ 满足：

$$
\begin{align*}
j &= \mu f. \ F(f, \ \mu g. \ G(f, g)) \\
g &= \mu g. \ G\left( \mu f. \ F(f, g), \ g \right)
\end{align*}
$$

这个形式直接体现了 $j$ 和 $g$ 之间的对称性。

---

### 例子

参考第9.8节，我们讨论了如何扩展 REC 语言以允许局部声明。考虑以下项：

$$
T = \text{let rec } B = (\text{let rec } A = t \ \text{in } u) \ \text{in } (\text{let rec } A = t \ \text{in } v)
$$

其中，$A$ 和 $B$ 被假设为不同的、元数为 $0$ 的函数变量。设 $p$ 和 $φ$ 为任意的变量环境和函数变量环境。

我们将定义：

$$
\begin{align*}
F(f, g) &= [t]_{φ}[f / A, \ g / B] \ p \\
G(f, g) &= [u]_{φ}[f / A, \ g / B] \ p
\end{align*}
$$

根据语义，我们有：

$$
\begin{align*}
g &= \mu g. \ [\text{let rec } A = t \ \text{in } u]_{φ}[g / B] \ p \\
&= \mu g. \ [u]_{φ}[g / B, \ \mu f. \ [t]_{φ}[f / A, \ g / B] \ p \ / A] \ p \\
&= \mu g. \ G(\mu f. \ F(f, g), \ g)
\end{align*}
$$

同样地，

$$
j = \mu f. \ [t]_{φ}[f / A, \ g / B] \ p = \mu f. \ F(f, g)
$$

根据 Bekić 定理，这意味着 $(j, g)$ 是 $(F, G)$ 的（同时）最小不动点。因此，我们可以通过同时声明来达到相同的效果：

$$
[T] = [\text{let rec } A = t \ \text{and } B = u \ \text{in } v]
$$

---

**重要性**：

- Bekić 定理对于建立涉及同时声明的项之间的程序等价性至关重要。
- 它允许我们将同时递归定义分解为一次一个变量的递归定义，这在证明和实现中都非常有用。

---

### 练习10.2

**问题**：

**推广并陈述 Bekić 定理对于 3 个方程的情况。**

**解答**：

让我们将定理推广到三个连续函数 $F : D \times E \times F \rightarrow D$，$G : D \times E \times F \rightarrow E$，$H : D \times E \times F \rightarrow F$，其中 $D, E, F$ 是带有底元素的 cpo。

**定理（Bekić 定理的推广）**：

$(F, G, H) : D \times E \times F \rightarrow D \times E \times F$ 的最小不动点是 $(j, g, h)$，其中：

$$
\begin{align*}
j &= \mu f. \ F(f, \ \mu g. \ G(f, g, \ \mu h. \ H(f, g, h)), \ \mu h. \ H(f, g, h)) \\
g &= \mu g. \ G\left( \mu f. \ F(f, g, \ \mu h. \ H(f, g, h)), \ g, \ \mu h. \ H(f, g, h) \right) \\
h &= \mu h. \ H\left( \mu f. \ F(f, \ \mu g. \ G(f, g, h), h), \ \mu g. \ G(f, g, h), \ h \right)
\end{align*}
$$

**解释**：

- 我们逐步地对每个变量求最小不动点，每一步都将前一步的结果代入下一步的定义中。
- 这种方法允许我们将同时递归定义拆分为嵌套的单变量递归定义。

---

### 练习10.3

**问题**：

设 $D$ 和 $E$ 是带有底元素的 cpo。证明如果 $f : D \rightarrow E$ 和 $g : E \rightarrow D$ 是连续函数，那么：

$$
\text{fix}(g \circ f) = g(\text{fix}(f \circ g))
$$

**提示**：使用 (fix1) 和 (fix2)。

**解答**：

**证明**：

1. **设 $h = f \circ g$**，那么 $h : E \rightarrow E$。

2. **考虑 $f \circ g \circ f \circ g$**，我们有：

   $$
   (g \circ f) (g \circ f) = g(f(g(f(\cdot))))
   $$

3. **利用不动点性质**：

   - $\text{fix}(g \circ f)$ 是 $g \circ f$ 的不动点，即：

     $$
     (g \circ f)(\text{fix}(g \circ f)) = \text{fix}(g \circ f)
     $$

   - 同样地，$\text{fix}(f \circ g)$ 是 $f \circ g$ 的不动点，即：

     $$
     (f \circ g)(\text{fix}(f \circ g)) = \text{fix}(f \circ g)
     $$

4. **证明 $\text{fix}(g \circ f) = g(\text{fix}(f \circ g))$**：

   - 我们知道：

     $$
     g(f(\text{fix}(g \circ f))) = \text{fix}(g \circ f)
     $$

   - 令 $e = \text{fix}(g \circ f)$，则：

     $$
     g(f(e)) = e
     $$

   - 现在考虑 $d = f(e)$，则 $g(d) = e$。

   - 由于 $f$ 和 $g$ 是连续的，我们有：

     $$
     f(g(d)) = f(e) = d
     $$

   - 因此，$d$ 是 $f \circ g$ 的不动点，即：

     $$
     (f \circ g)(d) = d
     $$

   - 因此，$d = \text{fix}(f \circ g)$。

   - 因此，$e = g(d) = g(\text{fix}(f \circ g))$。

5. **结论**：

   $$
   \text{fix}(g \circ f) = g(\text{fix}(f \circ g))
   $$

$\blacksquare$

---

## 10.2 不动点归纳法

在很多情况下，可以通过数学归纳法，证明某个性质对每个逼近都成立，从而证明它对最小不动点成立。例如，在第5章中，我们在证明操作语义和指称语义的等价性时，证明了对于所有状态 $\sigma, \sigma'$，如果 $(\sigma, \sigma') \in [\![c]\!]$，那么 $c, \sigma \rightarrow \sigma'$。

在这种情况下，很明显，对最小不动点的所有逼近都成立的性质，意味着它对其并集，即不动点本身也成立。但对于任意的性质，这种情况并不总是成立。

**不动点归纳法**，由 Dana Scott 提出，是一个证明原则，它特别适用于证明连续函数的最小不动点的性质。它本质上取代了沿着逼近 $F^n(\bot)$ 的数学归纳法，但它的表述方式避免了对整数的推理。它只适用于 **包容性质（inclusive property）**，包容性质的特点是：对最小不动点的所有逼近都成立的性质，保证它对不动点本身也成立。

### 定义：包容性质

设 $D$ 是一个 cpo。如果对于 $D$ 中的所有 $\omega$-链 $d_0 \sqsubseteq d_1 \sqsubseteq \dots \sqsubseteq d_n \sqsubseteq \dots$，如果对所有 $n \in \omega$，都有 $d_n \in P$，则 $ \bigvee_{n \in \omega} d_n \in P$，那么称子集 $P \subseteq D$ 是 **包容的（inclusive）**。

**包容性质的意义**：

包容性质的意义在于 **不动点归纳法** 这一证明原则。它由以下命题给出：

### 命题10.4（不动点归纳法 - Scott）

**命题**：

设 $D$ 是一个带有底元素 $\bot$ 的 cpo，$F : D \rightarrow D$ 是连续函数。设 $P$ 是 $D$ 的一个包容子集。如果：

1. $\bot \in P$，且
2. 对所有 $x \in D$，若 $x \in P$，则 $F(x) \in P$，

那么 $\text{fix}(F) \in P$。

**证明**：

- 我们有 $\text{fix}(F) = \bigvee_{n \in \omega} F^n(\bot)$。
- 如果 $P$ 是包容的，且满足上述条件，那么：
  - $\bot \in P$，因此 $F(\bot) \in P$。
  - 通过归纳，$F^n(\bot) \in P$ 对所有 $n$ 成立。
- 因此，$\{F^n(\bot)\}$ 形成一个 $\omega$-链，且对所有 $n$，$F^n(\bot) \in P$。
- 由于 $P$ 是包容的，故其上确界 $\bigvee_{n \in \omega} F^n(\bot) \in P$。
- 因此，$\text{fix}(F) \in P$。

$\blacksquare$

---

**解释**：

- **目的**：证明最小不动点 $\text{fix}(F)$ 属于包容子集 $P$。
- **关键步骤**：
  - 证明所有的逼近 $F^n(\bot)$ 都属于 $P$。
  - 利用 $P$ 的包容性，得出 $\text{fix}(F) \in P$。

---

### 练习10.5

**问题**：

$n$ 是由下列元素组成的 cpo：

$$
\begin{array}{c}
\bot \\
| \\
0 \\
| \\
1 \\
| \\
2 \\
| \\
\vdots \\
\end{array}
$$

$n$ 的包容子集是什么？

**解答**：

- 由于 $n$ 是一个 $\omega$-链，加上底元素 $\bot$。
- $n$ 的任何上闭（上集）都是包容的。
- 特别地，$n$ 的包容子集可以是：
  - 整个集合 $n$。
  - 只包含 $\bot$。
  - 包含 $\bot$ 及其以上某个元素 $k$ 及所有更大的元素。

---

### 练习10.6

**问题**：

一个 cpo 的 **Scott 闭集（Scott-closed subset）** 是其 **Scott 开集（Scott-open subset）** 的补集（参考练习8.4）。证明一个 Scott 闭集是包容的。举出一个 cpo 的包容子集，但不是 Scott 闭集。

**解答**：

1. **证明 Scott 闭集是包容的**：

   - Scott 开集的定义是：对于 cpo $D$，$U \subseteq D$ 是 Scott 开集，当且仅当：
     - $U$ 是上闭的，即 $x \in U$ 且 $x \sqsubseteq y$，则 $y \in U$。
     - $U$ 中的任意有向集合的上确界仍在 $U$ 中。
   - Scott 闭集是 Scott 开集的补集，因此：
     - Scott 闭集是下闭的，即 $y \in C$ 且 $x \sqsubseteq y$，则 $x \in C$。
     - 任意有向集合的上确界如果在 $C$ 外，则该有向集合的元素必定有一个不在 $C$ 中。
   - 对于一个 $\omega$-链，如果它的所有元素都在 Scott 闭集 $C$ 中，那么它的上确界也在 $C$ 中。
   - 因此，Scott 闭集是包容的。

2. **举出包容子集但不是 Scott 闭集**：

   - 考虑 cpo $D$ 为自然数序列 $0 \sqsubseteq 1 \sqsubseteq 2 \sqsubseteq \dots$，加上底元素 $\bot$。
   - 定义子集 $P = \{\bot, 2, 3, 4, \dots\}$。
   - $P$ 是包容的，因为任何 $\omega$-链的元素都在 $P$ 中，则其上确界也在 $P$ 中。
   - 但是 $P$ 不是下闭的，因为 $2 \in P$，但 $1 \sqsubseteq 2$，$1 \notin P$。

---

### 包容性质的构造方法

**基本关系**：

- **顺序关系**：$\{ (x, y) \in D \times D \mid x \sqsubseteq y \}$ 是 $D \times D$ 的包容子集。
- **相等关系**：$\{ (x, y) \in D \times D \mid x = y \}$ 是 $D \times D$ 的包容子集。

因此，谓词 $x \sqsubseteq y$ 和 $x = y$ 是包容的。

**逆像和替换**：

- 若 $f : D \rightarrow E$ 是 cpo 之间的连续函数，且 $P \subseteq E$ 是包容子集，则其逆像 $f^{-1}(P) = \{ x \in D \mid f(x) \in P \}$ 是 $D$ 的包容子集。

这意味着，只要替换的表达式是连续的，包容谓词在变量替换下是封闭的。

**逻辑运算**：

- 包容子集在有限并和任意交下是封闭的。
- 对于包容谓词 $P(x)$ 和 $Q(x)$，谓词 $P(x) \wedge Q(x)$ 和 $P(x) \vee Q(x)$ 是包容的。
- **注意**：包容谓词在全称量化下是封闭的，但在存在量化下未必封闭。

---

### 示例

**例子**：

设 $T_\bot$ 是真值的 cpo，即 $\{ \text{true}, \ \text{false}, \ \bot \}$，有序为：

$$
\bot \sqsubseteq \text{false}, \quad \bot \sqsubseteq \text{true}
$$

定义连续函数 $p : D \rightarrow T_\bot$ 和 $h : D \rightarrow D$，其中 $h$ 是严格的（即 $h(\bot) = \bot$）。设 $f : D \times D \rightarrow D$ 是满足以下方程的最小连续函数：

$$
f(x, y) = p(x) \rightarrow y \ \| \ h(f(h(x), y))
$$

对于所有 $x, y \in D$。

**证明目标**：

1. 对于所有 $b \in T_\bot$，$d, e \in D$，有：

   $$
   h(b \rightarrow_\bot d \| e) = b \rightarrow_\bot h(d) \| h(e)
   $$

2. 对于所有 $x, y \in D$，有：

   $$
   h(f(x, y)) = f(x, h(y))
   $$

**证明**：

1. **证明 (1)**：

   - 考虑 $b \in T_\bot$ 的三种可能值：$\bot$、$\text{true}$、$\text{false}$。
   - 对于每种情况，验证等式成立。

2. **证明 (2)**：

   - 定义谓词：

     $$
     P(g) \equiv \forall x, y \in D, \ h(g(x, y)) = g(x, h(y))
     $$

   - **验证 $P$ 是包容的**：

     - 根据之前的构造方法，$P$ 是包容的。

   - **验证 $P$ 对初始值 $\bot$ 成立**：

     - 因为 $h$ 是严格的，所以 $P(\bot)$ 成立。

   - **验证 $P(g) \implies P(F(g))$**：

     - 定义 $F(g) = \lambda x, y. \ p(x) \rightarrow y \ \| \ h(g(h(x), y))$。
     - 假设 $P(g)$ 成立，证明 $P(F(g))$ 成立。

   - **应用不动点归纳法**，得出 $P(f)$ 成立。

---

**总结**：

- 不动点归纳法是一个强有力的工具，可以证明连续函数的最小不动点的性质。
- 包容性质的构造需要谨慎，可以利用基本的包容关系、逆像、逻辑运算等方法。

---

## 结语

本章介绍了处理递归的技术，包括 Bekić 定理和不动点归纳法。我们学习了如何利用这些工具来证明连续函数的最小不动点的性质，以及如何构造包容性质。

这些技术在编程语言的形式语义学中非常重要，特别是在定义和证明递归结构和递归函数的性质时。

### ---------------------------

# 第10章 递归技术

本章提供了一些用于证明连续函数的最小不动点性质的技术。我们将介绍如何利用最小不动点作为最小前不动点来进行证明，这种方法有时被称为 **Park 归纳法**。我们将利用它来证明一个重要的结果，即 **Bekić 定理**，它提供了在 cpo（完备偏序集）的乘积中获得最小不动点的不同方法。

此外，我们将介绍 **Scott 的不动点归纳法**，以及它所依赖的 **包容性质（inclusive property）** 的概念；并提供构造包容性质的方法。然后，我们将给出一些关于 **良基归纳法（well-founded induction）** 的例子，扩展我们之前的工作，特别是展示如何构建良基关系。我们还将介绍一种称为 **良基递归（well-founded recursion）** 的通用方法，用于在具有良基关系的集合上定义函数。

本章的最后，我们将通过一个小但非平凡的练习，使用多种技术来证明两个列表上的递归函数的相等性。

---

## 10.3 良基归纳法

**背景**：

不动点归纳法对于某些类型的推理是不够的。例如，假设我们想要证明在整数上递归定义的函数总是在整数输入上终止。我们不能期望直接使用不动点归纳法来证明这一点。因为这将涉及到存在一个包容性质 $P$，它表达了终止性，并且对 $\bot$（完全未定义的函数）也为真。

因此，我们需要一个额外的证明原则，能够利用计算中使用的数据是如何归纳定义的。一个合适的原则是 **良基归纳法（well-founded induction）**。

**回顾**：

在第3章中，我们已经提到，一个集合 $A$ 上的 **良基关系**（well-founded relation）是一个没有无限下降链的二元关系 $\prec$。记住良基归纳法的原则是：

**良基归纳法原则**：

对于集合 $A$ 上的良基关系 $\prec$，设 $P$ 是一个性质。那么：

$$
\forall a \in A.\ P(a) \iff \left( \forall a \in A.\ \left( \left( \forall b \prec a.\ P(b) \right) \implies P(a) \right) \right)
$$

**解释**：

- **左侧**：对所有 $a \in A$，$P(a)$ 为真。
- **右侧**：对所有 $a \in A$，如果 $a$ 的所有前驱（即所有满足 $b \prec a$ 的 $b$）都满足 $P(b)$，则 $P(a)$ 为真。

**应用**：

应用该原则通常取决于对良基关系的明智选择。我们已经使用过一些良基关系，例如语法集合上的严格子表达式关系，或自然数上的“小于”关系 $<$。

这里，我们将给出一些构造更多良基关系的常用方法。需要注意的是，我们使用 $x \preccurlyeq y$ 来表示 $x \prec y$ 或 $x = y$。

---

### 构造良基关系的方法

1. **积（Product）**：

   如果 $\prec_1$ 是集合 $A_1$ 上的良基关系，$\prec_2$ 是集合 $A_2$ 上的良基关系，那么在 $A_1 \times A_2$ 上定义关系 $\preccurlyeq$ 为：

   $$
   (a_1, a_2) \preccurlyeq (b_1, b_2) \iff \left( a_1 \prec_1 b_1 \lor (a_1 = b_1 \land a_2 \preccurlyeq_2 b_2) \right)
   $$

   这给出了 $A_1 \times A_2$ 上的良基关系。

   **注意**：然而，积关系通常不如通过词典序（lexicographic orderings）产生的关系更普遍适用。

2. **词典序积（Lexicographic products）**：

   如果 $\prec_1$ 是 $A_1$ 上的良基关系，$\prec_2$ 是 $A_2$ 上的良基关系，那么在 $A_1 \times A_2$ 上定义词典序关系 $\prec$ 为：

   $$
   (a_1, a_2) \prec (b_1, b_2) \iff \left( a_1 \prec_1 b_1 \lor (a_1 = b_1 \land a_2 \prec_2 b_2) \right)
   $$

   **解释**：

   - 首先比较 $a_1$ 和 $b_1$，如果 $a_1 \prec_1 b_1$，则 $(a_1, a_2) \prec (b_1, b_2)$。
   - 如果 $a_1 = b_1$，则比较 $a_2$ 和 $b_2$，如果 $a_2 \prec_2 b_2$，则 $(a_1, a_2) \prec (b_1, b_2)$。

3. **逆像（Inverse image）**：

   如果 $f : A \rightarrow B$ 是一个函数，$\prec_B$ 是 $B$ 上的良基关系，那么在 $A$ 上定义关系 $\prec_A$ 为：

   $$
   a \prec_A a' \iff f(a) \prec_B f(a')
   $$

   这样，$\prec_A$ 是 $A$ 上的良基关系。

---

### 练习10.15

**问题**：

设 $\prec$ 是集合 $X$ 上的良基关系，且 $\prec$ 是全序关系（total order）。证明它不一定满足对于所有 $y \in X$，集合 $\{ x \in X \mid x \prec y \}$ 是有限的。

**（全序关系是一个偏序关系 $\leq$，满足对于其所有元素 $x, y$，要么 $x \leq y$，要么 $y \leq x$。）**

**提示**：考虑在 $\omega \times \omega$ 上的词典序积。

**解答**：

- **构造**：考虑集合 $X = \omega \times \omega$，即所有自然数对的集合。
- 定义在 $X$ 上的词典序关系 $\prec$：

  $$
  (n_1, m_1) \prec (n_2, m_2) \iff \left( n_1 < n_2 \lor (n_1 = n_2 \land m_1 < m_2) \right)
  $$

- 这是一个全序关系，因为对于任意的 $(n_1, m_1), (n_2, m_2) \in X$，要么 $(n_1, m_1) \preceq (n_2, m_2)$，要么 $(n_2, m_2) \preceq (n_1, m_1)$。

- 然而，对于固定的 $y = (n, m)$，集合 $\{ x \in X \mid x \prec y \}$ 是无限的。

  - 因为对于所有 $k < m$，都有 $(n, k) \prec (n, m)$。

  - 而对于所有 $k < n$，$m' \in \omega$，都有 $(k, m') \prec (n, m)$。

  - 因此，$\{ x \in X \mid x \prec y \}$ 是无限集。

- **结论**：因此，即使 $\prec$ 是良基关系和全序关系，$\{ x \in X \mid x \prec y \}$ 也不一定是有限的。

---

### 练习10.16

**问题**：

证明积、词典序积和逆像构造可以从良基关系中产生新的良基关系。

**解答**：

1. **积关系**：

   - 如果 $\prec_1$ 是 $A_1$ 上的良基关系，$\prec_2$ 是 $A_2$ 上的良基关系。

   - 定义在 $A_1 \times A_2$ 上的关系为：

     $$
     (a_1, a_2) \prec (b_1, b_2) \iff \left( a_1 \prec_1 b_1 \lor (a_1 = b_1 \land a_2 \prec_2 b_2) \right)
     $$

   - 假设存在一个无限下降链：

     $$
     (a_1^{(0)}, a_2^{(0)}) \succ (a_1^{(1)}, a_2^{(1)}) \succ (a_1^{(2)}, a_2^{(2)}) \succ \dots
     $$

   - 由于 $\prec_1$ 和 $\prec_2$ 是良基关系，因此不可能存在这样的无限下降链。

   - 因此，积关系是良基关系。

2. **词典序关系**：

   - 同样地，如果 $\prec_1$ 和 $\prec_2$ 是良基关系，词典序关系 $\prec$ 也是良基关系。

   - 假设存在一个无限下降链：

     $$
     (a_1^{(0)}, a_2^{(0)}) \succ (a_1^{(1)}, a_2^{(1)}) \succ \dots
     $$

   - 由于词典序的定义，$a_1^{(i)}$ 必须在某个时刻下降，否则 $a_1$ 保持不变，$a_2^{(i)}$ 会无限下降。

   - 由于 $\prec_1$ 和 $\prec_2$ 是良基的，这都不可能。

3. **逆像**：

   - 如果 $f : A \rightarrow B$，$B$ 上有良基关系 $\prec_B$。

   - 定义 $A$ 上的关系为 $a \prec_A a' \iff f(a) \prec_B f(a')$。

   - 假设存在 $A$ 中的无限下降链 $a_0 \succ_A a_1 \succ_A a_2 \succ_A \dots$。

   - 那么在 $B$ 中有 $f(a_0) \succ_B f(a_1) \succ_B f(a_2) \succ_B \dots$。

   - 这与 $\prec_B$ 是良基关系矛盾。

---

### 例子：Ackermann 函数

**定义**：

Ackermann 函数可以在 REC 中通过以下声明定义：

$$
A(x, y) = \text{if } x \ \text{then } y + 1 \ \text{else } \left( \text{if } y \ \text{then } A(x - 1, 1) \ \text{else } A(x - 1, A(x, y - 1)) \right)
$$

在值调用的指称语义下，这将 $A$ 声明为 $[N^2 \rightarrow N_\bot]$ 中的最小函数 $a$，使得对于所有 $m, n \in N$：

$$
a(m, n) =
\begin{cases}
n + 1, & \text{如果 } m = 0 \\
a(m - 1, 1), & \text{如果 } m \ne 0, \ n = 0 \\
a(m - 1, a(m, n - 1)), & \text{否则}
\end{cases}
$$

**目标**：

证明 Ackermann 函数 $a(m, n)$ 在所有 $m, n \geq 0$ 上都终止（即，不返回 $\bot$）。

**方法**：

- 使用良基归纳法，取 $(m, n)$ 按词典序排列的良基关系。

- **词典序关系**：

  - 定义在 $N \times N$ 上的关系 $\prec$ 为：

    $$
    (m', n') \prec (m, n) \iff \left( m' < m \lor (m' = m \land n' < n) \right)
    $$

- **证明**：

  - 对于所有的 $(m, n)$，假设对于所有 $(m', n') \prec (m, n)$，$a(m', n')$ 终止并且 $a(m', n') \geq 0$。

  - 需要证明 $a(m, n)$ 终止并且 $a(m, n) \geq 0$。

  - 通过对不同的情况进行分析，利用归纳假设，证明 $a(m, n)$ 终止。

---

### 练习10.17

**问题**：

通过良基归纳法证明 Ackermann 函数 $a(m, n)$ 在所有整数 $m, n \geq 0$ 上终止。

**提示**：

取归纳假设为：

$$
P(m, n) \iff (a(m, n) \ne \bot \ \text{且} \ a(m, n) \geq 0)
$$

**解答**：

- **归纳基**：

  - 当 $m = 0$ 时，$a(0, n) = n + 1$，显然终止，且 $a(0, n) \geq 1$。

- **归纳步骤**：

  - 假设对于所有 $(m', n') \prec (m, n)$，$P(m', n')$ 成立。

  - **情况1**：$m \ne 0$，$n = 0$：

    - $a(m, 0) = a(m - 1, 1)$。

    - 因为 $(m - 1, 1) \prec (m, 0)$，根据归纳假设，$a(m - 1, 1)$ 终止且 $\geq 0$。

    - 因此，$a(m, 0)$ 终止且 $\geq 0$。

  - **情况2**：$m \ne 0$，$n \ne 0$：

    - $a(m, n) = a(m - 1, a(m, n - 1))$。

    - 首先，需要证明 $a(m, n - 1)$ 终止且 $\geq 0$。

      - 因为 $(m, n - 1) \prec (m, n)$，根据归纳假设，$a(m, n - 1)$ 终止且 $\geq 0$。

    - 然后，需要证明 $a(m - 1, a(m, n - 1))$ 终止。

      - 因为 $a(m, n - 1) \geq 0$，所以 $(m - 1, a(m, n - 1)) \prec (m, n)$。

      - 根据归纳假设，$a(m - 1, a(m, n - 1))$ 终止且 $\geq 0$。

    - 因此，$a(m, n)$ 终止且 $\geq 0$。

- **结论**：

  - 对于所有 $m, n \geq 0$，$a(m, n)$ 终止且 $\geq 0$。

---

### 练习10.18

**问题**：

McCarthy 的 91 函数定义为 $[N \rightarrow N_\bot]$ 中的最小函数，满足：

$$
f(x) = \text{cond}(x > 100, \l x - 10 \r, \text{let } y \Leftarrow f(x + 11). f(y))
$$

（这里使用了第8.3.5节中的条件表达式。）

证明对于所有非负整数 $x$，有：

$$
f(x) = \text{cond}(x > 100, \l x - 10 \r, \l 91 \r)
$$

使用在 $\omega$ 上的良基关系 $n \prec m \iff m < n \leq 101$。

首先证明 $\prec$ 是良基关系。

**解答**：

1. **证明 $\prec$ 是良基关系**：

   - 由于 $\omega$ 是自然数的集合。

   - 关系 $n \prec m$ 定义为 $m < n \leq 101$。

   - 因此，$\prec$ 只在有限的范围内（$1 \leq n \leq 101$）成立。

   - 不存在无限下降链，因此 $\prec$ 是良基关系。

2. **证明 $f(x)$ 的性质**：

   - **当 $x > 100$ 时**，$f(x) = x - 10$。

   - **当 $x \leq 100$ 时**：

     - $f(x) = f(f(x + 11))$。

     - 由于 $x + 11 > x$，且 $x + 11 \geq x + 11 > 100$（当 $x \geq 90$ 时）。

     - 经过有限次递归调用，最终会有 $x + k \cdot 11 > 100$。

   - **使用良基归纳法**：

     - 设归纳假设为：对于所有 $n \prec m$，$f(n)$ 的性质成立。

     - 对于 $x \leq 100$，$f(x) = f(f(x + 11))$。

     - 因为 $x + 11 > x$，所以 $x + 11$ 更大。

     - 最终 $f(x) = 91$。

3. **结论**：

   - 对于所有 $x \geq 0$，$f(x) = \text{cond}(x > 100, x - 10, 91)$。

---

## 10.4 良基递归

**背景**：

在第3章中，我们注意到归纳定义和结构归纳都允许一种递归定义的形式。例如，算术表达式的长度可以递归地根据其严格子表达式的长度来定义；对于特定的参数（如 $a_1 + a_2$），其长度函数的定义基于其较小的参数（如 $a_1$ 和 $a_2$）的长度。

类似地，我们有权在任意良基集合上定义函数。假设 $B$ 是一个带有良基关系 $\prec$ 的集合。通过 **良基归纳法**（称为 **良基递归**），允许通过 $b$ 的前驱 $b' \prec b$ 上的函数值 $f(b')$ 来指定 $f(b)$。

**定义**：

- 对于 $B$ 中的每个元素 $b$，其前驱集合为：

  $$
  \prec^{-1}\{b\} = \{ b' \in B \mid b' \prec b \}
  $$

- 对于任意 $B' \subseteq B$，函数 $f : B \rightarrow C$ 在 $B'$ 上的限制为 $f |_{B'} : B' \rightarrow C$。

**良基递归定理（定理10.19）**：

**定理**：

设 $\prec$ 是集合 $B$ 上的良基关系。假设对于所有 $b \in B$，和函数 $h : \prec^{-1}\{b\} \rightarrow C$，都定义了 $F(b, h) \in C$。则存在唯一的函数 $f : B \rightarrow C$，使得：

$$
\forall b \in B.\ f(b) = F(b, f |_{\prec^{-1}\{b\}})
$$

**证明**：

**步骤**：

1. **唯一性**：

   - 假设存在两个函数 $f$ 和 $g$ 满足条件。

   - 对于任意 $x \in B$，如果对于所有 $y \prec x$，$f(y) = g(y)$，则有 $f(x) = g(x)$。

   - 这是通过良基归纳法证明的。

2. **存在性**：

   - 通过对 $x \in B$ 进行良基归纳，构造函数 $f$。

   - 假设对于所有 $z \prec x$，已经构造了 $f(z)$。

   - 定义 $f(x) = F(x, f |_{\prec^{-1}\{x\}})$。

   - 这样，我们就构造了函数 $f$。

**结论**：

- 存在唯一的函数 $f$，满足上述递归定义。

---

**应用**：

良基递归和归纳构成了一种通用的方法，通常适用于希望函数是全定义的情况。例如，从递归定理可以立即得出，在非负整数上存在唯一的全函数 ack，满足：

$$
\text{ack}(m, n) =
\begin{cases}
n + 1, & \text{如果 } m = 0 \\
\text{ack}(m - 1, 1), & \text{如果 } m \ne 0, \ n = 0 \\
\text{ack}(m - 1, \text{ack}(m, n - 1)), & \text{否则}
\end{cases}
$$

对于所有 $m, n \geq 0$。注意，$\text{ack}$ 在对 $(m - 1, 1)$ 和 $(m, n - 1)$ 的值进行定义时，只使用了比 $(m, n)$ 词典序更小的参数。

实际上，许多递归程序都是这样编写的，即在它们被求值时，某个良基集合中的度量会减小。对于这样的程序，通常可以用良基递归和归纳来替代最小不动点的机制。

---

## 10.5 一个练习

我们以一个练习结束本章，展示如何证明两个列表上的递归函数是相等的。解决这个问题结合了许多关于递归定义的推理技术。

我们倾向于专注于算术和布尔运算。这里，我们将关注整数的有限列表上的操作。

**列表的定义**：

- 一个整数列表（integer-list）通常是形如：

  $$
  [n_1; n_2; n_3; \dots; n_k]
  $$

  的列表，由 $N$ 中的 $k$ 个元素组成。

- 空列表也被认为是一个列表，记作 $[ \ ]$。

**基本操作**：

1. **空列表**：

   - 一个常数操作，接受空参数，返回空列表 $[ \ ]$。

2. **cons 操作**：

   - 将一个整数 $m$ 添加到列表 $l$ 的前面，记作 $m :: l$。

   - 例如：

     $$
     1 :: [2; 3; 4] = [1; 2; 3; 4]
     $$

**列表形成一个离散的 cpo**，称为 $\text{List}$。它可以表示为两个离散 cpo 的和：

$$
\text{List} = \text{in}_1 \{ ( ) \} \cup \text{in}_2 (N \times \text{List}) = \{ ( ) \} + (N \times \text{List})
$$

其中，注入函数的作用为：

- $\text{in}_1 ( ) = [ \ ]$（空列表）。
- $\text{in}_2 (m, l) = m :: l$。

这反映了整数列表的离散 cpo 与所有整数元组（包括空元组）的 cpo 同构。

**cases 构造**：

- 定义一个 cases 构造，用于对列表进行模式匹配：

  $$
  \text{case } l \ \text{of } [ \ ]. e_1 \ | \ x :: l'. e_2
  $$

- 这在递归定义中很有用。

**append 函数的定义**：

- 我们定义一个函数 $\text{append} : \text{List} \times \text{List} \rightarrow (\text{List})_\bot$，用于连接两个列表。

- $\text{append}$ 的定义为 cpo $[\text{List} \times \text{List} \rightarrow (\text{List})_\bot]$ 中的最小函数 $a$，满足：

  $$
  \begin{cases}
  a([ \ ], ls) = \l ls \r \\
  a(x :: l', ls) = \text{let } r \Leftarrow a(l', ls). \l x :: r \r
  \end{cases}
  $$

- **解释**：

  - 当第一个列表为空时，结果就是第二个列表。

  - 当第一个列表非空时，将第一个元素 $x$ 放在前面，然后递归地将剩余的列表 $l'$ 与 $ls$ 连接。

**证明**：

- 我们可以通过对第一个参数列表的长度进行归纳，证明 $\text{append}$ 总是全定义的。

- 定义列表之间的关系为：$l' \prec l$ 当且仅当列表 $l'$ 严格小于列表 $l$。

- 我们也可以通过良基递归，定义一个稍微不同的 append 操作 $\ @ \ : \text{List} \times \text{List} \rightarrow \text{List}$。

- 通过良基递归定理，可以定义唯一的全函数 $\ @$，满足：

  $$
  l @ ls = \text{case } l \ \text{of } [ \ ]. ls \ | \ x :: l'. x :: (l' @ ls)
  $$

- 对于所有 $l, ls \in \text{List}$。

- 我们可以通过良基归纳，证明两个函数之间的关系：

  $$
  \text{append}(l, ls) = \l l @ ls \r
  $$

---

**现在，我们可以陈述问题**：

### 练习10.20

**假设**：

- 有整数上的函数 $s : N \times N \rightarrow N$ 和 $r : N \times N \rightarrow \text{List}$。

- 定义函数 $f$ 为 $[\text{List} \times N \rightarrow N_\bot]$ 中的最小函数，满足：

  $$
  \begin{cases}
  f([ \ ], y) = \l y \r \\
  f(x :: xs, y) = f(r(x, y) @ xs, s(x, y))
  \end{cases}
  $$

- 定义函数 $g$ 为 $[\text{List} \times N \rightarrow N_\bot]$ 中的最小函数，满足：

  $$
  \begin{cases}
  g([ \ ], y) = \l y \r \\
  g(x :: xs, y) = \text{let } v \Leftarrow g(r(x, y), s(x, y)). g(xs, v)
  \end{cases}
  $$

**要求**：

证明 $f = g$。

**提示**：

1. 首先通过列表的大小进行归纳，证明 $g$ 满足：

   $$
   g(l @ xs, y) = \text{let } v \Leftarrow g(l, y). g(xs, v)
   $$

2. 推导出 $f \sqsubseteq g$。

3. 然后，通过不动点归纳法，证明 $f$ 满足：

   $$
   (\text{let } u \Leftarrow f(l, y). f(xs, u)) \sqsubseteq f(l @ xs, y)
   $$

   - 采用包容谓词：

     $$
     P(F) \equiv \forall l, xs, y.\ (\text{let } u \Leftarrow F(l, y). f(xs, u)) \sqsubseteq f(l @ xs, y)
     $$

4. 推导出 $g \sqsubseteq f$。

**解答**：

**第一步：证明 $g(l @ xs, y) = \text{let } v \Leftarrow g(l, y). g(xs, v)$**

- **证明思路**：

  - 对列表 $l$ 的大小进行归纳。

- **归纳基**（$l = [ \ ]$）：

  - $l = [ \ ]$，则：

    $$
    \begin{align*}
    g([ \ ] @ xs, y) &= g(xs, y) \\
    &= \text{let } v \Leftarrow g([ \ ], y). g(xs, v) \\
    &= \text{let } v \Leftarrow \l y \r. g(xs, v) \\
    &= \text{let } v = y. g(xs, v)
    \end{align*}
    $$

- **归纳假设**：

  - 假设对于列表 $l'$，$g(l' @ xs, y) = \text{let } v \Leftarrow g(l', y). g(xs, v)$。

- **归纳步骤**（$l = x :: l'$）：

  - 计算 $g((x :: l') @ xs, y)$：

    $$
    \begin{align*}
    (x :: l') @ xs &= x :: (l' @ xs) \\
    g(x :: (l' @ xs), y) &= \text{let } v \Leftarrow g(r(x, y), s(x, y)). g(l' @ xs, v) \\
    \end{align*}
    $$

  - 根据归纳假设，$g(l' @ xs, v) = \text{let } w \Leftarrow g(l', v). g(xs, w)$。

  - 因此，

    $$
    \begin{align*}
    g(x :: (l' @ xs), y) &= \text{let } v \Leftarrow g(r(x, y), s(x, y)). \left( \text{let } w \Leftarrow g(l', v). g(xs, w) \right) \\
    &= \text{let } v \Leftarrow g(r(x, y), s(x, y)). \text{let } w \Leftarrow g(l', v). g(xs, w) \\
    &= \text{let } v \Leftarrow g(l', v). \text{let } w \Leftarrow g(r(x, y), s(x, y)). g(xs, w) \\
    \end{align*}
    $$

  - **注意**：这里可能需要重新排列 let 表达式，以使其符合定义。

  - **另一方面**，

    $$
    \begin{align*}
    \text{let } v \Leftarrow g(x :: l', y). g(xs, v) &= \text{let } v \Leftarrow \text{let } u \Leftarrow g(r(x, y), s(x, y)). g(l', u). g(xs, v) \\
    \end{align*}
    $$

  - 通过展开和简化，可以证明两者相等。

- **结论**：

  - 因此，$g(l @ xs, y) = \text{let } v \Leftarrow g(l, y). g(xs, v)$ 对于所有列表 $l$ 成立。

**第二步：推导出 $f \sqsubseteq g$**

- **思路**：

  - 由于 $f$ 的定义中，直接递归调用自身，而 $g$ 的定义更复杂，因此 $f$ 的值可能比 $g$ 更精确。

- **证明**：

  - 我们可以通过对 $f$ 的定义进行展开，发现 $f$ 的计算路径是 $g$ 的一个子集。

- **结论**：

  - 因此，$f \sqsubseteq g$。

**第三步：证明 $g \sqsubseteq f$**

- **思路**：

  - 通过不动点归纳法，证明 $f$ 满足某个性质，然后得出 $g \sqsubseteq f$。

- **定义包容谓词**：

  - 定义 $P(F)$ 为：

    $$
    P(F) \equiv \forall l, xs, y.\ (\text{let } u \Leftarrow F(l, y). f(xs, u)) \sqsubseteq f(l @ xs, y)
    $$

- **验证 $P$ 是包容的**。

- **验证 $P(f)$ 成立**：

  - 需要证明对于所有 $l, xs, y$，有：

    $$
    (\text{let } u \Leftarrow f(l, y). f(xs, u)) \sqsubseteq f(l @ xs, y)
    $$

  - 通过不动点归纳法，假设对于所有 $l', xs', y'$，$P(f)$ 成立。

  - 通过展开 $f$ 的定义，验证上述不等式成立。

- **结论**：

  - 由不动点归纳法，得出 $g \sqsubseteq f$。

**第四步：得出 $f = g$**

- **由于 $f \sqsubseteq g$ 且 $g \sqsubseteq f$，因此 $f = g$。**

---

**总结**：

通过结合良基归纳法和不动点归纳法，我们证明了两个递归定义的函数 $f$ 和 $g$ 是相等的。这展示了如何应用不同的递归技术来证明函数的性质。

---

## 10.6 进一步阅读

本章的内容受到了以下文献的影响：[80]、[59] 和 [89]。特别是，Manna 的书 [59] 是关于不动点和良基归纳法的练习的丰富来源（尽管遗憾的是，他在那里将后者称为“结构归纳”）。感谢 Larry Paulson 提供了关于列表的这个问题。

需要注意的是，“包容”性质和谓词的术语并不统一。这里使用的“包容”一词来源于 Gordon Plotkin 的讲义 [80]。其他人使用“admissible”（可接受的），但也有其他名称。

对于良基递归的详细处理，可以参考 Enderton 的书 [39]。

---

**注**：文中提到的数字 [80]、[59] 等是参考文献编号，可以在原书的参考文献部分找到对应的文献信息。

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