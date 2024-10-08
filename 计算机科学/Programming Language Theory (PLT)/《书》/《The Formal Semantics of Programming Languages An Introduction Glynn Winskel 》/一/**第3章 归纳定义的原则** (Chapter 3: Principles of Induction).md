[toc]



# 第3章 归纳法的一些原理

证明程序性质的过程通常依赖于一种证明方法，或者说是一组证明方法，称为**归纳法（induction）**。最常用的归纳形式是**数学归纳法（mathematical induction）**和**结构归纳法（structural induction）**。这两者都是一种强大的证明方法的特例，称为**良基归纳法（well-founded induction）**。

## 3.1 数学归纳法

自然数是从 0 开始，通过不断添加后继元素构建起来的。自然数由以这种方式获得的元素组成。这对应着一个称为**数学归纳法**的证明原理。

设 $P(n)$ 是关于自然数 $n = 0, 1, 2, \dots$ 的一个性质。数学归纳法的原理指出，为了证明 $P(n)$ 对所有自然数 $n$ 都成立，只需证明：

- **基始情况（Base Case）**：$P(0)$ 为真。
- **归纳步骤（Inductive Step）**：如果对于任意自然数 $m$，$P(m)$ 为真，那么 $P(m + 1)$ 也为真。

我们可以用一些逻辑符号更简洁地表示：

$$
\left( P(0)\ \&\ \forall m \in \omega.\ P(m) \Rightarrow P(m + 1) \right) \Rightarrow \forall n \in \omega.\ P(n)
$$

**解释**：

- $\omega$ 表示自然数的集合。
- $P(0)$ 是基始情况，表示 $P$ 在 $n = 0$ 时成立。
- $\forall m \in \omega.\ P(m) \Rightarrow P(m + 1)$ 是归纳步骤，表示如果 $P(m)$ 成立，那么 $P(m + 1)$ 也成立。
- 整个公式表示，如果基始情况和归纳步骤都成立，那么 $P(n)$ 对所有自然数 $n$ 都成立。

这个原理直观上是清晰的：如果我们知道 $P(0)$ 成立，并且我们有一种方法可以从假设 $P(m)$ 成立推出 $P(m + 1)$ 成立，那么从 $P(0)$ 我们可以知道 $P(1)$ 成立，重复应用这个方法，可以得到 $P(2)$，然后是 $P(3)$，以此类推。

- **归纳假设（Induction Hypothesis）**：$P(m)$，即假设 $P$ 对 $m$ 成立。
- **归纳基础（Basis of the Induction）**：$P(0)$，即基始情况。
- **归纳步骤（Induction Step）**：$\forall m \in \omega.\ P(m) \Rightarrow P(m + 1)$。

**注意**：在所有归纳证明方法中，都有一个共同的特点，即最初选择的归纳假设可能无法在证明中奏效。如果需要证明性质 $P$ 对所有自然数成立，当然可以尝试使用 $P(m)$ 作为归纳假设。但在证明归纳步骤 $\forall m \in \omega.\ P(m) \Rightarrow P(m + 1)$ 时，可能无法完成。这是因为在从 $P(m)$ 推出 $P(m + 1)$ 时，$P(m)$ 的假设可能不够强。

解决方法是**加强归纳假设**，将其替换为一个更强的性质 $P_1(m)$，使得 $P_1(m)$ 蕴含 $P(m)$。然而，找到合适的 $P_1(m)$ 是一门艺术，因为虽然在证明归纳步骤时，我们有了更强的假设 $P_1(m)$，但需要证明的 $P_1(m + 1)$ 可能更加困难，甚至无法证明。

在证明一个性质 $Q(m)$ 对所有自然数 $m$ 都成立时，可能会遇到 $Q(m + 1)$ 的成立不仅依赖于其前一项 $Q(m)$ 的成立，还依赖于其他比 $m$ 小的数的成立。这时，我们可以将 $Q(m)$ 加强为归纳假设 $P(m)$，表示“对于所有 $k < m$，$Q(k)$ 都成立”。将 $P(m)$ 作为普通数学归纳法中的性质，我们得到：

- **基始情况**：$\forall k < 0.\ Q(k)$。

  由于没有严格小于 0 的自然数，基始情况是**平凡成立**的。

- **归纳步骤**：

  $$
  \forall m \in \omega.\ \left( \forall k < m.\ Q(k) \right) \Rightarrow \left( \forall k < m + 1.\ Q(k) \right)
  $$

  这等价于：

  $$
  \forall m \in \omega.\ \left( \forall k < m.\ Q(k) \right) \Rightarrow Q(m)
  $$

我们得到了**完全归纳法（course-of-values induction）**，这是数学归纳法的一种特殊形式：

$$
\left( \forall m \in \omega.\ \left( \forall k < m.\ Q(k) \right) \Rightarrow Q(m) \right) \Rightarrow \forall n \in \omega.\ Q(n)
$$

---

**练习 3.1**

**证明**：对于所有自然数，性质 $P(n)$ 成立，其中：

$$
P(n) \quad \Leftrightarrow \quad \sum_{i=1}^{n} (2i - 1) = n^2
$$

**提示**：$\sum_{i=k}^{l} S_i$ 表示当 $k \leq l$ 时，$S_k + S_{k+1} + \dots + S_l$。

**解答**：

**目标**：证明对于所有自然数 $n$，有：

$$
\sum_{i=1}^{n} (2i - 1) = n^2
$$

**方法**：使用数学归纳法。

**基始情况**（$n = 1$）：

$$
\sum_{i=1}^{1} (2i - 1) = 2 \times 1 - 1 = 1 = 1^2
$$

所以，$P(1)$ 成立。

**归纳假设**：假设对于某个 $n = k$，有：

$$
\sum_{i=1}^{k} (2i - 1) = k^2
$$

**归纳步骤**：证明对于 $n = k + 1$，$P(k + 1)$ 成立，即：

$$
\sum_{i=1}^{k+1} (2i - 1) = (k + 1)^2
$$

**证明**：

左边：

$$
\sum_{i=1}^{k+1} (2i - 1) = \left( \sum_{i=1}^{k} (2i - 1) \right) + (2(k + 1) - 1)
$$

根据归纳假设，$\sum_{i=1}^{k} (2i - 1) = k^2$，所以：

$$
\sum_{i=1}^{k+1} (2i - 1) = k^2 + (2k + 2 - 1) = k^2 + (2k + 1)
$$

计算：

$$
k^2 + 2k + 1 = (k + 1)^2
$$

所以，

$$
\sum_{i=1}^{k+1} (2i - 1) = (k + 1)^2
$$

因此，$P(k + 1)$ 成立。

**结论**：根据数学归纳法，$P(n)$ 对所有自然数 $n$ 成立。

---

**练习 3.2**

一个**字符串（string）**是符号的序列。长度为 $n$ 的字符串由 $n$ 个位置上的符号组成。一个字符串可以是空的，此时其长度为 0。两个字符串 $s$ 和 $t$ 可以**连接（concatenate）**形成字符串 $st$。

**证明**：对于两个不同的符号 $a$ 和 $b$，不存在字符串 $u$ 满足 $au = ub$。

**解答**：

**目标**：证明不存在字符串 $u$，使得 $au = ub$，其中 $a \ne b$。

**方法**：使用数学归纳法，基于字符串的长度。

**基始情况**：字符串 $u$ 的长度为 0。

- 如果 $u$ 是空字符串，即长度为 0，那么 $au$ 是 $a$，$ub$ 是 $b$。因为 $a \ne b$，所以 $au \ne ub$。

**归纳假设**：假设对于长度为 $k$ 的字符串，不存在满足 $au = ub$ 的字符串 $u$。

**归纳步骤**：考虑长度为 $k + 1$ 的字符串 $u$，设 $u = c v$，其中 $c$ 是第一个符号，$v$ 是剩余的字符串，长度为 $k$。

- 那么，$au = a c v$，$ub = c v b$。
- 如果 $a c v = c v b$，则比较第一个符号：

  - 若 $a = c$，则 $a = c$。
  - 但是已知 $a \ne b$，且 $c v$ 是公共部分，因此不可能有 $a = c$，矛盾。

- 因此，假设成立。

**结论**：根据数学归纳法，不存在字符串 $u$ 满足 $au = ub$。

---

## 3.2 结构归纳法

我们需要一种技术来证明一些“显然”的事实，例如：

$$
(a, \sigma) \rightarrow m\ \&\ (a, \sigma) \rightarrow m'\ \Rightarrow\ m = m'
$$

对所有算术表达式 $a$、状态 $\sigma$ 和数字 $m, m'$。这表示 IMP 中算术表达式的求值是**确定性的（deterministic）**。标准工具是**结构归纳法（structural induction）**。我们以算术表达式为例，但当然它更普遍地适用于语言 IMP 的所有语法集。

设 $P(a)$ 是关于算术表达式 $a$ 的一个性质。为了证明 $P(a)$ 对所有算术表达式 $a$ 成立，只需证明：

1. **基础情况**：

   - 对于所有数字 $n$，$P(n)$ 成立。
   - 对于所有位置 $X$，$P(X)$ 成立。

2. **归纳步骤**：

   - 对于所有算术表达式 $a_0$ 和 $a_1$，如果 $P(a_0)$ 和 $P(a_1)$ 成立，那么 $P(a_0 + a_1)$ 也成立。
   - 同理，对于 $a_0 - a_1$ 和 $a_0 \times a_1$。

**解释**：

- **归纳假设**：$P(a)$，即假设性质 $P$ 对 $a$ 成立。
- 这个原理表示，为了证明归纳假设对所有算术表达式都成立，只需证明它对原子表达式（数字和位置）成立，并且在构造新的算术表达式时，该性质被保持。

**用逻辑符号更紧凑地表示**：

$$
\left( \forall n \in N.\ P(n) \right)\ \&\ \left( \forall X \in \text{Loc}.\ P(X) \right)\ \&\ \left( \forall a_0, a_1 \in \text{Aexp}.\ P(a_0) \ \&\ P(a_1) \Rightarrow P(a_0 + a_1) \right)\ \&\ \left( \text{类似的对于减法和乘法} \right) \Rightarrow \forall a \in \text{Aexp}.\ P(a)
$$

实际上，这些条件不仅蕴含 $\forall a \in \text{Aexp}.\ P(a)$，而且与之等价。

有时，仅需要一种退化形式的结构归纳法，即对表达式结构的分类讨论（case analysis）。当一个性质仅仅由于表达式可以采取的不同形式而对所有表达式成立时，不需要使用对子表达式性质的假设。

**例如，对算术表达式进行分类讨论**，如果：

- 对于所有数字 $n$，$P(n)$ 成立。
- 对于所有位置 $X$，$P(X)$ 成立。
- 对于所有算术表达式 $a_0, a_1$，$P(a_0 + a_1)$ 成立（不需要假设 $P(a_0)$ 和 $P(a_1)$）。
- 类似的对于减法和乘法。

那么，可以得出 $\forall a \in \text{Aexp}.\ P(a)$。

---

**示例：证明算术表达式的求值是确定性的**

**命题 3.3**

对于所有算术表达式 $a$、状态 $\sigma$ 和数字 $m, m'$，如果：

$$
(a,\ \sigma)\ \rightarrow\ m\ \&\ (a,\ \sigma)\ \rightarrow\ m'
$$

那么：

$$
m = m'
$$

**证明**：

**目标**：证明对于所有 $a$，如果在状态 $\sigma$ 下，$a$ 求值为 $m$ 和 $m'$，那么 $m = m'$。

**方法**：对算术表达式 $a$ 进行结构归纳，归纳假设为 $P(a)$，其中：

$$
P(a) \quad \Leftrightarrow \quad \forall m, m'.\ \left( (a,\ \sigma)\ \rightarrow\ m\ \&\ (a,\ \sigma)\ \rightarrow\ m'\ \Rightarrow\ m = m' \right)
$$

**证明过程**：

- **基础情况**：

  - 当 $a$ 是数字 $n$ 时：

    根据求值规则，对于数字的求值，只有一条规则：

    $$
    \frac{}{\ (n,\ \sigma)\ \rightarrow\ n }
    $$

    所以，$m = m' = n$。

  - 当 $a$ 是位置 $X$ 时：

    求值规则：

    $$
    \frac{}{\ (X,\ \sigma)\ \rightarrow\ \sigma(X) }
    $$

    所以，$m = m' = \sigma(X)$。

- **归纳步骤**：

  - 当 $a = a_0 + a_1$ 时：

    假设对于 $a_0$ 和 $a_1$，归纳假设成立。

    如果 $(a,\ \sigma)\ \rightarrow\ m$ 和 $(a,\ \sigma)\ \rightarrow\ m'$，根据加法的求值规则：

    $$
    \frac{\ (a_0,\ \sigma)\ \rightarrow\ m_0 \quad (a_1,\ \sigma)\ \rightarrow\ m_1 }{\ (a_0 + a_1,\ \sigma)\ \rightarrow\ m }
    $$

    $$
    \frac{\ (a_0,\ \sigma)\ \rightarrow\ m_0' \quad (a_1,\ \sigma)\ \rightarrow\ m_1' }{\ (a_0 + a_1,\ \sigma)\ \rightarrow\ m' }
    $$

    根据归纳假设，对于 $a_0$，有 $m_0 = m_0'$；对于 $a_1$，有 $m_1 = m_1'$。

    所以，$m = m_0 + m_1 = m_0' + m_1' = m'$。

  - 类似地，可以对减法和乘法进行证明。

**结论**：根据结构归纳法，$P(a)$ 对所有 $a \in \text{Aexp}$ 成立。

---

**练习 3.4**

**证明**：通过结构归纳，证明算术表达式的求值总是终止的，即对于所有算术表达式 $a$ 和状态 $\sigma$，存在某个 $m$，使得 $(a,\ \sigma)\ \rightarrow\ m$。

**解答**：

**目标**：证明对所有 $a$，在状态 $\sigma$ 下，$a$ 总能求值到某个数字 $m$。

**方法**：对 $a$ 进行结构归纳。

- **基础情况**：

  - 当 $a$ 是数字或位置时，根据求值规则，直接得到 $(a,\ \sigma)\ \rightarrow\ m$。

- **归纳步骤**：

  - 当 $a = a_0 + a_1$ 时：

    根据归纳假设，存在 $m_0$ 和 $m_1$，使得 $(a_0,\ \sigma)\ \rightarrow\ m_0$，$(a_1,\ \sigma)\ \rightarrow\ m_1$。

    根据加法的求值规则，可以得到 $(a,\ \sigma)\ \rightarrow\ m$，其中 $m = m_0 + m_1$。

  - 类似地，对减法和乘法进行证明。

**结论**：根据结构归纳法，算术表达式的求值总是终止的。

---

**练习 3.5**

利用关于算术表达式的事实，通过结构归纳，证明布尔表达式的求值首先是确定性的，其次是完备的（总是终止的）。

**解答**：

**目标**：

1. 证明布尔表达式的求值是确定性的。

2. 证明布尔表达式的求值总是终止的。

**方法**：对布尔表达式 $b$ 进行结构归纳。

- **基础情况**：

  - 当 $b$ 是 $\text{true}$ 或 $\text{false}$ 时，求值是确定的，且终止。

- **归纳步骤**：

  - 当 $b = a_0 = a_1$ 或 $b = a_0 \leq a_1$ 时：

    根据归纳假设，$(a_0,\ \sigma)$ 和 $(a_1,\ \sigma)$ 的求值是确定且终止的。

    因此，比较 $a_0$ 和 $a_1$ 的值，布尔表达式的求值是确定且终止的。

  - 当 $b = \neg b_0$ 时：

    根据归纳假设，$(b_0,\ \sigma)$ 的求值是确定且终止的。

    因此，$\neg b_0$ 的求值也是确定且终止的。

  - 当 $b = b_0\ \&\ b_1$ 或 $b = b_0\ \text{or}\ b_1$ 时：

    根据归纳假设，$b_0$ 和 $b_1$ 的求值是确定且终止的。

    因此，组合后的布尔表达式的求值是确定且终止的。

**结论**：根据结构归纳法，布尔表达式的求值是确定的，且总是终止的。

---

**练习 3.6**

当你尝试通过对命令进行结构归纳来证明命令的执行是确定性的时，会出现什么问题？（稍后，在第 3.4 节中，我们将使用对推导的“结构归纳”给出一个证明。）

**解答**：

**问题**：命令的执行可能不会像表达式的求值那样是确定性的，特别是在存在非确定性或无限循环的情况下。

- 在 IMP 语言中，循环（如 $\text{while}$）的执行可能不会终止，因此命令的执行不一定总是确定的。
- 使用结构归纳法，无法有效地处理循环和递归的情况，因为命令的结构可能导致无限的执行过程。

因此，尝试对命令进行结构归纳来证明执行的确定性时，会遇到困难。

---

## 3.3 良基归纳法

数学归纳法和结构归纳法都是一个称为**良基归纳法（well-founded induction）**的通用而强大的证明原理的特例。本质上，结构归纳法之所以有效，是因为将一个表达式分解为子表达式的过程不可能无限进行，最终必须到达无法再分解的原子表达式。

如果一个性质在某个表达式上不成立，那么一定存在一个**最小的**使该性质不成立的表达式。当它被分解时，其子表达式都满足该性质。这个观察结果证明了结构归纳法的原理：要证明一个性质对所有表达式成立，只需证明如果该性质对一个表达式的所有子表达式成立，那么它对该表达式也成立。

类似地，对于自然数，如果一个性质不对所有自然数成立，那么必定存在最小的自然数使其不成立。

**定义**：一个**良基关系（well-founded relation）**是集合 $A$ 上的一个二元关系 $\prec$，满足不存在无限的下降链：

$$
\cdots\ \prec\ a_i\ \prec\ \cdots\ \prec\ a_1\ \prec\ a_0
$$

当 $a \prec b$ 时，我们称 $a$ 是 $b$ 的**前驱（predecessor）**。

**注意**：良基关系必定是**反自反的（irreflexive）**，即不存在 $a \prec a$，否则会有无限的下降链 $\cdots \prec a \prec a \prec a$。

通常，我们用 $\preceq$ 表示关系 $\prec$ 的**自反闭包（reflexive closure）**，即：

$$
a \preceq b \quad \Leftrightarrow \quad a = b \quad \text{或} \quad a \prec b
$$

有时，会看到良基关系的另一种定义，基于最小元素。

**命题 3.7**

设 $\prec$ 是集合 $A$ 上的一个二元关系。$\prec$ 是良基关系，当且仅当 $A$ 的任何非空子集 $Q$ 都有一个**最小元素（minimal element）**，即元素 $m$，使得：

$$
m \in Q\ \&\ \forall b \prec m.\ b \notin Q
$$

**证明**：

- **（$\Rightarrow$）**：

  假设每个非空子集 $Q$ 都有最小元素。如果存在无限下降链 $\cdots \prec a_i \prec \cdots \prec a_1 \prec a_0$，那么集合 $Q = \{ a_i\ |\ i \in \omega \}$ 是非空的，但没有最小元素，这与假设矛盾。因此，$\prec$ 是良基关系。

- **（$\Leftarrow$）**：

  假设 $\prec$ 是良基关系，$Q$ 是 $A$ 的非空子集。我们构造一个元素链如下：

  - 取 $a_0$ 为 $Q$ 中的任一元素。
  - 递归地，假设已经构造了 $a_n \prec \dots \prec a_0$，其中所有元素都在 $Q$ 中。
  - 如果不存在 $b \prec a_n$ 且 $b \in Q$，则停止构造。否则，取 $a_{n+1} = b$。

  由于 $\prec$ 是良基关系，链不可能无限延伸，最终会得到一个元素 $a_k$，使得 $\forall b \prec a_k.\ b \notin Q$。那么 $a_k$ 就是 $Q$ 的最小元素。

**结论**：因此，$\prec$ 是良基关系，当且仅当 $A$ 的任何非空子集都有最小元素。

---

**练习 3.8**

设 $\prec$ 是集合 $B$ 上的良基关系。证明：

1. 其传递闭包 $\prec^+$ 也是良基关系。

2. 其自反传递闭包 $\prec^*$ 是一个偏序关系。

**解答**：

1. **证明 $\prec^+$ 是良基关系**：

   - 假设存在 $\prec^+$ 上的无限下降链：

     $$
     \cdots\ \prec^+\ b_i\ \prec^+\ \cdots\ \prec^+\ b_1\ \prec^+\ b_0
     $$

   - 由于 $\prec^+$ 是 $\prec$ 的传递闭包，因此每个 $b_{i+1} \prec^+ b_i$ 可以分解为有限次 $\prec$ 关系的组合。

   - 将这些有限的 $\prec$ 关系拼接起来，得到一个 $\prec$ 上的无限下降链，与 $\prec$ 为良基关系矛盾。

   - 因此，$\prec^+$ 是良基关系。

2. **证明 $\prec^*$ 是偏序关系**：

   - $\prec^*$ 是自反的，因为对于任何 $a$，有 $a \prec^* a$。

   - $\prec^*$ 是传递的，因为如果 $a \prec^* b$ 且 $b \prec^* c$，则 $a \prec^* c$。

   - $\prec^*$ 是反对称的：如果 $a \prec^* b$ 且 $b \prec^* a$，则 $a = b$。否则，会有 $a \ne b$，且 $a$ 和 $b$ 之间存在 $\prec$ 的路径，导致 $\prec$ 上的循环，与良基关系矛盾。

   - 因此，$\prec^*$ 是偏序关系。

---

**良基归纳法的原理**

设 $\prec$ 是集合 $A$ 上的一个良基关系。设 $P$ 是一个性质。那么：

$$
\forall a \in A.\ P(a) \quad \Leftrightarrow \quad \forall a \in A.\ \left( \left[ \forall b \prec a.\ P(b) \right] \Rightarrow P(a) \right)
$$

**解释**：

- 该原理指出，为了证明一个性质对良基集合的所有元素成立，只需证明：对于任意元素 $a$，如果该性质对所有 $a$ 的前驱都成立，那么该性质对 $a$ 也成立。

**证明**：

- **方向一**：如果 $P(a)$ 对所有 $a$ 成立，那么显然对于任何 $a$，$\left[ \forall b \prec a.\ P(b) \right] \Rightarrow P(a)$。

- **方向二**：假设对于所有 $a$，$\left( \left[ \forall b \prec a.\ P(b) \right] \Rightarrow P(a) \right)$ 成立，但存在某个 $a$，使得 $P(a)$ 不成立。

  - 那么，集合 $F = \{ a \in A\ |\ \neg P(a) \}$ 非空。

  - 根据良基关系的性质，$F$ 有最小元素 $m$，满足 $\forall b \prec m.\ b \notin F$，即对所有 $b \prec m$，$P(b)$ 成立。

  - 但根据假设，$\left( \left[ \forall b \prec m.\ P(b) \right] \Rightarrow P(m) \right)$，因此 $P(m)$ 应该成立，与 $\neg P(m)$ 矛盾。

- **结论**：因此，$P(a)$ 对所有 $a \in A$ 成立。

---

**示例**：

- **数学归纳法**是良基归纳法的特例，其中关系 $\prec$ 是自然数上的后继关系 $n \prec m$ 当且仅当 $m = n + 1$。

- **完全归纳法**对应于 $\prec$ 为自然数上的“小于”关系 $<$。

- **结构归纳法**对应于表达式之间的关系 $\prec$，其中 $a \prec b$ 当且仅当 $a$ 是 $b$ 的直接子表达式。

---

**命题 3.7** 提供了一个替代良基归纳法的证明方法。假设 $A$ 是一个良基集合。为了证明 $A$ 的每个元素都满足性质 $P$，我们可以考虑 $A$ 的子集 $F$，其中 $P$ 不成立的元素组成的集合，即反例的集合。

为了证明 $F$ 为空，只需证明 $F$ 不可能有最小元素。这可以通过假设 $F$ 中存在最小元素，然后得出矛盾来完成。

**练习 3.9**

对于字符串上的合适的良基关系，使用上述“无反例”方法，证明不存在字符串 $u$ 满足 $au = ub$，其中 $a$ 和 $b$ 是两个不同的符号。将你的证明与使用良基归纳法的证明进行比较（以及与第 3.1 节中要求的数学归纳法的证明进行比较）。

**解答**：

- 定义关系 $\prec$：对于字符串 $s$ 和 $t$，$s \prec t$ 当且仅当 $s$ 是 $t$ 的真子串。

- 该关系是良基关系，因为字符串的长度有限，不能无限下降。

- 假设存在非空的字符串 $u$，使得 $au = ub$。

- 构造集合 $F$，包含所有满足 $au = ub$ 的最短字符串 $u$。

- 由于 $F$ 非空，存在最小长度的字符串 $u$，即 $u$ 是 $F$ 的最小元素。

- 考虑 $u$ 的第一个字符：

  - 如果 $u$ 为空，则 $a\varepsilon = \varepsilon b$，即 $a = b$，与 $a \ne b$ 矛盾。

  - 如果 $u$ 非空，设 $u = c v$，那么 $a c v = c v b$。

  - 因此，$a c v = c v b$，说明 $a = c$，并且 $a c v = a v b$，所以 $v$ 满足 $a v = v b$。

- 这意味着存在比 $u$ 更短的字符串 $v$，满足 $a v = v b$，与 $u$ 是最短字符串的假设矛盾。

- 因此，$F$ 中不存在最小元素，$F$ 为空。

**结论**：不存在字符串 $u$ 满足 $au = ub$。

---

**定理 3.10**

对于所有状态 $\sigma$，如果 $\sigma(M) \geq 1$ 且 $\sigma(N) \geq 1$，那么存在 $\sigma'$，使得 $(\text{Euclid},\ \sigma) \rightarrow \sigma'$。

**其中，$\text{Euclid}$ 算法如下**：

$$
\text{Euclid} :=\ \text{while}\ \neg (M = N)\ \text{do}\ \left( \text{if}\ M \leq N\ \text{then}\ N := N - M\ \text{else}\ M := M - N \right)
$$

**证明**：

**目标**：证明对于所有 $\sigma$，如果 $\sigma(M) \geq 1$ 且 $\sigma(N) \geq 1$，则存在 $\sigma'$，使得 $(\text{Euclid},\ \sigma) \rightarrow \sigma'$。

**方法**：使用良基归纳法，定义关系 $\prec$：

$$
\sigma' \prec \sigma \quad \Leftrightarrow \quad \sigma'(M) \leq \sigma(M)\ \&\ \sigma'(N) \leq \sigma(N)\ \&\ (\sigma'(M) \ne \sigma(M)\ \text{或}\ \sigma'(N) \ne \sigma(N))
$$

该关系是良基关系，因为 $M$ 和 $N$ 的值不能无限减少且保持正值。

**证明过程**：

- **基始情况**：当 $\sigma(M) = \sigma(N)$ 时：

  - 条件 $\neg (M = N)$ 在 $\sigma$ 下求值为 $\text{false}$。

  - 根据循环的求值规则，循环终止，得到 $(\text{Euclid},\ \sigma) \rightarrow \sigma$。

- **归纳步骤**：当 $\sigma(M) \ne \sigma(N)$ 时：

  - 条件 $\neg (M = N)$ 在 $\sigma$ 下求值为 $\text{true}$。

  - 执行条件语句：

    - 如果 $\sigma(M) \leq \sigma(N)$，则更新 $\sigma'' = \sigma[\sigma(N) - \sigma(M)/N]$。

    - 如果 $\sigma(N) < \sigma(M)$，则更新 $\sigma'' = \sigma[\sigma(M) - \sigma(N)/M]$。

  - 在两种情况下，$\sigma'' \prec \sigma$。

  - 根据归纳假设，存在 $\sigma'$，使得 $(\text{Euclid},\ \sigma'') \rightarrow \sigma'$。

  - 根据循环的求值规则，得到 $(\text{Euclid},\ \sigma) \rightarrow \sigma'$。

**结论**：根据良基归纳法，$\forall \sigma$，如果 $\sigma(M) \geq 1$ 且 $\sigma(N) \geq 1$，则存在 $\sigma'$，使得 $(\text{Euclid},\ \sigma) \rightarrow \sigma'$。

---

良基归纳法是证明程序终止性的重要原理。当程序中存在循环或递归时，终止性可能存在不确定性。如果可以证明程序中循环或递归的执行会在某个良基集合中降低值，那么它最终必须终止。

### ---------------------------

# 第3章 归纳法的一些原理

在证明程序性质的过程中，我们经常依赖于一种证明方法，或者说是一组证明方法，称为**归纳法（induction）**。最常用的归纳形式是**数学归纳法（mathematical induction）**和**结构归纳法（structural induction）**。这两者都是一种强大的证明方法的特例，称为**良基归纳法（well-founded induction）**。

---

## 3.4 关于推导的归纳法

单纯的结构归纳法在证明操作语义的性质时往往不够充分。我们经常需要对**推导的结构**进行归纳。这需要将上一章中遇到的一些想法形式化，从而建立一个坚实的基础。

可能的推导是通过规则来确定的。规则的实例形式如下：

- **公理**：

  $$
  \frac{}{\ x }
  $$

  这是一个没有前提的公理，其结论为 $x$。

- **规则实例**：

  $$
  \frac{ x_1,\ \dots,\ x_n }{ x }
  $$

  这里，$\{ x_1, \dots, x_n \}$ 是前提的集合，$x$ 是结论。

这些规则指定了如何构建推导，并通过这些推导来定义一个集合。规则定义的集合**恰好**由那些存在推导的元素组成。一个元素 $x$ 的推导形式是一个树形结构，要么是一个公理实例：

$$
\frac{}{\ x }
$$

要么是这样的形式：

$$
\frac{ x_1,\ \dots,\ x_n }{ x }
$$

其中包含对 $x_1, \dots, x_n$ 的推导，它们是结论为 $x$ 的规则实例的前提。在这样的推导中，我们认为这些对 $x_1, \dots, x_n$ 的推导是对 $x$ 的更大推导的**子推导**。

规则实例是通过将实际的项或值代入到规则中的元变量来获得的。我们感兴趣的所有规则都是**有限的（finitary）**，即它们的前提是有限的。因此，所有的规则实例都有一个有限的（可能为空）前提集合和一个结论。我们从规则实例的集合的概念开始对推导进行形式化。

一个**规则实例集合** $R$ 由元素组成，这些元素是形如 $(X\ /\ y)$ 的对，其中 $X$ 是一个有限集合，$y$ 是一个元素。这样的对 $(X\ /\ y)$ 称为一个规则实例，具有前提 $X$ 和结论 $y$。

我们更习惯于将规则实例 $(X\ /\ y)$ 写成：

$$
\frac{ X }{ y }
$$

当 $X = \{ x_1, \dots, x_n \}$ 时，表示为：

$$
\frac{ x_1,\ \dots,\ x_n }{ y }
$$

假设有一个规则实例集合 $R$。一个 $y$ 的 **$R$-推导**（$R$-derivation）要么是一个公理实例 $(\emptyset\ /\ y)$，要么是一个形如 $(\{ d_1, \dots, d_n \}\ /\ y)$ 的对，其中 $(\{ x_1, \dots, x_n \}\ /\ y)$ 是一个规则实例，且对于每个 $i$，$d_i$ 是 $x_i$ 的 $R$-推导。

我们用 $d\ \vdash_R\ y$ 来表示 $d$ 是 $y$ 的一个 $R$-推导。因此：

- 如果 $(\emptyset\ /\ y) \in R$，则有 $d = (\emptyset\ /\ y)$，且 $d\ \vdash_R\ y$。
- 如果 $(\{ x_1, \dots, x_n \}\ /\ y) \in R$，且对于所有 $i$，$d_i\ \vdash_R\ x_i$，那么 $d = (\{ d_1, \dots, d_n \}\ /\ y)$，且 $d\ \vdash_R\ y$。

我们说 $y$ 是从 $R$ **推导**出来的，如果存在 $y$ 的一个 $R$-推导 $d$，即 $d\ \vdash_R\ y$。我们写作 $\vdash_R\ y$ 来表示 $y$ 可从 $R$ 推导。当规则是明确的时，我们简写为 $d\ \vdash\ y$ 和 $\vdash\ y$。

在操作语义中，前提和结论是元组。例如：

$$
\vdash\ (c,\ \sigma)\ \rightarrow\ \sigma'
$$

表示 $(c,\ \sigma)\ \rightarrow\ \sigma'$ 可以从命令的操作语义中推导出来，通常只写作 $(c,\ \sigma)\ \rightarrow\ \sigma'$。理解到 $(c,\ \sigma)\ \rightarrow\ \sigma'$ 的含义包含了它是可推导的。当我们想强调存在一个推导时，我们才写作 $\vdash\ (c,\ \sigma)\ \rightarrow\ \sigma'$。

令 $d,\ d'$ 为推导。若 $d'$ 是 $d$ 的一个**直接子推导（immediate subderivation）**，记作 $d'\ \prec_1\ d$，当且仅当 $d$ 具有形式 $(D\ /\ y)$，且 $d' \in D$。记 $\prec$ 为 $\prec_1$ 的传递闭包，即 $\prec = (\prec_1)^+$。我们说 $d'$ 是 $d$ 的一个**真子推导（proper subderivation）**，当且仅当 $d'\ \prec\ d$。

由于推导是有限的，因此**直接子推导关系** $\prec_1$ 和**真子推导关系** $\prec$ 都是良基的（well-founded）。这个事实可用于证明命令的执行是确定性的。

### **定理 3.11**

设 $c$ 是一个命令，$\sigma_0$ 是一个状态。如果 $(c,\ \sigma_0)\ \rightarrow\ \sigma$ 且 $(c,\ \sigma_0)\ \rightarrow\ \sigma_1$，那么对于所有状态 $\sigma,\ \sigma_1$，都有 $\sigma = \sigma_1$。

**证明**：

**目标**：证明对于所有命令 $c$ 和状态 $\sigma_0$，如果 $(c,\ \sigma_0)\ \rightarrow\ \sigma$ 且 $(c,\ \sigma_0)\ \rightarrow\ \sigma_1$，那么 $\sigma = \sigma_1$。

**方法**：通过对推导的真子推导关系 $\prec$ 进行良基归纳，证明以下性质对所有推导 $d$ 成立：

$$
P(d) \quad \Leftrightarrow \quad \forall c \in \text{Com},\ \sigma_0,\ \sigma,\ \sigma_1 \in E.\ \left( d\ \vdash\ (c,\ \sigma_0)\ \rightarrow\ \sigma\ \&\ (c,\ \sigma_0)\ \rightarrow\ \sigma_1\ \Rightarrow\ \sigma = \sigma_1 \right)
$$

根据良基归纳法，只需证明：若对于所有 $d'\ \prec\ d$，$P(d')$ 成立，则 $P(d)$ 也成立。

**证明步骤**：

- **假设**：对于所有 $d'\ \prec\ d$，$P(d')$ 成立。
- **已知**：$d\ \vdash\ (c,\ \sigma_0)\ \rightarrow\ \sigma$，且 $(c,\ \sigma_0)\ \rightarrow\ \sigma_1$。
- **需要证明**：$\sigma = \sigma_1$。

**分类讨论命令 $c$ 的结构**：

1. **$c = \text{skip}$**：

   - 推导 $d$ 和 $d_1$ 都是：

     $$
     \frac{}{\ (\text{skip},\ \sigma_0)\ \rightarrow\ \sigma_0 }
     $$

     因此，$\sigma = \sigma_0 = \sigma_1$。

2. **$c = X := a$**：

   - 两个推导具有类似的形式：

     - 推导 $d$：

       $$
       \frac{ (a,\ \sigma_0)\ \rightarrow\ m }{ (X := a,\ \sigma_0)\ \rightarrow\ \sigma_0[m/X] }
       $$

     - 推导 $d_1$：

       $$
       \frac{ (a,\ \sigma_0)\ \rightarrow\ m_1 }{ (X := a,\ \sigma_0)\ \rightarrow\ \sigma_0[m_1/X] }
       $$

     - 因此，$\sigma = \sigma_0[m/X]$，$\sigma_1 = \sigma_0[m_1/X]$。

   - 由于算术表达式的求值是确定的（之前已证明过），所以 $m = m_1$。

   - 因此，$\sigma = \sigma_1$。

3. **$c = c_0;\ c_1$**（顺序组合）：

   - 推导 $d$：

     $$
     \frac{ (c_0,\ \sigma_0)\ \rightarrow\ \sigma' \quad (c_1,\ \sigma')\ \rightarrow\ \sigma }{ (c_0;\ c_1,\ \sigma_0)\ \rightarrow\ \sigma }
     $$

   - 推导 $d_1$：

     $$
     \frac{ (c_0,\ \sigma_0)\ \rightarrow\ \sigma'' \quad (c_1,\ \sigma'')\ \rightarrow\ \sigma_1 }{ (c_0;\ c_1,\ \sigma_0)\ \rightarrow\ \sigma_1 }
     $$

   - 由于 $d$ 和 $d_1$ 都以 $(c_0;\ c_1,\ \sigma_0)$ 为结论，因此我们有：

     - 对于 $c_0$ 的推导：

       - 从 $d$ 中的子推导 $d'$，有 $(c_0,\ \sigma_0)\ \rightarrow\ \sigma'$。

       - 从 $d_1$ 中的子推导 $d''$，有 $(c_0,\ \sigma_0)\ \rightarrow\ \sigma''$。

       - 由于 $d' \prec d$，$d'' \prec d_1$，而 $d_1$ 与 $d$ 的结论相同，因此 $d'' \prec d$。

       - 根据归纳假设 $P(d')$，有 $\sigma' = \sigma''$。

     - 对于 $c_1$ 的推导：

       - 在 $d$ 中，$(c_1,\ \sigma')\ \rightarrow\ \sigma$。

       - 在 $d_1$ 中，$(c_1,\ \sigma'')\ \rightarrow\ \sigma_1$。

       - 由于 $\sigma' = \sigma''$，且 $d'$ 和 $d''$ 是 $d$ 的子推导，根据归纳假设，$\sigma = \sigma_1$。

   - 因此，$\sigma = \sigma_1$。

4. **$c = \text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1$**（条件语句）：

   - 布尔表达式 $b$ 的求值是确定的（之前已证明过），所以要么 $(b,\ \sigma_0)\ \rightarrow\ \text{true}$，要么 $(b,\ \sigma_0)\ \rightarrow\ \text{false}$，但不会同时成立。

   - **情况 1**：$(b,\ \sigma_0)\ \rightarrow\ \text{true}$：

     - 推导 $d$：

       $$
       \frac{ (b,\ \sigma_0)\ \rightarrow\ \text{true} \quad (c_0,\ \sigma_0)\ \rightarrow\ \sigma }{ (\text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1,\ \sigma_0)\ \rightarrow\ \sigma }
       $$

     - 推导 $d_1$：

       $$
       \frac{ (b,\ \sigma_0)\ \rightarrow\ \text{true} \quad (c_0,\ \sigma_0)\ \rightarrow\ \sigma_1 }{ (\text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1,\ \sigma_0)\ \rightarrow\ \sigma_1 }
       $$

     - 对 $c_0$ 的推导是 $d'$，有 $d' \prec d$。

     - 根据归纳假设，$\sigma = \sigma_1$。

   - **情况 2**：$(b,\ \sigma_0)\ \rightarrow\ \text{false}$：

     - 类似地，证明 $\sigma = \sigma_1$。

5. **$c = \text{while}\ b\ \text{do}\ c$**（循环语句）：

   - 布尔表达式 $b$ 的求值是确定的。

   - **情况 1**：$(b,\ \sigma_0)\ \rightarrow\ \text{false}$：

     - 推导 $d$：

       $$
       \frac{ (b,\ \sigma_0)\ \rightarrow\ \text{false} }{ (\text{while}\ b\ \text{do}\ c,\ \sigma_0)\ \rightarrow\ \sigma_0 }
       $$

     - 因此，$\sigma = \sigma_0 = \sigma_1$。

   - **情况 2**：$(b,\ \sigma_0)\ \rightarrow\ \text{true}$：

     - 推导 $d$：

       $$
       \frac{ (b,\ \sigma_0)\ \rightarrow\ \text{true} \quad (c,\ \sigma_0)\ \rightarrow\ \sigma' \quad (\text{while}\ b\ \text{do}\ c,\ \sigma')\ \rightarrow\ \sigma }{ (\text{while}\ b\ \text{do}\ c,\ \sigma_0)\ \rightarrow\ \sigma }
       $$

     - 推导 $d_1$：

       $$
       \frac{ (b,\ \sigma_0)\ \rightarrow\ \text{true} \quad (c,\ \sigma_0)\ \rightarrow\ \sigma'' \quad (\text{while}\ b\ \text{do}\ c,\ \sigma'')\ \rightarrow\ \sigma_1 }{ (\text{while}\ b\ \text{do}\ c,\ \sigma_0)\ \rightarrow\ \sigma_1 }
       $$

     - 对 $c$ 的推导 $d'$，有 $d' \prec d$，根据归纳假设，$\sigma' = \sigma''$。

     - 对循环的推导 $d''$，有 $d'' \prec d$，根据归纳假设，$\sigma = \sigma_1$。

   - 因此，$\sigma = \sigma_1$。

**结论**：在所有情况下，我们都证明了 $d\ \vdash\ (c,\ \sigma_0)\ \rightarrow\ \sigma$ 且 $(c,\ \sigma_0)\ \rightarrow\ \sigma_1$，则 $\sigma = \sigma_1$。

因此，根据良基归纳法，$P(d)$ 对所有推导 $d$ 成立。这等价于：

$$
\forall c \in \text{Com},\ \sigma_0,\ \sigma,\ \sigma_1 \in E.\ \left( (c,\ \sigma_0)\ \rightarrow\ \sigma\ \&\ (c,\ \sigma_0)\ \rightarrow\ \sigma_1\ \Rightarrow\ \sigma = \sigma_1 \right)
$$

**证毕。**

---

正如之前所述，**命题 3.7** 提供了一种替代良基归纳法的证明方法。对推导进行归纳是一种特殊的良基归纳法，用于证明某个性质对所有推导都成立。相反，我们可以尝试从假设存在一个使性质不成立的最小推导出发，导出矛盾。下面的例子展示了这种方法：

### **命题 3.12**

对于所有状态 $\sigma,\ \sigma'$，都没有 $(\text{while}\ \text{true}\ \text{do}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma'$。

**证明**：

**目标**：证明对于所有状态 $\sigma,\ \sigma'$，$(\text{while}\ \text{true}\ \text{do}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma'$ 不存在。

**方法**：假设存在这样的 $\sigma'$，使得 $(w,\ \sigma)\ \rightarrow\ \sigma'$，其中 $w = \text{while}\ \text{true}\ \text{do}\ \text{skip}$。

假设存在一个最小的推导 $d$，使得存在 $\sigma,\ \sigma' \in E$，满足 $d\ \vdash\ (w,\ \sigma)\ \rightarrow\ \sigma'$。

根据循环的执行规则，推导 $d$ 必须具有以下形式：

$$
\frac{ (b,\ \sigma)\ \rightarrow\ \text{true} \quad (c,\ \sigma)\ \rightarrow\ \sigma'' \quad (\text{while}\ b\ \text{do}\ c,\ \sigma'')\ \rightarrow\ \sigma' }{ (\text{while}\ b\ \text{do}\ c,\ \sigma)\ \rightarrow\ \sigma' }
$$

其中 $b = \text{true}$，$c = \text{skip}$。

注意到在推导 $d$ 中包含了一个子推导 $d'$，即 $(w,\ \sigma'')\ \rightarrow\ \sigma'$。由于 $d'$ 是 $d$ 的真子推导，与 $d$ 是最小的假设矛盾。

**结论**：因此，对于所有 $\sigma,\ \sigma'$，不存在 $(w,\ \sigma)\ \rightarrow\ \sigma'$。

**证毕。**

---

## 3.5 通过归纳定义

像结构归纳法这样的技术通常用于定义集合上的运算。整数和算术表达式具有一个共同的特性，即它们是以唯一的方式构建起来的。一个整数要么是零，要么是某个唯一整数的后继；而一个算术表达式要么是原子表达式，要么是唯一一对表达式的和、差或积。正是由于它们是以唯一的方式构建的，我们可以通过对整数和表达式进行归纳来定义。

例如，要定义一个表达式的**长度（length）**，自然地可以根据其组成部分的长度来定义。对于算术表达式，我们可以定义：

- 对于数字 $n$ 和位置 $X$：

  $$
  \text{length}(n) = \text{length}(X) = 1
  $$

- 对于复合表达式：

  $$
  \text{length}(a_0 + a_1) = 1 + \text{length}(a_0) + \text{length}(a_1)
  $$

  类似地，对于减法和乘法进行定义。

为了将来参考，我们定义命令 $c$ 中出现在赋值左侧的位置的集合 $\text{loc}_L(c)$。对于命令 $c$，函数 $\text{loc}_L(c)$ 通过结构归纳定义如下：

- $\text{loc}_L(\text{skip}) = \emptyset$

- $\text{loc}_L(c_0;\ c_1) = \text{loc}_L(c_0) \cup \text{loc}_L(c_1)$

- $\text{loc}_L(\text{while}\ b\ \text{do}\ c) = \text{loc}_L(c)$

- $\text{loc}_L(X := a) = \{ X \}$

- $\text{loc}_L(\text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1) = \text{loc}_L(c_0) \cup \text{loc}_L(c_1)$

类似地，我们可以通过数学归纳法来定义自然数上的运算，以及通过规则给出的集合上的运算。实际上，**命题 3.7** 的证明，即每个非空的良基集合的子集都有一个最小元素，包含了一个对自然数进行隐式归纳定义的过程，以在非空集合中构造一个具有最小元素的链。

定义结构归纳和数学归纳都是良基归纳定义（也称为**良基递归（well-founded recursion）**）的特例。为了理解这个名称，请注意归纳定义和结构归纳都允许一种递归定义的形式。

例如，算术表达式的长度可以以这种方式定义：

$$
\text{length}(a) = \begin{cases}
1, & \text{如果 } a = n \text{ 或 } a = X \\
1 + \text{length}(a_0) + \text{length}(a_1), & \text{如果 } a = a_0 + a_1 \text{ 或其他复合形式}
\end{cases}
$$

对于特定的参数（如 $a_0 + a_1$），$\text{length}$ 函数的定义是基于它在其他参数（如 $a_0$ 和 $a_1$）上的作用。在这个意义上，$\text{length}$ 函数的定义是**递归地**以自身为基础定义的。然而，这种递归是以这样一种方式完成的：在特定参数上的值仅仅是根据严格更小的参数来指定的。

同样地，我们有权在任意良基集合上定义函数。一般的原理更难理解，因为它涉及一些相对复杂的集合构造，因此我们将其完整的讨论推迟到**第10A节**。（虽然在那之前不需要这些材料，但好奇或急切的读者可能会提前浏览。尽管它出现在较后的章节，但该节并不依赖于任何额外的概念。）

---

**练习 3.13**

通过结构归纳法，给出以下函数的定义：

- $\text{loc}(a)$：算术表达式 $a$ 中出现的位置的集合。

- $\text{loc}(b)$：布尔表达式 $b$ 中出现的位置的集合。

- $\text{loc}_R(c)$：命令 $c$ 中出现在赋值右侧的表达式中出现的位置的集合。

**解答**：

**对于算术表达式 $a$**，定义 $\text{loc}(a)$：

- **基础情况**：

  - 如果 $a$ 是数字 $n$，则 $\text{loc}(n) = \emptyset$。

  - 如果 $a$ 是位置 $X$，则 $\text{loc}(X) = \{ X \}$。

- **归纳步骤**：

  - 如果 $a = a_0 + a_1$，则 $\text{loc}(a) = \text{loc}(a_0) \cup \text{loc}(a_1)$。

  - 类似地，对于 $a = a_0 - a_1$ 和 $a = a_0 \times a_1$。

**对于布尔表达式 $b$**，定义 $\text{loc}(b)$：

- **基础情况**：

  - 如果 $b$ 是 $\text{true}$ 或 $\text{false}$，则 $\text{loc}(b) = \emptyset$。

- **归纳步骤**：

  - 如果 $b = a_0 = a_1$ 或 $b = a_0 \leq a_1$，则 $\text{loc}(b) = \text{loc}(a_0) \cup \text{loc}(a_1)$。

  - 如果 $b = \neg b_0$，则 $\text{loc}(b) = \text{loc}(b_0)$。

  - 如果 $b = b_0\ \&\ b_1$ 或 $b = b_0\ \text{or}\ b_1$，则 $\text{loc}(b) = \text{loc}(b_0) \cup \text{loc}(b_1)$。

**对于命令 $c$**，定义 $\text{loc}_R(c)$（赋值右侧出现的位置集合）：

- **基础情况**：

  - $\text{loc}_R(\text{skip}) = \emptyset$。

- **归纳步骤**：

  - 如果 $c = X := a$，则 $\text{loc}_R(c) = \text{loc}(a)$。

  - 如果 $c = c_0;\ c_1$，则 $\text{loc}_R(c) = \text{loc}_R(c_0) \cup \text{loc}_R(c_1)$。

  - 如果 $c = \text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1$，则 $\text{loc}_R(c) = \text{loc}(b) \cup \text{loc}_R(c_0) \cup \text{loc}_R(c_1)$。

  - 如果 $c = \text{while}\ b\ \text{do}\ c_0$，则 $\text{loc}_R(c) = \text{loc}(b) \cup \text{loc}_R(c_0)$。

---

## 3.6 进一步阅读

本章讨论的技术和思想是数学逻辑中众所周知的基本技术。由于操作语义遵循自然演绎（natural deduction）的思路，因此它与证明理论共享基本技术也就不足为奇了（例如，详见 [84]）——推导实际上是一种简单的证明。

对于具有计算机科学倾向的相当高级但易于理解的证明理论介绍，请参见 [51, 40]，其中包含了更多关于证明（以及推导）的表示法的内容。关于良基归纳法的进一步解释和应用，可以参见 [59] 和 [21]（在这些文献中，它被称为“结构归纳法”），以及 [58] 和 [73]，并在本书的**第10章**中进一步讨论。

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