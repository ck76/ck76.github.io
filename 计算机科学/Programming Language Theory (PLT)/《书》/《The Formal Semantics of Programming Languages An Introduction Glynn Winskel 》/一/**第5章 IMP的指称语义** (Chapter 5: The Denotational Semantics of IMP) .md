[toc]



# 第5章 IMP的指称语义

本章为 IMP（简单的命令式编程语言）提供了一个**指称语义（denotational semantics）**，并证明了它与之前给出的操作语义是等价的。本章最后介绍了指称语义的基础（完备偏序、连续函数和最小不动点）以及 Knaster-Tarski 定理。

## 5.1 动机

我们之前以**操作方式**描述了 IMP 程序的行为，通过归纳地定义转换关系来表示求值和执行。然而，在规则的选择上存在一些任意性，例如我们选择的转换步骤的大小。此外，在行为的描述中，语法与描述混杂在一起。

这种语义风格，在其中转换是由语法构建的，使得比较用不同编程语言编写的两个程序变得困难。尽管如此，这种语义风格相当接近于语言的实现，该描述可以转化为一个 IMP 的解释器，例如用 Prolog 编写的，并且它确立了算术表达式、布尔表达式和命令之间等价的严格定义。

例如，我们定义了命令 $c_0$ 和 $c_1$ 的等价性为：

$$
c_0\ \simeq\ c_1 \quad \text{当且仅当}\quad \forall \sigma,\ \sigma'.\ (c_0,\ \sigma)\ \rightarrow\ \sigma'\ \Leftrightarrow\ (c_1,\ \sigma)\ \rightarrow\ \sigma'
$$

也许你已经想到，如果我们只对命令的等价性 $\simeq$ 感兴趣，那么有一种更直接的方法来捕获 IMP 的语义。注意到 $c_0\ \simeq\ c_1$ 当且仅当：

$$
\{ (\sigma,\ \sigma')\ |\ (c_0,\ \sigma)\ \rightarrow\ \sigma' \} = \{ (\sigma,\ \sigma')\ |\ (c_1,\ \sigma)\ \rightarrow\ \sigma' \}
$$

换句话说，$c_0\ \simeq\ c_1$ 当且仅当 $c_0$ 和 $c_1$ 确定了相同的从状态到状态的部分函数（partial function）。这表明我们应该在更抽象的层面上定义 IMP 的含义或语义，我们将命令的指称（denotation）视为一个从状态到状态的部分函数。

我们采用的这种新的语义描述风格来自于**指称语义（denotational semantics）**。指称语义的适用范围比像 IMP 这样简单的编程语言要广泛得多——它几乎可以处理所有编程语言，尽管标准框架对于并行性和“公平性”似乎不足（参见第14章关于并行性的内容）。这种方法由 Christopher Strachey 开创，Dana Scott 提供了数学基础。我们对 IMP 的指称语义实际上只是一个入门示例。我们将在后续章节中看到更多关于指称语义的应用和基础。

- 一个算术表达式 $a \in \text{Aexp}$ 将表示一个函数 $A[a] : \Sigma \rightarrow \mathbb{N}$。
- 一个布尔表达式 $b \in \text{Bexp}$ 将表示一个函数 $B[b] : \Sigma \rightarrow T$，从状态的集合映射到真值的集合。
- 一个命令 $c$ 将表示一个部分函数 $C[c] : \Sigma \rightharpoonup \Sigma$。

**注意**：$\Sigma$ 是状态的集合，$T = \{ \text{true},\ \text{false} \}$。

方括号 $[\,]$ 在指称语义中是传统的表示法。你可以看到 $A$ 实际上是一个从算术表达式到函数的映射，即 $A : \text{Aexp} \rightarrow (\Sigma \rightarrow \mathbb{N})$。在普通数学中，当我们看到一个表达式时，我们的第一个想法是对其求值。方括号 $[a]$ 将算术表达式 $a$ 放在引号中，以便我们不对 $a$ 进行求值。

我们也可以写，例如，$A(\text{“3 + 5”})\ \sigma = 8$，而不是 $A[3 + 5]\ \sigma = 8$。引号表示我们正在映射的是语法片段“3 + 5”。真实情况稍微微妙一些，因为我们有时会写诸如 $A[a_0 + a_1]$ 这样的指称，其中 $a_0$ 和 $a_1$ 是元变量，代表算术表达式。将符号“+”放在语法对象 $a_0$ 和 $a_1$ 之间得到的语法对象被放在引号中。因此，方括号 $[\,]$ 并不代表真正和完整的引用。我们将在语义函数的参数周围使用方括号 $[\,]$，以表明参数是一个语法片段。

---

## 5.2 指称语义

我们通过**结构归纳（structural induction）**来定义以下语义函数：

- $A : \text{Aexp} \rightarrow (\Sigma \rightarrow \mathbb{N})$
- $B : \text{Bexp} \rightarrow (\Sigma \rightarrow T)$
- $C : \text{Com} \rightarrow (\Sigma \rightharpoonup \Sigma)$

例如，对于命令 $c$，我们在假设其子命令 $c'$ 已经定义了 $C[c']$ 的情况下，定义部分函数 $C[c]$。命令 $c$ 表示 $C[c]$，$C[c]$ 被称为 $c$ 的指称。

### **算术表达式的指称**

首先，我们通过结构归纳定义算术表达式的指称，作为状态和数字之间的关系：

1. 数字：

   $$
   A[n] = \{ (\sigma,\ n)\ |\ \sigma \in \Sigma \}
   $$

2. 位置（变量）：

   $$
   A[X] = \{ (\sigma,\ \sigma(X))\ |\ \sigma \in \Sigma \}
   $$

3. 加法：

   $$
   A[a_0 + a_1] = \{ (\sigma,\ n_0 + n_1)\ |\ (\sigma,\ n_0) \in A[a_0]\ \&\ (\sigma,\ n_1) \in A[a_1] \}
   $$

4. 减法：

   $$
   A[a_0 - a_1] = \{ (\sigma,\ n_0 - n_1)\ |\ (\sigma,\ n_0) \in A[a_0]\ \&\ (\sigma,\ n_1) \in A[a_1] \}
   $$

5. 乘法：

   $$
   A[a_0 \times a_1] = \{ (\sigma,\ n_0 \times n_1)\ |\ (\sigma,\ n_0) \in A[a_0]\ \&\ (\sigma,\ n_1) \in A[a_1] \}
   $$

显然，对算术表达式 $a$ 进行一个简单的结构归纳表明，每个指称 $A[a]$ 实际上是一个函数。

**注意**：左侧的符号“+”、“-”、“×”表示 IMP 中的语法符号，而右侧的符号表示数值运算。例如，对于任何状态 $\sigma$，

$$
A[3 + 5]\ \sigma = A[3]\ \sigma + A[5]\ \sigma = 3 + 5 = 8
$$

这是预期的结果。

使用 lambda 表达式，我们可以将语义的定义表示为以下等价的方式：

1. 数字：

   $$
   A[n] = \lambda \sigma \in \Sigma.\ n
   $$

2. 位置（变量）：

   $$
   A[X] = \lambda \sigma \in \Sigma.\ \sigma(X)
   $$

3. 加法：

   $$
   A[a_0 + a_1] = \lambda \sigma \in \Sigma.\ (A[a_0]\ \sigma + A[a_1]\ \sigma)
   $$

4. 减法：

   $$
   A[a_0 - a_1] = \lambda \sigma \in \Sigma.\ (A[a_0]\ \sigma - A[a_1]\ \sigma)
   $$

5. 乘法：

   $$
   A[a_0 \times a_1] = \lambda \sigma \in \Sigma.\ (A[a_0]\ \sigma \times A[a_1]\ \sigma)
   $$

---

### **布尔表达式的指称**

布尔语义函数 $B$ 是基于真值集合 $T = \{ \text{true},\ \text{false} \}$ 上的逻辑运算：合取（$\wedge_T$）、析取（$\vee_T$）和否定（$\neg_T$）给出的。布尔表达式的指称通过结构归纳定义为状态和真值之间的关系。

1. 常量 true：

   $$
   B[\text{true}] = \{ (\sigma,\ \text{true})\ |\ \sigma \in \Sigma \}
   $$

2. 常量 false：

   $$
   B[\text{false}] = \{ (\sigma,\ \text{false})\ |\ \sigma \in \Sigma \}
   $$

3. 等于（$=$）：

   $$
   \begin{aligned}
   B[a_0 = a_1] =\ & \{ (\sigma,\ \text{true})\ |\ \sigma \in \Sigma\ \&\ A[a_0]\ \sigma = A[a_1]\ \sigma \} \\
   \cup\ & \{ (\sigma,\ \text{false})\ |\ \sigma \in \Sigma\ \&\ A[a_0]\ \sigma \ne A[a_1]\ \sigma \}
   \end{aligned}
   $$

4. 小于等于（$\leq$）：

   $$
   \begin{aligned}
   B[a_0 \leq a_1] =\ & \{ (\sigma,\ \text{true})\ |\ \sigma \in \Sigma\ \&\ A[a_0]\ \sigma \leq A[a_1]\ \sigma \} \\
   \cup\ & \{ (\sigma,\ \text{false})\ |\ \sigma \in \Sigma\ \&\ A[a_0]\ \sigma > A[a_1]\ \sigma \}
   \end{aligned}
   $$

5. 否定（$\neg$）：

   $$
   B[\neg b] = \{ (\sigma,\ \neg_T t)\ |\ \sigma \in \Sigma\ \&\ (\sigma,\ t) \in B[b] \}
   $$

6. 合取（$\wedge$）：

   $$
   B[b_0\ \wedge\ b_1] = \{ (\sigma,\ t_0\ \wedge_T\ t_1)\ |\ \sigma \in \Sigma\ \&\ (\sigma,\ t_0) \in B[b_0]\ \&\ (\sigma,\ t_1) \in B[b_1] \}
   $$

---

显然，通过简单的结构归纳，每个指称都是一个函数。

例如，对于所有 $\sigma \in \Sigma$，有：

$$
B[a_0 \leq a_1]\ \sigma =
\begin{cases}
\text{true}, & \text{如果}\ A[a_0]\ \sigma \leq A[a_1]\ \sigma \\
\text{false}, & \text{如果}\ A[a_0]\ \sigma > A[a_1]\ \sigma
\end{cases}
$$

---

### **命令的指称**

命令 $c$ 的 $C[c]$ 的定义更为复杂。我们首先将指称定义为状态之间的关系；之后，通过一个直接的结构归纳将证明它们实际上是部分函数。

显然，我们应该定义：

1. **跳过（skip）**：

   $$
   C[\text{skip}] = \{ (\sigma,\ \sigma)\ |\ \sigma \in \Sigma \}
   $$

2. **赋值**：

   $$
   C[X := a] = \{ (\sigma,\ \sigma[n / X])\ |\ \sigma \in \Sigma\ \&\ n = A[a]\ \sigma \}
   $$

   **解释**：$\sigma[n / X]$ 表示将 $\sigma$ 中变量 $X$ 的值更新为 $n$ 的新状态。

3. **顺序组合**：

   $$
   C[c_0;\ c_1] = C[c_1] \circ C[c_0]
   $$

   **解释**：这里的 $\circ$ 表示关系的复合，定义如下：

   $$
   C[c_1] \circ C[c_0] = \{ (\sigma,\ \sigma')\ |\ \exists \sigma''.\ (\sigma,\ \sigma'') \in C[c_0]\ \&\ (\sigma'',\ \sigma') \in C[c_1] \}
   $$

   需要注意的是，$C[c_1]$ 在左边，$C[c_0]$ 在右边，这是因为复合关系的顺序。

4. **条件语句**：

   $$
   \begin{aligned}
   C[\text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1] =\ & \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{true}\ \&\ (\sigma,\ \sigma') \in C[c_0] \} \\
   \cup\ & \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{false}\ \&\ (\sigma,\ \sigma') \in C[c_1] \}
   \end{aligned}
   $$

---

然而，当我们考虑 while 循环的指称时，会遇到一些困难。

设 $w = \text{while}\ b\ \text{do}\ c$。

我们已经注意到等价性：

$$
w\ \simeq\ \text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip}
$$

因此，部分函数 $C[w]$ 应等于部分函数 $C[\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip}]$。

因此，我们应有：

$$
\begin{aligned}
C[w] =\ & \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{true}\ \&\ (\sigma,\ \sigma') \in C[c;\ w] \} \\
\cup\ & \{ (\sigma,\ \sigma)\ |\ B[b]\ \sigma = \text{false} \}
\end{aligned}
$$

进一步展开：

$$
C[w] = \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{true}\ \&\ (\sigma,\ \sigma') \in C[w] \circ C[c] \} \cup \{ (\sigma,\ \sigma)\ |\ B[b]\ \sigma = \text{false} \}
$$

这里出现了问题：$C[w]$ 在等式的两边都出现了。这是一个递归方程（“递归”的原因是我们在左边想知道的值在右边再次出现了）。我们如何解决它来找到 $C[w]$ 呢？

显然，我们需要一些技巧来解决这种形式的递归方程。

---

换一种方式，我们可以将函数 $f$ 定义为：

$$
f(\varphi) = \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{true}\ \&\ (\sigma,\ \sigma') \in \varphi \circ C[c] \} \cup \{ (\sigma,\ \sigma)\ |\ B[b]\ \sigma = \text{false} \}
$$

我们的目标是找到一个不动点 $\varphi$，使得：

$$
\varphi = f(\varphi)
$$

这个问题类似于上一章第 4.4 节中讨论的内容。

---

实际上，我们可以将 $f$ 视为由规则实例确定的算子 $R$，作用于集合上：

- 对于 $B[b]\ \sigma = \text{true}$，有规则实例：

  $$
  \frac{ (\sigma,\ \sigma'') }{ (\sigma,\ \sigma') }\quad \text{如果}\ (\sigma,\ \sigma'') \in C[c]\ \&\ (\sigma'',\ \sigma') \in \varphi
  $$

- 对于 $B[b]\ \sigma = \text{false}$，有规则实例：

  $$
  \frac{}{ (\sigma,\ \sigma) }\quad \text{如果}\ B[b]\ \sigma = \text{false}
  $$

因此，$f$ 可以看作是一个作用于集合上的算子 $R$，即：

$$
R(\varphi) = f(\varphi)
$$

根据第 4.4 节，我们知道 $R$ 有一个最小不动点 $\varphi = \text{fix}(R)$，具有性质 $\varphi = R(\varphi)$。

---

因此，我们将这个最小不动点作为 while 程序 $w$ 的指称。它的指称应该是一个不动点。对于将其作为最小不动点的选择的完整理由，我们将在下一节给出，在那里我们将证明这个语义选择与操作语义一致。

---

现在，我们可以按照以下方式，通过结构归纳来定义命令的指称：

1. **跳过（skip）**：

   $$
   C[\text{skip}] = \{ (\sigma,\ \sigma)\ |\ \sigma \in \Sigma \}
   $$

2. **赋值**：

   $$
   C[X := a] = \{ (\sigma,\ \sigma[n / X])\ |\ \sigma \in \Sigma\ \&\ n = A[a]\ \sigma \}
   $$

3. **条件语句**：

   $$
   \begin{aligned}
   C[\text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1] =\ & \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{true}\ \&\ (\sigma,\ \sigma') \in C[c_0] \} \\
   \cup\ & \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{false}\ \&\ (\sigma,\ \sigma') \in C[c_1] \}
   \end{aligned}
   $$

4. **顺序组合**：

   $$
   C[c_0;\ c_1] = C[c_1] \circ C[c_0]
   $$

5. **while 循环**：

   $$
   C[\text{while}\ b\ \text{do}\ c] = \text{fix}(f)
   $$

   其中，$f$ 是函数：

   $$
   f(\varphi) = \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{true}\ \&\ (\sigma,\ \sigma') \in \varphi \circ C[c] \} \cup \{ (\sigma,\ \sigma)\ |\ B[b]\ \sigma = \text{false} \}
   $$

通过这种方式，我们将每个命令的指称定义为状态之间的关系。

**注意**：语义定义是**可组合的（compositional）**，也就是说，命令的指称是由其直接子命令的指称构造的，这反映在定义是通过结构归纳的这一事实中。这种性质是指称语义的标志。

**然而**，对于 IMP 的操作语义来说，这并不成立，因为在 while 循环的规则中，while 循环在规则的前提中再次出现了。

---

我们基于 while 程序的操作等价性来定义语义，将其展开为条件语句。不出所料，根据指称语义，验证这个等价性是直接的。

### **命题 5.1**

设 $w = \text{while}\ b\ \text{do}\ c$，对于一个命令 $c$ 和布尔表达式 $b$。那么：

$$
C[w] = C[\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip}]
$$

**证明**：

- 我们知道 $C[w]$ 是 $f$ 的不动点。
- 因此：

  $$
  \begin{aligned}
  C[w] &= f(C[w]) \\
  &= \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{true}\ \&\ (\sigma,\ \sigma') \in C[w] \circ C[c] \} \cup \{ (\sigma,\ \sigma)\ |\ B[b]\ \sigma = \text{false} \} \\
  &= \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{true}\ \&\ (\sigma,\ \sigma') \in C[c;\ w] \} \cup \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{false}\ \&\ (\sigma,\ \sigma') \in C[\text{skip}] \} \\
  &= C[\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip}]
  \end{aligned}
  $$

**证毕。**

---

**练习 5.2**

通过对命令的结构归纳，证明对于所有命令 $c$，指称 $C[c]$ 是一个从状态到状态的部分函数。

**提示**：对于 while 循环的情况，涉及到数学归纳的证明，证明对于所有自然数 $n$，$f^n(\emptyset)$ 是状态之间的部分函数，并且这些部分函数形成一个递增的链，随后观察到这样一个部分函数的链的并集本身也是一个部分函数。

**解答**：

**目标**：证明对于所有命令 $c$，$C[c]$ 是一个部分函数，即对于任何 $\sigma \in \Sigma$，至多存在一个 $\sigma'$，使得 $(\sigma,\ \sigma') \in C[c]$。

**方法**：对命令 $c$ 进行结构归纳。对于每种命令形式，证明 $C[c]$ 是部分函数。

1. **基础情况**：

   - **跳过（skip）**：

     $$
     C[\text{skip}] = \{ (\sigma,\ \sigma)\ |\ \sigma \in \Sigma \}
     $$

     显然，对于每个 $\sigma$，$(\sigma,\ \sigma)$ 唯一，因此 $C[\text{skip}]$ 是部分函数。

   - **赋值（$X := a$）**：

     $$
     C[X := a] = \{ (\sigma,\ \sigma[n / X])\ |\ \sigma \in \Sigma\ \&\ n = A[a]\ \sigma \}
     $$

     对于每个 $\sigma$，$n = A[a]\ \sigma$ 是唯一的（因为 $A[a]$ 是一个函数），因此 $\sigma[n / X]$ 是唯一的。因此，$C[X := a]$ 是部分函数。

2. **归纳步骤**：

   - **顺序组合（$c_0;\ c_1$）**：

     假设 $C[c_0]$ 和 $C[c_1]$ 是部分函数。

     $$
     C[c_0;\ c_1] = C[c_1] \circ C[c_0]
     $$

     我们需要证明复合函数 $C[c_1] \circ C[c_0]$ 是部分函数。

     - 对于每个 $\sigma$，如果 $C[c_0]$ 和 $C[c_1]$ 都是部分函数，那么对于 $\sigma$，$C[c_0]$ 中最多有一个 $\sigma''$，使得 $(\sigma,\ \sigma'') \in C[c_0]$。

     - 然后，对于这个 $\sigma''$，$C[c_1]$ 中至多有一个 $\sigma'$，使得 $(\sigma'',\ \sigma') \in C[c_1]$。

     - 因此，对于每个 $\sigma$，至多存在一个 $\sigma'$，使得 $(\sigma,\ \sigma') \in C[c_0;\ c_1]$。

     - 所以，$C[c_0;\ c_1]$ 是部分函数。

   - **条件语句（$\text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1$）**：

     - 假设 $C[c_0]$ 和 $C[c_1]$ 是部分函数。

     - 对于每个 $\sigma$，$B[b]\ \sigma$ 要么是 $\text{true}$，要么是 $\text{false}$。

     - 如果 $B[b]\ \sigma = \text{true}$，则 $(\sigma,\ \sigma') \in C[c_0]$。

       - 由于 $C[c_0]$ 是部分函数，对于 $\sigma$，$\sigma'$ 是唯一的。

     - 如果 $B[b]\ \sigma = \text{false}$，则 $(\sigma,\ \sigma') \in C[c_1]$。

       - 由于 $C[c_1]$ 是部分函数，对于 $\sigma$，$\sigma'$ 是唯一的。

     - 因此，$C[\text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1]$ 是部分函数。

   - **while 循环（$\text{while}\ b\ \text{do}\ c$）**：

     - 我们需要证明 $C[\text{while}\ b\ \text{do}\ c]$ 是部分函数。

     - 定义 $f$ 为：

       $$
       f(\varphi) = \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{true}\ \&\ (\sigma,\ \sigma') \in \varphi \circ C[c] \} \cup \{ (\sigma,\ \sigma)\ |\ B[b]\ \sigma = \text{false} \}
       $$

     - 定义序列：

       $$
       \begin{aligned}
       \varphi_0 &= \emptyset \\
       \varphi_{n+1} &= f(\varphi_n)
       \end{aligned}
       $$

     - 证明对于所有 $n$，$\varphi_n$ 是部分函数。

       - **基始情况**：$\varphi_0 = \emptyset$，显然是部分函数。

       - **归纳假设**：假设 $\varphi_n$ 是部分函数。

       - **证明 $\varphi_{n+1}$ 是部分函数**：

         - 对于每个 $\sigma$，如果 $B[b]\ \sigma = \text{false}$，则 $(\sigma,\ \sigma) \in \varphi_{n+1}$。

           - $\sigma'$ 唯一。

         - 如果 $B[b]\ \sigma = \text{true}$，则：

           - $(\sigma,\ \sigma') \in \varphi_{n+1}$，当且仅当存在 $\sigma''$，使得 $(\sigma,\ \sigma'') \in C[c]$，且 $(\sigma'',\ \sigma') \in \varphi_n$。

           - 由于 $C[c]$ 和 $\varphi_n$ 都是部分函数，因此对于 $\sigma$，$\sigma'$ 是唯一的。

         - 因此，$\varphi_{n+1}$ 是部分函数。

     - 由于 $\varphi_n$ 是递增的部分函数序列，其并集 $\varphi = \bigcup_{n} \varphi_n$ 也是部分函数。

     - 因此，$C[\text{while}\ b\ \text{do}\ c] = \varphi$ 是部分函数。

---

## 5.3 语义的等价性

尽管受到我们对 IMP 操作行为的理解的启发，但指称语义尚未被证明与操作语义一致。我们首先检查表达式的操作语义和指称语义是否一致。

### **引理 5.3**

对于所有算术表达式 $a \in \text{Aexp}$，有：

$$
A[a] = \{ (\sigma,\ n)\ |\ (a,\ \sigma) \rightarrow n \}
$$

**证明**：

我们通过对算术表达式的结构归纳来证明此引理。作为归纳假设，我们设：

$$
P(a) \quad \Leftrightarrow \quad A[a] = \{ (\sigma,\ n)\ |\ (a,\ \sigma) \rightarrow n \}
$$

按照结构归纳的方案，证明分为算术表达式 $a$ 的结构的不同情况。

1. **$a = n$**（数字）：

   - 根据语义函数的定义，当 $a$ 是一个数字 $n$ 时：

     $$
     (\sigma,\ m) \in A[n] \quad \Leftrightarrow \quad \sigma \in \Sigma\ \&\ m = n
     $$

   - 根据操作语义的规则，对于 $(n,\ \sigma) \rightarrow m$，唯一可能的推导是 $m = n$。

   - 因此，$A[n] = \{ (\sigma,\ n)\ |\ \sigma \in \Sigma \ \&\ (n,\ \sigma) \rightarrow n \}$。

2. **$a = X$**（位置/变量）：

   - 根据定义：

     $$
     (\sigma,\ n) \in A[X] \quad \Leftrightarrow \quad \sigma \in \Sigma\ \&\ n = \sigma(X)
     $$

   - 根据操作语义，有 $(X,\ \sigma) \rightarrow \sigma(X)$。

   - 因此，$A[X] = \{ (\sigma,\ \sigma(X))\ |\ \sigma \in \Sigma \ \&\ (X,\ \sigma) \rightarrow \sigma(X) \}$。

3. **$a = a_0 + a_1$**（加法）：

   - 假设 $P(a_0)$ 和 $P(a_1)$ 成立。

   - 根据定义：

     $$
     (\sigma,\ n) \in A[a_0 + a_1] \quad \Leftrightarrow \quad \exists n_0,\ n_1.\ n = n_0 + n_1\ \&\ (\sigma,\ n_0) \in A[a_0]\ \&\ (\sigma,\ n_1) \in A[a_1]
     $$

   - 根据归纳假设，$(\sigma,\ n_0) \in A[a_0] \Leftrightarrow (a_0,\ \sigma) \rightarrow n_0$。

   - 类似地，$(\sigma,\ n_1) \in A[a_1] \Leftrightarrow (a_1,\ \sigma) \rightarrow n_1$。

   - 因此，有 $(a_0 + a_1,\ \sigma) \rightarrow n$，其中 $n = n_0 + n_1$。

   - 反过来，任何 $(a_0 + a_1,\ \sigma) \rightarrow n$ 的推导必须具有形式：

     $$
     \frac{ (a_0,\ \sigma) \rightarrow n_0 \quad (a_1,\ \sigma) \rightarrow n_1 }{ (a_0 + a_1,\ \sigma) \rightarrow n }
     $$

     其中 $n = n_0 + n_1$。

   - 根据归纳假设，$(\sigma,\ n_0) \in A[a_0]$，$(\sigma,\ n_1) \in A[a_1]$。

   - 因此，$(\sigma,\ n) \in A[a_0 + a_1]$。

4. **其他运算（减法、乘法）**：

   - 证明方式与加法相同。

因此，通过对算术表达式的结构归纳，我们得出：

$$
A[a] = \{ (\sigma,\ n)\ |\ (a,\ \sigma) \rightarrow n \}
$$

对于所有算术表达式 $a$。

**证毕。**

---

### **引理 5.4**

对于所有布尔表达式 $b \in \text{Bexp}$，有：

$$
B[b] = \{ (\sigma,\ t)\ |\ (b,\ \sigma) \rightarrow t \}
$$

**证明**：

对布尔表达式的证明与对算术表达式的证明类似。我们通过对布尔表达式的结构归纳进行证明，归纳假设为：

$$
P(b) \quad \Leftrightarrow \quad B[b] = \{ (\sigma,\ t)\ |\ (b,\ \sigma) \rightarrow t \}
$$

我们只证明两个典型的情况，其他情况类似。

1. **$b = a_0 = a_1$**（相等比较）：

   - 根据定义：

     $$
     B[a_0 = a_1] = \{ (\sigma,\ \text{true})\ |\ A[a_0]\ \sigma = A[a_1]\ \sigma \} \cup \{ (\sigma,\ \text{false})\ |\ A[a_0]\ \sigma \ne A[a_1]\ \sigma \}
     $$

   - 根据引理 5.3，$A[a_0]\ \sigma = n_0$，当且仅当 $(a_0,\ \sigma) \rightarrow n_0$。

   - 因此，$(\sigma,\ \text{true}) \in B[a_0 = a_1]$ 当且仅当存在 $n$，使得 $(a_0,\ \sigma) \rightarrow n$，$(a_1,\ \sigma) \rightarrow n$。

   - 根据操作语义，有：

     $$
     \frac{ (a_0,\ \sigma) \rightarrow n \quad (a_1,\ \sigma) \rightarrow n }{ (a_0 = a_1,\ \sigma) \rightarrow \text{true} }
     $$

   - 因此，$B[a_0 = a_1] = \{ (\sigma,\ t)\ |\ (a_0 = a_1,\ \sigma) \rightarrow t \}$。

2. **$b = b_0\ \wedge\ b_1$**（合取）：

   - 假设 $P(b_0)$ 和 $P(b_1)$ 成立。

   - 根据定义：

     $$
     (\sigma,\ t) \in B[b_0\ \wedge\ b_1] \quad \Leftrightarrow \quad \exists t_0,\ t_1.\ t = t_0\ \wedge_T\ t_1\ \&\ (\sigma,\ t_0) \in B[b_0]\ \&\ (\sigma,\ t_1) \in B[b_1]
     $$

   - 根据归纳假设，$(\sigma,\ t_0) \in B[b_0] \Leftrightarrow (b_0,\ \sigma) \rightarrow t_0$。

   - 类似地，$(\sigma,\ t_1) \in B[b_1] \Leftrightarrow (b_1,\ \sigma) \rightarrow t_1$。

   - 根据操作语义，有：

     $$
     \frac{ (b_0,\ \sigma) \rightarrow t_0 \quad (b_1,\ \sigma) \rightarrow t_1 }{ (b_0\ \wedge\ b_1,\ \sigma) \rightarrow t }
     $$

     其中 $t = t_0\ \wedge_T\ t_1$。

   - 因此，$B[b_0\ \wedge\ b_1] = \{ (\sigma,\ t)\ |\ (b_0\ \wedge\ b_1,\ \sigma) \rightarrow t \}$。

**其他情况**类似，留给读者作为练习。

**证毕。**

---

**练习 5.5**

上述证明涉及到考虑推导的形式。可以通过结构归纳和规则归纳的组合获得替代的证明。例如，证明对于所有算术表达式 $a$：

1. $\{ (\sigma,\ n)\ |\ (a,\ \sigma) \rightarrow n \} \subseteq A[a]$

2. $A[a] \subseteq \{ (\sigma,\ n)\ |\ (a,\ \sigma) \rightarrow n \}$

**提示**：

- 对于 (1)，使用算术表达式的操作语义的规则归纳。
- 对于 (2)，对算术表达式进行结构归纳。

---

**现在，我们可以检查命令的指称语义与其操作语义是否一致。**

### **引理 5.6**

对于所有命令 $c$ 和状态 $\sigma,\ \sigma'$，有：

$$
(c,\ \sigma) \rightarrow \sigma' \quad \Rightarrow \quad (\sigma,\ \sigma') \in C[c]
$$

**证明**：

我们使用命令的操作语义的规则归纳，如第 4.3.3 节所述。对于 $c \in \text{Com}$ 和 $\sigma,\ \sigma' \in \Sigma$，定义：

$$
P(c,\ \sigma,\ \sigma') \quad \Leftrightarrow \quad (\sigma,\ \sigma') \in C[c]
$$

如果我们能证明 $P$ 在命令执行的规则下是封闭的，那么对于任何命令 $c$ 和状态 $\sigma,\ \sigma'$，有：

$$
(c,\ \sigma) \rightarrow \sigma' \quad \Rightarrow \quad P(c,\ \sigma,\ \sigma')
$$

我们只检查第 4.3.3 节中与 while 循环相关的一个规则，即条件求值为 true 的情况。回想该规则：

$$
\frac{ (b,\ \sigma) \rightarrow \text{true} \quad (c,\ \sigma) \rightarrow \sigma'' \quad (w,\ \sigma'') \rightarrow \sigma' }{ (w,\ \sigma) \rightarrow \sigma' }
$$

其中 $w = \text{while}\ b\ \text{do}\ c$。

按照第 4.3.3 节的方案，假设：

- $(b,\ \sigma) \rightarrow \text{true}$
- $(c,\ \sigma) \rightarrow \sigma''$
- $P(c,\ \sigma,\ \sigma'')$
- $(w,\ \sigma'') \rightarrow \sigma'$
- $P(w,\ \sigma'',\ \sigma')$

根据引理 5.4，有 $B[b]\ \sigma = \text{true}$。

根据 $P$ 的定义，我们直接得到：

- $C[c]\ \sigma = \sigma''$
- $C[w]\ \sigma'' = \sigma'$

根据指称语义的定义，我们有：

$$
C[w]\ \sigma = C[w]\ (C[c]\ \sigma) = C[w]\ \sigma'' = \sigma'
$$

因此，$C[w]\ \sigma = \sigma'$，即 $P(w,\ \sigma,\ \sigma')$ 成立。

因此，$P$ 在此规则下是封闭的。通过类似的论证，$P$ 在命令执行的其他规则下也是封闭的。

因此，根据规则归纳，我们证明了引理。

**证毕。**

---

接下来的定理，通过结构归纳和在一个情况下（while 循环）的数学归纳的使用，证明了命令的操作语义和指称语义的等价性。

### **定理 5.7**

对于所有命令 $c$，有：

$$
C[c] = \{ (\sigma,\ \sigma')\ |\ (c,\ \sigma) \rightarrow \sigma' \}
$$

**证明**：

该定理可以表述为：对于所有命令 $c$，所有状态 $\sigma,\ \sigma'$，有：

$$
(\sigma,\ \sigma') \in C[c] \quad \Leftrightarrow \quad (c,\ \sigma) \rightarrow \sigma'
$$

注意，引理 5.6 给出了“$\Rightarrow$”方向的等价性。

我们对命令 $c$ 进行结构归纳，归纳假设为：

$$
\forall \sigma,\ \sigma' \in \Sigma.\ (\sigma,\ \sigma') \in C[c] \quad \Leftrightarrow \quad (c,\ \sigma) \rightarrow \sigma'
$$

1. **$c = \text{skip}$**：

   - $C[\text{skip}] = \{ (\sigma,\ \sigma)\ |\ \sigma \in \Sigma \}$。

   - 因此，如果 $(\sigma,\ \sigma') \in C[\text{skip}]$，则 $\sigma' = \sigma$。

   - 根据操作语义的规则，有 $(\text{skip},\ \sigma) \rightarrow \sigma$。

   - 归纳假设成立。

2. **$c = X := a$**：

   - 如果 $(\sigma,\ \sigma') \in C[X := a]$，则 $\sigma' = \sigma[n / X]$，其中 $n = A[a]\ \sigma$。

   - 根据引理 5.3，有 $(a,\ \sigma) \rightarrow n$。

   - 因此，根据操作语义，有 $(X := a,\ \sigma) \rightarrow \sigma'$。

   - 归纳假设成立。

3. **$c = c_0;\ c_1$**：

   - 假设归纳假设对 $c_0$ 和 $c_1$ 成立。

   - 如果 $(\sigma,\ \sigma') \in C[c_0;\ c_1]$，则存在 $\sigma''$，使得 $(\sigma,\ \sigma'') \in C[c_0]$，$(\sigma'',\ \sigma') \in C[c_1]$。

   - 根据归纳假设，有 $(c_0,\ \sigma) \rightarrow \sigma''$，$(c_1,\ \sigma'') \rightarrow \sigma'$。

   - 根据操作语义的规则，有 $(c_0;\ c_1,\ \sigma) \rightarrow \sigma'$。

   - 归纳假设成立。

4. **$c = \text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1$**：

   - 假设归纳假设对 $c_0$ 和 $c_1$ 成立。

   - 如果 $(\sigma,\ \sigma') \in C[c]$，则要么：

     - (i) $B[b]\ \sigma = \text{true}$，且 $(\sigma,\ \sigma') \in C[c_0]$。

       - 根据引理 5.4，有 $(b,\ \sigma) \rightarrow \text{true}$。

       - 根据归纳假设，有 $(c_0,\ \sigma) \rightarrow \sigma'$。

       - 因此，根据操作语义，有 $(c,\ \sigma) \rightarrow \sigma'$。

     - (ii) $B[b]\ \sigma = \text{false}$，且 $(\sigma,\ \sigma') \in C[c_1]$。

       - 类似地，可以得出 $(c,\ \sigma) \rightarrow \sigma'$。

   - 归纳假设成立。

5. **$c = \text{while}\ b\ \text{do}\ c_0$**：

   - 假设归纳假设对 $c_0$ 成立。

   - 记 $f$ 为：

     $$
     f(\varphi) = \{ (\sigma,\ \sigma')\ |\ B[b]\ \sigma = \text{true}\ \&\ (\sigma,\ \sigma') \in \varphi \circ C[c_0] \} \cup \{ (\sigma,\ \sigma)\ |\ B[b]\ \sigma = \text{false} \}
     $$

   - 设 $\varphi_n = f^n(\emptyset)$，则：

     $$
     C[c] = \bigcup_{n \in \mathbb{N}} \varphi_n
     $$

   - 我们将通过数学归纳证明：

     $$
     \forall n \in \mathbb{N},\ \forall \sigma,\ \sigma' \in \Sigma,\ (\sigma,\ \sigma') \in \varphi_n\ \Rightarrow\ (\text{while}\ b\ \text{do}\ c_0,\ \sigma) \rightarrow \sigma'
     $$

     **基始情况**（$n = 0$）：

     - $\varphi_0 = \emptyset$，因此命题平凡成立。

     **归纳步骤**：

     - 假设对于 $n$，命题成立。

     - 对于 $n+1$，假设 $(\sigma,\ \sigma') \in \varphi_{n+1}$。

       - 则要么：

         - (i) $B[b]\ \sigma = \text{true}$，且存在 $\sigma''$，使得 $(\sigma,\ \sigma'') \in C[c_0]$，$(\sigma'',\ \sigma') \in \varphi_n$。

           - 根据引理 5.4，有 $(b,\ \sigma) \rightarrow \text{true}$。

           - 根据归纳假设，有 $(\text{while}\ b\ \text{do}\ c_0,\ \sigma'') \rightarrow \sigma'$。

           - 根据归纳假设对 $c_0$ 的成立，有 $(c_0,\ \sigma) \rightarrow \sigma''$。

           - 因此，根据操作语义的规则，有 $(\text{while}\ b\ \text{do}\ c_0,\ \sigma) \rightarrow \sigma'$。

         - (ii) $B[b]\ \sigma = \text{false}$，且 $\sigma' = \sigma$。

           - 根据引理 5.4，有 $(b,\ \sigma) \rightarrow \text{false}$。

           - 因此，根据操作语义，有 $(\text{while}\ b\ \text{do}\ c_0,\ \sigma) \rightarrow \sigma$。

     - 因此，命题对 $n+1$ 成立。

   - 由数学归纳法，我们得出，对于所有 $n$，命题成立。

   - 因此，对于所有 $\sigma,\ \sigma'$，有：

     $$
     (\sigma,\ \sigma') \in C[c] \quad \Rightarrow \quad (\text{while}\ b\ \text{do}\ c_0,\ \sigma) \rightarrow \sigma'
     $$

   - 归纳假设成立。

**最终**，通过结构归纳，我们证明了定理。

**证毕。**

---

**练习 5.8**

设 $w = \text{while}\ b\ \text{do}\ c$。证明：

$$
C[w]\ \sigma = \sigma' \quad \Leftrightarrow \quad \left( B[b]\ \sigma = \text{false}\ \&\ \sigma = \sigma' \right) \quad \text{或} \quad \left( \exists \sigma_0, \sigma_1, \dots, \sigma_n.\ \sigma = \sigma_0\ \&\ \sigma' = \sigma_n\ \&\ B[b]\ \sigma_n = \text{false}\ \&\ \forall i (0 \leq i < n).\ B[b]\ \sigma_i = \text{true}\ \&\ C[c]\ \sigma_i = \sigma_{i+1} \right)
$$

**提示**：

- 从左到右的证明使用在构建 $w$ 的指称中使用的 $f^n(\emptyset)$ 上的归纳。
- 从右到左的证明使用对状态链长度的归纳。

---

**练习 5.9**

一个具有 repeat 构造的简单命令式语言的命令语法如下：

$$
c ::= X := e\ |\ c_0;\ c_1\ |\ \text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1\ |\ \text{repeat}\ c\ \text{until}\ b
$$

其中 $X$ 是一个位置，$e$ 是一个算术表达式，$b$ 是一个布尔表达式，$c,\ c_0,\ c_1$ 是命令。

根据你对这些命令行为的理解，解释如何将 while 程序的语义改为 repeat 程序的语义，以给出：

1. **操作语义**：以规则的形式生成形如 $(c,\ \sigma) \rightarrow \sigma'$ 的转换，表示从状态 $\sigma$ 开始执行命令 $c$，最终在状态 $\sigma'$ 终止。

2. **指称语义**：对于每个命令 $c$，定义一个从状态到状态的部分函数 $C[c]$。

3. **证明**：概述操作语义和指称语义之间的等价性，即 $(c,\ \sigma) \rightarrow \sigma'$ 当且仅当 $C[c]\ \sigma = \sigma'$，重点关注 $c$ 是 repeat 循环的情况。

---

**解答**：

**1. 操作语义**

- 为 repeat 循环添加规则：

  $$
  \frac{ (c,\ \sigma) \rightarrow \sigma'' \quad (b,\ \sigma'') \rightarrow \text{false} }{ (\text{repeat}\ c\ \text{until}\ b,\ \sigma) \rightarrow \sigma' }
  $$

  **注意**：repeat 循环至少执行一次，然后在布尔表达式 $b$ 为 true 时继续循环。

- 当 $b$ 在执行 $c$ 后为 false 时，repeat 循环终止。

- 当 $b$ 为 true 时，继续执行 repeat 循环。

**2. 指称语义**

- 定义 $C[\text{repeat}\ c\ \text{until}\ b]$ 为最小不动点 $\varphi$，满足：

  $$
  \varphi(\sigma) = \begin{cases}
  \varphi(C[c](\sigma)), & \text{如果}\ B[b](C[c](\sigma)) = \text{false} \\
  C[c](\sigma), & \text{如果}\ B[b](C[c](\sigma)) = \text{true}
  \end{cases}
  $$

- 可以将其定义为一个函数，类似于 while 循环的指称。

**3. 等价性的证明**

- 需要证明对于所有 $\sigma,\ \sigma'$，有：

  $$
  (\text{repeat}\ c\ \text{until}\ b,\ \sigma) \rightarrow \sigma' \quad \Leftrightarrow \quad C[\text{repeat}\ c\ \text{until}\ b]\ \sigma = \sigma'
  $$

- 重点关注 $c$ 是 repeat 循环的情况。

- 可以使用归纳法，证明对于执行次数 $n$，指称语义和操作语义一致。

---

### ---------------------------

# 第5章 IMP的指称语义（续）

## 5.4 完备偏序集和连续函数

在上一章中，我们给出了归纳定义理论的初步介绍，并展示了如何用它来为 IMP 提供指称语义。在实践中，很少有递归定义可以简单地视为集合上的算子的不动点，它们最好使用完备偏序集（complete partial orders）和连续函数（continuous functions）的更抽象概念来处理，这些是指称语义的标准工具。我们可以从归纳定义的框架来接近这个方法。通过这种方式，希望使完备偏序集的更抽象概念更易于理解，并展示它们与操作语义中更具体的概念之间的紧密联系。

假设我们有一组规则实例 $R$，形式为 $(X\ /\ y)$。我们已经看到 $R$ 如何确定一个作用于集合上的算子 $R$，对于给定的集合 $B$，结果为：

$$
R(B) = \{ y\ |\ \exists (X\ /\ y) \in R,\ X \subseteq B \}
$$

并且算子 $R$ 有一个最小不动点：

$$
\text{fix}(R) = \bigcup_{n \in \omega} R^n(\emptyset)
$$

通过取集合链的并集构成：

$$
\emptyset \subseteq R(\emptyset) \subseteq \dots \subseteq R^n(\emptyset) \subseteq \dots
$$

它是一个不动点，因为：

$$
R(\text{fix}(R)) = \text{fix}(R)
$$

并且它是最小的不动点，因为 $\text{fix}(R)$ 包含在任何不动点 $B$ 中，即：

$$
R(B) = B\ \Rightarrow\ \text{fix}(R) \subseteq B
$$

实际上，上一章第 4.12 命题表明，$\text{fix}(R)$ 是最小的 $R$-封闭集，其中 $R$-封闭集可以描述为满足 $R(B) \subseteq B$ 的集合 $B$。

通过选择适当的规则实例 $R$，我们可以得到 while 循环的指称所需的递归方程的解。然而，更一般地说，我们可以从上述示例中提取我们用来获得最小不动点的基本数学性质。这引出了完备偏序集和连续函数的概念。

“最小”的概念之所以有意义，是因为存在包含关系或子集关系。取而代之的是，我们采用更一般的偏序（partial order）概念。

### **偏序的定义**

一个**偏序**（partial order，简称 p.o.）是一个集合 $P$，其上有一个二元关系 $\leq$，满足：

1. **自反性**（reflexive）：对所有 $p \in P$，有 $p \leq p$。
2. **传递性**（transitive）：对所有 $p, q, r \in P$，如果 $p \leq q$ 且 $q \leq r$，则 $p \leq r$。
3. **反对称性**（antisymmetric）：对所有 $p, q \in P$，如果 $p \leq q$ 且 $q \leq p$，则 $p = q$。

然而，并非所有的偏序都支持我们在集合上所做的构造。在构造最小不动点时，我们形成了一个从 $\emptyset$ 开始的 $\omega$-链的并集 $\bigcup_{n} A_n$，其中 $A_0 \subseteq A_1 \subseteq \dots$。

在偏序集上，将集合的并运算推广为**上确界（least upper bound，lub）**的概念，我们只需要它们对由 $\omega$ 索引的递增链存在。

将这些性质翻译到偏序集中，我们得到完备偏序集的定义。

### **上确界的定义**

对于一个偏序 $(P, \leq)$ 和一个子集 $X \subseteq P$，如果 $p$ 满足：

- 对所有 $q \in X$，都有 $q \leq p$。

则称 $p$ 是 $X$ 的一个**上界（upper bound）**。

如果 $p$ 满足：

1. $p$ 是 $X$ 的上界，且
2. 对于 $X$ 的所有上界 $q$，都有 $p \leq q$。

则称 $p$ 是 $X$ 的**最小上界（least upper bound，lub）**。

当偏序的一个子集 $X$ 有最小上界时，我们将其记为 $\bigvee X$。对于有限集合，$\bigvee \{ d_1, \dots, d_m \}$ 可写作 $d_1 \vee \dots \vee d_m$。

### **完备偏序集的定义**

设 $(D, \leq_D)$ 是一个偏序集。

- 一个偏序集的**$\omega$-链**是元素的递增序列 $d_0 \leq_D d_1 \leq_D \dots \leq_D d_n \leq_D \dots$。
- 如果一个偏序集 $(D, \leq_D)$ 对所有 $\omega$-链都有上确界，则称其为一个**完备偏序集（complete partial order，简称 cpo）**。
- 如果完备偏序集 $(D, \leq_D)$ 有一个最小元素 $\bot_D$（称为“底”或“最小元素”），则称其为**有底的完备偏序集**。

**注意**：在后续内容中，我们通常将偏序集的排序 $\leq_D$ 简写为 $\leq$，其最小元素（如果存在）简写为 $\bot$。上下文通常会清楚地表明我们指的是哪个偏序集。

请注意，任何由恒等关系排序的集合都形成了一个完备偏序集，但通常没有最小元素。这样的完备偏序集被称为**离散的（discrete）**或**平坦的（flat）**。

**练习 5.10**

- **(1)** 证明对于任意集合 $X$，幂集 $\text{Pow}(X)$（即 $X$ 的所有子集的集合）与包含关系 $\subseteq$ 形成一个有底的完备偏序集。
  
  **解答**：

  - $\text{Pow}(X)$ 的所有 $\omega$-链都是子集序列，包含关系是偏序关系。
  - $\text{Pow}(X)$ 中的任意子集链都有最小上界（即并集）。
  - 空集 $\emptyset$ 是最小元素。
  - 因此，$(\text{Pow}(X),\ \subseteq)$ 是一个有底的完备偏序集。

- **(2)** 证明从 $\Sigma$ 到 $\Sigma$ 的部分函数集合 $\Sigma \rightharpoonup \Sigma$，与包含关系 $\subseteq$ 形成一个有底的完备偏序集。

  **解答**：

  - 部分函数可以视为图（即输入输出对的集合），包含关系是图的包含。
  - 部分函数的并集仍然是部分函数（如果没有冲突）。
  - 空函数是最小元素。
  - 因此，$(\Sigma \rightharpoonup \Sigma,\ \subseteq)$ 是一个有底的完备偏序集。

---

集合上操作的对应物是一个从 cpo $D$ 返回到自身的函数 $f : D \rightarrow D$。我们需要这样的函数以某种方式尊重 $D$ 上的排序。

为了激励这些性质，我们考虑从规则实例 $R$ 定义的算子。

假设

$$
B_0 = \emptyset,\quad B_{n+1} = R(B_n)
$$

那么

$$
B_0 \subseteq B_1 \subseteq \dots \subseteq B_n \subseteq \dots
$$

也是一个集合的递增链。这是因为 $R$ 是**单调的（monotonic）**，即对于 $B \subseteq C$，有 $R(B) \subseteq R(C)$。

由于单调性，当 $B_n \subseteq \bigcup_{n} B_n$ 时，有：

$$
\bigcup_{n} R(B_n) \subseteq R\left( \bigcup_{n} B_n \right)
$$

事实上，由于规则实例的有限性，反向包含也成立，因此实际上是等号。假设 $y \in R\left( \bigcup_{n} B_n \right)$，那么存在 $(X\ /\ y) \in R$，其中 $X \subseteq \bigcup_{n} B_n$。由于 $X$ 是有限的，$X \subseteq B_n$ 对于某个 $n$。因此，$y \in R(B_n)$。因此，

$$
R\left( \bigcup_{n} B_n \right) = \bigcup_{n} R(B_n)
$$

我们已经证明了 $R$ 是**连续的（continuous）**，即对于任何递增链 $B_0 \subseteq B_1 \subseteq \dots$，有：

$$
R\left( \bigcup_{n} B_n \right) = \bigcup_{n} R(B_n)
$$

这是因为规则是有限的，即每个规则实例 $(X\ /\ y)$ 只涉及有限的前提集合 $X$。

我们可以采用这些性质来定义 cpo 之间的连续函数。

### **单调函数和连续函数的定义**

- 一个从 cpo $D$ 到 cpo $E$ 的函数 $f : D \rightarrow E$ 是**单调的（monotonic）**，如果对于所有 $d, d' \in D$，有：

  $$
  d \leq d'\ \Rightarrow\ f(d) \leq f(d')
  $$

- 如果 $f$ 是单调的，并且对于 $D$ 中的所有链 $d_0 \leq d_1 \leq \dots \leq d_n \leq \dots$，有：

  $$
  f\left( \bigvee_{n} d_n \right) = \bigvee_{n} f(d_n)
  $$

  则称 $f$ 是**连续的（continuous）**。

这个定义的一个重要结果是，任何从有底的 cpo 到自身的连续函数都有一个最小不动点，这一般化了第 4.4 节中集合上算子的情况。

实际上，我们可以用序理论的**前不动点（prefixed point）**概念来捕获封闭集的概念（回想一下，当且仅当 $R(B) \subseteq B$ 时，集合 $B$ 在规则实例 $R$ 下是封闭的）。

### **不动点的定义**

- 对于 cpo 上的连续函数 $f : D \rightarrow D$，如果 $d \in D$ 满足 $f(d) = d$，则称 $d$ 是 $f$ 的**不动点（fixed point）**。
- 如果 $d$ 满足 $f(d) \leq d$，则称 $d$ 是 $f$ 的**前不动点（prefixed point）**。

下面这个简单但重要的定理给出了连续函数 $f$ 的最小不动点 $\text{fix}(f)$ 的显式构造。

### **定理 5.11（不动点定理）**

设 $f : D \rightarrow D$ 是有底的 cpo $D$ 上的一个连续函数。定义：

$$
\text{fix}(f) = \bigvee_{n} f^n(\bot)
$$

则 $\text{fix}(f)$ 是 $f$ 的不动点，且是 $f$ 的最小前不动点，即：

1. $f(\text{fix}(f)) = \text{fix}(f)$；
2. 如果 $f(d) \leq d$，则 $\text{fix}(f) \leq d$。

因此，$\text{fix}(f)$ 是 $f$ 的最小不动点。

**证明**：

1. **证明 $f(\text{fix}(f)) = \text{fix}(f)$**：

   - 由于 $f$ 的连续性，有：

     $$
     f(\text{fix}(f)) = f\left( \bigvee_{n} f^n(\bot) \right) = \bigvee_{n} f\left( f^n(\bot) \right) = \bigvee_{n} f^{n+1}(\bot) = \text{fix}(f)
     $$

   - 因此，$\text{fix}(f)$ 是 $f$ 的不动点。

2. **证明如果 $f(d) \leq d$，则 $\text{fix}(f) \leq d$**：

   - 由于 $\bot \leq d$。
   - 由于 $f$ 的单调性，有 $f(\bot) \leq f(d)$。
   - 由于 $f(d) \leq d$，所以 $f(\bot) \leq d$。
   - 通过归纳，可得 $f^n(\bot) \leq d$ 对所有 $n$ 成立。
   - 因此，$\text{fix}(f) = \bigvee_{n} f^n(\bot) \leq d$。

由于不动点必定是前不动点，因此 $\text{fix}(f)$ 是 $f$ 的最小不动点。

**证毕。**

---

我们稍微讨论一下完备偏序集和连续函数背后的直觉，这种直觉将在后面的章节中得到进一步讨论和更精确的阐述。完备偏序集对应于数据的类型，可以用作计算的输入或输出。可计算的函数被建模为它们之间的连续函数。

cpo 的元素被视为信息的点，排序 $x \leq y$ 表示 $x$ **近似于（approximates）** $y$（或者说，$x$ 包含的信息比 $y$ 少或相同），因此 $\bot$ 是信息最少的点。

我们可以将之前给 IMP 提供指称语义的方法重新表述为这个通用框架。我们将一个命令表示为从状态到状态的部分函数 $\Sigma \rightharpoonup \Sigma$。表面上看，这与命令计算的函数应该是连续的想法并不一致。然而，状态上的部分函数可以被视为连续的全函数。

我们通过添加一个新的元素 $\bot$ 将状态 $\Sigma$ 扩展为一个结果的 cpo $\Sigma_\bot$，其排序为：

$$
\bot \leq \sigma,\quad \forall \sigma \in \Sigma
$$

cpo $\Sigma_\bot$ 包含了额外的元素 $\bot$，表示未定义的状态，或者更准确地说，是关于状态的空信息，随着计算的进行，可以增长为确定的最终状态的信息。

不难看出，从 $\Sigma$ 到 $\Sigma$ 的部分函数与从 $\Sigma$ 到 $\Sigma_\bot$ 的（总的）函数一一对应，并且在这种情况下，任何全函数都是连续的。部分函数之间的包含关系对应于函数 $\Sigma \rightarrow \Sigma_\bot$ 之间的**逐点（pointwise）**排序：

$$
f \leq g \quad \Leftrightarrow \quad \forall \sigma \in \Sigma,\ f(\sigma) \leq g(\sigma)
$$

由于部分函数形成了一个 cpo，因此函数空间 $[\Sigma \rightarrow \Sigma_\bot]$（带有逐点排序）也是一个 cpo。因此，我们的指称语义可以等价地被视为将命令表示为 cpo $[\Sigma \rightarrow \Sigma_\bot]$ 的元素。

回想一下，为了给 while 程序的指称，我们通过在部分函数的 cpo 上取连续函数的最小不动点来求解递归方程，这现在重新表述为在 cpo $[\Sigma \rightarrow \Sigma_\bot]$ 上进行。

对于与部分函数同构的 cpo $[\Sigma \rightarrow \Sigma_\bot]$，更多的信息对应于函数的更多输入/输出行为，而 cpo 中的无信息（$\bot$）对应于不包含任何输入/输出对的空部分函数。我们可以将这些函数本身视为可以被计算使用或产生的数据。

注意，对这些函数的信息是以离散的单元（输入/输出对）的形式出现的。这样的离散性是许多在建模计算中出现的完备偏序集所共有的。

正如我们将看到的，可计算函数应该是连续的，这源于这样一种想法：可计算函数的输出中的一个信息单元的出现只应依赖于输入中有限多个信息单元的存在。否则，函数的计算在产生该输出单元之前必须使用无限多的信息单元。

我们以前已经遇到过这个想法；一组规则实例在规则实例是有限的情况下确定了一个连续算子，因为它们只有有限的前提集合。

**练习 5.12**

1. **(i)** 证明从 $\Sigma$ 到 $\Sigma_\bot$ 的单调映射与从 $\Sigma$ 到 $\Sigma$ 的部分函数一一对应，并且它们是连续的。确认上述声明：一个部分函数包含在另一个部分函数中，当且仅当对应的函数 $\Sigma \rightarrow \Sigma_\bot$ 在逐点排序下满足 $f \leq g$。

   **解答**：

   - 从 $\Sigma$ 到 $\Sigma$ 的部分函数可以扩展为从 $\Sigma$ 到 $\Sigma_\bot$ 的全函数，其中未定义的值对应于 $\bot$。
   - 单调性：如果 $f$ 是部分函数，则对应的全函数 $f'$ 满足 $\sigma \leq \sigma'$（总是成立），因此 $f(\sigma) \leq f'(\sigma')$。
   - 逐点包含：$f \subseteq g$ 当且仅当对所有 $\sigma$，$f(\sigma)$ 定义时，$g(\sigma)$ 也定义，且 $f(\sigma) = g(\sigma)$。
   - 因此，部分函数之间的包含关系对应于全函数之间的逐点排序。

2. **(ii)** 设 $D$ 和 $E$ 是 cpo。如果 $D$ 有这样的性质：每个 $\omega$-链 $d_0 \leq d_1 \leq \dots$ 是**稳定的（stationary）**，即存在 $n$，使得对于所有 $m \geq n$，$d_m = d_n$。证明从 $D$ 到 $E$ 的所有单调函数都是连续的。

   **解答**：

   - 对于 $D$ 中的任意 $\omega$-链 $\{ d_n \}$，由于它是稳定的，存在 $n$，使得 $\bigvee_{n} d_n = d_n$。
   - 对于单调函数 $f$，有：

     $$
     f\left( \bigvee_{n} d_n \right) = f(d_n) = \bigvee_{n} f(d_n)
     $$

   - 因此，$f$ 是连续的。

---

**练习 5.13**

如果我们放宽规则必须是有限的条件，允许具有无限前提的规则实例，那么由一组规则实例引出的算子可能不再是连续的。

**解答**：

- 当规则实例有无限多个前提时，算子可能不再满足：

  $$
  R\left( \bigcup_{n} B_n \right) = \bigcup_{n} R(B_n)
  $$

- 因此，$R$ 可能不是连续的。

---

## 5.5 Knaster-Tarski 定理

本节研究不动点的另一个抽象刻画。其结果在后面才会用到，所以第一次阅读时可以跳过。

回顾上一章，对于一个算子的最小不动点，还有另一种刻画。回想第 4.1 节的练习 4.3，对于一组规则实例 $R$，有：

$$
I_R = \bigcap \{ Q\ |\ Q\ \text{是}\ R\text{-封闭的} \}
$$

根据第 4.4 节的内容，这可以重新表述为：

$$
\text{fix}(R) = \bigcap \{ Q\ |\ R(Q) \subseteq Q \}
$$

这表达了算子 $R$ 的最小不动点可以刻画为其前不动点的交集。这是 Knaster-Tarski 定理的特例，一个关于不动点存在性的通用结果。

正如预期的那样，它的陈述涉及到将集合上的交运算推广到偏序集上的一个与最小上界对偶的概念。

### **下确界的定义**

对于一个偏序 $(P, \leq)$ 和一个子集 $X \subseteq P$，如果 $p$ 满足：

- 对所有 $q \in X$，都有 $p \leq q$。

则称 $p$ 是 $X$ 的**下界（lower bound）**。

如果 $p$ 满足：

1. $p$ 是 $X$ 的下界，且
2. 对于 $X$ 的所有下界 $q$，都有 $q \leq p$。

则称 $p$ 是 $X$ 的**最大下界（greatest lower bound，glb）**。

当偏序的一个子集 $X$ 有最大下界时，我们将其记为 $\bigwedge X$。对于有限集合，$\bigwedge \{ d_0, d_1 \}$ 可写作 $d_0 \wedge d_1$。

最小上界有时被称为**上确界（supremum）**，最大下界有时被称为**下确界（infimum）**。

### **完全格的定义**

如果一个偏序集对于任意子集都有最大下界，则称其为**完全格（complete lattice）**。

虽然我们选择将完全格定义为具有所有子集的最大下界的偏序集，但我们也可以将其定义为具有所有子集的最小上界的偏序集，这可以从以下练习得出。

**练习 5.14**

证明完全格也必须具有任意子集的最小上界。推论：如果 $(L, \leq)$ 是一个完全格，那么其逆序偏序 $(L, \geq)$ 也是一个完全格。

**解答**：

- 对于完全格 $(L, \leq)$，任意子集 $X$ 都有最大下界 $\bigwedge X$。
- 由于偏序的对偶性质，$\bigvee X$ 在 $(L, \geq)$ 中对应于 $\bigwedge X$ 在 $(L, \leq)$ 中。
- 因此，$(L, \geq)$ 也是一个完全格。

---

### **定理 5.15（Knaster-Tarski 最小不动点定理）**

设 $(L, \leq)$ 是一个完全格。令 $f : L \rightarrow L$ 是一个单调函数，即如果 $x \leq y$，则 $f(x) \leq f(y)$（但不一定是连续的）。定义：

$$
m = \bigwedge \{ x \in L\ |\ f(x) \leq x \}
$$

则 $m$ 是 $f$ 的不动点，且是 $f$ 的最小前不动点。

**证明**：

- 设 $X = \{ x \in L\ |\ f(x) \leq x \}$。
- 因为 $m = \bigwedge X$，所以对于所有 $x \in X$，有 $m \leq x$。
- 由于 $f$ 是单调的，对于所有 $x \in X$，有 $f(m) \leq f(x)$。
- 由于 $x \in X$，有 $f(x) \leq x$，因此 $f(m) \leq x$ 对所有 $x \in X$ 成立。
- 因此，$f(m) \leq m$，所以 $m$ 是 $f$ 的前不动点。
- 由于 $m$ 是 $X$ 的最大下界，显然是最小的前不动点。
- 由于 $f(m) \leq m$，且 $m \leq f(m)$（因为 $f(m) \in X$），所以 $f(m) = m$。
- 因此，$m$ 是 $f$ 的不动点。

**证毕。**

---

作为推论，我们可以证明单调函数在完全格上有最大的不动点。

### **定理 5.16（Knaster-Tarski 最大不动点定理）**

设 $(L, \leq)$ 是一个完全格。令 $f : L \rightarrow L$ 是一个单调函数。定义：

$$
M = \bigvee \{ x \in L\ |\ x \leq f(x) \}
$$

则 $M$ 是 $f$ 的不动点，且是 $f$ 的最大后不动点（postfixed point，即满足 $x \leq f(x)$ 的元素）。

**证明**：

- 这可从最小不动点的定理推出，注意到在偏序 $(L, \leq)$ 上的单调函数也是在完全格 $(L, \geq)$ 上的单调函数。

**证毕。**

---

Knaster-Tarski 定理很重要，因为它适用于完全格上的任何单调函数。然而，大多数时候，我们将关注连续函数的最小不动点，我们将通过上一节的技术来构造它们，作为 cpo 中的 $\omega$-链的最小上界。

---

## 5.6 进一步阅读

本章给出了指称语义的一个示例。后续章节将扩展指称方法的范围和能力。更多的初步材料可以在以下书籍中找到：

- Bird [21]
- Loeckx 和 Sieber [58]
- Schmidt [88]
- Stay [95]（尽管后者基于完全格而非完备偏序集）

一本更难但非常深入的书是 de Bakker [13]。

IMP 的指称语义是以牺牲规则为代价的更抽象的不动点使用而获得的。然而，也有收获。通过将其意义置于 cpo 和连续函数的框架内，IMP 可以适用于那里的技术。书 [69] 有几个关于 while 程序语言的应用示例。

---

**总结**：

在本节中，我们深入探讨了指称语义的数学基础，引入了完备偏序集和连续函数的概念，并讨论了不动点定理和 Knaster-Tarski 定理。这些工具对于理解和构造编程语言的指称语义至关重要，尤其是在处理递归定义和循环结构时。

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