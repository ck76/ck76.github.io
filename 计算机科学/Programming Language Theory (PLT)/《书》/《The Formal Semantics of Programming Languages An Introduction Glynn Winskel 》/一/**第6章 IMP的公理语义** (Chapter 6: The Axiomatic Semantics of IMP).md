[toc]



# 第6章 IMP的公理语义

在本章中，我们将转向系统地验证 IMP（一个简单的命令式编程语言）中的程序。我们将介绍 Hoare 规则，用于证明程序的部分正确性，并证明这些规则是正确的。这涉及到将布尔表达式扩展为关于程序状态的丰富的断言语言。最后，本章将以一个在 Hoare 规则框架内进行的验证示例作为结尾。

## 6.1 思想

我们来考虑如何证明我们在 IMP 中编写的程序能够实现我们所需要的功能。

让我们从一个简单的例子开始：计算前一百个自然数的和，即 $1 + 2 + 3 + \dots + 100$。这是一个朴素的方式来计算 $\sum_{1 \leq m \leq 100} m$。以下是在 IMP 中编写的程序：

```
S := 0;
N := 1;
while ¬(N = 101) do
    S := S + N;
    N := N + 1
```

我们希望证明，当这个程序终止时，$S$ 的值就是 $\sum_{1 \leq m \leq 100} m$。

当然，我们可以根据操作语义运行它，看看结果是否正确。但是，如果我们稍微修改一下程序，将 `"while ¬(N = 101) do"` 改为 `"while ¬(N = P + 1) do"`，并在开始之前对 $P$ 进行任意赋值。在这种情况下，无论 $P$ 的值是什么，执行后的 $S$ 的结果都应该是 $\sum_{1 \leq m \leq P} m$。

由于 $P$ 可以取无限多个值，我们无法通过对所有初始值的 $P$ 运行程序来验证这一事实。我们需要更加聪明和抽象一些，利用一些逻辑来推理程序。

最终，我们将建立一个用于证明 IMP 程序属性的形式化证明系统，基于 IMP 的每个编程构造的证明规则。其规则被称为 **Hoare 规则** 或 **Floyd-Hoare 规则**。历史上，R.W. Floyd 发明了用于推理流程图的规则，后来 C.A.R. Hoare 修改和扩展了这些规则，为类似于 IMP 但带有过程的语言提供了处理方法。最初，他们的方法不仅被提倡用于证明程序的属性，而且还作为一种解释程序构造含义的方法；构造的含义是根据关于如何证明其属性的“公理”（更准确地说是规则）来指定的。因此，这种方法传统上被称为 **公理语义**。

现在，让我们不要过于形式化。让我们看看程序，并根据我们对其行为的直观理解来非正式地推理。我们立即看到，命令 `S := 0; N := 1` 初始化了位置中的值。因此，我们可以在程序中添加一个注释：

```
S := 0; N := 1
{S = 0 ∧ N = 1}
while ¬(N = 101) do
    S := S + N;
    N := N + 1
```

这里，`S = 0` 表示位置 $S$ 的值为 $0$，与布尔表达式的处理方式相同。我们希望有一种方法来证明以下带有最终注释的程序是正确的：

```
S := 0; N := 1
{S = 0 ∧ N = 1}
while ¬(N = 101) do
    S := S + N;
    N := N + 1
{S = ∑_{1 ≤ m ≤ 100} m}
```

这意味着，如果在执行 `while` 循环之前，`S = 0 ∧ N = 1`，那么在执行之后，`S = ∑_{1 ≤ m ≤ 100} m`。

观察布尔条件，我们知道在 `while` 循环执行之后，`N` 的值不能不是 `101`；因为如果 `¬(N = 101)`，那么 `while` 循环将继续运行。因此，在执行结束时，我们知道 `N = 101`。但我们想知道 `S` 的值！

当然，对于这样一个简单的程序，我们可以观察到第一次循环时 `S = 1`，`N = 2`。第二次循环时 `S = 1 + 2`，`N = 3`，以此类推。我们可以看到一个模式：在第 $i$ 次循环后，`S = 1 + 2 + ... + i`，而 `N = i + 1`。由此可见，当我们退出循环时，`S = 1 + 2 + ... + 100`，因为当我们退出时，`N = 101`。

在 `while` 循环的每次迭代的开始和结束时，我们都有：

$$
S = 1 + 2 + 3 + \dots + (N - 1) \tag{1}
$$

这表达了位置 $S$ 的值和位置 $N$ 的值之间的关键关系。这个断言 $I$ 被称为 **循环不变式**，因为它在循环的每次迭代中都保持为真。因此，最终当循环终止时，$I$ 将在结束时成立。我们将在后面进一步讨论不变式。

现在看来，我们可以基于如下形式的断言建立一个证明系统：

$$
\{A\}\ c\ \{B\}
$$

其中 $A$ 和 $B$ 是我们已经看到的类似的断言，$c$ 是一个命令。

这样的复合断言的精确定义如下：

**对于所有满足前置条件 $A$ 的状态 $\sigma$，如果从状态 $\sigma$ 开始执行命令 $c$ 并在状态 $\sigma'$ 终止，那么 $\sigma'$ 满足后置条件 $B$。**

换句话说，$\{A\}\ c\ \{B\}$ 的含义是：从满足 $A$ 的状态开始成功（即终止）的任何 $c$ 的执行，最终都会在满足 $B$ 的状态结束。断言 $A$ 被称为 **前置条件**，$B$ 被称为 **后置条件**，这样的断言 $\{A\}\ c\ \{B\}$ 被称为 **部分正确性断言**。

部分正确性断言 $\{A\}\ c\ \{B\}$ 只关注命令 $c$ 的终止情况，如果 $c$ 未能终止，它们不作任何声明。作为一个极端的例子，考虑命令：

```
c ≡ while true do skip
```

从任何状态开始执行 $c$ 都不会终止。根据我们上面给出的解释，以下部分正确性断言是有效的：

$$
\{ \text{true} \}\ c\ \{ \text{false} \}
$$

这仅仅是因为执行 $c$ 不会终止。因此，更一般地，由于 $c$ 会无限循环，任何部分正确性断言 $\{A\}\ c\ \{B\}$ 都是有效的。

这与另一个概念形成了对比，即 **全局正确性（total correctness）**。有时人们会写：

$$
[A]\ c\ [B]
$$

表示从满足 $A$ 的任何状态开始执行 $c$ 都将终止在一个满足 $B$ 的状态。在本书中，我们不会过多地关注全局正确性断言。

**注意**：在表达部分和全局正确性时，有多种不同的符号。当你阅读一本书时，一定要仔细确认其中使用的符号。

我们留下了一些未解决的问题。例如，我们在部分正确性断言 $\{A\}\ c\ \{B\}$ 中允许哪些类型的断言 $A$ 和 $B$？我们稍后会详细讨论这个问题，现在先转向一个更普遍的问题。

下一个问题可以实用地看作是符号问题，尽管它也可以被看作是部分正确性的断言的语义。首先，我们引入一个缩写，表示状态 $\sigma$ **满足**断言 $A$，或者等价地说，断言 $A$ 在状态 $\sigma$ **为真**。我们将其记为：

$$
\sigma \vDash A
$$

当然，我们需要对其进行定义，尽管我们都对其含义有一个直观的理解。

考虑我们对部分正确性断言 $\{A\}\ c\ \{B\}$ 的解释。由于命令 $c$ 表示从初始状态到最终状态的部分函数，根据指称语义，部分正确性断言的含义是：

$$
\forall \sigma.\ (\sigma \vDash A \land C[c]\ \sigma\ \text{已定义})\ \Rightarrow\ C[c]\ \sigma \vDash B
$$

频繁地处理 $C[c]\ \sigma$ 是否已定义的情况是很麻烦的。回想一下第 5 章中 IMP 的指称语义。我们建议使用符号 $\bot$ 来表示未定义的状态（或更严格地说，关于状态的空信息）。对于命令 $c$，我们可以写 $C[c]\ \sigma = \bot$，当且仅当 $C[c]\ \sigma$ 未定义，并且根据部分函数的组合，取 $C[c]\ \bot = \bot$。

如果我们采用约定，即 **$\bot$ 满足任何断言**，那么在符号上处理部分正确性将变得更加简单。基于以下理解：

- **对于任何断言 $A$，$\bot \vDash A$。**

因此，我们可以描述部分正确性断言 $\{A\}\ c\ \{B\}$ 的含义为：

$$
\forall \sigma \in \Sigma.\ \sigma \vDash A\ \Rightarrow\ C[c]\ \sigma \vDash B
$$

由于我们正在处理部分正确性，这个约定与我们之前对部分正确性断言的解释是一致的。这也是直观的；发散（不终止）的计算表示为 $\bot$，正如我们所见，它们满足任何后置条件。

## 6.2 断言语言 Assn

我们希望对 IMP 程序做出什么样的断言？由于我们希望对布尔表达式进行推理，我们肯定需要包含 Bexp 中的所有断言。因为我们想要使用全称量词“$\forall$”和存在量词“$\exists$”来构造断言，我们需要使用扩展的 Bexp 和 Aexp，其中包含我们可以对其进行量化的整数变量。

例如，我们可以通过写：

$$
\exists i.\ k = i \times l
$$

来表示整数 $k$ 是 $l$ 的倍数。

我们将详细展示如何为一个特定的断言语言 Assn 引入整数变量和量词。原则上，我们对断言所做的一切都可以在 Assn 中完成——它足够表达——但在示例和练习中，我们将以各种方式扩展 Assn，而不会过于严格（例如，在一个示例中，我们将使用 $n! = n \times (n - 1) \times \dots \times 2 \times 1$ 来表示阶乘函数）。

首先，我们将 Aexp 扩展为包括整数变量 $i, j, k$ 等。这是通过在 BNF 描述中添加一条规则来实现的，该规则使任何整数变量都成为一个整数表达式。因此，扩展后的算术表达式的语法类别 Aexpv 定义为：

$$
a ::= n\ |\ X\ |\ i\ |\ a_0 + a_1\ |\ a_0 - a_1\ |\ a_0 \times a_1
$$

其中：

- $n$ 是数字，$n \in \mathbb{N}$
- $X$ 是位置（变量），$X \in \text{Loc}$
- $i$ 是整数变量，$i \in \text{Intvar}$

我们将布尔表达式扩展为包括这些更通用的算术表达式和量词，以及蕴含。规则如下：

$$
A ::= \text{true}\ |\ \text{false}\ |\ a_0 = a_1\ |\ a_0 \leq a_1\ |\ A_0 \land A_1\ |\ A_0 \lor A_1\ |\ \lnot A\ |\ A_0 \Rightarrow A_1\ |\ \forall i.\ A\ |\ \exists i.\ A
$$

我们将扩展的布尔断言集合称为 Assn。

在学校时，我们已经有了处理上述表达式的经验，尽管那时我们可能以不那么简洁的方式书写数学，不使用量词。例如，当我们遇到一个整数变量 $i$ 时，我们将其视为代表某个任意整数，并像对待未知数 $x, y, \dots$ 一样进行计算。像 $A_0 \Rightarrow A_1$ 这样的蕴含表示“如果 $A_0$，则 $A_1$”，当 $A_0$ 为假或 $A_1$ 为真时，该断言为真。我们之前在数学中使用过蕴含，现在我们将其添加到我们的形式断言集合 Assn 中。

我们对这些表达式和断言有一个“常识”的理解（这在做练习时应该足够了）。然而，因为我们希望基于断言来推理证明系统，而不仅仅是例子，所以我们将更加形式化，给出包含整数变量的表达式和断言的意义理论。这是 **谓词演算** 的一部分。

### 6.2.1 自由变量和约束变量

我们说，一个整数变量 $i$ 在一个断言中的出现是 **约束的（bound）**，如果它出现在一个封闭的量词 $\forall i$ 或 $\exists i$ 的作用域中。如果它没有被约束，我们称其为 **自由的（free）**。

例如，在断言：

$$
\exists i.\ k = i \times l
$$

中，整数变量 $i$ 的出现是约束的，而 $k$ 和 $l$ 的出现是自由的——变量 $k$ 和 $l$ 被理解为代表特定的整数，即使我们不精确地知道它们的值。

同一个整数变量可以在同一个断言中有不同的出现，其中一个是自由的，另一个是约束的。例如，在断言：

$$
(i + 100 \leq 77) \land (\forall i.\ j + 1 = i + 3)
$$

中，$i$ 的第一个出现是自由的，第二个出现是约束的，而 $j$ 的唯一出现是自由的。

尽管这个非正式的解释可能已经足够了，但我们可以使用结构归纳的定义给出一个形式的定义。

**定义：**

- 对于包含整数变量的算术表达式 $a \in \text{Aexpv}$，定义其 **自由变量集** $\text{FV}(a)$，通过结构归纳如下：

  - $\text{FV}(n) = \text{FV}(X) = \emptyset$
  - $\text{FV}(i) = \{ i \}$
  - $\text{FV}(a_0 + a_1) = \text{FV}(a_0 - a_1) = \text{FV}(a_0 \times a_1) = \text{FV}(a_0) \cup \text{FV}(a_1)$

  其中 $n \in \mathbb{N}$，$X \in \text{Loc}$，$i \in \text{Intvar}$，$a_0, a_1 \in \text{Aexpv}$。

- 对于断言 $A$，定义其 **自由变量集** $\text{FV}(A)$，通过结构归纳如下：

  - $\text{FV}(\text{true}) = \text{FV}(\text{false}) = \emptyset$
  - $\text{FV}(a_0 = a_1) = \text{FV}(a_0 \leq a_1) = \text{FV}(a_0) \cup \text{FV}(a_1)$
  - $\text{FV}(A_0 \land A_1) = \text{FV}(A_0 \lor A_1) = \text{FV}(A_0 \Rightarrow A_1) = \text{FV}(A_0) \cup \text{FV}(A_1)$
  - $\text{FV}(\lnot A) = \text{FV}(A)$
  - $\text{FV}(\forall i.\ A) = \text{FV}(\exists i.\ A) = \text{FV}(A) \setminus \{ i \}$

  其中 $a_0, a_1 \in \text{Aexpv}$，$i$ 是整数变量，$A_0, A_1, A$ 是断言。

因此，我们精确地定义了自由变量的概念。任何在断言 $A$ 中出现但不是自由的变量，我们称其为 **约束变量**。一个没有自由变量的断言被称为 **闭式（closed）**。

### 6.2.2 替换

我们可以将一个断言 $A$ 视为：

$$
\text{---}i\ \text{---}i\ \text{---}
$$

其中有整数变量 $i$ 的自由出现。设 $a$ 是一个算术表达式，为了简单起见，我们假设它不包含整数变量。那么，$A[a/i]$ 表示用 $a$ 替换 $i$ 的结果，即：

$$
A[a/i] \equiv \text{---}a\ \text{---}a\ \text{---}
$$

如果 $a$ 包含整数变量，那么在避免 $a$ 中的变量被 $A$ 中的量词绑定时，可能需要重命名 $A$ 的一些绑定变量——这就是一般替换的处理方式。

**更精确地**，我们在简单情况下描述替换。设 $i$ 是一个整数变量，$a$ 是一个不包含整数变量的算术表达式。首先，通过以下结构归纳定义算术表达式中的替换：

- $n[a/i] = n$
- $X[a/i] = X$
- $j[a/i] = j$，其中 $j$ 是整数变量，且 $j \ne i$
- $i[a/i] = a$
- $(a_0 + a_1)[a/i] = (a_0[a/i] + a_1[a/i])$
- $(a_0 - a_1)[a/i] = (a_0[a/i] - a_1[a/i])$
- $(a_0 \times a_1)[a/i] = (a_0[a/i] \times a_1[a/i])$

其中 $n$ 是一个数字，$X$ 是位置，$j$ 是整数变量，$j \ne i$，$a_0, a_1 \in \text{Aexpv}$。

现在，我们通过结构归纳定义在断言中用 $a$ 替换 $i$，记住 $a$ 不包含任何自由变量，因此我们不需要采取任何预防措施来避免其变量被绑定：

- $\text{true}[a/i] = \text{true}$
- $\text{false}[a/i] = \text{false}$
- $(a_0 = a_1)[a/i] = (a_0[a/i] = a_1[a/i])$
- $(a_0 \leq a_1)[a/i] = (a_0[a/i] \leq a_1[a/i])$
- $(A_0 \land A_1)[a/i] = (A_0[a/i] \land A_1[a/i])$
- $(A_0 \lor A_1)[a/i] = (A_0[a/i] \lor A_1[a/i])$
- $(\lnot A)[a/i] = \lnot (A[a/i])$
- $(A_0 \Rightarrow A_1)[a/i] = (A_0[a/i] \Rightarrow A_1[a/i])$
- $(\forall j.\ A)[a/i] = \forall j.\ (A[a/i])$，其中 $j \ne i$
- $(\forall i.\ A)[a/i] = \forall i.\ A$
- $(\exists j.\ A)[a/i] = \exists j.\ (A[a/i])$，其中 $j \ne i$
- $(\exists i.\ A)[a/i] = \exists i.\ A$

其中 $a_0, a_1 \in \text{Aexpv}$，$A_0, A_1, A$ 是断言，$j$ 是整数变量，$j \ne i$。

正如所提到的，在 $a$ 包含自由变量的情况下定义替换 $A[a/i]$ 是麻烦的，因为它涉及到绑定变量的重命名。幸运的是，我们目前不需要这种更复杂的替换定义。

我们使用相同的符号来表示对位置 $X$ 的替换，因此如果断言 $A$ 为 $\text{---}X\ \text{---}$，那么 $A[a/X] = \text{---}a\ \text{---}$，将 $a$ 代替 $X$。这次（更简单的）正式定义留给读者。

**练习 6.1**

写下一个断言 $A \in \text{Assn}$，具有一个自由整数变量 $i$，表示 $i$ 是一个素数，即需要满足：

- $\sigma \vDash_I A$ 当且仅当 $I(i)$ 是一个素数。

**解答：**

一个整数 $i$ 是素数，当且仅当它大于 $1$，并且除了 $1$ 和 $i$ 自身外，没有其他正整数能整除 $i$。因此，我们可以写出断言 $A$ 如下：

$$
A \equiv (i > 1) \land \left( \forall n.\ (1 < n < i) \Rightarrow \lnot (\exists k.\ i = n \times k) \right)
$$

这表示对于所有 $n$，如果 $n$ 在 $1$ 和 $i$ 之间，那么不存在 $k$ 使得 $i = n \times k$，即 $n$ 不能整除 $i$。

---

**练习 6.2**

定义一个公式 $LCM \in \text{Assn}$，具有自由整数变量 $i, j, k$，表示“$k$ 是 $i$ 和 $j$ 的最小公倍数”，即需要满足：

- $\sigma \vDash_I LCM$ 当且仅当 $I(k)$ 是 $I(i)$ 和 $I(j)$ 的最小公倍数。

**提示**：两个数的最小公倍数是能被它们两个整除的最小非负整数。

**解答：**

一个整数 $k$ 是 $i$ 和 $j$ 的最小公倍数，当且仅当：

1. $k$ 能被 $i$ 和 $j$ 整除，即 $i$ 和 $j$ 都是 $k$ 的因数；
2. 对于任何比 $k$ 小的正整数 $k'$，如果 $k'$ 能被 $i$ 和 $j$ 整除，那么 $k' \geq k$。

因此，我们可以写出断言 $LCM$ 如下：

$$
LCM \equiv (k > 0) \land (i > 0) \land (j > 0) \land (i\ |\ k) \land (j\ |\ k) \land \left( \forall k'.\ (k' > 0 \land i\ |\ k' \land j\ |\ k') \Rightarrow (k' \geq k) \right)
$$

其中，$i\ |\ k$ 表示 $k$ 能被 $i$ 整除，可以定义为：

$$
i\ |\ k \equiv \exists m.\ k = i \times m
$$

---

## 6.3 断言的语义

由于算术表达式已经扩展为包含整数变量，我们无法使用之前的语义函数 $A$ 来充分描述这些新表达式的值。我们必须首先将整数变量解释为特定的整数，这就是 **解释（interpretation）** 的作用。

一个 **解释** 是一个将整数变量映射到整数的函数，即 $I : \text{Intvar} \rightarrow \mathbb{N}$。

### 带有整数变量的表达式的意义

现在，我们可以定义一个语义函数 $A_v$，它在特定的解释 $I$ 和状态 $\sigma$ 下，给出包含整数变量的算术表达式的值。表达式 $a \in \text{Aexpv}$ 在解释 $I$ 和状态 $\sigma$ 下的值写作 $A_v[a]\ I\ \sigma$。

通过结构归纳定义如下：

- $A_v[n]\ I\ \sigma = n$
- $A_v[X]\ I\ \sigma = \sigma(X)$
- $A_v[i]\ I\ \sigma = I(i)$
- $A_v[a_0 + a_1]\ I\ \sigma = A_v[a_0]\ I\ \sigma + A_v[a_1]\ I\ \sigma$
- $A_v[a_0 - a_1]\ I\ \sigma = A_v[a_0]\ I\ \sigma - A_v[a_1]\ I\ \sigma$
- $A_v[a_0 \times a_1]\ I\ \sigma = A_v[a_0]\ I\ \sigma \times A_v[a_1]\ I\ \sigma$

这个带有整数变量的算术表达式的语义定义，扩展了第 5 章中对不包含整数变量的算术表达式给出的指称语义。

**命题 6.3**

对于所有不包含整数变量的 $a \in \text{Aexp}$，对于所有状态 $\sigma$ 和所有解释 $I$，有：

$$
A[a]\ \sigma = A_v[a]\ I\ \sigma
$$

**证明：**

这个证明是一个关于算术表达式的结构归纳的简单练习。

- 基础情况：

  - 对于常数 $n$，$A[n]\ \sigma = n = A_v[n]\ I\ \sigma$。

  - 对于位置 $X$，$A[X]\ \sigma = \sigma(X) = A_v[X]\ I\ \sigma$。

- 归纳步骤：

  - 假设对于 $a_0$ 和 $a_1$，$A[a_0]\ \sigma = A_v[a_0]\ I\ \sigma$，$A[a_1]\ \sigma = A_v[a_1]\ I\ \sigma$。

  - 那么，对于 $a = a_0 + a_1$，有：

    $$
    A[a]\ \sigma = A[a_0]\ \sigma + A[a_1]\ \sigma = A_v[a_0]\ I\ \sigma + A_v[a_1]\ I\ \sigma = A_v[a]\ I\ \sigma
    $$

  - 类似地，对于减法和乘法的情况。

因此，命题得证。

---

### 断言的意义

由于我们包含了整数变量，语义函数需要一个解释函数作为额外的参数。解释函数的作用仅仅是为整数变量提供一个整数值。

**记号**：我们使用 $I[n/i]$ 来表示从解释 $I$ 通过将整数变量 $i$ 的值更改为 $n$ 得到的解释，即：

$$
I[n/i](j) = \begin{cases}
n, & \text{如果 } j = i \\
I(j), & \text{否则}
\end{cases}
$$

我们可以像对包含整数变量的表达式那样指定 Assn 中的断言的意义，但这次语义函数将从断言映射到函数，该函数在给定解释和状态时返回一个真值。我们选择一种等价的替代方法。给定一个解释 $I$，我们直接定义在什么时候一个状态 $\sigma$ **满足**一个断言。

实际上，将状态集合 $\Sigma$ 扩展为包含一个特殊值 $\bot$，即 $\Sigma_\bot = \Sigma \cup \{ \bot \}$，其中 $\bot$ 与非终止计算相关联是很方便的。

对于 $A \in \text{Assn}$，我们通过结构归纳定义在解释 $I$ 下，当 $\sigma \vDash_I A$ 成立，其中 $\sigma \in \Sigma$。

**定义**：对于所有 $\sigma \in \Sigma$，解释 $I$，断言 $A$，定义：

- $\sigma \vDash_I \text{true}$
- $\sigma \vDash_I (a_0 = a_1)$ 当且仅当 $A_v[a_0]\ I\ \sigma = A_v[a_1]\ I\ \sigma$
- $\sigma \vDash_I (a_0 \leq a_1)$ 当且仅当 $A_v[a_0]\ I\ \sigma \leq A_v[a_1]\ I\ \sigma$
- $\sigma \vDash_I A_0 \land A_1$ 当且仅当 $\sigma \vDash_I A_0$ 且 $\sigma \vDash_I A_1$
- $\sigma \vDash_I A_0 \lor A_1$ 当且仅当 $\sigma \vDash_I A_0$ 或 $\sigma \vDash_I A_1$
- $\sigma \vDash_I \lnot A$ 当且仅当 $\sigma$ **不满足** $A$，即 $\sigma \nvDash_I A$
- $\sigma \vDash_I A_0 \Rightarrow A_1$ 当且仅当 $\sigma \nvDash_I A_0$ 或 $\sigma \vDash_I A_1$
- $\sigma \vDash_I \forall i.\ A$ 当且仅当 对于所有 $n \in \mathbb{N}$，$\sigma \vDash_{I[n/i]} A$
- $\sigma \vDash_I \exists i.\ A$ 当且仅当 存在某个 $n \in \mathbb{N}$，使得 $\sigma \vDash_{I[n/i]} A$

此外，定义 $\bot \vDash_I A$ 对于任何 $A$。

**注意**：$\sigma \nvDash_I A$ 通常写作 $\sigma \not\vDash_I A$。

上述定义正式地告诉我们，当我们决定以解释 $I$ 来解释整数变量时，断言在一个状态下为真的含义。布尔表达式的语义提供了另一种说明断言在一个状态下为真或假的方式。我们最好检查这两种方式是否一致。

**命题 6.4**

对于 $b \in \text{Bexp}$，$\sigma \in \Sigma$，对于任何解释 $I$，有：

- $B[b]\ \sigma = \text{true}$ 当且仅当 $\sigma \vDash_I b$
- $B[b]\ \sigma = \text{false}$ 当且仅当 $\sigma \nvDash_I b$

**证明**：

证明是对布尔表达式的结构归纳，利用命题 6.3。

---

**练习 6.5**

证明上述命题。

**解答**：

我们需要对布尔表达式 $b$ 进行结构归纳，证明对于任意状态 $\sigma$ 和解释 $I$，$B[b]\ \sigma = \text{true}$ 当且仅当 $\sigma \vDash_I b$。

- 基础情况：

  - 对于 $b = \text{true}$，$B[\text{true}]\ \sigma = \text{true}$，而 $\sigma \vDash_I \text{true}$。

  - 对于 $b = \text{false}$，$B[\text{false}]\ \sigma = \text{false}$，而 $\sigma \nvDash_I \text{false}$。

  - 对于 $b = a_0 = a_1$，$B[a_0 = a_1]\ \sigma = \text{true}$ 当且仅当 $A[a_0]\ \sigma = A[a_1]\ \sigma$。

    根据命题 6.3，$A[a_0]\ \sigma = A_v[a_0]\ I\ \sigma$，所以 $B[a_0 = a_1]\ \sigma = \text{true}$ 当且仅当 $A_v[a_0]\ I\ \sigma = A_v[a_1]\ I\ \sigma$，即 $\sigma \vDash_I (a_0 = a_1)$。

  - 类似地，对于 $b = a_0 \leq a_1$。

- 归纳步骤：

  - 对于 $b = \lnot b'$，$B[\lnot b']\ \sigma = \lnot B[b']\ \sigma$。

    假设对于 $b'$，$B[b']\ \sigma = \text{true}$ 当且仅当 $\sigma \vDash_I b'$。

    因此，$B[\lnot b']\ \sigma = \text{true}$ 当且仅当 $B[b']\ \sigma = \text{false}$，即 $\sigma \nvDash_I b'$。

    这与 $\sigma \vDash_I \lnot b'$ 一致。

  - 对于 $b = b_0 \land b_1$，类似地证明。

因此，命题得证。

---

**练习 6.6**

通过对表达式 $a \in \text{Aexpv}$ 进行结构归纳，证明：

$$
A_v[a]\ I[n/i]\ \sigma = A_v[a[n/i]]\ I\ \sigma
$$

注意，左边的 $n$ 是 $\mathbb{N}$ 中的一个元素，右边的 $n$ 是替换表达式中的常数。

利用上述事实，证明：

- $\sigma \vDash_I \forall i.\ A$ 当且仅当 对所有 $n \in \mathbb{N}$，$\sigma \vDash_I A[n/i]$
- $\sigma \vDash_I \exists i.\ A$ 当且仅当 存在 $n \in \mathbb{N}$，使得 $\sigma \vDash_I A[n/i]$

**解答**：

- 对于表达式 $a$，进行结构归纳：

  - 基础情况：

    - $a = n$，则 $A_v[n]\ I[n/i]\ \sigma = n = A_v[n[n/i]]\ I\ \sigma$。

    - $a = X$，同理。

    - $a = j$，其中 $j$ 是整数变量：

      - 如果 $j \ne i$，则 $A_v[j]\ I[n/i]\ \sigma = I[n/i](j) = I(j) = A_v[j]\ I\ \sigma = A_v[j[n/i]]\ I\ \sigma$。

      - 如果 $j = i$，则 $A_v[i]\ I[n/i]\ \sigma = I[n/i](i) = n = A_v[n]\ I\ \sigma = A_v[i[n/i]]\ I\ \sigma$。

  - 归纳步骤：

    - 对于 $a = a_0 + a_1$，有：

      $$
      \begin{aligned}
      A_v[a]\ I[n/i]\ \sigma &= A_v[a_0 + a_1]\ I[n/i]\ \sigma \\
      &= A_v[a_0]\ I[n/i]\ \sigma + A_v[a_1]\ I[n/i]\ \sigma \\
      \text{（归纳假设）} &= A_v[a_0[n/i]]\ I\ \sigma + A_v[a_1[n/i]]\ I\ \sigma \\
      &= A_v[a_0[n/i] + a_1[n/i]]\ I\ \sigma \\
      &= A_v[(a_0 + a_1)[n/i]]\ I\ \sigma
      \end{aligned}
      $$

    - 类似地，对于减法和乘法的情况。

- 利用上述结果，对于 $\forall i.\ A$，有：

  - $\sigma \vDash_I \forall i.\ A$ 当且仅当 对所有 $n \in \mathbb{N}$，$\sigma \vDash_{I[n/i]} A$。

  - 由于 $A_v[a]\ I[n/i]\ \sigma = A_v[a[n/i]]\ I\ \sigma$，因此 $\sigma \vDash_{I[n/i]} A$ 当且仅当 $\sigma \vDash_I A[n/i]$。

  - 因此，$\sigma \vDash_I \forall i.\ A$ 当且仅当 对所有 $n \in \mathbb{N}$，$\sigma \vDash_I A[n/i]$。

- 对于 $\exists i.\ A$，类似地证明。

---

### 断言的扩张

设 $I$ 是一个解释。在建立关于断言和部分正确性断言的性质时，考虑断言在 $I$ 下的 **扩张**（extension）是有用的，即断言在其上为真的状态的集合。

定义在解释 $I$ 下，断言 $A$ 的扩张为：

$$
\text{Ext}_I(A) = \{ \sigma \in \Sigma\ |\ \sigma \vDash_I A \}
$$

---

### 部分正确性断言

一个部分正确性断言的形式为：

$$
\{A\}\ c\ \{B\}
$$

其中 $A, B \in \text{Assn}$，$c \in \text{Com}$。注意，部分正确性断言不属于 Assn。

设 $I$ 是一个解释，$\sigma \in \Sigma_\bot$。我们定义状态与部分正确性断言之间的满足关系，在解释 $I$ 下为：

$$
\sigma \vDash_I \{A\}\ c\ \{B\} \quad \text{当且仅当} \quad \text{如果 } \sigma \vDash_I A,\ \text{则 } C[c]\ \sigma \vDash_I B
$$

换句话说，状态 $\sigma$ 满足部分正确性断言 $\{A\}\ c\ \{B\}$，当且仅当从 $\sigma$ 开始的 $c$ 的任何成功计算都以一个满足 $B$ 的状态结束。

---

### 有效性

设 $I$ 是一个解释。考虑部分正确性断言 $\{A\}\ c\ \{B\}$。我们对断言是否在所有状态下成立更感兴趣，即：

$$
\forall \sigma \in \Sigma_\bot.\ \sigma \vDash_I \{A\}\ c\ \{B\}
$$

我们可以写作：

$$
\vDash_I \{A\}\ c\ \{B\}
$$

表示部分正确性断言在解释 $I$ 下是 **有效的**，因为 $\{A\}\ c\ \{B\}$ 对于我们考虑的任何状态都为真。

进一步考虑，例如：

$$
\{ i < X \}\ X := X + 1\ \{ i < X \}
$$

我们对解释 $I$ 关联的 $i$ 的特定值并不太感兴趣。我们更关心的是它是否对所有解释 $I$ 的所有状态都为真。这激发了 **有效性（validity）** 的概念。

定义：

$$
\vDash \{A\}\ c\ \{B\}
$$

表示对于所有解释 $I$ 和所有状态 $\sigma$，都有：

$$
\sigma \vDash_I \{A\}\ c\ \{B\}
$$

当 $\vDash \{A\}\ c\ \{B\}$ 时，我们说部分正确性断言 $\{A\}\ c\ \{B\}$ 是 **有效的**。

类似地，对于任何断言 $A$，写作 $\vDash A$，当且仅当对于所有解释 $I$ 和状态 $\sigma$，都有 $\sigma \vDash_I A$。然后说 $A$ 是有效的。

**警告**：尽管密切相关，我们的有效性概念与标准的谓词演算或“逻辑编程”课程中遇到的有效性概念并不相同。在那里，一个断言被称为有效，当且仅当对于所有操作符（如 $+$、$\times$ 等）、数字（如 $0$、$1$ 等）以及自由变量的解释，断言都为真。我们对这种一般意义上的任意解释不感兴趣，因为 IMP 程序基于具有标准整数和整数操作概念的位置上的状态运行。为了将这里的有效性概念与更一般的概念区分开来，我们可以称我们的概念为 **算术有效性（arithmetic-validity）**，但我们将省略“算术”一词。

**示例**：假设 $\vDash (A \Rightarrow B)$。那么对于任何解释 $I$，有：

$$
\forall \sigma \in \Sigma.\ (\sigma \vDash_I A) \Rightarrow (\sigma \vDash_I B)
$$

即，在解释 $I$ 下，$A$ 的扩张包含于 $B$ 的扩张中。

**示意图**：

```
[.... A ....] ---> [.... B ....]
```

因此，$\vDash (A \Rightarrow B)$ 当且仅当对于所有解释 $I$，满足 $A$ 的所有状态也满足 $B$。

---

**示例**：假设 $\vDash \{A\}\ c\ \{B\}$。那么对于任何解释 $I$，有：

$$
\forall \sigma \in \Sigma.\ (\sigma \vDash_I A) \Rightarrow (C[c]\ \sigma \vDash_I B)
$$

即，$A$ 在 $C[c]$ 下的像包含于 $B$。

**示意图**：

```
[.... A ....] --C[c]--> [.... B ....]
```

因此，$\vDash \{A\}\ c\ \{B\}$ 当且仅当对于所有解释 $I$，如果从满足 $A$ 的状态开始执行 $c$，那么如果其执行终止于某个状态，该状态将满足 $B$。

---

**练习 6.7**

在前面的练习中，要求写下一个具有一个自由整数变量 $i$ 的断言 $A \in \text{Assn}$，表示 $i$ 是素数。通过在定义状态与断言之间的满足关系的适当情况下进行推导，追踪 $\sigma \vDash_I A$ 当且仅当 $I(i)$ 确实是素数的论证。

**解答**：

我们需要证明，对于任何状态 $\sigma$ 和解释 $I$，$\sigma \vDash_I A$ 当且仅当 $I(i)$ 是素数。

按照练习 6.1 中给出的 $A$：

$$
A \equiv (i > 1) \land \left( \forall n.\ (1 < n < i) \Rightarrow \lnot (\exists k.\ i = n \times k) \right)
$$

我们需要验证：

- $\sigma \vDash_I (i > 1)$ 当且仅当 $I(i) > 1$。
- 对于所有 $n$，如果 $1 < n < I(i)$，则不存在 $k$ 使得 $I(i) = n \times k$。

具体来说：

1. $\sigma \vDash_I (i > 1)$：

   - $A_v[i]\ I\ \sigma = I(i)$，所以 $A_v[i > 1]\ I\ \sigma$ 为真当且仅当 $I(i) > 1$。

2. 对于所有 $n$，$\sigma \vDash_I \left( (1 < n < i) \Rightarrow \lnot (\exists k.\ i = n \times k) \right)$：

   - 对于所有 $n \in \mathbb{N}$，如果 $1 < n < I(i)$，那么不存在 $k$ 使得 $I(i) = n \times k$。

   - 这正是定义素数的条件。

因此，$\sigma \vDash_I A$ 当且仅当 $I(i)$ 是一个大于 $1$ 的整数，且在 $1$ 和 $I(i)$ 之间不存在整数 $n$ 能整除 $I(i)$。

---

**总结**

在本章中，我们引入了 IMP 的公理语义，特别是 Hoare 逻辑，用于证明程序的部分正确性。我们定义了断言语言 Assn，包括自由变量和约束变量的概念，以及替换和断言的语义。我们讨论了部分正确性断言的含义和有效性，并通过示例和练习来加深理解。

### ---------------------------

# 第6章 IMP的公理语义（续）

## 6.4 部分正确性的证明规则

我们提出一些证明规则，用于生成有效的部分正确性断言。证明规则是**语法导向的（syntax-directed）**；这些规则将证明一个复合命令的部分正确性断言的任务，化简为证明其直接子命令的部分正确性断言。这些证明规则通常被称为 **Hoare 规则**，由这些规则组成的证明系统称为 **Hoare 逻辑**。

以下是 Hoare 逻辑的基本证明规则：

### **空语句的规则（skip）**：

$$
\{A\}\ \textbf{skip}\ \{A\}
$$

### **赋值语句的规则**：

$$
\{B[a/X]\}\ X := a\ \{B\}
$$

### **顺序组合的规则（序列化）**：

$$
\begin{align*}
&\{A\}\ c_0\ \{C\} \\
&\{C\}\ c_1\ \{B\} \\
\hline
&\{A\}\ c_0;\ c_1\ \{B\}
\end{align*}
$$

### **条件语句的规则**：

$$
\begin{align*}
&\{A \land b\}\ c_0\ \{B\} \\
&\{A \land \lnot b\}\ c_1\ \{B\} \\
\hline
&\{A\}\ \textbf{if}\ b\ \textbf{then}\ c_0\ \textbf{else}\ c_1\ \{B\}
\end{align*}
$$

### **循环语句的规则（while 循环）**：

$$
\begin{align*}
&\{A \land b\}\ c\ \{A\} \\
\hline
&\{A\}\ \textbf{while}\ b\ \textbf{do}\ c\ \{A \land \lnot b\}
\end{align*}
$$

### **推理规则（后果规则）**：

$$
\begin{align*}
&\vdash (A \implies A') \\
&\{A'\}\ c\ \{B'\} \\
&\vdash (B' \implies B) \\
\hline
&\{A\}\ c\ \{B\}
\end{align*}
$$

**注释**：

- 在赋值语句的规则中，$B[a/X]$ 表示在断言 $B$ 中用表达式 $a$ 替换变量 $X$。
- 在顺序组合规则中，$c_0;\ c_1$ 表示先执行命令 $c_0$，然后执行命令 $c_1$。

这些规则都是**推理规则**，因此在 Hoare 规则的上下文中存在**推导（derivation）**的概念。Hoare 规则被视为一个证明系统，推导被称为**证明**，任何推导的结论都是一个**定理**。当 $\vdash \{A\}\ c\ \{B\}$ 时，表示 $\{A\}\ c\ \{B\}$ 是一个定理。

这些规则相当容易理解，可能除了赋值语句和 while 循环的规则需要更多解释。

- **空语句（skip）的规则**：如果在执行 `skip` 之前断言 $A$ 为真，那么由于状态未改变，执行之后 $A$ 仍然为真。这就是 `skip` 规则的含义。

- **赋值语句的规则**：为了证明该规则的正确性，可以尝试对特定的断言和赋值语句进行验证。例如，考虑断言 $X = 3$ 和赋值语句 $X := X + 3$，使用规则可以验证其正确性。

- **顺序组合的规则**：该规则表示，如果 $\{A\}\ c_0\ \{C\}$ 和 $\{C\}\ c_1\ \{B\}$ 都是有效的，那么 $\{A\}\ c_0;\ c_1\ \{B\}$ 也是有效的。即，从满足 $A$ 的状态开始，成功执行 $c_0$ 后得到满足 $C$ 的状态，然后从满足 $C$ 的状态开始，成功执行 $c_1$ 后得到满足 $B$ 的状态，那么从满足 $A$ 的状态开始依次执行 $c_0$ 和 $c_1$，最终得到满足 $B$ 的状态。

- **条件语句的规则**：该规则的两个前提分别处理条件语句的两个分支（`then` 和 `else`）。

- **while 循环的规则**：在 `while` 循环中，断言 $A$ 被称为**循环不变式**，因为前提 $\{A \land b\}\ c\ \{A\}$ 表明在循环体的完整执行后，断言 $A$ 被保持，并且循环体的执行只在状态满足条件 $b$ 时发生。

- **后果规则（推理规则）**：该规则的特点是前提中包含有效的蕴含（implications）。应用该规则需要首先证明 $\vdash (A \implies A')$ 和 $\vdash (B' \implies B)$ 是有效的。一般来说，这可能是一个很困难的任务，因为这些蕴含可能表达了关于算术的复杂事实。然而，在实践中，由于程序通常不涉及深奥的数学事实，这些有效性的证明通常可以用初等数学来完成。

## 6.5 规则的健全性

我们考虑 Hoare 规则的两个关于逻辑系统的非常一般的性质：

1. **健全性（Soundness）**：每个规则都应该保持有效性，即如果规则前提中的假设是有效的，那么结论也是有效的。当规则满足这个性质时，称其为**健全的**。当证明系统中的每个规则都是健全的时，证明系统本身被称为**健全的**。因此，通过规则归纳，我们可以推断从 Hoare 规则的证明系统中获得的每个定理都是有效的部分正确性断言。

2. **完备性（Completeness）**：我们希望证明系统足够强大，以至于所有有效的部分正确性断言都可以作为定理得到。我们希望证明系统在这个意义上是完备的。（关于这一点存在一些微妙的问题，我们将在下一章讨论。）

证明规则的健全性依赖于一些关于替换的事实。

### **引理 6.8**

设 $I$ 是一个解释。令 $a, a_0 \in \text{Aexpv}$，$X \in \text{Loc}$。则对于所有解释 $I$ 和状态 $\sigma$，有：

$$
A_v[a_0[a/X]]\ I\ \sigma = A_v[a_0]\ I\ \sigma[A_v[a]\ I\ \sigma / X]
$$

**解释**：

- 左边是将 $a$ 替换到 $a_0$ 中，然后计算其在解释 $I$ 和状态 $\sigma$ 下的值。
- 右边是先在状态 $\sigma$ 下计算 $a$ 的值 $A_v[a]\ I\ \sigma$，然后将 $X$ 的值替换为 $A_v[a]\ I\ \sigma$，再计算 $a_0$ 在新的状态下的值。

**证明**：

证明是对 $a_0$ 进行结构归纳的练习，留给读者完成。

### **引理 6.9**

设 $I$ 是一个解释。令 $B \in \text{Assn}$，$X \in \text{Loc}$，$a \in \text{Aexp}$。对于所有状态 $\sigma \in \Sigma$，有：

$$
\sigma \vDash_I B[a/X] \quad \text{当且仅当} \quad \sigma[A[a]\ \sigma / X] \vDash_I B
$$

**解释**：

- 左边是用 $a$ 替换 $X$ 后，断言 $B$ 在状态 $\sigma$ 下为真。
- 右边是先计算 $a$ 在状态 $\sigma$ 下的值 $A[a]\ \sigma$，然后在状态 $\sigma$ 中将 $X$ 的值替换为 $A[a]\ \sigma$，在新的状态下检查 $B$ 是否为真。

**证明**：

证明是对 $B$ 进行结构归纳的练习，留给读者完成。

**练习 6.10**

提供上述引理的证明。

**解答**：

**对于引理 6.8**：

我们对 $a_0$ 进行结构归纳。

- **基础情况**：

  - 当 $a_0$ 为常数或变量时：

    - 如果 $a_0 = n$，则 $A_v[n[a/X]]\ I\ \sigma = n$，而 $A_v[n]\ I\ \sigma = n$，因此成立。

    - 如果 $a_0 = Y$，且 $Y \ne X$，则 $A_v[Y[a/X]]\ I\ \sigma = A_v[Y]\ I\ \sigma$，成立。

    - 如果 $a_0 = X$，则 $A_v[X[a/X]]\ I\ \sigma = A_v[a]\ I\ \sigma$，而 $A_v[X]\ I\ \sigma[A_v[a]\ I\ \sigma / X] = A_v[a]\ I\ \sigma$，成立。

- **归纳步骤**：

  - 假设对 $a_0$ 的子表达式成立。

  - 对于 $a_0 = a_1 + a_2$，有：

    $$
    \begin{aligned}
    A_v[(a_1 + a_2)[a/X]]\ I\ \sigma &= A_v[a_1[a/X] + a_2[a/X]]\ I\ \sigma \\
    &= A_v[a_1[a/X]]\ I\ \sigma + A_v[a_2[a/X]]\ I\ \sigma \\
    \text{（归纳假设）} &= \left( A_v[a_1]\ I\ \sigma[A_v[a]\ I\ \sigma / X] \right) + \left( A_v[a_2]\ I\ \sigma[A_v[a]\ I\ \sigma / X] \right) \\
    &= A_v[a_1 + a_2]\ I\ \sigma[A_v[a]\ I\ \sigma / X]
    \end{aligned}
    $$

  - 对于减法和乘法，类似地证明。

因此，引理 6.8 得证。

**对于引理 6.9**：

我们对 $B$ 进行结构归纳。

- **基础情况**：

  - 当 $B$ 为基本断言时：

    - 对于 $B = a_1 = a_2$，有：

      $$
      \sigma \vDash_I (a_1 = a_2)[a/X] \quad \text{当且仅当} \quad A_v[a_1[a/X]]\ I\ \sigma = A_v[a_2[a/X]]\ I\ \sigma
      $$

      根据引理 6.8，有：

      $$
      A_v[a_i[a/X]]\ I\ \sigma = A_v[a_i]\ I\ \sigma[A_v[a]\ I\ \sigma / X]
      $$

      因此，$\sigma[A_v[a]\ I\ \sigma / X] \vDash_I a_1 = a_2$。

    - 对于 $B = a_1 \leq a_2$，类似地证明。

- **归纳步骤**：

  - 假设对 $B$ 的子断言成立。

  - 对于 $B = B_1 \land B_2$，有：

    $$
    \sigma \vDash_I (B_1 \land B_2)[a/X] \quad \text{当且仅当} \quad \sigma \vDash_I B_1[a/X] \text{ 且 } \sigma \vDash_I B_2[a/X]
    $$

    根据归纳假设，有：

    $$
    \sigma[A_v[a]\ I\ \sigma / X] \vDash_I B_1 \text{ 且 } \sigma[A_v[a]\ I\ \sigma / X] \vDash_I B_2
    $$

    因此，$\sigma[A_v[a]\ I\ \sigma / X] \vDash_I B_1 \land B_2$。

  - 对于其他逻辑连接词和量词，类似地证明。

因此，引理 6.9 得证。

---

**定理 6.11**

设 $\{A\}\ c\ \{B\}$ 是一个部分正确性断言。

如果 $\vdash \{A\}\ c\ \{B\}$，那么 $\vDash \{A\}\ c\ \{B\}$。

**证明**：

只要我们能够证明每个规则都是健全的（即，如果其前提由有效的断言和部分正确性断言组成，那么其结论也是有效的），那么通过规则归纳，我们可以看到每个定理都是有效的。

- **空语句的规则**：显然，$\vDash \{A\}\ \textbf{skip}\ \{A\}$，因此该规则是健全的。

- **赋值语句的规则**：假设 $c \equiv (X := a)$。令 $I$ 是一个解释。根据引理 6.9，有：

  $$
  \sigma \vDash_I B[a/X] \quad \text{当且仅当} \quad \sigma[A[a]\ \sigma / X] \vDash_I B
  $$

  由于 $C[X := a]\ \sigma = \sigma[A[a]\ \sigma / X]$，因此：

  $$
  \sigma \vDash_I B[a/X] \quad \implies \quad C[X := a]\ \sigma \vDash_I B
  $$

  因此，$\vDash \{B[a/X]\}\ X := a\ \{B\}$，证明了赋值语句规则的健全性。

- **顺序组合的规则**：假设 $\vDash \{A\}\ c_0\ \{C\}$，$\vDash \{C\}\ c_1\ \{B\}$。令 $I$ 是一个解释。假设 $\sigma \vDash_I A$。由于 $\vDash \{A\}\ c_0\ \{C\}$，因此 $C[c_0]\ \sigma \vDash_I C$。又由于 $\vDash \{C\}\ c_1\ \{B\}$，因此 $C[c_1]\ (C[c_0]\ \sigma) \vDash_I B$。因此，$C[c_0;\ c_1]\ \sigma \vDash_I B$，所以 $\vDash \{A\}\ c_0;\ c_1\ \{B\}$。

- **条件语句的规则**：假设 $\vDash \{A \land b\}\ c_0\ \{B\}$，$\vDash \{A \land \lnot b\}\ c_1\ \{B\}$。令 $I$ 是一个解释。假设 $\sigma \vDash_I A$。如果 $\sigma \vDash_I b$，则 $\sigma \vDash_I A \land b$，所以 $C[c_0]\ \sigma \vDash_I B$。如果 $\sigma \vDash_I \lnot b$，则 $\sigma \vDash_I A \land \lnot b$，所以 $C[c_1]\ \sigma \vDash_I B$。因此，无论哪个分支，执行条件语句后都得到 $B$，所以 $\vDash \{A\}\ \textbf{if}\ b\ \textbf{then}\ c_0\ \textbf{else}\ c_1\ \{B\}$。

- **while 循环的规则**：假设 $\vDash \{A \land b\}\ c\ \{A\}$，即 $A$ 是循环的不变式。我们需要证明 $\vDash \{A\}\ \textbf{while}\ b\ \textbf{do}\ c\ \{A \land \lnot b\}$。

  **证明**：

  - 定义 $w \equiv \textbf{while}\ b\ \textbf{do}\ c$，令 $C[w]$ 为 $w$ 的指称语义。

  - 我们需要证明，对于所有状态 $\sigma$，如果 $\sigma \vDash_I A$，则 $C[w]\ \sigma \vDash_I A \land \lnot b$。

  - 通过数学归纳法证明，对于所有 $n \in \mathbb{N}$，对于 $C[w]$ 的展开 $C_n$，都有：

    $$
    \sigma \vDash_I A \quad \implies \quad C_n\ \sigma \vDash_I A \land \lnot b
    $$

  - 基础情况 $n = 0$：$C_0$ 对应于不执行任何循环体，直接检查 $b$ 是否为假。如果 $\sigma \vDash_I A$ 且 $\sigma \vDash_I \lnot b$，则 $C_0\ \sigma = \sigma$，且 $\sigma \vDash_I A \land \lnot b$。

  - 归纳步骤：假设对于 $n$ 成立，证明对于 $n + 1$ 也成立。

    - 如果 $\sigma \vDash_I A$ 且 $\sigma \vDash_I b$，则执行一次循环体 $c$，得到新的状态 $\sigma'$。

    - 由于 $\vDash \{A \land b\}\ c\ \{A\}$，所以 $\sigma' \vDash_I A$。

    - 然后，对 $\sigma'$ 应用归纳假设，得到 $C_n\ \sigma' \vDash_I A \land \lnot b$。

  - 因此，$\vDash \{A\}\ w\ \{A \land \lnot b\}$，证明了 while 循环规则的健全性。

- **后果规则（推理规则）**：假设 $\vDash (A \implies A')$，$\vDash \{A'\}\ c\ \{B'\}$，$\vDash (B' \implies B)$。令 $I$ 是一个解释。假设 $\sigma \vDash_I A$。由于 $\vDash (A \implies A')$，所以 $\sigma \vDash_I A'$。又由于 $\vDash \{A'\}\ c\ \{B'\}$，所以 $C[c]\ \sigma \vDash_I B'$。由于 $\vDash (B' \implies B)$，所以 $C[c]\ \sigma \vDash_I B$。因此，$\vDash \{A\}\ c\ \{B\}$，证明了后果规则的健全性。

综上所述，通过规则归纳，Hoare 规则的每个定理都是有效的。

---

**练习 6.12**

使用操作语义而不是指称语义来证明上述结果。对于 while 循环的情况，使用了什么证明方法？

**解答**：

- 当使用操作语义时，我们需要基于命令的执行步骤来证明规则的健全性。

- **对于 while 循环的规则**，证明方法是使用**全程归纳法（total induction）**，也称为**弱化的归纳法**。

  - 我们需要证明，对于任意执行步骤数 $n$，如果从满足 $A$ 的状态开始执行 $w$，经过 $n$ 步执行后，要么循环继续执行，要么结束在满足 $A \land \lnot b$ 的状态。

  - 通过对执行步骤数 $n$ 进行归纳，类似于在指称语义中对展开次数 $n$ 进行归纳。

---

## 6.6 使用 Hoare 规则的示例

Hoare 规则通过推导的概念确定了部分正确性断言的形式证明。这在证明的机械化中是有用的。但是在实践中，作为需要验证程序的人类，我们不必如此严格，可以在使用 Hoare 规则时在更非正式的层面上进行推理。（实际上，使用更正式的推导概念可能会分散获取证明的注意力；将生成形式推导的任务委托给像 LCF 或 HOL 这样的证明辅助工具可能更好 [74][43]。）

作为一个示例，我们将详细展示如何使用 Hoare 规则来验证以下命令：

$$
w \equiv \textbf{while}\ X > 0\ \textbf{do}\ Y := X \times Y;\ X := X - 1
$$

该命令确实计算了阶乘函数 $n! = n \times (n - 1) \times (n - 2) \times \dots \times 2 \times 1$，其中 $0!$ 被理解为 $1$，假设初始时 $X = n$，$n$ 是非负整数，且 $Y = 1$。

更精确地，我们希望证明：

$$
\{X = n \land n \geq 0 \land Y = 1\}\ w\ \{Y = n!\}
$$

为了证明这一点，我们显然需要使用 while 循环的证明规则，需要一个不变式。取：

$$
I \equiv (Y \times X! = n! \land X \geq 0)
$$

我们需要证明 $I$ 确实是不变式，即：

$$
\{I \land X > 0\}\ Y := X \times Y;\ X := X - 1\ \{I\}
$$

从赋值语句的规则，我们有：

1. 对于 $X := X - 1$，有：

   $$
   \{I[X - 1 / X]\}\ X := X - 1\ \{I\}
   $$

   其中 $I[X - 1 / X] \equiv (Y \times (X - 1)! = n! \land X - 1 \geq 0)$。

2. 对于 $Y := X \times Y$，有：

   $$
   \{X \times Y \times (X - 1)! = n! \land X - 1 \geq 0\}\ Y := X \times Y\ \{I[X - 1 / X]\}
   $$

因此，通过顺序组合的规则，我们有：

$$
\{X \times Y \times (X - 1)! = n! \land X - 1 \geq 0\}\ Y := X \times Y;\ X := X - 1\ \{I\}
$$

接下来，我们需要证明以下蕴含成立：

$$
I \land X > 0 \implies X \times Y \times (X - 1)! = n! \land X - 1 \geq 0 \tag{*}
$$

**证明**：

- 已知 $I$，即 $Y \times X! = n!$，且 $X \geq 0$。

- 由于 $X > 0$，所以 $X \geq 1$，因此 $X - 1 \geq 0$。

- 我们有：

  $$
  Y \times X! = n! \implies X \times Y \times (X - 1)! = X \times (Y \times (X - 1)!) = X \times Y \times (X - 1)! = n!
  $$

因此，蕴含成立。

因此，通过后果规则，我们有：

$$
\{I \land X > 0\}\ Y := X \times Y;\ X := X - 1\ \{I\}
$$

这表明 $I$ 是不变式。

现在，应用 while 循环的规则，我们得到：

$$
\{I\}\ w\ \{I \land X \leq 0\}
$$

显然，初始时 $X = n$，$n \geq 0$，$Y = 1$，因此 $I$ 成立。

当循环结束时，我们有：

$$
I \land X \leq 0 \implies Y \times X! = n! \land X \leq 0
$$

由于 $X \leq 0$ 且 $X \geq 0$（因为 $I$ 包含 $X \geq 0$），所以 $X = 0$。因此：

$$
Y \times 0! = Y = n!
$$

因为 $0! = 1$，所以 $Y = n!$。

因此，通过后果规则，我们得出：

$$
\{X = n \land n \geq 0 \land Y = 1\}\ w\ \{Y = n!\}
$$

**注意事项**：

- 在处理顺序组合的命令时，通常从右到左进行，因为赋值语句的规则是这种性质的。

- 我们的选择的 $I$ 可能看起来过于强烈。为什么我们在不变式中包含了 $X \geq 0$？注意我们在哪里使用了它，在 (*) 处，如果没有它，我们无法推导出在退出 while 循环时 $X = 0$。

- 在获取不变式以证明我们想要的内容时，它们通常必须被加强。它们就像归纳假设。加强不变式的一种明显方法是尽可能精确地指定变量和位置的值的范围。

---

## 练习

**练习 6.13**

使用 Hoare 规则，证明以下部分正确性断言的正确性：

$$
\begin{align*}
&\{1 \leq N\} \\
&P := 0; \\
&C := 1; \\
&\textbf{while}\ C \leq N\ \textbf{do}\ P := P + M;\ C := C + 1 \\
&\{P = M \times N\}
\end{align*}
$$

**解答**：

我们需要找到一个适当的循环不变式来应用 while 循环的规则。令不变式为：

$$
I \equiv P = M \times (C - 1) \land 1 \leq C \leq N + 1
$$

**步骤**：

1. **初始化**：

   - 执行 $P := 0;\ C := 1$ 后，$P = 0$，$C = 1$。
   - 检查初始条件是否满足不变式 $I$：

     $$
     P = 0 = M \times 0 = M \times (C - 1)
     $$

     且 $1 \leq C \leq N + 1$。

2. **保持不变式**：

   - 我们需要证明：

     $$
     \{I \land C \leq N\}\ P := P + M;\ C := C + 1\ \{I\}
     $$

   - 执行 $P := P + M$ 后，$P_{\text{new}} = P + M$。

   - 执行 $C := C + 1$ 后，$C_{\text{new}} = C + 1$。

   - 需要验证：

     $$
     P_{\text{new}} = M \times (C_{\text{new}} - 1)
     $$

     即：

     $$
     P + M = M \times ((C + 1) - 1) = M \times C
     $$

     由于 $P = M \times (C - 1)$，所以：

     $$
     P + M = M \times (C - 1) + M = M \times (C - 1 + 1) = M \times C
     $$

     因此，不变式在循环体执行后仍然成立。

   - 还需要验证 $1 \leq C_{\text{new}} \leq N + 1$。由于 $C \leq N$，所以 $C_{\text{new}} = C + 1 \leq N + 1$。

3. **终止条件**：

   - 当 $C > N$ 时，循环终止。根据不变式，$C = N + 1$，因此：

     $$
     P = M \times (C - 1) = M \times N
     $$

   - 因此，退出循环后，$P = M \times N$。

因此，证明了部分正确性断言的正确性。

---

**练习 6.14**

找到适当的不变式，用于在证明以下部分正确性断言时应用 while 规则：

$$
\{i = Y\}\ \textbf{while}\ \lnot (Y = 0)\ \textbf{do}\ Y := Y - 1;\ X := 2 \times X\ \{X = 2^i\}
$$

**解答**：

令不变式为：

$$
I \equiv X = 2^{i - Y}
$$

**证明**：

1. **初始条件**：

   - 初始时，$Y = i$，所以：

     $$
     X = 2^{i - Y} = 2^{i - i} = 2^0 = 1
     $$

     需要在初始状态下满足 $X$ 的初始值为 $1$。

2. **保持不变式**：

   - 在循环体中，执行 $Y := Y - 1$，$X := 2 \times X$。

   - 需要证明：

     $$
     \{I \land Y \ne 0\}\ Y := Y - 1;\ X := 2 \times X\ \{I\}
     $$

   - 执行 $Y := Y - 1$ 后，$Y_{\text{new}} = Y - 1$。

   - 执行 $X := 2 \times X$ 后，$X_{\text{new}} = 2 \times X$。

   - 验证：

     $$
     X_{\text{new}} = 2 \times X = 2 \times 2^{i - Y} = 2^{i - Y + 1} = 2^{i - Y_{\text{new}}}
     $$

     因此，不变式在循环体后仍然成立。

3. **终止条件**：

   - 当 $Y = 0$ 时，循环终止。不变式给出：

     $$
     X = 2^{i - 0} = 2^i
     $$

   - 因此，退出循环后，$X = 2^i$。

因此，证明了部分正确性断言的正确性。

---

**练习 6.15**

使用 Hoare 规则，证明对于整数 $n, m$，

$$
\{X = m \land Y = n \land Z = 1\}\ c\ \{Z = m^n\}
$$

其中 $c$ 是如下的 while 程序：

$$
\textbf{while}\ \lnot (Y = 0)\ \textbf{do} \\
\quad (\textbf{while}\ \text{even}(Y)\ \textbf{do}\ X := X \times X;\ Y := Y / 2); \\
\quad Z := Z \times X; \\
\quad Y := Y - 1
$$

其中 $Y / 2$ 表示将 $Y$ 的值除以 $2$，取整，$\text{even}(Y)$ 表示 $Y$ 的值是偶数。

**提示**：使用 $m^n = Z \times X^Y$ 作为不变式。

**解答**：

令不变式为：

$$
I \equiv Z \times X^Y = m^n \land Y \geq 0
$$

**证明思路**：

- **初始条件**：当 $X = m$，$Y = n$，$Z = 1$ 时，

  $$
  Z \times X^Y = 1 \times m^n = m^n
  $$

  因此，不变式在初始时成立。

- **外层 while 循环**：

  - 需要证明 $\{I \land Y \ne 0\}\ \text{循环体}\ \{I\}$。

- **内层 while 循环（快速幂算法的一部分）**：

  - 内层循环的作用是将 $Y$ 中的偶数因子除去，同时将 $X$ 平方。

  - 在内层循环中，不变式需要保持。

- **循环体之后**：

  - 执行 $Z := Z \times X$，$Y := Y - 1$。

  - 更新不变式：

    $$
    Z_{\text{new}} \times X^{Y_{\text{new}}} = (Z \times X) \times X^{Y - 1} = Z \times X^{Y} = m^n
    $$

- **终止条件**：

  - 当 $Y = 0$ 时，循环终止。不变式给出：

    $$
    Z \times X^0 = Z = m^n
    $$

因此，退出循环后，$Z = m^n$。

---

**练习 6.16**

(i) 显示两个正整数 $n, m$ 的最大公约数 $\text{gcd}(n, m)$ 满足：

(a) 如果 $n > m$，则 $\text{gcd}(n, m) = \text{gcd}(n - m, m)$。

(b) $\text{gcd}(n, m) = \text{gcd}(m, n)$。

(c) $\text{gcd}(n, n) = n$。

**解答**：

(a) 证明：如果 $n > m$，则 $\text{gcd}(n, m) = \text{gcd}(n - m, m)$。

- 任何能同时整除 $n$ 和 $m$ 的数也能整除 $n - m$ 和 $m$，反之亦然。

(b) 证明：$\text{gcd}(n, m) = \text{gcd}(m, n)$。

- 最大公约数在参数交换时不变。

(c) 证明：$\text{gcd}(n, n) = n$。

- 因为 $n$ 是 $n$ 的最大公约数。

(ii) 使用 Hoare 规则证明：

$$
\{N = n \land M = m \land 1 \leq n \land 1 \leq m\}\ \text{Euclid}\ \{X = \text{gcd}(n, m)\}
$$

其中：

$$
\text{Euclid} \equiv \textbf{while}\ \lnot (M = N)\ \textbf{do} \\
\quad \textbf{if}\ M \leq N\ \textbf{then}\ N := N - M\ \textbf{else}\ M := M - N
$$

**提示**：使用不变式 $X = \text{gcd}(N, M)$。

**解答**：

- 令不变式为：

  $$
  \text{gcd}(N, M) = \text{gcd}(n, m) \land N \geq 1 \land M \geq 1
  $$

- **初始条件**：$N = n$，$M = m$，满足不变式。

- **循环体**：

  - 当 $N > M$，执行 $N := N - M$。

    - 根据 (a)，$\text{gcd}(N - M, M) = \text{gcd}(N, M)$。

  - 当 $M > N$，执行 $M := M - N$。

    - 同理。

- **保持不变式**：在每次循环后，$\text{gcd}(N, M)$ 保持不变。

- **终止条件**：当 $N = M$ 时，循环终止，$N = M = \text{gcd}(n, m)$。

- **最终结果**：设 $X = N$，因此 $X = \text{gcd}(n, m)$。

---

**练习 6.17**

为 `repeat` 构造提供一个 Hoare 规则，并证明其健全性。（参见练习 5.9。）

**解答**：

- `repeat` 结构的语法：

  $$
  \textbf{repeat}\ c\ \textbf{until}\ b
  $$

- **Hoare 规则**：

  $$
  \begin{align*}
  &\{A\}\ c\ \{A'\} \\
  &\{A'\}\ \textbf{if}\ \lnot b\ \textbf{then}\ \textbf{repeat}\ c\ \textbf{until}\ b\ \{B\} \\
  \hline
  &\{A\}\ \textbf{repeat}\ c\ \textbf{until}\ b\ \{B\}
  \end{align*}
  $$

  或者，更简单的规则：

  $$
  \begin{align*}
  &\{A\}\ c\ \{A'\} \\
  &\{A' \land \lnot b\}\ \textbf{repeat}\ c\ \textbf{until}\ b\ \{B\} \\
  \hline
  &\{A\}\ \textbf{repeat}\ c\ \textbf{until}\ b\ \{A' \land b\}
  \end{align*}
  $$

- **证明健全性**：

  - 证明方法类似于 while 循环的规则，使用全程归纳法，证明在循环结束时，不变式 $A'$ 成立，并且条件 $b$ 为真。

---

## 6.7 进一步阅读

我们已经提到了 Gries 的书 [44]。Dijkstra 的《程序设计的纪律》[36] 具有很大的影响力。Backhouse 的《程序构造和验证》[12] 是同一领域的一本更基础的书。Cohen 的《90 年代的编程》[32] 是一本值得推荐的新书。Alagic 和 Arbib 的《良好结构和正确程序的设计》[5] 是一本包含许多练习的好书。Gordon 最近的书 [42] 对 Hoare 逻辑进行了初步处理，并进行了大量信息性的讨论。de Bakker 的《程序正确性的数学理论》[13] 和 Loeckx 与 Sieber 的《程序验证的基础》[58] 提供了与本书处理方式不同的替代方案，更侧重于语义问题。

---

**总结**

在本节中，我们介绍了 Hoare 逻辑的证明规则，并证明了它们的健全性。我们通过示例展示了如何使用这些规则来验证程序的部分正确性。我们还完成了几个练习，以加深对 Hoare 规则的理解和应用。

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