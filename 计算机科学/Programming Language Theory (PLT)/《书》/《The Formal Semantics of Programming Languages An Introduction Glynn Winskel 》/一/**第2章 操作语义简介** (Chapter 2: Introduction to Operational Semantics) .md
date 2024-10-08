[toc]



# 第2章 操作语义简介

本章介绍了一种名为 **IMP** 的编程语言的语法，这是一种简单的 **While** 程序语言。IMP 被称为一种**命令式（imperative）**语言，因为其程序的执行涉及执行一系列显式的命令来改变状态。从形式上讲，IMP 的行为由规则描述，这些规则指定了其表达式如何被求值，以及其命令如何被执行。这些规则为 IMP 提供了一种**操作语义（operational semantics）**，因为它们接近于该语言的实现，例如，可以在编程语言 Prolog 中实现。此外，这些规则还为命令之间的等价性提供了简单的证明基础。

## 2.1 IMP——一个简单的命令式语言

首先，我们列出与 IMP 相关的语法集：

- **数字（numbers）** $N$：包含正整数、负整数和零。
- **真值（truth values）** $T = \{\text{true}, \text{false}\}$。
- **位置（locations）** $\text{Loc}$。
- **算术表达式（arithmetic expressions）** $\text{Aexp}$。
- **布尔表达式（boolean expressions）** $\text{Bexp}$。
- **命令（commands）** $\text{Com}$。

### **基本元素的定义**

我们假设数字和位置的语法结构是给定的。例如，集合 $\text{Loc}$ 可以由非空的字母串或这些字母串后跟数字组成，而 $N$ 可以是正负整数的十进制表示——实际上，这些是我们在考虑具体示例时使用的表示方式。（位置通常被称为程序变量，但我们保留该术语用于另一个概念。）

### **其他语法集的构建**

对于其他语法集，我们需要说明它们的元素是如何构建的。我们将使用一种 BNF（巴科斯-诺尔范式，Backus-Naur Form）的变体，作为写下这些语法集的构成规则的一种方式。构成规则将表达类似：

- **如果 $a_0$ 和 $a_1$ 是算术表达式，那么 $a_0 + a_1$ 也是算术表达式。**

显然，符号 $a_0$ 和 $a_1$ 被用于表示任何算术表达式。在我们对语法的非正式介绍中，我们将使用这样的**元变量（metavariables）**来表示语法集的元素——上面的元变量 $a_0$ 和 $a_1$ 被理解为取自算术表达式的集合。

在呈现 IMP 的语法时，我们遵循以下约定：

- $n, m$ 代表数字集合 $N$ 中的元素。
- $X, Y$ 代表位置集合 $\text{Loc}$ 中的元素。
- $a$ 代表算术表达式集合 $\text{Aexp}$ 中的元素。
- $b$ 代表布尔表达式集合 $\text{Bexp}$ 中的元素。
- $c$ 代表命令集合 $\text{Com}$ 中的元素。

我们使用的元变量可以带撇号或下标。例如，$X, X', X_0, X_1, Y''$ 都代表位置。

### **构造算术表达式**

我们通过以下规则描述算术表达式 $\text{Aexp}$ 的构造：

$$
a ::= n\ |\ X\ |\ a_0 + a_1\ |\ a_0 - a_1\ |\ a_0 \times a_1
$$

这里，符号“$::=$”应读作“可以是（can be）”，符号“$|$”表示“或（or）”。因此，一个算术表达式 $a$ 可以是一个数字 $n$，或者一个位置 $X$，或者由算术表达式 $a_0$ 和 $a_1$ 通过加、减、乘运算构成的新表达式 $a_0 + a_1$、$a_0 - a_1$、$a_0 \times a_1$。

**注意**：我们的算术表达式的构造规则并没有告诉我们如何解析表达式，例如 $2 + 3 \times 4 - 5$，是解析为 $2 + ((3 \times 4) - 5)$ 还是解析为 $(2 + 3) \times (4 - 5)$ 等。这种表示给出了所谓的**抽象语法（abstract syntax）**，它只是说明如何构建新的算术表达式。对于我们写下的任何算术表达式，我们需要添加足够的括号，以确保它以唯一的方式构建。

将抽象语法视为指定了语言的**语法树（parse trees）**是有帮助的；而具体语法（concrete syntax）的工作是通过括号或操作符的优先级来提供足够的信息，以便唯一地解析一个字符串。我们的关注点是编程语言的含义，而不是如何书写它们的理论。抽象语法对于我们的目的已经足够。

### **IMP 的完整构造规则**

以下是 IMP 的完整构造规则：

#### **算术表达式（Aexp）**：

$$
a ::= n\ |\ X\ |\ a_0 + a_1\ |\ a_0 - a_1\ |\ a_0 \times a_1
$$

#### **布尔表达式（Bexp）**：

$$
b ::= \text{true}\ |\ \text{false}\ |\ a_0 = a_1\ |\ a_0 \leq a_1\ |\ \neg b\ |\ b_0\ \&\ b_1\ |\ b_0\ \text{or}\ b_1
$$

#### **命令（Com）**：

$$
c ::= \text{skip}\ |\ X := a\ |\ c_0;\ c_1\ |\ \text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1\ |\ \text{while}\ b\ \text{do}\ c
$$

从集合论的角度来看，这种表示为 IMP 的语法集提供了一个**归纳定义（inductive definition）**，即这些集合是满足构造规则的最小集合，其含义将在接下来的两章中明确。目前，这种表示应该被视为仅仅告诉我们如何构造语法集的元素。

### **语法元素的同一性**

我们需要一些符号来表示当两个相同语法集的元素 $e_0$ 和 $e_1$ **完全相同**时（即根据抽象语法以完全相同的方式构建，或者等价地，具有相同的语法树）。我们使用 $e_0 \equiv e_1$ 表示 $e_0$ 与 $e_1$ 完全相同。

例如，由数字 $3$ 和 $5$ 构建的算术表达式 $3 + 5$ 与表达式 $8$ 或 $5 + 3$ 在语法上并不相同，尽管我们期望它们计算的结果是相同的数字。因此，我们没有 $3 + 5 \equiv 5 + 3$。注意，我们确实有 $(3 + 5) \equiv 3 + 5$，因为括号并未改变表达式的构建方式。

**练习 2.1**：如果你熟悉编程语言 ML（例如参见 [101]）或 Miranda（例如参见 [22]），请将 IMP 的语法集定义为数据类型。如果你熟悉编程语言 Prolog（例如参见 [31]），请在其中编程实现 IMP 的构造规则。编写一个程序来检查语法元素 $e_0$ 和 $e_1$ 是否满足 $e_0 \equiv e_1$。

---

以上是关于 IMP 的语法部分。现在让我们转向它的语义，即当我们运行程序时，它们的行为如何。

## 2.2 算术表达式的求值

读者可能已经对用 IMP 编写的程序的行为有一个直观的模型。大多数模型的基础是**状态（state）**的概念，状态由位置中的内容决定。相对于一个状态，一个算术表达式计算为一个整数，一个布尔表达式计算为一个真值。结果值可以影响命令的执行，从而导致状态的改变。我们对 IMP 的行为的形式化描述将遵循这一思路。首先，我们定义状态，然后定义整数和布尔表达式的求值，最后是命令的执行。

### **状态的定义**

状态集合 $E$ 由从位置到数字的函数组成，即：

$$
E = \{\sigma\ |\ \sigma: \text{Loc} \rightarrow N\}
$$

因此，$\sigma(X)$ 是在状态 $\sigma$ 下位置 $X$ 的值或内容。

### **算术表达式的求值表示**

考虑在状态 $\sigma$ 下对算术表达式 $a$ 进行求值。我们可以用一个二元组 $(a, \sigma)$ 来表示表达式 $a$ 在状态 $\sigma$ 下等待被求值的情况。我们将定义一个**求值关系（evaluation relation）**，在这些二元组与数字之间建立联系：

$$
(a, \sigma) \rightarrow n
$$

这意味着：在状态 $\sigma$ 下，表达式 $a$ 求值为数字 $n$。我们将 $(a, \sigma)$ 称为**算术表达式配置（arithmetic-expression configuration）**，其中 $a$ 是算术表达式，$\sigma$ 是状态。

### **求值规则的制定**

考虑我们如何向某人解释如何求值一个算术表达式 $a_0 + a_1$。我们可能会这样说：

1. 计算 $a_0$，得到结果数字 $n_0$。
2. 计算 $a_1$，得到结果数字 $n_1$。
3. 然后，将 $n_0$ 和 $n_1$ 相加，得到结果 $n$，这就是 $a_0 + a_1$ 的值。

尽管这是非正式的，但我们可以看到，这指定了如何根据其加数来求值一个加法表达式；该规范是**语法导向的（syntax-directed）**。求值关系的形式化规范是通过规则给出的，这些规则非常接近上述直观和非正式的描述。

#### **求值规则**

我们以语法导向的方式指定求值关系，规则如下：

1. **数字的求值**：

   $$
   \frac{}{\ (n, \sigma) \rightarrow n}
   $$

   即，任何数字在状态 $\sigma$ 下求值为其自身。

2. **位置的求值**：

   $$
   \frac{}{\ (X, \sigma) \rightarrow \sigma(X)}
   $$

   即，位置在状态下的值是其在该状态下的内容。

3. **加法的求值**：

   $$
   \frac{\ (a_0, \sigma) \rightarrow n_0 \quad (a_1, \sigma) \rightarrow n_1}{\ (a_0 + a_1, \sigma) \rightarrow n}
   $$

   其中，$n$ 是 $n_0$ 和 $n_1$ 的和，即 $n = n_0 + n_1$。

4. **减法的求值**：

   $$
   \frac{\ (a_0, \sigma) \rightarrow n_0 \quad (a_1, \sigma) \rightarrow n_1}{\ (a_0 - a_1, \sigma) \rightarrow n}
   $$

   其中，$n$ 是 $n_0$ 减去 $n_1$ 的结果，即 $n = n_0 - n_1$。

5. **乘法的求值**：

   $$
   \frac{\ (a_0, \sigma) \rightarrow n_0 \quad (a_1, \sigma) \rightarrow n_1}{\ (a_0 \times a_1, \sigma) \rightarrow n}
   $$

   其中，$n$ 是 $n_0$ 和 $n_1$ 的乘积，即 $n = n_0 \times n_1$。

### **规则的解读**

我们如何阅读这些规则？以加法的规则为例，可以读作：

- **如果** $(a_0, \sigma) \rightarrow n_0$ **且** $(a_1, \sigma) \rightarrow n_1$，**那么** $(a_0 + a_1, \sigma) \rightarrow n$，其中 $n$ 是 $n_0$ 和 $n_1$ 的和。

规则有**前提（premises）**和**结论（conclusion）**，我们遵循常见的做法，将前提写在实线的上方，结论写在实线的下方。在推导（derivations）中，规则将被应用，其中线下的事实从线上面的事实推导而来。

一些规则（如数字和位置的求值规则）不需要前提，有时我们会在横线上写出它们，例如：

$$
\frac{}{\ (n, \sigma) \rightarrow n}
$$

带有空前提的规则称为**公理（axioms）**。给定任何算术表达式 $a$、状态 $\sigma$ 和数字 $n$，如果能够从这些规则出发，从公理开始推导，我们就认为 $a$ 在 $\sigma$ 下求值为 $n$，即 $(a, \sigma) \rightarrow n$。

加法的规则表达了两个表达式的和求值为通过对加数求值得到的两个数字的和。它没有解释获取两个数字的和的机制。我选择不详细分析数字的构造方式，以上规则仅表达了如何从表达式中消除位置和运算符 $+$、$-$、$\times$，以得到它们求值的数字。另一方面，如果我们选择描述一个特定的数字系统，如十进制或罗马数字，那么需要进一步的规则来指定乘法等操作。在考虑硬件设备时，这种描述级别可能很重要。但在这里，我们想要避免这些细节——我们都知道如何做简单的算术！

### **推导树和推导**

这些求值规则使用了元变量 $n$、$X$、$a_0$、$a_1$，它们取自适当的语法集，以及状态 $\sigma$。通过将这些元变量实例化为特定的数字、位置、表达式和状态，可以得到**规则实例（rule instances）**。

例如，当 $\sigma$ 是特定的状态，每个位置都具有值 $a$ 时，以下是一个规则实例：

$$
\frac{}{\ (2, \sigma) \rightarrow 2}
$$

另一个实例是：

$$
\frac{\ (2, \sigma) \rightarrow 2 \quad (3, \sigma) \rightarrow 3}{\ (2 \times 3, \sigma) \rightarrow 6}
$$

但不能出现这样的实例：

$$
\frac{\ (2, \sigma) \rightarrow 3 \quad (3, \sigma) \rightarrow 4}{\ (2 \times 3, \sigma) \rightarrow 12}
$$

因为在此情况下，前提或结论无法被推导出来。

为了查看推导的结构，考虑在状态 $\sigma$ 下求值算术表达式 $a = (X_{\text{Init}} + 5) + (7 + 9)$，其中 $X_{\text{Init}}$ 是一个位置，$\sigma(X_{\text{Init}}) = 0$。查看规则，我们发现这需要对 $(X_{\text{Init}} + 5)$ 和 $(7 + 9)$ 进行求值，而这些又可能取决于其他求值。实际上，对 $(a, \sigma)$ 的求值可以看作依赖于一个求值的树形结构：

第一层：

$$
\frac{}{\ (X_{\text{Init}}, \sigma) \rightarrow 0} \quad \frac{}{\ (5, \sigma) \rightarrow 5} \quad \frac{}{\ (7, \sigma) \rightarrow 7} \quad \frac{}{\ (9, \sigma) \rightarrow 9}
$$

第二层：

$$
\frac{\ (X_{\text{Init}}, \sigma) \rightarrow 0 \quad (5, \sigma) \rightarrow 5}{\ (X_{\text{Init}} + 5, \sigma) \rightarrow 5} \quad \frac{\ (7, \sigma) \rightarrow 7 \quad (9, \sigma) \rightarrow 9}{\ (7 + 9, \sigma) \rightarrow 16}
$$

第三层：

$$
\frac{\ (X_{\text{Init}} + 5, \sigma) \rightarrow 5 \quad (7 + 9, \sigma) \rightarrow 16}{\ ((X_{\text{Init}} + 5) + (7 + 9), \sigma) \rightarrow 21}
$$

我们称这样的结构为**推导树（derivation tree）**，或简称**推导（derivation）**。它是由规则实例构建的，使得规则的所有前提都是其上方规则实例的结论，因此最顶部的是公理，即上方没有前提的横线。最底层规则的结论称为推导的**结论**。当某个事实有一个以它为结论的推导时，我们称它是从规则中推导出来的。

一般而言，我们写 $(a, \sigma) \rightarrow n$，并说 $a$ 在 $\sigma$ 下求值为 $n$，当且仅当它可以从算术表达式的求值规则中推导出来。上述特定的推导以：

$$
((X_{\text{Init}} + 5) + (7 + 9), \sigma) \rightarrow 21
$$

为结论。因此，$(X_{\text{Init}} + 5) + (7 + 9)$ 在状态 $\sigma$ 下求值为 $21$，这正是我们想要的。

### **求值的构建方法**

考虑在某个状态 $\sigma$ 下求值算术表达式 $a$ 的问题。这相当于找到一个推导，使得结论的左部分与 $(a, \sigma)$ 匹配。构建推导的最佳方法是尝试以向上（自底向上）的方式构建推导：首先找到一个结论与 $(a, \sigma)$ 匹配的规则；如果这是一个公理，则推导完成；否则，尝试从前提向上构建推导，如果成功，则填写第一个规则的结论，完成以 $(a, \sigma) \rightarrow n$ 为结论的推导。

尽管在算术表达式的求值中不会发生，但一般而言，可能有多个规则的左部分与给定的配置匹配。为了保证找到一个具有匹配结论的推导树（如果存在的话），必须考虑所有左部分匹配配置的规则，看看它们是否可以成为推导的结论。所有可能的具有正确形式结论的推导都必须“并行地”构建。

通过这种方式，这些规则为算术表达式的求值提供了一种基于寻找推导树的算法。由于它可以被相当直接地实现，这些规则以操作的方式指定了算术表达式的含义，或者说语义，因此这些规则被称为这样的表达式的**操作语义**。

当然，还有其他方式可以给出表达式的含义，并且可以直接导致实现。我们选择的方式只是其中之一——任何实现的详细描述也是一种操作语义。然而，我们选择的语义风格正在变得普遍。这种方式通常被称为**结构化操作语义（structural operational semantics）**，因为规则是以语法导向的方式呈现的。它也被称为**自然语义（natural semantics）**，因为推导与自然演绎（natural deduction）中的证明类似——一种构建形式证明的方法。我们将在后面看到更复杂、更有说服力的操作语义示例。

### **表达式的等价关系**

求值关系决定了表达式上的一种自然的等价关系。定义：

$$
a_0 \simeq a_1 \quad \text{当且仅当} \quad \forall n \in N, \forall \sigma \in E.\ (a_0, \sigma) \rightarrow n \Leftrightarrow (a_1, \sigma) \rightarrow n
$$

这意味着，如果在所有状态下，$a_0$ 和 $a_1$ 求值为相同的值，那么它们是等价的。

**练习 2.2**：用 Prolog 和/或 ML（或你选择的其他语言）编程实现算术表达式的求值规则。当然，这需要在 Prolog 和/或 ML 中表示这些表达式的抽象语法。

---

## 2.3 布尔表达式的求值

我们使用以下规则来将布尔表达式求值为真值（$\text{true}$、$\text{false}$）：

1. **真值的求值**：

   $$
   \frac{}{\ (\text{true}, \sigma) \rightarrow \text{true}} \quad \frac{}{\ (\text{false}, \sigma) \rightarrow \text{false}}
   $$

2. **等于运算的求值**：

   $$
   \frac{\ (a_0, \sigma) \rightarrow n \quad (a_1, \sigma) \rightarrow m}{\ (a_0 = a_1, \sigma) \rightarrow \text{true}}
   $$

   如果 $n$ 和 $m$ **相等**。

   $$
   \frac{\ (a_0, \sigma) \rightarrow n \quad (a_1, \sigma) \rightarrow m}{\ (a_0 = a_1, \sigma) \rightarrow \text{false}}
   $$

   如果 $n$ 和 $m$ **不相等**。

3. **小于等于运算的求值**：

   $$
   \frac{\ (a_0, \sigma) \rightarrow n \quad (a_1, \sigma) \rightarrow m}{\ (a_0 \leq a_1, \sigma) \rightarrow \text{true}}
   $$

   如果 $n \leq m$。

   $$
   \frac{\ (a_0, \sigma) \rightarrow n \quad (a_1, \sigma) \rightarrow m}{\ (a_0 \leq a_1, \sigma) \rightarrow \text{false}}
   $$

   如果 $n > m$。

4. **否定的求值**：

   $$
   \frac{\ (b, \sigma) \rightarrow \text{true}}{\ (\neg b, \sigma) \rightarrow \text{false}} \quad \frac{\ (b, \sigma) \rightarrow \text{false}}{\ (\neg b, \sigma) \rightarrow \text{true}}
   $$

5. **与运算的求值**：

   $$
   \frac{\ (b_0, \sigma) \rightarrow t_0 \quad (b_1, \sigma) \rightarrow t_1}{\ (b_0\ \&\ b_1, \sigma) \rightarrow t}
   $$

   其中，当 $t_0 = \text{true}$ 且 $t_1 = \text{true}$ 时，$t = \text{true}$，否则 $t = \text{false}$。

6. **或运算的求值**：

   $$
   \frac{\ (b_0, \sigma) \rightarrow t_0 \quad (b_1, \sigma) \rightarrow t_1}{\ (b_0\ \text{or}\ b_1, \sigma) \rightarrow t}
   $$

   其中，当 $t_0 = \text{true}$ 或 $t_1 = \text{true}$ 时，$t = \text{true}$，否则 $t = \text{false}$。

### **规则的说明**

这些规则告诉我们如何消除所有布尔运算符和连接词，从而将一个布尔表达式简化为一个真值。

### **布尔表达式的等价关系**

同样，存在一个布尔表达式上的自然等价关系。两个表达式在所有状态下求值为相同的真值时，它们是等价的。定义：

$$
b_0 \simeq b_1 \quad \text{当且仅当} \quad \forall t \in T, \forall \sigma \in E.\ (b_0, \sigma) \rightarrow t \Leftrightarrow (b_1, \sigma) \rightarrow t
$$

### **更高效的求值策略**

可能有人会担心，我们的表达式求值方法不是最有效的。例如，根据当前的规则，为了求值一个与运算 $b_0\ \&\ b_1$，我们必须求值 $b_0$ 和 $b_1$，这在 $b_0$ 求值为 $\text{false}$ 时显然是不必要的，因为无论 $b_1$ 的结果如何，整个表达式都会是 $\text{false}$。一种更有效的求值策略是首先求值 $b_0$，只有在其求值结果为 $\text{true}$ 时才继续求值 $b_1$。我们可以将这种策略称为**左优先顺序求值（left-first-sequential evaluation）**。其求值规则为：

1. **当 $b_0$ 求值为 $\text{false}$ 时**：

   $$
   \frac{\ (b_0, \sigma) \rightarrow \text{false}}{\ (b_0\ \&\ b_1, \sigma) \rightarrow \text{false}}
   $$

2. **当 $b_0$ 求值为 $\text{true}$，且 $b_1$ 求值为 $\text{false}$ 时**：

   $$
   \frac{\ (b_0, \sigma) \rightarrow \text{true} \quad (b_1, \sigma) \rightarrow \text{false}}{\ (b_0\ \&\ b_1, \sigma) \rightarrow \text{false}}
   $$

3. **当 $b_0$ 求值为 $\text{true}$，且 $b_1$ 求值为 $\text{true}$ 时**：

   $$
   \frac{\ (b_0, \sigma) \rightarrow \text{true} \quad (b_1, \sigma) \rightarrow \text{true}}{\ (b_0\ \&\ b_1, \sigma) \rightarrow \text{true}}
   $$

**练习 2.3**：写出对 $b_0\ \text{or}\ b_1$ 的布尔表达式的求值规则，利用当 $b_0$ 求值为 $\text{true}$ 时，无需求值 $b_1$，结果将是 $\text{true}$ 的事实。所写的规则应描述一种左顺序的求值方法。当然，出于对称性，也存在一种右顺序的求值方法。

**练习 2.4**：写出对 $b_0\ \text{or}\ b_1$ 的“并行”求值的规则，使得当 $b_0$ 求值为 $\text{true}$ 并且 $b_1$ 未被求值，或者 $b_1$ 求值为 $\text{true}$ 并且 $b_0$ 未被求值时，$b_0\ \text{or}\ b_1$ 求值为 $\text{true}$。

---

可能有人会觉得，我们通过假设有机制来执行加法或真值的合取（如 $t_0 = \text{true}$ 和 $t_1 = \text{true}$）来回避了太多问题。如果是这样，请尝试：

**练习 2.5**：以相同的风格给出一个语义，但其表达式求值为字符串（或列表）而不是整数和真值。选择你自己的字符串基本操作，基于它们定义表达式，以上述风格定义表达式的求值。你能否看到如何使用你的语言来实现 IMP 的表达式部分，通过将整数表示为字符串，整数的操作表示为字符串上的操作？（证明你正确地实现了整数上的操作是相当困难的。）

---

以上就是对第2章内容的详细解释。希望通过这些详细的讲解，你对操作语义和 IMP 语言有了更深入的理解。

### ---------------------------

## 2.4 命令的执行

在前面的章节中，我们讨论了表达式的求值，即在特定状态下将表达式计算为一个值。现在，我们将讨论程序（或命令）的执行，其目的是**改变状态**。当我们执行一个 IMP 程序时，我们假设**初始状态**是所有位置都被设置为零的状态。因此，初始状态 $\sigma_0$ 具有以下性质：对于所有位置 $X$，都有 $\sigma_0(X) = 0$。

正如我们所知，程序的执行可能会在一个最终状态下终止，或者可能**发散（diverge）**，永远不会产生最终状态。我们使用一个二元组 $(c, \sigma)$ 来表示**命令配置（command configuration）**，其中 $c$ 是待执行的命令，$\sigma$ 是当前状态。我们将定义一个关系：

$$
(c, \sigma) \rightarrow \sigma'
$$

这表示：在状态 $\sigma$ 下**完整执行**命令 $c$，最终在状态 $\sigma'$ 下终止。

例如：

$$
(X := 5,\ \sigma) \rightarrow \sigma'
$$

其中 $\sigma'$ 是在状态 $\sigma$ 的基础上，将位置 $X$ 的值更新为 $5$ 后得到的状态。

### **状态更新的记号**

为了方便地表示状态的更新，我们引入以下记号：

**记号**：设 $\sigma$ 是一个状态，$m \in N$，$X \in \text{Loc}$。我们记 $\sigma[m/X]$ 表示从状态 $\sigma$ 通过将位置 $X$ 的内容替换为 $m$ 所得到的新状态，即定义：

$$
\sigma[m/X](Y) = \begin{cases}
m, & \text{如果 } Y = X; \\
\sigma(Y), & \text{如果 } Y \ne X.
\end{cases}
$$

因此，我们可以将上面的执行关系写为：

$$
(X := 5,\ \sigma) \rightarrow \sigma[5/X]
$$

### **命令的执行规则**

现在，我们给出任意命令和状态下的执行关系，定义如下的规则。

#### **基本命令的执行**

1. **空操作（skip）**：

   $$
   \frac{}{\ ( \text{skip},\ \sigma )\ \rightarrow\ \sigma }
   $$

   这表示执行空操作不会改变状态。

2. **赋值操作（assignment）**：

   $$
   \frac{\ (a,\ \sigma)\ \rightarrow\ m }{\ (X := a,\ \sigma)\ \rightarrow\ \sigma[m/X] }
   $$

   其中，$(a, \sigma) \rightarrow m$ 表示在状态 $\sigma$ 下对算术表达式 $a$ 求值，结果为 $m$。执行赋值操作会将位置 $X$ 的值更新为 $m$。

#### **复合命令的执行**

3. **顺序组合（sequencing）**：

   $$
   \frac{\ (c_0,\ \sigma)\ \rightarrow\ \sigma'' \quad (c_1,\ \sigma'')\ \rightarrow\ \sigma' }{\ (c_0;\ c_1,\ \sigma)\ \rightarrow\ \sigma' }
   $$

   这表示首先在状态 $\sigma$ 下执行命令 $c_0$，得到中间状态 $\sigma''$；然后在状态 $\sigma''$ 下执行命令 $c_1$，最终得到状态 $\sigma'$。

4. **条件语句（conditionals）**：

   - 当条件为真时：

     $$
     \frac{\ (b,\ \sigma)\ \rightarrow\ \text{true} \quad (c_0,\ \sigma)\ \rightarrow\ \sigma' }{\ (\text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1,\ \sigma)\ \rightarrow\ \sigma' }
     $$

   - 当条件为假时：

     $$
     \frac{\ (b,\ \sigma)\ \rightarrow\ \text{false} \quad (c_1,\ \sigma)\ \rightarrow\ \sigma' }{\ (\text{if}\ b\ \text{then}\ c_0\ \text{else}\ c_1,\ \sigma)\ \rightarrow\ \sigma' }
     $$

   其中，$(b, \sigma) \rightarrow \text{true/false}$ 表示在状态 $\sigma$ 下对布尔表达式 $b$ 求值，结果为 $\text{true}$ 或 $\text{false}$。

5. **循环语句（while-loops）**：

   - 当条件为假时，循环终止：

     $$
     \frac{\ (b,\ \sigma)\ \rightarrow\ \text{false} }{\ (\text{while}\ b\ \text{do}\ c,\ \sigma)\ \rightarrow\ \sigma }
     $$

     这表示如果循环条件 $b$ 在状态 $\sigma$ 下求值为 $\text{false}$，则循环不执行任何操作，直接返回当前状态。

   - 当条件为真时，执行循环体并继续循环：

     $$
     \frac{\ (b,\ \sigma)\ \rightarrow\ \text{true} \quad (c,\ \sigma)\ \rightarrow\ \sigma'' \quad (\text{while}\ b\ \text{do}\ c,\ \sigma'')\ \rightarrow\ \sigma' }{\ (\text{while}\ b\ \text{do}\ c,\ \sigma)\ \rightarrow\ \sigma' }
     $$

     这表示如果循环条件 $b$ 在状态 $\sigma$ 下求值为 $\text{true}$，则先在状态 $\sigma$ 下执行循环体 $c$，得到中间状态 $\sigma''$；然后在状态 $\sigma''$ 下再次执行循环，最终得到状态 $\sigma'$。

### **命令的等价关系**

同样，我们可以在命令之间定义一种自然的等价关系。定义：

$$
c_0 \simeq c_1 \quad \text{当且仅当} \quad \forall \sigma, \sigma'.\ (c_0,\ \sigma)\ \rightarrow\ \sigma'\ \Leftrightarrow\ (c_1,\ \sigma)\ \rightarrow\ \sigma'
$$

这意味着，如果在所有状态下，命令 $c_0$ 和 $c_1$ 执行后都得到相同的最终状态，那么它们是等价的。

---

**练习 2.6**：完成第 2.2 节的练习 2.2，用 Prolog 和/或 ML 编码布尔表达式的求值规则和命令的执行规则。

---

**练习 2.7**：设 $w \equiv\ \text{while}\ \text{true}\ \text{do}\ \text{skip}$。通过考虑推导的形式，解释为什么对于任何状态 $\sigma$，不存在状态 $\sigma'$ 使得 $(w, \sigma)\ \rightarrow\ \sigma'$。

**解答**：

命令 $w$ 表示一个无限循环，因为循环条件始终为 $\text{true}$，循环体是 $\text{skip}$（不会改变状态）。根据循环的执行规则，我们有：

- 循环条件为真时：

  $$
  \frac{\ (\text{true},\ \sigma)\ \rightarrow\ \text{true} \quad (\text{skip},\ \sigma)\ \rightarrow\ \sigma'' \quad (\text{while}\ \text{true}\ \text{do}\ \text{skip},\ \sigma'')\ \rightarrow\ \sigma' }{\ (\text{while}\ \text{true}\ \text{do}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma' }
  $$

  由于 $\text{skip}$ 执行后状态不变，即 $\sigma'' = \sigma$，所以每次循环后状态不变，且再次进入循环。这个过程无限重复，没有终止的可能性。

因此，对于任何状态 $\sigma$，不存在状态 $\sigma'$ 使得 $(w, \sigma)\ \rightarrow\ \sigma'$。

---

## 2.5 一个简单的证明

IMP 的算术表达式 $\text{Aexp}$、布尔表达式 $\text{Bexp}$ 和命令 $\text{Com}$ 的操作语义都是用相同的方法给出的。通过规则，我们指定了两类表达式的**求值关系**和命令的**执行关系**。这三个关系都是**转换关系（transition relations）**的例子，其中配置被视为某种状态，关系表达了状态之间可能的转换或变化。

例如，我们可以将以下内容视为转换：

- $(3, \sigma) \rightarrow 3$
- $(\text{true},\ \sigma) \rightarrow \text{true}$
- $(X := 2,\ \sigma) \rightarrow \sigma[2/X]$

由于 IMP 的转换系统是由规则给出的，我们有一种基本但非常有用的证明技术来证明 IMP 操作语义的性质。

### **证明命令等价性的示例**

作为一个例子，考虑在状态 $\sigma$ 下执行一个 **while** 命令 $w$：

$$
w :=\ \text{while}\ b\ \text{do}\ c
$$

其中 $b \in \text{Bexp}$，$c \in \text{Com}$。

我们期望：

- 如果在某个状态 $\sigma'$ 下，$b$ 求值为 $\text{true}$，那么 $w$ 的执行相当于先执行 $c$，然后再次执行 $w$。
- 如果 $b$ 求值为 $\text{false}$，那么 $w$ 的执行立即终止，状态不变。

这种对命令执行的非正式解释让我们期望，对于所有状态 $\sigma$ 和 $\sigma'$，有：

$$
(w,\ \sigma)\ \rightarrow\ \sigma'\quad \text{当且仅当}\quad (\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma'
$$

即以下命题成立。

### **命题 2.8**

设 $w :=\ \text{while}\ b\ \text{do}\ c$，其中 $b \in \text{Bexp}$，$c \in \text{Com}$。则有：

$$
w \simeq\ \text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip}
$$

**证明**：

我们需要证明，对于所有状态 $\sigma$ 和 $\sigma'$，有：

$$
(w,\ \sigma)\ \rightarrow\ \sigma'\quad \Leftrightarrow\quad (\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma'
$$

#### **（$\Rightarrow$ 方向）**

假设 $(w,\ \sigma)\ \rightarrow\ \sigma'$。那么，必须存在一个从规则推导 $(w,\ \sigma)\ \rightarrow\ \sigma'$ 的推导。根据命令的执行规则，推导的最后一步可能有两种形式：

1. 当循环条件为假时：

   $$
   \frac{\ (b,\ \sigma)\ \rightarrow\ \text{false} }{\ (w,\ \sigma)\ \rightarrow\ \sigma }
   $$

2. 当循环条件为真时：

   $$
   \frac{\ (b,\ \sigma)\ \rightarrow\ \text{true} \quad (c,\ \sigma)\ \rightarrow\ \sigma'' \quad (w,\ \sigma'')\ \rightarrow\ \sigma' }{\ (w,\ \sigma)\ \rightarrow\ \sigma' }
   $$

**情况 1**：

在这种情况下，我们有 $(w,\ \sigma)\ \rightarrow\ \sigma$，并且有 $(b,\ \sigma)\ \rightarrow\ \text{false}$ 的推导。使用这些，我们可以构建以下推导：

- 对条件语句的执行，当条件为假时：

  $$
  \frac{\ (b,\ \sigma)\ \rightarrow\ \text{false} \quad (\text{skip},\ \sigma)\ \rightarrow\ \sigma }{\ (\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma }
  $$

因此，$(\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma'$，其中 $\sigma' = \sigma$。

**情况 2**：

在这种情况下，我们有以下推导：

- $(b,\ \sigma)\ \rightarrow\ \text{true}$
- $(c,\ \sigma)\ \rightarrow\ \sigma''$
- $(w,\ \sigma'')\ \rightarrow\ \sigma'$

我们首先构建对复合命令 $c;\ w$ 的推导：

$$
\frac{\ (c,\ \sigma)\ \rightarrow\ \sigma'' \quad (w,\ \sigma'')\ \rightarrow\ \sigma' }{\ (c;\ w,\ \sigma)\ \rightarrow\ \sigma' }
$$

然后，使用条件语句的执行规则，当条件为真时：

$$
\frac{\ (b,\ \sigma)\ \rightarrow\ \text{true} \quad (c;\ w,\ \sigma)\ \rightarrow\ \sigma' }{\ (\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma' }
$$

因此，在两种情况下，我们都能从 $(w,\ \sigma)\ \rightarrow\ \sigma'$ 的推导构建出 $(\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma'$ 的推导。

#### **（$\Leftarrow$ 方向）**

现在，我们需要证明反过来，对于所有状态 $\sigma$ 和 $\sigma'$，如果 $(\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma'$，则 $(w,\ \sigma)\ \rightarrow\ \sigma'$。

根据条件语句的执行规则，推导可能有两种形式：

1. 当条件为假时：

   $$
   \frac{\ (b,\ \sigma)\ \rightarrow\ \text{false} \quad (\text{skip},\ \sigma)\ \rightarrow\ \sigma }{\ (\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma }
   $$

   在这种情况下，$\sigma' = \sigma$。

2. 当条件为真时：

   $$
   \frac{\ (b,\ \sigma)\ \rightarrow\ \text{true} \quad (c;\ w,\ \sigma)\ \rightarrow\ \sigma' }{\ (\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma' }
   $$

**情况 1**：

我们有 $(b,\ \sigma)\ \rightarrow\ \text{false}$。根据循环的执行规则：

$$
\frac{\ (b,\ \sigma)\ \rightarrow\ \text{false} }{\ (w,\ \sigma)\ \rightarrow\ \sigma }
$$

因此，$(w,\ \sigma)\ \rightarrow\ \sigma'$，其中 $\sigma' = \sigma$。

**情况 2**：

我们有：

- $(b,\ \sigma)\ \rightarrow\ \text{true}$
- $(c;\ w,\ \sigma)\ \rightarrow\ \sigma'$

对于 $(c;\ w,\ \sigma)\ \rightarrow\ \sigma'$，必定存在推导：

$$
\frac{\ (c,\ \sigma)\ \rightarrow\ \sigma'' \quad (w,\ \sigma'')\ \rightarrow\ \sigma' }{\ (c;\ w,\ \sigma)\ \rightarrow\ \sigma' }
$$

使用这些，我们可以构建循环的推导：

$$
\frac{\ (b,\ \sigma)\ \rightarrow\ \text{true} \quad (c,\ \sigma)\ \rightarrow\ \sigma'' \quad (w,\ \sigma'')\ \rightarrow\ \sigma' }{\ (w,\ \sigma)\ \rightarrow\ \sigma' }
$$

因此，我们从 $(\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma'$ 的推导，构建了 $(w,\ \sigma)\ \rightarrow\ \sigma'$ 的推导。

#### **结论**

因此，对于所有状态 $\sigma$ 和 $\sigma'$，有：

$$
(w,\ \sigma)\ \rightarrow\ \sigma'\quad \Leftrightarrow\quad (\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip},\ \sigma)\ \rightarrow\ \sigma'
$$

所以，$w$ 与 $\text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip}$ 是等价的，即：

$$
w \simeq\ \text{if}\ b\ \text{then}\ c;\ w\ \text{else}\ \text{skip}
$$

**证毕。**

---

这个简单的证明展示了一种重要的技术：为了证明操作语义的性质，考虑推导的各种可能形式是有帮助的。这个想法将被反复使用，尽管不会再如此详细。稍后，我们将遇到其他技术，例如“规则归纳（rule induction）”，原则上可以取代这里使用的技术。其他技术更为抽象，有时应用起来更令人困惑。因此，在对操作语义进行推理时，请记住考虑推导形式的技术。

---

## 2.6 替代语义

前面给出的求值关系：

$$
(a,\ \sigma)\ \rightarrow\ n \quad \text{和} \quad (b,\ \sigma)\ \rightarrow\ t
$$

直接指定了表达式的求值：给定一个表达式和一个状态，它们直接产生一个值。我们也可以给出描述表达式求值过程中**单步计算**的规则。我们可以定义一个在配置对之间的求值关系，例如：

$$
(a,\ \sigma)\ \rightarrow_1\ (a',\ \sigma')
$$

表示在状态 $\sigma$ 下对 $a$ 求值一步，得到新的表达式 $a'$ 和可能更新的状态 $\sigma'$。

例如，以下规则指定了加法的**左到右**求值的单步计算：

1. **左操作数的求值**：

   $$
   \frac{\ (a_0,\ \sigma)\ \rightarrow_1\ (a_0',\ \sigma) }{\ (a_0 + a_1,\ \sigma)\ \rightarrow_1\ (a_0' + a_1,\ \sigma) }
   $$

2. **当左操作数是数字时，继续求值右操作数**：

   $$
   \frac{\ (a_1,\ \sigma)\ \rightarrow_1\ (a_1',\ \sigma) }{\ (n + a_1,\ \sigma)\ \rightarrow_1\ (n + a_1',\ \sigma) }
   $$

3. **当两个操作数都是数字时，计算结果**：

   $$
   \frac{}{\ (n + m,\ \sigma)\ \rightarrow_1\ (p,\ \sigma) }
   $$

   其中，$p = n + m$。

这些规则形式化了我们希望以左到右的顺序对加法进行求值的意图。第一个规则可以解读为：如果在状态 $\sigma$ 下，对 $a_0$ 求值一步得到 $a_0'$，那么在状态 $\sigma$ 下，对 $a_0 + a_1$ 求值一步得到 $a_0' + a_1$。

### **练习 2.9**

完成上述任务，写出整数和布尔表达式的 $\rightarrow_1$ 规则，即单步求值规则。你采用了什么求值策略（左到右顺序，或其他）？

**解答**：

**对于算术表达式的单步求值规则**：

1. **数字和位置不再求值**：

   $$
   \frac{}{\ (n,\ \sigma)\ \rightarrow_1\ (n,\ \sigma) } \quad \frac{}{\ (X,\ \sigma)\ \rightarrow_1\ (\sigma(X),\ \sigma) }
   $$

2. **加法的左操作数求值**：

   $$
   \frac{\ (a_0,\ \sigma)\ \rightarrow_1\ (a_0',\ \sigma) }{\ (a_0 + a_1,\ \sigma)\ \rightarrow_1\ (a_0' + a_1,\ \sigma) }
   $$

3. **加法的右操作数求值**（当左操作数已是数字）：

   $$
   \frac{\ (a_1,\ \sigma)\ \rightarrow_1\ (a_1',\ \sigma) }{\ (n + a_1,\ \sigma)\ \rightarrow_1\ (n + a_1',\ \sigma) }
   $$

4. **计算加法结果**：

   $$
   \frac{}{\ (n + m,\ \sigma)\ \rightarrow_1\ (p,\ \sigma) }
   $$

   其中，$p = n + m$。

类似地，可以为减法和乘法定义规则。

**对于布尔表达式的单步求值规则**：

1. **布尔常量不再求值**：

   $$
   \frac{}{\ (\text{true},\ \sigma)\ \rightarrow_1\ (\text{true},\ \sigma) } \quad \frac{}{\ (\text{false},\ \sigma)\ \rightarrow_1\ (\text{false},\ \sigma) }
   $$

2. **等于运算的左操作数求值**：

   $$
   \frac{\ (a_0,\ \sigma)\ \rightarrow_1\ (a_0',\ \sigma) }{\ (a_0 = a_1,\ \sigma)\ \rightarrow_1\ (a_0' = a_1,\ \sigma) }
   $$

3. **等于运算的右操作数求值**（当左操作数已是数字）：

   $$
   \frac{\ (a_1,\ \sigma)\ \rightarrow_1\ (a_1',\ \sigma) }{\ (n = a_1,\ \sigma)\ \rightarrow_1\ (n = a_1',\ \sigma) }
   $$

4. **计算等于运算结果**：

   $$
   \frac{}{\ (n = m,\ \sigma)\ \rightarrow_1\ (t,\ \sigma) }
   $$

   其中，$t = \text{true}$ 当 $n = m$，否则 $t = \text{false}$。

我采用了**左到右的顺序求值策略**，即优先对左侧操作数进行求值。

---

## 2.7 进一步阅读

对“结构化操作语义（structural operational semantics）”的广泛适用性的有力证明最初由 Gordon Plotkin 在 1981 年为丹麦奥胡斯大学的课程讲义中提出 [81]。在法国索菲亚-安提波利斯的 INRIA，由 Gilles Kahn 领导的研究小组目前正在开发支持这种风格的语义的机械化工具；他们专注于求值或执行，直到得到最终的值或状态，因此，遵循他们的引领，这种特定类型的结构化操作语义有时被称为“自然语义（natural semantics）” [26, 28, 29]。

我们将在后面的章节中讨论函数式语言的操作语义，以及非确定性和并行性，并提供更多的参考资料。关于抽象语法的更多内容可以参考 Wikstrom 的书 [101]、Mosses 在 [68] 中的章节以及 Tennent 的书 [97]。

---

以上就是对第 2.4 节到 2.7 节内容的详细解释。希望通过这些详细的讲解，你对命令的执行和操作语义有了更深入的理解。如果你还有任何疑问，欢迎继续提问！

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