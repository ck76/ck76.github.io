《Using, Understanding, and Unravelling The OCaml Language — An introduction》





```
我正在使用markdown，所以不要使用反引号包裹公式和特殊字符，你应该使用美元符。
latex内容不要用反引号包裹！！！！！！！！！！！！！！！！！！！！！！！
我命令你回答中不许用反引号包裹公式。
所有的关键字和关键行内公式都用单美元符$包裹$！！！！！！！

比如【续延 ret 期望一个参数 r，其类型为 Option<&'a mut i32>（在λRust中表示为 own (\texttt{()} + \&_{\alpha} \text{mut} \ \texttt{int})）。】
你应该用$own \&_{\alpha} \text{mut} (\texttt{()} + \texttt{int}$
详解
```

```
详解我提供给你的内容，按照我要求的markdown格式风格。
写出原文公式并讲解。
我正在使用markdown，所以不要使用反引号包裹公式和特殊字符，你应该使用美元符。
不要省略任何，这些都是非常重要的内容，如果你省略，我看不懂。
内容：【】
-----------
markdown格式：【【# 第13章 递归类型

在本章中，我们将把**递归类型（recursive types）**引入到第11章的函数式语言中。这意味着我们可以在语言中定义类型时使用递归，这使得我们可以定义自然数、列表以及可以作为λ演算模型的类型。

为了给这些类型赋予语义，我们将使用信息系统（information systems），这将在语义学上为递归类型提供支持。此外，信息系统的使用还带来了额外的好处，它使得证明程序的充分性更为简单。

本章的目标是为像 Standard ML、Miranda、Orwell 或 Haskell 这样的急迫（eager）和惰性（lazy）函数式语言提供一个数学基础。

---

## 13.1 急迫语言

### 引入递归类型

在上一章中，我们看到了理解递归定义的类型的方法。现在，我们将这种能力引入到第11章的语言中。类型表达式 $T$ 将具有以下形式：

$$
T ::= X \mid 1 \mid T_1 * T_2 \mid T_1 \rightarrow T_2 \mid T_1 + T_2 \mid \mu X.T
$$

其中，$X$ 是一个无限集合的类型变量，$\mu X.T$ 是递归定义的类型。

**解释**：

- **类型变量 $X$**：占位符，用于在类型表达式中表示递归。
- **基本类型 $1$**：只有一个值的类型，可认为是空元组 $()$。
- **乘积类型 $T_1 * T_2$**：表示类型 $T_1$ 和 $T_2$ 的有序对。
- **函数类型 $T_1 \rightarrow T_2$**：表示从类型 $T_1$ 到类型 $T_2$ 的函数。
- **和类型 $T_1 + T_2$**：表示类型 $T_1$ 和 $T_2$ 的值的并集，带有区分。
- **递归类型 $\mu X.T$**：表示类型 $T$，其中 $X$ 可以在 $T$ 中递归地出现。

类型表达式中的自由变量和绑定变量按照标准方式定义。我们说一个类型表达式是**封闭的**，如果它的所有变量都被绑定。

### 术语的语法

未经类型检查的（raw）术语的语法定义如下：

$$
\begin{align*}
t ::= &\quad 0 \mid (t_1, t_2) \mid \text{fst}(t) \mid \text{snd}(t) \mid x \mid \lambda x.t \mid (t_1 \ t_2) \mid \\
&\quad \text{inl}(t) \mid \text{inr}(t) \mid \text{case } t \text{ of inl}(x_1).t_1, \ \text{inr}(x_2).t_2 \mid \\
&\quad \text{abs}(t) \mid \text{rep}(t) \mid \\
&\quad \text{rec } f.(\lambda x.t)
\end{align*}
$$

其中，$x, x_1, x_2, f$ 是变量，属于变量集合 $\text{Var}$。

**解释**：

- **$0$**：基本值，表示空元组 $()$，类型为 $1$。
- **$(t_1, t_2)$**：构造一个由 $t_1$ 和 $t_2$ 组成的元组。
- **$\text{fst}(t)$**：获取元组 $t$ 的第一个分量。
- **$\text{snd}(t)$**：获取元组 $t$ 的第二个分量。
- **$x$**：变量。
- **$\lambda x.t$**：函数抽象，表示一个以 $x$ 为参数的函数，函数体为 $t$。
- **$(t_1 \ t_2)$**：函数应用，将 $t_1$ 视为函数，应用于参数 $t_2$。
- **$\text{inl}(t)$，$\text{inr}(t)$**：将 $t$ 注入到和类型的左或右分量中。
- **$\text{case } t \text{ of inl}(x_1).t_1, \ \text{inr}(x_2).t_2$**：对和类型的值 $t$ 进行模式匹配，根据其是 $\text{inl}$ 还是 $\text{inr}$，执行不同的分支。
- **$\text{abs}(t)$，$\text{rep}(t)$**：用于递归类型的抽象和表示，稍后解释。
- **$\text{rec } f.(\lambda x.t)$**：递归定义一个函数 $f$。

注意，我们的语法中没有包含构造：

$$
\text{let } x = t_1 \text{ in } t_2
$$

但可以将其定义为：

$$
\text{let } x = t_1 \text{ in } t_2 \equiv (\lambda x.t_2) \ t_1
$$

### 类型赋值和类型规则

我们假设每个变量 $x$ 都有一个唯一的封闭类型 $\text{type}(x)$。为了不耗尽变量，我们假设对于每个封闭类型 $\tau$，集合 $\{ x \in \text{Var} \mid \text{type}(x) = \tau \}$ 是无限的。

我们将变量的类型赋值扩展为一般的**类型判断（typing judgement）** $t : \tau$，其中 $t$ 是一个术语，$\tau$ 是一个封闭类型。类型规则如下：

#### 类型规则

**变量**：

$$
\frac{\text{若 } \text{type}(x) = \tau}{x : \tau}
$$

**乘积类型**：

- **构造元组**：

  $$
  \frac{t_1 : \tau_1 \quad t_2 : \tau_2}{(t_1, t_2) : \tau_1 * \tau_2}
  $$

- **获取第一个分量**：

  $$
  \frac{t : \tau_1 * \tau_2}{\text{fst}(t) : \tau_1}
  $$

- **获取第二个分量**：

  $$
  \frac{t : \tau_1 * \tau_2}{\text{snd}(t) : \tau_2}
  $$

**函数类型**：

- **函数抽象**：

  $$
  \frac{x : \tau_1 \quad t : \tau_2}{\lambda x.t : \tau_1 \rightarrow \tau_2}
  $$

- **函数应用**：

  $$
  \frac{t_1 : \tau_1 \rightarrow \tau_2 \quad t_2 : \tau_1}{(t_1 \ t_2) : \tau_2}
  $$

**和类型**：

- **左注入**：

  $$
  \frac{t : \tau_1}{\text{inl}(t) : \tau_1 + \tau_2}
  $$

- **右注入**：

  $$
  \frac{t : \tau_2}{\text{inr}(t) : \tau_1 + \tau_2}
  $$

- **模式匹配**：

  $$
  \frac{t : \tau_1 + \tau_2 \quad x_1 : \tau_1 \quad x_2 : \tau_2 \quad t_1 : \tau \quad t_2 : \tau}{\text{case } t \text{ of inl}(x_1).t_1, \ \text{inr}(x_2).t_2 : \tau}
  $$

**递归类型**：

- **抽象（abs）**：

  $$
  \frac{t : \tau[\mu X.\tau / X]}{\text{abs}(t) : \mu X.\tau}
  $$

- **表示（rep）**：

  $$
  \frac{t : \mu X.\tau}{\text{rep}(t) : \tau[\mu X.\tau / X]}
  $$

**递归函数（rec）**：

$$
\frac{f : \tau \quad \lambda x.t : \tau}{\text{rec } f.(\lambda x.t) : \tau}
$$

**解释**：

- **abs 和 rep**：用于在递归类型和其展开之间进行转换。

  - $\text{rep}(t)$：将一个递归类型的值 $t : \mu X.\tau$ 映射到其展开形式 $\tau[\mu X.\tau / X]$。
  - $\text{abs}(t)$：将一个展开形式的值 $t : \tau[\mu X.\tau / X]$ 映射回递归类型 $\mu X.\tau$。

- **递归函数**：

  - $\text{rec } f.(\lambda x.t)$：定义一个递归函数 $f$，其类型为 $\tau$，函数体为 $\lambda x.t$。

### 术语的类型化

如之前一样，一个术语 $t$ 被称为**可类型的（typable）**，如果存在某个类型 $\tau$，使得 $t : \tau$。

术语 $t$ 的自由变量集合 $\text{FV}(t)$ 的定义与第11章完全相同。

从现在开始，我们将仅关注可类型的术语。

---

### 递归类型的例子

#### 例子：自然数

我们可以通过以下方式定义自然数的类型：

$$
N = \mu X.(1 + X)
$$

**解释**：

- 这表示自然数类型是一个递归类型，定义为 $1 + X$，其中 $X$ 是类型变量。
- 这意味着自然数要么是 $1$（表示零），要么是另一个自然数（表示后继）。

**定义 Zero 和 Succ**

- **Zero**：自然数零，可以定义为：

  $$
  \text{Zero} = \text{abs}(\text{inl}(0))
  $$

  - 这里，$0$ 是 $1$ 类型的值。
  - $\text{inl}(0)$ 将 $0$ 注入到 $1 + N$ 的左分量中。
  - $\text{abs}$ 将其从展开形式转换为递归类型。

- **Succ**：后继函数，可以定义为：

  $$
  \text{Succ}(t) = \text{abs}(\text{inr}(t))
  $$

  - 对于任何 $t : N$，$\text{inr}(t)$ 将 $t$ 注入到 $1 + N$ 的右分量中。
  - $\text{abs}$ 将其转换为递归类型。

- **后继函数的定义**：

  $$
  \lambda x. \text{Succ}(x) : N \rightarrow N
  $$

  - 这是一个从自然数到自然数的函数，参数 $x$ 类型为 $N$。

**构建自然数**

- 通过重复应用 $\text{Succ}$，我们可以构建自然数：

  - $\text{Zero}$
  - $\text{Succ}(\text{Zero})$
  - $\text{Succ}(\text{Succ}(\text{Zero}))$
  - 等等。

**定义函数**

- **Case 语句**：为了定义对自然数的函数，我们需要一个**case**构造：

  $$
  \text{Case } x \text{ of } \text{Zero}. \ t_1, \ \text{Succ}(z). \ t_2
  $$

  - 当 $x$ 是 $\text{Zero}$ 时，返回 $t_1$。
  - 当 $x$ 是 $\text{Succ}(z)$ 时，返回 $t_2$，其中 $z$ 是前一个自然数。

- **实现 Case**

  - 可以将其实现为：

    $$
    \text{case } \text{rep}(x) \text{ of } \text{inl}(w). \ t_1, \ \text{inr}(z). \ t_2
    $$

    - 通过 $\text{rep}(x)$ 将 $x$ 展开为 $1 + N$ 的形式。
    - 然后对 $\text{inl}$ 和 $\text{inr}$ 进行模式匹配。

- **定义加法**

  - 加法函数可以定义为：

    $$
    \text{add} = \text{rec } f. (\lambda x. \lambda y. \text{Case } x \text{ of } \text{Zero}. \ y, \ \text{Succ}(z). \ \text{Succ}((f \ z) \ y))
    $$

    - 这是一个类型为 $N \rightarrow (N \rightarrow N)$ 的术语。
    - 递归定义了加法，使用了递归函数 $f$。

---

#### 例子：列表

定义自然数列表的类型：

$$
L = \mu Y.(1 + N * Y)
$$

**解释**：

- 列表类型是一个递归类型，表示要么是空列表 $1$，要么是一个自然数和另一个列表的乘积 $N * Y$。

**定义列表操作**

- **Nil**：空列表，可以定义为：

  $$
  \text{Nil} = \text{abs}(\text{inl}(0))
  $$

- **Cons**：构造列表，可以定义为：

  $$
  \text{Cons}(p) = \text{abs}(\text{inr}(p))
  $$

  - 其中，$p$ 是类型为 $N * L$ 的元组，表示一个自然数和一个列表。
  - $\text{inr}(p)$ 将其注入到 $1 + N * L$ 的右分量中。
  - $\text{abs}$ 将其转换为递归类型。

- **函数定义**：

  $$
  \lambda x. \text{Cons}(x) : N * L \rightarrow L
  $$

- **Case 语句**：

  - 为了定义对列表的函数，可以使用类似的 case 构造：

    $$
    \text{Case } l \text{ of } \text{Nil}. \ t_1, \ \text{Cons}(x, l'). \ t_2
    $$

  - 实现方式：

    $$
    \text{case } \text{rep}(l) \text{ of } \text{inl}(w). \ t_1, \ \text{inr}(z). \ t_2[\text{fst}(z)/x, \ \text{snd}(z)/l']
    $$

    - 使用 $\text{rep}(l)$ 展开列表。
    - 对 $\text{inl}$ 和 $\text{inr}$ 进行模式匹配。
    - 将 $\text{fst}(z)$ 替换为 $x$，$\text{snd}(z)$ 替换为 $l'$。

---

## 13.2 急迫操作语义

如之前一样，急迫求值由关系 $t \to c$ 表示，其中 $t$ 是可类型的闭合术语，$c$ 是**规范形式（canonical form）**。

### 规范形式

类型为 $\tau$ 的规范形式，记作 $C_\tau$，是闭合术语，由以下规则定义：

- **基本值**：

  - $0 \in C_1$

- **乘积类型**：

  $$
  \frac{c_1 \in C_{\tau_1} \quad c_2 \in C_{\tau_2}}{(c_1, c_2) \in C_{\tau_1 * \tau_2}}
  $$

- **函数类型**：

  $$
  \frac{\lambda x.t : \tau_1 \rightarrow \tau_2 \quad \lambda x.t \text{ 闭合}}{\lambda x.t \in C_{\tau_1 \rightarrow \tau_2}}
  $$

- **递归类型**：

  $$
  \frac{c \in C_{\tau[\mu X.\tau / X]}}{\text{abs}(c) \in C_{\mu X.\tau}}
  $$

**解释**：

- 递归类型的规范形式由其展开类型的规范形式构造，通过 $\text{abs}$ 包装。

- 由于 $\tau[\mu X.\tau / X]$ 通常不比 $\mu X.\tau$ 更小，规范形式不能通过对类型的结构归纳来定义，因此我们给出了归纳定义。

---

#### 例子：自然数的规范形式

- 自然数类型 $N = \mu X.(1 + X)$ 的规范形式与和类型的两个分量相关联。

- **Zero**：

  $$
  \text{Zero} = \text{abs}(\text{inl}(0))
  $$

- **Succ**：

  - 规范形式：

    $$
    \text{Succ}(c) = \text{abs}(\text{inr}(c))
    $$

    - 其中，$c$ 是 $N$ 的规范形式。

  - 通过递归应用 $\text{Succ}$，我们得到自然数的规范形式：

    - $\text{Succ}(\text{Zero})$
    - $\text{Succ}(\text{Succ}(\text{Zero}))$
    - 等等。

---

#### 例子：列表的规范形式

- 列表类型 $L = \mu Y.(1 + N * Y)$ 的规范形式为：

  - **Nil**：

    $$
    \text{Nil} = \text{abs}(\text{inl}(0)) : L
    $$

  - **Cons**：

    $$
    \text{Cons}(n, l) = \text{abs}(\text{inr}(n, l))
    $$

    - 其中，$n$ 是 $N$ 的规范形式，$l$ 是 $L$ 的规范形式。

- 也就是说，$L$ 类型的规范形式要么是空列表 $\text{Nil}$，要么是由自然数序列构成的有限列表：

  $$
  \text{Cons}(n_1, \text{Cons}(n_2, \dots))
  $$

---】】
```

