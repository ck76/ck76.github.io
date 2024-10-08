[toc]



# 第13章 递归类型

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

---

### 急迫求值关系

急迫求值关系 $t \to c$ 定义在可类型的闭合术语 $t$ 和规范形式 $c$ 之间。求值规则如下：

#### 求值规则

- **规范形式**：

  $$
  \frac{\text{如果 } C \text{ 是规范形式}}{C \to C}
  $$

- **元组构造**：

  $$
  \frac{t_1 \to c_1 \quad t_2 \to c_2}{(t_1, t_2) \to (c_1, c_2)}
  $$

- **获取第一个分量**：

  $$
  \frac{t \to (c_1, c_2)}{\text{fst}(t) \to c_1}
  $$

- **获取第二个分量**：

  $$
  \frac{t \to (c_1, c_2)}{\text{snd}(t) \to c_2}
  $$

- **函数应用**：

  $$
  \frac{t_1 \to \lambda x.t_1' \quad t_2 \to c_2}{(t_1 \ t_2) \to t_1'[c_2 / x]}
  $$

- **模式匹配**：

  $$
  \frac{t \to \text{inl}(c)}{\text{case } t \text{ of } \text{inl}(x_1).t_1, \ \text{inr}(x_2).t_2 \to t_1[c / x_1]}
  $$

  $$
  \frac{t \to \text{inr}(c)}{\text{case } t \text{ of } \text{inl}(x_1).t_1, \ \text{inr}(x_2).t_2 \to t_2[c / x_2]}
  $$

- **抽象**：

  $$
  \frac{t \to c}{\text{abs}(t) \to \text{abs}(c)}
  $$

- **表示**：

  $$
  \frac{t \to \text{abs}(c)}{\text{rep}(t) \to c}
  $$

- **递归函数**：

  $$
  \text{rec } f.(\lambda x.t) \to \lambda x.t[\text{rec } f.(\lambda x.t) / f]
  $$

### 性质

**命题13.1**

对于可类型的闭合术语 $t$ 和规范形式 $c$、$c'$，有：

1. **类型保持**：如果 $t \to c$ 且 $t : \tau$，则 $c : \tau$。

2. **确定性**：如果 $t \to c_1$ 且 $t \to c_2$，则 $c_1 = c_2$。

**证明**：

- **类型保持**：通过对求值规则的归纳，可以证明每个规则都保持类型正确性。

- **确定性**：通过对求值规则的归纳，可以证明对于给定的术语 $t$，如果可以求值到 $c_1$ 和 $c_2$，则 $c_1$ 和 $c_2$ 必须相等。

---

## 13.3 急迫指称语义

在指称语义中，一个可类型的闭合术语可以求值到一个规范形式，或者发散（diverge）。因此，我们将其指称定义为类型 $\tau$ 的值集合 $V_\tau^\bot$ 中的一个元素，其中包含规范形式的值。

由于语言允许递归地定义类型，我们使用上一章的信息系统的机制，为每个类型定义一个值的信息系统。

### 类型环境

**定义**：一个类型环境 $\Xi$ 是一个从类型变量到信息系统的函数。

**通过结构归纳**，对于类型表达式，在类型环境 $\Xi$ 中定义其对应的信息系统 $V_{[T]}^\Xi$：

1. **基本类型**：

   $$
   V_{[1]}^\Xi = (\emptyset, \{ \emptyset \}, \emptyset)
   $$

   - 也称为 $1$，只有一个元素。

2. **乘积类型**：

   $$
   V_{[T_1 * T_2]}^\Xi = V_{[T_1]}^\Xi \times V_{[T_2]}^\Xi
   $$

   - 使用信息系统的积。

3. **函数类型**：

   $$
   V_{[T_1 \rightarrow T_2]}^\Xi = (V_{[T_1]}^\Xi \rightarrow V_{[T_2]}^\Xi)^\bot
   $$

   - 使用提升的函数空间。

4. **和类型**：

   $$
   V_{[T_1 + T_2]}^\Xi = V_{[T_1]}^\Xi + V_{[T_2]}^\Xi
   $$

   - 使用信息系统的和。

5. **类型变量**：

   $$
   V_{[X]}^\Xi = \Xi(X)
   $$

6. **递归类型**：

   $$
   V_{[\mu X.T]}^\Xi = \mu V_{[T]}^{\Xi [\Xi / X]}
   $$

   - 递归类型的语义是信息系统的最小不动点。

**解释**：

- 所有右侧的操作都是信息系统上的操作。

- 对于类型表达式 $\mu X.T$，在环境 $\Xi$ 中，它的指称是信息系统的最小不动点。

- 我们可以通过在信息系统的完全偏序集上构造不动点来定义递归类型。

### 闭合类型的值

一个封闭类型 $T$ 与一个信息系统 $V_{[T]}^\Xi$ 相关联，其元素形成一个值的 cpo（完全偏序集） $| V_{[T]}^\Xi |$。

由于类型是封闭的，环境 $\Xi$ 对结果指称没有影响，可以是任意的。

对于一个术语 $t$，在其自由变量的环境下，其指称是 $(V_{[T]}^\bot)$ 中的一个元素。

### 提升的解释

为了简化，我们选择以下方式解释底元 $\bot$ 和提升函数 $l - \rangle : V_\tau \rightarrow V_\tau^\bot$：

- **底元**：$\bot = \emptyset$，空集。

- **提升函数**：$l x \rangle = x$，对于所有 $x \in V_\tau$。

这满足信息系统元素始终非空的条件。

### 环境

环境的 cpo 定义为：

$$
\text{Env} = \{ \rho : \text{Var} \rightarrow \bigcup \{ V_\tau \mid \tau \text{ 是封闭类型} \} \mid \rho(x) \in V_{\text{type}(x)} \}
$$

按照点序关系排序。

---

### 指称语义的表示

为了呈现指称语义，我们做一些约定和简化。

#### 约定

- **和类型的同一性**：

  - 对于信息系统 $A$ 和 $B$，我们认为：

    $$
    | A | + | B | = | A + B |
    $$

  - 注入函数定义为：

    - $\text{inl} : | A | \rightarrow | A + B |$，$\text{inl}(x) = \text{inj}_1(x) = \{ (1, a) \mid a \in x \}$。

    - $\text{inr} : | B | \rightarrow | A + B |$，$\text{inr}(x) = \text{inj}_2(x) = \{ (2, b) \mid b \in x \}$。

- **积类型的同一性**：

  - 对于信息系统 $A$ 和 $B$，我们认为：

    $$
    | A | \times | B | = | A \times B |
    $$

  - 元素的配对表示为集合的乘积：

    $$
    (x, y) = x \times y
    $$

    - 其中 $x \in | A |$，$y \in | B |$。

  - 投影函数定义为：

    - $\pi_1 : | A \times B | \rightarrow | A |$，$\pi_1(z) = \text{Proj}_1(z) = \{ a \mid \exists b. (a, b) \in z \}$。

    - $\pi_2 : | A \times B | \rightarrow | B |$，$\pi_2(z) = \text{Proj}_2(z) = \{ b \mid \exists a. (a, b) \in z \}$。

#### 解释

- 这些约定使得在指称语义的定义中，我们不需要显式地提及同构，这样表达更为简洁。

- 对于函数类型，我们不将连续函数与其表示的可逼近映射（approximable mapping）视为相同，而是使用之前的同构。

---

### 指称语义

现在，我们给出术语的指称语义定义。

#### 基本值

$$
[0] = \lambda \rho. \{ \emptyset \}
$$

#### 构造元组

$$
[(t_1, t_2)] = \lambda \rho. [t_1]_\rho \times [t_2]_\rho
$$

- **解释**：将 $t_1$ 和 $t_2$ 的值组合成一个元组。

#### 获取第一个分量

$$
[\text{fst}(t)] = \lambda \rho. \pi_1([t]_\rho)
$$

- **解释**：对 $t$ 求值，取其第一分量。

#### 获取第二个分量

$$
[\text{snd}(t)] = \lambda \rho. \pi_2([t]_\rho)
$$

- **解释**：对 $t$ 求值，取其第二分量。

#### 变量

$$
[x] = \lambda \rho. \rho(x)
$$

#### 函数抽象

$$
[\lambda x.t] = \lambda \rho. \ '([\lambda v \in V_{\text{type}(x)}. [t]_{\rho [v / x]}])
$$

- **解释**：

  - 定义一个函数，从 $V_{\text{type}(x)}$ 中的 $v$ 映射到 $[t]_{\rho [v / x]}$。
  - 使用同构 $'$ 将其转换为 $A \rightarrow B^\bot$ 的元素。

#### 函数应用

$$
[t_1 \ t_2] = \lambda \rho. I[t_1]_\rho I([t_2]_\rho)
$$

- **解释**：

  - 对 $t_1$ 和 $t_2$ 求值，得到函数的表示和参数的值。
  - 使用函数表示的应用 $I[t_1]_\rho I([t_2]_\rho)$。

#### 左注入

$$
[\text{inl}(t)] = \lambda \rho. \text{inj}_1([t]_\rho)
$$

#### 右注入

$$
[\text{inr}(t)] = \lambda \rho. \text{inj}_2([t]_\rho)
$$

#### 模式匹配

$$
[\text{case } t \text{ of } \text{inl}(x_1).t_1, \ \text{inr}(x_2).t_2] = \lambda \rho. \text{case } [t]_\rho \text{ of } \text{inj}_1(v_1). [t_1]_{\rho [v_1 / x_1]}, \ \text{inj}_2(v_2). [t_2]_{\rho [v_2 / x_2]}
$$

#### 抽象（abs）

$$
[\text{abs}(t)] = [t]
$$

- **解释**：由于我们在信息系统层面将 $\mu X.\tau$ 和其展开视为同一，因此 $\text{abs}$ 和 $\text{rep}$ 可以视为恒等函数。

#### 表示（rep）

$$
[\text{rep}(t)] = [t]
$$

#### 递归函数

$$
[\text{rec } f.(\lambda x.t)] = \lambda \rho. \mu r. [\lambda x.t]_{\rho [r / f]}
$$

- **解释**：

  - 定义一个递归函数，其不动点为 $r$。
  - 在环境中替换 $f$ 为 $r$。

---

### 解释和注释

1. **元组操作**：

   - 元组的构造和投影使用集合的乘积和投影操作。

2. **函数抽象和应用**：

   - 使用同构 $I - I$ 和 $' - '$，将函数抽象表示为可逼近映射，然后再应用。

3. **注入和模式匹配**：

   - 注入使用 $\text{inj}_1$ 和 $\text{inj}_2$。
   - 模式匹配直接在注入的值上进行。

4. **抽象和表示**：

   - 由于我们的约定，$\text{abs}$ 和 $\text{rep}$ 在语义上可以视为恒等函数。

5. **递归函数**：

   - 使用不动点 $\mu r$ 来定义递归函数的语义。

---

### 性质

**引理13.2**

- **环境的影响**：如果 $\rho$ 和 $\rho'$ 在术语 $t$ 的自由变量上取相同的值，则 $[t]_\rho = [t]_{\rho'}$。

- **证明**：通过对 $t$ 的结构归纳。

**引理13.3**

- **规范形式的值**：如果 $c$ 是规范形式，则对于任何环境 $\rho$，$[c]_\rho \ne \emptyset$。

- **证明**：通过对 $c$ 的结构归纳。

---

# 总结

在本章中，我们将递归类型引入到急迫的函数式语言中，并定义了其语法、操作语义和指称语义。我们使用信息系统为递归类型赋予语义，并使用不动点来定义递归类型的指称。

通过定义自然数和列表等递归类型的例子，我们展示了如何在语言中构造和使用递归类型。操作语义中，我们定义了规范形式和急迫求值规则。指称语义中，我们详细定义了各个术语的语义，并讨论了约定和性质。

这些内容为理解和研究具有递归类型的函数式语言提供了坚实的理论基础。

### ---------------------------

# 13.4 急迫语义的充分性

在本节中，我们将证明**操作语义（operational semantics）**和**指称语义（denotational semantics）**在术语的求值是否收敛这一点上是一致的。也就是说，对于一个可类型的、闭合的术语 $t$，操作语义和指称语义都会同意 $t$ 是否会求值到一个规范形式，或者是否会发散。

首先，我们引入一些符号：

- **$t \Downarrow$**：表示存在某个规范形式 $c$，使得 $t$ 能够求值到 $c$，即 $t \to c$。
- **$t \uparrow$**：表示 $t$ 的指称不是底元，即 $[t]_\rho \ne \bot$，其中 $\rho$ 是任意环境。

**定义**：

- 对于一个可类型的、闭合的术语 $t$，定义：
  
  $$
  t \Downarrow \quad \text{当且仅当存在 } c, \ t \to c
  $$
  
  $$
  t \uparrow \quad \text{当且仅当 } [t]_\rho \ne \bot
  $$
  
  - 这里，$t \Downarrow$ 表示 $t$ 的求值终止于一个规范形式。
  - $t \uparrow$ 表示 $t$ 的指称不是底元（即非空集合）。

我们的目标是证明，对于可类型的、闭合的术语 $t$：

- 如果 $t \Downarrow$，则 $t \uparrow$。
- 如果 $t \uparrow$，则 $t \Downarrow$。

也就是说，操作语义的收敛性与指称语义的非底元性一致。

为了证明这一点，我们需要一些辅助引理。

---

## 替换引理（Substitution Lemma）

**引理13.4（替换引理）**：

设 $s$ 是一个可类型的、闭合的术语，且 $[s]_\rho \ne \bot$。则对于任意术语 $t$，有：

$$
[t[s/x]]_\rho = [t]_{\rho[[s]_\rho / x]}
$$

**解释**：

- $t[s/x]$ 表示在术语 $t$ 中，用 $s$ 替换所有自由出现的 $x$。
- $\rho[[s]_\rho / x]$ 表示将环境 $\rho$ 中 $x$ 的值替换为 $[s]_\rho$。
- 该引理表明，术语替换在指称语义下是保持一致的。

**证明**：

- 对术语 $t$ 进行结构归纳（structural induction）。
- **基本情况**：
  - 当 $t$ 是变量时，分情况讨论 $t$ 是否为 $x$。
  - 当 $t$ 是常量或基本值时，替换没有影响。
- **归纳步骤**：
  - 对于复合术语，如 $\lambda y.t'$、$t_1 \ t_2$、$(t_1, t_2)$ 等，利用归纳假设证明替换后的指称与替换前的一致性。

---

## 求值引理（Evaluation Lemma）

**引理13.5**：

如果 $t \to c$，则对于任意可类型的、闭合的术语 $t$ 和规范形式 $c$，以及任意环境 $\rho$，有：

$$
[t]_\rho = [c]_\rho
$$

**解释**：

- 这意味着，如果 $t$ 能够求值到规范形式 $c$，那么它们的指称语义是相同的。
- 这对于证明充分性很重要，因为它连接了操作语义和指称语义。

**证明**：

- 对求值规则进行归纳（rule induction）。
- **基本情况**：
  - 当 $t$ 本身就是规范形式 $c$ 时，显然 $[t]_\rho = [c]_\rho$。
- **归纳步骤**：
  - 对于每个求值规则，证明如果前提成立，结论的指称语义也一致。
  - 需要特别注意处理涉及替换的规则，如函数应用和递归函数。

---

### 练习13.6

**问题**：

为和类型和递归类型的求值规则证明上述引理13.5的归纳步骤。

**解答**：

- **和类型（Sum Types）**：
  
  - **规则**：
    
    $$
    \frac{t \to \text{inl}(c)}{\text{case } t \text{ of } \text{inl}(x_1).t_1, \ \text{inr}(x_2).t_2 \to t_1[c / x_1]}
    $$
    
    $$
    \frac{t \to \text{inr}(c)}{\text{case } t \text{ of } \text{inl}(x_1).t_1, \ \text{inr}(x_2).t_2 \to t_2[c / x_2]}
    $$
  
  - **证明**：
    
    - 假设 $t \to \text{inl}(c)$，根据归纳假设，$[t]_\rho = [\text{inl}(c)]_\rho$。
    - 根据指称语义，$[\text{case } t \text{ of } \dots ]_\rho = [t_1]_{\rho[c / x_1]}$。
    - 由于 $[c]_\rho$ 已知，应用替换引理，可得 $[t_1[c / x_1]]_\rho = [t_1]_{\rho[c / x_1]}$。
    - 因此，$[\text{case } t \text{ of } \dots ]_\rho = [t_1[c / x_1]]_\rho$，即证明成立。
  
- **递归类型（Recursive Types）**：
  
  - **规则**：
    
    $$
    \frac{t \to c}{\text{abs}(t) \to \text{abs}(c)}
    $$
    
    $$
    \frac{t \to \text{abs}(c)}{\text{rep}(t) \to c}
    $$
  
  - **证明**：
    
    - 对于 $\text{abs}(t)$，假设 $t \to c$，根据归纳假设，$[t]_\rho = [c]_\rho$。
    - 根据指称语义，$[\text{abs}(t)]_\rho = [t]_\rho$。
    - 因此，$[\text{abs}(t)]_\rho = [c]_\rho = [\text{abs}(c)]_\rho$，因为 $\text{abs}$ 在语义上是恒等的。
    - 对于 $\text{rep}(t)$，类似地证明。

---

由于引理13.3，规范形式的指称不是底元（非空）。因此，可以得出：

- 如果 $t \Downarrow$，则 $t \uparrow$。

---

如同往常，反过来的证明更为困难，需要使用逻辑关系（logical relation）的方法，类似于第11章所采用的方法。然而，这次我们有递归类型的额外复杂性。在第11章中，我们可以根据类型 $T$ 的结构对逻辑关系 $\leq_T$ 进行归纳定义。当类型可以递归定义时，我们不能再这样做；因为对于 $\leq_{\mu X.T}$ 的定义，如果直接基于 $\leq_{T[\mu X.T / X]}$，这样的定义将不是良基的（well-founded）。

幸运的是，我们仍然可以利用信息系统的表示，给出关系 $\leq_T$ 的简单定义。对于一个令牌（token） $a \in \text{Tok}_T$，类型 $T$，以及规范形式 $c \in C_T$，我们可以通过良基递归（well-founded recursion）来定义适当的关系。

---

## 定义逻辑关系

为了建立逻辑关系，我们需要一个大小函数（size function）来帮助我们进行良基递归。

**定义：令牌和规范形式的大小（size）**

- 对于通过以下方式构建的集合（sets built up inductively）：
  - 从空集开始，通过形成有限子集（finite subsets）、与 $1$ 和 $2$ 的配对、以及成对（pairing）构建。
- 定义大小函数 $\text{size}$：
  
  - **空集**：
    
    $$
    \text{size}(\emptyset) = 1
    $$
  
  - **有限非空子集 $X$**：
    
    $$
    \text{size}(X) = 1 + \sum_{a \in X} \text{size}(a)
    $$
  
  - **成对 $(a, b)$**：
    
    $$
    \text{size}((a, b)) = 1 + \text{size}(a) + \text{size}(b)
    $$
  
  - **与 $1$ 的配对 $(1, a)$**：
    
    $$
    \text{size}((1, a)) = 1 + \text{size}(a)
    $$
  
  - **与 $2$ 的配对 $(2, b)$**：
    
    $$
    \text{size}((2, b)) = 1 + \text{size}(b)
    $$

**引理13.7**

对于每个封闭类型 $T$，存在一个关系 $\vdash_T$，它定义了 $V_T$ 的令牌和规范形式之间的关系，满足以下性质：

- **基本类型**：
  
  $$
  \emptyset \vdash_1 0
  $$
  
- **乘积类型**：
  
  $$
  (a, b) \vdash_{T_1 * T_2} (c_1, c_2) \quad \text{当且仅当} \quad a \vdash_{T_1} c_1 \quad \text{且} \quad b \vdash_{T_2} c_2
  $$
  
- **函数类型**：
  
  $$
  (U, V) \vdash_{T_1 \rightarrow T_2} \lambda x.t \quad \text{当且仅当} \quad \forall c \in C_{T_1}, \ U \leq_{T_1} c \implies V \leq_{T_2} t[c / x]
  $$
  
  - 其中，$U$ 和 $V$ 是 $T_1$ 和 $T_2$ 的令牌集合。
  
- **和类型**：
  
  $$
  (1, a) \vdash_{T_1 + T_2} \text{inl}(c) \quad \text{当且仅当} \quad a \vdash_{T_1} c
  $$
  
  $$
  (2, b) \vdash_{T_1 + T_2} \text{inr}(c) \quad \text{当且仅当} \quad b \vdash_{T_2} c
  $$
  
- **递归类型**：
  
  $$
  a \vdash_{\mu X.T} \text{abs}(c) \quad \text{当且仅当} \quad a \vdash_{T[\mu X.T / X]} c
  $$
  

**符号说明**：

- $U \leq_T t$ 表示对于 $U$ 中的每个令牌 $b$，存在规范形式 $c$，使得 $b \vdash_T c$ 且 $t \to c$。

**证明**：

- 使用令牌和规范形式的大小的词典序（lexicographical order）进行良基递归。
- 在递归定义中，我们可以在大小上进行归纳，因为每次递归都会减少大小。

---

**引理13.8**

假设 $t$ 是类型为 $T$ 的闭合术语，且 $U, V \in \text{Con}_T$。那么，如果 $U \vdash V$ 且 $U \leq_T t$，则 $V \leq_T t$。

**解释**：

- 这表明，如果 $U$ 能推出 $V$，并且 $U$ 与 $t$ 相关联，那么 $V$ 也与 $t$ 相关联。
- 证明需要对令牌集 $U$ 的大小进行良基归纳。

**证明**：

- 对于任意令牌 $a \in U$，如果 $U \vdash a$，并且对于 $U$ 中的每个 $b$，存在规范形式 $c$，使得 $b \vdash_T c$ 且 $t \to c$，则 $a \vdash_T c$。
- 使用良基归纳，按 $U$ 的大小进行归纳，按照词典序排列。
- 对于不同的类型 $T$，分别进行证明，特别是在函数类型和递归类型的情况下，需要仔细处理。

---

**定理13.9**

对于任何可类型的、闭合的术语 $t$，如果 $t \uparrow$，则 $t \Downarrow$。

**证明思路**：

- 我们需要证明，对于任意可类型的、闭合的术语 $t$，如果其指称不是底元，那么它在操作语义下收敛。
- 我们通过对术语 $t$ 进行结构归纳（structural induction）来证明。
- 归纳假设是：如果 $t$ 的自由变量 $x_1 : T_1, \dots, x_k : T_k$ 对应的值 $v_1 \in V_{T_1}, \dots, v_k \in V_{T_k}$，并且有闭合术语 $s_1, \dots, s_k$，使得 $v_i \leq_{T_i} s_i$ 且 $s_i \to c_i$，那么 $t[s_1 / x_1, \dots, s_k / x_k] \Downarrow$。

**具体证明**：

- **情况1：函数应用 $(t_1 \ t_2)$**
  
  - 归纳假设应用于 $t_1$ 和 $t_2$。
  - 设 $[t_1]_\rho = r$，$[t_2]_\rho = v$，且 $r \ne \bot$，$v \ne \bot$。
  - 由于 $r$ 是一个可逼近映射（approximable mapping），且 $v$ 非空，我们可以找到 $U$ 和 $V$，使得 $(U, V) \in r$，$U \subseteq v$。
  - 根据归纳假设，存在规范形式 $c_1$，使得 $t_1 \to c_1$，且 $c_1$ 是 $\lambda x.t'$ 的形式。
  - 同理，对于 $t_2$，存在规范形式 $c_2$。
  - 然后，通过函数应用的求值规则，$t_1 \ t_2 \to t'[c_2 / x]$。
  - 根据归纳假设，$t'[c_2 / x] \Downarrow$，因此 $t_1 \ t_2 \Downarrow$。
  
- **情况2：函数抽象 $\lambda y.t$**
  
  - 需要证明 $\lambda y.t[s_1 / x_1, \dots, s_k / x_k] \Downarrow$。
  - 由于 $\lambda y.t$ 本身是规范形式，且在我们的操作语义中，规范形式总是能求值到自身。
  - 因此，$\lambda y.t$ 收敛。

- **其余情况**：

  - 对于其他构造，如元组、投影、注入、模式匹配等，类似处理。

---

**练习13.10**

**问题**：

完成上述证明中结构归纳的递归情况 $\text{rec } x.t$。

**解答**：

- **情况：递归函数 $\text{rec } f.(\lambda x.t)$**
  
  - 归纳假设应用于 $t$。
  - 我们需要证明 $\text{rec } f.(\lambda x.t)[s_1 / x_1, \dots, s_k / x_k] \Downarrow$。
  - 根据指称语义，$[\text{rec } f.(\lambda x.t)]_\rho = \text{fix}(F)$，其中 $F$ 是一个连续函数，定义为 $F(r) = [\lambda x.t]_{\rho[r / f]}$。
  - 由于 $t$ 在环境 $\rho[r / f]$ 下满足归纳假设，因此 $t$ 的展开会收敛。
  - 因此，$\text{rec } f.(\lambda x.t)$ 收敛。

---

**推论13.11**

对于任何可类型的、闭合的术语 $t$，有：

$$
t \Downarrow \quad \text{当且仅当} \quad t \uparrow
$$

---

# 13.5 急迫 $\lambda$-演算

在急迫语言中，我们可以定义递归类型：

$$
\Lambda = \mu X.(X \rightarrow X)
$$

**解释**：

- 该类型 $\Lambda$ 表示所有从自身到自身的函数的类型，即 $\Lambda$ 与其自身的提升的函数空间是相等的。
- 这是一个非常有趣的类型，因为它可以表示可以应用于自身的函数，从而允许定义不动点算子等。

术语仅使用类型为 $\Lambda$ 的术语构建，并且不包含 $\text{rec}$，可以被简单地描述为：

$$
t ::= x \mid t_1 \cdot t_2 \mid \Lambda x.t
$$

- 其中，$x$ 是类型为 $\Lambda$ 的变量。
- 我们使用以下缩写：

  - $t_1 \cdot t_2 = \text{rep}(t_1) \ t_2$
  - $\Lambda x.t = \text{abs}(\lambda x.t)$

- 根据类型规则，如果 $t, t_1, t_2$ 是类型为 $\Lambda$ 的术语，那么应用 $t_1 \cdot t_2$ 和抽象 $\Lambda x.t$ 也是类型为 $\Lambda$ 的术语。

这形成了一个可以做一些悖论性的事情的 $\lambda$-演算，例如将函数应用于自身，并且我们甚至可以定义不动点算子。

---

### 规范形式和求值规则

在这些术语中，唯一的规范形式是闭合的抽象 $\Lambda x.t$。它们的求值被捕获在规则：

$$
\Lambda x.t \to \Lambda x.t
$$

这当然是从操作语义中推导出来的。

对于应用 $(t_1 \cdot t_2)$ 的求值，我们可以从操作语义中得到以下推导：

1. $t_1 \to \text{abs}(\lambda x.t') = \Lambda x.t'$
2. $\text{rep}(t_1) \to \lambda x.t'$
3. $(\lambda x.t') \ t_2 \to t'[t_2 / x]$

这可以总结为推导规则：

$$
\frac{t_1 \to \Lambda x.t' \quad t_2 \to c_2 \quad t'[c_2 / x] \to c}{t_1 \cdot t_2 \to c}
$$

这个规则表明应用 $(t_1 \cdot t_2)$ 以急迫的方式求值。

---

### 急迫 $\lambda$-演算的指称语义

急迫 $\lambda$-演算继承了整个语言的指称语义。通过限制指称语义到这些术语，我们得到：

- **变量**：

  $$
  [x]_\rho = \rho(x)
  $$

- **应用**：

  $$
  [t_1 \cdot t_2]_\rho = [t_1]_\rho . [t_2]_\rho
  $$

  - 其中，应用 $f . d$ 被定义为：

    $$
    f . d = \bigcup \{ V \mid \exists U \subseteq d, \ (U, V) \in f \}
    $$

- **抽象**：

  $$
  [\Lambda x.t]_\rho = \{ (U, V) \mid U \in \text{Con}_\Lambda, V \subseteq_\text{fin} [t]_{\rho[U / x]} \} \cup \{ (\emptyset, \emptyset) \}
  $$

---

### 充分性结果

对于整个语言的充分性结果，也适用于急迫 $\lambda$-演算：一个急迫 $\lambda$-演算的闭合术语的指称是非底元当且仅当它在操作语义下收敛。

---

## 13.5.1 等式理论

我们可以认为两个相同类型的术语是等价的，当且仅当它们具有相同的指称。即，对于相同类型的术语 $t_1, t_2$，定义：

$$
t_1 = t_2 \quad \text{当且仅当} \quad [t_1]_\rho = [t_2]_\rho \quad \text{对于所有环境 } \rho
$$

定义：

$$
t \! \quad \text{当且仅当} \quad \forall \rho, \ [t]_\rho \ne \bot
$$

这在急迫 $\lambda$-演算中，$t$ 是可类型的术语，且在所有环境中收敛。

### 等式规则

1. **反身性**：

   $$
   t = t
   $$

2. **对称性**：

   $$
   \text{如果 } t_1 = t_2, \quad \text{则 } t_2 = t_1
   $$

3. **传递性**：

   $$
   \text{如果 } t_1 = t_2 \quad \text{且} \quad t_2 = t_3, \quad \text{则 } t_1 = t_3
   $$

4. **替换性**：

   $$
   \text{如果 } t_1 = t_2, \quad \text{则 } t[t_1 / x] = t[t_2 / x]
   $$

   - 前提是替换不会导致自由变量被绑定。

### 收敛性规则

- **变量和抽象总是收敛的**：

  $$
  x \! \quad \text{如果 } x \text{ 是类型 } \Lambda \text{ 的变量}
  $$

  $$
  \Lambda x.t \!
  $$

### 转换规则

1. **$\alpha$-转换**（变量重命名）：

   $$
   \Lambda x.t = \Lambda y.(t[y / x])
   $$

   - 前提是 $y$ 不在 $t$ 中自由或绑定。

2. **$\beta$-转换**：

   $$
   (\Lambda x.t) \cdot u = t[u / x]
   $$

   - 前提是替换不会导致自由变量被绑定。

3. **$\eta$-转换**：

   $$
   t \! \implies t = \Lambda x.(t \cdot x)
   $$

   - 前提是 $x$ 不在 $t$ 的自由变量中。

**练习13.12**

**问题**：

从指称语义证明规则（$\eta$）的正确性。

**解答**：

- 根据指称语义，$[\Lambda x.(t \cdot x)]_\rho$ 表示的函数与 $[t]_\rho$ 相同。
- 由于 $t$ 收敛，因此 $[t]_\rho \ne \bot$，从而 $[t]_\rho$ 是一个函数。
- 因此，$[\Lambda x.(t \cdot x)]_\rho = [t]_\rho$，即 $t = \Lambda x.(t \cdot x)$。

---

**练习13.13**

**问题**：

证明以下两个规则也是正确的：

- 如果 $t_1 = t_2$ 且 $t \!$，则 $t[t_1 / x] = t[t_2 / x]$。

- 如果 $t_1 = t_2$ 且 $t \!$，则 $t_1 \!$。

**解答**：

- 第一个规则是替换的等价性，在 $t$ 收敛的情况下，替换不会改变指称，因此替换后仍然等价。
- 第二个规则表明，如果 $t_1 = t_2$，且 $t$ 收敛，那么 $t_1$ 也收敛。

---

**练习13.14**

**问题**：

证明以下两个“严格性”规则也是正确的：

- 如果 $t \cdot u \!$，则 $t \!$。

- 如果 $t \cdot u \!$，则 $u \!$。

**解答**：

- 由于应用 $t \cdot u$ 收敛，按照操作语义，$t$ 和 $u$ 必须分别收敛到某个值，因此 $t$ 和 $u$ 都收敛。

---

**练习13.15**

**问题**：

为完整的急迫语言（不仅仅是急迫 $\lambda$-演算）给出等式 $=$ 和收敛性 $\!$ 的规则。

**解答**：

- 可以将上述规则扩展到所有的类型和术语，包括乘积类型、和类型、递归类型等。
- 需要定义类似的转换规则、替换规则和收敛性规则。

---

## 13.5.2 不动点算子

急迫 $\lambda$-演算非常强大，甚至可以在其中编码自然数和可计算的操作。

特别地，它有一个行为类似于不动点算子的术语 $Y$：

$$
Y = \Lambda f.(\Lambda x.\Lambda y.f \cdot (x \cdot x) \cdot y) \cdot (\Lambda x.\Lambda y.f \cdot (x \cdot x) \cdot y)
$$

**解释**：

- 这是一个固定的术语，应用于函数 $f$，可以得到 $Y \cdot f = f \cdot (Y \cdot f)$。

**推导**：

1. 应用 $\beta$-转换和等式规则，我们可以推导：

   $$
   Y \cdot F = (\Lambda x.\Lambda y.F \cdot (x \cdot x) \cdot y) \cdot (\Lambda x.\Lambda y.F \cdot (x \cdot x) \cdot y)
   $$

2. 继续化简：

   $$
   Y \cdot F = \Lambda y.F \cdot ((\Lambda x.\Lambda y.F \cdot (x \cdot x) \cdot y) \cdot (\Lambda x.\Lambda y.F \cdot (x \cdot x) \cdot y)) \cdot y
   $$

3. 注意到递归定义，我们得到：

   $$
   Y \cdot F = \Lambda y.F \cdot (Y \cdot F) \cdot y
   $$

这表明 $Y \cdot F$ 是 $F$ 的不动点。

---

**练习13.16**

**问题**：

(i) 从操作语义证明，对于任何急迫 $\lambda$-演算的闭合术语 $F$，$Y_1 \cdot F$ 发散，其中：

$$
Y_1 = \Lambda f.(\Lambda x.f \cdot (x \cdot x)) \cdot (\Lambda x.f \cdot (x \cdot x))
$$

(ii) 设 $F$ 是一个急迫 $\lambda$-演算的术语 $F = \Lambda g.(\Lambda z.h)$，证明：

$$
Y' = \Lambda f.(\Lambda x.f \cdot (\Lambda y.x \cdot x \cdot y)) \cdot (\Lambda x.f \cdot (\Lambda y.x \cdot x \cdot y))
$$

则有 $Y' \cdot F = F \cdot (Y' \cdot F)$。

**解答**：

(i) 由于急迫 $\lambda$-演算是严格的，$Y_1 \cdot F$ 的求值会无限地展开，导致发散。

(ii) 通过类似之前的推导，可以证明 $Y' \cdot F = F \cdot (Y' \cdot F)$。

---

### 不动点的指称语义

为了更好地理解 $Y$ 与最小不动点算子 $\text{fix}$ 的关系，我们考虑 $Y \cdot f$ 的指称，其中 $f : \Lambda$。

**引理13.17**

对于 $f, d \in |\Lambda|^\bot$，令牌 $b$ 和令牌集合 $V$，有：

$$
V \subseteq_\text{fin} f . d \quad \text{当且仅当} \quad (V = \emptyset) \ \text{或} \ \exists U \subseteq d, \ (U, V) \in f
$$

**解释**：

- 这是关于应用的性质的一个技术性引理，帮助我们理解应用在指称语义中的行为。

**证明**：

- 根据可逼近映射的性质，证明两个方向的等价关系。

---

**定理13.19**

对于任意环境 $\rho$，有：

$$
[Y \cdot f]_\rho = \text{fix}(\text{down} \circ |f|)
$$

其中，$\text{down}$ 是一个从 $|\Lambda|^\bot$ 到 $|\Lambda|$ 的函数，定义为：

$$
\text{down}(d) = \begin{cases}
d, & \text{如果 } d \in |\Lambda| \\
\bot_\Lambda, & \text{如果 } d = \bot
\end{cases}
$$

**解释**：

- 这表明 $Y \cdot f$ 的指称是 $\text{down} \circ |f|$ 的最小不动点。
- 这与递归定义和最小不动点的思想一致。

---

**练习13.20**

**问题**：

设

$$
f_1 = (\Lambda x.x \cdot x) \cdot (\Lambda x.x \cdot x)
$$

是急迫 $\lambda$-演算的一个术语。证明：

$$
[f_1]_\rho = \bot
$$

即，$f_1$ 在任意环境 $\rho$ 下的指称是底元。

**提示**：

- 采用定理13.19的证明中的相同约定。
- 首先注意到 $f_1$ 的指称是非空的，因此 $V \subseteq_\text{fin} f_1$ 当且仅当存在 $U \subseteq (\Lambda x.x \cdot x)$，使得 $(U, V) \in (\Lambda x.x \cdot x)$。
- 其次，证明 $(U, V) \in (\Lambda x.x \cdot x) \implies U \vdash_\Lambda (U, V)$。
- 最后，通过检查 $\vdash_\Lambda$ 的定义，得到存在最小的 $U$，并导出矛盾。

**解答**：

- **第一步**：由于 $f_1$ 的定义，我们需要考虑它的指称。
- **第二步**：假设存在非空的 $V \subseteq_\text{fin} f_1$，则存在 $U$，使得 $U \subseteq (\Lambda x.x \cdot x)$，且 $(U, V) \in (\Lambda x.x \cdot x)$。
- **第三步**：由于 $(U, V) \in (\Lambda x.x \cdot x)$，根据指称语义的定义，$U \vdash_\Lambda (U, V)$。
- **第四步**：因为 $U$ 包含 $(U, V)$，这会导致无限递归，无法找到最小的 $U$，从而导出矛盾。
- **结论**：因此，$f_1$ 的指称必须是底元，即 $[f_1]_\rho = \bot$。

---

# 总结

通过详细分析，我们证明了操作语义和指称语义在术语的收敛性上是一致的。这对于确保语言的语义一致性和正确性至关重要。我们还探讨了急迫 $\lambda$-演算中的不动点算子，以及如何在指称语义中解释它们。

希望这些详细的解释能帮助你更好地理解这部分内容。

### ----------------------------

# 13.6 惰性语言（Lazy Language）

在本节中，我们将讨论一种具有**惰性求值（lazy evaluation）**的语言。这种语言与急迫语言（eager language）有一些细微的区别，特别是在语法和类型方面。

## 类型的改变

在惰性语言中，类型的定义与急迫语言相似，但有一个小的变化：**最小的类型是 $0$**，而不是 $1$。类型 $0$ 将没有任何值，所有类型为 $0$ 的术语都会发散（diverge），也就是说，它们的求值不会终止。

**类型的定义**：

$$
T ::= X \mid 0 \mid T_1 * T_2 \mid T_1 \rightarrow T_2 \mid T_1 + T_2 \mid \mu X.T
$$

- **$X$**：类型变量，取自一个无限集合。
- **$0$**：最小的类型，没有值的类型。
- **$\mu X.T$**：递归定义的类型。

类型变量 $X$ 可以在 $\mu X.T$ 中递归地出现。

## 术语的语法

在惰性语言中，未经类型检查的术语的语法如下：

$$
\begin{align*}
t ::= &\quad \bullet \mid (t_1, t_2) \mid \text{fst}(t) \mid \text{snd}(t) \mid x \mid \lambda x.t \mid (t_1 \ t_2) \mid \\
&\quad \text{inl}(t) \mid \text{inr}(t) \mid \text{case } t \text{ of inl}(x_1).t_1, \ \text{inr}(x_2).t_2 \mid \\
&\quad \text{abs}(t) \mid \text{rep}(t) \mid \\
&\quad \text{rec } x.t
\end{align*}
$$

- **$\bullet$**：表示**底元**或**发散**，类型为 $0$。
- **$\text{rec } x.t$**：递归定义，与急迫语言中的不同之处在于，这里 $t$ 不一定是一个抽象（abstraction）。

其余的构造与急迫语言相同。

**变量**：

- $x, x_1, x_2$ 是变量，取自变量集合 $\text{Var}$。

## 类型规则

除了上面的变化，类型规则也有相应的调整。

### 基本类型规则

- **底元**：

  $$
  \frac{}{\,\bullet : 0}
  $$

- **变量**：

  $$
  \frac{x : T}{x : T}
  $$

- **递归定义**：

  $$
  \frac{x : T \quad t : T}{\text{rec } x.t : T}
  $$

其余的类型规则与急迫语言相同，包括乘积类型、函数类型、和类型、抽象和表示等。

### 自由变量和闭合术语

术语的自由变量（free variables）和闭合术语（closed term）的定义与之前相同。

---

# 13.7 惰性操作语义

在惰性语言中，**规范形式（canonical forms）**的定义有所不同。

## 规范形式

类型为 $T$ 的规范形式 $C_T$ 由以下规则定义：

1. **乘积类型**：

   $$
   \frac{t_1 : T_1 \quad t_2 : T_2 \quad t_1 \text{ 和 } t_2 \text{ 闭合}}{(t_1, t_2) \in C_{T_1 * T_2}}
   $$

2. **函数类型**：

   $$
   \frac{\lambda x.t : T_1 \rightarrow T_2 \quad \lambda x.t \text{ 闭合}}{\lambda x.t \in C_{T_1 \rightarrow T_2}}
   $$

3. **和类型**：

   - 左注入：

     $$
     \frac{t_1 : T_1 \quad t_1 \text{ 闭合}}{\text{inl}(t_1) \in C_{T_1 + T_2}}
     $$

   - 右注入：

     $$
     \frac{t_2 : T_2 \quad t_2 \text{ 闭合}}{\text{inr}(t_2) \in C_{T_1 + T_2}}
     $$

4. **递归类型**：

   $$
   \frac{c \in C_{T[\mu X.T / X]}}{\text{abs}(c) \in C_{\mu X.T}}
   $$

**解释**：

- 在惰性语言中，规范形式可以包含**未被求值的部分**。例如，在乘积类型的规范形式中，$t_1$ 和 $t_2$ 必须是闭合的，但不一定是规范形式。

---

## 例子：惰性自然数

考虑类型：

$$
\text{nat} = \mu X.(0 + X)
$$

在惰性语言中，$\text{nat}$ 的规范形式与和类型的左、右分量相关。

### 左分量的规范形式

- **形式**：

  $$
  \text{abs}(\text{inl}(t_1))
  $$

  - 其中，$t_1$ 是类型为 $0$ 的闭合术语。

- **解释**：

  - 由于类型 $0$ 没有值，但仍然有无限多个闭合术语 $t_1 : 0$。这些术语都表示发散，即底元。
  - 例如，$\bullet : 0$ 是一个闭合术语，表示发散。

- **定义 Zero**：

  $$
  \text{Zero} = \text{abs}(\text{inl}(\bullet))
  $$

  - $\text{Zero}$ 是类型为 $\text{nat}$ 的规范形式。

### 右分量的规范形式

- **形式**：

  $$
  \text{abs}(\text{inr}(t_2))
  $$

  - 其中，$t_2$ 是类型为 $\text{nat}$ 的闭合术语。

- **定义**：

  - 使用缩写：

    $$
    \text{Succ}(t_2) = \text{abs}(\text{inr}(t_2))
    $$

  - 可以构造规范形式：

    $$
    \text{Zero}, \quad \text{Succ}(\text{Zero}), \quad \text{Succ}(\text{Succ}(\text{Zero})), \quad \dots
    $$

- **特殊情况**：

  - 术语 $\text{Succ}(\text{rec } x.\text{Succ}(x))$ 对应于一个“无限”的自然数。
  - 术语 $\text{Succ}(\text{Succ}(\text{rec } x.x))$ 对应于一个部分的自然数。

---

## 求值关系

在惰性语言中，求值关系 $t \to c$ 定义在闭合术语 $t$ 和规范形式 $c$ 之间。

### 求值规则

1. **规范形式**：

   $$
   \frac{\text{如果 } c \text{ 是规范形式}}{c \to c}
   $$

2. **元组构造**：

   $$
   \frac{t \to (t_1, t_2) \quad t_1 \to c}{\text{fst}(t) \to c}
   $$

   $$
   \frac{t \to (t_1, t_2) \quad t_2 \to c}{\text{snd}(t) \to c}
   $$

3. **函数应用**：

   $$
   \frac{t_1 \to \lambda x.t_1' \quad t_1'[t_2 / x] \to c}{(t_1 \ t_2) \to c}
   $$

4. **抽象和表示**：

   $$
   \frac{t \to c}{\text{abs}(t) \to \text{abs}(c)}
   $$

   $$
   \frac{t \to \text{abs}(c)}{\text{rep}(t) \to c}
   $$

5. **递归定义**：

   $$
   \frac{t[\text{rec } x.t / x] \to c}{\text{rec } x.t \to c}
   $$

**解释**：

- 在惰性语言中，求值是**非严格的（non-strict）**，即术语的组件可能未被求值。
- 递归定义的求值规则允许 $t$ 不一定是一个抽象。

---

### 性质

**命题13.21**

对于闭合术语 $t$ 和规范形式 $c, c_1, c_2$，有：

1. **类型保持**：如果 $t \to c$ 且 $t : T$，则 $c : T$。

2. **确定性**：如果 $t \to c_1$ 且 $t \to c_2$，则 $c_1 = c_2$。

**证明**：

- **类型保持**：通过对求值规则的归纳，可以证明每个规则都保持类型正确性。
- **确定性**：通过对求值规则的归纳，可以证明对于给定的术语 $t$，如果可以求值到 $c_1$ 和 $c_2$，则 $c_1$ 和 $c_2$ 必须相等。

---

# 13.8 惰性指称语义

对于每个类型 $T$，我们关联一个信息系统，其元素是类型 $T$ 的值。由于类型 $T$ 可能包含自由类型变量（free type variables），我们需要一个**类型环境（type environment）** $\Xi$，它将每个类型变量 $X$ 映射到一个信息系统。

## 类型的指称

通过对类型表达式进行结构归纳，定义类型 $T$ 在环境 $\Xi$ 下对应的信息系统 $V_{[T]}^\Xi$：

1. **类型 $0$**：

   $$
   V_{[0]}^\Xi = (\emptyset, \{ \emptyset \}, \emptyset)
   $$

   - 这也是称为 $0$ 的信息系统。

2. **乘积类型**：

   $$
   V_{[T_1 * T_2]}^\Xi = V_{[T_1]}^\Xi \times V_{[T_2]}^\Xi
   $$

   - 使用信息系统的积。

3. **函数类型**：

   $$
   V_{[T_1 \rightarrow T_2]}^\Xi = (V_{[T_1]}^\Xi)^\bot \rightarrow (V_{[T_2]}^\Xi)^\bot
   $$

   - 使用提升的函数空间。

4. **和类型**：

   $$
   V_{[T_1 + T_2]}^\Xi = V_{[T_1]}^\Xi + V_{[T_2]}^\Xi
   $$

   - 使用信息系统的和。

5. **类型变量**：

   $$
   V_{[X]}^\Xi = \Xi(X)
   $$

6. **递归类型**：

   $$
   V_{[\mu X.T]}^\Xi = \mu I . V_{[T]}^{\Xi[I/X]}
   $$

   - 递归类型的语义是信息系统的最小不动点。

**解释**：

- 右侧的所有操作都是在信息系统上的操作。
- 递归类型 $\mu X.T$ 在环境 $\Xi$ 中，表示信息系统的最小不动点。

---

## 提升（Lifting）

在惰性语言中，我们将提升后的类型 $T^\bot$ 也表示为信息系统，并定义：

$$
V_{[T]^\bot} = (V_T^\bot) = (V_{[T]}^\bot, \text{Con}_{T^\bot}, \vdash_{T^\bot})
$$

- **底元**：$\bot$ 被解释为单元素集合 $\{ \emptyset \}$，其中 $\emptyset$ 是空集。

- **提升函数**：对于 $x \in V_T$，定义：

  $$
  \langle x \rangle = \text{Fin}(x)
  $$

  - $\text{Fin}(x)$ 表示 $x$ 的所有有限子集的集合。

---

## 环境

**环境（environment）**的 cpo 定义为：

$$
\text{Env} = \{ \rho : \text{Var} \rightarrow \bigcup \{ V_{[T]}^\bot \mid T \text{ 是封闭类型} \} \mid \rho(x) \in V_{\text{type}(x)}^\bot \}
$$

- 按照点序关系排序。

---

## 指称语义的表示

在呈现指称语义时，我们再次约定，将信息系统的和与乘积与对应的 cpo 进行同一处理：

- **和类型**：$| A | + | B | = | A + B |$
- **乘积类型**：$| A | \times | B | = | A \times B |$

对于函数空间类型，我们使用以下同构：

**命题13.23**

令 $A$ 和 $B$ 是信息系统。定义映射：

- **从元素到连续函数**：

  $$
  \| r \| = \lambda x \in | A^\bot | . \{ y \mid \exists X \subseteq x, \ (X, y) \in r \}
  $$

- **从连续函数到元素**：

  $$
  f^\dagger = \{ (X, y) \mid X \in \text{Con}_{A^\bot},\ X \ne \emptyset, \ y \in f(X) \} \cup \{ (\emptyset, \emptyset) \}
  $$

则 $\| - \|$ 和 $-^\dagger$ 是互逆的，同构 $| A^\bot | \rightarrow | B^\bot |$ 和 $[| A^\bot | \rightarrow | B^\bot |]$。

---

## 指称语义定义

**指称语义**

$$
\begin{align*}
[\,\bullet\,] &= \lambda \rho.\ \{ \emptyset \} \\
[(t_1, t_2)] &= \lambda \rho.\ \langle [t_1]_\rho, [t_2]_\rho \rangle \\
[\text{fst}(t)] &= \lambda \rho.\ \text{let } v \leftarrow [t]_\rho.\ \pi_1(v) \\
[\text{snd}(t)] &= \lambda \rho.\ \text{let } v \leftarrow [t]_\rho.\ \pi_2(v) \\
[x] &= \lambda \rho.\ \rho(x) \\
[\lambda x.t] &= \lambda \rho.\ \langle \lambda d \in V_{\text{type}(x)}^\bot . [t]_{\rho[d / x]} \rangle \\
[(t_1 \ t_2)] &= \lambda \rho.\ \text{let } r \leftarrow [t_1]_\rho.\ \| r \| ([t_2]_\rho) \\
[\text{inl}(t)] &= \lambda \rho.\ \langle \text{inl}([t]_\rho) \rangle \\
[\text{inr}(t)] &= \lambda \rho.\ \langle \text{inr}([t]_\rho) \rangle \\
[\text{case } t \text{ of } \dots ] &= \lambda \rho.\ \text{case } [t]_\rho \text{ of inl}(d_1). [t_1]_{\rho[d_1 / x_1]},\ \text{inr}(d_2). [t_2]_{\rho[d_2 / x_2]} \\
[\text{abs}(t)] &= [t] \\
[\text{rep}(t)] &= [t] \\
[\text{rec } x.t] &= \lambda \rho.\ \mu d.\ [t]_{\rho[d / x]}
\end{align*}
$$

**解释**：

- **底元**：

  - $[\,\bullet\,]$ 被解释为 $\{ \emptyset \}$，即底元。

- **元组构造**：

  - $[(t_1, t_2)]$ 被解释为 $[t_1]_\rho \times [t_2]_\rho$。

- **投影**：

  - $[\text{fst}(t)]$ 和 $[\text{snd}(t)]$ 使用投影函数 $\pi_1$ 和 $\pi_2$。

- **函数抽象**：

  - $[\lambda x.t]$ 被解释为从元素到连续函数的映射，使用 $\| - \|$。

- **函数应用**：

  - $[(t_1 \ t_2)]$ 使用了 $\| r \| ([t_2]_\rho)$，其中 $r = [t_1]_\rho$。

- **注入和模式匹配**：

  - 注入使用 $\text{inl}$ 和 $\text{inr}$。
  - 模式匹配根据 $[t]_\rho$ 的形式进行。

- **抽象和表示**：

  - 在惰性语言中，$\text{abs}$ 和 $\text{rep}$ 在语义上是恒等的。

- **递归定义**：

  - $[\text{rec } x.t]$ 被解释为不动点 $\mu d.\ [t]_{\rho[d / x]}$。

---

## 例子：惰性自然数

**类型**：

$$
\text{nat} = \mu X.(0 + X)
$$

- $\text{nat}$ 的信息系统是 $N^\bot$，是以下方程的最小解：

  $$
  N^\bot = (0)^\bot + N^\bot
  $$

- $N^\bot$ 的元素是 $| N^\bot |$。

**元素的表示**：

- **零**：

  $$
  \text{Zero} = \text{abs}(\text{inl}(\bullet))
  $$

- **后继**：

  $$
  \text{Succ}(t) = \text{abs}(\text{inr}(t))
  $$

- **元素的结构**：

  - $\text{Zero}, \quad \text{Succ}(\text{Zero}), \quad \text{Succ}(\text{Succ}(\text{Zero})), \quad \dots$

- **无限的自然数**：

  - 术语 $\text{rec } x.\text{Succ}(x)$ 表示一个“无限”的自然数。

**元素的 cpo 结构**：

- 元素之间的包含关系可以表示为一棵无限的树，其中每个节点对应于一个自然数或“无限”自然数。

---

**练习13.24**

**问题**：

解释为什么不可能将提升的自然数的 cpo $N^\bot$ 定义为惰性语言中某个类型的值或指称。惰性语言通常将该类型作为原语（primitive）。

**解答**：

- 在惰性语言中，类型 $0$ 没有值，所有类型为 $0$ 的术语都表示发散。
- 如果尝试定义一个类型来表示提升的自然数 $N^\bot$，由于类型 $0$ 没有值，无法在类型系统中表达提升的自然数。
- 因此，提升的自然数 $N^\bot$ 通常被作为原语，而不是通过类型构造来定义。

---

## 例子：惰性列表

**类型**：

$$
L = \mu Y.(0 + a * Y)
$$

- 其中，$a$ 是一个封闭的类型表达式，例如惰性自然数类型。

- $L$ 表示类型 $a$ 的惰性列表。

**信息系统**：

- **列表的信息系统**满足：

  $$
  L^\bot = (0)^\bot + A^\bot \times L^\bot
  $$

  - 其中，$A^\bot$ 是类型 $a$ 的信息系统。

**构造**：

- **空列表**：

  $$
  \text{Nil} = \text{abs}(\text{inl}(\bullet)) : L
  $$

- **列表构造函数**：

  $$
  \text{Cons} = \lambda x. \text{abs}(\text{inr}(x)) : a * L \rightarrow L
  $$

- **无限列表**：

  - 例如，术语 $\text{rec } l.\ \text{Cons}(a, l)$ 定义了一个无限的列表，其中每个元素都是 $a$。

---

**练习13.25**

**问题**：

分类类型为 $L$ 的惰性列表的不同类型的规范形式，指明它们的指称形式。

**解答**：

- **空列表**：

  - 形式为 $\text{Nil} = \text{abs}(\text{inl}(\bullet))$。
  - 指称是底元。

- **有限列表**：

  - 形式为 $\text{Cons}(a_1, \text{Cons}(a_2, \dots \text{Nil}))$。
  - 指称是由有限个元素组成的列表。

- **无限列表**：

  - 形式为 $\text{rec } l.\ \text{Cons}(a, l)$。
  - 指称是一个无限的列表。

- **部分列表**：

  - 形式为 $\text{Cons}(a_1, \text{Cons}(a_2, \text{rec } l.l))$。
  - 指称是一个部分定义的列表，可能在某个点开始发散。

---

# 总结

在本节中，我们介绍了惰性语言的语法、操作语义和指称语义。惰性语言与急迫语言的主要区别在于：

- **最小类型为 $0$**，没有值，所有类型为 $0$ 的术语都发散。
- **求值策略为惰性**，即术语的组件可能未被求值，直到需要时才进行求值。

我们讨论了惰性自然数和惰性列表的例子，展示了如何在惰性语言中定义递归类型和操作。同时，我们通过练习探讨了惰性语言中的一些细节，例如为什么提升的自然数不能作为惰性语言中的类型，以及惰性列表的规范形式和指称。

通过对惰性语言的深入理解，我们可以更好地掌握惰性求值的概念，以及如何在编程语言的语义中处理递归类型和发散行为。

### ---------------------------

# 13.9 惰性语义的充分性

在本节中，我们将证明惰性语言的**操作语义（operational semantics）**和**指称语义（denotational semantics）**在术语的求值收敛性方面是一致的。

## 收敛性的定义

令 $t : T$ 是一个闭合术语。我们说 $t$ **关于操作语义收敛**，当且仅当它可以求值到某个规范形式（canonical form）。即：

$$
t \Downarrow \quad \text{当且仅当存在 } c, \ t \to c.
$$

**解释**：

- $t \Downarrow$ 表示 $t$ 可以通过有限步操作语义规则求值到某个规范形式 $c$。

对于指称语义，我们认为，如果 $t$ 的指称在对应的 cpo $| V_T^\bot |$ 中不是底元素（bottom），则 $t$ 收敛。回想一下，$| V_T^\bot |$ 的底元素是 $\{ 0 \}$（仅包含空集的集合）。因此，我们定义：

$$
t \uparrow \quad \text{当且仅当 } U[t]_\rho \ne \emptyset \quad \text{对于任意环境 } \rho.
$$

**解释**：

- $t \uparrow$ 表示 $t$ 的指称不是底元素，即其对应的令牌集合非空。

## 从操作语义收敛到指称语义收敛

对于可类型的、闭合的术语 $t$，证明 $t \Downarrow$ 蕴涵 $t \uparrow$ 是相对直接的。以下引理有助于证明这一点。

### 引理13.26

**陈述**：

如果环境 $\rho$ 和 $\rho'$ 在 $t$ 的自由变量上取相同的值，则 $[t]_\rho = [t]_{\rho'}$。

**证明**：

- 对 $t$ 进行结构归纳。
- **基本情况**：
  - 当 $t$ 是变量 $x$ 时，$[x]_\rho = \rho(x)$，由于 $\rho$ 和 $\rho'$ 在 $x$ 上取相同的值，故 $[x]_\rho = [x]_{\rho'}$。
- **归纳步骤**：
  - 对于复合术语，如 $(t_1, t_2)$、$\lambda x.t$、$t_1 \ t_2$ 等，利用归纳假设，证明 $[t]_\rho = [t]_{\rho'}$。
- 因此，结论成立。$\quad \blacksquare$

### 引理13.27

**陈述**：

如果 $c \in C_T$（$c$ 是类型 $T$ 的规范形式），则 $c \uparrow$。

**解释**：

- 也就是说，规范形式的指称不是底元素。

**证明**：

- 对规范形式的构造规则进行归纳。
- **基本情况**：
  - 例如，$\lambda x.t$ 是规范形式，其指称是非空的。
- **归纳步骤**：
  - 对于复合的规范形式，如 $(t_1, t_2)$、$\text{inl}(t)$、$\text{abs}(c)$ 等，利用归纳假设，证明其指称非空。
- 因此，结论成立。$\quad \blacksquare$

### 引理13.28（替换引理）

**陈述**：

设 $s$ 是类型为 $\sigma$ 的闭合术语，$x$ 是类型为 $\sigma$ 的变量。假设 $t : T$，则 $t[s / x] : T$ 且：

$$
[t[s / x]] = [t]_{[s] / x}
$$

**解释**：

- 这里，$t[s / x]$ 表示在 $t$ 中用 $s$ 替换所有自由出现的 $x$。
- $[t]_{[s] / x}$ 表示在环境中将 $x$ 的值替换为 $[s]$。

**证明**：

- 对 $t$ 进行结构归纳。
- **基本情况**：
  - 当 $t$ 是变量时，分情况讨论 $t$ 是否为 $x$。
    - 如果 $t = x$，则 $[t[s / x]] = [s]$，而 $[t]_{[s] / x} = [s]$。
    - 如果 $t \ne x$，则 $[t[s / x]] = [t]$，$[t]_{[s] / x} = [t]$。
- **归纳步骤**：
  - 对于复合术语，如 $(t_1, t_2)$、$\lambda y.t$、$t_1 \ t_2$，利用归纳假设，证明替换后的指称与替换前的一致性。
- 因此，结论成立。$\quad \blacksquare$

### 引理13.29

**陈述**：

如果 $t \to c$，则对于任意闭合术语 $t$、规范形式 $c$ 和任意环境 $\rho$，有：

$$
[t]_\rho = [c]_\rho
$$

**解释**：

- 这意味着，如果 $t$ 可以求值到规范形式 $c$，则它们的指称是相同的。

**证明**：

- 对求值规则进行归纳。
- **基本情况**：
  - 当 $t$ 本身就是规范形式 $c$ 时，$[t]_\rho = [c]_\rho$ 显然成立。
- **归纳步骤**：
  - 对于每个求值规则，假设前提成立，证明结论的指称与前提的指称一致。
- 因此，结论成立。$\quad \blacksquare$

因此，对于可类型的、闭合的术语 $t$，如果 $t \Downarrow$，则 $t \uparrow$。

---

## 从指称语义收敛到操作语义收敛

证明反过来的方向，即对于可类型的、闭合的术语 $t$，如果 $t \uparrow$，则 $t \Downarrow$，需要使用一种**逻辑关系（logical relation）**，记为 $\leq_T$，它关联了 $V_T$ 的令牌子集和规范形式 $C_T$。

### 引理13.30

**陈述**：

对于每个封闭类型 $T$，存在一个关系 $C_T$，它将令牌 $a \in \text{Tok}_T$ 与规范形式 $c \in C_T$ 关联，满足以下性质：

- **函数类型**：

  $$
  (U, V) \ C_{T_1 \rightarrow T_2} \ \lambda x.t \quad \text{当且仅当} \quad (U \leq_{T_1} s \implies V \leq_{T_2} t[s / x] \ \text{对于任意闭合的 } s : T_1)
  $$

- **和类型**：

  $$
  (1, a) \ C_{T_1 + T_2} \ \text{inl}(t) \quad \text{当且仅当} \quad a \leq_{T_1} t
  $$

  $$
  (2, b) \ C_{T_1 + T_2} \ \text{inr}(t) \quad \text{当且仅当} \quad b \leq_{T_2} t
  $$

- **递归类型**：

  $$
  a \ C_{\mu X.T} \ \text{abs}(c) \quad \text{当且仅当} \quad a \ C_{T[\mu X.T / X]} \ c
  $$

其中，我们定义：

$$
U \leq_T t \quad \text{当且仅当} \quad \forall b \in U, \ \exists c \in C_T, \ (b \ C_T \ c \ \text{且} \ t \to c)
$$

- $U$ 是 $V_T$ 的令牌子集，$t$ 是闭合术语。

**证明**：

- 使用令牌大小和规范形式结构的词典序（lexicographical order）进行良基递归（well-founded recursion）。
- **构造**：
  - 按照令牌和规范形式的大小，从最小的开始定义关系 $C_T$。
- 因此，关系 $C_T$ 存在，满足上述性质。$\quad \blacksquare$

### 引理13.31

**陈述**：

对于 $U \in \text{Con}_{T^\bot}$ 和闭合术语 $t : T$，有：

$$
U \leq_T t \implies U \leq_T t
$$

**解释**：

- 这里，$U \leq_T t$ 表示 $U$ 与 $t$ 关联，$U \leq_T t$ 表示 $U$ 与 $t$ 的指称关联。
- 引理表明，如果 $U$ 与 $t$ 关联，那么 $U$ 与 $t$ 的指称也关联。

**证明**：

- 该引理可从以下事实推导：

  $$
  U \leq_T c \quad \text{且} \quad U \vdash_{T^\bot} a \implies a \leq_T c
  $$

  - 其中，$U \in \text{Con}_{T^\bot}$，$a \in \text{Tok}_{T^\bot}$，$c \in C_T$。

- 通过对 $U \cup \{ a \}$ 的大小进行良基归纳，并按照 $c$ 的结构词典序排序。
- 证明根据类型 $T$ 的不同形式进行讨论。
- 因此，结论成立。$\quad \blacksquare$

### 引理13.32

**陈述**：

对于每个可类型的、闭合的术语 $t$，如果 $t \uparrow$，则 $t \Downarrow$。

**证明思路**：

- 通过对术语 $t$ 进行结构归纳，证明对于所有类型为 $T$ 的术语 $t$，其自由变量为 $z_1 : \sigma_1, \dots, z_k : \sigma_k$，如果 $U_{d_1} \leq_{\sigma_1} s_1, \dots, U_{d_k} \leq_{\sigma_k} s_k$，其中 $d_i \in | V_{\sigma_i}^\bot |$，$s_i$ 是闭合术语，且 $U[t]_{[d_i / z_i]} \ne \emptyset$，则存在规范形式 $c$，使得 $t[s_1 / z_i, \dots] \to c$。

- 特别地，当 $t$ 是闭合的，即没有自由变量，我们有 $U[t]_\rho \ne \emptyset$，因此存在规范形式 $c$，使得 $t \to c$。

**具体证明**：

- **情况：$t$ 是一个抽象 $\lambda x.t'$**

  - 利用引理13.31。
  - 假设 $U \leq_T t$，需要证明 $t$ 能够求值到某个规范形式。
  - 由于 $t$ 是 $\lambda x.t'$，它本身就是一个规范形式。
  - 因此，$t \to t$。

- **其他情况**：

  - 对于其他构造，如应用、元组、注入等，利用归纳假设和引理，证明 $t$ 能够求值到规范形式。

- 因此，结论成立。$\quad \blacksquare$

---

# 13.10 惰性 $\lambda$-演算

在惰性语言中，我们可以定义递归类型：

$$
\Lambda = \mu X.(X \rightarrow X)
$$

**解释**：

- 该类型 $\Lambda$ 表示所有从自身到自身的函数的类型，即 $\Lambda \cong \Lambda^\bot \rightarrow \Lambda^\bot$。
- 这意味着类型 $\Lambda$ 的指称落在满足 $D \cong D \rightarrow D$ 的 cpo $D = | \Lambda^\bot |$ 中。

就像在急迫情况下一样，类型 $\Lambda$ 的术语构成了一个 $\lambda$-演算：

$$
t ::= x \mid t_1 \cdot t_2 \mid \Lambda x.t
$$

- 其中，$x$ 是类型为 $\Lambda$ 的变量。
- 使用以下缩写：

  - $t_1 \cdot t_2 \equiv (\text{rep}(t_1) \ t_2)$
  - $\Lambda x.t \equiv \text{abs}(\lambda x.t)$

我们从完整的语言中继承了操作语义和指称语义。在这些术语中，唯一的规范形式是闭合的抽象 $\Lambda x.t$。

## 操作语义规则

从操作语义中，我们得到以下规则：

1. **抽象的求值**：

   $$
   \Lambda x.t \to \Lambda x.t
   $$

2. **应用的求值**：

   $$
   \frac{t_1 \to \lambda x.t_1' \quad t_1'[t_2 / x] \to c}{(t_1 \cdot t_2) \to c}
   $$

**解释**：

- 这两个规则足以推导 $\lambda$-演算中闭合术语 $t$ 的任何求值关系 $t \to c$。
- 由于应用的求值方式，这些术语构成了一个**惰性 $\lambda$-演算**。

## 指称语义

通过将指称语义限制在 $\lambda$-演算的术语上，我们得到：

1. **变量**：

   $$
   [x]_\rho = \rho(x)
   $$

2. **应用**：

   $$
   [t_1 \cdot t_2]_\rho = [t_1]_\rho \cdot [t_2]_\rho
   $$

   - 其中，$[t_1]_\rho \cdot [t_2]_\rho$ 的定义为：

     $$
     \phi . d = \{ V \mid \exists U \subseteq d, \ (U, V) \in \phi \} \cup \{ 0 \}
     $$

3. **抽象**：

   $$
   [\Lambda x.t]_\rho = \{ (U, V) \mid U \ne \emptyset \in \text{Con}_{\Lambda^\bot}, \ V \in [t]_\rho[U / x] \} \cup \{ (\emptyset, \emptyset) \}
   $$

**解释**：

- 对于惰性 $\lambda$-演算来说，环境 $\rho$ 中唯一相关的部分是它如何将变量 $x : \Lambda$ 映射到 $| \Lambda^\bot |$ 的元素。

---

## 13.10.1 等式理论

我们认为惰性 $\lambda$-演算中的两个术语等价，当且仅当它们具有相同的指称。即，对于相同类型的术语 $t_1, t_2$，定义：

$$
t_1 = t_2 \quad \text{当且仅当} \quad [t_1]_\rho = [t_2]_\rho \quad \text{对于所有环境 } \rho.
$$

我们还定义：

$$
t \uparrow \quad \text{当且仅当} \quad \forall \rho, \ [t]_\rho \ne \emptyset
$$

**解释**：

- $t \uparrow$ 表示 $t$ 在所有环境下的指称都不是底元素。

### 等式规则

1. **反身性（refl）**：

   $$
   t = t
   $$

2. **替换规则（eq1）**：

   $$
   t_1 = t_2 \implies t[t_1 / x] = t[t_2 / x]
   $$

   - 前提是替换不会导致自由变量被绑定。

### 收敛性规则

- **如果 $t = t$，则 $t$ 收敛**：

  $$
  t = t \implies t \uparrow
  $$

### 转换规则

1. **$\alpha$-转换**：

   $$
   \lambda x.t = \lambda y.(t[y / x]) \quad \text{前提是 } y \text{ 不在 } t \text{ 中出现（自由或绑定）}
   $$

2. **$\beta$-转换**：

   $$
   (\lambda x.t) \ u = t[u / x] \quad \text{前提是替换不会导致自由变量被绑定}
   $$

3. **$\eta$-转换**：

   $$
   t \uparrow \implies t = \lambda x.(t \ x) \quad \text{前提是 } x \text{ 不是 } t \text{ 的自由变量}
   $$

**区别**：

- 与急迫 $\lambda$-演算不同，在惰性情况下，变量不一定收敛（因为它们可能不代表值）。
- $\beta$-转换在参数是否收敛的情况下都成立。

---

**练习13.33**

**问题**：

从指称语义中证明上述规则的正确性。

**解答**：

- **反身性（refl）**：显然成立，因为 $[t]_\rho = [t]_\rho$。
  
- **替换规则（eq1）**：

  - 假设 $t_1 = t_2$，即对于所有环境 $\rho$，$[t_1]_\rho = [t_2]_\rho$。
  - 我们需要证明 $[t[t_1 / x]]_\rho = [t[t_2 / x]]_\rho$。
  - 由于替换不会导致自由变量被绑定，指称语义中的替换引理（引理13.28）适用。
  - 因此，$[t[t_1 / x]]_\rho = [t]_{\rho[[t_1]_\rho / x]}$，同理对于 $t_2$。
  - 因为 $[t_1]_\rho = [t_2]_\rho$，所以结论成立。

- **收敛性规则（eq2）**：

  - 如果 $t = t$，那么显然 $[t]_\rho = [t]_\rho \ne \emptyset$，因此 $t \uparrow$。

- **$\alpha$-转换**：

  - 由于 $y$ 不在 $t$ 中出现，替换 $t[y / x]$ 不会改变 $t$ 的指称。
  - 因此，$[\lambda x.t]_\rho = [\lambda y.(t[y / x])]_\rho$。

- **$\beta$-转换**：

  - 我们需要证明 $[(\lambda x.t) \ u]_\rho = [t[u / x]]_\rho$。
  - 根据指称语义，$[(\lambda x.t) \ u]_\rho = [\lambda x.t]_\rho \cdot [u]_\rho$。
  - 而 $[\lambda x.t]_\rho \cdot [u]_\rho$ 的定义与 $[t]_{\rho[[u]_\rho / x]}$ 一致。
  - 因此，结论成立。

- **$\eta$-转换**：

  - 假设 $t \uparrow$，即对于所有 $\rho$，$[t]_\rho \ne \emptyset$。
  - 我们需要证明 $[t]_\rho = [\lambda x.(t \ x)]_\rho$。
  - 对于任何环境 $\rho$，有 $[t]_\rho \ne \emptyset$。
  - 计算 $[\lambda x.(t \ x)]_\rho$，其指称与 $[t]_\rho$ 相同。
  - 因此，$[t]_\rho = [\lambda x.(t \ x)]_\rho$。

$\quad \blacksquare$

---

**练习13.34**

**问题**：

证明以下“严格性”规则的正确性：

$$
t \cdot u \uparrow \implies t \uparrow
$$

**解答**：

- 假设 $t \cdot u \uparrow$，即对于所有环境 $\rho$，$[t \cdot u]_\rho \ne \emptyset$。
- 根据指称语义，$[t \cdot u]_\rho = [t]_\rho \cdot [u]_\rho$。
- 如果 $[t]_\rho = \emptyset$，则 $[t]_\rho \cdot [u]_\rho = \{ 0 \}$，因为应用于底元素会导致底元素。
- 但这与 $[t \cdot u]_\rho \ne \emptyset$ 矛盾。
- 因此，$[t]_\rho \ne \emptyset$，即 $t \uparrow$。

$\quad \blacksquare$

---

**练习13.35**

**问题**：

为完整的惰性语言提出等式 $=$ 和收敛性 $\uparrow$ 的规则。

**解答**：

- **等式规则**：

  - 反身性、对称性、传递性。
  - 替换规则：如果 $t_1 = t_2$，则 $t[t_1 / x] = t[t_2 / x]$，前提是替换不会导致自由变量被绑定。

- **收敛性规则**：

  - 如果 $t$ 是规范形式，则 $t \uparrow$。
  - 对于构造，如元组、注入、抽象等，如果其组件收敛，则整体收敛。

- **转换规则**：

  - $\alpha$-转换、$\beta$-转换、$\eta$-转换，类似于惰性 $\lambda$-演算的情况。

---

## 13.10.2 不动点算子

惰性 $\lambda$-演算有一个比急迫演算更简单的不动点算子——不再需要通过抽象来保护参数免于被求值。定义：

$$
Y = \lambda f.(\lambda x.f.(x \ x)) \ (\lambda x.f.(x \ x))
$$

**推导**：

1. 利用 $\beta$-转换，我们有：

   $$
   Y \cdot f = (\lambda x.f.(x \ x)) \ (\lambda x.f.(x \ x)) \tag{1}
   $$

2. 继续应用 $\beta$-转换：

   $$
   = f \ ( (\lambda x.f.(x \ x)) \ (\lambda x.f.(x \ x)) )
   $$

3. 由于式子 $(1)$，我们有：

   $$
   = f \ (Y \cdot f)
   $$

   - 这使用了等式规则（eq1），因为 $Y \cdot f = (\lambda x.f.(x \ x)) \ (\lambda x.f.(x \ x))$。

因此，我们得到：

$$
Y \cdot f = f \ (Y \cdot f)
$$

**解释**：

- 这表明 $Y \cdot f$ 是 $f$ 的不动点。

---

为了理解 $Y$ 的指称，我们引入一个函数 $\text{down} : | \Lambda^\bot | \to | \Lambda |$，使用 $| \Lambda |$ 的底元素定义。

- 由于：

  $$
  | \Lambda^\bot | = | \Lambda | \cup \{ \bot \}
  $$

- 约定 $| \Lambda |$ 的底元素为：

  $$
  \bot = \{ (U, 0) \mid U \in \text{Con}_\Lambda \}
  $$

- 定义 $\text{down} : | \Lambda^\bot | \to | \Lambda |$，如下：

  $$
  \text{down}(d) = \left( \bigcup d \right) \cup \bot_\Lambda
  $$

---

**引理13.36**

**陈述**：

对于 $\phi, d \in | \Lambda^\bot |$，有：

$$
\phi . d = \| \text{down}(\phi) \| (d)
$$

**证明**：

- 根据应用的定义：

  $$
  \phi . d = \{ V \mid \exists U \subseteq d, \ (U, V) \in \phi \} \cup \{ 0 \}
  $$

- 而 $\| \text{down}(\phi) \| (d)$ 的定义与上述表达式一致。

$\quad \blacksquare$

---

根据引理13.36，由于 $Y \cdot f = f \ (Y \cdot f)$，我们有：

$$
[Y \cdot f]_\rho = \rho(f) . [Y \cdot f]_\rho = \| \text{down}(\rho(f)) \| ([Y \cdot f]_\rho)
$$

因此，$[Y \cdot f]_\rho$ 是函数 $\| \text{down}(\rho(f)) \| : | \Lambda^\bot | \to | \Lambda^\bot |$ 的不动点。

因此，

$$
\text{fix}(\| \text{down}(\rho(f)) \|) \subseteq [Y \cdot f]_\rho
$$

接下来，我们将证明反向包含也成立，从而得到等式。

---

**定理13.37**

**陈述**：

令

$$
Y = \lambda f.(\lambda x.f.(x \ x)) \ (\lambda x.f.(x \ x))
$$

则对于任意环境 $\rho$，有：

$$
[Y \cdot f]_\rho = \text{fix}(\| \text{down}(\rho(f)) \|)
$$

**证明**：

- 证明所需的反向包含与定理13.19 的证明非常相似，我们将采用类似的记号。

- 假设特定的环境 $\rho$。

- 令 $\text{Fix}_f = \text{fix}(\| \text{down}(\rho(f)) \|)$。

- 我们将术语与其指称等同起来，记为：

  $$
  b \in t \quad \text{表示} \quad b \in [t]_\rho
  $$

- 首先注意，$\text{Fix}_f$ 可以描述为满足 $d = \rho(f) . d$ 的最小 $d \in | \Lambda^\bot |$，即：

  $$
  d = \{ V \mid \exists U \subseteq d, \ (U, V) \in \rho(f) \} \cup \{ 0 \}
  $$

- 根据 $\beta$-转换，我们有：

  $$
  Y \cdot f = (\lambda x.f.(x \ x)) \ (\lambda x.f.(x \ x))
  $$

- 因此，

  $$
  V \in Y \cdot f \iff V \in (\lambda x.f.(x \ x)) \ (\lambda x.f.(x \ x))
  $$

  $$
  \iff V = 0 \quad \text{或} \quad \exists U \subseteq (\lambda x.f.(x \ x)), \ (U, V) \in (\lambda x.f.(x \ x))
  $$

- 如果 $V = 0$，显然 $V \in \text{Fix}_f$。

- 因此，我们需要证明对于所有 $U \in \text{Con}_{\Lambda^\bot}$，如果：

  $$
  U \subseteq (\lambda x.f.(x \ x)) \quad \text{且} \quad (U, V) \in (\lambda x.f.(x \ x))
  $$

  则 $V \in \text{Fix}_f$。

- 我们通过对 $U$ 的大小进行归纳证明这个性质。

- **归纳步骤**：

  - 假设对于所有大小小于 $U$ 的 $U'$，性质成立。

  - 我们需要证明，当 $U$ 满足上述条件时，$V \in \text{Fix}_f$。

  - 根据指称语义，有：

    $$
    (U, V) \in (\lambda x.f.(x \ x)) \implies V \in [f.(x \ x)]_\rho[U / x]
    $$

  - 而：

    $$
    [f.(x \ x)]_\rho[U / x] = \rho(f) . (U . U)
    $$

  - 因此，存在 $W \subseteq U . U$，使得 $(W, V) \in \rho(f)$。

  - 我们需要证明 $W \subseteq \text{Fix}_f$。

  - 由于 $W \subseteq U . U$，并且 $U \subseteq (\lambda x.f.(x \ x))$，我们可以应用归纳假设，得出 $W \subseteq \text{Fix}_f$。

- 因此，$V \in \text{Fix}_f$。

- 这完成了证明。$\quad \blacksquare$

---

# 总结

通过以上证明，我们得到了惰性语言的充分性结果：对于可类型的、闭合的术语 $t$，$t$ 在指称语义中收敛（$t \uparrow$）当且仅当 $t$ 在操作语义中收敛（$t \Downarrow$）。这保证了惰性语言的操作语义和指称语义在收敛性方面的一致性。

希望这些详细的解释能够帮助你更好地理解本章的内容。如果你还有任何疑问，欢迎继续提问！

### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------