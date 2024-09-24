[toc]



### 第5章 无类型Lambda演算 (The Untyped Lambda-Calculus)

本章回顾了**无类型 (untyped)** 或**纯粹 (pure)** Lambda演算的定义和一些基本性质。Lambda演算是本书其余部分所描述的大多数类型系统的基础“计算基石”。

#### 历史背景

在20世纪60年代中期，彼得·兰丁（Peter Landin）观察到，一个复杂的编程语言可以通过将其形式化为一个微小的核心演算来理解，该核心演算捕获了该语言的基本机制，同时还有一系列方便的派生形式，其行为通过将它们翻译成核心来理解（Landin 1964, 1965, 1966；另见 Tennent 1981）。兰丁使用的核心语言是**Lambda演算 (lambda-calculus)**，这是阿隆佐·丘奇（Alonzo Church）在20世纪20年代发明的一个形式系统（1936, 1941），在该系统中，所有计算都被简化为函数定义和应用的基本操作。

继兰丁的洞察以及约翰·麦卡锡（John McCarthy）在 Lisp（1959, 1981）上的开创性工作之后，Lambda演算在编程语言特性的规范、语言设计和实现以及类型系统的研究中得到了广泛应用。它的重要性在于它可以同时被视为一个简单的编程语言，在其中可以描述计算过程，也可以作为一个关于其可以证明严格陈述的数学对象。

Lambda演算只是用于类似目的的大量核心演算之一。Milner、Parrow 和 Walker（1992, 1991）的**Pi演算 (pi-calculus)** 已成为定义基于消息的并发语言语义的流行核心语言，而 Abadi 和 Cardelli（1996）的**对象演算 (object calculus)** 则提炼了面向对象语言的核心特征。本书将在第19章讨论这方面的一个案例研究。

#### 本章示例

本章中的示例是纯粹无类型Lambda演算 $\lambda$（参见图5-3）或扩展了布尔值和算术操作的Lambda演算 $\lambda_{NB}$（3-2）的项。相关的OCaml实现是 `fulluntyped`。

---

### Lambda演算的扩展

Lambda演算可以以多种方式进行丰富化。首先，添加像数字、元组、记录等特性的特殊具体语法通常是方便的，这些特性的行为已经可以在核心语言中模拟。更有趣的是，我们可以添加更复杂的特性，例如可变的引用单元（mutable reference cells）或非局部异常处理（nonlocal exception handling），这些只能通过相当繁重的翻译在核心语言中建模。这种扩展最终导致了诸如 ML（Gordon, Milner, and Wadsworth, 1979; Milner, Tofte, and Harper, 1990; Weis et al., 1989; Milner et al., 1997）、Haskell（Hudak et al., 1992）或 Scheme（Sussman and Steele, 1975; Kelsey, Clinger, and Rees, 1998）等语言。正如我们将在后续章节中看到的，对核心语言的扩展通常也涉及对类型系统的扩展。

---

### 5.1 基础 (Basics)

**过程（或函数）抽象 (Procedural (or functional) abstraction)** 是几乎所有编程语言的关键特性。为了避免重复编写相同的计算，我们编写一个以一个或多个命名参数进行通用计算的过程或函数，然后在需要时实例化该函数，为每个案例提供参数值。例如，对于程序员来说，将像：

$$(5 \times 4 \times 3 \times 2 \times 1) + (7 \times 6 \times 5 \times 4 \times 3 \times 2 \times 1) - (3 \times 2 \times 1)$$

这样的长且重复的表达式重写为：

$$\text{factorial}(5) + \text{factorial}(7) - \text{factorial}(3)$$

是再自然不过的，其中：

$$\text{factorial}(n) = \text{if } n=0 \text{ then } 1 \text{ else } n \times \text{factorial}(n-1).$$

对于每个非负整数 $n$，用参数 $n$ 实例化函数 $\text{factorial}$ 会得到 $n$ 的阶乘作为结果。如果我们写作“$\lambda n.\ \ldots$”作为“对于每个 $n$，返回……的函数”的简写，我们可以将 $\text{factorial}$ 的定义重述为：

$$\text{factorial} = \lambda n.\ \text{if } n=0 \text{ then } 1 \text{ else } n \times \text{factorial}(n-1)$$

然后，$\text{factorial}(0)$ 表示“将函数 $\lambda n.\ \text{if } n=0 \text{ then } 1 \text{ else } \ldots$ 应用于参数 $0$”，也就是“当函数体中的参数变量 $n$ 被替换为 $0$ 时的值”，即“$\text{if } 0=0 \text{ then } 1 \text{ else } \ldots$”，即 $1$。

**Lambda演算 (lambda-calculus)** 以最纯粹的形式体现了这种函数定义和应用。在Lambda演算中，一切都是函数：函数接受的参数本身就是函数，函数返回的结果也是另一个函数。

---

#### Lambda演算的语法

Lambda演算的语法仅包含三类项（terms）：

1. **变量 (variable)**：$x$ 本身是一个项。
2. **抽象 (abstraction)**：从项 $t_1$ 中抽象出变量 $x$，记作 $\lambda x.t_1$，是一个项。
3. **应用 (application)**：将项 $t_1$ 应用于另一个项 $t_2$，记作 $t_1\ t_2$，是一个项。

这些形成项的方式总结如下的语法：

$$
\begin{align*}
t ::= \quad & \text{terms:} \\
& x \quad \quad \quad \quad \quad \quad \quad \text{变量 (variable)} \\
& \lambda x.t \quad \quad \quad \quad \text{抽象 (abstraction)} \\
& t\ t \quad \quad \quad \quad \quad \text{应用 (application)}
\end{align*}
$$

---

接下来的小节将探讨这个定义中的一些细节。

#### 抽象语法和具体语法 (Abstract and Concrete Syntax)

在讨论编程语言的语法时，区分两个层次的结构是有用的：

1. **具体语法 (concrete syntax)**：也称为**表面语法 (surface syntax)**，指程序员直接阅读和编写的字符序列。
2. **抽象语法 (abstract syntax)**：是一种更简单的程序内部表示，使用标记的树（称为**抽象语法树 (abstract syntax trees, ASTs)**）。树形表示使项的结构立即显现，适合于语言定义的复杂操作以及编译器和解释器的内部处理。

**从具体语法到抽象语法的转换**发生在两个阶段：

- **词法分析器 (lexer)**：首先，将程序员编写的字符序列转换为一系列**词法单元 (tokens)**——标识符、关键词、常量、标点符号等。词法分析器移除注释并处理空白、大小写约定以及数字和字符串常量的格式等问题。
- **语法分析器 (parser)**：接着，将词法单元序列转换为抽象语法树。在解析过程中，各种约定（如运算符的优先级和结合性）减少了在表面程序中使用括号来显式指示复合表达式结构的需要。

例如，乘法运算符 `*` 的优先级高于加法运算符 `+`，因此解析器将未加括号的表达式 `1+2*3` 解释为左边的抽象语法树，而非右边的：

左边的树：

```
    +
   / \
  1   *
     / \
    2   3
```

右边的树：

```
    *
   / \
  +   3
 / \
1   2
```

**本书的重点是抽象语法，而非具体语法**。像上面 Lambda 项的语法应该被理解为描述合法的树结构，而不是词法单元或字符的字符串。当然，当我们在示例、定义、定理和证明中编写项时，我们需要以具体的线性符号表示它们，但我们始终将其底层的抽象语法树牢记在心。

---

#### Lambda项的书写约定

为了避免写过多的括号，我们在以线性形式编写 Lambda 项时采用两个约定：

1. **应用结合性**：应用是左结合的——也就是说，$s\ t\ u$ 表示与 $(s\ t)\ u$ 相同的树：

```
    apply
   /     \
  apply   u
 /     \
s       t
```

2. **抽象体的范围**：抽象的主体尽可能向右延伸，例如，$\lambda x.\ \lambda y.\ x\ y\ x$ 表示与 $\lambda x.\ (\lambda y.\ ((x\ y)\ x))$ 相同的树：

```
    λx
     |
    λy
     |
   apply
  /     \
 apply    x
/     \
x       y
```

---

#### 变量和元变量 (Variables and Metavariables)

语法定义中的另一个细节是元变量的使用。我们将继续使用元变量 $t$（以及 $s$、$u$，有或没有下标）表示任意项。同样地，$x$（以及 $y$、$z$）表示任意变量。

注意，这里 $x$ 是一个**元变量 (metavariable)**，范围是变量！更糟糕的是，可用的短名称有限，我们还希望使用 $x$、$y$ 等作为对象语言的变量。但在这种情况下，语境总是能明确哪个是哪个。

例如，在以下句子中：“项 $\lambda x.\ \lambda y.\ x\ y$ 具有形式 $\lambda z.s$，其中 $z = x$ 且 $s = \lambda y.\ x\ y$”，名称 $z$ 和 $s$ 是元变量，而 $x$ 和 $y$ 是对象语言的变量。

---

#### 作用域 (Scope)

关于 Lambda 演算语法的最后一个问题是变量的作用域。

当变量 $x$ 出现在抽象 $\lambda x.t$ 的主体 $t$ 中时，这个 $x$ 的出现被称为**绑定的 (bound)**。（更精确地说，它被该抽象绑定。等价地，我们可以说 $\lambda x$ 是一个**绑定者 (binder)**，其作用域是 $t$。）

如果 $x$ 的出现不在对 $x$ 的封闭抽象中绑定，那么它是**自由的 (free)**。例如，在 $x\ y$ 和 $\lambda y.\ x\ y$ 中的 $x$ 的出现是自由的，而在 $\lambda x.x$ 和 $\lambda z.\ \lambda x.\ \lambda y.\ x\ (y\ z)$ 中的 $x$ 的出现是绑定的。在 $(\lambda x.x)\ x$ 中，第一个 $x$ 的出现是绑定的，第二个是自由的。

一个没有自由变量的项称为**闭合的 (closed)**；闭合的项也被称为**组合子 (combinators)**。最简单的组合子，称为**恒等函数 (identity function)**，如下：

$$
\text{id} = \lambda x.x;
$$

它只是返回其参数。

---

#### 操作语义 (Operational Semantics)

在其纯粹形式中，Lambda 演算没有内置的常量或原始操作符——没有数字、算术操作、条件、记录、循环、序列化、I/O 等。项“计算”的唯一手段是函数对参数的应用（参数本身也是函数）。计算中的每一步都包括重写一个左部组件为抽象的应用，将抽象主体中的绑定变量替换为右部组件。形式化地，我们写作：

$$
(\lambda x.\ t_{12})\ t_2 \longrightarrow [x \mapsto t_2]t_{12},
$$

其中 $[x \mapsto t_2]t_{12}$ 表示“通过将 $t_{12}$ 中的所有自由出现的 $x$ 替换为 $t_2$ 而获得的项”。例如，项 $(\lambda x.x)\ y$ 计算为 $y$，项 $(\lambda x.\ x\ (\lambda x.x))\ (u\ r)$ 计算为 $u\ r\ (\lambda x.x)$。按照丘奇（Church）的说法，形式为 $(\lambda x.\ t_{12})\ t_2$ 的项称为**可约表达式 (reducible expression)**，简称**红ex (redex)**，根据上述规则重写 redex 的操作称为**beta-约简 (beta-reduction)**。

---

#### 求值策略 (Evaluation Strategies)

多年来，编程语言设计者和理论家研究了 Lambda 演算的几种不同的求值策略。每种策略定义了在下一步求值中可以执行哪些 redex（或 redexes）。

1. **完全 beta-约简 (Full beta-reduction)**：任何 redex 都可以在任何时候被约简。在每一步中，我们选择正在求值的项内的某个 redex，并约简它。

   例如，考虑项：

   $$
   (\lambda x.x)\ ((\lambda x.x)\ (\lambda z.\ (\lambda x.x)\ z)),
   $$

   我们可以更易读地写成 $\text{id}\ (\text{id}\ (\lambda z.\ \text{id}\ z))$。

   这个项包含三个 redex：

   - 外层的 $\text{id}\ (\ldots)$
   - 内层的 $\text{id}\ (\lambda z.\ \text{id}\ z)$
   - 最内层的 $\text{id}\ z$

   在完全 beta-约简下，我们可以例如选择从最内层的 redex 开始，然后是中间的，然后是最外层的：

   $$
   \begin{align*}
   & \text{id}\ (\text{id}\ (\lambda z.\ \text{id}\ z)) \\
   \longrightarrow\ & \text{id}\ (\text{id}\ (\lambda z.\ z)) \\
   \longrightarrow\ & \text{id}\ (\lambda z.\ z) \\
   \longrightarrow\ & \lambda z.\ z \\
   \not\longrightarrow\ &
   \end{align*}
   $$

2. **正常序策略 (Normal Order Strategy)**：总是首先约简最左边、最外层的 redex。在这种策略下，上述项将被约简如下：

   $$
   \begin{align*}
   & \text{id}\ (\text{id}\ (\lambda z.\ \text{id}\ z)) \\
   \longrightarrow\ & \text{id}\ (\lambda z.\ \text{id}\ z) \\
   \longrightarrow\ & \lambda z.\ \text{id}\ z \\
   \longrightarrow\ & \lambda z.\ z \\
   \not\longrightarrow\ &
   \end{align*}
   $$

   在这种策略下（以及下面的策略），求值关系实际上是一个**偏函数 (partial function)**：每个项 $t$ 在一步中至多求值到一个项 $t'$。

3. **按名调用策略 (Call by Name Strategy)**：更为严格，不允许在抽象内部进行约简。从相同的项开始，我们将执行前两次约简，与正常序相同，但在最后一步之前停止，并将 $\lambda z.\ \text{id}\ z$ 视为一个**正常形式 (normal form)**：

   $$
   \begin{align*}
   & \text{id}\ (\text{id}\ (\lambda z.\ \text{id}\ z)) \\
   \longrightarrow\ & \text{id}\ (\lambda z.\ \text{id}\ z) \\
   \longrightarrow\ & \lambda z.\ \text{id}\ z \\
   \not\longrightarrow\ &
   \end{align*}
   $$

   按名调用的变体已在一些知名的编程语言中使用，特别是 Algol-60（Naur et al., 1963）和 Haskell（Hudak et al., 1992）。Haskell 实际上使用了一种称为**按需调用 (call by need)** 的优化版本（Wadsworth, 1971; Ariola et al., 1995），它不会每次使用时重新求值参数，而是在第一次求值时将参数的所有出现都覆盖为其值，避免了随后的重新求值。这种策略要求我们在运行时表示项时维护某种共享——实际上，它是对抽象语法**图 (graphs)** 而不是语法树的约简关系。

4. **按值调用策略 (Call by Value Strategy)**：只约简最外层的 redex，并且仅当其右侧已经约简为一个**值 (value)** 时才约简 redex——即一个已经完成计算、不能再进一步约简的项。在这种策略下，我们的示例项约简如下：

   $$
   \begin{align*}
   & \text{id}\ (\text{id}\ (\lambda z.\ \text{id}\ z)) \\
   \longrightarrow\ & \text{id}\ (\lambda z.\ \text{id}\ z) \\
   \longrightarrow\ & \lambda z.\ \text{id}\ z \\
   \not\longrightarrow\ &
   \end{align*}
   $$

   按值调用策略是**严格的 (strict)**，因为函数的参数总是被求值的，无论函数体是否使用它们。相比之下，非严格（或惰性）的策略（如按名调用和按需调用）只求值实际使用的参数。

---

#### 求值策略对类型系统的影响

当讨论类型系统时，求值策略的选择实际上影响不大。各种类型特性的动机，以及用于解决它们的技术，对于所有策略都是大致相同的。在本书中，我们使用**按值调用 (call by value)**，因为它存在于大多数知名语言中，并且最容易与异常（第14章）和引用（第13章）等特性一起丰富。

---

### 小结

- **Lambda演算**是研究编程语言和类型系统的重要工具，它以最纯粹的形式表达了函数定义和应用的概念。
- **语法**：Lambda演算的语法非常简洁，只包含变量、抽象和应用三种形式。
- **抽象语法和具体语法**：我们主要关注抽象语法，即程序的抽象语法树表示，以便更清晰地理解和操作程序结构。
- **变量作用域**：理解自由变量和绑定变量的概念对于正确处理变量替换和避免变量捕获非常重要。
- **操作语义**：通过 beta-约简规则，我们定义了 Lambda 演算的计算过程，即如何执行函数应用。
- **求值策略**：不同的求值策略（如完全 beta-约简、正常序、按名调用、按值调用）影响了计算的方式和效率。本书采用按值调用策略。

---

希望以上详尽的解释能够帮助您深入理解无类型 Lambda 演算的基础知识和概念。如果您有任何疑问或需要进一步的讲解，请随时提出！

### ---------------------------

### 第5章 无类型Lambda演算 (The Untyped Lambda-Calculus)

#### 5.2 在Lambda演算中编程 (Programming in the Lambda-Calculus)

Lambda演算的威力远超其微小定义所能暗示的。在本节中，我们将开发一些在Lambda演算中编程的标准示例。这些示例并非意在暗示Lambda演算应该被视为一门完整的编程语言——所有广泛使用的高级语言都提供了更清晰和更高效的方式来完成相同的任务——而是旨在作为热身练习，让我们熟悉该系统。

---

#### **多参数函数 (Multiple Arguments)**

首先，注意到Lambda演算不提供对多参数函数的内置支持。当然，添加这项支持并不难，但使用返回函数的高阶函数来实现相同的效果更容易。

假设 $s$ 是一个包含两个自由变量 $x$ 和 $y$ 的项，我们想要编写一个函数 $f$，对于每一对参数 $(v, w)$，都能将 $s$ 中的 $x$ 替换为 $v$，$y$ 替换为 $w$，并得到结果。

在更丰富的编程语言中，我们可能会写 $f = \lambda (x, y). s$。但在Lambda演算中，我们写作：

$$
f = \lambda x.\ \lambda y.\ s
$$

也就是说，$f$ 是一个函数，给定 $x$ 的值 $v$，返回一个函数，该函数给定 $y$ 的值 $w$，返回期望的结果。

然后，我们一次对 $f$ 应用一个参数，写作 $f\ v\ w$（即 $(f\ v)\ w$）。这将约简为：

$$
((\lambda y.\ [x \mapsto v]s)\ w)
$$

然后进一步约简为：

$$
[y \mapsto w][x \mapsto v]s
$$

将多参数函数转换为返回函数的高阶函数的过程称为**柯里化 (currying)**，以纪念 Haskell Curry，他是丘奇的同时代人。

---

#### **丘奇布尔值 (Church Booleans)**

Lambda演算中可以轻松地编码布尔值和条件表达式。定义项 $\text{tru}$ 和 $\text{fls}$ 如下：

$$
\text{tru} = \lambda t.\ \lambda f.\ t;
$$

$$
\text{fls} = \lambda t.\ \lambda f.\ f;
$$

（这些名称的缩写旨在帮助避免与第3章中的原始布尔常量 $\text{true}$ 和 $\text{false}$ 混淆。）

我们可以将项 $\text{tru}$ 和 $\text{fls}$ 视为表示布尔值“真 (true)”和“假 (false)”，因为我们可以使用这些项来执行布尔值的真值测试操作。特别是，我们可以使用应用来定义一个组合子 $\text{test}$，其属性是：当 $b$ 是 $\text{tru}$ 时，$\text{test}\ b\ v\ w$ 约简为 $v$；当 $b$ 是 $\text{fls}$ 时，约简为 $w$。

定义 $\text{test}$：

$$
\text{test} = \lambda l.\ \lambda m.\ \lambda n.\ l\ m\ n;
$$

实际上，$\text{test}$ 组合子并没有做很多事情：$\text{test}\ b\ v\ w$ 只是约简为 $b\ v\ w$。实际上，布尔值 $b$ 本身就是条件：它接受两个参数并选择第一个（如果它是 $\text{tru}$）或第二个（如果它是 $\text{fls}$）。

例如，项 $\text{test}\ \text{tru}\ v\ w$ 约简如下：

1. 首先，展开 $\text{test}$ 的定义：

   $$
   \text{test}\ \text{tru}\ v\ w = (\lambda l.\ \lambda m.\ \lambda n.\ l\ m\ n)\ \text{tru}\ v\ w
   $$

2. 进行一次 beta-约简（对下划线部分）：

   $$
   \underline{(\lambda l.\ \lambda m.\ \lambda n.\ l\ m\ n)\ \text{tru}}\ v\ w \longrightarrow (\lambda m.\ \lambda n.\ \text{tru}\ m\ n)\ v\ w
   $$

3. 再次 beta-约简：

   $$
   \underline{(\lambda m.\ \lambda n.\ \text{tru}\ m\ n)\ v}\ w \longrightarrow (\lambda n.\ \text{tru}\ v\ n)\ w
   $$

4. 再次 beta-约简：

   $$
   \underline{(\lambda n.\ \text{tru}\ v\ n)\ w} \longrightarrow \text{tru}\ v\ w
   $$

5. 展开 $\text{tru}$ 的定义：

   $$
   \text{tru}\ v\ w = (\lambda t.\ \lambda f.\ t)\ v\ w
   $$

6. 进行 beta-约简：

   $$
   \underline{(\lambda t.\ \lambda f.\ t)\ v}\ w \longrightarrow (\lambda f.\ v)\ w
   $$

7. 最后一次 beta-约简：

   $$
   \underline{(\lambda f.\ v)\ w} \longrightarrow v
   $$

因此，当 $b$ 是 $\text{tru}$ 时，$\text{test}\ b\ v\ w$ 约简为 $v$。

---

我们还可以定义逻辑运算符，如逻辑与（and）作为函数：

$$
\text{and} = \lambda b.\ \lambda c.\ b\ c\ \text{fls};
$$

也就是说，$\text{and}$ 是一个函数，给定两个布尔值 $b$ 和 $c$，如果 $b$ 是 $\text{tru}$，则返回 $c$；如果 $b$ 是 $\text{fls}$，则返回 $\text{fls}$。因此，当 $b$ 和 $c$ 都是 $\text{tru}$ 时，$\text{and}\ b\ c$ 得到 $\text{tru}$；如果 $b$ 或 $c$ 中有一个是 $\text{fls}$，则得到 $\text{fls}$。

验证：

- $\text{and}\ \text{tru}\ \text{tru}$ 约简为：

  $$
  \text{and}\ \text{tru}\ \text{tru} = (\lambda b.\ \lambda c.\ b\ c\ \text{fls})\ \text{tru}\ \text{tru}
  $$

  首先 beta-约简：

  $$
  \underline{(\lambda b.\ \lambda c.\ b\ c\ \text{fls})\ \text{tru}}\ \text{tru} \longrightarrow (\lambda c.\ \text{tru}\ c\ \text{fls})\ \text{tru}
  $$

  再次 beta-约简：

  $$
  \underline{(\lambda c.\ \text{tru}\ c\ \text{fls})\ \text{tru}} \longrightarrow \text{tru}\ \text{tru}\ \text{fls}
  $$

  展开 $\text{tru}$：

  $$
  \text{tru}\ \text{tru}\ \text{fls} = (\lambda t.\ \lambda f.\ t)\ \text{tru}\ \text{fls}
  $$

  Beta-约简：

  $$
  \underline{(\lambda t.\ \lambda f.\ t)\ \text{tru}}\ \text{fls} \longrightarrow (\lambda f.\ \text{tru})\ \text{fls}
  $$

  最后：

  $$
  \underline{(\lambda f.\ \text{tru})\ \text{fls}} \longrightarrow \text{tru}
  $$

- $\text{and}\ \text{tru}\ \text{fls}$ 约简为：

  $$
  \text{and}\ \text{tru}\ \text{fls} \longrightarrow \text{fls}
  $$

---

#### **练习 5.2.1 [«]：定义逻辑或（or）和非（not）函数。**

**解答：**

- **逻辑或（or）函数：**

  我们可以定义逻辑或函数 $\text{or}$，使得当 $b$ 或 $c$ 为 $\text{tru}$ 时，$\text{or}\ b\ c$ 返回 $\text{tru}$；只有当 $b$ 和 $c$ 都为 $\text{fls}$ 时，才返回 $\text{fls}$。

  定义：

  $$
  \text{or} = \lambda b.\ \lambda c.\ b\ \text{tru}\ c;
  $$

  解释：如果 $b$ 是 $\text{tru}$，则返回 $\text{tru}$；否则返回 $c$ 的值。

- **逻辑非（not）函数：**

  定义：

  $$
  \text{not} = \lambda b.\ b\ \text{fls}\ \text{tru};
  $$

  解释：如果 $b$ 是 $\text{tru}$，则返回 $\text{fls}$；如果 $b$ 是 $\text{fls}$，则返回 $\text{tru}$。

**验证：**

- 验证 $\text{not}\ \text{tru}$：

  $$
  \text{not}\ \text{tru} = (\lambda b.\ b\ \text{fls}\ \text{tru})\ \text{tru} \longrightarrow \text{tru}\ \text{fls}\ \text{tru}
  $$

  展开 $\text{tru}$：

  $$
  \text{tru}\ \text{fls}\ \text{tru} = (\lambda t.\ \lambda f.\ t)\ \text{fls}\ \text{tru} \longrightarrow (\lambda f.\ \text{fls})\ \text{tru} \longrightarrow \text{fls}
  $$

- 验证 $\text{not}\ \text{fls}$：

  $$
  \text{not}\ \text{fls} = (\lambda b.\ b\ \text{fls}\ \text{tru})\ \text{fls} \longrightarrow \text{fls}\ \text{fls}\ \text{tru}
  $$

  展开 $\text{fls}$：

  $$
  \text{fls}\ \text{fls}\ \text{tru} = (\lambda t.\ \lambda f.\ f)\ \text{fls}\ \text{tru} \longrightarrow (\lambda f.\ f)\ \text{tru} \longrightarrow \text{tru}
  $$

---

#### **对的表示 (Pairs)**

使用布尔值，我们可以将值的对（pair）编码为项。

定义：

$$
\text{pair} = \lambda f.\ \lambda s.\ \lambda b.\ b\ f\ s;
$$

$$
\text{fst} = \lambda p.\ p\ \text{tru};
$$

$$
\text{snd} = \lambda p.\ p\ \text{fls};
$$

也就是说，$\text{pair}\ v\ w$ 是一个函数，当应用于布尔值 $b$ 时，返回 $b\ v\ w$。根据布尔值的定义，如果 $b$ 是 $\text{tru}$，则此应用返回 $v$；如果 $b$ 是 $\text{fls}$，则返回 $w$。因此，第一和第二投影函数 $\text{fst}$ 和 $\text{snd}$ 可以简单地通过提供适当的布尔值来实现。

验证 $\text{fst}\ (\text{pair}\ v\ w) \longrightarrow^\ast v$：

1. 展开 $\text{pair}$ 的定义：

   $$
   \text{pair}\ v\ w = (\lambda f.\ \lambda s.\ \lambda b.\ b\ f\ s)\ v\ w
   $$

2. 进行两次 beta-约简：

   $$
   \underline{(\lambda f.\ \lambda s.\ \lambda b.\ b\ f\ s)\ v}\ w \longrightarrow (\lambda s.\ \lambda b.\ b\ v\ s)\ w \longrightarrow (\lambda b.\ b\ v\ w)
   $$

3. 展开 $\text{fst}$ 的定义：

   $$
   \text{fst}\ (\lambda b.\ b\ v\ w) = (\lambda p.\ p\ \text{tru})\ (\lambda b.\ b\ v\ w)
   $$

4. Beta-约简：

   $$
   \underline{(\lambda p.\ p\ \text{tru})\ (\lambda b.\ b\ v\ w)} \longrightarrow (\lambda b.\ b\ v\ w)\ \text{tru}
   $$

5. 应用 $\text{tru}$：

   $$
   \underline{(\lambda b.\ b\ v\ w)\ \text{tru}} \longrightarrow \text{tru}\ v\ w
   $$

6. 展开 $\text{tru}$：

   $$
   \text{tru}\ v\ w = (\lambda t.\ \lambda f.\ t)\ v\ w
   $$

7. Beta-约简：

   $$
   \underline{(\lambda t.\ \lambda f.\ t)\ v}\ w \longrightarrow (\lambda f.\ v)\ w \longrightarrow v
   $$

因此，$\text{fst}\ (\text{pair}\ v\ w) \longrightarrow^\ast v$。

---

#### **丘奇数 (Church Numerals)**

用Lambda项表示数字比我们刚才看到的稍微复杂一点。定义丘奇数 $c_0$，$c_1$，$c_2$，等等，如下：

$$
\begin{align*}
c_0 &= \lambda s.\ \lambda z.\ z; \\
c_1 &= \lambda s.\ \lambda z.\ s\ z; \\
c_2 &= \lambda s.\ \lambda z.\ s\ (s\ z); \\
c_3 &= \lambda s.\ \lambda z.\ s\ (s\ (s\ z)); \\
&\vdots
\end{align*}
$$

也就是说，每个数字 $n$ 表示为一个组合子 $c_n$，它接受两个参数 $s$ 和 $z$（代表“后继 (successor)”和“零 (zero)”），并将 $s$ 应用于 $z$ 共 $n$ 次。

像布尔值和对的编码一样，这种编码使得数字成为**主动实体 (active entities)**：数字 $n$ 被表示为一个执行某事 $n$ 次的函数——一种主动的一元数表示法。

（读者可能已经注意到 $c_0$ 和 $\text{fls}$ 实际上是相同的项。在汇编语言中类似的“谐音”很常见，在汇编语言中，相同的位模式可以表示许多不同的值——一个整数、一个浮点数、一个地址、四个字符等——取决于如何解释它。在诸如 C 这样的低级语言中，也将 $0$ 和 $\text{false}$ 视为相同。）

---

我们可以如下定义丘奇数的后继函数：

$$
\text{scc} = \lambda n.\ \lambda s.\ \lambda z.\ s\ (n\ s\ z);
$$

项 $\text{scc}$ 是一个组合子，接受一个丘奇数 $n$，并返回另一个丘奇数——也就是说，它返回一个函数，该函数接受参数 $s$ 和 $z$，并多次将 $s$ 应用于 $z$。我们通过首先将 $s$ 和 $z$ 作为参数传递给 $n$，然后再对结果显式地再应用一次 $s$，以得到正确次数的 $s$ 应用于 $z$。

---

#### **总结**

在本节中，我们展示了如何在Lambda演算中表示多参数函数、布尔值、逻辑运算、对以及数字。这些示例展示了Lambda演算的强大表达能力，即使它的语法和规则极其简洁。

通过这些编码，我们可以看到Lambda演算如何作为一种“通用计算模型”来模拟各种编程语言的特性。这为研究编程语言的理论基础提供了坚实的工具。

---

**如果您对上述内容有任何疑问或需要进一步的解释，请随时提出！**

### ----------------------------

### 5.2 在 Lambda 演算中编程 (Programming in the Lambda-Calculus)

#### 练习 5.2.2 [««]: 找到另一种方式来定义丘奇数的后继函数。

**解答：**

在之前的定义中，我们将后继函数 $\text{scc}$ 定义为：

$$
\text{scc} = \lambda n.\ \lambda s.\ \lambda z.\ s\ (n\ s\ z)
$$

这是通过在已有的数字 $n$ 应用 $s$ 和 $z$ 后，再额外应用一次 $s$。现在，我们尝试找到另一种方式来定义 $\text{scc}$。

**新定义：**

一种替代的定义方式是：

$$
\text{scc} = \lambda n.\ \lambda s.\ \lambda z.\ n\ s\ (s\ z)
$$

**解释：**

- 我们将 $n$ 应用于 $s$ 和 $s\ z$，即在初始值上先应用一次 $s$。
- 这实际上是在数字 $n$ 的基础上加 $1$，因为 $n$ 将 $s$ 应用于 $s\ z$ 共 $n$ 次，总共是 $n + 1$ 次。

**验证：**

以 $c_1$（即数字 $1$）为例，验证新定义的 $\text{scc}$ 是否正确。

- 首先，$c_1$ 定义为：

  $$
  c_1 = \lambda s.\ \lambda z.\ s\ z
  $$

- 计算 $\text{scc}\ c_1$：

  $$
  \begin{align*}
  \text{scc}\ c_1 & = (\lambda n.\ \lambda s.\ \lambda z.\ n\ s\ (s\ z))\ c_1 \\
  & = \lambda s.\ \lambda z.\ c_1\ s\ (s\ z) \\
  & = \lambda s.\ \lambda z.\ (\lambda s'.\ \lambda z'.\ s'\ z')\ s\ (s\ z) \\
  & = \lambda s.\ \lambda z.\ s\ (s\ z)
  \end{align*}
  $$

- 这正是 $c_2$ 的定义，即数字 $2$：

  $$
  c_2 = \lambda s.\ \lambda z.\ s\ (s\ z)
  $$

因此，这种新的定义方式也是正确的。

---

#### 练习 5.2.3 [««]: 是否可以在不使用加法函数 $\text{plus}$ 的情况下定义丘奇数的乘法？

**解答：**

是的，可以在不使用加法函数的情况下定义乘法。我们可以直接根据丘奇数的定义来构建乘法函数。

**定义：**

乘法函数 $\text{times}$ 可以定义为：

$$
\text{times} = \lambda m.\ \lambda n.\ \lambda s.\ \lambda z.\ m\ (n\ s)\ z
$$

**解释：**

- $\text{times}$ 接受两个丘奇数 $m$ 和 $n$，以及函数 $s$ 和初始值 $z$。
- 首先，将 $n\ s$ 作为函数传递给 $m$，即在 $m$ 中，将 $s$ 替换为 $n\ s$。
- 结果是，将 $s$ 应用于 $z$ 共 $m \times n$ 次。

**验证：**

以 $c_2$ 和 $c_3$ 为例，计算 $\text{times}\ c_2\ c_3$：

1. $c_2 = \lambda s.\ \lambda z.\ s\ (s\ z)$
2. $c_3 = \lambda s.\ \lambda z.\ s\ (s\ (s\ z))$

计算 $\text{times}\ c_2\ c_3$：

$$
\begin{align*}
\text{times}\ c_2\ c_3 & = (\lambda m.\ \lambda n.\ \lambda s.\ \lambda z.\ m\ (n\ s)\ z)\ c_2\ c_3 \\
& = \lambda s.\ \lambda z.\ c_2\ (c_3\ s)\ z \\
& = \lambda s.\ \lambda z.\ (\lambda s'.\ \lambda z'.\ s'\ (s'\ z'))\ (c_3\ s)\ z \\
& = \lambda s.\ \lambda z.\ (c_3\ s)\ ((c_3\ s)\ z)
\end{align*}
$$

- 继续展开 $c_3\ s$：

  $$
  c_3\ s = \lambda z'.\ s\ (s\ (s\ z'))
  $$

- 将其代入上式，得到：

  $$
  \lambda s.\ \lambda z.\ (\lambda z'.\ s\ (s\ (s\ z')))\ ((\lambda z'.\ s\ (s\ (s\ z')))\ z)
  $$

- 进一步计算，可以验证最终结果是将 $s$ 应用于 $z$ 共 $2 \times 3 = 6$ 次，即 $c_6$。

---

#### 练习 5.2.4 [推荐, ««]: 定义一个项来表示数的幂运算。

**解答：**

我们需要定义一个函数 $\text{power}$，使得对于两个丘奇数 $m$ 和 $n$，$\text{power}\ m\ n$ 表示 $m$ 的 $n$ 次幂。

**定义：**

$$
\text{power} = \lambda m.\ \lambda n.\ n\ m
$$

**解释：**

- 在 Lambda 演算中，函数的重复应用可以用于表示幂运算。
- $n\ m$ 表示将函数 $m$ 应用于某个参数共 $n$ 次。
- 因此，$\text{power}\ m\ n$ 实际上是一个函数，接受参数 $s$ 和 $z$，并将 $m$ 应用于 $s$ 和 $z$，共 $n$ 次。

**验证：**

以 $c_2$ 和 $c_3$ 为例，计算 $\text{power}\ c_2\ c_3$，即计算 $2^3$：

$$
\begin{align*}
\text{power}\ c_2\ c_3 & = (\lambda m.\ \lambda n.\ n\ m)\ c_2\ c_3 \\
& = c_3\ c_2 \\
& = (\lambda s.\ \lambda z.\ s\ (s\ (s\ z)))\ c_2 \\
& = \lambda z.\ c_2\ (c_2\ (c_2\ z))
\end{align*}
$$

- 由于 $c_2$ 将函数应用两次，因此 $c_2\ z$ 将函数应用于 $z$ 两次。
- 最终结果是将函数应用于 $z$ 共 $2^3 = 8$ 次，即 $c_8$。

---

#### 练习 5.2.5 [««]: 使用 $\text{prd}$ 来定义一个减法函数。

**解答：**

首先，回顾前驱函数（前一节中提到的 $\text{prd}$）的定义：

$$
\begin{align*}
\text{zz} & = \text{pair}\ c_0\ c_0; \\
\text{ss} & = \lambda p.\ \text{pair}\ (\text{snd}\ p)\ (\text{plus}\ c_1\ (\text{snd}\ p)); \\
\text{prd} & = \lambda m.\ \text{fst}\ (m\ \text{ss}\ \text{zz});
\end{align*}
$$

**定义减法函数 $\text{sub}$：**

$$
\text{sub} = \lambda m.\ \lambda n.\ n\ \text{prd}\ m
$$

**解释：**

- 减法 $\text{sub}\ m\ n$ 表示计算 $m - n$。
- 我们通过对 $m$ 应用 $n$ 次前驱函数 $\text{prd}$，即从 $m$ 开始，连续减去 $1$，共 $n$ 次。

**验证：**

以 $c_5$ 和 $c_2$ 为例，计算 $\text{sub}\ c_5\ c_2$：

$$
\begin{align*}
\text{sub}\ c_5\ c_2 & = (\lambda m.\ \lambda n.\ n\ \text{prd}\ m)\ c_5\ c_2 \\
& = c_2\ \text{prd}\ c_5 \\
& = \text{prd}\ (\text{prd}\ c_5)
\end{align*}
$$

- 计算 $\text{prd}\ c_5$，得到 $c_4$。
- 再次计算 $\text{prd}\ c_4$，得到 $c_3$。
- 因此，$\text{sub}\ c_5\ c_2 = c_3$，即 $5 - 2 = 3$。

---

#### 练习 5.2.6 [««]: 计算 $\text{prd}\ c_n$ 需要大约多少步的求值（作为 $n$ 的函数）？

**解答：**

在 $\text{prd}$ 的定义中，我们有：

$$
\text{prd}\ =\ \lambda m.\ \text{fst}\ (m\ \text{ss}\ \text{zz})
$$

- 其中，$\text{ss}$ 和 $\text{zz}$ 为常数。
- 应用 $m$ 次 $\text{ss}$，即对 $\text{zz}$ 进行 $m$ 次迭代。

每次应用 $\text{ss}$，内部需要进行固定数量的步骤，包括调用 $\text{plus}$。

**近似步骤数：**

- **应用 $\text{ss}$ 的次数**：$m$ 次，即 $n$ 次。
- **每次应用 $\text{ss}$ 的成本**：固定的步骤数，假设为常数 $k$。
- **总步骤数**：$O(n \times k) = O(n)$。

因此，计算 $\text{prd}\ c_n$ 需要大约 $O(n)$ 步。

---

#### 练习 5.2.7 [««]: 编写一个函数 $\text{equal}$，用于测试两个数字是否相等，并返回一个丘奇布尔值。

**解答：**

**定义：**

我们可以使用递归的方法，比较两个数的大小。

$$
\text{equal} = \lambda m.\ \lambda n.\ \text{and}\ (\text{iszro}\ (\text{sub}\ m\ n))\ (\text{iszro}\ (\text{sub}\ n\ m))
$$

**解释：**

- $\text{sub}\ m\ n$ 计算 $m - n$，如果 $m \geq n$，则结果为非负数，否则为 $0$。
- $\text{iszro}$ 检查结果是否为 $0$。
- 如果 $m - n = 0$ 且 $n - m = 0$，则 $m = n$。

**更简洁的定义：**

考虑到我们已经有了前面的函数，我们可以定义：

$$
\text{equal} = \lambda m.\ \lambda n.\ \text{iszro}\ (\text{sub}\ m\ n)\ \text{and}\ \text{iszro}\ (\text{sub}\ n\ m)
$$

但是这样可能有些冗长。更好的方法是使用以下定义：

$$
\text{equal} = \lambda m.\ \lambda n.\ \text{iszro}\ (\text{sub}\ m\ n\ \text{sub}\ n\ m)
$$

但是由于在纯 Lambda 演算中不支持减法的负数，因此需要更谨慎地处理。

**改进的定义：**

我们可以使用以下方法，通过将两个数字同时递减，检查它们是否同时到达零：

$$
\text{equal} = \lambda m.\ \lambda n.\ m\ (\lambda x.\ n\ (\lambda y.\ \text{fls})\ \text{fls})\ \text{tru}
$$

**解释：**

- 如果 $m$ 和 $n$ 都是零，则返回 $\text{tru}$。
- 如果一个数字先到达零，另一个未到达，则返回 $\text{fls}$。

---

#### 练习 5.2.8 [推荐, «««]: 在 Lambda 演算中，列表可以由其折叠函数表示。例如，列表 $[x, y, z]$ 表示为一个函数，接受两个参数 $c$ 和 $n$，返回 $c\ x\ (c\ y\ (c\ z\ n))$。那么，$\text{nil}$ 的表示是什么？

**解答：**

**定义 $\text{nil}$：**

$$
\text{nil} = \lambda c.\ \lambda n.\ n
$$

**解释：**

- 空列表表示为一个函数，接受参数 $c$ 和 $n$，直接返回 $n$。
- 这与丘奇数 $c_0$ 的定义类似。

**定义 $\text{cons}$ 函数：**

$$
\text{cons} = \lambda h.\ \lambda t.\ \lambda c.\ \lambda n.\ c\ h\ (t\ c\ n)
$$

**解释：**

- $\text{cons}$ 接受一个元素 $h$ 和一个列表 $t$，返回一个新列表。
- 当应用于 $c$ 和 $n$ 时，先对 $h$ 应用 $c$，然后对 $t$ 应用 $c$ 和 $n$。

**定义 $\text{isnil}$ 函数：**

$$
\text{isnil} = \lambda l.\ l\ (\lambda x.\ \lambda y.\ \text{fls})\ \text{tru}
$$

**解释：**

- 对于空列表 $\text{nil}$，应用 $\text{isnil}$ 将返回 $\text{tru}$。
- 对于非空列表，应用 $\text{isnil}$ 将返回 $\text{fls}$。

**定义 $\text{head}$ 函数：**

$$
\text{head} = \lambda l.\ l\ (\lambda h.\ \lambda t.\ h)\ \text{dummy}
$$

- 由于空列表没有头元素，我们需要一个占位符 $\text{dummy}$，或者假设列表非空。

**定义 $\text{tail}$ 函数：**

实现 $\text{tail}$ 较为复杂，需要类似于定义 $\text{prd}$ 的技巧。

**定义：**

$$
\begin{align*}
\text{zz} & = \text{pair}\ \text{nil}\ \text{nil}; \\
\text{ss} & = \lambda p.\ \text{pair}\ (\text{snd}\ p)\ (\text{cons}\ (\text{head}\ (\text{snd}\ p))\ (\text{snd}\ p)); \\
\text{tail} & = \lambda l.\ \text{fst}\ (l\ \text{ss}\ \text{zz});
\end{align*}
$$

**解释：**

- 我们对列表 $l$ 应用 $\text{ss}$，初始值为 $\text{zz}$，迭代构建新的列表。
- 最终得到的 $\text{fst}$ 即为 $l$ 的尾部。

---

### 丰富演算 (Enriching the Calculus)

Lambda 演算虽然强大，但在实际使用中，包含原始的布尔值和数字（以及其他数据类型）通常更方便。在我们的讨论中，我们有两种系统：

1. **纯粹的 Lambda 演算 $\lambda$**：只包含抽象、应用和变量。
2. **扩展的系统 $\lambda_{NB}$**：包含布尔值和算术表达式。

#### 系统之间的转换

在 $\lambda_{NB}$ 中，我们可以在编程时选择使用真实的布尔值和数字，或者使用我们在本章中开发的编码。

**将丘奇布尔值转换为原始布尔值：**

$$
\text{realbool} = \lambda b.\ b\ \text{true}\ \text{false}
$$

- 解释：将丘奇布尔值 $b$ 应用于原始布尔值 $\text{true}$ 和 $\text{false}$，得到对应的原始布尔值。

**将原始布尔值转换为丘奇布尔值：**

$$
\text{churchbool} = \lambda b.\ \text{if}\ b\ \text{then}\ \text{tru}\ \text{else}\ \text{fls}
$$

- 解释：根据原始布尔值 $b$，返回对应的丘奇布尔值 $\text{tru}$ 或 $\text{fls}$。

**示例：**

- 定义一个比较函数 $\text{realeq}$，它对丘奇数进行比较并返回原始布尔值：

  $$
  \text{realeq} = \lambda m.\ \lambda n.\ (\text{equal}\ m\ n)\ \text{true}\ \text{false}
  $$

**数字的转换：**

**将丘奇数转换为原始数字：**

$$
\text{realnat} = \lambda m.\ m\ (\lambda x.\ \text{succ}\ x)\ 0
$$

- 解释：将丘奇数 $m$ 应用于函数 $\lambda x.\ \text{succ}\ x$ 和初始值 $0$，得到对应的原始数字。

**注意：**

- 我们不能直接将 $m$ 应用于 $\text{succ}$，因为 $\text{succ}$ 在我们的语法中必须应用于一个参数。通过将 $\text{succ}$ 包装在一个接受参数 $x$ 并返回 $\text{succ}\ x$ 的函数中，我们解决了这个问题。

#### 求值顺序的影响

由于求值策略的原因，某些项在按值调用（call-by-value）策略下不会完全归约到期望的形式。例如，计算 $\text{scc}\ c_1$：

$$
\text{scc}\ c_1 \longrightarrow \lambda s.\ \lambda z.\ s\ ((\lambda s'.\ \lambda z'.\ s'\ z')\ s\ z)
$$

- 这个项包含一个红ex，但由于它在一个 lambda 抽象下，按值调用策略不允许我们继续约简。

为了解决这个问题，我们可以：

- **使用原始的布尔值和数字**：通过将结果转换为原始数据类型，强制执行剩余的计算。

**示例：**

- 计算 $\text{realnat}\ (\text{times}\ c_2\ c_2)$：

  - $\text{times}\ c_2\ c_2$ 不能直接在按值调用策略下完全归约到 $c_4$。
  - 通过应用 $\text{realnat}$，我们将其转换为原始数字 $4$，完成计算。

---

### 总结

- **Lambda 演算的扩展**：通过添加原始的布尔值和数字，我们可以更方便地编写和求值复杂的表达式。
- **系统间的转换**：可以在编码的布尔值和数字与原始的布尔值和数字之间进行转换。
- **求值策略的影响**：在按值调用策略下，某些项可能无法完全归约，通过转换或修改策略，可以完成计算。

---

希望以上详细的解答和讲解能够帮助您全面理解这些概念。如有任何疑问，请随时提问！

### ---------------------------

### 5.2 在 Lambda 演算中编程 (Programming in the Lambda-Calculus)

#### 练习 5.2.9 [«]: 为什么在定义 \( g \) 时使用了原始的 \( \text{if} \)，而不是对丘奇布尔值的 \( \text{test} \) 函数？展示如何使用 \( \text{test} \) 而不是 \( \text{if} \) 来定义阶乘函数。

**解答：**

**问题分析：**

- **目标**：理解为什么在定义函数 \( g \) 时，我们使用了原始的条件表达式 \( \text{if} \)，而不是基于丘奇布尔值的 \( \text{test} \) 函数。
- **要求**：展示如何使用 \( \text{test} \) 函数来定义阶乘函数。

**原因分析：**

- **使用原始的 \( \text{if} \) 的原因**：

  - **求值顺序**：在按值调用（Call-by-Value）的策略下，使用原始的 \( \text{if} \) 可以确保条件表达式的正确求值，避免不必要的延迟计算。
  - **布尔值类型**：\( \text{realeq} \) 函数返回的是原始布尔值 \( \text{true} \) 和 \( \text{false} \)，因此直接使用原始的 \( \text{if} \) 更为自然。
  - **避免额外的转换**：如果使用 \( \text{test} \) 函数，我们需要将原始布尔值转换为丘奇布尔值，增加了复杂性。

**如何使用 \( \text{test} \) 函数来定义阶乘函数：**

1. **定义返回丘奇布尔值的比较函数**

   - 修改 \( \text{equal} \) 函数，使其返回**丘奇布尔值**而不是原始布尔值。

     $$
     \text{equal} = \lambda m.\ \lambda n.\ \text{iszro}\ (\text{sub}\ m\ n\ \text{plus}\ \text{sub}\ n\ m)
     $$

     - 这里，\( \text{sub} \) 是减法函数，\( \text{iszro} \) 检查一个数是否为零。

2. **使用 \( \text{test} \) 函数**

   - 定义 \( \text{test} \) 函数：

     $$
     \text{test} = \lambda b.\ \lambda m.\ \lambda n.\ b\ m\ n
     $$

     - \( b \) 是丘奇布尔值，\( m \) 和 \( n \) 是待选择的项。

3. **重新定义函数 \( g \)**

   - 使用 \( \text{test} \) 而不是原始的 \( \text{if} \)：

     $$
     g = \lambda \text{fct}.\ \lambda n.\ \text{test}\ (\text{equal}\ n\ c_0)\ c_1\ (\text{times}\ n\ (\text{fct}\ (\text{prd}\ n)))
     $$

     - \( c_0 \) 和 \( c_1 \) 分别是丘奇数 \( 0 \) 和 \( 1 \)。
     - \( \text{times} \) 是乘法函数，\( \text{prd} \) 是前驱函数。

4. **定义阶乘函数**

   - 使用固定点组合子 \( \text{fix} \)：

     $$
     \text{factorial} = \text{fix}\ g
     $$

**验证新定义的阶乘函数：**

- 计算 \( \text{factorial}\ c_3 \)（即计算 \( 3! \)）：

  1. 展开 \( \text{factorial} \) 的定义：

     $$
     \text{factorial}\ c_3 = (\text{fix}\ g)\ c_3
     $$

  2. 按照固定点组合子的展开，递归计算，最终得到正确的阶乘结果。

**总结：**

- 使用 \( \text{test} \) 函数需要确保所有布尔值操作都基于丘奇布尔值。
- 需要修改相关函数（如 \( \text{equal} \)）以返回丘奇布尔值。
- 这样可以在纯粹的 Lambda 演算框架下，避免依赖原始的 \( \text{if} \) 控制结构。

---

#### 练习 5.2.10 [««]: 定义一个函数 \( \text{churchnat} \)，将原始自然数转换为对应的丘奇数。

**解答：**

**目标：**

- 编写一个函数 \( \text{churchnat} \)，接受一个原始自然数 \( n \)，返回对应的丘奇数 \( c_n \)。

**思路：**

- 使用递归的方法，根据 \( n \) 的值，构建对应的丘奇数。
- 当 \( n = 0 \) 时，返回 \( c_0 = \lambda s.\ \lambda z.\ z \)。
- 当 \( n > 0 \) 时，返回 \( \text{scc}\ (\text{churchnat}\ (n - 1)) \)。

**定义 \( \text{churchnat} \) 函数：**

1. **伪代码表示：**

   ```pseudo
   churchnat(n) =
     if n = 0 then
       c0
     else
       scc(churchnat(n - 1))
   ```

2. **Lambda 演算表示：**

   $$
   \text{churchnat} = \lambda n.\ \text{if}\ n = 0\ \text{then}\ c_0\ \text{else}\ \text{scc}\ (\text{churchnat}\ (n - 1))
   $$

3. **实现递归：**

   - 使用固定点组合子 \( \text{fix} \)：

     $$
     \text{churchnat} = \text{fix}\ (\lambda f.\ \lambda n.\ \text{if}\ n = 0\ \text{then}\ c_0\ \text{else}\ \text{scc}\ (f\ (n - 1)))
     $$

     - 其中，\( f \) 是递归函数自身。

**注意事项：**

- 由于纯 Lambda 演算中没有原始的自然数和算术操作，需要假设存在这些基本操作，或在扩展的演算中实现。
- 如果在纯 Lambda 演算中，需要进一步编码自然数的比较和减法操作。

**示例验证：**

- **计算 \( \text{churchnat}(2) \)：**

  1. \( n = 2 \)，不等于 \( 0 \)，进入递归：
     $$
     \text{churchnat}(2) = \text{scc}\ (\text{churchnat}(1))
     $$
  2. \( n = 1 \)，不等于 \( 0 \)，继续递归：
     $$
     \text{churchnat}(1) = \text{scc}\ (\text{churchnat}(0))
     $$
  3. \( n = 0 \)，返回 \( c_0 \)：
     $$
     \text{churchnat}(0) = c_0
     $$
  4. 逐层返回：
     $$
     \text{churchnat}(1) = \text{scc}\ c_0 = c_1
     $$
     $$
     \text{churchnat}(2) = \text{scc}\ c_1 = c_2
     $$

---

#### 练习 5.2.11 [推荐, ««]: 使用 \( \text{fix} \) 和练习 5.2.8 中对列表的编码，编写一个函数，求和丘奇数列表。

**解答：**

**步骤 1：回顾列表的编码（来自练习 5.2.8）**

- **空列表（nil）的表示：**

  $$
  \text{nil} = \lambda c.\ \lambda n.\ n
  $$

- **构造列表的函数（cons）：**

  $$
  \text{cons} = \lambda h.\ \lambda t.\ \lambda c.\ \lambda n.\ c\ h\ (t\ c\ n)
  $$

  - \( h \) 是头元素，\( t \) 是尾部列表。

**步骤 2：定义辅助函数**

- **判断列表是否为空（isnil）：**

  $$
  \text{isnil} = \lambda l.\ l\ (\lambda h.\ \lambda t.\ \text{fls})\ \text{tru}
  $$

- **获取列表头部元素（head）：**

  $$
  \text{head} = \lambda l.\ l\ (\lambda h.\ \lambda t.\ h)\ \text{dummy}
  $$

  - 需要注意处理空列表的情况，可能需要一个占位符 \( \text{dummy} \)。

- **获取列表尾部（tail）：**

  - 在纯 Lambda 演算中，实现 \( \text{tail} \) 比较复杂，可能需要使用类似于定义前驱函数的技巧。

**步骤 3：定义求和函数（sum）**

- **思路：**

  - 使用固定点组合子 \( \text{fix} \) 实现递归。
  - 如果列表为空，返回 \( c_0 \)（表示 0）。
  - 如果列表非空，返回头元素与递归调用尾部列表求和的结果之和。

- **定义：**

  $$
  \text{sum} = \text{fix}\ (\lambda f.\ \lambda l.\ \text{isnil}\ l\ c_0\ (\text{plus}\ (\text{head}\ l)\ (f\ (\text{tail}\ l))))
  $$

  - \( \text{plus} \) 是丘奇数的加法函数。

**步骤 4：验证求和函数**

- **示例列表**：\( L = \text{cons}\ c_1\ (\text{cons}\ c_2\ (\text{cons}\ c_3\ \text{nil})) \)

- **计算 \( \text{sum}\ L \)：**

  1. 检查 \( L \) 是否为空：\( \text{isnil}\ L \) 返回 \( \text{fls} \)。
  2. 计算 \( \text{head}\ L = c_1 \)。
  3. 递归计算 \( \text{sum}\ (\text{tail}\ L) \)：
     - \( \text{tail}\ L \) 是 \( \text{cons}\ c_2\ (\text{cons}\ c_3\ \text{nil}) \)。
     - 重复上述步骤，最终计算得到 \( c_2 + c_3 \)。
  4. 将结果相加：
     $$
     \text{sum}\ L = \text{plus}\ c_1\ (\text{plus}\ c_2\ c_3)
     $$

- **最终结果**：得到丘奇数 \( c_6 \)，对应于数字 6（即 \( 1 + 2 + 3 = 6 \)）。

**总结：**

- 使用固定点组合子和列表的折叠表示，我们成功定义了一个求和函数。
- 递归地处理列表中的每个元素，将其值相加。

---

### 丰富演算 (Enriching the Calculus)

我们已经看到，布尔值、数字以及它们的操作可以在纯粹的 Lambda 演算中编码。严格来说，我们可以在纯系统中完成所有需要的编程。然而，在处理示例时，包含原始的布尔值和数字（以及可能的其他数据类型）通常更为方便。

当我们需要明确正在使用哪个系统时，我们将使用符号 \( \lambda \) 表示纯 Lambda 演算（如图 5-3 定义的），使用 \( \lambda_{NB} \) 表示扩展了布尔值和算术表达式的系统（来自图 3-1 和 3-2）。

#### 在 \( \lambda_{NB} \) 中的两种实现

在 \( \lambda_{NB} \) 中，我们实际上有两种不同的布尔值和数字实现可供选择：**原始的**和我们在本章中开发的**编码的**。当然，在两者之间进行转换很容易。

**将丘奇布尔值转换为原始布尔值：**

- **定义：**

  $$
  \text{realbool} = \lambda b.\ b\ \text{true}\ \text{false}
  $$

- **解释：**

  - 丘奇布尔值 \( b \) 是一个函数，接受两个参数，选择其中一个返回。
  - 我们将 \( b \) 应用于原始布尔值 \( \text{true} \) 和 \( \text{false} \)，得到对应的原始布尔值。

**将原始布尔值转换为丘奇布尔值：**

- **定义：**

  $$
  \text{churchbool} = \lambda b.\ \text{if}\ b\ \text{then}\ \text{tru}\ \text{else}\ \text{fls}
  $$

- **解释：**

  - 使用原始的条件表达式 \( \text{if} \)。
  - 如果 \( b \) 为 \( \text{true} \)，则返回丘奇布尔值 \( \text{tru} \)。
  - 否则，返回 \( \text{fls} \)。

**构建更高级的操作：**

- **定义一个对丘奇数进行比较并返回原始布尔值的函数：**

  $$
  \text{realeq} = \lambda m.\ \lambda n.\ (\text{equal}\ m\ n)\ \text{true}\ \text{false}
  $$

  - \( \text{equal} \) 返回丘奇布尔值。
  - 我们将其应用于 \( \text{true} \) 和 \( \text{false} \)，得到原始布尔值。

**将丘奇数转换为原始数字：**

- **定义：**

  $$
  \text{realnat} = \lambda m.\ m\ (\lambda x.\ \text{succ}\ x)\ 0
  $$

- **解释：**

  - 丘奇数 \( m \) 是一个函数，接受两个参数 \( s \) 和 \( z \)。
  - 我们将 \( s \) 定义为 \( \lambda x.\ \text{succ}\ x \)，即对 \( x \) 应用一次 \( \text{succ} \)。
  - 初始值 \( z \) 为 \( 0 \)。
  - \( m \) 将 \( s \) 应用 \( n \) 次，即对 \( 0 \) 应用 \( \text{succ} \) 共 \( n \) 次，得到原始数字 \( n \)。

**注意事项：**

- 我们不能直接将 \( m \) 应用于 \( \text{succ} \)，因为 \( \text{succ} \) 在语法上必须应用于一个参数。
- 通过将 \( \text{succ} \) 包装在一个接受参数的函数中解决这个问题。

#### 原始布尔值和数字的便利性

原始布尔值和数字在示例中很有用，主要是因为**求值顺序**的原因。

**示例：**

- **考虑项 \( \text{scc}\ c_1 \)：**

  - 我们期望它求值为 \( c_2 \)（表示数字 2 的丘奇数）。
  - 实际上，在按值调用策略下，求值会停在某个中间状态，因为无法在 Lambda 抽象内部继续约简。

- **问题所在：**

  - 由于按值调用策略只在外层进行约简，无法在 Lambda 抽象内部进行。
  - 这导致剩余的计算无法完成。

**解决方法：**

- **使用原始数字进行转换：**

  $$
  \text{realnat}\ (\text{scc}\ c_1) \longrightarrow 2
  $$

  - 将丘奇数转换为原始数字，强制执行剩余的计算。
  - 这样，我们可以直接得到结果，验证我们的函数行为是否符合预期。

**更复杂的示例：**

- **计算 \( \text{times}\ c_2\ c_2 \)（即 \( 2 \times 2 \)）：**

  - 按值调用策略下，无法直接得到 \( c_4 \)。
  - 通过转换为原始数字，可以验证结果：

    $$
    \text{realnat}\ (\text{times}\ c_2\ c_2) \longrightarrow 4
    $$

#### 总结

- **系统间的转换：** 可以在编码的和原始的布尔值、数字之间轻松转换，方便我们在不同层次上操作数据。
- **求值顺序的影响：** 在按值调用策略下，某些项无法完全归约，通过转换，可以强制执行剩余的计算。
- **实用性：** 在示例和实际编程中，使用原始布尔值和数字更为方便，有助于验证和理解程序的行为。

---

**如果您对上述内容有任何疑问或需要进一步的解释，请随时提出！**

### ----------------------------

### 5.3 形式化内容 (Formalities)

#### **替换 (Substitution)**

**替换操作**在深入研究时会发现相当复杂。在本书中，我们实际上使用了两种不同的替换定义，每种都针对不同的目的进行了优化。第一种替换将在本节中介绍，它简洁直观，适用于示例以及数学定义和证明。第二种替换将在第6章展开，它符号上更为繁琐，依赖于一种替代的**德布鲁因表示法 (de Bruijn presentation)**，其中命名变量被数字索引取代，但在后续章节讨论的具体 ML 实现中更为方便。

为了理解替换的定义，我们可以通过几次错误的尝试来逐步推导出正确的定义。

---

**第一次尝试：最朴素的递归定义**

首先，让我们尝试最朴素的可能的递归定义。（形式上，我们正在通过对其参数 $t$ 进行归纳，定义一个函数 $[x \mapsto s]$。）

1. **替换变量本身：**

   $$
   [x \mapsto s]x = s
   $$

2. **替换其他变量：**

   $$
   [x \mapsto s]y = y \quad \text{如果 } x \ne y
   $$

3. **替换抽象：**

   $$
   [x \mapsto s](\lambda y.\ t_1) = \lambda y.\ [x \mapsto s]t_1
   $$

4. **替换应用：**

   $$
   [x \mapsto s](t_1\ t_2) = ([x \mapsto s]t_1)\ ([x \mapsto s]t_2)
   $$

**解释：**

- 对于变量 $x$，如果它等于要替换的变量，那么用替换项 $s$ 代替。
- 对于其他变量，保持不变。
- 对于抽象 $\lambda y.\ t_1$，递归地对其主体 $t_1$ 进行替换。
- 对于应用 $t_1\ t_2$，分别对 $t_1$ 和 $t_2$ 进行替换。

**示例：**

计算 $[x \mapsto (\lambda z.\ z\ w)](\lambda y.\ x)$：

$$
\begin{align*}
[x \mapsto (\lambda z.\ z\ w)](\lambda y.\ x) &= \lambda y.\ [x \mapsto (\lambda z.\ z\ w)]x \\
&= \lambda y.\ (\lambda z.\ z\ w)
\end{align*}
$$

这符合我们对替换行为的直觉。

**问题：**

然而，如果我们在绑定变量名的选择上不走运，这个定义会出错。例如：

计算 $[x \mapsto y](\lambda x.\ x)$：

$$
[x \mapsto y](\lambda x.\ x) = \lambda x.\ y
$$

这与我们对函数抽象的基本直觉相冲突，因为绑定变量的名字不重要——恒等函数无论写作 $\lambda x.\ x$、$\lambda y.\ y$ 还是 $\lambda \text{franz}.\ \text{franz}$ 都是完全相同的。如果它们在替换下的行为不完全相同，那么它们在约简下的行为也会不同，这显然是不合理的。

---

**分析问题：**

- **错误一**：在朴素的替换定义中，我们没有区分变量 $x$ 在项 $t$ 中的**自由出现**（应该被替换）和**绑定出现**（不应被替换）。
- 当我们在 $t$ 中遇到绑定 $x$ 的抽象时，替换操作应该停止。

---

**第二次尝试：改进的替换定义**

基于上述分析，我们修正替换定义：

1. **替换变量本身：**

   $$
   [x \mapsto s]x = s
   $$

2. **替换其他变量：**

   $$
   [x \mapsto s]y = y \quad \text{如果 } y \ne x
   $$

3. **替换抽象：**

   $$
   [x \mapsto s](\lambda y.\ t_1) =
   \begin{cases}
   \lambda y.\ t_1 & \text{如果 } y = x \\
   \lambda y.\ [x \mapsto s]t_1 & \text{如果 } y \ne x
   \end{cases}
   $$

4. **替换应用：**

   $$
   [x \mapsto s](t_1\ t_2) = ([x \mapsto s]t_1)\ ([x \mapsto s]t_2)
   $$

**解释：**

- 当遇到抽象 $\lambda y.\ t_1$ 时，如果绑定变量 $y$ 等于要替换的变量 $x$，那么替换操作停止，不对 $t_1$ 进行替换。
- 否则，递归地对 $t_1$ 进行替换。

**示例：**

计算 $[x \mapsto z](\lambda z.\ x)$：

$$
[x \mapsto z](\lambda z.\ x) = \lambda z.\ [x \mapsto z]x = \lambda z.\ z
$$

**问题：**

这次，我们犯了相反的错误：我们将常量函数 $\lambda z.\ x$ 变成了恒等函数 $\lambda z.\ z$！这发生的原因是我们碰巧在常量函数中选择了 $z$ 作为绑定变量名，因此显然仍然有问题。

---

**问题分析：**

- **错误二**：当我们将项 $s$（此处为 $z$）替换到项 $t$ 中时，$s$ 中的自由变量可能会在替换过程中变成绑定变量。这种现象称为**变量捕获 (variable capture)**。
- 为了避免变量捕获，我们需要确保 $t$ 中的绑定变量名与 $s$ 中的自由变量名不同。

---

**第三次尝试：避免变量捕获的替换**

为此，我们在抽象的替换规则中添加一个附加条件：

1. **替换变量本身：**

   $$
   [x \mapsto s]x = s
   $$

2. **替换其他变量：**

   $$
   [x \mapsto s]y = y \quad \text{如果 } y \ne x
   $$

3. **替换抽象：**

   $$
   [x \mapsto s](\lambda y.\ t_1) =
   \begin{cases}
   \lambda y.\ t_1 & \text{如果 } y = x \\
   \lambda y.\ [x \mapsto s]t_1 & \text{如果 } y \ne x \text{ 且 } y \notin FV(s)
   \end{cases}
   $$

   - 其中，$FV(s)$ 表示项 $s$ 的自由变量集。

4. **替换应用：**

   $$
   [x \mapsto s](t_1\ t_2) = ([x \mapsto s]t_1)\ ([x \mapsto s]t_2)
   $$

**解释：**

- 当绑定变量 $y$ 不等于 $x$ 且不在 $s$ 的自由变量集中时，才能递归地对 $t_1$ 进行替换。
- 否则，替换操作停止。

**问题：**

- 现在，我们的替换定义在它能做任何事情的时候，都是正确的。
- 然而，我们的修正使得替换操作变成了**部分函数 (partial operation)**，即在某些情况下无法应用。
- 例如，计算 $[x \mapsto y\ z](\lambda y.\ x\ y)$ 时，绑定变量 $y$ 不等于 $x$，但 $y$ 出现在替换项 $y\ z$ 的自由变量集中，因此替换规则无法适用。

---

**解决方案：使用 α-转换 (alpha-conversion)**

在类型系统和 Lambda 演算的文献中，常见的解决方法是使用**“同构于绑定变量重命名 (up to renaming of bound variables)”**的项。

**阿隆佐·丘奇 (Alonzo Church)** 使用**α-转换 (alpha-conversion)** 来表示在项中一致地重命名绑定变量的操作。

**5.3.4 约定 (Convention)**：

**仅在绑定变量名不同的项在所有上下文中都可以互换。**

**实际含义：**

- 我们可以在任何方便的地方，将任何 λ 绑定变量的名字更改为另一个名字（在 λ 的主体中一致地进行相同的更改）。
- 例如，如果我们想计算 $[x \mapsto y\ z](\lambda y.\ x\ y)$，我们首先将 $\lambda y.\ x\ y$ 重写为 $\lambda w.\ x\ w$。
- 然后计算 $[x \mapsto y\ z](\lambda w.\ x\ w)$，得到 $\lambda w.\ (y\ z)\ w$。

**这样做的好处：**

- 这个约定使得替换操作在实践中**“几乎是全域的 (as good as total)”**，因为每当我们发现替换操作无法应用时，我们可以通过必要的重命名来满足附加条件。
- 采用这个约定后，我们可以将替换的定义简化一些。

---

**最终的替换定义**

有了上述约定，我们可以假设绑定变量 $y$ 与 $x$ 以及 $s$ 的自由变量都不同。

**5.3.5 定义 (Definition) [替换 (Substitution)]：**

1. **替换变量本身：**

   $$
   [x \mapsto s]x = s
   $$

2. **替换其他变量：**

   $$
   [x \mapsto s]y = y \quad \text{如果 } y \ne x
   $$

3. **替换抽象：**

   $$
   [x \mapsto s](\lambda y.\ t_1) = \lambda y.\ [x \mapsto s]t_1 \quad \text{如果 } y \ne x \text{ 且 } y \notin FV(s)
   $$

4. **替换应用：**

   $$
   [x \mapsto s](t_1\ t_2) = [x \mapsto s]t_1\ [x \mapsto s]t_2
   $$

**总结：**

- 通过引入 α-转换，我们避免了变量捕获的问题，使得替换操作在实用中是全域的。
- 最终的替换定义确保了替换过程中不会发生变量捕获，同时简化了规则。

---

#### **习题 5.3.3 [««]：证明对于每个项 $t$，都有 $|FV(t)| \leq \text{size}(t)$。**

**解答：**

**目标：**

- 证明任意 Lambda 项 $t$ 的自由变量的数量不超过 $t$ 的大小，即 $|FV(t)| \leq \text{size}(t)$。

**定义回顾：**

1. **项的大小 (size)**：项中节点的数量，包括变量、抽象和应用。

2. **自由变量集 $FV(t)$**：项 $t$ 中所有自由变量的集合。

**证明思路：**

- 使用**数学归纳法**对项 $t$ 的结构进行证明。

**基例：**

- **当 $t$ 是一个变量 $x$ 时：**

  - $\text{size}(x) = 1$。
  - $FV(x) = \{ x \}$，因此 $|FV(x)| = 1$。
  - 所以 $|FV(x)| = \text{size}(x) = 1$。

**归纳假设：**

- 假设对于所有大小小于 $n$ 的项 $t$，都有 $|FV(t)| \leq \text{size}(t)$。

**归纳步骤：**

- **情况 1：$t = \lambda x.\ t_1$**

  - $\text{size}(t) = 1 + \text{size}(t_1)$。
  - $FV(t) = FV(t_1) \setminus \{ x \}$。
  - 所以 $|FV(t)| \leq |FV(t_1)|$。
  - 根据归纳假设，$|FV(t_1)| \leq \text{size}(t_1)$。
  - 因此，$|FV(t)| \leq \text{size}(t_1)$。
  - 又因为 $\text{size}(t) = 1 + \text{size}(t_1) \geq \text{size}(t_1)$。
  - 所以 $|FV(t)| \leq \text{size}(t)$。

- **情况 2：$t = t_1\ t_2$**

  - $\text{size}(t) = 1 + \text{size}(t_1) + \text{size}(t_2)$。
  - $FV(t) = FV(t_1) \cup FV(t_2)$。
  - 所以 $|FV(t)| \leq |FV(t_1)| + |FV(t_2)|$。
  - 根据归纳假设，$|FV(t_1)| \leq \text{size}(t_1)$，$|FV(t_2)| \leq \text{size}(t_2)$。
  - 因此，$|FV(t)| \leq \text{size}(t_1) + \text{size}(t_2)$。
  - 又因为 $\text{size}(t) = 1 + \text{size}(t_1) + \text{size}(t_2) \geq \text{size}(t_1) + \text{size}(t_2)$。
  - 所以 $|FV(t)| \leq \text{size}(t)$。

**结论：**

- 通过数学归纳法，我们证明了对于每个项 $t$，都有 $|FV(t)| \leq \text{size}(t)$。

---

#### **其余内容的讲解**

**操作语义 (Operational Semantics)**

Lambda 项的操作语义总结在**图 5-3**中。由于纯粹的 Lambda 演算仅有变量、抽象和应用，因此值的集合主要由 Lambda 抽象组成。

**求值规则：**

1. **E-AppAbs（应用抽象）**：

   $$
   (\lambda x.\ t_{12})\ v_2 \longrightarrow [x \mapsto v_2]t_{12}
   $$

   - 只要 $v_2$ 是一个值（即一个 Lambda 抽象），我们就可以对应用进行 beta-约简。

2. **E-App1（应用左侧求值）**：

   $$
   t_1 \longrightarrow t_1' \implies t_1\ t_2 \longrightarrow t_1'\ t_2
   $$

   - 当 $t_1$ 不是一个值且可以进一步求值时，优先对 $t_1$ 进行求值。

3. **E-App2（应用右侧求值）**：

   $$
   t_1 \text{ 是值},\ t_2 \longrightarrow t_2' \implies t_1\ t_2 \longrightarrow t_1\ t_2'
   $$

   - 当 $t_1$ 是一个值，且 $t_2$ 可以进一步求值时，对 $t_2$ 进行求值。

**求值顺序的控制：**

- 通过限制规则中元变量的选择，我们可以控制求值的顺序。
- 例如，在 E-App1 中，$t_1$ 可以是任何项，只要它能进一步求值。
- 在 E-App2 中，只有当 $t_1$ 是一个值时，才应用该规则。

---

**习题 5.3.6 [««]：将这些规则改编，用于描述其他三种求值策略——完全 beta-约简、正常序和惰性求值。**

**解答：**

**1. 完全 beta-约简 (Full Beta-Reduction)**

- **特点**：任何位置的 redex（可约项）都可以被约简。

- **规则修改**：

  - 引入一个通用的约简规则，允许在任何位置进行 beta-约简。

  - **E-Beta（通用 beta-约简）**：

    $$
    (\lambda x.\ t_{12})\ t_2 \longrightarrow [x \mapsto t_2]t_{12}
    $$

  - **E-Context（上下文规则）**：

    - 允许在任何上下文中进行约简，即在任何位置找到 redex 并进行替换。

**2. 正常序 (Normal Order)**

- **特点**：总是首先约简最左边、最外层的 redex。

- **规则修改**：

  - 定义**评价上下文 (Evaluation Context)**：

    $$
    E ::= [\ ] \mid E\ t \mid (\lambda x.\ E)
    $$

  - **E-Norm**：

    $$
    E[(\lambda x.\ t_{12})\ t_2] \longrightarrow E[[x \mapsto t_2]t_{12}]
    $$

  - 该规则确保总是优先约简最左、最外层的 redex。

**3. 惰性求值 (Lazy Evaluation)**

- **特点**：类似于正常序，但不会对函数的参数进行求值，除非必要。

- **规则修改**：

  - **E-Lazy**：

    $$
    (\lambda x.\ t_{12})\ t_2 \longrightarrow [x \mapsto t_2]t_{12}
    $$

  - 只有当应用发生时，才对参数进行替换，但不对参数 $t_2$ 进行求值。

---

### 5.4 **注释 (Notes)**

**历史背景：**

- **无类型 Lambda 演算 (Untyped Lambda-Calculus)** 由 **阿隆佐·丘奇 (Alonzo Church)** 及其同事在 1920 年代和 1930 年代开发（Church，1941）。

- 标准的无类型 Lambda 演算参考书是 **Barendregt（1984）**。

- **Hindley 和 Seldin（1986）** 也是一个重要的参考，虽然不如 Barendregt 那样全面，但更易于理解。

- **Barendregt（1990）** 在《理论计算机科学手册》中提供了简洁的综述。

**相关教材：**

- 关于 Lambda 演算的材料也可以在许多函数式编程语言教材中找到，例如 **Abelson 和 Sussman（1985）**、**Friedman、Wand 和 Haynes（2001）**、**Peyton Jones 和 Lester（1992）**。

- 以及编程语言语义学的教材，如 **Schmidt（1986）**、**Gunter（1992）**、**Winskel（1993）**、**Mitchell（1996）**。

**数据结构的编码：**

- **Böhm 和 Berarducci（1985）** 提供了使用 Lambda 项编码各种数据结构的系统方法。

**柯里化 (Currying)：**

- 尽管以 **Haskell Curry** 命名，但 Curry 否认发明了柯里化的想法。

- 通常将其归功于 **Schönfinkel（1924）**，但这一基本思想在 19 世纪的一些数学家中已经很熟悉，包括 **弗雷格 (Frege)** 和 **康托尔 (Cantor)**。

---

**引言：**

> **实际上，该系统可能还有其他应用，而不仅仅是用作逻辑。**
>
> —— 阿隆佐·丘奇 (Alonzo Church)，1932

---

**总结：**

- 本节深入探讨了替换操作的复杂性，展示了通过几次错误的尝试如何逐步完善替换定义。
- 介绍了变量捕获的问题，以及如何通过 α-转换和谨慎的替换规则来避免它。
- 还讨论了 Lambda 演算的操作语义，以及如何通过调整规则来描述不同的求值策略。
- 最后，提供了一些历史背景和参考资料，帮助读者更深入地理解 Lambda 演算的发展和应用。

---

**如果您对上述内容有任何疑问或需要进一步的解释，请随时提问！**

### ---------------------------

### 5 无类型 Lambda 演算 (The Untyped Lambda-Calculus)

在这一章中，我们将介绍 **无类型 Lambda 演算**，这是计算理论中最简洁的形式化系统之一，也是许多编程语言设计的基础。Lambda 演算的核心思想是用函数定义和应用来表示所有的计算。我们可以通过抽象 (abstraction)、应用 (application) 和变量 (variable) 来定义复杂的计算逻辑。

---

### **语法 (Syntax)**

在 Lambda 演算中，我们使用如下定义的三种基本构造来表示项 (terms)：

$$
t ::= x \mid \lambda x.\ t \mid t\ t
$$

**解释：**

1. **变量 (Variables)**：
   - $x$ 是变量，表示函数的参数或可以取值的名称。

2. **抽象 (Abstraction)**：
   - $\lambda x.\ t$ 是一个抽象，表示一个匿名函数。它接受一个参数 $x$ 并返回项 $t$。
   - 例如，$\lambda x.\ x + 1$ 表示一个将参数 $x$ 加 1 的函数。

3. **应用 (Application)**：
   - $t_1\ t_2$ 是函数应用，表示将函数 $t_1$ 应用于参数 $t_2$。
   - 例如，$(\lambda x.\ x + 1)\ 2$ 表示将参数 $2$ 传递给函数 $\lambda x.\ x + 1$，计算结果为 $3$。

---

### **值 (Values)**

在无类型 Lambda 演算中，只有一种值类型，即 Lambda 抽象：

$$
v ::= \lambda x.\ t
$$

**解释：**

- 值是 Lambda 抽象形式 $\lambda x.\ t$，表示一个函数，它接收参数 $x$ 并返回项 $t$。这是唯一的值类型。
- 注意：数值、布尔值等常见类型在无类型 Lambda 演算的基本形式中不存在，但我们可以通过 Lambda 抽象来编码这些数据类型。

---

### **操作语义 (Operational Semantics)**

无类型 Lambda 演算的计算通过函数应用的方式进行，操作语义定义了如何进行一步步的项约简 (reduction)。它的规则如下：

#### 1. **应用左侧求值 (E-App1)**

$$
t_1 \longrightarrow t_1' \implies t_1\ t_2 \longrightarrow t_1'\ t_2
$$

**解释：**

- 当左侧项 $t_1$ 可以被进一步求值时，首先对 $t_1$ 进行求值，再将结果应用到 $t_2$。
- 该规则保证了函数的左侧项 (即函数本身) 总是首先被求值。

---

#### 2. **应用右侧求值 (E-App2)**

$$
t_2 \longrightarrow t_2' \implies v_1\ t_2 \longrightarrow v_1\ t_2'
$$

**解释：**

- 当左侧项 $v_1$ 已经是一个值 (即一个 Lambda 抽象) 时，继续对右侧项 $t_2$ 进行求值。
- 该规则规定，当函数已经求值完成后，才会对参数进行求值。

---

#### 3. **应用抽象 (E-AppAbs)**

$$
(\lambda x.\ t_{12})\ v_2 \longrightarrow [x \mapsto v_2]t_{12}
$$

**解释：**

- 如果左侧项是一个抽象 $\lambda x.\ t_{12}$，并且右侧项是一个值 $v_2$，则将 $v_2$ 替换掉 $t_{12}$ 中的变量 $x$。
- 这个规则称为 **beta-约简 (beta-reduction)**，它是 Lambda 演算中最重要的约简规则。

**示例：**

设有 Lambda 表达式：

$$
(\lambda x.\ x + 1)\ 2
$$

通过应用 **E-AppAbs** 规则，进行替换操作：

$$
(\lambda x.\ x + 1)\ 2 \longrightarrow 2 + 1 = 3
$$

---

### **操作语义总结 (Summary of Operational Semantics)**

通过这三条规则，我们定义了无类型 Lambda 演算中的求值过程。整个求值顺序如下：

1. **左侧求值 (E-App1)**：首先对函数部分进行求值。
2. **右侧求值 (E-App2)**：函数部分求值后，再对参数部分进行求值。
3. **应用抽象 (E-AppAbs)**：最后对应用进行 beta-约简，即用实际参数替换抽象函数中的变量。

---

### **示例解析**

#### 示例 1：应用多个参数

假设有以下 Lambda 表达式：

$$
(\lambda x.\ \lambda y.\ x\ y)\ (\lambda z.\ z)\ 3
$$

这是一个多层次的应用。我们可以分步进行求值：

1. 首先应用 $\lambda x.\ \lambda y.\ x\ y$ 到 $\lambda z.\ z$：

   $$
   (\lambda x.\ \lambda y.\ x\ y)\ (\lambda z.\ z) \longrightarrow \lambda y.\ (\lambda z.\ z)\ y
   $$

2. 然后应用结果 $\lambda y.\ (\lambda z.\ z)\ y$ 到 $3$：

   $$
   (\lambda y.\ (\lambda z.\ z)\ y)\ 3 \longrightarrow (\lambda z.\ z)\ 3 \longrightarrow 3
   $$

最终结果是 $3$。

---

#### 示例 2：自应用 (Self-Application)

考虑自应用的 Lambda 表达式：

$$
(\lambda x.\ x\ x)\ (\lambda x.\ x\ x)
$$

这个表达式称为**omega (ω)**，它是一个发散 (divergent) 的表达式，因为它的约简步骤会无限重复下去：

$$
(\lambda x.\ x\ x)\ (\lambda x.\ x\ x) \longrightarrow (\lambda x.\ x\ x)\ (\lambda x.\ x\ x)
$$

每次应用后，结果仍然是相同的表达式，因此它没有终止形式（没有正常形式），会一直进行约简。

---

### **练习**

#### **5.2.2 习题 [««]：寻找另一种定义 Church 数字后继函数的方法。**

**解答：**

我们可以通过重新设计后继函数的应用方式来定义 Church 数字的后继函数。

新的后继函数可以定义为：

$$
scc = \lambda n.\ \lambda s.\ \lambda z.\ s\ (n\ s\ z)
$$

在此定义中，$n$ 是一个 Church 数字，$s$ 是表示递增的函数，$z$ 是初始值。我们通过递归地应用 $s$ 来构建数字的后继。

#### **5.2.3 习题 [««]：是否可以在定义 Church 数字的乘法时，不使用加法？**

**解答：**

是的，我们可以直接定义乘法，而不使用加法。乘法的定义可以通过重复应用后继函数来实现：

$$
times = \lambda m.\ \lambda n.\ \lambda s.\ \lambda z.\ m\ (n\ s)\ z
$$

这个定义直接通过 $n$ 和 $m$ 来构造乘法，即应用 $n$ 次 $s$，并对 $m$ 个 $n$ 进行这样的应用。

#### **5.3.6 习题 [««]：将这些规则改编，用于描述其他三种求值策略——完全 beta-约简、正常序和惰性求值。**

**解答：**

1. **完全 beta-约简 (Full Beta-Reduction)**：允许对任何位置的 redex 进行约简，而不受顺序限制。

   - 规则：对任何位置的 $(\lambda x.\ t_{12})\ t_2$ 进行约简。

2. **正常序 (Normal-Order Evaluation)**：总是优先约简最外层的 redex。

   - 规则：首先选择最左、最外层的 redex 进行约简。

3. **惰性求值 (Lazy Evaluation)**：仅在需要时对函数的参数进行求值。

   - 规则：在调用函数时，将参数保持为未求值的形式，直到参数的值确实被使用时再进行求值。

---

通过这些习题，我们深入理解了 Lambda 演算的不同求值策略，以及如何使用 Lambda 演算表达复杂的计算逻辑。

### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------