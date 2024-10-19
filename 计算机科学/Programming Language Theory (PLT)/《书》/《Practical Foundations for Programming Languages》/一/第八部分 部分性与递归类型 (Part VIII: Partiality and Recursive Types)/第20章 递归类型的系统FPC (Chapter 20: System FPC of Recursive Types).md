[toc]



### 第20章：递归类型系统 FPC (System FPC of Recursive Types)

在本章中，我们将探讨一种包含乘积、和、部分函数以及**递归类型 (Recursive Types)** 的语言 FPC (**System FPC**)。递归类型是**类型方程 (Type Equations)** 的解，这类方程具有形式 $t \sim= \tau$，其中 $t$ 可以在 $\tau$ 中任意位置出现。递归类型的本质是一种**不受限制的类型运算符 (Unrestricted Type Operator)** 的不动点，形式上表示为 $t.\tau$。在本章中，我们将详细研究如何在这种框架下解决类型方程，并讨论相关的特性与应用。

---

#### 20.1 解决类型方程 (Solving Type Equations)

首先，递归类型解决了类型方程 $t \sim= \tau$，其中类型 $t$ 自引用 (Self-referential)，意味着 $t$ 可以在类型表达式 $\tau$ 中重复出现，甚至在函数类型中也可以如此。例如，考虑类型方程 $t \sim= t \to t$，这意味着类型 $t$ 自身等价于从类型 $t$ 到 $t$ 的部分函数类型。如果类型是集合，这个方程在集合论中无法解决，因为一个集合上的部分函数的数量总是比集合中的元素要多，但在类型系统中，由于我们描述的是可计算的函数（而非任意函数），这类方程是可解的。

##### 例子：
类型方程 $t \sim= t \to t$ 描述了一个递归类型，它表示所有**定义在自身上的部分函数 (Partial Functions Defined on Itself)**。

公式解释：
$$
t \sim= t \to t
$$

这个类型方程描述了一个类型 $t$，它与一个从 $t$ 到 $t$ 的函数类型等价。在集合论的背景下，这种方程由于基数问题无法求解，但在递归类型系统中可以通过允许部分函数的存在来解决。

**关键概念：**
- **递归类型 (Recursive Types)**：一种类型的定义可以包含自身。
- **类型方程 (Type Equations)**：类型变量的等式，允许类型递归地引用自身。

---

#### 20.2 归纳和余归纳类型 (Inductive and Coinductive Types)

在 FPC 中，**归纳类型 (Inductive Types)** 和 **余归纳类型 (Coinductive Types)** 是两种主要的递归类型形式。在**懒惰求值 (Lazy Evaluation)** 语义下，递归类型具有余归纳的性质，而在**严格求值 (Eager Evaluation)** 语义下，递归类型具有归纳的性质。

##### 归纳类型：
归纳类型是一种**最小不动点 (Least Fixed Point)**，意味着它定义了递归结构的有限构造。例如，`List` 类型可以定义为：
$$
\text{List} \sim= \text{Nil} + (\text{Element} \times \text{List})
$$
这表示一个列表要么为空（`Nil`），要么是一个元素加上剩余列表的递归结构。

##### 余归纳类型：
余归纳类型是一种**最大不动点 (Greatest Fixed Point)**，适用于无限数据结构，如**无限流 (Streams)**。一个无限流可以定义为：
$$
\text{Stream} \sim= \text{Element} \times \text{Stream}
$$
这表示流是一个元素加上一个递归的流。

**懒惰与严格求值的区别**：
在懒惰求值下，递归类型具有余归纳性质，能够处理无限结构；而在严格求值下，递归类型具有归纳性质，主要用于有限结构。严格求值的系统更具表现力，因为可以通过函数类型来模拟懒惰求值，从而表达余归纳结构。

---

#### 20.3 自引用 (Self-Reference)

递归类型支持自引用，这意味着类型可以在其定义中引用自身。自引用允许在类型系统中定义具有递归性质的数据结构和函数。

##### 例子：
例如，定义一个**递归函数类型**可以写作：
$$
f : \tau \to (\tau \to \tau)
$$
这里的 $f$ 是一个递归函数，它的输入和输出都具有递归结构。

公式解释：
$$
f : \tau \to (\tau \to \tau)
$$
这个公式描述了一个函数 $f$，它的类型为从类型 $\tau$ 到 $\tau \to \tau$ 的函数，意味着函数 $f$ 的输出是一个递归函数。

---

#### 20.4 状态的起源 (The Origin of State)

递归类型还提供了描述**状态 (State)** 的能力。状态可以被视为一个递归结构，定义了如何从一个状态转移到下一个状态。在计算机科学中，状态通常用递归类型来表示，例如状态机和状态转移系统。

##### 例子：
一个简单的状态类型可以定义为：
$$
\text{State} \sim= \text{Input} \times (\text{State} \to \text{Output})
$$
这表示一个状态接收输入并递归地生成输出。

---

#### 20.5 笔记 (Notes)

递归类型系统 FPC 提供了对递归结构的强大支持，特别是在处理部分函数和状态表示方面具有显著的优势。在研究递归类型时，严格求值和懒惰求值之间的区别至关重要，这直接影响到类型系统的表现力和性质。

- **递归类型的核心思想**是将类型方程看作类型的不动点，通过递归定义类型自身，使得系统能够处理复杂的递归结构。
- **求值策略的选择**，无论是严格求值还是懒惰求值，都对递归类型的行为产生深远的影响。

### ---------------------------------

### 20.1 解决类型方程 (Solving Type Equations)

FPC (**System FPC**) 是一种包含乘积 (Product)、和 (Sum)、部分函数 (Partial Function) 以及递归类型 (Recursive Types) 的语言。递归类型允许定义**类型方程 (Type Equations)**，其中类型可以自引用 (Self-Reference)，这使得我们能够解决类似 $t \sim= \tau$ 的类型方程。递归类型是一种不动点 (Fixed Point)，用于表示复杂的自引用类型结构。

---

#### 递归类型的语法

在 FPC 中，递归类型的语法形式如下所示：

- **类型 (Type)** 的定义：
  $$
  \tau ::= t \quad \text{(自引用, self-reference)}
  \quad | \quad \text{rec}(t.\tau) \quad \text{(递归类型, recursive type)}
  $$

  其中，$\text{rec}(t.\tau)$ 定义了递归类型，它表示类型 $t$ 是类型 $\tau$ 的不动点。不动点的思想是，类型 $t$ 在 $\tau$ 中可以无限引用自身。

- **表达式 (Expression)** 的定义：
  $$
  e ::= \text{fold}(e) \quad \text{(折叠, fold)} 
  \quad | \quad \text{unfold}(e) \quad \text{(展开, unfold)}
  $$

  - $\text{fold}$ 操作将递归类型的实例封装为递归类型。
  - $\text{unfold}$ 操作将递归类型的实例展开，暴露其底层结构。

---

#### 静态语义 (Statics)

递归类型的静态语义定义如下规则：

1. **类型形成规则 (Type Formation Rule)**：
   递归类型由类型方程 $t$ 和 $\tau$ 组成。
   $$
   \frac{t \ \text{是类型} \quad \tau \ \text{是类型}}{\text{rec}(t.\tau) \ \text{是类型}} \quad (20.1)
   $$

2. **折叠 (fold) 和展开 (unfold) 的类型规则**：

   - **fold 操作**：给定表达式 $e$ 的类型为 $[rec(t.\tau)/t]\tau$，即用递归类型 $rec(t.\tau)$ 替换 $t$ 后的类型 $\tau$，那么折叠后的表达式类型为 $rec(t.\tau)$。
     $$
     \frac{e : [rec(t.\tau)/t]\tau}{\text{fold}(e) : rec(t.\tau)} \quad (20.2a)
     $$

   - **unfold 操作**：给定表达式 $e$ 的类型为递归类型 $rec(t.\tau)$，展开后的表达式类型为 $[rec(t.\tau)/t]\tau$。
     $$
     \frac{e : rec(t.\tau)}{\text{unfold}(e) : [rec(t.\tau)/t]\tau} \quad (20.2b)
     $$

---

#### 动态语义 (Dynamics)

递归类型的动态语义规则定义了折叠和展开的操作过程：

1. **值规则 (Value Rule)**：
   如果表达式 $e$ 是一个值，那么折叠后的表达式 $\text{fold}(e)$ 也是一个值：
   $$
   \frac{e \ val}{\text{fold}(e) \ val} \quad (20.3a)
   $$

2. **折叠和展开的求值规则**：

   - **fold 操作的求值规则**：
     如果 $e$ 可以求值到 $e'$，则 $\text{fold}(e)$ 可以求值到 $\text{fold}(e')$：
     $$
     \frac{e \ −→ e'}{\text{fold}(e) \ −→ \text{fold}(e')} \quad (20.3b)
     $$

   - **unfold 操作的求值规则**：
     如果 $e$ 可以求值到 $e'$，则 $\text{unfold}(e)$ 可以求值到 $\text{unfold}(e')$：
     $$
     \frac{e \ −→ e'}{\text{unfold}(e) \ −→ \text{unfold}(e')} \quad (20.3c)
     $$

   - **fold 与 unfold 的交互规则**：
     如果 $\text{fold}(e)$ 是一个值，展开它将还原为 $e$：
     $$
     \frac{\text{fold}(e) \ val}{\text{unfold}(\text{fold}(e)) \ −→ e} \quad (20.3d)
     $$

---

#### 安全性定理 (Safety Theorem)

**定理 20.1 (安全性定理 Safety Theorem)**：

1. **保持性 (Preservation)**：如果 $e : \tau$ 并且 $e \ −→ e'$，那么 $e' : \tau$。
2. **进展性 (Progress)**：如果 $e : \tau$，那么 $e$ 要么是一个值 (val)，要么存在 $e'$ 使得 $e \ −→ e'$。

证明该定理的关键是基于递归类型的折叠与展开规则，确保表达式在每一步求值后，仍然保持类型的正确性。

### ---------------------------------

### 20.2 归纳类型与余归纳类型 (Inductive and Coinductive Types)

**递归类型 (Recursive Types)** 可以用来表示归纳类型 (Inductive Types) 和余归纳类型 (Coinductive Types)。在 **System FPC** 中，递归类型使我们能够定义像自然数 (Natural Numbers) 这样的归纳类型，并且根据 FPC 的动态语义（eager 或 lazy），同样的递归类型可以表现出不同的行为。

---

#### 归纳类型和自然数 (Inductive Types and Natural Numbers)

在 FPC 的 **eager** 动态语义下，自然数的递归类型定义如下：

$$
\rho = \text{rec}\ t \ \text{is} \ [z \to \text{unit}, s \to t]
$$

这个递归类型 $\rho$ 满足以下类型方程：

$$
\rho \sim= [z \to \text{unit}, s \to \rho]
$$

这表示自然数的类型，它与**归纳定义的自然数类型**同构。通过递归定义，自然数的结构是：
- **零 (Zero)** 对应于 $z \to \text{unit}$，表示自然数 0。
- **继承者 (Successor)** 对应于 $s \to \rho$，表示递归定义的下一个自然数。

在 **eager** 语义中，递归类型 $\rho$ 对应于传统的自然数类型。具体的引入 (Introduction) 和消除 (Elimination) 操作定义如下：

- **引入 (Introduction)**：
  - 零：$z \equiv \text{fold}(z \cdot \langle \rangle)$
  - 继承者：$s(e) \equiv \text{fold}(s \cdot e)$

- **消除 (Elimination)**：
  对一个自然数类型的消除（类似于模式匹配）可以用以下表达式定义：
  $$
  \text{ifz}\ e \{z \to e_0 \ |\ s(x) \to e_1 \} \equiv \text{case}\ \text{unfold}(e)\ \{z \cdot \to e_0 \ |\ s \cdot x \to e_1\}
  $$

这个表达式的意思是，先展开 (unfold) 一个自然数 $e$，然后检查它是零 ($z$)，还是继承者 ($s(x)$)，并根据不同情况执行对应的操作。

> **练习**：检查 PCF 中自然数的 eager 动态语义是否与这些定义一致。

---

#### 余归纳类型与惰性自然数 (Coinductive Types and Lazy Natural Numbers)

在 **lazy** 动态语义下，递归类型 $\rho'$：

$$
\rho' = \text{rec}\ t \ \text{is} \ [z \to \text{unit}, s \to t]
$$

虽然满足相同的类型方程：

$$
\rho' \sim= [z \to \text{unit}, s \to \rho']
$$

但是这个 $\rho'$ **并不是**自然数类型，而是惰性自然数 (Lazy Natural Numbers)，即 **lnat** 类型。它允许包含一个“无限大的数” $\omega$，即：

$$
\omega = \text{fix}\ x : \text{nat} \ \text{is} \ s(x)
$$

这个 $\omega$ 代表了一个永远不会终止的继承者链，在惰性语义下，它被认为是自然数类型的一个合法值。显然，$\omega$ 并不是传统意义上的自然数，因为它比所有的自然数都大，并且违反了数学归纳法。

---

#### 列表类型 (List Types)

同样，在 **eager** 动态语义下，自然数列表 (Lists of Natural Numbers) 类型可以定义为以下递归类型：

$$
\text{rec}\ t \ \text{is} \ [n \to \text{unit}, c \to \text{nat} \times t]
$$

这个类型满足类型方程：

$$
\text{natlist} \sim= [n \to \text{unit}, c \to \text{nat} \times \text{natlist}]
$$

这表示了一个自然数列表的结构，其中：
- **nil** 对应于空列表 $n \to \text{unit}$。
- **cons** 对应于将一个元素添加到列表开头 $c \to \text{nat} \times \text{natlist}$。

引入 (Introduction) 操作如下：
- 空列表：$\text{nil} \equiv \text{fold}(n \cdot \langle \rangle)$
- 添加元素：$\text{cons}(e_1; e_2) \equiv \text{fold}(c \cdot \langle e_1, e_2 \rangle)$

列表的消除 (Elimination) 形式为：
$$
\text{case}\ e \{n \to e_0 \ |\ c(x; y) \to e_1 \} \equiv \text{case}\ \text{unfold}(e)\ \{n \cdot \to e_0 \ |\ c \cdot \langle x, y \rangle \to e_1\}
$$

---

### 总结

FPC 中递归类型的行为取决于动态语义的选择（**eager** 或 **lazy**）。在 eager 语义下，递归类型可以用于定义归纳类型，比如自然数和列表；而在 lazy 语义下，同样的递归类型则允许存在无限的结构，例如 $\omega$ 这样的惰性自然数。

### ---------------------------------

在惰性 (lazy) 语义下，递归类型表现出的行为与急性 (eager) 语义中的表现有显著区别。我们以类型

$$
\text{rec}\ t\ \text{is} \ [n \to \text{unit},\ c \to \text{nat} \times t]
$$

为例，来探讨这一差异。

---

### 惰性动态语义下的递归类型 (Recursive Types in Lazy Dynamics)

在 **lazy** 语义中，递归类型的值是惰性求值的，这意味着递归类型中的表达式直到使用时才会被求值。在该递归类型的定义中：

- $\text{fold}(e)$ 是类型 $\text{rec}\ t\ \text{is} \ [n \to \text{unit},\ c \to \text{nat} \times t]$ 的值，其中 $e$ 是未求值的 **sum 类型** 计算。
- $\text{sum}$ 类型的两个可能值是：
  - 一个包含未求值的 **unit 类型** 的注入值 $n \to \text{unit}$。
  - 一个包含未求值的 **product 类型** $\text{nat} \times t$ 的注入值 $c \to \text{nat} \times t$。

递归类型在这种情境下的值可以是：
- 一个有限列表（当递归终止时）。
- 一个无限列表（当递归永远不会终止时）。

特别地，惰性语义允许我们构造 **无限列表 (infinite lists)**，这些列表的尾部（即列表中的后续元素）可以无限延长。这种类型不仅可以表示终止的有限列表，还可以表示无限扩展的列表（**流 (streams)**）。

### 与急性动态语义的对比 (Comparison with Eager Dynamics)

在 **eager** 语义下，类似的递归类型定义了有限数据结构。例如，在急性语义下，类型：

$$
\rho = \text{rec}\ t\ \text{is} \ [z \to \text{unit},\ s \to t]
$$

对应于 **自然数** 类型，每个自然数要么是 $z$（零），要么是继承者 $s$。相反，惰性语义下的递归类型可以定义无限的、非终止的结构，例如惰性自然数和无限列表。具体来说，惰性递归类型中的值不会立即求值，因此它可以代表一个**无限流**，其尾部元素永远不会被完全计算出来。

---

### 惰性语义下的无限流 (Infinite Streams in Lazy Dynamics)

在惰性语义中，递归类型

$$
\text{rec}\ t\ \text{is} \ [n \to \text{unit},\ c \to \text{nat} \times t]
$$

实际上表示**无限流**。无限流是一种数据结构，允许惰性地生成无限序列的元素。这个递归类型包含的值可以是：
- **有限列表**，最终达到结束标志 $n \to \text{unit}$。
- **无限列表**（或流），其每个元素都是一个自然数以及另一个未求值的流。

例如，值 $\omega = \text{fix}\ x\ \text{is}\ s(x)$ 可以表示一个没有终止的数值序列（无限个继承者），这个序列永远不会结束。

---

### "框图-指针" 不足以表达惰性数据结构 (Box-and-Pointer Diagrams' Limitations)

在传统的编程语言教材中，数据结构通常通过**框图-指针**（box-and-pointer）形式表示，例如使用框图表示急性语义中的自然数列表。这种表示方式在急性语义下相对直观，因为数据结构是立即求值的，每个元素都有一个明确的值。

然而，这种表示方法在惰性语义下并不适用。惰性递归类型中的数据结构（例如无限流）包含未求值的表达式，这些表达式在图形化表示中很难直接表达。即使在实现无限列表时，框图也不能准确反映惰性语义中的惰性计算。实际上，数据结构的真实含义需要通过编程语言来精确定义，而不仅仅是图形化的描述。

---

### 同一递归类型的不同含义 (Different Meanings of the Same Recursive Type)

相同的递归类型在不同的动态语义下可以具有完全不同的含义。例如，在惰性语言中，通常使用 "list" 这个术语来表示 **流 (stream)**，而使用 "nat" 来表示 **惰性自然数 (lazy natural numbers)**。但这些术语在急性语言中代表的类型是有限的、严格计算的结构（如有限列表和自然数）。

在惰性语言中，我们无法定义真正的**有限列表**或严格的**自然数**类型。通过惰性语义，类型可以表示无限数据结构，而这些在急性语义下则是不可能的。因此，在编程中选择急性还是惰性语义会极大地影响程序的行为。

---

### 总结

递归类型在 FPC 中的行为依赖于所选择的动态语义。急性动态语义下，递归类型通常用于定义有限数据结构，如自然数和列表。而在惰性动态语义下，递归类型可以表示无限数据结构，如无限流和惰性自然数。

### ---------------------------------

### 20.3 自引用 (Self-Reference)

在 **递归表达式** $ \text{fix}^{\tau}(x.e) $ 中，变量 $ x $ 代表表达式自身。自引用通过以下规则实现：

$$
\text{fix}^{\tau}(x.e) \rightarrow [\text{fix}^{\tau}(x.e)/x] e
$$

这意味着在执行过程中，我们将表达式本身替换为 $ x $，并将其应用于表达式的主体 $ e $。这里可以将 $ x $ 视为表达式 $ e $ 的一个**隐式参数**，该参数在表达式执行时被实例化为表达式自身。在很多编程语言中，这个隐式参数通常有一个特殊的名称，比如 `this` 或 `self`，以强调其**自引用**的含义。

通过这种直观的解释，我们可以从**递归类型 (recursive types)** 中推导出通用递归。这一推导表明，通用递归可以像其他语言特性一样，被视为**类型结构的表现**，而不是语言的特定设计。通过推导，我们可以隔离出一种**自引用表达式类型 (self-referential type)**，它通过以下语法规则定义：

#### 类型的语法定义 (Type Syntax)
$$
\text{Typ}\ \tau ::= \text{self}(\tau) \quad \text{自引用类型 (self-referential type)}
$$

#### 表达式的语法定义 (Expression Syntax)
$$
\text{Exp}\ e ::= \text{self}^{\tau}(x.e) \quad \text{自引用表达式 (self-referential expression)}
$$
$$
\text{unroll}(e) \quad \text{展开自引用 (unroll self-reference)}
$$

### 静态语义 (Statics)
这些构造的**静态语义**定义如下：

- **规则 (20.4a)**：当 $ x $ 具有类型 $\text{self}(\tau)$ 并且 $ e $ 具有类型 $\tau$ 时，$ \text{self}^{\tau}(x.e) $ 具有类型 $\text{self}(\tau)$：
$$
\Gamma, x : \text{self}(\tau) \vdash e : \tau \quad \Rightarrow \quad \Gamma \vdash \text{self}^{\tau}(x.e) : \text{self}(\tau)
$$

- **规则 (20.4b)**：当 $ e $ 具有类型 $\text{self}(\tau)$ 时，$ \text{unroll}(e) $ 具有类型 $\tau$：
$$
\Gamma \vdash e : \text{self}(\tau) \quad \Rightarrow \quad \Gamma \vdash \text{unroll}(e) : \tau
$$

### 动态语义 (Dynamics)
自引用的**动态语义**通过以下规则定义：

- **规则 (20.5a)**：$ \text{self}^{\tau}(x.e) $ 是一个值：
$$
\text{self}^{\tau}(x.e) \ val
$$

- **规则 (20.5b)**：如果 $ e $ 可以进行一次求值步骤 $ e \rightarrow e' $，那么 $ \text{unroll}(e) $ 也可以进行一次求值步骤：
$$
e \rightarrow e' \quad \Rightarrow \quad \text{unroll}(e) \rightarrow \text{unroll}(e')
$$

- **规则 (20.5c)**：$ \text{unroll(self}^{\tau}(x.e)) $ 会将 $ x $ 替换为整个表达式自身 $ \text{self}^{\tau}(x.e) $：
$$
\text{unroll(self}^{\tau}(x.e)) \rightarrow [\text{self}^{\tau}(x.e)/x] e
$$

### 与通用递归的比较 (Comparison to General Recursion)

与通用递归相比，自引用类型与通用递归的主要区别在于，我们为**自引用表达式**区分了一个专门的类型 $\text{self}(\tau)$，而不是允许在每种类型中进行自引用。然而，正如我们将要看到的，自引用类型同样可以实现通用递归。因此，这种区别在某种程度上只是语法风格的差异。

通过引入自引用类型和展开操作，我们实际上提供了一种构造递归结构的类型化方式，使递归和自引用不仅限于特定的函数形式，而是通过类型语言中的一种自然方式进行表达。

### ---------------------------------

### 自引用类型 (Self-Referential Type) 的定义

**自引用类型** $ \text{self}(\tau) $ 可以通过**递归类型** (recursive types) 来定义。核心思想是，考虑一个类型为 $\tau$ 的**自引用表达式**依赖于表达式自身。换句话说，我们希望定义 $ \text{self}(\tau) $，使其满足以下同构关系：

$$
\text{self}(\tau) \cong \text{self}(\tau) \to \tau
$$

这意味着，类型 $ \text{self}(\tau) $ 是 $\tau$ 类型的自引用形式，可以看作一个函数类型，它的返回值依赖于自身。

#### 递归类型的固定点 (Fixed Point of Recursive Types)

我们需要求出类型算子 $ t.t \to \tau $ 的固定点，其中 $ t $ 是一个代表该类型的类型变量。我们可以通过递归类型来表示这个固定点，形式如下：

$$
\text{rec}(t.t \to \tau)
$$

这就是 $ \text{self}(\tau) $ 的定义。通过这种方式，我们可以使用递归类型定义自引用类型。

### 自引用表达式的定义

**自引用表达式** $ \text{self}^{\tau}(x.e) $ 实际上是一个通过折叠 (fold) 操作构造的表达式，定义如下：

$$
\text{self}^{\tau}(x.e) = \text{fold}(\lambda (x : \text{self}(\tau)) e)
$$

我们可以验证规则 (20.4a) 的推导，这表示当 $ x $ 的类型为 $ \text{self}(\tau) $ 且 $ e $ 的类型为 $\tau$ 时，表达式 $ \text{self}^{\tau}(x.e) $ 的类型为 $ \text{self}(\tau)$。

### 展开自引用表达式 (Unrolling Self-Referential Expression)

对应地，展开自引用表达式 $ \text{unroll}(e) $ 可以表示为：

$$
\text{unfold}(e)(e)
$$

我们可以验证规则 (20.4b) 的推导，这表示当 $ e $ 的类型为 $ \text{self}(\tau) $ 时，$ \text{unroll}(e) $ 的类型为 $\tau$。

此外，我们可以推导出：

$$
\text{unroll(self}^{\tau}(y.e)) \to^* [\text{self}^{\tau}(y.e)/y]e
$$

这完成了对类型 $ \text{self}(\tau) $ 的自引用表达式的推导。

### 定义通用递归 (General Recursion)

通过自引用类型 $ \text{self}(\tau) $，我们可以为任意类型定义通用递归。具体来说，我们可以定义通用递归表达式 $ \text{fix}^{\tau}(x.e) $ 如下：

$$
\text{fix}^{\tau}(x.e) = \text{unroll(self}^{\tau}(y.[\text{unroll}(y)/x]e))
$$

这表示在表达式 $ e $ 中，$ x $ 的每一次递归调用都会展开。

我们可以验证这种定义符合**通用递归**的静态语义（见第19章），并通过以下推导验证其动态语义：

$$
\text{fix}^{\tau}(x.e) = \text{unroll(self}^{\tau}(y.[\text{unroll}(y)/x]e)) \\
\to^* [\text{unroll(self}^{\tau}(y.[\text{unroll}(y)/x]e))/x]e \\
= [\text{fix}^{\tau}(x.e)/x]e
$$

### 结论

递归类型可以用来定义任意类型的**非终止表达式**，如 $ \text{fix}^{\tau}(x.x) $。

### ---------------------------------

### 20.4 状态的起源 (The Origin of State)

**状态**(State) 概念在计算中的引入源于**递归**(recursion)或**自引用**(self-reference)的概念，正如我们在前一部分通过递归类型看到的。状态在计算中非常重要，因为它表示系统在某个时间点的条件或配置，尤其是当涉及可变对象时。状态的一个典型例子是计算机硬件中的**RS 锁存器** (RS latch)，它通过反馈机制保持状态，而反馈正是一种自引用或递归的形式。

### RS 锁存器的建模

**RS 锁存器**是一种电路，通过组合逻辑（通常是 NOR 或 NAND 门）实现，它可以随着时间的推移保持和改变状态。输入信号 $R$ 和 $S$ 控制锁存器的状态输出为 0 或 1，而该状态是通过反馈机制实现的。通过递归类型可以建模这种反馈机制，其中锁存器的状态是通过自引用的方式计算出来的。

#### 递归类型定义

我们可以用递归类型定义 RS 锁存器的类型 $ \tau_{rsl} $，如下所示：

$$
\tau_{rsl} = \text{rec } t \text{ is } \langle X \to \text{bool}, Q \to \text{bool}, N \to t \rangle
$$

- **$X$** 表示当前的输出状态之一。
- **$Q$** 表示锁存器的当前状态（状态寄存器）。
- **$N$** 表示下一状态，它是另一个锁存器的状态，通过递归函数计算。

在这里，锁存器的状态随着时间的推移不断变化，而这些变化是基于当前输入信号 $R$ 和 $S$ 以及锁存器的当前输出状态 $X$ 和 $Q$。

#### 状态的获取

假设 $e$ 是类型为 $ \tau_{rsl} $ 的锁存器状态，我们可以通过以下方式访问锁存器的不同状态：

- **$e @ X$**：展开锁存器状态，获取当前输出 $X$ 的布尔值。
- **$e @ Q$**：获取当前状态 $Q$ 的布尔值。
- **$e @ N$**：获取锁存器的下一状态。

通过这种方式，$e$ 可以表示锁存器的当前状态及其随时间演变的下一状态。

### 递归函数的定义

锁存器的行为通过递归函数 **rsl** 实现。递归函数 $rsl$ 定义如下：

$$
\text{fix } rsl \text{ is } \lambda (l : \tau_{rsl}) \, e_{rsl}
$$

其中 $e_{rsl}$ 表示锁存器的递归表达式：

$$
\text{fix } this \text{ is fold}(\langle X \to enor(s, l @ Q), Q \to enor(r, l @ X), N \to rsl(this) \rangle)
$$

- **$enor$** 是一个计算布尔值的二元函数，用于表示逻辑门的行为（例如 NOR 门）。
- **$rsl(this)$** 表示递归调用当前状态计算下一状态。

通过这种递归定义，锁存器的输出是基于当前状态的输入信号 $R$ 和 $S$ 以及前一个状态的输出值 $X$ 和 $Q$ 来计算的。

### 初始化锁存器状态

为了开始递归，我们需要一个初始状态。初始状态可以设定为锁存器的输出值均为 $ \text{false} $，递归函数 $rsl$ 计算该状态的下一状态：

$$
\text{fix } this \text{ is fold}(\langle X \to \text{false}, Q \to \text{false}, N \to rsl(this) \rangle)
$$

这里，$X$ 和 $Q$ 的初始值为 $ \text{false} $，而下一状态通过 $rsl$ 函数递归地计算。

### 结论

**自引用**在锁存器的状态维持中起到了核心作用，它使得锁存器的当前状态能够依赖于其先前状态，从而模拟了计算机中的状态保持机制。通过递归类型和递归函数，我们可以很好地描述这些复杂的行为，例如反馈机制和时间的推移。

### ---------------------------------

### 20.5 注释 (Notes)

对**递归类型**(recursive types) 的系统研究最早由 **Scott** (1976, 1982) 提出，旨在为**无类型 λ 演算**(untyped λ-calculus) 提供数学模型。递归类型用于递归的推导，正是基于 **Scott** 理论的一个应用。在 Scott 的理论中，递归类型被用来解决一些通常用递归函数表示的问题。

**范畴论视角**(category-theoretic view) 的递归类型由 **Wand** (1979) 和 **Smyth** 以及 **Plotkin** (1982) 进一步发展，他们将递归类型与范畴理论中的固定点 (fixed points) 联系起来。这种方法给出了递归类型的数学基础，解释了递归类型作为类型方程的解。

**自引用**(self-reference) 在数字逻辑中应用广泛，尤其是在实现状态（state）的概念时。这一思路可以追溯到 **Ward** 和 **Halstead** (1990) 的工作。通过自引用实现的状态在数字电路（例如锁存器和触发器）中起到至关重要的作用，特别是我们在 20.4 节中讨论的 **RS 锁存器** 例子。该例子灵感来自于 **Cook** (2009) 以及 **Abadi** 和 **Cardelli** (1996) 的著作，进一步揭示了自引用与状态维持之间的紧密关系。

关于信号作为**流**(streams) 的处理，特别是在 exercises 部分探讨的信号处理问题，则受到了 **Kahn** 的开创性工作的启发。Kahn 对信号处理的流模型提出了新的视角，这在函数式编程中具有重要影响。

最后，FPC 这个语言名称来自于 **Gunter** (1992) 的研究，它在编程语言设计中为递归类型的运用奠定了基础。


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------