[toc]



### 第9章 **简单类型 λ 演算** (Simply Typed Lambda-Calculus)

本章介绍了我们将在本书余下部分研究的类型化语言家族中最🥑基本的成员：**简单类型 λ 演算**（Simply Typed Lambda-Calculus），由 Church (1940) 和 Curry (1958) 提出。

---

#### 9.1 **函数类型** (Function Types)

在第8章中，我们为算术表达式引入了一个简单的静态类型系统，包含两个类型：

- **$Bool$**：分类其求值结果为布尔值的项。
- **$Nat$**：分类其求值结果为数字的项。

**类型不良的**（ill-typed）项是指不属于这些类型的项，包括在求值过程中会进入卡住状态（stuck states）的所有项（例如 $\text{if 0 then 1 else 2}$），以及一些实际上在求值过程中表现良好的项，但我们的静态分类过于保守（如 $\text{if true then 0 else false}$）。

现在，假设我们想为一个结合布尔值（为了简洁，本章将忽略数字）的语言构建一个类似的类型系统，并且包含纯 λ 演算（pure lambda-calculus）的原语。也就是说，我们希望为变量、抽象和应用引入类型规则，这些规则应当满足以下条件：

1. **保持类型安全性**：即满足 **保持定理** (Preservation Theorem) 和 **进展定理** (Progress Theorem)，即定理 8.3.2 和 8.3.3。

2. **不过于保守**：即应该为我们实际关心编写的大多数程序赋予类型。

当然，由于纯 λ 演算是图灵完备的，我们不可能对这些原语给出精确的类型分析。例如，对于如下程序：

$$
\text{if } \langle \text{长且复杂的计算} \rangle \text{ then true else } (\lambda x.\ x)
$$

我们无法可靠地确定其结果是布尔值还是函数，而不实际运行长且复杂的计算，看看它的结果是 $\text{true}$ 还是 $\text{false}$。但一般来说，这个长且复杂的计算甚至可能🥑发散（diverge），任何试图精确预测其结果的类型检查器也将发散。

---

为了将布尔类型的类型系统扩展到包含函数，我们显然需要添加一个类型，用于分类其求值结果为函数的项。作为第一步近似，我们将此类型称为 **$\rightarrow$**。如果我们添加一个类型规则：

$$
\frac{}{\lambda x.\ t : \rightarrow}
$$

为每个🥑 λ 抽象赋予类型 $\rightarrow$，我们可以将🥑简单的项（如 $\lambda x.\ x$）和🥑复合的项（如 $\text{if true then } (\lambda x.\ true) \text{ else } (\lambda x.\ \lambda y.\ y)$）都分类为产生函数的项。

但是，这样的粗略分析显然过于保守：像 $\lambda x.\ \text{true}$ 和 $\lambda x.\ \lambda y.\ y$ 这样的函数被归为同一类型 $\rightarrow$，忽略了将第一个函数应用于 $\text{true}$ 会得到布尔值，而将第二个函数应用于 $\text{true}$ 会得到另一个函数的事实。

一般来说，为了给应用的结果赋予有用的类型，我们需要知道左侧项的不仅仅是它是一个函数：我们需要知道该🥑函数返回什么类型。此外，为了确保函数在被调用时能够正确运行，我们需要跟踪它期望的参数类型。为了跟踪这些信息，我们将裸类型 $\rightarrow$ 替换为无限多的形如 $T_1 \rightarrow T_2$ 的类型家族，每个类型分类期望参数类型为 $T_1$ 并返回结果类型为 $T_2$ 的函数。

---

#### 9.1.1 **定义**：

类型 $Bool$ 上的简单类型集合由以下语法生成：

$$
\begin{align*}
T ::= &\quad \text{types:} \\
&\quad Bool &\quad \text{布尔类型} \\
&\mid T \rightarrow T &\quad \text{函数类型}
\end{align*}
$$

**解释：**

- **$Bool$**：布尔类型。
- **$T \rightarrow T$**：函数类型，表示从类型 $T$ 到类型 $T$ 的函数。

**注意：**

- 类型构造符 $\rightarrow$ 是🥑🥑🥑🥑🥑**右结合的**（right-associative），即表达式 $T_1 \rightarrow T_2 \rightarrow T_3$ 表示 $T_1 \rightarrow (T_2 \rightarrow T_3)$。
  - 便于科里化🥑🥑🥑🥑🥑🥑🥑🥑🥑🥑


例如，$Bool \rightarrow Bool$ 是将布尔参数映射到布尔结果的函数的类型。而 $(Bool \rightarrow Bool) \rightarrow (Bool \rightarrow Bool)$，或等价地 $(Bool \rightarrow Bool) \rightarrow Bool \rightarrow Bool$，是接受布尔到布尔函数作为参数并将它们作为结果返回的函数的类型。

---

#### 9.2 **类型关系** (The Typing Relation)

为了给像 $\lambda x.\ t$ 这样的抽象赋予类型，我们需要计算当该抽象应用于某个参数时会发生什么。接下来出现的问题是：我们如何知道期望的参数类型？有两种可能的回应：

1. 我们可以简单地在 λ 抽象中注释其参数的预期类型。

2. 我们可以分析抽象的主体，看看参数是如何被使用的，并尝试从中推断出它应该具有的类型。

现在，我们选择第一种方法。我们将不再只写 $\lambda x.\ t$，而是写作 $\lambda x:T_1.\ t_2$，其中对绑定变量的注释告诉我们假设参数将具有类型 $T_1$。

**解释：**

- 🥑**显式类型化的语言**（Explicitly Typed Languages）：在项中使用类型注释来帮助指导类型检查器的语言。

- 🥑**隐式类型化的语言**（Implicitly Typed Languages）：我们要求类型检查器推断或重建这些信息。

（在 λ 演算文献中，术语**类型赋值系统**（type-assignment systems）也被使用。）

本书的大部分内容将集中在显式类型化的语言上；隐式类型化将在第22章中探讨。

---

一旦我们知道了抽象的参数类型，那么函数结果的类型将是主体 $t_2$ 的类型，其中 $t_2$ 中的 $x$ 被假设为类型 $T_1$ 的项。这一直觉由以下类型规则捕获：

$$
\frac{\Gamma, x:T_1 \vdash t_2 : T_2}{\Gamma \vdash \lambda x:T_1.\ t_2 : T_1 \rightarrow T_2} \quad (T\text{-Abs})
$$

**解释：**

- **$\Gamma$**：类型环境（typing context），记录了自由变量的类型假设。

- **$\Gamma, x:T_1$**：将新的类型假设 $x:T_1$ 添加到类型环境中。

- **$\vdash$**：表示在给定类型环境下的类型判断。

- **$\lambda x:T_1.\ t_2$**：类型为 $T_1 \rightarrow T_2$。

由于项可能包含嵌套的 λ 抽象，我们通常需要讨论多个这样的假设。这将类型关系从一个二元关系 $t : T$ 改变为一个三元关系 $\Gamma \vdash t : T$，其中 $\Gamma$ 是关于 $t$ 中自由变量类型的假设集合。

**形式化定义：**

- **类型环境（typing context）$\Gamma$**：一个变量和其类型的序列，逗号操作符将新的绑定添加到右侧。

- 空环境通常记作 $\emptyset$，但我们通常省略它，直接写作 $\vdash t : T$，表示“在空假设集合下，闭项 $t$ 具有类型 $T$”。

为了避免新绑定和 $\Gamma$ 中可能已存在的绑定之间的混淆，我们要求名称 $x$ 被选择为与 $\Gamma$ 绑定的变量不同。由于我们的约定是 λ 抽象绑定的变量可以在方便时重命名，这个条件总是可以通过必要时重命名绑定变量来满足。

因此，$\Gamma$ 可以被视为变量到其类型的有限函数。遵循这个直觉，我们写作 $\text{dom}(\Gamma)$ 表示 $\Gamma$ 所绑定的变量集合。

---

**变量的类型规则**也直接来自上述讨论：一个变量具有我们当前假设它具有的类型。

$$
\frac{x:T \in \Gamma}{\Gamma \vdash x : T} \quad (T\text{-Var})
$$

**解释：**

- **$x:T \in \Gamma$**：表示在类型环境 $\Gamma$ 中，$x$ 被假设为类型 $T$。

- 因此，我们可以直接得出 $\Gamma \vdash x : T$。

---

**应用的类型规则**：

$$
\frac{\Gamma \vdash t_1 : T_{11} \rightarrow T_{12} \quad \Gamma \vdash t_2 : T_{11}}{\Gamma \vdash t_1\ t_2 : T_{12}} \quad (T\text{-App})
$$

**解释：**

- 如果 $t_1$ 求值为一个将类型 $T_{11}$ 的参数映射到类型 $T_{12}$ 的结果的函数，并且 $t_2$ 求值为类型 $T_{11}$ 的结果，那么将 $t_1$ 应用于 $t_2$ 的结果将是类型 $T_{12}$ 的值。

---

布尔常量和条件表达式的类型规则与之前相同（**图8-1**），但需要注意的是，在条件表达式的规则中：

$$
\frac{\Gamma \vdash t_1 : Bool \quad \Gamma \vdash t_2 : T \quad \Gamma \vdash t_3 : T}{\Gamma \vdash \text{if } t_1 \text{ then } t_2 \text{ else } t_3 : T} \quad (T\text{-If})
$$

元变量 $T$ 现在可以实例化为任何函数类型，允许我们给分支是函数的条件表达式赋予类型。例如：

$$
\text{if true then } (\lambda x:Bool.\ x) \text{ else } (\lambda x:Bool.\ \text{not } x)
$$

的类型是 $Bool \rightarrow Bool$。

---

这些类型规则汇总在 **图9-1** 中（为了完整性，也包括了语法和求值规则）。图中高亮的部分表示相对于无类型 λ 演算（untyped lambda-calculus）新增的内容——包括新的规则和添加到旧规则的新部分。就像我们对布尔值和数字所做的那样，我们将完整演算的定义拆分为两部分：纯的简单类型 λ 演算（没有任何基本类型），如图所示，以及单独的布尔值规则（已在 **图8-1** 中看到，当然我们必须在该图的每个类型判断中添加上下文 $\Gamma$）。

我们通常使用符号 **$\lambda_{\rightarrow}$** 来表示简单类型 λ 演算（对于具有不同基本类型集合的系统，我们也使用相同的符号）。

---

#### 9.2.1 **习题 [«]**：没有基本类型的纯简单类型 λ 演算实际上是退化的，因为它根本没有任何类型良好的项。为什么？

**解答：**

- **答案：**

  - 在没有基本类型的情况下，简单类型 λ 演算中唯一的类型是函数类型，即形如🥑 $T \rightarrow T'$。

  - 但是，要构造类型良好的项，我们需要基本类型来作为基础。例如，变量需要有类型，而类型规则 $T\text{-Var}$ 需要从环境 $\Gamma$ 中获得变量的类型。

  - 由于没有基本类型，我们无法为变量赋予初始类型，也就无法构造任何类型良好的项。

- **结论：**

  - 因此，没有基本类型的纯简单类型 λ 演算是退化的，没有任何类型良好的项。

---

类型规则的实例可以像我们对类型化算术表达式所做的那样，组合成推导树。例如，以下是证明项 $(\lambda x:Bool.\ x)\ true$ 在空上下文中具有类型 $Bool$ 的推导：

$$
\frac{
    \frac{x:Bool \in x:Bool}{x:Bool \vdash x : Bool} \quad (T\text{-Var})
}{
    \vdash \lambda x:Bool.\ x : Bool \rightarrow Bool \quad (T\text{-Abs})
}
$$

$$
\frac{
    \vdash \lambda x:Bool.\ x : Bool \rightarrow Bool \quad (T\text{-Abs}) \quad
    \vdash true : Bool \quad (T\text{-True})
}{
    \vdash (\lambda x:Bool.\ x)\ true : Bool \quad (T\text{-App})
}
$$

---

#### 9.2.2 **习题 [« 3]**：通过绘制推导树，证明以下项具有所指示的类型：

1. 在上下文 $f:Bool \rightarrow Bool$ 下，$f\ (\text{if false then true else false}) : Bool$

2. 在上下文 $f:Bool \rightarrow Bool$ 下，$\lambda x:Bool.\ f\ (\text{if } x \text{ then false else } x) : Bool \rightarrow Bool$

**解答：**

**第1题：**

证明 $f:Bool \rightarrow Bool \vdash f\ (\text{if false then true else false}) : Bool$

- **步骤1**：证明 $\Gamma \vdash f : Bool \rightarrow Bool$

  - $\Gamma = \{ f:Bool \rightarrow Bool \}$

  - 通过 $T\text{-Var}$，有 $f:Bool \rightarrow Bool \in \Gamma$，因此 $\Gamma \vdash f : Bool \rightarrow Bool$

- **步骤2**：证明 $\Gamma \vdash \text{if false then true else false} : Bool$

  - **步骤2.1**：证明 $\Gamma \vdash false : Bool$，通过 $T\text{-False}$

  - **步骤2.2**：证明 $\Gamma \vdash true : Bool$，通过 $T\text{-True}$

  - **步骤2.3**：证明 $\Gamma \vdash false : Bool$，同步骤2.1

  - **步骤2.4**：应用 $T\text{-If}$：

    $$
    \frac{
        \Gamma \vdash false : Bool \quad
        \Gamma \vdash true : Bool \quad
        \Gamma \vdash false : Bool
    }{
        \Gamma \vdash \text{if false then true else false} : Bool
    }
    $$

- **步骤3**：应用 $T\text{-App}$：

  $$
  \frac{
      \Gamma \vdash f : Bool \rightarrow Bool \quad
      \Gamma \vdash \text{if false then true else false} : Bool
  }{
      \Gamma \vdash f\ (\text{if false then true else false}) : Bool
  }
  $$

**结论**：在上下文 $f:Bool \rightarrow Bool$ 下，$f\ (\text{if false then true else false})$ 具有类型 $Bool$。

---

**第2题：**

证明 $f:Bool \rightarrow Bool \vdash \lambda x:Bool.\ f\ (\text{if } x \text{ then false else } x) : Bool \rightarrow Bool$

- **步骤1**：设定上下文 $\Gamma = \{ f:Bool \rightarrow Bool \}$

- **步骤2**：为 $\lambda x:Bool.\ t$ 应用 $T\text{-Abs}$，其中 $t = f\ (\text{if } x \text{ then false else } x)$

  - 需要证明 $\Gamma, x:Bool \vdash t : Bool$

- **步骤3**：证明 $\Gamma, x:Bool \vdash f : Bool \rightarrow Bool$

  - $f:Bool \rightarrow Bool \in \Gamma$，因此 $\Gamma, x:Bool \vdash f : Bool \rightarrow Bool$

- **步骤4**：证明 $\Gamma, x:Bool \vdash \text{if } x \text{ then false else } x : Bool$

  - **步骤4.1**：$\Gamma, x:Bool \vdash x : Bool$，因为 $x:Bool \in \Gamma, x:Bool$

  - **步骤4.2**：$\Gamma, x:Bool \vdash false : Bool$，通过 $T\text{-False}$

  - **步骤4.3**：$\Gamma, x:Bool \vdash x : Bool$，同步骤4.1

  - **步骤4.4**：应用 $T\text{-If}$：

    $$
    \frac{
        \Gamma, x:Bool \vdash x : Bool \quad
        \Gamma, x:Bool \vdash false : Bool \quad
        \Gamma, x:Bool \vdash x : Bool
    }{
        \Gamma, x:Bool \vdash \text{if } x \text{ then false else } x : Bool
    }
    $$

- **步骤5**：应用 $T\text{-App}$：

  $$
  \frac{
      \Gamma, x:Bool \vdash f : Bool \rightarrow Bool \quad
      \Gamma, x:Bool \vdash \text{if } x \text{ then false else } x : Bool
  }{
      \Gamma, x:Bool \vdash f\ (\text{if } x \text{ then false else } x) : Bool
  }
  $$

- **步骤6**：应用 $T\text{-Abs}$：

  $$
  \frac{
      \Gamma, x:Bool \vdash f\ (\text{if } x \text{ then false else } x) : Bool
  }{
      \Gamma \vdash \lambda x:Bool.\ f\ (\text{if } x \text{ then false else } x) : Bool \rightarrow Bool
  }
  $$

**结论**：在上下文 $f:Bool \rightarrow Bool$ 下，$\lambda x:Bool.\ f\ (\text{if } x \text{ then false else } x)$ 具有类型 $Bool \rightarrow Bool$。

---

#### 9.2.3 **习题 [«]**：找到一个上下文 $\Gamma$，使得项 $f\ x\ y$ 具有类型 $Bool$。你能给出所有这样上下文的简单描述吗？

**解答：**

**目标**：找到一个类型环境 $\Gamma$，使得 $\Gamma \vdash f\ x\ y : Bool$。

**分析**：

- 根据类型规则 $T\text{-App}$，我们需要依次应用函数：

  1. 首先应用 $f$ 于 $x$，得到 $f\ x$。

  2. 然后将结果应用于 $y$，得到 $f\ x\ y$。

- 为了使 $f\ x\ y$ 类型为 $Bool$，需要满足：

  1. $\Gamma \vdash f : T_f$。

  2. $\Gamma \vdash x : T_x$。

  3. $\Gamma \vdash y : T_y$。

  4. $T_f$ 应当是函数类型，其形式为 $T_x \rightarrow T_{intermediate}$。

  5. $T_{intermediate}$ 应当是函数类型，其形式为 $T_y \rightarrow Bool$。

**因此，$\Gamma$ 可以是：**

- $f : T_x \rightarrow (T_y \rightarrow Bool)$

- $x : T_x$

- $y : T_y$

**简单描述所有这样的上下文：**

- **$f$**：类型为 $T_x \rightarrow (T_y \rightarrow Bool)$ 的函数。

- **$x$**：类型为 $T_x$。

- **$y$**：类型为 $T_y$。

其中，$T_x$ 和 $T_y$ 可以是任意类型（包括基本类型和函数类型）。

**结论：**

- 任何使得 $f$ 的类型为 $T_x \rightarrow (T_y \rightarrow Bool)$，$x$ 的类型为 $T_x$，$y$ 的类型为 $T_y$ 的上下文 $\Gamma$，都满足 $\Gamma \vdash f\ x\ y : Bool$。

---

**补充：**

- 这说明，对于项 $f\ x\ y$，只要 $f$ 是一个接受类型为 $T_x$ 的参数并返回一个接受类型为 $T_y$ 的参数的函数，最终返回 $Bool$，那么 $f\ x\ y$ 的类型就是 $Bool$。

---

**总结：**

- 我们通过详细的推导，理解了简单类型 λ 演算中的类型规则和类型推导过程。

- 习题帮助我们巩固了对类型环境、类型规则和类型推导的理解，熟悉了如何构建类型推导树。

---

**如果您对上述内容有任何疑问，欢迎进一步讨论！**

### ---------------------------

### 第9章 **类型性质** (Properties of Typing)

在本节中，我们需要在证明类型安全性之前，先发展一些基本的引理。这些引理中的大部分与我们之前见过的类似——我们只需要在类型关系中添加上下文，并为 $\lambda$-抽象、应用和变量的每个证明添加子句。唯一显著的新要求是**类型关系的替换引理**（引理9.3.8）。

---

首先，**反演引理**记录了关于类型推导是如何构建的一系列观察：对于每种语法形式的子句告诉我们，“如果某种形式的项是类型良好的，那么它的子项必须具有这些形式的类型……”

#### 9.3.1 **引理（类型关系的反演）**：

1. 如果 $\Gamma \vdash x : R$，那么 $x : R \in \Gamma$。

2. 如果 $\Gamma \vdash \lambda x : T_1.\ t_2 : R$，那么存在某个 $R_2$ 使得 $R = T_1 \rightarrow R_2$，且 $\Gamma , x : T_1 \vdash t_2 : R_2$。

3. 如果 $\Gamma \vdash t_1\ t_2 : R$，那么存在某个类型 $T_{11}$，使得 $\Gamma \vdash t_1 : T_{11} \rightarrow R$ 且 $\Gamma \vdash t_2 : T_{11}$。

4. 如果 $\Gamma \vdash \text{true} : R$，那么 $R = Bool$。

5. 如果 $\Gamma \vdash \text{false} : R$，那么 $R = Bool$。

6. 如果 $\Gamma \vdash \text{if } t_1 \text{ then } t_2 \text{ else } t_3 : R$，那么 $\Gamma \vdash t_1 : Bool$，且 $\Gamma \vdash t_2 : R$，$\Gamma \vdash t_3 : R$。

**证明：**

直接从类型关系的定义得出。

---

#### 9.3.2 **习题 [推荐, «««]**：

是否存在某个上下文 $\Gamma$ 和类型 $T$，使得 $\Gamma \vdash x\ x : T$？如果存在，给出 $\Gamma$ 和 $T$，并为 $\Gamma \vdash x\ x : T$ 展示一个类型推导；如果不存在，证明之。

**解答：**

**目标：**

- 确定是否存在 $\Gamma$ 和 $T$，使得 $\Gamma \vdash x\ x : T$。

**分析：**

- 需要找到一个类型环境 $\Gamma$，使得项 $x\ x$ 在 $\Gamma$ 下是类型良好的。

- 根据类型规则 $T\text{-App}$，我们需要满足：

  $$
  \frac{\Gamma \vdash x : T_{11} \rightarrow T_{12} \quad \Gamma \vdash x : T_{11}}{\Gamma \vdash x\ x : T_{12}}
  $$

- 因此，我们需要找到一个类型 $T_{11}$，使得：

  1. $\Gamma \vdash x : T_{11} \rightarrow T_{12}$

  2. $\Gamma \vdash x : T_{11}$

- 这意味着 $x$ 必须同时具有类型 $T_{11}$ 和 $T_{11} \rightarrow T_{12}$。

- **问题：** 🥑🥑🥑🥑🥑🥑🥑🥑在简单类型系统中，变量在类型环境中只能有一个类型绑定。

**结论：**

- **不存在**这样的上下文 $\Gamma$ 和类型 $T$，使得 $\Gamma \vdash x\ x : T$。

**证明：**

- 假设存在 $\Gamma$ 和 $T$，使得 $\Gamma \vdash x\ x : T$。

- 根据类型规则 $T\text{-App}$，必须有：

  - $\Gamma \vdash x : T_{11} \rightarrow T$（1）

  - $\Gamma \vdash x : T_{11}$（2）

- 但是，这要求在类型环境 $\Gamma$ 中，$x$ 同时具有类型 $T_{11}$ 和 $T_{11} \rightarrow T$。

- 在简单类型 λ 演算中，类型环境中的变量只能绑定一个类型，即 $x : T$。

- 因此，除非 $T_{11} = T_{11} \rightarrow T$，否则无法满足上述条件。

- 但这会导致无限类型，不可能在简单类型系统中构造。

**因此，无法找到满足条件的 $\Gamma$ 和 $T$。**

---

在第9.2节中，我们选择了显式类型化的演算表示，以简化类型检查的工作。这涉及在函数抽象中的绑定变量添加类型注释，但在其他地方没有添加。这种做法在某种意义上是“足够的”吗？一个答案由“类型的唯一性”定理提供，该定理告诉我们，类型良好的项与它们的类型推导一一对应：类型推导可以从项中唯一地恢复（反之亦然）。实际上，这种对应关系是如此直接，从某种意义上说，项和推导之间几乎没有区别。

#### 9.3.3 **定理（类型的唯一性）**：

在给定的类型环境 $\Gamma$ 中，项 $t$（其自由变量都在 $\Gamma$ 的定义域中）至多有一个类型。也就是说，如果一个项是可类型化的，那么它的类型是唯一的。此外，只有一个由生成类型关系的推理规则构建的此类型的推导。

**证明：**

*练习。*

证明实际上是如此直接，以至于几乎没有什么可说的；但写出一些细节对于“建立”关于类型关系的证明是很好的练习。

---

对于我们将在本书后面看到的许多类型系统，这种项和推导之间的简单对应关系将不再成立：一个项将被赋予多种类型，每个类型都有许多类型推导来证明。在这些系统中，从项中有效地恢复类型推导通常需要大量工作。

接下来，**规范形式引理**告诉我们各种类型的值可能具有的形状。

#### 9.3.4 **引理（规范形式）**：

1. 如果 $v$ 是类型 $Bool$ 的值，那么 $v$ 要么是 $\text{true}$，要么是 $\text{false}$。

2. 如果 $v$ 是类型 $T_1 \rightarrow T_2$ 的值，那么 $v = \lambda x : T_1.\ t_2$。

**证明：**

直接证明。（类似于算术表达式的规范形式引理的证明，见引理8.3.1。）

---

利用规范形式引理，我们可以证明类似于定理8.3.2的进展定理。该定理的陈述需要一个小的改变：我们只对🥑**闭项**（没有自由变量的项）感兴趣。🥑对于开放项，进展定理实际上不成立：像 $f\ \text{true}$ 这样的项是一个正常形式（normal form），但不是一个值。然而，这种失败并不代表语言的缺陷，因为🥑完整的程序（也是我们实际关心求值的项）总是闭项。

#### 9.3.5 **定理（进展）**：

假设 $t$ 是一个🥑闭的、类型良好的项（即对于某个 $T$，有 $\vdash t : T$）。那么，$t$ 要么是一个值，要么存在某个 $t'$，使得 $t \rightarrow t'$。

**证明：**

对类型推导进行直接的归纳。布尔常量和条件的情况与类型化算术表达式的进展定理（定理8.3.2）的证明完全相同。变量的情况不可能发生（因为 $t$ 是闭项）。抽象的情况是直接的，因为抽象是值。唯一有趣的情况是应用的情况，其中 $t = t_1\ t_2$，且 $\vdash t_1 : T_{11} \rightarrow T_{12}$，$\vdash t_2 : T_{11}$。根据归纳假设，$t_1$ 要么是一个值，要么可以进行一步求值，$t_2$ 同理。

- 如果 $t_1$ 可以进行一步求值，则根据规则 $E\text{-App1}$，$t$ 可以进行一步求值。

- 如果 $t_1$ 是一个值，且 $t_2$ 可以进行一步求值，则根据规则 $E\text{-App2}$，$t$ 可以进行一步求值。

- 最后，如果 $t_1$ 和 $t_2$ 都是值，那么根据规范形式引理，$t_1$ 的形式为 $\lambda x : T_{11}.\ t_{12}$，因此规则 $E\text{-AppAbs}$ 适用于 $t$。

---

接下来，我们的任务是证明求值保持类型。我们首先陈述几个关于类型关系的“结构性引理”。它们本身并不是特别有趣，但在后续的证明中，它们将允许我们对类型推导进行一些有用的操作。

第一个结构性引理告诉我们，我们可以方便地排列上下文的元素，而不会改变在其下可以推导出的类型判断集合。回想一下（见第101页），上下文中的所有绑定必须具有不同的名称，并且每当我们向上下文添加一个绑定时，我们默认为绑定的名称与已经绑定的所有名称不同（使用约定5.3.4，在需要时重命名新的绑定）。

#### 9.3.6 **引理（排列）**：

如果 $\Gamma \vdash t : T$，且 $\Delta$ 是 $\Gamma$ 的一个排列，那么 $\Delta \vdash t : T$。此外，后一个推导与前一个推导具有相同的深度。

**证明：**

对类型推导进行直接的归纳。

---

#### 9.3.7 **引理（弱化）**：

如果 $\Gamma \vdash t : T$ 且 $x \notin \text{dom}(\Gamma)$，那么 $\Gamma , x : S \vdash t : T$。此外，后一个推导与前一个推导具有相同的深度。

**证明：**

对类型推导进行直接的归纳。

---

利用这些技术引理，我们可以证明类型关系的一个关键性质：🥑🥑🥑🥑🥑🥑🥑当变量被适当类型的项替换时，类型良好性被保持。类似的引理在编程语言的安全性证明中起着普遍的作用，因此通常被称为**替换引理**。

#### 9.3.8 **引理（替换下类型的保持）**：

如果 $\Gamma , x : S \vdash t : T$ 且 $\Gamma \vdash s : S$，那么 $\Gamma \vdash [x \mapsto s] t : T$。

**证明：**

对陈述 $\Gamma , x : S \vdash t : T$ 的推导进行归纳。对于给定的推导，我们根据证明中使用的最后一个类型规则进行分类。最有趣的情况是变量和抽象的情况。

- **情况 $T\text{-Var}$**：$t = z$，且 $z : T \in (\Gamma , x : S)$。

  - 有两种子情况需要考虑，取决于 $z$ 是 $x$ 还是另一个变量。

    - 如果 $z = x$，那么 $[x \mapsto s] z = s$。所需的结果是 $\Gamma \vdash s : S$，这是引理的假设之一。

    - 如果 $z \neq x$，那么 $[x \mapsto s] z = z$，所需的结果直接得到。

- **情况 $T\text{-Abs}$**：$t = \lambda y : T_2.\ t_1$，且 $T = T_2 \rightarrow T_1$，且 $\Gamma , x : S , y : T_2 \vdash t_1 : T_1$。

  - 根据约定5.3.4，我们可以假设 $x \neq y$ 且 $y \notin \text{FV}(s)$。

  - 使用排列引理，我们可以得到 $\Gamma , y : T_2 , x : S \vdash t_1 : T_1$。

  - 使用弱化引理，对给定的推导 $\Gamma \vdash s : S$，我们得到 $\Gamma , y : T_2 \vdash s : S$。

  - 现在，根据归纳假设，$\Gamma , y : T_2 \vdash [x \mapsto s] t_1 : T_1$。

  - 通过 $T\text{-Abs}$，$\Gamma \vdash \lambda y : T_2.\ [x \mapsto s] t_1 : T_2 \rightarrow T_1$。

  - 由于 $[x \mapsto s] t = \lambda y : T_2.\ [x \mapsto s] t_1$，我们得到了所需的结果。

- **情况 $T\text{-App}$**：$t = t_1\ t_2$，且 $\Gamma , x : S \vdash t_1 : T_2 \rightarrow T_1$，$\Gamma , x : S \vdash t_2 : T_2$，$T = T_1$。

  - 根据归纳假设，$\Gamma \vdash [x \mapsto s] t_1 : T_2 \rightarrow T_1$，$\Gamma \vdash [x \mapsto s] t_2 : T_2$。

  - 通过 $T\text{-App}$，$\Gamma \vdash [x \mapsto s] t_1\ [x \mapsto s] t_2 : T$，即 $\Gamma \vdash [x \mapsto s] (t_1\ t_2) : T$。

- **情况 $T\text{-True}$** 和 **$T\text{-False}$**：

  - 这些情况直接成立，因为布尔常量不受替换影响，且 $\Gamma \vdash \text{true} : Bool$，$\Gamma \vdash \text{false} : Bool$。

- **情况 $T\text{-If}$**：$t = \text{if } t_1 \text{ then } t_2 \text{ else } t_3$，且 $\Gamma , x : S \vdash t_1 : Bool$，$\Gamma , x : S \vdash t_2 : T$，$\Gamma , x : S \vdash t_3 : T$。

  - 根据归纳假设，$\Gamma \vdash [x \mapsto s] t_1 : Bool$，$\Gamma \vdash [x \mapsto s] t_2 : T$，$\Gamma \vdash [x \mapsto s] t_3 : T$。

  - 通过 $T\text{-If}$，$\Gamma \vdash \text{if } [x \mapsto s] t_1 \text{ then } [x \mapsto s] t_2 \text{ else } [x \mapsto s] t_3 : T$。

---

利用替换引理，我们可以证明类型安全性的另一半——即求值保持类型良好性。

#### 9.3.9 **定理（保持）**：

如果 $\Gamma \vdash t : T$ 且 $t \rightarrow t'$，那么 $\Gamma \vdash t' : T$。

**证明：**

*练习 [推荐, «««]。*

该证明的结构与算术表达式的类型保持定理（定理8.3.3）的证明非常相似，除了使用了替换引理。

---

#### 9.3.10 **习题 [推荐, ««]**：

在习题8.3.6中，我们研究了简单的🥑类型化算术表达式演算的**主题扩张**（subject expansion）性质。它是否适用于🥑简单类型 λ 演算的“函数部分”？也就是说，假设 $t$ 不包含任何条件表达式。是否有 $t \rightarrow t'$ 且 $\Gamma \vdash t' : T$ 蕴含 $\Gamma \vdash t : T$？

**解答：**

**答案：**

- **主题扩张**一般不成立，即 $t \rightarrow t'$ 且 $\Gamma \vdash t' : T$ 🥑并不必然蕴含 $\Gamma \vdash t : T$。

**反例：**

- 考虑项 $t = (\lambda x : T.\ x\ x)\ (\lambda x : T.\ x\ x)$。

- 首先，$t$ 可以进行一步求值：

  $$
  t = (\lambda x : T.\ x\ x)\ (\lambda x : T.\ x\ x) \rightarrow (\lambda x : T.\ x\ x)\ (\lambda x : T.\ x\ x) = t'
  $$

- 这里，$t \rightarrow t'$，且 $t' = t$，发生了循环。

- 现在，尝试给 $t'$ 赋予类型。

- 但如前面的讨论所示，在简单类型 λ 演算中，无法为 $x\ x$ 这样的项赋予类型，因此 $\Gamma \vdash t' : T$ 不成立。

- 但是，如果假设 $\Gamma \vdash t' : T$ 成立，那么我们无法推导出 $\Gamma \vdash t : T$。

**结论：**

- 因此，🥑🥑🥑🥑🥑🥑🥑🥑主题扩张在简单类型 λ 演算的函数部分不成立。

---

### **图9-1：纯简单类型 λ 演算（$\lambda_{\rightarrow}$）**

**语法**

$$
\begin{align*}
t ::= &\quad \text{terms:} \\
&\quad x &\quad \text{变量 (variable)} \\
&\mid \lambda x : T.\ t &\quad \text{抽象 (abstraction)} \\
&\mid t\ t &\quad \text{应用 (application)} \\
v ::= &\quad \text{values:} \\
&\quad \lambda x : T.\ t &\quad \text{抽象值 (abstraction value)} \\
T ::= &\quad \text{types:} \\
&\quad T \rightarrow T &\quad \text{函数类型 (type of functions)} \\
\Gamma ::= &\quad \text{contexts:} \\
&\quad \emptyset &\quad \text{空上下文 (empty context)} \\
&\mid \Gamma , x : T &\quad \text{项变量绑定 (term variable binding)} \\
\end{align*}
$$

---

**求值规则 $t \rightarrow t'$**

$$
\begin{align*}
\frac{t_1 \rightarrow t_1'}{t_1\ t_2 \rightarrow t_1'\ t_2} \quad (E\text{-App1})
\end{align*}
$$

$$
\begin{align*}
\frac{t_2 \rightarrow t_2'}{v_1\ t_2 \rightarrow v_1\ t_2'} \quad (E\text{-App2})
\end{align*}
$$

$$
\begin{align*}
\frac{}{\left( \lambda x : T_{11}.\ t_{12} \right)\ v_2 \rightarrow [x \mapsto v_2] t_{12}} \quad (E\text{-AppAbs})
\end{align*}
$$

---

**类型规则 $\Gamma \vdash t : T$**

$$
\begin{align*}
\frac{x : T \in \Gamma}{\Gamma \vdash x : T} \quad (T\text{-Var})
\end{align*}
$$

$$
\begin{align*}
\frac{\Gamma , x : T_1 \vdash t_2 : T_2}{\Gamma \vdash \lambda x : T_1.\ t_2 : T_1 \rightarrow T_2} \quad (T\text{-Abs})
\end{align*}
$$

$$
\begin{align*}
\frac{\Gamma \vdash t_1 : T_{11} \rightarrow T_{12} \quad \Gamma \vdash t_2 : T_{11}}{\Gamma \vdash t_1\ t_2 : T_{12}} \quad (T\text{-App})
\end{align*}
$$

---

**总结：**

- 本节深入探讨了简单类型 λ 演算的类型性质，尤其是类型关系的基本引理和定理。

- 证明了类型的唯一性、进展定理，以及替换引理等关键性质。

- 通过习题，我们认识到主题扩张在简单类型 λ 演算中不成立，深化了对类型系统的理解。

---

**如果您对上述内容有任何疑问，欢迎进一步讨论！**

### ----------------------------

### 第9章 **Curry-Howard 对应** (The Curry-Howard Correspondence)

---

#### 9.4 **Curry-Howard 对应**

**概述：**

- **函数类型构造符** $"\rightarrow"$ 具有两类类型规则：
  1. **引入规则** (Introduction Rule)：描述如何创建该类型的元素。
  2. **消除规则** (Elimination Rule)：描述如何使用该类型的元素。

- 当一个**引入形式**（如 $\lambda$ 抽象）是一个**消除形式**（如应用）的直接子项时，结果是一个**红ex**（redex，指可约项）——即计算的机会。

**引入和消除形式：**

- **引入规则**：用于构造类型的元素。例如，🥑$\lambda$ 抽象用于构造函数类型的元素。
- **消除规则**：用于使用类型的元素。例如，🥑函数应用用于使用函数类型的元素。

---

**示例：**

- **引入规则 $T\text{-Abs}$：**

  $$
  \frac{\Gamma , x : T_1 \vdash t_2 : T_2}{\Gamma \vdash \lambda x : T_1 . t_2 : T_1 \rightarrow T_2}
  $$

  - 描述如何🥑创建类型为 $T_1 \rightarrow T_2$ 的函数。

- **消除规则 $T\text{-App}$：**

  $$
  \frac{\Gamma \vdash t_1 : T_{11} \rightarrow T_{12} \quad \Gamma \vdash t_2 : T_{11}}{\Gamma \vdash t_1\ t_2 : T_{12}}
  $$

  - 描述如何🥑应用类型为 $T_{11} \rightarrow T_{12}$ 的函数到类型为 $T_{11}$ 的参数上。

---

**红ex（Redex）：**

- **定义**：一个可以被简化或归约的表达式。
- 当一个 $\lambda$ 抽象（引入形式）直接出现在一个函数应用（消除形式）中时，我们得到了一个红ex，例如：

  $$
  (\lambda x : T . t) \ t'
  $$

- 这个表达式可以通过 $\beta$-约简进行简化。

---

**引入和消除形式在类型系统讨论中的作用：**

- 有助于理解类型构造符的使用方式。
- 在更复杂的系统中，每个类型构造符都具有相应的引入和消除规则。

---

#### **练习 9.4.1 [«]**

**题目：**

- 图 8-1 中的 $Bool$ 类型规则中，哪些是引入规则，哪些是消除规则？
- 图 8-2 中的 $Nat$ 类型规则呢？

**解答：**

**对于 $Bool$ 类型（图 8-1）：**

- **引入规则：**
  - **$T\text{-True}$：**

    $$
    \frac{}{\Gamma \vdash \text{true} : Bool}
    $$

    - 引入了布尔常量 $\text{true}$。

  - **$T\text{-False}$：**

    $$
    \frac{}{\Gamma \vdash \text{false} : Bool}
    $$

    - 引入了布尔常量 $\text{false}$。

- **消除规则：**
  - **$T\text{-If}$：**

    $$
    \frac{\Gamma \vdash t_1 : Bool \quad \Gamma \vdash t_2 : T \quad \Gamma \vdash t_3 : T}{\Gamma \vdash \text{if } t_1 \text{ then } t_2 \text{ else } t_3 : T}
    $$

    - 使用布尔类型的值来决定条件分支的执行，消除了布尔类型的值。

**对于 $Nat$ 类型（图 8-2）：**

- **引入规则：**
  - **$T\text{-Zero}$：**

    $$
    \frac{}{\Gamma \vdash 0 : Nat}
    $$

    - 引入了自然数常量 $0$。

  - **$T\text{-Succ}$：**

    $$
    \frac{\Gamma \vdash t : Nat}{\Gamma \vdash \text{succ } t : Nat}
    $$

    - 构造下一个自然数，基于已有的自然数。

- **消除规则：**
  - **$T\text{-Pred}$：**

    $$
    \frac{\Gamma \vdash t : Nat}{\Gamma \vdash \text{pred } t : Nat}
    $$

    - 对自然数进行前驱操作，消除了一个 $\text{succ}$。

  - **$T\text{-IsZero}$：**

    $$
    \frac{\Gamma \vdash t : Nat}{\Gamma \vdash \text{iszero } t : Bool}
    $$

    - 检查自然数是否为零，将自然数消除为布尔值。

---

#### **Curry-Howard 对应**

**概述：**

- **背景**：Curry 和 Howard 发现，在构造性逻辑中，命题的证明与计算之间存在深刻的联系。

- **构造性逻辑**：🥑🥑🥑要求对命题的证明提供具体的证据，即必须构造出证明对象。

- **对应关系**：

  | 逻辑               | 编程语言                              |
  | ------------------ | ------------------------------------- |
  | 命题               | 类型                                  |
  | 命题 $P \supset Q$ | 类型 $P \rightarrow Q$                |
  | 命题 $P \land Q$   | 类型 $P \times Q$（参见 §11.6）       |
  | 命题 $P$ 的证明    | 类型为 $P$ 的项 $t$                   |
  | 命题 $P$ 可证明    | 类型 $P$ 是可被某个项 inhabitation 的 |

**详细解释：**

- **命题与类型对应**：逻辑中的命题对应于类型系统中的类型。

- **证明与程序对应**：命题的证明对应于类型系统中具有相应类型的程序（项）。

- **逻辑联结词与类型构造符对应**：

  - **蕴含 $P \supset Q$**：对应于函数类型 $P \rightarrow Q$。

  - **合取 $P \land Q$**：对应于产品类型（乘积类型）$P \times Q$。

- **计算过程与证明简化对应**：在编程语言中，计算（如 λ-项的归约）对应于逻辑中通过消除**割规则**（cut elimination）进行的证明简化。

---

**意义：**

- **“命题即类型”**（Propositions as Types）：

  - 该对应关系也被称为“命题即类型”的类比。

  - 表明类型系统与逻辑系统之间存在深刻的结构对应。

- **应用**：

  - 这种对应关系不仅限于特定的类型系统和逻辑，可以扩展到各种类型系统和逻辑。

  - 例如，**System F**（第23章）对应于二阶构造性逻辑，允许**对命题进行全称量化**。

  - **System Fω**（第30章）对应于高阶逻辑。

---

**延伸和影响：**

- **线性逻辑与线性类型系统**：

  - Girard 的线性逻辑引出了线性类型系统的概念。

  - 线性类型系统在资源管理、并发编程等领域有重要应用。

- **模态逻辑与部分求值**：

  - 模态逻辑被用于设计部分求值和运行时代码生成的框架。

---

**参考文献：**

- Girard, Lafont, and Taylor (1989)

- Gallier (1993)

- Sørensen and Urzyczyn (1998)

- Pfenning (2001)

- Goubault-Larrecq and Mackie (1997)

- Simmons (2000)

---

#### 9.5 **擦除和可类型性** (Erasure and Typability)

**概述：**

- 在 **图9-1** 中，我们直接在简单类型化的项上定义了求值关系。

- **类型注释**在求值过程中不起作用，但我们在求值时仍然携带这些注释。

- **编译器实践**：

  - 🥑🥑🥑🥑🥑🥑🥑🥑🥑🥑🥑大多数实际的编程语言编译器在运行时避免携带类型注释。

  - 🥑🥑🥑🥑🥑🥑🥑🥑🥑🥑🥑🥑类型注释在类型检查和代码生成阶段使用，但不会出现在编译后的程序中。

- **效果**：

  - 程序在求值之前被转换回无类型的形式。

- **形式化**：

  - 这种语义可以使用**擦除函数**（erasure function）形式化，该函数将简单类型化的项映射到对应的无类型项。

---

#### **定义 9.5.1：擦除函数**

- **擦除规则**：

  $$
  \begin{align*}
  \text{erase}(x) &= x \\
  \text{erase}(\lambda x : T_1 . t_2) &= \lambda x . \text{erase}(t_2) \\
  \text{erase}(t_1\ t_2) &= \text{erase}(t_1)\ \text{erase}(t_2)
  \end{align*}
  $$

- **解释**：

  - 对于变量，擦除后仍然是变量本身。

  - 对于带类型注释的 λ 抽象，擦除类型注释，保留变量名和函数体。

  - 对于应用，递归擦除函数和参数。

---

**期望：**

- **等价性**：

  - 我们期望直接对类型化项求值与先擦除类型再对无类型项求值的结果是相同的。

- **形式化**：

  - 这一期望由以下定理形式化，总结为“求值与擦除可交换”。

---

#### **定理 9.5.2：求值与擦除的可交换性**

1. 如果 $t \rightarrow t'$（根据类型化的求值关系），那么 $\text{erase}(t) \rightarrow \text{erase}(t')$。

2. 如果 $\text{erase}(t) \rightarrow m'$（根据无类型的求值关系），那么存在一个简单类型化的项 $t'$，使得 $t \rightarrow t'$ 且 $\text{erase}(t') = m'$。

**证明：**

- 通过对求值推导的归纳证明。

**意义：**

- **编译器正确性**：

  - 该定理在更复杂的语言和编译器中变得非常重要。

  - 🥑它告诉我们，程序员所编写的高级语言语义与实际实现所使用的低级求值策略是一致的。

---

**进一步的问题：**

- **给定无类型的 λ 项 $m$，是否存在一个简单类型化的项 $t$，使得 $\text{erase}(t) = m$？**

- 这涉及到**可类型性**和**类型重建**的问题。

---

#### **定义 9.5.3：可类型性**

- **定义**：

  - 如果存在简单类型化的项 $t$、类型 $T$ 和上下文 $\Gamma$，使得 $\text{erase}(t) = m$ 且 $\Gamma \vdash t : T$，则称无类型 λ 项 $m$ 在 $\lambda_{\rightarrow}$ 中是**可类型化的**。

- **解释**：

  - 🥑🥑🥑🥑也就是说，我们能否为无类型项找到一个类型注释，使其成为类型良好的项。

- **后续讨论**：

  - 我们将在第22章详细讨论这个问题，涉及 $\lambda_{\rightarrow}$ 的🥑🥑🥑🥑类型重建（type reconstruction）。

### ------------------------------------------

在类型系统中，**可类型性**（Typability）和**类型重建**（Type Reconstruction）是两个密切相关但又各自有明确含义的概念。它们在编译器实现和编程语言的类型推导过程中扮演重要角色，特别是在静态类型语言中。

### **可类型性（Typability）**

#### **定义：**
- **可类型性**是指给定的表达式是否能够被赋予一个类型，即是否存在某种类型使得这个表达式符合该类型的类型规则。换句话说，**可类型性是判定某个表达式是否具有合法类型**的问题。
- 可类型性问题的核心是：**表达式 $e$ 是否能够被赋予某种类型 $T$** 使得它满足语言的类型规则。
  
#### **形式化描述：**
- 给定一个表达式 $e$，可类型性问题可以被描述为：是否存在类型 $T$ 使得 $e : T$ 成立，即 $e$ 是良类型（well-typed）的。
- 如果对于某个表达式 $e$ 存在类型 $T$，使得编译器可以证明 $e : T$，那么该表达式就是**可类型的**（typable）。
  
#### **重要性：**
- **可类型性**通常用于判断某个程序是否符合编程语言的类型规则。例如，编译器在编译阶段会检查每个表达式是否是可类型的，从而确保程序没有类型错误。
  
#### **举例：**
1. **合法的表达式**：
   ```go
   x := 42  // 这个表达式是可类型的，编译器可以推导出 x 的类型为 int
   ```
   在这个例子中，表达式 `42` 可以被赋予类型 `int`，因此它是**可类型的**。

2. **不合法的表达式**：
   ```go
   x := "hello" + 42  // 这个表达式不可类型，不能将字符串和整数相加
   ```
   在这个例子中，`"hello" + 42` 是不可类型的，因为不能对不同类型的操作数进行加法操作，编译器无法找到一个统一的类型来满足这个表达式的类型规则。

#### **可类型性问题的复杂性：**
- **可类型性问题的复杂性**取决于语言的类型系统。例如，在有些类型系统中，所有表达式都可以是可类型的（如动态类型语言），而在静态类型语言中，类型检查的过程可能较为复杂，尤其是在处理泛型、多态性或递归类型时。

---

### **类型重建（Type Reconstruction）**

#### **定义：**
- **类型重建**是指从未显式标注类型的程序中自动推导出所有表达式的类型。它的目标是通过分析程序的结构和上下文，**重建出所有变量、函数和表达式的类型**。
- **类型重建问题的核心是：给定一个不包含类型注释的表达式，能否推导出其类型**。

#### **形式化描述：**
- **类型重建**问题可以被表述为：在给定表达式 $e$ 和类型上下文 $\Gamma$ 的情况下，求解 $T$ 使得 $\Gamma \vdash e : T$ 成立。
  - $\Gamma$ 是类型上下文，包含已知的类型信息。
  - $e$ 是表达式。
  - $T$ 是要推导出的类型。

#### **类型重建 vs. 类型检查：**
- **类型检查**是验证给定表达式是否符合预期的类型，而**类型重建**则是推导出没有显式类型注释的表达式的类型。两者是不同的概念：
  - **类型检查**：输入的表达式通常带有显式的类型声明，编译器的任务是验证表达式是否符合声明的类型。
  - **类型重建**：输入的表达式没有显式类型声明，编译器的任务是通过推理和推导找出正确的类型。

#### **类型重建算法：**
最著名的类型重建算法是 **Hindley-Milner (HM) 类型推导算法**，其典型的实现为 **Algorithm W**。这种算法允许在不提供类型注释的情况下推导出程序中所有变量和表达式的类型。

1. **初始化**：为每个表达式分配一个类型变量。
2. **生成约束**：通过表达式之间的关系生成类型约束条件。
3. **求解约束**：通过合一算法解决这些约束条件，最终推导出表达式的类型。

#### **举例：**
假设我们有以下表达式：
```go
x := 42
y := x + 10
```
1. 编译器通过类型重建推导出 `x` 的类型为 `int`。
2. 然后，编译器推导出 `y` 的类型为 `int`，因为 `x + 10` 是 `int` 的加法操作。

#### **类型重建在 Go 中的应用：**
在 Go 语言中，局部变量的类型可以通过 `:=` 操作符自动推导，编译器通过**类型重建**推导出右侧表达式的类型并赋给左侧变量。例如：
```go
x := 42  // 类型重建推导出 x 的类型为 int
y := "hello"  // 类型重建推导出 y 的类型为 string
```
Go 编译器无需显式的类型注释就能够重建出变量 `x` 和 `y` 的类型。

#### **类型重建的挑战：**
- **泛型与多态性**：在引入泛型的语言中，类型重建需要处理多态性问题。例如，当一个函数可以接受多种类型参数时，编译器需要推导出所有可能的类型，并确保类型一致性。
- **递归类型**：递归数据结构或递归函数的类型重建也是一个复杂的问题，尤其是在泛型语言中。

---

### **可类型性与类型重建的关系**

1. **类型重建解决可类型性问题**：
   - **类型重建**是通过自动推导来解决**可类型性**问题的一种手段。类型重建算法能够推导出表达式的类型，并验证该表达式是否可类型。
   
2. **可类型性是类型重建的前提**：
   - **可类型性**问题首先判断某个表达式是否能够具有一个类型，若表达式不可类型，则类型重建也无法进行。例如，对于类型错误的表达式，类型重建算法会失败，指出该表达式不可类型。

#### **总结：**
- **可类型性**是一个表达式能否被赋予类型的问题，而**类型重建**则是从未显式标注类型的表达式中推导出类型的过程。
- **类型重建**不仅解决了**可类型性**问题，还提供了从代码中自动推导出正确类型的能力，特别是在没有类型注释的编程语言中（如函数式编程语言和 Go 语言中部分类型推导场景）。

### ------------------------------------------

---

#### 9.6 **Curry 风格 vs. Church 风格**

**概述：**

- **两种语义风格**：

  1. **Curry 风格**：

     - **定义**：先定义项，然后定义它们的语义，最后给出类型系统来拒绝一些我们不喜欢的项。

     - **特点**：语义优先于类型。即使项不是类型良好的，我们也可以讨论其行为。

  2. **Church 风格**：

     - **定义**：先定义项，然后确定类型良好的项，最后只给这些项定义语义。

     - **特点**：类型优先于语义。我们从不问“类型不良的项的行为是什么？”实际上，我们评估的是类型推导，而不是项本身。

---

**区别：**

- **Curry 风格**：

  - 允许讨论所有项的行为，包括类型不良的项。

  - 语义定义在更广泛的项集合上，然后使用类型系统来限制。

- **Church 风格**：

  - 只讨论类型良好的项。

  - 语义仅定义在类型良好的项上。

---

**历史和应用：**

- **隐式类型化的 λ 演算**：

  - 通常以 Curry 风格给出。

- **显式类型化的系统**：

  - Church 风格的表示更为常见。

- **术语混淆**：

  - 有时“Church 风格”被用来描述显式类型化的语法，“Curry 风格”用于隐式类型化。

### --------------------------------

### **Curry 风格 vs. Church 风格**

Curry 风格（Curry-style）和 Church 风格（Church-style）是两种在**类型系统**中表达函数类型和术语类型化的方式，它们最早来源于 λ 演算（Lambda Calculus），并且在函数式编程语言和类型理论中广泛应用。以下是两种风格的详细对比，从多个角度进行分析。

| **维度**                   | **Curry 风格**                                               | **Church 风格**                                              |
| -------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| **术语与类型的关联**       | 在 **Curry 风格**中，术语（terms）**不显式地标注类型**，类型是通过**类型推导**来获得的。 | 在 **Church 风格**中，**每个术语都显式标注类型**，即术语和类型紧密关联，类型是直接通过类型标注提供的。 |
| **术语表示**               | 术语表示没有类型标注。例如，λ 表达式表示为： `λx. x + 1`。类型由上下文推导出。 | 术语显示包含类型标注。例如，λ 表达式表示为： `λx: Int. x + 1`，类型直接体现在术语中。 |
| **类型推导**               | 类型由**类型推导算法**自动推断。编译器通过上下文和规则推导出术语的类型，常用算法如 Hindley-Milner（Algorithm W）。 | 不需要类型推导，**类型显式提供**，每个术语在书写时就包含类型信息，编译器只需要检查类型正确性。 |
| **类型检查**               | 类型检查是**推导过程的一部分**，在类型推导之后进行类型检查。如果类型推导成功，则进行类型检查以验证推导的类型是否正确。 | 类型检查是直接的，因为术语已经标注了类型，编译器只需要验证类型标注是否符合类型规则。 |
| **类型显式性**             | 类型是隐式的，不需要程序员显式注释。编译器会通过术语的结构推导类型。 | 类型是显式的，程序员在编写程序时需要在术语中标注类型。       |
| **灵活性**                 | **灵活**：可以书写未标注类型的表达式，编译器通过推导获得类型。这为简洁的表达式提供了便利，尤其是无需显式声明类型时。 | **较为严格**：每个表达式都必须有明确的类型声明，较为冗长，但能提高代码的明确性和健壮性。 |
| **多态性（Polymorphism）** | **隐式多态**：类型推导可以自动生成多态类型，开发者不需要手动声明多态。 | **显式多态**：多态性需要通过显式的类型标注和量词来表达。开发者必须在术语中声明多态类型，如使用 `forall`。 |
| **类型系统的表现力**       | **类型推导过程可以非常灵活**，允许编译器根据上下文和推导规则自动生成复杂类型推断。 | **类型系统非常显式**且表达力强，特别是在需要明确表达多态、依赖类型等场景下非常有用。 |
| **用户负担**               | **负担较轻**：用户不需要在术语中手动添加类型标注，编写代码更加简洁。 | **负担较重**：用户需要在每个术语中显式标注类型，增加了代码的冗长度，但提高了类型的清晰度。 |
| **常见使用语言**           | 许多支持类型推导的语言采用 **Curry 风格**，如：Haskell、OCaml、F# 等。 | **Church 风格**常见于类型显式要求较高的系统或语言，如：Coq、Agda、MLTT（Martin-Löf Type Theory）。 |
| **错误发现时间**           | 类型推导时可以延迟发现类型错误，可能直到整个推导过程结束时才报错。 | 类型错误更早被发现，因为类型在书写时就明确指定。编译器在检查术语时立刻报告类型错误。 |
| **泛型与多态的处理**       | 支持**自动泛型推导**，比如 Hindley-Milner 类型推导自动推导出函数的泛型类型。 | 泛型和多态性需要**显式声明**，类型中的多态量词必须明确指出，通常会用显式的类型变量或 `forall` 关键字来表示。 |
| **类型安全性**             | **类型安全性较高**：即使没有显式类型注释，类型推导算法能够保证类型安全性，推导出最合适的类型。 | **类型安全性极高**：类型显式声明，每个术语的类型必须直接书写在代码中，确保每个表达式都有正确的类型，从而保证类型系统的健壮性和安全性。 |

---

### **Curry 风格 vs. Church 风格 总结：**

- **Curry 风格**的核心是类型隐式推导，编译器通过类型推导算法自动推断表达式的类型。它适合那些不想显式声明类型的场景，通常用于函数式编程语言，如 Haskell 和 OCaml。它的优点是代码更加简洁灵活，但需要依赖编译器进行类型推导。
  
- **Church 风格**则要求所有术语都显式地带有类型标注，这使得程序的类型更加明确，并且类型检查过程更直接。Church 风格通常在需要明确类型的形式化系统中使用，例如依赖类型和定理证明工具。

### ---------------------------------

---

#### 9.7 **注释** (Notes)

- **简单类型 λ 演算**：

  - 在 Hindley 和 Seldin (1986) 中进行了研究。

  - 在 Hindley 的专著 (1997) 中有更深入的讨论。

- **名言**：

  > **“类型良好的程序不会‘出错’。”** —— Robin Milner (1978)

---

### **总结**

- **Curry-Howard 对应**：

  - 建立了逻辑系统与类型系统之间的深刻联系。

  - 在编程语言理论和逻辑学中都有重要影响。

- **擦除与可类型性**：

  - 类型注释在求值过程中可以被擦除，程序仍然保持相同的行为。

  - 这对于编译器的实现和类型重建有重要意义。

- **Curry 风格与 Church 风格**：

  - 代表了两种定义语言语义和类型系统的不同方法。

  - 选择哪种风格取决于语言设计的需求和目标。

---

**如果您对上述内容有任何疑问，欢迎进一步讨论！**

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