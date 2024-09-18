[toc]

### ------------------------------------------------

### **19 System PCF of Recursive Functions**

**PCF（Programming Computable Functions）** 是一种简单的函数式编程语言，它通过递归函数的定义，表达计算的基本模型。PCF 涉及的概念包括静态语义、动态语义、可定义性、有限与无限数据结构，以及递归函数的全局性与部分性。

#### **19.1 Statics （静态语义）**
- **静态语义**定义了程序中表达式的类型，而不涉及其执行过程。PCF 的静态语义主要集中在类型推导的规则上，保证表达式在程序中的类型是合法的。
- PCF 静态语义包括类型检查的规则，如：
  - 常量和基本运算的类型。
  - 函数应用的类型推导。
  - 递归函数定义中的类型规则。
  
  **例子**：一个简单的递归函数：
  ```haskell
  factorial :: Int -> Int
  factorial 0 = 1
  factorial n = n * factorial (n - 1)
  ```
  这个函数在静态语义下，类型推导会验证 `factorial` 的类型是否正确，即它将整数映射为整数。

#### **19.2 Dynamics （动态语义）**
- **动态语义**描述了程序的执行过程，定义了在执行时如何评估表达式。
- 在 PCF 中，动态语义使用的是 **β-简化** 和 **递归求值**，特别强调了对递归函数的计算和如何通过展开递归调用来达到最小结果。

  **例子**：
  - `factorial 3` 的计算过程涉及逐步展开递归调用：
    - `factorial 3 = 3 * factorial 2`
    - `factorial 2 = 2 * factorial 1`
    - `factorial 1 = 1 * factorial 0`
    - `factorial 0 = 1`
    - 合并结果为 `3 * 2 * 1 * 1 = 6`。

#### **19.3 Definability （可定义性）**
- **可定义性**关注哪些函数或值可以在 PCF 中定义。PCF 的一个核心特性是它能够定义任意的递归函数，但只能表达 **可计算函数**。
- PCF 的可定义性依赖于递归函数的表达能力，例如通过固定点算子（如 Y 组合子）实现递归。

  **例子**：递归函数 `factorial` 就是一个典型的可定义函数。

#### **19.4 Finite and Infinite Data Structures （有限与无限数据结构）**
- PCF 支持 **有限数据结构**，如整数、布尔值、有限列表等。
- **无限数据结构** 如无穷流（streams）也可以通过递归定义实现。无穷流可以通过 **惰性求值** 或 **协递归** 实现。

  **例子**：通过协递归定义一个生成自然数的无穷流：
  ```haskell
  nats = 0 : map (+1) nats
  ```

#### **19.5 Totality and Partiality （全局性与部分性）**
- 在 PCF 中，函数可以是**全函数**（对所有输入都有定义）或**部分函数**（对某些输入可能没有定义）。
- 全局性和部分性与递归定义密切相关，递归可能会导致程序的非终止（例如无穷递归），从而表现为部分性。

  **例子**：部分函数的递归调用可能不会终止：
  ```haskell
  f x = f (x + 1)  -- 递归永不终止
  ```

#### **19.6 Notes （注释）**
- PCF 的设计可以看作是理论计算机科学中的一个基础模型，提供了一个简洁的语言来研究递归、计算和定义性问题。

---

### **20 System FPC of Recursive Types**

**System FPC** 扩展了 PCF 系统，添加了对**递归类型**的支持，允许在类型层次上进行递归定义。递归类型使得我们能够定义更加复杂的数据结构和程序行为。

#### **20.1 Solving Type Equations （类型方程的求解）**
- **类型方程**在递归类型中表达递归关系。通过**固定点**技术，类型方程可以得到求解。
- 例如，列表类型可以通过递归类型来定义为：`List a = Unit + (a, List a)`，其中 `Unit` 表示空列表，`(a, List a)` 表示非空列表。

  **例子**：
  ```haskell
  data List a = Nil | Cons a (List a)
  ```

#### **20.2 Inductive and Coinductive Types （归纳类型与余归纳类型）**
- **归纳类型（Inductive Types）**：用于描述**有限数据结构**，通过有限次构造得到结果，例如列表、树等。
- **余归纳类型（Coinductive Types）**：用于描述**无限数据结构**，通过递归或协递归构造的无穷数据结构，如无穷流。

  **例子**：
  - 归纳定义列表：
    ```haskell
    data List a = Nil | Cons a (List a)
    ```
  - 余归纳定义无穷流：
    ```haskell
    data Stream a = Cons a (Stream a)
    ```

#### **20.3 Self-Reference （自引用）**
- **自引用类型**通过**递归类型**的方式表达，允许类型系统支持自引用的数据结构或函数。例如，递归定义的树结构或图结构就是自引用类型的例子。
  
  **例子**：自引用类型用于定义循环的数据结构：
  ```haskell
  data Tree a = Node a (Tree a) (Tree a)
  ```

#### **20.4 The Origin of State （状态的起源）**
- **状态的起源**指的是通过递归类型和自引用定义**状态机**。通过自引用，我们可以描述带有状态的对象，比如对象在不同状态下有不同的行为。
  
  **例子**：状态机可以通过递归类型实现：
  ```haskell
  data State = Idle | Busy State
  ```

#### **20.5 Notes （注释）**
- **System FPC** 的递归类型为复杂数据结构和计算提供了强大的表达能力，特别是在处理无限数据、协递归以及状态机等概念时。
  
---

### **总结与练习**

PCF 和 FPC 系统分别处理了递归函数和递归类型问题。这两个系统为理解**计算理论**、**类型系统**和**递归结构**提供了一个简洁的框架。在计算机科学领域，PCF 用于研究可计算性，FPC 用于研究类型递归与数据结构。

通过 PCF 和 FPC，读者可以更好地理解：
- **静态与动态语义**：如何定义与评估程序。
- **递归定义**：如何使用递归处理无限计算与数据结构。
- **可计算性**：哪些函数和类型可以在这些系统中被定义。

### ------------------------------------------------------------------

### 19 系统 PCF：递归函数系统 (System PCF of Recursive Functions)

在之前的章节中，我们讨论了 T 语言，它用来描述**完全计算** (total computations)，即那些能够保证终止的计算系统。而 M 语言则通过引入归纳类型和余归纳类型来推广 T，但仍然保持了**完全性**。本章中，我们引入了 **PCF** 语言，用来讨论**部分计算** (partial computations)，即使类型检查通过，计算也可能不会终止。

#### 1. **PCF 中的部分性** (Partiality in PCF)

PCF 中的部分性来源于**通用递归** (general recursion) 的概念，它允许在表达式之间求解方程。虽然这赋予了更大的表达能力，但计算可能不会终止，甚至可能是未定义的 (divergent)。因此，在 PCF 中，程序员必须自行保证计算的终止，类型系统不提供终止保证。

例如，考虑定义阶乘函数的方程：
$$
f(0) = 1 \\
f(n + 1) = (n + 1) \times f(n)
$$
直观上，这些方程定义了阶乘函数。它们形成了一个关于未知函数 $f$ 的方程系统，要求找到一个特定的函数 $f : N \to N$ 来满足这些条件。

#### 2. **固定点的引入** (Introduction of Fixed Point)

我们可以将这些方程重写为：
$$
f(n) = \begin{cases} 
1, & \text{if } n = 0 \\
n \times f(n - 1), & \text{if } n > 0 
\end{cases}
$$

从这里可以看到，问题变成了找到一个函数 $f$ 使得 $f = F(f)$，其中 $F$ 是一个**高阶函数** (higher-order function)，定义为：
$$
F(f)(n) = \begin{cases} 
1, & \text{if } n = 0 \\
n \times f(n - 1), & \text{if } n > 0 
\end{cases}
$$

函数 $f$ 的解就是这个函数的**固定点** (fixed point)。因此，我们可以定义函数 $f$ 为 $f = \text{fix}(F)$，其中 $\text{fix}$ 是一个作用于函数 $F$ 的高阶运算符，计算其固定点。

#### 3. **固定点的计算** (Computing Fixed Points)

为什么我们可以找到这样的固定点？关键在于 PCF 中的函数是**部分函数** (partial functions)，这意味着它们可能会在某些输入上发散。通过迭代求解方程，我们可以得到一系列**近似** (approximations)，最终逼近所需的解。我们从完全未定义的部分函数 $\bot$ 开始，逐步改善它：
$$
\bot(n) = \text{undefined for all } n
$$
然后通过不断应用 $F$ 来改进 $\bot$，逐步收敛到目标函数 $f$。

#### 4. **总函数和部分函数** (Total vs. Partial Functions)

在 PCF 中，并不能保证所有递归定义的函数都是**总函数** (total functions)，即在其整个定义域上都有定义。上面的阶乘函数恰好是一个总函数，但这需要通过**归纳法** (induction) 进行证明。然而，PCF 中递归定义的函数在某些输入上可能会发散，因此程序员需要确保所定义的函数是总函数，或者至少要理解它在哪些输入上是定义良好的。

---

通过引入递归和部分性，PCF 语言为我们提供了更强大的表达能力，但代价是计算可能不会终止。因此，程序员需要理解如何合理使用递归，并对其终止性进行充分验证。

### ---------------------------------

### 19.1 静态语义 (Statics)

#### **PCF 语言的语法**

在 PCF (递归函数系统) 中，语法定义如下：

- 类型 (Type) $\tau$ 的定义：
  $$
  \tau ::= \text{nat} \mid \tau_1 \to \tau_2
  $$
  其中，$\text{nat}$ 表示自然数类型，$\tau_1 \to \tau_2$ 表示从类型 $\tau_1$ 到类型 $\tau_2$ 的部分函数类型 (partial function type)。

- 表达式 (Expression) $e$ 的定义：
  $$
  e ::= x \mid z \mid s(e) \mid \text{ifz}\{e_0; x.e_1\}(e) \mid \lambda (x : \tau). e \mid e_1(e_2) \mid \text{fix}\{ \tau \}(x.e)
  $$

  这些表达式包含：
  - $x$：变量 (variable)
  - $z$：零 (zero)，表示自然数 $0$
  - $s(e)$：表示自然数 $e$ 的后继 (successor)
  - $\text{ifz}\{e_0; x.e_1\}(e)$：分支操作 (branching operation)，根据 $e$ 的值是否为零来选择 $e_0$ 或 $e_1$
  - $\lambda (x : \tau). e$：抽象 (abstraction)，表示从 $x$ 到 $e$ 的函数，$x$ 的类型为 $\tau$
  - $e_1(e_2)$：应用 (application)，将表达式 $e_2$ 作为参数应用到表达式 $e_1$
  - $\text{fix}\{ \tau \}(x.e)$：递归 (recursion)，表示递归定义的函数

#### **PCF 的静态语义规则**

PCF 的静态语义通过以下规则递归定义：

1. **变量规则** (Variable Rule)：
   - 若上下文 $\Gamma$ 中，$x$ 的类型为 $\tau$，则 $x$ 的类型为 $\tau$：
     $$
     \Gamma, x : \tau \vdash x : \tau \tag{19.1a}
     $$

2. **自然数零规则** (Zero Rule)：
   - 零 $z$ 的类型为 $\text{nat}$：
     $$
     \Gamma \vdash z : \text{nat} \tag{19.1b}
     $$

3. **后继规则** (Successor Rule)：
   - 若 $e$ 的类型为 $\text{nat}$，则其后继 $s(e)$ 的类型也是 $\text{nat}$：
     $$
     \Gamma \vdash e : \text{nat} \quad \Rightarrow \quad \Gamma \vdash s(e) : \text{nat} \tag{19.1c}
     $$

4. **零测试规则** (Zero Test Rule)：
   - 若 $e$ 的类型为 $\text{nat}$，且 $e_0$ 的类型为 $\tau$，$e_1$ 的类型也是 $\tau$，则 $\text{ifz}\{e_0; x.e_1\}(e)$ 的类型为 $\tau$：
     $$
     \Gamma \vdash e : \text{nat}, \Gamma \vdash e_0 : \tau, \Gamma, x : \text{nat} \vdash e_1 : \tau \quad \Rightarrow \quad \Gamma \vdash \text{ifz}\{e_0; x.e_1\}(e) : \tau \tag{19.1d}
     $$

5. **函数抽象规则** (Function Abstraction Rule)：
   - 若在上下文 $\Gamma$ 中，$x$ 的类型为 $\tau_1$，$e$ 的类型为 $\tau_2$，则 $\lambda(x : \tau_1). e$ 的类型为 $\tau_1 \to \tau_2$：
     $$
     \Gamma, x : \tau_1 \vdash e : \tau_2 \quad \Rightarrow \quad \Gamma \vdash \lambda(x : \tau_1). e : \tau_1 \to \tau_2 \tag{19.1e}
     $$

6. **函数应用规则** (Function Application Rule)：
   - 若 $e_1$ 的类型为 $\tau_2 \to \tau$，$e_2$ 的类型为 $\tau_2$，则应用 $e_1(e_2)$ 的类型为 $\tau$：
     $$
     \Gamma \vdash e_1 : \tau_2 \to \tau, \Gamma \vdash e_2 : \tau_2 \quad \Rightarrow \quad \Gamma \vdash e_1(e_2) : \tau \tag{19.1f}
     $$

7. **递归规则** (Recursion Rule)：
   - 若在上下文 $\Gamma$ 中，假设 $x$ 的类型为 $\tau$，并且 $e$ 的类型也是 $\tau$，则 $\text{fix}\{\tau\}(x.e)$ 的类型为 $\tau$：
     $$
     \Gamma, x : \tau \vdash e : \tau \quad \Rightarrow \quad \Gamma \vdash \text{fix}\{\tau\}(x.e) : \tau \tag{19.1g}
     $$
     这个规则反映了通用递归的自引用性质。在递归定义中，假设递归表达式 $x$ 已经具有类型 $\tau$，然后检查递归体 $e$ 在该假设下是否具有类型 $\tau$。

#### **静态语义的结构性规则** (Structural Rules)

静态语义还包含一些结构性规则，例如**替换规则** (substitution rule)，这些规则是可容许的。例如：

**引理 19.1** (Lemma 19.1)：
如果 $\Gamma, x : \tau \vdash e' : \tau'$ 且 $\Gamma \vdash e : \tau$，那么 $\Gamma \vdash [e/x]e' : \tau'$。这意味着如果 $e$ 具有类型 $\tau$，并且在上下文中 $x : \tau$，则我们可以将 $e$ 替换进 $e'$，保持类型一致性。

---

这些规则定义了 PCF 系统中如何确保表达式在静态检查时具有类型，并且类型系统保证表达式在编写时是类型安全的，尽管它们可能在运行时发散。

### ---------------------------------

### 19.2 动态语义 (Dynamics)

PCF 的动态语义是通过两个判断来定义的：$e \, \text{val}$，用于指定**闭合值** (closed values)，以及 $e \to e'$，用于指定**求值步骤** (steps of evaluation)。

#### **值的定义 (Value Judgment)**

表达式 $e \, \text{val}$ 的判断规则如下：

1. **零值** (Zero Value)：
   - $z$ 是一个值：
     $$
     z \, \text{val} \tag{19.2a}
     $$

2. **后继值** (Successor Value)：
   - 若 $e$ 是一个值，则 $s(e)$ 也是一个值：
     $$
     s(e) \, \text{val} \tag{19.2b}
     $$
     其中，方括号中的前提表示在**急切求值** (eager evaluation) 模式下适用，在惰性求值 (lazy evaluation) 模式下则省略。（详见第 36 章中的讨论。）

3. **抽象值** (Lambda Abstraction Value)：
   - $\lambda (x : \tau). e$ 是一个值：
     $$
     \lambda (x : \tau). e \, \text{val} \tag{19.2c}
     $$

#### **转换规则 (Transition Judgment)**

转换规则 $e \to e'$ 的定义如下：

1. **后继递归求值** (Successor Evaluation)：
   - 若 $e \to e'$，则 $s(e) \to s(e')$：
     $$
     e \to e' \quad \Rightarrow \quad s(e) \to s(e') \tag{19.3a}
     $$

2. **零测试递归求值** (If-Zero Recursive Evaluation)：
   - 若 $e \to e'$，则 $\text{ifz}\{e_0; x.e_1\}(e) \to \text{ifz}\{e_0; x.e_1\}(e')$：
     $$
     e \to e' \quad \Rightarrow \quad \text{ifz}\{e_0; x.e_1\}(e) \to \text{ifz}\{e_0; x.e_1\}(e') \tag{19.3b}
     $$

3. **零测试为零的情况** (If-Zero Case for Zero)：
   - 若 $e = z$，则 $\text{ifz}\{e_0; x.e_1\}(z) \to e_0$：
     $$
     \text{ifz}\{e_0; x.e_1\}(z) \to e_0 \tag{19.3c}
     $$

4. **零测试为后继的情况** (If-Zero Case for Successor)：
   - 若 $s(e)$ 是一个值，则 $\text{ifz}\{e_0; x.e_1\}(s(e)) \to [e/x]e_1$：
     $$
     s(e) \, \text{val} \quad \Rightarrow \quad \text{ifz}\{e_0; x.e_1\}(s(e)) \to [e/x]e_1 \tag{19.3d}
     $$

5. **函数应用递归求值** (Application Recursive Evaluation)：
   - 若 $e_1 \to e_1'$，则 $e_1(e_2) \to e_1'(e_2)$：
     $$
     e_1 \to e_1' \quad \Rightarrow \quad e_1(e_2) \to e_1'(e_2) \tag{19.3e}
     $$

6. **函数参数递归求值** (Application Argument Evaluation)：
   - 若 $e_1$ 是一个值，且 $e_2 \to e_2'$，则 $e_1(e_2) \to e_1(e_2')$：
     $$
     e_1 \, \text{val}, e_2 \to e_2' \quad \Rightarrow \quad e_1(e_2) \to e_1(e_2') \tag{19.3f}
     $$

7. **函数应用规约** (Function Application Reduction)：
   - 若 $e_2$ 是一个值，则 $\text{ap}(\lambda (x : \tau).e_2, e_1) \to [e_1/x]e_2$：
     $$
     e_2 \, \text{val} \quad \Rightarrow \quad \text{ap}(\lambda (x : \tau). e_2, e_1) \to [e_1/x]e_2 \tag{19.3g}
     $$

8. **递归展开规则** (Recursion Unfolding)：
   - $\text{fix}\{ \tau \}(x.e) \to [\text{fix}\{ \tau \}(x.e) / x]e$：
     $$
     \text{fix}\{ \tau \}(x.e) \to [\text{fix}\{ \tau \}(x.e) / x]e \tag{19.3h}
     $$

#### **安全性定理 (Theorem 19.2: Safety)**

**安全性定理**表明了类型系统和动态语义之间的关系：

1. **类型保持性 (Type Preservation)**：  
   如果 $e : \tau$ 且 $e \to e'$，那么 $e' : \tau$。
   
2. **前进性 (Progress)**：  
   如果 $e : \tau$，则 $e$ 要么是一个值 $e \, \text{val}$，要么存在 $e'$ 使得 $e \to e'$。

**证明：**

- **类型保持性**的证明使用对转换判断的推导进行归纳。例如，考虑规则 (19.3h)，假设 $\text{fix}\{\tau\}(x.e) : \tau$，通过反演和替换可以得到 $[\text{fix}\{\tau\}(x.e) / x]e : \tau$，满足类型要求。
  
- **前进性**的证明使用对类型判断推导的归纳。例如，对于规则 (19.1g)，通过展开递归表达式可以推进求值。

这个定理的一个重要结果是，如果 $e \, \text{val}$，那么 $e$ 是不可归约的，即不存在 $e'$ 使得 $e \to e'$。

### ---------------------------------

### 定理 19.2 (安全性定理)
#### 1. **类型保持性** (Type Preservation)
如果 $e : \tau$ 并且 $e \to e'$\ (即表达式 $e$ 经过一次转换得到了 $e'$)，那么 $e'$ 也具有类型 $\tau$。换句话说，表达式在动态求值过程中不会改变其类型。

#### 2. **前进性** (Progress)
如果 $e : \tau$，那么 $e$ 要么是一个值 $e \, \text{val}$，要么存在 $e'$ 使得 $e \to e'$。这意味着，如果一个表达式是闭合且类型正确的，它要么已经是一个值，要么可以继续被求值。

### 证明
- **类型保持性证明**：通过对转换推导过程的归纳证明。考虑规则 (19.3h)：
  - 假设 $\text{fix}\{\tau\}(x.e) : \tau$，通过反演(对表达式结构进行分析)和替换，可以得到 $[\text{fix}\{\tau\}(x.e)/x]e : \tau$。这符合类型规则的要求，即通过递归固定点操作生成的表达式保持了类型。

- **前进性证明**：通过对类型判断推导过程进行归纳证明。例如，考虑规则 (19.1g)，通过展开递归表达式，能够证明可以继续求值，即表达式能够“前进”或“执行”到下一步。

### 进一步的讨论

如果 $e \, \text{val}$，那么 $e$ 是不可归约的（即没有 $e'$ 使得 $e \to e'$）。安全性定理的一个重要推论是，对于一个闭合且类型正确的不可归约表达式，它一定是一个值。这一特性确保了求值过程的安全性——不会有错误地无法继续求值的表达式。

### 定义等价性 (Definitional Equality)
对于 PCF 中的按名字调用 (call-by-name) 变体，定义等价性 $\vdash e_1 \equiv e_2 : \tau$ 是包含以下公理的最强等价关系：

1. 对于零值的条件判断：
   $$
   \vdash \text{ifz}\{e_0; x.e_1\}(z) \equiv e_0 : \tau \tag{19.4a}
   $$

2. 对于后继值的条件判断：
   $$
   \vdash \text{ifz}\{e_0; x.e_1\}(s(e)) \equiv [e/x]e_1 : \tau \tag{19.4b}
   $$

3. 对于递归固定点：
   $$
   \vdash \text{fix}\{\tau\}(x.e) \equiv [\text{fix}\{\tau\}(x.e)/x]e : \tau \tag{19.4c}
   $$

4. 对于函数应用：
   $$
   \vdash \text{ap}(\lambda\{\tau_1\}(x.e_2); e_1) \equiv [e_1/x]e_2 : \tau \tag{19.4d}
   $$

这些规则保证了任意闭合的自然数类型表达式可以通过这些等价规则求出其最终值。具体来说，如果 $e : \text{nat}$，那么 $e \equiv n : \text{nat}$ 当且仅当 $e \to^* n$。

### ---------------------------------

### 19.3 可定义性 (Definability)

在这一节中，我们讨论递归函数 (recursive function) 的定义和在 PCF (Partial Computation Functions) 中的可定义性。

#### 递归函数的定义
我们可以通过递归函数定义一个函数，形式如下：
$$
\text{fun } x(y: \tau_1): \tau_2 \text{ is } e
$$
其中，$e: \tau_2$ 是递归函数的函数体，在这个函数体内绑定了两个变量：$y: \tau_1$ 表示函数的参数，$x: \tau_1 \to \tau_2$ 表示递归函数自身。对于这个递归函数的动态语义 (dynamic semantics)，我们有以下公理：

$$
(\text{fun } x(y: \tau_1): \tau_2 \text{ is } e)(e_1) \to [\text{fun } x(y: \tau_1): \tau_2 \text{ is } e, e_1/x, y]e
$$

这意味着，当我们应用一个递归函数时，我们将递归函数本身替换为 $x$，并将参数替换为 $y$，代入函数体 $e$ 中进行计算。

#### 在 PCF 中定义递归函数

在 PCF 中，递归函数是通过以下形式定义的：

$$
\text{fix } x : \tau_1 \to \tau_2 \text{ is } \lambda (y : \tau_1) e
$$

这个表达式表示的含义与上面提到的递归函数定义相同。可以很容易地验证，递归函数的静态语义和动态语义可以从这种定义中推导出来。

#### 原始递归的定义 (Primitive Recursion)

T 语言中的原始递归构造可以通过 PCF 中的递归函数来定义，具体写法为：

$$
\text{rec } e \{ z \to e_0 \, | \, s(x) \text{ with } y \to e_1 \}
$$

该表达式等价于应用一个通用的递归函数 $e'$，其中 $e'$ 是定义为：

$$
\text{fun } f(u: \text{nat}): \tau \text{ is } \text{ifz } u \{ z \to e_0 \, | \, s(x) \to [f(x)/y]e_1 \}
$$

这里使用了递归函数的结构来处理自然数的原始递归操作。

#### PCF 中定义的函数是部分函数 (Partial Functions)

在 PCF 中定义的函数通常是部分函数 (partial function)，即它们对于某些参数可能是未定义的。一个部分数学函数 $\phi: \mathbb{N} \to \mathbb{N}$ 在 PCF 中是可定义的，当且仅当存在一个表达式 $e_{\phi}: \text{nat} \to \text{nat}$ 使得 $\phi(m) = n$ 当且仅当 $e_{\phi}(m) \equiv n : \text{nat}$。

例如，如果 $\phi$ 是一个完全未定义的函数，那么对应的 $e_{\phi}$ 可以是任意一个当被应用时进入无限循环而不返回值的函数。

#### 最小化操作 (Minimization) 和部分递归函数 (Partial Recursive Functions)

部分递归函数是通过原始递归函数再加上**最小化操作 (minimization)** 来定义的。给定函数 $\phi(m, n)$，我们可以定义 $\psi(n)$ 为满足以下条件的最小值 $m \geq 0$：
1. 对于所有 $m' < m$，$\phi(m', n)$ 是已定义的并且不为零。
2. $\phi(m, n) = 0$。

如果不存在这样的 $m$，那么 $\psi(n)$ 是未定义的。这种定义方式确保了部分函数的可表达性，并展示了递归与部分计算的强大表达能力。

### ---------------------------------

### 定理 19.3：自然数上的部分函数在 PCF 中可定义，当且仅当它是部分递归函数 (partial recursive function)。

#### 证明思路

**证明步骤：**

1. **PCF 至少与部分递归函数同样强大：**  
   在 PCF 中，最小化操作 (minimization) 是可定义的，这意味着 PCF 至少与部分递归函数的集合一样强大。部分递归函数包括原始递归函数 (primitive recursive functions) 以及使用最小化操作构造的函数，因此 PCF 可以表达这些函数。

2. **PCF 不超过部分递归函数的能力：**  
   反过来，我们可以通过构造一个基于 **哥德尔编码 (Godel-numbering)** 的评估器 (evaluator) 来证明，PCF 中的每个表达式都可以用部分递归函数来表示。哥德尔编码可以将表达式映射为自然数，因此我们可以使用部分递归函数来模拟 PCF 表达式的动态语义。因此，PCF 的功能集不会超出部分递归函数的能力范围。

3. **教会定律 (Church's Law)：**  
   教会定律指出，部分递归函数集与自然数上的可有效计算函数集是等价的，也就是说，它们包括所有可以通过任何编程语言（无论当前存在或将来定义的语言）编写的程序所能实现的函数。因此，PCF 的能力与任何编程语言在自然数上可定义的函数集同样强大。

#### PCF 的通用函数 $\phi_{\text{univ}}$ (Universal Function)
在 PCF 中，通用函数 $\phi_{\text{univ}}$ 是自然数上的部分函数，定义如下：

$$
\phi_{\text{univ}}(\langle e \rangle)(m) = n \quad \text{当且仅当} \quad e(m) \equiv n : \text{nat}.
$$

解释：  
通用函数 $\phi_{\text{univ}}$ 实质上是一个解释器，它接收某个表达式 $e$ 的编码 $\langle e \rangle$ 和自然数 $m$ 作为输入，模拟 PCF 的动态语义 (dynamic semantics) 来计算结果。如果该表达式 $e$ 在 $m$ 上的计算成功终止并返回自然数 $n$，则 $\phi_{\text{univ}}(\langle e \rangle)(m) = n$。然而，由于部分函数的特性，$\phi_{\text{univ}}$ 对某些输入可能是未定义的（即计算可能不会终止）。

#### 与 T 语言的比较
根据教会定律，通用函数 $\phi_{\text{univ}}$ 在 PCF 中是可定义的。然而，在第9章中我们已经证明，类似的通用函数在 T 语言中是不可定义的，这通过对角化 (diagonalization) 技术得出。  
在 PCF 中，通过对表达式 $e$ 的自应用 (self-application) 可以得到以下等价式：

$$
e(\langle e \rangle) \equiv s(e(\langle e \rangle))
$$

在 T 语言中，这将导致得出通用函数不存在的结论。然而，在 PCF 中，我们得出的结论是通用函数 $\phi_{\text{univ}}$ 在自应用时会发散 (diverge)，即对自身代码的应用将导致递归而不终止。

因此，PCF 通过允许部分计算扩展了其表达能力，并能够定义部分递归函数，而在 T 语言中，所有函数都必须是全局定义的 (total)，即对所有输入都终止。

### ---------------------------------

### 19.4 有限与无限数据结构

**有限数据类型 (Finite Data Types)：**
在 PCF 中，有限数据类型（如乘积类型 (products) 和和类型 (sums)）及其在模式匹配和泛型编程中的使用可以直接沿用。  
但是，在 PCF 中，急性 (eager) 和惰性 (lazy) 的动态语义 (dynamics) 区别变得更加重要。选择急性或惰性动态语义不仅仅是偏好的问题，而是直接影响程序的含义。相同类型在惰性动态语义下和急性动态语义下的含义可能完全不同。例如：

- **急性语言 (Eager Language)：**  
  在急性语言中，乘积类型 (product type) 的元素是组成类型的值的有序对 (pair of values)。
  
- **惰性语言 (Lazy Language)：**  
  在惰性语言中，乘积类型的元素是组成类型的未计算的（可能会发散的）计算 (unevaluated computations) 的有序对。

这种差异也适用于和类型 (sums)。

**无限数据类型 (Infinite Data Types)：**
对于无限数据类型（如自然数类型 $nat$），这种区别变得更加显著。

#### 急性动态语义下的自然数类型 (Natural Numbers in Eager Dynamics)
在急性动态语义下，自然数类型 $nat$ 是**真正的自然数类型**，其定义是：
- 包含零 ($0$)。
- 闭合于继承者操作 (successor)。

数学归纳法 (mathematical induction) 对急性动态语义下的自然数类型是有效的，这与第 15 章定义的归纳类型 (inductive type) 一致。

#### 惰性动态语义下的自然数类型 (Natural Numbers in Lazy Dynamics)
然而，在惰性动态语义下，自然数类型 $nat$ 已经不再是传统意义上的自然数类型。例如，在惰性环境下，$nat$ 类型包含以下值：

$$
\omega = \text{fix} \, x : \text{nat} \, \text{is} \, s(x)
$$

**解释：**
- 这里的 $\omega$ 是一个具有自身作为前驱的值，这意味着它是一个**无限的继承者堆栈**，不断地自我递归增长，永远不会结束。
- 显然，$\omega$ 并不是一个自然数，因为它比所有自然数都要大。因此，数学归纳法不再适用于惰性动态语义下的 $nat$ 类型。

在惰性环境中，我们可以将 $nat$ 类型重新命名为 $lnat$（惰性自然数类型），以提醒我们它与传统的自然数类型不同。这与第 15 章中定义的协递归类型 (coinductive type) $conat$ 类似，代表的是潜在的无限数据结构。

### 小结：
急性动态语义和惰性动态语义下的相同类型可能代表非常不同的含义：
- **急性动态语义：** 自然数类型是标准的归纳类型，自然数的数学归纳法是适用的。
- **惰性动态语义：** 自然数类型可能包含无限数据结构，传统的数学归纳法不适用。

### ---------------------------------

### 19.5 全函数与部分函数 (Totality and Partiality)

**全函数编程语言 (Total Programming Languages)：**  
像语言 T 这样的全函数编程语言具有一个显著的优点：通过类型检查可以确保每个程序都会终止，并且每个函数都是**全函数 (Total Function)**。全函数意味着对于给定的输入，函数总是有定义的输出。  
在 T 中，类型检查会禁止程序进入无限循环。这个限制听起来非常有吸引力，因为它消除了潜在的无限循环错误。然而，当我们实际编写程序时，事情变得更复杂。

**部分函数编程语言 (Partial Programming Languages)：**  
在部分函数语言 PCF 中，我们允许通过一般递归 (general recursion) 来解决方程，但代价是程序可能不会终止。这种允许无限循环的特性看起来像是一个问题，但它实际上带来了更多的灵活性和表达能力。

### 示例：最大公约数计算
考虑用 PCF 编程计算两个自然数的**最大公约数 (Greatest Common Divisor, gcd)**。我们可以用以下递归方程来定义 gcd：

$$
gcd(m, 0) = m
$$

$$
gcd(0, n) = n
$$

$$
gcd(m, n) = gcd(m - n, n) \quad \text{if} \, m > n
$$

$$
gcd(m, n) = gcd(m, n - m) \quad \text{if} \, m < n
$$

在 PCF 中，gcd 的类型为 $(nat \times nat) \to nat$，这意味着它对于某些输入可能不会终止。然而，通过对参数之和进行归纳 (induction) 可以证明该函数实际上是一个**全函数**，它对于所有输入都能终止。

### 在 T 中编程 gcd 的挑战
现在考虑在语言 T 中编写这个函数。在 T 中，由于只允许使用**原始递归 (Primitive Recursion)**，实现 gcd 变得非常困难。  
T 中的递归要求在每次递归调用时必须将自然数减少 1，这意味着只能递归调用**直接前驱 (Immediate Predecessor)**。这使得编写像 gcd 这样复杂的递归变得非常麻烦和低效。

为了实现更一般的递归模式，必须将它们编码为 T 中的原始递归。然而，这种编码的代价很高，带来了以下问题：

- **程序复杂性 (Program Complexity)：** 编写更复杂的递归会导致程序的复杂性增加，虽然可以通过建立库来减少单个程序的复杂度，但从全局来看仍然是一个挑战。
- **性能问题 (Performance Issues)：** 在更复杂的递归中，程序内部必须维护一个递减的“计时器”，确保程序最终会终止。这种编码方式会导致程序运行速度较慢。

因此，尽管全函数语言提供了终止保证，但其性能和编程难度可能会比允许部分函数的语言高得多。

### ---------------------------------

### 总结与讨论：T 语言与部分语言的限制

虽然全函数语言 (Total Programming Languages) 像 T 这样的语言能够保证程序总是终止（不会出现无限循环），但其限制也十分明显。尤其是：

1. **普遍性 (Universality) 的限制：**  
   全函数语言**不是通用的**。这意味着我们无法在 T 语言内编写 T 语言的解释器 (Interpreter)。不仅仅是 T，这一限制对所有全函数语言都适用。这意味着任何全函数语言都无法自我解释，这对很多程序设计任务（例如编写解释器或编译器）来说是一个很大的限制。

2. **Blum 尺寸定理 (Blum Size Theorem, BST)：**  
   **Blum 尺寸定理**揭示了另一个关于全函数语言的限制。定理表明，在一个全函数语言 L 中，存在一个对自然数的全函数，它在 L 中的最短程序要比在部分语言（例如 PCF）中实现该函数的最短程序长得多。

   Blum 尺寸定理的关键在于，在全函数语言中，**程序的终止性证明必须嵌入到代码中**。相比之下，部分语言则不要求程序自身嵌入终止性证明，程序员可以在外部验证程序是否会终止。这意味着在全函数语言中，某些复杂的终止性证明可能需要大量的代码来表示，从而使得程序长度急剧增加。

### 示例：程序复杂度与终止性证明

我们可以通过一个简单的例子来理解这种复杂性：

- 在 T 语言中，终止性证明是嵌入到代码中的，例如使用**原始递归 (Primitive Recursion)** 来确保程序终止。
- 但是，对于某些复杂的递归模式（例如用二进制数表示的自然数，并且递归调用是通过二分法进行的），在 T 语言中实现这些算法时会遇到很大的挑战，因为我们需要明确地编码递减递归步骤。

**Blum 尺寸定理**告诉我们，对于某些复杂的函数，嵌入终止性证明可能导致程序变得非常冗长。例如，如果我们选择一个增长因子 $22^n$，那么 BST 定理告诉我们在全函数语言 L 中，某些函数的最短实现可能比在部分函数语言 PCF 中实现时长得多。

### 结论

因此，全函数语言虽然能提供终止性保证，但这也带来了代价：

- 程序可能会变得冗长且复杂，因为它们必须明确嵌入终止性证明。
- 一些程序的编写难度和执行效率可能较低。

相比之下，部分函数语言（如 PCF）则提供了更大的灵活性，允许程序员在外部验证程序是否终止，而不必将终止性证明嵌入到程序本身中。这种灵活性使得部分函数语言更具表达力，能够更简洁地实现复杂的递归和算法。

### ---------------------------------

### 19.6 笔记

这一节讨论了**递归方程 (Recursion Equations)** 的解决方案，该方案基于**Kleene 不动点定理 (Kleene’s Fixed Point Theorem)**，特别应用于部分函数的近似顺序。Kleene 不动点定理指出，对于一个连续的单调函数，其不动点可以通过递增序列的极限来获得，这一结果在部分递归函数的语义分析中起到了关键作用。

#### 关键概念：
- **Kleene 不动点定理 (Kleene's Fixed Point Theorem)**：它是用来解决递归定义的一个核心工具，尤其适用于描述部分函数的递归行为。通过不断应用递归函数，逐步逼近最终的解。
- **部分函数 (Partial Functions)**：指的是那些对于某些输入可能不终止或者没有定义的函数。PCF 语言中的函数可以是部分的，这与全函数语言 (Total Functions) 有所不同。

#### PCF 语言的起源和作用：
PCF (**Programming Computable Functions**) 语言来源于 Plotkin 在 1977 年的工作，它被设计为研究编程语言语义的实验室工具。PCF 语言已经成为许多语义学研究的主要对象，被用于探索编程语言的各种语义问题。因此，PCF 被比作编程语言研究中的“大肠杆菌 (E. coli)”——就像生物学中研究细胞生物学时经常用 E. coli 作为模型生物一样，PCF 也成为了编程语言语义学研究中的模型语言。

#### 教学与研究价值：
PCF 语言不仅简单而且具备强大的表达能力，因此它被广泛用于研究与推导编程语言的语义理论。在研究中，PCF 常用于分析和验证编程语言中的递归、部分性以及计算终止性等问题。

### 进一步讨论：
- **Church 定律 (Church's Law)**：在以后的章节中会详细讨论，特别是其与可计算函数的关系。Church 定律表明，所有可计算的自然数函数都可以通过部分递归函数来定义。

通过 PCF 语言，我们可以探讨编程语言语义中的许多基础问题，例如如何有效地表示递归函数、部分函数的定义以及如何通过不动点定理来解决递归定义的问题。


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