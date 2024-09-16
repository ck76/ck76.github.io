[toc]



# 第16章 递归类型 (Recursive Types)

在本章中，我们将探讨**递归类型 (Recursive Types)** 的概念，它们是解决类型同构等式的一种方式。递归类型允许我们在类型系统中表示自引用的类型，从而能够定义复杂的递归数据结构，如树、列表等。

---

## **16.1 解决类型同构 (Solving Type Isomorphisms)**

### **类型同构 (Type Isomorphism)**

**定义**：两个类型 $\tau_1$ 和 $\tau_2$ 是**同构的 (Isomorphic)**，如果存在两个表达式：

1. $x_1 : \tau_1 \vdash e_2 : \tau_2$
2. $x_2 : \tau_2 \vdash e_1 : \tau_1$

使得它们互为逆函数。

**解释**：这意味着从类型 $\tau_1$ 到 $\tau_2$ 的转换函数 $e_2$，以及从类型 $\tau_2$ 到 $\tau_1$ 的转换函数 $e_1$，它们的组合是恒等函数。

**示例**：自然数类型 $\text{nat}$ 与类型 $\text{unit} + \text{nat}$ 是同构的。这可以通过以下两个表达式证明：

1. **从 $\text{unit} + \text{nat}$ 到 $\text{nat}$ 的转换**：

   $$
   x : \text{unit} + \text{nat} \vdash \text{case } x\ \{\ l \cdot\ \Rightarrow z\ |\ r \cdot x_2 \Rightarrow s(x_2)\ \} : \text{nat}
   $$

   - **解释**：如果 $x$ 是左注入（对应于 $\text{unit}$），则返回零 $z$；如果 $x$ 是右注入（对应于 $\text{nat}$），则应用后继函数 $s$。

2. **从 $\text{nat}$ 到 $\text{unit} + \text{nat}$ 的转换**：

   $$
   x : \text{nat} \vdash \text{ifz } x\ \{\ z \Rightarrow l \cdot \langle\ \rangle\ |\ s(x_2) \Rightarrow r \cdot x_2\ \} : \text{unit} + \text{nat}
   $$

   - **解释**：如果 $x$ 是零，则返回左注入 $l \cdot \langle\ \rangle$；如果 $x$ 是后继 $s(x_2)$，则返回右注入 $r \cdot x_2$。

**结论**：这些转换函数互为逆函数，证明了 $\text{nat}$ 与 $\text{unit} + \text{nat}$ 是同构的。

### **递归类型的引入**

**目标**：解决类型同构等式，例如：

$$
\text{nat} \cong \text{unit} + \text{nat}
$$

**方法**：引入**递归类型** $\mu t.\tau$，作为类型等式 $t \cong \tau$ 的解。

- **$\mu t.\tau$**：表示类型变量 $t$ 的递归类型，其定义为 $\tau$ 中的 $t$ 被自身递归替换。

- **固定点方程**：

  $$
  \mu t.\tau \cong [\mu t.\tau / t]\tau
  $$

  - **$[\mu t.\tau / t]\tau$**：表示在 $\tau$ 中用 $\mu t.\tau$ 替换 $t$。

### **折叠和展开操作**

为了在类型之间进行转换，我们引入了**折叠 (fold)** 和**展开 (unfold)** 操作：

1. **折叠操作 (fold)**：

   $$
   x : [\mu t.\tau / t]\tau \vdash \text{fold}(x) : \mu t.\tau
   $$

   - **解释**：将类型 $[\mu t.\tau / t]\tau$ 的值 $x$ 折叠为递归类型 $\mu t.\tau$。

2. **展开操作 (unfold)**：

   $$
   x : \mu t.\tau \vdash \text{unfold}(x) : [\mu t.\tau / t]\tau
   $$

   - **解释**：将递归类型 $\mu t.\tau$ 的值 $x$ 展开为类型 $[\mu t.\tau / t]\tau$。

**性质**：折叠和展开操作互为逆函数，即：

$$
\text{unfold}(\text{fold}(x)) = x \quad \text{and} \quad \text{fold}(\text{unfold}(x)) = x
$$

### **类型与集合的区别**

**注意事项**：尽管在集合论中，一些类型同构是不可行的（例如，Cantor 定理指出不存在集合 $X$ 使得 $X \cong \mathcal{P}(X)$，其中 $\mathcal{P}(X)$ 是 $X$ 的幂集），但在类型理论中，我们可以通过引入递归类型来解决类型同构等式。

- **原因**：类型描述的是计算，而不是集合。某些计算可能不会终止，因此类型之间的同构不必遵循集合论的限制。

---

## **16.2 递归数据结构 (Recursive Data Structures)**

### **递归类型的定义**

**形式**：递归类型 $\mu t.\tau$，其中 $t.\tau$ 是一个类型算子，表示类型等式 $t \cong \tau$ 的解。

- **解释**：递归类型 $\mu t.\tau$ 是类型等式 $t = \tau$ 的**最小固定点**。

### **折叠和展开操作的类型**

1. **展开操作 (unfold)**：

   $$
   x : \mu t.\tau \vdash \text{unfold}(x) : [\mu t.\tau / t]\tau
   $$

2. **折叠操作 (fold)**：

   $$
   x : [\mu t.\tau / t]\tau \vdash \text{fold}(x) : \mu t.\tau
   $$

### **示例：自然数类型**

**定义自然数类型 $\text{nat}$**：

$$
\text{nat} = \mu t.\ \text{unit} + t
$$

- **解释**：自然数类型 $\text{nat}$ 是类型算子 $t.\ \text{unit} + t$ 的递归类型。

**折叠和展开操作的应用**：

- **折叠操作**：将类型 $\text{unit} + \text{nat}$ 的值折叠为 $\text{nat}$。

- **展开操作**：将 $\text{nat}$ 类型的值展开为 $\text{unit} + \text{nat}$。

**互为逆函数**：折叠和展开操作证明了 $\text{nat} \cong \text{unit} + \text{nat}$。

---

## **语言 $L\{+ \times * \mu\}$ 的定义**

### **扩展的语法**

**类型的抽象语法**：

- **类型变量**：$t$
  
- **递归类型**：$\text{rec}(t.\tau)$ 或 $\mu t.\tau$

- **其他类型构造器**：
  
  - **函数类型**：$\text{arr}(\tau_1; \tau_2)$

**表达式的抽象语法**：

- **折叠操作**：$\text{fold}[t.\tau](e)$ 或简写为 $\text{fold}(e)$

- **展开操作**：$\text{unfold}(e)$

### **静态语义 (Statics)**

#### **类型构造判断**

类型构造判断是形式为：

$$
\Delta \vdash \tau\ \text{type}
$$

其中 $\Delta$ 是类型变量的声明集合，形式为 $t_1\ \text{type},\ \dots,\ t_k\ \text{type}$。

**类型构造规则**：

1. **类型变量**：

   $$
   \frac{
     t\ \text{type} \in \Delta
   }{
     \Delta \vdash t\ \text{type}
   }
   \quad (16.1a)
   $$

2. **函数类型**：

   $$
   \frac{
     \Delta \vdash \tau_1\ \text{type} \quad \Delta \vdash \tau_2\ \text{type}
   }{
     \Delta \vdash \text{arr}(\tau_1; \tau_2)\ \text{type}
   }
   \quad (16.1b)
   $$

3. **递归类型**：

   $$
   \frac{
     \Delta,\ t\ \text{type} \vdash \tau\ \text{type}
   }{
     \Delta \vdash \text{rec}(t.\tau)\ \text{type}
   }
   \quad (16.1c)
   $$

   **解释**：在类型环境 $\Delta$ 中，假设 $t\ \text{type}$，如果 $\tau$ 是一个类型，则 $\text{rec}(t.\tau)$ 是一个类型。

#### **类型判断**

类型判断是形式为：

$$
\Gamma \vdash e : \tau
$$

**类型规则**：

1. **折叠操作的类型规则**：

   $$
   \frac{
     \Gamma \vdash e : [\text{rec}(t.\tau) / t]\tau
   }{
     \Gamma \vdash \text{fold}[t.\tau](e) : \text{rec}(t.\tau)
   }
   \quad (16.2a)
   $$

   **解释**：如果 $e$ 的类型是 $[\text{rec}(t.\tau) / t]\tau$，则 $\text{fold}(e)$ 的类型是 $\text{rec}(t.\tau)$。

2. **展开操作的类型规则**：

   $$
   \frac{
     \Gamma \vdash e : \text{rec}(t.\tau)
   }{
     \Gamma \vdash \text{unfold}(e) : [\text{rec}(t.\tau) / t]\tau
   }
   \quad (16.2b)
   $$

   **解释**：如果 $e$ 的类型是 $\text{rec}(t.\tau)$，则 $\text{unfold}(e)$ 的类型是 $[\text{rec}(t.\tau) / t]\tau$。

### **动态语义 (Dynamics)**

动态语义通过以下规则定义：

1. **折叠操作的值规则**：

   $$
   \text{fold}[t.\tau](e)\ \text{val}
   \quad (16.3a)
   $$

   **解释**：$\text{fold}(e)$ 是一个值。

2. **折叠操作的计算规则**（根据解释策略，可选择性地包括）：

   $$
   \frac{
     e \rightarrow e'
   }{
     \text{fold}[t.\tau](e) \rightarrow \text{fold}[t.\tau](e')
   }
   \quad (16.3b)
   $$

   **解释**：如果 $e$ 计算到 $e'$，则 $\text{fold}(e)$ 计算到 $\text{fold}(e')$。

   - **注意**：这个规则用于**严格求值策略 (Eager Evaluation)**。对于**惰性求值策略 (Lazy Evaluation)**，可以省略此规则。

3. **展开操作的计算规则**：

   $$
   \frac{
     e \rightarrow e'
   }{
     \text{unfold}(e) \rightarrow \text{unfold}(e')
   }
   \quad (16.3c)
   $$

4. **展开和折叠的互逆性**：

   $$
   \frac{
     \text{fold}[t.\tau](e)\ \text{val}
   }{
     \text{unfold}(\text{fold}[t.\tau](e)) \rightarrow e
   }
   \quad (16.3d)
   $$

   **解释**：对折叠的值应用展开操作，得到原始的 $e$。

---

## **定理 16.1 （安全性，Safety）**

### **陈述**

1. **类型保持性 (Preservation)**：如果 $e : \tau$ 且 $e \rightarrow e'$，则 $e' : \tau$。

2. **进展性 (Progress)**：如果 $e : \tau$，则要么 $e$ 是一个值，要么存在 $e'$ 使得 $e \rightarrow e'$。

### **证明思路**

- **类型保持性**：通过对计算规则 (16.3) 进行规则归纳，证明每个计算步骤中表达式的类型保持不变。

- **进展性**：通过对表达式的结构进行归纳，证明对于任意类型正确的表达式 $e$，要么 $e$ 是一个值，要么可以进一步计算。

**结论**：这证明了语言 $L\{+ \times * \mu\}$ 的类型安全性。

---

## **示例解析**

### **示例：定义列表类型**

**目标**：使用递归类型定义一个元素类型为 $A$ 的列表类型 $\text{List}(A)$。

**定义**：

$$
\text{List}(A) = \mu t.\ \text{unit} + (A \times t)
$$

- **解释**：列表类型 $\text{List}(A)$ 是类型算子 $t.\ \text{unit} + (A \times t)$ 的递归类型。

### **操作**

1. **构造空列表**：

   - **表达式**：$\text{fold}(\text{inl}(\langle\ \rangle))$

   - **类型**：$\text{List}(A)$

2. **构造非空列表**：

   - **表达式**：$\text{fold}(\text{inr}(\langle a,\ l \rangle))$

     - **$a$**：类型为 $A$ 的元素

     - **$l$**：类型为 $\text{List}(A)$ 的列表

   - **类型**：$\text{List}(A)$

3. **展开列表**：

   - **表达式**：$\text{unfold}(l)$

   - **类型**：$\text{unit} + (A \times \text{List}(A))$

### **函数定义**

**示例**：定义一个计算列表长度的函数 $\text{length} : \text{List}(A) \rightarrow \text{Int}$。

1. **基础情况**：

   - **当列表为空**：

     $$
     \text{length}(l) = 0 \quad \text{如果 } \text{unfold}(l) = \text{inl}(\langle\ \rangle)
     $$

2. **递归情况**：

   - **当列表为非空**：

     $$
     \text{length}(l) = 1 + \text{length}(l') \quad \text{如果 } \text{unfold}(l) = \text{inr}(\langle a,\ l' \rangle)
     $$

---

## **总结**

- **递归类型**：通过解决类型同构等式，递归类型允许我们在类型系统中表示自引用的类型，支持定义复杂的递归数据结构。

- **折叠 (fold) 和展开 (unfold)**：折叠操作用于将类型表达式的值构造为递归类型的值；展开操作用于将递归类型的值解构为类型表达式的值。它们互为逆函数。

- **类型系统**：在语言 $L\{+ \times * \mu\}$ 中，递归类型的引入扩展了类型系统的表达能力，使其能够表示和处理递归数据结构。

- **动态语义**：通过定义计算规则，确保了计算过程中的类型安全性。

- **类型安全性**：定理 16.1 证明了语言的类型安全性，即计算过程中类型保持，且表达式要么是值，要么可以进一步计算。

---

## **练习与思考**

**练习1**：

- **定义二叉树类型** $\text{Tree}(A)$，并实现一个函数，计算树中节点的数量。

  - **类型定义**：

    $$
    \text{Tree}(A) = \mu t.\ \text{unit} + (A \times t \times t)
    $$

  - **函数**：$\text{size} : \text{Tree}(A) \rightarrow \text{Int}$

    - **定义**：

      - **基础情况**：

        $$
        \text{size}(t) = 0 \quad \text{如果 } \text{unfold}(t) = \text{inl}(\langle\ \rangle)
        $$

      - **递归情况**：

        $$
        \text{size}(t) = 1 + \text{size}(t_1) + \text{size}(t_2) \quad \text{如果 } \text{unfold}(t) = \text{inr}(\langle a,\ t_1,\ t_2 \rangle)
        $$

**练习2**：

- **考虑类型同构** $X \cong (X \rightarrow \text{Bool})$。在集合论中，这是不可能的，但在类型系统中，我们可以定义这样的递归类型。

  - **定义**：$\text{X} = \mu t.\ t \rightarrow \text{Bool}$

  - **思考**：如何定义折叠和展开操作，以及相应的转换函数？

**思考**：

- **类型系统的设计**：在实际编程语言中，引入递归类型需要考虑类型检查的可判定性和类型安全性。如何设计类型系统，以支持递归类型的同时，保持这些性质？

- **递归类型的应用**：递归类型在编程语言中有广泛的应用，例如定义复杂的数据结构、处理无限数据等。如何在实践中有效地使用递归类型，避免潜在的陷阱（如无限递归导致的非终止计算）？

---

**参考文献**：

- **MacLane, S. (1998). Categories for the Working Mathematician. Springer.**

- **Mendler, N. P. (1987). Recursive Types and Type Constraints in Second-Order Lambda Calculus.**

希望以上详细的解释和公式解析能够帮助您深入理解 **第16章 递归类型 (Recursive Types)** 的内容。如有任何疑问，欢迎提问！



### --------------------------------------------------

# **16.2 递归数据结构 (Recursive Data Structures)**

在本节中，我们将探讨**递归类型 (Recursive Types)** 在表示归纳数据类型（如自然数类型）中的重要应用。递归类型允许我们解决类型等式，从而在类型系统中定义自引用的数据结构。

---

## **自然数类型的递归表示**

### **类型等式与同构**

我们可以将自然数类型 $\text{nat}$ 视为以下类型等式的同构（up to isomorphism）的解：

$$
\text{nat} \cong [\text{z} \mapsto \text{unit},\ \text{s} \mapsto \text{nat}]
$$

**解释**：

- **$\text{nat}$**：自然数类型。

- **$[\text{z} \mapsto \text{unit},\ \text{s} \mapsto \text{nat}]$**：表示一个类型构造，其中 $\text{z}$ 对应 $\text{unit}$ 类型，$\text{s}$ 对应 $\text{nat}$ 类型。

- **$\cong$**：表示类型同构，即两个类型在结构上是等价的。

### **自然数类型的递归类型定义**

根据上述同构，每个自然数要么是零，要么是另一个自然数的后继。我们可以通过以下**递归类型 (Recursive Type)** 来给出一个解：

$$
\mu t.\ [\text{z} \mapsto \text{unit},\ \text{s} \mapsto t]. \quad (16.4)
$$

**公式解析**：

- **$\mu t.\ \tau$**：递归类型的表示，表示类型等式 $t = \tau$ 的最小解。

- **$t$**：类型变量，用于自引用。

- **$[\text{z} \mapsto \text{unit},\ \text{s} \mapsto t]$**：类型构造器，表示类型 $t$ 是一个选择类型，要么是 $\text{unit}$，要么是 $t$ 本身。

### **引入形式的定义**

自然数类型 $\text{nat}$ 的**引入形式 (Introductory Forms)** 定义如下：

1. **零的定义**：

   $$
   \text{z} = \text{fold}(\text{z} \cdot \langle\ \rangle)
   $$

   **解释**：

   - **$\text{fold}$**：折叠操作，将类型展开的值折叠回递归类型。

   - **$\text{z} \cdot \langle\ \rangle$**：构造一个标签为 $\text{z}$ 的值，$\langle\ \rangle$ 表示 $\text{unit}$ 类型的值。

2. **后继的定义**：

   $$
   \text{s}(e) = \text{fold}(\text{s} \cdot e)
   $$

   **解释**：

   - **$e$**：类型为 $\text{nat}$ 的自然数。

   - **$\text{s} \cdot e$**：构造一个标签为 $\text{s}$ 的值，携带自然数 $e$。

### **条件分支的定义**

条件分支 $\text{ifz}$ 可定义如下：

$$
\text{ifz } e\ \{\ \text{z} \Rightarrow e_0\ |\ \text{s}(x) \Rightarrow e_1\ \} = \text{case unfold}(e)\ \{\ \text{z} \cdot\ \Rightarrow e_0\ |\ \text{s} \cdot x \Rightarrow e_1\ \},
$$

**解释**：

- **$\text{unfold}(e)$**：展开操作，将递归类型的值展开为其类型表达式。

- **$\text{case}$**：模式匹配操作，根据标签进行分支。

- **$\text{z} \cdot\ \Rightarrow e_0$**：如果展开结果是标签为 $\text{z}$，则执行 $e_0$。

- **$\text{s} \cdot x \Rightarrow e_1$**：如果展开结果是标签为 $\text{s}$，并绑定值为 $x$，则执行 $e_1$。

- **下划线**（**underscore**）表示在 $e_0$ 中不自由出现的变量。

### **验证定义的正确性**

这些定义能够体现预期的行为。通过使用 $\text{fold}$ 和 $\text{unfold}$，我们可以递归地构造和解构自然数，并通过模式匹配实现对自然数的操作。

---

## **列表类型的递归表示**

### **类型等式与同构**

另一个示例是自然数列表（list of natural numbers）的类型。我们可以将列表类型表示为以下递归类型：

$$
\mu t.\ [\text{n} \mapsto \text{unit},\ \text{c} \mapsto \text{nat} \times t]
$$

**因此，我们有类型同构**：

$$
\text{list} \cong [\text{n} \mapsto \text{unit},\ \text{c} \mapsto \text{nat} \times \text{list}]
$$

**解释**：

- **$\text{list}$**：列表类型。

- **$[\text{n} \mapsto \text{unit},\ \text{c} \mapsto \text{nat} \times \text{list}]$**：类型构造器，表示列表要么是空列表（标签为 $\text{n}$），要么是由自然数和列表组成的非空列表（标签为 $\text{c}$）。

### **列表构造操作的定义**

列表的构造操作可以通过以下等式表示：

1. **空列表的定义**：

   $$
   \text{nil} = \text{fold}(\text{n} \cdot \langle\ \rangle)
   $$

   **解释**：

   - **$\text{n} \cdot \langle\ \rangle$**：构造一个标签为 $\text{n}$ 的值，表示空列表。

2. **非空列表的定义**：

   $$
   \text{cons}(e_1;\ e_2) = \text{fold}(\text{c} \cdot \langle e_1,\ e_2 \rangle)
   $$

   **解释**：

   - **$e_1$**：类型为 $\text{nat}$ 的自然数，列表的头部元素。

   - **$e_2$**：类型为 $\text{list}$ 的列表，列表的尾部。

   - **$\text{c} \cdot \langle e_1,\ e_2 \rangle$**：构造一个标签为 $\text{c}$ 的值，包含头部元素和尾部列表。

### **列表的条件分支**

列表形式上的条件分支可定义为：

$$
\text{case } e\ \{\ \text{nil} \Rightarrow e_0\ |\ \text{cons}(x;\ y) \Rightarrow e_1\ \} = \text{case unfold}(e)\ \{\ \text{n} \cdot\ \Rightarrow e_0\ |\ \text{c} \cdot \langle x,\ y \rangle \Rightarrow e_1\ \},
$$

**解释**：

- **$\text{unfold}(e)$**：将列表 $e$ 展开。

- **$\text{case}$**：对展开的结果进行模式匹配。

- **$\text{n} \cdot\ \Rightarrow e_0$**：如果标签为 $\text{n}$（空列表），则执行 $e_0$。

- **$\text{c} \cdot \langle x,\ y \rangle \Rightarrow e_1$**：如果标签为 $\text{c}$（非空列表），并绑定头部为 $x$，尾部为 $y$，则执行 $e_1$。

- **下划线**表示“不关心”的变量，使用模式匹配语法来绑定对的组件。

---

## **求值策略对表示的影响**

当**和类型 (Sum Types)** 和**积类型 (Product Types)** 采用**严格求值策略 (Eager Evaluation)** 时，这种列表的表示与传统的链表“黑板符号”（blackboard notation）有自然的对应关系。

- **解释**：

  - 我们可以将 $\text{fold}$ 视为指向标记单元的抽象堆指针，该单元要么是标签 $\text{n}$（没有关联数据），要么是标签 $\text{c}$，附带一个由自然数和另一个列表组成的对，该列表也必须是相同类型的抽象指针。

- **如果和类型或积类型采用**惰性求值策略 (Lazy Evaluation)**，则黑板符号无法有效表示，因为它无法描绘数据结构中存在的**悬挂计算 (Suspended Computations)**。

- **结论**：一般来说，类型本身是不可替代的。绘图可以提供帮助，但类型决定了语义。

---

## **使用递归类型表示余归纳类型**

### **流类型的递归表示**

我们还可以使用递归类型表示**余归纳类型 (Co-Inductive Types)**，例如自然数的流类型（无限序列）。

当 $\text{fold}(-)$ 采用**惰性求值 (Lazy Evaluation)** 时，表示尤其自然。此时，我们可以将流类型 $\text{stream}$ 定义为以下递归类型：

$$
\mu t.\ \text{nat} \times t
$$

**解释**：

- **$\text{stream}$**：流类型。

- **$\mu t.\ \text{nat} \times t$**：表示每个流可以看作是一个由自然数和另一个流组成的对的计算。

### **严格求值策略下的流类型**

如果 $\text{fold}(-)$ 采用**严格求值 (Eager Evaluation)**，那么我们可以考虑以下递归类型：

$$
\mu t.\ \text{unit} \rightarrow (\text{nat} \times t)
$$

**解释**：

- **$\text{unit} \rightarrow (\text{nat} \times t)$**：表示一个函数，接受 $\text{unit}$ 类型的参数，返回一个由自然数和另一个流组成的对。

- **这种表示方式表达了流的延迟计算**，因为需要调用函数来获取下一个元素。

### **黑板符号的局限性**

无论哪种情况，流都无法容易地用黑板符号表示：

- **原因**：

  - **并非因为流是无限的**，而是因为除了使用编程语言中的表达式外，没有准确的方法来描绘延迟计算（delayed computation）。

- **结论**：再次说明，图示可以提供帮助，但对于准确定义数据结构而言，图示并不充分。

---

## **总结**

- **递归类型**允许我们在类型系统中定义自引用的数据结构，解决类型同构等式。

- **自然数类型和列表类型**可以使用递归类型进行表示，利用 $\text{fold}$ 和 $\text{unfold}$ 操作构造和解构数据。

- **求值策略**对数据结构的表示有影响：

  - **严格求值**时，递归类型与传统的数据结构表示有自然的对应。

  - **惰性求值**时，需要考虑延迟计算，黑板符号可能无法准确描述数据结构。

- **余归纳类型**也可以使用递归类型表示，尤其是在惰性求值策略下，流类型的表示非常自然。

- **类型的重要性**：类型决定了数据结构的语义，绘图和符号只能作为辅助，无法替代类型本身。

---

## **练习与思考**

**练习1**：

- **定义自然数的二进制表示的递归类型**，并实现一个将二进制表示转换为十进制自然数的函数。

  - **类型定义**：

    $$
    \text{BinNat} = \mu t.\ [\text{zero} \mapsto \text{unit},\ \text{double} \mapsto t,\ \text{double\_plus\_one} \mapsto t]
    $$

  - **解释**：

    - **$\text{zero}$**：表示数字 0。

    - **$\text{double}$**：表示当前数字乘以 2。

    - **$\text{double\_plus\_one}$**：表示当前数字乘以 2 加 1。

  - **函数**：$\text{binToNat} : \text{BinNat} \rightarrow \text{nat}$

    - **定义**：

      - **展开 $\text{BinNat}$**，使用模式匹配递归计算对应的自然数。

**练习2**：

- **使用递归类型定义一棵可能无限的树，其中每个节点包含一个自然数**，并实现一个函数，遍历树并收集所有节点的值。

  - **类型定义**：

    $$
    \text{InfTree} = \mu t.\ \text{nat} \times (\text{unit} \rightarrow t) \times (\text{unit} \rightarrow t)
    $$

  - **解释**：

    - **每个节点包含一个自然数**。

    - **左右子树通过函数延迟计算得到，支持无限树的表示**。

  - **函数**：$\text{traverse} : \text{InfTree} \rightarrow \text{List}(\text{nat})$

    - **定义**：

      - **使用惰性求值**，递归遍历树，收集节点值。

**思考**：

- **求值策略的选择如何影响数据结构的表示和操作？**

  - **严格求值**和**惰性求值**在处理递归类型时，各有哪些优缺点？

- **在实际编程语言中，如何利用递归类型和求值策略来表示复杂的数据结构，如无限数据结构或需要延迟计算的数据结构？**

---

希望以上详细的解释和公式解析能够帮助您深入理解 **16.2 递归数据结构 (Recursive Data Structures)** 的内容。如有任何疑问，欢迎提问！





### ----------------------------------------

# **第16章 递归类型 (Recursive Types)**

## **16.3 自引用 (Self-Reference)**

在本节中，我们将探讨递归表达式中的**自引用 (Self-Reference)** 概念，特别是如何使用递归类型来实现**通用递归 (General Recursion)**。自引用允许一个表达式在其自身内部引用自身，从而实现复杂的递归计算。

---

### **一般递归表达式**

在通用递归表达式中，例如 $\text{fix}[\tau](x.e)$，变量 $x$ 代表表达式自身。这通过以下**展开转换 (Unrolling Transition)** 来实现：

$$
\text{fix}[\tau](x.e) \rightarrow [\text{fix}[\tau](x.e)/x]e,
$$

**公式解析**：

- **$\text{fix}[\tau](x.e)$**：表示一个类型为 $\tau$ 的递归函数，其中 $x$ 在表达式 $e$ 中代表函数自身。
- **$[\,\text{fix}[\tau](x.e)/x\,]e$**：在 $e$ 中，将 $x$ 替换为 $\text{fix}[\tau](x.e)$，即表达式自身。
- **$\rightarrow$**：表示计算步骤（转化规则）。

**解释**：在执行过程中，$x$ 被表达式自身替换，从而实现了自引用。这确保了 $x$ 代表表达式自身。

### **自引用变量的理解**

可以将 $x$ 视为 $e$ 的一个**隐式参数 (Implicit Argument)**，每当使用该表达式时，它都会被隐式地实例化为自身。在许多熟知的编程语言中，这个隐式参数有一个特殊的名字，例如 **this** 或 **self**，以强调其自引用的解释。

---

### **从递归类型导出通用递归**

使用上述直觉作为指导，我们可以从**递归类型 (Recursive Types)** 中导出通用递归。这一推导表明，通用递归可以像其他语言特性一样，被视为类型结构的表现，而不是一种特殊的语言特性。

推导的关键在于定义一种**自引用表达式的类型 (Type of Self-Referential Expressions)**，其语法如下：

#### **类型的语法**

$$
\text{Typ}\ \tau ::= \text{self}(\tau)\ \tau\ \text{self}\quad \text{自引用类型 (Self-Referential Type)}
$$

- **$\text{self}(\tau)$**：自引用类型，表示一个类型为 $\tau$ 的自引用表达式。

#### **表达式的语法**

$$
\text{Exp}\ e ::= \text{self}[\tau](x.e)\quad \text{self } x \text{ is } e\quad \text{自引用表达式 (Self-Referential Expression)}
$$

$$
\quad\quad\quad\quad\quad \text{unroll}(e)\quad \text{unroll}(e)\quad \text{展开自引用 (Unroll Self-Reference)}
$$

- **$\text{self}[\tau](x.e)$**：自引用表达式，$x$ 是 $e$ 的自引用。
- **$\text{unroll}(e)$**：展开自引用表达式。

---

### **静态语义 (Statics)**

这些构造的静态语义由以下规则给出：

#### **规则 (16.5a)：自引用表达式的类型规则**

$$
\frac{
  \Gamma,\ x : \text{self}(\tau) \vdash e : \tau
}{
  \Gamma \vdash \text{self}[\tau](x.e) : \text{self}(\tau)
}
\quad (16.5a)
$$

**公式解析**：

- **$\Gamma$**：类型环境。
- **$x : \text{self}(\tau)$**：在类型环境中，$x$ 被赋予类型 $\text{self}(\tau)$。
- **$e : \tau$**：在类型环境 $\Gamma,\ x : \text{self}(\tau)$ 下，表达式 $e$ 的类型为 $\tau$。
- **结论**：$\text{self}[\tau](x.e)$ 的类型为 $\text{self}(\tau)$。

**解释**：如果在 $e$ 中，$x$ 具有自引用类型 $\text{self}(\tau)$，并且 $e$ 的类型为 $\tau$，那么 $\text{self}[\tau](x.e)$ 就是一个类型为 $\text{self}(\tau)$ 的自引用表达式。

#### **规则 (16.5b)：展开自引用表达式的类型规则**

$$
\frac{
  \Gamma \vdash e : \text{self}(\tau)
}{
  \Gamma \vdash \text{unroll}(e) : \tau
}
\quad (16.5b)
$$

**解释**：如果 $e$ 的类型为 $\text{self}(\tau)$，那么 $\text{unroll}(e)$ 的类型为 $\tau$。

---

### **动态语义 (Dynamics)**

动态语义由以下**展开自引用的规则**给出：

#### **规则 (16.6a)：自引用表达式是值**

$$
\text{self}[\tau](x.e)\ \text{val}
\quad (16.6a)
$$

**解释**：$\text{self}[\tau](x.e)$ 是一个值。

#### **规则 (16.6b)：展开操作的计算规则**

$$
\frac{
  e \rightarrow e'
}{
  \text{unroll}(e) \rightarrow \text{unroll}(e')
}
\quad (16.6b)
$$

**解释**：如果 $e$ 计算到 $e'$，那么 $\text{unroll}(e)$ 计算到 $\text{unroll}(e')$。

#### **规则 (16.6c)：展开自引用表达式**

$$
\text{unroll}(\text{self}[\tau](x.e)) \rightarrow [\text{self}[\tau](x.e)/x] e
\quad (16.6c)
$$

**公式解析**：

- **$[\text{self}[\tau](x.e)/x] e$**：在 $e$ 中，将 $x$ 替换为 $\text{self}[\tau](x.e)$。

**解释**：展开自引用表达式时，将表达式自身替换到其主体中，从而实现自引用。

---

### **与通用递归的比较**

主要的区别在于，我们区分了一个自引用表达式的类型 $\text{self}(\tau)$，而不是在每个类型上都强加自引用。然而，正如我们接下来会看到的，自引用类型足以实现通用递归，因此区别主要在于技术上的处理方式。

---

### **使用递归类型定义自引用类型**

正如之前所建议的，我们可以将类型 $\text{self}(\tau)$ 从递归类型中定义出来。关键在于将类型为 $\tau$ 的自引用表达式视为一个以表达式自身为参数的函数。也就是说，我们希望定义类型 $\text{self}(\tau)$，使其满足以下**同构 (Isomorphism)**：

$$
\text{self}(\tau) \cong \text{self}(\tau) \rightarrow \tau.
$$

**解释**：

- **$\text{self}(\tau)$**：自引用类型。
- **$\text{self}(\tau) \rightarrow \tau$**：从自引用类型到 $\tau$ 的函数类型。

这意味着我们在寻找类型算子 $t . t \rightarrow \tau$ 的一个**固定点 (Fixed Point)**，其中 $t \notin \tau$，即 $t$ 是我们所讨论的类型变量，不在 $\tau$ 中出现。

所需的固定点正是**递归类型 (Recursive Type)**：

$$
\text{rec}(t . t \rightarrow \tau),
$$

因此，我们将 $\text{self}(\tau)$ 定义为：

$$
\text{self}(\tau) = \text{rec}(t . t \rightarrow \tau).
$$

---

### **自引用表达式的定义**

自引用表达式 $\text{self}[\tau](x.e)$ 可以定义为：

$$
\text{self}[\tau](x.e) = \text{fold}(\lambda (x : \text{self}(\tau)) . e).
$$

**公式解析**：

- **$\lambda (x : \text{self}(\tau)) . e$**：一个以 $x$ 为参数的函数，$x$ 的类型为 $\text{self}(\tau)$，函数体为 $e$。
- **$\text{fold}$**：折叠操作，将类型展开的值折叠为递归类型。

**验证规则 (16.5a)**：根据此定义，我们可以容易地验证规则 (16.5a)。

### **展开操作的定义**

对应地，展开操作 $\text{unroll}(e)$ 定义为：

$$
\text{unroll}(e) = \text{unfold}(e)(e).
$$

**解释**：

- **$\text{unfold}(e)$**：展开递归类型，得到一个函数，类型为 $\text{self}(\tau) \rightarrow \tau$。
- **$(e)$**：将 $e$ 作为参数，应用于函数 $\text{unfold}(e)$。

**验证规则 (16.5b)**：根据此定义，我们可以验证规则 (16.5b)。

### **展开自引用表达式的计算**

此外，我们可以验证：

$$
\text{unroll}(\text{self}[\tau](y.e)) \rightarrow^* [\text{self}[\tau](y.e)/y]e.
$$

**解释**：

- **$\rightarrow^*$**：表示经过若干步计算。
- 这完成了类型为 $\tau$ 的自引用表达式的类型 $\text{self}(\tau)$ 的推导。

---

### **使用自引用类型定义通用递归**

引入自引用类型 $\text{self}(\tau)$ 的一个结果是，我们可以使用它来为任何类型定义通用递归。

具体来说，我们可以将 $\text{fix}[\tau](x.e)$ 定义为：

$$
\text{fix}[\tau](x.e) = \text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e)).
$$

**公式解析**：

- **$\text{self}[\tau](y.[\text{unroll}(y)/x] e)$**：构造一个自引用表达式，其中 $y$ 是自引用的，$e$ 中的 $x$ 被 $\text{unroll}(y)$ 替换。
- **$\text{unroll}$**：展开操作，将自引用表达式展开。
- **$[\text{unroll}(y)/x] e$**：在 $e$ 中，将 $x$ 替换为 $\text{unroll}(y)$。

**验证静态语义**：可以容易地验证这符合第10章中给出的通用递归的静态语义。

### **验证动态语义**

此外，它也验证了动态语义，如以下推导所示：

$$
\begin{align*}
\text{fix}[\tau](x.e) &= \text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e)) \\
&\rightarrow^* [\text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e))/x] e \\
&= [\text{fix}[\tau](x.e)/x] e.
\end{align*}
$$

**解释**：

- **第一步**：我们展开 $\text{fix}[\tau](x.e)$ 的定义。
- **第二步**：通过展开计算 $\text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e))$，得到 $[\text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e))/x] e$。
- **第三步**：注意到 $\text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e)) = \text{fix}[\tau](x.e)$，因此替换后得到 $[\text{fix}[\tau](x.e)/x] e$。

---

### **递归类型的影响**

由此可见，递归类型可用于为每种类型定义一个**非终止表达式 (Non-Terminating Expression)**，即：

$$
\text{fix}[\tau](x.x).
$$

与我们考虑的许多其他类型构造不同，递归类型改变了**每种类型**的含义，而不仅仅是那些涉及递归的类型。因此，递归类型被认为是对语言（如 $L\{\text{nat} \rightarrow\}$）的**非保守扩展 (Non-Conservative Extension)**，因为它们引入了新的非终止计算，而这些计算在原语言中不存在。

---

## **总结**

- **自引用 (Self-Reference)**：允许表达式在其自身内部引用自身，实现递归计算。
- **自引用类型 $\text{self}(\tau)$**：可以使用递归类型来定义，满足同构 $\text{self}(\tau) \cong \text{self}(\tau) \rightarrow \tau$。
- **通用递归的实现**：通过自引用类型，我们可以实现通用递归，并验证其静态语义和动态语义。
- **递归类型的影响**：递归类型改变了所有类型的含义，引入了新的非终止计算。

---

## **练习与思考**

**练习1**：

- **证明**：使用递归类型定义的 $\text{self}(\tau)$ 满足同构 $\text{self}(\tau) \cong \text{self}(\tau) \rightarrow \tau$。
- **提示**：考虑 $\text{self}(\tau) = \text{rec}(t . t \rightarrow \tau)$，使用展开和折叠操作验证同构。

**练习2**：

- **实现一个函数**，使用自引用类型和通用递归，计算阶乘函数 $\text{fact}(n)$。
- **要求**：定义 $\text{fact}(n)$，使用 $\text{fix}$ 或自引用类型，不使用显式的递归函数定义。

**思考**：

- **递归类型的引入如何影响类型系统的性质**，例如类型检查的可判定性和类型安全性？
- **递归类型是否会引入新的安全隐患**，如非终止计算导致的程序挂起？如何在实践中处理这些问题？

---

希望以上详细的解释和公式解析能够帮助您深入理解 **16.3 自引用 (Self-Reference)** 的内容。如有任何疑问，欢迎提问！


--------------------------------------
# **第16章 递归类型 (Recursive Types)**

## **16.4 状态的起源 (The Origin of State)**

在本节中，我们将探讨计算中的**状态 (State)** 概念的起源，特别是它与递归或自引用的关系。正如我们已经看到的，递归类型引出了自引用的概念，而状态的本质也源于此。

---

### **状态与递归的关系**

**状态 (State)** 是指在计算过程中可变的部分，即系统在某一时刻的信息。状态的概念在第十三部分中将被深入讨论，但它的起源可以追溯到递归或自引用的概念，这正是从递归类型中产生的。

#### **硬件层面的示例：触发器 (Flip-Flop) 或锁存器 (Latch)**

- **触发器 (Flip-Flop)** 和 **锁存器 (Latch)** 是硬件电路中的概念。
  
- **特点**：这些电路由组合逻辑元件（通常是 NOR 门或 NAND 门）构建，具有维持可变状态的特性。

- **行为**：例如，一个 RS 锁存器 (RS Latch) 可以根据 R 或 S 输入信号的变化，保持其输出为逻辑电平 0 或 1，并在一个短暂的稳定延迟后响应。

- **实现方式**：这种行为是通过**反馈 (Feedback)** 实现的，反馈是一种自引用或递归的形式：门的输出反馈到其输入，以便将门的当前状态传递给决定其下一状态的逻辑。

---

### **使用递归类型建模 RS 锁存器**

我们可以使用递归类型来建模 RS 锁存器 (RS Latch)，通过显式地表示时间的流逝，即当前输出是输入和先前输出的函数。

#### **类型定义**

定义一个 RS 锁存器的类型 $\tau_{\text{rsl}}$，如下：

$$
\tau_{\text{rsl}} = \mu t.\ \langle X \mapsto \text{bool},\ Q \mapsto \text{bool},\ N \mapsto t \rangle.
$$

**公式解析**：

- **$\mu t.$**：表示一个递归类型，$t$ 是类型变量。

- **$\langle X \mapsto \text{bool},\ Q \mapsto \text{bool},\ N \mapsto t \rangle$**：一个记录类型，包含三个字段：

  - **$X$**：表示锁存器的当前输出之一，类型为 $\text{bool}$。

  - **$Q$**：表示锁存器的当前状态，类型为 $\text{bool}$。

  - **$N$**：表示锁存器的下一状态，类型为 $t$（递归地引用自身的类型）。

#### **访问器的定义**

如果 $e$ 是类型为 $\tau_{\text{rsl}}$ 的表达式，那么我们定义：

- **$e @ X$**：表示 $\text{unfold}(e) \cdot X$，即展开 $e$，然后获取字段 $X$。

- **$e @ Q$** 和 **$e @ N$** 类似。

**解释**：$e @ X$ 和 $e @ Q$ 计算锁存器 $e$ 的“当前”输出，$e @ N$ 计算另一个锁存器，表示根据“当前”状态确定的“下一”状态。

#### **锁存器的递归函数定义**

对于给定的输入值 $r$ 和 $s$，可以通过递归函数 $\text{rsl}$ 从旧的锁存器计算新的锁存器，定义如下：

$$
\text{fix } \text{rsl} = \lambda (o : \tau_{\text{rsl}}).\ \text{fix this is } e_{\text{rsl}},
$$

其中，$e_{\text{rsl}}$ 定义为：

$$
\text{fold}(\langle X \mapsto \text{nor}(\langle s,\ o @ Q \rangle),\ Q \mapsto \text{nor}(\langle r,\ o @ X \rangle),\ N \mapsto \text{rsl}(this) \rangle)
$$

**公式解析**：

- **$\text{fix } \text{rsl}$**：定义一个递归函数 $\text{rsl}$。

- **$\lambda (o : \tau_{\text{rsl}})$**：$\text{rsl}$ 接受一个类型为 $\tau_{\text{rsl}}$ 的参数 $o$，表示旧的锁存器状态。

- **$\text{fix this is } e_{\text{rsl}}$**：在 $\text{rsl}$ 的定义中，我们使用 $\text{fix}$ 定义了一个自引用的表达式 $this$。

- **$e_{\text{rsl}}$**：构造了一个新的锁存器状态，包括当前输出 $X$、当前状态 $Q$、下一状态 $N$。

- **$\text{nor}$**：一个对布尔值执行 NOR 操作的函数。

- **$o @ Q$** 和 **$o @ X$**：获取旧锁存器的当前状态和输出。

- **$\text{rsl}(this)$**：递归地计算下一状态。

**注意**：为了简化，我们将 $R$ 和 $S$ 输入固定，即当这些输入改变时，我们需要构建一个新的锁存器。可以修改构造，使得在计算锁存器的下一状态时可以提供新的 $R$ 和 $S$ 输入，从而允许这些输入随时间变化。

#### **初始化锁存器**

为了开始构建，我们定义锁存器的初始状态，其中输出被任意地设置为 $\text{false}$，其下一状态通过将 $\text{rsl}$ 应用于初始状态来确定：

$$
\text{fix this is } \text{fold}(\langle X \mapsto \text{false},\ Q \mapsto \text{false},\ N \mapsto \text{rsl}(this) \rangle).
$$

**解释**：

- **$\text{fix this is } \cdots$**：定义一个自引用的初始状态。

- **$\text{fold}(\langle \cdots \rangle)$**：构造锁存器的初始状态。

- **$X$ 和 $Q$**：初始输出和状态，设为 $\text{false}$。

- **$N$**：下一状态，通过应用 $\text{rsl}$ 于 $this$ 计算。

#### **状态的维持**

选择锁存器的 $N$ 组件会导致根据当前输出重新计算输出。请注意，自引用在维持锁存器状态中起到了关键作用。

---

### **显式和隐式的时间建模**

上述锁存器的实现通过提供锁存器的 $N$ 组件，显式地建模了时间，即从当前状态计算下一状态。

也可以**隐式地建模时间**，将锁存器视为一个**转换器 (Transducer)**，其输入和输出是随时间变化的信号。

#### **信号的表示**

- **信号 (Signal)** 可以表示为布尔值的**流 (Stream)**。

- 可以使用第15章中描述的流类型，或本章之前讨论的通用递归类型。

#### **转换器的定义**

- **转换器 (Transducer)** 是一个**流变换器 (Stream Transformer)**，通过将函数应用于输入的连续元素，来计算输出的连续元素。

#### **对比**

- **隐式形式**：更自然地建模时间，信号随着时间变化，转换器处理信号。

- **相似性**：无论是显式建模还是隐式建模，都依赖于递归类型和自引用。

---

## **16.5 注解 (Notes)**

### **递归类型的系统性研究**

- **起源**：对编程中递归类型的系统性研究始于 **Scott (1976, 1982)**，以提供对无类型 $\lambda$-演算的数学模型。

- **递归类型与递归的关系**：从递归类型中推导出递归，实质上是应用了 Scott 的理论，在由递归类型给出的 $\lambda$-演算模型中找到不动点组合子 (Fixed Point Combinator) 的解释。

### **范畴论视角的递归类型**

- **研究者**：Wand (1979) 和 Smyth & Plotkin (1982) 发展了递归类型的范畴论视角。

### **使用自引用实现状态**

- **数字逻辑中的基础**：使用自引用实现状态是数字逻辑的基础。

- **相关研究**：Abadi 和 Cardelli (1996)，Cook (2009) 等人探讨了类似的思想来建模对象。

- **信号作为流的观点**：受 Kahn (MacQueen, 2009) 的开创性工作启发，将信号视为流。

---

## **总结**

- **状态的起源**：状态的概念源于递归或自引用，通过反馈机制实现状态的维持。

- **递归类型的应用**：使用递归类型可以建模硬件电路中的状态，如锁存器。

- **显式 vs. 隐式时间建模**：

  - **显式建模**：通过递归类型的下一状态组件，明确表示时间的流逝。

  - **隐式建模**：将系统视为处理随时间变化的信号的转换器。

- **自引用的重要性**：自引用在维持状态和建模递归计算中起到了关键作用。

---

## **练习与思考**

**练习1**：

- **设计一个锁存器的类型和实现**，允许 $R$ 和 $S$ 输入随时间变化。

  - **提示**：修改 $\text{rsl}$ 的定义，使其接受新的 $R$ 和 $S$ 输入，可能需要将它们表示为信号的流。

**练习2**：

- **使用流和转换器的概念**，实现一个简单的数字滤波器，处理输入信号流，输出处理后的信号流。

  - **提示**：定义一个流变换器，使用递归类型和自引用，实现信号的处理逻辑。

**思考**：

- **递归类型和自引用在现代编程语言中的应用**：

  - **对象模型**：如 Abadi 和 Cardelli 的工作，如何使用递归类型建模对象和类。

  - **并发和分布式系统**：信号作为流的观点如何影响并发系统的设计。

- **状态的管理**：

  - **纯函数式编程 vs. 命令式编程**：如何在纯函数式编程中模拟状态变化。

  - **类型系统的扩展**：如何在类型系统中安全地引入状态和递归类型，保持类型安全性。

---

**参考文献**：

- **Scott, D. (1976, 1982)**：对无类型 $\lambda$-演算的数学模型研究。

- **Wand, M. (1979)**，**Smyth, M. & Plotkin, G. (1982)**：递归类型的范畴论研究。

- **Abadi, M. & Cardelli, L. (1996)**，**Cook, W. (2009)**：使用递归类型和自引用建模对象的研究。

- **MacQueen, D. (2009)**：受 Kahn 的工作启发，将信号视为流。

希望以上详细的解释和公式解析能够帮助您深入理解 **16.4 状态的起源 (The Origin of State)** 和 **16.5 注解 (Notes)** 的内容。如有任何疑问，欢迎提问！

### -------------------------------------------
