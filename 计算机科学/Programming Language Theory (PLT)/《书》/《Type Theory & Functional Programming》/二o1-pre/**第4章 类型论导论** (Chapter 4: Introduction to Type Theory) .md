[toc]



# **第 4 章：类型理论简介**

---

### **概述**

本章是整本书的核心，结合了**逻辑**、**函数式编程**和**构造性数学**这三个主题，将它们融入到一个统一的系统中。在接下来的章节中，我们将对这个系统进行研究、发展和批判。

在之前对**构造性数学**的简短讨论中，我们引入了这样一个观点：**证明应该具有计算内容**。为了实现这一目标，系统的底层逻辑需要改变，只有当我们有了一个命题的证明时，才断言该命题的有效性。

因此，我们定义的系统与第一章中的系统不同，它产生的陈述形式为：

- “**p 是命题 P 的证明**”

我们将其记为：

$$
p : P
$$

**类型理论的核心是**：**命题与类型的对偶性，证明与元素的对偶性**。一个命题 $T$ 的证明可以看作类型 $T$ 的一个元素，反之亦然。来自其中一方的思想可以在另一方重新解释，丰富了两个领域的内容。

我们首先将类型理论作为一个逻辑系统来介绍，然后重新解释推导规则，将其视为**类型化函数式语言中程序构造的规则**，其中 $p : P$ 被理解为：

- “**p 是类型 P 的一个成员**”

---

### **4.1 命题逻辑：非正式概述**

在深入讨论类型理论之前，我们需要对**命题逻辑**有一个非正式的理解，特别是关于一个公式的证明意味着什么。

#### **什么是证明？**

- **证明**是对命题有效性的论证，展示了命题为何成立。

- 在构造性数学中，证明不仅仅是证明一个命题为真，还需要提供一个**构造性的方法**，展示如何构造满足命题的对象。

#### **逻辑连接词**

- **合取（Conjunction）**：$A \wedge B$，表示 $A$ 和 $B$ 都成立。

- **析取（Disjunction）**：$A \vee B$，表示 $A$ 或 $B$ 成立。

- **蕴含（Implication）**：$A \Rightarrow B$，表示如果 $A$ 成立，则 $B$ 成立。

- **否定（Negation）**：$\lnot A$，表示 $A$ 不成立。

#### **构造性解释**

- **合取**：要证明 $A \wedge B$，需要分别证明 $A$ 和 $B$。

- **析取**：要证明 $A \vee B$，需要证明 $A$ 或 $B$ 中的一个，并且知道哪个被证明了。

- **蕴含**：要证明 $A \Rightarrow B$，需要提供一个将 $A$ 的证明转换为 $B$ 的证明的函数。

- **否定**：$\lnot A$ 可以理解为 $A \Rightarrow \bot$，其中 $\bot$ 表示矛盾。

---

### **4.2 判断、证明和推导**

在类型理论中，我们需要区分以下三个概念：

1. **判断（Judgement）**：表示我们知道的事实，例如“$p$ 是命题 $P$ 的证明”。
   
2. **证明对象（Proof Object）**：证明的实际构造，例如函数、算法等。

3. **推导（Derivation）**：从假设出发，通过应用规则，得到判断的过程。

#### **判断的形式**

- 基本判断的形式：

  - **类型判断**：$A\ \text{type}$，表示 $A$ 是一个类型。

  - **元素判断**：$a : A$，表示 $a$ 是类型 $A$ 的一个元素。

  - **类型等式判断**：$A = B\ \text{type}$，表示类型 $A$ 与类型 $B$ 相等。

  - **元素等式判断**：$a = b : A$，表示在类型 $A$ 中，元素 $a$ 与元素 $b$ 相等。

#### **推导规则**

- 推导规则用于从已知的判断推导出新的判断。

- **上下文（Context）**：在推导过程中，我们有一个上下文 $\Gamma$，包含当前已知的假设和变量的类型。

---

### **4.3 命题演算的规则**

在这一节中，我们将正式介绍命题演算的规则，这些规则定义了如何构造证明，以及如何从已有的证明中得出新的证明。

#### **逻辑连接词的引入和消除规则**

1. **合取（Conjunction）**：

   - **引入规则（Introduction Rule）**：

     $$
     \frac{A\ \text{true} \quad B\ \text{true}}{A \wedge B\ \text{true}}
     $$

     - **解释**：如果我们有 $A$ 的证明和 $B$ 的证明，那么可以得到 $A \wedge B$ 的证明。

   - **消除规则（Elimination Rule）**：

     $$
     \frac{A \wedge B\ \text{true}}{A\ \text{true}}
     \quad
     \frac{A \wedge B\ \text{true}}{B\ \text{true}}
     $$

     - **解释**：从 $A \wedge B$ 的证明中，可以分别得到 $A$ 的证明和 $B$ 的证明。

2. **析取（Disjunction）**：

   - **引入规则**：

     $$
     \frac{A\ \text{true}}{A \vee B\ \text{true}}
     \quad
     \frac{B\ \text{true}}{A \vee B\ \text{true}}
     $$

     - **解释**：如果我们有 $A$ 的证明，可以得到 $A \vee B$ 的证明；同理对于 $B$。

   - **消除规则**：

     $$
     \frac{A \vee B\ \text{true} \quad A \Rightarrow C\ \text{true} \quad B \Rightarrow C\ \text{true}}{C\ \text{true}}
     $$

     - **解释**：如果 $A \vee B$ 成立，且 $A$ 和 $B$ 都可以推出 $C$，那么 $C$ 成立。

3. **蕴含（Implication）**：

   - **引入规则**：

     $$
     \frac{\Gamma, A\ \vdash\ B\ \text{true}}{\Gamma\ \vdash\ A \Rightarrow B\ \text{true}}
     $$

     - **解释**：如果在假设 $A$ 下可以推出 $B$，那么可以得到 $A \Rightarrow B$ 的证明。

   - **消除规则（即“**modus ponens**”）**：

     $$
     \frac{A \Rightarrow B\ \text{true} \quad A\ \text{true}}{B\ \text{true}}
     $$

     - **解释**：如果 $A \Rightarrow B$ 成立，且 $A$ 成立，那么 $B$ 成立。

4. **否定（Negation）**：

   - **定义**：$\lnot A \equiv A \Rightarrow \bot$，其中 $\bot$ 表示矛盾。

---

### **4.4 Curry-Howard 等价**

**Curry-Howard 等价**是类型理论中的一个重要概念，它建立了逻辑和类型系统之间的对应关系：

- **命题对应于类型**。

- **证明对应于类型的元素（程序）**。

#### **对应关系**

- **合取**（$A \wedge B$）对应于**乘积类型**（$A \times B$）。

- **析取**（$A \vee B$）对应于**和类型**（$A + B$）。

- **蕴含**（$A \Rightarrow B$）对应于**函数类型**（$A \rightarrow B$）。

- **真（True）**对应于**单位类型**，只有一个元素。

- **假（False）**对应于**空类型**，没有元素。

#### **意义**

- **逻辑证明**可以被视为**程序**，证明的结构对应于程序的结构。

- **逻辑推导**对应于**程序的类型检查和类型推导**。

---

### **4.5 一些示例**

#### **4.5.1 恒等函数：$A$ 蕴含自身**

- **命题**：$A \Rightarrow A$

- **证明**：

  - **在逻辑中**，假设 $A$ 成立，直接得出 $A$。

  - **对应的程序**：恒等函数 $\lambda x. x$，类型为 $A \rightarrow A$。

#### **4.5.2 蕴含的传递性：函数组合**

- **命题**：$(A \Rightarrow B) \Rightarrow (B \Rightarrow C) \Rightarrow (A \Rightarrow C)$

- **证明**：

  - **逻辑推导**：

    1. 假设 $f : A \Rightarrow B$，$g : B \Rightarrow C$。

    2. 要证明 $h : A \Rightarrow C$。

    3. 构造 $h = g \circ f$，即对于任意 $a : A$，$h(a) = g(f(a))$。

  - **对应的程序**：函数组合的实现。

#### **4.5.3 不同的证明**

- **多种证明方法**可以对应于不同的程序实现。

#### **4.5.4 不同的推导**

- **不同的推导顺序**可能导致相同的证明对象，但推导过程不同。

#### **4.5.5 合取和析取**

- **合取对应于类型的乘积**，元素是对。

- **析取对应于类型的和**，元素是标记的值，表示哪个分支成立。

---

### **4.6 量词**

#### **全称量化（Universal Quantification）**

- **符号**：$\forall x : A.\ P(x)$

- **解释**：对于类型 $A$ 的每个元素 $x$，命题 $P(x)$ 成立。

- **对应于**：**依赖函数类型**。

#### **存在量化（Existential Quantification）**

- **符号**：$\exists x : A.\ P(x)$

- **解释**：存在一个类型 $A$ 的元素 $x$，使得命题 $P(x)$ 成立。

- **对应于**：**依赖对类型**。

#### **4.6.1 一些示例证明**

- **示例**：证明对任意自然数 $n$，存在一个自然数 $m$，使得 $m = n + 1$。

- **构造性证明**：给定 $n$，构造 $m = \text{succ } n$，并证明 $m = n + 1$。

---

### **4.7 基本类型**

#### **4.7.1 布尔类型（Booleans）**

- **类型**：$\text{bool}$

- **元素**：$\text{true}$ 和 $\text{false}$

- **消除规则**：根据布尔值选择不同的计算分支。

#### **4.7.2 有限类型（Finite Types）**

- **定义**：有限多个元素的类型，例如单位类型 $\mathbf{1}$，只有一个元素 $\star$。

- **意义**：可以表示真值或用于构造其他类型。

#### **4.7.3 $\top$ 和 $\bot$**

- **$\top$（真）**：对应于只有一个元素的类型，表示真理。

- **$\bot$（假）**：对应于没有元素的类型，表示矛盾。

---

### **4.8 自然数**

- **类型**：$\mathbb{N}$

- **元素**：

  - **零**：$0$

  - **后继函数**：如果 $n : \mathbb{N}$，则 $\text{succ } n : \mathbb{N}$

- **递归定义函数**：使用自然数的归纳特性，可以定义递归函数。

- **归纳原理**：用于证明关于自然数的性质。

---

### **4.9 良基类型——树**

- **树的定义**：可以通过递归定义。

- **元素**：

  - **空树**

  - **节点**：由子树组成。

- **意义**：展示如何定义复杂的递归类型。

---

### **4.10 等式**

#### **4.10.1 基本类型上的等式**

- **定义**：对于类型 $A$ 的元素 $a, b$，$a =_A b$ 表示 $a$ 与 $b$ 在类型 $A$ 中相等。

- **等式本身是一个命题**，需要证明。

#### **4.10.2 不等式**

- **在构造性数学中**，使用“分离”概念替代“不等”，表示两个元素可以区分。

#### **4.10.3 依赖类型**

- **依赖类型**：类型可以依赖于值，例如，对于自然数 $n$，类型 $\text{Vector } n$ 表示长度为 $n$ 的向量。

#### **4.10.4 I 型上的等式**

- **I 型**：标识类型，包含证明两个元素相等的信息。

---

### **4.11 可转换性**

#### **4.11.1 定义：可转换性和等式**

- **可转换性（Convertibility）**：如果两个表达式可以通过一系列计算步骤互相转换，则它们是可转换的。

- **等式**：在类型理论中，可转换的表达式被视为相等。

#### **4.11.2 示例——加一**

- **函数**：定义一个函数，将自然数加一。

- **可转换性**：证明 $\text{succ } n$ 与 $n + 1$ 是可转换的。

#### **4.11.3 示例——自然数的等式**

- **证明**：给定两个自然数 $n, m$，如果 $n = m$，则 $\text{succ } n = \text{succ } m$。

- **意义**：展示等式在递归定义和证明中的应用。

---

### **总结**

- **类型理论**提供了一个将逻辑、函数式编程和构造性数学统一的框架。

- **核心思想**是命题与类型的对应，以及证明与程序的对应。

- **通过类型理论**，我们可以在同一个系统中编写程序、进行推理和验证程序的正确性。

- **接下来的章节**将进一步发展和探讨这个系统，包括其优点和局限性。

---

**如果您对任何部分有疑问，或者希望深入了解某个主题，请随时告诉我！**

### ---------------------------

# **第 4 章：类型理论简介**

---

## **4.1 命题逻辑：非正式概述**

在本节中，我们将非正式地讨论使用逻辑连接词（∧、⇒、∨ 和 ⊥）构造的命题的证明意味着什么。我们使用记号 $p : P$ 表示“$p$ 是命题 $P$ 的证明（或论证）”。

### **逻辑连接词的构造性解释**

让我们逐一查看逻辑连接词，理解在构造性逻辑中，什么构成了这些连接词下命题的证明。

#### **1. 合取（Conjunction）$A \wedge B$**

- **证明形式**：一个 $A \wedge B$ 的证明是一个**证明对** $(p, q)$，其中 $p : A$，$q : B$。
- **理由**：
  - 为了证明 $A \wedge B$ 的有效性，我们需要分别证明 $A$ 和 $B$ 的有效性。
  - 从一个合取的证明中，我们可以提取出组成命题的证明。

#### **2. 蕴含（Implication）$A \Rightarrow B$**

- **证明形式**：一个 $A \Rightarrow B$ 的证明是一个**方法或函数**，将任何 $A$ 的证明转换为 $B$ 的证明。
- **理由**：
  - 如果我们有 $A$ 的证明 $a$ 和 $A \Rightarrow B$ 的证明 $f$，我们可以通过将 $f$ 应用于 $a$ 来得到 $B$ 的证明。
  - 这反映了蕴含在构造性逻辑中的函数性。

#### **3. 析取（Disjunction）$A \vee B$**

- **证明形式**：一个 $A \vee B$ 的证明要么是 $A$ 的证明，要么是 $B$ 的证明，**并且需要指明是哪一个**。
- **与经典逻辑的区别**：
  - 在经典逻辑中，可以根据排中律断言 $A \vee \lnot A$ 总是成立，即使我们无法证明 $A$ 或 $\lnot A$。
  - 在构造性逻辑中，如果我们无法证明 $A$ 或 $\lnot A$，就不能断言 $A \vee \lnot A$。
  - 从构造性的 $A \vee B$ 的证明中，我们可以知道具体证明了 $A$ 或 $B$ 中的哪个，并且可以提取出相应的证明。

#### **4. 矛盾（Falsity）$\bot$**

- **证明形式**：没有 $\bot$ 的证明。
- **理由**：
  - $\bot$ 表示矛盾或不可能的命题，没有任何证明可以支持它。

### **定义 4.1：否定和双蕴含**

在上述描述中，否定 $\lnot A$ 和双蕴含 $A \Leftrightarrow B$ 可以定义为：

- **否定**：
  $$
  \lnot A \equiv A \Rightarrow \bot
  $$
  - **解释**：$\lnot A$ 的证明是一个将 $A$ 的证明转换为矛盾 $\bot$ 的函数。

- **双蕴含**：
  $$
  A \Leftrightarrow B \equiv (A \Rightarrow B) \wedge (B \Rightarrow A)
  $$
  - **解释**：$A \Leftrightarrow B$ 的证明是一个证明对，包含：
    - 一个将 $A$ 的证明转换为 $B$ 的证明的函数。
    - 一个将 $B$ 的证明转换为 $A$ 的证明的函数。

### **示例：构造证明**

#### **示例 1：证明 $(A \wedge B) \Rightarrow (B \wedge A)$**

- **目标**：构造一个函数，将 $A \wedge B$ 的证明转换为 $B \wedge A$ 的证明。
- **步骤**：
  1. **假设**我们有 $A \wedge B$ 的证明 $p$。
  2. **提取组成部分**：
     - $p$ 是一个证明对，包含 $p_1 : A$ 和 $p_2 : B$。
  3. **构造新的证明对**：
     - $(p_2, p_1)$ 是 $B \wedge A$ 的证明，其中交换了顺序。
  4. **使用函数表示**：
     - 定义一个函数 $\lambda p. (snd\ p, fst\ p)$，其中：
       - $snd\ p$ 提取 $p$ 的第二个组件（$B$ 的证明）。
       - $fst\ p$ 提取 $p$ 的第一个组件（$A$ 的证明）。

#### **示例 2：证明 $((A \vee B) \Rightarrow C) \wedge A \Rightarrow C$**

- **目标**：构造一个函数，将 $((A \vee B) \Rightarrow C) \wedge A$ 的证明转换为 $C$ 的证明。
- **步骤**：
  1. **假设**我们有 $((A \vee B) \Rightarrow C) \wedge A$ 的证明。
  2. **提取组成部分**：
     - $q : (A \vee B) \Rightarrow C$（一个函数，将 $A \vee B$ 的证明转换为 $C$ 的证明）。
     - $r : A$（$A$ 的证明）。
  3. **构造 $A \vee B$ 的证明**：
     - 使用 $inl\ r$ 表示 $A$ 的证明被标记为左侧析取项的证明。
  4. **应用函数**：
     - 计算 $q\ (inl\ r)$，得到 $C$ 的证明。
  5. **使用函数表示**：
     - 定义一个函数 $\lambda (q, r). q\ (inl\ r)$。

---

## **4.2 判断、证明和推导**

本节介绍了形式系统，区分了**判断（Judgements）**、**证明对象（Proofs）**和**推导（Derivations）**。

### **回顾第 1 章的逻辑**

在第 1 章中，我们的逻辑规则形式如下：

- **示例**：
  $$
  \frac{A\quad B}{A \wedge B}\ (\wedge I)
  $$
  - **解释**：如果 $A$ 和 $B$ 有效，那么 $A \wedge B$ 有效。

- **一般形式**：
  - **判断**：$A$ 有效。
  - **推导**：从一些判断中推导出新的判断。

### **构造性方法的区别**

- **引入证明对象**：
  - 在构造性数学中，我们不仅关注命题的有效性，还关注命题的**显式证明**。
  - **判断形式**：$p : P$，表示“$p$ 是命题 $P$ 的证明”。

- **引入合取的规则**：
  $$
  \frac{p : A \quad q : B}{(p, q) : A \wedge B}\ (\wedge I)
  $$
  - **解释**：如果 $p$ 是 $A$ 的证明，$q$ 是 $B$ 的证明，那么 $(p, q)$ 是 $A \wedge B$ 的证明。

- **推导的构造**：
  - **推导**是通过应用推导规则，从已知判断构建新的判断的过程。
  - **证明对象**和**命题**构成了**对象语言（Object Language）**。
  - **推导**是关于对象语言的判断的推理过程。

### **示例：推导合取的交换律**

- **目标**：从 $r : A \wedge B$ 推导 $(snd\ r, fst\ r) : B \wedge A$。
- **步骤**：
  1. **提取组成部分**：
     - 应用消除规则 $(\wedge E1)$ 得到 $fst\ r : A$。
     - 应用消除规则 $(\wedge E2)$ 得到 $snd\ r : B$。
  2. **构造新的证明**：
     - 使用引入规则 $(\wedge I)$，得到 $(snd\ r, fst\ r) : B \wedge A$。

### **假设和释放**

- **假设**：
  - 推导可以基于假设进行。
  - **释放假设**：某些规则可以释放（或消除）假设。

- **记号**：
  - 我们使用与之前相同的记号来表示假设的释放。

### **推导规则的命名**

- **统一命名**：
  - 为了避免混淆，我们继续使用之前的规则名称，如 $(\wedge I)$、$(\wedge E)$ 等。

### **公式的构造（Formation Rules）**

- **引入“公式”的判断**：
  - 某些规则涉及判断某个符号序列是一个公式。
  - **示例**：
    $$
    \frac{A\ \text{是一个公式} \quad B\ \text{是一个公式}}{A \wedge B\ \text{是一个公式}}\ (\wedge F)
    $$

- **作用**：
  - 这些规则被称为**构造规则（Formation Rules）**，用于解释在什么情况下可以形成特定的公式。
  - 它们定义了命题语言的语法规则。

### **语法和推导的交织**

- **在类型理论中**：
  - 一些公式只有在某些判断可推导时才是良构的，这将语法和推导紧密地联系在一起。
  - 因此，我们需要在规则中明确地包含公式的构造规则。

### **每个连接词的四种规则**

对于每个逻辑连接词，我们都定义四种规则：

1. **构造规则（Formation Rules）**：定义如何形成公式。
2. **引入规则（Introduction Rules）**：说明如何构造包含该连接词的公式的证明。
3. **消除规则（Elimination Rules）**：说明如何从包含该连接词的证明中提取信息。
4. **计算规则（Computation Rules）**：说明如何简化包含该连接词的证明对象。

### **计算规则的新概念**

- **计算规则**：
  - 提供了关于证明对象的计算信息。
  - **示例**：
    - 对于合取：
      $$
      fst\ (p, q) \to p \\
      snd\ (p, q) \to q
      $$
    - 其中 $\to$ 表示“归约为”或“简化为”，类似于 λ-演算中的归约。

- **作用**：
  - 计算规则描述了如何对证明对象进行简化或计算，这在传统的逻辑系统中是新的概念。

---

## **4.3 命题演算的规则**

本节正式给出了类型理论框架下的命题演算规则，包括每个逻辑连接词的构造、引入、消除和计算规则。

### **1. 合取（Conjunction）$\wedge$**

#### **构造规则（Formation Rule）**

$$
\frac{A\ \text{是一个公式} \quad B\ \text{是一个公式}}{A \wedge B\ \text{是一个公式}}\ (\wedge F)
$$

- **解释**：如果 $A$ 和 $B$ 是公式，那么 $A \wedge B$ 也是一个公式。

#### **引入规则（Introduction Rule）**

$$
\frac{p : A \quad q : B}{(p, q) : A \wedge B}\ (\wedge I)
$$

- **解释**：如果 $p$ 是 $A$ 的证明，$q$ 是 $B$ 的证明，那么 $(p, q)$ 是 $A \wedge B$ 的证明。

#### **消除规则（Elimination Rules）**

1. **提取第一部分**：
   $$
   \frac{r : A \wedge B}{fst\ r : A}\ (\wedge E1)
   $$

2. **提取第二部分**：
   $$
   \frac{r : A \wedge B}{snd\ r : B}\ (\wedge E2)
   $$

- **解释**：从 $A \wedge B$ 的证明 $r$ 中，我们可以提取出 $A$ 和 $B$ 的证明。

#### **计算规则（Computation Rules）**

$$
fst\ (p, q) \to p \\
snd\ (p, q) \to q
$$

- **解释**：这些规则描述了如何简化证明对象，直接提取出组成部分。

### **2. 蕴含（Implication）$\Rightarrow$**

#### **构造规则（Formation Rule）**

$$
\frac{A\ \text{是一个公式} \quad B\ \text{是一个公式}}{A \Rightarrow B\ \text{是一个公式}}\ (\Rightarrow F)
$$

- **解释**：如果 $A$ 和 $B$ 是公式，那么 $A \Rightarrow B$ 也是一个公式。

#### **引入规则（Introduction Rule）**

$$
\frac{\begin{array}{c}
[x : A] \\
\vdots \\
e : B
\end{array}}{(\lambda x : A). e : A \Rightarrow B}\ (\Rightarrow I)
$$

- **解释**：
  - 假设 $x : A$，如果在此假设下可以推导出 $e : B$，那么可以构造 $A \Rightarrow B$ 的证明 $(\lambda x : A). e$。
  - **记号**：$ x : A $ 表示假设 $x : A$ 被释放（即在构造 $(\lambda x : A). e$ 时，不再需要假设 $x : A$）。

#### **消除规则（Elimination Rule）**

$$
\frac{q : A \Rightarrow B \quad a : A}{(q\ a) : B}\ (\Rightarrow E)
$$

- **解释**：如果 $q$ 是 $A \Rightarrow B$ 的证明，$a$ 是 $A$ 的证明，那么 $q\ a$ 是 $B$ 的证明。

#### **计算规则（Computation Rule）**

$$
((\lambda x : A). e)\ a \to e[a / x]
$$

- **解释**：
  - 将证明对象 $(\lambda x : A). e$ 应用于 $a$ 时，结果是将 $e$ 中的所有自由出现的 $x$ 替换为 $a$。
  - **注意**：需要避免自由变量变为绑定变量（避免变量捕获）。

### **3. 析取（Disjunction）$\vee$**

#### **构造规则（Formation Rule）**

$$
\frac{A\ \text{是一个公式} \quad B\ \text{是一个公式}}{A \vee B\ \text{是一个公式}}\ (\vee F)
$$

- **解释**：如果 $A$ 和 $B$ 是公式，那么 $A \vee B$ 也是一个公式。

#### **引入规则（Introduction Rules）**

1. **左侧引入**：
   $$
   \frac{q : A}{inl\ q : A \vee B}\ (\vee I1)
   $$

2. **右侧引入**：
   $$
   \frac{r : B}{inr\ r : A \vee B}\ (\vee I2)
   $$

- **解释**：
  - 如果 $q$ 是 $A$ 的证明，那么 $inl\ q$ 是 $A \vee B$ 的证明，标记为左侧。
  - 如果 $r$ 是 $B$ 的证明，那么 $inr\ r$ 是 $A \vee B$ 的证明，标记为右侧。

#### **消除规则（Elimination Rule）**

$$
\frac{p : A \vee B \quad f : A \Rightarrow C \quad g : B \Rightarrow C}{cases\ p\ f\ g : C}\ (\vee E)
$$

- **解释**：
  - 如果 $p$ 是 $A \vee B$ 的证明，$f$ 是 $A \Rightarrow C$ 的证明，$g$ 是 $B \Rightarrow C$ 的证明，那么 $cases\ p\ f\ g$ 是 $C$ 的证明。
  - **机制**：
    - 如果 $p$ 是 $inl\ q$ 形式（$A$ 的证明），则计算 $f\ q$。
    - 如果 $p$ 是 $inr\ r$ 形式（$B$ 的证明），则计算 $g\ r$。

#### **计算规则（Computation Rules）**

$$
cases\ (inl\ q)\ f\ g \to f\ q \\
cases\ (inr\ r)\ f\ g \to g\ r
$$

- **解释**：根据 $p$ 的形式，选择相应的函数应用。

### **4. 矛盾（Falsity）$\bot$**

#### **构造规则（Formation Rule）**

$$
\bot\ \text{是一个公式}\ (\bot F)
$$

- **解释**：$\bot$ 是一个公式。

#### **引入规则（Introduction Rule）**

- **没有引入规则**：因为没有 $\bot$ 的证明。

#### **消除规则（Elimination Rule）**

$$
\frac{p : \bot}{abort_A\ p : A}\ (\bot E)
$$

- **解释**：如果我们有 $\bot$ 的证明 $p$，那么对于任何命题 $A$，我们都可以构造 $A$ 的证明。这反映了“从矛盾中可以推出任何命题”（**Ex Falso Quodlibet**）的原则。

#### **计算规则（Computation Rules）**

- **没有计算规则**：因为 $\bot$ 没有实际的证明对象。

### **5. 假设规则（Rule of Assumption）**

#### **假设规则（Assumption Rule）**

$$
\frac{A\ \text{是一个公式}}{x : A}\ (\text{AS})
$$

- **解释**：如果 $A$ 是一个公式，我们可以假设 $x : A$。

#### **注意事项**：

- **一致性**：我们的假设集应始终保持一致，不应假设任何变量是多个不同公式的证明。
- **推导位置**：假设不是出现在推导的叶子节点，而是需要在确认 $A$ 是一个公式之后。

### **简化**

- **在示例中**：
  - 我们可能会省略一些显而易见的推导，例如 $A$ 是一个公式的证明，以保持推导的简洁。
  - 只有在推导是平凡的或可以非正式地假设已完成的情况下，才会这样做。

---

## **练习**

### **4.1. 证明合取的结合律**

- **目标**：推导公式 $(A \wedge B) \wedge C \Rightarrow A \wedge (B \wedge C)$ 的证明。

- **思路**：
  1. **假设**：$p : (A \wedge B) \wedge C$。
  2. **提取组成部分**：
     - $fst\ p : A \wedge B$。
     - $snd\ p : C$。
     - 进一步分解 $fst\ p$：
       - $fst\ (fst\ p) : A$。
       - $snd\ (fst\ p) : B$。
  3. **构造新的证明对**：
     - $(fst\ (fst\ p), (snd\ (fst\ p), snd\ p)) : A \wedge (B \wedge C)$。
  4. **总结**：
     - 定义函数 $\lambda p. (fst\ (fst\ p), (snd\ (fst\ p), snd\ p))$。

### **4.2. 证明 $(\lnot A \vee B) \Rightarrow (A \Rightarrow B)$ 的有效性**

- **目标**：构造一个证明对象。

- **思路**：
  1. **假设**：$p : \lnot A \vee B$，$a : A$。
  2. **定义函数**：
     - $f : \lnot A \Rightarrow B$，定义为 $f = \lambda n. abort_B\ (n\ a)$，因为 $n : \lnot A = A \Rightarrow \bot$。
  3. **应用消除规则**：
     - 使用 $cases\ p\ f\ (\lambda b. b)$ 得到 $B$ 的证明。
  4. **总结**：
     - 定义函数 $\lambda p.\ \lambda a.\ cases\ p\ (\lambda n.\ abort_B\ (n\ a))\ (\lambda b.\ b)$。

- **关于逆命题** $(A \Rightarrow B) \Rightarrow (\lnot A \vee B)$：
  - **构造性地**，无法证明该命题，因为无法从 $A \Rightarrow B$ 得到 $\lnot A$ 或 $B$ 的具体证明。

### **4.3. 从 $x : A \vee \lnot A$ 推导 $\lnot\lnot A \Rightarrow A$**

- **目标**：构造 $\lnot\lnot A \Rightarrow A$ 的证明。

- **思路**：
  1. **假设**：$x : A \vee \lnot A$，$n : \lnot\lnot A$。
  2. **应用消除规则**：
     - $cases\ x\ (\lambda a.\ a)\ (\lambda na.\ abort_A\ (n\ na))$。
  3. **总结**：
     - 定义函数 $\lambda n.\ cases\ x\ (\lambda a.\ a)\ (\lambda na.\ abort_A\ (n\ na))$。

- **证明 $A \Rightarrow \lnot\lnot A$**：

  - **定义**：$\lambda a.\ \lambda n.\ n\ a$。

### **4.4. 从 $x : (A \wedge B) \Rightarrow C$ 和 $y : A$ 推导 $B \Rightarrow C$**

- **目标**：构造 $B \Rightarrow C$ 的证明。

- **思路**：
  1. **假设**：$x : (A \wedge B) \Rightarrow C$，$y : A$，$b : B$。
  2. **构造 $A \wedge B$ 的证明**：$(y, b) : A \wedge B$。
  3. **应用 $x$**：$x\ (y, b) : C$。
  4. **定义函数**：$\lambda b.\ x\ (y, b)$。

- **释放假设**：
  - 将 $y$ 从假设中释放，得到 $A \Rightarrow (B \Rightarrow C)$。
  - 定义证明对象：$\lambda y.\ \lambda b.\ x\ (y, b)$。

---

**希望以上详细解释能够帮助您深入理解第 4 章的前三节内容。如果您对任何部分有疑问或需要进一步澄清，请随时告诉我！**

### ----------------------------

# **第 4 章：类型理论简介**

---

## **4.4 Curry-Howard 等价**

### **概述**

本节是我们类型理论发展的核心，探讨了一种引人注目的对应关系，称为**Curry-Howard 等价**，它将**类型化 λ-演算**与**构造性逻辑**联系起来。为纪念 Haskell B. Curry 和 W. Howard 的贡献，这种对应关系以他们的名字命名。他们以及 Scott、Lauchli 和 Martin-Löf 等学者首次观察到了这种等价关系。

在这种等价关系下，**类型对应于命题**，而这些类型的**成员**（如对、函数等）对应于**证明**。在上一节的构造性逻辑发展中，我们已经为这种巧合做好了准备，因为我们引入的证明是熟悉的对象，如对和函数。

在接下来的章节中，我们将看到这种对应关系的广度，以及它如何丰富我们对逻辑和编程的理解。本节通过重新解释上述逻辑规则为程序构造规则，来展示这种等价关系的核心。

### **规则的重新解释**

我们将逻辑规则重新解释为：

- **构造规则（Formation Rules）**：定义系统中的类型是什么。
- **引入和消除规则（Introduction and Elimination Rules）**：说明哪些表达式是哪些类型的成员，即**类型检查**的规则。
- **计算规则（Computation Rules）**：说明这些对象如何简化为更简单的形式，即如何**计算或评估表达式**。

另一种看待这些规则的方式是，将构造规则视为解释语言的类型，引入和消除规则解释表达式的类型规则（即类型检查如何进行）——它们共同描述了传统语言的**静态部分**，而计算规则解释其行为的**动态部分**。

在完整的系统中，**静态和动态的区别会变得模糊**，因为类型检查和计算紧密地联系在一起。

现在，我们逐个连接词地回顾规则，将“是一个公式”的判断改为“是一个类型”，以反映我们的不同视角。

### **1. 合取（Conjunction）$\wedge$**

#### **构造规则（Formation Rule）**

$$
\frac{A\ \text{是一个类型} \quad B\ \text{是一个类型}}{A \wedge B\ \text{是一个类型}}\ (\wedge F)
$$

- **解释**：如果 $A$ 和 $B$ 是类型，那么 $A \wedge B$ 是一个类型。

#### **引入规则（Introduction Rule）**

$$
\frac{p : A \quad q : B}{(p, q) : A \wedge B}\ (\wedge I)
$$

- **解释**：如果 $p$ 是类型 $A$ 的一个成员，$q$ 是类型 $B$ 的一个成员，那么 $(p, q)$ 是类型 $A \wedge B$ 的一个成员。

#### **消除规则（Elimination Rules）**

1. **提取第一部分**：
   $$
   \frac{r : A \wedge B}{fst\ r : A}\ (\wedge E1)
   $$
2. **提取第二部分**：
   $$
   \frac{r : A \wedge B}{snd\ r : B}\ (\wedge E2)
   $$

- **解释**：从类型 $A \wedge B$ 的一个成员 $r$ 中，我们可以提取出类型 $A$ 和 $B$ 的成员。

#### **计算规则（Computation Rules）**

$$
fst\ (p, q) \to p \\
snd\ (p, q) \to q
$$

- **解释**：这些规则表明，从一个对 $(p, q)$ 中，我们可以直接获得其组成部分 $p$ 和 $q$。

#### **总结**

- **类型解释**：$A \wedge B$ 是两个类型的乘积类型。
- **成员**：根据引入规则，类型 $A \wedge B$ 的成员是由类型 $A$ 和 $B$ 的成员组成的对 $(p, q)$。
- **消除规则和计算规则**：表明我们可以从乘积类型的成员中提取原始的组成部分，这些规则一起说明了乘积类型的所有成员都是对。

在传统的编程语言中，乘积类型通常称为**记录类型（Record Type）**。几乎所有现代编程语言都支持记录类型。

### **2. 蕴含（Implication）$\Rightarrow$**

#### **构造规则（Formation Rule）**

$$
\frac{A\ \text{是一个类型} \quad B\ \text{是一个类型}}{A \Rightarrow B\ \text{是一个类型}}\ (\Rightarrow F)
$$

- **解释**：如果 $A$ 和 $B$ 是类型，那么 $A \Rightarrow B$ 是一个类型。

#### **引入规则（Introduction Rule）**

$$
\frac{\begin{array}{c}
[x : A] \\
\vdots \\
e : B
\end{array}}{(\lambda x : A). e : A \Rightarrow B}\ (\Rightarrow I)
$$

- **解释**：
  - 假设 $x : A$，在此假设下，如果我们可以推导出 $e : B$，那么我们可以构造一个函数 $(\lambda x : A). e$，其类型为 $A \Rightarrow B$。
  - **释放假设**：该规则释放了假设 $x : A$。

#### **消除规则（Elimination Rule）**

$$
\frac{q : A \Rightarrow B \quad a : A}{(q\ a) : B}\ (\Rightarrow E)
$$

- **解释**：如果 $q$ 是类型 $A \Rightarrow B$ 的一个成员，$a$ 是类型 $A$ 的一个成员，那么 $q\ a$ 是类型 $B$ 的一个成员。

#### **计算规则（Computation Rule）**

$$
((\lambda x : A). e)\ a \to e[a / x]
$$

- **解释**：将函数 $(\lambda x : A). e$ 应用于 $a$ 时，结果是将 $e$ 中的 $x$ 替换为 $a$。
- **这正是 λ-演算中的 β-归约规则**，通过将实际参数替换形式参数来实现函数应用。

#### **记号说明**

- **函数类型的记法**：
  - 为了简洁，我们通常省略类型注释，或者在需要时以下标形式标注：
    - $\lambda x_A . e$ 或简单地 $\lambda x . e$。
- **关联性约定**：
  - **函数应用**：左结合，即 $f\ g\ x$ 被解释为 $((f\ g)\ x)$。
  - **函数类型**：右结合，即 $A \Rightarrow B \Rightarrow C$ 被解释为 $A \Rightarrow (B \Rightarrow C)$。

### **3. 析取（Disjunction）$\vee$**

#### **构造规则（Formation Rule）**

$$
\frac{A\ \text{是一个类型} \quad B\ \text{是一个类型}}{A \vee B\ \text{是一个类型}}\ (\vee F)
$$

- **解释**：如果 $A$ 和 $B$ 是类型，那么 $A \vee B$ 是一个类型。

#### **引入规则（Introduction Rules）**

1. **左侧引入**：
   $$
   \frac{q : A}{inl\ q : A \vee B}\ (\vee I1)
   $$
2. **右侧引入**：
   $$
   \frac{r : B}{inr\ r : A \vee B}\ (\vee I2)
   $$

- **解释**：
  - 如果 $q$ 是类型 $A$ 的一个成员，那么 $inl\ q$ 是类型 $A \vee B$ 的一个成员，标记为来自左侧。
  - 如果 $r$ 是类型 $B$ 的一个成员，那么 $inr\ r$ 是类型 $A \vee B$ 的一个成员，标记为来自右侧。

#### **消除规则（Elimination Rule）**

$$
\frac{p : A \vee B \quad f : A \Rightarrow C \quad g : B \Rightarrow C}{cases\ p\ f\ g : C}\ (\vee E)
$$

- **解释**：
  - **输入**：
    - $p$ 是类型 $A \vee B$ 的一个成员。
    - $f$ 是一个函数，类型为 $A \Rightarrow C$。
    - $g$ 是一个函数，类型为 $B \Rightarrow C$。
  - **机制**：
    - 如果 $p$ 是 $inl\ q$ 的形式（来自 $A$ 的成员），那么结果是 $f\ q$。
    - 如果 $p$ 是 $inr\ r$ 的形式（来自 $B$ 的成员），那么结果是 $g\ r$。
  - **用途**：该规则提供了一种类型安全的方式来处理分离的联合类型。

#### **计算规则（Computation Rules）**

$$
cases\ (inl\ q)\ f\ g \to f\ q \\
cases\ (inr\ r)\ f\ g \to g\ r
$$

- **解释**：根据 $p$ 的具体形式（是 $inl$ 还是 $inr$），选择相应的函数应用。

#### **总结**

- **类型解释**：$A \vee B$ 是类型 $A$ 和 $B$ 的**不相交并集（Disjoint Union）**。
- **编程中的应用**：
  - 在编程语言中，这种类型可以用来表示一种**变体类型（Variant Type）**，例如某个类型的值可以是几种可能类型中的一种。
  - 通过引入标签 $inl$ 和 $inr$，我们可以在类型系统中区分不同的分支，避免运行时的类型错误。

### **4. 矛盾类型（Absurdity）$\bot$**

#### **构造规则（Formation Rule）**

$$
\bot\ \text{是一个类型}\ (\bot F)
$$

- **解释**：$\bot$ 是一个类型，称为**空类型（Empty Type）**，没有任何成员。

#### **消除规则（Elimination Rule）**

$$
\frac{p : \bot}{abort_A\ p : A}\ (\bot E)
$$

- **解释**：
  - 如果我们有一个 $\bot$ 的成员 $p$，那么对于任何类型 $A$，$abort_A\ p$ 都是 $A$ 的一个成员。
  - **意义**：由于 $\bot$ 没有成员，因此 $p$ 不可能存在。如果确实存在，那么系统应该“崩溃”或“中止”。

#### **总结**

- **逻辑解释**：$\bot$ 对应于**矛盾命题**，没有任何证明。
- **类型解释**：$\bot$ 是一个没有成员的类型。

### **5. 假设规则（Rule of Assumption）**

#### **规则（Assumption Rule）**

$$
\frac{A\ \text{是一个类型}}{x : A}\ (\text{AS})
$$

- **解释**：如果 $A$ 是一个类型，那么我们可以假设 $x : A$，即 $x$ 是类型 $A$ 的一个成员。

#### **注意事项**

- **必要性**：为了使假设 $x : A$ 有意义，必须先确认 $A$ 是一个类型。
- **简化**：在许多示例中，我们可能会省略 $A$ 是一个类型的推导，假设它已经完成。

### **总结**

- 我们已经重新解读了逻辑规则，将其视为：
  - **类型化函数式编程系统**的类型和对象的规则。
  - **逻辑系统**的命题和证明的规则。
- 在接下来的章节中，我们将进一步探讨这种对应关系，包括在命题逻辑中的影响，以及如何扩展逻辑和函数式语言。

---

## **4.5 一些示例**

在本节中，我们将通过一些类型理论中的推导示例，来理解如何根据不同的视角，将它们视为命题的证明或特定类型的对象。我们假设已经确认了各种类型的假设，例如 $A$ 是一个类型，等等。

### **4.5.1 恒等函数：$A$ 蕴含自身**

- **目标**：构造一个类型为 $A \Rightarrow A$ 的函数，即恒等函数 $\lambda x_A . x$。
- **推导过程**：
  1. **假设**：$x : A$。
  2. **推导**：因为 $x : A$，所以直接有 $x : A$。
  3. **应用引入规则**：
     $$
     \frac{\begin{array}{c}
     [x : A] \\
     x : A
     \end{array}}{\lambda x_A . x : A \Rightarrow A}\ (\Rightarrow I)
     $$
  4. **结果**：恒等函数 $\lambda x_A . x$ 是类型 $A \Rightarrow A$ 的一个成员。
- **解释**：这同时表明了恒等函数是命题 $A \Rightarrow A$ 的一个证明。

### **4.5.2 蕴含的传递性：函数组合**

- **目标**：证明如果有 $A \Rightarrow B$ 和 $B \Rightarrow C$，则有 $A \Rightarrow C$，即蕴含的传递性。
- **推导过程**：
  1. **假设**：
     - $a : A \Rightarrow B$。
     - $b : B \Rightarrow C$。
     - $x : A$（将在推导过程中释放）。
  2. **应用消除规则**：
     - $(a\ x) : B$（从 $a : A \Rightarrow B$ 和 $x : A$ 推导）。
     - $(b\ (a\ x)) : C$（从 $b : B \Rightarrow C$ 和 $(a\ x) : B$ 推导）。
  3. **构造函数**：
     - 将 $x$ 释放，得到 $\lambda x_A . (b\ (a\ x)) : A \Rightarrow C$。
  4. **总结**：
     - 构造的函数是 $\lambda x . b\ (a\ x)$，即函数组合 $b \circ a$。
     - 进一步，我们可以对 $a$ 和 $b$ 进行抽象，得到组合函数：
       $$
       \lambda a_{A \Rightarrow B} . \lambda b_{B \Rightarrow C} . \lambda x_A . b\ (a\ x)
       $$
     - 其类型为 $(A \Rightarrow B) \Rightarrow (B \Rightarrow C) \Rightarrow (A \Rightarrow C)$。

- **应用**：这也证明了命题 $(A \Rightarrow B) \Rightarrow (B \Rightarrow C) \Rightarrow (A \Rightarrow C)$。

### **4.5.3 不同的证明**

- **示例**：对于类型 $(A \wedge A) \Rightarrow (A \wedge A)$，存在不同的证明对象。

#### **证明一：恒等函数**

- **函数**：$\lambda x_{A \wedge A} . x$
- **解释**：直接返回输入，对证明的内容不做任何改变。

#### **证明二：交换对的组成部分**

- **推导过程**：
  1. **假设**：$x : A \wedge A$。
  2. **提取组成部分**：
     - $fst\ x : A$。
     - $snd\ x : A$。
  3. **构造新的对**：
     - $(snd\ x, fst\ x) : A \wedge A$。
  4. **构造函数**：
     - $\lambda x_{A \wedge A} . (snd\ x, fst\ x)$
- **解释**：该函数交换了对的两个组成部分。

- **结论**：尽管这两个函数的类型相同，但它们的行为不同，证明了同一命题的不同方式。

### **4.5.4 不同的推导产生相同的证明对象**

- **示例**：不同的推导可能产生相同的证明对象，特别是在应用计算规则后。

- **定义函数**：
  - **组合函数**：
    $$
    comp \equiv \lambda f_{(A \wedge A) \Rightarrow (A \wedge A)} . \lambda g_{(A \wedge A) \Rightarrow (A \wedge A)} . \lambda x_{A \wedge A} . g\ (f\ x)
    $$
  - **交换函数**：
    $$
    swap \equiv \lambda x_{A \wedge A} . (snd\ x, fst\ x)
    $$

- **推导**：从 $a_1 : A$ 和 $a_2 : A$ 出发，可以推导出 $comp\ swap\ swap\ (a_1, a_2) : A \wedge A$。

- **计算**：
  - 通过应用计算规则，可以简化 $comp\ swap\ swap\ (a_1, a_2)$ 得到 $(a_1, a_2)$，这与直接使用 $(a_1, a_2)$ 得到的结果相同。
  - **说明**：不同的推导可能在简化后产生相同的证明对象。

### **4.5.5 合取和析取**

- **示例**：探索合取和析取之间的关系。

#### **证明命题**：$((A \vee B) \Rightarrow C) \Rightarrow ((A \Rightarrow C) \wedge (B \Rightarrow C))$

- **步骤**：

  1. **从 $(A \vee B) \Rightarrow C$ 推导 $A \Rightarrow C$**：
     - **假设**：$y : (A \vee B) \Rightarrow C$，$x : A$。
     - **构造 $A \vee B$ 的成员**：$inl\ x : A \vee B$。
     - **应用函数**：$y\ (inl\ x) : C$。
     - **构造函数**：$\lambda x_A . y\ (inl\ x) : A \Rightarrow C$。

  2. **从 $(A \vee B) \Rightarrow C$ 推导 $B \Rightarrow C$**：
     - **类似地**，构造 $\lambda w_B . y\ (inr\ w) : B \Rightarrow C$。

  3. **组合结果**：
     - **构造对**：$(\lambda x_A . y\ (inl\ x),\ \lambda w_B . y\ (inr\ w)) : (A \Rightarrow C) \wedge (B \Rightarrow C)$。

  4. **释放假设**：将 $y$ 释放，得到所需的函数。

#### **证明逆命题**：$(A \Rightarrow C) \wedge (B \Rightarrow C) \Rightarrow (A \vee B) \Rightarrow C$

- **步骤**：

  1. **假设**：$p : (A \Rightarrow C) \wedge (B \Rightarrow C)$，$z : A \vee B$。
  2. **应用消除规则**：
     - **构造**：$cases\ z\ (fst\ p)\ (snd\ p) : C$。
  3. **构造函数**：
     - **构造**：$\lambda p . \lambda z . cases\ z\ (fst\ p)\ (snd\ p) : ((A \Rightarrow C) \wedge (B \Rightarrow C)) \Rightarrow (A \vee B) \Rightarrow C$。

- **总结**：完成了所需的推导，证明了这两个命题在构造性逻辑中的等价性。

#### **关于德摩根律**

- **命题**：$\lnot (A \vee B) \Leftrightarrow \lnot A \wedge \lnot B$

- **构造性逻辑中的情况**：
  - 我们可以证明 $\lnot (A \vee B) \Rightarrow \lnot A \wedge \lnot B$，但逆命题在构造性逻辑中不成立。
  - 另一个德摩根律 $\lnot (A \wedge B) \Leftrightarrow \lnot A \vee \lnot B$ 中，只有 $\lnot A \vee \lnot B \Rightarrow \lnot (A \wedge B)$ 在构造性逻辑中成立。

- **解释**：由于在构造性逻辑中，证明析取需要知道具体证明了哪个分支，而从 $\lnot (A \wedge B)$ 的证明中，无法提取出 $\lnot A$ 或 $\lnot B$ 的具体证明。

---

## **练习**

### **4.5. 练习**

**问题**：给定一个类型为 $A \Rightarrow (B \Rightarrow C)$ 的函数，如何从中定义一个类型为 $(A \wedge B) \Rightarrow C$ 的函数？反之又该如何做？

**解答**：

1. **从 $A \Rightarrow (B \Rightarrow C)$ 构造 $(A \wedge B) \Rightarrow C$**：
   - **给定**：$f : A \Rightarrow (B \Rightarrow C)$，$p : A \wedge B$。
   - **提取组成部分**：
     - $fst\ p : A$，$snd\ p : B$。
   - **应用函数**：
     - $f\ (fst\ p) : B \Rightarrow C$。
     - $(f\ (fst\ p))\ (snd\ p) : C$。
   - **构造函数**：
     - $\lambda p_{A \wedge B} . (f\ (fst\ p))\ (snd\ p) : (A \wedge B) \Rightarrow C$。

2. **从 $(A \wedge B) \Rightarrow C$ 构造 $A \Rightarrow (B \Rightarrow C)$**：
   - **给定**：$g : (A \wedge B) \Rightarrow C$，$a : A$，$b : B$。
   - **构造对**：
     - $(a, b) : A \wedge B$。
   - **应用函数**：
     - $g\ (a, b) : C$。
   - **构造函数**：
     - $\lambda a_A . \lambda b_B . g\ (a, b) : A \Rightarrow (B \Rightarrow C)$。

### **4.6. 练习**

**问题**：从 $x : A$ 和 $y : B \vee C$ 构造一个类型为 $(A \wedge B) \vee (A \wedge C)$ 的对象。

**解答**：

- **构造**：
  - **应用析取消除规则**：
    - $cases\ y\ (\lambda b . inl\ (x, b))\ (\lambda c . inr\ (x, c)) : (A \wedge B) \vee (A \wedge C)$。
  - **解释**：
    - 如果 $y$ 是 $inl\ b$，则构造 $inl\ (x, b)$。
    - 如果 $y$ 是 $inr\ c$，则构造 $inr\ (x, c)$。

### **4.7. 练习**

**问题**：从 $f : A \Rightarrow C$ 和 $g : B \Rightarrow D$ 定义一个类型为 $(A \wedge B) \Rightarrow (C \wedge D)$ 的函数。

**解答**：

- **给定**：$f : A \Rightarrow C$，$g : B \Rightarrow D$，$p : A \wedge B$。
- **提取组成部分**：
  - $fst\ p : A$，$snd\ p : B$。
- **应用函数**：
  - $f\ (fst\ p) : C$，$g\ (snd\ p) : D$。
- **构造新的对**：
  - $(f\ (fst\ p),\ g\ (snd\ p)) : C \wedge D$。
- **构造函数**：
  - $\lambda p_{A \wedge B} . (f\ (fst\ p),\ g\ (snd\ p)) : (A \wedge B) \Rightarrow (C \wedge D)$。

---

### **4.8. 练习**

**问题**：通过为以下每个公式给出证明对象，证明它们是有效的。

1. $A \Rightarrow \lnot\lnot A$

2. $(B \vee C) \Rightarrow \lnot (\lnot B \wedge \lnot C)$

3. $(A \Rightarrow B) \Rightarrow ((A \Rightarrow C) \Rightarrow (A \Rightarrow B \wedge C))$

**解答**：

1. **证明 $A \Rightarrow \lnot\lnot A$**：
   - **定义**：$\lambda a_A . \lambda n_{\lnot A} . n\ a$。
   - **解释**：假设有 $a : A$，要证明 $\lnot\lnot A$，即 $\lnot A \Rightarrow \bot$，所以构造 $\lambda n . n\ a$。

2. **证明 $(B \vee C) \Rightarrow \lnot (\lnot B \wedge \lnot C)$**：
   - **定义**：$\lambda p_{B \vee C} . \lambda q_{\lnot B \wedge \lnot C} . cases\ p\ (\lambda b . fst\ q\ b)\ (\lambda c . snd\ q\ c)$。
   - **解释**：
     - $p : B \vee C$，$q : \lnot B \wedge \lnot C$。
     - 如果 $p$ 是 $inl\ b$，则 $fst\ q : \lnot B$，矛盾 $fst\ q\ b$。
     - 如果 $p$ 是 $inr\ c$，则 $snd\ q : \lnot C$，矛盾 $snd\ q\ c$。

3. **证明 $(A \Rightarrow B) \Rightarrow ((A \Rightarrow C) \Rightarrow (A \Rightarrow B \wedge C))$**：
   - **定义**：$\lambda f_{A \Rightarrow B} . \lambda g_{A \Rightarrow C} . \lambda a_A . (f\ a,\ g\ a)$。
   - **解释**：
     - 对于 $a : A$，应用 $f$ 和 $g$ 分别得到 $B$ 和 $C$ 的成员，然后构造对 $(f\ a,\ g\ a) : B \wedge C$。

### **4.9. 练习**

**问题**：证明以下公式是等价的，通过假设其中一个证明另一个。你能想到这里涉及的函数的含义吗？

- $(A \wedge B) \Rightarrow C$
- $A \Rightarrow (B \Rightarrow C)$

**解答**：

1. **从 $(A \wedge B) \Rightarrow C$ 推导 $A \Rightarrow (B \Rightarrow C)$**：
   - **给定**：$f : (A \wedge B) \Rightarrow C$。
   - **构造函数**：
     - $\lambda a_A . \lambda b_B . f\ (a, b)$。
   - **解释**：对于 $a : A$ 和 $b : B$，构造对 $(a, b) : A \wedge B$，然后应用 $f$ 得到 $C$。

2. **从 $A \Rightarrow (B \Rightarrow C)$ 推导 $(A \wedge B) \Rightarrow C$**：
   - **给定**：$g : A \Rightarrow (B \Rightarrow C)$。
   - **构造函数**：
     - $\lambda p_{A \wedge B} . g\ (fst\ p)\ (snd\ p)$。
   - **解释**：从 $p : A \wedge B$ 提取 $fst\ p : A$ 和 $snd\ p : B$，然后应用 $g$ 得到 $C$。

**函数含义**：

- 这两个函数实际上是**柯里化（Currying）**和**反柯里化（Uncurrying）**的过程，它们之间可以相互转换。

### **4.10. 练习**

**问题**：通过给出一个类型为 $((A \Rightarrow C) \vee (B \Rightarrow C)) \Rightarrow ((A \wedge B) \Rightarrow C)$ 的对象，证明德摩根公式 $(\lnot A \vee \lnot B) \Rightarrow \lnot (A \wedge B)$ 的有效性。

**解答**：

- **注意**：在构造性逻辑中，我们用 $\lnot A$ 表示 $A \Rightarrow \bot$，因此 $(\lnot A \vee \lnot B) \Rightarrow \lnot (A \wedge B)$ 可以表示为 $((A \Rightarrow \bot) \vee (B \Rightarrow \bot)) \Rightarrow ((A \wedge B) \Rightarrow \bot)$。

- **定义**：
  - $\lambda p_{(A \Rightarrow C) \vee (B \Rightarrow C)} . \lambda q_{A \wedge B} . cases\ p\ (\lambda f . f\ (fst\ q))\ (\lambda g . g\ (snd\ q))$。
- **解释**：
  - $p$ 是 $A \Rightarrow C$ 或 $B \Rightarrow C$ 的成员。
  - $q : A \wedge B$，有 $fst\ q : A$，$snd\ q : B$。
  - 如果 $p$ 是 $A \Rightarrow C$，则应用 $f$ 于 $fst\ q$。
  - 如果 $p$ 是 $B \Rightarrow C$，则应用 $g$ 于 $snd\ q$。

### **4.11. 练习**

**问题**：为变量 $a$、$b$ 和 $c$ 指定适当的类型，给出以下判断的推导。

1. $\lambda a . \lambda b . a : A \Rightarrow (B \Rightarrow A)$
2. $\lambda a . \lambda b . \lambda c . (a\ c)\ (b\ c) : (A \Rightarrow (B \Rightarrow C)) \Rightarrow ((A \Rightarrow B) \Rightarrow (A \Rightarrow C))$

**解答**：

1. **第一个推导**：

   - **类型**：
     - $a : A$
     - $b : B$
   - **推导**：
     - $\lambda a_A . \lambda b_B . a : A \Rightarrow (B \Rightarrow A)$
   - **解释**：函数接受 $a : A$ 和 $b : B$，返回 $a : A$。

2. **第二个推导**：

   - **类型**：
     - $a : A \Rightarrow (B \Rightarrow C)$
     - $b : A \Rightarrow B$
     - $c : A$
   - **推导**：
     - **应用 $b$ 于 $c$**：$b\ c : B$
     - **应用 $a$ 于 $c$**：$a\ c : B \Rightarrow C$
     - **组合**：$(a\ c)\ (b\ c) : C$
     - **构造函数**：$\lambda a_{A \Rightarrow (B \Rightarrow C)} . \lambda b_{A \Rightarrow B} . \lambda c_A . (a\ c)\ (b\ c)$
     - **类型**：$(A \Rightarrow (B \Rightarrow C)) \Rightarrow ((A \Rightarrow B) \Rightarrow (A \Rightarrow C))$

   - **解释**：这构造了一个函数，从 $a$ 和 $b$ 构造一个新的函数，将 $c : A$ 映射到 $C$。

---

**希望以上详细的解释和推导能够帮助您理解第 4 章第 4.4 和 4.5 节的内容。如果您还有任何疑问或需要进一步的澄清，请随时告诉我！**

### ---------------------------

# **第 4 章：类型理论简介**

---

## **4.6 量词**

### **概述**

在本节中，我们引入了构造性类型理论中全称量词（∀）和存在量词（∃）的规则。在研究“存在量词”时，经典逻辑和构造性逻辑之间的差异最为明显。

### **存在量词（∃）的构造性解释**

- **构造性断言**：要构造性地断言 $\exists x.\ P$，我们需要实际找到一个对象 $x$，使得命题 $P$ 对 $x$ 成立。
- **类型化的量词**：由于我们的语言是类型化的，我们只关注**类型化或有界的量词**，形式为 $(\exists x : A). P$，其中 $A$ 是一个类型。
- **存在命题的证明**：一个存在命题 $(\exists x : A). P$ 的证明由两个部分组成：
  1. **证据（witness）**：一个 $A$ 的元素 $w$。
  2. **性质的证明**：一个证明，表明 $P$ 对 $w$ 成立，即 $P[w / x]$ 成立。

### **全称量词（∀）的构造性解释**

- **全称命题的证明**：要证明 $(\forall x : A). P$，需要对 $A$ 中的每个元素 $a$，都能证明 $P[a / x]$ 成立。
- **证明形式**：一个将任何 $A$ 的元素 $a$ 转换为 $P[a / x]$ 的证明的**函数**。

### **量词的规则**

对于量词的规则，我们仍然有四种类型：构造规则、引入规则、消除规则和计算规则。

#### **全称量词的规则**

##### **构造规则（Formation Rule）**

$$
\frac{A\ \text{是一个公式} \quad \left[ x : A \right] \vdots P\ \text{是一个公式}}{(\forall x : A). P\ \text{是一个公式}}\ (\forall F)
$$

- **解释**：
  - **第一假设**：$A$ 是一个公式或类型。
  - **第二假设**：在假设 $x : A$ 的情况下，$P$ 是一个公式。
  - **作用**：允许构造依赖于变量 $x$ 的公式 $P$。

##### **引入规则（Introduction Rule）**

$$
\frac{\left[ x : A \right] \vdots p : P}{(\lambda x : A). p : (\forall x : A). P}\ (\forall I)
$$

- **解释**：
  - 如果在假设 $x : A$ 下，我们可以构造一个 $P$ 的证明 $p$，并且 $x$ 只在这个假设中自由出现，那么我们可以构造 $(\forall x : A). P$ 的证明。
  - **释放假设**：该规则释放了假设 $x : A$。

##### **消除规则（Elimination Rule）**

$$
\frac{a : A \quad f : (\forall x : A). P}{f\ a : P[a / x]}\ (\forall E)
$$

- **解释**：
  - 如果 $f$ 是 $(\forall x : A). P$ 的证明，$a$ 是 $A$ 的一个元素，那么 $f\ a$ 是 $P[a / x]$ 的证明。

##### **计算规则（Computation Rule）**

$$
((\lambda x : A). p)\ a \to p[a / x]
$$

- **解释**：
  - 将全称量化的证明 $(\lambda x : A). p$ 应用于 $a$ 时，等价于将 $p$ 中的 $x$ 替换为 $a$。

#### **注意事项**

- **类型和公式的混用**：在这里，我们将一些表达式视为类型（如 $A$），另一些视为公式（如 $P$）。这种双重解释是可以混合的。
- **与蕴含的关系**：如果 $P$ 不依赖于 $x$，那么全称量词的规则与蕴含的规则相同。

### **依赖函数类型**

- **定义**：全称量词对应于**依赖函数类型**，即结果类型 $P[a / x]$ 依赖于输入值 $a$。
- **特点**：这在大多数现有编程语言的类型系统中是不允许的，因为它似乎会破坏类型系统的静态性质。然而，在构造性类型理论中，我们可以避免非终止的风险。
- **应用**：依赖函数类型可以用于表示类型多态性（类型的成员本身是类型），以及其他高级类型构造。

#### **存在量词的规则**

##### **构造规则（Formation Rule）**

$$
\frac{A\ \text{是一个公式} \quad \left[ x : A \right] \vdots P\ \text{是一个公式}}{(\exists x : A). P\ \text{是一个公式}}\ (\exists F)
$$

- **解释**：与全称量词的构造规则类似。

##### **引入规则（Introduction Rule）**

$$
\frac{a : A \quad p : P[a / x]}{(a, p) : (\exists x : A). P}\ (\exists I)
$$

- **解释**：
  - 如果我们有一个 $A$ 的元素 $a$，以及 $P[a / x]$ 的证明 $p$，那么 $(a, p)$ 是 $(\exists x : A). P$ 的证明。

##### **消除规则（Elimination Rules）**

由于存在量词的证明是一个对，我们可以使用投影来消除：

1. **投影第一部分**：
   $$
   \frac{p : (\exists x : A). P}{Fst\ p : A}\ (\exists E1)
   $$
2. **投影第二部分**：
   $$
   \frac{p : (\exists x : A). P}{Snd\ p : P[Fst\ p / x]}\ (\exists E2)
   $$

- **解释**：
  - 从存在量化的证明 $p$ 中，我们可以提取出元素 $a = Fst\ p$ 和对应的证明 $p = Snd\ p$。

##### **计算规则（Computation Rules）**

$$
Fst\ (a, p) \to a \\
Snd\ (a, p) \to p
$$

- **解释**：这些规则类似于合取的计算规则。

#### **存在量词的类型解释**

存在量词 $(\exists x : A). P$ 可以有多种类型解释：

1. **依赖对类型（Dependent Pair Type）**：
   - 这是二元乘积类型的泛化，其中第二个组件的类型 $P[a / x]$ 依赖于第一个组件的值 $a$。
2. **依赖和类型（Dependent Sum Type）**：
   - 可以视为类型 $P[a / x]$ 的并集，其中每个元素都与其来源 $a$ 配对。
3. **子类型的构造**：
   - 表示类型 $A$ 的一个子集，包含满足性质 $P$ 的元素 $a$，并且携带证据 $P[a / x]$。
4. **模块类型**：
   - 表示提供了某个规范 $P$ 的实现（类型为 $A$），并有证据表明实现满足规范。

#### **关于公式中自由变量的讨论**

- **自由变量**：到目前为止，我们还没有讨论如何构造含有自由变量的公式。这需要引入系统的原子命题，包括在第 4.10 节中讨论的等式，以及宇宙（Universe）的引入。

### **4.6.1 一些示例证明**

#### **示例 1：从 $(\forall x : A). (B \Rightarrow C)$ 和 $(\forall x : A). B$ 推导 $(\forall x : A). C$**

- **目标**：构造一个类型为 $(\forall x : A). C$ 的对象。
- **推导步骤**：
  1. **对全称量化进行实例化**：
     - 对于任意 $x : A$，有：
       - $r : (\forall x : A). (B \Rightarrow C)$，则 $r\ x : B \Rightarrow C$。
       - $p : (\forall x : A). B$，则 $p\ x : B$。
  2. **应用蕴含消除规则**：
     - $(r\ x)\ (p\ x) : C$。
  3. **构造函数**：
     - 对 $x$ 进行抽象，得到 $\lambda x : A . (r\ x)\ (p\ x) : (\forall x : A). C$。
  4. **进一步抽象**：
     - 对 $r$ 和 $p$ 进行抽象，得到 $\lambda r . \lambda p . \lambda x . (r\ x)\ (p\ x)$。

- **解释**：
  - 这个函数实际上是 S 组合子，在函数式编程中广为人知。

#### **示例 2：证明 $((\exists x : X). P) \Rightarrow Q$ 等价于 $(\forall x : X). (P \Rightarrow Q)$**

- **假设**：$x$ 不在 $Q$ 中自由出现。
- **证明方向一**：从 $((\exists x : X). P) \Rightarrow Q$ 推导 $(\forall x : X). (P \Rightarrow Q)$。
  1. **构造 $(\forall x : X). (P \Rightarrow Q)$**：
     - **假设**：$x : X$。
     - **需要构造**：$P \Rightarrow Q$。
     - **假设**：$p : P$。
     - **构造存在量化的证明**：$(x, p) : (\exists x : X). P$。
     - **应用假设的函数**：$e : ((\exists x : X). P) \Rightarrow Q$，得到 $e\ (x, p) : Q$。
     - **构造函数**：$\lambda p : P . e\ (x, p) : P \Rightarrow Q$。
     - **对 $x$ 进行抽象**：$\lambda x : X . \lambda p : P . e\ (x, p) : (\forall x : X). (P \Rightarrow Q)$。
  2. **注意**：
     - 需要确保在抽象时，变量的自由出现不会违反规则。

- **证明方向二**：从 $(\forall x : X). (P \Rightarrow Q)$ 推导 $((\exists x : X). P) \Rightarrow Q$。
  1. **假设**：$p : (\exists x : X). P$。
  2. **提取存在量化的组成部分**：
     - $Fst\ p : X$，$Snd\ p : P[Fst\ p / x]$。
  3. **应用全称量化的函数**：
     - $e : (\forall x : X). (P \Rightarrow Q)$。
     - $e\ (Fst\ p) : P[Fst\ p / x] \Rightarrow Q$。
  4. **应用蕴含消除规则**：
     - $(e\ (Fst\ p))\ (Snd\ p) : Q$。
  5. **构造函数**：
     - $\lambda p . (e\ (Fst\ p))\ (Snd\ p) : ((\exists x : X). P) \Rightarrow Q$。

- **函数解释**：
  - 这两个方向的函数可以视为柯里化（Currying）和反柯里化（Uncurrying）的泛化，涉及依赖和类型和依赖乘积类型。

### **练习**

#### **4.12. 练习**

**问题**：给出公式 $(\exists x : X). \lnot P \Rightarrow \lnot (\forall x : X). P$ 的证明对象。

**解答**：

- **假设**：$e : (\exists x : X). \lnot P$，需要证明 $\lnot (\forall x : X). P$。
- **目标**：构造一个函数 $f : (\forall x : X). P \Rightarrow \bot$。
- **步骤**：
  1. **提取存在量化的组成部分**：
     - $Fst\ e : X$，$Snd\ e : \lnot P[Fst\ e / x]$。
  2. **假设**：$p : (\forall x : X). P$。
  3. **应用全称量化的函数**：
     - $p\ (Fst\ e) : P[Fst\ e / x]$。
  4. **应用矛盾**：
     - $Snd\ e\ (p\ (Fst\ e)) : \bot$。
  5. **构造函数**：
     - $\lambda p . Snd\ e\ (p\ (Fst\ e))$。

- **逆命题**：不期望能够推导出逆命题，因为从 $\lnot (\forall x : X). P$ 无法构造 $(\exists x : X). \lnot P$ 的具体证据。

#### **4.13. 练习**

**问题**：证明公式 $(\forall x : X). \lnot P$ 和 $\lnot (\exists x : X). P$ 等价，给出将一个映射到另一个的两个函数。

**解答**：

1. **从 $(\forall x : X). \lnot P$ 推导 $\lnot (\exists x : X). P$**：
   - **假设**：$f : (\forall x : X). \lnot P$，需要证明 $(\exists x : X). P \Rightarrow \bot$。
   - **步骤**：
     1. **假设**：$e : (\exists x : X). P$。
     2. **提取组成部分**：$Fst\ e : X$，$Snd\ e : P[Fst\ e / x]$。
     3. **应用 $f$**：$f\ (Fst\ e) : \lnot P[Fst\ e / x]$。
     4. **应用矛盾**：$(f\ (Fst\ e))\ (Snd\ e) : \bot$。
     5. **构造函数**：$\lambda e . (f\ (Fst\ e))\ (Snd\ e)$。

2. **从 $\lnot (\exists x : X). P$ 推导 $(\forall x : X). \lnot P$**：
   - **假设**：$g : (\exists x : X). P \Rightarrow \bot$，需要证明 $(\forall x : X). \lnot P$。
   - **步骤**：
     1. **假设**：$x : X$，$p : P$。
     2. **构造存在量化的证明**：$(x, p) : (\exists x : X). P$。
     3. **应用 $g$**：$g\ (x, p) : \bot$。
     4. **构造函数**：$\lambda x . \lambda p . g\ (x, p)$。

#### **4.14. 练习**

**问题**：构造类型为 $(\forall x : X). (A \Rightarrow B) \Rightarrow ((\exists x : X). A \Rightarrow (\exists x : X). B)$ 的对象。

**解答**：

- **给定**：$f : (\forall x : X). (A \Rightarrow B)$，$e : (\exists x : X). A$。
- **步骤**：
  1. **提取存在量化的组成部分**：$Fst\ e : X$，$Snd\ e : A$。
  2. **应用 $f$**：$f\ (Fst\ e) : A \Rightarrow B$。
  3. **应用蕴含消除规则**：$(f\ (Fst\ e))\ (Snd\ e) : B$。
  4. **构造存在量化的证明**：$(Fst\ e, (f\ (Fst\ e))\ (Snd\ e)) : (\exists x : X). B$。
  5. **构造函数**：$\lambda e . (Fst\ e, (f\ (Fst\ e))\ (Snd\ e))$。

- **当 $A$ 和 $B$ 不依赖于 $x$ 时**，这个公式表示了在存在量化下的蕴含传递。

#### **4.15. 练习**

**问题**：构造类型为 $(\exists y : Y). (\forall x : X). P \Rightarrow (\forall x : X). (\exists y : Y). P$ 的对象，其中 $P$ 一般会包含 $x$ 和 $y$ 自由出现。在哪些情况下可以推导出逆命题的对象？

**解答**：

1. **从 $(\exists y : Y). (\forall x : X). P$ 推导 $(\forall x : X). (\exists y : Y). P$**：
   - **假设**：$e : (\exists y : Y). (\forall x : X). P$。
   - **提取组成部分**：$y : Y$，$p : (\forall x : X). P$。
   - **构造函数**：$\lambda x . (y, p\ x) : (\forall x : X). (\exists y : Y). P$。

2. **逆命题**：$(\forall x : X). (\exists y : Y). P \Rightarrow (\exists y : Y). (\forall x : X). P$ 一般不成立，除非 $Y$ 非空且 $y$ 不依赖于 $x$。

3. **当 $P$ 不包含 $x$ 和 $y$ 自由出现时**，公式可以简化，表示量词交换的情况。

---

## **4.7 基本类型**

### **概述**

到目前为止，我们的系统涵盖了逻辑和类型化 λ-演算的部分，引入了量词作为逻辑构造，但具有自然的编程解释。现在，我们引入一些基本类型，其起源于编程，但也有逻辑解释。

在本节和后续章节中，我们将介绍一些熟悉的类型：

- **布尔类型**：作为有限类型的一个例子。
- **有限类型**：一般的有限类型，包括特殊情况的单元素类型。
- **自然数**：作为基础的数值类型。
- **树**：作为代数类型的一个例子。

### **4.7.1 布尔类型**

#### **定义**

- **类型**：$\text{bool}$
- **元素**：$\text{True}$ 和 $\text{False}$

#### **逻辑解释**

- **命题与类型的对应**：
  - **命题 $\top$（真）**对应于只有一个证明的类型。
  - **命题 $\bot$（假）**对应于没有证明的类型。
  - **逻辑连接词**：如 $\wedge$、$\vee$ 等，对应于类型构造器。

- **布尔类型的角色**：
  - **计算意义**：用于表示条件判断的结果，可以进行条件分支。
  - **与逻辑命题的区别**：布尔类型中的 $\text{True}$ 和 $\text{False}$ 是值，而逻辑中的 $\top$ 和 $\bot$ 是命题。

#### **规则**

##### **构造规则（Formation Rule）**

$$
\text{bool}\ \text{是一个类型}\ (\text{bool F})
$$

##### **引入规则（Introduction Rules）**

1. **真值**：
   $$
   \text{True} : \text{bool}\ (\text{bool I1})
   $$
2. **假值**：
   $$
   \text{False} : \text{bool}\ (\text{bool I2})
   $$

##### **消除规则（Elimination Rule）**

- **条件表达式**：

  $$
  \frac{tr : \text{bool} \quad c : C[\text{True} / x] \quad d : C[\text{False} / x]}{\text{if}\ tr\ \text{then}\ c\ \text{else}\ d : C[tr / x]}\ (\text{bool E})
  $$

  - **解释**：
    - 根据布尔值 $tr$，选择 $c$ 或 $d$。
    - 类型 $C$ 可以依赖于 $x$，但目前我们假设 $C$ 不依赖于 $x$。

##### **计算规则（Computation Rules）**

$$
\text{if}\ \text{True}\ \text{then}\ c\ \text{else}\ d \to c \\
\text{if}\ \text{False}\ \text{then}\ c\ \text{else}\ d \to d
$$

- **解释**：根据条件的值，选择相应的分支。

#### **示例**

- **定义布尔运算**：
  - **非运算（negation）**：
    $$
    \lambda tr_{\text{bool}} .\ \text{if}\ tr\ \text{then}\ \text{False}\ \text{else}\ \text{True} : \text{bool} \Rightarrow \text{bool}
    $$
  - **与运算（conjunction）**：
    $$
    \lambda (x_{\text{bool}}, y_{\text{bool}}) .\ \text{if}\ x\ \text{then}\ y\ \text{else}\ \text{False} : \text{bool} \wedge \text{bool} \Rightarrow \text{bool}
    $$

- **注意**：布尔类型的操作与逻辑连接词（如 $\wedge$、$\vee$）不同，后者是类型构造器。

### **4.7.2 有限类型**

#### **定义**

- **类型**：对于自然数 $n$，定义有限类型 $N_n$。
- **元素**：$n$ 个元素，记为 $1_n, 2_n, \ldots, n_n$。

#### **规则**

##### **构造规则（Formation Rule）**

$$
N_n\ \text{是一个类型}\ (N_n F)
$$

##### **引入规则（Introduction Rules）**

- **每个元素**：

  $$
  1_n : N_n\ (N_n I1) \\
  \vdots \\
  n_n : N_n\ (N_n In)
  $$

##### **消除规则（Elimination Rule）**

- **多路分支**：

  $$
  \frac{e : N_n \quad c_1 : C[1_n / x] \quad \ldots \quad c_n : C[n_n / x]}{\text{cases}_n\ e\ c_1\ \ldots\ c_n : C[e / x]}\ (N_n E)
  $$

- **解释**：根据 $e$ 的值，选择对应的 $c_i$。

##### **计算规则（Computation Rules）**

$$
\text{cases}_n\ 1_n\ c_1\ \ldots\ c_n \to c_1 \\
\vdots \\
\text{cases}_n\ n_n\ c_1\ \ldots\ c_n \to c_n
$$

#### **示例**

- **布尔类型的特例**：当 $n = 2$ 时，$N_2$ 即布尔类型。

### **4.7.3 单元类型 $\top$ 和空类型 $\bot$**

#### **单元类型 $\top$**

- **定义**：只有一个元素的类型。

##### **构造规则（Formation Rule）**

$$
\top\ \text{是一个类型}\ (\top F)
$$

##### **引入规则（Introduction Rule）**

$$
\text{Triv} : \top\ (\top I)
$$

##### **消除规则（Elimination Rule）**

$$
\frac{x : \top \quad c : C(\text{Triv})}{\text{case}\ x\ c : C(x)}\ (\top E)
$$

##### **计算规则（Computation Rule）**

$$
\text{case}\ x\ c \to c
$$

- **解释**：由于 $x : \top$ 只有一个可能的值 $\text{Triv}$，所以消除规则直接返回 $c$。

#### **空类型 $\bot$**

- **当 $n = 0$ 时**，有限类型 $N_n$ 退化为空类型 $\bot$，对应于没有元素的类型。

#### **逻辑解释**

- **单元类型 $\top$** 对应于逻辑中的“真”，因为只有一个平凡的证明。
- **空类型 $\bot$** 对应于逻辑中的“假”，因为没有任何证明。

---

## **练习**

### **4.16. 练习**

**问题**：定义在布尔类型上的“或”（disjunction）和“蕴含”（implies）操作的函数。

**解答**：

1. **或操作**：
   $$
   \text{or} : \text{bool} \Rightarrow \text{bool} \Rightarrow \text{bool}
   $$
   定义：
   $$
   \text{or} = \lambda x_{\text{bool}} . \lambda y_{\text{bool}} . \text{if}\ x\ \text{then}\ \text{True}\ \text{else}\ y
   $$

2. **蕴含操作**：
   $$
   \text{implies} = \lambda x_{\text{bool}} . \lambda y_{\text{bool}} . \text{if}\ x\ \text{then}\ y\ \text{else}\ \text{True}
   $$

### **4.17. 练习**

**问题**：定义函数 $\text{equiv} : \text{bool} \Rightarrow \text{bool} \Rightarrow \text{bool}$，使得当且仅当 $b_1$ 和 $b_2$ 相等时，$\text{equiv}\ b_1\ b_2$ 为 $\text{True}$。

**解答**：

- 定义：
  $$
  \text{equiv} = \lambda b_1 . \lambda b_2 . \text{if}\ b_1\ \text{then}\ b_2\ \text{else}\ \text{not}\ b_2
  $$
- **其中**：
  - $\text{not} = \lambda b . \text{if}\ b\ \text{then}\ \text{False}\ \text{else}\ \text{True}$。

### **4.18. 练习**

**问题**：解释如何定义函数 $\text{equal}_n : N_n \Rightarrow N_n \Rightarrow \text{bool}$。

**解答**：

- **思路**：
  - 对于有限类型 $N_n$，可以通过比较两个元素是否相等，返回布尔值。
- **定义**：
  - 使用多路分支，比较两个元素的值。
  - 定义函数：
    $$
    \text{equal}_n = \lambda a . \lambda b . \text{cases}_n\ a\ (\text{cases}_n\ b\ \text{True}\ \text{False}\ \ldots)\ \ldots
    $$
  - 具体实现需要对所有可能的组合进行穷举。

### **4.19. 练习**

**问题**：定义类型为 $N_n \Rightarrow N_n$ 的函数 $\text{succ}_n$ 和 $\text{pred}_n$，并证明对于所有 $m_n$，有 $\text{succ}_n (\text{pred}_n\ m_n) \to\!\!\to m_n$。

**解答**：

- **定义**：
  - **后继函数 $\text{succ}_n$**：
    - 若 $m_n \neq n_n$，则 $\text{succ}_n\ m_n = (m+1)_n$。
    - 若 $m_n = n_n$，则 $\text{succ}_n\ n_n = 1_n$。
  - **前驱函数 $\text{pred}_n$**：
    - 若 $m_n \neq 1_n$，则 $\text{pred}_n\ m_n = (m-1)_n$。
    - 若 $m_n = 1_n$，则 $\text{pred}_n\ 1_n = n_n$。
- **证明**：
  - 对于任意 $m_n$，计算 $\text{succ}_n (\text{pred}_n\ m_n)$ 将返回 $m_n$。

### **4.20. 练习**

**问题**：证明当 $n = 0$ 时，$N_n$ 的规则退化为 $\bot$ 的规则。

**解答**：

- **当 $n = 0$ 时**：
  - **构造规则**：$N_0$ 是一个类型，相当于 $\bot$。
  - **引入规则**：没有元素，因此没有引入规则。
  - **消除规则**：没有元素可以消除，与 $\bot$ 的消除规则类似。
- **结论**：$N_0$ 与 $\bot$ 的规则相同。

---

**希望以上详细的解释和推导能够帮助您理解第 4 章第 4.6 和 4.7 节的内容。如果您还有任何疑问或需要进一步的澄清，请随时告诉我！**

### ----------------------------

# **第 4 章：类型理论简介**

---

## **4.8 自然数**

### **概述**

在第 2 章中，我们已经将自然数作为我们的 λ-演算的基本类型之一。本节将详细探讨自然数在类型理论中的定义、操作和逻辑解释。

### **自然数的定义**

#### **构造规则（Formation Rule）**

$$
\text{N 是一个类型}\ (\text{N F})
$$

- **解释**：我们引入一个类型 $\text{N}$，表示自然数的集合。

#### **引入规则（Introduction Rules）**

1. **零元素**：
   $$
   0 : \text{N}\ (\text{N I1})
   $$
2. **后继元素**：
   $$
   \frac{n : \text{N}}{\text{succ}\ n : \text{N}}\ (\text{N I2})
   $$

- **解释**：
  - **零元素**：$0$ 是自然数类型 $\text{N}$ 的一个元素。
  - **后继函数**：对于任意自然数 $n$，其后继 $\text{succ}\ n$ 也是自然数。

### **自然数的消除规则**

#### **消除规则（Elimination Rule for N）**

**特殊情况**：

$$
\frac{n : \text{N} \quad c : C \quad f : \text{N} \Rightarrow C \Rightarrow C}{\text{prim}\ n\ c\ f : C}\ (\text{N E})
$$

- **解释**：
  - **目的**：定义一个基于自然数 $n$ 的值 $\text{prim}\ n\ c\ f$，类型为 $C$。
  - **参数**：
    - $n : \text{N}$：一个自然数。
    - $c : C$：基例（base case），即当 $n = 0$ 时的值。
    - $f : \text{N} \Rightarrow C \Rightarrow C$：递归函数，定义如何从前一个值得到下一个值。

**一般情况**：

为了更全面地理解消除规则，我们引入一个更一般的形式，与数学归纳法对应。

$$
\frac{n : \text{N} \quad c : C[0 / x] \quad f : (\forall n : \text{N})(C[n / x] \Rightarrow C[\text{succ}\ n / x])}{\text{prim}\ n\ c\ f : C[n / x]}\ (\text{N E})
$$

- **解释**：
  - **目标**：构造一个类型为 $C[n / x]$ 的对象，即证明 $C(n)$ 对任意 $n$ 成立。
  - **参数**：
    - $c : C[0 / x]$：证明 $C(0)$ 成立的对象。
    - $f : (\forall n : \text{N})(C[n / x] \Rightarrow C[\text{succ}\ n / x])$：对任意 $n$，如果 $C(n)$ 成立，则 $C(n + 1)$ 也成立。

### **计算规则（Computation Rules for N）**

$$
\begin{align*}
\text{prim}\ 0\ c\ f &\to c \\
\text{prim}\ (\text{succ}\ n)\ c\ f &\to f\ n\ (\text{prim}\ n\ c\ f)
\end{align*}
$$

- **解释**：
  - **基例计算**：当 $n = 0$ 时，$\text{prim}\ 0\ c\ f$ 简化为 $c$。
  - **递归计算**：当 $n = \text{succ}\ n$ 时，$\text{prim}\ (\text{succ}\ n)\ c\ f$ 简化为 $f\ n\ (\text{prim}\ n\ c\ f)$，即递归地应用 $f$。

### **自然数的逻辑视角**

- **数学归纳法**：消除规则体现了数学归纳法的原理。
  - **要证明**：对于所有自然数 $n$，性质 $C(n)$ 成立。
  - **步骤**：
    1. **基例**：证明 $C(0)$ 成立。
    2. **归纳步骤**：假设 $C(n)$ 成立，证明 $C(n + 1)$ 成立。

- **对应关系**：在类型理论中，归纳法的证明对象与递归定义的函数对象是相同的规则。

### **示例**

#### **1. 后继函数**

- **定义**：
  $$
  \text{succ\_func} \equiv \lambda x : \text{N} . \text{succ}\ x
  $$
- **解释**：直接定义一个函数，将自然数 $x$ 映射到其后继 $\text{succ}\ x$。

#### **2. 使用原始递归定义后继函数**

- **定义**：
  $$
  \text{addone} \equiv \lambda x : \text{N} . \text{prim}\ x\ (\text{succ}\ 0)\ f
  $$
  其中，
  $$
  f \equiv \lambda n : \text{N} . \lambda y : \text{N} . \text{succ}\ y
  $$
- **解释**：
  - **基例**：当 $x = 0$ 时，$\text{addone}\ 0 = \text{succ}\ 0 = 1$。
  - **递归**：当 $x = \text{succ}\ n$ 时，$\text{addone}\ (\text{succ}\ n) = \text{succ}\ (\text{addone}\ n)$。

- **计算示例**：
  - **计算 $\text{addone}\ (\text{succ}\ (\text{succ}\ 0))$**：
    1. 应用 $\text{addone}$：
       $$
       \text{addone}\ (\text{succ}\ (\text{succ}\ 0)) \to \text{prim}\ (\text{succ}\ (\text{succ}\ 0))\ (\text{succ}\ 0)\ f
       $$
    2. 根据计算规则：
       $$
       \text{prim}\ (\text{succ}\ (\text{succ}\ 0))\ (\text{succ}\ 0)\ f \to f\ (\text{succ}\ 0)\ (\text{prim}\ (\text{succ}\ 0)\ (\text{succ}\ 0)\ f)
       $$
    3. 继续展开，最终得到：
       $$
       \text{addone}\ (\text{succ}\ (\text{succ}\ 0)) \to \text{succ}\ (\text{succ}\ (\text{succ}\ 0))
       $$

#### **3. 加法**

- **定义**：
  $$
  \text{add}\ m\ 0 = m \\
  \text{add}\ m\ (\text{succ}\ n) = \text{succ}\ (\text{add}\ m\ n)
  $$
- **形式化定义**：
  $$
  \text{add} \equiv \lambda m . \lambda n . \text{prim}\ n\ m\ (\lambda p . \lambda q . \text{succ}\ q)
  $$

#### **4. 乘法**

- **定义**：
  $$
  \text{mult}\ m\ 0 = 0 \\
  \text{mult}\ m\ (\text{succ}\ n) = \text{add}\ m\ (\text{mult}\ m\ n)
  $$
- **形式化定义**：
  $$
  \text{mult} \equiv \lambda m . \lambda n . \text{prim}\ n\ 0\ (\lambda p . \lambda q . \text{add}\ m\ q)
  $$

#### **5. Ackermann 函数**

- **定义**：
  $$
  \begin{align*}
  \text{ack}\ 0\ n &= n + 1 \\
  \text{ack}\ (m + 1)\ 0 &= 1 \\
  \text{ack}\ (m + 1)\ (n + 1) &= \text{ack}\ m\ (\text{ack}\ (m + 1)\ n)
  \end{align*}
  $$
- **高阶定义**：
  - 定义递归函数：
    $$
    \text{ack}\ 0 = \text{succ} \\
    \text{ack}\ (m + 1) = \text{iter}\ (\text{ack}\ m)
    $$
  - **迭代函数 $\text{iter}$**：
    $$
    \text{iter}\ f\ 0 = 1 \\
    \text{iter}\ f\ (\text{succ}\ n) = f\ (\text{iter}\ f\ n)
    $$
  - **形式化定义**：
    $$
    \text{iter} \equiv \lambda f_{\text{N} \Rightarrow \text{N}} . \lambda n_{\text{N}} . \text{prim}\ n\ 1\ (\lambda p . \lambda q . f\ q)
    $$
    $$
    \text{ack} \equiv \lambda n_{\text{N}} . \text{prim}\ n\ \text{succ}\ (\lambda p . \lambda g . \text{iter}\ g)
    $$

### **递归定义的表达能力**

- **限制**：原始递归定义的函数都是**全函数**，即对于每个输入都有输出。
- **不可表达的函数**：某些直观上可计算的函数无法用一阶原始递归定义，例如 Ackermann 函数。
- **更高的阶**：通过允许高阶函数参数，我们可以定义更复杂的函数，但仍有表达能力的限制。

### **证明示例**

由于我们还没有定义包含自由变量的谓词，因此无法给出非平凡的归纳证明。这将在第 4.10 节中解决。

### **练习**

#### **4.21. 定义等于函数**

**问题**：定义函数 $\text{equalN} : \text{N} \Rightarrow \text{N} \Rightarrow \text{bool}$。

**解答**：

- **思路**：
  - 比较两个自然数 $n$ 和 $m$，如果相等，则返回 $\text{True}$，否则返回 $\text{False}$。
- **递归定义**：
  $$
  \begin{align*}
  \text{equalN}\ 0\ 0 &= \text{True} \\
  \text{equalN}\ 0\ (\text{succ}\ m) &= \text{False} \\
  \text{equalN}\ (\text{succ}\ n)\ 0 &= \text{False} \\
  \text{equalN}\ (\text{succ}\ n)\ (\text{succ}\ m) &= \text{equalN}\ n\ m
  \end{align*}
  $$
- **形式化定义**：
  $$
  \text{equalN} \equiv \lambda n . \lambda m . \text{prim}\ n\ (\lambda m . \text{prim}\ m\ \text{True}\ (\lambda p . \text{False}))\ (\lambda n' . \lambda f . \lambda m . \text{prim}\ m\ \text{False}\ (\lambda m' . f\ m'))
  $$

#### **4.22. 定义大于等于函数**

**问题**：定义函数 $\text{geq} : \text{N} \Rightarrow \text{N} \Rightarrow \text{bool}$，使得当且仅当 $n \geq m$ 时，$\text{geq}\ n\ m$ 为 $\text{True}$。

**解答**：

- **思路**：
  - 递归地比较 $n$ 和 $m$。
- **递归定义**：
  $$
  \begin{align*}
  \text{geq}\ 0\ 0 &= \text{True} \\
  \text{geq}\ 0\ (\text{succ}\ m) &= \text{False} \\
  \text{geq}\ (\text{succ}\ n)\ 0 &= \text{True} \\
  \text{geq}\ (\text{succ}\ n)\ (\text{succ}\ m) &= \text{geq}\ n\ m
  \end{align*}
  $$
- **形式化定义**：
  - 定义函数 $\text{geq}$ 使用 $n$ 上的递归。

#### **4.23. 定义有界搜索函数**

**问题**：使用 $\text{geq}$ 或其他方法，定义函数 $\text{search} : (\text{N} \Rightarrow \text{bool}) \Rightarrow \text{N} \Rightarrow \text{N}$，使得 $\text{search}\ p\ n$ 是最小的 $l \leq n$，使得 $p\ l = \text{True}$，如果不存在这样的 $l$，则返回 $n$。

**解答**：

- **思路**：
  - 对 $l$ 从 $0$ 到 $n$ 进行递归，检查 $p\ l$ 是否为 $\text{True}$。
- **递归定义**：
  $$
  \begin{align*}
  \text{search}\ p\ 0 &= \text{if}\ p\ 0\ \text{then}\ 0\ \text{else}\ 0 \\
  \text{search}\ p\ (\text{succ}\ n) &= \text{if}\ p\ (\text{succ}\ n)\ \text{then}\ (\text{succ}\ n)\ \text{else}\ \text{search}\ p\ n
  \end{align*}
  $$

#### **4.24. 定义累加函数**

**问题**：给出函数 $\text{sumf}$ 的形式化定义，其中：

$$
\text{sumf}\ f\ n\ m \equiv \sum_{i = n}^{m} f\ i
$$

并确定其类型。部分应用 $\text{sumf}\ \text{id}$ 的类型是什么？

**解答**：

- **类型**：
  - $\text{sumf} : (\text{N} \Rightarrow \text{N}) \Rightarrow \text{N} \Rightarrow \text{N} \Rightarrow \text{N}$
- **定义**：
  - 递归地定义 $\text{sumf}\ f\ n\ m$。
  
  $$
  \begin{align*}
  \text{sumf}\ f\ n\ 0 &= 0 \\
  \text{sumf}\ f\ n\ (\text{succ}\ m) &= \text{sumf}\ f\ n\ m + f\ (\text{succ}\ m)
  \end{align*}
  $$
- **部分应用的类型**：
  - $\text{sumf}\ \text{id} : \text{N} \Rightarrow \text{N} \Rightarrow \text{N}$

---

## **4.9 良基类型——树**

### **概述**

本节探讨如何将递归数据类型（如列表和树）引入系统。机制类似于 Miranda 语言中的代数类型机制。我们将以树为例，展示如何定义递归数据类型，并使用结构归纳法和原始递归来证明性质和定义函数。

### **良基类型的定义**

- **良基性**：在定义递归函数或进行归纳证明时，我们需要确保不存在无限的简化序列，即递归过程总能在有限步内结束。

### **树类型的定义**

#### **Miranda 语言中的类型定义**

- **布尔类型**：
  $$
  \text{bool} ::= \text{True} \mid \text{False}
  $$
- **自然数类型**：
  $$
  \text{nat} ::= \text{Zero} \mid \text{Succ}\ \text{nat}
  $$
- **树类型**：
  $$
  \text{tree} ::= \text{Null} \mid \text{Bnode}\ \text{nat}\ \text{tree}\ \text{tree}
  $$

#### **在类型理论中的定义**

##### **构造规则（Formation Rule for tree）**

$$
\text{tree 是一个类型}\ (\text{tree F})
$$

##### **引入规则（Introduction Rules for tree）**

1. **空节点**：
   $$
   \text{Null} : \text{tree}\ (\text{tree I1})
   $$
2. **二叉节点**：
   $$
   \frac{n : \text{N} \quad u : \text{tree} \quad v : \text{tree}}{\text{Bnode}\ n\ u\ v : \text{tree}}\ (\text{tree I2})
   $$

- **解释**：
  - **空树**：$\text{Null}$ 是树类型的一个元素，表示空树。
  - **二叉节点**：给定一个自然数 $n$ 和两个子树 $u$ 和 $v$，可以构造一个二叉树节点 $\text{Bnode}\ n\ u\ v$。

### **结构归纳法和原始递归**

#### **结构归纳法（Structural Induction）**

- **目标**：证明对于所有树 $t$，性质 $P(t)$ 成立。
- **步骤**：
  1. **基例**：证明 $P(\text{Null})$ 成立。
  2. **归纳步骤**：假设 $P(u)$ 和 $P(v)$ 成立，证明 $P(\text{Bnode}\ n\ u\ v)$ 成立。

#### **原始递归（Primitive Recursion）**

- **目标**：定义一个函数 $f : \text{tree} \rightarrow P$。
- **步骤**：
  1. **基例**：指定 $f(\text{Null}) = a$，其中 $a : P$。
  2. **递归步骤**：定义 $f(\text{Bnode}\ n\ u\ v) = F\ n\ u\ v\ (f\ u)\ (f\ v)$，其中 $F : \text{N} \rightarrow \text{tree} \rightarrow \text{tree} \rightarrow P \rightarrow P \rightarrow P$。

- **在类型理论中的表示**：
  - **归纳函数**：
    $$
    F : (\forall n : \text{N})(\forall u : \text{tree})(\forall v : \text{tree})(P(u) \Rightarrow P(v) \Rightarrow P(\text{Bnode}\ n\ u\ v))
    $$

### **消除规则（Elimination Rule for tree）**

$$
\frac{\begin{array}{c}
t : \text{tree} \\
c : C[\text{Null} / x] \\
f : (\forall n : \text{N})(\forall u : \text{tree})(\forall v : \text{tree})(C[u / x] \Rightarrow C[v / x] \Rightarrow C[\text{Bnode}\ n\ u\ v / x])
\end{array}}{\text{trec}\ t\ c\ f : C[t / x]}\ (\text{tree E})
$$

- **解释**：
  - **目的**：对树 $t$ 进行递归定义或证明，目标类型为 $C[t / x]$。
  - **参数**：
    - $c : C[\text{Null} / x]$：基例，处理空树的情况。
    - $f$：递归函数，定义如何从子树的结果构造当前树的结果。

### **计算规则（Computation Rules for tree）**

$$
\begin{align*}
\text{trec}\ \text{Null}\ c\ f &\to c \\
\text{trec}\ (\text{Bnode}\ n\ u\ v)\ c\ f &\to f\ n\ u\ v\ (\text{trec}\ u\ c\ f)\ (\text{trec}\ v\ c\ f)
\end{align*}
$$

- **解释**：
  - **基例计算**：当 $t = \text{Null}$ 时，$\text{trec}\ \text{Null}\ c\ f$ 简化为 $c$。
  - **递归计算**：当 $t = \text{Bnode}\ n\ u\ v$ 时，递归地应用 $f$ 到子树的结果。

### **示例**

#### **树中元素的求和**

- **定义**：
  $$
  \begin{align*}
  \text{sumt}\ \text{Null} &= 0 \\
  \text{sumt}\ (\text{Bnode}\ n\ u\ v) &= n + \text{sumt}\ u + \text{sumt}\ v
  \end{align*}
  $$
- **形式化定义**：
  - 定义递归函数 $f$：
    $$
    f \equiv \lambda n . \lambda t_1 . \lambda t_2 . \lambda s_1 . \lambda s_2 . n + s_1 + s_2
    $$
  - 定义 $\text{sumt}$ 函数：
    $$
    \text{sumt} \equiv \lambda t_{\text{tree}} . \text{trec}\ t\ 0\ f
    $$

### **练习**

#### **4.25. 定义返回左子树的函数**

**问题**：定义一个函数，返回树的左子树（如果有），否则返回 $\text{Null}$ 树。

**解答**：

- **定义**：
  $$
  \begin{align*}
  \text{left}\ \text{Null} &= \text{Null} \\
  \text{left}\ (\text{Bnode}\ n\ u\ v) &= u
  \end{align*}
  $$
- **形式化定义**：
  - 定义递归函数 $f$：
    $$
    f \equiv \lambda n . \lambda u . \lambda v . \lambda l_u . \lambda l_v . u
    $$
  - 定义 $\text{left}$ 函数：
    $$
    \text{left} \equiv \lambda t_{\text{tree}} . \text{trec}\ t\ \text{Null}\ f
    $$

#### **4.26. 定义树的等于函数**

**问题**：定义树类型上的等于函数。

**解答**：

- **思路**：
  - 递归地比较两棵树，如果对应的节点和子树都相等，则两棵树相等。
- **递归定义**：
  $$
  \begin{align*}
  \text{equalTree}\ \text{Null}\ \text{Null} &= \text{True} \\
  \text{equalTree}\ \text{Null}\ (\text{Bnode}\ n\ u\ v) &= \text{False} \\
  \text{equalTree}\ (\text{Bnode}\ n\ u\ v)\ \text{Null} &= \text{False} \\
  \text{equalTree}\ (\text{Bnode}\ n_1\ u_1\ v_1)\ (\text{Bnode}\ n_2\ u_2\ v_2) &= \text{if}\ \text{equalN}\ n_1\ n_2\ \text{then}\ (\text{equalTree}\ u_1\ u_2\ \text{and}\ \text{equalTree}\ v_1\ v_2)\ \text{else}\ \text{False}
  \end{align*}
  $$
- **形式化定义**：
  - 使用原始递归定义，构造比较函数。

#### **4.27. 定义判断树是否有序的函数**

**问题**：定义函数 $\text{ordered} : \text{tree} \Rightarrow \text{bool}$，判断一棵树是否有序。

**解答**：

- **定义**：
  - **有序树的定义**：
    - $\text{Null}$ 是有序的。
    - $\text{Bnode}\ n\ u\ v$ 满足：
      - 所有 $u$ 中的元素小于等于 $n$。
      - 所有 $v$ 中的元素大于等于 $n$。
      - $u$ 和 $v$ 都是有序的。
- **递归定义**：
  - 定义辅助函数 $\text{leq} : \text{N} \Rightarrow \text{N} \Rightarrow \text{bool}$，用于比较自然数的大小。
  - 定义 $\text{ordered}$ 函数，使用递归，检查上述条件。

#### **4.28. 定义插入和删除函数**

**问题**：定义函数 $\text{insert}$ 和 $\text{delete} : \text{N} \Rightarrow \text{tree} \Rightarrow \text{tree}$，分别用于在有序树中插入和删除元素，保持树的有序性。

**解答**：

- **插入函数 $\text{insert}$**：
  - **递归定义**：
    $$
    \begin{align*}
    \text{insert}\ n\ \text{Null} &= \text{Bnode}\ n\ \text{Null}\ \text{Null} \\
    \text{insert}\ n\ (\text{Bnode}\ m\ u\ v) &= \begin{cases}
    \text{Bnode}\ m\ (\text{insert}\ n\ u)\ v, & \text{if}\ n \leq m \\
    \text{Bnode}\ m\ u\ (\text{insert}\ n\ v), & \text{if}\ n > m
    \end{cases}
    \end{align*}
    $$
- **删除函数 $\text{delete}$**：
  - **递归定义**：
    - 需要处理删除节点后如何重新组织树，保持有序性。
    - 详细定义涉及多种情况，可根据标准的二叉搜索树删除算法进行。

---

**希望以上详细的解释和推导能够帮助您理解第 4 章第 4.8 和 4.9 节的内容。如果您还有任何疑问或需要进一步的澄清，请随时告诉我！**

### ---------------------------

# **第 4 章：类型理论简介**

---

## **4.10 等式**

### **概述**

到目前为止，我们已经引入了一些类型构造器或逻辑运算，它们可以绑定公式中的自由变量。然而，我们还没有包含自由变量的公式，也就是说，我们缺少原始谓词。本节将弥补这一缺陷，引入**等式命题**。为了表示：

> “$a$ 和 $b$ 是类型 $A$ 的相等元素”

我们写作：

- $a =_A b$

或者，为了强调这是系统中的一个命题或类型，我们有时使用 Martin-Löf 的记号：

- $I(A, a, b)$

这样可以避免过度使用等号符号“=”。

引入等式类型 $I(A, a, b)$ 对系统的形式和直觉都有深远的影响。我们将通过引入该类型并研究其应用的示例来理解这一点。

### **等式类型的规则**

#### **构造规则（Formation Rule for I）**

$$
\frac{A\ \text{是一个类型} \quad a : A \quad b : A}{I(A, a, b)\ \text{是一个类型}}\ (\text{I F})
$$

- **解释**：
  - 如果 $a$ 和 $b$ 都是类型 $A$ 的元素，那么 $I(A, a, b)$ 就是一个类型。
  - 这是与我们之前看到的类型（或公式）构造规则不同的地方。以前的规则形式为：

    $$
    \frac{\text{... 是一个类型} \quad \text{... 是一个类型}}{\text{... 是一个类型}}
    $$

    - 这些规则仅依赖于类型本身，而不依赖于类型中具体的元素。
  - 但在等式类型的构造规则中，$I(A, a, b)$ 的构造需要知道 $a : A$ 和 $b : A$，即需要知道类型中的具体元素。

- **影响**：
  - 这意味着生成公式的规则与推导的规则紧密地交织在一起，这解释了我们没有单独表示公式（或类型）的语法的原因。

#### **引入规则（Introduction Rule for I）**

$$
\frac{a : A}{r(a) : I(A, a, a)}\ (\text{I I})
$$

- **解释**：
  - **反身性**：任何元素 $a : A$，都可以证明 $I(A, a, a)$，即 $a$ 等于自身。
  - **证明对象**：$r(a)$ 是 $I(A, a, a)$ 的一个元素。

- **注意**：
  - $r(a)$ 没有内部结构，其存在性本身允许我们进行一些其他情况下无法完成的推导。
  - 这看似无用，但实际上非常关键。

#### **消除规则（Elimination Rule for I）**

$$
\frac{c : I(A, a, b) \quad d : C(a, a, r(a))}{J(c, d) : C(a, b, c)}\ (\text{I E})
$$

- **解释**：
  - **目的**：如果我们知道 $a$ 和 $b$ 相等，那么我们希望在某个依赖于 $a$ 的命题 $C$ 中，将 $a$ 替换为 $b$。
  - **Leibniz 定律**：等同的元素可以互相替换。
  - **参数**：
    - $c : I(A, a, b)$：$a$ 和 $b$ 的相等性证明。
    - $d : C(a, a, r(a))$：在 $a$ 自身的情况下，命题 $C$ 的证明，依赖于 $r(a)$。
  - **结论**：得到 $J(c, d) : C(a, b, c)$，即在 $a$ 和 $b$ 相等的情况下，命题 $C$ 的证明。

- **解释细节**：
  - **C**：是一个可能依赖于 $a$、$b$ 和等式证明对象的命题 $C(a, b, c)$。
  - **等式证明的传递**：通过 $J$ 操作符，我们可以将证明从 $a$ 转移到 $b$。

#### **计算规则（Computation Rule for I）**

$$
J(r(a), d) \to d
$$

- **解释**：
  - 当等式证明对象是 $r(a)$ 时，$J$ 操作符简化为直接返回 $d$。

### **等式的性质**

#### **反身性（Reflexivity）**

- **引入规则**已经表明了反身性：对于任何 $a : A$，都有 $r(a) : I(A, a, a)$。

#### **对称性（Symmetry）**

- **目标**：从 $c : I(A, a, b)$ 推导出 $I(A, b, a)$。

- **证明**：

  - **设定**：
    - 定义命题 $C(a, b, c) \equiv I(A, b, a)$。
  - **步骤**：
    1. **计算 $C(a, a, r(a))$**：
       - $C(a, a, r(a)) \equiv I(A, a, a)$。
       - 我们有 $r(a) : I(A, a, a)$。
    2. **应用消除规则**：
       - $J(c, r(a)) : I(A, b, a)$。
       - 其中 $c : I(A, a, b)$，$r(a) : I(A, a, a)$。
  - **结论**：证明了 $I(A, b, a)$。

- **符号化**：

  $$
  \frac{c : I(A, a, b) \quad r(a) : I(A, a, a)}{J(c, r(a)) : I(A, b, a)}\ (\text{I E})
  $$

#### **传递性（Transitivity）**

- **目标**：从 $w : I(A, a, b)$ 和 $z : I(A, b, c)$ 推导出 $I(A, a, c)$。

- **证明**：

  - **设定**：
    - 定义命题 $C(b, c, z) \equiv I(A, a, c)$。
  - **步骤**：
    1. **计算 $C(b, b, r(b))$**：
       - $C(b, b, r(b)) \equiv I(A, a, b)$。
       - 我们有 $w : I(A, a, b)$。
    2. **应用消除规则**：
       - $J(z, w) : I(A, a, c)$。
       - 其中 $z : I(A, b, c)$，$w : I(A, a, b)$。
  - **结论**：证明了 $I(A, a, c)$。

- **符号化**：

  $$
  \frac{z : I(A, b, c) \quad w : I(A, a, b)}{J(z, w) : I(A, a, c)}\ (\text{I E})
  $$

### **等式在基本类型上的应用**

#### **示例 1：布尔类型**

- **目标**：证明对任意 $x : \text{bool}$，要么 $x =_{\text{bool}} \text{True}$，要么 $x =_{\text{bool}} \text{False}$。

- **公式**：

  $$
  (\forall x : \text{bool}). (x =_{\text{bool}} \text{True} \lor x =_{\text{bool}} \text{False})
  $$

- **证明**：

  1. **假设**：$x : \text{bool}$。
  2. **定义命题**：$C \equiv (x =_{\text{bool}} \text{True} \lor x =_{\text{bool}} \text{False})$。
  3. **构造 $c$ 和 $d$**：
     - $c \equiv \text{inl}\ r(\text{True}) : C[\text{True} / x]$，因为 $r(\text{True}) : I(\text{bool}, \text{True}, \text{True})$。
     - $d \equiv \text{inr}\ r(\text{False}) : C[\text{False} / x]$，因为 $r(\text{False}) : I(\text{bool}, \text{False}, \text{False})$。
  4. **应用布尔类型的消除规则**：
     - $\text{if}\ x\ \text{then}\ c\ \text{else}\ d : C$。
  5. **构造函数**：
     - $\lambda x . \text{if}\ x\ \text{then}\ c\ \text{else}\ d : (\forall x : \text{bool}). C$。

- **结论**：每个布尔值 $x$ 要么等于 $\text{True}$，要么等于 $\text{False}$。

#### **示例 2：自然数类型**

- **目标**：证明对任意 $x : \text{N}$，要么 $x =_{\text{N}} 0$，要么存在 $y : \text{N}$，使得 $x =_{\text{N}} \text{succ}\ y$。

- **公式**：

  $$
  (\forall x : \text{N}). (x =_{\text{N}} 0 \lor (\exists y : \text{N}). x =_{\text{N}} \text{succ}\ y)
  $$

- **证明**（练习 4.29，详细解答在后面）：

  1. **假设**：$x : \text{N}$。
  2. **定义命题**：$C \equiv (x =_{\text{N}} 0 \lor (\exists y : \text{N}). x =_{\text{N}} \text{succ}\ y)$。
  3. **基例（$x = 0$）**：
     - $c \equiv \text{inl}\ r(0) : C[0 / x]$，因为 $r(0) : I(\text{N}, 0, 0)$。
  4. **归纳步骤（$x = \text{succ}\ n$）**：
     - 假设 $n : \text{N}$。
     - $y = n$，则 $r(\text{succ}\ n) : I(\text{N}, \text{succ}\ n, \text{succ}\ n)$。
     - 构造 $\text{inr}\ (n, r(\text{succ}\ n)) : C[\text{succ}\ n / x]$。
  5. **应用自然数的消除规则（原始递归）**：
     - 定义函数 $f$ 和 $g$，然后使用递归构造证明。

- **结论**：每个自然数 $x$ 要么等于 $0$，要么是某个自然数的后继。

### **不等式**

- **问题**：到目前为止，我们的系统没有防止每个类型只有一个元素的可能性（称为平凡系统）。为了使系统非平凡，我们可以添加一个公理，说明：

  $$
  \text{ax} : \lnot ( \text{True} =_{\text{bool}} \text{False} )
  $$

- **解释**：
  - 这表示 $\text{True}$ 和 $\text{False}$ 是不同的元素。
  - 这足以推导出其他类型的非平凡性。

- **示例**：证明 $0$ 不是任何自然数的后继。

  - **定义函数**：
    $$
    \begin{align*}
    f\ 0 &\equiv \text{True} \\
    f\ (n + 1) &\equiv \text{False}
    \end{align*}
    $$

  - **解释**：可以使用原始递归定义这个函数，然后通过与公理的组合，证明 $0$ 不是任何数的后继。

### **依赖类型**

- **引入**：通过引入等式谓词，我们能够定义非平凡的依赖类型。

#### **示例：基于布尔变量的类型族**

- **定义**：构造一个类型族，使得当 $x = \text{True}$ 时，类型为 $\text{N}$；当 $x = \text{False}$ 时，类型为 $\text{bool}$。

- **表示**：

  - **方法一**：

    $$
    (x = \text{True} \wedge \text{N}) \lor (x = \text{False} \wedge \text{bool})
    $$

    - **元素**：
      - 若 $x = \text{True}$，则元素为 $\text{inl}(r, n)$，其中 $r : I(\text{bool}, x, \text{True})$，$n : \text{N}$。
      - 若 $x = \text{False}$，则元素为 $\text{inr}(r, b)$，其中 $r : I(\text{bool}, x, \text{False})$，$b : \text{bool}$。

  - **方法二**：

    $$
    (x = \text{True} \Rightarrow \text{N}) \wedge (x = \text{False} \Rightarrow \text{bool})
    $$

    - **元素**：
      - 函数对 $(f, g)$，其中：
        - $f : (x = \text{True} \Rightarrow \text{N})$。
        - $g : (x = \text{False} \Rightarrow \text{bool})$。

- **用途**：可以根据 $x$ 的值，选择不同的类型。

#### **示例：自然数的前驱函数**

- **问题**：只有正自然数 $n + 1$ 才有前驱 $n$。我们希望定义一个只对 $x \neq 0$ 有意义的前驱函数。

- **类型表示**：

  $$
  (\forall x : \text{N}). ((x \neq 0) \Rightarrow \text{N})
  $$

  - **其中**：$x \neq 0$ 是 $\lnot (x =_{\text{N}} 0)$ 的简写。

- **定义前驱函数**：

  1. **基例**：对于 $x = 0$，需要定义一个元素 $f : (0 \neq 0) \Rightarrow \text{N}$。

     - **因为**：$0 =_{\text{N}} 0$（有 $r(0)$）。
     - **假设**：$z : (0 \neq 0)$，那么 $z\ r(0) : \bot$。
     - **构造**：$\text{abortN}(z\ r(0)) : \text{N}$。

  2. **归纳步骤**：对于 $x = \text{succ}\ n$，需要定义 $f : (\text{succ}\ n \neq 0) \Rightarrow \text{N}$。

     - **注意**：$\text{succ}\ n \neq 0$ 恒成立，因为 $\text{succ}\ n$ 不等于 $0$。
     - **定义**：$f \equiv \lambda q . n$。

- **完整定义**：

  - **基例函数**：

    $$
    f_0 \equiv \lambda z . \text{abortN}(z\ r(0))
    $$

  - **递归函数**：

    $$
    f_s \equiv \lambda n . \lambda p . \lambda q . n
    $$

  - **前驱函数**：

    $$
    \text{pred} \equiv \lambda n . \text{prim}\ n\ f_0\ f_s
    $$

- **解释**：

  - 对于 $n = 0$，前驱未定义，通过 $\text{abortN}$ 处理。
  - 对于 $n = \text{succ}\ n$，前驱为 $n$。

### **等式类型上的等式**

- **问题**：$I(A, a, a)$ 的一般元素是什么？

- **观察**：

  - 对于每个 $a : A$，$I(A, a, a)$ 至少有一个元素 $r(a)$。
  - 使用消除规则，我们可以证明所有 $I$ 类型的元素都是相等的。

- **证明**：

  - **设定**：$p : I(A, a, a)$，$r(a) : I(A, a, a)$。
  - **构造**：$J(p, r(r(a))) : I(I(A, a, a), r(a), p)$。
    - **其中**：$r(r(a)) : I(I(A, a, a), r(a), r(a))$。
  - **结论**：所有 $I$ 类型的元素都可以被证明相等。

### **练习解答**

#### **4.29. 练习**

**问题**：证明对于自然数，类型 $(\forall x : \text{N}). (x =_{\text{N}} 0 \lor (\exists y : \text{N}). x =_{\text{N}} \text{succ}\ y)$ 是可居的。为树类型 $\text{tree}$ 公式化并证明类似的结果。

**解答**：

1. **目标**：构造一个函数，证明对于任意 $x : \text{N}$，要么 $x =_{\text{N}} 0$，要么存在 $y : \text{N}$，使得 $x =_{\text{N}} \text{succ}\ y$。

2. **定义命题**：

   $$
   C(x) \equiv (x =_{\text{N}} 0) \lor (\exists y : \text{N}). x =_{\text{N}} \text{succ}\ y
   $$

3. **使用自然数的消除规则（原始递归）**：

   - **基例**（当 $x = 0$ 时）：

     - $c \equiv \text{inl}\ r(0) : C(0)$，因为 $r(0) : I(\text{N}, 0, 0)$。

   - **归纳步骤**（当 $x = \text{succ}\ n$ 时）：

     - 假设 $n : \text{N}$。
     - $y = n$，则 $r(\text{succ}\ n) : I(\text{N}, \text{succ}\ n, \text{succ}\ n)$。
     - 构造 $\text{inr}\ (n, r(\text{succ}\ n)) : C(\text{succ}\ n)$，其中 $(n, r(\text{succ}\ n)) : (\exists y : \text{N}). x =_{\text{N}} \text{succ}\ y$。

4. **构造递归函数**：

   - 定义递归函数 $f$：

     $$
     f \equiv \lambda n . \lambda p . \text{inr}\ (n, r(\text{succ}\ n))
     $$

   - 定义 $\text{prim}$ 函数：

     $$
     \text{prim}\ x\ c\ f : C(x)
     $$

5. **完整证明**：

   - 构造函数：

     $$
     \text{nat\_case} \equiv \lambda x . \text{prim}\ x\ c\ f
     $$

   - 因此，我们得到了一个类型为 $(\forall x : \text{N}). C(x)$ 的证明对象。

6. **对于树类型 $\text{tree}$ 的类似结果**：

   - **目标**：证明对于任意 $t : \text{tree}$，要么 $t =_{\text{tree}} \text{Null}$，要么存在 $n : \text{N}, u : \text{tree}, v : \text{tree}$，使得 $t =_{\text{tree}} \text{Bnode}\ n\ u\ v$。

   - **类似地**，使用树的消除规则，构造相应的证明。

#### **4.30. 练习**

**问题**：在第 4.28 题中定义的函数 $\text{insert}$ 和 $\text{delete}$ 设计用于应用于有序树。重新定义它们，使得它们仅在有序树上定义，类似于上面前驱函数的受限类型。

**解答**：

1. **目标**：将 $\text{insert}$ 和 $\text{delete}$ 的类型限定为仅适用于有序树。

2. **定义有序树的类型**：

   - 定义一个谓词 $\text{ordered}(t) : \text{bool}$，判断树 $t$ 是否有序。

3. **限定类型**：

   - 将函数的类型定义为：

     $$
     \text{insert} : (\forall t : \text{tree}). (\text{ordered}(t) = \text{True} \Rightarrow \text{tree})
     $$

     - **解释**：对于有序的树 $t$，$\text{insert} \ t$ 定义良好。

4. **处理未定义的情况**：

   - 当 $\text{ordered}(t) = \text{False}$ 时，$\text{ordered}(t) = \text{True} \Rightarrow \text{tree}$ 无法成立，因此函数未定义。

5. **与前驱函数的类似性**：

   - 类似于前驱函数，我们使用依赖类型和等式来限制函数的定义域。

---

**希望以上详细的解释和推导能够帮助您理解第 4 章第 4.10 节的内容。如果您还有任何疑问或需要进一步的澄清，请随时告诉我！**

### ----------------------------

# **第 4 章：类型理论简介**

---

## **4.11 可转换性**

### **概述**

在本节中，我们将探讨系统的计算规则，以及由这些规则生成的等价关系或可转换性（convertibility）。我们的系统规则有两个截然不同的形式和目的：

1. **构造规则、引入规则和消除规则**：描述如何构建判断的推导。如果我们只对哪些命题有证明（即哪些类型是可居的）感兴趣，那么这些规则似乎已经足够。这在我们不考虑等式规则时是成立的。但在完整的系统中，等式命题的可推导性与通过可转换性关系和替换规则的可计算性密切相关。

2. **计算规则**：当我们将规则视为编程语言的规则时，前三种规则（构造、引入、消除）仅表达了语言的语法，指定了哪些表达式具有哪些类型。在编程中，执行或求值的过程是核心，这正是计算规则所表达的。

我们可能会问，在逻辑中，什么对应于求值的过程？那就是**证明对象的简化**。例如，假设我们有如下部分证明 $A$ 的过程：给定 $a : A$ 和 $b : B$ 的证明，构建 $A \wedge B$ 的证明 $(a, b)$，然后通过取第一分量来构造 $A$ 的证明。这就是证明对象 $\text{fst}(a, b)$，这个证明对象可以简化为 $a$。

### **4.11.1 定义：可转换性和等价性**

首先，我们给出一些推广自简单类型化 λ-演算的定义。

#### **自由子表达式**

**定义 4.7**：如果表达式 $e$ 中的一个子表达式 $f$ 的自由变量在 $e$ 中未被绑定，那么我们称 $f$ 是 $e$ 的一个**自由子表达式（free sub-expression）**。

- **解释**：
  - 自由子表达式是指在 $e$ 中，可以通过替换而得到的子表达式。
  - 如果 $f$ 中的自由变量在 $e$ 中被绑定，那么替换 $f$ 将破坏 $e$ 的结构。

#### **归约（Reduction）**

**定义 4.8**：我们的系统中已经针对每个构造引入了计算规则 $\to$。如果表达式的一个自由子表达式匹配这些规则的左侧（称为**红ex**，即 reducible expression），我们就可以应用计算规则。

- **红ex（Redex）**：一个表达式的自由子表达式，如果它匹配计算规则的左侧，我们称它为一个红ex。

- **归约规则**：
  - 如果 $e_1$ 的一个自由子表达式 $f_1$ 是一个红ex，且应用计算规则后得到 $f_2$，那么通过将 $f_1$ 替换为 $f_2$，我们得到 $e_2$。
  - 我们记作 $e_1 \to e_2$。

- **注意**：
  - 这种归约只能在自由子表达式上进行，不能在绑定了变量的部分进行。
  - 这与我们在第 2 章中讨论的原因类似，可以使归约关系更容易分析。

#### **归约关系的闭包**

**定义 4.9**：我们定义归约关系 $\to$ 的自反传递闭包为 $\to\!\!\to$。即，如果存在一系列的表达式 $e_0, e_1, \dots, e_n$，使得：

$$
e = e_0 \to e_1 \to \dots \to e_n = f
$$

那么我们记作 $e \to\!\!\to f$。

- **解释**：
  - $e$ 可以通过零次或多次归约到 $f$。

#### **可转换性（Convertibility）**

**定义 4.10**：**可转换性关系** $\leftrightarrow\!\!\leftrightarrow$ 是 $\to\!\!\to$ 的最小等价闭包。也就是说，$e \leftrightarrow\!\!\leftrightarrow f$ 当且仅当存在一系列表达式 $e_0, e_1, \dots, e_n$，使得：

- $e = e_0$，$f = e_n$；
- 对于每个 $i$，要么 $e_i \to\!\!\to e_{i+1}$，要么 $e_{i+1} \to\!\!\to e_i$。

- **解释**：
  - 可转换性关系允许我们通过归约和其逆关系（扩展）来连接两个表达式。
  - 这是一个等价关系（自反、对称、传递）。

#### **扩展到类型表达式**

由于类型表达式（或公式）可以包含嵌入的对象（或证明）表达式，我们以显然的方式将可转换性关系扩展到类型表达式。

### **替换规则（Substitution Rules）**

当我们考虑简单类型化 λ-演算时，我们可以将 $\leftrightarrow\!\!\leftrightarrow$ 视为一种等价关系。计算规则根据我们的直觉，将具有相同意义的项替换为其他项。因此，两个可互换的项应该在系统的判断中可以互换。

我们通过以下**替换规则**来表达这一点，这些规则允许对可转换的对象和类型表达式进行替换。

#### **规则 S1**

$$
\frac{a \leftrightarrow\!\!\leftrightarrow b \quad B(a)\ \text{是一个类型}}{B(b)\ \text{是一个类型}}\ (\text{S1})
$$

- **解释**：
  - 如果 $a$ 和 $b$ 可转换，并且 $B(a)$ 是一个类型，那么 $B(b)$ 也是一个类型。
  - $B(a)$ 是一个依赖于 $a$ 的类型。

#### **规则 S2**

$$
\frac{a \leftrightarrow\!\!\leftrightarrow b \quad p(a) : B(a)}{p(b) : B(b)}\ (\text{S2})
$$

- **解释**：
  - 如果 $a \leftrightarrow\!\!\leftrightarrow b$，并且 $p(a) : B(a)$，那么 $p(b) : B(b)$。
  - 这里，$p(a)$ 是依赖于 $a$ 的证明对象。

#### **规则 S3**

$$
\frac{A \leftrightarrow\!\!\leftrightarrow B \quad A\ \text{是一个类型}}{B\ \text{是一个类型}}\ (\text{S3})
$$

- **解释**：
  - 如果 $A$ 和 $B$ 可转换，并且 $A$ 是一个类型，那么 $B$ 也是一个类型。

#### **规则 S4**

$$
\frac{A \leftrightarrow\!\!\leftrightarrow B \quad p : A}{p : B}\ (\text{S4})
$$

- **解释**：
  - 如果 $A \leftrightarrow\!\!\leftrightarrow B$，并且 $p : A$，那么 $p : B$。

#### **替换自由变量的规则**

还有两个对应的规则，允许对自由变量进行替换。它们可以从上述规则推导出来。

#### **规则 S5**

$$
\frac{a : A \quad [x : A] \quad \vdots \quad B\ \text{是一个类型}}{B[a / x]\ \text{是一个类型}}\ (\text{S5})
$$

- **解释**：
  - 如果我们有一个 $a : A$，并且在假设 $x : A$ 下 $B$ 是一个类型，那么替换 $x$ 为 $a$ 后，$B[a / x]$ 也是一个类型。

#### **规则 S6**

$$
\frac{a : A \quad [x : A] \quad \vdots \quad b : B}{b[a / x] : B[a / x]}\ (\text{S6})
$$

- **解释**：
  - 如果在假设 $x : A$ 下，$b : B$，那么替换 $x$ 为 $a$ 后，$b[a / x] : B[a / x]$。

#### **替换中的注意事项**

- 当我们在推导中对自由变量 $x$ 进行替换时，除了对 $x$ 本身的假设外，我们还应该在所有出现 $x$ 的地方替换 $a$。
- 这确保了推导的一致性。

### **加强的等式引入规则**

基于替换规则，我们可以给出一个加强的等式引入规则。

**加强的等式引入规则（Introduction Rule for I）**：

$$
\frac{a \leftrightarrow\!\!\leftrightarrow b \quad a : A \quad b : A}{r(a) : I(A, a, b)}\ (\text{I I}')
$$

- **解释**：
  - 如果 $a$ 和 $b$ 可转换，并且都是 $A$ 的元素，那么 $r(a) : I(A, a, b)$。
  - 这意味着可转换的表达式不仅在系统外部被视为等价，而且在系统内部也可以被证明相等（通过 $r(a)$ 作为证明对象）。

- **影响**：
  - 通过这个加强的规则，我们可以在系统内部推理表达式的计算行为。

### **示例：加一函数**

在本小节中，我们将证明两种不同的加一函数在所有自然数上具有相同的结果。我们的目标是证明对于每个 $x : \text{N}$，都有：

$$
\text{addone}\ x =_{\text{N}} \text{succ}\ x
$$

#### **定义加一函数**

回顾我们在第 102 页定义的 $\text{addone}$ 函数：

$$
\text{addone} \equiv \lambda x . \text{prim}\ x\ 1\ \text{succ0}
$$

其中：

- $1 \equiv \text{succ}\ 0$。
- $\text{succ0} \equiv \lambda y . \lambda z . \text{succ}\ z$。

#### **证明过程**

1. **基例（$x = 0$）**：

   - 计算 $\text{addone}\ 0$：

     $$
     \text{addone}\ 0 = (\lambda x . \text{prim}\ x\ 1\ \text{succ0})\ 0 \to \text{prim}\ 0\ 1\ \text{succ0} \to 1 = \text{succ}\ 0
     $$

   - 因此，$\text{addone}\ 0 \leftrightarrow\!\!\leftrightarrow \text{succ}\ 0$，根据加强的等式引入规则（I I'），我们有：

     $$
     r(\text{addone}\ 0) : I(\text{N}, \text{addone}\ 0, \text{succ}\ 0)
     $$

   - 即，$\text{addone}\ 0 =_{\text{N}} \text{succ}\ 0$ 是可居的。

2. **归纳步骤（假设 $\text{addone}\ x =_{\text{N}} \text{succ}\ x$）**：

   - 假设：

     $$
     z : \text{addone}\ x =_{\text{N}} \text{succ}\ x
     $$

   - 需要证明：

     $$
     \text{addone}\ (\text{succ}\ x) =_{\text{N}} \text{succ}\ (\text{succ}\ x)
     $$

   - **计算 $\text{addone}\ (\text{succ}\ x)$**：

     $$
     \begin{align*}
     \text{addone}\ (\text{succ}\ x) &= (\lambda x . \text{prim}\ x\ 1\ \text{succ0})\ (\text{succ}\ x) \\
     &\to \text{prim}\ (\text{succ}\ x)\ 1\ \text{succ0} \\
     &\to \text{succ0}\ x\ (\text{prim}\ x\ 1\ \text{succ0}) \\
     &\to \text{succ}\ (\text{prim}\ x\ 1\ \text{succ0})
     \end{align*}
     $$

   - 根据加强的等式引入规则（I I'），由于 $\text{succ}\ (\text{prim}\ x\ 1\ \text{succ0}) \leftrightarrow\!\!\leftrightarrow \text{succ}\ (\text{addone}\ x)$，因此：

     $$
     r(\text{addone}\ (\text{succ}\ x)) : I(\text{N}, \text{addone}\ (\text{succ}\ x), \text{succ}\ (\text{addone}\ x))
     $$

   - **目标**：

     - 需要证明：

       $$
       \text{succ}\ (\text{addone}\ x) =_{\text{N}} \text{succ}\ (\text{succ}\ x)
       $$

     - 由于 $z : \text{addone}\ x =_{\text{N}} \text{succ}\ x$，我们可以通过等式的传递性来完成证明。

   - **应用等式消除规则（I E）**：

     - 设定：

       - 定义命题 $C(a, b, c) \equiv I(\text{N}, \text{succ}\ a, \text{succ}\ b)$。

     - 有：

       - $C(\text{addone}\ x, \text{succ}\ x, z) \equiv I(\text{N}, \text{succ}\ (\text{addone}\ x), \text{succ}\ (\text{succ}\ x))$。

     - 应用等式消除规则：

       $$
       J(z, r(\text{succ}\ (\text{addone}\ x))) : I(\text{N}, \text{succ}\ (\text{addone}\ x), \text{succ}\ (\text{succ}\ x))
       $$

   - **结论**：

     - 结合之前的结果，我们得到：

       $$
       \text{addone}\ (\text{succ}\ x) =_{\text{N}} \text{succ}\ (\text{succ}\ x)
       $$

3. **归纳结论**：

   - 通过数学归纳法，我们证明了对于所有 $x : \text{N}$，都有：

     $$
     \text{addone}\ x =_{\text{N}} \text{succ}\ x
     $$

#### **关于函数等式的讨论**

- **问题**：我们能否证明两个函数本身相等，即：

  $$
  \lambda x . \text{succ}\ x =_{\text{N} \Rightarrow \text{N}} \lambda x . \text{addone}\ x
  $$

- **答案**：在当前的系统中，无法证明这一点，因为两个 λ-项显然不可转换。

- **原因**：

  - 虽然它们在每个自然数上的取值相同，但它们的定义不同，无法通过可转换性关系连接。

- **结论**：

  - 我们可以证明它们对于所有输入值的应用是相等的：

    $$
    (\forall x : \text{N}). (\text{succ}\ x =_{\text{N}} \text{addone}\ x)
    $$

  - 但是，无法在系统中证明它们作为函数相等。

- **进一步讨论**：

  - 在第 5.8 节中，我们将进一步讨论如何处理这种情况。

### **示例：自然数的等式**

#### **证明 $0$ 不等于任何自然数的后继**

- **定义函数 $\text{discrim}$**：

  - 我们定义一个函数 $\text{discrim} : \text{N} \Rightarrow \text{bool}$，其在 $0$ 上为 $\text{True}$，在其他自然数上为 $\text{False}$。

  - **定义**：

    $$
    \text{discrim} \equiv \lambda n . \text{prim}\ n\ \text{True}\ f
    $$

    其中 $f \equiv \lambda n . \lambda b . \text{False}$。

- **计算**：

  - **当 $n = 0$ 时**：

    $$
    \begin{align*}
    \text{discrim}\ 0 &= (\lambda n . \text{prim}\ n\ \text{True}\ f)\ 0 \\
    &\to \text{prim}\ 0\ \text{True}\ f \\
    &\to \text{True}
    \end{align*}
    $$

  - **当 $n = \text{succ}\ n$ 时**：

    $$
    \begin{align*}
    \text{discrim}\ (\text{succ}\ n) &= (\lambda n . \text{prim}\ n\ \text{True}\ f)\ (\text{succ}\ n) \\
    &\to \text{prim}\ (\text{succ}\ n)\ \text{True}\ f \\
    &\to f\ n\ (\text{prim}\ n\ \text{True}\ f) \\
    &\to (\lambda n . \lambda b . \text{False})\ n\ (\text{prim}\ n\ \text{True}\ f) \\
    &\to \text{False}
    \end{align*}
    $$

- **结论**：

  - $\text{discrim}\ 0 = \text{True}$，$\text{discrim}\ (\text{succ}\ n) = \text{False}$。

- **证明 $0 \neq \text{succ}\ n$**：

  - 假设 $0 =_{\text{N}} \text{succ}\ n$（即有 $p : I(\text{N}, 0, \text{succ}\ n)$）。

  - 由于等式的替换性，我们有：

    $$
    \text{discrim}\ 0 =_{\text{bool}} \text{discrim}\ (\text{succ}\ n)
    $$

  - 但我们已经计算出左边为 $\text{True}$，右边为 $\text{False}$，因此：

    $$
    \text{True} =_{\text{bool}} \text{False}
    $$

  - 这与我们之前的公理 $\text{ax} : \lnot (\text{True} =_{\text{bool}} \text{False})$ 矛盾，因此 $0 \neq \text{succ}\ n$。

#### **证明后继函数的单射性**

- **目标**：证明 $(\text{succ}\ n =_{\text{N}} \text{succ}\ m) \Rightarrow (n =_{\text{N}} m)$。

- **使用前驱函数 $\text{pred}$**：

  - 回顾我们在第 4.10.3 节定义的 $\text{pred}$ 函数。

  - 对于 $\text{succ}\ n$，有：

    $$
    \text{pred}\ (\text{succ}\ n) : (\text{succ}\ n \neq 0) \Rightarrow \text{N}
    $$

  - 因为 $\text{succ}\ n \neq 0$ 恒成立，我们可以计算：

    $$
    \text{pred}\ (\text{succ}\ n)\ \text{tsn} = n
    $$

    其中 $\text{tsn}$ 是 $(\text{succ}\ n \neq 0)$ 的证明。

- **证明过程**：

  - 如果 $\text{succ}\ n =_{\text{N}} \text{succ}\ m$，那么通过等式的替换性，我们有：

    $$
    n =_{\text{N}} m
    $$

- **结论**：

  - 后继函数是单射的，即不同的自然数有不同的后继。

#### **总结**

- 我们证明了自然数的一些基本性质：

  - $0$ 不是任何自然数的后继。

  - 后继函数是单射的。

- 这些证明使用了原始递归和等式的性质。

### **小结**

通过对可转换性的讨论，我们完成了对类型理论核心系统的介绍，并给出了一些小示例。在下一章中，我们将从系统中退一步，考察规则的替代表述，研究系统的一些性质，检查理论中的各种等价关系，等等。

### **练习**

#### **4.31. 练习**

**问题**：证明 $\lambda x . ((\lambda y . y) x) \to \lambda x . x$，但论证我们不能通过在 $\lambda x . z$ 中替换 $z$ 为 $(\lambda y . y) x$ 来生成 $\lambda x . ((\lambda y . y) x)$。

**解答**：

- **证明 $\lambda x . ((\lambda y . y) x) \to \lambda x . x$**：

  - 计算：

    $$
    \lambda x . ((\lambda y . y) x) \to \lambda x . x
    $$

  - 因为 $(\lambda y . y) x \to x$。

- **论证不能通过替换生成**：

  - **假设**：试图通过在 $\lambda x . z$ 中替换 $z$ 为 $(\lambda y . y) x$ 来得到 $\lambda x . ((\lambda y . y) x)$。

  - **问题**：在 $\lambda x . z$ 中，$z$ 是一个自由变量，但 $(\lambda y . y) x$ 中的 $x$ 是被绑定的。

  - **结论**：由于 $x$ 在 $(\lambda y . y) x$ 中是自由的，但在 $\lambda x .$ 外层绑定了 $x$，因此不能直接替换。

#### **4.32. 练习**

**问题**：证明以下替换规则可以从系统的其他规则推导出来：

- 替换规则：

  $$
  \frac{a : A \quad [x : A] \quad \vdots \quad B\ \text{是一个类型}}{B[a / x]\ \text{是一个类型}}
  $$

  $$
  \frac{a : A \quad [x : A] \quad \vdots \quad b : B}{b[a / x] : B[a / x]}
  $$

**解答**：

- **思路**：

  - 使用归纳法和替换的定义，结合系统的构造规则、引入规则、消除规则和计算规则，来证明替换规则的正确性。

- **证明**：

  - 由于替换规则涉及到在假设 $x : A$ 下的推导，我们可以通过在推导过程中逐步替换 $x$ 为 $a$，并利用系统的规则来保持推导的正确性。

#### **4.33. 练习**

**问题**：对树类型 $\text{tree}$，给出类似于上面对 $\text{N}$ 的等式的刻画。

**解答**：

- **目标**：刻画树的等式，使得对于任意 $t : \text{tree}$，要么 $t =_{\text{tree}} \text{Null}$，要么存在 $n : \text{N}, u : \text{tree}, v : \text{tree}$，使得 $t =_{\text{tree}} \text{Bnode}\ n\ u\ v$。

- **证明**：

  - 使用树的消除规则，类似于自然数的情况，通过递归构造证明。

#### **4.34. 练习**

**问题**：对第 109 页定义的 $\text{insert}$ 和 $\text{delete}$ 函数，公式化并证明它们保持树参数的有序性。

**解答**：

- **思路**：

  - 定义性质 $\text{ordered}(t)$，表示树 $t$ 是有序的。

  - 证明对于任意有序树 $t$ 和自然数 $n$，$\text{insert}\ n\ t$ 和 $\text{delete}\ n\ t$ 仍然是有序的。

- **证明**：

  - 使用结构归纳法，对树的结构进行递归证明。

#### **4.35. 练习**

**问题**：公式化并证明任意自然数的阶乘大于零。

**解答**：

- **目标**：证明对于任意 $n : \text{N}$，有 $\text{fact}\ n > 0$。

- **证明**：

  - 使用数学归纳法：

    - **基例**：$n = 0$，$\text{fact}\ 0 = 1 > 0$。

    - **归纳步骤**：假设 $\text{fact}\ n > 0$，证明 $\text{fact}\ (\text{succ}\ n) = (\text{succ}\ n) \times \text{fact}\ n > 0$。

  - 因为 $\text{succ}\ n > 0$，且 $\text{fact}\ n > 0$，所以乘积大于零。

---

**这结束了我们对类型理论的介绍。我们将此处介绍的系统称为 $\text{TT}_0$。在下一章中，我们将在澄清一些更技术性的内容后，探索该系统的一些性质。事实上，系统 $\text{TT}_0$ 在下面的第 5.3 节中被完整定义，我们将在那里给出析取（$\lor$）和存在量词（$\exists$）的消除规则的泛化。**

### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------