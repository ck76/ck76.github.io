[toc]





章节Number对不上版本

# 第14章 泛型编程 (Generic Programming)

## **14.1 引言 (Introduction)**

### **概述**

在编程中，许多程序可以看作是将某种**通用模式 (General Pattern)** 应用于特定情况的实例。这种模式通常由所涉及的数据的类型决定。例如，在第9章中，我们通过自然数的递归计算模式来定义自然数类型。这一概念本身将作为**类型泛型编程 (Type-Generic Programming)** 或简称**泛型编程 (Generic Programming)** 的一个实例出现。

### **问题背景**

假设我们有一个函数 $f$，其类型为 $\rho \rightarrow \rho'$，即将类型为 $\rho$ 的值转换为类型为 $\rho'$ 的值。例如，$f$ 可能是对自然数的**倍增函数 (Doubling Function)**。

现在，我们希望将 $f$ 扩展为从类型 $[\rho / t]\tau$ 到类型 $[\rho' / t]\tau$ 的转换，即在输入中出现类型为 $\rho$ 的位置应用 $f$，以获得类型为 $\rho'$ 的值，同时保持数据结构的其余部分不变。

#### **示例**

- **类型 $\tau$**：假设 $\tau$ 是一个类型表达式，例如 $\text{bool} \times t$。

- **替换**：$[\rho / t]\tau$ 表示在类型表达式 $\tau$ 中，用类型 $\rho$ 替换类型变量 $t$。

- **扩展函数 $f$**：我们希望将 $f$ 扩展为一个类型为 $[\rho / t]\tau \rightarrow [\rho' / t]\tau$ 的函数。

- **具体例子**：如果 $\tau = \text{bool} \times t$，那么 $[\rho / t]\tau = \text{bool} \times \rho$，$[\rho' / t]\tau = \text{bool} \times \rho'$。扩展后的函数将类型为 $\text{bool} \times \rho \rightarrow \text{bool} \times \rho'$，并将对偶 $(a, b)$ 映射为 $(a, f(b))$。

#### **问题**

这个例子忽略了一个重要的**歧义问题 (Problem of Ambiguity)**。

- **歧义的来源**：给定函数 $f : \rho \rightarrow \rho'$，一般来说，如何将其扩展为从 $[\rho / t]\tau$ 到 $[\rho' / t]\tau$ 的函数并不明显。

- **多个 $\rho$ 的出现**：问题在于，即使 $\tau$ 中只有一个 $\rho$ 的出现，也不清楚 $\rho$ 在 $[\rho / t]\tau$ 的哪些位置应该被 $f$ 转换，哪些应该保持不变。

#### **解决方案**

为了避免歧义，我们需要一种方法来标记 $\tau$ 中 $\rho$ 出现的位置，指明哪些位置应该被 $f$ 转换，哪些应该保持固定。

- **类型算子 (Type Operator)**：这可以通过**隔离类型算子** $t.\tau$ 来实现，$t.\tau$ 是一个类型表达式，其中指定的变量 $t$ 标记了我们希望进行转换的位置。

- **扩展函数 $f$**：给定 $t.\tau$ 和 $f : \rho \rightarrow \rho'$，我们可以将 $f$ 明确地扩展为一个类型为 $[\rho / t]\tau \rightarrow [\rho' / t]\tau$ 的函数。

- **示例**：如果 $t.\tau = \text{unit} + (\text{bool} \times t)$，并且 $f : \rho \rightarrow \rho'$，那么扩展后的函数类型为 $[\rho / t]\tau \rightarrow [\rho' / t]\tau$。

### **泛型编程**

- **定义**：使用类型算子来确定代码行为的技术称为**泛型编程 (Generic Programming)**。

- **泛型编程的力量**：泛型编程的力量取决于所考虑的类型算子的形式。

  - **多项式类型算子 (Polynomial Type Operators)**：最简单的情况，由类型变量 $t$、类型 $\text{void}$ 和 $\text{unit}$、以及积类型和和类型构成的类型算子。

  - **正类型算子 (Positive Type Operators)**：可以扩展到允许受限形式的函数类型的算子。

---

## **14.2 类型算子 (Type Operators)**

### **类型算子的定义**

- **类型算子 (Type Operator)**：类型算子是一个类型，配有一个指定的变量，其出现标记了类型中希望应用转换的位置。

- **表示方式**：类型算子表示为一个抽象器 (Abstraction) $t.\tau$，其中 $t$ 是类型变量，$\tau$ 是类型表达式，并且 $t$ 可以在 $\tau$ 中出现。

  $$
  t\ \text{type} \vdash \tau\ \text{type}
  $$

  **解释**：

  - $t\ \text{type}$：$t$ 是一个类型变量。

  - $\vdash$：表示在类型环境中推导。

  - $\tau\ \text{type}$：$\tau$ 是一个类型表达式。

- **示例**：类型算子 $t.\ \text{unit} + (\text{bool} \times t)$。

  - **$\tau$ 的结构**：$\tau = \text{unit} + (\text{bool} \times t)$。

  - **$t$ 的出现**：类型变量 $t$ 在 $\tau$ 中出现，标记了希望进行转换的位置。

### **类型算子的实例**

- **实例化**：类型算子 $t.\tau$ 的一个实例是通过在类型 $\tau$ 中用类型 $\rho$ 替换变量 $t$ 获得的，记为 $[\rho / t]\tau$。

  $$
  [\rho / t]\tau
  $$

  **解释**：

  - **替换符号**：$[\rho / t]\tau$ 表示在 $\tau$ 中用 $\rho$ 替换所有的 $t$。

- **表示法**：有时，我们用 $\text{Map}[t.\tau](\rho)$ 来表示替换实例 $[\rho / t]\tau$。

  $$
  \text{Map}[t.\tau](\rho) = [\rho / t]\tau
  $$

### **多项式类型算子 (Polynomial Type Operators)**

- **定义**：多项式类型算子是由类型变量 $t$、类型 $\text{void}$ 和 $\text{unit}$、以及积类型和和类型构造的类型算子。

- **构造方法**：

  - **基本类型**：$t$、$\text{void}$、$\text{unit}$。

  - **类型构造器**：

    - **积类型**：$\tau_1 \times \tau_2$。

    - **和类型**：$\tau_1 + \tau_2$。

- **多项式类型算子的判断**：可以通过归纳定义判断 $t.\tau\ \text{poly}$，表示算子 $t.\tau$ 是一个多项式类型算子。

---

### **详细解析示例**

#### **示例1：类型算子 $t.\ \text{unit} + (\text{bool} \times t)$**

- **结构分析**：

  - **类型算子**：$t.\ \text{unit} + (\text{bool} \times t)$。

  - **含义**：在类型表达式 $\tau = \text{unit} + (\text{bool} \times t)$ 中，类型变量 $t$ 标记了我们希望进行转换的位置。

- **实例化**：

  - **给定类型**：$\rho$。

  - **替换**：$[\rho / t]\tau = \text{unit} + (\text{bool} \times \rho)$。

  - **扩展函数 $f$**：

    - 给定 $f : \rho \rightarrow \rho'$。

    - 我们可以构造一个函数 $F$，类型为 $[\rho / t]\tau \rightarrow [\rho' / t]\tau$。

- **函数 $F$ 的行为**：

  - **输入类型**：$[\rho / t]\tau = \text{unit} + (\text{bool} \times \rho)$。

  - **输出类型**：$[\rho' / t]\tau = \text{unit} + (\text{bool} \times \rho')$。

  - **操作**：

    - 对输入的值进行模式匹配。

    - 如果输入是 $\text{unit}$（左注入），则输出仍然是 $\text{unit}$。

    - 如果输入是 $(\text{bool},\ \rho)$（右注入的积类型），则将 $\rho$ 部分应用 $f$，得到 $\rho'$，然后构造 $(\text{bool},\ \rho')$。

#### **示例2：扩展倍增函数**

- **倍增函数**：$f : \mathbb{N} \rightarrow \mathbb{N}$，定义为 $f(n) = 2n$。

- **类型算子**：$t.\ \text{bool} \times t$。

- **实例化**：

  - **替换**：$[\mathbb{N} / t]\tau = \text{bool} \times \mathbb{N}$。

  - **扩展后的函数**：类型为 $\text{bool} \times \mathbb{N} \rightarrow \text{bool} \times \mathbb{N}$。

- **函数行为**：

  - 输入对 $(a, n)$，其中 $a : \text{bool}$，$n : \mathbb{N}$。

  - 输出对 $(a, f(n))$，即 $(a, 2n)$。

### **泛型编程的意义**

- **解决歧义**：通过类型算子，我们明确了在类型表达式中哪些位置需要应用转换函数，避免了歧义。

- **通用性**：泛型编程允许我们对任意类型结构进行抽象处理，提高了代码的复用性和灵活性。

- **多项式类型算子的重要性**：多项式类型算子限制了我们只能使用积类型和和类型，这使得泛型编程更加可预测和可管理。

---

## **进一步的讨论**

### **多项式类型算子的归纳定义**

为了完整地定义多项式类型算子，我们需要给出其归纳定义，即：

1. **基本情况**：

   - **类型变量**：$t.\ t$ 是多项式类型算子。

   - **常量类型**：$t.\ \text{unit}$ 和 $t.\ \text{void}$ 是多项式类型算子。

2. **归纳步骤**：

   - 如果 $t.\ \tau_1$ 和 $t.\ \tau_2$ 是多项式类型算子，那么：

     - **积类型**：$t.\ \tau_1 \times \tau_2$ 是多项式类型算子。

     - **和类型**：$t.\ \tau_1 + \tau_2$ 是多项式类型算子。

### **正类型算子**

- **定义**：正类型算子允许在多项式类型算子的基础上，加入受限形式的函数类型。

- **扩展能力**：这使得泛型编程可以处理更多类型的结构，但同时也增加了类型系统的复杂性。

---

## **总结**

- **类型算子是泛型编程的核心工具**，通过在类型表达式中标记需要转换的位置，使得我们能够定义通用的、类型安全的转换。

- **避免歧义**：通过明确地指定类型算子和替换，我们可以消除在扩展函数时的歧义，确保转换的准确性。

- **多项式类型算子**：限制在积类型和和类型上的类型算子，使得泛型编程既有足够的表达能力，又保持了类型系统的简单性。

- **实例化类型算子**：通过用具体类型替换类型变量，我们可以获得类型算子的具体实例，用于定义扩展后的函数。

- **泛型编程的力量**：通过泛型编程，我们可以编写高度抽象和可复用的代码，适用于各种类型的结构。

---

## **练习与思考**

**练习1**：给定类型算子 $t.\ \text{list}(t)$，其中 $\text{list}(t)$ 定义为递归类型 $\text{unit} + (\tau \times t)$，试着定义一个泛型函数，将列表中的每个元素应用函数 $f : \rho \rightarrow \rho'$。

**练习2**：证明多项式类型算子的归纳定义，即给出 $t.\tau\ \text{poly}$ 的归纳定义，并举例说明。

**思考**：

- **泛型编程的限制**：在什么情况下，多项式类型算子无法满足需求，需要引入更复杂的类型算子？

- **类型系统的设计**：如何设计类型系统，使得泛型编程既具有强大的表达能力，又保持类型检查的可判定性？

---

希望以上详细的解释和公式解析能够帮助您深入理解**第14章 泛型编程 (Generic Programming)** 中的内容。如有任何疑问，欢迎提问！

### ---------------------------------

# 第14章 泛型编程 (Generic Programming)

在本章中，我们将深入探讨**泛型编程**的概念，特别是**泛型扩展 (Generic Extension)** 的机制。泛型编程允许我们将某些函数或操作泛化，适用于不同类型的数据结构，从而提高代码的复用性和灵活性。

---

## **14.3 泛型扩展 (Generic Extension)**

### **泛型扩展的形式**

泛型扩展的基本形式为：

$$
\text{map}[t.\tau](x.e_0;\ e)
$$

**符号解释：**

- **$\text{map}[t.\tau]$**：表示针对类型算子 $t.\tau$ 的泛型扩展操作。
- **$x.e_0$**：抽象器 (abstraction)，表示一个从类型 $\rho$ 到类型 $\rho'$ 的转换函数，其中 $x$ 是类型为 $\rho$ 的变量，$e_0$ 是类型为 $\rho'$ 的表达式。
- **$e$**：类型为 $[\rho / t]\tau$ 的表达式，即在 $\tau$ 中用 $\rho$ 替换类型变量 $t$。

### **静态语义 (Statics)**

泛型扩展的类型规则如下：

$$
\frac{
  t\ \text{type} \vdash \tau\ \text{type} \quad \Gamma,\ x : \rho \vdash e_0 : \rho' \quad \Gamma \vdash e : [\rho / t]\tau
}{
  \Gamma \vdash \text{map}[t.\tau](x.e_0;\ e) : [\rho' / t]\tau
}
\quad (14.1)
$$

**符号解释：**

- **$t\ \text{type} \vdash \tau\ \text{type}$**：在类型环境中，$t$ 是一个类型变量，$\tau$ 是一个类型表达式，可能包含 $t$。
- **$\Gamma$**：类型环境，包含变量及其类型信息。
- **$x : \rho$**：变量 $x$ 的类型为 $\rho$。
- **$e_0 : \rho'$**：表达式 $e_0$ 的类型为 $\rho'$。
- **$e : [\rho / t]\tau$**：表达式 $e$ 的类型为在 $\tau$ 中用 $\rho$ 替换 $t$ 后得到的类型 $[\rho / t]\tau$。
- **$\Gamma \vdash \text{map}[t.\tau](x.e_0;\ e) : [\rho' / t]\tau$**：泛型扩展表达式的类型为 $[\rho' / t]\tau$，即在 $\tau$ 中用 $\rho'$ 替换 $t$。

**解释：**

- **泛型扩展的作用**：将一个函数 $f : \rho \rightarrow \rho'$ 扩展到数据结构类型 $[\rho / t]\tau$，得到新的数据结构类型 $[\rho' / t]\tau$。
- **$x.e_0$**：指定了从类型 $\rho$ 到 $\rho'$ 的转换，即函数 $f$。
- **$e$**：是要被转换的值，其类型为 $[\rho / t]\tau$。
- **$\tau$ 中 $t$ 的出现**：决定了在 $e$ 中哪些位置需要应用转换 $f$。

### **动态语义 (Dynamics)**

泛型扩展的计算规则如下（这里只考虑多项式类型算子）：

#### **规则 (14.2a)：直接应用转换**

$$
\text{map}[t.t](x.e_0;\ e) \rightarrow [e / x]e_0
\quad (14.2a)
$$

**解释：**

- **情况**：当类型算子是 $t.t$ 时，表示直接对 $e$ 应用转换函数。
- **$[e / x]e_0$**：将 $e$ 替换 $e_0$ 中的 $x$，即对 $e$ 应用转换 $e_0$。

#### **规则 (14.2b)：单位类型的转换**

$$
\text{map}[t.\text{unit}](x.e_0;\ e) \rightarrow \langle\ \rangle
\quad (14.2b)
$$

**解释：**

- **情况**：当类型算子是 $t.\text{unit}$ 时，表示类型为 $\text{unit}$，只有一个值 $\langle\ \rangle$。
- **结果**：$\langle\ \rangle$，即单位元本身，不需要转换。

#### **规则 (14.2c)：积类型的转换**

$$
\text{map}[t.\tau_1 \times \tau_2](x.e_0;\ e) \rightarrow \langle \text{map}[t.\tau_1](x.e_0;\ e \cdot l),\ \text{map}[t.\tau_2](x.e_0;\ e \cdot r) \rangle
\quad (14.2c)
$$

**符号解释：**

- **$e \cdot l$**：对的左投影，提取 $e$ 的左组件。
- **$e \cdot r$**：对的右投影，提取 $e$ 的右组件。

**解释：**

- **情况**：当类型算子是 $t.\tau_1 \times \tau_2$ 时，$t$ 可能在 $\tau_1$ 和 $\tau_2$ 中出现。
- **操作**：
  - 对 $e$ 的左组件 $e \cdot l$，递归地应用泛型扩展 $\text{map}[t.\tau_1](x.e_0;\ e \cdot l)$。
  - 对 $e$ 的右组件 $e \cdot r$，递归地应用泛型扩展 $\text{map}[t.\tau_2](x.e_0;\ e \cdot r)$。
- **结果**：将转换后的左右组件重新组合成一个对。

#### **规则 (14.2d)：空类型的转换**

$$
\text{map}[t.\text{void}](x.e_0;\ e) \rightarrow \text{abort}(e)
\quad (14.2d)
$$

**解释：**

- **情况**：当类型算子是 $t.\text{void}$ 时，由于类型 $\text{void}$ 没有值，因此无法转换。
- **操作**：调用 $\text{abort}(e)$，表示程序运行到不可能的状态，终止执行。

#### **规则 (14.2e)：和类型的转换**

$$
\text{map}[t.\tau_1 + \tau_2](x.e_0;\ e) \rightarrow \text{case } e\ \{ l \cdot x_1 \Rightarrow l \cdot \text{map}[t.\tau_1](x.e_0;\ x_1) \ |\ r \cdot x_2 \Rightarrow r \cdot \text{map}[t.\tau_2](x.e_0;\ x_2) \}
\quad (14.2e)
$$

**解释：**

- **情况**：当类型算子是 $t.\tau_1 + \tau_2$ 时，$t$ 可能在 $\tau_1$ 和 $\tau_2$ 中出现。
- **操作**：
  - 对 $e$ 进行案例分析：
    - 如果 $e$ 是左注入 $l \cdot x_1$，则对 $x_1$ 递归地应用泛型扩展 $\text{map}[t.\tau_1](x.e_0;\ x_1)$，然后再左注入。
    - 如果 $e$ 是右注入 $r \cdot x_2$，则对 $x_2$ 递归地应用泛型扩展 $\text{map}[t.\tau_2](x.e_0;\ x_2)$，然后再右注入。
- **结果**：根据原始的注入方向，构造新的和类型值。

### **规则详解**

#### **规则 (14.2a) 的详解**

**公式**：

$$
\text{map}[t.t](x.e_0;\ e) \rightarrow [e / x]e_0
$$

**符号解释**：

- **$t.t$**：类型算子直接是 $t$，表示需要对 $e$ 本身应用转换。
- **$[e / x]e_0$**：在 $e_0$ 中用 $e$ 替换 $x$。

**作用**：

- 将转换函数 $x.e_0$ 直接应用到 $e$。

**例子**：

- 如果 $x.e_0 = x + 1$，$e = 3$，则：
  $$
  \text{map}[t.t](x.x + 1;\ 3) \rightarrow [3 / x](x + 1) = 3 + 1 = 4
  $$

#### **规则 (14.2c) 的详解**

**公式**：

$$
\text{map}[t.\tau_1 \times \tau_2](x.e_0;\ e) \rightarrow \langle \text{map}[t.\tau_1](x.e_0;\ e \cdot l),\ \text{map}[t.\tau_2](x.e_0;\ e \cdot r) \rangle
$$

**符号解释**：

- **$e$**：类型为 $[\rho / t](\tau_1 \times \tau_2)$ 的值，即一个对。
- **$e \cdot l$**：对的左组件，类型为 $[\rho / t]\tau_1$。
- **$e \cdot r$**：对的右组件，类型为 $[\rho / t]\tau_2$。

**作用**：

- 分别对左、右组件递归地应用泛型扩展。
- 组合转换后的组件，构造新的对。

**例子**：

- 假设类型算子 $t.\text{bool} \times t$，$x.e_0 = x + 1$。
- 输入值 $e = \langle \text{true},\ 3 \rangle$。
- 计算过程：
  - **左组件**：$\text{true}$，类型为 $\text{bool}$，不含 $t$，不需要转换。
  - **右组件**：$3$，类型为 $\rho$，需要转换：
    $$
    \text{map}[t.t](x.e_0;\ 3) \rightarrow [3 / x](x + 1) = 3 + 1 = 4
    $$
  - **组合结果**：$\langle \text{true},\ 4 \rangle$。

### **泛型扩展的例子**

**类型算子**：

$$
t.\ \text{unit} + (\text{bool} \times t)
$$

**转换函数**：

$$
x.e = x + 1
$$

**输入值**：

$$
e = r \cdot \langle \text{true},\ n \rangle
$$

**计算过程**：

1. **应用规则 (14.2e)**：

   $$
   \text{map}[t.\text{unit} + (\text{bool} \times t)](x.e;\ e) \rightarrow \text{case } e\ \{ l \cdot x_1 \Rightarrow l \cdot \text{map}[t.\text{unit}](x.e;\ x_1) \ |\ r \cdot x_2 \Rightarrow r \cdot \text{map}[t.\text{bool} \times t](x.e;\ x_2) \}
   $$

2. **由于 $e$ 是右注入**，匹配 $r \cdot x_2$，其中 $x_2 = \langle \text{true},\ n \rangle$。

3. **递归应用规则 (14.2c) 到 $x_2$**：

   $$
   \text{map}[t.\text{bool} \times t](x.e;\ x_2) \rightarrow \langle \text{map}[t.\text{bool}](x.e;\ x_2 \cdot l),\ \text{map}[t.t](x.e;\ x_2 \cdot r) \rangle
   $$

4. **计算左组件**（类型 $\text{bool}$，不含 $t$）：

   - $x_2 \cdot l = \text{true}$。
   - $\text{map}[t.\text{bool}](x.e;\ \text{true})$，由于 $t$ 不在 $\text{bool}$ 中，返回 $\text{true}$。

5. **计算右组件**（类型 $t$，需要转换）：

   - $x_2 \cdot r = n$。
   - $\text{map}[t.t](x.e;\ n) \rightarrow [n / x](x + 1) = n + 1$。

6. **组合结果**：

   - $\langle \text{true},\ n + 1 \rangle$。

7. **最终结果**：

   - $r \cdot \langle \text{true},\ n + 1 \rangle$。

### **定理 14.1（保持性，Preservation）**

**陈述**：

如果 $\text{map}[t.\tau](x.e_0;\ e) : \tau'$ 且 $\text{map}[t.\tau](x.e_0;\ e) \rightarrow e''$，则 $e'' : \tau'$。

**证明思路**：

- **基于类型规则 (14.1)**，我们有：
  1. $t\ \text{type} \vdash \tau\ \text{type}$。
  2. $x : \rho \vdash e_0 : \rho'$。
  3. $e : [\rho / t]\tau$。
  4. $\tau' = [\rho' / t]\tau$。

- **对计算规则 (14.2) 进行分类讨论**，以规则 (14.2c) 为例：

  - **已知**：
    - $\text{map}[t.\tau_1](x.e_0;\ e \cdot l) : [\rho' / t]\tau_1$。
    - $\text{map}[t.\tau_2](x.e_0;\ e \cdot r) : [\rho' / t]\tau_2$。

  - **组合结果的类型**：
    $$
    \langle \text{map}[t.\tau_1](x.e_0;\ e \cdot l),\ \text{map}[t.\tau_2](x.e_0;\ e \cdot r) \rangle : [\rho' / t]\tau_1 \times \tau_2
    $$

  - **结论**：计算结果的类型是预期的，因此类型保持。

### **正类型算子 (Positive Type Operators)**

#### **定义**

- **正类型算子**：允许在多项式类型算子的基础上，加入受限形式的函数类型。
- **条件**：
  1. 类型算子形如 $t.\tau_1 \rightarrow \tau_2$。
  2. 类型变量 $t$ **不在** $\tau_1$ 中出现（即 $t$ 在函数的参数类型中不出现）。
  3. $t.\tau_2$ 是正类型算子。

- **正/负出现**：

  - **正出现**：类型变量 $t$ 在函数的返回类型、积类型、和类型中出现，称为正出现。
  - **负出现**：$t$ 在函数的参数类型中出现，称为负出现。

#### **泛型扩展的规则**

对于正类型算子，泛型扩展的计算规则增加了函数类型的处理：

$$
\text{map}[t.\tau_1 \rightarrow \tau_2](x.e_0;\ e) \rightarrow \lambda (x_1 : \tau_1) .\ \text{map}[t.\tau_2](x.e_0;\ e(x_1))
\quad (14.3)
$$

**符号解释**：

- **$e$**：类型为 $\tau_1 \rightarrow [\rho / t]\tau_2$ 的函数。
- **$\lambda (x_1 : \tau_1)$**：构造一个新的函数，参数类型为 $\tau_1$（$t$ 不在 $\tau_1$ 中出现）。
- **$e(x_1)$**：对 $e$ 应用参数 $x_1$，得到类型为 $[\rho / t]\tau_2$ 的结果。
- **$\text{map}[t.\tau_2](x.e_0;\ e(x_1))$**：对结果递归地应用泛型扩展。

**解释**：

- **由于 $t$ 不在 $\tau_1$ 中出现**，所以参数类型不受泛型扩展的影响。
- **对函数的返回值进行转换**：在函数体中，对 $e(x_1)$ 的结果应用泛型扩展，处理 $t$ 的正出现。

### **为什么限制 $t$ 不在参数类型中出现**

- **问题**：如果允许 $t$ 在参数类型中出现，即允许负出现，那么在泛型扩展时会遇到困难。
- **原因**：
  - **转换方向不一致**：泛型扩展需要将类型为 $[\rho / t]\tau$ 的值转换为 $[\rho' / t]\tau$。
  - **对于参数类型**，需要从 $[\rho' / t]\tau_1$ 转换回 $[\rho / t]\tau_1$，以便作为 $e$ 的参数，但我们没有这样的逆向转换。
- **解决方案**：
  - **限制 $t$ 的出现**：仅允许 $t$ 在返回类型中出现，即只允许正出现。
  - **需要可逆转换**：如果必须处理负出现，需要提供从 $\rho'$ 到 $\rho$ 的逆向转换，但一般情况下无法保证。

---

## **14.4 注解 (Notes)**

### **泛型扩展与函子 (Functors)**

- **函子 (Functor)**：在范畴论 (Category Theory) 中，函子是范畴之间的映射，保持结构。
- **泛型扩展**：泛型扩展是函子的一个实例，它将类型算子映射到类型之间的转换，同时保持类型结构。

### **函子化编程 (Functorial Programming)**

- **概念**：泛型编程本质上是函子化编程，利用多项式类型算子的函子行为。
- **参考**：Hinze and Jeuring (2003) 的研究深入探讨了多项式类型算子的函子性质，以及在编程语言中的应用。

---

## **总结**

- **泛型扩展的作用**：提供了一种机制，将针对某种类型的转换函数扩展到更复杂的数据结构中，自动应用转换。
- **类型算子的重要性**：通过在类型算子中标记类型变量 $t$ 的位置，明确了转换应用的位置，避免了歧义。
- **静态与动态语义**：静态语义确保了泛型扩展表达式的类型正确性，动态语义定义了泛型扩展的具体计算规则。
- **正类型算子与限制**：为了保证泛型扩展的正确性，需要限制类型变量 $t$ 的出现位置，仅允许正出现，避免在函数参数类型中的负出现。

---

## **练习与思考**

**练习1**：

- **定义一个类型算子** $t.\ \text{list}(t)$，表示元素类型为 $t$ 的列表。
- **实现泛型扩展** $\text{map}[t.\ \text{list}(t)](x.e_0;\ e)$，其中 $x.e_0$ 是对列表元素的转换函数。
- **解释**：编写递归函数，对列表中的每个元素应用转换。

**练习2**：

- **证明**：在正类型算子中，泛型扩展保持类型正确性，即如果 $\text{map}[t.\tau](x.e_0;\ e) \rightarrow e''$，则 $e''$ 的类型为 $[\rho' / t]\tau$。

**思考**：

- **泛型编程的优势**：如何利用泛型编程提高代码的抽象程度和复用性？
- **类型系统的设计**：在支持泛型编程的编程语言中，如何设计类型系统，以确保类型安全性和表达能力的平衡？

---

**参考文献**：

- Hinze, R., & Jeuring, J. (2003). Generic Haskell: practice and theory. Lecture Notes in Computer Science, 2793, 1–56.
- MacLane, S. (1998). Categories for the Working Mathematician. Springer.

希望以上详细的讲解和公式解析能够帮助您深入理解 **第14章 泛型编程 (Generic Programming)**，特别是 **14.3 泛型扩展 (Generic Extension)** 的内容。如有任何疑问，欢迎提问！

### ---------------------------------

# 第14章 泛型编程 (Generic Programming)

在本章中，我们将深入探讨**泛型编程 (Generic Programming)** 的概念，特别关注**泛型扩展 (Generic Extension)** 的机制。泛型编程允许我们定义通用的操作或函数，这些操作可以应用于不同类型的数据结构，从而提高代码的重用性和灵活性。

---

## **14.3 泛型扩展 (Generic Extension)**

### **泛型扩展的形式**

**泛型扩展原语的形式为：**

$$
\text{map}[t.\tau](x.e_0;\ e)
$$

**符号解释：**

- **$\text{map}[t.\tau]$**：泛型映射操作，针对类型算子 $t.\tau$。
- **$x.e_0$**：一个抽象器 (abstraction)，表示一个从类型 $\rho$ 到类型 $\rho'$ 的转换函数，其中 $x$ 是类型为 $\rho$ 的变量，$e_0$ 是类型为 $\rho'$ 的表达式。
- **$e$**：一个表达式，类型为 $[\rho / t]\tau$，即在 $\tau$ 中用类型 $\rho$ 替换类型变量 $t$。

### **静态语义 (Statics)**

泛型扩展的类型规则由以下推导规则给出：

$$
\frac{
  t\ \text{type} \vdash \tau\ \text{type} \quad \Gamma,\ x : \rho \vdash e_0 : \rho' \quad \Gamma \vdash e : [\rho / t]\tau
}{
  \Gamma \vdash \text{map}[t.\tau](x.e_0;\ e) : [\rho' / t]\tau
}
\quad (14.1)
$$

**公式详解：**

1. **类型算子的类型声明**：

   - $t\ \text{type} \vdash \tau\ \text{type}$：在类型环境下，类型变量 $t$ 被声明为类型，$\tau$ 是一个可能包含 $t$ 的类型表达式。

2. **转换函数的类型检查**：

   - $\Gamma,\ x : \rho \vdash e_0 : \rho'$：在类型环境 $\Gamma$ 中，假设 $x$ 的类型为 $\rho$，则表达式 $e_0$ 的类型为 $\rho'$。这表示抽象器 $x.e_0$ 是从类型 $\rho$ 到 $\rho'$ 的转换函数。

3. **被转换表达式的类型**：

   - $\Gamma \vdash e : [\rho / t]\tau$：表达式 $e$ 的类型是 $[\rho / t]\tau$，即在类型表达式 $\tau$ 中用类型 $\rho$ 替换类型变量 $t$。

4. **泛型扩展表达式的类型**：

   - $\Gamma \vdash \text{map}[t.\tau](x.e_0;\ e) : [\rho' / t]\tau$：泛型扩展表达式的类型是 $[\rho' / t]\tau$，即在类型表达式 $\tau$ 中用类型 $\rho'$ 替换类型变量 $t$。

**总结**：泛型扩展 $\text{map}[t.\tau](x.e_0;\ e)$ 将类型为 $[\rho / t]\tau$ 的表达式 $e$ 转换为类型为 $[\rho' / t]\tau$ 的表达式，其中转换函数由 $x.e_0$ 指定，类型变量 $t$ 指示了在 $\tau$ 中需要应用转换的位置。

### **动态语义 (Dynamics)**

泛型扩展的计算规则如下，我们这里仅考虑多项式类型算子 (Polynomial Type Operators)：

#### **规则 (14.2a)：直接应用转换**

$$
\text{map}[t.t](x.e_0;\ e) \rightarrow [e / x]e_0
\quad (14.2a)
$$

**解释**：

- 当类型算子为 $t.t$ 时，表示类型算子直接是 $t$ 本身。
- 计算时，直接将 $e$ 代入转换函数 $x.e_0$，得到 $[e / x]e_0$。

**公式详解**：

- **$[e / x]e_0$**：表示在表达式 $e_0$ 中，用 $e$ 替换变量 $x$。

#### **规则 (14.2b)：单位类型的转换**

$$
\text{map}[t.\text{unit}](x.e_0;\ e) \rightarrow \langle\ \rangle
\quad (14.2b)
$$

**解释**：

- 当类型算子为 $t.\text{unit}$ 时，由于 $\text{unit}$ 类型只有一个值 $\langle\ \rangle$，因此转换结果仍然是 $\langle\ \rangle$。

#### **规则 (14.2c)：积类型的转换**

$$
\text{map}[t.\tau_1 \times \tau_2](x.e_0;\ e) \rightarrow \langle \text{map}[t.\tau_1](x.e_0;\ e \cdot l),\ \text{map}[t.\tau_2](x.e_0;\ e \cdot r) \rangle
\quad (14.2c)
$$

**解释**：

- 当类型算子为 $t.\tau_1 \times \tau_2$ 时，$e$ 是一个对。
- 计算时，对 $e$ 的左组件 $e \cdot l$ 和右组件 $e \cdot r$ 分别递归地应用泛型扩展。
- 将转换后的左、右组件重新组合成一个新的对。

**公式详解**：

- **$e \cdot l$**：表示对 $e$ 的左投影，类型为 $[\rho / t]\tau_1$。
- **$e \cdot r$**：表示对 $e$ 的右投影，类型为 $[\rho / t]\tau_2$。
- **$\text{map}[t.\tau_1](x.e_0;\ e \cdot l)$**：对左组件应用泛型扩展，结果类型为 $[\rho' / t]\tau_1$。
- **$\text{map}[t.\tau_2](x.e_0;\ e \cdot r)$**：对右组件应用泛型扩展，结果类型为 $[\rho' / t]\tau_2$。

#### **规则 (14.2d)：空类型的转换**

$$
\text{map}[t.\text{void}](x.e_0;\ e) \rightarrow \text{abort}(e)
\quad (14.2d)
$$

**解释**：

- 当类型算子为 $t.\text{void}$ 时，由于 $\text{void}$ 类型没有值，因此对 $e$ 的转换导致程序中止。

#### **规则 (14.2e)：和类型的转换**

$$
\begin{aligned}
\text{map}[t.\tau_1 + \tau_2](x.e_0;\ e) \rightarrow \ \text{case } e\ \{ &l \cdot x_1 \Rightarrow l \cdot \text{map}[t.\tau_1](x.e_0;\ x_1) \\
&| \ r \cdot x_2 \Rightarrow r \cdot \text{map}[t.\tau_2](x.e_0;\ x_2) \}
\end{aligned}
\quad (14.2e)
$$

**解释**：

- 当类型算子为 $t.\tau_1 + \tau_2$ 时，$e$ 是一个和类型的值。
- 计算时，对 $e$ 进行案例分析：
  - 如果 $e$ 是左注入 $l \cdot x_1$，则对 $x_1$ 应用泛型扩展 $\text{map}[t.\tau_1](x.e_0;\ x_1)$，然后再左注入。
  - 如果 $e$ 是右注入 $r \cdot x_2$，则对 $x_2$ 应用泛型扩展 $\text{map}[t.\tau_2](x.e_0;\ x_2)$，然后再右注入。

**公式详解**：

- **$l \cdot x_1$**：表示 $e$ 是左注入，$x_1$ 的类型为 $[\rho / t]\tau_1$。
- **$r \cdot x_2$**：表示 $e$ 是右注入，$x_2$ 的类型为 $[\rho / t]\tau_2$。

### **示例解析**

**示例**：考虑类型算子 $t.\ \text{unit} + (\text{bool} \times t)$，转换函数 $x.e$ 定义为 $x + 1$（将自然数加 1）。

**目标**：对值 $r \cdot \langle \text{true}, n \rangle$ 应用泛型扩展。

**步骤**：

1. **应用规则 (14.2e)**：

   $$
   \begin{aligned}
   \text{map}[t.\text{unit} + (\text{bool} \times t)](x.e;\ r \cdot \langle \text{true}, n \rangle) \rightarrow \\
   r \cdot \text{map}[t.\text{bool} \times t](x.e;\ \langle \text{true}, n \rangle)
   \end{aligned}
   $$

2. **应用规则 (14.2c)**：

   $$
   \text{map}[t.\text{bool} \times t](x.e;\ \langle \text{true}, n \rangle) \rightarrow \langle \text{map}[t.\text{bool}](x.e;\ \text{true}),\ \text{map}[t.t](x.e;\ n) \rangle
   $$

3. **处理左组件**（类型为 $\text{bool}$，不包含类型变量 $t$）：

   $$
   \text{map}[t.\text{bool}](x.e;\ \text{true}) \rightarrow \text{true}
   $$

4. **处理右组件**（类型为 $t$，需要应用转换）：

   $$
   \text{map}[t.t](x.e;\ n) \rightarrow [n / x]e = n + 1
   $$

5. **组合结果**：

   $$
   \langle \text{true}, n + 1 \rangle
   $$

6. **最终结果**：

   $$
   r \cdot \langle \text{true}, n + 1 \rangle
   $$

**解释**：由于类型变量 $t$ 在类型算子 $t.\ \text{unit} + (\text{bool} \times t)$ 中出现在第二个组件的位置，因此对输入值的第二个组件应用了转换函数，将自然数 $n$ 加 1。

### **定理 14.1（保持性，Preservation）**

**陈述**：

如果 $\text{map}[t.\tau](x.e_0;\ e) : \tau'$ 且 $\text{map}[t.\tau](x.e_0;\ e) \rightarrow e''$，则 $e'' : \tau'$。

**证明**：

根据类型规则 (14.1) 的逆推，我们有：

1. $t\ \text{type} \vdash \tau\ \text{type}$：类型算子的类型声明。

2. $x : \rho \vdash e_0 : \rho'$：转换函数 $x.e_0$ 的类型。

3. $e : [\rho / t]\tau$：被转换表达式的类型。

4. $\tau' = [\rho' / t]\tau$：泛型扩展表达式的类型。

**对计算规则进行分类讨论**：

- **以规则 (14.2c) 为例**：

  - 已知 $\text{map}[t.\tau_1 \times \tau_2](x.e_0;\ e) \rightarrow \langle e'_1, e'_2 \rangle$。

  - 其中：

    - $e'_1 = \text{map}[t.\tau_1](x.e_0;\ e \cdot l)$，类型为 $[\rho' / t]\tau_1$。

    - $e'_2 = \text{map}[t.\tau_2](x.e_0;\ e \cdot r)$，类型为 $[\rho' / t]\tau_2$。

  - 因此，$\langle e'_1, e'_2 \rangle$ 的类型为 $[\rho' / t]\tau_1 \times \tau_2$，即 $[\rho' / t](\tau_1 \times \tau_2)$。

- **结论**：在每个计算步骤中，表达式的类型都保持不变，满足类型系统的要求。

---

## **正类型算子 (Positive Type Operators)**

### **定义**

**正类型算子**是扩展了多项式类型算子的类型算子，允许有限制的函数类型形式。具体来说，类型算子 $t.\tau_1 \rightarrow \tau_2$ 是正类型算子，条件是：

1. **类型变量 $t$ 不出现在 $\tau_1$ 中**（即 $t$ **不**在函数的参数类型中出现）。

2. **$t.\tau_2$ 是正类型算子**。

**正/负出现的定义**：

- **正出现 (Positive Occurrence)**：类型变量 $t$ 出现在函数返回类型、积类型或和类型中。

- **负出现 (Negative Occurrence)**：类型变量 $t$ 出现在函数参数类型中。

### **泛型扩展的规则**

对于正类型算子，泛型扩展增加了以下计算规则：

$$
\text{map}[t.\tau_1 \rightarrow \tau_2](x.e_0;\ e) \rightarrow \lambda (x_1 : \tau_1) .\ \text{map}[t.\tau_2](x.e_0;\ e(x_1))
\quad (14.3)
$$

**公式详解**：

- **$\lambda (x_1 : \tau_1)$**：构造一个新的匿名函数，其参数 $x_1$ 类型为 $\tau_1$。

- **$e(x_1)$**：对函数 $e$ 应用参数 $x_1$。

- **$\text{map}[t.\tau_2](x.e_0;\ e(x_1))$**：对结果 $e(x_1)$ 递归地应用泛型扩展，类型为 $[\rho' / t]\tau_2$。

**解释**：

- 由于 $t$ 不在 $\tau_1$ 中出现，因此参数类型 $\tau_1$ 不受泛型扩展影响。

- 泛型扩展只对返回类型中的 $t$ 进行处理。

### **为什么限制类型变量 $t$ 的出现**

**问题**：如果允许类型变量 $t$ 出现在函数参数类型中（即允许负出现），泛型扩展将遇到困难。

**原因**：

- 在泛型扩展过程中，我们需要将类型为 $[\rho' / t]\tau_1$ 的参数 $x_1$ 传递给类型为 $[\rho / t]\tau_1$ 的函数 $e$。

- 由于 $[\rho' / t]\tau_1$ 与 $[\rho / t]\tau_1$ 可能是不同的类型，无法直接传递。

**解决方案**：

- 限制类型变量 $t$ 只在类型算子的正位置出现，避免负出现。

- 如果必须处理负出现，需要提供转换函数的逆函数，但一般情况下转换函数不可逆。

---

## **14.4 注解 (Notes)**

### **泛型扩展与函子 (Functors) 的关系**

- **函子 (Functor)**：在范畴论 (Category Theory) 中，函子是范畴之间的映射，保持结构的映射。

- **泛型扩展**：泛型扩展是函子的一个实例，它将类型算子视为函子，将类型和函数映射到新的类型和函数。

### **泛型编程的本质**

- **函子化编程 (Functorial Programming)**：泛型编程本质上是函子化编程，利用多项式类型算子的函子行为。

- **参考**：

  - **MacLane, S. (1998). Categories for the Working Mathematician**：介绍了范畴论的基础。

  - **Hinze, R., & Jeuring, J. (2003). Generic Haskell: Practice and Theory**：探讨了泛型编程与函子之间的关系。

---

## **总结**

- **泛型扩展**提供了一种机制，可以将针对某一类型的转换函数自动应用到更复杂的数据结构中。

- **类型变量 $t$ 的作用**：在类型算子中标记需要应用转换的位置，确保泛型扩展的正确性和一致性。

- **正类型算子**：通过限制类型变量 $t$ 的出现位置，确保泛型扩展的类型安全性。

- **泛型编程与范畴论的联系**：泛型编程可以视为在编程语言中应用范畴论的概念，特别是函子的概念。

---

## **练习与思考**

**练习1**：

- **定义类型算子**：$t.\ \text{List}(t) = \text{unit} + (t \times \text{List}(t))$，表示元素类型为 $t$ 的列表类型。

- **实现泛型扩展**：编写泛型扩展 $\text{map}[t.\ \text{List}(t)](x.e_0;\ e)$，其中 $x.e_0$ 是元素类型的转换函数。

- **要求**：递归地对列表中的每个元素应用转换函数 $x.e_0$。

**练习2**：

- **证明**：对于正类型算子，泛型扩展 $\text{map}[t.\tau](x.e_0;\ e)$ 的计算结果 $e''$ 满足类型保持性，即 $e'' : [\rho' / t]\tau$。

**思考**：

- **泛型编程的优势**：在实际编程中，如何利用泛型编程提高代码的可重用性和可维护性？

- **类型系统设计**：如何设计支持泛型编程的类型系统，既保证类型安全性，又提供足够的表达能力？

---

希望以上详细的解释和公式解析能够帮助您深入理解 **14.3 泛型扩展 (Generic Extension)** 和 **14.4 注解 (Notes)** 的内容。如有任何疑问，欢迎提问！

### ---------------------------------

**范型式**（Parametric polymorphism，也称为**多态类型**）通常不直接属于**有限数据类型**或**无限数据类型**中的任何一种分类。它主要是一种**类型系统特性**，允许类型参数化和泛型编程。为了清晰区分有限数据类型和无限数据类型，我们可以这样理解：

1. **有限数据类型**指的是由有限构造方式生成的数据类型，例如布尔类型、枚举类型、有限长度的元组、有限集合等。这些类型的值集是有限的或由有限方式构建的。
   - 例如：
     - 布尔类型 `Bool`：`True` 和 `False` 是有限的。
     - 枚举类型 `Color = Red | Green | Blue`：它包含有限个可能值。

2. **无限数据类型**通常表示那些可以包含无限结构的数据类型，特别是递归类型，如链表、树等。
   - 例如：
     - 链表 `List a`：可以包含无限多的元素（尽管在实际程序运行中会受到内存限制）。
     - 树 `Tree a`：可以通过递归定义，理论上构建无限结构。

### **范型式的本质**：
**范型式**是一种**抽象机制**，允许数据类型或函数依赖于**类型参数**。例如，在泛型编程中，你可以定义一个通用的列表类型 `List a`，其中 `a` 可以是任意类型。范型式关注的是**类型的参数化和抽象**，而不是具体的值或数据的构造。

例如：
```haskell
data List a = Nil | Cons a (List a)
```

在这个例子中，`List a` 是一个范型数据类型，它可以根据参数 `a` 的不同实例化为不同的类型（例如 `List Int`, `List Bool`）。尽管 `List a` 可以表示无限长的列表，它本身并不属于有限数据类型或无限数据类型。相反，它是一个**类型构造器**，允许构建基于不同参数的具体类型。

### **范型式与有限/无限数据类型的关系**：

- **范型式可以用于构造有限数据类型**：例如，你可以定义一个泛型的有限长度的元组类型，通过范型式使其适用于不同类型的元素。
  
  ```haskell
  data Tuple2 a b = Tuple2 a b
  ```

  在这种情况下，`Tuple2 Int Bool` 是一个包含两个元素（`Int` 和 `Bool`）的元组，它的结构是有限的。

- **范型式也可以用于构造无限数据类型**：例如，定义泛型递归数据结构（如链表、树），允许构造无限数据结构的实例。

  ```haskell
  data Tree a = Empty | Node a (Tree a) (Tree a)
  ```

  这里，`Tree a` 可以表示任意类型的二叉树，理论上可以是无限的。

### **总结**：

范型式本质上是一种类型参数化机制，它既可以用于构造**有限数据类型**，也可以用于构造**无限数据类型**。因此，范型式并不直接属于有限数据类型或无限数据类型中的任何一种，而是提供了一种抽象能力，使得类型可以根据参数灵活变化。


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


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------