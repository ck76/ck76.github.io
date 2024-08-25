[toc]

### 简单类型 λ 演算 (Simply Typed Lambda Calculus)

#### 1. 简介与语法
简单类型 $\lambda$ 演算是λ演算的一个扩展，其中每个λ表达式都带有类型。通过类型系统，可以对程序进行静态检查，从而在编译时发现可能的错误。

**类型的 BNF 规则如下：**
$$
A, B ::= \iota \;|\; A \rightarrow B \;|\; A \times B \;|\; 1 \;|\; 0
$$
1. **基础类型** $\iota$：代表基本数据类型，如整数 (integers)，布尔值 (booleans) 等。
2. **函数类型** $A \rightarrow B$：表示从类型 $A$ 到类型 $B$ 的函数。
3. **乘积类型** $A \times B$：表示一个二元组 $\langle x, y \rangle$ 的类型，其中 $x$ 的类型为 $A$，$y$ 的类型为 $B$。
4. **单位类型** $1$：表示只有一个元素的类型，如程序语言中的 “unit” 或 Haskell 中的 `()` 类型。
5. **空类型** $0$：表示没有任何元素的类型。

**λ项（λ terms, 或者 λ表达式）的语法：**
$$
M, N ::= x \;|\; M N \;|\; \lambda x : A.M \;|\; \langle M, N \rangle \;|\; \pi_i M \;|\; \ast \;|\; \square M
$$
- $\langle M, N \rangle$：表示一个二元组。
- $\pi_i M$：表示从二元组中投射（projection）出第 $i$ 个元素。
- $\ast$：表示单位类型 $1$ 的唯一元素。
- $\square M$：将类型为 $0$ 的元素 $M$ 转换为类型 $A$。

#### 2. Curry-Howard 同构

Curry-Howard 同构展示了简单类型 λ 演算和直觉主义逻辑之间的对应关系：
- **类型** 对应 **命题**。
- **类型居留（type inhabitation）** 对应 **命题的可证明性**。
- **λ表达式** 对应 **逻辑证明**。

在这种同构下，类型居留问题可以转换为逻辑中的命题可证明性问题。具体来说，如果类型可以居留，则相应的逻辑公式是永真的。

例如，考虑以下逻辑公式与对应的λ类型：
1. $(A \times B) \rightarrow A$
   - 对应的逻辑公式：$(A \land B) \rightarrow A$
   - 该公式是永真的，因此存在对应的λ表达式：$\lambda x.\pi_1 x$。

2. $(A \rightarrow B) \rightarrow (B \rightarrow C) \rightarrow (A \rightarrow C)$
   - 对应的逻辑公式：$(A \rightarrow B) \rightarrow (B \rightarrow C) \rightarrow (A \rightarrow C)$
   - 该公式是永真的，因此存在对应的λ表达式：$\lambda f.\lambda g.\lambda x.g(fx)$。

#### 3. 定型规则与类型导出
简单类型 λ 演算有严格的定型规则，通过这些规则可以进行类型检查和类型推导。

**主要的定型规则：**
1. **(var)** 假设 $x$ 有类型 $A$，则 $x : A$。
   $$\frac{}{x : A \vdash x : A}$$

2. **(app)** 如果 $M$ 的类型为 $A \rightarrow B$ 且 $N$ 的类型为 $A$，则 $M N$ 的类型为 $B$。
   $$\frac{\Gamma \vdash M : A \rightarrow B \quad \Gamma \vdash N : A}{\Gamma \vdash M N : B}$$

3. **(abs)** 假设 $x$ 的类型为 $A$，如果 $M$ 在这种假设下有类型 $B$，则 $\lambda x : A.M$ 的类型为 $A \rightarrow B$。
   $$\frac{\Gamma, x : A \vdash M : B}{\Gamma \vdash \lambda x : A.M : A \rightarrow B}$$

4. **(pair)** 如果 $M$ 和 $N$ 分别有类型 $A$ 和 $B$，则二元组 $\langle M, N \rangle$ 的类型为 $A \times B$。
   $$\frac{\Gamma \vdash M : A \quad \Gamma \vdash N : B}{\Gamma \vdash \langle M, N \rangle : A \times B}$$

5. **(projection)** 如果 $M$ 有类型 $A \times B$，则 $\pi_1 M$ 的类型为 $A$，$\pi_2 M$ 的类型为 $B$。
   $$\frac{\Gamma \vdash M : A \times B}{\Gamma \vdash \pi_1 M : A \quad \Gamma \vdash \pi_2 M : B}$$

#### 4. 类型检查与类型推导
- **类型检查（Type Checking）**：给定一个类型环境 $\Gamma$、表达式 $M$ 和类型 $A$，如果存在类型导出 $\Gamma \vdash M : A$，则类型检查通过。
- **类型推导（Type Inference）**：给定一个类型环境 $\Gamma$ 和表达式 $M$，确定 $M$ 的类型 $A$。

#### 5. 正则性（Normalization）
正则性是 λ 演算中一个重要的性质，表明表达式经过一定的规约后会变成某种“标准形式”或“值”。

- **强正则性（Strong Normalization）**：无论选择何种规约策略，表达式最终都可以规约到标准形式。
- **弱正则性（Weak Normalization）**：存在一种规约策略，使得表达式可以规约到标准形式。

### 范畴论与类型论的联系
- 范畴论中的对象和态射对应着类型系统中的类型和函数。
- 函子（Functor）可以看作类型转换或函数的提升，它们在范畴论和类型论中都有广泛的应用。
- Curry-Howard 同构展示了逻辑系统与类型系统之间的深层次联系，反映了逻辑证明与程序构造的统一性。

这种联系帮助我们理解编程语言中类型系统的数学基础，并揭示了编程语言设计背后的逻辑结构。通过进一步研究类型论和范畴论，可以更深入理解编程语言的类型系统，特别是在函数式编程语言中的应用，如 Haskell 和 ML。