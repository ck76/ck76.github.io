[toc]



你提到的是经典命题逻辑中的一个重要系统——**Hilbert 演绎系统**，它是逻辑学中最基础、最常用的演绎系统之一。在这个系统中，推理是基于公理和推演规则（如分离规则）的。下面我将详细解释Hilbert 演绎系统的组成、定义，以及如何在这个系统中进行形式证明和推理。

### 1. Hilbert 演绎系统的基本组成

**Hilbert 演绎系统**由三个公理模式和一个推演规则（分离规则，MP）组成。这个系统的推理过程是基于这些公理和规则来生成形式定理的。

#### 公理模式
在 Hilbert 系统中，有三个公理模式，每个公理模式实际上代表了无穷多个具体公理，因为 $A$、$B$ 和 $C$ 是任意公式。

1. **公理模式 1**: $(A \rightarrow (B \rightarrow A))$
   - 这个公理表示任何命题 $A$ 都可以蕴涵出另一个命题 $B \rightarrow A$，即 $A$ 总是成立的。

2. **公理模式 2**: $((A \rightarrow (B \rightarrow C )) \rightarrow ((A \rightarrow B) \rightarrow (A \rightarrow C )))$
   - 这个公理表达了蕴涵的传递性。它可以理解为，如果 $A$ 可以蕴涵 $B \rightarrow C$，且 $A \rightarrow B$ 也成立，那么 $A$ 必然能直接蕴涵 $C$。

3. **公理模式 3**: $(((\neg A) \rightarrow (\neg B)) \rightarrow (B \rightarrow A))$
   - 这个公理描述了反对称性。如果命题 $A$ 的否定蕴涵 $B$ 的否定，那么 $B$ 蕴涵 $A$。

#### 分离规则（Modus Ponens, MP）
分离规则是 Hilbert 系统中的唯一推演规则。它可以描述为：

- 如果 $A$ 是一个公式，且 $A \rightarrow B$ 也是一个公式，那么我们可以推导出 $B$。
- 形式化表示为：$$\frac{A, A \rightarrow B}{B}$$

### 2. 形式证明

**形式证明**是指在 Hilbert 系统中，通过一系列推导步骤，从公理和已知公式出发，逐步得到一个公式的过程。

- **定义 3.12（形式证明）**：设有一个公式序列 $A_1, A_2, \dots, A_n$。这个序列是一个形式证明，当且仅当对于每一个 $A_i$（$1 \leq i \leq n$），下列条件之一成立：
  1. $A_i$ 是一个公理。
  2. 存在 $j, k < i$ 使得 $A_i$ 是由 $A_j$ 和 $A_k$ 应用分离规则（MP）得到的。
  

一个形式证明的长度是序列中公式的个数。

### 3. 形式定理

**形式定理**是指可以通过形式证明得出的公式。

- **定义 3.13（形式定理）**：如果存在一个形式证明 $A_1, A_2, \dots, A_n$，使得 $A_n = A$，则称公式 $A$ 为一个形式定理。换句话说，公式 $A$ 是形式可证的。

### 4. 形式推演

**形式推演**是从假定的公式集合出发，通过应用公理和分离规则，逐步推导出新的公式的过程。

- **定义 3.14（形式推演）**：设 $\Gamma$ 是一个公式集合，公式序列 $A_1, A_2, \dots, A_n$ 是由 $\Gamma$ 出发的形式推演，当且仅当对于每一个 $A_i$（$1 \leq i \leq n$），下列条件之一成立：
  1. $A_i \in \Gamma$。
  2. $A_i$ 是一个公理。
  3. 存在 $j, k < i$ 使得 $A_i$ 是由 $A_j$ 和 $A_k$ 应用分离规则（MP）得到的。

如果存在由 $\Gamma$ 出发的推演，使得 $A_n = A$，则称 $A$ 是 $\Gamma$ 的语义后承，记作 $\Gamma \vdash A$。

### 5. 一致性定理

**一致性定理**是指在命题演算中的形式定理都是永真的。换句话说，如果一个公式可以在 Hilbert 系统中证明为形式定理，那么它在任何模型下都是永真的。

### 6. 例子：证明 $p \rightarrow p$ 是形式定理

**例 3.1**：证明 $∅ ⊢ p \rightarrow p$，即 $p \rightarrow p$ 在 Hilbert 系统内可证，且 $p \rightarrow p$ 是一个定理。

我们可以通过如下步骤证明：

1. $p \rightarrow ((p \rightarrow p) \rightarrow p)$（公理 1）
2. $(p \rightarrow ((p \rightarrow p) \rightarrow p)) \rightarrow ((p \rightarrow (p \rightarrow p)) \rightarrow (p \rightarrow p))$（公理 2）
3. $((p \rightarrow (p \rightarrow p)) \rightarrow (p \rightarrow p))$（从步骤 1 和 2，应用 MP 规则）
4. $p \rightarrow (p \rightarrow p)$（公理 1）
5. $p \rightarrow p$（从步骤 4 和 3，应用 MP 规则）

通过以上证明步骤，我们得到了 $p \rightarrow p$ 是一个形式定理。

### 总结

Hilbert 演绎系统是经典逻辑的一种重要推理系统，通过定义公理模式和推理规则，提供了一个形式化的框架，用于证明和推导命题逻辑中的公式。理解这个系统可以帮助我们深入掌握逻辑推理的基础，并理解形式系统中的证明过程。