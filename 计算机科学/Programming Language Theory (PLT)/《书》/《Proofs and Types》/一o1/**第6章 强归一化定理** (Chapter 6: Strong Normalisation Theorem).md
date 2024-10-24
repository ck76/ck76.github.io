[toc]



### **第6章 强规范化定理**

#### **引言**

在这一章中，我们将证明**简单类型化λ演算（simply typed λ-calculus）**的**强规范化定理（Strong Normalisation Theorem）**。虽然我们之前已经详细讨论了这个主题，特别是已经证明了**弱规范化定理（Weak Normalisation Theorem）**，但本章的目的是介绍一种技术，我们将在后面将其应用到 **System F**。

对于简单类型化λ演算，有一些证明理论的技术可以将证明的论证表达为算术，甚至是在非常弱的系统中。然而，我们的方法可以直接扩展到 **G\"odel的System T**，它包括一个整数类型，因此可以编码 **皮亚诺算术（Peano Arithmetic，PA）**。因此，强规范化暗含了 PA 的一致性，这意味着它本身不能在 PA 中被证明（第二不完备定理）。

因此，我们必须使用一个强的归纳假设，为此我们引入一个称为 **可归约性（reducibility）** 的抽象概念，最初由 [Tait] 提出。一些技术改进，如中性项（neutral terms），来自于 [Gir72]。除了证明强规范化外，我们还确定了可归约性的三个重要性质（**CR 1-3**），我们将在第14章的 **System F** 中使用。

---

### **6.1 可归约性（Reducibility）**

#### **定义：**

我们通过对类型 $T$ 进行归纳，定义一个集合 $\text{RED}_T$（类型 $T$ 的**可归约项**，reducible terms of type $T$）。

1. **原子类型（Atomic Types）**：
   - 对于类型为原子类型 $T$ 的项 $t$，如果 $t$ 是**强规范化的（strongly normalisable）**，则 $t$ 是**可归约的**，即 $t \in \text{RED}_T$。
   - **强规范化**意味着从 $t$ 开始的任何归约序列都是有限的，即 $t$ 不能归约成无限长的序列。

2. **乘积类型（Product Type）**：
   - 对于类型为 $U \times V$ 的项 $t$，如果 $\pi_1 t$ 和 $\pi_2 t$ 都是可归约的，则 $t$ 是可归约的。
   - 这里，$\pi_1 t$ 和 $\pi_2 t$ 分别表示 $t$ 的第一个和第二个投影。

3. **箭头类型（Arrow Type）**：
   - 对于类型为 $U \to V$ 的项 $t$，如果对于**所有类型为 $U$ 的可归约项** $u$，$t \ u$ 是类型为 $V$ 的可归约项，则 $t$ 是可归约的。
   - 这意味着 $t$ 应用于任何可归约的输入 $u$，结果 $t \ u$ 都是可归约的。

#### **深层原因：**

可归约性之所以有效，而组合的直觉（combinatorial intuition）失败，是因为它具有**逻辑复杂性**。具体来说，我们有：

$$
t \in \text{RED}_{U \to V} \quad \text{当且仅当} \quad \forall u \ (u \in \text{RED}_U \implies t \ u \in \text{RED}_V)
$$

我们看到，在对 $U \to V$ 进行处理时，$\text{RED}_U$ 被**否定**，并且添加了一个**全称量词**。特别地，规范化的论证不能直接在算术中形式化，因为 $t \in \text{RED}_T$ 不能表达为 $t$ 和 $T$ 的算术公式。

---

### **6.2 可归约性的性质**

首先，我们引入**中性项（Neutral Terms）**的概念：如果一个项不是形如 $\langle u, v \rangle$ 或 $\lambda x. v$ 的形式，则称其为中性的。换句话说，中性项是以下形式的项：

- **变量**：$x$
- **投影**：$\pi_1 t$, $\pi_2 t$
- **应用**：$t \ u$

#### **重要的条件（CR 1-3）：**

我们感兴趣的条件如下：

1. **(CR 1)** 如果 $t \in \text{RED}_T$，则 $t$ 是**强规范化的**。

2. **(CR 2)** 如果 $t \in \text{RED}_T$ 且 $t \to t'$，则 $t' \in \text{RED}_T$。

   - 这意味着可归约性在归约过程中是保持的。

3. **(CR 3)** 如果 $t$ 是**中性的**，并且当我们对 $t$ 的一个约简式（redex）进行转换时，得到的项 $t'$ 满足 $t' \in \text{RED}_T$，那么 $t \in \text{RED}_T$。

   - 换句话说，如果一个中性项的所有一步归约结果都是可归约的，那么该中性项本身也是可归约的。

**特别情况**：

- **(CR 4)** 如果 $t$ 是**中性的**且**规范的（normal）**，则 $t \in \text{RED}_T$。

#### **验证：**

我们将通过对类型的归纳，验证 $\text{RED}_T$ 满足这些条件。

---

#### **6.2.1 原子类型**

对于原子类型的项，一个项可归约当且仅当它是强规范化的。因此，我们必须证明**强规范化的项的集合**满足上述三个条件：

- **(CR 1)**：显然成立，因为定义就是如此。

- **(CR 2)**：如果 $t$ 是强规范化的，则任何 $t$ 归约到的项 $t'$ 也是强规范化的。

- **(CR 3)**：从 $t$ 开始的任何归约路径必须经过 $t$ 的一些 $t'$，这些 $t'$ 是强规范化的，因此路径是有限的。事实上，很容易看出，对于 $t$ 的任何一步归约 $t \to t'$，$t$ 的归约长度 $\nu(t)$ 满足 $\nu(t) = \max(\nu(t')) + 1$。

---

#### **6.2.2 乘积类型**

对于乘积类型的项，一个项 $t$ 可归约当且仅当其投影 $\pi_1 t$ 和 $\pi_2 t$ 是可归约的。

- **(CR 1)**：假设 $t$ 类型为 $U \times V$，且 $t$ 是可归约的。那么，$\pi_1 t$ 和 $\pi_2 t$ 是可归约的，根据对 $U$ 和 $V$ 的归纳假设（CR 1），$\pi_1 t$ 和 $\pi_2 t$ 是强规范化的。

  - 此外，$\nu(t) \leq \max(\nu(\pi_1 t), \nu(\pi_2 t))$，因为对于任何归约序列 $t, t_1, t_2, \dots$，我们可以应用 $\pi_1$（或 $\pi_2$）构造归约序列 $\pi_1 t, \pi_1 t_1, \pi_1 t_2, \dots$。

  - 因此，$\nu(t)$ 有限，$t$ 是强规范化的。

- **(CR 2)**：如果 $t \to t'$，则 $\pi_1 t \to \pi_1 t'$，$\pi_2 t \to \pi_2 t'$。

  - 由于 $t$ 是可归约的，$\pi_1 t$ 和 $\pi_2 t$ 是可归约的。

  - 根据对 $U$ 和 $V$ 的归纳假设（CR 2），$\pi_1 t'$ 和 $\pi_2 t'$ 是可归约的，因此 $t'$ 是可归约的。

- **(CR 3)**：设 $t$ 是中性的，并且 $t$ 的所有一步归约结果 $t'$ 都是可归约的。

  - 在 $\pi_1 t$ 中应用一个内部转换，结果是 $\pi_1 t'$，因为 $t$ 不是一个对（pair），$\pi_1 t$ 不能自身成为一个约简式。

  - $\pi_1 t'$ 是可归约的，因为 $t'$ 是可归约的。

  - 由于 $\pi_1 t$ 是中性的，并且其所有一步归约结果都是可归约的，根据对 $U$ 的归纳假设（CR 3），$\pi_1 t$ 是可归约的。

  - 同理可得 $\pi_2 t$ 是可归约的，因此 $t$ 是可归约的。

---

#### **6.2.3 箭头类型**

对于箭头类型的项，一个项 $t$ 可归约当且仅当它应用于任何类型为 $U$ 的可归约项 $u$，结果 $t \ u$ 是可归约的。

- **(CR 1)**：如果 $t$ 是类型为 $U \to V$ 的可归约项，让 $x$ 是类型为 $U$ 的变量。

  - 根据对 $U$ 的归纳假设（CR 3），中性且规范的项 $x$ 是可归约的。

  - 因此，$t \ x$ 是可归约的。

  - 类似于乘积类型的情况，我们注意到 $\nu(t) \leq \nu(t \ x)$。

  - 根据对 $V$ 的归纳假设（CR 1），$\nu(t \ x)$ 有限，因此 $\nu(t)$ 有限，$t$ 是强规范化的。

- **(CR 2)**：如果 $t \to t'$ 且 $t$ 是可归约的，取类型为 $U$ 的可归约项 $u$。

  - 那么 $t \ u$ 是可归约的，并且 $t \ u \to t' \ u$。

  - 根据对 $V$ 的归纳假设（CR 2），$t' \ u$ 是可归约的，因此 $t'$ 是可归约的。

- **(CR 3)**：设 $t$ 是中性的，并且 $t$ 的所有一步归约结果 $t'$ 都是可归约的。取类型为 $U$ 的可归约项 $u$，我们需要证明 $t \ u$ 是可归约的。

  - 根据对 $U$ 的归纳假设（CR 1），$u$ 是强规范化的，因此我们可以对 $\nu(u)$ 进行归纳。

  - 在一步归约中，$t \ u$ 可能归约为：

    1. **$t' \ u$**：其中 $t' \to t'$，$t'$ 是可归约的，因此 $t' \ u$ 是可归约的。

    2. **$t \ u'$**：其中 $u \to u'$，$u'$ 是可归约的，$\nu(u') < \nu(u)$，根据归纳假设，$t \ u'$ 是可归约的。

    3. **注意**：$t \ u$ 不能自身成为一个约简式，因为 $t$ 不是形如 $\lambda x. v$ 的形式。

  - 在每种情况下，我们看到中性项 $t \ u$ 只归约到可归约的项。根据对 $V$ 的归纳假设（CR 3），$t \ u$ 是可归约的，因此 $t$ 是可归约的。

---

### **6.3 可归约性定理**

#### **6.3.1 配对（Pairing）**

**引理**：如果 $u$ 和 $v$ 是可归约的，则 $\langle u, v \rangle$ 是可归约的。

**证明**：

- 根据 (CR 1)，$u$ 和 $v$ 是强规范化的。

- 我们可以对 $\nu(u) + \nu(v)$ 进行归纳，证明 $\pi_1 \langle u, v \rangle$ 是可归约的。

- $\pi_1 \langle u, v \rangle$ 可能归约为：

  1. **$u$**：$u$ 是可归约的。

  2. **$\pi_1 \langle u', v \rangle$**：其中 $u \to u'$，$u'$ 是可归约的，$\nu(u') < \nu(u)$，根据归纳假设，该项是可归约的。

  3. **$\pi_1 \langle u, v' \rangle$**：其中 $v \to v'$，类似地，该项是可归约的。

- 在每种情况下，中性项 $\pi_1 \langle u, v \rangle$ 只归约到可归约的项。根据 (CR 3)，$\pi_1 \langle u, v \rangle$ 是可归约的。

- 同理可得，$\pi_2 \langle u, v \rangle$ 是可归约的，因此 $\langle u, v \rangle$ 是可归约的。

---

#### **6.3.2 抽象（Abstraction）**

**引理**：如果对于所有类型为 $U$ 的可归约项 $u$，$v[u / x]$ 是可归约的，那么 $\lambda x. v$ 是可归约的。

**证明**：

- 我们需要证明，对于所有类型为 $U$ 的可归约项 $u$，$(\lambda x. v) \ u$ 是可归约的。

- 我们对 $\nu(v) + \nu(u)$ 进行归纳。

- $(\lambda x. v) \ u$ 可能归约为：

  1. **$v[u / x]$**：根据假设，它是可归约的。

  2. **$(\lambda x. v') \ u$**：其中 $v \to v'$，$v'$ 是可归约的，$\nu(v') < \nu(v)$，根据归纳假设，该项是可归约的。

  3. **$(\lambda x. v) \ u'$**：其中 $u \to u'$，$u'$ 是可归约的，$\nu(u') < \nu(u)$，根据归纳假设，该项是可归约的。

- 在每种情况下，中性项 $(\lambda x. v) \ u$ 只归约到可归约的项。根据 (CR 3)，$(\lambda x. v) \ u$ 是可归约的，因此 $\lambda x. v$ 是可归约的。

---

#### **6.3.3 定理**

**定理**：所有的项都是可归约的。

**推论**：根据 (CR 1)，所有的项都是**强规范化的**。

**证明**：

- 在定理的证明中，我们需要一个更强的归纳假设来处理抽象的情况。

- **命题**：设 $t$ 是任意项（不假定其可归约），并且假设 $t$ 的所有自由变量都在 $x_1, \dots, x_n$ 中，类型分别为 $U_1, \dots, U_n$。如果 $u_1, \dots, u_n$ 是类型分别为 $U_1, \dots, U_n$ 的可归约项，那么 $t[u_1 / x_1, \dots, u_n / x_n]$ 是可归约的。

- **证明**：

  - 对 $t$ 进行结构归纳。

  - 记 $t[ u / x]$ 表示 $t[u_1 / x_1, \dots, u_n / x_n]$。

  1. **$t$ 是 $x_i$**：需要验证“如果 $u_i$ 是可归约的，则 $u_i$ 是可归约的”这个显然成立的命题。

  2. **$t$ 是 $\pi_1 w$**：根据归纳假设，对于所有可归约的 $u$，$w[ u / x]$ 是可归约的。

     - 这意味着 $\pi_1 (w[ u / x])$ 是可归约的，但这个项就是 $\pi_1 w[ u / x] = t[ u / x]$。

  3. **$t$ 是 $\pi_2 w$**：同理。

  4. **$t$ 是 $\langle v, w \rangle$**：根据归纳假设，$v[ u / x]$ 和 $w[ u / x]$ 是可归约的。

     - 根据引理 6.3.1，$t[ u / x] = \langle v[ u / x], w[ u / x] \rangle$ 是可归约的。

  5. **$t$ 是 $w \ v$**：根据归纳假设，$w[ u / x]$ 和 $v[ u / x]$ 是可归约的。

     - 因此，$w[ u / x] \ (v[ u / x])$ 是可归约的，但这个项就是 $t[ u / x]$。

  6. **$t$ 是 $\lambda y. w$，类型为 $V \to W$**：根据归纳假设，对于所有类型为 $V$ 的 $v$，$w[ u / x, v / y]$ 是可归约的。

     - 根据引理 6.3.2，$t[ u / x] = \lambda y. (w[ u / x])$ 是可归约的。

- 由此证明了所有的项都是可归约的。

---

### **总结**

在本章中，我们通过引入**可归约性（reducibility）**的概念，证明了**强规范化定理**：

- **所有的项都是可归约的**，因此根据 (CR 1)，**所有的项都是强规范化的**。

我们通过对类型和项进行归纳，证明了可归约性满足一系列重要的性质（CR 1-3）：

- **(CR 1)**：可归约的项是强规范化的。

- **(CR 2)**：可归约性在归约过程中保持。

- **(CR 3)**：对于中性项，如果其所有一步归约结果都是可归约的，那么它本身也是可归约的。

这些性质对于处理更复杂的系统（如 **System F**）非常重要。

---

### **后续展望**

- 在后续章节中，我们将应用这些技术到更复杂的类型系统，如 **System F**，并探讨如何证明其强规范化定理。

- 我们还将进一步研究可归约性的概念，以及它在类型论和逻辑中的应用。

如果您对本章内容有任何疑问，或者需要进一步的解释，请随时提出来！

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



### ---------------------------



### ----------------------------