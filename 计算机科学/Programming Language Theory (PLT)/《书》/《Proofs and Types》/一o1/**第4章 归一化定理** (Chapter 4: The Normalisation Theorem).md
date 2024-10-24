[toc]



### **第4章 规范化定理**

#### **引言**

在本章中，作者讨论了确保**类型化λ演算**在计算上表现良好的两个重要结果：

1. **规范化定理（Normalization Theorem）**：保证了**正常形式**的存在。
2. **Church-Rosser 性质**：保证了正常形式的唯一性。

虽然本章主要关注规范化定理的证明，但作者也简要提及了 Church-Rosser 性质，尽管没有给出详细的证明，因为这超出了类型理论的范围，并且在文献中已经有充分的讨论（如 [Barendregt]）。

规范化定理有两种形式：

- **弱规范化定理**：存在某种**终止的规范化策略**，即对于每个项，都存在一个转换序列将其归约到正常形式。本章将证明这一结果。
- **强规范化定理**：**所有可能的规范化策略**都终止，即无论选择哪种归约路径，都能在有限步内达到正常形式。这将在第6章中证明。

---

### **4.1 Church-Rosser 性质**

#### **陈述**

Church-Rosser 性质声明了**正常形式的唯一性**，独立于其存在性。即使在规范化定理不成立的系统（如**无类型λ演算**）中，这一性质也有意义。

**定理（Church-Rosser 定理）**：

如果项 $t$ 可以分别归约到项 $u$ 和 $v$（记为 $t \to^* u$ 和 $t \to^* v$），那么存在一个项 $w$，使得 $u$ 和 $v$ 都可以归约到 $w$（$u \to^* w$ 和 $v \to^* w$）。

用图示表示：

```
      t
     / \
    u   v
     \ /
      w
```

#### **推论**

- **正常形式的唯一性**：一个项 $t$ 的正常形式**至多**有一个。

**证明**：

- 假设 $u$ 和 $v$ 是 $t$ 的两个正常形式。
- 根据 Church-Rosser 定理，存在一个项 $w$，使得 $u \to^* w$ 和 $v \to^* w$。
- 由于 $u$ 和 $v$ 都是正常形式，不能进一步归约，因此 $u = w = v$。

因此，正常形式是唯一的。

#### **讨论**

- **证明的复杂性**：Church-Rosser 定理的证明相当复杂，特别是如果我们尝试用蛮力证明的话。
- **适用范围**：该定理可以针对多种系统陈述，其证明通常具有相似的结构。

#### **直接推论**

- **演算的一致性**：根据 Church-Rosser 定理，演算系统是一致的，即不可能从基本等式推导出所有类型相同的项都相等的结论。

**解释**：

- 如果 $u \to^* w$，则根据基本等式和等式的公理，可以推导出 $u = w$。
- 反之，如果可以从基本等式和等式公理推导出 $u = v$，则可以构造转换序列，使得 $u$ 和 $v$ 都归约到某个项 $w$。

- 如果存在两个不同的正常形式 $u$ 和 $v$（例如，两个不同的变量），那么就不存在这样的 $w$，因此不能证明 $u = v$。

---

### **4.2 弱规范化定理**

#### **陈述**

**弱规范化定理**声明了**正常形式的存在性**（它必然是唯一的），对于每个项，都存在一个归约序列将其归约到正常形式。

#### **推论**

- **指称相等性的可判定性**：我们可以通过计算项的正常形式并比较它们来决定两项是否相等。

**步骤**：

1. **计算正常形式**：对于项 $u$ 和 $v$，计算它们的正常形式 $u'$ 和 $v'$。
2. **比较**：如果 $u'$ 和 $v'$ 相等，则 $u$ 和 $v$ 指称相等；否则，它们不相等。

#### **注意事项**

- **归约的非确定性**：计算正常形式的归约过程不是确定性的。对于给定的项 $t$，在其子项上可能有多种归约路径（但数量有限）。
- **弱规范化的含义**：定理保证了通过适当的归约可以找到正常形式，但不排除存在无法达到正常形式的“坏的”归约路径。

#### **解决方法**

- **穷举归约**：可以通过枚举所有可能的归约序列，直到找到正常形式。尽管这种方法不够优雅，但由于从固定的项 $t$ 出发，只有有限多个长度为 $n$ 的归约序列，所以这种方法是可行的。

#### **强规范化定理的优势**

- **简化归约过程**：强规范化定理保证了**所有的规范化策略**都是好的，都会在有限步内达到正常形式。
- **效率问题**：尽管某些策略比其他策略更有效率（在步数上），但强规范化定理忽略了这一点，关注的是最终能否到达正常形式。

---

### **4.3 弱规范化定理的证明**

为了证明弱规范化定理，作者引入了一些概念和引理。

#### **4.3.1 类型的度量**

定义**类型的度数（degree）** $\partial(T)$：

1. **原子类型**：如果 $T_i$ 是原子类型，则 $\partial(T_i) = 1$。
2. **复合类型**：
   - 对于笛卡尔积类型 $U \times V$，$\partial(U \times V) = \max(\partial(U), \partial(V)) + 1$。
   - 对于函数类型 $U \to V$，$\partial(U \to V) = \max(\partial(U), \partial(V)) + 1$。

#### **4.3.2 约简式的度量**

定义**约简式（Redex）**的度数 $\partial(r)$：

1. 对于投影约简式 $\pi_1 \langle u, v \rangle$ 和 $\pi_2 \langle u, v \rangle$，$\partial(r) = \partial(U \times V)$，其中 $U \times V$ 是 $\langle u, v \rangle$ 的类型。
2. 对于应用约简式 $(\lambda x . v) \ u$，$\partial(r) = \partial(U \to V)$，其中 $U \to V$ 是 $(\lambda x . v)$ 的类型。

#### **4.3.3 项的度量**

定义**项** $t$ 的度数 $d(t)$ 为其包含的所有约简式的度数的**上确界**（supremum）。如果 $t$ 是正常项（不包含约简式），则 $d(t) = 0$。

**注意**：约简式 $r$ 有两个度数：

- 作为约简式的度数 $\partial(r)$。
- 作为项的度数 $d(r)$，因为 $r$ 可能包含其他约简式。

通常，$d(r) \geq \partial(r)$。

---

#### **引理：度数与替换的关系**

**引理 1**：如果 $x$ 是类型为 $U$ 的变量，则 $d(t[u / x]) \leq \max(d(t), d(u), \partial(U))$。

**证明思路**：

- 在 $t[u / x]$ 中，可能出现以下情况：
  1. **来自 $t$ 的约简式**：这些约简式中的 $x$ 被替换为 $u$，但度数不变。
  2. **来自 $u$ 的约简式**：由于 $u$ 被替换到 $t$ 中，$u$ 中的约简式可能被复制多次（取决于 $x$ 在 $t$ 中出现的次数）。
  3. **新的约简式**：如果 $x$ 出现在 $t$ 中的某些上下文中，如 $\pi_1 x$、$\pi_2 x$ 或 $x \ v$，并且 $u$ 是 $\langle u', u'' \rangle$ 或 $\lambda y . u'$，则可能产生新的约简式。这些约简式的度数为 $\partial(U)$。

---

#### **引理：度数与转换的关系**

**注意**：如果约简式 $r$ 的类型为 $T$，则 $\partial(r) > \partial(T)$。

**引理 2**：如果 $t \to u$，则 $d(u) \leq d(t)$。

**证明思路**：

- 考虑单步转换的情况，即 $u$ 是通过将 $t$ 中的一个约简式 $r$ 替换为其约简结果 $c$ 得到的。
- 在 $u$ 中可能出现的约简式：
  1. **$t$ 中除 $r$ 之外的约简式**：这些约简式可能因 $r$ 的替换而改变，但度数不变。
  2. **$c$ 中的约简式**：$c$ 是 $r$ 的简化或内部替换的结果，根据引理1，其度数不超过 $\max(d(s), d(s'), \partial(T))$，其中 $T$ 是 $x$ 的类型，并且 $\partial(T) < d(r)$。
  3. **由于替换产生的新约简式**：这些约简式的度数为 $\partial(T) < \partial(r)$。

---

#### **引理：最大度数的约简式的转换**

**引理 3**：设 $r$ 是项 $t$ 中度数为 $n$ 的约简式，且 $r$ 中严格包含的所有约简式的度数都小于 $n$。如果通过将 $r$ 转换为 $c$ 得到 $u$，那么 $u$ 中度数为 $n$ 的约简式的数量严格减少。

**证明思路**：

- 转换后发生的情况：
  1. **$r$ 之外的约简式**：保持不变。
  2. **$r$ 内部的约简式**：通常被保留，但可能被复制（如在替换中）。但由于假设，这些约简式的度数小于 $n$。
  3. **$r$ 本身**：被消除，可能被度数严格小于 $n$ 的约简式替换。

---

#### **定理的证明**

- 定义项 $t$ 的指标 $\mu(t) = (n, m)$，其中：
  - $n = d(t)$：项 $t$ 的最大度数。
  - $m$：项 $t$ 中度数为 $n$ 的约简式的数量。
- 根据引理3，可以选择一个约简式 $r$，使得转换后的项 $t'$ 满足 $\mu(t') < \mu(t)$（按字典序比较）。
- 由于 $\mu(t)$ 在每一步都严格减少，并且 $\mu(t)$ 的可能值有限（因为度数和约简式数量有限），归约过程必将在有限步内终止。

---

### **4.4 强规范化定理**

#### **讨论**

- **弱规范化定理的局限性**：尽管弱规范化定理保证了正常形式的存在，但它并未说明所有的归约策略都会终止。
- **强规范化的定义**：一个项 $t$ 是**强规范化的**，当且仅当**不存在**从 $t$ 开始的**无限归约序列**。

#### **引理**

**引理 4**：项 $t$ 是强规范化的，当且仅当存在一个数 $\nu(t)$，它是从 $t$ 开始的所有归约序列的长度的上界。

**证明思路**：

- **充分性**：如果存在这样的 $\nu(t)$，那么显然 $t$ 是强规范化的，因为所有归约序列都有限。
- **必要性**：假设 $t$ 是强规范化的，即不存在无限归约序列。
  - 利用**König 引理**：在一棵有限度的树上，如果每个节点都有有限的子节点，并且不存在无限路径，那么整棵树是有限的。
  - 将所有可能的归约序列组织成一棵树，节点代表项，边代表归约步骤。
  - 由于每个项都有有限个子项，树是有限度的。
  - 由于不存在无限归约序列，树中没有无限路径，因此树是有限的。
  - 因此，存在一个 $\nu(t)$，是从 $t$ 开始的归约序列的最大长度。

---

#### **证明方法**

- **内部化方法**：通过将演算系统转换到自身，以证明强规范化。这种方法较为复杂，最早由 Gandy 使用。
- **可归约性方法（Reducibility Method）**：引入“**遗传可计算性（Hereditary Computability）**”的概念，允许我们处理复杂的组合信息。这是作者选择的方法，因为它可以推广到非常复杂的情况。

---

### **总结**

本章深入探讨了**规范化定理**，这是确保类型化 λ 演算在计算上表现良好的关键结果。

- **Church-Rosser 性质**：保证了正常形式的唯一性，尽管其证明复杂，但它是理解演算系统一致性的关键。
- **弱规范化定理**：证明了每个项的正常形式的存在性，提供了决定项之间指称相等性的方法。
- **强规范化定理**：进一步确保了所有的归约策略都会在有限步内终止，从而简化了归约过程。

作者通过引入**度数**的概念，以及一系列引理，证明了弱规范化定理。强规范化定理将在第6章中通过**可归约性方法**进行证明。

---

### **后续展望**

- **第6章**将详细讨论强规范化定理的证明，介绍可归约性方法的细节。
- **应用**：规范化定理在类型理论和证明论中有重要应用，确保了逻辑系统的良好行为和一致性。
- **深入理解**：对规范化定理的深入理解有助于研究更复杂的类型系统和逻辑系统，如涉及依赖类型和高阶逻辑的系统。

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