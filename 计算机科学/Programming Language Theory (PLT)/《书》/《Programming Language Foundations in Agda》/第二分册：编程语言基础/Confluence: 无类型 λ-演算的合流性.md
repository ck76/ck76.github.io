[toc]

### 简介

**概念**: 本章的核心是证明无类型 $\lambda$-演算中的 $\beta$-归约是合流的（Confluent），也即满足 Church-Rosser 性质。简单来说，如果一个项 $L$ 可以通过归约序列分别归约到两个不同的项 $M_1$ 和 $M_2$，那么一定存在一个项 $N$，使得 $M_1$ 和 $M_2$ 都可以归约到 $N$。

**解释**:
- **合流性**（Confluence）: 合流性表示在某种归约关系下，无论从某个起始项如何归约，都可以找到一个共同的项作为归约的最终结果。
- **Church-Rosser 性质**: 这一性质是合流性的一个更具体的形式，它表示，如果一个项可以通过不同的路径归约到两个不同的结果，那么这两个结果可以再归约到同一个项。

**公式**:
- 图示说明合流性：
  $$
  \begin{array}{c}
      L \\
     / \backslash \\
    M_1 \, M_2 \\
     \backslash / \\
        N
  \end{array}
  $$

- **菱形性质**（Diamond Property）: 如果在某个归约系统中，每个项 $L$ 都满足对于 $L \rightarrow M_1$ 和 $L \rightarrow M_2$ 存在某个项 $N$，使得 $M_1 \rightarrow N$ 和 $M_2 \rightarrow N$，那么这个系统满足菱形性质。

  $$
  \begin{array}{c}
      L \\
     / \backslash \\
    M_1 \, M_2 \\
     \backslash / \\
        N
  \end{array}
  $$

  如果某个归约关系满足菱形性质的自反传递闭包，它就具有合流性。然而，$\lambda$-演算的 $\beta$-归约并不满足菱形性质，例子如下：

  - $(\lambda x. x x)((\lambda x. x) a) \rightarrow (\lambda x. x x) a$
  - $(\lambda x. x x)((\lambda x. x) a) \rightarrow ((\lambda x. x) a) ((\lambda x. x) a)$

### 导入

导入了一些必要的模块和定义，主要包括 Propositional Equality、Function 以及从前一章节中继承的一些定义和归约规则。

### 平行归约

**概念**: 平行归约是一种可以同时执行多个归约的归约关系。它的设计使得 $\lambda$-演算中的归约满足菱形性质，从而可以更容易地证明合流性。

**解释**:
- **平行归约**: 通过定义一种新的归约关系，我们可以同时对项的不同部分进行归约。这种归约方式比普通的 $\beta$-归约更强大，但它满足菱形性质，从而可以简化合流性的证明。
- **规则**:
  - **pvar**: 对变量进行平行归约时，变量保持不变。
  - **pabs**: 对 $\lambda$-抽象进行平行归约时，对抽象体进行平行归约。
  - **papp**: 对应用项进行平行归约时，同时对函数和参数进行平行归约。
  - **pbeta**: 对 $\lambda$-项应用某个项时，进行平行 $\beta$-归约。

**公式**:
- 平行归约的定义：
  $$
  \text{data} \, \Gamma \vdash A \rightarrow \Gamma \vdash A \rightarrow \text{Set} \, \text{where}
  $$
  $$
  \text{pvar} : (\Gamma \vdash A) \rightarrow (\Gamma \vdash A) \rightarrow \text{Set}
  $$
  $$
  \text{pabs} : (\Gamma, ★ \vdash ★) \rightarrow (\Gamma, ★ \vdash ★) \rightarrow \text{Set}
  $$
  $$
  \text{papp} : (\Gamma \vdash ★) \rightarrow (\Gamma \vdash ★) \rightarrow (\Gamma \vdash ★) \rightarrow (\Gamma \vdash ★) \rightarrow \text{Set}
  $$
  $$
  \text{pbeta} : (\Gamma, ★ \vdash ★) \rightarrow (\Gamma, ★ \vdash ★) \rightarrow (\Gamma \vdash ★) \rightarrow (\Gamma \vdash ★) \rightarrow \text{Set}
  $$

  这些规则表明如何将多个子项的归约过程组合成一个整体的归约。

### 平行归约与归约间等价性

**概念**: 本节证明了平行归约和普通 $\beta$-归约之间的等价性，即对于任意的 $M$ 和 $N$，$M \rightarrow N$ 当且仅当 $M \Rightarrow N$。

**解释**:
- 我们首先证明如果 $M$ 可以通过 $\beta$-归约得到 $N$，那么一定存在一个平行归约 $M \Rightarrow N$。这个证明通过对 $\beta$-归约规则进行归纳完成。
- 然后，我们证明如果 $M$ 可以通过平行归约得到 $N$，那么一定存在一个 $\beta$-归约 $M \rightarrow N$。这一部分较为复杂，因为平行归约可能同时包含多个 $\beta$-归约。

**公式**:
- 从 $\beta$-归约到平行归约：
  $$
  \text{beta-par} : \forall\{\Gamma A\}\{M N : \Gamma \vdash A\} \rightarrow M —→ N \rightarrow M ⇛ N
  $$
  - 对 $\beta$-归约进行归纳来证明。
- 从平行归约到 $\beta$-归约：
  $$
  \text{par-betas} : \forall\{\Gamma A\}\{M N : \Gamma \vdash A\} \rightarrow M ⇛ N \rightarrow M —↠ N
  $$

### 平行归约的替换引理

**概念**: 为了证明平行归约的菱形性质，我们需要证明替换操作是遵从平行归约的。

**解释**:
- 替换操作指的是将一个项代入到另一个项中的过程。如果我们在平行归约过程中对一个项进行了替换，那么结果应该保持平行归约的关系。这一部分的证明是通过逐点平行归约和扩展的替换引理完成的。

**公式**:
- 替换遵从平行归约的引理：
  $$
  \text{subst-par} : \forall\{\Gamma Δ A\} \{σ τ : \text{Subst} \Gamma Δ\} \{M M′ : \Gamma \vdash A\} \rightarrow \text{par-subst} σ τ \rightarrow M ⇛ M′ \rightarrow \text{subst} σ M ⇛ \text{subst} τ M′
  $$
  - 通过对替换操作和归约规则进行归纳来证明。

### 平行归约满足菱形性质

**概念**: 平行归约满足菱形性质，即如果一个项可以通过两个不同的平行归约序列得到两个不同的项，那么这两个项可以归约到同一个项。

**解释**:
- 我们通过定义一个三角性质（Triangle Property）来证明平行归约满足菱形性质。该性质表示如果 $M ⇛ N$，那么 $N ⇛ M⁺$，其中 $M⁺$ 是一个特殊的项，用于连接 $N$ 和 $N'$。

**公式**:
- 三角性质的定义和证明：
  $$
  \text{par-triangle} : \forall\{\Gamma A\} \{M N : \Gamma \vdash A\} \rightarrow M ⇛ N \rightarrow N ⇛ M⁺
  $$

- 菱形性质的证明：
  $$
  \text{par-diamond} : \forall\{\Gamma A\} \{M N N′ : \Gamma \vdash A\} \rightarrow M ⇛ N \rightarrow M ⇛ N′ \rightarrow Σ[L ∈ Γ \vdash A](N ⇛ L) × (N′ ⇛ L)
  $$

### 平行归约合流性的证明

**概念**: 通过证明带状引理（Strip Lemma），我们最终证明了平行归约的合流性。

**解释**:
- 带状引理的核心思想是，如果一个项 $M$ 通过平行归约得到 $N$，并且 $M$ 可以通过另一个平行归约序列得到 $N'$，那么一定存在一个项 $L$，使得 $N$ 和 $N'$ 都可以归约到 $L$。

**

公式**:
- 带状引理的定义和证明：
  $$
  \text{strip} : \forall\{\Gamma A\} \{M N N′ : \Gamma \vdash A\} \rightarrow M ⇛ N \rightarrow M ⇛* N′ \rightarrow Σ[L ∈ Γ \vdash A](N ⇛* L) × (N′ ⇛ L)
  $$
  
- 平行归约的合流性证明：
  $$
  \text{par-confluence} : \forall\{\Gamma A\} \{L M₁ M₂ : \Gamma \vdash A\} \rightarrow L ⇛* M₁ \rightarrow L ⇛* M₂ \rightarrow Σ[N ∈ Γ \vdash A](M₁ ⇛* N) × (M₂ ⇛* N)
  $$

### 归约合流性的证明

**概念**: 我们最终证明了 $\beta$-归约的合流性，这也是本章的主要目标。

**解释**:
- 利用平行归约的合流性，我们可以推导出 $\beta$-归约的合流性。通过证明任意两个 $\beta$-归约序列都可以转换为平行归约序列，我们可以证明这两个归约序列的最终结果是可合流的。

**公式**:
- 归约的合流性证明：
  $$
  \text{confluence} : \forall\{\Gamma A\} \{L M₁ M₂ : \Gamma \vdash A\} \rightarrow L —↠ M₁ \rightarrow L —↠ M₂ \rightarrow Σ[N ∈ Γ \vdash A](M₁ —↠ N) × (M₂ —↠ N)
  $$

### Unicode

**概念**: 列出了本章中使用的 Unicode 符号。

**解释**:
- 主要符号包括：
  - **⇛**: 表示平行归约关系
  - **⁺**: 表示某项的扩展

---

本章内容较为复杂，涵盖了平行归约的定义、菱形性质的证明、以及最终的 $\beta$-归约合流性证明。每一步都紧密联系，逐步构建了完整的合流性证明。