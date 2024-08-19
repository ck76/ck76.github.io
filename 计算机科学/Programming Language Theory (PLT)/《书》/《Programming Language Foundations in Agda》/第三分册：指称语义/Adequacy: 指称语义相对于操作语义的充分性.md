[toc]

### Adequacy: 指称语义相对于操作语义的充分性

#### 引言

在上一章中，我们证明了指称语义的保型性，这意味着如果一个项 $M$ 能通过某种归约规则被归约为一个结果，那么它的指称也保持不变。在本章中，我们将进一步探讨指称语义的充分性，即在语义上如果一个项 $M$ 的指称值与某个 $\lambda$-抽象的指称值等价，那么这个项 $M$ 也能够被归约为该 $\lambda$-抽象。这一性质可以表达为：

$$
\text{若 } \gamma \vdash M \Downarrow v \text{，则存在某个 } M \text{，要么 } M \text{ 是一个} \lambda\text{-抽象，要么 } M \rightarrow N \text{。}
$$

这种充分性意味着，若 $M$ 的指称等价于一个 $\lambda$-抽象，那么 $M$ 必然可以通过归约步骤转化为一个 $\lambda$-抽象。

#### 导入

为了开始我们的证明，我们需要引入相关的模块和定义：

- **Relation.Binary.PropositionalEquality**: 用于处理相等关系。
- **Data.Product**: 用于处理乘积类型。
- **plfa.part2.Untyped** 和 **plfa.part2.Substitution**: 提供了基本的非类型化 $\lambda$-演算操作和代换规则。
- **plfa.part2.BigStep**: 定义了大步求值的规则。
- **plfa.part3.Denotational**: 提供了关于值和环境的指称语义。

在这些导入之后，我们开始定义与证明指称语义充分性相关的性质。

#### 大于或等于某个函数的性质

首先，我们定义一个函数 `above-fun`，用于表示一个值 $u$ 是否大于或等于某个函数值。我们用：

$$
\text{above-fun } u = \Sigma[ v \in \text{Value} ] \Sigma[ w \in \text{Value} ] v \Rightarrow w \sqsubseteq u
$$

其中，$v \Rightarrow w$ 表示从 $v$ 到 $w$ 的函数值，$u \sqsubseteq u'$ 表示 $u$ 小于或等于 $u'$。

接下来，我们定义几个关于 `above-fun` 的性质：

- **上界传递性**：如果 $u$ 大于某个函数值，而 $u'$ 大于或等于 $u$，那么 $u'$ 也大于该函数值。这一性质可以形式化为：

  $$
  \text{above-fun-}\sqsubseteq : \forall{u u' : \text{Value}} \rightarrow \text{above-fun } u \rightarrow u \sqsubseteq u' \rightarrow \text{above-fun } u'
  $$

- **底值不大于任何函数**：我们证明了底值（$\bot$）不大于任何函数值。这一点很直观，因为底值表示计算失败或未定义，不可能大于任何实际的函数。

  $$\text{above-fun}\bot : \neg \text{above-fun } \bot$$

- **连接值的性质**：我们还证明了如果两个值 $u$ 和 $u'$ 的连接 $u \sqcup u'$ 大于某个函数值，那么至少有一个值大于该函数值。反之，如果两个值都不大于某个函数值，那么它们的连接也不大于该函数值。

  $$
  \text{above-fun-}\sqcup : \forall{u u'} \rightarrow \text{above-fun } (u \sqcup u') \rightarrow \text{above-fun } u \sqcup \text{above-fun } u'
  $$

- **判定性**：最后，我们证明了 `above-fun` 的判定性，即我们可以判定一个值是否大于某个函数值：

  $$\text{above-fun? } : (\text{Value} \rightarrow \text{Dec } (\text{above-fun } v))$$

通过这些性质的证明，我们为后续逻辑关系的定义和推导打下了基础。

#### 将值关联到闭包

在这部分中，我们将语义值关联到闭包。定义两个关系：$𝕍$ 和 $𝔼$，分别用于处理闭包处于弱头范式（WHNF）时的值关联，以及处理任意闭包的值关联。

**主要定义**：

- **$𝕍$：** 用于关联语义值和处于弱头范式的闭包。具体来说，$𝕍$ 对于处于 WHNF 的项成立，并且如果 $v$ 是某个函数值，那么 $c$ 的主体可以通过 $v$ 来求值。
  
  $$
  𝕍 : \text{Value} \rightarrow \text{Clos} \rightarrow \text{Set}
  $$
  
  例如：
  
  - 如果 $v = \bot$，那么 $𝕍 v (clos(\lambda N) \gamma)$ 总是成立。
  - 如果 $v = u \sqcup v$，那么 $𝕍 v (clos(\lambda N) \gamma)$ 当且仅当 $𝕍 u (clos(\lambda N) \gamma)$ 和 $𝕍 v (clos(\lambda N) \gamma)$ 成立。
  - 如果 $v = v \Rightarrow w$，那么若 $c$ 是 $𝕍 v$ 的闭包，且 $w$ 大于某个函数值，则 $𝕍 w$ 对应的闭包成立。

- **$𝔼$：** 用于关联任意闭包与值。$𝔼 v (clos M \gamma')$ 在 $M$ 的结果大于某个函数值时成立：

  $$
  𝔼 v (clos M \gamma') = \text{above-fun } v \rightarrow \Sigma[ c \in \text{Clos} ] \gamma' \vdash M \Downarrow c \times 𝕍 v c
  $$

通过这些定义，我们可以将语义值与具体的 $\lambda$-抽象和环境中的闭包相关联。这为后续证明语义值的性质和操作语义的推导提供了工具。

---

到目前为止，我们通过定义与值相关的性质，建立了与闭包相关的逻辑关系。接下来，我们将深入讨论这些关系在具体操作中的应用。如果你需要进一步的解释或者对某一部分有疑问，请告诉我！

#### 拥有函数指称的程序通过传名调用可停机

在这一节中，我们的目标是证明一个主引理：如果一个项 $M$ 的指称值 $v$ 大于某个函数值，并且环境 $\gamma$ 与闭包环境 $\gamma'$ 通过 $𝔾$ 关系关联，则项 $M$ 通过传名调用（call-by-name）时一定可以停机，即存在一个闭包 $c$ 使得 $𝔼 v (clos M \gamma')$ 成立。

##### 证明的结构

我们通过对 $γ \vdash M \Downarrow v$ 的推导过程进行归纳来证明。这个推导过程的每一步，都需要证明在给定环境 $γ$ 和 $γ'$ 下，项 $M$ 的值 $v$ 与某个闭包 $c$ 通过 $𝔼$ 关系关联。

具体来说，对于 $γ \vdash M \Downarrow v$ 的每种情况，我们需要证明：
$$
𝔾 \,γ \,γ' \rightarrow γ \vdash M \Downarrow v \rightarrow 𝔼 \,v \,(clos \,M \,γ')
$$

##### 各种情况的证明

1. **变量** $M = x$：

   对于变量的情况，我们查找环境 $γ'$ 中的 $x$，并得到某个闭包 $clos M' \delta$。根据 $𝔾 \,γ \,γ'$ 的定义，$γ' x$ 与该闭包通过 $𝔼$ 关系关联。此外，由于 $γ \vdash x \Downarrow v$，因此 $v$ 必须等于 $γ x$，我们可以通过引理 `kth-x` 获得这些事实。最后，根据 $𝔼$ 的定义，我们得出结论：$𝔼 v (clos M' \delta)$。

2. **应用** $M = L · N$：

   对于应用的情况，首先根据归纳假设我们有 $𝔼 v₁ (clos L' \delta)$ 和 $𝔼 v (clos N \gamma')$，其中 $v₁$ 是一个函数值 $v₁ \Rightarrow v$。根据 $𝔼$ 的定义，$L$ 和 $N$ 必须分别求值为某些闭包。然后，根据 $𝕍$ 的定义，我们可以推导出 $𝕍 v (clos N \delta)$。最终，我们得出 $𝔼 v (clos (L · N) \gamma')$。

3. **$\lambda$-抽象** $M = \lambda N$：

   对于 $\lambda$-抽象的情况，我们有 $v = v' \Rightarrow w$。根据归纳假设，我们可以将环境扩展为 $(\gamma, v)$ 和 $(\gamma', c)$，从而证明在新的环境下 $𝔼$ 关系仍然成立。这使得我们能够推导出 $𝔼 v (clos (\lambda N) \gamma')$。

4. **底值** $M = \bot$：

   对于 $\bot$ 的情况，由于 $\bot$ 不可能大于任何函数值，因此在这种情况下，我们没有 $𝔼 v c$ 的实例。

5. **并集** $M = v₁ \sqcup v₂$：

   对于并集的情况，我们有 $γ \vdash M \Downarrow (v₁ \sqcup v₂)$，并且 $v₁ \sqcup v₂$ 必须大于某个函数值。我们使用 `above-fun-⊔` 引理，将 $v₁$ 和 $v₂$ 的情况分别处理，并通过 $𝕍⊔-intro` 的引入规则得到 $𝔼 (v₁ \sqcup v₂) c$。

6. **子类型** $v' \sqsubseteq v$ 的情况：

   对于子类型的情况，我们有 $γ \vdash M \Downarrow v$ 且 $v' \sqsubseteq v$。根据 $𝕍$ 和 $𝔼$ 的定义，我们可以通过 `sub-𝕍` 和 `sub-𝔼` 引理将 $𝕍 v$ 推广到 $𝕍 v'$，从而推导出 $𝔼 v' c$。

通过上述的逐步分析，我们得出 $𝔼$ 关系对所有情况都是成立的。这证明了主引理：如果一个项的指称值 $v$ 大于某个函数值，并且与环境关联，则该项在传名调用时必定能停机。

#### 指称充分性的证明

接下来，我们利用刚刚证明的主引理，来证明指称语义的充分性。这意味着，如果项 $M$ 的指称值 $ℰ M$ 等价于一个 $\lambda$-抽象的指称值 $ℰ (\lambda N)$，那么 $M$ 必须可以被归约为一个 $\lambda$-抽象。

为了证明这一点，我们使用以下推论：

$$
\downarrow \rightarrow \Downarrow : \forall M \, N \, →  ℰ M ≃ ℰ (\lambda N) →  Σ[ Γ ∈ Context ] Σ[ N' ∈ (Γ , ★ ⊢ ★) ] Σ[ γ ∈ ClosEnv Γ ] ∅' ⊢ M ⇓ clos (\lambda N') γ
$$

我们通过应用之前的主引理，可以得出 $M$ 可以大步归约为某个 $\lambda$-抽象的结论，最终我们证明了：

$$
\text{adequacy } : ∀{M : ∅ ⊢ ★}{N : ∅ , ★ ⊢ ★}  →  ℰ M ≃ ℰ (\lambda N) → Σ[ N' ∈ (∅ , ★ ⊢ ★) ] (M —↠ \lambda N')
$$

通过这一充分性证明，我们可以确保如果一个项的指称值是一个 $\lambda$-抽象，那么该项一定可以被操作语义归约为一个 $\lambda$-抽象。

#### 传名调用等价于 β-归约

最后，我们利用上述充分性的结论来证明传名调用（call-by-name）求值和 β-归约的等价性。我们已经证明了：

- 如果一个程序通过传名调用可以产生结果，那么它必定可以通过 β-归约归约为一个 $\lambda$-抽象。
- 反之，如果一个程序可以 β-归约为一个 $\lambda$-抽象，那么通过传名调用也可以产生结果。

这证明了传名调用求值和 β-归约之间的等价性，即：

$$
\text{cbn↔reduce } : \forall {M : ∅ ⊢ ★} → (Σ[ N ∈ ∅ , ★ ⊢ ★ ] (M —↠ \lambda N))  \iff  (Σ[ Δ ∈ Context ] Σ[ N' ∈ Δ , ★ ⊢ ★ ] Σ[ δ ∈ ClosEnv Δ ] ∅' ⊢ M ⇓ clos (\lambda N') δ)
$$

这个结论表明传名调用和 β-归约在求值结果上是完全一致的。

---

这一节完整地阐述了指称语义与操作语义之间的充分性关系。通过证明，如果项 $M$ 的指称等价于某个 $\lambda$-抽象的指称值，那么项 $M$ 必然可以通过 β-归约归约为某个 $\lambda$-抽象。这个证明在更广泛的编程语言语义研究中具有重要意义。