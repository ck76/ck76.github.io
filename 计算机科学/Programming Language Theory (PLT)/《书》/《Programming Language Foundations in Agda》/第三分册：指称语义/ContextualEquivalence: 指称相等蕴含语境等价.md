[toc]

### ContextualEquivalence: 指称相等蕴含语境等价

这一章节讨论了指称语义中的指称相等如何蕴含语境等价。这是 λ-演算中证明程序等价的重要工具，特别是在需要证明两个程序在所有上下文中都表现相同的时候。

#### 导入模块

在这一节中，我们导入了与证明指称语义、组合性、充分性和可靠性相关的模块。这些模块提供了定义和引理，使我们能够证明指称等价蕴含语境等价。

```haskell
open import Data.Product using (_×_; Σ; Σ-syntax; ∃; ∃-syntax; proj₁; proj₂)
     renaming (_,_ to ⟨_,_⟩)
open import plfa.part2.Untyped using (_⊢_; ★; ∅; _,_; ƛ_; _—↠_)
open import plfa.part2.BigStep using (_⊢_⇓_; cbn→reduce)
open import plfa.part3.Denotational using (ℰ; _≃_; ≃-sym; ≃-trans; _iff_)
open import plfa.part3.Compositional using (Ctx; plug; compositionality)
open import plfa.part3.Soundness using (soundness)
open import plfa.part3.Adequacy using (↓→⇓)
```

这些导入为我们提供了必要的工具和定义，使得我们能够进行进一步的证明和讨论。

#### 语境等价

**语境等价** (Contextual Equivalence) 是编程语言中的一个重要概念，定义为在任意语境 (Context) 中插入两个程序片段，如果这两个程序的行为总是相同，那么这两个程序片段就是语境等价的。语境等价在 λ-演算中尤其重要，因为它能够确保替换程序片段不会改变整个程序的行为。

我们首先定义一个 `terminates` 函数，用于描述某个程序是否能停机。该函数使用了多步归约的语义：

```haskell
terminates : ∀{Γ} → (M : Γ ⊢ ★) → Set
terminates {Γ} M = Σ[ N ∈ (Γ , ★ ⊢ ★) ] (M —↠ ƛ N)
```

这个定义表示，一个程序 $M$ 在语境 $\Gamma$ 中终止，当且仅当它可以多步归约到某个 λ-抽象 $N$。

接下来定义语境等价 (Contextual Equivalence) 的形式化表示：

```haskell
_≅_ : ∀{Γ} → (M N : Γ ⊢ ★) → Set
(_≅_ {Γ} M N) = ∀ {C : Ctx Γ ∅}
                → (terminates (plug C M)) iff (terminates (plug C N))
```

这里定义了两个项 $M$ 和 $N$ 是语境等价的，当且仅当对于任意语境 $C$，$M$ 插入 $C$ 后和 $N$ 插入 $C$ 后的程序要么都终止，要么都不终止。

#### 指称等价蕴含语境等价

我们接下来证明了**指称等价** (Denotational Equivalence) 如何蕴含**语境等价**。这意味着，如果两个项 $M$ 和 $N$ 在指称语义下是等价的，那么它们在任意语境中也是等价的。

##### 引理：指称等价的项在相同语境中停机性一致

我们首先定义一个引理，证明如果两个项 $M$ 和 $N$ 的指称语义相等，那么对于任意语境 $C$，如果 $M$ 插入 $C$ 后的程序能停机，那么 $N$ 插入 $C$ 后的程序也能停机。

```haskell
denot-equal-terminates : ∀{Γ} {M N : Γ ⊢ ★} {C : Ctx Γ ∅}
  → ℰ M ≃ ℰ N  →  terminates (plug C M)
    -----------------------------------
  → terminates (plug C N)
```

证明的思路是通过组合性定理 (compositionality)、可靠性定理 (soundness) 以及充分性定理 (adequacy) 的应用，逐步推导出 $N$ 插入 $C$ 后的程序也能停机。

具体来说，如果 $M$ 的插入 $C$ 后的程序能归约到一个 λ-抽象，那么我们通过可靠性定理可以推导出 $M$ 的指称等价于这个 λ-抽象的指称。再利用组合性定理，我们得到 $N$ 的插入 $C$ 后的程序的指称等价于这个 λ-抽象的指称。最后，应用充分性定理，可以推导出 $N$ 插入 $C$ 后的程序可以归约到某个 λ-抽象，从而说明它能停机。

##### 定理：指称等价蕴含语境等价

通过应用上面的引理，我们证明了**指称等价蕴含语境等价**的主定理：

```haskell
denot-equal-contex-equal : ∀{Γ} {M N : Γ ⊢ ★}
  → ℰ M ≃ ℰ N
    ---------
  → M ≅ N
```

证明思路是通过两次应用前面的引理 `denot-equal-terminates` 来说明，如果 $M$ 和 $N$ 在指称语义上相等，那么它们在任意语境中表现也相同。

#### Unicode

这一章节使用了一些特殊的 Unicode 符号，以便在数学公式和代码中表示各种概念：

- ≅  U+2245  APPROXIMATELY EQUAL TO (\~= or \cong) 表示语境等价。