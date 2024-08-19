[toc]

### **引言**

本章的主要目的是证明代换在无类型 $\lambda$-演算中的交换性。在使用 de Bruijn 索引表示变量的情况下，这个代换引理可以表述为：

$$M [N] [L] \equiv M〔L〕[N[L]]$$

其中符号 $M〔L〕$ 表示用 $L$ 代换 $M$ 中的索引 1。此外，由于我们使用了平行代换的定义，代换的推广形式如下：

$$\text{subst } \sigma (M [N]) \equiv (\text{subst } (\text{exts } \sigma) M)[\text{subst } \sigma N]$$

这种推广形式更加强大，因为它使用了任意的平行代换 $\sigma$。

另外，重命名（即代换的特例）也有类似的形式：

$$\text{rename } \rho (M [N]) \equiv (\text{rename } (\text{ext} \rho) M)[\text{rename } \rho N]$$

本章的第二个目标是定义由 Abadi、Cardelli、Curien 和 Levy (1991) 提出的平行代换的 $\sigma$-代数。这种代数不仅有助于证明代换引理，还具有其他重要应用。代数方程的左到右应用形成了一个重写系统，可以用来判定任意两个代换是否相等。

### **导入**

在本节中，我们导入了所需的模块和函数以便进行证明和操作：

```haskell
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; sym; cong; cong₂; cong-app)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Function using (_∘_)
open import plfa.part2.Untyped
     using (Type; Context; _⊢_; ★; _∋_; ∅; _,_; Z; S_; `_; ƛ_; _·_;
            rename; subst; ext; exts; _[_]; subst-zero)
```

这些导入提供了等式推理、函数复合、以及未类型化的 $\lambda$-演算等工具。

### **记法**

在这一节中，我们引入了一些简写表示以简化后续的推导：

- **重命名 (Rename)**: 将语境 $\Gamma$ 中的变量重命名为语境 $\Delta$ 中的变量。
  
  定义为：
  $$\text{Rename } \Gamma \Delta = \forall \{A\} \rightarrow \Gamma \ni A \rightarrow \Delta \ni A$$

- **代换 (Subst)**: 将语境 $\Gamma$ 中的变量代换为语境 $\Delta$ 中的项。
  
  定义为：
  $$\text{Subst } \Gamma \Delta = \forall \{A\} \rightarrow \Gamma \ni A \rightarrow \Delta \vdash A$$

- **代换函数简写**: 使用 $\text{subst }$ 代换的函数可以简写为 $\langle \sigma \rangle$:
  
  定义为：
  $$\langle \sigma \rangle = \lambda M \rightarrow \text{subst } \sigma M$$

这些简写表示使得表达式更加简洁，并便于代数操作。

### **代换的 σ-代数**

在这一节中，我们定义了代换的 $\sigma$-代数。这种代数将 de Bruijn 索引映射到项，从而可以将代换视为项的有限序列。代数由以下四个运算组成：

1. **恒等 (ids)**: 构造序列 $0, 1, 2, \dots$
   
   $$\text{ids} : \forall \{\Gamma\} \rightarrow \text{Subst } \Gamma \Gamma$$
   $$\text{ids } x = ` x$$

2. **抬升 (↑)**: 构造序列 $1, 2, 3, \dots$
   
   $$\uparrow : \forall \{\Gamma A\} \rightarrow \text{Subst } \Gamma (\Gamma , A)$$
   $$\uparrow x = ` (\text{S } x)$$

3. **构造 (M • σ)**: 给定项 $M$ 和代换 $\sigma$，构造序列 $M, \sigma 0, \sigma 1, \sigma 2, \dots$
   
   $$M • \sigma : \forall \{\Gamma \Delta A\} \rightarrow (\Delta \vdash A) \rightarrow \text{Subst } \Gamma \Delta \rightarrow \text{Subst } (\Gamma , A) \Delta$$
   $$\text{(M • σ) Z} = M$$
   $$\text{(M • σ) (S x)} = \sigma x$$

4. **序列 (σ ⨟ τ)**: 通过先应用代换 $\sigma$，然后应用代换 $\tau$ 来组合两个代换，生成序列 $⟪τ⟫(σ 0), ⟪τ⟫(σ 1), ⟪τ⟫(σ 2), \dots$
   
   $$\sigma ⨟ τ : \forall \{\Gamma \Delta \Sigma\} \rightarrow \text{Subst } \Gamma \Delta \rightarrow \text{Subst } \Delta \Sigma \rightarrow \text{Subst } \Gamma \Sigma$$
   $$\sigma ⨟ τ = \langle \tau \rangle \circ \sigma$$

这些运算定义了代换的行为，并使得代数操作更加系统化。

### **σ-代数方程**

$\sigma$-代数包含以下方程，它们描述了 $\sigma$-代数中的各个运算如何相互作用：

1. **头部与尾部操作**：
   - **sub-head**: $⟪ M • σ ⟫(` Z) ≡ M$
   - **sub-tail**: $↑ ⨟ (M • σ) ≡ σ$
   - **sub-η**: $(⟪ σ ⟫(` Z)) • (↑ ⨟ σ) ≡ σ$
   - **Z-shift**: $(` Z) • ↑ ≡ \text{ids}$

2. **代换在项上的操作**：
   - **sub-id**: $⟪ \text{ids} ⟫ M ≡ M$
   - **sub-app**: $⟪ σ ⟫(L · M) ≡ (⟪ σ ⟫ L) · (⟪ σ ⟫ M)$
   - **sub-abs**: $⟪ σ ⟫(ƛ N) ≡ ƛ ⟪ σ ⟫ N$
   - **sub-sub**: $⟪ τ ⟫ ⟪ σ ⟫ M ≡ ⟪ σ ⨟ τ ⟫ M$

3. **序列操作的性质**：
   - **sub-idL**: $\text{ids} ⨟ σ ≡ σ$
   - **sub-idR**: $σ ⨟ \text{ids} ≡ σ$
   - **sub-assoc**: $(σ ⨟ τ) ⨟ θ ≡ σ ⨟ (τ ⨟ θ)$
   - **sub-dist**: $(M • σ) ⨟ τ ≡ (⟪ τ ⟫ M) • (σ ⨟ τ)$

这些方程提供了对 $\sigma$-代数的结构性理解，使得我们能够进行等式推导和变换，从而证明更复杂的性质。

我将继续为你解释接下来的部分。

### **关联 σ-代数和代换函数**

在本节中，我们将 $\sigma$-代数中的运算与 $\text{subst}$ 代换函数及其辅助函数 (如 $\text{rename}$, $\text{ext}$, $\text{exts}$, 和 $\text{subst-zero}$) 进行关联。

1. **重命名与代换的关系**：我们首先引入重命名与代换之间的关系：

   $$\text{rename } \rho \, M \equiv \langle \text{ren } \rho \rangle \, M$$

   其中，$\text{ren}$ 是将重命名函数 $\rho$ 转换为代换函数的操作：

   $$\text{ren} : \forall \{\Gamma \Delta\} \rightarrow \text{Rename } \Gamma \Delta \rightarrow \text{Subst } \Gamma \Delta$$
   $$\text{ren } \rho = \text{ids} \circ \rho$$

   当 $\rho$ 是递增函数时，这种重命名等价于抬升操作：

   $$\text{ren } S_ \equiv \uparrow$$

   因此，重命名 $\text{S}_$ 应用于 $M$ 等价于 $\langle \uparrow \rangle M$：

   $$\text{rename } S_ \, M \equiv \langle \uparrow \rangle M$$

   当重命名为恒等重命名时，项 $M$ 保持不变：

   $$\text{rename } (\lambda \{A\} \, x \rightarrow x) \, M \equiv M$$

2. **关联 $\text{exts}$ 函数**：$\text{exts}$ 函数扩展了代换的形式：

   $$\text{exts } \sigma = \` Z, \text{rename } S_ (\sigma 0), \text{rename } S_ (\sigma 1), \text{rename } S_ (\sigma 2), \dots$$

   这等价于将 $Z$ 作为头部，然后将 $\sigma$ 应用于序列后再抬升：

   $$\text{exts } \sigma \equiv \` Z \bullet (\sigma ⨟ \uparrow)$$

   $\text{ext}$ 函数的作用与 $\text{exts}$ 相似，只不过作用于重命名上。因此，将 $\text{ren}$ 复合到 $\text{ext}$ 上等价于将 $\text{exts}$ 复合到 $\text{ren}$ 上：

   $$\text{ren} (\text{ext } \rho) \equiv \text{exts} (\text{ren } \rho)$$

   这一关系可以转化为重命名的形式：

   $$\text{ren} (\text{ext } \rho) \equiv \` Z \bullet (\text{ren } \rho ⨟ \uparrow)$$

3. **特化的证明**：为了进一步证明 $\sigma$-代数的性质，我们可以将 $\text{sub-sub}$ 方程特化到第一个代换是重命名的情况：

   $$\langle \sigma \rangle (\text{rename } \rho \, M) \equiv \langle \sigma \circ \rho \rangle \, M$$

   这表明代换和重命名之间可以互相交换。

4. **$\text{subst-zero}$ 的关联**：$\text{subst-zero } M$ 等价于将 $M$ 作为头部，并结合恒等代换：

   $$\text{subst-zero } M \equiv M \bullet \text{ids}$$

   最后，将 $\text{subst-zero } M$ 添加到 $\text{exts } \sigma$ 之后，等价于将 $M$ 作为头部添加到 $\sigma$：

   $$\text{exts } \sigma ⨟ \text{subst-zero } M \equiv (M \bullet \sigma)$$

### **sub-head、sub-tail、sub-η、Z-shift、sub-idL、sub-dist 和 sub-app 的证明**

接下来，我们证明 $\sigma$-代数中的几个基本方程：

1. **sub-head**: 
   $$\langle M • σ \rangle (` Z) \equiv M$$
   证明：从定义出发直接推导可以得到这一结果。

2. **sub-tail**: 
   $$\uparrow ⨟ (M • σ) \equiv σ$$
   证明：通过函数的外延性可直接证明。

3. **sub-η**: 
   $$(\langle σ \rangle (` Z)) • (\uparrow ⨟ σ) \equiv σ$$
   证明：通过外延性推导，基于 $\sigma$ 的不同索引情况进行推理。

4. **Z-shift**: 
   $$(` Z) • \uparrow \equiv \text{ids}$$
   证明：同样通过外延性证明。

5. **sub-idL**: 
   $$\text{ids} ⨟ σ ≡ σ$$
   证明：直接推导得出。

6. **sub-dist**: 
   $$(M • σ) ⨟ τ \equiv (\langle τ \rangle M) • (σ ⨟ τ)$$
   证明：通过对代换进行外延性扩展，可以得到证明。

7. **sub-app**: 
   $$\langle σ \rangle (L · M) ≡ (\langle σ \rangle L) · (\langle σ \rangle M)$$
   证明：此性质可通过代换函数的线性性质直接推导出。

我将继续解释接下来的部分，涉及合同性和进一步的证明。

### **插曲：合同性**

在本节中，我们将为 $\sigma$-代数的运算 $\bullet$ 和 $⨟$ 以及 $\text{subst}$ 函数及其辅助函数（$\text{ext}$、$\text{rename}$、$\text{exts}$ 和 $\text{subst-zero}$）建立合同律。这些合同律在后续的等式推理中非常有用。

1. **合同性 (Congruence) 的定义**：
   - **合同扩展 (cong-ext)**: 对于任何两个重命名函数 $\rho$ 和 $\rho'$，如果它们在语境 $\Gamma$ 的所有元素上都相等，那么它们的扩展也是相等的：
   
     $$\text{cong-ext} : \forall \{\Gamma \Delta\} \{\rho \rho' : \text{Rename } \Gamma \Delta\} \{B : \text{Type}\}$$
     $$ \rightarrow (\forall \{A : \text{Type}\} \rightarrow \rho ≡ \rho') \rightarrow \forall \{A : \text{Type}\} \rightarrow \text{ext } \rho \{B = B\} ≡ \text{ext } \rho' \{A\}$$

     证明：通过函数的外延性，结合 $\text{ext}$ 的定义，可以证明这一合同性。

   - **合同重命名 (cong-rename)**: 如果两个重命名函数 $\rho$ 和 $\rho'$ 对语境中的所有变量都相等，那么对任何项 $M$ 进行重命名操作的结果也相等：
   
     $$\text{cong-rename} : \forall \{\Gamma \Delta\} \{\rho \rho' : \text{Rename } \Gamma \Delta\} \{B : \text{Type}\} \{M : \Gamma \vdash B\}$$
     $$\rightarrow (\forall \{A : \text{Type}\} \rightarrow \rho ≡ \rho') \rightarrow \text{rename } \rho \, M ≡ \text{rename } \rho' \, M$$

     证明：通过对项 $M$ 的归纳证明，包括对变量、$\lambda$-抽象和应用的情况。

   - **合同扩展 (cong-exts)**: 对于任何两个代换函数 $\sigma$ 和 $\sigma'$，如果它们在语境 $\Gamma$ 的所有元素上都相等，那么它们的扩展也是相等的：
   
     $$\text{cong-exts} : \forall \{\Gamma \Delta\} \{\sigma \sigma' : \text{Subst } \Gamma \Delta\} \{B : \text{Type}\}$$
     $$\rightarrow (\forall \{A : \text{Type}\} \rightarrow \sigma ≡ \sigma') \rightarrow \forall \{A : \text{Type}\} \rightarrow \text{exts } \sigma \{B = B\} ≡ \text{exts } \sigma' \{A\}$$

     证明：类似于 $\text{cong-ext}$ 的证明，通过函数外延性进行。

   - **合同代换 (cong-sub)**: 如果两个代换函数 $\sigma$ 和 $\sigma'$ 对语境中的所有变量都相等，并且两个项 $M$ 和 $M'$ 相等，那么代换 $\sigma$ 和 $\sigma'$ 应用于 $M$ 和 $M'$ 的结果也相等：
   
     $$\text{cong-sub} : \forall \{\Gamma \Delta\} \{\sigma \sigma' : \text{Subst } \Gamma \Delta\} \{A : \text{Type}\} \{M M' : \Gamma \vdash A\}$$
     $$\rightarrow (\forall \{A : \text{Type}\} \rightarrow \sigma ≡ \sigma') \rightarrow M ≡ M' \rightarrow \text{subst } \sigma \, M ≡ \text{subst } \sigma' \, M'$$

     证明：通过对项 $M$ 的归纳，并结合对 $\lambda$-抽象和应用的处理。

   - **合同 $\text{subst-zero}$ (cong-sub-zero)**: 对于任意两个相等的项 $M$ 和 $M'$，它们的 $\text{subst-zero}$ 结果也相等：
   
     $$\text{cong-sub-zero} : \forall \{\Gamma : \text{Context}\} \{B : \text{Type}\} \{M M' : \Gamma \vdash B\}$$
     $$\rightarrow M ≡ M' \rightarrow \forall \{A : \text{Type}\} \rightarrow \text{subst-zero } M ≡ \text{subst-zero } M' \{A\}$$

     证明：通过函数的外延性证明。

   - **合同构造 (cong-cons)**: 如果两个项 $M$ 和 $N$ 相等，并且两个代换函数 $\sigma$ 和 $\tau$ 在所有上下文中的作用相等，那么构造操作 $M \bullet \sigma$ 和 $N \bullet \tau$ 的结果也相等：
   
     $$\text{cong-cons} : \forall \{\Gamma \Delta : \text{Context}\} \{A : \text{Type}\} \{M N : \Delta \vdash A\} \{σ τ : \text{Subst } \Gamma \Delta\}$$
     $$\rightarrow M ≡ N \rightarrow (\forall \{A : \text{Type}\} \rightarrow σ \{A\} ≡ τ \{A\}) \rightarrow \forall \{A : \text{Type}\} \rightarrow (M \bullet σ) \{A\} ≡ (N \bullet τ) \{A\}$$

     证明：通过函数外延性以及对变量索引进行情况分析。

   - **合同序列 (cong-seq)**: 如果两个代换函数 $\sigma$ 和 $\sigma'$ 以及 $\tau$ 和 $\tau'$ 在所有上下文中的作用相等，那么它们的序列组合也相等：
   
     $$\text{cong-seq} : \forall \{\Gamma \Delta Σ : \text{Context}\} \{\sigma \sigma' : \text{Subst } \Gamma \Delta\} \{\τ τ' : \text{Subst } Δ Σ\}$$
     $$\rightarrow (\forall \{A : \text{Type}\} \rightarrow σ \{A\} ≡ σ' \{A\}) \rightarrow (\forall \{A : \text{Type}\} \rightarrow τ \{A\} ≡ τ' \{A\}) \rightarrow \forall \{A : \text{Type}\} \rightarrow (σ ⨟ τ) \{A\} ≡ (σ' ⨟ τ') \{A\}$$

     证明：通过对序列的定义和代换的合同性进行推导。

### **将 rename、exts、ext 和 subst-zero 关联到 σ-代数**

本节中，我们将 $\sigma$-代数中的项与 $\text{subst}$ 及其辅助函数（$\text{rename}$、$\text{exts}$、$\text{ext}$ 和 $\text{subst-zero}$）建立关联。

1. **重命名与代换的关联**: 我们首先证明 $ \text{rename } \rho \, M \equiv \langle \text{ren } \rho \rangle \, M $，即将 $\text{rename } \rho$ 应用于 $M$ 等价于将 $\text{ren } \rho$ 代换应用于 $M$。证明步骤如下：

   - **引理 ren-ext**: 证明 $\text{ren } (\text{ext } \rho) \equiv \text{exts } (\text{ren } \rho)$，即扩展操作对重命名和代换具有相同的效果。这是通过函数外延性和对变量索引的情况分析推导得出的。

   - **rename-subst-ren**: 证明 $ \text{rename } \rho \, M \equiv \langle \text{ren } \rho \rangle \, M $，通过对项 $M$ 的归纳，包括变量、$\lambda$-抽象和应用的情况。

2. **重命名与抬升的关系**: 我们证明 $\text{ren } S_ \equiv \uparrow$，即抬升操作等价于递增重命名。这个证明通过对索引变量的情况分析，以及外延性推导而得。

3. **扩展与序列组合的关联**: 我们证明 $ \text{exts } σ \equiv ` Z \bullet (σ ⨟ \uparrow)$，即扩展操作等价于在代换序列中添加头部并执行抬升。这通过对索引变量的情况分析，结合重命名与代换的关联性推导得出。

4. **sub-abs 方程的证明**:

 通过 $ \text{exts-cons-shift}$ 和 $\text{exts-ids}$ 引理，我们证明 $\langle σ \rangle (ƛ N) \equiv ƛ \langle ` Z \bullet (σ ⨟ \uparrow) \rangle N$。这表明 $\lambda$-抽象与代换操作可以互相交换，且代换序列中的头部和抬升操作保留。

5. **sub-id 方程的证明**: 证明 $\langle \text{ids} \rangle M \equiv M$，即恒等代换应用于任何项 $M$ 都保持不变。这通过扩展 $\text{exts-ids}$ 引理，以及对项 $M$ 进行归纳得出。

6. **sub-idR 的证明**: 证明 $σ ⨟ \text{ids} \equiv σ$，即恒等代换是序列组合的右单位元。这可以直接通过 sub-id 方程推导得出。

### **sub-sub 的证明**

在本节中，我们证明了 $\langle τ \rangle \langle σ \rangle M \equiv \langle σ ⨟ τ \rangle M$，即两个代换序列组合的结果与先应用其中一个再应用另一个的结果相等。

1. **引理 compose-rename**: 首先，我们证明了复合重命名函数与依次应用两个重命名的结果相等。这通过对 $\lambda$-抽象的情况分析和 $\text{compose-ext}$ 引理得出。

2. **引理 commute-subst-rename**: 其次，我们证明了代换与重命名可以互相交换。这是通过对项的情况分析，结合 $\text{exts}$ 和 $\text{ext}$ 的关联性进行推导的。

3. **引理 exts-seq**: 最后，我们证明了 $\text{exts}$ 操作对代换序列满足分配律。这是通过前面的 $\text{commute-subst-rename}$ 引理推导而来。

通过这些引理的组合，我们得出了 sub-sub 方程的证明，即 $ \langle τ \rangle \langle σ \rangle M \equiv \langle σ ⨟ τ \rangle M$。

---

接下来的内容将涉及到 **sub-assoc** 和 **subst-zero-exts-cons** 的证明，以及最终的 **代换引理** 的证明。让我知道你是否希望我继续解释这些内容。

### **sub-assoc 的证明**

在本节中，我们将证明 $\text{sub-assoc}$ 方程，它表明代换序列的组合满足结合律。即，对于任何三个代换 $\sigma$、$\tau$ 和 $\theta$，有：

$$
(\sigma ⨟ \tau) ⨟ \theta ≡ \sigma ⨟ (\tau ⨟ \theta)
$$

#### **证明思路**

1. **引理的陈述**:
   - **sub-assoc** 的证明通过对语境 $\Gamma$ 中的每个变量 $x$ 进行情况分析，证明其在代换序列 $(\sigma ⨟ \tau) ⨟ \theta$ 和 $\sigma ⨟ (\tau ⨟ \theta)$ 中的结果相等。

2. **情况分析**:
   - 对于语境 $\Gamma$ 中的每个变量 $x$：
     - **$x$ 是索引变量**：我们直接使用 $\text{sub-sub}$ 方程来处理变量的情况。
     - **$x$ 是 $\lambda$-抽象**：我们应用 $\text{sub-sub}$ 以及 $\text{exts}$ 操作的结合律来推导结果。
     - **$x$ 是应用**：我们对应用的两个子项分别应用 $\text{sub-sub}$ 方程，然后将结果组合。

3. **外延性推导**:
   - 使用函数的外延性将变量 $x$ 的情况推广到整个语境 $\Gamma$。

#### **sub-assoc 的证明**

$$
\text{sub-assoc} : \forall \{Γ Δ Σ Ψ : \text{Context}\} \{σ : \text{Subst } Γ Δ\} \{τ : \text{Subst } Δ Σ\} \{θ : \text{Subst } Σ Ψ\}
$$
$$
\forall \{A : \text{Type}\} → (σ ⨟ τ) ⨟ θ ≡ σ ⨟ (τ ⨟ θ) \{A\}
$$

证明是通过对每个变量 $x$ 在语境中的情况进行等式推导：

1. **步骤 1**: 对于索引变量 $x$：
   $$
   ((σ ⨟ τ) ⨟ θ) \, x ≡ \langle θ \rangle (\langle τ \rangle (\sigma \, x))
   $$
   然后，使用 $\text{sub-sub}$ 方程：
   $$
   \langle τ ⨟ θ \rangle (\sigma \, x) ≡ (σ ⨟ τ ⨟ θ) \, x
   $$
   这证明了两边相等。

2. **步骤 2**: 对于 $\lambda$-抽象项 $ƛ N$：
   $$
   \langle θ \rangle (\langle τ \rangle (\langle σ \rangle (ƛ N))) ≡ ƛ \langle \text{exts} (σ ⨟ τ) ⨟ \theta \rangle N
   $$
   通过使用 $\text{sub-sub}$ 和 $\text{exts-seq}$ 引理，我们得到：
   $$
   ƛ \langle σ ⨟ (τ ⨟ θ) \rangle N ≡ ƛ \langle (σ ⨟ τ) ⨟ θ \rangle N
   $$

3. **步骤 3**: 对于应用项 $L · M$，我们分别对 $L$ 和 $M$ 应用相同的推导步骤。

最终的结果通过外延性得到，从而完成 sub-assoc 的证明。

### **subst-zero-exts-cons 的证明**

在本节中，我们将证明 $\text{subst-zero-exts-cons}$ 方程，它表明将项 $M$ 与扩展代换 $\text{exts } \sigma$ 组合，再与 $\text{subst-zero}$ 的结果相当于将 $M$ 直接组合到代换 $\sigma$ 的序列中。这个方程的陈述如下：

$$
\text{subst-zero-exts-cons} : \forall \{Γ Δ : \text{Context}\} \{σ : \text{Subst } Γ Δ\} \{B : \text{Type}\} \{M : Δ ⊢ B\} \{A : \text{Type}\}
$$
$$
\rightarrow \text{exts } σ ⨟ \text{subst-zero } M ≡ M \bullet σ \{A\}
$$

#### **证明思路**

1. **方程展开**:
   - 我们首先将 $\text{exts } \sigma$ 和 $\text{subst-zero } M$ 代入方程的左边，并展开它们的定义。
   - 然后我们逐步将其化简为右边的形式，即 $M \bullet \sigma$。

2. **使用已证明的方程**:
   - 使用前面证明的 $\text{sub-assoc}$、$\text{sub-dist}$ 和 $\text{sub-idR}$ 等方程。

#### **证明步骤**

1. **步骤 1**: 展开 $\text{exts } \sigma$ 和 $\text{subst-zero } M$ 的定义：
   $$
   \text{exts } σ ⨟ \text{subst-zero } M ≡ (` Z \bullet (σ ⨟ \uparrow)) ⨟ (M \bullet \text{ids})
   $$
   
2. **步骤 2**: 使用 $\text{sub-dist}$ 和 $\text{sub-assoc}$ 方程：
   $$
   \langle \langle M \bullet \text{ids} \rangle (` Z) \bullet (\sigma ⨟ \uparrow ⨟ M \bullet \text{ids}) \rangle ≡ M \bullet (σ ⨟ \text{ids})
   $$

3. **步骤 3**: 使用 $\text{sub-idR}$ 方程，将 $\text{ids}$ 消去：
   $$
   M \bullet σ ≡ M \bullet σ
   $$
   这样我们得到了右边的形式，从而证明了 $\text{subst-zero-exts-cons}$ 方程。

### **代换引理的证明**

现在我们来证明代换引理 (Substitution Lemma)。这个引理陈述了代换之间的可交换性：

$$
(M [ N ]) [ L ] ≡ (M 〔 L 〕) [ (N [ L ]) ]
$$

#### **证明思路**

1. **代换引理的推广形式**:
   - 我们首先证明更一般的形式，即：
     $$
     \langle \text{exts } \sigma \rangle N [ \langle \sigma \rangle M ] ≡ \langle \sigma \rangle (N [ M ])
     $$
   - 这个推广形式表明了任意代换 $\sigma$ 对项 $M$ 和 $N$ 的作用是可以交换的。

2. **推导步骤**:
   - 首先从左边开始，逐步应用前面证明的方程，将其简化为右边的形式。
   - 最后，利用 $\text{subst-zero-exts-cons}$ 方程和 $\text{sub-dist}$ 方程将其化简为正规形式。

#### **证明**

$$
\text{subst-commute} : \forall \{Γ Δ : \text{Context}\} \{N : Γ , ★ ⊢ ★\} \{M : Γ ⊢ ★\} \{σ : \text{Subst } Γ Δ\}
$$
$$
\rightarrow \langle \text{exts } σ \rangle N [ \langle σ \rangle M ] ≡ \langle σ \rangle (N [ M ])
$$

证明通过对项 $M$ 和 $N$ 进行等式推导完成：

1. **步骤 1**: 展开左边的代换：
   $$
   \langle \text{subst-zero } (\langle σ \rangle M) \rangle (\langle \text{exts } σ \rangle N)
   $$
   
2. **步骤 2**: 使用 $\text{subst-zero-exts-cons}$ 和 $\text{sub-sub}$ 方程：
   $$
   \langle \langle σ ⨟ \text{subst-zero } M \rangle \rangle N
   $$
   
3. **步骤 3**: 化简得到：
   $$
   \langle M \bullet σ \rangle N
   $$
   
4. **步骤 4**: 将其化简为右边的形式：
   $$
   \langle σ \rangle (N [ M ])
   $$
   这样，代换引理的推广形式得到了证明。

#### **推论**

从 $\text{subst-commute}$ 中，我们可以推导出特定情况下的代换引理，即在项 $M$ 和 $N$ 的代换中，代换 $\sigma$ 和重命名函数 $\rho$ 可以交换。具体推导可以直接利用前面的结果和 $\text{rename-subst-ren}$ 方程。

### **总结**

通过这些步骤，我们证明了代换引理以及其推广形式。这些证明依赖于 σ-代数的方程，通过它们的组合和推导，我们得出了代换操作的可交换性。

