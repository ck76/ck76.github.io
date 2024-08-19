[toc]

### 可进性与保型性

这章讨论了简单类型 λ-演算的两个重要性质：可进性 (Progress) 和保型性 (Preservation)。通过这些性质，我们可以确保一个良类型的闭项要么能够归约到一个值，要么它本身就是一个值，并且在归约过程中保持类型不变。

#### **导入部分**
```agda
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl; sym; cong; cong₂)
open import Data.String using (String; _≟_)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.Product using (_×_; proj₁; proj₂; ∃; ∃-syntax) renaming (_,_ to ⟨_,_⟩)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Relation.Nullary using (¬_; Dec; yes; no)
open import Function using (_∘_)
open import plfa.part1.Isomorphism
open import plfa.part2.Lambda
```

这些导入是为了解决各类基础设施问题。例如，使用 `_≡_` 表示相等，`ℕ` 表示自然数，以及 `⊥` 表示不可能的命题。

### **简介**
在上一章中，我们学习了简单类型 λ-演算中的闭项、作为值的项、项之间的归约，以及良类型的项。通过展示如何通过归约序列将一个项最终简化为一个值，我们可以确保在良类型的闭项下归约的正确性。例如，二加二可以通过多步归约得到结果四。

#### **可进性 (Progress)**
可进性描述了良类型的闭项必定要么已经是一个值，要么可以继续进行一步归约。

- **定义**：
    - **公式**：如果 $\varnothing ⊢ M : A$，那么 $M$ 要么是一个值，要么存在 $N$ 使得 $M \rightarrow N$。
    - **解释**：如果项 $M$ 是一个良类型的闭项，那么它要么已经是一个不可进一步简化的值，要么可以继续简化为另一个项 $N$。
    - **公式**：
      $$
      \text{Progress}(M) \rightarrow \text{Value}(M) \vee \exists N \ (M \rightarrow N)
      $$

- **例子**：
    - 如果 $M$ 是一个函数应用 $(\lambda x \Rightarrow x) \cdot y$，根据可进性，要么 $M$ 是一个值，要么可以进一步归约。

#### **保型性 (Preservation)**
保型性确保了在归约过程中，项的类型保持不变。

- **定义**：
    - **公式**：如果 $\varnothing ⊢ M : A$ 且 $M \rightarrow N$，那么 $\varnothing ⊢ N : A$。
    - **解释**：如果 $M$ 是一个类型为 $A$ 的项，并且 $M$ 可以归约为 $N$，那么 $N$ 也应该是一个类型为 $A$ 的项。
    - **公式**：
      $$
      \text{Preservation}(M \rightarrow N) \rightarrow (\varnothing ⊢ M : A) \rightarrow (\varnothing ⊢ N : A)
      $$

- **例子**：
    - 考虑一个表达式 $M = (\lambda x \Rightarrow x) \cdot y$，它可以一步归约为 $N = y$。如果 $M$ 的类型是 $A$，那么 $N$ 的类型也应该是 $A$。

### **值无法被归约**

对于值，已经没有进一步的归约步骤。

- **定义**：
    - **公式**：
      $$
      V \nrightarrow N \quad \text{如果} \quad \text{Value}(V)
      $$

- **解释**：值是λ-表达式中的终点，无法进一步简化。

- **例子**：
    - $\lambda x \Rightarrow x$ 是一个值，不可能归约到其他形式。

### **可进性证明**

为了证明一个良类型的项满足可进性，我们使用递归分析项的结构：

- **公式**：
    $$
    \text{Progress}(M) \rightarrow \text{step} \ | \ \text{done}
    $$

- **解释**：我们通过递归分析一个项 $M$，要么证明它可以进行一步归约（step），要么证明它是一个值（done）。

- **例子**：
    - 对于项 $(\lambda x \Rightarrow x) \cdot y$，首先检查 $L = \lambda x \Rightarrow x$，它是一个值，因此通过可进性检查 $y$ 是否可以归约。如果 $y$ 是值，则可以应用 β 规则继续归约。

### **保型性证明**

保型性的证明需要考虑所有可能的归约步骤并逐一验证。

- **公式**：
    $$
    \text{Preserve}(M \rightarrow N) \rightarrow (\varnothing ⊢ M : A) \rightarrow (\varnothing ⊢ N : A)
    $$

- **解释**：通过归纳分析所有可能的归约规则，证明从 $M$ 归约到 $N$ 时，类型 $A$ 不变。

- **例子**：
    - 对于应用规则 $(\lambda x \Rightarrow x) \cdot y \rightarrow y$，如果 $M$ 的类型是 $A$，根据保型性，$N$ 的类型也为 $A$。

---

这个过程解释了λ-演算中可进性和保型性的概念及其证明方法。如果还有后续章节需要详细解释，请告诉我。

### 归约保持类型的准备工作

在证明保型性之前，首先需要准备一些引理，这些引理涉及重命名（Renaming）、替换（Substitution）和交换（Swapping）等操作。

#### 重命名保持类型

重命名的引理表明，如果我们将语境中的变量重命名，那么项的类型保持不变。

- **概念**：
  - **重命名**：假设有两个语境 $\Gamma$ 和 $\Delta$，如果 $\Gamma$ 中的每个变量在 $\Delta$ 中也存在且类型相同，那么在语境 $\Gamma$ 中可赋类型的项在 $\Delta$ 中也具有相同的类型。

- **公式**：
  - 对于任意项 $M$ 和类型 $A$，
    $$
    \text{如果} \quad \forall x, \ A, \ (\Gamma \ni x : A) \rightarrow (\Delta \ni x : A) \quad \text{那么} \quad (\Gamma \vdash M : A) \rightarrow (\Delta \vdash M : A)
    $$

- **解释**：
  - 通过将语境中的每个变量重命名，我们可以证明，重命名后的项在新的语境中仍然保持原来的类型。这个引理是保型性证明的重要一步，因为它确保在语境改变的情况下，项的类型仍然一致。

#### 替换保持类型

替换的引理确保在对项进行替换时，项的类型保持不变。

- **概念**：
  - **替换**：如果我们有一个闭项 $V$，并且我们将项 $N$ 中的变量 $x$ 替换为 $V$，那么替换后的项 $N[x := V]$ 仍然保持原来的类型。

- **公式**：
  - 对于闭项 $V$ 和项 $N$，如果 $\emptyset \vdash V : A$ 且 $\Gamma, x : A \vdash N : B$，那么
    $$
    \Gamma \vdash N[x := V] : B
    $$

- **解释**：
  - 替换引理表明，替换一个闭项不会改变原项的类型。这对于证明 β-归约等规则的保型性至关重要，因为这些规则通常涉及将一个变量替换为一个具体的项。

#### 交换引理（Swap）

交换引理表明，在语境中交换两个不同的变量不会改变项的类型。

- **公式**：
  - 对于项 $M$ 和类型 $A, B, C$，
    $$
    \text{如果} \quad \Gamma, y : B, x : A \vdash M : C \quad \text{且} \quad x \neq y \quad \text{那么} \quad \Gamma, x : A, y : B \vdash M : C
    $$

- **解释**：
  - 交换引理允许我们在语境中交换变量的顺序，同时保持项的类型不变。这个引理在处理复杂的语境操作时非常有用。

### 替换保持类型的证明

为了证明保型性，最关键的一步是证明替换保持类型。这一步涉及到复杂的归纳和递归。

- **证明过程**：
  - 对于每种可能的情况（变量、λ-抽象、函数应用、自然数、对自然数的分项等），我们需要分别考虑替换操作对类型的影响。

- **例子**：
  - 考虑 λ-抽象的情况，假设我们有一个项 $\lambda x \Rightarrow N$ 和一个闭项 $V$。如果我们将 $N$ 中的 $x$ 替换为 $V$，我们需要证明替换后的项 $\lambda x \Rightarrow N[x := V]$ 仍然保持原来的类型。这个证明过程涉及到将语境扩展、递归应用替换引理等操作。

- **公式**：
  $$
  \text{subst} : \forall \Gamma, x, N, V, A, B \ \text{若} \ \emptyset \vdash V : A \ \text{且} \ \Gamma, x : A \vdash N : B \ \text{则} \ \Gamma \vdash N[x := V] : B
  $$

### 保型性证明

一旦替换保持类型的引理被证明，保型性的证明就变得相对简单。

- **公式**：
  $$
  \text{preserve} : \forall M, N, A \ \text{若} \ \emptyset \vdash M : A \ \text{且} \ M \rightarrow N \ \text{则} \ \emptyset \vdash N : A
  $$

- **证明过程**：
  - 通过对所有可能的归约规则进行归纳分析，可以证明在归约 $M \rightarrow N$ 过程中，项的类型保持不变。例如，应用规则 $(\lambda x \Rightarrow N) \cdot V \rightarrow N[x := V]$ 时，使用替换保持类型的引理，我们可以证明 $N[x := V]$ 的类型与 $M$ 的类型相同。

- **例子**：
  - 对于一个 λ-抽象 $(\lambda x \Rightarrow N) \cdot V$，假设 $M$ 的类型是 $A$，通过应用 β-归约规则和替换引理，我们可以证明 $N[x := V]$ 也具有类型 $A$。

---

接下来如果需要继续解释归约过程的其他部分，如求值器的实现、范式化等内容，请告诉我。



### 求值器的实现

在这一部分，我们将通过实现一个求值器来展示如何对良类型的闭项进行求值。这个求值器不仅可以计算归约序列，还可以处理非终止的情况（例如无限递归）。

#### 燃料和求值

为了确保求值器总是终止，我们引入了“燃料”的概念。燃料是一个自然数，它限制了求值器可以执行的最大归约步骤数。

- **燃料**：在计算中，燃料相当于计算资源的一个度量。每执行一步归约，燃料都会减少。如果燃料耗尽但归约尚未完成，求值器将返回一个“燃料不足”的标志。

- **燃料定义**：
  ```agda
  record Gas : Set where
    constructor gas
    field
      amount : ℕ
  ```

#### Finished 数据类型

`Finished` 是一个表示项是否完成归约的数据类型。如果项已经是值，或者燃料耗尽了，求值器就会停止。

- **Finished 定义**：
  ```agda
  data Finished (N : Term) : Set where
    done : Value N → Finished N
    out-of-gas : Finished N
  ```

#### Steps 数据类型

`Steps` 数据类型表示从初始项 $L$ 到最终项 $N$ 的归约序列，以及归约是否完成。

- **Steps 定义**：
  ```agda
  data Steps (L : Term) : Set where
    steps : ∀ {N} → L —↠ N → Finished N → Steps L
  ```

#### eval 函数

`eval` 函数是求值器的核心。它接受燃料和项的类型信息，然后返回从初始项到最终项的归约步骤序列。

- **eval 函数**：
  ```agda
  eval : ∀ {L A} → Gas → ∅ ⊢ L ⦂ A → Steps L
  eval {L} (gas zero) ⊢L = steps (L ∎) out-of-gas
  eval {L} (gas (suc m)) ⊢L with progress ⊢L
  ... | done VL = steps (L ∎) (done VL)
  ... | step {M} L—→M with eval (gas m) (preserve ⊢L L—→M)
  ...    | steps M—↠N fin = steps (L —→⟨ L—→M ⟩ M—↠N) fin
  ```

- **解释**：
  - 如果燃料为零，求值器会立即停止，并返回“燃料不足”的结果。
  - 如果项已经是一个值，求值器返回包含该值的步骤序列。
  - 如果项可以进一步归约，则求值器递归调用自身，逐步减少燃料，直到达到终止条件。

#### 例子：计算归约序列

通过 `eval` 函数，我们可以计算各种项的归约序列。

- **例子：计算 `sucμ` 的归约序列**：
  ```agda
  _ : eval (gas 3) ⊢sucμ ≡
    steps
      (μ "x" ⇒ `suc ` "x"
      —→⟨ β-μ ⟩
      `suc (μ "x" ⇒ `suc ` "x")
      —→⟨ ξ-suc β-μ ⟩
      `suc `suc (μ "x" ⇒ `suc ` "x")
      —→⟨ ξ-suc (ξ-suc β-μ) ⟩
      `suc `suc `suc (μ "x" ⇒ `suc ` "x")
      ∎)
      out-of-gas
  ```

- **解释**：
  - 在这个例子中，`sucμ` 是一个无限递归项。我们给求值器提供了3步的燃料，求值器计算了3步归约并返回了“燃料不足”的标志。

- **例子：计算 Church 数字的归约**：
  ```agda
  _ : eval (gas 100) (⊢twoᶜ · ⊢sucᶜ · ⊢zero) ≡
    steps
      ((ƛ "s" ⇒ (ƛ "z" ⇒ ` "s" · (` "s" · ` "z"))) · (ƛ "n" ⇒ `suc ` "n")
      · `zero
      —→⟨ ξ-·₁ (β-ƛ V-ƛ) ⟩
      ...
      `suc (`suc `zero)
      ∎)
      (done (V-suc (V-suc V-zero)))
  ```

- **解释**：
  - 该例子展示了将 Church 数字的 2 应用于 `suc` 和 `zero`，求值器能够成功计算出 `suc (suc zero)`，即数字 2。

#### 保型性和可进性结合

通过结合保型性（Preservation）和可进性（Progress），我们确保求值器在处理良类型的闭项时，可以顺利进行求值并在适当的时候终止。

- **Progress 定理**：
  - 如果项在空语境中是良类型的，则项要么是一个值，要么可以进行一步归约。
  - 公式：
    $$
    \text{progress} : \forall M A, (\emptyset \vdash M : A) \rightarrow \text{Progress}(M)
    $$

- **Preservation 定理**：
  - 如果项 $M$ 可以归约到 $N$，且 $M$ 具有类型 $A$，则 $N$ 也具有类型 $A$。
  - 公式：
    $$
    \text{preserve} : \forall M N A, (\emptyset \vdash M : A) \rightarrow (M \rightarrow N) \rightarrow (\emptyset \vdash N : A)
    $$

### 归约的确定性

我们已经声称归约是确定的，即一个项 $M$ 如果可以归约到 $M'$ 和 $M''$，那么 $M'$ 和 $M''$ 必然是相等的。

#### 确定性证明

- **公式**：
  $$
  \text{det} : \forall M M' M'', (M \rightarrow M') \rightarrow (M \rightarrow M'') \rightarrow (M' \equiv M'')
  $$

- **证明过程**：
  - 通过对每一种可能的归约规则进行分析，我们可以证明归约是确定的。例如，在 $(\lambda x \Rightarrow N) \cdot V$ 归约的情况下，无论 $M$ 归约到 $M'$ 还是 $M''$，最终的结果 $M'$ 和 $M''$ 必然相等。

- **例子**：
  - 考虑两个归约规则的情况：一个是应用 ξ-规则，一个是应用 β-规则。在这两种情况下，我们可以通过矛盾证明，归约的结果是相同的。

---

这个部分涵盖了如何通过可进性和保型性构建一个求值器，并证明了归约的确定性。如果你对进一步的细节或其他方面有兴趣，请告诉我。

