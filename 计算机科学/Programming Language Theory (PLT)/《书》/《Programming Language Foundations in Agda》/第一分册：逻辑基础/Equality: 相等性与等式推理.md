[toc]

以下是本章节涉及的所有数理逻辑相关概念的解释，并使用了 Markdown 的公式表示法：

### 1. **相等性（Equality, $\equiv$）**
   - 相等性表示两个项在逻辑上是相同的，记为 $M \equiv N$，表示 $M$ 和 $N$ 可以相互替换。相等性是一种基本的逻辑关系。

### 2. **相等性的归纳定义**
   - 相等性在 Agda 中定义为一个归纳的数据类型，通过以下方式定义：
     $$
     \text{data } _\equiv_ \{A : \text{Set}\} (x : A) : A \rightarrow \text{Set} \text{ where }
     \text{refl : } x \equiv x
     $$
   - 这里，$A$ 是一个类型，$x$ 是该类型的一个值。$x \equiv x$ 的证明由构造子 `refl` 提供。

### 3. **自反性（Reflexivity）**
   - 自反性表明对于任何 $x$，$x \equiv x$ 成立。这直接由相等性的定义得出。

### 4. **对称性（Symmetry）**
   - 对称性表明如果 $x \equiv y$，那么 $y \equiv x$ 也成立。这个性质可以通过对相等性定义中的 `refl` 构造子来证明：
     $$
     \text{sym : } \forall \{A : \text{Set}\} \{x y : A\} \rightarrow x \equiv y \rightarrow y \equiv x
     $$
     证明为：
     $$
     \text{sym refl = refl}
     $$

### 5. **传递性（Transitivity）**
   - 传递性表明如果 $x \equiv y$ 且 $y \equiv z$，那么 $x \equiv z$ 也成立。这个性质可以通过如下方式证明：
     $$
     \text{trans : } \forall \{A : \text{Set}\} \{x y z : A\} \rightarrow x \equiv y \rightarrow y \equiv z \rightarrow x \equiv z
     $$
     证明为：
     $$
     \text{trans refl refl = refl}
     $$

### 6. **合同性（Congruence）**
   - 合同性表明如果两个项 $x \equiv y$，那么将它们代入相同的函数中，其结果仍然相等：
     $$
     \text{cong : } \forall \{A B : \text{Set}\} (f : A \rightarrow B) \{x y : A\} \rightarrow x \equiv y \rightarrow f x \equiv f y
     $$
     证明为：
     $$
     \text{cong f refl = refl}
     $$

### 7. **替换性（Substitution）**
   - 替换性表明如果 $x \equiv y$，且 $P x$ 成立，那么 $P y$ 也成立：
     $$
     \text{subst : } \forall \{A : \text{Set}\} \{x y : A\} (P : A \rightarrow \text{Set}) \rightarrow x \equiv y \rightarrow P x \rightarrow P y
     $$
     证明为：
     $$
     \text{subst P refl px = px}
     $$

### 8. **等式链（Equality Chain）**
   - 等式链是一种用于推理的技术，通过逐步应用相等性来连接一系列等式。Agda 中的等式链定义为：
     $$
     \begin{aligned}
     \text{begin } x \equiv y & = x \equiv y \\
     x \equiv\langle\rangle y & = x \equiv y \\
     \text{step-}\equiv & : \forall (x \{y z\} : A) \rightarrow y \equiv z \rightarrow x \equiv y \rightarrow x \equiv z
     \end{aligned}
     $$
     使用等式链可以让证明更易于阅读和理解。

### 9. **重写（Rewrite）**
   - 重写是一种在推理中使用的技术，通过已知的等式将目标表达式中的某一部分替换为等价的另一部分。重写在 Agda 中的语法为：
     $$
     \text{rewrite } \text{proof-name}
     $$
     重写可以简化证明过程。

### 10. **莱布尼兹相等性（Leibniz Equality, $\overset{\sim}{\equiv}$）**
   - 莱布尼兹相等性表示两个对象相等当且仅当它们满足完全相同的性质。定义如下：
     $$
     x \overset{\sim}{\equiv} y \text{ 当 } \forall P \, P(x) \rightarrow P(y)
     $$
   - 莱布尼兹相等性是 Martin-Löf 相等性的一个更强形式。

### 11. **全体多态（Universe Polymorphism）**
   - 全体多态允许定义可以应用于任何类型阶层 $ℓ$。例如，泛化的相等性定义为：
     $$
     \text{data } _\equiv' \{ℓ : \text{Level}\} \{A : \text{Set } ℓ\} (x : A) : A \rightarrow \text{Set } ℓ \text{ where }
     \text{refl' : } x \equiv' x
     $$

### 12. **层级（Levels）**
   - 层级表示类型的不同阶层，如 $Set₀$、$Set₁$ 等。定义层级时，可以使用以下构造子：
     $$
     \text{lzero : Level, lsuc : Level \rightarrow Level}
     $$
   - 运算符 $⊔$ 用于获取两个层级中较大的那个：
     $$
     _⊔_ : Level \rightarrow Level \rightarrow Level
     $$

### 13. **重载构造子（Overloaded Constructor）**
   - 重载构造子允许不同的数据类型使用相同的构造子名称。例如，在定义自然数与加法时，构造子 $suc$ 和 $zero$ 可以在多个定义中重复使用。

这些概念在数理逻辑和形式化证明中起到了关键作用，特别是在理解相等性和推理的过程中。希望这些解释能帮助您更好地理解本章节的内容。

### ----------------------------------

### 相等性与等式推理

#### 相等性的定义

**概念：**
- **相等性**：在 Agda 中，相等性被定义为一个归纳数据类型。具体来说，$M \equiv N$ 表示两个同类型的值 $M$ 和 $N$ 是相等的。相等性的定义如下：

  ```agda
  data _≡_ {A : Set} (x : A) : A → Set where
    refl : x ≡ x
  ```

  这个定义表示，对于任意类型 $A$ 和任意 $A$ 类型的值 $x$，构造子 `refl` 提供了 $x \equiv x$ 的证明。这个相等性只提供了自反性，并没有直接的手段来证明不同的值是相等的。

**解释：**
- 这意味着在 Agda 中，唯一的相等性证明是通过 `refl`，即一个值等于它自身。其他相等性需要通过对这些基本的自反性进行推理来证明。

#### 相等性是等价关系

**概念：**
- **等价关系**：一个等价关系是指满足自反性、对称性和传递性的关系。相等性自然满足这些性质：

  - **自反性（Reflexivity）**：通过 `refl` 直接证明。
  - **对称性（Symmetry）**：如果 $x \equiv y$，那么 $y \equiv x$。
  - **传递性（Transitivity）**：如果 $x \equiv y$ 且 $y \equiv z$，那么 $x \equiv z$。

**代码示例：**

- 对称性证明：

  ```agda
  sym : ∀ {A : Set} {x y : A}
    → x ≡ y
    -----
    → y ≡ x
  sym refl = refl
  ```

- 传递性证明：

  ```agda
  trans : ∀ {A : Set} {x y z : A}
    → x ≡ y
    → y ≡ z
      -----
    → x ≡ z
  trans refl refl  =  refl
  ```

**解释：**
- 这两个性质的证明都依赖于 `refl`，即对于自反性的依赖。通过模式匹配，我们可以将其他的相等性性质也构建出来。

#### 合同性和替换性

**概念：**
- **合同性（Congruence）**：如果两个项相等，那么对它们使用相同的函数，其结果仍然相等。

  ```agda
  cong : ∀ {A B : Set} (f : A → B) {x y : A}
    → x ≡ y
      ---------
    → f x ≡ f y
  cong f refl  =  refl
  ```

- **替换性（Substitution）**：如果两个值相等，其中一个满足某谓词，那么另一个也满足此谓词。

  ```agda
  subst : ∀ {A : Set} {x y : A} (P : A → Set)
    → x ≡ y
      ---------
    → P x → P y
  subst P refl px = px
  ```

**解释：**
- 合同性和替换性是相等性的两个重要性质，它们保证了在函数和谓词中相等性可以被传递。

#### 等式链

**概念：**
- **等式链（Equality Chain）**：在证明中，可以使用等式链将一系列等式连接起来，从而简化推导过程。Agda 提供了等式链的语法支持，使得推导过程更加直观和易于理解。

  ```agda
  module ≡-Reasoning {A : Set} where

    infix  1 begin_
    infixr 2 _≡⟨⟩_ step-≡
    infix  3 _∎

    begin_ : ∀ {x y : A}
      → x ≡ y
        -----
      → x ≡ y
    begin x≡y  =  x≡y

    _≡⟨⟩_ : ∀ (x : A) {y : A}
      → x ≡ y
        -----
      → x ≡ y
    x ≡⟨⟩ x≡y  =  x≡y

    step-≡ : ∀ (x {y z} : A) → y ≡ z → x ≡ y → x ≡ z
    step-≡ x y≡z x≡y  =  trans x≡y y≡z

    syntax step-≡ x y≡z x≡y  =  x ≡⟨  x≡y ⟩ y≡z

    _∎ : ∀ (x : A)
        -----
      → x ≡ x
    x ∎  =  refl
  ```

- 使用等式链重新证明传递性：

  ```agda
  trans′ : ∀ {A : Set} {x y z : A}
    → x ≡ y
    → y ≡ z
      -----
    → x ≡ z
  trans′ {A} {x} {y} {z} x≡y y≡z =
    begin
      x
    ≡⟨ x≡y ⟩
      y
    ≡⟨ y≡z ⟩
      z
    ∎
  ```

**解释：**
- 等式链提供了一种易于阅读和理解的方式来进行等式推导。在推导复杂的等式时，等式链非常有用。

#### 重写

**概念：**
- **重写（Rewrite）**：在 Agda 中，可以通过 `rewrite` 关键字进行重写推导，将已知的相等性应用到证明中。

  ```agda
  even-comm : ∀ (m n : ℕ)
    → even (m + n)
      ------------
    → even (n + m)
  even-comm m n ev  rewrite +-comm n m  =  ev
  ```

**解释：**
- `rewrite` 通过在推导过程中重写目标表达式，使得证明更加直接和易于管理。

#### 莱布尼兹相等性

**概念：**
- **莱布尼兹相等性（Leibniz Equality）**：这是另一种形式的相等性定义。莱布尼兹相等性表示如果对于所有谓词 $P$，$P \, x$ 蕴涵了 $P \, y$，那么 $x$ 与 $y$ 相等。

  ```agda
  _≐_ : ∀ {A : Set} (x y : A) → Set₁
  _≐_ {A} x y = ∀ (P : A → Set) → P x → P y
  ```

**解释：**
- 莱布尼兹相等性与 Martin-Löf 相等性是等价的，后者更直接，前者则基于谓词的不可区分性。

**练习：**
- **练习 `trans and ≡-Reasoning`**：
  - 尽管 `trans′` 使用了等式链的证明方法，但是不能作为 `trans` 的定义。理解原因并尝试解释。

**代码示例：**
- 等式链与 `trans` 的实现：

  ```agda
  trans′ : ∀ {A : Set} {x y z : A}
    → x ≡ y
    → y ≡ z
      -----
    → x ≡ z
  trans′ {A} {x} {y} {z} x≡y y≡z =
    begin
      x
    ≡⟨ x≡y ⟩
      y
    ≡⟨ y≡z ⟩
      z
    ∎
  ```

**解释：**
- `trans′` 使用等式链来推导相等性。尽管易于理解，但不能直接替代 `trans`，因为它依赖于 `trans` 的定义，这会导致循环依赖。

#### 全体多态与等级

**概念：**
- **全体多态（Universe Polymorphism）**：Agda 允许定义在不同的等级（levels）上进行多态的类型。通过使用全体多态，可以定义更通用的相等性和其他操作。

  ```agda
  data _≡′_ {ℓ : Level} {A : Set ℓ} (x : A) : A → Set ℓ where
    refl′ : x ≡′ x
  ```

**解释：**
- 使用全体多态的相等性定义可以适用于不同层级的类型，使得定义更加泛化。

#### 总结

本章详细探讨了相等性的定义及其性质，涵

### -----------------------------------------------

### **相等性与等式推理**

---

### **相等性**

#### 概念：
- **相等性**：在 Agda 中，相等性 $M ≡ N$ 表示两个项 $M$ 和 $N$ 是相等的，即它们可以互相替换。在之前的章节中，相等性被视为一种基础运算，现在将其定义为一个归纳数据类型。

  **定义：**
  ```agda
  data _≡_ {A : Set} (x : A) : A → Set where
    refl : x ≡ x
  ```

#### 解释：
- **解释**：该定义表示，对于任意类型 $A$ 和 $A$ 类型的项 $x$，构造子 `refl` 提供了 $x ≡ x$ 的证明。这个定义体现了相等性自反的特性，即每个值等同于它自身。

  - **参数与索引**：在这个定义中，相等性的第一个参数 $x : A$ 被定义为参数（Parameter），因为它不会变化；而第二个参数是一个索引（Index），因为它需要与第一个参数相等。

- **优先级**：相等性运算符 $≡$ 的优先级设置为 4，与 $≤$ 相同，因此其它算术运算符的结合性比它更紧密。此外，$x ≡ y ≡ z$ 是不合法的表达式，因为 $≡$ 既不是左结合也不是右结合的。

---

### **相等性是一个等价关系**

#### 概念：
- **等价关系**：一个等价关系是自反的、对称的和传递的。相等性满足这些条件，因此相等性是一个等价关系。

  **定义：**
  - **自反性**：通过构造子 `refl` 直接证明。
  - **对称性**：可以通过定义对称性的证明函数 `sym` 来证明。

    **定义：**
    ```agda
    sym : ∀ {A : Set} {x y : A} → x ≡ y → y ≡ x
    sym refl = refl
    ```

#### 解释：
- **自反性**：自反性表示任何值 $x$ 都与自身相等，即 $x ≡ x$。这个性质由 `refl` 构造子直接提供。

- **对称性**：对称性表示如果 $x ≡ y$，那么 $y ≡ x$ 也是成立的。在 `sym` 的证明中，由于参数类型为 $x ≡ y$，通过模式匹配 `refl`，$x$ 和 $y$ 必须相等，因此 `y ≡ x` 也成立。

  - **交互式证明**：通过交互式地使用 Agda 的工具，可以帮助理解 `sym` 的证明过程。首先，在 `sym` 的定义中使用一个变量作为参数，然后通过洞（hole）逐步展开 Agda 的提示，最后完成证明。

---

### **传递性**

#### 概念：
- **传递性**：传递性表示如果 $x ≡ y$ 且 $y ≡ z$，那么 $x ≡ z$ 也成立。

  **定义：**
  ```agda
  trans : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
  trans refl refl = refl
  ```

#### 解释：
- **解释**：在 `trans` 的定义中，通过模式匹配 `refl`，我们可以证明传递性。也就是说，如果 $x$ 与 $y$ 相等且 $y$ 与 $z$ 相等，那么 $x$ 与 $z$ 也相等。

---

### **合同性和替换性**

#### 概念：
- **合同性**：如果两个项相等，对它们应用相同的函数，结果仍然相等。

  **定义：**
  ```agda
  cong : ∀ {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
  cong f refl = refl
  ```

- **替换性**：如果两个值相等，其中一个满足某个谓词，另一个也满足该谓词。

  **定义：**
  ```agda
  subst : ∀ {A : Set} {x y : A} (P : A → Set) → x ≡ y → P x → P y
  subst P refl px = px
  ```

#### 解释：
- **合同性**：合同性定义表示，如果 $x ≡ y$，那么应用函数 $f$ 后，$f(x) ≡ f(y)$ 也成立。这个性质表明函数应用在相等的输入上会产生相等的输出。

- **替换性**：替换性允许我们将一个命题从一个值传递到另一个相等的值。$P$ 是一个谓词，如果 $x ≡ y$ 且 $P(x)$ 成立，那么 $P(y)$ 也成立。

---

### **等式链**

#### 概念：
- **等式链**：等式链是一种通过一系列相等性步骤来证明一个结论的推理方法。

  **定义：**
  ```agda
  module ≡-Reasoning {A : Set} where
    infix 1 begin_
    infixr 2 _≡⟨⟩_ step-≡
    infix 3 _∎
  
    begin_ : ∀ {x y : A} → x ≡ y → x ≡ y
    begin x≡y = x≡y
  
    _≡⟨⟩_ : ∀ (x : A) {y : A} → x ≡ y → x ≡ y
    x ≡⟨⟩ x≡y = x≡y
  
    step-≡ : ∀ (x {y z} : A) → y ≡ z → x ≡ y → x ≡ z
    step-≡ x y≡z x≡y = trans x≡y y≡z
  
    _∎ : ∀ (x : A) → x ≡ x
    x ∎ = refl
  ```

#### 解释：
- **等式链的使用**：等式链是为了方便书写和阅读推理过程，将多个相等性步骤连在一起的一个方法。通过 `begin_`、`_≡⟨⟩_` 和 `∎` 等工具，可以逐步构建和验证相等性链条，从而证明最终的结论。

- **示例**：
  ```agda
  trans′ : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
  trans′ {A} {x} {y} {z} x≡y y≡z =
    begin
      x
    ≡⟨ x≡y ⟩
      y
    ≡⟨ y≡z ⟩
      z
    ∎
  ```

  - 这段代码展示了如何使用等式链逐步推导出 $x ≡ z$。

---

### **莱布尼兹（Leibniz）相等性**

#### 概念：
- **莱布尼兹相等性**：两个对象 $x$ 和 $y$ 相等当且仅当对于每个谓词 $P$，$P(x)$ 成立意味着 $P(y)$ 也成立。

  **定义：**
  ```agda
  _≐_ : ∀ {A : Set} (x y : A) → Set₁
  _≐_ {A} x y = ∀ (P : A → Set) → P x → P y
  ```

#### 解释：
- **解释**：莱布尼兹相等性是另一种定义相等性的方法。它表示如果 $x$ 和 $y$ 相等，那么它们在所有谓词 $P$ 下都具有相同的性质。莱布尼兹相等性与 Martin-Löf 相等性是等价的，可以通过不同的方向进行证明。

  - **自反性和传递性**：通过 `refl-≐` 和 `trans-≐` 这两个函数，可以证明莱布尼兹相等性也是自反和传递的。

---

### **全体多态**

#### 概念：
- **全体多态（Universe Polymorphism）**：全体多态允许在不同层级的集合之间定义相等性和其他操作。

  **定义：**
  ```agda
  data _≡′_ {ℓ : Level} {A : Set ℓ} (x : A) : A → Set ℓ where
    refl′ : x ≡′ x
  ```

#### 解释：
- **解释**：全体多态的引入是

为了处理不同层级的集合。通过定义 `Level` 和相应的构造子 `lzero` 和 `lsuc`，我们可以在不同层级上推广相等性的定义，从而处理更为复杂的情况。

---

### **总结**

这一章介绍了相等性的定义及其在逻辑推理中的应用，特别是如何在 Agda 中使用相等性来构建和验证证明。它还介绍了莱布尼兹相等性和全体多态的概念，这些工具为更复杂的类型理论和证明提供了基础。
