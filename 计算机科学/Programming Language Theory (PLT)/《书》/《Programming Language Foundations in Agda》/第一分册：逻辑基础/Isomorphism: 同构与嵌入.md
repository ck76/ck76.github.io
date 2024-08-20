[toc]

以下是本章节涉及的所有数理逻辑相关概念的解释：

### 1. **同构（Isomorphism, $A \cong B$）**
   - 同构表示两个集合（类型）之间存在一一对应的关系。形式化定义为：
     $$
     \text{record } A \cong B : \text{Set} \text{ where}
     $$
     - **to**: $A \rightarrow B$
     - **from**: $B \rightarrow A$
     - **from∘to**: $\forall x : A, \text{from}(\text{to}(x)) \equiv x$
     - **to∘from**: $\forall y : B, \text{to}(\text{from}(y)) \equiv y$
   - 同构的关键在于，两个集合之间的映射不仅是双射（bijection），而且存在逆函数。

### 2. **自反性（Reflexivity）**
   - 自反性表示对于每个集合 $A$，存在 $A \cong A$，即 $A$ 与自身同构。该性质的证明通过恒等函数（identity function）作为同构的映射函数来实现。

### 3. **对称性（Symmetry）**
   - 对称性表示如果 $A \cong B$，那么 $B \cong A$ 也成立。对称性的证明通过交换同构中的映射函数（to 和 from）来实现。

### 4. **传递性（Transitivity）**
   - 传递性表示如果 $A \cong B$ 且 $B \cong C$，那么 $A \cong C$ 也成立。传递性通过组合映射函数来实现，即将 $A \rightarrow B$ 和 $B \rightarrow C$ 的映射函数组合起来，形成 $A \rightarrow C$ 的映射函数。

### 5. **嵌入（Embedding, $A \hookrightarrow B$ 或 $A \preceq B$）**
   - 嵌入是一种弱化的同构关系，表示 $A$ 可以嵌入到 $B$ 中，意味着 $A$ 的每个元素都可以映射到 $B$ 中的一个元素，但该映射可能不是双射。嵌入的定义形式化为：
     $$
     \text{record } A \preceq B : \text{Set} \text{ where}
     $$
     - **to**: $A \rightarrow B$
     - **from**: $B \rightarrow A$
     - **from∘to**: $\forall x : A, \text{from}(\text{to}(x)) \equiv x$
   - 嵌入的关键是，$A$ 可以通过某种方式包含在 $B$ 中。

### 6. **反对称性（Antisymmetry）**
   - 反对称性是指如果 $A \preceq B$ 且 $B \preceq A$，并且它们的映射函数相互对应，那么 $A \cong B$，即 $A$ 与 $B$ 是同构的。

### 7. **外延性（Extensionality）**
   - 外延性指的是，如果两个函数在相同的输入上总是产生相同的输出，那么这两个函数是相等的。形式化定义为：
     $$
     \text{postulate } \text{extensionality} : \forall \{A B : \text{Set}\} \{f g : A \rightarrow B\}, (\forall x : A, f(x) \equiv g(x)) \rightarrow f \equiv g
     $$
   - 外延性假设允许我们在证明函数相等时只需验证它们在所有输入下的输出相等。

### 8. **Lambda 表达式（Lambda Expressions, $\lambda$）**
   - Lambda 表达式是一种匿名函数定义方式，用于定义不带名称的函数。其形式为：
     $$
     \lambda x \rightarrow N
     $$
     其中，$x$ 是参数，$N$ 是函数体。Lambda 表达式在函数式编程中非常常见。

### 9. **函数组合（Function Composition, $∘$）**
   - 函数组合是指将一个函数的输出作为另一个函数的输入。其定义为：
     $$
     (g \circ f)(x) = g(f(x))
     $$
   - 函数组合允许将多个函数串联起来，形成新的函数。

### 10. **命题等价性（Propositional Equivalence, $\Leftrightarrow$ 或 $⇔$）**
   - 命题等价性表示两个命题 $A$ 和 $B$ 之间的双向蕴涵关系，即 $A \Rightarrow B$ 且 $B \Rightarrow A$。其形式化定义为：
     $$
     \text{record } A ⇔ B : \text{Set} \text{ where}
     $$
     - **to**: $A \rightarrow B$
     - **from**: $B \rightarrow A$
   - 命题等价性证明了 $A$ 和 $B$ 在逻辑上是等价的。

这些概念在数理逻辑和形式化证明中起到了关键作用，尤其是在理解类型同构、嵌入、函数外延性等高级概念时。通过这些概念，我们能够更好地理解类型论和构造性数学的基础。

### --------------------------------

好的，我将按照原文的结构逐小节地解释每个部分涉及到的概念，并在解释时使用公式进行说明。

---

### **Lambda 表达式**

#### 概念：
- **Lambda 表达式**：Lambda 表达式是一种用于定义匿名函数的简洁方法。在 Agda 中，Lambda 表达式的形式为 `λ {P₁ → N₁; ⋯ ; Pₙ → Nₙ}`。这种表达式等价于定义一个函数，该函数的行为由多个模式匹配规则决定。每个模式 $P_i$ 对应一个结果表达式 $N_i$。

  **公式：**
  $$
  λ \{ P₁ → N₁; ⋯ ; Pₙ → Nₙ \}
  $$
  该表达式表示一个匿名函数，它接受一个输入并根据匹配的模式 $P_i$ 返回结果 $N_i$。

#### 解释：
- **匿名函数**：使用 Lambda 表达式定义的函数是匿名的，即它们没有名字。它们常用于嵌套函数或仅一次使用的场景，从而避免了定义函数名的冗余。

  **示例：**
  $$
  λ x → x + 1
  $$
  该表达式定义了一个接受一个参数 $x$ 的匿名函数，并返回 $x + 1$。

---

### **函数组合 （Function Composition）**

#### 概念：
- **函数组合**：函数组合是将两个函数按顺序组合成一个新的函数。在 Agda 中，函数组合的定义如下：
  $$
  (g ∘ f) x = g (f(x))
  $$
  其中，$g ∘ f$ 表示一个新的函数，该函数首先应用 $f$ 到输入 $x$，然后将 $f(x)$ 的结果作为输入应用到函数 $g$。

#### 解释：
- **函数组合的意义**：函数组合提供了一种将多个函数操作链式连接的方式，这种方式在需要连续应用多个函数时非常方便。

  **公式示例：**
  $$
  (g ∘ f)(x) = g(f(x))
  $$
  例如，如果 $f(x) = x + 1$ 且 $g(y) = 2y$，那么 $g ∘ f$ 就是一个先将 $x$ 加 1，然后将结果乘以 2 的函数。

---

### **外延性（Extensionality）**

#### 概念：
- **外延性**：外延性是指如果两个函数对于所有输入返回相同的结果，那么这两个函数是相等的。这一特性可以通过假设（postulate）来声明：
  $$
  \text{extensionality} : (∀ x : A, f(x) ≡ g(x)) → f ≡ g
  $$
  这意味着如果对于所有 $x \in A$，$f(x)$ 都等于 $g(x)$，那么 $f$ 和 $g$ 是相等的函数。

#### 解释：
- **函数的不可区分性**：外延性表达了函数的相等性只取决于它们的行为，而不取决于它们的定义形式。两个函数在所有输入上返回相同的结果，就可以认为它们是同一个函数。

  **公式解释：**
  $$
  \text{extensionality} : (∀ x : A, f(x) ≡ g(x)) → f ≡ g
  $$
  该公式说明，如果对于集合 $A$ 中的所有元素 $x$，$f(x)$ 和 $g(x)$ 相等，那么 $f$ 和 $g$ 就是同一个函数。

---

### **同构（Isomorphism）**

#### 概念：
- **同构**：两个集合 $A$ 和 $B$ 之间是同构的，如果存在一对双射函数 $f: A \to B$ 和 $g: B \to A$，使得 $g ∘ f = \text{id}_A$ 且 $f ∘ g = \text{id}_B$。在 Agda 中，同构的定义为：
  $$
  \text{record } A ≃ B \text{ where}
  \text{ field } 
  f : A → B,
  g : B → A,
  g ∘ f = \text{id}_A,
  f ∘ g = \text{id}_B
  $$
  
  其中，$\text{id}_A$ 和 $\text{id}_B$ 分别是 $A$ 和 $B$ 的恒等函数。

#### 解释：
- **同构的含义**：同构表示两个集合在结构上是相同的，因为它们可以通过双射函数相互映射。这样的结构相等性意味着如果 $A$ 和 $B$ 是同构的，那么它们的任何性质在 $A$ 中成立也必然在 $B$ 中成立。

  **公式解释：**
  $$
  f ∘ g = \text{id}_B \quad \text{和} \quad g ∘ f = \text{id}_A
  $$
  这意味着 $f$ 和 $g$ 是彼此的逆函数，即通过 $f$ 映射到 $B$ 再通过 $g$ 返回 $A$，结果仍然是原来的 $A$ 中的元素，反之亦然。

---

### **同构是一个等价关系**

#### 概念：
- **自反性**：同构是自反的，因为任何集合 $A$ 都与自身同构。其证明是使用恒等函数作为同构映射：
  $$
  A ≃ A
  $$
  
- **对称性**：如果 $A ≃ B$，那么 $B ≃ A$。这可以通过交换同构映射来证明：
  $$
  \text{to} : A \to B \quad \text{和} \quad \text{from} : B \to A
  $$
  
- **传递性**：如果 $A ≃ B$ 且 $B ≃ C$，那么 $A ≃ C$。这可以通过组合两个同构映射来证明：
  $$
  \text{to} : A \to B \to C, \quad \text{from} : C \to B \to A
  $$

#### 解释：
- **自反性**：集合 $A$ 自身的同构性是显然的，因为恒等函数 $f(x) = x$ 是双射函数。

- **对称性**：同构的对称性表示如果 $A$ 和 $B$ 是同构的，那么我们可以将映射关系反过来，这样 $B$ 和 $A$ 也是同构的。

- **传递性**：同构的传递性允许我们通过已知 $A$ 和 $B$ 同构，以及 $B$ 和 $C$ 同构，推导出 $A$ 和 $C$ 也是同构的。

---

### **嵌入（Embedding）**

#### 概念：
- **嵌入**：嵌入是同构的弱化版本。嵌入仅要求集合 $A$ 可以通过一个单射映射嵌入到集合 $B$ 中，而不需要存在一个逆映射。在 Agda 中，嵌入的定义为：
  $$
  \text{record } A ≲ B \text{ where}
  \text{ field } 
  f : A → B,
  g : B → A,
  g ∘ f = \text{id}_A
  $$
  
  其中 $f$ 是嵌入映射，$g$ 是从 $B$ 到 $A$ 的映射，但不一定是双射。

#### 解释：
- **嵌入的意义**：嵌入表示集合 $A$ 的结构可以嵌入到集合 $B$ 中，即 $A$ 的每个元素可以在 $B$ 中找到一个唯一的对应元素。但与同构不同，嵌入不要求 $B$ 中的每个元素都能映射回 $A$。

  **公式解释：**
  $$
  g ∘ f = \text{id}_A
  $$
  这意味着 $A$ 中的每个元素 $x$，通过 $f$ 映射到 $B$ 中，再通过 $g$ 映射回 $A$ 中，得到的仍然是 $x$ 自身。

---

### **嵌入是自反和传递的**

#### 概念：
- **自反性**：集合 $A$ 自己可以嵌入到自身中。这是通过使用恒等函数 $f(x) = x$ 实现的：
  $$
  A ≲ A
  $$

- **传递性**：如果集合 $A$ 可以嵌入到 $B$ 中，并且 $B$ 可以嵌入到 $C$ 中，那么 $A$ 也可以嵌入到 $C$ 中：
  $$
  A ≲ B ≲ C \Rightarrow A ≲

 C
  $$

#### 解释：
- **自反性**：这类似于同构的自反性，集合 $A$ 总是可以通过恒等映射嵌入到自身中。

- **传递性**：嵌入的传递性允许我们通过组合两个嵌入映射，构造出一个新的嵌入映射，从而实现 $A$ 到 $C$ 的嵌入。

---

这些解释涵盖了每一小节的主要概念，并紧跟着解释其内涵。如果需要更多详细的内容或有其他问题，请告诉我。





### ------------------------------

### 同构与嵌入

#### Lambda 表达式

**概念：**
- **Lambda 表达式**：Lambda 表达式提供了一种定义匿名函数的简洁方式。在 Agda 中，Lambda 表达式的形式为 `λ {P₁ → N₁; ⋯ ; Pₙ → Nₙ}`，这相当于定义一个函数 $f$，使得 $f$ 满足一组模式匹配规则。Lambda 表达式可以避免冗长的类型声明，并且定义出现在使用的地方，使得代码更简洁。

**代码示例：**
```agda
λ x → N
```

**解释：**
- Lambda 表达式常用于匿名函数定义，尤其在嵌套或一次性使用时非常有用。

#### 函数组合 （Function Composition）

**概念：**
- **函数组合**：函数组合是将两个函数按顺序组合成一个新的函数。在 Agda 中，定义如下：

  ```agda
  _∘_ : ∀ {A B C : Set} → (B → C) → (A → B) → (A → C)
  (g ∘ f) x  = g (f x)
  ```

**解释：**
- `g ∘ f` 表示一个函数，先应用 $f$，再应用 $g$。这使得函数组合非常自然地表达链式操作。

#### 外延性（Extensionality）

**概念：**
- **外延性**：外延性断言如果两个函数在所有输入上返回相同的结果，那么它们就是相同的函数。这与 `cong-app` 是逆命题。外延性可以通过 `postulate` 进行假设：

  ```agda
  postulate
    extensionality : ∀ {A B : Set} {f g : A → B}
      → (∀ (x : A) → f x ≡ g x)
        -----------------------
      → f ≡ g
  ```

**解释：**
- 假设外延性不会引发理论上的矛盾，且在一些情况下，可以使得推导更加简洁。

#### 同构（Isomorphism）

**概念：**
- **同构**：两个集合是同构的，如果它们之间存在双射关系，即每个元素都可以一一对应。同构的定义如下：

  ```agda
  record _≃_ (A B : Set) : Set where
    field
      to   : A → B
      from : B → A
      from∘to : ∀ (x : A) → from (to x) ≡ x
      to∘from : ∀ (y : B) → to (from y) ≡ y
  ```

**解释：**
- 同构表示两个类型是“相等”的，因为它们可以通过一一对应的映射相互转换。同构是等价关系，满足自反性、对称性和传递性。

**代码示例：**

- 自反性证明：

  ```agda
  ≃-refl : ∀ {A : Set}
      -----
    → A ≃ A
  ≃-refl =
    record
      { to      = λ{x → x}
      ; from    = λ{y → y}
      ; from∘to = λ{x → refl}
      ; to∘from = λ{y → refl}
      }
  ```

- 对称性证明：

  ```agda
  ≃-sym : ∀ {A B : Set}
    → A ≃ B
      -----
    → B ≃ A
  ≃-sym A≃B =
    record
      { to      = from A≃B
      ; from    = to   A≃B
      ; from∘to = to∘from A≃B
      ; to∘from = from∘to A≃B
      }
  ```

- 传递性证明：

  ```agda
  ≃-trans : ∀ {A B C : Set}
    → A ≃ B
    → B ≃ C
      -----
    → A ≃ C
  ≃-trans A≃B B≃C =
    record
      { to       = to   B≃C ∘ to   A≃B
      ; from     = from A≃B ∘ from B≃C
      ; from∘to  = λ{x →
          begin
            (from A≃B ∘ from B≃C) ((to B≃C ∘ to A≃B) x)
          ≡⟨⟩
            from A≃B (from B≃C (to B≃C (to A≃B x)))
          ≡⟨ cong (from A≃B) (from∘to B≃C (to A≃B x)) ⟩
            from A≃B (to A≃B x)
          ≡⟨ from∘to A≃B x ⟩
            x
          ∎}
      ; to∘from = λ{y →
          begin
            (to B≃C ∘ to A≃B) ((from A≃B ∘ from B≃C) y)
          ≡⟨⟩
            to B≃C (to A≃B (from A≃B (from B≃C y)))
          ≡⟨ cong (to B≃C) (to∘from A≃B (from B≃C y)) ⟩
            to B≃C (from B≃C y)
          ≡⟨ to∘from B≃C y ⟩
            y
          ∎}
       }
  ```

**解释：**
- 通过这三个性质，可以证明两个集合之间的同构关系。

#### 嵌入（Embedding）

**概念：**
- **嵌入**：嵌入是同构的弱化版本，仅要求一个类型能够嵌入到另一个类型中，而不要求存在逆映射。嵌入的定义如下：

  ```agda
  record _≲_ (A B : Set) : Set where
    field
      to      : A → B
      from    : B → A
      from∘to : ∀ (x : A) → from (to x) ≡ x
  ```

**解释：**
- 嵌入意味着 $A$ 类型的每个元素都可以映射到 $B$ 类型中，并且这种映射是单射的。

**代码示例：**

- 自反性证明：

  ```agda
  ≲-refl : ∀ {A : Set} → A ≲ A
  ≲-refl =
    record
      { to      = λ{x → x}
      ; from    = λ{y → y}
      ; from∘to = λ{x → refl}
      }
  ```

- 传递性证明：

  ```agda
  ≲-trans : ∀ {A B C : Set} → A ≲ B → B ≲ C → A ≲ C
  ≲-trans A≲B B≲C =
    record
      { to      = λ{x → to   B≲C (to   A≲B x)}
      ; from    = λ{y → from A≲B (from B≲C y)}
      ; from∘to = λ{x →
          begin
            from A≲B (from B≲C (to B≲C (to A≲B x)))
          ≡⟨ cong (from A≲B) (from∘to B≲C (to A≲B x)) ⟩
            from A≲B (to A≲B x)
          ≡⟨ from∘to A≲B x ⟩
            x
          ∎}
       }
  ```

**解释：**
- 嵌入是一个方向的映射，因此它是自反和传递的，但不是对称的。

**练习：**
- **练习 `≃-implies-≲`**：
  - 证明每个同构蕴涵了一个嵌入。

**代码示例：**

```agda
postulate
  ≃-implies-≲ : ∀ {A B : Set}
    → A ≃ B
      -----
    → A ≲ B
```

**解释：**
- 每个同构关系自然可以产生一个嵌入关系，因为同构意味着存在双射，而嵌入只要求存在单射。

#### 标准库

**概念：**
- Agda 标准库中提供了一些与本章节定义类似的内容，但有时标准库的定义使用嵌套记录结构，使得使用起来不如本文中的定义方便。

**代码：**

```agda
import Function using (_∘_)
import Function.Inverse using (_↔_)
import Function.LeftInverse using (_↞_)
```

**解释：**


- 标准库中的 `_↔_` 和 `_↞_` 分别对应 `≃` 和 `≲`，但它们有更复杂的结构。

### 总结

本章探讨了同构与嵌入的概念。通过定义这些概念并证明它们的基本性质，我们能够更好地理解不同类型之间的关系。通过这些构造，我们还能够推导出更复杂的类型运算和性质。
