[toc]

以下是本章节涉及的所有数理逻辑相关概念的解释，并使用了 Markdown 的公式表示法：

### 1. **关系（Relation）**
   - 关系是在两个对象之间建立的一种联系。数学中最常见的关系包括等于、小于等于等。关系通常用符号如 $ \leq $ 来表示。

### 2. **小于等于关系（Less than or equal to, $\leq$）**
   - 小于等于关系定义为两个自然数之间的一种关系 $ m \leq n $，表示 $ m $ 小于或等于 $ n $。该关系可以通过两个推理规则来定义：
     - **起始步骤（Base Case）**：对于所有自然数 $ n $， $ 0 \leq n $ 成立。
     - **归纳步骤（Inductive Case）**：如果 $ m \leq n $，那么 $ \text{suc}(m) \leq \text{suc}(n) $ 也成立。

### 3. **索引数据类型（Indexed Datatype）**
   - 在 Agda 中，索引数据类型是带有索引（如自然数）的数据类型。例如，小于等于关系 $ m \leq n $ 是一个以 $ m $ 和 $ n $ 为索引的类型。

### 4. **隐式参数（Implicit Arguments）**
   - 隐式参数是指在定义中不需要显式声明的参数，Agda 会自动推导其值。例如，在 $ \leq $ 的定义中，$ m $ 和 $ n $ 可以是隐式的，因为它们可以从上下文推导出。

### 5. **可判定性（Decidability）**
   - 一种关系是可判定的，意味着我们可以有一个算法来决定这个关系是否成立。例如，对于两个自然数 $ m $ 和 $ n $，可以直接判断 $ m \leq n $ 是否成立。

### 6. **反演（Inversion）**
   - 反演是从更大的东西推导出更小的东西的过程。例如，如果 $ \text{suc}(m) \leq \text{suc}(n) $，那么我们可以反推出 $ m \leq n $。

### 7. **序关系的性质**
   - **自反性（Reflexivity）**：对于所有的 $n$，$ n \leq n $ 成立。
   - **传递性（Transitivity）**：如果 $ m \leq n $ 且 $ n \leq p $，那么 $ m \leq p $。
   - **反对称性（Anti-symmetry）**：如果 $ m \leq n $ 且 $ n \leq m $，那么 $ m = n $。
   - **完全性（Totality）**：对于所有的 $ m $ 和 $ n $，$ m \leq n $ 或 $ n \leq m $ 成立。

### 8. **预序、偏序与全序**
   - **预序（Preorder）**：满足自反性和传递性的关系。
   - **偏序（Partial Order）**：满足反对称性的预序。
   - **全序（Total Order）**：满足完全性的偏序。

### 9. **单调性（Monotonicity）**
   - 一个运算符对于一个序关系是单调的，意味着当参数的顺序满足某个序关系时，运算结果的顺序也满足该关系。例如，加法对于 $ \leq $ 是单调的，即：$\forall m, n, p, q \in \mathbb{N}, \, (m \leq n) \text{ 且 } (p \leq q) \Rightarrow (m + p \leq n + q)$

### 10. **严格不等关系（Strict Inequality, $<$）**
   - 严格不等关系表示一个自然数小于另一个自然数，定义为 $ m < n $。该关系可以通过两个推理规则来定义：
     - **起始步骤（Base Case）**：对于所有自然数 $ n $，$ 0 < \text{suc}(n) $ 成立。
     - **归纳步骤（Inductive Case）**：如果 $ m < n $，那么 $ \text{suc}(m) < \text{suc}(n) $ 也成立。

### 11. **非自反性（Irreflexivity）**
   - 非自反性表示对于任意 $ n $，$ n < n $ 不成立。

### 12. **三分律（Trichotomy）**
   - 三分律表明，对于任意自然数 $ m $ 和 $ n $，只能有以下三种情况之一成立：$m < n$，$ m = n $，或 $ m > n $（其中 $ m > n $ 意味着 $ n < m $）。

### 13. **奇数与偶数（Odd and Even Numbers）**
   - 偶数的定义：$ 0 $ 是偶数；如果 $ n $ 是奇数，那么 $ \text{suc}(n) $ 是偶数。
   - 奇数的定义：如果 $ n $ 是偶数，那么 $ \text{suc}(n) $ 是奇数。

### 14. **相互递归（Mutual Recursion）**
   - 相互递归指的是两个或多个定义相互依赖，例如奇数和偶数的定义是相互递归的。

### 15. **重载构造子（Overloaded Constructor）**
   - 重载构造子允许不同的数据类型使用相同的构造子名称。例如，在奇数和偶数的定义中，$ \text{suc} $ 是一个重载构造子，用于多个类型。

### 16. **with 语句**
   - 在 Agda 中，`with` 语句用于对一个表达式进行模式匹配，并根据匹配结果执行不同的代码块。

### 17. **where 语句**
   - `where` 语句用于在 Agda 中定义辅助函数或局部变量，帮助简化主函数的定义。

这些概念在数理逻辑和形式化证明中起到了重要作用，特别是在证明关系和运算的性质时。希望这些解释能帮助您理解本章节的内容。

### ----------------------------------

### 导入

**概念：**
- **基本导入**：为了定义和操作关系，我们需要导入前面章节中使用的相等性（`_≡_`）、自然数（`ℕ`）、加法（`_+_`）等内容。同时，我们还引入了一些自然数的性质，如加法的交换律和右幺元。

**代码：**
```agda
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong)
open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Nat.Properties using (+-comm; +-identityʳ)
```

**解释：**
- `Relation.Binary.PropositionalEquality` 提供了等式相关的基本工具，而 `Data.Nat` 和 `Data.Nat.Properties` 则引入了自然数及其运算的基本性质，为接下来定义关系和证明性质提供基础。

### 定义关系

**概念：**
- **小于等于关系的定义**：`_≤_` 是一个用于表示自然数之间小于等于关系的二元关系。这种关系具有无穷多个实例，但可以通过递归定义的两条推理规则来涵盖所有可能的情况。
  - **起始步骤**：`zero ≤ n` 对于所有自然数 `n` 成立。
  - **归纳步骤**：如果 `m ≤ n` 成立，则 `suc m ≤ suc n` 也成立。

**代码：**
```agda
data _≤_ : ℕ → ℕ → Set where
  z≤n : ∀ {n : ℕ} → zero ≤ n
  s≤s : ∀ {m n : ℕ} → m ≤ n → suc m ≤ suc n
```

**解释：**
- `z≤n` 构造子表明 `zero ≤ n` 的证明对任何自然数 `n` 都成立，而 `s≤s` 构造子则表明对于任意 `m` 和 `n`，如果 `m ≤ n` 成立，那么 `suc m ≤ suc n` 也成立。这样定义确保了我们可以构造出所有自然数之间的 `≤` 关系。

### 隐式参数

**概念：**
- **隐式参数的使用**：在 Agda 中，参数可以被显式或隐式声明。使用花括号 `{}` 的参数为隐式参数，Agda 的类型检查器可以自动推导出这些参数。

**代码：**
```agda
_ : 2 ≤ 4
_ = s≤s (s≤s z≤n)
```

**解释：**
- 在证明 `2 ≤ 4` 时，我们使用了 `s≤s` 和 `z≤n` 构造子。由于参数是隐式的，我们不需要显式写出每个参数，如 `n` 的值。而显式声明隐式参数时，可以使用 `{}` 或 `{name = value}` 的形式。

### 优先级

**概念：**
- **运算符优先级**：定义 `≤` 的优先级，使其在表达式中与其他运算符的结合方式正确。

**代码：**
```agda
infix 4 _≤_
```

**解释：**
- `≤` 的优先级被设置为 4，这确保了表达式如 `1 + 2 ≤ 3` 被解析为 `(1 + 2) ≤ 3`。设置优先级有助于避免不必要的括号并使表达式的语义更明确。

### 可判定性

**概念：**
- **关系的可判定性**：对于给定的两个自然数，是否满足 `≤` 关系是可以直接判定的。这将在后续章节中详细讨论。

**解释：**
- 由于每个自然数之间的大小关系是确定的，因此 `m ≤ n` 可以通过简单的比较直接得出结论，Agda 可以通过这种比较判定任意两个自然数之间的 `≤` 关系。

### 反演

**概念：**
- **反演规则**：有时我们需要从更大的结构反推出更小的结构。例如，从 `suc m ≤ suc n` 推导出 `m ≤ n`。

**代码：**
```agda
inv-s≤s : ∀ {m n : ℕ} → suc m ≤ suc n → m ≤ n
inv-s≤s (s≤s m≤n) = m≤n
```

**解释：**
- `inv-s≤s` 函数说明了如何通过反演 `suc` 的构造子来获得原始的 `≤` 关系。它的应用场景在于我们已经知道 `suc m ≤ suc n`，并希望推导出 `m ≤ n`。

### 序关系的性质

**概念：**
- **序关系的四大性质**：
  - **自反性**：`n ≤ n` 对于所有自然数 `n` 成立。
  - **传递性**：如果 `m ≤ n` 且 `n ≤ p`，则 `m ≤ p`。
  - **反对称性**：如果 `m ≤ n` 且 `n ≤ m`，则 `m ≡ n`。
  - **完全性**：对于任意的 `m` 和 `n`，`m ≤ n` 或 `n ≤ m` 成立。

**解释：**
- 这些性质帮助我们理解关系的基本行为，并为我们提供了判断关系是否是预序、偏序或全序的标准。

### 练习 orderings（实践）

**练习目标：**
- 给出一个例子，说明预序但不是偏序的关系，以及偏序但不是全序的关系。

**代码示例：**
```agda
-- 示例 1：不是偏序的预序
preorder-example : ℕ → ℕ → Set
preorder-example m n = True  -- 自反性和传递性成立，但没有反对称性

-- 示例 2：不是全序的偏序
partial-order-example : ℕ → ℕ → Set
partial-order-example m n = m ≤ n  -- 具有反对称性但不完全
```

**解释：**
- 第一个示例展示了自反性和传递性都成立的关系，但没有反对称性，所以它是一个预序，但不是偏序。第二个示例则展示了反对称性成立，但不完全，所以它是偏序，但不是全序。

### 自反性

**概念：**
- **自反性的证明**：证明对于任意自然数 `n`，`n ≤ n` 成立。

**代码：**
```agda
≤-refl : ∀ {n : ℕ} → n ≤ n
≤-refl {zero} = z≤n
≤-refl {suc n} = s≤s ≤-refl
```

**解释：**
- 使用归纳法进行证明。在起始步骤中，`zero ≤ zero` 由 `z≤n` 证明；在归纳步骤中，归纳假设 `≤-refl {n}` 提供了 `n ≤ n` 的证明，然后使用 `s≤s` 构造出 `suc n ≤ suc n` 的证明。

### 传递性

**概念：**
- **传递性的证明**：对于任意的自然数 `m`、`n` 和 `p`，如果 `m ≤ n` 且 `n ≤ p` 成立，那么 `m ≤ p` 成立。

**代码：**
```agda
≤-trans : ∀ {m n p : ℕ} → m ≤ n → n ≤ p → m ≤ p
≤-trans z≤n _ = z≤n
≤-trans (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans m≤n n≤p)
```

**解释：**
- 对 `m ≤ n` 进行归纳。在起始步骤中，`m` 为 `zero`，因此 `m ≤ p` 由 `z≤n` 证明。在归纳步骤中，利用归纳假设 `≤-trans m≤n n≤p` 可以推导出 `m ≤ p`，并通过 `s≤s` 构造出 `suc m ≤ suc p` 的证明。

### 反对称性

**概念：**
- **反对称性的证明**：如果 `m ≤ n` 且 `n ≤ m` 成立，那么 `m ≡ n`。

**代码：**
```agda
≤-antisym : ∀ {m n : ℕ} → m ≤ n → n ≤ m → m ≡ n
≤-antisym z≤n z≤n = refl
≤-antisym (s≤s m≤n) (s≤s n≤m) = cong suc (≤-antisym m≤n n≤m)
```

**解释：**
- 对 `m ≤ n` 和 `n ≤ m` 进行归纳。在起始步骤中，`m` 和 `n` 都为 `zero`，因此 `

m ≡ n` 由 `refl` 证明。在归纳步骤中，利用归纳假设 `≤-antisym m≤n n≤m` 证明 `m ≡ n`，然后通过 `cong suc` 推导出 `suc m ≡ suc n`。

### 完全性

**概念：**
- **完全性的证明**：对于任意自然数 `m` 和 `n`，要么 `m ≤ n`，要么 `n ≤ m` 成立。

**代码：**
```agda
data Total (m n : ℕ) : Set where
  forward : m ≤ n → Total m n
  flipped : n ≤ m → Total m n

≤-total : ∀ (m n : ℕ) → Total m n
≤-total zero n = forward z≤n
≤-total (suc m) zero = flipped z≤n
≤-total (suc m) (suc n) with ≤-total m n
... | forward m≤n = forward (s≤s m≤n)
... | flipped n≤m = flipped (s≤s n≤m)
```

**解释：**
- 通过对 `m` 和 `n` 进行递归，证明在每个情况下要么 `m ≤ n`，要么 `n ≤ m`。`Total` 数据类型用来封装这两种可能性：`forward` 代表 `m ≤ n`，而 `flipped` 代表 `n ≤ m`。

### 单调性

**概念：**
- **单调性的证明**：加法对小于等于关系是单调的，即如果 `m ≤ n` 且 `p ≤ q`，那么 `m + p ≤ n + q`。

**代码：**
```agda
+-monoʳ-≤ : ∀ (n p q : ℕ) → p ≤ q → n + p ≤ n + q
+-monoʳ-≤ zero p q p≤q = p≤q
+-monoʳ-≤ (suc n) p q p≤q = s≤s (+-monoʳ-≤ n p q p≤q)

+-monoˡ-≤ : ∀ (m n p : ℕ) → m ≤ n → m + p ≤ n + p
+-monoˡ-≤ m n p m≤n rewrite +-comm m p | +-comm n p = +-monoʳ-≤ p m n m≤n

+-mono-≤ : ∀ (m n p q : ℕ) → m ≤ n → p ≤ q → m + p ≤ n + q
+-mono-≤ m n p q m≤n p≤q = ≤-trans (+-monoˡ-≤ m n p m≤n) (+-monoʳ-≤ n p q p≤q)
```

**解释：**
- 分别证明了加法在右侧和左侧对小于等于关系的单调性，最后将两者结合起来证明整体的单调性。

### 严格不等关系

**概念：**
- **严格不等关系的定义**：严格不等关系 `_<_` 是 `≤` 关系的强化版，只在 `m < n` 时成立。

**代码：**
```agda
data _<_ : ℕ → ℕ → Set where
  z<s : ∀ {n : ℕ} → zero < suc n
  s<s : ∀ {m n : ℕ} → m < n → suc m < suc n
```

**解释：**
- `zero` 只能小于 `suc n`，而 `suc m` 只有在 `m < n` 时才会小于 `suc n`。严格不等关系不能自反，但具有传递性和其他相似性质。

这一章节主要介绍了自然数之间关系的定义及其性质，并通过归纳法证明了多个性质。了解这些概念对于后续章节的逻辑推导和复杂性质的证明至关重要。



### 严格不等关系的性质

**概念：**
- **严格不等关系的性质**：与不等关系类似，严格不等关系也是传递的，即如果 $m < n$ 且 $n < p$，那么 $m < p$ 也成立。此外，严格不等关系具有非自反性（Irreflexive），即对于任何自然数 $n$，$n < n$ 不成立。严格不等关系还满足三分律（Trichotomy），即对于任意自然数 $m$ 和 $n$，$m < n$，$m \equiv n$，或 $m > n$ 三者必有其一成立。

**练习：**
- **练习 `<-trans` （推荐）**：
  - 证明严格不等关系的传递性，即如果 $m < n$ 且 $n < p$，那么 $m < p$ 成立。

**代码示例：**
```agda
<-trans : ∀ {m n p : ℕ} → m < n → n < p → m < p
<-trans (z<s {n}) (s<s m<n) = z<s
<-trans (s<s m<n) (s<s n<p) = s<s (<-trans m<n n<p)
```

**解释：**
- 传递性证明分两种情况处理：
  - 如果第一个不等关系是 `zero < suc n`，那么直接得到 `zero < suc p`，这由 `z<s` 构造子保证。
  - 如果两个不等关系都是由 `suc` 构造子引入的，那么通过递归调用 `<-trans`，证明 `m < p`。

### 三分律（Trichotomy）

**概念：**
- **三分律的定义**：对于任意两个自然数 $m$ 和 $n$，三种情况之一必须成立：$m < n$，$m \equiv n$，或 $m > n$。这里，$m > n$ 定义为 $n < m$。

**练习：**
- **练习 `trichotomy`（实践）**：
  - 证明严格不等关系满足弱化的三分律，即对于任意 $m$ 和 $n$，$m < n$，$m \equiv n$，或 $m > n$ 三者必有其一成立。

**代码示例：**
```agda
data Trichotomy (m n : ℕ) : Set where
  lt : m < n → Trichotomy m n
  eq : m ≡ n → Trichotomy m n
  gt : n < m → Trichotomy m n

trichotomy : ∀ (m n : ℕ) → Trichotomy m n
trichotomy zero zero = eq refl
trichotomy zero (suc n) = lt z<s
trichotomy (suc m) zero = gt z<s
trichotomy (suc m) (suc n) with trichotomy m n
... | lt m<n = lt (s<s m<n)
... | eq m≡n = eq (cong suc m≡n)
... | gt n<m = gt (s<s n<m)
```

**解释：**
- `Trichotomy` 数据类型封装了三种可能性：`m < n`，`m ≡ n`，或 `m > n`。`trichotomy` 函数对自然数的不同情况进行模式匹配和递归判断，确保三者之一成立。

### 加法对严格不等关系的单调性

**概念：**
- **加法的单调性**：类似于 `≤`，加法对于严格不等关系也是单调的。也就是说，如果 $m < n$ 且 $p < q$，那么 $m + p < n + q$。

**练习：**
- **练习 `+-mono-<`（实践）**：
  - 证明加法对于严格不等关系是单调的，即如果 $m < n$ 且 $p < q$，那么 $m + p < n + q$。

**代码示例：**
```agda
+-monoʳ-< : ∀ {n p q : ℕ} → p < q → n + p < n + q
+-monoʳ-< {zero} {p} {q} p<q = p<q
+-monoʳ-< {suc n} {p} {q} p<q = s<s (+-monoʳ-< p<q)

+-monoˡ-< : ∀ {m n p : ℕ} → m < n → m + p < n + p
+-monoˡ-< m<n = rewrite +-comm m p | +-comm n p in +-monoʳ-< m<n

+-mono-< : ∀ {m n p q : ℕ} → m < n → p < q → m + p < n + q
+-mono-< m<n p<q = ≤-trans (+-monoˡ-< m<n) (+-monoʳ-< p<q)
```

**解释：**
- 分别证明加法在右侧和左侧对严格不等关系的单调性，然后将两者结合，使用传递性最终证明加法的整体单调性。

### 证明 `suc m ≤ n` 与 `m < n` 的等价性

**概念：**
- **等价性**：证明 `suc m ≤ n` 等价于 `m < n`，即如果 `suc m ≤ n`，那么 $m < n$；反之亦然。

**练习：**
- **练习 `≤-iff-<` （推荐）**：
  - 证明 `suc m ≤ n` 蕴涵了 $m < n$，及其逆命题。

**代码示例：**
```agda
≤-iff-< : ∀ {m n : ℕ} → suc m ≤ n ↔ m < n
≤-iff-< = record
  { to   = λ { .(suc n) → λ x → x }
  ; from = λ x → s≤s x
  }
```

**解释：**
- 通过证明 `suc m ≤ n` 和 `m < n` 两者互为蕴涵关系，证明了这两个关系的等价性。具体实现使用了 Agda 的 `record` 语法构造等价性。

### 奇数和偶数的定义

**概念：**
- **奇数和偶数**：奇偶性是一种一元关系，表示一个自然数是否为奇数或偶数。奇偶性的定义是相互递归的，一个数是偶数当且仅当它是 0 或是奇数的后继；一个数是奇数当且仅当它是偶数的后继。

**代码：**
```agda
data even : ℕ → Set where
  zero : even zero
  suc  : ∀ {n : ℕ} → odd n → even (suc n)

data odd : ℕ → Set where
  suc : ∀ {n : ℕ} → even n → odd (suc n)
```

**解释：**
- `even` 类型表示偶数，其中 `zero` 表示 0 是偶数，而 `suc` 构造子表示如果 `n` 是奇数，则 `suc n` 是偶数。类似地，`odd` 类型表示奇数。

### 证明奇偶性质

**练习：**
- **练习 `o+o≡e` （延伸）**：
  - 证明两个奇数之和为偶数。

**代码示例：**
```agda
o+o≡e : ∀ {m n : ℕ} → odd m → odd n → even (m + n)
o+o≡e (suc em) (suc en) = suc (o+o≡e em en)
```

**解释：**
- 通过递归证明，证明两个奇数之和为偶数。该证明利用了奇数与偶数的相互递归定义，并通过 `suc` 构造子进行归纳推理。

### 标准库

**概念：**
- **标准库中的对应定义**：Agda 标准库提供了与本章节定义类似的性质和运算符。

**代码：**
```agda
import Data.Nat using (_≤_; z≤n; s≤s)
import Data.Nat.Properties using (≤-refl; ≤-trans; ≤-antisym; ≤-total;
                                  +-monoʳ-≤; +-monoˡ-≤; +-mono-≤)
```

**解释：**
- 通过引用 Agda 标准库的定义，可以直接使用标准库中的预定义函数和性质，避免重复定义。

### Unicode

**概念：**
- **Unicode 符号**：在本章中使用了许多 Unicode 符号来表示不同的数学运算符和标识符。

**符号表：**
- `≤`  U+2264 