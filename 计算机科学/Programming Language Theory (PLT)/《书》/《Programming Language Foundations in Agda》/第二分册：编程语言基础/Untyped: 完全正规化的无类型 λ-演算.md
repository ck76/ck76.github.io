[toc]

### 导入

在这一节，我们首先导入了多个需要使用的模块，为后续的定义和推理打下基础。

- `Relation.Binary.PropositionalEquality` 模块用于处理命题等式，它使我们能够证明和操作 Agda 中的等式。
- `Data.Empty` 模块用于处理空类型，它表示一种无法构造的类型。
- `Data.Nat` 模块提供了自然数的基本操作，如零 (`zero`)、后继 (`suc`) 等。
- `Relation.Nullary` 和 `Relation.Nullary.Decidable` 模块处理了 Agda 中的布尔逻辑和可判定的关系。

通过导入这些模块，我们确保了后续的代码可以方便地使用这些基本工具。

### 无类型即是单一类型

**概念**: 在这一节中，我们提出了一个核心理念，即“无类型即是单一类型”，这意味着在无类型演算中，所有项都可以视为同一种类型。

**解释**: 
- 在无类型 $\lambda$-演算中，每个项都可以被认为是同一种类型，这种类型通常记为 $★$。这与 Dana Scott 和 Robert Harper 提出的观点一致：“无类型即是单一类型”。
- 在这种情况下，所有项的类型检查可以简化为检查项的作用域，而不需要具体区分项的类型。

**公式**: 
- 定义唯一的类型 $★$：
  $$\text{data Type : Set where}$$
  $$★ : Type$$

这种设计的一个重要后果是，以前需要额外给出的构造（如自然数和不动点），现在可以直接在语言中定义。

### 语法

**概念**: 在这一节中，我们定义了无类型 $\lambda$-演算的基本语法。

**解释**:
- 在无类型 $\lambda$-演算中，项由变量、抽象（$\lambda$-表达式）和应用三部分组成。
- 因为没有类型区分，所有项的类型都被视为 $★$。

**公式**:
- 定义中缀符号和基本语法：
  - 变量：$`x$ 表示在语境中查找变量 $x$。
  - 抽象：$\lambda x \Rightarrow N$ 表示对变量 $x$ 进行抽象，抽象体为 $N$。
  - 应用：$L \cdot M$ 表示将项 $L$ 应用于项 $M$。

这些基本构造定义了无类型 $\lambda$-演算的语法。

### 类型

**概念**: 在这一节中，我们定义了唯一的类型 $★$。

**解释**:
- 无类型 $\lambda$-演算中，所有项的类型都是 $★$，即所有项都被视为相同的类型。

**公式**:
- 定义唯一的类型 $★$：
  $$\text{data Type : Set where}$$
  $$★ : Type$$

由于所有项都具有相同的类型，类型检查的复杂性大大降低。

#### 练习 (Type≃⊤) （习题）

**概念**: 证明类型 $★$ 与单元类型 $\top$ 同构。

**解释**:
- 这个习题要求我们证明在无类型 $\lambda$-演算中，类型 $★$ 与单元类型 $\top$ 是等价的，这进一步表明了“无类型即是单一类型”的理念。

**公式**:
- 证明 $\text{Type}$ 和 $\top$ 同构：
  $$Type \cong \top$$

这可以通过定义一个从 $\text{Type}$ 到 $\top$ 的同构函数来实现。

### 语境

**概念**: 在这一节中，我们定义了无类型 $\lambda$-演算中的语境。

**解释**:
- 语境（Context）是一个类型的列表，其中最新出现的类型在最右边。语境用于存储在当前范围内有效的变量及其对应的类型。

**公式**:
- 语境的定义：
  $$\text{data Context : Set where}$$
  $$\emptyset : Context$$
  $$\Gamma, \,A : Context \rightarrow Type \rightarrow Context$$

语境允许我们跟踪变量的作用域并确保它们在推导过程中得到正确的处理。

#### 练习 (Context≃ℕ) （习题）

**概念**: 证明语境（Context）与自然数 $\mathbb{N}$ 同构。

**解释**:
- 这个习题要求我们证明在无类型 $\lambda$-演算中，语境和自然数是等价的。这表明语境的长度可以用自然数来表示。

**公式**:
- 证明 $\text{Context}$ 和 $\mathbb{N}$ 同构：
  $$\text{Context} \cong \mathbb{N}$$

这可以通过定义一个从 $\text{Context}$ 到 $\mathbb{N}$ 的同构函数来实现。

### 变量和查询判断

**概念**: 在这一节中，我们定义了变量及其在语境中的查询规则。

**解释**:
- 变量的定义与查询判断（Judgment）的定义紧密相关。查询判断用于检查一个变量是否在语境中，并返回该变量的类型。
- 由于无类型演算中只有一种类型 $★$，查询判断的主要任务是检查变量是否在语境中，而不涉及具体的类型区分。

**公式**:
- 变量和查询判断的定义：
  $$\text{data} \, \Gamma \ni A \, \text{where}$$
  $$Z : \Gamma, A \ni A$$
  $$S : \Gamma \ni A \rightarrow \Gamma, B \ni A$$

这表明在无类型 $\lambda$-演算中，变量的查询操作仅涉及语境的处理。

### 项与作用域判断

**概念**: 在这一节中，我们定义了项与其作用域的判断规则。

**解释**:
- 作用域判断用于验证一个项是否在当前语境中是有效的。
- 在无类型演算中，作用域判断确保所有使用的变量在语境内，而不是具体的类型判断。

**公式**:
- 项与作用域判断的定义：
  $$\text{data} \, \Gamma \vdash A \, \text{where}$$
  $$`x : \Gamma \ni A \rightarrow \Gamma \vdash A$$
  $$\lambda x \Rightarrow N : \Gamma, ★ \vdash ★ \rightarrow \Gamma \vdash ★$$
  $$L \cdot M : \Gamma \vdash ★ \rightarrow \Gamma \vdash ★ \rightarrow \Gamma \vdash ★$$

作用域判断通过语境和项的结构来验证项的有效性。

### 用数表示变量

**概念**: 在这一节中，我们展示了如何使用自然数来表示变量。

**解释**:
- 自然数可以用于表示 de Bruijn 因子，从而简化了变量的表示和处理。在这种表示方法下，变量的索引是基于语境中的位置。

**公式**:
- 使用自然数表示变量的定义：
  $$\text{length} : \text{Context} \rightarrow \mathbb{N}$$
  $$\text{count} : \forall \{\Gamma\} \rightarrow \{\text{n} : \mathbb{N}\} \rightarrow (\text{p} : \text{n} < \text{length} \, \Gamma) \rightarrow \Gamma \ni ★$$

通过这种表示方法，可以更方便地操作变量和查询它们的位置。

### 测试例子

**概念**: 在这一节中，我们给出了使用无类型 $\lambda$-演算的一些具体例子。

**解释**:
- 这些例子展示了如何定义和操作无类型 $\lambda$-演算中的项，包括 Church 数的定义和加法操作。

**公式**:
- Church 数的定义：
  $$\text{twoᶜ} : \forall \{\Gamma\} \rightarrow \Gamma \vdash ★$$
  $$\text{twoᶜ} = \lambda \lambda (\#1 \cdot (\#1 \cdot \#0))$$

这些例子验证了无类型 $\lambda$-演算的表达能力。

### 重命名

**概念**: 在这一节中，我们定义了无类型 $\lambda$-演算中的重命名操作。

**解释**:
- 重命名操作用于改变项中变量的名称，同时保持项的结构和语义不变。
- 这一操作通常用于证明和操作项的等价性。

**公式**:
- 重命名的定义：
  $$\text{rename} : \forall \{\Gamma \Delta\} \rightarrow (\forall \{A\} \rightarrow \Gamma \ni A \rightarrow \Delta \ni A) \rightarrow (\forall \{A\} \rightarrow \Gamma \vdash A \rightarrow \Delta \vdash A)$$
  
- 扩充引理：
  $$\text{ext}

 : \forall \{\Gamma \Delta\} \rightarrow (\forall \{A\} \rightarrow \Gamma \ni A \rightarrow \Delta \ni A) \rightarrow (\forall \{A B\} \rightarrow \Gamma, B \ni A \rightarrow \Delta, B \ni A)$$

通过重命名操作，我们可以确保项在不同语境中的一致性。

### 同时替换

**概念**: 在这一节中，我们定义了无类型 $\lambda$-演算中的同时替换操作。

**解释**:
- 同时替换操作允许我们在项中同时替换多个变量。这对于处理复杂的项结构和证明等式是非常有用的。

**公式**:
- 同时替换的定义：
  $$\text{subst} : \forall \{\Gamma \Delta\} \rightarrow (\forall \{A\} \rightarrow \Gamma \ni A \rightarrow \Delta \vdash A) \rightarrow (\forall \{A\} \rightarrow \Gamma \vdash A \rightarrow \Delta \vdash A)$$
  
- 扩充引理：
  $$\text{exts} : \forall \{\Gamma \Delta\} \rightarrow (\forall \{A\} \rightarrow \Gamma \ni A \rightarrow \Delta \vdash A) \rightarrow (\forall \{A B\} \rightarrow \Gamma, B \ni A \rightarrow \Delta, B \vdash A)$$

这种替换操作在处理需要替换多个变量的情况下非常有用。

### 单个替换

**概念**: 在这一节中，我们定义了无类型 $\lambda$-演算中替换单个变量的操作。

**解释**:
- 单个替换操作是同时替换操作的特例，它用于替换项中的一个自由变量。

**公式**:
- 单个替换的定义：
  $$\text{subst-zero} : \forall \{\Gamma B\} \rightarrow (\Gamma \vdash B) \rightarrow \forall \{A\} \rightarrow (\Gamma, B \ni A) \rightarrow (\Gamma \vdash A)$$
  
- 替换一个自由变量：
  $$\_[_] : \forall \{\Gamma A B\} \rightarrow \Gamma, B \vdash A \rightarrow \Gamma \vdash B \rightarrow \Gamma \vdash A$$

单个替换操作使得我们可以方便地操作项中的自由变量。

### 中性项和范式

**概念**: 在这一节中，我们定义了无类型 $\lambda$-演算中的中性项（Neutral Term）和范式（Normal Form）。

**解释**:
- 中性项表示那些无法再继续归约的项，例如变量或应用（未完全归约的）。
- 范式表示完全归约后的项，范式项不能再进行进一步的归约。

**公式**:
- 中性项的定义：
  $$\text{data Neutral : \forall \{Γ A\} → Γ ⊢ A → Set where}$$
  
- 范式的定义：
  $$\text{data Normal : \forall \{Γ A\} → Γ ⊢ A → Set where}$$

通过定义中性项和范式，我们可以区分哪些项可以继续归约，哪些已经是最终结果。

---

到这里，我们已经涵盖了“Untyped: 完全正规化的无类型 λ-演算”章节中的前半部分内容。接下来，我会继续解释剩下的部分。

### 归约步骤

**概念**: 在这一节中，我们定义了无类型 $\lambda$-演算中的归约规则。这些规则描述了如何从一个项一步步地归约到另一个项。

**解释**:
- 归约规则包括以下几种：
  1. **$\xi_1$ 规则**：如果函数部分可以归约，那么整个应用项可以归约。
  2. **$\xi_2$ 规则**：如果参数部分可以归约，那么整个应用项可以归约。
  3. **$\beta$ 规则**：如果一个 $\lambda$-抽象应用于一个项，那么可以进行 $\beta$ 归约。
  4. **$\zeta$ 规则**：允许在 $\lambda$-抽象的体内进行归约。

**公式**:
- 归约步骤的定义：
  $$\text{data} \, \Gamma \vdash A \rightarrow \Gamma \vdash A \rightarrow \text{Set} \, \text{where}$$
  $$\xi_1 : \forall \{\Gamma\} \{L L' M : \Gamma \vdash ★\} \rightarrow L —→ L' \rightarrow L \cdot M —→ L' \cdot M$$
  $$\xi_2 : \forall \{\Gamma\} \{L M M' : \Gamma \vdash ★\} \rightarrow M —→ M' \rightarrow L \cdot M —→ L \cdot M'$$
  $$\beta : \forall \{\Gamma\} \{N : \Gamma, ★ \vdash ★\} \{M : \Gamma \vdash ★\} \rightarrow (\lambda N) \cdot M —→ N[M]$$
  $$\zeta : \forall \{\Gamma\} \{N N' : \Gamma, ★ \vdash ★\} \rightarrow N —→ N' \rightarrow \lambda N —→ \lambda N'$$

这些归约规则定义了如何一步步地简化和求值无类型 $\lambda$-演算中的项。

#### 练习 (variant-1) （习题）

**概念**: 思考如何在传值调用（Call-by-Value）下修改规则，使得项在其中归约至范式。

**解释**:
- 这个习题要求我们考虑如何修改归约规则，使得在传值调用策略下，项只在它们是范式时归约。这意味着 $\beta$ 归约只能在两个项都是范式时进行。

**公式**:
- 在传值调用策略下修改的规则：
  $$\text{data} \, \Gamma \vdash A \rightarrow \Gamma \vdash A \rightarrow \text{Set} \, \text{where}$$
  $$\beta : \text{当} \, L \, \text{和} \, M \, \text{都是范式时，进行} \, \beta \, \text{归约}$$

#### 练习 (variant-2) （习题）

**概念**: 思考如何在传值调用下修改规则，使得项不在 $\lambda$-抽象之下归约。

**解释**:
- 这个习题要求我们修改归约规则，使得项在 $\lambda$-抽象之下不再归约。这种情况下，$\beta$ 归约只在函数和参数都是值（即 $\lambda$-抽象）时进行。

**公式**:
- 在传值调用策略下修改的规则：
  $$\text{data} \, \Gamma \vdash A \rightarrow \Gamma \vdash A \rightarrow \text{Set} \, \text{where}$$
  $$\beta : \text{当} \, L \, \text{和} \, M \, \text{都是值时，进行} \, \beta \, \text{归约}$$

### 自反传递闭包

**概念**: 在这一节中，我们定义了自反传递闭包，表示归约关系的多步闭包。

**解释**:
- 自反传递闭包表示一个项可以通过多次归约从一个状态转移到另一个状态。这个关系是传递的，并且包括零步（自反）和多步（传递）情况。

**公式**:
- 自反传递闭包的定义：
  $$\text{data} \, \Gamma \vdash A \rightarrow \Gamma \vdash A \rightarrow \text{Set} \, \text{where}$$
  $$L ∎ : L \text{可以归约到} L \text{本身}$$
  $$\text{step—→} : (L : \Gamma \vdash A) \{M N : \Gamma \vdash A\} \rightarrow M —↠ N \rightarrow L —→ M \rightarrow L —↠ N$$

通过定义自反传递闭包，我们可以更方便地处理多步归约问题。

### 归约序列的例子

**概念**: 在这一节中，我们展示了一个归约序列的具体例子，即二加二得四的归约过程。

**解释**:
- 这个例子展示了如何使用我们定义的归约规则和自反传递闭包来一步步地归约二加二（表示为 Church 数）得到四的过程。

**公式**:
- 归约序列的定义和展示：
  $$2+2ᶜ —↠ fourᶜ$$
  $$\begin{aligned} 
    &\text{begin} \, \text{plusᶜ} \cdot \text{twoᶜ} \cdot \text{twoᶜ} \\
    &—→⟨ \xi_1 \, \beta ⟩ \\
    &\text{...} \\
    &—→⟨ \zeta \, (\zeta \, \beta) ⟩ \\
    &\text{...}
  \end{aligned}$$

这个例子清晰地展示了归约序列如何在多步归约过程中推进。

### 可进性

**概念**: 在这一节中，我们定义了项的可进性。

**解释**:
- 可进性是指一个项要么可以进一步归约，要么已经达到范式（无法继续归约）。在无类型 $\lambda$-演算中，可进性确保了每个项最终都能归约到一个范式。

**公式**:
- 可进性的定义：
  $$\text{data Progress :} \, \Gamma \vdash A \rightarrow \text{Set} \, \text{where}$$
  $$\text{step} : \forall \{N : \Gamma \vdash A\} \rightarrow M —→ N \rightarrow \text{Progress} \, M$$
  $$\text{done} : \text{Normal} \, M \rightarrow \text{Progress} \, M$$

可进性保证了无类型 $\lambda$-演算中的每个项都能最终达到一个不可继续归约的状态。

### 求值

**概念**: 在这一节中，我们定义了无类型 $\lambda$-演算中的求值器。

**解释**:
- 求值器根据可进性对一个项进行归约，直到无法继续归约（达到范式）或耗尽资源（如“汽油”）。

**公式**:
- 求值器的定义：
  $$\text{eval} : \forall \{\Gamma A\} \rightarrow \text{Gas} \rightarrow (\Gamma \vdash A) \rightarrow \text{Steps} \, L$$

求值器通过递归调用，将项一步步地归约到范式或提示汽油耗尽。

### 例子

**概念**: 在这一节中，我们展示了求值器的一个具体例子，即二加二得四的求值过程。

**解释**:
- 这个例子展示了如何使用求值器对一个表示二加二的项进行求值，并得到范式四。

**公式**:
- 例子的展示：
  $$\text{eval} \, \text{(gas 100)} \, 2+2ᶜ ≡ \text{steps} \, (\lambda \lambda (\lambda \lambda (\# (S Z)) \cdot (\# Z)) \cdot (\lambda \lambda (\# Z)) \cdot (\lambda \lambda (\# Z)) ... ∎)$$

这个例子验证了求值器的正确性和计算能力。

### 自然数和不动点

**概念**: 在这一节中，我们展示了如何使用 Scott 数表示自然数，以及如何定义不动点运算符。

**解释**:
- Scott 数是一种用于表示自然数的编码方式，允许我们对数进行分情况讨论。
- 不动点运算符允许我们在 $\lambda$-演算中定义递归函数。

**公式**:
- Scott 数的定义：
  $$\text{data} \, \Gamma \vdash A \rightarrow \text{Set} \, \

text{where}$$
  $$\text{`zero} : \forall \{\Gamma\} \rightarrow (\Gamma \vdash ★)$$
  $$\text{`suc_} : \forall \{\Gamma\} \rightarrow (\Gamma \vdash ★) \rightarrow (\Gamma \vdash ★)$$
  $$\text{case} : \forall \{\Gamma\} \rightarrow (\Gamma \vdash ★) \rightarrow (\Gamma \vdash ★) \rightarrow (\Gamma, ★ \vdash ★) \rightarrow (\Gamma \vdash ★)$$

- 不动点运算符的定义：
  $$\mu_ : \forall \{\Gamma\} \rightarrow (\Gamma, ★ \vdash ★) \rightarrow (\Gamma \vdash ★)$$

使用 Scott 数和不动点运算符，我们可以在无类型 $\lambda$-演算中表示复杂的自然数运算和递归函数。

#### 练习 plus-eval （实践）

**概念**: 使用求值器，验证 plus · two · two 和 four 归约到相同的范式。

#### 练习 multiplication-untyped （推荐）

**概念**: 使用 Scott 表示法和编码后的不动点运算符，定义乘法运算并验证二乘二得四。

#### 练习 encode-more （延伸）

**概念**: 使用无类型 $\lambda$-演算编码 More 章节中的其他构造，除了原语数字之外。

### 多步归约是传递的

**概念**: 在这一节中，我们证明了多步归约的传递性。

**解释**:
- 多步归约的传递性意味着，如果项 $L$ 可以归约到 $M$，并且 $M$ 可以归约到 $N$，那么 $L$ 可以直接归约到 $N$。

**公式**:
- 传递性的定义：
  $$L —↠⟨ L—↠M ⟩ M—↠N = —↠-trans L—↠M M—↠N$$

通过证明传递性，我们可以更加简洁地描述多步归约过程。

### 多步归约是合同性的

**概念**: 在这一节中，我们证明了多步归约对于项构造子（如 $\lambda$ 抽象和应用）的合同性。

**解释**:
- 合同性意味着如果两个项 $L$ 和 $L'$ 满足多步归约关系，那么将它们应用于相同的参数后，结果仍然满足多步归约关系。

**公式**:
- 合同性的定义和证明：
  $$\text{appL-cong} : \forall \{\Gamma\} \{L L' M : \Gamma \vdash ★\} \rightarrow L —↠ L' \rightarrow L \cdot M —↠ L' \cdot M$$
  $$\text{appR-cong} : \forall \{\Gamma\} \{L M M' : \Gamma \vdash ★\} \rightarrow M —↠ M' \rightarrow L \cdot M —↠ L \cdot M'$$
  $$\text{abs-cong} : \forall \{\Gamma\} \{N N' : \Gamma, ★ \vdash ★\} \rightarrow N —↠ N' \rightarrow \lambda N —↠ \lambda N'$$

通过证明合同性，我们确保了多步归约过程中的一致性和可靠性。

### Unicode

**概念**: 这一节简单列出了本章中使用的 Unicode 符号。

**解释**:
- 本章使用的主要符号是黑星号（★），用于表示唯一的类型。

---

至此，我们已经涵盖了“Untyped: 完全正规化的无类型 λ-演算”章节的所有内容。每一小节都详细讲解了涉及的概念，并配以公式加以解释，以确保对内容的全面理解。