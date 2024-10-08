[toc]

好的，我们从头开始，按照原文的结构进行详细解释。我们会逐节进行，以确保不遗漏任何内容。

### 绪论：推理规则作为算法

**概念**: 在这一节中，我们探讨了推理规则作为算法的概念。给定一个项，如何通过推理规则来确定它的类型是这一节的核心。

**解释**: 在前面的章节中，类型推导是直接给出的，但在实际应用中，我们通常使用算法来推理出项的类型。这里引入了一种称为**双向类型推理（Bidirectional Type Inference）**的技术，它允许我们从类型推导中推理出项的类型。双向类型推理包括两种主要判断：

1. **生成（Synthesise）**: 根据项推理出类型。
2. **继承（Inherit）**: 验证项是否符合给定的类型。

**公式**: 例如，对于一个简单的 λ 演算项 $ \lambda x \colon A \to N $，我们可以根据输入类型 $ A $ 推导出结果类型 $ B $。这种推理过程可以形式化为算法规则：
$$
\frac{ \Gamma, x \colon A \vdash N \colon B }{ \Gamma \vdash (\lambda x \colon A \to N) \colon (A \to B) }
$$

### 生成和继承类型

**概念**: 这一节定义了**生成**和**继承**类型的概念，以及它们在双向类型推理中的应用。

**解释**: 
- **生成类型（Synthesise Types）**: 当我们知道项，并希望推理出其类型时，我们使用生成类型的规则。生成类型的判断形式为 $ \Gamma \vdash M \uparrow A $，表示在语境 $\Gamma$ 下，项 $M$ 推导出类型 $A$。
- **继承类型（Inherit Types）**: 当我们已经知道项的类型，并希望验证项是否符合这一类型时，我们使用继承类型的规则。继承类型的判断形式为 $ \Gamma \vdash M \downarrow A $，表示在语境 $\Gamma$ 下，项 $M$ 继承了类型 $A$。

**公式**: 生成和继承类型的规则可以表示为：

$$
\frac{ \Gamma \vdash M \uparrow A }{ \Gamma \vdash M \downarrow A }
$$

这个公式表示如果我们能够生成类型 $A$，那么 $M$ 也可以继承类型 $A$。

### 可靠性和完备性

**概念**: 讨论了双向类型推理的**可靠性**和**完备性**。

**解释**: 
- **可靠性（Soundness）**: 如果生成类型的推导成功，那么它推导出的类型一定是正确的。
- **完备性（Completeness）**: 如果生成类型的推导失败，那么一定不存在对应的类型。

**公式**: 这两个性质可以形式化为：

$$
\text{Soundness}: \quad \text{If } \Gamma \vdash M \uparrow A \text{ then } \Gamma \vdash M \downarrow A
$$
$$
\text{Completeness}: \quad \text{If } \Gamma \not\vdash M \uparrow A \text{ then } \Gamma \not\vdash M \downarrow A
$$

这表明双向类型推理不仅可以正确推导类型，而且能够识别出推导失败的情况。

### 导入

**概念**: 导入了与类型推理相关的各种基础模块和定义。

**解释**: 在这一部分，我们导入了处理类型、语境、以及判断的模块。这些模块包括类型的等价性检查、数据类型的定义，以及逻辑操作的基础设施。这些导入为后续的推理规则提供了必要的基础。

### 语法

**概念**: 定义了用于双向类型推理的项的语法。

**解释**: 这一节定义了项的语法，将其分为两类：
- **生成项（Term⁺）**: 通过生成类型来推理其类型的项，如变量、应用、和类型转换。
- **继承项（Term⁻）**: 通过继承类型来验证其类型的项，如 λ 抽象、零、后继、分情况讨论、不动点等。

**公式**: 语法规则如下：

$$
L^+, M^+, N^+ ::= x \mid L^+ \cdot M^- \mid M^- \downarrow A
$$
$$
L^-, M^-, N^- ::= \lambda x \Rightarrow N^- \mid 0 \mid \text{suc} M^- \mid \text{case } L^+ \, [0 \Rightarrow M^- \mid \text{suc} x \Rightarrow N^- ] \mid \mu x \Rightarrow N^- \mid M^+ \uparrow
$$

### 项的例子

**概念**: 给出了双向类型推理中的实际项的例子。

**解释**: 这里展示了如何在具体的例子中应用生成和继承类型的规则。通过这些例子，我们可以更好地理解如何在实际情况下使用双向类型推理。

**例子**: 一个例子是二加二的计算，另一个是 Church 数的二加二计算。每个例子都展示了如何在上下文中使用生成和继承类型。

如果你希望继续详细解释剩余部分，请告诉我。



### 双向类型检查

**概念**: 在这一节中，定义了双向类型检查的规则，包括如何在给定语境下，对生成项和继承项进行类型检查。

**解释**: 
- **生成类型检查**: 对于生成项（Term⁺），我们在给定语境 $\Gamma$ 和项 $M$ 的情况下，推导出它的类型 $A$。这部分推理的核心是从语境中找到变量的类型，并根据项的结构推导出结果类型。
- **继承类型检查**: 对于继承项（Term⁻），我们在给定语境 $\Gamma$、项 $M$ 和预期类型 $A$ 的情况下，验证 $M$ 是否符合类型 $A$。

**公式**: 双向类型检查的规则可以形式化为：

- **生成类型检查规则**:
  $$
  \frac{ \Gamma \ni x \colon A }{ \Gamma \vdash `x \uparrow A }
  $$
  $$
  \frac{ \Gamma \vdash L \uparrow (A \Rightarrow B) \quad \Gamma \vdash M \downarrow A }{ \Gamma \vdash L \cdot M \uparrow B }
  $$
  $$
  \frac{ \Gamma \vdash M \downarrow A }{ \Gamma \vdash M \downarrow A \uparrow A }
  $$

- **继承类型检查规则**:
  $$
  \frac{ \Gamma, x \colon A \vdash N \downarrow B }{ \Gamma \vdash \lambda x \Rightarrow N \downarrow (A \Rightarrow B) }
  $$
  $$
  \frac{ }{ \Gamma \vdash 0 \downarrow \text{`Nat} }
  $$
  $$
  \frac{ \Gamma \vdash M \downarrow \text{`Nat} }{ \Gamma \vdash \text{suc} M \downarrow \text{`Nat} }
  $$
  $$
  \frac{ \Gamma \vdash L \uparrow \text{`Nat} \quad \Gamma \vdash M \downarrow A \quad \Gamma, x \colon \text{`Nat} \vdash N \downarrow A }{ \Gamma \vdash \text{case } L [0 \Rightarrow M \mid \text{suc} x \Rightarrow N ] \downarrow A }
  $$

这些规则系统地定义了如何对项进行类型检查，并确定其类型。

### 练习 bidirectional-mul （推荐）

**概念**: 这一节提供了一个练习，要求读者将 Lambda 章节中的乘法定义重写为支持双向类型推理的形式。

**解释**: 练习的目标是让读者熟悉如何将已有的λ演算定义转换为支持双向类型推理的形式。这个过程涉及将每个项的类型推导重新表述为生成和继承的类型检查规则。

**操作**: 例如，在 Lambda 章节中，可能有如下乘法定义：

```agda
mul = λm. λn. case m [0 → 0 | suc m' → add n (mul m' n)]
```

你需要将其重写为：

```agda
mulᶜ : Term⁺
mulᶜ = (λ "m" ⇒ λ "n" ⇒
           case ` "m" [zero⇒ `zero | suc "m′" ⇒ `add (` "n" ↑) (`mul (` "m′" ↑) (` "n" ↑) ↑) ↑])
           ↓ (`ℕ ⇒ `ℕ ⇒ `ℕ)
```

### 练习 bidirectional-products （推荐）

**概念**: 这一节要求读者扩展双向类型推理规则，以包括 More 章节中的积类型（Products）。

**解释**: 这个练习旨在让读者了解如何扩展双向类型推理规则，以涵盖更复杂的类型系统。积类型表示在项中同时包含多个值的能力，比如元组 $(A, B)$。

**操作**: 你需要添加新的规则来处理积类型的生成和继承。假设有如下定义的积类型：

```agda
data Product : Type where
  _×_ : Type → Type → Type
  _,_ : Term⁻ → Term⁻ → Term⁻
  π₁  : Term⁺ → Term⁺
  π₂  : Term⁺ → Term⁺
```

需要扩展现有的规则来支持这些类型。

### 练习 bidirectional-rest （延伸）

**概念**: 这一节进一步扩展练习，要求读者扩展双向类型推理规则，以包括 More 章节的其余构造。

**解释**: 通过这个练习，读者将熟悉如何系统地扩展类型推理规则，使其涵盖特定的编程语言构造，如递归、不动点、以及其他高级类型。

**操作**: 扩展代码，加入支持这些构造的推理规则，并确保它们与现有的规则兼容。

### 前置需求

**概念**: 这一节介绍了推理过程中所需的基本操作，如类型等价性检查和语境查询。

**解释**: 
- **类型等价性检查**: 为了确保继承类型时的准确性，需要能够判定两个类型是否等价。实现方法通常是递归检查类型的结构。
- **语境查询**: 从语境中查询变量的类型，以确保在类型推理时，能够正确地获取到变量的类型。

**公式**: 对于等价性检查，规则可以写为：

$$
A \equiv B \iff (\text{如果 } A \text{ 和 } B \text{ 是相同的基本类型} \text{或它们的结构相同})
$$

### 唯一的类型

**概念**: 这一节探讨了类型在推理过程中唯一性的性质。

**解释**: 
- **唯一性**: 在推理过程中，对于给定的变量，其类型在语境中是唯一的。如果我们发现同一个变量在语境中有两个不同的类型，这将构成矛盾。

**公式**: 假设有两个推导 $\Gamma \vdash x \colon A$ 和 $\Gamma \vdash x \colon B$，唯一性可以表述为：

$$
A \equiv B
$$

这意味着，如果同一变量在同一语境下出现多次，则其类型必须一致。

### 查询语境中变量的类型

**概念**: 这一节详细讨论了如何从语境中查询变量的类型。

**解释**: 查询语境是推理过程中非常基础的一步，确保在推理时能准确获取到变量的类型。

**操作**: 实现一个函数 `lookup`，它在给定语境和变量名的情况下，返回变量的类型（如果存在的话），否则返回错误信息。该函数需要处理语境为空和语境不为空两种情况。

### 提升否定

**概念**: 讨论了如何处理在推理过程中遇到的矛盾和否定情况。

**解释**: 在推理过程中，我们可能会遇到一些不可能成立的情况。这些情况需要被妥善处理，以确保推理过程的准确性。

**公式**: 例如，如果我们推导出 $A \equiv B$，但同时又有 $A \neq B$ 的证明，这就构成了矛盾。

### 生成和继承类型（再次出现）

**概念**: 这里再次讨论了生成和继承类型，详细介绍了如何实现这两个过程。

**解释**: 生成和继承类型的过程在前文已经介绍，这里对其实现进行了详细说明。

**操作**: 编写生成和继承类型的算法，实现双向类型推理的功能。

### 测试项的例子

**概念**: 给出了如何测试生成和继承类型推理的具体例子。

**解释**: 通过实际的例子，验证推理算法是否能够正确处理各种类型推导。这些例子通常包括对简单的类型推导，如加法和乘法，以及对复杂类型的推导，如函数应用和类型转换。

**例子**: 例如，对表达式 $2 + 2$ 的类型推导，可以验证算法是否能正确推导出结果类型为自然数。

### 测试错误的例子

**概念**: 测试推理过程中遇到的错误情况。

**解释**: 除了验证算法在正确情况下的表现，还需要验证其在错误输入下的处理能力。这包括测试不合法的变量、类型不匹配的应用、以及其他会导致推理失败的情况。

**例子**: 例如，尝试对一个未定义的变量进行推理，或对一个不匹配的类型进行应用，验证算法是否能够正确识别并报告错误。

### 擦除

**概念**: 讨论了如何从装饰过的项中擦除类型注释，恢复原始项。

**解释

**: 擦除的过程是将类型注释从项中去掉，以得到一个更简洁的表示。这对于将装饰过的项转换为更基础的λ演算项非常有用。

**操作**: 编写擦除函数，将双向类型推理中的类型注释移除，得到对应的内在类型项。

### 练习 inference-multiplication （推荐）

**概念**: 这个练习要求对乘法项进行双向推理，并证明推理得到的赋型可以擦除为 DeBruijn 章节中的定义。

**操作**: 将乘法项通过双向类型推理进行处理，并验证擦除后的结果与 DeBruijn 表示一致。

### 练习 inference-products （推荐）

**概念**: 要求扩展双向推理规则以包括积类型，并扩展相应的擦除规则。

**操作**: 为积类型定义新的推理规则，并实现这些规则的擦除函数。

### 练习 inference-rest （延伸）

**概念**: 扩展推理规则以包括 More 章节中的所有构造。

**操作**: 继续扩展推理规则，处理所有剩余的构造，并实现对应的擦除函数。

### Agda 中的双向推理

**概念**: 讨论了 Agda 编程语言中的双向类型推理机制。

**解释**: Agda 使用双向类型推理来进行类型检查。理解 Agda 的推理机制有助于理解如何在更大的编程语言或类型系统中实现类似的机制。

**操作**: 阅读并理解 Agda 的文档，了解其双向推理的实现方式，尤其是在定义和应用中类型推理的表现。

### Unicode

**概念**: 列出了本章中使用的 Unicode 字符。

**解释**: 这些字符在推理和表达公式时被广泛使用，确保了符号的清晰性和一致性。

**符号**: 
- **↓** (U+2193): 表示继承类型的下箭头符号。
- **↑** (U+2191): 表示生成类型的上箭头符号。
- **∥** (U+2225): 表示平行关系符号。

这些符号在推理过程中为表达各种逻辑关系提供了便利。





### ----------------------------------------

好的，我将逐一解释【Inference: 双向类型推理】章节的内容，并将概念和解释结合在一起。我们按照原文的结构逐节进行详细讲解，使用公式时将适时插入。

### 绪论：推理规则作为算法

**概念：**  
在这个小节中，首先介绍了推理规则如何作为算法来使用。我们引入了一种带有类型注释的项的语法，该语法与先前在Lambda章节中介绍的项相似，允许对项的类型进行推理。

- **语法：**
  
  项的语法如下：
  $$
  L, M, N ::= \quad x \quad | \quad \lambda x : A \Rightarrow N \quad | \quad L \cdot M
  $$
  其中：
  - $x$ 表示变量。
  - $\lambda x : A \Rightarrow N$ 表示带有类型注释的抽象项。
  - $L \cdot M$ 表示应用项。

**解释：**  
这里的关键思想是，每个项的类型可以通过语法导向（syntax-directed）的推理规则自动推导出来。推理规则类似于编写程序时的推理步骤，可以看作是为每一个子项分配类型的“算法”。

**概念：推理规则的输入和输出**  
推理规则根据输入项的不同部分，将其标记为“输入”或“输出”。例如，在查询变量的类型时，语境 $\Gamma$ 和变量 $x$ 是输入，而类型 $A$ 是输出。

- **规则：**
  
  对于查询语境中的变量 $x$ 的类型，推理规则如下：
  $$
  \frac{}{\Gamma, x : A \vdash x : A} \quad \text{(Z 规则)}
  $$
  $$
  \frac{\Gamma \vdash x : A}{\Gamma, y : B \vdash x : A} \quad \text{(S 规则)}
  $$
  其中，Z 规则用于处理语境中最后一个变量，S 规则用于处理语境中的其他变量。

**解释：**  
根据上述规则，当给定语境 $\Gamma$ 和变量 $x$ 时，如果变量 $x$ 是最后一个引入的变量（通过 Z 规则），我们可以直接从语境中找到类型 $A$；否则（通过 S 规则），我们通过递归查询前面的语境来确定 $x$ 的类型。

### 生成和继承类型

**概念：生成类型与继承类型**  
在双向类型推理中，赋型判断分为两种：生成类型和继承类型。生成类型是指从给定项推导出其类型的过程，继承类型则是验证给定项是否符合指定类型的过程。

- **规则：**
  
  生成类型的判断：
  $$
  \Gamma \vdash M \uparrow A
  $$
  继承类型的判断：
  $$
  \Gamma \vdash M \downarrow A
  $$
  其中，$\Gamma$ 是语境，$M$ 是项，$A$ 是类型。

**解释：**  
在生成类型的判断中，语境 $\Gamma$ 和项 $M$ 是输入，推导出的类型 $A$ 是输出。而在继承类型的判断中，语境 $\Gamma$、项 $M$ 和类型 $A$ 都是输入。

**概念：语法导向的生成与继承**  
在双向类型推理中，生成类型主要应用于变量和函数应用等需要从表达式中提取类型的情况，而继承类型则用于如 $\lambda$ 抽象和自然数等构造固定类型的情况。

- **语法：**
  
  - 生成类型的语法项（Term⁺）：
    $$
    L^+, M^+, N^+ ::= x \quad | \quad L^+ \cdot M^- \quad | \quad M^- \downarrow A
    $$
  - 继承类型的语法项（Term⁻）：
    $$
    L^-, M^-, N^- ::= \lambda x \Rightarrow N^- \quad | \quad \texttt{zero} \quad | \quad \texttt{suc } M^- \quad | \quad \texttt{case } L^+ [\texttt{zero} \Rightarrow M^- | \texttt{suc } x \Rightarrow N^-]
    $$

**解释：**  
- 对于生成类型的语法项 $Term^+$ 来说，变量 $x$ 和应用 $L^+ \cdot M^-$ 是可以生成类型的，而表达式 $M^- \downarrow A$ 表示从继承类型转变为生成类型，这需要验证继承的类型是否一致。
- 对于继承类型的语法项 $Term^-$ 来说，$\lambda$ 抽象、自然数构造子（zero 和 suc）等都是可以继承类型的。

### 可靠性和完备性

**概念：可靠性与完备性**  
在双向类型推理中，可靠性指的是推理成功时的推导是正确的，而完备性指的是如果类型确实存在，推理必然能找到该类型。

- **规则：**
  
  生成类型的算法：
  $$
  \texttt{synthesize} : \Gamma \to M^+ \to \texttt{Dec} (\exists A. \Gamma \vdash M^+ \uparrow A)
  $$
  继承类型的算法：
  $$
  \texttt{inherit} : \Gamma \to M^- \to A \to \texttt{Dec} (\Gamma \vdash M^- \downarrow A)
  $$

**解释：**  
synthesize 和 inherit 是两个主要的类型推理函数，分别用于生成类型和继承类型。通过这些函数，我们可以决定给定项的类型是否符合规则。

---

接下来，我们将继续讨论生成和继承类型的具体推理规则及其实现。

### 双向类型推理：生成和继承类型的规则与实现

#### 变量的生成类型规则

**概念：**  
在语境中查询变量的类型是生成类型推理中的基本操作。

- **规则：**
  $$
  \frac{\Gamma \ni x : A}{\Gamma \vdash x \uparrow A}
  $$
  该规则表示，如果在语境 $\Gamma$ 中，变量 $x$ 具有类型 $A$，那么我们可以推导出 $\Gamma \vdash x \uparrow A$。

**解释：**  
这是生成类型的一条基本规则。通过在语境中查找变量 $x$ 的类型，我们直接得到了 $x$ 的类型 $A$，这是生成类型推理的一个简单而直接的过程。

#### 应用的生成类型规则

**概念：**  
函数应用的类型推导涉及到对函数项和参数项的类型推导。

- **规则：**
  $$
  \frac{\Gamma \vdash L \uparrow A \Rightarrow B \quad \Gamma \vdash M \downarrow A}{\Gamma \vdash L \cdot M \uparrow B}
  $$
  该规则表示，如果在语境 $\Gamma$ 中，函数项 $L$ 生成了一个函数类型 $A \Rightarrow B$，且参数项 $M$ 能够继承类型 $A$，那么我们可以推导出 $\Gamma \vdash L \cdot M \uparrow B$。

**解释：**  
在这个推理规则中，我们首先从函数项 $L$ 推导出它的类型 $A \Rightarrow B$，然后检查参数项 $M$ 是否能够继承类型 $A$。如果这两者都成立，那么应用项 $L \cdot M$ 的生成类型为 $B$。

#### 切换至继承类型的规则

**概念：**  
在某些情况下，我们需要将生成类型转化为继承类型来进行进一步的推理。

- **规则：**
  $$
  \frac{\Gamma \vdash M \downarrow A}{\Gamma \vdash M \uparrow A}
  $$
  该规则表示，如果在语境 $\Gamma$ 中，项 $M$ 能够继承类型 $A$，那么它也可以生成类型 $A$。

**解释：**  
这条规则允许我们在生成类型推导过程中，使用已经存在的继承类型推导结果。这在处理复杂的嵌套表达式时尤为重要。

### 继承类型的推理规则

#### 抽象的继承类型规则

**概念：**  
$\lambda$ 抽象在推理类型时使用继承类型规则，要求提供抽象参数的类型信息。

- **规则：**
  $$
  \frac{\Gamma, x : A \vdash N \downarrow B}{\Gamma \vdash \lambda x \Rightarrow N \downarrow A \Rightarrow B}
  $$
  该规则表示，如果在语境 $\Gamma$ 中，当引入参数 $x : A$ 后，项 $N$ 能继承类型 $B$，那么整个抽象项 $\lambda x \Rightarrow N$ 能继承类型 $A \Rightarrow B$。

**解释：**  
$\lambda$ 抽象的类型推理依赖于对抽象体 $N$ 的类型推导。在推理 $N$ 的类型时，我们假设参数 $x$ 的类型是 $A$，然后推导出 $N$ 的类型 $B$。最终，抽象项的类型是 $A \Rightarrow B$。

#### 零和后继的继承类型规则

**概念：**  
自然数的零和后继项具有固定的类型。

- **规则：**
  - 对于零：
    $$
    \frac{}{\Gamma \vdash \texttt{zero} \downarrow \mathbb{N}}
    $$
  - 对于后继：
    $$
    \frac{\Gamma \vdash M \downarrow \mathbb{N}}{\Gamma \vdash \texttt{suc } M \downarrow \mathbb{N}}
    $$

**解释：**  
- 零（zero）的类型总是 $\mathbb{N}$，不依赖于语境。
- 后继项（suc）的类型推导则依赖于其前驱项的类型。如果 $M$ 是一个自然数，那么其后继项也是一个自然数。

#### 分情况讨论的继承类型规则

**概念：**  
分情况讨论项需要对不同情况分别进行类型推导。

- **规则：**
  $$
  \frac{\Gamma \vdash L \uparrow \mathbb{N} \quad \Gamma \vdash M \downarrow A \quad \Gamma, x : \mathbb{N} \vdash N \downarrow A}{\Gamma \vdash \texttt{case } L [\texttt{zero} \Rightarrow M | \texttt{suc } x \Rightarrow N ] \downarrow A}
  $$

**解释：**  
分情况讨论的类型推导涉及三个部分：
- 首先，$L$ 生成自然数类型 $\mathbb{N}$。
- 然后，处理 $\texttt{zero}$ 情况的项 $M$ 必须继承类型 $A$。
- 最后，在处理 $\texttt{suc } x$ 情况时，项 $N$ 必须继承类型 $A$，且需要考虑 $x$ 是自然数的语境。

#### 递归定义的继承类型规则

**概念：**  
不动点（递归定义）的类型推导。

- **规则：**
  $$
  \frac{\Gamma, x : A \vdash N \downarrow A}{\Gamma \vdash \mu x \Rightarrow N \downarrow A}
  $$

**解释：**  
在递归定义中，假设递归变量 $x$ 具有类型 $A$，我们推导出递归体 $N$ 的类型 $A$。如果这成立，则递归定义 $\mu x \Rightarrow N$ 也具有类型 $A$。

### 可靠性和完备性的证明

**概念：可靠性与完备性**  
为了确保推理系统的正确性，我们需要证明推理规则的可靠性和完备性。

- **可靠性：**  
  如果推理成功，推导出的类型是正确的。
- **完备性：**  
  如果类型确实存在，推理必然能找到该类型。

**解释：**  
这部分内容通过算法 `synthesize` 和 `inherit` 的定义，逐步实现了生成类型和继承类型的推理。在这个过程中，确保算法的每一步都是可靠和完备的。

### 示例与测试

**概念：示例和测试**  
通过具体的代码示例和测试，验证算法的正确性。

- **示例：**  
  定义自然数 $2+2$ 的项：
  ```agda
  two : Term⁻
  two = `suc (`suc `zero)
  
  2+2 : Term⁺
  2+2 = plus · two · two
  ```

**解释：**  
在这部分，我们通过具体的代码示例展示了如何使用 `synthesize` 和 `inherit` 来对项进行类型推理。测试的结果表明，推理函数能够正确推导出项的类型。

### 类型擦除

**概念：类型擦除**  
从带有外在类型推导的项转换为内在类型表示的方法。

- **函数：**
  ```agda
  ∥_∥Tp : Type → DB.Type
  ∥ `ℕ ∥Tp             =  DB.`ℕ
  ∥ A ⇒ B ∥Tp          =  ∥ A ∥Tp DB.⇒ ∥ B ∥Tp
  ```

**解释：**  
类型擦除的过程将每个赋型判断中的构造子替换为内在类型系统中的对应项构造子。通过类型擦除，我们可以将双向类型推理转换为内在类型系统中的表示，确保其与之前的内在类型项一致。

---

到此为止，我们已经覆盖了双向类型推理的主要内容，包括生成类型和继承类型的规则、算法的实现、可靠性和完备性的证明，以及类型擦除。下一步，你可以应用这些概念来处理更复杂的推理任务，如乘法项和积的双向类型推理。

### ----------------------------------------

