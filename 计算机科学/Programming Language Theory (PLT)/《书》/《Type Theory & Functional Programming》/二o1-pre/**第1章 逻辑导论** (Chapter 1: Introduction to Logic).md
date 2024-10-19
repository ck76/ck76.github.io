[toc]



好的，接下来我会按照您提供的风格逐步讲解《Type Theory and Functional Programming》这本书的内容。我们从第一章开始，详细解释每个关键概念和细节，确保初学者能够理解。如果有不明白的地方或需要进一步解释，请随时告知。

### **Chapter 1 Introduction to Logic (逻辑导论)**

#### **解释：**
在第一章中，书籍通过简要介绍形式逻辑，目的是为后续章节建立基本的符号和术语。逻辑是关于推理的科学，形式逻辑的作用主要有以下几点：
1. **提供有效推理的清晰表征**：通过形式化推理系统，可以清楚地区分有效和无效的推理，从而帮助我们更好地理解非形式推理。
2. **程序正确性验证**：对于需要验证程序正确性的推理，形式化的推理是必要的。只有通过形式化，我们才能通过机械化的方式检查推理的正确性。
3. **研究逻辑系统的性质**：除了验证个别推理的正确性之外，我们还可以研究整个逻辑系统的性质，如其表达能力或语义解释。

这些目的为形式逻辑奠定了基础，而接下来我们会介绍一种形式逻辑系统——自然演绎系统 (Natural Deduction)。

### **1.1 Propositional Logic (命题逻辑)**

#### **解释：**
命题逻辑是一种用于形式化涉及“与”（and）、“或”（or）、“非”（not）、“蕴涵”（implies）等连接词推理的逻辑系统。我们可以用这些连接词来构造复合命题（compound propositions），例如：
- **A ∧ B** 表示 A 和 B。
- **A ∨ B** 表示 A 或 B。
- **¬A** 表示非 A，即 A 不成立。
- **A ⇒ B** 表示如果 A 成立，则 B 也成立。

#### **公式定义：**
$$
\text{命题公式的语法规则}：
$$
- 一个公式可以是一个命题变量 $X_0, X_1, X_2, \ldots$，或
- 一个复合公式的形式如下：
  $$
  (A \land B), (A \Rightarrow B), (A \lor B), \bot, (A \Leftrightarrow B), (\neg A)
  $$

#### **推导规则：**

接下来我们讨论逻辑系统的两部分内容：
1. **语言**：我们已经定义了命题如何表示。
2. **有效推理**：接下来需要定义哪些推理是有效的，这些有效推理称为**证明**或**推导**。

推导的基本形式是基于某些假设推导结论。我们通过**推导规则**将简单的推导组合成复杂的推导。

#### **假设规则 (Assumption Rule)**：
任何命题 $A$ 都可以从它自身的假设推出。用公式表示为：
$$
A
$$
这是从假设 $A$ 推导出命题 $A$ 的证明。

#### **合取引入规则 (Conjunction Introduction)**：
如果我们有 $A$ 和 $B$ 的证明，我们可以通过合取引入规则推导出 $A \land B$：
$$
A \quad B
$$
$$
A \land B \ (\land I)
$$

#### **例子：**
假设我们已经推导出 $A$ 和 $B$，那么我们可以推导出 $A \land B$：
$$
A \quad B
$$
$$
A \land B \ (\land I)
$$

通过类似方式，我们可以继续推导更复杂的表达式。

#### **合取消去规则 (Conjunction Elimination)**：
如果我们已经证明了 $A \land B$，我们可以推导出 $A$ 和 $B$：
$$
A \land B
$$
$$
A \ (\land E1) \quad B \ (\land E2)
$$

#### **推理过程演示：**
如果我们有 $(A \land B) \land C$ 的证明，我们可以逐步通过合取消去规则推导出 $A$、$B$ 和 $C$，最终组合这些推导得到 $A \land (B \land C)$，这表明合取操作具有结合律。

以上是对命题逻辑和合取操作的一些基本介绍，后续我们还会进一步介绍蕴涵（⇒）等其他操作的推导规则。

### **总结：**
命题逻辑通过定义连接词如“与”（∧）、“或”（∨）、“非”（¬）等，以及推导规则，帮助我们形式化描述推理过程。推导规则如合取引入和消去可以帮助我们从简单的推导构建复杂推理。这一部分为后续章节更深入的逻辑推理奠定了基础。

接下来我们会进一步讲解蕴涵和其他推导规则。如果有任何疑问或需要进一步解释的内容，请继续提供给我，我会详细讲解。

### ---------------------------

### **Chapter 1 Introduction to Logic (逻辑导论)**

#### **解释：**

在第一章中，作者为我们介绍了**形式逻辑**（formal logic）的基本概念，这将为整本书的符号和术语奠定基础。虽然假设读者已经熟悉逻辑的基础知识，但为了全面理解，我们将详细解释每个概念。

**逻辑**是关于**论证**（argument）的科学。形式化逻辑系统的目的多种多样，主要包括：

1. **清晰刻画有效证明**：形式化使我们能够明确系统中哪些是有效的证明，从而可以评判个别论证，增强我们对非形式推理的理解。

2. **验证程序的正确性**：当论证本身涉及形式系统（例如验证计算机程序的正确性）时，论证应以可被检查其正确性的形式书写。这只有在论证被形式化时才能实现，且正确性可以机械地检查。例如，怀特海和罗素的《数学原理》（*Principia Mathematica*）中包含大量形式证明，但其中许多证明实际上是错误的，人类校对者很难发现这些错误。

3. **研究逻辑系统的整体性质**：除了关注个别证明的正确与否，我们还可以研究逻辑系统整体的性质。例如，我们可以研究其相对于其他理论的表达能力，或者研究其含义或语义。这项工作主要是数学性质的，被称为**数学逻辑**（mathematical logic），更多细节可以在 [Men87a] 等文献中找到。

#### **我们的目标：**

我们希望提供一个**形式系统**，在其中可以表达特定句子的有效性论证。逻辑系统有多种不同的风格，这里我们将首先研究**命题逻辑**（propositional logic）的自然演绎系统（natural deduction system），然后是谓词逻辑（predicate logic）。

### **1.1 Propositional Logic (命题逻辑)**

#### **解释：**

**命题逻辑**形式化了涉及连接词（connectives）的论证，例如“和”（and，$\land$）、“或”（or，$\lor$）、“非”（not，$\neg$）、“蕴涵”（implies，$\Rightarrow$）等。使用这些连接词，我们可以从**命题变量**（propositional variables）或**原子命题**（atomic propositions）构建复杂的命题。

#### **定义 1.1：**

我们的**语法**正式定义如下：一个**公式**（formula）要么是：

- 一个命题变量 $X_0, X_1, X_2, \ldots$，或者
- 一个复合公式（compound formula），形式为：
  $$
  (A \land B), \quad (A \Rightarrow B), \quad (A \lor B), \quad \bot, \quad (A \Leftrightarrow B), \quad (\neg A)
  $$
  其中 $A$ 和 $B$ 是公式。

这些复合公式对应以下非形式组合：

- $A \land B$：A **和** B
- $A \Rightarrow B$：A **蕴涵** B（如果 A，则 B）
- $A \lor B$：A **或** B
- $\bot$：**假命题**（Falsity），表示矛盾或不可能的情况
- $A \Leftrightarrow B$：A **当且仅当** B
- $\neg A$：**非** A，即 A 不成立

我们采用以下约定：大写斜体字母 $A, B, C, \ldots$ 表示任意公式。这些字母是用于讨论上述语法定义的对象语言（object language）的元语言（metalanguage）中的变量。当不会产生歧义时，我们会省略公式中的括号。

#### **逻辑系统的两部分：**

1. **语言（Language）**：已经定义，用于书写断言或命题的形式系统。
2. **有效论证（Valid Arguments）**：我们需要描述哪些推理是有效的，这些有效的推理称为**证明**（proofs）或**推导**（derivations）。

#### **推导的形式：**

一个推导通常是基于一些（可能为空）**假设**（assumptions）来推断一个**结论**（conclusion）。较大的推导可以通过**推导规则**（deduction rules）从较小的推导中归纳构建而成。

#### **假设规则（Assumption Rule）：**

这是最简单的推导规则，声明任何公式 $A$ 都可以从自身的假设推出。

**表示方式：**

$$
\frac{}{A} \quad (\text{Assumption Rule})
$$

解释：这是一个从假设 $A$ 推导出公式 $A$ 的证明。

#### **合取引入规则（∧ Introduction）：**

如果我们有 $A$ 和 $B$ 的证明，那么通过**合取引入规则**，我们可以推导出 $A \land B$。

**表示方式：**

$$
\frac{A \quad B}{A \land B} \quad (\land I)
$$

解释：从 $A$ 和 $B$ 的证明，可以推出 $A \land B$。$A \land B$ 的证明所依赖的假设是 $A$ 和 $B$ 的证明所依赖的假设的并集。

#### **例子：**

构建一个简单的证明：

1. 证明 $A$ 和 $B$：

   $$
   A \quad B
   $$

2. 应用合取引入规则，得到 $A \land B$：

   $$
   \frac{A \quad B}{A \land B} \quad (\land I)
   $$

3. 证明 $C$：

   $$
   C
   $$

4. 再次应用合取引入规则，得到 $A \land C$：

   $$
   \frac{A \quad C}{A \land C} \quad (\land I)
   $$

5. 最后，将以上结果组合，得到 $((A \land B) \land (A \land C))$：

   $$
   \frac{A \land B \quad A \land C}{(A \land B) \land (A \land C)} \quad (\land I)
   $$

在这个证明的树形结构的叶子节点，我们发现假设 $A$ 出现了两次，$B$ 和 $C$ 各出现一次。通过三次应用合取引入规则，我们从组成部分推导出了最终的合取公式。

#### **合取消去规则（∧ Elimination）：**

合取消去规则告诉我们如何从一个合取公式中推导出其组成部分，即何时可以**消去**合取。

**表示方式：**

1. 从 $A \land B$ 推导出 $A$：

   $$
   \frac{A \land B}{A} \quad (\land E1)
   $$

2. 从 $A \land B$ 推导出 $B$：

   $$
   \frac{A \land B}{B} \quad (\land E2)
   $$

解释：从 $A \land B$ 的证明，可以分别推出 $A$ 和 $B$。这些推导所依赖的假设与 $A \land B$ 的证明相同。

#### **示例：**

假设我们有 $(A \land B) \land C$ 的证明，我们可以：

1. 应用合取消去规则，得到 $A \land B$：

   $$
   \frac{(A \land B) \land C}{A \land B} \quad (\land E1)
   $$

2. 再次应用合取消去规则，得到 $A$：

   $$
   \frac{A \land B}{A} \quad (\land E1)
   $$

3. 类似地，从 $(A \land B) \land C$ 推导出 $B$ 和 $C$，最终得到 $A \land (B \land C)$，展示了合取操作的**结合律**（associativity）。

#### **总结：**

- **合取引入规则**允许我们从 $A$ 和 $B$ 的证明推出 $A \land B$。
- **合取消去规则**允许我们从 $A \land B$ 的证明推出 $A$ 或 $B$。
- 通过这些规则，我们可以构建复杂的证明，展示逻辑连接词的性质，如结合律。

接下来，我们将介绍**蕴涵**（implication，$\Rightarrow$）的推导规则。

### **蕴涵的推导规则**

#### **解释：**

在给出蕴涵（$\Rightarrow$）的规则之前，我们先非形式地理解其含义。$A \Rightarrow B$ 表示“如果 $A$，则 $B$”，或者从 $A$ 可以推导出 $B$。因此，当我们有一个基于假设 $A$ 的 $B$ 的推导时，我们希望得出 $A \Rightarrow B$。

关键点在于，新的 $A \Rightarrow B$ 的证明所依赖的假设是原本 $B$ 的证明所依赖的假设，**除去**假设 $A$ 本身。原因是 $A$ 已经成为公式 $A \Rightarrow B$ 的**前提**（hypothesis），通过逻辑公式表达了从 $A$ 推导出 $B$。

#### **蕴涵引入规则（⇒ Introduction）：**

**表示方式：**

$$
\frac{\begin{array}{c}
[A] \\
\vdots \\
B
\end{array}}{A \Rightarrow B} \quad (\Rightarrow I)
$$

- 方括号 $[A]$ 表示假设 $A$，将在此规则中被**消解**（discharged）。
- 从依赖于假设 $A$ 的 $B$ 的证明，可以推出 $A \Rightarrow B$，且新的证明不再依赖于假设 $A$。
- 需要强调的是，即使 $B$ 的证明不依赖于 $A$，该规则仍然适用。

#### **蕴涵消去规则（⇒ Elimination）：**

**表示方式：**

$$
\frac{A \quad A \Rightarrow B}{B} \quad (\Rightarrow E)
$$

- 从 $A$ 和 $A \Rightarrow B$ 的证明，可以推出 $B$。
- $B$ 的证明所依赖的假设是 $A$ 和 $A \Rightarrow B$ 的证明所依赖的假设的并集。

#### **例子：**

我们来看一个更复杂的例子，推导 $((A \land B) \Rightarrow C) \Rightarrow (A \Rightarrow (B \Rightarrow C))$。

**步骤：**

1. **假设 $(A \land B) \Rightarrow C$**：

   $$
   [(A \land B) \Rightarrow C]_3
   $$

2. **假设 $A$**：

   $$
   [A]_2
   $$

3. **假设 $B$**：

   $$
   [B]_1
   $$

4. **应用合取引入规则，得到 $A \land B$**：

   $$
   \frac{A \quad B}{A \land B} \quad (\land I)
   $$

5. **应用蕴涵消去规则，得到 $C$**：

   $$
   \frac{A \land B \quad (A \land B) \Rightarrow C}{C} \quad (\Rightarrow E)
   $$

6. **消解假设 $B$，得到 $B \Rightarrow C$**：

   $$
   \frac{\begin{array}{c}
   [B]_1 \\
   \vdots \\
   C
   \end{array}}{B \Rightarrow C} \quad (\Rightarrow I)_1
   $$

7. **消解假设 $A$，得到 $A \Rightarrow (B \Rightarrow C)$**：

   $$
   \frac{\begin{array}{c}
   [A]_2 \\
   \vdots \\
   B \Rightarrow C
   \end{array}}{A \Rightarrow (B \Rightarrow C)} \quad (\Rightarrow I)_2
   $$

8. **消解假设 $(A \land B) \Rightarrow C$，得到最终结果**：

   $$
   \frac{\begin{array}{c}
   [(A \land B) \Rightarrow C]_3 \\
   \vdots \\
   A \Rightarrow (B \Rightarrow C)
   \end{array}}{((A \land B) \Rightarrow C) \Rightarrow (A \Rightarrow (B \Rightarrow C))} \quad (\Rightarrow I)_3
   $$

#### **说明：**

- **消解假设（Discharging Assumptions）**：在推导过程中，我们通过蕴涵引入规则，将一些假设“封装”进蕴涵公式中，从而消解这些假设，使得最终的结论不再依赖于这些假设。
- **标签的使用**：为了明确哪个假设被哪个规则消解，我们使用下标标签，例如 $[A]_2$ 和 $(\Rightarrow I)_2$。

#### **自反性和其他推导：**

- **自反性**：证明 $A \Rightarrow A$：

  $$
  \frac{\begin{array}{c}
  [A] \\
  \vdots \\
  A
  \end{array}}{A \Rightarrow A} \quad (\Rightarrow I)
  $$

- **推导 $B \Rightarrow (A \Rightarrow B)$**：

  - 首先，假设 $B$：

    $$
    [B]_2
    $$

  - 然后，通过蕴涵引入规则，得到 $A \Rightarrow B$，注意这里 $B$ 的证明不依赖于 $A$：

    $$
    \frac{\begin{array}{c}
    [A]_1 \\
    \vdots \\
    B
    \end{array}}{A \Rightarrow B} \quad (\Rightarrow I)_1
    $$

  - 最后，消解假设 $B$，得到 $B \Rightarrow (A \Rightarrow B)$：

    $$
    \frac{\begin{array}{c}
    [B]_2 \\
    \vdots \\
    A \Rightarrow B
    \end{array}}{B \Rightarrow (A \Rightarrow B)} \quad (\Rightarrow I)_2
    $$

  - 注意，在这个推导中，假设 $A$ 被立即消解，因为 $B$ 的证明并不依赖于 $A$。

### **析取的推导规则**

#### **解释：**

**析取**（disjunction，$\lor$）表示“或”连接词，$A \lor B$ 表示 $A$ 或 $B$。

#### **析取引入规则（∨ Introduction）：**

**表示方式：**

1. 从 $A$ 推导出 $A \lor B$：

   $$
   \frac{A}{A \lor B} \quad (\lor I1)
   $$

2. 从 $B$ 推导出 $A \lor B$：

   $$
   \frac{B}{A \lor B} \quad (\lor I2)
   $$

解释：如果我们有 $A$ 或 $B$ 的证明，就可以推出 $A \lor B$。

#### **析取消去规则（∨ Elimination）：**

如果我们有 $A \lor B$ 的证明，并且可以从 $A$ 推导出 $C$，从 $B$ 推导出 $C$，那么我们可以推导出 $C$。

**表示方式：**

$$
\frac{\begin{array}{c}
A \lor B \\
\begin{array}{l}
[A]_1 \\
\vdots \\
C
\end{array} \quad
\begin{array}{l}
[B]_2 \\
\vdots \\
C
\end{array}
\end{array}}{C} \quad (\lor E)
$$

- **消解假设**：假设 $A$ 和 $B$ 被消解，最终的结论 $C$ 不再依赖于它们。

#### **例子：**

假设我们有 $A \lor B$ 和 $(A \Rightarrow C) \land (B \Rightarrow C)$，想要推导出 $C$。

1. 从 $A$ 推导 $C$：

   - 假设 $A$：

     $$
     [A]_1
     $$

   - 从 $(A \Rightarrow C) \land (B \Rightarrow C)$ 中提取 $A \Rightarrow C$：

     $$
     \frac{(A \Rightarrow C) \land (B \Rightarrow C)}{A \Rightarrow C} \quad (\land E1)
     $$

   - 应用蕴涵消去规则，得到 $C$：

     $$
     \frac{A \quad A \Rightarrow C}{C} \quad (\Rightarrow E)
     $$

2. 从 $B$ 推导 $C$（类似步骤）：

   $$
   [B]_2 \quad \frac{(A \Rightarrow C) \land (B \Rightarrow C)}{B \Rightarrow C} \quad (\land E2) \quad \frac{B \quad B \Rightarrow C}{C} \quad (\Rightarrow E)
   $$

3. 应用析取消去规则，消解假设 $A$ 和 $B$，得到 $C$：

   $$
   \frac{\begin{array}{c}
   A \lor B \\
   \begin{array}{l}
   [A]_1 \\
   \vdots \\
   C
   \end{array} \quad
   \begin{array}{l}
   [B]_2 \\
   \vdots \\
   C
   \end{array}
   \end{array}}{C} \quad (\lor E)
   $$

4. 最后，消解剩余的假设，得到 $((A \Rightarrow C) \land (B \Rightarrow C)) \Rightarrow ((A \lor B) \Rightarrow C)$。

### **否定和矛盾**

#### **假命题（⊥）和否定（¬）**

- **假命题**（Falsity），记作 $\bot$，表示逻辑上的矛盾或不可能的情况。

- **假命题的消去规则（⊥ Elimination）**：

  **表示方式：**

  $$
  \frac{\bot}{A} \quad (\bot E)
  $$

  解释：从假命题 $\bot$ 可以推出任何命题 $A$。这也被称为“从矛盾中可推出任何东西”（*ex falso quodlibet*）。

- **否定的定义**：

  $$
  \neg A \equiv_{\text{定义}} A \Rightarrow \bot
  $$

  解释：$A$ 的否定 $\neg A$ 被定义为 $A$ 蕴涵 $\bot$，即如果 $A$ 成立，则产生矛盾。

#### **否定的推导规则：**

1. **否定引入规则（¬ Introduction）**：

   如果从假设 $A$ 可以推出矛盾 $\bot$，那么可以推出 $\neg A$。

   **表示方式：**

   $$
   \frac{\begin{array}{c}
   [A] \\
   \vdots \\
   \bot
   \end{array}}{\neg A} \quad (\neg I)
   $$

2. **否定消去规则（¬ Elimination）**：

   如果有 $A$ 和 $\neg A$ 的证明，可以推出任何命题 $B$。

   **表示方式：**

   $$
   \frac{A \quad \neg A}{B} \quad (\neg E)
   $$

#### **推导这些规则：**

- 由于 $\neg A$ 被定义为 $A \Rightarrow \bot$，因此可以使用之前的蕴涵规则推导出否定的引入和消去规则。

#### **双重否定和经典逻辑**

- **直觉主义逻辑（Intuitionistic Logic）**：我们目前介绍的系统是直觉主义的，强调构造性证明。

- **经典逻辑（Classical Logic）**：基于真值函数的意义理论，每个命题要么为真，要么为假。

- **排中律（Law of Excluded Middle）**：在经典逻辑中，对于任何命题 $A$，$A \lor \neg A$ 都为真。

  **表示方式：**

  $$
  \frac{}{A \lor \neg A} \quad (\text{EM})
  $$

- **双重否定规则（Double Negation）**：

  $$
  \frac{\neg \neg A}{A} \quad (\text{DN})
  $$

- **反证法（Proof by Contradiction）**：

  $$
  \frac{\begin{array}{c}
  [\neg A] \\
  \vdots \\
  B \\
  [\neg A] \\
  \vdots \\
  \neg B
  \end{array}}{A} \quad (\text{CC})
  $$

#### **练习：**

1. **证明蕴涵的传递性**：从假设 $A \Rightarrow B$ 和 $B \Rightarrow C$ 推导出 $A \Rightarrow C$。

2. **证明其他公式**：例如，证明 $((A \lor B) \Rightarrow C) \Rightarrow ((A \Rightarrow C) \land (B \Rightarrow C))$。

### **总结：**

第一章通过引入命题逻辑的自然演绎系统，为我们建立了形式逻辑的基础。我们学习了如何使用推导规则构建证明，包括合取、蕴涵、析取和否定的引入和消去规则。

这些规则不仅帮助我们理解逻辑推理的形式化过程，还为后续章节更深入的类型理论和函数式编程奠定了基础。

如果您有任何疑问或需要进一步解释的内容，请随时告诉我！

### ----------------------------

### **1.2 Predicate Logic (谓词逻辑)**

#### **解释：**

在本节中，我们将探讨**谓词逻辑**，即关于属性或谓词的逻辑。在之前对**命题逻辑**的研究中，最简单的命题是“原子”（atomic）或未分析的命题。而在谓词逻辑中，我们构建的系统允许命题由关于对象具有某些属性或某些对象相等的陈述构成。

谓词逻辑比命题逻辑更为精细，因为它不仅考虑命题本身，还考虑命题内部的结构，尤其是对象和它们之间的关系。

#### **语法定义：**

**定义 1.2**

我们的语言包含两类符号：

1. **项（Terms）**：用于表示对象，形式包括：

   - **个体变量**：$v_0, v_1, v_2, \ldots$。在接下来的解释中，我们用 $x, y, z, u, v, \ldots$ 表示任意的个体变量。
   - **个体常量**：$c_0, c_1, c_2, \ldots$。我们用 $a, b, c, \ldots$ 表示任意的常量。
   - **复合项**：通过将函数符号应用于其他项而形成。每个函数符号都有一个**元数（arity）**，即参数的数量。一个 $n$ 元函数符号 $f_{n,m}$ 作用于 $n$ 个参数项 $t_1, \ldots, t_n$，形成项 $f_{n,m}(t_1, \ldots, t_n)$。

   在后续内容中，我们将使用 $f, g, h, \ldots$ 表示任意的函数符号。

   我们将使用 $s, t, t_1, \ldots$ 来表示任意的项。

**注意：**

- 我们可以将**常量**视为**0 元函数符号**。
- 这里引入的变量旨在表示**对象**，而非之前命题逻辑中表示命题的变量。
  
2. **公式（Formulas）**：用于表示命题，形式包括：

   **定义 1.3**

   - **原子公式（Atomic formulas）**：

     - **谓词应用**：$P_{n,m}(t_1, \ldots, t_n)$，其中 $P_{n,m}$ 是一个 $n$ 元谓词符号，$t_1, \ldots, t_n$ 是项。该公式表示由谓词符号 $P_{n,m}$ 所表示的关系在 $t_1, \ldots, t_n$ 所代表的对象之间成立。我们将使用 $P, Q, R, \ldots$ 表示任意的谓词符号。
     - **等式**：$t_1 = t_2$，其中 $t_1$ 和 $t_2$ 是项。等式被视为系统的基本部分。

   - **命题的组合**：使用命题逻辑中的连接词 $\lor, \land, \Rightarrow, \Leftrightarrow, \neg$ 对公式进行组合。

   - **量化公式（Quantified formulas）**：

     - **全称量化**：$\forall x. A$
     - **存在量化**：$\exists x. B$

     其中 $A$ 和 $B$ 是公式，$x$ 是任意变量。

   **量词解释：**

   - **全称量词 $\forall$**（for all）：表示对于所有对象，某个性质都成立。
   - **存在量词 $\exists$**（there exists）：表示存在某个对象，使得某个性质成立。

   因此，量化公式表达了具有特定性质的对象的数量（量）。

#### **直观理解：**

为了强化对量词的直观理解，我们来看一些例子。在每个例子中，量词都有一个特定的应用领域。例如，“对于所有鱼”（for all fish）、“对于所有实数”（for all real numbers）等。

假设我们的讨论域是**自然数**，即量词范围为 $0, 1, 2, \ldots$。此外，假设我们选择了一个**二元谓词符号** $<$，使得 $x < y$ 表示“$y$ 大于 $x$”。

**示例 1：最大值**

假设 $f$ 是一个函数。我们说一个值 $m$ 在函数 $f$ 的值域中，如果存在某个输入 $i$ 使得 $f(i) = m$。要用逻辑形式表示 $m$ 是 $f$ 的值域中的最大值，我们需要：

1. **$m$ 在值域中**：

   $$
   \exists i.\ (f(i) = m)
   $$

2. **$m$ 大于等于值域中的所有元素**：

   $$
   \forall j.\ (f(j) \leq m)
   $$

**完整的性质**可以表示为以上两个公式的合取：

$$
\exists i.\ (f(i) = m) \land \forall j.\ (f(j) \leq m)
$$

**示例 2：量词的顺序**

量词的顺序会影响公式的含义。

- **公式一**：

  $$
  \forall x.\ \exists y.\ (x < y)
  $$

  **解释**：对于每个 $x$，存在一个 $y$ 使得 $x < y$。这在自然数中成立，因为对于任意给定的 $x$，我们可以选择 $y = x + 1$。

- **公式二**：

  $$
  \exists y.\ \forall x.\ (x < y)
  $$

  **解释**：存在一个 $y$，使得对于所有 $x$，都有 $x < y$。这意味着 $y$ 大于所有的自然数，包括自身，这是不可能的，因此该命题为假。

#### **练习：**

1. **练习 1.9**

   使用与上述相同的谓词 $x < y$，表达以下性质：

   “对于每一对不同的数字之间，存在一个数字。”

   **提示**：需要表达“对于任意的 $x$ 和 $y$，如果 $x \neq y$，且 $x < y$，那么存在一个 $z$，使得 $x < z < y$。

2. **练习 1.10**

   如何用逻辑表达函数 $f$ 的以下性质（$f$ 是从自然数到自然数的函数）：

   - **$f$ 是一一对应的函数（Injective）**：

     对于任意的 $x$ 和 $y$，如果 $f(x) = f(y)$，那么 $x = y$。

     $$
     \forall x.\ \forall y.\ (f(x) = f(y) \Rightarrow x = y)
     $$

   - **$f$ 是满射（Surjective）**：

     对于每个自然数 $y$，存在一个自然数 $x$，使得 $f(x) = y$。

     $$
     \forall y.\ \exists x.\ (f(x) = y)
     $$

   - **$f$ 保持关系 $<$**：

     对于任意的 $x$ 和 $y$，如果 $x < y$，那么 $f(x) < f(y)$。

     $$
     \forall x.\ \forall y.\ (x < y \Rightarrow f(x) < f(y))
     $$

#### **变量和替换：**

在给出量词的推导规则之前，我们需要考虑变量的作用，尤其是涉及量化公式中的变量。

**定义 1.4**

- **自由变量（Free Variable）**：在公式中未被量词绑定的变量。
- **绑定变量（Bound Variable）**：在量化公式（如 $\forall x. A$ 或 $\exists x. A$）中被量词绑定的变量。

变量的绑定关系类似于编程语言中的作用域规则，最内层的量词绑定对应的变量。

**示例：**

在公式

$$
\forall y.\ (x > y \land \forall x.\ (P(x) \Rightarrow P(y)) \land Q(y, x))
$$

中：

- 第一个 $x$ 是自由的（未被任何量词绑定）。
- 第二个 $x$ 被内部的 $\forall x$ 所绑定。
- 第三个 $x$ 是自由的。

**替换（Substitution）：**

我们需要将项 $t$ 替换公式 $A$ 中的变量 $x$，记作 $A[t/x]$。在替换时，只替换**自由**的 $x$，而不替换被绑定的 $x$。

**变量捕获（Variable Capture）的问题：**

如果不注意，替换可能导致变量捕获，即原本自由的变量在替换后变成绑定的变量。

**示例：**

考虑公式

$$
\exists y.\ (y > x)
$$

我们希望将 $x$ 替换为 $y + 1$，直接替换会得到

$$
\exists y.\ (y > y + 1)
$$

这里，替换后的 $y$ 被量词 $\exists y$ 绑定，导致原本自由的 $y$ 被捕获，改变了公式的含义。

**解决方法：**

在替换之前，先将被绑定的变量重命名，避免冲突。例如，将 $\exists y.\ (y > x)$ 中的 $y$ 改为 $z$：

$$
\exists z.\ (z > x)
$$

然后进行替换：

$$
\exists z.\ (z > y + 1)
$$

**替换的正式定义：**

- **定义 1.5（项的替换）**

  - $x[t/x] \equiv t$
  - 对于 $y \neq x$，$y[t/x] \equiv y$
  - 对于复合项：

    $$
    (f_{n,m}(t_1, \ldots, t_n))[t/x] \equiv f_{n,m}(t_1[t/x], \ldots, t_n[t/x])
    $$

- **定义 1.6（公式的替换）**

  - **原子公式**：

    - $(P_{n,m}(t_1, \ldots, t_n))[t/x] \equiv P_{n,m}(t_1[t/x], \ldots, t_n[t/x])$
    - $(t_1 = t_2)[t/x] \equiv t_1[t/x] = t_2[t/x]$

  - **逻辑连接词**：

    - $(A \land B)[t/x] \equiv A[t/x] \land B[t/x]$
    - 其他连接词类似处理。

  - **量化公式**：

    - 如果 $A \equiv \forall x. B$，那么 $A[t/x] \equiv A$（因为 $x$ 在 $A$ 中是绑定的，不替换）。
    - 如果 $A \equiv \forall y. B$，且 $y \neq x$：

      - 如果 $y$ 不在 $t$ 中出现，$A[t/x] \equiv \forall y. (B[t/x])$。
      - 如果 $y$ 在 $t$ 中出现，先将 $y$ 重命名为一个不在 $t$ 和 $B$ 中出现的新变量 $z$，然后替换：

        $$
        A[t/x] \equiv \forall z. (B[z/y][t/x])
        $$

    - **存在量词**的替换与全称量词类似处理。

  - **特例**：如果 $x$ 在 $A$ 中不自由出现，那么 $A[t/x] \equiv A$。

**简化记号：**

有时我们会用 $A(t)$ 表示 $A[t/x]$，其中 $x$ 是我们理解的变量。

#### **练习：**

1. **练习 1.11**

   找出以下公式中自由和绑定的变量，并指出每个绑定变量与哪个量词关联：

   $$
   \forall x.\ (x < y \land \forall z.\ (y > z \Rightarrow \exists x.\ (x > z)))
   $$

   **解答：**

   - **自由变量**：$y$（未被任何量词绑定）。
   - **绑定变量**：

     - 第一个 $x$：被最外层的 $\forall x$ 绑定。
     - $z$：被内部的 $\forall z$ 绑定。
     - 内部的 $x$：被 $\exists x$ 绑定。

2. **练习 1.12**

   我们想将公式

   $$
   \forall z.\ \exists y.\ (z < y \land y < z)
   $$

   中的变量 $z$ 重命名为 $y$，需要避免变量捕获。

   **解决方法：**

   - 由于目标是将 $z$ 改为 $y$，但公式中已经有一个 $\exists y$，直接替换会导致变量捕获。
   - 先将内部的 $\exists y$ 中的 $y$ 重命名为另一个不冲突的变量，例如 $w$：

     $$
     \forall z.\ \exists w.\ (z < w \land w < z)
     $$

   - 然后将 $z$ 改为 $y$：

     $$
     \forall y.\ \exists w.\ (y < w \land w < y)
     $$

#### **量词的推导规则：**

现在，我们给出量词的推导规则，解释如何引入和消去量化公式。

**全称量词的引入规则（∀ Introduction）**：

- **规则描述**：如果在一个证明中，得到了公式 $A$，并且变量 $x$ 对于该证明是“任意的”（即 $x$ 不在任何假设中自由出现），那么可以引入全称量词，得到 $\forall x. A$。

- **表示方式**：

  $$
  \frac{A}{\forall x. A} \quad (\forall I)
  $$

- **侧条件（Side Condition）**：$x$ 不在证明 $A$ 时所依赖的任何假设中自由出现。

**全称量词的消去规则（∀ Elimination）**：

- **规则描述**：从 $\forall x. A(x)$，对于任意项 $t$，可以推出 $A(t)$。

- **表示方式**：

  $$
  \frac{\forall x. A(x)}{A(t)} \quad (\forall E)
  $$

- **解释**：全称量词表示对于所有 $x$，$A(x)$ 都成立，因此对于任意的 $t$，$A(t)$ 都成立。

**存在量词的引入规则（∃ Introduction）**：

- **规则描述**：如果对于某个特定的项 $t$，证明了 $A(t)$，那么可以引入存在量词，得到 $\exists x. A(x)$。

- **表示方式**：

  $$
  \frac{A(t)}{\exists x. A(x)} \quad (\exists I)
  $$

- **解释**：证明了存在一个 $x$（即 $t$），使得 $A(x)$ 成立。

**存在量词的消去规则（∃ Elimination）**：

- **规则描述**：如果有 $\exists x. A(x)$ 的证明，并且从 $A(x)$ 可以推出 $B$，且 $x$ 不在 $B$ 或任何假设中自由出现（除了 $A(x)$ 本身），那么可以消解存在量词，得到 $B$。

- **表示方式**：

  $$
  \frac{\begin{array}{c}
  \exists x. A(x) \\
  [A(x)] \\
  \vdots \\
  B
  \end{array}}{B} \quad (\exists E)
  $$

- **侧条件**：$x$ 不在 $B$ 或任何假设中自由出现，除了 $A(x)$。

- **解释**：我们从存在的 $x$（满足 $A(x)$）出发，证明了 $B$，由于 $x$ 是临时引入的（类似于程序中的局部变量），可以将其消解，得到 $B$。

#### **对量词规则的直观理解：**

- **全称量词**：

  - **引入**：当变量 $x$ 对于证明是任意的（不依赖于任何假设中的 $x$），可以推广到所有 $x$。
  - **消去**：全称量词可以应用于任何特定的项 $t$。

- **存在量词**：

  - **引入**：当对于某个特定的项 $t$，$A(t)$ 成立，可以声明存在这样的 $x$。
  - **消去**：从 $\exists x. A(x)$ 出发，可以引入一个新的“任意”变量 $x$，假设 $A(x)$ 成立，进行推导。

**量词与逻辑连接词的类比：**

- **全称量词**类似于无限大的合取：

  $$
  \forall x. A(x) \equiv A(a) \land A(b) \land A(c) \land \ldots
  $$

- **存在量词**类似于无限大的析取：

  $$
  \exists x. A(x) \equiv A(a) \lor A(b) \lor A(c) \lor \ldots
  $$

#### **例子：**

**例子 1：**

假设有以下假设：

- $\theta \equiv \forall x.\ (P(x) \Rightarrow Q(x))$
- $\exists x.\ P(x)$

需要证明：$\exists x.\ Q(x)$。

**证明步骤：**

1. **从存在量词消去规则开始**：

   - 从 $\exists x.\ P(x)$，引入一个新的变量 $x$，假设 $P(x)$ 成立。

2. **应用全称量词的消去规则**：

   - 从 $\theta$，得到 $P(x) \Rightarrow Q(x)$。

3. **应用蕴涵消去规则**：

   - 由 $P(x)$ 和 $P(x) \Rightarrow Q(x)$，得到 $Q(x)$。

4. **应用存在量词的引入规则**：

   - 由 $Q(x)$，得到 $\exists x.\ Q(x)$。

5. **应用存在量词的消去规则，消解 $x$**：

   - 由于 $x$ 是临时引入的，可以消解，最终得到 $\exists x.\ Q(x)$。

**形式化表示：**

$$
\begin{array}{ll}
1. & \exists x.\ P(x) \\
2. & [P(x)]^1 \quad \text{（从存在量词引入 $x$）} \\
3. & \theta \equiv \forall x.\ (P(x) \Rightarrow Q(x)) \\
4. & P(x) \Rightarrow Q(x) \quad (\forall E) \\
5. & Q(x) \quad (\Rightarrow E) \\
6. & \exists x.\ Q(x) \quad (\exists I) \\
7. & \exists x.\ Q(x) \quad (\exists E)^1 \\
\end{array}
$$

**例子 2：**

假设：

- $\forall x.\ (A(x) \lor B(x))$（所有对象要么是苹果，要么是香蕉）
- $\forall x.\ (A(x) \Rightarrow T(x))$（苹果是美味的）
- $\forall x.\ (B(x) \Rightarrow T(x))$（香蕉是美味的）

需要证明：$\forall x.\ T(x)$（所有东西都是美味的）。

**证明步骤：**

1. **对任意的 $y$，从全称量词消去**：

   - $A(y) \lor B(y)$
   - $A(y) \Rightarrow T(y)$
   - $B(y) \Rightarrow T(y)$

2. **对 $A(y)$ 和 $B(y)$ 分别讨论**：

   - 如果 $A(y)$ 成立，由 $A(y) \Rightarrow T(y)$，得到 $T(y)$。
   - 如果 $B(y)$ 成立，由 $B(y) \Rightarrow T(y)$，得到 $T(y)$。

3. **应用析取消去规则**：

   - 由 $A(y) \lor B(y)$ 和上述推导，得到 $T(y)$。

4. **应用全称量词的引入规则**：

   - 由于 $y$ 是任意的，得到 $\forall x.\ T(x)$。

**形式化表示：**

$$
\begin{array}{ll}
1. & \forall x.\ (A(x) \lor B(x)) \\
2. & \forall x.\ (A(x) \Rightarrow T(x)) \\
3. & \forall x.\ (B(x) \Rightarrow T(x)) \\
4. & [y \text{ 是任意的}] \\
5. & A(y) \lor B(y) \quad (\forall E) \\
6. & [A(y)]^1 \\
7. & A(y) \Rightarrow T(y) \quad (\forall E) \\
8. & T(y) \quad (\Rightarrow E) \\
9. & [B(y)]^1 \\
10.& B(y) \Rightarrow T(y) \quad (\forall E) \\
11.& T(y) \quad (\Rightarrow E) \\
12.& T(y) \quad (\lor E)^1 \\
13.& \forall x.\ T(x) \quad (\forall I) \\
\end{array}
$$

**例子 3：**

证明：$\exists y.\ \forall x.\ A(x, y) \Rightarrow \forall x.\ \exists y.\ A(x, y)$。

**证明思路：**

1. **假设 $\exists y.\ \forall x.\ A(x, y)$**。

2. **应用存在量词的消去规则，引入一个特定的 $y$，使得 $\forall x.\ A(x, y)$ 成立。**

3. **对于任意的 $x$，由 $\forall x.\ A(x, y)$，得到 $A(x, y)$。**

4. **应用存在量词的引入规则，得到对于任意的 $x$，都有 $\exists y.\ A(x, y)$。**

5. **应用全称量词的引入规则，得到 $\forall x.\ \exists y.\ A(x, y)$。**

**形式化表示：**

$$
\begin{array}{ll}
1. & [\exists y.\ \forall x.\ A(x, y)]^2 \\
2. & [\forall x.\ A(x, y)]^1 \quad (\exists E) \\
3. & [x \text{ 是任意的}] \\
4. & A(x, y) \quad (\forall E) \\
5. & \exists y.\ A(x, y) \quad (\exists I) \\
6. & \forall x.\ \exists y.\ A(x, y) \quad (\forall I) \\
7. & \forall x.\ \exists y.\ A(x, y) \quad (\exists E)^1 \\
8. & \exists y.\ \forall x.\ A(x, y) \Rightarrow \forall x.\ \exists y.\ A(x, y) \quad (\Rightarrow I)^2 \\
\end{array}
$$

#### **练习：**

1. **练习 1.13**

   解释为什么在推导 $\forall x.\ \exists y.\ A(x, y) \Rightarrow \exists y.\ \forall x.\ A(x, y)$ 时，侧条件会阻止我们构造类似于前面例子的证明。

   **提示**：在尝试应用存在量词的消去规则时，会导致变量 $y$ 不满足侧条件（即 $y$ 不应在结论或其他假设中自由出现）。

2. **练习 1.14**

   假设变量 $x$ 不在 $B$ 中自由出现，证明以下公式是等价的，即可以在彼此的假设下证明对方：

   - $\forall x.\ (A(x) \Rightarrow B)$
   - $(\exists x.\ A(x)) \Rightarrow B$

   **解答**：

   - **从 $\forall x.\ (A(x) \Rightarrow B)$ 推出 $(\exists x.\ A(x)) \Rightarrow B$**：

     1. 假设 $\exists x.\ A(x)$。
     2. 从存在量词消去规则，引入一个 $x$，使得 $A(x)$ 成立。
     3. 由 $\forall x.\ (A(x) \Rightarrow B)$，得到 $A(x) \Rightarrow B$。
     4. 由 $A(x)$ 和 $A(x) \Rightarrow B$，得到 $B$。
     5. 由于 $B$ 不依赖于 $x$，可以消解 $x$，得到 $B$。

   - **从 $(\exists x.\ A(x)) \Rightarrow B$ 推出 $\forall x.\ (A(x) \Rightarrow B)$**：

     1. 取任意的 $x$，假设 $A(x)$ 成立。
     2. 由存在量词的引入规则，得到 $\exists x.\ A(x)$。
     3. 由假设 $(\exists x.\ A(x)) \Rightarrow B$，得到 $B$。
     4. 由于 $B$ 不依赖于 $x$，得到 $A(x) \Rightarrow B$。
     5. 由于 $x$ 是任意的，得到 $\forall x.\ (A(x) \Rightarrow B)$。

3. **练习 1.15**

   使用前一练习的结果，或者其他方法，论证以下公式是等价的：

   - $\neg (\exists x.\ A(x))$
   - $\forall x.\ \neg A(x)$

   并证明：

   - $\exists x.\ \neg A(x) \Rightarrow \neg \forall x.\ A(x)$

   **解答**：

   - **等价性**：

     - $\neg (\exists x.\ A(x)) \equiv \forall x.\ \neg A(x)$

       **证明**：

       - 如果不存在 $x$ 满足 $A(x)$，那么对于所有的 $x$，$A(x)$ 都不成立，即 $\forall x.\ \neg A(x)$。
       - 反之，如果对于所有的 $x$，$A(x)$ 都不成立，那么不存在 $x$ 满足 $A(x)$。

   - **证明 $\exists x.\ \neg A(x) \Rightarrow \neg \forall x.\ A(x)$**：

     - 假设存在一个 $x$，使得 $\neg A(x)$ 成立。
     - 假设 $\forall x.\ A(x)$ 成立，则对于该 $x$，$A(x)$ 成立，与 $\neg A(x)$ 矛盾。
     - 因此，$\forall x.\ A(x)$ 不成立，即 $\neg \forall x.\ A(x)$。

   - **是否期望反过来的结果成立？**

     - $\neg \forall x.\ A(x) \Rightarrow \exists x.\ \neg A(x)$

     - **在经典逻辑中**，该结果成立，因为如果并非对所有的 $x$ 都有 $A(x)$，那么存在一个 $x$，使得 $A(x)$ 不成立。

     - **在直觉主义逻辑中**，该结果不一定成立，因为拒绝使用排中律。

#### **总结：**

本节深入探讨了谓词逻辑的语法和推导规则。我们引入了项和公式的概念，定义了量词以及它们的推导规则，强调了变量绑定和替换中的注意事项。通过详细的例子和练习，加深了对全称量词和存在量词的理解，为进一步学习逻辑和类型理论奠定了坚实的基础。

如果您有任何疑问或需要进一步的解释，请随时提出！

### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------