[toc]





### ---------------------------------

### 第31章 经典逻辑 (Classical Logic)

在**构造逻辑**(Constructive Logic)中，一个命题被认为为真，当且仅当它有一个**证明**(Proof)，即从公理和假设中推导出来的证明；一个命题被认为为假，当且仅当它有一个**反驳**(Refutation)，即在假设它为真的情况下导出一个矛盾。构造逻辑是一种关于**正面证据**(Positive Evidence)的逻辑。要**断言**(Affirm)或**否定**(Deny)一个命题，需要提供一个证明，要么是命题本身的证明，要么是在假设它有证明的情况下，导出矛盾的证明。

然而，我们并不总是能够断言或否定一个命题。一个**开放问题**(Open Problem)是指我们既没有证明也没有反驳的问题——因此，从构造逻辑的角度来看，它既不为真也不为假。

---

与此相对，**经典逻辑**(Classical Logic)（即我们在学校中学到的逻辑）是一种**完美信息**(Perfect Information)的逻辑，在其中每个命题要么为真，要么为假。我们可以说，经典逻辑对应于“**上帝视角**”(God's View)的世界——不存在开放问题，每个命题要么为真，要么为假。

换句话说，断言每个命题要么为真，要么为假，相当于弱化了真理的概念，以包含所有不为假的命题，这与构造（以及经典）逻辑中对虚假的有效解释（即所有不为真的内容）形成对偶。**真理**(Truth)和**虚假**(Falsity)的对称性很有吸引力，但为此需要付出代价：在经典情况下，**逻辑连接词**(Logical Connectives)的含义比在构造情况下更弱。

---

一个主要的例子是**排中律**(Law of the Excluded Middle)，即断言对于所有命题$\varphi$，$\varphi \lor \neg\varphi$为真：

$$
\forall \varphi, \quad \varphi \lor \neg\varphi \quad \text{为真}
$$

**公式解析：**

- $\forall \varphi$：对于所有命题$\varphi$。
- $\varphi \lor \neg\varphi$：命题$\varphi$与其否定$\neg\varphi$的**析取**(Disjunction)。
- $\varphi \lor \neg\varphi$为真：表示该命题总是成立。

**解释：**

- **构造性地**，这一原则并非普遍有效，因为这将意味着每个命题要么有一个证明，要么有一个反驳，而这显然并非如此。
  
- **经典逻辑中**，排中律是有效的，因为每个命题要么被认为为假，要么不为假（在经典逻辑中，这被认为是为真，与构造逻辑相反）。

尽管如此，经典逻辑与构造逻辑是一致的，因为构造逻辑并不反驳经典逻辑。正如我们所见，构造逻辑证明了排中律是正面地不可反驳的（其**双重否定**(Double Negation)在构造逻辑中为真）。也就是说：

$$
\neg\neg (\varphi \lor \neg\varphi) \quad \text{在构造逻辑中为真}
$$

**公式解析：**

- $\neg\neg (\varphi \lor \neg\varphi)$：对$\varphi \lor \neg\varphi$取双重否定。
- 表示排中律的双重否定在构造逻辑中成立。

**解释：**

- 这表明，虽然构造逻辑不接受排中律作为普遍真理，但它承认排中律的双重否定为真，即排中律是不可反驳的。

---

这表明**构造逻辑**比**经典逻辑**更强（更具表达力），因为它可以表达更多的区分（即在**断言**(Affirmation)和**不可反驳性**(Irrefutability)之间），并且因为它与经典逻辑一致（可以在不引起矛盾的情况下添加排中律）。

---

在构造逻辑中，**证明**具有**计算内容**(Computational Content)：它们可以作为程序执行，其行为受其类型的约束。在经典逻辑中，证明也具有计算内容，但在比构造逻辑更弱的意义上。

与其积极地断言一个命题，不如说在经典逻辑中，一个证明是一个**无法被反驳**(Unrefutable)的计算。

**从计算的角度来看：**

- **反驳**(Refutation)：由一个**续延**(Continuation)或**控制栈**(Control Stack)组成，它接受一个命题的证明并从中导出矛盾。
- 因此，在经典逻辑中，一个命题的证明是一个计算，当给定该命题的反驳时，导出一个矛盾，见证了反驳它的**不可能性**(Impossibility of Refuting It)。

**因此：**

- **排中律**有一个证明，正是因为它是不可反驳的。

---

### 31.1 经典逻辑 (Classical Logic)

在**构造逻辑**(Constructive Logic)中，一个逻辑连接词是通过给出其**引入规则**(Introduction Rules)和**消去规则**(Elimination Rules)来定义的。

在**经典逻辑**(Classical Logic)中，一个逻辑连接词是通过给出其**真值条件**(Truth Conditions)和**假值条件**(Falsity Conditions)来定义的。

- 其**真值规则**对应于引入规则。
- 其**假值规则**对应于消去规则。

**真理**(Truth)和**虚假**(Falsity)的对称性通过**间接证明原则**(Principle of Indirect Proof)来表达：

- 要证明$\varphi$为真，只需证明$\varphi$为假会导致矛盾。
- 反之，要证明$\varphi$为假，只需证明$\varphi$为真会导致矛盾。

**注意：**

- 其中第二点（要证明$\varphi$为假，只需证明$\varphi$为真会导致矛盾）在构造逻辑中是有效的。
- 第一点（要证明$\varphi$为真，只需证明$\varphi$为假会导致矛盾）本质上是**经典的**，表达了**间接证明原则**。

---

### 章节内容概览

- **经典逻辑** (Classical Logic)
- **可证明性和可反驳性** (Provability and Refutability)
- **证明和反驳** (Proofs and Refutations)
- **导出消去形式** (Deriving Elimination Forms)
- **证明动力学** (Proof Dynamics)
- **排中律** (Law of the Excluded Middle)
- **双重否定翻译** (Double-Negation Translation)
- **笔记** (Notes)

---

**解释：**

- **可证明性和可反驳性**：讨论命题是否可以被证明或反驳的条件和标准。
- **证明和反驳**：深入研究证明和反驳的结构、特点以及它们之间的关系。
- **导出消去形式**：探讨如何从引入规则推导出消去规则，或者从逻辑连接词的定义推导出其使用方式。
- **证明动力学**：研究证明过程中的动态行为，特别是证明与计算之间的关系。
- **排中律**：详细分析排中律在经典逻辑和构造逻辑中的地位和作用。
- **双重否定翻译**：介绍一种将经典逻辑命题翻译为构造逻辑形式的方法，通过双重否定来保持逻辑的等价性。
- **笔记**：可能包含对本章内容的补充说明、历史背景或进一步的阅读建议。

---

### 总结

本章探讨了**经典逻辑**与**构造逻辑**之间的区别和联系，重点关注它们对命题真值的理解方式。经典逻辑假设每个命题要么为真，要么为假，这种观点被形象地描述为“上帝视角”。在这种逻辑中，排中律是普遍有效的。

相比之下，构造逻辑强调证明的重要性，一个命题的真或假需要有明确的证明或反驳。构造逻辑不接受排中律作为普遍真理，但承认其双重否定为真，表明排中律是不可反驳的。

此外，本章还讨论了两种逻辑中**证明**的计算内容。在构造逻辑中，证明可以直接视为程序，其行为由类型约束；而在经典逻辑中，证明的计算内容更弱，更多地体现为不可反驳性。

通过对经典逻辑和构造逻辑的比较，我们可以更深入地理解逻辑系统的基础，以及它们在**编程语言设计原理**(Programming Language Design Principles)中的应用。这种理解有助于我们在设计和分析编程语言时，更好地把握语言的逻辑特性和计算模型。

### ---------------------------------

### 31.1.1 可证明性和可反驳性 (Provability and Refutability)

在**经典逻辑** (Classical Logic) 中，有三种基本的判断形式：

1. **$\varphi$ 为真** ($\varphi$ true)：表示命题 $\varphi$ 是**可证明的** (provable)。
2. **$\varphi$ 为假** ($\varphi$ false)：表示命题 $\varphi$ 是**可反驳的** (refutable)。
3. **矛盾** ($\#$)：表示已经导出了一个**矛盾** (contradiction)。

---

这些判断形式被扩展到**假设判断** (hypothetical judgments)，在其中我们同时接受可证明性和可反驳性的假设：

$$
\varphi_1 \ \text{false}, \dotsc, \varphi_m \ \text{false} \quad \psi_1 \ \text{true}, \dotsc, \psi_n \ \text{true} \vdash J.
$$

**公式解析：**

- **$\varphi_1 \ \text{false}, \dotsc, \varphi_m \ \text{false}$**：一组**假设为假**的命题。
- **$\psi_1 \ \text{true}, \dotsc, \psi_n \ \text{true}$**：一组**假设为真**的命题。
- **$J$**：一个判断，可以是命题为真、命题为假或矛盾。
- 假设被分成两个区域：
  - **$\Delta$**：包含所有**虚假假设** (falsity assumptions)。
  - **$\Gamma$**：包含所有**真值假设** (truth assumptions)。

---

**经典逻辑**的规则是围绕着**真理** (truth) 和 **虚假** (falsity) 之间的对称性组织的，这种对称性通过**矛盾判断** (contradiction judgment) 来调和。

---

#### 假设判断的反身性 (Reflexivity of Hypothetical Judgment)

- **(31.1a)** 从假设 $\varphi$ 为假，可以推导出 $\varphi$ 为假：

  $$
  \frac{}{\Delta, \varphi \ \text{false} \quad \Gamma \vdash \varphi \ \text{false}} \tag{31.1a}
  $$

- **(31.1b)** 从假设 $\varphi$ 为真，可以推导出 $\varphi$ 为真：

  $$
  \frac{}{\Delta \quad \Gamma, \varphi \ \text{true} \vdash \varphi \ \text{true}} \tag{31.1b}
  $$

**公式解析：**

- **$\Delta$**：虚假假设的集合。
- **$\Gamma$**：真值假设的集合。
- **$\vdash$**：推导符号，表示在假设下可以推导出某个结论。

**解释：**

- (31.1a) 和 (31.1b) 表明，在包含 $\varphi$ 为假或 $\varphi$ 为真的假设下，可以直接得出 $\varphi$ 为假或为真的结论。这体现了判断的**反身性** (reflexivity)。

---

#### 结构性质 (Structural Properties)

其余的规则被陈述为使得**弱化** (weakening)、**合并** (contraction) 和**传递性** (transitivity) 的结构性质是可容许的。

---

#### 矛盾的产生 (Contradiction Arises)

当一个命题被判断为**既为真又为假**时，就会产生一个矛盾。

- **(31.1c)** 如果在同一假设下，$\varphi$ 被推导为真，又被推导为假，那么就得出矛盾：

  $$
  \frac{\Delta \ \Gamma \vdash \varphi \ \text{false} \quad \Delta \ \Gamma \vdash \varphi \ \text{true}}{\Delta \ \Gamma \vdash \#} \tag{31.1c}
  $$

- **(31.1d)** 如果在假设 $\varphi$ 为假下，导出了矛盾，那么可以推导出 $\varphi$ 为真：

  $$
  \frac{\Delta, \varphi \ \text{false} \quad \Gamma \vdash \#}{\Delta \ \Gamma \vdash \varphi \ \text{true}} \tag{31.1d}
  $$

- **(31.1e)** 如果在假设 $\varphi$ 为真下，导出了矛盾，那么可以推导出 $\varphi$ 为假：

  $$
  \frac{\Delta \quad \Gamma, \varphi \ \text{true} \vdash \#}{\Delta \ \Gamma \vdash \varphi \ \text{false}} \tag{31.1e}
  $$

**公式解析：**

- **$\#$**：矛盾。
- (31.1c) 的分子部分表示在相同的假设下，$\varphi$ 被推导为真和假。
- (31.1d) 和 (31.1e) 表示通过导出矛盾，可以推导出命题的相反真值。

**解释：**

- (31.1c) 表示矛盾的产生条件。
- (31.1d) 和 (31.1e) 表明，如果假设某个命题为真或假会导致矛盾，那么该命题的真值就是相反的。

---

#### 真理和虚假的基本性质

- **真理的性质：**

  - **(31.1f)** **真命题** (Truth) $\top$ 是**平凡地为真** (trivially true)，并且**不能被反驳**：

    $$
    \frac{}{\Delta \ \Gamma \vdash \top \ \text{true}} \tag{31.1f}
    $$

- **虚假的性质：**

  - **(31.1j)** **假命题** (Falsity) $\bot$ 是**平凡地为假** (trivially false)，并且**不能被证明**：

    $$
    \frac{}{\Delta \ \Gamma \vdash \bot \ \text{false}} \tag{31.1j}
    $$

**解释：**

- **$\top$** (真命题, Truth)：总是为真，无法被反驳。
- **$\bot$** (假命题, Falsehood)：总是为假，无法被证明。

---

#### 合取的规则 (Rules for Conjunction)

- **(31.1g)** **合取为真**：如果两个合取项都为真，那么合取为真：

  $$
  \frac{\Delta \ \Gamma \vdash \varphi_1 \ \text{true} \quad \Delta \ \Gamma \vdash \varphi_2 \ \text{true}}{\Delta \ \Gamma \vdash \varphi_1 \wedge \varphi_2 \ \text{true}} \tag{31.1g}
  $$

- **(31.1h)** **合取为假（左项为假）**：如果 $\varphi_1$ 为假，那么合取为假：

  $$
  \frac{\Delta \ \Gamma \vdash \varphi_1 \ \text{false}}{\Delta \ \Gamma \vdash \varphi_1 \wedge \varphi_2 \ \text{false}} \tag{31.1h}
  $$

- **(31.1i)** **合取为假（右项为假）**：如果 $\varphi_2$ 为假，那么合取为假：

  $$
  \frac{\Delta \ \Gamma \vdash \varphi_2 \ \text{false}}{\Delta \ \Gamma \vdash \varphi_1 \wedge \varphi_2 \ \text{false}} \tag{31.1i}
  $$

**公式解析：**

- **$\varphi_1 \wedge \varphi_2$**：合取 (conjunction)。
- (31.1g) 表示同时证明 $\varphi_1$ 和 $\varphi_2$ 为真，可以得出合取为真。
- (31.1h) 和 (31.1i) 表示只要有一个合取项为假，合取就为假。

---

#### 析取的规则 (Rules for Disjunction)

- **(31.1k)** **析取为真（左项为真）**：如果 $\varphi_1$ 为真，那么析取为真：

  $$
  \frac{\Delta \ \Gamma \vdash \varphi_1 \ \text{true}}{\Delta \ \Gamma \vdash \varphi_1 \lor \varphi_2 \ \text{true}} \tag{31.1k}
  $$

- **(31.1l)** **析取为真（右项为真）**：如果 $\varphi_2$ 为真，那么析取为真：

  $$
  \frac{\Delta \ \Gamma \vdash \varphi_2 \ \text{true}}{\Delta \ \Gamma \vdash \varphi_1 \lor \varphi_2 \ \text{true}} \tag{31.1l}
  $$

- **(31.1m)** **析取为假**：如果 $\varphi_1$ 和 $\varphi_2$ 都为假，那么析取为假：

  $$
  \frac{\Delta \ \Gamma \vdash \varphi_1 \ \text{false} \quad \Delta \ \Gamma \vdash \varphi_2 \ \text{false}}{\Delta \ \Gamma \vdash \varphi_1 \lor \varphi_2 \ \text{false}} \tag{31.1m}
  $$

**解释：**

- **$\varphi_1 \lor \varphi_2$**：析取 (disjunction)。
- (31.1k) 和 (31.1l) 表示只要有一个析取项为真，析取就为真。
- (31.1m) 表示只有当两个析取项都为假时，析取才为假。

---

#### 否定的规则 (Rules for Negation)

- **(31.1n)** 如果 $\varphi$ 为假，那么 $\neg\varphi$ 为真：

  $$
  \frac{\Delta \ \Gamma \vdash \varphi \ \text{false}}{\Delta \ \Gamma \vdash \neg\varphi \ \text{true}} \tag{31.1n}
  $$

- **(31.1o)** 如果 $\varphi$ 为真，那么 $\neg\varphi$ 为假：

  $$
  \frac{\Delta \ \Gamma \vdash \varphi \ \text{true}}{\Delta \ \Gamma \vdash \neg\varphi \ \text{false}} \tag{31.1o}
  $$

**解释：**

- **$\neg\varphi$**：否定 (negation)。
- 否定运算反转每个判断的意义：
  - 如果 $\varphi$ 为假，那么 $\neg\varphi$ 为真。
  - 如果 $\varphi$ 为真，那么 $\neg\varphi$ 为假。

---

#### 蕴涵的规则 (Rules for Implication)

- **(31.1p)** **蕴涵为真**：如果在假设 $\varphi_1$ 为真下，可以推导出 $\varphi_2$ 为真，那么蕴涵为真：

  $$
  \frac{\Delta \quad \Gamma, \varphi_1 \ \text{true} \vdash \varphi_2 \ \text{true}}{\Delta \ \Gamma \vdash \varphi_1 \supset \varphi_2 \ \text{true}} \tag{31.1p}
  $$

- **(31.1q)** **蕴涵为假**：如果 $\varphi_1$ 为真，且 $\varphi_2$ 为假，那么蕴涵为假：

  $$
  \frac{\Delta \ \Gamma \vdash \varphi_1 \ \text{true} \quad \Delta \ \Gamma \vdash \varphi_2 \ \text{false}}{\Delta \ \Gamma \vdash \varphi_1 \supset \varphi_2 \ \text{false}} \tag{31.1q}
  $$

**公式解析：**

- **$\varphi_1 \supset \varphi_2$**：蕴涵 (implication)。
- (31.1p) 表示如果在假设 $\varphi_1$ 为真下可以得出 $\varphi_2$ 为真，那么 $\varphi_1 \supset \varphi_2$ 为真。
- (31.1q) 表示如果 $\varphi_1$ 为真而 $\varphi_2$ 为假，那么 $\varphi_1 \supset \varphi_2$ 为假。

**解释：**

- 蕴涵的真值取决于前件和后件的真值：
  - **为真**：只要在假设前件为真下，能推导出后件为真。
  - **为假**：当前件为真但后件为假。

---

### 总结

上述规则完整地描述了**经典逻辑**中各个逻辑连接词的推导规则，体现了真理和虚假的对称性，以及矛盾在逻辑推导中的作用。

- **假设判断**的结构允许同时包含真值和虚假的假设，并通过**矛盾**来调和真理和虚假的推导。
- **真理和虚假**的基本性质定义了 $\top$ 和 $\bot$ 的行为。
- **逻辑连接词**的规则（合取、析取、否定、蕴涵）详细说明了它们在经典逻辑中的真值条件和推导方式。

通过这些规则，我们可以在经典逻辑的框架下，对命题的可证明性和可反驳性进行形式化的推理和证明。

### ---------------------------------

### 31.1.2 证明和反驳 (Proofs and Refutations)

为了解释**经典证明**（Classical Proofs）的**动力学**（Dynamics），我们首先引入一个用于**证明**（proofs）和**反驳**（refutations）的显式语法。我们将为经典逻辑定义三个带有显式推导的**假设判断**（hypothetical judgments）：

1. **$\Delta \ \Gamma \vdash p : \varphi$**：表示$p$是$\varphi$的一个**证明**（proof of $\varphi$）。

2. **$\Delta \ \Gamma \vdash k \div \varphi$**：表示$k$是$\varphi$的一个**反驳**（refutation of $\varphi$）。

3. **$\Delta \ \Gamma \vdash k \# p$**：表示$k$和$p$是**矛盾的**（contradictory）。

---

**公式解析：**

- **$\Delta$**：**虚假假设**（falsity assumptions）的上下文，形式为$u_1 \div \varphi_1, \dotsc, u_m \div \varphi_m$，其中$m \geq 0$，变量$u_1, \dotsc, u_m$代表反驳。

- **$\Gamma$**：**真值假设**（truth assumptions）的上下文，形式为$x_1 : \psi_1, \dotsc, x_n : \psi_n$，其中$n \geq 0$，变量$x_1, \dotsc, x_n$代表证明。

- **$p$**：**证明项**（proof term）。

- **$k$**：**反驳项**（refutation term）。

---

#### 证明和反驳的语法 (Syntax of Proofs and Refutations)

证明和反驳的语法由以下语法规则定义：

- **证明项**（Prf $p$）：

  - **$\text{>T } \langle \rangle$**：**真理**（truth），对应于命题$\top$的证明。

  - **$\wedge\text{T}(p_1; p_2)$**：**合取**（conjunction），记为$\langle p_1, p_2 \rangle$。

  - **$\vee\text{T}[l](p)$**：**析取左**（disjunction left），记为$l \cdot p$。

  - **$\vee\text{T}[r](p)$**：**析取右**（disjunction right），记为$r \cdot p$。

  - **$\neg\text{T}(k)$**：**否定**（negation），记为$\text{not}(k)$。

  - **$\supset\text{T}(\varphi; x.p)$**：**蕴涵**（implication），记为$\lambda (x:\varphi) p$。

- **反驳项**（Ref $k$）：

  - **$\bot\text{F } \text{abort}$**：**假命题**（falsehood），对应于命题$\bot$的反驳。

  - **$\wedge\text{F}[l](k)$**：**合取左**（conjunction left），记为$\text{fst}; k$。

  - **$\wedge\text{F}[r](k)$**：**合取右**（conjunction right），记为$\text{snd}; k$。

  - **$\vee\text{F}(k_1; k_2)$**：**析取**（disjunction），记为$\text{case}(k_1; k_2)$。

  - **$\neg\text{F}(p)$**：**否定**（negation），记为$\text{not}(p)$。

  - **$\supset\text{F}(p; k)$**：**蕴涵**（implication），记为$\text{ap}(p); k$。

---

**符号解析：**

- **$\langle \rangle$**：表示**真命题**（truth）$\top$的证明。

- **$\langle p_1, p_2 \rangle$**：表示**合取** $\varphi_1 \wedge \varphi_2$ 的证明，$p_1$ 和 $p_2$ 分别是 $\varphi_1$ 和 $\varphi_2$ 的证明。

- **$l \cdot p$**：表示**析取左项**的证明，$p$ 是 $\varphi_1$ 的证明，形成 $\varphi_1 \lor \varphi_2$ 的证明。

- **$r \cdot p$**：表示**析取右项**的证明，$p$ 是 $\varphi_2$ 的证明，形成 $\varphi_1 \lor \varphi_2$ 的证明。

- **$\text{not}(k)$**：表示 $\neg\varphi$ 的证明，其中 $k$ 是 $\varphi$ 的反驳。

- **$\lambda (x:\varphi) p$**：表示**蕴涵** $\varphi \supset \psi$ 的证明，$p$ 是在假设 $x:\varphi$ 下得到的 $\psi$ 的证明。

- **$\text{abort}$**：表示对 $\bot$ 的反驳。

- **$\text{fst}; k$**：表示**合取左项**的反驳，$k$ 是 $\varphi_1$ 的反驳，用于 $\varphi_1 \wedge \varphi_2$ 的反驳。

- **$\text{snd}; k$**：表示**合取右项**的反驳，$k$ 是 $\varphi_2$ 的反驳，用于 $\varphi_1 \wedge \varphi_2$ 的反驳。

- **$\text{case}(k_1; k_2)$**：表示**析取**的反驳，$k_1$ 和 $k_2$ 分别是 $\varphi_1$ 和 $\varphi_2$ 的反驳。

- **$\text{ap}(p); k$**：表示**蕴涵**的反驳，$p$ 是 $\varphi_1 \supset \varphi_2$ 的证明，$k$ 是 $\varphi_2$ 的反驳。

---

#### 矛盾的产生 (Contradictions Arise)

当一个命题既被证明为真又被反驳为假时，就会产生**矛盾**（contradiction）：

$$
\frac{\Delta \ \Gamma \vdash k \div \varphi \quad \Delta \ \Gamma \vdash p : \varphi}{\Delta \ \Gamma \vdash k \# p} \tag{31.2a}
$$

**公式解析：**

- **$\Delta \ \Gamma \vdash k \div \varphi$**：$k$ 是 $\varphi$ 的反驳。

- **$\Delta \ \Gamma \vdash p : \varphi$**：$p$ 是 $\varphi$ 的证明。

- **$\Delta \ \Gamma \vdash k \# p$**：$k$ 和 $p$ 构成矛盾。

---

#### 真理和虚假的对称定义 (Symmetric Definition of Truth and Falsity)

- **(31.2b)** 如果在假设 $u \div \varphi$ 下，$k$ 和 $p$ 构成矛盾，那么可以推出 $p$ 是 $\varphi$ 的证明：

  $$
  \frac{\Delta, u \div \varphi \quad \Gamma \vdash k \# p}{\Delta \ \Gamma \vdash \text{ccr}(u \div \varphi.k \# p) : \varphi} \tag{31.2b}
  $$

- **(31.2c)** 类似地，如果在假设 $x : \varphi$ 下，$k$ 和 $p$ 构成矛盾，那么可以推出 $k$ 是 $\varphi$ 的反驳：

  $$
  \frac{\Delta \quad \Gamma, x : \varphi \vdash k \# p}{\Delta \ \Gamma \vdash \text{ccp}(x : \varphi.k \# p) \div \varphi} \tag{31.2c}
  $$

**符号解析：**

- **$\text{ccr}$**（从矛盾构造证明）：**从矛盾中构造 $\varphi$ 的证明**。

- **$\text{ccp}$**（从矛盾构造反驳）：**从矛盾中构造 $\varphi$ 的反驳**。

---

#### 反身性 (Reflexivity)

- **(31.2d)** 在 $\Delta$ 中假设 $u \div \varphi$，可以直接推出 $u$ 是 $\varphi$ 的反驳：

  $$
  \frac{}{\Delta, u \div \varphi \quad \Gamma \vdash u \div \varphi} \tag{31.2d}
  $$

- **(31.2e)** 在 $\Gamma$ 中假设 $x : \varphi$，可以直接推出 $x$ 是 $\varphi$ 的证明：

  $$
  \frac{}{\Delta \quad \Gamma, x : \varphi \vdash x : \varphi} \tag{31.2e}
  $$

---

#### 真理和虚假的基本性质

- **(31.2f)** **真命题**（truth）$\top$ 是**平凡地为真**（trivially true），不能被反驳：

  $$
  \frac{}{\Delta \ \Gamma \vdash \langle \rangle : \top} \tag{31.2f}
  $$

- **(31.2j)** **假命题**（falsehood）$\bot$ 是**平凡地为假**（trivially false），不能被证明：

  $$
  \frac{}{\Delta \ \Gamma \vdash \text{abort} \div \bot} \tag{31.2j}
  $$

---

#### 合取的规则 (Rules for Conjunction)

- **(31.2g)** **合取为真**：如果 $p_1$ 是 $\varphi_1$ 的证明，$p_2$ 是 $\varphi_2$ 的证明，则 $\langle p_1, p_2 \rangle$ 是 $\varphi_1 \wedge \varphi_2$ 的证明：

  $$
  \frac{\Delta \ \Gamma \vdash p_1 : \varphi_1 \quad \Delta \ \Gamma \vdash p_2 : \varphi_2}{\Delta \ \Gamma \vdash \langle p_1, p_2 \rangle : \varphi_1 \wedge \varphi_2} \tag{31.2g}
  $$

- **(31.2h)** **合取为假（左项为假）**：如果 $k_1$ 是 $\varphi_1$ 的反驳，则 $\text{fst}; k_1$ 是 $\varphi_1 \wedge \varphi_2$ 的反驳：

  $$
  \frac{\Delta \ \Gamma \vdash k_1 \div \varphi_1}{\Delta \ \Gamma \vdash \text{fst}; k_1 \div \varphi_1 \wedge \varphi_2} \tag{31.2h}
  $$

- **(31.2i)** **合取为假（右项为假）**：如果 $k_2$ 是 $\varphi_2$ 的反驳，则 $\text{snd}; k_2$ 是 $\varphi_1 \wedge \varphi_2$ 的反驳：

  $$
  \frac{\Delta \ \Gamma \vdash k_2 \div \varphi_2}{\Delta \ \Gamma \vdash \text{snd}; k_2 \div \varphi_1 \wedge \varphi_2} \tag{31.2i}
  $$

---

#### 析取的规则 (Rules for Disjunction)

- **(31.2k)** **析取为真（左项为真）**：如果 $p_1$ 是 $\varphi_1$ 的证明，则 $l \cdot p_1$ 是 $\varphi_1 \lor \varphi_2$ 的证明：

  $$
  \frac{\Delta \ \Gamma \vdash p_1 : \varphi_1}{\Delta \ \Gamma \vdash l \cdot p_1 : \varphi_1 \lor \varphi_2} \tag{31.2k}
  $$

- **(31.2l)** **析取为真（右项为真）**：如果 $p_2$ 是 $\varphi_2$ 的证明，则 $r \cdot p_2$ 是 $\varphi_1 \lor \varphi_2$ 的证明：

  $$
  \frac{\Delta \ \Gamma \vdash p_2 : \varphi_2}{\Delta \ \Gamma \vdash r \cdot p_2 : \varphi_1 \lor \varphi_2} \tag{31.2l}
  $$

- **(31.2m)** **析取为假**：如果 $k_1$ 是 $\varphi_1$ 的反驳，$k_2$ 是 $\varphi_2$ 的反驳，则 $\text{case}(k_1; k_2)$ 是 $\varphi_1 \lor \varphi_2$ 的反驳：

  $$
  \frac{\Delta \ \Gamma \vdash k_1 \div \varphi_1 \quad \Delta \ \Gamma \vdash k_2 \div \varphi_2}{\Delta \ \Gamma \vdash \text{case}(k_1; k_2) \div \varphi_1 \lor \varphi_2} \tag{31.2m}
  $$

---

#### 否定的规则 (Rules for Negation)

- **(31.2n)** 如果 $k$ 是 $\varphi$ 的反驳，则 $\text{not}(k)$ 是 $\neg\varphi$ 的证明：

  $$
  \frac{\Delta \ \Gamma \vdash k \div \varphi}{\Delta \ \Gamma \vdash \text{not}(k) : \neg\varphi} \tag{31.2n}
  $$

- **(31.2o)** 如果 $p$ 是 $\varphi$ 的证明，则 $\text{not}(p)$ 是 $\neg\varphi$ 的反驳：

  $$
  \frac{\Delta \ \Gamma \vdash p : \varphi}{\Delta \ \Gamma \vdash \text{not}(p) \div \neg\varphi} \tag{31.2o}
  $$

---

#### 蕴涵的规则 (Rules for Implication)

- **(31.2p)** **蕴涵为真**：如果在假设 $x : \varphi_1$ 下，$p_2$ 是 $\varphi_2$ 的证明，则 $\lambda (x:\varphi_1) p_2$ 是 $\varphi_1 \supset \varphi_2$ 的证明：

  $$
  \frac{\Delta \quad \Gamma, x : \varphi_1 \vdash p_2 : \varphi_2}{\Delta \ \Gamma \vdash \lambda (x:\varphi_1) p_2 : \varphi_1 \supset \varphi_2} \tag{31.2p}
  $$

- **(31.2q)** **蕴涵为假**：如果 $p_1$ 是 $\varphi_1$ 的证明，$k_2$ 是 $\varphi_2$ 的反驳，则 $\text{ap}(p_1); k_2$ 是 $\varphi_1 \supset \varphi_2$ 的反驳：

  $$
  \frac{\Delta \ \Gamma \vdash p_1 : \varphi_1 \quad \Delta \ \Gamma \vdash k_2 \div \varphi_2}{\Delta \ \Gamma \vdash \text{ap}(p_1); k_2 \div \varphi_1 \supset \varphi_2} \tag{31.2q}
  $$

**符号解析：**

- **$\text{ap}(p_1)$**：**应用**（application），表示将证明 $p_1$ 应用于反驳 $k_2$。

---

### 总结

在这一节中，我们详细定义了经典逻辑中**证明**和**反驳**的显式语法，以及它们的推导规则。这些规则体现了经典逻辑中真理和虚假的对称性，以及**矛盾**（contradiction）在逻辑推导中的作用。

- **证明项**（proof terms）是对真理判断的证据，**反驳项**（refutation terms）是对虚假判断的证据。

- **矛盾**由证明和反驳的并置来见证，当一个命题既被证明为真又被反驳为假时，就会产生矛盾。

- 通过定义证明和反驳的语法，我们可以形式化地描述经典逻辑的**动力学**，并深入理解逻辑连接词在经典逻辑中的行为。

这些规则为后续研究**证明动力学**（Proof Dynamics）和经典逻辑中的计算内容奠定了基础。

### ---------------------------------

### 31.2 导出消去形式 (Deriving Elimination Forms)

在**经典逻辑** (Classical Logic) 中，为了实现**真理** (Truth) 和**虚假** (Falsity) 之间的对称性，我们必须经常依赖于**间接证明原则** (Principle of Indirect Proof)：要证明一个命题为真，我们常常需要从假设它为假的情况下导出一个矛盾。

#### 间接证明的代价

例如，要在经典逻辑中证明以下命题：

$$
(\varphi \wedge (\psi \wedge \theta)) \supset (\theta \wedge \varphi)
$$

**公式解析：**

- **$\varphi$, $\psi$, $\theta$**：任意命题。
- **$\wedge$**：合取 (Conjunction)。
- **$\supset$**：蕴涵 (Implication)。

**解释：**

- 该命题表示：如果 $\varphi$ 和 $(\psi \wedge \theta)$ 为真，那么可以得出 $(\theta \wedge \varphi)$ 为真。

在经典逻辑中，这个命题的证明形式为：

$$
\lambda (w:\varphi \wedge (\psi \wedge \theta)) \ \text{ccr}(u \div \theta \wedge \varphi.k \# w),
$$

其中 $k$ 是以下反驳：

$$
\text{fst}; \ \text{ccp}(x : \varphi.\ \text{snd}; \ \text{ccp}(y : \psi \wedge \theta.\ \text{snd}; \ \text{ccp}(z : \theta.u \# \langle z, x \rangle) \# y) \# w).
$$

**符号解析：**

- **$\lambda (w:\varphi \wedge (\psi \wedge \theta))$**：对 $w$ 进行抽象，$w$ 是 $\varphi \wedge (\psi \wedge \theta)$ 的证明。
- **$\text{ccr}(u \div \theta \wedge \varphi.k \# w)$**：从反驳 $k$ 和 $w$ 的矛盾中构造 $\theta \wedge \varphi$ 的证明。
- **$k \# w$**：$k$ 和 $w$ 构成矛盾。
- **$k$**：反驳项，针对 $\theta \wedge \varphi$。

**反驳项 $k$ 的详细结构：**

$$
\text{fst}; \ \text{ccp}(x : \varphi.\ \text{snd}; \ \text{ccp}(y : \psi \wedge \theta.\ \text{snd}; \ \text{ccp}(z : \theta.u \# \langle z, x \rangle) \# y) \# w).
$$

**解析：**

- **$\text{fst};$**：对合取项 $w$ 进行左投影，提取出 $\varphi$ 的证明。
- **$\text{ccp}(x : \varphi.\ ...)$**：在假设 $x$ 是 $\varphi$ 的证明下，继续构造反驳。
- **$\text{snd};$**：对合取项 $w$ 进行右投影，提取出 $\psi \wedge \theta$ 的证明。
- **$\text{ccp}(y : \psi \wedge \theta.\ ...)$**：在假设 $y$ 是 $\psi \wedge \theta$ 的证明下，继续构造反驳。
- **$\text{snd};$**：对 $y$ 进行右投影，提取出 $\theta$ 的证明。
- **$\text{ccp}(z : \theta.u \# \langle z, x \rangle)$**：在假设 $z$ 是 $\theta$ 的证明下，构造与 $u$ 的矛盾，其中 $u$ 是对 $\theta \wedge \varphi$ 的反驳假设。
- **$\langle z, x \rangle$**：构造 $\theta \wedge \varphi$ 的证明，$z$ 是 $\theta$ 的证明，$x$ 是 $\varphi$ 的证明。

---

#### 构造逻辑中的直接证明

然而，在**构造逻辑** (Constructive Logic) 中，这个命题有一个直接的证明，避免了通过矛盾的绕圈子：

$$
\lambda (w:\varphi \wedge (\psi \wedge \theta))\ \langle w \cdot r \cdot r, \ w \cdot l \rangle.
$$

**符号解析：**

- **$w \cdot r$**：对 $w$ 进行右投影，提取 $\psi \wedge \theta$ 的证明。
- **$w \cdot r \cdot r$**：对 $\psi \wedge \theta$ 的证明再进行右投影，提取 $\theta$ 的证明。
- **$w \cdot l$**：对 $w$ 进行左投影，提取 $\varphi$ 的证明。
- **$\langle w \cdot r \cdot r, \ w \cdot l \rangle$**：构造 $\theta \wedge \varphi$ 的证明。

**解释：**

- 直接通过对 $w$ 的投影操作，提取出所需的证明，然后构造目标命题的证明。

---

#### 经典逻辑缺乏消去形式

但是，这个证明无法在经典逻辑中直接表达，因为经典逻辑**缺乏构造逻辑中的消去形式** (Elimination Forms)。

---

#### 导出消去规则

然而，我们可以将间接证明的使用**封装**起来，通过**导出构造逻辑的消去规则**，使其形式更易于接受。

例如，以下规则：

$$
\frac{\Delta \ \Gamma \vdash \varphi \wedge \psi\ \text{true}}{\Delta \ \Gamma \vdash \varphi\ \text{true}}
$$

**在经典逻辑中是可导出的：**

$$
\begin{align}
1.\ &\Delta, \varphi\ \text{false}\ \Gamma \vdash \varphi\ \text{false} \tag{假设 $\varphi$ 为假} \\
2.\ &\Delta, \varphi\ \text{false}\ \Gamma \vdash \varphi \wedge \psi\ \text{false} \tag{根据 (31.1h) 合取为假} \\
3.\ &\Delta\ \Gamma \vdash \varphi \wedge \psi\ \text{true} \tag{假设 $\varphi \wedge \psi$ 为真} \\
4.\ &\Delta, \varphi\ \text{false}\ \Gamma \vdash \varphi \wedge \psi\ \text{true} \tag{从 (3) 弱化} \\
5.\ &\Delta, \varphi\ \text{false}\ \Gamma \vdash \# \tag{由 (2) 和 (4) 导出矛盾 (31.1c)} \\
6.\ &\Delta\ \Gamma \vdash \varphi\ \text{true} \tag{由 (5) 和 (31.1d)}
\end{align}
$$

**步骤解析：**

- **步骤1**：假设 $\varphi$ 为假。
- **步骤2**：根据合取为假的规则 (31.1h)，如果 $\varphi$ 为假，则 $\varphi \wedge \psi$ 为假。
- **步骤3**：假设 $\varphi \wedge \psi$ 为真。
- **步骤4**：在假设 $\varphi$ 为假的情况下，$\varphi \wedge \psi$ 仍为真（从步骤3弱化而来）。
- **步骤5**：从步骤2和步骤4，$\varphi \wedge \psi$ 被判断为既真又假，产生矛盾 (31.1c)。
- **步骤6**：根据 (31.1d)，从假设 $\varphi$ 为假导出矛盾，可以得出 $\varphi$ 为真。

---

#### 其他消去形式

其他的消去形式也可以以类似的方式导出，在每种情况下，都依赖于间接证明，从假设命题为假的情况下导出矛盾，来构造命题为真的证明。

---

#### 使用证明和反驳表达式

导出构造逻辑的消去形式最容易的方法是使用**证明** (Proof) 和**反驳表达式** (Refutation Expressions)，如下所示：

- **$\text{abort}(p) = \text{ccr}(u \div \varphi.\ \text{abort}\ \#\ p)$**

- **$p \cdot l = \text{ccr}(u \div \varphi.\ \text{fst};\ u\ \#\ p)$**

- **$p \cdot r = \text{ccr}(u \div \psi.\ \text{snd};\ u\ \#\ p)$**

- **$p_1(p_2) = \text{ccr}(u \div \psi.\ \text{ap}(p_2);\ u\ \#\ p_1)$**

- **$\text{case } p_1\ \{l \cdot x \Rightarrow p_2\ |\ r \cdot y \Rightarrow p\} = \text{ccr}(u \div \gamma.\ \text{case}(\text{ccp}(x : \varphi.\ u\ \#\ p_2);\ \text{ccp}(y : \psi.\ u\ \#\ p))\ \#\ p_1)$**

**符号解析：**

- **$\text{ccr}$**：从反驳和矛盾构造证明。
- **$\text{ccp}$**：从证明和矛盾构造反驳。
- **$\text{fst}$**、**$\text{snd}$**：合取的左、右投影。
- **$\text{ap}(p_2)$**：应用 $p_2$，对应于函数应用。
- **$\text{case}$**：对析取进行模式匹配。

---

#### 验证消去规则

通过上述定义，可以直接验证预期的消去规则成立。例如，规则：

$$
\frac{\Delta \ \Gamma \vdash p_1 : \varphi \supset \psi \quad \Delta \ \Gamma \vdash p_2 : \varphi}{\Delta \ \Gamma \vdash p_1(p_2) : \psi} \tag{31.3}
$$

**是可导出的**，使用上述对 $p_1(p_2)$ 的定义。

通过隐藏证明项，我们可以导出相应的可证明性规则：

$$
\frac{\Delta \ \Gamma \vdash \varphi \supset \psi\ \text{true} \quad \Delta \ \Gamma \vdash \varphi\ \text{true}}{\Delta \ \Gamma \vdash \psi\ \text{true}} \tag{31.4}
$$

---

### 总结

在这一节中，我们展示了如何在经典逻辑中**导出构造逻辑的消去形式**，使得经典逻辑可以更方便地表达和使用这些形式。虽然经典逻辑缺乏构造逻辑中直接的消去规则，但通过**间接证明**和使用证明、反驳表达式，我们可以重新获得这些规则的功能。

- **间接证明**是经典逻辑中证明命题为真的主要手段，即从假设命题为假的情况下导出矛盾。

- **导出消去规则**使我们能够在经典逻辑中类似于构造逻辑地操作，简化了证明的复杂性。

- **使用证明和反驳表达式**，我们可以形式化地定义消去操作，验证它们的正确性。

通过这些方法，我们在经典逻辑中实现了对构造逻辑中消去形式的模拟，丰富了经典逻辑的表达能力，并使得证明过程更加直观和简洁。

### ---------------------------------

### 31.3 证明的动力学 (Proof Dynamics)

在**经典逻辑** (Classical Logic) 中，动力学源于对**命题** (proposition) 的**证明** (proof) 和**反驳** (refutation) 之间矛盾的简化。为了明确这一点，我们将定义一个**转换系统** (transition system)，其状态是由同一命题的证明 $p$ 和反驳 $k$ 构成的**矛盾** (contradiction) $k \# p$。

**解释：**

- **$k \# p$**：表示反驳 $k$ 和证明 $p$ 针对同一命题，且二者构成矛盾。

计算的步骤包括基于 $p$ 和 $k$ 的形式，对矛盾状态的简化。

---

#### 真理和虚假的规则交互 (Interaction of Truth and Falsity Rules)

**逻辑连接词** (logical connectives) 的真理和虚假规则以一种令人愉快的方式相互作用：

1. **合取左投影 (First Conjunction Projection)：**

   $$
   \frac{}{\text{fst} ; k \ \#\ \langle p_1, p_2 \rangle \quad \rightarrow \quad k \ \#\ p_1} \tag{31.5a}
   $$

2. **合取右投影 (Second Conjunction Projection)：**

   $$
   \frac{}{\text{snd} ; k \ \#\ \langle p_1, p_2 \rangle \quad \rightarrow \quad k \ \#\ p_2} \tag{31.5b}
   $$

3. **析取左情况 (Case Left for Disjunction)：**

   $$
   \frac{}{\text{case}(k_1; k_2) \ \#\ l \cdot p_1 \quad \rightarrow \quad k_1 \ \#\ p_1} \tag{31.5c}
   $$

4. **析取右情况 (Case Right for Disjunction)：**

   $$
   \frac{}{\text{case}(k_1; k_2) \ \#\ r \cdot p_2 \quad \rightarrow \quad k_2 \ \#\ p_2} \tag{31.5d}
   $$

5. **否定的相互抵消 (Negation Cancellation)：**

   $$
   \frac{}{\text{not}(p) \ \#\ \text{not}(k) \quad \rightarrow \quad k \ \#\ p} \tag{31.5e}
   $$

6. **蕴涵应用 (Implication Application)：**

   $$
   \frac{}{\text{ap}(p_1) ; k \ \#\ \lambda (x:\varphi) p_2 \quad \rightarrow \quad k \ \#\ [p_1/x]p_2} \tag{31.5f}
   $$

**公式解析：**

- **$\text{fst}$**：表示合取的左投影操作。
- **$\text{snd}$**：表示合取的右投影操作。
- **$;$**：表示操作的组合或顺序执行。
- **$\langle p_1, p_2 \rangle$**：表示合取命题的证明，$p_1$ 和 $p_2$ 分别是 $\varphi_1$ 和 $\varphi_2$ 的证明。
- **$\text{case}(k_1; k_2)$**：表示对析取反驳的情况分析，$k_1$ 和 $k_2$ 分别是针对 $\varphi_1$ 和 $\varphi_2$ 的反驳。
- **$l \cdot p_1$**：表示析取左项的证明。
- **$r \cdot p_2$**：表示析取右项的证明。
- **$\text{not}(p)$**：表示对证明 $p$ 的否定。
- **$\text{not}(k)$**：表示对反驳 $k$ 的否定。
- **$\text{ap}(p_1)$**：表示将 $p_1$ 应用于后续的操作，通常与蕴涵相关。
- **$\lambda (x:\varphi) p_2$**：表示蕴涵的证明，$p_2$ 是在假设 $x:\varphi$ 下的证明。
- **$[p_1/x]p_2$**：表示在 $p_2$ 中用 $p_1$ 替换 $x$。

---

#### 间接证明规则导致的转换 (Transitions from Rules of Indirect Proof)

**间接证明** (Indirect Proof) 的规则产生了以下转换：

7. **从反驳到证明的调用 (Call with Current Proof, ccp)：**

   $$
   \frac{}{\text{ccp}(x : \varphi.k_1 \ \#\ p_1) \ \#\ p_2 \quad \rightarrow \quad [p_2/x]k_1 \ \#\ [p_2/x]p_1} \tag{31.5g}
   $$

8. **从证明到反驳的调用 (Call with Current Refutation, ccr)：**

   $$
   \frac{}{\ k_1 \ \#\ \text{ccr}(u \div \varphi.k_2 \ \#\ p_2) \quad \rightarrow \quad [k_1/u]k_2 \ \#\ [k_1/u]p_2} \tag{31.5h}
   $$

**符号解析：**

- **$\text{ccp}(x : \varphi.k_1 \ \#\ p_1)$**：表示在假设 $x : \varphi$ 下，通过矛盾 $k_1 \# p_1$ 构造的反驳，称为“带当前证明的调用” (Call with Current Proof)。
- **$\text{ccr}(u \div \varphi.k_2 \ \#\ p_2)$**：表示在假设 $u \div \varphi$ 下，通过矛盾 $k_2 \# p_2$ 构造的证明，称为“带当前反驳的调用” (Call with Current Refutation)。
- **$[p_2/x]k_1$**：在 $k_1$ 中用 $p_2$ 替换 $x$。
- **$[p_2/x]p_1$**：在 $p_1$ 中用 $p_2$ 替换 $x$。
- **$[k_1/u]k_2$**：在 $k_2$ 中用 $k_1$ 替换 $u$。
- **$[k_1/u]p_2$**：在 $p_2$ 中用 $k_1$ 替换 $u$。

---

#### 规则的解释

- **(31.5g)**：这个规则定义了针对 $\varphi$ 的反驳的行为，该反驳通过反驳 $\varphi$ 为真的假设来进行。当给它提供一个 $\varphi$ 的证明时，这个反驳被激活，并在新的状态中用该证明替换假设。因此，$\text{ccp}$ 表示“带当前证明的调用” (Call with Current Proof)。

- **(31.5h)**：这个转换定义了针对 $\varphi$ 的证明的行为，该证明通过反驳 $\varphi$ 为假的假设来进行。当给它提供一个 $\varphi$ 的反驳时，这个证明被激活，并在新的状态中用该反驳替换假设。因此，$\text{ccr}$ 表示“带当前反驳的调用” (Call with Current Refutation)。

---

#### 动力学的非确定性 (Non-determinism of Dynamics)

规则 **(31.5g)** 和 **(31.5h)** 存在重叠，因为对于以下形式的状态：

$$
\text{ccp}(x : \varphi.k_1 \ \#\ p_1) \ \#\ \text{ccr}(u \div \varphi.k_2 \ \#\ p_2),
$$

有两种可能的转换：

1. 转换到状态 **$[p/x]k_1 \ \#\ [p/x]p_1$**，其中 $p = \text{ccr}(u \div \varphi.k_2 \ \#\ p_2)$。

2. 转换到状态 **$[k/u]k_2 \ \#\ [k/u]p_2$**，其中 $k = \text{ccp}(x : \varphi.k_1 \ \#\ p_1)$。

**解释：**

- 因此，经典逻辑的动力学是**非确定性的** (Non-deterministic)。为避免这种情况，可以在两种情况下施加**优先级顺序** (Priority Ordering)，当有选择时，偏好一种转换。

- **偏好第一种转换**对应于证明的“**惰性**”动力学 (Lazy Dynamics)，因为我们将**未计算的证明** $p$ 传递给左侧的反驳，从而激活反驳。

- **偏好第二种转换**对应于证明的“**急切**”动力学 (Eager Dynamics)，因为我们将**未计算的反驳** $k$ 传递给证明，从而激活证明。

---

#### 定理 31.1 （保持性）(Theorem 31.1 - Preservation)

**定理 31.1**（保持性）：

如果 $k \div \varphi$，$p : \varphi$，并且 $k \ \#\ p \ \rightarrow\ k' \ \#\ p'$，那么存在命题 $\varphi'$，使得 $k' \div \varphi'$ 并且 $p' : \varphi'$。

**证明：**

通过对经典逻辑的动力学规则进行**规则归纳** (Rule Induction)。

---

#### 定理 31.2 （前进性）(Theorem 31.2 - Progress)

**定理 31.2**（前进性）：

如果 $k \div \varphi$ 并且 $p : \varphi$，那么要么 $k \ \#\ p$ 是**终止状态** (final)，要么存在 $k' \ \#\ p'$，使得 $k \ \#\ p \ \rightarrow\ k' \ \#\ p'$。

**证明：**

通过对经典逻辑的静态规则进行**规则归纳** (Rule Induction)。

---

#### 计算的初始和终止状态 (Initial and Final States of Computation)

为了启动计算，我们假设 **$\text{halt}$** 是任何命题的反驳。

- **初始状态：**

  $$
  \frac{}{\text{halt} \ \#\ p \quad \text{initial}} \tag{31.6a}
  $$

- **终止状态：**

  $$
  \frac{}{p \quad \text{canonical} \quad \text{halt} \ \#\ p \quad \text{final}} \tag{31.6b}
  $$

**符号解析：**

- **$\text{halt}$**：表示一个特殊的反驳项，作为任何命题的反驳，用于启动计算。
- **$p \quad \text{canonical}$**：表示 $p$ 是一个**规范的证明** (Canonical Proof)，其定义为任何非间接证明的证明。

---

#### 规范证明 (Canonical Proof)

**判断 $p$ 是规范的**表示 $p$ 是一个规范证明，其定义为任何**非间接证明** (Non-indirect Proof)。

**解释：**

- **规范证明**：指不包含间接证明的证明，即不依赖于间接推理或反驳来构建的证明。

- **间接证明**：通过反驳命题的否定或假设其反面为真来进行的证明。

---

### 总结

在这一节中，我们探讨了**经典逻辑**中的**证明动力学**，特别关注了**证明**和**反驳**之间的**矛盾**如何通过转换规则进行简化。

- **转换系统**的状态由 $k \# p$ 表示，$k$ 是命题的反驳，$p$ 是同一命题的证明。

- **转换规则** (31.5a)-(31.5h) 定义了如何根据 $p$ 和 $k$ 的形式，对矛盾状态进行简化。

- **间接证明的转换规则** (31.5g) 和 (31.5h) 特别重要，因为它们引入了经典逻辑动力学的**非确定性**，需要通过**优先级**来解决。

- **定理 31.1（保持性）** 和 **定理 31.2（前进性）** 确保了转换系统的正确性，即转换过程保持了证明和反驳的类型一致性，并且转换过程要么终止，要么可以继续前进。

- **初始和终止状态**通过引入 $\text{halt}$ 来定义，表示计算的开始和结束。

通过对这些概念和规则的理解，我们可以更深入地认识**经典逻辑**中的**计算内容** (Computational Content)，并理解证明过程中的动态行为。这对于理解**编程语言设计原理** (Programming Language Design Principles) 以及逻辑与计算之间的关系具有重要意义。

### ---------------------------------

### 31.4 排中律 (Law of the Excluded Middle)

**排中律** (Law of the Excluded Middle) 在**经典逻辑** (Classical Logic) 中是可导出的：

$$
\begin{align}
&\quad \varphi \lor \neg\varphi\ \text{false},\ \varphi\ \text{true} \vdash \varphi\ \text{true} \tag{1}
\end{align}
$$

**解释：**

- **$\varphi$**：任意命题 (proposition)。
- **$\varphi \lor \neg\varphi\ \text{false}$**：假设 $\varphi \lor \neg\varphi$ 为假 (false)。
- **$\varphi\ \text{true}$**：假设 $\varphi$ 为真 (true)。
- **$\vdash \varphi\ \text{true}$**：在上述假设下，推出 $\varphi$ 为真。

---

接下来，我们逐步推导出 $\varphi \lor \neg\varphi$ 的真值。

#### 步骤 1

$$
\begin{align}
&\quad \varphi \lor \neg\varphi\ \text{false},\ \varphi\ \text{true} \vdash \varphi\ \text{true} \tag{1}
\end{align}
$$

**解释：**

- 我们从假设开始，$\varphi \lor \neg\varphi$ 为假，$\varphi$ 为真。
- 在此假设下，$\varphi$ 显然为真。

#### 步骤 2

$$
\begin{align}
&\quad \varphi \lor \neg\varphi\ \text{false},\ \varphi\ \text{true} \vdash \varphi \lor \neg\varphi\ \text{true} \tag{2}
\end{align}
$$

**解释：**

- 根据 (31.1k) 和 (31.1l) 的规则，若 $\varphi$ 为真，则 $\varphi \lor \neg\varphi$ 为真。
- 这里，我们应用了**析取为真** (Disjunction True) 的规则。

#### 步骤 3

$$
\begin{align}
&\quad \varphi \lor \neg\varphi\ \text{false},\ \varphi\ \text{true} \vdash \varphi \lor \neg\varphi\ \text{false} \tag{3}
\end{align}
$$

**解释：**

- 这是我们最初的假设，即 $\varphi \lor \neg\varphi$ 为假。

#### 步骤 4

$$
\begin{align}
&\quad \varphi \lor \neg\varphi\ \text{false},\ \varphi\ \text{true} \vdash \# \tag{4}
\end{align}
$$

**解释：**

- 从步骤 (2) 和步骤 (3) 可知，$\varphi \lor \neg\varphi$ 被推导为既真又假，导致矛盾 (contradiction)。
- 根据 (31.1c) 的规则，如果同一命题被推导为真和假，则得到矛盾。

#### 步骤 5

$$
\begin{align}
&\quad \varphi \lor \neg\varphi\ \text{false} \vdash \varphi\ \text{false} \tag{5}
\end{align}
$$

**解释：**

- 根据 (31.1e) 的规则，如果假设 $\varphi$ 为真导致矛盾，则可推导出 $\varphi$ 为假。
- 这里，我们从矛盾 (步骤 4) 推出了 $\varphi$ 为假。

#### 步骤 6

$$
\begin{align}
&\quad \varphi \lor \neg\varphi\ \text{false} \vdash \neg\varphi\ \text{true} \tag{6}
\end{align}
$$

**解释：**

- 根据 (31.1n) 的规则，如果 $\varphi$ 为假，则 $\neg\varphi$ 为真。

#### 步骤 7

$$
\begin{align}
&\quad \varphi \lor \neg\varphi\ \text{false} \vdash \varphi \lor \neg\varphi\ \text{true} \tag{7}
\end{align}
$$

**解释：**

- 由于 $\neg\varphi$ 为真，因此根据析取为真的规则 (31.1k) 和 (31.1l)，$\varphi \lor \neg\varphi$ 为真。

#### 步骤 8

$$
\begin{align}
&\quad \varphi \lor \neg\varphi\ \text{false} \vdash \varphi \lor \neg\varphi\ \text{false} \tag{8}
\end{align}
$$

**解释：**

- 这是我们的假设，$\varphi \lor \neg\varphi$ 为假。

#### 步骤 9

$$
\begin{align}
&\quad \varphi \lor \neg\varphi\ \text{false} \vdash \# \tag{9}
\end{align}
$$

**解释：**

- 再次，从步骤 (7) 和 (8) 得到矛盾。

#### 最终结论

$$
\begin{align}
&\quad \varphi \lor \neg\varphi\ \text{true} \tag{10}
\end{align}
$$

**解释：**

- 根据 (31.1d) 的规则，如果假设 $\varphi \lor \neg\varphi$ 为假导致矛盾，则可推出 $\varphi \lor \neg\varphi$ 为真。
- 因此，**排中律**在经典逻辑中得证。

---

#### 使用显式证明和反驳表示

当使用**显式的证明** (explicit proofs) 和**反驳** (refutations) 来书写上述推导时，我们得到证明项 $p_0 : \varphi \lor \neg\varphi$：

$$
\begin{align}
p_0 = \text{ccr}\left(u \div \varphi \lor \neg\varphi.\ u\ \#\ r \cdot \text{not}\left(\text{ccp}\left(x : \varphi.\ u\ \#\ l \cdot x\right)\right)\right) \tag{11}
\end{align}
$$

**公式解析：**

- **$p_0$**：证明项，证明 $\varphi \lor \neg\varphi$ 为真。
- **$\text{ccr}(u \div \varphi \lor \neg\varphi.\ ...)$**：构造一个证明，假设 $u$ 是 $\varphi \lor \neg\varphi$ 的反驳，通过与其产生矛盾来证明。
- **$u\ \#\ r \cdot \text{not}(\text{ccp}(x : \varphi.\ u\ \#\ l \cdot x))$**：表示 $u$ 与 $r \cdot \text{not}(\text{ccp}(x : \varphi.\ u\ \#\ l \cdot x))$ 构成矛盾。

**符号解析：**

- **$\text{ccr}$** (call with current refutation)：带当前反驳的调用，用于构造证明。
- **$u \div \varphi \lor \neg\varphi$**：假设 $u$ 是对 $\varphi \lor \neg\varphi$ 的反驳。
- **$u\ \#\ ...$**：$u$ 与后续项构成矛盾。
- **$r \cdot ...$**：构造 $\varphi \lor \neg\varphi$ 的右侧证明，即 $\neg\varphi$ 的证明。
- **$\text{not}(\text{ccp}(x : \varphi.\ u\ \#\ l \cdot x))$**：构造 $\neg\varphi$ 的证明，其中 $\text{ccp}$ 是带当前证明的调用。

---

#### 理解该证明的计算含义

为了理解该证明的**计算含义** (computational meaning)，我们将其与一个反驳 $k \div \varphi \lor \neg\varphi$ 并置，并使用第 31.3 节给出的动力学进行简化。

首先，我们有以下转换：

$$
\begin{align}
&\quad k\ \#\ \text{ccr}\left(u \div \varphi \lor \neg\varphi.\ u\ \#\ r \cdot \text{not}\left(\text{ccp}\left(x : \varphi.\ u\ \#\ l \cdot x\right)\right)\right) \\
&\quad \rightarrow \quad k\ \#\ r \cdot \text{not}\left(\text{ccp}\left(x : \varphi.\ k\ \#\ l \cdot x\right)\right) \tag{12}
\end{align}
$$

**解释：**

- 根据转换规则 (31.5h)，我们将 $k$ 替换到 $\text{ccr}$ 中，得到新的状态。
- 在这个过程中，我们将 $k$ 复制了一份，使其在结果状态中出现了两次。

---

由于 $k$ 的类型为 $k \div \varphi \lor \neg\varphi$，因此 $k$ 必须具有以下形式：

$$
\begin{align}
k = \text{case}(k_1; k_2) \tag{13}
\end{align}
$$

其中：

- **$k_1 \div \varphi$**：$k_1$ 是 $\varphi$ 的反驳。
- **$k_2 \div \neg\varphi$**：$k_2$ 是 $\neg\varphi$ 的反驳。

---

继续化简，我们得到：

**步骤 1**

$$
\begin{align}
&\quad \text{case}(k_1; k_2)\ \#\ r \cdot \text{not}\left(\text{ccp}\left(x : \varphi.\ \text{case}(k_1; k_2)\ \#\ l \cdot x\right)\right) \\
&\quad \rightarrow \quad k_2\ \#\ \text{not}\left(\text{ccp}\left(x : \varphi.\ \text{case}(k_1; k_2)\ \#\ l \cdot x\right)\right) \tag{14}
\end{align}
$$

**解释：**

- 根据转换规则 (31.5d)，针对析取右项的情况。
- 我们将 $k$ 具体化为 $\text{case}(k_1; k_2)$，并对 $r \cdot ...$ 进行匹配。

---

由于 $k_2$ 的类型为 $k_2 \div \neg\varphi$，因此 $k_2$ 必须具有以下形式：

$$
\begin{align}
k_2 = \text{not}(p_2) \tag{15}
\end{align}
$$

其中 $p_2 : \varphi$。

---

继续化简：

**步骤 2**

$$
\begin{align}
&\quad \text{not}(p_2)\ \#\ \text{not}\left(\text{ccp}\left(x : \varphi.\ \text{case}(k_1; k_2)\ \#\ l \cdot x\right)\right) \\
&\quad \rightarrow \quad \text{ccp}\left(x : \varphi.\ \text{case}(k_1; k_2)\ \#\ l \cdot x\right)\ \#\ p_2 \tag{16}
\end{align}
$$

**解释：**

- 根据转换规则 (31.5e)，否定的相互抵消。
- 我们得到一个新的状态，其中 $\text{ccp}$ 与 $p_2$ 并置。

---

注意到 $p_2$ 是 $\varphi$ 的一个有效证明。

**步骤 3**

$$
\begin{align}
&\quad \text{ccp}\left(x : \varphi.\ \text{case}(k_1; k_2)\ \#\ l \cdot x\right)\ \#\ p_2 \\
&\quad \rightarrow \quad \text{case}(k_1; k_2)\ \#\ l \cdot p_2 \tag{17}
\end{align}
$$

**解释：**

- 根据转换规则 (31.5g)，将 $p_2$ 替换到 $x$ 中。

**步骤 4**

$$
\begin{align}
&\quad \text{case}(k_1; k_2)\ \#\ l \cdot p_2 \\
&\quad \rightarrow \quad k_1\ \#\ p_2 \tag{18}
\end{align}
$$

**解释：**

- 根据转换规则 (31.5c)，针对析取左项的情况。

---

在这两个步骤中，关键在于：

- **第一次使用中**，反驳 $k = \text{case}(k_1; k_2)$，在推导开始时被复制，现在被重新使用，但参数不同。
- **在第一次使用时**，反驳 $k$ 被提供了一个证明 $r \cdot p_1$，即 $\varphi \lor \neg\varphi$ 的右侧证明，表现为 $\neg\varphi$ 为真，亦即 $\varphi$ 为假。
- **如果上下文提供了 $\varphi$ 的证明 $p_2$ 来反驳 $\neg\varphi$**，则证明将“回溯” (backtrack) 上下文，转而提供 $l \cdot p_2$ 给 $k$，然后 $k$ 将 $p_2$ 传递给 $k_1$。

---

**总结：**

- **排中律**的证明大胆地断言 $\neg\varphi$ 为真，无论 $\varphi$ 的形式如何。
- **如果上下文通过提供 $\varphi$ 的证明 $p_2$ 来揭露其谎言**，证明就会“改变主意”，最终向原始上下文 $k$ 断言 $\varphi$。
- **不可能进一步反转**，因为上下文已经提供了 $\varphi$ 的证明 $p_2$。

---

#### 排中律的意义

**排中律**说明了经典证明应被视为**证明和反驳之间的交互** (interactions between proofs and refutations)，即**证明**与其使用的**上下文** (context) 之间的交互。

- **在编程术语中**，这对应于具有显式**控制栈** (control stack) 或**续延** (continuation) 的抽象机，表示表达式的求值上下文。
- **该表达式可以访问上下文** (栈、续延)，以便在必要时进行回溯 (backtracking)，以维持真理和虚假的完美对称性。

**代价在于：**

- 一个**封闭的析取证明** (closed proof of a disjunction) 不再需要揭示它证明的是哪个析取项。
- 如我们所见，它可能在进一步检查时“改变主意”。

---

### 总结

通过对**排中律**在经典逻辑中的证明和计算含义的深入分析，我们可以看出：

- **经典逻辑**中的证明过程实际上是**证明**与**反驳**之间的动态交互。
- **证明项**可以根据上下文的反应而改变其行为，例如在被“揭穿”后回溯并提供另一个证明。
- 这反映了经典逻辑中真理和虚假之间的**完美对称性**，但也引入了计算过程中的**非确定性** (non-determinism) 和**回溯** (backtracking) 行为。

这种理解对于深入掌握**编程语言设计原理** (Programming Language Design Principles) 以及**逻辑与计算** (Logic and Computation) 之间的关系具有重要意义。

### ---------------------------------

### 31.5 双重否定翻译 (The Double-Negation Translation)

**构造逻辑** (Constructive Logic) 的更大表达能力的一个后果是，**经典证明** (Classical Proofs) 可以系统地翻译成在构造逻辑中**等价命题** (classically equivalent proposition) 的**构造性证明** (Constructive Proofs)。这意味着，通过系统地重新组织经典证明，我们可以在不改变其经典意义的前提下，将其转换为对**构造性较弱** (constructively weaker) 的命题的构造性证明。

---

#### 关键点：

- **构造性较弱但经典等价**：虽然在构造逻辑中，翻译后的命题可能较弱，但在经典逻辑中，它们仍然是等价的。
- **遵循构造性证明没有损失**：因为每个经典证明都可以被视为一个构造逻辑中对构造性较弱但经典等价命题的证明。

---

这表明，遵循构造性证明并没有损失，因为每个经典证明都可以被视为一个构造逻辑中对构造性较弱但经典等价命题的证明。

此外，这也证明了**经典逻辑** (Classical Logic) 比构造逻辑**更弱** (weaker, less expressive)，这与一种天真的解释相反，后者认为经典逻辑提供了额外的推理原则（如**排中律** (Law of the Excluded Middle)），使其更强。在**编程语言** (Programming Language) 的术语中，添加一个“特性”并不一定会增强（提高表达能力）你的语言；相反，它可能会削弱它。

---

#### 命题的翻译 (Translation of Propositions)

我们将定义一个命题的翻译 $\varphi^\ast$，根据以下对应关系，将经典逻辑解释为构造逻辑：

---

##### 对应关系 (Correspondences):

- **经典逻辑** (Classical Logic) 与 **构造逻辑** (Constructive Logic) 的对应：

  | 经典逻辑                                      | 构造逻辑                                                     | 解释                     |
  | --------------------------------------------- | ------------------------------------------------------------ | ------------------------ |
  | $\Delta\ \Gamma \vdash \varphi\ \text{true}$  | $\neg\Delta^\ast\ \Gamma^\ast \vdash \neg\neg\varphi^\ast\ \text{true}$ | **真理** (truth)         |
  | $\Delta\ \Gamma \vdash \varphi\ \text{false}$ | $\neg\Delta^\ast\ \Gamma^\ast \vdash \neg\varphi^\ast\ \text{true}$ | **虚假** (falsity)       |
  | $\Delta\ \Gamma \vdash\ \# $                  | $\neg\Delta^\ast\ \Gamma^\ast \vdash \bot\ \text{true}$      | **矛盾** (contradiction) |

---

**公式解析：**

- **$\Delta$**：虚假假设的集合 (**Falsity Assumptions**)。
- **$\Gamma$**：真值假设的集合 (**Truth Assumptions**)。
- **$\varphi$**：命题 (**Proposition**)。
- **$\vdash$**：推导符号 (**Turnstile**)，表示可以从假设中推导出结论。
- **$\neg$**：否定符号 (**Negation**)。
- **$\bot$**：假命题 (**Falsehood**)。

---

#### 翻译的解释：

- **经典真理被弱化为构造逻辑中的不可反驳性** (Classical truth is weakened to constructive irrefutability)。
- **经典虚假被表示为构造逻辑中的可反驳性** (Classical falsehood is represented as constructive refutability)。
- **经典矛盾被表示为构造逻辑中的假命题** (Classical contradiction is represented by constructive falsehood)。

---

- **虚假假设**：在翻译后取否定，以表示它们的虚假性。
  
  - **$\neg\Delta^\ast$**：对 $\Delta^\ast$ 中的每个假设取否定。

- **真值假设**：直接翻译，无需变化。
  
  - **$\Gamma^\ast$**：$\Gamma$ 中的假设直接翻译。

---

由于**双重否定** (Double Negations) 在经典逻辑中是可消去的，这种翻译很容易被视为产生一个**经典等价** (classically equivalent) 的命题。但由于在构造逻辑中，$\neg\neg\varphi$ 比 $\varphi$ **构造性地更弱** (constructively weaker)，因此我们也看到，经典逻辑中的一个证明被翻译为对一个更弱的陈述的构造性证明。

---

#### 命题翻译的定义 (Definition of Translation):

有多种选择可以用于命题的翻译；我们选择了一种使经典逻辑和构造逻辑之间的对应证明顺利进行的方式：

- **真命题** (Truth)：

  $$
  >^\ast = >
  $$

- **合取** (Conjunction)：

  $$
  (\varphi_1 \wedge \varphi_2)^\ast = \varphi_1^\ast \wedge \varphi_2^\ast
  $$

- **假命题** (Falsehood)：

  $$
  \bot^\ast = \bot
  $$

- **析取** (Disjunction)：

  $$
  (\varphi_1 \lor \varphi_2)^\ast = \varphi_1^\ast \lor \varphi_2^\ast
  $$

- **蕴涵** (Implication)：

  $$
  (\varphi_1 \supset \varphi_2)^\ast = \varphi_1^\ast \supset \neg\neg\varphi_2^\ast
  $$

- **否定** (Negation)：

  $$
  (\neg\varphi)^\ast = \neg\varphi^\ast
  $$

---

**公式解析：**

- **$\varphi^\ast$**：命题 $\varphi$ 的翻译。
- **$>$**：真命题 (**Truth**)。
- **$\wedge$**：合取 (**Conjunction**)。
- **$\lor$**：析取 (**Disjunction**)。
- **$\supset$**：蕴涵 (**Implication**)。
- **$\neg$**：否定 (**Negation**)。
- **$\bot$**：假命题 (**Falsehood**)。

---

#### 解释：

- **真命题**和**假命题**在翻译中保持不变。

- **合取**和**析取**的翻译是对各自的分量进行递归翻译，然后保留原来的连接词。

- **蕴涵**的翻译在结论部分添加了双重否定，即 $\neg\neg\varphi_2^\ast$。

  - 这使得蕴涵在构造逻辑中表示为从 $\varphi_1^\ast$ 到 $\varphi_2^\ast$ 的**不可反驳性**的推导。

- **否定**的翻译是对被否定的命题进行翻译，然后保留否定符号。

---

#### 证明对应关系 (Proving Correspondences):

通过对**经典逻辑规则** (Rules of Classical Logic) 进行归纳，可以直接证明上述对应关系成立。

---

例如，我们需要证明以下蕴涵在构造逻辑中是可导出的：

$$
\neg\neg\varphi\ \text{true},\ \neg\neg\psi\ \text{true} \vdash \neg\neg(\varphi \wedge \psi)\ \text{true}
$$

**公式解析：**

- **$\neg\neg\varphi\ \text{true}$**：$\varphi$ 的双重否定为真，表示 $\varphi$ 是不可反驳的。
- **$\neg\neg\psi\ \text{true}$**：$\psi$ 的双重否定为真。
- **$\vdash$**：推导符号。
- **$\neg\neg(\varphi \wedge \psi)\ \text{true}$**：合取 $\varphi \wedge \psi$ 的双重否定为真。

---

#### 证明思路：

- **在构造逻辑中**，要证明 $\neg\neg(\varphi \wedge \psi)$，需要假设其否定 $\neg(\varphi \wedge \psi)$ 为假，然后导出矛盾。

- **步骤：**

  1. 假设 $\neg(\varphi \wedge \psi)$ 为真。
  
  2. 由于 $\neg\neg\varphi$ 和 $\neg\neg\psi$ 为真，即 $\varphi$ 和 $\psi$ 都是不可反驳的。
  
  3. 通过构造 $\varphi$ 和 $\psi$ 的证明，得到 $\varphi \wedge \psi$ 的证明。
  
  4. 这与假设 $\neg(\varphi \wedge \psi)$ 为真相矛盾，因而导出矛盾。
  
  5. 因此，$\neg\neg(\varphi \wedge \psi)$ 为真。

---

### 总结

通过这种**双重否定翻译** (Double-Negation Translation)，我们将**经典逻辑**的命题和证明系统地转换为**构造逻辑**中的命题和证明。虽然翻译后的命题在构造逻辑中可能更弱，但在经典逻辑中仍然等价。这进一步证明了构造逻辑的表达能力更强，经典逻辑可以被视为构造逻辑的一个特例。

这种翻译方法在逻辑学和编程语言理论中具有重要意义，帮助我们理解不同逻辑系统之间的关系，以及如何在更强的逻辑系统中模拟较弱的系统。


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------