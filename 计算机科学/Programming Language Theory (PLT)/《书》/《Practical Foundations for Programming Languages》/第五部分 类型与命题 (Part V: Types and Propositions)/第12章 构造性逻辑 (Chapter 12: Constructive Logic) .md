[toc]



> 不要在意章节标号

# 第30章 **构造逻辑 (Constructive Logic)**

**构造逻辑**将数学推理的原则进行系统化编码，反映了数学实践的实际情况。在数学中，一个命题被认为是真的，当且仅当它有一个**证明 (Proof)**；一个命题被认为是假的，当且仅当它有一个**反驳 (Refutation)**。因为存在并将一直存在未解决的问题，我们不能普遍地期望一个命题要么为真，要么为假，因为在大多数情况下，我们既没有它的证明，也没有它的反驳。

构造逻辑可以描述为“**以人为本的逻辑 (Logic as if People Matter)**”，与之相对的是经典逻辑，它被描述为“**上帝心智的逻辑 (Logic of the Mind of God)**”。从构造的角度来看，判断“$\varphi$ 为真 (φ\ true)”意味着“存在 $\varphi$ 的一个证明 (There is a proof of φ)”。

**证明 (Proof)** 的构成是一个社会构建，即人们就什么是有效的论证达成一致。逻辑的规则对可用于有效证明的推理原则进行了编码。有效的证明形式由被断言为真的命题的最外层结构决定。例如，一个**合取 (Conjunction)** 的证明由其每个合取项的证明组成；一个**蕴含 (Implication)** 的证明由将前提的证明转化为结论的证明的转换构成。

当完全展开时，证明的形式与编程语言的表达形式完全对应。每个命题都关联了其证明的类型 (Type)；一个证明是其关联类型的表达式。这种在程序和证明之间的关联为证明赋予了**动态性 (Dynamics)**，因为它们只是某种类型的程序。通过这种方式，构造逻辑中的证明具有计算内容，即它们可以被解释为其关联类型的可执行程序。反之，程序具有数学内容，作为其类型关联的命题的证明。

这种逻辑和编程的统一被称为**命题即类型原则 (Propositions as Types Principle)**。它是编程语言理论的核心组织原则。**命题 (Propositions)** 被识别为类型，**证明 (Proofs)** 被识别为程序。编程技术对应于一种证明方法；证明技术对应于一种编程方法。将类型视为程序的行为规范，命题可以被视为问题陈述，其证明是实现规范的解决方案。

---

## **30.1 构造语义 (Constructive Semantics)**

**构造逻辑 (Constructive Logic)** 关注两个判断：

1. **$\varphi\ \text{prop}$**：表示 $\varphi$ 表达了一个**命题 (Proposition)**。
2. **$\varphi\ \text{true}$**：表示 $\varphi$ 是一个**真命题 (True Proposition)**。

### **构造逻辑与非构造逻辑的区别**

构造逻辑与非构造逻辑的区别在于，一个命题不仅被视为一个真值，而且被视为一个问题陈述，如果它有解，其解就是一个**证明 (Proof)**。根据通常的数学实践，当且仅当一个命题有一个证明时，它被称为真。

### **真理的标准**

这一原则有重要且可能令人惊讶的结果，其中最重要的是，我们不能普遍地说一个命题要么为真，要么为假。如果对于一个命题为真意味着有它的一个证明，那么对于一个命题为假意味着什么呢？这意味着我们有一个对它的**反驳 (Refutation)**，表明它不可能被证明。也就是说，如果我们能证明假设它为真（即它有一个证明）与已知的事实矛盾，那么这个命题就是假的。

因此，构造逻辑是一种关于**正面信息 (Positive or Affirmative Information)** 的逻辑——我们必须以证明的形式拥有明确的证据，才能确认一个命题的真或假。

### **不可判定性**

鉴于此，很明显并非每个命题都是要么真要么假。例如，对于一个表达未解决问题的命题，如著名的 **P vs NP 问题 (P ?= NP Problem)**，我们既没有它的证明，也没有它的反驳（仅仅没有证明并不构成反驳）。这样的问题是**未决的 (Undecided)**，正是因为它是未解决的。

因为总会有未解决的问题（存在无限多的命题，但在我们知识的演进过程中，在给定的时刻只有有限多的证明），我们不能说每个命题都是**可判定的 (Decidable)**，即要么为真，要么为假。

### **可判定命题的示例**

当然，一些命题是可判定的，因此可以认为是要么真要么假。例如，如果 $\varphi$ 表达了自然数之间的不等式，那么 $\varphi$ 是可判定的，因为对于给定的自然数 $m$ 和 $n$，我们总是可以确定 $m \leq n$ 或 $m \not\leq n$ ——我们可以证明或反驳给定的不等式。

#### **举例说明**

- **给定自然数 $m$ 和 $n$，判断 $m \leq n$**

  - **情况1**：如果 $m \leq n$，我们可以通过数学计算证明这一点，因此 $\varphi$ 为真。
  - **情况2**：如果 $m > n$，则 $m \leq n$ 不成立，我们可以反驳 $\varphi$，因此 $\varphi$ 为假。

### **对实数的扩展**

然而，这个论点不能扩展到实数。要理解原因，考虑通过其十进制展开来表示实数。在任何有限的时间内，我们只能探索展开的有限初始段，这不足以确定它是否，例如，小于1。

#### **示例**

- **假设我们有一个实数，其十进制展开为 $0.99\ldots9$**

  - 在任何有限的时间内，我们只能看到有限个 $9$，无法决定这个数是否等于1。
  - 因为在无限的极限下，$0.999\ldots = 1$，但在有限的展开中，我们无法确定这一点。

**注意**：这个论证并不是严格的证明，因为我们可能会怀疑是否存在某种其他的实数表示，使得这种决定可以在有限时间内做出，但事实证明并非如此。

### **构造态度**

构造主义的态度是简单地接受这种情况是不可避免的，并与之和平相处。当面临一个问题时，我们别无选择，只能卷起袖子，尝试证明或反驳它。成功并没有保证！生活是艰难的，但我们总能以某种方式勉强度日。

---

## **总结**

- **构造逻辑**强调命题的真理性依赖于是否存在其证明，而非仅仅将命题视为具有某个真值。
- **真命题**：有一个证明的命题。
- **假命题**：有一个反驳，表明它不可能被证明。
- **未决命题**：既没有证明，也没有反驳的命题，不能简单地认为要么真要么假。
- **命题即类型原则 (Propositions as Types Principle)**：将命题与类型对应，证明与程序对应，建立了逻辑与编程之间的统一。
- **可判定性**：并非所有命题都是可判定的，因为总存在未解决的问题。

---

## **练习与思考**

**练习1**：

- **理解命题即类型原则**：

  - **任务**：举例说明如何将一个简单的逻辑命题对应到一个类型，并将其证明对应到一个程序。

  - **提示**：考虑命题“如果 $A$ 且 $B$，那么 $A$”，对应到类型 $(A \times B) \rightarrow A$。

**练习2**：

- **探究不可判定性**：

  - **任务**：解释为什么对于某些实数的比较，无法在有限时间内确定大小关系。

  - **提示**：思考无理数和超越数的表示，以及其十进制展开的无限性。

**思考**：

- **构造逻辑在计算机科学中的应用**：

  - **问题**：构造逻辑如何影响编程语言的设计，尤其是类型系统和程序验证？

- **命题即类型原则的意义**：

  - **问题**：这种原则如何促进了逻辑和编程之间的交叉研究，带来了哪些实际的好处？

---

**参考文献**：

- **构造逻辑的理论与实践**。
- **命题即类型原则的深入研究**。

希望以上详细的解释能够帮助您深入理解 **第30章 构造逻辑 (Constructive Logic)** 的内容。如有任何疑问，欢迎提问！

### ---------------------------------

# **第30章 构造逻辑 (Constructive Logic)**

## **30.2 构造逻辑 (Constructive Logic)**

在本节中，我们将深入探讨**构造逻辑**中的基本判断和推理规则。构造逻辑强调证明的构造性，即一个命题为真意味着我们有其证明。

---

### **基本判断**

构造逻辑中的基本判断通常不会单独被关注，而是以**假设判断 (Hypothetical Judgment)** 的形式出现，形式为：

$$
\varphi_1\ \text{true},\ \dots,\ \varphi_n\ \text{true} \vdash \varphi\ \text{true}.
$$

**解释**：

- **$\varphi_i\ \text{true}$**：表示命题 $\varphi_i$ 为真（有一个证明）。
- **$\vdash$**：表示在假设的前提下推导。
- **$\varphi\ \text{true}$**：在假设 $\varphi_1,\ \dots,\ \varphi_n$ 为真的情况下，命题 $\varphi$ 为真。

当 $n = 0$ 时，这与判断 $\varphi\ \text{true}$ 等价，即没有任何假设。

### **结构属性**

假设判断的结构属性，专门用于构造逻辑，定义了我们在假设下推理的含义：

1. **假设引入**

   $$
   \frac{}{
     \Gamma,\ \varphi\ \text{true} \vdash \varphi\ \text{true}
   }
   \quad (30.1a)
   $$

   **解释**：在假设列表 $\Gamma$ 中添加 $\varphi\ \text{true}$ 后，可以推出 $\varphi\ \text{true}$。

2. **假设使用**

   $$
   \frac{
     \Gamma \vdash \varphi_1\ \text{true} \quad \Gamma,\ \varphi_1\ \text{true} \vdash \varphi_2\ \text{true}
   }{
     \Gamma \vdash \varphi_2\ \text{true}
   }
   \quad (30.1b)
   $$

   **解释**：如果我们能在 $\Gamma$ 下证明 $\varphi_1\ \text{true}$，并且在 $\Gamma$ 加上 $\varphi_1\ \text{true}$ 的假设下能证明 $\varphi_2\ \text{true}$，那么在 $\Gamma$ 下可以直接推出 $\varphi_2\ \text{true}$。

3. **假设弱化**

   $$
   \frac{
     \Gamma \vdash \varphi_2\ \text{true}
   }{
     \Gamma,\ \varphi_1\ \text{true} \vdash \varphi_2\ \text{true}
   }
   \quad (30.1c)
   $$

   **解释**：如果在 $\Gamma$ 下可以证明 $\varphi_2\ \text{true}$，那么在 $\Gamma$ 加上任意新的假设 $\varphi_1\ \text{true}$ 下，仍然可以证明 $\varphi_2\ \text{true}$。

4. **假设收缩**

   $$
   \frac{
     \Gamma,\ \varphi_1\ \text{true},\ \varphi_1\ \text{true} \vdash \varphi_2\ \text{true}
   }{
     \Gamma,\ \varphi_1\ \text{true} \vdash \varphi_2\ \text{true}
   }
   \quad (30.1d)
   $$

   **解释**：如果在假设 $\varphi_1\ \text{true}$ 出现两次的情况下可以推出 $\varphi_2\ \text{true}$，那么在只包含一个 $\varphi_1\ \text{true}$ 的假设下，也可以推出 $\varphi_2\ \text{true}$。

5. **假设交换**

   $$
   \frac{
     \Gamma_1,\ \varphi_2\ \text{true},\ \varphi_1\ \text{true},\ \Gamma_2 \vdash \varphi\ \text{true}
   }{
     \Gamma_1,\ \varphi_1\ \text{true},\ \varphi_2\ \text{true},\ \Gamma_2 \vdash \varphi\ \text{true}
   }
   \quad (30.1e)
   $$

   **解释**：假设的顺序不影响推导结果，可以交换假设的顺序。

**注意**：最后两个规则（收缩和交换）是隐含的，因为我们将 $\Gamma$ 视为假设的集合，因此多个相同的假设和假设的顺序都不影响推理。

---

### **30.2.1 可证明性 (Provability)**

#### **命题逻辑的语法**

命题逻辑的语法由以下文法定义：

- **命题 (Proposition) φ**：

  $$
  \begin{align*}
  \text{Prop}\ \varphi ::= &\ \top \quad \top\ \text{真理 (Truth)} \\
  &\ \bot \quad \bot\ \text{虚假 (Falsity)} \\
  &\ \wedge(\varphi_1;\ \varphi_2) \quad \varphi_1 \wedge \varphi_2\ \text{合取 (Conjunction)} \\
  &\ \vee(\varphi_1;\ \varphi_2) \quad \varphi_1 \vee \varphi_2\ \text{析取 (Disjunction)} \\
  &\ \supset(\varphi_1;\ \varphi_2) \quad \varphi_1 \supset \varphi_2\ \text{蕴含 (Implication)}
  \end{align*}
  $$

#### **逻辑联结词的引入和消去规则**

逻辑的联结词通过规则赋予意义，这些规则决定了：

1. **(a) 直接证明 (Direct Proof)**：如何构造由给定联结词组成的命题的证明。

2. **(b) 间接证明 (Indirect Proof)**：如何利用这样一个证明来证明另一个命题。

这些被称为联结词的**引入规则 (Introduction Rules)** 和**消去规则 (Elimination Rules)**。

**证明的守恒原则 (Principle of Conservation of Proof)** 指出，这些规则彼此互逆——消去规则不能提取比引入规则更多的信息（以证明的形式），引入规则可以从消去规则提取的信息中重建证明。

---

#### **各联结词的规则**

1. **真理 (Truth)**

   - **引入规则**：

     $$
     \frac{}{
       \Gamma \vdash \top\ \text{true}
     }
     \quad (30.2a)
     $$

     **解释**：在任何情况下，我们都可以推出 $\top$ 为真。

   - **消去规则**：没有消去规则。

2. **合取 (Conjunction)**

   - **引入规则**：

     $$
     \frac{
       \Gamma \vdash \varphi_1\ \text{true} \quad \Gamma \vdash \varphi_2\ \text{true}
     }{
       \Gamma \vdash \varphi_1 \wedge \varphi_2\ \text{true}
     }
     \quad (30.3a)
     $$

     **解释**：如果我们能分别证明 $\varphi_1$ 和 $\varphi_2$，那么它们的合取也为真。

   - **消去规则**：

     1. 提取左项：

        $$
        \frac{
          \Gamma \vdash \varphi_1 \wedge \varphi_2\ \text{true}
        }{
          \Gamma \vdash \varphi_1\ \text{true}
        }
        \quad (30.3b)
        $$

     2. 提取右项：

        $$
        \frac{
          \Gamma \vdash \varphi_1 \wedge \varphi_2\ \text{true}
        }{
          \Gamma \vdash \varphi_2\ \text{true}
        }
        \quad (30.3c)
        $$

3. **蕴含 (Implication)**

   - **引入规则**：

     $$
     \frac{
       \Gamma,\ \varphi_1\ \text{true} \vdash \varphi_2\ \text{true}
     }{
       \Gamma \vdash \varphi_1 \supset \varphi_2\ \text{true}
     }
     \quad (30.4a)
     $$

     **解释**：如果在假设 $\varphi_1\ \text{true}$ 下能推出 $\varphi_2\ \text{true}$，那么可以断言 $\varphi_1 \supset \varphi_2$ 为真。

   - **消去规则**：

     $$
     \frac{
       \Gamma \vdash \varphi_1 \supset \varphi_2\ \text{true} \quad \Gamma \vdash \varphi_1\ \text{true}
     }{
       \Gamma \vdash \varphi_2\ \text{true}
     }
     \quad (30.4b)
     $$

     **解释**：如果我们有 $\varphi_1 \supset \varphi_2$ 的证明，并且有 $\varphi_1$ 的证明，那么可以推出 $\varphi_2$。

4. **虚假 (Falsehood)**

   - **引入规则**：没有引入规则。

   - **消去规则**：

     $$
     \frac{
       \Gamma \vdash \bot\ \text{true}
     }{
       \Gamma \vdash \varphi\ \text{true}
     }
     \quad (30.5b)
     $$

     **解释**：从矛盾可以推出任何命题（爆炸原理）。

5. **析取 (Disjunction)**

   - **引入规则**：

     1. 左项成立：

        $$
        \frac{
          \Gamma \vdash \varphi_1\ \text{true}
        }{
          \Gamma \vdash \varphi_1 \vee \varphi_2\ \text{true}
        }
        \quad (30.6a)
        $$

     2. 右项成立：

        $$
        \frac{
          \Gamma \vdash \varphi_2\ \text{true}
        }{
          \Gamma \vdash \varphi_1 \vee \varphi_2\ \text{true}
        }
        \quad (30.6b)
        $$

   - **消去规则**：

     $$
     \frac{
       \Gamma \vdash \varphi_1 \vee \varphi_2\ \text{true} \quad \Gamma,\ \varphi_1\ \text{true} \vdash \varphi\ \text{true} \quad \Gamma,\ \varphi_2\ \text{true} \vdash \varphi\ \text{true}
     }{
       \Gamma \vdash \varphi\ \text{true}
     }
     \quad (30.6c)
     $$

     **解释**：如果我们有 $\varphi_1 \vee \varphi_2$ 的证明，并且在分别假设 $\varphi_1$ 和 $\varphi_2$ 为真的情况下都能推出 $\varphi$，那么可以推出 $\varphi$。

6. **否定 (Negation)**

   - **定义**：否定 $\neg \varphi$ 定义为 $\varphi \supset \bot$。

     **解释**：$\neg \varphi\ \text{true}$ 意味着在假设 $\varphi\ \text{true}$ 的情况下能推出矛盾（$\bot$），即 $\varphi$ 是可反驳的。

   - **说明**：由于构造性的真理被视为证明的存在，否定的含义相当强烈。特别地，当我们既不能确认也不能反驳一个命题 $\varphi$ 时，问题 $\varphi$ 就是**未决的 (Open)**。

---

### **30.2.2 证明项 (Proof Terms)**

**命题即类型原则**的关键是使证明的形式显式化。基本判断 $\varphi\ \text{true}$（表示 $\varphi$ 有一个证明）被替换为判断 $p : \varphi$，表示 $p$ 是 $\varphi$ 的证明。（有时 $p$ 被称为“证明项 (Proof Term)”，但我们将直接称 $p$ 为“证明”。）

对应地，假设判断也被修改，变量代表假设的但未知的证明：

$$
x_1 : \varphi_1,\ \dots,\ x_n : \varphi_n \vdash p : \varphi.
$$

我们再次用 $\Gamma$ 表示这样的假设列表，且要求没有变量重复出现。

#### **证明项的语法**

证明项的语法由以下文法定义：

- **证明 (Proof) p**：

  $$
  \begin{align*}
  \text{Prf}\ p ::= &\ \top\text{I}\ \langle\ \rangle \quad \text{真理引入 (Truth Introduction)} \\
  &\ \wedge\text{I}(p_1;\ p_2) \quad \langle p_1,\ p_2 \rangle \quad \text{合取引入 (Conj. Intro)} \\
  &\ \wedge\text{E}[l](p) \quad p \cdot l \quad \text{合取消去左 (Conj. Elim Left)} \\
  &\ \wedge\text{E}[r](p) \quad p \cdot r \quad \text{合取消去右 (Conj. Elim Right)} \\
  &\ \supset\text{I}[\varphi](x.p) \quad \lambda (x : \varphi). p \quad \text{蕴含引入 (Impl. Intro)} \\
  &\ \bot\text{E}(p) \quad \text{abort}(p) \quad \text{虚假消去 (False Elim)} \\
  &\ \vee\text{I}[l](p) \quad l \cdot p \quad \text{析取引入左 (Disj. Intro Left)} \\
  &\ \vee\text{I}[r](p) \quad r \cdot p \quad \text{析取引入右 (Disj. Intro Right)} \\
  &\ \vee\text{E}(p;\ x_1.p_1;\ x_2.p_2) \quad \text{case } p\ \{\ l \cdot x_1 \Rightarrow p_1\ |\ r \cdot x_2 \Rightarrow p_2\ \} \quad \text{析取消去 (Disj. Elim)}
  \end{align*}
  $$

- **注释**：

  - **$\top\text{I}\ \langle\ \rangle$**：真理的引入，表示 $\top$ 的证明，不需要任何信息。
  
  - **$\langle p_1,\ p_2 \rangle$**：合取的引入，$p_1$ 和 $p_2$ 是 $\varphi_1$ 和 $\varphi_2$ 的证明。
  
  - **$p \cdot l$** 和 **$p \cdot r$**：合取的消去，从合取的证明 $p$ 中提取左或右部分的证明。
  
  - **$\lambda (x : \varphi). p$**：蕴含的引入，表示从 $\varphi$ 的假设 $x$ 推导出结论的证明 $p$。
  
  - **$\text{abort}(p)$**：虚假的消去，从矛盾的证明 $p$ 推出任意命题的证明。
  
  - **$l \cdot p$** 和 **$r \cdot p$**：析取的引入，$p$ 是对应分支的证明。
  
  - **$\text{case } p\ \{\ l \cdot x_1 \Rightarrow p_1\ |\ r \cdot x_2 \Rightarrow p_2\ \}$**：析取的消去，根据 $p$ 的形式，选择对应的分支，$x_1$ 或 $x_2$ 是对应命题的证明。

#### **规则的形式化**

使用证明项，构造命题逻辑的规则可重新表述如下：

1. **真理引入**：

   $$
   \frac{}{
     \Gamma \vdash \langle\ \rangle : \top
   }
   \quad (30.7a)
   $$

2. **合取引入**：

   $$
   \frac{
     \Gamma \vdash p_1 : \varphi_1 \quad \Gamma \vdash p_2 : \varphi_2
   }{
     \Gamma \vdash \langle p_1,\ p_2 \rangle : \varphi_1 \wedge \varphi_2
   }
   \quad (30.7b)
   $$

3. **合取消去左**：

   $$
   \frac{
     \Gamma \vdash p : \varphi_1 \wedge \varphi_2
   }{
     \Gamma \vdash p \cdot l : \varphi_1
   }
   \quad (30.7c)
   $$

4. **合取消去右**：

   $$
   \frac{
     \Gamma \vdash p : \varphi_1 \wedge \varphi_2
   }{
     \Gamma \vdash p \cdot r : \varphi_2
   }
   \quad (30.7d)
   $$

5. **蕴含引入**：

   $$
   \frac{
     \Gamma,\ x : \varphi_1 \vdash p : \varphi_2
   }{
     \Gamma \vdash \lambda (x : \varphi_1). p : \varphi_1 \supset \varphi_2
   }
   \quad (30.7e)
   $$

6. **蕴含消去（应用）**：

   $$
   \frac{
     \Gamma \vdash p : \varphi_1 \supset \varphi_2 \quad \Gamma \vdash p_1 : \varphi_1
   }{
     \Gamma \vdash p(p_1) : \varphi_2
   }
   \quad (30.7f)
   $$

7. **虚假消去**：

   $$
   \frac{
     \Gamma \vdash p : \bot
   }{
     \Gamma \vdash \text{abort}(p) : \varphi
   }
   \quad (30.7g)
   $$

8. **析取引入左**：

   $$
   \frac{
     \Gamma \vdash p : \varphi_1
   }{
     \Gamma \vdash l \cdot p : \varphi_1 \vee \varphi_2
   }
   \quad (30.7h)
   $$

9. **析取引入右**：

   $$
   \frac{
     \Gamma \vdash p : \varphi_2
   }{
     \Gamma \vdash r \cdot p : \varphi_1 \vee \varphi_2
   }
   \quad (30.7i)
   $$

10. **析取消去**：

    $$
    \frac{
      \Gamma \vdash p : \varphi_1 \vee \varphi_2 \quad \Gamma,\ x_1 : \varphi_1 \vdash p_1 : \varphi \quad \Gamma,\ x_2 : \varphi_2 \vdash p_2 : \varphi
    }{
      \Gamma \vdash \text{case } p\ \{\ l \cdot x_1 \Rightarrow p_1\ |\ r \cdot x_2 \Rightarrow p_2\ \} : \varphi
    }
    \quad (30.7j)
    $$

---

## **总结**

- **构造逻辑的核心**：强调证明的构造性，命题的真理性由其证明的存在性决定。

- **假设判断**：在假设命题为真的情况下推导新命题为真。

- **逻辑联结词的引入和消去规则**：定义了如何构造和使用命题的证明。

- **证明项 (Proof Terms)**：使证明的形式显式化，与程序相对应，体现了命题即类型的原则。

---

## **练习与思考**

**练习1**：

- **任务**：使用证明项，构造一个证明，证明 $\varphi \wedge \psi \supset \varphi$。

  - **提示**：需要构造一个从 $\varphi \wedge \psi$ 到 $\varphi$ 的函数，即 $\lambda (x : \varphi \wedge \psi). x \cdot l$。

**练习2**：

- **任务**：解释为什么在构造逻辑中，否定的含义比经典逻辑更强。

  - **提示**：考虑在构造逻辑中，$\neg \varphi$ 意味着存在从 $\varphi$ 到 $\bot$ 的证明，而不是仅仅 $\varphi$ 为假。

**思考**：

- **命题即类型原则的应用**：

  - **问题**：如何利用命题即类型原则，将逻辑推理转化为程序设计？

  - **思考方向**：考虑类型系统、函数式编程语言（如 Haskell、Coq）中的应用。

---

希望以上详细的解释和公式解析能够帮助您深入理解 **30.2 构造逻辑 (Constructive Logic)** 的内容。如有任何疑问，欢迎提问！

### ---------------------------------

### 30.3 Proof Dynamics

在构造逻辑中，证明术语具有动态特性，由 Gentzen 原则（Gentzen's Principle）指导。这个原则的一个方面是**证明的保存原则**（principle of conservation of proof），它说明引入到证明中的信息可以通过消除（elimination）过程被提取出来而不会丢失。例如，可以用以下定义等式来说明联结消除（conjunction elimination）是联结引入（conjunction introduction）的后向逆操作：

#### 联结消除与引入

对于联结消除（conjunction elimination）：
$$
\Gamma \vdash p_1 : \phi_1 \\
\Gamma \vdash p_2 : \phi_2 \\
\Gamma \vdash \langle p_1, p_2 \rangle \cdot l \equiv p_1 : \phi_1
$$
$$
\Gamma \vdash p_1 : \phi_1 \\
\Gamma \vdash p_2 : \phi_2 \\
\Gamma \vdash \langle p_1, p_2 \rangle \cdot r \equiv p_2 : \phi_2
$$

在这些等式中：
- $p_1$ 和 $p_2$ 分别是关于 $\phi_1$ 和 $\phi_2$ 的证明。
- $\langle p_1, p_2 \rangle$ 表示一个包含 $p_1$ 和 $p_2$ 的联结对。
- $\langle p_1, p_2 \rangle \cdot l$ 和 $\langle p_1, p_2 \rangle \cdot r$ 分别提取 $p_1$ 和 $p_2$。

#### 联结的反转原则

每一个证明都可以从通过消除得到的信息中重建。这在联结的情况下可以通过以下等式表述：
$$
\Gamma \vdash p_1 : \phi_1 \\
\Gamma \vdash p_2 : \phi_2 \\
\Gamma \vdash \langle p \cdot l, p \cdot r \rangle \equiv p : \phi_1 \land \phi_2
$$

在这里：
- $\langle p \cdot l, p \cdot r \rangle$ 表示将从证明 $p$ 中提取的信息重新组合为 $\phi_1 \land \phi_2$ 的证明。

#### 影响和导数原则

对于蕴涵（implication），保存和反转原则的规则如下：

保存原则：
$$
\Gamma, x : \phi_1 \vdash p_2 : \phi_2 \\
\Gamma \vdash p_2 : \phi_2 \\
\Gamma \vdash (\lambda (x : \phi_1) \, p_2)(p_1) \equiv [p_1/x]p_2 : \phi_2
$$

反转原则：
$$
\Gamma \vdash p : \phi_1 \supset \phi_2 \\
\Gamma \vdash \lambda (x : \phi_1) \, (p(x)) \equiv p : \phi_1 \supset \phi_2
$$

#### 其他逻辑连结的规则

对析取（disjunction）和虚假（falsehood）的对应规则如下：

析取：
$$
\Gamma \vdash p : \phi_1 \lor \phi_2 \\
\Gamma, x_1 : \phi_1 \vdash p_1 : \psi \\
\Gamma, x_2 : \phi_2 \vdash p_2 : \psi \\
\Gamma \vdash \text{case } \langle p \rangle \{ \text{l} \cdot x_1 \Rightarrow p_1 \mid \text{r} \cdot x_2 \Rightarrow p_2 \} \equiv [p/x_1]p_1 : \psi
$$

虚假：
$$
\Gamma \vdash p : \bot \\
\Gamma \vdash \text{false} \equiv p : \bot
$$

这些规则体现了构造逻辑中的证明动态，即如何从给定的证明中提取和重建信息，并通过引入和消除规则对证明进行操作。

### ---------------------------------

### 30.3 证明的动力学 (Proof Dynamics)

在**构造逻辑**(Constructive Logic)中，证明项被赋予了一种动力学，这基于**根岑原理**(Gentzen's Principle)。根岑原理指出，消去形式（eliminatory forms）应被视为引入形式（introductory forms）的逆操作。

#### 根岑原理的两个方面：

1. **证明的守恒性**(Conservation of Proof)：证明中引入的信息可以通过消去操作无损地提取出来。
2. **证明的可逆性**(Reversibility of Proof)：每个证明都可以从通过消去操作提取的信息中重构出来。

---

#### 1. 证明的守恒性 (Conservation of Proof)

以合取（**合取**(Conjunction)，即逻辑中的“与”操作）为例，我们可以表达如下定义等价式（definitional equivalences）：

- 对于左投影（提取左项）：

  $$
  \begin{align}
  &\quad \frac{\Gamma \vdash p_1 : \varphi_1 \quad \Gamma \vdash p_2 : \varphi_2}{\Gamma \vdash \langle p_1, p_2 \rangle \cdot l \equiv p_1 : \varphi_1} \tag{30.8a}
  \end{align}
  $$

- 对于右投影（提取右项）：

  $$
  \begin{align}
  &\quad \frac{\Gamma \vdash p_1 : \varphi_1 \quad \Gamma \vdash p_2 : \varphi_2}{\Gamma \vdash \langle p_1, p_2 \rangle \cdot r \equiv p_2 : \varphi_2} \tag{30.8b}
  \end{align}
  $$

**公式解析：**

- $\Gamma$：假设环境（context），包含变量及其类型的信息。
- $\vdash$：可判定关系（turnstile），表示在环境下可得出某个判断。
- $p_1$, $p_2$：证明项（proof terms），对应于命题的证明。
- $\varphi_1$, $\varphi_2$：命题（propositions）。
- $\langle p_1, p_2 \rangle$：合取的引入形式，表示同时证明了$\varphi_1$和$\varphi_2$。
- $\cdot l$, $\cdot r$：左、右投影操作，提取合取中的左项或右项。
- $\equiv$：定义等价关系（definitional equivalence），表示两边在定义上是相同的。

**解释：**

- (30.8a) 表示在已证明$\varphi_1$和$\varphi_2$的情况下，合取的左投影$\langle p_1, p_2 \rangle \cdot l$等价于$p_1$，即提取出左项的证明。
- (30.8b) 类似地，右投影$\langle p_1, p_2 \rangle \cdot r$等价于$p_2$，提取出右项的证明。

---

#### 2. 证明的可逆性 (Reversibility of Proof)

同样以合取为例，证明的可逆性可以表示为：

$$
\begin{align}
&\quad \frac{\Gamma \vdash p_1 : \varphi_1 \quad \Gamma \vdash p_2 : \varphi_2}{\Gamma \vdash \langle p \cdot l, p \cdot r \rangle \equiv p : \varphi_1 \wedge \varphi_2} \tag{30.9}
\end{align}
$$

**公式解析：**

- $p$：合取的证明项。
- $p \cdot l$, $p \cdot r$：对$p$进行左、右投影，分别提取出对$\varphi_1$和$\varphi_2$的证明。
- $\langle p \cdot l, p \cdot r \rangle$：将提取出的证明重新组合成合取的证明。

**解释：**

- 该公式表示，通过对证明项$p$进行左、右投影，然后再将其组合，结果等价于原始的证明$p$。这体现了证明的可逆性。

---

#### 其他逻辑连接词的等价式

##### 对于蕴涵（**蕴涵**(Implication)，即逻辑中的“如果...那么...”）：

- **证明的守恒性：**

  $$
  \begin{align}
  &\quad \frac{\Gamma, x : \varphi_1 \vdash p_2 : \varphi_2 \quad \Gamma \vdash p_1 : \varphi_1}{\Gamma \vdash (\lambda (x:\varphi_1) p_2)(p_1) \equiv [p_1/x]p_2 : \varphi_2} \tag{30.10a}
  \end{align}
  $$

- **证明的可逆性：**

  $$
  \begin{align}
  &\quad \frac{\Gamma \vdash p : \varphi_1 \supset \varphi_2}{\Gamma \vdash \lambda (x:\varphi_1) (p(x)) \equiv p : \varphi_1 \supset \varphi_2} \tag{30.10b}
  \end{align}
  $$

**公式解析：**

- $\lambda (x:\varphi_1) p_2$：构造一个从$\varphi_1$到$\varphi_2$的函数，即对$x$进行抽象，得到$\varphi_2$的证明。
- $(\lambda (x:\varphi_1) p_2)(p_1)$：将$p_1$代入函数，得到$\varphi_2$的证明。
- $[p_1/x]p_2$：表示在$p_2$中用$p_1$替换$x$。

**解释：**

- (30.10a) 表示应用蕴涵的函数到$p_1$，等价于直接在$p_2$中替换$x$为$p_1$。
- (30.10b) 表示对$p$进行抽象，得到的结果等价于原始的蕴涵证明项$p$。

---

##### 对于析取（**析取**(Disjunction)，即逻辑中的“或”）和假（**假命题**(Falsehood)，即$\bot$）：

- **析取的守恒性（左）：**

  $$
  \begin{align}
  &\quad \frac{\Gamma \vdash p : \varphi_1 \lor \varphi_2 \quad \Gamma, x_1 : \varphi_1 \vdash p_1 : \psi \quad \Gamma, x_2 : \varphi_2 \vdash p_2 : \psi}{\Gamma \vdash \text{case } l \cdot p \{ l \cdot x_1 \Rightarrow p_1 \; | \; r \cdot x_2 \Rightarrow p_2 \} \equiv [p/x_1]p_1 : \psi} \tag{30.11a}
  \end{align}
  $$

- **析取的守恒性（右）：**

  $$
  \begin{align}
  &\quad \frac{\Gamma \vdash p : \varphi_1 \lor \varphi_2 \quad \Gamma, x_1 : \varphi_1 \vdash p_1 : \psi \quad \Gamma, x_2 : \varphi_2 \vdash p_2 : \psi}{\Gamma \vdash \text{case } r \cdot p \{ l \cdot x_1 \Rightarrow p_1 \; | \; r \cdot x_2 \Rightarrow p_2 \} \equiv [p/x_2]p_2 : \psi} \tag{30.11b}
  \end{align}
  $$

- **析取的可逆性：**

  $$
  \begin{align}
  &\quad \frac{\Gamma \vdash p : \varphi_1 \lor \varphi_2 \quad \Gamma, x : \varphi_1 \lor \varphi_2 \vdash q : \psi}{\Gamma \vdash [p/x]q \equiv \text{case } p \{ l \cdot x_1 \Rightarrow [l \cdot x_1 / x]q \; | \; r \cdot x_2 \Rightarrow [r \cdot x_2 / x]q \} : \psi} \tag{30.11c}
  \end{align}
  $$

- **假命题的可逆性：**

  $$
  \begin{align}
  &\quad \frac{\Gamma \vdash p : \bot \quad \Gamma, x : \bot \vdash q : \psi}{\Gamma \vdash [p/x]q \equiv \text{abort}(p) : \psi} \tag{30.11d}
  \end{align}
  $$

**公式解析：**

- $\text{case }$：模式匹配的结构，对不同的情况进行处理。
- $l \cdot p$, $r \cdot p$：将$p$标记为左或右，表示$\varphi_1$或$\varphi_2$中的一个。
- $[p/x]q$：在$q$中用$p$替换$x$。
- $\text{abort}(p)$：由于$p$是$\bot$的证明，因此可以得出任何命题$\psi$的证明。

**解释：**

- (30.11a) 和 (30.11b) 表示对于析取命题的证明，可以通过模式匹配提取出对应的证明。
- (30.11c) 表示析取的可逆性，可以通过对$p$进行分析重构$q$。
- (30.11d) 表示从假命题的证明中，可以推出任何命题的证明。

---

### 30.4 命题即类型 (Propositions as Types)

回顾在构造逻辑中证明的静态和动力学，我们发现证明项与各种类型的表达式在静态和动力学上有着惊人的相似性。

#### 命题和类型的对应关系：

| 命题 (Proposition)            | 类型 (Type)                 |
| ----------------------------- | --------------------------- |
| $\top$                        | $\text{unit}$               |
| $\bot$                        | $\text{void}$               |
| $\varphi_1 \wedge \varphi_2$  | $\tau_1 \times \tau_2$      |
| $\varphi_1 \supset \varphi_2$ | $\tau_1 \rightarrow \tau_2$ |
| $\varphi_1 \lor \varphi_2$    | $\tau_1 + \tau_2$           |

**解释：**

- **合取 (Conjunction) 和积类型 (Product Type)**：合取的引入规则对应于积类型的构造（即有序对$\langle p_1, p_2 \rangle$），其消去规则对应于投影（提取元素）。
- **蕴涵 (Implication) 和函数类型 (Function Type)**：蕴涵的证明对应于函数的构造，应用规则对应于函数的应用。
- **析取 (Disjunction) 和和类型 (Sum Type)**：析取的证明对应于和类型的构造，其消去规则对应于模式匹配。
- **真命题 (Truth) 和单位类型 (Unit Type)**：$\top$对应于只有一个值的类型，即单位类型。
- **假命题 (Falsehood) 和空类型 (Void Type)**：$\bot$对应于没有值的类型，即空类型。

---

**结论：**

这种**命题与类型的对应关系**（Propositions as Types）是编程语言理论的基石。它揭示了计算与推理之间的深刻联系，为通过将语言结构和推理原则相互关联来分析它们提供了框架。

通过理解这种对应关系，我们可以更深入地理解编程语言的设计原理，以及如何将逻辑推理应用于程序的构造和验证。

### ---------------------------------

# **30.5 注解 (Notes)**

**命题即类型原则 (Propositions as Types Principle)** 的起源可以追溯到 **Brouwer (布劳威尔)** 所发展的**直觉主义逻辑语义 (Semantics of Intuitionistic Logic)**。根据他的观点，一个命题的真理性由一个为其提供可计算证据的**构造 (Construction)** 来证明。

---

### **Brouwer 的直觉主义逻辑**

- **核心观点**：命题的真理性与我们对其的构造性证明直接相关，即存在一个构造为命题提供可计算的证据。
  
- **构造的形式**：证据的形式由命题的形式决定。例如：
  
  - **蕴含 (Implication) $\varphi \supset \psi$** 的证据是一个可计算函数，将假设 $\varphi$ 的证据转化为结论 $\psi$ 的证据。
  
    - **形式化表示**：如果我们有一个从 $\varphi$ 的证据到 $\psi$ 的证据的可计算函数 $f$，那么这个函数就是 $\varphi \supset \psi$ 的证据。
  
    - **解释**：这与函数类型 $\tau_1 \rightarrow \tau_2$ 的对应关系一致，其中 $\tau_1$ 是 $\varphi$ 的类型，$\tau_2$ 是 $\psi$ 的类型。

---

### **Heyting 对语义的明确表述**

**Heyting (海廷)** 对这种语义进行了明确的表述，并进一步发展了**构造逻辑 (Constructive Logic)**。

- **贡献**：他将 Brouwer 的直觉主义逻辑形式化，定义了构造证明的规则和结构。

---

### **其他贡献者的进一步发展**

一系列学者对这一思想进行了进一步的发展，包括：

- **de Bruijn**
  
- **Curry (柯里)**
  
- **Gentzen (根岑)**
  
- **Girard (吉拉德)**
  
- **Howard (霍华德)**
  
- **Kolmogorov (柯尔莫哥洛夫)**
  
- **Martin-Löf (马丁-勒夫)**
  
- **Tait (泰特)**

他们在不同的方面拓展和深化了**命题即类型 (Propositions as Types)** 的思想。

---

### **命题即类型对应 (Propositions-as-Types Correspondence)**

- **别称**：这种对应有时被称为**柯里-霍华德同构 (Curry-Howard Isomorphism)**。

  - **说明**：柯里和霍华德发现了逻辑系统与类型系统之间的对应关系，即逻辑证明与程序之间的对应。

- **注意事项**：然而，这种称谓忽略了上述其他学者的重要贡献。

- **进一步说明**：

  - **非一般性的同构**：这种对应关系并不总是严格的同构 (Isomorphism)。

    - **解释**：在某些情况下，命题与类型之间的对应并非一一对应，而是更广泛的关联。

  - **Brouwer 的格言 (Brouwer’s Dictum)**：

    - **内容**：证明的概念最好由更一般的**构造 (Construction)**（即程序）来解释。

    - **含义**：这强调了证明与程序构造之间的紧密联系，证明是通过构造实现的。

---

### **总结**

- **命题的真理性**：由对其提供可计算证据的构造来证明。

- **证据的形式**：由命题的形式决定，不同的逻辑联结词对应不同的证明形式。

- **命题即类型原则**：建立了逻辑与类型系统、证明与程序之间的对应关系。

- **学者们的贡献**：多个学者共同发展了这一思想，丰富了逻辑和计算机科学的理论基础。

- **重要性**：这一原则是编程语言理论的基石，揭示了计算与推理之间的深层联系，为分析语言构造和推理原则提供了框架。

---

## **思考与延伸**

**思考1**：如何在实际编程语言中应用命题即类型原则？

- **例如**：函数式编程语言如 **Haskell** 和 **Coq** 中，类型系统与逻辑系统紧密结合，程序即证明，类型即命题。

**思考2**：命题即类型原则对程序验证和形式化证明有何影响？

- **回答**：它提供了一种将程序正确性证明内嵌到程序开发过程中的方法，使得程序的编写和验证同时进行，提高了软件的可靠性。

---

**参考文献**：

- **Brouwer, L.E.J.**：直觉主义逻辑的创始人。

- **Heyting, A.**：将直觉主义逻辑形式化。

- **Curry, H.B. 和 Howard, W.A.**：提出了逻辑证明与类型系统的对应关系。

- **其他学者**：de Bruijn、Gentzen、Girard、Kolmogorov、Martin-Löf、Tait 等人对该领域的发展做出了重要贡献。

---

希望以上详细的解释能够帮助您深入理解 **30.5 注解 (Notes)** 的内容。


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


### ---------------------------------


### ---------------------------------


### ---------------------------------