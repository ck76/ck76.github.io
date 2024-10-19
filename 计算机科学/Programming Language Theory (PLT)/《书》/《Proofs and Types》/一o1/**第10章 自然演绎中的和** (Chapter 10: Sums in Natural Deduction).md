[toc]



### **第10章 自然演绎中的和（Sums in Natural Deduction）**

#### **引言**

本章将简要描述自然演绎（Natural Deduction）中那些行为不太漂亮的部分，尽管它们恰好展示了直觉主义（Intuitionism）最典型的特征。对于这个片段，我们的**语法方法**坦率地说是不够的，只有对这些思想进行完全重构才能使我们取得进展。

在语法方面，我们需要重新引入三个连接词：**否定（¬）**、**析取（∨）** 和 **存在量词（∃）**。对于否定，通常添加一个符号 **⊥**（荒谬，absurdity），并将 **¬A** 解释为 **A ⇒ ⊥**。

---

### **10.1 规则**

首先，我们列出这些连接词在自然演绎中的推理规则。

#### **析取（∨）的引入规则**

1. **左侧引入（∨1I）**：
   $$
   \frac{\vdots \quad A}{A \lor B} \ (\lor 1 \text{I})
   $$
   这表示如果我们能够证明 $A$，那么可以得出 $A \lor B$。

2. **右侧引入（∨2I）**：
   $$
   \frac{\vdots \quad B}{A \lor B} \ (\lor 2 \text{I})
   $$
   这表示如果我们能够证明 $B$，那么可以得出 $A \lor B$。

#### **析取（∨）的消解规则**

**析取消解（∨E）**：
$$
\frac{\vdots \quad A \lor B \quad [A] \ \vdots \ C \quad [B] \ \vdots \ C}{C} \ (\lor \text{E})
$$

- **解释**：如果我们有一个证据证明 $A \lor B$，并且从 $A$ 可以推出 $C$，从 $B$ 也可以推出 $C$，那么我们可以得出 $C$。
- 方括号 $[A]$ 和 $[B]$ 表示在推导 $C$ 时暂时假设 $A$ 和 $B$。

#### **矛盾（⊥）的消解规则**

**矛盾消解（⊥E）**：
$$
\frac{\vdots \quad \bot}{C} \ (\bot \text{E})
$$

- **解释**：如果我们得到了矛盾（$\bot$），那么我们可以推出任何命题 $C$。

#### **存在量词（∃）的引入规则**

**存在引入（∃I）**：
$$
\frac{\vdots \quad A[a/ξ]}{\exists ξ. A} \ (\exists \text{I})
$$

- **解释**：如果我们有一个具体的对象 $a$，并且能够证明 $A[a/ξ]$（即 $A$ 中的变量 $ξ$ 被 $a$ 取代后成立），那么我们可以得出存在 $ξ$ 使得 $A$ 成立。

#### **存在量词（∃）的消解规则**

**存在消解（∃E）**：
$$
\frac{\vdots \quad \exists ξ. A \quad [A] \ \vdots \ C}{C} \ (\exists \text{E})
$$

- **解释**：如果我们有一个证据证明 $\exists ξ. A$，并且从 $A$ 可以推出 $C$（在 $A$ 中，$ξ$ 是一个新变量），那么我们可以得出 $C$。
- **注意**：在使用规则 $\exists \text{E}$ 后，变量 $ξ$ 不再可以在假设或结论中自由出现。

---

### **10.1 系统的缺陷**

#### **引入规则的优点**

首先，我们注意到引入规则（两个针对 $\lor$，没有针对 $\bot$，一个针对 $\exists$）是非常优秀的！而且，如果我们将它们上下颠倒（从结论到前提看），你会发现它们与 $\land$ 的消解规则 $\land 1\text{E}$、$\land 2\text{E}$、$\forall \text{E}$ 具有相同的结构。

- 在线性逻辑（Linear Logic）中，每种情况只有一个规则，因为它们实际上是颠倒过来的。

#### **消解规则的问题**

然而，**消解规则**就很糟糕了。它们的**灾难性之处**在于存在一个**寄生公式（parasitic formula）** $C$，它与被消解的公式没有结构上的联系。

- 例如，在 $\lor \text{E}$ 中，$C$ 出现在结论中，但它与被消解的 $A \lor B$ 没有直接关系。
- $C$ 扮演了**上下文（context）**的角色，这种规则的写法是对**序列演算（Sequent Calculus）**的一种让步。

实际上，采用这些规则（需要再次强调，目前没有替代方案）**与自然演绎是证明背后的“真实对象”这一理念相矛盾**。

- 例如，我们无法在不预先识别不同推导的情况下合理地处理完整的片段，例如：

  **推导1**：
  $$
  \frac{\vdots \quad A \lor B \quad [A] \ \vdots \ C \quad [B] \ \vdots \ C}{C} \ (\lor \text{E})
  \quad r \\
  D
  $$

  **推导2**：
  $$
  \frac{\vdots \quad A \lor B \quad [A] \ \vdots \ C \ r \ D \quad [B] \ \vdots \ C \ r \ D}{D} \ (\lor \text{E})
  $$

- 这里，我们看到两个推导看似不同，但实际上应该被视为相同。

幸运的是，这种识别可以以一种**不对称的形式**写成**“交换转换（commuting conversion）”**，满足**Church-Rosser 性质**和**强规范化（strong normalization）**。

#### **语法的不充分性**

尽管损害有限，但需要添加这些补充规则揭示了**语法的不充分性**。

- **真正的推导**不过是模交换规则的推导的等价类。

我们希望在处理 $\lor \text{E}$ 时写成：

$$
\frac{\vdots \quad A \lor B}{\begin{cases} A \\ B \end{cases}}
$$

- 这表示我们从 $A \lor B$ 推出两个结论 $A$ 和 $B$。
- 但我们没有办法将它们重新合并为一个结论，除非像之前那样编写 $\lor \text{E}$，这迫使我们选择重新统一的时刻。
- **交换规则**表达了这种时刻可以从根本上被推迟。

---

### **10.2 标准转换（Standard Conversions）**

**标准转换**是引入和消解类型的**重写（redexes）**。

#### **第一种重写（引入-消解）**

1. **从引入到消解的转换**：
   $$
   \frac{\vdots \quad A \quad \lor 1\text{I} \quad A \lor B \quad [A] \ \vdots \ C \quad [B] \ \vdots \ C}{C} \ (\lor \text{E})
   $$
   **转换为**：
   $$
   \vdots \quad A \quad \vdots \quad C
   $$
   - 解释：当我们从 $A$ 通过 $\lor 1\text{I}$ 得到 $A \lor B$，然后立即使用 $\lor \text{E}$ 消解 $A \lor B$，我们可以直接从 $A$ 推出 $C$。

#### **第二种重写（引入-消解）**

2. **从引入到消解的转换**：
   $$
   \frac{\vdots \quad B \quad \lor 2\text{I} \quad A \lor B \quad [A] \ \vdots \ C \quad [B] \ \vdots \ C}{C} \ (\lor \text{E})
   $$
   **转换为**：
   $$
   \vdots \quad B \quad \vdots \quad C
   $$

#### **第三种重写（引入-消解）**

3. **存在量词的转换**：
   $$
   \frac{\vdots \quad A[a/ξ] \quad \exists \text{I} \quad \exists ξ. A \quad [A] \ \vdots \ C}{C} \ (\exists \text{E})
   $$
   **转换为**：
   $$
   \vdots \quad A[a/ξ] \quad \vdots \quad C
   $$

**注意**：由于没有针对 $\bot$ 的引入规则，因此对于这个符号没有标准转换。

---

### **10.2.1 重写的结构**

让我们花点时间思考一下重写的结构：

- 一方面是一个**引入**，另一方面是一个**消解**，并且消解跟在引入之后。
- 但是，有些消解（如 $\Rightarrow$、$\lor$、$\exists$）有多个前提，我们只考虑**引入结束于消解的主前提（principal premise）**的情况，即携带被消解符号的那个前提。

**例如**：

$$
\frac{[A] \ \vdots \ B \quad \Rightarrow \text{I} \quad A \Rightarrow B \quad \vdots \quad (A \Rightarrow B) \Rightarrow C \quad \Rightarrow \text{E} \quad C}{}
$$

- 这个推导不被视为一个重写。
- 这是幸运的，因为我们很难对其进行转换！

---

### **10.3 引入额外转换的必要性**

为了理解我们如何自然地被引导到引入额外的转换，让我们在 $(\land, \Rightarrow, \forall)$ 片段的情况下检查**子公式性质（Subformula Property）**的证明，以便看到推广它的障碍。

#### **10.3.1 子公式性质**

**定理**：设 $\delta$ 是 $(\land, \Rightarrow, \forall)$ 片段中的一个**规范推导（normal deduction）**。那么：

1. **（i）** $\delta$ 中的每个公式都是 $\delta$ 的结论或假设的**子公式（subformula）**。

2. **（ii）** 如果 $\delta$ 以一个消解结束，它有一个**主分支（principal branch）**，即一系列公式 $A_0, A_1, \dots, A_n$，使得：

   - $A_0$ 是一个（未被释放的）假设；
   - $A_n$ 是结论；
   - 对于 $i = 0, \dots, n-1$，$A_i$ 是一个消解的主前提，其结论是 $A_{i+1}$。

   特别地，$A_n$ 是 $A_0$ 的子公式。

**证明**：

我们需要考虑三种情况：

1. **如果 $\delta$ 由一个假设组成**，则无需做任何事情。

2. **如果 $\delta$ 以一个引入结束**，例如：

   $$
   \frac{A \quad B}{A \land B} \ (\land \text{I})
   $$

   - 那么只需在 $A$ 和 $B$ 之上应用归纳假设即可。

3. **如果 $\delta$ 以一个消解结束**，例如：

   $$
   \frac{A \Rightarrow B \quad A}{B} \ (\Rightarrow \text{E})
   $$

   - 不可能存在一个以引入结束的主前提的证明，因此它以一个消解结束，并有一个主分支，可以扩展为 $\delta$ 的主分支。

---

#### **10.3.2 对完整片段的扩展**

对于完整的演算，我们遇到了一个巨大的困难：

- **消解的结论不再是其主前提的子公式**：三个消解规则中的 $C$ 与被消解的公式没有关系。

因此，我们被迫将**主分支的概念**限制在那些表现良好的消解（$\land 1\text{E}$、$\land 2\text{E}$、$\Rightarrow \text{E}$ 和 $\forall \text{E}$）上，我们可以尝试扩展我们的定理。

- 当然，需要将定理的部分（ii）限制在 $\delta$ 以“好的”消解结束的情况。

**证明过程**与之前相同，在引入的情况下，但消解的情况更为复杂：

- **如果 $\delta$ 以一个好的消解结束**，查看其主前提 $A$：

  - 当 $A$ 是一个坏消解的结论时，我们会感到尴尬。
  - 否则，我们可以得出存在一个主分支。

- **如果 $\delta$ 以一个坏的消解结束**，再次查看其主前提 $A$：

  - 它不是一个引入的结论。
  - 如果 $A$ 是一个假设或一个好消解的结论，它是一个假设的子公式，结果很容易得到。
  - 仍然存在 $A$ 来自一个坏消解的情况。

**总结**：

- 必须消除由两个规则连续形成的配置：一个坏消解，其结论 $C$ 是一个消解（好的或坏的）的主前提。
- 一旦我们做到这一点，我们就可以恢复子公式性质。

**快速计算**显示，配置的数量是 $3 \times 7 = 21$，我们不可能一一考虑它们。

- 无论如何，移除这些配置是必要的，如以下示例所示：

  $$
  \frac{A \lor A \quad [A] \quad [A] \quad \land \text{I} \quad A \land A \quad [A] \quad [A] \quad \land \text{I} \quad A \land A}{A \land A \quad (\lor \text{E})}
  $$

  $$
  A \land A \quad \land 1\text{E} \quad A
  $$

- 这个推导不满足子公式性质。

---

### **10.4 交换转换（Commuting Conversions）**

在接下来的内容中，符号 $C \ \vdots \ r \ D$ 表示对主前提 $C$ 进行的一个消解，其结论是 $D$，省略号表示可能的次要前提和对应的推导。

- 这种符号表示涵盖了七种消解情况。

#### **1. ⊥E 的交换**

$$
\frac{\vdots \quad \bot \quad \bot \text{E} \quad C \quad \vdots \ r \ D}{}
$$

**转换为**：

$$
\frac{\vdots \quad \bot \quad \bot \text{E} \quad D}{}
$$

- **解释**：我们可以将对 $\bot$ 的消解直接用于得到 $D$。

#### **2. ∨E 的交换**

$$
\frac{\vdots \quad A \lor B \quad [A] \ \vdots \ C \quad [B] \ \vdots \ C \quad \lor \text{E} \quad C \quad \vdots \ r \ D}{}
$$

**转换为**：

$$
\frac{\vdots \quad A \lor B \quad [A] \ \vdots \ C \quad \vdots \ r \ D \quad [B] \ \vdots \ C \quad \vdots \ r \ D \quad \lor \text{E} \quad D}{}
$$

- **解释**：我们将对 $C$ 的进一步推导 $r$ 应用于每个分支，然后再使用 $\lor \text{E}$ 得到 $D$。

#### **3. ∃E 的交换**

$$
\frac{\vdots \quad \exists ξ. A \quad [A] \ \vdots \ C \quad \exists \text{E} \quad C \quad \vdots \ r \ D}{}
$$

**转换为**：

$$
\frac{\vdots \quad \exists ξ. A \quad [A] \ \vdots \ C \quad \vdots \ r \ D \quad \exists \text{E} \quad D}{}
$$

- **解释**：我们将对 $C$ 的进一步推导 $r$ 应用于存在消解的内部，然后再使用 $\exists \text{E}$ 得到 $D$。

#### **示例**

**最复杂的情况是**：

$$
\begin{align*}
&\frac{\vdots \quad A \lor B \quad [A] \ \vdots \ C \lor D \quad [B] \ \vdots \ C \lor D \quad \lor \text{E} \quad C \lor D \quad [C] \ \vdots \ E \quad [D] \ \vdots \ E \quad \lor \text{E} \quad E}{}
\end{align*}
$$

**转换为**：

$$
\begin{align*}
&\frac{\vdots \quad A \lor B \quad [A] \ \vdots \ C \lor D \quad [C] \ \vdots \ E \quad [D] \ \vdots \ E \quad \lor \text{E} \quad E \quad [B] \ \vdots \ C \lor D \quad [C] \ \vdots \ E \quad [D] \ \vdots \ E \quad \lor \text{E} \quad E \quad \lor \text{E} \quad E}{}
\end{align*}
$$

- **解释**：我们将内部的 $\lor \text{E}$ 转换，使得对 $C \lor D$ 的消解分别在每个分支 $[A]$ 和 $[B]$ 中展开，然后再将结果组合。

---

### **10.5 转换的性质**

#### **唯一的规范形式**

首先，如果**规范形式（normal form）**存在，则它是唯一的：这再次来自**Church-Rosser 性质**。

- 这个结果在本例中仍然成立，因为像下面这样的冲突：

  $$
  \frac{\vdots \quad A \quad \lor 1\text{I} \quad A \lor B \quad [A] \ \vdots \ C \quad [B] \ \vdots \ C \quad \lor \text{E} \quad C \quad \vdots \ r \ D}{}
  $$

  可以以两种不同的方式转换，即：

  - **第一种转换**：
    $$
    [A] \quad \vdots \quad C \quad \vdots \ r \ D
    $$

  - **第二种转换**：
    $$
    \frac{\vdots \quad A \quad \lor 1\text{I} \quad A \lor B \quad [A] \ \vdots \ C \quad \vdots \ r \ D \quad [B] \ \vdots \ C \quad \vdots \ r \ D \quad \lor \text{E} \quad D}{}
    $$

  - **解决方法**：第二个推导转换为第一个推导，因此冲突被轻松解决。

#### **结果的推广**

可以将之前在 $(\land, \Rightarrow, \forall)$ 片段中获得的结果扩展到完整的演算，但代价是一些繁琐的复杂性。

- **参考文献**：[Prawitz] 提供了完成此操作的所有技术细节。
- **可归约性的抽象性质**：在 [Gir72] 中讨论了这种情况下的可归约性的抽象性质，当我们将其扩展到类型上的存在量化时，没有真正的问题。

**说明**：

- 我们不会在此给出证明，因为理论兴趣有限。
- 人们倾向于认为自然演绎应该被修改以纠正这些问题：如果一个连接词有如此糟糕的规则，可以忽略它（这是非常常见的态度），或者尝试改变自然演绎的精神，以便能够与其他规则和谐地集成。
- 看起来演算的 $(\bot, \lor, \exists)$ 片段并不是不可改变的。

此外，这些扩展既冗长又困难，而您不会学到任何新东西，除了可归约性的技术变化。因此，只需知道**强规范化定理（Strong Normalization Theorem）**在这种情况下也成立即可。

- 如果您想看到证明，可以查阅上述参考文献。

---

### **10.6 相关的函数演算**

回到 **Heyting** 的思想，在处理 $\bot$ 和 $\lor$ 的情况下，理解 **Curry-Howard 对应（Curry-Howard Isomorphism）** 是可能的（对 $\exists$ 的情况不会比对 $\forall$ 的情况得到更多的考虑）。

#### **10.6.1 空类型（Empty Type）**

**空类型（Emp）**被认为是**空的类型**。因此，将存在一个从 **Emp** 到任何类型 $U$ 的规范函数 $\varepsilon_U$：

- 如果 $t$ 的类型是 **Emp**，那么 $\varepsilon_U \ t$ 的类型是 $U$。

**五种情况下的转换（转换规则）**：

1. **乘积类型的第一投影**：
   $$
   \pi_1 (\varepsilon_{U \times V} \ t) \quad \equiv \quad \varepsilon_U \ t
   $$
   - 解释：从空类型构造的乘积类型的第一投影相当于从空类型到 $U$ 的 $\varepsilon_U$。

2. **乘积类型的第二投影**：
   $$
   \pi_2 (\varepsilon_{U \times V} \ t) \quad \equiv \quad \varepsilon_V \ t
   $$
   - 类似地，第二投影对应于从空类型到 $V$ 的 $\varepsilon_V$。

3. **函数类型的应用**：
   $$
   (\varepsilon_{U \to V} \ t) \ u \quad \equiv \quad \varepsilon_V \ t
   $$
   - 解释：将从空类型到函数类型的 $\varepsilon_{U \to V} \ t$ 应用于 $u$，相当于 $\varepsilon_V \ t$。

4. **嵌套的空类型**：
   $$
   \varepsilon_U (\varepsilon_{\text{Emp}} \ t) \quad \equiv \quad \varepsilon_U \ t
   $$
   - 解释：从空类型到空类型的 $\varepsilon_{\text{Emp}} \ t$，再应用 $\varepsilon_U$，等同于直接应用 $\varepsilon_U$。

5. **和类型的匹配**：
   $$
   \delta \ x. \ u \ y. \ v \ (\varepsilon_{R+S} \ t) \quad \equiv \quad \varepsilon_U \ t
   $$
   - 其中，$U$ 是 $u$ 和 $v$ 的公共类型。

**解释**：

- $\varepsilon_U$ 对应于 $\bot \text{E}$。
- 上述五个转换对应于 $\bot$ 的五个交换转换。

#### **10.6.2 和类型（Sum Type）**

对于和类型 $U + V$，我们有以下结构：

1. **左注入（$\iota_1$）**：
   $$
   \text{如果 } u : U, \quad \text{则 } \iota_1 \ u : U + V
   $$
   - 解释：将 $U$ 中的元素 $u$ 注入到 $U + V$ 的左侧。

2. **右注入（$\iota_2$）**：
   $$
   \text{如果 } v : V, \quad \text{则 } \iota_2 \ v : U + V
   $$
   - 解释：将 $V$ 中的元素 $v$ 注入到 $U + V$ 的右侧。

3. **匹配（$\delta$）**：

   $$
   \delta \ x. \ u \ y. \ v \ t
   $$

   - **类型**：如果 $x$、$y$ 分别是类型 $R$、$S$ 的变量，$u$、$v$、$t$ 分别是类型 $U$、$U$、$R + S$ 的项，那么 $\delta \ x. \ u \ y. \ v \ t$ 的类型是 $U$。
   - **解释**：这对应于函数式编程语言（如 **CAML**）中的模式匹配：

     ```ocaml
     match t with
     | inl x -> u
     | inr y -> v
     ```

**标准转换**：

1. **左注入的匹配**：
   $$
   \delta \ x. \ u \ y. \ v \ (\iota_1 \ r) \quad \equiv \quad u[r/x]
   $$
   - 解释：如果 $t$ 是左注入的结果，我们直接在 $u$ 中用 $r$ 替换 $x$。

2. **右注入的匹配**：
   $$
   \delta \ x. \ u \ y. \ v \ (\iota_2 \ s) \quad \equiv \quad v[s/y]
   $$

**交换转换**：

1. **乘积类型的第一投影**：
   $$
   \pi_1 (\delta \ x. \ u \ y. \ v \ t) \quad \equiv \quad \delta \ x. (\pi_1 \ u) \ y. (\pi_1 \ v) \ t \quad \text{其中 } U = V \times W
   $$

2. **乘积类型的第二投影**：
   $$
   \pi_2 (\delta \ x. \ u \ y. \ v \ t) \quad \equiv \quad \delta \ x. (\pi_2 \ u) \ y. (\pi_2 \ v) \ t \quad \text{其中 } U = V \times W
   $$

3. **函数类型的应用**：
   $$
   (\delta \ x. \ u \ y. \ v \ t) \ w \quad \equiv \quad \delta \ x. (u \ w) \ y. (v \ w) \ t \quad \text{其中 } U = V \to W
   $$

4. **与空类型的组合**：
   $$
   \varepsilon_W (\delta \ x. \ u \ y. \ v \ t) \quad \equiv \quad \delta \ x. (\varepsilon_W \ u) \ y. (\varepsilon_W \ v) \ t \quad \text{其中 } U = \text{Emp}
   $$

5. **嵌套匹配**：
   $$
   \delta \ x'. \ u' \ y'. \ v' \ (\delta \ x. \ u \ y. \ v \ t) \quad \equiv \quad \delta \ x. (\delta \ x'. \ u' \ y'. \ v' \ u) \ y. (\delta \ x'. \ u' \ y'. \ v' \ v) \ t \quad \text{其中 } U = V + W
   $$

- **解释**：这些转换完全对应于自然演绎中的规则。

#### **10.6.3 额外的转换**

我们记录以下与 $h\pi_1 \ t, \pi_2 \ t i \equiv t$ 和 $\lambda x. \ t \ x \equiv t$ 类似的转换：

1. **空类型的简化**：
   $$
   \varepsilon_{\text{Emp}} \ t \quad \equiv \quad t
   $$

2. **和类型的简化**：
   $$
   \delta \ x. (\iota_1 \ x) \ y. (\iota_2 \ y) \ t \quad \equiv \quad t
   $$

- **解释**：显然，等号两侧的项在指称意义上是相等的。

**注意**：

- 转换的方向并不十分明确：实际上，相反的方向更为自然。
- 这表明在函数演算中，对于这些转换，我们需要谨慎对待其方向性。

---

### **总结**

在本章中，我们探讨了自然演绎中关于 **析取（∨）**、**矛盾（⊥）** 和 **存在量词（∃）** 的推理规则及其缺陷。

- **引入规则**通常是良好的，但**消解规则**存在严重的问题，尤其是引入了与被消解公式无关的寄生公式 $C$。
- 这些问题导致了语法上的不足，需要通过**交换转换**和**额外的转换**来弥补。
- 我们讨论了**子公式性质**在完整演算中的困难，并说明了需要引入额外转换的必要性。
- 通过**交换转换**，我们可以解决一些推导中的问题，并维护推导的规范性和唯一性。
- 最后，我们介绍了与这些逻辑规则相关的**函数演算**，并讨论了**空类型**和**和类型**的表示，以及它们的转换规则。

---

### **后续展望**

- 在进一步的研究中，可以考虑修改自然演绎的结构，以更优雅地处理这些问题。
- 例如，探索其他逻辑系统，如**线性逻辑**，可能会提供更好的框架来处理这些连接词。
- 另外，深入研究 **Curry-Howard 对应**，了解逻辑证明和类型系统之间的关系，将有助于我们构建更强大的类型化编程语言。

---

如果您对本章的内容有任何疑问，或者需要进一步的解释和详细讲解，请随时提出来！

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



### ---------------------------



### ----------------------------