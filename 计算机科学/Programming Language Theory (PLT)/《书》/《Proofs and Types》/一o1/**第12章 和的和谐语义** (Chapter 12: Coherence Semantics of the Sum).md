[toc]



### **第12章 和之一致性语义**

#### **引言**

在本章中，我们考虑第10章中引入的 **Emp** 和加号 **+**（对应于逻辑中的 **⊥** 和 **∨**）的指称语义（denotational semantics）。

- **Emp** 自然地被解释为其网（web）为空的**一致空间（coherence space）Emp**，对 **ε_U** 的解释立即跟随而来。

- **和类型（Sum）**，即 **A + B**，则带来了一些微妙的问题。当 **A** 和 **B** 是两个一致空间时，只有一种明显的和的概念，即下面介绍的**直接和（direct sum）**。不幸的是，**δ 方案**（casewise definition）无法被解释。这个问题也存在于其他类型的语义中，例如 **Scott 域**。

在尝试并拒绝了一定数量的变通方案后，我们回到了原始的解决方案，它适用于**线性函数**（即保留并集的函数）。我们得到了和类型的表示为：

$$
!A \oplus !B
$$

这种分解是**线性逻辑（linear logic）**的起源：运算符 **⊕**（直接和）和 **!**（线性化）实际上是独立的逻辑运算。

---

### **12.1 直接和**

**问题**：和类型的问题在于无法通过直接和来定义其解释：

- **定义直接和**：
  $$
  |A \oplus B| = |A| + |B| = \{1\} \times |A| \cup \{2\} \times |B|
  $$
  
  - **一致性（coherence）**：
    - $(1, \alpha) \ \underline{\smile} \ (1, \alpha')$（模 $A \oplus B$）如果 $\alpha \ \underline{\smile} \ \alpha'$（模 $A$）。
    - $(2, \beta) \ \underline{\smile} \ (2, \beta')$（模 $A \oplus B$）如果 $\beta \ \underline{\smile} \ \beta'$（模 $B$）。
    - 否则，不一致。

- **问题所在**：
  
  - 在域论的角度，这相当于取不交并（disjoint union）并将空集元素识别为一个元素，有时称为**合并和（amalgamated sum）**。
  
  - 如果我们定义从 $A$ 到 $A \oplus B$ 的**注入函数** $\text{Inj}_1$，以及从 $B$ 到 $A \oplus B$ 的 $\text{Inj}_2$：

    - $\text{Inj}_1(a) = \{1\} \times a$
    - $\text{Inj}_2(b) = \{2\} \times b$

  - 那么，$A \oplus B$ 的每个对象都可以写成 $\text{Inj}_1(a)$ 或 $\text{Inj}_2(b)$ 的形式。然而，对于空集，有 $\emptyset = \text{Inj}_1(\emptyset) = \text{Inj}_2(\emptyset)$，这导致了分解的非唯一性。

  - **结果**：无法定义基于情况的函数，例如从 $A$ 到 $C$ 的稳定函数 $F$ 和从 $B$ 到 $C$ 的稳定函数 $G$，组合成一个函数 $H$：

    - $H(\text{Inj}_1(a)) = F(a)$
    - $H(\text{Inj}_2(b)) = G(b)$

    - **原因**：对于参数 $\emptyset$，$F(\emptyset)$ 没理由等于 $G(\emptyset)$。

---

### **12.2 提升和（Lifted Sum）**

**初步解决方案**：

- 通过向 $|A \oplus B|$ 添加两个标签 $1$ 和 $2$，形成 $A \sqcup B$。

  - 标签 $1$ 与 $(1, \alpha)$ 一致，但与 $(2, \beta)$ 不一致。

  - 类似地，标签 $2$ 与 $(2, \beta)$ 一致，但与 $(1, \alpha)$ 不一致。

- **定义新的注入函数**：

  - $q_1(a) = \{1\} \cup \text{Inj}_1(a)$

  - $q_2(b) = \{2\} \cup \text{Inj}_2(b)$

- **基于情况的定义可以实现**：

  - 从 $F: A \to C$ 和 $G: B \to C$，可以定义 $H: A \sqcup B \to C$：

    - $H(q_1(a)) = F(a)$

    - $H(q_2(b)) = G(b)$

    - 如果 $c \cap \{1, 2\} = \emptyset$，则 $H(c) = \emptyset$。

  - **解释**：为了确定 $\gamma \in H(c)$，我们在 $c$ 中寻找标签 $1$ 或 $2$，如果找到，例如 $1$，则将 $c$ 写成 $q_1(a)$，然后检查 $\gamma \in F(a)$。

- **好处**：这种解决方案解释了标准转换方案：

  - $\delta \ x. \ u \ y. \ v \ (\iota_1 \ r) \to u[r/x]$

  - $\delta \ x. \ u \ y. \ v \ (\iota_2 \ s) \to v[s/y]$

- **问题**：

  - 然而，项 $\delta \ x.(\iota_1 x) \ y.(\iota_2 y) \ z$ 的解释 $H$，其定义为：

    - $H(q_1(a)) = q_1(a)$

    - $H(q_2(b)) = q_2(b)$

    - $H(c) = \emptyset$，如果 $c \cap \{1, 2\} = \emptyset$

  - **不总是满足 $H(c) = c$**。实际上，这个等式仅在 $c$ 形如 $q_1(a)$、$q_2(b)$ 或 $\emptyset$ 时成立。

- **结论**：

  - 尽管存在这种不完美，我们必须拒绝这种语义。即使我们不确定如何使用它，等式 $\delta \ x.(\iota_1 x) \ y.(\iota_2 y) \ t = t$ 在析取的隐含对称性中起作用。

  - 我们并非为了任何代价寻找模型，而是寻找一个令人信服的模型。

---

### **12.2.1 dI-域**

**另一种解决方案**：

- 我们只考虑 $A \sqcup B$ 中的对象 $q_1 a$、$q_2 b$ 和 $\emptyset$。

- **结果**：

  - 一切都将正常工作，但所得的结构不再是一个一致空间。

  - **原因**：例如，如果 $\alpha \in |A|$，则 $q_1 \alpha = \{1, (1, \alpha)\}$ 出现在 $A \sqcup B$ 中，但它的子集 $\{(1, \alpha)\}$ 不在其中。

- **需要引入偏序关系**：

  - 这里，$1 < (1, \alpha)$，$2 < (2, \beta)$。

  - 我们感兴趣的是一致空间的向下闭合的子集。

- **事件结构（event structures）**：

  - 根据 **Winskel** 的观点，标记被视为“事件”，一致性指定了两个事件何时可以共存，偏序关系 $\alpha' < \alpha$ 表示如果事件 $\alpha$ 存在，那么事件 $\alpha'$ 也必须存在。

- **结论**：

  - 这样得到的空间正是 **Berry** 最初的 **dI-域**。

  - 这可以作为定义归纳数据类型的另一种方式。

- **有限损害**：

  - 由于可以要求对于所有 $\alpha \in |A|$，满足 $\{\alpha' \mid \alpha' < \alpha\}$ 是有限的，因此向下闭包的有限集始终是有限的，从而避免了 **Scott 域** 的一个问题。

- **缺点**：

  - 然而，这迫使我们离开一致空间的类别，并使用了一个妥协系统概念简单性的偏序关系。

- **寻求替代方案**：

  - 我们希望找到其他保留一致空间类别的解决方案。

---

### **12.3 线性性**

**定义线性函数**：

- **观察**：操作 $t \mapsto t \ u$ 是**严格的（strict）**，即保留 $\emptyset$。

- **更进一步**：它是**线性的（linear）**。

- **线性函数的意义**：

  - 一个稳定函数 $F$ 从 $A$ 到 $B$ 是线性的，当且仅当其轨迹（trace）由形如 $({\alpha}, \beta)$ 的对组成，其中 $\alpha \in |A|$，$\beta \in |B|$。

- **线性函数的特性**：

  1. **保空性（F(∅) = ∅）**：

     - 要有 $\beta \in F(\emptyset)$，需要存在 $a^\circ \subseteq \emptyset$，使得 $(a^\circ, \beta) \in \text{Tr}(F)$。

     - 但 $a^\circ = \emptyset$，无法是单元素集，因此 $F(\emptyset) = \emptyset$。

  2. **保并性（F(a_1 \cup a_2) = F(a_1) \cup F(a_2)）**：

     - 显然，$F(a_1) \cup F(a_2) \subseteq F(a_1 \cup a_2)$。

     - 反之，如果 $\beta \in F(a_1 \cup a_2)$，则存在 $a' \subseteq a_1 \cup a_2$，使得 $(a', \beta) \in \text{Tr}(F)$。

     - 由于 $a'$ 是单元素集，因此 $a' \subseteq a_1$ 或 $a' \subseteq a_2$，从而 $\beta \in F(a_1)$ 或 $\beta \in F(a_2)$。

- **线性函数的刻画**：

  - **条件**：稳定函数 $F$ 是线性的，当且仅当满足上述保空性和保并性。

  - **证明**：如果 $\beta \in F(a)$ 且 $a$ 是极小的，则 $a$ 必须是单元素集。

---

### **12.3.2 线性蕴涵**

**定义线性蕴涵 $A \multimap B$**：

- 定义为由所有稳定线性函数从 $A$ 到 $B$ 构成的空间。

- **其一致空间的结构**：

  - $|A \multimap B| = |A| \times |B|$

  - **一致性关系**：

    - $(\alpha, \beta) \ \underline{\smile} \ (\alpha', \beta')$（模 $A \multimap B$）当且仅当：

      1. 如果 $\alpha \ \underline{\smile} \ \alpha'$（模 $A$），则 $\beta \ \underline{\smile} \ \beta'$（模 $B$）。

      2. 如果 $\beta$ 与 $\beta'$ 不一致，则 $\alpha$ 与 $\alpha'$ 不一致。

- **线性否定 $A^\perp$**：

  - 定义为与 $A$ 有相同的标记集，且一致性关系相反。

- **反对称性**：

  - 线性蕴涵的一个重要性质是反对称性。

  - **等价关系**：

    - $(\alpha, \beta) \ \underline{\smile} \ (\alpha', \beta')$（模 $A \multimap B$）当且仅当 $(\beta, \alpha) \ \underline{\smile} \ (\beta', \alpha')$（模 $B^\perp \multimap A^\perp$）。

- **意义**：

  - 当函数是线性的时，输入和输出的角色是对称的。

  - 这引入了一种功能依赖的对称形式，参数和结果的角色通过线性否定 $A \mapsto A^\perp$ 表达。

---

### **12.4 线性化**

**定义 $!A$（“当然 $A$”）**：

- $|!A| = A_{\text{fin}}$，即 $A$ 中的所有有限集。

- 一致性关系为：$a_1 \ \underline{\smile} \ a_2$（模 $!A$）当且仅当 $a_1 \cup a_2 \in A$。

- **基本函数**：

  - 从 $A$ 到 $!A$，定义为 $a \mapsto !a = \{ a^\circ \mid a^\circ \subseteq a, a^\circ \text{有限} \}$。

  - 这个函数是稳定的，但远非线性的。

- **重要性质**：

  - **同构**：$A \to B$ 等于 $!A \multimap B$。

- **解释**：

  - 通过改变源空间，每个稳定函数都可以视为线性的。

- **操作**：

  - 对于稳定函数 $F: A \to B$，可以定义线性稳定函数 $\text{Lin}(F): !A \to B$。

    - $\text{Lin}(F)(!a) = F(a)$

  - 对于线性函数 $G: !A \to B$，可以定义稳定函数 $\text{Delin}(G): A \to B$。

    - $\text{Delin}(G)(a) = G(!a)$

  - **性质**：$\text{Lin}$ 和 $\text{Delin}$ 互为逆。

---

### **12.5 线性化的和**

**定义 $A \sqcup B = !A \oplus !B$**：

- **注入函数**：

  - $q_1 a = \{1\} \times !a$

  - $q_2 b = \{2\} \times !b$

- **基于情况的定义不再是问题**：

  - 如果 $F$ 是从 $A$ 到 $C$ 的稳定函数，$G$ 是从 $B$ 到 $C$ 的稳定函数，定义 $H$：

    - $H(\{1\} \times A) = \text{Lin}(F)(A)$

    - $H(\{2\} \times B) = \text{Lin}(G)(B)$

  - 由于 $\text{Lin}(F)$ 和 $\text{Lin}(G)$ 是线性的，所以 $H(\emptyset) = \emptyset$，没有冲突。

- **解释等式**：

  - **标准转换**：$\delta \ x. \ u \ y. \ v \ (\iota_1 \ r)$ 的解释是 $\text{Lin}(F)(!a) = F(a)$，与 $u[r/x]$ 的解释一致。

  - **等式 $\delta \ x.(\iota_1 x) \ y.(\iota_2 y) \ t = t$**：

    - 对于 $t$ 的解释 $q_1 A$，$\delta \ x.(\iota_1 x) \ y.(\iota_2 y) \ t$ 的解释是 $\text{Lin}(q_1)(A) = \{1\} \times A$，与 $t$ 的解释一致。

- **交换转换**：

  - 对于形式 $E(\delta \ x. \ u \ y. \ v \ t) = \delta \ x.(E u) \ y.(E v) \ t$ 的转换，$E$ 是一个消解。

  - **关键点**：所有的消解都是线性的。

  - **结论**：可以证明 $\text{Lin}(E \circ F) = E \circ \text{Lin}(F)$，因此转换成立。

---

### **12.6 张量积和单位元**

**定义张量积 $A \otimes B$**：

- **标记集**：$|\ A \otimes B| = \{ \langle \alpha, \beta \rangle \mid \alpha \in |A|, \beta \in |B| \}$

- **一致性关系**：

  - $\langle \alpha, \beta \rangle \ \underline{\smile} \ \langle \alpha', \beta' \rangle$（模 $A \otimes B$）当且仅当 $\alpha \ \underline{\smile} \ \alpha'$（模 $A$），且 $\beta \ \underline{\smile} \ \beta'$（模 $B$）。

- **张量积的对偶（线性否定）**称为**平行（par）**或**张量和**：

  - $(A \otimes B)^\perp = A^\perp \parr B^\perp$

- **线性蕴涵的关系**：

  - $A \multimap B = A^\perp \parr B = (A \otimes B^\perp)^\perp$

- **单位元**：

  - 四个二元运算 $\oplus$、$\&$（直积）、$\otimes$ 和 $\parr$ 各有一个单位元，分别称为 $0$、$>$、$1$ 和 $\bot$。

  - **对于一致空间**，它们成对地重合：

    - $0 = > = \text{Emp}$，其中 $|\text{Emp}| = \emptyset$。

    - $1 = \bot = \text{Sgl}$，其中 $|\text{Sgl}| = \{\bullet\}$。

- **问题**：

  - 哪个是一致空间和稳定映射的终对象？

  - 对于线性映射呢？

  - 这些类型如何与自然演绎中的矛盾和重言式相关？

---

### **总结**

在本章中，我们探讨了 **Emp** 和 **和类型（Sum Type）** 的一致性语义。

- **Emp**：自然地被解释为网为空的一致空间。

- **和类型的困难**：

  - 直接和的定义导致了分解的非唯一性，无法定义基于情况的函数。

  - 提升和提供了一种初步解决方案，但引入了新的问题。

- **线性函数和线性化**：

  - 引入了线性函数的概念，线性函数满足保空性和保并性。

  - 通过线性化，可以将每个稳定函数视为线性的。

  - **重要结果**：$A \to B = !A \multimap B$

- **线性化的和**：

  - 通过定义 $A \sqcup B = !A \oplus !B$，解决了和类型的解释问题。

  - 这种表示使得基于情况的定义成为可能，并解释了标准转换和交换转换。

- **张量积和单位元**：

  - 定义了张量积和其对偶运算，以及它们的单位元。

  - 探讨了这些运算与逻辑运算的关系。

---

### **后续展望**

- **线性逻辑**：本章的讨论引出了线性逻辑的概念，线性逻辑在处理资源和并发方面有着重要的应用。

- **一致空间的进一步研究**：可以继续研究一致空间、稳定映射和线性映射之间的关系，以及它们在计算机科学和逻辑中的应用。

- **类型系统和语义学**：深入理解类型系统的语义学，有助于构建更强大的编程语言和证明系统。

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