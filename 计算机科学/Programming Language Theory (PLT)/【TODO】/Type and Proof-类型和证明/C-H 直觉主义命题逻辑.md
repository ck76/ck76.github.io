[toc]

### 直觉主义命题逻辑 (Intuitionistic Propositional Logic)

#### 1. 简介

直觉主义命题逻辑（Intuitionistic Logic）是逻辑学中的一种分支，它与经典逻辑的主要区别在于对真值的解释。直觉主义逻辑的基础是构造性数学哲学，认为数学对象和命题的存在性必须通过构造来证明。直觉主义者强调“存在必须被构造”，即任何数学命题的真值必须可以通过具体的构造过程加以证明，而不仅仅是通过逻辑推导。

#### 2. 直觉主义逻辑 vs. 经典逻辑

- **经典逻辑**认为命题的真值可以是“真”或“假”，且遵循排中律 $A \vee \neg A$。
- **直觉主义逻辑**不承认排中律，认为命题 $A$ 的真值需要通过构造 $A$ 来证明，只有当能够具体地构造出 $A$ 时，$A$ 才能被认为是真的。

#### 3. 语法

直觉主义逻辑的语法包括以下逻辑连接词：

$$
A, B ::= \alpha \;|\; A \rightarrow B \;|\; A \wedge B \;|\; \top \;|\; \bot
$$

- $\alpha$：原子命题。
- $A \rightarrow B$：表示 $A$ 蕴含 $B$，即如果能构造出 $A$，那么可以从中构造出 $B$。
- $A \wedge B$：表示 $A$ 和 $B$ 都能被构造。
- $\top$：表示一种“绝对真”或“证明已完成”的状态。
- $\bot$：表示一种矛盾状态。

#### 4. BHK 语义

BHK (Brouwer–Heyting–Kolmogorov) 语义是直觉主义逻辑中的一种解释框架，主要用于理解命题的构造性含义。其核心思想如下：

- **蕴含 $A \rightarrow B$**：要证明 $A \rightarrow B$，需要给出一个从 $A$ 到 $B$ 的构造性方法。也就是说，如果我们可以构造 $A$，就能从中构造出 $B$。
- **合取 $A \wedge B$**：证明 $A \wedge B$ 的方法是同时构造出 $A$ 和 $B$。
- **析取 $A \vee B$**：证明 $A \vee B$ 的方法是要么构造出 $A$，要么构造出 $B$。

#### 5. 自然演绎系统 (Natural Deduction System)

在直觉主义逻辑中，自然演绎是一种证明系统，通过规则的形式演绎命题的证明。该系统主要由引入规则和消去规则组成，每个逻辑连接词都有其对应的规则。

**主要演绎规则：**

1. **公理 (Axiom):**
   - 如果假设 $A$ 成立，那么 $A$ 本身就是一个证明。
   - 记法：$[x : A] \vdash A \; (ax)$

2. **合取引入 (∧ Introduction):**
   - 如果基于假设集 $\Gamma$，可以分别证明 $A$ 和 $B$，则可以证明 $A \wedge B$。
   - 记法：$\frac{\Gamma \vdash A \quad \Gamma \vdash B}{\Gamma \vdash A \wedge B} \; (\wedge I)$

3. **合取消去 (∧ Elimination):**
   - 如果可以证明 $A \wedge B$，则可以分别推出 $A$ 和 $B$。
   - 记法：$\frac{\Gamma \vdash A \wedge B}{\Gamma \vdash A} \; (\wedge E_1)$ 和 $\frac{\Gamma \vdash A \wedge B}{\Gamma \vdash B} \; (\wedge E_2)$

4. **真理引入 (⊤ Introduction):**
   - 无需任何假设就可以得到 $\top$。
   - 记法：$\Gamma \vdash \top \; (\top I)$

5. **矛盾消去 (⊥ Elimination):**
   - 如果得出了矛盾 $\bot$，则可以证明任何命题 $A$。
   - 记法：$\frac{\Gamma \vdash \bot}{\Gamma \vdash A} \; (\bot E)$

6. **蕴涵引入 (→ Introduction):**
   - 如果假设 $A$ 成立，可以推导出 $B$，那么可以推导出 $A \rightarrow B$。注意，在这种规则中，$A$ 被“消耗”了。
   - 记法：$\frac{\Gamma, x : A \vdash B}{\Gamma \vdash A \rightarrow B} \; (→ I)$

7. **蕴涵消去 (→ Elimination):**
   - 如果可以证明 $A \rightarrow B$ 且可以证明 $A$，则可以推导出 $B$。这与 Hilbert 系统中的 MP 规则等价。
   - 记法：$\frac{\Gamma \vdash A \rightarrow B \quad \Gamma \vdash A}{\Gamma \vdash B} \; (→ E)$

#### 6. 直觉主义逻辑与类型论的关系

直觉主义逻辑与类型论密切相关，特别是在 Curry-Howard 同构（Curry-Howard Isomorphism）中：
- **命题** 对应 **类型**。
- **证明** 对应 **程序**。
- **推导规则** 对应 **类型系统中的定型规则**。

通过这种联系，我们可以将逻辑推理与程序构造统一起来，直觉主义逻辑在这种同构关系下具有特别的意义，因为它要求每一个逻辑命题的证明都是一个构造性的过程，这与程序设计中的函数和算法的设计思想非常一致。

### 总结

直觉主义命题逻辑是经典逻辑的一个重要变体，它强调构造性和操作性。它在计算机科学，特别是程序验证、类型论和函数式编程等领域有广泛的应用。理解直觉主义逻辑有助于深入理解类型论和逻辑系统的构造性本质，从而更好地掌握编程语言设计和验证技术。
