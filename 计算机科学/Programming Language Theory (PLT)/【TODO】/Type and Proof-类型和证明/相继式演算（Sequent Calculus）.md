[toc]

### 1. 简介

**相继式演算（Sequent Calculus）**是由 Gerhard Gentzen 在 1935 年提出的，它是对逻辑推理的一种形式化表示方法。相继式演算与自然演绎系统的主要区别在于处理假设的方式：在自然演绎中，假设通常出现在推理的顶部，而在相继式演算中，假设被直接表示在每一个相继式的左侧。这样的表示方式不仅更加直观，也更好地体现了逻辑连接词的对偶性。

### 2. 相继式的定义

一个**相继式（Sequent）**通常具有以下形式：

$$
A_1, A_2, \dots, A_n \vdash B_1, B_2, \dots, B_m
$$

其中，$A_1, A_2, \dots, A_n$ 是条件（或前提），$B_1, B_2, \dots, B_m$ 是结论。相继式表示“在满足左侧所有条件的情况下，可以推出右侧至少一个结论”。条件和结论的顺序是不区分的，且可以为空集。

- **当左侧为空**时，相继式表达的是“在没有任何条件下，右侧结论无条件成立”。
- **当右侧为空**时，相继式表达的是“如果左侧条件都成立，那么可以推导出矛盾”。

1. A 和 B 是任意命题逻辑的公式
2. Γ、∆ 是任意公式序列，代表任意多个公式（包括 0 个）

### 3. 相继式演算的演绎规则

在相继式演算中，演绎规则分为左规则和右规则，分别处理出现在 $\vdash$ 左侧和右侧的逻辑连接词。

#### 初始相继式 (Initial Sequent)
初始相继式是指任何公式作为条件时，可以证明它自身：
$$
\text{A (I)} \quad \frac{\Gamma, A \vdash \Delta, A}{\Gamma, A \vdash \Delta, A}
$$

#### 逻辑割 (Cut)
逻辑割规则允许我们将一个中间结论作为新的条件引入推理：
$$
\text{Cut} \quad \frac{\Gamma \vdash \Delta, A \quad \Gamma, A \vdash \Delta}{\Gamma \vdash \Delta}
$$

#### 合取与析取的规则

- **合取右规则 $\land R$**：将结论中的合取符号拆分为单独的结论：
  $$
  \frac{\Gamma \vdash A, \Delta \quad \Gamma \vdash B, \Delta}{\Gamma \vdash A \land B, \Delta}
  $$

- **合取左规则 $\land L$**：将条件中的合取符号拆分为多个条件：
  $$
  \frac{\Gamma, A, B \vdash \Delta}{\Gamma, A \land B \vdash \Delta}
  $$

- **析取右规则 $\lor R$**：将结论中的析取符号拆分为多个结论：
  $$
  \frac{\Gamma \vdash A, \Delta \quad \Gamma \vdash B, \Delta}{\Gamma \vdash A \lor B, \Delta}
  $$

- **析取左规则 $\lor L$**：将条件中的析取符号拆分为多个条件：
  $$
  \frac{\Gamma, A \vdash \Delta \quad \Gamma, B \vdash \Delta}{\Gamma, A \lor B \vdash \Delta}
  $$

#### 否定与蕴涵的规则

- **否定左规则 $\neg L$**：将否定符号从条件中移到结论中：
  $$
  \frac{\Gamma \vdash A, \Delta}{\Gamma, \neg A \vdash \Delta}
  $$

- **否定右规则 $\neg R$**：将否定符号从结论中移到条件中：
  $$
  \frac{\Gamma, A \vdash \Delta}{\Gamma \vdash \neg A, \Delta}
  $$

- **蕴涵右规则 $\rightarrow R$**：将条件中的蕴涵符号转化为结论中的条件：
  $$
  \frac{\Gamma, A \vdash B, \Delta}{\Gamma \vdash A \rightarrow B, \Delta}
  $$

- **蕴涵左规则 $\rightarrow L$**：将条件中的蕴涵符号转化为结论中的条件，并分解为两个步骤：
  $$
  \frac{\Gamma \vdash A, \Delta \quad \Gamma, B \vdash \Delta}{\Gamma, A \rightarrow B \vdash \Delta}
  $$

#### 矛盾规则 $\bot L$
矛盾可以推出任何结论：
$$
\text{矛盾 (Ex Falso Quodlibet)} \quad \frac{\Gamma, \bot \vdash \Delta}{\Gamma \vdash \Delta}
$$

### 4. 相继式演算在命题逻辑中的解释

一个相继式如 $A_1, A_2, \dots, A_n \vdash B_1, B_2, \dots, B_m$ 表示在逻辑上等价于命题逻辑中的公式 $(A_1 \land A_2 \land \dots \land A_n) \rightarrow (B_1 \lor B_2 \lor \dots \lor B_m)$。

在经典逻辑中，这可以进一步理解为 $\neg A_1 \lor \dots \lor \neg A_n \lor B_1 \lor \dots \lor B_m$，或者通过 De Morgan 定律表示为 $\neg (A_1 \land \dots \land A_n \land \neg B_1 \land \dots \land \neg B_m)$。

### 5. 相继式演算中的证明过程

相继式演算的证明过程主要通过消去逻辑连接词来简化相继式。具体过程如下：

- **初始相继式 (I)**：匹配条件与结论的相继式，可以终止证明。
- **消去逻辑连接词**：通过演绎规则一步步消去逻辑连接词，直到只剩下初始相继式为止。

此外，Gentzen 证明了在不失去证明能力的情况下，可以去掉逻辑割 (Cut) 规则。这使得相继式演算的证明过程更加简单和高效。

### 6. 例子

#### 例子 1: 证明 $\vdash A \lor \neg A$（排中律）

1. 初始相继式：$A \vdash A$
   $$
   \text{A (I)} \quad \frac{A \vdash A}{A \vdash A}
   $$
2. 否定引入 $\neg R$：将条件中的 $A$ 转化为结论中的 $\neg A$。
   $$
   \frac{A \vdash A}{\vdash \neg A}
   $$
3. 析取引入 $\lor R$：将 $A$ 和 $\neg A$ 组合成析取。
   $$
   \vdash A \lor \neg A
   $$

#### 例子 2: 证明 $(P \rightarrow Q) \rightarrow P \vdash P$

1. 初始相继式：$P \vdash P$
   $$
   \text{P (I)} \quad \frac{P \vdash P}{P \vdash P}
   $$
2. 蕴涵消去 $\rightarrow E$：由 $P \rightarrow Q$ 和 $P$ 得出 $Q$。
   $$
   \frac{P \vdash Q}{P \rightarrow Q \vdash P}
   $$
3. 初始相继式：$P \vdash P$
   $$
   \text{P (I)} \quad \frac{P \vdash P}{P \vdash P}
   $$

### 总结

**相继式演算**是一种强大的逻辑推理工具，它通过将假设和结论清晰地分开，使推理过程更加直观和系统化。相继式演算不仅能够处理命题逻辑，还能够推广到一阶逻辑和其他更复杂的逻辑系统，是逻辑学研究中的一个重要工具。通过理解和掌握相继式演算，我们可以更深入地理解逻辑推理的本质以及逻辑系统之间的相互关系。