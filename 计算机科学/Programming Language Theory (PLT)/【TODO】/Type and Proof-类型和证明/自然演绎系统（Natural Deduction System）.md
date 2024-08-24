[toc]

**自然演绎系统（Natural Deduction System）** 是由 Gentzen 于 1935 年提出的一种逻辑推理系统。与 Hilbert 系统相比，自然演绎系统更符合实际推理的直觉，且证明过程更简洁。它通过引入和消去规则，使得逻辑推理过程更为自然，并且有助于分析逻辑系统的一致性和完备性。

### 1. 自然演绎系统的特点

自然演绎系统的推理过程基于以下几点：

1. **逻辑连接词对应的引入规则和消去规则**：每个逻辑连接词都有相应的引入规则（说明如何得出这个连接词的结论）和消去规则（说明如何从一个已经包含这个连接词的公式中推导出新的结论）。

2. **假设的引入和消耗**：在推理中，假设可以被引入（假设某个命题为真），并且在推理过程中可以多次使用该假设。假设可以通过消耗（即通过推导得到其否定）来结束使用。

3. **推理过程中的开放假设**：在推理的最后，所有未被消耗的假设必须被明确声明。开放假设越少，结论越可靠。

### 2. 经典命题逻辑中的自然演绎规则

在经典命题逻辑中，常用的逻辑连接词包括 $\rightarrow$（蕴涵），$\land$（合取），$\lor$（析取），$\neg$（否定）和 $\bot$（矛盾）。以下是这些连接词在自然演绎系统中的引入和消去规则。

#### 合取 $\land$ 的规则

- **合取引入 $\land I$**：如果 $A$ 和 $B$ 都是可证的，那么 $A \land B$ 也是可证的。
  $$
  \frac{A \quad B}{A \land B} \ (\land I)
  $$

- **合取消去 $\land E$**：如果 $A \land B$ 是可证的，那么 $A$ 和 $B$ 也都是可证的。
  $$
  \frac{A \land B}{A} \ (\land E)
  $$
  $$
  \frac{A \land B}{B} \ (\land E)
  $$

#### 析取 $\lor$ 的规则

- **析取引入 $\lor I$**：如果 $A$ 是可证的，那么 $A \lor B$ 也是可证的；同样，如果 $B$ 是可证的，那么 $A \lor B$ 也是可证的。
  $$
  \frac{A}{A \lor B} \ (\lor I)
  $$
  $$
  \frac{B}{A \lor B} \ (\lor I)
  $$

- **析取消去 $\lor E$**：如果 $A \lor B$ 是可证的，且在假设 $A$ 和 $B$ 分别成立的情况下可以得出结论 $C$，那么 $C$ 也是可证的。
  $$
  \frac{[A] \quad C \quad [B] \quad C \quad A \lor B}{C} \ (\lor E)
  $$

#### 蕴涵 $\rightarrow$ 的规则

- **蕴涵引入 $\rightarrow I$**：如果在假设 $A$ 的前提下可以推导出 $B$，那么可以得出 $A \rightarrow B$。
  $$
  \frac{[A] \quad B}{A \rightarrow B} \ (\rightarrow I)
  $$

- **蕴涵消去 $\rightarrow E$**：如果 $A \rightarrow B$ 和 $A$ 都是可证的，那么 $B$ 也是可证的。这等同于 Hilbert 系统中的 MP 规则。
  $$
  \frac{A \rightarrow B \quad A}{B} \ (\rightarrow E)
  $$

#### 否定 $\neg$ 的规则

- **否定引入 $\neg I$**：如果假设 $A$ 可以推出矛盾 $\bot$，那么可以得出 $A$ 的否定 $\neg A$。
  $$
  \frac{[A] \quad \bot}{\neg A} \ (\neg I)
  $$

- **否定消去 $\neg E$**：如果 $A$ 和 $\neg A$ 都是可证的，那么可以推出矛盾 $\bot$。
  $$
  \frac{A \quad \neg A}{\bot} \ (\neg E)
  $$

#### Ex Falso Quodlibet

- **Ex Falso Quodlibet**：从矛盾可以导出任何命题。这是一个重要的逻辑规则，表示一旦得到矛盾，任何命题都是可证的。
  $$
  \frac{\bot}{A} \ (\bot)
  $$

#### Reductio ad Absurdum

- **Reductio ad Absurdum**（反证法）：如果假设 $\neg A$ 可以推出矛盾 $\bot$，那么可以得出 $A$。
  $$
  \frac{[\neg A] \quad \bot}{A} \ (RAA)
  $$

### 3. 自然演绎的例子

#### 例子 3.2

**推导 $A \land B \vdash B \land A$**

1. 从 $A \land B$ 可推导出 $A$ 和 $B$（合取消去 $\land E$）。
   $$
   A \land B \quad \land E \quad A
   $$
   $$
   A \land B \quad \land E \quad B
   $$
2. 由 $B$ 和 $A$ 可以引入 $B \land A$（合取引入 $\land I$）。
   $$
   B \quad A \quad \land I \quad B \land A
   $$

因此，$A \land B \vdash B \land A$。

#### 例子 3.3

**推导 $\vdash A \rightarrow \neg\neg A$**

1. 假设 $A$，我们需要推导出 $A \rightarrow \neg\neg A$。
2. 假设 $\neg A$，由 $A$ 和 $\neg A$ 可以推导出矛盾 $\bot$（否定消去 $\neg E$）。
   $$
   A \quad \neg A \quad \neg E \quad \bot
   $$
3. 因此，假设 $\neg A$ 推导出了矛盾，所以 $\neg A$ 是错误的，故 $\neg\neg A$ 成立（否定引入 $\neg I$）。
   $$
   \neg A \quad \bot \quad \neg I \quad \neg\neg A
   $$
4. 因此，可以推导出 $A \rightarrow \neg\neg A$（蕴涵引入 $\rightarrow I$）。

### 4. 自然演绎练习

- **练习 3.1.1**：证明 $A \rightarrow B \vdash \neg B \rightarrow \neg A$。

- **练习 3.1.2**：证明 $A \lor B \vdash \neg A \rightarrow B$。

- **练习 3.1.3**：证明 $\vdash A \lor \neg A$（排中律）。

### 总结

自然演绎系统提供了一种更符合直觉的逻辑推理方式，允许通过假设和消去假设来进行推理。通过定义每个逻辑连接词的引入和消去规则，自然演绎系统使得逻辑推理过程更加灵活和自然。理解自然演绎系统对于学习逻辑推理、形式证明以及证明论都有重要的帮助。