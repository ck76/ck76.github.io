[toc]

### 简介

**概念**: 本章介绍了**传名调用**（Call-by-name）求值策略，这是一种在 $\lambda$-演算中求解程序值的确定性方法。传名调用求值能够确定某个程序表达式是否可以被归约为一个 $\lambda$-抽象。在大步语义（Big-step semantics）中，表达式直接与其最终结果相关联，通常表示为 $M \Downarrow V$，其中 $M$ 是输入表达式，$V$ 是最终结果。而在小步归约（Small-step semantics）中，表达式通过多个中间步骤逐步归约。

**解释**:
- **大步语义**: 直接定义表达式 $M$ 和最终结果 $V$ 之间的关系。大步语义能够跳过中间步骤，直接描述初始表达式与最终值的关系。
- **小步语义**: 表达式通过多个中间步骤逐步归约到最终结果。

### 导入

引入了必要的模块和定义，包括 Propositional Equality、Data Product、Function，以及前一章节中的相关定义，如上下文、替换规则、和归约规则。

### 环境

**概念**: 在传名调用中，环境（Environment）是一个从变量到闭包（Closure，即项与其对应的环境）的映射。环境的使用使得传名调用求值的实现更加接近于实际编程语言的实现。

**解释**:
- **环境**: 映射变量到它们的闭包。通过环境，我们可以避免替换操作直接修改表达式中的变量。
- **闭包**: 一个 $\lambda$-表达式与其定义时的环境的组合。

**公式**:
- **ClosEnv** 定义了一个上下文到闭包的映射。
  $$
  \text{ClosEnv} \, \Gamma = \forall \, x : \Gamma \ni ★ \rightarrow \text{Clos}
  $$
- **闭包**的定义：
  $$
  \text{data Clos : Set} \, \text{where}
  \, \text{clos} : \forall \, \Gamma \rightarrow (\Gamma \vdash ★) \rightarrow \text{ClosEnv} \, \Gamma \rightarrow \text{Clos}
  $$

### 大步求值

**概念**: 大步求值描述了输入表达式 $M$ 和其求值结果 $V$ 之间的关系。这里的结果 $V$ 是一个 $\lambda$-抽象的闭包。

**解释**:
- **大步求值**: 使用环境来定义输入表达式与输出结果之间的关系。大步求值关系写作 $\gamma \vdash M \Downarrow V$，其中 $\gamma$ 是环境，$M$ 是输入表达式，$V$ 是结果值。
- **求值规则**:
  - **⇓-var**: 通过从环境中查找相关闭包来求值变量。
  - **⇓-lam**: 通过包装 $\lambda$-抽象与其环境，将其转变为闭包。
  - **⇓-app**: 处理函数应用。首先对操作项 $L$ 求值，如果结果是一个包含 $\lambda$-抽象的闭包，则在扩展后的环境中对该 $\lambda$-抽象的主体 $N$ 进行求值。

**公式**:
- 大步语义的定义：
  $$
  \text{data} \, \gamma \vdash M \Downarrow V : \forall \, \Gamma \rightarrow \text{ClosEnv} \, \Gamma \rightarrow (\Gamma \vdash ★) \rightarrow \text{Clos} \rightarrow \text{Set} \, \text{where}
  $$
  - **⇓-var**:
    $$
    \frac{\gamma \, x \equiv \text{clos} \, M \, \delta \quad \delta \vdash M \Downarrow V}{\gamma \vdash ` \, x \Downarrow V}
    $$
  - **⇓-lam**:
    $$
    \frac{}{\gamma \vdash \lambda \, M \Downarrow \text{clos} \, (\lambda \, M) \, \gamma}
    $$
  - **⇓-app**:
    $$
    \frac{\gamma \vdash L \Downarrow \text{clos} \, (\lambda \, N) \, \delta \quad (\delta, \text{clos} \, M \, \gamma) \vdash N \Downarrow V}{\gamma \vdash L \cdot M \Downarrow V}
    $$

### 大步语义是确定的

**概念**: 大步语义是确定性的，也就是说，如果大步语义关系将一个项 $M$ 求值为两个不同的结果 $V$ 和 $V'$，那么 $V$ 和 $V'$ 必然相同。

**解释**:
- **确定性**: 传名调用求值是一个部分函数，即对于相同的输入表达式，求值的结果是唯一的。
- **证明方法**: 通过对两个不同的求值过程进行归纳，证明它们的结果必须相同。

**公式**:
- 大步语义的确定性证明：
  $$
  \text{⇓-determ} : \forall \, \Gamma \, \gamma \, M \, V \, V' \rightarrow \gamma \vdash M \Downarrow V \rightarrow \gamma \vdash M \Downarrow V' \rightarrow V \equiv V'
  $$

### 大步求值蕴含 $\beta$-归约至 $\lambda$-抽象

**概念**: 如果一个表达式 $M$ 在大步求值下能够求出值 $V$，那么 $M$ 可以通过 $\beta$-归约归约为一个 $\lambda$-抽象。

**解释**:
- **必要性证明**: 我们通过对大步推导进行归纳，证明大步求值关系能够推导出相应的 $\beta$-归约关系。
- **推广命题**: 为了证明更加复杂的情况，如函数应用中的参数扩展，我们将命题推广以考虑任意环境和替换。

**公式**:
- 大步求值蕴含 $\beta$-归约的命题：
  $$
  \frac{\emptyset \vdash M \Downarrow \text{clos} \, (\lambda \, N') \, \delta}{\Sigma \, N \in \emptyset, ★ \vdash ★ \, (M —↠ \lambda \, N)}
  $$
- 主要引理：
  $$
  \text{⇓→—↠×≈} : \forall \, \Gamma \, \gamma \, \sigma \, M \, V \rightarrow \gamma \vdash M \Downarrow V \rightarrow \gamma \approx_{e} \sigma \rightarrow \Sigma \, N \in \emptyset \vdash ★ \, (\text{subst} \, \sigma \, M —↠ N) \times V \approx N
  $$

### β-归约至 λ-抽象蕴含大步求值

**概念**: 反过来，如果一个表达式可以通过 $\beta$-归约归约为 $\lambda$-抽象，那么它也可以通过大步求值得到相应的闭包。这部分的证明更加复杂。

**解释**:
- **Curry-Feys 标准化**: 经典的标准化方法，通过在 $\lambda$-演算中定义标准归约序列，将传名调用扩展为包含归约的标准序列。
- **左归约**（Left Reduction）: 传名调用的小步描述，可以与大步语义求值联系起来。

**公式**:
- Plotkin 定理和推论：
  - 定理 1（标准化）:
    $$
    M —↠ L \quad \text{当且仅当} \quad M \quad \text{能通过一个标准归约序列归约成} \quad L
    $$
  - 推论 1:
    $$
    M —↠ \lambda \, N \quad \text{当且仅当对于某个} \, N' \, M \, \text{能通过左归约成} \, \lambda \, N'
    $$
  - 定理 2:
    $$
    M \, \text{左归约成} \, \lambda \, N \quad \text{当且仅当} \quad \vdash M \Downarrow \lambda \, N
    $$
  - 推论 2:
    $$
    M —↠ \lambda \, N \quad \text{当且仅当对某个} \, N' \quad \vdash M \Downarrow \lambda \, N'
    $$

### Unicode

本章中使用了以下 Unicode 符号：
- **≈**: 表示近似（Almost Equal To）。
- **ₑ**: 下标小写字母 e。
- **⊢**:

 表示推导（Right Tack）。
- **⇓**: 表示大步求值关系（Downwards Double Arrow）。