[toc]

接下来我们逐小节分析“指称语义的可组合性”这一章节，解释其涉及到的概念和公式。

### 简介

#### 概念
- **指称语义的可组合性**：指称语义满足可组合性意味着对于给定的程序片段，其指称语义可以通过组合其子片段的指称语义来得到。换句话说，整个程序的语义是由其各部分的语义组合而成的。

#### 解释
在本章中，我们要证明以下等式，显示出指称语义是可组合的：
$$
\mathcal{E}(` x) \simeq ...
$$
$$
\mathcal{E}(\lambda M) \simeq ... \mathcal{E}(M) ...
$$
$$
\mathcal{E}(M \cdot N) \simeq ... \mathcal{E}(M) ... \mathcal{E}(N) ...
$$

这些等式表明，指称语义可以被定义为一个递归函数。我们将在本章末尾给出该函数的定义，并证明其与现有的指称语义函数 $\mathcal{E}$ 等价。

### 导入

#### 概念
- **导入模块**：为了实现和证明指称语义的可组合性，我们需要导入一些模块和数据结构，特别是指称语义所依赖的基本定义和规则。

#### 解释
导入的模块包括 `Data.Product`、`Data.Sum`、`Data.Unit`、`plfa.part2.Untyped` 和 `plfa.part3.Denotational`，这些模块为我们提供了基本的数据类型、语法结构和指称语义相关的规则。通过导入这些模块，可以方便地使用其中定义的概念和工具来进行证明。

### λ-抽象的方程

#### 概念
- **$\lambda$-抽象的指称语义**：$\lambda$-抽象的语义表示为一个从输入值映射到输出值的函数。为了定义该语义，需要考虑语义中非递归的部分，包括规则 `↦-intro`、`⊥-intro` 和 `⊔-intro`。

- **$\mathcal{F}$ 函数**：用于将项的指称语义从一个较大上下文映射到一个较小上下文中。这个函数的定义非常类似于函数式编程中的柯里化操作（Currying）。

#### 解释
我们定义一个函数 $\mathcal{F}$ 来处理 $\lambda$-抽象的指称语义。这个函数接受一个指称 $D$，一个环境 $\gamma$，以及一个值 $v$，并返回新的指称：
$$
\mathcal{F} : \forall \{\Gamma\} \rightarrow \text{Denotation}(\Gamma , ★) \rightarrow \text{Denotation} \Gamma
$$
其实现如下：
$$
\mathcal{F} D \gamma (v \mapsto w) = D (\gamma `, v) w
$$
$$
\mathcal{F} D \gamma \bot = \top
$$
$$
\mathcal{F} D \gamma (u \sqcup v) = (\mathcal{F} D \gamma u) \times (\mathcal{F} D \gamma v)
$$

- 对于值 $v \mapsto w$，$\mathcal{F}$ 函数返回 $D$ 在扩展环境 $\gamma , v$ 下的输出 $w$。
- 对于 $\bot$，$\mathcal{F}$ 函数返回 $\top$，表示该值无意义。
- 对于 $u \sqcup v$，$\mathcal{F}$ 函数返回 $u$ 和 $v$ 在环境 $\gamma$ 下分别应用 $\mathcal{F}$ 的结果的笛卡尔积。

这一部分的核心思想是将 $\lambda$-抽象的语义从较大的上下文中抽象出来，并简化其为非递归的部分，最终证明其与 $\mathcal{E}(\lambda N)$ 等价。

### sub-ℱ 的定义和性质

#### 概念
- **sub-ℱ**：该函数处理 $\mathcal{F}$ 在值递归过程中可能发生的情况，确保在更大的值 $v$ 映射到较小的值 $u$ 时，函数 $\mathcal{F}$ 仍然成立。

#### 解释
sub-ℱ 的定义如下：
$$
\text{sub-}\mathcal{F} : \forall \{\Gamma\} \{N : \Gamma , ★ \vdash ★\} \{ \gamma v u \} \rightarrow \mathcal{F} (\mathcal{E} N) \gamma v \rightarrow u \sqsubseteq v \rightarrow \mathcal{F} (\mathcal{E} N) \gamma u
$$

- 该函数确保当 $u \sqsubseteq v$ 时，$\mathcal{F}$ 在值 $u$ 上也能成立。这通过对 $u \sqsubseteq v$ 的推导过程进行归纳来实现。

### λ-抽象语义的证明

#### 概念
- **$\mathcal{E}(\lambda N) \simeq \mathcal{F}(\mathcal{E} N)$ 的证明**：这一证明显示了 $\lambda$-抽象的语义确实可以通过函数 $\mathcal{F}$ 来组合，这表明 $\lambda$-抽象的指称语义是可组合的。

#### 解释
这一部分通过归纳法证明了 $\mathcal{E}(\lambda N) \simeq \mathcal{F}(\mathcal{E} N)$。关键步骤如下：

- **规则 `↦-intro` 的情况**：当存在 $\lambda$-抽象的指称时，直接使用它即可。
- **规则 `⊥-intro` 的情况**：返回 $\top$，表示无意义的值。
- **规则 `⊔-intro` 的情况**：使用笛卡尔积操作将两个值合并，保持 $\mathcal{F}$ 的组合性。

通过这些步骤，我们证明了 $\lambda$-抽象的语义向右的方向（即从 $\mathcal{E}$ 到 $\mathcal{F}$）的可组合性。

### λ-抽象语义的反演引理

#### 概念
- **lambda-inversion**：这是一个反演引理，它表明如果我们知道某个 $\lambda$-抽象的指称语义为某个值的函数映射，那么可以推导出在该映射下的具体输入和输出的关系。

#### 解释
该引理的公式表示为：
$$
\text{lambda-inversion} : \forall \{\Gamma\} \{\gamma : \text{Env} \Gamma\} \{N : \Gamma , ★ \vdash ★\} \{v_1 v_2 : \text{Value}\} \rightarrow \gamma \vdash \lambda N \downarrow v_1 \mapsto v_2 \rightarrow (\gamma `, v_1) \vdash N \downarrow v_2
$$

这个引理在证明“归约保持指称不变”时非常有用。

---



### 函数应用的方程

#### 概念
- **函数应用的指称语义**：函数应用 $M \cdot N$ 的指称语义表示为将两个指称语义组合起来，类似于函数式编程中的应用操作。它要求我们定义一个运算符 $●$，用于组合两个指称语义，以模拟函数应用的语义。

#### 解释
我们需要定义一个函数 $●$，该函数接受两个指称语义，并将其组合为新的指称语义。这个操作符 $●$ 用于捕捉函数应用的语义：
$$
\_●\_ : \forall \{\Gamma\} \rightarrow \text{Denotation} \Gamma \rightarrow \text{Denotation} \Gamma \rightarrow \text{Denotation} \Gamma
$$
具体定义如下：
$$
(D_1 ● D_2) \gamma w = w \sqsubseteq \bot \sqcup \Sigma [v \in \text{Value}] (D_1 \gamma (v \mapsto w) \times D_2 \gamma v)
$$

解释：
- $w \sqsubseteq \bot$ 表示当 $w$ 是 $\bot$ 时，函数应用的结果仍然是 $\bot$。
- $\Sigma [v \in \text{Value}] (D_1 \gamma (v \mapsto w) \times D_2 \gamma v)$ 表示在 $D_1$ 中找到一个函数 $v \mapsto w$，并且在 $D_2$ 中找到 $v$，则应用的结果是 $w$。

### ℰ·→●ℰ 的证明

#### 概念
- **函数应用的语义方程的向右证明**：这一证明展示了从函数应用的指称语义 $\mathcal{E}(M \cdot N)$ 到组合运算符 $●$ 的等价性。

#### 解释
我们对语义进行归纳，证明了：
$$
\mathcal{E}(M \cdot N) \gamma v \implies (\mathcal{E}(M) ● \mathcal{E}(N)) \gamma v
$$

具体步骤：
1. **对于 `↦-elim` 的情况**：假设我们有 $\gamma \vdash M \downarrow (v' \mapsto v)$ 和 $\gamma \vdash N \downarrow v'$，那么我们可以推导出 $(\mathcal{E}(M) ● \mathcal{E}(N)) \gamma v$。
2. **对于 `⊥-intro` 的情况**：当 $v = \bot$ 时，我们有 $v \sqsubseteq \bot$。
3. **对于 `⊔-intro` 的情况**：我们处理了组合运算符 $●$ 的多个情况，分别讨论 $v_1 \sqsubseteq \bot$ 和 $v_2 \sqsubseteq \bot$，以及 $\gamma \vdash M \downarrow v'_1 \mapsto v_2$ 的子情况。

这一部分通过详细归纳展示了函数应用的指称语义如何通过 $●$ 运算符进行组合。

### ●ℰ→ℰ· 的证明

#### 概念
- **函数应用的语义方程的向左证明**：证明了从 $●$ 运算符的语义到函数应用 $\mathcal{E}(M \cdot N)$ 的等价性。

#### 解释
通过对 $(\mathcal{E}(M) ● \mathcal{E}(N)) \gamma v$ 进行情况分析，我们可以证明：
$$
(\mathcal{E}(M) ● \mathcal{E}(N)) \gamma v \implies \mathcal{E}(M \cdot N) \gamma v
$$

关键点：
1. **对于 $v \sqsubseteq \bot$ 的情况**，通过 $⊥-intro$ 规则得到 $\gamma \vdash M \cdot N \downarrow \bot$。
2. **对于 $\Sigma [v_1 \in \text{Value}]$ 的情况**，我们通过 $↦-elim$ 规则得出 $\gamma \vdash M \cdot N \downarrow v$。

### 变量的反演引理

#### 概念
- **变量的反演引理**：当我们知道某个变量的指称语义为某个值时，可以推导出该值与环境中变量值之间的关系。

#### 解释
该引理的公式表示为：
$$
\text{var-inv} : \forall \{\Gamma v x\} \{\gamma : \text{Env} \Gamma\} \rightarrow \mathcal{E}(` x) \gamma v \implies v \sqsubseteq \gamma x
$$

这个引理表明，如果我们有 $\gamma \vdash ` x \downarrow v$，那么 $v$ 必须是环境中变量 $\gamma x$ 的子集。

### 指称语义方程的完善

#### 概念
- **指称语义方程的完善**：通过前述证明和推导，最终完善了 $\mathcal{E}$ 的定义，使其能够正确描述程序的指称语义。

#### 解释
最终我们得到了三个关键的语义方程：
1. **变量**：$\mathcal{E}(` x) \simeq (\lambda \gamma v \rightarrow v \sqsubseteq \gamma x)$。
2. **$\lambda$-抽象**：$\mathcal{E}(\lambda N) \simeq \mathcal{F}(\mathcal{E}(N))$。
3. **应用**：$\mathcal{E}(L \cdot M) \simeq \mathcal{E}(L) ● \mathcal{E}(M)$。

通过这些方程，我们证明了指称语义的可组合性，即程序的语义是由其各部分的语义组合而成的。

### 合同性

#### 概念
- **合同性（Congruence）**：合同性表明，如果两个项在语义上是等价的，那么将它们替换到更大的上下文中，整个上下文仍然是等价的。

#### 解释
通过应用之前的语义方程和组合性，我们证明了 $\lambda$-抽象和应用的合同性：
1. **$\lambda$-抽象的合同性**：如果 $\mathcal{E}(N) \simeq \mathcal{E}(N')$，那么 $\mathcal{E}(\lambda N) \simeq \mathcal{E}(\lambda N')$。
2. **应用的合同性**：如果 $\mathcal{E}(L) \simeq \mathcal{E}(L')$ 且 $\mathcal{E}(M) \simeq \mathcal{E}(M')$，那么 $\mathcal{E}(L \cdot M) \simeq \mathcal{E}(L' \cdot M')$。

### 可组合性

#### 概念
- **可组合性（Compositionality Property）**：可组合性说明，在同一语境下，两个指称相等的项会产生两个指称相等的程序。

#### 解释
我们定义了“语境”和“在语境下”的概念，并证明了组合性原则：
$$
\text{compositionality} : \forall \{\Gamma \Delta\} \{C : \text{Ctx} \Gamma \Delta\} \{M N : \Gamma \vdash ★\} \rightarrow \mathcal{E}(M) \simeq \mathcal{E}(N) \implies \mathcal{E}(\text{plug} C M) \simeq \mathcal{E}(\text{plug} C N)
$$
这一证明通过对语境 $C$ 进行归纳，并应用之前建立的合同性性质，展示了在同一语境下的指称语义的可组合性。

### 指称语义作为函数来定义

#### 概念
- **指称语义的函数定义**：最终我们将指称语义定义为一个在输入项 $M$ 上递归的函数 $\llbracket M \rrbracket$。

#### 解释
函数 $\llbracket M \rrbracket$ 定义如下：
$$
\llbracket ` x \rrbracket \gamma v = v \sqsubseteq \gamma x
$$
$$
\llbracket \lambda N \rrbracket = \mathcal{F} \llbracket N \rrbracket
$$
$$
\llbracket L \cdot M \rrbracket = \llbracket L \rrbracket ● \llbracket M \rrbracket
$$

最后，我们通过归纳证明了 $\mathcal{E}(M) \simeq \llbracket M \rrbracket$，即新定义的函数语义与之前的指称语义等价。

### 总结
在本章中，我们逐步证明了指称语义的可组合性，通过构建语义方程、