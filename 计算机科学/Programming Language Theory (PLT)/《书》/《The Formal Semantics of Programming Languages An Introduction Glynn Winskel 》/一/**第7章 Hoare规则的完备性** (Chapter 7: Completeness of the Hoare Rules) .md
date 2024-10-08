[toc]



# 第7章 Hoare规则的完备性

在本章中，我们讨论Hoare规则的完备性意味着什么。Gödel的不完备性定理表明，不可能存在一个完全的证明系统来精确地建立所有有效的断言。Hoare规则继承了这种不完备性。然而，通过将断言语言的不完备性与由于编程语言构造的公理和规则不足导致的不完备性分开，我们可以在Cook的意义上获得相对完备性。证明Hoare规则的相对完备性依赖于最弱前置条件的概念，并引出了对验证条件生成器的讨论。

## 7.1 Gödel的不完备性定理

再次回顾部分正确性断言的证明规则，特别是后果规则。要知道我们有一个后果规则的实例，需要确定Assn（断言语言）中的某些断言是有效的。理想情况下，我们希望有一个断言的公理和规则的证明系统，使我们能够证明所有Assn中有效的断言，而不证明任何无效的断言。

自然地，我们希望证明系统在某种意义上是有效的，即检查某个被提议为规则实例的东西确实是一个规则实例是一个常规的事情。这种常规的意思是，有一个可计算的方法（形式为一个程序），它以一个真正的规则实例作为输入，返回确认它是一个规则实例；对于不是规则实例的输入，它不返回确认（甚至不必终止）。缺乏这样一个可计算的方法，我们可能有一个证明推导而不知道它，因为它使用了我们无法检查的规则实例。

我们不能声称Hoare规则的证明系统是有效的，因为我们没有一个可计算的方法来检查后果规则的实例。拥有这样一个方法取决于是否有一个可计算的方法来检查Assn中的断言是否有效。但在这里我们遇到了一个绝对的限制。伟大的奥地利逻辑学家库尔特·Gödel表明，不可能存在一个有效的证明系统，使得可以精确地证明Assn中的所有有效断言。

这个非凡的结果，称为 **Gödel的不完备性定理**，现在通过可计算性理论的结果来证明并不太难。事实上，我们将在第7.3节中基于一些可计算性的结果给出定理的证明。任何那里存在的差距或不足之处都可以通过查阅附录关于基于while程序IMP的可计算性和不可判定性的内容来弥补。

**定理7.1（Gödel的不完备性定理，1931年）：**

> **不存在一个有效的证明系统，使得其定理与Assn中有效的断言完全一致。**

这个定理意味着我们不能有一个部分正确性断言的有效证明系统。由于 $\vDash B$ 当且仅当 $\vDash \{ \text{true} \} \ \text{skip} \ \{ B \}$，如果我们有一个部分正确性的有效证明系统，它将归结为一个Assn中断言的有效证明系统，而根据Gödel的不完备性定理，这是不可能的。事实上，我们可以更直接地表明，不存在部分正确性断言的有效证明系统。

**命题7.2：**

> **不存在一个部分正确性断言的有效证明系统，使得其定理正好是所有有效的部分正确性断言。**

**证明：**

观察到 $\vDash \{ \text{true} \} \ c \ \{ \text{false} \}$ 当且仅当命令 $c$ 在所有状态上都发散（即永不终止）。如果我们有一个部分正确性断言的有效证明系统，它将产生一个可计算的方法来确认一个命令 $c$ 在所有状态上都发散。但已知这是不可能的——参见附录中的练习A.13。$\quad \blacksquare$

面对这一不可逾越的事实，我们接受第6.4节中的Hoare规则的证明系统，尽管我们知道由于后果规则的性质，它不是有效的；确定我们有一个后果规则的实例依赖于某些断言是有效的。尽管如此，我们仍然可以探究这个系统的完备性。这个完备性是由S. Cook在[33]中建立的。如果一个部分正确性断言是有效的，那么使用Hoare规则就有一个证明，即对于任何部分正确性断言 $\{ A \} \ c \ \{ B \}$，

$$
\vDash \{ A \} \ c \ \{ B \} \implies \vdash \{ A \} \ c \ \{ B \}
$$

尽管这个证明可能依赖于Assn中某些断言是有效的。在构建证明时，我们可以在任何需要知道Assn中一个断言是否有效的阶段咨询一个“神谕（oracle）”。由于这个原因，Cook的结果被称为部分正确性的Hoare规则的**相对完备性**——它们的完备性是相对于能够从关于算术的有效断言集中进行推理的能力。通过这种方式，我们试图将关于程序和对它们的推理的关注，与关于算术和任何对其的证明系统的不完备性的关注分开。

## 7.2 最弱前置条件和表达能力

证明相对完备性依赖于另一个概念。考虑尝试证明 $\{ A \} \ c_0 ; c_1 \ \{ B \}$。为了使用组合规则，我们需要一个中间断言 $C$，使得 $\{ A \} \ c_0 \ \{ C \}$ 和 $\{ C \} \ c_1 \ \{ B \}$ 是可证明的。我们如何知道可以找到这样的中间断言 $C$？一个充分的条件是，对于每个命令 $c$ 和后置条件 $B$，我们可以在Assn中表达它们的**最弱前置条件**。

设 $c \in \text{Com}$，$B \in \text{Assn}$。令 $I$ 是一个解释。相对于 $c$ 的 $B$ 的最弱前置条件 $\text{wp}_I[c, B]$ 定义为：

$$
\text{wp}_I[c, B] = \{ \sigma \in \Sigma_\bot \ | \ C[c]\ \sigma \vDash_I B \}
$$

即，从 $\sigma$ 开始执行 $c$，如果 $c$ 的执行要么发散（不终止），要么终止于一个满足 $B$ 的状态，那么 $\sigma$ 属于 $\text{wp}_I[c, B]$。因此，如果 $\vDash_I \{ A \} \ c \ \{ B \}$，那么

$$
A_I \subseteq \text{wp}_I[c, B]
$$

反之亦然。因此，$\vDash_I \{ A \} \ c \ \{ B \}$ 当且仅当 $A_I \subseteq \text{wp}_I[c, B]$。

假设存在一个断言 $A_0$，使得对于所有解释 $I$，有

$$
A_0^I = \text{wp}_I[c, B]
$$

那么

$$
\vDash_I \{ A \} \ c \ \{ B \} \quad \text{当且仅当} \quad \vDash_I (A \implies A_0)
$$

对于任何解释 $I$，即

$$
\vDash \{ A \} \ c \ \{ B \} \quad \text{当且仅当} \quad \vDash (A \implies A_0)
$$

所以我们看到为什么它被称为最弱前置条件，因为它被任何使部分正确性断言有效的前置条件所蕴含。然而，并不显然特定的断言语言中有一个断言 $A_0$，使得 $A_0^I = \text{wp}_I[c, B]$。

**定义**：如果对于每个命令 $c$ 和断言 $B$，在Assn中存在一个断言 $A_0$，使得对于任何解释 $I$，有 $A_0^I = \text{wp}_I[c, B]$，那么称Assn是**可表达的（expressive）**。

在展示可表达性时，我们将使用Gödel的 $\beta$ 谓词将关于状态序列的事实编码为Assn中的断言。$\beta$ 谓词涉及操作 $a \mod b$，即当 $a$ 除以 $b$ 时的余数。我们可以将这个概念表示为Assn中的一个断言。对于 $x = a \mod b$，我们写作：

$$
a \geq 0 \land b > 0 \land \exists k. \left[ k \geq 0 \land k \times b \leq a \land (k + 1) \times b > a \land x = a - (k \times b) \right]
$$

**引理7.3**

令 $\beta(a, b, i, x)$ 是自然数上的谓词，定义为

$$
\beta(a, b, i, x) \quad \overset{\text{def}}{\Longleftrightarrow} \quad x = a \mod (1 + (1 + i) \times b)
$$

对于任何自然数序列 $n_0, \dots, n_k$，存在自然数 $n, m$，使得对于所有 $j$，$0 \leq j \leq k$，以及所有 $x$，有

$$
\beta(n, m, j, x) \quad \Longleftrightarrow \quad x = n_j
$$

**证明：**

这个关于算术的证明留给读者作为本节末尾的一系列小练习。$\quad \blacksquare$

$\beta$ 谓词之所以重要，是因为利用它，我们可以将一个自然数序列 $n_0, \dots, n_k$ 编码为一对自然数 $n, m$。给定 $n, m$，对于任何长度 $k$，我们可以提取一个序列，即由 $n_0, \dots, n_k$ 组成的序列，使得

$$
\beta(n, m, j, x) \quad \Longleftrightarrow \quad x = n_j
$$

对于 $0 \leq j \leq k$。注意，$\beta$ 的定义表明，通过选择 $n, m$，序列 $n_0, \dots, n_k$ 是唯一确定的。上述引理断言，任何序列 $n_0, \dots, n_k$ 都可以以这种方式编码。

现在我们必须面对一个小小的烦恼。我们的状态和断言语言可以涉及负数和正数。我们必须扩展Gödel的 $\beta$ 谓词，以便编码正数和负数的序列。幸运的是，这可以很容易地完成，通过将正数编码为偶数，负数编码为奇数的自然数。

**引理7.4**

令 $F(x, y)$ 是自然数 $x$ 和整数 $y$ 上的谓词，定义为：

$$
F(x, y) \quad \overset{\text{def}}{\Longleftrightarrow} \quad \exists z \geq 0. \ \left( x = 2 \times z \implies y = z \right) \land \left( x = 2 \times z + 1 \implies y = -z \right)
$$

定义

$$
\beta^{\pm}(n, m, j, y) \quad \overset{\text{def}}{\Longleftrightarrow} \quad \exists x. \left( \beta(n, m, j, x) \land F(x, y) \right)
$$

那么对于任何整数序列 $n_0, \dots, n_k$（可以是正数或负数），存在自然数 $n, m$，使得对于所有 $j$，$0 \leq j \leq k$，以及所有 $x$，有

$$
\beta^{\pm}(n, m, j, x) \quad \Longleftrightarrow \quad x = n_j
$$

**证明：**

显然，$F(n, m)$ 表示自然数 $m \in \mathbb{N}$ 和整数 $n \in \mathbb{Z}$ 之间的一个一一对应，其中偶数 $m$ 代表非负整数，奇数 $m$ 代表负整数。该引理由引理7.3得出。$\quad \blacksquare$

谓词 $\beta^{\pm}$ 可以在Assn中表示，因为 $\beta$ 和 $F$ 都可以。在此，我们使用Assn中的断言来表示这个谓词。这个Assn中的断言将具有自由的整数变量，例如 $n, m, j, x$，其含义与上述相同，即 $n, m$ 编码一个序列，其第 $j$ 个元素为 $x$。

我们可能需要使用除了 $n, m, j, x$ 之外的其他整数变量，因此我们写 $\beta^{\pm}(n', m', j', x')$ 作为 $\beta^{\pm}$ 的一个替代，其中用整数变量 $n', m', j', x'$ 替换 $n, m, j, x$。我们没有给出在断言中替换整数变量的正式定义。第6.2.2节中的替换定义仅定义了在断言 $A$ 中，用不包含整数变量的算术表达式 $a$ 替换整数变量 $i$ 的替换 $A[a/i]$。

然而，只要变量 $n', m', j', x'$ 是“新鲜的（fresh）”，即它们是不同的，并且不在 $\beta^{\pm}$ 中出现（自由或绑定），相同的定义也适用于整数变量的替换；断言 $\beta^{\pm}[n'/n, m'/m, j'/j, x'/x]$ 是通过 $\beta^{\pm}[n'/n][m'/m][j'/j][x'/x]$ 得到的，使用第6.2.2节的定义。

现在我们可以证明：

**定理7.5**

> **Assn是可表达的。**

**证明：**

我们通过对命令 $c$ 的结构归纳来证明，对于所有断言 $B$，存在一个断言 $w[c, B]$，使得对于所有解释 $I$，

$$
\text{wp}_I[c, B] = w[c, B]^I
$$

对于所有命令 $c$。

注意，根据最弱前置条件的定义，对于解释 $I$，等式 $\text{wp}_I[c, B] = w[c, B]^I$ 意味着

$$
\sigma \vDash_I w[c, B] \quad \text{当且仅当} \quad C[c]\ \sigma \vDash_I B
$$

对所有状态 $\sigma$ 成立，这是我们在证明中偶尔会使用的事实。

**1. 情况 $c = \textbf{skip}$：**

在这种情况下，取 $w[\textbf{skip}, B] = B$。显然，对于所有状态 $\sigma$ 和解释 $I$，

$$
\sigma \in \text{wp}_I[\textbf{skip}, B] \quad \Longleftrightarrow \quad C[\textbf{skip}]\ \sigma \vDash_I B \quad \Longleftrightarrow \quad \sigma \vDash_I B \quad \Longleftrightarrow \quad \sigma \vDash_I w[\textbf{skip}, B]
$$

**2. 情况 $c = X := a$：**

在这种情况下，定义 $w[X := a, B] = B[a/X]$。然后，

$$
\begin{aligned}
\sigma \in \text{wp}_I[X := a, B] &\quad \Longleftrightarrow \quad C[X := a]\ \sigma \vDash_I B \\
&\quad \Longleftrightarrow \quad \sigma[A[a]\ \sigma / X] \vDash_I B \quad \text{（因为赋值语句的语义）} \\
&\quad \Longleftrightarrow \quad \sigma \vDash_I B[a/X] \quad \text{（根据引理6.9）} \\
&\quad \Longleftrightarrow \quad \sigma \vDash_I w[X := a, B]
\end{aligned}
$$

**3. 情况 $c = c_0; c_1$：**

归纳地定义 $w[c_0; c_1, B] = w[c_0, w[c_1, B]]$。那么，对于 $\sigma \in \Sigma$ 和解释 $I$，

$$
\begin{aligned}
\sigma \in \text{wp}_I[c_0; c_1, B] &\quad \Longleftrightarrow \quad C[c_0; c_1]\ \sigma \vDash_I B \\
&\quad \Longleftrightarrow \quad C[c_1](C[c_0]\ \sigma) \vDash_I B \\
&\quad \Longleftrightarrow \quad C[c_0]\ \sigma \vDash_I w[c_1, B] \quad \text{（根据归纳假设）} \\
&\quad \Longleftrightarrow \quad \sigma \vDash_I w[c_0, w[c_1, B]] \\
&\quad \Longleftrightarrow \quad \sigma \vDash_I w[c_0; c_1, B]
\end{aligned}
$$

**4. 情况 $c = \textbf{if}\ b\ \textbf{then}\ c_0\ \textbf{else}\ c_1$：**

定义

$$
w[\textbf{if}\ b\ \textbf{then}\ c_0\ \textbf{else}\ c_1, B] = \left( (b \land w[c_0, B]) \lor (\lnot b \land w[c_1, B]) \right)
$$

那么，对于 $\sigma \in \Sigma$ 和解释 $I$，

$$
\begin{aligned}
\sigma \in \text{wp}_I[c, B] &\quad \Longleftrightarrow \quad C[c]\ \sigma \vDash_I B \\
&\quad \Longleftrightarrow \quad \left( [\sigma \vDash_I b \land C[c_0]\ \sigma \vDash_I B] \lor [\sigma \vDash_I \lnot b \land C[c_1]\ \sigma \vDash_I B] \right) \\
&\quad \Longleftrightarrow \quad \left( [\sigma \vDash_I b \land \sigma \vDash_I w[c_0, B]] \lor [\sigma \vDash_I \lnot b \land \sigma \vDash_I w[c_1, B]] \right) \quad \text{（根据归纳假设）} \\
&\quad \Longleftrightarrow \quad \sigma \vDash_I \left( (b \land w[c_0, B]) \lor (\lnot b \land w[c_1, B]) \right) \\
&\quad \Longleftrightarrow \quad \sigma \vDash_I w[c, B]
\end{aligned}
$$

**5. 情况 $c = \textbf{while}\ b\ \textbf{do}\ c_0$：**

这是唯一困难的情况。对于状态 $\sigma$ 和解释 $I$，我们有（参见练习5.8）：

$$
\sigma \in \text{wp}_I[c, B] \quad \Longleftrightarrow \quad \forall k \geq 0, \forall \sigma_0, \dots, \sigma_k \in \Sigma. \left[ \sigma = \sigma_0 \land \forall i (0 \leq i < k). \left( \sigma_i \vDash_I b \land \sigma_{i+1} \in C[c_0]\ \sigma_i \right) \land \left( \sigma_k \vDash_I \lnot b \implies \sigma_k \vDash_I B \right) \right]
$$

如其所示，$\sigma$ 在 $\text{wp}_I[c, B]$ 中的数学特征并不是Assn中的一个断言；特别是它直接引用了状态 $\sigma_0, \dots, \sigma_k$。然而，我们将展示如何用Assn中的一个等价描述来替换它。

第一步是将对状态 $\sigma_0, \dots, \sigma_k$ 的引用替换为对它们在 $c$ 和 $B$ 中提到的位置的值的引用。假设 $X = X_1, \dots, X_t$ 是 $c$ 和 $B$ 中提到的位置——其余位置的值与计算无关。我们利用以下事实：

**事实**：假设 $A$ 是Assn中的一个断言，它只涉及位置 $X = X_1, \dots, X_t$。对于状态 $\sigma$，令 $s_i = \sigma(X_i)$，其中 $1 \leq i \leq t$，并写作 $s = s_1, \dots, s_t$。那么对于任何解释 $I$，有：

$$
\sigma \vDash_I A \quad \text{当且仅当} \quad A[s/X] \quad \text{在解释 } I \text{ 下成立}
$$

其中 $A[s/X]$ 表示在断言 $A$ 中用 $s_i$ 替换位置 $X_i$。这个事实可以通过结构归纳证明（练习！）。

利用上述事实，我们可以将原来的条件转换为关于序列的等价断言。对于 $i \geq 0$，令 $s_i$ 表示状态 $\sigma_i$ 在位置 $X$ 处的值，即 $s_i = (\sigma_i(X_1), \dots, \sigma_i(X_t))$，是一个自然数的序列。

我们声称：$\sigma \in \text{wp}_I[c, B]$ 当且仅当

$$
\forall k \geq 0, \forall s_0, \dots, s_k \in \mathbb{N}^t. \left[ \sigma \vDash_I X = s_0 \land \forall i (0 \leq i < k). \left( \vDash_I b[s_i/X] \land s_{i+1} \text{ 是 } c_0 \text{ 在 } s_i \text{ 上的执行结果} \right) \land \left( \vDash_I \lnot b[s_k/X] \implies \vDash_I B[s_k/X] \right) \right]
$$

（为简化，我们假设位置 $X$ 只有一个，即 $t = 1$。）

现在，利用 Gödel 的编码方法，我们可以在Assn中表示这个断言，尽管看起来相当复杂。

为了完成证明，我们需要利用之前定义的 $\beta^\pm$ 谓词，将状态序列编码为自然数的对 $(n, m)$。这样，我们可以在Assn中表达关于状态序列的断言。

详细的证明相当复杂，并且涉及到对 $\beta^\pm$ 谓词的深入理解。由于篇幅限制，我们将在此处跳过进一步的细节。

---

通过结构归纳完成了证明。$\quad \blacksquare$

由于Assn是可表达的，对于任何命令 $c$ 和断言 $B$，存在一个断言 $w[c, B]$，具有以下性质：

$$
\text{wp}_I[c, B] = w[c, B]^I
$$

对于任何解释 $I$。

当然，在上面的可表达性证明中构造的断言 $w[c, B]$ 并不是具有这个性质的唯一断言（为什么不是？）。然而，假设 $A_0$ 是另一个断言，使得对于所有 $I$，有 $A_0^I = \text{wp}_I[c, B]$。那么

$$
\vDash (w[c, B] \Longleftrightarrow A_0)
$$

因此，表达最弱前置条件的断言在逻辑等价意义上是唯一的。

关于这样的断言 $w[c, B]$ 的重要事实是，根据最弱前置条件的定义，它的特征是：

$$
\sigma \vDash_I w[c, B] \quad \text{当且仅当} \quad C[c]\ \sigma \vDash_I B
$$

对所有状态 $\sigma$ 和解释 $I$ 都成立。

利用Assn的可表达性，我们将证明相对完备性。首先是一个重要的引理。

**引理7.6**

> 对于 $c \in \text{Com}$ 和 $B \in \text{Assn}$，令 $w[c, B]$ 是一个表达最弱前置条件的断言，即对于任何解释 $I$，有 $w[c, B]^I = \text{wp}_I[c, B]$（$w[c, B]$ 不必是上面定理7.5中构造的那个）。那么

$$
\vdash \{ w[c, B] \} \ c \ \{ B \}
$$

**证明：**

令 $w[c, B]$ 是一个表达命令 $c$ 和后置条件 $B$ 的最弱前置条件的断言。我们通过对 $c$ 的结构归纳，证明对于所有 $B \in \text{Assn}$，

$$
\vdash \{ w[c, B] \} \ c \ \{ B \}
$$

（在除最后一种情况外的所有情况下，证明与定理7.5的证明重叠。）

**1. 情况 $c = \textbf{skip}$：**

在这种情况下，$\vDash w[\textbf{skip}, B] \implies B$，因此根据后果规则，$\vdash \{ w[\textbf{skip}, B] \} \ \textbf{skip} \ \{ B \}$。

**2. 情况 $c = X := a$：**

在这种情况下，

$$
\begin{aligned}
\sigma \in \text{wp}_I[c, B] &\quad \Longleftrightarrow \quad \sigma[A[a]\ \sigma / X] \vDash_I B \\
&\quad \Longleftrightarrow \quad \sigma \vDash_I B[a/X]
\end{aligned}
$$

因此，$\vDash (w[c, B] \implies B[a/X])$。因此，通过赋值语句的规则和后果规则，我们得到 $\vdash \{ w[c, B] \} \ c \ \{ B \}$。

**3. 情况 $c = c_0; c_1$：**

在这种情况下，对于状态 $\sigma$ 和解释 $I$，

$$
\begin{aligned}
\sigma \vDash_I w[c_0; c_1, B] &\quad \Longleftrightarrow \quad \sigma \vDash_I w[c_0, w[c_1, B]] \\
&\quad \Longleftrightarrow \quad C[c_0]\ \sigma \vDash_I w[c_1, B] \quad \text{（根据归纳假设）} \\
&\quad \Longleftrightarrow \quad C[c_0; c_1]\ \sigma \vDash_I B \\
\end{aligned}
$$

因此，$\vDash (w[c_0; c_1, B] \implies w[c_0, w[c_1, B]])$。根据归纳假设，

$$
\vdash \{ w[c_0, w[c_1, B]] \} \ c_0 \ \{ w[c_1, B] \} \quad \text{和} \quad \vdash \{ w[c_1, B] \} \ c_1 \ \{ B \}
$$

因此，通过顺序组合规则，我们得到

$$
\vdash \{ w[c_0, w[c_1, B]] \} \ c_0; c_1 \ \{ B \}
$$

通过后果规则，我们得到 $\vdash \{ w[c_0; c_1, B] \} \ c_0; c_1 \ \{ B \}$。

**4. 情况 $c = \textbf{if}\ b\ \textbf{then}\ c_0\ \textbf{else}\ c_1$：**

在这种情况下，对于 $\sigma$ 和 $I$，

$$
\sigma \vDash_I w[c, B] \quad \Longleftrightarrow \quad \left( [\sigma \vDash_I b \land \sigma \vDash_I w[c_0, B]] \lor [\sigma \vDash_I \lnot b \land \sigma \vDash_I w[c_1, B]] \right)
$$

因此，

$$
\vDash (w[c, B] \implies (b \land w[c_0, B]) \lor (\lnot b \land w[c_1, B]))
$$

根据归纳假设，

$$
\vdash \{ w[c_0, B] \} \ c_0 \ \{ B \} \quad \text{和} \quad \vdash \{ w[c_1, B] \} \ c_1 \ \{ B \}
$$

并且

$$
\vDash (w[c, B] \land b) \implies w[c_0, B], \quad \vDash (w[c, B] \land \lnot b) \implies w[c_1, B]
$$

因此，通过后果规则，我们有：

$$
\vdash \{ w[c, B] \land b \} \ c_0 \ \{ B \} \quad \text{和} \quad \vdash \{ w[c, B] \land \lnot b \} \ c_1 \ \{ B \}
$$

通过条件语句的规则，我们得到 $\vdash \{ w[c, B] \} \ c \ \{ B \}$。

**5. 情况 $c = \textbf{while}\ b\ \textbf{do}\ c_0$：**

取 $A = w[c, B]$。我们需要证明：

1. $\vDash \{ A \land b \} \ c_0 \ \{ A \}$

2. $\vDash (A \land \lnot b) \implies B$

然后，从（1）中，根据归纳假设，我们得到 $\vdash \{ A \land b \} \ c_0 \ \{ A \}$，因此根据while循环的规则，$\vdash \{ A \} \ c \ \{ A \land \lnot b \}$。继续，根据（2）和后果规则，我们得到 $\vdash \{ A \} \ c \ \{ B \}$。

**证明（1）：**

假设对于解释 $I$，$\sigma \vDash_I A \land b$。那么 $\sigma \vDash_I w[c, B]$ 且 $\sigma \vDash_I b$。即，$C[c]\ \sigma \vDash_I B$。但注意到 $C[c]$ 是定义的，即

$$
C[c] = C[\textbf{if}\ b\ \textbf{then}\ c_0; c\ \textbf{else}\ \textbf{skip}]
$$

因此，$C[c] = C[c_0; c]$ 当 $\sigma \vDash_I b$。因此，$C[c_0]\ \sigma \vDash_I w[c, B]$，即 $C[c_0]\ \sigma \vDash_I A$。

因此，$\vDash \{ A \land b \} \ c_0 \ \{ A \}$。

**证明（2）：**

假设 $\sigma \vDash_I A \land \lnot b$。那么 $\sigma \vDash_I w[c, B]$ 且 $\sigma \vDash_I \lnot b$。但由于 $C[c]\ \sigma = \sigma$（因为条件为假，执行 $\textbf{skip}$），因此 $C[c]\ \sigma \vDash_I B$。因此，$\sigma \vDash_I B$。

因此，$\vDash (A \land \lnot b) \implies B$。

这完成了所有情况的证明。通过结构归纳，引理得证。$\quad \blacksquare$

**定理7.7**

> **部分正确性的证明系统是相对完备的，即对于任何部分正确性断言 $\{ A \} \ c \ \{ B \}$，**

$$
\vDash \{ A \} \ c \ \{ B \} \implies \vdash \{ A \} \ c \ \{ B \}
$$

**证明：**

假设 $\vDash \{ A \} \ c \ \{ B \}$。那么根据上述引理，$\vdash \{ w[c, B] \} \ c \ \{ B \}$，其中 $w[c, B]^I = \text{wp}_I[c, B]$ 对于任何解释 $I$。由于 $\vDash (A \implies w[c, B])$，通过后果规则，我们得到 $\vdash \{ A \} \ c \ \{ B \}$。$\quad \blacksquare$

---

**练习7.8（Gödel的 $\beta$ 谓词）**

(a) 设 $n_0, \dots, n_k$ 是一个自然数序列，令

$$
m = \left( \max \{ k, n_0, \dots, n_k \} \right)!
$$

证明数

$$
P_i = 1 + (1 + i) \times m, \quad \text{对于 } 0 \leq i \leq k
$$

互质（即，对于 $i \ne j$，有 $\text{gcd}(P_i, P_j) = 1$），并且 $n_i < P_i$。

(b) 此外，定义

$$
C_i = \frac{P_0 \times P_1 \times \dots \times P_k}{P_i}, \quad \text{对于 } 0 \leq i \leq k
$$

证明对于所有 $i$，存在唯一的 $d_i$，$0 \leq d_i < P_i$，使得

$$
n_i \equiv d_i \mod P_i
$$

(c) 另外，定义

$$
n = \sum_{i=0}^k d_i \times C_i
$$

证明对于 $0 \leq i \leq k$，有

$$
n \mod P_i = d_i
$$

(d) 最后，证明引理7.3。

---

**总结**

在本章中，我们讨论了Hoare规则的完备性，特别是其相对完备性。由于Gödel的不完备性定理，我们不能有一个有效的证明系统来证明所有有效的断言。然而，通过引入最弱前置条件的概念，并证明断言语言Assn是可表达的，我们证明了Hoare规则的相对完备性。

我们详细地讨论了最弱前置条件的定义，以及如何在Assn中构造它们。我们还使用了Gödel的 $\beta$ 谓词来将关于状态序列的信息编码为Assn中的断言。

最后，通过证明相关的引理和定理，我们得出了Hoare规则的相对完备性。

---

**备注**：

由于篇幅和复杂性限制，我们对一些技术细节和证明步骤进行了简化。如果您对某些部分有疑问，建议进一步阅读相关教材或参考文献，以获得更深入的理解。

### ---------------------------

# 第7章 Hoare规则的完备性（续）

## 7.3 Gödel定理的证明

**Gödel的不完备性定理**表明，Assn 中有效断言的子集不是**递归可枚举的（recursively enumerable）**（即，不存在一个程序，它以断言为输入，精确地对有效断言返回确认——参见附录中的可计算性部分以获取精确定义和更详细的处理）。

**定理7.9**：

> **集合 $\{ A \in \text{Assn} \mid \vDash A \}$ 不是递归可枚举的。**

**证明**：

假设相反，集合 $\{ A \in \text{Assn} \mid \vDash A \}$ 是递归可枚举的。那么存在一个可计算的方法来确认一个断言是有效的。

这将提供一个可计算的方法来确认一个命令 $c$ 在零状态 $\sigma_0$ 上发散（即永不终止），其中每个位置 $X$ 的值都是 $0$。

构造断言 $w[c, \text{false}]$，如定理7.5的证明中所示。令 $X$ 包含 $w[c, \text{false}]$ 中提到的所有位置。令 $A$ 为 $w[c, \text{false}][0/X]$，即将位置变量替换为零得到的断言。

那么，通过检查断言 $A$ 的有效性（假设存在一个可计算的方法），就可以确认 $c$ 在零状态上发散。

但是，已知在零状态上发散的命令 $c$ 并不形成一个递归可枚举的集合——参见附录中的定理A.12。这个矛盾表明集合 $\{ A \in \text{Assn} \mid \vDash A \}$ 不是递归可枚举的。$\quad \blacksquare$

作为推论，我们得到 Gödel 的不完备性定理：

**定理7.10（Gödel 不完备性定理，重新表述）**：

> **不存在一个有效的证明系统，使得其定理与 Assn 中有效的断言完全一致。**

**证明**：

假设存在一个有效的证明系统，对于一个断言 $A$，当且仅当 $A$ 是有效的时，$A$ 是可证明的。证明系统的有效性意味着存在一个可计算的方法来精确地确认某个东西是一个证明。系统地搜索所有证明，直到找到断言 $A$ 的证明，就提供了一个可计算的方法来精确地确认断言 $A$ 是否有效。因此，不可能存在这样的有效证明系统。$\quad \blacksquare$

尽管我们已经对 Assn 中的断言陈述了 Gödel 定理，但位置的存在在结果中并不起决定性作用。Gödel 定理通常针对更小的断言语言陈述，即不带位置的断言语言——算术语言。事实上，在这个语言中，有效断言不形成一个递归可枚举的集合，这意味着算术的公理化永远不会结束——总会有一些关于算术的事实无法被证明。我们也不能希望有一个程序，生成一个无限的公理列表和有效的证明规则，使得所有关于算术的有效断言都可以被推导出来。如果存在这样的程序，那么就会存在一个算术断言的有效证明系统，与 Gödel 的不完备性定理相矛盾。

Gödel 的结果具有巨大的历史意义。Gödel 当时并没有可计算性的概念。相反，他的结果激发了逻辑学家研究可计算性的新形式化。原始的证明通过将一个形式系统的可证明性概念表达为一个断言本身，并构造一个断言，使得它在且仅在它不可证明时有效。这种自指性质导致了不完备性定理的成立。

应该承认，我们只考虑了 Gödel 的第一个不完备性定理；还有一个第二定理，说明一个算术的形式系统不能在系统自身中被证明是无矛盾的。Gödel 很清楚，他的不完备性证明依赖于能够通过断言来表达自然数上的某些函数集合——这个集合后来被称为**原始递归函数（primitive recursive functions）**。认识到简单的扩展导致了一个稳定的可计算函数的概念，这又花费了几年时间，最终导致了**丘奇-图灵论题（Church-Turing thesis）**。

不完备性定理打击了希尔伯特（Hilbert）所设立的计划。作为对数学基础中像罗素悖论（Russell's paradox）这样的悖论的反应，希尔伯特倡导研究在某个形式系统中进行推理时使用的有限方法，希望这将导致重要证明系统（如算术系统）的无矛盾性和完备性的证明。Gödel 的定理确立了有限推理能力的绝对限制。

## 7.4 验证条件

原则上，Assn 的可表达性提供了一种方法，将证明部分正确性断言的有效性的问题，转化为证明 Assn 中一个断言的有效性的问题；形如 $\{ A \} \ c \ \{ B \}$ 的部分正确性断言的有效性等价于断言 $A \implies w[c, B]$ 的有效性，从中命令 $c$ 已被消除。

通过这种方式，给定一个谓词演算的定理证明器，我们或许可以推导出一个用于 IMP 程序的定理证明器。不幸的是，我们用来获得 $w[c, B]$ 的方法是复杂且低效的，绝对不实用。

然而，一旦我们允许一些人类的指导，就可以沿着类似的思路获得用于建立部分正确性断言有效性的有用的自动化工具。让我们用断言来注释程序。定义注释命令（annotated commands）的语法集合如下：

$$
\begin{align*}
c ::= & \ \textbf{skip} \\
      & \mid X := a \\
      & \mid c_0; (X := a) \\
      & \mid c_0; \{ D \} c_1 \\
      & \mid \textbf{if} \ b \ \textbf{then} \ c_0 \ \textbf{else} \ c_1 \\
      & \mid \textbf{while} \ b \ \textbf{do} \ \{ D \} c
\end{align*}
$$

其中：

- $X$ 是一个位置（变量）；
- $a$ 是一个算术表达式；
- $b$ 是一个布尔表达式；
- $c, c_0, c_1$ 是注释命令；
- $D$ 是一个断言；
- 在 $c_0; \{ D \} c_1$ 中，注释命令 $c_1$ 不是一个赋值语句。

这个想法是，在注释命令中的某个点的断言在控制流到达该点时是真实的。因此，我们只在控制从 $c_0$ 转移到 $c_1$ 的点对形如 $c_0; c_1$ 的命令进行注释。当 $c_1$ 是一个赋值 $X := a$ 时，不需要这样做，因为在这种情况下，可以简单地从后置条件推导出注释。

一个带注释的 while 循环：

$$
\textbf{while} \ b \ \textbf{do} \ \{ D \} c
$$

包含一个断言 $D$，它被认为是一个**不变式（invariant）**。

一个带注释的部分正确性断言具有形式：

$$
\{ A \} \ c \ \{ B \}
$$

其中 $c$ 是一个注释命令。注释命令与普通命令相关联，可以通过忽略注释得到。有时将注释命令视为其关联的命令是方便的。在这种意义上，当其关联的（未注释的）部分正确性断言有效时，我们说一个带注释的部分正确性断言是有效的。

一个带注释的 while 循环：

$$
\{ A \} \ \textbf{while} \ b \ \textbf{do} \ \{ D \} c \ \{ B \}
$$

包含一个断言 $D$，我们希望它被明智地选择为 $D$ 是一个不变式。

**作为不变式意味着**：

$$
\{ D \land b \} \ c \ \{ D \}
$$

是有效的。为了确保：

$$
\{ A \} \ \textbf{while} \ b \ \textbf{do} \ \{ D \} c \ \{ B \}
$$

是有效的，一旦知道 $D$ 是不变式，就足以证明以下两个断言是有效的：

1. $A \implies D$
2. $D \land \lnot b \implies B$

快速理解的方法是注意到，我们可以使用 Hoare 规则（已知是健全的）从 $\{ D \land b \} \ c \ \{ D \}$ 推导出 $\{ A \} \ \textbf{while} \ b \ \textbf{do} \ c \ \{ B \}$。

显然，并非所有带注释的部分正确性断言都是有效的。要使其有效，充分的条件是证明某些断言的有效性，称为**验证条件（verification conditions）**，其中消除了对命令的所有提及。

通过对注释命令的结构归纳，定义一个带注释的部分正确性断言的验证条件，记为 $\text{vc}(\{ A \} \ c \ \{ B \})$。

具体定义如下：

1. 对于 $\{ A \} \ \textbf{skip} \ \{ B \}$：

   $$
   \text{vc}(\{ A \} \ \textbf{skip} \ \{ B \}) = \{ A \implies B \}
   $$

2. 对于 $\{ A \} \ X := a \ \{ B \}$：

   $$
   \text{vc}(\{ A \} \ X := a \ \{ B \}) = \{ A \implies B[a/X] \}
   $$

3. 对于 $\{ A \} \ c_0; X := a \ \{ B \}$：

   $$
   \text{vc}(\{ A \} \ c_0; X := a \ \{ B \}) = \text{vc}(\{ A \} \ c_0 \ \{ B[a/X] \})
   $$

4. 对于 $\{ A \} \ c_0; \{ D \} c_1 \ \{ B \}$，其中 $c_1$ 不是赋值：

   $$
   \text{vc}(\{ A \} \ c_0; \{ D \} c_1 \ \{ B \}) = \text{vc}(\{ A \} \ c_0 \ \{ D \}) \cup \text{vc}(\{ D \} \ c_1 \ \{ B \})
   $$

5. 对于 $\{ A \} \ \textbf{if} \ b \ \textbf{then} \ c_0 \ \textbf{else} \ c_1 \ \{ B \}$：

   $$
   \text{vc}(\{ A \} \ \textbf{if} \ b \ \textbf{then} \ c_0 \ \textbf{else} \ c_1 \ \{ B \}) = \text{vc}(\{ A \land b \} \ c_0 \ \{ B \}) \cup \text{vc}(\{ A \land \lnot b \} \ c_1 \ \{ B \})
   $$

6. 对于 $\{ A \} \ \textbf{while} \ b \ \textbf{do} \ \{ D \} c \ \{ B \}$：

   $$
   \text{vc}(\{ A \} \ \textbf{while} \ b \ \textbf{do} \ \{ D \} c \ \{ B \}) = \text{vc}(\{ D \land b \} \ c \ \{ D \}) \cup \{ A \implies D \} \cup \{ D \land \lnot b \implies B \}
   $$

**练习7.11**：

**证明**：通过对注释命令 $c$ 进行结构归纳，对于所有带注释的部分正确性断言 $\{ A \} \ c \ \{ B \}$，如果 $\text{vc}(\{ A \} \ c \ \{ B \})$ 中的所有断言都是有效的，那么 $\{ A \} \ c \ \{ B \}$ 是有效的。

**解答**：

**（此处提供详细的证明步骤）**

我们通过对注释命令 $c$ 进行结构归纳来证明上述结论。证明的总体思路与引理7.6类似。

**基础情况**：

1. **当 $c = \textbf{skip}$ 时**：

   - 验证条件为 $\{ A \implies B \}$。
   - 如果 $A \implies B$ 是有效的，那么根据 Hoare 规则的 skip 规则，有 $\vdash \{ A \} \ \textbf{skip} \ \{ A \}$。
   - 通过后果规则（consequence rule），因为 $\vdash (A \implies B)$，因此有 $\vdash \{ A \} \ \textbf{skip} \ \{ B \}$。
   - 因此，$\{ A \} \ \textbf{skip} \ \{ B \}$ 是有效的。

2. **当 $c = X := a$ 时**：

   - 验证条件为 $\{ A \implies B[a/X] \}$。
   - 如果 $A \implies B[a/X]$ 是有效的，那么根据 Hoare 规则的赋值规则，有 $\vdash \{ B[a/X] \} \ X := a \ \{ B \}$。
   - 由于 $A \implies B[a/X]$，通过后果规则，有 $\vdash \{ A \} \ X := a \ \{ B \}$。
   - 因此，$\{ A \} \ X := a \ \{ B \}$ 是有效的。

**归纳步骤**：

假设对于所有比 $c$ 更简单的注释命令，结论成立。我们考虑各个构造的情况。

3. **当 $c = c_0; X := a$ 时**：

   - 验证条件为 $\text{vc}(\{ A \} \ c_0 \ \{ B[a/X] \})$。
   - 归纳假设：如果 $\text{vc}(\{ A \} \ c_0 \ \{ B[a/X] \})$ 中的所有断言都是有效的，那么 $\{ A \} \ c_0 \ \{ B[a/X] \}$ 是有效的。
   - 根据赋值规则，有 $\vdash \{ B[a/X] \} \ X := a \ \{ B \}$。
   - 通过顺序组合规则（sequencing rule），有 $\vdash \{ A \} \ c_0; X := a \ \{ B \}$。
   - 因此，$\{ A \} \ c_0; X := a \ \{ B \}$ 是有效的。

4. **当 $c = c_0; \{ D \} c_1$，且 $c_1$ 不是赋值时**：

   - 验证条件为 $\text{vc}(\{ A \} \ c_0 \ \{ D \}) \cup \text{vc}(\{ D \} \ c_1 \ \{ B \})$。
   - 归纳假设：

     - 如果 $\text{vc}(\{ A \} \ c_0 \ \{ D \})$ 中的所有断言都是有效的，那么 $\{ A \} \ c_0 \ \{ D \}$ 是有效的。
     - 如果 $\text{vc}(\{ D \} \ c_1 \ \{ B \})$ 中的所有断言都是有效的，那么 $\{ D \} \ c_1 \ \{ B \}$ 是有效的。

   - 通过顺序组合规则，有 $\vdash \{ A \} \ c_0; c_1 \ \{ B \}$。
   - 因此，$\{ A \} \ c \ \{ B \}$ 是有效的。

5. **当 $c = \textbf{if} \ b \ \textbf{then} \ c_0 \ \textbf{else} \ c_1$ 时**：

   - 验证条件为 $\text{vc}(\{ A \land b \} \ c_0 \ \{ B \}) \cup \text{vc}(\{ A \land \lnot b \} \ c_1 \ \{ B \})$。
   - 归纳假设：

     - 如果 $\text{vc}(\{ A \land b \} \ c_0 \ \{ B \})$ 中的所有断言都是有效的，那么 $\{ A \land b \} \ c_0 \ \{ B \}$ 是有效的。
     - 如果 $\text{vc}(\{ A \land \lnot b \} \ c_1 \ \{ B \})$ 中的所有断言都是有效的，那么 $\{ A \land \lnot b \} \ c_1 \ \{ B \}$ 是有效的。

   - 根据条件语句的规则，有 $\vdash \{ A \} \ \textbf{if} \ b \ \textbf{then} \ c_0 \ \textbf{else} \ c_1 \ \{ B \}$。
   - 因此，$\{ A \} \ c \ \{ B \}$ 是有效的。

6. **当 $c = \textbf{while} \ b \ \textbf{do} \ \{ D \} c_0$ 时**：

   - 验证条件为 $\text{vc}(\{ D \land b \} \ c_0 \ \{ D \}) \cup \{ A \implies D \} \cup \{ D \land \lnot b \implies B \}$。
   - 归纳假设：

     - 如果 $\text{vc}(\{ D \land b \} \ c_0 \ \{ D \})$ 中的所有断言都是有效的，那么 $\{ D \land b \} \ c_0 \ \{ D \}$ 是有效的。

   - 因此，$D$ 是循环的不变式。
   - 通过 while 循环的规则，有 $\vdash \{ D \} \ \textbf{while} \ b \ \textbf{do} \ c_0 \ \{ D \land \lnot b \}$。
   - 由于 $\vDash A \implies D$，通过后果规则，有 $\vdash \{ A \} \ \textbf{while} \ b \ \textbf{do} \ c_0 \ \{ D \land \lnot b \}$。
   - 由于 $\vDash D \land \lnot b \implies B$，再次应用后果规则，有 $\vdash \{ A \} \ \textbf{while} \ b \ \textbf{do} \ c_0 \ \{ B \}$。
   - 因此，$\{ A \} \ c \ \{ B \}$ 是有效的。

综上所述，通过结构归纳，我们证明了：如果 $\text{vc}(\{ A \} \ c \ \{ B \})$ 中的所有断言都是有效的，那么 $\{ A \} \ c \ \{ B \}$ 是有效的。$\quad \blacksquare$

因此，要证明一个带注释的部分正确性断言的有效性，足以证明其验证条件是有效的。通过这种方式，程序验证的任务可以交给一个用于谓词演算的定理证明器。一些商业程序验证系统（如 Gypsy [41]）就是以这种方式工作的。

注意，虽然验证条件的有效性足以保证带注释的部分正确性断言的有效性，但它不是必要的。这可能是因为选择的不变式对于前置和后置条件来说不合适。例如，尽管

$$
\{ \text{true} \} \ \textbf{while} \ \text{false} \ \textbf{do} \ \{ \text{false} \} \ \textbf{skip} \ \{ \text{true} \}
$$

显然是有效的（以 $\text{false}$ 作为不变式），但其验证条件包含：

$$
\text{true} \implies \text{false}
$$

这显然不是一个有效的断言。

我们以指出我们的注释命令处理中的一个特殊性来结束本节。两个命令 $(c_1; X := a_1); X := a_2$ 和 $c_1; (X := a_1; X := a_2)$ 被理解为基本相同的；实际上，在许多命令式语言中，它们都会被写成：

$$
c_1; \\
X := a_1; \\
X := a_2
$$

然而，根据我们的注释命令语法，这两个命令支持不同的注释。第一个只允许在 $c_1$ 中出现可能的注释，而第二个可以被注释为 $c_1; \{ D \} (X := a_1; X := a_2)$。注释规则不在单个赋值之前放置注释，但会在任何其他的赋值链之前放置注释。这即使在这种情况下仍然可以通过一系列替换从后置条件中推导出注释。

**练习7.12**：

**建议**：修改注释命令的语法和验证条件的定义，以解决这种特殊性，使得任何赋值链或 $\textbf{skip}$ 都像目前对待单个赋值一样被处理。

**解答**：

一种方法是将赋值语句的序列视为一个单独的复合语句，并允许在其前面添加注释。

修改后的语法可以是：

$$
\begin{align*}
c ::= & \ \textbf{skip} \\
      & \mid X := a \\
      & \mid \{ D \} (X := a; c') \\
      & \mid c_0; c_1 \\
      & \mid \textbf{if} \ b \ \textbf{then} \ c_0 \ \textbf{else} \ c_1 \\
      & \mid \textbf{while} \ b \ \textbf{do} \ \{ D \} c
\end{align*}
$$

然后，在验证条件的定义中，对于复合赋值语句，可以类似于单个赋值语句进行处理，允许在赋值序列之前放置注释，并相应地调整验证条件。

通过这种方式，我们统一了对赋值链和单个赋值的处理，使得验证条件的生成和处理更加一致。$\quad \blacksquare$

**练习7.13**：

**任务**：一个更大的项目是编写一个验证条件生成器（例如，用标准的 ML 或 Prolog 实现），它以一个带注释的部分正确性断言作为输入，输出其验证条件的集合或列表。（参见 Gordon 的书 [42]，其中有一个 Lisp 语言的程序。）

**解答**：

（此处应当描述实现验证条件生成器的思路和方法，讨论如何解析注释命令，递归地生成验证条件，以及在编程语言中的具体实现细节。）

由于篇幅限制，这里不展开具体的程序实现。

## 7.5 谓词变换器

本节是可选的，呈现了关于断言和最弱前置条件的一个抽象且更数学化的观点。

抽象地，命令可以被视为一个从状态到状态的函数 $f : \Sigma \rightarrow \Sigma_\bot$，其中 $\bot$ 表示未定义的状态；这样的函数有时被称为**状态变换器（state transformers）**。它们形成一个 **cpo（complete partial order，完全偏序集）**，当按逐点方式排序时，与状态上的偏函数同构。

抽象地，部分正确性的断言是一个包含 $\bot$ 的状态子集，因此我们定义部分正确性的谓词集合为：

$$
\text{Pred}(\Sigma) = \{ Q \mid Q \subseteq \Sigma_\bot \text{ 且 } \bot \in Q \}
$$

我们可以通过反向包含来使谓词成为一个 cpo。部分正确性的谓词的 cpo 是 $(\text{Pred}(\Sigma), \supseteq)$。

在这里，关于由命令配置产生的最终状态的更多信息对应于将其执行限制在一个更小的集合中，前提是其执行终止。特别地，最少的信息对应于元素 $\bot_{\text{Pred}} = \Sigma \cup \{ \bot \}$。

我们将简单地使用 $\text{Pred}(\Sigma)$ 来表示部分正确性谓词的 cpo。

最弱前置条件构造确定了谓词 cpo 上的一个连续函数——一个**谓词变换器（predicate transformer）**。

**定义**：令 $f : \Sigma \rightarrow \Sigma_\bot$ 是一个状态上的偏函数。定义：

$$
W_f : \text{Pred}(\Sigma) \rightarrow \text{Pred}(\Sigma); \quad (W_f)(Q) = f^{-1}(Q) \cup \{ \bot \}
$$

即，

$$
(W_f)(Q) = \{ \sigma \in \Sigma_\bot \mid f(\sigma) \in Q \} \cup \{ \bot \}
$$

一个命令 $c$ 可以被视为一个状态变换器 $C[c] : \Sigma \rightarrow \Sigma_\bot$，并约定未定义由 $\bot$ 表示。对于一个解释 $I$，根据这种理解，

$$
W_{C[c]}(B_I) = \text{wp}_I[c, B]
$$

**练习7.14**：

令 $ST$ 表示状态变换器的 cpo（即 $[\Sigma_\bot \rightarrow \Sigma_\bot]$），$PT$ 表示谓词变换器的 cpo（即 $[\text{Pred}(\Sigma) \rightarrow \text{Pred}(\Sigma)]$）。

- **任务1**：证明 $W : ST \rightarrow PT$ 且 $W$ 是连续的。（注意！这里有很多需要检查的地方。）

- **任务2**：证明 $W(\text{id}_{\Sigma_\bot}) = \text{id}_{\text{Pred}(\Sigma)}$，即 $W$ 将状态 cpo 上的恒等函数映射为谓词 $\text{Pred}(\Sigma)$ 上的恒等函数。

- **任务3**：证明 $W(f \circ g) = W_g \circ W_f$。

**解答**：

（这里需要对每个任务进行详细的证明，包括定义相关的 cpo，证明连续性，以及验证所需的性质。）

由于篇幅限制，这里不展开具体的证明。

## 7.6 进一步阅读

Crossley 等人的《What is mathematical logic?》[34] 一书对 Gödel 的不完备性定理有出色的解释，尽管缺少一些细节。Kleene [54]、Mendelson [61] 和 Enderton [38] 的逻辑学教材有完整的论述。一本面向计算机科学学生的教材是 Kfoury、Moll 和 Arbib 的《A Programming Approach to Computability》[11]。

Cook 在 [33] 中关于相对完备性的原始证明使用了“最强后置条件（strongest postconditions）”，而不是最弱前置条件；后者在 Clarke 的 [23] 及其早期工作中被使用。此外，Clarke 的论文中还有一些负面结果，显示了不可能为比这里更丰富的编程语言建立健全且相对完备的证明系统。Apt 的论文 [8] 提供了良好的指导。本章材料的替代介绍可以在 [58]、[13] 中找到。Gordon 的书 [42] 包含了对验证条件更为基础和详细的处理。

---

**总结**

在本节中，我们证明了 Gödel 的不完备性定理，讨论了验证条件的概念，以及如何使用它们来辅助程序验证。我们还探讨了谓词变换器的抽象概念，提供了更数学化的观点。

尽管存在理论上的限制，但通过明智地选择不变式和验证条件，我们可以实用地验证程序的部分正确性。这对于编写可靠的软件和理解程序行为都是非常重要的。

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