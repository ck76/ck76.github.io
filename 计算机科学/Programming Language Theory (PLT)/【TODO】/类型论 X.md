[toc]

# 类型论

## 类型论 vs 集合论

- 同伦类型论 (Homotopy type theory) 是一种与 Zermelo-Fraenkel 集合论不同的数学基础。
- 直觉主义类型论 (Intuitionistic type theory) 或 Martin-Löf 类型论基于 **命题即类型** 原则，并澄清了 Brouwer-Heyting-Kolmogorov 对直觉主义逻辑的解释。
- 在直觉主义类型论中，排中律 (Law of excluded middle) 和双重否定律 (Law of double negation) 不适用。

### 演绎系统

- 一组用于推导称为 **判断 (judgments)** 的规则。
- 在一阶逻辑 (First-order logic) 演绎系统中，只有一种判断：给定的命题有证明。
- 从 A 和 B 推导出 $A \land B$ 是构造证明的规则之一。
- 在类型论中，一个命题可以写作 $a : A$，表示 A 的证明 a。
- 在类型论中，命题是类型，这意味着相等性也是一种类型。对于 $a , b : A$，我们有一个类型 $a = _A b$。当 $a = _A b$ 有实例时，表示 a 和 b 是 **命题上的相等**。
- 另一种判断相等性与判断 "$x : A$" 在同一层级上。我们写作 $a \equiv b :A$。这种判断的相等性无法否定或假设。

函数的定义可以写作 $f(x):\equiv x^2$。

## Curry-Howard 同构

> 命题是其证明的类型。

## 语境 (Context)

构造一个类型 A 的元素 a 就是推导 $a:A$。判断明确地在 **语境 (context)** 中进行，或者在假设列表中，形式为：
$$x_1:A_1 , x_2:A_2,\cdots , x_n:A_n$$
一个元素 $x_i:A_i$ 表示假设变量 $x_i$ 属于类型 $A_i$。语境中的类型可以依赖于 **更早的** 类型的变量。例如，$A_n$ 可以依赖于 $x_1:A_1, x_2:A_2, \cdots , x_{n-1}:A_{n-1}$。

在语境 $\Gamma$ 中的判断 $a:A$ 可以写作：
$$\Gamma \vdash a:A$$

空语境使用 $\cdot$ 表示。

### 符号约定

- $\cdot \vdash A \ \text{type}$ 表示 A 是一个良构的类型。
- $\Gamma \ \text{context}$ 表示 $\Gamma$ 是一个良构的语境。
- $\Gamma \vdash A \ \text{type}$ 可以简写为 $\Gamma \vdash A$。
- $\Gamma \ \text{context}$ 可以简写为 $\Gamma \vdash$。

## 函数类型 (Function Types)

- 我们可以构造一个类型 $f: A \to B$，其定义域为 A，值域为 B。给定一个函数 $f$ 和一个元素 $a :A$，我们可以应用该函数得到一个值域 $B$ 的元素，记作 $f(a)$。
- 如何构造？ 使用 $\lambda$ 抽象：
    $$f(x):\equiv \Phi $$
    其中 $\Phi$ 可能包含 x 作为变量，我们需要验证 $\Phi:B$ 在假设 $x:A$ 的情况下成立。
    如果我们不想给变量命名，可以写作 $\lambda$ 抽象：
    $$(\lambda(x:A). \Phi):A\to B$$ 或
    $$(x\mapsto\Phi):A\to B$$
    
- 例子：
    $$(\lambda(x:\mathbb{N}).x+x):\mathbb{N} \to \mathbb{N} $$

- $\beta$-化简 (计算规则): 将其应用于 $a:A$：
    $$(\lambda x . \Phi) (a) \equiv \Phi ' $$
- $\lambda$ 抽象函数 (作为函数类型的唯一性原则):
    $$f\equiv (\lambda x.f(x))$$ （该相等性的使用常称为 $\eta$-转换）。

- 显式参数定义函数：
    $$f(x) :\equiv \Phi$$ 或 $$f:\equiv \lambda x.\Phi$$
    
- 对于多个变量，我们使用柯里化 (currying) 函数 ($f:A\to B \to C$):
    $$f(x,y) :\equiv \Phi$$ 或
    $$f:\equiv \lambda x.\lambda y. \Phi$$ 或者
    $$f:\equiv x\mapsto y \mapsto \Phi$$

- 隐式抽象多个变量，写作多个空白符号 $g(-,-)$ 意味着 $\lambda x . \lambda y . g(x,y)$。

## 宇宙 (Universes) 和类型族 (Families)

- 宇宙是一个类型，它的元素是其他类型。
- 为了避免 Russell 悖论，我们使用宇宙的层次结构：
  $$ \mathscr{U}_0: \mathscr{U}_1 : \mathscr{U}_2 : \cdots $$
- 我们假设宇宙是累积的，即第 i 个宇宙的所有元素也是第 $i+1$ 个宇宙的元素。
- 常见的歧义：宇宙的索引通常被省略。
- 为了表示一个类型集合依赖于给定类型 A，我们使用函数（称为类型族或依赖类型）$B:A\to \mathscr{U}$，其值域是一个宇宙。

## 依赖函数类型 ( $\Pi$-类型)

- 给定一个类型 $A:\mathscr{U}$ 和一个族 $B:A\to \mathscr{U}$，我们可以构造依赖函数类型 $\Pi _{(x:A)}B(x):\mathscr{U}$ 或 $\Pi (x:A) ,B(x)$。当 B 是常量族时，普通的函数类型就是其特殊情况：
$$
\Pi_{(x:A)}B \equiv (A\to B)
$$

- 定义 $f: \Pi _{(x:A)}B(x)$ 的显式定义：
$$f(x):\equiv \Phi  \text{ for } x:A$$
或使用 $\lambda$ 抽象：
$$\lambda x.\Phi :\Pi _{(x:A)}B(x) $$

- 计算规则：对于 $a:A$ 我们有 $f(a) \equiv \Phi '$ 和 $(\lambda x.\Phi)(a)\equiv \Phi '$，其中 $\Phi '$ 是通过将 $\Phi$ 中的所有 x 替换为 a 得到的。

- 定义 **多态** 函数。它以类型为参数，并对该类型的元素进行操作，例如：
$$\text{id} \Pi_{(A:\mathscr{U}) } :(A\to A)$$

- 另一个例子是交换函数的参数顺序：
$$\text{swap} :\prod _{(A:\mathscr{U})}\prod _{(B:\mathscr{U})}\prod _{(C:\mathscr{U})} (A\to B\to C)\to (B\to A\to C)$$
或简写：
$$\text{swap} :\prod _{A,B,C:\mathscr{U}} (A\to B\to C)\to (B\to A\to C)$$

可以通过定义：
$$\text{swap}(A,B,C,g) :\equiv \lambda b .\lambda a. g(a)(b)$$

## 乘积类型 (Product types)

- 在类型论中，乘积类型和函数类型一样是基本概念。

构造乘积类型的规则：

- 形成规则：如何构造类型。
- 引入规则：如何构造元素。
- 计算规则：如何使用元素。

构造乘积类型：
- 形式和引入：给定 $a:A$ 和 $b:B$，我们可以构造 $(a,b):A \times B$。
- 计算规则：构造函数 $f:A\times B\to C$。
- 投影函数：
$$\text{pr}_1:A\times B \to A \\
\text{pr}_2:A\times B \to B$$

## 依赖对类型 ( $\Sigma$-类型)

- $\Sigma$-类型用于表示依赖于其他类型的对。
- 定义：$$\Sigma_{(x:A)}B(x):\mathscr{U}$$