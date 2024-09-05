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



---



# Type Theory 

## Type Theory VS Set Theory 
- Homotopy type theory is a alternative foundation of Mathematics to Zermelo-Fraenkel set theory.
- Intuitionistic type theory (Or Martin-Löf type theory) based on the propositions-as-types principle and clarifies the Brouwer-Heyting-Kolmogorov interpretation of intuitionistic logic.
-  The law of excluded middle and the law of double negation are not allowed in intuitionistic type theory.

### Deductive System 
- A collection of rules for deriving things called **judgments**.
- In first-order logic deductive system there is only one kind of judgment : A given proposition has a proof. 
- From A and B infer A $\land$ B is a rule of proof construction.
- A has a proof can be written a : A in type theory.
- In type theory , propositions are types, this means that equality is a type. For $a , b : A$ we have a type $a = _A b$. When $a = _A b$ is inhabited we say that a and b are ( **propositional** ) equal.
- Another judgmental equality existing at the same level as the judgment "$x : A$". We write $a \equiv b :A$. It makes no sense to negate or assume judgment equal.
- Definition of a function can be written as $f(x):\equiv x^2$

## Curry-Howard Correspondence
> a proposition is the type of its proofs.


## Context 
To construct an element a of a type A is to derive $a:A$. Judgments are explicitly formulated in an ambient **context** , or list of assumptions , of the form :
$$x_1:A_1 , x_2:A_2,\cdots , x_n:A_n$$
An element $x_i:A_i$ expresses the assumption that the variable $x_i$ has type $A_i$ .The types in a context can depend on variables of **earlier** types. For instance , $A_n$ can depend on $x_1:A_1, x_2:A_2, \cdots , x_{n-1}:A_{n-1}$.

The judgment $a:A$ in context $\Gamma$ is written:
$$\Gamma \vdash a:A$$ 

Use $\cdot$ to denote the empty context.

### Notations
- $\cdot \vdash A \ \text{type}$ meaning that $A$ is a well-formed type
- $\Gamma \ \text{context}$ meaning that $\Gamma$ is a well-formed context.
- $\Gamma \vdash A \ \text{type}$ can be abbreviated as  $\Gamma \vdash A$
- $\Gamma \ \text{context}$ can be abbreviated as $\Gamma \vdash$


## Function Types
- We can construct type $f: A \to B$ of functions with domain A and codomain B. Given a function $f$ and an element $a :A$, we can apply the function to obtain an element of the codomain $B$, denoted $f(a)$.
- How to construct ? $\lambda$-abstraction
    $$f(x):\equiv \Phi $$
    $\Phi$ may contains x as a variable, we have to check $\Phi:B$ assuming $x:A$.
    If we don't want to give a name, we can write down (the type can be omitted ) $\lambda$-abstraction: 
    $$(\lambda(x:A). \Phi):A\to B$$ or
    $$(x\mapsto\Phi):A\to B$$
    
- Example:
    $$(\lambda(x:\mathbb{N}).x+x):\mathbb{N} \to \mathbb{N} $$

- $\beta$-reduction (computation rule): Apply it to $a:A$
    $$(\lambda x . \Phi) (a) \equiv \Phi ' $$
- $\lambda$-abstraction function (Roles the uniqueness principle for function types):
    $$f\equiv (\lambda x.f(x))$$ (Use of the equality is often referred to as $\eta$-conversion).

- Define function with explicit parameters : 
    $$f(x) :\equiv \Phi$$ or $$f:\equiv \lambda x.\Phi$$
    
- For several variables we using currying function ($f:A\to B \to C$):
    $$f(x,y) :\equiv \Phi$$ or
    $$f:\equiv \lambda x.\lambda y. \Phi$$ or even 
    $$f:\equiv x\mapsto y \mapsto \Phi$$

- Implicitly abstract over multiple variables by writting multiple blanks $g(-,-)$ means $\lambda x . \lambda y . g(x,y)$

## Universes and families
- A universe is a type whose elements are types.
- To avoid Russell's paradox we use a hierarchy of universes:
  $$ \mathscr{U}_0: \mathscr{U}_1 : \mathscr{U}_2 : \cdots $$
- We assume that our universes are cumulative that is that all the elements of the $i$-th universe are also elements of the $(i+1)$-th universe
- Typical ambiguity : Left the universes indices implicit 
- To model a collection of types varying over a given type $A$ , we use functions (called families of types of sometimes dependent types) $B:A\to \mathscr{U}$ whose codomain is a universe. 


## Dependent function types ( $\Pi$-Types )

- Given a type $A:\mathscr{U}$ and a family $B:A\to \mathscr{U}$ we can construct the type of dependent functions $\Pi _{(x:A)}B(x):\mathscr{U}$ or $\Pi (x:A) ,B(x) $. Ordinary function type is a special case when $B$ is a constant family
$$
\Pi_{(x:A)}B \equiv (A\to B)
$$
- Define $f: \Pi _{(x:A)}B(x)$ by explicit definitions : 
$$f(x):\equiv \Phi  \text{ for } x:A$$
or using $\lambda$-abstraction
$$\lambda x.\Phi :\Pi _{(x:A)}B(x) $$

- Computation rule: for $a:A$ we have $f(a) \equiv \Phi '$ and $(\lambda x.\Phi)(a)\equiv \Phi '$ where $\Phi ' $ is obtained by replacing all occurrences of $x$ in $\Phi$ by $a$.

- Functions which are **polymorphic** over a given universe. It takes a **type** as one of its arguments and then acts on elements of that type (or other type constructed from it ) An example is $\text{id} \Pi_{(A:\mathscr{U}) } :(A\to A)$

- Another non-trivial example is swap operations that switches the order of the arguments of a (curried ) two-argument function:
$$\text{swap} :\prod _{(A:\mathscr{U})}\prod _{(B:\mathscr{U})}\prod _{(C:\mathscr{U})} (A\to B\to C)\to (B\to A\to C)$$
or use condense notation:
$$\text{swap} :\prod _{A,B,C:\mathscr{U}} (A\to B\to C)\to (B\to A\to C)$$
we can define this by:
$$\text{swap}(A,B,C,g) :\equiv \lambda b .\lambda a. g(a)(b) 
\\ \text{or use subscript: } \\ 
\text{swap} _{A,B,C}(g)(b,a):\equiv g(a,b)$$
- Note that as we did for ordinary functions. In the dependent case we can construct $\Pi _{(x:A)}\Pi_ {(y:B(x))} C(x,y)$

## Product types
- In type theory, product type is a primitive concept as function.

To specify a type, we specify the following rules:
- Formation rule (How to form)
- Introduction rule (How to construct elements) : For example a function type has one **constructor** $\lambda$-abstraction.
- Computation rule (How to use elements) : For example the function type has one **eliminator** function application.
- Computation rule (How eliminator acts on constructor ) 
- (Optional) uniqueness principle: express uniqueness of maps into or out of the type.

Let's construct the product type:
- Form and Introduction: Given $a:A$ and $b:B$ we can form $(a,b):A \times B$. 
- Uniqueness principle (Not a rule , prove later): We expect that every element of $A\times B$ is a pair.
- Computation :
  - Construct non-dependent function $f:A\times B\to C$.
  - Introduce a new rule (Elimination Rule) : For any $g:A\to B\to C$ we can define a function $f:A\times B\to C$ by $f((a,b)) :\equiv g(a)(b)$. Reversely to set theory, we assume that a function on $A\times B$ is well-defined as soon as we specify its values on pairs, and form this we will able to prove that every element of $A\times B$ is a pair.
- Projection functions: $$\text{pr}_1:A\times B \to A \\ \text{pr}_2:A\times B \to B$$ with the defining equations: $$\text{pr}_1((a,b)) :\equiv a \\ \text{pr}_2((a,b)) :\equiv b$$
- Alternative approach by recursor(Product types are a degenerate example of a general framework for **inductive types**): $$\text{rec}_{A\times B}:\prod_{C:\mathscr{U} } (A\to B \to C) \to A\times B \to C$$ with the defining equation: $$\text{rec}_{A\times B}(C, g, (a,b)) :\equiv g(a)(b) $$
- Hence we can define pr by: $$\text{pr}_1 :\equiv rec_{A\times B} (A, \lambda a. \lambda b. a) \\\text{pr}_2:\equiv rec_{A\times B} (A, \lambda a. \lambda b. b)$$

### Generalize the recursor (Non-dependent recursor)
Given $C:A\times B \to \mathscr{U}$ we can define a function $f:\prod_{(x:A\times B)} C(x)$ by $g:\prod_{(x:A)}\prod_{(y:B)}C((x,y))$ with defining equation $$f((x,y)):\equiv g(x)(y)$$

Prove the propositional uniqueness principle: Every element of $A\times B$ is equal to a pair. $$\text{uniq}_{A\times B}:\prod_{x:A\times B} ((\text{pr}_1(x),\text{pr}_2(x)) =_{A\times B} x)$$ (Used the identity type. There is a reflexivity element $\text{refl}_x:x=_A x$ for any $x:A$) 

So we can define: $$\text{uniq}_{A\times B}((a,b)):\equiv \text{refl}_{(a,b)}$$ 

### Induction for product type (Dependent Eliminator)
Given $A,B:\mathscr{U}$ we have $$\text{ind}_{A\times B} \prod_{C:A\times B \to \mathscr{U}}\left(\prod_{x:A} \prod_{y:B} C((x,y))\right)\to \prod_{x:A\times B} C(x)$$ with the defining equation $$\text{ind}_{A\times B} (C,g,(ab)):\equiv g(a)(b)$$

The recursor is just the special case of induction when the family $C$ is constant.

## Dependent pair types ($\Sigma \text{-types}$)
The second component of a pair varies depending on the choice of the first component. (In set theory it corresponds to an indexed sum over a given type. In the sense of coproduct or disjoint union)

Given a type $A:\mathscr{U}$ and a family $B:A\to \mathscr{U}$ the dependent pair type is written: $\Sigma_{(x:A)}B(x):\mathscr{U}$ or $$\sum_{(x:A)}B(x) \text{ or } \Sigma (x:A),B(x)$$

> Like $\lambda-\text{abstractions}$ and $\prod$s, $\sum$s automatically scope over the rest of the expression.

If $B$ is constant then the dependent pair type is the ordinary product type:$$\sum_{x:A}B\equiv A\times B$$

The projection functions $\text{pr}_1$ is similar to product type but the $\text{pr}_2$ is a dependent function :$$\text{pr}_2 : \prod_{p:\sum_{(x:A)}B(x)}B(\text{pr}_1(p))$$

Thus the induction principle (dependent eliminator), we need $$g:\prod_{a:A} \prod_{b:B(a)}C((a,b))$$ We can derive $$f: \prod_{p:\sum_{(x:A)}B(x)}C(p)$$ with $$f((a,b)):\equiv g(a)(b)$$ where $C(p)=B(\text{pr}_1(p))$. Then $\text{pr}_2$ can be defined with $$\text{pr}_2((a,b)):\equiv b$$