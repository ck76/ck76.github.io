





```
不要省略，不要偷懒，重新生成。
详解，同样内容按顺序详解不要遗漏任何，并且公式详解
```





```
详解，同样内容按顺序详解不要遗漏任何，并且公式详解
```





```
我正在阅读一本书。《Type Theory and Functional Programming (Simon Thompson) 》
请按照文章内容顺序详解其内容帮助我理解。不要偷懒。
我是一个没有相关知识背景的小白，所以解释的时候请你详尽，不省略任何重要细节。
按照我提供给你的风格和格式详解：【】
——————————
按照下面这种风格：【### **8.1 Types (类型)**

#### **解释：**
在这一部分中，我们讨论的主要是语言的类型。类型系统的核心任务是为表达式分配适当的类型，并确保每个表达式的类型与其操作一致。这里引入的类型可以是简单的布尔类型和算术类型。我们可以定义以下两种类型：
- 布尔类型：表示逻辑真值 $true$ 和 $false$。
- 算术类型：表示整数或自然数。

这些类型的定义方式可能类似于：
- $\text{Bool}$ 表示布尔类型。
- $\text{Nat}$ 表示自然数类型。

#### **公式定义：**
$$
T ::= \text{Bool} \, | \, \text{Nat}
$$

- $T$ 表示类型的集合。我们引入两种类型：布尔类型 $\text{Bool}$ 和自然数类型 $\text{Nat}$。
- 这是一个简单的类型系统，目的是为表达式赋予一个静态类型，使得表达式的类型在计算前可以被确定。

### **8.2 The Typing Relation (类型关系)**

#### **解释：**
类型关系是类型系统的核心，它描述了表达式如何与类型关联。我们可以通过 **Typing Judgments (类型判断)** 来为表达式赋予类型。类型判断的格式通常是：
$$
\Gamma \vdash e : T
$$
其中：
- $\Gamma$ 是上下文（Context），描述了变量的类型信息。
- $e$ 是表达式。
- $T$ 是表达式的类型。
- $\vdash$ 代表“类型推导”关系，表示在上下文 $\Gamma$ 中，表达式 $e$ 的类型是 $T$。

对于我们这里的布尔和自然数表达式，我们可以定义一些基本的类型推导规则。例如：
1. **布尔常量**的类型规则：
   $$
   \frac{}{\vdash true : \text{Bool}} \quad \frac{}{\vdash false : \text{Bool}}
   $$
   解释：在空上下文中，$true$ 和 $false$ 的类型都是 $\text{Bool}$。

2. **自然数**的类型规则：
   $$
   \frac{}{\vdash 0 : \text{Nat}} \quad \frac{\vdash n : \text{Nat}}{\vdash \text{succ}(n) : \text{Nat}}
   $$
   解释：自然数 $0$ 的类型是 $\text{Nat}$，如果 $n$ 是自然数，那么 $succ(n)$ 也是自然数。

这些类型规则通过推导树的形式表达出来，清晰地描述了表达式的类型推导过程。

#### **推导树实例：**

我们来看一个自然数 $1$ 的推导过程，它可以被表示为：
$$
\frac{\frac{}{\vdash 0 : \text{Nat}}}{\vdash succ(0) : \text{Nat}}
$$

- 首先，$0$ 被推导为自然数。
- 接着，使用类型规则 $succ$，我们推导出 $succ(0)$ 也是自然数。

### **8.3 Safety = Progress + Preservation (安全性 = 前进性 + 保持性)**

#### **解释：**
在类型系统中，安全性通常意味着程序的执行不会发生类型错误。为了证明语言的类型系统是安全的，我们需要两个核心性质：
1. **Progress (前进性)**: 如果表达式 $e$ 是一种类型 $T$ 的表达式，那么要么 $e$ 是一个🥑值（它已经计算完成），要么它能够🥑进一步计算。这意味着程序不会卡住。
2. **Preservation (保持性)**: 如果表达式 $e$ 能够从一个表达式演变为另一个表达式 $e'$（即 $e \to e'$），并且 $e$ 的类型是 $T$，那么 $e'$ 的类型也必须是 $T$。这意味着计算不会改变表达式的类型。

为了证明一个语言的类型系统是安全的，我们需要分别证明这两个性质，并将它们组合起来，证明类型系统的安全性。

#### **公式表示：**

- **前进性**的定义：
   $$
   \text{If } \Gamma \vdash e : T \text{ and } e \text{ is not a value, then } e \to e'.
   $$
   解释：如果表达式 $e$ 有一个类型 $T$，且 $e$ 不是一个值，那么 $e$ 一定可以进行进一步的计算。

- **保持性**的定义：
   $$
   \text{If } \Gamma \vdash e : T \text{ and } e \to e', \text{ then } \Gamma \vdash e' : T.
   $$
   解释：如果表达式 $e$ 的类型是 $T$，并且 $e$ 能够演变为 $e'$，那么 $e'$ 的类型也必须是 $T$。

### **总结：**
第8章通过引入类型系统，为我们展示了如何为算术和布尔表达式赋予类型。通过类型关系和推导规则，我们可以静态地推断出表达式的类型，并证明表达式的计算过程是安全的。本章提出了一个重要的安全性定理，即“安全性 = 前进性 + 保持性”，为后续的类型理论学习奠定了基础。

接下来如果有具体内容或者习题，欢迎提供给我，我会继续详细讲解。

### ---------------------------

### **8.1 Types (类型)** 详解

#### **布尔和算术表达式的语法回顾：**

首先，回顾在第三章中介绍的算术表达式的语法：

$$
t ::= \text{terms:}
$$

- $\text{true}$：常量 $true$（布尔类型的常量，表示“真”）
- $\text{false}$：常量 $false$（布尔类型的常量，表示“假”）
- $\text{if } t_1 \, \text{then } t_2 \, \text{else } t_3$：条件表达式，根据 $t_1$ 的布尔值选择执行 $t_2$ 或 $t_3$
- $0$：常量 $0$（自然数类型的常量，表示数字 0）
- $\text{succ } t$：后继函数，表示自然数的后继（即 $t+1$）
- $\text{pred } t$：前驱函数，表示自然数的前驱（即 $t-1$）
- $\text{iszero } t$：零测试函数，检查一个数是否为 $0$

#### **值的定义：**

我们将术语（terms）的求值结果称为**值**(value)。定义如下：
$$
v ::= \text{values:}
$$

- $\text{true}$：布尔值 $true$
- $\text{false}$：布尔值 $false$
- $nv$：数字值（numeric value）

**数字值**的定义进一步为：
$$
nv ::= \text{numeric values:}
$$

- $0$：自然数 $0$
- $\text{succ } nv$：自然数的后继

这些值的定义为我们提供了构造布尔值和自然数值的基础。表达式通过求值可以得到这些基本值。

#### **问题引出：**

在第三章，我们看到对术语的求值要么产生一个值，要么在求值过程中卡住。例如，如果我们对 $\text{pred false}$ 进行求值，由于 $false$ 不是自然数，无法应用前驱操作符，所以这种情况下求值会“卡住”（没有相应的求值规则）。

**卡住的术语**往往表示程序中出现了无意义或错误的部分。我们希望在不实际求值的情况下，🥑能够静态地判断一个术语是否会卡住，确保其求值不会遇到类型错误。

#### 🥑**类型引入：**

为此，我们需要区分术语的不同类型，从而保证只有自然数才能作为 $\text{pred}$、$\text{succ}$ 和 $\text{iszero}$ 的参数，而只有布尔值才能作为条件语句的条件。我们引入两种类型：
- **Nat**：自然数类型，表示自然数的值，如 $0$ 和其后继。
- **Bool**：布尔类型，表示布尔值 $true$ 和 $false$。

类型系统的任务就是为术语分配这些类型，并静态地保证术语的类型是合法的。我们将使用元变量 $S$、$T$ 和 $U$ 等来表示类型。

#### **类型判断：**

当我们说“一个术语 $t$ 的类型是 $T$”，这意味着 $t$ 可以“显然”地被求值为一个特定形式的值。这里的“显然”是指我们可以通过**静态分析**来确定，而不需要真正对术语进行求值。

例如：
- 术语 $if \ true \ then \ false \ else \ true$ 的类型是 $\text{Bool}$，因为无论条件为真或假，该表达式都会返回布尔值。
- 术语 $\text{pred (succ (pred (succ 0)))}$ 的类型是 $\text{Nat}$，因为它最终可以求值为一个自然数。

然而，我们的类型分析是保守的，基于静态信息的推理手段可能无法给出所有术语的类型。例如，像 $if \ (iszero \ 0) \ then \ 0 \ else \ false$ 或 $if \ true \ then \ 0 \ else \ false$ 这样的表达式在求值过程中虽然不会卡住，但类型系统无法为它们分配类型，因为这些表达式混用了布尔和自然数类型，违反了类型规则。

#### **总结：**

- **类型系统**可以通过🥑静态分析帮助我们在不进行求值的情况下预测术语是否能正确求值。
- **布尔类型**和**自然数类型**是这个类型系统的基本组成部分，它们分别用于区分布尔值和自然数值。
- 🥑**类型判断**能够为术语分配类型，确保术语在计算时不会发生类型错误。
  

这种静态类型系统为我们提供了一个保守的推理机制，在后续章节中将进一步扩展以涵盖更复杂的类型系统。

如果有进一步的问题或者习题，欢迎继续提供给我，我会详细讲解。】
```





```
我在使用markdown编辑器，请注意你公式的格式该用美元符包裹不要用反引号。
详解：【5.11 Expressibility
This section gives a characterisation of the functions which can be written
in the system T T0.
Definition 5.41 A term e of T T0 (or T T, T T +) represents the function
f over the natural numbers if and only if for all natural numbers n1, . . . , nk,
e n1 . . . nk →→ f n1 . . . nk
where n is the representation of the natural number n, given by
succ (succ . . . (succ
| {z }
n
0))
How can we characterise the functions f which are representable? First we
know by the normalisation theorem that they are recursive, since for each
term e, to find the value of
e n1 . . . nk
we simply have to reduce the expression to normal form, and the application
of the rules is certainly a mechanical process. It is equally clear that we
cannot represent all recursive functions in this way, since if we could a
diagonalisation argument would lead to a contradiction. (For an exposition
of the elementary details of computability theory see, for example, [Cut81,
Rog67].)
We thus have that the class of functions is properly contained between
the classes of primitive recursive functions and total recursive functions. A
clue to the precise characterisation lies in the normalisation result, and the
formalisation of its proof term by term.
Theorem 5.42 For each term e of T T0, the proof of normalisation of e
can be formalised in the theory of first-order intuitionistic arithmetic, HA,
or its classical counterpart P A.
Proof: The proof uses a coding (or G¨odel numbering) of the system T T0
within the theory of arithmetic. It involves checking that the steps of the
proof outlined in section 5.6 can be encoded thus. ✷
Note that the result does not claim that the complete normalisation
proof can be coded as a whole — the coding is uniform, but the individual
results cannot be combined into a single proof, as the logical complexity
of the individual proofs grows unboundedly with the complexity of the
expression e.
Just as we explained what it was for a function f to be representable in
one of our type theories, we can define how a function is representable in
P A.
190 CHAPTER 5. EXPLORING TYPE THEORY
Definition 5.43 The term g of P A represents a k-ary function f if and
only if for all n1, . . . , nk,
P A ` g n1 . . . nk = f n1 . . . nk
where n is the representation of the natural number n in P A.
Definition 5.44 A representable k-ary function f is provably total in
P A (HA) if and only if we can prove in P A (HA) that its representative
is total, i.e.
P A ` (∀ x1, . . . , xk)(∃ y)(g x1 . . . xk = y)
Theorem 5.42 can be seen now in a slightly different light, showing that
every function representable in T T0 is provably total in P A. We can also
prove a converse to this, which shows that all functions provable total in
P A can be represented in T T0. The origins of this result lie with G¨odel’s
Dialectica interpretation of P A in a theory of functions which itself can
be viewed as a subtheory of T T0 [G¨58]. More details of this and many
other topics relating to the metamathematics of intuitionism can be found
in [Tro73].
Theorem 5.45 A function f over the natural numbers is representable in
T T0 if and only if it is provably total in P A (or HA).
The author is unaware of precise characterisations of the functions representable in the stronger theories T T and T T +, although [Bee85] gives some
partial results, including one for a system with a single universe. Whatever
the case, the class of functions representable in the type theories is very
large, and indeed it can be argued that this more than encompasses all the
functions we might ever wish to program. In terms of sheer computation
time all the functions we program are primitive recursive, in the sense that
by suitable transformation any more complex calculations can be bounded
by primitive recursive bounds. This is not the most natural way to proceed; in the next chapter we look at the ways in which functions are most
naturally implemented in the language.
Exercise
5.34. One function which cannot be written in T T is an interpreter for the
expressions of T T itself. Discuss how a bounded interpreter for the language
can be written.
5.12. THE CURRY HOWARD ISOMORPHISM? 191
5.12 The Curry Howard Isomorphism?
The identification of propositions and types, proofs and objects has been
fundamental to our investigation so far. In this section we look at two
aspects of the system which seem not to fit with this identification.
5.12.1 Assumptions
Suppose we have a proof p of the proposition B depending upon the assumption A. The rule of ⇒introduction allows us to derive A ⇒ B without
the assumption of A. There may be a number of occurrences of A in p;
without loss of generality all these are discharged by the implication introduction. This intuitive account is not an accurate account of the rule
(⇒ I); only the assumptions of A named x are discharged in the application
[x:A]
.
.
.
e:B
λx . e : A ⇒ B
(⇒ I)
and if e also contains y :A, the proof of A ⇒ B still depends upon A. The
alternative rule, which we call (⇒ I)alt would discharge all assumptions of
A. It might be argued that the rule (⇒ I) allows the user of the system more
freedom in proof construction. This is the case, but nonetheless it allows
no more theorems to be proved, for we can simply replace all occurrences
of (⇒ I) by (⇒ I)alt, some of the applications of the latter resulting in
vacuous discharges of the hypothesis of the implication.
On the other hand, named variables are crucial, as can be seen by the
derivation
[x:N]
2
[y :N]
1
(x + y) : N
λy .(x + y) : N ⇒ N
(⇒ I)1
λx . λy .(x + y) : N ⇒ N ⇒ N
(⇒ I)2
For the object λx . λy .(x+y) to have the proper computational behaviour,
it is crucial that the two assumptions x : N and y : N are distinct, and
that x : N is not identified with y : N. As far as the inhabitation of the
proposition N ⇒ N ⇒ N is concerned, it is irrelevant, naturally.
The mismatch here can be traced to the divergence of interests between
the users of a logical system, who are primarily interested in proving theorems, that is in showing that particular types are inhabited, and the users
of a programming language who are interested in the behaviour of many
192 CHAPTER 5. EXPLORING TYPE THEORY
different objects of a given type. On the other hand, the proof theorist
who studies the general behaviour of logical systems is interested in such
behaviour. We look at this next.
5.12.2 Normal Forms of Proofs
When we discussed computation and reduction in section 5.5 the emphasis
was from the programming point of view: we argued that the expressions
we were interested in studying were closed expressions of ground type: these
are the printable values of a functional language. If we think of the expressions as denoting proofs of formulas, then neither assumption is tenable.
We are interested in proofs of expressions like
(A ⇒ B) ⇒ (A ⇒ C)
which are not of ground type, and which may depend upon assumptions
(in this case, on A ⇒ (B ⇒ C) say).
Proof theorists, such as Prawitz in his pioneering study of Natural Deduction, [Pra65], are interested in showing that
[The] rules allow the deduction to proceed in a certain direct
fashion, affording an interesting normal form for deductions.
[Pra65] shows the normal form theorem for proofs in a number of different
systems of deduction, including first- and second-order classical and intuitionistic logic. The crucial reduction to ensure ‘directness of proof’ is that
embodied in our computation rules: the elimination of a formula just introduced can be avoided. For example, the rule of β-reduction is interpreted
as saying that the natural deduction proof
.
.
.
A
[A]
.
.
.
B
A ⇒ B
(⇒ I)
B
(⇒ E)
can be transformed to
.
.
.
A
.
.
.
B
in which the proof of A replaces the assumption(s) of A in the proof of B.
5.12. THE CURRY HOWARD ISOMORPHISM? 193
The computation rules are not the only simplifications possible. For the
reasons above, the arguments of 2.11 do not apply, and so we have another
mismatch. The extra rules come in two different forms. Instead of replacing
‘introduction then elimination’ we can also replace ‘elimination then introduction’. These are examples of the equivalence rules we discussed earlier.
For example, we might encounter the following steps in a proof.
[A] A ⇒ B
B
(⇒ E)
A ⇒ B
(⇒ I)
A ∧ B
A
(∧E1)
A ∧ B
B
(∧E2)
A ∧ B
(∧I)
both of which are completely irrelevant to the result of the proof. The
corresponding reduction rules in type theory are
λx .(f x) → f if x not free in f
(fst p, snd p) → p
and for each type we can devise a similar rule. The reading we have given to
the rules above shows that as far as proofs are concerned, they do perform
a simplification.
The other class of commutation rules are included in the system studied by Prawitz for more technical reasons, which are discussed by him and
also in [GLT89, Section 10]. The simplest is the equivalence between
P1
∃x.B
P2
F
F
(E∃) P3
D
(R)
P1
∃x.B
P2
F
P3
D
(R)
D
(E∃)
in which we can see that the proof of D from F and the proof P3 can be
performed before or after the existential elimination. Any orientation of
this equivalence into a reduction rule will be arbitrary. Prawitz chooses to
reduce the left-hand to the right-hand side.
These considerations seem to be motivated by proof-theoretic considerations, but a final twist is added by their link with the discussion of the
computational efficiency (or otherwise) of certain rules, and in particular
the considerations which lead us to the strong elimination rules of section
7.7.】
```

