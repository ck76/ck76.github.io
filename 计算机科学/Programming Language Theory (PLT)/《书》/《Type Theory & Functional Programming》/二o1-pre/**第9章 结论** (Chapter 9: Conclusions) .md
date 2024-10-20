[toc]



# 第9章 结论

---

## 概览

在本章中，我们对与构造性类型理论相关的多个系统进行了回顾和比较。我们强调了这些系统与类型理论本身的主要区别，但要完全公正地介绍它们，每一个都需要一本同样规模的书籍。在接下来的部分中，我们将简要介绍这些系统，提供相关的参考文献，并讨论它们与类型理论之间的相似点和差异。

---

## 9.1 相关工作

在这一节中，我们考察了一些与 Martin-Löf 类型理论相关的系统，包括已实现的和理论上的系统。我们不会给出详尽的描述，而是提供对这些系统的简要介绍、参考文献，并讨论它们与类型理论的相似点和差异。

值得注意的是，在我们的讨论中，我们主要关注类型理论的内涵版本（intensional version），即 [ML75b] 中提出的版本；在整个过程中，我们也讨论了它与外延版本的区别。

### 9.1.1 Nuprl 系统

**背景**

在过去的十五年里，康奈尔大学计算机科学系有一个活跃的研究小组，由 Constable 和 Bates 领导，致力于逻辑的实现，特别是用于数学形式化和程序开发的逻辑。他们的研究 culminated（达到了高潮）于 Nuprl 系统的开发，该系统在 [C+86a] 一书中有详细描述。

**系统概述**

- **类型理论基础**：Nuprl 实现了一个与 [ML85] 中的外延版本类型理论相关的类型理论，但在多个方面进行了修改和扩展。
- **逻辑取向**：与我们在本书中主要将类型理论视为函数式编程系统不同，Nuprl 更侧重于逻辑，旨在支持在自然演绎系统中从上到下地构建命题的推导。
- **证明对象**：类型理论中的证明对象被称为“抽取项（extract terms）”，这些项是从推导中后期抽取出来的，在推导中它们只是隐含地出现。

**系统特性**

- **战术和策略**：证明是以从上到下的方式构建的，使用了与 LCF 系统类似的战术（tactics）和策略（tacticals）。实际上，逻辑被嵌入到 ML 元语言中，就像 LCF 一样。
- **强消除规则**：系统包含了第 7.7.2 节中的强消除规则，以及所谓的“直接计算规则”（direct computation rules），详见 [C+86a] 的附录 C。这些规则允许在没有通常所需的良构性证明的情况下重写项。
- **附加构造**：Nuprl 系统中增加了多个构造，包括子集（参见第 7.2 节）、商类型（第 7.5 节）、部分函数类型（第 7.12 节）等。

**讨论**

- **消除计算无关信息**：许多附加构造的引入是为了从抽取的对象中消除计算上无关的信息，正如我们在第 7.1.2 节中讨论的那样。这在一定程度上是由于在许多推导中证明对象是隐含的。
- **与类型理论的区别**：Nuprl 的取向与类型理论有显著区别，更加强调逻辑推理和证明的构造，而不是函数式编程。

### 9.1.2 TK：类型和类的理论

**背景**

TK 系统在 [HT88] 中被介绍，其中包含了对 TT（类型理论）和 TK 的有用比较，程序开发在 TK 中的讨论见 [Hen89]。TK 是一个构造性集合的理论，设计的主要目标是程序开发。

**系统特性**

- **类型与命题的分离**：TK 系统将类型（或集合）与逻辑断言分离，这与类型理论中类型和命题的统一形成对比。Henson 和 Turner 认为，类型理论中类型与命题的统一导致了一些缺点，包括对子集和一般递归的不满意处理。
- **集合构造操作**：主要的集合构造操作是**分离（separation）**和**归纳（induction）**。
  - **分离**：允许定义如 \(\{ x \mid \Phi(x) \}$ 的集合，包含满足性质 \(\Phi\) 的所有 $x$。
  - **归纳构造**：类似于我们在第 7.10 节中讨论的归纳类型。
- **层次结构**：为了增加表达能力，系统中增加了一个层次的宇宙（kinds），因此得名 TK（Types and Kinds）。
- **部分定义的项**：与类型理论中所有项都是全定义的不同，TK 中的项可以是部分定义的或未定义的。

**讨论**

- **逻辑的一致性**：由于逻辑和类型不再统一，部分对象的存在并不使逻辑不一致。
- **程序提取**：在一个丰富的逻辑语言中对集合进行推理，由于逻辑是构造性的，存在其实现性模型（realizability models）。使用第 8.1.2 节中概述的技术，可以从构造性证明中提取程序。这种实现性方法允许在选择实现操作时有很大的灵活性。
- **消除计算无关部分**：实现性方法的另一个优势是，函数中计算无关的部分不需要出现，这在 [Hen91] 中有详细讨论。
- **与类型理论的比较**：在 TT 中，程序提取和显式的函数式编程都可用，并且可以在一个开发中结合使用。TK 是否能够同样好地结合这两者尚不明确。

**现状**

- **理论基础和实现**：关于 TK 的理论基础和实现的工作都在进行中，预计这些工作将丰富我们对 TT 的理解。
- **实用性**：还需要观察这两个系统中哪一个在实际的程序开发中更具优势。

### 9.1.3 PX：一个计算逻辑

**背景**

不仅可以从类型理论中提取证明，直觉主义形式系统的实现性模型也允许从系统中提取计算。一个明显不同的系统，称为 PX，在京都大学数理科学研究所开发，介绍见 [Hay90]，详细描述见专著 [HN88]。

**系统特性**

- **逻辑基础**：PX 是一个针对计算的无类型理论的逻辑，基于 Feferman 的 T₀ 系统（[Fef79]），通过一种称为 px-实现性的概念，从中提取 LISP 程序。
- **非终止计算**：Hayashi 认为像 TT 这样的理论要求所有程序都终止，这对于实际的程序开发来说过于限制。因此，他的逻辑基于一个可能非终止计算的系统。
- **变量类型**：系统中包含两种变量，一种范围涵盖终止的对象，另一种范围涵盖所有对象。此外，还引入了一个定义谓词 $E$，类似于 [Sco79] 中的做法。

**关键概念**

- **条件归纳生成（CIG）**：这是定义子集、进行递归和归纳证明（包括证明域上的终止性）的关键原则。在这方面，它类似于第 7.9 节中的良基递归和第 7.10 节中的归纳类型。
- **Rank 0 公式**：Hayashi 定义了不包含 $\lor$ 或 $\exists$ 的公式，称为 Rank 0。对于这些公式，不需要实现项，因为它们没有计算内容。例如，包含关系 $A \subseteq B$ 是一个 Rank 0 公式。

**讨论**

- **经典逻辑的应用**：可以使用经典逻辑证明 Rank 0 公式，而不会危及系统的一致性或计算解释，这体现了我们在上一节讨论的灵活性。

### 9.1.4 AUTOMATH

**背景**

AUTOMATH 项目于 1966 年在埃因霍温大学由 Nicolaas de Bruijn 启动，其目标是：

> 开发一种系统，以如此精确的方式书写完整的数学理论，使得对正确性的验证可以通过对文本的形式操作来进行。

引文来自 [dB80]，该文对该项目进行了概述。关于在系统中呈现数学的示例，可以参见 [dB73]。该小组的成就之一是对 Landau 的《基础》这本分析学教材的完整形式化。

**系统特性**

- **类型理论**：系统使用了一种基于“命题即类型”概念的类型理论来表示经典或构造性数学的逻辑。
- **技术创新**：包括在经典背景下讨论证明的无关性，这是 de Bruijn 对系统中类型和命题（prop）概念分离的原因之一，这一思想比计算机科学界的讨论（见第 7.1.2 节）早了几年。

**实现技巧**

- **变量名处理**：在实现系统的过程中，发现识别仅在绑定变量名（α-转换）上不同的 λ 表达式是一个相当大的开销。[dB72] 引入了一种非常有用的方法来绕过这个问题：
  - **De Bruijn 索引**：用数值索引替换变量名。变量的出现被替换为从该出现到绑定它的 λ 之间的 λ 的数量。例如，λ 表达式：

    $$
    \lambda a . \lambda b . \lambda c . (a c)(b c)
    $$

    被替换为：

    $$
    \lambda . \lambda . \lambda . (2\ 0)(1\ 0)
    $$

  - 这种技术在函数式编程语言和逻辑的实现中都有应用，参见 [Hue90a] 的第一部分。

### 9.1.5 类型理论

**背景**

系统 TT₀ 和 TT 可以被视为简单类型 λ-演算的扩展（参见第 2 章）。其他扩展也存在，我们在此考察其中两个最重要的。

#### 二阶 λ-演算（多态 λ-演算）

- **起源**：由 [Rey74] 和 [Gir72] 独立发明，后者称其为系统 F。
- **介绍**：一个简短而全面的介绍见 [Rey90]。
- **特性**：
  - 允许类型变量 $\alpha, \beta, \ldots$，使得函数 $K \equiv \lambda x . \lambda y . x$ 可以赋予类型 $\alpha \rightarrow \beta \rightarrow \alpha$，这在多态语言如 SML 和 Miranda 中也是可能的。
  - 通过引入类型抽象操作符 $\Lambda$ 和类型构造 $\Pi$，将隐式的全称量化显式化。

- **类型系统**：
  - 如果 $e : t$，那么表达式 $\Lambda \alpha . e$ 对类型变量 $\alpha$ 进行参数化，类型为 $\Pi \alpha . t$。
  - 类型抽象可以像普通的抽象一样应用，满足 $(\Lambda \alpha . e) \ \xi : t[\xi / \alpha]$，其中 $\xi$ 是任意类型表达式。

- **讨论**：
  - 该系统比 Milner 类型系统更强，因为类型可以包含嵌入的全称量化。
  - 该系统比 TT₀ 更具表达能力，具有二阶算术的证明论强度。

#### 构造演算（Calculus of Constructions）

- **起源**：由 Coquand 和 Huet 在 [CH85] 中引入，在 [Hue90b] 中与其他类型理论进行比较。
- **特性**：
  - 允许直接定义类型操作，如从 $A$ 和 $B$ 构造 $\Pi C . ((A \rightarrow C) \rightarrow (B \rightarrow C) \rightarrow C)$。
  - 通过不引入新的类型 $\text{Prop}$ 来避免逻辑不一致性。
  - 允许在一个非常简单的基础理论中发展大量数学。
- **讨论**：
  - 一个需要注意的点是语言中似乎无法定义强存在类型，这体现在消除规则中证明对象和类型的混合方式上。

---

## 9.2 结语

从最后两章的内容可以看出，对类型理论的研究仍在发展中。尽管如此，系统的核心已被证明是逻辑和函数式编程的一个健全而优雅的结合，我希望读者能分享我对这一主题的热情。

**反思**

- **添加特性的代价**：从第 7 章的内容反思，一个不可避免的负面结论是，尽管可以对系统进行扩展，但每次添加特性似乎都要付出代价，要么增加系统的复杂性，要么改变其元数学性质。因此，我们应该在添加特性之前非常确定其必要性。
- **编程语言的教训**：这个结论对于熟悉大多数编程语言的人来说是显而易见的，在这些语言中，各种特性杂乱地共存，导致了不稳定的状况。这可能是我们需要自己去学习的东西。

**展望**

- **命令式编程的困境**：尽管理论上完全可以为用 Pascal 和 C 编写的程序开发证明，但实际上并未发生。
- **函数式编程的优势**：函数式语言提供了更光明的前景，因为它们清晰的语义使得证明更短、更高层次。
- **类型理论的潜力**：下一步似乎是转向像类型理论这样的系统，它可以为程序开发、变换和验证活动提供单一的基础，就像 LISP 机器支持集成调试、性能分析等一样。
- **未来的发展**：尽管仍有许多工作要做，以使类型理论成为一个可用且有吸引力的系统，支持大型编程，但我相信，基于类型理论的语言将在几年后像现在基于简单类型 λ-演算的函数式语言（如 Miranda 和 ML）一样受欢迎。

---

## 参考文献

- [C+86a] Constable, R. L., et al. (1986). *Implementing Mathematics with the Nuprl Proof Development System*. Prentice Hall.
- [ML75b] Martin-Löf, P. (1975). "An intuitionistic theory of types: predicative part." In *Logic Colloquium '73*, North-Holland.
- [HT88] Henson, C., & Turner, R. (1988). "A Theory of Types for Program Development." *Mathematical Structures in Computer Science*, 1(1), 45–70.
- [Hen89] Henson, C. (1989). "Program development in a constructive theory of types and kinds." *Mathematical Structures in Computer Science*, 2(1), 41–70.
- [Hen91] Henson, C. (1991). "A realizability interpretation for a constructive theory of types and kinds." *Journal of Symbolic Logic*, 56(1), 274–293.
- [Hay90] Hayashi, S. (1990). "PX: A Computational Logic." In *Category Theory and Computer Science*, Springer.
- [HN88] Hayashi, S., & Nakano, H. (1988). *PX: A Computational Logic*. Research Institute of Mathematical Sciences, Kyoto University.
- [Fef79] Feferman, S. (1979). "Constructive theories of functions and classes." In *Logic Colloquium '78*, North-Holland.
- [Sco79] Scott, D. S. (1979). "Constructive Validity." In *Symposia Mathematica*, Vol. 21, Academic Press.
- [dB80] de Bruijn, N. G. (1980). "A survey of the project AUTOMATH." In *To H. B. Curry: Essays on Combinatory Logic, Lambda Calculus and Formalism*, Academic Press.
- [dB73] de Bruijn, N. G. (1973). "The mathematical language AUTOMATH, its usage and some of its extensions." In *Symposium on Automatic Demonstration*, Springer.
- [dB72] de Bruijn, N. G. (1972). "Lambda calculus notation with nameless dummies: a tool for automatic formula manipulation." *Indagationes Mathematicae*, 34(5), 381–392.
- [Hue90a] Huet, G. (Ed.). (1990). *Logical Frameworks*. Cambridge University Press.
- [Rey74] Reynolds, J. C. (1974). "Polymorphic lambda calculus and semantic models." *Proceedings of the ACM Conference on Proving Assertions about Programs*.
- [Gir72] Girard, J.-Y. (1972). "Interprétation fonctionnelle et élimination des coupures dans l'arithmétique d'ordre supérieur." Ph.D. thesis, Université de Paris VII.
- [Rey90] Reynolds, J. C. (1990). "Theories of Programming Languages." Cambridge University Press.
- [GLT89] Girard, J.-Y., Lafont, Y., & Taylor, P. (1989). *Proofs and Types*. Cambridge University Press.
- [CH85] Coquand, T., & Huet, G. (1985). "The Calculus of Constructions." *Information and Computation*, 76(2-3), 95–120.
- [Hue90b] Huet, G. (1990). "The Calculus of Constructions." In *Logical Frameworks*, Cambridge University Press.

### ---------------------------

# 第9章 结论

---

## 概览

本章对与**构造性类型理论**相关的多个系统进行了深入的回顾和讨论。这些系统在某种程度上都与类型理论有关，但也存在显著的差异。我们试图强调这些系统与类型理论之间的主要区别，但要全面地介绍它们，每一个都需要一本同样规模的书籍。

本章的目标是提供对这些系统的**简要介绍**、**相关参考文献**，并讨论它们与类型理论的**相似点和不同点**。

---

## 9.1 相关工作

在这一节中，我们将探讨一些与 Martin-Löf 类型理论（Type Theory，简称 TT）相关的系统，包括已实现的和理论上的系统。我们不会给出详尽的描述，而是提供对这些系统的**简要介绍**、**参考文献**，并讨论它们与类型理论的相似点和差异。

值得注意的是，在我们的讨论中，我们主要关注类型理论的**内涵版本（intensional version）**，即 [Martin-Löf, 1975b] 中提出的版本；在整个过程中，我们也会讨论它与外延版本的区别。

### 9.1.1 Nuprl 系统

**背景**

在过去的十五年里，康奈尔大学计算机科学系有一个活跃的研究小组，由 Robert L. Constable 和 Stuart F. Allen 等人领导，致力于**逻辑的实现**，特别是用于**数学形式化**和**程序开发**的逻辑。他们的研究 culminated（达到顶峰）于 Nuprl 系统的开发，该系统在 [Constable et al., 1986a] 一书中有详细描述。

**系统概述**

- **类型理论基础**：Nuprl 实现了一个与 [Martin-Löf, 1985] 中的**外延版本**类型理论相关的类型理论，但在多个方面进行了修改和扩展。

- **逻辑取向**：与我们在本书中主要将类型理论视为**函数式编程系统**不同，Nuprl 更侧重于**逻辑**，旨在支持在自然演绎系统中**从上到下地构建命题的推导**。

- **证明对象**：类型理论中的证明对象被称为**抽取项（extract terms）**，这些项在推导中**隐式**出现，事后从推导中提取出来。

**系统特性**

- **战术和策略**：证明是以**从上到下**的方式构建的，使用了与 LCF 系统类似的**战术（tactics）**和**策略（tacticals）**。实际上，逻辑被嵌入到 ML 元语言中，就像 LCF 一样。

- **直接计算规则**：系统包含了**强消除规则**（参见第 7.7.2 节），以及所谓的**直接计算规则（direct computation rules）**（见 [Constable et al., 1986a] 的附录 C），这些规则允许在没有通常所需的良构性证明的情况下重写项。

- **附加构造**：Nuprl 系统中增加了多个构造，包括**子集类型**（见第 7.2 节）、**商类型**（第 7.5 节）、**部分函数类型**（第 7.12 节）等。

**讨论**

- **消除计算无关信息**：许多附加构造的引入是为了从抽取的对象中**消除计算上无关的信息**，正如我们在第 7.1.2 节中讨论的那样。这在一定程度上是由于在许多推导中，证明对象是**隐式**的。

- **与类型理论的区别**：Nuprl 的取向与类型理论有显著区别，更加强调**逻辑推理**和**证明的构造**，而不是函数式编程。

- **参考文献**：关于系统的全面参考资料是 [Constable et al., 1986a]；关于底层思想的简短介绍可以在 [Bates and Constable, 1985] 和 [Constable, Knoblock, and Bates, 1984] 中找到。

---

### 9.1.2 TK：类型和类的理论

**背景**

TK 系统在 [Henson and Turner, 1988] 中被介绍，其中包含了对 TT（类型理论）和 TK 的有用比较。关于在 TK 中的程序开发，可以参见 [Henson, 1989]。TK 是一个**构造性集合的理论**，设计的主要目标是**程序开发**。

**系统特性**

- **类型与命题的分离**：TK 系统将**类型（或集合）**与**逻辑断言**分离，这与类型理论中类型和命题的统一形成对比。Henson 和 Turner 认为，类型理论中类型与命题的统一导致了一些缺点，包括对子集和一般递归的不满意处理。

- **集合构造操作**：主要的集合构造操作是**分离（separation）**和**归纳（induction）**。

  - **分离**：允许定义如 \(\{ x \mid \Phi(x) \}$ 的集合，包含满足性质 \(\Phi\) 的所有 $x$。

  - **归纳构造**：类似于我们在第 7.10 节中讨论的归纳类型。

- **层次结构**：为了增加表达能力，系统中增加了一个层次的**宇宙或类（kinds）**，因此得名 TK（Types and Kinds）。

- **部分定义的项**：与类型理论中所有项都是全定义的不同，TK 中的项可以是**部分**或**未定义**的。

**讨论**

- **逻辑的一致性**：由于逻辑和类型不再统一，部分对象的存在并不使逻辑不一致。

- **程序提取**：在一个丰富的逻辑语言中对集合进行推理，由于逻辑是构造性的，存在其**实现性模型（realisability models）**。使用第 8.1.2 节中概述的技术，可以从构造性证明中提取程序。这种实现性方法允许在选择实现操作时有很大的灵活性。

- **消除计算无关部分**：实现性方法的另一个优势是，函数中**计算无关的部分**不需要出现，这一主题在 [Henson, 1991] 中有详细讨论。

- **与类型理论的比较**：在 TT 中，**程序提取**和**显式的函数式编程**都可用，并且可以在一个开发中结合使用。TK 是否能够同样好地结合这两者尚不明确。

**现状**

- **理论基础和实现**：关于 TK 的理论基础和实现的工作都在进行中，预计这些工作将丰富我们对 TT 的理解。

- **实用性**：还需要观察这两个系统中哪一个在实际的程序开发中更具优势。

---

### 9.1.3 PX：一个计算逻辑

**背景**

不仅可以从类型理论中提取证明，**直觉主义形式系统的实现性模型**也允许从系统中提取计算。一个明显不同的系统，称为 **PX**，在京都大学数理科学研究所开发，介绍见 [Hayashi, 1990]，详细描述见专著 [Hayashi and Nakano, 1988]。

**系统特性**

- **逻辑基础**：PX 是一个针对**无类型计算理论**的逻辑，基于 Feferman 的 T₀ 系统（[Feferman, 1979]），通过一种称为 **px-实现性** 的概念，从中提取 LISP 程序。

- **非终止计算**：Hayashi 认为像 TT 这样的理论要求所有程序都终止，这对于实际的程序开发来说过于限制。因此，他的逻辑基于一个可能**非终止计算**的系统。

- **变量类型**：系统中包含两种变量，一种范围涵盖**终止的对象**，另一种范围涵盖**所有对象**。此外，还引入了一个**定义谓词** $E$，类似于 [Scott, 1979] 中的做法。

**关键概念**

- **条件归纳生成（CIG）**：这是定义子集、进行递归和归纳证明（包括证明域上的终止性）的关键原则。在这方面，它类似于第 7.9 节中的**良基递归**，以及第 7.10 节中的**归纳类型**。

- **Rank 0 公式**：Hayashi 定义了不包含 $\lor$ 或 $\exists$ 的公式，称为 **Rank 0**。对于这些公式，不需要实现项，因为它们没有计算内容。例如，包含关系 $A \subseteq B$ 是一个 Rank 0 公式；一个这样的公式的实例可能是一个关于终止性的断言：集合 $A$ 包含在函数 $f$ 的定义域 $B$ 中。

**讨论**

- **经典逻辑的应用**：可以使用**经典逻辑**证明 Rank 0 公式，而不会危及系统的一致性或计算解释，这体现了我们在上一节讨论的灵活性。

- **灵活性**：实现性方法允许在选择实现操作时有很大的灵活性，例如，可以将一个条件断言添加到语言中，其实现者可以与通常的逻辑定义明显不同。

---

### 9.1.4 AUTOMATH

**背景**

AUTOMATH 项目于 1966 年在埃因霍温大学由 Nicolaas de Bruijn 启动，其目标是：

> 开发一种系统，以如此精确的方式书写完整的数学理论，使得对正确性的验证可以通过对文本的形式操作来进行。

引文来自 [de Bruijn, 1980]，该文对该项目进行了概述。关于在系统中呈现数学的示例，可以参见 [de Bruijn, 1973]。该小组的成就之一是对 Landau 的《基础》这本分析学教材的完整形式化。

**系统特性**

- **类型理论**：系统使用了一种基于“命题即类型”概念的**类型理论**来表示**经典或构造性数学**的逻辑。

- **技术创新**：

  - **证明的无关性**：在经典背景下讨论**证明的无关性**，这是 de Bruijn 对系统中**类型**和**命题（prop）**概念分离的原因之一，这一思想比计算机科学界的讨论（见第 7.1.2 节）早了几年。

  - **De Bruijn 索引**：在实现系统的过程中，发现识别仅在绑定变量名（α-转换）上不同的 λ 表达式是一个相当大的开销。[de Bruijn, 1972] 引入了一种非常有用的方法来绕过这个问题：

    - 用**数值索引**替换变量名。变量的出现被替换为从该出现到绑定它的 λ 之间的 λ 的数量。

    - 例如，λ 表达式：

      $$
      \lambda a . \lambda b . \lambda c . (a\ c)(b\ c)
      $$

      被替换为：

      $$
      \lambda . \lambda . \lambda . (2\ 0)(1\ 0)
      $$

    - 这种技术在函数式编程语言和逻辑的实现中都有应用，参见 [Huet, 1990a] 的第一部分。

**讨论**

- **系统的贡献**：AUTOMATH 项目在数学形式化和类型理论的发展中起到了重要作用，提供了早期的**计算机辅助证明系统**的模型。

- **变量绑定的处理**：De Bruijn 索引的引入解决了处理 λ 演算中变量绑定的一个主要问题，对后续的编程语言和证明助手的实现产生了深远影响。

---

### 9.1.5 类型理论的扩展

**背景**

系统 TT₀ 和 TT 可以被视为**简单类型 λ 演算**的扩展（参见第 2 章）。其他扩展也存在，我们在此考察其中两个最重要的。

#### 二阶 λ 演算（多态 λ 演算）

**起源**

- 由 [Reynolds, 1974] 和 [Girard, 1972] 独立发明，后者称其为**系统 F（System F）**。

- 关于该系统的简短而全面的介绍见 [Reynolds, 1990]。

**特性**

- **类型变量**：引入类型变量 $\alpha, \beta, \ldots$，允许编写多态函数。

- **类型抽象和应用**：引入类型抽象操作符 $\Lambda$ 和类型构造 $\Pi$，使得隐式的全称量化显式化。

- **示例**：

  - 函数 $K \equiv \lambda x . \lambda y . x$ 可以赋予类型 $\alpha \rightarrow \beta \rightarrow \alpha$。

  - 通过类型抽象，可以定义：

    $$
    K \equiv \Lambda \alpha . \Lambda \beta . \lambda x^\alpha . \lambda y^\beta . x : \Pi \alpha . \Pi \beta . (\alpha \rightarrow \beta \rightarrow \alpha)
    $$

- **类型系统**：

  - 如果 $e : t$，那么表达式 $\Lambda \alpha . e$ 对类型变量 $\alpha$ 进行参数化，类型为 $\Pi \alpha . t$。

  - 类型抽象可以像普通的抽象一样应用，满足：

    $$
    (\Lambda \alpha . e) \ \xi : t[\xi / \alpha]
    $$

    其中 $\xi$ 是任意类型表达式。

**讨论**

- **表达能力**：该系统比 Milner 类型系统更强，因为类型可以包含嵌入的全称量化，例如：

  $$
  (\Pi \alpha . (\alpha \rightarrow \alpha)) \rightarrow (\Pi \alpha . (\alpha \rightarrow \alpha))
  $$

- **循环性**：该类型位于 $\Pi \alpha$ 的量化域中，因此存在定义上的循环性。相比之下，TT 中的类型：

  $$
  ((\forall \alpha : U_0).(\alpha \rightarrow \alpha)) \rightarrow ((\forall \alpha : U_0).(\alpha \rightarrow \alpha))
  $$

  其定义不是循环的，因为类型位于 $U_1$ 中，而不在 $U_0$ 中，因此在量化符号 $(\forall \alpha : U_0)$ 的范围之外。

- **一致性和强规范化**：尽管存在循环性，但可以证明该系统是一致的并且具有强规范化性质，具有二阶算术的证明论强度，比 TT₀ 更具表达能力。这些结果可在 [Girard, Lafont, and Taylor, 1989] 中找到。

- **语义问题**：关于该演算的语义存在一些微妙的问题，特别是关于类型的循环性和一致性，在 [Huet, 1990a] 的第二部分有详细讨论。

#### 构造演算（Calculus of Constructions）

**起源**

- 由 Thierry Coquand 和 Gérard Huet 在 [Coquand and Huet, 1985] 中引入。

- 在 [Huet, 1990b] 中与其他类型理论进行比较。

**特性**

- **类型操作的定义**：允许直接定义类型操作，例如从 $A$ 和 $B$ 构造：

  $$
  \Pi C . ((A \rightarrow C) \rightarrow (B \rightarrow C) \rightarrow C)
  $$

- **避免逻辑不一致性**：通过不引入新的类型 $\text{Prop}$ 来避免逻辑不一致性，这是 [Girard, 1972] 中讨论的问题。

- **简洁的基础理论**：允许在一个非常简单的基础理论中发展大量数学。

**讨论**

- **存在类型的定义**：一个需要注意的点是，语言中似乎无法定义**强存在类型**，这体现在消除规则中**证明对象和类型的混合方式**上。

- **实现性语义**：关于构造演算的实现性语义的讨论可以在 [Huet, 1990a] 的第三部分找到。

---

## 9.2 结语

从最后两章的内容可以看出，对类型理论的研究仍在发展中。尽管如此，系统的核心已被证明是**逻辑和函数式编程的一个健全而优雅的结合**，我希望读者能分享我对这一主题的热情。

**反思**

- **知识的联系**：能够在以前未曾关联的领域中发现联系，是最令人兴奋的智力享受之一。我仍然记得，当我发现希尔伯特风格逻辑中**推导定理的证明**与 λ 演算中的**括号抽象算法**完全相同时的激动。这一结果用于函数式编程语言的实现，是我多年后在计算环境中遇到的。

- **添加特性的代价**：反思第 7 章的内容，一个不可避免的负面结论是，尽管可以对系统进行扩展，但每次添加特性似乎都要付出代价，要么增加系统的复杂性，要么改变其元数学性质。因此，我们应该在添加特性之前非常确定其必要性。

- **编程语言的教训**：这个结论对于熟悉大多数编程语言的人来说是显而易见的，在这些语言中，各种特性杂乱地共存，导致了不稳定的状况。这可能是我们需要自己去学习的东西。

**展望**

- **命令式编程的困境**：尽管理论上完全可以为用 Pascal 和 C 编写的程序开发证明，但实际上并未发生。

- **函数式编程的优势**：函数式语言提供了更光明的前景，因为它们清晰的语义使得证明更短、更高层次。

- **类型理论的潜力**：下一步似乎是转向像类型理论这样的系统，它可以为**程序开发、变换和验证**活动提供单一的基础，就像 LISP 机器支持集成调试、性能分析等一样。

- **未来的发展**：尽管仍有许多工作要做，以使类型理论成为一个可用且有吸引力的系统，支持**大型编程**，但我相信，基于类型理论的语言将在几年后像现在基于简单类型 λ 演算的函数式语言（如 Miranda 和 ML）一样受欢迎。

---

## 参考文献

- **[Bates and Constable, 1985]** Bates, J. L., & Constable, R. L. (1985). "Proofs as Programs." *ACM Transactions on Programming Languages and Systems*, 7(1), 113–136.

- **[Constable et al., 1986a]** Constable, R. L., et al. (1986). *Implementing Mathematics with the Nuprl Proof Development System*. Prentice Hall.

- **[Constable, Knoblock, and Bates, 1984]** Constable, R. L., Knoblock, T. B., & Bates, J. L. (1984). "Writing programs that construct proofs." *Journal of Automated Reasoning*, 1(3), 285–326.

- **[Henson and Turner, 1988]** Henson, C., & Turner, R. (1988). "A Theory of Types for Program Development." *Mathematical Structures in Computer Science*, 1(1), 45–70.

- **[Henson, 1989]** Henson, C. (1989). "Program development in a constructive theory of types and kinds." *Mathematical Structures in Computer Science*, 2(1), 41–70.

- **[Henson, 1991]** Henson, C. (1991). "A realizability interpretation for a constructive theory of types and kinds." *Journal of Symbolic Logic*, 56(1), 274–293.

- **[Hayashi and Nakano, 1988]** Hayashi, S., & Nakano, H. (1988). *PX: A Computational Logic*. Research Institute of Mathematical Sciences, Kyoto University.

- **[Hayashi, 1990]** Hayashi, S. (1990). "PX: A Computational Logic." In *Category Theory and Computer Science*, Springer.

- **[Feferman, 1979]** Feferman, S. (1979). "Constructive theories of functions and classes." In *Logic Colloquium '78*, North-Holland.

- **[Scott, 1979]** Scott, D. S. (1979). "Constructive Validity." In *Symposia Mathematica*, Vol. 21, Academic Press.

- **[de Bruijn, 1972]** de Bruijn, N. G. (1972). "Lambda calculus notation with nameless dummies: a tool for automatic formula manipulation." *Indagationes Mathematicae*, 34(5), 381–392.

- **[de Bruijn, 1973]** de Bruijn, N. G. (1973). "The mathematical language AUTOMATH, its usage and some of its extensions." In *Symposium on Automatic Demonstration*, Springer.

- **[de Bruijn, 1980]** de Bruijn, N. G. (1980). "A survey of the project AUTOMATH." In *To H. B. Curry: Essays on Combinatory Logic, Lambda Calculus and Formalism*, Academic Press.

- **[Huet, 1990a]** Huet, G. (Ed.). (1990). *Logical Frameworks*. Cambridge University Press.

- **[Reynolds, 1974]** Reynolds, J. C. (1974). "Polymorphic lambda calculus and semantic models." *Proceedings of the ACM Conference on Proving Assertions about Programs*.

- **[Girard, 1972]** Girard, J.-Y. (1972). "Interprétation fonctionnelle et élimination des coupures dans l'arithmétique d'ordre supérieur." Ph.D. thesis, Université de Paris VII.

- **[Reynolds, 1990]** Reynolds, J. C. (1990). *Theories of Programming Languages*. Cambridge University Press.

- **[Girard, Lafont, and Taylor, 1989]** Girard, J.-Y., Lafont, Y., & Taylor, P. (1989). *Proofs and Types*. Cambridge University Press.

- **[Coquand and Huet, 1985]** Coquand, T., & Huet, G. (1985). "The Calculus of Constructions." *Information and Computation*, 76(2-3), 95–120.

- **[Huet, 1990b]** Huet, G. (1990). "The Calculus of Constructions." In *Logical Frameworks*, Cambridge University Press.

---

希望以上详细的解读和总结能够满足您的要求。如果您还有其他问题或需要进一步的说明，请随时告诉我。

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