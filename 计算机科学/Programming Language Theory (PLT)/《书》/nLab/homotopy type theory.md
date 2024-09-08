[toc]



以下是对每个部分的详细解释：

### 1. Idea (基本思想)

Homotopy Type Theory (HoTT) 是一种基于类型论的数学基础，结合了同伦论和范畴论的思想。HoTT的核心思想是通过同伦的观点将类型解释为空间，将等式解释为路径，并通过这些结构研究数学对象的同伦性质。这种方法为传统的集合论基础提供了一种替代方案。与传统数学不同，HoTT 强调的是类型及其等价性，而不仅仅是集合中的元素。这种方式使得数学中的等价性问题可以通过类型系统自然地进行处理。

HoTT 的一个重要特性是它与高阶同伦论（尤其是 $(\infty,1)$-范畴论）紧密相关，特别是通过同伦层级来表示高阶等价性。这个想法为数学和计算机科学中的形式化和证明助理工具（如Coq）提供了基础。

### 2. Properties (性质)

#### 2.1 Advantages (优势)

HoTT的优势主要在于以下几点：

- **处理等价性的能力**：传统数学通常依赖于集合中的等同性，而HoTT直接处理路径和等价性，使得更自然地表示同伦空间的结构成为可能。
  
- **计算机辅助证明**：HoTT 的公理化体系与现代证明助理（如Coq）兼容，使得通过计算机进行数学的形式化成为可能。它为数学证明提供了机械化的验证手段。

- **统一的基础**：HoTT 通过类型论和同伦论统一了许多数学领域，为不同学科提供了一个共同的框架。

#### 2.2 Models in $(\infty,1)$-categories and $(\infty,1)$-toposes

HoTT 与 $(\infty,1)$-范畴和 $(\infty,1)$-拓扑斯有着深刻的联系。在$(\infty,1)$-范畴中，对象可以看作是具有路径和高阶路径的空间，而 $(\infty,1)$-拓扑斯提供了与HoTT理论框架一致的几何模型。

- **$(\infty,1)$-范畴**：这是一种高阶范畴，在这种范畴中对象之间的态射不仅是映射，而且是路径，路径之间的变换可以继续构成高阶结构。

- **$(\infty,1)$-拓扑斯**：是$(\infty,1)$-范畴的一个特例，提供了表示空间及其同伦信息的几何语义模型。

#### 2.3 New Axioms (新公理)

HoTT 引入了一个新的公理，称为 **Univalence Axiom（不可区分公理）**，由Vladimir Voevodsky提出。这个公理表明等价类型在数学上可以视为相同的。这一公理在HoTT中起着核心作用，因为它允许我们通过类型等价来替代集合的等同性。

- **不可区分公理的作用**：它允许我们在类型等价的情况下认为两个类型是同一类型。这对于同伦论中的等价关系的处理至关重要。

#### 2.4 Relation to Representation Theory (与表示论的关系)

HoTT 和表示论的关系主要体现在通过类型系统来表示群的作用和同伦空间。HoTT 可以用来描述某些代数对象（如群）的同伦表示，特别是通过其高阶结构来分析这些对象之间的关系。表示论研究的是代数结构（如群、代数）的表示，而HoTT 为这些表示提供了一种新的基础和语义解释方式。

### 3. Machine Implementation (机器实现)

HoTT 的一个重要应用是通过证明助理工具（如Coq）进行实现和验证。通过HoTT，我们可以在计算机上实现复杂的数学证明。特别是HoTT在Coq中的实现（通常称为 **HoTT Coq**）允许数学家和计算机科学家利用类型系统验证复杂的同伦空间性质。

- **证明助理**：Coq是一种强大的工具，它结合了HoTT的理论，能够进行自动化的定理验证。通过在Coq中定义类型，我们可以形式化并证明复杂的数学理论。

- **机械化证明**：这种形式化证明的机械化允许更高程度的准确性和安全性，特别是在高阶数学证明中。

### 4. Dictionary HoTT-Coq / $(\infty,1)$-Category Theory (HoTT-Coq 与 $(\infty,1)$-范畴论的词典)

HoTT 和 $(\infty,1)$-范畴论之间的关系可以通过一种“词典”来理解，即两者之间的术语和概念可以相互转换：

- **类型（Type）** 对应于 $(\infty,1)$-范畴中的对象。
- **路径（Path）** 对应于态射。
- **等价（Equivalence）** 对应于 $(\infty,1)$-范畴中的弱等价。

通过这种词典，可以将 $(\infty,1)$-范畴论中的概念直接映射到HoTT的理论框架中，反之亦然。

### 5. Related Entries (相关条目)

#### 5.1 Proof Assistants (证明助理)

HoTT 与以下证明助理有密切关联：

- **Coq**：支持HoTT的实现，广泛应用于数学证明的形式化和验证。
- **Agda**：另一个基于类型论的证明助理，虽然其HoTT支持不如Coq，但仍然是一个重要工具。

#### 5.2 Projects for Formalization of Mathematics with Proof Assistants (使用证明助理形式化数学的项目)

以下是一些使用证明助理（如Coq）形式化数学的项目，这些项目与HoTT密切相关：

- **HoTT library**：这是Coq的一个库，专门用于HoTT的形式化和证明。
- **Lean**：一个用于形式化数学的证明助理，也逐渐支持HoTT的某些理论。
- **UniMath**：Voevodsky 提出的项目，旨在通过证明助理形式化数学。

### 6. References (参考文献)

#### 6.1 Early Development (早期发展)

HoTT 起源于同伦论与类型论的交汇，Vladimir Voevodsky 是这一领域的主要贡献者。他引入了不可区分公理（Univalence Axiom），这为同伦类型论奠定了基础。

- **Voevodsky's 2010 lectures**：Voevodsky 在2010年关于不可区分公理的讲座为HoTT的发展提供了基础。

#### 6.2 Introductions (介绍)

以下是一些HoTT的入门资源：

- **《Homotopy Type Theory: Univalent Foundations of Mathematics》**：这是HoTT领域的标准入门书籍，由Voevodsky和其他贡献者编写。
- **《HoTT book》**：HoTT 的入门教科书，涵盖了从基础理论到高级应用的内容。

#### 6.3 General (概述)

- **《Types and Spaces》**：一本将类型论与拓扑空间相联系的书，详细讨论了HoTT的核心思想。

#### 6.4 Models and Categorical Semantics (模型与范畴语义)

- **《Models for Homotopy Type Theory》**：详细探讨了HoTT的各种模型，特别是与 $(\infty,1)$-范畴的关联。
- **《Categorical Semantics for HoTT》**：研究HoTT在范畴语义下的解释。

#### 6.5 Syntax (语法)

HoTT的语法和语义在类型系统中紧密结合：

- **《Syntax and Semantics of Homotopy Type Theory》**：提供了HoTT的语法结构和范畴语义之间的联系。

#### 6.6 Code (代码)

- **HoTT Coq library**：HoTT 在Coq中的实现，包括许多核心理论的形式化代码。

#### 6.7 Homotopy Theory Formalized in Homotopy Type Theory (在同伦类型论中形式化的同伦论)

- **Formalizing Homotopy Theory in HoTT**：通过HoTT形式化传统的同伦论。

### --------------------