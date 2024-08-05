

[toc]

```
A Timeline for Logic, λ-Calculus, and Programming Language Theory

Dana S. Scott 
University Professor Emeritus Carnegie Mellon University 
Visiting Scholar 
University of California, Berkeley 
dana.scott@cs.cmu.edu
Taken from talks prepared for: 
TURING CENTENNIAL CELEBRATION Princeton University, May 10-12, 2012 
ACM TURING CENTENARY CELEBRATION San Francisco, June 15-16, 2012 
```



### 非常繁忙的十年

#### 1930年代的重要发展

1. **组合逻辑（Combinatory Logic）**
   - **Curry（1930-32）**
   - **解释**：组合逻辑是一种不依赖变量的函数计算方法。Haskell B. Curry提出了这一理论，用于消除变量在函数表达中的作用。它为λ演算和函数式编程提供了理论基础。

2. **赫尔布兰德定理（Herbrand’s Theorem）**
   - **Herbrand（1930）**
   - **解释**：赫尔布兰德定理是关于逻辑公式的一种定理，用于证明在某些情况下公式的可满足性。它在自动定理证明中具有重要作用。

3. **完备性证明（Completeness Proof）**
   - **Gödel（1930）**
   - **解释**：哥德尔证明了所有在一阶逻辑系统中是有效的命题都可以被证明。这意味着所有在系统内可验证的真理都可以通过逻辑推理得出。

4. **部分一致性证明（Partial Consistency Proof）**
   - **Herbrand（1931）**
   - **解释**：赫尔布兰德提供了一种部分一致性的证明，尽管没有完全解决一致性问题，但为逻辑系统的研究提供了重要的见解。

5. **不完全性定理（Incompleteness Theorem）**
   - **Gödel（1931）**
   - **解释**：哥德尔的不完全性定理表明，在任何一致的形式系统中，总存在一些命题是既无法被证明为真，也无法被证明为假的。这揭示了形式系统的局限性。

6. **无类型λ演算（Untyped λ-calculus）**
   - **Church（1932-33-41）**
   - **解释**：Alonzo Church提出的无类型λ演算是描述函数定义和应用的一种形式系统，是计算理论和编程语言设计的基础。

7. **原始递归的研究（Studies of Primitive Recursion）**
   - **Péter（1932-36）**
   - **解释**：Rózsa Péter研究了递归函数，这些函数在定义上允许调用自身，为计算理论中的递归概念奠定了基础。

8. **非标准模型（Non-standard Models）**
   - **Skolem（1933）**
   - **解释**：Thoralf Skolem提出了非标准模型的概念，表明数学系统可能存在非直观的模型，挑战了传统的数学观念。

9. **组合逻辑中的函数性（Functionality in Combinatory Logic）**
   - **Curry（1934）**
   - **解释**：Curry进一步研究了组合逻辑中的函数性，深入探讨了如何通过组合子表示复杂的函数。

10. **《数学基础》（Grundlagen der Mathematik）**
    - **Hilbert-Bernays（1934-39）**
    - **解释**：这本书系统地研究了数学逻辑的基础，由David Hilbert和Paul Bernays编写，对数学和逻辑学研究产生了深远影响。

11. **自然演绎法（Natural Deduction）**
    - **Gentzen（1934）**
    - **解释**：Gerhard Gentzen提出了一种新的逻辑推理方法，称为自然演绎法，使得逻辑证明过程更为直观和系统化。

12. **数论一致性和ε-归纳（Number-theoretic Consistency & ε-induction）**
    - **Gentzen（1934）**
    - **解释**：Gentzen在数论中提出了关于一致性和ε-归纳的研究，为证明数论的无矛盾性提供了方法。

13. **Church系统的不一致性（Inconsistency of Church’s System）**
    - **Kleene-Rosser（1936）**
    - **解释**：Stephen Kleene和J.B. Rosser发现了Alonzo Church提出的逻辑系统中的不一致性问题，这对逻辑系统的健全性提出了挑战。

14. **汇合定理（Confluence Theorem）**
    - **Church-Rosser（1936）**
    - **解释**：Church和Rosser证明了在λ演算中，归约顺序不会影响最终结果，这意味着不同的计算路径最终会收敛到相同的结果。

15. **有限组合过程（Finite Combinatory Processes）**
    - **Post（1936）**
    - **解释**：Emil Post研究了有限状态自动机和递归函数，提出了关于有限组合过程的理论。

16. **图灵机（Turing Machines）**
    - **Turing（1936-37）**
    - **解释**：Alan Turing发明了图灵机，这是一种抽象的计算模型，为现代计算机科学奠定了理论基础。

17. **递归不可判定性（Recursive Undecidability）**
    - **Church-Turing（1936）**
    - **解释**：Church和Turing证明了一些问题无法通过算法来解决，提出了递归不可判定性的概念。

18. **一般递归函数（General Recursive Functions）**
    - **Kleene（1936）**
    - **解释**：Stephen Kleene进一步研究了递归函数的性质，为计算理论的发展提供了重要的理论基础。

19. **进一步的完备性证明（Further Completeness Proofs）**
    - **Maltsev（1936）**
    - **解释**：Malcev在逻辑系统中提供了更多的完备性证明，进一步验证了形式逻辑系统的完备性。

20. **改进的不完全性定理（Improving Incompleteness Theorems）**
    - **Rosser（1936）**
    - **解释**：J.B. Rosser提出了哥德尔不完全性定理的改进版本，增加了定理的适用范围和精确性。

21. **固定点组合子（Fixed-point Combinator）**
    - **Turing（1937）**
    - **解释**：Turing介绍了固定点组合子的概念，允许定义可以调用自身的函数，对递归函数的定义产生了重大影响。

22. **计算性和λ定义（Computability and λ-definability）**
    - **Turing（1937）**
    - **解释**：Turing研究了可计算函数和λ演算的关系，进一步探索了计算的本质。

### 总结

从1930年代开始，以Gödel和Turing为代表的学者们在逻辑学、计算理论和数学基础方面做出了许多开创性的工作。这些研究为现代计算机科学的发展提供了坚实的理论基础。理解和应用这些成果需要大量的时间和努力，但它们的影响深远且广泛。

### 1940年代以来发生了什么？

#### 1940年代
- **简单类型论与λ演算（Simple Type Theory & λ-Calculus）**
  - **Church（1940）**
  - **解释**：Alonzo Church进一步发展了λ演算，将其扩展为包含类型的系统，这为形式逻辑和计算理论提供了更丰富的结构。

- **原始递归函数（Primitive Recursive Functionals）**
  - **Gödel（1941-58）**
  - **解释**：Kurt Gödel研究了原始递归函数，提出了一些关键性的定理，为计算理论和数理逻辑提供了重要的基础。

- **第二次世界大战（WW II）**
  - **1940年代**
  - **解释**：战争期间，计算机科学的研究几乎停滞，很多学者被战争动员。然而，战争结束后，计算机科学迅速发展。

- **递归层次（Recursive Hierarchies）**
  - **Kleene（1943）**
  - **解释**：Stephen Kleene研究了递归层次，为递归函数分类提供了理论基础。

- **范畴论（Theory of Categories）**
  - **Eilenberg-Mac Lane（1945）**
  - **解释**：Samuel Eilenberg和Saunders Mac Lane引入了范畴论，这是数学中一个新的抽象结构，广泛应用于代数拓扑和计算机科学。

- **新的完备性证明（New Completeness Proofs）**
  - **Henkin（1949-50）**
  - **解释**：Leon Henkin提供了新的完备性证明，扩展了逻辑系统的完备性理论。

#### 1950年代
- **计算与智能（Computing and Intelligence）**
  - **Turing（1950）**
  - **解释**：Alan Turing发表了关于机器智能的论文，提出了著名的图灵测试，探索了机器能否表现出智能行为的问题。

- **重新思考组合子（Rethinking Combinators）**
  - **Rosenbloom（1950）**
  - **解释**：Paul C. Rosenbloom重新审视了组合子理论，为其应用和扩展提供了新的视角。

- **IAS计算机（MANIAC）**
  - **von Neumann（1951）**
  - **解释**：John von Neumann设计了IAS计算机（也称为MANIAC），这是现代计算机的原型，采用了存储程序的概念。

- **元数学导论（Introduction to Metamathematics）**
  - **Kleene（1952）**
  - **解释**：Stephen Kleene的这本书系统地介绍了元数学，为形式逻辑和计算理论的研究奠定了基础。

- **IBM 701**
  - **Thomas Watson, Jr.（1952）**
  - **解释**：IBM 701是IBM的第一台商用计算机，由Thomas Watson, Jr.领导开发，标志着计算机商业化的开始。

- **算术谓词（Arithmetical Predicates）**
  - **Kleene（1955）**
  - **解释**：Stephen Kleene研究了算术谓词，提出了关于数理逻辑的重要定理。

- **FORTRAN**
  - **Backus等人（1956-57）**
  - **解释**：John Backus及其团队开发了FORTRAN，这是第一个广泛使用的高阶编程语言，主要用于科学计算。

- **ALGOL 58**
  - **Bauer等人（1958）**
  - **解释**：ALGOL 58是ALGOL编程语言的第一个版本，奠定了现代编程语言设计的基础。

- **LISP**
  - **McCarthy（1958）**
  - **解释**：John McCarthy开发了LISP，这是第一个用于人工智能研究的编程语言，以其强大的符号处理能力著称。

- **组合逻辑第一卷（Combinatory Logic. Volume I.）**
  - **Curry-Feys-Craig（1958）**
  - **解释**：Haskell B. Curry、Robert Feys和William Craig编写了这本书，系统地介绍了组合逻辑的基础理论。

- **伴随函子（Adjoint Functors）**
  - **Kan（1958）**
  - **解释**：Daniel M. Kan引入了伴随函子的概念，这是范畴论中的一个重要工具，广泛应用于数学和计算机科学中。

- **递归函数与量词I和II（Recursive Functionals & Quantifiers, I. & II.）**
  - **Kleene（1959-63）**
  - **解释**：Stephen Kleene研究了递归函数和量词，提出了关于计算理论的重要定理。

- **可数函数（Countable Functionals）**
  - **Kleene-Kreisel（1959）**
  - **解释**：Stephen Kleene和Georg Kreisel研究了可数函数，提出了关于计算理论的重要定理。

### 总结

从1940年代到1950年代，计算理论和数理逻辑领域经历了重要的发展。许多基础性的理论被提出和证明，这些研究奠定了现代计算机科学的基础。理解这些发展需要深入的数学和逻辑知识，但它们对现代计算机科学和技术的发展具有深远的影响。

### 1960年代的重要发展

#### 递归过程与ALGOL 60
- **递归过程（Recursive Procedures）**
  - **Dijkstra (1960)**: Edsger Dijkstra提出了在编程语言中引入递归过程的概念，这在后来的编程语言设计中成为了一个标准特性。递归过程允许函数调用自身，为解决复杂的算法问题提供了强大工具。

- **ALGOL 60**
  - **Backus等人 (1960)**: John Backus等人开发了ALGOL 60，这是一个极具影响力的编程语言，奠定了许多现代编程语言设计的基础。ALGOL 60引入了块结构、作用域规则和递归等关键概念。

#### 基本形式系统与Grothendieck拓扑
- **基本形式系统（Elementary Formal Systems）**
  - **Smullyan (1961)**: Raymond Smullyan研究了基本形式系统，这是一种用于形式逻辑和计算理论的系统，提供了对递归函数和计算过程的深入理解。

- **Grothendieck拓扑（Grothendieck Topologies）**
  - **M.Artin (1962)**: Michael Artin进一步发展了Grothendieck的拓扑理论，这是代数几何中的一个重要概念，用于研究代数簇的结构。

#### 高阶λ-定义与Grothendieck拓扑
- **高阶λ-定义（Higher-type λ-definability）**
  - **Kleene (1962)**: Stephen Kleene研究了高阶λ-定义，这是一种扩展的λ演算，用于定义更复杂的函数和计算过程。

- **Grothendieck拓扑（Grothendieck Topoi）**
  - **Grothendieck等人 SGA 4 (1963-64-72)**: Alexander Grothendieck及其同事在SGA 4中详细描述了拓扑的理论，进一步推动了代数几何的发展。

#### CPL与函子语义
- **CPL（Combined Programming Language）**
  - **Strachey等人 (1963)**: Christopher Strachey等人开发了CPL，这是一种通用的编程语言，影响了后来的编程语言设计。

- **函子语义（Functorial Semantics）**
  - **Lawvere (1963)**: William Lawvere引入了函子语义，这是范畴论中的一个重要概念，用于描述代数结构和逻辑系统。

#### 持续与伴随函子
- **持续（Continuations）**
  - **van Wijngaarden (1964)**: Adriaan van Wijngaarden研究了持续，这是编程语言中的一个重要概念，用于处理控制流和异常处理。

- **伴随函子与三元组（Adjoint Functors & Triples）**
  - **Eilenberg-Moore (1965)**: Samuel Eilenberg和John C. Moore研究了伴随函子和三元组，这是范畴论中的一个重要工具，用于描述代数结构和逻辑系统。

#### 笛卡尔封闭范畴与ISWIM
- **笛卡尔封闭范畴（Cartesian Closed Categories）**
  - **Eilenberg-Kelly (1966)**: Samuel Eilenberg和G. M. Kelly研究了笛卡尔封闭范畴，这是范畴论中的一个重要概念，用于描述函数类型和高阶函数。

- **ISWIM & SECD机器（ISWIM & SECD Machine）**
  - **Landin (1966)**: Peter Landin开发了ISWIM，这是一个早期的函数式编程语言，SECD机器则是一个用于解释ISWIM程序的虚拟机。

#### CUCH与组合子编程
- **CUCH与组合子编程（CUCH & Combinatory Programming）**
  - **Böhm (1966)**: Corrado Böhm研究了组合子编程，这是函数式编程中的一个重要概念，用于构造不需要变量的函数表达式。

#### 递归理论的新基础与规范化定理
- **递归理论的新基础（New Foundations of Recursion Theory）**
  - **Platek (1966)**: Richard Platek提出了递归理论的新基础，提供了对递归函数和计算过程的深入理解。

- **规范化定理（Normalization Theorem）**
  - **Tait (1967)**: William W. Tait提出了规范化定理，这是λ演算中的一个重要结果，证明了所有可计算的函数都可以被规范化。

#### AUTOMATH与依赖类型
- **AUTOMATH与依赖类型（AUTOMATH & Dependent Types）**
  - **de Bruijn (1967)**: Nicolaas Govert de Bruijn开发了AUTOMATH，这是一个早期的自动定理证明系统，依赖类型则是其中的重要概念，用于描述函数的输入输出之间的关系。

#### 有限类型可计算函数与ALGOL 68
- **有限类型可计算函数（Finite-type Computable Functionals）**
  - **Gandy (1967)**: Robin Gandy研究了有限类型可计算函数，这是计算理论中的一个重要概念。

- **ALGOL 68**
  - **van Wijngaarden (1968)**: Adriaan van Wijngaarden开发了ALGOL 68，这是一种通用的编程语言，影响了后来的编程语言设计。

#### 范畴论与类型化域逻辑
- **范畴论（Category of Sets）**
  - **Lawvere (1969)**: William Lawvere研究了集合的范畴，这是范畴论中的一个基本概念，用于描述集合及其之间的关系。

- **类型化域逻辑（Typed Domain Logic）**
  - **Scott (1969-93)**: Dana Scott研究了类型化域逻辑，这是计算理论中的一个重要概念，用于描述函数和计算过程。

- **基于范畴论的λ模型（Domain-theoretic λ-models）**
  - **Scott (1969)**: Dana Scott提出了基于范畴论的λ模型，为函数式编程提供了数学基础。

#### 公式即类型
- **公式即类型（Formulae-as-Types）**
  - **Howard (1969 -1980)**: William A. Howard提出了公式即类型的概念，这是类型论中的一个重要结果，表明逻辑公式和类型之间存在对应关系。

- **基础中的伴随性（Adjointness in Foundations）**
  - **Lawvere (1969)**: William Lawvere研究了基础中的伴随性，这是范畴论中的一个重要工具，用于描述代数结构和逻辑系统。

#### 笛卡尔封闭范畴的应用
- **定理**
  - **笛卡尔封闭范畴**：笛卡尔封闭范畴为类型λ演算提供了代数版本，描述了函数类型和高阶函数的结构。

以上这些研究和发现为计算理论、编程语言设计以及逻辑系统提供了重要的基础，推动了计算机科学的发展。



### 1970年代的重要发展

#### 继续与范畴逻辑
- **Continuations（继续）**
  - **Mazurkiewicz (1970)**: Andrzej Mazurkiewicz研究了继续，这是计算机科学中的一个重要概念，用于描述控制流和异常处理。
  - **F. Lockwood Morris (1970)**: F. Lockwood Morris进一步研究了继续的应用，特别是在编程语言设计中的作用。
  - **Wadsworth (1970)**: Christopher Wadsworth探讨了继续在函数式编程中的具体实现，提出了实际的应用示例。

- **范畴逻辑（Categorical Logic）**
  - **Joyal (1970+)**: André Joyal在1970年代开始研究范畴逻辑，这是一种将逻辑与范畴论结合起来的方法，为形式化逻辑提供了新的视角。

#### 初等拓扑与指称语义
- **初等拓扑（Elementary Topoi）**
  - **Lawvere-Tierney (1970)**: William Lawvere和Myles Tierney研究了初等拓扑，这是代数几何中的一个重要概念，用于研究代数结构的拓扑性质。

- **指称语义（Denotational Semantics）**
  - **Scott-Strachey (1970)**: Dana Scott和Christopher Strachey提出了指称语义，这是编程语言理论中的一个重要分支，用于形式化描述编程语言的语义。

#### 封闭范畴中的一致性与量词与层
- **封闭范畴中的一致性（Coherence in Closed Categories）**
  - **Kelly (1971)**: G. M. Kelly研究了封闭范畴中的一致性，这是范畴论中的一个重要问题，涉及如何确保不同路径得到的结果一致。

- **量词与层（Quantifiers and Sheaves）**
  - **Lawvere (1971)**: William Lawvere研究了量词与层的关系，为代数几何和逻辑提供了新的工具。

#### 马丁-洛夫类型论与系统F
- **马丁-洛夫类型论（Martin-Löf Type Theory）**
  - **Martin-Löf (1971)**: Per Martin-Löf提出了马丁-洛夫类型论，这是构造性数学中的一个重要系统，用于形式化描述数学证明。

- **系统F和Fω（System F, Fω）**
  - **Girard (1971)**: Jean-Yves Girard提出了系统F和Fω，这是多态λ演算中的两个重要系统，用于研究类型和计算。

#### 计算函数逻辑与从层到逻辑
- **计算函数逻辑（Logic for Computable Functions）**
  - **Milner (1972)**: Robin Milner研究了计算函数的逻辑，这是编程语言理论中的一个重要分支，用于形式化描述可计算函数的行为。

- **从层到逻辑（From Sheaves to Logic）**
  - **Reyes (1974)**: Gonzalo Reyes研究了从层到逻辑的转换，这是代数几何中的一个重要问题。

#### 多态λ-演算与按需调用与按值调用
- **多态λ-演算（Polymorphic λ-Calculus）**
  - **Reynolds (1974)**: John Reynolds提出了多态λ-演算，这是函数式编程中的一个重要系统，用于研究多态类型和计算。

- **按需调用与按值调用（Call-by-Name, Call-by-Value）**
  - **Plotkin (1975)**: Gordon Plotkin研究了按需调用和按值调用，这是编程语言中的两个重要计算策略。

#### 模型过程与函数式编程与FP
- **模型过程（Modeling Processes）**
  - **Milner (1975)**: Robin Milner研究了模型过程，这是形式化描述计算过程的一个重要工具。

- **SASL与Scheme**
  - **Turner (1975)**: David Turner开发了SASL，这是一个函数式编程语言。
  - **Sussman-Steele (1975-80)**: Gerald Jay Sussman和Guy L. Steele开发了Scheme，这是一个基于LISP的函数式编程语言。

- **函数式编程与FP（Functional Programming & FP）**
  - **Backus (1977)**: John Backus提出了函数式编程的概念，并开发了FP，这是一个早期的函数式编程语言。

#### 一阶范畴逻辑与爱丁堡LCF
- **一阶范畴逻辑（First-order Categorical Logic）**
  - **Makkai-Reyes (1977)**: Michael Makkai和Gonzalo Reyes研究了一阶范畴逻辑，这是范畴论中的一个重要分支，用于形式化描述逻辑系统。

- **爱丁堡LCF（Edinburgh LCF）**
  - **Milner等人 (1978)**: Robin Milner等人开发了爱丁堡LCF，这是一个基于逻辑的编程语言，用于形式化证明。

#### 交叉类型与ML
- **交叉类型（Intersection Types）**
  - **Coppo-Dezani (1978)**: Mario Coppo和Mariangiola Dezani提出了交叉类型，这是类型理论中的一个重要概念，用于描述类型的交叉和组合。

- **ML**
  - **Milner等人 (1979)**: Robin Milner等人开发了ML，这是一个基于函数式编程的语言，广泛用于学术研究和实际应用。

#### *-自主范畴与层和逻辑
- **\*-自主范畴（*-Autonomous Categories）**
  - **Barr (1979)**: Michael Barr研究了\*-自主范畴，这是范畴论中的一个重要概念，用于描述代数结构和逻辑系统。

- **层和逻辑（Sheaves and Logic）**
  - **Fourman-Scott (1979)**: Michael Fourman和Dana Scott研究了层和逻辑的关系，为代数几何和逻辑提供了新的工具。

#### 1970年代的重要发展总结
1970年代的重要研究突出了构造性逻辑的应用、编程语言设计和语义学的联系，以及范畴论的应用。这些发展为现代计算机科学奠定了坚实的理论基础，并推动了编程语言和逻辑系统的进步。



### 1980年代：逻辑和类型论的关键发展

1980年代见证了类型论、逻辑及其在计算机科学应用中的显著进展。以下是这一时期的主要成就和概念的详细介绍：

1. **弗雷格结构** (Aczel, 1980)
   - **解释**：弗雷格结构为理解自然语言和数学逻辑的语义提供了一个正式框架，以逻辑学家戈特洛布·弗雷格命名。
   - **重要性**：这些结构有助于以逻辑和结构化的方式表示表达式的含义，影响了后来的形式语义学和类型论的发展。

2. **HOPE** (Burstall et al., 1980)
   - **解释**：HOPE 是一种支持模式匹配的函数式编程语言，是早期实现代数数据类型和惰性求值的语言之一。
   - **重要性**：它影响了后来的函数式编程语言的设计，如 Haskell。

3. **Lambda演算书** (Barendregt, 1981-84)
   - **解释**：Henk Barendregt 对 lambda 演算的综合研究提供了该领域的广泛参考。
   - **重要性**：它仍然是理解计算机科学和数学中 lambda 演算理论和应用的基础性文本。

4. **结构操作语义** (Plotkin, 1981)
   - **解释**：结构操作语义 (SOS) 是一种通过描述语句执行如何改变计算系统状态来精确定义编程语言语义的框架。
   - **重要性**：SOS 已成为定义和分析编程语言语义的标准方法。

5. **有效拓扑** (Hyland, 1982)
   - **解释**：有效拓扑是一种反映数学证明计算内容的建设性数学和逻辑范畴。
   - **重要性**：它在范畴论和建设性数学之间架起了一座桥梁，影响了包含建设性原则的类型论的发展。

6. **依赖类型与模块化** (Burstall-Lampson, 1984)
   - **解释**：依赖类型允许数据结构的类型依赖于一个值，从而实现更精确的类型检查和验证。
   - **重要性**：这一概念对证明助手和 Agda、Coq 等语言的发展至关重要，这些工具利用依赖类型进行形式验证。

7. **局部笛卡尔闭合范畴与类型论** (Seely, 1984)
   - **解释**：局部笛卡尔闭合范畴 (LCCC) 提供了建模依赖类型和类型论的范畴框架。
   - **重要性**：LCCC 是理解类型论语义及其范畴解释的理论基础。

8. **构造演算** (Coquand-Huet, 1985)
   - **解释**：构造演算 (CoC) 是一种高阶类型的 lambda 演算，构成了证明助手 Coq 的基础。
   - **重要性**：CoC 将函数式编程与建设性逻辑结合，使正式验证软件的开发成为可能。

9. **有界量化** (Cardelli-Wegner, 1985)
   - **解释**：有界量化引入了对类型变量的约束，从而定义更灵活和可重用的多态函数。
   - **重要性**：这一概念对理解现代编程语言中的泛型编程和类型推断至关重要。

10. **NUPRL** (Constable et al., 1986)
    - **解释**：NUPRL 是基于建设性类型论的交互式定理证明器，支持已验证软件和数学证明的开发。
    - **重要性**：它对形式验证领域和其他证明助手的发展作出了贡献。

11. **高阶范畴逻辑** (Lambek-P.J.Scott, 1986)
    - **解释**：这种逻辑将一阶逻辑扩展到高阶函数和类型，提供了逻辑系统的范畴视角。
    - **重要性**：它影响了范畴逻辑的发展及其在计算机科学中的应用。

12. **剑桥LCF** (Paulson, 1987)
    - **解释**：可计算函数逻辑 (LCF) 是开发定理证明器的框架，允许创建证明策略和策略。
    - **重要性**：LCF 启发了许多现代证明助手，包括 Isabelle 和 HOL。

13. **线性逻辑** (Girard et al., 1987-89)
    - **解释**：线性逻辑是一种资源敏感的逻辑，跟踪证明中假设的使用，防止它们的任意复制或删除。
    - **重要性**：它在并发性、资源管理和编程语言语义等领域有广泛应用。

14. **HOL** (Gordon, 1988)
    - **解释**：HOL (高阶逻辑) 是支持高阶逻辑的交互式定理证明器，用于硬件和软件的形式验证。
    - **重要性**：HOL 在关键系统的验证中得到了广泛应用，并影响了其他定理证明器的发展。

15. **FORSYTHE** (Reynolds, 1988)
    - **解释**：FORSYTHE 是一种编程语言，旨在探索类型和编程结构的集成，强调简洁和表达能力。
    - **重要性**：它影响了后续编程语言和类型系统的设计。

16. **证明与类型** (Girard et al., 1989)
    - **解释**：这项工作探讨了证明与类型之间的关系，提供了对柯里-霍华德对应的基础性观点。
    - **重要性**：它加深了对逻辑和类型论相互作用的理解。

17. **逻辑与范畴类型的集成** (Gray, 1989)
    - **解释**：这项工作集成了逻辑和范畴对类型的视角，增强了对类型系统的理论理解。
    - **重要性**：它对类型论及其在编程语言中的应用做出了贡献。

18. **计算λ演算与单子** (Moggi, 1989)
    - **解释**：Eugenio Moggi 引入的单子为函数式编程中处理副作用提供了框架，导致了更简洁和模块化的代码。
    - **重要性**：单子已成为函数式编程中的核心概念，特别是在 Haskell 等语言中。

在1980年代，形式化和理解类型论、逻辑及其在计算机科学中的应用取得了重大进展。这些进展为许多现代编程语言和验证工具奠定了基础，使开发更可靠和形式化验证的软件系统成为可能。

### 1990年代以来的进展

#### HASKELL
**创立者：** Hudak、Hughes、Peyton Jones、Wadler (1990)
**解释：** Haskell 是一种纯函数式编程语言，广泛用于学术研究和工业界。它通过懒评估和强类型系统，实现了高效和安全的代码。

#### Higher-type recursion theory
**创立者：** Sacks (1990)
**解释：** 这是一种高阶递归理论，用于研究复杂的递归函数和结构。

#### STANDARD ML
**创立者：** Milner 等人 (1990-1997)
**解释：** Standard ML 是一种函数式编程语言，具有强类型和模块化特性，广泛用于编译器和形式验证中。

#### Lazy λ-calculus
**创立者：** Abramsky (1990)
**解释：** 懒 λ 演算是 λ 演算的一种变体，延迟计算表达式，直到需要其值时才进行求值。

#### Higher-order subtyping
**创立者：** Cardelli、Longo (1991)
**解释：** 研究高阶类型的子类型关系，促进了面向对象语言中的类型系统发展。

#### Categories, Types and Structure
**创立者：** Asperti、Longo (1991)
**解释：** 研究范畴、类型和结构之间的关系，推动了范畴论在计算机科学中的应用。

#### STANDARD ML of NJ
**创立者：** MacQueen、Appel (1991-1998)
**解释：** 标准ML的实现版本之一，强调编译器优化和性能。

#### QUEST
**创立者：** Cardelli (1991)
**解释：** 研究编程语言的语义和类型系统。

#### Edinburgh LF
**创立者：** Harper 等人 (1992)
**解释：** 爱丁堡逻辑框架，提供了形式化推理的基础。

#### Pi-Calculus
**创立者：** Milner、Parrow、Walker (1992)
**解释：** π演算是一种用于描述并发系统的计算模型。

#### Categorical combinators
**创立者：** Curien (1993)
**解释：** 研究范畴论中的组合子，促进了计算模型的发展。

#### Translucent types & modularity
**创立者：** Harper、Lillibridge (1994)
**解释：** 提出了半透明类型和模块化编程的概念，增强了软件组件的可重用性。

#### Full abstraction for PCF
**创立者：** Hyland-Ong/Abramsky 等人 (1995)
**解释：** 研究PCF（编程计算函数）的全抽象性，确保语言的语义和实现一致。

#### Algebraic set theory
**创立者：** Joyal、Moerdijk (1995)
**解释：** 研究代数集论，促进了集合论在计算机科学中的应用。

#### Object Calculus
**创立者：** Abadi、Cardelli (1996)
**解释：** 研究面向对象编程语言的计算模型。

#### Typed intermediate languages
**创立者：** Tarditi、Morrisett 等人 (1996)
**解释：** 研究类型化的中间语言，提高编译器的安全性和优化能力。

#### Proof-carrying code
**创立者：** Necula、Lee (1996)
**解释：** 通过附带证明的代码确保软件的安全性和正确性。

#### Computability and totality in domains
**创立者：** Berger (1997)
**解释：** 研究域中的可计算性和完备性，促进了函数式编程的理论基础。

#### Typed assembly language
**创立者：** Morrisett 等人 (1998)
**解释：** 研究类型化汇编语言，增强了低级代码的安全性和可靠性。

#### Type theory via exact categories
**创立者：** Birkedal 等人 (1998)
**解释：** 通过精确范畴研究类型论，促进了范畴论和类型论的结合。

#### Categorification
**创立者：** Baez (1998)
**解释：** 研究范畴化，将集合论概念推广到范畴论中，增强了数学结构的表达能力。

这些发展使抽象概念在语言实现和编译中找到了许多应用。

## The New Millennium

### 2000年及以后的发展

#### 1. 预基类型拓扑（Predicative Topos）
**Moerdijk-Palmgren (2000)**
预基类型拓扑是由Ieke Moerdijk和Erik Palmgren提出的，主要研究如何在不涉及全称量化的前提下，定义和使用拓扑结构。这项工作在类型论的基础上进一步拓展，为数学逻辑和拓扑提供了新的视角和工具。

#### 2. 象的草图（Sketches of an Elephant）
**Johnstone (2002+)**
Peter Johnstone的《象的草图》是一部关于拓扑和范畴论的书，涵盖了广泛的主题和复杂的数学结构。这部书为数学家提供了详细的工具和方法，用于研究和应用这些理论。

#### 3. 微分λ演算（Differential λ-calculus）
**Ehrhard/Regnier (2003)**
微分λ演算是由Thomas Ehrhard和Laurent Regnier提出的一种扩展的λ演算，通过引入微分算子，使得在λ演算中能够处理更复杂的计算和变化。这在程序分析和优化中有重要应用。

#### 4. 模块化结构操作语义学（Modular Structural Operational Semantics）
**Mosses (2004)**
Peter Mosses提出的模块化结构操作语义学（MSOS）是一种新的语义学方法，旨在通过模块化设计，使得语义规则更易于理解、扩展和组合。这对于编程语言设计和分析有重要的意义。

#### 5. 用于实分析的λ演算（A λ-calculus for Real Analysis）
**Taylor (2005+)**
Paul Taylor提出了一种用于实数分析的λ演算，探索如何在计算机科学和数学的交叉点上使用λ演算来处理实数和连续函数的问题。

#### 6. 同伦类型论（Homotopy Type Theory, HoTT）
**Awodey-Warren (2006)**
同伦类型论（HoTT）由Steve Awodey和Michael Warren提出，结合了同伦论和类型论的概念，形成了一种新的数学基础理论。HoTT通过类型等价来处理同伦类型，为数学家提供了强大的工具来研究几何和拓扑问题。

#### 7. 单值性公理（Univalence Axiom）
**Voevodsky (2006+)**
Vladimir Voevodsky提出的单值性公理（Univalence Axiom）是同伦类型论的核心内容之一。该公理表明，在类型论中等价的类型可以相互替代，这为数学证明和计算提供了新的视角和工具。

#### 8. 安全λ演算（The Safe λ-calculus）
**Ong, et al. (2007)**
C.-H. Luke Ong等人提出的安全λ演算研究如何在λ演算中确保程序的安全性和正确性，这对于程序验证和安全性分析具有重要意义。

#### 9. 高阶拓扑理论（Higher Topos Theory）
**Lurie (2009)**
Jacob Lurie的高阶拓扑理论扩展了传统拓扑学，研究高阶结构和拓扑空间的性质，为现代数学提供了新的框架和工具。

#### 10. 函数式反应编程（Functional Reactive Programming, FRP）
**Hudak, et al. (2010)**
Paul Hudak等人提出的函数式反应编程（FRP）是一种编程范式，专注于处理动态和时间变化的数据流。FRP在交互系统、实时控制和动画中有广泛应用。

#### 11. 单值基础计划和HoTT书（Univalent Foundations Program & HoTT Book）
**Voevodsky, et al. (2012-13)**
Vladimir Voevodsky等人开展的单值基础计划和HoTT书项目，旨在通过HoTT和单值性公理，建立新的数学基础。这些工作推动了数学和计算机科学的深度融合。

### 总结
从λ演算到类型论，从同伦类型论到函数式反应编程，20世纪和21世纪初的这些研究和发现，不仅深刻地影响了计算机科学和数学，还推动了两者之间的跨学科交流和发展。这些理论为现代编程语言的设计、程序分析和证明提供了强大的工具和方法。







---

```

```

