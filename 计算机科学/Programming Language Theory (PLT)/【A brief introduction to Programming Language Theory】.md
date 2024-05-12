



## 编程语言理论简介

大多数开发人员专注于学习“如何”编程，而不是消耗阐述底层理论基础的材料。以下对 PLT 的介绍不包括证明或构建深刻的数学直觉。相反，目标是熟悉核心概念和研究领域。

1. **[图灵完备性](https://en.wikipedia.org/wiki/Turing_completeness)。**此属性定义了被视为真正的编程语言所需的标准。编程语言是由一组用于将指令翻译给计算机的规则定义的[形式语言](https://en.wikipedia.org/wiki/Formal_language)。这些规则指定了与计算机通信的可接受的方式。图灵机是一种抽象计算机的数学模型，能够通过模拟其逻辑来实现任何算法。当一个可以模拟图灵机的系统也可以实现任何算法时——它被称为图灵完备。这通常要求语言能够拥有[状态](https://en.wikipedia.org/wiki/State_(computer_science))（即变量）和条件逻辑。根据这个定义，[ HTML](https://stackoverflow.com/questions/30719221/is-html-turing-complete)不是图灵完备的，但[lambda 演算](https://en.wikipedia.org/wiki/Lambda_calculus)是。
2. **[可判定性和停止问题](https://en.wikipedia.org/wiki/Halting_problem)。**艾伦·图灵在他的开创性工作中证明，没有通用算法可以用来确定程序是否会因所有可能的输入而终止（尽管我们可以为某些程序输入对的预期行为建模）。这称为停机问题。可判定问题是可以解决的问题。因此，停机问题是*不可判定的*。相反，可判定的问题是可以构建一种始终得出正确的是或否答案的算法的问题。
3. **[类型论](https://en.wikipedia.org/wiki/Type_theory)。**这是编程语言中类型系统的数学基础。它构成了编译器中类型检查算法的内部逻辑。类型理论框架允许我们评估和计算证明为什么某些类型应该或不应该存在于强类型语言中。类型是定义语言不同部分之间接口并确保程序一致连接的一种方式。在函数式编程中，类型理论表明类型应该表达编程语言要做什么（从这些类型具体化的 lambda 演算就是语言本身）。这意味着熟悉某些类型理论为更好地理解具有丰富类型系统的静态语言（例如 Haskell）奠定了基础。类型理论本质上是研究类型系统，它定义了语言的组织规则。[ Hindley-Milner 类型](http://dev.stephendiehl.com/fun/006_hindley_milner.html)系统是一个有趣的类型，它使用[统一](http://dev.stephendiehl.com/fun/006_hindley_milner.html#unification)为非类型化语法提供类型推断功能。
4. **[集合论](https://en.wikipedia.org/wiki/Set_theory)**。集合是对象的集合。这些集合具有运算（例如[交集、并集和补集](https://www.probabilitycourse.com/chapter1/1_2_2_set_operations.php)）。根据形式语言理论，语言是一*组*字符串。数据结构本身也可以被视为具有集合运算的各种实现的集合。集合论在概念上与编程相关的一种明确方式是通过关系数据库（其中数据库是集合上的关系）。
5. **[范畴论](https://en.wikipedia.org/wiki/Category_theory)**。范畴论概括了数学的几个分支。它通过提供通用的“元”语言来抽象地建模概念来实现这一点。这是非常有用的，因为它允许我们推理关系网络，而不是关心特定系统的细节。类别*是*对象的集合。这些对象之间的关系（例如组合或结合性）称为*[态射](https://mathworld.wolfram.com/Morphism.html)*。类别之间的关系称为*[函子](https://en.wikipedia.org/wiki/Functor)*。范畴论描述的几个概念在计算机科学中用于评估程序的正确性和简洁性。类别理论概念在纯函数范式中更加明显，其中计算由可以组合以构建复杂性的数学函数组成。
6. **[自动机理论](https://en.wikipedia.org/wiki/Automata_theory)**。计算机科学的这一领域定义了形式语言分析的基础，为编译器设计、解析器、语法和正则表达式奠定了理论基础。我们使用自动机对形式语言进行建模，自动机是从无限集合中提取的形式语言的有限表示。有限自动机是一种抽象机器，它通过状态移动来执行计算，根据形式语言定义的一组规则确定是否应该接受或拒绝给定的输入。[ Chomksy 层次结构](https://en.wikipedia.org/wiki/Chomsky_hierarchy)提供了一种将形式语言分组为连续更大的类的方法。





## A brief introduction to Programming Language Theory

Most developers focus on learning the “how” of programming rather than consuming materials that expound on underlying theoretical foundations. The following introduction to PLT does not include proofs or build deep mathematical intuition. The goal instead is to establish familiarity with core concepts and areas of study.

1. **[Turing completeness](https://en.wikipedia.org/wiki/Turing_completeness).** This property defines the criteria necessary for being considered a real programming language. A programming language is a [formal language](https://en.wikipedia.org/wiki/Formal_language) defined by a set of rules used to translate instructions to a computer. These rules specify acceptable ways to communicate with a computer. The Turing machine is a mathematical model of an abstract computer capable of implementing any algorithm by simulating its logic. When a system that can simulate the Turing machine can also implement any algorithm—it is known as Turing complete. This usually requires the language to be able to possess [state](https://en.wikipedia.org/wiki/State_(computer_science)) (i.e., variables) and conditional logic. By this definition, [HTML](https://stackoverflow.com/questions/30719221/is-html-turing-complete) is not Turing complete—but [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is.
2. **[Decidability and the halting problem](https://en.wikipedia.org/wiki/Halting_problem).** In his seminal work, Alan Turing proved that there is no general algorithm we can use to determine whether a program will terminate for all possible inputs (though we can model expected behaviors for some program-input pairs). This is known as the halting problem. A decidable problem is one that can be solved. Therefore, the halting problem is *undecidable*. Conversely, a decidable problem is one for which it is possible to construct an algorithm that always leads to a correct yes-or-no answer.
3. **[Type theory](https://en.wikipedia.org/wiki/Type_theory).** This is a strand of mathematics underlying type systems in programming languages. It forms the internal logic for type checking algorithms in compilers. A type-theoretic framework allows us to evaluate and computationally justify why certain types should or should not exist in a strongly typed language. Types are one way of defining interfaces between different parts of a language, and ensuring the program is connected consistently. In functional programming, type theory suggests that types should express what the programming language is to do (and the lambda calculus that materializes from these types is the language itself). This means that being acquainted with some type theory provides the foundation to better understand static languages with rich type systems (such as Haskell). Type theory essentially studies type systems, which define a language’s organizing rules. An interesting type system is the [Hindley-Milner Type System](http://dev.stephendiehl.com/fun/006_hindley_milner.html) which uses [unification](http://dev.stephendiehl.com/fun/006_hindley_milner.html#unification) to provide type inference capabilities for untyped syntax.
4. **[Set theory](https://en.wikipedia.org/wiki/Set_theory)**. Sets are collections of objects. These sets have operations (such as [intersection, union and complement](https://www.probabilitycourse.com/chapter1/1_2_2_set_operations.php)). According to formal language theory, a language is a *set* of strings. Data structures themselves can also be seen as sets with various implementations of set operations. One clear way set theory is notionally relevant to programming is through relational databases (where a database is a relation over sets).
5. **[Category theory](https://en.wikipedia.org/wiki/Category_theory)**. Category theory generalizes several branches of mathematics. It does so by providing a generic “meta” language to be able to model concepts abstractly. This is profoundly useful as it allows us to reason about a network of relationships, instead of being concerned with the details of a particular system. A *category* is a collection of objects. The relations between these objects (such as composition or associativity) are known as *[morphisms](https://mathworld.wolfram.com/Morphism.html)*. Relationships between categories are known as *[functors](https://en.wikipedia.org/wiki/Functor)*. Several concepts described by category theory are used in computer science to assess the correctness and concision of programs. Category theoretical concepts are more visible in pure functional paradigms, where computation comprises of mathematical functions that can be composed to build complexity.
6. **[Automata theory](https://en.wikipedia.org/wiki/Automata_theory)**. This corner of computer science defines the footing for formal language analysis, developing the theoretical basis for compiler design, parsers, grammars, and regular expressions. We model formal languages using an automaton, a finite representation of a formal language that draws from an infinite set. A finite automaton is an abstract machine that moves through states to perform a computation, determining whether a given input should be accepted or rejected based on a set of rules defined by the formal language. The [Chomksy hierarchy](https://en.wikipedia.org/wiki/Chomsky_hierarchy) provides a way to group formal languages into successively larger classes.





---

Type Theory 🚧

書籍、講義

```
Type Theory and Formal Proof: An Introduction
Homotopy Type Theory: Univalent Foundations of Mathematics
PROGRAM = PROOF
https://www.ps.uni-saarland.de/courses.html
http://www.cs.cmu.edu/~rwh/courses/chtt/
http://www.cs.ru.nl/~freek/courses/tt-2013/
http://www.cs.cornell.edu/courses/cs6180/
https://www.cs.cornell.edu/courses/cs3110/2020sp/lectures.html
```

我不打算整理。我只找了資源，各位請自便。

Category Theory 🚧

書籍、講義

```
Category Theory for Programmers
Category Theory for Computer Science
Category Theory for the Sciences
The Joy of Abstraction: An Exploration of Math, Category Theory, and Life
An Invitation to Applied Category Theory: Seven Sketches in Compositionality
Introduction to Higher-Order Categorical Logic
category theory = type theory
https://cs.stackexchange.com/questions/3028
```

我不打算整理。我只找了資源，各位請自便。

Mathematical Logic 🚧

Mathematical Logic

數學邏輯重塑邏輯學所有概念，重新歸類命名，形成四大領域：

```
set theory        集合論（來自predicate）
proof theory      證明論（來自derivation）
model theory      模型論（來自interpretation）
recursion theory  遞迴論（來自language/automaton）
```

數學名詞全面以邏輯學名詞重新定義，變得嚴謹。

```
[formal language]
sentence = statement
language = a set of sentences
grammar = a set of rule of inference
substitution = rule of replacement

[formal proof]
theory = a set of sentences
calculi = inference
axiom = a set of premisses
proof = derivation
assumption = argument
hypothesis = premiss
theorem = conclusion

[formal proof with first-order logic]
formula = theorem
valuation = interpretation
structure = (?)
model = a set of satisfiable intepretations for all formulas
        (consistency)
```

Programming Language 🚧

Programming Language

```
Programming Language Pragmatics
Types and Programming Languages
Practical Foundations for Programming Languages
The Structure of Typed Programming Languages
Formal Semantics of Programming Languages
Introduction to the Theory of Programming Languages
Software Foundations
Communicating Sequential Processes
https://github.com/steshaw/plt
```

想要瞭解程式語言理論，可以讀這些書。

Logic Programming Language

```
Foundations of Logic Programming
Introduction to Logic Programming
```

Prolog 是知名的邏輯式程式語言。我一點興趣都沒有。

Functional Programming Language

```
Programming Languages: Functional Programming
Formosan Summer School on Logic, Language, and Computation
Structure and Interpretation of Computer Programs
https://zhuanlan.zhihu.com/p/24648375
https://en.wikipedia.org/wiki/Domain_theory
```

如果你喜歡函數式程式語言，別錯過台灣中研院的研習營。

Haskell 、 Clojure 、 Scala 是知名的函數式程式語言。 C++ 、 Rust 、 JavaScript 包山包海，可以做到函數式編程。

Imperative Programming Language

```
C++ Primer
The C++ Programming Language
A Tour of C++
Discovering Modern C++: An Intensive Course for Scientists, Engineers, and Programmers
Effective C++: 55 Specific Ways to Improve Your Programs and Designs
Effective Modern C++: 42 Specific Ways to Improve Your Use of C++11 and C++14
```

指令式程式語言是當今最普遍的程式語言。甚至現在已經成為中學課程的一部分了。生而為人，這是必須要知道的基本常識。

硬體韌體 C 、應用程式 C++ 、視窗軟體 C# 、手機軟體 Java 與 Kotlin 、函式庫調用 Python 、瀏覽器操作 JavaScript 、網頁設計 PHP 、伺服器程式 GO 、系統程式 Rust 、定位不明的 D 、數學工具 MATLAB 、統計學工具 R 、科學計算 Fortran 、金融交易 COBOL 、 …… ，這些都是指令式程式語言。語言越來越多，分工越來越細。

這邊只提供 C++ 的書單，難易程度位於程式設計師生涯第一年的程度。主要原因是我只有這種程度。次要原因是 C++ 花樣很多。 C++ 是一個實驗性語言，一直在添加新想法。雖然作為營生工具算是挺痛苦的，但是拿來開開眼界還是挺不賴的。

Automatic Reasoning 🚧

書籍、講義

```
Logic in Computer Science: modelling and reasoning about systems
https://web.stanford.edu/class/cs227/
https://lfcps.org/course/constlog-schedule.html
https://www.amazon.science/blog/a-gentle-introduction-to-automated-reasoning
https://symbolaris.com/
```

我不打算整理。我只找了資源，各位請自便。

Automatic Reasoning

自動推理。四個主題，由易到難。

```
Satisfiability Checking：判斷邏輯敘述是否可以為真（是否恆假）。
Proof Verification：判斷邏輯推論是否正確（是否有效／可衍生）。
Theorem Proving：給定前提、結論，找到衍生過程。
Logical Reasoning：找到前提、結論、衍生過程。
```

Satisfiability Problem （ SAT ）

```
http://www.satlive.org/
https://sat-smt.codes/
http://www.cs.man.ac.uk/~korovink/
https://homepage.iis.sinica.edu.tw/~bywang/courses/comp-logic/
```

給定邏輯敘述，判斷是否可以為真（判斷是否恆假）。

簡易解法： Interpretation 。

進階解法：邏輯敘述改寫成 CNF ，觀察每個括號是否皆真。

大家編寫程式進行檢查，稱作 SAT Solver 。這個網站整理了一份詳細列表：



Conjunctive Normal Form （ CNF ）

CNF 是一種格式：變數適度加上 ¬ ，括號裡面全是 ∨ ，括號之間全是 ∧ ，括號只有一層。

例如 (p∨¬q)∧(q)∧(¬q∨¬r∨¬s)∧(r∨s) 。

變數適度加上 ¬ 稱作「字面值 literal 」。

例如 p 、 ¬q 、 q 、 ¬q 、 ¬r 、 ¬s 、 r 、 s 是字面值。

括號稱作「子句 clause 」。

例如 (p∨¬q) 、 (q) 、 (¬q∨¬r∨¬s) 、 (r∨s) 是子句。

CNF 容易判斷真假：每個子句皆是真，那麼整體是真。子句裡面任何一個字面值是真，那麼子句是真。

CNF 容易設計演算法。

邏輯敘述總是可以改寫成 CNF

一、移除 → 、 ↔ 。改寫成 ¬ 、 ∧ 、 ∨ 。

例如 p→q 換成 ¬p∨q 。

例如 p↔q 換成 (p∧q)∨(¬p∧¬q) 。

二、笛摩根定律：移除括號附帶的 ¬ 。

例如 ¬(p∧q) 換成 ¬p∨¬q 。

三、分配律：移除括號之間的 ∨ 。

例如 (p∧q)∨(r∧s) 換成 (p∨(r∧s))∧(q∨(r∧s)) 再換成 (p∨r)∧(p∨s)∧(q∨r)∧(q∨s) 。

四、雙反律：移除變數附帶的多餘 ¬ 。

例如 ¬¬p 換成 p 。

例如 ¬¬¬q 換成 ¬q 。

k-SAT

SAT 有個重要特例是 k-SAT ： CNF 括號裡面至多 k 個字面值。

k=1 擁有一次方時間演算法，手法是檢查 x 和 ¬x 是否同時出現。 k=2 擁有一次方時間演算法，手法是化作圖論問題。 k≥3 是 NP-complete 問題，只有指數時間演算法，手法是回溯法。

現實應用， k 都很大，基本無望。然而大家依舊努力改良演算法，盡可能減少計算時間。詳情請見 TAOCP 4B 。

Resolution



「檢查兩個子句是真」精簡成「檢查一個子句是真」。

```
p∨r , q∨¬r ⊨ p∨q
p₁∨...∨pₙ∨r , q₁∨...∨qₘ∨¬r ⊨ p₁∨...∨pₙ∨q₁∨...∨qₘ
```

講一下思路。當前提為真，以 r 和 ¬r 為主角，分成兩種情況。一、如果 r 真，那麼 q 真。二、如果 ¬r 真，那麼 p 真。也就是說， p q 至少有一個是真。

延伸閱讀： Semantic Tableaux



樹狀圖。遇到 ∧ ：分配律共催落去。遇到 ∨ ：分支。

如果你不喜歡 CNF ，那麼可以用這個方法。

```
   p∧(p∨¬q)
       |            ⊙ open
  (p∧p)∨(q∨p)       × close
   /       \
(p∧p)     (q∨p)
  ⊙        ×
```

Satisfiability Modulo Theories （ SMT ）

SMT 是 SAT 加強版本。援引集合、追加函數。



Proof Verification

給定邏輯推論，判斷是否有效。

解法：將邏輯推論改寫成邏輯敘述，利用 SAT 解決。

邏輯推論無效，只有一種情況：前提是真、結論是假。

前提不改變、結論追加 ¬ ，兩者 ∧ 起來，形成邏輯敘述。敘述可以為真，推論無效。敘述是恆假句，推論有效。

```
verify p₁ , p₂ , ... , pₙ ⊨ q

if p₁ ∧ p₂ ∧ ... ∧ pₙ ∧ ¬q can be ⊤,
then p₁ , p₂ , ... , pₙ ⊨ q is invalid.

if p₁ ∧ p₂ ∧ ... ∧ pₙ ∧ ¬q is always ⊥,
then p₁ , p₂ , ... , pₙ ⊨ q is valid.
```

Theorem Proving

```
https://tpchina.github.io/readings/
http://www.cs.ru.nl/~herman/
https://www.cs.cornell.edu/courses/cs3110/2018sp/a5/coq-tactics-cheatsheet.html
http://www.inf.ed.ac.uk/teaching/courses/ar/
http://logitext.mit.edu/
https://www.tptp.org/CASC/
```

簡易解法：規則表變成圖，利用狀態空間搜尋找到衍生過程。

進階解法：規則表變成自動機，利用自動機遍歷找到衍生過程。

大家編寫程式進行檢查，稱作 Theorem Prover 。知名工具 Coq 、 Isabelle 、 Lean 、 ACL2 。維基百科整理了一份詳細列表：



Reasoning



大家編寫程式進行檢查，稱作 Reasoning System 。目前沒有知名工具。維基百科整理了一份詳細列表：



Formal Verification 🚧

書籍、講義

```
Principles of Model Checking
Model Checking
Systems and Software Verification: Model-Checking Techniques and Tools
http://im.ntu.edu.tw/~tsay/dokuwiki/doku.php?id=courses:ssv2022:main
http://www.cs.cmu.edu/~15414/
Formosan Summer School on Logic, Language, and Computation
SRI: Summer School on Formal Techniques
```

我不打算整理。我只找了資源，各位請自便。

Formal Verification

```
https://www.cis.upenn.edu/~stevez/vellvm/
https://nfm2022.caltech.edu/
Formal Verification: An Essential Toolkit for Modern VLSI Design
```

形式驗證。模擬驗算。

形式 Formal 是一個形容詞，意思是在架空世界建立規律。

現實世界的詞彙，開頭冠上 Formal ，便得到架空世界的詞彙。例如形式科學、形式證明、形式謬誤、形式驗證、形式語言、 …… 。

工程當中，創新事物，改造現實。想要檢查創新事物是否合理，那就建立一個架空世界，令創新事物媒合虛擬規則，改為檢查虛擬規則是否合理。

計算機工程當中，編譯器領域的 LLVM 、網路領域的加密演算法，使用了形式驗證。至於電子、航太、金融、製造、醫療，我只有聽說似乎有在使用，詳情我就不清楚了。

設計規則，然後又設計規則以驗證這些規則，乍看自討苦吃。然而這些領域一旦犯錯，將會消耗巨大成本，甚至消耗人命。而驗證成本相對較少。即便是看似無謂的苦工，也值得一做再做。

Model Checking = Theorem Proving

```
https://www.cs.cornell.edu/courses/cs6480/2020sp/schedule/
http://spinroot.com/cs118/
https://saw.galois.com/
https://www.prismmodelchecker.org/
https://www.stormchecker.org/about.html
```

模型檢測。

利用邏輯學進行形式驗證。創新事物改寫成邏輯敘述、邏輯推論。檢查邏輯敘述是否恆假、邏輯推論是否無效。

大家編寫程式進行檢查，稱作 Model Checker 。已有各式各樣的工具，用於各式各樣的用途。例如 [Infer](https://github.com/facebook/infer) 和 [TLA+](https://github.com/tlaplus) 用來檢查程式運作（編寫程式碼檢查程式碼）、 Spin 用來檢查多執行緒軟體、 SAW 用來檢查加密演算法。維基百科整理了一份詳細列表：



Modal Logic 🚧

Modal Logic



Temporal Logic 🚧

Temporal Logic

```
https://en.wikipedia.org/wiki/Linear_temporal_logic
https://en.wikipedia.org/wiki/Büchi_automaton
https://en.wikipedia.org/wiki/Computation_tree_logic
https://5nizza.github.io/2016/08/11/ctl-to-haa/
```