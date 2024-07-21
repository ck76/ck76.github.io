https://web.ntnu.edu.tw/~algo/ProgrammingLanguage.html

type theory 🚧

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

category theory 🚧

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

mathematical logic 🚧

mathematical logic

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

programming language 🚧

programming language

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

logic programming language

```
Foundations of Logic Programming
Introduction to Logic Programming
```

Prolog 是知名的邏輯式程式語言。我一點興趣都沒有。

functional programming language

```
Programming Languages: Functional Programming
Formosan Summer School on Logic, Language, and Computation
Structure and Interpretation of Computer Programs
https://zhuanlan.zhihu.com/p/24648375
https://en.wikipedia.org/wiki/Domain_theory
```

如果你喜歡函數式程式語言，別錯過台灣中研院的研習營。

Haskell 、 Clojure 、 Scala 是知名的函數式程式語言。 C++ 、 Rust 、 JavaScript 包山包海，可以做到函數式編程。

imperative programming language

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

automatic reasoning 🚧

書籍、講義

```
Logic in Computer Science: modelling and reasoning about systems
https://web.stanford.edu/class/cs227/
https://lfcps.org/course/constlog-schedule.html
https://www.amazon.science/blog/a-gentle-introduction-to-automated-reasoning
https://symbolaris.com/
```

我不打算整理。我只找了資源，各位請自便。

automatic reasoning

自動推理。四個主題，由易到難。

```
satisfiability checking：判斷邏輯敘述是否可以為真（是否恆假）。
proof verification：判斷邏輯推論是否正確（是否有效／可衍生）。
theorem proving：給定前提、結論，找到衍生過程。
logical reasoning：找到前提、結論、衍生過程。
```

satisfiability problem （ SAT ）

```
http://www.satlive.org/
https://sat-smt.codes/
http://www.cs.man.ac.uk/~korovink/
https://homepage.iis.sinica.edu.tw/~bywang/courses/comp-logic/
```

給定邏輯敘述，判斷是否可以為真（判斷是否恆假）。

簡易解法： interpretation 。

進階解法：邏輯敘述改寫成 CNF ，觀察每個括號是否皆真。

大家編寫程式進行檢查，稱作 SAT solver 。這個網站整理了一份詳細列表：



conjunctive normal form （ CNF ）

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

resolution



「檢查兩個子句是真」精簡成「檢查一個子句是真」。

```
p∨r , q∨¬r ⊨ p∨q
p₁∨...∨pₙ∨r , q₁∨...∨qₘ∨¬r ⊨ p₁∨...∨pₙ∨q₁∨...∨qₘ
```

講一下思路。當前提為真，以 r 和 ¬r 為主角，分成兩種情況。一、如果 r 真，那麼 q 真。二、如果 ¬r 真，那麼 p 真。也就是說， p q 至少有一個是真。

延伸閱讀： semantic tableaux



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

satisfiability modulo theories （ SMT ）

SMT 是 SAT 加強版本。援引集合、追加函數。



proof verification

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

theorem proving

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

大家編寫程式進行檢查，稱作 theorem prover 。知名工具 Coq 、 Isabelle 、 Lean 、 ACL2 。維基百科整理了一份詳細列表：



reasoning



大家編寫程式進行檢查，稱作 reasoning system 。目前沒有知名工具。維基百科整理了一份詳細列表：



formal verification 🚧

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

formal verification

```
https://www.cis.upenn.edu/~stevez/vellvm/
https://nfm2022.caltech.edu/
Formal Verification: An Essential Toolkit for Modern VLSI Design
```

形式驗證。模擬驗算。

形式 formal 是一個形容詞，意思是在架空世界建立規律。

現實世界的詞彙，開頭冠上 formal ，便得到架空世界的詞彙。例如形式科學、形式證明、形式謬誤、形式驗證、形式語言、 …… 。

工程當中，創新事物，改造現實。想要檢查創新事物是否合理，那就建立一個架空世界，令創新事物媒合虛擬規則，改為檢查虛擬規則是否合理。

計算機工程當中，編譯器領域的 LLVM 、網路領域的加密演算法，使用了形式驗證。至於電子、航太、金融、製造、醫療，我只有聽說似乎有在使用，詳情我就不清楚了。

設計規則，然後又設計規則以驗證這些規則，乍看自討苦吃。然而這些領域一旦犯錯，將會消耗巨大成本，甚至消耗人命。而驗證成本相對較少。即便是看似無謂的苦工，也值得一做再做。

model checking = theorem proving

```
https://www.cs.cornell.edu/courses/cs6480/2020sp/schedule/
http://spinroot.com/cs118/
https://saw.galois.com/
https://www.prismmodelchecker.org/
https://www.stormchecker.org/about.html
```

模型檢測。

利用邏輯學進行形式驗證。創新事物改寫成邏輯敘述、邏輯推論。檢查邏輯敘述是否恆假、邏輯推論是否無效。

大家編寫程式進行檢查，稱作 model checker 。已有各式各樣的工具，用於各式各樣的用途。例如 [Infer](https://github.com/facebook/infer) 和 [TLA+](https://github.com/tlaplus) 用來檢查程式運作（編寫程式碼檢查程式碼）、 Spin 用來檢查多執行緒軟體、 SAW 用來檢查加密演算法。維基百科整理了一份詳細列表：



modal logic 🚧

modal logic



temporal logic 🚧

temporal logic

```
https://en.wikipedia.org/wiki/Linear_temporal_logic
https://en.wikipedia.org/wiki/Büchi_automaton
https://en.wikipedia.org/wiki/Computation_tree_logic
https://5nizza.github.io/2016/08/11/ctl-to-haa/
```