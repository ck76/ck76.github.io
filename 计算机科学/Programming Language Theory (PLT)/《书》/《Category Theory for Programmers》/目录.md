[toc]

以下是《Category Theory for Programmers》目录的中英双语对照版：

### 目录 (Contents)

**编辑者的说明** (A note from the editor) ix

**序言** (Preface) xi

### 第一部分 (Part One)

**第1章 范畴：组合的本质** (Chapter 1: Category: The Essence of Composition) 3  
1.1 箭头作为函数 (Arrows as Functions) 3  
1.2 组合的性质 (Properties of Composition) 5  
1.3 组合是编程的本质 (Composition is the Essence of Programming) 7  
1.4 挑战 (Challenges) 8

**第2章 类型与函数** (Chapter 2: Types and Functions) 11  
2.1 谁需要类型？ (Who Needs Types?) 11  
2.2 类型与组合性 (Types Are About Composability) 12  
2.3 什么是类型？ (What Are Types?) 13  
2.4 为什么我们需要一个数学模型？ (Why Do We Need a Mathematical Model?) 15  
2.5 纯函数与脏函数 (Pure and Dirty Functions) 16  
2.6 类型的例子 (Examples of Types) 17  
2.7 挑战 (Challenges) 20

**第3章 大小范畴** (Chapter 3: Categories Great and Small) 21  
3.1 无对象 (No Objects) 21  
3.2 简单图 (Simple Graphs) 21  
3.3 顺序 (Orders) 22  
3.4 单子作为集合 (Monoid as Set) 22  
3.5 单子作为范畴 (Monoid as Category) 25  
3.6 挑战 (Challenges) 27

**第4章 Kleisli 范畴** (Chapter 4: Kleisli Categories) 29  
4.1 Writer 范畴 (The Writer Category) 32  
4.2 Haskell 中的 Writer (Writer in Haskell) 34  
4.3 Kleisli 范畴 (Kleisli Categories) 36  
4.4 挑战 (Challenge) 37

**第5章 积与余积** (Chapter 5: Products and Coproducts) 39  
5.1 初始对象 (Initial Object) 39  
5.2 终端对象 (Terminal Object) 41  
5.3 对偶性 (Duality) 42  
5.4 同构 (Isomorphisms) 42  
5.5 积 (Products) 44  
5.6 余积 (Coproduct) 49  
5.7 不对称性 (Asymmetry) 51  
5.8 挑战 (Challenges) 53  
5.9 参考书目 (Bibliography) 54

**第6章 简单代数数据类型** (Chapter 6: Simple Algebraic Data Types) 55  
6.1 积类型 (Product Types) 55  
6.2 记录 (Records) 59  
6.3 和类型 (Sum Types) 61  
6.4 类型代数 (Algebra of Types) 64  
6.5 挑战 (Challenges) 67

**第7章 函子** (Chapter 7: Functors) 69  
7.1 编程中的函子 (Functors in Programming) 71

7.1.1 Maybe 函子 (The Maybe Functor) 71  
7.1.2 等式推理 (Equational Reasoning) 73  
7.1.3 Optional (Optional) 75  
7.1.4 类型类 (Typeclasses) 76  
7.1.5 C++ 中的函子 (Functor in C++) 77  
7.1.6 List 函子 (The List Functor) 78  
7.1.7 Reader 函子 (The Reader Functor) 80

7.2 函子作为容器 (Functors as Containers) 81  
7.3 函子组合 (Functor Composition) 84  
7.4 挑战 (Challenges) 86

**第8章 函子的性质** (Chapter 8: Functoriality) 87  
8.1 双函子 (Bifunctors) 87  
8.2 积与余积的双函子 (Product and Coproduct Bifunctors) 89  
8.3 代数数据类型的函子性 (Functorial Algebraic Data Types) 91  
8.4 C++ 中的函子 (Functors in C++) 94  
8.5 Writer 函子 (The Writer Functor) 96  
8.6 协变与逆变函子 (Covariant and Contravariant Functors) 98  
8.7 预函子 (Profunctors) 100  
8.8 同构函子 (The Hom-Functor) 102  
8.9 挑战 (Challenges) 102

**第9章 函数类型** (Chapter 9: Function Types) 105  
9.1 泛型构造 (Universal Construction) 106  
9.2 柯里化 (Currying) 109  
9.3 指数 (Exponentials) 112  
9.4 笛卡尔封闭范畴 (Cartesian Closed Categories) 113  
9.5 指数与代数数据类型 (Exponentials and Algebraic Data Types) 114

9.5.1 零次幂 (Zeroth Power) 114  
9.5.2 幂为一 (Powers of One) 114  
9.5.3 一次幂 (First Power) 115  
9.5.4 和的指数 (Exponentials of Sums) 115  
9.5.5 指数的指数 (Exponentials of Exponentials) 115  
9.5.6 积上的指数 (Exponentials over Products) 116

9.6 柯里-霍华德同构 (Curry-Howard Isomorphism) 116  
9.7 参考书目 (Bibliography) 118

**第10章 自然变换** (Chapter 10: Natural Transformations) 119  
10.1 多态函数 (Polymorphic Functions) 122  
10.2 超越自然性 (Beyond Naturality) 127  
10.3 函子范畴 (Functor Category) 129  
10.4 2-范畴 (2-Categories) 131  
10.5 结论 (Conclusion) 135  
10.6 挑战 (Challenges) 135

### 第二部分 (Part Two)

**第11章 声明式编程** (Chapter 11: Declarative Programming) 139

**第12章 极限与余极限** (Chapter 12: Limits and Colimits) 145  
12.1 极限作为自然同构 (Limit as a Natural Isomorphism) 149  
12.2 极限的例子 (Examples of Limits) 152  
12.3 余极限 (Colimits) 157  
12.4 连续性 (Continuity) 158  
12.5 挑战 (Challenges) 160

**第13章 自由单子** (Chapter 13: Free Monoids) 161  
13.1 Haskell 中的自由单子 (Free Monoid in Haskell) 162  
13.2 自由单子的泛型构造 (Free Monoid Universal Construction) 163  
13.3 挑战 (Challenges) 166

**第14章 可表示函子** (Chapter 14: Representable Functors) 169  
14.1 同构函子 (The Hom Functor) 170  
14.2 可表示函子 (Representable Functors) 172  
14.3 挑战 (Challenges) 177  
14.4 参考书目 (Bibliography) 177

**第15章 Yoneda 引理** (Chapter 15: The Yoneda Lemma) 179  
15.1 Haskell 中的 Yoneda (Yoneda in Haskell) 183  
15.2 Co-Yoneda (Co-Yoneda) 185  
15.3 挑战 (Challenges) 186  
15.4 参考书目 (Bibliography) 186

**第16章 Yoneda 嵌入** (Chapter 16: Yoneda Embedding) 187  
16.1 嵌入 (The Embedding) 188  
16.2 应用于 Haskell (Application to Haskell) 189  
16.3 预序的例子 (Preorder Example) 190  
16.4 自然性 (Naturality) 191  
16.5 挑战 (Challenges) 192

### 第三部分 (Part Three)

**第17章 一切都是关于态射** (Chapter 17: It’s All About Morphisms) 195  
17.1 函子 (Functors) 195  
17.2 交换图 (Commuting Diagrams) 195  
17.3 自然变换 (Natural Transformations) 196  
17.4 自然同构 (Natural Isomorphisms) 197  
17.5 同构集 (Hom-Sets) 198  
17.6 同构集的同构 (Hom-Set Isomorphisms) 198  
17.7 同构集的不对称性 (Asymmetry of Hom-Sets) 199  
17.8 挑战 (Challenges) 199

**第18章 对偶** (Chapter 18: Adjunctions) 201  
18.1 对偶与单位/余单位对 (Adjunction and Unit/Counit Pair) 201  
18.2 对偶与同构集 (Adjunctions and Hom-Sets) 206  
18.3 从对偶到积 (Product from Adjunction) 209  
18.4 从对偶到指数 (Exponential from Adjunction) 212  
18.5 挑战 (Challenges) 213

**第19章 自由/遗忘对偶** (Chapter 19: Free/Forgetful Adjunctions) 215  
19.1 一些直觉 (Some Intuitions) 217  
19.2 挑战 (Challenges) 219

**第20章 Monad：程序员的定义** (Chapter 20: Monads: Programmer’s Definition) 221  
20.1 Kleisli 范畴 (The Kleisli Category) 222  
20.2 鱼的解剖 (Fish Anatomy) 225  
20.3 do 语法 (The do Notation) 227

**第21章 Monad 与效果** (Chapter 21: Monads and Effects) 233  
21.1 问题 (The Problem) 233  
21.2 解决方案 (The Solution) 234

21.2.1 部分性 (Partiality) 234  
21.2.2 非确定性 (Nondeterminism) 235  
21.2.3 只读状态 (Read-Only State) 237  
21.2.4 只写状态 (Write-Only State) 239  
21.2.5 状态 (State) 240  
21.2.6 异常 (Exceptions) 241  
21.2.7 连续性 (Continuations) 242  
21.2.8 交互输入 (Interactive Input) 244  
21.2.9 交互输出 (Interactive Output) 246

21.3 结论 (Conclusion) 247

**第22章 从范畴论的角度看 Monad** (Chapter 22: Monads Categorically) 249  
22.1 单篡范畴 (Monoidal Categories) 252  
22.2 单篡范畴中的单子 (Monoid in a Monoidal Category) 257  
22.3 Monad 作为单子 (Monads as Monoids) 258  
22.4 从对偶到 Monad (Monads from Adjunctions) 260

**第23章 共单子** (Chapter 23: Comonads) 263  
23.1 使用共单子编程 (Programming with Comonads) 264  
23.2 积共单子 (The Product Comonad) 264  
23.3 解剖组合 (Dissecting the Composition) 266  
23.4 流共单子 (The Stream Comonad) 268  
23.5 从范畴论的角度看共单子 (Comonad Categorically) 270  
23.6 存储共单子 (The Store Comonad) 272  
23.7 挑战 (Challenges) 275

**第24章 F-代数** (Chapter 24: F-Algebras) 277  
24.1 递归 (Recursion) 280  
24.2 F-代数的范畴 (Category of F-Algebras) 282  
24.3 自然数 (Natural Numbers) 284  
24.4 代数同构 (Catamorphisms) 285  
24.5 折叠 (Folds) 287  
24.6 余代数 (Coalgebras) 288  
24.7 挑战 (Challenges) 291

**第25章 Monad 的代数** (Chapter 25: Algebras for Monads) 293  
25.1 T-代数 (T-algebras) 295  
25.2 Kleisli 范畴 (The Kleisli Category) 297  
25.3 共单子的余代数 (Coalgebras for Comonads) 299  
25.4 透镜 (Lenses) 299  
25.5 挑战 (Challenges) 302

**第26章 端与余端** (Chapter 26: Ends and Coends) 303  
26.1 双自然变换 (Dinatural Transformations) 304  
26.2 端 (Ends) 305  
26.3 端作为等化子 (Ends as Equalizers) 308  
26.4 自然变换作为端 (Natural Transformations as Ends) 310  
26.5 余端 (Coends) 311  
26.6 忍者 Yoneda 引理 (Ninja Yoneda Lemma) 314  
26.7 预函子组合 (Profunctor Composition) 315

**第27章 Kan 扩展** (Chapter 27: Kan Extensions) 317  
27.1 右 Kan 扩展 (Right Kan Extension) 319  
27.2 Kan 扩展作为对偶 (Kan Extension as Adjunction) 320  
27.3 左 Kan 扩展 (Left Kan Extension) 322  
27.4 Kan 扩展作为端 (Kan Extensions as Ends) 324  
27.5 Haskell 中的 Kan 扩展 (Kan Extensions in Haskell) 326  
27.6 自由函子 (Free Functor) 329

**第28章 丰富范畴** (Chapter 28: Enriched Categories) 333  
28.1 为什么是单篡范畴？ (Why Monoidal Category?) 334  
28.2 单篡范畴 (Monoidal Category) 334  
28.3 丰富范畴 (Enriched Category) 336  
28.4 预序 (Preorders) 337  
28.5 度量空间 (Metric Spaces) 338  
28.6 丰富函子 (Enriched Functors) 339  
28.7 自我丰富 (Self Enrichment) 340  
28.8 与 2-范畴的关系 (Relation to 2-Categories) 341

**第29章 拓扑** (Chapter 29: Topoi) 343  
29.1 子对象分类器 (Subobject Classifier) 344  
29.2 拓扑 (Topos) 347  
29.3 拓扑与逻辑 (Topoi and Logic) 347  
29.4 挑战 (Challenges) 348

**第30章 Lawvere 理论** (Chapter 30: Lawvere Theories) 349  
30.1 泛代数 (Universal Algebra) 349  
30.2 Lawvere 理论 (Lawvere Theories) 350  
30.3 Lawvere 理论的模型 (Models of Lawvere Theories) 353  
30.4 单子的理论 (The Theory of Monoids) 354  
30.5 Lawvere 理论与 Monad (Lawvere Theories and Monads) 355  
30.6 Monad 作为余端 (Monads as Coends) 357  
30.7 Lawvere 副作用理论 (Lawvere Theory of Side Effects) 359  
30.8 挑战 (Challenges) 360  
30.9 进一步阅读 (Further Reading) 361

**第31章 Monad、单子与范畴** (Chapter 31: Monads, Monoids, and Categories) 363  
31.1 双范畴 (Bicategories) 363  
31.2 单子 (Monads) 367  
31.3 挑战 (Challenges) 370  
31.4 参考书目 (Bibliography) 370

### 附录 (Appendices)

**附录** (Appendices) 371

**索引** (Index) 371

**致谢** (Acknowledgments) 373

**书尾** (Colophon) 374

**版权声明** (Copyleft notice) 375