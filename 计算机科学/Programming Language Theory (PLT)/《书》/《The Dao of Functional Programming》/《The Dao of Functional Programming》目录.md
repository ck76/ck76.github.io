[toc]

以下是《The Dao of Functional Programming》的中英双语对照目录：

### 目录 (Contents)

**序言** (Preface) x

集合论 (Set theory) x  
约定 (Conventions) xi

**第1章 白板** (Chapter 1: Clean Slate) 1  
1.1 类型与函数 (Types and Functions) 1  
1.2 阴阳 (Yin and Yang) 2  
1.3 元素 (Elements) 3  
1.4 箭头的对象 (The Object of Arrows) 4

**第2章 组合** (Chapter 2: Composition) 5  
2.1 组合 (Composition) 5  
2.2 函数应用 (Function application) 7  
2.3 恒等 (Identity) 8  
2.4 单态性 (Monomorphisms) 9  
2.5 满态性 (Epimorphisms) 10

**第3章 同构** (Chapter 3: Isomorphisms) 13  
3.1 同构对象 (Isomorphic Objects) 13  
3.2 自然性 (Naturality) 15  
3.3 用箭头推理 (Reasoning with Arrows) 16  
反向箭头 (Reversing the Arrows) 19

**第4章 和类型** (Chapter 4: Sum Types) 21  
4.1 布尔 (Bool) 21  
示例 (Examples) 23  
4.2 枚举 (Enumerations) 24  
Haskell短篇 (Short Haskell Digression) 25  
4.3 和类型 (Sum Types) 26  
可能性 (Maybe) 27  
逻辑 (Logic) 28  
4.4 余笛卡尔范畴 (Cocartesian Categories) 28  
一加零 (One Plus Zero) 28  
某物加零 (Something Plus Zero) 29  
交换律 (Commutativity) 29  
结合律 (Associativity) 30  
函子性 (Functoriality) 31  
对称单体范畴 (Symmetric Monoidal Category) 31

**第5章 积类型** (Chapter 5: Product Types) 33  
逻辑 (Logic) 34  
元组和记录 (Tuples and Records) 34  
5.1 笛卡尔范畴 (Cartesian Category) 35  
元组算术 (Tuple Arithmetic) 35  
函子性 (Functoriality) 37  
5.2 对偶性 (Duality) 37  
5.3 单体范畴 (Monoidal Category) 38  
幺半群 (Monoids) 39

**第6章 函数类型** (Chapter 6: Function Types) 43  
消去规则 (Elimination rule) 43  
引入规则 (Introduction rule) 44  
柯里化 (Currying) 45  
与λ演算的关系 (Relation to lambda calculus) 46  
演绎法 (Modus ponens) 47  
6.1 重新审视和积 (Sum and Product Revisited) 47  
和类型 (Sum types) 47  
积类型 (Product types) 48  
函子性再探 (Functoriality revisited) 49  
6.2 函数类型的函子性 (Functoriality of the Function Type) 49  
6.3 双笛卡尔闭范畴 (Bicartesian Closed Categories) 50  
分配律 (Distributivity) 51

**第7章 递归** (Chapter 7: Recursion) 55  
7.1 自然数 (Natural Numbers) 55  
引入规则 (Introduction Rules) 56  
消去规则 (Elimination Rules) 56  
在编程中的应用 (In Programming) 58  
7.2 列表 (Lists) 59  
消去规则 (Elimination Rule) 59  
7.3 函子性 (Functoriality) 61

**第8章 函子** (Chapter 8: Functors) 63  
8.1 范畴 (Categories) 63  
集合范畴 (Category of sets) 63  
对立范畴 (Opposite categories) 64  
积范畴 (Product categories) 64  
切片范畴 (Slice categories) 65  
余切片范畴 (Coslice categories) 65  
8.2 函子 (Functors) 65  
范畴间的函子 (Functors between categories) 66  
8.3 在编程中的应用 (Functors in Programming) 68  
自函子 (Endofunctors) 68  
双函子 (Bifunctors) 69  
反变函子 (Contravariant functors) 69  
预函子 (Profunctors) 70  
8.4 同态函子 (The Hom-Functor) 71  
8.5 函子组合 (Functor Composition) 72  
范畴的范畴 (Category of categories) 73

**第9章 自然变换** (Chapter 9: Natural Transformations) 75  
9.1 同态函子间的自然变换 (Natural Transformations Between Hom-Functors) 75  
9.2 函子间的自然变换 (Natural Transformation Between Functors) 77  
9.3 在编程中的应用 (Natural Transformations in Programming) 78  
自然变换的垂直组合 (Vertical composition of natural transformations) 80  
函子范畴 (Functor categories) 81  
自然变换的水平组合 (Horizontal composition of natural transformations) 82  
“搅拌” (Whiskering) 84  
交换律 (Interchange law) 86  
9.4 重新审视普遍构造 (Universal Constructions Revisited) 86  
挑选对象 (Picking objects) 87  
余笛卡尔图作为自然变换 (Cospans as natural transformations) 87  
余笛卡尔图的函子性 (Functoriality of cospans) 88  
作为普遍余笛卡尔图的和 (Sum as a universal cospan) 89  
作为普遍笛卡尔图的积 (Product as a universal span) 89  
指数对象 (Exponentials) 90  
9.5 极限与余极限 (Limits and Colimits) 92  
等化子 (Equalizers) 93  
余等化子 (Coequalizers) 94  
终对象的存在 (The existence of the terminal object) 96  
9.6 米田引理 (The Yoneda Lemma) 97  
编程中的米田引理 (Yoneda lemma in programming) 99  
反变米田引理 (The contravariant Yoneda lemma) 99  
9.7 米田嵌入 (Yoneda Embedding) 100  
9.8 可表函子 (Representable Functors) 102  
猜谜游戏 (The guessing game) 102  
编程中的可表函子 (Representable functors in programming) 103  
9.9 二范畴 (2-category Cat) 104  
9.10 有用的公式 (Useful Formulas) 104

**第10章 伴随** (Chapter 10: Adjunctions) 105  
10.1 柯里化伴随 (The Currying Adjunction) 105  
10.2 和与积的伴随 (The Sum and the Product Adjunctions) 106  
对角函子 (The diagonal functor) 106  
和的伴随 (The sum adjunction) 107  
积的伴随 (The product adjunction) 108  
分配律 (Distributivity) 108  
10.3 函子之间的伴随 (Adjunction between functors) 109  
10.4 作为伴随的极限与余极限 (Limits and Colimits as Adjunctions) 110  
10.5 伴随的单元与余单元 (Unit and Counit of an Adjunction) 111  
三角恒等式 (Triangle identities) 113  
柯里化伴随的单元与余单元 (The unit and counit of the currying adjunction) 114  
10.6 使用普遍箭头的伴随 (Adjunctions Using Universal Arrows) 115  
逗号范畴 (Comma category) 116  
普遍箭头 (Universal arrow) 116  
从伴随到普遍箭头 (Universal arrows from adjunctions) 117  
从普遍箭头到伴随 (Adjunction from universal arrows) 117  
10.7 伴随的性质 (Properties of Adjunctions) 118  
左伴随保余极限 (Left adjoints preserve colimits) 118  
右伴随保极限 (Right adjoints preserve limits) 120  
10.8 弗雷德伴随函子定理 (Freyd’s adjoint functor theorem) 120  
在预序中的弗雷德定理 (Freyd’s theorem in a preorder) 121  
解集条件 (Solution set condition) 122  
去功能化 (Defunctionalization) 123  
10.9 自由/遗忘伴随 (Free/Forgetful Adjunctions) 126  
幺半群范畴 (The category of monoids) 126  
自由幺半群 (Free monoid) 127  
编程中的自由幺半群 (Free monoid in programming) 128  
10.10伴随的范畴 (The Category of Adjunctions) 129  
10.11抽象的层次 (Levels of Abstraction) 130

**第11章 依赖类型** (Chapter 11: Dependent Types) 131  
11.1 依赖向量 (Dependent Vectors) 132  
11.2 范畴论中的依赖类型 (Dependent Types Categorically) 133  
纤维丛 (Fibrations) 133  
类型族作为纤维丛 (Type families as fibrations) 134  
拉回 (Pullbacks) 134  
替换 (Substitution) 137  
依赖环境 (Dependent environments) 137  
弱化 (Weakening) 137  
基变换函子 (Base-change functor) 137  
11.3 依赖和 (Dependent Sum) 139  
添加图集 (Adding the atlas) 141  
存在量化 (Existential quantification) 142  
11.4 依赖积 (Dependent Product) 142  
Haskell中的依赖积 (Dependent product in Haskell) 142  
集合的依赖积 (Dependent product of sets) 143  
范畴论中的依赖积 (Dependent product categorically) 143  
添加图集 (Adding the atlas) 145  
普遍量化 (Universal quantification) 147  
11.5 等式 (Equality) 147  
等式推理 (Equational reasoning) 148  
等式与同构 (Equality vs isomorphism) 149  
等式类型 (Equality types) 150  
引入规则 (Introduction rule) 150  
β-简化和η-转换 (β-reduction and η-conversion) 150  
自然数的归纳原则 (Induction principle for natural numbers) 151  
等式消去规则 (Equality elimination rule) 152

**第12章 代数** (Chapter 12: Algebras) 155  
12.1 自自函子的代数 (Algebras from Endofunctors) 156  
12.2 代数的范畴 (Category of Algebras) 157  
初始代数 (Initial algebra) 157  
12.3 Lambek引理与不动点 (Lambek’s Lemma and Fixed Points) 158  
Haskell中的不动点 (Fixed point in Haskell) 159  
12.4 Catamorphisms (Catamorphisms) 160  
示例 (Examples) 161  
列表作为初始代数 (Lists as initial algebras) 162  
12.5 从普遍性构造初始代数 (Initial Algebra from Universality) 163  
12.6 作为余极限的初始代数 (Initial Algebra as a Colimit) 164  
证明 (The proof) 166

**第13章 余代数** (Chapter 13: Coalgebras) 169  
13.1 自自函子的余代数 (Coalgebras from Endofunctors) 169  
13.2 余代数的范畴 (Category of Coalgebras) 170  
13.3 Anamorphisms (Anamorphisms) 171  
无限数据结构 (Infinite data structures) 172  
13.4 Hylomorphisms (Hylomorphisms) 173  
阻抗失配 (The impedance mismatch) 174  
13.5 从普遍性构造终余代数 (Terminal Coalgebra from Universality) 174  
13.6 作为极限的终余代数 (Terminal Coalgebra as a Limit) 176

**第14章 单子** (Chapter 14: Monads) 179  
14.1 带副作用的编程 (Programming with Side Effects) 179  
不完全性 (Partiality) 180  
日志 (Logging) 180  
环境 (Environment) 180  
状态 (State) 181  
非确定性 (Nondeterminism) 181  
输入/输出 (Input/Output) 182  
延续 (Continuation) 182  
14.2 组合副作用 (Composing Effects) 183  
14.3 替代定义 (Alternative Definitions) 184  
14.4 单子实例 (Monad Instances) 186  
不完全性 (Partiality) 186  
日志 (Logging) 186  
环境 (Environment) 186  
状态 (State) 187  
非确定性 (Nondeterminism) 188  
延续 (Continuation) 188  
输入/输出 (Input/Output) 189  
14.5 Do记法 (Do Notation) 189  
14.6 传递风格 (Continuation Passing Style) 190  
尾递归与CPS (Tail recursion and CPS) 191  
使用命名函数 (Using named functions) 192  
去功能化 (Defunctionalization) 193  
14.7 范畴中的单子 (Monads Categorically) 194  
替换 (Substitution) 194  
单子作为幺半群 (Monad as a monoid) 195  
14.8 自由单子 (Free Monads) 196  
单子的范畴 (Category of monads) 197  
自由单子 (Free monad) 197  
堆栈计算器示例 (Stack calculator example) 199  
14.9 单体函子 (Monoidal Functors) 201  
松弛单体函子 (Lax monoidal functors) 202  
函子强度 (Functorial strength) 203  
适用函子 (Applicative functors) 204  
闭函子 (Closed functors) 205  
单子与适用函子 (Monads and applicatives) 206

**第15章 单子与伴随** (Chapter 15: Monads and Adjunctions) 209  
15.1 串图 (String Diagrams) 209  
单子的串图 (String diagrams for the monad) 212  
伴随的串图 (String diagrams for the adjunction) 214  
15.2 从伴随构造单子 (Monads from Adjunctions) 215  
15.3 从伴随构造单子的示例 (Examples of Monads from Adjunctions) 216  
自由幺半群与列表单子 (Free monoid and the list monad) 216  
柯里化伴随与状态单子 (The currying adjunction and the state monad) 217  
M-集与写者单子 (M-sets and the writer monad) 219  
指向对象与可能单子 (Pointed objects and the Maybe monad) 221  
延续单子 (The continuation monad) 221  
15.4 单子变换器 (Monad Transformers) 221  
状态单子变换器 (State monad transformer) 223  
15.5 单子代数 (Monad Algebras) 225  
Eilenberg-Moore范畴 (Eilenberg-Moore category) 226  
Kleisli范畴 (Kleisli category) 228

**第16章 余单子** (Chapter 16: Comonads) 231  
16.1 编程中的余单子 (Comonads in Programming) 231  
Stream余单子 (The Stream comonad) 232  
16.2 范畴中的余单子 (Comonads Categorically) 234  
余幺半群 (Comonoids) 234  
16.3 从伴随构造余单子 (Comonads from Adjunctions) 235  
代状态余单子 (Costate comonad) 236  
余单子代数 (Comonad coalgebras) 238  
透镜 (Lenses) 238

**第17章 端与余端** (Chapter 17: Ends and Coends) 241  
17.1 预函子 (Profunctors) 241  
拼接 (Collages) 242  
预函子作为关系 (Profunctors as relations) 242  
Haskell中的预函子组合 (Profunctor composition in Haskell) 243  
17.2 余端 (Coends) 244  
超自然变换 (Extranatural transformations) 246  
使用余端的预函子组合 (Profunctor composition using coends) 248  
余极限作为余端 (Colimits as coends) 248  
17.3 端 (Ends) 249  
自然变换作为端 (Natural transformations as an end) 251  
极限作为端 (Limits as ends) 252  
17.4 同态函子的连续性 (Continuity of the Hom-Functor) 253  
17.5 富比尼规则 (Fubini Rule) 253  
17.6 忍者米田引理 (Ninja Yoneda Lemma) 254  
Haskell中的米田引理 (Yoneda lemma in Haskell) 255  
17.7 Day卷积 (Day Convolution) 256  
适用函子作为幺半群 (Applicative functors as monoids) 257  
自由适用函子 (Free Applicatives) 258  
17.8 预函子的双范畴 (The Bicategory of Profunctors) 259  
双范畴中的单子 (Monads in a bicategory) 260  
预箭头作为Prof中的单子 (Prearrows as monads in Prof) 261  
17.9 存在透镜 (Existential Lens) 262  
Haskell中的存在透镜 (Existential lens in Haskell) 262  
范畴论中的存在透镜 (Existential lens in category theory) 263  
Haskell中的类型变化透镜 (Type-changing lens in Haskell) 263  
透镜组合 (Lens composition) 264  
透镜的范畴 (Category of lenses) 265  
17.10透镜与纤维丛 (Lenses and Fibrations) 265  
传输律 (Transport law) 266  
恒等律 (Identity law) 266  
组合律 (Composition law) 267  
类型变化透镜 (Type-changing lens) 267  
17.11重要公式 (Important Formulas) 268

**第18章 Tambara模** (Chapter 18: Tambara Modules) 271  
18.1 Tannakian重建 (Tannakian Reconstruction) 271  
幺半群及其表示 (Monoids and their Representations) 271  
幺半群的Tannakian重建 (Tannakian reconstruction of a monoid) 272  
凯莱定理 (Cayley’s theorem) 274  
Tannakian重建证明 (Proof of Tannakian reconstruction) 276  
Haskell中的Tannakian重建 (Tannakian reconstruction in Haskell) 277  
使用伴随进行Tannakian重建 (Tannakian reconstruction with adjunction) 278  
18.2 预函子透镜 (Profunctor Lenses) 279  
同构 (Iso) 280  
预函子与透镜 (Profunctors and lenses) 281  
Tambara模 (Tambara module) 281  
预函子透镜 (Profunctor lenses) 283  
Haskell中的预函子透镜 (Profunctor lenses in Haskell) 284  
18.3 一般光学 (General Optics) 285  
棱镜 (Prisms) 285  
遍历 (Traversals) 286  
18.4 混合光学 (Mixed Optics) 289

**第19章 Kan扩展** (Chapter 19: Kan Extensions) 291  
19.1 闭单体范畴 (Closed Monoidal Categories) 291  
Day卷积的内部同态 (Internal hom for Day convolution) 292  
幂与余幂 (Powering and co-powering) 293  
19.2 反转函子 (Inverting a functor) 294  
19.3 右Kan扩展 (Right Kan extension) 296  
作为端的右Kan扩展 (Right Kan extension as an end) 297  
Haskell中的右Kan扩展 (Right Kan extension in Haskell) 298  
作为Kan扩展的极限 (Limits as Kan extensions) 299  
作为右Kan扩展的左伴随 (Left adjoint as a right Kan extension) 301  
密度单子 (Codensity monad) 302  
Haskell中的密度单子 (Codensity monad in Haskell) 303  
19.4 左Kan扩展 (Left Kan extension) 304  
作为余端的左Kan扩展 (Left Kan extension as a coend) 305  
Haskell中的左Kan扩展 (Left Kan extension in Haskell) 306  
作为Kan扩展的余极限 (Colimits as Kan extensions) 307  
作为左Kan扩展的右伴随 (Right adjoint as a left Kan extension) 308  
作为Kan扩展的Day卷积 (Day convolution as a Kan extension) 308  
19.5 有用的公式 (Useful Formulas) 309

**第20章 丰富** (Chapter 20: Enrichment) 311  
20.1 丰富的范畴 (Enriched Categories) 311  
集合论基础 (Set-theoretical foundations) 311  
同态对象 (Hom-Objects) 312  
丰富的范畴 (Enriched Categories) 312  
示例 (Examples) 314  
预序 (Preorders) 314  
自丰富 (Self-enrichment) 315  
20.2 $\mathcal{V}$-函子 ($\mathcal{V}$-Functors) 315  
同态函子 (The Hom-functor) 316  
丰富的协同预层 (Enriched co-presheaves) 317  
函子强度与丰富 (Functorial strength and enrichment) 317  
20.3 $\mathcal{V}$-自然变换 ($\mathcal{V}$-Natural Transformations) 319  
20.4 米田引理 (Yoneda Lemma) 321  
20.5 加权极限 (Weighted Limits) 321  
20.6 作为加权极限的端 (Ends as Weighted Limits) 322  
20.7 Kan扩展 (Kan Extensions) 324  
20.8 有用的公式 (Useful Formulas) 325

**索引** (Index) 327

