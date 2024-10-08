[toc]

- https://book.douban.com/subject/35813677/
- https://www.cs.cmu.edu/~rwh/pfpl/

以下是《Practical Foundations for Programming Languages (Second Edition)》的目录文字版：

### 目录 (Contents)

Preface to the Second Edition xv

Preface to the First Edition xvii

### 第一部分 判断与规则 (Part I: Judgments and Rules)

第1章 抽象语法 (Chapter 1: Abstract Syntax) 3  
1.1 抽象语法树 (Abstract Syntax Trees) 3  
1.2 抽象绑定树 (Abstract Binding Trees) 6  
1.3 备注 (Notes) 10

第2章 归纳定义 (Chapter 2: Inductive Definitions) 12  
2.1 判断 (Judgments) 12  
2.2 推理规则 (Inference Rules) 12  
2.3 推导 (Derivations) 14  
2.4 规则归纳法 (Rule Induction) 15  
2.5 迭代与同时归纳定义 (Iterated and Simultaneous Inductive Definitions) 17  
2.6 通过规则定义函数 (Defining Functions by Rules) 18  
2.7 备注 (Notes) 19

第3章 假设与一般判断 (Chapter 3: Hypothetical and General Judgments) 21  
3.1 假设判断 (Hypothetical Judgments) 21  
3.2 假设归纳定义 (Hypothetical Inductive Definitions) 24  
3.3 一般判断 (General Judgments) 26  
3.4 通用归纳定义 (Generic Inductive Definitions) 27  
3.5 备注 (Notes) 28

### 第二部分 静态与动态 (Part II: Statics and Dynamics)

第4章 静态语法 (Chapter 4: Statics) 33  
4.1 语法 (Syntax) 33  
4.2 类型系统 (Type System) 34  
4.3 结构属性 (Structural Properties) 35  
4.4 备注 (Notes) 37

第5章 动态语法 (Chapter 5: Dynamics) 39  
5.1 转换系统 (Transition Systems) 39  
5.2 结构动态性 (Structural Dynamics) 40  
5.3 上下文动态性 (Contextual Dynamics) 42  
5.4 等式动态性 (Equational Dynamics) 44  
5.5 备注 (Notes) 46

第6章 类型安全性 (Chapter 6: Type Safety) 48  
6.1 保持性 (Preservation) 48  
6.2 进展性 (Progress) 49  
6.3 运行时错误 (Run-Time Errors) 50  
6.4 备注 (Notes) 52

第7章 评价动态性 (Chapter 7: Evaluation Dynamics) 53  
7.1 评价动态性 (Evaluation Dynamics) 53  
7.2 结构与评价动态性之间的关系 (Relating Structural and Evaluation Dynamics) 54  
7.3 类型安全性，重述 (Type Safety, Revisited) 55  
7.4 成本动态性 (Cost Dynamics) 56  
7.5 备注 (Notes) 57

### 第三部分 全函数 (Part III: Total Functions)

第8章 函数定义与值 (Chapter 8: Function Definitions and Values) 61  
8.1 一阶函数 (First-Order Functions) 61  
8.2 高阶函数 (Higher-Order Functions) 62  
8.3 评价动态性与定义等式 (Evaluation Dynamics and Definitional Equality) 65  
8.4 动态作用域 (Dynamic Scope) 66  
8.5 备注 (Notes) 67

第9章 高阶递归的系统T (Chapter 9: System T of Higher-Order Recursion) 69  
9.1 静态性 (Statics) 69  
9.2 动态性 (Dynamics) 70  
9.3 可定义性 (Definability) 71  
9.4 不可定义性 (Undefinability) 73  
9.5 备注 (Notes) 75

### 第四部分 有限数据类型 (Part IV: Finite Data Types)

第10章 乘积类型 (Chapter 10: Product Types) 79  
10.1 零元与二元乘积 (Nullary and Binary Products) 79  
10.2 有限乘积 (Finite Products) 81  
10.3 原始的相互递归 (Primitive Mutual Recursion) 82  
10.4 备注 (Notes) 83

第11章 和类型 (Chapter 11: Sum Types) 85  
11.1 零元与二元和 (Nullary and Binary Sums) 85  
11.2 有限和 (Finite Sums) 86  
11.3 和类型的应用 (Applications of Sum Types) 88  
11.4 备注 (Notes) 91

### 第五部分 类型与命题 (Part V: Types and Propositions)

第12章 构造性逻辑 (Chapter 12: Constructive Logic) 95  
12.1 构造语义学 (Constructive Semantics) 95  
12.2 构造性逻辑 (Constructive Logic) 96  
12.3 证明动态性 (Proof Dynamics) 100  
12.4 命题即类型 (Propositions as Types) 101  
12.5 备注 (Notes) 101

第13章 经典逻辑 (Chapter 13: Classical Logic) 104  
13.1 经典逻辑 (Classical Logic) 105  
13.2 推导消去形式 (Deriving Elimination Forms) 109  
13.3 证明动态性 (Proof Dynamics) 110  
13.4 排中律 (Law of the Excluded Middle) 111  
13.5 双重否定翻译 (The Double-Negation Translation) 113  
13.6 备注 (Notes) 114

### 第六部分 无限数据类型 (Part VI: Infinite Data Types)

第14章 泛型编程 (Chapter 14: Generic Programming) 119  
14.1 引言 (Introduction) 119  
14.2 多项式类型运算符 (Polynomial Type Operators) 119  
14.3 正类型运算符 (Positive Type Operators) 122  
14.4 备注 (Notes) 123

第15章 归纳与共归纳类型 (Chapter 15: Inductive and Coinductive Types) 125  
15.1 动机示例 (Motivating Examples) 125  
15.2 静态性 (Statics) 128  
15.3 动态性 (Dynamics) 130  
15.4 求解类型方程 (Solving Type Equations) 131  
15.5 备注 (Notes) 132

### 第七部分 变量类型 (Part VII: Variable Types)

第16章 系统F的多态类型 (Chapter 16: System F of Polymorphic Types) 137  
16.1 多态抽象 (Polymorphic Abstraction) 137  
16.2 多态定义性 (Polymorphic Definability) 140  
16.3 参数性概览 (Parametricity Overview) 142  
16.4 备注 (Notes) 144

第17章 抽象类型 (Chapter 17: Abstract Types) 146  
17.1 存在类型 (Existential Types) 146  
17.2 数据抽象 (Data Abstraction) 149  
17.3 存在类型的定义性 (Definability of Existential Types) 150  
17.4 表示独立性 (Representation Independence) 151  
17.5 备注 (Notes) 153

第18章 高阶类型 (Chapter 18: Higher Kinds) 154  
18.1 构造器与种类 (Constructors and Kinds) 155  
18.2 构造器等式 (Constructor Equality) 156  
18.3 表达式与类型 (Expressions and Types) 157  
18.4 备注 (Notes) 158

### 第八部分 部分性与递归类型 (Part VIII: Partiality and Recursive Types)

第19章 递归函数的系统PCF (Chapter 19: System PCF of Recursive Functions) 161  
19.1 静态性 (Statics) 162  
19.2 动态性 (Dynamics) 163  
19.3 定义性 (Definability) 165  
19.4 有限与无限数据结构 (Finite and Infinite Data Structures) 167  
19.5 完全性与部分性 (Totality and Partiality) 167  
19.6 备注 (Notes) 169

第20章 递归类型的系统FPC (Chapter 20: System FPC of Recursive Types) 171  
20.1 求解类型方程 (Solving Type Equations) 171  
20.2 归纳与共归纳类型 (Inductive and Coinductive Types) 172  
20.3 自引用 (Self-Reference) 174  
20.4 状态的起源 (The Origin of State) 176  
20.5 备注 (Notes) 177

### 第九部分 动态类型 (Part IX: Dynamic Types)

第21章 无类型的λ-演算 (Chapter 21: The Untyped λ-Calculus) 181  
21.1 λ-演算 (The λ-Calculus) 181  
21.2 定义性 (Definability) 182  
21.3 Scott定理 (Scott’s Theorem) 184  
21.4 无类型即单类型 (Untyped Means Uni-Typed) 186  
21.5 备注 (Notes) 187

第22章 动态类型化 (Chapter 22: Dynamic Typing) 189  
22.1 动态类型化的PCF (Dynamically Typed PCF) 189  
22.2 变化与扩展 (Variations and Extensions) 192  
22.3 动态类型化的批判 (Critique of Dynamic Typing) 194  
22.4 备注 (Notes) 195

第23章 混合类型化 (Chapter 23: Hybrid Typing) 198  
23.1 混合语言 (A Hybrid Language) 198  
23.2 动态即静态类型化 (Dynamic as Static Typing) 200  
23.3 动态类型化的优化 (Optimization of Dynamic Typing) 201  
23.4 静态类型化与动态类型化的对比 (Static versus Dynamic Typing) 203  
23.5 备注 (Notes) 204

### 第十部分 子类型化 (Part X: Subtyping)

第24章 结构子类型化 (Chapter 24: Structural Subtyping) 207  
24.1 吞并 (Subsumption) 207  
24.2 子类型化的多样性 (Varieties of Subtyping) 208  
24.3 变型 (Variance) 211  
24.4 动态性与安全性 (Dynamics and Safety) 215  
24.5 备注 (Notes) 216

第25章 行为类型化 (Chapter 25: Behavioral Typing) 219  
25.1 静态性 (Statics) 220  
25.2 布尔失明 (Boolean Blindness) 226  
25.3 精炼安全性 (Refinement Safety) 228  
25.4 备注 (Notes) 229

### 第十一部分 动态调度 (Part XI: Dynamic Dispatch)

第26章 类与方法 (Chapter 26: Classes and Methods) 235  
26.1 调度矩阵 (The Dispatch Matrix) 235  
26.2 基于类的组织 (Class-Based Organization) 238  
26.3 基于方法的组织 (Method-Based Organization) 239  
26.4 自引用 (Self-Reference) 240  
26.5 备注 (Notes) 242

第27章 继承 (Chapter 27: Inheritance) 245  
27.1 类与方法的扩展 (Class and Method Extension) 245  
27.2 基于类的继承 (Class-Based Inheritance) 246  
27.3 基于方法的继承 (Method-Based Inheritance) 248  
27.4 备注 (Notes) 249

### 第十二部分 控制流 (Part XII: Control Flow)

第28章 控制栈 (Chapter 28: Control Stacks) 253  
28.1 机器定义 (Machine Definition) 253  
28.2 安全性 (Safety) 255  
28.3 K机的正确性 (Correctness of the K Machine) 256  
28.4 备注 (Notes) 259

第29章 异常 (Chapter 29: Exceptions) 260  
29.1 失败 (Failures) 260  
29.2 异常 (Exceptions) 262  
29.3 异常值 (Exception Values) 263  
29.4 备注 (Notes) 264

第30章 持续性 (Chapter 30: Continuations) 266  
30.1 概述 (Overview) 266  
30.2 持续性动态 (Continuation Dynamics) 268  
30.3 通过持续性实现协程 (Coroutines from Continuations) 269  
30.4 备注 (Notes) 272

### 第十三部分 符号数据 (Part XIII: Symbolic Data)

第31章 符号 (Chapter 31: Symbols) 277  
31.1 符号声明 (Symbol Declaration) 277  
31.2 符号引用 (Symbol References) 280  
31.3 备注 (Notes) 282

第32章 流体绑定 (Chapter 32: Fluid Binding) 284  
32.1 静态性 (Statics) 284  
32.2 动态性 (Dynamics) 285  
32.3 类型安全性 (Type Safety) 286  
32.4 一些细微之处 (Some Subtleties) 287  
32.5 流体引用 (Fluid References) 288  
32.6 备注 (Notes) 289

第33章 动态分类 (Chapter 33: Dynamic Classification) 291  
33.1 动态类 (Dynamic Classes) 291  
33.2 类引用 (Class References) 293  
33.3 动态类的定义性 (Definability of Dynamic Classes) 294  
33.4 动态分类的应用 (Applications of Dynamic Classification) 295  
33.5 备注 (Notes) 296

### 第十四部分 可变状态 (Part XIV: Mutable State)

第34章 现代化的Algol (Chapter 34: Modernized Algol) 301  
34.1 基本命令 (Basic Commands) 301  
34.2 一些编程习语 (Some Programming Idioms) 306  
34.3 类型化命令与类型化可赋值项 (Typed Commands and Typed Assignables) 307  
34.4 备注 (Notes) 310

第35章 可赋值引用 (Chapter 35: Assignable References) 313  
35.1 能力 (Capabilities) 313  
35.2 作用域可赋值项 (Scoped Assignables) 314  
35.3 自由可赋值项 (Free Assignables) 316  
35.4 安全性 (Safety) 318  
35.5 良性效果 (Benign Effects) 320  
35.6 备注 (Notes) 321

第36章 惰性求值 (Chapter 36: Lazy Evaluation) 323  
36.1 需要按需进行的PCF (PCF By-Need) 323  
36.2 需要按需进行的PCF的安全性 (Safety of PCF By-Need) 326  
36.3 需要按需进行的FPC (FPC By-Need) 328  
36.4 暂停类型 (Suspension Types) 329  
36.5 备注 (Notes) 331

### 第十五部分 并行性 (Part XV: Parallelism)

第37章 嵌套并行性 (Chapter 37: Nested Parallelism) 335  
37.1 二元叉-连接 (Binary Fork-Join) 335  
37.2 成本动态性 (Cost Dynamics) 338  
37.3 多叉-连接 (Multiple Fork-Join) 341  
37.4 有界实现 (Bounded Implementations) 342  
37.5 调度 (Scheduling) 346  
37.6 备注 (Notes) 348

第38章 Futures与推测 (Chapter 38: Futures and Speculations) 350  
38.1 Futures (Futures) 350  
38.2 推测 (Speculations) 351  
38.3 并行动态性 (Parallel Dynamics) 352  
38.4 通过Futures进行流水线 (Pipelining with Futures) 354  
38.5 备注 (Notes) 356

### 第十六部分 并发性与分布性 (Part XVI: Concurrency and Distribution)

第39章 过程演算 (Chapter 39: Process Calculus) 359  
39.1 行动与事件 (Actions and Events) 359  
39.2 交互 (Interaction) 361  
39.3 复制 (Replication) 363  
39.4 分配通道 (Allocating Channels) 364  
39.5 通信 (Communication) 366  
39.6 通道传递 (Channel Passing) 369  
39.7 普遍性 (Universality) 371  
39.8 备注 (Notes) 372

第40章 并发的Algol (Chapter 40: Concurrent Algol) 375  
40.1 并发的Algol (Concurrent Algol) 375  
40.2 广播通信 (Broadcast Communication) 378  
40.3 选择性通信 (Selective Communication) 380  
40.4 作为过程的自由可赋值项 (Free Assignables as Processes) 382  
40.5 备注 (Notes) 383

第41章 分布式Algol (Chapter 41: Distributed Algol) 385  
41.1 静态性 (Statics) 385  
41.2 动态性 (Dynamics) 388  
41.3 安全性 (Safety) 390  
41.4 备注 (Notes) 391

### 第十七部分 模块化 (Part XVII: Modularity)

第42章 模块化与链接 (Chapter 42: Modularity and Linking) 395  
42.1 简单单元与链接 (Simple Units and Linking) 395  
42.2 初始化与效果 (Initialization and Effects) 396  
42.3 备注 (Notes) 398

第43章 单例种类与子种类 (Chapter 43: Singleton Kinds and Subkinding) 399  
43.1 概述 (Overview) 399  
43.2 单例 (Singletons) 400  
43.3 依赖种类 (Dependent Kinds) 402  
43.4 高阶单例 (Higher Singletons) 405  
43.5 备注 (Notes) 407

第44章 类型抽象与类型类 (Chapter 44: Type Abstractions and Type Classes) 409  
44.1 类型抽象 (Type Abstraction) 410  
44.2 类型类 (Type Classes) 412  
44.3 模块语言 (A Module Language) 414  
44.4 一等与二等 (First- and Second-Class) 418  
44.5 备注 (Notes) 419

第45章 层次结构与参数化 (Chapter 45: Hierarchy and Parameterization) 422  
45.1 层次结构 (Hierarchy) 422  
45.2 抽象 (Abstraction) 425  
45.3 层次结构与抽象 (Hierarchy and Abstraction) 427  
45.4 应用型函子 (Applicative Functors) 429  
45.5 备注 (Notes) 431

### 第十八部分 方程式推理 (Part XVIII: Equational Reasoning)

第46章 系统T的等式 (Chapter 46: Equality for System T) 435  
46.1 观察等价 (Observational Equivalence) 435  
46.2 逻辑等价 (Logical Equivalence) 439  
46.3 逻辑与观察等价的吻合 (Logical and Observational Equivalence Coincide) 440  
46.4 一些等式定律 (Some Laws of Equality) 443  
46.5 备注 (Notes) 444

第47章 系统PCF的等式 (Chapter 47: Equality for System PCF) 445  
47.1 观察等价 (Observational Equivalence) 445  
47.2 逻辑等价 (Logical Equivalence) 446  
47.3 逻辑与观察等价的吻合 (Logical and Observational Equivalence Coincide) 446  
47.4 紧凑性 (Compactness) 449  
47.5 惰性自然数 (Lazy Natural Numbers) 452  
47.6 备注 (Notes) 453

第48章 参数性 (Chapter 48: Parametricity) 454  
48.1 概述 (Overview) 454  
48.2 观察等价 (Observational Equivalence) 455  
48.3 逻辑等价 (Logical Equivalence) 456  
48.4 参数性属性 (Parametricity Properties) 461  
48.5 表示独立性，重述 (Representation Independence, Revisited) 464  
48.6 备注 (Notes) 465

第49章 过程等价 (Chapter 49: Process Equivalence) 467  
49.1 过程演算 (Process Calculus) 467  
49.2 强等价 (Strong Equivalence) 469  
49.3 弱等价 (Weak Equivalence) 472  
49.4 备注 (Notes) 473

### 附录 (Part XIX: Appendices)

附录A: 有限集的背景知识 (Background on Finite Sets) 477  
参考文献 (Bibliography) 479  
索引 (Index) 487
