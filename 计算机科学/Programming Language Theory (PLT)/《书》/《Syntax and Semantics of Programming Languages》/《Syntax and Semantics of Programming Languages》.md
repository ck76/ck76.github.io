### 目录 (Contents)

**第1章 语法的定义** (Chapter 1: Specifying Syntax) 1  
1.1  文法与BNF (Grammars and BNF) 2  
- 上下文无关文法 (Context-Free Grammars) 4  
- 上下文相关文法 (Context-Sensitive Grammars) 8  
- 练习 (Exercises) 8  

1.2  编程语言Wren (The Programming Language Wren) 10  
- 歧义性 (Ambiguity) 12  
- Wren中的上下文约束 (Context Constraints in Wren) 13  
- Wren中的语义错误 (Semantic Errors in Wren) 15  
- 练习 (Exercises) 16  

1.3  BNF的变体 (Variants of BNF) 18  
- 练习 (Exercises) 20  

1.4  抽象语法 (Abstract Syntax) 21  
- 抽象语法树 (Abstract Syntax Trees) 21  
- 编程语言的抽象语法 (Abstract Syntax of a Programming Language) 23  
- 练习 (Exercises) 29  

1.5  延伸阅读 (Further Reading) 30  

**第2章 实验活动导论** (Chapter 2: Introduction to Laboratory Activities) 31  
2.1  扫描 (Scanning) 33  
- 练习 (Exercises) 39  

2.2  逻辑文法 (Logic Grammars) 40  
- 逻辑文法的动机 (Motivating Logic Grammars) 41  
- 改进解析器 (Improving the Parser) 44  
- Prolog文法规则 (Prolog Grammar Rules) 46  
- 文法中的参数 (Parameters in Grammars) 47  
- 在逻辑文法中执行目标 (Executing Goals in a Logic Grammar) 49  
- 练习 (Exercises) 49  

2.3  解析Wren (Parsing Wren) 50  
- 处理左递归 (Handling Left Recursion) 52  
- 左因子化 (Left Factoring) 55  
- 练习 (Exercises) 56  

2.4  延伸阅读 (Further Reading) 57  

**第3章 属性文法** (Chapter 3: Attribute Grammars) 59  
3.1  概念与示例 (Concepts and Examples) 59  
- 属性文法示例 (Examples of Attribute Grammars) 60  
- 形式定义 (Formal Definitions) 66  
- 通过属性文法进行语义化 (Semantics via Attribute Grammars) 67  
- 练习 (Exercises) 71  

3.2  Wren的属性文法 (An Attribute Grammar for Wren) 74  
- 符号表 (The Symbol Table) 74  
- 命令 (Commands) 80  
- 表达式 (Expressions) 82  
- 练习 (Exercises) 90  

3.3  实验：上下文检查Wren (Laboratory: Context Checking Wren) 92  
- 声明 (Declarations) 96  
- 命令 (Commands) 99  
- 表达式 (Expressions) 101  
- 练习 (Exercises) 102  

3.4  延伸阅读 (Further Reading) 103  

**第4章 两级文法** (Chapter 4: Two-Level Grammars) 105  
4.1  概念与示例 (Concepts and Examples) 105  
- Fortran字符串字面量 (Fortran String Literals) 111  
- 派生树 (Derivation Trees) 113  
- 练习 (Exercises) 115  

4.2  Wren的两级文法 (A Two-Level Grammar for Wren) 116  
- 声明 (Declarations) 117  
- 命令与表达式 (Commands and Expressions) 124  
- 练习 (Exercises) 132  

4.3  两级文法与Prolog (Two-Level Grammars and Prolog) 132  
- 在Prolog中实现两级文法 (Implementing Two-Level Grammars in Prolog) 133  
- 两级文法与逻辑编程 (Two-Level Grammars and Logic Programming) 136  
- 练习 (Exercises) 138  

4.4  延伸阅读 (Further Reading) 138  

**第5章 Lambda演算** (Chapter 5: The Lambda Calculus) 139  
5.1  概念与示例 (Concepts and Examples) 140  
- Lambda演算的语法 (Syntax of the Lambda Calculus) 140  
- Curry函数 (Curried Functions) 143  
- Lambda表达式的语义 (Semantics of Lambda Expressions) 145  
- 练习 (Exercises) 146  

5.2  Lambda简化 (Lambda Reduction) 147  
- 简化策略 (Reduction Strategies) 151  
- 与参数传递的关系 (Correlation with Parameter Passing) 155  
- 纯Lambda演算中的常量 (Constants in the Pure Lambda Calculus) 156  
- 函数式编程语言 (Functional Programming Languages) 158  
- 练习 (Exercises) 158  

5.3  实验：Lambda演算求值器 (Laboratory: A Lambda Calculus Evaluator) 160  
- 扫描器与解析器 (Scanner and Parser) 160  
- Lambda演算求值器 (The Lambda Calculus Evaluator) 162  
- 练习 (Exercises) 165  

5.4  延伸阅读 (Further Reading) 166  

**第6章 编程语言的自定义** (Chapter 6: Self-Definition of Programming Languages) 167  
6.1  Lisp的自定义 (Self-Definition of Lisp) 167  
- 元循环解释器 (Metacircular Interpreter) 169  
- 运行解释器 (Running the Interpreter) 174  
- 练习 (Exercises) 178  

6.2  Prolog的自定义 (Self-Definition of Prolog) 179  
- 显示失败 (Displaying Failure) 181  
- 练习 (Exercises) 185  

6.3  延伸阅读 (Further Reading) 185  

**第7章 翻译语义** (Chapter 7: Translational Semantics) 187  
7.1  概念与示例 (Concepts and Examples) 187  
- 程序翻译 (A Program Translation) 189  
- 练习 (Exercises) 191  

7.2  属性文法的代码生成 (Attribute Grammar Code Generation) 191  
- 表达式 (Expressions) 193  
- 命令 (Commands) 201  
- 练习 (Exercises) 213  

7.3  实验：实现代码生成 (Laboratory: Implementing Code Generation) 215  
- 命令 (Commands) 217  
- 表达式 (Expressions) 219  
- 练习 (Exercises) 221  

7.4  延伸阅读 (Further Reading) 222  

**第8章 传统操作语义** (Chapter 8: Traditional Operational Semantics) 223  
8.1  概念与示例 (Concepts and Examples) 224  
- VDL (VDL) 226  
- 练习 (Exercises) 227  

8.2  SECD：一个抽象机器 (SECD: An Abstract Machine) 228  
- 示例 (Example) 231  
- 参数传递 (Parameter Passing) 232  
- 静态作用域 (Static Scoping) 233  
- 练习 (Exercises) 234  

8.3  实验：实现SECD机器 (Laboratory: Implementing the SECD Machine) 235  
- 练习 (Exercises) 237  

8.4  结构操作语义：介绍 (Structural Operational Semantics: Introduction) 238  
- 定义语法 (Specifying Syntax) 239  
- 推理系统与结构归纳 (Inference Systems and Structural Induction) 242  
- 练习 (Exercises) 244  

8.5  结构操作语义：表达式 (Structural Operational Semantics: Expressions) 245  
- Wren中表达式的语义 (Semantics of Expressions in Wren) 245  
- 示例 (Example) 248  
- 结果 (Outcomes) 250  
- 练习 (Exercises) 252  

8.6  结构操作语义：命令 (Structural Operational Semantics: Commands) 253  
- 示例计算 (A Sample Computation) 256  
- 语义等价性 (Semantic Equivalence) 260  
- 自然语义 (Natural Semantics) 261  
- 练习 (Exercises) 262  

8.7  实验：实现结构操作语义 (Laboratory: Implementing Structural Operational Semantics) 264  
- 命令 (Commands) 265  
- 表达式 (Expressions) 267  
- 顶层驱动 (Top-Level Driver) 268  
- 练习 (Exercises) 269  

8.8  延伸阅读 (Further Reading) 269  

**第9章 释义语义** (Chapter 9: Denotational Semantics) 271  
9.1  概念与示例 (Concept

s and Examples) 271  
- 句法世界 (The Syntactic World) 272  
- 语义世界 (The Semantic World) 273  
- 组合性 (Compositionality) 276  
- 练习 (Exercises) 277  

9.2  一个计算器语言 (A Calculator Language) 277  
- 计算器语义 (Calculator Semantics) 280  
- 语义函数 (Semantic Functions) 282  
- 示例计算 (A Sample Calculation) 283  
- 练习 (Exercises) 284  

9.3  Wren的释义语义 (The Denotational Semantics of Wren) 285  
- 语义域 (Semantic Domains) 286  
- Wren中的语言构造 (Language Constructs in Wren) 288  
- 辅助函数 (Auxiliary Functions) 290  
- 语义方程 (Semantic Equations) 290  
- 错误处理 (Error Handling) 293  
- 语义等价性 (Semantic Equivalence) 294  
- 输入与输出 (Input and Output) 294  
- 制定释义定义 (Elaborating a Denotational Definition) 296  
- 练习 (Exercises) 302  

9.4  实验：实现释义语义 (Laboratory: Implementing Denotational Semantics) 304  
- 练习 (Exercises) 309  

9.5  环境下的释义语义 (Denotational Semantics with Environments) 310  
- 环境 (Environments) 311  
- 存储 (Stores) 312  
- 语义函数 (Semantic Functions) 313  
- 语义方程 (Semantic Equations) 316  
- 过程 (Procedures) 318  
- 练习 (Exercises) 321  

9.6  上下文相关语法检查 (Checking Context-Sensitive Syntax) 323  
- 练习 (Exercises) 327  

9.7  续延语义 (Continuation Semantics) 328  
- 续延 (Continuations) 331  
- 编程语言Gull (The Programming Language Gull) 333  
- 辅助函数 (Auxiliary Functions) 335  
- 语义方程 (Semantic Equations) 336  
- 错误续延 (The Error Continuation) 336  
- 练习 (Exercises) 338  

9.8  延伸阅读 (Further Reading) 339  

**第10章 域理论与不动点语义** (Chapter 10: Domain Theory and Fixed-Point Semantics) 341  
10.1  概念与示例 (Concepts and Examples) 341  
- 函数的递归定义 (Recursive Definitions of Functions) 342  
- 集合的递归定义 (Recursive Definitions of Sets (Types)) 343  
- 非终止的建模 (Modeling Nontermination) 344  
- 练习 (Exercises) 345  

10.2  域理论 (Domain Theory) 345  
- 基本域 (Elementary Domains) 348  
- 笛卡尔积域 (Product Domains) 349  
- 和域（不交并）(Sum Domains (Disjoint Unions)) 351  
- 函数域 (Function Domains) 355  
- 域上函数的连续性 (Continuity of Functions on Domains) 361  
- 练习 (Exercises) 363  

10.3  不动点语义 (Fixed-Point Semantics) 365  
- 第一步 (First Step) 366  
- 第二步 (Second Step) 368  
- 连续函子 (Continuous Functionals) 374  
- 非递归函数的不动点 (Fixed points for Nonrecursive Functions) 379  
- 重新审视释义语义 (Revisiting Denotational Semantics) 380  
- 不动点归纳 (Fixed-Point Induction) 382  
- 练习 (Exercises) 384  

10.4  实验：Lambda演算中的递归 (Laboratory: Recursion in the Lambda Calculus) 388  
- 条件表达式 (Conditional Expressions) 390  
- 悖论组合子 (Paradoxical Combinator) 390  
- 不动点恒等式 (Fixed-Point Identity) 392  
- 练习 (Exercises) 393  

10.5  延伸阅读 (Further Reading) 394  

**第11章 公理语义** (Chapter 11: Axiomatic Semantics) 395  
11.1  概念与示例 (Concepts and Examples) 395  
- 编程语言的公理语义 (Axiomatic Semantics of Programming Languages) 396  

11.2  Wren的公理语义 (Axiomatic Semantics for Wren) 398  
- 赋值命令 (Assignment Command) 398  
- 输入与输出 (Input and Output) 400  
- 推理规则 (Rules of Inference) 401  
- While命令与循环不变式 (While Command and Loop Invariants) 405  
- 更多的循环不变式 (More on Loop Invariants) 408  
- 嵌套的While循环 (Nested While Loops) 410  
- 练习 (Exercises) 415  

11.3  Pelican的公理语义 (Axiomatic Semantics for Pelican) 418  
- 代码块 (Blocks) 420  
- 非递归过程 (Nonrecursive Procedures) 422  
- 递归过程 (Recursive Procedures) 425  
- 练习 (Exercises) 429  

11.4  证明终止 (Proving Termination) 432  
- 证明终止的步骤 (Steps in Showing Termination) 433  
- 递归过程的终止性 (Termination of Recursive Procedures) 435  
- 练习 (Exercises) 436  

11.5  程序推导简介 (Introduction to Program Derivation) 437  
- 立方表 (Table of Cubes) 437  
- 二分查找 (Binary Search) 440  
- 练习 (Exercises) 441  

11.6  延伸阅读 (Further Reading) 442  

**第12章 代数语义** (Chapter 12: Algebraic Semantics) 443  
12.1  概念与示例 (Concepts and Examples) 444  
- 一个真值模块 (A Module for Truth Values) 446  
- 模块语法 (Module Syntax) 447  
- 自然数模块 (A Module for Natural Numbers) 448  
- 字符模块 (A Module for Characters) 452  
- 一个参数化模块及其实例化 (A Parameterized Module and Some Instantiations) 453  
- 一个有限映射模块 (A Module for Finite Mappings) 456  
- 练习 (Exercises) 459  

12.2  数学基础 (Mathematical Foundations) 460  
- 基础项 (Ground Terms) 461  
- Σ-代数 (Σ-Algebras) 461  
- 从方程式生成的同余关系 (A Congruence from the Equations) 463  
- 商代数 (The Quotient Algebra) 465  
- 同态 (Homomorphisms) 466  
- 一致性与完备性 (Consistency and Completeness) 467  
- 练习 (Exercises) 469  

12.3  使用代数规范 (Using Algebraic Specifications) 471  
- 数据抽象 (Data Abstraction) 471  
- 无界队列的模块 (A Module for Unbounded Queues) 472  
- 作为无界数组实现的队列 (Implementing Queues as Unbounded Arrays) 474  
- 队列公理的验证 (Verification of Queue Axioms) 477  
- 将ADT作为代数 (ADTs As Algebras) 477  
- 抽象语法与代数规范 (Abstract Syntax and Algebraic Specifications) 481  
- 练习 (Exercises) 485  

12.4  Wren的代数语义 (Algebraic Semantics for Wren) 487  
- Wren中的类型和值 (Types and Values in Wren) 488  
- Wren的抽象语法 (Abstract Syntax for Wren) 489  
- Wren的类型检查器 (A Type Checker for Wren) 490  
- Wren的解释器 (An Interpreter for Wren) 494  
- Wren系统 (A Wren System) 498  
- 练习 (Exercises) 499  

12.5  实验：实现代数语义 (Laboratory: Implementing Algebraic Semantics) 499  
- 布尔模块 (Module Booleans) 500  
- 自然数模块 (Module Naturals) 501  
- 声明 (Declarations) 503  
- 命令 (Commands) 503  
- 表达式 (Expressions) 505  
- 练习 (Exercises) 505  

12.6  延伸阅读 (Further Reading) 506  

**第13章 行为语义** (Chapter 13: Action Semantics) 507  
13.1  概念与示例 (Concepts and Examples) 508  
- 数据与排序 (Data and Sorts) 511  
- 值产出器 (Yielders) 514  


- 行为 (Actions) 515  
- 函数性层面 (The Functional Facet) 515  
- 命令性层面 (The Imperative Facet) 518  
- 练习 (Exercises) 520  

13.2  计算器的行为语义 (Action Semantics of a Calculator) 522  
- 语义函数 (Semantic Functions) 523  
- 语义方程 (Semantic Equations) 524  
- 示例计算 (A Sample Calculation) 528  
- 练习 (Exercises) 530  

13.3  声明性层面与Wren (The Declarative Facet and Wren) 531  
- 编程语言Wren (The Programming Language Wren) 534  
- 练习 (Exercises) 540  

13.4  反射性层面与Pelican (The Reflective Facet and Pelican) 541  
- 反射性层面与过程 (The Reflective Facet and Procedures) 545  
- 无参数过程 (Procedures Without Parameters) 547  
- 带参数的过程 (Procedures With A Parameter) 548  
- 递归定义 (Recursive Definitions) 550  
- 翻译为行为符号 (Translating to Action Notation) 551  
- 练习 (Exercises) 558  

13.5  实验：翻译为行为符号 (Laboratory: Translating into Action Notation) 559  
- 练习 (Exercises) 563  

13.6  延伸阅读 (Further Reading) 563  

**附录A** (Appendix A: Logic Programming with Prolog) 565  
- Prolog (Prolog) 566  
- Prolog的BNF语法 (BNF Syntax for Prolog) 568  
- 一个Prolog示例 (A Prolog Example) 569  
- 预定义谓词 (Predefined Predicates) 571  
- Prolog中的递归 (Recursion in Prolog) 572  
- Prolog的控制方面 (Control Aspects of Prolog) 574  
- Prolog中的列表 (Lists in Prolog) 575  
- Prolog中的排序 (Sorting in Prolog) 581  
- 逻辑变量 (The Logical Variable) 582  
- Prolog中的相等与比较 (Equality and Comparison in Prolog) 583  
- 输入与输出谓词 (Input and Output Predicates) 585  

**附录B** (Appendix B: Functional Programming with Scheme) 587  
- Lisp (Lisp) 588  
- Scheme的语法 (Scheme Syntax) 589  
- S表达式上的函数 (Functions on S-expressions) 590  
- Scheme中的列表 (Lists in Scheme) 591  
- 函数的语法 (Syntax for Functions) 592  
- Scheme求值 (Scheme Evaluation) 593  
- 特殊形式 (Special Forms) 596  
- 在Scheme中定义函数 (Defining Functions in Scheme) 596  
- 递归定义 (Recursive Definitions) 598  
- 列表上的递归函数 (Recursive Functions on Lists) 599  
- Scheme中的作用域规则 (Scope Rules in Scheme) 603  
- 在Scheme中证明正确性 (Proving Correctness in Scheme) 605  
- 高阶函数 (Higher-Order Functions) 606  
- 柯里化 (Currying) 608  
- 尾递归 (Tail Recursion) 609  

**参考文献** (Bibliography) 611  

**索引** (Index) 625  