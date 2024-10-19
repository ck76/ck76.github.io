

以下是《Implementing Functional Languages: A Tutorial》的目录中英对照版：

### 目录 (Contents)

**序言** (Preface) 5

**第1章 核心语言** (Chapter 1: The Core Language) 10  
1.1 核心语言概述 (An Overview of the Core Language) 11  
1.2 核心语言的语法 (Syntax of the Core Language) 15  
1.3 核心语言的数据类型 (Data Types for the Core Language) 16  
1.4 一个小的标准预备 (A Small Standard Prelude) 19  
1.5 核心语言的美化打印器 (A Pretty-Printer for the Core Language) 20  
1.6 核心语言的解析器 (A Parser for the Core Language) 28

**第2章 模板实例化** (Chapter 2: Template Instantiation) 42  
2.1 模板实例化的回顾 (A Review of Template Instantiation) 42  
2.2 状态转换系统 (State Transition Systems) 48  
2.3 第1版：最小化模板实例化图形化简器 (Mark 1: A Minimal Template Instantiation Graph Reducer) 50  
2.4 第2版：let(rec)表达式 (Mark 2: let(rec) Expressions) 62  
2.5 第3版：添加更新 (Mark 3: Adding Updating) 63  
2.6 第4版：添加算术运算 (Mark 4: Adding Arithmetic) 65  
2.7 第5版：结构化数据 (Mark 5: Structured Data) 68  
2.8 替代实现 (Alternative Implementations) 74  
2.9 垃圾收集 (Garbage Collection) 76

**第3章 G机** (Chapter 3: The G-machine) 83  
3.1 G机简介 (Introduction to the G-machine) 83  
3.2 构建模板的代码序列 (Code Sequences for Building Templates) 85  
3.3 第1版：最小化G机 (Mark 1: A Minimal G-machine) 89  
3.4 第2版：实现惰性计算 (Mark 2: Making It Lazy) 102  
3.5 第3版：let(rec)表达式 (Mark 3: let(rec) Expressions) 105  
3.6 第4版：添加原语 (Mark 4: Adding Primitives) 111  
3.7 第5版：更好地处理算术运算 (Mark 5: Towards Better Handling of Arithmetic) 119  
3.8 第6版：添加数据结构 (Mark 6: Adding Data Structures) 123  
3.9 第7版：进一步改进 (Mark 7: Further Improvements) 131  
3.10 结论 (Conclusions) 141

**第4章 TIM：三指令机** (Chapter 4: TIM: The Three Instruction Machine) 143  
4.1 背景：TIM的工作原理 (Background: How TIM Works) 143  
4.2 第1版：最小化TIM (Mark 1: A Minimal TIM) 151  
4.3 第2版：添加算术运算 (Mark 2: Adding Arithmetic) 161  
4.4 第3版：let(rec)表达式 (Mark 3: let(rec) Expressions) 167  
4.5 第4版：更新 (Mark 4: Updating) 172  
4.6 第5版：结构化数据 (Mark 5: Structured Data) 183  
4.7 第6版：常量应用形式和代码存储 (Mark 6: Constant Applicative Forms and the Code Store) 189  
4.8 总结 (Summary) 192

**第5章 并行G机** (Chapter 5: A Parallel G-machine) 196  
5.1 简介 (Introduction) 196  
5.2 第1版：最小化并行G机 (Mark 1: A Minimal Parallel G-machine) 200  
5.3 第2版：求值即消亡模型 (Mark 2: The Evaluate-and-Die Model) 209  
5.4 第3版：现实的并行G机 (Mark 3: A Realistic Parallel G-machine) 212  
5.5 第4版：更好的阻塞处理方法 (Mark 4: A Better Way to Handle Blocking) 214  
5.6 结论 (Conclusions) 216

**第6章 Lambda 提升** (Chapter 6: Lambda Lifting) 217  
6.1 简介 (Introduction) 217  
6.2 改进expr数据类型 (Improving the Expr Data Type) 217  
6.3 第1版：简单的Lambda提升器 (Mark 1: A Simple Lambda Lifter) 221  
6.4 第2版：改进简单的Lambda提升器 (Mark 2: Improving the Simple Lambda Lifter) 230  
6.5 第3版：Johnsson风格的Lambda提升 (Mark 3: Johnsson-Style Lambda Lifting) 231  
6.6 第4版：单独的完全惰性传递 (Mark 4: A Separate Full Laziness Pass) 236  
6.7 第5版：完全惰性的改进 (Mark 5: Improvements to Full Laziness) 250  
6.8 第6版：依赖性分析 (Mark 6: Dependency Analysis) 252  
6.9 结论 (Conclusion) 260

**附录A 实用模块** (Appendix A: Utilities Module) 262  
A.1 堆类型 (The Heap Type) 262  
A.2 关联列表类型 (The Association List Type) 264  
A.3 生成唯一名称 (Generating Unique Names) 265  
A.4 集合 (Sets) 265  
A.5 其他有用的函数定义 (Other Useful Function Definitions) 267

**附录B 核心语言程序示例** (Appendix B: Example Core-Language Programs) 268  
B.1 基本程序 (Basic Programs) 268  
B.2 let和letrec (let and letrec) 269  
B.3 算术运算 (Arithmetic) 269  
B.4 数据结构 (Data Structures) 270

希望这份目录对你有所帮助！如果需要进一步的帮助，请告诉我。