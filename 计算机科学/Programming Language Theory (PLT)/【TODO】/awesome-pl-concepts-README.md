[toc]

| [原理](rationale.md) | [约定](convention.md) | [贡献](contributing.md) | [TODO](TODO.md) |

注意：某些功能尚未有名称，我可能需要自创术语。如果你找到更好的术语，请提交 PR。

## 按主题分类

- 缓存： [使用响应性无效化的缓存](#caching-with-reactive-invalidation)

- 微积分： [Dot 微积分](#dot-calculus), [Lambda 微积分](#lambda-calculus), [对称交互微积分](#symmetric-interaction-calculus), [术语重写](#term-rewriting), [图灵机](#turing-machine)

- 代码生成： [语法卫生宏](#hygienic-macro), [Lisp 宏](#lisp-macro), [阅读器宏](#reader-macro)

- 依赖： [基于片段的代码分发](#fragment-based-code-distribution)

- 设计模式： [异步/等待](#asyncawait), [Continuation Passing](#continuation-passing), [消息传递](#message-passing), [Trait](#trait)

- 模式匹配： [Active Pattern](#active-pattern), [模式匹配](#pattern-matching), [视图模式](#view-pattern)

- 评估： [编译时代码评估](#compile-time-code-evaluation), [惰性评估](#lazy-evaluation)

- 一等公民： [一等日期](#first-class-date), [一等模块](#first-class-module), [一等正则表达式](#first-class-regex), [一等类型](#first-class-type)

- 内存管理： [垃圾回收](#garbage-collection), [所有权](#ownership), [引用计数](#reference-counting), [内存安全](#memory-safety)

- 多任务： [异步/等待](#asyncawait), [协程](#coroutine), [绿色线程](#green-thread), [对称交互微积分](#symmetric-interaction-calculus), [自由并行](#free-parallelism)

- 范式： [函数式编程](#functional-programming), [逻辑编程](#logic-programming)

- 多态： [行多态](#row-polymorphism)

- 属性： [引用透明性](#referential-transparency)

- 子系统： [Datalog](#datalog), [模块](#module)

- 语法糖： [自动广播实现](#automatic-broadcast-implementation), [链式尝试](#chained-try), [For Comprehension](#for-comphrehension), [泛化更新语法](#generalized-update-syntax), [通用函数调用语法](#universal-function-call-syntax)

- 类型： [代数数据类型](#algebraic-data-type), [能力安全](#capability-safety), [类不变量](#class-invariant), [依赖类型](#dependent-type), [效果系统](#dependent-type), [泛化代数数据类型](#generalized-algebraic-data-type), [渐进类型](#gradual-typing), [线性类型](#linear-type), [Nil Fallthrough](#nil-fallthrough), [引用能力](#reference-capabilities)

## 功能列表

### Actor 模型

### Active Pattern

### 代数数据类型

### [异步/等待](https://en.wikipedia.org/wiki/Async/await)
  - 描述： 一种语法糖，用于编写看起来像串行的[异步](https://en.wikipedia.org/wiki/Async/await) [并发](https://en.wikipedia.org/wiki/Concurrency_(computer_science))代码。
  - 实现： [JavaScript](https://www.javascript.com/) [async-await](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
  - 视频：[Fireship: The Async Await Episode I Promised](https://www.youtube.com/watch?v=vn3tm0quoqE)

### 自动广播实现
  - 描述：给定一个定义 `f: a -> b` 和一个函子 `M`, `f: M a -> M b` 可以自动实现。
  - 实现： [Chapel](https://chapel-lang.org/), [Matlab](https://www.mathworks.com/products/matlab.html) [数组和矩阵语义](https://www.mathworks.com/help/matlab/learn_matlab/matrices-and-arrays.html)（部分实现）
  - 文章：[Hillel Wayne - Microfeatures I'd like to see in more languages](https://buttondown.email/hillelwayne/archive/microfeatures-id-like-to-see-in-more-languages/)

### 使用响应性无效化的缓存
  - 描述：缓存函数的结果，并在缓存可能过时时响应性地使其无效。
  - 实现： [Skip](http://skiplang.com/docs/tutorial.html) [使用响应性无效化的缓存](http://skiplang.com/)

### 能力安全
  - 描述：一种属性，确保函数不能访问其闭包、输入和全局常量之外的数据。通常由[引用透明性](#referential-transparency)隐含，并且与可变全局变量不兼容。
  - 实现：[Pony](https://www.ponylang.io/), [Monte](http://www.monte-language.org/), [Wyvern](https://wyvernlang.github.io/)

### 链式尝试
  - 描述：一种 [JavaScript](https://www.javascript.com/) 中[可选链式调用](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining)的泛化。一个语言结构接受一系列代码块，每个块只有在前面的块出现错误时才会执行。
  - 示例用法：
  ```js
  try {
      assert user_exists(name)
      current_user = get_user(name)
      assert user_not_banned(current_user)
      do_lots_of_stuff_with(current_user)
  } {
      deny_login_of_user(name)
  }
  
  // 替代 if 和绑定，比如 Python 的 walrus 操作符：
  try {
      match = regex.search("\d+(\d)", text)
      assert match
      process_numbers(match[0])
  } {
      match = regex.search("\w+(\d)", text)
      assert match
      process_letters(match[0])
  } {
      print("No match.")
  }
  
  // 替代复杂的单行条件：
  try {
      assert user.age > 18 and user.has_feature_enabled
      assert user.has_billing_account and user.get_credits() > 0
      assert user.get_last_payment().age_in_days < 90
      process_as_premium_user()
  } {
      process_as_normal_user()
  }
  
  // 替代嵌套的 catch 块：
  try {
      response = fetch_from_network(resource_id)
      assert response.network_success
      value = response.value
  } {
      assert resource_id in offline_cache
      value = offline_cache[resource_id]
  } {
      print("Failed to get resource. Use manual value?")
      response = user_input("Resource override: ")
      assert not response.cancelled
      value = response.value
  } {
      raise Error('Network not available and ID {resource_id} not in offline cache.')
  }
  print(value)
  ```
  - 文章：[BoppreH - a Reply in If-fail-else Construct](https://www.reddit.com/r/ProgrammingLanguages/comments/110bx5e/comment/j88xu52)

### 类不变量
  - 描述：特定类型的约束，在函数调用期间确保数据的正确性。
  - 实现：[Ada](https://ada-lang.io/) [类型不变量](http://www.ada-auth.org/standards/12rat/html/Rat12-2-4.html)
  - 文章：[Riccardo Bernardini - Reasons for loving Ada: Type invariants (because bugs shouldn't sleep...)](https://dev.to/pinotattari/reasons-for-loving-ada-type-invariants-because-bugs-shouldn-t-sleep-1082)

### 编译时代码评估
  - 描述：在[编译时](https://en.wikipedia.org/wiki/Compile_time)对代码进行评估。
  - 实现：[Zig](https://ziglang.org/) [comptime](https://ziglang.org/documentation/master/#comptime), [Konna](https://github.com/eashanhatti/konna) [2LTT](https://www.reddit.com/r/ProgrammingLanguages/comments/rpe65y/konna_my_programming_language/)

### [Continuation Passing](https://en.wikipedia.org/wiki/Continuation-passing_style)
  - 描述：一种模式，允许捕获并传递程序的执行状态，并按需恢复。
  - 实现： [Scheme](https://www.scheme.com/) [continuation](https://cs.brown.edu/courses/cs173/2008/Manual/guide/Continuations.html)

### [协程](https://en.wikipedia.org/wiki/Coroutine)
  - 描述：一种语法糖，用于模块化构建[协作式](https://en.wikipedia.org/wiki/Cooperative_multitasking) [并发](https://en.wikipedia.org/wiki

/Concurrency_(computer_science))代码。
  - 实现： [Lua](https://www.lua.org/) [协程](https://www.lua.org/pil/9.1.html)

### [Datalog](https://en.wikipedia.org/wiki/Datalog)

### 依赖类型
  - 相关文章：
    - [Hacker News - serokell.io - Dependent Haskell Is the Future (2018)](https://news.ycombinator.com/item?id=20712646)
    - [pressron - Why Writing Correct Software Is Hard](https://pron.github.io/posts/correctness-and-complexity)

### Dot 微积分
  - 相关文章：[Essense of Scala](https://www.scala-lang.org/blog/2016/02/03/essence-of-scala.html)

### 效果系统
  - 描述：类似类型系统的系统，但跟踪[副作用](https://en.wikipedia.org/wiki/Side_effect_(computer_science))而不是值的类型。
  - 实现： [Koka](https://koka-lang.github.io/koka/doc/index.html) [效果系统](https://en.wikipedia.org/wiki/Effect_system)
  - 相关文章：[Stephen Diehl - Exotic Programming Ideas: Part 3 (Effect Systems)](https://www.stephendiehl.com/posts/exotic03.html)

### 一等日期
  - 实现： [Frink](https://frinklang.org/) [日期/时间处理](https://frinklang.org/#DateTimeHandling)

### 一等模块
  - 描述：模块被视为结构体。
  - 实现： [1ML](https://github.com/rossberg/1ml), [Zig](https://ziglang.org), [Ocaml](https://v2.ocaml.org/) [一等模块](https://v2.ocaml.org/manual/firstclassmodules.html)
  - 相关文章：[Stackoverflow - What (exactly) are "First Class" modules?](https://stackoverflow.com/questions/56575195/what-exactly-are-first-class-modules)

### 一等正则表达式

### 一等类型

### 基于片段的代码分发

- 描述：一种编译器基础设施，通过其哈希识别代码片段。
- 实现： [Haskell](https://www.haskell.org/) [fragnix](https://github.com/fragnix/fragnix), [Unison](https://www.unison-lang.org/) [基于哈希的AST](https://www.unison-lang.org/learn/tour/_big-technical-idea/)
- 相关文章： [Unison中的重大技术理念](https://www.unison-lang.org/learn/tour/_big-technical-idea/)

### 自由并行

- 描述：并行性由语言运行时隐式施加。
- 实现： [Futhark](https://futhark-lang.org/), [HVM](https://github.com/HigherOrderCO/HVM)

### For Comprehension（for理解）

### 形式化方法

- 描述：一系列技术，用于证明程序的行为。
- 实现： [Coq](https://coq.inria.fr/) [证明助手](https://en.wikipedia.org/wiki/Proof_assistant)

### 函数式编程

### [垃圾回收](https://en.wikipedia.org/wiki/Garbage_collection_(computer_science))

- 描述：一个在[运行时](https://en.wikipedia.org/wiki/Runtime_system)释放不再使用的[堆分配](https://en.wikipedia.org/wiki/C_dynamic_memory_allocation)内存的系统。
- 实现： [Common Lisp](https://lisp-lang.org/)

### 泛化代数数据类型

### 泛化更新语法

- 描述：对于任何二元操作函数`f : a -> a -> a`，我们可以将 `a = f a b` 重写为 `a f= b`。
- 实现： [Noulith](https://github.com/betaveros/noulith/) [泛化更新语法](https://github.com/betaveros/noulith/)
- 相关文章： 
    - [Hillel Wayne - 我希望更多语言有的微特性](https://buttondown.email/hillelwayne/archive/microfeatures-id-like-to-see-in-more-languages/)

### [渐进类型](https://en.wikipedia.org/wiki/Gradual_typing)

- 描述：一种类型系统，部分表达式/变量在编译时检查，其他的留给运行时类型检查器处理。
- 实现： [Typescript](https://www.typescriptlang.org/)
- 相关文章：
    - [Vlad Balin - TypeScript: 静态或动态？战争结束了。](https://itnext.io/typescript-static-or-dynamic-64bceb50b93e)

### [绿色线程](https://en.wikipedia.org/wiki/Green_thread)

- 描述：一种抽象，允许轻松编写[抢占式](https://en.wikipedia.org/wiki/Preemption_(computing)) [并发](https://en.wikipedia.org/wiki/Concurrency_(computer_science))代码，而无需实际使用操作系统线程。
- 实现： [Java](https://www.java.com/) 绿色线程

### 语法卫生宏

- 描述： [Lisp 宏](#lisp-macro) 遵循[词法作用域](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scope)。
- 实现： [Scheme](https://www.scheme.com/) [语法卫生宏](https://docs.scheme.org/guide/macros/)

### Lambda 微积分

- 实现： [pLam](https://github.com/slovnicki/pLam)

### 惰性评估

- 描述：只有在需要时才评估代码的任何部分。
- 实现： [Haskell](https://www.haskell.org/) [惰性评估](https://wiki.haskell.org/Lazy_evaluation)

### 线性类型

### Lisp 宏

- 描述： [编译时](https://en.wikipedia.org/wiki/Compile_time) 基于[抽象语法树](https://en.wikipedia.org/wiki/Abstract_syntax_tree)的代码生成，在[同像性](https://en.wikipedia.org/wiki/Homoiconicity)和[动态类型](https://en.wikipedia.org/wiki/Type_system#Dynamic_type_checking_and_runtime_type_information)的语言中实现。
- 实现： [Common Lisp](https://lisp-lang.org/) [宏](https://lispcookbook.github.io/cl-cookbook/macros.html)

### 逻辑编程

- 实现： [Prolog](https://en.wikipedia.org/wiki/Prolog)

### 内存安全

- 相关文章： [Fernando Borretti - 用于内存安全的类型系统](https://borretti.me/article/type-systems-memory-safety)

### 模块

### [消息传递](https://en.wikipedia.org/wiki/Message_passing)

- 实现： [Erlang](https://www.erlang.org/) [并发编程](https://www.erlang.org/doc/getting_started/conc_prog.html), [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk)

### Nil Fallthrough

- 描述： [JavaScript](https://www.javascript.com/) 中[可选链式调用](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining)的泛化，任何对 nil 的操作总是返回 nil。
- 相关文章：
    - [sporeboyofbigness - 行动呼吁：让我们为非平凡编程语言功能建立一个列表](https://www.reddit.com/r/ProgrammingLanguages/comments/156bfwc/comment/jt047a5/?utm_source=share&utm_medium=web2x&context=3)

### 对象能力

- 描述：见[能力安全](#capability-safety)

### 所有权

- 描述：一种系统，通过注释堆分配变量的所有权，帮助编译器静态地决定何时释放[堆分配内存](https://en.wikipedia.org/wiki/C_dynamic_memory_allocation)。
- 实现： [Rust](https://www.rust-lang.org/) [所有权](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html)

### 模式匹配

- 描述：一种结构化方式，从复杂的[数据结构](https://en.wikipedia.org/wiki/Data_structure)中提取数据。
- 实现： [Haskell](https://www.haskell.org/) [模式匹配](https://www.haskell.org/tutorial/patterns.html)

### 阅读器宏

- 描述：在形成抽象语法树之前，由阅读器调用的宏系统。
- 实现： [Common Lisp](https://lisp-lang.org/), [Elixir](https://elixir-lang.org/) [Sigils](https://elixir-lang.org/getting-started/sigils.html)（这是阅读器宏的弱化替代）

### 引用能力

- 描述：一种类型系统特性，其中每个引用携带一个能力修饰符，可以描述是否允许读取或写入，以及这些特性是否仅限于此引用。
- 实现： [Pony](https://www.ponylang.io/)

### 引用计数

### [引用透明性](https://en.wikipedia.org/wiki/Referential_transparency)

- 描述：函数的一种属性，满足以下条件：1. 对相同的参数总是返回相同的结果；2. 无其他副作用。

### [行多态](https://en.wikipedia.org/wiki/Row_polymorphism)

- 描述：一种多态性，通过记录字段及其类型进行调度。
- 实现： [OCaml](https://ocaml.org/) [行多态](https://www.cl.cam.ac.uk/teaching/1415/L28/rows.pdf)
- 相关文章：
    - [Jadon Fowler - 去掉行多态术语的解释](https://jadon.io/blog/row-polymorphism/)
    - [Stackoverflow - 什么是行类型？它们是代数数据类型吗？](https://stackoverflow.com/questions/48092739/what-are-row-types-are-they-algebraic-data-types)
- 论文： [Objective ML: An effective object-oriented extension to ML](https://caml.inria.fr/pub/papers/remy_vouillon-objective_ml-tapos98.pdf)

### 对称交互微积分

- 描述：类似[Lambda 微积分](https://en.wikipedia.org/wiki/Lambda_calculus)的一种微积分，支持并发实现，支持投影和复制。
- 实现： [HVM](https://github.com/HigherOrderCO/HVM)
- 相关文章：
    - [Victor Maia - 对称交互微积分](https://medium.com/@maiavictor/the-abstract-calculus-fe8c46bcf39c)
    - [XXIIVV - 交互网](https://wiki.xxiivv.com/site/interaction_nets.html)
    - [Zicklag - 交互网、组合子和微积分](https://zicklag.github.io/blog/interaction-nets-combinators-calculus/)
- 论文：
    - [交互网](https://dl.acm.org/doi/10.1145/96709.96718)

### 术语重写

- 实现： [Mathematica](https://www.wolfram.com/mathematica/)

### Trait

 描述：允许对不同类型编写类型安全代码的接口。
- 实现： [Haskell](https://www.haskell.org/) [类型类](https://www.haskell.org/tutorial/classes.html), [Rust](https://www.rust-lang.org/) [特质](https://doc.rust-lang.org/book/ch10-02-traits.html)

### 图灵机

### [通用函数调用语法](https://en.wikipedia.org/wiki/Uniform_Function_Call_Syntax)

- 描述：一种函数调用的语法糖，简化了函数调用的链式编写。
- 实现： [D](https://dlang.org/) [UFCS](https://tour.dlang.org/tour/en/gems/uniform-function-call-syntax-ufcs), [Elixir](https://elixir-lang.org/) [管道操作符](https://elixir-lang.org/getting-started/enumerables-and-streams.html#the-pipe-operator), [Clojure](https://clojure.org/) [线程宏](https://clojure.org/guides/threading_macros)

### 视图模式 (View Pattern)

---

这部分内容详细介绍了计算机科学与编程语言中使用的各种技术、范式和语言特性。



---

| [Rationale](rationale.md) | [Convention](convention.md) | [Contributing](contributing.md) | [TODO](TODO.md) |

Note: some features doesn't have a name, I have to coin it out. That said, please make a PR if you find better term for them.  

## By topic

- Caching: [Caching with Reactive Invalidation](#caching-with-reactive-invalidation) 

- Calculus: [Dot Calculus](#dot-calculus), [Lambda Calculus](#lambda-calculus), [Symmetric Interaction Calculus](#symmetric-interaction-calculus), [Term Rewriting](#term-rewriting), [Turing Machine](#turing-machine)

- Code Generation: [Hygienic Macro](#hygienic-macro), [Lisp Macro](#lisp-macro), [Reader Macro](#reader-macro)

- Dependency: [Fragment Based Code Distribution](#fragment-based-code-distribution)

- Design Pattern: [Async/await](#asyncawait), [Continuation Passing](#continuation-passing), [Message Passing](#message-passing), [Trait](#trait)

- Pattern Matching: [Active Pattern](#active-pattern), [Pattern matching](#pattern-matching), [View Pattern](#view-pattern)

- Evaluation: [Compile-time Code Evaluation](#compile-time-code-evaluation), [Lazy Evaluation](#lazy-evaluation)

- First Class: [First Class Date](#first-class-date), [First Class Module](#first-class-module), [First Class Regex](#first-class-regex), [First Class Type](#first-class-type)

- Memory Management: [Garbage Collection](#garbage-collection), [Ownership](#ownership), [Reference Counting](#reference-counting), [Memory Safety](#memory-safety)

- Mutitasking: [Async/await](#asyncawait), [Coroutine](#coroutine), [Green Thread](#green-thread), [Symmetric Interaction Calculus](#symmetric-interaction-calculus), [Free Parallelism](#free-parallelism)

- Paradigm: [Functional Programming](#functional-programming), [Logic Programming](#logic-programming)

- Polymorphism: [Row Polymorphism](#row-polymorphism)

- Property: [Referential Transparency](#referential-transparency)

- Subsystem: [Datalog](#datalog), [Module](#module)

- Syntax Sugar: [Automatic Broadcast Implementation](#automatic-broadcast-implementation), [Chained Try](#chained-try), [For Comprehension](#for-comphrehension), [Generalized Update Syntax](#generalized-update-syntax), [Universal Function Call Syntax](#universal-function-call-syntax)

- Type: [Algebraic Data Type](#algebraic-data-type), [Capability Safety](#capability-safety), [Class Invariant](#class-invariant), [Dependent Type](#dependent-type), [Effect System](#dependent-type), [Generalized Algebraic Data Type](#generalized-algebraic-data-type), [Gradual Typing](#gradual-typing), [Linear Type](#linear-type), [Nil Fallthrough](#nil-fallthrough), [Reference Capabilities](#reference-capabilities)

## List of features

### Actor Model

### Active Pattern

### Algebraic Data Type

### [Async/await](https://en.wikipedia.org/wiki/Async/await)
  - Description: A syntax sugar for writing [asynchronous](https://en.wikipedia.org/wiki/Async/await) [concurrent](https://en.wikipedia.org/wiki/Concurrency_(computer_science)) code that looks serial. 
  - Implementation: [JavaScript](https://www.javascript.com/) [async-await](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
  - Videos: [Fireship: The Async Await Episode I Promised](https://www.youtube.com/watch?v=vn3tm0quoqE)

### Automatic Broadcast Implementation
  - Description: Given a definition `f: a -> b` and a functor `M`, `f: M a -> M b` is implemented automatically.
  - Implementation: [Chapel](https://chapel-lang.org/), [Matlab](https://www.mathworks.com/products/matlab.html) [array and matrix semantic](https://www.mathworks.com/help/matlab/learn_matlab/matrices-and-arrays.html) (partial)
  - Articles: [Hillel Wayne - Microfeatures I'd like to see in more languages](https://buttondown.email/hillelwayne/archive/microfeatures-id-like-to-see-in-more-languages/)

### Caching with Reactive Invalidation
  - Description: Caching the result of function, invalidating the data reactively when the cache are potentially outdated.
  - Implementation: [Skip](http://skiplang.com/docs/tutorial.html) [Caching with reactive invalidation](http://skiplang.com/)

### Capability Safety
  - Description: A property that functions cannot access data besides that reachable via their closure, their inputs, and global constants. Generally implied by [referential transparency](#referential-transparency), and incompatible with mutable global variables.
  - Implementations: [Pony](https://www.ponylang.io/), [Monte](http://www.monte-language.org/), [Wyvern](https://wyvernlang.github.io/)

### Chained Try
  - Description: A generalization of [optional chaining](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining) in [JavaScript](https://www.javascript.com/). A language construct that accepts a series of blocks where each block will be executed only if the previous blocks yield an error. 
  - Example Usage:
  ```js
  try {
      assert user_exists(name)
      current_user = get_user(name)
      assert user_not_banned(current_user)
      do_lots_of_stuff_with(current_user)
  } {
      deny_login_of_user(name)
  }

  // Replacing if-and-bind, like Python's walrus operator:
  try {
      match = regex.search("\d+(\d)", text)
      assert match
      process_numbers(match[0])
  } {
      match = regex.search("\w+(\d)", text)
      assert match
      process_letters(match[0])
  } {
      print("No match.")
  }

  // Replacing complicated conditions that would otherwise be in a single line.
  try {
      assert user.age > 18 and user.has_feature_enabled
      assert user.has_billing_account and user.get_credits() > 0
      assert user.get_last_payment().age_in_days < 90
      process_as_premium_user()
  } {
      process_as_normal_user()
  }

  // Replacing nested catch blocks:
  try {
      response = fetch_from_network(resource_id)
      assert response.network_success
      value = response.value
  } {
      assert resource_id in offline_cache
      value = offline_cache[resource_id]
  } {
      print("Failed to get resource. Use manual value?")
      response = user_input("Resource override: ")
      assert not response.cancelled
      value = response.value
  } {
      raise Error('Network not available and ID {resource_id} not in offline cache.')
  }
  print(value)


  ```
  - Articles: [BoppreH - a Reply in If-fail-else Construct](https://www.reddit.com/r/ProgrammingLanguages/comments/110bx5e/comment/j88xu52)

### Class Invariant
  - Constraints of specific types that gets ran across function calls to ensure the data is in correct shape.
  - Implementation [Ada](https://ada-lang.io/) [type invariant](http://www.ada-auth.org/standards/12rat/html/Rat12-2-4.html)
  - Articles: [Riccardo Bernardini - Reasons for loving Ada: Type invariants (because bugs shouldn't sleep...)](https://dev.to/pinotattari/reasons-for-loving-ada-type-invariants-because-bugs-shouldn-t-sleep-1082)

### Compile-time Code Evaluation
  - Description: Evaluation of code at [compile time](https://en.wikipedia.org/wiki/Compile_time).
  - Implementation: [Zig](https://ziglang.org/) [comptime](https://ziglang.org/documentation/master/#comptime), [Konna](https://github.com/eashanhatti/konna) [2LTT](https://www.reddit.com/r/ProgrammingLanguages/comments/rpe65y/konna_my_programming_language/)

### [Continuation Passing](https://en.wikipedia.org/wiki/Continuation-passing_style)
  - Description: a pattern in which the state of an executing program may be captured and passing around, resume on demand.
  - Implementation: [Scheme](https://www.scheme.com/) [continuation](https://cs.brown.edu/courses/cs173/2008/Manual/guide/Continuations.html)

### [Coroutine](https://en.wikipedia.org/wiki/Coroutine)
  - Description: A syntax sugar for structuring [cooperative](https://en.wikipedia.org/wiki/Cooperative_multitasking) [concurrent](https://en.wikipedia.org/wiki/Concurrency_(computer_science)) code modularly.
  - Implementation: [Lua](https://www.lua.org/) [coroutine](https://www.lua.org/pil/9.1.html)

### [Datalog](https://en.wikipedia.org/wiki/Datalog)

### Dependent Type
  - Articles: 
    - [Hacker News - serokell.io - Dependent Haskell Is the Future (2018)](https://news.ycombinator.com/item?id=20712646)
    - [pressron - Why Writing Correct Software Is Hard](https://pron.github.io/posts/correctness-and-complexity) 

### Dot Calculus
  - Articles: [Essense of Scala](https://www.scala-lang.org/blog/2016/02/03/essence-of-scala.html)

### Effect System
  - Description: A system similar to type system, but tracks [side effects](https://en.wikipedia.org/wiki/Side_effect_(computer_science)) instead of type of the value.
  - Implementation: [Koka](https://koka-lang.github.io/koka/doc/index.html) [effect system](https://en.wikipedia.org/wiki/Effect_system)
  - Articles: [Stephen Diehl - Exotic Programming Ideas: Part 3 (Effect Systems)](https://www.stephendiehl.com/posts/exotic03.html)

### First Class Date
  - Implementation: [Frink](https://frinklang.org/) [Date/Time Handling](https://frinklang.org/#DateTimeHandling)

### First Class Module
  - Description: Modules are treated as structs
  - Implementation: [1ML](https://github.com/rossberg/1ml), [Zig](https://ziglang.org), [Ocaml](https://v2.ocaml.org/) [First class module](https://v2.ocaml.org/manual/firstclassmodules.html)
  - Articles: [Stackoverflow - What (exactly) are "First Class" modules?](https://stackoverflow.com/questions/56575195/what-exactly-are-first-class-modules)

### First Class Regex

### First Class Type

### Fragment-based Code Distribution
  - Description: A compiler infrastructure that identify codes fragment by its hash.
  - Implementaion: [Haskell](https://www.haskell.org/) [fragnix](https://github.com/fragnix/fragnix), [Unison](https://www.unison-lang.org/) [hash identified AST](https://www.unison-lang.org/learn/tour/_big-technical-idea/)
  - Articles: [Big Technical Idea on Unison](https://www.unison-lang.org/learn/tour/_big-technical-idea/)

### Free Parallelism
  - Description: Parallelism is imposed implicitly by the langauge runtime.
  - Implementation: [Futhark](https://futhark-lang.org/), [HVM](https://github.com/HigherOrderCO/HVM)

### For Comprehension

### Formal Methods
  - Description: A series of techiques that helps proving the behavior of the program
  - Implementation: [Coq](https://coq.inria.fr/) [proof assitant](https://en.wikipedia.org/wiki/Proof_assistant)

### Functional Programming

### [Garbage Collection](https://en.wikipedia.org/wiki/Garbage_collection_(computer_science))
  - Description: A system that free unused [heap-allocated](https://en.wikipedia.org/wiki/C_dynamic_memory_allocation) memory in [runtime](https://en.wikipedia.org/wiki/Runtime_system)
  - Implementation: [Common Lisp](https://lisp-lang.org/)

### Generalized Algebraic Data Type

### Generalized Update Syntax
  - Description: for any binary operator function `f : a -> a -> a`, we can rewrite `a = f a b` as `a f= b`
  - Implementation: [Noulith](https://github.com/betaveros/noulith/) [generalized update syntax](https://github.com/betaveros/noulith/)
  - Articles: 
    - [Hillel Wayne - Microfeatures I'd like to see in more languages](https://buttondown.email/hillelwayne/archive/microfeatures-id-like-to-see-in-more-languages/)

### [Gradual Typing](https://en.wikipedia.org/wiki/Gradual_typing)
  - Description: A type system that checks some expresions/variables at compile time while leaving others to the runtime type checker.
  - Implementation: [Typescript](https://www.typescriptlang.org/)
  - Articles:
    - [Vlad Balin - TypeScript: Static or Dynamic? The war is over.](https://itnext.io/typescript-static-or-dynamic-64bceb50b93e)

### [Green Thread](https://en.wikipedia.org/wiki/Green_thread)
  - Description: An abstraction that allow easily writing [preemptive](https://en.wikipedia.org/wiki/Preemption_(computing)) [concurrent](https://en.wikipedia.org/wiki/Concurrency_(computer_science)) code, without actually using OS thread
  - Implementation: [Java](https://www.java.com/) green thread

### Hygienic Macro
  - Description: [Lisp macros](#lisp-macros) that follows [lexical scope](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scope).
  - Implementation: [Scheme](https://www.scheme.com/) [Hygienic macro](https://docs.scheme.org/guide/macros/)

### Lambda Calculus
  - Implementation: [pLam](https://github.com/slovnicki/pLam)

### Lazy Evaluation
  - Description: Any portion of the code is evaluated only when needed.
  - Implementation: [Haskell](https://www.haskell.org/) [lazy evaluation](https://wiki.haskell.org/Lazy_evaluation)

### Linear Type

### Lisp Macro
  - Description: [Compile-time](https://en.wikipedia.org/wiki/Compile_time) [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree)-based code generation in a [homoiconic](https://en.wikipedia.org/wiki/Homoiconicity) and [dynamically-typed](https://en.wikipedia.org/wiki/Type_system#Dynamic_type_checking_and_runtime_type_information) language.
  - Implementation: [Common Lisp](https://lisp-lang.org/) [macros](https://lispcookbook.github.io/cl-cookbook/macros.html)

### Logic Programming
  - Implementation: [Prolog](https://en.wikipedia.org/wiki/Prolog)

### Memory Safety
  - Articles: [Fernando Borretti - Type Systems for Memory Safety](https://borretti.me/article/type-systems-memory-safety)
### Module 

### [Message Passing](https://en.wikipedia.org/wiki/Message_passing)
  - Implementation: [Erlang](https://www.erlang.org/) [concurrent programming](https://www.erlang.org/doc/getting_started/conc_prog.html), [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk)

### Nil Fallthrough
  - Description: A generalization of [optional chaining](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining) in [JavaScript](https://www.javascript.com/), where any operation on nil will always yield nil.
  - Articles: 
    - [sporeboyofbigness - Call for action: let's build a list of features for non-trivial PL features](https://www.reddit.com/r/ProgrammingLanguages/comments/156bfwc/comment/jt047a5/?utm_source=share&utm_medium=web2x&context=3)

### Object Capabilities
  - Description: See [Capability safety](#capability-safety)

### Ownership
  - Description: A system that helps compiler decide when to free [heap-allocated memory](https://en.wikipedia.org/wiki/C_dynamic_memory_allocation) statically by annotating the ownership of heap allocated variables to entities in the program.
  - Implementation: [Rust](https://www.rust-lang.org/) [ownership](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html)

### Pattern matching 
  - Description: A structural way for obtain data from complicated [data structures](https://en.wikipedia.org/wiki/Data_structure).
  - Implementation: [Haskell](https://www.haskell.org/) [pattern matching](https://www.haskell.org/tutorial/patterns.html)

### Reader Macro
  - Description: Macro system that gets called by the reader, before AST is formed.
  - Implementation: [Common Lisp](https://lisp-lang.org/), [Elixir](https://elixir-lang.org/) [Sigils](https://elixir-lang.org/getting-started/sigils.html) (this is a weaker alternative to reader macro)
)

### Reference Capabilities
  - Description: A type system feature in which each reference carries a modifier to the capabilites, which can describe whether reading or writing is allowed, and whether these features are isolated to this reference.
  - Impelementation: [Pony](https://www.ponylang.io/)

### Reference Counting

### [Referential Transparency](https://en.wikipedia.org/wiki/Referential_transparency)
  - Description: A property for a function where: 1. returns the same for identical arguments; 2. has no other side effects.

### [Row Polymorphism](https://en.wikipedia.org/wiki/Row_polymorphism)
  - Description: A polymorphism that dispatch on concerned record fields and their type.
  - Implementation: [OCaml](https://ocaml.org/) [row polymorphism](https://www.cl.cam.ac.uk/teaching/1415/L28/rows.pdf)
  - Articles: 
    - [Jadon Fowler - Row Polymorphism without the Jargon](https://jadon.io/blog/row-polymorphism/)
    - [Stackoverflow - What are row types? Are they algebraic data types?](https://stackoverflow.com/questions/48092739/what-are-row-types-are-they-algebraic-data-types)
  - Paper: [Objective ML: An effective object-oriented extension to ML](https://caml.inria.fr/pub/papers/remy_vouillon-objective_ml-tapos98.pdf)

### Symmetric Interaction Calculus
  - Description: A calculus similar to [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus), that can be implement concurrently due to its support for projection and duplication.
  - Implementation: [HVM](https://github.com/HigherOrderCO/HVM)
  - Articles: 
    - [Victor Maia - The Symmetric Interaction Calculus](https://medium.com/@maiavictor/the-abstract-calculus-fe8c46bcf39c)
    - [XXIIVV - interaction nets](https://wiki.xxiivv.com/site/interaction_nets.html)
    - [Zicklag - Interaction Nets, Combinators, and Calculus](https://zicklag.github.io/blog/interaction-nets-combinators-calculus/)
  - Papers:
    - [Interaction nets](https://dl.acm.org/doi/10.1145/96709.96718)

### Term Rewriting
  - Implementation: [Mathematica](https://www.wolfram.com/mathematica/)

### Trait
  - Description: An interface that allows writing type safe code on different types.
  - Implementation: [Haskell](https://www.haskell.org/) [typeclass](https://www.haskell.org/tutorial/classes.html), [Rust](https://www.rust-lang.org/) [trait](https://doc.rust-lang.org/book/ch10-02-traits.html)

### Turing Machine

### [Universal Function Call Syntax](https://en.wikipedia.org/wiki/Uniform_Function_Call_Syntax)
  - Description: A syntax sugar for function call that makes chaining function calls easy.
  - Implementation: [D](https://dlang.org/) [UFCS](https://tour.dlang.org/tour/en/gems/uniform-function-call-syntax-ufcs), [Elixir](https://elixir-lang.org/) [pipe operator](https://elixir-lang.org/getting-started/enumerables-and-streams.html#the-pipe-operator), [Clojure](https://clojure.org/) [threading macro](https://clojure.org/guides/threading_macros)

### View Pattern
