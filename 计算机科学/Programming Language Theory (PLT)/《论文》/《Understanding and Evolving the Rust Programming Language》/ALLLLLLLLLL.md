

[toc]

### ----------------------------------

### Stacked Borrows 是什么？

**Stacked Borrows** 是一种为 Rust 编程语言设计的指针别名（aliasing）模型，旨在帮助编译器优化代码，同时确保安全性。它通过动态检查指针的使用方式，确保 Rust 的别名规则不会被违反。在 Rust 中，指针分为可变引用（`&mut`）和共享引用（`&`），而可变引用在同一时刻不能与其他指针别名。Stacked Borrows 通过为每个内存位置维护一个堆栈结构来动态跟踪指针使用，确保不同的指针在访问内存时不会引发未定义行为。

Rust 支持使用不安全代码（unsafe）直接操作原始指针，这为低层次的系统编程带来了很大灵活性，但也增加了潜在的风险。Stacked Borrows 的作用是定义动态语义规则，帮助确定某些指针操作是否会引发别名冲突。这种动态语义允许编译器在处理安全代码时进行更强的优化，例如重排内存访问，从而提升性能。

### RustBelt 是什么？

**RustBelt** 是 Rust 编程语言的第一个形式化证明系统，它为 Rust 的类型系统及其安全保证提供了一个逻辑模型。Rust 的一个重要特点是通过类型系统和所有权规则来保证内存安全，RustBelt 为这些保证提供了形式化的理论基础。它通过构建 λRust（Rust 的核心子语言）并利用分离逻辑（Iris）来证明 Rust 的安全性，包括所有权、借用和生命周期等特性。

RustBelt 的核心思想是通过语义模型来验证 Rust 程序的内存安全性，确保只要程序中的不安全代码严格遵循其定义的规则，整个程序依然是安全的。RustBelt 的出现使得 Rust 社区能够验证复杂库和标准库的正确性，并确保其不安全代码的实现没有违反 Rust 的安全模型。

### Iris 是什么？

**Iris** 是一个通用的、高阶并发分离逻辑框架，用于形式化证明并发程序的正确性。Iris 基于 Coq 证明助手开发，能够帮助研究者和开发者在高阶并发系统中构建证明。分离逻辑是验证程序中共享可变状态的强大工具，而 Iris 在此基础上扩展了可并发处理的能力。

Iris 提供了一些重要的特性，例如资源代数（Resource Algebras）和不变量（Invariants），使得它在复杂系统中的应用变得更加灵活。它被用作 RustBelt 的基础，为 Rust 的类型系统提供了坚实的理论支持。

### ----------------------------------------



这篇论文《理解与发展 Rust 编程语言》由 **Ralf Jung** 撰写，分为多个章节，详细探讨了 Rust 语言的内存安全、类型系统以及验证这些安全性保证的形式化模型。以下是对论文各章节的详细介绍：

### 1. 引言
引言概述了 Rust 解决的核心问题，尤其是系统编程中的内存安全问题。Rust 通过提供低级内存控制（类似 C 和 C++）与高级安全保证之间的平衡。论文中重点介绍了两个主要项目：
- **RustBelt**：Rust 类型系统健全性证明的形式化模型。
- **Stacked Borrows（堆栈借用）**：为 Rust 中的别名提供语义模型，用于编译器优化。

### 2. 为什么选择 Iris？
本章介绍了 **Iris**，这是一个构建分离逻辑（separation logic）的框架，是 RustBelt 的基础。Iris 提供了处理所有权和并发性所需的逻辑工具，重要的是，它支持构建 Rust 的安全属性模型。本章解释了 Iris 如何帮助处理复杂的所有权结构和不变性。

### 3. Iris 入门
本章进一步解释 Iris 的工作原理，重点讲解了关键概念，如虚状态（ghost state）、资源代数（resource algebras）和不变性（invariants），这些都是形式化所有权和权限模型的关键元素。

### 4. 虚状态构造
这一章深入探讨了如何在 Iris 中管理虚状态，重点介绍了资源代数的机制。它解释了这些构造如何使得我们能够对状态转换系统和共享所有权进行推理。

### 5. 不变性和模态
本章讨论了 Iris 支持对不变性进行推理的方式，并介绍了模态（modalities），这是一种可以描述程序如何在不同状态之间转换的逻辑结构。

### 6. 悖论
本章讨论了在实现高阶虚状态和自证不变性时出现的两种悖论，指出了分离逻辑设计中的一些有趣边界。

### 7. 与之前工作的关键差异
本章将 Iris 和 RustBelt 与之前的工作进行比较，尤其是针对系统编程语言的类型系统和并发性模型的形式化验证方法。

### 8. Rust 基础
这一章概述了 Rust 的类型系统，特别是 **所有权（ownership）**、**借用（borrowing）**、**可变和共享引用**以及**生命周期**等概念。它解释了这些特性如何确保内存安全并防止数据竞争。

### 9. λRust 语言和类型系统
本章介绍了 Rust 的核心特性（称为 λRust）的形式化模型，解释了该模型的语法和操作语义，并介绍了 RustBelt 类型系统的基础。

### 10. Iris 中 λRust 类型的语义模型
本章讨论了 Iris 中 λRust 类型的语义模型，并解释了该模型如何帮助建立 Rust 类型系统的健全性证明。

### 11. 生命周期逻辑
本章介绍了**生命周期逻辑**，这是分离逻辑的一个新扩展，用于处理 Rust 中的借用。生命周期逻辑是 RustBelt 类型系统模型的核心部分，允许对 Rust 的内存安全规则进行推理。

### 12. 类型系统健全性的语义证明
本章展示了 RustBelt 的核心健全性结果，证明了良好类型的 λRust 程序是安全的，它们不会出现内存错误或数据竞争。

### 13. 带内部可变性的类型建模
Rust 支持**内部可变性**模式，即共享引用可以用来改变数据。本章形式化了 Rust 类型系统中的这些模式，并使用前几章中开发的语义工具对它们进行建模。

### 14. 相关工作
本章将 RustBelt 及论文中展示的工作与编程语言理论和形式化验证领域的其他工作进行对比，讨论了类型系统和并发性研究中的相关进展。

### 15. 唯一性与不可变性
本章转向 **Stacked Borrows（堆栈借用）**，这是为 Rust 中的别名和借用提供形式化操作语义的模型。它解释了 Rust 的类型系统如何确保引用以正确的方式使用，从而防止例如迭代器失效等问题。

### 16. 保护器和内部可变性
本章介绍了**保护器（protectors）**的概念，解释了它如何帮助 Rust 实现依赖内部可变性的类型，讨论了这种机制如何与 Rust 的安全保证交互，并在 Stacked Borrows 模型中进行了建模。

### 17. 形式化操作语义
本章形式化了 Stacked Borrows 的操作语义，解释了 Stacked Borrows 如何在 Rust 的 Miri 解释器中实现，以及它如何与 Rust 的低级功能（例如原始指针）交互。

### 18. 评估
本章讨论了对 Stacked Borrows 模型的评估，介绍了该模型如何通过实际 Rust 程序进行验证，并揭示了 Rust 标准库中的一些错误。

### 19. 相关工作
本章将 Stacked Borrows 模型与其他编程语言中的别名和内存安全模型进行对比，并与其他低级语言的形式化模型进行比较。

### 20. 结论
论文总结了其贡献，并提出了未来工作方向。它强调了 RustBelt 和 Stacked Borrows 一起为 Rust 的类型系统和内存安全保证提供了第一个正式的、通过机器验证的模型。

---

本论文详细介绍了 Rust 的安全保证的形式化模型，通过 RustBelt 和 Stacked Borrows 两个项目，分别从理论和实际应用两个方面解决了 Rust 类型系统和内存模型中的重要问题。



### ----------------------------

This dissertation titled *"Understanding and Evolving the Rust Programming Language"* by Ralf Jung is divided into multiple sections that contribute to the understanding of Rust, its safety guarantees, and the formal models used to verify these guarantees. Below is an overview of the dissertation, chapter by chapter:

### 1. Introduction
The introduction provides a detailed overview of the problem space that Rust addresses, especially regarding memory safety in systems programming. It explains how Rust balances low-level memory control (like C and C++) with high-level safety guarantees. The two main projects discussed in the dissertation are:
- **RustBelt**: A formal model for proving the soundness of Rust's type system.
- **Stacked Borrows**: A model that provides semantics for aliasing in Rust, used for compiler optimizations.

### 2. Why Iris?
This chapter introduces **Iris**, a framework for building separation logics, which serves as the foundation for RustBelt. Iris provides the necessary logical tools to reason about ownership and concurrency, essential for modeling Rust’s safety properties. The chapter explains Iris’s advantages in handling complex ownership structures and invariants.

### 3. An Introduction to Iris
The chapter explains how Iris works, including key concepts such as ghost state, resource algebras, and invariants, which help formalize ownership and permissions in concurrent programs.

### 4. Ghost State Constructions
The chapter dives deeper into the mechanisms for managing ghost state in Iris, focusing on resource algebras. It also explains how these constructions enable reasoning about state transition systems and shared ownership.

### 5. Invariants and Modalities
Here, the focus shifts to Iris's support for reasoning about invariants, and it introduces modalities, which are logical constructs that can describe how programs transition between different states.

### 6. Paradoxes
This section discusses two paradoxes that emerge from naive implementations of higher-order ghost state and impredicative invariants, leading to interesting boundaries in the design of separation logics.

### 7. Key Differences to Prior Work
The chapter compares Iris and RustBelt with other prior approaches to modeling type systems and concurrency in systems programming languages.

### 8. Rust 101
This chapter provides a high-level overview of Rust’s type system, focusing on concepts like **ownership**, **borrowing**, **mutable and shared references**, and **lifetimes**. It explains how these constructs ensure memory safety and prevent data races.

### 9. The λRust Language and Type System
The formal model for Rust’s core features (referred to as λRust) is introduced. The chapter explains the syntax and operational semantics of this model and introduces the type system that forms the foundation of RustBelt.

### 10. A Semantic Model of λRust Types in Iris
This chapter discusses the semantic model of λRust types in Iris and explains how this model helps establish a soundness proof for Rust’s type system.

### 11. Lifetime Logic
This chapter introduces the **lifetime logic**—a new addition to separation logic for reasoning about borrowing in Rust. The lifetime logic forms the backbone of RustBelt’s model of Rust’s type system, enabling reasoning about the ownership rules that govern Rust’s memory safety guarantees.

### 12. Semantic Type System Soundness
This chapter presents the core soundness results of RustBelt, proving that well-typed λRust programs are safe to execute and that they do not exhibit memory or data race bugs.

### 13. Modeling Types with Interior Mutability
Rust supports patterns of **interior mutability**, where shared references can be used to mutate data. This chapter formalizes these patterns in Rust’s type system and models them using the semantic tools developed in the previous chapters.

### 14. Related Work
This chapter situates RustBelt and the work presented in the dissertation within the broader context of programming language theory and formal verification, drawing connections to other work on type systems and concurrency.

### 15. Uniqueness and Immutability
This chapter shifts to **Stacked Borrows**, a model that provides a formal operational semantics for aliasing and borrowing in Rust. It explains how Rust’s type system ensures that references are used in a well-formed, non-aliasing way, preventing issues like iterator invalidation.

### 16. Protectors and Interior Mutability
This chapter introduces the concept of **protectors**, which allow Rust to implement types that rely on interior mutability. It discusses how this mechanism interacts with Rust’s safety guarantees and models its behavior in Stacked Borrows.

### 17. Formal Operational Semantics
The operational semantics of Stacked Borrows is formalized in this chapter. It explains how Stacked Borrows is implemented in Rust’s Miri interpreter and how it interacts with low-level Rust features like raw pointers.

### 18. Evaluation
This chapter describes the evaluation of the Stacked Borrows model. It discusses how the model has been validated using real Rust programs and how it has uncovered bugs in the Rust standard library.

### 19. Related Work
This chapter compares the Stacked Borrows model to other models of aliasing and memory safety in programming languages and relates it to other formal models of low-level languages.

### 20. Conclusion
The dissertation concludes with a summary of the contributions and suggestions for future work. It emphasizes how RustBelt and Stacked Borrows together provide the first formal, machine-checked model of Rust’s type system and memory safety guarantees.

---

This dissertation is highly detailed, presenting formal models and proofs for verifying Rust’s safety guarantees. It addresses both theoretical concerns (through RustBelt) and practical concerns (through Stacked Borrows) in Rust’s type system and memory model.