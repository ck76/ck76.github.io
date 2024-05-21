[TOC]

- https://ncatlab.org/nlab/show/relation+between+type+theory+and+category+theory

### Type Theory and Category Theory: An Overview

#### Idea

类型论（Type Theory）和某些类型的范畴论（Category Theory）之间有着密切的关系。通过语法-语义对偶性（syntax-semantics duality），我们可以将类型论视为范畴论的形式语法语言或演算工具，反过来也可以将范畴论视为类型论的语义基础。具体使用的范畴论类型取决于相应的类型论类型。这种关系也延伸到同伦类型论（Homotopy Type Theory）和某些类型的（∞,1）-范畴论（(∞,1)-category theory）。

#### Overview

以下是类型论和范畴论之间的对比：

| 内部逻辑/类型论 (语法)                        | 语境范畴 (语义)                             | 参考资料                                 |
| --------------------------------------------- | ------------------------------------------- | ---------------------------------------- |
| 命题逻辑 (Propositional Logic)                | 林登鲍姆-塔斯基代数 (Lindenbaum-Tarski Algebra) |                                          |
| 直觉主义命题逻辑 (Intuitionistic Propositional Logic) | 海廷代数 (Heyting Algebra)                  |                                          |
| 古典命题逻辑 (Classical Propositional Logic)  | 布尔代数 (Boolean Algebra)                  |                                          |
| 对偶直觉主义命题逻辑 (Dual Intuitionistic Propositional Logic) | 对偶海廷代数 (Dual Heyting Algebra)          |                                          |
| 一阶逻辑 (First-Order Logic)                 | 超教义 (Hyperdoctrine)                      | Seely 1984a                              |
| 规则逻辑 (Regular Logic)                     | 规则超教义/规则范畴 (Regular Hyperdoctrine/Regular Category) |                                          |
| 一致逻辑 (Coherent Logic)                    | 一致超教义/一致范畴 (Coherent Hyperdoctrine/Coherent Category) |                                          |
| 直觉主义谓词逻辑 (Intuitionistic Predicate Logic) | 一阶超教义/海廷范畴 (First-Order Hyperdoctrine/Heyting Category) |                                          |
| 古典谓词逻辑 (Classical Predicate Logic)     | 布尔超教义/布尔范畴 (Boolean Hyperdoctrine/Boolean Category) |                                          |
| 模态逻辑 (Modal Logic)                       | 模态超教义 (Modal Hyperdoctrine)            |                                          |
| 线性逻辑 (Linear Logic)                      | 线性超教义 (Linear Hyperdoctrine)           |                                          |
| 简单类型λ演算 (Simply Typed Lambda Calculus)  | 笛卡尔闭范畴 (Cartesian Closed Category)    | Lambek & Scott 1986, Part I              |
| 扩展依赖类型论 (Extended Dependent Type Theory) | 局部笛卡尔闭范畴 (Locally Cartesian Closed Category) | Seely 1984b                              |
| 丛逻辑 (Bunched Logic)                       | 双闭单体范畴 (Bicartesian Closed Monoidal Category) | O’Hearn & Pym 1999                       |
| 无单值性的同伦类型论 (Non-Uniqueness Homotopy Type Theory) | 局部笛卡尔闭（∞,1）-范畴 (Locally Cartesian Closed (∞,1)-Category) | Cisinski 2012 & Shulman 2012             |
| 具有更高归纳类型和单值性的同伦类型论 (Homotopy Type Theory with Higher Inductive Types and Uniqueness) | 基本（∞,1）-拓扑斯 (Elementary (∞,1)-Topos) | 参见此处 (Refer to this)                |


#### Detailed Explanation and Theorems

1. **Type Theory as Syntax and Category Theory as Semantics**:
   - 类型论可以被看作是范畴论的语法，而范畴论提供了类型论的语义。
   - 例如，简单类型λ演算对应于笛卡尔闭范畴，扩展依赖类型论对应于局部笛卡尔闭范畴。

2. **Syntax-Semantics Duality**:
   - 在这种对偶性中，类型论的命题逻辑、谓词逻辑等在范畴论中有相应的语义解释。
   - 例如，直觉主义命题逻辑对应于海廷代数，古典命题逻辑对应于布尔代数。

3. **Dependent Type Theory and Locally Cartesian Closed Categories**:
   - 依赖类型论是局部笛卡尔闭范畴的语法。
   - 对于Martin-Löf依赖类型论，其语境范畴具有有限极限，并且是笛卡尔闭范畴。
   
4. **Homotopy Type Theory and Higher Category Theory**:
   - 同伦类型论（不带单值性）对应于局部笛卡尔闭（∞,1）-范畴。
   - 同伦类型论（带单值性和更高归纳类型）对应于基本（∞,1）-拓扑斯。

5. **Category of Contexts**:
   - 给定一个依赖类型论，其语境范畴的对象是类型，态射是函数类型的术语。
   - 例如，对于一个类型理论T，其语境范畴Con(T)是一个具有有限极限和笛卡尔闭结构的范畴。

#### Example and Applications

1. **Simply-Typed Lambda Calculus and Cartesian Closed Category**:
   - 简单类型λ演算对应于笛卡尔闭范畴。
   - λ演算中的类型可以看作范畴中的对象，而λ表达式可以看作范畴中的态射。

2. **Extensional Dependent Type Theory and Locally Cartesian Closed Category**:
   - 扩展依赖类型论的语义由局部笛卡尔闭范畴提供。
   - 例如，Martin-Löf依赖类型论的语境范畴具有所有的拉回和等化器。

3. **Homotopy Type Theory and (∞,1)-Category Theory**:
   - 同伦类型论的语法可以在（∞,1）-范畴中找到其语义。
   - 例如，带单值性公理的同伦类型论可以在（∞,1）-拓扑斯中找到其语义。

#### Conclusion

类型论和范畴论之间的关系展示了数学和逻辑中的深层次结构。类型论提供了形式的语法框架，而范畴论则提供了相应的语义解释。这种语法-语义对偶性在许多具体的逻辑和数学结构中都得到了体现，并且在现代数学基础研究中具有重要意义。