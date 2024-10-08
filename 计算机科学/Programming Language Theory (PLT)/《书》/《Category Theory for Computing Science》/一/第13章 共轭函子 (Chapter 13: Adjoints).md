[toc]





### ---------------------------

好的，让我们全面、详细地解析**第13章：Adjoints（伴随函子）**。伴随函子是范畴论中最重要的概念之一，仅次于可表示函子（representable functors）。本章不仅涵盖了伴随函子的基本理论，还通过具体例子展示了伴随函子在不同数学结构中的应用。理解伴随函子对于深入掌握范畴论及其在数学和计算机科学中的应用至关重要。

## 整体概览

### 章节概述

**第13章：Adjoints（伴随函子）**主要探讨伴随函子的概念及其基本性质，并通过自由单子（free monoids）的例子引出伴随函子的定义。随后，章节深入讨论伴随函子的进一步性质和应用，特别是在局部笛卡尔闭范畴（locally cartesian closed categories）中的应用。伴随函子的理论不仅在范畴论中占据核心地位，还在逻辑、代数、拓扑以及计算机科学中的类型理论和函数式编程语言中有广泛应用。

### 学习目标

通过本章的学习，您将能够：

1. 理解伴随函子的基本定义和构造方法。
2. 认识自由单子作为伴随函子实例的作用。
3. 掌握伴随函子的基本性质及其在范畴论中的重要性。
4. 理解局部笛卡尔闭范畴及其与伴随函子的关系。
5. 应用伴随函子理论解决实际问题，并理解其在其他数学领域中的应用。

## 13.1 自由单子（Free Monoids）

### 自由单子的概念

在讨论伴随函子之前，我们首先回顾自由单子的概念。自由单子是一个基本的代数结构，在各种数学和计算机科学领域中都有广泛应用。自由单子可以看作是在给定集合上生成的最自由的单子，意味着它满足单子的所有公理，但不附加任何额外的关系。

### 自由单子作为伴随函子的例子

自由单子的构造实际上是伴随函子的一个典型例子。具体来说，构造自由单子的过程可以看作是一个左伴随函子与一个忘记函子（forgetful functor）之间的伴随关系。

#### 定义

设 $\text{Set}$ 为集合范畴，$\text{Mon}$ 为单子范畴。存在一个忘记函子
$$
U: \text{Mon} \to \text{Set}
$$
它将一个单子 $M$ 映射到其底层集合 $U(M)$，并将单子态射映射到集合态射。

自由单子函子 $F: \text{Set} \to \text{Mon}$ 将一个集合 $X$ 映射到在 $X$ 上生成的自由单子 $F(X)$。具体地，$F(X)$ 是所有有限长度的 $X$ 上的字串（包括空串）的集合，乘法定义为串联操作，单位元是空串。

#### 伴随关系

自由单子函子 $F$ 是忘记函子 $U$ 的左伴随函子，即 $F \dashv U$。这意味着，对于任何集合 $X$ 和任何单子 $M$，存在一个自然同构：
$$
\text{Hom}_{\text{Mon}}(F(X), M) \cong \text{Hom}_{\text{Set}}(X, U(M))
$$
这个同构表明，给定一个从 $X$ 到 $U(M)$ 的集合态射，可以唯一地扩展为一个从 $F(X)$ 到 $M$ 的单子态射，反之亦然。

### 具体构造

1. **函子映射**：
   - **对象映射**：$F(X)$ 为 $X$ 上生成的自由单子。
   - **态射映射**：对于集合态射 $f: X \to Y$，$F(f): F(X) \to F(Y)$ 将每个字串 $x_1x_2\ldots x_n$ 映射为 $f(x_1)f(x_2)\ldots f(x_n)$。

2. **自然同构**：
   - 对于 $h: F(X) \to M$ 是一个单子态射，对应于 $h \circ \eta_X: X \to U(M)$，其中 $\eta_X: X \to F(X)$ 是单位自然变换（将 $X$ 中的元素映射为自由单子中的单元素字串）。
   - 反之，对于 $g: X \to U(M)$，对应的单子态射 $\hat{g}: F(X) \to M$ 将自由单子中的字串 $x_1x_2\ldots x_n$ 映射为 $g(x_1)g(x_2)\ldots g(x_n)$。

### 结论

自由单子的构造展示了伴随函子在代数结构构造中的应用。通过伴随函子，我们能够系统地理解自由结构的生成过程，并且这种理解在更复杂的范畴论构造中具有重要意义。

## 13.2 伴随函子（Adjoint Functors）

### 伴随函子的定义

在范畴论中，伴随函子是两个函子之间的一种重要关系，反映了它们在构造和忘记过程中的对偶性。具体来说，给定两个范畴 $\mathcal{C}$ 和 $\mathcal{D}$，若存在两个函子 $F: \mathcal{C} \to \mathcal{D}$ 和 $U: \mathcal{D} \to \mathcal{C}$，使得 $F$ 是 $U$ 的左伴随函子（left adjoint），即 $F \dashv U$，那么对于 $\mathcal{C}$ 中的每个对象 $C$ 和 $\mathcal{D}$ 中的每个对象 $D$，存在一个自然同构：
$$
\text{Hom}_{\mathcal{D}}(F(C), D) \cong \text{Hom}_{\mathcal{C}}(C, U(D))
$$
这个同构意味着，通过 $F$ 和 $U$ 之间的伴随关系，可以在两种范畴之间自如地转化态射。

### 单位和余单位

伴随函子之间的关系可以通过单位自然变换（unit）和余单位自然变换（counit）来描述。

1. **单位自然变换** $\eta: \text{Id}_{\mathcal{C}} \to U \circ F$：
   对于每个对象 $C \in \mathcal{C}$，有一个态射 $\eta_C: C \to U(F(C))$。

2. **余单位自然变换** $\epsilon: F \circ U \to \text{Id}_{\mathcal{D}}$：
   对于每个对象 $D \in \mathcal{D}$，有一个态射 $\epsilon_D: F(U(D)) \to D$。

这些自然变换满足伴随关系的三角恒等式（triangle identities），确保了 $F$ 和 $U$ 之间的互逆性。

### 基本性质

伴随函子具有许多重要的性质，使其在范畴论中具有核心地位。

1. **保守性**：
   左伴随函子 $F$ 保持所有的自由极限（colimits），而右伴随函子 $U$ 保持所有的极限（limits）。

2. **单子和余单子**：
   伴随函子之间形成单子和余单子的结构，左伴随函子 $F$ 与右伴随函子 $U$ 之间存在单子和余单子的对应关系。

3. **反演性**：
   若 $F \dashv U$，则 $U$ 不是 $F$ 的伴随函子，而是 $F$ 的右伴随函子。

### 伴随函子的构造方法

构造伴随函子的方法多种多样，常见的方法包括：

1. **自由构造**：
   类似于自由单子的例子，很多自由构造都可以看作是伴随函子的实例。

2. **限制和扩展**：
   在函数空间中，通过限制和扩展函子的定义可以构造伴随函子。

3. **共核与等核**：
   利用范畴中的共核和等核结构，可以构造伴随函子。

### 具体例子

除了自由单子的例子，伴随函子在许多其他数学结构中都有重要应用。例如：

1. **积与余积**：
   积（product）和余积（coproduct）在范畴中通常形成伴随函子对。积是极限的一种，而余积是极限的对偶概念。

2. **函数空间**：
   在笛卡尔闭范畴中，函数空间的构造形成伴随函子对。

3. **切片范畴**：
   切片范畴（slice categories）和纤维化范畴中的伴随函子也是常见的例子。

### 结论

伴随函子作为范畴论中的核心概念，通过自由构造、限制与扩展等方法在不同范畴之间建立了深刻的联系。理解伴随函子的定义、性质及其构造方法，对于深入研究范畴论及其在其他数学领域中的应用至关重要。

## 13.3 伴随函子的进一步性质（Further Topics on Adjoints）

### 3.1 伴随函子的反演性

伴随函子之间的反演性体现在左伴随函子和右伴随函子之间的互逆关系。具体来说，若 $F: \mathcal{C} \to \mathcal{D}$ 是 $U: \mathcal{D} \to \mathcal{C}$ 的左伴随函子，即 $F \dashv U$，那么 $U$ 不是 $F$ 的左伴随函子，而是 $F$ 的右伴随函子，即 $U \dashv F$。

### 3.2 伴随函子的组合

伴随函子的组合具有重要的结构性。例如，若 $F_1 \dashv U_1$ 和 $F_2 \dashv U_2$，则 $F_1 \circ F_2 \dashv U_2 \circ U_1$。这表明，通过伴随函子的组合，可以构造更复杂的伴随关系。

### 3.3 伴随函子的推广

伴随函子的概念可以推广到更广泛的范畴，例如双范畴（double categories）和高阶范畴（higher categories）中。通过这种推广，伴随函子在更高层次的范畴论结构中仍然保持其核心地位。

### 结论

伴随函子的进一步性质展示了其在范畴论中的高度灵活性和广泛适用性。通过理解伴随函子的反演性、组合性和推广性，能够更全面地掌握其在复杂范畴结构中的作用和应用。

## 13.4 局部笛卡尔闭范畴（Locally Cartesian Closed Categories）

### 定义与基本概念

局部笛卡尔闭范畴（LCCC）是指在每个切片范畴（slice category）中都是笛卡尔闭的范畴。具体来说，设 $\mathcal{C}$ 是一个范畴，若对于 $\mathcal{C}$ 中的每个对象 $C$，切片范畴 $\mathcal{C}/C$ 是笛卡尔闭的，那么 $\mathcal{C}$ 就是局部笛卡尔闭的。

### 伴随函子的应用

局部笛卡尔闭范畴的定义和性质依赖于伴随函子的理论。具体来说，切片范畴的笛卡尔闭结构通常通过某些伴随函子的存在和性质来构造和证明。例如，函数空间的存在可以通过右伴随函子的应用来实现。

### 示例

1. **集合范畴（Set）**：
   集合范畴是一个局部笛卡尔闭范畴，因为其切片范畴 $\text{Set}/C$ 对于每个集合 $C$ 都是笛卡尔闭的。

2. **范畴论中的基本范畴**：
   许多在范畴论中常见的范畴，如预序范畴（preorder categories）、拓扑范畴（Top）、代数范畴（Algebraic categories）等，都是局部笛卡尔闭范畴。

### 结构与性质

局部笛卡尔闭范畴具有许多重要的结构和性质，例如：

1. **指数对象**：
   在切片范畴中存在指数对象，使得 $\mathcal{C}/C$ 中的每个对象都可以构成一个函数空间。

2. **内部逻辑**：
   局部笛卡尔闭范畴支持一种内部逻辑，使得其在逻辑和类型理论中的应用尤为重要。

### 结论

局部笛卡尔闭范畴通过伴随函子的应用，展示了范畴论中高阶结构的构造方法。理解局部笛卡尔闭范畴及其与伴随函子的关系，对于深入研究范畴论在逻辑、代数和计算机科学中的应用具有重要意义。

## 结尾与总结

### 本章总结

**第13章：Adjoints（伴随函子）**深入探讨了范畴论中的伴随函子概念及其基本性质。通过自由单子的例子，介绍了伴随函子的构造方法，并展示了其在不同数学结构中的应用。进一步的性质讨论和在局部笛卡尔闭范畴中的应用，展示了伴随函子在复杂范畴结构中的核心地位。

### 重要性与应用

伴随函子是范畴论中最基本且最重要的工具之一。它们不仅在理论范畴论中占据核心位置，还在逻辑、代数、拓扑以及计算机科学中的类型理论和函数式编程语言中有广泛应用。例如，伴随函子的概念在定义和理解高阶类型构造、函数空间以及范畴的内在逻辑中起到了关键作用。

### 进一步学习

为了更深入地理解伴随函子及其应用，推荐参考以下文献：

- [Barr and Wells, 1985]：详细讨论了伴随函子的理论基础和应用。
- [Mac Lane, 1971]：作为范畴论的经典教材，提供了伴随函子的全面介绍。
- [Diers, 1980a; 1980b]：探讨了伴随函子的广义概念，包括FD sketches。
- [Hagino, 1987a; 1987b]：研究了基于伴随函子的函数式编程语言的类型构造。
- [Chen and Cockett, 1989]：进一步探讨了伴随函子在编程语言类型系统中的应用。

### 应用实例

通过理解伴随函子，您将能够：

1. **构造自由对象**：如自由单子、自由群等，系统地生成满足特定公理的最自由结构。
2. **分析范畴结构**：利用伴随函子分析和构造复杂的范畴结构，如局部笛卡尔闭范畴。
3. **应用于逻辑与类型理论**：在内部逻辑和类型理论中使用伴随函子构造类型和函数空间。
4. **理解高级代数结构**：通过伴随函子理解和构造高级代数结构，如单子和余单子。

### 结束语

伴随函子作为范畴论的核心概念，通过其深刻的结构性和广泛的应用性，成为理解和构造各种数学结构的重要工具。通过本章的学习，您应能掌握伴随函子的基本定义、构造方法及其在不同范畴中的应用，为后续章节的深入学习奠定坚实的理论基础。

继续学习后续章节，您将发现伴随函子在构建更复杂的范畴结构、理解范畴间的深层关系以及应用于其他数学和计算机科学领域中，扮演着不可或缺的角色。

### ----------------------------

好的，让我们全面、详细地解析**第13章：伴随函子（Adjoints）**中的**13.1节：自由单子（Free Monoids）**。本节深入探讨自由单子的定义、它们的普遍映射性质（universal mapping property），以及自由单子函子的构造和性质。通过对这些概念的详尽解释，您将能够更好地理解伴随函子的基本例子和其在范畴论中的重要作用。

## 整体概览

### 章节背景与重要性

在范畴论中，**伴随函子**（Adjoint Functors）是最核心且最重要的概念之一，几乎所有后续章节的理论都建立在这一基础之上。理解伴随函子的基本概念和性质，对于掌握范畴论的高级内容至关重要。**自由单子**作为伴随函子的一个典型例子，不仅帮助我们理解伴随函子的定义，还展示了其在构造自由代数结构中的应用。

本节内容涵盖以下几个方面：

1. **自由单子的定义与普遍映射性质**：详细说明自由单子的构造及其在范畴论中的普遍映射性质。
2. **自由单子函子的构造**：通过自由单子构造函子的定义，展示其作为一个范畴函子的属性。
3. **范畴论中的实例与应用**：通过具体例子，展示伴随函子在不同范畴中的应用。
4. **练习题解析**：通过练习题，巩固对自由单子和伴随函子性质的理解。

通过本节的学习，您将能够理解自由单子如何作为伴随函子的实例，以及伴随函子在构造自由代数结构中的关键作用。

## 13.1 自由单子（Free Monoids）

### 13.1.1 自由单子的普遍映射性质

#### 背景与前提

在3.1.14节中，我们简要介绍了自由单子的普遍性质。现在，我们将更为严谨地表述这一性质，并更详细地关注涉及的范畴（categories）。需要注意的是，当我们提到单子（monoid）的子集时，我们实际上混淆了两种不同的概念。单子不仅仅是一个集合，其子集不一定构成子单子（submonoids）。为了准确描述单子，我们必须提供三个数据：元素的集合、二元运算和单位元。仅仅提到“单子的子集”是不完整的，因为这忽略了运算和单位元的存在。

#### 定义范畴与基本概念

- **Mon**：单子范畴，表示所有单子及其同态（monoid homomorphisms）的范畴。
- **U: Mon → Set**：忘记函子（forgetful functor），将单子映射到其底层集合，并将单子同态映射到对应的集合函数。

**单子的详细定义**：
- **单子 M**：一个三元组 $(UM, \cdot, 1)$，其中：
  - $UM$ 是单子的底层集合。
  - $\cdot$ 是 $UM$ 上的二元运算。
  - $1 \in UM$ 是单位元。
- **单子同态 $f: M \to N$**：一个函数 $f: UM \to UN$，满足保持运算和单位元，即：
  $$
  f(x \cdot y) = f(x) \cdot f(y) \quad \text{且} \quad f(1_M) = 1_N
  $$

#### 自由单子的普遍性质

**定义13.1.2：自由单子的普遍映射性质**

给定一个集合 $X$，自由单子 $F(X) = (X^*, \cdot, h_i)$ 由以下普遍映射性质唯一确定：

**命题**：设 $X$ 是一个集合，$M$ 是一个单子，$u: X \to U(M)$ 是一个函数。那么存在且仅存在一个单子同态 $g: F(X) \to M$，使得以下图形交换：
$$
\begin{array}{ccc}
X & \xrightarrow{\eta_X} & X^* = UF(X) \\
 & \searrow_{u} & \downarrow_{Ug} \\
 & & U(M)
\end{array}
$$
即 $u = U(g) \circ \eta_X$。

**解释**：
- **$\eta_X$**：映射 $X$ 中的每个元素 $x$ 到 $X^*$ 中的单元素串 $h_i(x)$。
- **$F(X)$**：自由单子，底层集合是 $X^*$（即 $X$ 上的所有有限长度字串，包括空串），运算是字串的串联，单位元是空串。

**证明概要**：
1. **构造单子同态 $g$**：
   - 定义 $g(h_i(x)) = u(x)$ 对于所有 $x \in X$。
   - 对于 $X^*$ 中的任意字串 $h_{x_1} h_{x_2} \ldots h_{x_n}$，定义 $g(h_{x_1} h_{x_2} \ldots h_{x_n}) = u(x_1) \cdot u(x_2) \cdot \ldots \cdot u(x_n)$。
   - 确保 $g$ 保持运算和单位元，从而 $g$ 是一个合法的单子同态。

2. **唯一性**：
   - 由于 $g$ 的定义完全基于 $u$，并且自由单子的普遍性质确保任何满足 $u = U(g) \circ \eta_X$ 的同态 $g$ 必须按照上述方式定义，因此 $g$ 是唯一的。

**结论**：
自由单子的普遍映射性质确保了自由单子在范畴论中的唯一性，即自由单子由其生成集合和普遍映射性质唯一确定。

### 13.1.3 自由单子函子

**定义13.1.3：自由单子函子**

每个集合 $X$ 生成一个自由单子 $F(X)$。在3.1.12节中，我们将这一过程扩展为一个函子 $F: \text{Set} \to \text{Mon}$。本节将证明这一函子（作为13.1.4节中命题的部分证明）的性质，仅依赖于自由单子的普遍映射性质，因此该论证具有广泛的适用性。

**构造细节**：

1. **单位自然变换 $\eta$**：
   - 定义 $\eta_Y: Y \to Y^* = UF(Y)$，将每个元素 $y \in Y$ 映射到长度为1的字串 $h_i(y)$。
   
2. **态射映射**：
   - 对于函数 $f: X \to Y$，定义函子 $F(f): F(X) \to F(Y)$，其行为如下：
     - 对于字串 $h_i(x_1) h_i(x_2) \ldots h_i(x_n) \in F(X)$，有：
       $$
       F(f)(h_i(x_1) h_i(x_2) \ldots h_i(x_n)) = h_i(f(x_1)) h_i(f(x_2)) \ldots h_i(f(x_n))
       $$
   - 这确保了 $F(f)$ 是一个单子同态，因为它保持了字串的串联运算和单位元（空串）的映射。

3. **自然性**：
   - 对于任意函数 $g: Y \to Z$，有：
     $$
     F(g \circ f) = F(g) \circ F(f)
     $$
   - 对于恒等函数 $\text{id}_X: X \to X$，有：
     $$
     F(\text{id}_X) = \text{id}_{F(X)}
     $$
   

**证明命题13.1.4：F 是一个函子，且 $\eta$ 是自然变换**

**命题13.1.4**：函子 $F: \text{Set} \to \text{Mon}$ 是一个函子，并且对于任何集合 $X$，$\eta_X$ 是自然变换 $\eta: \text{Id}_{\text{Set}} \to U \circ F$ 的在 $X$ 上的组件。

**证明**：

1. **身份态射保持**：
   - 对于恒等函数 $\text{id}_X: X \to X$，需要验证 $F(\text{id}_X) = \text{id}_{F(X)}$。
   - 根据自由单子的普遍映射性质，$F(\text{id}_X)$ 是唯一满足 $\text{id}_X = U(F(\text{id}_X)) \circ \eta_X$ 的同态。
   - 但 $\text{id}_{F(X)}$ 映射每个字串 $s \in F(X)$ 到自身，满足 $U(\text{id}_{F(X)}) \circ \eta_X = \eta_X$。
   - 由于 $F(\text{id}_X)$ 也是满足这一条件的唯一同态，因此 $F(\text{id}_X) = \text{id}_{F(X)}$。

2. **态射复合保持**：
   - 对于任意函数 $f: X \to Y$ 和 $g: Y \to Z$，需要验证：
     $$
     F(g \circ f) = F(g) \circ F(f)
     $$
   - 根据普遍映射性质，$F(g \circ f)$ 是唯一满足 $U(F(g \circ f)) \circ \eta_X = (g \circ f)$ 的同态。
   - 而 $F(g) \circ F(f)$ 也是满足 $U(F(g) \circ F(f)) \circ \eta_X = U(F(g)) \circ U(F(f)) \circ \eta_X = g \circ f$ 的同态。
   - 由于自由单子的普遍映射性质保证唯一性，故 $F(g \circ f) = F(g) \circ F(f)$。

3. **自然变换 $\eta$ 的自然性**：
   - 需要验证对于任意函数 $f: X \to Y$，下图交换：
     $$
     \begin{array}{ccc}
     X & \xrightarrow{\eta_X} & F(X) \\
     \downarrow{f} & & \downarrow{F(f)} \\
     Y & \xrightarrow{\eta_Y} & F(Y)
     \end{array}
     $$
   - 即，$F(f) \circ \eta_X = \eta_Y \circ f$。
   - 对于任意 $x \in X$，有：
     $$
     (F(f) \circ \eta_X)(x) = F(f)(h_i(x)) = h_i(f(x))
     $$
     $$
     (\eta_Y \circ f)(x) = \eta_Y(f(x)) = h_i(f(x))
     $$
   - 因此，$F(f) \circ \eta_X = \eta_Y \circ f$，满足自然变换的条件。

**结论**：
函子 $F: \text{Set} \to \text{Mon}$ 是一个合法的范畴函子，并且 $\eta: \text{Id}_{\text{Set}} \to U \circ F$ 是一个自然变换。这表明自由单子构造不仅在对象上有意义，而且在态射上的结构也被严格保持。

**类似的例子**：
另一个类似的例子是**自由范畴**（free category），它将一个图（graph）生成一个自由范畴。其证明方法与自由单子函子的构造类似，都是基于普遍映射性质。

### 13.1.4 命题13.1.4的详细证明

**命题13.1.4**：函子 $F: \text{Set} \to \text{Mon}$ 是一个函子，并且对于任何集合 $X$，$\eta_X$ 是自然变换 $\eta: \text{Id}_{\text{Set}} \to U \circ F$ 的在 $X$ 上的组件。

**证明步骤**：

1. **验证身份态射保持**：
   - 对于恒等函数 $\text{id}_X: X \to X$，我们需要验证 $F(\text{id}_X) = \text{id}_{F(X)}$。
   - 根据普遍映射性质，$F(\text{id}_X)$ 是唯一满足 $\text{id}_X = U(F(\text{id}_X)) \circ \eta_X$ 的单子同态。
   - 但是 $\text{id}_{F(X)}$ 是一个单子同态，且 $U(\text{id}_{F(X)}) \circ \eta_X = \eta_X$，因此 $F(\text{id}_X) = \text{id}_{F(X)}$。

2. **验证态射复合保持**：
   - 对于任意函数 $f: X \to Y$ 和 $g: Y \to Z$，需要验证：
     $$
     F(g \circ f) = F(g) \circ F(f)
     $$
   - 通过自由单子的普遍映射性质，$F(g \circ f)$ 是唯一满足 $U(F(g \circ f)) \circ \eta_X = g \circ f$ 的单子同态。
   - 另一方面，$F(g) \circ F(f)$ 是一个单子同态，且：
     $$
     U(F(g) \circ F(f)) \circ \eta_X = U(F(g)) \circ U(F(f)) \circ \eta_X = g \circ f
     $$
   - 因此，根据普遍映射性质，$F(g \circ f) = F(g) \circ F(f)$。

3. **验证自然变换 $\eta$ 的自然性**：
   - 对于任意函数 $f: X \to Y$，需要验证下图交换：
     $$
     \begin{array}{ccc}
     X & \xrightarrow{\eta_X} & F(X) \\
     \downarrow{f} & & \downarrow{F(f)} \\
     Y & \xrightarrow{\eta_Y} & F(Y)
     \end{array}
     $$
   - 对于任意 $x \in X$，有：
     $$
     (F(f) \circ \eta_X)(x) = F(f)(h_i(x)) = h_i(f(x))
     $$
     $$
     (\eta_Y \circ f)(x) = \eta_Y(f(x)) = h_i(f(x))
     $$
   - 因此，$F(f) \circ \eta_X = \eta_Y \circ f$，满足自然变换的条件。

**结论**：
通过以上验证，函子 $F: \text{Set} \to \text{Mon}$ 是一个合法的函子，并且 $\eta$ 是一个自然变换。这表明自由单子构造不仅在对象上有效，而且在态射上也严格保持了范畴结构。

### 13.1.5 练习题解析

**练习1**：证明在命题13.1.2的证明中定义的函数 $g$ 是一个从自由单子 $F(X)$ 到单子 $M$ 的单子同态。

**解析**：

**目标**：证明定义的函数 $g: F(X) \to M$ 满足单子同态的条件，即保持运算和单位元。

**定义**：
- $g(h_i(x)) = u(x)$，对所有 $x \in X$。
- $g(h_i(x_1) \cdot h_i(x_2) \cdot \ldots \cdot h_i(x_n)) = u(x_1) \cdot u(x_2) \cdot \ldots \cdot u(x_n)$。

**验证**：

1. **保持单位元**：
   - $F(X)$ 的单位元是空串 $\epsilon$。
   - $g(\epsilon) = 1_M$，即 $M$ 的单位元。

2. **保持运算**：
   - 取任意两个字串 $s = h_i(x_1) h_i(x_2) \ldots h_i(x_n)$ 和 $t = h_i(y_1) h_i(y_2) \ldots h_i(y_m)$ 在 $F(X)$ 中。
   - 运算 $s \cdot t = h_i(x_1) h_i(x_2) \ldots h_i(x_n) h_i(y_1) h_i(y_2) \ldots h_i(y_m)$。
   - 计算 $g(s \cdot t)$：
     $$
     g(s \cdot t) = g(h_i(x_1) h_i(x_2) \ldots h_i(x_n) h_i(y_1) h_i(y_2) \ldots h_i(y_m)) = u(x_1) \cdot u(x_2) \cdot \ldots \cdot u(x_n) \cdot u(y_1) \cdot u(y_2) \cdot \ldots \cdot u(y_m)
     $$
   - 计算 $g(s) \cdot g(t)$：
     $$
     g(s) \cdot g(t) = (u(x_1) \cdot u(x_2) \cdot \ldots \cdot u(x_n)) \cdot (u(y_1) \cdot u(y_2) \cdot \ldots \cdot u(y_m))
     $$
   - 由于 $M$ 是一个单子，乘法具有结合律，因此：
     $$
     g(s) \cdot g(t) = u(x_1) \cdot u(x_2) \cdot \ldots \cdot u(x_n) \cdot u(y_1) \cdot u(y_2) \cdot \ldots \cdot u(y_m) = g(s \cdot t)
     $$
   - 因此，$g(s \cdot t) = g(s) \cdot g(t)$，保持了运算。

**结论**：
函数 $g$ 保持了自由单子的运算和单位元，因此 $g$ 是一个合法的单子同态。这完成了练习题的证明。

## 本节总结

**自由单子**作为伴随函子的一个基本例子，通过其普遍映射性质展示了伴随函子的核心概念。自由单子的构造不仅在代数结构中有重要应用，在范畴论中也起到了连接不同范畴的重要桥梁作用。

**主要要点**：

1. **自由单子的定义与普遍映射性质**：
   - 自由单子 $F(X)$ 由集合 $X$ 生成，满足其普遍映射性质，即任何从 $X$ 到单子 $M$ 底层集合的函数都唯一地扩展为单子同态 $g: F(X) \to M$。

2. **自由单子函子的构造**：
   - 自由单子构造扩展为一个函子 $F: \text{Set} \to \text{Mon}$，其对象映射和态射映射通过自由单子的普遍性质严格定义，确保了函子的合法性。

3. **伴随函子的实例**：
   - 自由单子函子 $F$ 是忘记函子 $U$ 的左伴随函子，即 $F \dashv U$，这种伴随关系展示了自由构造与忘记过程之间的深刻联系。

4. **练习题的应用**：
   - 通过练习题，巩固了对自由单子构造和伴随函子性质的理解，确保了对普遍映射性质的掌握。

**应用与扩展**：

自由单子的概念在范畴论中广泛应用于构造其他自由代数结构，如自由群、自由环等。伴随函子的理论不仅限于这些基础构造，还扩展到更复杂的范畴结构和逻辑体系中。例如，在逻辑和类型理论中，自由构造用于定义和理解基本类型及其操作。

理解自由单子及其伴随函子性质，为后续学习更高级的伴随函子理论和其在局部笛卡尔闭范畴等复杂结构中的应用奠定了坚实的基础。

通过本节的学习，您应能：

1. 理解自由单子的构造及其普遍映射性质。
2. 掌握自由单子函子的定义及其作为伴随函子的性质。
3. 通过练习题，验证自由单子构造中的关键步骤和单子同态的合法性。
4. 认识到伴随函子在范畴论中的核心地位及其在构造自由代数结构中的重要作用。

接下来的章节将进一步探讨伴随函子的更广泛性质和应用，帮助您全面掌握范畴论的精髓。

### ---------------------------

好的，让我们全面、详细地解析**第13章：伴随函子（Adjoints）**中的**13.2节：Adjoints**。本节深入探讨了伴随函子的定义、性质及其在不同范畴中的应用。通过具体例子和命题的解析，您将能够全面理解伴随函子的核心概念及其在范畴论中的重要地位。

## 整体概览

### 章节背景与重要性

**伴随函子**是范畴论中最核心且最重要的概念之一，几乎所有后续章节的理论都建立在这一基础之上。伴随函子的概念不仅在理论范畴论中占据关键位置，还在逻辑、代数、拓扑以及计算机科学中的类型理论和函数式编程语言中有广泛应用。理解伴随函子的基本概念和性质，对于掌握范畴论的高级内容至关重要。

本节内容涵盖以下几个方面：

1. **伴随函子的定义与普遍映射性质**：详细说明伴随函子的构造及其在范畴论中的普遍映射性质。
2. **伴随函子的基本性质**：探讨伴随函子的反演性、组合性等基本性质。
3. **伴随函子的具体例子**：通过多个具体例子展示伴随函子的广泛应用。
4. **习题解析**：通过习题巩固对伴随函子理论的理解。

通过本节的学习，您将能够理解伴随函子的基本定义、构造方法及其在不同范畴中的应用，为后续章节的深入学习奠定坚实的理论基础。

## 13.2 伴随函子（Adjoint Functors）

### 13.2.1 定义

**伴随函子**描述了两个范畴之间的一种特殊关系，这种关系反映了它们在构造和忘记过程中的对偶性。具体来说，给定两个范畴 $\mathcal{A}$ 和 $\mathcal{B}$，若存在两个函子 $F: \mathcal{A} \to \mathcal{B}$ 和 $U: \mathcal{B} \to \mathcal{A}$，使得 $F$ 是 $U$ 的左伴随函子（left adjoint），即 $F \dashv U$，则称 $U$ 是 $F$ 的右伴随函子（right adjoint）。

#### 定义

**定义13.2.1：伴随函子**

设 $\mathcal{A}$ 和 $\mathcal{B}$ 是范畴。如果 $F: \mathcal{A} \to \mathcal{B}$ 和 $U: \mathcal{B} \to \mathcal{A}$ 是两个函子，且存在一个自然变换 $\eta: \text{Id}_{\mathcal{A}} \to U \circ F$，满足以下条件：

对于任何对象 $A \in \mathcal{A}$ 和 $B \in \mathcal{B}$，以及任何态射 $f: A \to U(B)$，存在唯一的态射 $g: F(A) \to B$，使得下图交换：

$$
\begin{array}{ccc}
A & \xrightarrow{\eta_A} & U(F(A)) \\
\downarrow{f} & & \downarrow{U(g)} \\
U(B) & = & U(B)
\end{array}
$$

即 $f = U(g) \circ \eta_A$。

**解释**：

- **$\eta$**：称为**单位自然变换**（unit of the adjunction）。
- **普遍映射性质**：给定一个从 $A$ 到 $U(B)$ 的态射 $f$，可以唯一地“提升”为一个从 $F(A)$ 到 $B$ 的态射 $g$，使得 $f$ 可以通过 $g$ 和单位自然变换 $\eta_A$ 表达。

**备注**：

- 表达式 $UFUA$ 简写为 $(U \circ F \circ U)(A)$，即 $U(F(U(A)))$。
- 该定义表面上看起来不对称，但实际上通过下一节的命题，可以展示这种不对称性如何被对偶性所补偿。

### 13.2.2 命题

**命题13.2.2**：设 $F: \mathcal{A} \to \mathcal{B}$ 和 $U: \mathcal{B} \to \mathcal{A}$ 是函子，且 $F \dashv U$。则存在一个自然变换 $\epsilon: F \circ U \to \text{Id}_{\mathcal{B}}$，称为**余单位自然变换**（counit of the adjunction），满足以下条件：

对于任何态射 $g: F(A) \to B$，存在唯一的态射 $f: A \to U(B)$，使得：

$$
\epsilon_B \circ F(f) = g
$$

**解释**：

- **$\epsilon$**：余单位自然变换。
- **普遍映射性质的对偶**：伴随函子的定义和这个命题实际上是范畴论中的对偶关系，即左伴随和右伴随的相互关系。

**命题的对偶性**：

- 如果 $F \dashv U$，那么根据范畴论中的对偶性，$U \dashv F$ 并不成立。相反，$U$ 本身不具备成为伴随函子的性质。
- 因此，伴随函子之间的关系具有本质上的不对称性，但通过单位和余单位自然变换，这种不对称性被框定和利用。

### 13.2.3 伴随函子的例子

伴随函子的概念在许多数学结构中都有广泛的应用，特别是在自由结构的构造中。以下是几个典型的例子：

1. **自由单子与忘记函子**：

   - **自由单子函子** $F: \text{Set} \to \text{Mon}$：将一个集合 $X$ 映射到其生成的自由单子 $F(X)$。
   - **忘记函子** $U: \text{Mon} \to \text{Set}$：将一个单子 $M$ 映射到其底层集合 $U(M)$。
   - **关系**：$F \dashv U$。即自由单子函子是忘记函子的左伴随函子。

2. **积与投影函子**：

   - **对角函子** $\Delta: \mathcal{C} \to \mathcal{C} \times \mathcal{C}$：将一个对象 $A$ 映射到 $(A, A)$，将一个态射 $f: A \to B$ 映射到 $(f, f)$。
   - **投影函子** $\Pi: \mathcal{C} \times \mathcal{C} \to \mathcal{C}$：将 $(A, B)$ 映射到 $A$。
   - **关系**：$\Delta \dashv \Pi$，其中 $\Pi$ 是右伴随函子。

3. **函数空间与指数对象**：

   - **积函子** $- \times A: \mathcal{C} \to \mathcal{C}$（固定一个对象 $A$）。
   - **指数对象** $[A, -]: \mathcal{C} \to \mathcal{C}$。
   - **关系**：$- \times A \dashv [A, -]$，即积函子是指数对象的左伴随函子。

4. **切片范畴与投影函子**：

   - **切片函子** $U_A: \mathcal{C}/A \to \mathcal{C}$：将一个对象 $f: B \to A$ 映射到 $B$。
   - **切片的伴随函子** $P_A: \mathcal{C} \to \mathcal{C}/A$：将一个对象 $C$ 映射到 $(C \times A \to A)$。
   - **关系**：$P_A \dashv U_A$。

**解释**：

这些例子展示了伴随函子在不同范畴中的普遍性和重要性。无论是在构造自由结构、定义产品和指数对象，还是在切片范畴中的应用，伴随函子都发挥着关键作用。

### 13.2.4 具体例子

#### 例子1：积函子与投影函子

**设 $\mathcal{C}$ 是一个范畴**。考虑范畴 $\mathcal{C} \times \mathcal{C}$，其对象是 $\mathcal{C}$ 中的有序对 $(A, B)$，态射是有序对 $(f, g): (A, B) \to (A', B')$，其中 $f: A \to A'$ 和 $g: B \to B'$。

**对角函子** $\Delta: \mathcal{C} \to \mathcal{C} \times \mathcal{C}$ 定义为：
$$
\Delta(A) = (A, A), \quad \Delta(f) = (f, f)
$$

**投影函子** $\Pi: \mathcal{C} \times \mathcal{C} \to \mathcal{C}$ 定义为：
$$
\Pi(A, B) = A, \quad \Pi(f, g) = f
$$

**关系**：$\Delta \dashv \Pi$

**解释**：

- **单位自然变换** $\eta: \text{Id}_{\mathcal{C}} \to \Pi \circ \Delta$
  - 对于每个对象 $A \in \mathcal{C}$，定义 $\eta_A: A \to \Pi(\Delta(A)) = A$，即 $\eta_A = \text{id}_A$。

- **余单位自然变换** $\epsilon: \Delta \circ \Pi \to \text{Id}_{\mathcal{C} \times \mathcal{C}}$
  - 对于每个对象 $(A, B) \in \mathcal{C} \times \mathcal{C}$，定义 $\epsilon_{(A, B)}: \Delta(\Pi(A, B)) = (A, A) \to (A, B)$，即 $\epsilon_{(A, B)} = (\text{id}_A, \epsilon_B)$，其中 $\epsilon_B$ 是从 $A$ 到 $B$ 的态射，通常需要具体定义。

**普遍映射性质**：

对于任意对象 $(A, B) \in \mathcal{C} \times \mathcal{C}$ 和 $C \in \mathcal{C}$，以及态射 $f: \Delta(C) = (C, C) \to (A, B)$，存在唯一的态射 $g: C \to \Pi(A, B) = A$ 使得：
$$
f = \epsilon_{(A, B)} \circ \Delta(g)
$$
即 $f = (\text{id}_A, g)$。

这表明 $\Delta$ 和 $\Pi$ 构成了一个伴随函子对，其中 $\Delta$ 是左伴随函子，$\Pi$ 是右伴随函子。

**解释**：

这个例子展示了伴随函子在定义积（product）中的应用。积在范畴论中是一个重要的构造，伴随函子提供了一种系统化的方法来定义和理解积的存在及其性质。

#### 例子2：固定对象的积函子与指数对象

**设 $\mathcal{C}$ 是一个笛卡尔闭范畴（cartesian closed category）**，即对于每个对象 $A$，存在指数对象 $[A, -]$。

**积函子** $- \times A: \mathcal{C} \to \mathcal{C}$ 定义为：
$$
- \times A (B) = B \times A, \quad - \times A (f: B \to C) = f \times \text{id}_A: B \times A \to C \times A
$$

**指数对象函子** $[A, -]: \mathcal{C} \to \mathcal{C}$ 定义为：
$$
[A, -] (C) = [A, C], \quad [A, -] (f: C \to D) = [A, f]: [A, C] \to [A, D]
$$

**关系**：$- \times A \dashv [A, -]$

**解释**：

- **单位自然变换** $\eta: \text{Id}_{\mathcal{C}} \to [A, -] \circ (- \times A)$
  - 对于每个对象 $C \in \mathcal{C}$，定义 $\eta_C: C \to [A, C \times A]$，即 $\eta_C(c) = \lambda a. (c, a)$。

- **余单位自然变换** $\epsilon: [A, -] \circ (- \times A) \to \text{Id}_{\mathcal{C}}$
  - 对于每个对象 $C \in \mathcal{C}$，定义 $\epsilon_C: [A, C \times A] \to C$，即 $\epsilon_C(f) = \text{eval}_f$，其中 $\text{eval}_f: C \times A \to C$ 是评估态射。

**普遍映射性质**：

对于任意对象 $C \in \mathcal{C}$ 和 $D \in \mathcal{C}$，以及态射 $f: C \times A \to D$，存在唯一的态射 $g: C \to [A, D]$ 使得：
$$
f = \epsilon_D \circ ([A, f] \circ g)
$$
即 $f = \text{eval}_D \circ (g \times \text{id}_A)$。

**解释**：

这个例子展示了伴随函子在定义指数对象（函数空间）中的应用。指数对象在逻辑和类型理论中具有重要意义，伴随函子提供了一种系统化的方法来定义和理解指数对象的存在及其性质。

#### 例子3：切片范畴的伴随函子

**设 $\mathcal{C}$ 是一个范畴**。考虑切片范畴 $\mathcal{C}/A$，其对象是 $\mathcal{C}$ 中的态射 $f: B \to A$，态射是满足 $f' \circ g = f$ 的态射 $g: B \to B'$。

**切片函子** $U_A: \mathcal{C}/A \to \mathcal{C}$ 定义为：
$$
U_A(f: B \to A) = B, \quad U_A(g: (B \to A) \to (B' \to A)) = g
$$

**伴随函子** $P_A: \mathcal{C} \to \mathcal{C}/A$ 定义为：
$$
P_A(C) = (C \times A \to A), \quad P_A(f: C \to D) = (f \times \text{id}_A)
$$

**关系**：$P_A \dashv U_A$

**解释**：

- **单位自然变换** $\eta: \text{Id}_{\mathcal{C}} \to U_A \circ P_A$
  - 对于每个对象 $C \in \mathcal{C}$，定义 $\eta_C: C \to U_A(P_A(C)) = C \times A$，即 $\eta_C(c) = (c, a_0)$，其中 $a_0$ 是 $A$ 的某个固定元素。

- **余单位自然变换** $\epsilon: P_A \circ U_A \to \text{Id}_{\mathcal{C}/A}$
  - 对于每个对象 $(B \to A) \in \mathcal{C}/A$，定义 $\epsilon_{(B \to A)}: P_A(U_A(B \to A)) = (B \times A \to A) \to (B \to A)$，即投影态射 $p_2: B \times A \to A$。

**普遍映射性质**：

对于任意对象 $C \in \mathcal{C}$ 和 $(B \to A) \in \mathcal{C}/A$，以及态射 $f: C \to B$，满足 $(B \to A) \circ f = g: C \to A$，存在唯一的态射 $h: C \to P_A(B \to A) = (B \times A \to A)$ 使得：
$$
f = U_A(h) \circ \eta_C
$$
即 $f = h \circ \eta_C$。

**解释**：

这个例子展示了伴随函子在切片范畴中的应用。切片范畴在几何和逻辑中有重要应用，伴随函子提供了一种系统化的方法来定义和理解切片范畴的结构及其与原范畴之间的关系。

### 13.2.5 习题解析

以下是本节的习题解析，帮助您巩固对伴随函子理论的理解。

**练习1**：证明如果 $f: S \to T$ 是集合之间的一个函数，则对于 $S_0 \subseteq S$ 和 $T_0 \subseteq T$，有 $S_0 \subseteq f^{-1}(T_0)$ 当且仅当 $f_*(S_0) \subseteq T_0$。

**解析**：

**目标**：证明集合 $S$ 的子集 $S_0$ 包含于 $f^{-1}(T_0)$ 当且仅当 $f_*(S_0)$ 包含于 $T_0$。

**定义**：

- **逆像**：$f^{-1}(T_0) = \{ s \in S \mid f(s) \in T_0 \}$
- **直接像**：$f_*(S_0) = \{ f(s) \mid s \in S_0 \}$

**证明**：

**(⇒) 如果 $S_0 \subseteq f^{-1}(T_0)$，则 $f_*(S_0) \subseteq T_0$**：

- 任取 $t \in f_*(S_0)$，则存在 $s \in S_0$ 使得 $t = f(s)$。
- 由于 $S_0 \subseteq f^{-1}(T_0)$，有 $s \in f^{-1}(T_0)$，即 $f(s) \in T_0$。
- 因此，$t = f(s) \in T_0$，故 $f_*(S_0) \subseteq T_0$。

**(⇐) 如果 $f_*(S_0) \subseteq T_0$，则 $S_0 \subseteq f^{-1}(T_0)$**：

- 任取 $s \in S_0$，则 $f(s) \in f_*(S_0) \subseteq T_0$。
- 因此，$s \in f^{-1}(T_0)$，故 $S_0 \subseteq f^{-1}(T_0)$。

**结论**：

因此，$S_0 \subseteq f^{-1}(T_0)$ 当且仅当 $f_*(S_0) \subseteq T_0$。

---

**练习2**：证明如果 $\Delta: \mathcal{C} \to \mathcal{C} \times \mathcal{C}$ 的右伴随函子存在，则其左伴随函子将对象 $(A, B)$ 映射为 $A + B$。

**解析**：

**目标**：证明 $\Pi \dashv \Delta$ 时，$\Pi(A, B) = A + B$，即积函子的左伴随函子将 $(A, B)$ 映射为 $A + B$。

**假设**：

- 存在右伴随函子 $\Pi: \mathcal{C} \times \mathcal{C} \to \mathcal{C}$，使得 $\Pi \dashv \Delta$。

**要证明**：

- 对于 $(A, B) \in \mathcal{C} \times \mathcal{C}$，有 $\Pi(A, B) = A + B$。

**证明**：

- 根据伴随函子的定义，对于任意 $C \in \mathcal{C}$，有自然同构：
  $$
  \text{Hom}_{\mathcal{C}}(\Pi(A, B), C) \cong \text{Hom}_{\mathcal{C} \times \mathcal{C}}((A, B), \Delta(C)) = \text{Hom}_{\mathcal{C}}(A, C) \times \text{Hom}_{\mathcal{C}}(B, C)
  $$
  
- 而 $A + B$ 是 $A$ 和 $B$ 的积（或余积，具体取决于范畴的定义），满足：
  $$
  \text{Hom}_{\mathcal{C}}(A + B, C) \cong \text{Hom}_{\mathcal{C}}(A, C) \times \text{Hom}_{\mathcal{C}}(B, C)
  $$
  
- 因此，$\Pi(A, B)$ 必须满足与 $A + B$ 相同的普遍映射性质，即 $\Pi(A, B) = A + B$。

**结论**：

因此，若 $\Pi \dashv \Delta$，则 $\Pi(A, B) = A + B$。

---

**练习3**：找出13.2.3节中描述的伴随函子 $\eta$。

**解析**：

**目标**：描述伴随函子 $\eta$，即自然变换 $\eta: \text{Id}_{\mathcal{C}} \to U \circ F$。

**背景**：

- 在伴随函子 $F \dashv U$ 中，$\eta$ 是单位自然变换。
- $\eta_A: A \to U(F(A))$ 对于每个对象 $A \in \mathcal{C}$。

**描述**：

- 对于每个对象 $A \in \mathcal{C}$，$\eta_A$ 是从 $A$ 到 $U(F(A))$ 的态射，满足普遍映射性质。
- $\eta_A$ 将对象 $A$ 映射为 $F(A)$ 的结构（如自由单子中的生成映射）。

**具体例子**：

- 在自由单子与忘记函子的例子中，$\eta_X: X \to U(F(X)) = X^*$ 将每个元素 $x \in X$ 映射为自由单子中的单元素字串 $h_i(x)$。

**结论**：

伴随函子的单位自然变换 $\eta$ 定义了从原范畴到伴随函子组合的自然映射，确保了伴随关系的普遍映射性质。

---

**练习4**：设 $R$ 和 $Z$ 分别表示实数和整数的有序集（作为范畴）。证明包含 $Z \subseteq R$ 有左伴随和右伴随函子，并识别它们。

**解析**：

**目标**：证明包含函子 $i: Z \hookrightarrow R$ 存在左伴随和右伴随函子，并明确它们的具体形式。

**设定**：

- $R$ 和 $Z$ 被视为预序范畴（preordered sets），即范畴中最多只有一个态射。
- $i: Z \hookrightarrow R$ 是包含函子。

**证明**：

1. **存在左伴随函子**：

   - **左伴随函子** $F: R \to Z$ 定义为对每个 $r \in R$，$F(r) = \lfloor r \rfloor$，即 $r$ 的下取整。
   - **单位自然变换** $\eta: \text{Id}_Z \to i \circ F$，即 $\eta_z: z \to i(F(z)) = z$，这里 $\eta_z$ 是恒等态射。
   - **普遍映射性质**：
     对于任何 $z \in Z$ 和 $r \in R$，有 $F(r) \leq z$ 当且仅当 $r \leq i(z) = z$，即 $\lfloor r \rfloor \leq z$ 当且仅当 $r \leq z$。

2. **存在右伴随函子**：

   - **右伴随函子** $G: R \to Z$ 定义为对每个 $r \in R$，$G(r) = \lceil r \rceil$，即 $r$ 的上取整。
   - **余单位自然变换** $\epsilon: i \circ G \to \text{Id}_R$，即 $\epsilon_r: i(G(r)) = \lceil r \rceil \to r$，这里 $\epsilon_r$ 是恒等态射（在预序范畴中）。
   - **普遍映射性质**：
     对于任何 $r \in R$ 和 $z \in Z$，有 $r \leq G(z)$ 当且仅当 $i(F(r)) = \lfloor r \rfloor \leq z$，即 $r \leq \lceil r \rceil \leq z$。

**结论**：

因此，包含函子 $i: Z \hookrightarrow R$ 存在左伴随函子 $F: R \to Z$（下取整）和右伴随函子 $G: R \to Z$（上取整）。

### 13.2.6 逆像函子的伴随函子

#### 例子4：逆像函子及其伴随函子

**设 $S$ 和 $T$ 是集合，且 $f: S \to T$ 是一个函数**。考虑**子集范畴** $\text{Sub}(T)$ 和 $\text{Sub}(S)$，其中对象是集合的子集，态射是子集之间的包含关系。

**逆像函子** $f^{-1}: \text{Sub}(T) \to \text{Sub}(S)$ 定义为：
$$
f^{-1}(T_0) = \{ s \in S \mid f(s) \in T_0 \}
$$
对于包含关系 $T_0 \subseteq T_1$，有 $f^{-1}(T_0) \subseteq f^{-1}(T_1)$，因此 $f^{-1}$ 是一个函子。

**伴随函子**：

- **左伴随函子** $f_*: \text{Sub}(S) \to \text{Sub}(T)$ 定义为：
  $$
  f_*(S_0) = \{ f(s) \mid s \in S_0 \}
  $$
  **普遍映射性质**：
  $$
  S_0 \subseteq f^{-1}(T_0) \quad \text{当且仅当} \quad f_*(S_0) \subseteq T_0
  $$
  
- **右伴随函子** $f_!: \text{Sub}(S) \to \text{Sub}(T)$ 定义为：
  $$
  t \in f_!(S_0) \quad \text{当且仅当} \quad f^{-1}(\{t\}) \subseteq S_0
  $$
  **解释**：$t$ 属于 $f_!(S_0)$ 当且仅当 $t$ 的逆像中的所有元素都属于 $S_0$。

**关系**：

- **$f_* \dashv f^{-1} \dashv f_!$**

**解释**：

- 逆像函子 $f^{-1}$ 同时拥有左伴随函子 $f_*$ 和右伴随函子 $f_!$。
- $f_*$ 将子集 $S_0$ 映射到其直接像 $f_*(S_0)$，满足普遍映射性质：$S_0 \subseteq f^{-1}(T_0)$ 当且仅当 $f_*(S_0) \subseteq T_0$。
- $f_!$ 将子集 $S_0$ 映射到那些元素 $t \in T$，其逆像完全包含于 $S_0$，即 $f^{-1}(\{t\}) \subseteq S_0$。

**解释**：

这个例子展示了逆像函子如何同时具有左伴随和右伴随函子，体现了伴随函子在处理集合子集和函数映射时的灵活性和普遍性。

### 13.2.7 伴随函子在偏序集中的应用

**设 $P$ 和 $Q$ 是偏序集（preordered sets），即范畴中最多有一条态射**。若 $f: P \to Q$ 和 $u: Q \to P$ 是保持偏序关系的函子（即单调函子），则 $f \dashv u$ 的条件可以通过以下等价条件描述：

**条件**：

$$
f(x) \leq y \quad \text{当且仅当} \quad x \leq u(y)
$$

**解释**：

- 这意味着，对于所有 $x \in P$ 和 $y \in Q$，有 $f(x) \leq y$ 当且仅当 $x \leq u(y)$。
- 这是一种**Galois连接**（Galois connection），命名来源于Galois理论中的对应关系。

**具体例子**：

- **Galois连接**：在Galois理论中，若 $E$ 是 $F$ 的Galois扩张，则存在一个从子域到子群的双射，满足类似的对偶关系。这个对应关系是一个伴随函子对的具体体现。

**双函子对**：

- 若 $f: P^{\text{op}} \to Q$ 和 $u: Q \to P^{\text{op}}$ 是函子对（即其中一个是逆变的），则伴随关系仍然成立，反映了范畴论中的对偶性。

**总结**：

在偏序集范畴中，伴随函子对简化为满足特定条件的单调函子对，反映了元素之间的对偶关系。这种对应关系在许多数学领域中都有重要应用，如Galois理论中的子群与子域的对应关系。

### 13.2.8 习题解析

**练习1**：证明如果 $f: S \to T$ 是集合之间的一个函数，则对于 $S_0 \subseteq S$ 和 $T_0 \subseteq T$，有 $S_0 \subseteq f^{-1}(T_0)$ 当且仅当 $f_*(S_0) \subseteq T_0$。

**解析**：

**目标**：证明集合 $S$ 的子集 $S_0$ 包含于逆像 $f^{-1}(T_0)$ 当且仅当其直接像 $f_*(S_0)$ 包含于 $T_0$。

**定义**：

- **逆像**：$f^{-1}(T_0) = \{ s \in S \mid f(s) \in T_0 \}$
- **直接像**：$f_*(S_0) = \{ f(s) \mid s \in S_0 \}$

**证明**：

**(⇒) 如果 $S_0 \subseteq f^{-1}(T_0)$，则 $f_*(S_0) \subseteq T_0$**：

- 任取 $t \in f_*(S_0)$，则存在 $s \in S_0$ 使得 $t = f(s)$。
- 由于 $s \in S_0 \subseteq f^{-1}(T_0)$，有 $f(s) \in T_0$。
- 因此，$t \in T_0$，即 $f_*(S_0) \subseteq T_0$。

**(⇐) 如果 $f_*(S_0) \subseteq T_0$，则 $S_0 \subseteq f^{-1}(T_0)$**：

- 任取 $s \in S_0$，则 $f(s) \in f_*(S_0) \subseteq T_0$。
- 因此，$s \in f^{-1}(T_0)$，即 $S_0 \subseteq f^{-1}(T_0)$。

**结论**：

因此，$S_0 \subseteq f^{-1}(T_0)$ 当且仅当 $f_*(S_0) \subseteq T_0$。

---

**练习2**：证明如果 $\Delta: \mathcal{C} \to \mathcal{C} \times \mathcal{C}$ 的右伴随函子存在，则其左伴随函子将对象 $(A, B)$ 映射为 $A + B$。

**解析**：

**目标**：证明 $\Delta \dashv \Pi$ 时，$\Pi(A, B) = A + B$，即积函子的左伴随函子将 $(A, B)$ 映射为 $A + B$。

**假设**：

- 存在右伴随函子 $\Pi: \mathcal{C} \times \mathcal{C} \to \mathcal{C}$，使得 $\Pi \dashv \Delta$。

**要证明**：

- 对于 $(A, B) \in \mathcal{C} \times \mathcal{C}$，有 $\Pi(A, B) = A + B$。

**证明**：

- 根据伴随函子的定义，对于任意 $C \in \mathcal{C}$，有自然同构：
  $$
  \text{Hom}_{\mathcal{C}}(\Pi(A, B), C) \cong \text{Hom}_{\mathcal{C} \times \mathcal{C}}((A, B), \Delta(C)) = \text{Hom}_{\mathcal{C}}(A, C) \times \text{Hom}_{\mathcal{C}}(B, C)
  $$
  
- 而 $A + B$ 是 $A$ 和 $B$ 的积（或余积，具体取决于范畴的定义），满足：
  $$
  \text{Hom}_{\mathcal{C}}(A + B, C) \cong \text{Hom}_{\mathcal{C}}(A, C) \times \text{Hom}_{\mathcal{C}}(B, C)
  $$
  
- 因此，$\Pi(A, B)$ 必须满足与 $A + B$ 相同的普遍映射性质，即 $\Pi(A, B) = A + B$。

**结论**：

因此，若 $\Pi \dashv \Delta$，则 $\Pi(A, B) = A + B$。

---

**练习3**：找出伴随函子 $\eta$ 的具体形式。

**解析**：

**目标**：描述伴随函子 $\eta: \text{Id}_{\mathcal{C}} \to U \circ F$ 的具体形式。

**背景**：

- 在伴随函子 $F \dashv U$ 中，$\eta$ 是单位自然变换。
- $\eta_A: A \to U(F(A))$ 对于每个对象 $A \in \mathcal{C}$。

**描述**：

- 对于每个对象 $A \in \mathcal{C}$，$\eta_A$ 是从 $A$ 到 $U(F(A))$ 的态射，满足普遍映射性质。
- 在自由单子与忘记函子的例子中，$\eta_X: X \to U(F(X)) = X^*$ 将每个元素 $x \in X$ 映射为自由单子中的单元素字串 $h_i(x)$。

**具体形式**：

- **自由单子**：$\eta_X(x) = h_i(x)$，即将 $x$ 映射为自由单子中的单元素字串。
- **积函子与指数对象**：$\eta_C(c) = \lambda a. (c, a)$，即将 $c$ 映射为函数 $\lambda a. (c, a)$。

**结论**：

伴随函子的单位自然变换 $\eta$ 定义了从原范畴到伴随函子组合的自然映射，确保了伴随关系的普遍映射性质。通过具体例子的定义，伴随函子在不同范畴中的具体形式得以明确。

---

**练习4**：设 $R$ 和 $Z$ 分别表示实数和整数的有序集（作为范畴）。证明包含 $Z \subseteq R$ 有左伴随和右伴随函子，并识别它们。

**解析**：

**目标**：证明包含函子 $i: Z \hookrightarrow R$ 存在左伴随函子和右伴随函子，并明确它们的具体形式。

**设定**：

- $R$ 和 $Z$ 被视为预序范畴（preordered sets），即范畴中最多只有一个态射。
- $i: Z \hookrightarrow R$ 是包含函子。

**证明**：

1. **存在左伴随函子**：

   - **左伴随函子** $F: R \to Z$ 定义为对每个 $r \in R$，$F(r) = \lfloor r \rfloor$，即 $r$ 的下取整。
   - **单位自然变换** $\eta: \text{Id}_Z \to i \circ F$，即 $\eta_z: z \to i(F(z)) = z$，这里 $\eta_z$ 是恒等态射。
   - **普遍映射性质**：
     对于任何 $z \in Z$ 和 $r \in R$，有 $F(r) \leq z$ 当且仅当 $r \leq i(z) = z$，即 $\lfloor r \rfloor \leq z$ 当且仅当 $r \leq z$。

2. **存在右伴随函子**：

   - **右伴随函子** $G: R \to Z$ 定义为对每个 $r \in R$，$G(r) = \lceil r \rceil$，即 $r$ 的上取整。
   - **余单位自然变换** $\epsilon: i \circ G \to \text{Id}_R$，即 $\epsilon_r: i(G(r)) = \lceil r \rceil \to r$，这里 $\epsilon_r$ 是恒等态射（在预序范畴中）。
   - **普遍映射性质**：
     对于任何 $r \in R$ 和 $z \in Z$，有 $r \leq G(z)$ 当且仅当 $i(F(r)) = \lfloor r \rfloor \leq z$，即 $r \leq \lceil r \rceil \leq z$。

**结论**：

因此，包含函子 $i: Z \hookrightarrow R$ 存在左伴随函子 $F: R \to Z$（下取整）和右伴随函子 $G: R \to Z$（上取整）。

## 本节总结

**伴随函子**作为范畴论中的核心概念，通过其深刻的结构性和广泛的应用性，成为理解和构造各种数学结构的重要工具。通过本节的学习，您将能够：

1. **理解伴随函子的基本定义**：掌握伴随函子的定义及其普遍映射性质，了解左伴随函子和右伴随函子之间的关系。
2. **识别伴随函子的具体例子**：通过自由单子、积函子与投影函子、函数空间与指数对象等具体例子，理解伴随函子的应用。
3. **掌握伴随函子的基本性质**：了解伴随函子的反演性、组合性等基本性质，认识到伴随函子在范畴论中的重要地位。
4. **应用伴随函子解决实际问题**：通过习题和具体例子，掌握如何利用伴随函子理论解决实际问题，理解其在不同范畴中的应用。

### 应用与扩展

伴随函子的理论不仅在理论范畴论中有重要地位，还在逻辑、代数、拓扑以及计算机科学中的类型理论和函数式编程语言中有广泛应用。例如：

- **逻辑与类型理论**：伴随函子用于定义和理解高阶类型构造、函数空间等概念。
- **自由代数结构**：通过伴随函子构造自由单子、自由群等代数结构，系统化地生成满足特定公理的最自由结构。
- **范畴间的深层关系**：伴随函子提供了一种系统化的方法来分析和构造范畴间的深层关系，如积、指数对象、切片范畴等。

通过本节的学习，您应能全面理解伴随函子的基本概念、定义及其在不同范畴中的具体应用，为后续章节的深入学习打下坚实的基础。

---

以上内容详尽地解释了**第13章：伴随函子**中的**13.2节：Adjoints**，涵盖了定义、命题、具体例子以及习题解析。通过这些内容，您将能够深入理解伴随函子的理论基础及其在范畴论中的广泛应用。

### ----------------------------

好的，让我们全面、详细地解析**第13章：伴随函子（Adjoints）**中的**13.3节：Further topics on adjoints（伴随函子的进一步主题）**。本节深入探讨了伴随函子的高级性质和相关理论，包括同态集的伴随性、伴随函子的唯一性、逐点伴随性定理（Pointwise Adjointness Theorem）以及伴随函子在保持极限和余极限中的作用。通过具体的命题、定理和例子，我们将进一步理解伴随函子的深刻结构及其在范畴论中的广泛应用。

## 整体概览

### 章节背景与重要性

在**第13章**中，我们已经介绍了伴随函子的基本定义、性质以及一些具体例子，如自由单子与忘记函子的伴随关系。**13.3节**进一步扩展了伴随函子的理论，探讨了同态集的伴随性、伴随函子的唯一性以及逐点伴随性定理等高级主题。这些内容不仅深化了我们对伴随函子的理解，还为后续章节中的复杂理论打下了坚实的基础。

### 学习目标

通过本节的学习，您将能够：

1. 理解同态集的伴随性及其与伴随函子之间的关系。
2. 掌握伴随函子的唯一性定理，了解伴随函子在不同范畴中的唯一性。
3. 理解逐点伴随性定理及其在构造伴随函子中的应用。
4. 了解伴随函子如何保持范畴中的极限和余极限。
5. 通过习题巩固对伴随函子高级性质的理解。

## 13.3 伴随函子的进一步主题

### 13.3.1 同态集的伴随性（Hom Set Adjointness）

在范畴论的文献中，伴随函子经常通过同态集的自然同构来定义。这种替代性的表述方式更加抽象和普遍，是理解伴随函子的关键途径之一。

**定义**：

设 $\mathcal{A}$ 和 $\mathcal{B}$ 是范畴，$F: \mathcal{A} \to \mathcal{B}$ 和 $U: \mathcal{B} \to \mathcal{A}$ 是两个函子。若存在自然同构：
$$
\text{Hom}_{\mathcal{B}}(F(-), -) \cong \text{Hom}_{\mathcal{A}}(-, U(-))
$$
作为函子 $\mathcal{A}^{\text{op}} \times \mathcal{B} \to \text{Set}$ 的自然同构，则称 $F \dashv U$，即 $F$ 是 $U$ 的左伴随函子，$U$ 是 $F$ 的右伴随函子。

这种表述方式强调了在两个范畴之间，$F$ 和 $U$ 通过同态集之间的自然同构建立了伴随关系。

### 13.3.2 定理：伴随函子与同态集的自然同构

**定理13.3.2**：

设 $\mathcal{A}$ 和 $\mathcal{B}$ 是范畴，$F: \mathcal{A} \to \mathcal{B}$ 和 $U: \mathcal{B} \to \mathcal{A}$ 是函子。则 $F \dashv U$ 当且仅当同态集函子 $\text{Hom}_{\mathcal{B}}(F(-), -)$ 和 $\text{Hom}_{\mathcal{A}}(-, U(-))$ 作为函子 $\mathcal{A}^{\text{op}} \times \mathcal{B} \to \text{Set}$ 自然同构。

**证明概要**：

1. **若 $F \dashv U$，则同态集函子自然同构**：
   
   - 假设 $F \dashv U$，即存在单位自然变换 $\eta: \text{Id}_{\mathcal{A}} \to U \circ F$，并且对于任意 $A \in \mathcal{A}$ 和 $B \in \mathcal{B}$，存在唯一的态射 $g: F(A) \to B$ 使得 $f = U(g) \circ \eta_A$。
   
   - 定义同构 $\beta_{A,B}: \text{Hom}_{\mathcal{B}}(F(A), B) \to \text{Hom}_{\mathcal{A}}(A, U(B))$ 由 $\beta_{A,B}(g) = U(g) \circ \eta_A$ 给出。
   
   - 定义逆同构 $\gamma_{A,B}: \text{Hom}_{\mathcal{A}}(A, U(B)) \to \text{Hom}_{\mathcal{B}}(F(A), B)$ 由 $\gamma_{A,B}(f) = g$，其中 $g$ 是唯一满足 $f = U(g) \circ \eta_A$ 的态射。
   
   - 通过验证 $\gamma_{A,B} \circ \beta_{A,B} = \text{id}$ 和 $\beta_{A,B} \circ \gamma_{A,B} = \text{id}$，可得 $\beta$ 和 $\gamma$ 是自然同构。

2. **反之，若同态集函子自然同构，则 $F \dashv U$**：

   - 假设存在自然同构 $\text{Hom}_{\mathcal{B}}(F(-), -) \cong \text{Hom}_{\mathcal{A}}(-, U(-))$。
   
   - 对于每个 $A \in \mathcal{A}$，设 $\eta_A$ 是对应于 $\text{id}_{F(A)}$ 的同态集同构中的元素，即 $\eta_A: A \to U(F(A))$。
   
   - 证明 $\eta$ 满足伴随函子的单位自然变换的性质。
   
   - 通过唯一性和自然性，可以构造出 $F \dashv U$ 的伴随关系。

**结论**：

此定理展示了伴随函子之间的关系可以通过同态集函子的自然同构来等价地描述，这是理解伴随函子关系的另一种重要方式。

### 13.3.3 命题13.2.2的证明

**命题13.2.2**：

设 $F: \mathcal{A} \to \mathcal{B}$ 和 $U: \mathcal{B} \to \mathcal{A}$ 是函子，且 $F \dashv U$。则存在一个自然变换 $\epsilon: F \circ U \to \text{Id}_{\mathcal{B}}$，称为余单位自然变换（counit of the adjunction），满足对于任何 $g: F(A) \to B$，存在唯一的态射 $f: A \to U(B)$，使得：
$$
\epsilon_B \circ F(f) = g
$$

**证明概要**：

1. **利用同构 $\beta$ 和 $\gamma$**：

   - 定义 $\beta_{A,B}: \text{Hom}_{\mathcal{B}}(F(A), B) \to \text{Hom}_{\mathcal{A}}(A, U(B))$ 由 $\beta_{A,B}(g) = U(g) \circ \eta_A$ 给出。
   
   - 定义逆同构 $\gamma_{A,B}: \text{Hom}_{\mathcal{A}}(A, U(B)) \to \text{Hom}_{\mathcal{B}}(F(A), B)$ 由 $\gamma_{A,B}(f)$ 是唯一满足 $f = U(g) \circ \eta_A$ 的态射 $g$。
   
   - 通过验证 $\beta \circ \gamma = \text{id}$ 和 $\gamma \circ \beta = \text{id}$，得到 $\beta$ 和 $\gamma$ 是同构。

2. **构造余单位自然变换 $\epsilon$**：

   - 对于每个 $B \in \mathcal{B}$，令 $\epsilon_B = \gamma_{F(A), B}( \text{id}_{F(A)} )$，即 $\epsilon_B: F(U(B)) \to B$。
   
   - 验证 $\epsilon$ 满足自然性条件，确保其为余单位自然变换。

3. **验证普遍映射性质**：

   - 对于任何 $g: F(A) \to B$，有 $g = \epsilon_B \circ F(f)$，其中 $f = \beta_{A,B}(g) = U(g) \circ \eta_A$。
   
   - 因为 $\gamma$ 是 $\beta$ 的逆，同样 $f$ 是唯一的满足条件的态射。

**结论**：

通过上述步骤，证明了在 $F \dashv U$ 的情况下，存在余单位自然变换 $\epsilon$，并且它满足伴随函子的普遍映射性质。此证明展示了伴随函子关系的对偶性和结构性。

### 13.3.4 伴随函子的唯一性（Uniqueness of Adjoints）

**命题13.3.4**：

若 $U: \mathcal{B} \to \mathcal{A}$ 是一个函子，则其左伴随函子（若存在）在自然同构的意义下是唯一的。

**证明概要**：

1. **设定**：

   - 设 $F$ 和 $F'$ 都是 $U$ 的左伴随函子。
   
2. **利用同构的自然同构**：

   - 对于每个 $A \in \mathcal{A}$，有自然同构 $\text{Hom}_{\mathcal{B}}(F(A), -) \cong \text{Hom}_{\mathcal{A}}(A, U(-))$ 和 $\text{Hom}_{\mathcal{B}}(F'(A), -) \cong \text{Hom}_{\mathcal{A}}(A, U(-))$。
   
   - 因此，$\text{Hom}_{\mathcal{B}}(F(A), -) \cong \text{Hom}_{\mathcal{B}}(F'(A), -)$ 作为函子 $\mathcal{B} \to \text{Set}$ 的自然同构。
   
3. **应用Yoneda引理**：

   - 由Yoneda引理（Theorem 4.5.3），如果 $\text{Hom}_{\mathcal{B}}(F(A), -) \cong \text{Hom}_{\mathcal{B}}(F'(A), -)$ 自然同构，则 $F(A) \cong F'(A)$。
   
4. **构造自然同构**：

   - 对于每个 $A \in \mathcal{A}$，存在唯一的同构 $\phi_A: F(A) \to F'(A)$，使得对于所有 $B \in \mathcal{B}$，有 $\phi_A^* \circ \text{id}_{F(A)} = \text{id}_{F(A)}$ 和 $\text{id}_{F'(A)} \circ \phi_A = \text{id}_{F'(A)}$。
   
   - 通过自然性的要求，这些同构 $\phi_A$ 组成了一个自然同构 $\phi: F \to F'$。

**结论**：

因此，左伴随函子（若存在）在自然同构的意义下是唯一的。这意味着，尽管可能存在多个左伴随函子，但它们在范畴论的结构性意义下是等价的。

### 13.3.5 逐点伴随性定理（Pointwise Adjointness Theorem）

**定理13.3.5（Pointwise Adjointness Theorem）**：

设 $\mathcal{A}$ 和 $\mathcal{B}$ 是范畴，且 $U: \mathcal{B} \to \mathcal{A}$ 是一个函子。如果对于每个对象 $A \in \mathcal{A}$，存在一个对象 $F(A) \in \mathcal{B}$ 使得同态集函子 $\text{Hom}_{\mathcal{B}}(F(A), -)$ 与 $\text{Hom}_{\mathcal{A}}(A, U(-))$ 自然等价，则存在一个函子 $F: \mathcal{A} \to \mathcal{B}$，使得 $F \dashv U$。

**解释**：

此定理表明，如果我们能够为每个对象 $A \in \mathcal{A}$ 在 $\mathcal{B}$ 中找到一个对象 $F(A)$，并且对于所有 $B \in \mathcal{B}$，有自然同构 $\text{Hom}_{\mathcal{B}}(F(A), B) \cong \text{Hom}_{\mathcal{A}}(A, U(B))$，那么我们可以将这些 $F(A)$ 组织成一个函子 $F$，并且 $F \dashv U$。

**应用**：

- 此定理是构造伴随函子的一个重要工具，特别是在需要逐点定义伴随函子时。
- 通过验证同态集的自然同构，可以系统地构造伴随函子，而无需直接构造自然变换 $\eta$ 和 $\epsilon$。

**备注**：

- 该定理的证明类似于**第13.1.3节**中自由单子函子的构造方法，但更为一般化。
- 详细证明可参考[Barr and Wells, 1985]，Section 1.9, Theorem 1 或 [Mac Lane, 1971]，Chapter IV。

### 13.3.6 命题：伴随函子的普遍元素

**命题13.3.6**：

一个函子 $U: \mathcal{B} \to \mathcal{A}$ 有左伴随函子当且仅当，对于每个对象 $A \in \mathcal{A}$，同态集函子 $\text{Hom}_{\mathcal{A}}(A, U(-))$ 有一个普遍元素。如果 $b: A \to U(E)$ 是这个普遍元素，则 $F(A) = E$ 且 $b: A \to U(F(A))$ 是单位自然变换 $\eta$ 在 $A$ 上的分量。

**解释**：

- **普遍元素**：指在函子 $\text{Hom}_{\mathcal{A}}(A, U(-))$ 中，一个元素 $b: A \to U(E)$ 满足对于所有 $B \in \mathcal{B}$，任何态射 $f: A \to U(B)$ 都唯一地对应一个态射 $g: E \to B$ 使得 $f = U(g) \circ b$。
- **意义**：此命题提供了一种通过普遍元素构造左伴随函子的方法，进一步强化了逐点伴随性定理的应用。

**应用**：

- 可以使用此命题来证明具体例子中伴随函子的存在性，如自由单子函子 $F \dashv U$。

**示例**：

- **自由单子函子**：通过命题13.3.6，可以从命题13.1.2的普遍映射性质推导出自由单子函子 $F: \text{Set} \to \text{Mon}$ 是忘记函子 $U: \text{Mon} \to \text{Set}$ 的左伴随函子。

### 13.3.7 保持极限和余极限的定理（Theorem: Adjoints Preserve Limits and Colimits）

**定理13.3.7**：

若 $F: \mathcal{A} \to \mathcal{B}$ 是 $U: \mathcal{B} \to \mathcal{A}$ 的左伴随函子（即 $F \dashv U$），则 $U$ 保持所有的极限，$F$ 保持所有的余极限。

**证明概要**：

1. **$U$ 保持极限**：

   - 设 $D: I \to \mathcal{B}$ 是一个小图，且 $V: \Delta I \to D$ 是它的极限态射（cone）。
   
   - 需要证明 $U(V)$ 是 $U(D)$ 的极限。
   
   - 利用伴随函子的性质，将同态集函子的自然同构转化为同态集的等价性，进而推导出 $U(V)$ 满足极限的普遍映射性质。
   
   - 具体来说，利用范畴论中的对偶性和普遍映射性质，可以构造出 $U(V)$ 的极限态射。
   
2. **$F$ 保持余极限**：

   - 通过对偶性，可以类比证明 $F$ 保持余极限。
   
   - 即，若 $F \dashv U$，则 $F$ 保持余极限，因为 $U$ 保持极限。

**备注**：

- **极限**：包括所有的产品、等式、核等构造。
- **余极限**：包括所有的余积、余等式、余核等构造。
- 伴随函子的这一性质在范畴论中极为重要，因为它连接了不同范畴之间的结构保持性。

**进一步讨论**：

- **对偶性**：范畴论中的对偶性意味着许多定理在对偶范畴中也成立。伴随函子保持极限和余极限的定理利用了这种对偶性。
- **Freyd的伴随函子定理**（Adjoint Functor Theorem）：探讨伴随函子的存在性与范畴的完备性和余完备性之间的关系，是理解伴随函子理论的更深层次内容。

**结论**：

伴随函子不仅在定义和构造上具有重要意义，还在保持范畴结构（极限和余极限）方面发挥关键作用。这使得伴随函子成为范畴论中连接不同范畴结构的强大工具。

### 13.3.8 习题解析

本节提供了一系列习题，旨在巩固和扩展对伴随函子高级性质的理解。以下是部分习题的详细解析：

**练习1**：

**题目**：设 $\mathcal{A}$ 和 $\mathcal{B}$ 是范畴，且 $T: \mathcal{A} \to \mathcal{B}$，$L, R: \mathcal{B} \to \mathcal{A}$ 是函子，满足 $L \dashv T \dashv R$。证明 $L \circ T \dashv R \circ T$ 作为 $\mathcal{A}$ 和 $\mathcal{B}$ 上的自同构函子。

**解析**：

**目标**：证明在 $L \dashv T \dashv R$ 的假设下，$L \circ T \dashv R \circ T$。

**证明**：

1. **伴随函子的定义**：

   - $L \dashv T$：存在自然变换 $\eta: \text{Id}_{\mathcal{A}} \to T \circ L$ 和 $\epsilon: L \circ T \to \text{Id}_{\mathcal{B}}$ 满足伴随关系的三角恒等式。
   
   - $T \dashv R$：存在自然变换 $\eta': \text{Id}_{\mathcal{B}} \to R \circ T$ 和 $\epsilon': T \circ R \to \text{Id}_{\mathcal{A}}$ 满足伴随关系的三角恒等式。
   
2. **构造 $L \circ T \dashv R \circ T$**：

   - 需要构造自然变换 $\eta'': \text{Id}_{\mathcal{A}} \to (R \circ T) \circ (L \circ T)$ 和 $\epsilon'': (L \circ T) \circ (R \circ T) \to \text{Id}_{\mathcal{B}}$。
   
   - 定义 $\eta'' = R \circ \eta \circ L \circ T$。
   
   - 定义 $\epsilon'' = \epsilon \circ T \circ R \circ T$。
   
3. **验证三角恒等式**：

   - 通过组合 $L \dashv T$ 和 $T \dashv R$ 的三角恒等式，验证 $L \circ T \dashv R \circ T$ 的三角恒等式。
   
   - 具体而言，验证 $(R \circ T) \circ \eta'' = \text{id}$ 和 $\epsilon'' \circ (L \circ T) = \text{id}$。

4. **结论**：

   - 因为自然变换 $\eta''$ 和 $\epsilon''$ 满足伴随函子的三角恒等式，故 $L \circ T \dashv R \circ T$。

**结论**：

在 $L \dashv T \dashv R$ 的假设下，$L \circ T \dashv R \circ T$，即伴随函子的组合仍然构成伴随关系。

---

**练习2**：

**题目**：

a. 设 $X$ 是一个实数集，作为一个有序集（考虑为范畴）。证明包含 $X \subseteq \mathbb{R}$ 的极限（如果存在）是 $X$ 的下确界（infimum），同理，包含的余极限是 $X$ 的上确界（supremum）。

**解析**：

**目标**：证明在有序集范畴中，包含 $X \subseteq \mathbb{R}$ 的极限是 $X$ 的下确界，余极限是上确界。

**证明**：

1. **极限的定义**：

   - 在有序集范畴中，极限是最小上界。
   
   - 对于包含函子 $i: X \hookrightarrow \mathbb{R}$，极限是 $X$ 的下确界（infimum）。

2. **证明下确界是极限**：

   - 设 $\inf X = m$，则 $m$ 是 $X$ 的下确界，满足：
     - $m \leq x$ 对所有 $x \in X$ 成立。
     - 若 $n \leq x$ 对所有 $x \in X$ 成立，则 $n \leq m$。
   
   - 这正是极限的普遍映射性质，即对于任意 $y \in \mathbb{R}$，若 $y \leq x$ 对所有 $x \in X$ 成立，则 $y \leq m$。
   
3. **同理，余极限是上确界**：

   - 设 $\sup X = M$，则 $M$ 是 $X$ 的上确界，满足：
     - $x \leq M$ 对所有 $x \in X$ 成立。
     - 若 $x \leq n$ 对所有 $x \in X$ 成立，则 $M \leq n$。
   
   - 这正是余极限的普遍映射性质。

**结论**：

在有序集范畴中，包含 $X \subseteq \mathbb{R}$ 的极限是 $X$ 的下确界，余极限是上确界。

**练习2b**：

**题目**：

参考上一节的练习4，证明天花板函数（ceiling function）保持上确界，但不一定保持下确界；地板函数（floor function）保持下确界，但不一定保持上确界。

**解析**：

**目标**：证明天花板函数 $\lceil \cdot \rceil$ 保持上确界，但不一定保持下确界；地板函数 $\lfloor \cdot \rfloor$ 保持下确界，但不一定保持上确界。

**证明**：

1. **天花板函数保持上确界**：

   - 设 $X \subseteq \mathbb{R}$，且 $\sup X = M$。
   
   - 计算 $\lceil X \rceil = \{ \lceil x \rceil \mid x \in X \}$。
   
   - 因为 $x \leq M$ 对所有 $x \in X$ 成立，且 $\lceil x \rceil \leq \lceil M \rceil$，所以 $\sup \lceil X \rceil \leq \lceil M \rceil$。
   
   - 由于 $M \leq \lceil M \rceil$，且 $\lceil X \rceil$ 中存在 $\lceil M \rceil$，则 $\sup \lceil X \rceil = \lceil M \rceil$。

2. **天花板函数不保持下确界**：

   - 设 $X = (0, 1) \subseteq \mathbb{R}$，则 $\inf X = 0$。
   
   - 计算 $\lceil X \rceil = \{ \lceil x \rceil \mid x \in (0, 1) \} = \{1\}$，则 $\inf \lceil X \rceil = 1 \neq 0 = \inf X$。
   
3. **地板函数保持下确界**：

   - 设 $X \subseteq \mathbb{R}$，且 $\inf X = m$。
   
   - 计算 $\lfloor X \rfloor = \{ \lfloor x \rfloor \mid x \in X \}$。
   
   - 因为 $m \leq x$ 对所有 $x \in X$ 成立，且 $\lfloor m \rfloor \leq \lfloor x \rfloor$，所以 $\inf \lfloor X \rfloor \geq \lfloor m \rfloor$。
   
   - 并且 $\lfloor m \rfloor$ 是 $\lfloor X \rfloor$ 的下界，所以 $\inf \lfloor X \rfloor = \lfloor m \rfloor \leq m$。
   
   - 因此，$\lfloor X \rfloor$ 的下确界与 $X$ 的下确界一致，即 $\inf \lfloor X \rfloor = \lfloor m \rfloor$。

4. **地板函数不保持上确界**：

   - 设 $X = (0, 1) \subseteq \mathbb{R}$，则 $\sup X = 1$。
   
   - 计算 $\lfloor X \rfloor = \{ \lfloor x \rfloor \mid x \in (0, 1) \} = \{0\}$，则 $\sup \lfloor X \rfloor = 0 \neq 1 = \sup X$。

**结论**：

天花板函数 $\lceil \cdot \rceil$ 保持上确界，但不一定保持下确界；地板函数 $\lfloor \cdot \rfloor$ 保持下确界，但不一定保持上确界。

---

**练习3**：

**题目**：证明在定理13.3.2的证明中定义的同构 $\beta_{A,B}$ 是一个自然变换，从 $\text{Hom}_{\mathcal{B}}(F(-), -)$ 到 $\text{Hom}_{\mathcal{A}}(-, U(-))$，作为函子 $\mathcal{A}^{\text{op}} \times \mathcal{B} \to \text{Set}$。

**解析**：

**目标**：证明 $\beta_{A,B}: \text{Hom}_{\mathcal{B}}(F(A), B) \to \text{Hom}_{\mathcal{A}}(A, U(B))$ 是一个自然变换。

**定义**：

- 对于每个 $(A, B) \in \mathcal{A}^{\text{op}} \times \mathcal{B}$，定义 $\beta_{A,B}(g) = U(g) \circ \eta_A$，其中 $\eta_A: A \to U(F(A))$ 是单位自然变换。

**自然性的定义**：

需要验证对于任意态射 $(f: A' \to A, h: B \to B')$，下图交换：

$$
\begin{array}{ccc}
\text{Hom}_{\mathcal{B}}(F(A), B) & \xrightarrow{\beta_{A,B}} & \text{Hom}_{\mathcal{A}}(A, U(B)) \\
\downarrow{\text{Hom}(F(f), h)} & & \downarrow{\text{Hom}(f, U(h))} \\
\text{Hom}_{\mathcal{B}}(F(A'), B') & \xrightarrow{\beta_{A',B'}} & \text{Hom}_{\mathcal{A}}(A', U(B'))
\end{array}
$$

**验证步骤**：

1. **计算上方路径**：

   $$
   \text{Hom}(f, U(h)) \circ \beta_{A,B}(g) = \text{Hom}(f, U(h))(U(g) \circ \eta_A) = U(h) \circ U(g) \circ \eta_A
   $$
   
2. **计算下方路径**：

   $$
   \beta_{A',B'} \circ \text{Hom}(F(f), h)(g) = \beta_{A',B'}(h \circ g \circ F(f)) = U(h \circ g \circ F(f)) \circ \eta_{A'}
   $$
   
3. **利用函子 $U$ 的性质**：

   $$
   U(h \circ g \circ F(f)) = U(h) \circ U(g) \circ U(F(f))
   $$
   
4. **利用单位自然变换的自然性**：

   因为 $\eta$ 是自然变换，满足 $U(F(f)) \circ \eta_{A'} = \eta_A \circ f$。
   
   - 即：
     $$
     U(F(f)) \circ \eta_{A'} = \eta_A \circ f
     $$
   
   - 代入：
     $$
     U(h) \circ U(g) \circ U(F(f)) \circ \eta_{A'} = U(h) \circ U(g) \circ (\eta_A \circ f) = U(h) \circ U(g) \circ \eta_A \circ f
     $$
   
   - 因此：
     $$
     U(h) \circ U(g) \circ \eta_A = U(h) \circ U(g) \circ \eta_A \circ f
     $$
   
5. **结论**：

   因此，$\text{Hom}(f, U(h)) \circ \beta_{A,B} = \beta_{A',B'} \circ \text{Hom}(F(f), h)$，满足自然性条件。

**结论**：

$\beta_{A,B}$ 是一个自然变换，从 $\text{Hom}_{\mathcal{B}}(F(-), -)$ 到 $\text{Hom}_{\mathcal{A}}(-, U(-))$，作为函子 $\mathcal{A}^{\text{op}} \times \mathcal{B} \to \text{Set}$。

---

**练习4**：

**题目**：对于任何函子 $G: \mathcal{C} \to \mathcal{D}$，令 $G^{\text{op}}: \mathcal{C}^{\text{op}} \to \mathcal{D}^{\text{op}}$ 是逆变函子（即在对象和态射上与 $G$ 相同，但方向相反）。证明对于任意函子 $F$ 和 $U$，如果 $F \dashv U$，则 $F^{\text{op}} \dashv U^{\text{op}}$。

**解析**：

**目标**：证明若 $F: \mathcal{A} \to \mathcal{B}$ 是 $U: \mathcal{B} \to \mathcal{A}$ 的左伴随函子（即 $F \dashv U$），则 $F^{\text{op}}: \mathcal{A}^{\text{op}} \to \mathcal{B}^{\text{op}}$ 是 $U^{\text{op}}: \mathcal{B}^{\text{op}} \to \mathcal{A}^{\text{op}}$ 的左伴随函子（即 $F^{\text{op}} \dashv U^{\text{op}}$）。

**证明**：

1. **伴随函子的对偶**：

   - 伴随函子关系在对偶范畴中仍然成立。
   
   - 具体而言，$F \dashv U$ 意味着 $\text{Hom}_{\mathcal{B}}(F(A), B) \cong \text{Hom}_{\mathcal{A}}(A, U(B))$，自然同构。
   
   - 取对偶，得到：
     $$
     \text{Hom}_{\mathcal{B}^{\text{op}}}(B, F^{\text{op}}(A)) \cong \text{Hom}_{\mathcal{A}^{\text{op}}}(U^{\text{op}}(B), A)
     $$
   
   - 这正是 $F^{\text{op}} \dashv U^{\text{op}}$ 的定义。

2. **构造自然同构**：

   - 由于 $F \dashv U$，存在自然同构 $\beta: \text{Hom}_{\mathcal{B}}(F(-), -) \cong \text{Hom}_{\mathcal{A}}(-, U(-))$。
   
   - 取对偶，得到 $\beta^{\text{op}}: \text{Hom}_{\mathcal{B}^{\text{op}}}(-, F^{\text{op}}(-)) \cong \text{Hom}_{\mathcal{A}^{\text{op}}}(U^{\text{op}}(-), -)$，满足自然性条件。

3. **验证伴随关系**：

   - 通过对偶自然同构，验证 $F^{\text{op}} \dashv U^{\text{op}}$。
   
   - 具体来说，对于任意 $A \in \mathcal{A}^{\text{op}}$ 和 $B \in \mathcal{B}^{\text{op}}$，有：
     $$
     \text{Hom}_{\mathcal{B}^{\text{op}}}(F^{\text{op}}(A), B) \cong \text{Hom}_{\mathcal{A}^{\text{op}}}(A, U^{\text{op}}(B))
     $$
   
   - 这与 $F^{\text{op}} \dashv U^{\text{op}}$ 的定义一致。

**结论**：

若 $F \dashv U$，则其逆变函子 $F^{\text{op}} \dashv U^{\text{op}}$。这展示了伴随函子关系在对偶范畴中的对称性。

---

**练习5**：

**题目**：这一系列习题旨在展示若范畴 $\mathcal{C}$ 具有有限极限，则函子 $P_A: \mathcal{C} \to \mathcal{C}/A$ 的存在右伴随函子 $R_A$ 等价于 $\mathcal{C}$ 中存在指数对象 $[A \rightarrow -]$，即满足 $\text{Hom}(A \times B, C) \cong \text{Hom}(B, [A \rightarrow C])$。

**解析**：

**目标**：通过一系列步骤证明，若 $\mathcal{C}$ 具有有限极限，则函子 $P_A: \mathcal{C} \to \mathcal{C}/A$ 存在右伴随函子 $R_A$ 等价于 $\mathcal{C}$ 中存在指数对象 $[A \rightarrow -]$。

**步骤**：

1. **定义函子 $P_A$ 和 $L_A$**：

   - **函子 $P_A: \mathcal{C} \to \mathcal{C}/A$**：将对象 $C$ 映射到投影态射 $p_2: C \times A \to A$，态射 $f: C \to D$ 映射到 $f \times \text{id}_A: C \times A \to D \times A$。
   
   - **函子 $L_A: \mathcal{C}/A \to \mathcal{C}$**：将对象 $f: B \to A$ 映射到 $B$，态射 $g: f \to f'$ 映射到 $g$。
   
2. **证明 $L_A \dashv P_A$**：

   - **定义**：构造自然同构 $\text{Hom}_{\mathcal{C}/A}(P_A(C), f) \cong \text{Hom}_{\mathcal{C}}(C, L_A(f))$。
   
   - **利用普遍映射性质**：通过验证伴随函子的普遍映射性质，证明 $L_A \dashv P_A$。
   
   - **构造单位和余单位自然变换**：
     - 单位自然变换 $\eta: \text{Id}_{\mathcal{C}} \to L_A \circ P_A$ 定义为 $\eta_C: C \to L_A(P_A(C)) = C \times A$，即 $\eta_C(c) = (c, a_0)$，其中 $a_0$ 是 $A$ 的某个固定元素。
     - 余单位自然变换 $\epsilon: P_A \circ L_A \to \text{Id}_{\mathcal{C}/A}$ 定义为投影态射 $p_2: C \times A \to A$。
   
3. **构造右伴随函子 $R_A$**：

   - 若 $P_A$ 存在右伴随函子 $R_A: \mathcal{C}/A \to \mathcal{C}$，则通过逐点伴随性定理，可构造出指数对象 $[A \rightarrow C]$。
   
4. **建立指数对象的存在性与伴随函子的等价性**：

   - **若 $\mathcal{C}$ 中存在指数对象**，则通过构造 $R_A$，证明 $P_A \dashv R_A$。
   
   - **反之**，若 $P_A \dashv R_A$，则 $\mathcal{C}$ 中存在指数对象。

**结论**：

通过上述步骤，证明了在具有有限极限的范畴 $\mathcal{C}$ 中，函子 $P_A$ 存在右伴随函子 $R_A$ 等价于 $\mathcal{C}$ 中存在指数对象 $[A \rightarrow -]$。

---

**练习6**：

**题目**：证明图的路径范畴构造（path category）是切片范畴与基本函子之间的伴随函子对。

**解析**：

**目标**：证明图的路径范畴构造 $\text{Path}$ 是切片范畴与基本函子之间的左伴随函子。

**证明**：

1. **定义路径范畴构造**：

   - **路径范畴**：给定一个图 $G$，其路径范畴 $\text{Path}(G)$ 包含所有从 $G$ 开始的路径，态射是路径之间的延伸。
   
   - **基础函子**：切片范畴 $\mathcal{C}/A$ 中的对象是从某对象到 $A$ 的态射。

2. **构造伴随函子对**：

   - **自由路径函子** $\text{Path}: \text{Graph} \to \text{Cat}$：将图 $G$ 映射到其路径范畴 $\text{Path}(G)$。
   
   - **基础函子** $U: \text{Cat} \to \text{Graph}$：将范畴 $\mathcal{C}$ 映射到其基础图，其中对象是图的顶点，态射是图的边。
   
3. **验证普遍映射性质**：

   - 证明 $\text{Path} \dashv U$，即存在自然同构 $\text{Hom}_{\text{Cat}}(\text{Path}(G), \mathcal{C}) \cong \text{Hom}_{\text{Graph}}(G, U(\mathcal{C}))$。
   
   - 通过验证同态集之间的自然同构，确保路径范畴构造与基础函子构成伴随函子对。

**结论**：

通过验证同态集的自然同构，证明图的路径范畴构造是切片范畴与基础函子之间的伴随函子对。这展示了伴随函子在构造复杂范畴结构中的应用。

---

**练习7**：

**题目**：找出从范畴 $\text{Cat}$ 到 $\text{Set}$ 的对象集函子的左伴随和右伴随函子。

**解析**：

**目标**：找出范畴 $\text{Cat}$ 到 $\text{Set}$ 的对象集函子（underlying object set functor）的左伴随和右伴随函子。

**证明**：

1. **定义对象集函子** $U: \text{Cat} \to \text{Set}$：

   - 将一个范畴 $\mathcal{C}$ 映射到其对象集 $\text{Ob}(\mathcal{C})$。
   
   - 将一个函子 $F: \mathcal{C} \to \mathcal{D}$ 映射到 $F$ 在对象上的函数 $\text{Ob}(F): \text{Ob}(\mathcal{C}) \to \text{Ob}(\mathcal{D})$。

2. **寻找左伴随函子**：

   - **左伴随函子** $F: \text{Set} \to \text{Cat}$：将一个集合 $S$ 映射到离散范畴 $D(S)$，其中 $D(S)$ 的对象是 $S$，态射仅为恒等态射。
   
   - **普遍映射性质**：
     $$
     \text{Hom}_{\text{Cat}}(D(S), \mathcal{C}) \cong \text{Hom}_{\text{Set}}(S, U(\mathcal{C}))
     $$
   
   - **解释**：每个从离散范畴 $D(S)$ 到范畴 $\mathcal{C}$ 的函子对应于一个从集合 $S$ 到 $\text{Ob}(\mathcal{C})$ 的函数。

3. **寻找右伴随函子**：

   - **右伴随函子** $G: \text{Set} \to \text{Cat}$：将一个集合 $S$ 映射到自由范畴 $\text{Free}(S)$，其中 $\text{Free}(S)$ 是由集合 $S$ 生成的范畴，包含所有从 $S$ 到 $S$ 的自由生成的态射。
   
   - **普遍映射性质**：
     $$
     \text{Hom}_{\text{Cat}}(\mathcal{C}, \text{Free}(S)) \cong \text{Hom}_{\text{Set}}(U(\mathcal{C}), S)
     $$
   
   - **解释**：每个从范畴 $\mathcal{C}$ 到自由范畴 $\text{Free}(S)$ 的函子对应于一个从 $\text{Ob}(\mathcal{C})$ 到集合 $S$ 的函数。

**结论**：

对象集函子 $U: \text{Cat} \to \text{Set}$ 具有左伴随函子 $F: \text{Set} \to \text{Cat}$（离散范畴函子）和右伴随函子 $G: \text{Set} \to \text{Cat}$（自由范畴函子）。这展示了伴随函子在范畴间构造自由结构和离散结构中的应用。

---

**练习8**：

**题目**：证明对象集函子 $U: \text{Cat} \to \text{Set}$ 只有左伴随函子，而没有右伴随函子。

**解析**：

**目标**：证明对象集函子 $U: \text{Cat} \to \text{Set}$ 有左伴随函子（离散范畴函子），但不存在右伴随函子。

**证明**：

1. **已知**：

   - 对象集函子 $U$ 的左伴随函子是离散范畴函子 $F: \text{Set} \to \text{Cat}$，满足 $F \dashv U$。
   
2. **证明不存在右伴随函子**：

   - 假设存在右伴随函子 $G: \text{Set} \to \text{Cat}$，即 $U \dashv G$。
   
   - 则存在自然同构：
     $$
     \text{Hom}_{\text{Set}}(U(\mathcal{C}), S) \cong \text{Hom}_{\text{Cat}}(\mathcal{C}, G(S))
     $$
   
   - 选择 $\mathcal{C}$ 为离散范畴 $D(S)$，则：
     $$
     \text{Hom}_{\text{Set}}(U(D(S)), S) = \text{Hom}_{\text{Set}}(S, S) \cong \text{Hom}_{\text{Cat}}(D(S), G(S))
     $$
   
   - 然而，对于 $G(S)$ 为任何范畴，若 $G \dashv U$，则必须满足每个从离散范畴 $D(S)$ 到 $G(S)$ 的函子对应于从集合 $S$ 到 $S$ 的函数。
   
   - 这要求 $G(S)$ 必须是一个离散范畴，否则无法满足自然同构。
   
   - 因此，$G(S)$ 必须总是离散范畴，但这与自由范畴构造的情况不同，导致无法满足一般范畴的普遍映射性质。
   
   - 具体来说，若 $G(S)$ 必须是离散范畴，则对于任意范畴 $\mathcal{C}$，有：
     $$
     \text{Hom}_{\text{Cat}}(\mathcal{C}, G(S)) \cong \text{Hom}_{\text{Set}}(U(\mathcal{C}), S)
     $$
     但这只在 $G(S)$ 是离散范畴时成立，与伴随函子的构造不符。

3. **结论**：

   - 因此，对象集函子 $U: \text{Cat} \to \text{Set}$ 只有左伴随函子（离散范畴函子），而不存在右伴随函子。

**结论**：

对象集函子 $U$ 只有左伴随函子，而没有右伴随函子。这说明在范畴论中，并非所有函子都同时具有左右伴随函子，伴随函子的存在性依赖于特定的范畴结构。

---

**练习9**：

**题目**：证明对于任意非单元素的集合 $A$，函子 $- \times A: \text{Set} \to \text{Set}$ 没有左伴随函子。提示：$1 \times A$ 同构于 $A$。

**解析**：

**目标**：证明对于任意非单元素的集合 $A$，函子 $- \times A$ 没有左伴随函子。

**证明**：

1. **假设存在左伴随函子** $F: \text{Set} \to \text{Set}$，使得 $F \dashv (- \times A)$。

2. **利用伴随函子的性质**：

   - $F \dashv (- \times A)$ 意味着存在自然同构：
     $$
     \text{Hom}_{\text{Set}}(F(S), T) \cong \text{Hom}_{\text{Set}}(S, T \times A)
     $$
     对于所有 $S, T \in \text{Set}$。

3. **选择特定集合 $S = 1$（单元素集）**：

   - 则有：
     $$
     \text{Hom}_{\text{Set}}(F(1), T) \cong \text{Hom}_{\text{Set}}(1, T \times A) \cong T \times A
     $$
   
   - 因为从单元素集 $1$ 到 $T \times A$ 的函数对应于 $T \times A$ 的元素。

4. **构造函子 $F$**：

   - 令 $F(1) = A$，因为 $\text{Hom}(F(1), T) \cong T \times A$。
   
   - 但由提示可知 $1 \times A \cong A$，即 $F(1) \cong A$。

5. **推导 $F$ 的行为**：

   - 对于任意 $S \in \text{Set}$，由伴随函子的自然同构，$F(S)$ 必须满足：
     $$
     \text{Hom}(F(S), T) \cong \text{Hom}(S, T \times A)
     $$
   
   - 对于 $S = 1$，得到 $F(1) \cong A$。
   
   - 对于 $S = A$，得到 $\text{Hom}(F(A), T) \cong \text{Hom}(A, T \times A)$。

6. **分析特殊情况**：

   - 若 $F \dashv (- \times A)$，则 $F$ 应满足对于任意 $S, T \in \text{Set}$，
     $$
     \text{Hom}(F(S), T) \cong \text{Hom}(S, T \times A)
     $$
   
   - 但是，如果 $A$ 不是单元素集，左伴随函子 $F$ 的行为难以满足上述同构的自然性和一致性。
   
7. **反证法**：

   - 假设 $F$ 存在，则 $F(S)$ 必须在结构上能够“分解”为 $S \times A$ 的某种形式。
   
   - 由于 $A$ 不是单元素集，无法构造一个自然的 $F$ 使得上述同构对于所有集合 $S, T$ 都成立。
   
   - 因此，$- \times A$ 没有左伴随函子。

**结论**：

对于任意非单元素的集合 $A$，函子 $- \times A: \text{Set} \to \text{Set}$ 没有左伴随函子。这说明在范畴论中，伴随函子的存在性依赖于特定函子的结构特性。

---

**练习10**：

**题目**：在13.2.5节中，逆像函子的右伴随函子是余单位自然变换 $e$ 定义的。描述这个伴随函子的单位自然变换及其在集合范畴 $\text{Set}$ 中的具体形式。

**解析**：

**目标**：描述在13.2.5节中，逆像函子 $f^{-1}$ 的右伴随函子 $f_!$ 的单位自然变换 $\eta$ 及其在集合范畴中的具体形式。

**背景**：

- **逆像函子** $f^{-1}: \text{Sub}(T) \to \text{Sub}(S)$ 定义为 $f^{-1}(T_0) = \{ s \in S \mid f(s) \in T_0 \}$。
- **左伴随函子** $f_*: \text{Sub}(S) \to \text{Sub}(T)$ 定义为 $f_*(S_0) = \{ f(s) \mid s \in S_0 \}$。
- **右伴随函子** $f_!: \text{Sub}(S) \to \text{Sub}(T)$ 定义为 $f_!(S_0) = \{ t \in T \mid f^{-1}(\{t\}) \subseteq S_0 \}$。

**伴随函子的单位自然变换** $\eta: \text{Id}_{\text{Sub}(S)} \to f_* \circ f^{-1}$：

1. **定义**：

   - 对于每个 $S_0 \subseteq S$，定义 $\eta_{S_0}: S_0 \to f_*(f^{-1}(S_0))$。
   
   - 具体地，$\eta_{S_0}$ 是包含映射，因为 $S_0 \subseteq f^{-1}(f_*(S_0))$。
   
2. **在集合范畴 $\text{Set}$ 中的具体形式**：

   - 对于集合 $S$ 和其子集 $S_0 \subseteq S$，
     $$
     \eta_{S_0}: S_0 \hookrightarrow f^{-1}(f(S_0)) = \{ s \in S \mid f(s) \in f(S_0) \}
     $$
   - 因为 $S_0 \subseteq f^{-1}(f(S_0))$，所以 $\eta_{S_0}$ 是自然的包含映射。

**结论**：

在集合范畴中，逆像函子 $f^{-1}$ 的右伴随函子 $f_!$ 的单位自然变换 $\eta$ 是包含映射 $S_0 \hookrightarrow f^{-1}(f(S_0))$，反映了 $S_0$ 与其直接像 $f_*(S_0)$ 之间的关系。

## 本节总结

**伴随函子的进一步主题**扩展了我们对伴随函子的理解，通过同态集的伴随性定理、伴随函子的唯一性、逐点伴随性定理以及伴随函子在保持极限和余极限中的作用，全面展示了伴随函子的高级性质和广泛应用。

**主要要点**：

1. **同态集的伴随性**：
   - 通过同态集函子的自然同构，定义并理解了伴随函子的关系。
   - 定理13.3.2展示了伴随函子关系与同态集自然同构之间的等价性。

2. **伴随函子的唯一性**：
   - 若伴随函子存在，则其左伴随函子在自然同构的意义下是唯一的（命题13.3.4）。
   
3. **逐点伴随性定理**：
   - 提供了一种通过逐点构造伴随函子的方法（定理13.3.5）。
   
4. **伴随函子的普遍元素**：
   - 通过普遍元素的概念，建立了伴随函子的存在条件（命题13.3.6）。
   
5. **保持极限和余极限**：
   - 伴随函子具有保持范畴中的极限和余极限的性质（定理13.3.7）。

6. **具体例子与习题**：
   - 通过具体例子展示了伴随函子的广泛应用，如自由单子、积函子与投影函子、逆像函子等。
   - 通过习题巩固了对伴随函子高级性质的理解，强化了理论应用能力。

**应用与扩展**：

- **逻辑与类型理论**：伴随函子用于定义和理解高阶类型构造、函数空间等概念。
- **自由代数结构**：通过伴随函子构造自由单子、自由群等代数结构，系统化地生成满足特定公理的最自由结构。
- **范畴间的深层关系**：伴随函子提供了一种系统化的方法来分析和构造范畴间的深层关系，如积、指数对象、切片范畴等。

通过本节的学习，您应能全面理解伴随函子的高级性质、相关定理及其在不同范畴中的具体应用，为进一步掌握范畴论中的复杂理论和构造方法打下坚实的基础。

继续学习后续章节，您将发现伴随函子在构建更复杂的范畴结构、理解范畴间的深层关系以及应用于其他数学和计算机科学领域中，扮演着不可或缺的角色。

### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------