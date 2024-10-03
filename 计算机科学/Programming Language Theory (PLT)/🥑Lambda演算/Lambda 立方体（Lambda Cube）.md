

- https://www.wikiwand.com/zh/Lambda%E7%AB%8B%E6%96%B9%E4%BD%93

[toc]


$$
\xymatrix@-1pc{
{}\\ {}\\ {}\\ {}\\
&{\scriptsize(\Box,~\Box)} \\
{} \ar[u]^{(\Box,~\ast)} \ar[ur] \ar[r]_{(\ast,~\Box)} & {}
}
\qquad
\xymatrix@!=10pt{
& {\boldsymbol{\lambda\omega}} \ar@{-}[rr]\ar@{-}'[d][dd]
& & {\boldsymbol{\lambda\textbf{C}}} \ar@{-}[dd]
\\
{\boldsymbol{\lambda 2}} \ar@{-}[ur]\ar@{-}[rr]\ar@{-}[dd]
& & {\boldsymbol{\lambda\textbf{P}2}} \ar@{-}[ur]\ar@{-}[dd]
\\
& {\boldsymbol{\lambda \underline{\omega}}} \ar@{-}'[r][rr]
& & {\boldsymbol{\lambda\textbf{P}\underline{\omega}}}
\\
{\boldsymbol{\lambda_\to}} \ar@{-}[rr]\ar@{-}[ur]
& & {\boldsymbol{\lambda \textbf{P}}} \ar@{-}[ur]
}
$$






| 体系の名前 | (∗,∗) | (◻,∗) | (∗,◻) | (◻,◻) | 等価な型システム              | 対応する論理体系 |
| ---------- | ----- | ----- | ----- | ----- | ----------------------------- | ---------------- |
| λ→         | 〇    | ×     | ×     | ×     | 単純型付きλ計算               | 命題論理         |
| λ2         | 〇    | 〇    | ×     | ×     | system F                      | 2階命題論理      |
| λP         | 〇    | ×     | 〇    | ×     | Logical Framework (LF)        | 述語論理         |
| λω―        | 〇    | ×     | ×     | 〇    |                               | 弱高階命題論理   |
| λP2        | 〇    | 〇    | 〇    | ×     |                               | 2階述語論理      |
| λω         | 〇    | 〇    | ×     | 〇    | system Fω                     | 高階命題論理     |
| λPω―       | 〇    | ×     | 〇    | 〇    |                               | 弱高階述語論理   |
| λC         | 〇    | 〇    | 〇    | 〇    | Calculus of Construction (CC) | 高階述語論理     |

- https://liewecmays.net/articles/pure-type-system_lambda-cube

### ----------------------------------------------

Lambda 立方体（Lambda Cube）是一种用于表示不同类型系统间关系的模型，由 Henk Barendregt 提出。它主要用于研究和分类各种类型系统，特别是那些在函数式编程语言如 Haskell 中使用的系统。Lambda 立方体中的每一个维度代表类型系统能表达的一种能力。

Lambda 立方体有三个维度，分别是：
1. **类型依赖于类型**（Types depending on types）：允许类型操作其他类型，例如在系统中可以定义类型构造函数。
2. **类型依赖于项**（Types depending on terms）：允许类型表达式依赖于具体的值，例如依赖类型。
3. **项依赖于类型**（Terms depending on types）：允许表达式中的变量具有某种类型，这是最基本的类型系统能力。

### 通俗例子解释 Lambda 立方体

想象一个编程语言的类型系统是一座由多层构成的大楼：
- **底层（项依赖于类型）**：这是所有建筑必须的基础，表示我们的程序中的函数和变量都需要有类型。例如，我们可以声明一个整数变量 `int x = 5;`，这里 `x` 是基于 `int` 这个类型的。

- **第二层（类型依赖于项）**：在这一层，类型可以基于具体的值进行定义。例如，我们可以定义一个长度为 `n` 的数组类型，其中 `n` 是一个具体的数值。这允许我们根据实际的需要来定制类型，比如 `Array<5>` 表示一个具有 5 个元素的数组。

- **第三层（类型依赖于类型）**：这层允许类型构造器，可以根据一个类型来创建另一个类型。例如，我们可以定义一个泛型容器类型，如 `Container<T>`，其中 `T` 可以是任何其他类型，如 `Container<Integer>` 或 `Container<String>`。

这三个维度一起作用，形成了一个非常强大的类型系统框架，允许语言设计者和程序员根据需要定义更加丰富和复杂的类型。在 Lambda 立方体中，每个顶点都代表了这三个维度的一种组合，可以描述具有相应类型系统特征的编程语言。例如，简单的类型化 Lambda 演算（如 Java 中的方法类型系统）就位于立方体的一个角落，而更复杂的系统，如允许依赖类型的语言，则可能位于立方体的另一个角落。

![bcf6afc886daf30a921a8ac885feb5a1](https://p.ipic.vip/wnijcv.png)

**基本类型**（$\star$）和**类型变量**（$\square$）是类型系统中的核心概念，它们帮助我们理解类型构造和多态性在编程语言中的角色。让我们分别从两个方面来解释这两个符号，并通过相关例子说明它们在不同类型系统中的应用。

### 1. 基本类型（$\star$）

**基本类型**，也称为*具体类型*，是编程语言中最基础、最直接的类型。它们通常表示可以直接使用的值类型，比如整数、布尔值、字符串等。在类型论的符号中，$\star$ 是用来表示基本类型的。

#### 例子：
- 在 Haskell 或 OCaml 中，`Int`、`Bool` 就是具体类型，这些类型表示的值都是程序中可以直接操作的数据。

```haskell
-- 一个接受整数并返回整数的函数，参数类型和返回值类型都是基本类型
addOne :: Int -> Int
addOne x = x + 1
```

在这个例子中，`Int` 是一种基本类型，它可以被实例化为具体的值。

### 2. 类型变量（$\square$）

**类型变量**（或多态类型）允许类型抽象化，用来表示某种类型，但不指定其具体类型。它在泛型和多态系统中被广泛使用。类型变量使得我们可以编写能够适用于多种类型的函数或数据结构。在类型论的符号中，$\square$ 被用来表示类型变量。

#### 例子：
- 在 Haskell 中，类型变量通常用小写字母表示，例如 `a`、`b` 等。

```haskell
-- 这是一个多态函数 identity，它适用于任意类型
identity :: a -> a
identity x = x
```

在这个例子中，`a` 是一个类型变量，它可以代表任意类型。这个函数可以用于任何类型的输入，并返回相同类型的输出。这使得 `identity` 变得更加通用。

### 3. 类型构造与两者的结合

**类型构造器**可以将基本类型或类型变量作为输入，并生成新的类型。类型构造器可以是高阶的，即可以接受类型本身作为参数。

#### 例子：
- 在 Haskell 中，`Maybe` 是一个类型构造器，它接受一个类型（可以是 $\star$ 或 $\square$）作为参数，并返回一个新的类型。

```haskell
-- Maybe 类型构造器，接受一个类型参数并返回一个新的类型
maybeInt :: Maybe Int
maybeInt = Just 5

maybeString :: Maybe String
maybeString = Nothing
```

在这个例子中，`Maybe Int` 是用 `Int` 这样的基本类型构造的具体类型，而 `Maybe a` 是用类型变量 `a` 构造的多态类型。

### 4. 基本类型与类型变量的对比

| **概念**     | **符号**  | **定义**                                               | **例子**                          |
| ------------ | --------- | ------------------------------------------------------ | --------------------------------- |
| **基本类型** | $\star$   | 直接表示可操作的具体类型，例如整数、布尔值、字符串等。 | `Int`、`Bool`                     |
| **类型变量** | $\square$ | 表示抽象类型或泛型类型，用来定义通用的类型和多态函数。 | `a`（在 `identity :: a -> a` 中） |

### 5. 结合应用的理解

在高级类型系统（如 Haskell）中，$\star$ 和 $\square$ 结合使用可以构造非常复杂的类型。通过类型构造器，函数和数据结构能够接受类型参数，使得它们既可以应用于具体的基本类型，也可以应用于抽象的类型变量。

#### 例子：
```haskell
-- Functor 是一个高阶类型构造器，它接受一个类型构造器并作用于任意类型
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)
```

在这个例子中，`f` 是一个类型变量，它本身是一个类型构造器（可以是 `Maybe`、`[]` 等），而 `a` 和 `b` 是普通的类型变量。`Maybe` 接受基本类型 `Int` 或类型变量 `a` 来生成新的类型。

### 6. 总结

- **基本类型**（$\star$）是程序中直接操作的具体类型，如整数和布尔值。
- **类型变量**（$\square$）允许类型抽象化，能够用于编写通用的多态函数和数据结构。
- 通过**类型构造器**，可以将基本类型和类型变量结合，构造出复杂的高阶类型和多态结构。

这些概念是理解类型系统的核心，特别是在泛型和高阶类型编程中，它们有助于增强程序的灵活性和可复用性。

### ---------------------------

要详解类型系统表中的八个系统，我们需要理解每个系统如何扩展了基本的简单类型λ演算（Simply Typed Lambda Calculus，STLC）。以下是每个系统的详细解释：

1. **λ→ (STLC)**：
   - **关系**：$ \star \rightarrow \star $
   - **例子**：简单类型λ演算（STLC）
   - **解释**：在简单类型λ演算中，类型只包含基本类型（$\star$）和从一种类型到另一种类型的箭头类型（$ \rightarrow $）。这是最基本的类型系统，支持函数应用和函数抽象。

2. **λ2 (System F)**：
   - **关系**：$ \star \rightarrow \star $, $ \square \rightarrow \star $
   - **例子**：System F
   - **解释**：System F引入了类型多态性，允许函数不仅可以抽象值，还可以抽象类型。类型变量（$ \square $）可以被实例化为任何类型。这扩展了简单类型λ演算，使得泛型编程成为可能。

3. **λω (Weak λω)**：
   - **关系**：$ \star \rightarrow \star $, $ \square \rightarrow \square $
   - **例子**：Weak λω
   - **解释**：Weak λω进一步扩展了System F，允许类型构造器之间的抽象。这意味着我们可以构造接受类型并返回类型的类型构造器，使得类型系统更加灵活和强大。

4. **λω (System Fω)**：
   - **关系**：$ \star \rightarrow \star $, $ \square \rightarrow \star $, $ \square \rightarrow \square $
   - **例子**：System Fω
   - **解释**：System Fω结合了System F和Weak λω的特性，既允许类型多态性又允许类型构造器之间的抽象。这是一个非常强大的类型系统，能够表示复杂的类型关系和抽象。

5. **λP (LF)**：
   - **关系**：$ \star \rightarrow \star $, $ \star \rightarrow \square $
   - **例子**：LF（Logical Framework）
   - **解释**：LF是一种逻辑框架，用于表示逻辑推理中的各种规则。它允许类型依赖于其他类型，从而支持更复杂的逻辑表达。

6. **λP2 (λP2)**：
   - **关系**：$ \star \rightarrow \star $, $ \star \rightarrow \square $, $ \square \rightarrow \star $
   - **例子**：λP2
   - **解释**：λP2进一步扩展了LF，结合了类型多态性和类型依赖性。这使得它可以表示更复杂的逻辑和类型关系。

7. **λPω (Weak λPω)**：
   - **关系**：$ \star \rightarrow \star $, $ \star \rightarrow \square $, $ \square \rightarrow \square $
   - **例子**：Weak λPω
   - **解释**：Weak λPω是一个结合了Weak λω和LF的特性，允许类型构造器之间的抽象和类型依赖性。

8. **λPω (CoC)**：
   - **关系**：$ \star \rightarrow \star $, $ \square \rightarrow \star $, $ \star \rightarrow \square $, $ \square \rightarrow \square $
   - **例子**：CoC（Calculus of Constructions）
   - **解释**：CoC是λ立方体中最强大的系统，结合了所有前面系统的特性，允许完全的类型多态性、类型构造器抽象和类型依赖性。它是依赖类型理论的重要基础。

这些系统通过扩展基本类型λ演算，逐步增加了类型多态性、类型构造器和类型依赖性，使得类型系统能够表示更复杂的类型和逻辑关系。这些扩展使得编程语言能够更强大地表达和验证程序的正确性。

### -----------------------------

