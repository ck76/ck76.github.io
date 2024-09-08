[toc]

### -------------------------

### 第15章 单子与伴随（Monads and Adjunctions）

在这一章中，我们将探讨 **单子（monads）** 和 **伴随（adjunctions）** 之间的关系。通过理解如何从伴随构造单子，以及如何在编程中应用这一理论，我们可以深入理解函数式编程中的许多核心概念，如 **Monad 变换器（Monad Transformers）** 和 **Monad 代数（Monad Algebras）**。

---

### 15.1 字符串图（String Diagrams）

**字符串图** 是一种图形表示法，用来表示范畴论中的态射（箭头）和复合关系。在描述伴随关系时，字符串图可以帮助我们更直观地理解复杂的箭头组合。

在字符串图中：
- 箭头表示态射（函数）。
- 箭头的连接表示函数复合。
- 特定的形状和路径表示伴随和单子的构造方式。

例如，当我们处理 **伴随函子对（adjoint functors）** 时，左伴随和右伴随之间的关系可以通过字符串图直观地表示出来，从而展示如何通过伴随构造出单子。

---

### 15.2 从伴随构造单子（Monads from Adjunctions）

**单子（Monad）** 可以从 **伴随函子对** 中自然地构造出来。假设我们有一对伴随函子 $F \dashv G$，其中 $F$ 是左伴随函子，$G$ 是右伴随函子。伴随的定义表明存在自然同构：

$$ \text{Hom}(F(A), B) \cong \text{Hom}(A, G(B)) $$

从这个伴随函子对，我们可以构造出一个单子 $T = G \circ F$。构造过程如下：
1. **函子**：单子 $T$ 是通过组合 $G$ 和 $F$ 得到的，即 $T = G \circ F$。
2. **单子的单位**：由伴随的单位 $\eta : \text{Id} \to G \circ F$ 提供。
3. **单子的乘法**：通过伴随函子构造出的乘法是 $\mu : G \circ F \circ G \circ F \to G \circ F$。

这表明，通过伴随函子对，我们可以自然地构造一个 Monad。

---

### 15.3 从伴随推导出的单子示例（Examples of Monads from Adjunctions）

在这一节中，我们会通过具体的例子来说明如何从伴随构造单子。

#### 1. **自由-遗忘伴随（Free-Forgetful Adjoint）**

在代数结构中，自由对象（如自由群、自由模等）与遗忘函子之间构成一个伴随关系。通过这个伴随函子对，我们可以构造出相关的 Monad。

例如：
- **自由群函子** 是左伴随函子，遗忘函子是右伴随函子。
- 通过组合这两个函子，我们可以构造出一个 Monad，该 Monad 操作与群相关的计算。

#### 2. **上积-对角伴随（Product-Diagonal Adjoint）**

另一个常见的例子是范畴中的 **上积（product）** 和 **对角函子（diagonal functor）** 之间的伴随。通过这个伴随对，可以构造出一个 Monad，表示范畴中的特定计算模式。

---

### 15.4 Monad 变换器（Monad Transformers）

**Monad 变换器（Monad Transformers）** 是一种扩展 Monad 功能的工具，它允许我们将一个 Monad 叠加到另一个 Monad 之上，从而在多个 Monad 上组合不同的计算模式。

通过 Monad 变换器，编程者可以组合多个 Monad 的特性，例如将 **状态 Monad** 和 **IO Monad** 结合起来，从而同时处理状态和输入输出。

#### 1. **定义与用途**

Monad 变换器是一种将一个 Monad 转换为另一个 Monad 的工具。每个 Monad 变换器都有一个对应的基础 Monad，它扩展了基础 Monad 的功能。

例如：
- **StateT** 是状态 Monad 的变换器，它允许我们将状态操作与其他 Monad（如 IO）结合。
- **ReaderT** 是读者 Monad 的变换器，用于将依赖注入的上下文与其他 Monad 结合。

#### 2. **Monad 变换器的应用**

Monad 变换器在实际编程中非常有用，特别是在需要处理多种效果（如错误处理、状态管理、输入输出等）的场景中。通过 Monad 变换器，开发者可以优雅地组合多个效果，而无需手动管理复杂的 Monad 堆栈。

---

### 15.5 Monad 代数（Monad Algebras）

**Monad 代数（Monad Algebras）** 是一种将 Monad 与代数结构相结合的方式。在范畴论中，给定一个 Monad $T$，我们可以定义 $T$-代数，其中包含一个载体对象 $A$ 和一个满足特定条件的态射 $\alpha: T(A) \to A$。

#### 1. **定义**

Monad 代数的核心是通过一个态射 $\alpha$ 将 Monad 的结构映射到代数对象上。这意味着，Monad 代数描述了如何通过代数方式操作 Monad 中的值。

#### 2. **Monad 代数的性质**

每个 Monad 都可以构造出一类代数结构。这些代数结构在编程中有实际的应用，如状态 Monad 的代数、列表 Monad 的代数等。Monad 代数允许我们通过代数方式处理 Monad，从而简化了某些复杂的计算。

---

### 总结

第15章介绍了 **单子（Monad）** 与 **伴随（Adjunction）** 之间的紧密关系，并展示了如何通过伴随构造单子。此外，本章还讨论了 **Monad 变换器** 和 **Monad 代数** 的应用。在编程中，这些理论帮助我们设计出更具扩展性和灵活性的代码结构。

### -------------------------

### 15.1 字符串图（String Diagrams）

**字符串图** 是一种图形表示法，用来直观地展示范畴论中的概念，如函子（functor）、自然变换（natural transformation）等。通过字符串图，我们可以更容易地理解函子的组合和自然变换的组合。

#### 1. 范畴与函子

范畴（category）可以被表示为一个点，而函子（functor）作为箭头从一个范畴映射到另一个范畴。例如，考虑以下两个范畴 $\mathcal{C}$ 和 $\mathcal{D}$，以及从 $\mathcal{C}$ 到 $\mathcal{D}$ 的函子 $F$ 和 $G$，它们之间由自然变换 $\alpha$ 连接：

$$
\mathcal{C} \xrightarrow{F} \mathcal{D} \xrightarrow{G} \mathcal{C}
$$

在字符串图中，范畴被表示为平面的区域，函子是连接这些区域的边界线，自然变换 $\alpha$ 是将这些边界线段连接在一起的点。自然变换 $\alpha$ 作用于两个函子 $F$ 和 $G$ 之间，并可以视为在两个函子之间的“转换”。

```plaintext
  F   G
C ---> D
  \alpha
```

在 Haskell 中，自然变换 $\alpha$ 被表示为多态函数：

```haskell
alpha :: forall x. F x -> G x
```

字符串图的表示使得我们可以从下到上、从左到右读取这些图，模拟坐标系中的函数流动。

#### 2. 垂直组合的字符串图

当我们有多个自然变换时，它们可以组合起来。例如，如果有两个自然变换 $\alpha: F \to G$ 和 $\beta: G \to H$，则它们的垂直组合可以用字符串图表示：

```plaintext
  F   G   H
C ---> D ---> E
  \alpha  \beta
```

在 Haskell 中，我们可以通过函数组合来实现这些自然变换的组合：

```haskell
beta_alpha :: forall x. F x -> H x
beta_alpha = beta . alpha
```

#### 3. 水平组合的字符串图

对于水平组合，我们有三个范畴 $\mathcal{C}$、$\mathcal{D}$ 和 $\mathcal{E}$，函子 $F$ 和 $G$ 在 $\mathcal{C}$ 和 $\mathcal{D}$ 之间作用，而函子 $F'$ 和 $G'$ 在 $\mathcal{D}$ 和 $\mathcal{E}$ 之间作用：

```plaintext
  F   F' 
C ---> D ---> E
  G   G'
```

自然变换 $\alpha: F \to F'$ 和 $\beta: G \to G'$ 可以通过水平组合来表示，这对应于字符串图中的水平连接。

在 Haskell 中，我们可以像这样实现：

```haskell
g_alpha :: forall x. G (F x) -> G (F' x)
g_alpha = fmap alpha
```

#### 4. Whiskering（掺合）

Whiskering 是自然变换的特殊组合形式，其中一个自然变换是恒等变换。我们可以通过以下字符串图来表示 whiskering：

```plaintext
  F   
C ---> D
  G   G'
```

在这种组合中，一个自然变换是恒等的，因此我们不需要特别绘制它。例如，如果自然变换 $\beta: G \to G'$ 是一个多态函数，那么其 Haskell 实现是：

```haskell
beta_f :: forall x. G (F x) -> G' (F x)
beta_f = beta
```

#### 5. 交换律

自然变换的组合遵循 **交换律（commutative law）**，即自然变换的组合方式不影响最终结果。无论我们是先进行垂直组合，还是先进行水平组合，结果都是相同的。字符串图可以很好地展示这一点：

```plaintext
  F    F'  F''
C ---> D ---> E
  G    G'  G''
```

通过交换律，我们知道自然变换 $\alpha \circ \beta$ 和 $\beta' \circ \alpha'$ 可以自由地组合，而不需要担心顺序。在 Haskell 中，这表示为函数组合的结合性。

#### 6. 字符串图的灵活性

通过字符串图，自然变换的顺序和位置是可以自由调整的。这种灵活性类似于在编程中函数组合的灵活性，例如 Haskell 中的函数组合 `.`。字符串图为我们提供了一种直观的方式来理解自然变换的组合、函子之间的关系，以及范畴论中的各种概念。

### 总结

**字符串图** 是一种有力的工具，可以直观地展示函子、自然变换及其组合。在范畴论和编程中，字符串图帮助我们理解复杂的箭头和态射的组合，并展示了自然变换的垂直和水平组合如何在视觉上进行。通过 Haskell 中的函数组合，字符串图的这些组合可以用代码轻松实现。

### -------------------------

### 单子的字符串图（String Diagrams for the Monad）

**单子（Monad）** 是一个自函子（即作用于同一个范畴的函子），并配备有两个自然变换：**单位（unit）** 和 **结合（multiplication）**，分别称为 $\eta$ 和 $\mu$。字符串图为我们提供了一种直观的方式来表示和理解这些自然变换及其遵循的单子律。

#### 1. 单子的定义字符串图

我们首先回顾单子的定义。单子由一个函子 $T$ 和两个自然变换 $\eta$ 和 $\mu$ 组成：
- **单位变换 $\eta$**：$\eta : \text{Id} \to T$，将恒等函子映射到函子 $T$。
- **结合变换 $\mu$**：$\mu : T \circ T \to T$，将 $T$ 的两次应用压缩为一次应用。

这些变换可以通过以下字符串图表示：

1. **单位变换 $\eta$** 的图：
   ```
   Id
    |
    |
    η
    |
    T
   ```

   $\eta$ 将恒等函数映射到 $T$，表示为将恒等线（通常省略）通过 $\eta$ 点连接到 $T$ 线。

2. **结合变换 $\mu$** 的图：
   ```
    T     T
     \   /
      μ
      |
      T
   ```

   $\mu$ 将两个 $T$ 函子组合为一个，表示为两条 $T$ 线通过 $\mu$ 点连接为一条 $T$ 线。

#### 2. 单子律的字符串图

字符串图在表达 **单子律（Monad Laws）** 时非常有用。单子律包括 **左恒等律**、**右恒等律** 和 **结合律**。

##### 2.1 左恒等律

左恒等律表示：$ \mu \circ (\eta \circ T) = \text{id}_T $。这意味着，先将单位变换 $\eta$ 应用到 $T$，再应用结合变换 $\mu$，结果相当于对 $T$ 做恒等变换。

字符串图表示如下：
```
    T
     |
    η | T
     |  
    μ
     |
    T
```

通过上下拉动 $\eta$，可以看到它缩回到直线中，表明最终的结果是 $\text{id}_T$，即一条直接的 $T$ 线。

##### 2.2 右恒等律

右恒等律表示：$ \mu \circ (T \circ \eta) = \text{id}_T $。这与左恒等律类似，但作用在右边。

字符串图表示如下：
```
    T
     |
    T | η
     |  
    μ
     |
    T
```

同样，通过拉动 $\eta$，我们可以看到右恒等律也简化为一条直接的 $T$ 线。

##### 2.3 结合律

结合律表示：$ \mu \circ (T \circ \mu) = \mu \circ (\mu \circ T) $。这意味着无论我们如何组合三个 $T$，最终的结果是相同的。

字符串图表示如下：
```
   T       T
    \     /
     μ   T
      \ /
       μ
       |
       T
```

这个图展示了两条路径的组合，无论我们先应用哪一个 $\mu$，最终的结果都是相同的 $T$ 线。

#### 3. 总结

通过字符串图，我们可以直观地理解单子的构造和单子律。字符串图帮助我们展示自然变换的组合方式，并且可以通过图形变换验证律的正确性。在编程中，单子律保证了我们在使用 Monad 进行计算时具有一致性和正确性。

### -------------------------

### 伴随函子的字符串图（String Diagrams for the Adjunction）

**伴随函子（adjoint functors）** 是一对函子 $L: \mathcal{D} \to \mathcal{C}$ 和 $R: \mathcal{C} \to \mathcal{D}$，它们通过 **伴随单元（unit）** $\eta: \text{Id}_\mathcal{D} \to R \circ L$ 和 **伴随余单元（counit）** $\varepsilon: L \circ R \to \text{Id}_\mathcal{C}$ 相互关联，并满足 **三角恒等式（triangle identities）**。

#### 1. 伴随单元的字符串图

伴随单元 $\eta$ 将恒等函子 $Id_\mathcal{D}$ 映射到 $R \circ L$。这可以通过一个“杯”形的字符串图来表示：

```plaintext
L   R
 \ /
  η
```

在这个图中，$\eta$ 点将下方的恒等函子变换为上方的 $R \circ L$ 组合。

#### 2. 伴随余单元的字符串图

伴随余单元 $\varepsilon$ 将 $L \circ R$ 映射回恒等函子 $Id_\mathcal{C}$，其字符串图为“帽”形：

```plaintext
  ε
 / \
L   R
```

伴随余单元 $\varepsilon$ 是 $L \circ R$ 函子和恒等函子之间的变换，表示为将 $L$ 和 $R$ 的组合通过 $\varepsilon$ 点变回恒等函数。

#### 3. 三角恒等式的字符串图

**三角恒等式** 是伴随函子必须满足的两个条件。这些恒等式可以用字符串图表示，使得我们直观理解函子的关系。

##### 3.1 第一个三角恒等式（Zig-Zag Identity）

第一个三角恒等式是 $R \circ \varepsilon \circ \eta \circ R = \text{id}_R$，它展示了伴随单元和余单元的组合如何在 $R$ 上产生恒等变换。字符串图表示如下：

```plaintext
R
L
 \   /
  ε
 /   \
L     R
  η
 / \
R   R
```

这个图说明从下到上读取的映射序列：
- 首先应用 $\eta$ 将 $R$ 转换为 $R \circ L \circ R$；
- 然后应用 $\varepsilon$ 将其缩回到 $R$；
- 最终等价于 $R$ 上的恒等自然变换。

我们可以通过“拉直”中间的部分（即 $\eta$ 和 $\varepsilon$）来展示其等价性。这也可以翻译为 Haskell：

```haskell
triangle :: forall x. R x -> R x
triangle = fmap counit . unit
```

在 Haskell 中，`unit` 对应于伴随单元 $\eta$，`counit` 对应于伴随余单元 $\varepsilon$，而函数组合 `fmap counit . unit` 实现了三角恒等式。

##### 3.2 第二个三角恒等式

第二个三角恒等式是 $L \circ \eta \circ \varepsilon \circ L = \text{id}_L$，展示了 $L$ 的组合如何生成恒等变换。字符串图如下：

```plaintext
L   L
 \ /
  η
 / \
L   R
 \   /
  ε
 / \
L   L
```

从下到上读取该图：
- 首先应用 $\varepsilon$ 将 $L \circ R$ 缩回到 $L$；
- 然后通过 $\eta$ 将其扩展回 $L \circ R \circ L$；
- 最终结果是 $L$ 上的恒等自然变换。

这个等式也可以用 Haskell 表达：

```haskell
triangle' :: forall x. L x -> L x
triangle' = counit . fmap unit
```

此 Haskell 实现表示了第二个三角恒等式的组合，其中 `fmap unit` 将伴随单元应用于 $L$ 函子，`counit` 则实现伴随余单元。

### 总结

通过字符串图，我们可以直观地理解伴随函子之间的关系以及它们满足的三角恒等式。这些图展示了伴随单元和伴随余单元之间的变换如何简化为恒等函数。在编程中，这些关系可以通过 Haskell 中的多态函数实现，从而将理论与实际代码相结合。

### -------------------------

### 15.2 从伴随构造单子（Monads from Adjunctions）

如你所见，伴随函子对 $L \dashv R$ 中的伴随单元（$\eta$）和伴随余单元（$\epsilon$）与单子的单元（$\eta$）和结合（$\mu$）有非常紧密的关系。对于每一个伴随函子对 $L \dashv R$，我们可以通过构造 $T = R \circ L$ 来定义一个单子。

#### 1. 从伴随推导出的单子

给定伴随 $L: \mathcal{D} \to \mathcal{C}$ 和 $R: \mathcal{C} \to \mathcal{D}$，我们可以构造出一个自函子 $T = R \circ L$，并将其作为单子。这里的 **单子单位** $\eta$ 和 **结合** $\mu$ 定义如下：

- **单子单位 $\eta$**：单位 $\eta : \text{Id}_\mathcal{D} \to R \circ L$。
- **单子结合 $\mu$**：结合 $\mu : T \circ T \to T$，通过伴随余单元 $\epsilon : L \circ R \to \text{Id}_\mathcal{C}$ 定义为 $\mu = R \circ \epsilon \circ L$。

可以用以下字符串图表示：

#### 2. 单子的字符串图表示

##### 2.1 单子单位的字符串图

首先，我们表示单子单位 $\eta$。在伴随函子中，$\eta$ 将恒等函子映射为 $R \circ L$，即：

```plaintext
  L   R
   \ /
    η
```

这对应于单子中的单位变换，将恒等函子注入到自函子 $T = R \circ L$ 中。

##### 2.2 单子结合的字符串图

单子的结合 $\mu = R \circ \epsilon \circ L$，其中 $\epsilon$ 是伴随的余单元。其字符串图如下：

```plaintext
  L     R
   \   /
    ε
   / \
L R L R
 \   /
  μ
```

从下到上读取该图，我们得到 $T \circ T = R \circ L \circ R \circ L \to R \circ L = T$，这是通过余单元 $\epsilon$ 将中间的 $L \circ R$ 缩约掉。

#### 3. 单子定律的字符串图

我们现在通过字符串图展示单子的 **单子定律**。单子定律包括 **单位元律** 和 **结合律**。

##### 3.1 单子单位元律

单位元律要求：

$$ \mu \circ (\eta \circ T) = \text{id}_T \quad \text{(左单位律)} $$
$$ \mu \circ (T \circ \eta) = \text{id}_T \quad \text{(右单位律)} $$

这可以通过字符串图展示为：

###### 左单位律：

```plaintext
   L     R
    \   /
     η
    / \
  L   R   L
   \   / \
    ε   R
     \   /
      μ
```

通过拉直 $\eta$ 和 $\epsilon$ 的部分，我们可以看到这个图最终简化为 $T$ 上的恒等函数。

###### 右单位律：

```plaintext
L   R
 \ / \
  L   η
 / \   R
ε   \ /
 \   /
  μ
```

通过同样的“拉直”方式，我们可以看到这条路径等价于 $T$ 上的恒等变换。

##### 3.2 单子结合律

结合律要求：

$$ \mu \circ (T \circ \mu) = \mu \circ (\mu \circ T) $$

结合律可以用字符串图表示为：

```plaintext
L   R   L   R
 \ / \ / \
  L   ε   L   R
 / \   \   / \
L   R   R   ε
 \   \ /     /
  μ   L     R
     / \
    μ   R
```

无论你如何组合两个 $T$ 函子，最终都会简化为一个 $T$ 函子，确保结合律成立。

#### 4. 总结

通过字符串图，我们可以直观地展示从伴随推导出的单子的构造，并验证单子的定律。单位元和结合变换由伴随单元 $\eta$ 和伴随余单元 $\epsilon$ 提供，并且可以通过字符串图的“拉直”操作，清楚地看到这些定律的成立。

### -------------------------

### 15.3 从伴随推导出的单子示例（Examples of Monads from Adjunctions）

在这一节中，我们通过几个实际示例展示了如何从伴随（adjunction）生成单子（monads），并讨论这些单子在编程中的实际应用。伴随函子通过自然变换定义，而这些自然变换在许多情况下可以通过组合构造出单子。我们接下来看一些常见的从伴随推导出的单子示例。

#### 1. **Reader Monad（读取器单子）**

##### 伴随关系：
- 左伴随函子 $L: \text{Set} \to \text{Set}^X$：将一个集合 $A$ 映射为从 $X$ 到 $A$ 的函数，即 $L(A) = X \to A$。
- 右伴随函子 $R: \text{Set}^X \to \text{Set}$：将一个从 $X$ 到 $A$ 的函数映射为集合 $A$，即 $R(F) = F(X)$。

##### 单子的构造：
从这个伴随关系中，我们得到的自函子 $T = R \circ L$ 对应于 **Reader Monad**。在 Haskell 中，Reader Monad 定义如下：

```haskell
newtype Reader r a = Reader { runReader :: r -> a }

instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    (Reader f) >>= g = Reader $ \r -> runReader (g (f r)) r
```

Reader Monad 表示一种从环境 $r$ 中读取的操作，单子结合律和单位律通过这个伴随关系直接导出。

#### 2. **State Monad（状态单子）**

##### 伴随关系：
- 左伴随函子 $L: \text{Set} \to \text{Set}^S$：将一个集合 $A$ 映射为从 $S$ 到 $A \times S$ 的函数，表示带状态的计算。
- 右伴随函子 $R: \text{Set}^S \to \text{Set}$：将一个带状态的计算映射为最终的状态和结果。

##### 单子的构造：
从这个伴随关系中，我们得到的自函子 $T = R \circ L$ 对应于 **State Monad**。State Monad 在 Haskell 中定义如下：

```haskell
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    (State f) >>= g = State $ \s ->
        let (a, s') = f(s)
        in runState (g a) s'
```

State Monad 通过保存和更新状态来执行带状态的计算，结合律和单位律通过伴随函子推导出。

#### 3. **List Monad（列表单子）**

##### 伴随关系：
- 左伴随函子 $L: \text{Set} \to \text{Monoid}$：将集合映射为幺半群，即将 $A$ 映射为由 $A$ 元素组成的列表 $[A]$。
- 右伴随函子 $R: \text{Monoid} \to \text{Set}$：将幺半群映射为集合。

##### 单子的构造：
从这个伴随关系中，我们得到的自函子 $T = R \circ L$ 对应于 **List Monad**。List Monad 在 Haskell 中定义如下：

```haskell
instance Monad [] where
    return x = [x]
    xs >>= f = concatMap f xs
```

List Monad 允许我们表示非确定性计算，它将每一个列表中的元素通过 $f$ 转换为新的列表，并将结果合并在一起。结合律和单位律由伴随关系自然导出。

#### 4. **Writer Monad（记录器单子）**

##### 伴随关系：
- 左伴随函子 $L: \text{Set} \to \text{Monoid}$：将集合映射为一个具有累加性质的类型（比如日志）。
- 右伴随函子 $R: \text{Monoid} \to \text{Set}$：从累加类型中提取结果。

##### 单子的构造：
从这个伴随关系中，我们得到的自函子 $T = R \circ L$ 对应于 **Writer Monad**。Writer Monad 在 Haskell 中定义如下：

```haskell
newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
    return a = Writer (a, mempty)
    (Writer (a, w)) >>= f = 
        let (Writer (b, w')) = f a
        in Writer (b, w `mappend` w')
```

Writer Monad 通过记录副作用（如日志）来执行计算，并且结合律和单位律通过伴随函子推导出。

### 总结

从伴随函子推导出单子是一个强大的工具，伴随函子提供了从两个不同范畴之间的映射关系中推导出自函子（单子）的途径。通过这些例子，我们可以看到这些单子广泛应用于编程中，包括 Reader、State、List、Writer 等单子，它们都来源于具体的伴随函子对。

### -------------------------

### 自由幺半群与列表单子（Free Monoid and the List Monad）

**自由幺半群（Free Monoid）** 和 **列表单子（List Monad）** 是在范畴论中密切相关的两个概念。自由幺半群的伴随函子生成了列表单子，提供了我们在编程中常用的列表操作。接下来我们深入讨论这个过程及其在 Haskell 中的实现。

#### 1. 自由幺半群的伴随构造

在集合范畴 $\text{Set}$ 和幺半群范畴 $\text{Monoid}$ 之间，我们有一个伴随函子对：
- **左伴随函子 $F: \text{Set} \to \text{Monoid}$**：它将一个集合 $X$ 映射为由 $X$ 的元素生成的自由幺半群 $F(X)$，即元素 $X$ 的列表。
- **右伴随函子 $U: \text{Monoid} \to \text{Set}$**：它将一个幺半群 $M$ 映射为其底层集合，忘记幺半群的乘法结构。

##### 伴随单元 $\eta_X: X \to U(F(X))$

- 伴随单元 $\eta_X$ 将集合 $X$ 的元素注入到自由幺半群 $F(X)$ 中，具体来说，将集合 $X$ 的元素映射为单元素列表。
- 在 Haskell 中，这相当于使用 `return` 函数将元素包装成单元素列表：

```haskell
return :: a -> [a]
return x = [x]
```

##### 伴随余单元 $\epsilon_M: F(U(M)) \to M$

- 伴随余单元 $\epsilon_M$ 将从自由幺半群返回到幺半群 $M$，这对应于从列表 $[a]$ 到幺半群 $M$ 的一个幺半群同态。
- 在 Haskell 中，这可以通过 `foldr` 函数实现，它将列表中的元素折叠成幺半群元素：

```haskell
epsilon :: Monoid m => [m] -> m
epsilon = foldr mappend mempty
```

- 这里的 `foldr` 函数将空列表映射到幺半群的单位元 `mempty`，将连接映射为幺半群的二元运算 `mappend`。

#### 2. 列表单子的构造

从自由幺半群伴随的自然变换可以推导出列表单子的 **单子乘法（join）**，即将嵌套列表扁平化为单一层级的列表。通过余单元 $\epsilon$ 的 **whiskering**（掺合）可以定义单子的乘法：

$$
\mu = U \circ \epsilon \circ F
$$

##### 结合律的实现：`join`

在 Haskell 中，`join` 实现为将列表的列表展平成单一列表，这可以通过 `foldr` 和列表连接 `++` 来实现：

```haskell
join :: [[a]] -> [a]
join = foldr (++) []
```

`foldr (++) []` 将嵌套的列表结构中的每一层列表都通过列表连接符 `++` 组合起来。换句话说，`join` 就是将多个列表连接在一起，类似于 Haskell 标准库中的 `concat` 函数：

```haskell
concat :: [[a]] -> [a]
concat = foldr (++) []
```

因此，在列表单子中，**单子乘法**（`join`）就是 **列表连接**，它通过连接嵌套列表的元素来扁平化结构。

#### 3. 列表单子的实现

我们可以总结列表单子的实现如下：

```haskell
instance Monad [] where
    return x = [x]  -- 将元素包装成单元素列表
    xs >>= f = concatMap f xs  -- 列表中的每个元素都映射为列表，并连接结果
```

`concatMap` 的实现与 `join` 相同，它将 `f` 应用于每个元素 `xs`，然后连接结果。

#### 4. 总结

- **自由幺半群** 是在范畴论中构造的一个基本结构，它生成了由元素组成的列表。伴随单元将元素注入到列表中，而伴随余单元则将列表折叠成一个幺半群。
- **列表单子** 是从自由幺半群伴随生成的，在编程中，它的 `return` 操作对应于将元素包装成单元素列表，而 `join` 操作则对应于列表的连接操作。
- Haskell 中的 `foldr` 和 `concatMap` 函数很好地体现了列表单子的这些性质，使得我们可以使用自由幺半群的幺半群同态来处理列表操作。

通过理解自由幺半群与列表单子的关系，我们能够更好地掌握列表在编程中的底层机制，并认识到这些概念在范畴论和编程语言中的深刻联系。

### -------------------------

### 柯里化伴随与状态单子（The Currying Adjunction and the State Monad）

状态单子（State Monad）是由 **柯里化伴随**（currying adjunction）生成的，这一伴随函子定义了指数对象。我们通过研究伴随函子对 $L_s \dashv R_s$ 来推导出状态单子的结构，并讨论它在 Haskell 中的实现。

#### 1. 柯里化伴随的定义

在 **柯里化伴随** 中，伴随函子对 $L_s \dashv R_s$ 的左伴随 $L_s$ 和右伴随 $R_s$ 定义如下：

- **左伴随函子 $L_s$:**
  $$ L_s(a) = a \times s $$
  这是与固定对象 $s$ 的乘积函子。在 Haskell 中，我们将其实现为：

  ```haskell
  newtype L s a = L (a, s)
  ```

  这个类型封装了一个带有两个分量的元组，其中第一个分量是类型 $a$，第二个分量是类型 $s$。

- **右伴随函子 $R_s$:**
  $$ R_s(c) = c^s $$
  这是指数函子，它将对象 $c$ 映射为从 $s$ 到 $c$ 的函数。在 Haskell 中，它是一个函数类型的封装：

  ```haskell
  newtype R s c = R (s -> c)
  ```

#### 2. 状态单子的构造

我们可以通过将这两个伴随函子组合起来构造一个自函子 $T = R_s \circ L_s$。这个组合对应于 **状态单子** 的结构。在 Haskell 中，我们会写成：

```haskell
newtype St s a = St (R s (L s a))
```

展开后我们得到：

```haskell
newtype State s a = State (s -> (a, s))
```

这就是 Haskell 中的状态单子，它表示从状态 $s$ 到新状态和结果 $(a, s)$ 的计算。

#### 3. 伴随单元和余单元

##### 单元（unit）

伴随函子 $L_s \dashv R_s$ 的单元 $\eta$ 是一个从对象 $a$ 到 $R_s(L_s(a))$ 的映射，它将对象 $a$ 映射为从状态 $s$ 到 $(a, s)$ 的函数。这在 Haskell 中实现为状态单子的 `return`：

```haskell
unit :: a -> R s (L s a)
unit a = R (\s -> L (a, s))
```

这等价于状态单子的 `return` 函数：

```haskell
return :: a -> State s a
return a = State (\s -> (a, s))
```

##### 余单元（counit）

伴随函子 $L_s \dashv R_s$ 的余单元 $\epsilon$ 是一个从 $R_s(L_s(c))$ 到 $c$ 的映射。在 Haskell 中，它表示为：

```haskell
counit :: L s (R s a) -> a
counit (L (R f, s)) = f s
```

这剥离了数据构造器，返回函数 $f$ 作用在 $s$ 上的结果。这等价于 `runState` 的非柯里化版本，它提取状态单子中的计算结果：

```haskell
runState :: State s a -> s -> (a, s)
runState (State h) s = h s
```

#### 4. 单子乘法（join）

单子乘法 $\mu$ 是由余单元的 **whiskering**（掺合）给出的，在 Haskell 中可以表示为：

```haskell
mu :: R s (L s (R s (L s a))) -> R s (L s a)
mu = fmap counit
```

右边的 **whiskering** 由类型推断引擎自动处理，而左边的 **whiskering** 是通过提升自然变换的分量来完成的。在状态单子中，这对应于 `join` 的实现：

```haskell
join :: State s (State s a) -> State s a
join mma = State (fmap (uncurry runState) (runState mma))
```

`runState` 的双重使用展现了与余单元 `counit` 的类似性：

```haskell
runState :: State s a -> s -> (a, s)
runState (State h) s = h s
```

当它被非柯里化时，类型签名变为：

```haskell
uncurry runState :: (State s a, s) -> (a, s)
```

这与 `counit` 的类型签名相同。

#### 5. 总结

通过柯里化伴随 $L_s \dashv R_s$，我们推导出了状态单子。伴随函子的单元 $\eta$ 对应于状态单子的 `return`，余单元 $\epsilon$ 对应于 `runState`。单子乘法 `join` 则是通过余单元的 **whiskering** 来定义的。在编程中，状态单子广泛用于管理状态的计算，并且其底层结构来源于柯里化伴随这一数学概念。


### -------------------------

### M-集与 Writer 单子（M-sets and the Writer Monad）

#### 1. M-集的定义

**M-集**是一个集合 $S$，并且在集合 $S$ 上定义了幺半群 $M$ 的作用。这种作用是一个映射：

$$ a: M \times S \to S $$

我们通常使用作用的柯里化版本，表示为 $a_m: S \to S$，其中 $m \in M$ 是幺半群的元素，$a_m(s)$ 表示将幺半群元素 $m$ 作用到集合元素 $s$ 上。

这个作用必须满足以下条件：

1. **幺半群单位的作用**：幺半群的单位 $1$ 不能改变集合中的元素，因此：

   $$ a_1 = \text{id}_S $$

2. **作用的组合**：幺半群元素的连续作用必须等同于幺半群乘法的作用：

   $$ a_{m_1} \circ a_{m_2} = a_{m_1 \cdot m_2} $$

这些条件定义了所谓的 **左作用**，如果我们交换两个幺半群元素的顺序，则得到 **右作用**。

#### 2. M-集范畴与函子

M-集的集合与保持幺半群作用的映射构成一个范畴，称为 **MSet** 范畴。MSet 中的对象是对 $(S, a: M \times S \to S)$，箭头是保持作用的协变映射。协变映射的定义如下：

一个函数 $f: S \to R$ 是从 $(S, a)$ 到 $(R, b)$ 的协变映射，如果对于每个 $m \in M$，以下图表交换：

$$
\begin{aligned}
S &\xrightarrow{f} R \\
a_m \downarrow &\quad \downarrow b_m \\
S &\xrightarrow{f} R
\end{aligned}
$$

这意味着先应用幺半群的作用 $a_m$ 然后映射到 $R$，等价于先映射到 $R$，再应用 $b_m$ 的作用。

从 **MSet** 到 **Set** 有一个 **遗忘函子** $U$，它将一个 M-集 $(S, a)$ 映射为集合 $S$，忽略作用。与此对应的是一个 **自由函子** $F$，它在集合 $S$ 上产生一个 M-集，定义为：

$$ F(S) = M \times S $$

自由作用由以下规则定义：

$$ \varphi_n: (x, m) \mapsto (x, n \cdot m) $$

这意味着元素 $x$ 不变，$m$ 分量被乘以幺半群元素 $n$。

#### 3. 伴随函子与单子的构造

自由函子 $F$ 和遗忘函子 $U$ 构成了一个伴随函子对，其中 $F \dashv U$。该伴随函子对的单元 $\eta_S: S \to U(F(S))$ 将集合 $S$ 中的元素映射为 M-集中的形式 $(x, 1)$，即幺半群单位的作用。这与 Haskell 中 Writer 单子的 `return` 非常类似：

```haskell
return a = Writer (a, mempty)
```

**余单元** $\epsilon_Q: F(U(Q)) \to Q$，将 M-集 $Q$ 的底层集合与幺半群 $M$ 的底层集合取积，然后将作用从自由作用恢复到 $Q$ 中的作用。余单元的定义为：

$$ \epsilon_Q: (x, m) \mapsto a_m(x) $$

其中 $x$ 是集合 $Q$ 中的元素，$a_m$ 是在 $Q$ 上定义的幺半群作用。这与 Writer 单子的累积操作非常类似。

#### 4. Writer 单子的实现

通过伴随函子对 $F \dashv U$，我们可以推导出 Writer 单子的定义：

1. **单子单元** $\eta_S: S \to U(F(S))$ 对应于 Writer 单子的 `return`：

   ```haskell
   return a = Writer (a, mempty)
   ```

2. **单子乘法** $\mu$ 是通过余单元的 **whiskering** 得到的，其作用是将 $M$ 作用到 M-集的所有元素上，并累积幺半群的作用。这对应于 Writer 单子的 `join` 实现：

   ```haskell
   join :: Monoid m => Writer m (Writer m a) -> Writer m a
   join (Writer (Writer (x, m), n)) = Writer (x, mappend n m)
   ```

#### 5. 总结

通过将幺半群的作用表示为 M-集，我们可以理解 **Writer 单子** 的结构。伴随函子对 $F \dashv U$ 提供了构建 Writer 单子的数学基础，其中 **单元** 对应于 `return`，**余单元** 对应于日志累积的操作，单子的 **join** 操作通过自由幺半群的组合实现。这一过程揭示了 Writer 单子的数学背景，并展示了其与 M-集的密切关系。

### -------------------------

要证明 $U \circ F$ 是 Maybe 单子，我们首先需要了解有点对象（pointed objects）范畴以及伴随函子的构造，接着分析这个伴随构造如何与 Maybe 单子相对应。

### 有点对象范畴

在范畴论中，有点对象范畴 $1/\mathcal{C}$ 是指那些带有指定元素的对象。形式上，一个有点对象是一个二元组 $(a, p: 1 \to a)$，其中 $a$ 是 $\mathcal{C}$ 中的对象，$p$ 是从终端对象 $1$ 到 $a$ 的箭头。这意味着我们从终端对象 $1$ 中选择了 $a$ 中的某个特定元素。

在有点对象范畴中，从一个有点对象 $(a, p: 1 \to a)$ 到另一个有点对象 $(b, q: 1 \to b)$ 的态射是保持这个点的箭头 $f: a \to b$，使得以下图表交换：

$$
\begin{aligned}
1 &\xrightarrow{p} a \\
&\quad \downarrow f \\
1 &\xrightarrow{q} b
\end{aligned}
$$

这个条件保证了 $f$ 保持点，即 $q = f \circ p$。

### 遗忘函子 $U$ 和自由函子 $F$

有一个遗忘函子 $U: 1/\mathcal{C} \to \mathcal{C}$，它会遗忘点，仅返回对象 $a$。这个函子从有点对象 $(a, p: 1 \to a)$ 中抽取出 $a$，忽略从终端对象 $1$ 的箭头 $p$。

自由函子 $F: \mathcal{C} \to 1/\mathcal{C}$ 则为对象 $a$ 自由地添加一个点。换句话说，$F$ 将对象 $a$ 映射为二元组 $(1 + a, \text{Left})$，其中 $1 + a$ 是 $a$ 与终端对象 $1$ 的余积，而 $\text{Left}$ 是 $1 \to 1 + a$ 中将 $1$ 映射为 $1 + a$ 中的左侧分量的箭头。

### Maybe 单子的构造

我们现在来看如何通过这个伴随对 $F \dashv U$ 构造 Maybe 单子。要构造一个单子，我们需要定义：

1. **函子**：定义一个自函子 $T = U \circ F$。
2. **单子单元**：定义一个自然变换 $\eta: \text{id}_{\mathcal{C}} \to T$。
3. **单子乘法**：定义一个自然变换 $\mu: T \circ T \to T$。

#### 1. 构造函子 $T = U \circ F$

对于任意对象 $a \in \mathcal{C}$，自由函子 $F$ 将其映射为 $(1 + a, \text{Left})$，也就是说，它将 $a$ 转换为带有一个点的对象 $1 + a$。然后，遗忘函子 $U$ 从中丢弃点，仅返回 $1 + a$。因此，$T a = U(F(a)) = 1 + a$。

在 Haskell 中，这正是 `Maybe` 类型的构造：`Maybe a` 要么是 `Nothing`（对应于 $1$），要么是 `Just a`（对应于 $a$）。

#### 2. 单子单元 $\eta$

自然变换 $\eta: \text{id}_{\mathcal{C}} \to T$ 将对象 $a$ 映射到 $1 + a$。它为每个对象 $a$ 生成一个箭头 $\eta_a: a \to 1 + a$，在 Haskell 中可以表示为：

```haskell
return a = Just a
```

$\eta_a$ 将对象 $a$ 的每个元素映射为 $1 + a$ 中的右侧分量，即 `Just a`。

#### 3. 单子乘法 $\mu$

自然变换 $\mu: T \circ T \to T$ 将 $T(T(a)) = 1 + (1 + a)$ 折叠为 $1 + a$。在 Haskell 中，这就是 `join` 的定义，它消除了两层 `Maybe` 嵌套：

```haskell
join :: Maybe (Maybe a) -> Maybe a
join (Just (Just a)) = Just a
join (Just Nothing)  = Nothing
join Nothing         = Nothing
```

$\mu$ 将 `Just (Just a)` 映射为 `Just a`，将 `Just Nothing` 映射为 `Nothing`，将 `Nothing` 映射为 `Nothing`。

### 结论

通过定义 $T = U \circ F$，我们得到了一个自函子，它将对象 $a$ 映射为 $1 + a$，并且构造了 Maybe 单子的所有组成部分：`return` 和 `join`。因此，$U \circ F$ 正是 Maybe 单子。

### -------------------------

续延单子（Continuation Monad）源于范畴论中的一对反变函子。让我们一步步来分析续延单子的构造，并将其与范畴论的伴随函子联系起来。

### 续延单子的定义

续延单子的定义通常写为：

$$
\text{Cont r a} = (a \to r) \to r
$$

这表示一个类型为 `Cont r a` 的值是一个接受类型为 $a \to r$ 的函数并返回类型为 $r$ 的值的函数。可以把它看作是一个从未来需要的继续计算中返回结果的机制。

在范畴论中，我们可以通过伴随函子的形式构造续延单子。

### 反变函子和对偶范畴

在范畴论中，我们考虑集合范畴 $\text{Set}$ 及其对偶 $\text{Set}^\text{op}$。为了定义续延单子，我们使用两对函子：

1. **左函子** $L_Z : \text{Set}^\text{op} \to \text{Set}$，它将一个集合 $X$ 映射到从 $X$ 到 $Z$ 的集合的同态集合，即：
   
   $$
   L_Z(X) = \text{Set}(X, Z)
   $$

   这个函子将集合 $X$ 映射到所有从 $X$ 到 $Z$ 的函数集合，$Z$ 是固定的参数化集合。

2. **右函子** $R_Z : \text{Set} \to \text{Set}^\text{op}$，它将一个集合 $X$ 映射到从 $Z$ 到 $X$ 的集合的同态集合，即：
   
   $$
   R_Z(X) = \text{Set}(X, Z)
   $$

   注意这里的同态集合是从 $Z$ 到 $X$ 的函数，因为它是反变函子。由于 $\text{Set}^\text{op}$ 中的态射方向是反的，结果与前面的左函子的形式看似相同。

### 函子的组合与续延单子

当我们组合这两个函子时，得到的就是续延单子的结构：

$$
R_Z \circ L_Z(X) = (X \to Z) \to Z
$$

在 Haskell 中，组合后的形式写为：

```haskell
type Cont r a = (a -> r) -> r
```

这就是续延单子的定义：一个接受类型为 $a \to r$ 的函数并返回 $r$ 类型值的函数。这种组合方式非常直观，因为我们是在对集合 $X$ 应用 $X \to Z$ 的同态函数，再通过另一个同态函数从中提取结果。

### 单子结构

为了完整地构造一个单子，我们需要定义 `return` 和 `bind` 函数：

1. **`return`** 将一个值包装到续延计算中，它可以这样实现：

```haskell
return a = \k -> k a
```

它接受一个值 $a$，并返回一个接受函数 $k$ 的函数，直接应用 $k$ 到 $a$。

2. **`bind`** 将两个续延计算组合在一起，它可以这样实现：

```haskell
m >>= k = \c -> m (\a -> k a c)
```

它接受一个续延计算 $m$，将其结果应用到另一个续延计算 $k$，最后将其结果应用到函数 $c$ 上。

### 范畴论解释

在范畴论中，续延单子通过反变函子定义，但这并不会改变伴随函子的定义。我们通过对偶范畴 $\text{Set}^\text{op}$ 处理反变的结构。最终，我们得到的组合函子 $R_Z \circ L_Z$ 表示的是一种接受函数作为输入，并返回结果的函数。这正是续延单子的本质——它将计算过程延续（continuation），并允许在适当的时机应用这些延续。

### 总结

续延单子是从集合范畴中的一对反变函子的组合构造的。通过在对偶范畴中处理反变性，函子的组合与续延单子的定义自然吻合。

### -------------------------

Monad 变换器（Monad Transformers）提供了一种方法来组合不同的 monad 效果，例如状态（state）和可能的失败（failure）。通过这种方式，我们可以在不从头定义新 monad 的情况下，将多个 monad 结合使用。

### 从组合 monad 开始

假设我们想要组合两个效果：**状态** 和 **可能的失败**。一种方式是从头定义自己的 monad：

```haskell
newtype MaybeState s a = MS (s -> Maybe (a, s))
  deriving Functor
```

这个 `MaybeState` monad 表示一个可能失败的状态计算。我们还需要定义一个函数来运行这种 monad：

```haskell
runMaybeState :: MaybeState s a -> s -> Maybe (a, s)
runMaybeState (MS h) s = h s
```

接着，我们为它定义一个 `Monad` 实例：

```haskell
instance Monad (MaybeState s) where
  return a = MS (\s -> Just (a, s))
  ms >>= k = MS (\s -> case runMaybeState ms s of
                         Nothing -> Nothing
                         Just (a, s') -> runMaybeState (k a) s')
```

### Monad 变换器的想法

这种方法虽然可以，但每次要组合不同的 monad 效果时都需要定义新的 monad，显得十分繁琐。而 **monad 变换器** 提供了一种机制，可以通过已有的 monad 来构造新的 monad。这种机制来自于伴随函子的组合。

在范畴论中，伴随函子是可组合的，因此，通过伴随函子生成的 monad 也可以组合。我们可以通过复合伴随函子构造 **monad 变换器**，它能够将一个 monad 转换为一个新的 monad。

### 内在 monad 和外在 monad

考虑两个伴随函子 $L$ 和 $R$，它们分别从一个范畴映射到另一个范畴。如果我们有两个伴随函子 $L_s ⊣ R_s$ 和 $L \subset R$，它们可以生成两个 monad：

- **内在 monad** $T = R_s \circ L_s$
- **外在 monad** $M = R \circ L$

我们可以通过复合这两个 monad 来生成一个新的 monad，即 **monad 变换器**，如下：

$$
M' = R \circ T \circ L
$$

在 Haskell 中，类似的情况可以通过 **Maybe** monad 和 **State** monad 的组合来表示。假设我们将 `Maybe` 视为内在的 monad，而状态 monad 是通过外在的伴随函子定义的：

```haskell
s -> Maybe (a, s)
```

这个复合 monad 就是 `MaybeState` 的定义。

### Monad 变换器的组合

为了使 monad 变换器工作，我们需要为它定义 **单位元** 和 **乘法**，即为复合 monad 定义 `return` 和 `bind`。

- **单位元（unit）** $\eta : \text{Id} \to R \circ T \circ L$，通过以下字符串图表示：

  $$
  L \quad T \quad R
  $$

- **乘法（multiplication）** $\mu : R \circ T \circ L \circ R \circ T \circ L \to R \circ T \circ L$，通过以下字符串图表示：

  $$
  L \quad T \quad R \quad L \quad T \quad R
  $$

这些结构保证 monad 变换器能够满足 monad 的结合律和单位律。

### 在 Haskell 中的应用

在 Haskell 中，我们可以通过 `MonadTrans` 类型类来定义 monad 变换器。以下是 `StateT` 变换器的示例：

```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  (StateT h) >>= k = StateT $ \s -> do
    (a, s') <- h s
    runStateT (k a) s'
```

在这个例子中，`StateT` 是将状态 monad 和任意 monad `m` 组合的 monad 变换器。它允许我们在 monad 中引入状态效果，而无需重新定义整个 monad。

### 总结

Monad 变换器是一种将多个效果组合到同一 monad 中的机制。通过结合伴随函子的构造，我们能够定义 monad 变换器，并且它们自动遵循 monad 定律。这种方法大大简化了在编程中处理复杂效果组合的工作。

### -------------------------

### State Monad Transformer 详解

**State Monad Transformer** (`StateT`) 是一种 monad 变换器，它允许我们将状态计算与其他 monad 结合起来，以便在 monadic 计算中处理状态。`StateT` 提供了一种机制，可以将状态传递给任意的 monad，并保持 monad 的特性，如 `Maybe`、`IO` 等。

#### 1. **柯里化伴随与 State Monad**

`StateT` 来自 **柯里化伴随** (currying adjunction)。左函子 \( L_s \) 表示乘积函子，它将类型 \( a \) 转换为 \( (a, s) \)，即一个带有状态 \( s \) 的元组。右函子 \( R_s \) 表示指数函子 (exponential functor)，它将 \( c \) 转换为 \( s \to c \)，这也被称为 **读者函子** (Reader functor)。

在 Haskell 中，`StateT` 的定义如下：

```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
```

- 这里，`StateT` 是一个带有状态 \( s \) 的 monad 变换器，它与任意的 monad \( m \) 结合使用。
- `runStateT` 是用于运行状态 monad 的函数，它接受一个初始状态 \( s \) 并返回一个 monadic 结果 \( m (a, s) \)。

#### 2. **单位元 (unit)**

`StateT` 的 `return` 函数是单位元 \( \eta \)，它表示将一个值引入到状态计算中，而不会改变状态。公式为：

$$
\eta_a = R(\eta_i L_a) \circ \eta_o
$$

在 Haskell 中，`return` 的实现如下：

```haskell
return x = StateT (\s -> return (x, s))
```

- `return x` 创建了一个新的状态计算，该计算接收初始状态 \( s \)，并将其作为不变的状态返回，同时输出值 \( x \)。
- 公式中的 \( \eta_i \) 是内在 monad 的 `return`，将值封装到 monad 中；而 \( \eta_o \) 是柯里化构造函数 `unit`，即状态不会改变。

#### 3. **乘法 (multiplication)**

`StateT` 的 `bind` 函数通过 `join` 实现，它表示将两个嵌套的状态计算组合为一个单一的状态计算。公式为：

$$
\mu_a = R(\mu_i L_a) \circ (R \circ T)(\epsilon_o (T \circ L)_a)
$$

在 Haskell 中，`join` 的实现如下：

```haskell
join :: StateT s m (StateT s m a) -> StateT s m a
join mma = StateT (join . fmap (uncurry runStateT) . runStateT mma)
```

- `join mma` 接受一个嵌套的状态计算，将其展平为单一的计算。
- `runStateT mma` 将数据构造函数 `StateT` 剥离出来，得到函数类型 \( s \to m (StateT s m a, s) \)。
- `uncurry runStateT` 将结果从嵌套状态中解包，并合并为单一状态。

#### 4. **使用 `StateT` 结合其他 Monad**

通过 `StateT`，我们可以将状态计算与其他 monad 结合。例如，将状态与 `Maybe` monad 结合，可以处理带有失败可能性的状态计算：

```haskell
type MaybeState s a = StateT s Maybe a
```

在这种情况下，`MaybeState` 类型表示一个带有状态的可能失败的计算。我们可以使用 `runStateT` 来运行该计算：

```haskell
runMaybeState :: MaybeState s a -> s -> Maybe (a, s)
runMaybeState = runStateT
```

#### 5. **恢复普通的 State Monad**

我们可以通过将 `StateT` 应用于 `Identity` 函子来恢复普通的 `State` monad。`Identity` 函子在 Haskell 中定义为：

```haskell
newtype Identity a = Identity { runIdentity :: a }
instance Monad Identity where
  return = Identity
  (Identity a) >>= f = f a
```

普通的 `State` monad 可以定义为 `StateT` 应用于 `Identity` 函子：

```haskell
type State s a = StateT s Identity a
```

这意味着 `State s a` 是 `StateT s Identity` 的一种特例，它只处理状态，不涉及其他 monadic 效果。

### 结论

**StateT** 是一种强大的工具，它允许我们将状态计算与任意 monad 结合。通过使用 **monad 变换器**，我们可以灵活地处理组合多个 monad 的需求，而不需要从头定义新的 monad。`StateT` 提供了一个非常通用的方式来管理状态，并与其他效果（如 `Maybe`、`IO` 等）无缝结合。

### -------------------------

### Monad 代数详解

**Monad 代数** 是理解 monad 的一个关键概念，它不仅仅是数学理论中的一种结构，也为我们在编程中如何使用 monad 提供了进一步的指导。

### 1. **Monad 和代数的关系**

每个 monad 可以看作是生成表达式的工具，而代数则用于评估这些表达式。一个 monad 定义了如何组合计算，而代数则提供了一种将计算应用到某个具体类型并返回结果的方法。

在编程中，monad 的**单位元**和**乘法**为我们提供了从值生成 monadic 表达式以及组合 monadic 表达式的方法，而代数提供了评估这些表达式的规则。

#### 单位元与代数
假设我们有一个 monad \( T \)，并且我们正在寻找它的代数 \( (\alpha: T a \to a) \)，其中 \( a \) 是代数的载体类型。代数 \( \alpha \) 应该与 monad 的单位元 \( \eta \) 兼容，也就是说，它应该满足以下条件：

$$
\alpha \circ \eta_a = \text{id}_a
$$

这个等式的意思是：将值 \( a \) 封装到 monad \( T a \) 中，再通过代数 \( \alpha \) 评估它时，应该得到原来的值 \( a \)。

#### 乘法与代数
monad 的乘法 \( \mu \) 用于将嵌套的 monadic 表达式 \( T (T a) \) 扁平化为 \( T a \)。对于代数来说，也有类似的要求，即代数应该能够直接评估嵌套的表达式：

$$
\alpha \circ \mu_a = \alpha \circ T \alpha
$$

这里的意思是：我们可以先用 \( T \alpha \) 对每个 monadic 表达式进行代数映射，然后再通过 \( \alpha \) 扁平化结果，或者直接扁平化嵌套的表达式再应用 \( \alpha \)，两者的结果应该是相同的。

### 2. **Monad 代数的定义**

**Monad 代数** 是配备了一个箭头 \( \alpha: T a \to a \) 的对象 \( a \)，它满足以下两个交换图：

#### 单位定律
$$
\alpha \circ \eta_a = \text{id}_a
$$

这个定律要求代数的结构映射 \( \alpha \) 与 monad 的单位元 \( \eta \) 兼容。也就是说，代数应该对 monadic 单元没有任何作用。

#### 乘法定律
$$
\alpha \circ \mu_a = \alpha \circ T \alpha
$$

这个定律要求代数结构映射 \( \alpha \) 对 monad 的嵌套表达式处理是一致的。

这些图表确保了 monad 的 algebra 可以正确处理通过 monad 生成的表达式，并且这种处理方式与 monad 的单位元和乘法操作兼容。

### 3. **Monad 代数的态射**

在范畴论中，**代数态射**是代数之间的箭头，它保持代数的结构。对于 monad \( T \) 的两个代数 \( (\alpha: T a \to a) \) 和 \( (\beta: T b \to b) \)，代数态射是满足以下交换图的箭头 \( f: a \to b \)：

$$
\beta \circ T f = f \circ \alpha
$$

这意味着 \( f \) 不仅是 \( a \) 到 \( b \) 的一个映射，它还必须保持 monad 的结构，换句话说，\( f \) 必须与 monad 的作用方式兼容。

### 4. **Monad 的代数结构与因式分解**

寻找 monad 的代数也可以帮助我们将 monad 表示为两个函子的组合，即 \( T = R \circ L \)。其中 \( R \) 和 \( L \) 是从某个中间范畴到原范畴的两个函子。通过研究 monad 的代数结构，我们可以确定适当的中间范畴，并找到生成这个 monad 的伴随函子。

在编程中，我们可以通过代数的角度来看待 monad 的使用场景。例如，在 `Maybe` monad 中，代数可以表示为如何处理 `Just` 和 `Nothing` 两种不同的情况；在 `State` monad 中，代数则可以表示为如何处理状态的更新和传递。

### 结论

**Monad 代数** 为我们提供了一种理解和操作 monad 的结构化方式。通过代数，我们不仅能够评估由 monad 生成的表达式，还可以确保这种评估方式与 monad 的单位元和乘法操作兼容。此外，代数的态射确保了代数之间的映射也保持了 monad 的结构。

在编程中，理解 monad 代数有助于我们在设计和使用 monad 时确保其行为的一致性，并为 monad 的组合和变换提供理论支持。

### -------------------------

### Eilenberg-Moore 范畴详解

**Eilenberg-Moore 范畴** 是给定 monad 的代数范畴。该范畴提供了一个框架，帮助我们将 monad \( T \) 分解为伴随函子的组合，从而揭示 monad 的结构。这种范畴通常记作 \( \mathcal{C}_T \)，其中 \( \mathcal{C} \) 是我们研究的范畴，而 \( T \) 是范畴 \( \mathcal{C} \) 上的 monad。

### 1. **Eilenberg-Moore 范畴的定义**

在 \( \mathcal{C} \) 上的 monad \( T \) 的 Eilenberg-Moore 范畴 \( \mathcal{C}_T \) 的对象是 **monad 代数**。每个 monad 代数是一个配对 \( (a, \alpha: T a \to a) \)，其中：
- \( a \) 是 \( \mathcal{C} \) 中的对象。
- \( \alpha \) 是一个从 \( T a \) 到 \( a \) 的箭头，满足两个条件：
    - **单位律**： \( \alpha \circ \eta_a = \text{id}_a \)。
    - **结合律**： \( \alpha \circ \mu_a = \alpha \circ T \alpha \)，其中 \( \mu \) 是 monad 的乘法。

### 2. **Eilenberg-Moore 范畴中的态射**

在 \( \mathcal{C}_T \) 中，两个代数 \( (a, \alpha) \) 和 \( (b, \beta) \) 之间的态射是满足如下条件的箭头 \( f: a \to b \)：
- \( \beta \circ T f = f \circ \alpha \)

也就是说，箭头 \( f \) 保持 monad 的结构，即它在两个代数之间是兼容的。

### 3. **自由函子和遗忘函子**

为了构建与 monad 对应的伴随关系，我们引入两个函子：
1. **遗忘函子 \( U_T \)**：从 Eilenberg-Moore 范畴 \( \mathcal{C}_T \) 到原始范畴 \( \mathcal{C} \)。它将代数 \( (a, \alpha) \) 映射到它的载体 \( a \)，并将代数态射视为载体之间的常规态射。
2. **自由函子 \( F_T \)**：从 \( \mathcal{C} \) 到 \( \mathcal{C}_T \)。它将 \( \mathcal{C} \) 中的对象 \( a \) 映射为自由 monad 代数 \( (T a, \mu_a) \)，其中 \( \mu_a \) 是 monad 的乘法。

通过定义这两个函子，我们可以形成伴随关系：
$$
F_T \dashv U_T
$$
即，\( F_T \) 是 \( U_T \) 的左伴随函子。

### 4. **单位与余单位**

在伴随函子中，我们需要定义**单位**和**余单位**。对于这个伴随关系：
- **单位元 \( \eta \)**：这是 monad \( T \) 的单位元，自然变换 \( \eta_a: a \to U_T(F_T a) \)，它将对象 \( a \) 映射到自由代数 \( (T a, \mu_a) \) 中。
- **余单位元 \( \varepsilon \)**：这是一个自然变换 \( \varepsilon: F_T(U_T(a, \alpha)) \to (a, \alpha) \)。对于代数 \( (a, \alpha) \)，余单位元就是 \( \alpha \) 本身，因为 \( \alpha \) 是从 \( T a \) 到 \( a \) 的 monad 代数态射。

这两个自然变换满足伴随函子的**三角恒等式**，从而建立了伴随关系的完备性。

### 5. **从伴随函子生成 Monad**

对于每个伴随函子对 \( F_T \dashv U_T \)，我们可以生成一个 monad，其定义如下：
- **自函子**：\( U_T \circ F_T \)，即将 \( a \) 映射到 \( T a \)。
- **单位元**：\( \eta \) 是这个组合的单位元。
- **乘法**：通过 \( U_T \circ \varepsilon \circ F_T \) 定义的自然变换。

可以证明这个 monad 就是我们最初的 monad \( T \)。在对象上，复合 \( U_T(F_T a) \) 恰好是 \( T a \)，而在箭头上，它对应于 monad \( T \) 在箭头上的提升。

### 6. **总结**

Eilenberg-Moore 范畴为我们提供了一个分解 monad 的方法，通过将 monad 表示为两个函子的组合 \( T = R \circ L \)。这种伴随关系可以从 monad 代数中构建，证明了每个 monad 都可以通过伴随函子的组合来生成。

这种结构化的方法有助于我们更好地理解 monad 的行为，并为我们在编程中设计复杂的 monad 提供了理论支持。

### -------------------------

### Kleisli 范畴详解

Kleisli 范畴是与 monad 密切相关的一个结构，它从另一个角度为我们理解 monad 提供了帮助。在这个范畴中，箭头不仅仅是普通的函子，而是与 monad 相联系的特殊箭头，也就是 **Kleisli 箭头**。在 Eilenberg-Moore 范畴中，我们研究 monad 代数，而在 Kleisli 范畴中，我们则更关注如何将 monad 视为一种计算序列的方式。

### 1. **Kleisli 范畴的定义**

给定一个范畴 \( \mathcal{C} \) 和一个 monad \( T \) 在这个范畴上，**Kleisli 范畴** \( \mathcal{C}_T \) 的定义如下：
- **对象**：Kleisli 范畴 \( \mathcal{C}_T \) 的对象与 \( \mathcal{C} \) 中的对象相同。
- **箭头**：从对象 \( a \) 到对象 \( b \) 的箭头是 \( \mathcal{C} \) 中的箭头 \( f: a \to T b \)，即从对象 \( a \) 到对象 \( b \) 的 Kleisli 箭头是从 \( a \) 到 \( T b \) 的普通箭头。

换句话说，Kleisli 范畴中的箭头是我们之前见过的 "Kleisli 箭头"（形式为 \( a \to m b \)），这些箭头与 monad \( T \) 的结构相吻合。

#### **Kleisli 箭头的组合**

Kleisli 箭头可以通过 "鱼操作符" \( <=< \) 来组合。对于两个 Kleisli 箭头 \( f: a \to T b \) 和 \( g: b \to T c \)，它们的组合定义为：
$$
g <=< f = \mu_c \circ T g \circ f
$$
其中 \( \mu_c: T(T c) \to T c \) 是 monad 的乘法，表示将两个嵌套的 monad 进行扁平化。

### 2. **自由函子与 Kleisli 范畴**

Kleisli 范畴是从 monad \( T \) 生成的自由函子的像。为了更清楚地理解这一点，我们定义两个重要的函子：

#### **左函子 \( L_T \)**：自由函子
- 对象上，它将 \( \mathcal{C} \) 中的对象映射到 Kleisli 范畴中的相同对象。
- 箭头上，它将 \( \mathcal{C} \) 中的普通箭头 \( f: a \to b \) 映射为 Kleisli 箭头 \( L_T f: a \to T b \)，具体地定义为复合 \( \eta_b \circ f \)，其中 \( \eta_b \) 是 monad 的单位元。

#### **右函子 \( R_T \)**：遗忘函子
- 对象上，它将 Kleisli 范畴中的对象映射回 \( \mathcal{C} \) 中的对象 \( T a \)。
- 箭头上，给定一个 Kleisli 箭头 \( g: a \to T b \)，\( R_T \) 将它映射为 \( \mu_b \circ T g: T a \to T b \)，即通过 monad 的乘法 \( \mu_b \) 将箭头进行组合。

### 3. **伴随关系**

我们有以下的伴随关系：
$$
L_T \dashv R_T
$$
即 \( L_T \) 是 \( R_T \) 的左伴随函子。这个伴随关系表明，Kleisli 范畴可以看作是通过自由构造获得的。

#### **伴随关系的单位与余单位**

- **单位元** \( \eta \)：这是 monad \( T \) 的单位元，它从 \( a \) 到 \( R_T(L_T a) \) 的箭头。
- **余单位元** \( \varepsilon \)：这是 monad \( T \) 的余单位元，它将 \( L_T \circ R_T \) 复合产生的箭头映射回原来的箭头。

### 4. **Kleisli 范畴的组合**

在 Kleisli 范畴中，箭头的组合基于 monad 的结构。具体来说，给定两个箭头 \( f: a \to T b \) 和 \( g: b \to T c \)，它们在 Kleisli 范畴中的组合定义为：
$$
g \circ_T f = \mu_c \circ T(g) \circ f
$$
这意味着我们首先使用 \( f \) 生成 \( T b \) 的结果，然后使用 \( g \) 处理结果，最后用 \( \mu_c \) 进行扁平化。

### 5. **Kleisli 范畴与 Eilenberg-Moore 范畴的关系**

尽管 Eilenberg-Moore 范畴 \( \mathcal{C}_T \) 更加丰富，Kleisli 范畴 \( \mathcal{C}_T \) 则提供了一个更直接的方式来处理 monad 的计算。在两者之间，Kleisli 范畴是更小的范畴，但它捕捉了 monad 作为序列计算的一部分。这两个范畴为我们理解 monad 提供了互补的视角。

Kleisli 伴随关系是所有生成给定 monad 的伴随关系中最初的对象，而 Eilenberg-Moore 伴随关系是终极的对象，这意味着 Kleisli 范畴提供了 monad 的一个更初级的形式，而 Eilenberg-Moore 范畴则更加完整。

### 6. **总结**

Kleisli 范畴为我们提供了一种方式来处理 monad 的计算，它通过 "Kleisli 箭头" 来表示函数式编程中的效果。通过 Kleisli 范畴，我们可以看到 monad 如何处理计算的序列化，而这些计算通过 monad 的结构规则来组合。这种范畴的结构帮助我们理解 monad 的内在运作，特别是在函数式编程中，它是理解 monad 工作方式的基础。


### -------------------------

