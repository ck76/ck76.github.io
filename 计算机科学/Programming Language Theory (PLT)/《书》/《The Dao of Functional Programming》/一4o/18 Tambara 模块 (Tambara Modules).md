[toc]

### -------------------------

### 第十八章 Tambara 模块 (Tambara Modules)

Tambara 模块是范畴论中的一个不太知名但正在崛起的概念，尤其是在编程领域中的 **profunctor optics**（泛函光学）中得到了新的关注。这种数学结构为组合光学系统提供了一种简洁而有效的方式。

在编程中，光学（optics）指的是一类数据结构，用来描述和处理复杂数据类型的某些部分，例如透镜（lens）、棱镜（prism）、折射（affine）、聚合（fold）等。这些光学结构的组合往往是一个挑战，特别是在 `setter` 操作的组合时。使用 Tambara 模块，这种组合问题可以简化为普通的函数组合。

### 1. 光学的组合挑战

我们先从透镜的组合问题开始。透镜是一种最基本的光学工具，它允许我们从一个复合数据结构中提取（`get`）一个部分，然后修改（`set`）这个部分并返回修改后的数据结构。

* `getter` 的组合非常简单。比如你可以通过函数组合轻松地将多个 `getter` 组合起来：从一个对象中提取数据，然后进一步从提取的结果中提取。

* `setter` 的组合要复杂得多。`setter` 涉及到改变数据结构的某个部分，同时确保该部分的修改正确影响整个数据结构。比如当你对数据的嵌套层次进行修改时，如何保证修改的正确性和一致性是一个大问题。

#### 示例：透镜组合中的挑战

想象你有一个透镜结构，它可以从对象中提取出一部分数据，称为焦点 `a`，然后你有另一个透镜，它可以从 `a` 中提取出更细节的数据。使用 getter 组合是直接的：我们可以首先提取 `a`，然后从 `a` 中提取更小的部分。但在 setter 中，你需要确保任何一次设置都能正确传播到整个结构。这就需要复杂的调整和传递。

为了简化这种复杂操作，人们找到了使用 `profunctor` 的表示法。

### 2. Profunctor 表示法与光学系统

#### 什么是 Profunctor？

`Profunctor` 是一种数学概念，用来描述对象和态射之间的关系。它类似于双函子 (bifunctor)，它的两个参数，一个是逆变的（contravariant），另一个是协变的（covariant）。简而言之，`profunctor` 可以被认为是一种在两个范畴之间建立联系的工具。

在 Haskell 中，`profunctor` 的定义类似于：

```haskell
class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'
```

- `dimap` 接受两个映射函数：
  - 第一个是 `a' -> a`，这是逆变方向上的映射；
  - 第二个是 `b -> b'`，是协变方向上的映射。

通过这种机制，`profunctor` 可以很自然地表示光学系统中的 `getter` 和 `setter`，并可以轻松进行组合。

#### Profunctor Optics（泛函光学）

Profunctor optics 利用了 `profunctor` 的特性，特别是它能够从两个不同的方向映射数据的特性。光学系统的核心原理是通过 `profunctor` 来抽象数据结构的部分提取和修改操作。通过这种抽象化，可以将原本复杂的操作，尤其是 `setter` 的操作，简化为普通的函数组合。

例如，透镜可以通过以下方式用 `profunctor` 来表示：

```haskell
type Lens s t a b = forall p. Profunctor p => p a b -> p s t
```

在这个定义中，`Lens s t a b` 接受一个 `profunctor`，将其作用于 `a` 和 `b` 上，并将其推广到整个数据结构 `s` 和 `t`。

### 3. Tambara 模块

Tambara 模块是对 `profunctor` 结构的进一步推广。它定义了如何将 `profunctor` 光学系统与特定的范畴结构进行交互，特别是那些具有张量积（tensor product）的范畴。

Tambara 模块的核心思想是：通过定义一种特殊的方式来描述如何组合两个 `profunctor`，我们可以简化光学系统的组合问题，尤其是 `setter` 操作的组合。

#### Tambara 模块的定义

Tambara 模块的定义依赖于某种形式的张量结构（tensor structure）。在 Haskell 中，它大致可以定义如下：

```haskell
class Profunctor p => Tambara p where
  tambara :: p a b -> p (s, a) (s, b)
```

这里的 `tambara` 函数说明了如何将一个 `profunctor` 作用于某种形式的组合（比如 `s` 和 `a` 的笛卡尔积），并返回组合后的结果。这种组合方式使得我们可以轻松地在光学系统中处理复杂的数据结构。

#### 为什么 Tambara 模块对光学有帮助？

Tambara 模块通过定义这种特殊的组合方式，使得光学系统中的 `setter` 操作不再复杂。它将组合问题简化为对 `profunctor` 的简单函数操作。通过 `tambara` 函数，我们可以轻松地将不同的光学操作组合起来，而不需要担心每个 `setter` 的内部细节。

举例来说，如果我们有两个透镜 `Lens s t a b` 和 `Lens a b c d`，我们可以使用 Tambara 模块将它们组合成一个新的透镜，从 `s` 中提取出 `c`，并修改它以生成 `d`，最终返回修改后的 `t`。

### 4. Tambara 模块在编程中的应用

在编程中，尤其是 Haskell 中，Tambara 模块通过 `profunctor optics` 的方式极大简化了处理复杂数据结构的光学工具。这种技术已经被广泛应用于一些库中，例如 `Optics` 和 `Profunctor` 库。

通过使用 Tambara 模块，我们可以：

1. **简化光学系统的组合**：特别是 `setter` 操作，之前需要复杂的调整和传递，而现在可以通过函数组合轻松实现。
2. **提高代码的可读性**：通过将复杂的数学结构隐藏在 `profunctor` 的抽象中，代码变得更加直观。
3. **更好的性能**：由于 `profunctor` 的函数组合本质上是高效的，因此 Tambara 模块的应用也能提升程序的性能。

### 总结

Tambara 模块通过 `profunctor` 表示法，为光学系统中的组合问题提供了一种优雅的解决方案。它允许我们通过函数组合来轻松处理复杂的 `getter` 和 `setter` 操作，极大地简化了处理复杂数据结构的任务。

### -------------------------

### 18.1 Tannakian 重建 (Tannakian Reconstruction)

Tannakian 重建理论研究如何从一个范畴的表示中重建这个范畴的结构。在本节中，我们将通过幺半群及其表示来理解这一点。这一理论最初是为了解群的表示论而发展起来的，但这里我们将使用幺半群（monoids）来代替群，因为它们是更通用的数学对象。

#### 幺半群与其表示 (Monoids and their Representations)

**幺半群**可以被看作是一个特殊的结构。它是一个只包含一个对象的范畴，在这个范畴中，同态集 $\mathcal{M}(\langle \rangle, \langle \rangle)$ 包含了所有该对象上的态射。这些态射在结合律和单位元的作用下满足幺半群的性质。

举个例子，考虑整数加法构成的幺半群。我们可以将其理解为一个只有一个对象 $\langle \rangle$ 的范畴，并且每个整数代表从该对象到自身的一个态射。两个整数相加，就相当于两个态射的组合。如下图所示：

$$
\langle \rangle \xrightarrow{2} \langle \rangle \xrightarrow{5} \langle \rangle
$$
态射的组合就是将两个整数相加：$2 + 5 = 7$。

幺半群中的“乘法”对应于态射的组合，而幺半群的单位元对应于范畴中的恒等态射（identity morphism）。

在范畴论的角度下，每一个幺半群都可以表示为一个单对象范畴，反之亦然。因此，研究幺半群的表示理论就等价于研究单对象范畴的表示理论。

#### 幺半群的表示

幺半群的表示是一个将幺半群映射到集合的函数。通过一个函子 $F: \mathcal{M} \to \text{Set}$，我们可以将幺半群 $\mathcal{M}$ 的对象 $\langle \rangle$ 映射到某个集合 $S$，并将同态集 $\mathcal{M}(\langle \rangle, \langle \rangle)$ 映射为一组从集合 $S$ 到自身的变换（即 $S \to S$ 的函数）。

这意味着，每个幺半群的表示实际上是定义在一个集合上的函数族，这些函数保持幺半群的结构。

##### 例子：整数加法幺半群的表示

假设我们有一个幺半群 $\mathcal{M}$ 由整数加法构成。将其表示 $F: \mathcal{M} \to \text{Set}$ 应该满足两个条件：

1. 单位元 $0$ 对应集合 $S$ 上的恒等函数。
2. 任意两个元素 $m, n \in \mathcal{M}$ 的组合 $m + n$ 在集合 $S$ 上的表示就是将 $F(m)$ 和 $F(n)$ 组合起来，结果是 $F(m + n)$。

因此，如果将整数加法幺半群表示为函数作用 $S \to S$，那么它就类似于一种变换在集合上的表示。

#### 完全忠实的函子

如果这个表示的函子 $F$ 是 **完全忠实的**（fully faithful），那么这个表示能够保持所有幺半群的结构信息，并且没有任何额外的信息丢失。

* “完全忠实”意味着两个性质：
  1. **忠实性（faithfulness）**：不同的态射映射到不同的函数上。
  2. **完全性（fullness）**：所有映射都在函数集中，没有丢失任何信息。

##### 函子的“作弊”

然而，在很多情况下，函子并不是完全忠实的。例如：

- 映射到集合 $S$ 上的函数集 $\text{Set}(S, S)$ 可能包含一些不在幺半群中的函数。
- 幺半群中的多个态射可能会映射到同一个函数上。

在极端情况下，幺半群的所有态射可能都映射到集合上的恒等函数 $id_S$，导致信息丢失。因此，单凭一个表示 $S$ 来观察幺半群的结构是不够的，我们需要考虑它的所有表示。

#### Tannakian 重建

为了重建幺半群的完整结构，我们必须同时查看它的 **所有表示**。这些表示构成一个函子范畴 $[\mathcal{M}, \text{Set}]$，也称为幺半群 $\mathcal{M}$ 上的 **余预层范畴**（co-presheaf category）。在这个范畴中，幺半群的表示是对象，自然变换（natural transformations）是箭头。

由于幺半群只有一个对象，自然变换的结构变得相对简单。自然变换 $\alpha: F \to G$ 只有一个分量，它是从集合 $F(\langle \rangle)$ 到 $G(\langle \rangle)$ 的一个函数。自然性条件要求下列图交换：

$$
\xymatrix{
F(\langle \rangle) \ar[r]^{\alpha} \ar[d]_{F(m)} & G(\langle \rangle) \ar[d]^{G(m)} \\
F(\langle \rangle) \ar[r]^{\alpha} & G(\langle \rangle)
}
$$
这表示在集合上的三个函数之间存在一个关系：

$$
G(m) \circ \alpha = \alpha \circ F(m)
$$
换句话说，对于幺半群中的每一个态射 $m$，在 $F$ 和 $G$ 上的表示是兼容的。你可以先在 $F$ 中通过 $m$ 变换一个元素，再通过 $\alpha$ 映射到 $G$，或者先通过 $\alpha$ 进行映射，再在 $G$ 中通过 $m$ 进行变换，最终的结果是相同的。

#### 等变函数

这种符合自然性条件的函数被称为 **等变函数**（equivariant functions）。等变函数能够在两个集合上的作用之间建立联系。它们是幺半群的同态，通过前置或后置组合，等变函数保持了不同集合上的结构。

### 总结

Tannakian 重建理论通过考察幺半群的所有表示，可以重建幺半群的完整结构。通过考虑这些表示的自然变换，我们能够获得幺半群的等变函数，从而可以保持其操作和性质。

这种重建思想不仅适用于幺半群，也适用于更广泛的范畴和表示论问题。在编程语言中，表示论和 Tannakian 重建可以用于解析数据结构和其上的操作，为抽象代数提供理论支撑。

### -------------------------

### 幺半群的 Tannakian 重建 (Tannakian Reconstruction of a Monoid)

Tannakian 重建提供了一种方法，通过观察幺半群的所有表示，来重建幺半群自身。仅仅查看单个集合 $S$ 及其上的操作是不够的，因为任何幺半群都可以在任意集合上表示。然而，通过构建这些集合之间的函数及其关系，我们有机会重建幺半群的结构。

#### 函子 $F: \mathcal{M} \to \text{Set}$ 和同态集

给定一个从幺半群 $\mathcal{M}$ 到 $\text{Set}$ 的函子 $F$，我们可以考虑所有函数的集合 $F \langle \rangle \to F \langle \rangle$，即同态集 $\text{Set}(F \langle \rangle, F \langle \rangle)$。这些函数中的一些将是幺半群的作用，即形如 $F m$ 的函数，其中 $m$ 是幺半群 $\mathcal{M}$ 中的某个箭头。

我们需要注意，这个同态集可能包含一些函数，它们并不对应于幺半群的作用。这意味着并非所有的函数都是幺半群的变换，但其中确实有一些是幺半群元素的表示。

#### 等变函数与自然变换

设 $G \langle \rangle$ 是另一个集合，来自另一个函子 $G$ 的像。在其同态集 $\text{Set}(G \langle \rangle, G \langle \rangle)$ 中，我们会发现形如 $G m$ 的表示。同样，这里的 $m$ 是幺半群的元素。

一个 **等变函数** 或 **自然变换**（natural transformation）$\alpha$ 连接任何两个相关的幺半群作用 $F m$ 和 $G m$。这种连接关系可以用以下交换图表示：

$$
\begin{aligned}
F \langle \rangle \xrightarrow{\alpha} G \langle \rangle \\
F \langle \rangle \xrightarrow{F m} F \langle \rangle \xrightarrow{\alpha} G \langle \rangle \\
G \langle \rangle \xrightarrow{G m} G \langle \rangle
\end{aligned}
$$

这意味着，等变函数 $\alpha$ 保证了你可以先在 $F$ 上应用 $m$，然后通过 $\alpha$ 映射到 $G$，或者你可以先通过 $\alpha$ 映射到 $G$，再在 $G$ 上应用 $m$，两者的结果是相同的。

#### 端（Ends）的定义

我们可以通过构造一个“巨大的元组”来捕获所有函数，这些函数来自所有从 $\mathcal{M}$ 到 $\text{Set}$ 的函子 $F$。这个元组的每个元素是来自同态集 $\text{Set}(F \langle \rangle, F \langle \rangle)$ 中的一个函数。然而，我们只对那些彼此相关的元组感兴趣。也就是说，任何两个函数 $g \in \text{Set}(G \langle \rangle, G \langle \rangle)$ 和 $h \in \text{Set}(H \langle \rangle, H \langle \rangle)$ 通过自然变换 $\alpha$ 连接，必须满足：

$$
\alpha \circ g = h \circ \alpha
$$

这正是 **端**（ends）的定义。端的楔条件保证了在两个表示之间保持自然性。

#### 端下的 Profunctor

在这个表示中，profunctor $P$ 由下式给出：

$$
P \langle G, H \rangle = \text{Set}(G \langle \rangle, H \langle \rangle)
$$

这意味着我们可以通过与 $\alpha$ 的前置组合和与 $\beta$ 的后置组合，生成一个新的函数。这个过程由函数 $\beta \circ f \circ \alpha$ 完成，其中 $\alpha$ 和 $\beta$ 是自然变换，而 $f$ 是从 $\text{Set}(G \langle \rangle, H \langle \rangle)$ 中的一个函数。

#### Tannakian 重建定理

Tannakian 重建定理表明，端的表示为：

$$
\int_F \text{Set}(F \langle \rangle, F \langle \rangle) \cong \mathcal{M}(\langle \rangle, \langle \rangle)
$$

这意味着，通过观察所有函子 $\mathcal{M} \to \text{Set}$ 的表示以及它们的自然变换，我们可以重建原始的幺半群结构。

### 总结

Tannakian 重建为我们提供了一种从表示中重建幺半群的方式。通过查看所有的函子及其上的自然变换，我们能够捕捉到幺半群的作用及其结构。这种方法不仅适用于幺半群，还可以推广到更复杂的代数结构，使得它成为一个重要的工具。

### -------------------------

### Cayley 定理 (Cayley’s Theorem)

Cayley 定理是群论中的一个基本定理，指出 **每个群都同构于某个置换群的子群**。群是一个幺半群，其中每个元素都有逆元。置换是将一个集合映射到其自身的双射（bijection）函数。换句话说，置换是集合元素的重新排列。

在范畴论的背景下，Cayley 定理几乎内置于幺半群及其表示的定义中。我们可以通过构造一个函子将幺半群映射到集合，从而建立群与置换群的对应关系。

#### 幺半群的表示

考虑单对象幺半群 $\mathcal{M}$，其唯一对象记为 $\langle \rangle$。$\mathcal{M}(\langle \rangle, \langle \rangle)$ 是该幺半群的同态集，定义了所有的“变换”操作。Cayley 定理告诉我们，任何幺半群的每个元素都可以通过置换操作来表示。

为了使这一点更加形式化，我们构造一个函子 $F : \mathcal{M} \to \text{Set}$，该函子将 $\langle \rangle$ 映射到集合 $S$，其中 $S = \mathcal{M}(\langle \rangle, \langle \rangle)$。换句话说，集合 $S$ 包含了幺半群中的所有形态。

我们定义 $F$ 在形态上的作用为后置组合：

$$
(F m) n = m \circ n
$$

这里，$m$ 是 $\mathcal{M}$ 中的一个形态，而 $n$ 是集合 $S$ 中的一个元素，也是 $\mathcal{M}$ 中的形态。因此，$F m$ 是通过 $m$ 对集合 $S$ 进行的置换操作。

#### 幺半群的表示

我们可以将这个表示视为 $\text{Set}$ 中的幺半群。我们需要定义幺半群的两个基本操作：**单位元**和**乘法**：

- **单位元** $\eta : 1 \to S$ 选择 $\mathcal{M}(\langle \rangle, \langle \rangle)$ 中的恒等元素 $\text{id}_{\langle \rangle}$ 作为 $S$ 中的元素。
- **乘法** $\mu : S \times S \to S$ 是两个元素 $m$ 和 $n$ 的组合，对应于 $m \circ n$。

通过这些定义，$S$ 可以看作是 $F : \mathcal{M} \to \text{Set}$ 的像。在这种情况下，幺半群的表示就是 $S \to S$ 的函数，Cayley 定理本质上表明，每个幺半群都可以通过一组自函子表示。

#### 编程中的 Cayley 定理示例

Cayley 定理在编程中的一个著名应用是**高效实现列表的反转**。

我们先来看一个简单的递归实现，用来反转列表：

```haskell
reverse :: [a] -> [a]
reverse [] = []
reverse (a : as) = reverse as ++ [a]
```

这种实现将列表分为头部 $a$ 和尾部 $as$，递归地反转尾部，然后将头部元素追加到结果中。然而，这种实现有一个问题：每次追加操作的时间复杂度是 $O(N)$，导致整个算法的性能为 $O(N^2)$。

列表是一个幺半群，因此我们可以使用 **Cayley 定理** 提供的一种更高效的表示，称为**差异列表** (DList)：

```haskell
type DList a = [a] -> [a]
```

**差异列表** 是一个函数，它接受一个列表并将其追加到另一个列表。我们通过将一个普通列表转换为一个差异列表来实现这个表示：

```haskell
rep :: [a] -> DList a
rep as = \xs -> as ++ xs
```

要将一个差异列表还原为普通列表，我们只需将其应用于一个空列表：

```haskell
unRep :: DList a -> [a]
unRep f = f []
```

我们可以验证，空列表的表示是恒等函数，而两个列表的连接是两个差异列表的组合：

```haskell
rep [] = id
rep (xs ++ ys) = rep xs . rep ys
```

因此，差异列表是 Cayley 表示的一种形式。

现在我们可以用差异列表来优化我们的列表反转算法：

```haskell
rev :: [a] -> DList a
rev [] = rep []
rev (a : as) = rev as . rep [a]

fastReverse :: [a] -> [a]
fastReverse = unRep . rev
```

尽管看起来这个算法在我们的递归实现上增加了一个额外的转换层，然而它的性能是线性的 $O(N)$，而不是 $O(N^2)$。例如，当我们反转列表 `[1, 2, 3]` 时，函数 `rev` 将其转换为一个函数组合：

$$
\text{rep} [3] \circ \text{rep} [2] \circ \text{rep} [1]
$$

这个组合函数可以在 $O(N)$ 的时间内完成，因为前置操作的时间复杂度是常数。

#### Cayley 表示与 Haskell 中的 foldl

另一种实现高效列表反转的方法是使用 `foldl`，它也能提供 $O(N)$ 的性能：

```haskell
reverse = foldl (\as a -> a : as) []
```

这是因为 `foldl` 从左到右遍历列表，累积一个闭包，并根据需要按先进先出的顺序执行这些操作。这与差异列表的实现有相似之处。

### 总结

Cayley 定理在范畴论中为我们提供了一种通过表示置换操作来理解幺半群的方式。它不仅是数学中的一个基本定理，在编程中也有许多实际的应用，例如列表的高效操作。在 Haskell 中，通过差异列表，我们能够将递归列表操作的时间复杂度从 $O(N^2)$ 优化为 $O(N)$。

### -------------------------

### Tannakian 重建的证明 (Proof of Tannakian Reconstruction)

Tannakian 重建是一个更广泛的范畴论定理的特例，其中使用了常规范畴而不是单对象范畴。类似于幺半群的重建，在 Tannakian 重建中，我们可以从范畴的表示中恢复原始范畴的同态集。公式如下：

$$
\int_{F : [\mathcal{C}, \text{Set}]} \text{Set}(F a, F b) \cong \mathcal{C}(a, b)
$$

这是我们需要证明的等式。

#### Yoneda 引理的应用

首先，我们利用 **Yoneda 引理** 来表示 $F$ 的作用。根据 Yoneda 引理，有如下同构：

$$
F a \cong [\mathcal{C}, \text{Set}](\mathcal{C}(a, *), F)
$$

这意味着，$F a$ 是表示函子 $\mathcal{C}(a, -)$ 在函子范畴 $[\mathcal{C}, \text{Set}]$ 中的一个自然变换。同样的方式，我们可以表示 $F b$，即：

$$
F b \cong [\mathcal{C}, \text{Set}](\mathcal{C}(b, *), F)
$$

接下来我们得到：

$$
\int_{F : [\mathcal{C}, \text{Set}]} \text{Set}([\mathcal{C}, \text{Set}](\mathcal{C}(a, *), F), [\mathcal{C}, \text{Set}](\mathcal{C}(b, *), F))
$$

这里，自然变换的集合是 $[\mathcal{C}, \text{Set}]$ 中的同态集。

#### 使用 Yoneda 引理

接下来我们应用 Yoneda 引理的推论，它适用于任何范畴 $\mathcal{A}$，给出：

$$
[\mathcal{A}, \text{Set}](\mathcal{A}(x, *), \mathcal{A}(y, *)) \cong \mathcal{A}(y, x)
$$

特别地，我们可以在这里使用函子范畴 $[\mathcal{C}, \text{Set}]$ 来替换 $\mathcal{A}$。代入之后，我们得到：

$$
\int_{F : [\mathcal{C}, \text{Set}]} \text{Set}([\mathcal{C}, \text{Set}](\mathcal{C}(a, *), F), [\mathcal{C}, \text{Set}](\mathcal{C}(b, *), F)) \cong [\mathcal{C}, \text{Set}](\mathcal{C}(b, *), \mathcal{C}(a, *))
$$

这时，我们可以再次应用 Yoneda 引理到右侧，得到：

$$
\mathcal{C}(a, b)
$$

这正是我们要证明的结果。

#### 端 (End) 的楔条件 (Wedge Condition)

重要的是要理解函子范畴的结构如何通过**楔条件**进入端。楔条件通过自然变换的存在来约束。每当在两个函子 $\alpha : G \to H$ 之间存在一个自然变换时，以下图表必须交换：

$$
\int_{F} \text{Set}(F a, F b) \to \text{Set}(G a, G b) \quad \text{Set}(H a, H b) \to \text{Set}(G a, H b)
$$

这里的自然变换 $\alpha$ 是函子范畴 $[\mathcal{C}, \text{Set}]$ 中的一个形态。

#### 重建的直观理解

我们可以从余预层（co-presheaf）的角度重新解释 Tannakian 重建。设 $F : \mathcal{C} \to \text{Set}$ 是一个余预层。它定义了一个与 $\mathcal{C}$ 的对象相关的子集。如果对于某个对象 $a$，$F a$ 是非空的，我们可以说 $a$ 属于这个子集。$F a$ 的每个元素可以被视为 $a$ 属于该子集的证据。

不过，如果范畴 $\mathcal{C}$ 不是离散的，并不是所有的子集都对应于余预层。特别是，如果 $a \to b$ 之间存在箭头，则存在函数 $F f : F a \to F b$，它将 $a$ 属于某个子集的证据映射为 $b$ 属于该子集的证据。因此，余预层定义了与范畴结构兼容的证据相关子集。

类似地，Tannakian 重建表示，如果 $a$ 属于某个证据相关的子集，那么 $b$ 也必须属于该子集。这只有在 $a \to b$ 的同态存在时才可能。

因此，通过重建定理的应用，我们可以从范畴 $\mathcal{C}$ 的所有表示中恢复范畴 $\mathcal{C}$ 本身的同态集。

### 结论

Tannakian 重建通过使用端（end）和 Yoneda 引理，使得从幺半群及其表示重建原始同态集变得可能。这种重建适用于更一般的范畴，不仅限于幺半群，提供了一个直观且强大的工具来理解范畴的结构。

### -------------------------

### Haskell 中的 Tannakian 重建 (Tannakian Reconstruction in Haskell)

Tannakian 重建理论告诉我们，我们可以通过考虑所有函子 $F$ 的形式 $F a \to F b$ 来恢复范畴 $\mathcal{C}$ 的箭头 $a \to b$。这一想法在 Haskell 中可以使用多态函数和函数组合来实现。

#### 端 (End) 的翻译

在 Haskell 中，`forall` 量词可以用来表示端 (end)。重建理论中的公式：

$$
\int_F \text{Set}(F a, F b) \cong \mathcal{C}(a, b)
$$

在 Haskell 中翻译为：

```haskell
forall f. Functor f => f a -> f b
```

这意味着：给定一个装有类型 $a$ 的函子 `f`，我们要生成一个装有类型 $b$ 的函子 `f`，无论 `f` 是什么函子。右侧的 $\mathcal{C}(a, b)$ 则直接表示为 Haskell 中的普通函数类型 `a -> b`。

#### 多态函数的含义

在 Haskell 中，多态函数可以作用于所有类型或某些类型类的实例。这里，我们有一个为所有函子定义的多态函数：

```haskell
forall f. Functor f => f a -> f b
```

这种函数告诉我们：无论是什么函子，只要给出一个装有 $a$ 的函子实例，它都可以生成一个装有 $b$ 的实例。唯一可以实现这种函数的方法是通过捕获一个 `a -> b` 的函数，并使用 `fmap` 在装有 $a$ 的函子上应用这个函数。

#### 实现 Tannakian 重建

##### 从 `a -> b` 到多态函数

我们可以定义一个函数 `toRep`，它捕获一个类型为 `a -> b` 的函数，并将其提升为多态函数 `forall f. Functor f => f a -> f b`，这个过程通过 `fmap` 来实现：

```haskell
toRep :: (a -> b) -> (forall f. Functor f => f a -> f b)
toRep g fa = fmap g fa
```

##### 从多态函数到 `a -> b`

反过来，我们可以使用 **Yoneda 引理** 的技巧来实现从多态函数到 `a -> b` 的转换。具体来说，我们使用恒等函子 `Id` 来提取这个函数：

```haskell
fromRep :: (forall f. Functor f => f a -> f b) -> (a -> b)
fromRep g a = unId (g (Id a))
```

其中，恒等函子 `Id` 定义为：

```haskell
data Id a = Id a
unId :: Id a -> a
unId (Id a) = a
```

恒等函子的 `Functor` 实例则是：

```haskell
instance Functor Id where
  fmap g (Id a) = Id (g a)
```

#### 使用示例

这种重建看起来似乎不必要地复杂，因为它只是将简单的函数类型 `a -> b` 替换为了一个更复杂的多态函数类型：

```haskell
type Getter a b = forall f. Functor f => f a -> f b
```

然而，这种形式是 Haskell 中所有 **Optics** 的基础。`Getter` 可以看作是从 $a$ 中提取 $b$ 的透镜，它描述了如何从 `a` 中获取 `b`，也即“getter”或“访问器”。

#### Getter 组合

有趣的是，尽管普通函数 `a -> b` 可以通过函数组合进行组合，`Getter` 也可以通过简单的函数组合来进行组合。以下是一个例子：

```haskell
boolToStrGetter :: Getter Bool String
boolToStrGetter = toRep show . toRep (bool "False" "True")
```

这个 `Getter` 首先将 `Bool` 转换为 `String`，使用了 `show` 和 `bool` 函数。尽管其他更复杂的 optics 的组合不一定是如此简单，但 `Getter` 的函子表示法允许通过函数组合来进行组合，使得整个过程更加直观和简洁。

### 总结

在 Haskell 中，Tannakian 重建通过多态函数实现了从范畴论到编程语言的自然转化。这一理论表明，透镜、getter 以及其他 optics 本质上是基于 `a -> b` 函数的推广。使用这种形式，我们可以实现更复杂的组合操作，而函子的组合也得到了极大简化。

### -------------------------

### 带伴随的 Tannakian 重建 (Tannakian Reconstruction with Adjunction)

广义的 Tannakian 重建技巧是在一个函子范畴 $\mathcal{T}$ 上定义端，结合自由/遗忘伴随 $F \dashv U$ 来恢复原始的范畴信息。我们假设两个函子范畴 $\mathcal{T}$ 和 $[\mathcal{C}, \text{Set}]$ 之间存在自由/遗忘伴随关系：

$$
\mathcal{T}(FQ, P) \cong [\mathcal{C}, \text{Set}](Q, UP)
$$

其中，$Q$ 是 $[\mathcal{C}, \text{Set}]$ 中的一个函子，而 $P$ 是 $\mathcal{T}$ 中的一个函子。

#### Tannakian 重建的端表达

Tannakian 重建的起点是以下端公式：

$$
\int_P^{\mathcal{T}} \text{Set}((UP)a, (UP)s)
$$

这里，$P \mapsto (UP)a$ 是所谓的纤维函子，它将 $\mathcal{T}$ 中的函子映射到集合。这个端的公式可以解释为描述两个纤维函子之间的自然变换集。纤维函子探测对象的“微分邻域”，并且它不仅将函子映射到集合，还将自然变换映射到函数。

#### 应用 Yoneda 引理

接下来，我们通过 Yoneda 引理将 $UP$ 的作用表示为自然变换集：

$$
\int_P^{\mathcal{T}} \text{Set}\left([\mathcal{C}, \text{Set}](\mathcal{C}(a, *), UP), [\mathcal{C}, \text{Set}](\mathcal{C}(s, *), UP)\right)
$$

然后，我们可以利用伴随关系 $F \dashv U$，将其转换为 $\mathcal{T}$ 中的自然变换：

$$
\int_P^{\mathcal{T}} \text{Set}(\mathcal{T}(F\mathcal{C}(a, *), P), \mathcal{T}(F\mathcal{C}(s, *), P))
$$

这意味着我们最终得到了 $\mathcal{T}$ 中两个函子之间自然变换的集合。

#### 应用 Yoneda 引理的推论

使用 Yoneda 引理的推论，我们可以进一步简化为：

$$
\mathcal{T}(F\mathcal{C}(s, *), F\mathcal{C}(a, *))
$$

再次应用伴随关系，我们可以得到：

$$
\text{Set}(\mathcal{C}(s, *), (U \circ F)\mathcal{C}(a, *))
$$

最后应用 Yoneda 引理，得出：

$$
((U \circ F)\mathcal{C}(a, *))_s
$$

这意味着，伴随函子 $U \circ F$ 的复合在函子范畴中构成了一个幺半群，我们将其称为 $\Phi$。

#### 最终结果

最终，我们得到了 Tannakian 重建公式：

$$
\int_P^{\mathcal{T}} \text{Set}((UP)a, (UP)s) \cong (\Phi \mathcal{C}(a, *))_s
$$

其中，右侧是幺半群 $\Phi = U \circ F$ 对可表示函子 $\mathcal{C}(a, *)$ 的作用，并在 $s$ 处求值。

#### 比较早期的 Tannakian 重建

这个结果可以与早期的 Tannakian 重建公式比较，尤其是当我们将其写为：

$$
\int_F^{[\mathcal{C}, \text{Set}]} \text{Set}(F a, F s) \cong \mathcal{C}(a, *)_s
$$

这两个公式表明，我们通过自由/遗忘伴随和自然变换集的组合，成功地将范畴的结构通过函子的作用恢复出来。

#### Optics 的推导

在 Optics 中，我们将 $a$ 和 $s$ 替换为 $\mathcal{C}^{\text{op}} \mathcal{C}$ 中的对象对 $\langle a, b \rangle$ 和 $\langle s, t \rangle$，从而使得这些函子变为 **profunctor**，也即将结构推广到了 Profunctor Optics 的框架下。

### -------------------------

### 18 Tambara 模块 (Tambara Modules)

Tambara 模块在编程中的一个重要应用是解决 **profunctor optics** 的组合问题。它简化了 getter 和 setter 的组合，这与图形编程中旋转矩阵的组合方式类似。通过 profunctor 表示法，复杂的 optics 组合可以通过简单的函数组合实现。

### 18.1 Tannakian 重建 (Tannakian Reconstruction)

Tannakian 重建的核心思想是通过函子的表示来重建范畴结构，尤其是从幺半群的表示中恢复幺半群。

#### 幺半群与其表示 (Monoids and their Representations)

一个幺半群可以看作是一个单对象范畴 $\mathcal{M}$，同态集 $\mathcal{M}(\star, \star)$ 代表幺半群中的元素。通过函子 $F : \mathcal{M} \to \text{Set}$，幺半群可以在集合上表示，这种表示保持了幺半群的结构。

#### 幺半群的 Tannakian 重建 (Tannakian Reconstruction of a Monoid)

从幺半群的表示中重建幺半群的关键在于考虑同态集 $\text{Set}(F\star, F\star)$ 中的函数，特别是那些对应于幺半群作用的函数。通过构造一个端 $\int_F \text{Set}(F\star, F\star)$，我们可以恢复幺半群的同态集 $\mathcal{M}(\star, \star)$。

#### Cayley 定理 (Cayley's Theorem)

Cayley 定理表明，每个群都同构于一个置换群的子群。在范畴论中，这个定理自然体现在幺半群表示的定义中。通过构造函子 $F: \mathcal{M} \to \text{Set}$，幺半群可以表示为一组自函子。

Haskell 中的一个应用是使用差异列表（**DList**）来高效实现列表反转。通过将列表表示为函数，列表反转的复杂度可以从 $O(N^2)$ 优化为 $O(N)$。

#### Tannakian 重建的证明 (Proof of Tannakian Reconstruction)

Tannakian 重建定理在更广泛的上下文中得到了证明，公式为：

$$
\int_F^{[\mathcal{C}, \text{Set}]} \text{Set}(F a, F b) \cong \mathcal{C}(a, b)
$$

通过应用 Yoneda 引理，我们可以将左侧的自然变换集转化为范畴 $\mathcal{C}$ 中的同态集，从而证明 Tannakian 重建的有效性。

#### Haskell 中的 Tannakian 重建 (Tannakian Reconstruction in Haskell)

在 Haskell 中，Tannakian 重建可以通过多态函数来实现：

```haskell
toRep :: (a -> b) -> (forall f. Functor f => f a -> f b)
toRep g fa = fmap g fa

fromRep :: (forall f. Functor f => f a -> f b) -> (a -> b)
fromRep g a = unId (g (Id a))
```

这种形式的重建展示了将简单的函数类型 $a \to b$ 转换为更复杂的类型 `forall f. Functor f => f a -> f b` 的过程。

#### 带伴随的 Tannakian 重建 (Tannakian Reconstruction with Adjunction)

在更一般的情况下，Tannakian 重建可以与自由/遗忘伴随 $F \dashv U$ 结合使用。我们从以下端开始：

$$
\int_P^{\mathcal{T}} \text{Set}((UP)a, (UP)s)
$$

通过使用 Yoneda 引理和伴随关系，我们得出最终结果：

$$
\int_P^{\mathcal{T}} \text{Set}((UP)a, (UP)s) \cong (\Phi \mathcal{C}(a, *))_s
$$

其中 $\Phi = U \circ F$ 是伴随函子的复合。这为 profunctor optics 提供了理论基础。


### -------------------------

### --------------------------------




### -------------------------


### -------------------------


### -------------------------


### -------------------------


### -------------------------


### -------------------------


### -------------------------


### -------------------------

