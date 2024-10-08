[toc]

### -------------------------

### 余代数 (Coalgebras)

余代数（Coalgebras）可以被看作是代数（Algebras）在**对偶范畴**中的对应物。代数通过**折叠**递归数据结构来处理它们，而余代数则通过**展开**递归结构来生成它们。余代数的主要工具是**异态射** (Anamorphisms)，它用于从种子生成递归的、类似树的结构。

#### 1. **代数和余代数的对偶性**

代数处理的是**初代数** (Initial Algebras)，这是一个初对象，没有任何入射箭头。而**余代数**则处理**终余代数** (Terminal Coalgebras)，它们与终对象关联，具有唯一的入射箭头和多个出射箭头。

- **代数**：从初对象开始，通过卡塔态射（Catamorphisms）折叠递归数据结构。
- **余代数**：从终对象开始，通过异态射（Anamorphisms）展开递归数据结构。

#### 2. **递归和展开**

代数主要用来处理递归数据结构的**折叠**，例如处理列表、树或其他递归结构中的信息。折叠是一种减少信息的过程，例如将一个列表中的元素求和。在这个过程中，结构中的信息被压缩成一个单一的值，丢失了原始的细节。

与此相对，余代数用于**生成**递归结构。它将一个初始的**种子**值逐步展开为一个复杂的递归结构。虽然余代数不会创造新的信息，但它会将种子中的信息以递归结构的形式展开，方便进一步处理和操作。

例如，使用余代数生成一棵树时，种子中包含所有最终生成树的信息。余代数只是将这些信息展开，而不增加新的信息。

#### 3. **异态射（Anamorphisms）**

异态射 (Anamorphisms) 是余代数中的核心操作，就像代数中的卡塔态射一样。它们可以通过递归地展开种子，生成无限的数据结构。与折叠不同，展开不会丢失信息，而是将原始的信息重新组织成不同的结构。

- **卡塔态射 (Catamorphisms)**：通过折叠递归结构，减少信息量（例如求和）。
- **异态射 (Anamorphisms)**：通过展开递归结构，从种子生成数据结构。

#### 4. **信息的转化**

代数和余代数的共同点是它们都不会创造新的信息，只是将已有的信息重新组织。例如：
- **折叠**会将复杂的结构压缩成简单的值（例如列表求和）。
- **展开**会将简单的种子展开成复杂的结构（例如生成一个无限的列表）。

代数和余代数这两个过程在某种意义上是对立的，但它们都体现了范畴论中对结构的处理方式：折叠压缩结构，展开生成结构。

#### 5. **终余代数的特性**

与初代数不同，终余代数不仅仅是初代数的镜像。由于它们是基于终对象构造的，具有更多的灵活性。这种灵活性体现在终余代数的出射箭头上，它们允许展开过程生成丰富的结构，而不仅仅是镜像初代数的折叠过程。

#### 6. **总结**

余代数在递归结构的生成中扮演着重要角色，它通过异态射将简单的种子信息展开为复杂的递归结构。与代数的折叠操作相对，余代数通过展开操作为我们提供了一种生成和组织信息的方式。

这一章展示了代数和余代数的对偶性，以及它们在处理递归结构中的不同作用——代数用于折叠，余代数用于展开。这两者一起构成了处理递归数据结构的完整工具集。

### -------------------------

### 13.1 由自函子生成的余代数 (Coalgebras from Endofunctors)

在范畴论中，**余代数**（Coalgebra）是代数的对偶。给定一个自函子 (Endofunctor) 𝐹，一个余代数是一个包含载体和结构映射的对。具体来说，它定义了从载体类型到应用自函子的映射。这可以被用来生成递归的数据结构，比如树或列表。

#### 1. **余代数的定义**

一个 **余代数** 是一个对 $(a, \gamma)$，其中：
- **载体** (Carrier) $a$ 是生成结构的种子类型。
- **结构映射** $\gamma : a \to F a$ 是一个从载体到自函子作用后的载体的箭头。

在 Haskell 中，我们可以将其定义为：

```haskell
type Coalgebra f a = a -> f a
```

载体 $a$ 代表着我们生成结构时的种子，而 $f a$ 是应用了自函子后的结果。在余代数中，结构映射会递归地生成递归结构。

#### 2. **TreeF 函子的例子**

考虑一个表示二叉树的自函子 `TreeF`，其中节点存储整数，二叉树的定义如下：

```haskell
data TreeF x = LeafF | NodeF Int x x
  deriving (Show, Functor)
```

- `LeafF` 表示叶子。
- `NodeF` 表示包含整数和两个子节点的二叉树节点。

`deriving (Show, Functor)` 是 Haskell 的自动派生机制，用来自动生成 `Functor` 实例和 `Show` 实例。`Functor` 实例允许我们将函数映射到树结构中的子树。

#### 3. **余代数的结构映射**

现在，我们可以定义一个具体的余代数来生成二叉树结构。我们使用一个整数列表作为种子，并递归地生成二叉树。

```haskell
split :: Coalgebra TreeF [Int]
split [] = LeafF
split (n : ns) = NodeF n left right
  where
    (left, right) = partition (<= n) ns
```

在 `split` 函数中：
- **空列表** 被映射为 `LeafF`，表示生成一个叶子。
- **非空列表** 以其第一个元素 `n` 为根节点，`ns` 是剩余的列表。函数 `partition (<= n) ns` 将 `ns` 分割为两个列表：
  - 一个包含所有小于或等于 `n` 的元素。
  - 另一个包含所有大于 `n` 的元素。
  - 左子树使用小于等于 `n` 的元素构建，右子树使用大于 `n` 的元素构建。

这个余代数生成了一棵**二叉搜索树**（BST），每个节点的左子树中的值都小于或等于该节点的值，右子树中的值都大于该节点的值。

#### 4. **递归生成树**

通过递归地应用 `split` 函数，余代数会自动生成一个排序的二叉树。

例如，给定列表 `[5, 2, 8, 3, 1]`，`split` 会将 5 作为根节点，左子树是由 `[2, 3, 1]` 构成，右子树则由 `[8]` 构成。这个过程会递归地进行，直到构建出整个树。

#### 5. **进一步应用：排序**

接下来，我们可以使用这个余代数构造的树来实现排序。通过中序遍历树的节点，生成一个排序后的列表。这个方法实际上是**二叉树排序算法**的实现。

总结，余代数允许我们使用自函子生成递归的数据结构，并通过结构映射递归地展开这些结构。在这个例子中，`split` 函数展示了如何使用余代数生成一个二叉搜索树，余代数为生成和处理递归结构提供了强大的抽象工具。

### -------------------------

### 13.2 余代数的范畴 (Category of Coalgebras)

与代数类似，余代数（Coalgebra）的态射（Morphism）也是满足特定交换条件的箭头。通过这种结构，我们可以定义余代数的范畴。在余代数的范畴中，我们关心的是**终余代数**，这是在该范畴中的特殊对象，它是所有其他余代数的“最大不动点”。

#### 1. **余代数态射 (Coalgebra Morphisms)**

给定两个余代数 $(a, \alpha)$ 和 $(b, \beta)$，**余代数态射**是一个箭头 $f : a \to b$，使得以下图交换：

$$
\begin{array}{c}
a \xrightarrow{f} b \\
\downarrow_{\alpha} \quad \downarrow_{\beta} \\
F a \xrightarrow{F f} F b
\end{array}
$$

这意味着无论是先通过箭头 $f$ 映射载体，再应用余代数 $\beta$，还是先应用余代数 $\alpha$ 再通过 $F f$ 映射到目标，结果都是相同的。

这个交换条件表达了 $f$ 保持余代数结构的一致性。可以类比代数中的态射，余代数态射也是可以组合的，并且恒等箭头也是余代数态射。这样一来，余代数形成了一个**范畴**。

#### 2. **终余代数 (Terminal Coalgebras)**

在余代数的范畴中，**终余代数**是一个特殊对象 $(t, \tau)$，它满足以下条件：对于任何其他余代数 $(a, \alpha)$，都存在唯一的余代数态射从 $(a, \alpha)$ 到 $(t, \tau)$。这是余代数的对偶现象，类似于代数范畴中的初代数。

**Lambek 引理**的对偶也适用于终余代数，证明其结构映射 $\tau$ 是一个同构，即 $\tau : t \to F t$ 是可逆的。根据这个引理，终余代数的载体也是自函子的**不动点**，表示为 $\nu F$，满足：

$$
F t \cong t
$$

这意味着终余代数的载体是这个自函子的**最大不动点**，记为 $\nu F$。相比之下，初代数的载体是自函子的**最小不动点**，记为 $\mu F$。

#### 3. **初代数到终余代数的态射**

在余代数和代数的范畴中，存在一个从初代数到终余代数的唯一态射。这个态射 $\rho$ 嵌入了初代数的载体到终余代数的载体。在集合范畴中，初代数的载体集合是终余代数的载体集合的子集。这个唯一的态射 $\rho$ 将初代数嵌入终余代数：

$$
\rho: \mu F \to \nu F
$$

这个态射可以视为将一个递归结构的“最小表示”映射到其“最大表示”。例如，在集合范畴中，初代数的载体可能是有限深度的结构，而终余代数的载体则可以表示为无限深度的结构。

#### 4. **Haskell 中的余代数**

在 Haskell 中，固定点类型 `Fix f` 可以作为代数和余代数的载体。这种类型可以递归定义，如下所示：

```haskell
data Fix f where
  In :: f (Fix f) -> Fix f
```

这种定义可以用于递归生成数据结构。在具有叶节点的函子中（如树结构），`Fix f` 既是初代数的载体，也是终余代数的载体。

#### 5. **练习：Set 中的恒等函子**

- **练习 13.2.2**: 证明在集合范畴（Set）中的恒等函子中，每个对象都是不动点，空集是最小的不动点，而单集合是最大的。这意味着空集到单集合存在唯一的箭头。

- **练习 13.2.3**: 证明空集是恒等函子的初代数的载体，而单集合是终余代数的载体。

这些练习展示了在集合范畴中如何构建初代数和终余代数，并通过唯一箭头建立它们之间的联系。

### -------------------------

### 13.3 异态射 (Anamorphisms)

终余代数 $(t, \tau)$ 通过其**泛性质**（universal property）定义：对任何余代数 $(a, \alpha)$，存在一个唯一的余代数态射 $h : a \to t$，使得如下图所示的交换图成立：

$$
\begin{array}{c}
a \xrightarrow{h} t \\
\downarrow_{\alpha} \quad \downarrow_{\tau} \\
F a \xrightarrow{F h} F t
\end{array}
$$

这个唯一的余代数态射 $h$ 被称为**异态射 (Anamorphism)**。作为一个余代数态射，它通过 $\alpha$ 和 $\tau$ 的递归结构定义，并遵循 Lambek 引理的对偶性。

#### **Lambek 引理解法**
根据 Lambek 引理，异态射 $h$ 的递归解法可以表示为：

$$
h = \tau^{-1} \circ F h \circ \alpha
$$

这个公式表明，我们可以通过递归方式构造异态射。该递归结构与 catamorphism（用于代数的结构）类似，只不过这里是逆向的过程，用于生成数据结构。

#### **异态射在 Haskell 中的实现**
在 Haskell 中，异态射的实现方式几乎与 catamorphism 类似，只不过它是通过**余代数**生成数据结构而不是折叠数据结构。对于任何自函子 $F$ 和余代数 $\alpha$，我们可以定义异态射 `ana` 如下：

```haskell
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coa = In . fmap (ana coa) . coa
```

**解释：**
- `ana` 是一个接受余代数 `coa` 和种子值的函数。
- 首先对种子应用余代数 `coa`，这会返回一个函子 `f a`，其内容包含用于递归展开的“新种子”。
- 通过 `fmap`，我们递归地对这些新种子再次应用 `ana`，继续展开结构。
- 最后，通过 `In` 将结果注入到递归结构中。

#### **例子：生成二叉树**

例如，考虑我们之前定义的用于生成二叉树的 `split` 余代数：

```haskell
split :: Coalgebra TreeF [Int]
split [] = LeafF
split (n : ns) = NodeF n left right
  where
    (left, right) = partition (<= n) ns
```

这个余代数通过列表生成二叉排序树。当种子列表为空时，生成叶子 `LeafF`；当列表有元素时，生成 `NodeF` 节点，其中左右子树由分割后的子列表递归生成。

现在我们可以使用 `ana` 来应用这个余代数：

```haskell
ana split :: [Int] -> Fix TreeF
```

这会将一个整数列表转换为二叉树。

#### **与 Catamorphism 结合使用**

为了对生成的二叉树进行进一步操作（如排序），我们可以定义一个 catamorphism 来将树折叠回列表。这里是将树转换为列表的代数 `toList`：

```haskell
toList :: Algebra TreeF [Int]
toList LeafF = []
toList (NodeF n ns ms) = ns ++ [n] ++ ms
```

`toList` 将树的左侧列表、单个枢轴节点、右侧列表连接成一个新的列表。为了实现快速排序，我们将 `ana` 和 `cata` 结合起来：

```haskell
qsort :: [Int] -> [Int]
qsort = cata toList . ana split
```

这个函数首先通过 `ana split` 将列表生成一棵二叉树，然后通过 `cata toList` 把树折叠回排序后的列表。这实际上是一个（尽管低效的）快速排序算法的实现。

#### **总结**

- **异态射 (Anamorphisms)** 是余代数中生成递归数据结构的方式。
- 与 catamorphism 通过递归地“折叠”数据结构不同，anamorphism 是递归地“展开”数据结构。
- 在 Haskell 中，`ana` 通过递归应用余代数生成数据结构，而 `cata` 可以与其结合来处理递归结构。

**练习：**

1. 尝试定义一个余代数，用于生成另一种递归结构（如列表或其他类型的树），并通过异态射生成结构。
2. 思考如何将 anamorphism 与 catamorphism 结合，处理更多复杂的数据生成和处理问题。

### -------------------------

### 13.3 无限数据结构 (Infinite Data Structures)

在研究 **代数 (Algebras)** 时，我们构建的数据结构通常具有叶节点（即没有子节点的终止条件），这意味着我们从某个地方开始，并递归地构建整个结构。然而，在 **余代数 (Coalgebras)** 中，我们可以生成**无限数据结构**，不必依赖叶节点成分。

#### **代数与余代数的区别**

- 在代数中，我们通过 **catamorphisms** 对递归数据结构进行折叠（如遍历和处理结构）。
- 在余代数中，我们通过 **anamorphisms** 递归地生成数据结构，特别是在无限数据结构的情况下。

#### **惰性求值 (Lazy Evaluation)** 和无限数据结构

在 Haskell 中，由于其惰性求值特性，我们可以轻松表示和处理无限数据结构。惰性求值意味着只有当我们真正需要某个值时，才会去计算它。Haskell 中的无限数据结构是在后台生成的，并且在使用这些数据时才会进行计算。

对于**严格求值语言**，无限数据结构需要使用函数来表示未计算的值，这些函数称为“**延迟计算 (thunks)**”。而在 Haskell 中，惰性求值已经内建于语言中，帮助我们轻松构造和使用无限数据结构。

#### **例子：无限流 (Stream)**

我们来看一个简单的例子——**无限流 (Stream)**。流是一种可以无限扩展的数据结构，每次需要时递归地生成下一部分。

##### 1. **定义函子 (Functor)**

首先，我们定义一个生成无限流的函子，类似于生成列表的函子。不同的是，这里没有叶节点，也没有空列表构造器。

```haskell
data StreamF a x = StreamF a x
  deriving Functor
```

在这里，`StreamF a x` 表示流的一个元素，其中 `a` 是当前流的值，`x` 是流的下一个部分。`StreamF` 的 Functor 实例通过 `deriving` 自动生成。

##### 2. **定义 Stream 类型**

接下来，我们定义 `Stream` 类型作为 `StreamF` 的不动点 (fixed point)：

```haskell
type Stream a = Fix (StreamF a)
```

这定义了一个包含无限数据的流，其中 `Fix` 是构造递归数据结构的标准方式。

##### 3. **定义 Coalgebra (余代数)**

我们定义一个生成整数流的余代数。它接受一个整数 `n` 作为种子，并生成包含 `n` 和下一个种子 `n + 1` 的流：

```haskell
step :: Coalgebra (StreamF Int) Int
step n = StreamF n (n + 1)
```

这个余代数生成当前数 `n`，并且使用 `n + 1` 作为下一个流的生成种子。

##### 4. **定义 Anamorphism (异态射)**

使用 `ana` 函数，我们可以通过递归应用 `step` 来生成包含所有自然数的无限流：

```haskell
allNats :: Stream Int
allNats = ana step 0
```

当我们执行 `ana step 0` 时，会生成一个从 `0` 开始的自然数流：`0, 1, 2, 3, ...`。Haskell 的惰性求值特性使得这个操作是瞬时完成的，直到我们显式需要访问流中的某些元素时才进行实际计算。

##### 5. **访问无限流**

我们可以定义一些简单的访问器来从无限流中提取数据：

```haskell
head :: Stream a -> a
head (In (StreamF a _)) = a

tail :: Stream a -> Stream a
tail (In (StreamF _ s)) = s
```

- `head` 用于获取流的第一个元素。
- `tail` 用于获取流的剩余部分（即去掉头部后的流）。

这些访问器是惰性的，只有当我们使用它们时，才会逐步展开和计算流中的值。

#### **总结**

通过 Haskell 的惰性求值和 `anamorphism`，我们可以轻松地定义和操作无限数据结构，如无限流。`ana` 函数帮助我们递归地生成结构，而 Haskell 的惰性特性确保我们只在需要时计算数据。

### **练习：**

1. 使用 `ana` 定义一个生成斐波那契数列的无限流，并通过 `head` 和 `tail` 访问其元素。
2. 思考如何将 `ana` 与 `cata` 结合来对无限数据结构进行处理，如生成并操作无限列表、树等。

### -------------------------

### 13.4 合态射（Hylomorphisms）

合态射（Hylomorphism）是代数（Algebra）和余代数（Coalgebra）结合在一起形成的一个递归模式。在这一节，我们将探讨如何通过合态射来组合递归展开和折叠操作。

#### anamorphism 与 catamorphism

首先，我们需要回顾两个重要的概念：

- **anamorphism**：是一种生成递归数据结构的方式。它的输出类型是一个函子的固定点，表示通过展开生成递归结构。
- **catamorphism**：是将递归数据结构折叠成某个值的方式。它的输入类型是一个函子的固定点，表示将递归结构折叠到一个特定的值。

在 Haskell 中，递归结构的固定点通常由 `Fix f` 类型描述，`Fix` 是一个递归类型构造器，用来表示数据结构的最小固定点。因此，我们可以将 anamorphism 和 catamorphism 组合在一起，这种组合的递归模式称为 **hylomorphism**。

#### Hylomorphism 的定义

在 Haskell 中，hylomorphism 可以通过以下函数定义：

```haskell
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coa = alg . fmap (hylo alg coa) . coa
```

- **`Algebra f b`**：代数，表示将一个函子 `f` 映射到某个类型 `b` 的函数。它对应的是折叠操作，将递归结构压缩成 `b` 类型的值。
- **`Coalgebra f a`**：余代数，表示将某个类型 `a` 展开为一个函子 `f` 的结构。它对应的是展开操作，从种子 `a` 生成递归结构。
- **`hylo alg coa a`**：使用代数 `alg` 和余代数 `coa`，将种子 `a` 展开为递归结构，并随后将其折叠为 `b` 类型的值。

函数 `hylo` 的工作原理如下：
1. 首先通过 `coa` 将输入 `a` 展开为递归结构。
2. 对这个递归结构的每一部分进行递归，继续展开并折叠。
3. 最终通过 `alg` 将递归结构折叠成结果值 `b`。

通过这种组合方式，我们能够避免手动处理展开和折叠的逻辑，直接使用合态射实现递归结构的生成和处理。

#### 快速排序（Quicksort）中的应用

我们可以将经典的快速排序算法用 `hylomorphism` 表达。快速排序的本质是递归地分解数据，并最终将其排序。我们可以定义一个合态射来完成这一过程：

```haskell
qsort = hylo toList split
```

在这个定义中：
- `toList` 是一个代数（Algebra），它将递归结构折叠为一个有序的列表。
- `split` 是一个余代数（Coalgebra），它通过将列表分解为子列表来展开递归结构。

通过这种方式，快速排序的逻辑变得非常简洁，递归展开与折叠的控制交由 `hylo` 来管理。

#### 惰性求值与内存优化

在 Haskell 中，得益于惰性求值（Lazy Evaluation），中间的递归数据结构并不会在内存中完全实体化。这意味着，即使递归展开生成了一个非常大的中间结构，Haskell 也不会立刻评估它。只有当前正在处理的部分会被计算，其余部分会等到需要时才展开。这种机制可以大大减少内存开销，特别是在处理大规模递归结构时，避免了大量的中间数据占用内存。

此外，Haskell 的垃圾回收机制能够在递归处理完成后，及时清除不再需要的中间数据，进一步提高了效率。

#### 递归回溯算法的替代方案

在命令式编程语言中，实现递归回溯算法往往非常复杂，尤其是需要仔细跟踪递归过程中每个步骤的状态。而在 Haskell 中，借助合态射和惰性求值，我们可以用递归数据结构来代替手动管理控制流，从而大大简化了算法的实现。通过合态射，我们将数据结构视为控制流的一部分，递归的展开和折叠过程可以清晰地映射到数据结构的生成和处理上。

#### 结论

合态射将余代数和代数结合在一起，使得我们能够通过组合展开和折叠操作来实现复杂的递归算法。Haskell 的惰性求值和内存管理机制进一步增强了这种模式的实用性，使得即使在处理大规模递归结构时，也能保持高效的内存使用。

### -------------------------

### 阻抗不匹配

**阻抗不匹配**（Impedance Mismatch）是指在不同的抽象层次或模型之间，存在不兼容的部分，这些部分使得它们的结合变得困难或不自然。在计算机科学中，这通常指的是不同的模型或概念之间的差异，尤其是数学模型与编程模型之间的差异。

#### 初代数与终余代数

在集合范畴（Set）中，初代数和终余代数的关系可能并不总是直观的。例如，恒等函子具有空集作为其初代数的载体，而具有单集合作为其终余代数的载体。这种情况表明，在 Set 中，初代数可能是终余代数的子集，合态射（morphisms）只能定义在这个子集中。这意味着我们只能在特定的余代数的 anamorphism 落入这个子集时使用合态射。

#### 流函子与合态射

流函子（Stream Functor）是没有叶节点成分的函子。这样的函子的初代数也是空集。比如，流代数 `add` 的定义如下：

```haskell
add :: Algebra (StreamF Int) Int
add (StreamF n sum) = n + sum
```

可以用合态射（hylo）来计算所有自然数的和：

```haskell
sumAllNats :: Int
sumAllNats = hylo add step 1
```

在 Haskell 中，`sumAllNats` 是一个完全有效的程序，并且通过了类型检查。然而，运行这个程序时会生成什么值呢？实际上，由于这个程序会进入无限递归，并最终耗尽计算机资源，它不会终止。这个例子展示了现实计算中，某些计算可能永远不会终止的问题。

#### 域理论与底值

在形式上，递归函数通过 **域理论**（Domain Theory）来描述为部分定义函数的极限。如果函数未对特定的参数值定义，则该函数被认为返回一个底值 ⊥。如果我们将底值作为每种类型的特殊元素（这些类型称为**提升类型**（lifted types）），我们可以说 `sumAllNats` 返回了一个 `Int` 类型的底值。一般来说，针对无限类型的 catamorphisms 不会终止，因此我们可以将它们视为返回底值。

#### Haskell 的范畴解释

引入底值的概念增加了 Haskell 的范畴解释的复杂性。特别是，许多依赖于唯一性的映射的泛化构造不再如预期那样工作。因此，Haskell 代码应该被视为范畴概念的一个说明，而不是严格证明的来源。

### -------------------------

### 13.5 从泛性质推导的终余代数（Terminal Coalgebra from Universality）

#### 异态射与终余代数

在 Haskell 中，**anamorphism** 是递归生成结构的一种方式，它基于余代数的构造。我们可以把 **anamorphism** 的定义看作是 **终余代数** 的一种表达方式。

##### 异态射的定义

异态射是从种子生成递归结构的函数。它的签名为：

```haskell
ana :: Functor f => forall a. Coalgebra f a -> (a -> Fix f)
```

其中：
- `Functor f` 限制了 `f` 必须是函子。
- `Coalgebra f a` 是一个余代数，定义了如何从类型 `a` 生成 `f a`，即从某个种子开始逐步生成递归结构。
- `Fix f` 是递归结构的最小固定点。

函数 `ana` 的具体定义如下：

```haskell
ana coa = In . fmap (ana coa) . coa
```

解释：
- `coa` 是余代数，它将种子 `a` 映射到 `f a`，表示递归展开的步骤。
- `fmap (ana coa)` 表示递归地将 `ana` 应用于余代数的结果，生成整个结构。
- `In` 是将递归结构的生成过程封装到固定点 `Fix f` 中。

这意味着，对于任何余代数 `coa`，我们可以从种子 `a` 开始生成一个递归结构。这个生成过程实际上构造了从 `a` 到终余代数的一个映射，这也是由 **Lambek 引理**（Lambek's Lemma）所保证的。

##### 参数展开后的异态射

我们可以对 `ana` 的参数进行展开，以便更清晰地理解其工作原理：

```haskell
ana :: Functor f => forall a. (a -> f a, a) -> Fix f
ana (coa, x) = In (fmap (curry ana coa) (coa x))
```

在这个展开版本中：
- `a -> f a` 是余代数的核心，它定义了如何从类型 `a` 生成 `f a`。
- `x` 是初始种子。
- `fmap (curry ana coa)` 是递归地将 `ana` 应用于生成的每个部分。
- `In` 将生成的部分封装为固定点。

这个公式展示了如何使用余代数的生成过程，逐步构造出递归结构。

#### 定义终余代数 Nu f

我们可以将这个递归生成的固定点类型 `Fix f` 替换为一个新的类型，称为 **Nu f**，并使用它来定义终余代数。

##### 终余代数的类型签名

终余代数 `Nu f` 的类型签名如下：

```haskell
forall a. (a -> f a, a) -> Nu f
```

这意味着我们可以通过一个 `a -> f a` 函数和一个种子 `a`，构造出终余代数 `Nu f` 的一个元素。这个过程看起来就像是数据的构造器。

##### 存在类型（Existential Types）

具有多态构造器的数据类型称为 **存在类型**（Existential Types）。存在类型允许我们构造出一种类型的元素，但不让外部知道其内部的具体类型。换句话说，外部用户只知道存在某种类型，但无法得知具体是什么类型。

在伪代码中（非实际的 Haskell），我们可以定义 `Nu f` 为：

```haskell
data Nu f = Nu (exists a. (Coalgebra f a, a))
```

这意味着 `Nu f` 是一个包装了某个存在类型 `a` 的数据类型，它携带了一个余代数 `a -> f a` 和一个种子 `a`。

##### 最小不动点代数的定义对比

我们可以将 `Nu f` 的定义与 **最小不动点代数** `Mu f` 进行比较：

```haskell
data Mu f = Mu (forall a. Algebra f a -> a)
```

这里，`Mu f` 表示代数的最小不动点，其类型携带了代数 `Algebra f a -> a`。`Mu f` 定义的是通过折叠生成的结构，而 `Nu f` 定义的则是通过展开生成的递归结构。

#### 实例：构造一个无限整数流

我们可以选择一个方便的类型，例如 `Int`，并构造出类型 `Nu (StreamF Int)` 的一个项：

```haskell
nuArgs :: (Int -> StreamF Int Int, Int)
nuArgs = (\n -> StreamF n (n+1), 0)
```

在这个例子中，我们使用 `Int` 作为种子类型，并定义了如何从一个整数生成 `StreamF`（表示一个无限整数流）。具体地说，`StreamF` 是一个递归结构，表示每个元素及其后续元素的生成规则。

#### 终余代数的访问与操作

为了操作 `Nu f` 类型，我们可以定义一些访问函数，例如提取流的第一个元素（`head`）或推进流到下一个元素（`tail`）。

##### 提取第一个元素

```haskell
head :: Nu (StreamF a) -> a
head (Nu (unf, s)) =
  let (StreamF a _) = unf s
  in a
```

这里，`head` 函数提取 `StreamF` 中的第一个元素。

##### 推进流

```haskell
tail :: Nu (StreamF a) -> Nu (StreamF a)
tail (Nu (unf, s)) =
  let (StreamF _ s') = unf s
  in Nu (unf, s')
```

`tail` 函数用于推进流，生成流中的下一个元素。

##### 测试

你可以通过以下代码在无限整数流上测试这些函数：

```haskell
allNats = Nu nuArgs
```

`allNats` 是一个无限的整数流，我们可以通过 `head` 和 `tail` 函数访问流中的元素。

#### 总结

在这一节中，我们定义了 **终余代数** `Nu f`，它表示通过余代数递归生成的结构。终余代数和存在类型密切相关，它允许我们从一个种子生成递归数据结构。Haskell 提供了几种方式来定义和使用存在类型，它们在递归生成无限数据流等场景中非常有用。

通过将余代数（展开）与代数（折叠）结合在一起，我们可以高效地操作递归结构，而无需显式管理控制流或中间数据。


### -------------------------

### 13.6 极限表示的终余代数（Terminal Coalgebra as a Limit）

在范畴论中，我们擅长处理无限结构，并使它们具备数学意义。在本节中，我们将探讨如何通过极限（limit）的形式表示终余代数。这个过程涉及构造无限的递归结构，并用极限将它们结合起来。

#### 终余代数和递归不动点

设想我们有一个函子 $F$，我们可以将它无限次应用于某个对象。例如，考虑从终对象 $1$ 开始反复应用函子 $F$。看起来，这种反复应用并不会改变结果，因为 "在无限上再加一" 仍然是无限的，这似乎让 $F$ 的这种递归应用形成了一个 **不动点**。

以更加正式的方式来看，$F$ 的不动点是指存在某个对象 $X$，使得 $F(X) \cong X$。然而，如果 $X$ 是通过将 $F$ 不断作用于终对象 $1$ 得到的，那么我们需要建立一种严格的数学方式来定义这个对象。这是我们引入极限的地方。

#### 极限与无限流

为了更好地理解，我们考虑一个具体的例子：**乘积函子** $F(a) = a \times \_ $。我们将演示如何通过极限来构造其终余代数，这将导致生成一个无限流的结构。

1. **初始对象**：从终对象 $1$ 开始。我们可以将其看作流的起点。
   - $F(1) = a \times 1 \cong a$
   - 这可以表示为一个长度为 1 的流。
   
2. **递归展开**：我们继续递归地应用 $F$。
   - $F(a \times 1) = a \times (a \times 1) \cong a \times a$
   - 这表示一个长度为 2 的流。

3. **进一步展开**：继续这个过程，我们可以构造出更长的流。
   - $F(a \times a) = a \times (a \times a) \cong a \times a \times a$
   - 表示一个长度为 3 的流。

通过这个过程，我们逐步生成越来越长的流。但要构造一个完整的终余代数，我们需要一种方法将所有这些递归展开的近似值组合在一起。

#### 极限的构造

我们引入极限来将这些递归步骤“粘合”在一起。考虑到范畴论中的 **行走箭头图**，这个图中的极限为我们提供了将递归结果组合为一个整体的方式。

##### 单箭头图的极限

首先，考虑一个简单的单箭头图，$D_1$：

$$
L = \lim D_1 = \lim (1 \to F(1))
$$

这个极限会产生与 $F(1)$ 相同的元素。因此，$L$ 可以被看作是我们构造终余代数的一个初始近似。

##### 双箭头图的极限

接着，我们考虑扩展到双箭头图 $D_2$：

$$
L = \lim D_2 = \lim (1 \to F(1) \to F(F(1)))
$$

这个极限生成与 $F(F(1))$ 相同的元素，再次推进了我们的近似。

##### 无限链的极限

我们可以继续扩展这个过程，将图无限次扩展。考虑一个无限链：

$$
1 \to F(1) \to F(F(1)) \to \dots \to F^n(1) \to \dots
$$

这个无限链的极限将产生 $F$ 的一个固定点，也就是我们所需的终余代数的载体。

#### 极限对应终余代数的固定点

最终，通过无限次应用 $F$ 并结合极限，我们得到了终余代数的固定点对象 $X$，其满足：

$$
F(X) \cong X
$$

这一点可以通过与初代数的类似过程进行对比。初代数的固定点通过 **余极限**（colimit）构造，而终余代数的固定点则是通过 **极限** 构造。这些不动点实际上是通过反向箭头证明的。

#### 总结

通过范畴论中的极限，我们可以为终余代数定义一个不动点对象。这个过程涉及将函子 $F$ 无限次应用于某个初始对象，并通过极限将这些递归步骤组合起来。最终，我们得到了一个满足 $F(X) \cong X$ 的对象 $X$，这就是我们所求的终余代数。

这种构造方式为递归定义和操作无限数据结构提供了一个形式化的基础，在处理无限对象时非常有用。

### ------------------

