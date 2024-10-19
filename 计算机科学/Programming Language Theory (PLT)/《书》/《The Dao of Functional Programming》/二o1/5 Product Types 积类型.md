[toc]



# 第 5 章：积类型（Product Types）

在本章中，我们将深入探讨 **积类型（Product Types）**，这是函数式编程和范畴论中的一个重要概念。积类型允许我们将多个类型组合在一起，以构建更复杂的数据结构。这在编码和处理数据时非常有用。

---

## 5.1 积类型的引入

### 组合对象的需求

在之前的章节中，我们学习了如何使用和类型（Sum Types）枚举给定类型的可能值。例如，我们可以使用和类型来表示十进制数字：

```haskell
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
```

然而，当我们需要表示更大范围的数字时，单纯依靠和类型可能变得低效。为了表示两位数的数字，我们需要定义 100 个构造器，这显然是不现实的。

### 使用积类型来组合类型

为了解决这个问题，我们可以将两个数字组合成一个数据结构，即一个 **两位数的十进制数**。这样，我们只需要使用两个数字（每个数字有 10 个可能的值）来表示 100 个不同的数字。这体现了组合类型的力量。

正如老子所说：

> 只需四位数字就可以编码一万个数字。

这意味着，通过组合多个类型，我们可以有效地表示大量的数据。

### 积类型的定义

这种组合两种类型的数据类型称为 **积类型（Product Type）**，或 **笛卡尔积（Cartesian Product）**。积类型的核心思想是将两个类型组合成一个新的类型，其中包含来自原始类型的所有组合。

#### 消除规则（Elimination Rules）

积类型的定义特性是 **消除规则**，即我们可以通过 **投影（Projection）** 将积类型的值分解为其组成部分。

- 对于类型 $a \times b$，我们有两个投影函数：
  - $fst$（First）：从 $a \times b$ 提取第一个元素，得到类型 $a$。
  - $snd$（Second）：从 $a \times b$ 提取第二个元素，得到类型 $b$。

下面是对应的图示：

$$
\begin{tikzcd}
& a \times b \arrow[dl, "fst"'] \arrow[dr, "snd"] \\
a && b
\end{tikzcd}
$$

#### 构造积类型的值

假设有人给了你一个积类型的元素，即一个从终端对象 $1$ 到 $a \times b$ 的箭头 $h$。你可以通过组合轻松检索一对元素：

- 一个来自 $a$ 的元素：

  $$
  x = fst \circ h
  $$

- 一个来自 $b$ 的元素：

  $$
  y = snd \circ h
  $$

图示如下：

$$
\begin{tikzcd}
& 1 \arrow[d, "h"] \\
& a \times b \arrow[dl, "fst"'] \arrow[dr, "snd"] \\
a && b
\end{tikzcd}
$$

这意味着，给定一个积类型的值，我们可以使用 $fst$ 和 $snd$ 将其分解为原始的两个部分。

#### 一般化的映射

实际上，给定一个从任意对象 $c$ 到 $a \times b$ 的箭头（函数），我们可以通过组合定义一对箭头：

- $f : c \to a$
- $g : c \to b$

图示如下：

$$
\begin{tikzcd}
& c \arrow[d, "h"] \\
& a \times b \arrow[dl, "fst"'] \arrow[dr, "snd"] \\
a && b
\end{tikzcd}
$$

这表示，从 $c$ 到 $a \times b$ 的映射 $h$，可以通过一对映射 $(f, g)$ 来唯一确定。

#### 引入规则（Introduction Rules）

正如我们之前对和类型所做的那样，我们可以反过来使用这个图来定义积类型：我们规定一个函数对 $(f, g)$ 与从 $c$ 到 $a \times b$ 的映射 $h$ 是一一对应的关系。这是积类型的 **引入规则**。

---

## 5.2 在 Haskell 中定义积类型

### 使用元组表示积类型

在 Haskell 中，积类型通常使用 **元组（Tuple）** 来表示。给定两个类型 $A$ 和 $B$，它们的积类型表示为 $(A, B)$。

构造积类型的值也很简单。给定两个元素：

- $a :: A$
- $b :: B$

我们可以构造积类型的值：

```haskell
(a, b) :: (A, B)
```

内置的元组语法就是这样：一对括号和中间的逗号。它既适用于定义两个类型的积 $(A, B)$，也适用于数据构造器 $(a, b)$，它接受两个元素并将它们配对在一起。

### 示例

假设我们有以下两个值：

```haskell
a :: Int
a = 5

b :: Char
b = 'x'
```

我们可以构造它们的积类型的值：

```haskell
pair :: (Int, Char)
pair = (a, b)  -- pair = (5, 'x')
```

### 访问积类型的元素

我们可以使用内置的函数 $fst$ 和 $snd$ 来访问元组的元素：

```haskell
fst :: (a, b) -> a
snd :: (a, b) -> b
```

例如：

```haskell
firstElement = fst pair  -- firstElement = 5
secondElement = snd pair  -- secondElement = 'x'
```

### 编程的目的

我们应该始终牢记编程的目的：**将复杂问题分解为一系列简单的问题**。

在积类型的定义中，我们再次看到这一点。每当我们必须构造一个进入积类型的映射时，我们将其分解为两个较小的任务，即构造一个函数对，每个函数映射到积的一个组成部分。这就像说，为了实现一个返回一对值的函数，足以实现两个函数，每个函数返回对中一个元素。

---

## 5.3 积类型与逻辑

### 逻辑对应

在逻辑中，积类型对应于 **逻辑合取（Conjunction）**。也就是说，命题 $A \wedge B$ 表示 $A$ 和 $B$ 同时为真。

为了证明 $A \wedge B$，你需要同时提供 $A$ 和 $B$ 的证明。这对应于积类型中的两个投影：

- 从 $A \times B$ 可以得到 $A$（通过 $fst$）。
- 从 $A \times B$ 可以得到 $B$（通过 $snd$）。

### 消除规则

消除规则表明，如果你有 $A \times B$ 的证明（即一个积类型的值），那么你自动获得 $A$ 的证明（通过 $fst$）和 $B$ 的证明（通过 $snd$）。

这与和类型对应于逻辑析取（Disjunction）相对应。

---

## 5.4 元组和记录

### 元组

在 Haskell 中，我们可以使用元组表示任意多个类型的积。例如，三种类型的积写作 $(A, B, C)$。该类型的一个值可以由三个元素构造：

```haskell
(a, b, c) :: (A, B, C)
```

### 特殊情况：零元组

在数学中，有时我们会使用 **符号滥用（Abuse of Notation）**。零个类型的积写作 $()$，一个 **空元组**，它恰好与终端对象或单位类型相同。这是因为积的行为非常像数字的乘法，终端对象扮演了“一”的角色。

在 Haskell 中，$()$ 表示单位类型，只有一个值，也写作 $()$。

### 模式匹配

在 Haskell 中，与其为所有元组定义单独的投影，我们使用 **模式匹配（Pattern Matching）** 语法。

例如，要从三元组中提取第三个组件，我们可以这样写：

```haskell
third :: (a, b, c) -> c
third (_, _, c) = c
```

在这个函数中，我们对想忽略的组件使用通配符 $\\_$，只提取我们感兴趣的部分。

### 记录（Records）

老子说：

> 命名是所有特殊事物的起源。

在编程中，如果不给它们命名，跟踪特定元组中组件的含义是困难的。**记录语法** 允许我们为投影命名，使代码更具可读性和可维护性。

#### 记录的定义

这是以记录方式编写的积类型的定义：

```haskell
data Product a b = Pair { fst :: a, snd :: b }
```

在这个定义中：

- $Pair$ 是数据构造器，用于构造 $Product$ 类型的值。
- $fst$ 和 $snd$ 是投影函数，用于从 $Product$ 类型的值中提取组成部分。

#### 使用记录

这是如何声明和初始化特定对的：

```haskell
ic :: Product Int Char
ic = Pair { fst = 10, snd = 'A' }
```

或者更简单地：

```haskell
ic = Pair 10 'A'
```

然后，我们可以直接使用命名的字段来访问值：

```haskell
number = fst ic  -- number = 10
character = snd ic  -- character = 'A'
```

记录使得代码更加清晰，因为我们为每个字段都提供了有意义的名称。

---

## 5.5 笛卡尔范畴（Cartesian Category）

### 定义

在 Haskell 中，我们可以定义任意两种类型的积。具有所有积和终端对象的范畴称为 **笛卡尔范畴（Cartesian Category）**。

### 元组算术

积类型满足一些重要的恒等式，这些恒等式可以使用映射的性质导出。例如，要证明 $a \times b \cong b \times a$，我们可以考虑以下两个图：

#### 图示 1

$$
\begin{tikzcd}
& x \arrow[d, "h"] \\
& a \times b \arrow[dl, "fst"'] \arrow[dr, "snd"] \\
a && b
\end{tikzcd}
$$

#### 图示 2

$$
\begin{tikzcd}
& x \arrow[d, "h'"] \\
& b \times a \arrow[dl, "fst"'] \arrow[dr, "snd"] \\
b && a
\end{tikzcd}
$$

它们显示，对于任何对象 $x$，到 $a \times b$ 的箭头与到 $b \times a$ 的箭头是一一对应的。这是因为这些箭头中的每一个都由相同的函数对 $(f, g)$ 确定，只是顺序不同。

### 自然性条件

你可以检查自然性条件是否得到了满足，因为当你使用箭头 $k : x' \to x$ 切换焦点时，所有起始于 $x$ 的箭头都通过前组合（$h \circ k$）来转换。

### 在 Haskell 中实现同构

这个同构可以实现为一个函数，它是其自身的逆：

```haskell
swap :: (a, b) -> (b, a)
swap x = (snd x, fst x)
```

或者使用模式匹配：

```haskell
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
```

#### 注意事项

重要的是要记住，积类型仅在 **同构意义上** 是对称的。这并不意味着交换对的顺序不会改变程序的行为。对称性意味着交换对的信息内容相同，但访问它需要进行修改。

---

## 5.6 单位元与同构

### 左单位元

终端对象（单位类型）是积的单位元素，即 $1 \times a \cong a$。见证 $1 \times a$ 和 $a$ 之间同构的箭头称为 **左单位元（Left Unit）**：

- $\lambda : 1 \times a \to a$

它可以实现为：

```haskell
lambda :: ((), a) -> a
lambda (_, a) = a  -- 相当于 snd
```

它的逆 $\lambda^{-1}$ 定义为以下图中的唯一箭头：

$$
\begin{tikzcd}
& a \arrow[d, "\lambda^{-1}"] \\
& 1 \times a \arrow[dl, "fst"'] \arrow[dr, "snd"] \\
1 && a
\end{tikzcd}
$$

从 $a$ 到 $1$ 的箭头称为 $!$（读作 "bang"）。在 Haskell 中，这是从 $a$ 到 $()$ 的函数：

```haskell
bang :: a -> ()
bang _ = ()
```

#### 验证同构

我们需要验证：

- $snd \circ \lambda^{-1} = id$

以及

- $\lambda^{-1} \circ snd = id$

这表明 $\lambda^{-1}$ 是 $snd$ 的左逆。

#### 左单位律的证明

考虑以下图：

$$
\begin{tikzcd}
& 1 \times a \arrow[d, "h"] \\
& 1 \times a \arrow[dl, "fst"'] \arrow[dr, "snd"] \\
1 && a
\end{tikzcd}
$$

对于 $h = id$，这个图是交换的。对于 $h = \lambda^{-1} \circ snd$，这个图也是交换的，因为：

$$
snd \circ \lambda^{-1} \circ snd = snd
$$

由于 $h$ 应该是唯一的，我们得出结论：

$$
\lambda^{-1} \circ snd = id
$$

#### 练习 1

**练习 5.1.1**：证明左单位证明中的双射是自然的。提示：使用箭头 $g : a \to b$ 来更改焦点。

**解答**：

我们需要证明自然性，即以下图表交换：

$$
\begin{tikzcd}
1 \times a \arrow[r, "\lambda"] \arrow[d, "id \times g"'] & a \arrow[d, "g"] \\
1 \times b \arrow[r, "\lambda"] & b
\end{tikzcd}
$$

其中：

- $id \times g : 1 \times a \to 1 \times b$，在 Haskell 中为：

  ```haskell
  id_g :: ((), a) -> ((), b)
  id_g (u, a) = (u, g a)
  ```

- $\lambda : 1 \times a \to a$，为 $snd$。

验证左下角到右上角的两种路径得到相同的结果，即：

$$
g \circ \lambda = \lambda \circ (id \times g)
$$

因此，双射 $\lambda$ 是自然的。

---

### 其他同构

以下是用 Haskell 编写的其他一些同构（没有证明逆）：

#### 结合律

```haskell
assoc :: ((a, b), c) -> (a, (b, c))
assoc ((a, b), c) = (a, (b, c))
```

#### 右单位元

```haskell
runit :: (a, ()) -> a
runit (a, _) = a  -- 相当于 fst
```

这些函数对应于结合子 $\alpha$ 和右单位元 $\rho$：

- $\alpha : (a \times b) \times c \to a \times (b \times c)$
- $\rho : a \times 1 \to a$

---

## 5.7 函子性（Functoriality）

### 提升箭头

假设我们有箭头将 $a$ 和 $b$ 映射到某个 $a'$ 和 $b'$：

- $f : a \to a'$
- $g : b \to b'$

这些箭头与投影 $fst$ 和 $snd$ 的组合分别用于定义积之间的映射 $h$。

图示如下：

$$
\begin{tikzcd}
& a \times b \arrow[dl, "fst"'] \arrow[dr, "snd"] \arrow[d, "h"] \\
a \arrow[d, "f"] && b \arrow[d, "g"] \\
a' && b' \\
& a' \times b' \arrow[ul, "fst"] \arrow[ur, "snd"'] \\
\end{tikzcd}
$$

其中 $h$ 由 $f$ 和 $g$ 唯一定义。简写符号是：

$$
a \times b \xrightarrow{f \times g} a' \times b'
$$

这种积类型的性质称为 **函子性（Functoriality）**。你可以想象它允许你在积类型内部转换两个对象以获得新的积类型。我们还说函子性允许我们 **提升** 一对箭头以操作积类型。

---

## 5.8 对偶性（Duality）

### 概念介绍

当一个孩子看到一个箭头时，它知道哪一端指向源，哪一端指向目标：

$$
a \to b
$$

但这也许只是一个先入之见。如果我们把 $b$ 称为源，$a$ 称为目标，宇宙会有很大不同吗？

### 对偶范畴

在对偶宇宙中，我们将箭头的方向反转，得到对偶范畴。对于每个箭头 $f : a \to b$，在对偶范畴中有一个箭头 $f^{op} : b \to a$。

### 和类型与积类型的对偶

现在考虑我们用来定义和类型的这个图：

$$
\begin{tikzcd}
& a \arrow[dr, "f"] \\
a + b \arrow[ur, "Left"] \arrow[dr, "Right"'] && c \\
& b \arrow[ur, "g"']
\end{tikzcd}
$$

在新的解释中，箭头 $h$ 将从任意对象 $c$ 到我们称为 $a + b$ 的对象。这个箭头由一对箭头 $(f, g)$ 唯一定义，其 **源** 是 $c$。

如果我们将 $Left$ 重命名为 $fst$，将 $Right$ 重命名为 $snd$，我们将得到积类型的定义图。

因此：

- **积类型是和类型的对偶**。
- **和类型是积类型的对偶**。

### 区别

虽然在范畴论中，每个构造都有其对偶，但在编程中，和类型和积类型的区别并不是仅仅在箭头的方向上。这种区别可以追溯到我们一开始做的一个假设：**没有指向初始对象的箭头（除身份箭头外）**。

这与终端对象有大量指向外部的箭头形成对比，我们用这些箭头来定义（全局）元素。事实上，我们假设每个感兴趣的对象都有元素，而没有元素的对象是与 $Void$ 同构的。

---

## 5.9 幺半范畴（Monoidal Category）

### 定义

我们已经看到积类型满足以下简单规则：

1. **左单位元**：$1 \times a \cong a$
2. **交换律**：$a \times b \cong b \times a$
3. **结合律**：$(a \times b) \times c \cong a \times (b \times c)$
4. **函子性**：能够提升箭头。

定义了具有这些属性的运算的范畴称为 **对称幺半范畴（Symmetric Monoidal Category）**。

### 一般化的符号

当你不想为你的幺半结构命名时，你可以用 **张量符号** $\otimes$ 代替加号或乘号，用字母 $I$ 代替中性元素。对称幺半范畴的规则可以写作：

1. **左单位元**：$I \otimes a \cong a$
2. **交换律**：$a \otimes b \cong b \otimes a$
3. **结合律**：$(a \otimes b) \otimes c \cong a \otimes (b \otimes c)$

这些同构通常写作称为 **结合子（Associator）** 和 **单位子（Unitors）**。

- 结合子 $\alpha : (a \otimes b) \otimes c \to a \otimes (b \otimes c)$
- 左单位子 $\lambda : I \otimes a \to a$
- 右单位子 $\rho : a \otimes I \to a$
- 对称性由箭头 $\gamma : a \otimes b \to b \otimes a$ 见证。

### 函子性

函子性让我们提升一对箭头：

- $f : a \to a'$
- $g : b \to b'$

以操作张量积：

$$
a \otimes b \xrightarrow{f \otimes g} a' \otimes b'
$$

---

## 5.10 幺半群（Monoids）

### 定义

**幺半群（Monoid）** 是非常简单的结构，具有二元运算和一个单位元素。以加法和零为运算的自然数构成一个幺半群。以乘法和一为运算的自然数也构成一个幺半群。

在编程中，我们可以在类型和函数的范畴内定义幺半群。

### 范畴中的幺半群

在范畴中，我们倾向于整体定义运算，而不是关注个别元素。因此，我们从一个对象 $m$ 开始。二元运算是一个有两个参数的函数。由于积类型的元素是成对的元素，我们可以将二元运算表征为从积 $m \times m$ 到 $m$ 的箭头：

- $\mu : m \times m \to m$

单位元素可以定义为从终端对象 $1$ 的箭头：

- $\eta : 1 \to m$

### 在 Haskell 中定义

我们可以直接将这种描述翻译为 Haskell，定义一个类型类，它配备了两个方法，传统上称为 $mappend$ 和 $mempty$：

```haskell
class Monoid m where
  mappend :: (m, m) -> m
  mempty :: () -> m
```

### 幺半群律

这两个箭头 $\mu$ 和 $\eta$ 必须满足 **幺半群律**，但我们需要在整体上表述它们，而不涉及元素。

#### 单位律

为了表述左单位律，我们需要以下图表：

$$
\begin{tikzcd}
& 1 \times m \arrow[d, "\eta \times id_m"] \arrow[ddl, "\lambda"'] \\
& m \times m \arrow[d, "\mu"] \\
m && m \arrow[ll, "id_m"']
\end{tikzcd}
$$

这里，$\lambda$ 是左单位子。

右单位律类似。

#### 结合律

为了表述结合律，我们必须从三重积开始，并在整体上操作：

$$
\begin{tikzcd}
& (m \times m) \times m \arrow[d, "\mu \times id_m"] \arrow[rr, "\alpha"] && m \times (m \times m) \arrow[d, "id_m \times \mu"] \\
& m \times m \arrow[dl, "\mu"'] && m \times m \arrow[dr, "\mu"] \\
m &&&& m
\end{tikzcd}
$$

其中，$\alpha$ 是结合子。

### 一般化到任意幺半范畴

上述定义同样适用于任意幺半范畴中的张量积。它甚至不需要是对称的。我们只需假设：

- 存在一个单位对象 $I$。
- 张量积是函子的，并且满足单位和结合律（最多同构）。

因此，在任意幺半范畴中，幺半群是一个配备两个态射的对象 $m$：

- $\mu : m \otimes m \to m$
- $\eta : I \to m$

满足单位和结合律：

- 左单位律
- 右单位律
- 结合律

---

## 5.11 练习

### 练习 5.1.2

**题目**：构造一个箭头

$$
h : b + (a \times b) \to (1 + a) \times b
$$

这个箭头是唯一的吗？

**提示**：这是映射到积的映射，所以它由一对箭头给出。这些箭头，反过来，从和类型中映射出来，因此每一个都是由一对箭头给出的。

**解答**：

我们需要构造函数：

```haskell
h :: Either b (a, b) -> (Either () a, b)
```

由于目标类型是积类型，我们可以将 $h$ 定义为一对函数：

1. $h_1 : Either b (a, b) \to Either () a$
2. $h_2 : Either b (a, b) \to b$

#### 定义 $h_1$

对于 $h_1$，我们需要从 $Either b (a, b)$ 到 $Either () a$ 的函数。

- 如果输入是 $Left\ b$，我们需要返回 $Left\ ()$。
- 如果输入是 $Right\ (a, b)$，我们需要返回 $Right\ a$。

定义如下：

```haskell
h1 :: Either b (a, b) -> Either () a
h1 (Left _)         = Left ()
h1 (Right (a, _))   = Right a
```

#### 定义 $h_2$

对于 $h_2$，我们需要从 $Either b (a, b)$ 到 $b$ 的函数。

- 如果输入是 $Left\ b$，我们返回 $b$。
- 如果输入是 $Right\ (a, b)$，我们返回 $b$。

定义如下：

```haskell
h2 :: Either b (a, b) -> b
h2 (Left b)        = b
h2 (Right (_, b))  = b
```

#### 组合成 $h$

最后，$h$ 可以定义为：

```haskell
h :: Either b (a, b) -> (Either () a, b)
h e = (h1 e, h2 e)
```

#### 唯一性

这个箭头不是唯一的。对于 $h_1$，我们可以在 $Left\ b$ 的情况下返回 $Right\ a$，但由于我们没有 $a$ 的值，这是不可能的。因此，$h_1$ 在 $Left\ b$ 的情况下只能返回 $Left\ ()$。

对于 $h_2$，定义是唯一的，因为我们需要返回一个 $b$。

因此，$h$ 基本上是唯一的。

---

### 练习 5.1.3

**题目**：重做上一个练习，这次将 $h$ 视为从和类型中映射出。

**解答**：

现在，我们将 $h$ 视为从和类型中映射出的函数，即：

$$
h : b + (a \times b) \to (1 + a) \times b
$$

我们可以定义 $h$ 为：

```haskell
h :: Either b (a, b) -> (Either () a, b)
h (Left b)         = (Left (), b)
h (Right (a, b))   = (Right a, b)
```

这与之前的定义一致。

---

### 练习 5.1.4

**题目**：实现一个 Haskell 函数：

```haskell
maybeAB :: Either b (a, b) -> (Maybe a, b)
```

这个函数是由其类型签名唯一定义的吗？还是有一些余地？

**解答**：

我们需要实现函数：

```haskell
maybeAB :: Either b (a, b) -> (Maybe a, b)
```

定义如下：

```haskell
maybeAB :: Either b (a, b) -> (Maybe a, b)
maybeAB (Left b)         = (Nothing, b)
maybeAB (Right (a, b))   = (Just a, b)
```

这个函数的定义是唯一的，因为：

- 当输入为 $Left\ b$ 时，我们只能构造 $Nothing$，因为没有 $a$ 的值可用。
- 当输入为 $Right\ (a, b)$ 时，我们可以构造 $Just\ a$。

因此，函数 $maybeAB$ 由其类型签名唯一确定。

---

# 总结

在本章中，我们深入探讨了积类型的概念，了解了如何在 Haskell 中使用元组和记录来表示积类型。我们还讨论了积类型在逻辑中的对应关系，以及积类型的性质和相关的范畴论概念。

通过练习和示例，我们进一步加深了对积类型的理解。这些概念在函数式编程中非常重要，有助于我们构建更复杂、更强大的数据结构和程序。

# 参考资料

- **Haskell 官方文档**：了解更多关于 Haskell 编程语言的信息。
- **范畴论入门**：进一步学习范畴论的基础知识。
- **函数式编程原理**：深入理解函数式编程的核心概念。

希望这些内容对你的学习有所帮助！

# 附录：练习的解答

**练习 5.1.1**：见上文。

**练习 5.1.2**：见上文。

**练习 5.1.3**：见上文。

**练习 5.1.4**：见上文。

---

如果你对任何概念还有疑问，欢迎继续提问！

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



### ---------------------------



### ----------------------------
