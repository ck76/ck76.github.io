[toc]



# 第 4 章：和类型（Sum Types）

在之前的章节中，我们了解了如何组合箭头（函数）。现在，我们要探讨如何组合对象（类型）。这将引导我们深入理解 **和类型**（Sum Types）的概念，以及它们在编程、逻辑和范畴论中的重要性。

## 4.1 布尔类型（Bool）

首先，让我们考虑一个简单的问题：在定义了初始对象 $0$ 和终端对象 $1$ 之后，$2$ 是什么？从数字的角度来看，$2$ 是 $1 + 1$。在类型的世界中，$2$ 可以被视为一个具有两个元素的对象。这意味着有两个从 $1$ 发出的箭头（函数）到这个对象。

我们可以将这两个箭头分别称为 $\text{True}$ 和 $\text{False}$。这两个箭头只是名称，不要将它们与逻辑上的真和假混淆。它们只是两个不同的箭头。

下面是对应的图示：

$$
\begin{tikzcd}
1
\arrow[dd, bend right, "\text{True}"']
\arrow[dd, bend left, "\text{False}"]
\\
\\
2
\end{tikzcd}
$$

在编程语言 Haskell 中，我们可以直接用代码表达这个概念。通常，我们定义一个类型，传统上称为 $\text{Bool}$，以纪念其发明者乔治·布尔（George Boole，1815-1864）。下面是它的定义：

```haskell
data Bool where
  True  :: () -> Bool
  False :: () -> Bool
```

这里，我们定义了一个数据类型 $\text{Bool}$，它有两个构造器 $\text{True}$ 和 $\text{False}$。它们分别是从单位类型 $\text{()}$ 到 $\text{Bool}$ 的函数。这个定义对应于上面的图，只是我们用 $\text{()}$ 替换了 $1$，用 $\text{Bool}$ 替换了 $2$。

我们可以简化这个定义，因为从单位类型到某个类型的函数实际上就是该类型的元素。因此，我们可以写成：

```haskell
data Bool where
  True  :: Bool
  False :: Bool
```

现在，$\text{True}$ 和 $\text{False}$ 直接就是 $\text{Bool}$ 类型的元素。

### 使用布尔类型

现在我们可以定义 $\text{Bool}$ 类型的一个值，例如：

```haskell
x :: Bool
x = True
```

第一行声明了 $\text{x}$ 是 $\text{Bool}$ 类型的一个值（实际上是一个从 $\text{()}$ 到 $\text{Bool}$ 的函数）。第二行告诉我们 $\text{x}$ 是两个可能的值中的哪个。

在 Haskell 中，$\text{True}$ 和 $\text{False}$ 被称为 **数据构造器**。它们用于构造特定的值。

但是，仅仅定义了 $\text{Bool}$ 类型还不够。我们需要知道如何 **使用** 它，也就是如何定义从 $\text{Bool}$ 到其他类型的箭头（函数）。这意味着，我们需要能够定义从 $\text{Bool}$ 到任意类型 $\text{A}$ 的函数。

### 从布尔到其他类型的函数

如果我们有一个从 $\text{Bool}$ 到某个类型 $\text{A}$ 的函数 $\text{h}$，那么我们可以通过组合 $\text{h}$ 与 $\text{True}$ 和 $\text{False}$，得到两个从 $\text{()}$ 到 $\text{A}$ 的箭头，也就是 $\text{A}$ 的两个元素。

以下是对应的图示：

$$
\begin{tikzcd}
\text{()}
\arrow[dd, bend right, "\text{True}"']
\arrow[dd, bend left, "\text{False}"]
\arrow[ddd, bend right=90, "x"']
\arrow[ddd, bend left=90, "y"]
\\
\\
\text{Bool}
\arrow[d, dashed, "h"]
\\
A
\end{tikzcd}
$$

换句话说，每个 $\text{Bool} \to \text{A}$ 的函数都会生成一对 $\text{A}$ 的元素。

给定一个具体类型 $\text{A}$：

```haskell
h :: Bool -> A
```

我们有：

```haskell
x = h True
y = h False
```

其中：

```haskell
x :: A
y :: A
```

这里，我们使用了将函数应用于元素的简写表示法。例如，$\text{h True}$ 表示 $\text{h} \circ \text{True}$。

### 反过来：由元素构造函数

不仅如此，一对元素 $(x, y)$ 也唯一地确定了从 $\text{Bool}$ 到 $\text{A}$ 的函数 $\text{h}$。换句话说，给定 $\text{A}$ 的两个元素，我们可以定义一个函数 $\text{h}$，使得：

```haskell
h :: Bool -> A
h True  = x
h False = y
```

这就建立了一个双射：元素对 $(x, y)$ 与函数 $h$ 之间存在一一对应的关系。

### Haskell 中的条件表达式

在 Haskell 中，我们通常使用条件表达式来定义这样的函数：

```haskell
h :: Bool -> A
h b = if b then x else y
```

这里，$\text{b}$ 是 $\text{Bool}$ 类型的一个值。这个定义表示，如果 $\text{b}$ 为 $\text{True}$，则返回 $\text{x}$；如果 $\text{b}$ 为 $\text{False}$，则返回 $\text{y}$。

### 引入规则和消除规则

一般来说，数据类型通过 **引入规则**（Introduction Rules）来创建，通过 **消除规则**（Elimination Rules）来解构。

- **引入规则**：定义如何构造数据类型的值。在 $\text{Bool}$ 类型中，$\text{True}$ 和 $\text{False}$ 就是引入规则。
- **消除规则**：定义如何使用数据类型的值。在 $\text{Bool}$ 类型中，条件表达式 $\text{if} \ldots \text{then} \ldots \text{else}$ 就是消除规则。

**计算规则**（Computation Rules）告诉我们如何计算函数的结果。例如，使用 $\text{h}$ 时，如果输入是 $\text{True}$，则结果是 $\text{x}$；如果输入是 $\text{False}$，则结果是 $\text{y}$。

### 编程的本质

编程的核心思想是将复杂的问题分解为更简单的问题。$\text{Bool}$ 的定义很好地体现了这一点。当我们需要构造一个从 $\text{Bool}$ 到某个类型的函数时，我们将其分解为构造目标类型的两个元素的任务。

## 4.2 枚举类型（Enumerations）

在 $0$、$1$ 和 $2$ 之后是什么？当然是 $3$。一个具有三个数据构造器的对象。例如，我们可以定义一个表示颜色的类型：

```haskell
data RGB where
  Red   :: RGB
  Green :: RGB
  Blue  :: RGB
```

或者使用简写的语法：

```haskell
data RGB = Red | Green | Blue
```

这里，$\text{RGB}$ 是一个枚举类型，具有三个可能的值：$\text{Red}$、$\text{Green}$ 和 $\text{Blue}$。

我们可以定义一个 $\text{RGB}$ 类型的值：

```haskell
c :: RGB
c = Blue
```

### 从枚举类型到其他类型的函数

要定义从 $\text{RGB}$ 到其他类型的函数，我们需要更通用的消除模式。就像从 $\text{Bool}$ 到某个类型的函数由两个元素决定一样，从 $\text{RGB}$ 到 $\text{A}$ 的函数由 $\text{A}$ 的三个元素 $\text{x}$、$\text{y}$、$\text{z}$ 决定。

使用模式匹配，我们可以编写这样的函数：

```haskell
h :: RGB -> A
h Red   = x
h Green = y
h Blue  = z
```

这表示 $\text{h}$ 是一个从 $\text{RGB}$ 到 $\text{A}$ 的函数，根据输入的不同值，返回不同的结果。

### 使用模式匹配和条件表达式

我们也可以对 $\text{Bool}$ 类型使用模式匹配：

```haskell
h :: Bool -> A
h True  = x
h False = y
```

或者使用 $\text{case}$ 表达式：

```haskell
h b = case b of
  True  -> x
  False -> y
```

对于 $\text{RGB}$ 类型：

```haskell
h c = case c of
  Red   -> x
  Green -> y
  Blue  -> z
```

编程时，可以根据需要选择使用模式匹配或条件表达式。

### 更多的枚举类型

这些模式也适用于具有四个、五个或更多数据构造器的类型。例如，十进制数字可以定义为：

```haskell
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
```

Haskell 中的 $\text{Char}$ 类型也是一个巨大的枚举类型，包含所有的 Unicode 字符。字符字面量使用单引号，例如：

```haskell
c :: Char
c = 'a'
```

当需要匹配特定字符时，可以使用模式匹配：

```haskell
isVowel :: Char -> Bool
isVowel c = case c of
  'a' -> True
  'e' -> True
  'i' -> True
  'o' -> True
  'u' -> True
  _   -> False
```

其中下划线 $\_$ 表示通配符模式，匹配所有未被前面模式匹配到的情况。

对于大量的枚举值，例如 $\text{Int}$ 类型（整数），虽然在实践中不可能对每个值进行模式匹配，但原则上仍然适用。

## 4.3 和类型（Sum Types）

$\text{Bool}$ 类型可以看作是和类型 $2 = 1 + 1$ 的一个特例。但没有什么能阻止我们用其他类型替换这两个 $1$，甚至用不同的类型替换它们。我们可以使用两个箭头定义一个新类型 $a + b$，我们称它们为 $\text{Left}$ 和 $\text{Right}$。

下面是对应的图示：

$$
\begin{tikzcd}
a
\arrow[dr, "\text{Left}"']
&& b
\arrow[dl, "\text{Right}"]
\\
& a + b
\end{tikzcd}
$$

在 Haskell 中，类型 $a + b$ 被称为 $\text{Either a b}$。它的定义是：

```haskell
data Either a b where
  Left  :: a -> Either a b
  Right :: b -> Either a b
```

这里，$\text{Left}$ 和 $\text{Right}$ 是数据构造器，它们分别接受类型 $a$ 和 $b$ 的值，返回 $\text{Either a b}$ 类型的值。

### 从和类型到其他类型的函数

从 $a + b$ 到某个类型 $c$ 的函数由以下图示决定：

$$
\begin{tikzcd}
a
\arrow[dr, bend left, "\text{Left}"']
\arrow[ddr, bend right, "f"']
&& b
\arrow[dl, bend right, "\text{Right}"]
\arrow[ddl, bend left, "g"]
\\
& a + b
\arrow[d, dashed, "h"]
\\
& c
\end{tikzcd}
$$

给定一个函数 $h$，我们可以通过组合 $\text{h} \circ \text{Left}$ 和 $\text{h} \circ \text{Right}$ 得到两个函数 $f$ 和 $g$。反过来，这对函数 $(f, g)$ 唯一地决定了 $h$。这就是和类型的消除规则。

当将这个图转换为 Haskell 代码时，我们可以写成：

```haskell
h :: Either a b -> c
h (Left  a) = f a
h (Right b) = g b
```

这里，我们使用模式匹配，根据输入是 $\text{Left a}$ 还是 $\text{Right b}$，分别调用 $f$ 或 $g$。

### 和类型的本质

因此，和类型的本质是将一个类型的值分为多个可能的情况，每种情况对应一个数据构造器。使用消除规则，我们可以对不同的情况分别处理。

## 4.4 可能类型（Maybe）

一个非常有用的和类型是 $\text{Maybe a}$，它可以看作是 $1 + a$，即一个类型 $a$ 加上一个特殊的情况 $\text{Nothing}$。它的定义是：

```haskell
data Maybe a where
  Nothing :: () -> Maybe a
  Just    ::  a -> Maybe a
```

这里，$\text{Nothing}$ 是从单位类型 $\text{()}$ 到 $\text{Maybe a}$ 的函数，表示没有值。$\text{Just}$ 是从 $a$ 到 $\text{Maybe a}$ 的函数，表示有一个值。

简写形式为：

```haskell
data Maybe a = Nothing | Just a
```

### 可能类型的用途

$\text{Maybe}$ 类型主要用于处理部分定义的函数。当函数在某些输入上没有合理的输出时，可以返回 $\text{Nothing}$，而不是引发异常或程序崩溃。

例如，定义一个安全的除法函数：

```haskell
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)
```

## 4.5 逻辑中的和类型

在逻辑中，和类型对应于 **逻辑或**（Disjunction）。命题 $A + B$ 表示 $A$ 或 $B$ 为真。你可以通过证明 $A$ 或证明 $B$ 来证明 $A + B$。

如果你想证明 $C$ 从 $A + B$ 推导出来，你必须为两种可能性提供证明：

1. 当 $A$ 为真时，如何从 $A$ 推导出 $C$。
2. 当 $B$ 为真时，如何从 $B$ 推导出 $C$。

这正是和类型的消除规则中的箭头 $f$ 和 $g$。

## 4.6 余笛卡尔范畴（Cocartesian Categories）

在 Haskell 中，我们可以使用 $\text{Either}$ 定义任意两个类型的和类型。在范畴论中，如果所有的和类型都存在，并且初始对象（空类型）也存在，那么这个范畴被称为 **余笛卡尔范畴**（Cocartesian Category），而和类型被称为 **余积**（Coproduct）。

### 4.6.1 一加零

我们首先展示 $1 + 0 \cong 1$，这意味着终端对象（单位类型）与初始对象（空类型）的和类型同构于终端对象。

在 Haskell 中，$\text{Void}$ 类型表示初始对象，它没有值。以下是对应的图示：

$$
\begin{tikzcd}
1
\arrow[dr, bend left, "\text{Left}"']
\arrow[ddr, bend right, "x"']
&& 0
\arrow[dl, bend right, "\text{Right}"]
\arrow[ddl, bend left, "\mbox{!`}"']
\\
& 1 + 0
\arrow[d, dashed, "h"]
\\
& a
\end{tikzcd}
\qquad
\begin{tikzcd}
1
\arrow[dd, "x"]
\\
\\
a
\end{tikzcd}
$$

在这个图中，$h$ 由一对 $(x, \mbox{!`})$ 决定，其中 $\mbox{!`}$ 是从初始对象到 $a$ 的唯一箭头。在 Haskell 中，$\mbox{!`}$ 可以看作是 $\text{absurd}$ 函数，它从 $\text{Void}$ 到任何类型。

我们可以建立从 $1 + 0$ 到 $1$ 的同构。这意味着 $\text{Either () Void}$ 与 $\text{()}$ 是同构的。

在 Haskell 中，我们可以定义以下函数：

```haskell
f :: Either () Void -> ()
f (Left ()) = ()
-- f (Right _) = undefined -- 这行实际上不需要，因为 Void 没有值

f' :: () -> Either () Void
f' _ = Left ()
```

函数 $\text{f}$ 和 $\text{f'}$ 互为逆映射，建立了 $\text{Either () Void}$ 与 $\text{()}$ 之间的同构。

### 4.6.2 某物加零

类似地，我们可以证明 $a + 0 \cong a$。也就是说，任何类型 $a$ 加上初始对象，等价于 $a$ 本身。

在 Haskell 中，我们可以定义：

```haskell
f :: Either a Void -> a
f (Left a) = a
-- f (Right _) = undefined -- 不需要实现，因为 Void 没有值

f' :: a -> Either a Void
f' a = Left a
```

### 4.6.3 交换性（Commutativity）

和类型的定义具有左右对称性，表明它满足交换律，即 $a + b \cong b + a$。

在 Haskell 中，我们可以定义：

```haskell
swap :: Either a b -> Either b a
swap (Left a)  = Right a
swap (Right b) = Left b
```

这个函数将 $\text{Either a b}$ 中的 $\text{Left a}$ 映射为 $\text{Right a}$，将 $\text{Right b}$ 映射为 $\text{Left b}$。它的逆映射也是它自己，因此 $\text{swap}$ 与自身组合得到恒等函数。

### 4.6.4 结合性（Associativity）

和类型也是结合的，即 $(a + b) + c \cong a + (b + c)$。

在 Haskell 中，我们可以定义：

```haskell
associate :: Either (Either a b) c -> Either a (Either b c)
associate (Left (Left a))  = Left a
associate (Left (Right b)) = Right (Left b)
associate (Right c)        = Right (Right c)

associate' :: Either a (Either b c) -> Either (Either a b) c
associate' (Left a)          = Left (Left a)
associate' (Right (Left b))  = Left (Right b)
associate' (Right (Right c)) = Right c
```

这里，$\text{associate}$ 和 $\text{associate'}$ 互为逆映射，建立了 $(a + b) + c$ 与 $a + (b + c)$ 之间的同构。

### 4.6.5 函子性（Functoriality）

和类型具有 **函子性**，意味着可以将箭头（函数）提升到和类型的层次。

假设有箭头 $f : a \to a'$ 和 $g : b \to b'$，我们可以定义一个从 $a + b$ 到 $a' + b'$ 的箭头 $h$，使得：

```haskell
h :: Either a b -> Either a' b'
h (Left a)  = Left (f a)
h (Right b) = Right (g b)
```

这表示我们可以将函数 $f$ 和 $g$ 分别应用于 $\text{Either}$ 的左右分支。

### 练习

**练习 1**：证明函子性保持组合。

**提示**：取两条可组合的箭头 $g : b \to b'$ 和 $g' : b' \to b''$，证明应用 $g' \circ g$ 给出与首先应用 $g$ 将 $a + b$ 变换为 $a + b'$，然后应用 $g'$ 将 $a + b'$ 变换为 $a + b''$ 相同的结果。

**解答**：

我们需要证明以下等式成立：

$$
\text{map}_{a + b''} (f, g' \circ g) = \text{map}_{a + b''} (f, g') \circ \text{map}_{a + b'} (f, g)
$$

其中 $\text{map}_{a + b'} (f, g)$ 是将 $f$ 和 $g$ 提升到和类型的映射。

证明过程：

- 对于 $\text{Left a}$，有：

  $$
  \text{map}_{a + b''} (f, g' \circ g) (\text{Left a}) = \text{Left} (f a)
  $$

  同时，

  $$
  (\text{map}_{a + b''} (f, g') \circ \text{map}_{a + b'} (f, g)) (\text{Left a}) = \text{map}_{a + b''} (f, g') (\text{Left} (f a)) = \text{Left} (f a)
  $$

- 对于 $\text{Right b}$，有：

  $$
  \text{map}_{a + b''} (f, g' \circ g) (\text{Right b}) = \text{Right} ((g' \circ g) b)
  $$

  同时，

  $$
  (\text{map}_{a + b''} (f, g') \circ \text{map}_{a + b'} (f, g)) (\text{Right b}) = \text{map}_{a + b''} (f, g') (\text{Right} (g b)) = \text{Right} (g' (g b))
  $$

因此，两者相等，函子性保持组合。

**练习 2**：证明函子性保持恒等函数。

**解答**：

对于恒等函数 $id_a : a \to a$ 和 $id_b : b \to b$，提升到和类型后得到 $h : a + b \to a + b$，定义为：

```haskell
h (Left a)  = Left (id_a a) = Left a
h (Right b) = Right (id_b b) = Right b
```

因此，$h$ 就是 $\text{Either}$ 类型上的恒等函数。

### 4.6.6 对称幺半范畴（Symmetric Monoidal Category）

具有上述和类型性质的范畴被称为 **对称幺半范畴**（Symmetric Monoidal Category）。当操作是和类型（余积）时，它被称为 **余笛卡尔范畴**（Cocartesian Category）。

这些性质包括：

- 存在一个中性元素 $0$，使得 $a + 0 \cong a$。
- 和类型满足交换律：$a + b \cong b + a$。
- 和类型满足结合律：$(a + b) + c \cong a + (b + c)$。
- 和类型具有函子性。

这些性质允许我们在类型之间进行灵活的组合和转换，帮助我们构建复杂的程序结构。

---

**小结**：

通过本章的学习，我们深入了解了和类型的概念，以及它们在编程、逻辑和范畴论中的意义。我们探讨了 $\text{Bool}$ 类型、枚举类型、一般的和类型、$\text{Maybe}$ 类型，以及它们在编程中的应用。同时，我们了解了和类型在逻辑中的对应关系，以及在范畴论中的形式化描述。

这些知识对于理解函数式编程语言中的类型系统，以及构建健壮、抽象的程序，具有重要的作用。

---

# 附录：练习的解答

**练习 3**：在 Haskell 中实现证明 $\text{Either a Void}$ 与 $\text{a}$ 之间同构的函数。

**解答**：

```haskell
-- 从 Either a Void 到 a
fromEither :: Either a Void -> a
fromEither (Left a)  = a
-- fromEither (Right v) -- 不需要实现，因为 Void 没有值

-- 从 a 到 Either a Void
toEither :: a -> Either a Void
toEither a = Left a
```

**练习 4**：证明上面定义的双射是自然的。

**解答**：

要证明自然性，我们需要证明在改变焦点时，双射保持成立。也就是说，对于任意箭头 $k : x \to y$，以下图交换：

$$
\begin{tikzcd}
h
\arrow[r, mapsto, "k \circ -"]
\arrow[d, mapsto, "\beta_x"']
& h \circ k
\arrow[d, mapsto, "\beta_y"]
\\
\beta_x(h)
\arrow[r, mapsto, "k \circ -"]
& \beta_y(h \circ k)
\end{tikzcd}
$$

由于 $\beta$ 是在函数之间建立的双射，而这些函数的定义中没有依赖于具体的值，因此自然性成立。

**练习 5**：在 Haskell 中实现证明 $\text{Either a b}$ 与 $\text{Either b a}$ 之间同构的函数。

**解答**：

```haskell
swap :: Either a b -> Either b a
swap (Left a)  = Right a
swap (Right b) = Left b
```

这个函数的逆映射也是它自己，因为再次调用 $\text{swap}$ 会返回原来的值。

**练习 6**：证明函子性保持组合和恒等函数（见上文）。

---

通过这些练习，我们进一步加深了对和类型及其性质的理解。这些概念在函数式编程中非常重要，有助于我们构建更抽象、更通用的程序结构。

### ---------------------------

# 第 4 章：和类型（Sum Types）

在之前的章节中，我们讨论了如何组合箭头（函数）。现在，我们要探讨如何组合对象（类型）。这将引领我们进入 **和类型**（Sum Types）的世界，这是编程语言和数学中非常重要的概念。

## 4.1 布尔类型（Bool）

首先，让我们回顾一下我们已经定义的对象：

- **初始对象**：$0$
- **终端对象**：$1$

那么，$2$ 是什么呢？在类型的世界中，$2$ 可以被视为 $1$ 加上 $1$，即 $2 = 1 + 1$。

### 定义布尔类型

一个 $2$ 对象是一个具有两个元素的对象，这意味着有两个从 $1$ 发出的箭头（函数）指向它。我们可以将这两个箭头分别命名为 $\text{True}$ 和 $\text{False}$。需要注意的是，这两个名称仅仅是箭头的标签，不要将它们与逻辑上的真和假混淆。

下面是对应的图示：

$$
\begin{tikzcd}
1
\arrow[dd, bend right, "\text{True}"']
\arrow[dd, bend left, "\text{False}"]
\\
\\
2
\end{tikzcd}
$$

在 Haskell 中，我们可以直接表达这个概念。我们定义一个类型，传统上称为 $\text{Bool}$，以纪念其发明者 **George Boole**（1815-1864）。这种定义风格在 Haskell 中称为 **广义代数数据类型**（GADTs）。

```haskell
data Bool where
  True  :: () -> Bool
  False :: () -> Bool
```

这个定义对应于上述的图示，只是我们在 Haskell 中使用了不同的名称。这里，$\text{()}$ 代表单位类型（对应于 $1$），而 $\text{Bool}$ 对应于 $2$。

为了简化，我们可以使用元素的简写表示法，因为从单位类型 $\text{()}$ 到某个类型的函数实际上就是该类型的元素。因此，我们可以重写定义为：

```haskell
data Bool where
  True  :: Bool
  False :: Bool
```

现在，$\text{True}$ 和 $\text{False}$ 直接就是 $\text{Bool}$ 类型的元素。

### 使用布尔类型

我们可以定义一个 $\text{Bool}$ 类型的值，例如：

```haskell
x :: Bool
x = True
```

第一行声明了 $\text{x}$ 是 $\text{Bool}$ 类型的一个值（实际上是一个函数 $\text{()} \to \text{Bool}$），第二行告诉我们 $\text{x}$ 是两个可能的值中的哪个。

在 $\text{Bool}$ 的定义中，$\text{True}$ 和 $\text{False}$ 被称为 **数据构造器**。它们用于构造特定的值，就像上面的例子一样。值得注意的是，在 Haskell 中，函数名通常以小写字母开头，除非它们是数据构造器。

### 定义从布尔类型到其他类型的函数

到目前为止，我们已经知道如何构造一个 $\text{Bool}$ 类型的值，但我们还不知道如何使用它。也就是说，我们需要能够定义从 $\text{Bool}$ 到其他类型的 **映射**（函数）。

假设我们有一个从 $\text{Bool}$ 到某个具体类型 $\text{A}$ 的函数 $\text{h}$，那么我们可以通过组合得到两个从单位类型 $\text{()}$ 到 $\text{A}$ 的函数。如下图所示：

$$
\begin{tikzcd}
\text{()}
\arrow[dd, bend right, "\text{True}"']
\arrow[dd, bend left, "\text{False}"]
\arrow[ddd, bend right=90, "x"']
\arrow[ddd, bend left=90, "y"]
\\
\\
\text{Bool}
\arrow[d, dashed, "h"]
\\
A
\end{tikzcd}
$$

这个图表示，每个 $\text{Bool} \to \text{A}$ 的函数都会生成一对 $\text{A}$ 的元素。

给定一个具体类型 $\text{A}$，假设：

```haskell
h :: Bool -> A
```

我们可以得到：

```haskell
x = h True
y = h False
```

其中：

```haskell
x :: A
y :: A
```

这里，我们使用了将函数应用于元素的简写表示法，例如：

```haskell
h True  -- 相当于 h . True
```

### 双射关系

现在，我们要强调的是，从 $\text{Bool}$ 到 $\text{A}$ 的任何函数不仅能生成，而且 **等价于** 一对 $\text{A}$ 的元素。换句话说，一对元素 $(x, y)$ 唯一地决定了从 $\text{Bool}$ 到 $\text{A}$ 的函数 $\text{h}$。

这意味着，我们可以从两个方面理解上面的图：

1. **已知 $\text{h}$**：我们可以轻松地得到 $\text{x}$ 和 $\text{y}$。
2. **已知 $(x, y)$**：我们可以唯一地定义 $\text{h}$。

因此，元素对 $(x, y)$ 与箭头 $\text{h}$ 之间存在一一对应的 **双射**（bijection）。

### 使用条件表达式定义函数

在 Haskell 中，$\text{h}$ 的定义通常封装在 $\text{if} ... \text{then} ... \text{else}$ 结构中。给定：

```haskell
x :: A
y :: A
```

我们可以定义映射为：

```haskell
h :: Bool -> A
h b = if b then x else y
```

这里，$\text{b}$ 是 $\text{Bool}$ 类型的一个值。当 $\text{b}$ 为 $\text{True}$ 时，返回 $\text{x}$；当 $\text{b}$ 为 $\text{False}$ 时，返回 $\text{y}$。

### 引入规则和消除规则

一般来说，数据类型是通过 **引入规则**（Introduction Rules）创建的，并通过 **消除规则**（Elimination Rules）解构的。

- **引入规则**：定义如何构造数据类型的值。在 $\text{Bool}$ 类型中，$\text{True}$ 和 $\text{False}$ 是引入规则。
- **消除规则**：定义如何使用数据类型的值。在 $\text{Bool}$ 类型中，$\text{if} ... \text{then} ... \text{else}$ 结构就是消除规则。

### 计算规则

考虑到对 $\text{h}$ 的定义，我们可以检索用于定义它的两个元素。这被称为 **计算规则**（Computation Rules）。它告诉我们如何计算 $\text{h}$ 的结果：

- 如果我们用 $\text{True}$ 调用 $\text{h}$，结果是 $\text{x}$。
- 如果我们用 $\text{False}$ 调用 $\text{h}$，结果是 $\text{y}$。

### 编程的目的

永远不要忘记编程的目的：**将复杂问题分解为一系列更简单的问题**。$\text{Bool}$ 的定义很好地说明了这个想法。每当我们需要构造一个从 $\text{Bool}$ 到某类型的映射时，我们将其分解为构造目标类型的一对元素的两个更小的任务。我们用两个更简单的问题替代了一个更复杂的问题。

---

## 4.2 枚举类型（Enumerations）

在 $0$、$1$、$2$ 之后，接下来是什么？答案是 $3$。我们可以定义一个具有三个数据构造器的类型。例如：

```haskell
data RGB where
  Red   :: RGB
  Green :: RGB
  Blue  :: RGB
```

如果你觉得这种语法过于冗长，我们可以使用简写形式：

```haskell
data RGB = Red | Green | Blue
```

### 使用枚举类型

这个定义允许我们构造 $\text{RGB}$ 类型的值，例如：

```haskell
c :: RGB
c = Blue
```

### 定义从枚举类型到其他类型的函数

要定义从 $\text{RGB}$ 到其他类型的函数，我们需要更通用的消除模式。类似于从 $\text{Bool}$ 到某个类型的函数由两个元素决定，从 $\text{RGB}$ 到 $\text{A}$ 的函数由 $\text{A}$ 的三个元素 $\text{x}$、$\text{y}$、$\text{z}$ 决定。

我们可以使用模式匹配语法来编写这样的函数：

```haskell
h :: RGB -> A
h Red   = x
h Green = y
h Blue  = z
```

这意味着，根据输入的颜色，我们返回不同的 $\text{A}$ 类型的值。

### 模式匹配的其他用法

我们也可以对 $\text{Bool}$ 类型使用相同的语法，代替 $\text{if} ... \text{then} ... \text{else}$ 结构：

```haskell
h :: Bool -> A
h True  = x
h False = y
```

实际上，还有另一种写法，使用 $\text{case}$ 表达式：

```haskell
h c = case c of
  Red   -> x
  Green -> y
  Blue  -> z
```

对于 $\text{Bool}$ 类型：

```haskell
h b = case b of
  True  -> x
  False -> y
```

在编程时，你可以根据需要选择这些方式中的任何一种。

### 适用于更多的数据构造器

这些模式也适用于具有四个、五个或更多数据构造器的类型。例如，十进制数字可以定义为：

```haskell
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
```

### 字符类型

Haskell 中有一个巨大的 Unicode 字符枚举，称为 $\text{Char}$。它的构造器有特殊的名称：你在字符本身前后加上单引号，例如：

```haskell
c :: Char
c = 'a'
```

### 通配符模式

正如老子所说，**万物之模式，需时多年，故人们发明了通配符模式“下划线”，它匹配一切**。

由于模式是按顺序匹配的，因此你应该将通配符模式作为系列中的最后一个：

```haskell
yesno :: Char -> Bool
yesno c = case c of
  'y' -> True
  'Y' -> True
  _   -> False
```

在这个函数中，如果字符是 `'y'` 或 `'Y'`，返回 $\text{True}$；否则，返回 $\text{False}$。

### 更多的枚举类型

但我们为什么要止步于此呢？类型 $\text{Int}$ 可以被看作是在 $-2^{29}$ 到 $2^{29}$ 范围内的整数枚举（或更多，取决于实现）。当然，在这种范围内进行详尽的模式匹配是不可行的，但原理仍然成立。

实际上，类型 $\text{Char}$ 用于 Unicode 字符，$\text{Int}$ 用于固定精度整数，$\text{Double}$ 用于双精度浮点数，以及其他几个类型，都内置于语言中。

这些类型不是无限的。它们的元素可以被枚举，即使可能需要一万年。类型 $\text{Integer}$ 则是无限的，因为它表示任意大小的整数。

---

## 4.3 和类型（Sum Types）

$\text{Bool}$ 类型可以看作是和类型 $2 = 1 + 1$ 的特例。但没有什么能阻止我们用其他类型替换 $1$，甚至用不同的类型替换这两个 $1$。我们可以使用两个箭头定义一个新类型 $a + b$，我们称它们为 $\text{Left}$ 和 $\text{Right}$。

下面是对应的图示：

$$
\begin{tikzcd}
a
\arrow[dr, "\text{Left}"']
&& b
\arrow[dl, "\text{Right}"]
\\
& a + b
\end{tikzcd}
$$

### 在 Haskell 中定义和类型

在 Haskell 中，类型 $a + b$ 被称为 $\text{Either a b}$。类似于 $\text{Bool}$，我们可以将其定义为：

```haskell
data Either a b where
  Left  :: a -> Either a b
  Right :: b -> Either a b
```

注意，这里的 $a$ 和 $b$ 是类型变量，用小写字母表示。

### 从和类型到其他类型的函数

类似地，从 $a + b$ 到某个类型 $c$ 的映射由以下可交换图决定：

$$
\begin{tikzcd}
a
\arrow[dr, bend left, "\text{Left}"']
\arrow[ddr, bend right, "f"']
&& b
\arrow[dl, bend right, "\text{Right}"]
\arrow[ddl, bend left, "g"]
\\
& a + b
\arrow[d, dashed, "h"]
\\
& c
\end{tikzcd}
$$

在这个图中：

- 我们有两个函数 $f : a \to c$ 和 $g : b \to c$。
- 我们的目标是定义一个函数 $h : a + b \to c$。

给定一个函数 $h$，我们可以通过组合它与 $\text{Left}$ 和 $\text{Right}$ 得到 $f$ 和 $g$：

- $f = h \circ \text{Left}$
- $g = h \circ \text{Right}$

反过来，已知 $f$ 和 $g$，可以唯一地确定 $h$。这就是 **消除规则**。

### 在 Haskell 中实现

当我们想将此图转换为 Haskell 时，我们需要选择两种类型的元素。我们可以通过定义从终端对象（单位类型 $\text{()}$）的箭头来完成。

考虑以下图示，其中 $a$ 和 $b$ 都从 $\text{()}$ 接收输入：

$$
\begin{tikzcd}
& \text{()}
\arrow[ld, "a"']
\arrow[rd, "b"]
\\
a
\arrow[dr, bend left, "\text{Left}"']
\arrow[ddr, bend right, "f"']
&& b
\arrow[dl, bend right, "\text{Right}"]
\arrow[ddl, bend left, "g"]
\\
& a + b
\arrow[d, dashed, "h"]
\\
& c
\end{tikzcd}
$$

沿着这个图的箭头，我们得到：

- $h \circ \text{Left} \circ a = f \circ a$
- $h \circ \text{Right} \circ b = g \circ b$

在 Haskell 中，我们可以使用模式匹配语法定义 $\text{h}$：

```haskell
h :: Either a b -> c
h (Left  a) = f a
h (Right b) = g b
```

再次注意，小写字母用于类型变量，并且这些字母与这些类型的值相同。在这个定义中：

- 当输入是 $\text{Left a}$ 时，返回 $f \ a$。
- 当输入是 $\text{Right b}$ 时，返回 $g \ b$。

你还可以从右向左阅读这些方程，这样你就会看到和类型的 **计算规则**：定义 $\text{h}$ 所使用的两个函数可以通过应用 $\text{h}$ 到 $\text{Left a}$ 和 $\text{Right b}$ 来恢复。

### 使用 case 表达式

你也可以使用 $\text{case}$ 语法定义 $\text{h}$：

```haskell
h e = case e of
  Left  a -> f a
  Right b -> g b
```

### 数据类型的本质

那么，数据类型的本质是什么？它是 **操纵箭头（函数）** 的一种方式。通过定义数据构造器和消除规则，我们能够构造复杂的数据类型，并定义如何使用它们。

---

## 4.4 可能类型（Maybe）

一个非常有用的数据类型是 $\text{Maybe}$，它被定义为 $1 + a$，对于任意类型 $a$。这意味着 $\text{Maybe a}$ 要么是一个值 $\text{a}$，要么是一个特殊的值表示“无”。

### 在 Haskell 中定义可能类型

在 Haskell 中，$\text{Maybe}$ 类型的定义如下：

```haskell
data Maybe a where
  Nothing :: () -> Maybe a
  Just    ::  a -> Maybe a
```

这里，$\text{Nothing}$ 是从单位类型 $\text{()}$ 到 $\text{Maybe a}$ 的函数，$\text{Just}$ 则从 $a$ 到 $\text{Maybe a}$。

简写形式为：

```haskell
data Maybe a = Nothing | Just a
```

### 可能类型的用途

$\text{Maybe}$ 主要用于对部分函数的返回类型进行编码：这些函数在某些参数值上未定义。在这种情况下，这些函数不会失败，而是返回 $\text{Nothing}$。在其他编程语言中，部分函数通常使用异常（或程序崩溃）来处理。

例如，定义一个安全的除法函数：

```haskell
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)
```

当除数为 $0$ 时，返回 $\text{Nothing}$；否则，返回 $\text{Just (x / y)}$。

---

## 4.5 逻辑

在逻辑中，命题 $A + B$ 被称为 **逻辑或**（Disjunction）。你可以通过提供 $A$ 的证明或 $B$ 的证明来证明它。任意一个证明都足够了。

### 从 $A + B$ 推导出 $C$

如果你想证明 $C$ 从 $A + B$ 中得出，你必须为两种可能性做好准备：

1. **$A$ 为真**：你必须展示如何从 $A$ 推导出 $C$。
2. **$B$ 为真**：你必须展示如何从 $B$ 推导出 $C$。

这与 $\text{Either}$ 类型的消除规则完全一致：

- 定义从 $A$ 到 $C$ 的函数 $f$。
- 定义从 $B$ 到 $C$ 的函数 $g$。
- 然后，使用这些函数来定义从 $A + B$ 到 $C$ 的函数 $h$。

---

## 4.6 余笛卡尔范畴（Cocartesian Categories）

在 Haskell 中，我们可以使用 $\text{Either}$ 定义任意两种类型的和类型。在范畴论中，如果所有的和类型都存在，并且初始对象（零对象）也存在，我们称这个范畴为 **余笛卡尔范畴**，而和类型则被称为 **余积**（Coproduct）。

你可能注意到，和类型模仿了数字的加法。事实上，初始对象扮演了零的角色。

### 4.6.1 一加零（One Plus Zero）

我们首先展示 $1 + 0 \cong 1$，这意味着 **终端对象** 与 **初始对象** 的和类型同构于终端对象。

#### 使用约内达引理（Yoneda Lemma）

此类证明的标准方法是使用 **约内达引理**。约内达引理告诉我们，如果两个对象之间的 **仿射空间**（Hom-sets）存在自然的双射，那么这两个对象是同构的。

也就是说，如果对于任意对象 $a$，从 $1 + 0$ 到 $a$ 的箭头集合与从 $1$ 到 $a$ 的箭头集合之间存在自然的双射，那么 $1 + 0$ 与 $1$ 同构。

#### 分析映射

让我们看看 $1 + 0$ 的定义以及它到任意对象 $a$ 的映射。这个映射由一对 $(x, \text{!`})$ 定义，其中：

- $x$ 是从 $1$ 到 $a$ 的箭头，即 $x : 1 \to a$。
- $\text{!`}$ 是从 $0$ 到 $a$ 的唯一箭头。在 Haskell 中，$0$ 对应于 $\text{Void}$ 类型，$\text{!`}$ 对应于从 $\text{Void}$ 到 $a$ 的函数 $\text{absurd}$。

下面是对应的图示：

$$
\begin{tikzcd}
1
\arrow[dr, bend left, "\text{Left}"']
\arrow[ddr, bend right, "x"']
&& 0
\arrow[dl, bend right, "\text{Right}"]
\arrow[ddl, bend left, "\text{!`}"]
\\
& 1 + 0
\arrow[d, dashed, "h"]
\\
& a
\end{tikzcd}
\quad
\begin{tikzcd}
1
\arrow[dd, "x"]
\\
\\
a
\end{tikzcd}
$$

#### 建立双射

我们希望在从 $1 + 0$ 发出的箭头与从 $1$ 发出的箭头之间建立一对一的映射。

- **从 $1 + 0$ 到 $a$ 的箭头**：由一对 $(x, \text{!`})$ 决定。
- **从 $1$ 到 $a$ 的箭头**：由 $x$ 决定。

由于从 $0$ 到 $a$ 的箭头 $\text{!`}$ 是唯一的，因此 $h$ 与 $x$ 之间存在一个双射。

我们定义一个函数 $\beta_a$，将从 $1 + 0$ 到 $a$ 的箭头 $h$ 映射到 $x$。反过来，$\beta^{-1}_a$ 将 $x$ 映射到 $h$。

#### 检查自然性

但是，这是一个 **自然变换** 吗？要回答这个问题，我们需要考虑当我们从 $a$ 切换到通过箭头 $g : a \to b$ 与 $a$ 连接的某个 $b$ 时会发生什么。

我们有两个选项：

1. **后组合后映射**：使 $h$ 通过后组合 $g$ 来切换焦点，得到新的箭头 $h' = g \circ h$。然后使用 $\beta_b$。
2. **先映射后后组合**：使用 $\beta_a$ 将 $h$ 映射到 $x$，然后后组合 $g$，得到 $x' = g \circ x$。

在这两种情况下，我们都得到相同的箭头 $y = g \circ x$。因此，映射 $\beta$ 是自然的。

#### 得出结论

因此，$1 + 0$ 与 $1$ 是同构的，即 $1 + 0 \cong 1$。

### 在 Haskell 中实现

在 Haskell 中，我们可以定义构成同构的两个函数，但没有直接的方式表达它们是彼此的逆。

```haskell
-- 从 Either () Void 到 ()
f :: Either () Void -> ()
f (Left ()) = ()
-- f (Right _) -- 不需要实现，因为 Void 没有值

-- 从 () 到 Either () Void
f_1 :: () -> Either () Void
f_1 _ = Left ()
```

这里，$\text{Void}$ 是一个没有值的类型，因此 $\text{Right}$ 分支实际上不可能发生。

---

### 4.6.2 某物加零（Something Plus Zero）

一个非常相似的论证可以用来证明 $a + 0 \cong a$。以下是对应的图示：

$$
\begin{tikzcd}
a
\arrow[dr, bend left, "\text{Left}"']
\arrow[ddr, bend right, "f"']
&& 0
\arrow[dl, bend right, "\text{Right}"]
\arrow[ddl, bend left, "\text{!`}"]
\\
& a + 0
\arrow[d, dashed, "h"]
\\
& x
\end{tikzcd}
\quad
\begin{tikzcd}
a
\arrow[dd, "f"]
\\
\\
x
\end{tikzcd}
$$

我们可以通过实现一个多态函数 $\text{h}$，它适用于任何类型 $a$，将此论证转化为 Haskell。

#### 练习

**练习 1**：在 Haskell 中实现两个函数，这两个函数形成了 $\text{Either a Void}$ 与 $a$ 之间的同构。

**解答**：

```haskell
-- 从 Either a Void 到 a
fromEither :: Either a Void -> a
fromEither (Left a) = a
-- fromEither (Right v) -- 不需要实现，因为 Void 没有值

-- 从 a 到 Either a Void
toEither :: a -> Either a Void
toEither a = Left a
```

---

### 4.6.3 交换性（Commutativity）

和类型定义中的图具有漂亮的左右对称性，这表明它满足交换律，即 $a + b \cong b + a$。

#### 建立双射

让我们考虑公式两边的映射。对于左侧由一对 $(f, g)$ 决定的每个 $h$，右侧有一个对应的 $h'$，它由一对 $(g, f)$ 给出。这就建立了箭头之间的双射。

下面是对应的图示：

$$
\begin{tikzcd}
a
\arrow[dr, bend left, "\text{Left}"']
\arrow[ddr, bend right, "f"']
&& b
\arrow[dl, bend right, "\text{Right}"]
\arrow[ddl, bend left, "g"]
\\
& a + b
\arrow[d, dashed, "h"]
\\
& x
\end{tikzcd}
\quad
\begin{tikzcd}
b
\arrow[dr, bend left, "\text{Left}"']
\arrow[ddr, bend right, "g"']
&& a
\arrow[dl, bend right, "\text{Right}"]
\arrow[ddl, bend left, "f"]
\\
& b + a
\arrow[d, dashed, "h'"]
\\
& x
\end{tikzcd}
$$

#### 练习

**练习 2**：证明上面定义的双射是自然的。提示：$f$ 和 $g$ 通过与 $k : x \to y$ 的后组合来改变焦点。

**解答**：

要证明双射是自然的，我们需要验证对于任意箭头 $k : x \to y$，以下图交换：

$$
\begin{tikzcd}
h
\arrow[r, mapsto, "k \circ -"]
\arrow[d, mapsto, "\beta_x"']
& k \circ h
\arrow[d, mapsto, "\beta_y"]
\\
h'
\arrow[r, mapsto, "k \circ -"]
& k \circ h'
\end{tikzcd}
$$

由于 $\beta$ 是交换左右分支的函数，后组合 $k$ 不会影响这种交换性，因此双射是自然的。

**练习 3**：在 Haskell 中实现见证 $\text{Either a b}$ 与 $\text{Either b a}$ 之间同构的函数。注意，这个函数本身是它的逆。

**解答**：

```haskell
swap :: Either a b -> Either b a
swap (Left a)  = Right a
swap (Right b) = Left b
```

这个函数将 $\text{Left a}$ 映射为 $\text{Right a}$，将 $\text{Right b}$ 映射为 $\text{Left b}$。再次应用 $\text{swap}$，会恢复原始值，因此它是自己的逆。

---

### 4.6.4 结合性（Associativity）

正如在算术中，我们定义的和是结合的：

$$
(a + b) + c \cong a + (b + c)
$$

#### 在 Haskell 中实现

编写左侧的映射：

```haskell
h :: Either (Either a b) c -> x
h (Left (Left a))  = f1 a
h (Left (Right b)) = f2 b
h (Right c)        = f3 c
```

右侧的映射：

```haskell
h' :: Either a (Either b c) -> x
h' (Left a)          = f1 a
h' (Right (Left b))  = f2 b
h' (Right (Right c)) = f3 c
```

这里，$f1$, $f2$, $f3$ 是从 $a$, $b$, $c$ 到 $x$ 的函数。

#### 建立同构

这建立了两个定义映射的函数组之间的一对一映射。这种映射是自然的，因为所有的焦点转换都是通过后组合完成的。因此，两边是同构的。

#### 图示表示

以下是同构左侧的图：

$$
\begin{tikzcd}
a
\arrow[rd, "L"']
\arrow[rrddd, bend right, red, "f_1"']
&& b
\arrow[ld, "R"]
\arrow[ddd, bend left=60, red, "f_2"]
&& c
\arrow[lldd, "R"]
\arrow[llddd, bend left, red, "f_3"]
\\
& a + b
\arrow[rd, "L"']
\\
&& (a + b) + c
\arrow[d, dashed, "h"]
\\
&& x
\end{tikzcd}
$$

---

### 4.6.5 函子性（Functoriality）

由于和类型是由映射出去的性质定义的，所以很容易看出当我们改变 **焦点** 时会发生什么：它会随着定义和类型的箭头焦点的变化而“自然地”变化。

#### 提升箭头

当我们移动这些箭头的 **源** 时，会发生什么呢？假设我们有箭头，将 $a$ 和 $b$ 映射到某个 $a'$ 和 $b'$：

- $f : a \to a'$
- $g : b \to b'$

这些箭头与构造器 $\text{Left}$ 和 $\text{Right}$ 分别组合，定义了和类型之间的映射：

$$
\begin{tikzcd}
a
\arrow[d, "f"]
\arrow[dr, bend left, "\text{Left}"']
&& b
\arrow[d, "g"]
\arrow[dl, bend right, "\text{Right}"]
\\
a'
\arrow[rd, "\text{Left}"']
& a + b
\arrow[d, dashed, "h"]
& b'
\arrow[ld, "\text{Right}"]
\\
& a' + b'
\end{tikzcd}
$$

箭头对 $(\text{Left} \circ f, \text{Right} \circ g)$ 唯一地定义了箭头 $h : a + b \to a' + b'$。

#### 函子性的定义

这种和类型的性质称为 **函子性**（Functoriality）。你可以想象它允许你在和类型的内部转换两个对象，并获得一个新的和类型。我们还说函子性允许我们 **提升** 一对箭头以操作和类型。

#### 练习

**练习 4**：证明函子性保持组合。提示：取两条可组合的箭头，$g : b \to b'$ 和 $g' : b' \to b''$，证明应用 $g' \circ g$ 给出与首先应用 $g$ 将 $a + b$ 变换为 $a + b'$，然后应用 $g'$ 将 $a + b'$ 变换为 $a + b''$ 相同的结果。

**解答**：

对于和类型的提升映射，我们有：

- $\text{map}_{a + b'} (f, g) : a + b \to a' + b'$
- $\text{map}_{a + b''} (f, g') : a' + b' \to a'' + b''$

组合映射为：

$$
\text{map}_{a + b''} (f, g') \circ \text{map}_{a + b'} (f, g) = \text{map}_{a + b''} (f, g' \circ g)
$$

因此，函子性保持组合。

**练习 5**：证明函子性保持身份。提示：使用 $\text{id}_b$ 并证明它被映射到 $\text{id}_{a + b}$。

**解答**：

对于恒等箭头 $\text{id}_a : a \to a$ 和 $\text{id}_b : b \to b$，提升后得到 $\text{id}_{a + b} : a + b \to a + b$，即：

$$
\text{map}_{a + b} (\text{id}_a, \text{id}_b) = \text{id}_{a + b}
$$

因此，函子性保持身份。

---

## 4.7 对称幺半范畴（Symmetric Monoidal Category）

当一个孩子学习加法时，我们称之为 **算术**。当一个成年人学习加法时，我们称之为 **余笛卡尔范畴**。

无论我们是加数字，组合箭头，还是构造对象的和类型，我们都在重复使用将复杂事物分解为其简单组成部分的相同思想。

正如老子所说，**当事物结合形成新事物时，如果操作是结合的，并且有一个中性元素，我们就知道如何处理万事万物**。

### 和类型的性质

我们定义的和类型满足以下属性：

1. **有中性元素 $0$**：

   $$
   a + 0 \cong a
   $$

2. **满足交换律**：

   $$
   a + b \cong b + a
   $$

3. **满足结合律**：

   $$
   (a + b) + c \cong a + (b + c)
   $$

4. **具有函子性**：能够提升箭头，并保持组合和身份。

### 定义

具有这种操作的范畴称为 **对称幺半范畴**（Symmetric Monoidal Category）。当操作是和类型（余积）时，它被称为 **余笛卡尔范畴**（Cocartesian Category）。

在下一章中，我们将看到另一种被称为 **笛卡尔** 的幺半结构，不带“余”字，它涉及到 **积类型**（Product Types）。

---

通过深入理解和类型及其在编程和数学中的应用，我们能够更好地掌握函数式编程的核心思想。这些概念不仅帮助我们构建复杂的数据类型，还为我们提供了处理逻辑和范畴论问题的强大工具。

### ----------------------------

# 第 4 章：和类型（Sum Types）

在本章中，我们将探讨如何组合对象（类型），这是函数式编程和范畴论中的一个重要概念。我们将从布尔类型开始，逐步引入枚举类型、和类型，以及它们在编程和逻辑中的应用。

---

## 4.1 布尔类型（Bool）

### 引入布尔类型

在之前的章节中，我们讨论了如何组合箭头（函数）。现在，我们要思考如何组合对象（类型）。

我们已经定义了初始对象 $0$ 和终端对象 $1$。那么，$2$ 是什么呢？在类型的世界中，$2$ 可以被视为 $1 + 1$，即两个终端对象的和。

一个 $2$ 对象是一个具有两个元素的对象，这意味着有两个从 $1$ 发出的箭头（函数）指向它。我们可以将这两个箭头分别命名为 $True$ 和 $False$。需要注意的是，这两个名称只是箭头的标签，不要将它们与逻辑上的真和假混淆。

下面是对应的图示：

$$
\begin{tikzcd}
1
\arrow[dd, bend right, "True"']
\arrow[dd, bend left, "False"]
\\
\\
2
\end{tikzcd}
$$

### 在 Haskell 中定义布尔类型

在 Haskell 中，我们可以直接表达这个概念。我们定义一个类型，传统上称为 $Bool$，以纪念其发明者 **George Boole**（1815-1864）。这种定义风格在 Haskell 中称为 **广义代数数据类型（GADTs）**。

```haskell
data Bool where
  True  :: () -> Bool
  False :: () -> Bool
```

这里，$()$ 表示单位类型（对应于 $1$）。因此，上述定义对应于以下的图示，只是我们在 Haskell 中使用了不同的名称：

$$
\begin{tikzcd}
()
\arrow[dd, bend right, "True"']
\arrow[dd, bend left, "False"]
\\
\\
Bool
\end{tikzcd}
$$

实际上，由于从单位类型 $()$ 到某个类型的函数与该类型的元素是一一对应的，我们可以简化定义：

```haskell
data Bool where
  True  :: Bool
  False :: Bool
```

### 使用布尔类型

现在，我们可以定义 $Bool$ 类型的值。例如：

```haskell
x :: Bool
x = True
```

- 第一行声明了 $x$ 是 $Bool$ 类型的一个值（实际上是一个函数 $() \to Bool$）。
- 第二行指定了 $x$ 是两个可能的值中的哪个。

在 $Bool$ 的定义中，$True$ 和 $False$ 被称为 **数据构造器**。它们用于构造特定的值。需要注意的是，在 Haskell 中，函数名通常以小写字母开头，除非它们是数据构造器。

### 定义从布尔类型到其他类型的函数

目前，我们知道了如何构造一个 $Bool$ 类型的值，但我们还不知道如何使用它。也就是说，我们需要能够定义从 $Bool$ 到其他类型的 **映射**（函数）。

假设我们有一个从 $Bool$ 到某个具体类型 $A$ 的函数 $h$，那么我们可以通过组合得到两个从单位类型 $()$ 到 $A$ 的函数。如下图所示：

$$
\begin{tikzcd}
()
\arrow[dd, bend right, "True"']
\arrow[dd, bend left, "False"]
\arrow[ddd, bend right=90, "x"']
\arrow[ddd, bend left=90, "y"]
\\
\\
Bool
\arrow[d, dashed, "h"]
\\
A
\end{tikzcd}
$$

这个图表示，每个 $Bool \to A$ 的函数都会生成一对 $A$ 的元素。

给定一个具体类型 $A$，假设：

```haskell
h :: Bool -> A
```

我们可以得到：

```haskell
x = h True
y = h False
```

其中：

```haskell
x :: A
y :: A
```

这里，我们使用了将函数应用于元素的简写表示法。例如，$h\ True$ 表示 $h \circ True$。

### 双射关系

现在，我们要强调的是，从 $Bool$ 到 $A$ 的任何函数不仅能生成，而且 **等价于** 一对 $A$ 的元素。换句话说，一对元素 $(x, y)$ 唯一地决定了从 $Bool$ 到 $A$ 的函数 $h$。

这意味着，我们可以从两个方面理解上面的图：

1. **已知 $h$**：我们可以轻松地得到 $x$ 和 $y$。
2. **已知 $(x, y)$**：我们可以唯一地定义 $h$。

因此，元素对 $(x, y)$ 与函数 $h$ 之间存在一一对应的 **双射**（bijection）。

### 使用条件表达式定义函数

在 Haskell 中，$h$ 的定义通常封装在 $if\ ...\ then\ ...\ else$ 结构中。给定：

```haskell
x :: A
y :: A
```

我们可以定义映射为：

```haskell
h :: Bool -> A
h b = if b then x else y
```

这里，$b$ 是 $Bool$ 类型的一个值。当 $b$ 为 $True$ 时，返回 $x$；当 $b$ 为 $False$ 时，返回 $y$。

### 引入规则和消除规则

一般来说，数据类型是通过 **引入规则**（Introduction Rules）创建的，并通过 **消除规则**（Elimination Rules）解构的。

- **引入规则**：定义如何构造数据类型的值。在 $Bool$ 类型中，$True$ 和 $False$ 是引入规则。
- **消除规则**：定义如何使用数据类型的值。在 $Bool$ 类型中，$if\ ...\ then\ ...\ else$ 结构就是消除规则。

### 计算规则

考虑到对 $h$ 的定义，我们可以检索用于定义它的两个元素。这被称为 **计算规则**（Computation Rules）。它告诉我们如何计算 $h$ 的结果：

- 如果我们用 $True$ 调用 $h$，结果是 $x$。
- 如果我们用 $False$ 调用 $h$，结果是 $y$。

### 编程的目的

永远不要忘记编程的目的：**将复杂问题分解为一系列更简单的问题**。$Bool$ 的定义很好地说明了这个想法。每当我们需要构造一个从 $Bool$ 到某类型的映射时，我们将其分解为构造目标类型的一对元素的两个更小的任务。我们用两个更简单的问题替代了一个更复杂的问题。

---

## 4.2 枚举类型（Enumerations）

### 引入枚举类型

在 $0$、$1$、$2$ 之后，接下来是什么？答案是 $3$。我们可以定义一个具有三个数据构造器的类型。例如，定义一个代表颜色的类型：

```haskell
data RGB where
  Red   :: RGB
  Green :: RGB
  Blue  :: RGB
```

如果你觉得这种语法过于冗长，我们可以使用简写形式：

```haskell
data RGB = Red | Green | Blue
```

### 使用枚举类型

这个定义允许我们构造 $RGB$ 类型的值，例如：

```haskell
c :: RGB
c = Blue
```

### 定义从枚举类型到其他类型的函数

要定义从 $RGB$ 到其他类型的函数，我们需要更通用的消除模式。类似于从 $Bool$ 到某个类型的函数由两个元素决定，从 $RGB$ 到 $A$ 的函数由 $A$ 的三个元素 $x$、$y$、$z$ 决定。

我们可以使用模式匹配语法来编写这样的函数：

```haskell
h :: RGB -> A
h Red   = x
h Green = y
h Blue  = z
```

这意味着，根据输入的颜色，我们返回不同的 $A$ 类型的值。

### 模式匹配的其他用法

我们也可以对 $Bool$ 类型使用相同的语法，代替 $if\ ...\ then\ ...\ else$ 结构：

```haskell
h :: Bool -> A
h True  = x
h False = y
```

实际上，还有另一种写法，使用 $case$ 表达式：

```haskell
h c = case c of
  Red   -> x
  Green -> y
  Blue  -> z
```

对于 $Bool$ 类型：

```haskell
h :: Bool -> A
h b = case b of
  True  -> x
  False -> y
```

在编程时，你可以根据需要选择这些方式中的任何一种。

### 适用于更多的数据构造器

这些模式也适用于具有四个、五个或更多数据构造器的类型。例如，十进制数字可以定义为：

```haskell
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
```

### 字符类型

Haskell 中有一个巨大的 Unicode 字符枚举，称为 $Char$。它的构造器有特殊的名称：你在字符本身前后加上单引号，例如：

```haskell
c :: Char
c = 'a'
```

### 通配符模式

正如老子所说，**万物之模式，需时多年，故人们发明了通配符模式“下划线”，它匹配一切**。

由于模式是按顺序匹配的，因此你应该将通配符模式作为系列中的最后一个：

```haskell
yesno :: Char -> Bool
yesno c = case c of
  'y' -> True
  'Y' -> True
  _   -> False
```

在这个函数中，如果字符是 $'y'$ 或 $'Y'$，返回 $True$；否则，返回 $False$。

### 更多的枚举类型

但我们为什么要止步于此呢？类型 $Int$ 可以被看作是在 $-2^{29}$ 到 $2^{29}$ 范围内的整数枚举（或更多，取决于实现）。当然，在这种范围内进行详尽的模式匹配是不可行的，但原理仍然成立。

实际上，类型 $Char$ 用于 Unicode 字符，$Int$ 用于固定精度整数，$Double$ 用于双精度浮点数，以及其他几个类型，都内置于语言中。

这些类型不是无限的。它们的元素可以被枚举，即使可能需要一万年。类型 $Integer$ 则是无限的，因为它表示任意大小的整数。

---

## 4.3 和类型（Sum Types）

### 引入和类型

$Bool$ 类型可以看作是和类型 $2 = 1 + 1$ 的特例。但没有什么能阻止我们用其他类型替换 $1$，甚至用不同的类型替换这两个 $1$。我们可以使用两个箭头定义一个新类型 $a + b$，我们称它们为 $Left$ 和 $Right$。

下面是对应的图示：

$$
\begin{tikzcd}
a
\arrow[dr, "Left"']
&& b
\arrow[dl, "Right"]
\\
& a + b
\end{tikzcd}
$$

### 在 Haskell 中定义和类型

在 Haskell 中，类型 $a + b$ 被称为 $Either\ a\ b$。类似于 $Bool$，我们可以将其定义为：

```haskell
data Either a b where
  Left  :: a -> Either a b
  Right :: b -> Either a b
```

注意，这里的 $a$ 和 $b$ 是类型变量，用小写字母表示。

### 从和类型到其他类型的函数

类似地，从 $a + b$ 到某个类型 $c$ 的映射由以下可交换图决定：

$$
\begin{tikzcd}
a
\arrow[dr, bend left, "Left"']
\arrow[ddr, bend right, "f"']
&& b
\arrow[dl, bend right, "Right"]
\arrow[ddl, bend left, "g"]
\\
& a + b
\arrow[d, dashed, "h"]
\\
& c
\end{tikzcd}
$$

在这个图中：

- 我们有两个函数 $f : a \to c$ 和 $g : b \to c$。
- 我们的目标是定义一个函数 $h : a + b \to c$。

给定一个函数 $h$，我们可以通过组合它与 $Left$ 和 $Right$ 得到 $f$ 和 $g$：

- $f = h \circ Left$
- $g = h \circ Right$

反过来，已知 $f$ 和 $g$，可以唯一地确定 $h$。这就是 **消除规则**。

### 在 Haskell 中实现

当我们想将此图转换为 Haskell 时，我们需要选择两种类型的元素。我们可以通过定义从终端对象（单位类型 $()$）的箭头来完成。

考虑以下图示，其中 $a$ 和 $b$ 都从 $()$ 接收输入：

$$
\begin{tikzcd}
& ()
\arrow[ld, "a"']
\arrow[rd, "b"]
\\
a
\arrow[dr, bend left, "Left"']
\arrow[ddr, bend right, "f"']
&& b
\arrow[dl, bend right, "Right"]
\arrow[ddl, bend left, "g"]
\\
& a + b
\arrow[d, dashed, "h"]
\\
& c
\end{tikzcd}
$$

沿着这个图的箭头，我们得到：

- $h \circ Left \circ a = f \circ a$
- $h \circ Right \circ b = g \circ b$

在 Haskell 中，我们可以使用模式匹配语法定义 $h$：

```haskell
h :: Either a b -> c
h (Left  a) = f a
h (Right b) = g b
```

再次注意，小写字母用于类型变量，并且这些字母与这些类型的值相同。在这个定义中：

- 当输入是 $Left\ a$ 时，返回 $f\ a$。
- 当输入是 $Right\ b$ 时，返回 $g\ b$。

你还可以从右向左阅读这些方程，这样你就会看到和类型的 **计算规则**：定义 $h$ 所使用的两个函数可以通过应用 $h$ 到 $Left\ a$ 和 $Right\ b$ 来恢复。

### 使用 case 表达式

你也可以使用 $case$ 语法定义 $h$：

```haskell
h e = case e of
  Left  a -> f a
  Right b -> g b
```

### 数据类型的本质

那么，数据类型的本质是什么？它是 **操纵箭头（函数）** 的一种方式。通过定义数据构造器和消除规则，我们能够构造复杂的数据类型，并定义如何使用它们。

---

## 4.4 可能类型（Maybe）

### 引入可能类型

一个非常有用的数据类型是 $Maybe$，它被定义为 $1 + a$，对于任意类型 $a$。这意味着 $Maybe\ a$ 要么是一个值 $a$，要么是一个特殊的值表示“无”。

### 在 Haskell 中定义可能类型

在 Haskell 中，$Maybe$ 类型的定义如下：

```haskell
data Maybe a where
  Nothing :: () -> Maybe a
  Just    ::  a -> Maybe a
```

这里，$Nothing$ 是从单位类型 $()$ 到 $Maybe\ a$ 的函数，$Just$ 则从 $a$ 到 $Maybe\ a$。

简写形式为：

```haskell
data Maybe a = Nothing | Just a
```

### 可能类型的用途

$Maybe$ 主要用于对部分函数的返回类型进行编码：这些函数在某些参数值上未定义。在这种情况下，这些函数不会失败，而是返回 $Nothing$。在其他编程语言中，部分函数通常使用异常（或程序崩溃）来处理。

例如，定义一个安全的除法函数：

```haskell
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)
```

当除数为 $0$ 时，返回 $Nothing$；否则，返回 $Just\ (x / y)$。

### $Maybe$ 与 $Either$

$Maybe\ a$ 与 $Either\ ()\ a$ 是同构的。$Either\ ()\ a$ 表示要么是 $()$（无值），要么是 $a$（有值）。这与 $Maybe$ 的定义非常相似。

---

## 4.5 逻辑

### 和类型在逻辑中的对应

在逻辑中，命题 $A + B$ 被称为 **逻辑或（Disjunction）**。你可以通过提供 $A$ 的证明或 $B$ 的证明来证明它。任意一个证明都足够了。

### 从 $A + B$ 推导出 $C$

如果你想证明 $C$ 从 $A + B$ 中得出，你必须为两种可能性做好准备：

1. **$A$ 为真**：你必须展示如何从 $A$ 推导出 $C$。
2. **$B$ 为真**：你必须展示如何从 $B$ 推导出 $C$。

这与 $Either$ 类型的消除规则完全一致：

- 定义从 $A$ 到 $C$ 的函数 $f$。
- 定义从 $B$ 到 $C$ 的函数 $g$。
- 然后，使用这些函数来定义从 $A + B$ 到 $C$ 的函数 $h$。

---

## 4.6 余笛卡尔范畴（Cocartesian Categories）

### 引入余笛卡尔范畴

在 Haskell 中，我们可以使用 $Either$ 定义任意两种类型的和类型。在范畴论中，如果所有的和类型都存在，并且初始对象（零对象）也存在，我们称这个范畴为 **余笛卡尔范畴**，而和类型则被称为 **余积（Coproduct）**。

你可能注意到，和类型模仿了数字的加法。事实上，初始对象扮演了零的角色。

### 4.6.1 一加零（One Plus Zero）

#### 证明 $1 + 0 \cong 1$

我们首先展示 $1 + 0 \cong 1$，这意味着 **终端对象** 与 **初始对象** 的和类型同构于终端对象。

#### 使用约内达引理（Yoneda Lemma）

此类证明的标准方法是使用 **约内达引理**。约内达引理告诉我们，如果两个对象之间的 **Hom-集合** 存在自然的双射，那么这两个对象是同构的。

也就是说，如果对于任意对象 $a$，从 $1 + 0$ 到 $a$ 的箭头集合与从 $1$ 到 $a$ 的箭头集合之间存在自然的双射，那么 $1 + 0$ 与 $1$ 同构。

#### 分析映射

让我们看看 $1 + 0$ 的定义以及它到任意对象 $a$ 的映射。这个映射由一对 $(x, !')$ 定义，其中：

- $x$ 是从 $1$ 到 $a$ 的箭头，即 $x : 1 \to a$。
- $!'$ 是从 $0$ 到 $a$ 的唯一箭头。在 Haskell 中，$0$ 对应于 $Void$ 类型，$!'$ 对应于从 $Void$ 到 $a$ 的函数 $absurd$。

下面是对应的图示：

$$
\begin{tikzcd}
1
\arrow[dr, bend left, "Left"']
\arrow[ddr, bend right, "x"']
&& 0
\arrow[dl, bend right, "Right"]
\arrow[ddl, bend left, "!'"']
\\
& 1 + 0
\arrow[d, dashed, "h"]
\\
& a
\end{tikzcd}
\quad
\begin{tikzcd}
1
\arrow[dd, "x"]
\\
\\
a
\end{tikzcd}
$$

#### 建立双射

我们希望在从 $1 + 0$ 发出的箭头与从 $1$ 发出的箭头之间建立一对一的映射。

- **从 $1 + 0$ 到 $a$ 的箭头**：由一对 $(x, !')$ 决定。
- **从 $1$ 到 $a$ 的箭头**：由 $x$ 决定。

由于从 $0$ 到 $a$ 的箭头 $!'$ 是唯一的，因此 $h$ 与 $x$ 之间存在一个双射。

我们定义一个函数 $\beta_a$，将从 $1 + 0$ 到 $a$ 的箭头 $h$ 映射到 $x$。反过来，$\beta^{-1}_a$ 将 $x$ 映射到 $h$。

#### 检查自然性

但是，这是一个 **自然变换** 吗？要回答这个问题，我们需要考虑当我们从 $a$ 切换到通过箭头 $g : a \to b$ 与 $a$ 连接的某个 $b$ 时会发生什么。

我们有两个选项：

1. **后组合后映射**：使 $h$ 通过后组合 $g$ 来切换焦点，得到新的箭头 $h' = g \circ h$。然后使用 $\beta_b$。
2. **先映射后后组合**：使用 $\beta_a$ 将 $h$ 映射到 $x$，然后后组合 $g$，得到 $x' = g \circ x$。

在这两种情况下，我们都得到相同的箭头 $y = g \circ x$。因此，映射 $\beta$ 是自然的。

#### 得出结论

因此，$1 + 0$ 与 $1$ 是同构的，即 $1 + 0 \cong 1$。

### 在 Haskell 中实现

在 Haskell 中，我们可以定义构成同构的两个函数，但没有直接的方式表达它们是彼此的逆。

```haskell
-- 从 Either () Void 到 ()
f :: Either () Void -> ()
f (Left ()) = ()
-- f (Right v) -- 不需要实现，因为 Void 没有值

-- 从 () 到 Either () Void
f_1 :: () -> Either () Void
f_1 _ = Left ()
```

这里，$Void$ 是一个没有值的类型，因此 $Right$ 分支实际上不可能发生。

### 4.6.2 某物加零（Something Plus Zero）

#### 证明 $a + 0 \cong a$

一个非常相似的论证可以用来证明 $a + 0 \cong a$。以下是对应的图示：

$$
\begin{tikzcd}
a
\arrow[dr, bend left, "Left"']
\arrow[ddr, bend right, "f"']
&& 0
\arrow[dl, bend right, "Right"]
\arrow[ddl, bend left, "!'"']
\\
& a + 0
\arrow[d, dashed, "h"]
\\
& x
\end{tikzcd}
\quad
\begin{tikzcd}
a
\arrow[dd, "f"]
\\
\\
x
\end{tikzcd}
$$

#### 在 Haskell 中实现

我们可以通过实现一个多态函数 $h$，它适用于任何类型 $a$，将此论证转化为 Haskell。

**练习 1**：在 Haskell 中实现两个函数，这两个函数形成了 $Either\ a\ Void$ 与 $a$ 之间的同构。

**解答**：

```haskell
-- 从 Either a Void 到 a
fromEither :: Either a Void -> a
fromEither (Left a) = a
-- fromEither (Right v) -- 不需要实现，因为 Void 没有值

-- 从 a 到 Either a Void
toEither :: a -> Either a Void
toEither a = Left a
```

这里，我们定义了两个函数：

- $fromEither$：将 $Either\ a\ Void$ 映射到 $a$。由于 $Void$ 没有值，我们只需要处理 $Left\ a$ 的情况。
- $toEither$：将 $a$ 映射到 $Either\ a\ Void$，通过构造 $Left\ a$。

这两个函数互为逆映射，建立了 $Either\ a\ Void$ 与 $a$ 之间的同构。

### 4.6.3 交换性（Commutativity）

#### 证明 $a + b \cong b + a$

和类型定义中的图具有漂亮的左右对称性，这表明它满足交换律，即 $a + b \cong b + a$。

#### 建立双射

让我们考虑公式两边的映射。对于左侧由一对 $(f, g)$ 决定的每个 $h$，右侧有一个对应的 $h'$，它由一对 $(g, f)$ 给出。这就建立了箭头之间的双射。

下面是对应的图示：

$$
\begin{tikzcd}
a
\arrow[dr, bend left, "Left"']
\arrow[ddr, bend right, "f"']
&& b
\arrow[dl, bend right, "Right"]
\arrow[ddl, bend left, "g"]
\\
& a + b
\arrow[d, dashed, "h"]
\\
& x
\end{tikzcd}
\quad
\begin{tikzcd}
b
\arrow[dr, bend left, "Left"']
\arrow[ddr, bend right, "g"']
&& a
\arrow[dl, bend right, "Right"]
\arrow[ddl, bend left, "f"]
\\
& b + a
\arrow[d, dashed, "h'"]
\\
& x
\end{tikzcd}
$$

#### 在 Haskell 中实现

**练习 2**：在 Haskell 中实现证明 $Either\ a\ b$ 与 $Either\ b\ a$ 之间同构的函数。注意，这个函数本身是它的逆。

**解答**：

```haskell
swap :: Either a b -> Either b a
swap (Left a)  = Right a
swap (Right b) = Left b
```

这个函数将 $Left\ a$ 映射为 $Right\ a$，将 $Right\ b$ 映射为 $Left\ b$。再次应用 $swap$，会恢复原始值，因此它是自己的逆。

#### 检查自然性

**练习 3**：证明上面定义的双射是自然的。提示：$f$ 和 $g$ 通过与 $k : x \to y$ 的后组合来改变焦点。

**解答**：

要证明双射是自然的，我们需要验证对于任意箭头 $k : x \to y$，以下图交换：

$$
\begin{tikzcd}
h
\arrow[r, mapsto, "k \circ -"]
\arrow[d, mapsto, "\beta_x"']
& k \circ h
\arrow[d, mapsto, "\beta_y"]
\\
h'
\arrow[r, mapsto, "k \circ -"]
& k \circ h'
\end{tikzcd}
$$

由于 $swap$ 是交换左右分支的函数，后组合 $k$ 不会影响这种交换性，因此双射是自然的。

### 4.6.4 结合性（Associativity）

#### 证明 $(a + b) + c \cong a + (b + c)$

正如在算术中，我们定义的和类型是结合的：

$$
(a + b) + c \cong a + (b + c)
$$

#### 在 Haskell 中实现

编写左侧的映射：

```haskell
h :: Either (Either a b) c -> x
h (Left (Left a))  = f1 a
h (Left (Right b)) = f2 b
h (Right c)        = f3 c
```

右侧的映射：

```haskell
h' :: Either a (Either b c) -> x
h' (Left a)          = f1 a
h' (Right (Left b))  = f2 b
h' (Right (Right c)) = f3 c
```

这里，$f1$, $f2$, $f3$ 是从 $a$, $b$, $c$ 到 $x$ 的函数。

#### 建立同构

这建立了两个定义映射的函数组之间的一对一映射。这种映射是自然的，因为所有的焦点转换都是通过后组合完成的。因此，两边是同构的。

#### 图示表示

以下是同构左侧的图：

$$
\begin{tikzcd}
a
\arrow[rd, "L"']
\arrow[rrddd, bend right, red, "f_1"']
&& b
\arrow[ld, "R"]
\arrow[ddd, bend left=60, red, "f_2"]
&& c
\arrow[lldd, "R"]
\arrow[llddd, bend left, red, "f_3"]
\\
& a + b
\arrow[rd, "L"']
\\
&& (a + b) + c
\arrow[d, dashed, "h"]
\\
&& x
\end{tikzcd}
$$

### 4.6.5 函子性（Functoriality）

#### 引入函子性

由于和类型是由映射出去的性质定义的，所以很容易看出当我们改变 **焦点** 时会发生什么：它会随着定义和类型的箭头焦点的变化而“自然地”变化。

#### 提升箭头

当我们移动这些箭头的 **源** 时，会发生什么呢？假设我们有箭头，将 $a$ 和 $b$ 映射到某个 $a'$ 和 $b'$：

- $f : a \to a'$
- $g : b \to b'$

这些箭头与构造器 $Left$ 和 $Right$ 分别组合，定义了和类型之间的映射：

$$
\begin{tikzcd}
a
\arrow[d, "f"]
\arrow[dr, bend left, "Left"']
&& b
\arrow[d, "g"]
\arrow[dl, bend right, "Right"]
\\
a'
\arrow[rd, "Left"']
& a + b
\arrow[d, dashed, "h"]
& b'
\arrow[ld, "Right"]
\\
& a' + b'
\end{tikzcd}
$$

箭头对 $(Left \circ f, Right \circ g)$ 唯一地定义了箭头 $h : a + b \to a' + b'$。

#### 定义函子性

这种和类型的性质称为 **函子性（Functoriality）**。你可以想象它允许你在和类型的内部转换两个对象，并获得一个新的和类型。我们还说函子性允许我们 **提升** 一对箭头以操作和类型。

#### 检查组合性

**练习 4**：证明函子性保持组合。提示：取两条可组合的箭头，$g : b \to b'$ 和 $g' : b' \to b''$，证明应用 $g' \circ g$ 给出与首先应用 $g$ 将 $a + b$ 变换为 $a + b'$，然后应用 $g'$ 将 $a + b'$ 变换为 $a + b''$ 相同的结果。

**解答**：

对于和类型的提升映射，我们有：

- $map_{a + b'} (f, g) : a + b \to a' + b'$
- $map_{a' + b''} (f', g') : a' + b' \to a'' + b''$

组合映射为：

$$
map_{a + b''} (f', g') \circ map_{a + b'} (f, g) = map_{a + b''} (f' \circ f, g' \circ g)
$$

因此，函子性保持组合。

#### 检查恒等性

**练习 5**：证明函子性保持身份。提示：使用 $id_b$ 并证明它被映射到 $id_{a + b}$。

**解答**：

对于恒等箭头 $id_a : a \to a$ 和 $id_b : b \to b$，提升后得到 $id_{a + b} : a + b \to a + b$，即：

$$
map_{a + b} (id_a, id_b) = id_{a + b}
$$

因此，函子性保持身份。

---

## 4.7 对称幺半范畴（Symmetric Monoidal Category）

### 引入对称幺半范畴

当一个孩子学习加法时，我们称之为 **算术**。当一个成年人学习加法时，我们称之为 **余笛卡尔范畴**。

无论我们是加数字，组合箭头，还是构造对象的和类型，我们都在重复使用将复杂事物分解为其简单组成部分的相同思想。

正如老子所说，**当事物结合形成新事物时，如果操作是结合的，并且有一个中性元素，我们就知道如何处理万事万物**。

### 和类型的性质

我们定义的和类型满足以下属性：

1. **有中性元素 $0$**：

   $$
   a + 0 \cong a
   $$

2. **满足交换律**：

   $$
   a + b \cong b + a
   $$

3. **满足结合律**：

   $$
   (a + b) + c \cong a + (b + c)
   $$

4. **具有函子性**：能够提升箭头，并保持组合和身份。

### 定义对称幺半范畴

具有这种操作的范畴称为 **对称幺半范畴（Symmetric Monoidal Category）**。当操作是和类型（余积）时，它被称为 **余笛卡尔范畴（Cocartesian Category）**。

在下一章中，我们将看到另一种被称为 **笛卡尔** 的幺半结构，不带“余”字，它涉及到 **积类型（Product Types）**。

---

通过深入理解和类型及其在编程和数学中的应用，我们能够更好地掌握函数式编程的核心思想。这些概念不仅帮助我们构建复杂的数据类型，还为我们提供了处理逻辑和范畴论问题的强大工具。

---

# 总结

本章我们详细探讨了以下内容：

- **布尔类型**：如何定义 $Bool$ 类型，以及如何通过引入规则和消除规则来使用它。
- **枚举类型**：扩展了 $Bool$ 的概念，介绍了具有多个数据构造器的类型。
- **和类型**：引入了 $Either$ 类型，展示了如何将不同的类型组合在一起。
- **可能类型**：介绍了 $Maybe$ 类型，用于处理可能缺失的值。
- **逻辑对应**：解释了和类型在逻辑中的对应关系，即逻辑或。
- **余笛卡尔范畴**：引入了范畴论中的概念，展示了和类型的更深层次的数学结构。
- **函子性**：探讨了和类型如何保持组合和身份映射，体现了函子的性质。
- **对称幺半范畴**：总结了和类型的性质，介绍了对称幺半范畴的概念。

希望通过本章的学习，你对和类型以及相关的概念有了深入的理解。这些知识对于深入研究函数式编程和范畴论都是非常重要的。

# 附录：练习的解答

**练习 1**：在 Haskell 中实现两个函数，这两个函数形成了 $Either\ a\ Void$ 与 $a$ 之间的同构。

**解答**：见上文。

---

**练习 2**：在 Haskell 中实现证明 $Either\ a\ b$ 与 $Either\ b\ a$ 之间同构的函数。注意，这个函数本身是它的逆。

**解答**：见上文。

---

**练习 3**：证明上面定义的双射是自然的。提示：$f$ 和 $g$ 通过与 $k : x \to y$ 的后组合来改变焦点。

**解答**：见上文。

---

**练习 4**：证明函子性保持组合。提示：取两条可组合的箭头，$g : b \to b'$ 和 $g' : b' \to b''$，证明应用 $g' \circ g$ 给出与首先应用 $g$ 将 $a + b$ 变换为 $a + b'$，然后应用 $g'$ 将 $a + b'$ 变换为 $a + b''$ 相同的结果。

**解答**：见上文。

---

**练习 5**：证明函子性保持身份。提示：使用 $id_b$ 并证明它被映射到 $id_{a + b}$。

**解答**：见上文。

---

通过这些练习，我们进一步加深了对和类型及其性质的理解。这些概念在函数式编程中非常重要，有助于我们构建更抽象、更通用的程序结构。

# 参考资料

- **Haskell 官方文档**：了解更多关于 Haskell 编程语言的信息。
- **范畴论入门**：进一步学习范畴论的基础知识。
- **函数式编程原理**：深入理解函数式编程的核心概念。

希望这些内容对你的学习有所帮助！

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