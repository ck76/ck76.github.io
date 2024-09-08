[toc]

### -------------------------

### 16 Comonads（余单子）

余单子（Comonad）是 Monad 的对偶，提供了一种处理上下文的方式。与 Monad 处理副作用相对，余单子处理上下文。例如，Monad 处理的是从一个状态转换到另一个状态的过程，而余单子则处理的是从上下文中提取和处理数据的方式。余单子在编程中经常用于处理结构化数据、流和状态等场景。

### 16.1 Comonads in Programming（编程中的余单子）

#### The Stream Comonad（流余单子）

流余单子是余单子的经典例子之一，它提供了一种处理无限序列（Stream）的方式。在 Haskell 中，流可以表示为一个数据结构，其中每个元素都有一个当前值和一个剩余的流。流余单子使得我们能够在流的上下文中进行计算。

在 Haskell 中，流可以定义为：

```haskell
data Stream a = Cons a (Stream a)
```

`Stream` 是一个无限的列表，每个元素都有一个当前值（`a`）和指向下一个流元素的指针。余单子为流提供了一种自然的操作方式，允许我们聚焦于流的某个部分，并沿着流进行导航和处理。

```haskell
instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Comonad Stream where
  extract (Cons x _) = x
  extend f w@(Cons _ xs) = Cons (f w) (extend f xs)
```

- **`extract`** 提取流中的当前元素。
- **`extend`** 通过应用函数 `f`，在整个流上扩展计算。

余单子提供了一种从上下文中提取数据并在上下文中扩展计算的机制。在流余单子中，`extend` 操作允许我们在当前流的上下文中进行计算，并且可以沿着流的每个位置扩展该计算。

#### Example: Averaging with Stream Comonad

一个简单的使用流余单子的例子是计算流中每个元素及其后续元素的平均值。

```haskell
average :: Stream Int -> Int
average (Cons x (Cons y (Cons z _))) = (x + y + z) `div` 3

averagedStream :: Stream Int -> Stream Int
averagedStream = extend average
```

这个例子展示了如何在流余单子的上下文中定义一个平均值计算器，并将其应用到整个流中。

### 16.2 Comonads Categorically（范畴论中的余单子）

#### Comonoids（余半群）

在范畴论中，余单子可以看作是一种特殊的函子。和 Monad 类似，它们是从函子及其相关的自然变换构造的。然而，余单子是由对偶的结构定义的。

余单子由以下三个部分组成：
1. **`extract`**：从上下文中提取值的自然变换。
2. **`duplicate`**：将上下文嵌套为更深层次的上下文的自然变换。
3. **`extend`**：将一个函数应用到嵌套的上下文中。

范畴论中的余半群（Comonoid）是单子中半群的对偶。余半群的结构由单位元和乘法定义，而余单子则由余单子的自然变换（如 `extract` 和 `duplicate`）定义。

#### Comonads from Adjunctions（从伴随构造余单子）

类似于 Monad 可以从伴随函子构造，余单子也可以从伴随函子构造。通过定义两个范畴之间的伴随函子，我们可以构造出一个余单子。

#### Costate Comonad（余状态余单子）

余状态余单子是一种特殊的余单子，它的工作方式与状态单子相似，但从上下文的角度处理状态。在余状态余单子中，我们可以将整个状态空间视为一个上下文，并在其中提取和更新状态。

余状态余单子的定义如下：

```haskell
data Costate s a = Costate (s -> a) s

instance Functor (Costate s) where
  fmap f (Costate g s) = Costate (f . g) s

instance Comonad (Costate s) where
  extract (Costate f s) = f s
  extend f w@(Costate g s) = Costate (\s' -> f (Costate g s')) s
```

- **`extract`**：提取当前状态下的值。
- **`extend`**：在整个状态上下文中扩展计算。

#### Comonad Coalgebras（余单子余代数）

余单子余代数是余单子的对偶结构。余代数为余单子提供了评估上下文的机制，就像代数为 Monad 提供评估副作用的机制一样。

#### Lenses（透镜）

透镜（Lens）是一种用于操作嵌套数据结构的函数工具。透镜允许我们对复杂的嵌套结构进行局部操作。透镜与余单子密切相关，因为透镜提供了在上下文中提取和更新数据的机制。

透镜的基本定义是：

```haskell
type Lens s t a b = (a -> b) -> s -> t
```

透镜为我们提供了一种在上下文中操作数据的方式，使得我们能够从一个更大结构中提取和更新特定部分的数据。

### 总结

余单子在编程中广泛用于处理上下文，它们通过提供 `extract` 和 `extend` 操作，允许我们在上下文中提取和扩展数据。范畴论为余单子提供了更深层次的理论基础，余状态余单子和透镜是余单子的经典应用。

### -------------------------

### 证明：使用 `composeWithEnv` 的 co-Kleisli 箭头组合是结合的

我们需要证明，给定三个 co-Kleisli 箭头 $g: (b, e) \to c$、$f: (a, e) \to b$、$h: (x, e) \to a$，`composeWithEnv` 操作满足结合律，即：

$$
(\text{composeWithEnv} \ g \ (\text{composeWithEnv} \ f \ h)) = (\text{composeWithEnv} \ (\text{composeWithEnv} \ g \ f) \ h)
$$

#### 1. **展开定义**

根据 `composeWithEnv` 的定义：

$$
\text{composeWithEnv} \ g \ f = \lambda (a, e) \to g (f (a, e), e)
$$

我们先分别展开左侧和右侧的表达式。

#### 2. **左侧表达式**

考虑左侧 $ (\text{composeWithEnv} \ g \ (\text{composeWithEnv} \ f \ h)) $：

- 首先，计算 $ \text{composeWithEnv} \ f \ h $：
  
  $$ \text{composeWithEnv} \ f \ h = \lambda (x, e) \to f (h (x, e), e) $$
  
  现在，我们将这个结果代入到 $ \text{composeWithEnv} \ g $：

  $$ \text{composeWithEnv} \ g \ (\text{composeWithEnv} \ f \ h) = \lambda (x, e) \to g ((\lambda (x, e) \to f (h (x, e), e)) (x, e), e) $$
  
  展开内部的 $ \lambda $ 表达式：

  $$ = \lambda (x, e) \to g (f (h (x, e), e), e) $$

#### 3. **右侧表达式**

接下来，考虑右侧 $ \text{composeWithEnv} \ (\text{composeWithEnv} \ g \ f) \ h $：

- 首先，计算 $ \text{composeWithEnv} \ g \ f $：
  
  $$ \text{composeWithEnv} \ g \ f = \lambda (a, e) \to g (f (a, e), e) $$
  
  现在，我们将这个结果代入到 $ \text{composeWithEnv} \ h $：

  $$ \text{composeWithEnv} \ (\text{composeWithEnv} \ g \ f) \ h = \lambda (x, e) \to (\lambda (a, e) \to g (f (a, e), e)) (h (x, e), e) $$
  
  展开内部的 $ \lambda $ 表达式：

  $$ = \lambda (x, e) \to g (f (h (x, e), e), e) $$

#### 4. **比较左侧和右侧**

从上面的展开步骤可以看到，左侧和右侧的结果都是：

$$ \lambda (x, e) \to g (f (h (x, e), e), e) $$

因此，我们得出 $ (\text{composeWithEnv} \ g \ (\text{composeWithEnv} \ f \ h)) = (\text{composeWithEnv} \ (\text{composeWithEnv} \ g \ f) \ h) $，也就是说 `composeWithEnv` 的组合是 **结合的**。

### 结论

我们已经证明，使用 `composeWithEnv` 的 co-Kleisli 箭头组合是结合的。

### -------------------------

### 互相转换的实现

要实现 `duplicate` 和 `extend` 之间的互相转换，我们可以从它们的定义出发：

- **`duplicate`**：将一个上下文 `w a` 转换为包含上下文本身的 `w (w a)`。
- **`extend`**：将一个函数 `f :: w a -> b` 扩展到整个上下文 `w a` 上，返回 `w b`。

根据定义，我们可以推导出它们之间的转换关系：

1. **用 `extend` 实现 `duplicate`**  
   `duplicate` 可以通过 `extend` 实现，因为 `extend` 是通过将一个函数应用到上下文来返回新值，而 `duplicate` 的效果相当于将上下文本身作为参数传递给 `extend`。

   ```haskell
   duplicate :: Comonad w => w a -> w (w a)
   duplicate = extend id
   ```

   这里，`extend id` 表示将 `id` 函数应用到整个上下文，因此我们得到了包含上下文本身的结构，即 `w (w a)`。

2. **用 `duplicate` 实现 `extend`**  
   `extend` 可以通过 `duplicate` 和 `fmap` 实现，因为我们可以先使用 `duplicate` 生成一个 `w (w a)`，然后用 `fmap` 将函数 `f` 应用到每个 `w a`。

   ```haskell
   extend :: Comonad w => (w a -> b) -> w a -> w b
   extend f = fmap f . duplicate
   ```

   在这里，`duplicate` 生成一个包含上下文的 `w (w a)`，然后 `fmap f` 将函数 `f` 应用于每个 `w a`，从而得到 `w b`。

### 总结

通过 `duplicate` 和 `extend` 之间的相互关系，可以轻松地实现一个余单子支持的函数转换。

### -------------------------

在编程中，余单子 ($$Comonad$$) 是函子的一种扩展，与单子相对立。余单子为我们提供了一种方法来处理带有上下文的数据，就像单子允许我们处理带有依赖性或状态的数据一样。

### 余单子的定义

在 Haskell 中，余单子通过以下类型类定义：

```haskell
class Functor w => Comonad w where
    (=<=) :: (w b -> c) -> (w a -> b) -> (w a -> c)
    extract :: w a -> a
```

这里的 `extract` 是从上下文中提取值的函数，而 `=<=` 是 **co-Kleisli 箭头**的组合，类似于单子中的 `<=<`（Kleisli 组合）。换句话说，它允许我们将两个在上下文中工作的函数组合起来。

例如，给定两个函数 $$f :: w a -> b$$ 和 $$g :: w b -> c$$，我们可以通过 `=<=` 将它们组合成 $$g =<= f :: w a -> c$$。这个组合过程类似于普通函数组合，但涉及到上下文。

### 例子：对构造器作为余单子

在下面的实例中，余单子由对构造器 `((,) e)` 的部分应用给出：

```haskell
instance Comonad ((,) e) where
    g =<= f = \ea -> g (fst ea, f ea)
    extract = snd
```

- `extract` 从对的第二个组件中提取值。
- `g =<= f` 组合两个 co-Kleisli 箭头，`f` 应用到整个对 `ea` 上，然后 `g` 以对的第一个组件和 `f` 的结果作为输入。

### 余单子的其他操作

除了 `=<=` 和 `extract`，余单子还定义了对单子 `join` 和 `bind` 的对偶操作：

- `duplicate :: w a -> w (w a)`：这类似于 `join`，它将一个值复制到一个新的上下文中。
- `extend :: (w a -> b) -> w a -> w b`：这是 `bind` 的对偶，它将一个函数应用于上下文并返回结果。

### 实现 co-Kleisli 组合

使用 `duplicate` 和 `fmap` 可以实现 co-Kleisli 箭头的组合：

```haskell
g =<= f = g . fmap f . duplicate
```

这意味着我们首先使用 `duplicate` 复制上下文，然后通过 `fmap` 将 `f` 应用于复制的上下文，最后再将结果传给 `g`。

### 练习 16.1.1：实现 `duplicate` 和 `extend` 的互相转换

- 使用 `duplicate` 实现 `extend`：
  ```haskell
  extend f = fmap f . duplicate
  ```

  在这段代码中，`duplicate` 生成了嵌套上下文 `w (w a)`，而 `fmap f` 将 `f` 应用于整个嵌套上下文。

- 使用 `extend` 实现 `duplicate`：
  ```haskell
  duplicate = extend id
  ```

  这里，`extend id` 通过将 `id`（恒等函数）应用于上下文，产生了一个 `w (w a)`。

这两个函数本质上是余单子操作的对偶：`duplicate` 创建嵌套的上下文，`extend` 则是通过应用函数来扩展上下文中的信息。

### 总结

余单子提供了一个与单子对偶的结构，允许我们处理带有上下文的数据。通过 `extract`、`=<=`、`duplicate` 和 `extend`，我们可以在不直接操作上下文的情况下处理数据。

### -------------------------

### Stream 余单子的详细解读

在编程中，余单子的一个有趣应用是处理更大规模甚至无限上下文的场景。例如，在无限流（$$Stream$$）结构中，我们可以通过余单子在流上执行结构化计算。

#### 1. 无限流结构

首先，我们定义一个简单的无限流：

```haskell
data Stream a = Cons a (Stream a)
    deriving Functor
```

在这个数据结构中，每个流元素是一个头部元素和剩余流的尾部。这里的 `Stream` 类型自然符合函子的结构。

#### 2. 余单子实例化

我们可以为 `Stream` 定义一个 `Comonad` 实例：

```haskell
instance Comonad Stream where
    extract (Cons a _) = a
    duplicate (Cons a as) = Cons (Cons a as) (duplicate as)
```

- `extract` 提取流的头部。
- `duplicate` 生成了一个“流的流”，其中每个子流代表原始流的某个“当前位置”。它为我们创建了多个流的不同视图，允许我们从多个上下文中进行操作。

#### 3. 直观理解

`duplicate` 的作用是将流分解为多个流，每个流都以原始流中的某个元素为头部，尾部是原流的剩余部分。这可以理解为一种在原始流上设置多个视角的方式，使得每个“视角”都可以访问流的未来元素。

```haskell
extend f (Cons a as) = Cons (f (Cons a as)) (extend f as)
```

`extend` 的作用是将某个 co-Kleisli 箭头 $$f :: Stream a \to b$$ 应用到由 `duplicate` 生成的每个子流。换句话说，它在流的每个子流上执行计算。

### 4. 平滑操作（Low-pass filter）

我们可以使用 `extend` 来计算流的滑动平均值，这是余单子强大能力的展示。以下是一个计算流前五个元素平均值的例子：

```haskell
avg :: Stream Double -> Double
avg = (/5) . sum . stmTake 5
```

这里的 `avg` 函数使用 `stmTake` 提取前 5 个流元素并计算它们的平均值。然后我们可以用 `extend` 在整个流上平滑波动：

```haskell
smooth :: Stream Double -> Stream Double
smooth = extend avg
```

这类似于信号处理中常用的低通滤波器，在这里，它生成了原始流的一个滑动平均值。

### 5. 双向流（Exercise 16.1.2）

**练习 16.1.2** 要求我们为双向流实现一个 `Comonad` 实例：

```haskell
data BiStream a = BStr [a] [a]
```

这里的 `BiStream` 数据结构表示一个双向流，其中：
- 第一个列表表示过去的元素（逆序排列）。
- 第二个列表表示现在的元素及未来的元素（正序排列）。

我们可以实现如下的余单子实例：

```haskell
instance Comonad BiStream where
    extract (BStr _ (a:_)) = a
    duplicate (BStr past (a:future)) = BStr (tail (scanl (flip (:)) [] (a:past))) (iterate (\(BStr p (f:fs)) -> BStr (a:p) fs) (BStr past future))
```

- `extract` 提取当前的元素，即第二个列表的头部。
- `duplicate` 生成了双向流的流，每个流代表不同的“当前位置”，可以向前或向后遍历。

#### 6. 实现低通滤波器（Exercise 16.1.3）

为了实现低通滤波器，我们需要在当前值、上一个值（过去的一个值）和下一个值（未来的一个值）上进行平均计算：

```haskell
avg3 :: BiStream Double -> Double
avg3 (BStr (p:_) (a:f:_)) = (p + a + f) / 3
```

这个函数计算当前值的前一个值、当前值和下一个值的平均数。我们可以使用 `extend` 在整个双向流上应用这个滤波器：

```haskell
smoothBiStream :: BiStream Double -> BiStream Double
smoothBiStream = extend avg3
```

### 7. 高斯滤波器

对于高斯滤波器，我们可以使用类似的方式，但是需要使用高斯加权平均值代替简单的平均数。假设权重为 [0.25, 0.5, 0.25]，我们可以实现如下：

```haskell
gaussianFilter :: BiStream Double -> Double
gaussianFilter (BStr (p:_) (a:f:_)) = (0.25 * p + 0.5 * a + 0.25 * f)
```

然后使用 `extend` 将其应用到整个流：

```haskell
smoothGaussian :: BiStream Double -> BiStream Double
smoothGaussian = extend gaussianFilter
```

### 总结

余单子非常适合在结构化数据（如无限流或双向流）上进行局部计算。它允许我们访问上下文，并通过 `extend` 和 `duplicate` 等操作在任意上下文中执行计算。在流的场景中，余单子提供了强大的机制，用于处理需要未来和过去信息的计算，例如信号处理中的平滑和滤波操作。

### -------------------------

<img src="https://p.ipic.vip/py4yqi.png" alt="3a95cfc8668725164d37ed577349d562" style="zoom:50%;" />

在这一节中，我们从范畴论的角度来看待余单子的定义。

### 余单子（Comonad）的定义
余单子可以通过反转单子的定义来获得：
- $$ \text{duplicate} $$ 对应于反转的 $$ \text{join} $$。
- $$ \text{extract} $$ 对应于反转的 $$ \text{return} $$。

余单子是一个配备了两个自然变换的自函子 $$ W $$，这些自然变换分别是：
- $$ \delta : W \to W \circ W $$ 对应于 $$ \text{duplicate} $$。
- $$ \epsilon : W \to \text{Id} $$ 对应于 $$ \text{extract} $$。

### 恒等式
余单子的这两个自然变换 $$ \delta $$ 和 $$ \epsilon $$ 必须满足与单子相同的恒等式，只不过所有的箭头是反转的。这意味着我们要确保这些变换满足以下定律：

#### 余单元定律
有两个余单元定律（如图所示的交换图）：
- $$ \text{Id} \circ W \xrightarrow{\epsilon \circ W} W \xrightarrow{\delta} W \circ W \xrightarrow{W \circ \epsilon} W \circ \text{Id} $$：这表示将 $$ \text{Id} $$ 映射到 $$ W $$ 和 $$ W $$ 自己的组合映射是一致的。
- $$ W \circ W \xrightarrow{\delta \circ W} (W \circ W) \circ W \xrightarrow{W \circ \delta} W \circ (W \circ W) $$：这表示在组合时 $$ \delta $$ 具有结合律。

这两者的图表（交换图）展示了余单子的数学性质，它们保证了 $$ W $$ 的结构与 $$ \delta $$ 和 $$ \epsilon $$ 的组合在不同方式下保持一致。

通过这些图表，我们可以理解：
1. 自函子 $$ W $$ 的行为如何在范畴内保持结构一致性。
2. $$ \text{duplicate} $$ 和 $$ \text{extract} $$ 如何与 $$ W $$ 的范畴性质相关联。

这些定律也类似于单子中的结合律和单位律，只是方向相反。

### -------------------------

### 余半群（Comonoids）

余半群 (Comonoid) 的定义与单半群 (Monoid) 是对偶的。我们可以通过反转半群的定义来获得余半群的定义。在范畴论中，余半群是在一个单元范畴 $$(\mathcal{C}, \otimes, I)$$ 中的对象 $$w$$，它配备了两个态射：

- **余乘法 (comultiplication)**：$$ \delta : w \to w \otimes w $$
- **余单元 (counit)**：$$ \epsilon : w \to I $$

余乘法的作用是将对象“复制”，而余单元的作用是将对象“丢弃”或映射到范畴的终对象（也就是范畴中唯一存在的一个对象，通常是“无值”）。

#### 类比单子和余单子的定义
我们可以将余半群的定义转换为范畴论中余单子的定义：
- 在余单子的上下文中，我们使用函子组合替代张量积 $$(\otimes)$$。
- 使用恒等函子 $$Id$$ 替代单位对象 $$I$$。

这使得余半群的定义可以推广到自函子范畴中的余单子。在 Haskell 编程语言中，我们可以定义一个 `Comonoid` 类型类用于笛卡尔积（Cartesian product）的实现：

```haskell
class Comonoid w where
    split :: w -> (w, w)
    destroy :: w -> ()
```

- `split` 对应于余乘法 $$ \delta $$，它将值复制为两个。
- `destroy` 对应于余单元 $$ \epsilon $$，它丢弃了这个值。

### 余半群的 Haskell 实现

在编程中，我们实际上很容易为任何类型定义一个 `Comonoid` 实例，因为在大多数编程任务中，我们可以直接复制和丢弃值。下面是一个简单的实现：

```haskell
instance Comonoid w where
    split w = (w, w)
    destroy w = ()
```

在这个实例中，`split` 将一个值复制为两个，`destroy` 则简单地丢弃了这个值。

#### 对于复制和丢弃的直觉

余半群的操作很常见。例如，当我们使用一个函数的参数多次时，就相当于隐式地调用了 `split`：

```haskell
f x = x + x
```

上面的函数 `f` 使用参数 `x` 两次，这与 `split` 操作是一致的。而丢弃参数的情况则类似于调用 `destroy`：

```haskell
g y = 42
```

这里的 `g` 并不使用参数 `y`，这类似于我们丢弃这个值的情况。

如果我们显式地编写这些操作，可以用 `split` 和 `destroy` 来描述：

```haskell
f x = let (x1, x2) = split x
      in x1 + x2

g y = let () = destroy y
      in 42
```

### 资源管理中的余半群问题

虽然在大多数编程中，复制和丢弃值是非常常见且无害的，但在某些特殊情况下（例如操作外部资源时）这样做是不合适的。比如：
- **文件句柄**
- **网络端口**
- **在堆上分配的内存块**

这些资源有明确的生命周期，从分配到释放。如果轻易地复制或丢弃这些资源，可能会导致严重的编程错误。例如，复制文件句柄可能导致多个进程竞争同一个资源，而丢弃文件句柄可能导致资源泄漏。

因此，在处理这些资源时，我们通常需要更加严谨地管理它们的生命周期。简单的复制或丢弃可能会导致难以调试的错误。

### 笛卡尔范畴与线性类型

在编程中，基于笛卡尔范畴（支持对角映射和终对象）的模型总是会存在这个问题。每个对象都可以通过复制或丢弃，但有时这在处理特定资源时并不合适。

为了解决这个问题，可以改用不支持对象复制和丢弃的范畴，例如**线性类型**。线性类型系统强制资源只能使用一次，这就避免了复制和丢弃资源的问题。

- 在 **Rust** 语言中，线性类型是通过借用和所有权系统实现的。
- 在 **C++** 中，我们可以通过 `unique_ptr` 和移动语义来模拟线性类型。

在 Haskell 中，也正在尝试引入线性类型，以便更好地管理资源生命周期，避免不必要的复制和丢弃。

### 总结

余半群（Comonoid）是单半群（Monoid）的对偶结构，它允许我们在编程中复制和丢弃值。然而，在处理外部资源时，这种模型可能不适合，必须通过更为严谨的线性类型系统来管理资源的生命周期。

在 Haskell 中，`Comonoid` 类型类提供了一种显式的方式来复制和丢弃值，但在大多数场景下，我们不需要显式地定义这些操作。在特定情况下，使用线性类型能够帮助避免因为随意复制和丢弃资源导致的错误。

### -------------------------

在这一节中，我们讨论的是如何通过伴随关系构造余单子。具体来说，两个函子之间的伴随关系 $$L: \mathcal{D} \to \mathcal{C}$$ 和 $$R: \mathcal{C} \to \mathcal{D}$$ 产生的余单子。

### 伴随关系与余单子

我们已经知道，在伴随关系 $$L \dashv R$$ 中，组合 $$R \circ L$$ 在范畴 $$\mathcal{D}$$ 中形成一个单子。同样地，另一个组合 $$L \circ R$$ 在范畴 $$\mathcal{C}$$ 中形成一个**余单子**。

- **伴随单子的余单元**：伴随的余单元 $$\epsilon$$ 成为余单子的余单元。
- **余乘法**：由 $$\eta$$（伴随的单位）通过缠绕 (whiskering) 生成，公式为：
  $$ \delta = L \circ \eta \circ R $$

这意味着 $$L \circ R$$ 这个组合是一个余单子，其余单元是 $$\epsilon$$，而余乘法是通过将 $$\eta$$ 嵌入 $$L$$ 和 $$R$$ 的组合中来定义的，如公式所示。

### 弦图解释

如图所示的上部分展示了余单子的余单元 $$\epsilon$$，而下部分展示了余乘法 $$\delta$$ 的结构，通过 $$L \circ \eta \circ R$$ 的构造形成。

- 上图描述了 $$\epsilon$$ 作为余单子的余单元，在范畴 $$\mathcal{C}$$ 中对应的弦图。
- 下图通过缠绕的方式展示了 $$\delta$$ 是如何从单位 $$\eta$$ 派生出来的。具体来说，$$\eta$$ 的位置被嵌入到函子 $$L$$ 和 $$R$$ 的组合中，使其形成余乘法。

### 总结

伴随函子的组合 $$L \circ R$$ 是一个余单子，其中：
- 余单元由 $$\epsilon$$ 给出。
- 余乘法 $$\delta$$ 由 $$L \circ \eta \circ R$$ 构造。

这与之前看到的单子和伴随之间的关系相对称，也体现了伴随函子不仅可以用于单子的构造，还可以用于余单子的构造。

### -------------------------

### 余状态余单子 (Costate Comonad)

余状态余单子（也称为存储余单子）是从状态单子 (State Monad) 的伴随关系衍生而来的对偶结构。它是基于乘积和指数之间的柯里化伴随构造的。在这个构造中：
- **左函子** $$ L_s a = a \times s $$，它将某个固定对象 $$ s $$ 与 $$ a $$ 进行笛卡尔积。
- **右函子** $$ R_s c = c^s $$，它是以 $$ s $$ 为参数的指数对象。

### 在 Haskell 中的存储余单子定义

在 Haskell 中，我们可以将这个伴随关系的复合表示为以下数据类型：

```haskell
data Store s c = St (s -> c) s
```

或使用 GADT 表示法：

```haskell
data Store s c where
  St :: (s -> c) -> s -> Store s c
```

其中：
- 第一个组件 $$ (s \to c) $$ 表示从状态 $$ s $$ 到结果 $$ c $$ 的映射。
- 第二个组件 $$ s $$ 表示当前的状态。

### 函子实例

`Store` 是一个函子，因此可以定义 `fmap`：

```haskell
instance Functor (Store s) where
  fmap g (St f s) = St (g . f) s
```

`fmap` 函数通过组合新函数 $$ g $$ 来操作第一个组件。

### 余单子的 `extract` 和 `duplicate`

在余单子中，`extract` 和 `duplicate` 是最基本的两个操作。

- **`extract`** 提取存储的当前值：

```haskell
extract :: Store s c -> c
extract (St f s) = f s
```

`extract` 函数简单地应用函数 $$ f $$ 到当前状态 $$ s $$，从而提取当前的值。

- **`duplicate`** 用来生成更深层次的结构（即“余单子内部的余单子”）：

```haskell
duplicate :: Store s c -> Store s (Store s c)
duplicate (St f s) = St (St f) s
```

`duplicate` 的作用是生成一个存储的存储，其中每个存储单元仍然可以通过 $$ s $$ 来访问，这为构造复杂的依赖性操作提供了基础。

### 伴随的单位与余乘法

伴随的单位 $$ \eta $$ 是 $$ Id \to R_s \circ L_s $$，在 Haskell 中实现如下：

```haskell
unit :: c -> (s -> (c, s))
unit c = \s -> (c, s)
```

要得到 `duplicate`，需要将 $$ \eta $$ 进行缠绕，最终得到：

```haskell
delta :: forall c. Pair (Fun c) -> Pair (Fun (Pair (Fun c)))
delta = fmap @Pair eta
```

这进一步被重写为：

```haskell
delta (P (f, s)) = P (\s' -> P (f, s'), s)
```

### 实现 `Comonad` 实例

使用 `extract` 和 `duplicate` 可以直接为 `Store` 定义 `Comonad` 实例：

```haskell
instance Comonad (Store s) where
  extract (St f s) = f s
  duplicate (St f s) = St (St f) s
```

### 存储余单子的直觉理解

在存储余单子的上下文中，第一组件 $$ f :: s \to c $$ 可以看作是一个虚拟的无限值流的访问器，每个 $$ s $$ 对应一个值。第二个组件 $$ s $$ 则是当前的索引，`extract` 使用这个索引来检索当前值。

- **`duplicate`** 的作用是生成一个“流的流”，每个流由不同的偏移量进行移位。
- **`extend`** 则是在这个流上进行卷积操作。

由于 Haskell 的惰性求值特性，只有明确要求的值才会被计算，因此整个结构非常高效。

### 练习 16.3.1：使用存储余单子实现元胞自动机

这里我们使用存储余单子来实现一个元胞自动机，具体规则如下：

```haskell
data Cell = L | D
  deriving Show

step :: Store Int Cell -> Cell
step (St f n) =
  case (f (n-1), f n, f (n+1)) of
    (L, L, L) -> D
    (L, D, D) -> D
    (D, D, D) -> D
    _ -> L
```

在这个自动机中，`Cell` 可以是活的 (D) 或者是死的 (L)。根据周围三个元胞的状态，我们决定下一个元胞的状态。这类似于 Conway 的生命游戏，但这里是一个更加简单的规则 110 的实现。

- **`f (n-1)`**：表示当前元胞的左边状态。
- **`f n`**：表示当前元胞的当前状态。
- **`f (n+1)`**：表示当前元胞的右边状态。

我们通过模式匹配来定义状态转移规则。

### 运行元胞自动机

我们可以使用 Haskell 的 `iterate` 函数来生成自动机的多个世代：

```haskell
iterate step initialStore
```

这里的 `initialStore` 是 `Store Int Cell` 的初始状态，它可以通过定义一系列初始元胞状态来实现。

### 总结

存储余单子 (Store Comonad) 是一个强大的编程概念，尤其在处理类似于状态的情境时非常有用。它与状态单子相对，但提供了一个更加灵活的框架来操作数据流，尤其是在需要访问过去或未来状态时。

### -----------------------------

### 余单子余代数（Comonad Coalgebras）

余单子余代数是单子代数的对偶。给定一个余单子 $$(W, \epsilon, \delta)$$，余代数的结构由一个承载对象 $$a$$ 和一个箭头 $$\phi: a \to W a$$ 组成。为了使这个余代数能够和余单子很好地配合，我们要求满足两个条件：

1. **单位律**：能够从 $$\phi$$ 注入的值中提取出原来的值，即箭头 $$\phi$$ 满足 $$\epsilon_a \circ \phi = \text{id}_a$$。
   
2. **结合律**：$$\phi$$ 作用于 $$\phi$$ 结果上的提升要与 $$\delta$$ 的作用相同，即 $$\phi \circ \delta_a = W(\phi) \circ \phi$$。

这些条件对应的交换图如下：

- **单位律**：
  $$ \epsilon_a \circ \phi = \text{id}_a $$
  
  交换图如下：
  $$
  \begin{array}{c}
  a \overset{\phi}{\longrightarrow} W a \\
  \downarrow \epsilon_a & \downarrow \text{id}_a \\
  a \longrightarrow a
  \end{array}
  $$

- **结合律**：
  $$ W(\phi) \circ \phi = \delta_a \circ \phi $$

  交换图如下：
  $$
  \begin{array}{ccc}
  a & \overset{\phi}{\longrightarrow} & W a \\
  \downarrow \phi &  & \downarrow W(\phi) \\
  W a & \overset{\delta_a}{\longrightarrow} & W(W a)
  \end{array}
  $$

### Eilenberg-Moore 范畴

正如单子代数可以形成一个范畴，余单子余代数也可以形成一个类别，称为 **Eilenberg-Moore 范畴**，记为 $$\mathcal{C}_W$$。在这个范畴中：
- 对象是余单子余代数 $$(a, \phi)$$。
- 态射是那些能够保持余单子结构的映射。

此外，还有一个 **co-Kleisli 子范畴**，记为 $$\mathcal{C}_W$$（有时使用前缀 $$\text{co-}$$ 来表明它是与 Kleisli 范畴对偶的结构）。这个子范畴包含的对象和态射与 Eilenberg-Moore 范畴有着紧密联系。

### 伴随构造

给定一个余单子 $$W$$，我们可以使用两个范畴 $$\mathcal{C}_W$$ 和 $$\mathcal{C}_W$$ 构造一个伴随构造，该构造能够重新生成余单子 $$W$$。这个构造完全类似于单子伴随的构造，只不过所有的定义和图形是对偶的。

### 总结

- **余单子余代数** 是由一个承载对象 $$a$$ 和箭头 $$\phi: a \to W a$$ 组成，它们满足单位律和结合律。
- **Eilenberg-Moore 范畴** 是余单子余代数形成的范畴，其结构与单子代数的范畴对称。
- **伴随构造** 可以通过这两个范畴重新构造余单子 $$W$$，其构造与单子的伴随构造完全对称。

余单子余代数提供了一种处理带有上下文信息的结构化数据的方式，特别适合那些需要通过多层次上下文进行计算的场景。

### -------------------------

### 透镜 (Lenses)

透镜是 Store 余单子的一个特别有趣的余代数。透镜可以被看作一种结构化访问的抽象，允许我们在一个复杂的数据结构中聚焦于某个部分进行读取或修改。透镜的应用最初来源于数据库记录的操作，但现在在处理各种数据结构时也得到了广泛应用。

### Store 余单子与透镜

首先，Store 余单子的定义如下：

```haskell
data Store a s = St (a -> s) a
```

- **`a`**：焦点 (focus) 类型，即我们操作的具体部分。
- **`s`**：源 (source) 类型，即整个数据结构。

余代数的核心是一个函数：

```haskell
phi :: s -> Store a s
```

它等价于一对函数：

```haskell
set :: s -> a -> s
get :: s -> a
```

- **`set`**：用新值 $$a$$ 替换焦点，生成新的 $$s$$。
- **`get`**：从 $$s$$ 中提取焦点 $$a$$。

### 透镜的解释

在这个上下文中，`get` 用于提取焦点，而 `set` 用于更新焦点。透镜的作用就是从一个更大的对象中聚焦并操作它的一部分。

#### 透镜的定义与操作

- `get`：从源 $$s$$ 中提取焦点 $$a$$。
- `set`：将新的焦点 $$a$$ 设置到源 $$s$$，生成一个新的 $$s$$。

这种操作形式化了访问更大数据结构中的某个部分的概念。透镜让我们可以轻松地对复合结构进行局部的读写操作。例如，透镜可以让我们聚焦于一个元组中的某个元素，或一个记录的特定字段。

### 余单子的定律与透镜

将余单子余代数的定律应用于透镜，我们得到了以下结果：

1. **`set/get` 定律**：
   ```haskell
   set s (get s) = s
   ```

   这意味着在一个数据结构上先获取焦点值，再将其设置回去，应该不会改变任何东西。换句话说，如果你不改变数据，只是提取和重新放入，结果应该保持不变。

2. **`set/set` 定律**：
   ```haskell
   set (set s a) a' = set s a'
   ```

   这个定律意味着，先将焦点设置为 $$a$$，然后覆盖为 $$a'$$，等同于直接将焦点设置为 $$a'$$。也就是说，连续的两次设置，其中一次被覆盖，是没有意义的，结果等同于只执行最后一次的设置。

3. **`get/set` 定律**：
   ```haskell
   get (set s a) = a
   ```

   这个定律意味着，在将焦点设置为 $$a$$ 后，`get` 应该返回新设置的值 $$a$$。这确保了数据的一致性：当你修改数据后，再次读取时，应该得到你刚刚设置的值。

满足这些定律的透镜被称为**合法透镜**。

### 透镜的 Store 余单子形式

在 Store 余单子的上下文中，我们可以使用 `phi` 和 `delta` 的形式化表示将其写为余单子的定义：

```haskell
phi s = (set s, get s)
epsilon (f, a) = f a
delta (f, a) = (\x -> (f, x), a)
```

这对应于我们之前讨论的透镜定律：
- **`set/get` 定律**保证了你可以从 `get` 得到的值中设置回去。
- **`set/set` 定律**和**`get/set` 定律**确保了当你修改焦点后，数据的一致性和可预测性。

### 透镜的实际应用

透镜在编程中非常有用，尤其是当你需要在嵌套数据结构中进行局部修改时。例如，透镜可以让你轻松地访问和修改一个嵌套在记录中的某个字段，而不需要手动处理数据结构的所有细节。

在 Haskell 中，透镜已经成为非常流行的抽象，并且有许多库（例如 `lens` 库）实现了透镜的各种操作，包括组合、变换和应用。

### 总结

透镜是 Store 余单子的一个余代数，它们可以帮助我们聚焦于更大数据结构中的某个部分，并进行读取和修改。透镜遵循几条基本定律：`set/get`、`set/set` 和 `get/set`，这些定律确保了数据操作的合理性和一致性。

透镜的出现使得在复杂数据结构上进行局部操作变得更加简洁和可维护。在函数式编程的世界中，透镜为我们提供了一种强大且灵活的方式来处理数据。


### -------------------------


### -------------------------


### -------------------------


### -------------------------


### -------------------------


### -------------------------


### -------------------------

