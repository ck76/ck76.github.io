[TOC]

### 常见组合子（Combinators）

组合子（combinators）是一种在lambda演算和函数式编程中用于函数组合的基本构造块。以下是一些常见的组合子、它们的名字来源及其作用。

#### 1. I 组合子（Identity Combinator）
- **名字来源**：Identity（身份）的首字母。
- **定义**：\[ I = \lambda x . x \]
- **作用**：返回自身。是最简单的组合子，用于保持输入不变。

#### 2. K 组合子（Kestrel Combinator）
- **名字来源**：鸟类“Kestrel”（隼）的首字母，名字来自Combinatory Logic中的传统命名。
- **定义**：\[ K = \lambda x . \lambda y . x \]
- **作用**：忽略第二个参数，返回第一个参数。

#### 3. S 组合子（Starling Combinator）
- **名字来源**：鸟类“Starling”（椋鸟）的首字母，名字来自Combinatory Logic中的传统命名。
- **定义**：\[ S = \lambda x . \lambda y . \lambda z . (xz)(yz) \]
- **作用**：分配应用，将参数传递给两个函数并将结果组合。

#### 4. B 组合子（Bluebird Combinator）
- **名字来源**：鸟类“Bluebird”（蓝知更鸟）的首字母，名字来自Combinatory Logic中的传统命名。
- **定义**：\[ B = \lambda f . \lambda g . \lambda x . f (g x) \]
- **作用**：函数组合，将一个函数的输出作为另一个函数的输入。

#### 5. C 组合子（Cardinal Combinator）
- **名字来源**：鸟类“Cardinal”（红雀）的首字母，名字来自Combinatory Logic中的传统命名。
- **定义**：\[ C = \lambda f . \lambda x . \lambda y . f y x \]
- **作用**：交换函数的两个参数。

#### 6. W 组合子（Warbler Combinator）
- **名字来源**：鸟类“Warbler”（莺）的首字母，名字来自Combinatory Logic中的传统命名。
- **定义**：\[ W = \lambda f . \lambda x . f x x \]
- **作用**：将同一个参数传递给函数两次。

#### 7. Y 组合子（Y Combinator）
- **名字来源**：不动点组合子（Fixed-point Combinator），名字可能与形状类似有关。
- **定义**：\[ Y = \lambda f . (\lambda x . f (x x)) (\lambda x . f (x x)) \]
- **作用**：用于定义递归函数。

#### 8. M 组合子（Mockingbird Combinator）
- **名字来源**：鸟类“Mockingbird”（知更鸟）的首字母，名字来自Combinatory Logic中的传统命名。
- **定义**：\[ M = \lambda f . f f \]
- **作用**：将函数应用于自身。

### 总结

这些组合子通过组合和应用函数，提供了一个强大的工具集，用于构建复杂的函数操作和表达式。它们在lambda演算、函数式编程以及组合子逻辑中具有重要的应用。组合子的名字大多来源于鸟类名称，这些名称在Combinatory Logic的传统中得到了沿用。组合子的作用主要体现在函数组合、参数传递和递归定义等方面。

### 具体例子

以下是一些使用这些组合子的例子，以展示它们的具体应用：

#### 例子 1：I 组合子
```haskell
-- Haskell代码
let I = \x -> x
I 5  -- 返回 5
```

#### 例子 2：K 组合子
```haskell
-- Haskell代码
let K = \x -> \y -> x
K 5 10  -- 返回 5
```

#### 例子 3：S 组合子
```haskell
-- Haskell代码
let S = \x -> \y -> \z -> (x z) (y z)
S (\x -> x + 1) (\x -> x * 2) 5  -- 返回 11
```

#### 例子 4：B 组合子
```haskell
-- Haskell代码
let B = \f -> \g -> \x -> f (g x)
B (\x -> x + 1) (\x -> x * 2) 5  -- 返回 11
```

#### 例子 5：C 组合子
```haskell
-- Haskell代码
let C = \f -> \x -> \y -> f y x
C (\x -> \y -> x - y) 5 10  -- 返回 5
```

#### 例子 6：W 组合子
```haskell
-- Haskell代码
let W = \f -> \x -> f x x
W (\x -> x * 2) 5  -- 返回 10
```

#### 例子 7：Y 组合子
```haskell
-- Haskell代码
let Y = \f -> (\x -> f (x x)) (\x -> f (x x))
let factorial = Y (\f -> \n -> if n == 0 then 1 else n * f (n - 1))
factorial 5  -- 返回 120
```

#### 例子 8：M 组合子
```haskell
-- Haskell代码
let M = \f -> f f
M (\x -> x + 1)  -- 无限递归，不建议实际运行
```