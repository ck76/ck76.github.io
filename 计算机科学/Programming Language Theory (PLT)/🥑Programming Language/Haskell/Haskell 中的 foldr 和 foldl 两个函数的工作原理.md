[toc]

这张图展示了 Haskell 中的 `foldr` 和 `foldl` 两个函数的工作原理，并且用树状图形直观地展示了它们的折叠过程。

### `foldr` (fold right)

`foldr` 的定义为：
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
```

`foldr f z xs` 将列表 `xs` 从右到左折叠起来：

1. 当列表为空时，返回初始值 `z`。
2. 对于非空列表 `(x:xs)`，将第一个元素 `x` 和递归折叠 `xs` 的结果作为参数传给函数 `f`。

图中展示了 `foldr f z [1, 2, 3]` 的过程：

```
   (:)              f
  /  \            /  \
 1   (:)         1    f
    /  \   -->      /  \
   2   (:)         2    f
      /  \           /  \
     3   []         3    z
```

折叠过程可以看成是将 `(:)` 替换成 `f`，`[]` 替换成 `z`。

### `foldl` (fold left)

`foldl` 的定义为：
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs
```

`foldl f z xs` 将列表 `xs` 从左到右折叠起来：

1. 当列表为空时，返回初始值 `z`。
2. 对于非空列表 `(x:xs)`，将初始值 `z` 和第一个元素 `x` 传给函数 `f`，然后继续递归处理剩下的列表 `xs`。

图中展示了 `foldl f z [1, 2, 3]` 的过程：

```
   (:)              f
  /  \            /  \
 1   (:)         f    3
    /  \   -->  / \
   2   (:)     f   2
      /  \    / \
     3   []  z   1
```

折叠过程可以看成是将 `(:)` 替换成 `f`，`[]` 替换成 `z`。

### 总结

`foldr` 和 `foldl` 都是用于列表折叠的函数，但它们的折叠方向不同：
- `foldr` 从右向左折叠，适用于处理可能产生无限列表的情况。
- `foldl` 从左向右折叠，适用于处理需要累积结果的情况。

折叠的过程可以通过替换运算符 `(:)` 和空列表 `[]` 来理解。图中的树状图形展示了如何一步步替换，直到得到最终结果。



---

### 使用 `foldr` 实现 `foldl`

首先，我们需要理解 `foldl` 和 `foldr` 的工作原理：

- `foldl` (左折叠) 从左到右处理列表，即从列表的头部开始处理每个元素。
- `foldr` (右折叠) 从右到左处理列表，即从列表的尾部开始处理每个元素。

为了使用 `foldr` 实现 `foldl`，我们可以将 `foldl` 的折叠过程转换为一个函数组合的过程。我们需要将 `foldr` 转换成一个生成函数的过程，然后将初始值应用于这些函数。这种方式巧妙地利用了函数组合来模拟 `foldl` 的从左到右的折叠过程。

#### 使用 `foldr` 实现 `foldl` 的代码：

```haskell
-- foldl 定义
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs = foldr (\a g x -> g (f x a)) id xs z
```

解释：
1. `foldr` 的 lambda 函数 `(\a g x -> g (f x a))` 用于构建一个新的函数。
2. 初始值 `id` 是恒等函数。
3. `foldr` 将列表转换成一系列函数的组合，然后将初始值 `z` 作为参数传递给这些函数。

### 使用 `foldl` 实现 `foldr`

为了使用 `foldl` 实现 `foldr`，我们需要将 `foldr` 的从右到左的折叠过程转换为一个从左到右的过程。我们需要将 `foldl` 转换成一个生成函数的过程，然后将初始值应用于这些函数。这种方式巧妙地利用了函数组合来模拟 `foldr` 的从右到左的折叠过程。

#### 使用 `foldl` 实现 `foldr` 的代码：

```haskell
-- foldr 定义
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z xs = foldl (\g a b -> g (f a b)) id xs z
```

解释：
1. `foldl` 的 lambda 函数 `(\g a b -> g (f a b))` 用于构建一个新的函数。
2. 初始值 `id` 是恒等函数。
3. `foldl` 将列表转换成一系列函数的组合，然后将初始值 `z` 作为参数传递给这些函数。

### 示例

为了更好地理解这些实现，我们可以对其进行一些测试：

```haskell
-- 测试 foldl' 实现
main :: IO ()
main = do
  let xs = [1, 2, 3, 4]
  print $ foldl' (+) 0 xs  -- 输出 10
  print $ foldl' (*) 1 xs  -- 输出 24

  -- 测试 foldr' 实现
  print $ foldr' (:) [] xs  -- 输出 [1, 2, 3, 4]
  print $ foldr' (+) 0 xs  -- 输出 10
  print $ foldr' (*) 1 xs  -- 输出 24
```

这些测试将验证我们使用 `foldr` 实现 `foldl` 以及使用 `foldl` 实现 `foldr` 的正确性。

### 详细解释

#### 使用 `foldr` 实现 `foldl` 的详细步骤

考虑 `foldl f z [1, 2, 3]` 的计算过程：

1. `foldl f z [1, 2, 3]` = `((z `f` 1) `f` 2) `f` 3`
2. 使用 `foldr` 来生成函数：
   ```haskell
   foldr (\a g x -> g (f x a)) id [1, 2, 3] z
   ```
3. 展开 `foldr` 的计算过程：
   ```haskell
   (\1 (\2 (\3 id))) z
   ```
4. 应用初始值 `z`：
   ```haskell
   id (f (f (f z 1) 2) 3) = (f (f (f z 1) 2) 3)
   ```

#### 使用 `foldl` 实现 `foldr` 的详细步骤

考虑 `foldr f z [1, 2, 3]` 的计算过程：

1. `foldr f z [1, 2, 3]` = `1 `f` (2 `f` (3 `f` z))`
2. 使用 `foldl` 来生成函数：
   ```haskell
   foldl (\g a b -> g (f a b)) id [1, 2, 3] z
   ```
3. 展开 `foldl` 的计算过程：
   ```haskell
   (((id (\1 (\2 (\3 z)))))
   ```
4. 应用初始值 `z`：
   ```haskell
   id (f 1 (f 2 (f 3 z))) = f 1 (f 2 (f 3 z))
   ```

这两段代码展示了如何使用 `foldr` 实现 `foldl` 以及使用 `foldl` 实现 `foldr` 的详细步骤。通过理解这些过程，我们可以更好地掌握高阶函数的使用技巧。