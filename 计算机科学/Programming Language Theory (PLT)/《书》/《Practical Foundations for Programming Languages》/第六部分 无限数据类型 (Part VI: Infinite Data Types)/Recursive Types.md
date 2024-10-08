[toc]



# 第16章 递归类型 (Recursive Types)

在本章中，我们将探讨**递归类型 (Recursive Types)** 的概念，它们是解决类型同构等式的一种方式。递归类型允许我们在类型系统中表示自引用的类型，从而能够定义复杂的递归数据结构，如树、列表等。

---

## **16.1 解决类型同构 (Solving Type Isomorphisms)**

### **类型同构 (Type Isomorphism)**

**定义**：两个类型 $\tau_1$ 和 $\tau_2$ 是**同构的 (Isomorphic)**，如果存在两个表达式：

1. $x_1 : \tau_1 \vdash e_2 : \tau_2$
2. $x_2 : \tau_2 \vdash e_1 : \tau_1$

使得它们互为逆函数。

**解释**：这意味着从类型 $\tau_1$ 到 $\tau_2$ 的转换函数 $e_2$，以及从类型 $\tau_2$ 到 $\tau_1$ 的转换函数 $e_1$，它们的组合是恒等函数。

**示例**：自然数类型 $\text{nat}$ 与类型 $\text{unit} + \text{nat}$ 是同构的。这可以通过以下两个表达式证明：

1. **从 $\text{unit} + \text{nat}$ 到 $\text{nat}$ 的转换**：

   $$
   x : \text{unit} + \text{nat} \vdash \text{case } x\ \{\ l \cdot\ \Rightarrow z\ |\ r \cdot x_2 \Rightarrow s(x_2)\ \} : \text{nat}
   $$

   - **解释**：如果 $x$ 是左注入（对应于 $\text{unit}$），则返回零 $z$；如果 $x$ 是右注入（对应于 $\text{nat}$），则应用后继函数 $s$。

2. **从 $\text{nat}$ 到 $\text{unit} + \text{nat}$ 的转换**：

   $$
   x : \text{nat} \vdash \text{ifz } x\ \{\ z \Rightarrow l \cdot \langle\ \rangle\ |\ s(x_2) \Rightarrow r \cdot x_2\ \} : \text{unit} + \text{nat}
   $$

   - **解释**：如果 $x$ 是零，则返回左注入 $l \cdot \langle\ \rangle$；如果 $x$ 是后继 $s(x_2)$，则返回右注入 $r \cdot x_2$。

**结论**：这些转换函数互为逆函数，证明了 $\text{nat}$ 与 $\text{unit} + \text{nat}$ 是同构的。

### **递归类型的引入**

**目标**：解决类型同构等式，例如：

$$
\text{nat} \cong \text{unit} + \text{nat}
$$

**方法**：引入**递归类型** $\mu t.\tau$，作为类型等式 $t \cong \tau$ 的解。

- **$\mu t.\tau$**：表示类型变量 $t$ 的递归类型，其定义为 $\tau$ 中的 $t$ 被自身递归替换。

- **固定点方程**：

  $$
  \mu t.\tau \cong [\mu t.\tau / t]\tau
  $$

  - **$[\mu t.\tau / t]\tau$**：表示在 $\tau$ 中用 $\mu t.\tau$ 替换 $t$。

### **折叠和展开操作**

为了在类型之间进行转换，我们引入了**折叠 (fold)** 和**展开 (unfold)** 操作：

1. **折叠操作 (fold)**：

   $$
   x : [\mu t.\tau / t]\tau \vdash \text{fold}(x) : \mu t.\tau
   $$

   - **解释**：将类型 $[\mu t.\tau / t]\tau$ 的值 $x$ 折叠为递归类型 $\mu t.\tau$。

2. **展开操作 (unfold)**：

   $$
   x : \mu t.\tau \vdash \text{unfold}(x) : [\mu t.\tau / t]\tau
   $$

   - **解释**：将递归类型 $\mu t.\tau$ 的值 $x$ 展开为类型 $[\mu t.\tau / t]\tau$。

**性质**：折叠和展开操作互为逆函数，即：

$$
\text{unfold}(\text{fold}(x)) = x \quad \text{and} \quad \text{fold}(\text{unfold}(x)) = x
$$

### **类型与集合的区别**

**注意事项**：尽管在集合论中，一些类型同构是不可行的（例如，Cantor 定理指出不存在集合 $X$ 使得 $X \cong \mathcal{P}(X)$，其中 $\mathcal{P}(X)$ 是 $X$ 的幂集），但在类型理论中，我们可以通过引入递归类型来解决类型同构等式。

- **原因**：类型描述的是计算，而不是集合。某些计算可能不会终止，因此类型之间的同构不必遵循集合论的限制。

---

### -------------------------------------

### **折叠（fold）和展开（unfold）操作详解**

**折叠 (fold)** 和 **展开 (unfold)** 操作是处理递归数据类型的重要工具。它们在递归数据结构的**构造**和**解构**中发挥着重要作用。为了帮助理解这些概念，我们将通过具体的例子来展示它们的应用，并解释它们如何互为逆函数。

#### **1. 折叠操作 (fold)**

**折叠 (fold)** 操作用于**构造递归类型**。它将一个已经部分展开的递归类型“折叠”成完整的递归类型。

$$
x : [\mu t.\tau / t]\tau \vdash \text{fold}(x) : \mu t.\tau
$$

#### **解释**：
- 递归类型 $\mu t.\tau$ 表示通过递归定义的类型，例如列表、树等。
- 操作 $\text{fold}$ 将一个部分展开的递归类型 $[\mu t.\tau / t]\tau$ 转换为完整的递归类型 $\mu t.\tau$。
- **直观理解**：折叠操作是将递归结构的各个部分**组合成一个整体**。

#### **示例：列表的折叠**

假设我们有一个递归定义的整数列表类型：

```haskell
data List a = Nil | Cons a (List a)
```

我们可以用 `fold` 来**构造**一个列表。

```python
# 折叠：将构造器的部分应用折叠为一个整体列表
def fold_list(xs):
    # 从右向左构造列表，模拟递归
    if not xs:
        return Nil()  # 空列表
    else:
        return Cons(xs[0], fold_list(xs[1:]))  # 递归构造非空列表

# 测试构造列表
lst = fold_list([1, 2, 3])
print(lst)  # Cons(1, Cons(2, Cons(3, Nil)))
```

#### **解释：**
- `fold_list` 是一个递归函数，用来从数组 `xs` 构造一个链表。
- 它模拟了递归类型的折叠操作，将一个列表通过 `fold` 构造为完整的递归类型。

---

#### **2. 展开操作 (unfold)**

**展开 (unfold)** 操作用于**解构递归类型**。它将递归类型的值“展开”成它的构造形式。

$$
x : \mu t.\tau \vdash \text{unfold}(x) : [\mu t.\tau / t]\tau
$$

#### **解释**：
- 操作 $\text{unfold}$ 将递归类型 $\mu t.\tau$ 的值 $x$ 展开为类型 $[\mu t.\tau / t]\tau$。
- **直观理解**：展开操作是将递归结构**解构为各个组成部分**。

#### **示例：列表的展开**

我们可以用 `unfold` 来将递归类型的列表展开为其构造形式。

```python
# 展开：将列表的递归结构展开为普通的 Python 列表
def unfold_list(lst):
    # 通过递归解构列表
    if isinstance(lst, Nil):
        return []  # 空列表返回空数组
    elif isinstance(lst, Cons):
        return [lst.head] + unfold_list(lst.tail)  # 递归解构非空列表

# 测试解构列表
array = unfold_list(lst)
print(array)  # [1, 2, 3]
```

#### **解释：**
- `unfold_list` 是一个递归函数，用来将自定义的链表 `lst` 转换回普通的 Python 列表。
- 它模拟了递归类型的展开操作，将一个递归类型的列表解构为原始的列表形式。

---

### **3. 折叠和展开的逆函数性质**

折叠和展开操作是**互为逆函数**的。这意味着：

$$
\text{unfold}(\text{fold}(x)) = x \quad \text{and} \quad \text{fold}(\text{unfold}(x)) = x
$$

#### **直观理解**：
- **折叠**：从部分构造出整体。
- **展开**：将整体解构回其组成部分。

#### **性质验证示例**

我们可以验证这两者的性质，展示折叠和展开是如何互为逆函数的。

```python
# 验证 unfold(fold(x)) = x
array = [1, 2, 3]
lst = fold_list(array)
unfolded = unfold_list(lst)
print(unfolded == array)  # True

# 验证 fold(unfold(x)) = x
new_lst = fold_list(unfolded)
print(new_lst == lst)  # True
```

#### **解释：**
- 第一个测试验证 `unfold(fold(x)) = x`，即将列表 `[1, 2, 3]` 先折叠再展开，得到与原始列表相同的结果。
- 第二个测试验证 `fold(unfold(x)) = x`，即将链表 `lst` 先展开再折叠，得到与原始链表相同的结构。

---

### **4. 结合更复杂的递归结构**

折叠和展开操作不仅适用于简单的递归类型（如列表），还可以应用于更复杂的数据结构，比如树。

#### **示例：树的折叠和展开**

```python
# 定义二叉树
class Tree:
    pass

class Leaf(Tree):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"Leaf({self.value})"

class Node(Tree):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __repr__(self):
        return f"Node({self.left}, {self.right})"

# 折叠：构造二叉树
def fold_tree(values):
    if len(values) == 1:
        return Leaf(values[0])  # 叶子节点
    mid = len(values) // 2
    left = fold_tree(values[:mid])
    right = fold_tree(values[mid:])
    return Node(left, right)  # 内部节点

# 展开：解构二叉树
def unfold_tree(tree):
    if isinstance(tree, Leaf):
        return [tree.value]  # 叶子节点
    elif isinstance(tree, Node):
        return unfold_tree(tree.left) + unfold_tree(tree.right)  # 递归解构左右子树

# 测试
tree = fold_tree([1, 2, 3, 4])
print(tree)  # 输出二叉树
unfolded_values = unfold_tree(tree)
print(unfolded_values)  # [1, 2, 3, 4]
```

#### **解释：**
- `fold_tree` 将数组递归地构造成一棵二叉树。
- `unfold_tree` 将二叉树递归地解构为一个数组。
- 通过这些操作，可以处理更复杂的递归数据结构。

---

### **总结**

1. **折叠操作 (fold)**：用于**构造递归类型**，从部分构造出整体。例如，从数组构造链表或树。
2. **展开操作 (unfold)**：用于**解构递归类型**，将递归数据结构解构为其组成部分。例如，将链表或树展开为数组。
3. **互为逆函数的性质**：折叠和展开操作互为逆函数，意味着我们可以在递归数据结构和其构造形式之间来回转换。
4. **应用场景**：折叠和展开操作适用于多种递归数据结构，如列表、树等。这种方法是处理复杂递归结构的基础。



### --------------------------------------

## **16.2 递归数据结构 (Recursive Data Structures)**

### **递归类型的定义**

**形式**：递归类型 $\mu t.\tau$，其中 $t.\tau$ 是一个类型算子，表示类型等式 $t \cong \tau$ 的解。

- **解释**：递归类型 $\mu t.\tau$ 是类型等式 $t = \tau$ 的**最小固定点**。

### **折叠和展开操作的类型**

1. **展开操作 (unfold)**：

   $$
   x : \mu t.\tau \vdash \text{unfold}(x) : [\mu t.\tau / t]\tau
   $$

2. **折叠操作 (fold)**：

   $$
   x : [\mu t.\tau / t]\tau \vdash \text{fold}(x) : \mu t.\tau
   $$

### **示例：自然数类型**

**定义自然数类型 $\text{nat}$**：

$$
\text{nat} = \mu t.\ \text{unit} + t
$$

- **解释**：自然数类型 $\text{nat}$ 是类型算子 $t.\ \text{unit} + t$ 的递归类型。

**折叠和展开操作的应用**：

- **折叠操作**：将类型 $\text{unit} + \text{nat}$ 的值折叠为 $\text{nat}$。

- **展开操作**：将 $\text{nat}$ 类型的值展开为 $\text{unit} + \text{nat}$。

**互为逆函数**：折叠和展开操作证明了 $\text{nat} \cong \text{unit} + \text{nat}$。

---

## **语言 $L\{+ \times * \mu\}$ 的定义**

### **扩展的语法**

**类型的抽象语法**：

- **类型变量**：$t$
  
- **递归类型**：$\text{rec}(t.\tau)$ 或 $\mu t.\tau$

- **其他类型构造器**：
  
  - **函数类型**：$\text{arr}(\tau_1; \tau_2)$

**表达式的抽象语法**：

- **折叠操作**：$\text{fold}[t.\tau](e)$ 或简写为 $\text{fold}(e)$

- **展开操作**：$\text{unfold}(e)$

### **静态语义 (Statics)**

#### **类型构造判断**

类型构造判断是形式为：

$$
\Delta \vdash \tau\ \text{type}
$$

其中 $\Delta$ 是类型变量的声明集合，形式为 $t_1\ \text{type},\ \dots,\ t_k\ \text{type}$。

**类型构造规则**：

1. **类型变量**：

   $$
   \frac{
     t\ \text{type} \in \Delta
   }{
     \Delta \vdash t\ \text{type}
   }
   \quad (16.1a)
   $$

2. **函数类型**：

   $$
   \frac{
     \Delta \vdash \tau_1\ \text{type} \quad \Delta \vdash \tau_2\ \text{type}
   }{
     \Delta \vdash \text{arr}(\tau_1; \tau_2)\ \text{type}
   }
   \quad (16.1b)
   $$

3. **递归类型**：

   $$
   \frac{
     \Delta,\ t\ \text{type} \vdash \tau\ \text{type}
   }{
     \Delta \vdash \text{rec}(t.\tau)\ \text{type}
   }
   \quad (16.1c)
   $$

   **解释**：在类型环境 $\Delta$ 中，假设 $t\ \text{type}$，如果 $\tau$ 是一个类型，则 $\text{rec}(t.\tau)$ 是一个类型。

#### **类型判断**

类型判断是形式为：

$$
\Gamma \vdash e : \tau
$$

**类型规则**：

1. **折叠操作的类型规则**：

   $$
   \frac{
     \Gamma \vdash e : [\text{rec}(t.\tau) / t]\tau
   }{
     \Gamma \vdash \text{fold}[t.\tau](e) : \text{rec}(t.\tau)
   }
   \quad (16.2a)
   $$

   **解释**：如果 $e$ 的类型是 $[\text{rec}(t.\tau) / t]\tau$，则 $\text{fold}(e)$ 的类型是 $\text{rec}(t.\tau)$。

2. **展开操作的类型规则**：

   $$
   \frac{
     \Gamma \vdash e : \text{rec}(t.\tau)
   }{
     \Gamma \vdash \text{unfold}(e) : [\text{rec}(t.\tau) / t]\tau
   }
   \quad (16.2b)
   $$

   **解释**：如果 $e$ 的类型是 $\text{rec}(t.\tau)$，则 $\text{unfold}(e)$ 的类型是 $[\text{rec}(t.\tau) / t]\tau$。

### -------------------------------------

### **类型判断公式的解释与具体例子**

在类型系统中，类型判断的基本形式是：

$$
\Gamma \vdash e : \tau
$$

#### **解释：**
- **$\Gamma$**：表示**上下文**或**环境**，包含当前已知的变量和它们的类型。
- **$e$**：表示**表达式**，它是我们要判断的目标。
- **$\tau$**：表示**类型**，是我们要判断表达式所属的类型。

这一公式的意思是，在给定的上下文 $\Gamma$ 中，表达式 $e$ 的类型是 $\tau$。接下来我们将通过具体例子来解释**折叠**和**展开**操作的类型规则。

---

### 1. **折叠操作的类型规则**

类型规则：

$$
\frac{
  \Gamma \vdash e : [\text{rec}(t.\tau) / t]\tau
}{
  \Gamma \vdash \text{fold}[t.\tau](e) : \text{rec}(t.\tau)
}
\quad (16.2a)
$$

#### **解释：**
- **折叠操作 (fold)** 是将表达式 $e$ 从部分展开的递归类型转换成完整的递归类型。
- 如果表达式 $e$ 的类型是 $[\text{rec}(t.\tau) / t]\tau$，则通过折叠操作，$e$ 的类型将变为 $\text{rec}(t.\tau)$，即完整的递归类型。

#### **具体例子：列表的折叠**

假设我们有一个递归定义的整数列表类型 `List`，定义如下：

```haskell
data List a = Nil | Cons a (List a)
```

现在，我们有一个部分展开的列表：

```python
lst_part = Cons(1, Cons(2, Nil()))
```

`lst_part` 的类型是 `1 + (Int × List Int)`，这对应于 $[\text{List(Int)} / t]\tau$，即部分递归的列表类型。根据折叠规则，我们可以将 `lst_part` 折叠为完整的递归类型 `List Int`：

```python
lst = fold_list(lst_part)  # 完整的递归列表类型
```

在类型判断公式中：

- $e$ 是 `lst_part`，其类型是 $[\text{List(Int)} / t]\tau$。
- `fold(lst_part)` 的类型是 `List Int`。

因此：

$$
\Gamma \vdash lst_part : [\text{List(Int)} / t]\tau \quad \text{fold}(lst_part) : \text{List(Int)}
$$

---

### 2. **展开操作的类型规则**

类型规则：

$$
\frac{
  \Gamma \vdash e : \text{rec}(t.\tau)
}{
  \Gamma \vdash \text{unfold}(e) : [\text{rec}(t.\tau) / t]\tau
}
\quad (16.2b)
$$

#### **解释：**
- **展开操作 (unfold)** 是将完整的递归类型解构为部分展开的形式。
- 如果表达式 $e$ 的类型是 $\text{rec}(t.\tau)$，则通过展开操作，$e$ 的类型变为 $[\text{rec}(t.\tau) / t]\tau$，即部分递归的展开形式。

#### **具体例子：列表的展开**

假设我们现在有一个完整的递归列表：

```python
lst = Cons(1, Cons(2, Nil()))
```

`lst` 的类型是 `List Int`，这是完整的递归类型。根据展开规则，我们可以将它展开为部分形式 `1 + (Int × List Int)`，也就是递归结构的构造形式。

```python
lst_part = unfold_list(lst)  # 展开为部分递归形式
```

在类型判断公式中：

- $e$ 是 `lst`，其类型是 `List Int`，即 $\text{rec}(t.\tau)$。
- `unfold(lst)` 的类型是 $[\text{List(Int)} / t]\tau$，即部分展开形式。

因此：

$$
\Gamma \vdash lst : \text{List(Int)} \quad \text{unfold}(lst) : [\text{List(Int)} / t]\tau
$$

---

### **折叠与展开操作的逆函数性质**

折叠和展开操作是互为逆函数的，也就是说：

$$
\text{unfold}(\text{fold}(x)) = x \quad \text{and} \quad \text{fold}(\text{unfold}(x)) = x
$$

#### **具体解释：**
1. **$\text{unfold}(\text{fold}(x)) = x$**：如果你先折叠一个部分展开的类型，再将它展开，你会得到原始的部分展开形式。
2. **$\text{fold}(\text{unfold}(x)) = x$**：如果你先将一个完整的递归类型展开，再折叠它，你会得到原始的完整递归类型。

#### **例子验证：**

假设我们有一个部分展开的列表 `lst_part`：

```python
lst_part = Cons(1, Cons(2, Nil()))
```

我们先折叠它，再展开：

```python
lst = fold_list(lst_part)  # 折叠
lst_part_unfolded = unfold_list(lst)  # 展开
assert lst_part == lst_part_unfolded  # 确认展开后的部分形式等于原始部分展开形式
```

然后，如果我们先展开完整的列表，再折叠它：

```python
lst_part_unfolded = unfold_list(lst)  # 展开
lst_folded = fold_list(lst_part_unfolded)  # 再次折叠
assert lst == lst_folded  # 确认折叠后的完整列表等于原始完整列表
```

---

### **总结**

- **折叠操作 (fold)**：将部分展开的递归类型折叠为完整的递归类型。
- **展开操作 (unfold)**：将完整的递归类型展开为部分展开的形式。
- **类型判断公式**定义了如何判断折叠和展开操作的类型。
- **互为逆函数的性质**表明折叠和展开操作是可以相互逆转的，确保递归类型和部分展开形式之间的一致性。

通过这些例子，我们能够更直观地理解递归类型中的折叠和展开操作，以及它们如何通过类型判断公式进行推导。

### ------------------------------------

### **动态语义 (Dynamics)**

动态语义通过以下规则定义：

1. **折叠操作的值规则**：

   $$
   \text{fold}[t.\tau](e)\ \text{val}
   \quad (16.3a)
   $$

   **解释**：$\text{fold}(e)$ 是一个值。

2. **折叠操作的计算规则**（根据解释策略，可选择性地包括）：

   $$
   \frac{
     e \rightarrow e'
   }{
     \text{fold}[t.\tau](e) \rightarrow \text{fold}[t.\tau](e')
   }
   \quad (16.3b)
   $$

   **解释**：如果 $e$ 计算到 $e'$，则 $\text{fold}(e)$ 计算到 $\text{fold}(e')$。

   - **注意**：这个规则用于**严格求值策略 (Eager Evaluation)**。对于**惰性求值策略 (Lazy Evaluation)**，可以省略此规则。

3. **展开操作的计算规则**：

   $$
   \frac{
     e \rightarrow e'
   }{
     \text{unfold}(e) \rightarrow \text{unfold}(e')
   }
   \quad (16.3c)
   $$

4. **展开和折叠的互逆性**：

   $$
   \frac{
     \text{fold}[t.\tau](e)\ \text{val}
   }{
     \text{unfold}(\text{fold}[t.\tau](e)) \rightarrow e
   }
   \quad (16.3d)
   $$

   **解释**：对折叠的值应用展开操作，得到原始的 $e$。

---

## **定理 16.1 （安全性，Safety）**

### **陈述**

1. **类型保持性 (Preservation)**：如果 $e : \tau$ 且 $e \rightarrow e'$，则 $e' : \tau$。

2. **进展性 (Progress)**：如果 $e : \tau$，则要么 $e$ 是一个值，要么存在 $e'$ 使得 $e \rightarrow e'$。

### **证明思路**

- **类型保持性**：通过对计算规则 (16.3) 进行规则归纳，证明每个计算步骤中表达式的类型保持不变。

- **进展性**：通过对表达式的结构进行归纳，证明对于任意类型正确的表达式 $e$，要么 $e$ 是一个值，要么可以进一步计算。

**结论**：这证明了语言 $L\{+ \times * \mu\}$ 的类型安全性。

---

## **示例解析**

### **示例：定义列表类型**

**目标**：使用递归类型定义一个元素类型为 $A$ 的列表类型 $\text{List}(A)$。

**定义**：

$$
\text{List}(A) = \mu t.\ \text{unit} + (A \times t)
$$

- **解释**：列表类型 $\text{List}(A)$ 是类型算子 $t.\ \text{unit} + (A \times t)$ 的递归类型。

### **操作**

1. **构造空列表**：

   - **表达式**：$\text{fold}(\text{inl}(\langle\ \rangle))$

   - **类型**：$\text{List}(A)$

2. **构造非空列表**：

   - **表达式**：$\text{fold}(\text{inr}(\langle a,\ l \rangle))$

     - **$a$**：类型为 $A$ 的元素

     - **$l$**：类型为 $\text{List}(A)$ 的列表

   - **类型**：$\text{List}(A)$

3. **展开列表**：

   - **表达式**：$\text{unfold}(l)$

   - **类型**：$\text{unit} + (A \times \text{List}(A))$

### **函数定义**

**示例**：定义一个计算列表长度的函数 $\text{length} : \text{List}(A) \rightarrow \text{Int}$。

1. **基础情况**：

   - **当列表为空**：

     $$
     \text{length}(l) = 0 \quad \text{如果 } \text{unfold}(l) = \text{inl}(\langle\ \rangle)
     $$

2. **递归情况**：

   - **当列表为非空**：

     $$
     \text{length}(l) = 1 + \text{length}(l') \quad \text{如果 } \text{unfold}(l) = \text{inr}(\langle a,\ l' \rangle)
     $$

---

## **总结**

- **递归类型**：通过解决类型同构等式，递归类型允许我们在类型系统中表示自引用的类型，支持定义复杂的递归数据结构。

- **折叠 (fold) 和展开 (unfold)**：折叠操作用于将类型表达式的值构造为递归类型的值；展开操作用于将递归类型的值解构为类型表达式的值。它们互为逆函数。

- **类型系统**：在语言 $L\{+ \times * \mu\}$ 中，递归类型的引入扩展了类型系统的表达能力，使其能够表示和处理递归数据结构。

- **动态语义**：通过定义计算规则，确保了计算过程中的类型安全性。

- **类型安全性**：定理 16.1 证明了语言的类型安全性，即计算过程中类型保持，且表达式要么是值，要么可以进一步计算。

---

## **练习与思考**

**练习1**：

- **定义二叉树类型** $\text{Tree}(A)$，并实现一个函数，计算树中节点的数量。

  - **类型定义**：

    $$
    \text{Tree}(A) = \mu t.\ \text{unit} + (A \times t \times t)
    $$

  - **函数**：$\text{size} : \text{Tree}(A) \rightarrow \text{Int}$

    - **定义**：

      - **基础情况**：

        $$
        \text{size}(t) = 0 \quad \text{如果 } \text{unfold}(t) = \text{inl}(\langle\ \rangle)
        $$

      - **递归情况**：

        $$
        \text{size}(t) = 1 + \text{size}(t_1) + \text{size}(t_2) \quad \text{如果 } \text{unfold}(t) = \text{inr}(\langle a,\ t_1,\ t_2 \rangle)
        $$

**练习2**：

- **考虑类型同构** $X \cong (X \rightarrow \text{Bool})$。在集合论中，这是不可能的，但在类型系统中，我们可以定义这样的递归类型。

  - **定义**：$\text{X} = \mu t.\ t \rightarrow \text{Bool}$

  - **思考**：如何定义折叠和展开操作，以及相应的转换函数？

**思考**：

- **类型系统的设计**：在实际编程语言中，引入递归类型需要考虑类型检查的可判定性和类型安全性。如何设计类型系统，以支持递归类型的同时，保持这些性质？

- **递归类型的应用**：递归类型在编程语言中有广泛的应用，例如定义复杂的数据结构、处理无限数据等。如何在实践中有效地使用递归类型，避免潜在的陷阱（如无限递归导致的非终止计算）？

---

**参考文献**：

- **MacLane, S. (1998). Categories for the Working Mathematician. Springer.**

- **Mendler, N. P. (1987). Recursive Types and Type Constraints in Second-Order Lambda Calculus.**

希望以上详细的解释和公式解析能够帮助您深入理解 **第16章 递归类型 (Recursive Types)** 的内容。如有任何疑问，欢迎提问！



### --------------------------------------------------

# **16.2 递归数据结构 (Recursive Data Structures)**

在本节中，我们将探讨**递归类型 (Recursive Types)** 在表示归纳数据类型（如自然数类型）中的重要应用。递归类型允许我们解决类型等式，从而在类型系统中定义自引用的数据结构。

---

## **自然数类型的递归表示**

### **类型等式与同构**

我们可以将自然数类型 $\text{nat}$ 视为以下类型等式的同构（up to isomorphism）的解：

$$
\text{nat} \cong [\text{z} \mapsto \text{unit},\ \text{s} \mapsto \text{nat}]
$$

**解释**：

- **$\text{nat}$**：自然数类型。

- **$[\text{z} \mapsto \text{unit},\ \text{s} \mapsto \text{nat}]$**：表示一个类型构造，其中 $\text{z}$ 对应 $\text{unit}$ 类型，$\text{s}$ 对应 $\text{nat}$ 类型。

- **$\cong$**：表示类型同构，即两个类型在结构上是等价的。

### **自然数类型的递归类型定义**

根据上述同构，每个自然数要么是零，要么是另一个自然数的后继。我们可以通过以下**递归类型 (Recursive Type)** 来给出一个解：

$$
\mu t.\ [\text{z} \mapsto \text{unit},\ \text{s} \mapsto t]. \quad (16.4)
$$

**公式解析**：

- **$\mu t.\ \tau$**：递归类型的表示，表示类型等式 $t = \tau$ 的最小解。

- **$t$**：类型变量，用于自引用。

- **$[\text{z} \mapsto \text{unit},\ \text{s} \mapsto t]$**：类型构造器，表示类型 $t$ 是一个选择类型，要么是 $\text{unit}$，要么是 $t$ 本身。

### **引入形式的定义**

自然数类型 $\text{nat}$ 的**引入形式 (Introductory Forms)** 定义如下：

1. **零的定义**：

   $$
   \text{z} = \text{fold}(\text{z} \cdot \langle\ \rangle)
   $$

   **解释**：

   - **$\text{fold}$**：折叠操作，将类型展开的值折叠回递归类型。

   - **$\text{z} \cdot \langle\ \rangle$**：构造一个标签为 $\text{z}$ 的值，$\langle\ \rangle$ 表示 $\text{unit}$ 类型的值。

2. **后继的定义**：

   $$
   \text{s}(e) = \text{fold}(\text{s} \cdot e)
   $$

   **解释**：

   - **$e$**：类型为 $\text{nat}$ 的自然数。

   - **$\text{s} \cdot e$**：构造一个标签为 $\text{s}$ 的值，携带自然数 $e$。

### **条件分支的定义**

条件分支 $\text{ifz}$ 可定义如下：

$$
\text{ifz } e\ \{\ \text{z} \Rightarrow e_0\ |\ \text{s}(x) \Rightarrow e_1\ \} = \text{case unfold}(e)\ \{\ \text{z} \cdot\ \Rightarrow e_0\ |\ \text{s} \cdot x \Rightarrow e_1\ \},
$$

**解释**：

- **$\text{unfold}(e)$**：展开操作，将递归类型的值展开为其类型表达式。

- **$\text{case}$**：模式匹配操作，根据标签进行分支。

- **$\text{z} \cdot\ \Rightarrow e_0$**：如果展开结果是标签为 $\text{z}$，则执行 $e_0$。

- **$\text{s} \cdot x \Rightarrow e_1$**：如果展开结果是标签为 $\text{s}$，并绑定值为 $x$，则执行 $e_1$。

- **下划线**（**underscore**）表示在 $e_0$ 中不自由出现的变量。

### **验证定义的正确性**

这些定义能够体现预期的行为。通过使用 $\text{fold}$ 和 $\text{unfold}$，我们可以递归地构造和解构自然数，并通过模式匹配实现对自然数的操作。

---

## **列表类型的递归表示**

### **类型等式与同构**

另一个示例是自然数列表（list of natural numbers）的类型。我们可以将列表类型表示为以下递归类型：

$$
\mu t.\ [\text{n} \mapsto \text{unit},\ \text{c} \mapsto \text{nat} \times t]
$$

**因此，我们有类型同构**：

$$
\text{list} \cong [\text{n} \mapsto \text{unit},\ \text{c} \mapsto \text{nat} \times \text{list}]
$$

**解释**：

- **$\text{list}$**：列表类型。

- **$[\text{n} \mapsto \text{unit},\ \text{c} \mapsto \text{nat} \times \text{list}]$**：类型构造器，表示列表要么是空列表（标签为 $\text{n}$），要么是由自然数和列表组成的非空列表（标签为 $\text{c}$）。

### **列表构造操作的定义**

列表的构造操作可以通过以下等式表示：

1. **空列表的定义**：

   $$
   \text{nil} = \text{fold}(\text{n} \cdot \langle\ \rangle)
   $$

   **解释**：

   - **$\text{n} \cdot \langle\ \rangle$**：构造一个标签为 $\text{n}$ 的值，表示空列表。

2. **非空列表的定义**：

   $$
   \text{cons}(e_1;\ e_2) = \text{fold}(\text{c} \cdot \langle e_1,\ e_2 \rangle)
   $$

   **解释**：

   - **$e_1$**：类型为 $\text{nat}$ 的自然数，列表的头部元素。

   - **$e_2$**：类型为 $\text{list}$ 的列表，列表的尾部。

   - **$\text{c} \cdot \langle e_1,\ e_2 \rangle$**：构造一个标签为 $\text{c}$ 的值，包含头部元素和尾部列表。

### **列表的条件分支**

列表形式上的条件分支可定义为：

$$
\text{case } e\ \{\ \text{nil} \Rightarrow e_0\ |\ \text{cons}(x;\ y) \Rightarrow e_1\ \} = \text{case unfold}(e)\ \{\ \text{n} \cdot\ \Rightarrow e_0\ |\ \text{c} \cdot \langle x,\ y \rangle \Rightarrow e_1\ \},
$$

**解释**：

- **$\text{unfold}(e)$**：将列表 $e$ 展开。

- **$\text{case}$**：对展开的结果进行模式匹配。

- **$\text{n} \cdot\ \Rightarrow e_0$**：如果标签为 $\text{n}$（空列表），则执行 $e_0$。

- **$\text{c} \cdot \langle x,\ y \rangle \Rightarrow e_1$**：如果标签为 $\text{c}$（非空列表），并绑定头部为 $x$，尾部为 $y$，则执行 $e_1$。

- **下划线**表示“不关心”的变量，使用模式匹配语法来绑定对的组件。

---

## **求值策略对表示的影响**

当**和类型 (Sum Types)** 和**积类型 (Product Types)** 采用**严格求值策略 (Eager Evaluation)** 时，这种列表的表示与传统的链表“黑板符号”（blackboard notation）有自然的对应关系。

- **解释**：

  - 我们可以将 $\text{fold}$ 视为指向标记单元的抽象堆指针，该单元要么是标签 $\text{n}$（没有关联数据），要么是标签 $\text{c}$，附带一个由自然数和另一个列表组成的对，该列表也必须是相同类型的抽象指针。

- **如果和类型或积类型采用**惰性求值策略 (Lazy Evaluation)**，则黑板符号无法有效表示，因为它无法描绘数据结构中存在的**悬挂计算 (Suspended Computations)**。

- **结论**：一般来说，类型本身是不可替代的。绘图可以提供帮助，但类型决定了语义。

---

## **使用递归类型表示余归纳类型**

### **流类型的递归表示**

我们还可以使用递归类型表示**余归纳类型 (Co-Inductive Types)**，例如自然数的流类型（无限序列）。

当 $\text{fold}(-)$ 采用**惰性求值 (Lazy Evaluation)** 时，表示尤其自然。此时，我们可以将流类型 $\text{stream}$ 定义为以下递归类型：

$$
\mu t.\ \text{nat} \times t
$$

**解释**：

- **$\text{stream}$**：流类型。

- **$\mu t.\ \text{nat} \times t$**：表示每个流可以看作是一个由自然数和另一个流组成的对的计算。

### **严格求值策略下的流类型**

如果 $\text{fold}(-)$ 采用**严格求值 (Eager Evaluation)**，那么我们可以考虑以下递归类型：

$$
\mu t.\ \text{unit} \rightarrow (\text{nat} \times t)
$$

**解释**：

- **$\text{unit} \rightarrow (\text{nat} \times t)$**：表示一个函数，接受 $\text{unit}$ 类型的参数，返回一个由自然数和另一个流组成的对。

- **这种表示方式表达了流的延迟计算**，因为需要调用函数来获取下一个元素。

### **黑板符号的局限性**

无论哪种情况，流都无法容易地用黑板符号表示：

- **原因**：

  - **并非因为流是无限的**，而是因为除了使用编程语言中的表达式外，没有准确的方法来描绘延迟计算（delayed computation）。

- **结论**：再次说明，图示可以提供帮助，但对于准确定义数据结构而言，图示并不充分。

---

## **总结**

- **递归类型**允许我们在类型系统中定义自引用的数据结构，解决类型同构等式。

- **自然数类型和列表类型**可以使用递归类型进行表示，利用 $\text{fold}$ 和 $\text{unfold}$ 操作构造和解构数据。

- **求值策略**对数据结构的表示有影响：

  - **严格求值**时，递归类型与传统的数据结构表示有自然的对应。

  - **惰性求值**时，需要考虑延迟计算，黑板符号可能无法准确描述数据结构。

- **余归纳类型**也可以使用递归类型表示，尤其是在惰性求值策略下，流类型的表示非常自然。

- **类型的重要性**：类型决定了数据结构的语义，绘图和符号只能作为辅助，无法替代类型本身。

---

## **练习与思考**

**练习1**：

- **定义自然数的二进制表示的递归类型**，并实现一个将二进制表示转换为十进制自然数的函数。

  - **类型定义**：

    $$
    \text{BinNat} = \mu t.\ [\text{zero} \mapsto \text{unit},\ \text{double} \mapsto t,\ \text{double\_plus\_one} \mapsto t]
    $$

  - **解释**：

    - **$\text{zero}$**：表示数字 0。

    - **$\text{double}$**：表示当前数字乘以 2。

    - **$\text{double\_plus\_one}$**：表示当前数字乘以 2 加 1。

  - **函数**：$\text{binToNat} : \text{BinNat} \rightarrow \text{nat}$

    - **定义**：

      - **展开 $\text{BinNat}$**，使用模式匹配递归计算对应的自然数。

**练习2**：

- **使用递归类型定义一棵可能无限的树，其中每个节点包含一个自然数**，并实现一个函数，遍历树并收集所有节点的值。

  - **类型定义**：

    $$
    \text{InfTree} = \mu t.\ \text{nat} \times (\text{unit} \rightarrow t) \times (\text{unit} \rightarrow t)
    $$

  - **解释**：

    - **每个节点包含一个自然数**。

    - **左右子树通过函数延迟计算得到，支持无限树的表示**。

  - **函数**：$\text{traverse} : \text{InfTree} \rightarrow \text{List}(\text{nat})$

    - **定义**：

      - **使用惰性求值**，递归遍历树，收集节点值。

**思考**：

- **求值策略的选择如何影响数据结构的表示和操作？**

  - **严格求值**和**惰性求值**在处理递归类型时，各有哪些优缺点？

- **在实际编程语言中，如何利用递归类型和求值策略来表示复杂的数据结构，如无限数据结构或需要延迟计算的数据结构？**

---

希望以上详细的解释和公式解析能够帮助您深入理解 **16.2 递归数据结构 (Recursive Data Structures)** 的内容。如有任何疑问，欢迎提问！





### ----------------------------------------

# **第16章 递归类型 (Recursive Types)**

## **16.3 自引用 (Self-Reference)**

在本节中，我们将探讨递归表达式中的**自引用 (Self-Reference)** 概念，特别是如何使用递归类型来实现**通用递归 (General Recursion)**。自引用允许一个表达式在其自身内部引用自身，从而实现复杂的递归计算。

---

### **一般递归表达式**

在通用递归表达式中，例如 $\text{fix}[\tau](x.e)$，变量 $x$ 代表表达式自身。这通过以下**展开转换 (Unrolling Transition)** 来实现：

$$
\text{fix}[\tau](x.e) \rightarrow [\text{fix}[\tau](x.e)/x]e,
$$

**公式解析**：

- **$\text{fix}[\tau](x.e)$**：表示一个类型为 $\tau$ 的递归函数，其中 $x$ 在表达式 $e$ 中代表函数自身。
- **$[\,\text{fix}[\tau](x.e)/x\,]e$**：在 $e$ 中，将 $x$ 替换为 $\text{fix}[\tau](x.e)$，即表达式自身。
- **$\rightarrow$**：表示计算步骤（转化规则）。

**解释**：在执行过程中，$x$ 被表达式自身替换，从而实现了自引用。这确保了 $x$ 代表表达式自身。

### **自引用变量的理解**

可以将 $x$ 视为 $e$ 的一个**隐式参数 (Implicit Argument)**，每当使用该表达式时，它都会被隐式地实例化为自身。在许多熟知的编程语言中，这个隐式参数有一个特殊的名字，例如 **this** 或 **self**，以强调其自引用的解释。

---

### **从递归类型导出通用递归**

使用上述直觉作为指导，我们可以从**递归类型 (Recursive Types)** 中导出通用递归。这一推导表明，通用递归可以像其他语言特性一样，被视为类型结构的表现，而不是一种特殊的语言特性。

推导的关键在于定义一种**自引用表达式的类型 (Type of Self-Referential Expressions)**，其语法如下：

#### **类型的语法**

$$
\text{Typ}\ \tau ::= \text{self}(\tau)\ \tau\ \text{self}\quad \text{自引用类型 (Self-Referential Type)}
$$

- **$\text{self}(\tau)$**：自引用类型，表示一个类型为 $\tau$ 的自引用表达式。

#### **表达式的语法**

$$
\text{Exp}\ e ::= \text{self}[\tau](x.e)\quad \text{self } x \text{ is } e\quad \text{自引用表达式 (Self-Referential Expression)}
$$

$$
\quad\quad\quad\quad\quad \text{unroll}(e)\quad \text{unroll}(e)\quad \text{展开自引用 (Unroll Self-Reference)}
$$

- **$\text{self}[\tau](x.e)$**：自引用表达式，$x$ 是 $e$ 的自引用。
- **$\text{unroll}(e)$**：展开自引用表达式。

### -----------------------------------------

### **一般递归表达式的解释与例子**

在许多编程语言中，递归函数是通过自引用的方式实现的。在类型理论中，递归表达式可以通过 $\text{fix}[\tau](x.e)$ 来表示。下面通过具体的例子详细解释递归表达式的展开转换及自引用。

---

### **1. 递归表达式的展开转换**

我们有一个递归表达式 $\text{fix}[\tau](x.e)$，其中：
- $\text{fix}$ 是一个递归函数构造器，用于定义递归函数。
- $x$ 是表达式 $e$ 的一个变量，它代表函数自身。
- $e$ 是递归函数的主体。

递归的核心在于，**展开转换**允许我们将递归表达式转换为包含自引用的形式：

$$
\text{fix}[\tau](x.e) \rightarrow [\text{fix}[\tau](x.e)/x]e
$$

#### **公式解析**：
- **$\text{fix}[\tau](x.e)$**：表示一个类型为 $\tau$ 的递归函数，其中 $x$ 代表函数自身。
- **$[\,\text{fix}[\tau](x.e)/x\,]e$**：表示在 $e$ 中，将 $x$ 替换为整个递归表达式 $\text{fix}[\tau](x.e)$。
- **$\rightarrow$**：表示计算步骤，也叫转化规则。

这意味着递归函数在每次调用时，会将自身替换到表达式中，从而实现自引用。

#### **具体例子：阶乘函数**

假设我们使用递归定义阶乘函数：

$$
\text{fix}[\text{Int} \rightarrow \text{Int}](f. \lambda n. \text{if } n = 0 \text{ then } 1 \text{ else } n \times f(n-1))
$$

1. $\text{fix}$ 定义了一个递归函数。
2. $f$ 是函数自身的引用。
3. 表达式 $e$ 是递归函数的主体，即阶乘的定义。

展开这个递归函数时：

$$
\text{fix}[\text{Int} \rightarrow \text{Int}](f. \lambda n. \text{if } n = 0 \text{ then } 1 \text{ else } n \times f(n-1))
\rightarrow \lambda n. \text{if } n = 0 \text{ then } 1 \text{ else } n \times (\text{fix}[\text{Int} \rightarrow \text{Int}](f. \lambda n. \text{if } n = 0 \text{ then } 1 \text{ else } n \times f(n-1)))(n-1)
$$

每次递归调用时，都会将函数自身替换进表达式中，直到递归结束。

---

### **2. 自引用变量的理解**

在递归表达式中，变量 $x$ 代表表达式自身。通过 $x$，函数可以在执行过程中引用自身。

- 这种自引用变量类似于在许多编程语言中使用的**`self`** 或 **`this`** 关键字。例如，在 Python 的类方法中，`self` 代表类的实例，允许方法引用和调用其他类方法。
  
#### **例子：Python 中的递归**

```python
def factorial(n):
    if n == 0:
        return 1
    else:
        return n * factorial(n - 1)
```

在这个 Python 例子中，`factorial` 函数通过名字 `factorial` 自引用来进行递归。类似地，在递归表达式 $\text{fix}[\tau](x.e)$ 中，$x$ 就像 Python 中的 `factorial` 函数名，代表函数自身。

#### **对比**：
- 在编程语言中，递归函数通过函数名调用自身（如 Python 中的 `factorial`）。
- 在类型理论中，递归通过 $\text{fix}$ 运算符和变量 $x$ 来实现自引用。

---

### **3. 从递归类型导出通用递归**

递归类型可以用来定义具有自引用特性的函数和数据结构。在这些递归类型中，**自引用变量** $x$ 允许我们表达通用递归函数。

#### **推导：递归类型到通用递归**

递归类型可以通过定义**自引用表达式**来推导通用递归。假设我们有以下自引用类型和表达式：

1. **类型语法**：

   $$\text{Typ}\ \tau ::= \text{self}(\tau)\ \tau\ \text{self}$$

   - $\text{self}(\tau)$：表示类型为 $\tau$ 的自引用表达式。

2. **表达式语法**：

   $$\text{Exp}\ e ::= \text{self}[\tau](x.e)$$

   - $\text{self}[\tau](x.e)$：定义了一个自引用表达式，其中 $x$ 是 $e$ 的自引用。

#### **具体例子：递归列表**

假设我们有一个递归定义的列表类型 `List`，可以表示为自引用类型：

$$
\text{List} = \text{self}(\text{List}) (\text{Nil} + (\text{Cons} \times \text{List}))
$$

这里的 `List` 是通过自引用定义的递归类型。`Nil` 表示空列表，`Cons` 表示递归构造列表。

在表达式层面，我们可以定义递归列表的构造与操作。通过 $\text{self}$ 关键字，我们可以在定义列表时引用自身。

---

### **4. 自引用与展开操作**

自引用变量的递归展开操作 $\text{unroll}(e)$，可以将自引用表达式解构为基础类型。这类似于我们在递归过程中对表达式进行展开。

#### **展开自引用的例子**

对于递归列表类型 `List`，我们可以通过 $\text{unroll}$ 操作将递归表达式展开：

$$
\text{unroll}(\text{self}[\text{List}](x.e)) = [\text{self}[\text{List}](x.e)/x]e
$$

这与我们前面讨论的 $\text{fix}$ 展开转换类似。在递归展开过程中，每次调用都会将自引用变量替换为整个表达式，从而继续递归。

---

### **总结**

- **递归表达式 $\text{fix}[\tau](x.e)$**：表示类型为 $\tau$ 的递归函数，其中 $x$ 是自引用变量。通过 $\text{fix}$ 运算符实现自引用，从而支持递归调用。
- **展开转换**：通过 $[\,\text{fix}[\tau](x.e)/x\,]e$，我们将自引用变量替换为表达式自身，从而实现递归展开。
- **自引用变量**：在递归表达式中，$x$ 代表函数自身，类似于许多编程语言中的 `self` 或 `this` 关键字。
- **递归类型**：递归类型通过自引用来表达复杂的递归数据结构，如列表、树等。通过 $\text{self}$ 关键字定义递归结构。
- **展开与折叠操作**：自引用表达式可以通过 $\text{unroll}$ 操作进行展开，从而解构递归结构。

### --------------------------------------

---

### **静态语义 (Statics)**

这些构造的静态语义由以下规则给出：

#### **规则 (16.5a)：自引用表达式的类型规则**

$$
\frac{
  \Gamma,\ x : \text{self}(\tau) \vdash e : \tau
}{
  \Gamma \vdash \text{self}[\tau](x.e) : \text{self}(\tau)
}
\quad (16.5a)
$$

**公式解析**：

- **$\Gamma$**：类型环境。
- **$x : \text{self}(\tau)$**：在类型环境中，$x$ 被赋予类型 $\text{self}(\tau)$。
- **$e : \tau$**：在类型环境 $\Gamma,\ x : \text{self}(\tau)$ 下，表达式 $e$ 的类型为 $\tau$。
- **结论**：$\text{self}[\tau](x.e)$ 的类型为 $\text{self}(\tau)$。

**解释**：如果在 $e$ 中，$x$ 具有自引用类型 $\text{self}(\tau)$，并且 $e$ 的类型为 $\tau$，那么 $\text{self}[\tau](x.e)$ 就是一个类型为 $\text{self}(\tau)$ 的自引用表达式。

#### **规则 (16.5b)：展开自引用表达式的类型规则**

$$
\frac{
  \Gamma \vdash e : \text{self}(\tau)
}{
  \Gamma \vdash \text{unroll}(e) : \tau
}
\quad (16.5b)
$$

**解释**：如果 $e$ 的类型为 $\text{self}(\tau)$，那么 $\text{unroll}(e)$ 的类型为 $\tau$。

---

### **动态语义 (Dynamics)**

动态语义由以下**展开自引用的规则**给出：

#### **规则 (16.6a)：自引用表达式是值**

$$
\text{self}[\tau](x.e)\ \text{val}
\quad (16.6a)
$$

**解释**：$\text{self}[\tau](x.e)$ 是一个值。

#### **规则 (16.6b)：展开操作的计算规则**

$$
\frac{
  e \rightarrow e'
}{
  \text{unroll}(e) \rightarrow \text{unroll}(e')
}
\quad (16.6b)
$$

**解释**：如果 $e$ 计算到 $e'$，那么 $\text{unroll}(e)$ 计算到 $\text{unroll}(e')$。

#### **规则 (16.6c)：展开自引用表达式**

$$
\text{unroll}(\text{self}[\tau](x.e)) \rightarrow [\text{self}[\tau](x.e)/x] e
\quad (16.6c)
$$

**公式解析**：

- **$[\text{self}[\tau](x.e)/x] e$**：在 $e$ 中，将 $x$ 替换为 $\text{self}[\tau](x.e)$。

**解释**：展开自引用表达式时，将表达式自身替换到其主体中，从而实现自引用。

---

### **与通用递归的比较**

主要的区别在于，我们区分了一个自引用表达式的类型 $\text{self}(\tau)$，而不是在每个类型上都强加自引用。然而，正如我们接下来会看到的，自引用类型足以实现通用递归，因此区别主要在于技术上的处理方式。

---

### **使用递归类型定义自引用类型**

正如之前所建议的，我们可以将类型 $\text{self}(\tau)$ 从递归类型中定义出来。关键在于将类型为 $\tau$ 的自引用表达式视为一个以表达式自身为参数的函数。也就是说，我们希望定义类型 $\text{self}(\tau)$，使其满足以下**同构 (Isomorphism)**：

$$
\text{self}(\tau) \cong \text{self}(\tau) \rightarrow \tau.
$$

**解释**：

- **$\text{self}(\tau)$**：自引用类型。
- **$\text{self}(\tau) \rightarrow \tau$**：从自引用类型到 $\tau$ 的函数类型。

这意味着我们在寻找类型算子 $t . t \rightarrow \tau$ 的一个**固定点 (Fixed Point)**，其中 $t \notin \tau$，即 $t$ 是我们所讨论的类型变量，不在 $\tau$ 中出现。

所需的固定点正是**递归类型 (Recursive Type)**：

$$
\text{rec}(t . t \rightarrow \tau),
$$

因此，我们将 $\text{self}(\tau)$ 定义为：

$$
\text{self}(\tau) = \text{rec}(t . t \rightarrow \tau).
$$

---

### **自引用表达式的定义**

自引用表达式 $\text{self}[\tau](x.e)$ 可以定义为：

$$
\text{self}[\tau](x.e) = \text{fold}(\lambda (x : \text{self}(\tau)) . e).
$$

**公式解析**：

- **$\lambda (x : \text{self}(\tau)) . e$**：一个以 $x$ 为参数的函数，$x$ 的类型为 $\text{self}(\tau)$，函数体为 $e$。
- **$\text{fold}$**：折叠操作，将类型展开的值折叠为递归类型。

**验证规则 (16.5a)**：根据此定义，我们可以容易地验证规则 (16.5a)。

### **展开操作的定义**

对应地，展开操作 $\text{unroll}(e)$ 定义为：

$$
\text{unroll}(e) = \text{unfold}(e)(e).
$$

**解释**：

- **$\text{unfold}(e)$**：展开递归类型，得到一个函数，类型为 $\text{self}(\tau) \rightarrow \tau$。
- **$(e)$**：将 $e$ 作为参数，应用于函数 $\text{unfold}(e)$。

**验证规则 (16.5b)**：根据此定义，我们可以验证规则 (16.5b)。

### **展开自引用表达式的计算**

此外，我们可以验证：

$$
\text{unroll}(\text{self}[\tau](y.e)) \rightarrow^* [\text{self}[\tau](y.e)/y]e.
$$

**解释**：

- **$\rightarrow^*$**：表示经过若干步计算。
- 这完成了类型为 $\tau$ 的自引用表达式的类型 $\text{self}(\tau)$ 的推导。

---

### **使用自引用类型定义通用递归**

引入自引用类型 $\text{self}(\tau)$ 的一个结果是，我们可以使用它来为任何类型定义通用递归。

具体来说，我们可以将 $\text{fix}[\tau](x.e)$ 定义为：

$$
\text{fix}[\tau](x.e) = \text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e)).
$$

**公式解析**：

- **$\text{self}[\tau](y.[\text{unroll}(y)/x] e)$**：构造一个自引用表达式，其中 $y$ 是自引用的，$e$ 中的 $x$ 被 $\text{unroll}(y)$ 替换。
- **$\text{unroll}$**：展开操作，将自引用表达式展开。
- **$[\text{unroll}(y)/x] e$**：在 $e$ 中，将 $x$ 替换为 $\text{unroll}(y)$。

**验证静态语义**：可以容易地验证这符合第10章中给出的通用递归的静态语义。

### **验证动态语义**

此外，它也验证了动态语义，如以下推导所示：

$$
\begin{align*}
\text{fix}[\tau](x.e) &= \text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e)) \\
&\rightarrow^* [\text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e))/x] e \\
&= [\text{fix}[\tau](x.e)/x] e.
\end{align*}
$$

**解释**：

- **第一步**：我们展开 $\text{fix}[\tau](x.e)$ 的定义。
- **第二步**：通过展开计算 $\text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e))$，得到 $[\text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e))/x] e$。
- **第三步**：注意到 $\text{unroll}(\text{self}[\tau](y.[\text{unroll}(y)/x] e)) = \text{fix}[\tau](x.e)$，因此替换后得到 $[\text{fix}[\tau](x.e)/x] e$。

---

### **递归类型的影响**

由此可见，递归类型可用于为每种类型定义一个**非终止表达式 (Non-Terminating Expression)**，即：

$$
\text{fix}[\tau](x.x).
$$

与我们考虑的许多其他类型构造不同，递归类型改变了**每种类型**的含义，而不仅仅是那些涉及递归的类型。因此，递归类型被认为是对语言（如 $L\{\text{nat} \rightarrow\}$）的**非保守扩展 (Non-Conservative Extension)**，因为它们引入了新的非终止计算，而这些计算在原语言中不存在。

---

## **总结**

- **自引用 (Self-Reference)**：允许表达式在其自身内部引用自身，实现递归计算。
- **自引用类型 $\text{self}(\tau)$**：可以使用递归类型来定义，满足同构 $\text{self}(\tau) \cong \text{self}(\tau) \rightarrow \tau$。
- **通用递归的实现**：通过自引用类型，我们可以实现通用递归，并验证其静态语义和动态语义。
- **递归类型的影响**：递归类型改变了所有类型的含义，引入了新的非终止计算。

---

## **练习与思考**

**练习1**：

- **证明**：使用递归类型定义的 $\text{self}(\tau)$ 满足同构 $\text{self}(\tau) \cong \text{self}(\tau) \rightarrow \tau$。
- **提示**：考虑 $\text{self}(\tau) = \text{rec}(t . t \rightarrow \tau)$，使用展开和折叠操作验证同构。

**练习2**：

- **实现一个函数**，使用自引用类型和通用递归，计算阶乘函数 $\text{fact}(n)$。
- **要求**：定义 $\text{fact}(n)$，使用 $\text{fix}$ 或自引用类型，不使用显式的递归函数定义。

**思考**：

- **递归类型的引入如何影响类型系统的性质**，例如类型检查的可判定性和类型安全性？
- **递归类型是否会引入新的安全隐患**，如非终止计算导致的程序挂起？如何在实践中处理这些问题？

---

希望以上详细的解释和公式解析能够帮助您深入理解 **16.3 自引用 (Self-Reference)** 的内容。如有任何疑问，欢迎提问！


--------------------------------------
# **第16章 递归类型 (Recursive Types)**

## **16.4 状态的起源 (The Origin of State)**

在本节中，我们将探讨计算中的**状态 (State)** 概念的起源，特别是它与递归或自引用的关系。正如我们已经看到的，递归类型引出了自引用的概念，而状态的本质也源于此。

---

### **状态与递归的关系**

**状态 (State)** 是指在计算过程中可变的部分，即系统在某一时刻的信息。状态的概念在第十三部分中将被深入讨论，但它的起源可以追溯到递归或自引用的概念，这正是从递归类型中产生的。

#### **硬件层面的示例：触发器 (Flip-Flop) 或锁存器 (Latch)**

- **触发器 (Flip-Flop)** 和 **锁存器 (Latch)** 是硬件电路中的概念。
  
- **特点**：这些电路由组合逻辑元件（通常是 NOR 门或 NAND 门）构建，具有维持可变状态的特性。

- **行为**：例如，一个 RS 锁存器 (RS Latch) 可以根据 R 或 S 输入信号的变化，保持其输出为逻辑电平 0 或 1，并在一个短暂的稳定延迟后响应。

- **实现方式**：这种行为是通过**反馈 (Feedback)** 实现的，反馈是一种自引用或递归的形式：门的输出反馈到其输入，以便将门的当前状态传递给决定其下一状态的逻辑。

---

### **使用递归类型建模 RS 锁存器**

我们可以使用递归类型来建模 RS 锁存器 (RS Latch)，通过显式地表示时间的流逝，即当前输出是输入和先前输出的函数。

#### **类型定义**

定义一个 RS 锁存器的类型 $\tau_{\text{rsl}}$，如下：

$$
\tau_{\text{rsl}} = \mu t.\ \langle X \mapsto \text{bool},\ Q \mapsto \text{bool},\ N \mapsto t \rangle.
$$

**公式解析**：

- **$\mu t.$**：表示一个递归类型，$t$ 是类型变量。

- **$\langle X \mapsto \text{bool},\ Q \mapsto \text{bool},\ N \mapsto t \rangle$**：一个记录类型，包含三个字段：

  - **$X$**：表示锁存器的当前输出之一，类型为 $\text{bool}$。

  - **$Q$**：表示锁存器的当前状态，类型为 $\text{bool}$。

  - **$N$**：表示锁存器的下一状态，类型为 $t$（递归地引用自身的类型）。

#### **访问器的定义**

如果 $e$ 是类型为 $\tau_{\text{rsl}}$ 的表达式，那么我们定义：

- **$e @ X$**：表示 $\text{unfold}(e) \cdot X$，即展开 $e$，然后获取字段 $X$。

- **$e @ Q$** 和 **$e @ N$** 类似。

**解释**：$e @ X$ 和 $e @ Q$ 计算锁存器 $e$ 的“当前”输出，$e @ N$ 计算另一个锁存器，表示根据“当前”状态确定的“下一”状态。

#### **锁存器的递归函数定义**

对于给定的输入值 $r$ 和 $s$，可以通过递归函数 $\text{rsl}$ 从旧的锁存器计算新的锁存器，定义如下：

$$
\text{fix } \text{rsl} = \lambda (o : \tau_{\text{rsl}}).\ \text{fix this is } e_{\text{rsl}},
$$

其中，$e_{\text{rsl}}$ 定义为：

$$
\text{fold}(\langle X \mapsto \text{nor}(\langle s,\ o @ Q \rangle),\ Q \mapsto \text{nor}(\langle r,\ o @ X \rangle),\ N \mapsto \text{rsl}(this) \rangle)
$$

**公式解析**：

- **$\text{fix } \text{rsl}$**：定义一个递归函数 $\text{rsl}$。

- **$\lambda (o : \tau_{\text{rsl}})$**：$\text{rsl}$ 接受一个类型为 $\tau_{\text{rsl}}$ 的参数 $o$，表示旧的锁存器状态。

- **$\text{fix this is } e_{\text{rsl}}$**：在 $\text{rsl}$ 的定义中，我们使用 $\text{fix}$ 定义了一个自引用的表达式 $this$。

- **$e_{\text{rsl}}$**：构造了一个新的锁存器状态，包括当前输出 $X$、当前状态 $Q$、下一状态 $N$。

- **$\text{nor}$**：一个对布尔值执行 NOR 操作的函数。

- **$o @ Q$** 和 **$o @ X$**：获取旧锁存器的当前状态和输出。

- **$\text{rsl}(this)$**：递归地计算下一状态。

**注意**：为了简化，我们将 $R$ 和 $S$ 输入固定，即当这些输入改变时，我们需要构建一个新的锁存器。可以修改构造，使得在计算锁存器的下一状态时可以提供新的 $R$ 和 $S$ 输入，从而允许这些输入随时间变化。

#### **初始化锁存器**

为了开始构建，我们定义锁存器的初始状态，其中输出被任意地设置为 $\text{false}$，其下一状态通过将 $\text{rsl}$ 应用于初始状态来确定：

$$
\text{fix this is } \text{fold}(\langle X \mapsto \text{false},\ Q \mapsto \text{false},\ N \mapsto \text{rsl}(this) \rangle).
$$

**解释**：

- **$\text{fix this is } \cdots$**：定义一个自引用的初始状态。

- **$\text{fold}(\langle \cdots \rangle)$**：构造锁存器的初始状态。

- **$X$ 和 $Q$**：初始输出和状态，设为 $\text{false}$。

- **$N$**：下一状态，通过应用 $\text{rsl}$ 于 $this$ 计算。

#### **状态的维持**

选择锁存器的 $N$ 组件会导致根据当前输出重新计算输出。请注意，自引用在维持锁存器状态中起到了关键作用。

---

### **显式和隐式的时间建模**

上述锁存器的实现通过提供锁存器的 $N$ 组件，显式地建模了时间，即从当前状态计算下一状态。

也可以**隐式地建模时间**，将锁存器视为一个**转换器 (Transducer)**，其输入和输出是随时间变化的信号。

#### **信号的表示**

- **信号 (Signal)** 可以表示为布尔值的**流 (Stream)**。

- 可以使用第15章中描述的流类型，或本章之前讨论的通用递归类型。

#### **转换器的定义**

- **转换器 (Transducer)** 是一个**流变换器 (Stream Transformer)**，通过将函数应用于输入的连续元素，来计算输出的连续元素。

#### **对比**

- **隐式形式**：更自然地建模时间，信号随着时间变化，转换器处理信号。

- **相似性**：无论是显式建模还是隐式建模，都依赖于递归类型和自引用。

---

## **16.5 注解 (Notes)**

### **递归类型的系统性研究**

- **起源**：对编程中递归类型的系统性研究始于 **Scott (1976, 1982)**，以提供对无类型 $\lambda$-演算的数学模型。

- **递归类型与递归的关系**：从递归类型中推导出递归，实质上是应用了 Scott 的理论，在由递归类型给出的 $\lambda$-演算模型中找到不动点组合子 (Fixed Point Combinator) 的解释。

### **范畴论视角的递归类型**

- **研究者**：Wand (1979) 和 Smyth & Plotkin (1982) 发展了递归类型的范畴论视角。

### **使用自引用实现状态**

- **数字逻辑中的基础**：使用自引用实现状态是数字逻辑的基础。

- **相关研究**：Abadi 和 Cardelli (1996)，Cook (2009) 等人探讨了类似的思想来建模对象。

- **信号作为流的观点**：受 Kahn (MacQueen, 2009) 的开创性工作启发，将信号视为流。

---

## **总结**

- **状态的起源**：状态的概念源于递归或自引用，通过反馈机制实现状态的维持。

- **递归类型的应用**：使用递归类型可以建模硬件电路中的状态，如锁存器。

- **显式 vs. 隐式时间建模**：

  - **显式建模**：通过递归类型的下一状态组件，明确表示时间的流逝。

  - **隐式建模**：将系统视为处理随时间变化的信号的转换器。

- **自引用的重要性**：自引用在维持状态和建模递归计算中起到了关键作用。

---

## **练习与思考**

**练习1**：

- **设计一个锁存器的类型和实现**，允许 $R$ 和 $S$ 输入随时间变化。

  - **提示**：修改 $\text{rsl}$ 的定义，使其接受新的 $R$ 和 $S$ 输入，可能需要将它们表示为信号的流。

**练习2**：

- **使用流和转换器的概念**，实现一个简单的数字滤波器，处理输入信号流，输出处理后的信号流。

  - **提示**：定义一个流变换器，使用递归类型和自引用，实现信号的处理逻辑。

**思考**：

- **递归类型和自引用在现代编程语言中的应用**：

  - **对象模型**：如 Abadi 和 Cardelli 的工作，如何使用递归类型建模对象和类。

  - **并发和分布式系统**：信号作为流的观点如何影响并发系统的设计。

- **状态的管理**：

  - **纯函数式编程 vs. 命令式编程**：如何在纯函数式编程中模拟状态变化。

  - **类型系统的扩展**：如何在类型系统中安全地引入状态和递归类型，保持类型安全性。

---

**参考文献**：

- **Scott, D. (1976, 1982)**：对无类型 $\lambda$-演算的数学模型研究。

- **Wand, M. (1979)**，**Smyth, M. & Plotkin, G. (1982)**：递归类型的范畴论研究。

- **Abadi, M. & Cardelli, L. (1996)**，**Cook, W. (2009)**：使用递归类型和自引用建模对象的研究。

- **MacQueen, D. (2009)**：受 Kahn 的工作启发，将信号视为流。

希望以上详细的解释和公式解析能够帮助您深入理解 **16.4 状态的起源 (The Origin of State)** 和 **16.5 注解 (Notes)** 的内容。如有任何疑问，欢迎提问！

### -------------------------------------------



### **类型等式的最小解与最大解**

在类型论和范畴论中，**类型等式的最小解（Least Fixed Point, LFP）**和**最大解（Greatest Fixed Point, GFP）**是递归数据类型的重要概念。这些概念用于描述递归类型（如列表、树、流等）的定义和构造方式，它们分别对应于**归纳类型**和**余归纳类型**。

我们通过具体例子来理解这两个解。

---

### **1. 最小解（Least Fixed Point, LFP）**

**最小解**通常用于定义**归纳类型**，它表示递归类型定义中的**有限解**，即递归过程最终会在某个点上终止。这种递归类型的元素是通过**有限次的构造**生成的。

#### **解释**：
- **最小解**意味着递归类型的元素只能通过有限次应用构造器生成。
- 这种定义方法对应于递归的**最小固定点**，它是递归类型的“最小”解决方案。
- 最小解常用于描述**有限数据结构**，如列表、有限树、自然数等。

#### **例子：列表的归纳定义**

假设我们有一个递归定义的列表类型 `List`：

```haskell
data List a = Nil | Cons a (List a)
```

- `Nil` 是列表的基底情况，表示空列表。
- `Cons a (List a)` 是递归构造器，它将一个元素 `a` 加入列表。

这里，`List a` 的定义就是一个**最小解**。通过递归地应用 `Cons` 构造器，最终我们会遇到 `Nil`，这是递归的终止点。因此，`List a` 是通过有限次应用构造器生成的。

- **类型等式**：列表的递归定义可以用类型等式表示为：
  $$
  \text{List}(a) = 1 + (a \times \text{List}(a))
  $$
  - 这里，`1` 对应于 `Nil`，`a \times \text{List}(a)` 对应于 `Cons a (List a)`。

- **最小解**：最小解表示递归最终会终止，形成一个有限的列表结构。

---

### **2. 最大解（Greatest Fixed Point, GFP）**

**最大解**用于定义**余归纳类型**，它表示递归类型的**无限解**，即递归过程可以**无限继续**，而不必在某个点上终止。最大解通常用于表示**无限数据结构**，如流（streams）、无限树等。

#### **解释**：
- **最大解**意味着递归类型的元素可以通过**无限次的递归构造**生成。
- 最大解对应递归的**最大固定点**，它是递归类型的“最大”解决方案。
- 最大解常用于描述那些**无限生成**的数据结构，允许递归结构**无限展开**。

#### **例子：流的余归纳定义**

假设我们有一个定义无限流的递归类型 `Stream`：

```haskell
data Stream a = Cons a (Stream a)
```

- `Stream a` 的定义允许我们生成一个无限长的序列，每次递归调用都返回一个新元素，并且流没有终点。
- **流**与列表类似，但它**没有终止条件**，它允许递归无限进行。

- **类型等式**：流的递归定义可以用类型等式表示为：
  $$
  \text{Stream}(a) = a \times \text{Stream}(a)
  $$

- **最大解**：这里的最大解表示流是**无限递归**的，递归过程永远不会终止。通过协递归（co-recursion），我们可以在流中无限生成元素。

#### **具体实现**（Python 示例）：

```python
# 定义无限流的生成器
def stream(start=0):
    while True:
        yield start
        start += 1

# 使用生成器生成无限自然数流
s = stream(0)

# 获取前 5 个流中的元素
print([next(s) for _ in range(5)])  # 输出: [0, 1, 2, 3, 4]
```

在这个例子中，`stream` 是通过协递归实现的无限流，每次调用都会生成下一个自然数。这正是最大解的典型例子，流的生成过程是**无限的**。

---

### **3. 最小解与最大解的对比**

| **属性**     | **最小解 (Least Fixed Point, LFP)**          | **最大解 (Greatest Fixed Point, GFP)**         |
| ------------ | -------------------------------------------- | ---------------------------------------------- |
| **类型结构** | 递归类型的**有限解**，即递归过程最终会终止。 | 递归类型的**无限解**，递归过程可以无限展开。   |
| **数据结构** | **有限数据结构**，如列表、自然数、有限树等。 | **无限数据结构**，如流、无限树、无限状态机等。 |
| **递归特性** | 通过有限次递归生成的类型。                   | 通过无限次递归生成的类型。                     |
| **终止性**   | 递归在某个点上终止。                         | 递归没有终点，可能无限进行。                   |
| **适用场景** | 用于有限数据结构的建模。                     | 用于无限数据结构或持续生成数据的场景。         |

### **4. 最小解与最大解的数学解释**

在数学上，最小解和最大解是递归定义的**固定点**（Fixed Points）。递归类型的定义可以看作是**类型等式**，其解是递归类型的固定点。

- **最小解（LFP）**：是类型等式的**最小固定点**，表示最小的递归结构。所有的递归过程都必须在某个点上终止。
- **最大解（GFP）**：是类型等式的**最大固定点**，表示最大的递归结构，允许递归过程无限展开。

#### **递归类型的固定点方程**：

递归类型通常通过类型等式定义，例如：

$$
X = F(X)
$$

这里，$F$ 是一个递归定义的函数或函子。

- **最小解**是这个方程的最小固定点，它对应于有限的递归结构。
- **最大解**是这个方程的最大固定点，它对应于无限递归结构。

---

### **总结**

- **最小解 (Least Fixed Point, LFP)**：适用于递归过程**终止**的数据结构，典型例子是**有限列表**、**自然数**等。递归结构通过有限次的应用构造器生成，最终会停止。
  
- **最大解 (Greatest Fixed Point, GFP)**：适用于递归过程**无限进行**的数据结构，典型例子是**流（streams）**、**无限树**等。递归结构允许无限生成元素，不需要终止。

最小解与最大解反映了递归类型定义中的两种不同的解决方案：前者适用于有限结构，后者适用于无限结构。

### -------------------------------------
