[toc]





### More: 简单类型 λ-演算的更多构造

在这一章中，我们将扩展简单类型 λ-演算，引入更多的构造，包括原语数字、let 绑定、积、和、单元类型、空类型、列表等。这些构造在编程语言中广泛使用，通过它们可以更灵活和丰富地表达程序。

#### 原语数字 (Primitive Numbers)

##### 语法

我们首先定义自然数类型 `Nat`，以及用于表示自然数的常量和乘法操作。

$$
\begin{aligned}
A, B, C &::= \ldots \mid \text{Nat} \quad \text{（类型）} \\
L, M, N &::= \ldots \mid \text{con } c \mid L `* M \quad \text{（项）} \\
V, W &::= \ldots \mid \text{con } c \quad \text{（值）}
\end{aligned}
$$

- $$\text{Nat}$$: 表示自然数类型。
- $$\text{con } c$$: 表示常量 $$c$$，其中 $$c$$ 是一个自然数。
- $$L `* M$$: 表示项 $$L$$ 和 $$M$$ 的乘法。

##### 赋型 (Typing)

赋型规则定义了常量和乘法的类型。

$$
\frac{c : \mathbb{N}}{\Gamma \vdash \text{con } c : \text{Nat}} \quad \text{（con 规则）}
$$

$$
\frac{\Gamma \vdash L : \text{Nat} \quad \Gamma \vdash M : \text{Nat}}{\Gamma \vdash L `* M : \text{Nat}} \quad \text{（乘法规则）}
$$

- $$\text{con } c$$ 的类型是 $$\text{Nat}$$，前提是 $$c$$ 是一个自然数。
- 如果 $$L$$ 和 $$M$$ 都是 $$\text{Nat}$$ 类型，那么 $$L `* M$$ 也是 $$\text{Nat}$$ 类型。

##### 归约 (Reduction)

归约规则定义了自然数项的求值方式。

$$
\frac{L \rightarrow L'}{L `* M \rightarrow L' `* M} \quad \text{（ξ-*₁规则）}
$$

$$
\frac{M \rightarrow M'}{V `* M \rightarrow V `* M'} \quad \text{（ξ-*₂规则）}
$$

$$
\text{con } c `* \text{con } d \rightarrow \text{con } (c * d) \quad \text{（δ-规则）}
$$

- 如果 $$L$$ 可以归约为 $$L'$$，那么 $$L `* M$$ 可以归约为 $$L' `* M$$。
- 如果 $$M$$ 可以归约为 $$M'$$，而 $$V$$ 是一个值，那么 $$V `* M$$ 可以归约为 $$V `* M'$$。
- 两个自然数常量相乘的结果是它们的乘积。

##### 例子

下面是一个计算立方的函数：

```haskell
cube : ∅ ⊢ Nat ⇒ Nat
cube = ƛ x ⇒ x `* x `* x
```

这个函数 $$\text{cube}$$ 接受一个自然数 $$x$$ 并返回它的立方，即 $$x \times x \times x$$。

#### Let 绑定

##### 语法

`let` 绑定引入了一种新的项构造：

$$
L, M, N ::= \ldots \mid \text{let } x = M \text{ in } N
$$

这里，$$M$$ 是被绑定的表达式，$$x$$ 是局部变量，$$N$$ 是在绑定了 $$x$$ 的环境下进行求值的表达式。

##### 赋型 (Typing)

$$
\frac{\Gamma \vdash M : A \quad \Gamma, x : A \vdash N : B}{\Gamma \vdash \text{let } x = M \text{ in } N : B} \quad \text{（let 规则）}
$$

- 首先推导 $$M$$ 的类型为 $$A$$，然后在环境中加入 $$x : A$$ 后推导 $$N$$ 的类型为 $$B$$，则 `let x = M in N` 的类型为 $$B$$。

##### 归约 (Reduction)

$$
\frac{M \rightarrow M'}{\text{let } x = M \text{ in } N \rightarrow \text{let } x = M' \text{ in } N} \quad \text{（ξ-let规则）}
$$

$$
\text{let } x = V \text{ in } N \rightarrow N[x := V] \quad \text{（β-let规则）}
$$

- 当 $$M$$ 可以归约为 $$M'$$ 时，`let x = M in N` 归约为 `let x = M' in N`。
- 当 $$M$$ 是一个值 $$V$$ 时，`let x = V in N` 直接归约为 $$N$$，并且在 $$N$$ 中将 $$x$$ 替换为 $$V$$。

##### 例子

计算十次方的函数：

```haskell
exp10 : ∅ ⊢ Nat ⇒ Nat
exp10 = ƛ x ⇒ let x2  = x  * x  in
              let x4  = x2 * x2 in
              let x5  = x4 * x  in
              x5 * x5
```

这个函数 $$\text{exp10}$$ 使用了三次 `let` 绑定，分别计算 $$x^2$$、$$x^4$$、$$x^5$$，最后返回 $$x^{10}$$ 的值。

##### 翻译

我们可以将 `let` 绑定翻译为一个 λ-抽象和应用：

$$
(\text{let } x = M \text{ in } N) † = (ƛ x ⇒ (N †)) · (M †)
$$

这里，$$M †$$ 和 $$N †$$ 表示将 $$M$$ 和 $$N$$ 翻译为不含 `let` 绑定的项。

#### 积 (Product)

##### 语法

积类型允许我们将两个项组合成一个有序对。

$$
\begin{aligned}
A, B, C &::= \ldots \mid A `× B \quad \text{（类型）} \\
L, M, N &::= \ldots \mid \text{⟨ } M , N \text{ ⟩} \mid \text{proj₁ } L \mid \text{proj₂ } L \quad \text{（项）} \\
V, W &::= \ldots \mid \text{⟨ } V , W \text{ ⟩} \quad \text{（值）}
\end{aligned}
$$

- $$A `× B$$: 表示两个类型的笛卡尔积。
- $$\text{⟨ } M , N \text{ ⟩}$$: 表示有序对。
- $$\text{proj₁ } L$$ 和 $$\text{proj₂ } L$$: 表示投影运算，分别提取有序对的第一和第二个分量。

##### 赋型 (Typing)

$$
\frac{\Gamma \vdash M : A \quad \Gamma \vdash N : B}{\Gamma \vdash \text{⟨ } M , N \text{ ⟩} : A `× B} \quad \text{（⟨_,_⟩规则）}
$$

$$
\frac{\Gamma \vdash L : A `× B}{\Gamma \vdash \text{proj₁ } L : A} \quad \text{（proj₁规则）}
$$

$$
\frac{\Gamma \vdash L : A `× B}{\Gamma \vdash \text{proj₂ } L : B} \quad \text{（proj₂规则）}
$$

- 有序对 $$\text{⟨ } M , N \text{ ⟩}$$ 的类型是 $$A `× B$$，其中 $$M$$ 和 $$N$$ 分别具有类型 $$A$$ 和 $$B$$。
- 投影 $$\text{proj₁ } L$$ 的类型是 $$A$$，如果 $$L$$ 的类型是 $$A `× B$$。
- 投影 $$\text{proj₂ } L$$ 的类型是 $$B$$，如果 $$L$$ 的类型是 $$A `× B$$。

##### 归约 (Reduction)

$$
\frac{M \rightarrow M'}{\text{⟨ } M , N \text{ ⟩} \rightarrow \text{⟨ } M' , N \text{ ⟩}} \quad \text{（ξ-⟨,⟩₁规则）}
$$

$$
\frac{N \rightarrow N'}{\text{⟨ } V , N \

text{ ⟩} \rightarrow \text{⟨ } V , N' \text{ ⟩}} \quad \text{（ξ-⟨,⟩₂规则）}
$$

$$
\frac{L \rightarrow L'}{\text{proj₁ } L \rightarrow \text{proj₁ } L'} \quad \text{（ξ-proj₁规则）}
$$

$$
\frac{L \rightarrow L'}{\text{proj₂ } L \rightarrow \text{proj₂ } L'} \quad \text{（ξ-proj₂规则）}
$$

$$
\text{proj₁ } \text{⟨ } V , W \text{ ⟩} \rightarrow V \quad \text{（β-proj₁规则）}
$$

$$
\text{proj₂ } \text{⟨ } V , W \text{ ⟩} \rightarrow W \quad \text{（β-proj₂规则）}
$$

- 有序对的归约规则分为两种情况：$$M$$ 归约时和 $$N$$ 归约时。
- 投影运算 $$\text{proj₁ } L$$ 和 $$\text{proj₂ } L$$ 的归约规则同样分为两种情况：$$L$$ 归约时，以及当 $$L$$ 是一个有序对时。

##### 例子

定义一个交换有序对分量的函数：

```haskell
swap× : ∅ ⊢ A `× B ⇒ B `× A
swap× = ƛ z ⇒ `⟨ `proj₂ z , `proj₁ z ⟩
```

这个函数接收一个有序对并返回它的分量交换后的结果。

#### 积的替代表示方法

##### 语法

积的替代表示方法使用匹配表达式（`case×`），可以同时绑定两个变量，以便在操作有序对时更为简洁。

$$
\begin{aligned}
A, B, C &::= \ldots \mid A `× B \quad \text{（类型）} \\
L, M, N &::= \ldots \mid \text{⟨ } M , N \text{ ⟩} \mid \text{case× } L [⟨ x , y ⟩ ⇒ N ] \quad \text{（项）} \\
V, W &::= \ldots \mid \text{⟨ } V , W \text{ ⟩} \quad \text{（值）}
\end{aligned}
$$

- $$A `× B$$: 表示两个类型的笛卡尔积。
- $$\text{⟨ } M , N \text{ ⟩}$$: 表示有序对。
- $$\text{case× } L [⟨ x , y ⟩ ⇒ N ]$$: 表示匹配有序对 $$L$$，并将其第一分量绑定到变量 $$x$$，第二分量绑定到变量 $$y$$，然后对 $$N$$ 进行计算。

##### 赋型 (Typing)

匹配表达式的赋型规则如下：

$$
\frac{\Gamma \vdash L : A `× B \quad \Gamma , x : A , y : B \vdash N : C}{\Gamma \vdash \text{case× } L [⟨ x , y ⟩ ⇒ N ] : C} \quad \text{（case× 规则）}
$$

- 如果 $$L$$ 的类型是 $$A `× B$$，并且在环境中加入 $$x : A$$ 和 $$y : B$$ 后，$$N$$ 的类型是 $$C$$，那么匹配表达式 $$\text{case× } L [⟨ x , y ⟩ ⇒ N ]$$ 的类型就是 $$C$$。

##### 归约 (Reduction)

匹配表达式的归约规则如下：

$$
\frac{L \rightarrow L'}{\text{case× } L [⟨ x , y ⟩ ⇒ N ] \rightarrow \text{case× } L' [⟨ x , y ⟩ ⇒ N ]} \quad \text{（ξ-case× 规则）}
$$

$$
\text{case× } \text{⟨ } V , W \text{ ⟩} [⟨ x , y ⟩ ⇒ N ] \rightarrow N [ x := V ][ y := W ] \quad \text{（β-case× 规则）}
$$

- 当 $$L$$ 可以归约为 $$L'$$ 时，匹配表达式 $$\text{case× } L [⟨ x , y ⟩ ⇒ N ]$$ 可以归约为 $$\text{case× } L' [⟨ x , y ⟩ ⇒ N ]$$。
- 当 $$L$$ 是一个有序对 $$\text{⟨ } V , W \text{ ⟩}$$ 时，匹配表达式 $$\text{case× } \text{⟨ } V , W \text{ ⟩} [⟨ x , y ⟩ ⇒ N ]$$ 直接归约为 $$N$$，并且在 $$N$$ 中将 $$x$$ 替换为 $$V$$，将 $$y$$ 替换为 $$W$$。

##### 翻译

匹配表达式可以被翻译成使用投影的表达式：

$$
(\text{case× } L [⟨ x , y ⟩ ⇒ N ]) † = \text{let } z = (L †) \text{ in let } x = \text{proj₁ } z \text{ in let } y = \text{proj₂ } z \text{ in } (N †)
$$

这里，$$z$$ 是一个新鲜变量，即 $$z$$ 在 $$N$$ 中没有作为自由变量出现。这个翻译首先对 $$L$$ 进行求值，得到 $$z$$，然后分别对 $$z$$ 进行投影，得到 $$x$$ 和 $$y$$，最后在 $$N$$ 中使用这些绑定。

也可以将投影表达式反向翻译为匹配表达式：

$$
(\text{proj₁ } L) ‡ = \text{case× } (L ‡) [⟨ x , y ⟩ ⇒ x]
$$

$$
(\text{proj₂ } L) ‡ = \text{case× } (L ‡) [⟨ x , y ⟩ ⇒ y]
$$

这种翻译展示了匹配表达式如何在语言中起到替代投影操作的作用，提供了一个更加统一和简洁的操作方式。

##### 例子

用匹配表达式重写的交换有序对分量的函数：

```haskell
swap×-case : ∅ ⊢ A `× B ⇒ B `× A
swap×-case = ƛ z ⇒ case× z [⟨ x , y ⟩ ⇒ `⟨ y , x ⟩]
```

这个函数与前面的 `swap×` 功能相同，但使用了 `case×` 进行匹配，从而避免了显式的投影操作。

---

以上解释涵盖了积类型及其替代表示方法的定义、赋型规则、归约规则，以及相关例子和翻译的详细内容。接下来可以继续讨论和类型的扩展。

### 和类型（Sum Type）

#### 语法

和类型表示一种“非此即彼”的数据结构。其语法如下：

$$
\begin{aligned}
A, B, C &::= \ldots \mid A `⊎ B \quad \text{（类型）} \\
L, M, N &::= \ldots \mid \text{inj₁ } M \mid \text{inj₂ } N \mid \text{case⊎ } L [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ] \quad \text{（项）} \\
V, W &::= \ldots \mid \text{inj₁ } V \mid \text{inj₂ } W \quad \text{（值）}
\end{aligned}
$$

- $$A `⊎ B$$: 表示类型 $$A$$ 或 $$B$$ 的和类型。
- $$\text{inj₁ } M$$ 和 $$\text{inj₂ } N$$: 表示将 $$M$$ 和 $$N$$ 分别注入到和类型的第一个和第二个分量中。
- $$\text{case⊎ } L [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ]$$: 表示对和类型 $$L$$ 进行匹配，如果是 $$\text{inj₁ } V$$，则执行 $$M$$，如果是 $$\text{inj₂ } W$$，则执行 $$N$$。

#### 赋型 (Typing)

和类型的赋型规则如下：

$$
\frac{\Gamma \vdash M : A}{\Gamma \vdash \text{inj₁ } M : A `⊎ B} \quad \text{（inj₁ 规则）}
$$

$$
\frac{\Gamma \vdash N : B}{\Gamma \vdash \text{inj₂ } N : A `⊎ B} \quad \text{（inj₂ 规则）}
$$

$$
\frac{\Gamma \vdash L : A `⊎ B \quad \Gamma , x : A \vdash M : C \quad \Gamma , y : B \vdash N : C}{\Gamma \vdash \text{case⊎ } L [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ] : C} \quad \text{（case⊎ 规则）}
$$

- 若 $$M$$ 的类型是 $$A$$，则 $$\text{inj₁ } M$$ 的类型是 $$A `⊎ B$$。
- 若 $$N$$ 的类型是 $$B$$，则 $$\text{inj₂ } N$$ 的类型是 $$A `⊎ B$$。
- 若 $$L$$ 的类型是 $$A `⊎ B$$，在 $$x$$ 被绑定为 $$A$$ 类型且 $$y$$ 被绑定为 $$B$$ 类型后，$$M$$ 和 $$N$$ 都的类型为 $$C$$，则 $$\text{case⊎ } L [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ]$$ 的类型为 $$C$$。

#### 归约 (Reduction)

和类型的归约规则如下：

$$
\frac{M \rightarrow M'}{\text{inj₁ } M \rightarrow \text{inj₁ } M'} \quad \text{（ξ-inj₁ 规则）}
$$

$$
\frac{N \rightarrow N'}{\text{inj₂ } N \rightarrow \text{inj₂ } N'} \quad \text{（ξ-inj₂ 规则）}
$$

$$
\frac{L \rightarrow L'}{\text{case⊎ } L [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ] \rightarrow \text{case⊎ } L' [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ]} \quad \text{（ξ-case⊎ 规则）}
$$

$$
\text{case⊎ } (\text{inj₁ } V) [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ] \rightarrow M [ x := V ] \quad \text{（β-inj₁ 规则）}
$$

$$
\text{case⊎ } (\text{inj₂ } W) [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ] \rightarrow N [ y := W ] \quad \text{（β-inj₂ 规则）}
$$

- 当 $$M$$ 归约为 $$M'$$ 时，$$\text{inj₁ } M$$ 归约为 $$\text{inj₁ } M'$$；同理，$$\text{inj₂ } N$$ 归约为 $$\text{inj₂ } N'$$。
- 当 $$L$$ 可以归约为 $$L'$$ 时，匹配表达式 $$\text{case⊎ } L [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ]$$ 可以归约为 $$\text{case⊎ } L' [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ]$$。
- 当 $$L$$ 是 $$\text{inj₁ } V$$ 时，匹配表达式 $$\text{case⊎ } L [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ]$$ 直接归约为 $$M [ x := V ]$$。
- 当 $$L$$ 是 $$\text{inj₂ } W$$ 时，匹配表达式 $$\text{case⊎ } L [\text{inj₁ } x ⇒ M \mid \text{inj₂ } y ⇒ N ]$$ 直接归约为 $$N [ y := W ]$$。

#### 例子

定义一个交换和类型分量的函数：

```haskell
swap⊎ : ∅ ⊢ A `⊎ B ⇒ B `⊎ A
swap⊎ = ƛ z ⇒ case⊎ z [inj₁ x ⇒ inj₂ x | inj₂ y ⇒ inj₁ y]
```

这个函数接收一个和类型的值并返回其分量交换后的结果。如果输入是 $$\text{inj₁ } x$$，输出为 $$\text{inj₂ } x$$；如果输入是 $$\text{inj₂ } y$$，输出为 $$\text{inj₁ } y$$。

---

以上解释涵盖了和类型的定义、赋型规则、归约规则，以及相关例子的详细内容。接下来可以继续讨论单元类型的扩展。



### 单元类型（Unit Type）

#### 语法

单元类型是一种非常简单的数据类型，它只有一个值。单元类型的语法如下：

$$
\begin{aligned}
A, B, C &::= \ldots \mid \top \quad \text{（类型）} \\
L, M, N &::= \ldots \mid \text{tt} \quad \text{（项）} \\
V, W &::= \ldots \mid \text{tt} \quad \text{（值）}
\end{aligned}
$$

- $$\top$$: 表示单元类型。
- $$\text{tt}$$: 表示单元类型的唯一值。

#### 赋型 (Typing)

单元类型的赋型规则非常简单，如下所示：

$$
\frac{}{\Gamma \vdash \text{tt} : \top} \quad \text{（tt 规则）}
$$

- 不论当前环境 $$\Gamma$$ 是什么，只要项是 $$\text{tt}$$，其类型就是 $$\top$$。

#### 归约 (Reduction)

单元类型没有归约规则，因为 $$\text{tt}$$ 已经是值，并且没有进一步简化的空间。

#### 例子

定义 $$A$$ 和 $$A `× \top$$ 之间的同构函数：

```haskell
to×⊤ : ∅ ⊢ A ⇒ A `× \top
to×⊤ = ƛ x ⇒ `⟨ x , \text{tt} ⟩
```

这个函数接收一个类型为 $$A$$ 的值，并返回一个有序对，其中包含该值和单元值 $$\text{tt}$$。

```haskell
from×⊤ : ∅ ⊢ A `× \top ⇒ A
from×⊤ = ƛ z ⇒ `proj₁ z
```

这个函数接收一个类型为 $$A `× \top$$ 的有序对，并返回该对的第一个分量（即类型为 $$A$$ 的值）。

### 单元类型的替代表达方法

为了进一步简化操作，我们可以使用匹配表达式来处理单元类型的值。这种替代表达方法允许我们在单元类型的情况下进行模式匹配，即使单元类型只有一个可能的值。

#### 语法

替代表达方法的语法如下：

$$
\begin{aligned}
A, B, C &::= \ldots \mid \top \quad \text{（类型）} \\
L, M, N &::= \ldots \mid \text{tt} \mid \text{case⊤ } L [\text{tt} ⇒ N] \quad \text{（项）} \\
V, W &::= \ldots \mid \text{tt} \quad \text{（值）}
\end{aligned}
$$

- $$\text{case⊤ } L [\text{tt} ⇒ N]$$: 匹配项 $$L$$ 是否为单元值 $$\text{tt}$$，如果是，则执行 $$N$$。

#### 赋型 (Typing)

单元类型的替代表达方法的赋型规则如下：

$$
\frac{\Gamma \vdash L : \top \quad \Gamma \vdash M : A}{\Gamma \vdash \text{case⊤ } L [\text{tt} ⇒ M] : A} \quad \text{（case⊤ 规则）}
$$

- 如果 $$L$$ 的类型是 $$\top$$ 且 $$M$$ 的类型是 $$A$$，那么 $$\text{case⊤ } L [\text{tt} ⇒ M]$$ 的类型就是 $$A$$。

#### 归约 (Reduction)

单元类型的替代表达方法的归约规则如下：

$$
\frac{L \rightarrow L'}{\text{case⊤ } L [\text{tt} ⇒ M] \rightarrow \text{case⊤ } L' [\text{tt} ⇒ M]} \quad \text{（ξ-case⊤ 规则）}
$$

$$
\text{case⊤ } \text{tt} [\text{tt} ⇒ M] \rightarrow M \quad \text{（β-case⊤ 规则）}
$$

- 当 $$L$$ 可以归约为 $$L'$$ 时，$$\text{case⊤ } L [\text{tt} ⇒ M]$$ 归约为 $$\text{case⊤ } L' [\text{tt} ⇒ M]$$。
- 当 $$L$$ 是 $$\text{tt}$$ 时，$$\text{case⊤ } \text{tt} [\text{tt} ⇒ M]$$ 归约为 $$M$$。

#### 翻译

替代表达方法可以被翻译为标准表达方法：

$$
(\text{case⊤ } L [\text{tt} ⇒ M]) † = \text{let } z = (L †) \text{ in } (M †)
$$

这里 $$z$$ 是一个在 $$M$$ 中没有作为自由变量出现的新鲜变量。此翻译将 $$L$$ 求值，并将其结果绑定到 $$z$$，然后在 $$M$$ 中使用 $$z$$ 进行计算。

#### 例子

用新记法重新定义 $$A$$ 和 $$A `× \top$$ 之间同构的一部分：

```haskell
from×⊤-case : ∅ ⊢ A `× \top ⇒ A
from×⊤-case = ƛ z ⇒ case× z [⟨ x , y ⟩ ⇒ case⊤ y [\text{tt} ⇒ x]]
```

这个函数接收一个有序对，并使用模式匹配来提取其中的第一个分量。它首先使用 $$\text{case×}$$ 匹配有序对 $$z$$，然后使用 $$\text{case⊤}$$ 匹配单元类型的值 $$y$$，最终返回 $$x$$。

---

以上解释涵盖了单元类型及其替代表达方法的定义、赋型规则、归约规则，以及相关例子和翻译的详细内容。接下来可以继续讨论空类型的扩展。



### 空类型（Empty Type）

#### 语法

空类型是类型系统中一个特殊的类型，它没有任何值。空类型的语法如下：

$$
\begin{aligned}
A, B, C &::= \ldots \mid \bot \quad \text{（类型）} \\
L, M, N &::= \ldots \mid \text{case⊥ } L [] \quad \text{（项）}
\end{aligned}
$$

- $$\bot$$: 表示空类型。
- $$\text{case⊥ } L []$$: 用于处理类型为 $$\bot$$ 的项，这种情况下，由于空类型没有值，所以匹配到该类型的项是不可达的。

#### 赋型 (Typing)

空类型的赋型规则如下：

$$
\frac{\Gamma \vdash L : \bot}{\Gamma \vdash \text{case⊥ } L [] : A} \quad \text{（case⊥ 规则）}
$$

- 如果 $$L$$ 的类型是 $$\bot$$，则 $$\text{case⊥ } L []$$ 可以赋给任意类型 $$A$$。

由于空类型没有值，因此这个规则允许我们处理任何不可能存在的值，从而在逻辑上推导出任何结论。

#### 归约 (Reduction)

空类型的归约规则如下：

$$
\frac{L \rightarrow L'}{\text{case⊥ } L [] \rightarrow \text{case⊥ } L' []} \quad \text{（ξ-case⊥ 规则）}
$$

- 当 $$L$$ 可以归约为 $$L'$$ 时，$$\text{case⊥ } L []$$ 归约为 $$\text{case⊥ } L' []$$。

注意，这个归约规则在实际使用中通常不会触发，因为 $$L$$ 是类型为 $$\bot$$ 的项，而 $$\bot$$ 没有值，因此 $$L$$ 不会有任何归约。

#### 例子

定义 $$A$$ 和 $$A \oplus \bot$$ 之间的同构函数：

```haskell
to⊎⊥ : ∅ ⊢ A ⇒ A `⊎ \bot
to⊎⊥ = ƛ x ⇒ `inj₁ x
```

这个函数接收一个类型为 $$A$$ 的值，并将其注入到 $$A \oplus \bot$$ 中的左侧分量。

```haskell
from⊎⊥ : ∅ ⊢ A `⊎ \bot ⇒ A
from⊎⊥ = ƛ z ⇒ case⊎ z [inj₁ x ⇒ x | inj₂ y ⇒ case⊥ y []]
```

这个函数接收一个类型为 $$A \oplus \bot$$ 的项，如果它是注入的左侧分量 $$A$$，则返回该值；如果是注入的右侧分量 $$\bot$$，由于 $$\bot$$ 没有值，因此在这种情况下通过 $$\text{case⊥ }$$ 构造处理。

---

这部分内容解释了空类型的语法、赋型规则、归约规则，以及如何通过具体的例子来应用空类型。空类型的关键特性在于它没有值，因此在任何匹配它的情况下都可以推导出任意类型的值。接下来，可以继续讨论列表类型的扩展。

### 列表类型（List Type）

#### 语法

列表类型用于表示元素的有序集合。其语法如下：

$$
\begin{aligned}
A, B, C &::= \ldots \mid \text{List } A \quad \text{（类型）} \\
L, M, N &::= \ldots \mid [] \mid M \,\text{∷}\, N \mid \text{caseL } L [ [] \Rightarrow M \mid x \,\text{∷}\, xs \Rightarrow N ] \quad \text{（项）}
\end{aligned}
$$

- $\text{List } A$: 表示元素类型为 $A$ 的列表类型。
- $[]$: 表示空列表。
- $M \,\text{∷}\, N$: 表示通过向列表 $N$ 的前端添加元素 $M$ 所构造的非空列表。
- $\text{caseL } L [ [] \Rightarrow M \mid x \,\text{∷}\, xs \Rightarrow N ]$: 用于匹配列表 $L$，如果 $L$ 是空列表，则返回 $M$；如果 $L$ 是非空列表，则将其分解为头元素 $x$ 和尾部列表 $xs$，然后对 $N$ 进行求值。

#### 赋型 (Typing)

列表类型的赋型规则如下：

- 空列表的赋型：

$$
\frac{}{\Gamma \vdash [] : \text{List } A} \quad \text{（List-I₁ 规则）}
$$

- 构造列表的赋型：

$$
\frac{\Gamma \vdash M : A \quad \Gamma \vdash N : \text{List } A}{\Gamma \vdash M \,\text{∷}\, N : \text{List } A} \quad \text{（List-I₂ 规则）}
$$

- 列表匹配的赋型：

$$
\frac{\Gamma \vdash L : \text{List } A \quad \Gamma \vdash M : B \quad \Gamma, x : A, xs : \text{List } A \vdash N : B}{\Gamma \vdash \text{caseL } L [ [] \Rightarrow M \mid x \,\text{∷}\, xs \Rightarrow N ] : B} \quad \text{（List-E 规则）}
$$

#### 归约 (Reduction)

列表类型的归约规则如下：

- 列表构造项的归约：

$$
\frac{M \rightarrow M'}{M \,\text{∷}\, N \rightarrow M' \,\text{∷}\, N} \quad \text{（ξ-∷₁ 规则）}
$$

$$
\frac{N \rightarrow N'}{V \,\text{∷}\, N \rightarrow V \,\text{∷}\, N'} \quad \text{（ξ-∷₂ 规则）}
$$

- 列表匹配项的归约：

$$
\frac{L \rightarrow L'}{\text{caseL } L [ [] \Rightarrow M \mid x \,\text{∷}\, xs \Rightarrow N ] \rightarrow \text{caseL } L' [ [] \Rightarrow M \mid x \,\text{∷}\, xs \Rightarrow N ]} \quad \text{（ξ-caseL 规则）}
$$

- 匹配空列表的归约：

$$
\text{caseL } [] [ [] \Rightarrow M \mid x \,\text{∷}\, xs \Rightarrow N ] \rightarrow M \quad \text{（β-[] 规则）}
$$

- 匹配非空列表的归约：

$$
\text{caseL } (V \,\text{∷}\, W) [ [] \Rightarrow M \mid x \,\text{∷}\, xs \Rightarrow N ] \rightarrow N [ x := V ][ xs := W ] \quad \text{（β-∷ 规则）}
$$

#### 例子

下面是一个列表的映射函数的定义：

```haskell
mapL : ∅ ⊢ (A ⇒ B) ⇒ \text{List } A ⇒ \text{List } B
mapL = μ mL ⇒ ƛ f ⇒ ƛ xs ⇒
         \text{caseL } xs
           [ [] \Rightarrow []
           | x \,\text{∷}\, xs \Rightarrow f \cdot x \,\text{∷}\, mL \cdot f \cdot xs ]
```

这个函数接收一个函数 $f$ 和一个列表 $xs$，将 $f$ 应用于 $xs$ 中的每个元素，并返回新的列表。

---

这部分内容解释了列表类型的语法、赋型规则、归约规则，以及如何通过具体的例子来应用列表类型。列表是编程语言中常用的数据结构，掌握其规则对于理解复杂的数据类型至关重要。



### 形式化 (Formalization)

本节将展示如何形式化地定义和操作我们之前讨论的各种构造，包括原语数字、let绑定、积、积的替代表示方法等。这部分内容将涉及类型、语境、变量的查询、赋型判断、值和归约规则等。

#### 导入 (Imports)

在形式化之前，需要导入一些基本的依赖模块。以下是需要导入的模块及其使用情况：

- **Relation.Binary.PropositionalEquality**: 提供命题等式。
- **Data.Empty**: 提供空类型 $⊥$ 和对应的消去规则 $⊥\text{-elim}$。
- **Data.Nat**: 提供自然数、零和后继操作，以及乘法和不等式等标准运算。
- **Relation.Nullary**: 提供布尔逻辑中的否定符号 $¬$。
- **Relation.Nullary.Decidable**: 提供可判定性和证据转换的工具。

导入这些模块后，我们将能够定义类型和语境，并进一步定义赋型判断和归约规则。

```haskell
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.Nat using (ℕ; zero; suc; _*_; _<_; _≤?_; z≤n; s≤s)
open import Relation.Nullary using (¬_)
open import Relation.Nullary.Decidable using (True; toWitness)
```

#### 语法 (Syntax)

在形式化的定义中，首先需要定义类型（Type）、语境（Context）、赋型判断（Typing Judgment）等基本语法规则。

- **类型**：包括自然数类型、函数类型、积类型等。

```haskell
data Type : Set where
  `ℕ    : Type
  _⇒_   : Type → Type → Type
  Nat   : Type
  _`×_  : Type → Type → Type
```

- **语境**：语境用于表示变量的环境，即变量及其类型的集合。

```haskell
data Context : Set where
  ∅   : Context
  _,_ : Context → Type → Context
```

- **变量的查询判断**：用于表示变量在语境中的位置。

```haskell
data _∋_ : Context → Type → Set where
  Z : ∀ {Γ A}
      ---------
    → Γ , A ∋ A

  S_ : ∀ {Γ A B}
    → Γ ∋ B
      ---------
    → Γ , A ∋ B
```

在这个定义中，$Z$ 表示变量在语境中的第一个位置，而 $S\_$ 表示变量在语境中的后续位置。

- **项及赋型判断**：项包括变量、函数、自然数、乘法、let绑定、积等；赋型判断表示项是否符合指定类型。

```haskell
data _⊢_ : Context → Type → Set where
  `_ : ∀ {Γ A}
    → Γ ∋ A
      -----
    → Γ ⊢ A

  ƛ_  :  ∀ {Γ A B}
    → Γ , A ⊢ B
      ---------
    → Γ ⊢ A ⇒ B

  _·_ : ∀ {Γ A B}
    → Γ ⊢ A ⇒ B
    → Γ ⊢ A
      ---------
    → Γ ⊢ B

  -- 原语数字

  con : ∀ {Γ}
    → ℕ
      -------
    → Γ ⊢ Nat

  _`*_ : ∀ {Γ}
    → Γ ⊢ Nat
    → Γ ⊢ Nat
      -------
    → Γ ⊢ Nat

  -- let

  `let : ∀ {Γ A B}
    → Γ ⊢ A
    → Γ , A ⊢ B
      ----------
    → Γ ⊢ B

  -- 积

  `⟨_,_⟩ : ∀ {Γ A B}
    → Γ ⊢ A
    → Γ ⊢ B
      -----------
    → Γ ⊢ A `× B

  `proj₁ : ∀ {Γ A B}
    → Γ ⊢ A `× B
      -----------
    → Γ ⊢ A

  `proj₂ : ∀ {Γ A B}
    → Γ ⊢ A `× B
      -----------
    → Γ ⊢ B

  -- 积的替代表示方法

  case× : ∀ {Γ A B C}
    → Γ ⊢ A `× B
    → Γ , A , B ⊢ C
      --------------
    → Γ ⊢ C
```

这些定义为我们接下来要进行的各种操作奠定了基础。我们可以在此基础上进行重命名、代换、归约等操作，并保证这些操作的正确性。

---

这部分内容解释了如何形式化地定义简单类型的λ-演算，涵盖了类型、语境、赋型判断、变量查询等基本概念。这些形式化定义是进一步进行推导、证明和实现的基础，在学习形式化方法时非常重要。



#### 缩减 de Bruijn 因子 (Reducing de Bruijn Index)

在形式化语法中，我们经常使用 de Bruijn 索引来表示变量位置。接下来，我们定义缩减 de Bruijn 因子的相关函数。

1. **length**：计算语境的长度。

   语境的长度是语境中变量的个数。对于空语境 ∅，长度为零；对于非空语境 $(\Gamma , A)$，长度为 $1 + \text{length}(\Gamma)$。

   ```haskell
   length : Context → ℕ
   length ∅        =  zero
   length (Γ , _)  =  suc (length Γ)
   ```

2. **lookup**：根据 de Bruijn 索引查找语境中的类型。

   给定语境 $\Gamma$ 和索引 $n$，lookup 函数返回位于该位置的类型。查找过程是递归的，索引为零时返回当前语境中的类型，索引为非零时递归查找上一层语境。

   ```haskell
   lookup : {Γ : Context} → {n : ℕ} → (p : n < length Γ) → Type
   lookup {(_ , A)} {zero}    (s≤s z≤n)  =  A
   lookup {(Γ , _)} {(suc n)} (s≤s p)    =  lookup p
   ```

3. **count**：根据 de Bruijn 索引查找对应的变量在语境中的位置。

   count 函数用于查找给定索引 $n$ 对应的变量在语境中的具体位置。它与 lookup 函数配合使用。

   ```haskell
   count : ∀ {Γ} → {n : ℕ} → (p : n < length Γ) → Γ ∋ lookup p
   count {_ , _} {zero}    (s≤s z≤n)  =  Z
   count {Γ , _} {(suc n)} (s≤s p)    =  S (count p)
   ```

4. **\#_**：根据 de Bruijn 索引生成项。

   这个函数结合 lookup 和 count，用于生成 de Bruijn 索引对应的项。

   ```haskell
   #_ : ∀ {Γ}
     → (n : ℕ)
     → {n∈Γ : True (suc n ≤? length Γ)}
       --------------------------------
     → Γ ⊢ lookup (toWitness n∈Γ)
   #_ n {n∈Γ}  =  ` count (toWitness n∈Γ)
   ```

#### 重命名 (Renaming)

重命名是将一项中的所有变量替换为新的变量，而不改变它的结构。以下是定义重命名的函数：

1. **ext**：扩展重命名函数以适应语境的变化。

   ext 函数将原有的重命名函数扩展，以便能够处理添加到语境中的新变量。

   ```haskell
   ext : ∀ {Γ Δ}
     → (∀ {A}   →     Γ ∋ A →     Δ ∋ A)
       ---------------------------------
     → (∀ {A B} → Γ , A ∋ B → Δ , A ∋ B)
   ext ρ Z      =  Z
   ext ρ (S x)  =  S (ρ x)
   ```

2. **rename**：对项进行重命名。

   rename 函数递归地对项中的每个子项进行重命名。

   ```haskell
   rename : ∀ {Γ Δ}
     → (∀ {A} → Γ ∋ A → Δ ∋ A)
       -----------------------
     → (∀ {A} → Γ ⊢ A → Δ ⊢ A)
   rename ρ (` x)          =  ` (ρ x)
   rename ρ (ƛ N)          =  ƛ (rename (ext ρ) N)
   rename ρ (L · M)        =  (rename ρ L) · (rename ρ M)
   rename ρ (`zero)        =  `zero
   rename ρ (`suc M)       =  `suc (rename ρ M)
   rename ρ (case L M N)   =  case (rename ρ L) (rename ρ M) (rename (ext ρ) N)
   rename ρ (μ N)          =  μ (rename (ext ρ) N)
   rename ρ (con n)        =  con n
   rename ρ (M `* N)       =  rename ρ M `* rename ρ N
   rename ρ (`let M N)     =  `let (rename ρ M) (rename (ext ρ) N)
   rename ρ `⟨ M , N ⟩     =  `⟨ rename ρ M , rename ρ N ⟩
   rename ρ (`proj₁ L)     =  `proj₁ (rename ρ L)
   rename ρ (`proj₂ L)     =  `proj₂ (rename ρ L)
   rename ρ (case× L M)    =  case× (rename ρ L) (rename (ext (ext ρ)) M)
   ```

#### 同时代换 (Simultaneous Substitution)

同时代换是指在一项中同时替换多个变量。以下是同时代换的定义：

1. **exts**：扩展代换函数以适应语境的变化。

   这个函数类似于重命名中的 ext，用于扩展代换函数。

   ```haskell
   exts : ∀ {Γ Δ} → (∀ {A} → Γ ∋ A → Δ ⊢ A) → (∀ {A B} → Γ , A ∋ B → Δ , A ⊢ B)
   exts σ Z      =  ` Z
   exts σ (S x)  =  rename S_ (σ x)
   ```

2. **subst**：对项进行代换。

   subst 函数递归地对项中的每个子项进行代换。

   ```haskell
   subst : ∀ {Γ Δ} → (∀ {C} → Γ ∋ C → Δ ⊢ C) → (∀ {C} → Γ ⊢ C → Δ ⊢ C)
   subst σ (` k)          =  σ k
   subst σ (ƛ N)          =  ƛ (subst (exts σ) N)
   subst σ (L · M)        =  (subst σ L) · (subst σ M)
   subst σ (`zero)        =  `zero
   subst σ (`suc M)       =  `suc (subst σ M)
   subst σ (case L M N)   =  case (subst σ L) (subst σ M) (subst (exts σ) N)
   subst σ (μ N)          =  μ (subst (exts σ) N)
   subst σ (con n)        =  con n
   subst σ (M `* N)       =  subst σ M `* subst σ N
   subst σ (`let M N)     =  `let (subst σ M) (subst (exts σ) N)
   subst σ `⟨ M , N ⟩     =  `⟨ subst σ M , subst σ N ⟩
   subst σ (`proj₁ L)     =  `proj₁ (subst σ L)
   subst σ (`proj₂ L)     =  `proj₂ (subst σ L)
   subst σ (case× L M)    =  case× (subst σ L) (subst (exts (exts σ)) M)
   ```

3. **单个和双重代换**：定义单个变量代换和双变量代换的操作。

   ```haskell
   _[_] : ∀ {Γ A B}
     → Γ , B ⊢ A
     → Γ ⊢ B
       ---------
     → Γ ⊢ A
   _[_] {Γ} {A} {B} N M =  subst {Γ , B} {Γ} σ {A} N
     where
     σ : ∀ {A} → Γ , B ∋ A → Γ ⊢ A
     σ Z      =  M
     σ (S x)  =  ` x
   
   _[_][_] : ∀ {Γ A B C}
     → Γ , A , B ⊢ C
     → Γ ⊢ A
     → Γ ⊢ B
       -------------
     → Γ ⊢ C
   _[_][_] {Γ} {A} {B} N V W =  subst {Γ , A , B} {Γ} σ N
     where
     σ : ∀ {C} → Γ , A , B ∋ C → Γ ⊢ C
     σ Z          =  W
     σ (S Z)      =  V
     σ (S (S x))  =  ` x
   ```

这些函数为接下来的形式化推导和

证明提供了必要的工具。通过重命名和代换，我们可以处理更加复杂的形式化构造，并且确保项在不同环境下的正确性。

---

这部分内容详细解释了如何进行缩减 de Bruijn 因子、重命名和同时代换的操作。这些操作是形式化推导和证明的基础，尤其是在处理复杂的代数结构时非常重要。通过理解这些操作，我们可以更深入地理解 λ-演算的形式化构造及其操作逻辑。



### 值 (Values)

在形式化系统中，我们定义了一些特定的项作为值（Values）。值是那些不再能被进一步归约的项。

#### 定义值的数据类型

1. **V-ƛ (V-λ)**: 表示一个 λ 抽象是一个值。

   $$\text{V-λ} : ∀\{Γ A B\} \{N : Γ , A ⊢ B\} \rightarrow \text{Value} (λ N)$$

   这里的 $λ N$ 表示一个 λ 抽象，它是由环境 $Γ$ 和类型 $A$ 组成的，并且 $N$ 是一个类型为 $B$ 的项。

2. **V-zero**: 表示自然数零是一个值。

   $$\text{V-zero} : ∀\{Γ\} \rightarrow \text{Value} (\text{zero})$$

   这里的 $\text{zero}$ 是自然数零，它在任何环境 $Γ$ 下都是一个值。

3. **V-suc**: 表示自然数的后继数也是一个值。

   $$\text{V-suc} : ∀\{Γ\} \{V : Γ ⊢ \text{ℕ}\} \rightarrow \text{Value} (V) \rightarrow \text{Value} (\text{suc} V)$$

   这里的 $\text{suc} V$ 表示自然数 $V$ 的后继数，它也是一个值。

4. **V-con**: 表示常量是一个值。

   $$\text{V-con} : ∀\{Γ n\} \rightarrow \text{Value} (\text{con} n)$$

   这里的 $\text{con} n$ 是一个常量，它在任何环境 $Γ$ 下都是一个值。

5. **V-⟨_,_⟩**: 表示一个有序对是一个值。

   $$\text{V-⟨_,_⟩} : ∀\{Γ A B\} \{V : Γ ⊢ A\} \{W : Γ ⊢ B\} \rightarrow \text{Value} (V) \rightarrow \text{Value} (W) \rightarrow \text{Value} (\text{⟨ V , W ⟩})$$

   这里的 $\text{⟨ V , W ⟩}$ 表示一个有序对，如果 $V$ 和 $W$ 都是值，那么这个有序对也是一个值。

#### 值的不可归约性

我们定义一个辅助函数 $V¬—→$ 来证明一个值是不可归约的，即该值不能被进一步归约。

```haskell
V¬—→ : ∀ {Γ A} {M N : Γ ⊢ A}
  → Value M
    ----------
  → ¬ (M —→ N)
V¬—→ V-λ          ()
V¬—→ V-zero       ()
V¬—→ (V-suc VM)   (ξ-suc M—→M′)     =  V¬—→ VM M—→M′
V¬—→ V-con        ()
V¬—→ V-⟨ VM , _ ⟩ (ξ-⟨,⟩₁ M—→M′)    =  V¬—→ VM M—→M′
V¬—→ V-⟨ _ , VN ⟩ (ξ-⟨,⟩₂ _ N—→N′)  =  V¬—→ VN N—→N′
```

在上面的定义中，我们通过递归地检查值的各个部分来判断它是否可以被归约。如果一个值可以被归约，那么它就不符合值的定义。通过这种方式，我们确保了值的不可归约性。

### 归约 (Reduction)

归约是 λ-演算中将复杂表达式简化为更基本形式的过程。在形式化系统中，归约规则定义了如何将项简化为更基本的形式。

#### 归约规则的定义

1. **函数应用的归约**

   - ξ-·₁: 如果函数部分可以归约，则整个应用表达式可以归约。

     $$\text{ξ-·₁} : ∀\{Γ A B\} \{L L' : Γ ⊢ A ⇒ B\} \{M : Γ ⊢ A\} \rightarrow L —→ L' \rightarrow L · M —→ L' · M$$

   - ξ-·₂: 如果参数部分可以归约，并且函数部分是一个值，则整个应用表达式可以归约。

     $$\text{ξ-·₂} : ∀\{Γ A B\} \{V : Γ ⊢ A ⇒ B\} \{M M' : Γ ⊢ A\} \rightarrow \text{Value} (V) \rightarrow M —→ M' \rightarrow V · M —→ V · M'$$

   - β-λ: 如果参数是一个值，则可以进行 β-归约。

     $$\text{β-λ} : ∀\{Γ A B\} \{N : Γ , A ⊢ B\} \{V : Γ ⊢ A\} \rightarrow \text{Value} (V) \rightarrow (λ N) · V —→ N [ V ]$$

2. **自然数的归约**

   - ξ-suc: 如果一个自然数可以归约，则其后继也可以归约。

     $$\text{ξ-suc} : ∀\{Γ\} \{M M' : Γ ⊢ ℕ\} \rightarrow M —→ M' \rightarrow \text{suc} M —→ \text{suc} M'$$

   - ξ-case: 如果 case 表达式中的条件部分可以归约，则整个 case 表达式可以归约。

     $$\text{ξ-case} : ∀\{Γ A\} \{L L' : Γ ⊢ ℕ\} \{M : Γ ⊢ A\} \{N : Γ , ℕ ⊢ A\} \rightarrow L —→ L' \rightarrow \text{case} L M N —→ \text{case} L' M N$$

   - β-zero: 如果条件是自然数零，则 case 表达式可以直接归约为零分支。

     $$\text{β-zero} : ∀\{Γ A\} \{M : Γ ⊢ A\} \{N : Γ , ℕ ⊢ A\} \rightarrow \text{case zero M N} —→ M$$

   - β-suc: 如果条件是自然数的后继，则 case 表达式可以归约为后继分支。

     $$\text{β-suc} : ∀\{Γ A\} \{V : Γ ⊢ ℕ\} \{M : Γ ⊢ A\} \{N : Γ , ℕ ⊢ A\} \rightarrow \text{Value} (V) \rightarrow \text{case (suc V) M N} —→ N [ V ]$$

3. **不动点的归约**

   - β-μ: 对不动点表达式进行归约。

     $$\text{β-μ} : ∀\{Γ A\} \{N : Γ , A ⊢ A\} \rightarrow μ N —→ N [ μ N ]$$

4. **原语数字的归约**

   - ξ-*₁: 如果乘法表达式的左侧可以归约，则整个乘法表达式可以归约。

     $$\text{ξ-*₁} : ∀\{Γ\} \{L L' M : Γ ⊢ Nat\} \rightarrow L —→ L' \rightarrow L `* M —→ L' `* M$$

   - ξ-*₂: 如果乘法表达式的右侧可以归约，并且左侧是一个值，则整个乘法表达式可以归约。

     $$\text{ξ-*₂} : ∀\{Γ\} \{V M M' : Γ ⊢ Nat\} \rightarrow \text{Value} (V) \rightarrow M —→ M' \rightarrow V `* M —→ V `* M'$$

   - δ-*: 两个常量的乘法可以直接进行数值计算。

     $$\text{δ-*} : ∀\{Γ c d\} \rightarrow \text{con} c `* \text{con} d —→ \text{con} (c * d)$$

这些规则描述了如何在 λ-演算中将复杂的表达式逐步简化为更基本的形式。这些归约规则是 λ-演算的核心，定义了程序计算的行为。

---

这部分内容详细解释了值的定义及其不可归约性，以及归约规则的具体实现。通过这些规则，我们可以理解 λ-演算中项如何被简化，以及程序如何从一个复杂的表达式逐步计算到最终结果。

### 自反传递闭包 (Reflexive Transitive Closure)

在 λ-演算中，定义自反传递闭包是为了表示多个连续的归约步骤。自反传递闭包允许我们从一个项逐步归约到另一个项，并且这个过程可以通过多个中间步骤完成。

#### 自反传递闭包的数据类型定义

1. **_—↠_**: 这是归约关系的自反传递闭包。我们定义了一个数据类型来表示从一个项归约到另一个项的过程，其中可能包含多个步骤。

   ```haskell
   data _—↠_ {Γ A} : (Γ ⊢ A) → (Γ ⊢ A) → Set where
   ```

   这个定义表示从环境 $Γ$ 下的类型为 $A$ 的项归约到另一个类型为 $A$ 的项。

2. **_∎**: 表示自反关系，即一个项归约到自身，不需要任何归约步骤。

   $$\_∎ : (M : Γ ⊢ A) \rightarrow M —↠ M$$

   这里的 $M$ 表示项，$M —↠ M$ 表示 $M$ 自己归约到自己，这表示没有任何变化。

3. **step—→**: 表示传递关系，即如果我们有一个项 $L$ 归约到 $M$，$M$ 又归约到 $N$，那么 $L$ 就可以直接归约到 $N$。

   ```haskell
   step—→ : (L : Γ ⊢ A) {M N : Γ ⊢ A} → M —↠ N → L —→ M → L —↠ N
   ```

   这里，$L$ 首先通过某个规则归约到 $M$，然后 $M$ 通过多步归约到 $N$，所以 $L$ 可以直接归约到 $N$。

#### 归约路径 (Reduction Path)

为了简化归约路径的表示，我们定义了一个模式：

```haskell
pattern _—→⟨_⟩_ L L—→M M—↠N = step—→ L M—↠N L—→M
```

这个模式表示从 $L$ 通过 $L—→M$ 归约到 $M$，然后通过多步归约 $M—↠N$ 到 $N$。

#### 开始归约 (Begin Reduction)

为了方便表示归约过程的开始，我们定义了一个辅助函数 $begin\_$，它用于表示归约路径的开始：

```haskell
begin_ : ∀ {Γ A} {M N : Γ ⊢ A} → M —↠ N → M —↠ N
begin M—↠N = M—↠N
```

这个函数简单地返回传入的归约路径，是一个辅助函数，用于提高代码的可读性。

### 值不再归约 (Values Don't Reduce Further)

为了证明一个值不再能被进一步归约，我们定义了一个辅助函数 $V¬—→$。这个函数接受一个值，并且证明该值不能被进一步归约：

```haskell
V¬—→ : ∀ {Γ A} {M N : Γ ⊢ A} → Value M → ¬ (M —→ N)
```

具体地，这个函数通过递归地检查值的各个部分来验证它们是否能够进一步归约。如果一个值尝试进行归约（即 $M —→ N$），那么这个证明将失败。

### 可进性 (Progress)

在 λ-演算中，**可进性** 是一条非常重要的性质，它表明任何给定的项要么是一个值，要么可以被进一步归约。为此，我们定义了一个数据类型 Progress 来表示项的这一性质。

#### 定义 Progress 数据类型

1. **step**: 表示一个项可以被进一步归约。

   ```haskell
   step : ∀ {N : ∅ ⊢ A} → M —→ N → Progress M
   ```

   这里的 $M$ 表示项，$N$ 是 $M$ 归约后的结果。如果 $M$ 可以归约到 $N$，那么我们就说 $M$ 具有 Progress。

2. **done**: 表示一个项已经是一个值，无法被进一步归约。

   ```haskell
   done : Value M → Progress M
   ```

   如果 $M$ 是一个值，则我们说 $M$ 具有 Progress，因为它已经达到了不可归约的状态。

#### Progress 的证明

我们定义了一个函数 $progress$，它接受一个项并返回其 Progress 的证明：

```haskell
progress : ∀ {A} → (M : ∅ ⊢ A) → Progress M
```

这个函数通过递归检查项的结构，并根据项的类型选择适当的归约规则，证明项的 Progress。

### 求值 (Evaluation)

在 λ-演算中，求值是将一个项逐步简化为一个值的过程。我们通过定义 $eval$ 函数来实现这一过程。这个函数模拟了计算机执行程序的过程，其中每一步都减少了可用的 "gas"（计算资源）。

#### 定义 $eval$ 函数

1. **Gas**: 我们首先定义了一个 Gas 数据类型，它表示求值过程中可用的资源量（类似于计算步骤的限制）。

   ```haskell
   record Gas : Set where
     constructor gas
     field amount : ℕ
   ```

2. **Finished**: 我们定义了一个 Finished 数据类型，用于表示求值的结果。可能的结果包括：

   - **done**: 表示求值结束并得到了一个值。
   - **out-of-gas**: 表示在求值过程中用尽了可用的计算资源。

   ```haskell
   data Finished {Γ A} (N : Γ ⊢ A) : Set where
     done : Value N → Finished N
     out-of-gas : Finished N
   ```

3. **Steps**: 我们定义了一个 Steps 数据类型，表示求值的多个步骤。

   ```haskell
   data Steps {A} : ∅ ⊢ A → Set where
     steps : {L N : ∅ ⊢ A} → L —↠ N → Finished N → Steps L
   ```

4. **eval**: 最后，我们定义了 eval 函数，它接受一个 Gas 值和一个项，然后返回该项的求值结果。

   ```haskell
   eval : ∀ {A} → Gas → (L : ∅ ⊢ A) → Steps L
   ```

   这个函数通过递归地减少 Gas 的量并逐步归约项，直到达到值或者用尽 Gas 为止。

---

这部分详细解释了自反传递闭包的定义及其在 λ-演算中的作用，并介绍了如何通过 Progress 性质证明项的归约性。最后，描述了求值过程及其实现方式，这为理解 λ-演算的执行机制提供了重要的基础。







### -------------------------------------------------

### 简单类型 λ-演算的更多构造

在这个章节中，作者介绍了一些扩展和新的构造，使得 λ-演算更加丰富和多样化。主要涉及以下几个概念：原语数字、$$\texttt{let}$$绑定、积、和、单元类型、空类型、以及列表。每个构造都定义了语法、赋型规则、归约规则，并通过例子进行展示。

#### 原语数字（Primitive Numbers）

##### 概念1：$$\texttt{Nat}$$类型
- **$$\texttt{Nat}$$类型**：$$\texttt{Nat}$$表示自然数类型，它等同于 Agda 中内置的自然数类型 $$\mathbb{N}$$。这是基础的数据类型，用于表示自然数和执行基本算术操作。

  **公式**：$$\texttt{Nat} \equiv \mathbb{N}$$

##### 概念2：乘法操作
- **乘法操作**：定义了自然数乘法操作 $$\texttt{*}$$。通过引入常量项（$\texttt{con c}$，其中 $$c$$ 是自然数）和乘法运算符，定义了 $$\texttt{Nat}$$ 类型上的二元运算。

  **公式**：
  $$L \texttt{ `* } M$$

##### 赋型规则
- **$$\texttt{con}$$规则**：常量 $$c$$ 是自然数，$$\texttt{con c}$$ 具有类型 $$\texttt{Nat}$$。
  $$\frac{c : \mathbb{N}}{\Gamma \vdash \texttt{con c} : \texttt{Nat}}$$

- **乘法赋型规则**：两个项 $$L$$ 和 $$M$$ 都具有 $$\texttt{Nat}$$ 类型时，其乘积也具有 $$\texttt{Nat}$$ 类型。
  $$\frac{\Gamma \vdash L : \texttt{Nat} \quad \Gamma \vdash M : \texttt{Nat}}{\Gamma \vdash L \texttt{ `* } M : \texttt{Nat}}$$

##### 归约规则
- **ξ规则**：通过以下两个规则，递归地归约乘法项的各个部分，直至其为值。
  $$\frac{L \rightarrow L'}{L \texttt{ `* } M \rightarrow L' \texttt{ `* } M}$$
  $$\frac{M \rightarrow M'}{V \texttt{ `* } M \rightarrow V \texttt{ `* } M'}$$

- **$$\delta$$规则**：两个常量相乘时，归约为常量的乘积。
  $$\frac{}{\texttt{con c} \texttt{ `* } \texttt{con d} \rightarrow \texttt{con (c * d)}}$$

##### 例子
- **求立方函数**：通过 $$\lambda$$ 抽象和乘法运算，定义了一个求立方的函数。
  $$\texttt{cube} : \emptyset \vdash \texttt{Nat} \Rightarrow \texttt{Nat}$$
  $$\texttt{cube} = \lambda x \Rightarrow x \texttt{ `* } x \texttt{ `* } x$$

#### $$\texttt{let}$$绑定（Let Bindings）

##### 概念1：$$\texttt{let}$$表达式
- **$$\texttt{let}$$表达式**：$$\texttt{let}$$ 绑定用于将一个项的值绑定到一个变量上，然后在后续表达式中使用该变量。它只影响项的语法，而不引入新的类型或值。

  **公式**：
  $$\texttt{let } x = M \texttt{ in } N$$

##### 赋型规则
- **$$\texttt{let}$$赋型规则**：首先给项 $$M$$ 赋型 $$A$$，然后在包含 $$x$$ 的上下文中给 $$N$$ 赋型 $$B$$。
  $$\frac{\Gamma \vdash M : A \quad \Gamma, x : A \vdash N : B}{\Gamma \vdash \texttt{let } x = M \texttt{ in } N : B}$$

##### 归约规则
- **ξ规则**：首先对 $$M$$ 进行归约，递归地处理 $$\texttt{let}$$ 表达式。
  $$\frac{M \rightarrow M'}{\texttt{let } x = M \texttt{ in } N \rightarrow \texttt{let } x = M' \texttt{ in } N}$$

- **$$\beta$$规则**：当 $$M$$ 是一个值时，将 $$x$$ 的值替换到 $$N$$ 中。
  $$\frac{}{\texttt{let } x = V \texttt{ in } N \rightarrow N[x := V]}$$

##### 例子
- **求十次方函数**：通过嵌套的 $$\texttt{let}$$ 表达式，定义了一个求十次方的函数。
  $$\texttt{exp10} : \emptyset \vdash \texttt{Nat} \Rightarrow \texttt{Nat}$$
  $$\texttt{exp10} = \lambda x \Rightarrow \texttt{let } x2 = x \texttt{ `* } x \texttt{ in let } x4 = x2 \texttt{ `* } x2 \texttt{ in let } x5 = x4 \texttt{ `* } x \texttt{ in } x5 \texttt{ `* } x5$$

##### 翻译
- **$$\texttt{let}$$ 表达式的翻译**：每个 $$\texttt{let}$$ 项都可以翻译成一个抽象函数应用。
  $$\texttt{(let } x = M \texttt{ in } N)^\dagger = (\lambda x \Rightarrow (N^\dagger)) \cdot (M^\dagger)$$

---

这些概念和规则为我们提供了更加丰富和灵活的 λ-演算操作。通过定义原语数字和 $$\texttt{let}$$ 绑定，我们可以构建更加复杂和实用的函数。同时，利用赋型和归约规则，我们能够形式化地描述这些操作并验证其正确性。



### 积类型（Product Types）

#### 概念1：积类型的定义

- **积类型**：积类型 (Product Type) 表示一个由两个类型组合而成的复合类型。我们使用 $$A \times B$$ 来表示类型 $$A$$ 和 $$B$$ 的积类型，它表示同时具有类型 $$A$$ 和 $$B$$ 的值。

  **公式**：
  $$A, B, C ::= \ldots \quad \text{(类型)} \quad | \quad A \times B \quad \text{(积类型)}$$

#### 概念2：有序对和投影操作

- **有序对**：有序对 (Ordered Pair) 是一种复合项，由两个子项构成，分别属于类型 $$A$$ 和 $$B$$。用 $$\langle M, N \rangle$$ 表示有序对，其中 $$M$$ 是类型为 $$A$$ 的项，$$N$$ 是类型为 $$B$$ 的项。

  **公式**：
  $$L, M, N ::= \ldots \quad \text{(项)} \quad | \quad \langle M, N \rangle \quad \text{(有序对)}$$

- **投影操作**：投影操作用于从有序对中提取其分量。$$\text{proj}_1 L$$ 提取有序对的第一个分量，$$\text{proj}_2 L$$ 提取第二个分量。

  **公式**：
  $$\text{proj}_1 L$$ 和 $$\text{proj}_2 L$$

#### 赋型规则

- **有序对的赋型规则**：如果 $$M$$ 具有类型 $$A$$，$$N$$ 具有类型 $$B$$，那么有序对 $$\langle M, N \rangle$$ 具有类型 $$A \times B$$。

  **公式**：
  $$\frac{\Gamma \vdash M : A \quad \Gamma \vdash N : B}{\Gamma \vdash \langle M, N \rangle : A \times B}$$

- **投影操作的赋型规则**：如果 $$L$$ 具有类型 $$A \times B$$，那么 $$\text{proj}_1 L$$ 具有类型 $$A$$，$$\text{proj}_2 L$$ 具有类型 $$B$$。

  **公式**：
  $$\frac{\Gamma \vdash L : A \times B}{\Gamma \vdash \text{proj}_1 L : A}$$
  $$\frac{\Gamma \vdash L : A \times B}{\Gamma \vdash \text{proj}_2 L : B}$$

#### 归约规则

- **ξ规则**：通过以下规则，递归地归约有序对中的各个分量和投影操作的项。

  **公式**：
  $$\frac{M \rightarrow M'}{\langle M, N \rangle \rightarrow \langle M', N \rangle}$$
  $$\frac{N \rightarrow N'}{\langle V, N \rangle \rightarrow \langle V, N' \rangle}$$
  $$\frac{L \rightarrow L'}{\text{proj}_1 L \rightarrow \text{proj}_1 L'}$$
  $$\frac{L \rightarrow L'}{\text{proj}_2 L \rightarrow \text{proj}_2 L'}$$

- **$$\beta$$规则**：从有序对中投影出其分量。

  **公式**：
  $$\frac{}{\text{proj}_1 \langle V, W \rangle \rightarrow V}$$
  $$\frac{}{\text{proj}_2 \langle V, W \rangle \rightarrow W}$$

#### 例子

- **交换有序对中分量的函数**：定义了一个函数，用于交换有序对中的两个分量。

  **公式**：
  $$\text{swap} \times : \emptyset \vdash A \times B \Rightarrow B \times A$$
  $$\text{swap} \times = \lambda z \Rightarrow \langle \text{proj}_2 z, \text{proj}_1 z \rangle$$

#### 积的替代表示方法

#### 概念1：积的替代表示

- **积的替代表示方法**：我们可以使用一个匹配表达式来同时绑定两个变量，作为积的替代表示方法。这种方法比直接使用投影操作更为简洁。

  **公式**：
  $$\text{case}_\times L [\langle x, y \rangle \Rightarrow M]$$

#### 赋型规则

- **case× 的赋型规则**：如果 $$L$$ 具有类型 $$A \times B$$，并且 $$M$$ 在包含 $$x : A$$ 和 $$y : B$$ 的上下文中具有类型 $$C$$，那么 $$\text{case}_\times L [\langle x, y \rangle \Rightarrow M]$$ 具有类型 $$C$$。

  **公式**：
  $$\frac{\Gamma \vdash L : A \times B \quad \Gamma, x : A, y : B \vdash M : C}{\Gamma \vdash \text{case}_\times L [\langle x, y \rangle \Rightarrow M] : C}$$

#### 归约规则

- **ξ规则**：首先对 $$L$$ 进行归约，递归地处理 $$\text{case}_\times$$ 表达式。

  **公式**：
  $$\frac{L \rightarrow L'}{\text{case}_\times L [\langle x, y \rangle \Rightarrow M] \rightarrow \text{case}_\times L' [\langle x, y \rangle \Rightarrow M]}$$

- **$$\beta$$规则**：当 $$L$$ 是一个有序对时，将分量分别绑定到 $$x$$ 和 $$y$$，并在 $$M$$ 中替换。

  **公式**：
  $$\frac{}{\text{case}_\times \langle V, W \rangle [\langle x, y \rangle \Rightarrow M] \rightarrow M[x := V][y := W]}$$

#### 例子

- **使用新记法重写交换有序对中分量的函数**：通过 $$\text{case}_\times$$ 表达式重写之前的交换函数。

  **公式**：
  $$\text{swap} \times \text{-case} : \emptyset \vdash A \times B \Rightarrow B \times A$$
  $$\text{swap} \times \text{-case} = \lambda z \Rightarrow \text{case}_\times z [\langle x, y \rangle \Rightarrow \langle y, x \rangle]$$

#### 翻译

- **积的替代表示法的翻译**：可以将 $$\text{case}_\times$$ 翻译为使用投影的表达式。

  **公式**：
  $$\text{(case}_\times L [\langle x, y \rangle \Rightarrow M])^\dagger = \text{let } z = (L^\dagger) \text{ in let } x = \text{proj}_1 z \text{ in let } y = \text{proj}_2 z \text{ in } (M^\dagger)$$

- **逆向翻译**：反向将投影表达式翻译为 $$\text{case}_\times$$ 表达式。

  **公式**：
  $$\text{(proj}_1 L)^\ddag = \text{case}_\times (L^\ddag) [\langle x, y \rangle \Rightarrow x]$$
  $$\text{(proj}_2 L)^\ddag = \text{case}_\times (L^\ddag) [\langle x, y \rangle \Rightarrow y]$$

### 和类型（Sum Types）

#### 概念1：和类型的定义

- **和类型**：和类型 (Sum Type) 表示一个可以是两种类型之一的复合类型。我们使用 $$A \oplus B$$ 来表示类型 $$A$$ 和 $$B$$ 的和类型，它表示一个值要么具有类型 $$A$$，要么具有类型 $$B$$。

  **公式**：
  $$A, B, C ::= \ldots \quad \text{(类型)} \quad | \quad A \oplus B \quad \text{(和类型)}$$

#### 概念2：注入和匹配操作

- **注入操作**：注入操作 (Injection) 用于将一个值注入到和类型中。$$\text{inj}_1 M$$ 将类型 $$A$$ 的值 $$M$$ 注入到 $$A \oplus B$$ 中，$$\text{inj}_2 N$$ 将类型 $$B$$ 的值 $$N$$ 注入到 $$A \oplus B$$ 中。

  **公式**：
  $$\text{inj}_1 M$$ 和

 $$\text{inj}_2 N$$

- **匹配操作**：匹配操作 (Case Analysis) 用于根据和类型的值属于哪种类型来进行分支计算。$$\text{case}_\oplus L [\text{inj}_1 x \Rightarrow M \, | \, \text{inj}_2 y \Rightarrow N]$$。

  **公式**：
  $$\text{case}_\oplus L [\text{inj}_1 x \Rightarrow M \, | \, \text{inj}_2 y \Rightarrow N]$$

#### 赋型规则

- **注入操作的赋型规则**：如果 $$M$$ 具有类型 $$A$$，则 $$\text{inj}_1 M$$ 具有类型 $$A \oplus B$$；如果 $$N$$ 具有类型 $$B$$，则 $$\text{inj}_2 N$$ 具有类型 $$A \oplus B$$。

  **公式**：
  $$\frac{\Gamma \vdash M : A}{\Gamma \vdash \text{inj}_1 M : A \oplus B}$$
  $$\frac{\Gamma \vdash N : B}{\Gamma \vdash \text{inj}_2 N : A \oplus B}$$

- **匹配操作的赋型规则**：如果 $$L$$ 具有类型 $$A \oplus B$$，并且在上下文中 $$M$$ 和 $$N$$ 都具有类型 $$C$$，那么 $$\text{case}_\oplus L [\text{inj}_1 x \Rightarrow M \, | \, \text{inj}_2 y \Rightarrow N]$$ 具有类型 $$C$$。

  **公式**：
  $$\frac{\Gamma \vdash L : A \oplus B \quad \Gamma, x : A \vdash M : C \quad \Gamma, y : B \vdash N : C}{\Gamma \vdash \text{case}_\oplus L [\text{inj}_1 x \Rightarrow M \, | \, \text{inj}_2 y \Rightarrow N] : C}$$

#### 归约规则

- **ξ规则**：首先对 $$L$$ 进行归约，递归地处理 $$\text{case}_\oplus$$ 表达式。

  **公式**：
  $$\frac{L \rightarrow L'}{\text{case}_\oplus L [\text{inj}_1 x \Rightarrow M \, | \, \text{inj}_2 y \Rightarrow N] \rightarrow \text{case}_\oplus L' [\text{inj}_1 x \Rightarrow M \, | \, \text{inj}_2 y \Rightarrow N]}$$

- **$$\beta$$规则**：根据 $$L$$ 的值是 $$\text{inj}_1$$ 还是 $$\text{inj}_2$$，选择相应的分支并进行替换。

  **公式**：
  $$\frac{}{\text{case}_\oplus (\text{inj}_1 V) [\text{inj}_1 x \Rightarrow M \, | \, \text{inj}_2 y \Rightarrow N] \rightarrow M[x := V]}$$
  $$\frac{}{\text{case}_\oplus (\text{inj}_2 W) [\text{inj}_1 x \Rightarrow M \, | \, \text{inj}_2 y \Rightarrow N] \rightarrow N[y := W]}$$

#### 例子

- **交换和类型中两个分量的函数**：定义了一个函数，用于交换和类型中的两个分量。

  **公式**：
  $$\text{swap}_\oplus : \emptyset \vdash A \oplus B \Rightarrow B \oplus A$$
  $$\text{swap}_\oplus = \lambda z \Rightarrow \text{case}_\oplus z [\text{inj}_1 x \Rightarrow \text{inj}_2 x \, | \, \text{inj}_2 y \Rightarrow \text{inj}_1 y]$$

---

这部分内容进一步扩展了 λ-演算，通过引入积类型和和类型，使得表达式可以组合不同类型的值，并在计算中进行模式匹配和处理。这些构造不仅丰富了表达能力，也为实现更加复杂的函数奠定了基础。

### ----------------

### Let 绑定

**概念与解释：**

1. **语法 (Syntax)**:
    - 在这里，`let` 绑定被引入作为一种简化表达的方式，用于在项内部绑定变量。其形式为 `let x = M in N`，表示首先计算项 $M$，然后在上下文中将结果绑定到变量 $x$，并在项 $N$ 中使用。

2. **赋型 (Typing)**:
    - 如果上下文 $\Gamma$ 中 $M$ 的类型为 $A$，并且在扩展上下文 $\Gamma, x : A$ 中 $N$ 的类型为 $B$，那么我们可以推导出 $\Gamma ⊢ let x = M in N : B$。
    - $$\frac{\Gamma ⊢ M : A \quad \Gamma, x : A ⊢ N : B}{\Gamma ⊢ let x = M in N : B}$$

3. **归约 (Reduction)**:
    - 在归约规则中，首先定义了 `let` 绑定的兼容性规则，用以处理项 $M$ 的归约。当 $M$ 归约到 $M'$ 时，$let x = M in N$ 归约为 $let x = M' in N$。
    - $$\frac{M \rightarrow M'}{let x = M in N \rightarrow let x = M' in N}$$
    - 当 $M$ 是一个值时，将该值替换到 $N$ 中，即 $let x = V in N \rightarrow N[x := V]$。

4. **例子 (Examples)**:
    - 例如，定义一个计算 $10$ 次幂的函数 `exp10`，可以通过连续的乘法和 `let` 绑定来实现。每次 `let` 绑定都会先计算前一个表达式的结果，并将其绑定到一个新的变量，以便后续使用。

5. **翻译 (Translation)**:
    - `let` 绑定可以通过 $\lambda$ 抽象来进行翻译。`(let x = M in N)^{\dagger}$ 可以表示为 `((\lambda x. N^\dagger) M^\dagger)`。

### 积 (Product)

**概念与解释：**

1. **语法 (Syntax)**:
    - 积类型的形式表示为 $A \times B$，它表示一个包含两个类型 $A$ 和 $B$ 的有序对。

2. **赋型 (Typing)**:
    - 如果上下文 $\Gamma$ 中 $M$ 的类型为 $A$，$N$ 的类型为 $B$，则 $\Gamma ⊢ \langle M, N \rangle : A \times B$。
    - $$\frac{\Gamma ⊢ M : A \quad \Gamma ⊢ N : B}{\Gamma ⊢ \langle M, N \rangle : A \times B}$$
    - 投影操作 $\text{proj}_1$ 和 $\text{proj}_2$ 分别从有序对中提取第一个和第二个分量：
    - $$\frac{\Gamma ⊢ L : A \times B}{\Gamma ⊢ \text{proj}_1 L : A} \quad \text{和} \quad \frac{\Gamma ⊢ L : A \times B}{\Gamma ⊢ \text{proj}_2 L : B}$$

3. **归约 (Reduction)**:
    - 积类型的归约规则涉及有序对和投影的归约。例如，当有序对的第一个分量 $M$ 归约到 $M'$ 时，整个有序对 $\langle M, N \rangle$ 归约为 $\langle M', N \rangle$。
    - $$\frac{M \rightarrow M'}{\langle M, N \rangle \rightarrow \langle M', N \rangle}$$
    - 投影的归约规则如 $\text{proj}_1 \langle V, W \rangle \rightarrow V$，表示从有序对中提取第一个分量。

4. **例子 (Examples)**:
    - 例如，交换有序对中的两个分量可以表示为 $swap_\times = \lambda z. \langle \text{proj}_2 z, \text{proj}_1 z \rangle$。

### 积的替代表示方法 (Alternative Representation of Product)

**概念与解释：**

1. **语法 (Syntax)**:
    - 积的替代表示方法使用一种匹配表达式来同时绑定两个变量。其形式为 `case× L [⟨x, y⟩ ⇒ N]`，表示从 $L$ 中解构出一个有序对，并将其分量绑定到 $x$ 和 $y$，然后在 $N$ 中使用它们。

2. **赋型 (Typing)**:
    - 如果 $L$ 的类型为 $A \times B$，在扩展的上下文 $\Gamma, x : A, y : B$ 中 $N$ 的类型为 $C$，则可以推导出 $\Gamma ⊢ \text{case× } L [⟨x, y⟩ ⇒ N] : C$。
    - $$\frac{\Gamma ⊢ L : A \times B \quad \Gamma, x : A, y : B ⊢ N : C}{\Gamma ⊢ \text{case× } L [⟨x, y⟩ ⇒ N] : C}$$

3. **归约 (Reduction)**:
    - 归约规则包括匹配表达式的归约。例如，当 $L$ 归约到 $L'$ 时，匹配表达式 `case× L [⟨x, y⟩ ⇒ N]` 归约为 `case× L' [⟨x, y⟩ ⇒ N]`。
    - $$\frac{L \rightarrow L'}{\text{case× } L [⟨x, y⟩ ⇒ N] \rightarrow \text{case× } L' [⟨x, y⟩ ⇒ N]}$$
    - 当 $L$ 是一个有序对时，匹配表达式会直接解构有序对并进行替换：$\text{case× } \langle V, W \rangle [⟨x, y⟩ ⇒ N] \rightarrow N[x := V][y := W]$。

4. **例子 (Examples)**:
    - 例如，使用 `case×` 表示的有序对分量交换函数为 `swap×-case = λ z ⇒ case× z [⟨x, y⟩ ⇒ ⟨y, x⟩]`。

### 和类型 (Sum Type)

**概念与解释：**

1. **语法 (Syntax)**:
    - 和类型的形式表示为 $A \oplus B$，它表示两个类型 $A$ 和 $B$ 的和类型，类似于一种选择结构。

2. **赋型 (Typing)**:
    - 和类型的引入规则包括两个注入规则：`inj₁` 和 `inj₂`，分别将项注入到和类型的第一个或第二个分量。
    - $$\frac{\Gamma ⊢ M : A}{\Gamma ⊢ \text{inj}_1 M : A \oplus B} \quad \text{和} \quad \frac{\Gamma ⊢ N : B}{\Gamma ⊢ \text{inj}_2 N : A \oplus B}$$

3. **归约 (Reduction)**:
    - 和类型的归约规则涉及匹配表达式的归约。例如，当匹配表达式的输入项 $L$ 归约到 $L'$ 时，整个表达式归约为 `case⊎ L' [inj₁ x ⇒ M | inj₂ y ⇒ N]`。
    - $$\frac{L \rightarrow L'}{\text{case⊎ } L [inj₁ x ⇒ M | inj₂ y ⇒ N] \rightarrow \text{case⊎ } L' [inj₁ x ⇒ M | inj₂ y ⇒ N]}$$
    - 注入后的匹配表达式会直接将注入的值替换到相应的分支中，例如：$\text{case⊎ } (\text{inj}_1 V) [inj₁ x ⇒ M | inj₂ y ⇒ N] \rightarrow M[x := V]$。

4. **例子 (Examples)**:
    - 和类型的一个例子是实现两个分量的交换函数 `swap⊎ = λ z ⇒ case⊎ z [inj₁ x ⇒ inj₂ x | inj₂ y ⇒ inj₁ y]`。

后续章节将继续探讨更多高级类型与构造的概念。



### 单元类型 (Unit Type)

**概念与解释：**

1. **语法 (Syntax)**:
    - 单元类型通常表示为 $⊤$，它只有一个值，通常表示为 `tt`。单元类型在逻辑中对应于命题的“真”，在编程中对应于“无实际内容”的占位符。

2. **赋型 (Typing)**:
    - 单元类型的赋型规则非常简单，在任何上下文 $\Gamma$ 中，`tt` 的类型为 $⊤$：
    - $$\frac{}{\Gamma ⊢ \text{tt} : ⊤}$$

3. **归约 (Reduction)**:
    - 单元类型没有归约规则，因为它只有一个值 `tt`，并且不能被进一步简化。

4. **例子 (Examples)**:
    - 一个简单的例子是定义从类型 $A$ 到 $A \times ⊤$ 的映射函数 `to×⊤ = λ x ⇒ ⟨x, tt⟩`，它将一个值 $x$ 映射到一个有序对，其中第二个分量是单元类型的值 `tt`。
    - 另一个例子是从 $A \times ⊤$ 到 $A$ 的映射 `from×⊤ = λ z ⇒ \text{proj}_1 z`，它从有序对中提取出第一个分量。

### 单元类型的替代表示方法 (Alternative Representation of Unit Type)

**概念与解释：**

1. **语法 (Syntax)**:
    - 单元类型的替代表示方法使用匹配表达式 `case⊤ L [tt ⇒ N]`。这里的 $L$ 是一个单元类型的值，`tt` 是匹配的唯一可能结果，然后在 $N$ 中使用这个结果。

2. **赋型 (Typing)**:
    - 如果 $L$ 的类型是 $⊤$，并且在上下文 $\Gamma$ 中 $N$ 的类型为 $A$，则可以推导出 $\Gamma ⊢ \text{case⊤ } L [tt ⇒ N] : A$：
    - $$\frac{\Gamma ⊢ L : ⊤ \quad \Gamma ⊢ N : A}{\Gamma ⊢ \text{case⊤ } L [tt ⇒ N] : A}$$

3. **归约 (Reduction)**:
    - 归约规则规定当 $L$ 归约到 $L'$ 时，匹配表达式 `case⊤ L [tt ⇒ N]` 归约为 `case⊤ L' [tt ⇒ N]`：
    - $$\frac{L \rightarrow L'}{\text{case⊤ } L [tt ⇒ N] \rightarrow \text{case⊤ } L' [tt ⇒ N]}$$
    - 当 $L$ 是单元值 `tt` 时，匹配表达式直接归约为 $N$：
    - $$\text{case⊤ } \text{tt} [tt ⇒ N] \rightarrow N$$

4. **例子 (Examples)**:
    - 一个示例是从 $A \times ⊤$ 到 $A$ 的映射函数 `from×⊤-case = λ z ⇒ \text{case× } z [⟨x, y⟩ ⇒ \text{case⊤ } y [tt ⇒ x]]`，该函数通过匹配表达式从有序对中提取第一个分量。

5. **翻译 (Translation)**:
    - 我们可以将 `case⊤ L [tt ⇒ N]` 翻译为 `let z = L in N`，这里 $z$ 是 $N$ 中不存在的自由变量。

### 空类型 (Empty Type)

**概念与解释：**

1. **语法 (Syntax)**:
    - 空类型通常表示为 $⊥$，它没有值。在逻辑中，它对应于命题的“假”，在编程中表示一种“不可能的情况”。

2. **赋型 (Typing)**:
    - 空类型的赋型规则只有一种形式，即消去规则。假设我们有一个项 $L$，它的类型为 $⊥$，那么在上下文 $\Gamma$ 中任何类型 $A$ 的项都可以通过 `case⊥ L []` 构造。
    - $$\frac{\Gamma ⊢ L : ⊥}{\Gamma ⊢ \text{case⊥ } L [] : A}$$

3. **归约 (Reduction)**:
    - 空类型的归约规则表明，当 $L$ 归约到 $L'$ 时，匹配表达式 `case⊥ L []` 归约为 `case⊥ L' []`：
    - $$\frac{L \rightarrow L'}{\text{case⊥ } L [] \rightarrow \text{case⊥ } L' []}$$

4. **例子 (Examples)**:
    - 一个例子是 $A \oplus ⊥$ 与 $A$ 的同构函数。该函数将值从 $A \oplus ⊥$ 映射到 $A$：
    - `from⊎⊥ = λ z ⇒ case⊎ z [inj₁ x ⇒ x | inj₂ y ⇒ case⊥ y []]`。

### 列表 (List)

**概念与解释：**

1. **语法 (Syntax)**:
    - 列表是一种递归的数据结构，其语法包括空列表 `[]` 和构造列表的操作符 `∷`，表示将一个元素加到一个列表的开头。

2. **赋型 (Typing)**:
    - 列表的赋型规则包括两个部分：空列表和构造操作符。如果上下文 $\Gamma$ 中 $M$ 的类型为 $A$，$N$ 的类型为 $List A$，则 $\Gamma ⊢ M ∷ N : List A$：
    - $$\frac{}{\Gamma ⊢ [] : List A} \quad \text{和} \quad \frac{\Gamma ⊢ M : A \quad \Gamma ⊢ N : List A}{\Gamma ⊢ M ∷ N : List A}$$

3. **归约 (Reduction)**:
    - 列表的归约规则涉及构造和匹配表达式的归约。例如，当匹配表达式的输入列表 $L$ 归约到 $L'$ 时，表达式 `caseL L [[] ⇒ M | x ∷ xs ⇒ N]` 归约为 `caseL L' [[] ⇒ M | x ∷ xs ⇒ N]`：
    - $$\frac{L \rightarrow L'}{\text{caseL } L [[] ⇒ M | x ∷ xs ⇒ N] \rightarrow \text{caseL } L' [[] ⇒ M | x ∷ xs ⇒ N]}$$
    - 当列表是空列表 `[]` 时，匹配表达式直接归约为 $M$：
    - $$\text{caseL } [] [[] ⇒ M | x ∷ xs ⇒ N] \rightarrow M$$

4. **例子 (Examples)**:
    - 列表的一个常见操作是映射函数 `mapL`，它将一个函数应用到列表中的每一个元素：
    - `mapL = μ mL ⇒ λ f ⇒ λ xs ⇒ caseL xs [[] ⇒ [] | x ∷ xs ⇒ f · x ∷ mL · f · xs]`。

本章节介绍了更多的构造和类型，扩展了简单类型 $\lambda$ 演算的表达能力，使得它可以表示更加复杂的数据结构和计算模式。下一章节将进一步探讨这些扩展的形式化定义及其性质。

### 形式化构造 (Formalization of Constructs)

这一部分深入探讨了如何对前面章节中介绍的构造进行形式化，包括原语数字、let 绑定、积类型以及积类型的替代表示方法。这部分内容主要使用内在类型 (intrinsic typing) 的形式化方法，并介绍了如何将这些构造应用于简单类型 $\lambda$ 演算。

#### 语法 (Syntax)

**概念与解释：**

1. **类型 (Types)**:
    - 类型的定义扩展了之前的形式，包括自然数类型 `Nat` 和积类型 `A × B`。
    - 类型的定义如下：
      ```agda
      data Type : Set where
        `ℕ    : Type
        _⇒_   : Type → Type → Type
        Nat   : Type
        _`×_  : Type → Type → Type
      ```

2. **上下文 (Context)**:
    - 上下文定义为类型的列表，表示变量在上下文中的类型信息。上下文可以是空的，也可以通过添加新类型扩展已有的上下文。
    - 上下文的定义如下：
      ```agda
      data Context : Set where
        ∅   : Context
        _,_ : Context → Type → Context
      ```

3. **变量及查询判断 (Variables and Lookup Judgements)**:
    - 变量由 de Bruijn 因子表示，查询判断表示在特定上下文中，变量的类型。
    - 例如，Z 表示最近的变量，S 表示更外层的变量。
    - 查询判断的定义如下：
      ```agda
      data _∋_ : Context → Type → Set where
        Z : ∀ {Γ A}
          ---------
          → Γ , A ∋ A
      
        S_ : ∀ {Γ A B}
          → Γ ∋ B
          ---------
          → Γ , A ∋ B
      ```

4. **项及赋型判断 (Terms and Typing Judgements)**:
    - 项 (terms) 定义了语言的语法，包括变量、函数、自然数、let 绑定、积类型等。
    - 赋型判断 (typing judgement) 规定了每个项在上下文中的类型。
    - 项和赋型判断的定义如下：
      ```agda
      data _⊢_ : Context → Type → Set where
        -- 变量
        `_ : ∀ {Γ A}
          → Γ ∋ A
            -----
          → Γ ⊢ A
      
        -- 函数
        ƛ_  :  ∀ {Γ A B}
          → Γ , A ⊢ B
            ---------
          → Γ ⊢ A ⇒ B
      
        _·_ : ∀ {Γ A B}
          → Γ ⊢ A ⇒ B
          → Γ ⊢ A
            ---------
          → Γ ⊢ B
      
        -- 自然数
        `zero : ∀ {Γ}
            ------
          → Γ ⊢ `ℕ
      
        `suc_ : ∀ {Γ}
          → Γ ⊢ `ℕ
            ------
          → Γ ⊢ `ℕ
      
        -- 原语数字
        con : ∀ {Γ}
          → ℕ
            -------
          → Γ ⊢ Nat
      
        _`*_ : ∀ {Γ}
          → Γ ⊢ Nat
          → Γ ⊢ Nat
            -------
          → Γ ⊢ Nat
      
        -- let 绑定
        `let : ∀ {Γ A B}
          → Γ ⊢ A
          → Γ , A ⊢ B
            ----------
          → Γ ⊢ B
      
        -- 积类型
        `⟨_,_⟩ : ∀ {Γ A B}
          → Γ ⊢ A
          → Γ ⊢ B
            -----------
          → Γ ⊢ A `× B
      
        `proj₁ : ∀ {Γ A B}
          → Γ ⊢ A `× B
            -----------
          → Γ ⊢ A
      
        `proj₂ : ∀ {Γ A B}
          → Γ ⊢ A `× B
            -----------
          → Γ ⊢ B
      
        -- 积的替代表示方法
        case× : ∀ {Γ A B C}
          → Γ ⊢ A `× B
          → Γ , A , B ⊢ C
            --------------
          → Γ ⊢ C
      ```

### 缩减 de Bruijn 因子 (Reducing de Bruijn Indices)

**概念与解释：**

1. **length 函数**:
    - `length` 函数计算上下文的长度，即上下文中类型的数量。这个函数在之后用于确保一个因子在上下文中合法。
    - 函数定义如下：
      ```agda
      length : Context → ℕ
      length ∅        =  zero
      length (Γ , _)  =  suc (length Γ)
      ```

2. **lookup 函数**:
    - `lookup` 函数根据上下文和 de Bruijn 因子的值来查找对应的类型。
    - 函数定义如下：
      ```agda
      lookup : {Γ : Context} → {n : ℕ} → (p : n < length Γ) → Type
      lookup {(_ , A)} {zero}    (s≤s z≤n)  =  A
      lookup {(Γ , _)} {(suc n)} (s≤s p)    =  lookup p
      ```

3. **count 函数**:
    - `count` 函数将自然数转换成其对应的 de Bruijn 因子，并从上下文中查询它的类型。
    - 函数定义如下：
      ```agda
      count : ∀ {Γ} → {n : ℕ} → (p : n < length Γ) → Γ ∋ lookup p
      count {_ , _} {zero}    (s≤s z≤n)  =  Z
      count {Γ , _} {(suc n)} (s≤s p)    =  S (count p)
      ```

4. **#_ 函数**:
    - `#_` 函数是一个简略表示方法，用于从上下文中选择一个类型，并返回相应的 de Bruijn 因子。
    - 函数定义如下：
      ```agda
      #_ : ∀ {Γ}
        → (n : ℕ)
        → {n∈Γ : True (suc n ≤? length Γ)}
          --------------------------------
        → Γ ⊢ lookup (toWitness n∈Γ)
      #_ n {n∈Γ}  =  ` count (toWitness n∈Γ)
      ```

这些函数和类型定义为接下来的形式化构造打下了基础，特别是在处理变量和上下文时，它们确保了类型的正确性和表达的精确性。接下来，我们将探讨如何进行重命名和代换操作。

### 重命名 (Renaming)

**概念与解释：**

1. **ext 函数**:
    - `ext` 函数用于扩展一个映射，使其能够处理带有新变量的上下文。这是为了处理语境中变量扩展时的重命名问题。
    - 函数定义如下：
      ```agda
      ext : ∀ {Γ Δ}
        → (∀ {A}   →     Γ ∋ A →     Δ ∋ A)
          ---------------------------------
        → (∀ {A B} → Γ , A ∋ B → Δ , A ∋ B)
      ext ρ Z      =  Z
      ext ρ (S x)  =  S (ρ x)
      ```

2. **rename 函数**:
    - `rename` 函数定义了如何对一个上下文中的变量进行重命名，并将这个操作应用于整个项。它直接递归地对项中的每个部分进行重命名。
    - 函数定义如下：
      ```agda
      rename : ∀ {Γ Δ}
        → (∀ {A} → Γ ∋ A → Δ ∋ A)
          -----------------------
        → (∀ {A} → Γ ⊢ A → Δ ⊢ A)
      rename ρ (` x)          =  ` (ρ x)
      rename ρ (ƛ N)          =  ƛ (rename (ext ρ) N)
      rename ρ (L · M)        =  (rename ρ L) · (rename ρ M)
      rename ρ (`zero)        =  `zero
      rename ρ (`suc M)       =  `suc (rename ρ M)
      rename ρ (case L M N)   =  case (rename ρ L) (rename ρ M) (rename (ext ρ) N)
      rename ρ (μ N)          =  μ (rename (ext ρ) N)
      ```

3. **重命名操作的重要性**:
    - 重命名操作在处理复杂项时非常重要，特别是在保证类型安全的情况下。通过定义 `

ext` 和 `rename` 函数，语言可以正确地处理变量的重命名，从而避免类型错误。

### 代换 (Substitution)

**概念与解释：**

1. **exts 函数**:
    - `exts` 函数用于扩展代换 (substitution) 映射。它允许在给定上下文的基础上，扩展新的变量，并将它们映射到新的项。
    - 函数定义如下：
      ```agda
      exts : ∀ {Γ Δ} → (∀ {A} → Γ ∋ A → Δ ⊢ A) → (∀ {A B} → Γ , A ∋ B → Δ , A ⊢ B)
      exts σ Z      =  ` Z
      exts σ (S x)  =  rename S_ (σ x)
      ```

2. **subst 函数**:
    - `subst` 函数定义了如何在项中进行代换操作。它递归地将代换应用于项的每个部分，以确保代换的正确性。
    - 函数定义如下：
      ```agda
      subst : ∀ {Γ Δ} → (∀ {C} → Γ ∋ C → Δ ⊢ C) → (∀ {C} → Γ ⊢ C → Δ ⊢ C)
      subst σ (` k)          =  σ k
      subst σ (ƛ N)          =  ƛ (subst (exts σ) N)
      subst σ (L · M)        =  (subst σ L) · (subst σ M)
      subst σ (`zero)        =  `zero
      subst σ (`suc M)       =  `suc (subst σ M)
      ```

3. **代换操作的用途**:
    - 代换操作用于处理在项中的变量替换，确保即使在复杂的表达式中，代换后的项依然保持类型的正确性。这在实现计算模型或编译器时至关重要。

通过这些操作的形式化定义，可以确保在编写程序或进行逻辑推导时，变量和类型的操作是安全的，避免了常见的编译和推理错误。这些操作是简单类型 $\lambda$ 演算及其扩展构造的基础，确保了构造的正确性和表达能力。

### 同时代换 (Simultaneous Substitution)

**概念与解释：**

1. **单个代换 (Single Substitution) 与双重代换 (Double Substitution)**:
    - `[_][_]:` 这个操作符表示同时进行两个代换。在某些情况下，这比单独的两次代换更为方便和高效。
    - 例如，$N[V][W]$ 表示在 $N$ 中首先用 $V$ 代换某个变量，然后用 $W$ 进行第二次代换。

2. **定义与解释**:
    - 同时代换的定义如下：
      ```agda
      _[_][_] : ∀ {Γ A B C}
        → Γ , A , B ⊢ C
        → Γ ⊢ A
        → Γ ⊢ B
          -------------
        → Γ ⊢ C
      _[_][_] {Γ} {A} {B} N V W =  subst {Γ , A , B} {Γ} σ N
        where
        σ : ∀ {C} → Γ , A , B ∋ C → Γ ⊢ C
        σ Z          =  W
        σ (S Z)      =  V
        σ (S (S x))  =  ` x
      ```

通过对这些操作的理解和应用，可以确保在处理复杂表达式和类型时，程序的正确性和类型的安全性。这些操作是简单类型 $\lambda$ 演算的核心机制之一。

### 值 (Values)

在这部分中，我们正式定义了简单类型 $\lambda$ 演算中的值。值是那些不再能够进一步归约的项 (terms)。在计算过程中，一旦一个项归约为一个值，就意味着计算达到了一个稳定状态，不需要再进一步计算。

**概念与解释：**

1. **值的定义 (Definition of Values)**:
    - 一个值是某种类型的项，它不能再进一步归约。这些值包括：
        - 函数 (lambdas)
        - 自然数 (natural numbers)
        - 原语数字 (primitive numbers)
        - 有序对 (pairs)
    - 值的定义如下：
      ```agda
      data Value : ∀ {Γ A} → Γ ⊢ A → Set where
      
        -- 函数
        V-ƛ : ∀ {Γ A B} {N : Γ , A ⊢ B}
          ---------------------------
          → Value (ƛ N)
      
        -- 自然数
        V-zero : ∀ {Γ}
          -----------------
          → Value (`zero {Γ})
      
        V-suc_ : ∀ {Γ} {V : Γ ⊢ `ℕ}
          → Value V
          --------------
          → Value (`suc V)
      
        -- 原语数字
        V-con : ∀ {Γ n}
          -----------------
          → Value (con {Γ} n)
      
        -- 积 (有序对)
        V-⟨_,_⟩ : ∀ {Γ A B} {V : Γ ⊢ A} {W : Γ ⊢ B}
          → Value V
          → Value W
          ----------------
          → Value `⟨ V , W ⟩
      ```

2. **函数 (Lambdas) 的值**:
    - 函数是一个重要的值类型。它们的定义方式是使用 $\lambda$ 演算构造的抽象函数，这些函数不能进一步归约。

3. **自然数 (Natural Numbers) 的值**:
    - 自然数的值包括 `zero` 和 `suc`，表示零和后继（即下一个自然数）。这些值不能再进一步简化或归约。
    - 例如，`zero` 是自然数 0 的表示，`suc zero` 表示 1，`suc (suc zero)` 表示 2。

4. **原语数字 (Primitive Numbers) 的值**:
    - 原语数字是那些内置的数字值，比如 1，2，3 等。它们在计算过程中不会被进一步归约。

5. **有序对 (Pairs) 的值**:
    - 有序对的值表示两个值的组合。一个有序对是由两个值组成的，它们本身也不能再进一步归约。
    - 例如，`⟨1, 2⟩` 表示一个由两个数字 1 和 2 组成的有序对。

这些定义确保了在计算过程中，能明确判断一个项是否已经达到了值的状态。值是计算的终止条件，确保了程序在达到特定状态后不再进行无意义的计算。

### 归约 (Reduction)

归约定义了项如何在计算过程中一步步被简化为值。这里详细定义了各种情况下的归约规则。

**概念与解释：**

1. **归约规则的定义**:
    - 归约规则定义了项如何一步步被简化。它们是计算的基本操作，通过这些规则，一个项最终会被归约为值。
    - 归约规则的定义如下：
      ```agda
      infix 2 _—→_
      
      data _—→_ : ∀ {Γ A} → (Γ ⊢ A) → (Γ ⊢ A) → Set where
      
        -- 函数应用的归约
        ξ-·₁ : ∀ {Γ A B} {L L′ : Γ ⊢ A ⇒ B} {M : Γ ⊢ A}
          → L —→ L′
            ---------------
          → L · M —→ L′ · M
      
        ξ-·₂ : ∀ {Γ A B} {V : Γ ⊢ A ⇒ B} {M M′ : Γ ⊢ A}
          → Value V
          → M —→ M′
            ---------------
          → V · M —→ V · M′
      
        β-ƛ : ∀ {Γ A B} {N : Γ , A ⊢ B} {V : Γ ⊢ A}
          → Value V
            --------------------
          → (ƛ N) · V —→ N [ V ]
      
        -- 自然数的归约
        ξ-suc : ∀ {Γ} {M M′ : Γ ⊢ `ℕ}
          → M —→ M′
            -----------------
          → `suc M —→ `suc M′
      
        ξ-case : ∀ {Γ A} {L L′ : Γ ⊢ `ℕ} {M : Γ ⊢ A} {N : Γ , `ℕ ⊢ A}
          → L —→ L′
            -------------------------
          → case L M N —→ case L′ M N
      
        β-zero :  ∀ {Γ A} {M : Γ ⊢ A} {N : Γ , `ℕ ⊢ A}
            -------------------
          → case `zero M N —→ M
      
        β-suc : ∀ {Γ A} {V : Γ ⊢ `ℕ} {M : Γ ⊢ A} {N : Γ , `ℕ ⊢ A}
          → Value V
            ----------------------------
          → case (`suc V) M N —→ N [ V ]
      ```

2. **函数应用 (Function Application) 的归约**:
    - 这里定义了当函数被应用时，它们如何归约为结果的过程。例如，当一个函数应用了一个值后，通过规则 `β-ƛ`，这个值被代入函数体中。
    - 例如，`(λ x . x + 1) · 2` 可以归约为 `2 + 1`。

3. **自然数 (Natural Numbers) 的归约**:
    - 自然数的归约规则主要用于处理 `suc` 和 `case` 语句。
    - 例如，`suc (suc 0)` 通过 `ξ-suc` 规则可以简化为 `2`。

4. **案例 (Case) 语句的归约**:
    - `case` 语句用于处理条件分支，根据自然数的不同，选择不同的分支进行计算。例如，`case 0 of {0 -> A | n -> B}` 会简化为 `A`。

5. **组合归约 (Composition of Reductions)**:
    - 归约规则可以组合使用，通过多个步骤将复杂的表达式简化为值。例如：
      - 通过多个 `ξ-·₁` 和 `ξ-·₂` 规则，可以将多步计算归约为最终的值。

### 进展 (Progress)

进展 (Progress) 是指一项能在计算过程中进一步简化或者已经是值的状态。

**概念与解释：**

1. **进展的定义 (Definition of Progress)**:
    - 进展是指项在计算过程中，要么能进一步简化 (step)，要么已经是值 (done)。
    - 定义如下：
      ```agda
      data Progress {A} (M : ∅ ⊢ A) : Set where
      
        step : ∀ {N : ∅ ⊢ A}
          → M —→ N
            ----------
          → Progress M
      
        done :
            Value M
            ----------
          → Progress M
      ```

2. **进展的意义**:
    - 进展确保了程序的正确性，即程序的每一步要么是一个归约步骤，要么已经达到最终结果 (值)。
    - 这意味着程序不会陷入无限循环或者停滞不前。

3. **进展的计算 (Progress Computation)**:
    - `progress` 函数定义了如何计算项的进展状态。它递归地检查项是否能进行归约，并返回相应的进展状态。
    - 例如，`progress (suc zero)` 会返回 `done`，因为 `suc zero` 是一个值。
    - 定义如下：
```agda
      progress : ∀ {A}
        → (M : ∅ ⊢ A)
          -----------
        → Progress M
      progress (` ())
      progress (ƛ N)                              =  done V-ƛ
      progress (L · M) with progress L
      ...    | step L—→L′                         =  step (ξ-·₁ L—→L′)
      ...    | done V-ƛ with progress M
      ...        | step M—→M′                     =  step (ξ-·₂ V-ƛ M—→M′)
      ...        | done VM                        =  step (β-ƛ VM)
      progress (`zero)                            =  done V-zero
      progress (`suc M) with progress M


      ...    | step M—→M′                         =  step (ξ-suc M—→M′)
      ...    | done VM                            =  done (V-suc VM)
```

通过这些进展规则的定义，可以确保在计算过程中，项总是能够前进到一个新的状态，最终达到值的状态，确保了程序的终止性。

这些部分定义了简单类型 $\lambda$ 演算中的关键概念，确保了计算过程的正确性和可靠性。通过这些严格的形式化定义，可以保证程序不会出现无穷归约、类型错误等问题。

### 求值 (Evaluation)

在这个部分，我们定义了求值过程，也就是如何在有限的计算资源（比如汽油，Gas）下对一个项进行归约，直到项变成一个值或者资源耗尽为止。

**概念与解释：**

1. **汽油 (Gas) 的概念**:
    - 为了避免无限循环，我们引入了一个称为“汽油”的计数器。每次进行归约时，消耗一点汽油。如果汽油用尽而还未达到值状态，则停止计算。
    - 汽油的定义如下：
      ```agda
      record Gas : Set where
        constructor gas
        field
          amount : ℕ
      ```

2. **完成状态 (Finished State)**:
    - 当项被归约到一个值时，我们称其为“完成”。如果汽油耗尽而未达到值，则返回“耗尽”状态。
    - 定义如下：
      ```agda
      data Finished {Γ A} (N : Γ ⊢ A) : Set where
         done :
             Value N
             ----------
           → Finished N
         out-of-gas :
             ----------
             Finished N
      ```

3. **求值步骤 (Steps of Evaluation)**:
    - 求值的过程是一个递归过程，逐步消耗汽油，进行归约，直到达到值或汽油耗尽。
    - 每个求值步骤记录从初始项到最终项的整个归约过程。
    - 定义如下：
      ```agda
      data Steps {A} : ∅ ⊢ A → Set where
        steps : {L N : ∅ ⊢ A}
          → L —↠ N
          → Finished N
            ----------
          → Steps L
      ```

4. **求值器 (Evaluator)**:
    - 求值器接受汽油和一个项作为输入，输出该项的求值步骤。
    - 递归地进行求值，每次消耗一点汽油并进行一步归约。
    - 定义如下：
      ```agda
      eval : ∀ {A}
        → Gas
        → (L : ∅ ⊢ A)
          -----------
        → Steps L
      eval (gas zero)    L                     =  steps (L ∎) out-of-gas
      eval (gas (suc m)) L with progress L
      ... | done VL                            =  steps (L ∎) (done VL)
      ... | step {M} L—→M with eval (gas m) M
      ...    | steps M—↠N fin                  =  steps (L —→⟨ L—→M ⟩ M—↠N) fin
      ```

5. **求值示例 (Examples of Evaluation)**:
    - **立方函数 (Cube Function)**:
        - 定义了一个立方函数 `cube`，计算某个自然数的立方。
        - 求值示例如下：
          ```agda
          cube : ∅ ⊢ Nat ⇒ Nat
          cube = ƛ (# 0 `* # 0 `* # 0)
          
          _ : cube · con 2 —↠ con 8
          _ =
            begin
              cube · con 2
            —→⟨ β-ƛ V-con ⟩
              con 2 `* con 2 `* con 2
            —→⟨ ξ-*₁ δ-* ⟩
              con 4 `* con 2
            —→⟨ δ-* ⟩
              con 8
            ∎
          ```

    - **十次方函数 (Exponentiation by 10)**:
        - 通过 `let` 绑定计算某个数的十次方。
        - 求值示例如下：
          ```agda
          exp10 : ∅ ⊢ Nat ⇒ Nat
          exp10 = ƛ (`let (# 0 `* # 0)
                      (`let (# 0 `* # 0)
                        (`let (# 0 `* # 2)
                          (# 0 `* # 0))))
          
          _ : exp10 · con 2 —↠ con 1024
          _ =
            begin
              exp10 · con 2
            —→⟨ β-ƛ V-con ⟩
              `let (con 2 `* con 2) (`let (# 0 `* # 0) (`let (# 0 `* con 2) (# 0 `* # 0)))
            —→⟨ ξ-let δ-* ⟩
              `let (con 4) (`let (# 0 `* # 0) (`let (# 0 `* con 2) (# 0 `* # 0)))
            —→⟨ β-let V-con ⟩
              `let (con 4 `* con 4) (`let (# 0 `* con 2) (# 0 `* # 0))
            —→⟨ ξ-let δ-* ⟩
              `let (con 16) (`let (# 0 `* con 2) (# 0 `* # 0))
            —→⟨ β-let V-con ⟩
              `let (con 16 `* con 2) (# 0 `* # 0)
            —→⟨ ξ-let δ-* ⟩
              `let (con 32) (# 0 `* # 0)
            —→⟨ β-let V-con ⟩
              con 32 `* con 32
            —→⟨ δ-* ⟩
              con 1024
            ∎
          ```

6. **交换函数 (Swap Function)**:
    - 定义了一个交换函数 `swap×`，交换一个有序对中的两个元素。
    - 求值示例如下：
      ```agda
      swap× : ∀ {A B} → ∅ ⊢ A `× B ⇒ B `× A
      swap× = ƛ `⟨ `proj₂ (# 0) , `proj₁ (# 0) ⟩
      
      _ : swap× · `⟨ con 42 , `zero ⟩ —↠ `⟨ `zero , con 42 ⟩
      _ =
        begin
          swap× · `⟨ con 42 , `zero ⟩
        —→⟨ β-ƛ V-⟨ V-con , V-zero ⟩ ⟩
          `⟨ `proj₂ `⟨ con 42 , `zero ⟩ , `proj₁ `⟨ con 42 , `zero ⟩ ⟩
        —→⟨ ξ-⟨,⟩₁ (β-proj₂ V-con V-zero) ⟩
          `⟨ `zero , `proj₁ `⟨ con 42 , `zero ⟩ ⟩
        —→⟨ ξ-⟨,⟩₂ V-zero (β-proj₁ V-con V-zero) ⟩
          `⟨ `zero , con 42 ⟩
        ∎
      ```

通过这些求值规则和示例，可以看到如何在受限的资源下对项进行计算，并最终得到结果。这些定义确保了计算过程的安全性和终止性，避免了无穷循环和资源耗尽的问题。

这部分展示了如何在形式化系统中定义和验证简单类型 $\lambda$ 演算的计算过程，包括处理资源限制的策略，确保了系统的可靠性。
