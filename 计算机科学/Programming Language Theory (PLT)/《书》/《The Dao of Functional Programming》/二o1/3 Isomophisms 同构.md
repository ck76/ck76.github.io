[toc]



# 第 3 章：同构

在前面的章节中，我们讨论了类型、函数以及它们之间的关系。在本章中，我们将深入探讨**同构**（*Isomorphism*）的概念，这是数学和编程中非常重要的主题。我们将了解什么是同构对象，它们的特性，以及如何使用箭头进行推理。

## 3.1 同构对象

当我们写下如下等式：

$$
f \circ (g \circ h) = (f \circ g) \circ h
$$

或者：

$$
f = f \circ id
$$

我们正在断言**箭头的相等性**。左边的箭头是某个操作的结果，右边的箭头是另一个操作的结果，但它们的结果是**相等**的。

### 交换图

为了说明这种相等性，我们通常使用**交换图**来表示。例如：

$$
\begin{tikzcd}
a \arrow[r, "h"] \arrow[rr, bend left=45, "g \circ h"] \arrow[rrr, bend left=80, "f \circ (g \circ h)"] \arrow[rrr, bend right=80, "(f \circ g) \circ h"'] & b \arrow[r, "g"] \arrow[rr, bend right=45, "f \circ g"'] & c \arrow[r, "f"] & d
\end{tikzcd}
$$

以及：

$$
\begin{tikzcd}
a \arrow[r, "f"] \arrow[loop left, "id"'] & b
\end{tikzcd}
$$

这些图表展示了箭头之间的相等性和组合关系。

### 对象的比较

在范畴论中，我们**不比较对象的相等性**。相反，我们将对象视为箭头的汇合点，如果我们想比较两个对象，我们会看它们之间的箭头。

最简单的两个对象之间的关系就是箭头。最简单的往返（双向）关系是由两个方向相反的箭头组成的组合：

$$
\begin{tikzcd}
a \arrow[r, bend left, "f"] & b \arrow[l, bend left, "g"]
\end{tikzcd}
$$

在这个图中，有两种可能的往返方式：

1. 从 $a$ 到 $a$ 的组合箭头：$g \circ f$。
2. 从 $b$ 到 $b$ 的组合箭头：$f \circ g$。

如果这两个组合箭头都等于对应对象上的**恒等箭头**（Identity Arrow），即：

$$
g \circ f = id_a
$$

以及

$$
f \circ g = id_b
$$

那么我们说 $g$ 是 $f$ 的**逆元**（Inverse），记作 $g = f^{-1}$。也就是说，箭头 $f^{-1}$ 撤销了箭头 $f$ 的作用。

### 同构的定义

这样的一对箭头被称为**同构**（Isomorphism），这两个对象被称为**同构的**（Isomorphic）。同构的存在告诉我们，这两个对象在结构上是相同的，它们可以通过箭头彼此互相转换。

#### 例子：同构对象的性质

我们已经知道，对象是通过与其他对象的相互作用来描述的。让我们考虑从观察者 $x$ 的角度看这两个同构对象 $a$ 和 $b$。

假设有一个箭头 $h$ 从 $x$ 到 $a$：

$$
\begin{tikzcd}
& x \arrow[ld, "h"'] \\
a \arrow[rr, "f"] && b \arrow[ll, bend left, "f^{-1}"]
\end{tikzcd}
$$

通过组合 $f$ 和 $h$，我们可以得到一个从 $x$ 到 $b$ 的箭头：

$$
f \circ h : x \to b
$$

这表示我们可以使用后组合 $(f \circ -)$ 将焦点从 $a$ 切换到 $b$。

同样，对于任何探测 $b$ 的箭头，我们可以使用 $f^{-1}$ 将焦点从 $b$ 切换回 $a$。这允许我们在 $a$ 和 $b$ 之间来回切换。

### 同构的意义

这意味着从 $x$ 的角度来看，箭头之间存在一一对应关系。也就是说，对于每个从 $x$ 到 $a$ 的箭头，都存在一个对应的从 $x$ 到 $b$ 的箭头，反之亦然。

数学家称这种一一对应为**双射**（Bijection）。

因此，就箭头而言，这两个对象之间没有区别。它们具有完全相同的属性，特别是它们的元素（从终端对象 $1$ 到它们的箭头）是一一对应的。

我们将这种同构关系表示为：

$$
a \cong b
$$

当涉及到对象时，我们用同构代替相等。在编程中，两个同构的类型具有相同的外部行为。一个类型可以用另一个类型来实现，反之亦然。你可以在不改变系统行为的情况下（可能除外性能）用一个类型替换另一个类型。

#### 逻辑中的等价

在经典逻辑中，如果 $B$ 从 $A$ 推导出来，且 $A$ 从 $B$ 推导出来，那么 $A$ 和 $B$ 在逻辑上是等价的。我们经常说“当且仅当”$A$ 为真时，$B$ 为真。

然而，当我们考虑证明的相关性时，这种等价关系并不那么直截了当。这实际上导致了一个新的数学分支的发展，称为**同伦类型论**（Homotopy Type Theory，简称 HoTT）。

### 练习

**练习 1**：提出一个论证，证明从两个同构对象出发的箭头之间存在双射。绘制相应的图来说明这一点。

**练习 2**：证明每个对象都与自身同构。

**练习 3**：如果存在两个终端对象，证明它们是同构的。

**练习 4**：证明前一个练习中的同构是唯一的。

---

## 3.2 自然性

我们已经看到，当两个对象同构时，我们可以使用**后组合** $(f \circ -)$ 在它们之间切换焦点。相反，要在不同的观察者之间切换，我们会使用**前组合** $(- \circ g)$。

### 视角的切换

考虑以下情况：从 $x$ 到 $a$ 的箭头 $h$ 与从 $y$ 到 $a$ 的箭头 $h \circ g$ 相关，其中 $g$ 是从 $y$ 到 $x$ 的箭头。

$$
\begin{tikzcd}
x \arrow[d, "h"'] && y \arrow[ll, dashed, "g"'] \arrow[dll, "h \circ g"] \\
a \arrow[rr, "f"] && b \arrow[ll, bend left, "f^{-1}"]
\end{tikzcd}
$$

在这种情况下，我们通过应用前组合 $(- \circ g)$ 来改变视角，从 $x$ 切换到 $y$。

### 自然性条件

重要的是，这种视角的改变保持了由同构建立的配对系统。也就是说，先应用 $g$（切换视角），然后应用 $f$（切换焦点），与先应用 $f$，然后应用 $g$，结果是相同的。

符号上，我们写作：

$$
(- \circ g) \circ (f \circ -) = (f \circ -) \circ (- \circ g)
$$

这个等式称为**自然性条件**（Naturality Condition）。

当你将这个等式应用于一个箭头 $h : x \to a$ 时，两边都简化为 $f \circ h \circ g$。

#### 交换图表示

这个自然性条件可以用以下交换图表示：

$$
\begin{tikzcd}
h \arrow[r, mapsto, "(- \circ g)"] \arrow[d, mapsto, "(f \circ -)"'] & h \circ g \arrow[d, mapsto, "(f \circ -)"] \\
f \circ h \arrow[r, mapsto, "(- \circ g)"] & f \circ h \circ g
\end{tikzcd}
$$

### 自然性的意义

自然性条件确保了操作的兼容性。在范畴论中，当我们需要从一个地方移动到另一个地方时，通常需要两个步骤。这里，前组合和后组合的操作可以按任意顺序完成，我们说它们**交换**。

自然性告诉我们，所有的对象都获得了一致的视图，而不管路径如何。

### 反转箭头的自然性

我们也可以逆转观察者和被观察者的角色。使用一个箭头 $h : a \to x$，对象 $a$ 可以探测任意对象 $x$。

如果有一个箭头 $g : y \to x$，我们可以通过前组合 $(- \circ g)$ 切换焦点到 $y$。

切换视角到 $b$ 是通过 $f^{-1}$ 的前组合完成的。

$$
\begin{tikzcd}
a \arrow[rr, "f"] \arrow[d, "h"'] \arrow[rrd, "g \circ h"] && b \arrow[ll, bend right, "f^{-1}"'] \\
x \arrow[rr, dashed, "g"'] && y
\end{tikzcd}
$$

自然性条件在这种情况下是：

$$
(- \circ f^{-1}) \circ (g \circ -) = (g \circ -) \circ (- \circ f^{-1})
$$

---

## 3.3 使用箭头推理

约内达大师（Master Yoneda）说：“**看箭头！**”

- 如果两个对象是同构的，它们拥有相同的入射箭头集合。
- 如果两个对象是同构的，它们也拥有相同的出射箭头集合。
- 如果你想知道两个对象是否同构，**看箭头！**

### 通过箭头建立同构

当两个对象 $a$ 和 $b$ 是同构的，任何同构 $f$ 都会在它们之间建立一个一一对应的箭头映射 $(f \circ -)$。

考虑所有从某个对象 $x$ 到 $a$ 的箭头 $h : x \to a$，通过后组合 $(f \circ -)$，它们被映射为从 $x$ 到 $b$ 的箭头 $f \circ h : x \to b$。

#### 图示表示

$$
\begin{tikzcd}
& x \arrow[ld, "h"'] \arrow[rd, "f \circ h"] \\
a \arrow[rr, "f"] && b \arrow[ll, bend left, "f^{-1}"]
\end{tikzcd}
$$

同样，逆映射 $(f^{-1} \circ -)$ 将从 $x$ 到 $b$ 的箭头映射回从 $x$ 到 $a$ 的箭头。

### 基于箭头的同构推理

假设我们不知道对象 $a$ 和 $b$ 是否同构，但我们知道对于每个对象 $x$，存在箭头集合之间的双射 $\alpha_x$，即从 $x$ 到 $a$ 的箭头和从 $x$ 到 $b$ 的箭头之间存在一一对应关系：

$$
\begin{tikzcd}
& x \arrow[ld, "h"'] \arrow[rd, "\alpha_x(h)"] \\
a && b
\end{tikzcd}
$$

我们可以问：这是否意味着这两个对象是同构的？我们能否从这些映射 $\{\alpha_x\}$ 中构造出同构 $f$？

### 自然变换和同构

答案是肯定的，只要这些映射族 $\{\alpha_x\}$ 满足**自然性条件**。

#### 利用自然性条件构造同构

由于 $\alpha_x$ 对于每个对象 $x$ 都定义了，我们可以在 $x = a$ 的情况下使用它。$\alpha_a$ 将从 $a$ 到 $a$ 的箭头映射到从 $a$ 到 $b$ 的箭头。

我们知道至少存在一个从 $a$ 到 $a$ 的箭头，即恒等箭头 $id_a$。因此，我们可以定义：

$$
f = \alpha_a(id_a)
$$

这就是我们要寻找的同构箭头 $f$。

#### 验证同构

接下来，我们需要验证对于任意对象 $x$，$\alpha_x$ 确实等于 $(f \circ -)$。通过应用自然性条件，我们可以证明这一点。

自然性条件要求：

$$
\alpha_y \circ (- \circ g) = (- \circ g) \circ \alpha_x
$$

当作用于箭头 $h : x \to a$ 时，我们希望以下图交换：

$$
\begin{tikzcd}
h \arrow[r, mapsto, "(- \circ g)"] \arrow[d, mapsto, "\alpha_x"'] & h \circ g \arrow[d, mapsto, "\alpha_y"] \\
\alpha_x(h) \arrow[r, mapsto, "(- \circ g)"] & \alpha_x(h) \circ g = \alpha_y(h \circ g)
\end{tikzcd}
$$

这样，我们就可以证明 $\alpha_x = (f \circ -)$，从而证明 $f$ 是同构。

### 反转箭头

同样，我们可以定义从 $b$ 到 $a$ 的箭头 $f^{-1}$，方法是利用另一个映射族 $\{\beta_x\}$，它们在箭头之间建立双射。

#### 构造逆同构

使用类似的方法，我们可以定义：

$$
f^{-1} = \beta_b(id_b)
$$

然后通过自然性条件证明 $\beta_x = (f^{-1} \circ -)$。

### 练习

**练习 5**：使用恒等箭头的技巧，从映射族 $\{\beta_x\}$ 中恢复 $f^{-1}$。

**练习 6**：使用前一个练习中的 $f^{-1}$，对于任意对象 $y$ 和任意箭头 $g : a \to y$，求出 $\beta_y(g)$ 的值。

---

## 3.4 小结

在本章中，我们深入探讨了同构的概念。我们了解了同构对象的性质，以及如何通过箭头之间的关系来判断两个对象是否同构。

关键点包括：

- **同构**是由两个方向相反的箭头组成的，它们的组合等于相应对象的恒等箭头。
- **自然性条件**确保了操作的兼容性，使得从不同的路径得到的结果相同。
- **使用箭头推理**可以帮助我们从箭头的双射关系中构造同构。

理解同构在编程和数学中非常重要，它帮助我们识别不同对象之间的等价性，以及在不改变系统行为的情况下替换组件的能力。

---

**提示**：本章涉及的概念可能比较抽象，建议读者多画图、多思考，结合具体的例子来加深理解。

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
