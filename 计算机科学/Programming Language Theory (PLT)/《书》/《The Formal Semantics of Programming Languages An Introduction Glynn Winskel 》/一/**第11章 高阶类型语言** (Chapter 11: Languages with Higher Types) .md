[toc]



# 第11章 具有高阶类型的语言

在本章中，我们将探讨具有高阶类型的语言的操作语义和指称语义。所谓高阶类型，是指允许使用函数空间构造器来显式地构造类型的语言；函数成为“第一类”值，可以作为函数的输入或输出。这使得函数可以像其他数据类型一样被处理。

同样地，我们将面临选择：评估（evaluation）应当以值调用（call-by-value）还是名调用（call-by-name）的方式进行。第一种选择（值调用）将导致一种行为类似于急切（eager）语言 Standard ML 的语言；第二种选择（名调用）则会导致一种行为类似于惰性（lazy）语言，如 Miranda、Orwell 或 Haskell。这开启了对这些函数式编程语言的语义研究。

作为语义的一个应用，我们将研究如何在急切和惰性情况下表达不动点算子（fixed-point operators）。这引出了指称语义相对于操作语义的充分性（adequacy）以及全抽象性（full abstraction）的概念。

本章主要考虑的类型构造是乘积类型（products）和函数空间（function space），虽然本章最后会指明如何将其结果扩展到包含和类型（sums）。

---

## 11.1 急切语言

在函数式编程的背景下，值调用的求值方式通常被称为 **急切（eager）**。为了提高效率，名调用的求值方式被以 **需要调用（call-by-need）** 或 **惰性（lazy）** 的方式实现；通过谨慎地共享，实现在需要时才对参数进行求值，并且最多只求值一次。

我们选择值调用（急切）或名调用（惰性）求值模式，将会在某种程度上影响我们的语言语法，特别是在我们允许递归定义的方式上。我们从研究值调用开始。

与第9章的语言 REC 类似，我们将拥有一些求值为基本可打印值（如数字）的项。这些项可以通过数字、变量、条件语句和算术操作来构建，并将产生数字作为值，或者发生发散（diverge）。然而，除此之外，还会有一些项，其值可能是对（pair），甚至是函数（function）。（我们很快会看到如何在操作上理解一个计算以函数作为值的情况。）

为了考虑项可以求值为不同类型的值，我们在编程语言中引入了 **类型（types）**。一个项，如果它不发散，则求值为一个数字，我们将其赋予类型 $\text{int}$。一个项，如果它求值为一个对（pair），则具有形如 $τ_1 * τ_2$ 的乘积类型。一个项，如果它求值为一个函数，则具有形如 $τ_1 \rightarrow τ_2$ 的函数类型。

**总结**：类型表达式 $τ$ 的形式如下：

$$
τ ::= \text{int} \mid τ_1 * τ_2 \mid τ_1 \rightarrow τ_2
$$

为了简化语言，我们假设变量 $x, y, \dots$ 在变量集合 $\text{Var}$ 中，并且每个变量都关联了一个唯一的类型，记为 $\text{type}(x)$。（实际上，这可以通过将类型 $τ$ 构建到变量名中来实现，因此变量 $x$ 的形式为 $x : τ$。）

项 $t, t_0, t_1, \dots$ 的语法定义如下：

$$
\begin{align*}
t ::= \ & x \\
& \mid n \\
& \mid t_1 + t_2 \mid t_1 - t_2 \mid t_1 \times t_2 \\
& \mid \text{if } t_0 \ \text{then } t_1 \ \text{else } t_2 \\
& \mid (t_1, t_2) \mid \text{fst}(t) \mid \text{snd}(t) \\
& \mid \lambda x. t \mid (t_1 \ t_2) \\
& \mid \text{let } x \Leftarrow t_1 \ \text{in } t_2 \\
& \mid \text{rec } y. (\lambda x. t)
\end{align*}
$$

**解释**：

- **算术表达式**：我们可以像在第9章的语言 REC 中那样书写算术表达式。与 REC 一样，条件语句根据一个算术（而非布尔）项进行分支。然而，与 REC 不同的是，分支的结果不一定是数字。

- **构造对（pairs）**：使用 $(t_1, t_2)$ 构造对，使用 $\text{fst}(t)$ 和 $\text{snd}(t)$ 来投影出第一个和第二个元素。

- **定义函数**：使用 λ 抽象 $\lambda x. t$ 来定义函数，应用函数 $(t_1 \ t_2)$ 表示将函数 $t_1$ 应用于 $t_2$。

- **强制求值**：使用 $\text{let } x \Leftarrow t_1 \ \text{in } t_2$ 强制在计算 $t_2$ 前先求值 $t_1$。

- **递归定义**：使用 $\text{rec } y. (\lambda x. t)$ 来递归地定义一个函数 $y$，其中 $t$ 可以涉及 $y$。注意，在这个急切语言中，任何递归定义都必须具有函数类型，即如果 $\text{rec } y. (\lambda x. t) : τ$，那么 $τ = τ_1 \rightarrow τ_2$，其中 $τ_1, τ_2$ 是类型。

通过这个语法的选择，我们的处理方式与 Standard ML 保持一致。

我们可以书写类似于在 REC 中看到的算术项。然而，也有可能书写一些无意义的项：例如尝试将两个函数相加，或者给一个函数太多或太少的参数。

**良构项**：被赋予类型 $τ$ 的项 $t$ 称为 **良构的（well-formed）**，记为 $t : τ$。

**类型规则**：

一个项 $t$ 是可类型的（typable），如果对于某个类型 $τ$，有 $t : τ$，并且根据以下规则：

### 类型规则

1. **变量**：

   - 如果 $\text{type}(x) = τ$，则 $x : τ$。

2. **常量**：

   - $n : \text{int}$，其中 $n$ 是一个整数。

3. **算术运算**：

   - 如果 $t_1 : \text{int}$，$t_2 : \text{int}$，并且运算符 $\text{op}$ 是 $+, -, \times$，则 $t_1 \ \text{op} \ t_2 : \text{int}$。

4. **条件语句**：

   - 如果 $t_0 : \text{int}$，$t_1 : τ$，$t_2 : τ$，则 $\text{if } t_0 \ \text{then } t_1 \ \text{else } t_2 : τ$。

5. **构造对**：

   - 如果 $t_1 : τ_1$，$t_2 : τ_2$，则 $(t_1, t_2) : τ_1 * τ_2$。

6. **投影**：

   - 如果 $t : τ_1 * τ_2$，则 $\text{fst}(t) : τ_1$，$\text{snd}(t) : τ_2$。

7. **函数抽象**：

   - 如果在上下文中 $x : τ_1$，并且 $t : τ_2$，则 $\lambda x. t : τ_1 \rightarrow τ_2$。

8. **函数应用**：

   - 如果 $t_1 : τ_1 \rightarrow τ_2$，$t_2 : τ_1$，则 $(t_1 \ t_2) : τ_2$。

9. **let 绑定**：

   - 如果 $t_1 : τ_1$，并且在上下文中 $x : τ_1$，$t_2 : τ_2$，则 $\text{let } x \Leftarrow t_1 \ \text{in } t_2 : τ_2$。

10. **递归函数定义**：

    - 如果在上下文中 $y : τ$，$\lambda x. t : τ$，则 $\text{rec } y. (\lambda x. t) : τ$。

---

### 练习11.1

**问题**：

称一个项 $t$ 是 **唯一类型化的（uniquely typed）**，如果 $t : τ$ 且 $t : τ'$，则 $τ$ 和 $τ'$ 是相同的类型。

证明所有可类型的项都具有唯一的类型。

**解答**：

**思路**：

- 我们需要证明：如果一个项 $t$ 可以被赋予两个类型 $τ$ 和 $τ'$，那么 $τ = τ'$。
- 我们可以通过对项 $t$ 的结构进行归纳来证明这一点。

**证明**：

我们对项 $t$ 的结构进行归纳。

- **基础情况**：

  - **变量**：如果 $x : τ$ 且 $x : τ'$，那么根据类型规则，$\text{type}(x) = τ$ 且 $\text{type}(x) = τ'$，因此 $τ = τ'$。
  
  - **整数常量**：$n : \text{int}$，类型唯一。

- **归纳步骤**：

  - **算术运算**：如果 $t = t_1 \ \text{op} \ t_2$，且 $t : τ$，根据类型规则，$τ = \text{int}$。因此，如果 $t : τ'$，则 $τ' = \text{int}$，所以 $τ = τ'$。
  
  - **条件语句**：如果 $t = \text{if } t_0 \ \text{then } t_1 \ \text{else } t_2$，且 $t : τ$。根据类型规则，$t_1 : τ$，$t_2 : τ$。根据归纳假设，$t_1$ 和 $t_2$ 的类型唯一，因此 $τ$ 唯一。

  - **对的构造和投影**：类似地，可以证明 $(t_1, t_2)$，$\text{fst}(t)$，$\text{snd}(t)$ 的类型唯一。

  - **函数抽象和应用**：对于 $\lambda x. t$ 和 $(t_1 \ t_2)$，类型也由其子项的类型唯一确定。

  - **let 绑定**：类型由 $t_1$ 和 $t_2$ 的类型唯一确定。

  - **递归定义**：$\text{rec } y. (\lambda x. t)$ 的类型由 $\lambda x. t$ 的类型唯一确定。

**结论**：

- 因此，所有可类型的项都具有唯一的类型。

---

**自由变量的定义**：

项 $t$ 的自由变量集合 $\text{FV}(t)$ 可以通过对 $t$ 的结构归纳来定义：

- $\text{FV}(n) = \emptyset$

- $\text{FV}(x) = \{ x \}$

- $\text{FV}(t_1 \ \text{op} \ t_2) = \text{FV}(t_1) \cup \text{FV}(t_2)$

- $\text{FV}(\text{if } t_0 \ \text{then } t_1 \ \text{else } t_2) = \text{FV}(t_0) \cup \text{FV}(t_1) \cup \text{FV}(t_2)$

- $\text{FV}((t_1, t_2)) = \text{FV}(t_1) \cup \text{FV}(t_2)$

- $\text{FV}(\text{fst}(t)) = \text{FV}(t)$

- $\text{FV}(\text{snd}(t)) = \text{FV}(t)$

- $\text{FV}(\lambda x. t) = \text{FV}(t) \setminus \{ x \}$

- $\text{FV}((t_1 \ t_2)) = \text{FV}(t_1) \cup \text{FV}(t_2)$

- $\text{FV}(\text{let } x \Leftarrow t_1 \ \text{in } t_2) = \text{FV}(t_1) \cup (\text{FV}(t_2) \setminus \{ x \})$

- $\text{FV}(\text{rec } y. (\lambda x. t)) = (\text{FV}(\lambda x. t)) \setminus \{ y \} = (\text{FV}(t) \setminus \{ x \}) \setminus \{ y \}$

**说明**：

- 在 $\lambda x. t$ 中，$x$ 在 $t$ 中被绑定，因此从 $\text{FV}(t)$ 中移除 $x$。
- 在 $\text{let } x \Leftarrow t_1 \ \text{in } t_2$ 中，$x$ 在 $t_2$ 中被绑定，因此从 $\text{FV}(t_2)$ 中移除 $x$。
- 在 $\text{rec } y. (\lambda x. t)$ 中，$y$ 在整个项中被绑定，因此从 $\text{FV}(t)$ 中移除 $y$。

**闭项**：如果项 $t$ 的自由变量集合为空，即 $\text{FV}(t) = \emptyset$，则称 $t$ 为 **闭项（closed term）**。

---

操作语义在某些情况下需要我们将一个闭项 $s$ 代入到项 $t$ 中的自由变量 $x$。我们用 $t[s/x]$ 表示这样的替换。读者应该可以轻松地形式化替换的定义。更一般地，我们用 $t[s_1 / x_1, \dots, s_k / x_k]$ 表示在 $t$ 中同时将闭项 $s_1$ 替换 $x_1$，…，$s_k$ 替换 $x_k$，假设 $x_1, \dots, x_k$ 是不同的。

---

## 11.2 急切的操作语义

到目前为止，我们只是在非正式地解释编程语言的预期行为。我们考虑一种 **值调用（急切）** 的求值方法。就像在 REC 的情况中一样，这意味着为了求值一个函数应用于某些参数，我们应该首先对参数进行求值，以获得函数可以作用的值。

但在这个更一般的语言中，**什么是值（values）**？当然，我们期望数字是值，但在函数被应用于函数作为参数的情况下，我们何时停止对这些参数函数进行求值，并将其视为已产生了一个函数值？

这里有一个选择，但一个合理的决定是，当一个项是一个 λ 抽象时，将其视为表示一个函数值。更一般地，对于每种类型，我们可以询问哪些项表示值。传统上，**这样的项被称为** **规范形式（canonical forms）**。

**判断**：$t \in C_{τ}$ 表示项 $t$ 是类型 $τ$ 的一个规范形式。

**规范形式的定义**（根据类型 $τ$ 进行结构归纳）：

- **基本类型**（Ground type）：

  - 数字是规范形式，即对于 $n$，$n \in C_{\text{int}}$。

- **乘积类型**：

  - 如果 $c_1 \in C_{τ_1}$ 且 $c_2 \in C_{τ_2}$，则 $(c_1, c_2) \in C_{τ_1 * τ_2}$。

- **函数类型**：

  - **闭的** λ 抽象是规范形式，即如果 $\lambda x. t : τ_1 \rightarrow τ_2$ 且 $\lambda x. t$ 是闭项，则 $\lambda x. t \in C_{τ_1 \rightarrow τ_2}$。

**注意**：规范形式是特殊类型的闭项。

---

现在我们可以给出评估关系的规则，其形式为：

$$
t \ \longrightarrow_e \ c
$$

其中，$t$ 是一个可类型的闭项，$c$ 是一个规范形式，表示 $t$ 评估为 $c$。

### 评估规则

1. **规范形式**：

   $$
   \frac{}{c \ \longrightarrow_e \ c} \quad \text{其中 } c \in C_{τ}
   $$

   **解释**：规范形式评估为自身。

2. **算术运算**：

   类似于 REC 中的规则，这里省略。

3. **条件语句**：

   类似于 REC 中的规则，这里省略。

4. **构造对**：

   $$
   \frac{t_1 \ \longrightarrow_e \ c_1 \quad t_2 \ \longrightarrow_e \ c_2}{(t_1, t_2) \ \longrightarrow_e \ (c_1, c_2)}
   $$

5. **投影**：

   $$
   \frac{t \ \longrightarrow_e \ (c_1, c_2)}{\text{fst}(t) \ \longrightarrow_e \ c_1}
   $$
   
   $$
   \frac{t \ \longrightarrow_e \ (c_1, c_2)}{\text{snd}(t) \ \longrightarrow_e \ c_2}
   $$

6. **函数应用**：

   $$
   \frac{t_1 \ \longrightarrow_e \ \lambda x. t' \quad t_2 \ \longrightarrow_e \ c_2 \quad t'[c_2 / x] \ \longrightarrow_e \ c}{(t_1 \ t_2) \ \longrightarrow_e \ c}
   $$

   **解释**：当函数部分和参数都已评估为值时，可以对函数体进行替换，并继续评估。

7. **let 绑定**：

   $$
   \frac{t_1 \ \longrightarrow_e \ c_1 \quad t_2[c_1 / x] \ \longrightarrow_e \ c_2}{\text{let } x \Leftarrow t_1 \ \text{in } t_2 \ \longrightarrow_e \ c_2}
   $$

   **解释**：首先评估 $t_1$，然后将结果 $c_1$ 替换到 $t_2$ 中，继续评估。

8. **递归定义**：

   $$
   \frac{}{\text{rec } y. (\lambda x. t) \ \longrightarrow_e \ \lambda x. (t[\text{rec } y. (\lambda x. t) / y])}
   $$

   **解释**：递归定义展开一次，得到一个 λ 抽象，其函数体中 $y$ 被替换为整个递归定义。

---

**说明**：

- 评估规则中的 $\longrightarrow_e$ 表示急切的评估过程。

- **关键规则**是函数应用和递归定义的规则，它们规定了何时以及如何展开函数应用和递归函数。

---

**性质**：

- **命题11.2**：

  - **确定性**：如果 $t \ \longrightarrow_e \ c$ 且 $t \ \longrightarrow_e \ c'$，则 $c = c'$。

  - **类型保留**：如果 $t \ \longrightarrow_e \ c$ 且 $t : τ$，则 $c : τ$。

**证明**：

- **确定性**的证明：

  - 通过对评估规则的归纳，可以证明评估是确定的，即对于给定的 $t$，评估结果 $c$ 唯一。

- **类型保留**的证明：

  - 通过对评估过程的归纳，利用类型规则，可以证明如果 $t : τ$，则评估结果 $c : τ$。

---

### 练习11.3

**问题**：

设

$$
\text{fact} = \text{rec } f. (\lambda x. \text{if } x \ \text{then } 1 \ \text{else } x \times f(x - 1))
$$

从操作语义推导 $\text{fact } 2$ 的评估过程。

**解答**：

**思路**：

- 我们需要根据评估规则，逐步推导 $\text{fact } 2$ 的评估过程。

- 需要展开递归定义，并按照评估规则处理函数应用和算术运算。

**推导**：

1. **评估 $\text{fact } 2$**：

   $$
   \begin{align*}
   \text{fact } 2 &\longrightarrow_e (\text{rec } f. (\lambda x. \text{if } x \ \text{then } 1 \ \text{else } x \times f(x - 1))) \ 2 \\
   &\longrightarrow_e (\lambda x. (\text{if } x \ \text{then } 1 \ \text{else } x \times f(x - 1)))[\text{rec } f. (\lambda x. \text{if } x \ \text{then } 1 \ \text{else } x \times f(x - 1))/f] \ 2 \\
   &\longrightarrow_e (\lambda x. (\text{if } x \ \text{then } 1 \ \text{else } x \times (\text{rec } f. (\lambda x. \dots))(x - 1))) \ 2
   \end{align*}
   $$

2. **应用函数**：

   $$
   \begin{align*}
   (\lambda x. \dots) \ 2 \longrightarrow_e \text{if } 2 \ \text{then } 1 \ \text{else } 2 \times (\text{rec } f. (\lambda x. \dots))(2 - 1)
   \end{align*}
   $$

3. **计算条件**：

   - 因为 $2$ 不等于 $0$，所以执行 $\text{else}$ 分支：

     $$
     2 \times (\text{rec } f. (\lambda x. \dots))(1)
     $$

4. **评估递归调用**：

   - 计算 $(\text{rec } f. (\lambda x. \dots))(1)$，重复上述步骤：

     $$
     (\text{rec } f. (\lambda x. \dots))(1) \longrightarrow_e (\lambda x. (\text{if } x \ \text{then } 1 \ \text{else } x \times f(x - 1)))[\text{rec } f. (\lambda x. \dots)/f] \ 1
     $$

   - 应用函数：

     $$
     \text{if } 1 \ \text{then } 1 \ \text{else } 1 \times (\text{rec } f. (\lambda x. \dots))(1 - 1)
     $$

   - 计算条件：

     - 因为 $1$ 不等于 $0$，执行 $\text{else}$ 分支：

       $$
       1 \times (\text{rec } f. (\lambda x. \dots))(0)
       $$

5. **继续递归调用**：

   - 计算 $(\text{rec } f. (\lambda x. \dots))(0)$：

     $$
     \text{if } 0 \ \text{then } 1 \ \text{else } 0 \times (\text{rec } f. (\lambda x. \dots))(-1)
     $$

   - 计算条件：

     - 因为 $0$ 等于 $0$，执行 $\text{then}$ 分支，得到 $1$。

6. **回溯计算**：

   - 从第5步回溯，得到：

     $$
     (\text{rec } f. (\lambda x. \dots))(0) \longrightarrow_e 1
     $$

   - 因此，

     $$
     (\text{rec } f. (\lambda x. \dots))(1) \longrightarrow_e 1 \times 1 = 1
     $$

   - 然后，

     $$
     \text{fact } 2 \longrightarrow_e 2 \times 1 = 2
     $$

**结论**：

- $\text{fact } 2$ 的评估结果是 $2$。

---

## 11.3 急切的指称语义

指称语义将展示如何将类型 $τ_1 \rightarrow τ_2$ 的项理解为函数，从而证明在函数式语言中编程的非正式理解是合理的。通过在 cpo（完备偏序集）和连续函数的框架中解释语言，编程语言将变得可以应用第10章的证明技术。

**首先，我们需要决定如何解释类型表达式。**

一个类型为 $τ$ 的闭项 $t$，可以评估为类型 $τ$ 的一个规范形式，或者发散。因此，合理的做法是将 $t$ 指称为 $(V_{τ})_{\bot}$ 中的一个元素，其中 $V_{τ}$ 是类型 $τ$ 的值的 cpo，应当包含规范形式的指称。

**基于此指导思想**，我们通过对类型表达式进行结构归纳，定义：

- 对于基本类型 $\text{int}$：

  $$
  V_{\text{int}} = N
  $$

  **解释**：$V_{\text{int}}$ 是自然数的集合 $N$。

- 对于乘积类型 $τ_1 * τ_2$：

  $$
  V_{τ_1 * τ_2} = V_{τ_1} \times V_{τ_2}
  $$

- 对于函数类型 $τ_1 \rightarrow τ_2$：

  $$
  V_{τ_1 \rightarrow τ_2} = [V_{τ_1} \rightarrow (V_{τ_2})_{\bot}]
  $$

  **解释**：$V_{τ_1 \rightarrow τ_2}$ 是从 $V_{τ_1}$ 到 $(V_{τ_2})_{\bot}$ 的连续函数的集合。

**最后一条**捕捉了这样一个想法：一个函数值接受一个值作为输入，返回一个值作为输出，或者发散。

---

**环境（environment）**：

一般来说，项包含自由变量。然后，指称语义需要一种环境的概念，为自由变量提供值。对于这个急切语言，一个环境通常是一个函数：

$$
\rho : \text{Var} \rightarrow \bigcup_{\tau \text{ 是类型}} V_{\tau}
$$

并且满足类型一致性：

- 如果 $x : τ$，则 $\rho(x) \in V_{τ}$。

记 $\text{Env}_e$ 为所有这样的环境的 cpo。

---

**指称语义**：

一个类型为 $τ$ 的项 $t$，在环境 $\rho$ 下的指称为 $[t]_e^{\rho} \in (V_{τ})_{\bot}$。

**指称语义的定义**（对可类型的项 $t$ 进行结构归纳）：

1. **变量**：

   $$
   [x]_e^{\rho} = \lambda \rho. \rho(x)
   $$

2. **整数常量**：

   $$
   [n]_e^{\rho} = \lambda \rho. \underline{n}
   $$

   **解释**：$\underline{n}$ 是自然数 $n$。

3. **算术运算**（以加法为例）：

   $$
   [t_1 + t_2]_e = \lambda \rho. ([t_1]_e^{\rho} \oplus_{\bot} [t_2]_e^{\rho})
   $$

   其中，$\oplus_{\bot}$ 是在 $N_{\bot}$ 上定义的加法，处理未定义值 $\bot$。

4. **条件语句**：

   $$
   [\text{if } t_0 \ \text{then } t_1 \ \text{else } t_2]_e = \lambda \rho. \text{Cond}([t_0]_e^{\rho}, [t_1]_e^{\rho}, [t_2]_e^{\rho})
   $$

   其中，$\text{Cond}$ 是条件函数，在 $N_{\bot}$ 和 $D$ 上定义（$D$ 是一个 cpo），满足：

   $$
   \text{Cond}(z_0, z_1, z_2) =
   \begin{cases}
   z_1, & \text{如果 } z_0 = \underline{0} \\
   z_2, & \text{如果 } z_0 = \underline{n} \text{ 且 } n \ne 0 \\
   \bot, & \text{如果 } z_0 = \bot
   \end{cases}
   $$

5. **构造对**：

   $$
   [(t_1, t_2)]_e = \lambda \rho. \text{let } v_1 \Leftarrow [t_1]_e^{\rho}, v_2 \Leftarrow [t_2]_e^{\rho}. \underline{(v_1, v_2)}
   $$

6. **投影**：

   - **第一投影**：

     $$
     [\text{fst}(t)]_e = \lambda \rho. \text{let } v \Leftarrow [t]_e^{\rho}. \pi_1(v)
     $$

     其中，$\pi_1$ 是对的第一投影函数。

   - **第二投影**：

     $$
     [\text{snd}(t)]_e = \lambda \rho. \text{let } v \Leftarrow [t]_e^{\rho}. \pi_2(v)
     $$

7. **函数抽象**：

   $$
   [\lambda x. t]_e = \lambda \rho. \lambda v \in V_{τ_1}. [t]_e^{\rho[v/x]}
   $$

   **解释**：函数抽象的指称是一个函数，从 $V_{τ_1}$ 映射到 $(V_{τ_2})_{\bot}$。

8. **函数应用**：

   $$
   [(t_1 \ t_2)]_e = \lambda \rho. \text{let } \varphi \Leftarrow [t_1]_e^{\rho}, v \Leftarrow [t_2]_e^{\rho}. \varphi(v)
   $$

9. **let 绑定**：

   $$
   [\text{let } x \Leftarrow t_1 \ \text{in } t_2]_e = \lambda \rho. \text{let } v \Leftarrow [t_1]_e^{\rho}. [t_2]_e^{\rho[v/x]}
   $$

10. **递归定义**：

    $$
    [\text{rec } y. (\lambda x. t)]_e = \lambda \rho. \mu \varphi. (\lambda v. [t]_e^{\rho[v/x, \varphi/y]})
    $$

    **解释**：递归定义的指称是一个最小不动点。

---

**注释**：

- 在 $\text{let } v \Leftarrow e. e'$ 中，如果 $e = \bot$，则整个表达式的值为 $\bot$。

- 在递归定义中，$\mu \varphi$ 表示最小不动点。

---

### 引理11.5

**陈述**：

设 $t$ 是一个可类型的项。设 $\rho, \rho'$ 是在 $t$ 的自由变量上相同的环境。那么对于 $[t]_e^{\rho} = [t]_e^{\rho'}$。

**证明**：

- **思路**：对 $t$ 进行结构归纳，证明在 $t$ 的自由变量上相同的环境对 $[t]_e^{\rho}$ 的值没有影响。

- **证明**：留给读者完成（很简单的结构归纳）。

---

### 引理11.6（替换引理）

**陈述**：

设 $s$ 是一个闭项，$s : τ$，并且 $[s]_e^{\rho} = v$。设 $x$ 是类型为 $τ$ 的变量。假设 $t : τ'$。则 $t[s/x] : τ'$，并且 $[t[s/x]]_e^{\rho} = [t]_e^{\rho[v/x]}$。

**证明**：

- **思路**：对 $t$ 进行结构归纳，证明替换后的项的指称等于在环境中用 $v$ 替换 $x$ 后的指称。

- **证明**：略，读者可以自己完成。

---

### 引理11.8

**陈述**：

(i) 如果 $t : τ$，则对于任意环境 $\rho$，$[t]_e^{\rho} \in (V_{τ})_{\bot}$。

(ii) 如果 $c \in C_{τ}$，则对于任意环境 $\rho$，$[c]_e^{\rho} \ne \bot$。

**证明**：

- **(i)**：通过对 $t$ 进行结构归纳，证明 $[t]_e^{\rho}$ 属于 $(V_{τ})_{\bot}$。

- **(ii)**：通过对规范形式 $c$ 进行结构归纳，证明 $[c]_e^{\rho}$ 不等于 $\bot$。

---

### 练习11.9

**问题**：

证明引理11.8的部分 (ii)。

**解答**：

**思路**：

- 对规范形式 $c$ 进行结构归纳，证明对于任意环境 $\rho$，$[c]_e^{\rho} \ne \bot$。

**证明**：

- **基础情况**：

  - **数字**：如果 $c = n$，则 $[n]_e^{\rho} = \underline{n} \ne \bot$。

- **归纳步骤**：

  - **乘积类型**：如果 $c = (c_1, c_2)$，并且已知 $[c_1]_e^{\rho} \ne \bot$，$[c_2]_e^{\rho} \ne \bot$，则

    $$
    [c]_e^{\rho} = \underline{([c_1]_e^{\rho}, [c_2]_e^{\rho})} \ne \bot
    $$

  - **函数类型**：如果 $c = \lambda x. t$，并且 $c$ 是闭项，则

    $$
    [c]_e^{\rho} = \lambda v. [t]_e^{\rho[v/x]}
    $$

    由于 $t$ 是闭项或仅依赖于 $x$，且评估不会发散，因此 $[c]_e^{\rho} \ne \bot$。

**结论**：

- 因此，对于任意规范形式 $c$，$[c]_e^{\rho} \ne \bot$。

---

**总结**：

通过定义急切语言的指称语义，我们可以将程序项解释为 cpo 和连续函数的元素，这为进一步的语义分析和证明奠定了基础。

### ---------------------------

## 11.4 急切语义的一致性

**目标**：

我们想要了解操作语义和指称语义是否一致。我们将看到它们确实一致，但可能并不像最初预期的那样程度深。

**背景**：

之前，操作语义和指称语义非常紧密地匹配，这可能导致我们错误地期望，对于闭项 $t$ 和规范形式 $c$，有：

$$
t \ \longrightarrow_e \ c \quad \iff \quad [t]_e^\rho = [c]_e^\rho
$$

但这种双向的等价在涉及函数空间的类型时并不成立。原因是可能存在多个具有相同指称的规范形式，而一个项的评估至多只能产生其中的一个（见下面的练习）。

然而，我们可以证明如下单向的等价关系对任意类型的 $t$ 都成立：

1. **如果 $t$ 评估到规范形式 $c$，则 $t$ 的指称等于 $c$ 的指称**：

   $$
   t \ \longrightarrow_e \ c \quad \implies \quad [t]_e^\rho = [c]_e^\rho
   $$

2. **此外，操作语义和指称语义在项是否收敛（converge）方面是一致的**。

**定义**：

- **操作收敛**：对于一个可类型的闭项 $t$，根据评估规则，$t$ 要么发散，要么产生一个规范形式。定义 $t$ 的操作收敛为：

  $$
  t \ \Downarrow_e \quad \text{当且仅当存在 } c \text{，使得 } t \ \longrightarrow_e \ c
  $$

- **指称收敛**：$t$ 的指称为 $[t]_e^\rho$，它是 $(V_\tau)_\bot$ 的一个元素，其中 $\tau$ 是 $t$ 的类型，$\rho$ 是任意环境（因为 $t$ 是闭项）。$[t]_e^\rho$ 要么是 $\bot$（如果 $t$ 发散），要么是某个值 $v$（如果 $t$ 收敛）。定义 $t$ 的指称收敛为：

  $$
  t \ \Downarrow_d \quad \text{当且仅当 } [t]_e^\rho \ne \bot
  $$

**结论**：

- **收敛性一致性**：

  $$
  t \ \Downarrow_d \quad \iff \quad t \ \Downarrow_e
  $$

- **对于类型为 $\text{int}$ 的闭项 $t$，有以下等价**：

  $$
  t \ \longrightarrow_e \ n \quad \iff \quad [t]_e^\rho = n
  $$

**解释**：

- **充分性（adequacy）**：上面的结果表明了指称语义相对于操作语义的充分性。这些结果证明了我们可以从指称语义推断操作语义中评估的结果。

---

### 练习11.10

**问题**：

证明对于一般的类型，上述等价的逆命题：

$$
[t]_e^\rho = [c]_e^\rho \quad \implies \quad t \ \longrightarrow_e \ c
$$

并不成立。

**提示**：取 $t = \lambda x. x$，$c = \lambda x. x + 0$，其中 $x : \text{int}$。

**解答**：

- **思路**：

  - 我们需要找到一个闭项 $t$ 和一个规范形式 $c$，使得 $[t]_e^\rho = [c]_e^\rho$，但 $t$ 不评估到 $c$。

- **具体分析**：

  - 设 $t = \lambda x. x$，$c = \lambda x. x + 0$，其中 $x : \text{int}$。

  - $t$ 和 $c$ 都是类型为 $\text{int} \rightarrow \text{int}$ 的闭项。

  - 计算它们的指称：

    - $[t]_e^\rho = \lambda v. v$

    - $[c]_e^\rho = \lambda v. v + 0$

    - 由于在自然数上，加 $0$ 是恒等操作，因此对于所有 $v \in N$，$v = v + 0$。

    - 因此，$[t]_e^\rho = [c]_e^\rho$

  - 但是，$t$ 和 $c$ 作为项是不同的，$t$ 不会评估到 $c$。

- **结论**：

  - 因此，逆命题不成立。

---

现在，我们将证明前面的结论，即**引理11.11**。

### 引理11.11

**陈述**：

如果 $t \ \longrightarrow_e \ c$，则对于任意环境 $\rho$，有 $[t]_e^\rho = [c]_e^\rho$。

**证明**：

我们将对评估规则进行归纳，证明在每个规则下，结论成立。

**解释**：

- **我们的目标**是对于每个评估规则，假设前提成立，证明结论成立，即 $[t]_e^\rho = [c]_e^\rho$。

- **关键规则**是函数应用和递归定义的规则。

**具体证明**：

1. **规范形式**：

   - 规则：

     $$
     \frac{}{c \ \longrightarrow_e \ c}
     $$

   - 因此，$[c]_e^\rho = [c]_e^\rho$，显然成立。

2. **投影**（以 $\text{fst}$ 为例）：

   - 规则：

     $$
     \frac{t \ \longrightarrow_e \ (c_1, c_2)}{\text{fst}(t) \ \longrightarrow_e \ c_1}
     $$

   - **假设**：$[t]_e^\rho = [(c_1, c_2)]_e^\rho$

   - **根据指称语义**：

     $$
     [t]_e^\rho = [(c_1, c_2)]_e^\rho = \underline{(v_1, v_2)}
     $$

     其中，$[c_1]_e^\rho = v_1$，$[c_2]_e^\rho = v_2$

   - **计算**：

     $$
     [\text{fst}(t)]_e^\rho = \text{let } v \Leftarrow [t]_e^\rho.\ \pi_1(v) = \pi_1((v_1, v_2)) = v_1 = [c_1]_e^\rho
     $$

   - **结论**：$[\text{fst}(t)]_e^\rho = [c_1]_e^\rho$

3. **函数应用**：

   - 规则：

     $$
     \frac{t_1 \ \longrightarrow_e \ \lambda x. t' \quad t_2 \ \longrightarrow_e \ c_2 \quad t'[c_2 / x] \ \longrightarrow_e \ c}{(t_1\ t_2) \ \longrightarrow_e \ c}
     $$

   - **假设**：

     - $[t_1]_e^\rho = [\lambda x. t']_e^\rho$

     - $[t_2]_e^\rho = [c_2]_e^\rho$

     - $[t'[c_2 / x]]_e^\rho = [c]_e^\rho$

   - **根据指称语义**：

     - $[t_1]_e^\rho = \lambda v. [t']_e^{\rho[v/x]}$

     - $[t_2]_e^\rho = v_2$，其中 $v_2 = [c_2]_e^\rho$

   - **计算**：

     $$
     [(t_1\ t_2)]_e^\rho = \text{let } \varphi \Leftarrow [t_1]_e^\rho,\ v \Leftarrow [t_2]_e^\rho.\ \varphi(v) = \varphi(v_2) = [t']_e^{\rho[v_2 / x]} = [t'[c_2 / x]]_e^\rho = [c]_e^\rho
     $$

   - **结论**：$[(t_1\ t_2)]_e^\rho = [c]_e^\rho$

4. **递归定义**：

   - 规则：

     $$
     \frac{}{\text{rec } y. (\lambda x. t) \ \longrightarrow_e \ \lambda x. (t[\text{rec } y. (\lambda x. t) / y])}
     $$

   - **计算指称**：

     - $[\text{rec } y. (\lambda x. t)]_e^\rho = \mu \varphi. (\lambda v. [t]_e^{\rho[v/x, \varphi / y]})$

   - **展开 $\mu \varphi$**：

     - $\varphi$ 是最小不动点，即 $\varphi = \lambda v. [t]_e^{\rho[v/x, \varphi / y]}$

   - **计算 $[\lambda x. (t[\text{rec } y. (\lambda x. t) / y])]_e^\rho$**：

     - $[\lambda x. (t[\text{rec } y. (\lambda x. t) / y])]_e^\rho = \lambda v. [t[\text{rec } y. (\lambda x. t) / y]]_e^{\rho[v/x]}$

     - 由于 $y$ 和 $x$ 不同，我们可以写成：

       $$
       \lambda v. [t]_e^{\rho[v/x, [\text{rec } y. (\lambda x. t)]_e^\rho / y]} = \lambda v. [t]_e^{\rho[v/x, \varphi / y]}
       $$

     - 因此，$[\text{rec } y. (\lambda x. t)]_e^\rho = [\lambda x. (t[\text{rec } y. (\lambda x. t) / y])]_e^\rho$

   - **结论**：$[\text{rec } y. (\lambda x. t)]_e^\rho = [\lambda x. (t[\text{rec } y. (\lambda x. t) / y])]_e^\rho$

---

**由引理11.8和引理11.11，我们得到**：

如果 $t \ \longrightarrow_e \ c$，则 $[t]_e^\rho = [c]_e^\rho$

---

**接下来，我们想要证明逆向的结论，即如果 $t$ 的指称收敛，则 $t$ 操作收敛**。

然而，直接使用结构归纳法可能遇到困难，因为在处理函数应用时，替换后的项与原项可能没有明显的结构关系。

**解决方法**：

- **引入逻辑关系（logical relations）的方法**。

- **定义**一系列的关系 $ \leq_\tau $，在类型 $\tau$ 上的元素之间建立关系。

---

### 定义：逻辑关系

对于每个类型 $\tau$，定义关系 $ \leq_\tau \subseteq V_\tau \times C_\tau$，其中 $V_\tau$ 是类型 $\tau$ 的值的集合，$C_\tau$ 是类型 $\tau$ 的规范形式的集合。

- **基本类型**（$\text{int}$）：

  - 对于所有的数字 $n$，$n \leq_{\text{int}} n$

- **函数类型**（$\tau_1 \rightarrow \tau_2$）：

  - 对于所有 $\varphi \in V_{\tau_1 \rightarrow \tau_2}$，$c \in C_{\tau_1 \rightarrow \tau_2}$，有：

    $$
    \varphi \leq_{\tau_1 \rightarrow \tau_2} c \quad \iff \quad \forall v \in V_{\tau_1},\ \forall c' \in C_{\tau_1},\ v \leq_{\tau_1} c' \implies \varphi(v) \leq_{\tau_2} c[c'/x]
    $$

    **解释**：函数 $\varphi$ 和规范形式 $c$ 相关，当且仅当对于所有相关的输入，它们的输出也是相关的。

---

**定义扩展到 $ (V_\tau)_\bot $ 和项**：

- 对于 $d \in (V_\tau)_\bot$，$t$ 是类型 $\tau$ 的闭项，定义：

  $$
  d \leq_\tau t \quad \iff \quad d = \bot \quad \text{或} \quad \left( d = v \in V_\tau \quad \text{且存在} c \text{使得 } t \ \longrightarrow_e \ c \text{，并且 } v \leq_\tau c \right)
  $$

---

### 引理11.12

**性质**：

- **(i)** $\bot \leq_\tau t$，对于任何 $t : \tau$。

- **(ii)** 如果 $d \sqsubseteq d'$，且 $d' \leq_\tau t$，则 $d \leq_\tau t$。

- **(iii)** 对于 $ (V_\tau)_\bot $ 中的 $\omega$-链 $d_0 \sqsubseteq d_1 \sqsubseteq \dots$，如果对于所有 $n$，$d_n \leq_\tau t$，则 $\bigvee_{n} d_n \leq_\tau t$。

**证明**：

- **(i)** $\bot \leq_\tau t$，因为按照定义，$d = \bot$ 时，总是有 $d \leq_\tau t$。

- **(ii)** 如果 $d \sqsubseteq d'$，且 $d' \leq_\tau t$，则 $d$ 要么是 $\bot$，要么 $d = v$，且 $v \in V_\tau$。

  - 如果 $d = \bot$，则 $d \leq_\tau t$。

  - 如果 $d = v$，由于 $d \sqsubseteq d'$，且 $d' \geq d$，因此 $d' = v$ 或 $d' = \bot$。

    - 如果 $d' = v$，且 $d' \leq_\tau t$，则 $v \leq_\tau t$，所以 $d \leq_\tau t$。

- **(iii)** 类似地，可以证明，对于 $\omega$-链的上确界，性质成立。

---

### 引理11.14

**陈述**：

对于可类型的闭项 $t$，如果 $[t]_e^\rho \ne \bot$，则 $t \ \Downarrow_e$。

**证明**：

我们将对项 $t$ 进行结构归纳，并**加强归纳假设**：

- **归纳假设**：

  对于任意项 $t$，如果 $[t]_e^\rho = v \ne \bot$，且对于 $t$ 的自由变量 $x_i$，有 $[x_i]_e^\rho = v_i$，且 $v_i \leq_{\tau_i} s_i$，则存在规范形式 $c$，使得 $t[s_1 / x_1, \dots, s_k / x_k] \ \longrightarrow_e \ c$，且 $v \leq_\tau c$。

- **说明**：这样，我们不仅证明了收敛性，还证明了指称值和评估结果之间的关系。

**具体证明步骤**：

1. **变量**：

   - $t = x$

   - $[x]_e^\rho = v$

   - 由于 $[x_i]_e^\rho = v_i$，并且 $v_i \leq_{\tau_i} s_i$，所以 $v \leq_{\tau} s$

   - 因此，$x[s_i / x_i] = s$，所以 $x[s_i / x_i] \ \longrightarrow_e \ s$

2. **数字常量**：

   - 类似地，$n \leq_{\text{int}} n$，因此成立。

3. **算术运算**：

   - 假设 $[t_1 + t_2]_e^\rho = n$

   - 由指称语义，有 $[t_1]_e^\rho = n_1$，$[t_2]_e^\rho = n_2$，且 $n = n_1 + n_2$

   - 由归纳假设，$t_1[s_i / x_i] \ \longrightarrow_e \ n_1$，$t_2[s_i / x_i] \ \longrightarrow_e \ n_2$

   - 因此，$t[s_i / x_i] = t_1[s_i / x_i] + t_2[s_i / x_i] \ \longrightarrow_e \ n$

4. **条件语句**：

   - 类似处理。

5. **函数抽象**：

   - $t = \lambda x. t'$

   - $[t]_e^\rho = \varphi = \lambda v. [t']_e^{\rho[v / x]}$

   - 我们需要证明 $\varphi \leq_{\tau_1 \rightarrow \tau_2} \lambda x. t'[s_i / x_i]$

   - 根据逻辑关系的定义，取任意 $v \leq_{\tau_1} c$，由归纳假设，有 $t'[s_i / x_i, c / x] \ \longrightarrow_e c'$，且 $\varphi(v) \leq_{\tau_2} c'$

6. **函数应用**：

   - $t = t_1\ t_2$

   - $[t]_e^\rho = [t_1]_e^\rho([t_2]_e^\rho) = \varphi(v)$

   - 由归纳假设，$t_1[s_i / x_i] \ \longrightarrow_e c_1$，$t_2[s_i / x_i] \ \longrightarrow_e c_2$，且 $[t_1]_e^\rho \leq_{\tau_1 \rightarrow \tau_2} c_1$，$[t_2]_e^\rho \leq_{\tau_1} c_2$

   - 由逻辑关系，$\varphi(v) \leq_{\tau_2} c$，并且 $t_1[s_i / x_i]\ t_2[s_i / x_i] \ \longrightarrow_e c$

7. **递归定义**：

   - $t = \text{rec } y. (\lambda x. t')$

   - $[t]_e^\rho$ 是 $\varphi$ 的不动点

   - 需要证明 $[t]_e^\rho \leq_{\tau} \text{rec } y. (\lambda x. t'[s_i / x_i])$

   - 使用展开和归纳，类似于之前的证明

---

**最终结论**：

- 通过加强归纳假设和使用逻辑关系的方法，我们证明了如果 $[t]_e^\rho \ne \bot$，则 $t$ 操作收敛，即 $t \ \Downarrow_e$

---

### 推论11.15

**陈述**：

假设 $t$ 是一个类型为 $\text{int}$ 的闭项，那么对于任意 $n \in N$，有：

$$
t \ \longrightarrow_e \ n \quad \iff \quad [t]_e^\rho = n
$$

**证明**：

- 由前面的结论，我们知道，如果 $t \ \longrightarrow_e \ n$，则 $[t]_e^\rho = n$。

- 反之，如果 $[t]_e^\rho = n$，则 $t$ 操作收敛，由于在 $\text{int}$ 类型上，规范形式是数字，因此 $t \ \longrightarrow_e \ n$。

---

## 11.5 一个惰性语言

现在我们考虑一个具有高阶类型的语言，它以 **名调用（call-by-name）** 或 **惰性（lazy）** 的方式求值。我们将再次给出操作语义和指称语义，并建立它们的一致性。

**语法**：

- 语法与急切语言几乎相同，唯一的区别在于递归的语法。

- 现在，递归定义可以采用以下形式：

  $$
  \text{rec } x. t
  $$

  - 与急切语言不同，我们不要求主体 $t$ 是一个抽象。

- **对应的类型规则**：

  $$
  \frac{x : \tau \quad t : \tau}{\text{rec } x. t : \tau}
  $$

  - 这个规则比急切语言对递归定义的要求更宽松。

**自由变量的定义**：

- 与之前相同，但对递归定义有：

  $$
  \text{FV}(\text{rec } x. t) = \text{FV}(t) \setminus \{ x \}
  $$

- 一个没有自由变量的项被称为 **闭项**。

---

## 11.6 惰性的操作语义

**规范形式**：

- 在惰性求值中，基本类型和函数类型的规范形式仍然是数字和抽象。

- 但与急切求值不同的是，乘积类型的规范形式将是 **任意的可类型的闭项对**，其组件不必是规范形式。

- **惰性规范形式 $C_\tau$** 根据类型 $\tau$ 进行归纳定义：

  - **基本类型**：数字 $n \in N$

  - **函数类型**：如果 $\lambda x. t : \tau_1 \rightarrow \tau_2$ 且 $\lambda x. t$ 是闭项，则 $\lambda x. t \in C_{\tau_1 \rightarrow \tau_2}$

**评估关系**：

- 使用关系：

  $$
  t \ \longrightarrow_l \ c
  $$

  - 其中，$t$ 是一个可类型的闭项，$c$ 是一个规范形式。

### 评估规则

1. **规范形式**：

   $$
   \frac{}{c \ \longrightarrow_l \ c} \quad \text{其中 } c \in C_\tau
   $$

2. **算术运算**：

   $$
   \frac{t_1 \ \longrightarrow_l \ n_1 \quad t_2 \ \longrightarrow_l \ n_2}{t_1 \ \text{op} \ t_2 \ \longrightarrow_l \ n_1 \ \text{op} \ n_2}
   $$

3. **条件语句**：

   $$
   \frac{t_0 \ \longrightarrow_l \ n \quad t_2 \ \longrightarrow_l \ c_2 \quad n \ne 0}{\text{if } t_0 \ \text{then } t_1 \ \text{else } t_2 \ \longrightarrow_l \ c_2}
   $$

4. **投影**：

   - 第一投影：

     $$
     \frac{t \ \longrightarrow_l (t_1, t_2) \quad t_1 \ \longrightarrow_l \ c_1}{\text{fst}(t) \ \longrightarrow_l \ c_1}
     $$

   - 第二投影：

     $$
     \frac{t \ \longrightarrow_l (t_1, t_2) \quad t_2 \ \longrightarrow_l \ c_2}{\text{snd}(t) \ \longrightarrow_l \ c_2}
     $$

5. **函数应用**：

   $$
   \frac{t_1 \ \longrightarrow_l \ \lambda x. t' \quad t_1[t_2 / x] \ \longrightarrow_l \ c}{(t_1\ t_2) \ \longrightarrow_l \ c}
   $$

   - **注意**：在惰性求值中，不需要先评估参数 $t_2$。

6. **递归定义**：

   $$
   \frac{t[\text{rec } x. t / x] \ \longrightarrow_l \ c}{\text{rec } x. t \ \longrightarrow_l \ c}
   $$

   - **解释**：在惰性求值中，递归定义需要展开，并将自身替换到主体中。

**性质**：

- **命题11.16**：

  - 如果 $t \ \longrightarrow_l \ c$ 且 $t \ \longrightarrow_l \ c'$，则 $c = c'$。

  - 如果 $t \ \longrightarrow_l \ c$ 且 $t : \tau$，则 $c : \tau$。

- **证明**：

  - 通过对评估规则的归纳，可以证明评估是确定性的，并且类型保留。

---

**总结**：

- 在惰性求值中，函数应用不需要首先评估参数，这是惰性的本质。

- 对于乘积类型，规范形式是任意的可类型的闭项对，其组件不必是规范形式。

- 递归定义的评估规则也不同，需要展开自身并继续评估。

### ----------------------------

## 11.7 惰性的指称语义

**目标**：

在惰性求值（lazy evaluation）中，一个可类型的闭项（closed term）可以惰性地评估为一个规范形式（canonical form），或者发散（diverge）。因此，我们将把它的指称（denotation）视为 $(V_\tau)_\bot$ 中的一个元素，其中 $V_\tau$ 是类型 $\tau$ 的值（values）的 cpo（完全偏序集），包括类型 $\tau$ 的规范形式的指称。

**定义 $V_\tau$**：

我们通过对类型 $\tau$ 进行结构归纳，定义 $V_\tau$：

1. **基本类型**（$\text{int}$）：

   $$
   V_{\text{int}} = \mathbb{N}
   $$

   **解释**：整数类型的值是自然数的集合 $\mathbb{N}$。

2. **乘积类型**（$\tau_1 * \tau_2$）：

   $$
   V_{\tau_1 * \tau_2} = (V_{\tau_1})_\bot \times (V_{\tau_2})_\bot
   $$

   **解释**：乘积类型的值是其组件类型的 lifted cpo 的乘积。也就是说，乘积类型的值可以是组件可能发散的对。

3. **函数类型**（$\tau_1 \rightarrow \tau_2$）：

   $$
   V_{\tau_1 \rightarrow \tau_2} = \left[ (V_{\tau_1})_\bot \rightarrow (V_{\tau_2})_\bot \right]
   $$

   **解释**：函数类型的值是从 $(V_{\tau_1})_\bot$ 到 $(V_{\tau_2})_\bot$ 的连续函数的集合。

**说明**：

- **对于乘积类型**，定义反映了这样一个想法：乘积类型的值是任意的对，即使其组件可能发散。

- **对于函数类型**，定义反映了这样一个想法：函数类型的值只需要被识别为一个函数，实际上是一个不需要先求值其参数的函数。

---

**环境的定义**：

对于惰性语言，一个环境（environment）是一个函数：

$$
\rho : \text{Var} \rightarrow \bigcup_{\tau \text{ 是类型}} (V_\tau)_\bot
$$

满足类型一致性，即对于任何变量 $x$ 和类型 $\tau$，如果 $x : \tau$，则 $\rho(x) \in (V_\tau)_\bot$。

记 $\text{Env}_l$ 为所有这样的惰性环境的 cpo。

---

**指称语义的定义**：

现在，我们给出惰性语言的指称语义。类型为 $\tau$ 的项 $t$ 的指称是从环境 $\text{Env}_l$ 到 cpo $(V_\tau)_\bot$ 的函数。

可类型的项 $t$ 的指称通过结构归纳给出，仍然保持在第8.4节的元语言（metalanguage）范围内。

### 指称语义规则

1. **变量**：

   $$
   [x]_l^\rho = \rho(x)
   $$

   **解释**：变量 $x$ 的指称是在环境 $\rho$ 中查找 $x$ 的值。

2. **整数常量**：

   $$
   [n]_l^\rho = \underline{n}
   $$

   **解释**：整数常量 $n$ 的指称是自然数 $n$ 本身。

3. **算术运算**（以 $+$ 为例）：

   $$
   [t_1 + t_2]_l^\rho = [t_1]_l^\rho \oplus_\bot [t_2]_l^\rho
   $$

   其中，$\oplus_\bot$ 是 lifted 算术操作，在处理 $\bot$ 时遵循以下规则：

   - 如果任一操作数为 $\bot$，结果为 $\bot$。

   - 否则，执行正常的算术运算。

4. **条件语句**：

   $$
   [\text{if } t_0 \ \text{then } t_1 \ \text{else } t_2]_l^\rho = \text{Cond}([t_0]_l^\rho, [t_1]_l^\rho, [t_2]_l^\rho)
   $$

   其中，$\text{Cond}$ 是条件函数，定义如下：

   $$
   \text{Cond}(z_0, z_1, z_2) = 
   \begin{cases}
   z_1, & \text{如果 } z_0 = \underline{0} \\
   z_2, & \text{如果 } z_0 = \underline{n}, n \ne 0 \\
   \bot, & \text{如果 } z_0 = \bot
   \end{cases}
   $$

5. **构造对**：

   $$
   [(t_1, t_2)]_l^\rho = ([t_1]_l^\rho, [t_2]_l^\rho)
   $$

   **解释**：对的指称是其组件的指称组成的有序对。

6. **投影**：

   - **第一投影**：

     $$
     [\text{fst}(t)]_l^\rho = \text{let } v \leftarrow [t]_l^\rho.\ \pi_1(v)
     $$

     其中，$\pi_1$ 是对的第一投影函数。

   - **第二投影**：

     $$
     [\text{snd}(t)]_l^\rho = \text{let } v \leftarrow [t]_l^\rho.\ \pi_2(v)
     $$

     其中，$\pi_2$ 是对的第二投影函数。

   **注意**：在 $\text{let } v \leftarrow e.\ e'$ 中，如果 $e = \bot$，则整个表达式的值为 $\bot$。

7. **函数抽象**：

   $$
   [\lambda x. t]_l^\rho = \lambda d \in (V_{\tau_1})_\bot.\ [t]_l^{\rho[d/x]}
   $$

   其中，$\lambda x. t : \tau_1 \rightarrow \tau_2$。

   **解释**：函数抽象的指称是一个从 $(V_{\tau_1})_\bot$ 到 $(V_{\tau_2})_\bot$ 的函数，接受一个参数 $d$，并在环境 $\rho$ 中将 $x$ 绑定为 $d$ 后，计算 $t$ 的指称。

8. **函数应用**：

   $$
   [(t_1\ t_2)]_l^\rho = \text{let } \varphi \leftarrow [t_1]_l^\rho.\ \varphi([t_2]_l^\rho)
   $$

   **解释**：首先计算 $t_1$ 的指称，得到函数 $\varphi$，然后应用于 $t_2$ 的指称。

9. **let 绑定**：

   $$
   [\text{let } x \leftarrow t_1 \ \text{in } t_2]_l^\rho = [t_2]_l^{\rho[[t_1]_l^\rho / x]}
   $$

   **解释**：首先计算 $t_1$ 的指称，将结果绑定到 $x$，然后在更新的环境中计算 $t_2$ 的指称。

10. **递归定义**：

    $$
    [\text{rec } x. t]_l^\rho = \mu d.\ [t]_l^{\rho[d/x]}
    $$

    **解释**：递归定义的指称是 $d$ 的最小不动点，其中 $d$ 满足 $d = [t]_l^{\rho[d/x]}$。

---

### 一些重要的引理和证明

**引理11.17**

**陈述**：

设 $t$ 是一个可类型的项。设 $\rho, \rho'$ 是在 $t$ 的自由变量上相同的环境。那么 $[t]_l^\rho = [t]_l^{\rho'}$。

**证明**：

- **思路**：对 $t$ 进行结构归纳，证明在 $t$ 的自由变量上相同的环境对 $[t]_l^\rho$ 的值没有影响。

- **基础情况**：

  - **变量**：$t = x$，$[x]_l^\rho = \rho(x)$。由于 $\rho$ 和 $\rho'$ 在 $x$ 上相同，所以 $[x]_l^\rho = [x]_l^{\rho'}$。

  - **整数常量**：$t = n$，$[n]_l^\rho = n$。常量不依赖于环境，显然成立。

- **归纳步骤**：

  - **算术运算**、**条件语句**、**构造对**、**投影**、**函数抽象**、**函数应用**、**let 绑定**、**递归定义**等，类似处理。

- **结论**：因此，$[t]_l^\rho = [t]_l^{\rho'}$。

---

**引理11.18（替换引理）**

**陈述**：

设 $s$ 是一个闭项，$s : \tau$。设 $x$ 是类型为 $\tau$ 的变量。假设 $t : \tau'$。则 $t[s/x] : \tau'$，并且 $[t[s/x]]_l^\rho = [t]_l^{\rho[[s]_l^\rho / x]}$。

**证明**：

- **思路**：对 $t$ 进行结构归纳，证明替换后的项的指称等于在环境中用 $[s]_l^\rho$ 替换 $x$ 后的指称。

- **基础情况**：

  - **变量**：$t = x$。

    - 如果 $x$ 就是被替换的变量，那么 $t[s/x] = s$。

      - $[t[s/x]]_l^\rho = [s]_l^\rho$

      - $[t]_l^{\rho[[s]_l^\rho / x]} = \rho[[s]_l^\rho / x](x) = [s]_l^\rho$

    - 如果 $x$ 不是被替换的变量，那么 $t[s/x] = x$，$[t[s/x]]_l^\rho = \rho(x)$，$[t]_l^{\rho[[s]_l^\rho / x]} = \rho(x)$。

  - **整数常量**：$t = n$，替换后仍是 $n$，指称不变。

- **归纳步骤**：

  - **算术运算**、**条件语句**、**构造对**、**投影**、**函数抽象**、**函数应用**、**let 绑定**、**递归定义**等，类似处理。

- **结论**：因此，$[t[s/x]]_l^\rho = [t]_l^{\rho[[s]_l^\rho / x]}$。

---

**引理11.19**

**陈述**：

(i) 如果 $t : \tau$，则对于任意环境 $\rho \in \text{Env}_l$，有 $[t]_l^\rho \in (V_\tau)_\bot$。

(ii) 如果 $c \in C_\tau$，则对于任意环境 $\rho \in \text{Env}_l$，有 $[c]_l^\rho \ne \bot$。

**证明**：

- **(i)**：通过对 $t$ 进行结构归纳。

  - **基础情况**：

    - **变量**：$[x]_l^\rho = \rho(x) \in (V_\tau)_\bot$。

    - **整数常量**：$[n]_l^\rho = n \in V_{\text{int}} \subseteq (V_{\text{int}})_\bot$。

  - **归纳步骤**：

    - **算术运算**：$[t_1 + t_2]_l^\rho = [t_1]_l^\rho \oplus_\bot [t_2]_l^\rho$。由于 $[t_1]_l^\rho, [t_2]_l^\rho \in (V_{\text{int}})_\bot$，因此结果在 $(V_{\text{int}})_\bot$ 中。

    - **其他构造**类似处理。

- **(ii)**：通过对规范形式 $c$ 进行结构归纳。

  - **基础情况**：

    - **整数常量**：$c = n$，$[n]_l^\rho = n \ne \bot$。

  - **归纳步骤**：

    - **函数抽象**：$c = \lambda x. t$，$[c]_l^\rho = \lambda d \in (V_{\tau_1})_\bot.\ [t]_l^{\rho[d/x]}$。由于 $t$ 是闭项或只依赖于 $x$，所以 $[c]_l^\rho \ne \bot$。

- **结论**：因此，(i) 和 (ii) 成立。

---

## 11.8 惰性语义的一致性

**目标**：

我们展示指称语义相对于操作语义是充分的（adequate），即它尊重评估关系并在项何时收敛（converge）时达成一致。

**定义**：

- **操作收敛**：对于可类型的闭项 $t$，定义相对于惰性求值的操作收敛为：

  $$
  t \Downarrow_l \quad \text{当且仅当存在 } c \text{，使得 } t \ \longrightarrow_l \ c
  $$

- **指称收敛**：定义指称收敛为：

  $$
  t \Downarrow_d \quad \text{当且仅当存在 } v \in V_\tau \text{，使得 } [t]_l^\rho = v
  $$

  其中，$\rho$ 是 $\text{Env}_l$ 中的任意环境。

---

**结论**：

- 如果 $t \Downarrow_l$，即 $t$ 评估到某个规范形式 $c$，我们将证明对于任意环境 $\rho$，有 $[t]_l^\rho = [c]_l^\rho$。

- 由于根据引理11.19，$[c]_l^\rho \ne \bot$，因此这将意味着 $t \Downarrow_d$。

- 我们还将证明逆命题：如果 $t \Downarrow_d$，则 $t \Downarrow_l$。

---

**证明策略**：

- 和急切求值的情况类似，我们需要证明一个更强的关系，通过逻辑关系（logical relations）来表达，存在于项和其指称之间。

### 引理11.20

**陈述**：

如果 $t \ \longrightarrow_l \ c$，则对于任意环境 $\rho$，有 $[t]_l^\rho = [c]_l^\rho$。

**证明**：

- **思路**：对惰性求值的评估规则进行归纳，证明在每个规则下，结论成立。

- **关键步骤**：

  - **规范形式**：显然，$[c]_l^\rho = [c]_l^\rho$。

  - **算术运算**：如果 $t_1 \ \longrightarrow_l \ n_1$，$t_2 \ \longrightarrow_l \ n_2$，则 $[t_1]_l^\rho = n_1$，$[t_2]_l^\rho = n_2$，因此 $[t_1 \text{ op } t_2]_l^\rho = n_1 \text{ op } n_2$。

  - **函数应用**：对于惰性求值，函数应用规则为：

    $$
    \frac{t_1 \ \longrightarrow_l \ \lambda x. t' \quad t_1[t_2 / x] \ \longrightarrow_l \ c}{(t_1\ t_2) \ \longrightarrow_l \ c}
    $$

    - 假设 $[t_1]_l^\rho = \lambda d.\ [t']_l^{\rho[d/x]}$。

    - 然后，$[t_1\ t_2]_l^\rho = \left( \lambda d.\ [t']_l^{\rho[d/x]} \right)([t_2]_l^\rho) = [t']_l^{\rho[[t_2]_l^\rho / x]}$。

    - 根据归纳假设，$[t_1[t_2 / x]]_l^\rho = [t']_l^{\rho[[t_2]_l^\rho / x]}$。

    - 因此，$[t_1\ t_2]_l^\rho = [t_1[t_2 / x]]_l^\rho$。

- **结论**：因此，$[t]_l^\rho = [c]_l^\rho$。

---

**引理11.23**

**陈述**：

设 $t$ 是一个可类型的闭项。那么，如果 $t \Downarrow_d$，则 $t \Downarrow_l$。

**证明**：

- **思路**：通过对项 $t$ 进行结构归纳，并加强归纳假设，使用逻辑关系的方法。

- **定义逻辑关系**：

  对于每个类型 $\tau$，定义关系 $\leq_\tau \subseteq V_\tau \times C_\tau$。

  - **基本类型**：对于所有数字 $n$，$n \leq_{\text{int}} n$。

  - **函数类型**：对于 $\varphi \in V_{\tau_1 \rightarrow \tau_2}$，$c \in C_{\tau_1 \rightarrow \tau_2}$，有：

    $$
    \varphi \leq_{\tau_1 \rightarrow \tau_2} \lambda x. t \quad \iff \quad \forall d \in (V_{\tau_1})_\bot, \ \text{如果 } d \leq_{\tau_1} u \text{，且 } u \text{ 是闭项，则 } \varphi(d) \leq_{\tau_2} t[u / x]
    $$

- **扩展到 $ (V_\tau)_\bot $ 和项**：

  - 对于 $d \in (V_\tau)_\bot$，$t$ 是类型 $\tau$ 的闭项，定义：

    $$
    d \leq_\tau t \quad \iff \quad \text{如果 } d = v \in V_\tau \text{，则存在 } c \text{，使得 } t \ \longrightarrow_l \ c \text{，且 } v \leq_\tau c
    $$

- **证明**：

  - **归纳假设**：对于所有项 $t$，如果 $[t]_l^\rho = v$，并且对于 $t$ 的自由变量 $x_i$，有 $d_i \leq_{\tau_i} s_i$，那么存在规范形式 $c$，使得 $t[s_1 / x_1, \dots, s_k / x_k] \ \longrightarrow_l \ c$，且 $v \leq_\tau c$。

  - **基础情况**：

    - **变量**：$t = x$。

      - $[x]_l^\rho = \rho(x) = d$

      - 由于 $d \leq_{\tau} s$，$s$ 是闭项。

      - 因此，$x[s / x] = s$，且 $t[s / x] \ \longrightarrow_l \ c$，$d \leq_{\tau} c$。

    - **整数常量**：类似处理。

  - **归纳步骤**：

    - **函数应用**：

      - $t = t_1\ t_2$。

      - 假设 $[t]_l^\rho = v$。

      - 通过归纳假设，得到 $t_1[s_i / x_i] \ \longrightarrow_l \ \lambda x. t'$，$t_2[s_i / x_i] \ \longrightarrow_l s'$。

      - 由于 $[t]_l^\rho = \varphi(d)$，并且 $\varphi \leq_{\tau_1 \rightarrow \tau_2} \lambda x. t'$，$d \leq_{\tau_1} s'$。

      - 因此，$\varphi(d) \leq_{\tau_2} t'[s' / x]$。

      - 由归纳假设，$t'[s' / x] \ \longrightarrow_l c$，$[t]_l^\rho \leq_{\tau} c$。

    - **其他构造**类似处理。

- **结论**：因此，如果 $t \Downarrow_d$，则 $t \Downarrow_l$。

---

**推论11.24**

**陈述**：

假设 $t$ 是一个类型为 $\text{int}$ 的闭项。那么，对于任意 $n \in \mathbb{N}$，有：

$$
t \ \longrightarrow_l \ n \quad \iff \quad [t]_l^\rho = n
$$

**证明**：

- 由于 $\text{int}$ 类型的规范形式是数字，逻辑关系在这种类型上是单射的。

- 因此，如果 $t$ 的指称为 $n$，则 $t$ 必然评估为 $n$。

- 反之亦然。

---

## 11.9 不动点算子

**目标**：

指称语义为我们提供了数学模型，可以用来推理具有高阶类型的语言中项的评估。作为一个例子，我们将研究如何在急切和惰性变体的语言中表达不动点算子（fixed-point operators）。

**惰性求值中的不动点算子**：

- 首先，我们假设求值是惰性的。

- **定义**：一个不动点算子是类型为 $(\tau \rightarrow \tau) \rightarrow \tau$ 的闭项 $Y$，当应用于一个抽象 $F$ 时，产生 $F$ 的一个不动点，即：

  $$
  Y F = F (Y F)
  $$

- **猜测定义**：基于上述性质，一个合理的定义是：

  $$
  Y = \text{rec } Y.\ (\lambda f. f (Y f))
  $$

- **验证**：

  - 根据指称语义，这确实定义了一个不动点算子。

  - 我们计算 $Y$ 的指称：

    $$
    [Y]_l^\rho = \mu \varphi.\ \lambda f.\ f (\varphi f)
    $$

    其中，$\varphi$ 是从 $(V_{\tau \rightarrow \tau})_\bot$ 到 $(V_\tau)_\bot$ 的函数。

  - 通过展开不动点，我们可以证明 $Y F = F (Y F)$。

- **结论**：在惰性求值下，不动点算子的定义相对直接。

---

**急切求值中的不动点算子**：

- 在急切求值下，上述定义不再有效，因为急切求值会导致无限展开，导致发散。

- **问题**：根据急切求值的指称语义，$[Y F]_e^\rho = \bot$，即计算发散。

- **解决方案**：

  - 使用抽象来延迟求值，通过这种方式模拟惰性语言。

  - 重新定义 $Y$ 为：

    $$
    Y = \text{rec } Y.\ (\lambda f.\ \lambda x.\ f (Y f) x)
    $$

  - **验证**：

    - 计算 $Y$ 的指称，发现它确实是一个不动点算子。

    - 通过延迟对 $Y f$ 的求值，避免了急切求值导致的发散。

- **结论**：在急切求值下，通过使用额外的函数抽象，可以成功地定义不动点算子。

---

**总结**：

- 指称语义允许我们分析和理解编程语言中高阶函数和不动点算子的行为。

- 在惰性求值下，不动点算子的定义和使用相对简单。

- 在急切求值下，需要采取额外的措施（如增加抽象）来避免发散，实现不动点算子。

---

**练习11.26**

**问题**：

证明即使当 $[F]_l^\rho = \bot$，对于 $F : \tau \rightarrow \tau$，也有：

$$
[F (Y F)]_l^\rho = \bot
$$

**解答**：

- **思路**：由于 $[F]_l^\rho = \bot$，因此 $F$ 的应用将导致发散。

- **计算**：

  - $[Y F]_l^\rho = \bot$，因为 $Y F$ 需要 $F$ 的值。

  - 因此，$[F (Y F)]_l^\rho = [F]_l^\rho ([Y F]_l^\rho) = \bot ([Y F]_l^\rho) = \bot$

- **结论**：因此，即使 $[F]_l^\rho = \bot$，仍然有 $[F (Y F)]_l^\rho = \bot$。

---

**练习11.27**

**问题**：

从操作语义出发，论证 $R (\lambda x. t)$ 发散，即不存在规范形式 $c$ 使得 $R (\lambda x. t) \ \longrightarrow_e \ c$。

**解答**：

- **思路**：在急切求值下，$R (\lambda x. t)$ 的评估将导致无限的展开，导致发散。

- **论证**：

  - 根据急切求值的评估规则，评估 $R (\lambda x. t)$ 需要先评估 $R$，然后评估其应用。

  - 由于 $R$ 的定义涉及到自身的应用，导致无限递归。

  - 因此，评估永远不会到达规范形式，发散。

- **结论**：因此，$R (\lambda x. t)$ 发散。

---

**进一步说明**：

- **数学中的函数**通常被认为是相同的，例如 $F (Y F)$ 和 $\lambda x. (F (Y F) x)$ 在数学上被视为相同的函数。

- **在编程语言中**，由于求值策略的不同，$F (Y F)$ 和 $\lambda x. (F (Y F) x)$ 的行为可能不同。

- **通过指称语义**，我们可以更深入地理解这些区别，并设计正确的程序结构来实现预期的行为。

### ---------------------------

## 11.10 观察和完全抽象

我们刚刚看到，如何在指称语义提供的数学模型中进行推理，以解释程序的行为。根据指称语义，某些项表现为不动点算子（fixed-point operators）。仅仅使用操作语义，很难证明甚至正确地陈述这些事实。我们可能会想，为什么我们有理由使用指称语义来推断程序在机器上将如何运行，当然假设实现忠实于我们的操作语义。**我们为什么有理由这样做呢？**因为操作语义和指称语义在“感兴趣的观察”（observations of interest）上是一致的。

**关键点**：

- 如果指称语义表明一个类型为 $\text{int}$ 的闭项表示一个特定的整数，那么它将精确地评估为该整数，反之亦然。
- 对于其他类型，如果一个项收敛（即不指称 $\bot$），那么它的评估也将收敛，反之亦然。
- 两种语义（指称语义和操作语义）在告诉我们一个项是否收敛以及类型为 $\text{int}$ 的项评估为哪个整数的观察上是一致的。
  

**这种一致性**就是指称语义相对于操作语义的充分性（adequacy）结果的内容。事实上，我们可以将观察限制为仅仅是收敛性的观察。对于收敛性的充分性将确保两种语义也在类型为 $\text{int}$ 的项如何评估上达成一致。

**简单的论证**基于将项包含在一个上下文（context）中：

$$
\text{if } - \ \text{then } 0 \ \text{else } \text{Diverge}
$$

其中，$\text{Diverge} : \text{int}$ 是一个发散的闭项。

**对于类型为 $\text{int}$ 的闭项 $t$ 和数字 $n$，我们可以论证**，对于急切和惰性语义，都有：

- $t \longrightarrow_e n$ 当且仅当 $\text{if } (t - n) \ \text{then } 0 \ \text{else } \text{Diverge} \ \longrightarrow_e 0$
- 当且仅当 $\text{if } (t - n) \ \text{then } 0 \ \text{else } \text{Diverge}$ 收敛
- 当且仅当 $[t]_e^\rho = n$（基于充分性）

**这说明**：

- 类型为 $\text{int}$ 的项的评估和收敛性是一个合理的观察选择。
- 许多实现确实精确地向用户报告了我们讨论的那种收敛行为，仅仅为诸如整数或列表等具体数据类型提供具体的值。

**然而**，如果我们将兴趣扩大到其他属性，例如评估一个项需要多长时间，我们可能需要更详细的观察，以及为了尊重这些观察，更详细的语义。

**也有可能**限制观察，对于固定的操作语义，粗略的指称语义就足够了。为了说明这一点，我们给出了惰性语言的另一种指称语义。这种指称语义将忽略一般情况下高阶类型的收敛行为，但仍然确保在基本类型 $\text{int}$ 上：

$$
t \longrightarrow_l n \quad \text{当且仅当} \quad [t]_l^\rho = \underline{n}
$$

其中，$t$ 是类型为 $\text{int}$ 的闭项，$n$ 是整数。它关注于类型为 $\text{int}$ 的项的评估所产生的可打印值的观察。

---

### 定义 $D_\tau$：指称语义中的值的 cpo

我们通过对类型 $\tau$ 进行结构归纳，定义 $D_\tau$：

1. **基本类型**（$\text{int}$）：

   $$
   D_{\text{int}} = N_\bot
   $$

   **解释**：$D_{\text{int}}$ 是自然数的 lifted cpo，包括底元素 $\bot$。

2. **乘积类型**（$\tau_1 * \tau_2$）：

   $$
   D_{\tau_1 * \tau_2} = D_{\tau_1} \times D_{\tau_2}
   $$

3. **函数类型**（$\tau_1 \rightarrow \tau_2$）：

   $$
   D_{\tau_1 \rightarrow \tau_2} = [D_{\tau_1} \rightarrow D_{\tau_2}]
   $$

   **解释**：$D_{\tau_1 \rightarrow \tau_2}$ 是从 $D_{\tau_1}$ 到 $D_{\tau_2}$ 的函数的集合。

---

### 环境的定义

惰性语言的环境现在被认为是一个函数：

$$
\rho : \text{Var} \rightarrow \bigcup_{\tau \text{ 是类型}} D_\tau
$$

其中，对于任何变量 $x$ 和类型 $\tau$，如果 $x : \tau$，则 $\rho(x) \in D_\tau$。

记 $\text{Env}$ 为所有这样的环境的 cpo。

---

### 指称语义的定义

类型为 $\tau$ 的可类型项 $t$ 的指称 $[t]_l^\rho$ 是通过对 $t$ 进行结构归纳来定义的：

1. **变量**：

   $$
   [x]_l^\rho = \rho(x)
   $$

2. **整数常量**：

   $$
   [n]_l^\rho = \underline{n}
   $$

3. **算术运算**（以 $+$ 为例）：

   $$
   [t_1 + t_2]_l^\rho = [t_1]_l^\rho \oplus_\bot [t_2]_l^\rho
   $$

   其中，$\oplus_\bot$ 是 lifted 算术操作。

4. **条件语句**：

   $$
   [\text{if } t_0 \ \text{then } t_1 \ \text{else } t_2]_l^\rho = \text{Cond}([t_0]_l^\rho, [t_1]_l^\rho, [t_2]_l^\rho)
   $$

5. **构造对**：

   $$
   [(t_1, t_2)]_l^\rho = ([t_1]_l^\rho, [t_2]_l^\rho)
   $$

6. **投影**：

   - **第一投影**：

     $$
     [\text{fst}(t)]_l^\rho = \pi_1([t]_l^\rho)
     $$

   - **第二投影**：

     $$
     [\text{snd}(t)]_l^\rho = \pi_2([t]_l^\rho)
     $$

7. **函数抽象**：

   $$
   [\lambda x. t]_l^\rho = \lambda d \in D_{\tau_1}.\ [t]_l^{\rho[d / x]}
   $$

   其中，$\lambda x. t : \tau_1 \rightarrow \tau_2$。

8. **函数应用**：

   $$
   [(t_1\ t_2)]_l^\rho = [t_1]_l^\rho ([t_2]_l^\rho)
   $$

9. **let 绑定**：

   $$
   [\text{let } x \leftarrow t_1 \ \text{in } t_2]_l^\rho = [t_2]_l^{\rho[[t_1]_l^\rho / x]}
   $$

10. **递归定义**：

    $$
    [\text{rec } x. t]_l^\rho = \mu d.\ [t]_l^{\rho[d / x]}
    $$

---

### 练习11.28

**问题**：

1. **(1)** 假设变量 $x : \text{int} \rightarrow \text{int}$，$w : \text{int} \rightarrow \text{int}$，$y : \text{int}$。求以下项的指称：

   - $((\lambda x. x)\ 0)$
   - $((\lambda x. \lambda y. (x\ y))\ 0)$

   其中，$0 = \text{rec } w. w$。

2. **(2)** 证明对于惰性语言的操作语义：

   $$
   t \ \longrightarrow_l \ c \quad \implies \quad [t]_l^\rho = [c]_l^\rho
   $$

   对于任意环境 $\rho$。

   **提示**：在使用规则归纳的论证中，你只需要处理足够的情况以令人信服。你可以假设替换引理的一个变体，但要清晰地陈述它。

3. **(3)** 对于类型为 $\text{int}$ 的闭项 $t$，证明：

   $$
   t \ \longrightarrow_l \ n \quad \text{当且仅当} \quad [t]_l^\rho = \underline{n}
   $$

   对于任何 $n \in \mathbb{N}$。

   **建议**：使用类型为 $\tau$ 的元素 $d \in D_\tau$ 和闭项 $t$ 之间的逻辑关系 $\leq_\tau$，按照以下方式通过对类型的结构归纳来定义：

   $$
   d \leq_{\text{int}} t \quad \iff \quad d = \underline{n} \implies t \ \longrightarrow_l \ n
   $$

---

#### **解答：**

##### **(1) 计算项的指称**

**已知**：

- 变量 $x : \text{int} \rightarrow \text{int}$
- 变量 $w : \text{int} \rightarrow \text{int}$
- 变量 $y : \text{int}$

**定义**：

- $0 = \text{rec } w. w$

**计算**：

1. **计算 $((\lambda x. x)\ 0)$ 的指称**

   **步骤**：

   - 计算 $[0]_l^\rho$
     - $0 = \text{rec } w. w$
     - 根据指称语义：
       $$
       [0]_l^\rho = \mu d.\ \rho[d / w](w)
       $$
     - 由于 $w : \text{int} \rightarrow \text{int}$，且 $\rho[w \mapsto d](w) = d$，因此
       $$
       [0]_l^\rho = \mu d.\ d
       $$
     - 这个不动点是 $\bot$，因为 $\mu d.\ d$ 的最小解是 $\bot$。

   - 计算 $[(\lambda x. x)]_l^\rho$
     - $[\lambda x. x]_l^\rho = \lambda d \in D_{\text{int}}.\ \rho[d / x](x)$
     - 由于 $\rho[d / x](x) = d$，所以
       $$
       [\lambda x. x]_l^\rho = \lambda d.\ d
       $$

   - 因此，
     $$
     [(\lambda x. x)\ 0]_l^\rho = [\lambda x. x]_l^\rho ([0]_l^\rho) = (\lambda d.\ d)(\bot) = \bot
     $$

   **结论**：

   $$
   [(\lambda x. x)\ 0]_l^\rho = \bot
   $$

2. **计算 $((\lambda x. \lambda y. (x\ y))\ 0)$ 的指称**

   **步骤**：

   - 计算 $[0]_l^\rho$，如上所述，$[0]_l^\rho = \bot$

   - 计算 $[\lambda x. \lambda y. (x\ y)]_l^\rho$
     - $[\lambda x. \lambda y. (x\ y)]_l^\rho = \lambda d_1 \in D_{\text{int} \rightarrow \text{int}}.\ [\lambda y. (x\ y)]_l^{\rho[d_1 / x]}$
     - 继续计算内部的抽象：
       $$
       [\lambda y. (x\ y)]_l^{\rho[d_1 / x]} = \lambda d_2 \in D_{\text{int}}.\ [x\ y]_l^{\rho[d_1 / x][d_2 / y]}
       $$
     - 由于 $x$ 在 $\rho[d_1 / x]$ 中绑定为 $d_1$，$y$ 在 $\rho[d_1 / x][d_2 / y]$ 中绑定为 $d_2$。
     - 因此，
       $$
       [x\ y]_l^{\rho[d_1 / x][d_2 / y]} = d_1(d_2)
       $$
     - 因此，整个指称为：
       $$
       [\lambda x. \lambda y. (x\ y)]_l^\rho = \lambda d_1.\ \lambda d_2.\ d_1(d_2)
       $$

   - 因此，
     $$
     [(\lambda x. \lambda y. (x\ y))\ 0]_l^\rho = (\lambda d_1.\ \lambda d_2.\ d_1(d_2))([0]_l^\rho) = (\lambda d_2.\ \bot (d_2)) = \lambda d_2.\ \bot
     $$

   **结论**：

   $$
   [(\lambda x. \lambda y. (x\ y))\ 0]_l^\rho = \lambda d_2.\ \bot
   $$

   也就是说，该项的指称是一个从 $D_{\text{int}}$ 到 $D_{\text{int}}$ 的函数，恒等于 $\bot$。

---

##### **(2) 证明 $t \ \longrightarrow_l \ c \implies [t]_l^\rho = [c]_l^\rho$**

**思路**：

- 我们需要对惰性语言的操作语义的评估规则进行规则归纳，证明对于任意环境 $\rho$，如果 $t \ \longrightarrow_l \ c$，则 $[t]_l^\rho = [c]_l^\rho$。

- **假设**我们可以使用替换引理的一个变体，即对于任意项 $t$ 和变量 $x$，有：

  $$
  [t[x \mapsto s]]_l^\rho = [t]_l^{\rho[[s]_l^\rho / x]}
  $$

- 我们只需证明足够的情况以令人信服。

**证明**：

1. **规范形式**：

   - 规则：
     $$
     \frac{}{c \ \longrightarrow_l \ c}
     $$
   - 显然，$[c]_l^\rho = [c]_l^\rho$

2. **算术运算**：

   - 规则：
     $$
     \frac{t_1 \ \longrightarrow_l n_1 \quad t_2 \ \longrightarrow_l n_2}{t_1 \ \text{op} \ t_2 \ \longrightarrow_l n_1 \ \text{op} \ n_2}
     $$
   - 假设 $[t_1]_l^\rho = \underline{n_1}$，$[t_2]_l^\rho = \underline{n_2}$。
   - 则 $[t_1 \ \text{op} \ t_2]_l^\rho = \underline{n_1} \ \text{op} \ \underline{n_2} = \underline{n}$。
   - 同时，$[n_1 \ \text{op} \ n_2]_l^\rho = \underline{n}$。
   - 因此，$[t_1 \ \text{op} \ t_2]_l^\rho = [n_1 \ \text{op} \ n_2]_l^\rho$。

3. **条件语句**：

   - 类似处理。

4. **函数应用**：

   - 规则：
     $$
     \frac{t_1 \ \longrightarrow_l \lambda x. t' \quad t_1[t_2 / x] \ \longrightarrow_l c}{(t_1\ t_2) \ \longrightarrow_l c}
     $$
   - 假设 $[t_1]_l^\rho = \varphi$，其中 $\varphi = [\lambda x. t']_l^\rho$
   - 则 $[t_1\ t_2]_l^\rho = \varphi([t_2]_l^\rho)$
   - 根据替换引理，有：
     $$
     [t_1[t_2 / x]]_l^\rho = [t']_l^{\rho[[t_2]_l^\rho / x]}
     $$
   - 由于 $\varphi([t_2]_l^\rho) = [t']_l^{\rho[[t_2]_l^\rho / x]}$
   - 因此，$[t_1\ t_2]_l^\rho = [t_1[t_2 / x]]_l^\rho$
   - 又因为 $t_1[t_2 / x] \ \longrightarrow_l c$，所以根据归纳假设，$[t_1[t_2 / x]]_l^\rho = [c]_l^\rho$
   - 因此，$[t_1\ t_2]_l^\rho = [c]_l^\rho$

**结论**：

- 通过对评估规则的归纳，我们证明了 $t \ \longrightarrow_l \ c \implies [t]_l^\rho = [c]_l^\rho$

---

##### **(3) 对于类型为 $\text{int}$ 的闭项 $t$，证明 $t \ \longrightarrow_l n \iff [t]_l^\rho = \underline{n}$**

**思路**：

- 使用逻辑关系 $\leq_\tau$，在类型 $\tau$ 的元素 $d \in D_\tau$ 和闭项 $t$ 之间建立关系。

- 定义：

  $$
  d \leq_{\text{int}} t \quad \iff \quad d = \underline{n} \implies t \ \longrightarrow_l n
  $$

- 我们需要证明：

  - 如果 $t \ \longrightarrow_l n$，则 $[t]_l^\rho = \underline{n}$

  - 如果 $[t]_l^\rho = \underline{n}$，则 $t \ \longrightarrow_l n$

**证明**：

1. **充分性（$t \ \longrightarrow_l n \implies [t]_l^\rho = \underline{n}$）**

   - 由 **(2)** 中的结论，如果 $t \ \longrightarrow_l c$，则 $[t]_l^\rho = [c]_l^\rho$
   - 当 $t \ \longrightarrow_l n$，则 $c = n$，因此 $[t]_l^\rho = [n]_l^\rho = \underline{n}$

2. **必要性（$[t]_l^\rho = \underline{n} \implies t \ \longrightarrow_l n$）**

   - 我们使用逻辑关系 $\leq_{\text{int}}$，对于 $d = \underline{n}$，有 $d \leq_{\text{int}} t$
   - 需要证明 $t \ \longrightarrow_l n$
   - 我们对 $t$ 进行结构归纳：

     - **变量**：不可能，因为 $t$ 是闭项，无自由变量

     - **整数常量**：$t = n$，显然 $t \ \longrightarrow_l n$

     - **算术运算**：

       - 假设 $t = t_1 + t_2$，且 $[t]_l^\rho = \underline{n}$

       - 则 $[t_1]_l^\rho = \underline{n_1}$，$[t_2]_l^\rho = \underline{n_2}$，且 $n = n_1 + n_2$

       - 由于 $[t_1]_l^\rho = \underline{n_1}$，根据归纳假设，$t_1 \ \longrightarrow_l n_1$

       - 同理，$t_2 \ \longrightarrow_l n_2$

       - 因此，根据操作语义的规则，$t \ \longrightarrow_l n$

     - **其他构造**类似处理

**结论**：

- 因此，对于类型为 $\text{int}$ 的闭项 $t$，有 $t \ \longrightarrow_l n \iff [t]_l^\rho = \underline{n}$

---

### 总结

**充分性**结果对于使用指称语义的数学模型来预测和推理程序行为至关重要。它们证明了指称语义相对于操作语义的充分性。

**然而**，要使指称语义与一组观察良好地契合，还有另一个重要的标准，即语义是**完全抽象的（fully abstract）**。

---

### 完全抽象（Full Abstraction）

**定义**：

- 给定一组观察（observations），如果对于两个具有相同类型的项 $t_1, t_2$，我们有 $t_1 \sim t_2$，表示对于所有上下文 $C[\,]$，$C[t_1]$ 和 $C[t_2]$ 的观察结果一致。

- **上下文（Context）**：直观上，一个上下文是一个带有“空洞” $[\,]$ 的项 $C[\,]$，我们可以将可类型的项 $t$ 填入其中，得到可类型的项 $C[t]$。

- 对于给定的观察，如果指称语义区分了操作语义无法区分的项，那么它就不是完全抽象的。

- **完全抽象性**：如果对于所有项 $t_1, t_2$，有：

  $$
  [t_1]_l^\rho = [t_2]_l^\rho \quad \iff \quad t_1 \sim t_2
  $$

  则指称语义相对于观察是完全抽象的。

**说明**：

- “$\implies$” 方向通常由充分性保证，即如果指称语义无法区分 $t_1, t_2$，则它们在所有上下文中的观察结果一致。

- “$\impliedby$” 方向更难证明，因为这需要确保指称语义只区分那些在观察下可区分的项。

---

### 为什么完全抽象性难以实现？

- 因为在我们的 cpo 中，有一些元素（如“并行或”函数）无法通过程序中的项来定义，而某些项对这些元素的行为不同。

- **“并行或”函数**是一种连续函数，它具有非顺序性，不能通过我们的语言中的项来表示。

- 这些“寄生”元素导致了指称语义区分了操作语义无法区分的项。

- **解决方法**：为了实现完全抽象性，我们需要重新定义 cpo 上的构造，限制到“顺序”函数，避免出现这些无法定义的元素。

- 然而，这非常困难，至少在不依赖于某种形式的操作语义的编码的情况下很难实现。

---

### 总结

- 完全抽象性虽然不是必须的，但它是一个有用的性质。

- 对于具有高阶类型的语言，实现完全抽象性激发了对高阶类型中顺序性的研究。

- 这种研究涉及将操作性的关键概念（如顺序性）形式化到域理论的数学框架中。

- 尽管完全抽象性可能难以实现，但它推动了编程语言语义理论的重要发展。

### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------