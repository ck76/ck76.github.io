[toc]



### 第35章 现代化的 Algol（Modernized Algol）

---

**现代化的 Algol**，记作 $L\{\text{nat cmd } *\}$，是一种基于经典语言 Algol 的**命令式（imperative）**、**块结构化（block-structured）**的编程语言。

- **$L\{\text{nat cmd } *\}$** 可以看作是 $L\{\text{nat } *\}$ 的扩展，它引入了一种新的语法类别：**命令（commands）**。
- **命令**作用于**可赋值量（assignables）**，通过检索和修改它们的内容。
- **可赋值量**是通过在指定作用域内的声明而引入的；这就是**块结构**的本质。
- **命令**可以通过**顺序组合（sequencing）**进行组合，并且可以使用**递归（recursion）**进行迭代。

---

### 35.1 现代化的 Algol

---

#### 1. 纯表达式和不纯命令的分离

$L\{\text{nat cmd } *\}$ 保持了**纯表达式（pure expressions）**和**不纯命令（impure commands）**之间的严格分离：

- **纯表达式**：其含义不依赖于任何可赋值量。
  - **优势**：表达式的求值顺序不受可赋值量的存在所限制，允许像在 PCF（纯函数式编程语言）中那样操作表达式。
- **不纯命令**：其含义是基于可赋值量定义的。
  - **特点**：命令的执行顺序受到严格限制，因为一个命令的执行可能会影响另一个命令的含义。

#### 2. 栈式纪律（Stack Discipline）

$L\{\text{nat cmd } *\}$ 的一个显著特征是遵循**栈式纪律（stack discipline）**：

- **定义**：可赋值量在进入其声明的作用域时分配（allocated），并在退出时解除分配（deallocated），使用传统的栈式管理。
- **优点**：避免了更复杂的存储管理形式，如垃圾回收，简化了内存管理。
- **代价**：减少了语言的表达能力，因为可赋值量的生命周期严格受限于作用域。

---

### 35.2 基本命令（Basic Commands）

---

#### 1. 命令的基本概念

- **命令（command）**：用于执行操作，特别是对可赋值量的操作，包括检索和修改其内容。
- **可赋值量（assignable）**：类似于变量，可以存储和修改值。

#### 2. 语法扩展

为了引入命令和可赋值量，我们需要扩展语言的语法。

##### 类型语法（Type Syntax）

$$
\text{Typ}\ \tau ::= \dotsb\ |\ \text{cmd} \quad \text{（命令类型，command type）}
$$

- **$\text{cmd}$**：表示命令的类型。

##### 表达式语法（Expression Syntax）

$$
\text{Exp}\ e ::= \dotsb\ |\ c \quad \text{（命令作为表达式）}
$$

- **$c$**：命令也可以作为一种特殊的表达式。

##### 命令语法（Command Syntax）

$$
\begin{align*}
\text{Cmd}\ c ::= &\ \text{skip} \quad \text{（空命令）} \\
                  &\ |\ c_1\ ;\ c_2 \quad \text{（命令序列）} \\
                  &\ |\ \text{assign}(x, e) \quad \text{（赋值）} \\
                  &\ |\ \text{newvar } x := e\ \text{in } c \quad \text{（新变量声明）}
\end{align*}
$$

- **$\text{skip}$**：不执行任何操作的命令。
- **$c_1\ ;\ c_2$**：顺序执行命令，先执行 $c_1$，再执行 $c_2$。
- **$\text{assign}(x, e)$**：将表达式 $e$ 的值赋给可赋值量 $x$。
- **$\text{newvar } x := e\ \text{in } c$**：在命令 $c$ 中声明一个新的可赋值量 $x$，初始值为 $e$。

---

### 35.3 静态语义（Statics）

---

#### 1. 类型规则

##### 赋值命令的类型规则

$$
\frac{
  \Gamma \vdash x : \text{var}(\tau) \quad \Gamma \vdash e : \tau
}{
  \Gamma \vdash \text{assign}(x, e) : \text{cmd}
}
$$

- **解释**：
  - 如果 $x$ 是类型为 $\text{var}(\tau)$ 的可赋值量，$e$ 是类型为 $\tau$ 的表达式，
  - 那么赋值命令 $\text{assign}(x, e)$ 的类型为 $\text{cmd}$。

##### 顺序命令的类型规则

$$
\frac{
  \Gamma \vdash c_1 : \text{cmd} \quad \Gamma \vdash c_2 : \text{cmd}
}{
  \Gamma \vdash c_1\ ;\ c_2 : \text{cmd}
}
$$

- **解释**：
  - 如果 $c_1$ 和 $c_2$ 都是命令，
  - 那么顺序组合命令 $c_1\ ;\ c_2$ 的类型为 $\text{cmd}$。

##### 新变量声明的类型规则

$$
\frac{
  \Gamma \vdash e : \tau \quad \Gamma, x : \text{var}(\tau) \vdash c : \text{cmd}
}{
  \Gamma \vdash \text{newvar } x := e\ \text{in } c : \text{cmd}
}
$$

- **解释**：
  - 如果 $e$ 是类型为 $\tau$ 的表达式，
  - 在扩展的类型环境 $\Gamma, x : \text{var}(\tau)$ 下，$c$ 是一个命令，
  - 那么新变量声明命令的类型为 $\text{cmd}$。

---

### 35.4 动态语义（Dynamics）

---

#### 1. 运行时状态

- **状态（state）**：映射可赋值量到它们的当前值的函数，表示程序的存储。

#### 2. 操作语义规则

##### 赋值命令的执行规则

$$
\frac{
  e\ \Downarrow\ v
}{
  \langle \text{assign}(x, e), s \rangle \rightarrow \langle \text{skip}, s[x \mapsto v] \rangle
}
$$

- **解释**：
  - 表达式 $e$ 在当前状态 $s$ 下求值为 $v$。
  - 执行赋值命令后，状态更新为 $s[x \mapsto v]$，即将 $x$ 的值更新为 $v$。
  - 赋值命令执行完毕，变为 $\text{skip}$。

##### 顺序命令的执行规则

$$
\frac{
  \langle c_1, s \rangle \rightarrow \langle c_1', s' \rangle
}{
  \langle c_1\ ;\ c_2, s \rangle \rightarrow \langle c_1'\ ;\ c_2, s' \rangle
}
$$

- **解释**：
  - 在状态 $s$ 下，命令 $c_1$ 变为 $c_1'$，状态变为 $s'$。
  - 顺序命令继续执行，更新为 $c_1'\ ;\ c_2$，状态为 $s'$。

$$
\frac{}{
  \langle \text{skip}\ ;\ c_2, s \rangle \rightarrow \langle c_2, s \rangle
}
$$

- **解释**：
  - 当 $c_1$ 执行完毕（变为 $\text{skip}$）时，继续执行 $c_2$。

##### 新变量声明的执行规则

$$
\frac{
  e\ \Downarrow\ v \quad \langle c, s[x \mapsto v] \rangle \rightarrow \langle c', s' \rangle
}{
  \langle \text{newvar } x := e\ \text{in } c, s \rangle \rightarrow \langle c', s' - x \rangle
}
$$

- **解释**：
  - 表达式 $e$ 在状态 $s$ 下求值为 $v$。
  - 在扩展的状态 $s[x \mapsto v]$ 下执行命令 $c$，得到 $c'$ 和新状态 $s'$。
  - 执行完毕后，删除 $x$ 的绑定，状态变为 $s' - x$。

---

### 35.5 安全性（Safety）

---

#### 1. 类型安全性定理

**定理**：如果在类型环境 $\Gamma$ 下，命令 $c$ 是类型正确的（$\Gamma \vdash c : \text{cmd}$），并且状态 $s$ 与 $\Gamma$ 一致，那么要么命令 $c$ 已经是 $\text{skip}$，要么存在 $c'$ 和 $s'$，使得 $\langle c, s \rangle \rightarrow \langle c', s' \rangle$。

#### 2. 保持性（Preservation）

- **保持性定理**：如果 $\Gamma \vdash c : \text{cmd}$，并且 $\langle c, s \rangle \rightarrow \langle c', s' \rangle$，那么 $\Gamma \vdash c' : \text{cmd}$。

#### 3. 前进性（Progress）

- **前进性定理**：如果 $\Gamma \vdash c : \text{cmd}$，并且状态 $s$ 与 $\Gamma$ 一致，那么要么 $c = \text{skip}$，要么存在 $c'$ 和 $s'$，使得 $\langle c, s \rangle \rightarrow \langle c', s' \rangle$。

---

### 35.6 一些编程习惯（Some Programming Idioms）

---

#### 1. 变量的作用域和生命周期

- **块结构**：可赋值量的生命周期限定在其声明的块内，退出块时自动销毁。
- **示例**：

  ```pseudo
  newvar x := 0 in
    assign(x, x + 1);
    assign(x, x * 2)
  ```

  - 在块内，$x$ 可以被多次赋值和使用。

#### 2. 命令的组合

- **顺序执行**：使用 $c_1\ ;\ c_2$ 来确保命令按指定顺序执行。

#### 3. 递归和循环

- **递归**：可以通过递归定义命令来实现循环行为。

---

### 35.7 类型化的命令和可赋值量（Typed Commands and Typed Assignables）

---

#### 1. 可赋值量的类型

- **$\text{var}(\tau)$**：表示类型为 $\tau$ 的可赋值量。

#### 2. 类型化命令的好处

- **类型安全性**：通过对命令和可赋值量进行类型检查，防止类型错误。
- **可读性和维护性**：明确类型有助于理解程序的行为和意图。

---

### 35.8 注释（Notes）

---

- **现代化的 Algol** 通过将命令式编程与类型系统相结合，提供了一个研究命令式语言特性的良好框架。
- **栈式纪律**的应用简化了内存管理，但限制了可赋值量的作用域和生命周期。
- **纯表达式与命令的分离**使得表达式的求值与命令的执行顺序相互独立，提高了语言的灵活性。

---

### 总结

---

通过深入理解现代化的 Algol 的语法、静态语义和动态语义，我们能够更好地理解命令式编程语言的设计原则，尤其是在类型系统和内存管理方面的考虑。

---

### ---------------------------------

### 第35章 现代化的 Algol

---

在本章中，我们将探讨**现代化的 Algol**（Modernized Algol），记作 $L\{\text{nat cmd } *\}$。这是一个基于经典语言 Algol 的**命令式（imperative）**、**块结构化（block-structured）**的编程语言。

- **$L\{\text{nat cmd } *\}$** 可以视为 $L\{\text{nat } *\}$（参见第10章）的扩展，增加了一种新的语法类别：**命令（commands）**。
- **命令**作用于**可赋值量（assignables）**，通过检索和修改它们的内容。
- **可赋值量**是通过在指定作用域内的声明而引入的；这就是**块结构**的本质。
- **命令**可以通过**顺序组合（sequencing）**进行组合，并且可以使用**递归（recursion）**进行迭代。

---

### 35.1 基本命令（Basic Commands）

---

#### 1. 纯表达式与不纯命令的区别

$L\{\text{nat cmd } *\}$ 的语法区分了**纯表达式（pure expressions）**和**不纯命令（impure commands）**。

- **纯表达式**包括 $L\{\text{nat } *\}$ 的表达式（如第10章所述），并增加了一个新的构造。
- **命令**是基于赋值操作的简单命令式编程语言的命令。

语言保持了**变量（variables）**和**可赋值量（assignables）**之间的明确区别。

- **变量**通过 $\lambda$-抽象引入，其意义通过替换（substitution）给出。
- **可赋值量**通过声明引入，其意义通过对其内容的赋值和检索给出，目前仅限于自然数。

#### 2. 表达式与命令的作用

- **表达式（Expressions）**：求值为值，不对可赋值量产生影响。
- **命令（Commands）**：执行以对可赋值量产生影响，并返回一个值。

- **命令的组合（Composition of commands）**：不仅顺序地执行命令，还将第一个命令返回的值传递给第二个命令，然后执行第二个命令。

- **命令返回的值**：目前仅限于自然数（但参见第35.3节了解一般情况）。

---

#### 35.1.1 语法（Syntax）

$L\{\text{nat cmd } *\}$ 的语法如下，为简洁起见，我们省略了 $L\{\text{nat } *\}$ 的表达式语法的重复部分。

##### 类型语法（Type Syntax）

$$
\text{Typ}\ \tau ::= \dotsb\ |\ \text{cmd} \quad \text{（命令类型，command）}
$$

- **$\text{cmd}$**：表示命令类型。

##### 表达式语法（Expression Syntax）

$$
\text{Exp}\ e ::= \dotsb\ |\ \text{cmd}(m) \quad \text{（封装命令，encapsulation）}
$$

- **$\text{cmd}(m)$**：由未求值的命令 $m$ 组成，被视为类型为 $\text{cmd}$ 的值。

##### 命令语法（Command Syntax）

$$
\begin{align*}
\text{Cmd}\ m ::= &\ \text{ret}(e) \quad \text{（返回，return）} \\
                  &\ |\ \text{bnd}(e;\ x.m) \quad \text{（序列，sequence）} \\
                  &\ |\ \text{dcl}(e;\ a.m) \quad \text{（新可赋值量，new assignable）} \\
                  &\ |\ \text{get}[a] \quad \text{（获取，fetch）} \\
                  &\ |\ \text{set}[a](e) \quad \text{（赋值，assign）}
\end{align*}
$$

- **$\text{ret}(e)$**：返回表达式 $e$ 的值，不对可赋值量产生任何影响。
- **$\text{bnd}(e;\ x.m)$**：求值 $e$ 得到一个封装的命令，然后执行该命令，对可赋值量产生影响，并将其返回的值替换到 $m$ 中的变量 $x$，然后执行 $m$。
- **$\text{dcl}(e;\ a.m)$**：引入一个新的可赋值量 $a$，初始内容由表达式 $e$ 给出，用于命令 $m$ 中。
- **$\text{get}[a]$**：返回可赋值量 $a$ 的当前内容。
- **$\text{set}[a](e)$**：将表达式 $e$ 的值赋给可赋值量 $a$，并返回该值。

---

#### 35.1.1 静态语义（Statics）

$L\{\text{nat cmd } *\}$ 的静态语义包含两种形式的判断（judgment）：

1. **表达式类型判断**：$\Gamma \vdash_{\Sigma} e : \tau$。

   - **$\Gamma$**：变量的类型环境，记录变量及其类型。
   - **$\Sigma$**：签名（signature），由一组有限的可赋值量组成。

2. **命令形成判断**：$\Gamma \vdash_{\Sigma} m\ \text{ok}$。

以下是这些判断的推导规则：

###### **规则 (35.1a)：命令类型的引入**

$$
\frac{
  \Gamma \vdash_{\Sigma} m\ \text{ok}
}{
  \Gamma \vdash_{\Sigma} \text{cmd}(m) : \text{cmd}
}
\tag{35.1a}
$$

- **解释**：
  - 如果在上下文 $\Gamma$ 和签名 $\Sigma$ 下，命令 $m$ 是正确的（$\Gamma \vdash_{\Sigma} m\ \text{ok}$），
  - 那么表达式 $\text{cmd}(m)$ 的类型为 $\text{cmd}$。

###### **规则 (35.1b)：返回命令**

$$
\frac{
  \Gamma \vdash_{\Sigma} e : \text{nat}
}{
  \Gamma \vdash_{\Sigma} \text{ret}(e)\ \text{ok}
}
\tag{35.1b}
$$

- **解释**：
  - 如果表达式 $e$ 的类型为 $\text{nat}$，
  - 那么命令 $\text{ret}(e)$ 是正确的。

###### **规则 (35.1c)：序列命令**

$$
\frac{
  \Gamma \vdash_{\Sigma} e : \text{cmd} \quad \Gamma,\ x : \text{nat} \vdash_{\Sigma} m\ \text{ok}
}{
  \Gamma \vdash_{\Sigma} \text{bnd}(e;\ x.m)\ \text{ok}
}
\tag{35.1c}
$$

- **解释**：
  - 如果表达式 $e$ 的类型为 $\text{cmd}$，
  - 并且在扩展的环境 $\Gamma,\ x : \text{nat}$ 和签名 $\Sigma$ 下，命令 $m$ 是正确的，
  - 那么命令 $\text{bnd}(e;\ x.m)$ 是正确的。

###### **规则 (35.1d)：声明新可赋值量**

$$
\frac{
  \Gamma \vdash_{\Sigma} e : \text{nat} \quad \Gamma \vdash_{\Sigma,\ a} m\ \text{ok}
}{
  \Gamma \vdash_{\Sigma} \text{dcl}(e;\ a.m)\ \text{ok}
}
\tag{35.1d}
$$

- **解释**：
  - 如果表达式 $e$ 的类型为 $\text{nat}$，
  - 并且在签名扩展 $\Sigma,\ a$ 下，命令 $m$ 是正确的，
  - 那么命令 $\text{dcl}(e;\ a.m)$ 是正确的。
- **注意**：
  - 可赋值量 $a$ 的名称在声明中绑定，可以重命名以满足隐含的约束，即 $a$ 不应已在 $\Sigma$ 中出现。

###### **规则 (35.1e)：获取可赋值量的内容**

$$
\frac{}{
  \Gamma \vdash_{\Sigma,\ a} \text{get}[a]\ \text{ok}
}
\tag{35.1e}
$$

- **解释**：
  - 在签名 $\Sigma,\ a$ 下，命令 $\text{get}[a]$ 是正确的。

###### **规则 (35.1f)：设置可赋值量的内容**

$$
\frac{
  \Gamma \vdash_{\Sigma,\ a} e : \text{nat}
}{
  \Gamma \vdash_{\Sigma,\ a} \text{set}[a](e)\ \text{ok}
}
\tag{35.1f}
$$

- **解释**：
  - 如果在签名 $\Sigma,\ a$ 下，表达式 $e$ 的类型为 $\text{nat}$，
  - 那么命令 $\text{set}[a](e)$ 是正确的。

---

#### 35.1.2 动态语义（Dynamics）

$L\{\text{nat cmd } *\}$ 的动态语义是基于一个**内存（memory）** $\mu$ 定义的，$\mu$ 是一个有限函数，将可赋值量映射到数值。

动态语义包括以下两种判断形式：

1. **表达式求值判断**：

   - **$e\ \text{val}_{\Sigma}$**：表示相对于签名 $\Sigma$，$e$ 是一个值。
   - **$e\ \rightarrow_{\Sigma}\ e'$**：表示表达式 $e$ 逐步求值为表达式 $e'$。

2. **命令执行判断**：

   - **$m\ \| \ \mu\ \text{final}_{\Sigma}$**：状态 $m\ \| \ \mu$ 已经完全执行。
   - **$m\ \| \ \mu\ \rightarrow_{\Sigma}\ m'\ \| \ \mu'$**：状态 $m\ \| \ \mu$ 逐步执行到状态 $m'\ \| \ \mu'$。

以下是这些判断的推导规则：

##### 表达式的值规则

###### **规则 (35.2a)：封装命令是值**

$$
\frac{}{
  \text{cmd}(m)\ \text{val}_{\Sigma}
}
\tag{35.2a}
$$

- **解释**：
  - 封装的命令 $\text{cmd}(m)$ 是一个值。

##### 命令的执行规则

###### **规则 (35.3a)：返回命令的完成**

$$
\frac{
  e\ \text{val}_{\Sigma}
}{
  \text{ret}(e)\ \| \ \mu\ \text{final}_{\Sigma}
}
\tag{35.3a}
$$

- **解释**：
  - 如果 $e$ 在签名 $\Sigma$ 下是一个值，
  - 那么状态 $\text{ret}(e)\ \| \ \mu$ 已经完全执行。

###### **规则 (35.3b)：返回命令的求值**

$$
\frac{
  e\ \rightarrow_{\Sigma}\ e'
}{
  \text{ret}(e)\ \| \ \mu\ \rightarrow_{\Sigma}\ \text{ret}(e')\ \| \ \mu
}
\tag{35.3b}
$$

- **解释**：
  - 如果 $e$ 逐步求值为 $e'$，
  - 那么 $\text{ret}(e)$ 逐步求值为 $\text{ret}(e')$，内存 $\mu$ 不变。

###### **规则 (35.3c)：序列命令的表达式求值**

$$
\frac{
  e\ \rightarrow_{\Sigma}\ e'
}{
  \text{bnd}(e;\ x.m)\ \| \ \mu\ \rightarrow_{\Sigma}\ \text{bnd}(e';\ x.m)\ \| \ \mu
}
\tag{35.3c}
$$

- **解释**：
  - 如果 $e$ 逐步求值为 $e'$，
  - 那么序列命令 $\text{bnd}(e;\ x.m)$ 逐步求值为 $\text{bnd}(e';\ x.m)$，内存 $\mu$ 不变。

###### **规则 (35.3d)：序列命令的命令执行**

$$
\frac{
  e\ \text{val}_{\Sigma}
}{
  \text{bnd}(\text{cmd}(\text{ret}(e));\ x.m)\ \| \ \mu\ \rightarrow_{\Sigma}\ [e/x]m\ \| \ \mu
}
\tag{35.3d}
$$

- **解释**：
  - 如果 $e$ 是一个值，
  - 那么执行封装的命令 $\text{cmd}(\text{ret}(e))$，得到返回值 $e$，
  - 然后将 $e$ 替换到 $m$ 中的 $x$，继续执行 $m$。

###### **规则 (35.3e)：序列命令的命令求值**

$$
\frac{
  m_1\ \| \ \mu\ \rightarrow_{\Sigma}\ m_1'\ \| \ \mu'
}{
  \text{bnd}(\text{cmd}(m_1);\ x.m_2)\ \| \ \mu\ \rightarrow_{\Sigma}\ \text{bnd}(\text{cmd}(m_1');\ x.m_2)\ \| \ \mu'
}
\tag{35.3e}
$$

- **解释**：
  - 如果命令 $m_1$ 逐步执行为 $m_1'$，内存从 $\mu$ 变为 $\mu'$，
  - 那么序列命令逐步执行，更新封装的命令为 $\text{cmd}(m_1')$，内存更新为 $\mu'$。

###### **规则 (35.3f)：获取可赋值量的内容**

$$
\frac{}{
  \text{get}[a]\ \| \ \mu \otimes a \mapsto e\ \rightarrow_{\Sigma,\ a}\ \text{ret}(e)\ \| \ \mu \otimes a \mapsto e
}
\tag{35.3f}
$$

- **解释**：
  - 获取可赋值量 $a$ 的内容 $e$，并返回 $e$。

###### **规则 (35.3g)：设置命令的表达式求值**

$$
\frac{
  e\ \rightarrow_{\Sigma}\ e'
}{
  \text{set}[a](e)\ \| \ \mu\ \rightarrow_{\Sigma}\ \text{set}[a](e')\ \| \ \mu
}
\tag{35.3g}
$$

- **解释**：
  - 如果 $e$ 逐步求值为 $e'$，
  - 那么 $\text{set}[a](e)$ 逐步求值为 $\text{set}[a](e')$，内存 $\mu$ 不变。

###### **规则 (35.3h)：设置可赋值量的内容**

$$
\frac{
  e\ \text{val}_{\Sigma}
}{
  \text{set}[a](e)\ \| \ \mu \otimes a \mapsto \_\ \rightarrow_{\Sigma}\ \text{ret}(e)\ \| \ \mu \otimes a \mapsto e
}
\tag{35.3h}
$$

- **解释**：
  - 如果 $e$ 是一个值，
  - 那么将 $e$ 赋给可赋值量 $a$，并返回 $e$。

###### **规则 (35.3i)：声明命令的表达式求值**

$$
\frac{
  e\ \rightarrow_{\Sigma}\ e'
}{
  \text{dcl}(e;\ a.m)\ \| \ \mu\ \rightarrow_{\Sigma}\ \text{dcl}(e';\ a.m)\ \| \ \mu
}
\tag{35.3i}
$$

- **解释**：
  - 如果 $e$ 逐步求值为 $e'$，
  - 那么声明命令逐步求值为 $\text{dcl}(e';\ a.m)$，内存 $\mu$ 不变。

###### **规则 (35.3j)：声明命令的命令求值**

$$
\frac{
  e\ \text{val}_{\Sigma} \quad m\ \| \ \mu \otimes a \mapsto e\ \rightarrow_{\Sigma,\ a}\ m'\ \| \ \mu' \otimes a \mapsto e_0
}{
  \text{dcl}(e;\ a.m)\ \| \ \mu\ \rightarrow_{\Sigma}\ \text{dcl}(e;\ a.m')\ \| \ \mu'
}
\tag{35.3j}
$$

- **解释**：
  - 如果 $e$ 是一个值，
  - 在扩展的内存 $\mu \otimes a \mapsto e$ 下，命令 $m$ 逐步执行为 $m'$，内存变为 $\mu' \otimes a \mapsto e_0$，
  - 那么声明命令逐步执行为 $\text{dcl}(e;\ a.m')$，内存更新为 $\mu'$。

###### **规则 (35.3k)：声明命令的完成**

$$
\frac{
  e\ \text{val}_{\Sigma} \quad e_0\ \text{val}_{\Sigma,\ a}
}{
  \text{dcl}(e;\ a.\text{ret}(e_0))\ \| \ \mu\ \rightarrow_{\Sigma}\ \text{ret}(e_0)\ \| \ \mu
}
\tag{35.3k}
$$

- **解释**：
  - 如果 $e$ 和 $e_0$ 都是值，
  - 那么声明命令执行完毕，返回 $e_0$，内存恢复为 $\mu$。

---

#### 35.1.3 安全性（Safety）

定义判断 $m\ \| \ \mu\ \text{ok}_{\Sigma}$ 如下：

###### **规则 (35.4)：命令状态的正确性**

$$
\frac{
  \vdash_{\Sigma} m\ \text{ok} \quad \mu : \Sigma
}{
  m\ \| \ \mu\ \text{ok}_{\Sigma}
}
\tag{35.4}
$$

- **解释**：
  - 如果命令 $m$ 在签名 $\Sigma$ 下是正确的（$\vdash_{\Sigma} m\ \text{ok}$），
  - 并且内存 $\mu$ 与签名 $\Sigma$ 一致（$\mu : \Sigma$），
  - 那么状态 $m\ \| \ \mu$ 是正确的。

其中，辅助判断 $\mu : \Sigma$ 定义如下：

###### **规则 (35.5)：内存与签名的一致性**

$$
\frac{
  \forall a \in \Sigma\quad \exists e\quad \mu(a) = e \quad \text{且}\ e\ \text{val}_{\emptyset} \quad \text{且}\ \vdash_{\emptyset} e : \text{nat}
}{
  \mu : \Sigma
}
\tag{35.5}
$$

- **解释**：
  - 对于 $\Sigma$ 中的每个可赋值量 $a$，内存 $\mu$ 都绑定了一个值 $e$，
  - 并且 $e$ 在空签名下是一个值，类型为 $\text{nat}$，
  - 那么内存 $\mu$ 与签名 $\Sigma$ 一致。

---

#### 定理 35.1（保持性，Preservation）

1. **表达式的保持性**：如果 $e\ \rightarrow_{\Sigma}\ e'$ 且 $\vdash_{\Sigma} e : \tau$，那么 $\vdash_{\Sigma} e' : \tau$。

2. **命令的保持性**：如果 $m\ \| \ \mu\ \rightarrow_{\Sigma}\ m'\ \| \ \mu'$，且 $\vdash_{\Sigma} m\ \text{ok}$ 且 $\mu : \Sigma$，那么 $\vdash_{\Sigma} m'\ \text{ok}$ 且 $\mu' : \Sigma$。

**证明**：

- 通过对规则 (35.2) 和 (35.3) 的归纳证明。

- **示例**：

  - 考虑规则 (35.3j)：

    - 假设 $\vdash_{\Sigma} \text{dcl}(e;\ a.m)\ \text{ok}$ 且 $\mu : \Sigma$。
    - 通过类型判断的逆推，我们有 $\vdash_{\Sigma} e : \text{nat}$ 且 $\vdash_{\Sigma,\ a} m\ \text{ok}$。
    - 因为 $e\ \text{val}_{\Sigma}$ 且 $\mu : \Sigma$，因此 $\mu \otimes a \mapsto e : \Sigma,\ a$。
    - 通过归纳假设，我们有 $\vdash_{\Sigma,\ a} m'\ \text{ok}$ 且 $\mu' \otimes a \mapsto e : \Sigma,\ a$。
    - 因此，结论成立。

---

#### 定理 35.2（前进性，Progress）

1. **表达式的前进性**：如果 $\vdash_{\Sigma} e : \tau$，那么要么 $e\ \text{val}_{\Sigma}$，要么存在 $e'$ 使得 $e\ \rightarrow_{\Sigma}\ e'$。

2. **命令的前进性**：如果 $\vdash_{\Sigma} m\ \text{ok}$ 且 $\mu : \Sigma$，那么要么 $m\ \| \ \mu\ \text{final}_{\Sigma}$，要么存在 $m'$ 和 $\mu'$ 使得 $m\ \| \ \mu\ \rightarrow_{\Sigma}\ m'\ \| \ \mu'$。

**证明**：

- 通过对规则 (35.1) 的归纳证明。

- **示例**：

  - 考虑规则 (35.1d)：

    - 通过归纳假设，要么 $e\ \rightarrow_{\Sigma}\ e'$，要么 $e\ \text{val}_{\Sigma}$。
    - 在前一种情况下，应用规则 (35.3i)。
    - 在后一种情况下，通过第二个归纳假设，要么 $m\ \| \ \mu \otimes a \mapsto e\ \text{final}_{\Sigma,\ a}$，要么 $m\ \| \ \mu \otimes a \mapsto e\ \rightarrow_{\Sigma,\ a}\ m'\ \| \ \mu' \otimes a \mapsto e_0$。
    - 在前一种情况下，应用规则 (35.3k)。
    - 在后一种情况下，应用规则 (35.3j)。

---

### 总结

---

在本节中，我们详细介绍了现代化的 Algol 的基本命令，包括其语法、静态语义、动态语义以及安全性。

- **语法**：区分了纯表达式和命令，引入了命令的各种构造，如返回、序列、声明、新可赋值量的获取和设置等。

- **静态语义**：通过类型判断和命令形成判断，确保程序的类型正确性。

- **动态语义**：定义了表达式和命令的求值规则，以及内存状态的变化。

- **安全性**：通过保持性和前进性定理，证明了语言的类型安全性。

理解这些内容有助于我们深入掌握命令式编程语言的设计原理，特别是在类型系统、内存管理和程序执行模型方面的知识。

### ---------------------------------

### 第35章 现代化的 Algol

---

在前面的章节中，我们介绍了现代化的 Algol 语言 $L\{\text{nat cmd } *\}$ 的基本命令。本节将展示如何在 $L\{\text{nat cmd } *\}$ 中推导出一些命令式编程的标准惯用法（idioms）。这些惯用法展示了表达式的求值与命令对可赋值量（assignables）的影响之间的优雅互动。

---

### 35.2 一些编程惯用法（Some Programming Idioms）

#### 1. 顺序组合（Sequential Composition）

**定义**：

- **顺序组合命令**：记作 $\{ x \leftarrow m_1;\ m_2 \}$，表示命令 $bnd\ x \leftarrow \text{cmd}(m_1);\ m_2$。
  
  形式化地，顺序组合可以定义为：

  $$
  \{ x \leftarrow m_1;\ m_2 \} \triangleq \text{bnd}( \text{cmd}(m_1);\ x.m_2 )
  $$

- **n 元形式的顺序组合**：

  $$
  \{ x_1 \leftarrow m_1;\ \dotsb;\ x_{n-1} \leftarrow m_{n-1};\ m_n \}
  $$

  被定义为迭代的组合：

  $$
  \{ x_1 \leftarrow m_1;\ \{ x_2 \leftarrow m_2;\ \dotsb;\ \{ x_{n-1} \leftarrow m_{n-1};\ m_n \} \dotsb \} \}
  $$

**简写**：

- 如果忽略返回值，我们可以写成：

  $$
  \{ m_1;\ m_2 \} \triangleq \{ \ \leftarrow m_1;\ m_2 \}
  $$

  其中返回值被忽略。这种形式可以推广到 n 元的组合。

**解释**：

- **顺序组合**允许我们依次执行多个命令，将前一个命令的返回值传递给下一个命令。
- 这种机制在命令式编程中非常常见，有助于编写结构化的程序。

---

#### 2. 执行封装命令的惯用法（do Idiom）

**定义**：

- **命令 $\text{do}\ e$**：执行一个封装的命令并返回其结果。
  
  定义为：

  $$
  \text{do}\ e \triangleq \text{bnd}(e;\ x.\ \text{ret}\ x)
  $$

**解释**：

- 这个惯用法用于执行一个表达式 $e$，其中 $e$ 是一个封装的命令，执行后返回其结果。

---

#### 3. 条件命令（Conditional Command）

**定义**：

- **条件命令**：$\text{if}\ (m)\ m_1\ \text{else}\ m_2$，根据执行命令 $m$ 的结果是否为零，执行 $m_1$ 或 $m_2$。

  定义为：

  $$
  \{ x \leftarrow m;\ \text{do}\ (\text{ifz}\ x\ \{ z \Rightarrow \text{cmd}\ m_1\ |\ s(\_ ) \Rightarrow \text{cmd}\ m_2 \}) \}
  $$

  其中 $\text{ifz}$ 是用于检查零值的表达式，形式为：

  $$
  \text{ifz}\ e\ \{ z \Rightarrow e_1\ |\ s(y) \Rightarrow e_2 \}
  $$

**解释**：

- **流程**：
  - 执行命令 $m$，得到结果 $x$。
  - 根据 $x$ 是否为零，选择执行命令 $m_1$ 或 $m_2$。
- **返回值**：
  - 条件命令的返回值是所选择执行的命令的返回值。

---

#### 4. While 循环命令（While Loop Command）

**定义**：

- **While 循环命令**：$\text{while}\ (m_1)\ m_2$，当命令 $m_1$ 返回非零数时，重复执行命令 $m_2$。

  定义为：

  $$
  \text{do}\ (\text{fix}\ \text{loop}:\text{cmd}\ \text{is}\ \text{cmd}\ (\text{if}\ (m_1)\ \{ \text{ret}\ z \}\ \text{else}\ \{ m_2;\ \text{do}\ \text{loop} \}))
  $$

  - **解释**：
    - 使用了递归定义 $\text{loop}$，类型为 $\text{cmd}$。
    - **$\text{fix}$**：固定点组合子，用于定义递归函数或命令。

**流程**：

- 执行封装的自引用命令 $\text{loop}$。
- **步骤**：
  1. 执行命令 $m_1$，检查结果。
  2. 如果结果为零，返回零（可以是任意值）。
  3. 如果结果为非零，执行命令 $m_2$，然后再次执行 $\text{loop}$。

---

#### 5. 过程（Procedure）

**定义**：

- **过程**是类型为 $\tau \rightarrow \text{cmd}$ 的函数，接受类型为 $\tau$ 的参数，返回一个未执行的命令。

  - **一般形式**：$\lambda\ (x:\tau)\ \text{cmd}\ m$。

  - 简写为：$\text{proc}\ (x:\tau)\ m$。

- **过程调用**：

  - **调用语法**：$\text{call}\ e_1(e_2)$。

  - 定义为：

    $$
    \text{call}\ e_1(e_2) \triangleq \text{do}\ (e_1(e_2))
    $$

  - **解释**：
    - 首先计算 $e_1(e_2)$，得到一个命令。
    - 然后立即执行该命令，返回结果。

---

#### 6. 计算阶乘的过程示例

**代码**：

```pseudo
proc (x : nat) {
  dcl r := 1 in
  dcl a := x in
  {
    while (@ a) {
      y ← @ r;
      z ← @ a;
      r := (x - z + 1) × y;
      a := z - 1
    };
    @ r
  }
}
```

**解释**：

- **过程定义**：

  - 定义了一个过程，接受一个自然数 $x$，返回一个命令，该命令计算 $x$ 的阶乘。

- **变量声明**：

  - **$r$**：初始化为 $1$，用于存储计算结果。
  - **$a$**：初始化为 $x$，用于控制循环次数。

- **循环体**：

  - **条件**：当 $@ a$ 非零时，继续循环。
    - **$@ a$**：获取可赋值量 $a$ 的当前值。
  - **步骤**：
    1. **$y \leftarrow @ r$**：获取 $r$ 的当前值，赋给 $y$。
    2. **$z \leftarrow @ a$**：获取 $a$ 的当前值，赋给 $z$。
    3. **$r := (x - z + 1) \times y$**：更新 $r$，计算当前阶乘值。
    4. **$a := z - 1$**：将 $a$ 减一，准备下一次循环。
  - **循环不变式**：
    - **$r$ 的值始终等于 $x$ 减去 $a$ 的值的阶乘**。
    - 初始时，$r = 1$，$a = x$，因此 $r = (x - x + 1)! = 1! = 1$。
    - 每次迭代都保持该不变式，直到 $a = 0$。

- **返回值**：

  - 当循环结束（$a = 0$）时，返回 $@ r$，即计算出的阶乘值。

---

**详细解析**：

- **初始状态**：

  - $r = 1$
  - $a = x$

- **循环执行过程**：

  - **第一次迭代**：
    - $y = r = 1$
    - $z = a = x$
    - $r := (x - z + 1) \times y = (x - x + 1) \times 1 = 1 \times 1 = 1$
    - $a := z - 1 = x - 1$
  - **第二次迭代**：
    - $y = r = 1$
    - $z = a = x - 1$
    - $r := (x - (x - 1) + 1) \times y = (1 + 1) \times 1 = 2 \times 1 = 2$
    - $a := (x - 1) - 1 = x - 2$
  - **第三次迭代**：
    - $y = r = 2$
    - $z = a = x - 2$
    - $r := (x - (x - 2) + 1) \times y = (2 + 1) \times 2 = 3 \times 2 = 6$
    - $a := (x - 2) - 1 = x - 3$
  - **持续迭代**，直到 $a = 0$。

- **循环终止条件**：

  - 当 $a = 0$ 时，循环结束。
  - 此时，$r$ 的值为 $x!$。

---

**总结**：

- 这个示例展示了如何在 $L\{\text{nat cmd } *\}$ 中编写一个计算阶乘的过程，利用了命令式编程的特性。
- **关键点**：
  - 使用可赋值量来存储和更新状态。
  - 使用循环和条件命令来控制流程。
  - 通过维护循环不变式，确保算法的正确性。

---

### 练习与思考

**练习 1：理解顺序组合**

- **题目**：解释以下命令的执行流程，并给出返回值。

  ```pseudo
  { x ← ret(5); y ← ret(x + 2); ret(y * 3) }
  ```

- **解答**：

  1. **执行 $x \leftarrow \text{ret}(5)$**：
     - 返回值 $5$，赋给 $x$。
  2. **执行 $y \leftarrow \text{ret}(x + 2)$**：
     - 计算 $x + 2 = 5 + 2 = 7$，返回值 $7$，赋给 $y$。
  3. **执行 $\text{ret}(y * 3)$**：
     - 计算 $y * 3 = 7 * 3 = 21$，返回值 $21$。

- **返回值**：$21$

**练习 2：编写一个计算斐波那契数列的过程**

- **题目**：使用 $L\{\text{nat cmd } *\}$ 编写一个过程，计算给定自然数 $n$ 的斐波那契数 $F(n)$。

- **提示**：

  - 使用两个可赋值量存储前两个斐波那契数。
  - 使用循环更新这两个值。

- **解答**：

  ```pseudo
  proc (n : nat) {
    dcl a := 0 in
    dcl b := 1 in
    dcl i := n in
    {
      while (@ i) {
        temp ← @ b;
        b := @ a + @ b;
        a := temp;
        i := @ i - 1
      };
      @ a
    }
  }
  ```

- **解释**：

  - **初始状态**：
    - $a = 0$
    - $b = 1$
    - $i = n$
  - **循环过程**：
    - 当 $i > 0$，执行：
      1. $temp \leftarrow b$
      2. $b := a + b$
      3. $a := temp$
      4. $i := i - 1$
  - **返回值**：当 $i = 0$ 时，返回 $a$，即 $F(n)$。

---

### 总结

- 本节介绍了在现代化的 Algol 语言中实现常见命令式编程惯用法的方法。
- 通过定义顺序组合、条件命令、循环以及过程，我们可以编写复杂的程序，并利用类型系统和语法的特性来保证程序的正确性和安全性。
- 这些惯用法展示了表达式求值与命令执行之间的交互，以及如何利用可赋值量和命令来实现算法。

---

### 进一步思考

- **类型系统的扩展**：如何在 $L\{\text{nat cmd } *\}$ 中支持更多的数据类型，如布尔类型、复合类型等？
- **错误处理**：如何在命令中引入异常处理机制，确保程序的鲁棒性？
- **优化与性能**：在实际编译器中，如何优化这些命令的执行，提高程序的性能？

---

通过对这些问题的思考，我们可以更深入地理解命令式编程语言的设计和实现原理。


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------


### ---------------------------------