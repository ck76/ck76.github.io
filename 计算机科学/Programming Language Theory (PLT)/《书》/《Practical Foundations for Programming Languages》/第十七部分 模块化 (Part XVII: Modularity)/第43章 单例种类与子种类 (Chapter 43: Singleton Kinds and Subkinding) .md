[toc]



### 43. 单元素类型（Singleton Kinds）和子类型划分（Subkinding）

在这一节中，作者讨论了如何通过类型别名和类型变量绑定来增强类型系统的表达能力。核心问题是如何在表达式的范围内绑定类型变量，使得类型系统可以更好地处理类型别名和抽象。

### 1. **表达式级的 `let` 语法**

通常我们会用 `let e1 : τ be x in e2` 来绑定一个表达式的值到一个变量，并在表达式 $e2$ 的范围内使用这个变量 $x$。这是一个常见的缩略机制，用于简化代码中的重复表达式。

在函数类型中，类似的绑定也可以通过应用 $\lambda$-抽象来实现：
$$
(\lambda (x : \tau) e2)(e1)
$$

这段代码的作用与 `let e1 : τ be x in e2` 相同，都是将表达式 $e1$ 绑定到变量 $x$，并在 $e2$ 中使用它。

### 2. **类型级的 `let` 语法**

基于表达式级的 `let` 机制，我们可以类比地引入类型级别的 `let`，来在类型系统中进行类似的绑定操作。例如：

```plaintext
def t is nat × nat in λ (x : t) s(x · l)
```

在这个表达式中，我们引入了一个类型别名 $t$，它代表类型 $nat \times nat$，并且我们可以在函数体 $\lambda (x : t) s(x · l)$ 中使用这个类型别名。

### 3. **类型定义的困难**

按照表达式级 `let` 绑定的模式，类型定义可以被认为是多态实例化的一种形式，如：
$$
\Lambda (t) e[\tau]
$$

这意味着类型变量 $t$ 被绑定到类型 $\tau$，并在表达式 $e$ 中使用该类型。然而，这种解释方式的问题在于，它要求表达式 $e$ 在不知晓 $t$ 具体是什么类型的情况下进行类型检查，而这显然是不符合预期的。

在上面的例子中，如果我们不知道 $t = nat \times nat$，则 $\lambda (x : t) s(x \cdot l)$ 中的类型检查就会失败。因此，类型定义的动态绑定机制不能直接采用多态抽象和类型应用的方式，需要引入新的方法来处理类型别名的静态绑定。

### 4. **引入单元素类型（Singleton Kinds）**

一种解决方案是引入**单元素类型（singleton kinds）**，即通过单元素类型来将类型构造器与其具体的定义联系起来。单元素类型揭示了类型构造器的身份，从而解决了类型定义中的问题。

单元素类型不仅可以解决类型定义的问题，还在模块系统的设计中发挥着重要作用（将在第44和45章中进一步讨论）。

#### **公式表示：**
为了使类型定义和表达式保持一致，作者提出了以下形式的类型规则：
$$
\frac{\Gamma[\tau/t] \vdash e : \tau'}{\Gamma \vdash \text{def} \, t \, \text{is} \, \tau \, \text{in} \, e : \tau'}
$$

- 这条规则表示：在上下文 $\Gamma$ 下，将类型 $\tau$ 绑定到类型变量 $t$，并在表达式 $e$ 中使用该绑定，最终结果的类型为 $\tau'$。

### 5. **子类型划分（Subkinding）**

子类型划分是一种用于组织和分类类型构造器的技术。通过将类型构造器分为不同的层次（子类型），可以提高类型系统的表达能力和灵活性。子类型划分为类型系统引入了一种**层级结构**，使得某些类型可以被视为其他类型的特化版本。

在引入单元素类型之后，子类型划分的概念自然地适用于更复杂的类型系统，允许类型系统能够灵活地处理各种抽象、模块化以及多态性需求。

### 6. **总结**

- **类型别名（Type Abbreviation）**：引入类型级别的 `let` 语法，使得可以在范围内绑定类型变量为某个具体类型，实现类型别名的功能。
- **单元素类型（Singleton Kinds）**：通过引入单元素类型来分类类型构造器，使得类型定义更具表达力。
- **子类型划分（Subkinding）**：通过将类型构造器进行子类型划分，增强了类型系统的灵活性和模块化设计能力。

如果你对这些概念或规则有进一步的问题或需要更多详细的解释，请告诉我！

### ---------------------------------

### 43.1 概述

在这一节中，作者讨论了**类型理论（type theory）**的核心原则——**组合性（compositionality）**，并引入了**单元素类型（singleton kinds）**的概念来解决类型别名和类型抽象的问题。单元素类型使得类型系统可以更灵活地处理类型绑定，并保持类型的组成部分之间的独立性。

### 1. **组合性原则（Compositionality Principle）**

**组合性**是类型理论的核心组织原则。为了确保程序可以被分解成独立的部分，程序的组合由这些部分的类型来调控。换句话说，一个程序的部分只知道另一部分的类型，而不是它的具体形式或值。

例如，两个自然数的加法规则仅依赖于它们的类型（即 $nat$），而不依赖于它们的具体值。类型提供了程序各部分之间相互作用的接口，使得我们可以在不知道值的情况下推理程序的行为。

然而，当我们处理类型别名时，比如 `def t is τ in e`，我们希望类型变量 $t$ 在 $e$ 中表现为类型 $\tau$。根据组合性原则，表达式 $e$ 只应该知道类型变量 $t$ 的**种类（kind）**，即 $T$，而不是它的具体绑定 $\tau$。

### 2. **类型抽象和应用的限制**

使用类型抽象（type abstraction）和类型应用（type application）来处理类型别名的问题在于它无法正确传播类型变量 $t$ 的身份信息。例如，以下表示：
$$
\Lambda (t) e[\tau]
$$
并不能在 $e$ 中正确地让 $t$ 与 $\tau$ 绑定，导致类型检查时出错。

一种可能的解决方案是将类型别名视为一个原始概念，但这会违背类型理论的核心原则。因此，作者引入了**单元素类型（singleton kinds）**，来捕捉类型变量 $t$ 的身份信息。

### 3. **单元素类型（Singleton Kinds）**

**单元素类型**的概念是，类型变量 $t$ 的种类不仅仅是一个普通的类型种类 $T$，而是一个捕捉类型具体身份的**单元素种类**。单元素种类 $S(\tau)$ 表示类型与 $\tau$ 定义性相等（definitional equality），即 $S(\tau)$ 中只有一个成员，它就是 $\tau$ 本身。

如果一个变量 $u$ 的种类是 $S(\tau)$，那么在其作用范围内，$u$ 就与类型 $\tau$ 同义。这使得我们可以使用以下形式来表示类型别名：
$$
\Lambda (t :: S(\tau)) e[\tau]
$$
这种表示确保了 $t$ 在类型检查期间与 $\tau$ 绑定，从而正确传播了类型的身份信息。

### 4. **单元素类型的形式化与子类型划分（Subkinding）**

为了形式化单元素类型，作者引入了一些新的机制在**构造器（constructor）**和**种类（kind）**层面。

首先，**单元素种类（singleton kind）**是类型构造器的一种特殊形式，它属于种类 $T$。为了确保这一点，我们引入了**子类型划分（subkinding）**的关系，表示种类之间的层次关系。基本的子类型划分公理为：
$$
S(\tau) <:: T
$$
这意味着每个单元素种类 $S(\tau)$ 都是 $T$ 的子种类，从而允许单元素种类的构造器可以作为类型使用。

其次，单元素种类是**依赖种类（dependent kind）**，其含义依赖于某个构造器。例如，$S(\tau)$ 是一个由构造器 $T$ 索引的种类族。

#### **依赖积（Dependent Product）和依赖函数（Dependent Function）**

为了支持这种依赖关系，作者将**积类型（product types）**和**函数类型（function types）**推广为依赖积和依赖函数。

- **依赖积种类（Dependent Product Kind）**：
  $$
  \Pi u :: \kappa_1.\kappa_2
  $$
  表示由构造器 $c_1 :: \kappa_1$ 和 $c_2 :: [c_1/u] \kappa_2$ 组成的对，其中第二个成分的种类依赖于第一个成分的具体值，而不仅仅是它的种类。

- **依赖函数种类（Dependent Function Kind）**：
  $$
  \lambda u :: \kappa_1.\kappa_2
  $$
  表示一个函数，当应用于构造器 $c_1 :: \kappa_1$ 时，结果是一个种类为 $[c_1/u] \kappa_2$ 的构造器。

这些依赖种类使得类型系统可以处理更复杂的类型依赖关系，而不仅仅是简单的种类检查。

### 5. **高阶单元素类型（Higher-Order Singleton Kinds）**

作者进一步推广了单元素类型的概念，引入了**高阶单元素类型（higher-order singleton kinds）**。高阶单元素类型的形式为：
$$
S(c :: \kappa)
$$
其中 $\kappa$ 是一个种类，$c$ 是 $\kappa$ 的一个构造器。高阶单元素类型可以用依赖函数和依赖积的形式来定义。

### 6. **总结**

- **组合性原则**：程序的不同部分之间只能通过类型进行相互作用，这确保了程序的可分解性。
- **单元素类型（Singleton Kinds）**：通过单元素种类捕捉类型构造器的身份信息，使得类型变量与其具体绑定正确传播。
- **子类型划分（Subkinding）**：通过定义种类之间的层次关系，允许类型构造器在不同的种类之间灵活转换。
- **依赖积和依赖函数**：推广了积类型和函数类型，使得类型系统可以处理更复杂的依赖关系。

如果你对这些概念有更多问题或需要进一步的详细解释，请告诉我！

### ---------------------------------

### 43.2 单元素类型（Singletons）

**单元素种类（Singleton kind）**的形式为 $S(c)$，其中 $c$ 是一个构造器（constructor）。单元素种类用于分类所有与构造器 $c$ 等价的构造器。在这一节中，我们讨论单元素种类在 **Fω 语言**（详见第18章）中的应用，**Fω 语言** 包含类型种类（kind of types），并且在积类型和函数类型下是封闭的。第43.3节将扩展种类的语言，使得 **Fω** 的积类型和函数类型是可定义的。

#### 1. **单元素种类的静态语法（Statics of Singletons）**

单元素种类的静态语法基于以下判断形式：
- **$\Gamma \vdash \kappa$**：种类形成（kind formation），表示 $\kappa$ 是一个种类。
- **$\Gamma \vdash \kappa_1 \equiv \kappa_2$**：种类等价（kind equivalence），表示 $\kappa_1$ 和 $\kappa_2$ 是等价的种类。
- **$\Gamma \vdash c :: \kappa$**：构造器形成（constructor formation），表示 $c$ 是种类 $\kappa$ 下的一个构造器。
- **$\Gamma \vdash c_1 \equiv c_2 :: \kappa$**：构造器等价（constructor equivalence），表示 $c_1$ 和 $c_2$ 在种类 $\kappa$ 下是等价的构造器。
- **$\Gamma \vdash \kappa_1 <:: \kappa_2$**：子种类（subkinding），表示 $\kappa_1$ 是 $\kappa_2$ 的子种类。

这些判断通过一系列规则定义，下面列出了一些重要的规则。

#### 2. **规则详解**

1. **类型构造器形成规则**：
   $$
   \frac{}{\Gamma \vdash c :: \text{Type}}
   $$
   该规则表示在没有其他前提的情况下，构造器 $c$ 属于种类 $\text{Type}$。

2. **单元素种类形成规则**：
   $$
   \frac{\Gamma \vdash c :: \text{Type}}{\Gamma \vdash S(c) \text{kind}} \tag{43.2a}
   $$
   - 解释：如果构造器 $c$ 属于种类 $\text{Type}$，那么单元素种类 $S(c)$ 是一个有效的种类。

3. **单元素种类的构造器形成规则**：
   $$
   \frac{\Gamma \vdash c :: \text{Type}}{\Gamma \vdash c :: S(c)} \tag{43.2b}
   $$
   - 解释：如果 $c$ 属于种类 $\text{Type}$，那么它也是单元素种类 $S(c)$ 的构造器。此规则表明 $c$ 自身属于它的单元素种类。

4. **构造器等价规则**：
   $$
   \frac{\Gamma \vdash c :: S(d)}{\Gamma \vdash c \equiv d :: \text{Type}} \tag{43.2c}
   $$
   - 解释：如果构造器 $c$ 属于单元素种类 $S(d)$，则 $c$ 和 $d$ 在 $\text{Type}$ 种类下等价。这表明单元素种类中所有构造器都与 $d$ 相等。

5. **子种类规则**：
   $$
   \frac{\Gamma \vdash c :: \kappa_1 \quad \Gamma \vdash \kappa_1 <:: \kappa_2}{\Gamma \vdash c :: \kappa_2} \tag{43.2d}
   $$
   - 解释：如果 $c$ 属于种类 $\kappa_1$，且 $\kappa_1$ 是 $\kappa_2$ 的子种类，那么 $c$ 也属于 $\kappa_2$。

6. **单元素种类的子种类规则**：
   $$
   \frac{\Gamma \vdash c :: \text{Type}}{\Gamma \vdash S(c) <:: \text{Type}} \tag{43.2e}
   $$
   - 解释：如果构造器 $c$ 属于种类 $\text{Type}$，那么单元素种类 $S(c)$ 是 $\text{Type}$ 的子种类。这条规则确保单元素种类仍然是类型种类的一部分。

7. **单元素种类等价规则**：
   $$
   \frac{\Gamma \vdash c \equiv d :: \text{Type}}{\Gamma \vdash S(c) \equiv S(d)} \tag{43.2f}
   $$
   - 解释：如果 $c$ 和 $d$ 在 $\text{Type}$ 下等价，那么单元素种类 $S(c)$ 和 $S(d)$ 也等价。

8. **子种类的传递性规则**：
   $$
   \frac{\Gamma \vdash \kappa_1 <:: \kappa_2 \quad \Gamma \vdash \kappa_2 <:: \kappa_3}{\Gamma \vdash \kappa_1 <:: \kappa_3} \tag{43.2h}
   $$
   - 解释：如果 $\kappa_1$ 是 $\kappa_2$ 的子种类，且 $\kappa_2$ 是 $\kappa_3$ 的子种类，那么 $\kappa_1$ 是 $\kappa_3$ 的子种类。这确保了种类之间的层次关系是传递的。

#### 3. **总结**

单元素种类通过引入种类中构造器的身份来增强类型系统的表达能力。通过这些规则，单元素种类不仅能够捕捉构造器的等价关系，还能确保种类之间的层次结构。

如果你对这些规则和公式有更多问题或需要更详细的解释，请告诉我！

### ---------------------------------

### 解释与公式详解：单元素种类（Singleton Kinds）

单元素种类的核心思想是构造器 $c$ 与它的单元素种类 $S(c)$ 的等价性。这些规则在构造器和类型系统中起到了至关重要的作用。

#### 1. **规则 43.2b：自识别原则（Self-Recognition Principle）**

规则 (43.2b) 表示**自识别（self-recognition）**的原则，表明任意构造器 $c$ 的种类是 $\text{Type}$，同时它也属于单元素种类 $S(c)$。形式化表示为：
$$
\frac{\Gamma \vdash c :: \text{Type}}{\Gamma \vdash c :: S(c)} \tag{43.2b}
$$

- **解释**：构造器 $c$ 自身不仅是类型种类的一部分，而且也是其单元素种类 $S(c)$ 的成员。这揭示了构造器等价性的**自反性（reflexivity）**，即构造器 $c$ 与自己等价。

#### 2. **规则 43.2c：单元素种类内的构造器等价**

规则 (43.2c) 指出，任何属于单元素种类 $S(c)$ 的构造器都与 $c$ 定义性相等（definitional equality）。形式化表示为：
$$
\frac{\Gamma \vdash c :: S(d)}{\Gamma \vdash c \equiv d :: \text{Type}} \tag{43.2c}
$$

- **解释**：如果 $c$ 属于单元素种类 $S(d)$，那么 $c$ 和 $d$ 在 $\text{Type}$ 种类下是等价的。换句话说，单元素种类 $S(c)$ 中的构造器与 $c$ 本身是等价的。

#### 3. **规则 43.2e：子类型划分中的单元素种类**

规则 (43.2e) 将**子类型划分原则（subsumption principle）**应用到构造器和种类的层面：
$$
\frac{\Gamma \vdash c :: \text{Type}}{\Gamma \vdash S(c) <:: \text{Type}} \tag{43.2e}
$$

- **解释**：如果构造器 $c$ 属于 $\text{Type}$ 种类，那么单元素种类 $S(c)$ 是 $\text{Type}$ 的子种类。这意味着单元素种类不单独存在，而是作为类型种类的一部分。

#### 4. **规则 43.2f：单元素种类的等价性**

规则 (43.2f) 表示单元素种类之间尊重它们的构造器的等价性：
$$
\frac{\Gamma \vdash c \equiv d :: \text{Type}}{\Gamma \vdash S(c) \equiv S(d)} \tag{43.2f}
$$

- **解释**：如果 $c$ 和 $d$ 在 $\text{Type}$ 下是等价的，那么 $S(c)$ 和 $S(d)$ 这两个单元素种类也必须是等价的。

#### 5. **例子：单元素种类的变量行为**

现在，让我们通过例子来理解这些规则的应用。假设 $u$ 是种类 $S(c)$ 下的一个变量：
$$
\Gamma \vdash u :: S(c)
$$

根据规则 (43.2c)，我们可以推导出 $u$ 与 $c$ 在 $\text{Type}$ 下是等价的：
$$
\Gamma \vdash u \equiv c :: \text{Type}
$$

- **解释**：这意味着在声明 $u$ 时，$u$ 被定义为与构造器 $c$ 等价的构造器。因此，$u$ 的种类 $S(c)$ 实际上固定了 $u$ 是 $c$ 本身。

#### 6. **存在类型与单元素种类**

考虑**存在类型（existential type）**的例子。存在类型 $\exists u :: S(c). \tau$ 是一种表示其表示类型等价于 $c$ 的包类型（package type）。它是一种抽象类型，通过赋予其单元素种类来揭示它的身份。

通过一般的等价性原则，我们有：
$$
\exists u :: S(c). \tau \equiv \exists :: S(c).[c/u] \tau
$$

- **解释**：我们将 $u$ 和 $c$ 的等价性传播到类型 $\tau$ 中。这种传播确保了类型变量 $u$ 与构造器 $c$ 的绑定。

#### 7. **“遗忘”单元素种类的定义**

我们也可以通过子类型推导“遗忘”单元素种类的定义。通过以下的存在类型的变型规则，我们可以推导：
$$
\frac{\Gamma \vdash \kappa_1 <:: \kappa_2 \quad \Gamma, u :: \kappa_1 \vdash \tau_1 <:: \tau_2}{\Gamma \vdash \exists u :: \kappa_1. \tau_1 <:: \exists u :: \kappa_2. \tau_2} \tag{43.3}
$$

这表示：
$$
\exists u :: S(c). \tau <: \exists u :: \text{Type}. \tau
$$

- **解释**：我们可以通过“遗忘” $u$ 的定义，使得存在类型的定义更为宽泛。

#### 8. **全称类型与单元素种类**

同样，我们可以从全称类型的变型规则推导出：
$$
\frac{\Gamma \vdash \kappa_2 <:: \kappa_1 \quad \Gamma, u :: \kappa_2 \vdash \tau_1 <:: \tau_2}{\Gamma \vdash \forall u :: \kappa_1. \tau_1 <:: \forall u :: \kappa_2. \tau_2} \tag{43.4}
$$

这表示：
$$
\forall u :: \text{Type}. \tau <: \forall u :: S(c). \tau
$$

- **解释**：这表示一个可以应用于任何类型的多态函数，同样可以应用于某个特定类型 $c$。

#### 9. **总结**

这些例子表明，**单元素种类**提供了一种**类型变量的作用域定义**方式，而不依赖于任意的定义机制，而是自然地从绑定和作用域的原则中产生。在之后的章节（44 和 45 章）中，单元素种类将被用来管理程序模块之间的交互。

通过以上规则和推导，我们看到单元素种类不仅仅是一个语法上的抽象概念，而是一个与类型系统的等价性和子类型划分密切相关的工具。

如果你对这些规则、公式或例子有进一步的问题，请告诉我！

### ---------------------------------

### 43.3 依赖种类（Dependent Kinds）

在类型系统中，**依赖种类（Dependent Kinds）**是一类更为通用的种类。通过依赖种类，我们可以在积类型（product kinds）和函数类型（function kinds）中，让第二个部分的类型依赖于第一个部分的具体构造器。依赖种类提供了更高的表达能力，能够解决高阶种类（higher kinds）无法解决的问题。

#### 1. **依赖种类的需求**

在第18章中引入的高阶种类虽然强大，但存在表达能力的限制。我们能够表达某些构造器类型的种类，例如当应用于一个类型时，返回一个特定类型（比如 $int$）的种类：
$$
T \to S(int)
$$
然而，使用这种表达方式，我们无法表示**当应用于某个类型时，返回的结果是这个类型本身**的种类，因为函数的结果无法引用函数的参数。例如，如果我们想表示一对（pair）的第二个组件与第一个组件等价的情况，高阶种类也无法处理这种依赖关系。

为了解决这个问题，积种类和函数种类需要进行推广，使得对偶的第二个部分可以引用第一个部分的值，或者函数的返回类型可以引用参数的类型。这种扩展就是**依赖种类**，因为它涉及到依赖于构造器（constructor）的种类。

#### 2. **依赖种类的语法**

依赖种类的语法如下所示：

$$
\kappa ::= S(c) \mid \Pi (\kappa_1; u. \kappa_2) \mid \lambda (\kappa_1; u. \kappa_2)
$$

其中：
- **$S(c)$**：单元素种类，表示与构造器 $c$ 等价的所有构造器。
- **$\Pi (u :: \kappa_1 . \kappa_2)$**：依赖积种类，表示构造器对 $(c_1, c_2)$，其中 $c_1$ 属于种类 $\kappa_1$，$c_2$ 属于种类 $[c_1/u]\kappa_2$。
- **$\lambda (u :: \kappa_1 . \kappa_2)$**：依赖函数种类，表示当应用一个构造器 $c_1 :: \kappa_1$ 时，返回一个种类 $[c_1/u]\kappa_2$ 的构造器。

这些种类表达了构造器与其参数之间的依赖关系。

#### 3. **依赖积种类（Dependent Product Kind）**

依赖积种类 $\Pi u :: \kappa_1 . \kappa_2$ 分类由构造器对 $(c_1, c_2)$ 组成的对，其中 $c_1$ 的种类为 $\kappa_1$，而 $c_2$ 的种类为 $[c_1/u] \kappa_2$。

例如，种类 $\Pi u :: T . S(u)$ 表示所有对 $(c_1, c_2)$，其中 $c_1$ 是种类 $T$ 的构造器，并且 $c_2$ 等价于 $c_1$。

- **公式示例**：
  $$
  \Pi u :: T . S(u)
  $$
  - 解释：此依赖积种类表示一对构造器 $(c, c)$，其中 $c$ 是种类 $T$ 的构造器。更一般地，这种种类分类的是构造器对 $(c_1, c_2)$，其中 $c_1$ 和 $c_2$ 是等价的构造器，但不一定是完全相同的构造器。

#### 4. **依赖函数种类（Dependent Function Kind）**

依赖函数种类 $\lambda u :: \kappa_1 . \kappa_2$ 分类构造器 $c$，当应用于构造器 $c_1 :: \kappa_1$ 时，生成一个种类 $[c_1/u]\kappa_2$ 的构造器。

例如，种类 $\lambda u :: T . S(u)$ 表示当应用于构造器 $c$ 时，生成一个等价于 $c$ 的构造器。实际上，这类构造器相当于**恒等函数（identity function）**。

- **公式示例**：
  $$
  \lambda u :: T . S(u)
  $$
  - 解释：此依赖函数种类分类的是当应用于某个构造器 $c$ 时，返回一个与 $c$ 等价的构造器，这可以视为恒等函数的类型。

#### 5. **依赖种类的组合**

我们可以将依赖种类组合起来，形成更复杂的类型。例如，种类：
$$
\lambda u :: T \times T . S(u \cdot r) \times S(u \cdot l)
$$
表示一种函数类型，它接受一对类型，并将其两个分量交换。

- **公式示例**：
  $$
  \lambda u :: T \times T . S(u \cdot r) \times S(u \cdot l)
  $$
  - 解释：该种类表示一个函数，当其应用于一对类型时，返回一对新类型，交换了原对中的左右分量。

#### 6. **总结**

依赖种类通过允许类型系统中的构造器对参数进行引用或依赖，显著增强了类型系统的表达能力。它们不仅仅是种类层面的扩展，也为类型的精确表达提供了强大的工具，特别是在复杂类型推理中。

**依赖积种类**和**依赖函数种类**允许我们定义和操作更加精细的类型结构，这在模块化程序设计中具有重要意义。

### ---------------------------------

### 43.3.1 依赖积种类的构建、引入和消解规则

依赖积种类的**构建规则、引入规则和消解规则**如下：

#### 1. **构建规则 (Formation Rule)**

$$
\frac{\Gamma \vdash \kappa_1 \ \text{kind} \quad \Gamma, u :: \kappa_1 \vdash \kappa_2 \ \text{kind}}{\Gamma \vdash \Pi u :: \kappa_1 . \kappa_2 \ \text{kind}} \tag{43.5a}
$$

- **解释**：该规则说明，如果 $\kappa_1$ 是一个种类（kind），并且在上下文中有 $u :: \kappa_1$，$\kappa_2$ 也是一个种类，那么 $\Pi u :: \kappa_1 . \kappa_2$ 是一个有效的种类。这意味着依赖积种类是通过将一个种类与其后继的依赖种类结合来形成的。

#### 2. **引入规则 (Introduction Rule)**

$$
\frac{\Gamma \vdash c_1 :: \kappa_1 \quad \Gamma \vdash c_2 :: [c_1/u] \kappa_2}{\Gamma \vdash \langle c_1, c_2 \rangle :: \Pi u :: \kappa_1 . \kappa_2} \tag{43.5b}
$$

- **解释**：如果构造器 $c_1$ 属于种类 $\kappa_1$，并且 $c_2$ 属于种类 $[c_1/u]\kappa_2$，那么对 $(c_1, c_2)$ 的对（pair）属于依赖积种类 $\Pi u :: \kappa_1 . \kappa_2$。此规则表明如何从两个构造器引入依赖积种类。

#### 3. **消解规则：左投影 (Elimination Rule: Left Projection)**

$$
\frac{\Gamma \vdash c :: \Pi u :: \kappa_1 . \kappa_2}{\Gamma \vdash c \cdot l :: \kappa_1} \tag{43.5c}
$$

- **解释**：如果 $c$ 属于依赖积种类 $\Pi u :: \kappa_1 . \kappa_2$，那么 $c$ 的左投影 $c \cdot l$ 属于种类 $\kappa_1$。这一规则允许我们从对中提取第一个元素。

#### 4. **消解规则：右投影 (Elimination Rule: Right Projection)**

$$
\frac{\Gamma \vdash c :: \Pi u :: \kappa_1 . \kappa_2}{\Gamma \vdash c \cdot r :: [c \cdot l / u] \kappa_2} \tag{43.5d}
$$

- **解释**：如果 $c$ 属于依赖积种类 $\Pi u :: \kappa_1 . \kappa_2$，那么 $c$ 的右投影 $c \cdot r$ 属于种类 $[c \cdot l / u] \kappa_2$。该规则允许从对中提取第二个元素，而第二个元素的种类可能依赖于第一个元素。

#### 5. **等价性公理 (Equivalence Axioms)**

依赖积种类的构造器遵循以下等价性公理：

1. **左投影的等价性 (Left Projection Equivalence)**：
   $$
   \frac{\Gamma \vdash c_1 :: \kappa_1 \quad \Gamma \vdash c_2 :: \kappa_2}{\Gamma \vdash \langle c_1, c_2 \rangle \cdot l \equiv c_1 :: \kappa_1} \tag{43.6a}
   $$

2. **右投影的等价性 (Right Projection Equivalence)**：
   $$
   \frac{\Gamma \vdash c_1 :: \kappa_1 \quad \Gamma \vdash c_2 :: \kappa_2}{\Gamma \vdash \langle c_1, c_2 \rangle \cdot r \equiv c_2 :: \kappa_2} \tag{43.6b}
   $$

- **解释**：这些等价性公理表明，在依赖积种类中，左投影总是等价于第一个构造器 $c_1$，而右投影总是等价于第二个构造器 $c_2$。

#### 6. **子种类规则 (Subkinding Rule)**

依赖积种类的子种类规则指出它在两个位置都是协变的（covariant）：
$$
\frac{\Gamma \vdash \kappa_1 <:: \kappa'_1 \quad \Gamma, u :: \kappa_1 \vdash \kappa_2 <:: \kappa'_2}{\Gamma \vdash \Pi u :: \kappa_1 . \kappa_2 <:: \Pi u :: \kappa'_1 . \kappa'_2} \tag{43.7}
$$

- **解释**：如果 $\kappa_1$ 是 $\kappa'_1$ 的子种类，并且 $\kappa_2$ 是 $\kappa'_2$ 的子种类，那么依赖积种类 $\Pi u :: \kappa_1 . \kappa_2$ 是 $\Pi u :: \kappa'_1 . \kappa'_2$ 的子种类。这个规则指出了依赖积种类在两个位置的协变性。

#### 7. **依赖积种类的等价性规则 (Equivalence Rule for Dependent Product Kinds)**

依赖积种类的等价性规则类似于子种类规则：
$$
\frac{\Gamma \vdash \kappa_1 \equiv \kappa'_1 \quad \Gamma, u :: \kappa_1 \vdash \kappa_2 \equiv \kappa'_2}{\Gamma \vdash \Pi u :: \kappa_1 . \kappa_2 \equiv \Pi u :: \kappa'_1 . \kappa'_2} \tag{43.8}
$$

- **解释**：如果 $\kappa_1$ 和 $\kappa'_1$ 是等价的种类，并且 $\kappa_2$ 和 $\kappa'_2$ 是等价的种类，那么依赖积种类 $\Pi u :: \kappa_1 . \kappa_2$ 和 $\Pi u :: \kappa'_1 . \kappa'_2$ 是等价的。

#### 8. **推导示例**

以下是通过这些规则推导出的子种类关系和等价性：

- 子种类：
  $$
  \Pi u :: S(int) . S(u) <:: \Pi u :: T . S(u)
  $$
  - **解释**：单元素种类 $S(int)$ 的依赖积种类是更一般的 $T$ 种类的子种类，因为我们“遗忘”了关于 $int$ 的具体身份。

- 等价性：
  $$
  \Pi u :: S(int) . S(u) \equiv S(int) \times S(int)
  $$
  - **解释**：单元素种类 $S(int)$ 的依赖积种类等价于两个 $S(int)$ 的积种类。

#### 9. **总结**

这些规则揭示了依赖积种类在类型系统中的行为，特别是在处理对（pairs）和构造器之间的依赖关系时。通过这些规则，我们可以表达更加复杂的类型系统，并处理不同构造器之间的关系。

### ---------------------------------

### 43.4 依赖函数种类的构建、引入和消解规则

依赖函数种类（Dependent Function Kind）的**构建规则**、**引入规则**和**消解规则**如下所示。

#### 1. **构建规则 (Formation Rule)**

$$
\frac{\Gamma \vdash \kappa_1 \ \text{kind} \quad \Gamma, u :: \kappa_1 \vdash \kappa_2 \ \text{kind}}{\Gamma \vdash \lambda u :: \kappa_1 . \kappa_2 \ \text{kind}} \tag{43.9a}
$$

- **解释**：如果 $\kappa_1$ 是一个种类（kind），并且当 $u$ 属于种类 $\kappa_1$ 时，$\kappa_2$ 是一个种类，那么依赖函数种类 $\lambda u :: \kappa_1 . \kappa_2$ 是一个有效的种类。这个规则确保依赖函数可以通过构造函数的输入和输出定义。

#### 2. **引入规则 (Introduction Rule)**

$$
\frac{\Gamma, u :: \kappa_1 \vdash c :: \kappa_2}{\Gamma \vdash \lambda (u :: \kappa_1) c :: \lambda u :: \kappa_1 . \kappa_2} \tag{43.9b}
$$

- **解释**：如果在上下文中，$u$ 属于种类 $\kappa_1$，且构造器 $c$ 属于种类 $\kappa_2$，那么依赖函数 $\lambda (u :: \kappa_1) c$ 属于依赖函数种类 $\lambda u :: \kappa_1 . \kappa_2$。这条规则表明如何通过一个依赖变量引入依赖函数。

#### 3. **消解规则 (Elimination Rule)**

$$
\frac{\Gamma \vdash c :: \lambda u :: \kappa_1 . \kappa_2 \quad \Gamma \vdash c_1 :: \kappa_1}{\Gamma \vdash c[c_1] :: [c_1 / u] \kappa_2} \tag{43.9c}
$$

- **解释**：如果 $c$ 是依赖函数种类 $\lambda u :: \kappa_1 . \kappa_2$ 的构造器，并且 $c_1$ 属于种类 $\kappa_1$，那么函数应用 $c[c_1]$ 属于种类 $[c_1 / u]\kappa_2$。该规则描述了如何通过将具体参数代入函数来消解依赖函数种类。

#### 4. **等价性规则 (Equivalence Rule)**

$$
\frac{\Gamma, u :: \kappa_1 \vdash c :: \kappa_2 \quad \Gamma \vdash c_1 :: \kappa_1}{\Gamma \vdash (\lambda (u :: \kappa_1) c)[c_1] \equiv [c_1 / u] c :: \kappa_2} \tag{43.10}
$$

- **解释**：如果 $c$ 是依赖函数种类 $\lambda u :: \kappa_1 . \kappa_2$ 中的构造器，且 $c_1$ 是种类 $\kappa_1$ 的构造器，那么应用 $\lambda(u :: \kappa_1) c$ 的结果等价于将 $c_1$ 代入 $u$ 得到的构造器 $[c_1 / u] c$。这表明依赖函数的结果通过将参数代入结果种类来计算。

#### 5. **子种类规则 (Subkinding Rule)**

$$
\frac{\Gamma \vdash \kappa'_1 <:: \kappa_1 \quad \Gamma, u :: \kappa'_1 \vdash \kappa_2 <:: \kappa'_2}{\Gamma \vdash \lambda u :: \kappa_1 . \kappa_2 <:: \lambda u :: \kappa'_1 . \kappa'_2} \tag{43.11}
$$

- **解释**：依赖函数种类在其**定义域（domain）**中是**逆变（contravariant）**的，而在其**值域（codomain）**中是**协变（covariant）**的。即，如果 $\kappa'_1$ 是 $\kappa_1$ 的子种类，并且在此上下文下 $\kappa_2$ 是 $\kappa'_2$ 的子种类，那么 $\lambda u :: \kappa_1 . \kappa_2$ 是 $\lambda u :: \kappa'_1 . \kappa'_2$ 的子种类。

#### 6. **等价性规则 (Equivalence Rule for Dependent Function Kinds)**

$$
\frac{\Gamma \vdash \kappa_1 \equiv \kappa'_1 \quad \Gamma, u :: \kappa_1 \vdash \kappa_2 \equiv \kappa'_2}{\Gamma \vdash \lambda u :: \kappa_1 . \kappa_2 \equiv \lambda u :: \kappa'_1 . \kappa'_2} \tag{43.12}
$$

- **解释**：该规则表明依赖函数种类的等价性。如果 $\kappa_1$ 和 $\kappa'_1$ 是等价的，并且在此上下文下 $\kappa_2$ 和 $\kappa'_2$ 也是等价的，那么 $\lambda u :: \kappa_1 . \kappa_2$ 和 $\lambda u :: \kappa'_1 . \kappa'_2$ 也是等价的。

#### 7. **推导示例**

通过这些规则，我们可以推导出一些有趣的子种类和等价性：

- **子种类 (Subkinding)**：
  $$
  \lambda u :: T . S(int) <:: \lambda u :: S(int) . T
  $$
  - **解释**：此推导表明，一个函数可以接受任何类型并返回 $int$，同时也可以仅接受 $int$ 并返回任意类型。这展示了依赖函数种类的协变和逆变特性。

- **等价性 (Equivalence)**：
  $$
  \lambda u :: S(int) . S(u) \equiv S(int) \to S(int)
  $$
  - **解释**：这表明一个参数为 $S(int)$ 的函数返回的结果可以等价于将 $S(int)$ 映射到自身。这可以通过将参数的信息传播到结果种类中来解释。

- **子种类的进一步推导**：
  $$
  \lambda u :: T . S(u) <:: S(int) \to S(int)
  $$
  - **解释**：该推导表明一个构造函数接受一个参数并返回其本身，实际上是一个可以应用于 $int$ 并返回 $int$ 的构造函数。

#### 8. **总结**

依赖函数种类提供了一种强大的机制来表示函数类型中的复杂依赖关系。通过这些规则，类型系统可以处理具有依赖性的构造器函数，表达函数在其参数和返回值之间的细致联系。

### ---------------------------------

### 43.4 高阶单例 (Higher Singletons)

高阶单例 (Higher Singletons) 是对单例种类（singleton kinds）的扩展，允许为任何种类的构造器定义单例，而不仅仅是种类 $T$ 的构造器。依赖乘积种类（dependent product kinds）和依赖函数种类（dependent function kinds）的使用使得可以为任意高阶种类定义单例。接下来，我们将逐步解释高阶单例的定义和推导规则。

#### 1. **高阶单例的定义**

单例种类 $S(c :: \kappa)$ 表示所有与构造器 $c$ 等价的构造器。对于基础种类 $T$，单例 $S(c)$ 的定义很简单；但是对于更高的种类，例如依赖乘积种类 $ u :: \kappa_1 . \kappa_2$ 和依赖函数种类 $ u :: \kappa_1 . \kappa_2$，我们需要通过递归来定义高阶单例。

#### 2. **依赖乘积种类的单例 (Singleton for Dependent Product Kinds)**

假设 $c$ 是一个依赖乘积种类 $c :: \kappa_1 \times \kappa_2$ 的构造器，根据该构造器的定义，它可以分解为 $c \cdot l$ 和 $c \cdot r$，即取其左右部分。因此，种类 $S(c :: \kappa_1 \times \kappa_2)$ 可以定义为两个单例的乘积：
$$
S(c :: \kappa_1 \times \kappa_2) \equiv S(c \cdot l :: \kappa_1) \times S(c \cdot r :: \kappa_2)
$$

#### 3. **依赖函数种类的单例 (Singleton for Dependent Function Kinds)**

对于依赖函数种类 $c :: \kappa_1 \to \kappa_2$ 的构造器，我们可以通过 λ-抽象（lambda abstraction）来定义单例：
$$
S(c :: \kappa_1 \to \kappa_2) \equiv  u :: \kappa_1 . S(c[u] :: \kappa_2)
$$
其中 $c[u]$ 表示将构造器 $u$ 作为参数应用于 $c$。

#### 4. **推导规则**

接下来，我们通过一系列的等式来递归定义 $S(c :: \kappa)$ 的结构。规则如下：

1. **基本单例规则**：
   $$
   \frac{\Gamma \vdash c :: S(c')} {\Gamma \vdash S(c :: S(c')) \equiv S(c)} \tag{43.13a}
   $$

2. **依赖乘积种类的单例**：
   $$
   \frac{\Gamma \vdash c ::  u :: \kappa_1 . \kappa_2} {\Gamma \vdash S(c ::  u :: \kappa_1 . \kappa_2) \equiv  u :: S(c \cdot l :: \kappa_1) . S(c \cdot r :: \kappa_2)} \tag{43.13b}
   $$

3. **依赖函数种类的单例**：
   $$
   \frac{\Gamma \vdash c ::  u :: \kappa_1 . \kappa_2} {\Gamma \vdash S(c ::  u :: \kappa_1 . \kappa_2) \equiv  u :: \kappa_1 . S(c[u] :: \kappa_2)} \tag{43.13c}
   $$

#### 5. **构造器的外延性原则 (Extensionality Principles)**

为了合理化这些定义，我们依赖构造器等价的外延性原则。外延性原则表明，依赖乘积种类和依赖函数种类中的构造器在形式上是对等的：

- **依赖乘积种类的外延性**：
  $$
  \frac{\Gamma \vdash c ::  u :: \kappa_1 . \kappa_2} {\Gamma \vdash c \equiv  c \cdot l, c \cdot r \rangle ::  u :: \kappa_1 . \kappa_2} \tag{43.14a}
  $$
  这表明，乘积构造器等价于其左右部分的对。

- **依赖函数种类的外延性**：
  $$
  \frac{\Gamma \vdash c ::  u :: \kappa_1 . \kappa_2} {\Gamma \vdash c \equiv \lambda (u :: \kappa_1) c[u] ::  u :: \kappa_1 . \kappa_2} \tag{43.14b}
  $$
  这表明，函数构造器等价于对应的 λ-抽象。

#### 6. **总结**

通过这些规则，我们可以为任何种类的构造器定义其单例，从而保证在类型检查和类型推导过程中保持一致性。高阶单例允许我们精确地定义构造器的等价性，这对于模块系统和复杂的类型系统设计具有重要意义。

### ---------------------------------

### 43.4 高阶单例规则的补充 (Supplementary Rules for Higher Singletons)

为了确保单例规则能够扩展到更高种类，需要引入一组自识别规则。这些规则类似于先前在低阶情况下的自识别原则 (self-recognition rules)，但它们适用于更复杂的依赖种类 (dependent kinds)。

#### 1. **规则 43.15a：依赖乘积种类的自识别规则**

依赖乘积种类 $ u :: \kappa_1. \kappa_2$ 的自识别规则如下：
$$
\frac{\Gamma \vdash c \cdot l :: \kappa_1 \quad \Gamma \vdash c \cdot r :: [c \cdot l/u]\kappa_2}{\Gamma \vdash c ::  u :: \kappa_1 . \kappa_2} \tag{43.15a}
$$

这个规则的含义是，如果一个构造器 $c$ 的左部分 $c \cdot l$ 属于种类 $\kappa_1$，右部分 $c \cdot r$ 属于种类 $[c \cdot l/u] \kappa_2$，那么整个构造器 $c$ 属于依赖乘积种类 $ u :: \kappa_1 . \kappa_2$。

#### 2. **规则 43.15b：依赖函数种类的自识别规则**

依赖函数种类 $ u :: \kappa_1 . \kappa_2$ 的自识别规则如下：
$$
\frac{\Gamma, u :: \kappa_1 \vdash c[u] :: \kappa_2}{\Gamma \vdash c ::  u :: \kappa_1 . \kappa_2} \tag{43.15b}
$$

这个规则表明，如果在上下文 $\Gamma$ 中，$c[u]$ 属于种类 $\kappa_2$，那么整个函数构造器 $c$ 属于依赖函数种类 $ u :: \kappa_1 . \kappa_2$。

#### 3. **应用示例**

为了说明这些规则的实际应用，假设 $u$ 是一个属于种类 $ v :: T. S(v)$ 的构造器变量。我们可以利用规则 $43.2b$ 推导出 $u \cdot l :: S(u \cdot l)$，这表示 $u$ 的左部分具有单例种类 $S(u \cdot l)$。

再利用规则 $43.5d$，我们可以推导出 $u \cdot r :: S(u \cdot l)$。根据规则 $43.15a$，我们可以进一步推导出 $u ::  v :: S(u \cdot l). S(u \cdot l)$，这个种类是 $ v :: T. S(v)$ 的子种类 (subkind)。这意味着 $u$ 的第一个部分是 $u \cdot l$，而第二个部分等价于第一个部分，因此它也是 $u \cdot l$。

#### 4. **高阶单例的作用**

引入高阶单例 (higher singletons) 的目的是确保每个构造器都可以通过一个种类来分类，这个种类能够通过定义等价性 (definitional equality) 来确定构造器。换句话说，通过扩展单例类型，我们希望高阶单例也能享有类似的性质。

#### 5. **定理 43.1：高阶单例的性质**

**定理 43.1** 说明：
- 如果 $\Gamma \vdash c :: \kappa$，那么 $\Gamma \vdash S(c :: \kappa) <:: \kappa$ 并且 $\Gamma \vdash c :: S(c :: \kappa)$。

这一定理表明，单例种类 $S(c)$ 始终是其基础种类 $\kappa$ 的子种类 (subkind)，并且 $c$ 确实属于其自身定义的单例种类。

**定理的证明** 由于其复杂性，超出了本书的讨论范围。

### ---------------------------------

### 43.5 注释 (Notes)

单例种类 (Singleton Kinds) 最早由 Stone 和 Harper 在 2006 年引入，目的是在 ML 模块系统 (Module System) 中隔离类型共享 (Type Sharing) 的概念。ML 模块系统是由 Milner 等人 (1997) 提出的，Harper 和 Lillibridge (1994)，以及 Leroy (1994) 对其进行了进一步发展。

#### 1. **单例种类的复杂性**

单例种类的元理论 (Meta-theory) 出奇地复杂，主要原因来自于**构造器索引的种类族**(constructor-indexed families of kinds)。具体来说，如果我们有 $u :: \kappa$ 且 $c' :: \kappa'$，并且 $c_1 :: \kappa$ 和 $c_2 :: \kappa$ 是不同但等价的构造器，那么 $[c_1 / u] \kappa'$ 和 $[c_2 / u] \kappa'$ 也是不同但等价的实例。这种等价性的管理在证明过程中引发了显著的技术难题。

#### 2. **类型共享问题的背景**

单例种类的引入源于解决类型共享的问题。在 ML 模块系统中，不同模块可能会共享某些类型信息，而确保这些共享信息的一致性和正确性变得至关重要。例如，当模块之间共享数据结构时，必须确保这些数据结构的类型在不同模块中等价，这便是类型共享的核心问题。

#### 3. **类型共享的挑战**

在单例种类的上下文中，构造器等价性是核心挑战之一。正如上文提到的，如果两个构造器 $c_1$ 和 $c_2$ 是等价的，那么它们在依赖种类或构造器索引的家族中的表现也必须保持一致。然而，处理这些等价性的细微之处增加了证明的复杂性，尤其是在处理依赖种类的情况下，需要追踪每个构造器的等价性并确保其正确传播。

#### 4. **总结**

单例种类为 ML 模块系统中的类型共享问题提供了理论基础。然而，由于涉及构造器和种类等价的复杂性，其元理论在证明过程中面临重大技术挑战。这种复杂性主要来源于如何管理不同构造器在依赖种类中的等价性。


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