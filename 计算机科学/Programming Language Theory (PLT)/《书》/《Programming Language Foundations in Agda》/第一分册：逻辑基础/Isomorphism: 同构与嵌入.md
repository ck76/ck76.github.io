以下是本章节涉及的所有数理逻辑相关概念的解释：

### 1. **同构（Isomorphism, $A \cong B$）**
   - 同构表示两个集合（类型）之间存在一一对应的关系。形式化定义为：
     $$
     \text{record } A \cong B : \text{Set} \text{ where}
     $$
     - **to**: $A \rightarrow B$
     - **from**: $B \rightarrow A$
     - **from∘to**: $\forall x : A, \text{from}(\text{to}(x)) \equiv x$
     - **to∘from**: $\forall y : B, \text{to}(\text{from}(y)) \equiv y$
   - 同构的关键在于，两个集合之间的映射不仅是双射（bijection），而且存在逆函数。

### 2. **自反性（Reflexivity）**
   - 自反性表示对于每个集合 $A$，存在 $A \cong A$，即 $A$ 与自身同构。该性质的证明通过恒等函数（identity function）作为同构的映射函数来实现。

### 3. **对称性（Symmetry）**
   - 对称性表示如果 $A \cong B$，那么 $B \cong A$ 也成立。对称性的证明通过交换同构中的映射函数（to 和 from）来实现。

### 4. **传递性（Transitivity）**
   - 传递性表示如果 $A \cong B$ 且 $B \cong C$，那么 $A \cong C$ 也成立。传递性通过组合映射函数来实现，即将 $A \rightarrow B$ 和 $B \rightarrow C$ 的映射函数组合起来，形成 $A \rightarrow C$ 的映射函数。

### 5. **嵌入（Embedding, $A \hookrightarrow B$ 或 $A \preceq B$）**
   - 嵌入是一种弱化的同构关系，表示 $A$ 可以嵌入到 $B$ 中，意味着 $A$ 的每个元素都可以映射到 $B$ 中的一个元素，但该映射可能不是双射。嵌入的定义形式化为：
     $$
     \text{record } A \preceq B : \text{Set} \text{ where}
     $$
     - **to**: $A \rightarrow B$
     - **from**: $B \rightarrow A$
     - **from∘to**: $\forall x : A, \text{from}(\text{to}(x)) \equiv x$
   - 嵌入的关键是，$A$ 可以通过某种方式包含在 $B$ 中。

### 6. **反对称性（Antisymmetry）**
   - 反对称性是指如果 $A \preceq B$ 且 $B \preceq A$，并且它们的映射函数相互对应，那么 $A \cong B$，即 $A$ 与 $B$ 是同构的。

### 7. **外延性（Extensionality）**
   - 外延性指的是，如果两个函数在相同的输入上总是产生相同的输出，那么这两个函数是相等的。形式化定义为：
     $$
     \text{postulate } \text{extensionality} : \forall \{A B : \text{Set}\} \{f g : A \rightarrow B\}, (\forall x : A, f(x) \equiv g(x)) \rightarrow f \equiv g
     $$
   - 外延性假设允许我们在证明函数相等时只需验证它们在所有输入下的输出相等。

### 8. **Lambda 表达式（Lambda Expressions, $\lambda$）**
   - Lambda 表达式是一种匿名函数定义方式，用于定义不带名称的函数。其形式为：
     $$
     \lambda x \rightarrow N
     $$
     其中，$x$ 是参数，$N$ 是函数体。Lambda 表达式在函数式编程中非常常见。

### 9. **函数组合（Function Composition, $∘$）**
   - 函数组合是指将一个函数的输出作为另一个函数的输入。其定义为：
     $$
     (g \circ f)(x) = g(f(x))
     $$
   - 函数组合允许将多个函数串联起来，形成新的函数。

### 10. **命题等价性（Propositional Equivalence, $\Leftrightarrow$ 或 $⇔$）**
   - 命题等价性表示两个命题 $A$ 和 $B$ 之间的双向蕴涵关系，即 $A \Rightarrow B$ 且 $B \Rightarrow A$。其形式化定义为：
     $$
     \text{record } A ⇔ B : \text{Set} \text{ where}
     $$
     - **to**: $A \rightarrow B$
     - **from**: $B \rightarrow A$
   - 命题等价性证明了 $A$ 和 $B$ 在逻辑上是等价的。

这些概念在数理逻辑和形式化证明中起到了关键作用，尤其是在理解类型同构、嵌入、函数外延性等高级概念时。通过这些概念，我们能够更好地理解类型论和构造性数学的基础。