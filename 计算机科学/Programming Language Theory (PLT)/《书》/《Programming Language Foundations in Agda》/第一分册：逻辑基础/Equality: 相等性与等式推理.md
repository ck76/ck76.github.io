[toc]

以下是本章节涉及的所有数理逻辑相关概念的解释，并使用了 Markdown 的公式表示法：

### 1. **相等性（Equality, $\equiv$）**
   - 相等性表示两个项在逻辑上是相同的，记为 $M \equiv N$，表示 $M$ 和 $N$ 可以相互替换。相等性是一种基本的逻辑关系。

### 2. **相等性的归纳定义**
   - 相等性在 Agda 中定义为一个归纳的数据类型，通过以下方式定义：
     $$
     \text{data } _\equiv_ \{A : \text{Set}\} (x : A) : A \rightarrow \text{Set} \text{ where }
     \text{refl : } x \equiv x
     $$
   - 这里，$A$ 是一个类型，$x$ 是该类型的一个值。$x \equiv x$ 的证明由构造子 `refl` 提供。

### 3. **自反性（Reflexivity）**
   - 自反性表明对于任何 $x$，$x \equiv x$ 成立。这直接由相等性的定义得出。

### 4. **对称性（Symmetry）**
   - 对称性表明如果 $x \equiv y$，那么 $y \equiv x$ 也成立。这个性质可以通过对相等性定义中的 `refl` 构造子来证明：
     $$
     \text{sym : } \forall \{A : \text{Set}\} \{x y : A\} \rightarrow x \equiv y \rightarrow y \equiv x
     $$
     证明为：
     $$
     \text{sym refl = refl}
     $$

### 5. **传递性（Transitivity）**
   - 传递性表明如果 $x \equiv y$ 且 $y \equiv z$，那么 $x \equiv z$ 也成立。这个性质可以通过如下方式证明：
     $$
     \text{trans : } \forall \{A : \text{Set}\} \{x y z : A\} \rightarrow x \equiv y \rightarrow y \equiv z \rightarrow x \equiv z
     $$
     证明为：
     $$
     \text{trans refl refl = refl}
     $$

### 6. **合同性（Congruence）**
   - 合同性表明如果两个项 $x \equiv y$，那么将它们代入相同的函数中，其结果仍然相等：
     $$
     \text{cong : } \forall \{A B : \text{Set}\} (f : A \rightarrow B) \{x y : A\} \rightarrow x \equiv y \rightarrow f x \equiv f y
     $$
     证明为：
     $$
     \text{cong f refl = refl}
     $$

### 7. **替换性（Substitution）**
   - 替换性表明如果 $x \equiv y$，且 $P x$ 成立，那么 $P y$ 也成立：
     $$
     \text{subst : } \forall \{A : \text{Set}\} \{x y : A\} (P : A \rightarrow \text{Set}) \rightarrow x \equiv y \rightarrow P x \rightarrow P y
     $$
     证明为：
     $$
     \text{subst P refl px = px}
     $$

### 8. **等式链（Equality Chain）**
   - 等式链是一种用于推理的技术，通过逐步应用相等性来连接一系列等式。Agda 中的等式链定义为：
     $$
     \begin{aligned}
     \text{begin } x \equiv y & = x \equiv y \\
     x \equiv\langle\rangle y & = x \equiv y \\
     \text{step-}\equiv & : \forall (x \{y z\} : A) \rightarrow y \equiv z \rightarrow x \equiv y \rightarrow x \equiv z
     \end{aligned}
     $$
     使用等式链可以让证明更易于阅读和理解。

### 9. **重写（Rewrite）**
   - 重写是一种在推理中使用的技术，通过已知的等式将目标表达式中的某一部分替换为等价的另一部分。重写在 Agda 中的语法为：
     $$
     \text{rewrite } \text{proof-name}
     $$
     重写可以简化证明过程。

### 10. **莱布尼兹相等性（Leibniz Equality, $\overset{\sim}{\equiv}$）**
   - 莱布尼兹相等性表示两个对象相等当且仅当它们满足完全相同的性质。定义如下：
     $$
     x \overset{\sim}{\equiv} y \text{ 当 } \forall P \, P(x) \rightarrow P(y)
     $$
   - 莱布尼兹相等性是 Martin-Löf 相等性的一个更强形式。

### 11. **全体多态（Universe Polymorphism）**
   - 全体多态允许定义可以应用于任何类型阶层 $ℓ$。例如，泛化的相等性定义为：
     $$
     \text{data } _\equiv' \{ℓ : \text{Level}\} \{A : \text{Set } ℓ\} (x : A) : A \rightarrow \text{Set } ℓ \text{ where }
     \text{refl' : } x \equiv' x
     $$

### 12. **层级（Levels）**
   - 层级表示类型的不同阶层，如 $Set₀$、$Set₁$ 等。定义层级时，可以使用以下构造子：
     $$
     \text{lzero : Level, lsuc : Level \rightarrow Level}
     $$
   - 运算符 $⊔$ 用于获取两个层级中较大的那个：
     $$
     _⊔_ : Level \rightarrow Level \rightarrow Level
     $$

### 13. **重载构造子（Overloaded Constructor）**
   - 重载构造子允许不同的数据类型使用相同的构造子名称。例如，在定义自然数与加法时，构造子 $suc$ 和 $zero$ 可以在多个定义中重复使用。

这些概念在数理逻辑和形式化证明中起到了关键作用，特别是在理解相等性和推理的过程中。希望这些解释能帮助您更好地理解本章节的内容。