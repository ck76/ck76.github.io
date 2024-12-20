[toc]



好的，我将按照您的要求，详细地解释《Proofs and Types》的第一章内容，并按照原文的顺序，不遗漏任何重要细节。

---

### **第1章 感觉、指称与语义**

#### **引言**

作者指出，**理论计算**尚未成为一门成熟的科学。许多基本概念仍然没有被澄清，目前的研究工作遵循着一种类似于“**婚礼蛋糕**”的模式。这种模式意味着层层叠加，但缺乏坚实的基础。例如，**语言设计**被比作**托勒密的天文学**，总是需要进一步的修正和补丁。然而，一些有限的主题，如**复杂性理论**和**指称语义学**，相对地摆脱了这种批评。

在这样的情形下，**方法论的讨论**变得极为重要，因为我们必须将方法论视为**战略**，而将具体的结果视为**战术性**的。我们感兴趣的问题，源自20世纪初逻辑学的“漩涡”，这由弗雷格（Frege）、洛文海姆（Löwenheim）、哥德尔（Gödel）等人的工作所体现。对于不熟悉逻辑史的读者，建议参考[van Heijenoort]的著作。

#### **1.1 逻辑中的感觉和指称**

##### **例子分析**

让我们从一个例子开始。对于输入27和37，有一个标准的乘法过程，得到结果999。我们能对此说些什么？

- **初步尝试**：我们可以写出等式：
  
  $$
  27 \times 37 = 999
  $$

  在数学的主流观点中，这个等式表示左右两边**指称**同一个整数（在本文中，整数指自然数：0，1，2，...）。乘法符号“$\times$”被视为康托尔（Cantor）意义上的**函数**，即一个由有序对组成的**图（graph）**。

- **指称方面**：这是指称的角度，毫无疑问是正确的，但它**忽略了关键的一点**：
  
  **存在一个有限的计算过程，展示了左右两边的指称是相等的。**

  如果我们简单地说$27 \times 37$等于999，这是一种滥用（这不是廉价的哲学问题——这是一个具体的问题），因为如果两者真的是同一个东西，我们就不会觉得有必要声明它们相等。实际上，我们提出了一个问题“$27 \times 37$”，并得到一个答案“999”。这两个表达式具有不同的**感觉（Sense）**，我们必须做一些事情（例如，进行计算或证明，或者至少查阅百科全书）来证明这两个感觉具有相同的**指称（Denotation）**。

- **关于乘法“$\times$”**：将其视为一个函数（作为一个图）是不正确的，因为计算机中加载的程序没有足够的空间来存储一个无限的图。因此，我们必须得出结论，我们面对的是一个与**感觉**相关的**有限动态过程**。

##### **感觉与指称的区别**

- **指称（Denotation）**：表达式所指向的对象或值。在上述例子中，$27 \times 37$和999具有相同的指称，即数字999。
  
- **感觉（Sense）**：表达式的计算过程、证明步骤或操作序列。$27 \times 37$和999虽然指称相同，但它们的感觉不同，因为前者需要通过计算才能得到后者。

作者指出，虽然指称早在很早的时候就被建模了，但**感觉被推向了主观领域**，导致目前对感觉的数学处理几乎被简化为句法操作。这并非学科的本质，我们可以期待在未来的几十年中，找到一种对计算的处理方式，能够结合**指称语义**的数学清晰性和**句法**的有限动态性。本书明显基于这样一种传统：在**无限的、静态的指称**和**有限的、动态的感觉**的二分中，**指称的方面**比另一面发展得更加充分。

##### **弗雷格的观点**

在逻辑中，弗雷格（Frege）提出了最基本的区分：给定一个句子$A$，有两种看待它的方式：

1. **作为一系列指令**，这确定了它的**感觉**。例如，$A \lor B$表示“$A$或$B$”，等等。

2. **作为这些操作的理想结果**：这就是它的**指称**。

   - **指称**（与**符号**相对）是被指称的对象，而不是指称的符号。
   - 例如，一个逻辑句子的指称是真（$t$）或假（$f$）；$A \lor B$的指称可以通过$A$和$B$的指称以及析取的真值表来获得。

需要注意的是：

- 两个具有相同**感觉**的句子具有相同的**指称**，这是显然的。
- 但两个具有相同**指称**的句子很少具有相同的**感觉**。

例如，考虑一个复杂的数学等价式$A \Leftrightarrow B$。这两个句子具有相同的指称（它们同时为真或假），但显然不是相同的感觉，否则证明它们的等价性就没有意义了。

##### **关联的概念**

- **感觉**：句法、证明、计算过程。

- **指称**：真值、语义、代数操作。

这是逻辑中的基本二分法。然而，需要指出的是，**这两方面并非对等地发挥作用**。

#### **1.1.1 代数传统**

- **起源**：这一传统由布尔（Boole）在弗雷格之前就开始了，基于奥卡姆剃刀的激进应用：**直接舍弃感觉，只考虑指称**。

- **理由**：这种对逻辑的“削减”有其操作性方面的理由：**它有效！**

- **关键转折点**：1916年洛文海姆（Löwenheim）的定理确立了这一传统的主导地位。如今，模型论（Model Theory）被视为这一认识论选择所带来的丰富成果。

- **方法**：从指称的角度考虑逻辑，即操作的结果，我们发现了一种有点特殊的代数，但它允许我们研究传统代数中不常见的操作。特别是，可以避免局限于（比如说）等式类，而考虑一般的可定义结构。因此，模型论以一种经常富有成效的方式，重新激发了代数的思想和方法。

#### **1.1.2 句法传统**

- **无法完全忽略指称**：另一方面，**不可能完全忘记指称而只关注感觉**，因为感觉至少隐含地包含了指称。因此，这并不是一种对称的关系。

- **缺乏统一的观点**：实际上，几乎没有统一的句法观点，因为我们从未能够赋予这种神秘的感觉以操作性的意义。关于感觉的唯一可触及的现实是其书写方式，即形式主义；但形式主义仍然是一个难以处理的研究对象，缺乏真正的结构，如同一块软奶酪。

- **价值**：这并不意味着纯粹的句法方法没有任何可取之处，Gentzen在1934年的著名定理（如**削减消除定理**）表明，逻辑在句法层面上具有一些深刻的对称性。然而，这些对称性被句法的缺陷所模糊。换句话说，它们不是句法的对称性，而是**感觉**的对称性。由于缺乏更好的手段，我们必须将它们表达为句法的属性，结果并不十分美观。

- **总结**：总的来说，我们对这一传统的看法是，它一直在寻找其基本概念，即对**感觉**和**句法**的操作性区分。更具体地说，它旨在找到句法的深层几何不变量：这就是感觉所在。

- **发展的停滞**：这一所谓的“句法”传统从未达到与其竞争对手相当的水平。在最近的几十年里，代数传统蓬勃发展，而句法传统并不显眼，如果再过一两代人，可能会因为缺乏出路或方法论而消失。

#### **计算机科学的影响**

- **救赎**：这一灾难被计算机科学所避免——这门极大地操纵句法的学科——它提出了一些非常重要的理论问题。

- **问题类型**：

  - **算法复杂性**等问题似乎更关注逻辑的表面，而非其精神。
  
  - 另一方面，所有关于**程序的正确性**和**模块化**的问题，则深刻地呼应了句法传统，即**证明理论**。

- **重新审视证明理论**：我们被引导去重新审视证明理论，从1930年Herbrand的基本定理开始。这种重新审视为那些被认为已固定、长期受惯例支配的领域带来了新的视角。

- **未来展望**：在句法逻辑传统和计算机科学的交流中，我们可以期待在计算方面出现新的语言和机器。但在逻辑方面（这是本书主要作者的领域），我们终于有希望挖掘出那些一直被忽视的概念基础。

### **1.2 两种语义传统**

#### **1.2.1 塔斯基（Tarski）语义**

- **特点**：这一传统以极其朴素的方式进行解释：逻辑连接词“$\lor$”被翻译为“或”，依此类推。这种解释对逻辑连接词并没有特别深刻的揭示；其表面的平凡性正是其可操作性的原因。

- **关注点**：我们只关注句子的**指称**（闭合表达式）的真值（真$t$或假$f$）。

- **定义方法**：

  1. **原子句子**：我们假设其指称是已知的。例如：

     - $3 + 2 = 5$的指称为真（$t$）。
     - $3 + 3 = 5$的指称为假（$f$）。

  2. **逻辑连接词**：使用真值表来获得表达式的指称。

     | $A$  | $B$  | $A \land B$ | $A \lor B$ | $A \Rightarrow B$ | $\lnot A$ |
     | ---- | ---- | ----------- | ---------- | ----------------- | --------- |
     | $t$  | $t$  | $t$         | $t$        | $t$               | $f$       |
     | $t$  | $f$  | $f$         | $t$        | $f$               | $f$       |
     | $f$  | $t$  | $f$         | $t$        | $t$               | $t$       |
     | $f$  | $f$  | $f$         | $f$        | $t$               | $t$       |

  3. **量词**：

     - **全称量词 $\forall \xi. A$**：当且仅当对于解释域中的每个$a$，$A[a/\xi]$的指称为真时，$\forall \xi. A$的指称为真。
     - **存在量词 $\exists \xi. A$**：当且仅当存在某个$a$，使得$A[a/\xi]$的指称为真时，$\exists \xi. A$的指称为真。

- **评价**：再次强调，从逻辑的角度来看，这种定义方式似乎过于简单，但对于其目的而言已足够。模型论的发展证明了这种方法的有效性。

#### **1.2.2 海廷（Heyting）语义**

- **特点**：海廷的想法不太为人所知，但很难想象原始思想的辉煌程度和随后的平庸发展之间有更大的差异。其目标极具雄心：**对证明进行建模，而非仅对指称进行建模**。

- **方法**：我们不再问“一个句子$A$何时为真？”，而是问“$A$的**证明**是什么？”。这里的**证明**不是指句法形式的记录，而是其内在的对象，书面形式只是对其的影子描述。我们认为，我们所写的证明只是某种已经存在的过程的描述。因此，对于这个极具雄心的问题（如果从计算的角度来看，这也是一个重要的问题），答案不能是一个形式系统。

- **定义方法**：

  1. **原子句子**：我们假设对其证明有内在的理解。例如，用纸笔计算可以作为$27 \times 37 = 999$的证明。

  2. **逻辑连接词**：

     - **合取 $A \land B$**：其证明是一个二元组$(p, q)$，其中$p$是$A$的证明，$q$是$B$的证明。

     - **析取 $A \lor B$**：其证明是一个二元组$(i, p)$，其中：

       - 如果$i = 0$，则$p$是$A$的证明。
       - 如果$i = 1$，则$p$是$B$的证明。

     - **蕴涵 $A \Rightarrow B$**：其证明是一个函数$f$，它将$A$的每个证明$p$映射到$B$的证明$f(p)$。

     - **否定 $\lnot A$**：通常被视为$A \Rightarrow \bot$，其中$\bot$是一个无可证明的句子。

     - **全称量词 $\forall \xi. A$**：其证明是一个函数$f$，它将定义域中的每个$a$映射到$A[a/\xi]$的证明$f(a)$。

     - **存在量词 $\exists \xi. A$**：其证明是一个二元组$(a, p)$，其中$a$是定义域中的一个点，$p$是$A[a/\xi]$的证明。

- **举例**：例如，命题$A \Rightarrow A$的证明是**恒等函数**，它将$A$的每个证明$p$映射到自身$p$。另一方面，如何证明$A \lor \lnot A$？我们必须能够找到$A$的证明或$\lnot A$的证明，但这在一般情况下是不可能的。因此，海廷语义对应于另一种逻辑，即布劳威尔（Brouwer）的**直觉主义逻辑**，我们将在后面遇到。

- **评价**：不可否认，海廷语义非常原创：它并非单独解释逻辑运算符，而是通过抽象构造来解释它们。实际上，这些构造就是**类型化的（即模块化的）程序**。但领域专家在其中看到了完全不同的东西：对数学的功能性方法。换句话说，**证明的语义表达了数学的本质**。

- **问题**：这是一种幻想：一方面，我们有塔斯基的传统，它直截了当但诚实，没有宣称要为基础奠基；另一方面，海廷的传统实际上存在相同的问题——哥德尔的不完备性定理顺便告诉我们，不可能完全避免这种情况。如果我们试图通过证明$A$来解释$A$，我们会遇到这样一个事实：在定义中，量词被使用了两次（对于$\Rightarrow$和$\forall$）。此外，在$\Rightarrow$的情况下，我们不能说$f$的定义域是特别容易理解的！

- **解决尝试**：有人提议在第4和第6条中添加附加条件：“以及一个证明，证明$f$具有该性质”。当然，这并不能解决问题，而关于如何理解这条附加条件的拜占庭式讨论（缺乏任何数学内容）只会削弱这一重要思想的可信度。

- **应用**：我们将在**Curry-Howard同构**中遇到海廷的思想。这也出现在**可实现性**理论中。在这两种情况下，其基础性的夸张被消除了，这使我们能够充分利用这一可能在未来有重大应用的思想。

---

### **总结**

本章深入探讨了逻辑和计算中的两个核心概念：**感觉（Sense）**和**指称（Denotation）**。通过分析乘法的例子，作者强调了在逻辑中区分感觉和指称的重要性。感觉涉及表达式的计算过程和证明步骤，而指称则是表达式最终指向的对象或值。

作者回顾了逻辑中的两种传统：

- **代数传统**：起源于布尔，强调指称，忽略感觉，直接将逻辑简化为代数操作。这一传统在模型论的发展中取得了成功。

- **句法传统**：试图关注感觉，但由于无法完全忽略指称，且感觉的操作性难以把握，这一传统一直在寻找其基本概念。

随着计算机科学的发展，特别是在程序验证和模块化等问题上，对句法和证明的需求重新激发了对句法传统的兴趣。这为逻辑的概念基础带来了新的希望。

在语义方面，作者比较了**塔斯基语义**和**海廷语义**：

- **塔斯基语义**：直接且实用，只关注指称，通过真值表和简单的定义来解释逻辑连接词。

- **海廷语义**：试图对证明进行建模，具有深刻的原创性，但在形式化和基础性方面遇到了困难。

作者指出，尽管海廷语义的基础性目标可能无法实现，但其思想在Curry-Howard同构和可实现性理论中得到了应用，可能在未来有重要的影响。

---

请问您还有其他问题或需要进一步的解释吗？

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