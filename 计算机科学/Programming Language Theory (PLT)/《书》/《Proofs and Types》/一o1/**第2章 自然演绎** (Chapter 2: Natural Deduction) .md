[toc]



### **第2章 自然演绎**

#### **引言**

在本章中，作者探讨了**自然演绎（Natural Deduction）**，这是一种逻辑推理系统。与Gentzen的序列演算相比，自然演绎更直接地展示了逻辑的对称性。虽然序列演算（Sequent Calculus）在揭示逻辑的深层对称性方面非常令人满意，但其计算意义（Computational Significance）却被一些句法上的复杂性所掩盖。因此，作者选择先介绍Prawitz的自然演绎，然后再讨论序列演算。

自然演绎具有一些**悖论性**：

- **限制性**：它仅限于**直觉主义逻辑（Intuitionistic Logic）**。在经典逻辑的情况下，自然演绎并没有特别好的性质。
- **片面性**：它对语言的部分（**合取 $\land$、蕴涵 $\Rightarrow$、全称量词 $\forall$**）处理得比较好，但对于**析取 $\lor$**和**存在量词 $\exists$**，直到第10章才进行讨论。然而，**析取**和**存在**恰恰是最典型的直觉主义连接词。

**自然演绎的基本思想**是一个**不对称性**：

- **证明结构**：证明被视为一个模糊的**树状结构**，具有一个或多个**假设**（可能没有假设），但只有一个**结论**。
- **对称性体现**：演算的深层对称性通过**引入规则（Introduction Rules）**和**消去规则（Elimination Rules）**来展示，这些规则彼此精确匹配。

需要注意的是：

- 由于证明是树状结构，我们总是可以**唯一地确定**最后使用的规则。
- 如果存在多个结论，这种唯一性将无法保证。

---

### **2.1 演算**

#### **证明的表示方式**

我们使用以下符号表示一个证明：

$$
\vdots \\
A
$$

- 这表示一个以$A$为结论的**推导**。
- 证明被写成一个有限的**树形结构**，特别地，树的**叶子节点**标记着**句子**。
- 这些句子有两种可能的状态：**活的（Alive）**或**死的（Dead）**。

#### **活的和死的句子**

- **活的句子**：在通常情况下，一个句子是活的，即它在证明中起着积极的作用，我们称之为**假设（Hypothesis）**。
  
  - 典型情况是自然演绎的第一个规则，它允许我们形成仅包含一个句子的推导：

    $$
    A
    $$

    - 这里，$A$既是叶子节点，也是根节点；逻辑上，我们推导出$A$，但这是因为我们**假设**了$A$。

- **死的句子**：一个句子可以是死的，当它不再在证明中起积极作用。**死的句子**是通过**杀死（Discharge）**活的句子得到的。

  - 典型的例子是**蕴涵引入规则（$\Rightarrow$-Introduction Rule）**：

    $$
    \begin{array}{c}
    [A] \\
    \vdots \\
    B \\
    \hline
    A \Rightarrow B
    \end{array}
    $$

    - 理解方式：
      - 从$B$的一个推导开始，其中我们选择若干次出现的$A$作为假设（次数是任意的：0，1，250，...）。
      - 我们形成一个新的推导，其结论是$A \Rightarrow B$，但其中所有这些$A$的出现都被**消除（Discharged）**了，即被杀死了。
      - 可能还有其他的$A$的出现，我们选择不消除它们。

#### **树形结构的幻觉**

- 这个规则很好地说明了树形符号的幻觉：
  - **重要性**：知道一个假设何时被消除是至关重要的，因此必须记录这一点。
  - **问题**：如果我们在上面的例子中记录这种消除，我们必须将被划掉的$A$与$\Rightarrow$引入规则的横线连接起来；但这已经不再是真正的树形结构了！

---

### **2.1.1 规则**

#### **假设规则（Hypothesis Rule）**

- **形式**：

  $$
  A
  $$

  - 直接假设$A$为真。

#### **引入规则（Introduction Rules）**

1. **合取引入（$\land$-Introduction）**：

   $$
   \begin{array}{c}
   \vdots \\
   A \\
   \end{array}
   \quad
   \begin{array}{c}
   \vdots \\
   B \\
   \end{array}
   \quad
   \begin{array}{c}
   \hline
   A \land B
   \end{array}
   $$

   - **解释**：如果我们分别证明了$A$和$B$，那么我们就可以证明$A \land B$。

2. **蕴涵引入（$\Rightarrow$-Introduction）**：

   $$
   \begin{array}{c}
   [A] \\
   \vdots \\
   B \\
   \hline
   A \Rightarrow B
   \end{array}
   $$

   - **解释**：假设$A$，然后推导出$B$，那么我们就可以得出$A \Rightarrow B$。
   - **注意**：这里的$[A]$表示$A$作为假设被使用，但在应用$\Rightarrow$引入规则后，$A$被**消除**了。

3. **全称量词引入（$\forall$-Introduction）**：

   $$
   \begin{array}{c}
   \vdots \\
   A \\
   \hline
   \forall \xi . A
   \end{array}
   $$

   - **解释**：如果$A$对于任意$\xi$成立，我们就可以推导出$\forall \xi . A$。
   - **限制条件**：在应用$\forall$引入规则时，变量$\xi$不能在任何假设中是自由的（但可以在被消除的假设中自由出现）。

#### **消去规则（Elimination Rules）**

1. **合取消去1（$\land$-Elimination 1）**：

   $$
   \begin{array}{c}
   \vdots \\
   A \land B \\
   \hline
   A
   \end{array}
   $$

   - **解释**：从$A \land B$可以推出$A$。

2. **合取消去2（$\land$-Elimination 2）**：

   $$
   \begin{array}{c}
   \vdots \\
   A \land B \\
   \hline
   B
   \end{array}
   $$

   - **解释**：从$A \land B$可以推出$B$。

3. **蕴涵消去（$\Rightarrow$-Elimination，也称为“** **模式推论（Modus Ponens）**”）**：

   $$
   \begin{array}{c}
   \vdots \\
   A \\
   \end{array}
   \quad
   \begin{array}{c}
   \vdots \\
   A \Rightarrow B \\
   \hline
   B
   \end{array}
   $$

   - **解释**：已知$A$成立，且$A \Rightarrow B$成立，则$B$成立。

4. **全称量词消去（$\forall$-Elimination）**：

   $$
   \begin{array}{c}
   \vdots \\
   \forall \xi . A \\
   \hline
   A[a / \xi]
   \end{array}
   $$

   - **解释**：从$\forall \xi . A$可以推出$A[a / \xi]$，其中$a$是解释域中的一个元素。

#### **备注**

- **关于假设的保留**：
  - 除了$\Rightarrow$引入规则外，所有规则都**保留**了假设的集合。
  - 例如，在应用$\Rightarrow$消去规则得到的推导中，其假设集合是两个直接子推导的假设的并集。

- **关于$\forall$引入规则的限制**：
  - 出于众所周知的逻辑原因，必须将$\forall$引入规则限制在变量$\xi$不在任何**活的假设**中自由出现的情况。
  - 这意味着$\xi$可以在**死的假设**中自由出现。

- **系统的基本对称性**：
  - 系统的对称性体现在**引入规则**和**消去规则**之间的对称，这取代了在这种情况下无法实现的**假设/结论对称性**。

---

### **2.2 计算意义**

#### **Heyting语义视角下的自然演绎**

我们从**Heyting语义**的角度重新审视自然演绎。假设：

- **原子公式**的解释和**量词的范围**是固定的。

在这种框架下：

- **公式$A$**被视为其可能的**证明的集合**。
- **记号**：与其说“$\delta$证明了$A$”，我们不如说“$\delta \in A$”，即$\delta$是$A$的一个证明。

#### **证明与函数的对应关系**

自然演绎的规则表现为一种特殊的构造函数的方式：

- **推导表示函数**：
  - 一个基于假设$B_1, B_2, ..., B_n$推导出$A$的证明，可以被看作一个函数$t[x_1, x_2, ..., x_n]$。
  - 该函数将元素$b_i \in B_i$映射到结果$t[b_1, b_2, ..., b_n] \in A$。

- **关于假设的“包裹”（Parcels of Hypotheses）**：
  - 为了使这种对应关系精确，我们需要处理**假设的包裹**：
    - **同一个公式$B$**可能在假设中出现多次。
    - **同一包裹中的$B$的两个出现**将对应于**同一个变量**。

#### **规则的解释**

1. **假设规则（Hypothesis Rule）**

   - **表示**：一个仅包含假设$A$的推导被表示为变量$x$，其中$x$是$A$的一个元素的变量。
   - **变量选择**：
     - 如果我们稍后还有其他的$A$出现，我们可以选择相同的$x$，或另一个变量，这取决于这些$A$是否在**同一个包裹**中。

2. **合取引入（$\land$-Introduction）**

   - **假设**：
     - 有两个推导，分别对应于$u[x_1, ..., x_n]$和$v[x_1, ..., x_n]$。
     - 这两个推导可能依赖于相同的变量，因为某些假设包裹是相同的。
   - **构造**：
     - 我们将这两个推导组合，关联到我们的推导上，形成一个**有序对**$\langle u[x_1, ..., x_n], v[x_1, ..., x_n] \rangle$。
     - 因为合取的证明是由两个部分组成的。

3. **合取消去（$\land$-Elimination）**

   - **对于$\land$消去1（$\land$-Elimination 1）**：
     - 如果前一步的推导关联了一个项$t[x_1, ..., x_n]$（这是一个有序对），那么我们将**第一投影**$\pi_1 t[x_1, ..., x_n]$关联到我们的推导上。
     - **解释**：$\pi_1$是取有序对的第一个元素的函数。
   - **对于$\land$消去2（$\land$-Elimination 2）**：
     - 使用**第二投影**$\pi_2 t[x_1, ..., x_n]$，取有序对的第二个元素。

   - **基本等式**：

     $$
     \pi_1 \langle u, v \rangle = u \\
     \pi_2 \langle u, v \rangle = v \\
     \langle \pi_1 t, \pi_2 t \rangle = t
     $$

     - 这些等式是逻辑与计算机科学之间对应关系的精髓。

4. **蕴涵引入（$\Rightarrow$-Introduction）**

   - **假设**：
     - 设$v$是与直接子推导关联的项。
     - 该子推导在假设包裹的层面上被明确确定，即一个完整的$A$包裹已被消除。
     - $x$是与这个包裹关联的变量。
     - 因此，我们有一个函数$v[x, x_1, ..., x_n]$。
   - **构造**：
     - 我们将一个新函数$t[x_1, ..., x_n]$关联到我们的推导上，它将$A$的每个参数$a$映射到$v[a, x_1, ..., x_n]$。
     - **记号**：用$\lambda x . v[x, x_1, ..., x_n]$表示，其中$x$是绑定的变量。
     - **注意**：**绑定**对应于**消除**。

5. **蕴涵消去（$\Rightarrow$-Elimination）**

   - **假设**：
     - 有两个项$t[x_1, ..., x_n]$和$u[x_1, ..., x_n]$，分别与两个直接子推导关联。
   - **构造**：
     - 对于固定的$x_1, ..., x_n$的值，$t$是从$A$到$B$的函数，$u$是$A$的一个元素。
     - 因此，$t(u)$是$B$中的一个元素。
     - **表示**：$t[x_1, ..., x_n] \ u[x_1, ..., x_n]$。

   - **基本等式**：

     $$
     (\lambda x . v) \ u = v[u / x] \\
     \lambda x . t \ x = t \quad (\text{当} x \text{在} t \text{中不自由出现时})
     $$

     - 这些等式反映了$\lambda$抽象和函数应用的性质。

6. **全称量词规则（$\forall$ Rules）**

   - **引入和消去规则**与蕴涵的规则类似，因此不再详细讨论。
   - **后续**：我们将用更具新颖性的**二阶量词**取代一阶量词$\forall$。

---

### **2.2.2 推导的同一性**

回到自然演绎，我们写下的等式导致了**推导之间的等式关系**。例如：

1. **第一组等式**

   $$
   \begin{array}{c}
   \vdots \\
   A \\
   \vdots \\
   B \\
   \hline
   A \land B \\
   \hline
   A
   \end{array}
   \quad \text{“等于”} \quad
   \begin{array}{c}
   \vdots \\
   A \\
   \end{array}
   $$

   - **解释**：通过合取引入和合取消去，我们可以直接得到$A$。

2. **第二组等式**

   $$
   \begin{array}{c}
   \vdots \\
   A \\
   \vdots \\
   B \\
   \hline
   A \land B \\
   \hline
   B
   \end{array}
   \quad \text{“等于”} \quad
   \begin{array}{c}
   \vdots \\
   B \\
   \end{array}
   $$

   - **解释**：同理，我们可以直接得到$B$。

3. **第三组等式**

   $$
   \begin{array}{c}
   \vdots \\
   B \\
   \vdots \\
   A \\
   [A] \\
   \vdots \\
   B \\
   \hline
   A \Rightarrow B \\
   \hline
   B
   \end{array}
   \quad \text{“等于”} \quad
   \begin{array}{c}
   \vdots \\
   A \\
   \vdots \\
   B \\
   \end{array}
   $$

   - **解释**：通过假设$A$并推导出$B$，然后应用$\Rightarrow$引入和消去规则，我们可以回到$B$。

在最后一个例子中，需要仔细观察发生了什么：

- **消除的假设**：所有被消除的假设都被$A$的推导（的副本）所替换。

---

### **总结**

本章深入探讨了**自然演绎**的概念及其计算意义。主要内容包括：

- **自然演绎的基本思想**：证明被视为一个树形结构，具有多个假设和单一结论。
- **规则的定义**：详细介绍了假设规则、引入规则和消去规则，以及这些规则如何应用于推导中。
- **计算意义**：从Heyting语义的角度，将证明视为构造函数的过程，解释了自然演绎规则与函数构造之间的对应关系。
- **推导的同一性**：通过等式，展示了不同推导之间的等价关系，这些等式反映了逻辑与计算机科学之间的深层联系。

本章为理解逻辑证明与计算过程之间的关系奠定了基础，后续章节将进一步探讨更复杂的逻辑系统和类型理论。

---

### **后续展望**

- **二阶量词**：在后续的讨论中，我们将用二阶量词取代一阶量词，以探讨更丰富的逻辑结构和性质。
- **深入理解自然演绎**：进一步研究自然演绎的性质，以及如何在计算机科学中应用这些概念，例如在类型系统和程序验证中。

如果您对本章内容有任何疑问，或者需要进一步的解释，请随时提出来！

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