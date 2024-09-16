[toc]



### 第46章 层次结构与参数化 (Hierarchy and Parameterization)

#### **46.1 层次结构 (Hierarchy)**

在模块化编程中，为了达到足够的表达能力，模块系统必须支持**模块层次结构（Module Hierarchies）**的构建。层次结构在编程中自然产生，既作为将大型程序划分为可管理部分的组织工具，又作为在一个类型抽象或类型类之上层叠另一个的本地化工具。在这种场景下，下层模块相对于上层模块扮演辅助角色，上层模块可以被视为被下层模块参数化，即任何下层模块的实现都会诱发与之对应的上层模块实例。

**层次结构（Hierarchy）**与**参数化（Parameterization）**协同工作，为组织程序提供了一种富有表现力的语言。以下将详细解析层次结构的概念、其在模块系统中的实现方式以及通过具体实例来说明其运作机制。

---

### **层次结构的定义与实现**

#### **模块层次结构 (Module Hierarchy)**

**模块层次结构**允许在一个模块类（type class）或类型抽象之上构建另一个模块类或类型抽象。这样做的好处在于，可以在更高层次上定义模块的行为，同时依赖于更低层次模块提供的基础功能。这种分层的方式有助于模块系统的组织和扩展。

#### **层次结构的构建方式**

层次结构的构建通过**子签名关系（Subsignature Relationship）**和**共享传播（Sharing Propagation）**机制来实现。这些机制确保了模块之间的依赖关系和类型约束得以正确管理，从而维护了模块系统的类型安全性和一致性。

---

### **层次结构的实例解析：有序类型与字典模块**

让我们通过一个具体的例子来理解模块层次结构的构建与依赖管理。

#### **1. 定义有序类型类 (Ordered Type Class)**

首先，定义一个类型类，用于描述**有序类型（Ordered Types）**。有序类型类是对**等价类型类（Equality Type Class）**的扩展，增加了一个用于比较元素大小的二元操作。

- **等价类型类签名（Equality Type Signature）**：

  $$
  \sigma_{\text{eq}} = [t :: T;\ \text{heq} : (t \times t) \rightarrow \text{bool}]
  $$

  - **$t :: T$**：类型变量 $t$，种类为 $T$，表示等价类型。
  - **$\text{heq}$**：二元等价操作，用于判断两个类型 $t$ 的值是否相等。

- **有序类型类签名（Ordered Type Signature）**：

  $$
  \sigma_{\text{ord}} = [t :: T;\ \text{heq} : (t \times t) \rightarrow \text{bool},\ \text{hlt} : (t \times t) \rightarrow \text{bool}]
  $$

  - **$\text{hlt}$**：二元严格比较操作，用于判断第一个类型 $t$ 的值是否严格小于第二个。

**注意**：$\sigma_{\text{ord}}$ 是 $\sigma_{\text{eq}}$ 的子签名，因为它在 $\sigma_{\text{eq}}$ 的基础上增加了新的操作。

#### **2. 实例化有序类型类**

创建一个自然数类型的有序类型类实例：

- **自然数有序类型类实例签名（Ordered Natural Numbers Instance Signature）**：

  $$
  \sigma_{\text{natord}} = [t :: S(\text{nat});\ \text{heq} : (t \times t) \rightarrow \text{bool},\ \text{leq} : (t \times t) \rightarrow \text{bool}]
  $$

  - **$t :: S(\text{nat})$**：使用单例种类 $S(\text{nat})$，确保类型 $t$ 具体为自然数类型。
  - **$\text{leq}$**：自然数的比较操作，定义了自然数的有序性。

- **自然数有序类型类实例模块值（Ordered Natural Numbers Instance Module Value）**：

  $$$haskell
  [nat; heq → ..., leq → ...]
  $$$

  - **nat**：具体的自然数类型。
  - **heq** 和 **leq**：具体实现的等价和比较操作。

#### **3. 字典模块的定义与实现**

基于有序类型类，定义一个字典模块，该模块依赖于键类型具备等价和有序操作。

- **字典模块签名（Dictionary Module Signature）**：

  $$
  \sigma_{\text{dict}} = [t :: T;\ \text{hemp} : t,\ \text{ins} : \tau_{\text{key}} \times \tau_{\text{val}} \times t \rightarrow t,\ \text{fnd} : \tau_{\text{key}} \times t \rightarrow \tau_{\text{val}} \ \text{opti}]
  $$

  - **$\text{hemp}$**：创建空字典的操作。
  - **$\text{ins}$**：插入键值对的操作。
  - **$\text{fnd}$**：查找值的操作。

- **层次化签名（Hierarchical Signature）**：

  为了将有序类型类层叠到字典模块上，定义一个层次化签名 $\sigma_{\text{eqord}}$：

  $$
  \sigma_{\text{eqord}} = \Sigma X : \sigma_{\text{eq}}.\ \sigma_X^{\text{ord}}
  $$

  - **$\sigma_X^{\text{ord}}$**：

    $$
    \sigma_X^{\text{ord}} = [t :: S(X \cdot s);\ \text{hlt} : (t \times t) \rightarrow \text{bool}]
    $$

    - **$X \cdot s$**：提取模块 $X$ 的静态部分，作为类型 $t$ 的具体类型。

- **层次化签名的解释**：

  - **$\Sigma X : \sigma_{\text{eq}}.\ \sigma_X^{\text{ord}}$**：表示签名 $\sigma_{\text{eqord}}$ 包含两个部分：
    1. **$X : \sigma_{\text{eq}}$**：下层模块，定义了等价操作。
    2. **$\sigma_X^{\text{ord}}$**：上层模块，基于下层模块的等价操作，定义了有序操作。

#### **4. 模块的实例化与共享传播**

- **模块实例化（Module Instantiation）**：

  创建一个有序自然数类型的字典模块实例：

  $$$haskell
  let Mnatord = [nat; heq → ..., leq → ...] : σnatord
  let Mnatdict = MX_bstdict \upharpoonright σ_X dict
  $$$

  - **$Mnatord$**：自然数有序类型类实例，具有签名 $\sigma_{\text{natord}}$。
  - **$Mnatdict$**：字典模块，绑定并封装实现，具有签名 $\sigma_{\text{natdict}}$。

- **共享传播（Sharing Propagation）**：

  通过**共享传播**机制，消除签名中的模块变量依赖，实现类型的独立性：

  1. **从层次化签名到具体签名**：

     $$
     \sigma_{\text{eqord}} <: \sigma_{\text{eqord}}
     $$

     - 通过共享传播，将 $X \cdot s$ 替换为具体类型 $nat$，得到一个封闭的签名 $\sigma_{\text{natord}}$：

       $$
       \sigma_{\text{natord}} = \Sigma X : \sigma_{\text{nateq}}.\ \sigma_{\text{natord}}
       $$

  2. **最终签名**：

     通过多轮共享传播，最终得到一个完全独立的签名：

     $$
     \rho_{\text{natord}} = [\ :: S(\text{nat});\ \text{hlt} : (\text{nat} \times \text{nat}) \rightarrow \text{bool}]
     $$

     - **$\rho_{\text{natord}}$**：封闭的签名，不再依赖于模块变量 $X$，确保类型的独立性。

#### **5. 模块的投影与子模块**

- **投影操作（Projection）**：

  模块可以通过投影操作提取其组成部分：

  - **第一投影（First Projection）**：

    $$
    M \cdot 1 : \sigma_{\text{eq}}
    $$

    - 提取模块 $M$ 的第一个组件，得到一个子模块 $M \cdot 1$，其签名为 $\sigma_{\text{eq}}$。

  - **第二投影（Second Projection）**：

    $$
    M \cdot 2 : \sigma_X^{\text{ord}}
    $$

    - 提取模块 $M$ 的第二个组件，得到模块 $M \cdot 2$，其签名依赖于第一个组件。

- **子模块的可投影性（Projectibility of Submodules）**：

  - **可投影**：当第二组件的签名 $\sigma_X^{\text{ord}}$ 的依赖可以通过共享传播消除时，第二投影 $M \cdot 2$ 是可投影的，具有封闭的签名。
  
  - **不可投影**：如果第二组件的签名依赖于第一个组件的动态部分（如封装模块），则无法通过共享传播消除依赖，导致第二投影 $M \cdot 2$ 无法形成有效的签名。

**关键点**：

- **子模块**：层次结构中的下层模块（第一组件）称为**子模块（Submodule）**，其操作和类型独立于上层模块。
  
- **依赖性**：上层模块（第二组件）依赖于下层模块的静态部分，实现了模块间的类型约束和行为扩展。

---

### **46.2 参数化 (Parameterization)**

#### **参数化的定义与作用**

**参数化（Parameterization）** 是模块系统中描述一个模块依赖于另一个模块实现的机制。通过参数化，一个模块可以被视为另一个模块实现的函数，即模块的实现被视为另一个模块实现的函数值。这样，模块的构建可以动态地根据其依赖模块的具体实现进行调整。

**参数化** 与 **层次结构** 密切相关，共同提供了一种灵活且表达力强的方式来组织和管理程序中的模块依赖关系。

#### **参数化的实现方式**

参数化通过**抽象机制**，允许一个模块的实现依赖于另一个模块的实现。具体来说，一个模块可以接受另一个模块作为参数，利用其提供的功能来实现自身的操作。这种机制类似于高阶函数接受函数作为参数。

---

### **参数化与层次结构的结合**

参数化与层次结构结合使用，可以构建出高度模块化和可重用的程序结构。通过这种结合，模块系统不仅支持模块之间的层次依赖，还允许模块根据不同的依赖实现进行实例化，从而实现灵活的代码复用和扩展。

#### **参数化的具体实例：基于有序键的字典模块**

让我们通过一个具体的例子来说明参数化与层次结构的结合如何工作。

##### **1. 定义基于有序键的字典模块签名**

- **字典模块签名（Dictionary Module Signature）**：

  $$
  \sigma_{\text{keydict}} = \Sigma X : \sigma_{\text{eqord}}.\ \sigma_X^{\text{dict}}
  $$

  - **$\sigma_X^{\text{dict}}$**：

    $$
    \sigma_X^{\text{dict}} = [t :: T;\ \text{hemp} : t,\ \text{ins} : X \cdot s \times \tau \times t \rightarrow t,\ \text{fnd} : X \cdot s \times t \rightarrow \tau \ \text{opti}]
    $$

    - **$X \cdot s$**：提取模块 $X$ 的静态部分，作为字典的键类型。

##### **2. 模块的实现与参数化**

- **字典模块的实现（Dictionary Module Implementation）**：

  $$$haskell
  [Meq; Mordi]
  $$$

  - **$Meq$**：实现了 $\sigma_{\text{eqord}}$ 的模块，提供了键类型的等价和有序操作。
  - **$Mordi$**：实现了 $\sigma_X^{\text{dict}}$ 的模块，具体实现字典操作，依赖于 $Meq$ 提供的键类型和操作。

##### **3. 参数化的效果**

- **参数化关系**：

  字典模块的实现 $Mdict$ 通过参数化接受一个有序类型类实例 $X$，从而生成特定键类型的字典实例。具体来说，$Mdict$ 的类型依赖于传入的 $X$ 模块的实现。

- **共享传播**：

  通过共享传播机制，模块 $Mdict$ 在签名中将 $X \cdot s$ 替换为具体的键类型，如 $nat$，确保字典模块的类型独立于具体的键类型实现。

---

### **参数化的优势与应用**

#### **1. 提高模块的可复用性**

通过参数化，模块可以在不同的上下文中被复用，只需提供不同的参数模块即可。例如，字典模块可以通过传入不同的有序类型类实例，实现针对不同键类型的字典。

#### **2. 支持模块的动态组合**

参数化允许模块在运行时根据需要动态组合，增强了程序的灵活性。例如，可以根据配置或运行时条件选择不同的模块实现，构建出适应不同需求的系统。

#### **3. 促进代码的分层与组织**

通过参数化与层次结构的结合，程序可以被组织为多个层次，每一层依赖于其下层提供的基础功能。这种分层的方式有助于管理复杂性，提升代码的可维护性和可扩展性。

---

### **总结**

- **层次结构（Hierarchy）**：
  - 允许在一个模块类或类型抽象之上构建另一个模块类或类型抽象。
  - 通过子签名关系和共享传播机制，管理模块之间的依赖关系和类型约束。
  - 通过具体实例（如有序自然数字典），展示了模块层次结构的构建与依赖管理。

- **参数化（Parameterization）**：
  - 描述一个模块依赖于另一个模块实现的机制。
  - 通过参数化，模块的实现可以动态地根据其依赖模块的具体实现进行调整。
  - 结合层次结构，提供了灵活且富有表现力的模块组织方式。

- **层次结构与参数化的结合**：
  - 提高模块的可复用性和灵活性。
  - 支持模块的动态组合和分层组织。
  - 通过具体实例，展示了如何利用参数化与层次结构构建复杂的模块系统。

- **类型系统的支持**：
  - 子签名关系确保了模块签名的继承和扩展。
  - 共享传播机制消除签名中的模块变量依赖，实现类型独立性和表示独立性。

通过深入理解层次结构与参数化的概念及其在模块系统中的实现方式，开发者能够构建出高效、可维护且灵活的模块化程序结构，提升系统的整体质量和扩展能力。

---

### **进一步阅读**

- **MacQueen, B. (1986)**：首次提出使用依赖类型表达模块化的概念，奠定了模块系统设计的基础。
  
- **Harper, R., Lillibridge, J., & others**：
  
  - **Harper et al. (1990)**：扩展依赖类型应用于阶段区分。
  
  - **Harper 和 Lillibridge (1994)**：结合类型抽象与类型类，提出避免问题。
  
- **Leroy, X. (1994)**：进一步发展类型抽象与类型类的理论，提出自我识别规则。
  
- **Stone, M., & Harper, R. (2006)**：将自我识别规则与高阶单例联系起来。
  
- **Lee, H., et al. (2007)**：基于前人研究，构建模块系统的机理化理论。
  
- **Dreyer, D. (2005)**：总结模块系统设计中的主要问题，提供全面的模块化系统设计概述。
  
- **Rossberg, A., et al. (2010)**：基于Elaboration方法，提出模块化构造的严格类型理论表述。
  
- **Cardagna, E., & Pierce, B. C. (1994)**：首次明确提出避免问题，强调类型系统在模块化设计中的重要性。
  
- **Milner, R., Tofte, I., & Harper, R. (1997)**：**Definition of Standard ML**，作为Elaboration方法的代表性实例，详细描述了模块系统的实现。

---

通过对**层次结构**与**参数化**的深入分析与实例解析，我们能够更好地理解模块系统在现代编程语言中的重要性和应用。掌握这些概念有助于设计和实现高效、灵活且可维护的模块化程序结构，提升系统的整体质量和扩展能力。

### ---------------------------------

### 第46章 层次结构与参数化 (Hierarchy and Parameterization)

#### **46.2 参数化 (Parameterization)**

在模块系统中，**参数化（Parameterization）** 是描述模块依赖关系的一种关键机制。参数化允许模块的实现依赖于其他模块的实现，使得模块可以根据不同的依赖模块动态地进行实例化。这不仅增强了模块系统的灵活性和可复用性，还支持复杂的模块层次结构的构建。

本节将深入探讨参数化的概念、实现方式以及其在模块层次结构中的应用。通过具体的实例，我们将展示如何利用参数化机制构建通用且可扩展的模块，实现不同模块之间的依赖和组合。

---

### **参数化的基本概念**

#### **参数化模块与函子（Functor）**

**参数化模块**，通常称为**函子（Functor）**，是模块系统中的一种高级构造。函子本质上是一个模块函数，它接受一个模块作为参数，并返回一个新的模块。通过这种方式，函子可以根据输入模块的具体实现生成相应的输出模块，实现了模块的高度抽象和可复用性。

- **定义**：
  - **函子**：一个模块，它接受一个符合特定签名的模块作为输入，并返回一个符合另一个签名的模块作为输出。
  
  - **表示形式**：
    $$
    \lambda Z : \sigma_{\text{eqord}} . \text{Mkeydict}
    $$
    其中，$\text{Mkeydict}$ 是一个通用的字典模块实现，依赖于输入模块 $Z$，其签名为 $\sigma_{\text{eqord}}$。

- **签名**：
  - **函子签名（Functor Signature）**：
    $$
    \sigma_{\text{dictfun}} = \Pi Z : \sigma_{\text{eqord}} . \rho_Z^{\text{keydict}}
    $$
    其中：
    - **域签名（Domain Signature）**：$\sigma_{\text{eqord}}$，描述了输入模块 $Z$ 必须满足的签名要求（如有序类型类）。
    - **域签名依赖的结果签名（Range Signature）**：$\rho_Z^{\text{keydict}}$，描述了输出模块的签名，它依赖于输入模块 $Z$ 的具体实现。

---

### **参数化模块的实例解析**

#### **1. 字典模块的参数化实现**

假设我们希望实现一个**字典模块**，其键类型必须是有序类型。为了实现这一点，我们可以定义一个参数化模块（函子）$Mdictfun$，该函子接受一个有序类型类实例作为参数，并返回一个专门用于该键类型的字典模块。

- **字典模块签名（Dictionary Module Signature）**：
  $$
  \sigma_{\text{keydict}} = \Sigma X : \sigma_{\text{eqord}}.\ \sigma_X^{\text{dict}}
  $$
  
  其中：
  - **$\sigma_X^{\text{dict}}$**：
    $$
    \sigma_X^{\text{dict}} = [t :: T;\ \text{hemp} : t,\ \text{ins} : X \cdot s \times \tau \times t \rightarrow t,\ \text{fnd} : X \cdot s \times t \rightarrow \tau \ \text{opti}]
    $$
    - **$X \cdot s$**：提取模块 $X$ 的静态部分，作为字典的键类型。
    - **$\text{hemp}$**：创建空字典的操作。
    - **$\text{ins}$**：插入键值对的操作。
    - **$\text{fnd}$**：查找值的操作。

- **字典函子签名（Dictionary Functor Signature）**：
  $$
  \sigma_{\text{dictfun}} = \Pi Z : \sigma_{\text{eqord}}.\ \rho_Z^{\text{keydict}}
  $$
  其中：
  - **$\rho_Z^{\text{keydict}}$** 是一个依赖于输入模块 $Z$ 的签名，确保输出模块的键类型与 $Z$ 提供的有序类型一致。

#### **2. 函子的实现与实例化**

- **函子实现（Functor Implementation）**：
  
  $$$haskell
  Mdictfun = λ Z : σeqord . Mkeydict
  $$$
  
  - **$Mkeydict$**：一个通用的字典模块实现，依赖于输入模块 $Z$，其签名为 $\sigma_X^{\text{dict}}$。

- **函子实例化（Functor Application）**：
  
  假设我们有一个自然数有序类型类实例 $Mnatord$，其签名为 $\sigma_{\text{natord}}$：
  
  $$$haskell
  Mnatord = [nat; heq → ..., leq → ...] : σnatord
  $$$
  
  我们可以通过将 $Mnatord$ 作为参数应用到 $Mdictfun$ 上，生成一个专门用于自然数键的字典模块 $Mnatdict$：
  
  $$$haskell
  Mnatdict = Mdictfun (Mnatord) : ρnatdict
  $$$
  
  - **解释**：
    - $Mnatdict$ 是 $Mdictfun$ 的实例，专门用于自然数键的字典，实现了 $\rho_{\text{natdict}}$ 签名。
  
  - **结果签名（Result Signature）**：
    $$
    \rho_{\text{natdict}} = [\ :: S(\text{nat});\ \text{hemp} : \text{nat},\ \text{ins} : \text{nat} \times \tau \times \text{nat} \rightarrow \text{nat},\ \text{fnd} : \text{nat} \times \text{nat} \rightarrow \tau \ \text{opti}]
    $$
    - **$\rho_{\text{natdict}}$**：封闭的签名，不再依赖于模块变量 $X$，确保类型的独立性。

---

### **参数化签名的约束与共享传播**

#### **1. 签名修改（Signature Modification）**

为了表达参数化签名中模块变量之间的依赖关系，可以使用**签名修改（Signature Modification）**的机制。这种机制允许在签名中引入类型共享的约束，从而确保模块之间的类型一致性。

- **定义**：
  - **签名修改**：对现有签名施加额外的类型共享约束。
  
  - **表示形式**：
    $$
    Y : \sigma_{\text{keydict}} / Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
    $$
    - **解释**：将签名 $\sigma_{\text{keydict}}$ 修改为要求模块 $Y$ 的键类型与模块 $Z$ 的键类型共享，即 $Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s$。

#### **2. 共享传播（Sharing Propagation）**

**共享传播** 是通过子签名关系和子类型关系，将模块签名中的依赖关系消除，使得输出模块的签名独立于输入模块的具体实现。

- **过程**：
  1. **子签名关系**：利用子签名关系（$\sigma_1 <: \sigma_2$），将层次化签名转化为更具体的签名。
  
  2. **依赖消除**：通过共享传播，将依赖模块的类型信息传播到输出模块的签名中，消除对模块变量的依赖。
  
  3. **签名简化**：最终得到一个独立的、封闭的签名，使得模块实例的签名不再依赖于输入模块的变量。

- **示例**：
  
  - **初始签名**：
    $$
    \sigma_{\text{eqord}} = \Sigma X : \sigma_{\text{eq}}.\ \sigma_X^{\text{ord}}
    $$
  
  - **应用共享传播**：
    - 替换签名中的 $X \cdot s$ 为具体类型 $nat$，得到：
      $$
      \rho_{\text{natord}} = [\ :: S(\text{nat});\ \text{hlt} : (\text{nat} \times \text{nat}) \rightarrow \text{bool}]
      $$
  
  - **结果**：
    - $Mnatdict$ 的签名 $\rho_{\text{natdict}}$ 独立于模块变量 $X$，确保类型的独立性和封装性。

---

### **参数化与层次结构的结合应用**

参数化与层次结构的结合为模块系统提供了强大的表达能力和灵活性。通过层次结构，可以构建模块之间的依赖关系；通过参数化，可以根据不同的依赖模块生成相应的模块实例，实现代码的高度复用和可扩展性。

#### **具体实例解析：基于有序键的字典模块**

让我们通过一个具体实例来综合理解参数化与层次结构的结合应用。

##### **1. 定义有序类型类签名**

- **有序类型类签名（Ordered Type Signature）**：
  $$
  \sigma_{\text{ord}} = [t :: T;\ \text{heq} : (t \times t) \rightarrow \text{bool},\ \text{hlt} : (t \times t) \rightarrow \text{bool}]
  $$
  
  - **$t :: T$**：类型变量 $t$，种类为 $T$，表示有序类型。
  - **$\text{heq}$** 和 **$\text{hlt}$**：定义了类型 $t$ 的等价性和严格小于关系。

##### **2. 定义字典函子签名**

- **字典函子签名（Dictionary Functor Signature）**：
  $$
  \sigma_{\text{dictfun}} = \Pi Z : \sigma_{\text{eqord}}.\ \rho_Z^{\text{keydict}}
  $$
  
  - **$\rho_Z^{\text{keydict}}$**：
    $$
    \rho_Z^{\text{keydict}} = \Sigma Y : \sigma_{\text{keydict}}.\ Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
    $$
    - **共享约束**：确保字典模块的键类型与输入模块 $Z$ 的键类型共享。

##### **3. 函子的实现与应用**

- **函子实现（Functor Implementation）**：
  
  $$$haskell
  Mdictfun = λ Z : σeqord . Mkeydict
  $$$
  
  - **$Mkeydict$**：一个通用的字典模块实现，依赖于输入模块 $Z$，其签名为 $\sigma_{\text{keydict}}$。

- **函子实例化（Functor Application）**：
  
  假设我们有一个自然数有序类型类实例 $Mnatord$：
  
  $$$haskell
  Mnatord = [nat; heq → natHeq, leq → natLeq] : σnatord
  $$$
  
  将 $Mnatord$ 作为参数应用到 $Mdictfun$ 上，生成一个自然数字典模块 $Mnatdict$：
  
  $$$haskell
  Mnatdict = Mdictfun (Mnatord) : ρnatdict
  $$$
  
  - **签名推导**：
    $$
    \rhonatdict = [\ :: S(\text{nat});\ \text{hemp} : \text{nat},\ \text{ins} : \text{nat} \times \tau \times \text{nat} \rightarrow \text{nat},\ \text{fnd} : \text{nat} \times \text{nat} \rightarrow \tau \ \text{opti}]
    $$

##### **4. 签名修改与共享传播**

- **签名修改（Signature Modification）**：
  
  为了简化签名的表达，可以使用签名修改机制：
  
  $$
  Y : \sigma_{\text{keydict}} / Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
  $$
  
  - **解释**：将 $Y$ 的键类型与 $Z$ 的键类型共享，即 $Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s$。

- **共享传播（Sharing Propagation）**：
  
  通过共享传播机制，将签名中的模块变量依赖消除，得到独立的封闭签名：
  
  $$
  \rho_{\text{natdict}} = [\ :: S(\text{nat});\ \text{hemp} : \text{nat},\ \text{ins} : \text{nat} \times \tau \times \text{nat} \rightarrow \text{nat},\ \text{fnd} : \text{nat} \times \text{nat} \rightarrow \tau \ \text{opti}]
  $$
  
  - **效果**：字典模块的签名不再依赖于输入模块的变量 $Z$，确保类型的独立性和封装性。

---

### **参数化签名的详细解析**

#### **1. 签名的依赖性**

- **层次化签名（Hierarchical Signature）**：
  
  $$
  \sigma_{\text{eqord}} = \Sigma X : \sigma_{\text{eq}}.\ \sigma_X^{\text{ord}}
  $$
  
  - **解释**：
    - **$\Sigma X : \sigma_{\text{eq}}.\ \sigma_X^{\text{ord}}$**：表示签名 $\sigma_{\text{eqord}}$ 包含两个部分：
      1. **$X : \sigma_{\text{eq}}$**：定义一个模块变量 $X$，其签名为 $\sigma_{\text{eq}}$。
      2. **$\sigma_X^{\text{ord}}$**：基于模块变量 $X$，定义了有序操作 $\text{hlt}$，其类型为 $(t \times t) \rightarrow \text{bool}$。

- **依赖性与共享**：
  
  签名中的模块变量 $X$ 使得上层签名 $\sigma_X^{\text{ord}}$ 依赖于下层模块 $X$ 的实现。通过共享传播机制，依赖关系可以被消除，使得输出模块的签名独立于输入模块的具体实现。

#### **2. 签名修改的简化表达**

为了避免重复书写复杂的签名修改约束，可以采用更简洁的**签名修改（Signature Modification）**机制，直接表达类型共享的约束关系。

- **签名修改形式**：
  
  $$
  Y : \sigma_{\text{keydict}} / Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
  $$
  
  - **解释**：
    - **$Y : \sigma_{\text{keydict}}$**：模块 $Y$ 必须满足签名 $\sigma_{\text{keydict}}$。
    - **$Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s$**：模块 $Y$ 的键类型与模块 $Z$ 的键类型共享。

#### **3. 子签名关系与签名推导**

利用子签名关系，可以将参数化签名转化为更具体的封闭签名，实现类型的独立性和封装性。

- **子签名关系（Subsignature Relationship）**：
  
  $$
  \sigma_{\text{natord}} <: \sigma_{\text{eqord}}
  $$
  
  - **解释**：签名 $\sigma_{\text{natord}}$ 是签名 $\sigma_{\text{eqord}}$ 的子签名，意味着 $\sigma_{\text{natord}}$ 提供了 $\sigma_{\text{eqord}}$ 所要求的所有功能，并可能提供了更多功能。

- **签名推导过程**：
  
  1. **强化域签名**：
     
     将函子的域签名从 $\sigma_{\text{eqord}}$ 加强到 $\sigma_{\text{natord}}$，确保输入模块 $Z$ 是自然数有序类型类实例。
  
  2. **共享传播**：
     
     通过共享传播，将签名中的依赖关系消除，得到封闭签名 $\rho_{\text{natdict}}$。
  
  3. **应用子签名关系**：
     
     利用子签名关系，将签名 $\rho_{\text{natdict}}$ 应用于具体实例化后的模块，确保类型的独立性。

---

### **参数化签名的应用实例**

#### **1. 自然数有序字典实例**

- **定义**：
  
  假设我们有一个自然数有序类型类实例 $Mnatord$，其签名为 $\sigma_{\text{natord}}$：
  
  $$$haskell
  Mnatord = [nat; heq → natHeq, leq → natLeq] : σnatord
  $$$
  
  - **$nat$**：自然数类型。
  - **$natHeq$** 和 **$natLeq$**：自然数的等价和比较操作实现。

- **函子应用**：
  
  将 $Mnatord$ 作为参数应用到字典函子 $Mdictfun$ 上，生成自然数字典模块 $Mnatdict$：
  
  $$$haskell
  Mnatdict = Mdictfun (Mnatord) : ρnatdict
  $$$
  
  - **解释**：
    - **$\rhonatdict$**：字典模块 $Mnatdict$ 的签名，封闭且独立于模块变量 $Z$。
  
  - **签名推导**：
    $$
    \rhonatdict = [\ :: S(\text{nat});\ \text{hemp} : \text{nat},\ \text{ins} : \text{nat} \times \tau \times \text{nat} \rightarrow \text{nat},\ \text{fnd} : \text{nat} \times \text{nat} \rightarrow \tau \ \text{opti}]
    $$
  
  - **结果**：
    - $Mnatdict$ 具有封闭的签名 $\rhonatdict$，其键类型固定为 $nat$，确保类型的独立性和封装性。

#### **2. 通用字典函子的签名推导**

- **初始函子签名**：
  $$
  \sigma_{\text{dictfun}} = \Pi Z : \sigma_{\text{eqord}}.\ \rho_Z^{\text{keydict}}
  $$
  
- **应用子签名关系**：
  
  由于 $\sigma_{\text{natord}} <: \sigma_{\text{eqord}}$，可以将签名 $\rho_Z^{\text{keydict}}$ 转化为更具体的签名：
  
  $$
  \rho_Z^{\text{keydict}} = \Sigma Y : \sigma_{\text{keydict}}.\ Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
  $$
  
  - **共享传播**：
    
    替换签名中的模块变量 $Y \cdot 1 \cdot 1 \cdot s$ 为具体类型 $nat$，得到封闭签名 $\rhonatdict$：
    
    $$
    \rhonatdict = [\ :: S(\text{nat});\ \text{hemp} : \text{nat},\ \text{ins} : \text{nat} \times \tau \times \text{nat} \rightarrow \text{nat},\ \text{fnd} : \text{nat} \times \text{nat} \rightarrow \tau \ \text{opti}]
    $$
  
  - **结果**：
    
    函子应用后的字典模块 $Mnatdict$ 具有独立且封闭的签名 $\rhonatdict$，确保类型安全和封装性。

---

### **参数化与签名推导的总结**

- **依赖消除**：
  
  通过共享传播和子签名关系，参数化签名中的模块变量依赖得以消除，使得输出模块的签名独立于输入模块的具体实现。

- **类型独立性**：
  
  参数化确保了模块实例的类型独立性，避免了类型系统中因模块实现变化引起的不一致性。

- **签名推导**：
  
  签名推导过程通过子签名关系和共享传播机制，将依赖关系转化为封闭签名，确保类型系统的安全性和一致性。

- **签名修改**：
  
  签名修改机制简化了签名中的类型共享约束表达，避免了重复书写复杂的依赖关系。

---

### **类型安全性的保障**

通过严格的参数化签名推导和共享传播机制，模块系统能够确保类型安全性。这意味着，模块的实现和其依赖关系在类型系统中得到正确管理，避免了因模块依赖不一致导致的类型错误。

- **保持性（Preservation）**：
  
  如果模块符合签名 $\sigma$，则其在类型系统中的推导过程不会改变其类型。

- **进展性（Progress）**：
  
  合法的模块表达式要么已经是值，要么可以继续计算，确保模块系统的运行时安全。

---

### **总结**

- **参数化（Parameterization）**：
  - **定义**：模块系统中描述模块依赖关系的机制，允许模块根据输入模块的实现动态实例化。
  - **实现**：通过函子（Functor）和签名修改机制，实现模块间的依赖和组合。
  - **优势**：
    - **灵活性与可复用性**：模块可以根据不同的依赖模块生成相应的实例，提升代码的复用性。
    - **类型独立性**：通过共享传播和子签名关系，确保模块实例的类型独立于输入模块的具体实现。
    - **组织与扩展**：支持复杂的模块层次结构，促进代码的高内聚低耦合。

- **层次结构（Hierarchy）**：
  - **定义**：模块系统中模块间的层次依赖关系，允许在一个模块类或类型抽象之上构建另一个模块类或类型抽象。
  - **实现**：通过子签名关系和共享传播机制，管理模块之间的依赖和类型约束。
  - **优势**：
    - **组织性**：将大型程序划分为多个层次，提升代码的可管理性。
    - **扩展性**：允许在更高层次上定义模块行为，依赖于更低层次模块提供的基础功能。

- **参数化与层次结构的结合**：
  - **协同工作**：通过结合参数化和层次结构，模块系统能够构建出高度模块化、灵活且可扩展的程序结构。
  - **类型安全**：严格的签名推导和共享传播机制，确保模块间依赖关系的类型安全性和一致性。

通过深入理解参数化与层次结构的概念及其在模块系统中的实现方式，开发者能够设计和实现高效、可维护且灵活的模块化程序结构，提升系统的整体质量和扩展能力。

---

### **进一步阅读**

- **MacQueen, B. (1986)**：首次提出使用依赖类型表达模块化的概念，奠定了模块系统设计的基础。
  
- **Harper, R., Lillibridge, J., & others**：
  - **Harper et al. (1990)**：扩展依赖类型应用于阶段区分。
  - **Harper 和 Lillibridge (1994)**：结合类型抽象与类型类，提出避免问题。
  
- **Leroy, X. (1994)**：进一步发展类型抽象与类型类的理论，提出自我识别规则。
  
- **Stone, M., & Harper, R. (2006)**：将自我识别规则与高阶单例联系起来。
  
- **Lee, H., et al. (2007)**：基于前人研究，构建模块系统的机理化理论。
  
- **Dreyer, D. (2005)**：总结模块系统设计中的主要问题，提供全面的模块化系统设计概述。
  
- **Rossberg, A., et al. (2010)**：基于Elaboration方法，提出模块化构造的严格类型理论表述。
  
- **Cardagna, E., & Pierce, B. C. (1994)**：首次明确提出避免问题，强调类型系统在模块化设计中的重要性。
  
- **Milner, R., Tofte, I., & Harper, R. (1997)**：**Definition of Standard ML**，作为Elaboration方法的代表性实例，详细描述了模块系统的实现。

---

通过对**参数化**与**层次结构**的深入分析与实例解析，我们能够更好地理解模块系统在现代编程语言中的重要性和应用。掌握这些概念有助于设计和实现高效、灵活且可维护的模块化程序结构，提升系统的整体质量和扩展能力。

### ---------------------------------

### **46.3 扩展模块以支持层次结构与参数化 (Extending Modules with Hierarchies and Parameterization)**

在本节中，我们将概述如何扩展第45章介绍的模块语言 **L{mod}**，以支持**模块层次结构（Hierarchies）**和**模块参数化（Parameterization）**。通过这些扩展，模块系统能够更好地组织程序结构，支持复杂的模块依赖关系，并提升代码的复用性和可维护性。

---

### **语法定义 (Syntax Definition)**

为了支持层次结构和参数化，**L{mod}** 的语法被扩展，引入了新的模块和签名构造。具体的扩展如下：

#### **签名语法扩展 (Extended Signature Syntax)**

- **层次化签名（Hierarchical Signature）**：
  
  $$
  \sigma_{\text{hier}} ::= \text{hier}(\sigma_1; X.\sigma_2) \quad \text{（层次化签名）}
  $$
  
  $$
  \sigma_{\text{fun}} ::= \text{fun}(\sigma_1; X.\sigma_2) \quad \text{（函子签名）}
  $$

#### **模块语法扩展 (Extended Module Syntax)**

- **层次化模块（Hierarchical Module）**：
  
  $$
  M_{\text{hier}} ::= \text{hier}(M_1; M_2) \quad \text{（层次化模块）}
  $$
  
  - **解释**：将模块 $M_1$ 和模块 $M_2$ 层次化组合，形成一个层次化模块。

- **模块投影（Module Projection）**：
  
  $$
  \text{fst}(M) ::= M \cdot 1 \quad \text{（第一投影）}
  $$
  
  $$
  \text{snd}(M) ::= M \cdot 2 \quad \text{（第二投影）}
  $$

- **函子定义（Functor Definition）**：
  
  $$
  \text{fun}[\sigma](X.M) ::= \lambda X : \sigma . M \quad \text{（函子定义）}
  $$

- **函子应用（Functor Application）**：
  
  $$
  \text{app}(M_1; M_2) ::= M_1 (M_2) \quad \text{（函子应用）}
  $$

**符号解释：**

- **hier(σ₁; X.σ₂)**：定义一个层次化签名，其中 $σ_1$ 是下层签名，$σ_2$ 是上层签名，依赖于模块变量 $X$。
- **fun(σ₁; X.σ₂)**：定义一个函子签名，接受一个签名 $σ_1$，并返回一个签名 $σ_2$，依赖于模块变量 $X$。
- **hier(M₁; M₂)**：创建一个层次化模块，由模块 $M_1$ 和模块 $M_2$ 组成。
- **fst(M)** 和 **snd(M)**：分别用于提取层次化模块的第一和第二组件。
- **fun[σ](X.M)**：定义一个函子，参数为模块 $X$，其签名为 $σ$，并返回模块 $M$。
- **app(M₁; M₂)**：将函子 $M₁$ 应用于模块 $M₂$，生成一个新的模块实例。

---

### **静态语义 (Statics)**

模块层次结构和参数化的静态语义通过一系列新的类型判断规则来定义。这些规则确保了模块之间的依赖关系和类型约束的正确性。

#### **1. 模块可投影性判定 (Projectibility Judgment)**

**判定** $M$ **是否可投影（Projectible）**，即模块的组成部分是否可以通过投影操作引用其类型。

**判定规则：**

$$
\frac{
  \Gamma, X : \sigma \vdash x \text{ projectible}
}{
  \Gamma \vdash x \text{ projectible}
}
\quad (46.1a)
$$

$$
\frac{
  \Gamma \vdash M_1 \text{ projectible} \quad \Gamma \vdash M_2 \text{ projectible}
}{
  \Gamma \vdash \text{hier}(M_1; M_2) \text{ projectible}
}
\quad (46.1b)
$$

$$
\frac{
  \Gamma \vdash M \text{ projectible}
}{
  \Gamma \vdash M \cdot 1 \text{ projectible}
}
\quad (46.1c)
$$

$$
\frac{
  \Gamma \vdash M \text{ projectible}
}{
  \Gamma \vdash M \cdot 2 \text{ projectible}
}
\quad (46.1d)
$$

**解释：**

- **规则 (46.1a)**：所有模块变量在任何上下文中都被视为可投影的，前提是它们满足相应的签名。
- **规则 (46.1b)**：层次化模块 $hier(M₁; M₂)$ 是可投影的，当且仅当其组成模块 $M₁$ 和 $M₂$ 都是可投影的。
- **规则 (46.1c)** 和 **(46.1d)**：如果模块 $M$ 是可投影的，则其第一和第二组件 $M · 1$ 和 $M · 2$ 也是可投影的。

**限制：**

- **封装模块**：封装模块（Sealed Modules）不被视为可投影的，因为它们隐藏了内部表示，防止通过投影访问内部类型。
- **函子和函子实例**：函子及其实例不被视为可投影的，因为函子的签名依赖于参数模块，无法通过投影直接访问其内部类型。

#### **2. 签名形成判定 (Signature Formation Judgment)**

**判定** 某个签名是否符合语法和类型规则。

**判定规则：**

$$
\frac{
  \Gamma \vdash \sigma_1 \text{ sig} \quad \Gamma, X : \sigma_1 \vdash \sigma_2 \text{ sig}
}{
  \Gamma \vdash \text{hier}(\sigma_1; X.\sigma_2) \text{ sig}
}
\quad (46.2a)
$$

$$
\frac{
  \Gamma \vdash \sigma_1 \text{ sig} \quad \Gamma, X : \sigma_1 \vdash \sigma_2 \text{ sig}
}{
  \Gamma \vdash \text{fun}(\sigma_1; X.\sigma_2) \text{ sig}
}
\quad (46.2b)
$$

**解释：**

- **规则 (46.2a)**：层次化签名 $hier(σ₁; X.σ₂)$ 的形成需要下层签名 $σ₁$ 已经是有效签名，并且在假设模块变量 $X$ 具有签名 $σ₁$ 的上下文中，上层签名 $σ₂$ 也是有效签名。
- **规则 (46.2b)**：函子签名 $fun(σ₁; X.σ₂)$ 的形成类似，需要下层签名 $σ₁$ 和在假设模块变量 $X$ 具有签名 $σ₁$ 的上下文中，上层签名 $σ₂$ 也是有效签名。

#### **3. 签名等价性 (Signature Equivalence)**

**定义**：签名等价性保证了不同形式的签名在类型系统中被视为相同。

**判定规则：**

$$
\frac{
  \Gamma \vdash \sigma_1 \equiv \sigma'_1 \quad \Gamma, X : \sigma_1 \vdash \sigma_2 \equiv \sigma'_2
}{
  \Gamma \vdash \text{hier}(\sigma_1; X.\sigma_2) \equiv \text{hier}(\sigma'_1; X.\sigma'_2)
}
\quad (46.3a)
$$

$$
\frac{
  \Gamma \vdash \sigma_1 \equiv \sigma'_1 \quad \Gamma, X : \sigma_1 \vdash \sigma_2 \equiv \sigma'_2
}{
  \Gamma \vdash \text{fun}(\sigma_1; X.\sigma_2) \equiv \text{fun}(\sigma'_1; X.\sigma'_2)
}
\quad (46.3b)
$$

**解释：**

- **规则 (46.3a)**：层次化签名 $hier(σ₁; X.σ₂)$ 与 $hier(σ'₁; X.σ'₂)$ 等价，当且仅当下层签名 $σ₁$ 与 $σ'₁$ 等价，并且在假设模块变量 $X$ 具有签名 $σ₁$ 的上下文中，上层签名 $σ₂$ 与 $σ'_₂$ 等价。
- **规则 (46.3b)**：函子签名 $fun(σ₁; X.σ₂)$ 与 $fun(σ'₁; X.σ'₂)$ 等价，当且仅当下层签名 $σ₁$ 与 $σ'₁$ 等价，并且在假设模块变量 $X$ 具有签名 $σ₁$ 的上下文中，上层签名 $σ₂$ 与 $σ'_₂$ 等价。

#### **4. 子签名关系 (Subsignature Judgment)**

**定义**：子签名关系描述了一个签名是否可以被视为另一个签名的子集，即是否满足继承关系。

**判定规则：**

$$
\frac{
  \Gamma \vdash \sigma_1 <: \sigma'_1 \quad \Gamma, X : \sigma_1 \vdash \sigma_2 <: \sigma'_2
}{
  \Gamma \vdash \text{hier}(\sigma_1; X.\sigma_2) <: \text{hier}(\sigma'_1; X.\sigma'_2)
}
\quad (46.4a)
$$

$$
\frac{
  \Gamma \vdash \sigma_1 <: \sigma'_1 \quad \Gamma, X : \sigma_1 \vdash \sigma_2 <: \sigma'_2
}{
  \Gamma \vdash \text{fun}(\sigma_1; X.\sigma_2) <: \text{fun}(\sigma'_1; X.\sigma'_2)
}
\quad (46.4b)
$$

**解释：**

- **规则 (46.4a)**：层次化签名 $hier(σ₁; X.σ₂)$ 是 $hier(σ'₁; X.σ'₂)$ 的子签名，当且仅当下层签名 $σ₁$ 是 $σ'₁$ 的子签名，并且在假设模块变量 $X$ 具有签名 $σ₁$ 的上下文中，上层签名 $σ₂$ 是 $σ'₂$ 的子签名。
- **规则 (46.4b)**：函子签名 $fun(σ₁; X.σ₂)$ 是 $fun(σ'₁; X.σ'₂)$ 的子签名，当且仅当下层签名 $σ₁$ 是 $σ'₁$ 的子签名，并且在假设模块变量 $X$ 具有签名 $σ₁$ 的上下文中，上层签名 $σ₂$ 是 $σ'₂$ 的子签名。

**关键点：**

- **层次化签名的协变性**：层次化签名在两个位置上都是协变的，即子签名关系在下层和上层签名中都保持。
- **函子签名的协变性**：函子签名在其域和范围上也保持协变，确保函子的输入和输出签名的一致性。

#### **5. 模块表达式的静态语义 (Statics of Module Expressions)**

模块表达式的静态语义通过以下规则进行定义，确保模块的类型与签名相匹配。

**模块层次化表达式的类型判断：**

$$
\frac{
  \Gamma \vdash M_1 : \sigma_1 \quad \Gamma \vdash M_2 : \sigma_2
}{
  \Gamma \vdash \text{hier}(M_1; M_2) : \Sigma X : \sigma_1.\ \sigma_2
}
\quad (46.5a)
$$

**解释：**

- **规则 (46.5a)**：层次化模块 $hier(M₁; M₂)$ 的类型是一个层次化签名 $Σ X : σ₁. σ₂$，其中 $M₁$ 的类型是 $σ₁$，$M₂$ 的类型是 $σ₂$。

**模块投影的类型判断：**

$$
\frac{
  \Gamma \vdash M : \Sigma X : \sigma_1.\ \sigma_2
}{
  \Gamma \vdash M \cdot 1 : \sigma_1
}
\quad (46.5b)
$$

$$
\frac{
  \Gamma \vdash M : \Sigma X : \sigma_1.\ \sigma_2
}{
  \Gamma \vdash M \cdot 2 : \sigma_2
}
\quad (46.5c)
$$

**解释：**

- **规则 (46.5b)**：对于层次化模块 $hier(M₁; M₂)$，第一投影 $M · 1$ 的类型是 $σ₁$。
- **规则 (46.5c)**：对于层次化模块 $hier(M₁; M₂)$，第二投影 $M · 2$ 的类型是 $σ₂$。

**函子定义的类型判断：**

$$
\frac{
  \Gamma, X : \sigma_1 \vdash M_2 : \sigma_2
}{
  \Gamma \vdash \lambda X : \sigma_1.\ M_2 : \Pi X : \sigma_1.\ \sigma_2
}
\quad (46.5d)
$$

**解释：**

- **规则 (46.5d)**：函子定义 $λ X : σ₁. M₂$ 的类型是函子签名 $Π X : σ₁. σ₂$，其中在假设模块变量 $X$ 具有签名 $σ₁$ 的上下文中，模块 $M₂$ 的类型是 $σ₂$。

**函子应用的类型判断：**

$$
\frac{
  \Gamma \vdash M_1 : \Pi : \sigma_2.\ \sigma \quad \Gamma \vdash M_2 : \sigma_2
}{
  \Gamma \vdash M_1 (M_2) : \sigma
}
\quad (46.5e)
$$

**解释：**

- **规则 (46.5e)**：函子应用 $app(M₁; M₂)$ 的类型是 $σ$，前提是 $M₁$ 的类型是函子签名 $Π : σ₂. σ$，并且 $M₂$ 的类型是 $σ₂$。

**关键点：**

- **规则 (46.5a)**：层次化模块的类型需要满足其组成模块的类型与层次化签名的对应关系。
- **规则 (46.5b)** 和 **(46.5c)**：确保通过投影操作可以正确提取层次化模块的组成部分。
- **规则 (46.5d)** 和 **(46.5e)**：确保函子的定义和应用符合签名规则，维护类型系统的一致性。

---

### **函数器签名与子签名关系 (Functor Signature and Subsignature Relationship)**

#### **1. 函子签名的定义 (Definition of Functor Signature)**

函子（Functor）是模块的一种形式，其签名描述了函子的输入和输出签名之间的关系。

**函子签名（Functor Signature）：**

$$
\sigma_{\text{dictfun}} = \Pi Z : \sigma_{\text{eqord}}.\ \rho_Z^{\text{keydict}}
$$

- **解释**：
  - **域签名（Domain Signature）**：$σ_{\text{eqord}}$，定义了函子接受的输入模块的签名要求（如有序类型类）。
  - **范围签名（Range Signature）**：$ρ_Z^{\text{keydict}}$，依赖于输入模块 $Z$ 的具体实现，定义了输出模块的签名。

#### **2. 子签名关系中的协变与逆变 (Covariance and Contravariance in Subsignature Relationship)**

在签名的子签名关系中，层次化签名表现为协变，而函子签名则表现为逆变。

- **层次化签名的协变性**：
  
  层次化签名在子签名关系中保持协变，即子签名必须在下层和上层签名中都满足协变关系。

- **函子签名的逆变性**：
  
  函子签名在其域上表现为逆变，而在范围上表现为协变。这意味着在函子的输入签名上，子签名关系需要逆转，而在输出签名上保持协变。

**判定规则回顾：**

$$
\Gamma \vdash \text{fun}(\sigma_1; X.\sigma_2) <: \text{fun}(\sigma'_1; X.\sigma'_2)
\quad \text{当且仅当} \quad \Gamma \vdash \sigma'_1 <: \sigma_1 \ \text{且} \ \Gamma, X : \sigma'_1 \vdash \sigma'_2 <: \sigma_2
$$

**解释：**

- **域签名的逆变性**：如果函子签名的域 $σ₁$ 是 $σ'_1$ 的子签名，则在子签名关系中 $σ'_1 <: σ₁$。
- **范围签名的协变性**：如果在假设 $X : σ'_1$ 的上下文中，$σ'_2$ 是 $σ₂$ 的子签名，则 $σ'_2 <: σ₂$。

---

### **签名推导示例 (Signature Derivation Example)**

让我们通过一个具体的例子来理解层次化签名与函子签名的结合应用。

#### **1. 定义有序等价类型类签名**

首先，定义一个有序等价类型类签名 $σeqord$，表示具有等价和严格小于操作的类型。

$$
\sigma_{\text{eqord}} = \Sigma X : \sigma_{\text{eq}}.\ \sigma_X^{\text{ord}}
$$

其中：

$$
\sigma_X^{\text{ord}} = [t :: S(X \cdot s);\ \text{hlt} : (t \times t) \rightarrow \text{bool}]
$$

- **$X \cdot s$**：提取模块 $X$ 的静态部分，作为类型 $t$ 的具体类型。
- **$\text{hlt}$**：定义类型 $t$ 的严格小于操作。

#### **2. 定义字典函子签名**

定义一个字典函子 $Mdictfun$，其签名为 $σdictfun$：

$$
\sigma_{\text{dictfun}} = \Pi Z : \sigma_{\text{eqord}}.\ \rho_Z^{\text{keydict}}
$$

其中：

$$
\rho_Z^{\text{keydict}} = \Sigma Y : \sigma_{\text{keydict}}.\ Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
$$

**解释：**

- **$\Sigma Y : \sigma_{\text{keydict}}.\ Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s$**：
  - **$Y : \sigma_{\text{keydict}}$**：定义模块变量 $Y$，其签名为 $σkeydict$。
  - **共享约束**：要求模块 $Y$ 的键类型 $Y · 1 · 1 · s$ 与输入模块 $Z$ 的键类型 $Z · 1 · s$ 相同，确保字典的键类型与有序类型类实例一致。

#### **3. 函子的实现与实例化**

- **函子实现（Functor Implementation）**：

  $$$haskell
  Mdictfun = λ Z : σeqord . Mkeydict
  $$$

  - **$Mkeydict$**：一个通用的字典模块实现，依赖于输入模块 $Z$，其签名为 $σkeydict$。

- **函子应用（Functor Application）**：

  假设我们有一个自然数有序类型类实例 $Mnatord$：

  $$$haskell
  Mnatord = [nat; heq → natHeq, leq → natLeq] : σnatord
  $$$

  将 $Mnatord$ 作为参数应用到字典函子 $Mdictfun$ 上，生成自然数字典模块 $Mnatdict$：

  $$$haskell
  Mnatdict = Mdictfun (Mnatord) : ρnatdict
  $$$

  - **$\rho_{\text{natdict}}$**：

    $$
    \rho_{\text{natdict}} = [\ :: S(\text{nat});\ \text{hemp} : \text{nat},\ \text{ins} : \text{nat} \times \tau \times \text{nat} \rightarrow \text{nat},\ \text{fnd} : \text{nat} \times \text{nat} \rightarrow \tau \ \text{opti}]
    $$

  **解释：**

  - **封闭签名**：$ρnatdict$ 是一个封闭的签名，不再依赖于模块变量 $Z$，确保字典模块的键类型固定为自然数类型 $nat$。
  - **类型独立性**：通过共享传播，将签名中的依赖关系消除，使得字典模块的类型独立于输入模块的具体实现。

---

### **签名修改与共享传播 (Signature Modification and Sharing Propagation)**

#### **1. 签名修改机制 (Signature Modification Mechanism)**

为了简化签名中类型共享约束的表达，可以采用**签名修改**机制，直接在签名中引入共享约束。

- **签名修改的定义**：

  $$
  \text{Y : } \sigma_{\text{keydict}} \text{ / } Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
  $$

  **解释**：

  - **$\sigma_{\text{keydict}}$**：原始的字典模块签名。
  - **共享约束**：要求模块 $Y$ 的键类型 $Y · 1 · 1 · s$ 与模块 $Z$ 的键类型 $Z · 1 · s$ 相同。

#### **2. 共享传播机制 (Sharing Propagation Mechanism)**

**共享传播**通过子签名关系和子类型关系，将签名中的依赖关系消除，使输出模块的签名独立于输入模块的具体实现。

**过程：**

1. **子签名关系应用**：
   
   利用子签名关系，将层次化签名转化为更具体的签名。

2. **依赖消除**：
   
   通过共享传播，将依赖模块的类型信息传播到输出模块的签名中，消除对模块变量的依赖。

3. **签名简化**：
   
   最终得到一个独立的、封闭的签名，确保模块实例的签名不再依赖于输入模块的变量。

**示例推导：**

- **初始签名**：

  $$
  \rho_Z^{\text{keydict}} = \Sigma Y : \sigma_{\text{keydict}}.\ Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
  $$

- **签名修改**：

  通过共享传播，将 $Y · 1 · 1 · s$ 替换为具体类型 $nat$，得到封闭签名 $ρnatdict$：

  $$
  \rho_{\text{natdict}} = [\ :: S(\text{nat});\ \text{hemp} : \text{nat},\ \text{ins} : \text{nat} \times \tau \times \text{nat} \rightarrow \text{nat},\ \text{fnd} : \text{nat} \times \text{nat} \rightarrow \tau \ \text{opti}]
  $$

- **结果**：

  $Mnatdict$ 的签名 $ρnatdict$ 不再依赖于模块变量 $Z$，确保类型的独立性和封装性。

---

### **签名等价性与子签名关系的总结**

通过引入层次化签名和函子签名，并结合签名修改与共享传播机制，模块系统能够灵活地管理复杂的模块依赖关系，支持模块的层次化组织与参数化实现。

- **签名等价性**：确保不同形式的签名在类型系统中被视为相同，维护类型一致性。
- **子签名关系**：定义模块签名之间的继承和扩展关系，支持类型系统的协变与逆变需求。
- **共享传播**：消除签名中的模块变量依赖，确保输出模块的签名独立于输入模块的具体实现。

---

### **实例解析：有序键的字典模块扩展**

让我们通过一个具体的实例来综合理解层次结构与参数化的扩展应用。

#### **1. 定义有序等价类型类签名**

首先，定义一个有序等价类型类签名 $σeqord$，表示具有等价和严格小于操作的类型。

$$
\sigma_{\text{eqord}} = \Sigma X : \sigma_{\text{eq}}.\ \sigma_X^{\text{ord}}
$$

其中：

$$
\sigma_X^{\text{ord}} = [t :: S(X \cdot s);\ \text{hlt} : (t \times t) \rightarrow \text{bool}]
$$

- **$X \cdot s$**：提取模块 $X$ 的静态部分，作为类型 $t$ 的具体类型。
- **$\text{hlt}$**：定义类型 $t$ 的严格小于操作。

#### **2. 定义字典函子签名**

定义一个字典函子 $Mdictfun$，其签名为 $σdictfun$：

$$
\sigma_{\text{dictfun}} = \Pi Z : \sigma_{\text{eqord}}.\ \rho_Z^{\text{keydict}}
$$

其中：

$$
\rho_Z^{\text{keydict}} = \Sigma Y : \sigma_{\text{keydict}}.\ Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
$$

**解释：**

- **$\Sigma Y : \sigma_{\text{keydict}}.\ Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s$**：
  - **$Y : \sigma_{\text{keydict}}$**：定义模块变量 $Y$，其签名为 $σkeydict$。
  - **共享约束**：要求模块 $Y$ 的键类型 $Y · 1 · 1 · s$ 与输入模块 $Z$ 的键类型 $Z · 1 · s$ 相同，确保字典的键类型与有序类型类实例一致。

#### **3. 函子的实现与实例化**

- **函子实现（Functor Implementation）**：

  $$$haskell
  Mdictfun = λ Z : σeqord . Mkeydict
  $$$

  - **$Mkeydict$**：一个通用的字典模块实现，依赖于输入模块 $Z$，其签名为 $σkeydict$。

- **函子应用（Functor Application）**：

  假设我们有一个自然数有序类型类实例 $Mnatord$：

  $$$haskell
  Mnatord = [nat; heq → natHeq, leq → natLeq] : σnatord
  $$$

  将 $Mnatord$ 作为参数应用到字典函子 $Mdictfun$ 上，生成自然数字典模块 $Mnatdict$：

  $$$haskell
  Mnatdict = Mdictfun (Mnatord) : ρnatdict
  $$$

  - **$\rho_{\text{natdict}}$**：

    $$
    \rho_{\text{natdict}} = [\ :: S(\text{nat});\ \text{hemp} : \text{nat},\ \text{ins} : \text{nat} \times \tau \times \text{nat} \rightarrow \text{nat},\ \text{fnd} : \text{nat} \times \text{nat} \rightarrow \tau \ \text{opti}]
    $$

  **解释：**

  - **封闭签名**：$ρnatdict$ 是一个封闭的签名，不再依赖于模块变量 $Z$，确保字典模块的键类型固定为自然数类型 $nat$。
  - **类型独立性**：通过共享传播，将签名中的依赖关系消除，使得字典模块的类型独立于输入模块的具体实现。

---

### **参数化签名的约束与共享传播**

#### **1. 签名修改（Signature Modification）**

为了避免重复书写复杂的签名修改约束，可以使用签名修改机制，直接在签名中引入共享约束。

**签名修改形式：**

$$
Y : \sigma_{\text{keydict}} / Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
$$

**解释：**

- **$Y : \sigma_{\text{keydict}}$**：模块 $Y$ 必须满足签名 $σkeydict$。
- **共享约束**：模块 $Y$ 的键类型 $Y · 1 · 1 · s$ 必须与模块 $Z$ 的键类型 $Z · 1 · s$ 相同。

#### **2. 共享传播（Sharing Propagation）**

**共享传播** 通过子签名关系和子类型关系，将签名中的依赖关系消除，使得输出模块的签名独立于输入模块的具体实现。

**推导过程示例：**

1. **初始签名**：

   $$
   \rho_Z^{\text{keydict}} = \Sigma Y : \sigma_{\text{keydict}}.\ Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
   $$

2. **应用签名修改**：

   将 $Y · 1 · 1 · s$ 替换为具体类型 $nat$，得到封闭签名 $ρnatdict$：

   $$
   \rho_{\text{natdict}} = [\ :: S(\text{nat});\ \text{hemp} : \text{nat},\ \text{ins} : \text{nat} \times \tau \times \text{nat} \rightarrow \text{nat},\ \text{fnd} : \text{nat} \times \text{nat} \rightarrow \tau \ \text{opti}]
   $$

**结果**：

$Mnatdict$ 的签名 $ρnatdict$ 不再依赖于模块变量 $Z$，确保字典模块的类型独立性和封装性。

---

### **参数化与层次结构的结合应用**

通过结合**层次结构**和**参数化**，模块系统能够构建出高度模块化、灵活且可扩展的程序结构。以下通过具体实例展示其结合应用的效果。

#### **实例解析：基于有序键的字典模块**

##### **1. 定义有序等价类型类签名**

首先，定义有序等价类型类签名 $σeqord$：

$$
\sigma_{\text{eqord}} = \Sigma X : \sigma_{\text{eq}}.\ \sigma_X^{\text{ord}}
$$

其中：

$$
\sigma_X^{\text{ord}} = [t :: S(X \cdot s);\ \text{hlt} : (t \times t) \rightarrow \text{bool}]
$$

##### **2. 定义字典函子签名**

定义字典函子 $Mdictfun$，其签名为 $σdictfun$：

$$
\sigma_{\text{dictfun}} = \Pi Z : \sigma_{\text{eqord}}.\ \rho_Z^{\text{keydict}}
$$

其中：

$$
\rho_Z^{\text{keydict}} = \Sigma Y : \sigma_{\text{keydict}}.\ Y \cdot 1 \cdot 1 \cdot s = Z \cdot 1 \cdot s
$$

##### **3. 函子的实现与应用**

- **函子实现**：

  $$$haskell
  Mdictfun = λ Z : σeqord . Mkeydict
  $$$

- **函子应用**：

  假设有一个自然数有序类型类实例 $Mnatord$：

  $$$haskell
  Mnatord = [nat; heq → natHeq, leq → natLeq] : σnatord
  $$$

  应用函子生成自然数字典模块 $Mnatdict$：

  $$$haskell
  Mnatdict = Mdictfun (Mnatord) : ρnatdict
  $$$

- **签名推导**：

  $$
  \rho_{\text{natdict}} = [\ :: S(\text{nat});\ \text{hemp} : \text{nat},\ \text{ins} : \text{nat} \times \tau \times \text{nat} \rightarrow \text{nat},\ \text{fnd} : \text{nat} \times \text{nat} \rightarrow \tau \ \text{opti}]
  $$

**解释：**

- **封闭签名**：$ρnatdict$ 不再依赖于模块变量 $Z$，确保字典模块的键类型固定为自然数类型 $nat$。
- **类型独立性**：通过共享传播，消除了签名中的依赖关系，确保类型系统的独立性和一致性。

---

### **类型安全性的保障**

通过严格的静态语义规则和共享传播机制，模块系统能够确保类型安全性。这意味着，模块的实现和其依赖关系在类型系统中得到正确管理，避免了因模块依赖不一致导致的类型错误。

#### **保持性（Preservation）**

如果一个模块表达式在类型环境 $\Gamma$ 下被推导为某个签名 $\sigma$，并且该模块表达式通过一步计算变为另一个模块表达式，那么新的模块表达式仍然具有相同的签名 $\sigma$。

#### **进展性（Progress）**

如果一个模块表达式在类型环境 $\Gamma$ 下被推导为某个签名 $\sigma$，那么该模块表达式要么已经是一个值，要么可以进一步计算。

**证明

### ---------------------------------

### **46.4 应用函子 (Applicative Functors)**

在前述的模块语言中，函子（Functors）被视为**生成性的（Generative）**，即任何两个实例，即使具有不同的参数，也被认为会“生成”不同的抽象类型。这一特性通过将函子应用 $M (M1)$ 视为**不可投影的（Non-Projectible）**来确保。因此，如果 $M (M1)$ 在结果中定义了一个抽象类型，该类型无法在未先将应用绑定到变量的情况下被引用。任何两个此类绑定必然绑定到不同的变量 $X$ 和 $Y$，因此抽象类型 $X · s$ 和 $Y · s$ 是不同的，无论它们的绑定如何。

#### **应用函子的引入与动机**

将函子视为生成性确保了函子的客户（Client）无法依赖于函子的具体实现。这意味着我们将**表示独立性原则（Representation Independence）**自然地扩展到了函子。这一策略的一个后果是，模块语言与诸如条件模块（Conditional Modules）等扩展机制兼容，这类模块可能根据运行时的任意动态条件（甚至依赖于诸如月相等外部条件）进行分支。因此，具有此类实现的函子必须被视为生成性的，因为任何实例的抽象类型在绑定到变量之前无法被视为定义良好的。

通过将所有函子视为生成性，我们实际上最大化了利用表示变化而不破坏函子客户行为的机会，这也是模块化分解的基石原则。

然而，由于前述的模块语言并未包含诸如条件模块等强大的功能，我们可能认为限制函子为生成性过于严格，可以有意义地放宽这一限制。**应用函子（Applicative Functors）**便是这种放宽的一种替代方案。应用函子允许通过值进行的实例化被视为**可投影的（Projectible）**：

$$
\frac{
  M \text{ projectible} \quad M1 \text{ val}
}{
  M (M1) \text{ projectible}
}
\quad (46.7)
$$

**注意**：应用函子规则（46.7）意味着应用函子的实例 $M (M1)$ 是可投影的。然而，这种设计决策也带来了新的挑战和限制。

---

### **应用函子的特性与限制**

#### **1. 应用函子的可投影性**

通过将应用函子实例 $M (M1)$ 视为可投影，我们能够形成诸如 $(M (M1)) · s$ 的类型表达式，用以投影实例的静态部分。然而，这引发了一个关键问题：**何时两个此类类型表达式应被视为等价？**

例如，假设 $F$ 是一个应用函子变量，在哪些条件下 $(F (M1)) · s$ 和 $(F (M2)) · s$ 应被视为相同类型？对于生成性函子，这个问题不存在，因为实例不可投影；但对于应用函子，这一问题必须被解决，以确保类型系统的一致性和安全性。

#### **2. 函子体的封装性**

另一个复杂性在于，应用函子的主体无法被封装以强加抽象。因此，根据前述规则，没有封装模块被视为可投影，因为封装是强加抽象的唯一手段。为了支持应用函子，我们必须放宽这一限制，允许**封装的可投影模块**也被视为可投影：

$$
\frac{
  M \text{ projectible}
}{
  M \langle \sigma \rangle \text{ projectible}
}
\quad (46.8)
$$

通过这一扩展，我们可以形成诸如 $(M \langle \sigma \rangle) · s$ 的类型表达式，用以投影封装模块的静态部分。但这也带来了另一个问题：**如何定义两个此类类型表达式的等价性？**

#### **3. 类型等价性的挑战**

为了将函子视为应用函子，我们需要定义如何判定 $(M \langle \sigma \rangle) · s$ 和 $(N \langle \sigma' \rangle) · s$ 的等价性。由于封装模块的静态部分与具体实现相关，等价性的判定依赖于底层模块是否完全相同。这意味着：

1. **类型比较**：需要在类型检查时比较封装模块的可执行代码，以确保它们在类型和行为上是等价的。
2. **表示独立性的妥协**：这使得在类型系统中对封装模块的表示独立性产生依赖，违背了模块系统设计中保持抽象的初衷。

因此，将函子视为应用函子需要在**表示独立性**和**类型系统的灵活性**之间做出权衡。

---

### **应用函子的优缺点**

#### **优点**

1. **允许类型投影**：通过应用函子，能够直接投影实例的静态部分，增强了类型表达的灵活性。
2. **统一实现**：在大多数情况下，字典操作等模块功能可以通过相同的实现来处理不同的键类型，仅需根据具体参数调整部分函数引用。
3. **类型复用**：应用函子允许模块根据不同参数生成相应的实例，提高了代码的复用性和模块系统的表达能力。

#### **缺点**

1. **破坏表示独立性**：为了支持可投影性，必须放宽对封装模块的限制，导致类型系统依赖于具体模块实现。
2. **类型系统复杂性增加**：需要在类型系统中处理封装模块的等价性判定，增加了类型检查的复杂性和开销。
3. **不兼容条件模块**：应用函子不支持条件模块，因为条件模块可能基于运行时条件生成不同的模块实例，这与应用函子的可投影性要求相冲突。

---

### **总结**

- **生成性函子**：
  - **定义**：任何两个函子实例生成不同的抽象类型，确保表示独立性。
  - **优点**：保持模块系统的表示独立性，兼容条件模块，允许模块间的灵活组合。
  - **缺点**：限制了某些类型投影操作，增加了类型系统的复杂性。

- **应用函子**：
  - **定义**：函子实例被视为可投影，允许直接投影其静态部分。
  - **优点**：增强类型表达的灵活性，支持类型投影，提升代码复用性。
  - **缺点**：破坏表示独立性，增加类型系统复杂性，不兼容条件模块。

- **权衡**：
  - 将函子视为生成性或应用性取决于模块系统设计的优先级。
  - 生成性函子适用于需要严格表示独立性的场景。
  - 应用函子适用于需要类型投影和高复用性的场景，但需接受一定的表示依赖性。

通过理解生成性函子与应用函子的区别及其在模块系统中的应用，开发者可以根据具体需求选择合适的函子类型，设计出既灵活又安全的模块化程序结构。

---

### **进一步阅读**

- **Leroy, X. (1995)**：介绍了应用函子的概念，并在O’Caml的模块系统中进行了详细应用。
- **Milner, R., Tofte, I., & Harper, R. (1997)**：**Definition of Standard ML**，详细描述了生成性函子的实现与应用。
- **Harper, R., Lillibridge, J. (1994)**：研究了模块层次结构与类型抽象的结合。
- **Leroy, X. (1994)**：进一步发展了模块系统的理论，提出了自我识别规则。
- **Mitchell, J.C., & Plotkin, G. (1988)**：关于存在类型与模块系统的早期研究。
- **Stone, M., & Harper, R. (2006)**：将自我识别规则与高阶单例联系起来，深化了模块系统的类型理论。
- **Lee, H., et al. (2007)**：构建了模块系统的机理化理论，基于前人研究成果。
- **Dreyer, D. (2005)**：总结了模块系统设计中的主要问题，提供了全面的模块化系统设计概述。

通过深入研究这些文献，可以更全面地理解模块层次结构与函子在现代编程语言中的重要性和应用。

---

### **总结**

- **生成性与应用性函子**：
  - 生成性函子通过确保每个实例生成独立的抽象类型，保持了模块系统的表示独立性，但限制了类型投影操作。
  - 应用性函子允许类型投影，增强了类型表达的灵活性和模块复用性，但牺牲了部分表示独立性。

- **设计决策**：
  - 模块系统设计者需要根据具体需求权衡生成性与应用性函子的优缺点。
  - 在需要严格封装和表示独立性的系统中，生成性函子更为适用。
  - 在需要类型投影和高复用性的系统中，应用性函子提供了更大的灵活性。

- **类型系统的支持**：
  - 无论选择生成性还是应用性函子，类型系统都需要提供相应的机制来处理模块依赖、签名推导和类型等价性判定，确保模块系统的类型安全性和一致性。

通过掌握生成性函子与应用性函子的概念及其在模块系统中的应用，开发者能够设计出高效、灵活且安全的模块化程序结构，提升系统的整体质量和扩展能力。


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