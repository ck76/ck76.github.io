[toc]

### Chapter 1: Abstract Syntax
抽象语法。用树状结构表达。书中的内容不关注具体的语法，仅仅关注语法片段（句法）对应的树，以及树中的标识符（类似于变量）的绑定关系（类似于变量的赋值）。这本书主要关注语法的结构化信息，对于语法最后是如何用字符串表达的并不关心。

abstract syntax tree。 抽象语法树，经常用$ a $表示。叶子被称作variables，非叶节点（内部节点）被称作operators。

sort。语法树的类型。不同类型的语法树对应不同形式的语法。比如SQL语句和一般的C++编程是用的不同的语法。经常用$ s $表示。经常用$ S $表示sort的集合。

variable。未指定的或者通用的语法片段。一个未知的占位符，其具体的含义与其替代者有关。经常用$ x $表示。用$ \chi $表示集合。用$ \chi_s $表示对应类型的所有变量的集合。我们可以使用$ A[\chi]=\{A[\chi]_{s \in S}\} $来代表类型为$ s $的语法树，这个语法树中的变量是类型$ s $的、以及所有operator在连接不同子树之后也是类型$ s $的。当然，也可以使用$ A[\chi_{1}, \chi_{2}] $来并联多个集合。

operator。语法树的内部（非叶）节点。通常使用$ o $来表示。$ O $用来表示operator的集合。其子节点被称作argument（参数）。其内部包含一个叫做arity的概念来表达operator及其参数的类型。用$ o(a_1;...;a_n) $代表一个包含了语法树$ a_1 $到$ a_n $作为子节点的operator。

arity。元数。一般用变量$ \alpha $表示。其完整表示为$ (s_1,...,s_n)s $，其代表对应的operator为类型$ s $，带有$ n $个参数（子节点），每个参数的类型为$ s_1 $到$ s_n $。通过将arity考虑在内。我们可以使用$ O_\alpha $来表示相同arity的operator的集合。

structural induction。结构归纳。 如果要证明语法树$ a $满足某一个属性$ P $，即$ P(a) $。我们只需要证明1）$ a $中所有的变量$ x $满足属性$ P $；2）每一个operator在连接了其子树（参数）之后所形成的新 抽象语法树也满足属性$ P $。在全局不明朗，变量替代关系复杂的树状结构中，结构归纳只证明小的单元，然后推广至全局的方法来证明语法树的性质。

substitution。替代。用一个语法片段去代替语法树中的一个变量。$ [b/x]a $代表用语法树b替代语法树$ a $中所有命名为$ x $的变量。

abstrast binding tree。抽象绑定树。解决变量的作用域问题。在语法片段中，如果变量$ x $特指是语法树$ a $中的$ x $，需要将变量写成$ x.a $。这是相对于abstract syntax tree唯一的区别。这是x被称作绑定变量（减occur free）。抽象绑定树可以用于表达函数式编程、匿名函数和lambda文法。在文法中，函数的声明和函数调用可能出现在一行代码中。这时绑定变量就更像是匿名函数的形参和局部变量，这些绑定变量是不会在匿名函数外面被替换和赋值的。

abstractor。抽象绑定树相比抽象树，加入的新语法。即abstract binding tree中的operator中的argument的新名字。使用$ x_1,...,x_n.a $表达绑定在语法树$ a $中的变量$ x_1 $到$ x_n $，变量可以进一步写成向量$ \vec{x}.a $来简介表达。从此之后，operator的子节点变成了abstractor。abstractor感觉像匿名函数的声明。

generalized arity。广义的元数。扩大了arity的原始定义。表示为$ (v_1,...,v_n)s $。其中$ s $为operator的类型，$ v $是valence，直译是化合价的意思，代表一个语法抽象树（原子）和其绑定的变量（化学键）。$ v $可以进一步表达为$ s_1,...s_n.s $和$ \vec{s}.s $，其中s是argument（连在operator下面的一个小语法树）的类型，$ s_1 $到$ s_n $是每一个绑定的变量的类型。

fresh renaming。重命名操作。在抽象绑定树中存在嵌套关系，即一个抽象绑定树是另外一个抽象绑定树的子树。如果这个时候两个层次的抽象绑定树的绑定变量的名字是一样的，会造成歧义，故需要引入重命名，让其中一个语法树的绑定变量的名字错开。

$ \alpha $-equivalent。$ \alpha $等价。如果两个abt可以通过改变变量名来使得二者完全相同，那么这两个abt被称作$ \alpha $等价。

occur free。一个变量如果在一个abstractor中是occur free的，代表这一变量是可以被替换（substitution）的。但是如果一个变量是被绑定的，那它就是不能被替换的。这里涉及到自由变量和绑定变量的区别维基百科—自由变量与绑定变量。简单来说，绑定变量是一种形式上的变量，可以类比于编程语言中函数声明中的形参和函数内的局部变量。这种变量本质上是被固定在了一个作用域中，用以介绍一段逻辑，外部的赋值不会影响到函数内部。自由变量是真正的、实际的变量，可以被替换（在编程语言中称为赋值），可以理解为编程语言中的全局变量。

symbolic parameter（symbol）。符号参数。抽象语法并不在任何时候都包含固定的operator。有时，一个operator是否可用与当前语法的上下文有关。我们使用symbolic parameter，或者symbol来表示operator的集合的索引，只有symbolic parameter是活跃的，对应的那些operator才是可用的，用$ u $表示，用$ U $代表symbol的集合。$ o[u] $表示与symbol $ u $对应的operator $ o $。语法绑定树的定义可以进一步扩展，用$ B[U;\chi] $代表语法绑定树的集合，包含了符号集合$ U $和变量集合$ \chi $。与variable（变量）一样，symbol也支持绑定到一定的语法树中，比如abstractor $ u.a $。symbol和variable在重命名上是完全一致的，只要不产生新的冲突，但是除此之外，symbol就有所不同，symbol不是一个占位符，不能被替换。

### Chapter  2: Inductive Definitions
归纳定义。一种编程语言的基本工具。用递归的方式来做出定义。structural induction是一种特殊的归纳定义。

judgment。判断，也被称作assertion，断言。judgement是一种对于语法树性质和多个语法树关系的描述。

judgement form。判断形式，也称作谓词predicate。判断中的性质与关系。用$ J $去表示。谓词是归纳定义的定义对象。

instance of judgement form。谓词的实例，也就是谓词加上主语subjects。主语表达了性质与关系所作用的对象。使用$ a\ J $或者$ J\ a $代表语法（绑定）树满足谓词$ J $。有时候如果主语暂时缺席，那就变成$ -\ J $或者$ J\ - $用一个占位符来代替。在有些语境下主语并不重要，这个时候可以直接使用$ J $来表达整个断言。

rule。规则。写成$ \frac{J_1,...,J_k}{J} $的形式，上面的断言是被称作rule的前提premise，下面的被称作rule的结论conclusion。读作前提对于结果是充分的sufficient。如果一个rule没有前提，那么其就被称作公理axiom。对于某一个谓词inductive definitions由一系列的rule构成。rule的集合一般写做$ R $。

iff。当且仅当。iff会连接两个断言，代表二者之间是相互推导，充分必要的关系。

rule scheme。规则族。rule中的judgement的主语有时是一个变量，因为变量可以无限地被替换。带有变量的rule本质上是一堆rule的集合，称为规则族，对于主语的不同的、具体的替换构成规则族的一个实例，也就是规则。

derivation。推导。证明一个judgement有效的过程。推导过程从公理开始，通过连接不同rule的前提和结论，最终在推导过程的尾部案得到某一个judgement。一般用符号$ \nabla $来表示。

closed under。闭包。一个在不同领域有不同具体含义的概念。如果一个judgement可以通过一系列给定rule来推导得出，那么我们就可以说这个judgement闭包于这些rule。closed under说明了rule对于judgement的推导是充分的，即通过给定的rule可以找到一种方式来证明judgement的有效性。可以用拓扑学和集合论和角度来解释闭包这个概念。通过组合rule可以得到很多的推理路径（有向图），这些路径中节点是judgement，边是rule（边的起点是premise，终点是conclusion）。那么通过组合各种各样的rule可以得到命题的集合，如果一个judgement包含在所有rule的合理组合而形成的图中，那么这个judgement就是这些rule的一个闭包。

strongest judgement。强断言。是一种相比闭包更为严格的条件。如果一个judgement闭包与给定的rule，并且能且只能由给定的rule推导得出，那么就可以将其称为闭包于给定rule的strongest judgement。这些rule对于strongest judgement的推导是必要的。

rule induction。规则归纳。归纳定义（inductive definition）的一种方式。规则归纳出现在我们要推理的judgement（所定义的性质）既是一推rule要推理的目标，又存在于这些rule的前提之中。所谓”我推理我自己“。数学归纳法就是一个规则归纳，即通常中一个基础的对象对应的断言出发，然后通过加入一个假设（归纳假设），扩展到整个值域。rule induction必须保证judgement是strongest judgment。PFPL在为什么要strongest judgment，没说明原因。我猜测只有strongest judgment才能保证对于一个推理的路径是唯一的，从而使得归纳过程可以无限递归下去。或者如果在前提中包含了目标judgement的rule集合不是唯一的推理路径，那么可能使用普通的、不递归的其他方式也可以推理出目标judgement，rule induction就不是必须的。

### Chapter  3: Hypothetical and General Judgements
这一章节包含了两种进阶的断言。与### Chapter  2介绍的judgement不同（这一章将这类断言称作basic judgement），### Chapter  3中的断言更像是“断言的断言”，与之前的以语法树为对象的断言不同，这一章的断言的对象也是断言。这一章的两种断言，一个是hypothetical judgement，研究的是假设与可推理性，还有可接受性。另外一种断言是general judgment，研究的是断言的变量（见variable的概念）的不同重命名和替换以及断言的符号参数（见symbolic parameter的概念）的重命名对于断言有效性的影响。此外，这一章还给出了两种进阶的断言对应的rule和rule induction（归纳定义）。

derivability judgement。可推导性断言，假设性断言的一种。这类断言可以表达为$ J_1,...,J_k\vdash_RK $。其中$ J $和$ K $都代表断言，$ J $可以进一步称为假设（hypothesis），可以用符号$ \Gamma $和$ \triangle $代表所有假设的集合。$ R $代表规则的集合。$ \vdash $是可推导的意思，可以不带假设，单独使用$ \vdash_R\Gamma $表示，代表$ R $中的规则可以推导出$ \Gamma $中所有的judgement。这个断言可以表达为本质相同的四种意思：

假设所有的$ J $都成立，进一步利用$ R $中的规则可以推导出$ K $；
将$ J_1,...,J_k $全部视为公理$ \frac{}{J_1},...,\frac{}{J_k} $，那么可以依据$ R $中的rule和这些公理，即$ R\cup\frac{}{J_1},...,\frac{}{J_k} $，找到一条使$ K $成立的推导（$ \vdash_{R\cup\frac{}{J_1},...,\frac{}{J_k}}K $）。这一解释可以帮助我们理解derivability judgement的一系列性质。
规则$ \frac{J_1,...,J_k}{K} $可以推导自$ R $中的规则。
stability of derivability judgement。可推导性断言的稳定性。扩展可推导性断言中的规则集合不会影响可推导性断言的有效性。如果$ \Gamma\vdash_RJ $，那么$ \Gamma\vdash_{R\cup R'}J $。

reflexivity of derivability judgement。可推导性断言的反射性。每个（假设中的）断言都是自身的结论，这个很好理解。即$ \Gamma,J\vdash_RJ $。

weakening of derivability judgement。可推导性断言的弱化。增加的额外假设，不影响可推导性。即若$ \Gamma\vdash_RJ $成立，$ \Gamma,K\vdash_RJ $也成立。$ K $为额外增加的假设。

transitivity of derivability judgement。可推导性断言的传递性。如果假设中的一部分断言可以从另外的假设和规则集合$ R $中推导出来，那么这部分断言是可以忽略的。即如果$ \Gamma,K\vdash_RJ $并且$ \Gamma\vdash_RK $，那么$ \Gamma\vdash_RJ $。$ K $能被剩下的部分假设$ \Gamma $以及规则$ R $推导出来，所以$ K $可以被忽略。

admissibility judgement。可接纳性断言，另一种假设性断言。如果规则可以推导出某些judgement，那么也一定能推导出另外的judgement。表达为$ \Gamma\models_RJ $。有本质相同的两种意思：

假设（如果）$ \vdash_R\Gamma $成立，那么$ \vdash_RJ $也成立。
设$ \Gamma $中的断言为$ J_1,...,J_n $。那么$ \frac{J_1,...,J_n}{J} $相对于$ R $是可接受（admissible）的。如果可以推出前期，那么结论也一定是成立的。
如果假设$ \Gamma $中的断言实际上没法被$ R $中的规则推导出来，那么这个admissibility judgement只能说是“虚真的”（vacuously true）。admissibility judgement能够成立通常是因为$ R $对于断言假设的推理路径可以“经过”可接纳性断言的结论。admissibility是一种比derivability更宽松的断言，有时候$ \Gamma\vdash_RJ $不成立，但是$ \Gamma\models_RJ $是成立的。

no stability of admissibility judgement。可接受性断言的不稳定性。扩展可接受断言的rule集合$ R $并不能保证可接受性断言的继续成立。因为新加入的规则会改变$ R $对于断言假设推理路径，从而反而使得可接受性断言不成立。但是如果扩展的rule相对于$ R $是可接受（admissible）的，那么这个rule的扩展就不影响admissibility judgement的有效性。因为此时R对于rule前提的推导是经过rule的结论的。rule的扩展相当于在R原来的推理路径上加上“回头路”，不影响原本推理路径所经过的断言，也就不影响可接受性断言本身。

reflexivity of admissibility judgement。可接受性断言的反射性。如果假设相对于$ R $是可推导的，那么显而易见，与假设相同的结论也是可推导的。即$ J\models_RJ $。

weakening of admissibility judgement。可接受性断言的弱化。增加断言的假设不影响断言是否成立。即如果$ \Gamma\models_RJ $那么$ \Gamma,K\models_RJ $。新增加的假设不影响对于原来假设的推理路径，也就不影响推理路径经过断言$ J $。

transitivity of admissibility judgement。可接受性断言的传递性。如果一部分假设相对于剩余的假设和rule集合是可接受的，那么这部分假设可以不需要。即如果$ \Gamma,K\models_RJ $并且$ \Gamma\models_RK $，那么$ \Gamma\models_RJ $。如果$ K $已经在$ R $对于$ \Gamma $的推理路径中经过了，那就不需要再做出假设了。

formal derivability judgement。形式可推导断言。代表两组断言之间存在可以推导的关系，但是不关心具体是怎么推导的。即$ \Gamma\vdash J $，代表存在某种方式使得$ \Gamma $中的judgement可以推导出$ J $中的judgement。这一概念为derivability judgement多了一种解释：$ \Gamma\vdash_RJ $代表了形式可推导断言$ \Gamma\vdash J $可以推导自规则集合$ R $。

hypothetical rule。假设规则。在premise和conclusion中包含formal derivability judgement的规则：
$ \frac{\Gamma\Gamma_1\vdash J_1 \ ... \ \Gamma\Gamma_n\vdash J_n}{\Gamma\vdash J} $

其中$ \Gamma $被称作全局假设（global hypothesis），$ \Gamma_1,...,\Gamma_n $，称作对应前提的局部假设（local hypothesis）。与basic judgement的推导一样，formal derivability judgement的证明会依赖假设规则集合$ R $来做出推理。因为这里是假设断言的范畴，所以形式可推导断言的证明除了依赖于$ R $还会依赖于derivability judgement的性质（见derivability judgement部分的介绍）。此外，一般来说，可以规定在$ R $中的所有rule的全局假设是统一的。故假设规则可以简化为更局部的形式：
$ \frac{\Gamma_1\vdash J_1 \ ... \ \Gamma_n\vdash J_n}{J} $

hypothetical rule induction。假设规则归纳，由一系列rule对于形式可推导断言某一性质的定义$ P(\Gamma\vdash J) $（猜测可能是形式可推导断言中的语法树的性质）。与之前rule induction所表达的归纳定义是一个意思。都是为了某一性质在rule前提与假设之间的传递。在假设规则中，具体说，如果$ P(\Gamma\Gamma_1\vdash J_1),...,P(\Gamma\Gamma_n\vdash J_n) $都被满足，那么$ P(\Gamma\vdash J) $也能被满足。在此基础上，就可以进行归纳定义。

uniformity of rules。rule的一致性。集合$ R $中的rule如果是满足一致性的，那么其中任意一个rule中的变量和symbol被重命名、被替换所产生的rule也被包含在$ R $中。

general derivability judgement。一般性可推导断言。关注断言中的语法树的variable的renaming和substitution以及symbolic parameter的renaming对于可推导性的影响（详见### Chapter  1）。用符号$ \Gamma\vdash_{R}^{U;\chi}J $来代表$ \Gamma $可以通过规则集$ R $推导出$ J $。在这一可推导性断言中的所有variable包含集合$ \chi $中，所有的symbol包含在集合$ U $中。对于一个一般性可推导断言，如果有可推导性断言$ \Gamma\vdash_{R}^{\chi\ Y}J $，$ \chi $和$ Y $都是变量的集合，其中$ Y $中都是可以随意命名和替换的变量，那么一般性可推导断言可以将“自由”的变量放到外面去。$ Y|\Gamma\vdash_R^\chi J $，代表Y中的变量无论怎么赋值和重命名，都不改变可推导性。此外这里要求$ R $中的规则是满足uniformity的。当然，symbolic parameter的重命名也是一样的。可以用双杠放在更前面来表示：$ V||Y|\Gamma\vdash_R^{U,\chi}J $，$ V $是可以随意命名的symbol。

formal generic judgement、generic rule、generic inductive definition。与formal derivability judgement、hypothetical rule、hypothetical rule induction的定义类似。

### Chapter  4: Statics
（编程语言）静力学。用以在程序编写完之后、运行之前检查程序的语法。主要以表达式的类型检查为主。

abstract syntax。抽象语法。本质上是对于语法树的描述，包含了一系列的operator及其arity。BNF范式是表达语法的常用方法。从类型检查的角度来说，一个语法表中包含两种sort的语法，一种是表达式，即expression；另一种是表达式的类型，即type。注意sort与type两个概念的区别。sort是abt的种类（例如表达式是一种abt，是一种语法类型），type是表达式的类型（有类似于编程语言中的数据类型）。

typing judgment。类型系统中有关类型的断言。 表达为一般性假设断言，声明一个表达式的类型。$ \chi|\Gamma\vdash e:\tau $（”$ : $“用来表达类型），代表表达式$ e $的类型（type）为$ \tau $。$ \chi $代表上下文中的变量集合，这些变量是可以被随意替换和重命名的。在这一语境中，假设$ \Gamma $被称为类型上下文（type context），代表表达式上下文的中的（$ \chi $中的）变量类型（有时表达式$ e $的类型由其上下文中其他变量的类型决定）。$ x\in dom(\Gamma) $代表变量$ x $没有出现在类型上下文中（fresh）。

typing rule。类型系统中的规则。用来定义一个表达式的type的推理，前提是一般是其子表达式的type，结论一般是这些子表达式（作为abt中的变量）构建的更大表达式的type。也有一些rule是没有前提的axiom，强制规定某种表达式的type，通常是针对的非常简单的表达式，比如编程语言中变量（这里指的不是abt的变量）和常量。typing rule是强烈语法导向的，一般来一条语法规则对应一个typing rule。

unicity of typing。类型的独特性。对于每一对上下文$ \Gamma $和表达式$ e $，最多只能有一个类型$ \tau $使得$ \Gamma\vdash e:\tau $。

inversion for typing。类型的倒置。根据表达式，可以反推出其表达式的类型以及表达式中变量（语法的、abt的变量）的类型。

weakening of statics。静力学的弱化。在假设中增加不存在于typing context（$ \Gamma $）的$ x:\tau $，不影响这一typing context中表达式的类型。

substitution of statics。静力学的替换原则。表达式本质上是一个abt，将abt中的变量替换为与这个变量type相同的子abt，不影响外层的表达式的类型。替换原则类似于软件中调用过程，我们在调用一个函数时本质上调用的是函数的签名，但是最终这个签名会被函数的实现替换。abt、expression的本质就是一个代码段。

decomposition of statics。静力学中的分解。替换的逆过程。表达式中的子表达式可以分离出来，并且在对应位置放上变量。可以将一个表达式分解为等效的两个小表达式。

introduction and elimination。语法构建的两种形式：介绍和消除。introduction直接决定了一个值的type（什么是什么），elimination决定了怎么用同一个类型的一系列值在计算之后去形成另一个类型（让复杂的、带有变量的表达式最终简化为自身的introduction form）。

closed expression。封闭的表达式。当表达式中的所有的variable所代表的子表达式都计算成值（value）了，当前表达式已经可以被直接计算为值了，那么这一表达式称为封闭表达式。

### Chapter  5: Dynamics
代码动力学。主要是研究代码怎么被执行的。主要包含了三种动力学：structural dynamics（表达式在一步步执行中的变化），contextual dynamics（对于一个多层嵌套的复杂表达式来说，表达式不同部分的执行顺序），equational dynamics（表达式之间的等效关系）。在这一章中，编程语言的表达式（expression）基本等同于计算机程序（program）或者指令（instruction）。

transition system。变迁系统。类似于状态机。变迁系统中包含一系列state（状态）。包含initial（起始状态）和final（终结状态）作为一开始的状态和结束的状态。以及它们之间变迁的方式。如果每一个状态最多只有一个后继状态，那么就说这一变迁系统是确定的（deterministic）。

transition judgement。变迁断言。用$ s\longmapsto s' $代表state $ s $和state $ s' $之间的转换。另外，如果强调两个状态之间包含多步的变迁。$ s\longmapsto^* s' $代表两个状态的变迁是0步或者多步的。当然也可以通过将星号变成一个数字来表达两个状态的变迁步骤的数量，比如$ s\longmapsto^0 s $或者$ s\longmapsto^k s $。

transition sequence。变迁序列。如果state $ s_0,...,s_n $依次变迁，$ s_0 $是起始状态，那么这个序列叫做变迁序列。如果$ s_n $之后没有状态了， 这一序列被称作最大的（maximal）。如果$ s_n $是终结状态，那么这个序列是完整的（complete）。

structural dynamics。结构动力学。将语言的表达式视为state，那么就可以针对一个语言建立一个表达式的变迁系统，表达了表达式的变化关系。比如$ plus(num[n_1];num[n_2])\longmapsto num[n] $。这些变化都有前提，都有对应的rule。rule分为两种，$ \frac{n_1+n_2=n}{plus(num[n_1];num[n_2])\longmapsto num[n]} $叫做指令变迁（instruction transition），代表一个expression真正执行产生的变迁，反应了其原始语义。还有一种叫搜索变迁，表达了一个表达式内部的指令（子表达式）的计算顺序（规定计算过程），比如$ \frac{e_1 \ val \ \ \ \ e_2\longmapsto e_2'}{plus(e_1;e_2)\longmapsto plus(e_1;e_2')} $代表operator plus的第一个参数先变迁（计算）到一个具体的值之后才能变迁（计算）第二个参数所对应的表达式。

by-value and by-name interpretation。按值解释和按名字解释。值，value，被认为是一个表达式变迁过程的final state。从introduction and elimination的角度来说，introduction form就是一个表达式已经不可再算的最终状态，就是value，elimination form就是用rule来规定表达式的内部子表达式的计算，直到子表达式都变成了introduction form。按值解释，是在一个子表达式的实例带入母表达式的对应变量之前先计算出值。按名字解释，子表达式在带入之前不进行任何计算而直接带入。二者的存在都是为了在执行的过程中省计算量，当一个表达式在程序中（其他表达式中）出现多次时，by-value可以将多次对于一个子表达式的计算合并成一次计算，但是对于一个注定不会被算到的子表达式来说，by-value的策略就会产生额外的、不需要的计算，这种情况时by-name的方式是更好的。

contextual dynamics。上下文动力学。结构动力学的变体，为了进一步强调指令执行的顺序。上下文动力学包含了结构动力学也包含的指令变迁，只是符号变成了单纯的箭头$ plus(num[n_1];num[n_2])\longrightarrow num[n] $。而结构动力学中的搜索变迁在上下文动力学中变成执行上下文（evaluation context），用$ \varepsilon $表示 。执行上下文是一个被挖掉一部分的表达式，是下一个要执行的表达式（指令）的母表达式的其他部分。被挖掉的部分被称作hole，用符号$ \circ $表示。$ \circ $中的表达式就是下一个要执行的指令。使用符号$ e'=\varepsilon\{e\} $代表上下文$ \varepsilon $中的孔洞被表达式$ e $填满之后形成了完成表达式$ e' $。那么上下文动力学就能表达为$ \frac{e_0\longrightarrow e_0'}{\varepsilon\{e_0\}\longmapsto \varepsilon\{e_0'\}} $ 。

equational dynamics。等价动力学。与结构或者上下文动力学都不同，等价动力学研究程序之间的等价关系。这一等价关系被称为定义等价性或者计算等价性。等价的概念可以表达为$ \chi|\Gamma \vdash e\equiv e':\tau $。一般隐含地要求$ e $的类型和$ e' $是一样的。两个表达式等价，说明两个表达式最终可以变迁到相等大小的值上。同时等价也有结构化的性质，自身与自身是等价的（反射性），$ \equiv $两侧的内容可以交换（交换性），$ e \equiv e' $以及$ e' \equiv e'' $那么$ e'\equiv e'' $（传递性）。

### Chapter  6: Type Safety
类型安全。类型安全代表程序在语法上完全正确。Statics和Dynamics都可以保证类型安全，Statics主要在运行前通过type system保证，Dynamics主要通过transition system来检查运行时错误。

type safety。类型安全，类型安全要满足两个条件。第一个称为preservation（类型保存），代表一个变迁断言$ e\longmapsto e' $前后的两个状态的type必须是一样的，即如果$ e:\tau $，那么$ e':\tau $。第二个条件称为progress，代表如果一个表达式不是值，那么它就应该可以变迁下去。

run-time error。运行时错误。有些错误在静力学阶段难以通过类型系统推断出来，那么在动力学阶段加入名为error的表达式和相关变迁规则来处理就可以，一旦出现错误的前提，那么直接变迁出error来弹出错误即可。而与一般编程语言的异常处理一样，在变迁系统中除了要规定错误的产生的直接rule之外，还需要给出子表示中的错误怎么从外层表达式传递的问题，即当子表达式变迁到error之后，外层表达式也要变迁到error。

### Chapter  7: Evaluation Dynamics
计算动力学。有时候为了简化dynamics的表达，我们需要仅仅关注表达式的计算结果，而对于其具体的执行过程乃至类型安全的检查都不在意，那么我们可以使用evaluation dynamics。

evaluation judgement。计算断言。表达为$ e\Downarrow v $，代表封闭表达式$ e $（见### Chapter  4）可以计算出值$ v $，用结构动力学可以表达为$ e\longmapsto^* v $。此外，一个表达式$ e $的计算结果和其在变迁系统的后继表达式$ e' $的计算结果是一致的，即如果$ e\longmapsto e' $并且$ e'\Downarrow v $，那么$ e\Downarrow v $。

rule of evaluation dynamics。计算动力学中的规则。一个语法的计算动力学由一大堆规则构成，规则前提是一个表达式中子表达式的结果以及表达式自身的语义，规则的结论是表达式自身的计算结果。例如，$ \frac{e_1\Downarrow num[n_1]\ \ e_2\Downarrow num[n_2]\ \ n_1+n_2=n}{plus(e_1;e_2)\Downarrow num[n]} $。与之前的动力学不同，计算动力学不是语法导向的，不是每一个语法表达式都对应一个动力学规则。

goes wrong。（一个表达式）发生错误。结构动力学通过声明什么样的语法是合法的，来检查错误，不合法的程序会在statics和dynamics过程中违背type safety的preservation和progress的范畴，在变迁过程中被卡住。与结构动力学相反，计算动力学省略了中间过程，所以只能显示地声明什么样的表达式是错误的。所以计算动力学难以进行类型检查，因为在表达式的无穷嵌套和组合中，正确的情况是少的，错误的情况是多的，所以穷尽所有的错误是不现实的。但是依旧有一种断言来定义一个表达式的错误：$ e?? $。

cost dynamics。开销动力学。结构动力学天然地表达了时间复杂度，通过看两个表达式之间变迁的步骤。但是evaluation dynamics因为省略了结构动力学中的中间步骤，所以对于开销的表达就需要额外的定义，通过扩展计算断言为$ e \Downarrow^k v $代表表达式$ e $需要$ k $步来变迁到值$ v $，等效于$ e\longmapsto^k v $。开销动力学包含了一系列由扩展过的evaluation judgement构成的rule，比如$ \frac{e_1\Downarrow^{k_1} num[n_1]\ \ e_2\Downarrow^{k_2} num[n_2]}{plus(e_1;e_2)\Downarrow^{k_1+k_2+1} num[n]} $。

### Chapter  8: Function Definitions and Values
函数定义和值。函数表达了对于一个变量的操作。一般来说，一个表达式可能包含多个变量，这些变量中有一个是可变的，剩下的部分是这一变量的上下文，也称作这一变量的函数。函数分为定义和调用两个过程。定义（definition）本质上是将一个变量绑定到一个abt中，并且这一变量本质上就是函数的参数。函数的应用（application）本质上是用一个表达式来替换函数的参数，最终形成一个没有绑定变量的完整表达式的过程。函数的输入是一个表达式（函数的参数），输出是另一个表达式。函数的参数是输出表达式的一个绑定变量，而输入的表达式会替换这个绑定变量。

first-order function。一阶函数。如果一个函数不能作为一个编程语言的变量、不能作为另一个函数的参数和返回值。那么这个函数就是一阶函数。一阶函数调用，表达为$ apply\{f\}(e) $。$ f $是函数名，$ e $是函数的参数。也可以表达为“将函数名$ f $绑定在参数$ e $上”。一阶函数的定义，表达为$ fun\{\tau_1;\tau_2\}(x_1.e_2;f.e) $。其中$ e_2 $是一个类型为$ \tau_2 $表达式，定义了函数的输出（按照编程语言的语境，定义了函数内容），称为range。其中$ x_1 $是一个类型为$ \tau_1 $，为函数的参数，是$ e_2 $的一个绑定变量，称为domain。在函数的定义中，$ e $是一个表达式，是函数声明的外层表达式，即函数声明会替换到$ e $中。$ e $通常会是函数调用的表达式。

function type of first-order function。一阶函数函数类型。在一般的语法中，一阶函数和一般的表达式是区分开来的。所以除了表达式的类型，还有函数的类型。表达为$ f(\tau_1):\tau_2 $，也被称作“函数头”。

statics of first-order function。一阶函数的静力学，用以函数的类型检查。一个是函数声明和调用的类型检查：
$ \frac{\Gamma,x_1:\tau_1\vdash e_2:\tau_2\ \ \ \ \Gamma,f(\tau_1):\tau_2\vdash e:\tau}{\Gamma\vdash fun\{\tau_1;\tau_2\}(x_1.e_2;f.e):\tau} $

$ \frac{\Gamma\vdash f(\tau_1):\tau_2\ \ \ \ \Gamma\vdash e_1:\tau_1}{\Gamma\vdash apply\{f\}(e_1):\tau_2} $

function substitution。函数替换。与一般变量的替换类似，表达式中的函数名也可以被替换。KaTeX parse error: Undefined control sequence: \textlbrackdbl at position 1: \̲t̲e̲x̲t̲l̲b̲r̲a̲c̲k̲d̲b̲l̲ ̲x_1.e_2/f \text…，其中$ f $是表达式$ e $中的一个函数名称，$ x_1.e_2 $是函数定义的表达式（输出）。因此函数定义的动力学可以写成函数替换的形式：
KaTeX parse error: Undefined control sequence: \textlbrackdbl at position 55: ….e)\longmapsto \̲t̲e̲x̲t̲l̲b̲r̲a̲c̲k̲d̲b̲l̲ ̲x_1.e_2/f \text…
higher-order function。高阶函数。不将函数的语法视为特殊的，将函数视为一般的变量，将函数的调用视为一般的表达式。在高阶函数中，一个函数可以作为另外一个函数的参数（domain）或者输出（range），这是与一阶函数最大的不同。

$ \lambda $-abstraction。函数的一种表达方式。写成$ \lambda\{\tau_1\}(x.e) $，是一个值。$ e $是函数的定义或者输出，$ x $绑定于$ e $是函数的参数。或者表达为$ \lambda(x:\tau_1)e $。在函数调用的时候，后面加上一个括号，在括号中加入一个type为$ \tau_1 $的表达式来实例化$ x $即可。

function type of higher-order function。高阶函数的函数类型。用函数输入表达式的类型$ \tau_1 $和输出表达式的类型$ \tau_2 $来定义高阶函数的类型，表达为$ \tau_1 \longrightarrow \tau_2 $。

dynamics of higher-order function。高阶函数的动力学。用$ ap(e_1,e_2) $代表函数的调用$ e_1(e_2) $，这一过程可以变迁为一个对表达式$ e_1 $中绑定变量的替换过程：
$ \frac{}{ap(\lambda\{\tau_2\}(x.e_1);e_2)\longmapsto [e_2/x]e_1} $

static scoping(binding)。静态绑定。变量具有作用域，变量的名字仅仅代表在一个abt内的那个变量，如abt的定义一样。

dynamic scoping。动态绑定。相当于变量没有作用域，所有的自由变量都是编程语言中的全局变量，在实际的运行中，根据执行的情况，某一个变量会被不同的作用域的同名变量改变其赋值，那么一个变量的赋值由其最后一次被赋值决定。Dynamic scoping in Lua举了一个例子。dynamic scoping只在高阶函数中存在，高阶函数可以通过互为输入输出嵌套在一起，才有内部的变量重名问题。动态绑定没法保证类型安全，在当代的编程语言中已经不存在了。

### Chapter  9: System T of Higher-Order Recursion
System T，关注一个程序是不是可以结束的。其核心语法为递归。System T中的递归强制了递归的次数。System T只关注自然数和函数，任何可以被System T的语义表达的程序是可以保证“一定可以结束的程序”。

recursor。递归器。每一轮递归都是拿上一轮递归的结果、当前的递归次数作为输入，得出这一轮递归的新结果用以下一轮递归。表达为$ rec\{\tau\}(e;e_0;x.y.e_1) $。其中，$ e_0 $是递归计算的初值，$ e $是要递归的总次数，$ e_1 $是每一轮递归中要执行的表达式。$ e_1 $中有两个绑定变量，$ x $代表当前已经递归次数，当$ x $不断自增、到达$ e $后递归过程会停止。$ y $是每一轮递归的结果和下一轮递归的输入，一开始从$ e_0 $开始，最后作为整个递归过程的输出。

statics of recursion。递归的静力学。递归相关的类型规则有很多，核心的只有一条，其中递归次数相关的变量是自然数类型（nat，在PFPL中，也可以通过在相关表达式或者变量头上加上一个横线来表示，如$ \overline{x} $），剩下的变量是相同的任意类型，从递归的语义本身也可以推测：
$ \frac{\Gamma\vdash e:nat\ \ \ \Gamma\vdash e_0:\tau\ \ \ \Gamma,x:nat,y:\tau\vdash e_1:\tau}{\Gamma\vdash rec\{\tau\}(e;e_0;x.y.e_1):\tau} $

dynamics of recursion。递归的动力学。包含了两条核心的结构动力学。第一个代表了，如果递归次数是0（用$ z $表示），那么递归的初值就是其结果。第二个代表了如果增加一次迭代次数（用$ s(e) $表示，$ s $代表+1），那么就是用当前归约的结果再执行一次归约的逻辑$ e_1 $。表达为如下的结构动力学：
$ \frac{}{rec\{\tau\}(z;e_0;x.y.e_1)\longmapsto e_0} $

$ \frac{s(e)\ val}{rec\{\tau\}(s(e);e_0;x.y.e_1)\longmapsto [e,rec\{\tau\}(e;e_0;x.y.e_1)/x,y]e_1} $

syntax of system T。system T的语义。相比一般的编程语言system T的语义非常简单，除了变量的定义、零的定义、函数的定义与调用、递归的定义之外，唯一一个可以改变变量的值的语法只有$ s(e) $，用以给一个表达式的值+1。即便是最为基本的加减乘数都是没有的。

definability of system T。在system T中可定义性。如果一个参数和返回值都是自然数的数学函数$ f $可以在system T中被定义，即可以找到表达式$ e_f(\overline{n})\equiv\overline{f(n)}:nat $，那么我们就说这一函数在system T中是可被定义的。在system T中可定义说明这一函数的执行的有穷的，不会出现无限的递归（或者循环，二者等价）。system的这一性质也被称作termination theorem（终止定理），对于一个system T中的表达式$ e:\tau $，一定有数值$ v $，使得$ e\equiv v:\tau $。这一定理的证明主要是用递归语法来执行数学归纳法。

### Chapter  10: Product Types
这一章提出一个叫做product（积）的概念。类似于一般编程语言中的元组或者数组的概念，将一些有限的变量组合起来。而product type是将元组中每一元素的类型组合起来产生的积类型。

binary products。二元积。二元积本质上为两个表达式（称作product的component）的有序组合，表达为$ \langle e_1,e_2 \rangle $，或者写成operator的形式$ pair(e_1;e_2) $。二元积的的类型本质上就是两个表达式的类型组合在一起，写成$ \tau_1 \times \tau_2 $或者$ \times(\tau_1;\tau_2) $的形式。二元组有两种操作，分别是取二元组的左右值，取左值写成$ e.l $或者$ pr[l](e) $，取右值写成$ e.r $或者$ pr[r](e) $。

nullary products。空积。代表空的元组，空的元组也是值的一种，类似于一般编程语言中的None或者null的语法。

finite products。有限积。将有限的表达式按照顺序组合起来，每一个表达式都有自己的类型，这些类型构成了有限积的类型。有限积中表达式的索引用$ I=\{i_1,...,i_n\} $表示。每一个索引对应的表达式类型和表达式表达为$ i_k \hookrightarrow \tau_k $，和$ i_k \hookrightarrow e_k $。$ \hookrightarrow $代表了“映射”或者对应的关系，在PFPL中，这一符号的含义很丰富，在这里代表索引对应的元素，也可以代表函数输入输出的关系，也可以代表递归（迭代）次数和迭代结果之间的关系，等等。有限积的表达形式和其类型的表达方式与二元积是一样的。当然也有取对应位置的值的操作，表达为$ pr[i](e) $或者$ e.i $。

### Chapter  11: Sum Types
Sum Types处理有选择的执行过程。一个表达式可以带入不同的分支中的不同的母表达式中（当然要和表达式中的变量的类型一致）。那么分支的索引加上表达式就也就是一个表达式被带入到不同分支的所有可能。sum type就是不同分支中表达式带入到的变量的type的集合。product type代表同时具有一系列类型，sum type代表从一系列类型中选一个类型。sum type的表达式可以用来表达分支、枚举之类的语法。

binary sums。二元和。表达了表达式向不同分支中母表达式的带入。可以写成case表达式，写成$ case(e_0;x_1.e_1;x_2.e_2) $和$ case\ e_0\{l.x_1\hookrightarrow e_1|r.x_2 \hookrightarrow e_2\} $的形式。$ l.x_1 $和$ r.x_2 $是注入（injection）表达式，包含了被替换的变量名称（$ x_1\ x_2 $）和被替换变量所处的分支的名称($ l\ r $)。也可以写成operator的形式，$ in[l]\{\tau_1;\tau_2\}(e) $。注入表达式也用以表达要注入的表达式，$ e_0 $可以表达为类似的形式$ l.e $，代表将表达式$ e $带入到$ l $分支的变量$ x_1 $中，并且给出表达式$ e_1 $。这个过程可以表达为一个核心的动力学断言：
$ case\ l.e\{l.x_1\hookrightarrow e_1|r.x_2 \hookrightarrow e_2\}\longmapsto [e/x_1]e_1 $

其中，替换（注入）与被注入的变量类型要一致。

nullary sums。空和。与空积不同，空和不产生任何意义的值。

sum type。和类型。代表注入表达式的type，其中包含了所有可以被注入的变量的type。对于前文的例子中，$ x_1:\tau_1\ \ \ x_2:\tau_2 $，那么其对应的注入表达式对应的类型满足对应的静力学规则：
$ \frac{\Gamma\vdash e:\tau_1}{\Gamma\vdash l.e:\tau_1+\tau_2} $

injection表达式是对应分支的被注入变量和分支名字的组合。injection表达式的类型是所有分支的中被替代变量的类型的组合，中间用$ + $连接，或者表达为$ +\{\tau_1;\tau_2\} $的形式。这一type的制定规则保证了静力学的type safety中被注入表达式与注入表达式之间的type一致（因为静力学阶段无法知道要注入的表达式是什么type，所以干脆就先把潜在的所有type都组合起来）。

finite sums。有限和。与有限积类似，分支超过两个的sum。在有限和中注入表达式为$ i.e $，也可以表达为$ in[i]\{\vec{\tau}\}(e) $。其中$ \vec{\tau} $代表了所有分支中被替代变量的类型，$ i $代表被注入的分支。

### Chapter  12: Constructive Logic
建设性逻辑。利用数学的推理方法来逻辑推理。即要证明一个命题是正确的，那就给出一个证明，如果要证明一个命题是错误的，那么只需要给出一个反例就好了。所谓的证明，就是以公理或者假设为源头的推导。这种证明方法不能证明所有的命题，有些命题可能（在有限时间内）证明和反例都给不出来（这类问题被称为undecided，或者称作open problem，反之可以给出真值的问题被称为decidable），这是建设性逻辑必然需要面对和接受的情况。

constructive logic。建设性逻辑。使用$ \phi $来代表一个命题。使用$ \phi\ true $代表命题$ \Phi $是正确的，是可以给出一个证明的。当然，一个命题的证明可能存在以其他命题存在证明为前提，表达为$ \phi_1\ true,...,\phi_n\ true \vdash \phi\ true $。这是假设命题，满足假设命题的一系列属性。

propositional logic。命题逻辑。使用一系列的语法符号来代表。命题符号$ \phi $可以被一系列命题逻辑的表达式替换。这些表达式中$ \top $代表真理；$ \bot $代表谬误；$ \wedge(\phi_1;\phi_2) $或者$ \Phi_1\wedge \Phi_2 $代表合取，当两个命题都为真（存在证明）时，那么两个命题的合取也为真；$ \vee(\phi_1;\phi_2) $或者$ \phi_1\vee \phi_2 $代表析取，当两个命题中有一个命题为真，那么两个命题的析取也为真；$ \supset(\phi_1;\phi_2) $或者$ \phi_1\supset \phi_2 $是暗含的意思，当$ \phi_1 $是真的时候，$ \phi_2 $也是真的，或者说$ \phi_2 $的证明包含在$ \phi_1 $的证明中。这里这里说的都是命题逻辑中的introduction规则，表达了带有连词命题的子命题的真值与母命题真值之间的关系。之后会介绍其elimination形式的规则，从带有连接词的母命题真值推导子命题的真值。

truth。真理。有一个公理，真理必然是正确的$ \overline{\Gamma\vdash \top\ true} $。

conjunction。合取。一个带有合取的命题是真的，那么其合取命题中的子命题也是真的。表达为$ \frac{\Gamma\vdash \phi_1\wedge \phi_2\ true}{\Gamma\vdash \phi_1\ true} $和$ \frac{\Gamma\vdash \phi_1\wedge \phi_2\ true}{\Gamma\vdash \phi_2\ true} $。

implication。暗含。暗含代表了一个命题为真可以作为另一个命题为真的假设前提，表达为$ \frac{\Gamma,\phi_1\ true\vdash\phi_2\ true}{\Gamma\vdash \phi_1\supset\phi_2\ true } $。如果作为假设前提的命题实际上就是真命题，那么被暗含的命题，肯定也是真命题，是可证的。表达为$ \frac{\Gamma\vdash \phi_1\supset\phi_2\ true\ \ \Gamma\vdash\phi_1\ true}{\Gamma\vdash\phi_2\ true} $。后面这个rule称为elimination rule，表达了从母命题到子命题的关系。

disjunction。析取。析取命题中的子命题为真都可以推导出某个其他命题为真，那么析取命题本身确实为真，那么那个其他命题也是真。
$ \frac{\Gamma\vdash\phi_1\vee\phi_2\ true\ \ \Gamma,\phi_1\ true\vdash\phi\ true\ \ \Gamma,\phi_2\ true\vdash\phi\ true}{\Gamma\vdash\phi\ true} $

negation。否定。命题$ \phi $的否定表达为$ \neg\phi $。$ \neg\phi\ true $当且仅当命题$ \phi $存在一个反例，也就是$ \phi $真值为假。

propositions-as-types。命题作为类型。将命题的证明接驳到之前的静力学和动力学理论当中，使得可以用程序来执行命题的证明。使用$ p:\phi $来表达相关语法，其中$ \phi $是命题，$ p $代表这一命题的证明（证明术语），称作proof term。

syntax of proof terms。证明术语的语法。表达了命题证明之间的产生关系，即怎么组合多个子命题的证明产生一个带有连接词母命题的证明，以及怎么从一个带有连接词的母命题中提取一个更小的命题的证明。因为在建设性命题中，真值为真本质上就是存在命题，所以证明术语的语法中也隐含了带连接符的命题中母命题和子命题的真值关系。从静力学和动力学的角度来说，合取本质上是一个product type，析取本质上是一个sum type，暗含本质上是一个function type。真理本质上是空积，谬论本质上是空和。部分规则表达为如下形式，其中introduction形式（从子命题的证明中推导出母命题的证明）本质上是二元组、case表达式、函数定义；其中投影、注入、函数调用为为elimination形式（从母命题的证明中推导出子命题的证明）：
$ \overline{\Gamma\vdash \langle\rangle:\top} $

$ \frac{\Gamma\vdash p_1:\phi_1\ \ \Gamma\vdash p_2:\phi_2}{\Gamma\vdash\langle p_1,p_2 \rangle:\phi_1\wedge \phi_2} $

$ \frac{\Gamma\vdash p:\phi_1\wedge \phi_2}{\Gamma\vdash p.l:\phi_1} $

$ \frac{\Gamma,x:\phi_1\vdash p_2:\phi_2}{\Gamma\vdash \lambda(x)p_2:\phi_1\supset\phi_2} $

$ \frac{\Gamma\vdash p:\phi_1\supset\phi_2\ \ \Gamma\vdash p_1:\phi_1}{\Gamma\vdash p(p_1):\phi_2} $

$ \frac{\Gamma\vdash p:\bot}{\Gamma\vdash case\ p\{\}:\phi} $

$ \frac{\Gamma\vdash p_1:\phi_1}{\Gamma\vdash l·p_1:\phi_1\vee\phi_2} $

$ \frac{\Gamma\vdash p:\phi_1\vee\phi_2\ \ \Gamma,x_1:\phi_1\vdash p_1:\phi\ \ \Gamma,x_2:\phi_2\vdash p_2:\phi}{\Gamma\vdash case\ p\{l.x\hookrightarrow p_1|r.x_2 \hookrightarrow p_2\}:\phi} $

Gentzen’s Principle。根岑原理。根岑原理认为，在introduction形式（由子命题证明母命题）对母命题的证明中可以通过elimination形式的rule还原出子命题的证明。即对于命题逻辑来说，子命题的证明可以在introduction form的规则中用以产生母命题的证明，而母命题可以进一步利用elimination form的规则复原对于子命题的证明。反之，对于母命题的证明可以用以在elimination form的规则中产生对于其子命题的证明，而通过introduction form的规则又可以从这些子命题中复原出对于母命题的证明。

### Chapter  13: Classical Logic
经典逻辑。与建设性逻辑（证明命题为真就给出证明，证明命题为假就给出反例）不同。经典逻辑无论证明命题为真还是命题为假都使用反证法来证明。主要的区别在想证明一个命题为真，那就先假设这个命题为假，然后推导出一个矛盾。在经典逻辑中，命题的真假是具有对称性的、是没有open problem的、非真即假的。经典逻辑对于命题为假的证明的严格程度和建设性逻辑是一样的，但是对于命题为真的证明是不用给出真正意义上的证明的，这会使得经典逻辑对于真命题的证明比建设性逻辑更弱。

provability and refutability。可证明性与可反驳性。使用$ \phi\ true $来表达命题$ \phi $是真的、可以证明的。使用$ \phi\ false $来证明命题$ \phi $是假的、是可以找出反驳的。使用$ \# $来代表一个矛盾。可以使用假设命题来表达与命题真假相关的断言$ J $，表达为：
$ \phi_1\ false,...,\phi_m\ false\ \ \psi_1\ true,...,\psi_n\ true\vdash J $

可以将所有假命题相关的假设表示为集合$ \bigtriangleup $，将所有真命题相关的假设表示为集合$ \Gamma $。那么这个假设断言可以表达为：$ \bigtriangleup\ \Gamma\vdash J $。

contradiction。矛盾。经典逻辑中计算命题真值的核心概念。当在相同的假设下，如果推导出一个命题同时为真和假，这就产生了一个矛盾，可以表达为rule：
$ \frac{\bigtriangleup\ \Gamma\vdash\phi\ false\ \ \ \bigtriangleup\ \Gamma\vdash\phi\ true}{\bigtriangleup\ \Gamma\vdash\#} $

而想证明一个命题是真的或者假的，那就先假设一个命题是假的或者真的，并且在此基础上可以推导出一个矛盾（$ \vdash $有“可推导”的意思），表达为：
$ \frac{\bigtriangleup,\phi\ false\ \ \Gamma\vdash\#}{\bigtriangleup\ \ \Gamma\vdash\phi\ true} $

这本质上是一个命题为真的证明，被称作proof；
$ \frac{\bigtriangleup,\phi\ true\ \ \Gamma\vdash\#}{\bigtriangleup\ \ \Gamma\vdash\phi\ false} $

这本质上是一个命题为假的证明，被称作refutation。

这种不直接给出证明，而是通过先假设一个相反的真值再推导出矛盾的证明方式，被称作间接证明。

introduction and elimination form。引入与消除形式。如前文所述，在不同领域中有不同的含义。在经典逻辑的rule中，introduction form表达了一个带有连接词的母表达式为真的时候子表达式的真值要求，这与建设性逻辑一致。而elimination form表达了当母表达式为假的时候，子表达式的真值要求，这与建设性逻辑完全不一样。

conjunction。合取。其introduction form的rule为$ \frac{\bigtriangleup\ \Gamma\vdash\phi_1\ true\ \ \bigtriangleup\ \Gamma\vdash\phi_2\ true}{\bigtriangleup\ \Gamma\vdash\phi_1\wedge\phi_2\ true} $，与constructive logic一致（后文不再赘述）。elimination form的rule为$ \frac{\bigtriangleup\ \Gamma\vdash\phi_1\ false}{\bigtriangleup\ \Gamma\vdash\phi_1\wedge\phi_2\ false} $（$ \phi_2 $同理），代表了命题的合取是false，需要每一个子命题是false的。

disjunction。析取。其elimination form的rule为$ \frac{\bigtriangleup\ \Gamma\vdash\phi_1\ false\ \ \bigtriangleup\ \Gamma\vdash\phi_2\ false}{\bigtriangleup\ \Gamma\vdash\phi_1\vee\phi_2\ false} $。

negation。否定。一个命题的否定会反转这一命题的真值。比如$ \frac{\bigtriangleup\ \Gamma\vdash\phi\ true}{\bigtriangleup\ \Gamma\vdash\neg\phi\ false} $。

implication。暗含。假设一个命令成立的时候另外一个命题也成立，那么这两个命题之间的暗含关系就成立。当作为假设的命题成立时，另外一个命题不成立，那么二者之间的暗含关系就不成立，这是暗含关系的elimination form。表达为$ \frac{\bigtriangleup\ \Gamma\vdash\phi_1\ true\ \ \bigtriangleup\ \Gamma\vdash\phi_2\ false}{\bigtriangleup\ \Gamma\vdash\phi_1\supset\phi_2\ false} $。

explicit syntax for proofs and refutations。显式证明与反驳的语法。与建设性命题中propositions-as-types的方式类似。将命题类比于type。因为在经典逻辑中false和true是对称的，反例也被显式表达出来。在显式表达证明（proof）和反驳（refutation）的语法中，$ \bigtriangleup\ \Gamma\vdash p:\phi $，代表$ p $是（真）命题$ \phi $的proof；$ \bigtriangleup\ \Gamma\vdash k\div\phi $，代表$ k $是（假）命题$ \phi $的refutation；$ \bigtriangleup\ \Gamma\vdash k\# p $代表针对同一个命题同时存在证明$ p $和反驳$ k $，产生了冲突。将证明与反驳视为表达式，命题的连接词视为operator。由此可以将经典逻辑的推理过程视为语法的逻辑，接驳到静力学和动力学系统中，其定义方式与建设性逻辑中的propositions-as-types的方式类似。矛盾在经典逻辑中比较重要。使用显式语法表达命题证明为：$ ccr(u.(k\#p)) $，命题反驳为：$ ccp(x.(k\#p)) $。以前者为例，其静力学可以表达为：
$ \frac{\bigtriangleup,u\div\phi\ \Gamma\vdash k\#p}{\bigtriangleup\ \Gamma\vdash ccr(u.(k\#p)):\phi} $

代表如果存在证明命题$ \phi $为假的反例$ u $，可以推导出矛盾$ k\#p $，那么这一过程就可以作为命题为真的证明。这一假设+推导出矛盾的过程可以直接简写为$ ccr(u.(k\#p)) $，并且作为命题为真的证明。

proof dynamics。证明动力学。一种简化矛盾关系的动力学。其主要的好处是消除矛盾关系$ k\#p $的反驳$ k $和证明$ p $中没有间接证明（反证法，或者说没有$ ccr $和$ ccp $语句），或者尽可能消去连接词。以$ ccp $表达式为例，其可以表达为一个替换，以消除$ ccp $表达式：
$ ccp(x.(k_1\#p_1)\#p_2\longmapsto[p_2/x]k_1\#[p_2/x]p_1 $

law of the excluded middle。排中率，在一个命题要么是真的要么是假的，没有中间地带。表达为$ \phi\vee\neg\phi\ true $。书中给出了这一问题的证明，使用了大量经典逻辑的规则以及假设断言的规则。整体上先将$ \phi\vee\neg\phi\ false $作为一切规则的假设。比如证明引入了基于反射性的公理$ \overline{\phi\vee\neg\phi\ false\vdash\phi\vee\neg\phi\ false} $。以及基于假设断言中weaken性质的公理$ \overline{\phi\vee\neg\phi\ false,\phi\ true\vdash\phi\vee\neg\phi\ false} $以及$ \overline{\phi\vee\neg\phi\ false,\phi\ true\vdash\phi\ true} $。作为推理的起点，然后使用前文所述的有关经典逻辑的rule在这一假设下推导出一个矛盾，形如$ \phi\vee\neg\phi\ false\vdash\# $。最终就可以得到排中律正确的证明。

