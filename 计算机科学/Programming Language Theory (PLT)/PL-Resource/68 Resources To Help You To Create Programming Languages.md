[toc]

- https://tomassetti.me/resources-create-programming-languages/



以下是全文翻译：

---

这里有一个新指南，旨在收集和组织你从零开始创建编程语言所需的所有知识。

创建一门编程语言是作为开发者可以梦想的最迷人的挑战之一。

问题在于有许多动态部分，需要做对很多事情，而且很难找到一张详细的地图来指引方向。当然，你可以找到关于编写半个解析器的教程、半生不熟的语言设计建议列表、一个天真的解释器示例。要找到这些东西，你需要花费数小时浏览论坛和跟随链接。

我们认为有必要通过收集相关资源、评估并组织它们来为你节省一些时间。这样你可以花时间使用好的资源，而不是寻找它们。

我们将资源围绕创建编程语言的三个阶段进行组织：设计、解析和执行。

### 第一部分
#### 语言设计
在创建编程语言时，你需要将想法转化为决策。这就是你在设计阶段所做的。

**开始之前…**
一些不错的资源可以增强你在语言设计方面的文化素养。

**文章**
- **设计下一门编程语言？了解人们如何学习！**，关于如何设计一门易于理解的编程语言的一些考虑因素。
- **关于语言设计的五个问题**，Paul Graham 关于编程语言设计的一些好点子和随机笔记。
- **傻瓜也能懂的编程范式：每个程序员都应该知道的**（PDF），实际上这是一本书的一个章节，除非你是那种拥有计算机科学学位的“傻瓜”，否则不太适合“傻瓜”。除此之外，它是对不同编程范式的一个很好的概述，有助于你理解你的语言将适合哪种范式。

**书籍**
- **编程语言中的设计概念**，如果你想在创建编程语言时做出有意识的选择，这是你需要的书。否则，如果你没有必要的理论背景，你有可能像别人一样做事。它还帮助你建立一个通用框架，以理解不同编程语言的行为及其原因。
- **编程语言的实用基础**，这本书大部分内容是关于研究和分类编程语言。但通过理解不同的可用选项，它也可以用于指导你的编程语言的实现。
- **编程语言实用主义**，第4版，这是理解当代编程语言最全面的书籍。它讨论了从C#到OCaml的各种方面，甚至包括函数式和逻辑编程语言等不同类型的编程语言。它还涵盖了实现的多个步骤和部分，如中间语言、链接、虚拟机等。
- **计算机程序的结构和解释**，第二版，是为已经拥有计算机科学学位的人介绍计算机科学的书籍。这本书受到了程序员的广泛赞誉，包括Paul Graham在亚马逊页面上的直接评价，帮助你发展一种新的编程语言思维方式。它相当抽象，示例使用Scheme。它还涵盖了编程语言的许多不同方面，包括垃圾回收等高级主题。

**类型系统**
围绕类型系统有长时间的讨论和无休止的争论。无论你最终做出什么选择，了解不同的立场都是有意义的。

**文章**
- **类型系统你需要知道什么**，这两篇是关于类型系统主题的良好入门文章。第一篇讨论静态/动态的二分法，第二篇深入探讨内省。
- **在辩论类型系统之前需要了解的内容**，如果你已经了解类型系统的基础，这篇文章适合你。它通过深入定义和细节帮助你更好地理解类型系统。
- **类型系统**（PDF），一篇关于类型系统形式化的论文，同时介绍了不同类型系统的更精确定义。

**书籍**
- **类型与编程语言**，一本全面了解类型系统的书。它将影响你设计编程语言和编译器的能力。它有强大的理论支持，但也解释了各个概念的实际重要性。
- **函数式编程与类型系统**，一门关于函数式编程类型系统的有趣大学课程。在著名的法国大学使用。还有笔记和演示材料可用，内容如你所预期的那样高级。
- **编程语言的类型系统**，这是一个关于（函数式）编程语言类型系统的较简单课程。

### 第二部分
#### 解析
解析将具体语法转换为计算机更容易管理的形式。通常意味着将人类编写的文本转换为更有用的源代码表示，即抽象语法树。

**欧几里得算法的抽象语法树**

解析通常包含两个部分：词法分析器和真正的解析器。词法分析器，也称为标记器或扫描器，将单个字符转换为标记，意义的原子。解析器则将标记组织成程序的适当抽象语法树。但由于它们通常需要协同工作，你可能会使用一个同时完成这两项任务的工具。

**工具**
- **Flex**，作为词法分析器生成器和（伯克利）Yacc或Bison，用于生成真正的解析器，是生成完整解析器的传统选择。它们已有几十年的历史，仍作为开源软件维护。它们是用C/C++编写和设计的。它们仍然有效，但在功能和对其他语言的支持方面有局限性。
- **ANTLR**，既是词法分析器又是解析器生成器。它也是更积极开发的开源软件。它用Java编写，但可以生成多种语言的词法分析器和解析器，如C#、C++、Python、JavaScript、Go等。
- **你自己的词法分析器和解析器**。如果你需要最佳性能并且能够创建你自己的解析器，只需具备必要的计算机科学知识。

**教程**
- **Flex和Bison教程**，关于这两个工具的良好入门教程，并附加一些提示。
- **Lex和Yacc教程**，40页的内容，是学习如何在几个小时内组合使用Lex和Yacc的理想起点。
- **Lex/Yacc视频教程，分两部分**，在一个小时的视频中，你可以学习使用Lex和Yacc的基础知识。
- **ANTLR大型教程**，著名且受欢迎的教程，解释了有关ANTLR的所有知识，并附加提示、技巧甚至更多资源以供深入了解。

**书籍**
- **Lex & Yacc**，尽管是1992年出版的书，但仍是该主题上最推荐的书籍之一。有些人认为是因为缺乏竞争，另一些人认为它已经足够好。
- **Flex & Bison：文本处理工具**，本千年内撰写的该主题最佳书籍。
- **ANTLR 4权威参考**，由该工具的主要作者撰写，这确实是关于ANTLR 4的权威书籍。它解释了所有的秘密，也是关于整个解析过程如何工作的良好入门。
- **解析技术**，第二版，是一本全面、先进且昂贵的书，了解解析的知识远超你的可能需求。

### 第三部分
#### 执行
要实现你的编程语言，即真正使其运作，你可以构建两种东西之一：编译器或解释器。如果你愿意，你也可以同时构建它们。这里你可以找到一个好的概述：**编译型和解释型语言**。

这里的资源专注于解释如何构建编译器和/或解释器，但出于实际原因，它们通常也解释了创建词法分析器和解析器的基础知识。

##### 编译器
编译器将原始代码转换为其他形式，通常是机器码，但也可以是任何更低级的语言，如C。在后一种情况下，有些人更喜欢使用“转译器”一词。

**工具**
- **LLVM**，一套模块化和可重用的编译器和工具链技术，用于创建编译器。
- **CLR**，.NET技术的虚拟机部分，允许执行转换为通用中间语言的不同语言。
- **JVM**，支持Java执行的Java虚拟机。
- **Babel**，一个JavaScript编译器。它具有非常现代的结构，基于插件，这意味着它可以轻松适应，但默认情况下实际上什么也不做，文档是这样说的。其创建者将其描述为支持旧环境中新JavaScript特性的工具，但它可以做更多事情。例如，你可以用它来创建基于JavaScript的语言，如LightScript。

**文章和教程**
- **在CLR上构建领域特定语言**，一篇关于如何在CLR上构建内部DSL的文章。虽然有些过时，因为它来自2008年，但它仍然是该主题的良好介绍。
- **2008年2月MSDN杂志的数字版**（CHM格式），包含一篇关于如何为.NET框架创建语言编译器的文章。它仍然是整个过程的有力概述。
- **使用LLVM框架创建一个工作编译器，第一部分和第二部分**，IBM关于创建自定义编译器的两部分系列文章，来自2012年，因此有些过时。
- **LLVM文档中的几个教程系列**，这是三个很棒的关联教程系列，介绍如何使用LLVM实现一种名为Kaleidoscope的语言。唯一的问题是有些部分并不总是最新的。
- **我的第一个LLVM编译器**，一个简短而温和的介绍，介绍如何使用LLVM构建编译器。
- **为Cpu0架构创建LLVM后端**，一个600页的教程，学习如何创建LLVM后端，也提供PDF或ePub格式。内容很棒，但英语水平不足。积极的一面是，如果你是学生，他们理解将理论知识转化为实际应用的痛苦，这本书是为你而写。
- **用于编译器教育的纳米通框架**，一篇介绍一个框架的论文，该框架通过将传统的整体方法转化为一系列简单的转换，以更简单的方式教授编译器的创建。如果你已经具备一些计算机科学的理论背景，这是一个有趣的阅读。
- **编译器构造的增量方法**（PDF），一篇也是教程的论文，通过一种更易学的方法开发一个基本的Scheme编译器。
- **超级小型编译器！**：“这是一个用易读的JavaScript编写的现代编译器所有主要部分的超简化示例”（并使用Babel）。有一个简短的介绍，但这更像是代码的演练而不是一个真正的教程。这可能是好事也可能是坏事，取决于你的偏好。

**书籍**
- **编译器：原理、技术与工具**，第二版，这是广为人知的“龙书”（因封面）第二版（紫龙）。有平装版，可能价格较低，但没有龙图案，所以你买不到。它是一本理论书，不要指望这些技术实际上包含很多可重用的代码。
- **现代编译器实现（ML版）**，被称为“虎书”，是“龙书”的竞争者。它详细讲解了编译器的结构和元素。这是一本理论书，尽管它用代码解释了概念。还有使用Java和C编写的其他版本，但普遍认为ML版是最好的。
- **工程编译器**，第二版，另一本文理论方法的编译器书籍，但它涵盖了更现代的方法，更易读。它也更专注于编译器的优化。因此，如果你需要理论基础和工程方法，这是最好的书。

##### 解释器
解释器直接执行语言而不将其转换为其他形式。

**文章和教程**
- **用Python从零开始构建一个简单的解释器**，一个四部分的系列文章，介绍如何用Python创建一个解释器，简单而不错。
- **让我们构建一个简单的解释器**，一个十二部分的系列，解释如何为Pascal的一个子集创建一个解释器。源代码是用Python编写的，但它有足够的理论以应用于其他语言。还有很多有趣的图片。
- **如何用JavaScript编写一个简单的解释器**，一篇组织良好的文章，解释如何用JavaScript创建一个简单的解释器。

**书籍**
- **用Go编写一个解释器**，尽管标题如此，实际上它展示了从解析到创建解释器的所有内容。这是一本当代书籍，无论是在最近几个月内出版，还是以动手学习的态度充满代码、测试且不使用第三方库。我们采访了作者Thorsten Ball。
- **打造解释器**，一本正在进行中的免费书籍，已经有良好的评价。它专注于构建高效的解释器，实际上将构建两个。它计划有恰到好处的理论量，以适应编程语言创建者的聚会。
  
### 第四部分
#### 通用
这些是涵盖创建编程语言过程广泛范围的资源。它们可能是全面的，也可能只是提供一般概述。

**工具**
- **Xtext**，是多个相关技术的一部分，用于开发编程语言，特别是领域特定语言。它允许你从解析器、编辑器到验证规则构建一切。你可以用它为你的语言构建出色的IDE支持。它通过在底层重用和链接现有技术（如ANTLR解析器生成器）简化了整个语言构建过程。
- **JetBrains MPS**，是一个投影语言工作台。投影意味着抽象语法树保存在磁盘上，并向用户展示一个投影。投影可以是类似文本的，或是表格、图表或你能想象到的任何其他形式。这的一个副作用是你不需要进行任何解析，因为这是不必要的。术语“语言工作台”表明JetBrains MPS是一个完整的技术系统，旨在帮助你创建自己的编程语言：从语言本身到为你的语言设计的IDE和支持工具。你可以用它来构建各种语言，但创建一切的可能性和需求使其非常适合为特定用途、特定受众创建领域特定语言。
- **Racket**，其作者描述为“通用编程语言以及世界上第一个用于开发和部署新语言的生态系统”。它是一个具有实践雄心的教学工具，甚至有一个宣言。它是一门用来创建其他语言的语言，拥有一切：从开发GUI应用程序的库到IDE以及开发逻辑语言的工具。它是Lisp家族语言的一部分，这意味着一切都遵循Lisp的方式。

**文章**
- **为JVM创建编程语言：入门指南**，概述了如何以及为什么为JVM创建语言。
- **如何编写一个非常基本的编译器的回答**，一个很好的回答，概述了构建编译器所需的步骤和可用的选项。
- **在Racket中创建语言**，来自ACM期刊的Racket的良好概述和介绍，带有代码。
- **可处理的Scheme实现**（PDF），一篇讨论专注于可靠性和可处理性的Scheme实现的论文。它构建了一个会即时生成某种字节码并由虚拟机立即执行的解释器。名称源于最初版本在48小时内完成。完整的源代码可在项目网站上找到。

**教程**
- **创建一个有用的语言及所有支持工具**，一系列从零开始的文章，教你从解析到构建带有自动补全的编辑器，同时构建一个面向JVM的编译器。
- **Racket的丰富文档**，有大量文档可以帮助你开始使用，即使你不熟悉任何编程语言。
- **Xtext的丰富文档**，包括几个15分钟的教程，可以帮助你开始使用。
- **JetBrains MPS的丰富文档**，包括专门的指南，如为专家语言设计师准备的指南。还有一个视频频道，提供帮助你使用软件的视频，以及关于在JetBrains MPS中创建你的第一个语言的介绍。
- **一小时内创建一种语言：stacker**，该教程提供了Racket及其工作流程的介绍。
- **创建你自己的编程语言**，一个五部分的系列，提供一个简单的示例，展示创建编程语言的原理，使用JavaScript构建。
- **创建你自己的编程语言**，一篇文章展示了使用JavaCC创建解析器和Java反射能力创建编程语言的一个简单且有些敷衍的方法。显然这不是正确的做法，但它展示了所有步骤，且易于跟随。
- **使用Flex、Bison和LLVM编写你自己的玩具编译器**，它如其名所示，使用适当的工具（Flex、Bison、LLVM等），但有些过时，因为它来自2009年。如果你想了解整体情况及所有内容如何配合，这仍然是一个良好的起点。
- **项目：一门编程语言**，这是《Eloquent JavaScript》一书的一章。它展示了如何使用JavaScript和正则表达式解析创建一个简单的编程语言。这一切都是错误的，但也奇妙地好。作者这样做是为了揭开创建编程语言的神秘面纱。你不应该做那些事情，但你可能会发现阅读它很有用。
- **设计编程语言I**，“从头到尾设计一种语言并构建一个解释器”。它介于文章和书籍之间。它有理论和实践的良好结合，并实现了所谓的Duck编程语言（灵感来自Duck-Typing）。计划中的第二部分，解释如何创建编译器，但从未完成。
- **构建编程语言介绍**，一个100分钟的视频，用JavaScript实现PHP的一个子集：有点令人困惑，但无可否认地很酷。
- **用Ruby编写编译器，从下往上**，一个45部分的系列文章，介绍如何用Ruby创建编译器。出于某种原因，它从代码生成开始，最终完成解析器。这是与传统（和逻辑）方法相反的做法。它很奇特，但也非常实在。
- **使用C# 4.0实现编程语言**，方法简单，库相当过时，但是一篇不错的文章，介绍如何在C#中构建解释器。
- **如何创建你自己的虚拟机！**（PDF），这个教程解释了如何用C#创建一个虚拟机。出乎意料地有趣，尽管不一定有实际应用。

**书籍**
- **如何创建实用的、轻量级的语言**，这里的重点是制作一个实际可用的语言。它解释了如何生成字节码、目标LLVM、为你的语言构建编辑器。一旦你读了这本书，你应该知道制作一个可用且高效语言所需的一切。顺便说一句，我们已经写了这本书。
- **如何创建你自己他妈的棒极了的编程语言**，这是一个100页的PDF和一个屏幕录制，教你如何使用Ruby或JVM创建编程语言。如果你喜欢快速而肮脏的方法，这本书会让你在短时间内入门。
- **编写编译器和解释器：软件工程方法**，第三版，这是一本实用的书，仍然教授编译器/解释器的正确方法。只是它不是学术重点，而是工程重点。这意味着它充满了Java代码，偶尔还有UML。技术和代码都有些过时，但如果你是软件工程师，并且需要实际做一些在几个月内通过适当审核流程后能够正确运行的东西，这是最好的书。
- **语言实现模式**，这是ANTLR的作者写的书，他也是一名计算机科学教授。所以这是一本理论与实践相结合的书，引导你从头到尾，从解析到编译器和解释器。顾名思义，它侧重于解释在构建这类软件时使用的已知工作模式，而不是直接解释所有理论然后进行实际应用。如果你需要立即可用的东西，这是一本书。Python设计者Guido van Rossum也推荐了它。
- **打造你自己的Lisp**，这是一本非常独特的书，旨在教你如何使用C语言并构建你自己的编程语言，使用一个迷你Lisp作为主要示例。你可以在线免费阅读或购买。它旨在教你关于C的知识，但你必须已经熟悉编程。书中甚至有迈克·泰森的图片（因为… Lisp）：这一切都如此奇怪，但迷人。
- **美丽的Racket：如何用Racket制作你自己的编程语言**，这是一本关于如何使用Racket构建编程语言的良好且持续更新的在线书籍。该书由一系列教程和解释及参考部分组成。这是一本技术上免费的书，但如果你使用它，应该付费。
- **编程语言：应用与解释**，一本有趣的书，解释了如何使用Racket从零开始创建编程语言。作者是一名教师，但是真正理解的那种。事实上，还有一系列配套讲座的录音，尽管音频有时质量堪忧。书籍和录音都有更新版本，但新书有不同的重点，因为它还想教授编程语言的相关内容。它也不使用Racket。如果你完全不了解编程，可能想阅读新版本；如果你有经验，可能更喜欢旧版本。
- **使用Xtext和Xtend实现领域特定语言**，第二版，是一本适合希望通过示例和测试驱动方法学习的人的好书。它涵盖了设计DSL的所有层面，从类型系统设计、解析到构建编译器。
- **实现编程语言**，是一本介绍如何使用JVM作为主要目标构建编译器和解释器的入门书籍。相关材料（演示、源代码等）在专门的网页上。它在理论和实践之间有良好的平衡，但明确作为教科书。因此，不要期待有太多可重用的代码。它也是典型的教科书，在你已经具备必要背景（或有教师指导）的情况下，是一本很好的生产性阅读材料，否则你可能会感到困惑。
- **实现函数式语言：教程**，一本免费书，解释如何从解析到解释器和编译器创建一个简单的函数式编程语言。另一方面：“这本书以实用的方法理解非严格函数式语言的实现，使用惰性图减少”。此外，还要预期有大量的数学内容。
- **DSL工程**，一本很棒的书，解释了使用语言工作台（如MPS和Xtext）构建DSL的理论和实践。这意味着除了传统的设计方面，如解析和解释器，它还涵盖了如何创建IDE或如何测试你的DSL。对软件工程师尤其有用，因为它还讨论了DSL的软工程和业务相关方面。也就是说，它讨论了为什么公司应该构建DSL。
- **小块Lisp**，一本有趣的书，详细解释了如何设计和实现Lisp家族语言。它描述了“11个解释器和2个编译器”，以及许多高级实现细节，如编译器优化。显然，这对有兴趣创建Lisp相关语言的人最有用，但对所有人来说都是一本有趣的读物。

### 第五部分
#### 总结
这里你拥有创建编程语言的最完整的高质量资源集合。你只需要决定首先阅读什么。

此时我们有两个建议给你：

1. **开始吧**。无论我们发送给你多少令人惊叹的资源，如果你不花时间练习、尝试并从错误中学习，你永远无法创建一门编程语言。
2. **如果你对构建编程语言感兴趣，应该订阅我们的新闻通讯**。你将收到关于新文章、更多资源、想法、建议的更新，并最终成为一个与你共同分享构建语言兴趣的社区的一部分。

---

### ----------------

Here it is a new **guide, to collect and organize all the knowledge that you need to create your programming language from scratch**.

Creating a programming language is one of the most fascinating challenge you can dream of as a developer.

The problem is that there are a lot of moving parts, a lot of things to do right and it is difficult to find a well detailed map, to show you the way. Sure, you can find a tutorial on writing half a parser there, an half-baked list of advices on language design, an example of a naive interpreter. To find those things you will need to spend hours navigating forums and following links.

We thought it was the case to save you some time by collecting relevant resources, evaluate them and organize them. So you can **spend time using good resources, not looking for them**.

We organized the resources around the three stages in the creation of a programming language: design, parsing, and execution.

## **Section 1** Designing the Language

When creating a programming language you need to take ideas and transform them in decisions. This is what you do during the design phase.

### Before You Start…

Some good resources to beef up your culture on language design.

#### Articles

- [Designing the next programming language? Understand how people learn!](http://www.theenterprisearchitect.eu/blog/2013/02/14/designing-the-next-programming-language-understand-how-people-learn/), a few considerations on how to design a programming language that it’s easy to understand.
- [Five Questions About Language Design](http://www.paulgraham.com/langdes.html), (some good and some random) notes on programming language design by Paul Graham.
- [Programming Paradigms for Dummies: What Every Programmer Should Know (PDF)](https://www.info.ucl.ac.be/~pvr/VanRoyChapter.pdf), this is actually a chapter of a book and it’s not really for dummies, unless they are the kind of dummies with a degree in Computer Science. Apart from that, it’s a great overview of the different programming paradigms, which can be useful to help you understand where your language will fit.

#### Books

- [Design Concepts in Programming Languages](https://mitpress.mit.edu/books/design-concepts-programming-languages), if you want to make deliberate choices in the creation of your programming language, this is the book you need. Otherwise, if you don’t already have the necessary theoretical background, you risk doing things the way everybody else does them. It’s also useful to develop a general framework to understand how the different programming languages behave and why.
- [Practical Foundations for Programming Languages](http://www.cambridge.org/it/academic/subjects/computer-science/programming-languages-and-applied-logic/practical-foundations-programming-languages-2nd-edition), this is, for the most part, a book about studying and classifying programming languages. But by understanding the different options available it can also be used to guide the implementation of your programming language.
- [Programming Language Pragmatics, 4th Edition](https://www.elsevier.com/books/programming-language-pragmatics/scott/978-0-12-410409-9), this is the most comprehensive book to understand contemporary programming languages. It discusses different aspects, of everything from C# to OCaml, and even the different kinds of programming languages such as functional and logical ones. It also covers the several steps and parts of the implementation, such as an intermediate language, linking, virtual machines, etc.
- [Structure and Interpretation of Computer Programs, Second Edition](https://mitpress.mit.edu/books/structure-and-interpretation-computer-programs), an introduction to computer science for people that already have a degree in it. A book widely praised by programmers, including Paul Graham directly on the [Amazon Page](https://www.amazon.com/Structure-Interpretation-Computer-Programs-Second/dp/0070004846/), that helps you developing a new way to think about programming language. It’s quite abstract and examples are proposed in Scheme. It also covers many different aspect of programming languages including advanced topics like garbage collection.

### Type Systems

Long discussions and infinite disputes are fought around type systems. Whatever choices you end up making it make sense to know the different positions.

#### Articles

- These are two good introductory articles on the subject of type systems. The first discuss the dichotomy [Static/Dynamic](https://thesocietea.org/2015/11/programming-concepts-static-vs-dynamic-type-checking/) and the second one dive into [Introspection](https://thesocietea.org/2016/02/programming-concepts-type-introspection-and-reflection/).
- [What To Know Before Debating Type Systems](https://cdsmith.wordpress.com/2011/01/09/an-old-article-i-wrote/), if you already know the basics of type systems this article is for you. It will permit you to understand them better by going into definitions and details.
- [Type Systems (PDF)](http://lucacardelli.name/Papers/TypeSystems.pdf), a paper on the formalization of type systems that also introduces more precise definitions of the different type systems.

#### Books

- [Types and Programming Languages](https://mitpress.mit.edu/books/types-and-programming-languages), a comprehensive book on understanding type systems. It will impact your ability to design programming languages and compilers. It has a strong theoretical support, but it also explains the practical importance of individual concepts.
- [Functional programming and type systems](http://gallium.inria.fr/~remy/mpri/), an interesting university course on type systems for functional programming. It is used in a well known French university. There are also notes and presentation material available. It is as advanced as you would expect.
- [Type Systems for Programming Language](http://www.doc.ic.ac.uk/~svb/TSfPL/), is a simpler course on type system for (functional) programming languages.

## **Section 2** Parsing

Parsing transforms the concrete syntax in a form that is more easily manageable by computers. This usually means transforming text written by humans in a more useful representation of the source code, an Abstract Syntax Tree.

![An abstract syntax tree for the Euclidean algorithm](https://p.ipic.vip/aepwy7.png)

There are usually two components in parsing: a lexical analyzer and the proper parser. Lexers, which are also known as tokenizers or scanners, transform the individual characters in tokens, the atom of meaning. Parsers instead organize the tokens in the proper Abstract Syntax Tree for the program. But since they are usually meant to work together you may use a single tool that does both the tasks.

#### Tools

- [Flex](https://github.com/westes/flex), as a lexer generator and [(Berkeley) Yacc](http://invisible-island.net/byacc/byacc.html) or [Bison](https://www.gnu.org/software/bison/), for the generation of the proper parser, are the venerable choices to generate a complete parser. They are a few decades old and they are still maintained as open source software. They are written in and thought for C/C++. They still works, but they have limitations in features and support for other languages.
- [ANTLR](https://github.com/antlr/antlr4), is both a lexer and a parser generator. It’s also more actively developed as open source software. It is written in Java, but it can generate both the lexer and the parser in languages as varied as C#, C++, Python, Javascript, Go, etc.
- *Your own lexer and parser.* If you need the best performance and you can create your own parser. You just need to have the necessary computer science knowledge.

#### Tutorials

- [Flex and Bison tutorial](http://aquamentus.com/flex_bison.html), a good introduction to the two tools with bonus tips.
- [Lex and Yacc Tutorial](http://epaperpress.com/lexandyacc/index.html), at 40 pages this is the ideal starting point to learn how to put together lex and yacc in a few hours.
- Video Tutorial on lex/yacc in [two](https://www.youtube.com/watch?v=54bo1qaHAfk) [parts](https://www.youtube.com/watch?v=__-wUHG2rfM), in an hour of video you can learn the basics of using lex and yacc.
- [ANTLR Mega Tutorial](https://tomassetti.me/antlr-mega-tutorial/), the renown and beloved tutorial that explains everything you need to know about ANTLR, with bonus tips and tricks and even resources to know more.

#### Books

- [lex & yacc](http://shop.oreilly.com/product/9781565920002.do), despite being a book written in 1992 it’s still the most recommended book on the subject. Some people say because the lack of competition, others because it is good enough.
- [flex & bison: Text Processing Tools](http://shop.oreilly.com/product/9780596155988.do), the best book on the subject written in this millennium.
- [The Definitive ANTLR 4 Reference](https://pragprog.com/book/tpantlr2/the-definitive-antlr-4-reference), written by the main author of the tool this is really the definitive book on ANTLR 4. It explains all of its secrets and it’s also a good introduction about how the whole parsing thing works.
- [Parsing Techniques, 2nd edition](http://www.springer.com/us/book/9780387202488), a comprehensive, advanced and costly book to know more than you possibly need about parsing.

## Section 3 Execution

To implement your programming language, that is to say to actually making something happens, you can build one of two things: a compiler or an interpreter. You could also build both of them if you want. Here you can find a good overview if you need it: [Compiled and Interpreted Languages](https://thesocietea.org/2015/07/programming-concepts-compiled-and-interpreted-languages/).

The resources here are dedicated to explaining how compilers and/or interpreters are built, but for practical reasons often they also explain the basics of creating lexers and parsers.

### Compilers

A compiler transforms the original code into something else, usually machine code, but it could also be simply any lower level language, such as C. In the latter case some people prefer to use the term *transpiler*.

#### Tools

- [LLVM](http://www.llvm.org/), a collection of modular and reusable compiler and toolchain technologies used to create compilers.
- [CLR](https://www.ecma-international.org/publications/files/ECMA-ST/ECMA-335.pdf), is the virtual machine part of the .NET technologies, that permits to execute different languages transformed in a common intermediate language.
- [JVM](https://docs.oracle.com/javase/specs/jvms/se8/html/), the Java Virtual Machine that powers the Java execution.
- [Babel](http://babeljs.io/), a JavaScript compiler. It has a very modern structure, based upon plugins, which means that it can be easily adapted, but it doesn’t do anything by default, really, [that is what the documentation says](http://babeljs.io/docs/plugins/). Its creators present it as a tool to support new featuers of JavaScript in old environment, but it can do much more. For instance, you can use to create JavaScript-based languages, such as [LightScript](https://github.com/lightscript/lightscript).

#### Articles & Tutorials

- [Building Domain Specific Languages on the CLR](https://www.infoq.com/articles/dsl-on-the-clr), an article on how to build internal DSL on the CLR. It’s slightly outdated, since it’s from 2008, but it’s still a good presentation on the subject.
- The digital issue of [MSDN Magazine for February 2008 (CHM format)](http://download.microsoft.com/download/3/A/7/3A7FA450-1F33-41F7-9E6D-3AA95B5A6AEA/MSDNMagazineFebruary2008en-us.chm), contains an article on how to Create a Language Compiler for the .NET Framework. It’s still a competent overview of the whole process.
- Create a working compiler with the LLVM framework, [Part 1](https://www.ibm.com/developerworks/opensource/library/os-createcompilerllvm1/index.html) and [Part 2](https://www.ibm.com/developerworks/opensource/library/os-createcompilerllvm2/index.html), a two-part series of articles on creating a custom compiler by IBM, from 2012 and thus slightly outdated.
- [A few series of tutorials froms the LLVM Documentation](http://llvm.org/docs/tutorial/index.html), this is three great linked series of tutorial on how to implement a language, called Kaleidoscope, with LLVM. The only problem is that some parts are not always up-to-date.
- [My First LLVM Compiler](http://www.wilfred.me.uk/blog/2015/02/21/my-first-llvm-compiler/), a short and gentle introduction to the topic of building a compiler with LLVM.
- [Creating an LLVM Backend for the Cpu0 Architecture](http://jonathan2251.github.io/lbd/), a whopping 600-pages tutorial to learn how to create a LLVM backend, also available in PDF or ePub. The content is great, but the English is lacking. On the positive side, if you are a student, they feel your pain of transforming theoretical knowledge into practical applications, and the book was made for you.
- [A Nanopass Framework for Compiler Education](http://www.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf), a paper that present a framework to teach the creation of a compiler in a simpler way, transforming the traditional monolithic approach in a long series of simple transformations. It’s an interesting read if you already have some theoretical background in computer science.
- [An Incremental Approach to Compiler Construction (PDF)](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf), a paper that it’s also a tutorial that develops a basic Scheme compiler with an easier to learn approach.
- [The Super Tiny Compiler!](https://the-super-tiny-compiler.glitch.me/intro): “This is an ultra-simplified example of all the major pieces of a modern compiler written in easy to read JavaScript” (and using Babel). There is a short introduction, but this is more of a walkthrough of the code than a proper tutorial. This may be a good or a bad thing, depending on your preference.

#### Books

- [Compilers: Principles, Techniques, and Tools, 2nd Edition](https://www.pearsonhighered.com/program/Aho-Compilers-Principles-Techniques-and-Tools-2nd-Edition/PGM167067.html), this is the widely known Dragon book (because of the cover) in the 2nd edition (purple dragon). There is a paperback edition, which probably costs less but it has no dragon on it, so you cannot buy that. It is a theoretical book, so don’t expect the techniques to actually include a lot of reusable code.
- [Modern Compiler Implementation in ML](https://www.cs.princeton.edu/~appel/modern/), this is known as a the Tiger book and a competitor of the Dragon book. It is a book that teaches the structure and the elements of a compiler in detail. It’s a theoretical book, although it explains the concept with code. There are other versions of the same book written using Java and C, but it’s widely agreed that the ML one is the best.
- [Engineering a Compiler, 2nd edition](https://www.elsevier.com/books/engineering-a-compiler/cooper/978-0-12-088478-0), it is another compiler book with a theoretical approach, but that it covers a more modern approach and it is more readable. It’s also more dedicated to the optimization of the compiler. So if you need a theoretical foundation and an engineering approach this is the best book to get.

### Interpreters

An interpreter directly executes the language without transforming it in another form.

#### Articles & Tutorials

- [A simple interpreter from scratch in Python](http://www.jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python-part-1), a four-parts series of articles on how to create an interpreter in Python, simple yet good.
- [Let’s Build A Simple Interpreter](https://ruslanspivak.com/lsbasi-part1/), a twelve-parts series that explains how to create a interpreter for a subset of Pascal. The source code is in Python, but it has the necessary amount of theory to apply to another language. It also has a lot of funny images.
- [How to write a simple interpreter in JavaScript](https://www.codeproject.com/articles/345888/how-to-write-a-simple-interpreter-in-javascript), a well organized article that explains how to create a simple interpreter in JavaScript.

#### Books

- [Writing An Interpreter In Go](https://interpreterbook.com/), despite the title it actually shows everything from parsing to creating an interpreter. It’s contemporary book both in the sense that is recent (a few months old), and it is a short one with a learn-by-doing attitude full of code, testing and without 3-rd party libraries. We have [interviewed the author](https://tomassetti.me/thorsten-ball/), Thorsten Ball.
- [Crafting Interpreters](http://craftinginterpreters.com/), a work-in-progress and free book that already has good reviews. It is focused on making interpreters that works well, and in fact it will builds two of them. It plan to have just the right amount of theory to be able to fit in at a party of programming language creators.

## Section 4 General

This are resources that cover a wide range of the process of creating a programming language. They may be comprehensive or just give the general overview.

### Tools

In this section we include tools that cover the whole spectrum of building a programming language and that are usually used as standalone tools.

- [Xtext](http://www.eclipse.org/Xtext/), is a framework part of several related technologies to develop programming languages and especially [Domain Specific Languages](https://tomassetti.me/domain-specific-languages). It allows you to build everything from the parser, to the editor, to validation rules. You can use it to build great IDE support for your language. It simplifies the whole language building process by reusing and linking existing technologies under the hood, such as the ANTLR parser generator.
- [JetBrains MPS](https://www.jetbrains.com/mps/), is a projectional language workbench. Projectional means that the Abstract Syntax Tree is saved on disk and a projection is presented to the user. The projection could be text-like, or be a table or diagram or anything else you can imagine. One side effect of this is that you will not need to do any parsing, because it is not necessary. The term *Language Workbench* indicates that Jetbrains MPS is a whole system of technologies created to help you create your own programming language: everything from the language itself to IDE and supporting tools designed for your language. You can use it to build every kind of language, but the possibility and need to create everything makes it ideal to create [Domain Specific Languages](https://tomassetti.me/domain-specific-languages) that are used for specific purposes, by specific audiences.
- [Racket](http://racket-lang.org/), is described by its authors as “a general-purpose programming language as well as the [world’s first ecosystem](https://www2.ccs.neu.edu/racket/pubs/manifesto.pdf) for developing and deploying new languages”. It’s a pedagogical tool developed with practical ambitions that has even a manifesto. It is a language made to create other languages that has everything: from libraries to develop GUI applications to an IDE and the tools to develop logic languages. It’s part of the Lisp family of languages, and this tells everything you need to know: it’s all or nothing and always the Lisp-way.

### Articles

- [Create a programming language for the JVM: getting started](https://tomassetti.me/create-a-programming-language-for-the-jvm-getting-started/), an overview of how and why to create a language for the JVM.
- [An answer to How to write a very basic compiler](https://softwareengineering.stackexchange.com/questions/165543/how-to-write-a-very-basic-compiler/165558#165558), a good answer to the question that gives an overview of the steps needed and the options available to perform the task of building a compiler.
- [Creating Languages in Racket](http://queue.acm.org/detail.cfm?id=2068896), a great overview and presentation of Racket from the ACM Journal, with code.
- [A Tractable Scheme Implementation (PDF)](https://web.archive.org/web/20180820150709/http://repository.readscheme.org/ftp/papers/vlisp-lasc/scheme48.pdf), a paper discussing a Scheme implementation that focuses on reliability and tractability. It builds an interpreter that will generate a sort of bytecode on the fly. This bytecode will then be immediately executed by a VM. The name derives from the fact that the original version was built in 48 hours. The full source code is available on [the website of the project](http://www.s48.org/).

### Tutorials

- [Create a useful language and all the supporting tools](https://tomassetti.me/getting-started-with-antlr-building-a-simple-expression-language/), a series of articles that start from scratch and teach you everything from parsing to build an editor with autocompletion, while building a compiler targeting the JVM.
- There is a [great deal of documentation for Racket](https://docs.racket-lang.org/index.html) that can help you to start using it, even if you don’t know any programming language.
- There is a [good amount documentation for Xtext](http://www.eclipse.org/Xtext/documentation/index.html) that can help you to start using it, including a couple of [15 minutes tutorials](http://www.eclipse.org/Xtext/documentation/102_domainmodelwalkthrough.html).
- There is a [great deal of documentation for JetBrains MPS](https://www.jetbrains.com/mps/documentation/index.html), including specialized guides such as one for [expert language designers](https://confluence.jetbrains.com/display/MPSD34/MPS+User's+Guide). There is a video channel with videos to help you use the software and an introduction on [Creating your first language in JetBrains MPS](https://www.youtube.com/watch?v=xXmYE9HrooM).
- [Make a language in one hour: stacker](http://beautifulracket.com/stacker/intro.html), the tutorial provides a tour of Racket and its workflow.
- [Make Your Own Programming Language](https://felix.plesoianu.ro/languages/scratch-lang/), a 5-parts series that provides a simple example on the principles of creating a programming language works, build with JavaScript.
- [Create Your Own Programming Language](https://www.codeproject.com/articles/50377/create-your-own-programming-language), an article that shows a simple and hacky way of creating a programming language using JavaCC to create a parser and the Java reflection capabilities. It’s clearly not the proper way of doing it, but it presents all the steps and it’s easy to follow.
- [Writing Your Own Toy Compiler Using Flex, Bison and LLVM](http://gnuu.org/2009/09/18/writing-your-own-toy-compiler/), it does what it says, using the proper tools (flex, bison, LLVM, etc.) but it’s slightly outdated since it’s from 2009. If you want to understand the general picture and how everything fit together this is still a good place where to start.
- [Project: A Programming Language](http://eloquentjavascript.net/11_language.html), this is a chapter of the book Eloquent Javascript. It shows how to create a simple programming language using JavaScript and parsing with regular expressions. This is all so wrong, yet it’s also bizarrely good. The author does it to demystify the creation of a programming language. You shouldn’t do any of that stuff, but you might find useful to read it.
- [Designing a Programming Language I](http://ducklang.org/designing-a-programming-language-i), “Designing a language and building an interpreter from beginning to end”. It is more than an article and less than a book. It has a good mix of theory and practice and it implements what it calls Duck Programming Language (inspired from [Duck-Typing](https://en.wikipedia.org/wiki/Duck_typing)). A [Part ii](https://github.com/gregtour/blog-posts/blob/master/article2.md), that explained how to create a compiler, was planned but never finished.
- [Introduction to building a programming language](https://www.youtube.com/watch?v=sTLkomM2P0o), A 100 minutes video that implements a subset of PHP in JavaScript: it is a bit troubling, but it is also undeniably cool.
- [Writing a compiler in Ruby, bottom up](http://hokstad.com/compiler), a 45-parts series of articles on creating a compiler with Ruby. For some reason it starts bottom up, that is to say from the code generation to end up with the parser. This is the reverse of the traditional (and logical) way of doing things. It’s peculiar, but also very down-to-earth.
- [Implementing Programming Languages Using C# 4.0](https://www.codeproject.com/Articles/272494/Implementing-Programming-Languages-using-Csharp), the approach is a simple one and the libraries are quite outdated, but it’s a neat article to read a good introduction on how to build an interpreter in C#.
- [How to create your own virtual machine! (PDF)](https://www.codeproject.com/KB/recipes/B32Machine1/VMCS.pdf), this tutorial explains how to create a virtual machine in C#. It’s surprisingly interesting, although not necessarily with a practical application.

### Books

- [How to create pragmatic, lightweight languages](https://tomassetti.me/create-languages), the focus here is on making a language that works in practice. It explains how to generate bytecode, target the LLVM, build an editor for your language. Once you read the book you should know everything you need to make a usable, productive language. Incidentally, we have written this book.
- [How To Create Your Own Freaking Awesome Programming Language](http://createyourproglang.com/), it’s a 100-page PDF and a screencast that teach how to create a programming language using Ruby or the JVM. If you like the quick-and-dirty approach this book will get you started in little time.
- [Writing Compilers and Interpreters: A Software Engineering Approach, 3rd edition](http://wiley.com/WileyCDA/WileyTitle/productCd-0470177071.html), it’s a pragmatic book that still teaches the proper approach to compilers/interpreters. Only that instead of an academic focus, it has an engineering one. This means that it’s full of Java code and there is also UML sprinkled here and there. Both the techniques and the code are slightly outdated, but this is still the best book if you are a software engineer and you need to actually do something that works correctly right now, that is to say in a few months after the proper review process has completed.
- [Language Implementation Patterns](https://pragprog.com/titles/tpdsl/language-implementation-patterns), this is a book from the author of ANTLR, which is also a computer science professor. So it’s a book with a mix of theory and practice, that guides you from start to finish, from parsing to compilers and interpreters. As the name implies, it focuses on explaining the known working patterns that are used in building this kind of software, more than directly explaining all the theory followed by a practical application. It’s the book to get if you need something that really works right now. It’s even recommended by Guido van Rossum, the designer of Python.
- [Build Your Own Lisp](http://www.buildyourownlisp.com/), it’s a very peculiar book meant to teach you how to use the C language and how to build you own programming language, using a mini-Lisp as the main example. You can read it for free online or buy it. It’s meant you to teach about C, but you have to be already familiar with programming. There is even a picture of Mike Tyson (because… lisp): it’s all so weird, but fascinating.
- [Beautiful Racket: how to make your own programming languages with Racket](http://beautifulracket.com/), it’s a good and continually updated online book on how to use Racket to build a programming language. The book is composed of a series of tutorials and parts of explanation and reference. It’s the kind of book that is technically free, but you should pay for it if you use it.
- [Programming Languages: Application and Interpretation](http://cs.brown.edu/courses/cs173/2012/book/), an interesting book that explains how to create a programming language from scratch using Racket. The author is a teacher, but of the good and understandable kind. In fact, there is also a [series of recordings](http://cs.brown.edu/courses/cs173/2012/Videos/) of the companion lectures, that sometimes have questionable audio. There is an updated version of the [book](http://papl.cs.brown.edu/2017/) and of [the recordings](https://brown.hosted.panopto.com/Panopto/Pages/Sessions/List.aspx?#folderID="76dfe3c9-667f-446a-b143-0818010f00e4"), but the new book has a different focus, because it want also to teach about programming language. It also doesn’t uses Racket. If you don’t know any programming at all you may want to read the new version, if you are an experienced one you may prefer the old one.
- [Implementing Domain-Specific Languages with Xtext and Xtend, 2nd edition](https://www.packtpub.com/application-development/implementing-domain-specific-languages-xtext-and-xtend-second-edition), is a great book for people that want to learn with examples and using a test-driven approach. It covers all levels of designing a DSL, from the design of the type system, to parsing and building a compiler.
- [Implementing Programming Languages](http://www.collegepublications.co.uk/computing/?00016), is an introduction to building compilers and interpreters with the JVM as the main target. There are related materials (presentations, source code, etc.) in [a dedicated webpage](https://web.archive.org/web/20160324035023/http://www1.digitalgrammars.com/ipl-book/). It has a good balance of theory and practice, but it’s explicitly meant as a textbook. So don’t expect much reusable code. It’s the typical textbook also in the sense that it can be a great and productive read if you already have the necessary background (or a teacher), otherwise you risk ending up confused.
- [Implementing functional languages: a tutorial](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/), a free book that explains how to create a simple functional programming language from the parsing to the interpreter and compiler. On the other hand: “this book gives a practical approach to understanding implementations of non-strict functional languages using lazy graph reduction”. Also, expect a lot of math.
- [DSL En](https://www.voelter.de/books.html)[gineering](http://www.voelter.de/books/), a great book that explains the theory and practice of building DSLs using language workbenches, such as MPS and Xtext. This means that other than traditional design aspects, such as parsing and interpreters, it covers things like how to create an IDE or how to test your DSL. It’s especially useful to software engineers, because it also discusses software engineering and business related aspects of DSLs. That is to say it talks about why a company should build a DSL.
- [Lisp in Small Pieces](http://www.cambridge.org/it/academic/subjects/computer-science/programming-languages-and-applied-logic/lisp-small-pieces), an interesting book that explain in details how to design and implement a language of the Lisp family. It describes “11 interpreters and 2 compilers” and many advanced implementation details such as the optimization of the compiler. It’s obviously most useful to people interested in creating a Lisp-related language, but it can be an interesting reading for everybody.

## Section 5 Summary

Here you have the most complete collection of high-quality resources on creating programming languages. You have just to decide what you are going to read first.

At this point we have two advices for you:

1. Get started. It does not matter how many amazing resources we will send you, if you do not take the time to practice, trying and learning from your mistake you will never create a programming language
2. If you are interested in building programming languages you should subscribe to our [newsletter](https://strumenta.com/subscribe-to-our-newsletter/). You will receive updates on new articles, more resources, ideas, advices and ultimately become part of a community that share your interests on building languages