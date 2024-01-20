[TOC]

- https://norvig.com/21-days.html

## 为何人人都这么着急？Why is everyone in such a rush?

信步走进任何一家书店，你会看到名为《如何在 24 小时内学会 Java》的书，还有各 种各样类似的书： 在几天内或几小时内学会 C，SQL，Ruby，算法等等，一眼望不到 尽头。我在 Amazon 上做了如下的高级检索 ：[[title: teach, yourself, hours, since: 2000](http://www.amazon.com/gp/search/ref=sr_adv_b/?search-alias=stripbooks&unfiltered=1&field-keywords=&field-author=&field-title=teach+yourself+hours&field-isbn=&field-publisher=&node=&field-p_n_condition-type=&field-feature_browse-bin=&field-subject=&field-language=&field-dateop=After&field-datemod=&field-dateyear=2000&sort=relevanceexprank&Adv-Srch-Books-Submit.x=16&Adv-Srch-Books-Submit.y=5) 得到了512 本这样的书。在前十本中有九本是关于编程的书（其它的是关于记账的）。把 “teach yourself” 换成 “learn” 或者用 "hours" 换成 "days"，可以得到了类似的结果。

Walk into any bookstore, and you'll see how to *Teach Yourself Java in 24 Hours* alongside endless variations offering to teach C, SQL, Ruby, Algorithms, and so on in a few days or hours. The Amazon advanced search for [[title: teach, yourself, hours, since: 2000](http://www.amazon.com/gp/search/ref=sr_adv_b/?search-alias=stripbooks&unfiltered=1&field-keywords=&field-author=&field-title=teach+yourself+hours&field-isbn=&field-publisher=&node=&field-p_n_condition-type=&field-feature_browse-bin=&field-subject=&field-language=&field-dateop=After&field-datemod=&field-dateyear=2000&sort=relevanceexprank&Adv-Srch-Books-Submit.x=16&Adv-Srch-Books-Submit.y=5) and found 512 such books. Of the top ten, nine are programming books (the other is about bookkeeping). Similar results come from replacing "teach yourself" with "learn" or "hours" with "days."

从上面的搜索结果可以看出来，要么就是人们对计算机技术的学习如饥似渴，要么就是计算机技术实在太简单，不费吹灰之力就能学会。

The conclusion is that either people are in a big rush to learn about programming, or that programming is somehow fabulously easier to learn than anything else.

Felleisen 以及其它人在他们的著作[《如何设计程序》](http://www.ccs.neu.edu/home/matthias/HtDP2e/index.html)一书中明确指出了这种“速成”的趋势，并评论到：“垃圾的编程技术当然非常容易，傻子都能在 21 天之内学会，哪怕他天生就是个白痴。” Abtruse Goose 的漫画中也有[类似尝试](http://abstrusegoose.com/249)。

Felleisen *et al.* give a nod to this trend in their book *[How to Design Programs](http://www.ccs.neu.edu/home/matthias/HtDP2e/index.html)*, when they say "Bad programming is easy. *Idiots* can learn it in *21 days*, even if they are *dummies*." The Abtruse Goose comic also had [their take](http://abstrusegoose.com/249).

让我们分析一下，象一本名为*[《24 小时内学会 C++》](http://www.amazon.com/exec/obidos/ISBN=1556225679/4094-7934802-027992)*的书意味着什么：

Let's analyze what a title like *[Teach Yourself C++ in 24 Hours](http://www.amazon.com/Sams-Teach-Yourself-Hours-5th/dp/0672333317/ref=sr_1_6?s=books&ie=UTF8&qid=1412708443&sr=1-6&keywords=learn+c%2B%2B+days)* could mean:

- **学习：** 在 24 小时里，你没有时间写一些重大的程序，并从成功或失败中得益。你没有时间与有经验的程序员合作，并理解在 C++ 的环境下工作是怎么回事。一句话，你不会有时间学到太多东西。因此他们只能谈论一些肤浅的东西，而不是深入的理解。正如亚历山大教皇所说，浅尝辄止是危险的事情。
- **Teach Yourself:** In 24 hours you won't have time to write several significant programs, and learn from your successes and failures with them. You won't have time to work with an experienced programmer and understand what it is like to live in a C++ environment. In short, you won't have time to learn much. So the book can only be talking about a superficial familiarity, not a deep understanding. As Alexander Pope said, a little learning is a dangerous thing.
- **C++：** 在 24小时的时间里，你可能学会 C++ 的语法（如果你 已经学过类似的语言），但你学不到更多的如何使用这些语法的知识。也就是说， 假如你曾是个 BASIC  程序员，你可以学着用 C++ 语法写出 BASIC 风格的程序，但你不可能了解 C++ 真正的好处（和坏处）。那么关键是什么？ [Alan Perlis](http://www-pu.informatik.uni-tuebingen.de/users/klaeren/epigrams.html) 说过：“一种不改变你编程的思维方式的语言，不值得去学。” 一种可 能的情况是：你必须学一点儿 C++（或可能性更大的像 JavaScript 或 Processing 之类），因为你为了完成某种特定的任务，需要与一个现存的工具建立接口。不过那不是学习如何编程，而是在学习如何完成那个任务。
- **C++:** In 24 hours you might be able to learn some of the syntax of C++ (if you already know another language), but you couldn't learn much about how to use the language. In short, if you were, say, a Basic programmer, you could learn to write programs in the style of Basic using C++ syntax, but you couldn't learn what C++ is actually good (and bad) for. So what's the point? [Alan Perlis](http://www-pu.informatik.uni-tuebingen.de/users/klaeren/epigrams.html) once said: "A language that doesn't affect the way you think about programming, is not worth knowing". One possible point is that you have to learn a tiny bit of C++ (or more likely, something like JavaScript or Processing) because you need to interface with an existing tool to accomplish a specific task. But then you're not learning how to program; you're learning to accomplish that task.
- **24 小时内：** 很不幸，这不够，原因由下一节告诉我们。
- **in 24 Hours:** Unfortunately, this is not enough, as the next section shows.

## 在十年里学会编程 Teach yourself programming in ten years

研究人员([Bloom (1985)](http://www.amazon.com/exec/obidos/ASIN/034531509X/), [Bryan & Harter (1899)](http://www.norvig.com/21-days.html#bh), [Hayes (1989)](http://www.amazon.com/exec/obidos/ASIN/0805803092), [Simmon & Chase (1973)](http://www.norvig.com/21-days.html#sc))一系列调查显示，在各个领域内，要想获得专业级别的水平，大约需要 10 年时间的努力。参与此项调查的领域包括：国际象棋，作曲，发报，绘画，钢琴演奏，游泳，网球以及对神经心理学和拓扑学的研究。若要在某一领域内达到专家级的水平，其关键在于“*刻意*练习”，也就是说，并非是机械地，一遍又一遍地练习，而是要不断地挑战自我，试图超越自身当前的水平，通过不断的尝试挑战，并在尝试的过程中和尝试之后对自身的表现进行分析和总结，吸取经验，纠正之前犯过的各种错误。把这一过程不断重复，才能取得成功。所谓的“捷径”是不存在的，即使对于莫扎特这种天才来说，也没有捷径可走，尽管 4 岁就开始作曲，可是他也花了 13 年的时间，才真正地写出了世界级的作品。再举一个例子，甲壳虫乐队（The Beatles）,他们似乎在 1964 年凭借一系列热门单曲和其在艾德沙利文秀（The Ed Sullivan show）上的演出一炮而红，但是你也许不知道，他们早在 1957 年就在利物浦和汉堡两地进行小规模演出了，而在此之前的非正式演出更是不计其数。甲壳虫乐队的主要成名曲《*Sgt. Peppers*》，则是 1967 年才发行的。

Researchers ([Bloom (1985)](http://www.amazon.com/exec/obidos/ASIN/034531509X/), [Bryan & Harter (1899)](http://www.norvig.com/21-days.html#bh), [Hayes (1989)](http://www.amazon.com/exec/obidos/ASIN/0805803092), [Simmon & Chase (1973)](http://www.norvig.com/21-days.html#sc)) have shown it takes about ten years to develop expertise in any of a wide variety of areas, including chess playing, music composition, telegraph operation, painting, piano playing, swimming, tennis, and research in neuropsychology and topology. The key is *deliberative* practice: not just doing it again and again, but challenging yourself with a task that is just beyond your current ability, trying it, analyzing your performance while and after doing it, and correcting any mistakes. Then repeat. And repeat again. There appear to be no real shortcuts: even Mozart, who was a musical prodigy at age 4, took 13 more years before he began to produce world-class music. In another genre, the Beatles seemed to burst onto the scene with a string of #1 hits and an appearance on the Ed Sullivan show in 1964. But they had been playing small clubs in Liverpool and Hamburg since 1957, and while they had mass appeal early on, their first great critical success, *Sgt. Peppers*, was released in 1967.

[Malcolm Gladwell](http://www.amazon.com/Outliers-Story-Success-Malcolm-Gladwell/dp/0316017922?tag=job0ae-20) 推广了这个理念，尽管他的重点在 10000 个小时而不是 10 年。Henri Cartier-Bresson (1908-2004)说过，“你所拍摄的头 10000 张照片都是最糟糕的”。（他没有预料到，有了数码相机后，有些人可以在一周内达到这个数量~）真正的专业或许需要花费一生的时间。Samuel Johnson (塞缪尔 约翰逊 1709-1784)说：“在任何一个领域要想做到极好,势必穷尽一生的精力，否则根本无法企及。” Chaucer (乔叟 1340-1400)也发出过“生命如此短暂，技能如此高深”的感叹。Hippocrates (希波克拉底，约 400BC)因写下了如下的句子而被人称颂：“ars longa, vita brevis”，该句是来自于一个更长的引用：”Ars longa, vita brevis, occasio praeceps, experimentum periculosum, iudicium difficile”, 这段话（拉丁文）翻译成英语就是：“生命很短暂，但是技艺却很高深，机遇转瞬即逝，探索难以捉摸，抉择困难重重”。

[Malcolm Gladwell](http://www.amazon.com/Outliers-Story-Success-Malcolm-Gladwell/dp/0316017922) has popularized the idea, although he concentrates on 10,000 hours, not 10 years. Henri Cartier-Bresson (1908-2004) had another metric: "Your first 10,000 photographs are your worst." (He didn't anticipate that with digital cameras, some people can reach that mark in a week.) True expertise may take a lifetime: Samuel Johnson (1709-1784) said "Excellence in any department can be attained only by the labor of a lifetime; it is not to be purchased at a lesser price." And Chaucer (1340-1400) complained "the lyf so short, the craft so long to lerne." Hippocrates (c. 400BC) is known for the excerpt "ars longa, vita brevis", which is part of the longer quotation "Ars longa, vita brevis, occasio praeceps, experimentum periculosum, iudicium difficile", which in English renders as "Life is short, [the] craft long, opportunity fleeting, experiment treacherous, judgment difficult."

显然，没有一个单一的数字可以作为最终的回答：假定所有技能（比如编程，国际象棋，跳棋，音乐）都需要相同的时间来成为专家或者假定任何人都需要一样的时间是没理由的。正如 [K. Anders Ericsson](http://www.amazon.com/K.-Anders-Ericsson/e/B000APB8AQ/ref=dp_byline_cont_book_1) 教授所指出的那样：“在绝大多数领域，哪怕是最有天分的个人，他们达到最高水平所耗费的时间也是极为显著的。10000 小时这个数字只是想让你意识到即便是人们口中的那些最具天赋的个体想达到最高水平也需要年复一年的每周花上 10 到 20 小时。”

Of course, no single number can be the final answer: it doesn't seem reasonable to assume that all skills (e.g., programming, chess playing, checkers playing, and music playing) could all require exactly the same amount of time to master, nor that all people will take exactly the same amount of time. As Prof. [K. Anders Ericsson](http://www.amazon.com/K.-Anders-Ericsson/e/B000APB8AQ/ref=dp_byline_cont_book_1)puts it, "In most domains it's remarkable how much time even the most talented individuals need in order to reach the highest levels of performance. The 10,000 hour number just gives you a sense that we're talking years of 10 to 20 hours a week which those who some people would argue are the most innately talented individuals still need to get to the highest level."

## 你想当程序员么？So you want to be a programmer

这是我为编程成功开出的方子：Here's my recipe for programming success:

- 设法对编程感**兴趣**，并且因为它有趣而编一些程序。确保编程一直充满足够乐趣，这样你才愿意投入十年/10000 小时宝贵时间。
- Get **interested** in programming, and do some because it is fun. Make sure that it keeps being enough fun so that you will be willing to put in your ten years/10,000 hours.
- **写程序**。 最好的学习方式是 [从实践中学习](http://www.engines4ed.org/hyperbook/nodes/NODE-120-pg.html)。 用更技术性的话说，“在一个给定的领域内，个人的最大能力不 是自动地由扩展了的经验取得的，但即使是高度有经验的人也可以通过有意识的 努力来提高自己的能力” [(p. 366)](http://www2.umassd.edu/swpi/DesignInCS/expertise.html) 和 “最有效的学习需要因人而异的适当难度，目标明确的任务，丰富的信息反馈，以及重复的机会和错误修正。” (p. 20-21) 此书 *[Cognition in Practice: Mind，Mathematics，and Culture in Everyday Life](http://www.amazon.com/exec/obidos/ASIN/0521357349)* 是阐明此观点的令人感兴趣的参考文献。
- **Program**. The best kind of learning is [learning by doing](http://www.engines4ed.org/hyperbook/nodes/NODE-120-pg.html). To put it more technically, "the maximal level of performance for individuals in a given domain is not attained automatically as a function of extended experience, but the level of performance can be increased even by highly experienced individuals as a result of deliberate efforts to improve." [(p. 366)](http://www2.umassd.edu/swpi/DesignInCS/expertise.html) and "the most effective learning requires a well-defined task with an appropriate difficulty level for the particular individual, informative feedback, and opportunities for repetition and corrections of errors." (p. 20-21) The book *[Cognition in Practice: Mind, Mathematics, and Culture in Everyday Life](http://www.amazon.com/exec/obidos/ASIN/0521357349)* is an interesting reference for this viewpoint.
- 与其他程序员**交流**； 阅读其它程序。这比任何书本或训练课程都重要。
- **Talk with** other programmers; read other programs. This is more important than any book or training course.
- 如果愿意，在**大学**里呆上 4 年（或更长，在研究生院里）。你会接触到一些需要文凭的工作，你会对此领域有更深的理解。如果你不喜欢学校， 你可以（会有所牺牲）依靠自身或在工作中获得相似的经验。在任何情况下，光啃书本是不够的。Eric Raymond，*The New Hacker's Dictionary* 一书的作者，说过，“计算机科学不能把任何人变成编程专家，就象光研究刷子和颜料不会使人变成画家一样。” 我雇佣过的最好的程序员之一仅有高中文凭；他做出了许多[优秀的](http://www.xemacs.org/)[软件](http://www.mozilla.org/)，有他自己的[新闻组](http://groups.google.com/groups?q=alt.fan.jwz&meta=site%3Dgroups)， 而且通过股票期权买到了自己的[夜总会](http://en.wikipedia.org/wiki/DNA_Lounge)。
- If you want, put in four years at a **college** (or more at a graduate school). This will give you access to some jobs that require credentials, and it will give you a deeper understanding of the field, but if you don't enjoy school, you can (with some dedication) get similar experience on your own or on the job. In any case, book learning alone won't be enough. "Computer science education cannot make anybody an expert programmer any more than studying brushes and pigment can make somebody an expert painter" says Eric Raymond, author of *The New Hacker's Dictionary*. One of the best programmers I ever hired had only a High School degree; he's produced a lot of [great](http://www.xemacs.org/) [software](http://www.mozilla.org/), has his own [news group](http://groups.google.com/groups?q=alt.fan.jwz&meta=site%3Dgroups), and made enough in stock options to buy his own [nightclub](http://en.wikipedia.org/wiki/DNA_Lounge).
- 和其他程序员**一起做项目**。在其中的一些项目中作为最好的程序员； 而在另一些项目中是最差的。当你是最好的，你能测试领导项目的能力，用你 的观点激发别人。当你是最差的，你学习杰出者是怎么做的，了解他们不喜欢做 什么（因为他们吩咐你做事）。
- Work on **projects with** other programmers. Be the best programmer on some projects; be the worst on some others. When you're the best, you get to test your abilities to lead a project, and to inspire others with your vision. When you're the worst, you learn what the masters do, and you learn what they don't like to do (because they make you do it for them).
- 在其他程序员***之后\*接手项目**。使自己理解别人写的程序。 当程序的原作者不在的时候，研究什么需要理解并且修改它。思考如何设计你的 程序以便后来者的维护。
- Work on **projects \*after\*** other programmers. Understand a program written by someone else. See what it takes to understand and fix it when the original programmers are not around. Think about how to design your programs to make it easier for those who will maintain them after you.
- 学习至少半打的编程语言。包括一种支持类抽象的语言（像 Java  或 C++），一种支持函数化抽象的语言（像 Lisp 或 ML 或 Haskell），一种支持语法抽象的语言（像 Lisp），一种支持声明规格说明的语言（像 Prolog 或 C++ 的模板），以及那些强调并行的语言（像 Clojure 或 Go）。
- Learn at least a half dozen **programming languages**. Include one language that emphasizes class abstractions (like Java or C++), one that emphasizes functional abstraction (like Lisp or ML or Haskell), one that supports syntactic abstraction (like Lisp), one that supports declarative specifications (like Prolog or C++ templates), and one that emphasizes parallelism (like Clojure or Go).
- 请记住“计算机科学”中有“**计算机**”一词。了解你的计算机要花多 长时间执行一条指令，从内存中取一个字（有cache），从磁盘中读取连续的字， 和在磁盘中找到新的位置。（[答案](http://daiyuwen.freeshell.org/gb/misc/21-days-cn.html#answers)）
- Remember that there is a "**computer**" in "computer science". Know how long it takes your computer to execute an instruction, fetch a word from memory (with and without a cache miss), read consecutive words from disk, and seek to a new location on disk. ([Answers here.](http://www.norvig.com/21-days.html#answers))
- 参与一种语言**标准化**的工作。它可以是 ANSI C++ 委员会， 也可以是决定你周围小范围内的编程风格是应该两个还是四个空格缩进。通过任何一种方式，你了解到其他人在某种语言中的想法，他们的理解深度，甚至一些他们这样想的原因。
- Get involved in a language **standardization** effort. It could be the ANSI C++ committee, or it could be deciding if your local coding style will have 2 or 4 space indentation levels. Either way, you learn about what other people like in a language, how deeply they feel so, and perhaps even a little about why they feel so.
- 具备良好的判断力，能尽快地从语言标准化的纠缠中**脱身**。
- Have the good sense to **get off** the language standardization effort as quickly as possible.

明白了这些，仅从书本中你能得到多少就成了一个问题。在我第一个孩子出生前， 我读了所有的（关于育儿的）*How to* 书籍，仍然感觉是个手足无措的新手。30 个月以后，我 的第二个孩子快要出生了，我回头温习这些书了吗？ 没有。相反，我依靠我的个人 经验，它比专家写的数千页书更有用和可靠。

With all that in mind, its questionable how far you can get just by book learning. Before my first child was born, I read all the *How To* books, and still felt like a clueless novice. 30 Months later, when my second child was due, did I go back to the books for a refresher? No. Instead, I relied on my personal experience, which turned out to be far more useful and reassuring to me than the thousands of pages written by experts.

Fred Brooks在他的随笔 *[《没有银弹》](http://citeseer.nj.nec.com/context/7718/0)* 中定出了一个寻找优秀软件设计者的三步计划：

Fred Brooks, in his essay *[No Silver Bullet](http://en.wikipedia.org/wiki/No_Silver_Bullet)* identified a three-part plan for finding great software designers:

1. 尽可能早地，有系统地识别顶级的设计人员。
2. 为设计人员指派一位职业导师，负责他们技术方面的成长，仔细地为他们规划 职业生涯。
3. 为成长中的设计人员提供相互交流和学习的机会。

1. Systematically identify top designers as early as possible.
2. Assign a career mentor to be responsible for the development of the prospect and carefully keep a career file.
3. Provide opportunities for growing designers to interact and stimulate each other.

此计划假设某些人已经具备了杰出设计者的必要才能； 要做的只是如何恰当地诱导他们。 [Alan Perlis](http://www-pu.informatik.uni-tuebingen.de/users/klaeren/epigrams.html) 说得更简明扼要：“每个人都能被教会雕刻：对米开朗其罗而言， 反倒是告诉他哪些事不要做。同样的道理也适用于优秀的程序员。”

This assumes that some people already have the qualities necessary for being a great designer; the job is to properly coax them along. [Alan Perlis](http://www-pu.informatik.uni-tuebingen.de/users/klaeren/epigrams.html) put it more succinctly: "Everyone can be taught to sculpt: Michelangelo would have had to be taught how not to. So it is with the great programmers".

Perlis 认为，伟大的软件开发人员都有一种内在的特质，这种特质往往比他们所接受的训练更重要。但是这些特质是从哪里来的呢？是与生俱来的？还是通过后天勤奋而来？正如 Auguste Gusteau（动画电影《料理鼠王》里的幻象大厨）所说，“谁都能做饭，但只有那些无所畏惧的人才能成为大厨！”我把“将你生命中的大部分时间花在刻意练习上”更多视作为一种自愿！但或许“无所畏惧”才是概括它的方式。或者，就像是《料理鼠王》里那个与 Gusteau 作对的刻薄的美食评论家 Anton Ego 说的那样：“不是任何人都能成为伟大的艺术家，不过，伟大的艺术家可以来自任何地方。”

Perlis is saying that the greats have some internal quality that transcends their training. But where does the quality come from? Is it innate? Or do they develop it through diligence? As Auguste Gusteau (the fictional chef in*Ratatouille*) puts it, "anyone can cook, but only the fearless can be great." I think of it more as willingness to devote a large portion of one's life to deliberative practice. But maybe *fearless* is a way to summarize that. Or, as Gusteau's critic, Anton Ego, says: "Not everyone can become a great artist, but a great artist can come from anywhere."

所以尽管买本 Java/Ruby/Javascript/PHP 的书吧。你可能会从中学到点儿东西。但作为一个程序员，你不会在 21 天内或 24 小时内改变你的人生，或你实际的水平。你尝试过连续 24 个月不懈努力提高自己么？呵呵，如果你做到了，好吧，那么你开始上路了……

So go ahead and buy that Java/Ruby/Javascript/PHP book; you'll probably get some use out of it. But you won't change your life, or your real overall expertise as a programmer in 24 hours or 21 days. How about working hard to continually improve over 24 months? Well, now you're starting to get somewhere...

## 参考文献 References

 

Bloom, Benjamin (ed.) *[Developing Talent in Young People](http://www.amazon.com/exec/obidos/ASIN/034531509X)*, Ballantine, 1985.

Brooks, Fred, *[No Silver Bullets](http://citeseer.nj.nec.com/context/7718/0)*, IEEE Computer, vol. 20, no. 4, 1987, p. 10-19.

Bryan, W.L. & Harter, N. "Studies on the telegraphic language: The acquisition of a hierarchy of habits. *Psychology Review*, 1899, 8, 345-375

Hayes, John R., *[Complete Problem Solver](http://www.amazon.com/exec/obidos/ASIN/0805803092)* Lawrence Erlbaum, 1989.

Chase, William G. & Simon, Herbert A. ["Perception in Chess"](http://books.google.com/books?id=dYPSHAAACAAJ&dq="perception+in+chess"+simon&ei=z4PyR5iIAZnmtQPbyLyuDQ) *Cognitive Psychology*, 1973, 4, 55-81.

Lave, Jean, *[Cognition in Practice: Mind, Mathematics, and Culture in Everyday Life](http://www.amazon.com/exec/obidos/ASIN/0521357349)*, Cambridge University Press, 1988.

## 答案 Answers

典型 PC 系统各种操作指令的大概时间（nanosec：纳秒）Approximate timing for various operations on a typical PC:























| execute typical instruction执行基本指令                      | 1/1,000,000,000 sec = 1 nanosec        |
| ------------------------------------------------------------ | -------------------------------------- |
| fetch from L1 cache memory从一级缓存中读取数据               | 0.5 nanosec                            |
| branch misprediction分支预测失败                             | 5 nanosec                              |
| fetch from L2 cache memory从二级缓存获取数据                 | 7 nanosec                              |
| Mutex lock/unlock互斥加锁/解锁                               | 25 nanosec                             |
| fetch from main memory从主内存获取数据                       | 100 nanosec                            |
| send 2K bytes over 1Gbps network通过1G bps 的网络发送 2K 字节 | 20,000 nanosec                         |
| read 1MB sequentially from memory从内存中顺序读取 1MB 数据   | 250,000 nanosec                        |
| fetch from new disk location (seek)从新的磁盘位置获取数据（寻道） | 8,000,000 nanosec                      |
| read 1MB sequentially from disk从磁盘中顺序读取 1MB 数据     | 20,000,000 nanosec                     |
| send packet US to Europe and back从美国发送一个报文包到欧洲再返回 | 150 milliseconds = 150,000,000 nanosec |

## 附录：语言的选择 Appendix: Language Choice

不少人问我，他们首先该学哪种编程语言。没有绝对的答案，不过请考虑以下几 点：

Several people have asked what programming language they should learn first. There is no one answer, but consider these points:

- *利用你的朋友*。当被问起“我该用哪种操作系统，Windows，Unix， 还是 Mac？”，我总是回答：“你朋友用什么，你就用什么。” 你从朋友那能学到知识，这种优势可以抵销不同操作系统或语言之间本质的差异。也考虑你将来的朋友：程序员社区 — 你将成为它的一部分如果你继续往前走的话。你选择的语言是否有一个成长中的社区，还是人数不多、即将消亡？ 有没有书籍、网站、 在线论坛回答你的问题？ 你喜欢论坛里的那些人吗？
- *Use your friends*. When asked "what operating system should I use, Windows, Unix, or Mac?", my answer is usually: "use whatever your friends use." The advantage you get from learning from your friends will offset any intrinsic difference between OS, or between programming languages. Also consider your future friends: the community of programmers that you will be a part of if you continue. Does your chosen language have a large growing community or a small dying one? Are there books, web sites, and online forums to get answers from? Do you like the people in those forums?
- *保持简单.* 像 C++ 和 Java 这样的语言是为经验丰富的 程序员组成的团队进行专业开发而设计的，他们专注于代码运行时的效率。因此，这些语言有些部分非常复杂。 而你关注的是如何编程，不需要那些复杂性。你 需要的是这样的语言： 对单个的编程新手来说，它易学易记。
- *Keep it simple*. Programming languages such as C++ and Java are designed for professional development by large teams of experienced programmers who are concerned about the run-time efficiency of their code. As a result, these languages have complicated parts designed for these circumstances. You're concerned with learning to program. You don't need that complication. You want a language that was designed to be easy to learn and remember by a single new programmer.
- *练习*。你偏爱哪种学弹钢琴的方式：通常的交互式的方式，你一 按下琴键就能听到音符；还是“批量”模式，你只有弹完整首曲子才能听到音符？ 显然，用交互模式学习弹钢琴更容易些，编程也一样。坚持用交互模式学习并使 用一种语言。
- *Play.* Which way would you rather learn to play the piano: the normal, interactive way, in which you hear each note as soon as you hit a key, or "batch" mode, in which you only hear the notes after you finish a whole song? Clearly, interactive mode makes learning easier for the piano, and also for programming. Insist on a language with an interactive mode and use it.

有了上面的准则，我推荐的第一个编程语言是 [Python](http://python.org/) 或 [Scheme](http://www.schemers.org/)。另一个选择是 Javascript，不是因为它对初学者友好，而是因为有大量的在线教程，比如 [Khan Academy's tutorial](https://www.khanacademy.org/computing/cs/programming)。因人而异，还有其它好的选择。如果你的年纪是 10 岁以下，你可能更喜欢[Alice](http://alice.org/) 或 [Squeak](http://www.squeak.org/) 或 [Blockly](https://blockly-demo.appspot.com/static/apps/index.html) （大一些年纪的人也可能会喜欢）关键是你要选择并开始实践。

Given these criteria, my recommendations for a first programming language would be **[Python](http://python.org/)** or **[Scheme](http://www.schemers.org/)**. Another choice is Javascript, not because it is perfectly well-designed for beginners, but because there are so many online tutorials for it, such as [Khan Academy's tutorial](https://www.khanacademy.org/computing/cs/programming). But your circumstances may vary, and there are other good choices. If your age is a single-digit, you might prefer [Alice](http://alice.org/) or [Squeak](http://www.squeak.org/) or [Blockly](https://blockly-demo.appspot.com/static/apps/index.html) (older learners might also enjoy these). The important thing is that you choose and get started.

## 附录：书籍和其它资源 Appendix: Books and other resources

不少人问我，他们该从什么书籍或网页开始学起。我重申“仅从书本里学习是不 够的。” 但我还是推荐：

Several people have asked what books and web pages they should learn from. I repeat that "book learning alone won't be enough" but I can recommend the following:

- **Scheme:**[ Structure and Interpretation of Computer Programs (Abelson & Sussman)](http://www.amazon.com/gp/product/0262011530)可能是最好 的计算机科学的入门书，而且它的确把讲授编程作为理解计算机科学的一种方法。 你可以在 [online videos of lectures](http://www.swiss.ai.mit.edu/classes/6.001/abelson-sussman-lectures/) 观看本书的在线视频教程，以及 [complete text online](http://mitpress.mit.edu/sicp/full-text/book/book.html) 的在线文字版。 学习本书是有些挑战的，选用其它语言会成功的人在这里可能会遇到一些困难。
- **Scheme:** [Structure and Interpretation of Computer Programs (Abelson & Sussman)](http://www.amazon.com/gp/product/0262011530) is probably the best introduction to computer science, and it does teach programming as a way of understanding the computer science. You can see [online videos of lectures](http://www.swiss.ai.mit.edu/classes/6.001/abelson-sussman-lectures/) on this book, as well as the [complete text online](http://mitpress.mit.edu/sicp/full-text/book/book.html). The book is challenging and will weed out some people who perhaps could be successful with another approach.
- **Scheme:**[ How to Design Programs (Felleisen et al.)](http://www.amazon.com/gp/product/0262062186)是关于如何用一种优美的、函数化的方式设 计程序的最好的书之一。
- **Scheme:** [How to Design Programs (Felleisen *et al.*)](http://www.amazon.com/gp/product/0262062186) is one of the best books on how to actually design programs in an elegant and functional way.
- **Python:**[ Python Programming: An Intro to CS (Zelle)](http://www.amazon.com/gp/product/1887902996)是优秀的 Python 入门指导。
- **Python:** [Python Programming: An Intro to CS (Zelle)](http://www.amazon.com/gp/product/1887902996) is a good introduction using Python.
- **Python:**[ Python.org](http://python.org/)上有许多在线[指导](http://wiki.python.org/moin/BeginnersGuide)。
- **Python:** Several online [tutorials](http://wiki.python.org/moin/BeginnersGuide) are available at [Python.org](http://python.org/).
- **Oz:**[ Concepts, Techniques, and Models of Computer Programming (Van Roy & Haridi)](http://www.amazon.com/gp/product/0262220695) 被视为Abelson & Sussman 的当代继承者。它是对编程的高层次概念的巡视。 涉及的范围比 Abelson & Sussman 更广，同时可能更容易学习和跟进。 它用了叫 做  Oz 的语言，不太知名，却可以作为学习其它语言的基础。
- **Oz:** [Concepts, Techniques, and Models of Computer Programming (Van Roy & Haridi)](http://www.amazon.com/gp/product/0262220695) is seen by some as the modern-day successor to Abelson & Sussman. It is a tour through the big ideas of programming, covering a wider range than Abelson & Sussman while being perhaps easier to read and follow. It uses a language, Oz, that is not widely known but serves as a basis for learning other languages.

## Notes 备注

T. Capey points out that the [Complete Problem Solver](http://www.amazon.com/exec/obidos/ASIN/0805803092) page on Amazon now has the "Teach Yourself Bengali in 21 days" and "Teach Yourself Grammar and Style" books under the "Customers who shopped for this item also shopped for these items" section. I guess that a large portion of the people who look at that book are coming from this page. Thanks to Ross Cohen for help with Hippocrates.

T. Capey 指出，Amazon 网页上那个 [Complete Problem Solver](http://www.amazon.com/gp/product/0805803092/ref=as_li_qf_sp_asin_il_tl?ie=UTF8&tag=job0ae-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0805803092) 页面把《21 天搞定孟加拉语》以及《21 天学会语法》这两本书移到了“购买此书的用户还购买过这些产品”这个区域内。我估计大部分人就是从这个区域看到这本书的。感谢 Ross Cohen 的帮助。