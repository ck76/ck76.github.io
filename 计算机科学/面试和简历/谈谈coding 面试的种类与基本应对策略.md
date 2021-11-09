- https://zhuanlan.zhihu.com/p/262279257
- https://www.1point3acres.com/bbs/thread-435598-1-1.html

### 谈谈coding 面试的种类与基本应对策略

大致上，面试官在开始面试前会收到一封email，里面会大致说明每个人需要侧重于考察面试者的哪个方面。对于coding来说，一般有三类问题，每个面试官会被分配到一类问题。

## 1. solid coding

这类问题 说白了，谁都知道怎么做，纯粹就是考察coding 是不是扎实，平时自己写code多不多，能不能快速地把自己的idea 转化为code。对于面试者来说属于必考种类，new grad 一般会有两轮甚至三轮这样的题目，有很多工作经验的人可能只有一轮。这类题目不过关，很可能电面死掉或者前几轮突然死亡。

solid coding 一般可以分成两小类：

1.1 考察你对算法的基本理解以及边界条件的运用。比如findKth largest integer, search in rotate array, bit manipulation等等

1.2 考察你对基本数据结构以及复杂度的理解。比如binary search tree, linkedlist vs array, stack, tree, dfs, bfs等等

按照难度来分，easy 比如 3sum, tree level order iterator, medium 难度的比如reverse linked list from index m to index n, course schedule, string multiplication, hard 难度比如valid number, 复杂的比如calculator等

应对策略

1.1 类型，如果是easy和medium，那就是希望你又快又好，除了勤奋和熟练，没有什么好策略。对于像merge sort, partition这类算法，如果7-8分钟内还写不出bug free我估计就没戏了。easy问题多注意边界条件，int 溢出， null pointer, 负数，非法输入等。hard 1.1 请参考1.3

1.2类型，easy和medium请在写代码前多阐明复杂度，这类数据结构的问题往往也可以在coding 前画图来表示运行状态，图画得清楚也是个重要的加分项。hard请参考1.3

1.3 hard类型的coding 题目，往往考察你的solid coding的能力，即我在前文提到的，你做事的方式和你思考问题的方法。即给你一个coding任务，你如何从白板开始，一步步的做出bug free的程序。这类问题的过程重于结果。比如valid number，你能确保每实现一个模块，都没有regression，都没有bug，比你一下子实现所有的feature 但是很多bug 要好很多。

一般来说，面试官看你能否一步步地分隔出小的coding 模块（method)，你如何设计 test case, 你如何能够确保这些test case能cover 所有的scenario，你是不是和面试官提前做了足够的沟通并且限定了coding 范围。从这个角度来说，valid number 其实是个很不错的solid coding 面试题。限于篇幅，我就不展开说了。

## 2. problem resolving

这类问题对于NG是关键，也是能帮你脱颖而出的关键。计算并不是只有算法，还需要数据库，操作系统，网络，安全等方面的知识。NG在这方面要弱一些，所以面试官希望NG能展现出思维敏捷、多思考、快速反应的能力。problem resolving 就是为了考察这个方面而诞生的。

problem resolving 也可以分为四个小类型。

2.1 API design

这类问题是为了更深入考察你对数据结构的理解和运用。例如 LRU cache, insert delete getRandom ALL O(1), design Twitter等等

2.2 Abstraction

这类问题考察你能不能把一个相对抽象的问题规约到你熟悉的问题上面。比如skyline problem， int stream find median， cleaning robot等等

2.3 计算机小程序

比如线程池，爬虫，日志merge等，random generator等。

2.4 dynamic programming

这类问题有点像solid problem resolving.主要考察你是不是有systematic的方法来降低一个brute force程序的复杂度。

这类问题一般都不是很easy的问题，根据面试官心情，可能走得很深很难，也可能最后演变成bar raiser。

应对策略

2.1 主要考察你对数据结构的深层次认识。首先请同时确保你理解了题目的意思，最好能问清点条件，例如immutable array max subarray sum， **那数组将来会变吗**？问清这类问题有助于你写代码前做好重构和测试的准备。其次，如果你能证明你选择的算法的复杂度，甚至证明这就是最佳的算法复杂度，那是一个大大的加分项。如果不能，至少你也问问面试官是不是已经满意了再开始写代码。

2.2 这个我自己也头疼，说实话如果第一次遇见了skyline，我也不知道能不能搞定。大家有好办法请回复能系统化的解决这类问题，我个人觉得很多时候需要靠灵光一现。

2.3 这类问题主要看你平时的积累，也是一大类不能通过leetcode 练习的问题。临时抱佛脚的话我推荐《Java Concurrency in practice》这本书

2.4 动态规划

我不知道为什么很多人 害怕动态规划。面试中的动态规划大致分为单向递归（首或者尾），O(n2)或者O(n3)距离递归，O(mn)递归，有限定条件的NP(背包)。每种类型听几节课，懂基本原理即可。至于贪心和带状态的dp(走道铺砖)一类的dp，至少我没在面试中遇到过，因为很难临时造出一道这样的题目，面试官一般也没这个能力或者时间来思考题目是不是严谨。贪心准备下加油站，迪杰斯特拉，最小生成树就足够了。

## 3. bar raiser

这类问题只有当onsite 应聘者数量远远大于head count的时候，或者你前几轮明显超出电面时对你的定位才会发生，其目的是帮助公司选择最优秀的人。对应聘者来说，坏消息是要度过痛苦的一小时，好消息是你能充分了解这公司厉害的人有多厉害，能充分展示你的能力，甚至被越级录取也不是不可能。

不要上来就写代码！不要上来就写代码！不要上来就写代码！给面试官展示核心算法，然后写几个测试数据，演示一下代码的运行很重要。一定要有develop 思路的过程，这也能体现对解法的充分理解。不要上来就写代码（第四遍！）。

复杂度意义上的最优解其实只是面试官所期望的最优解法的一部分。也就是说，不管你有没有背过，复杂度这个考察点算是过了。

直接说出最优解或者大家期待的秒题这一做法，会对其他考察点有负面的影响，例如你对问题的抽象能力，如何以小见大进行归纳，对语言细节和底层机制的把控与讨论等等。

如果你看大公司比如FB给的Interview Guide，他们期望的解题过程基本上符合算法研究的方法论---如何通过对问题的观察、通过举例简化的输入进行归纳而抓住各种性质。一般的问题，核心的不变形（invariant) 也就一两个，列出来后代码基本上也就是搬砖了。

回到skyline 这类问题，背题其实是知识体系不全面又受时间精力所迫的无奈之举，短期方案我也不知道。但是真想提升自己内力的话，可以有意识积累一些“一锤定音”的东西：想象一下有一个对skyline 这题完全没有头绪的人向你请教，能不能给出三到四点关键性质（定理），让他恍然大悟并且可以自己写出代码？面试时列举出这样的关键定理甚至能让面试官自身对一个问题学习到新的理解。