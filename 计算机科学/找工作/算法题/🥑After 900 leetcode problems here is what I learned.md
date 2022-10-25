# 

[TOC]

# After 900 leetcode problems here is what I learned

# Why we practice algorithm? (not just for interview)

Solving Algorithm problems could help train our mind be used to useful datastructure and algorithms, be ready to use them to solve issues .

instead of CRUD works ,the leetcode problems usually requires good understanding of data structures like tree, graph, heap, even though most of them are unlikely being used in daily work, but when the requirement comming in(path finding, shorted path (weight based), graph/tree traverse, reference counting etc) it will helpful if we can come out with solutions fast and then compare the pros and cons to get an optimal solution.

so we need practise .

# Problem Tags

Below are common tags (sorted by number of problems being asked during interview based on my experience). there are much more listed on leetcode

## DFS

A classic way traverse tree or graph start from a node reach until end. or within a metrix finding the area by going up,down,left,right .usually every tree or graph or 0–1 metrix searching problem could be related to DFS .

## BFS

Unlike DFS. BFS could be done with queue .Find the next reachable nodes, add into queue until queue is empty. unlike DFS, BFS focus more on all “next nodes” from current “parents” queue.

## Binary Search

Within sorted array find some value , usually fall into this category .

## Heap

When find max or min , or topK issue . usually this structure is available in the language you choose, can directly use it . java, c#, python etc. if not you can also build your heap .

## Trie

Given dictionary , build the Trie , do word search or frequency .

## Stack

Use min or max stack to compare the top 1 in stack while looping through some array.

## Linklist

common problems are like find cycle, common node between 2 link list , reverse , etc .

## Greedy

The idea isto use “greedy thinking” find the maximum or minimum .

## Sliding window

use start and end position to track the “window” start and end position while looping array, need increase the start when match some condition ,usually need to track the window size also

## Two pointers

maintain 2 index while looping array or string

## Back Tracking

like DFS ,just that need loop through each possibilities for each recursion .This approach only useful for small amount of input values .

## Devide and conquer

The idea is to find a partitioner and devide the issue into smaller ones (e.g. left and right), find the answer for each of them and “merge” the sub-answers . e.g. quick-sort

## Union Find

“Group” the parents to the same “root-parent” while finding parent for child node

## Dynamic programming

There is an array to store “previous answers” . it may not be easy to come out with this approach in the first time . but when we solved some issue with DFS or back tracking ,then we may find “some issue solved there should be a cache for these previous answers” . this is when DP come to be a solution.

Another pattern is “bottom-up” . try solve the problem with small amount of numbers, see if the answer, the trying to reuse previous answer to solve the problem with more numbers added in.

## Topo sort

while traverse graph remove the “out degree=0”

## Bit manipulation

use bit operation to solve the issue . e.g. bit mask

# How to leet the issues?

## By Tag

If not sure where to start ,on leetcode there are many tags (or started from the list above). clicking in each of them can focus only those issues .

so that you “reduced” the difficulty level by knowing where should find the answer (e.g. if choose DFS, you already know this issue should be solved by DFS approach)

## Skip the low rating issues first

Those issues with low rating usually because poor description or not related to any algorithm or not a programming issue , e.g. pure math . can skip those issues first if you are not interested .

## If you are preparing interview, focus on medium problems first

I have been interviewed with facebook , amazon, google ,microsoft ,indeed . all of them giving me some medium level leetcode problems , like topK (find 5th larges number among millions) , read4, longest palindrome str, binary search , graph traverse, Trie, string permutation, etc . you do not have to solve all the problems ,but make sure have a good understanding on the problems under the common tags .

## A timeline

Give yourself time limit while solving issues (e.g. 20 mins) . this will put on a bit stress on yourself to come out with answer within given time, not unlimited .

## Discuss

If you stuck , no worry , there are hundreds or thousands of good answers to each of the issue in “discuss tab” .

That’s all . Happy leet coding !



---

# 900个leetcode问题后这里是我学到的

# 我们为什么要练习算法？ （不只是面试）

解决算法问题可以帮助训练我们的思维习惯于有用的数据结构和算法，准备好使用它们来解决问题。

与 CRUD 不同的是，leetcode 问题通常需要对树、图、堆等数据结构有很好的理解，尽管它们中的大多数不太可能在日常工作中使用，但是当需求进入时（路径查找、短路路径（基于权重）、图/树遍历、引用计数等）如果我们能够快速提出解决方案，然后比较优缺点以获得最佳解决方案，这将很有帮助。

所以我们需要练习。

# 问题标签

以下是常见的标签（根据我的经验，按照面试中被问到的问题数量排序）。 leetcode 上列出了更多内容

## DFS

一种经典的遍历树或图的方式，从节点开始直到结束。或在一个矩阵中通过上、下、左、右查找区域。通常每棵树或图或 0-1 矩阵搜索问题都可能与 DFS 相关。

## BFS

与 DFS 不同。 BFS 可以通过队列完成。找到下一个可达节点，添加到队列中直到队列为空。与 DFS 不同，BFS 更关注当前“父母”队列中的所有“下一个节点”。

## 二分查找

在有序数组中找到一些值，通常属于这一类。

## 堆

当发现 max 或 min 或 topK 问题时。通常这种结构有你选择的语言，可以直接使用。 java、c#、python 等，如果没有，你也可以构建你的堆。

## 试一试

给定字典，构建 Trie，进行单词搜索或频率。

### 堆

在循环遍历某个数组时，使用最小或最大堆栈来比较堆栈中的顶部 1。

## 链接列表

常见问题如查找循环、两个链接列表之间的公共节点、反向等。

### 贪婪的

这个想法是使用“贪婪思维”找到最大值或最小值。

### 滑动窗口

在循环数组时使用开始和结束位置来跟踪“窗口”的开始和结束位置，当匹配某些条件时需要增加开始，通常还需要跟踪窗口大小

## 两个指针

在循环数组或字符串时保持 2 个索引

## 回溯

就像 DFS 一样，只是需要循环遍历每个递归的每个可能性。这种方法仅对少量输入值有用。

## 分而治之

这个想法是找到一个分区器并将问题分成更小的问题（例如左和右），找到每个问题的答案并“合并”子答案。例如快速排序

## 联合查找

在为子节点寻找父节点时将父节点“分组”到同一个“根父节点”

## 动态规划

有一个数组来存储“以前的答案”。第一次提出这种方法可能并不容易。但是当我们用 DFS 或回溯解决一些问题时，我们可能会发现“解决了一些问题，应该缓存这些以前的答案”。这是DP成为解决方案的时候。

另一种模式是“自下而上”。尝试用少量数字解决问题，看看是否有答案，尝试重复使用以前的答案来解决添加更多数字的问题。

## 拓扑排序

而遍历图删除“out degree=0”

## 位操作

使用位操作解决问题。例如位掩码

# 如何解决问题？

## 按标签

如果不确定从哪里开始，在 leetcode 上有很多标签（或从上面的列表开始）。点击它们中的每一个都可以只关注那些问题。

这样您就可以通过知道应该在哪里找到答案来“降低”难度级别（例如，如果选择 DFS，您已经知道这个问题应该通过 DFS 方法解决）

## 先跳过评分低的问题

那些评分低的问题通常是因为描述不佳或与任何算法无关或不是编程问题，例如纯数学。如果您不感兴趣，可以先跳过这些问题。

## 如果你正在准备面试，首先关注中等问题

我确实接受过 Facebook、亚马逊、谷歌、微软的采访。他们都给了我一些中等级别的 leetcode 问题，比如 topK（在数百万中找到第 5 个大数）、read4、最长回文 str、二分查找、图遍历、Trie、字符串排列等。您不必解决所有问题，但请确保对常见标签下的问题有一个很好的理解。

## 时间线

在解决问题时给自己时间限制（例如 20 分钟）。这会给自己带来一点压力，在给定的时间内给出答案，而不是无限的。

## 讨论

如果你卡住了，不用担心，“讨论”标签中的每个问题都有成百上千的好答案。

就这样 。快乐的leet编码！





- https://iorilan.medium.com/after-900-leetcode-problems-here-is-what-i-learned-4d39b17e0853



---

# ---------------------------------------------------------



# How to identify which Data Structure to use.

So I have seen a lot of videos giving you the road map of How to learn Data Structure, 3-month plan, 5-month plan, etc. But I guess we forget the most important part that how can a student identify what to use and how to identify which Data Structure will be the best.

![img](https://miro.medium.com/max/1400/1*6FkPc_FyPLjP8WXSJOQTlQ.png)

This image is taken from Interview Bit & is basically a classification of Data Structures

I am here to help you but there are some prerequisites before you continue reading. So you should know what Data Structures are and have a fair amount of knowledge of all the data structures like Arrays, Strings, LinkedList, Trees, Graphs, Heap, and obviously Dynamic Programing.

👾**Tip 1:**

> ***If the array is sorted or there are 2 pointers.\***

If the array is sorted then the binary search should be the first thing that should come to your mind and there is a high probability that you will get to a solution with that.

Another thing that can help you with the sorted array is a two-pointer keep an i at the start and j at the ending and try if you can solve it by moving those.

👾**Tip 2:**

> **If you are given a linked list**

Linked List is my favourite Data Structure and after solving 100+ questions on Linked List I have realized that the two-pointer (in which you have a slow pointer that moves one step and a fast pointer that moves two steps) method solves the problem.

👾**Tip 3:**

> **If asked for top/least K items**

If you see K you should immediately think of a Heap. If it is a direct question then you can easily figure it out but sometimes the question is in a form of a story like top 3 from 10 then heap can be helpful.

👾**Tip 4:**

> **Tree or Graph Question**

This was one of the scariest topics for me but I figured that learning BFS & DFS will solve most of the questions. I talked to so many friends who gave interviews in companies like Amazon, Microsoft, BNY Mellon, etc most of them told me that graph question in an interview was easily solved just by using one of the two approaches.

👾**Tip 5:**

> **If you have been given frequency/ duplicates**

In these cases, hashmaps come handy because you can store key-value pairs at better complexity as compared to storing in an array.

👾**Tip 6:**

> **If asked for maximum/ minimum subarray/ subset**

In such cases, Dynamic Programming comes handy

👾**Tip 7:**

> **If permutations or subsets**

Recursion or BackTracking can be helpful in such cases

While the above tips can help you solve 90% of the questions still there is no set method through which you can identify what DS we have to use. There are questions where Binary Search is used even in an unsorted array so it’s about building your logic and choosing the right data structure that solves your question in the least time complexity.





---

# 如何识别要使用的数据结构。

所以我看过很多视频给你如何学习数据结构的路线图，3个月计划，5个月计划等。但我想我们忘记了最重要的部分，学生如何确定使用什么以及如何确定哪种数据结构是最好的。

![img](https://tva1.sinaimg.cn/large/e6c9d24egy1h5yac079d0j20s20es0te.jpg)

此图取自 Interview Bit & 基本上是数据结构的分类

我在这里为您提供帮助，但在您继续阅读之前有一些先决条件。所以你应该知道什么是数据结构，并且对所有的数据结构有相当多的知识，比如数组、字符串、链表、树、图、堆，当然还有动态编程。

👾**提示1：**

> ***如果数组已排序或有 2 个指针。\***

如果数组已排序，那么二进制搜索应该是您首先想到的事情，并且您很有可能会找到解决方案。

另一件可以帮助您处理排序数组的事情是一个两指针，将 i 放在开头，j 放在结尾，并尝试通过移动它们来解决它。

👾**提示2：**

> **如果给你一个链表**

链表是我最喜欢的数据结构，在解决了链表上的 100 多个问题后，我意识到两指针（其中你有一个移动一步的慢指针和一个移动两步的快速指针）方法解决了这个问题。

👾**提示3：**

> **如果询问前/最少 K 个项目**

如果您看到 K，您应该立即想到堆。如果这是一个直接的问题，那么您可以很容易地弄清楚，但有时问题是以故事的形式出现，例如 10 中的前 3 名，那么堆可能会有所帮助。

👾**提示4：**

> **树或图问题**

这对我来说是最可怕的话题之一，但我认为学习 BFS 和 DFS 将解决大部分问题。我和很多在亚马逊、微软、纽约梅隆银行等公司接受采访的朋友交谈过，他们中的大多数人告诉我，面试中的图形问题只需使用这两种方法中的一种就可以轻松解决。

👾**提示5：**

> **如果您被给予频率/重复**

在这些情况下，hashmap 会派上用场，因为与存储在数组中相比，您可以以更复杂的方式存储键值对。

👾**提示6：**

> **如果要求最大/最小子数组/子集**

在这种情况下，动态编程就派上用场了

👾**提示7：**

> **如果排列或子集**

在这种情况下，递归或回溯可能会有所帮助

虽然上述提示可以帮助您解决 90% 的问题，但仍然没有固定的方法可以让您确定我们必须使用什么 DS。有些问题甚至在未排序的数组中也使用了二分搜索，因此它是关于构建您的逻辑并选择正确的数据结构，以最小的时间复杂度解决您的问题。





- https://medium.com/codechef-vit/how-to-identify-which-data-structure-to-use-5a1c66ad2742