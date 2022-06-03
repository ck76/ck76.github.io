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

https://medium.com/codechef-vit/how-to-identify-which-data-structure-to-use-5a1c66ad2742