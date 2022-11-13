

[TOC]

- https://zhuanlan.zhihu.com/p/352842845

### word2vec 与 bert 作为NLP领域两个里程碑的工作，他们之间的联系与区别是怎样的？对二者的对比，有助于更好的理解二者的原理

### 模型原理与架构

word2vec

word2vec的一个基本假设在于，可以通过word的周边词实现对word的理解表征。

相应的模型结构和训练方式包括两种，skip（中间词预测周边词）和cbow（周边词预测中间词）



![img](https://pic1.zhimg.com/80/v2-1e9425d5877edf787e55fbfb54d46024_1440w.webp)

CBOW

![img](https://pic2.zhimg.com/80/v2-819d291ef96b8b880e43cd05df4798d5_1440w.webp)

SKIP



bert

bert是利用transformer的结构，同时考虑word所在句子（序列）中左右两边词的信息，实现对词的表征。

关于bert的基本原理，可以参考 [phynlp：bert 面试 知识点](https://zhuanlan.zhihu.com/p/474260774)



![img](https://pic1.zhimg.com/80/v2-33b250d79c38d488e85b3d47a2fc99cc_1440w.webp)

Bert

### Bert 与 word2vec 的区别

如上所述，可以看成 word2vec 和 bert 都考虑了 word 的周边信息，实现对word的表征，获得word的embedding。虽然二者都利用了word的周边信息，但是二者使用周边信息的方式不同，模型架构和训练方式不同，造成了二者对word的表征效果不同，具体的差别如下。

### BOW(Bag of words) vs Bidirectional

对于word2vec，无论是周边词预测中心词，还是中心词预测周边词的预测方式，周边词的方式都可以看成是Bog of words的模式。这种模式有两个问题：

- 没有考虑词序问题
- 受限于window窗口大小的限制，不能考虑整个句子中所有词的相关性

对于Bert，利用transformer中的注意力方法和mask language model的训练，以及embedding中编码了词序信息。通过以上的方式，实现了同时考虑上下文(Bidirectional)及词序信息，实现对word的embedding。



### static vs context

word2vec给出的向量，是模型训练好以后，给出的embedding table。对于一个word而言，通过embedding table得出word对应的vector，即实现了word to vector。这样的vector是一种静态 static 的vector，与word所在的句子无关。例如“I want to access my **bank** account”，和“we went to the river **bank**”，对于这两个句子中，word2vec给出的bank的向量是固定的。

对于bert，词的向量是依赖于整个句子的，是输入句子后得出的词的向量，不是静态固定不定的向量，是根据句子的context得出的向量。



### 句子层面的信息利用

Bert的预训练采用了多任务训练，包括mask language model和next sentence prediction两种任务。Bert通过next sentence prediction的任务，利用了句子粒度的信息，可以实现词语的信息的更好表征。

这里通过一个例子给大家做一个解释，相信能够给大家更直观的理解。

无论是Bert还是word2vec，对词语的表征都利用了周围词的信息。这样就会带来一个问题，对于周围词相同，但是语义相反的词，得出的embedding表征信息大概率就会非常相似。如“你今天来的太早了”、“你今天来的太晚了”。这两个句子中的“早”和“晚”，由于周围的词完全相同，在word2vec中，得出的向量表征就会非常相似。但是在Bert的预训练中，同时训练了next sentence prediction，“你今天来的太早了。不到8点就到公司了”，“你今天来的太晚了。都11点才到公司”，“早”和“晚”的信息表征，利用下一句的信息，相比word2vec而言，就能更准确的表征出“早”和“晚”的差别。

### bert 的embedding table 与word2vec中的embedding table

bert主要利用transformer结构，实现基于上下文的word表征，bert预训练得出的embedding table，同样可以作为一个静态的向量表征方法，可以看做是基于大量语料训练后学习到词的共性（平均）表征。

相比于word2vec训练得到的embedding table，没有利用周围词的信息（bert中周围词的信息是通过transformer中的注意力结构实现编码的，embedding table中的embedding信息没有利用周围字符的信息。），因此就embedding table的效果而言可能不如后者。







### ref:

[https://medium.com/swlh/differences-between-word2vec-and-bert-c08a3326b5d1](https://link.zhihu.com/?target=https%3A//medium.com/swlh/differences-between-word2vec-and-bert-c08a3326b5d1)

BERT: Pre-training of Deep Bidirectional Transformers for Language Understanding

[http://www.blindfiveyearold.com/algorithm-analysis-in-the-age-of-embeddings](https://link.zhihu.com/?target=http%3A//www.blindfiveyearold.com/algorithm-analysis-in-the-age-of-embeddings)

[http://ai.stanford.edu/blog/contextual/ai.stanford.edu/blog/contextual/![img](https://pic1.zhimg.com/v2-3dd696fedfdf7c1509ae0d4d2a82b354_180x120.jpg)](https://link.zhihu.com/?target=http%3A//ai.stanford.edu/blog/contextual/)

[穆文：[NLP\] 秒懂词向量Word2vec的本质6289 赞同 · 227 评论文章![img](https://pic4.zhimg.com/v2-a1a73c063b32036429fbd8f1ef59034b_180x120.jpg)](https://zhuanlan.zhihu.com/p/26306795)