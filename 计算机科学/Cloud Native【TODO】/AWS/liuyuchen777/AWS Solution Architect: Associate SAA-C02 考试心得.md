[TOC]

### AWS Solution Architect: Associate SAA-C02 考试心得

- https://zhuanlan.zhihu.com/p/415896345

## 写在前面

首先分享自己记的SAA-C02备考笔记以及各种备考资源，刷题的时候可以做以参考：

[https://liuyuchen777.github.io/2021/10/04/AWS-Study-Note/AWS-SAA-C02/liuyuchen777.github.io/2021/10/04/AWS-Study-Note/AWS-SAA-C02/![img](https://pic3.zhimg.com/v2-3414694e57e9b89dcafd29f9958a6876_ipico.jpg)](https://link.zhihu.com/?target=https%3A//liuyuchen777.github.io/2021/10/04/AWS-Study-Note/AWS-SAA-C02/)

------

我去年写过一篇关于AWS的入门资格AWS Cloud Practitioner的考试心得，时隔快一年，我又抽出时间考了这个AWS认证中可能是最多人考的资格——AWS Solution Architect: Associate（统称SAA）。SAA非常像云从业者的超集，考过SAA，云从业者资格也失去了意义了。

[VR战士Splatoon：AWS Cloud Practitioner 云从业者 考试心得32 赞同 · 12 评论文章](https://zhuanlan.zhihu.com/p/295326042)

关于这两个考试的区别， 我个人认为有过任何开发经验的同学最好都直接报SAA的考试，真的不难。而对于云计算，以及前端后端数据库API等概念都不清楚的同学，云从业者对你会是一个非常好的开始。

另外我觉得云从业者也在日本想找IT相关工作的文科生同学的很好的一个证明，你简历上写这个证找IT相关工作的时候，人事至少会觉得你很认真地在考虑这件事情。

通过任何一门AWS考试，都会送给你下一次注册考试的半价优惠券。云从业是是100美金，其他所有考试都是150美金，如果你实在没信心先考云从业者再考SAA的话，花的钱上来说是差不太多的。唯一的考量就是时间成本了。

------

## 关于AWS认证

![img](https://pic1.zhimg.com/80/v2-2212e39a7b701c37c92caa6fc85049e0_1440w.jpg)AWS认证体系

AWS的认证体系如上，在入门的云从业者之后，是三个不同的发展方向：架构师、系统管理员和开发者。不同的类别侧重点也不一样，developer更多的是如何对一些具体的服务（EC2、DynamoDB等）的配置，CLI的内容也会占很大的比重；operations则是侧重安全配置（Security Group、IAM等）以及系统配置等；architect的题目则是给定一个客户需求，选择那些服务的组合去满足客户的需求。

就我个人的观点来说，architect学到的应该是最通用的技能，不论是对于开发者、DevOps以及PM，都是价值的基础。实际上architect也是考的人最多的。developer的很多知识过于琐碎，日常真的开发的时候也是直接搜文档。operations则没有具体了解过。

其他speciality的资格就按照自己需求去考就好了。

FreeCodeCamp的SAA教程里还有一张这样的图以供参考：

![img](https://pic1.zhimg.com/80/v2-4001c002832185758121015d2110fb3c_1440w.jpg)YouTube@FreeCodeCamp

## Overview

SAA主要包含以下4方面的内容，考试大纲和分数占比如下：

```text
Domain 1: Design Resilient Architectures 30%
1.1 Design a multi-tier architecture solution
1.2 Design highly available and/or fault-tolerant architectures
1.3 Design decoupling mechanisms using AWS services
1.4 Choose appropriate resilient storage 
Domain 2: Design High-Performing Architectures 28%
2.1 Identify elastic and scalable compute solutions for a workload
2.2 Select high-performing and scalable storage solutions for a workload
2.3 Select high-performing networking solutions for a workload
2.4 Choose high-performing database solutions for a workload
Domain 3: Design Secure Applications and Architectures 24%
3.1 Design secure access to AWS resources
3.2 Design secure application tiers
3.3 Select appropriate data security options
Domain 4: Design Cost-Optimized Architectures 18%
4.1 Identify cost-effective storage solutions
4.2 Identify cost-effective compute and database services
4.3 Design cost-optimized network architectures
```

考试时间是130分钟，总共65道题。其中50道题是计分题目，15道题是新题目，混在考试中为了判断难度是否合适，不计入分数。当然你也不知道那些题目是计分题目，所以对你来说也就是65道题，每题2分钟了。

总分1000分，720分即可通过。一道题200分，50道计分题，36道通过

通过AWS认证的考试合作伙伴可以预约在家考试以及到考试中心进行考试，考试中心在全国主要城市都有，甚至在西安都有超过5家，遍布全城。

机考现场出分，点了提交后立马能知道结果，非常令人愉悦。

考试形式全部是选择题，包含单选和多个选项选两个。个人经验来说，单选是占绝大多数的（大概9：1）。

全部以选择题为形式这一点来说，难度上确实难不到哪里去了。并且很多题非常套路，后面我会详细说。对于我这种一路国内应试教育上来的小镇做题家来说，不成难度。

SAA去年十月之前是可以选择报C02或者是C01的，C01是老一版的考试。现在的时间点就只有C02可以报了。顺便附上C01的大纲，很有意思可以对照，看一看对于AWS官方的视点来说哪些部分更重要了。

```text
design resilient architectures 34%
specify secure applications and architectures 26%
design cost-optimized architectures 10%
define operationally-excellent architectures 6%
```

备考时间来说的话，有以下建议：

```text
Developer 1 month

No Experience 1-2 months

Cloud Engineer 20 hours
```

对于0经验的人来说：第一个月过一过教程，自己上手做一些实验；第二个月自己做做题，知识点查漏补缺。

对于工作中常用AWS的工程师来说，确实20个小时做做题看看白皮书就完全够了。

## 备考资源

我备考的全部资源都整理在下面的链接：

[[AWS SAA-C02 Study Note\] Introliuyuchen777.github.io/2021/10/04/AWS-Study-Note/AWS-SAA-C02/![img](https://pic3.zhimg.com/v2-3414694e57e9b89dcafd29f9958a6876_ipico.jpg)](https://link.zhihu.com/?target=https%3A//liuyuchen777.github.io/2021/10/04/AWS-Study-Note/AWS-SAA-C02/)

再做一个资源上的补充，推荐鸿鹄论坛：

[鸿鹄论坛-CCNA,HCIA,思科华为,CCNA题库,CCNA考试,网络工程师论坛,CCIE培训,思科认证,华为认证,思科论坛 - 思科华为论坛bbs.hh010.com/](https://link.zhihu.com/?target=https%3A//bbs.hh010.com/)

因为我在国外，访问鸿鹄慢得令人发指，这次备考没怎么用。如果在国内的话还是可以有效利用一下的。

## 心得

我去年刚考云从业者的时候，大部分知识点也是死记硬背，以及看着例题发挥自己的做题技巧。今年这一年自己也在学一些开发流程，在企业也是实习过之后，再考SAA的时候更多的时候是看看白皮书规范自己开发的范式，轻松了许多。

同时我也建议，专注于过程而不是结果。边看白皮书或者视频教程，边跟着在AWS Console做做实验，心情好了给个人博客买个域名，放到S3上托管，这才是更好的学习方式。题做得不错，拿到了资格，实操上什么也不会也是没有价值的。

> 你的目标应该是提高技能，而不是取得某种结果。 Heidi Grant Halvorson博士做了一项研究，她让两组人解决各种问题。第一组被告知得分尽可能高。第二组被告知将问题视为学习机会。结果令人惊讶。第一组渐渐感到沮丧，而第二组坚持并解决了更多问题。通过专注于掌握知识，你会将遇到的困难和时间上紧迫视为有助于你成长的要素。反之，以结果为导向的心态，会把遇到的问题视为阻碍，并渐渐让你感到气馁。更重要的是，如果你专注于掌握，你会看到持续的进步。每次当你阅读一个新段落或解决一个新问题时，你都在提高自己的技能。这种持续不断的满足令人难以置信。 因此，下次你在上课或练习面试时，请专注于变得更好，而不是通过考试或获得录取通知书。

另外，单纯以题目来说，充满着套路：

1. 提到CPU时常占用率过高，客户希望保证性能和cost，就一套ALB/NLB+ASG就可以了。
2. 提到S3或者DynamoDB的访问希望从VPC内部走，直接Gateway Endpoint就行了。
3. 提到服务全球客户就CloudFront。
4. 提到不希望管理自己的计算单元，就往serverless的那几个上靠就行了。
5. etc

由于我在日本，我个人觉得这个证还是蛮有用的。因为日本新卒（应届生）找工作，投ES（简历）非常玄学，再好的学校简历都用可能给你刷了。但你有这个证，人事一般会抱着这个小伙子有点意思，进面试先聊聊吧的心情，给你保过去简历关。还挺好的。

拿到Badge之后，建议发到LinkedIn的[AWS Certified Global Community](https://link.zhihu.com/?target=https%3A//www.linkedin.com/groups/6814264/)。我收获了领英点赞最多的一条动态。

![img](https://pic3.zhimg.com/80/v2-d8d6a7a24d261a94c25d821411f25c62_1440w.jpg)

成年之后能如此廉价获取成就感的方式确实不多，诸位有兴趣可以花些看电影刷题打游戏的时间去试一试。