[TOC]

# AWS认证备考心得



因为工作需要，今年年初备考AWS Solution Architecture Associate的认证考试。

AWS Certified Solutions Architect 系列认证是亚马逊从2013年开始推出的对云计算架构师能力的官方认证。考试不仅考察应试者对AWS相关云服务的熟悉程度，题目也多来源于实际中要解决的问题，要求应试者有全面的架构设计经验和积累，所以含金量很高。

备考时间其实挺紧的，前后也就十天左右，而且工作也不能耽误，属于突击备考。

备考心得如下：

\0. 最好对云计算的架构有基本认识，有软件设计架构经验最佳，比如经常画拓扑图，熟悉各种类型数据库，熟悉文件存储类型等。这直接决定看资料的过程是事半功倍还是事倍功半。

\1. 首先先强调一下真题宝典之类的是不存在的，这也是其含金量高的原因。

\2. 网上比较全的攻略：[AWS认证攻略 – E哥的AWS Solution Architecture Associate 认证攻略](https://link.jianshu.com?t=http%3A%2F%2Fblog.csdn.net%2Frobertlee32%2Farticle%2Fdetails%2F69949159)

\3. 考试注册：https://aws.amazon.com/cn/certification/certified-solutions-architect-associate/

注册邮箱最好用公司邮箱，但是profile中的First Name与Last Name最好与自己的护照名字一致，或者就是自己姓名的汉语拼音，因为考试前是需要确认身份证的。

考试地点：深圳的考点位于罗湖区深房广场1903室，是各种专业认证考试机构，里面有一个小房间，大概四台电脑，一堆摄像头。

值得注意的是，AWS认证考试是连接美国那边服务器的，考试之前最好打电话确认一下当天AWS认证考试的网络情况如何，我去考试那天，网络断了四次，不过每次恢复网络之后，做题进度是联网保存的，考试时间也不会有影响。以至于做题速度非常快，而且没有检查就提交了。（几乎每题提交答案后，都在祈祷不要断网）

为了应对这种情况，要求心理素质过硬，备考充分，哈哈~~~

如果做完之后，确认submit之后，是马上就出结果的，也会有相关邮件发送给你的注册邮箱。如果快的话，一天之后应该可以在认证注册站点找到通过的相关AWS证书，我的证书如下：

![img](https:////upload-images.jianshu.io/upload_images/6936356-043b4cf805162643.png?imageMogr2/auto-orient/strip|imageView2/2/w/1059/format/webp)

\4. 如果是Morningstar员工，可以在Wiki获得AWS备考资料：[Prepare for AWS Certification](https://link.jianshu.com?t=https%3A%2F%2Fmswiki.morningstar.com%2Fdisplay%2FSZTECH%2FPrepare%2Bfor%2BAWS%2BCertification)

\5. 备考书籍

在这些备考资料里面，我重点看的是官方备考书籍：

AWS Certified Solutions Architect Official Study Guide: Associate Exam

可以在亚马逊购买，亚马逊中国站点目前是缺货的，这也说明在中国考这个的不多

![img](https:////upload-images.jianshu.io/upload_images/6936356-b1124afb93c332af.png?imageMogr2/auto-orient/strip|imageView2/2/w/940/format/webp)

官方备考书籍

如果有海购经验，也可以直接在亚马逊美国站点购买，不考虑关税的话，便宜不少：

![img](https:////upload-images.jianshu.io/upload_images/6936356-15fff84cd77647f1.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

官方备考书籍

根据备考经验，这本书应该是必看的，考试的内容应该大都包含了，内容编排很合理，如每章都有详细的介绍，然后有考试需要注意的重点项目，还有相关内容的仿真题型。

如果能把这本书认真看一遍，并且每章的题目能达到90%的准确率，通过可能性就很大了。

不过有一些考点在这本书里并没有，或者很少提及，比如：Lambda，ECS，Elastic Beanstalk，CloudFormation，API Gateway，我考试的题目里面，有几题的知识点与它们相关，所以还是应该看一下相关的内容，最起码知道它们是做什么的。

\6. AWS站点资料

各种Guide文档，因为随便一个都几百页，反正我是没时间去看的……

我主要是看两部分内容，这两块的内容应该包括考试大部分内容了（不过确实也需要看好几天的。。。）

a. 白皮书

https://aws.amazon.com/whitepapers/

b. FAQs

https://aws.amazon.com/faqs/

\7. Youtube的学习视频

[https://www.youtube.com/playlist?list=PLEiEAq2VkUULlNtIFhEQHo8gacvme35rz](https://link.jianshu.com?t=https%3A%2F%2Fwww.youtube.com%2Fplaylist%3Flist%3DPLEiEAq2VkUULlNtIFhEQHo8gacvme35rz)

需要在能访问Youtube的环境看。

\8. 学会看图说话

如果一头扎进浩如烟海的备考资料，应该会很晕，甚至无所适从，看图说话其实会容易上手一些。所谓一图胜千言。

AWS资料中，有很多拓扑图或架构图，先把架构相关的知识点理解，然后再分别学习构成相关架构的各个元素，如学会读懂下图，掌握VPC, AZone, subnet, load balancer, auto scaling



![img](https://tva1.sinaimg.cn/large/008i3skNly1gw3ai2835lj30ke0hvabc.jpg)

VPC

\9. 多做模拟题

免费的模拟题站点：[https://www.briefmenow.org/amazon/](https://link.jianshu.com?t=https%3A%2F%2Fwww.briefmenow.org%2Famazon%2F)

选择AWS SAA相关的题目，题目很多，一共是500多道题，我在考试过程中，发现有5题是来自这里的。虽然比例只有10%左右，但还是有必要过一遍的，关键是熟悉题型。

需要注意的是，有很多题目的答案是错的，还好这是论坛性质的，可以看题目下面的讨论。

如果有些题目的题干特别长，看了就蒙圈，应该是有人把professional相关的模拟题也搬过来了，可以忽略。

做题其实不是目的，关键是熟悉题型，了解相关的知识点，如果做错了，那么就把相关的知识点再好好巩固一下。

\10. 预考

如果这些题目在考试前能够达到90%的准确率，说明基本掌握了备考的知识点了，如果不放心可以在考试前在AWS认证网站花费20美元预考，大概20个题目，65%及格。强烈建议考前尝试一下，我是在考前一周做的预考题，得分是70%，这坚定了我之后正式花费150美元报考的决心。事实上，我的最终得分是75%，与预考得分基本一致。由此可见预考与真实考试的难度系数是非常接近的。

\11. 需要数字敏感

AWS产品介绍，有大量数字，比如最大值，最小值，默认值，建议把S3, SQS, SNS等相关知识点出现的数字记一下。

我考试的过程中，出现了EC2费用的计算题，好像是说刚开始的费用是x, 使用之后费用调整为y，但是我们的折扣费是z，问使用的小时费用是多少。。。

\12. 正式考试前一天的准备

我是把在免费模拟题做错的题大致过一遍，加强相关知识点的理解与记忆。同时再快速把那本官方备考书过一遍。然后就踏上考试的行程。

\13. 实战考试注意点

提前半小时到考点，深圳的考点位于罗湖区深房广场1903室，考试时间是上午9点30或者下午13:30分，最好下午考试，这样还有半天复习时间，而且可以避开上午早高峰的尴尬。

考点有个黑漆漆的区域，有几个沙发，前台会安排考生在那里闭目养神，注意：别真睡着了

考场工作人员在13:15就会询问考生是否现在就考，如果准备好的话，先去趟洗手间，然后就是去小房间考试了。

考试题目没有概念题，都是应用题（ Scenario），大都是描述遇到什么样的情况，应该如何解决。注意关键字，比如实时大批session处理，或许就是与kinesis有关，但是session storage或许就与DynamoDB有关；如果涉及analyze logs，或许就与RedShift相关。

网络，安全相关的题目，占分至少40%，直接关系能不能过，所以VPC, subnet, 各种类型的gateway，load balancer, auto scaling, Availability Zone等知识点必须完全掌握。

每题的题干大致30~50个单词量，先一眼扫过去，把题目涉及的知识点过一下，然后瞟一眼选项，然后再仔细看题，抓住关键词。注意或许比较tricky的描述，不要轻易踩坑。

多选题大概有十几道，不过题干大都会标注需要选几项，多选题重点是排除法。比如5个选项选择2个，能排除3个就搞定了。

\14. 总结

总结就是没有捷径，上述的各个内容最好都认真看一遍，如果备考时间是一个月，每天备考时间大概晚上8点到10点，先半个月左右，即30个学时，把官方备考书先看完。然后每天4个小时，大概20个学时，把官方白皮书，FAQs过一遍，然后每天4个小时去那个免费题库网站做题。考试前三天，建议除了工作就是学习，集中突击，把弱点部分巩固一下。

我的备考时间，其实就一周，或许与软件从业经验十几年有关，看资料速度有一定保障。（就此打住，否则装B要被打，哈哈哈）

最后，祝好运。



73人点赞



[随笔]()





作者：blade_he
链接：https://www.jianshu.com/p/98e4bcdaea3b
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



- https://www.jianshu.com/p/98e4bcdaea3b
- https://www.briefmenow.org/amazon/