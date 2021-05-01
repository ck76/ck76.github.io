

```
作者：渡口的小太阳
链接：https://www.nowcoder.com/discuss/525807?type=2&order=0&pos=1&page=1&channel=-1&source_id=discuss_tag_nctrack
来源：牛客网

1.Activity的生命周期，A启动B会走什么生命周期
2.Handler的实现
3.map的实现原理
4.输入网址后，DNS寻址过程
5.归并排序相关，时间复杂度，空间复杂度，是否稳定
6.是否了解Android各大版本的区别
7.Android 打包过程经历了什么
8.Android 混淆需要注意什么
9.view的绘制

作者：渡口的小太阳
链接：https://www.nowcoder.com/discuss/525807?type=2&order=0&pos=1&page=1&channel=-1&source_id=discuss_tag_nctrack
来源：牛客网

1.自我介绍和项目
2.ArrayList是怎么扩容的知道吗，不知道……
3.说下HashMap
4.线程相关，synchronized和Lock的区别
5.写一个单例，问volatile的 作用 和原理
6.wait 和sleep的区别，他们分别是哪个类的方法
7.Okhttp源码，evntbus源码，如何通过反射找到方法
8. 9.类加载机制，双亲委派模型
10.handler Looper相关（好像每个公司都问过……）
11.写个堆（没听清楚，以为把数组变成个普通的二叉树，写完他说这是啥，我尴尬得重写）

作者：渡口的小太阳
链接：https://www.nowcoder.com/discuss/525807?type=2&order=0&pos=1&page=1&channel=-1&source_id=discuss_tag_nctrack
来源：牛客网

3.给个题目写下（写不出来就不用聊了是吧）
在一个list中擅长一个目标值 如list = {1,2,4,2,5,2,2,6} 中删除2，返回删除后的list
使用迭代器，但是我尴尬的忘记迭代器的各种方法名了，在那里调了很久，没有代码提示很难受，最后他提醒我了
4.为什么要用迭代器，不能在遍历list时remove
5.之前实习过程中做得比较好的经历
6.重写和重载的区别
7.再写个算法 Leetcode34 在排序数组中查找元素的第一个和最后一个位置


作者：渡口的小太阳
链接：https://www.nowcoder.com/discuss/525807?type=2&order=0&pos=1&page=1&channel=-1&source_id=discuss_tag_nctrack
来源：牛客网

快手一面
笔试四道题 AC了三道。
2020.4.1 当初投快手是想着拿来练手的，结果现在才面试，头条和腾讯都hr面了，有点不想面试了，所以面的比较随意，不会的直接就说不会了……
1.自我介绍完先做三道算法题 讲思路就行
（1）一堆出现2次的数中找出现1次的数 异或操作
（2）两数之和等于目标数 返回两个数的下标 hashmap放值和下标
（3）第k小的数 大根堆，快排
2.多线程 sychronized volatile
3.单例有哪几种，写个双重校验锁的，然后问各种细节，为什么需要第二重校验，为什么使用volatile,为什么锁的是class
4.hashmap,concurrentmap 实现原理
5.handler消息机制的实现
6.内存优化了解多少
7.view的绘制流程


作者：渡口的小太阳
链接：https://www.nowcoder.com/discuss/525807?type=2&order=0&pos=1&page=1&channel=-1&source_id=discuss_tag_nctrack
来源：牛客网

快手二面 2020.4.1 一面之后一个小时左右二面，中间网络卡了好几次，不知道是谁网络不好
1.android基础，四大组件，生命周期
2.service的启动方式，区别
3.如何跨进程通信
4.fragment生命周期
5.eventbus源码
6.又是多线程 sychronized volatile Lock ，synchronized可以锁String.class吗，我没搞懂什么意思，锁它有什么意义……
7.view的事件分发
8.android各个大版本的区别有没有了解（没有）
9.http协议 版本区别，2.0多路复用怎么实现，请求头有什么，get,post区别
10.项目相关
11.再写两道题
(1)合并有序链表
(2)二叉树的序列化，先序和层序方式（感觉就是写先序遍历和层序遍历）

腾讯一面 （鞭尸第三次）3.23 这一轮感觉就是基础快问快答
1.acitvity的四种加载模式，和对应哪些场景使用
2.eventbus的原理
3.java的四种引用
4.JVM运行时区域包括哪些东西
5.堆和栈的区别
6.局部变量是在类加载哪个时期分配的
7.map的原理
8.了解哪些垃圾回收算法
9.handler原理，looper是怎么轮询的，为什么不会阻塞
10.算法：爬楼梯 一步或两步，问除了动态规划还有什么方法，我说应该还有数学方法，他让我推导一下（推导斐波那契数列的数学公式，这是要招大数学家？？？）我假装算了一下，然后跟他说不会

腾讯二面 3.24
这一面面到很不好，面完脑子一片混乱,问了几个基础问题后就是内存性能优化相关的连续发问，大概记得这些问题，每个问题还都往深里去挖，为什么要这样做，怎样才算是性能好
1.了解什么设计模式及其在Android上的应用
2.OOM什么时候会出现
3.使用约束布局的好处，层级嵌套最好不要超过多少层
4.对于卡顿的理解，怎样算卡
5.一个应用占用内存大小怎么算合格
6.如果有网络请求，等待多少秒合适，在弱网情况下呢
7.从内存资源等多个方面讲讲对性能优化的理解
8.算法题，输入一个int 数比如12345，输入对应的中文读法，比如一万一千三百四十五

腾讯三面 3.26
这一面是电话面，想让我转ios，所以没问技术问题，就是了解我平时是怎么学习的，怎么找学习资料的，然后还让我对比了自己手机三个比较小众应用，哪个实现比较复杂，为什么

腾讯四面 3.27
这一面还是ios那边的面我，就写了三道题
1.从一堆有正有负，有浮点数里，找到第一个出现的正整数(本来以为很简单，但是浮点数和整数的判断，卡了我一下)
2.一堆可能出现多次的字符串中，找到出现一次的字符串 使用set
3.LRU缓存的实现 Leetcode 146
腾讯最后也过了，但是要转ios , 考虑腾讯转正率不高，最后还是选了头条
```



> 在上一篇[实习随笔| 周记(五)](https://www.jianshu.com/p/fcdfb8234b66)提过因为实习忙碌和秋招提前批将尽而焦灼和苦恼，于是在各种思想斗争和考虑后，终于下定决心在项目新版本上线之际办理了离职，并开始全心全意投入秋招。
>  原以为此行必将艰难，也做好了持久战的准备，没曾想一路出奇的顺利。现在不打算再找了，终于可以好好填旧坑写总结了！

##### 在[2018Android暑期实习面试总结](https://www.jianshu.com/p/eb570935d586)曾介绍过个人情况、自学经历、校招流程和复习重点，这部分就不再赘述。那么本篇先以日历形式回顾秋招之路，方便各位参考某厂的处理进度；之后会简单进行美团实习总结，也算给“美团实习”板块文章画上句号；然后是总结归纳春秋招Android面试题库，时间原因后续再出个“有问有答”的系列；最后做个总结还有展望，开始新的征程~

- 秋招日历
- 实习总结
- 题库大全
- 一点感悟

------

1.**校招日历**

在美团实习的最后一周项目在提测，相比开发阶段着实轻松了不少，所以开始改简历、投简历和复习，复习内容主要是个人博客和春招总结的笔记。

不过各厂子简历处理速度令我始料不及，在紧接的下一周就被各种面试狂轰滥炸，这波操作来得快去的也快，最终通过BAT、vivo、爱奇艺和一点资讯的全部面试流程，自此整个秋招以面试0error完美收官。

![img](https:////upload-images.jianshu.io/upload_images/5494434-a116733833338df3.jpg?imageMogr2/auto-orient/strip|imageView2/2/w/1000/format/webp)

中途发生个乌龙，在牛客给头条hr发过简历，结果竟然误进了社招池，还发来了面试邀约......差点就去应聘Android高级工程师了emmm

以下就是从开始准备到等到offer整个秋招日历表：

| 日期       | 具体事宜                                        |
| :--------- | :---------------------------------------------- |
| 8.6(周一)  | 修改简历、实习转正口头offer                     |
| 8.7~8.9    | 完成简历投递、复习                              |
| 8.10(周五) | 离职办理                                        |
|            | 收到vivo的面试通知                              |
| 8.11(周六) | 网易笔试                                        |
| 8.13(周一) | 阿里一面：电话40min                             |
| 8.14(周二) | 收到百度、爱奇艺、头条的面试通知                |
| 8.15(周三) | 百度一面：视频35min                             |
|            | 阿里二面：电话20min                             |
|            | 收到一点资讯的面试通知                          |
|            | vivo一面：视频25min                             |
| 8.16(周四) | 百度现场面：二面60min、三面30min                |
|            | 收到网易的面试通知（放弃）                      |
|            | vivo hr面：视频20min                            |
| 8.17(周五) | 阿里三面：电话30min                             |
|            | 收到腾讯的面试通知                              |
| 8.18(周六) | 一点资讯现场面：一面40min、二面50min、三面20min |
|            | 腾讯一面：电话35min                             |
| 8.19(周日) | vivo线下交流会                                  |
| 8.20(周一) | 爱奇艺一面：现场70min                           |
|            | 一点资讯hr面：电话15min                         |
| 8.21(周二) | 收到百度面试通过邮件+文化测评                   |
| 8.22(周三) | 爱奇艺二面：视频30min                           |
|            | 收到vivo录用意向书                              |
| 8.23(周四) | 阿里hr面：视频40min                             |
| 8.28(周二) | 腾讯二面：电话20min                             |
| 8.29(周三) | 爱奇艺三面：视频30min                           |
|            | 收到快手的面试通知（放弃）                      |
| 8.30(周四) | 腾讯hr面：视频15min                             |
| 8.31(周五) | 一点资讯hr沟通意向                              |
| 9.1(周六)  | 收到阿里录用意向书                              |
| 9.7(周五)  | 爱奇艺hr沟通意向                                |
| 9.12(周三) | 百度hr沟通意向                                  |
|            | 收到腾讯录用意向书                              |

> 汇总：面试部门及全部流程
>
> - 杭州/阿里/数据技术及产品部：2轮技术面+交叉面+hr面
> - 深圳/腾讯/OMG/腾讯视频：2轮技术面+hr面
> - 北京/百度/百度APP研发部：3轮技术面+文化测评
> - 北京/爱奇艺：3轮技术面
> - 深圳/vivo：1轮技术面+hr面+线下交流会
> - 北京/一点资讯：3轮技术面+hr面

------

2.**实习总结**

两个月的实习生活转瞬即逝，也有幸得到部门老大的肯定和对留用的认可。本节对这段短暂但充实的实习生活做个简短的总结，算是给“实习随笔”系列文章做个收尾（想看面经的可跳过此节）。

> 实习单位：北京/美团/新零售事业群/ 闪购事业部/赋能业务组

a.**工作总结**

- 参与完整的[开发流程](https://www.jianshu.com/p/4b89d681c5a0)，从v1.0需求评审到上线v1.1，掌握多人协作必需的[Git](https://www.jianshu.com/p/e0867ac2a261)使用
- 通过Code  Review熟悉业务代码，并刻画页面的类图和活动图，强化对[MVP模式](https://www.jianshu.com/p/e0867ac2a261)的理解
- 协助推进项目的进展，如v1.0后期埋点、修改bug、APP打包和性能优化，使用Lint工具优化代码质量、美团Metrics工具监测Crash情况等，并产出相应分析和优化建议文档
- 开发和维护v1.1新增的接入商家会员模块功能，包括会员识别、用户授权和短信验证
- 熟悉美团各种基础框架库并应用到项目中，如网络Retrofit-mt、定位Locate、验证服务Yoda

![img](https:////upload-images.jianshu.io/upload_images/5494434-d946f77534112554.jpg?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)

**b.学习总结**

- 坚持写[周记](https://www.jianshu.com/p/e0867ac2a261)的习惯，总结每周学到的新知识，以及记录所见所闻所想
- 读完《深入理解Java虚拟机》重要章节，并做相应的[读书笔记](https://www.jianshu.com/p/cd93567ed868)
- 学习主流的网络框架[Retrofit](https://www.jianshu.com/p/89ce9bf53073)，并深入源码
- 巩固基础：Java、Android、JVM、操作系统、计算机网络、数据结构等

![img](https:////upload-images.jianshu.io/upload_images/5494434-5a57017271aaa680.jpg?imageMogr2/auto-orient/strip|imageView2/2/w/410/format/webp)

**c.收获和成长**

这是我第一次走出校园踏入社会，于我而言意义非凡，所以非常珍惜每个在美团的日子。

- 从丝毫不了解新零售的井底之蛙到意识到这是新发展、大趋势、刻不容缓，每个业务沟通会都给我格局放大、眼界放宽的成长；
- 通过实际参与一个大的完整的项目，感受到不同于学校那些小项目的流程更规范、技术更强大；
- 在美团，培训制度完善、wiki知识库无所不有、学习气氛浓厚、分享会比比皆是，这些都让我大饱眼福，技术也不断在进步；
- 当然也少不了各种生活福利，时不时就搞个小活动，楼下楼上好不热闹，吃喝玩乐，正应了美团的发展目标--"帮大家吃得更好，活得更好"。

总之在这里收获的不仅有知识和技术，还有见识和人情味，我超喜欢这里的~



![img](https:////upload-images.jianshu.io/upload_images/5494434-7e62c66d0ea07472.jpeg?imageMogr2/auto-orient/strip|imageView2/2/w/1200/format/webp)


**d.不足和反思**



和实习前对自己的期望相比，现已完成大部分的计划，也有部分未达到预期：

- 原来一直想看外卖里一些基础组件的源码，最后也只是泛泛深入Retrofit-mt，还有很多自己感兴趣的框架没看，看源码确实不容易但这是一位工程师的必会技能，优秀框架必然有其精彩绝伦之处，从"可以写"到"写得好"自己还需要走很长的路；
- 实际写项目也发现自己会有考虑不全的问题，性能优化的意识也不高，虽然理论知识足够，但毕竟是纸上谈兵，还需多实践形成良好的编程习惯；
- 之前给自己制定的写文计划基本落实，起码没有弃坑，不过仍有一些未总结，后续时间充裕定会补上。

------

3.**题库大全**

> - 之前说过我的复习范围无非是个人技术博客还有整理的笔记，考虑到笔记是手写版不利于保存，所以打算重新整理并放到网上，时间原因这里先列出面试问题，题解详见：
>   - [2019校招Android面试题解1.0（上篇）](https://www.jianshu.com/p/718aa3c1a70b)
>   - [2019校招Android面试题解1.0（中篇）](https://www.jianshu.com/p/2dd855aa1938)
>   - [2019校招Android面试题解1.0（下篇）](https://www.jianshu.com/p/168e52336b53)
>   - [2019校招Android面试题解1.0（算法篇）](https://www.jianshu.com/p/9648e8dd5bdb)
> - 当然，我认为看面经主要是为了查缺补漏，自己也要有一定的知识储备和学习体系，而不是临时抱佛脚、试图通过背题背答案方式应付面试，只有自己真学会的东西才能侃侃而谈，更有自信。
>    （注：部分重点有文字链接，表示有具体的文章讲解）

a.`Android`

- [Activity](https://www.jianshu.com/p/602b1ec4ca7a)

> Q：说下Activity的生命周期？
>  Q：onStart()和onResume()/onPause()和onStop()的区别？
>  Q：Activity A启动另一个Activity B会回调哪些方法？如果Activity B是完全透明呢？如果启动的是一个Dialog呢？
>  Q：谈谈onSaveInstanceState()方法？何时会调用？
>  Q：onSaveInstanceState()与onPause()的区别？
>  Q：如何避免配置改变时Activity重建？
>  Q：优先级低的Activity在内存不足被回收后怎样做可以恢复到销毁前状态？
>  Q：说下Activity的四种启动模式？（有时会出个实际问题来分析返回栈中Activity的情况）
>  Q：谈谈singleTop和singleTask的区别以及应用场景
>  Q：onNewIntent()调用时机？
>  Q：了解哪些Activity启动模式的标记位？
>  Q：如何启动其他应用的Activity？
>  Q：[Activity的启动过程？](https://www.jianshu.com/p/37f366064b98)

- [Fragment](https://www.jianshu.com/p/184f0c8857d6)

> Q：谈一谈Fragment的生命周期？
>  Q：Activity和Fragment的异同？
>  Q：Activity和Fragment的关系？
>  Q：何时会考虑使用Fragment？

- [Service](https://www.jianshu.com/p/1959eb5c99f5)

> Q：谈一谈Service的生命周期？
>  Q：Service的两种启动方式？区别在哪？
>  Q：一个Activty先start一个Service后，再bind时会回调什么方法？此时如何做才能回调Service的destory()方法？
>  Q：Service如何和Activity进行通信？
>  Q：用过哪些系统Service？
>  Q：是否能在Service进行耗时操作？如果非要可以怎么做？
>  Q：AlarmManager能实现定时的原理？
>  Q：前台服务是什么？和普通服务的不同？如何去开启一个前台服务？
>  Q：[是否了解ActivityManagerService，谈谈它发挥什么作用？](https://www.jianshu.com/p/37f366064b98)
>  Q：如何保证Service不被杀死？

- [Broadcast Receiver](https://www.jianshu.com/p/82ecfc95924f)

> Q：广播有几种形式？什么特点？
>  Q：广播的两种注册形式？区别在哪？

- [ContentProvider](https://www.jianshu.com/p/9048b47bb267)

> Q：ContentProvider了解多少？

- [数据存储](https://www.jianshu.com/p/8d6acf45e9b7)

> Q：Android中提供哪些数据持久存储的方法？
>  Q：Java中的I/O流读写怎么做？
>  Q：SharePreferences适用情形？使用中需要注意什么？
>  Q：了解SQLite中的事务处理吗？是如何做的？
>  Q：使用SQLite做批量操作有什么好的方法吗？
>  Q：如果现在要删除SQLite中表的一个字段如何做？
>  Q：使用SQLite时会有哪些优化操作?

- [IPC](https://www.jianshu.com/p/1c70d7306808)

> Q：Android中进程和线程的关系？区别？
>  Q：为何需要进行IPC？多进程通信可能会出现什么问题？
>  Q：什么是序列化？Serializable接口和Parcelable接口的区别？为何推荐使用后者？
>  Q：Android中为何新增Binder来作为主要的IPC方式？
>  Q：使用Binder进行数据传输的具体过程？
>  Q：Binder框架中ServiceManager的作用？
>  Q：Android中有哪些基于Binder的IPC方式？简单对比下？
>  Q：是否了解AIDL？原理是什么？如何优化多模块都使用AIDL的情况？

- [View](https://www.jianshu.com/p/06ff0dfeed39)

> Q：MotionEvent是什么？包含几种事件？什么条件下会产生？
>  Q：scrollTo()和scrollBy()的区别？
>  Q：Scroller中最重要的两个方法是什么？主要目的是？
>  Q：谈一谈View的事件分发机制？
>  Q：如何解决View的滑动冲突？
>  Q：谈一谈View的工作原理？
>  Q：MeasureSpec是什么？有什么作用？
>  Q：自定义View/ViewGroup需要注意什么？
>  Q：[onTouch()、onTouchEvent()和onClick()关系？](https://www.jianshu.com/p/4b89d681c5a0)
>  Q：SurfaceView和View的区别？
>  Q：invalidate()和postInvalidate()的区别？

- [Drawable](https://www.jianshu.com/p/35c7775b8202)等资源

> Q：了解哪些Drawable？适用场景？
>  Q：mipmap系列中xxxhdpi、xxhdpi、xhdpi、hdpi、mdpi和ldpi存在怎样的关系？
>  Q：dp、dpi、px的区别？
>  Q：res目录和assets目录的区别？

- [Animation](https://www.jianshu.com/p/10dc575896d3)

> Q：Android中有哪几种类型的动画？
>  Q：帧动画在使用时需要注意什么？
>  Q：View动画和属性动画的区别？
>  Q：View动画为何不能真正改变View的位置？而属性动画为何可以？
>  Q：属性动画插值器和估值器的作用？

- [Window](https://www.jianshu.com/p/ed03aed9a4db)

> Q：Activity、View、Window三者之间的关系？
>  Q：Window有哪几种类型？
>  Q：Activity创建和Dialog创建过程的异同？

- [Handler](https://www.jianshu.com/p/1c79fb5296b6)

> Q：谈谈消息机制Hander？作用？有哪些要素？流程是怎样的？
>  Q：为什么系统不建议在子线程访问UI？
>  Q：一个Thread可以有几个Looper？几个Handler？
>  Q：如何将一个Thread线程变成Looper线程？Looper线程有哪些特点？
>  Q：可以在子线程直接new一个Handler吗？那该怎么做？
>  Q：Message可以如何创建？哪种效果更好，为什么？
>  Q：这里的[ThreadLocal](https://www.jianshu.com/p/ca8801044352)有什么作用？
>  Q：主线程中Looper的轮询死循环为何没有阻塞主线程？
>  Q：使用Hanlder的postDealy()后消息队列会发生什么变化？

- [线程](https://www.jianshu.com/p/ab77a2e83c52)

> Q：Android中还了解哪些方便线程切换的类？
>  Q：AsyncTask相比Handler有什么优点？不足呢？
>  Q：使用AsyncTask需要注意什么？
>  Q：AsyncTask中使用的线程池大小？
>  Q：HandlerThread有什么特点？
>  Q：快速实现子线程使用Handler
>  Q：IntentService的特点？
>  Q：为何不用bindService方式创建IntentService？
>  Q：线程池的好处、原理、类型？
>  Q：ThreadPoolExecutor的工作策略？
>  Q：什么是ANR？什么情况会出现ANR？如何避免？在不看代码的情况下如何快速定位出现ANR问题所在？

- [Bitmap](https://www.jianshu.com/p/aaafcd72c127)

> Q：加载图片的时候需要注意什么？
>  Q：LRU算法的原理？
>  Q：Android中缓存更新策略？

- [性能优化](https://www.jianshu.com/p/81485e65c2c8)

> Q：项目中如何做性能优化的？
>  Q：了解哪些性能优化的工具？
>  Q：布局上如何优化？列表呢？
>  Q：内存泄漏是什么？为什么会发生？常见哪些内存泄漏的例子？都是怎么解决的？
>  Q：内存泄漏和内存溢出的区别？
>  Q：什么情况会导致内存溢出？

- 开源框架（略）
- 谷歌新动态

> Q：是否了解和使用过谷歌推出的新技术？
>  Q：有了解刚发布的Androidx.0的特性吗？
>  Q：Kotlin对Java做了哪些优化？

b.`Java`

- 基础

> Q：面向对象编程的四大特性及其含义？
>  Q：String、StringBuffer和StringBuilder的区别？
>  Q：String a=""和String a=new String("")的的关系和异同？
>  Q：Object的equal()和==的区别？
>  Q：装箱、拆箱什么含义？
>  Q：int和Integer的区别？
>  Q：[遇见过哪些运行时异常？异常处理机制知道哪些？](https://www.jianshu.com/p/3718766df5ba)
>  Q：[什么是反射，有什么作用和应用？](https://www.jianshu.com/p/fcdfb8234b66)
>  Q：什么是内部类？有什么作用？静态内部类和非静态内部类的区别？
>  Q：final、finally、finalize()分别表示什么含义？
>  Q：重写和重载的区别？
>  Q：抽象类和接口的异同？
>  Q：为什么匿名内部类中使用局部变量要用final修饰？
>  Q：Object有哪些公用方法？

- [集合](https://www.jianshu.com/p/7b9abda70c8f)

> Q：Java集合框架中有哪些类？都有什么特点
>  Q：集合、数组、[泛型](https://www.jianshu.com/p/fcdfb8234b66)的关系，并比较
>  Q：ArrayList和LinkList的区别？
>  Q：ArrayList和Vector的区别？
>  Q：HashSet和TreeSet的区别？
>  Q：HashMap和Hashtable的区别？
>  Q：HashMap在put、get元素的过程？体现了什么数据结构？
>  Q：如何解决Hash冲突？
>  Q：如何保证HashMap线程安全？什么原理？
>  Q：HashMap是有序的吗？如何实现有序？
>  Q：HashMap是如何扩容的？如何避免扩容？
>  Q：hashcode()的作用，与equal()有什么区别？

- 并发

> Q：开启一个线程的方法有哪些？销毁一个线程的方法呢？
>  Q：同步和非同步、阻塞和非阻塞的概念
>  Q：Thread的join()有什么作用？
>  Q：[线程的有哪些状态？](https://www.jianshu.com/p/90a036212cb4)
>  Q：[什么是线程安全？保障线程安全有哪些手段？](https://www.jianshu.com/p/ca8801044352)
>  Q：[ReentrantLock和synchronized的区别?](https://www.jianshu.com/p/ca8801044352)
>  Q：synchronized和volatile的区别？
>  Q：synchronized同步代码块还有同步方法本质上锁住的是谁？为什么？
>  Q：sleep()和wait()的区别？

- Java新动态

> Q：是否了解Java1.x的特性吗？
>  Q：谈谈对面向过程编程、面向对象编程还有面向切面编程的理解

c.`计算机网络`

- 基础

> Q：五层协议的体系结构分别是什么？每一层都有哪些协议？
>  Q：为何有MAC地址还要IP地址？

- TCP

> Q：TCP和UDP的区别？
>  Q：拥塞控制和流量控制都是什么，两者的区别？
>  Q：谈谈TCP为什么要三次握手？为什么要四次挥手？
>  Q：播放视频用TCP还是UDP？为什么？

- HTTP

> Q：了解哪些响应状态码？
>  Q：get和post的区别？
>  Q：Http1.0、Http1.1、Http2.0的区别？
>  Q：HTTP和TCP的区别?
>  Q：HTTP和HTTPS的区别?
>  Q：HTTP和Socket的区别?
>  Q：在地址栏打入http://www.baidu.com会发生什么？

d.`JVM`

> Q：[JVM内存是如何划分的？](https://www.jianshu.com/p/cd93567ed868)
>  Q：[谈谈垃圾回收机制？为什么引用计数器判定对象是否回收不可行？知道哪些垃圾回收算法？](https://www.jianshu.com/p/a62697f00b85)
>  Q：[Java中引用有几种类型？](https://www.jianshu.com/p/a62697f00b85)在Android中常用于什么情景？
>  Q：[类加载的全过程是怎样的？什么是双亲委派模型？](https://www.jianshu.com/p/9ea809edebb6)
>  Q：[工作内存和主内存的关系？在Java内存模型有哪些可以保证并发过程的原子性、可见性和有序性的措施？](https://www.jianshu.com/p/90a036212cb4)
>  Q：JVM、Dalvik、ART的区别？
>  Q：Java中堆和栈的区别？

e.`操作系统`

> Q：操作系统中进程和线程的区别？
>  Q：死锁的产生和避免?

f.`数据结构&算法`

> Q：怎么理解数据结构？
>  Q：什么是斐波那契数列？
>  Q：迭代和递归的特点，并比较优缺点
>  Q：了解哪些查找算法，时间复杂度都是多少？
>  Q：了解哪些排序算法，并比较一下，以及适用场景
>  Q：快排的基本思路是什么？最差的时间复杂度是多少？如何优化？
>  Q：AVL树插入或删除一个节点的过程是怎样的？
>  Q：什么是红黑树？
>  Q：100盏灯问题
>  Q：老鼠和毒药问题，加个条件，必须要求第二天出结果
>  Q：海量数据问题
>  Q：（手写算法）二分查找
>  Q：（手写算法）反转链表
>  Q：（手写算法）用两个栈实现队列
>  Q：（手写算法）多线程轮流打印问题
>  Q：（手写算法）如何判断一个链有环/两条链交叉
>  Q：（手写算法）快速从一组无序数中找到第k大的数/前k个大的数
>  Q：（手写算法）最长（不）重复子串

g.`设计模式`

> Q：[谈谈MVC、MVP和MVVM，好在哪里，不好在哪里？](https://www.jianshu.com/p/e0867ac2a261)
>  Q：如何理解生产者消费者模型？
>  Q：是否能从Android中举几个例子说说用到了什么设计模式？
>  Q：装饰模式和代理模式有哪些区别？
>  Q：实现单例模式有几种方法？懒汉式中双层锁的目的是什么？两次判空的目的又是什么？
>  Q：谈谈了解的设计模式原则？

h.`数据库`

> Q：数据库中的事务了解吗？事务的四大特性？
>  Q：如何理解数据库的范式？

i.`hr问题`

> Q：请简单的自我介绍一下
>  Q：谈谈项目经历，为什么会做，怎么做的，遇到的难点？
>  Q：谈谈实习经历，做了什么，收获有哪些？
>  Q：谈谈学习Android的经历，有哪些学习方法和技巧？
>  Q：是否会考研？/为何不保研？
>  Q：成绩怎么样？奖学金情况?
>  Q：学过哪些课程？那门课印象最深刻/最有意义/学的最好/最不喜欢？为什么？
>  Q：近x年的职业规划？
>  Q：为什么想来我们公司？/为何不转正留在xx?
>  Q：对公司/部门是否有了解？
>  Q：为何会选择做技术？/对女生做开发的看法？
>  Q：学习生活中遇到什么挫折，如何解决的？
>  Q：还投过那些公司，进展如何？如果xx和xx都给你发offer会如何选择？
>  Q：家是哪里的？是独生子女吗？从小的家庭环境如何？
>  Q：平常有哪些兴趣爱好？大学参加了哪些校园活动？
>  Q：有男/女朋友吗？未来有什么规划？
>  Q：评价一下自己的优缺点？/用x个词形容你自己。/别人都是怎样评价你的？
>  Q：觉得自己博客写的最好的文章是什么？为什么？
>  Q：觉得自己的优势是什么？
>  Q：如何看待加班？
>  Q：意向工作城市是哪？/是否会考虑在xx发展?
>  Q：对于薪酬有什么想法？
>  Q：有什么问题想要问我？

j.`项目相关、实习相关技术问题`（略）

> Q：使用那些版本控制工具？Git和SVN的区别？
>  Q：[了解Git工具吗？用过哪些命令？解决冲突时git merge和git rebase的区别？](https://www.jianshu.com/p/e0867ac2a261)

（持续更新...）

[另：点击此处见Android学习笔记清单](https://www.jianshu.com/p/c44d7a106302)

------

4.**一点感悟**

其实到现在还觉得一切不太真实，没想到已然结束了令我心惊胆战许久的秋招，回望过去，不由得感慨，大概真的是越努力越幸运吧。

从最开始我的目标就很明确，而且一路脚踏实地，不曾动摇，用一年多的时间来积累资本丰满简历，学基础、写博客、做项目、去实习，也不忽视在校的学业，成绩一直很稳定，唯独没什么竞赛和论文，因为大学前些年一直沉迷于培养兴趣爱好，各处舞蹈排练和比赛，也算是大学里最丰富多彩的一段时光吧。

当然我深知现在不过是暂且告一段落，前方依旧任重而道远，要学的还有很多，努力加油吧丫头！



作者：厘米姑娘
链接：https://www.jianshu.com/p/0f72ac621f82
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。