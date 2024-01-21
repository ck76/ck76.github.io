[TOC]

- https://www.cnblogs.com/xueweihan/p/5743327.html
- https://www.cnblogs.com/zhangsanfeng/p/9575184.html

**Git merge**是用来合并两个分支的。

```
git merge b      # 将b分支合并到当前分支
同样 git rebase b，也是把 b分支合并到当前分支
```

 

**原理 如下**： 

假设你现在基于远程分支"origin"，创建一个叫"mywork"的分支。

$ git checkout -b mywork origin

假设远程分支"origin"已经有了2个提交，如图

![img](http://my.csdn.net/uploads/201206/14/1339682677_4329.jpg)

现在我们在这个分支做一些修改，然后生成两个提交(commit).

```
$ vi file.txt
$ git commit
$ vi otherfile.txt
$ git commit
```

 

 

但是与此同时，有些人也在"origin"分支上做了一些修改并且做了提交了. 这就意味着"origin"和"mywork"这两个分支各自"前进"了，它们之间"分叉"了。

![img](http://my.csdn.net/uploads/201206/14/1339682809_4752.jpg)

 

在这里，你可以用"pull"命令把"origin"分支上的修改拉下来并且和你的修改合并； 结果看起来就像一个新的"合并的提交"(merge commit):

![img](http://my.csdn.net/uploads/201206/14/1339682845_9921.jpg)

但是，如果你想让"mywork"分支历史看起来像没有经过任何合并一样，你也许可以用 git rebase:

$ git checkout mywork

$ git rebase origin

这些命令会把你的"mywork"分支里的每个提交(commit)取消掉，并且把它们临时 保存为补丁(patch)(这些补丁放到".git/rebase"目录中),然后把"mywork"分支更新 为最新的"origin"分支，最后把保存的这些补丁应用到"mywork"分支上。

![img](http://my.csdn.net/uploads/201206/14/1339682915_7495.jpg)

当'mywork'分支更新之后，它会指向这些新创建的提交(commit),而那些老的提交会被丢弃。 如果运行垃圾收集命令(pruning garbage collection), 这些被丢弃的提交就会删除. （请查看 git gc)

![img](http://my.csdn.net/uploads/201206/14/1339682976_4523.jpg)

二、解决冲突

在rebase的过程中，也许会出现冲突(conflict). 在这种情况，Git会停止rebase并会让你去解决 冲突；在解决完冲突后，用"git-add"命令去更新这些内容的索引(index), 然后，你无需执行 git-commit,只要执行:

$ git rebase --continue

这样git会继续应用(apply)余下的补丁。

在任何时候，你可以用--abort参数来终止rebase的行动，并且"mywork" 分支会回到rebase开始前的状态。

$ git rebase --abort

三、git rebase和git merge的区别

现在我们可以看一下用合并(merge)和用rebase所产生的历史的区别：

![img](http://my.csdn.net/uploads/201206/14/1339683149_4793.jpg)

当我们使用Git log来参看commit时，其commit的顺序也有所不同。

假设C3提交于9:00AM,C5提交于10:00AM,C4提交于11:00AM，C6提交于12:00AM,

对于使用git merge来合并所看到的commit的顺序（从新到旧）是：C7 ,C6,C4,C5,C3,C2,C1

对于使用git rebase来合并所看到的commit的顺序（从新到旧）是：C7 ,C6‘,C5',C4,C3,C2,C1

 因为C6'提交只是C6提交的克隆，C5'提交只是C5提交的克隆，

从用户的角度看使用git rebase来合并后所看到的commit的顺序（从新到旧）是：C7 ,C6,C5,C4,C3,C2,C1

 

 

 

知乎用户回答https://www.zhihu.com/question/36509119/answer/67828312



两个使用场景是不一样的，merge只是合并另外一个分支的内容，rebase也合并另外一个分支的内容，但是会把本分支的commits顶到最顶端

假设我们现在有3个分支

1. master分支：线上环境使用的分支
2. testing分支：测试环境使用的分支
3. my_feature分支：开发新功能的分支，也就是当前分支

A. 假设我在my_feature上开发了一段时间，之后另外的同事开发的功能正式上线到master分支了，那么我可以在当前的分支下rebase一下master分支，这样我这个分支的几个commits相对于master还是处于最顶端的，也就是说rebase主要用来跟上游同步，同时把自己的修改顶到最上面

B. 我在my_feature上开发了一段时间了，想要放到testing分支上，那就切到testing，然后merge my_feature进来，因为是个测试分支，commits的顺序无所谓，也就没必要用rebase (当然你也可以用rebase)



另外，单独使用rebase，还有调整当前分支上commits的功能(合并，丢弃，修改commites msg)

**PS:**
其他知友的答案都说到冲突的问题，

\1. 用merge确实只需要解决一遍冲突，比较简单粗暴
\2. 用rebase有时候会需要多次fix冲突（原因在于本地分支已经提交了非常多的commit，而且很久都没有和上游合并过）

我个人推荐大家开发的时候，**尽量及时rebase上游分支**（我习惯是每周merge一次），有冲突提前就fix掉，即使我们自己的分支开发了很久（哪怕是几个月），也不会积累太多的conflict，最后合并进主分支的时候特别轻松， 非常反对从master check出新分支，自己闷头开发几个月，结果最后merge进主分支的时候，一大堆冲突，自己还嗷嗷叫的行为 .。

 

参考博客：https://www.cnblogs.com/marblemm/p/7161614.html