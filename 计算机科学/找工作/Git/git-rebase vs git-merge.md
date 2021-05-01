先上几个链接

- [git rebase简介(基本篇)](https://blog.csdn.net/hudashi/article/details/7664631)
- [rebase](https://blog.csdn.net/FightFightFight/article/details/80850328)



> git checkout dev
>
> git rebase master
>
> git checkout master
>
> git merge dev





原文：

http://gitbook.liuhui998.com/4_2.html

一、基本

git rebase用于把一个分支的修改合并到当前分支。

假设你现在基于远程分支"origin"，创建一个叫"mywork"的分支。

$ git checkout -b mywork origin

假设远程分支"origin"已经有了2个提交，如图

![img](https://img-my.csdn.net/uploads/201206/14/1339682677_4329.jpg)

 

现在我们在这个分支做一些修改，然后生成两个提交(commit).

$ vi file.txt

$ git commit

$ vi otherfile.txt

$ git commit

...

但是与此同时，有些人也在"origin"分支上做了一些修改并且做了提交了. 这就意味着"origin"和"mywork"这两个分支各自"前进"了，它们之间"分叉"了。

![img](https://img-my.csdn.net/uploads/201206/14/1339682809_4752.jpg)

 

 

在这里，你可以用"pull"命令把"origin"分支上的修改拉下来并且和你的修改合并； 结果看起来就像一个新的"合并的提交"(merge commit):

![img](https://img-my.csdn.net/uploads/201206/14/1339682845_9921.jpg)

 

但是，如果你想让"mywork"分支历史看起来像没有经过任何合并一样，你也许可以用 git rebase:

$ git checkout mywork

$ git rebase origin

这些命令会把你的"mywork"分支里的每个提交(commit)取消掉，并且把它们临时 保存为补丁(patch)(这些补丁放到".git/rebase"目录中),然后把"mywork"分支更新 为最新的"origin"分支，最后把保存的这些补丁应用到"mywork"分支上。

![img](https://img-my.csdn.net/uploads/201206/14/1339682915_7495.jpg)

 

当'mywork'分支更新之后，它会指向这些新创建的提交(commit),而那些老的提交会被丢弃。 如果运行垃圾收集命令(pruning garbage collection), 这些被丢弃的提交就会删除. （请查看 git gc)

![img](https://img-my.csdn.net/uploads/201206/14/1339682976_4523.jpg)

 

二、解决冲突

在rebase的过程中，也许会出现冲突(conflict). 在这种情况，Git会停止rebase并会让你去解决 冲突；在解决完冲突后，用"git-add"命令去更新这些内容的索引(index), 然后，你无需执行 git-commit,只要执行:

$ git rebase --continue

这样git会继续应用(apply)余下的补丁。

在任何时候，你可以用--abort参数来终止rebase的行动，并且"mywork" 分支会回到rebase开始前的状态。

$ git rebase --abort

三、git rebase和git merge的区别

现在我们可以看一下用合并(merge)和用rebase所产生的历史的区别：

![img](https://img-my.csdn.net/uploads/201206/14/1339683149_4793.jpg)

当我们使用Git log来参看commit时，其commit的顺序也有所不同。

假设C3提交于9:00AM,C5提交于10:00AM,C4提交于11:00AM，C6提交于12:00AM,

对于使用git merge来合并所看到的commit的顺序（从新到旧）是：C7 ,C6,C4,C5,C3,C2,C1

对于使用git rebase来合并所看到的commit的顺序（从新到旧）是：C7 ,C6‘,C5',C4,C3,C2,C1

 因为C6'提交只是C6提交的克隆，C5'提交只是C5提交的克隆，

从用户的角度看使用git rebase来合并后所看到的commit的顺序（从新到旧）是：C7 ,C6,C5,C4,C3,C2,C1

 另外，我们在使用git pull命令的时候，可以使用--rebase参数，即git pull --rebase,这里表示把你的本地当前分支里的每个提交(commit)取消掉，并且把它们临时 保存为补丁(patch)(这些补丁放到".git/rebase"目录中),然后把本地当前分支更新 为最新的"origin"分支，最后把保存的这些补丁应用到本地当前分支上。关于git pull的更多内容请参考《[git pull简介](http://blog.csdn.net/hudashi/article/details/7664449)》

码农学演讲：

《[拔草还是种花](http://hubingforever.lofter.com/post/273462_12e3d89dc)》

《[小城故事](http://hubingforever.lofter.com/post/273462_12e2cba09)》



---

# 概述

在之前总结分支相关内容时说道，合并两个分支的提交可以使用`git merge`，然而除了这种方式之外，还有一种方式就是使用`git rebase`，这两种方式的最终结果都相同，但是合并历史却不同；`git merge`是将两个分支做一个三方合并(如果不是直接上游分支)，这样一来，查看提交历史记录，可能会显得非常凌乱。`git rebase`则会将当前分支相对于基低分支的所有提交生成一系列补丁，然后放到基底分支的顶端，从而使得提交记录变称一条直线，非常整洁。

# git merge 和 git rebase 区别

假设现在本地仓库中有两个分支:`master`分支和`branch1`分支,提交历史用图来表示如下：
![这里写图片描述](https://img-blog.csdn.net/20180628225429477?watermark/2/text/aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L0ZpZ2h0RmlnaHRGaWdodA==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70)
现在要合并branch1到master分支，如果使用`git merge`则执行如下命令：

```bash
$ git checkout master
$ git merge branch1
12
```

合并后查看提交历史如下：

```bash
$ git log --graph --pretty="oneline"
*   fe8799e0aec30e388306883960b4cf438d3f1ec4 Merge branch 'branch1'
|\  
| * cf31255da6e84acc6f6840e3ceb0fd3129e2d73e UserA commit 3--branch1
| * 5c2d1c938f8e5f98dccaa0a5ab6222bd6b1cd75d UserA commit 2--branch1
* | 284aa3eb6c405411584d682a1387118fe92e4821 Usera commit master
* | 967fca58deb914ad1cda9ff84291fd946045207d Usera commit master
|/  
* d989fc50530918b3b7b0ed68b31d6751c2302875 UserA commit 1
123456789
```

使用图来表示，本地仓库提交历史如下：
![这里写图片描述](https://img-blog.csdn.net/20180628225750277?watermark/2/text/aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L0ZpZ2h0RmlnaHRGaWdodA==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70)

现在我们使用`git rebase`合并原来的`master`分支和`branch1`分支，假设当前分支为branch1，基地分支为master:

```bash
$ git checkout branch1
$ git rebase master
First, rewinding head to replay your work on top of it...
Applying: UserA commit 2--branch1
Applying: UserA commit 3--branch1
Applying: Usera commit master
Applying: Usera commit master
1234567
```

合并后查看提交历史如下：

```bash
$ git log --graph --pretty="oneline"
* 6cf95c391ba7d43d0f5d95300130a43816af82c8 Usera commit master
* 63def8a8740b9b3c9f6c09ae49ba72faa9446cf6 Usera commit master
* 33049864f83a686bff9b2a2d8626427653a16f22 UserA commit 3--branch1
* 14ac1cac7357ccf35581c89e099793260264d3ea UserA commit 2--branch1
* d989fc50530918b3b7b0ed68b31d6751c2302875 UserA commit 1
123456
```

使用图来表示，本地仓库提交历史如下：
![这里写图片描述](https://img-blog.csdn.net/20180628230159867?watermark/2/text/aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L0ZpZ2h0RmlnaHRGaWdodA==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70)
可以看到，现在`branch1`分支上相对于`master`分支的提交，提交到了`master`分支的顶端，如此一来整个提交记录保持在一条直线上。这就是`git rebase`。

# rebase原理

`git rebase <branch>`的原理是：找到两个分支最近的共同祖先，根据当前分支(上例中`branch1`)的提交历史生成一系列补丁文件，然后以基地分支最后一个提交为新的提交起始点，应用之前生成的补丁文件，最后形成一个新的合并提交。从而使得变基分支成为基地分支的直接下游。rebase一般被翻译为变基。

当`branch1`分支完成变基后，直接变成了`master`分支的下游了，这时切换到master分支，直接通过`git merge`即可将branch1分支的合并到master分支上：

```bash
$ git merge branch1 
Updating ff7658d..d6168dc
Fast-forward
 test.txt | 1 +
 1 file changed, 1 insertion(+)
12345
```

掌握了rebase基本原理后，接下来对该命令一些参数进行下总结。

#### git rebase branchA branchB

首先会取出branchB，将branchB中的提交放在branchA的顶端，一般branchB为当前分支，可以不指定。

假设当前本地仓库提交历史如下，且处于topic分支:

```
     A---B---C topic
    /
D---E---F---G master
123
```

此时我们使用`git rebase`将两个分支的提交合并到master分支的顶端：

```bash
$ git rebase master
# 或者
$ git rebase master topic
123
```

此时，提交历史将变为：

```
             A'--B'--C' topic
            /
D---E---F---G master
123
```

#### git rebase --onto

如果要在合并两分支时需要跳过某一分支的提交，这时就可以使用`--onto`参数了。比如，假设当前本地仓库提交历史如下：

```
A---B---E---F---G  master
    \
     C---D---H---I---J  next
                      \
                       K---L---M  topic
12345
```

此时`topic`分支的上游分支是`next`分支，如果要将`topic`分支上的提交(K,M,L)跳过next分支，直接放到master分支上，就需要加上`--onto`参数：

```bash
$ git rebase --onto master next topic
1
```

上述命令的意思是:取出topic分支，找出topic和next分支的共同祖先之后的提交，然后放在master分支上，执行后提交历史变为如下：

```
A---B---E---F---G  master
    \            \
     \            K'---L'---M'  topic 
      \     
       C---D---H---I---J  next
                      
123456
```

#### git rebase --continue/abort/skip

这三个命令分别表示:继续执行变基操作、终止变基、跳过某一文件继续进行。在rebase的过程中，有可能会出现文件冲突,比如:

```bash
$ git rebase master
First, rewinding head to replay your work on top of it...
Applying: [Description]:test1
Using index info to reconstruct a base tree...
M	test.txt
Falling back to patching base and 3-way merge...
Auto-merging test.txt
CONFLICT (content): Merge conflict in test.txt
error: Failed to merge in the changes.
Patch failed at 0001 [Description]:test1
The copy of the patch that failed is found in: .git/rebase-apply/patch

When you have resolved this problem, run "git rebase --continue".
If you prefer to skip this patch, run "git rebase --skip" instead.
To check out the original branch and stop rebasing, run "git rebase --abort".
$ 
12345678910111213141516
```

这种情况下，首先要解决冲突,解决冲突后可以选择继续执行rebase或者结束rebase，一般的做法为：

```bash
$ git add filename
$ git rebase --continue
12
```

或者可以选择终止变基：

```bash
$ git rebase --abort
1
```

或者跳过该patch:

```bash
$ git rebase --skip
1
```

#### git rebase -i

该命令相比其他命令，使用频率要高得多。`git rebase -i <commitid>`可以进行交互式变基，相比于`git rebase <branch>`用来变基，它经常用来操作当前分支的提交，git会将`<commitid>-HEAD`之间的提交列在一个变基脚本中，每一笔提交根据用户设置的命令，会进行不同的操作，如修改提交信息、移除指定提交、合并两个提交为一个(压缩提交)、拆分提交等。

如，要对最近4次提交进行重新操作，则:

```bash
$ git rebase -i HEAD~4
1
```

此时将会弹出如下形式的变基脚本:

```bash
  1 pick af98479 [Description]:test4
  2 pick 3cc9d66 test3
  3 pick a7e819e usera commit03 branch2
  4 pick efc5b15 usera commit04 branch2
  5 
  6 # Rebase de7b118..efc5b15 onto de7b118 (4 command(s))
  7 #
  8 # Commands:
  9 # p, pick = use commit
 10 # r, reword = use commit, but edit the commit message
 11 # e, edit = use commit, but stop for amending
 12 # s, squash = use commit, but meld into previous commit
 13 # f, fixup = like "squash", but discard this commit's log message
 14 # x, exec = run command (the rest of the line) using shell
 15 # d, drop = remove commit
 16 #
 17 # These lines can be re-ordered; they are executed from top to bottom.
 18 #
 19 # If you remove a line here THAT COMMIT WILL BE LOST.
 20 #
 21 # However, if you remove everything, the rebase will be aborted.
 22 #
 23 # Note that empty commits are commented out

123456789101112131415161718192021222324
```

这里我们可以修改pick为下面给出的其他命令，比如如果要修改提交信息，就使用r或reword，各指令的含义如下:

```bash
p,pick:直接使用该次提交
r,reword:使用该次提交，但重新编辑提交信息
e,edit:停止到该次提交，通过`git commit --amend`追加提交，完毕之后不要忘记使用`git rebase --continue`完成这此rebase
s,squash,压缩提交，将和上一次提交合并为一个提交
x,exec,运行命令
d,drop,移除这次提交
123456
```

接下来我们看看他们如何使用。

###### 1.修改多个提交信息

使用`git commit --amend`在最近一次提交上追加提交，因此可以使用该命令来修改最后一次的提交信息，如果要修改做个提交信息，需要`git rebase -i <commitid>`打开变基脚本后在需要修改信息的提交上执行reword操作，比如以下示例:

查看最近四次提交记录

```bash
$ git log --oneline -4
d0a80c2 02-b2
b6c6595 01-b2
ea2f366 b2
49afab9 commit branch3w
12345
```

提交信息非常不人性化，因此对以上几个提交记录修改提交信息，将默认的pick改为r或者reword:

```bash
$ git rebase -i HEAD~4

  1 r 49afab9 commit branch3w
  2 r ea2f366 b2
  3 r b6c6595 01-b2
  4 r d0a80c2 02-b2
  5 # ......
1234567
```

保存退出后,会弹出编译器输入提交信息，输入完毕后：

```bash
$ git rebase -i HEAD~4
[detached HEAD afeaed3] commit first
 Date: Wed Jun 27 20:26:33 2018 +0800
 1 file changed, 1 insertion(+), 1 deletion(-)
[detached HEAD 528910c] commit second
 Date: Wed Jun 27 20:29:07 2018 +0800
 1 file changed, 1 deletion(-)
[detached HEAD 0e09a0f] commit 3th
 Date: Wed Jun 27 20:29:25 2018 +0800
 1 file changed, 1 deletion(-)
[detached HEAD eaed13d] commit 4th
 Date: Wed Jun 27 20:29:35 2018 +0800
 1 file changed, 1 deletion(-)
Successfully rebased and updated detached HEAD.

123456789101112131415
```

再次查看提交log：

```bash
$ git log --oneline -4
eaed13d commit 4th
0e09a0f commit 3th
528910c commit second
afeaed3 commit first
12345
```

利用`git rebase -i <commitid>`和`reword`就可以完成修改多次提交信息了。

###### 2.重新排序提交

改变变基脚本中的顺序就可以对之前的提交重新排序，如:
选择最近4次提交进行处理:

```
$ git rebase -i HEAD~4
1
```

此时会打开变基脚本，在脚本中将second这次提交放在最后一次提交中:

```bash
  1 pick ecd66f5 commit first
  2 pick 7dbfe25 commit 3th
  3 pick 82ba6a6 commit 4th
  4 pick a77e06e commit second
  5 # .....
12345
```

保存退出，查看提交log,发现second变为最后一次提交：

```bash
$ git log --oneline -4
fe15bdb commit second
18fa9a9 commit 4th
d08c408 commit 3th
ecd66f5 commit first
$ 

1234567
```

###### 3.压缩提交

如果要压缩两个提交为一次，需要`git rebase -i <commitid>`打开变基脚本后在需要压缩的提交上执行`squash`操作，当保存退出后，会将该笔提交和上一笔提交压缩为一个提交，比如:

先查看当前提交记录:

```bash
$ git log --oneline -4
fe15bdb commit second
18fa9a9 commit 4th
d08c408 commit 3th
ecd66f5 commit first
12345
```

现在我们将4th和3th这两笔提交压缩为一笔提交，在执行`git rebase -i HEAD~4`后在变基脚本中做如下修改：

```bash
  1 pick ecd66f5 commit first
  2 pick d08c408 commit 3th
  3 squash 18fa9a9 commit 4th
  4 pick fe15bdb commit second
1234
```

保存退出后，输入一下提交信息，成功后再查看log:

```bash
$ git log --oneline -4
cf4159b commit second
9d73407 commit----compress 3th and 4th
ecd66f5 commit first
1234
```

说明合并成功了，如果要对多个提交进行合并压缩，则可以按照如下的方式:

```bash
  1 pick ecd66f5 commit first
  2 squash d08c408 commit 3th
  3 squash 18fa9a9 commit 4th
  4 pick fe15bdb commit second
1234
```

这表示会将first、3th、4th进行合并。

###### 4.拆分提交

如果要将一次提交拆分为多次提交，则可以将变基脚本中对应提交的指令修改为`edit`。拆分一个提交会撤消这个提交，然后多次地、部分地、暂存与提交直到完成你所需次数的提交。比如下面示例：

首先查看提交记录:

```bash
$ git log --oneline -4
2a5c2aa commit 4th
f2ceb0f commit 3th
20fe2f9 commit second
c51adbe commit first
12345
```

现在修改3th这次提交，将这次提交拆分为多次提交,首先执行`git rebase -i HEAD～3`，然后在变基脚本中将该次提交指令改为`edit`：

```bash
  1 pick 20fe2f9 commit second
  2 edit f2ceb0f commit 3th
  3 pick 2a5c2aa commit 4th
  # ......
1234
```

保存退出后，再次查看提交记录：

```bash
$ git log --oneline -3
f2ceb0f commit 3th
20fe2f9 commit second
c51adbe commit first
1234
```

也就是说，3th这次提交是现在最近的一次提交了，我们要拆分这次提交，那就就要重置这次提交，让HEAD指针指向上一次提交:

```bash
$ git reset HEAD～
1
```

现在进行多次提交:

```bash
$ git add .
$ git commit -m "split commit3th---1"
$ git add .
$ git commit -m "split commit3th---2"
......
$ git add .
$ git commit -m "split commit3th---n"
1234567
```

满足自己拆分后，继续完成这次rebase:

```bash
$ git rebase --continue
1
```

最后查看提交记录，原来的提交被移除，新增了三条:

```bash
$ git log --oneline -6
1df4a4d split 3th----3
1c22d70 split 3th----2
dbc7d91 split 3th----1
20fe2f9 commit second
c51adbe commit first
123456
```

###### 5.移除提交

如果要移除某次提交，可以在变基脚本中将对应提交指令改为`drop`,或者直接干脆删除，比如要移除上例中新家的三个记录:

```
  1 pick c51adbe commit first
  2 pick 20fe2f9 commit second
  3 drop dbc7d91 split 3th----1
  4 drop 1c22d70 split 3th----2
  5 #pick 1df4a4d split 3th----3

123456
```

查看提交记录:

```bash
$ git log --oneline -6
20fe2f9 commit second
c51adbe commit first
123
```

# rebase的风险

一旦分支中的提交对象发布到公共仓库，就千万不要对该分支进行变基操作。
因为不管是`git rebase <branch>`还是`git rebase <commitid>`,都重置了提交的SHA-1校验和，当你将本地变基后的提交推送到远端后，别人从服务器同步代码时，由于相同的内容却有不同的SHA-1校验值，因此会再此进行一次合并，于是就会有两个作者、commit信息完全相同的提交，但是SHA-1校验值不同，这无疑会带来麻烦。
因此，如果把变基当成一种在推送之前清理提交历史的手段，而且仅仅变基那些尚未公开的提交对象，就没问题。如果变基那些已经公开的提交对象，并且已经有人基于这些提交对象开展了后续开发工作的话，就会出现叫人沮丧的麻烦。

参考：
[Git 官网](https://git-scm.com/book/en/v2/Git-Branching-Rebasing)