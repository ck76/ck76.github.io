[TOC]

# 前言

正文开始之前，我想我们需要弄明白几个问题：

1.tag 是什么？ 
2.使用tag 的好处？ 
3.tag 和 branch 的区别以及使用场景？

<!--more-->

- **tag 是什么？** 
  tag , 翻译过来是标签的意思，顾名思义，标签是为了标记某种事物。 
  tag 是 Git 版本库的一个快照，指向某个 commit 的指针。

- **使用tag 的好处？** 
  tag 的存在，是因为我们需要这种标记的功能。目前的项目开发中，当发布版本时 tag 就派上用场了。例如 v1.0.1，v1.0.2… 
  另外，git 提供了 tag 的增删改查一系列操作，在 tag 的使用上，可谓非常之方便。

- **tag 和 branch 的区别以及使用场景？** 
  想到这里，你可能觉得 tag 和 branch 有点相似。没错，的确是有点像，但是它们的职责分工和本质都是不同的。 
  tag 对应某次 commit, 是一个点，是不可移动的。 
  branch 对应一系列 commit，是很多点连成的一根线，有一个HEAD 指针，是可以依靠 HEAD 指针移动的。

  所以，两者的区别决定了使用方式，改动代码用 branch ,不改动只查看用 tag。

  tag 和 branch 的相互配合使用，有时候起到非常方便的效果，例如 已经发布了 v1.0 v2.0 v3.0 三个版本，这个时候，我突然想不改现有代码的前提下，在 v2.0 的基础上加个新功能，作为 v4.0 发布。就可以 检出 v2.0 的代码作为一个 branch ，然后作为开发分支。

# tag 的简单使用

以下命令都是我使用 tag 过程中一般会使用到的，可以说都是常用命令。

## 1.创建标签

需要说明的是，创建 tag 是基于本地分支的 commit，而且与分支的推送是两回事，就是说分支已经推送到远程了，但是你的 tag 并没有，如果把 tag 推送到远程分支上，需要另外执行 tag 的推送命令。

```sh
git tag <tagName> //创建本地tag
git push origin <tagName> //推送到远程仓库12
```

若存在很多未推送的本地标签，你想一次全部推送的话，可以使用一下的命令：

```shell
git push origin --tags  
```

以上是基于本地当前分支的最后的一个 commit 创建的 tag ，但是如果不想以最后一个，只想以某一个特定的提交为 tag ，也是可以的，只要你知道 commit 的 id。

```sh
git log --pretty=oneline //查看当前分支的提交历史，里面包含 commit id
git tag -a <tagName> <commitId>12
```

- 创建轻量标签

  `$ git tag v0.2.0 -light`

  解释：创建轻量标签不需要传递参数，直接指定标签名称即可。

- 创建附注标签

  `$ git tag -a v0.1.0 -m "release 0.1.0 version"`

  解释：创建附注标签时，参数-a即annotated的缩写，指定标签类型，后附标签名。参数m指定标签说明，说明信息会保存在标签对象中。

## 2.查看标签

查看本地某个 tag 的详细信息：

```sh
git show <tagName>
```

查看本地所有 tag ：

```sh
//下面两个命令都可以
git tag 
git tag -l
```

查看远程所有 tag：

```shell
git ls-remote --tags origin
```

## 3.删除标签

本地 tag 的删除：

```sh
git tag -d <tagName>
```

远程 tag 的删除：

```sh
git push origin :<tagName>
```

## 4.重命名标签

这个本质上是删除掉旧名字 tag ，然后再新建新名字 tag ,然后实现重命名的作用。

如果 tag 只存在本地，那么只需要删除本地的旧名字 tag ，然后新建新名字 tag：

```sh
git tag -d <oldTagName>
git tag <newTagName>
git push origin <newTagName> //推送到远程仓库
```

若已经推送到远程了，那么不仅要删除本地的，还要删除远程的，再重新创建和推送：

```sh
git tag -d <oldTagName>
git push origin :<oldTagName>
git tag <newTagName>
git push origin <newTagName> //推送到远程仓库
```

## 5.检出标签

命令如下：

```sh
git checkout -b <branchName> <tagName>
```

因为 tag 本身指向的就是一个 commit，所以和根据 commit id 检出分支是一个道理。 
但是需要特别说明的是，如果我们想要修改 tag 检出代码分支，那么虽然分支中的代码改变了，但是 tag 标记的 commit 还是同一个，标记的代码是不会变的，这个要格外的注意。

##  6.发布标签

- 将v0.1.0标签提交到git服务器

  ```sh
  $ git push origin v0.1.0
  ```

  解释：通常的git push不会将标签对象提交到git服务器，我们需要进行显式的操作。

- 将本地所有标签一次性提交到git服务器

  ```sh
  $ git push origin –tags
  ```