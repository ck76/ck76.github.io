**适用场景：**

比方说，你的代码已经提交到git库，leader审核的时候发现有个Java文件代码有点问题，于是让你修改，通常有2种方法：

**方法1**：leader 将你提交的所有代码 abandon掉，然后你回去 通过git reset …将代码回退到你代码提交之前的版本，然后你修改出问题的Java文件，然后 git add xx.java xxx.java -s -m “Porject : 1.修改bug…” 

最后通过 git push origin HEAD:refs/for/branches

**方法2**： 

leader不abandon代码，你回去之后，修改出问题的Java文件，修改好之后，git add 该出问题.java 

然后 git commit –amend –-no-edit, 

最后 git push origin HEAD:refs/for/branches。

当我们想要对上一次的提交进行修改时，我们可以使用git commit –amend命令。git commit –amend既可以对上次提交的内容进行修改，也可以修改提交说明。

**举个例子：**

Step1：我们先在工作区中创建两个文件a.txt和b.txt。并且add到暂存区，然后执行提交操作：

Step2：此时我们查看一下我们的提交日志：

可以看到我们的提交日志中显示最新提交有两个文件被改变。

Step3：此时我们发觉我们忘了创建文件c.txt，而我们认为c.txt应该和a.txt,b.txt一同提交，而且a.txt文件中应该有内容‘a’。于是我们在工作区中创建c.txt，并add到暂存区。并且修改a.txt（故意写错语法且没有将a.txt的修改add到暂存区）：

Step4：我们查看一下此时的提交日志，可以看到上次的提交0c35a不见了，并且新的提交11225好就是上次提交的修补提交，它就像是在上次提交被无视了，修改后重新进行提交了一样：

Step5：此时我们发现a.txt文件修改没有成功，于是我们还得进行一次对a.txt的修改，将a.txt add到stage，然后再执行一次与上一次类似的提交修补：

OK了，git commit –amend的用法大致就是这样。

**总结：git  commit --amend 相当于上次提交错误的信息被覆盖了，gitk图形化界面上看不到上次提交的信息，git log上也看不到之前的信息，而add 后再commit 相当于重新加了一个信息。相当于打了个补丁？**

 

 `git push origin HEAD:refs/for/master`

 

 