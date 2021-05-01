- git stash

`git stash`命令提供了参数用于缓存上面两种类型的文件。使用`-u`或者`--include-untracked`可以stash untracked文件。使用`-a`或者`--all`命令可以stash当前目录下的所有修改。

- git stash pop      弹出栈顶

- git stash apply   不弹出栈顶，可多次使用

- git stash list        查看现有stash

- git stash drop    移除stash  

  ```java
  $ git stash list
  stash@{0}: WIP on master: 049d078 added the index file
  stash@{1}: WIP on master: c264051 Revert "added file_size"
  stash@{2}: WIP on master: 21d80a5 added number to log
  $ git stash drop stash@{0}
  Dropped stash@{0} (364e91f3f268f0900bc3ee613f9f733e82aaed43)
  ```

