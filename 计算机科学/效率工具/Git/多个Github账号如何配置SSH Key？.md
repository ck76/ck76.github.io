- vim ~/.ssh/config

  ```sh
  Host github.com
      HostName github.com
      PreferredAuthentications publickey
      IdentityFile ~/.ssh/id_rsa
  
  Host chengkun0804.github.com
      HostName github.com
      PreferredAuthentications publickey
      IdentityFile ~/.ssh/chengkun0804_id_rsa
  ```

- ssh-add ~/.ssh/chengkun0804_id_rsa

- ssh -T git@chengkun0804.github.com

- Hi chengkun0804! You've successfully authenticated, but GitHub does not provide shell access.

- 修改项目的git的config文件

  ```sh
  [core]
  	repositoryformatversion = 0
  	filemode = true
  	bare = false
  	logallrefupdates = true
  	ignorecase = true
  	precomposeunicode = true
  [remote "origin"]
  	url = git@chengkun0804.github.com:chengkun0804/TinyOS.git
  	fetch = +refs/heads/*:refs/remotes/origin/*
  [branch "master"]
  	remote = origin
  	merge = refs/heads/master
  ```



- <https://www.jianshu.com/p/e50aeb57ea57>