https://mirrors.tuna.tsinghua.edu.cn/help/homebrew/

https://zhuanlan.zhihu.com/p/93092044

## **什么是Homebrew？**

简单来说就是一个macOS（或Linux）的包管理器，可以用它来安装你需要的软件，方便卸载跟升级。

## **如何安装Homebrew？**

我们一般用官网提供的统一安装方法，执行如下命令即可：

```text
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

但是，国内大家都懂得，网速很慢，而且会出现：

```text
error: RPC failed; curl 56 LibreSSL SSL_read: SSL_ERROR_SYSCALL, errno 54
fatal: The remote end hung up unexpectedly
fatal: early EOF
fatal: index-pack failed
```

因此，**解决问题：**

1.先试下在浏览器访问地址：[https://raw.githubusercontent.com/Homebrew/install/master/install](https://link.zhihu.com/?target=https%3A//raw.githubusercontent.com/Homebrew/install/master/install) ，不过一般出现这种情况，你在浏览器肯定是不能浏览到此地址的内容的。

复制手机获取到的内容（可以通过微信把你复制的内容传到电脑中），在电脑中新建文件brew_install，把内容复制进去，保存。

把brew_install文件里面的地址换成国内源（提高安装速度，不然会出现如下图的错误）

```text
#BREW_REPO = "https://github.com/Homebrew/brew".freeze
BREW_REPO = "git://mirrors.ustc.edu.cn/brew.git".freeze
```

执行命令进行安装

```text
/usr/bin/ruby ~/brew_install
```

2.关于第1步和大多数是一样的。但在安装到

```text
==> Tapping homebrew/core
Cloning into '/usr/local/Homebrew/Library/Taps/homebrew/homebrew-core'...
```

就会出现问题。没关系，**在出现问题前直接关掉命令窗口Terminal，然后进入下面的 Taps 目录，clone homebrew-core**

```text
cd /usr/local/Homebrew/Library/Taps/homebrew

git clone https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/homebrew-core.git
```

3.**替换homebrew源**

替换homebrew默认源

```text
cd "$(brew --repo)"
git remote set-url origin git://mirrors.ustc.edu.cn/brew.git
```

替换homebrew-core源

```text
cd "$(brew --repo)/Library/Taps/homebrew/homebrew-core"
git remote set-url origin git://mirrors.ustc.edu.cn/homebrew-core.git
```

### **brew更新**

```text
brew update
```

### **设置 bintray镜像**

```text
echo 'export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles' >> ~/.bash_profile
source ~/.bash_profile
```

总的来说：和知乎大佬Rayer一样，***只是大神没有说明在第二步的时候，不用等克隆完成，会让类似作者自己没有经验的菜鸟误以为安装失败。\***