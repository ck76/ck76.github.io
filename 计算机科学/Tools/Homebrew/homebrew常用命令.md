[TOC]

### Homebrew 常用命令和工具

```sh
1安装软件：brew install 软件名，例：brew install wget
2搜索软件：brew search 软件名，例：brew search wget
3卸载软件：brew uninstall 软件名，例：brew uninstall wget
4更新所有软件：brew update
5更新具体软件：brew upgrade 软件名 ，例：brew upgrade git
6显示已安装软件：brew list
7查看软件信息：brew info／home 软件名 ，例：brew info git ／ brew home git
8显示包依赖：brew reps
9显示安装的服务：brew services list
10安装服务启动、停止、重启：brew services start/stop/restart serverName
```

```
brew -v
brew update --verbose #观察update过程
brew info demo #查看demo包的信息
Brew tap xxx 添加源
brew update     #更新自己的Homebrew
brew search demo #寻找包名含有demo的包
brew doctor #检查brew的运行状态
brew outdates #检查本机内已经处于旧版本的包
brew list #列出brew安装的包
brew cleanup #清理缓存
brew install demo #安装demo包
brew uninstall demo #卸载demo包
brew link demo #关联demo包
php -m | grep phalcon  #检测扩展
brew -S phalcon   #找到对应版本的phalcon扩展
#修改homebrew镜像源:
  #国内清华大学
  echo "export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles" >> ~/.bash_profile
  #中科大源
  echo 'export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles' >> ~/.bash_profile
  #立即生效
  source ~/.bash_profile
```

- **安装卸载软件**

```text
brew --version 或者 brew -v 显示brew版本信息
brew install <formula> 安装指定软件
brew unistall <formula 卸载指定软件
brew list  显示所有的已安装的软件
brew search text 搜索本地远程仓库的软件，已安装会显示绿色的勾
brew search /text/ 使用正则表达式搜软件
```

- **升级软件**

```text
brew update 自动升级homebrew（从github下载最新版本）
brew outdated 检测已经过时的软件
brew upgrade  升级所有已过时的软件，即列出的以过时软件
brew upgrade <formula>升级指定的软件
brew pin <formula> 禁止指定软件升级
brew unpin <formula> 解锁禁止升级
brew upgrade --all 升级所有的软件包，包括未清理干净的旧版本的包
```

- **清理相关**

homebrew 在升级软件时候不会清理相关的旧版本，在软件升级后可以使用如下命令清理：

```text
brew cleanup -n 列出需要清理的内容
brew cleanup <formula> 清理指定的软件过时包
brew cleanup 清理所有的过时软件
brew unistall <formula> 卸载指定软件
brew unistall <fromula> --force 彻底卸载指定软件，包括旧版本
```

### 可视化homebrew安装工具

```sh
brew cask install cakebrew
```

