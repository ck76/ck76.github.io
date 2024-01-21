```sh
#f. ~/.bashrc
#Setting PATH for android Adb Tools 2018-12-7 ck

#加载顺序
#a. /etc/profile
#b. /etc/paths
#c. ~/.bash_profile
#d. ~/.bash_login
#e. ~/.profile
#f. ~/.bashrc

#HOME
export HOME=/Users/chengkun

#mysql
export PATH=$PATH:/usr/local/mysql/bin

#Android
export PATH=${PATH}:$HOME/Library/Android/sdk/platform-tools
export PATH=${PATH}:$HOME/Library/Android/sdk/tools
export PATH=${PATH}:$HOME/Library/Android/gradle/wrapper/dists/gradle-4.6-all/bcst21l2brirad8k2ben1letg/gradle-4.6/bin
export PATH=${PATH}:$HOME/workspace/flutter/bin

#flutter
"~/.bash_profile" 32L, 791C
```

