

```c
Got permission denied while trying to connect to the Docker daemon socket at unix:///var/run/docker.sock: Get "http://%2Fvar%2Frun%2Fdocker.sock/v1.24/containers/json": dial unix /var/run/docker.sock: connect: permission denied
```



在用户权限下docker 命令需要 sudo 否则出现以下问题



通过将用户添加到docker用户组可以将sudo去掉，命令如下

sudo groupadd docker #添加docker用户组

sudo gpasswd -a $USER docker #将登陆用户加入到docker用户组中

newgrp docker #更新用户组

ubuntu18.04在重启后会生效，如果不是特别着急，可以先重启然后再做docker操作。

如果比较着急，可以使用置顶评论的方法

————————————————
版权声明：本文为CSDN博主「代码不好读啊」的原创文章，遵循CC 4.0 BY-SA版权协议，转载请附上原文出处链接及本声明。
原文链接：https://blog.csdn.net/u011337602/article/details/104541261

![image-20211029153231762](https://tva1.sinaimg.cn/large/008i3skNly1gvw7uhynjfj315k0pmq7r.jpg)



```sh
ubuntu@satisfying-fowl:~$ docker ps
Got permission denied while trying to connect to the Docker daemon socket at unix:///var/run/docker.sock: Get "http://%2Fvar%2Frun%2Fdocker.sock/v1.24/containers/json": dial unix /var/run/docker.sock: connect: permission denied
ubuntu@satisfying-fowl:~$ sudo groupadd docker
groupadd: group 'docker' already exists
ubuntu@satisfying-fowl:~$ sudo gpasswd -a $USER docker
Adding user ubuntu to group docker
ubuntu@satisfying-fowl:~$ newgrp docker
ubuntu@satisfying-fowl:~$ docker ps
CONTAINER ID   IMAGE                                               COMMAND                  CREATED          STATUS         PORTS                                           NAMES
8a8069c1cc0f   chengkun0804/nana-js-docker-demo-app:main-6764d55   "docker-entrypoint.s…"   23 minutes ago   Up 5 minutes   0.0.0.0:3000->3000/tcp, :::3000->3000/tcp       ubuntu_my-app_1
fa9a4743436d   mongo-express                                       "tini -- /docker-ent…"   23 minutes ago   Up 5 minutes   0.0.0.0:8080->8081/tcp, :::8080->8081/tcp       ubuntu_mongo-express_1
326cb5471be4   mongo                                               "docker-entrypoint.s…"   23 minutes ago   Up 5 minutes   0.0.0.0:27017->27017/tcp, :::27017->27017/tcp   ubuntu_mongodb_1
ubuntu@satisfying-fowl:~$ 
```

