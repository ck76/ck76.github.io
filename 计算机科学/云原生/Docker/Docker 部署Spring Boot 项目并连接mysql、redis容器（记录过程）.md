[TOC]

# [Docker 部署Spring Boot 项目并连接mysql、redis容器（记录过程）](https://www.cnblogs.com/ring2/p/12926359.html)



> **Content**
>
> [  Spring Boot 项目配置](https://www.cnblogs.com/ring2/p/12926359.html#spring-boot-项目配置)

## Spring Boot 项目配置

1. 将写好的Spring Boot 项目通过maven 进行package打包获得可执行Jar
2. 再src/main/docker（放哪都行）下编写创建Dockerfile文件并编写如下内容

> ```
> # 该镜像需要依赖的基础镜像
> FROM java:8
> # 将当前目录下的jar包复制到docker容器的/目录下
> ADD manage_analysis-0.0.1-SNAPSHOT.jar /manage_analysis.jar
> # 运行过程中创建一个mall-tiny-docker-file.jar文件
> RUN bash -c 'touch /manage_analysis.jar'
> # 声明服务运行在8089端口
> EXPOSE 8089
> # 指定docker容器启动时运行jar包
> ENTRYPOINT ["java", "-jar","/manage_analysis.jar"]
> # 指定维护者的名字
> MAINTAINER ring2
> ```

1. 将打包好的Jar包和Dockerfile 文件上传到云服务器或者虚拟机中的同一目录下（找个临时目录存放）

2. 请先确保docker 环境已经安装好并在该临时目录下执行如下命令

   > docker build -t analysis-manage:1.0 .

3. 获取mysql 5.7 版本的镜像

> docker pull mysql:5.7

1. 启动mysql 并配置root密码 并挂载相关目录

> docker run -p 3306:3306 --name mysql -v $PWD/conf:/etc/mysql/conf.d -v $PWD/logs:/logs -v $PWD/data:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=123456 -d mysq

1. 进入mysql 容器中并设置访问权限

> docker exec -it 62349aa31687 /bin/bash //进入mysql 容器
>
> mysql -uroot -p // 输入密码进入
>
> GRANT ALL ON *.* TO 'root'@'%'; // 允许所有终端访问
>
> flush privileges; // 刷新配置
>
> exit；
>
> exit; // 退出

1. 拉取redis镜像并启动redis

> docker pull redis
>
> docker run -p 6379:6379 --name redis -d redis

1. 接下来启动我们的Spring Boot 项目镜像

> run -p 8089:8089 --name analysis --link mysql:mysql --link redis:redis -d analysis-manage:1.0

 over！ 即可远程访问