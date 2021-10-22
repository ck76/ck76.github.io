[TOC]

## Github Actions

`Github Actions`是`Github`的持续集成服务，点击`Actions`在你Github上的项目上创建配置文件，实际也就是保存在`.github/workflows`下的以`.yml`结尾的文件。

### 1、配置文件的基本术语结构

（1）**workflow** （工作流程）：持续集成一次运行的过程，就是一个 workflow。

（2）**job** （任务）：一个 workflow 由一个或多个 jobs 构成，含义是一次持续集成的运行，可以完成多个任务。

（3）**step**（步骤）：每个 job 由多个 step 构成，一步步完成。

（4）**action** （动作）：每个 step 可以依次执行一个或多个命令（action）。

### 2、实例demo，将Github中的项目自动更新到云服务器

> Github存在一个[官方市场](https://link.segmentfault.com/?enc=N2rveSoA%2FFz8u2CoJ3OlmQ%3D%3D.N69PIPJyKupGOOLRaTWmgg7JPDjQPKtyDXGG31J1xbfQjgNxFW9k2e%2FCkYtjGINU)，搜索满足你需求的action就可以了。在`steps`配置`uses`来引用这个`action`的脚本。

```shell
name: Blog CI  # 配置名称

on: # 触发条件，master分支push代码后触发workflow
  push:
    branches: [ master ]

jobs:
  build:
    runs-on: ubuntu-latest # 构建运行环境
    steps:
    - name: Checkout  # 获取源码，使用actions/checkout@v2
      uses: actions/checkout@v2

    - name: Install Node.js # 安装指定Node版本，使用actions/setup-node@v1
      uses: actions/setup-node@v1
      with:
        node-version: '12.x'

    - name: Install & Build # 安装依赖打包静态资源
      run: |
        yarn config set registry https://registry.npm.taobao.org 
        yarn install
        yarn build

    - name: Deploy to Server # 部署到云服务器，使用easingthemes/ssh-deploy@v2.1.1，通过ssh的方式连接
      uses: easingthemes/ssh-deploy@v2.1.1
      env:
          SSH_PRIVATE_KEY: ${{ secrets.SSH_PRIVATE_KEY }}  # 私钥，公钥拷贝到服务器在/root/.ssh/authorized_keys中中

          ARGS: ${{ secrets.ARGS }} # 对于任何初始/必需的rsync标志，默认-avzr --delete，如果目录下有其他不可删除文件或文件夹可以用--exclude忽略，如--exclude /uploads/   
          SOURCE: "build/" # 源目录
          REMOTE_HOST: ${{ secrets.REMOTE_HOST }} # 服务器地址
          REMOTE_PORT: ${{ secrets.REMOTE_PORT }} # ssh连接端口号
          REMOTE_USER: root # 连接用户名
          TARGET: ${{ secrets.REMOTE_TARGET }} # 目标部署目录
```

### 3、敏感数据配置

因为部署到云服务器需要身份验证，相应的敏感数据不能直接暴露，Github中可以在项目`setting`中的`Secrets`设置相应的环境变量，然后通过`${{}}`的语法就可以访问相应的变量了。

## Nginx的安装与配置

因为我的服务器使用了`Nginx`这里简单做些记录

> Nginx是一个高性能的HTTP和反向代理web服务器，平时应用场景可以作为反向代理服务器，静态资源服务器，负载均衡等功能。安装使用以Linux为例，Windows和Mac可以直接下载安装包。

### 1、安装

```shell
yum install nginx -y # Centos 7.x直接使用yum安装即可
```

### 2、相关文件夹

使用`rpm -ql nginx`查看`Nginx`主要安装在什么地方，`/etc/nginx/nginx.conf`对应Nginx的主配置文件。

### 3、常用操作命令

```shell
nginx -s reload  # 向主进程发送信号，重新加载配置文件，热重启
nginx -s reopen     # 重启 Nginx
nginx -s stop    # 快速关闭
nginx -s quit    # 等待工作进程处理完成后关闭
nginx -T         # 查看当前 Nginx 最终的配置

systemctl enable nginx  # 使用系统管理命令设置Nginx开机启动
```

### 4、常用配置

#### 4.1 首先看下主配置文件`/etc/nginx/nginx.conf`的 基本结构。

```shell
main        # 全局配置，对全局生效
├── events  # Nginx服务器相关链接配置
|   ├── worker_connections 1024;# 默认最大并发连接数
├── http    # 配置代理，缓存，日志定义等绝大多数功能和第三方模块的配置
│   ├── upstream # 配置后端服务器具体地址，可以配置多个，也是负载均衡配置的地方
│   ├── server   # 配置虚拟主机的相关参数，一个 http 块中可以有多个 server 块
│   ├── server
│   │   ├── location  # 每个server可以包含多个location块，location用于匹配相应的uri
│   │   ├── location
│   │   └── ...
│   └── ...
└── ...
```

#### 4.2 一个相对完整的配置demo

```shell
#   For more information on configuration, see:
#   * Official English Documentation: http://nginx.org/en/docs/
#   * Official Russian Documentation: http://nginx.org/ru/docs/

user nginx;
worker_processes auto;
error_log /var/log/nginx/error.log;
pid /run/nginx.pid;

# Load dynamic modules. See /usr/share/doc/nginx/README.dynamic.
include /usr/share/nginx/modules/*.conf;

events {
    worker_connections 1024;
}

http {
    log_format  main  '$remote_addr - $remote_user [$time_local] "$request" '
                      '$status $body_bytes_sent "$http_referer" '
                      '"$http_user_agent" "$http_x_forwarded_for"';

    access_log  /var/log/nginx/access.log  main;

    sendfile            on;
    tcp_nopush          on;
    tcp_nodelay         on;
    keepalive_timeout   65;
    types_hash_max_size 2048;

    include             /etc/nginx/mime.types;
    default_type        application/octet-stream;

    # Load modular configuration files from the /etc/nginx/conf.d directory.
    # See http://nginx.org/en/docs/ngx_core_module.html#include
    # for more information.
    include /etc/nginx/conf.d/*.conf;   # 包括其它自定义配置

    server {
        listen 80; # 服务端口
        server_name www.example.com # 服务地址;
        rewrite ^(.*)$  https://$host$1 # $host$1变量对应上面的服务地址，配置了http访问时就重定向到https的地址;  
    }
    
# Settings for a TLS enabled server.

    server {
        listen       443 ssl http2 default_server;
        listen       [::]:443 ssl http2 default_server;
        server_name  _;
        root         /usr/share/nginx/html;

        #ssl证书配置
        ssl_certificate /etc/nginx/Nginx/ssl.crt; # 证书地址
        ssl_certificate_key /etc/nginx/Nginx/ssl.key; # 证书私钥
           ssl_session_timeout 10m;
        ssl_session_cache shared:SSL:1m;
        ssl_ciphers HIGH:!aNULL:!MD5;
        ssl_prefer_server_ciphers on;

        # Load configuration files for the default server block.
        include /etc/nginx/default.d/*.conf;

        location / {
            root /home/dist # 静态资源地址;
            index index.html;
            try_files  $uri $uri/ /index.html @rewrites; # 单页面history模式下的路由配置
        }

        # 其他静态资源、公共资源目录
        location /public {
          alias           /home/public;  # 静态资源目录
          autoindex             on;   # 开启静态资源列目录
          autoindex_exact_size  off;  # on(默认)显示文件的确切大小，单位是byte；off显示文件大概大小，单位KB、MB、GB
          autoindex_localtime   off;   # off(默认)时显示的文件时间为GMT时间；on显示的文件时间为服务器时间
       }

        location ~ /api/ {
          proxy_pass http://www.example.com:8080; # 相应接口转发的uri
       }

        error_page 404 /404.html;
            location = /40x.html {
        }

        error_page 500 502 503 504 /50x.html;
            location = /50x.html {
        }
    }
}
```

#### 4.3 其它配置

在`/etc/nginx/conf.d`目录下增加`Gzip`的配置

```shell
gzip on; # 默认off，是否开启gzip
gzip_types text/plain text/css application/json application/x-javascript text/xml application/xml application/xml+rss text/javascript;

gzip_static on; # 默认off，开启后会检查.gz结尾的压缩文件
gzip_proxied any; # 默认off，控制从代理服务器上接受压缩资源
gzip_vary on; # 响应头中增加 `Vary: Accept-Encoding`
gzip_comp_level 6; # gzip压缩比，压缩级别是1-9，1压缩级别最低，9最高，级别越高压缩率越大，压缩时间越长，建议4-6
gzip_buffers 16 8k; # 获取多少内存用于缓存压缩结果，16 8k 表示以 8k*16为单位获得
gzip_min_length 1k; # 允许需要压缩的最小资源大小
gzip_http_version 1.1; # 默认1.1，开启Gzip所需的最低HTTP版本
```

> 参考

- [GitHub Actions 入门教程](https://link.segmentfault.com/?enc=WN%2Fj9TBkdwk2yH1j8fqY2w%3D%3D.u9p1%2Bftz9nB%2B4ciOeWXI3XCiMVFzJRrBNDTUwf%2FwRpHFBbSlaOJI6TLJ5lTrmd2vXm8LKumNssQXAtjQy8lynRObVArVmvm3Gy1GM8rFeDU%3D)
- [Nginx 从入门到实践，万字详解！](https://link.segmentfault.com/?enc=8Evu%2B%2FqxFCDv6MiCFVx%2B1A%3D%3D.yqbJaq9HEH%2BY7ShsDYTfr%2FjJ%2BK92lWyKymu7A86Qg%2F021OjRBZRsT7KpDBUeZkRl)

- https://segmentfault.com/a/1190000022990551