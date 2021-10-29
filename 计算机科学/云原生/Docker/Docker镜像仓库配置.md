



1、修改配置文件 vi    /etc/docker/daemon.json

  添加内容：

```json
 {
        "registry-mirrors": ["https://registry.docker-cn.com",
        "https://xwx6wxd1.mirror.aliyuncs.com",
        "http://hub-mirror.c.163.com",
        "https://3laho3y3.mirror.aliyuncs.com",
        "http://f1361db2.m.daocloud.io",
        "https://mirror.ccs.tencentyun.com"]
  }
```

```json
{
  "builder": {
    "gc": {
      "defaultKeepStorage": "20GB",
      "enabled": true
    }
  },
  "experimental": false,
  "features": {
    "buildkit": true
  },
  "registry-mirrors": [
    "https://1nj0zren.mirror.aliyuncs.com",
    "https://docker.mirrors.ustc.edu.cn",
    "http://f1361db2.m.daocloud.io",
    "https://registry.docker-cn.com"
  ]
}
```



2.创建并修改完daemon.json文件后，需要让这个文件生效
       修改完成后reload配置文件

             sudo systemctl daemon-reload
    
       重启docker服务
    
            sudo systemctl restart   docker.service
    
       查看状态
    
            sudo systemctl status docker -l
    
        查看服务
    
             sudo docker info
————————————————
版权声明：本文为CSDN博主「devil2012」的原创文章，遵循CC 4.0 BY-SA版权协议，转载请附上原文出处链接及本声明。
原文链接：https://blog.csdn.net/devil2012/article/details/90596993