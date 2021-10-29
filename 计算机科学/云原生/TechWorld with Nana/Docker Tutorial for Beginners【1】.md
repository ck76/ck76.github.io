[TOC]

- https://www.youtube.com/watch?v=jPdIRX6q4jA&list=PLy7NrYWoggjzfAHlUusx2wuDwfCrmJYcs



### What is Docker? Docker container concept explained || Docker Tutorial 1

```c
What is Docker? What is a Docker container? Understand why Docker is used and what problems do containers solve for the development process and deployment process?

► Subscribe to me on Youtube: https://bit.ly/2z5rvTV 

C o n t a i n e r   C o n c e p t    e x p l a i n e d 
Docker is becoming more and more popular and seems like every company is now turning to it to make the development and deployment process more efficient. The reason is that docker solves some common problems that have been around for quite a while in software development.

In this video I talk about container concepts and the above mentioned popular implementation called Docker. You will learn about what a container is, what problems it solves, how containers make the development process much easier and also how they solve some of the problems in the deployment process.  

▬▬▬▬▬▬ T I M E S T A M P S ⏰  ▬▬▬▬▬▬
0:00 - Intro
1:05 - What is a container and what problems does it solve?
1:50 - Container repository - where do containers live?
3:38 - Application development - before/after container
7:15 - Application deployment - before/after container
```

- Dockerhub

![image-20211027202731503](https://tva1.sinaimg.cn/large/008i3skNly1gvu54vgxm8j31nt0u0dj9.jpg)



- 容易部署

![image-20211027203212492](https://tva1.sinaimg.cn/large/008i3skNly1gvu59ov4pbj31ck0u00vy.jpg)



### What is a Docker Container? Docker Demo || Docker Tutorial 2

```c
What is a Docker container technically? This is a practical video of how to use Docker containers.

Docker image vs container
A container is made up of images, from base image which is mostly Linux Alpine to application images. In this video I explain docker images in more detail and then show you a practical example of how you can use and run a docker container on your local machine.


T i m e s t a m p s  🐳 
0:09 - What is a container technically? (layers of images)
1:16 - Demo part (docker hub and run a docker container locally)
  
The complete step-by-step guide to Docker and Kubernetes will include the following content:

🐳 D O C K E R
 - Container concept
 - Why docker? (image vs. traditional DevOps)
 - Install docker on different operating systems
 - 8 basic commands you need to know
 - Docker vs. Virtual Machine
 - Docker in Practice:  Overview of whole development process with Docker (development, continuous delivery, deployment)
      - Develop an application with Docker
      - Docker Compose
      - Dockerfile
      - Private Repository
      - Deploying your containerized application
- Docker Volumes from theory to practice
```

- 分层的

![image-20211027203316487](https://tva1.sinaimg.cn/large/008i3skNly1gvu5at2xr9j31n50u0tbe.jpg)



![image-20211027203542073](https://tva1.sinaimg.cn/large/008i3skNly1gvu5dbvh60j31y00qotdw.jpg)



- 分层的，所以在镜像更新的时候只需要下载更新的层，什么基础层不变的就不用更新了

![image-20211027203716363](https://tva1.sinaimg.cn/large/008i3skNly1gvu5eyyxm0j31wk0ai0ul.jpg)



![image-20211027204005658](https://tva1.sinaimg.cn/large/008i3skNly1gvu5hw7lrwj31td0u0grj.jpg)



![image-20211027204021270](https://tva1.sinaimg.cn/large/008i3skNly1gvu5i62uf8j31wg0b0di0.jpg)



### How to install docker? Step by Step || Docker Tutorial 3

跳过



### 8 Basic Docker Commands || Docker Tutorial 4

```c
In this video I show you some basic docker commands. In the beginning I explain the difference between docker image and container and go through the topics of version and tag in an docker image. The main part is the demo, where I show you how to use these basic docker commands.

T i m e s t a m p s  🐳 
 - 2:25 docker pull
 - 4:04 docker run
 - 4:40 docker ps
 - 5:06 docker run --options
 - 6:17 docker stop
 - 6:43 docker start
 - 10:08 - docker ports, docker port mapping
 - docker logs (in part 2)
 - docker exec -it (in part 2)
```

![image-20211027204328403](https://tva1.sinaimg.cn/large/008i3skNly1gvu5letjtlj31fg0u0n1f.jpg)



<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvu5m93jc2j30xi0u0gnr.jpg" alt="image-20211027204417068" style="zoom:50%;" />



![image-20211027204918811](https://tva1.sinaimg.cn/large/008i3skNly1gvu5rhzdjaj31hc0du0ws.jpg)

- Detach mode的话，docker run -d redis就好了

![image-20211027205115240](https://tva1.sinaimg.cn/large/008i3skNly1gvu5tij021j320g0okdkf.jpg)



<img src="https://tva1.sinaimg.cn/large/008i3skNly1gvu5vg1rcrj315t0u0dhi.jpg" alt="image-20211027205306801" style="zoom:50%;" />



![image-20211027205749395](https://tva1.sinaimg.cn/large/008i3skNly1gvu60csf76j32120ea77s.jpg)





### Debugging Docker Containers with docker exec and docker logs || Docker Tutorial 5

```c
Debugging Docker Containers with docker exec and docker logs 

Commands for troubleshooting a Docker container are very useful. If something goes wrong in the container, you want to see the logs of the Docker container or get inside the container and use the terminal there.

First part 1 ►  https://www.youtube.com/watch?v=xGn7c...

T i m e s t a m p s  🐳 
 - 1:56 docker logs
 - 5:28 docker exec -it
```



- docker logs containerID

![image-20211027210041758](https://tva1.sinaimg.cn/large/008i3skNly1gvu63bzuvtj31zw0r47ee.jpg)



- 本机运行过的container

```c
                                      chengkun02@B000000407241Y ~ % docker ps -a
CONTAINER ID   IMAGE                                             COMMAND                  CREATED              STATUS                      PORTS                                       NAMES
68628b179ce0   redis                                             "docker-entrypoint.s…"   About a minute ago   Exited (0) 3 seconds ago                                                crazy_turing
7f2f03e3865c   redis                                             "docker-entrypoint.s…"   7 minutes ago        Exited (1) 6 minutes ago                                                quizzical_spence
abc9f99a369c   hello-world                                       "/hello"                 13 minutes ago       Exited (0) 13 minutes ago                                               frosty_greider
884c564cde13   hello-world                                       "/hello"                 13 minutes ago       Exited (0) 13 minutes ago                                               eloquent_banach
3300846079f3   chengkun0804/github-actions-demo:master-f644797   "java -jar github-ac…"   6 days ago           Exited (255) 2 days ago     0.0.0.0:8080->8080/tcp                      github-actions-demo-container
f995278b4918   chengkun0804/github-actions-demo:master-f644797   "java -jar github-ac…"   6 days ago           Exited (130) 5 days ago                                                 sweet_feistel
ce447c7d7109   myredis:v1.1                                      "docker-entrypoint.s…"   11 days ago          Exited (0) 11 days ago                                                  upbeat_meninsky
f4ecec611d16   e7acf8ff0ebf                                      "docker-entrypoint.s…"   11 days ago          Exited (0) 11 days ago                                                  jolly_jennings
321140b16320   myredis:v1.1                                      "docker-entrypoint.s…"   11 days ago          Exited (0) 11 days ago                                                  competent_bardeen
8a8f3a894cbf   redis                                             "docker-entrypoint.s…"   11 days ago          Exited (255) 6 days ago     6379/tcp                                    determined_wiles
9aff10adb491   hello-world                                       "/hello"                 12 days ago          Exited (0) 12 days ago                                                  elated_jang
a4306c77cea4   hello-world                                       "/hello"                 12 days ago          Exited (0) 12 days ago                                                  kind_rhodes
8d46edeb31a0   redis                                             "docker-entrypoint.s…"   2 weeks ago          Created                                                                 redis
919168421afe   centos/mysql-56-centos7                           "container-entrypoin…"   2 weeks ago          Exited (255) 12 days ago    0.0.0.0:3306->3306/tcp, :::3306->3306/tcp   mysql56
```



![image-20211027210404130](https://tva1.sinaimg.cn/large/008i3skNly1gvu66u6el8j31xe0huaem.jpg)

- docker exec -it containerid /bin/bash

```sh
chengkun02@B000000407241Y ~ % docker exec -it dc3681b21a22 /bin/sh
# ls
# pwd
/data
# env
HOSTNAME=dc3681b21a22
REDIS_DOWNLOAD_SHA=5b2b8b7a50111ef395bf1c1d5be11e6e167ac018125055daa8b5c2317ae131ab
HOME=/root
TERM=xterm
PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
REDIS_DOWNLOAD_URL=http://download.redis.io/releases/redis-6.2.6.tar.gz
REDIS_VERSION=6.2.6
GOSU_VERSION=1.12
PWD=/data
# cd data
/bin/sh: 4: cd: can't cd to data
# ls
# cd /data
# pwd
/data
# ls
# 
```



### Overview of Workflow with Docker - Docker in Practice || Docker Tutorial 7

```c
Work with Docker from development to deployment. Get an overview of how to develop Docker Nodejs and MongoDB application. 

Once you've learnt the Docker basic concepts, it's important to see how Docker is actually used in practice or in real world development so to say.
In software development workflow you have this steps of development, continuous delivery until it gets deployed on some environment. It's important to see how Docker integrates in all those steps.
  🐳 D O C K E R
 - Container concept
 - Why docker? (image vs. traditional DevOps)
 - Install docker on different operating systems
 - 8 basic commands you need to know
 - Docker vs. Virtual Machine
 - Docker in Practice:  Overview of whole development process with Docker (development, continuous delivery, deployment)
      - Develop an application with Docker
      - Docker Compose
      - Dockerfile
      - Private Repository
      - Deploying your containerized application
- Docker Volumes from theory to practice
```

![image-20211027210851391](https://tva1.sinaimg.cn/large/008i3skNly1gvu6btphsvj31kn0u0jv7.jpg)



![image-20211027211328164](https://tva1.sinaimg.cn/large/008i3skNly1gvu6gmqnpvj31ig0u0ado.jpg)



### Developing with Docker - Docker in Practice || Docker Tutorial 8

- https://github.com/ck76/nana-js-docker-demo-app
- https://medium.com/zenofai/how-to-build-a-node-js-and-mongodb-application-with-docker-containers-15e535baabf5

```c
How to develop an application with Docker. This is a practical tutorial building a simple Javascript application with Node.js and MongoDB using Docker.

Once you've learnt the basic concepts, it's important to see how Docker is actually used in practice or in real world development so to say.
In this video I show you how to work with Docker containers when developing applications. So, this will be a demo of developing a simple UI with JavaScript and Nodejs in the backend and connecting it with a Docker Container with a Mongodb database

The code on gitlab: https://gitlab.com/nanuchi/techworld-...

▬▬▬▬▬▬ T I M E S T A M P S 🐳 
0:00 - Intro
0:14 - Pre-Requisites
0:55 - what we will do in this video
1:44 - 1st part: The JavaScript App (HTML, JavaScript Frontend, Node.js Backend)
3:30 - 2nd part: MongoDB and Mongo Express Set-Up with Docker
5:00 - Docker Network concept and demo (docker-compose uses docker network under the hood, in the next video(s) I will show you how to do it with docker-compose)
15:33 - 3rd part: Connect Node Server with MongoDB container
```



![image-20211027211352399](https://tva1.sinaimg.cn/large/008i3skNly1gvu6h1wvmaj31uy0sigq8.jpg)



![image-20211027211705267](https://tva1.sinaimg.cn/large/008i3skNly1gvu6kecdy5j31la0u0aci.jpg)



![image-20211027212225088](https://tva1.sinaimg.cn/large/008i3skNly1gvu6pxm7qfj30yh0u076j.jpg)



- docker内部的容器之间在一个网络，可以通过名字互相访问

![image-20211027213122960](https://tva1.sinaimg.cn/large/008i3skNly1gvu6z99junj314x0u0jtg.jpg)

- 外部浏览器通过localhost

![image-20211027213212266](https://tva1.sinaimg.cn/large/008i3skNly1gvu7049nw8j31bs0tgmzf.jpg)



- 参数都是在dockerhub看来的

- 启动MongoDB

![image-20211027215315054](https://tva1.sinaimg.cn/large/008i3skNly1gvu7m0ttahj31z20awmz0.jpg)

- 启动mongo-express

![image-20211027215459359](https://tva1.sinaimg.cn/large/008i3skNly1gvu7nu1nugj31zk0ge41q.jpg)



```javascript
let express = require('express');
let path = require('path');
let fs = require('fs');
let MongoClient = require('mongodb').MongoClient;
let bodyParser = require('body-parser');
let app = express();

app.use(bodyParser.urlencoded({
  extended: true
}));
app.use(bodyParser.json());

app.get('/', function (req, res) {
    res.sendFile(path.join(__dirname, "index.html"));
  });

app.get('/profile-picture', function (req, res) {
  let img = fs.readFileSync(path.join(__dirname, "images/profile-1.jpg"));
  res.writeHead(200, {'Content-Type': 'image/jpg' });
  res.end(img, 'binary');
});

// use when starting application locally
let mongoUrlLocal = "mongodb://admin:password@localhost:27017";

// use when starting application as docker container
let mongoUrlDocker = "mongodb://admin:password@mongodb";

// pass these options to mongo client connect request to avoid DeprecationWarning for current Server Discovery and Monitoring engine
let mongoClientOptions = { useNewUrlParser: true, useUnifiedTopology: true }

app.post('/update-profile', function (req, res) {
  let userObj = req.body;

  MongoClient.connect(mongoUrlLocal, mongoClientOptions, function (err, client) {
    if (err) throw err;

    let db = client.db('user-account');
    userObj['userid'] = 1;

    let myquery = { userid: 1 };
    let newvalues = { $set: userObj };

    db.collection("users").updateOne(myquery, newvalues, {upsert: true}, function(err, res) {
      if (err) throw err;
      client.close();
    });

  });
  // Send response
  res.send(userObj);
});

app.get('/get-profile', function (req, res) {
  let response = {};
  // Connect to the db
  MongoClient.connect(mongoUrlLocal, mongoClientOptions, function (err, client) {
    if (err) throw err;

    let db = client.db('user-account');

    let myquery = { userid: 1 };

    db.collection("users").findOne(myquery, function (err, result) {
      if (err) throw err;
      response = result;
      client.close();

      // Send response
      res.send(response ? response : {});
    });
  });
});

app.listen(3000, function () {
  console.log("app listening on port 3000!");
});
```







- ![image-20211027220044088](https://tva1.sinaimg.cn/large/008i3skNly1gvu7tt9lnkj31dy0u0dk4.jpg)

![image-20211027215954228](https://tva1.sinaimg.cn/large/008i3skNly1gvu7sxx3qpj31d70u00wy.jpg)



![image-20211027220013248](https://tva1.sinaimg.cn/large/008i3skNly1gvu7ta2czwj31rk0u00xm.jpg)



![image-20211027220139461](https://tva1.sinaimg.cn/large/008i3skNly1gvu7uspz8rj31gu0u017p.jpg)



- 本机

![image-20211028121925064](https://tva1.sinaimg.cn/large/008i3skNly1gvuwn9a0k7j31fb0u0773.jpg)



```sh
chengkun02@B000000407241Y ~ % docker ps
CONTAINER ID   IMAGE           COMMAND                  CREATED        STATUS          PORTS                      NAMES
3d37fb131d6c   mongo           "docker-entrypoint.s…"   3 hours ago    Up 11 minutes   0.0.0.0:27017->27017/tcp   mongodb
a2de4096cc8c   mongo-express   "tini -- /docker-ent…"   18 hours ago   Up 11 minutes   0.0.0.0:8081->8081/tcp     mongo-express
```





- 新建user-account数据库

![image-20211028154352103](https://tva1.sinaimg.cn/large/008i3skNly1gvv2jzqrqxj31cv0u0djm.jpg)

![image-20211028154401348](https://tva1.sinaimg.cn/large/008i3skNly1gvv2k560joj312k0u0whn.jpg)

- 插入一条数据

![image-20211028154332559](https://tva1.sinaimg.cn/large/008i3skNly1gvv2jn6x4yj31v40oc0vw.jpg)

```json
{
    _id: ObjectId('617a54741a6f5cf7ee272c9f'),
    userid: 1,
    email: 'cc@example.com',
    interests: 'haha',
    name: 'ck'
}
```





![image-20211028121940372](https://tva1.sinaimg.cn/large/008i3skNly1gvuwnim4w6j31ol0u047p.jpg)



- **有那么点问题呢……**

```javascript
app listening on port 3000!

get-profile mongoUrlDockermongodb://admin:password@mongodb:27017

/home/app/server.js:53

    if (err) throw err;

             ^


MongoTimeoutError: Server selection timed out after 30000 ms

    at Timeout.<anonymous> (/home/app/node_modules/mongodb/lib/core/sdam/topology.js:878:9)

    at listOnTimeout (internal/timers.js:549:17)

    at processTimers (internal/timers.js:492:7) {

  reason: Error: getaddrinfo ENOTFOUND mongodb

      at GetAddrInfoReqWrap.onlookup [as oncomplete] (dns.js:66:26) {

    name: 'MongoNetworkError',

    errorLabels: [ 'TransientTransactionError' ],

    [Symbol(mongoErrorContextSymbol)]: {}

  },

  [Symbol(mongoErrorContextSymbol)]: {}

}
```





- 这种会失败

![image-20211028175547118](https://tva1.sinaimg.cn/large/008i3skNly1gvv6d9yphpj30f207o0sx.jpg)

![image-20211028175728389](https://tva1.sinaimg.cn/large/008i3skNly1gvv6f0egh5j31gx0u0wha.jpg)



- 这种没问题

![image-20211028175622760](https://tva1.sinaimg.cn/large/008i3skNly1gvv6dv0u5mj30ca06i74g.jpg)

![image-20211028175645025](https://tva1.sinaimg.cn/large/008i3skNly1gvv6e9826bj31gx0u0ju7.jpg)

- 但是没问题这种手动启动就是有问题了……

  ``` c
  好鸡儿神奇的一个问题，自己在命令行里手动启动app就连接不到mongo
  docker run -d -p 3000:3000 2981f2cd7c8d
  要是写在compose里就是可以的
   my-app:
      image: my-app:docker
      ports:
        - 3000:3000
  ```



