[TOC]

# 基于Golang的开发框架Gin实战

[Gin中文文档](https://link.juejin.cn?target=https%3A%2F%2Fgin-gonic.com%2Fzh-cn%2Fdocs%2F)

学习Golang的过程中，虽然后端语言的大致思路是差不多的，但是在语言细节和语法上跟之前接触的Laravel, Node.js还是有区别的.多熟悉，多对比，更容易知道其所以然了。

## Gin 是什么？

Gin 是一个用 Go (Golang) 编写的 HTTP web 框架。类比与

```
Node.js`的web框架： `express, koa
PHP`的web框架：`laravel, lumen , yii
Python`的web框架 `Django , Flask
```

只要你之前接触过这些，这么一类比你肯定就秒懂了。

官网右上角可以切换语言

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvhhiia04oj31er0u0tcg.jpg)



## Gin安装

Golang开发环境参照我前面写的[基于Golang的微服务——上手篇](https://juejin.cn/post/6844903888567402504)文章配置就行

新建项目目录 `tech`, 我之前写Laravel的时候感觉代码和项目结构真的很优雅，就按照Laravel的项目结构来了。

```
tech
    -app
        -Controllers // 控制器函数
        -Middleware
        -Models //  数据模型
    -config // 配置文件
    -databse // 数据库
        -mysql.go
    -public // 静态资源目录
        -css
        -imgs
        -js
    -resources // 项目资源，比如放scss，通过编译转化成项目用的css
        -lang
        -sass
        -js
    -routes // 路由
        -web.go
    -views // 视图文件，这个项目会涉及到后端模板渲染和 API开发
        -usr
            -index.tmpl
        -post
            -index.tmpl
        -home
            -index.tmpl
    .env // 配置文件
    .gitignore 
    main.go //  入口文件
    README.md
复制代码
```

附一张项目结构图：



![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/7/15/16bf55471c44e283~tplv-t2oaga2asx-watermark.awebp)

项目包依赖用的[govender](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Fkardianos%2Fgovendor),具体用法我前面的文章有写到。也可以自己查查资料。



## 入口文件

main.go

```
package main

import (
	"os"
	_ "tech/config"
	_ "tech/database"
	"tech/routes"
)

func main()  {
	r := routes.InitRouter()
	port := os.Getenv("HTTP_PORT")
	r.Run(":" + port) // 监听并在 0.0.0.0:8080 上启动服务
}
复制代码
```

先贴下入口文件里的内容，然后根据入口文件从上到下讲解。

## 配置文件

```
_ tech/config
```

这里导入的是配置文件，`tech/config/config.go`文件源码如下：

```
package config

import (
	"github.com/joho/godotenv"
	"log"
	"os"
)

func init(){
	err := godotenv.Load() // 载入 godotenv
	if err != nil {
		log.Fatal("Error loading .env file")
	}

	PORT := os.Getenv("HTTP_PORT") //  获取.env配置文件里的HTTP_POTRT值
	log.Print(PORT)
}
复制代码
```

这里涉及到了一个 本地.env 文件读取的包[godotenv](https://link.juejin.cn?target=https%3A%2F%2Fgithub.com%2Fjoho%2Fgodotenv)，可以获取到项目里.env文件预设置的值

```
// 这里是 .env文件内容
HTTP_PORT=8090
AUTHOR=winyh
复制代码
```

这里通过简单的配置文件演示项目的一些配置，比如项目端口号，数据库开发环境的数据库信息，都可以放到.env 如果正式环境的数据库信息放在.env里就一定要注意了，把这个配置文件添加到.gitingore忽略文件里，避免上传到远程代码仓库了。

## 数据库文件

```
_ tech/database
```

这里是数据库链接功能的实现，`tech/database/mysql.go`源码如下：

```
package database

import (
	"fmt"
	_ "github.com/go-sql-driver/mysql" //加载mysql驱动
	"github.com/jinzhu/gorm"
	_ "github.com/jinzhu/gorm/dialects/mysql"
)

var DB *gorm.DB

func init()  {
	var err error
	DB, err = gorm.Open("mysql", "root:123456@tcp(127.0.0.1:3306)/tech?charset=utf8&parseTime=True&loc=Local&timeout=10ms")

	if err != nil {
		fmt.Printf("mysql connect error %v", err)
	}

	if DB.Error != nil {
		fmt.Printf("database error %v", DB.Error)
	}
}
复制代码
```

项目数据库选的是mysql，所以需要先安装mysql的驱动包

```
go get github.com/go-sql-driver/mysql
复制代码
```

写原生sql语句台痛苦了，所以引入一个ORM（简单粗暴的理解为一系列的原生sql 语句的封装，可以让你操作数据库更简便，会想起之前写Larvel，它自带的ORM用着是真 爽，基本上是要啥有啥）

```
go get github.com/jinzhu/gorm
复制代码
```

数据库的链接主要是下面这句，其中

root:是你本机的mysql用户名 123456: 是你本机的数据库密码 127.0.0.1:3306：表示数据库地址和端口号 tech:是你本机建立的数据库名称

```
DB, err = gorm.Open("mysql", "root:123456@tcp(127.0.0.1:3306)/tech?charset=utf8&parseTime=True&loc=Local&timeout=10ms")
复制代码
```

## 路由文件

```
tech/routes
```

入口文件里引入了routes这个包，`tech/routes/web.go`源码如下：

```
package routes

import (
	"github.com/gin-gonic/gin"
	"tech/app/Controllers"
)

func InitRouter() *gin.Engine {
	r := gin.Default()

	r.Static("/public", "./public") // 静态文件服务
	r.LoadHTMLGlob("views/**/*") // 载入html模板目录

	// web路由
	r.GET("/", Controllers.Home)
	r.GET("/about", Controllers.About)
	r.GET("/post/index", Controllers.Post)

	// 简单的路由组: v1
	v1 := r.Group("/api")
	{
		v1.GET("/ping", Controllers.Ping)
		v1.POST("/user/create", Controllers.UserCreate)
		v1.POST("/user/delete", Controllers.UserDestroy)
		v1.POST("/user/update", Controllers.UserUpdate)
		v1.POST("/users", Controllers.UserFindAll)
	}

	return r
}
复制代码
```

后端项目基本都是MVC的模式，微服务架构可能会更简单一点。路由是一个项目对外提供服务的入口，所有的前端请求从这里进入到指定的处理函数。处理函数会操作业务逻辑，返回数据和给出响应，完成一次请求。 这里引入的包`"github.com/gin-gonic/gin"`就是Gin框架的包。其次引入了我们自己编写的本地包`"tech/app/Controllers"`

我把这个路由文件里实现的功能分为了两类，一类是提供web服务的路由（也就是返回模板渲染后的html文件），一类是提供 Restful API 服务的路由。路由地址匹配我们的处理函数。

## 控制器

上面每个路由地址都关联有一个处理函数，我们把它叫做控制器。讲解其中的 `Controllers/Home.go`控制器内容，源码如下：

```
package Controllers

import (
	"github.com/gin-gonic/gin"
	"net/http"
)

func Home(c *gin.Context) {
	c.HTML(http.StatusOK, "home/index.tmpl", gin.H{
		"title": "这是首页",
	})
}
复制代码
```

基本逻辑是获取到请求的上下文，用Gin提供的c.HTML返回需要渲染返回到前台的数据。这里的`title`数据可以手动定义，也可以在数据库里查询到后再放入模板文件里渲染，这样看到的数据就是我们数据库里存放的数据了。

## 视图

视图是返回给前端的页面模板渲染文件，后端从数据库查询到的或者自定义的数据，都可以通过变量的方式渲染到模板里面，视图从后端到前端可见会经历两次渲染，第一次是后端把数据填充进去，然后用模板引擎把模板文件渲染成html,发送到前端的时候，浏览器识别到 content-Type:'text/html'，再次通过浏览器的渲染，把html标签渲染成用户可见的界面。第一次渲染是在服务器渲染的，第二次渲染是在前端渲染的。参照这里你应该可以理解前端常说的为了做SEO优化，需要用Node.js做服务端渲染是什么个意思可，可以参照我之前写的[服务端渲染](https://juejin.cn/post/6844903850646700045)文章。 这里贴一下 `views/home/index.tmpl` 模板文件的源码：

```
{{ define "home/index.tmpl" }}
    <html>
        <head>
            <meta charset="UTF-8">
            <title>Gin 框架测试</title>
        </head>

        <body>
        <div class="container">
            <h1>这是一个测试页面</h1>
            <p>{{ .title }}</p>
        </div>
        </body>
    </html>
{{ end }}
复制代码
```

再列举一个 API服务的控制器，`Controllers/Api.go`源码如下：

```
package Controllers

import (
	"fmt"
	"github.com/gin-gonic/gin"
	"github.com/jinzhu/gorm"
	"net/http"
	"tech/app/Models"
)

type Admins struct {
	gorm.Model
	Name string `json:"name"  binding:"required"`
	Password string `json:"password"  binding:"required"`
	Mobile string `json:"mobile" binding:"required"`
}

func Ping(c *gin.Context) {
	c.JSON(200, gin.H{
		"message": "pong",
	})
}

func UserCreate(c *gin.Context) {
	var json  Models.Admins //  定义json 变量 数据结构类型 为 Models.Admins
	err := c.BindJSON(&json) //  获取前台传过来的 json数据

	if err != nil {
		fmt.Printf("mysql connect error %v", err)
	}

	id, err := json.Insert()

	if err != nil {
		fmt.Printf("database error %v", err)
		fmt.Printf("database error %v", id)
		return
	}

	c.JSON(200, gin.H{ // 反馈给前台的信息，同时返回最新创建的一条数据的Id
		"status": true,
		"id":id,
		"message":"创建成功",
	})
}

func UserDestroy(c *gin.Context)  {
	var json  Models.Admins
	err := c.BindJSON(&json)
	if err != nil {
		fmt.Printf("mysql connect error %v", err)
		return
	}

	json.Destroy()

	c.JSON(200, gin.H{
		"status": true,
		"message":"删除成功",
	})
}

func UserUpdate(c *gin.Context) {
	var json  Models.Admins
	err := c.BindJSON(&json)

	if err != nil {
		fmt.Printf("mysql connect error %v", err)
	}

	id, err := json.Update(3)

	if err != nil {
		fmt.Printf("database error %v", err)
		fmt.Printf("database error %v", id)
		return
	}

	c.JSON(200, gin.H{
		"status": true,
		"id":id,
		"message":"更新成功",
	})
}

func UserFindAll(c *gin.Context)  {
	var json  Models.Admins
	err := c.BindJSON(&json)

	if err != nil {
		fmt.Printf("mysql connect error %v", err)
	}

	result, err := json.FindAll()

	if err != nil {
		c.JSON(http.StatusOK, gin.H{
			"status":  false,
			"message": "抱歉未找到相关信息",
		})
		return
	}

	c.JSON(200, gin.H{
		"status": true,
		"data": result,
		"message":"查询成功",
	})
}
复制代码
```

这个文件里完成了后端基本的增删改查功能。内容相对多点，列举其中的用户新建来说一下： UserCreate 函数会接收到前台传过来的json数据，然后 调用 `id, err := json.Insert()` 做数据入库的操作。这里涉及到 Insert函数，也就是接下来要降到的数据模型

## 数据模型

数据模型是数据的抽象，可以提供一些数据操作的函数封装。 `app/Models/admin.go` 源码如下：

```
package Models

import (
	"github.com/jinzhu/gorm"
	. "tech/database"
)

type Admins struct {
	gorm.Model  // 这里的配置可以让ORM 自动维护 时间戳字段，很爽有木有
	Name string `json:"name"  binding:"required"`
	Password string `json:"password"  binding:"required"`
	Mobile string `json:"mobile" binding:"required"`
}

// Insert 新增admin用户
func (admin *Admins) Insert() (userID uint, err error) {

	result := DB.Create(&admin) //  这里的DB变量是 database 包里定义的，Create 函数是 gorm包的创建数据API
	userID = admin.ID
	if result.Error != nil {
		err = result.Error
	}
	return  // 返回新建数据的id 和 错误信息，在控制器里接收
}

// Destroy 删除admin用户
func (admin *Admins) Destroy() (err error) {

	result := DB.Delete(&admin)
	if result.Error != nil {
		err = result.Error
	}
	return
}


// Update 修改admin用户
func (admin *Admins) Update(id int64) (user Admins, err error) {
	result := DB.Model(&admin).Where("id = ?", id).Updates(&admin)
	if result.Error != nil {
		err = result.Error
	}
	return
}


// FindOne 查询admin用户
func (admin *Admins) FindAll() (admins []Admins, err error) {

	result := DB.Find(&admins) // 这里的 &admins 跟返回参数要一致

	if result.Error != nil {
		err = result.Error
		return
	}
	return
}
复制代码
```

在项目根目录下运行

```
go run main.go
复制代码
```

就可以在本地访问配置的路由了

```
localhost:8090/api/ping
复制代码
```

项目最后放上几个测试截图

控制台启动信息



![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/7/15/16bf55903753eb51~tplv-t2oaga2asx-watermark.awebp)

IDE控制台里的红色输出不是错误信息哦！会把我们项目路由和加载的模板文件都打印出来。



文章页面



![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/7/15/16bf55e5435251fa~tplv-t2oaga2asx-watermark.awebp)



用户数据查询，我这里用的是POST方法，查询接口建议大家遵循Restful Api 规则，用GET方法



![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/7/15/16bf5606366f9c64~tplv-t2oaga2asx-watermark.awebp)



数据新增，返回了当前新增数据的ID,前端端基于JSON数据格式传输交互



![img](https://p1-jj.byteimg.com/tos-cn-i-t2oaga2asx/gold-user-assets/2019/7/15/16bf562e00346389~tplv-t2oaga2asx-watermark.awebp)



项目里的基本逻辑就是这样了，突然感觉写文章还是很浪费时间的，自己写的质量也很低，权当跟大家交流吧，把自己的缺点暴露出来，有人指点交流，能学到东西就是进步。

反思了下自己，以前基本是什么技术都想学，后来发现这样不现实，要找准方向，将精力集中，让时间和努力发挥出它最大的价值而不至于分散了。

后面自己还是主攻前端技术，深入研究Golang 和 Node.js ,欢迎大家一起交流。Linux应该算是基础知识了，希望在这个平台可以认识更多优秀的朋友。

再仔细的反思了下自己，技术栈深度不够，以后太基础的文章我可能只会自己做笔记不会写出来了，质量太低浪费了自己精力，也浪费了点击进来看的人时间，时间是多么宝贵的东西啊，于心不忍。看到一篇很不错的文章。

总结几点我很认同的观点：

- 要去经历大多数人经历不到的，要把学习时间花在那些比较难的地方。
- 要写文章就要写没有人写过的，或是别人写过，但我能写得更好的。
- 更重要的是，技术和知识完全是可以变现的。
- 最好的 SEO 就是独一份，物以稀为贵。
- 努力只是成功的必要条件之一，努力就会成功这句话得多思考下。

**最宝贵的财富并不是钱，而是你的时间。**

**25~35 岁是每个人最宝贵的时光，应该用在刀刃上。**

**25~35 岁是每个人最宝贵的时光，应该用在刀刃上。**

**25~35 岁是每个人最宝贵的时光，应该用在刀刃上。**

**不会交流的计算机人员30岁以后一般会遇到很多瓶颈**

原文链接[左耳朵耗子：程序员如何用技术变现?](https://link.juejin.cn?target=https%3A%2F%2Fzhuanlan.zhihu.com%2Fp%2F30477400) 博主博客：[酷壳](https://link.juejin.cn?target=http%3A%2F%2Fwww.coolshell.cn)


作者：winyh
链接：https://juejin.cn/post/6844903889112662029
来源：稀土掘金
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。