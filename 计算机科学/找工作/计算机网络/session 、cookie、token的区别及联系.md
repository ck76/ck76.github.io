## session

 session的中文翻译是“会话”，当用户打开某个web应用时，便与web服务器产生一次session。服务器使用session把用户的信息临时保存在了服务器上，用户离开网站后session会被销毁。这种用户信息存储方式相对cookie来说更安全，可是session有一个缺陷：如果web服务器做了负载均衡，那么下一个操作请求到了另一台服务器的时候session会丢失。

 

## cookie

 cookie是保存在本地终端的数据。cookie由服务器生成，发送给浏览器，浏览器把cookie以kv形式保存到某个目录下的文本文件内，下一次请求同一网站时会把该cookie发送给服务器。由于cookie是存在客户端上的，所以浏览器加入了一些限制确保cookie不会被恶意使用，同时不会占据太多磁盘空间，所以每个域的cookie数量是有限的。

   cookie的组成有：名称(key)、值(value)、有效域(domain)、路径(域的路径，一般设置为全局:"\")、失效时间、安全标志(指定后，cookie只有在使用SSL连接时才发送到服务器(https))。下面是一个简单的js使用cookie的例子:

用户登录时产生cookie:

document.cookie = "id="+result.data['id']+"; path=/";

document.cookie = "name="+result.data['name']+"; path=/";

document.cookie = "avatar="+result.data['avatar']+"; path=/";

使用到cookie时做如下解析：

var cookie = document.cookie;var cookieArr = cookie.split(";");var user_info = {};for(var i = 0; i < cookieArr.length; i++) {

  user_info[cookieArr[i].split("=")[0]] = cookieArr[i].split("=")[1];

}

$('#user_name').text(user_info[' name']);

$('#user_avatar').attr("src", user_info[' avatar']);

$('#user_id').val(user_info[' id']);

 

## token

   token的意思是“令牌”，是用户身份的验证方式，最简单的token组成:uid(用户唯一的身份标识)、time(当前时间的时间戳)、sign(签名，由token的前几位+盐以哈希算法压缩成一定长的十六进制字符串，可以防止恶意第三方拼接token请求服务器)。还可以把不变的参数也放进token，避免多次查库

##  cookie 和session的区别

1、cookie数据存放在客户的浏览器上，session数据放在服务器上。

2、cookie不是很安全，别人可以分析存放在本地的COOKIE并进行COOKIE欺骗
  考虑到安全应当使用session。

3、session会在一定时间内保存在服务器上。当访问增多，会比较占用你服务器的性能
  考虑到减轻服务器性能方面，应当使用COOKIE。

4、单个cookie保存的数据不能超过4K，很多浏览器都限制一个站点最多保存20个cookie。

5、所以个人建议：
  将登陆信息等重要信息存放为SESSION
  其他信息如果需要保留，可以放在COOKIE中

## token 和session 的区别

  session 和 oauth token并不矛盾，作为身份认证 token安全性比session好，因为每个请求都有签名还能防止监听以及重放攻击，而session就必须靠链路层来保障通讯安全了。如上所说，如果你需要实现有状态的会话，仍然可以增加session来在服务器端保存一些状态

  App通常用restful api跟server打交道。Rest是stateless的，也就是app不需要像browser那样用cookie来保存session,因此用session token来标示自己就够了，session/state由api server的逻辑处理。 如果你的后端不是stateless的rest api, 那么你可能需要在app里保存session.可以在app里嵌入webkit,用一个隐藏的browser来管理cookie session.

 

  Session 是一种HTTP存储机制，目的是为无状态的HTTP提供的持久机制。所谓Session 认证只是简单的把User 信息存储到Session 里，因为SID 的不可预测性，暂且认为是安全的。这是一种认证手段。 而Token ，如果指的是OAuth Token 或类似的机制的话，提供的是 认证 和 授权 ，认证是针对用户，授权是针对App 。其目的是让 某App有权利访问 某用户 的信息。这里的 Token是唯一的。不可以转移到其它 App上，也不可以转到其它 用户 上。 转过来说Session 。Session只提供一种简单的认证，即有此 SID，即认为有此 User的全部权利。是需要严格保密的，这个数据应该只保存在站方，不应该共享给其它网站或者第三方App。 所以简单来说，如果你的用户数据可能需要和第三方共享，或者允许第三方调用 API 接口，用 Token 。如果永远只是自己的网站，自己的 App，用什么就无所谓了。

```
 token就是令牌，比如你授权（登录）一个程序时，他就是个依据，判断你是否已经授权该软件；cookie就是写在客户端的一个txt文件，里面包括你登录信息之类的，这样你下次在登录某个网站，就会自动调用cookie自动登录用户名；session和cookie差不多，只是session是写在服务器端的文件，也需要在客户端写入cookie文件，但是文件里是你的浏览器编号.Session的状态是存储在服务器端，客户端只有session id；而Token的状态是存储在客户端。
```



\------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

本文分别对Cookie与Session做一个介绍和总结，并分别对两个知识点进行对比分析，让大家对Cookie和Session有一个更深入的了解，并对自己的开发工作中灵活运用带来启示。　　

## cookie机制

Cookies是服务器在本地机器上存储的小段文本并随每一个请求发送至同一个服务器。IETF RFC 2965 HTTP State Management Mechanism 是通用cookie规范。网络服务器用HTTP头向客户端发送cookies，在客户终端，浏览器解析这些cookies并将它们保存为一个本地文件，它会自动将同一服务器的任何请求缚上这些cookies 。

具体来说cookie机制采用的是在客户端保持状态的方案。它是在用户端的会话状态的存贮机制，他需要用户打开客户端的cookie支持。cookie的作用就是为了解决HTTP协议无状态的缺陷所作的努力。
正统的cookie分发是通过扩展HTTP协议来实现的，服务器通过在HTTP的响应头中加上一行特殊的指示以提示浏览器按照指示生成相应的cookie。然而纯粹的客户端脚本如JavaScript也可以生成cookie。而cookie的使用是由浏览器按照一定的原则在后台自动发送给服务器的。浏览器检查所有存储的cookie，如果某个cookie所声明的作用范围大于等于将要请求的资源所在的位置，则把该cookie附在请求资源的HTTP请求头上发送给服务器。

cookie的内容主要包括：名字，值，过期时间，路径和域。路径与域一起构成cookie的作用范围。若不设置过期时间，则表示这个cookie的生命期为浏览器会话期间，关闭浏览器窗口，cookie就消失。这种生命期为浏览器会话期的cookie被称为会话cookie。会话cookie一般不存储在硬盘上而是保存在内存里，当然这种行为并不是规范规定的。若设置了过期时间，浏览器就会把cookie保存到硬盘上，关闭后再次打开浏览器，这些cookie仍然有效直到超过设定的过期时间。存储在硬盘上的cookie可以在不同的浏览器进程间共享，比如两个IE窗口。而对于保存在内存里的cookie，不同的浏览器有不同的处理方式。

而session机制采用的是一种在服务器端保持状态的解决方案。同时我们也看到，由于采用服务器端保持状态的方案在客户端也需要保存一个标识，所以session机制可能需要借助于cookie机制来达到保存标识的目的。而session提供了方便管理全局变量的方式 。

session是针对每一个用户的，变量的值保存在服务器上，用一个sessionID来区分是哪个用户session变量,这个值是通过用户的浏览器在访问的时候返回给服务器，当客户禁用cookie时，这个值也可能设置为由get来返回给服务器。

就安全性来说：当你访问一个使用session 的站点，同时在自己机子上建立一个cookie，建议在服务器端的session机制更安全些，因为它不会任意读取客户存储的信息。



## session机制

session机制是一种服务器端的机制，服务器使用一种类似于散列表的结构（也可能就是使用散列表）来保存信息。

**当程序需要为某个客户端的请求创建一个session时，服务器首先检查这个客户端的请求里是否已包含了一个session标识（称为session id），如果已包含则说明以前已经为此客户端创建过session，服务器就按照session id把这个session检索出来使用（检索不到，会新建一个），如果客户端请求不包含session id，则为此客户端创建一个session并且生成一个与此session相关联的session id，session id的值应该是一个既不会重复，又不容易被找到规律以仿造的字符串，这个session id将被在本次响应中返回给客户端保存。**

**保存这个session id的方式可以采用cookie**，这样在交互过程中浏览器可以自动的按照规则把这个标识发挥给服务器。一般这个cookie的名字都是类似于SEEESIONID。但cookie可以被人为的禁止，则必须有其他机制以便在cookie被禁止时仍然能够把session id传递回服务器。
经常被使用的一种技术叫做URL重写，就是把session id直接附加在URL路径的后面。还有一种技术叫做表单隐藏字段。就是服务器会自动修改表单，添加一个隐藏字段，以便在表单提交时能够把session id传递回服务器。

Cookie与Session都能够进行会话跟踪，但是完成的原理不太一样。普通状况下二者均能够满足需求，但有时分不能够运用Cookie，有时分不能够运用Session。下面经过比拟阐明二者的特性以及适用的场所。

1 .存取方式的不同

Cookie中只能保管ASCII字符串，假如需求存取Unicode字符或者二进制数据，需求先进行编码。Cookie中也不能直接存取Java对象。若要存储略微复杂的信息，运用Cookie是比拟艰难的。
而Session中能够存取任何类型的数据，包括而不限于String、Integer、List、Map等。Session中也能够直接保管Java Bean乃至任何Java类，对象等，运用起来十分便当。能够把Session看做是一个Java容器类。

2 .隐私策略的不同

Cookie存储在客户端阅读器中，对客户端是可见的，客户端的一些程序可能会窥探、复制以至修正Cookie中的内容。而Session存储在服务器上，对客户端是透明的，不存在敏感信息泄露的风险。
假如选用Cookie，比较好的方法是，敏感的信息如账号密码等尽量不要写到Cookie中。最好是像Google、Baidu那样将Cookie信息加密，提交到服务器后再进行解密，保证Cookie中的信息只要本人能读得懂。而假如选择Session就省事多了，反正是放在服务器上，Session里任何隐私都能够有效的保护。
3.有效期上的不同

使用过Google的人都晓得，假如登录过Google，则Google的登录信息长期有效。用户不用每次访问都重新登录，Google会持久地记载该用户的登录信息。要到达这种效果，运用Cookie会是比较好的选择。只需要设置Cookie的过期时间属性为一个很大很大的数字。

由于Session依赖于名为JSESSIONID的Cookie，而Cookie JSESSIONID的过期时间默许为–1，只需关闭了阅读器该Session就会失效，因而Session不能完成信息永世有效的效果。运用URL地址重写也不能完成。而且假如设置Session的超时时间过长，服务器累计的Session就会越多，越容易招致内存溢出。

4.服务器压力的不同

Session是保管在服务器端的，每个用户都会产生一个Session。假如并发访问的用户十分多，会产生十分多的Session，耗费大量的内存。因而像Google、Baidu、Sina这样并发访问量极高的网站，是不太可能运用Session来追踪客户会话的。

而Cookie保管在客户端，不占用服务器资源。假如并发阅读的用户十分多，Cookie是很好的选择。关于Google、Baidu、Sina来说，Cookie或许是唯一的选择。

5 .浏览器支持的不同

Cookie是需要客户端浏览器支持的。假如客户端禁用了Cookie，或者不支持Cookie，则会话跟踪会失效。关于WAP上的应用，常规的Cookie就派不上用场了。

假如客户端浏览器不支持Cookie，需要运用Session以及URL地址重写。需要注意的是一切的用到Session程序的URL都要进行URL地址重写，否则Session会话跟踪还会失效。关于WAP应用来说，Session+URL地址重写或许是它唯一的选择。

假如客户端支持Cookie，则Cookie既能够设为本浏览器窗口以及子窗口内有效（把过期时间设为–1），也能够设为一切阅读器窗口内有效（把过期时间设为某个大于0的整数）。但Session只能在本阅读器窗口以及其子窗口内有效。假如两个浏览器窗口互不相干，它们将运用两个不同的Session。（IE8下不同窗口Session相干）

6.跨域支持上的不同

Cookie支持跨域名访问，例如将domain属性设置为“.biaodianfu.com”，则以“.biaodianfu.com”为后缀的一切域名均能够访问该Cookie。跨域名Cookie如今被普遍用在网络中，例如Google、Baidu、Sina等。而Session则不会支持跨域名访问。Session仅在他所在的域名内有效。
仅运用Cookie或者仅运用Session可能完成不了理想的效果。这时应该尝试一下同时运用Cookie与Session。Cookie与Session的搭配运用在实践项目中会完成很多意想不到的效果。 

**关系的理解**

**客户第一次发送请求给服务器，此时服务器产生一个唯一的sessionID，并返回给客户端(通过cookie)，保存于客户端的内存中，并与一个浏览器窗口对应着,由于HTTP协议的特性，这一次连接就断开了**

**以后此客户端再发送请求给服务器的时候，就会在请求request中携带cookie,由于cookie中有sessionID,所以服务器就知道这是刚才那个客户端。**

 

举个简单例子就像人们去超市购物，去存包，第一个去的时候(客户第一次发送请求给服务器),超市会给你一个号码牌(此时服务器产生一个唯一的sessionID，并返回给客户端(通过cookie)),你可以在你自己的柜子里存东西(在服务器属于此客户的内存区域存数据),下次你再去的时候，拿着这个号码牌(请求request中携带cookie),超市就知道哪些东西是你的，然后给你取出来，如果你几天都没去取（session失效了，在服务器端配置）,你再去的时候东西就拿不到了
如果你把这个号码牌丢了(刚才的cookie失效了，比如你重启电脑，刚才存于内存中sessionID也就丢了)，再去拿东西，当然无法定位了，也就拿不到东西了
如果是新打开一个浏览器的情况，那就像是又一个人去超市存东西一样，你的东西跟他的东西是两码事，互不影响,他有他自己的sessionID,你有你自己的





---

#### 什么是Cookie？

 Cookie 技术产生源于 HTTP 协议在互联网上的急速发展。随着互联网时代的策马奔腾，带宽等限制不存在了，人们需要更复杂的互联网交互活动，**就必须同服务器保持活动状态（简称：保活）**。于是，在浏览器发展初期，为了适应用户的需求**技术上推出了各种保持 Web 浏览状态的手段**，其中就包括了 Cookie 技术。Cookie 在计算机中是个存储在浏览器目录中的文本文件，当浏览器运行时，存储在 RAM 中发挥作用 （此种 Cookies 称作 Session Cookies），一旦用户从该网站或服务器退出，Cookie 可存储在用户本地的硬盘上 （此种 Cookies 称作 Persistent Cookies）。

**Cookie 起源**：1993 年，网景公司雇员 Lou Montulli 为了让用户在访问某网站时，进一步提高访问速度，同时也为了进一步实现个人化网络，发明了今天广泛使用的 Cookie。（所以，适当的偷懒也会促进人类计算机发展史的一小步~）

**Cookie时效性**：目前有些 Cookie 是临时的，有些则是持续的。临时的 Cookie 只在浏览器上保存一段规定的时间，一旦超过规定的时间，该 Cookie 就会被系统清除。

**Cookie使用限制**：Cookie 必须在 HTML 文件的内容输出之前设置；不同的浏览器 (Netscape Navigator、Internet Explorer) 对 Cookie 的处理不一致，使用时一定要考虑；客户端用户如果设置禁止 Cookie，则 Cookie 不能建立。 并且在客户端，一个浏览器能创建的 Cookie 数量最多为 300 个，并且每个不能超过 4KB，每个 Web 站点能设置的 Cookie 总数不能超过 20 个。



### Session :

Session是对于服务端来说的，客户端是没有Session一说的。Session是服务器在和客户端建立连接时添加客户端连接标志，最终会在服务器软件（Apache、Tomcat、JBoss）转化为一个临时Cookie发送给给客户端，当客户端第一请求时服务器会检查是否携带了这个Session（临时Cookie），如果没有则会添加Session，如果有就拿出这个Session来做相关操作。



### Token :

token是用户身份的验证方式，我们通常叫它：令牌。最简单的token组成:uid(用户唯一的身份标识)、time(当前时间的时间戳)、sign(签名，由token的前几位+盐以哈希算法压缩成一定长的十六进制字符串，可以防止恶意第三方拼接token请求服务器)。还可以把不变的参数也放进token，避免多次查库。

应用场景：

A：当用户首次登录成功（注册也是一种可以适用的场景）之后, 服务器端就会生成一个 token 值，这个值，会在服务器保存token值(保存在数据库中)，再将这个token值返回给客户端.

B：客户端拿到 token 值之后,进行本地保存。（SP存储是大家能够比较支持和易于理解操作的存储）

C：当客户端再次发送网络请求(一般不是登录请求)的时候,就会将这个 token 值附带到参数中发送给服务器.

D：服务器接收到客户端的请求之后,会取出token值与保存在本地(数据库)中的token值做对比

对比一：**如果两个 token 值相同， 说明用户登录成功过!当前用户处于登录状态!**

对比二：如果没有这个 token 值, 则说明没有登录成功.

对比三：如果 token 值不同: 说明原来的登录信息已经失效,让用户重新登录.



**Cookie和Session的区别：**

1、cookie数据存放在客户的浏览器上，session数据放在服务器上。

2、cookie不是很安全，别人可以分析存放在本地的cookie并进行cookie欺骗,考虑到安全应当使用session。

3、session会在一定时间内保存在服务器上。当访问增多，会比较占用你服务器的性能,考虑到减轻服务器性能方面，应当使用cookie。

4、单个cookie保存的数据不能超过4K，很多浏览器都限制一个站点最多保存20个cookie。

5、所以个人建议：

将登陆信息等重要信息存放为session

其他信息如果需要保留，可以放在cookie中

**Token 和 Session 的区别：**

session和 token并不矛盾，作为身份认证token安全性比session好，因为每个请求都有签名还能防止监听以及重放攻击，而session就必须靠链路层来保障通讯安全了。如上所说，如果你需要实现有状态的会话，仍然可以增加session来在服务器端保存一些状态

App通常用restful api跟server打交道。Rest是stateless的，也就是app不需要像browser那样用cookie来保存session,因此用session token来标示自己就够了，session/state由api server的逻辑处理。如果你的后端不是stateless的rest api,那么你可能需要在app里保存session.可以在app里嵌入webkit,用一个隐藏的browser来管理cookie session.

Session是一种HTTP存储机制，目的是为无状态的HTTP提供的持久机制。所谓Session认证只是简单的把User信息存储到Session里，因为SID的不可预测性，暂且认为是安全的。这是一种认证手段。而Token，如果指的是OAuth Token或类似的机制的话，提供的是 认证 和 授权 ，认证是针对用户，授权是针对App。其目的是让 某App有权利访问 某用户 的信息。这里的Token是唯一的。不可以转移到其它App上，也不可以转到其它 用户 上。转过来说Session。Session只提供一种简单的认证，即有此SID，即认为有此User的全部权利。是需要严格保密的，这个数据应该只保存在站方，不应该共享给其它网站或者第三方App。所以简单来说，如果你的用户数据可能需要和第三方共享，或者允许第三方调用API接口，用Token。如果永远只是自己的网站，自己的App，用什么就无所谓了。

token就是令牌，比如你授权（登录）一个程序时，他就是个依据，判断你是否已经授权该软件；cookie就是写在客户端的一个txt文件，里面包括你登录信息之类的，这样你下次在登录某个网站，就会自动调用cookie自动登录用户名；session和cookie差不多，只是session是写在服务器端的文件，也需要在客户端写入cookie文件，但是文件里是你的浏览器编号.Session的状态是存储在服务器端，客户端只有session id；而Token的状态是存储在客户端。



作者：骑小猪看流星
链接：https://www.jianshu.com/p/bd1be47a16c1
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。



---

### 6.5 路由器与交换机的区别

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq4xr6sytj30k0064dfw.jpg)

示意图

### 6.6 Cookie 与 Session

- 简介

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq4xqb7u3j30xc07ygq3.jpg)

示意图

- 区别 & 对比

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq4xovr4fj30ri08cdh2.jpg)

示意图

### 6.7 Cookie 与 Token

- 简介

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq4xnhli2j30xc0fqdix.jpg)

示意图

- 基于`Cookie`的身份验证 & 验证流程

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq4xm4k7aj30xc0iatc1.jpg)

示意图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq4xkpdz2j30ha0ftwfr.jpg)

示意图

- 基于`Token`的身份验证 & 验证流程

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq4xh1hwjj30xc0e8jve.jpg)

示意图

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glq4xfkf8ij30ha0fpwft.jpg)



作者：Carson_Ho
链接：https://www.jianshu.com/p/45d27f3e1196
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。