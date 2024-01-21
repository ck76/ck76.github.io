django作为一个流行的基于python的web框架，也是支持MVC模式的，其实在django中，将这种实现方式称为MVT（Model-view-tempate）框架。



![img](https://mmbiz.qpic.cn/mmbiz_png/QbzaF3VBtHNGNmod9k3O2ZXj9n5cH9ibMWCou0ZE7tw6UPKAMG5qsMkoDxT2MWy9xr43W5MRLbkVc5tFib2dyjVQ/640?wx_fmt=png&tp=webp&wxfrom=5&wx_lazy=1&wx_co=1)

MVC框架的核心思想是：解耦，让不同的代码块之间降低耦合，增强代码的可扩展性和可移植性，实现向后兼容。

**什么是MVC？**

MVC，全名是Model View Controller，是软件工程中的一种软件架构模式，把软件系统分为三个基本部分：模型(Model)、视图(View)和控制器(Controller)，

1. 最上面的一层，是直接面向最终用户的"视图层"（View）。它是提供给用户的操作界面，是程序的外壳。

2. 最底下的一层，是核心的"数据层"（Model），也就是程序需要操作的数据或信息。

3. 中间的一层，就是"控制层"（Controller），它负责根据用户从"视图层"输入的指令，选取"数据层"中的数据，然后对其进行相应的操作，产生最终结果。

   其实不管是网页还是软件，都可以很好的应用MVC模式来设计。

   

![img](https://tva1.sinaimg.cn/large/007S8ZIlly1gjk82ly4xrj30e00fxq4t.jpg)

django在一定程度上也借鉴了mvc的模式。与它不同的是，django框架的设计变成了models，template，views三个部分：



  M 代表模型（Model），即数据存取层。该层处理与数据相关的所有事务：如何存取、如何验证有效性、包含哪些行为以及数据之间的关系等
  T 代表模板(Template)，即表现层。负责如何把页面展示给用户
  V 代表视图（View），即业务逻辑层。该层包含存取模型及调取恰当模板的相关逻辑。你可以把它看作模型与模板之间的桥梁。

此外，Django还有一个urls分发器，它的作用是将一个个URL的页面请求分发给不同的view处理，view再调用相应的Model和Template

![img](https://tva1.sinaimg.cn/large/007S8ZIlly1gjk82o4saqj30fs0jijuu.jpg)

MVT模式流程图如下：

![image-20201010142959369](https://tva1.sinaimg.cn/large/007S8ZIlly1gjk834x35mj31160hqwoq.jpg)

step1、 用户点击注册按钮，将要注册的内容发送给网站的服务器。
step2、 View 视图，接收到用户发来的注册数据，View 告诉 Model 将用户的注册信息保存进数据库。

step3、 Model 层将用户的注册信息保存到数据库中。
step4、 数据库将保存的结果返回给 Model。
step5、 Model 将保存的结果给 View 视图。
step6、 View 视图告诉 Template 模板去产生一个 Html 页面。
step7、 Template 生成 html 内容返回给 View 视图。

step8、 View 将 html 页面内容返回给浏览器。

step9、 浏览器拿到 view 返回的 html 页面内容进行解析展示。