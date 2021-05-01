https://www.baidu.com/s?wd=mvc%20mvp%20mvvm&rsv_spt=1&rsv_iqid=0xaf3732a60001434a&issp=1&f=8&rsv_bp=1&rsv_idx=2&ie=utf-8&tn=baiduhome_pg&rsv_enter=1&rsv_dl=ib&rsv_sug3=8&rsv_sug1=8&rsv_sug7=100



## 一、MVC

MVC模式的意思是，软件可以分成三个部分。

![img](http://www.ruanyifeng.com/blogimg/asset/2015/bg2015020104.png)

> - 视图（View）：用户界面。
> - 控制器（Controller）：业务逻辑
> - 模型（Model）：数据保存

各部分之间的通信方式如下。

![img](http://www.ruanyifeng.com/blogimg/asset/2015/bg2015020105.png)

> 1. View 传送指令到 Controller
> 2. Controller 完成业务逻辑后，要求 Model 改变状态
> 3. Model 将新的数据发送到 View，用户得到反馈

所有通信都是单向的。

## 二、互动模式

接受用户指令时，MVC 可以分成两种方式。一种是通过 View 接受指令，传递给 Controller。

![img](http://www.ruanyifeng.com/blogimg/asset/2015/bg2015020106.png)

另一种是直接通过controller接受指令。

![img](http://www.ruanyifeng.com/blogimg/asset/2015/bg2015020107.png)

## 三、实例：Backbone

实际项目往往采用更灵活的方式，以 [Backbone.js](http://documentcloud.github.com/backbone) 为例。

![img](http://www.ruanyifeng.com/blogimg/asset/2015/bg2015020108.png)

\1. 用户可以向 View 发送指令（DOM 事件），再由 View 直接要求 Model 改变状态。

\2. 用户也可以直接向 Controller 发送指令（改变 URL 触发 hashChange 事件），再由 Controller 发送给 View。

\3. Controller 非常薄，只起到路由的作用，而 View 非常厚，业务逻辑都部署在 View。所以，Backbone 索性取消了 Controller，只保留一个 Router（路由器） 。

## 四、MVP

MVP 模式将 Controller 改名为 Presenter，同时改变了通信方向。

![img](http://www.ruanyifeng.com/blogimg/asset/2015/bg2015020109.png)

\1. 各部分之间的通信，都是双向的。

\2. View 与 Model 不发生联系，都通过 Presenter 传递。

\3. View 非常薄，不部署任何业务逻辑，称为"被动视图"（Passive View），即没有任何主动性，而 Presenter非常厚，所有逻辑都部署在那里。

## 五、MVVM

MVVM 模式将 Presenter 改名为 ViewModel，基本上与 MVP 模式完全一致。

![img](http://www.ruanyifeng.com/blogimg/asset/2015/bg2015020110.png)

唯一的区别是，它采用双向绑定（data-binding）：View的变动，自动反映在 ViewModel，反之亦然。[Angular](https://angularjs.org/) 和 [Ember](http://emberjs.com/) 都采用这种模式。

（完）



---

# 1. MVC框架

MVC全名是Model View Controller，是模型(model)－视图(view)－控制器(controller)的缩写，一种软件设计典范，用一种业务逻辑、数据、界面显示分离的方法组织代码，将业务逻辑聚集到一个部件里面，在改进和个性化定制界面及用户交互的同时，不需要重新编写业务逻辑。MVC被独特的发展起来用于映射传统的输入、处理和输出功能在一个逻辑的图形化用户界面的结构中。

![img](https://upload-images.jianshu.io/upload_images/15226743-86c2d4be3b5833c3.png?imageMogr2/auto-orient/strip|imageView2/2/w/601/format/webp)

MVC框架模式图

### 1.1 MVC 编程模式

MVC 是一种使用 MVC（Model View Controller 模型-视图-控制器）设计创建 Web 应用程序的模式： [1]

- Model（模型）表示应用程序核心（如数据库）。
- View（视图）显示效果（HTML页面）。
- Controller（控制器）处理输入（业务逻辑）。

MVC 模式同时提供了对 HTML、CSS 和 JavaScript 的完全控制。

**Model（模型）**是应用程序中用于处理应用程序数据逻辑的部分。
 　通常模型对象负责在数据库中存取数据。

**View（视图）**是应用程序中处理数据显示的部分。
 　通常视图是依据模型数据创建的。

**Controller（控制器）**是应用程序中处理用户交互的部分。
 　通常控制器负责从视图读取数据，控制用户输入，并向模型发送数据。

### 优点

[**耦合性**](https://links.jianshu.com/go?to=https%3A%2F%2Fbaike.baidu.com%2Fitem%2F%E8%80%A6%E5%90%88%E6%80%A7)**低**

视图层和业务层分离，这样就允许更改视图层代码而不用重新编译模型和控制器代码，同样，一个应用的业务流程或者业务规则的改变只需要改动MVC的模型层即可。因为模型与控制器和视图相分离，所以很容易改变应用程序的数据层和业务规则。

模型是自包含的，并且与控制器和视图相分离，所以很容易改变应用程序的数据层和业务规则。如果把数据库从MySQL移植到[Oracle](https://links.jianshu.com/go?to=https%3A%2F%2Fbaike.baidu.com%2Fitem%2FOracle)，或者改变基于RDBMS数据源到[LDAP](https://links.jianshu.com/go?to=https%3A%2F%2Fbaike.baidu.com%2Fitem%2FLDAP)，只需改变模型即可。一旦正确的实现了模型，不管数据来自数据库或是LDAP服务器，视图将会正确的显示它们。由于运用MVC的应用程序的三个部件是相互独立，改变其中一个不会影响其它两个，所以依据这种设计思想能构造良好的松耦合

**重用性高**

随着技术的不断进步，需要用越来越多的方式来访问应用程序。MVC模式允许使用各种不同样式的视图来访问同一个服务器端的代码，因为多个视图能共享一个模型，它包括任何WEB（HTTP）浏览器或者无线浏览器（wap），比如，用户可以通过电脑也可通过手机来订购某样产品，虽然订购的方式不一样，但处理订购产品的方式是一样的。由于模型返回的数据没有进行格式化，所以同样的构件能被不同的界面使用。例如，很多数据可能用HTML来表示，但是也有可能用WAP来表示，而这些表示所需要的命令是改变视图层的实现方式，而控制层和模型层无需做任何改变。由于已经将数据和业务规则从表示层分开，所以可以最大化的重用代码了。模型也有状态管理和数据持久性处理的功能，例如，基于会话的购物车和电子商务过程也能被Flash网站或者无线联网的应用程序所重用。 [11]

[**生命周期**](https://links.jianshu.com/go?to=https%3A%2F%2Fbaike.baidu.com%2Fitem%2F%E7%94%9F%E5%91%BD%E5%91%A8%E6%9C%9F)**成本低**

MVC使开发和维护用户[接口](https://links.jianshu.com/go?to=https%3A%2F%2Fbaike.baidu.com%2Fitem%2F%E6%8E%A5%E5%8F%A3)的技术含量降低。

**部署快**

使用MVC模式使开发时间得到相当大的缩减，它使程序员（Java开发人员）集中精力于业务逻辑，界面程序员（HTML和JSP开发人员）集中精力于表现形式上。

**可维护性高**

分离视图层和业务逻辑层也使得WEB应用更易于维护和修改。

**有利软件工程化管理**

由于不同的层各司其职，每一层不同的应用具有某些相同的特征，有利于通过工程化、工具化管理程序代码。控制器也提供了一个好处，就是可以使用控制器来联接不同的模型和视图去完成用户的需求，这样控制器可以为构造应用程序提供强有力的手段。给定一些可重用的模型和视图，控制器可以根据用户的需求选择模型进行处理，然后选择视图将处理结果显示给用户。

### 缺点

**没有明确的定义**

完全理解MVC并不是很容易。使用MVC需要精心的计划，由于它的内部原理比较复杂，所以需要花费一些时间去思考。同时由于模型和视图要严格的分离，这样也给调试应用程序带来了一定的困难。每个构件在使用之前都需要经过彻底的测试。

**不适合小型，中等规模的应用程序**

花费大量时间将MVC应用到规模并不是很大的应用程序通常会得不偿失。

**增加系统结构和实现的复杂性**

对于简单的界面，严格遵循MVC，使模型、视图与控制器分离，会增加结构的复杂性，并可能产生过多的更新操作，降低运行效率。

**视图与控制器间的过于紧密的连接**

视图与控制器是相互分离，但却是联系紧密的部件，视图没有控制器的存在，其应用是很有限的，反之亦然，这样就妨碍了他们的独立重用。

**视图对模型数据的低效率访问**

依据模型操作接口的不同，视图可能需要多次调用才能获得足够的显示数据。对未变化数据的不必要的频繁访问，也将损害操作性能。

**一般高级的界面工具或构造器不支持模式**

改造这些工具以适应MVC需要和建立分离的部件的代价是很高的，会造成MVC使用的困难。

# 2. MVP模式

全称：Model-View-Presenter ；MVP 是从经典的模式MVC演变而来，它们的基本思想有相通的地方Controller/Presenter负责逻辑的处理，Model提供数据，View负责显示。



![img](https://upload-images.jianshu.io/upload_images/15226743-947a7c01f8199148.png?imageMogr2/auto-orient/strip|imageView2/2/w/537/format/webp)

MVP框架模式图

## 优点

1、模型与视图完全分离，我们可以修改视图而不影响模型

2、可以更高效地使用模型，因为所有的交互都发生在一个地方——Presenter内部

3、我们可以将一个Presenter用于多个视图，而不需要改变Presenter的逻辑。这个特性非常的有用，因为视图的变化总是比模型的变化频繁。

4、如果我们把逻辑放在Presenter中，那么我们就可以脱离用户接口来测试这些逻辑（单元测试）

## 缺点

由于对视图的渲染放在了Presenter中，所以视图和Presenter的交互会过于频繁。还有一点需要明白，如果Presenter过多地渲染了视图，往往会使得它与特定的视图的联系过于紧密。一旦视图需要变更，那么Presenter也需要变更了。比如说，原本用来呈现Html的Presenter现在也需要用于呈现Pdf了，那么视图很有可能也需要变更。

### MVP与MVC区别：

作为一种新的模式，MVP与MVC有着一个重大的区别：在MVP中View并不直接使用Model，它们之间的通信是通过Presenter (MVC中的Controller)来进行的，所有的交互都发生在Presenter内部，而在MVC中View会直接从Model中读取数据而不是通过 Controller。
 在MVC里，View是可以直接访问Model的！从而，View里会包含Model信息，不可避免的还要包括一些业务逻辑。 在MVC模型里，更关注的Model的改变，而同时有多个对Model的不同显示，即View。所以，在MVC模型里，Model不依赖于View，但是View是依赖于Model的。不仅如此，因为有一些业务逻辑在View里实现了，导致要更改View也是比较困难的，至少那些业务逻辑是无法重用的。
 虽然 MVC 中的 View 的确“可以”访问 Model，但是我们不建议在 View 中依赖 Model，而是要求尽可能把所有业务逻辑都放在 Controller 中处理，而 View 只和 Controller 交互。

### 区别如下图所示：

![img](https:////upload-images.jianshu.io/upload_images/15226743-7b9ec190c760b7a8.png?imageMogr2/auto-orient/strip|imageView2/2/w/601/format/webp)

mvc.png



![img](https:////upload-images.jianshu.io/upload_images/15226743-cc8ce0ab83777e7a.png?imageMogr2/auto-orient/strip|imageView2/2/w/537/format/webp)

mvp.png

# 3.MVVM框架

MVVM是Model-View-ViewModel的简写。它本质上就是MVC 的改进版。MVVM 就是将其中的View 的状态和行为抽象化，让我们将视图 UI 和业务逻辑分开。当然这些事 ViewModel 已经帮我们做了，它可以取出 Model 的数据同时帮忙处理 View 中由于需要展示内容而涉及的业务逻辑。微软的WPF带来了新的技术体验，如Silverlight、音频、视频、3D、动画……，这导致了软件UI层更加细节化、可定制化。同时，在技术层面，WPF也带来了 诸如Binding、Dependency Property、Routed Events、Command、DataTemplate、ControlTemplate等新特性。MVVM（Model-View-ViewModel）框架的由来便是MVP（Model-View-Presenter）模式与WPF结合的应用方式时发展演变过来的一种新型架构框架。它立足于原有MVP框架并且把WPF的新特性糅合进去，以应对客户日益复杂的需求变化。

![img](https://upload-images.jianshu.io/upload_images/15226743-1b2adc4a66e12c6e.png?imageMogr2/auto-orient/strip|imageView2/2/w/715/format/webp)

MVVM框架模式图

### 3.1 MVVM模式的组成部分

- 模型
  - *模型*是指代表真实状态内容的领域模型（面向对象），或指代表内容的数据访问层（以数据为中心）。
- 视图
  - 就像在MVC和MVP模式中一样，视图是用户在屏幕上看到的结构、布局和外观（UI）。
- 视图模型
  - *视图模型*是暴露公共属性和命令的视图的抽象。MVVM没有MVC模式的控制器，也没有MVP模式的presenter，有的是一个*绑定器*。在视图模型中，绑定器在视图和数据绑定器之间进行通信。
- 绑定器
  - 声明性数据和命令绑定隐含在MVVM模式中。在Microsoft解决方案堆中，绑定器是一种名为[XAML](https://links.jianshu.com/go?to=https%3A%2F%2Fbaike.baidu.com%2Fitem%2FXAML)的标记语言。绑定器使开发人员免于被迫编写样板式逻辑来同步视图模型和视图。在微软的堆之外实现时，声明性数据绑定技术的出现是实现该模式的一个关键因素。 [1]

### 3.2  MVVM优点

MVVM模式和MVC模式一样，主要目的是分离视图（View）和模型（Model），有几大优点

**1. 低耦合**。视图（View）可以独立于Model变化和修改，一个ViewModel可以绑定到不同的"View"上，当View变化的时候Model可以不变，当Model变化的时候View也可以不变。

**2. 可重用性**。你可以把一些视图逻辑放在一个ViewModel里面，让很多view重用这段视图逻辑。

**3. 独立开发**。开发人员可以专注于业务逻辑和数据的开发（ViewModel），设计人员可以专注于页面设计，使用Expression Blend可以很容易设计界面并生成xaml代码。

**4. 可测试**。界面素来是比较难于测试的，而现在测试可以针对ViewModel来写。

### 3.2  MVVM与MVP区别：

mvvm模式将Presener改名为View Model，基本上与MVP模式完全一致，唯一的区别是，它采用双向绑定(data-binding):   View的 变动，自动反映在View Model，反之亦然。这样开发者就不用处理接收事件和View更新的工作，框架已经帮你做好了。



作者：IT小白_
链接：https://www.jianshu.com/p/ff6de219f988
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---

