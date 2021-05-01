[TOC]



React框架本身只应用于View，如果基于MVC模式开发，还需要Model和Control层，这样催生了Flux的产生，而Redux是基于Flux理念的一种解决方式。



<https://www.jianshu.com/p/0f5c018e16a5>

Flux

- Flux 提供了一种非常清晰的方式来存储和更新应用状态,并且只会在必要的时候才触发

单向数据流框架的始祖Flux

Flux理念的一个更强实现Redux

结合React和Redux

**Flux的不足**

- 1.Store之间依赖关系
- 2.难以进行服务器端渲染
- 3.Store混杂了逻辑和状态





**Redux** 是一个 JavaScript 应用的可预测状态容器。它可以运行在客户端、服务端以及原生应用中。

共享的可变性状态是万恶之源

核心思想

核心思想1：Store是单一数据源，只有一个Store。

核心思想2：改变state的唯一方法是触发action。不能在state上直接修改数据。

使用reducer纯函数来改变state，reducer函数接收旧的state和action作为参数，返回新的state。



**Redux的组成**

1、action，它是信息的载体，它描述了一个特定的行为。使用store.dispatch()可以把action传递到store中去。action是store的唯一信息来源。

2、reducer，它的任务是定义整个程序的state如何响应。

3、store，它保存着整个程序的state，它是action和reducer这二者的粘合剂。它的常用API有：getState()、dispatch()、subscribe()。





**六、小结**

1、调用store.dispatch(action)，用于执行一个action。

2、store调用时传入reducer函数。store的来源就是reducer，createStore(rootReducer)用于创建store。

3、reducer接收state和action作为输入，根据action处理后返回新的state。

4、store中保存了reducer返回的完整state。可以使用store.getState()来获得当前state，也可以通过store.subscribe()来监听state的变化。

5、Redux没有Dispatcher，也不支持多个Store。当应用程序中数据越来越复杂时，建议的做法是对root reducer函数进行分割。





而dispatcher相当于controller，store相当于Model,Flux相当于view；这样就形成了一个如果想要改变页面必须经过action动作；

dispatcher:处理动作分发，维持store之间的依赖关系；

store：负责存储数据和处理数据相关逻辑；

action:驱动dispatcher的js对象；

view：视图；





flux的不足之处：
无法自动刷新，每一次都需要手动的在页面进行事件的监听，从而改变store的值；

store中混杂了逻辑与状态，在开发过程不停的更换store的逻辑，容易出现bug；

 

flux的优势：
传统前端的mvc无法禁绝view和model之间的通讯问题，而flux的中store，在只有get方法，在页面中只能读取数据，而无法直接更改数据，每次数据的更变都必须通过action派发一个事件给dispatcher，由dispatcher去改变；

 

### redux

简介：
如果把flux看成一个架构理念，那么redux就是flux的一种实现，除了redux之外，还有很多flux的框架，比如reflux、fluxible等等；2013年flux问世之后饱受争议，所以在2015年redux出现；在flux中只是提出了单向数据流，而redux在此基础上提出三个原则：

- 唯一数据源：在redux的应用中只能有一个state数据源；

- 保持状态只读:在view层只能对state数据进行读取，无法改变；

- 数据改变只能通过纯函数完成:reducer函数只做数据的运算的不做数据的存储，所有的reducer函数只会接受值，进行运算，redux中的reducer接受两个参数，第一个参数是state当前的数据状态，第二个参数是action传入的值；将原始值和传入进行运算；

理论讲解：

- action:
  定义：Action 是把数据从应用（译者注：这里之所以不叫 view 是因为这些数据有可能是服务器响应，用户输入或其它非 view 的数据 ）传到 store 的有效载荷。它是 store 数据的唯一来源。一般来说你会通过 store.dispatch() 将 action 传到 store。
  action 内必须使用一个**字符串类型的 type 字段**来表示将要执行的动作
  redux中的action和flux中的action的区别：
  在 Redux 中的 action 创建函数只是简单的返回一个 action，redux中action只需要将返回的对象传递给reducer函数去做处理；

  在 传统的 Flux 实现中，当调用 action 创建函数时，一般会触发一个 dispatch

- reducer:
  state的设计（应用数据库）
  尽可能地把 state 范式化，不存在嵌套。把所有数据放到一个对象里，每个数据以 ID 为主键，不同实体或列表间通过 ID 相互引用数据。把应用的 state 想像成数据库

  reducer函数的设计：
  保持 reducer 纯净非常重要。永远不要在 reducer 里做这些操作：

  修改传入参数；

  执行有副作用的操作，如 API 请求和路由跳转；

  调用非纯函数，如 Date.now() 或 Math.random()。

  注意：

  在reducer中不要直接修改state，。 使用 Object.assign() 新建了一个副本。不能这样使用 Object.assign(state, {visibilityFilter: action.filter })，因为它会改变第一个参数的值。你必须把第一个参数设置为空对象。你也可以开启对ES7提案对象展开运算符的支持, 从而使用 { ...state, ...newState } 达到相同的目的。

  对于reducer文件的拆分以及使用combineReducers()工具类
  随着应用的膨胀，我们还可以将拆分后的 reducer 放到不同的文件中, 以保持其独立性并用于专门处理不同的数据域。每个 reducer 只负责管理全局 state 中它负责的一部分。每个 reducer 的 state 参数都不同，分别对应它管理的那部分 state 数据

  注：combineReducers() 所做的只是生成一个函数，这个函数来调用你的一系列 reducer，每个 reducer 根据它们的 key 来筛选出 state 中的一部分数据并处理，然后这个生成的函数再将所有 reducer 的结果合并成一个大的对象。

  注：这里其实实现的就是类似于vuex中模块式管理数据逻辑层；

- store:
  维持应用的 state；

  提供 getState() 方法获取 state；

  提供 dispatch(action) 方法更新 state；

  通过 subscribe(listener) 注册监听器;

  通过 unsubscribe(listener) 返回的函数注销监听器

  注：Redux 应用只有一个单一的 store。当需要拆分数据处理逻辑时，你应该使用 reducer 组合而不是创建多个 store。

  



### react与redux的搭配：

声明：Redux 和 React 之间**没有关系**。Redux 支持 React、Angular、Ember、jQuery 甚至纯 JavaScript。

在redux中使用react，需要安装react-redux；????

npm install --save react-redux

容器组件：（顶层组件）

只在最顶层组件（如路由操作）里使用 Redux，数据的读取和更改都来自redux；

展示组件：（傻瓜组件）

其余内部组件仅仅是展示性的，所有数据都通过 props 传入。数据的读取和修改来自于props的数据和props传递的回掉函数；

 

react和redux的链接：

我们需要做出两个变化，将 App 组件连接到 Redux 并且让它能够 dispatch actions 以及从 Redux store 读取到 state。

首先，我们需要获取从之前安装好的 react-redux 提供的 Provider，并且在渲染之前将根组件包装进\<Provider>。

接着，我们想要通过 react-redux 提供的 connect() 方法将包装好的组件连接到Redux。尽量只做一个顶层的组件，或者 route 处理。从技术上来说你可以将应用中的任何一个组件 connect() 到 Redux store 中，但尽量避免这么做，因为这个数据流很难追踪。

任何一个从 connect() 包装好的组件都可以得到一个 dispatch 方法作为组件的 props，以及得到全局 state 中所需的任何内容。 connect() 的唯一参数是 selector。此方法可以从 Redux store 接收到全局的 state，然后返回组件中需要的 props。最简单的情况下，可以返回一个初始的 state （例如，返回认证方法），但最好先将其进行转化。




- <https://www.zhihu.com/question/36516604>

- <https://www.jianshu.com/p/126b77a894b0>

- <https://blog.csdn.net/qq_empire/article/details/82749557>

- [react入门之 Redux与flux的比较学习](https://www.jianshu.com/p/908af9f49d42)

- [Vuex、Flux、Redux、Redux-saga、Dva、MobX](https://baijiahao.baidu.com/s?id=1625766414552010804&wfr=spider&for=pc)

- [理顺react，flux，redux这些概念的关系](https://www.cnblogs.com/dreamingbaobei/p/8476984.html)

- [React高级篇（一）从Flux到Redux，react-redux](https://www.jianshu.com/p/fe53e5fe189d)

- http://www.ruanyifeng.com/blog/2016/09/redux_tutorial_part_three_react-redux.html
- https://www.infoq.cn/article/react-flux/
- https://www.zhihu.com/tardis/sogou/ans/107209140
- https://www.zhihu.com/question/36516604/answer/82878351
- https://www.zhihu.com/tardis/sogou/art/30735938
- https://www.cnblogs.com/yangyangxxb/p/10105856.html



https://github.com/nonocast/todolist
曹操不吃饭  01:08:36
https://github.com/LiveLikeCounter/Flutter-Todolist
曹操不吃饭  01:09:07
https://github.com/fishenal/Todos_Vuejs
曹操不吃饭  01:15:17
https://github.com/facebook/flux
曹操不吃饭  01:17:04
https://github.com/reduxjs/redux