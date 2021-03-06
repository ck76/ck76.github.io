## 引言

  第一个简单的问题：请问MVC模式是设计模式吗？答案：不是。如果你对设计模式和架构模式还有认识上的误区，那你就要警惕了！
  第二个简单的问题：请问你在什么时候使用到MVC模式？是在Java开发后台的时候？还是在前端开发，比如Android开发？
  第三个简单的问题：在使用MVC模式从事开发的过程中，你有没有发现它这个模式本身有什么局限性，或者有什么可以改进的地方？

  可以说，这是灵魂三问了吧！今天我们要介绍的MVVM模式，就是MVC模式的升级版，它针对MVC模式的短板进行了改进，是现如今Android开发过程中最常见的架构模式，今天我们就来认识它。（灵魂三问的答案先卖个关子，读完文章你就知道了）

------

## 介绍

  首先，要搞明白MVVM模式，它是一种架构模式，有别于设计模式。这两种是有本质区别的，设计模式只是为了解决一类问题而总结出的抽象方法，比如单例模式，就是防止对象被多次实例化；而架构模式往往能使用多种设计模式。

  MVVM是Model-View-ViewModel的简写，MVVM模式实现了数据视图的绑定（DataBinding），当数据变化时，视图会自动更新；反之，当视图发生改变时，数据也会自动更新。

  说人话？来设想一下，比如你要调用接口获取数据，进而在视图中展示给用户，那如果出现了这种情况，数据改变了呢？你是不是在想重新请求下接口数据，再获取到就OK了啊，重新请求接口是没错，但拿到的数据，怎么更新？还需要繁琐地设置各种填充数据流吧？这就是MVC的设计思路，而MVVM所做的事情，就是简化其将数据转化为视图的过程！（再不懂就没道理了啊！）

------

## 解析

##### 何为MVVM模式？介绍的太皮毛了！

拆分来看：Model+View+ViewModel
 对比MVC：Model+View+Controller
  看出区别了吗？Model+View两者都一样有，无非是数据保存和用户界面，它们最大的区别就在于M与V的交互方式不同，MVC模式采用Controller来处理业务逻辑，而MVVM则采用ViewModel来处理。

##### 具体来说：

对比MVC模式：
  用户操作> View (负责接受用户的输入操作)>Controller（业务逻辑处理）>Model（数据持久化）>View（将结果通过View反馈给用户）。
  先不说其他，请问把所有的业务逻辑都交给Controller来进行会出现什么问题？
 1.所有业务逻辑都在Controller里操作，逻辑复杂且不利于维护。
 2.大量的DOM 操作使页面渲染性能降低，加载速度变慢，影响用户体验。
 3.当 Model 频繁发生变化，需要主动更新到View；当用户的操作导致Model发生变化，同样需要将变化的数据同步到Model中， 这样的工作不仅繁琐，而且很难维护复杂多变的数据状态。

## 于是MVVM出现了，它来了它来了，它带着自动更新走来了！ 

  ViewModel 是一个同步View 和 Model的对象。View 和 Model 之间并没有直接的联系，而是通过ViewModel进行交互。ViewModel 通过双向数据绑定把 View 层和 Model 层连接了起来，而View 和 Model 之间的同步工作完全是自动的，无需人为干涉，因此开发者只需关注业务逻辑，不需要手动操作DOM, 不需要关注数据状态的同步问题，复杂的数据状态维护完全由 MVVM 来统一管理。

  数据视图双向绑定VM双向绑定：在 MVVM 框架中，View(视图) 和 Model(数据) 是不可以直接通讯的，在它们之间存在着 ViewModel 这个中间介充当着观察者的角色。当用户操作 View(视图)，ViewModel 感知到变化，然后通知 Model 发生相应改变；反之当 Model(数据) 发生改变，ViewModel 也能感知到变化，使 View 作出相应更新。这个一来一回的过程就是我们所熟知的双向绑定。而这一切就需要大师兄DataBinding（JetPack中的一个成员，进行数据绑定）来实现。

## 操作步骤：

（1）提供View，ViewModel以及Model三层；
 （2）将布局修改为DataBinding布局；
 （3）View与ViewModel之间通过DataBinding进行通信；
 （4）获取数据并展示在界面上。

## 优缺点

##### 优点：

1.简化了界面与业务的依赖性，也解决了数据的频繁更新的问题。
 2.实现了数据和视图的双向绑定，极大地简化了代码。
 3.减少了接口数量；
 4.告别了繁琐findViewById操作；

##### 缺点：

bug难以调试，并且DataBinding目前还存在一些编译问题。

## 总结：

  要学好MVVM模式，需要先学习了解DataBinding和LiveData。
 DataBinding是MVVM数据绑定的工具。
  LiveData可以更好的解决MVVM之间的通信问题，并且它可以更好地感知组件的生命周期，能够有效地避免内存泄漏。

## 闲话

  在我的印象中，MVVM模式，就是一个取经团队，其中DataBinding是大师兄悟空，LiveData是二师兄悟能，Lifecycle则是三师弟悟净。
  DataBinding（大师兄）负责将肉眼可见的所有视图精怪都整理好，交由ViewModel（玄奘）来处理业务逻辑，但看过大话西游的都知道，悟空好斗，而玄奘良善，二者不可沟通，这就需要LiveData（二师兄）来进行沟通两者了，事实上LiveData就是为了解决DataBinding与ViewModel之间的通信问题而出现的，ViewModel调用接口获取数据（如来给它的），LiveData负责通知DataBinding，师傅要你更新视图，因为数据更改了，而Lifecycle（悟净）一直默默支持着大师兄，二师兄，以及师傅。

## Tips

  第一：MVVM模式中的DataBinding是非必须的。也就是说可以不用DataBinding实现MVVM模式。
  第二：ViewModel配合LiveData更新数据，只是它作为辅助的功能，它最主要的功能是共享数据。在Activity和Fragment复用同一个ViewModel时，Activity中的数据更改后，Fragment无需任何多余操作，可以自动更新数据。



作者：千夜零一
链接：https://www.jianshu.com/p/9ced36babff3
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。