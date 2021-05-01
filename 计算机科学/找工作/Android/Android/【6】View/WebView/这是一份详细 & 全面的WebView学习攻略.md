[TOC]

- 那么这种该如何实现呢？其实这是`Android`里一个叫`WebView`组件实现
- 今天，我将献上一份全面 & 详细的 `WebView`攻略，含具体介绍、使用教程、与前端`JS`交互、缓存机制构建等等，希望您们会喜欢。

------

# 目录

<img src="https://tva1.sinaimg.cn/large/008eGmZEly1gmwfmwpz9rj30ro0ve3zm.jpg" alt="img" style="zoom:33%;" />

示意图

------

# 1. 简介

一个基于`webkit`引擎、展现`web`页面的控件

> a. `Android 4.4`前：`Android Webview`在低版本 & 高版本采用了不同的`webkit`版本的内核
> b. `Android 4.4`后：直接使用了`Chrome`内核

------

# 2. 作用

- 在 `Android` 客户端上加载`h5`页面
- 在本地 与 `h5`页面实现交互 & 调用
- 其他：对 `url` 请求、页面加载、渲染、对话框 进行额外处理。

------

# 3. 具体使用

- `Webview`的使用主要包括：`Webview`类 及其 工具类（`WebSettings`类、`WebViewClient`类、`WebChromeClient`类）

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfmuqspzj30ka0c8dhs.jpg)

示意图

- 下面我将详细介绍上述4个使用类 & 使用方法
- 具体请看文章：[Android开发：最全面、最易懂的Webview详解](https://www.jianshu.com/p/3c94ae673e2a)

------

# 4. WebView与 JS 的交互方式

- 在`Android WebView`的使用中，与前端`h5`页面交互的需求十分常见
- `Android` 与 `JS` 通过WebView互相调用方法，实际上是：`Android` 去调用`JS`的代码 + `JS`去调用`Android`的代码

> 二者沟通的桥梁是`WebView`

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfmt0uitj30xc0brwh1.jpg)

示意图

- 具体介绍请看文章：[最全面总结 Android WebView与 JS 的交互方式](https://www.jianshu.com/p/345f4d8a5cfa)

------

# 5. 使用漏洞

- `WebView` 使用过程中存在许多漏洞，容易造成用户数据泄露等等危险，而很多人往往会忽视这个问题
- `WebView`中，主要漏洞有3类：**任意代码执行漏洞、密码明文存储漏洞、域控制不严格漏洞**
- 漏洞具体介绍 & 修复方式请看文章：[你不知道的 Android WebView 使用漏洞](https://www.jianshu.com/p/3a345d27cd42)

------

# 6. 缓存机制构建

- `Android WebView`由于前端`h5`本身的原因，存在加载效率慢 & 流量耗费的性能问题，具体介绍如下：

![img](https://upload-images.jianshu.io/upload_images/944365-d0e842a6e92eef2c.png?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

示意图

- 本文通过 **`H5`缓存机制 + 资源预加载 + 资源拦截**的方式 构建了一套`WebView`缓存机制，从而解决`Android WebView`的性能问题，最终提高用户使用体验
- 具体缓存机制的讲解请看文章：[手把手教你构建 Android WebView 的缓存机制 & 资源预加载方案](https://www.jianshu.com/p/5e7075f4875f)

至此，关于`Android WebView`的所有知识讲解完毕。

------

# 7. 总结

- 本文全面讲解了 `WebView`的相关知识，含具体介绍、使用教程、与前端`JS`交互、缓存机制构建等等，相信你对`Android WebView`的使用已经非常熟悉了。
- 关于WebView的系列文章希望对你有所帮助
  [Android开发：最全面、最易懂的Webview详解](https://www.jianshu.com/p/3c94ae673e2a)
  [最全面总结 Android WebView与 JS 的交互方式](https://www.jianshu.com/p/345f4d8a5cfa)
  [手把手教你构建 Android WebView 的缓存机制 & 资源预加载方案](https://www.jianshu.com/p/5e7075f4875f)
  [你不知道的 Android WebView 使用漏洞](https://www.jianshu.com/p/3a345d27cd42)
- 接下来我会继续讲解其他安卓开发的知识，[https://www.wandouys.com/video/64612.html](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.wandouys.com%2Fvideo%2F64612.html)

------

#### 请点赞！因为你们的赞同/鼓励是我写作的最大动力！

> **相关文章阅读**
> [Android开发：最全面、最易懂的Android屏幕适配解决方案](https://www.jianshu.com/p/ec5a1a30694b)
> [Android事件分发机制详解：史上最全面、最易懂](https://www.jianshu.com/p/38015afcdb58)
> [手把手教你写一个完整的自定义View](https://www.jianshu.com/p/e9d8420b1b9c)
> [快来看看Google出品的Protocol Buffer，别只会用Json和XML了](https://www.jianshu.com/p/1538bf85dad1)
> [Android开发：JSON简介及最全面解析方法!](https://www.jianshu.com/p/b87fee2f7a23)

------

### 



作者：Carson_Ho
链接：https://www.jianshu.com/p/d2d4f652029d
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。