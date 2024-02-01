

[TOC]

### 一、前言

- 图片加载是Android开发中最最基础的功能，同时图片加载OOM也一直困扰着很多开发者，因此为了降低开发周期和难度，我们经常会选用一些图片加载的开源库。
- 老牌的有ImageLoader，UIL,Volley，主流的有，Picasso，Glide，Fresco等等，选择一款好的图片加载裤就成了我们的首要问题。
- 接下来我们对比一下主流的三款 Picasso，Glide，Fresco框架的优缺点。



### 二、基本项对比

![基本项对比](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/BvtPqv9g*EFLuvG4xYvHO8JlTt6KXVXCoyZm51.wvxE!/r/dDABAAAAAAAA)



### 三、加载图片耗时及内存对比

#### 1、静态图片

> 加载静态图片可以看出三大主流框架性能都不错，不过用数据说话整体而言Glide更胜一筹

![静态](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/xiGS0DBhaSkt6YdQgVS*mHVPQpNnTEAB.ywg3tdhiRo!/r/dLYAAAAAAAAA)



#### 2、gif

> 下面的数据我们可以忽略Picasso了，因为它根本不支持gif，那么Glide和Fresco可以看出Fresco的java heap基本保持较低平稳状态，**而Glide的java heap基本为Fresco的一倍**，所以OOM的风险也比fresco大一倍。 
> 从时间上glide是有一定差距，不过fresco有两张图片没加载完成，所以时间不是完全可靠的数据 
> 从native heap可以看出Fresco最高545MB，这个有点恐怖，下面我们看个知识点。

![动图](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/SL2Roe*FwdEycXOxiI5ipzM9zICYY0uuYCjmltLDg2o!/r/dD4BAAAAAAAA)



##### 知识点

Java Heap是对于Java 虚拟机而说的，一般的大小上限是 16M 24M 48M 76M 具体视手机而定。
Native Heap是对于C/C++直接操纵的系统堆内存，所以它的上限一般是具体RAM的2/3左右。
所以对于2G的手机而言，Java Heap 大概76M，而Native Heap是760M左右，相差10倍。

**所以Fresco也是存在一定风险的，因为native heap数据实在是太恐怖了。**



### 四、详细属性对比(Glide和Fresco)

> Picasso从各方面都比这两个弱就算了,可以看出来**Fresco蛮强大的**，不过使用起来相对Glide要**复杂**一点，而且需要自己的SimpleDraweeView，这一点在切换框架的时候最让人头疼了。**而且Glide直接缓存相对大小的图片，节省空间的同时下场如果是同样大小的图片就不要再次请求，直接可以使用**。

![](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/dKS2ymmqeh7wLsrJKL*UvBnNZhJB1bcsrIJsRZ7FJIA!/r/dFQBAAAAAAAA)



### 五、总结

#### 1、优点

- Glide

  多种图片格式的缓存，适用于更多的内容表现形式（如Gif、WebP、缩略图、Video）
  生命周期集成（根据Activity或者Fragment的生命周期管理图片加载请求）
  高效处理Bitmap（bitmap的复用和主动回收，减少系统回收压力）
  高效的缓存策略，灵活（Picasso只会缓存原始尺寸的图片，Glide缓存的是多种规格），加载速度快且内存开销小（默认Bitmap格式的不同，使得内存开销是Picasso的一半）

- Fresco

  最大的优势在于5.0以下(最低2.3)的bitmap加载。在5.0以下系统，Fresco将图片放到一个特别的内存区域(Ashmem区)
  大大减少OOM（在更底层的Native层对OOM进行处理，图片将不再占用App的内存）
  适用于需要高性能加载大量图片的场景

#### 2、缺点
- Glide

  -没有文件缓存 
  -java heap比Fresco高

- Fresco

  包较大（2~3M）
  用法复杂
  底层涉及c++领域，阅读源码深入学习难度大
  结论

**Fresco虽然很强大，但是包很大，依赖很多，使用复杂，而且还要在布局使用SimpleDraweeView控件加载图片。相对而言Glide会轻好多，上手快，使用简单，配置方便，而且从加载速度和性能方面不相上下。对于一般的APP来说Glide是一个不错的选择，如果是专业的图片APP那么Fresco还是必要的。**