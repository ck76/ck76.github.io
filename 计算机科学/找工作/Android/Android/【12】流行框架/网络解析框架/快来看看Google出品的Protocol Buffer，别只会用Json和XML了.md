[TOC]

# 前言

- 习惯用 `Json、XML` 数据存储格式的你们，相信大多都没听过`Protocol Buffer`
- `Protocol Buffer` 其实 是 `Google`出品的一种轻量 & 高效的结构化数据存储格式，性能比 `Json、XML` 真的强！太！多！

> 由于 `Google`出品，我相信`Protocol Buffer`已经具备足够的吸引力

- 今天，我将献上一份全面 & 详细的 `Protocol Buffer`攻略，含介绍、特点、具体使用、源码分析、序列化原理等等，希望您们会喜欢。

------

# 目录

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfv2ngi0j30xc0t8acc.jpg)

示意图

------

# 1. 定义

一种 结构化数据 的数据存储格式（类似于 `XML、Json` ）

> 1. `Google` 出品 （开源）
> 2. `Protocol Buffer` 目前有两个版本：`proto2` 和 `proto3`
> 3. 因为`proto3` 还是beta 版，所以本次讲解是 `proto2`

------

# 2. 作用

通过将 结构化的数据 进行 串行化（**序列化**），从而实现 **数据存储 / RPC 数据交换**的功能

> 1. 序列化： 将 数据结构或对象 转换成 二进制串 的过程
> 2. 反序列化：将在序列化过程中所生成的二进制串 转换成 数据结构或者对象 的过程

------

# 3. 特点

- 对比于 常见的 `XML、Json` 数据存储格式，`Protocol Buffer`有如下特点：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfuzzmazj30xc0fugq3.jpg)

Protocol Buffer 特点

------

# 4. 应用场景

传输数据量大 & 网络环境不稳定 的**数据存储、RPC 数据交换** 的需求场景

> 如 即时IM （QQ、微信）的需求场景

------

# 总结

在 **传输数据量较大**的需求场景下，`Protocol Buffer`比`XML、Json` 更小、更快、使用 & 维护更简单！

------

# 5. 序列化原理解析

- 序列化的本质：对数据进行编码 + 存储
- `Protocol Buffer`的性能好：传输效率快，主要原因 = **序列化速度快 & 序列化后的数据体积小**，其原因如下：

1. 序列化速度快的原因：
   a. 编码 / 解码 方式简单（只需要简单的数学运算 = 位移等等）
   b. 采用 **`PB` 自身的框架代码 和 编译器** 共同完成
2. 序列化后的数据量体积小（即数据压缩效果好）的原因：
   a. 采用了独特的编码方式，如`Varint`、`Zigzag`编码方式等等
   b. 采用`T - L - V` 的数据存储方式：减少了分隔符的使用 & 数据存储得紧凑

更加详细的介绍，请看文章：[Protocol Buffer 序列化原理大揭秘 - 为什么Protocol Buffer性能这么好？](https://www.jianshu.com/p/30ef9b3780d9)

至此， 关于`Protocol Buffer`的序列化原理讲解完毕。下面将继续讲解如何具体使用`Protocol Buffer`

------

# 6. 使用步骤 & 实例讲解

使用 `Protocol Buffer` 的流程如下：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfux1xxnj30qe1k4jtx.jpg)

Protocol Buffer使用流程

下面，我将对流程中的每个流程进行详细讲解。

### 6.1 环境配置

- 要使用`Protocol Buffer` ，需要先在电脑上安装`Protocol Buffer`
- 具体请看文章：[手把手教你如何安装Protocol Buffer](https://www.jianshu.com/p/92dbe1ef0054)

至此， `Protocol Buffer`已经安装完成。下面将讲解如何具体使用`Protocol Buffer`

------

### 6.2 构建 `Protocol Buffer` 消息对象模型

- 构建步骤具体如下：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfuvsu75j309g0aujrp.jpg)

构建步骤

- 下面将通过一个实例（`Android（Java）` 平台为例）详细介绍每个步骤。
- 具体请看文章：[这是一份很有诚意的 Protocol Buffer 语法详解](https://www.jianshu.com/p/e06ba6249edc)

至此， 关于`Protocol Buffer`的语法 & 如何构建`Protocol Buffer` 消息对象模型讲解完毕。下面将继续讲解如何具体使用`Protocol Buffer`

------

### 6.3 应用到具体平台（`Android`平台）

- 终于到了应用到具体平台项目中的步骤了。

> 此处以 `Android`平台 为例

- 具体步骤如下：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfuu857yj30d20ukwfb.jpg)

具体步骤

- 更加详细介绍请看文章：[Android：手把手教你学会使用Google出品的序列化神器Protocol Buffer](https://www.jianshu.com/p/4575342bc8ad)

至此， 关于`Protocol Buffer`的使用讲解完毕。下面将讲解`Protocol Buffer`的源码分析

------

# 7. 源码分析

### 7.1 核心分析

在下面的源码分析中，主要分析的是：

1. `Protocol Buffer`具体是如何进行序列化 & 反序列化 ？
2. 与 `XML、Json` 相比，`Protocol Buffer` 序列化 & 反序列化速度 为什么如此快 & 序列化后的数据体积这么小？

> 本文主要讲解`Protocol Buffer`在 `Android` 平台上的应用，即 `Java`
> 平台

### 7.2 具体描述

具体的源码分析请看文章：[Android：手把手带你分析 Protocol Buffer使用 源码](https://www.jianshu.com/p/2a5aa5ac6cf6)

至此，关于 `Protocol Buffer`的所有内容讲解完毕，含介绍、特点、具体使用、源码分析、序列化原理等等。

------

# 8. 总结

- 在 **传输数据量较大**的需求场景下，`Protocol Buffer`比`XML、Json` 更小、更快、使用 & 维护更简单！
- 下面用 一张图 总结在 Android平台中使用 `Protocol Buffer` 的整个步骤流程：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfurjsp7j30qe1k4jtx.jpg)

总结

- 关于

  ```
  Protocol Buffer
  ```

  的系列文章请看：

  1. [手把手教你如何安装Protocol Buffer](https://www.jianshu.com/p/92dbe1ef0054)
  2. [这是一份很有诚意的 Protocol Buffer 语法详解](https://www.jianshu.com/p/e06ba6249edc)
  3. [快来看看Google出品的Protocol Buffer，别只会用Json和XML了](https://www.jianshu.com/p/1538bf85dad1)
  4. [Protocol Buffer 序列化原理大揭秘 - 为什么Protocol Buffer性能这么好？](https://www.jianshu.com/p/30ef9b3780d9)
  5. [Android：手把手带你分析 Protocol Buffer使用 源码](https://www.jianshu.com/p/2a5aa5ac6cf6)

- 下一篇文章我将对`Protocol Buffer` 进行**源码分析**，感兴趣的同学可以继续关注本人运营的`Wechat Public Account`：

- [我想给你们介绍一个与众不同的Android微信公众号（福利回赠）](https://www.jianshu.com/p/2e92908af6ec)

- [我想邀请您和我一起写Android（福利回赠）](https://www.jianshu.com/p/2c5d57fb054d)

------

# 



作者：Carson_Ho
链接：https://www.jianshu.com/p/1538bf85dad1
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。