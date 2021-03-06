#### 一、对AOP的初印象

**首先先给出一段比较专业的术语**（来自百度）：

```
在软件业，AOP为Aspect Oriented Programming的缩写，意为：面向切面编程，通过预编译方
式和运行期动态代理实现程序功能的统一维护的一种技术。AOP是OOP的延续，是软件开发中的一个
热点，也是Spring框架中的一个重要内容，是函数式编程的一种衍生范型。利用AOP可以对业务逻辑
的各个部分进行隔离，从而使得业务逻辑各部分之间的耦合度降低，提高程序的可重用性，同时提高
了开发的效率。
12345
```

**然后我们举一个比较容易理解的例子**（来自：[Spring 之 AOP](https://www.jianshu.com/p/570c5283b1fc)）：

要理解切面编程，就需要先理解什么是切面。用刀把一个西瓜分成两瓣，切开的切口就是切面；炒菜，锅与炉子共同来完成炒菜，锅与炉子就是切面。web层级设计中，web层->网关层->服务层->数据层，每一层之间也是一个切面。**编程中，对象与对象之间，方法与方法之间，模块与模块之间都是一个个切面。**

我们一般做活动的时候，一般对每一个接口都会做活动的有效性校验（是否开始、是否结束等等）、以及这个接口是不是需要用户登录。

按照正常的逻辑，我们可以这么做。
![这里写图片描述](https://tva1.sinaimg.cn/large/008eGmZEly1gmyr04npvzj30o50juta8.jpg)

这有个问题就是，有多少接口，就要多少次代码copy。对于一个“懒人”，这是不可容忍的。好，提出一个公共方法，每个接口都来调用这个接口。这里有点切面的味道了。
![这里写图片描述](https://tva1.sinaimg.cn/large/008eGmZEly1gmyr02b4uxj30wm0l6jsj.jpg)

同样有个问题，我虽然不用每次都copy代码了，但是，每个接口总得要调用这个方法吧。于是就有了切面的概念，我将方法注入到接口调用的某个地方（切点）。

![这里写图片描述](https://tva1.sinaimg.cn/large/008eGmZEly1gmyr01if7rj30wk0f3dho.jpg)

这样接口只需要关心具体的业务，而不需要关注其他非该接口关注的逻辑或处理。
**红框处，就是面向切面编程。**

#### 二、AOP中的相关概念

看过了上面的例子，我想大家脑中对AOP已经有了一个大致的雏形，但是又对上面提到的切面之类的术语有一些模糊的地方，接下来就来讲解一下AOP中的相关概念，了解了AOP中的概念，才能真正的掌握AOP的精髓。
**这里还是先给出一个比较专业的概念定义**：

- `Aspect`（切面）： Aspect 声明类似于 Java 中的类声明，在 Aspect 中会包含着一些 Pointcut 以及相应的 Advice。
- `Joint point`（连接点）：表示在程序中明确定义的点，典型的包括方法调用，对类成员的访问以及异常处理程序块的执行等等，它自身还可以嵌套其它 joint point。
- `Pointcut`（切点）：表示一组 joint point，这些 joint point 或是通过逻辑关系组合起来，或是通过通配、正则表达式等方式集中起来，它定义了相应的 Advice 将要发生的地方。
- `Advice`（增强）：Advice 定义了在 `Pointcut` 里面定义的程序点具体要做的操作，它通过 before、after 和 around 来区别是在每个 joint point 之前、之后还是代替执行的代码。
- `Target`（目标对象）：织入 `Advice` 的目标对象.。
- `Weaving`（织入）：将 `Aspect` 和其他对象连接起来, 并创建 `Advice`d object 的过程

**然后举一个容易理解的例子**：
看完了上面的理论部分知识, 我相信还是会有不少朋友感觉到 AOP 的概念还是很模糊, 对 AOP 中的各种概念理解的还不是很透彻. 其实这很正常, 因为 AOP 中的概念是在是太多了, 我当时也是花了老大劲才梳理清楚的.
下面我以一个简单的例子来比喻一下 AOP 中 `Aspect`, `Joint point`, `Pointcut` 与 `Advice`之间的关系.
让我们来假设一下, 从前有一个叫爪哇的小县城, 在一个月黑风高的晚上, 这个县城中发生了命案. 作案的凶手十分狡猾, 现场没有留下什么有价值的线索. 不过万幸的是, 刚从隔壁回来的老王恰好在这时候无意中发现了凶手行凶的过程, 但是由于天色已晚, 加上凶手蒙着面, 老王并没有看清凶手的面目, 只知道凶手是个男性, 身高约七尺五寸. 爪哇县的县令根据老王的描述, 对守门的士兵下命令说: 凡是发现有身高七尺五寸的男性, 都要抓过来审问. 士兵当然不敢违背县令的命令, 只好把进出城的所有符合条件的人都抓了起来.

来让我们看一下上面的一个小故事和 AOP 到底有什么对应关系.
首先我们知道, 在 Spring AOP 中 `Joint point` 指代的是所有方法的执行点, 而 point cut 是一个描述信息, 它修饰的是 `Joint point`, 通过 point cut, 我们就可以确定哪些 `Joint point` 可以被织入 `Advice`. 对应到我们在上面举的例子, 我们可以做一个简单的类比, **`Joint point` 就相当于 爪哇的小县城里的百姓**,**`pointcut` 就相当于 老王所做的指控, 即凶手是个男性, 身高约七尺五寸**, **而 `Advice` 则是施加在符合老王所描述的嫌疑人的动作: 抓过来审问**.
为什么可以这样类比呢?

- `Joint point` ： 爪哇的小县城里的百姓: 因为根据定义, `Joint point` 是所有可能被织入 `Advice` 的候选的点, 在 Spring AOP中, 则可以认为所有方法执行点都是 `Joint point`. 而在我们上面的例子中, 命案发生在小县城中, 按理说在此县城中的所有人都有可能是嫌疑人.
- `Pointcut` ：男性, 身高约七尺五寸: 我们知道, 所有的方法(joint point) 都可以织入 `Advice`, 但是我们并不希望在所有方法上都织入 `Advice`, 而 `Pointcut` 的作用就是提供一组规则来匹配joinpoint, 给满足规则的 joinpoint 添加 `Advice`. 同理, 对于县令来说, 他再昏庸, 也知道不能把县城中的所有百姓都抓起来审问, 而是根据凶手是个男性, 身高约七尺五寸, 把符合条件的人抓起来. 在这里 凶手是个男性, 身高约七尺五寸 就是一个修饰谓语, 它限定了凶手的范围, 满足此修饰规则的百姓都是嫌疑人, 都需要抓起来审问.
- `Advice` ：抓过来审问, `Advice` 是一个动作, 即一段 Java 代码, 这段 Java 代码是作用于 point cut 所限定的那些 `Joint point` 上的. 同理, 对比到我们的例子中, 抓过来审问 这个动作就是对作用于那些满足 男性, 身高约七尺五寸 的爪哇的小县城里的百姓.
- `Aspect`:：`Aspect` 是 point cut 与 `Advice` 的组合, 因此在这里我们就可以类比: “根据老王的线索, 凡是发现有身高七尺五寸的男性, 都要抓过来审问” 这一整个动作可以被认为是一个 `Aspect`.

**最后是一个描述这些概念之间关系的图**：
![这里写图片描述](https://img-blog.csdn.net/20180530175605692?watermark/2/text/aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L3E5ODIxNTE3NTY=/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70)

#### 三、其他的一些内容

`AOP`中的`Joinpoint`可以有多种类型：构造方法调用，字段的设置和获取，方法的调用，方法的执行，异常的处理执行，类的初始化。也就是说在`AOP`的概念中我们可以在上面的这些`Joinpoint`上织入我们自定义的`Advice`，但是在`Spring`中却没有实现上面所有的`joinpoint`，确切的说，`Spring`只支持方法执行类型的`Joinpoint`。

**Advice 的类型**

- `before advice`, 在 join point 前被执行的 advice. 虽然 before advice 是在 join point 前被执行, 但是它并不能够阻止 join point 的执行, 除非发生了异常(即我们在 before advice 代码中, 不能人为地决定是否继续执行 join point 中的代码)
- `after return advice`, 在一个 join point 正常返回后执行的 advice
- `after throwing advice`, 当一个 join point 抛出异常后执行的 advice
- `after(final) advice`, 无论一个 join point 是正常退出还是发生了异常, 都会被执行的 advice.
- `around advice`, 在 join point 前和 joint point 退出后都执行的 advice. 这个是最常用的 advice.
- `introduction`，introduction可以为原有的对象增加新的属性和方法。

在`Spring`中，通过动态代理和动态字节码技术实现了`AOP`，这些内容，我们将在以后进行讲解。