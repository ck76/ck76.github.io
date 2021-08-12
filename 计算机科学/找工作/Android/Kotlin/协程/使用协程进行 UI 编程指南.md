[TOC]

本篇教程假定你已经熟悉了包含 [kotlinx.coroutines 指南](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/docs/coroutines-guide.md) 在内的基础协程概念，并提供了有关如何在 UI 应用程序中使用协程的具体示例。

所有的 UI 程序库都有一个共同的特征。即所有的 UI 状态都被限制在单个的主线程中，并且所有更新 UI 的操作都应该发生在该线程中。在使用协程时， 这意味着你需要一个适当的 *协程调度器上下文* 来限制协程运行与 UI 主线程中。

特别是，`kotlinx.coroutines` 为不同的 UI 应用程序库提供了三个协程上下文模块：

- [kotlinx-coroutines-android](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-android) -- `Dispatchers.Main` 为 Android 应用程序提供的上下文。
- [kotlinx-coroutines-javafx](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx) -- `Dispatchers.JavaFx` 为 JavaFX UI 应用程序提供的上下文。
- [kotlinx-coroutines-swing](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-swing) -- `Dispatchers.Swing` 为 Swing UI 应用程序提供的上下文。

当然，UI 调度器被允许通过来自于 `kotlinx-coroutines-core` 的 `Dispatchers.Main` 获得并被 [`ServiceLoader`](https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html) API 发现的相应实现（Android、JavaFx 或 Swing）。 举例来说，假如你编写了一个 JavaFx 应用程序，你使用 `Dispatchers.Main` 或者 `Dispachers.JavaFx` 扩展都是可以的，它们指向同一个对象。

本教程同时包含了所有的 UI 库，因为每个模块中只包含一个长度为几页的对象定义。你可以使用它们中的任何一个作为例子来为你最喜爱的 UI 库编写上下文对象，甚至是没有被包含在本教程中的。



## 体系

本篇教程中的可运行的示例是通过 JavaFx 来呈现的。其优点是所有的示例可以直接在任何运行在操作系统中而不需要模拟器或任何类似的东西，并且它们可以完全独立存在 （每个示例都在同一个文件中）。 关于在 Android 上重现它们需要进行哪些更改（如果有的话）会有单独的注释。

### JavaFx

这个 JavaFx 的基础的示例应用程度由一个包含名为 `hello` 的文字标签的窗口构成，它最初包含“Hello World!”字符串以及一个右下角的粉色圆形 `fab`（floating action button —— 悬浮动作按钮）。

[![JavaFx 的 UI 示例](https://github.com/hltj/kotlinx.coroutines-cn/raw/master/ui/ui-example-javafx.png)](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/ui-example-javafx.png)

JavaFX 应用程序中的 `start` 函数调用了 `setup` 函数，将它引用到 `hello` 与 `fab` 节点上。这是本指南其余部分中放置各种代码的地方：

```kotlin
fun setup(hello: Text, fab: Circle) {
    // 占位
}
```

> 可以在[这里](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx/test/guide/example-ui-basic-01.kt)获取完整代码。

你可以在 Github 上 clone [kotlinx.coroutines](https://github.com/Kotlin/kotlinx.coroutines) 这个项目到你的工作站中并在 IDE 中打开这个项目。所有本教程中的示例都在[`ui/kotlinx-coroutines-javafx`](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx) 模块的 test 文件夹中。 这样的话你就能够运行并观察每一个示例是如何工作的并在你对它们修改时进行实验。

### Android

请跟随这篇教程——[在 Android 中开始使用 Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-android.html)， 来在 Android Studio 中创建一个 Kotlin 项目。我们也鼓励你添加[Android 的 Kotlin 扩展](https://kotlinlang.org/docs/tutorials/android-plugin.html) 到你的应用程序中。

在 Android Studio 2.3 中，您将获得一个类似于下图所示的应用程序：

[![UI example for Android](https://github.com/hltj/kotlinx.coroutines-cn/raw/master/ui/ui-example-android.png)](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/ui-example-android.png)

到你的应用程序的 `context_main.xml` 文件中，并将 ID “hello” 指定给你写有 “Hello World!” 字符串的文本视图， 因此它在您的应用程序中可用作 “hello” 和 Kotlin Android 扩展。粉红色的悬浮动作按钮在已创建的项目模板中已命名为 “fab”。

在你的应用程序的 `MainActivity.kt` 中移除 `fab.setOnClickListener { ... }` 代码块并替换添加 `setup(hello, fab)`的调用作为 `onCreate` 函数的最后一行。 在文件的末尾创建一个占位的 `setup` 函数。 这是本指南其余部分中放置各种代码的地方：

```kotlin
fun setup(hello: TextView, fab: FloatingActionButton) {
    // 占位
}
```

在 `app/build.gradle` 文件的 `dependencies { ... }` 部分中添加 `kotlinx-coroutines-android` 模块的依赖：

```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-coroutines-android:1.3.0-RC"
```

你可以在 Github 上 clone [kotlinx.coroutines](https://github.com/Kotlin/kotlinx.coroutines) 这个项目到你的工作站中。Android 生成的模板项目位于 [`ui/kotlinx-coroutines-android/example-app`](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-android/example-app) 目录。 你可以在 Android Studio 中打开它并跟着本教程在 Android 上学习。

## UI 协程基础

本节将展示协程在 UI 应用程序中的基础用法。

### 启动 UI 协程

`kotlinx-coroutines-javafx` 模块包含 [Dispatchers.JavaFx](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-javafx/kotlinx.coroutines.javafx/kotlinx.coroutines.-dispatchers/-java-fx.html) 调度器，其包含执行 JavaFx 应用程序线程。我们通过 `Main`引入它，使所有呈现的示例都可以轻松的移植到 Android：

```
import kotlinx.coroutines.javafx.JavaFx as Main
```

协程被限制在 UI 主线程就可以自如的做任何更新 UI 的操作，并且可以在主线程中进行无阻塞的挂起。 举例来说，我们可以通过命令式编码来执行动画。下面的代码使用 [launch](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/launch.html) 协程构建器，将文本每秒两次倒序的 从 10 更新到 1：

```kotlin
fun setup(hello: Text, fab: Circle) {
    GlobalScope.launch(Dispatchers.Main) { // 在主线程中启动协程
        for (i in 10 downTo 1) { // 从 10 到 1 的倒计时
            hello.text = "Countdown $i ..." // 更新文本
            delay(500) // 等待半秒钟
        }
        hello.text = "Done!"
    }
}
```

> 可以在[这里](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx/test/guide/example-ui-basic-02.kt)获取完整代码。

所以，这里将发生什么？由于我们在主 UI 上下文中启动协程，我们可以在该协程内部自如的更新 UI，并同时调用就像 [delay](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/delay.html) 这样的 *挂起函数* 。当 `delay` 函数的等待期间UI 并不会冻结，因为它不会阻塞 UI 线程——它只会挂起协程。

> 相应的代码在 Android 应用程序中表现也是类似的。 你只需要在相应的代码中拷贝 `setup` 的函数体到 Android 项目中。

### 取消 UI 协程

我们可以对 `launch` 函数返回的 [Job](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-job/index.html) 对象保持一个引用，来使用它当我们想停止一个任务的时候来取消协程。让我们在粉色按钮被点击的时候来取消协程：

```kotlin
fun setup(hello: Text, fab: Circle) {
    val job = GlobalScope.launch(Dispatchers.Main) { // 在主线程中启动协程
        for (i in 10 downTo 1) { // 从 10 到 1 的倒计时
            hello.text = "Countdown $i ..." // 更新文本
            delay(500) // 等待半秒钟
        }
        hello.text = "Done!"
    }
    fab.onMouseClicked = EventHandler { job.cancel() } // 在点击时取消协程
}
```

> 可以在[这里](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx/test/guide/example-ui-basic-03.kt)获取完整代码。

现在，如果当倒计时仍然在运行时点击圆形按钮，倒计时会停止。 注意，[Job.cancel](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-job/cancel.html) 的调用是完全线程安全和非阻塞的。它仅仅是示意协程取消它的任务，而不会去等待任务事实上的终止。它可以在任何地方被调用。 在已经取消或已完成的协程上调用它不会做任何事情。

> 相关的代码行在 Android 中如下所示：

```kotlin
fab.setOnClickListener { job.cancel() }  // 在点击时取消协程
```

## 在 UI 上下文中使用 actors

在本节中，我们将展示 UI 应用程序如何在其 UI 上下文中使用 actor 以确保被启动的协程的数量没有无限制的增长。

### 协程扩展

我们的目标是写一个扩展 *协程构建器* 函数并命名为 `onClick`， 所以我们可以在该示例代码中展示当每次圆形按钮被点击的时候都会进行倒记时动画：

```kotlin
fun setup(hello: Text, fab: Circle) {
    fab.onClick { // 当圆形按钮被点击的时候启动协程
        for (i in 10 downTo 1) { // 从 10 到 1 的倒记时
            hello.text = "Countdown $i ..." // 更新文本
            delay(500) // 等待半秒钟
        }
        hello.text = "Done!"
    }
}
```

我们的第一个 `onClick` 实现只是在每次鼠标事件到来时启动了一个新的协程并将相应的鼠标事件传递给提供的操作（仅仅在每次我们需要它时）：

```kotlin
fun Node.onClick(action: suspend (MouseEvent) -> Unit) {
    onMouseClicked = EventHandler { event ->
        GlobalScope.launch(Dispatchers.Main) { 
            action(event)
        }
    }
}
```

> 你可以在[这里](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx/test/guide/example-ui-actor-01.kt)获取完整代码。

注意，当每次圆形按钮被点击时，它启动了一个新的协程并都将竞争更新文本。尝试一下。它看起来并不是非常棒。我们将在稍后修正它。

> 在 Android 中，相关的扩展可以写给 `View` 类，所以在上面这段代码中展示的 `setup` 函数可以无需修改就能使用。而在 Android 的 OnClickListener 中 `MouseEvent` 没有被使用，所以省略它。

```kotlin
fun View.onClick(action: suspend () -> Unit) {
    setOnClickListener { 
        GlobalScope.launch(Dispatchers.Main) {
            action()
        }
    }
}
```

### 最多一个并发任务

在启动一个新协程之前，我们可以取消一个存活中的任务以确保最多一个协程正在进行倒计时动画。然而，这通常不是一个好的主意。[cancel](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-job/cancel.html) 函数仅仅被用来指示退出一个协程。协程会协同的进行取消，在这时，做一些不可取消的事情或以其它方式忽略取消信号。一个好的解决方式是使用一个 [actor](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines.channels/actor.html) 来执行任务而不应该进行并发。让我们修改 `onClick` 扩展的实现：

```kotlin
fun Node.onClick(action: suspend (MouseEvent) -> Unit) {
    // 在这个节点中启动一个 actor 来处理所有事件
    val eventActor = GlobalScope.actor<MouseEvent>(Dispatchers.Main) {
        for (event in channel) action(event) // 将事件传递给 action
    }
    // 设置一个监听器来为这个 actor 添加事件
    onMouseClicked = EventHandler { event ->
        eventActor.offer(event)
    }
}
```

> 可以在[这里](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx/test/guide/example-ui-actor-02.kt)获取完整代码。

构成协程和常规事件处理程序的集成基础的关键思想是 [SendChannel](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines.channels/-send-channel/index.html) 上的 [offer](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines.channels/-send-channel/offer.html) 函数不会等待。它会立即将一个元素发送到 actor， 如果可能的话，或者丢弃一个元素。一个 `offer` 事实上返回了一个我们在这里忽略的 `Boolean` 结果。

在这个版本的代码中尝试反复点击圆形按钮。当倒计时动画进行中时， 点击动作会被忽略。这会发生的原因是 actor 正忙于执行而不会从通道中接收元素。 默认的，一个 actor 的邮箱由 `RendezvousChannel` 支持，只有当 `receive` 在运行中的时候 `offer` 操作才会成功。

> 在 Android 中，这里有一个 `View` 在 OnClickListener 中发送事件，所以我们发送一个 `View` 到 actor 来作为信号。 相关的 `View` 类的扩展如下所示：

```kotlin
fun View.onClick(action: suspend (View) -> Unit) {
    // 启动一个 actor
    val eventActor = GlobalScope.actor<View>(Dispatchers.Main) {
        for (event in channel) action(event)
    }
    // 设置一个监听器来启用 actor
    setOnClickListener { 
        eventActor.offer(it)
    }
}
```

### 事件归并

有时处理最近的事件更合适，而不是在我们忙于处理前一个事件的时候忽略事件。[actor](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines.channels/actor.html) 协程构建器接收一个可选的 `capacity` 参数来控制此 actor 用于其邮箱的通道的实现。所有关于可用选项的描述于 [`Channel()`](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines.channels/-channel/index.html) 工厂函数的文档中给出。

让我们修改代码来使用 `ConflatedChannel` 通过 [Channel.CONFLATED](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines.channels/-channel/-c-o-n-f-l-a-t-e-d.html) 修改容量值。这只需要在创建 actor 的这一行作出修改：

```kotlin
fun Node.onClick(action: suspend (MouseEvent) -> Unit) {
    // 在这个 node 上启动一个 actor 来处理所有的事件
    val eventActor = GlobalScope.actor<MouseEvent>(Dispatchers.Main, capacity = Channel.CONFLATED) { // <--- 修改这里
        for (event in channel) action(event) // 将事件传递给 action
    }
    // 设置一个监听器来为这个 actor 添加事件
    onMouseClicked = EventHandler { event ->
        eventActor.offer(event)
    }
}
```

> 可以在[这里](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx/test/guide/example-ui-actor-03.kt)获取完整代码。 在 Android 中你需要在前面的示例中更新 `val eventActor = ...` 这一行。

现在，当动画运行中时如果这个圆形按钮被点击，动画将在结束后重新运行。仅仅一次。 在倒数进行中时，重复点击将被 *合并* ，只有最近的事件才会被处理。

对于必须对高频传入的事件做出反应的 UI 应用程序，这也是一种期望的行为， 事件流通过基于最近收到的更新更新其UI。协程通过使用 `ConflatedChannel` 来避免通过引入事件缓冲而造成的延迟。

您可以在上面的代码行中试验 `capacity` 参数，看看它如何影响代码的行为。 设置 `capacity = Channel.UNLIMITED` 参数来创建协程以及 `LinkedListChannel` 邮箱来缓冲所有的事件。在这个案例中，动画会在单击圆形按钮时运行多次。

## 阻塞操作

本节说明了如何使用 UI 协程来进行线程阻塞操作。

### UI 冻结的问题

如果所有 API 都被编写为永不阻塞执行线程的挂起函数， 那就太好了。然而，通常情况并非如此。有时你需要做一些消耗 CPU 的运算或者只是需要调用第三方的 API 来进行网络访问，比如说，那将阻塞调用它的线程。 你不能在 UI 主线程中那样做，也不能直接在 UI 限定的协程中直接调用，因为那将阻塞 UI 主线程并冻结 UI。

下面的示例将说明这个问题。我们将使用最后一节中的 UI 限定的 `onClick` 事件合并 actor 在 UI 主线程中处理最后一次点击。 在这个例子中，我们将展示[斐波那契数列](https://en.wikipedia.org/wiki/Fibonacci_number)的简单计算：

```kotlin
fun fib(x: Int): Int =
    if (x <= 1) x else fib(x - 1) + fib(x - 2)
```

我们将在每次点击圆圈时计算越来越大的斐波纳契数。 为了让 UI 冻结更明显，还有一个始终在运行的快速计数动画并时刻在 UI 主线程调度器中更新文本：

```kotlin
fun setup(hello: Text, fab: Circle) {
    var result = "none" // 最后一个结果
    // 计数动画
    GlobalScope.launch(Dispatchers.Main) {
        var counter = 0
        while (true) {
            hello.text = "${++counter}: $result"
            delay(100) // 每 100 毫秒更新一次文本
        }
    }
    // 在每次点击时计算下一个斐波那契数
    var x = 1
    fab.onClick {
        result = "fib($x) = ${fib(x)}"
        x++
    }
}
```

> 可以在[这里](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx/test/guide/example-ui-blocking-01.kt)获得完整的 JavaFx 代码。 你可以只拷贝 `fib` 函数和 `setup` 函数的函数体到你的 Android 工程中。

尝试在这个例子中点击圆形按钮。在大约 30 到 40 次点击后我们的简单计算将会变得非常缓慢并且你会立即看到 UI 主线程是如何冻结的，因为动画会在 UI 冻结期间停止运行。

### 结构化并发，生命周期以及协程父子层级结构

一个典型的 UI 应用程序含有大量的具有生命周期的元素。窗口，UI 控制器，活动（即 Android 四大组件中的 Activity，这里直译了），视图，碎片（应该指的是 Android 中的 Fragment，这里直译了）以及其它可视的元素都是可被创建和销毁的。一个长时间运行的协程，在进行一些 IO 或后台计算时，会保留持有相关 UI 元素的引用超过所需要的时间，并阻止垃圾回收机制在整个 UI 对象树不再需要被显示时将其销毁。

这个问题的一个自然的解决方式是关联每一个拥有生命周期并在该 job 的上下文中创建协程的 UI 对象的 job 对象。但是通过关联每一个协程构建器的 job 对象是容易出错的， 它是非常容易被忘记的。对于这个目的，UI 的所有者可以实现 [CoroutineScope](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-coroutine-scope/index.html) 接口，那么每一个协程构建器被定义为了 [CoroutineScope](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-coroutine-scope/index.html) 上的扩展并承袭了没有显示声明的 UI job。 为了简单起见，可以使用 [MainScope()](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-main-scope.html) 工厂函数。它将会自动提供 `Dispatchers.Main` 以及父级 任务。

举例来说，在 Android 应用程序中一个 `Activity` 最初被 *created* 以及被当它不再被需要时 *destroyed* 并且当内存必须被释放时。一个自然的解决方式是绑定一个 `Job` 作为 `Activity` 的单例：

```kotlin
abstract class ScopedAppActivity: AppCompatActivity(), CoroutineScope by MainScope() {
    override fun onDestroy() {
        super.onDestroy()
        cancel() // CoroutineScope.cancel
    } 
}
```

现在，一个继承自 ScopedAppActivity 的 Activity 和 job 发生了关联。

```kotlin
class MainActivity : ScopedAppActivity() {

    fun asyncShowData() = launch { // Activity 的 job 作为父结构时，这里将在 UI 上下文中被调用
        // 实际实现
    }
    
    suspend fun showIOData() {
        val deferred = async(Dispatchers.IO) {
            // 实现
        }
        withContext(Dispatchers.Main) {
          val data = deferred.await()
          // 在 UI 中展示数据
        }
    }
}
```

每一个在 `MainActivity` 中启动的协程都以该 Activity 的 job 作为父级结构并会在 activity 被销毁的时候立即取消。

将 activity 作用域传播给它的视图与 presenters，很多技术可以被使用：

- [coroutineScope](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-coroutine-scope/index.html) 构建起提供了一个嵌套 scope
- 在 presenter 方法参数中接收 [CoroutineScope](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-coroutine-scope/index.html)
- 使方法在 [CoroutineScope](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-coroutine-scope/index.html) 上实现扩展（仅适用于顶级方法）

```kotlin
class ActivityWithPresenters: ScopedAppActivity() {
    fun init() {
        val presenter = Presenter()
        val presenter2 = ScopedPresenter(this)
    }
}

class Presenter {
    suspend fun loadData() = coroutineScope {
        // 外部 activity 的嵌套作用域
    }
    
    suspend fun loadData(uiScope: CoroutineScope) = uiScope.launch {
      // 在 UI 作用域中调用
    }
}

class ScopedPresenter(scope: CoroutineScope): CoroutineScope by scope {
    fun loadData() = launch { // 作为 ActivityWithPresenters 的作用域的扩展
    }
}

suspend fun CoroutineScope.launchInIO() = launch(Dispatchers.IO) {
   // 在调用者的作用域中启动，但使用 IO 调度器
}
```

Job 之间的父子关系形成层次结构。代表执行某些后台工作的协程视图及其上下文可以创建更多的子协程。当父任务被取消时， 整个协程树都会被取消。请参见协程指南中[“子协程”](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/docs/coroutine-context-and-dispatchers.md#children-of-a-coroutine)这一小节的示例。

### 阻塞操作

使用协程在 UI 主线程上修正阻塞操作是非常直接了当的。我们将改造我们的 “阻塞” `fib` 函数为非阻塞的挂起函数来在后台线程执行计算，并使用 [withContext](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/with-context.html) 函数来将它的执行上下文改变为 [Dispatchers.Default](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-dispatchers/-default.html) —— 通过后台线程池支持。 注意，这个 `fib` 函数现在被标记了 `suspend` 修饰符。它在任何地方被调用的时候都不会阻塞该协程，但是它将会在后台线程执行计算工作时被挂起：

```kotlin
suspend fun fib(x: Int): Int = withContext(Dispatchers.Default) {
    if (x <= 1) x else fib(x - 1) + fib(x - 2)
}
```

> 可以在[这里](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx/test/guide/example-ui-blocking-02.kt)获取完整代码。

你可以运行这段代码并验证当大量的计算斐波那契数时 UI 并不会被冻结。 然而，这段代码计算 `fib` 有些慢，因为每次递归都会调用 `fib` 去调用 `withContext`。这在实践中并不是一个大问题，因为 `withContext` 会足够智能的去检查协程已经准备好运行在需要的上下文中并避免再次将协程发送到另一个线程的开销。 尽管如此，这在原始代码上可以看见，它并不执行任何其他操作，但只在调用 `withContext` 时添加整数。对于一些更实质的代码，额外的 `withContext` 调用开销不会很重要。

但这部分的 `fib` 实现可以像之前一样快速运行，但是在后台线程中，通过重命名原始的 `fib` 函数为 `fibBlocking` 并在上层的 `fib` 函数的 `withContext` 包装中调用 `fibBlocking`：

```kotlin
suspend fun fib(x: Int): Int = withContext(Dispatchers.Default) {
    fibBlocking(x)
}

fun fibBlocking(x: Int): Int = 
    if (x <= 1) x else fibBlocking(x - 1) + fibBlocking(x - 2)
```

> 可以在[这里](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx/test/guide/example-ui-blocking-03.kt)获取完整代码。

现在你可以享受全速的，不阻塞 UI 主线程的简单斐波那契计算。 我们需要的都在 `withContext(Dispatchers.Default)`中。

注意，由于在我们的代码中 `fib` 函数是被单 actor 调用的，这里在任何给定时间最多只会进行一个计算，所以这段代码具有天然的资源利用率限制。 它会饱和占用最多一个 CPU 核心。

## 高级主题

本节包含了各种高级主题。

### 没有调度器时在 UI 事件处理器中启动协程

让我们在 `setup` 中编写以下代码，以便在 UI 线程中启动协程时可以以可视化的方式观察执行顺序：

```kotlin
fun setup(hello: Text, fab: Circle) {
    fab.onMouseClicked = EventHandler {
        println("Before launch")
        GlobalScope.launch(Dispatchers.Main) {
            println("Inside coroutine")
            delay(100)
            println("After delay")
        } 
        println("After launch")
    }
}
```

> 可以在[这里](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx/test/guide/example-ui-advanced-01.kt)获得完整的 JavaFX 代码。

当我们运行这段代码并点击粉色圆形按钮，下面的信息将会在控制台中打印：

```
Before launch
After launch
Inside coroutine
After delay
```

你可以看到，当协程被发送到 UI 主线程上会稍后执行， 在 [launch](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/launch.html) 后立即继续执行。所有 `kotlinx.coroutines` 中的 UI 调度器都会实现这个方法。为什么呢？

基本上，这里选择介于 “JS 风格” 的异步方法（异步操作总是推迟在事件调度线程中执行）与 “C# 风格” 的方法 （异步操作总是在调用它的线程上执行并直到第一个挂起点）之间的风格。 然而，C# 方法看起来更高效，它在建议下结束就像 “使用 `yield` 如果你需要去....”。 这是易错的。JS 风格的方法是更加连贯的 并且不需要程序员去思考他们是否需要让步。

然而，在当协程从事件处理程序启动时周围没有其它代码这种特殊案例中， 这个额外的调度确实增加了额外的开销，而没有带来任何额外的价值。 在这个案例中一个可选的 [CoroutineStart](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-coroutine-start/index.html) 参数可赋值给 [launch](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/launch.html)、[async](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/async.html) 以及 [actor](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines.channels/actor.html) 协程构建器 来进行性能优化。 将它的值设置为 [CoroutineStart.UNDISPATCHED](https://kotlin.github.io/kotlinx.coroutines/kotlinx-coroutines-core/kotlinx.coroutines/-coroutine-start/-u-n-d-i-s-p-a-t-c-h-e-d.html) 可以更有效率的开始立即执行协程并直到第一个挂起点，如同下面的例子所示：

```kotlin
fun setup(hello: Text, fab: Circle) {
    fab.onMouseClicked = EventHandler {
        println("Before launch")
        GlobalScope.launch(Dispatchers.Main, CoroutineStart.UNDISPATCHED) { // <--- 通知这次改变
            println("Inside coroutine")
            delay(100)                            // <--- 这里是协程挂起的地方
            println("After delay")
        }
        println("After launch")
    }
}
```

> 可以在[这里](https://github.com/hltj/kotlinx.coroutines-cn/blob/master/ui/kotlinx-coroutines-javafx/test/guide/example-ui-advanced-02.kt)获得完整的 JavaFx 代码。

它在点击后将会打印如下信息，确认这段代码在协程启动后会立即执行：

```kotlin
Before launch
Inside coroutine
After launch
After delay
```