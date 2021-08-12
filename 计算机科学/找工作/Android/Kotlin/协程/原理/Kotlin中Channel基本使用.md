### Channel的源代码如下：

```java
public interface Channel<E> : SendChannel<E>, ReceiveChannel<E> {
   ...
}
```

- Channel的父类有发送消息的SendChannel和接受消息的ReceiveChannel,Channel分为有缓冲区和无缓冲区，无缓冲的通道在发送者和接收者相遇时传输元素。如果发送先被调用，则它将被挂起直到接收被调用， 如果接收先被调用，它将被挂起直到发送被调用。Channel() 工厂函数与 produce和actor 建造器通过一个可选的参数 capacity 来指定 缓冲区大小 。缓冲允许发送者在被挂起前发送多个元素， 就像 BlockingQueue 有指定的容量一样，当缓冲区被占满的时候将会引起阻塞。

### Channel的构建

- GlobalScope 中提供2个函数 produce() 和 actor()

  - produce()函数返回值是一个ReceiveChannel对象，最后一个参数是一个继承SendChannel的对象，使用代码如下：

  ```java
    val data = GlobalScope.produce<String> {
           send("a")
       }.receive()
  ```

  - actor()函数的最后一个参数是一个继承ActorScope类的对象，ActorScope类继承了ReceiveChannel类，用来接受处理函数返回一个SendChannel对象发送的消息，使用代码如下:

  ```java
   GlobalScope.actor<String> {
           val data = receive()
       }.send("a")
  ```

  **produce()和actor()第二个参数capacity 默认值是0，表示构建的channel是无缓冲区的，若重新赋值为大约0，则构建的channel是有缓冲区的。**

  - 使用标准库中的iterator()函数构建一个相似的管道，使用 iterator 替换 produce、yield 替换 send、next 替换 receive、 Iterator 替换 ReceiveChannel 来摆脱协程作用域，你将不再需要 runBlocking。代码如下：

  ```java
  iterator<String> {
       yield("ss")
   }.next()
  ```

  - 直接初始化,send()和receive()函数是挂起函数，只能在协程或者其他挂起函数中调用。代码如下：

  ```java
   GlobalScope.launch {
       val channel = Channel<String>()
       channel.send("11")
       val data = channel.receive()
   }
  ```

  ```java
  **Channel()参数capacity 默认值是0，表示构建的channel是无缓冲区的，若重新赋值为大约0，则构建的channel是有缓冲区的。**
  ```

- 计时器Channel-- ticker (),使用代码如下：

```java
 GlobalScope.launch(Dispatchers.Main) {
            val tickerChannle = ticker(1,
             initialDelayMillis = 0)
            tickerChannle.receive()
            tickerChannle.cancel()
        }
```

### 使用Channel 实现View 防止重复点击

```java
fun View.setOnceClick(block: suspend () -> Unit) {
   val action = GlobalScope.actor<Unit> {
       for (event in channel) block()
   }
   setOnClickListener {
       action.offer(Unit)
   }
}
```

- 在View 类中扩展一个函数setOnceClick(),在该函数中调用View的点击事件函数， GlobalScope.actor()函数返回一个SendChannel 对象，在点击事件中调用offer()函数，发送消息。
- actor()函数的最后一个参数是一个继承ActorScope类的对象，ActorScope类继承了ReceiveChannel类，用来接受处理函数返回一个SendChannel对象发送的消息