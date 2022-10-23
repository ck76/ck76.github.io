

## channel的底层数据结构

channel是golang中用来实现多个goroutine通信的管道，它的底层是一个叫做hchan的结构体。在go的runtime包下。

### 数据结构

```go
type hchan struct {
  //channel分为无缓冲和有缓冲两种。
  //对于有缓冲的channel存储数据，借助的是如下循环数组的结构
	qcount   uint           // 循环数组中的元素数量
	dataqsiz uint           // 循环数组的长度
	buf      unsafe.Pointer // 指向底层循环数组的指针
	elemsize uint16 //能够收发元素的大小
  

	closed   uint32   //channel是否关闭的标志
	elemtype *_type //channel中的元素类型
  
  //有缓冲channel内的缓冲数组会被作为一个“环型”来使用。
  //当下标超过数组容量后会回到第一个位置，所以需要有两个字段记录当前读和写的下标位置
	sendx    uint   // 下一次发送数据的下标位置
	recvx    uint   // 下一次读取数据的下标位置
  
  //当循环数组中没有数据时，收到了接收请求，那么接收数据的变量地址将会写入读等待队列
  //当循环数组中数据已满时，收到了发送请求，那么发送数据的变量地址将写入写等待队列
	recvq    waitq  // 读等待队列
	sendq    waitq  // 写等待队列


	lock mutex //互斥锁，保证读写channel时不存在并发竞争问题
}
复制代码
```

**对应图解如下：**

![img](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/a20f10cd62284684963a3a1edd44a90e~tplv-k3u1fbpfcp-zoom-in-crop-mark:3024:0:0:0.awebp)

总结hchan结构体的主要组成部分有四个：

- 用来保存goroutine之间传递数据的循环链表。=====> buf。
- 用来记录此循环链表当前发送或接收数据的下标值。=====> sendx和recvx。
- 用于保存向该chan发送和从改chan接收数据的goroutine的队列。=====> sendq 和 recvq
- 保证channel写入和读取数据时线程安全的锁。 =====> lock

### 举个栗子

```go
//G1
func sendTask(taskList []Task) {
	...
  
	ch:=make(chan Task, 4) // 初始化长度为4的channel
	for _,task:=range taskList {
		ch <- task  //发送任务到channel
	}
  
	...
}

//G2
func handleTask(ch chan Task) {
	for {
		task:= <-ch //接收任务
		process(task) //处理任务
	}
}
复制代码
```

ch是长度为4的带缓冲的channel，G1是发送者，G2是接收者

- 初始hchan结构体重的buf为空，sendx和recvx均为0。
- 当G1向ch里发送数据时，首先会对buf加锁，然后**将数据copy到buf中**，然后sendx++，然后释放对buf的锁。
- 当G2消费ch的时候，会首先对buf加锁，然后将buf中的**数据copy到task变量对应的内存里**，然后recvx++,并释放锁。

可以发现整个过程，G1和G2没有共享的内存，**底层是通过hchan结构体的buf，并使用copy内存的方式进行通信，最后达到了共享内存的目的**，这里也体现了Go中的CSP并发模型。

> Go中的CSP并发模型即是通过goroutine和channel实现的。
>
> CSP并发模型：不要以共享内存的方式来通信，相反，要通过通信的方式来共享内存。

**那么当channel中的缓存满了之后会发生什么呢？**

首先简单了解下**GMP的概念**。相关的数据模型如下图:

![img](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/0ad2bc00a330431ba9a4c828b8d04c8c~tplv-k3u1fbpfcp-zoom-in-crop-mark:3024:0:0:0.awebp)

【G】goroutine是Golang实现的用户空间的轻量级的线程

【M】代表操作系统线程

【P】处理器, 它包含了待运行goroutine。

如果线程M想运行goroutine，必须先获取P，从P中获取goroutine执行。

当G1向buf已经满了的ch发送数据的时候，检测到hchan的buf已经满了，会通知调度器，调度器会将G1的状态设置为waiting, 并移除与线程M的联系，然后从P的runqueue中选择一个goroutine在线程M中执行，此时G1就是阻塞状态，但是不是操作系统的线程阻塞，所以这个时候只用消耗少量的资源。

![img](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/0a970d0a7dbe47dcb476521d63b25546~tplv-k3u1fbpfcp-zoom-in-crop-mark:3024:0:0:0.awebp)

那么G1设置为waiting状态后去哪了？怎们去resume呢？我们再回到hchan结构体，注意到hchan有个sendq的成员，其类型是waitq，查看源码如下：

```go
type hchan struct {
	...
  recvq    waitq  // 读等待队列
	sendq    waitq  // 写等待队列
	...
}

type waitq struct {
	first *sudog
	last *sudog
}
复制代码
```

实际上，当G1变为waiting状态后，会创建一个代表自己的sudog的结构，然后放到sendq这个list中，sudog结构中保存了channel相关的变量的指针(如果该Goroutine是sender，那么保存的是待发送数据的变量的地址，如果是receiver则为接收数据的变量的地址，之所以是地址，前面我们提到在传输数据的时候使用的是copy的方式)

![img](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/30991767131a444a82334aafb669c3a3~tplv-k3u1fbpfcp-zoom-in-crop-mark:3024:0:0:0.awebp)

当G2从ch中接收一个数据时，会通知调度器，设置G1的状态为runnable，然后将加入P的runqueue里，等待线程执行.

```go
func G2(){
  t := <-ch
}
复制代码
```

![img](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/a743e18e102d4ac7a1ed380f05d3c30d~tplv-k3u1fbpfcp-zoom-in-crop-mark:3024:0:0:0.awebp)

前面我们是假设G1先运行，如果G2先运行会怎么样呢？

如果G2先运行，那么G2会从一个empty的channel里取数据，这个时候G2就会阻塞，和前面介绍的G1阻塞一样，G2也会创建一个sudog结构体，保存接收数据的变量的地址，但是该sudog结构体是放到了recvq列表里。

![img](https://p3-juejin.byteimg.com/tos-cn-i-k3u1fbpfcp/e8c9b98336ae4d50b3a4ad8aa340a0d5~tplv-k3u1fbpfcp-zoom-in-crop-mark:3024:0:0:0.awebp)

当G1向ch发送数据的时候，为了提升效率，**runtime并不会对hchan结构体题的buf进行加锁，而是直接将G1里的发送到ch的数据copy到了G2 sudog里对应的elem指向的内存地址！【不通过buf】**

这时候，张三道友抛出了三个问题:

- **为什么在第一种情况下，即G1向缓存满的channel中发送数据时被阻塞。在G2后来接收时，不将阻塞的G1发送的数据直接拷贝到G2中呢？**

  这是因为channel中的数据是队列的，遵循先进先出的原则，当有消费者G2接收数据时，需要先接收缓存中的数据，即buf中的数据，而不是直接消费阻塞的G1中的数据。

- **多个goroutine向有缓存的channel接收/发送数据时，可以保证顺序吗？**

  ```go
  func main(){
  	cache:=make(chan int,3)
  	go func() {
  		for i:=0;i< 3;i++ {
  		cache<-i
  	}
  	}()
  	time.Sleep(time.Second)
    //休眠1秒钟，保证channel中的数据已经写入完整
  	go getCache("gorouine1",cache)
  	go getCache("gorouine2",cache)
  	go getCache("gorouine3",cache)
  	time.Sleep(time.Second)
  }
  
  func getCache(routine string,cache <-chan int) {
  	for  {
  		select {
  		case i:=<-cache:
  			fmt.Printf("%s:%d\n",routine,i)
  		}
  	}
  }
  复制代码
  ```

  很多道友在工作中应用channel时遇到上述场景都会默认为是有序的，即认为输出结果应该是：

  ```bash
  gorouine1:0
  gorouine2:1
  gorouine3:2
  复制代码
  ```

  但实则不然，输出结果如下：

  ```bash
  $go run main.go
  gorouine3:1
  gorouine2:2
  gorouine1:0
  复制代码
  ```

  这里其实主要需要明确两点：

  - channel中的数据遵循队列先进先出原则。
  - 每一个goroutine抢到处理器的时间点不一致，gorouine的执行本身不能保证顺序。

  即代码中先写的gorouine并不能保证先从channel中获取数据，或者发送数据。但是先执行的gorouine与后执行的goroutine在channel中获取的数据肯定是有序的。

- **Channel为什么是线程安全的？**

  在对buf中的数据进行入队和出队操作时，为当前chnnel使用了互斥锁，防止多个线程并发修改数据

## channel的用法

### 使用for range 读取channel

```go
for i := range ch{
    fmt.Println(i)
}
复制代码
```

- 场景：当需要不断从channel读取数据时
- 原理：使用`for-range`读取channel，这样既安全又便利，当channel关闭时，for循环会自动退出，无需主动监测channel是否关闭，可以防止读取已经关闭的channel，造成读到数据为通道所存储的数据类型的零值。

### 使用_,ok判断channel是否关闭

```go
if v, ok := <- ch; ok {
    fmt.Println(v)
}
复制代码
```

- 场景：读channel，但不确定channel是否关闭时

- 原理：读已关闭的channel会得到零值，如果不确定channel，需要使用

  ```
  ok
  ```

  进行检测。ok的结果和含义：

  - `true`：读到数据，并且通道没有关闭。
  - `false`：通道关闭，无数据读到。

### 使用select处理多个channel

```go
for{
  select {
  case <-ch1:
  	process1()
  	return
  case <-ch2:
  	process2()
  	return
	}
}
复制代码
```

- 场景：需要对多个通道进行同时处理，但只处理最先发生的channel时

- 原理：

  ```
  select
  ```

  可以同时监控多个通道的情况，只处理未阻塞的case。

  - 当通道为nil时，对应的case永远为阻塞。
  - 如果channel已经关闭，则这个case是非阻塞的，每次select都可能会被执行到。
  - 如果多个channel都处于非阻塞态，则select会随机选择一个执行。

### 关闭channel的注意事项

- 一个 channel不能多次关闭，会导致painc
- 向一个已经关闭了的 channel发送数据会导致panic

## 面试点总结

- Go channel的底层数据结构以及工作原理？
- 向缓存满的channel写数据会发生什么？
- 向没有数据的channel读数据会发生什么？
- 简述channel的日常用法？


作者：小道哥
链接：https://juejin.cn/post/7037656471210819614
来源：稀土掘金
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。