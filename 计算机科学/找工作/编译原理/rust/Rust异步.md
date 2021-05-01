什么是异步开发模式

什么是异步开发模式，事件驱动、非阻塞的开发模式。

为什么会需要异步框架呢，原因是性能，就是快

异步框架难的地方有几个：异步的编程方式、事件、线程/协程的处理等

让我们来看看 Rust 以及相关框架 Tokio 和 async-std 是怎么做的

## 先了解 std::Waker

task 就是未来要执行的任务，称为异步任务。对于异步的操作，有一个点，就是能够控制任务。让任务能够在合适的时候启动，以及在没有事的时候 pending。为了达到这个目标，rust 设计了 Waker 这个机制

Waker 包含以下几个内容：

- Context : 上下文，在这里，就是包装 Waker 的入口
- RawWaker : 生成 Waker
- RawWakerVTable : 是一个虚函数指针表(Virtual Function Pointer Table, vtable)，指向 RawWaker 对象的指针的列表
- Waker : 通知任务准备启动的入口

以上几个的关系是这样：

![img](https://pic4.zhimg.com/v2-64ac7f896f32fb4947b54a0b1733a5fb_r.jpg)Std::task 结构图

Context 结构里面定义一个 Waker，实现了和 Waker 的相互转换。RawWaker 定义了一个指向任何数据类型的指针，和虚函数表，用来实现和虚函数的绑定、RawWakerVTable 定义了虚函数的几种操作，其中wake 和 wake_by_ref 和Waker 的实例对应

Waker 里面有两个重要的操作：wake() 和 clone()

wake()的代码如下：

```rust
pub fn wake(self) {
        // The actual wakeup call is delegated through a virtual function call
        // to the implementation which is defined by the executor.
        let wake = self.waker.vtable.wake;
        let data = self.waker.data;

        // Don't call `drop` -- the waker will be consumed by `wake`.
        crate::mem::forget(self);

        // SAFETY: This is safe because `Waker::from_raw` is the only way
        // to initialize `wake` and `data` requiring the user to acknowledge
        // that the contract of `RawWaker` is upheld.
        unsafe { (wake)(data) };
    }
```

wake() 的作用是让 task 再次启动，并且获得 pending 之前的信息(堆栈和函数操作)。这个代码里面的 vtable 和 data 就是 RawWaker 里面的数据。中间有一个操作 mem::forget(self)，forget 的作用是让 self 实例进入一个不能操作的状态，但是实例还在，可以下次调用。forget 是调用了 mem crate 里面 ManuallyDrop::new() 。ManuallyDrop 能够控制编译器，让其不要自动去调用实例的解构函数 (destructor)，这样就能保存之前函数运行的信息。forget 调用 ManuallDrop::new() ，就是把 self 实例创建成一个 ManuallyDrop，获得之前 Waker 的信息，这样在切换到另外一个runtime 或者 thread 的时候，之前的 Waker 信息就同步过去，不会随着进程结束而解构。

clone() 的代码如下：

```rust
impl Clone for Waker {
    #[inline]
    fn clone(&self) -> Self {
        Waker {
            // SAFETY: This is safe because `Waker::from_raw` is the only way
            // to initialize `clone` and `data` requiring the user to acknowledge
            // that the contract of [`RawWaker`] is upheld.
            waker: unsafe { (self.waker.vtable.clone)(self.waker.data) },
        }
    }
}

```

clone() 的作用是 clone 这个 waker ，这个用在 task 在要进入 Pending 之前，把获得数据 clone 一份到一个全局的数据结构里面，下一次调用的时候，不会丢失。

为什么会设计一个 Waker ? 在 task 进入 pending 的时候用一个全局的方法来保存状态，下次启动再调用不就可以了吗？我猜测主要是为了灵活的和性能。还有一个原因，task 很多时候是以闭包的形式出现，闭包可以获取所在进程的环境变量，但是自己没有 heap allocation 。这样的话，其实就很难通过全局或者参数传递的方法来实现。

Withoutboats 有一篇文章：[What does a waker do?](https://link.zhihu.com/?target=https%3A//boats.gitlab.io/blog/post/wakers-i/)。说了一个意思是，在异步过程中，有一个 Poll(这个下一篇会讲到)，传入 Future 的值(data和vtable)会连续传递，就是 Poll 的第二个参数，Context，里面实际是 Waker。直到有一个事件触发 Future 重新启动，这个值也重新使用。但是这个传递过程是动态的，因为也可能是多线程的的等待获取。所以重新设计了 Waker。Waker 有一个关键操作， Clone()。在事件触发，Waker 启动的时候，一定要 impl Clone。还是上面的原因，Waker 存在的运行时可能动态的，也可能是多派发的。所以 Waker 必须要自我 clone。然后 Clone 操作的返回必须是 Waker，而不是 self。也就是重新建立一个 Waker，而不是引用到之前的 Waker。

总结一下，Waker 是为了支持异步任务的灵活控制而设计的。让任务能够在需要的时候启动，和空闲的时候停止。那任务是什么呢？我们需要理解两个概念 Future 和 Task，让我们先理解一下 Future 是什么？

> task 是 rust 的 libcore 的模块

## Future ：异步的核心概念

Future 意思就是未来要执行的动作 。按照 Aaron Turon 的定义，future 本质上表达一个还没有准备好的值。通常，future 的完成(值准备好了)取决于事件在某处完成了某个事件。

> In essence ,afuture represents a value tha might not be ready yet. Usually, the future becomes complete (the value is ready ) due to an event happening somewhere else.
> [原文](https://link.zhihu.com/?target=https%3A//aturon.github.io/blog/2016/09/07/futures-design/)

future 这个 trait 定义如下：

```rust
pub trait Future {
  type Output;
  fn poll(self:Pin<&mut Self>,cx: &mut Context <`_>) -> Poll<Self::Output>;
}
enum Poll<T> {
  Ready(T),
  Pending,
}
```

Output 是 Future 完成之后的类型 poll 有两个状态 Ready 和 Pending ，当 future 还没有完成，poll 返回 Pending 状态。同时启动一个 waker，记录到 event loop（或者 reactor） 里面。当操作 Ready 的时候，event loop 再次调用 poll，返回 ready ，然后 runtime 会启动一个 event loop 和线程来执行操作。

在 future 在 Ready 的状态容易理解，就完成后续操作就行。但是 poll 在 Pending 的状态下就面临一些问题。因为 future 在执行到 pending 的时候，需要停下来，然后就像 Aaron Turon 说的，在其他某个事件完成之后，再启动这个 future 执行后续的动作。

这就需要保存 future 的状态，然后在之后再启动。定时启动的机制是通过 waker 里面的 mutex + condvar 来实现的，在 tokio 里，包装成了 park/unpark 。这个在后面一章详细描述。

保存 future 的状态，就需要保存 future 的的相关信息，包括引用的数据。但是在 future 停止再启动的时候，程序的运行空间会变化，例如到了另外一个线程。引用就指向了不对的位置，导致问题。这个问题在 rust 里面，用pin/unpin来解决。

Future 里面的 poll 。有两个 Impl，针对 Unpin 和 Pin 类型。

```rust
impl<F: ?Sized + Future + Unpin> Future for &mut F {
    type Output = F::Output;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        F::poll(Pin::new(&mut **self), cx)
    }
}

impl<P> Future for Pin<P>
where
    P: Unpin + ops::DerefMut<Target: Future>,
{
    type Output = <<P as ops::Deref>::Target as Future>::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Pin::get_mut(self).as_mut().poll(cx)
    }
}
```

Pin 类型能够让数据在内存中固定。withoutboats 有一篇 [blog](https://link.zhihu.com/?target=https%3A//boats.gitlab.io/blog/post/2018-01-25-async-i-self-referential-structs/)，详细说明了自引用结构在 MOVE 之后产生的问题。Pin 能够解决这类问题。在上面的代码里面，看到针对 unpin 的类型，也增加了 Pin::new 来保障数据在内存中固定

Future 的 poll 的第二个参数是 Context 类型，也就是 waker 的封装。

```rust
fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output>;
```

这个 Context 用来保存 future 的状态，在 poll 是 Pending 的时候保存 future 的信息和状态，在 poll 到 Ready 的时候，再执行。所以在每次 poll 之后，如果发现不是 Ready 的状态，都会重新把这个 Context 带入到一个新的 future 等待下次事件触发调用。

Future 和 Pin 构成了 rust async/await 的基础。在函数前面加上 async ，就把函数包装称为了一个 Future；Future 后面加上 .await，就执行 Future 的 poll 操作。例如：

```rust
async fn read_file(path: &str) -> io::Result<String> {
  let mut file = File::open(path).await?;
  let mut contentx = String::new();
  file.read_to_string(&mut contexts).await?;
  Ok(contents)
}
```

async 在函数前面，把函数包装为一个

```rust
Future<output = io::Result<String>>
```

下面就以 Tokio 为例来说明以上几个内容

- executor ： 执行任务的基础。线程、协程、进程或者其他计算运行时
- scheduler：调度方式，针对不同的计算调度不同的 executor 来运行
- event : 事件管理，Waker 是个基础，但是需要更强大的事件管理方式
- task ：Future 是小片的执行操作，要完整一个完整的任务，需要更强大的方式，就是task

如果要设计一个高效完整的异步框架。在 Future 和 Waker 的基础上，还需要提供几个部分：

刚才提到，Future 是未来要执行的动作。那怎么执行 Future，就需要在代码里调用 Poll，或者在函数后面增加 .await 。Future 的设计是为了提供异步执行的模式，为什么会有异步执行的模式，主要是为了性能。能够按照计算机的运行方式，采用事件触发，把任务切成小的操作步骤，更高效的调度，来达到高性能。

看到这里，可能会有些迷惑。还记得之前提到的 ，Future 是未来要执行的动作。这些组合以及执行顺序，就有点类似编织未来程序执行的路径和方式。但是 Future 如果不被 Poll 或者 .await，Future 是静态的。这个和我们平时写同步执行代码有相似，也有不同。不同的地方在于，如果要获得一个中间判断的状态，就需要 Poll。如果要根据输入做动作，也需要在 Poll 的下面写对应的 match。感觉上会比较复杂。好在 Rust 提供 async / .await 的方式，能够让我们按照同步顺序操作的思路来写，就像上面描述的 read_file 函数。

当然， Future 不止针对两个的组合。也有针对 3、4 甚至多个组合。Tokio 就有一个 try_join3的组合。组合方式也不止这几种，[http://crate.io](https://link.zhihu.com/?target=http%3A//crate.io) 里面有一些 crates ，针对 Flattern ，还有 FlatternSink 和 FlatternStream，以及其他组合方式

- Join 比较容易理解。有两个 Future ，L 和 R 。先检查 L 是不是 Ready，如果 Ready ，再检查 R 的 Output 是不是有值（并没有 Poll L）。如果是，则把 L 和 R 的 Output 组合成一个 tuple 作为 Join 之后的 Output，然后返回 Poll::Ready 状态
- TryJoin 和 Join 类似。先检查 L 是不是 Ready，然后检查 L 的 Output 是不是有错误，如果有错误，就返回Err。然后检查 R 的 Output 是不是有值，如果有值，就把 L 和 R 的 Output 合并返回 Poll:Ready。和 Join 相比，加了一步，检查 L 的 Output 是不是有错误
- Race 是两个比谁更快的意思。先检查 L 是不是 Ready，如果Ready，就去 执行 L。然后对 R 再做同样的操作。如果其中有一个完成，就算 Race 完成，另外一个就不管了。Future 退出。当然，这个里面，L 占有一点先发优势，因为先执行 L
- TryRace 和 TryJoin 类似，就不在重复描述
- Flattern ：第一个 Future 的输出，是第二个 Future 的输入。也就是嵌套 Future 。类似这样： async{ async {1} }。执行顺序就是先执行第一个，然后有返回结果之后。把返回结果再生成一个 Future，继续执行。然后返回最后的结果。
- Delay ：就是延迟执行的 Future，可以为 Future 设置程序到了之后再过一段时间执行

当然， Future 不止针对两个的组合。也有针对 3、4 甚至多个组合。Tokio 就有一个 try_join3的组合。组合方式也不止这几种，[http://crate.io](https://link.zhihu.com/?target=http%3A//crate.io) 里面有一些 crates ，针对 Flattern ，还有 FlatternSink 和 FlatternStream，以及其他组合方式

看到这里，可能会有些迷惑。还记得之前提到的 ，Future 是未来要执行的动作。这些组合以及执行顺序，就有点类似编织未来程序执行的路径和方式。但是 Future 如果不被 Poll 或者 .await，Future 是静态的。这个和我们平时写同步执行代码有相似，也有不同。不同的地方在于，如果要获得一个中间判断的状态，就需要 Poll。如果要根据输入做动作，也需要在 Poll 的下面写对应的 match。感觉上会比较复杂。好在 Rust 提供 async / .await 的方式，能够让我们按照同步顺序操作的思路来写，就像上面描述的 read_file 函数。

刚才提到，Future 是未来要执行的动作。那怎么执行 Future，就需要在代码里调用 Poll，或者在函数后面增加 .await 。Future 的设计是为了提供异步执行的模式，为什么会有异步执行的模式，主要是为了性能。能够按照计算机的运行方式，采用事件触发，把任务切成小的操作步骤，更高效的调度，来达到高性能。

如果要设计一个高效完整的异步框架。在 Future 和 Waker 的基础上，还需要提供几个部分：

- executor ： 执行任务的基础。线程、协程、进程或者其他计算运行时
- scheduler：调度方式，针对不同的计算调度不同的 executor 来运行
- park : 对线程进行管理，满足 scheduler 的调度要求
- task ：Future 是小片的执行操作，要完整一个完整的任务，需要更强大的方式，就是task

下面就以 Tokio 为例来说明以上几个模块

## executor : 为什么用 Native Thread

作为异步并发模式的基础，采用线程、Green Thread 还是 Coroutine，从可行性上都可以。Rust 采用的是 Native Thread。

为什么会采用 Native Thread ，什么是 Native Thread ? Rust Team 核心成员 Steve Klabnik ( @withoutboats ) 有[一个演讲](https://link.zhihu.com/?target=https%3A//www.infoq.com/presentations/rust-2019/)详细说明了。

核心的原因是，Rust 是 "system programming language" ，和 C 之间不能有 overhead 。也就是说，Rust 必须使用系统 Native 的 Thread，才能和 C 的转换没有额外的 IO 损耗。

Rust 的 Async 采用了一种 "Synchronous non-blocking network I/O" （同步非阻塞IO）。这个看上去有些矛盾，但是仔细了解一下，感觉挺有道理的。同步阻塞的问题，就是效率较低。异步非阻塞的问题，对于长耗时的操作效率较低。异步阻塞，能够让长耗时的任务安排到独立线程运行，达到更好的性能。同步非阻塞IO，就是用同步的方法来写代码，但是内部其实是异步调用。

async-std 在[这篇博客](https://link.zhihu.com/?target=https%3A//async.rs/blog/stop-worrying-about-blocking-the-new-async-std-runtime/) 这样说："The new runtime **detects blocking** automatically. We don’t need [spawn_blocking](https://link.zhihu.com/?target=https%3A//docs.rs/async-std/1.2.0/async_std/task/fn.spawn_blocking.html) 。anymore and can simply deprecate it " 。系统 runtime 竟然能够自动检测是不是阻塞操作，不需要显式调用 spawn_blocking 来针对阻塞操作。

但是 Native Thread 在应对 IO 请求的时候，存在问题。它会针对每个请求，准备一个线程。这样会极大消耗系统资源，并且这些线程在等待的时候什么都不做。这样的机制面对大量请求的异步操作时会非常低效。

Go 和 Erlang 都是采用 Green Thread 来解决这个问题。但是 Rust 因为不想和 C 之间有更多的隔阂，不想采用 Green Thread 模式。

Rust 参考了 Nginx 的 Event Poll 模型，还有 Node.js 的 "Evented non-blocking IO" 模型。withoutboats 非常推崇 Node.js 模型，但是 Node.js 带来了回调地狱 (callback hell) 。Javascript 又创造了 Promise 来避免回调的一些问题。Promise 就是 Future 的思路来源。

Twitter 的工程师在处理这个问题的时候，放弃啦 JVM 转而用 Scala ，获得了非常大的性能提升 。然后他们写了一个 Paper 叫做 "[Your Server as a Function](https://link.zhihu.com/?target=https%3A//monkey.org/~marius/funsrv.pdf)" 。介绍了一个概念，叫做 Future 。这样描述：

> A future is a container used to hold the result of an asynchronous operation such as a network RPC, a timeout, or a disk I/O opera- tion. A future is either *empty*—the result is not yet available; *suc- ceeded*—the producer has completed and has populated the future with the result of the operation; or *failed*—the producer failed, and the future contains the resulting exception

Rust 在这个基础上，完善并推出了 zero cost future 。就是上面一篇讲述的内容。

"Synchronous non-blocking network I/O " 是怎么实现的呢？这里面的核心是调度 (scheduler)，就是让你用同步的方式来写代码，但是内部却是用异步调用的方式在运行。

下一篇就说一下调度 ( scheduler )

## scheduler ：让异步运作更有效率

Scheduler 的核心是调度，让 Future 和 Task 在 Executor 里面运行的更有效率。Tokio 和 Mio 的核心工程师 Carl Lerche 在 Tokio Blog 写了一篇文章 ： [Making the Tokio scheduler 10x faster](https://link.zhihu.com/?target=https%3A//tokio.rs/blog/2019-10-scheduler/)

文章描述了怎么优化 work-stealing 的任务调度机制，达到10倍的加速。优化的核心是针对任务调度的消息队列。最开始的 tokio 采用 [crossbeam](https://link.zhihu.com/?target=https%3A//github.com/crossbeam-rs/crossbeam) 的消息队列，是一种“single producer , multi-consumer”的模式。Tokio 参考 Go 优化成 "multi-producer , single-consumer" 模型，并且增加了一个 Global Queue，提升了调度效率

我们从 runtime 入手来看 scheduler 是怎么实现的。

![img](https://pic4.zhimg.com/v2-eeb3c8d1319541a03751f07e40c756e7_r.jpg)scheduler 结构图



builder 有两组 threads，一组是 core_threads，默认是和 CPU 的核数一样。一组是 max_threads ， 默认是512 。Core_threads 是作为 tokio runtime 的主要 executor。max_thread 是作为 blocking_pool 的 executor。在 runtime 启动的时候，core_threads 和 blocking_thread 都启动。在运行 Future 的时候，tokio::spawn ，在 core_threads 运行； tokio::block_on ，在 blocking_thread 启动。当然两种情况线程调度的机制都不一样

这里面有三种调度方式： shell、basic_scheduler 和 threaded_scheduler

Shell 没有实际使用，主要是 basic 和 threaded 两种。我们先来看一下 basice_scheduler。 BasicScheduler 的结构如下：

```rust
pub(crate) struct BasicScheduler<P>
where   P: Park,
{
    /// Scheduler component
    scheduler: Arc<SchedulerPriv>,
    /// Local state
    local: LocalState<P>,
}
struct LocalState<P> {
    /// Current tick
    tick: u8,
    /// Thread park handle
    park: P,
}
use tokio::net::TcpListener;
use tokio::prelude::*;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut listener = TcpListener::bind("127.0.0.1:8080").await?;

    loop {
        let (mut socket, _) = listener.accept().await?;

        tokio::spawn(async move {
            let mut buf = [0; 1024];

            // In a loop, read data from the socket and write the data back.
            loop {
                let n = match socket.read(&mut buf).await {
                    // socket closed
                    Ok(n) if n == 0 => return,
                    Ok(n) => n,
                    Err(e) => {
                        eprintln!("failed to read from socket; err = {:?}", e);
                        return;
                    }
                };

                // Write the data back
                if let Err(e) = socket.write_all(&buf[0..n]).await {
                    eprintln!("failed to write to socket; err = {:?}", e);
                    return;
                }
            }
        });
    }
}
```

LocalState 包含两个比较重要的点， tick 和 park。Park 是针对 Waker 的再次封装，为了能够更好控制线程，这个我们后面单独解释。 tick 是针对 task state 的增强，一次 tick 可能包含很多次 task 。例如在读取 socket 的时候，可以执行很多 task ，每个 task 读取一小段数据，执行多次，读完整个数据。在 basic_scheduler 里面，MAX_TASKS_PER_TICK 默认设置为 61

我们在使用 Tokio 的时候，往往是这样用 ：

```rust
use tokio::net::TcpListener;
use tokio::prelude::*;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut listener = TcpListener::bind("127.0.0.1:8080").await?;

    loop {
        let (mut socket, _) = listener.accept().await?;

        tokio::spawn(async move {
            let mut buf = [0; 1024];

            // In a loop, read data from the socket and write the data back.
            loop {
                let n = match socket.read(&mut buf).await {
                    // socket closed
                    Ok(n) if n == 0 => return,
                    Ok(n) => n,
                    Err(e) => {
                        eprintln!("failed to read from socket; err = {:?}", e);
                        return;
                    }
                };

                // Write the data back
                if let Err(e) = socket.write_all(&buf[0..n]).await {
                    eprintln!("failed to write to socket; err = {:?}", e);
                    return;
                }
            }
        });
    }
}
```

最开始的 #[tokio::main] 是一个宏，宏下面的 async 函数，是作为宏的 Future 输入。tokio::main 做的主要工作是 builder 一个 runtime ， 然后启动 block_on 函数，把 Future 包装进入 Block_on 。具体流程如下：

![img](https://pic4.zhimg.com/80/v2-fec23a086516fb22453c2a9be85a50e7_1440w.jpg)

block_on 主要做的工作就是获得 Future 然后执行。在 Tick Local State 这一步，实际是循环执行了多次 Task，如下：

![img](https://pic3.zhimg.com/v2-24d368482dde9d8d68c47fb434b08156_r.jpg)

就像上面说的，持续的执行 Task ，到没有 Task。或者执行 MAX_TASK_PER_TICK 次

在刚才的 TcpListener 里面，有一个 Tokio::spawn(future)，spawn 的流程如下：

![img](https://pic4.zhimg.com/v2-3a6b1d02965dc3ee56384b224c0b7ccb_r.jpg)

启动一个任务，然后判断是不是有 Scheduler 存在。有的话，就 push future 到 Local 的任务队列，没有就 push 到 remote 的队列。

还有一个关键的地方，就是 Scheduler 有一个 Mpsc (multi-producer, single consumer) 的队列。还记得这结刚开始的时候那篇文章: [Making the Tokio scheduler 10x faster](https://link.zhihu.com/?target=https%3A//tokio.rs/blog/2019-10-scheduler/)。为了提升性能，tokio 优化了 crossbeam 的 queue。这个 Mpsc 有两个 equeue ：Local 和 Remote，Local 是自己线程的任务， Remote 是其他线程推送过来的任务。 在 tick 里面 获取 next_task 的时候，有一个逻辑，每一段次数(CHECK_REMOTE_INTERVAL，默认是13) 之后，就去获取一次 Remote queue 里面的任务。另外，在 spawn future 的时候，如果 scheduler 不存在，就推送 task 到 remote queue

Thread_Scheduler 和 Basic_Scheduler 的区别在于， Thread 是多线程多任务模式，Basic 是单线程多任务模式。Threaded 用和 CPU 核数一样的线程数，针对每个任务用一个线程来执行。block_on 和 spawn 都是这样。所以 Thread 的流程较为简单，没有 Basic 复杂。

除了这个之外，还有一个针对长耗时的 blocking 操作的 spawn_blocking 。就是用在 builder 里面 create_blocking_pool (512个) 来执行。

以上内容基本描述了 Scheduler 的流程。但是还有很多地方不清晰，因为我们还有两个重要的内容没描述：Park 和 Task 。Park 是在 Waker 基础上的增强，针对线程状态和 Future 做更细节的管理和控制； Task 是连接 Future 和 线程模型的重要控制模块。也可以说是 Rust 异步模式之后开发时接触最多的概念。按照 Aaron Turon 的定义， Task 是正在执行的 Future ( a task is a future thas is being executed)

我们先说 Park/Unpark，之后详细描述 Task

## Park/Unpark : 管理线程

在上面的 Basic 和 Threaded 里面，都设计到线程的管理。在 tokio::main 开始的时候，启动了 cpu_cores 的 core Threads 和 512 个 Thread Pool。这些线程怎么管理，达到 Scheduler 的要求。Park 起到了关键作用。

先描述一下 Park 的内部关系：

![img](https://pic4.zhimg.com/v2-66c0e23a94cd8b101342e1f9e3cb4543_r.jpg)Park/Unpark 结构图



Park Trait 定义了一个关于状态切换的关系，如下：

![img](https://pic1.zhimg.com/v2-31001e83b169fafef73b63e3d4333f9c_r.jpg)Park 状态切换



在 Thread 里面，有三个状态，Empty、PARKED 和 NOTIFIED。通过 park() 和 unpark() 转换状态。在 Inner 这个 struct 和它的 impl 里面。有一个 Condvar 是 std::sys::condvar，是一个条件变量。条件变量的官方描述是这样：

> Condition variables represent the ability to block a thread such that it consumes no CPU time while waiting for an event to occur. Condition variables are typically associated with a boolean predicate (a condition) and a mutex. The predicate is always verified inside of the mutex before determining that a thread must block.

条件变量能够阻塞一个线程，让它不消耗 CPU。直到一个事件触发，线程再继续执行。条件变量往往和一个 bool类型及一个 mutex 关联，这个 bool 类型包装在 mutex 里面，用来确认这个线程是不是要阻塞。condvar 的内部实现利用了 sys::condvar ，这是一个操作系统层的条件变量。

在 park() 里面，有一个 self.condvar.wait ，会让线程 block ，等待 condvar 调用 notify 。在 unpark() ， 除了设置 state 为 NOFITIED 外，还调用了了 self.condvar.notify_one() ，让线程重新激活。

Thread 除了 ParkThread 和 UnparkThread 之外，还有一个 CachedParkThread，用在多线程 blocking 操作里面，就是上面一篇说过的 spawn_blocking 。

## Task 把一切都连起来

在 Tokio 的文档里说，Task 是轻量级、非阻塞的执行单元。在 Tokio Task 的代码说，Task 是异步绿色线程(Asynchronnouse green-threads)。 Task 类似于线程，但是被 Tokio::Runtime 管理

Task 的结构如下：

![img](https://pic3.zhimg.com/v2-05757fdbfac7c8a68d59e15c40c866ca_r.jpg)

![img](https://pic1.zhimg.com/v2-61d83076df6305ec0df788f5aa947d4c_r.jpg)

这两页结构图还不是 Task 的全部，Task 还包含了 join、list、local、harness、queue、stack 等10多个 Struct 和 Trait 。我们尝试换一个方式，通过 Task 的实际使用来理解 Task 的机制。

按照 Tokio 文档的案例：

```rust
use tokio::net::TcpListener;
use tokio::prelude::*;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut listener = TcpListener::bind("127.0.0.1:8080").await?;
    loop {
        let (mut socket, _) = listener.accept().await?;
        tokio::spawn(async move {
            loop {
                // ... do somethings ...
            }
        });
    }
}
```

上面说过，tokio::main 是启动了 runtime::block_on ， 然后把 async 后面的内容作为一个 Future 传给 block_on Block_on ， 如果是 Basic Schedule ，则按照 Basic Schedule 开始循环执行 Future.poll 。 流程如下：

![img](https://pic1.zhimg.com/v2-852389317a50b8b26a6fea525e62b1f8_r.jpg)block_on

在 aync 后面，又调用了 tokio::spawn 一个新的 task ，代码如下：

```rust
pub(crate) fn spawn<F>(&self, future: F) -> JoinHandle<F::Output>
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        let (task, handle) = task::joinable(future);
        self.scheduler.schedule(task);
        handle
    }
```

spawn 一个新的 Future ，包装成 joinable task。JoinHandle 包装了 Task，让 task 能够在 future lifetime 结束还能保留。JoinHandle 在 Drop Trait 里增了逻辑

```rust
impl<T> Drop for JoinHandle<T> {
    fn drop(&mut self) {
        if let Some(raw) = self.raw.take() {
            if raw.header().state.drop_join_handle_fast() {
                return;
            }

            raw.drop_join_handle_slow();
        }
    }
}
```

如果 Task 可以快速释放，就调用 drop_join_handle_fast，否则就是 drop_join_handle_slow。这个在 task::state 里面实现

上面的 Schedule.tick， 在 前面 Scheduler 的时候描述过，多次调动 Task ，可以包装成一个 tick 来返回。

我们简单描述了一下 Task 以及 Task 的使用。至此，我们可以说囫囵吞枣的把 Tokio 的内核和机制了解了一下。Tokio 还在发展 ，Rust 也还在进化。但是投入 Rust 及学习 Tokio ，还是非常有意义，希望我们都能坚持下去。

https://zhuanlan.zhihu.com/p/104098627?utm_source=wechat_timeline