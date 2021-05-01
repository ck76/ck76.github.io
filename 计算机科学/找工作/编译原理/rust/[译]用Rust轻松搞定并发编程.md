原文：[Fearless Concurrency with Rust](https://link.jianshu.com?t=http://blog.rust-lang.org/2015/04/10/Fearless-Concurrency.html) by [Aaron Turon](https://link.jianshu.com?t=http://www.mpi-sws.org/~turon/)  Apr 10, 2015

(译者注：这是一篇很好地讲解并发编程的文章，涉及几种编程模型，常见的错误，及Rust的解决方法，不管是否学习Rust，其思路和做法对并发编程而言，都值得思考和借鉴，希望我的翻译清楚传递了原作者的想法，若发现不当之处，请直接留言，我会第一时间改正并更新。就原文是否允许被翻译成中文的版权问题，我已给原作者发了邮件，等待回复中，如涉及侵权，麻烦告知，我会立即撤下来。)

Rust建立的目的是为了解决两个棘手的问题：

- 怎样才能安全地进行系统编程？
- 怎样才能容易地使用并发？

一开始这两个问题看起来并不相关，但令人惊讶地是，他们的解决方案居然是相同的：**让Rust安全的方法同样可以直接解决并发问题**。

因为内存安全bug和并发bug经常都是因为代码对数据不正确地访问造成的。为了解决上面的两个问题，Rust使用一个叫做ownership的秘密武器，它是一条系统程序员尽量遵守的访问控制准则，但是Rust编译器会帮你做静态检查。

对于内存安全而言，这意味着没有垃圾收集器，你编程也不用担心段错误，因为Rust会发现你的错误。

对于并发而言，这意味着你可以选择各种不同的并发模型（消息传递(message passing)，共享状态(shared state)，无锁(lock-free)，纯函数式(purely functional)），Rust将帮助你避免常见错误。

下面是Rust的并发编程喜好：

- [通道(channel)](https://link.jianshu.com?t=http://static.rust-lang.org/doc/master/std/sync/mpsc/index.html)可以传递消息的ownership，这样你就可以从一个线程发送一个指针到另一个线程，而不用担心线程在后续通过指针访问数据而发生竞争。**Rust的通道(channel)能强制隔离线程。**
- [锁(lock)](https://link.jianshu.com?t=http://static.rust-lang.org/doc/master/std/sync/struct.Mutex.html)知道它所保护的数据，Rust会确保访问数据之前，必须先持有锁。状态永远不会被意外的共享。**"锁定数据，而非代码"在Rust中被强制执行。**
- 每一种数据类型都知道它是否可以安全地在多个线程间[发送(send)](https://link.jianshu.com?t=http://static.rust-lang.org/doc/master/std/marker/trait.Send.html)和[访问(access)](https://link.jianshu.com?t=http://static.rust-lang.org/doc/master/std/marker/trait.Sync.html)，Rust强制执行这种安全用法。这种方式不存在数据竞争，即使是无锁(lock-free)的数据结构。**线程安全并不仅仅是文档；它是法律。**
- 你甚至可以在线程间[共享栈(share stack frames)](https://link.jianshu.com?t=http://static.rust-lang.org/doc/master/std/thread/fn.scoped.html)，Rust会静态地确保其他线程使用它时，它还存活着。**在Rust中即使是最大胆的共享方式也被保障是安全的**

所有这些都得益于Rust的ownership模型，事实上，锁(lock)，通道(channel)和无锁(lock-free)数据结构等等都是定义在库中，而不是语言中。这就意味着Rust的并发处理方式是开放的：新库可以仅仅通过添加一些使用ownership特性的API，就能集成新的并发模型，捕捉新的并发bug。

这篇文章的目的就是让你大致了解一下这是如何做到的。

## 背景: ownership

> 下面我们将从Rust的ownership和borrowing系统概述开始。如果你已经对他们有所了解，可跳过接下来的两个“背景”段落，直接进入并发段落。如果你想要更深入地了解，我强烈推荐[Yehuda Katz的文章](https://link.jianshu.com?t=http://blog.skylight.io/rust-means-never-having-to-close-a-socket/)。同时[the Rust book](https://link.jianshu.com?t=http://doc.rust-lang.org/book/ownership.html)包含所有的细节。

在Rust中，每一个值都具有一个“拥有域(owning scope)”，传递或返回一个值会转移ownership(移动它)到新的域(scope)中。当一个域(scope)结束时，如果域所拥有的值还没销毁，此时将自动销毁。

下面让我们看一些比较简单的例子。假设我们需要创建一个vector，并添加一些元素进去：



```rust
fn make_vec() {
    let out vet = Vec::new(); // owned by make_vc’s scope
    vec.push(0);
    vec.push(1);
    // scope ends, `vet` is destroyed
}
```

一个值创建时所在的域(scope)，也是该值的拥有域(owning scope)，在创建时该域就拥有它。在上面这个例子中，`make_vec`函数体就是`vec`的拥有域(owning scope)。它可以随心所欲地使用`vec`，包括添加元素来改变它。在域(scope)结束时，`vec`仍然被域(scope)所拥有，因此它将自动销毁。

如果vector是由函数返回，或者把它作为参数传递，这会变得更加有趣：



```rust
fn make_vec() -> Vec<i32> {
    let mut vec = Vec::new();
    vec.push(0);
    vec.push(1);
    vec // transfer ownership to the caller
}

fn print_vec(vec: Vec<i32>) {
    // the `vec` parameter is part of this scope, so it's owned by `print_vec`

    for i in vec.iter() {
        println!("{}", i)
    }

    // now, `vec` is deallocated
}

fn use_vec() {
    let vec = make_vec(); // take ownership of the vector
    print_vec(vec);       // pass ownership to `print_vec`
}
```

现在，在`make_vec`的域(scope)结束之前，`vec`以函数返回值的方式被移了出来，它没有被销毁，它的ownership被调用者`use_vec`所接收。

另一方面，函数`print_vec`有一个`vec`参数，当该函数被调用时，参数的ownership将从调用者转移给它。由于`vec`的ownership在函数`print_vec`中没有被再次转移，为此在`print_vec`的域(scope)结束时，`vec`将自动销毁。

一旦ownership被转移了，值将不再可用。举个例子，考虑一下下面这个不太一样的`use_vec`：



```rust
fn use_vec() {
    let vec = make_vec();  // take ownership of the vector
    print_vec(vec);        // pass ownership to `print_vec`

    for i in vec.iter() {  // continue using `vec`
        println!("{}", i * 2)
    }
}
```

编译上面这段代码，会产生下面的编译错误：



```csharp
error: use of moved value: `vec`

for i in vec.iter() {
         ^~~
```

编译器会告诉你`vec`不可用；它的ownership已经被转移到其他地方去了。这非常牛逼，编译器发现问题了，因为在这个时候`vec`已经被销毁了。

**灾难得以避免！**

## 背景：borrowing

到目前为止，上面的结果并不让人完全满意，因为我们并不打算让`print_vec`销毁传递给它的vector。我们真正希望地是让`print_vec`只是临时访问一下vector，而不是销毁它，之后我们还可以继续使用它。

为了解决上面这个问题，Rust提供了borrowing特性。在Rust中，如果你能访问(access)一个值，你可以把它借给你所调用的函数，供它们访问。**Rust会检查所有借出的值，确保它们的寿命不会超过值本身的寿命**

为了借出(borrow)一个值，你可以使用引用(reference)（一种指针），对应的操作符是 `&`：



```rust
fn print_vec(vec: &Vec<i32>) {
    // the `vec` parameter is borrowed for this scope

    for i in vec.iter() {
        println!("{}", i)
    }

    // now, the borrow ends
}

fn use_vec() {
    let vec = make_vec();  // take ownership of the vector
    print_vec(&vec);       // lend access to `print_vec`
    for i in vec.iter() {  // continue using `vec`
        println!("{}", i * 2)
    }
    // vec is destroyed here
}
```

现在 `print_vec`拥有一个vector的引用，并且`use_vec`使用`&vec`的方式把vector借给它。因为借出的值只是用于临时访问用的，`use_vec`仍然拥有vector的ownership，为此，在函数`print_vec`调用之后（`print_vec`对`vec`的借用将过期）还可以继续使用它。

每一个引用仅在一个有限的域(scope)中有效，编译器会自动判定。引用具有以下两种形式：

- 不可变引用`&T`，可以共享但不能被改变。一个值可以同时具有多个`&T`引用，但即使引用可用也不能改变它。
- 可变引用`&mut T`，可以被改变但不能共享。如果一个值已经有了一个 `&mut T`引用，就不能同时具有其他可用的引用，但是它可以被改变。

Rust会在编译时检查上面的规则，因此并不会产生运行时开销。

为什么会存在两种不同的引用？考虑下面这个函数：



```rust
fn push_all(from: &Vec<i32>, to: &mut Vec<i32>) {
    for i in from.iter() {
        to.push(*i);
    }
}
```

该函数会遍历一个vector中的每一个元素，并把它们添加到另一个vector中。迭代器持有一个vector当前位置和结束位置的指针，一次前进一个元素。

假设我们在调用这个函数时，把同一个vector做为该函数的两个参数传入，将会发生什么？



```rust
push_all(&vec, &mut vec)
```

这将是个灾难！因为我们把元素放入vector时，它将会改变大小，分配新的内存，并拷贝元素到新内存。迭代器将会持有一个指向旧内存的无效指针，从而导致内存不安全（随之出现段错误或更糟的情况）。

幸运地是，Rust会保证**无论何时，只要存在一个有效的可变引用时，就不能同时具有其他的引用**，并且产生如下错误信息：



```csharp
error: cannot borrow `vec` as mutable because it is also borrowed as immutable
push_all(&vec, &mut vec);
                    ^~~
```

**灾难得以避免！**

## 消息传递(Message passing)

到目前为止，我们已经介绍了Rust里关于ownership的基础知识。下面让我们看一下它对于并发而言，意味着什么。

并发编程具有多种模型，但是尤其简单的就是消息传递，线程和actors之间通过互相发送消息进行通信。该模型的支持者最看重的一点是，它把共享和通信紧密地结合在一起：

> Do not communicate by sharing memory; instead, share memory by communicating.
>
> –[Effective Go](https://link.jianshu.com?t=http://golang.org/doc/effective_go.html)

由于Rust具有ownership模型，可以把上面这条建议转化到编译器检查规则中，从而使消息传递并发模型编程变得更加简单。为什么？
 下面让我们先看一下Rust的通道(channel)API（[channels in Rust's standard library](https://link.jianshu.com?t=http://static.rust-lang.org/doc/master/std/sync/mpsc/index.html)有一些不同）:



```rust
fn send<T: Send>(chan: &Channel<T>, t: T);
fn recv<T: Send>(chan: &Channel<T>) -> T;
```

通道中传输的数据类型是泛型的(`<T:Send>`是API的一部分)。`Send`代表`T`可以安全地在线程中传输，本文后面一点我们将回过头来详细讲解它，此处先不细说，现在我们只要知道`Vec<i32>`是一个`Send`就足够了。

在Rust中，一如既往地，只要传递一个`T`给函数`send`就意味着会转移它的ownership。这一原则具有重大影响: 它意味着像下面这样的代码将产生一个编译器错误。



```rust
// Suppose chan: Channel<Vec<i32>>

let mut vec = Vec::new();
// do some computation
send(&chan, vec);
print_vec(&vec);
```

在这里， 线程先创建一个vector，并把它发给了其他的线程，然后继续使用它。当线程继续运行时，接收这个vector的线程可能会修改它，所以`print_vec`将有可能引起竞争(race condition)，或为此出现一个释放后使用(use-after-free)的bug。（译者注：这是一种使用消息传递并发模型编程时常遇见的错误，需要程序员自己时刻注意。）

然而，在Rust中，由于ownership转移了，对于上面这种情况, Rust编译器会直接在函数`print_vec`调用处产生一个错误：



```jsx
Error: use of moved value `vec`
```

**灾难得以避免！**

（译者注：从这点来看，在Rust中使用消息传递并发模型，确实更简单，更有信心。）

## 锁(Locks)

另一种并发编程模型是让线程之间通过被动地共享状态来实现通信。

共享状态式并发编程模型(shared-state concurrency)名声不怎么好。因为容易忘记加锁，或者在不正确的时间改变不正确的数据，从而导致灾难性后果。由于太容易犯这些错误，从而导致很多人都避免使用这种模型。

Rust对于该模型的态度是：

1. 共享状态式并发编程模型仍然是一项基本的模型，被系统编程，性能优化及实现其他并发编程模型所需要。
2. 问题的根源在于意外地共享状态。

不管你使用加锁(locking)又或者无锁(lock-free)技术，Rust的目标是给你直接征服共享状态式并发编程的工具。

在Rust中，因为ownership的关系，线程之间都是相互隔离的。写操作只会发生在线程具有数据的可变访问权限时，拥有该数据，或者拥有该数据的可变引用。 换句话说，**在同一时间，只有一个线程能访问数据**。为了弄清楚这是怎么做到的，让我们先看一下Rust中的锁。

记住可变引用和其他引用不能同时存在。通过在运行时提供同步，锁能做到同样的保障（“可变排他性”）。这导致锁的API直接牵扯到Rust的ownership系统。

下面是一个简单的版本([标准库](https://link.jianshu.com?t=http://static.rust-lang.org/doc/master/std/sync/struct.Mutex.html)更为复杂)：



```rust
// create a new mutex
fn mutex<T: Send>(t: T) -> Mutex<T>;

// acquire the lock
fn lock<T: Send>(mutex: &Mutex<T>) -> MutexGuard<T>;

// access the data protected by the lock
fn access<T: Send>(guard: &mut MutexGuard<T>) -> &mut T;
```

这个锁的API在很多方面都和普通的API不一样。

首先，`Mutex`是一个类型`T`的泛型类型，`T`是锁要保护的数据。当你在创建一个`Mutex`时，会将数据的ownership转移到mutex中，并立即放弃对它的访问。（锁在创建时，默认是没有锁定数据的）

然后，你可以调用`lock`函数来阻塞线程直到获取到锁。这个函数同普通的函数也不太一样，它会返回一个值，`MutexGuard<T>`。 当`MutexGuard<T>`销毁时，它会自动释放锁，这里不存在单独的`unlock`函数。

访问数据的唯一方式是通过函数`access`，它将可变引用`MutexGuard<T>`转换为一个可变引用`T`（临时借用）：



```rust
fn use_lock(mutex: &Mutex<Vec<i32>>) {
    // acquire the lock, taking ownership of a guard;
    // the lock is held for the rest of the scope
    let mut guard = lock(mutex);

    // access the data by mutably borrowing the guard
    let vec = access(&mut guard);

    // vec has type `&mut Vec<i32>`
    vec.push(3);

    // lock automatically released here, when `guard` is destroyed
}
```

这里有两个关键点：

- `access`函数返回的可变引用的寿命不能超过`MutexGuard`的寿命。
- 只有当`MutexGuard`销毁时，锁才会被释放。

这样做的结果就是**Rust强制执行加锁的准则：访问被保护的数据前必须先持有锁**。任何除此之外的访问都将产生一个编译错误。举个例子，思考一下下面这个具有bug的重构代码：



```rust
fn use_lock(mutex: &Mutex<Vec<i32>>) {
    let vec = {
        // acquire the lock
        let mut guard = lock(mutex);

        // attempt to return a borrow of the data
        access(&mut guard)

        // guard is destroyed here, releasing the lock
    };

    // attempt to access the data outside of the lock.
    vec.push(3);
}
```

Rust将产生一个错误用于指出问题所在：



```swift
error: `guard` does not live long enough
access(&mut guard)
            ^~~~~
```

**灾难得以避免！**

## 线程安全和“Send”(Thread safety and "Send")

存在一些典型的方法用于判定数据类型是否线程安全。线程安全的数据结构在内部会使用足够多的同步以确保被多个线程并发使用时是安全的。

举例说明，Rust有两种用于引用计数的"智能指针(smart pointers)"：

- `Rc<T>` 通过常用的读写方式进行引用计数，它不是线程安全的。
- `Arc<T>` 通过原子操作进行引用计数，它是线程安全的。

因为`Arc`使用的硬件原子操作比`Rc`使用的普通操作更为昂贵，因此使用`Rc`更具优势。但另一方面，非常关键的是永远不能将`Rc<T>`从一个线程转移到另一个线程。因为那样可能导致竞争，从而扰乱计数。

(到底该使用`Arc`还是`Rc`？)对于这种情况而言，通常来说，唯一能够指望的就是文档了，因为在大多数语言中，线程安全和线程不安全的类型在语义上没有任何差异。

然而在Rust中，类型会被分为两种：一种是`Send`，表示把他们从一个线程移到另一个线程是安全的；另一种是`!Send`，表示把他们从一个线程移到另一个线程可能不安全。是不是一个类型的所有组成部分都是`Send`，那么这个类型就是`Send`？只能说大多数时候是。尽管某些基本类型也并不是线程安全的，但他们可以显式地使用像`Arc`一类的类来转变为`Send`，从而告诉编译器：“相信我，我已经通过了必要的同步检验”。

自然而然，`Arc`是`Send`，`Rc`不是。

我们在前面已经看到`Channel`和`Mutex`APIs只作用于`Send`数据。因为`Send`数据是穿越线程边界的关键点，同时穿越线程边界也是`Send`数据的关键点。

结合所有这些在一起，Rust程序员可以放心大胆地在多线程环境中使用`Rc`和其他线程不安全的类型，从而充分利用他们的优点，而不用担心意外地把他们从当前线程发送到其他线程去了，因为Rust编译器将提示：



```rust
`Rc<Vec<i32>>` cannot be sent between threads safely
```

**灾难得以避免！**

## 共享栈：“域”(Sharing the stack: "scoped")

到目前为止，所有我们看到的在线程间分享的数据，都是在堆上创建的。要是我们想启动一些线程，并共享一些在当前栈上的数据会怎样呢？那将非常危险：



```rust
fn parent() {
    let mut vec = Vec::new();
    // fill the vector
    thread::spawn(|| {
        print_vec(&vec)
    })
}
```

子线程拥有一个`vec`的引用，`vec`是驻留在`parent`的栈上的。当`parent`退出时，栈会被弹出，但是子线程并没有这么聪明，它并不知道栈被弹出了啊。Oops!

为了避免出现这样的内存不安全问题，Rust创建线程的基本API看起来像下面这样：



```rust
fn spawn<F>(f: F) where F: 'static, ...
```

`'static` 简单粗暴地要求这个闭包(closure)不允许有borrow的数据。这意味着像上面`parent`这样的函数，将产生一个编译错误：



```cpp
error: `vec` does not live long enough
```

看来Rust基本具备了知道`parent`的栈会被弹出的能力。灾难得以避免。

除了上面这种方式之外，还有另一种可以保障安全的方式：确保在子线程完成之前，父线程的栈没有被弹出。这称之为分解/合并(fork-join)编程模型，经常应用于分治并行算法。Rust提供了一个叫"scoped"的创建线程的API来支持它：



```rust
fn scoped<'a, F>(f: F) -> JoinGuard<'a> where F: 'a, ...
```

同上面的`spawn`API比较，存在两个关键差异：

- 使用了`'a`而不是`'static`。它表示一个作用域(scope)，用做所有包含在闭包（`f`）中的引用的作用域。
- 返回值是一个`JoinGuard`。同它名字所表示的意思一样，`JoinGuard`在析构时会执行一种隐形合并(join)，以此来确保父线程合并(join)(等待)它的子线程。

把`JoinGuard`的域(scope)设置为`'a`，是为了确保它**不会逃离在闭包中被借用的数据的域(scope)**。换句话说，Rust保证父线程在弹出任何子线程可能访问的栈之前，会一直等待子线程结束。

因此，稍微调整一下我们前面的例子，就可以修复bug，并且通过编译器检查：



```rust
fn parent() {
    let mut vec = Vec::new();
    // fill the vector
    let guard = thread::scoped(|| {
        print_vec(&vec)
    });
    // guard destroyed here, implicitly joining
}
```

因此在Rust中，你可以随意借用(borrow)栈上的数据到子线程中，相信编译器会做充分的同步检查。

## 数据竞争(Data races)

到此，我们已经见识了很多东西，从而可以冒险地对Rust的并发编程方式做出强有力的声明：**编译器阻止了所有的数据竞争**

> A data race is any unsynchronized, concurrent access to data involving a write.

上面这句话中的同步包括底层的原子指令。它本质上说明了你不能在线程间意外地共享状态，状态的所有(可变)访问都需要以某种同步方式进行。

数据竞争只是一种(非常重要)竞争(race condition)，但是通过阻止它，Rust经常帮助你有效地阻止其他的，更加微妙的竞争。举个例子，把不同位置的数据更新弄成原子操作是很重要的：其他线程要么能看到所有更新，要么一个更新也看不到。在Rust中，同时拥有相关位置的`&mut`引用，将保证对他们的更新是原子的。因为不可能会有其他的线程能同时访问。

它值得你停下来想一想这种在语言层面更具广大前景的保障。许多语言通过垃圾回收来保障内存安全。但是垃圾回收并没有在阻止数据竞争方面给你提供任何帮助。

Rust则用ownership和borrowing来实现它的两个关键的价值观：

- 没有垃圾回收的内存安全
- 没有数据竞争的并发

## 未来(The future)

在Rust开始之初，它直接将通道添加到了语言中，在并发编程方面采取了一种自以为是的方式。

但如今，并发已经完全由库来处理，在这篇文章中所描述的所有东西，包括`Send`，都是定义在标准库中的，同样也可以定义在一个外部库中。

这非常地令人兴奋，因为这就意味着Rust的并发可以无止境的进化，可以包含新的并发模式，捕捉新的并发bug。像[syncbox](https://link.jianshu.com?t=https://github.com/carllerche/syncbox)和[simple_parallel](https://link.jianshu.com?t=https://github.com/huonw/simple_parallel)这样的库走出了第一步，我们期望在接下来的几个月中，在这个领域进行大量投入。敬请期待!



作者：猿基地
链接：https://www.jianshu.com/p/a4bc33022aa3
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。