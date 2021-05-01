https://coolshell.cn/articles/20845.html

[TOC]

总是有很多很多人来问我对Rust语言怎么看的问题，在各种地方被at，其实，我不是很想表达我的想法。因为在不同的角度，你会看到不同的东西。编程语言这个东西，老实说很难评价，在学术上来说，Lisp就是很好的语言，然而在工程使用的时候，你会发现Lisp没什么人用，而Javascript或是PHP这样在学术很糟糕设计的语言反而成了主流，你觉得C++很反人类，在我看来，C++有很多不错的设计，而且对于了解编程语言和编译器的和原理非常有帮助。**但是C++也很危险，所以，出现在像Java或Go 语言来改善它，Rust本质上也是在改善C++的。他们各自都有各自的长处和优势**。

因为各个语言都有好有不好，因此，我不想用别的语言来说Rust的问题，或是把Rust吹成朵花以打压别的语言，写成这样的文章，是很没有营养的事。**本文主要想通过Rust的语言设计来看看编程中的一些挑战，尤其是Rust重要的一些编程范式，这样反而更有意义一些，因为这样你才可能一通百通**。

这篇文章的篇幅比较长，而且有很多代码，信息量可能会非常大，所以，**在读本文前，你需要有如下的知识准备**：

- 你对C++语言的一些特性和问题比较熟悉。尤其是：指针、引用、右值move、内存对象管理、泛型编程、智能指针……
- 当然，你还要略懂Rust，不懂也没太大关系，但本文不会是Rust的教程文章，可以参看“[Rust的官方教程](https://doc.rust-lang.org/book/title-page.html)”（[中文版](https://kaisery.github.io/trpl-zh-cn/)）

**因为本文太长，所以，我有必要写上 TL;DR ——**

 

Java 与 Rust 在改善C/C++上走了完全不同的两条路，他们主要改善的问题就是C/C++ Safety的问题。所谓C/C++编程安全上的问题，主要是：内存的管理、数据在共享中出现的“野指针”、“野引用”的问题。

- 对于这些问题，Java用引用垃圾回收再加上强大的VM字节码技术可以进行各种像反射、字节码修改的黑魔法。
- 而Rust不玩垃圾回收，也不玩VM，所以，作为静态语言的它，只能在编译器上下工夫。如果要让编译器能够在编译时检查出一些安全问题，那么就需要程序员在编程上与Rust语言有一些约定了，其中最大的一个约定规则就是变量的所有权问题，并且还要在代码上“去糖”，比如让程序员说明一些共享引用的生命周期。
- Rust的这些所有权的约定造成了很大的编程上的麻烦，写Rust的程序时，基本上来说，你的程序再也不要想可能轻轻松松能编译通过了。而且，在面对一些场景的代码编写时，如：函数式的闭包，多线程的不变数据的共享，多态……开始变得有些复杂，并会让你有种找不到北的感觉。
- Rust的Trait很像Java的接口，通过Trait可以实现C++的拷贝构造、重载操作符、多态等操作……
- 学习Rust的学习曲线并不平，用Rust写程序，基本上来说，一旦编译通过，代码运行起来是安全的，bug也是很少的。

**如果你对Rust的概念认识的不完整，你完全写不出程序，那怕就是很简单的一段代码**。**这逼着程序员必需了解所有的概念才能编码。但是，另一方面也表明了这门语言并不适合初学者……**

目录

- [变量的可变性](https://coolshell.cn/articles/20845.html?from=timeline#变量的可变性)
- [变量的所有权](https://coolshell.cn/articles/20845.html?from=timeline#变量的所有权)
- [Owner语义带来的复杂度](https://coolshell.cn/articles/20845.html?from=timeline#Owner语义带来的复杂度)
- 引用（借用）和生命周期
  - [引用（借用）](https://coolshell.cn/articles/20845.html?from=timeline#引用（借用）)
  - [生命周期](https://coolshell.cn/articles/20845.html?from=timeline#生命周期)
- 闭包与所有权
  - [函数闭包](https://coolshell.cn/articles/20845.html?from=timeline#函数闭包)
  - [线程闭包](https://coolshell.cn/articles/20845.html?from=timeline#线程闭包)
- [Rust的智能指针](https://coolshell.cn/articles/20845.html?from=timeline#Rust的智能指针)
- [线程与智能指针](https://coolshell.cn/articles/20845.html?from=timeline#线程与智能指针)
- 多态和运行时识别
  - [通过Trait多态](https://coolshell.cn/articles/20845.html?from=timeline#通过Trait多态)
  - [向下转型](https://coolshell.cn/articles/20845.html?from=timeline#向下转型)
- [Trait 重载操作符](https://coolshell.cn/articles/20845.html?from=timeline#Trait_重载操作符)
- [小结](https://coolshell.cn/articles/20845.html?from=timeline#小结)

### 变量的可变性

首先，Rust里的变量声明默认是“不可变的”，如果你声明一个变量 `let x = 5;` 变量 `x` 是不可变的，也就是说，`x = y + 10;` 编译器会报错的。如果你要变量的话，你需要使用 `mut` 关键词，也就是要声明成 `let mut x = 5;` 表示这是一个可以改变的变量。这个是比较有趣的，因为其它主流语言在声明变量时默认是可变的，而Rust则是要反过来。这可以理解，不可变的通常来说会有更好的稳定性，而可变的会代来不稳定性。所以，Rust应该是想成为更为安全的语言，所以，默认是 immutable 的变量。当然，Rust同样有 `const` 修饰的常量。于是，Rust可以玩出这么些东西来：

- 常量：`const LEN:u32 = 1024;` 其中的 `LEN` 就是一个`u32` 的整型常量（无符号32位整型），是编译时用到的。
- 可变的变量： `let mut x = 5;` 这个就跟其它语言的类似， 在运行时用到。
- 不可变的变量：`let x= 5;` 对这种变量，你无论修改它，但是，你可以使用 `let x = x + 10;` 这样的方式来重新定义一个新的 `x`。这个在Rust里叫 Shadowing ，第二个 `x` 把第一个`x` 给遮蔽了。

不可变的变量对于程序的稳定运行是有帮助的，这是一种编程“契约”，当处理契约为不可变的变量时，程序就可以稳定很多，尤其是多线程的环境下，因为不可变意味着只读不写，其他好处是，与易变对象相比，它们更易于理解和推理，并提供更高的安全性。有了这样的“契约”后，编译器也很容易在编译时查错了。这就是Rust语言的编译器的编译期可以帮你检查很多编程上的问题。

对于标识不可变的变量，在 C/C++中我们用`const` ，在Java中使用 `final` ，在 C#中使用 `readonly`，Scala用 `val` ……（在Javascript 和Python这样的动态语言中，原始类型基本都是不可变的，而自定义类型是可变的）。

对于Rust的Shadowing，我个人觉得是比较危险的，在我的职业生涯中，这种使用同名变量（在嵌套的scope环境下）带来的bug还是很不好找的。一般来说，每个变量都应该有他最合适的名字，最好不要重名。

### 变量的所有权

这个是Rust这个语言中比较强调的一个概念。其实，在我们的编程中，很多情况下，都是把一个对象（变量）传递过来传递过去，在传递的过程中，传的是一份复本，还是这个对象本身，也就是所谓的“传值还是传引用”的被程序员问得最多的问题。

- **传递副本（传值）**。把一个对象的复本传到一个函数中，或是放到一个数据结构容器中，可能需要出现复制的操作，这个复制对于一个对象来说，需要深度复制才安全，否则就会出现各种问题。而深度复制就会导致性能问题。
- **传递对象本身（传引用）**。传引用也就是不需要考虑对象的复制成本，但是需要考虑对象在传递后，会多个变量所引用的问题。比如：我们把一个对象的引用传给一个List或其它的一个函数，这意味着，大家对同一个对象都有控制权，如果有一个人释放了这个对象，那边其它人就遭殃了，所以，一般会采用引用计数的方式来共享一个对象。引用除了共享的问题外，还有作用域的问题，比如：你从一个函数的栈内存中返回一个对象的引用给调用者，调用者就会收到一个被释放了个引用对象（因为函数结束后栈被清了）。

这些东西在任何一个编程语言中都是必需要解决的问题，要足够灵活到让程序员可以根据自己的需要来写程序。

在C++中，如果你要传递一个对象，有这么几种方式：

- **引用或指针。**也就是不建复本，完全共享，于是，但是会出现悬挂指针（[Dangling Pointer](https://en.wikipedia.org/wiki/Dangling_pointer)）又叫野指针的问题，也就是一个指针或引用指向一块废弃的内存。为了解决这个问题，C++的解决方案是使用 `share_ptr` 这样的托管类来管理共享时的引用计数。
- **传递复本**，传递一个拷贝，需要重载对象的“拷贝构造函数”和“赋值构造函数”。
- **移动Move**。C++中，为了解决一些临时对象的构造的开销，可以使用Move操作，把一个对象的所有权移动到给另外一个对象，这个解决了C++中在传递对象时的会产生很多临时对象来影响性能的情况。

C++的这些个“神操作”，可以让你非常灵活地在各种情况下传递对象，但是也提升整体语言的复杂度。而Java直接把C/C++的指针给废了，用了更为安全的引用 ，然后为了解决多个引用共享同一个内存，内置了引用计数和垃圾回收，于是整个复杂度大大降低。对于Java要传对象的复本的话，需要定义一个通过自己构造自己的构造函数，或是通过prototype设计模式的 `clone()` 方法来进行，如果你要让Java解除引用，需要明显的把引用变量赋成 `null` 。总之，无论什么语言都需要这对象的传递这个事做好，不然，无法提供相对比较灵活编程方法。

在Rust中，Rust强化了“所有权”的概念，下面是Rust的所有者的三大铁律：

1. Rust 中的每一个值都有一个被称为其 **所有者**（owner）的变量。
2. 值有且只有一个所有者。
3. 当所有者（变量）离开作用域，这个值将被丢弃。

这意味着什么？

如果你需要传递一个对象的复本，你需要给这个对象实现 `Copy` trait ，**trait** 怎么翻译我也不知道，你可以认为是一个对象的一些特别的接口（可以用于一些对像操作上的约定，比如：`Copy` 用于复制（类型于C++的拷贝构造和赋值操作符重载），`Display` 用于输出（类似于Java的`toString()`），还有 `Drop` 和操作符重载等等，当然，也可以是对象的方法，或是用于多态的接口定义，后面会讲）。

对于内建的整型、布尔型、浮点型、字符型、多元组都被实现了 `Copy` 所以，在进行传递的时候，会进行`memcpy` 这样的复制（bit-wise式的浅拷贝）。而对于对象来说，则不行，在Rust的编程范式中，需要使用的是 `Clone` trait。

于是，`Copy` 和 `Clone` 这两个相似而又不一样的概念就出来了，`Copy` 主要是给内建类型，或是由内建类型全是支持 `Copy` 的对象，而 `Clone` 则是给程序员自己复制对象的。嗯，这就是浅拷贝和深拷贝的差别，`Copy` 告诉编译器，我这个对象可以进行 bit-wise的复制，而 `Clone` 则是指深度拷贝。

像 `String` 这样的内部需要在堆上分布内存的数据结构，是没有实现`Copy` 的（因为内部是一个指针，所以，语义上是深拷贝，浅拷贝会招至各种bug和crash），需要复制的话，必需手动的调用其`clone()` 方法，如果不这样的的话，当在进行函数参数传递，或是变量传递的时候，所有权一下就转移了，而之前的变量什么也不是了（这里编译器会帮你做检查有没有使用到所有权被转走的变量）。这个相当于C++的Move语义。

参看下面的示例，你可能对Rust自动转移所有权会有更好的了解（代码中有注释了，我就不多说了）。

```rust
// takes_ownership 取得调用函数传入参数的所有权，因为不返回，所以变量进来了就出不去了
fn takes_ownership(some_string: String) {
    println!("{}", some_string);
} // 这里，some_string 移出作用域并调用 `drop` 方法。占用的内存被释放

// gives_ownership 将返回值移动给调用它的函数
fn gives_ownership() -> String {
    let some_string = String::from("hello"); // some_string 进入作用域.
    some_string // 返回 some_string 并移出给调用的函数
}

// takes_and_gives_back 将传入字符串并返回该值
fn takes_and_gives_back(mut a_string: String) -> String {
    a_string.push_str(", world");
    a_string  // 返回 a_string 将所有权移出给调用的函数
}

fn main()
{
    // gives_ownership 将返回值移给 s1
    let s1 = gives_ownership();
    // 所有权转给了 takes_ownership 函数, s1 不可用了
    takes_ownership(s1);
    // 如果编译下面的代码，会出现s1不可用的错误
    // println!("s1= {}", s1);
    //                    ^^ value borrowed here after move
    let s2 = String::from("hello");// 声明s2
    // s2 被移动到 takes_and_gives_back 中, 它也将返回值移给 s3。
    // 而 s2 则不可用了。
    let s3 = takes_and_gives_back(s2);
    //如果编译下面的代码，会出现可不可用的错误
    //println!("s2={}, s3={}", s2, s3);
    //                         ^^ value borrowed here after move
    println!("s3={}", s3);
}
```

这样的 Move 的方式，在性能上和安全性上都是非常有效的，而Rust的编译器会帮你检查出使用了所有权被move走的变量的错误。而且，我们还可以从函数栈上返回对象了，如下所示：

```rust
fn new_person() -> Person {
    let person = Person {
        name : String::from("Hao Chen"),
        age : 44,
        sex : Sex::Male,
        email: String::from("haoel@hotmail.com"),
    };
    return person;
}

fn main() {
   let p  = new_person();
}
```

因为对象是Move走的，所以，在函数上 `new_person()` 上返回的 `Person` 对象是Move 语言，被Move到了 `main()` 函数中来，这样就没有性能上的问题了。而在C++中，我们需要把对象的Move函数给写出来才能做到。因为，C++默认是调用拷贝构造函数的，而不是Move的。

### Owner语义带来的复杂度

Owner + Move 的语义也会带来一些复杂度。首先，如果有一个结构体，我们把其中的成员 Move 掉了，会怎么样。参看如下的代码：

```rust
#[derive(Debug)] // 让结构体可以使用 `{:?}`的方式输出
struct Person {
    name :String,
    email:String,
}

let _name = p.name; // 把结构体 Person::name Move掉
println!("{} {}", _name, p.email); //其它成员可以正常访问
println!("{:?}", p); //编译出错 "value borrowed here after partial move"
p.name = "Hao Chen".to_string(); // Person::name又有了。
println!("{:?}", p); //可以正常的编译了
```

上面这个示例，我们可以看到，结构体中的成员是可以被Move掉的，Move掉的结构实例会成为一个部分的未初始化的结构，如果需要访问整个结构体的成员，会出现编译问题。但是后面把 Person::name补上后，又可以愉快地工作了。

下面我们再看一个更复杂的示例——这个示例模拟动画渲染的场景，我们需要有两个buffer，一个是正在显示的，另一个是下一帧要显示的。

```rust
struct Buffer {
    buffer : String,
}

struct Render {
    current_buffer : Buffer,
    next_buffer : Buffer,
}
//实现结构体 `Render` 的方法
impl Render { 
    //实现 update_buffer() 方法，
    //更新buffer，把 next 更新到 current 中，再更新 next
    fn update_buffer(& mut self, buf : String) {
        self.current_buffer = self.next_buffer;
        self.next_buffer = Buffer{ buffer: buf};
    }
}
```

上面这段代码，我们写下来没什么问题，但是 Rust 编译不会让我们编译通过。它会告诉我们如下的错误：

error[E0507]: cannot move out of `self.next_buffer` which is behind a mutable reference

```rust
error[E0507]: cannot move out of `self.next_buffer` which is behind a mutable reference
--> /.........../xxx.rs:18:31
|
14 | self.current_buffer = self.next_buffer;
|                          ^^^^^^^^^^^^^^^^ move occurs because `self.next_buffer` has type `Buffer`,
                                            which does not implement the `Copy` trait
```

编译器会提示你，`Buffer` 没有 Copy trait 方法。**但是，如果你实现了 Copy 方法后，你又不能享受 Move 带来的性能上快乐了。于是，到这里，你开始进退两难了，完全不知道取舍了**。

- Rust编译器不让我们在成员方法中把成员Move走，因为 `self` 引用就不完整了。
- Rust要我们实现 `Copy` Trait，但是我们不想要拷贝，因为我们就是想把 `next_buffer` move 到`current_buffer` 中

我们想要同时 Move 两个变量，参数 `buf` move 到 `next_buffer` 的同时，还要把 `next_buffer` 里的东西 move 到 `current_buffer` 中。 我们需要一个“杂耍”的技能。
![img](https://www.pianshen.com/images/47/ee591dc8236922b29abb69297a753c8f.gif)

这个需要动用 `std::mem::replace(&dest, src)` 函数了， 这个函数技把 `src` 的值 move 到 `dest`中，然后把 `dest` 再返回出来（这其中使用了 unsafe 的一些底层骚操作才能完成）。Anyway，最终是这样实现的：

```rust
use std::mem::replace
fn update_buffer(& mut self, buf : String) { 
  self.current_buffer = replace(&mut self.next_buffer, Buffer{buffer : buf}); 
}
```

不知道你觉得这样“杂耍”的代码看上去怎么以样？我觉得可读性下性一个数量级。

#### 引用（借用）和生命周期

下面，我们来讲讲引用，因为把对象的所有权 Move 走了的情况，在一些时候肯定不合适，比如，我有一个 `compare(s1: Student, s2: Student) -> bool` 我想比较两个学生的平均份成绩， 我不想传复本，因为太慢，我也不想把所有权交进去，因为只是想计算其中的数据。这个时候，传引用就是一个比较好的选择，Rust同样支持传引用。只需要把上面的函数声明改成：`compare(s1 :&Student, s2 : &Student) -> bool` 就可以了，在调用的时候，`compare (&s1, &s2);` 与C++一致。在Rust中，这也叫“借用”（嗯，Rust发明出来的这些新术语，在语义上感觉让人更容易理解了，当然，也增加了学习的复杂度了）

#### 引用（借用）

另外，如果你要修改这个引用对象，就需要使用“可变引用”，如：`foo( s : &mut Student)` 以及`foo( &mut s);`另外，为了避免一些数据竞争需要进行数据同步的事，Rust严格规定了——**在任意时刻，要么只能有一个可变引用，要么只能有多个不可变引用**。

这些严格的规定会导致程序员失去编程的灵活性，不熟悉Rust的程序员可能会在一些编译错误下会很崩溃，但是你的代码的稳定性也会提高，bug率也会降低。

另外，Rust为了解决“野引用”的问题，也就是说，有多个变量引用到一个对象上，还不能使用额外的引用计数来增加程序运行的复杂度。那么，Rust就要管理程序中引用的生命周期了，而且还是要在编译期管理，如果发现有引用的生命周期有问题的，就要报错。比如：

```rust
let r;
{
    let x = 10;
    r = &x;
}
println!("r = {}",r );
```

上面的这段代码，程序员肉眼就能看到 `x` 的作用域比 `r` 小，所以导致 `r` 在 `println()` 的时候 `r`引用的 `x` 已经没有了。这个代码在C++中可以正常编译而且可以执行，虽然最后可以打出“内嵌作用域”的 `x` 的值，但其实这个值已经是有问题的了。而在 Rust 语言中，编译器会给出一个编译错误，告诉你，“`x` dropped here while still borrowed”，这个真是太棒了。

但是这中编译时检查的技术对于目前的编译器来说，只在程序变得稍微复杂一点，编译器的“失效引用”检查就不那么容易了。比如下面这个代码：

```rust
fn order_string(s1 : &str, s2 : &str) -> (&str, &str) {
    if s1.len() < s2.len() {
        return (s1, s2);
    }
    return (s2, s1);
}

let str1 = String::from("long long long long string");
let str2 = "short string";

let (long_str, short_str) = order_string(str1.as_str(), str2);

println!(" long={} nshort={} ", long_str, short_str);
```

我们有两个字符串，`str1` 和 `str2` 我们想通过函数 `order_string()` 把这两个字串符返回成`long_str` 和 `short_str` 这样方便后面的代码进行处理。这是一段很常见的处理代码的示例。然而，你会发现，这段代码编译不过。编译器会告诉你，`order_string()` 返回的 引用类型 `&str` 需要一个 lifetime的参数 – “ expected lifetime parameter”。这是因为Rust编译无法通过观察静态代码分析返回的两个引用返回值，到底是`(s1, s2)` 还是 `(s2, s1)` ，因为这是运行时决定的。所以，返回值的两个参数的引用没法确定其生命周期到底是跟 `s1` 还是跟 `s2`，这个时候，编译器就不知道了。

生命周期

如果你的代码是下面这个样子，编程器可以自己推导出来，函数 `foo()` 的参数和返回值都是一个引用，他们的生命周期是一样的，所以，也就可以编译通过。

```rust
fn foo (s: &mut String) -> &String {
    s.push_str("coolshell");
    s
}

let mut s = "hello, ".to_string();
println!("{}", foo(&mut s))
```

而对于传入多个引用，返回值可能是任一引用，这个时候编译器就犯糊涂了，因为不知道运行时的事，所以，就需要程序员来标注了。

```rust
fn long_string<'c>(s1 : &'c str, s2 : &'c str) -> (&'c str, &'c str) {
    if s1.len() > s2.len() {
        return (s1, s2);
    }
    return (s2, s1);
}
```

上述的Rust的标注语法，用个单引号加一个任意字符串来标注（`'static`除外，这是一个关键词，表示生命周期跟整个程序一样长），然后，说明返回的那两个引用的生命周期跟 `s1` 和 `s2` 的生命周期相同，这个标注的目的就是把运行时的事变成了编译时的事。于是程序就可以编译通过了。（注：你也不要以为你可以用这个技术乱写生命周期，这只是一种“去语法糖操作”，是帮助编译器理解其中的生命周期，如果违反实际生命周期，编译器也是会拒绝编译的）

这里有两个说明，

- 只要你玩引用，生命周期标识就会来了。
- Rust编译器不知道运行时会发生什么事，所以，需要你来标注声明

我感觉，你现在开始有点头晕了吧？接下来，我们让你再晕一下。比如：如果你要在结构体中玩引用，那必需要为引用声明生命周期，如下所示：

```rust
// 引用 ref1 和 ref2 的生命周期与结构体一致
struct Test <'life> {
    ref_int : &'life i32,
    ref_str : &'life str,
}
```

其中，生命周期标识 `'life` 定义在结构体上，被使用于其成员引用上。意思是声明规则——“**结构体的生命周期 <= 成员引用的生命周期**”

然后，如果你要给这个结构实现两个 `set` 方法，你也得带上 lifetime 标识。

```rust
imp<'life> Test<'life> {
    fn set_string(&mut self, s : &'life str) {
        self.ref_str = s;
    }
    fn set_int(&mut self,  i : &'life i32) {
        self.ref_int = i;
    }
}
```

在上面的这个示例中，生命周期变量 `'life` 声明在 `impl` 上，用于结构体和其方法的入参上。 意思是声明规则——“**结构体方法的“引用参数”的生命周期 >= 结构体的生命周期**”

有了这些个生命周期的标识规则后，Rust就可以愉快地检查这些规则说明，并编译代码了。

### 闭包与所有权

这种所有权和引用的严格区分和管理，会影响到很多地方，下面我们来看一下函数闭包中的这些东西的传递。函数闭包又叫Closure，是函数式编程中一个不可或缺的东西，又被称为lambda表达式，基本上所有的高级语言都会支持。在 Rust 语言中，其闭包函数的表示是用两根竖线（| |）中间加传如参数进行定义。如下所示：

```rust
// 定义了一个 x + y 操作的 lambda f(x, y) = x + y;
let plus = |x: i32, y:i32| x + y; 
// 定义另一个lambda g(x) = f(x, 5)
let plus_five = |x| plus(x, 5); 
//输出
println!("plus_five(10)={}", plus_five(10) );
```

#### 函数闭包

但是一旦加上了上述的所有权这些东西后，问题就会变得复杂开来。参看下面的代码。

```rust
struct Person {
    name : String,
    age : u8,
}

fn main() {
    let p = Person{ name: "Hao Chen".to_string(), age : 44};
    //可以运行，因为 `u8` 有 Copy Trait
    let age = |p : Person| p.age; 
    // String 没有Copy Trait，所以，这里所有权就 Move 走了
    let name = |p : Person | p.name; 
    println! ("name={}, age={}" , name(p), age(p));
}
```

上面的代码无法编译通过，因为Rust编译器发现在调用 `name(p)` 的时候，`p` 的所有权被移走了。然后，我们想想，改成引用的版本，如下所示：

```rust
let age = |p : &Person| p.age;
let name = |p : &Person | &p.name;

println! ("name={}, age={}" , name(&p), age(&p));
```

你会现在还是无法编译，报错中说：**cannot infer an appropriate lifetime for borrow expression due to conflicting requirements**

```rust
error[E0495]: cannot infer an appropriate lifetime for borrow expression due to conflicting requirements
  --> src/main.rs:11:31
   |
11 |     let name = |p : &Person | &p.name;
   |                               ^^^^^^^
```

然后你开始尝试加 lifetime，用尽各种Rust的骚操作（官方Github上的[ #issue 58052](https://github.com/rust-lang/rust/issues/58052)），然后，还是无法让你的程序可以编译通过。最后，上StackOverflow 里寻找帮助，得到下面的正确写法（这个可能跟这个bug有关系：[#issue 41078](https://github.com/rust-lang/rust/issues/41078) ）。但是这样的写法，已经让简洁的代码变得面目全非。

```rust
//下面的声明可以正确译
let name: for<'a> fn(&'a Person) -> &'a String = |p: &Person| &p.name;
```

上面的这种lifetime的标识也是很奇葩，通过定义一个函数类型来做相关的标注，但是这个函数类型，需要用到 `for<'a>` 关键字。你可能会很confuse这个关键字不是用来做循环的吗？嗯，Rust这种重用关键字的作法，我个人觉得带来了很多不必要的复杂度。总之，这样的声明代码，我觉得基本不会有人能想得到的——“去语法糖操作太严重了，绝大多数人绝对hold不住”！

最后，我们再来看另一个问题，下面的代码无法编译通过：

```rust
let s = String::from("coolshell");
let take_str = || s;
println!("{}", s); //ERROR
println!("{}",  take_str()); // OK
```

Rust的编译器会告诉你，`take_str` 把 `s` 的所有权给拿走了（因为需要作成返回值）。所以，后面的输出语句就用不到了。这里意味着：

- 对于内建的类型，都实现了 `Copy` 的 trait，那么闭包执行的是 “借用”
- 对于没有实现 `Copy` 的trait，在闭包中可以调用其方法，是“借用”，但是不能当成返回值，当成返回值了就是“移动”。

虽然有了这些“通常情况下是借用的潜规则”，但是还是不能满足一些情况，所以，还要让程序员可以定义 `move` 的“明规则”。下面的代码，一个有 move 一个没有move，他们的差别也不一样。

```rust
//-----------借用的情况-----------
let mut num = 5;
{
    let mut add_num = |x: i32| num += x;
    add_num(5);
}
println!("num={}", num); //输出 10

//-----------Move的情况-----------
let mut num = 5;
{
    // 把 num（5）所有权给 move 到了 add_num 中，
    // 使用其成为闭包中的局部变量。
    let mut add_num = move |x: i32| num += x;
    add_num(5);
    println!("num(move)={}", num); //输出10
}
//因为i32实现了 `Copy`，所以，这里还可以访问
println!("num(move)={}", num); //输出5
```

真是有点头大了，int这样的类型，因为实现了Copy Trait，所以，所有权被移走后，意味着，在内嵌块中的`num` 和外层的 `num` 是两个完全不相干的变量。**但是你在读代码的时候，你的大脑可能并不会让你这么想，因为里面的那个num又没有被声明过，应该是外层的**。我个人觉得这是Rust 各种“按下葫芦起了瓢”的现象。

#### 线程闭包

通过上面的示例，我们可以看到， `move` 关键词，可以把闭包外使用到的变量给移动到闭包内，成为闭包内的一个局部变量。这种方式，在多线程的方式下可以让线程运行地更为的安全。参看如下代码：

```rust
let name = "CoolShell".to_string();
let t = thread::spawn(move || {
    println!("Hello, {}", name);
});
println!("wait {:?}", t.join());
```

首先，线程 `thread::spawn()` 里的闭包函数是不能带参数的，因为是闭包，所以可以使用这个可见范围内的变量，但是，问题来了，因为是另一个线程，所以，这代表其和其它线程（如：主线程）开始共享数据了，所以，在Rust下，要求把使用到的变量给 Move 到线程内，这就保证了安全的问题—— `name` 在编程中永远不会失效，而且不会被别人改了。

你可能会有一些疑问，你会质疑到

- 一方面，这个 `name` 变量又没有声明成 `mut` 这意味着不变，没必要使用move语义也是安全的。
- 另一方面，如果我想把这个 `name` 传递到多个线程里呢？

嗯，是的，但是Rust的线程必需是 move的，不管是不是可变的，不然编译不过去。如果你想把一个变量传到多个线程中，你得创建变量的复本，也就是调用 `clone()` 方法。

```rust
let name = "CoolShell".to_string();
let name1 = name.clone();
let t1 = thread::spawn(move || {
    println!("Hello, {}", name.clone());
})
let t2 = thread::spawn(move || {
    println!("Hello, {}", name1.clone());
});
println!("wait t1={:?}, t2={:?}", t1.join(), t2.join());
```

然后，你说，这种clone的方式成本不是很高？设想，如果我要用多线程对一个很大的数组做统计，这种clone的方式完全吃不消。嗯，是的。这个时候，需要使用另一个技术，智能指针了。

Rust的智能指针

如果你看到这里还不晕的话，那么，我的文章还算成功（如果晕的话，请告诉我，我会进行改善）。接下来我们来讲讲Rust的智能指针和多态。

因为有些内存需要分配在Heap（堆）上，而不是Stack（堆）上，Stack上的内存一般是编译时决定的，所以，编译器需要知道你的数组、结构构、枚举等这些数据类型的长度，没有长度是无法编译的，而且长度也不能太大，Stack上的内存大小是有限，太大的内存会有StackOverflow的错误。所以，对于更大的内存或是动态的内存分配需要分配在Heap上。学过C/C++的同学对于这个概念不会陌生。

Rust 作为一个内存安全的语言，这个堆上分配的内存也是需要管理的。在C中，需要程序员自己管理，而在C++中，一般使用 [RAII 的机制](https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization)（面向对象的代理模式），一种通过分配在Stack上的对象来管理Heap上的内存的技术。在C++中，这种技术的实现叫“智能指针”（Smart Pointer）。

在C++11中，会有三种智能指针（这三种指针是什么我就不多说了）：

- `unique_ptr`。独占内存，不共享。在Rust中是：`std::boxed::Box`
- `shared_ptr`。以引用计数的方式共享内存。在Rust中是：`std::rc::Rc`
- `weak_ptr`。不以引用计数的方式共享内存。在Rust中是：`std::rc::Weak`

对于独占的 `Box` 不多说了，这里重点说一下共享的 `Rc` 和 `Weak` ：

- 对于Rust的 Rc 来说，Rc指针内会有一个 `strong_count` 的引用持计数，一旦引用计数为0后，内存就自动释放了。
- 需要共享内存的时候，需要调用实例的 `clone()` 方法。如： `let another = rc.clone()` 克隆的时候，只会增加引用计数，不会作深度复制（个人觉得Clone的语义在这里被践踏了）
- 有这种共享的引用计数，就意味着有多线程的问题，所以，如果需要使用线程安全的智能指针，则需要使用`std::sync::Arc`
- 可以使用 `Rc::downgrade(&rc)` 后，会变成 Weak 指针，Weak指针增加的是 `weak_count` 的引用计数，内存释放时不会检查它是否为 0。

我们简单的来看个示例：

```rust
use std::rc::Rc;
use std::rc::Weak

//声明两个未初始化的指针变量
let weak : Weak; 
let strong : Rc;
{
    let five = Rc::new(5); //局部变量
    strong = five.clone(); //进行强引用
    weak = Rc::downgrade(&five); //对局部变量进行弱引用
}
//此时，five已析构，所以 Rc::strong_count(&strong)=1， Rc::weak_count(&strong)=1
//如果调用 drop(strong)，那个整个内存就释放了
//drop(strong);

//如果要访问弱引用的值，需要把弱引用 upgrade 成强引用，才能安全的使用
match  weak_five.upgrade() {
    Some(r) => println!("{}", r),
    None => println!("None"),
} 
```

上面这个示例比较简单，其中主要展示了，指针共享的东西。因为指针是共享的，所以，对于强引用来说，最后的那个人把引用给释放了，是安全的。但是对于弱引用来说，这就是一个坑了，你们强引用的人有Ownership，但是我们弱引用没有，你们把内存释放了，我怎么知道？

于是，**在弱引用需要使用内存的时候需要“升级”成强引用 ，但是这个升级可能会不成功，因为内存可能已经被别人清空了**。所以，这个操作会返回一个 `Option` 的枚举值，`Option::Some(T)` 表示成功了，而 `Option::None` 则表示失改了。你会说，这么麻烦，我们为什么还要 `Weak` ? 这是因为强引用的 `Rc` 会有循环引用的问题……（学过C++的都应该知道）

另外，如果你要修改 `Rc` 里的值，Rust 会给你两个方法，一个是 `get_mut()`，一个是 `make_mut()`，这两个方法都有副作用或是限制。

`get_mut()` 需要做一个“唯一引用”的检查，也就是没有任何的共享才能修改

```rust
//修改引用的变量 - get_mut 会返回一个Option对象
//但是需要注意，仅当（只有一个强引用 && 没有弱引用）为真才能修改
if let Some(val) = Rc::get_mut(&mut strong) {
    *val = 555;
}
```

`make_mut()` 则是会把当前的引用给clone出来，再也不共享了， 是一份全新的。

```rust
//此处可以修改，但是是以 clone 的方式，也就是让strong这个指针独立出来了。
*Rc::make_mut(&mut strong) = 555;
```

如果不这样做，就会出现很多内存不安全的情况。**这些小细节一定要注意，不然你的代码怎么运作的你会一脸蒙逼的**。

嗯，如果你想更快乐地使用智能指针，这里还有个选择 – `Cell` 和 `RefCell`，它们弥补了 Rust 所有权机制在灵活性上和某些场景下的不足。他们提供了 `set()`/`get()` 以及 `borrow()`/`borrow_mut()`的方法，让你的程序更灵活，而不会被限制得死死的。参看下面的示例。

```rust
use std::cell::Cell;
use std::cell::RefCell

let x = Cell::new(1);
let y = &x; //引用（借用）
let z = &x; //引用（借用）
x.set(2); // 可以进行修改，x，y，z全都改了
y.set(3);
z.set(4);
println!("x={} y={} z={}", x.get(), y.get(), z.get());

let x = RefCell::new(vec![1,2,3,4]);
{
    println!("{:?}", *x.borrow())
}

{
    let mut my_ref = x.borrow_mut();
    my_ref.push(1);
}
println!("{:?}", *x.borrow());
```

通过上面的示例你可以看到你可以比较方便地更为正常的使用智能指针了。然而，需要注意的是`Cell` 和 `RefCell` 不是线程安全的。在多线程下，需要使用Mutex进行互斥。

线程与智能指针

现在，我们回来来解决前面那还没有解决的问题，就是——我想在多个线程中共享一个只读的数据，比如：一个很大的数组，我开多个线程进行并行统计。我们肯定不能对这个大数组进行clone，但也不能把这个大数组move到一个线程中。根据上述的智能指针的逻辑，我们可以通过智指指针来完成这个事，下面是一个例程：

```rust
const TOTAL_SIZE:usize = 100 * 1000; //数组长度
const NTHREAD:usize = 6; //线程数

let data : Vec<i32> = (1..(TOTAL_SIZE+1) as i32).collect(); //初始化一个数据从1到n数组
let arc_data = Arc::new(data); //data 的所有权转给了 ar_data
let result  = Arc::new(AtomicU64::new(0)); //收集结果的数组(原子操作)

let mut thread_handlers = vec![]; // 用于收集线程句柄

for i in 0..NTHREAD {
    // clone Arc 准备move到线程中，只增加引用计数，不会深拷贝内部数据
    let test_data = arc_data.clone(); 
    let res = result.clone(); 
    thread_handlers.push( 
        thread::spawn(move || {
            let id = i;
            //找到自己的分区
            let chunk_size = TOTAL_SIZE / NTHREAD + 1;
            let start = id * chunk_size;
            let end = std::cmp::min(start + chunk_size, TOTAL_SIZE);
            //进行求和运算
            let mut sum = 0;
            for  i in start..end  {
                sum += test_data[i];
            }
            //原子操作
            res.fetch_add(sum as u64, Ordering::SeqCst);
            println!("id={}, sum={}", id, sum );
        }
    ));
}
//等所有的线程执行完
for th in thread_handlers {
    th.join().expect("The sender thread panic!!!");
}
//输出结果
println!("result = {}",result.load(Ordering::SeqCst));
```

上面的这个例程，是用多线程的方式来并行计算一个大的数组的和，每个线程都会计算自己的那一部分。上面的代码中，

- 需要向每个线程传入一个只读的数组，我们用`Arc` 智能指针把这个数组包了一层。
- 需要向每个线程传入一个变量用于数据数据，我们用 `Arc<AtomicU64>` 包了一层。
- 注意：`Arc` 所包的对象是不可变的，所以，如果要可变的，那要么用原子对象，或是用Mutex/Cell对象再包一层。

这一些都是为了要解决“线程的Move语义后还要共享问题”。

### 多态和运行时识别

#### 通过Trait多态

多态是抽象和解耦的关键，所以，一个高级的语言是必需实现多态的。在C++中，多态是通过虚函数表来实现的（参看《[C++的虚函数表](https://coolshell.cn/articles/12165.html)》），Rust也很类似，不过，在编程范式上，更像Java的接口的方式。其通过借用于Erlang的Trait对象的方式来完成。参看下面的代码：

```rust
struct Rectangle {
    width : u32,
    height : u32,
} 

struct Circle {
    x : u32,
    y : u32,
    radius : u32,
}

trait  IShape  { 
    fn area(&self) -> f32;
    fn to_string(&self) -> String;
}
```

我们有两个类，一个是“长方形”，一个是“圆形”， 还有一个 `IShape` 的trait 对象（原谅我用了Java的命名方式），其中有两个方法：求面积的 `area()` 和 转字符串的 `to_string()`。下面相关的实现：

```rust
impl IShape  for Rectangle {
    fn area(&self) -> f32 { (self.height * self.width) as f32 }
    fn to_string(&self) ->String {
         format!("Rectangle -> width={} height={} area={}", 
                  self.width, self.height, self.area())
    }
}

use std::f64::consts::PI;
impl IShape  for Circle  {
    fn area(&self) -> f32 { (self.radius * self.radius) as f32 * PI as f32}
    fn to_string(&self) -> String {
        format!("Circle -> x={}, y={}, area={}", 
                 self.x, self.y, self.area())
    }
}
```

于是，我们就可以有下面的多态的使用方式了（我们使用独占的智能指针类 `Box`）：

```rust
use std::vec::Vec;

let rect = Box::new( Rectangle { width: 4, height: 6});
let circle = Box::new( Circle { x: 0, y:0, radius: 5});
let mut v : Vec<Box> = Vec::new();
v.push(rect);
v.push(circle);

for i in v.iter() {
   println!("area={}", i.area() );
   println!("{}", i.to_string() );
}
```

#### 向下转型

但是，在C++中，多态的类型是抽象类型，我们还想把其转成实际的具体类型，在C++中叫运行进实别RTTI，需要使用像 `type_id` 或是 `dynamic_cast` 这两个技术。在Rust中，转型是使用 ‘`as`‘ 关键字，然而，这是编译时识别，不是运行时。那么，在Rust中是怎么做呢？

嗯，这里需要使用 Rust 的 `std::any::Any` 这个东西，这个东西就可以使用 `downcast_ref` 这个东西来进行具体类型的转换。于是我们要对现有的代码进行改造。

首先，先得让 `IShape` 继承于 `Any` ，并增加一个 `as_any()` 的转型接口。

```rust
use std::any::Any;
trait  IShape : Any + 'static  {
    fn as_any(&self) -> &dyn Any; 
    …… …… …… 
}
```

然后，在具体类中实现这个接口：

```rust
impl IShape  for Rectangle {
    fn as_any(&self) -> &dyn Any { self }
    …… …… …… 
}
impl IShape  for Circle  {
    fn as_any(&self) -> &dyn Any { self }
    …… …… …… 
}
```

于是，我们就可以进行运行时的向下转型了：

```rust
let mut v : Vec<Box<dyn IShape>> = Vec::new();
v.push(rect);
v.push(circle);
for i in v.iter() {
    if let Some(s) = i.as_any().downcast_ref::<Rectangle>() {
        println!("downcast - Rectangle w={}, h={}", s.width, s.height);
    }else if let Some(s) = i.as_any().downcast_ref::<Circle>() {
        println!("downcast - Circle x={}, y={}, r={}", s.x, s.y, s.radius);
    }else{
        println!("invaild type");
    }
}
```

#### Trait 重载操作符

操作符重载对进行泛行编程是非常有帮助的，如果所有的对象都可以进行大于，小于，等于这亲的比较操作，那么就可以直接放到一个标准的数组排序的的算法中去了。在Rust中，在 `std::ops` 下有全载的操作符重载的Trait，在`std::cmp` 下则是比较操作的操作符。我们下面来看一个示例：

假如我们有一个“员工”对象，我们想要按员工的薪水排序，如果我们想要使用`Vec::sort()`方法，我们就需要实现这个对象的各种“比较”方法。这些方法在 `std::cmp` 内—— 其中有四个Trait :`Ord`、`PartialOrd` 、`Eq` 和 `PartialEq` 。其中，`Ord` 依赖于 `PartialOrd` 和 `Eq` ，而`Eq` 依赖于`PartialEq`，这意味着你需要实现所有的Trait，而`Eq` 这个Trait 是没有方法的，所以，其实现如下：

```rust
use std::cmp::{Ord, PartialOrd, PartialEq, Ordering};

#[derive(Debug)]
struct Employee {
    name : String,
    salary : i32,
}
impl Ord for Employee {
    fn cmp(&self, rhs: &Self) -> Ordering {
        self.salary.cmp(&rhs.salary)
    }
}
impl PartialOrd for Employee {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}
impl Eq for Employee {
}
impl PartialEq for Employee {
    fn eq(&self, rhs: &Self) -> bool {
        self.salary == rhs.salary
    }
}
```

于是，我们就可以进行如下的操作了：

```rust
let mut v = vec![
    Employee {name : String::from("Bob"),     salary: 2048},
    Employee {name : String::from("Alice"),   salary: 3208},
    Employee {name : String::from("Tom"),     salary: 2359},
    Employee {name : String::from("Jack"),    salary: 4865},
    Employee {name : String::from("Marray"),  salary: 3743},
    Employee {name : String::from("Hao"),     salary: 2964},
    Employee {name : String::from("Chen"),    salary: 4197},
];

//用for-loop找出薪水最多的人
let mut e = &v[0];
for i in 0..v.len() {
    if *e < v[i] { 
        e = &v[i]; 
    }
}
println!("max = {:?}", e);

//使用标准的方法
println!("min = {:?}", v.iter().min().unwrap());
println!("max = {:?}", v.iter().max().unwrap());

//使用标准的排序方法
v.sort();
println!("{:?}", v);
```

### 小结

现在我们来小结一下：

- 在Rust的中，最重要的概念就是“不可变”和“所有权”以及“Trait”这三个概念。
- 在所有权概念上，Rust喜欢move所有权，如果需要借用则需要使用引用。
- Move所有权会导致一些编程上的复杂度，尤其是需要同时move两个变量时。
- 引用（借用）的问题是生命周期的问题，一些时候需要程序员来标注生命周期。
- 在函数式的闭包和多线程下，这些所有权又出现了各种麻烦事。
- 使用智能指针可以解决所有权和借用带来的复杂度，但带来其它的问题。
- 最后介绍了Rust的Trait对象完成多态和函数重载的玩法。

Rust是一个比较严格的编程语言，它会严格检查你程序中的：

- 变量是否是可变的
- 变量的所有权是否被移走了
- 引用的生命周期是否完整
- 对象是否需要实现一些Trait

这些东西都会导致失去编译的灵活性，并在一些时候需要“去糖”，导致，你在使用Rust会有诸多的不适应，程序编译不过的挫败感也是令人沮丧的。在初学Rust的时候，我想自己写一个单向链表，结果，费尽心力，才得以完成。也就是说，**如果你对Rust的概念认识的不完整，你完全写不出程序，那怕就是很简单的一段代码**。我觉得，这种挺好的，逼着程序员必需了解所有的概念才能编码。但是，另一方面也表明了这门语言并不适合初学者。

没有银弹，任何语言都有些适合的地方和场景。