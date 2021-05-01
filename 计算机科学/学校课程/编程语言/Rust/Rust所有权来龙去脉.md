[TOC]

## 序

Rust 的所有权在 Rust 编程中有重要的地位， 基本上理解了 Rust 所有权可以说一只脚就跨境 Rust 大门了。本文面向 Rust 初学者以及在所有权和借用等概念做一个梳理， 如果你曾经有从入门到放弃， 可能是你碰到的学习资料有问题， 未必是你自己的问题， 因为很多资料忽略了一些前因后果， 容易让人蒙圈。碰巧我自己也是一个初学者， 所以我能懂的， 对于你难度可能也不大。本文内容实际是一篇笔记， 内容取自 `Rust Doc` 和 `Rust programming book`, 内容上稍微有点冗余， 不过对理解是有好处的。

## Rust Doc: 所有权和生命周期

> 一条核心原则：共享不可变， 可变不共享

### Default match bindings

在Rust 2018中这种match用法是自然的：

```
let s: &Option<String> = &Some("hello".to_string());
match s {
    Some(s) => println!("s is: {}", s),
    _ => (),
};
```

但是， 在Rust 2015中需要费劲的怎么写才行：

```
// Rust 2015
let s: &Option<String> = &Some("hello".to_string());
match s {
    &Some(ref s) => println!("s is: {}", s),
    _ => (),
};
```

> 理解：编译器帮你完成了自动推导ref 和 &

> 结论：除了match之外， 还有其他有类似的影响

- let statements
- closure arguments
- for loops

> more details:

```
Rust 2015里match默认都是 by-value模式， 如果要by-reference, 必需显示指定。
```

### Lifetime elision in impl

在impl语句中的lifetime省略。

```
trait MyIterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
}
```

Rust2015中的写法：

```
impl<'a, I: MyIterator> MyIterator for &'a mut I {
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        (*self).next()
    }
}
```

Rust2018中的写法：

```
impl<I: MyIterator> MyIterator for &mut I {
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        (*self).next()
    }
}
```

> `'a` 都可以省略掉

```
struct SetOnDrop<'a, T> {
    borrow: &'a mut T,
    value: Option<T>,
}
```

Rust2015中为上述Struct实现`Drop`:

```
impl<'a, T> Drop for SetOnDrop<'a, T> {
    fn drop(&mut self) {
        if let Some(x) = self.value.take() {
            *self.borrow = x;
        }
    }
}
```

Rust2018中写法：

```
impl<T> Drop for SetOnDrop<'_, T> {
    fn drop(&mut self) {
        if let Some(x) = self.value.take() {
            *self.borrow = x;
        }
    }
}
```

> 在Rust2018中可以结合省略和匿名lifetime.

### Simpler lifetimes in static and const

在Rust2015中const 和 static声明里都需要显式声明`'static`:

```
const NAME: &'static str = "Ferris";
static NAME: &'static str = "Ferris";
```

Rust2018中就不用了:

```
const NAME: &str = "Ferris";
static NAME: &str = "Ferris";
```

## Rust Book: Understanding Ownership

`所有权`是Rust**最独特的特性**，它使Rust能够在不需要GC的情况下保证`内存安全`。因此，了解所有权如何在Rust中工作是很重要的。在本章中，我们将讨论所有权以及几个相关特性:`借用`、`切片`以及Rust如何`在内存中存放数据`。

### What is Ownership?

关于内存管理有三种方式：

1. GC方式（Java）
2. 自己管理内存方式 (C/C++)
3. 所有权系统， 结合编译时检查 （Rust）

> 掌握所有权规则的好处：安全高效的编写代码.

本章我们从一个常见的数据结构 `string` 开始.

#### stack and heap

对于普通编程来说， 我们不用关注堆栈内存结构， 但是对于像 Rust 这样的系统编程语言来说是需要的.**stack**: 顺序存储， get 或者 remove 是相反的顺序， 被称为 `LIFO`. 大小确定， 如果大小不确定或者后面需要修改的， 则要保存到堆上。stack 上插入数据是 `push`, 取出数据是 `pop`.**heap**: less organized. 调用端发出请求， 操作系统做一个 `allocating` 动作：在堆上找到一个足够大的空间， 标记为使用状态， 并返回指针。指针地址可以存放在 stack 上， 但是实际的数据还是存放在 heap 上。stack 比 heap 分配空间速度更快， 因为操作系统不要查找， 直接在栈上分配就可以。对于函数来说， 其 local variable 都会 `pushed into stack`, 函数调用结束后， `popped off the stack`.

> 所有权需要解决的问题是:1、跟踪代码的哪些部分是使用的堆上的数据2、最小化堆上重复数据3、清除堆上不再使用的数据 （确保内存不会用光）

#### 所有权规则

- Each value in Rust has a variable that’s called its owner. (Rust 中的每一个值都有一个对应的变量，叫所有者）
- There can only be one owner at a time.( 一次只能有一个所有者）
- When the owner goes out of scope, the value will be dropped. （所有者超出范围， 其对应的值会被销毁

#### 变量范围 variable scope

**什么是 Scope**？A scope is the range within a program for which an item is valid.

```
{                      // s is not valid here, it’s not yet declared
    let s = "hello";   // s is valid from this point forward

    // do stuff with s
}                      // this scope is now over, and s is no longer valid
```

> 理解：When `s` comes into scope, it is valid.It remains valid until it goes out of scope.

#### `String` 类型

字符串字面量是预先知道大小, 比如 `let s = "Hello Rust"`.String 类型可以修改， 比如从终端接收输入的场景。

```
let mut s = String::from("hello");
s.push_str(", world!"); // push_str() appends a literal to a String
println!("{}", s); // This will print `hello, world!`
```

字符串字面量和 `String` 类型的区别是什么呢？本质是他们处理内存的方式不同。

#### Memory and Allocation

字符串字面量是在编译时就已经知道具体的存储的内容， 所以，为什么字符串字面量速度快性能好？是因为字符串字面量的不可更改特性 `immutability`.而对于编译时不能确定字符串内容的情况， 我们需要在堆上分配内存， 这意味着：

1. 内存是在运行时通过操作系统分配 『 `String::from(..)` 』
2. 当我们处理完字符串之后， 需要一个方式来将内存返回给操作系统

那么 Rust 是如何处理内存返回给操作系统的呢？答案是 Rust 发现变量超出它的范围之后**自动返还**.

```
{
    let s = String::from("hello"); // s is valid from this point forward
    // do stuff with s
}                                  // this scope is now over, and s is no
                                   // longer valid
```

> 说明：Rust 在大括号结束后， 自动调用 `drop` 函数, 借鉴的是 C++ 中的 `RAII`.这个范式对 Rust 代码的编写有着**深远的影响**， 尤其是当堆上有多个变量的复杂情形

#### 变量和数据的交互方式： `Move`

来看普通数字的例子：

```
let x = 5;
let y = x;
```

> 上述代码含义：1、将值与变量 x 绑定2、把 x 里的值做一份拷贝， 然后绑定到变量 y 。3、这里的 integer 是简单值， 并且大小确定， 两个 5 的值被存到栈上。

来看 String 字符串的例子：

```
let s1 = String::from("hello");
let s2 = s1;
```

![img](data:image/gif;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWNgYGBgAAAABQABh6FO1AAAAABJRU5ErkJggg==)

上述代码背后可以看到字符串由三部分组成：1、指向内存的指针 ptr, length 和 capacity.2、上述的数据保存在栈上3、 右侧是持有具体的内容的堆上内存

`let s2 = s1` 背后是：String 栈上数据被拷贝了一份， 包括指针, length 和 capacity。而右侧的对上数据并没有 copy.

![img](https://tva1.sinaimg.cn/large/007S8ZIlly1gjj9iip0r3j30a50a2jrd.jpg)



这个拷贝只是发生在栈上， 为什么不能在堆上也拷贝一份呢？答案是代价太大。

栈上数据拷贝了一份之后， 相当于一份数据有了两个指针， 那么就有如下问题：1、s1 和 s2 都指向同一个内存地址2、超出范围之后， s1 和 s2 都会自动释放内存3、他们释放的是相同的内存 （这个是不允许的）

为了解决这个问题， Rust 引入了 `Move`:1、在栈上做了一个类似 `shallow copy`2、随后将第一个变量失效, 即 `invalidate`3、这个过程称为 move， 在这里我们称为 `s1 was moved into s2`

示意图如下：

![img](https://mmbiz.qpic.cn/mmbiz_png/IxaA7MOpq6vLWVPlQPlBRibok7jqiaMkOI6YiakV8J5Br5s8ysqwiazK1oqrVu7RjoLne6E0YQiajI8uUl7gE1I6DPw/640?wx_fmt=png&tp=webp&wxfrom=5&wx_lazy=1&wx_co=1)



到这里， 问题解决了：只有 s2 变量是有效的， 当它超出范围后， 自动通过 s2 释放内存。

所以， 有了 Move 机制之后， 下面的代码在编译阶段会报错：

```
let s1 = String::from("hello");
let s2 = s1;
println!("{}, world!", s1);
error[E0382]: use of moved value: `s1`
 --> src/main.rs:5:28
  |
3 |     let s2 = s1;
  |         -- value moved here
4 |
5 |     println!("{}, world!", s1);
  |                            ^^ value used here after move
  |
  = note: move occurs because `s1` has type `std::string::String`, which does
  not implement the `Copy` trait
```

> 一个隐含的 Rust 设计选择：Rust 永远不会有 deep copy.因此， 对于 rust 运行时的自动拷贝没有昂贵的开销问题

#### 变量和数据交互的方式: `Clone`

根据前面的说明可知， Rust 默认不会有 deep copy, 但假如我们不想要栈的拷贝， 就是要堆上的拷贝如何做呢？答案是 `clone`.

```
let s1 = String::from("hello");
let s2 = s1.clone();
println!("s1 = {}, s2 = {}", s1, s2);
```

示意图如下：

![img](https://mmbiz.qpic.cn/mmbiz_png/IxaA7MOpq6vLWVPlQPlBRibok7jqiaMkOIdbDNLDqC7xicJbrhkrRnWUpZQlsoicwGXwDToqK83jyx6JjlBeYibJqzw/640?wx_fmt=png&tp=webp&wxfrom=5&wx_lazy=1&wx_co=1)



#### 只在 stack 上数据的拷贝

```
let x = 5;
let y = x;

println!("x = {}, y = {}", x, y);
```

提出一个问题：为何这里我们即没有显式调用 `clone`， 而且也没有发生 `move`， 而且 `x` 还是依然合法使用？

答案是 Rust 引入了 `Copy` 这个注解, 即对于栈上的数据显式调用 Clone 和默认的浅拷贝效果是等价的。

`Copy` 有如下特性：1、 赋值之后旧的变量依然可用2、如果一个变量已经实现了 `Drop` 这个 trait， 或者任何成员实现了 `Drop` 这个 trait 是禁止实现 `Copy` 的

那么有哪些类型已经实现了 `Copy` 注解呢？

- 所有的 `integer` 类型， 比如 u32, i32
- 布尔类型 `book`
- 浮点类型， 比如 `f64`
- 字符类型 `char`
- 所有成员实现 Copy 的`元组 Tuple`, 比如`(i32, i32)` is Copy, but `(i32, String)` is not

#### 所有权和函数

将值传给函数和将值传给变量的语义是一致的。即， 要么是 `copy` 要么是 `move`。

```
fn main() {
    let s = String::from("hello");  // s comes into scope

    takes_ownership(s);             // s's value moves into the function...
                                    // ... and so is no longer valid here

    let x = 5;                      // x comes into scope

    makes_copy(x);                  // x would move into the function,
                                    // but i32 is Copy, so it’s okay to still
                                    // use x afterward

} // Here, x goes out of scope, then s. But because s's value was moved, nothing
  // special happens.

fn takes_ownership(some_string: String) { // some_string comes into scope
    println!("{}", some_string);
} // Here, some_string goes out of scope and `drop` is called. The backing
  // memory is freed.

fn makes_copy(some_integer: i32) { // some_integer comes into scope
    println!("{}", some_integer);
} // Here, some_integer goes out of scope. Nothing special happens.
```

> 上述代码小结：`takes_ownership` 的参数是 `String`, 则默认就是 `move``makes_copy` 的参数是 `i32`，则默认就是 `copy`变量是否释放都是根据是否 `go out scope`

> 现在， 我们碰到一个恼人的问题:即如何才能让一个函数使用一个值， 但是不要发生所有权转移呢？

恼人的做法是这样的：

```
fn gives_ownership() -> String {             // gives_ownership will move its
                                             // return value into the function
                                             // that calls it

    let some_string = String::from("hello"); // some_string comes into scope

    some_string                              // some_string is returned and
                                             // moves out to the calling
                                             // function
}
```

即， 明确的再在函数里返回。

那有更好的方式能够实现让函数使用值的时候不发生所有权转移吗？答案是使用引用 `reference`

### References and Borrowing

引用的方式好处：1、 不用发生所有权转移2、不用显式的返回

例如：

```
fn main() {
    let s1 = String::from("hello");
    let len = calculate_length(&s1);
    println!("The length of '{}' is {}.", s1, len);
}

fn calculate_length(s: &String) -> usize {
    s.len()
}
```

> ```
> calculate_length` 的参数是`&String`, 而不是 `String
> ```

引用的示意图如下：

![img](https://tva1.sinaimg.cn/large/007S8ZIlly1gjj9ifzlihj30hb07cmx4.jpg)



因为是`引用`， 所以没有`所有权`， 也即不会在离开有效范围之后执行 `Drop` 函数.

如下代码详细说明这个规则：

```
fn calculate_length(s: &String) -> usize { // s is a reference to a String
    s.len()
} // Here, s goes out of scope. But because it does not have ownership of what
  // it refers to, nothing happens.
```

引用，也称为借用。而既然是`借用`， 你就没有权利修改之， 好比你借的别人的东西不能破坏别人的东西， 因为你没有`所有权`。

如下代码所示， 编译器会给错报错警告：

```
fn main() {
    let s = String::from("hello");
    change(&s);
}

fn change(some_string: &String) {
    some_string.push_str(", world");
}
```

但是， 尽管默认的引用的默认行为是禁止修改，（这一点和变量的默认类型是 `immutable` 是一致的）， 如果你想没有所有权也可以修改可以吗？答案是可以， 只要声明为`可修改引用` (`mutable reference`)即可.

#### Mutable References 可变引用

如下代码所以：

```
fn main() {
    let mut s = String::from("hello");
    change(&mut s);
}

fn change(some_string: &mut String) {
    some_string.push_str(", world");
}
```

但是可变引用是有限制的， 即一次只能有一个可变借用， 不能多个。这一点和我们之前强调的原则是一致的：

> 核心原则：共享不可变， 可变不共享

如下代码编译器会报错：

```
let mut s = String::from("hello");

let r1 = &mut s;
let r2 = &mut s;

println!("{}, {}", r1, r2);
```

> 理解：可变可以， 但是要可控，要节制.而且这么做可以很大程度避免 `data race` 的问题.

补充： `data race` 是如何发生的？1）、2 到 3 个指针同时访问相同的数据2）、一个以上的指针往数据里写入3）、没有同步访问的机制

> 所以， Rust 编译器在编译阶段就阻止 `data race`, 编译器再一次挽救了我们

上述代码改为串行就没有问题了:

```
let mut s = String::from("hello");
{
    let r1 = &mut s;
} // r1 goes out of scope here, so we can make a new reference with no problems.
let r2 = &mut s;
```

另一个需要注意的问题是， 不能同时有不可变引用和可变应用， 理由很简单， 会引起 `dirty read`, 并且也不符合我们之前提的一个原则：

> 核心原则：共享不可变， 可变不共享

错误代码示例如下:

```
let mut s = String::from("hello");

let r1 = &s; // no problem
let r2 = &s; // no problem
let r3 = &mut s; // BIG PROBLEM

println!("{}, {}, and {}", r1, r2, r3);
```

> 上述代码 `r1` 和 `r2` 是没有问题的， 因为都是不可变， 不存在 `dirty read` 问题.

但是， 转折一下， 上述代码之所以会出错， 核心原因是因为后来还要继续使用 `r1` 和 `r2` 变量， 如果不再使用实际是没有问题的. 是不是有点意外？

看一下上述代码改变下就没有问题了:

```
let mut s = String::from("hello");

let r1 = &s; // no problem
let r2 = &s; // no problem
println!("{} and {}", r1, r2);
// r1 and r2 are no longer used after this point

let r3 = &mut s; // no problem
println!("{}", r3);
```

所以， 我们把前面的结论再完善一下：是否是 `dirty read` 还取决于是否变量不再访问， 假如不再访问，逻辑上相当于是隔离安全的。

#### Dangling References 悬垂指针

什么是悬垂指针？就是内存实际已经释放了， 但是还保留引用。而 Rust 编译器会阻止你这么做。比如以下代码编译器会报错：

```
fn main() {
    let reference_to_nothing = dangle();
}

fn dangle() -> &String {
    let s = String::from("hello");

    &s
}
```

编译器报错的信息是 `missing lifetime specifier`, 关于生命周期后面会单独列一个段落， 暂且不表。看一下上述代码到底发生了什么：

```
fn dangle() -> &String { // dangle returns a reference to a String
    let s = String::from("hello"); // s is a new String
    &s // we return a reference to the String, s
} // Here, s goes out of scope, and is dropped. Its memory goes away.
  // Danger!
```

> 意思就是：说好的送给人家一件东西，结果还没出门你就销毁了。给别人的承诺变成了空气币。这是很危险的。

解决办法：函数转移所有权， 这样就没有东西会被释放销毁:

```
fn no_dangle() -> String {
    let s = String::from("hello");
    s
}
```

总结一下引用的规则：

- 任何时候， 你要么只有一个可变引用， 要么就是任意数量的不可变引用
- 引用必须重视有效合法的（不合法就报错）

当然， 还是记得一句话规则就是：

> 核心原则：共享不可变， 可变不共享

### The Slice Type

为什么要提到`切片类型`？因为切片类型`没有所有权`, 算是很特别， 所以要了解下怎么个特别法。

首先什么是切片？简单说就是**引用**一个集合里的部分连续元素， 而不是所有。所以， 集合，天然就是`引用`。

代码：

```
let s = String::from("hello world");
let hello = &s[0..5];
let world = &s[6..11];
```

示意图：

![img](data:image/gif;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWNgYGBgAAAABQABh6FO1AAAAABJRU5ErkJggg==)



应用Example：从一个字符串里返回第一个单词：

```
fn first_word(s: &String) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }
    &s[..]
}
```

#### String Slice as parameters 用 String 切片作为参数

对上述 `first_word` 函数有一个优化：原来的函数签名是这样的：

```
fn first_word(s: &String) -> &str {
```

而一个有经验的 Rust 程序员会将参数改为 `&str`:

```
fn first_word(s: &str) -> &str {
```

为什么呢？因为这样既可以传入`&String` 也可以传入 `&str`, 详细说明如下：

```
fn main() {
    let my_string = String::from("hello world");

    // first_word works on slices of `String`s
    let word = first_word(&my_string[..]);
    let my_string_literal = "hello world";

    // first_word works on slices of string literals
    let word = first_word(&my_string_literal[..]);

    // Because string literals *are* string slices already,
    // this works too, without the slice syntax!
    let word = first_word(my_string_literal);
}
```

> 看起来有点绕口， 最后还是要理解 `String` 和 `str` 的关系, Rust 的字符串有点变态的感觉.

> 小结：如何获得一个 slice 切片？ `&引用`， 加上 `range`。

## Rust Book: Validating References with Lifetimes

lifetime, 即生命周期， 和类型是一致的， 默认都是隐式推断的， 什么时候需要显示指定呢？就是当编译器搞不清楚的时候， 提醒编译器一下。

### Preventing Dangling References with Lifetimes

lifetime 的主要目的是防止悬垂指针， 机制就是前面已经提了很多次的`超过范围就销毁`的机制， 比如如下代码：

```
{
    let r;
    {
        let x = 5;
        r = &x;
    }
    println!("r: {}", r);
}
```

上述代码 Rust 编译器会报错， 说`x does not live long enough`

那么， Rust 编译器是如何聪明的发现代码非法的呢？答案是 `borrow checker` (借用检查器）

### The borrow checker (借用检查器)

Rust 编译器有一个借用检查器来比较不同变量生命周期的范围， 来确定是否合法。前面报错的代码用注解的方式说明如下：

```
{
    let r;                // ---------+-- 'a
                          //          |
    {                     //          |
        let x = 5;        // -+-- 'b  |
        r = &x;           //  |       |
    }                     // -+       |
                          //          |
    println!("r: {}", r); //          |
}     
```

> 说明：1、上述的变量 `r` 和 `x` 的生命周期分别用`'a` 和`'b` 来标注2、判断准则：短的可以引用长的， 长的不可以引用短的3、上述代码示例 `'b` 比 `'a` 短， 所以 `'a` 不能引用`'b`

修正后的正确代码如下：

```
{
    let x = 5;            // ----------+-- 'b
                          //           |
    let r = &x;           // --+-- 'a  |
                          //   |       |
    println!("r: {}", r); //   |       |
                          // --+       |
}                         // ----------+
```

> 理解：出借方必须活得足够久， 不能`跑路`， 这和现实世界的规则是一致的.

### Generic Lifetimes in Functions：函数里的生命周期

假设我们要实现一个功能， 实现从两个字符串中返回更长的那个， 代码如下：

```
fn main() {
    let string1 = String::from("abcd");
    let string2 = "xyz";

    let result = longest(string1.as_str(), string2);
    println!("The longest string is {}", result);
}
```

`longest` 函数的参数是`&str`， 好处是既可以接受 String 切片，也可以接受字符串`字面量`:

```
fn longest(x: &str, y: &str) -> &str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}
```

编译器报错如下：

```
error[E0106]: missing lifetime specifier
 --> src/main.rs:1:33
  |
1 | fn longest(x: &str, y: &str) -> &str {
  |                                 ^ expected lifetime parameter
  |
  = help: this function's return type contains a borrowed value, but the
signature does not say whether it is borrowed from `x` or `y`
```

报错提示：返回类型包含借用值， 但是函数签名并没有明确到底是从 `x` 借的还是从 `y` 借的 ， 问题是编译器不知道， 我们自己也不知道， 因为 if else 语句两种情况都有可能， 我们现在也不能明确到底是哪个。总之， 借用检查器不能确定， 只好报错。

那怎么办呢？方法是增加泛型生命周期标注 `generic lifetime parameters`

### Lifetime Annotation Syntax

生命周期标注不改变引用的存活期， 它的`作用`主要是:在不影响生命周期的前提下，`描述多个引用的生命周期关系`.代码示例如下：

```
&i32        // a reference
&'a i32     // a reference with an explicit lifetime
&'a mut i32 // a mutable reference with an explicit lifetime
```

要注意， 既然是描述的`关系`， 那么单个的生命周期是没有意义的， 因为没有关系的说法。比如， 如下代码就明确说明， 第一个参数 `first` 和第二个参数 `second` 的生命周期是一样的:

```
fn example(first: &'a i32, second: &'b i32)
```

### 补充: lifetime 和 traits 用法对比

和泛型的使用类似， 冒号`:`的使用上有一点不同， 但是加号`+` 上使用是一样的:1、T: 'a: All references in T must outlive lifetime 'a. （类型`T` 中的所有引用必需比`'a`生命周期长)2、T: Trait + 'a: Type T must implement trait Trait and all references in T must outlive 'a. (类型 `T` 必须实现 `Trait` 这个 trait， 并且类型 `T` 中的所有引用必需比`'a` 生命周期长)

### Lifetime Annotations in Function Signatures 函数签名里的生命周期标注

看下 `longest` 函数签名， 我们在函数名和参数列表之间的尖括号里声明范式生命周期参数。

```
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}
```

这个签名想表达的意思是：参数和返回值对应的引用有**相同的生命周期**， 我们大家都是`'a` 标记

具体的说， 返回值的生命周期和两个参数中的较小的一个是相同的。

> 返回值引用参数中的一个， 所以和较小的那个参数相同就保证了所有的都合法

来测试下， 下属代码是可以正确编译的：

```
fn main() {
    let string1 = String::from("long string is long");
    {
        let string2 = String::from("xyz");
        let result = longest(string1.as_str(), string2.as_str());
        println!("The longest string is {}", result);
    }
}
```

> 理解：这里 string1 和 string2 中较小生命周期的是 string2， result 和 string2 是相同的， 所以编译没有问题。

再来一个不符合这个要求的例子:

```
fn main() {
    let string1 = String::from("long string is long");
    let result;
    {
        let string2 = String::from("xyz");
        result = longest(string1.as_str(), string2.as_str());
    }
    println!("The longest string is {}", result);
}
```

编译器报错：

```
`string2` does not live long enough
```

> 理解：string1 和 string2 中较小的生命周期是 string2， 返回值 result 和 string1 相同， 不和较小的 string2 相同， 所以不符合要求。另外， 还是前面说的话， 人眼能在这里识别最终结果是 `string1`, 并且 `string1` 和 `result` 有相同的生命周期， 但是 Rust 编译器不能识别这个， 我们已经明确告诉 Rust 返回值的生命周期与参数中的两个中的较小的是相同的。 `borrow chekcer` 根据这个规则去检查， 报错.

### Thinking in terms of lifetimes 从生命周期角度考虑

生命周期的指定方式取决于函数的具体行为， 加入 `longest` 函数是永远返回第一个参数 `x`， 那么 `y` 就可以不用指定生命周期:

```
fn longest<'a>(x: &'a str, y: &str) -> &'a str {
    x
}
```

规则：当从函数中返回一个引用时， 返回类型的生命周期参数需要匹配参数列表里的参数之一的生命周期参数.

如果没有引用参数， 那它肯定是引用了函数内的一个值， 这将导致悬垂指针.

如下面代码所示：

```
fn longest<'a>(x: &str, y: &str) -> &'a str {
    let result = String::from("really long string");
    result.as_str()
}
```

> 这里虽然返回值指定的`'a` 和参数 `x`, `y` 的生命周期一样， 但是实际并不相关。没有办法通过指定声明周期来避免悬垂指针。这种情况最佳的修复方式就是返回一个有所有权的类型的值而不是引用， 让调用者负责清理内存.

> 结论：生命周期参数是函数的参数和函数的返回值的`桥梁`， 一旦他们连接上了， Rust 就有足够的信息来进行`内存安全`的操作， 并根据此链接来拒绝`悬垂指针`或者其他违背内存安全的操作.

### Lifetime Annotations in Struct Definitions 在 Struct 定义里做生命周期标注

在 Struct 里我们大多是持有有所有权的类型， 但实际上也是可以持有引用的， 前提是我们需要增加生命周期标注.

```
struct ImportantExcerpt<'a> {
    part: &'a str,
}

fn main() {
    let novel = String::from("Call me Ishmael. Some years ago...");
    let first_sentence = novel.split('.')
        .next()
        .expect("Could not find a '.'");
    let i = ImportantExcerpt { part: first_sentence };
}
```

> 理解：`struct ImportantExcerpt<'a>` 表示： `ImportantExcerpt` 的实例的生命周期不会长于它持有的字段引用， 即 `part` 字段.

### 生命周期省略

我们已经知道函数或者结构体如果使用了引用， 需要声明生命周期， 但是下面这个例子为何没有指定生命周期也没有报错呢？

```
fn first_word(s: &str) -> &str {
    let bytes = s.as_bytes();
    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }
    &s[..]
}
```

这其实是一个历史原因， 在 Rust 早期版本里写上述代码的签名是这样的：

```
fn first_word<'a>(s: &'a str) -> &'a str {
```

后来开发团队觉得会有大量的这类生命周期标注， 尤其在特定的场景下会有一定的规律， 所以开发团队决定将一些这些可以捕捉的规律放入编译器代码里， 这样借用检查器 `borrow checker`就可以在这些情景里自动推断出生命周期.

这些总结出来的规则就是 `lifetime elision rules` (生命周期省略规则）。

生命周期细分：1、input lifetimes (函数或者方法参数）2、output lifetimes （返回值）

具体来说， 当没有显式指定生命周期的时候，编译器使用三条规则来推断：使用第一条规则应用到 `input lifetimes`; 使用第二条、第三条规则应用到 `output lifetimes`。当使用完三条规则后编译器如果还没有判断出生命周期， 编译器就直接报错。这些规则应用到 `fn` 定义以及 `impl` 块。

那么这三条规则具体是什么呢？第一条规则：作为引用的每个参数都有自己的生命周期参数， 换句话说， 有一个参数的函数有一个生命周期参数：`fn foo<'a>(x: &'a i32);`， 以此类推两个参数的获得两个独立的生命周期参数： `fn foo<'a, 'b>(x: &'a i32, y: &'b i32);`第二条规则：如果只有一个指定的 `input lifetime` 参数， 那么将这个生命周期引用到所有的 `output lifetime` 参数 `fn foo<'a>(x: &'a i32) -> &'a i32`第三条规则：如果在众多的 `input lifetime` 参数中有一个是`&self`或者 `*mut self`, 则 self 的生命周期会引用到所有的 `output lifetime` 参数.

让我们从编译器的角度使用上述三条规则， 现在引用没有标注生命周期的函数签名如下：

```
fn first_word(s: &str) -> &str {
```

应用第一条规则， 即每个参数都有一个独立的自己的生命周期， 得到：

```
fn first_word<'a>(s: &'a str) -> &str {
```

> 即为参数 `s` 增加`'a` 标记

应用第二条规则：如果只有一个 input lifetime， 那么 output lifetime 和其一样， 得到:

```
fn first_word<'a>(s: &'a str) -> &'a str {
```

现在， `first_world` 函数中的所有引用都有了生命周期标注， 编译器可以在没有程序员明确标注生命周期的情况下继续分析。

再来看另一个例子， 即之前提到的 `longest` 函数例子:

```
fn longest(x: &str, y: &str) -> &str {
```

应用第一条规则：每个参数都有自己独立的生命周期：

```
fn longest<'a, 'b>(x: &'a str, y: &'b str) -> &str {
```

**第二条规则不能使用**， 因为 `input lifetime` 超过了 1 个.**第三条规则也不能使用**， 因为没有 `self` 参数。

三条规则都应用完之后， 还没有完成所有返回类型的 `lifetime`， 所以编译器只能报错。

### Lifetime Annotations in Method Definitions 方法定义里的生命周期标注

```
impl<'a> ImportantExcerpt<'a> {
    fn announce_and_return_part(&self, announcement: &str) -> &str {
        println!("Attention please: {}", announcement);
        self.part
    }
}
```

根据第一条规则：两个参数都分配 lifetime根据第三条规则：因为 `input lifetime` 里有一个是`&self`类型， 所以 `output lifetime` 也是`&self` 类型至此， 全部生命周期完成推断。

### The Static Lifetime

有一个特别的生命周期是 `'static`, 其含义是该引用会在程序运行的整个期间存活. 所有的 `string literals`(字符串字面量)都有 `'static` 生命周期。

```
let s: &'static str = "I have a static lifetime.";
```

> 注释：The text of this string is stored directly in the program’s binary, which is always available. Therefore, the lifetime of all string literals is `'static`.字符串的文本直接存储为二进制， 总是可用， 所以字符串字面量的生命周期是 `'static` 的

### 在一个函数里集成泛型、trait bounds 以及生命周期

```
use std::fmt::Display;
fn longest_with_an_announcement<'a, T>(x: &'a str, y: &'a str, ann: T) -> &'a str
    where T: Display
{
    println!("Announcement! {}", ann);
    if x.len() > y.len() {
        x
    } else {
        y
    }
}
```

至此， 所有权以及生命周期以及引用相关的问题基本差不多了.

简单做一下总结吧:

## Rust 所有权小结

- 核心原则：共享不可变， 可变不共享
- 理解堆、栈数据结构是理解 Rust 内存管理的基础， 简单说栈的数据是连续的， 快速的， 堆上的数据是动态的并且是昂贵的， 而且堆上的数据还需要在栈上有对应的指针， 所以每次读取相当于有跳转。所有权就是要解决堆上的数据在必要的时候清理掉， 保障内存安全
- 栈上的数据可以便宜的 copy， 堆上的数据要想实现类似栈上的拷贝， 即所谓深拷贝， 需要调用 `Clone` 函数
- 基本数据类型默认实现了 Copy， 其他数据类型默认都是 Move
- move 的意思就是所有权发生了转移， 默认对应的变量在外部不能再使用， 除非在函数内部显式的返回
- 如果想使用数据还不发生所有权转移， 可以使用引用
- 引用又分为只读引用和可写引用
- 重复核心原则：共享不可变， 可变不共享
- Rust 根据作用域范围自动来判断是否释放内存， 背后执行的实际是 `Drop` 函数
- 为什么要指定生命周期？因为编译器有的时候会分不清变量之间到底什么关系， 所以需要程序员显式指定
- 部分情况下， 生命周期也是可以省略的， 这种情况编译器将这部分可省略的部分写在编译器代码里， 依赖的是生命周期省略原则
- 生命周期省略三条规则：1、每个参数都会分配一个单独的 lifetime 2、如果只有一个参数， 那么返回值的生命周期和参数的生命周期一样 3、如果众多参数中有一个是&self， 那么返回值的生命周期也是&self



---



变量的值可以被其他变量使用有 4 种方法，需要遵循的规则如下：

- **克隆（clone）**：此处将值复制到新的变量。新变量拥有新的复制值的所有权，而原始变量保留其原始值的所有权。
- **移动（move）：**所有权被转移到另一个要使用该值的变量，原始变量不再拥有所有权。
- **不可变借用（immutable borrow）**。这里没有发生所有权转移，但是可以通过另一个变量访问该值以进行读取。如果借用变量超出范围，内存不会被回收，因为借用变量没有所有权。
- **可变借用（mutable borrow）**。可以通过另一个变量对该值进行读取和写入操作。如果借用变量超出范围，内存也不会回收，因为借用变量没有所有权。



- 使用 clone：

- - 没有什么合适规则防止内存 bug
  - 对于非常规数据结构，通常代价昂贵

- 使用 move：

- - 将所有权从一个变量中移出后，该变量将无法再访问其最初拥有的值。

- 使用不可变借用 (immutable borrow)： 

- - 可以创建无限的不可变借用
  - 所有不可变借用只能读
  - 原始拥有变量在修改其拥有的值方面存在限制，只有不存在不可变借用，它才可以修改。这样可以确保 Rust 保证对不可变借用的担保不会改变。
  - 基本上：**许多读操作，没有写操作（条件是一直有读操作，否则就可以写）** 

- 使用可变借用（mutable borrow）：

- - 只能使用一次可变借用。
  - 所有的读写操作都只能通过活动的可变借用（active borrow）完成。
  - 只要有活动的可变借用，原始拥有变量也将无法再读取或写入。
  - 基本上：如果只有一个读和写：**使用****可变借用**