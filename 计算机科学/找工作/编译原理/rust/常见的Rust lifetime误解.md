[TOC]

原文：[https://github.com/pretzelhammer/rust-blog/blob/master/posts/common-rust-lifetime-misconceptions.md](https://link.zhihu.com/?target=https%3A//github.com/pretzelhammer/rust-blog/blob/master/posts/common-rust-lifetime-misconceptions.md)

译者的话：从我开始学习Rust到现在已经4年了，我已经敢于自称Rust熟练工了，而我自始至终感到困难的概念只有一个：lifetime。一方面是理解上的困难，另一方面是实际使用时，如果需要标注lifetime，或者编译器报了lifetime相关的错，总会让我十分烦燥。我相信有这样体验的人并不少。这篇文章是我读过的最好的lifetime释疑的文章，其中的大部分内容我其实已经有了模糊的理解，但并不都能清晰地表述出来；但对于哪怕是1年前的我，如果能读到这篇文章，将会使我少走很多弯路。对于Rust初学者，毫不夸张地说，你可以不读官方的book另外选本书入门，而这篇文章必读；对于理解了lifetime的老手，这篇文章也是非常好的reference。更推荐大家去读原文，翻译它主要是为了加深自己的理解。

## 引言

我曾经有过所有这些误解，而我看到现在很多初学者仍然被它们困扰。我的某些术语可能不是标准说法，下面的表格是我将使用的缩略语和对应的含义。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gtgosk6qqxj60if05dwez02.jpg)

## 误解

一言以蔽之：一个变量的lifetime是编译器能静态地确定它指向的数据在它当前的地址有效的区间。我将会使用接下来的6500字解释人们常常困惑的细节。

### 1) T只包括独立类型

这个误解更多是关于泛型而不是lifetime的，但是Rust里的泛型和lifetime是紧紧纠缠在一起的，不可能只讨论其中一个而不涉及到另一个。总之：

当我最初学习Rust时，我理解i32、&i32和&mut i32是不同的类型。我也理解泛型类型参数T代表了一个包括所有类型的集合。然而，虽然我分别理解这两者，我并不能把它们合起来。在我初学者的Rust思维里，我是这么理解泛型的行为的：

![img](https://pic3.zhimg.com/80/v2-5ac3eabaa491c826255301f160a06286_1440w.png)

T包括所有独立类型。&T包括所有不可变借用类型。&mut T包括所有可变借用类型。T、&T、&mut T是不相交的有限集。美妙，简单，清晰，容易，符合直觉，并且完全错误。这才是Rust里泛型真正的行为：

![img](https://pic4.zhimg.com/80/v2-b4595e58b824a7315daf1886972afdbb_1440w.png)

T、&T和&mut T都是无限集，因为可以无限次地借用一个类型。T是&T和&mut T的超集。&T和&mut T是不相交的集合。下面是几个验证这些概念的例子：

```rust
trait Trait {}

impl<T> Trait for T {}

impl<T> Trait for &T {} // compile error

impl<T> Trait for &mut T {} // compile error
```

上面的程序编译失败：

```text
error[E0119]: conflicting implementations of trait `Trait` for type `&_`:
 --> src/lib.rs:5:1
  |
3 | impl<T> Trait for T {}
  | ------------------- first implementation here
4 |
5 | impl<T> Trait for &T {}
  | ^^^^^^^^^^^^^^^^^^^^ conflicting implementation for `&_`

error[E0119]: conflicting implementations of trait `Trait` for type `&mut _`:
 --> src/lib.rs:7:1
  |
3 | impl<T> Trait for T {}
  | ------------------- first implementation here
...
7 | impl<T> Trait for &mut T {}
  | ^^^^^^^^^^^^^^^^^^^^^^^^ conflicting implementation for `&mut _`
```

编译器并不允许我们为&T和&mut T实现Trait，因为这会和T的Trait实现相冲突，它就已经包括了所有&T和&mut T。而如下的程序编译成功，因为&T和&mut T是不相交的：

```text
trait Trait {}

impl<T> Trait for &T {} // compiles

impl<T> Trait for &mut T {} // compiles
```

**要点：**

- T是&T和&mut T的超集
- &T和&mut T是不相交的集合

### 2) 如果T: 'static，那么T必须在整个程序运行时都有效

**误解的推论**

- T: 'static应该读做“T有'static lifetime”
- &'static T和T: 'static是同一个东西
- 如果T: 'static那么T必须是不可变的
- 如果T: 'static那么T只能在编译时创建

大部分Rust初学者第一次接触到'static lifetime，都是在像这样的代码里：

```text
fn main() {
    let str_literal: &'static str = "str literal";
}
```

他们学到，"str literal"是被硬编码在编译出的二进制文件里的，在运行时被加载到只读内存中。因此，它是不可变的，并且在整个程序运行时有效，因此它是'static的。这些概念在后续使用static关键词定义静态变量时被进一步强化了。

```text
static BYTES: [u8; 3] = [1, 2, 3];
static mut MUT_BYTES: [u8; 3] = [1, 2, 3];

fn main() {
   MUT_BYTES[0] = 99; // compile error, mutating static is unsafe

    unsafe {
        MUT_BYTES[0] = 99;
        assert_eq!(99, MUT_BYTES[0]);
    }
}
```

关于静态变量

- 它们只能在编译时创建
- 它们应该是不可变的，改变它们是unsafe的
- 它们在整个程序运行时都有效

'static lifetime大概是得名于静态变量的默认lifetime，对吧？所以'static lifetime也要遵循所有同样的规则，对吧？

并不是不对，但是一个有'static lifetime的类型，和一个被'static lifetime约束的类型是不同的。后者能在运行时创建，能被安全和随意地改变，能被drop，能活任意时长。

区分&'static T和T: 'static是很重要的。

&'static T是一个指向某个T的不可变引用，它能够被安全地持有无限久，直到整个程序终止。仅当T本身不可变，且在引用创建后就不再move才能满足这些条件。T不需要在编译时创建。在运行时生成动态分配的随机数据，然后返回一个'static引用是可行的，只需要付出内存泄漏的代价，例如

```rust
use rand;

// generate random 'static str refs at run-time
fn rand_str_generator() -> &'static str {
    let rand_string = rand::random::<u64>().to_string();
    Box::leak(rand_string.into_boxed_str())
}
```

T: 'static是一个能够被安全地持有无限久的T，直到整个程序终止。T: 'static包括了所有的&'static T，以及所有的独立类型，例如String、Vec等等。数据的拥有者得到保证，只要它还持有数据，数据就不会失效。因此拥有者可以无限久地持有数据，直到整个程序终止。T: 'static应该读作“T被'static lifetime约束”，而不是“T有'static lifetime”。一段阐明这些概念的程序：

```text
use rand;

fn drop_static<T: 'static>(t: T) {
    std::mem::drop(t);
}

fn main() {
    let mut strings: Vec<String> = Vec::new();
    for _ in 0..10 {
        if rand::random() {
            // all the strings are randomly generated
            // and dynamically allocated at run-time
            let string = rand::random::<u64>().to_string();
            strings.push(string);
        }
    }

    // strings are owned types so they're bounded by 'static
    for mut string in strings {
        // all the strings are mutable
        string.push_str("a mutation");
        // all the strings are droppable
        drop_static(string); // compiles
    }

    // all the strings have been invalidated before the end of the program
    println!("i am the end of the program");
}
```

**要点：**

- T: 'static应该读作“T被'static lifetime约束”

- 如果T: 'static，那么T可以是一个有'static lifetime的借用类型，或者是一个独立类型

- T: 'static包括了所有独立类型，也就意味着T

- - 可以在运行时动态创建
  - 不需要在整个程序运行时有效
  - 可以被安全和随意地改变
  - 可以在运行时动态地drop
  - 可以拥有任意的lifetime

### 3) &'a T和T: 'a是同一个东西

这个误解是上一个的泛化版本。

&'a T要求也意味着T: 'a，因为如果T本身不在整个'a有效，T的的引用不可能在整个'a都有效。例如，Rust编译器不会允许构造类型&'static Ref<'a, T>，因为如果Ref仅在'a有效，我们不可能构造一个它的'static引用。

T: 'a包括了所有&'a T，但反过来是不成立的。

```text
// only takes ref types bounded by 'a
fn t_ref<'a, T: 'a>(t: &'a T) {}

// takes any types bounded by 'a
fn t_bound<'a, T: 'a>(t: T) {}

// owned type which contains a reference
struct Ref<'a, T: 'a>(&'a T);

fn main() {
    let string = String::from("string");

    t_bound(&string); // compiles
    t_bound(Ref(&string)); // compiles
    t_bound(&Ref(&string)); // compiles

    t_ref(&string); // compiles
    t_ref(Ref(&string)); // compile error, expected ref, found struct
    t_ref(&Ref(&string)); // compiles

    // string var is bounded by 'static which is bounded by 'a
    t_bound(string); // compiles
}
```

**要点：**

- T: 'a比&'a T更通用和灵活
- T: 'a接受独立类型，含有引用的独立类型，以及引用
- &'a T只接受引用
- 如果T: 'static，那么T: 'a，因为任意'a均满足'static >= 'a

（译注：我理解就lifetime而言不可变引用&'a T其实是和Ref<'a, T>同等的地位，甚至可以说前者是后者的一种。只不过前者非常常见，而后者绝大部分情况下追根溯源来自前者）

### 4)我的代码不是泛型化的，也没有lifetime

**误解的推论**

- 可以避免使用泛型和lifetime

这个令人安心的误解来源于Rust的lifetime省略规则，即borrow checker会根据如下规则推导函数的lifetime标注，从而免去你手写的必要：

- 每一个函数的输入引用会被分配一个单独的lifetime
- 如果只有一个输入lifetime，它被应用给所有输出引用
- 如果有多个输入lifetime，但其中一个是&self或者&mut self，那么self的lifetime被应用给所有输出引用
- 否则，输出lifetime必须显式标注

这么多的规则，所以让我们看一些例子：

```text
// elided
fn print(s: &str);

// expanded
fn print<'a>(s: &'a str);

// elided
fn trim(s: &str) -> &str;

// expanded
fn trim<'a>(s: &'a str) -> &'a str;

// illegal, can't determine output lifetime, no inputs
fn get_str() -> &str;

// explicit options include
fn get_str<'a>() -> &'a str; // generic version
fn get_str() -> &'static str; // 'static version

// illegal, can't determine output lifetime, multiple inputs
fn overlap(s: &str, t: &str) -> &str;

// explicit (but still partially elided) options include
fn overlap<'a>(s: &'a str, t: &str) -> &'a str; // output can't outlive s
fn overlap<'a>(s: &str, t: &'a str) -> &'a str; // output can't outlive t
fn overlap<'a>(s: &'a str, t: &'a str) -> &'a str; // output can't outlive s & t
fn overlap(s: &str, t: &str) -> &'static str; // output can outlive s & t
fn overlap<'a>(s: &str, t: &str) -> &'a str; // no relationship between input & output lifetimes

// expanded
fn overlap<'a, 'b>(s: &'a str, t: &'b str) -> &'a str;
fn overlap<'a, 'b>(s: &'a str, t: &'b str) -> &'b str;
fn overlap<'a>(s: &'a str, t: &'a str) -> &'a str;
fn overlap<'a, 'b>(s: &'a str, t: &'b str) -> &'static str;
fn overlap<'a, 'b, 'c>(s: &'a str, t: &'b str) -> &'c str;

// elided
fn compare(&self, s: &str) -> &str;

// expanded
fn compare<'a, 'b>(&'a self, &'b str) -> &'a str;
```

只要你写了：

- 结构体方法（译注：这里应该只是指&self和&mut self方法）
- 以引用为参数的函数
- 返回引用的函数
- 泛型函数
- trait object（后面详述）
- 闭包（后面详述）

那么你的代码就充满了泛型省略lifetime标注。

**要点**

- 几乎所有Rust代码都是泛型代码，到处都是省略的lifetime标注

### 5) 如果通过了编译，那么我的lifetime标注就是正确的

**误解的推论**

- Rust的函数的lifetime省略规则总是正确的
- Rust的borrow checke总是正确的，无论是技术上还是语义上（译注：技术上是指内存安全，参见后面；语义上是指程序的意图，例如函数返回的引用是来自于哪一个输入参数）
- Rust比我更懂我的程序的语义

当一个Rust程序技术上可通过编译，它在语义上仍然可能是错的。例如：

```text
struct ByteIter<'a> {
    remainder: &'a [u8]
}

impl<'a> ByteIter<'a> {
    fn next(&mut self) -> Option<&u8> {
        if self.remainder.is_empty() {
            None
        } else {
            let byte = &self.remainder[0];
            self.remainder = &self.remainder[1..];
            Some(byte)
        }
    }
}

fn main() {
    let mut bytes = ByteIter { remainder: b"1" };
    assert_eq!(Some(&b'1'), bytes.next());
    assert_eq!(None, bytes.next());
}
```

ByteIter是一个在byte的切片上迭代的迭代器。为了简洁，这里并没有实现Iterator trait。它看起来正常工作，但是如果我们想同时检查多个byte呢？

```text
fn main() {
    let mut bytes = ByteIter { remainder: b"1123" };
    let byte_1 = bytes.next();
    let byte_2 = bytes.next();
    if byte_1 == byte_2 {
        // do something
    }
}
```

噢编译错误：

```text
error[E0499]: cannot borrow `bytes` as mutable more than once at a time
  --> src/main.rs:20:18
   |
19 |     let byte_1 = bytes.next();
   |                  ----- first mutable borrow occurs here
20 |     let byte_2 = bytes.next();
   |                  ^^^^^ second mutable borrow occurs here
21 |     if byte_1 == byte_2 {
   |        ------ first borrow later used here
```

我想我们可以改成copy每个byte。当我们在处理byte的时候，这样做是没问题的。但是如果我们把ByteIter扩展为一个能够迭代任意的&'a [T]的通用切片迭代器时，我们将来可能会想要用它处理copy/clone会很耗费甚至不可能的类型。好吧，看来我们无计可施了，代码通过编译了，所以lifetime标注肯定是正确的，对吧？

并不，现在的lifetime标注正是bug的原因！错误的lifetime标注被省略了，所以想看出这一点并不容易。让我们补上被省略的lifetime，以获得更清晰的视野：

```text
struct ByteIter<'a> {
    remainder: &'a [u8]
}

impl<'a> ByteIter<'a> {
    fn next<'b>(&'b mut self) -> Option<&'b u8> {
        if self.remainder.is_empty() {
            None
        } else {
            let byte = &self.remainder[0];
            self.remainder = &self.remainder[1..];
            Some(byte)
        }
    }
}
```

然而这并没有什么用。我还是没看懂。这里有一条只有Rust专家才知道的小诀窍：给lifetime一个有意义的名字。让我们再试试：

```text
struct ByteIter<'remainder> {
    remainder: &'remainder [u8]
}

impl<'remainder> ByteIter<'remainder> {
    fn next<'mut_self>(&'mut_self mut self) -> Option<&'mut_self u8> {
        if self.remainder.is_empty() {
            None
        } else {
            let byte = &self.remainder[0];
            self.remainder = &self.remainder[1..];
            Some(byte)
        }
    }
}
```

返回的byte被标注为'mut_self，但显然它们是来自于'remainder！试试修复它。

```text
struct ByteIter<'remainder> {
    remainder: &'remainder [u8]
}

impl<'remainder> ByteIter<'remainder> {
    fn next(&mut self) -> Option<&'remainder u8> {
        if self.remainder.is_empty() {
            None
        } else {
            let byte = &self.remainder[0];
            self.remainder = &self.remainder[1..];
            Some(byte)
        }
    }
}

fn main() {
    let mut bytes = ByteIter { remainder: b"1123" };
    let byte_1 = bytes.next();
    let byte_2 = bytes.next();
    std::mem::drop(bytes); // we can even drop the iterator now!
    if byte_1 == byte_2 { // compiles
        // do something
    }
}
```

现在我们再回头去看看我们程序的上一个版本，它显然是错的。那为什么编译通过了？答案很简单：它是内存安全的。

对于lifetime标注，Rust的borrow checker只在乎能否用它静态地验证程序的内存安全。即使lifetime标注有语义上的错误，Rust也会愉快地成功编译，而后果就是程序变得不必要的严格。

下面是一个上面例子的反面：Rust的lifetime省略规则碰巧在语义上是正确的，但是我们无意识地用不必要的lifetime标注加上了过于严格的限制。

```text
#[derive(Debug)]
struct NumRef<'a>(&'a i32);

impl<'a> NumRef<'a> {
    // my struct is generic over 'a so that means I need to annotate
    // my self parameters with 'a too, right? (answer: no, not right)
    fn some_method(&'a mut self) {}
}

fn main() {
    let mut num_ref = NumRef(&5);
    num_ref.some_method(); // mutably borrows num_ref for the rest of its lifetime
    num_ref.some_method(); // compile error
    println!("{:?}", num_ref); // also compile error
}
```

当我们有一个关于'a泛型的结构体时，我们几乎不会需要写带有&'a mut self的方法。它的含义是“在这个结构体的整个lifetime，这个方法会可变借用它”。结果就是Rust的borrow checker只会允许最多一次对此方法的调用，然后结构体就被永久地可变借用了，无法用于其它用途。需要这样做的情形非常少，但对于一个懵懂的初学者，他很容易这样写，而且编译还通过了。修复方法是不要加上不必要的显式lifetime标注，让Rust的lifetime省略规则处理它：

```text
#[derive(Debug)]
struct NumRef<'a>(&'a i32);

impl<'a> NumRef<'a> {
    // no more 'a on mut self
    fn some_method(&mut self) {}

    // above line desugars to
    fn some_method_desugared<'b>(&'b mut self){}
}

fn main() {
    let mut num_ref = NumRef(&5);
    num_ref.some_method();
    num_ref.some_method(); // compiles
    println!("{:?}", num_ref); // compiles
}
```

**要点**

- Rust的函数的lifetime省略规则并不是所有情形都总是正确的
- Rust并不比你更懂你程序的语义
- 给你的lifetime标注有意义的名字
- 当添加显式lifetime标注时，想清楚加在哪和为什么

### 6) boxed trait object没有lifetime

前面我们讨论了Rust的函数的lifetime省略规则。对于trait object，也有省略规则：

- 如果一个trait object被用于泛型类型的类型参数，则它的lifetime约束由容器类型推导（译注：容器就是指泛型类型例如Box<T>、&T）

- - 如果容器类型只有一个lifetime约束，就用它
  - 如果多于一个，必须显式指定一个lifetime约束

- 如果上面并不成立

- - 如果trait只有一个lifetime约束，就用它
  - 如果任意一个lifetime约束使用了'static，则用'static
  - 如果trait没有lifetime约束，那么在表达式里它的lifetime是（根据上下文）推导的，否则是'static

这些听上去非常复杂，但简单总结就是“trait object的lifetime约束是从上下文推导的”。通过下面这些例子，我们会看到lifetime约束推导非常符合直觉，所以我们并不需要记住正式的规则：

```text
use std::cell::Ref;

trait Trait {}

// elided
type T1 = Box<dyn Trait>;
// expanded, Box<T> has no lifetime bound on T, so inferred as 'static
type T2 = Box<dyn Trait + 'static>;

// elided
impl dyn Trait {}
// expanded
impl dyn Trait + 'static {}

// elided
type T3<'a> = &'a dyn Trait;
// expanded, &'a T requires T: 'a, so inferred as 'a
type T4<'a> = &'a (dyn Trait + 'a);

// elided
type T5<'a> = Ref<'a, dyn Trait>;
// expanded, Ref<'a, T> requires T: 'a, so inferred as 'a
type T6<'a> = Ref<'a, dyn Trait + 'a>;

trait GenericTrait<'a>: 'a {}

// elided
type T7<'a> = Box<dyn GenericTrait<'a>>;
// expanded
type T8<'a> = Box<dyn GenericTrait<'a> + 'a>;

// elided
impl<'a> dyn GenericTrait<'a> {}
// expanded
impl<'a> dyn GenericTrait<'a> + 'a {}
```

实现了trait的具体类型可以含有引用，因此它们也有lifetime约束，于是它们对应的trait object也有lifetime约束。另外你可以直接为引用实现trait，而引用显然是有lifetime约束的：

```text
trait Trait {}

struct Struct {}
struct Ref<'a, T>(&'a T);

impl Trait for Struct {}
impl Trait for &Struct {} // impl Trait directly on a ref type
impl<'a, T> Trait for Ref<'a, T> {} // impl Trait on a type containing refs
```

无论如何，这很值得详细解释，因为当初学者把函数由使用trait object重构为使用泛型（或反过来时），常常会感到困惑。例如：

```text
use std::fmt::Display;

fn dynamic_thread_print(t: Box<dyn Display + Send>) {
    std::thread::spawn(move || {
        println!("{}", t);
    }).join();
}

fn static_thread_print<T: Display + Send>(t: T) {
    std::thread::spawn(move || {
        println!("{}", t);
    }).join();
}
```

它抛出如下编译错误：

```text
error[E0310]: the parameter type `T` may not live long enough
  --> src/lib.rs:10:5
   |
9  | fn static_thread_print<T: Display + Send>(t: T) {
   |                        -- help: consider adding an explicit lifetime bound...: `T: 'static +`
10 |     std::thread::spawn(move || {
   |     ^^^^^^^^^^^^^^^^^^
   |
note: ...so that the type `[closure@src/lib.rs:10:24: 12:6 t:T]` will meet its required lifetime bounds
  --> src/lib.rs:10:5
   |
10 |     std::thread::spawn(move || {
   |     ^^^^^^^^^^^^^^^^^^
```

好的很棒，编译器告诉了我们如何修复问题，让我们试一试。

```text
use std::fmt::Display;

fn dynamic_thread_print(t: Box<dyn Display + Send>) {
    std::thread::spawn(move || {
        println!("{}", t);
    }).join();
}

fn static_thread_print<T: Display + Send + 'static>(t: T) {
    std::thread::spawn(move || {
        println!("{}", t);
    }).join();
}
```

现在编译通过了，但这两个函数放一起看起来很奇怪。为什么第二个函数需要对T有'static约束，而第一个函数不需要呢？这是一个微妙的问题。Rust使用lifetime省略规则，自动地对第一个函数推导出了'static约束，所以实际上两个函数都是'static约束。Rust编译器看到的实际是这样：

```text
use std::fmt::Display;

fn dynamic_thread_print(t: Box<dyn Display + Send + 'static>) {
    std::thread::spawn(move || {
        println!("{}", t);
    }).join();
}

fn static_thread_print<T: Display + Send + 'static>(t: T) {
    std::thread::spawn(move || {
        println!("{}", t);
    }).join();
}
```

**要点**

- 所有的trait object都有推导出来的默认lifetime约束

### 7) 编译错误消息会告诉我如何修复我的程序

**误解的推论**

- Rust的trait object的lifetime省略规则总是正确的
- Rust比我更懂我的程序的语义

这个误解，就是前面两个误解合成一个例子：

```text
use std::fmt::Display;

fn box_displayable<T: Display>(t: T) -> Box<dyn Display> {
    Box::new(t)
}
```

抛出了如下错误：

```text
error[E0310]: the parameter type `T` may not live long enough
 --> src/lib.rs:4:5
  |
3 | fn box_displayable<T: Display>(t: T) -> Box<dyn Display> {
  |                    -- help: consider adding an explicit lifetime bound...: `T: 'static +`
4 |     Box::new(t)
  |     ^^^^^^^^^^^
  |
note: ...so that the type `T` will meet its required lifetime bounds
 --> src/lib.rs:4:5
  |
4 |     Box::new(t)
  |     ^^^^^^^^^^^
```

行，让我们按编译器的建议修复这个问题，而不在意它自动地为我们的boxed trait object推导了'static lifetime约束，没有告诉我们，并且它建议的修复是基于这个隐含的事实：

```text
use std::fmt::Display;

fn box_displayable<T: Display + 'static>(t: T) -> Box<dyn Display> {
    Box::new(t)
}
```

现在程序编译通过了；但这真的是我们想要的吗？有可能，但也可能不是。编译器并没有提到任何其它的修复方式，但这也是正确的修复：

```text
use std::fmt::Display;

fn box_displayable<'a, T: Display + 'a>(t: T) -> Box<dyn Display + 'a> {
    Box::new(t)
}
```

这个函数接受所有之前的版本接受的参数，再加上更多！这是不是一个更好的函数？并不一定，这要看我们的程序的需求和限制。这个例子有一点抽象，让我们来看一个更简单和明显的情形：

```text
fn return_first(a: &str, b: &str) -> &str {
    a
}
```

抛出错误：

```text
error[E0106]: missing lifetime specifier
 --> src/lib.rs:1:38
  |
1 | fn return_first(a: &str, b: &str) -> &str {
  |                    ----     ----     ^ expected named lifetime parameter
  |
  = help: this function's return type contains a borrowed value, but the signature does not say whether it is borrowed from `a` or `b`
help: consider introducing a named lifetime parameter
  |
1 | fn return_first<'a>(a: &'a str, b: &'a str) -> &'a str {
  |                ^^^^    ^^^^^^^     ^^^^^^^     ^^^
```

错误消息建议把输入和输出都标成同一个lifetime。如果我们这样做了，我们的程序能够编译，但这个函数会过于限制返回类型。我们真正想要的是这样：

```text
fn return_first<'a>(a: &'a str, b: &str) -> &'a str {
    a
}
```

**要点**

- Rust的trait object的lifetime省略规则并不是所有情形都总是正确的
- Rust并不比你更懂你程序的语义
- Rust编译器的错误消息建议的修复方式，会让你的程序通过编译，但这和能让你的程序通过编译并且最符合你的程序的需求是两回事

（译注：Rust编译器的错误消息确实是独一无二的强大，但认为按其建议进行修改就能通过编译也过于狂热了。初学者写或者从别的语言翻译复杂程序的常见情况是，按编译器的建议改了一圈，最后编译器建议把它之前的建议再改回去，就怒而退坑了）

### 8) lifetime能在运行时伸长缩短

**误解的推论**

- 容器类型能够在运行时交换引用，从而改变它们的lifetime
- Rust的borrow checker进行了高级的控制流分析

如下代码无法编译：

```text
struct Has<'lifetime> {
    lifetime: &'lifetime str,
}

fn main() {
    let long = String::from("long");
    let mut has = Has { lifetime: &long };
    assert_eq!(has.lifetime, "long");

    {
        let short = String::from("short");
        // "switch" to short lifetime
        has.lifetime = &short;
        assert_eq!(has.lifetime, "short");

        // "switch back" to long lifetime (but not really)
        has.lifetime = &long;
        assert_eq!(has.lifetime, "long");
        // `short` dropped here
    }

    // compile error, `short` still "borrowed" after drop
    assert_eq!(has.lifetime, "long");
}
```

错误消息：

```text
error[E0597]: `short` does not live long enough
  --> src/main.rs:11:24
   |
11 |         has.lifetime = &short;
   |                        ^^^^^^ borrowed value does not live long enough
...
15 |     }
   |     - `short` dropped here while still borrowed
16 |     assert_eq!(has.lifetime, "long");
   |     --------------------------------- borrow later used here
```

这同样无法编译，抛出和上面完全一致的错误：

```text
struct Has<'lifetime> {
    lifetime: &'lifetime str,
}

fn main() {
    let long = String::from("long");
    let mut has = Has { lifetime: &long };
    assert_eq!(has.lifetime, "long");

    // this block will never run
    if false {
        let short = String::from("short");
        // "switch" to short lifetime
        has.lifetime = &short;
        assert_eq!(has.lifetime, "short");

        // "switch back" to long lifetime (but not really)
        has.lifetime = &long;
        assert_eq!(has.lifetime, "long");
        // `short` dropped here
    }

    // still a compile error, `short` still "borrowed" after drop
    assert_eq!(has.lifetime, "long");
}
```

lifetime必须在编译时验证，Rust的borrow checker只会进行非常基本的控制流分析，它认为一个if-else语句的所有语句块和一个match语句的所有分支都可能走到，于是会为变量选择其中最短的lifetime。一但一个变量有了lifetime约束，它就永远被那个lifetime约束了。一个变量的lifetime只可能缩短，并且所有的缩短都发生在编译时。

**要点**

- lifetime是在编译时静态验证的
- 在运行时，lifetime不可能以任何方式伸长缩短或改变
- Rust的borrow checker总是会假设所有的分支都会走到，为变量选择其中最短的lifetime

### 9) 把可变引用降级为共享引用是安全的

**误解的推论**

- 重借用一个引用结束了它的lifetime，开启了一个新的lifetime

你能够把一个可变引用传给一个需要共享引用的函数，因为Rust会隐式地把可变引用重借用为不可变引用：

```text
fn takes_shared_ref(n: &i32) {}

fn main() {
    let mut a = 10;
    takes_shared_ref(&mut a); // compiles
    takes_shared_ref(&*(&mut a)); // above line desugared
}
```

直觉上这完全正确，因为把一个可变引用重借用为不可变没有任何危险，对吧？令人惊讶的是并非如此，如下的程序无法编译：

```text
fn main() {
    let mut a = 10;
    let b: &i32 = &*(&mut a); // re-borrowed as immutable
    let c: &i32 = &a;
    dbg!(b, c); // compile error
}
```

抛出如下错误：

```text
error[E0502]: cannot borrow `a` as immutable because it is also borrowed as mutable
 --> src/main.rs:4:19
  |
3 |     let b: &i32 = &*(&mut a);
  |                     -------- mutable borrow occurs here
4 |     let c: &i32 = &a;
  |                   ^^ immutable borrow occurs here
5 |     dbg!(b, c);
  |          - mutable borrow later used here
```

一次可变借用确实发生了，但它被立即无条件地重借用为不可变，然后被drop掉。为什么Rust认为不可变的重借用仍然具有那个可变借用的互斥lifetime？虽然在上面这个例子里没有什么问题，但是允许把可变引用降级为共享引用确实会引入潜在的内存安全问题：

```text
use std::sync::Mutex;

struct Struct {
    mutex: Mutex<String>
}

impl Struct {
    // downgrades mut self to shared str
    fn get_string(&mut self) -> &str {
        self.mutex.get_mut().unwrap()
    }
    fn mutate_string(&self) {
        // if Rust allowed downgrading mut refs to shared refs
        // then the following line would invalidate any shared
        // refs returned from the get_string method
        *self.mutex.lock().unwrap() = "surprise!".to_owned();
    }
}

fn main() {
    let mut s = Struct {
        mutex: Mutex::new("string".to_owned())
    };
    let str_ref = s.get_string(); // mut ref downgraded to shared ref
    s.mutate_string(); // str_ref invalidated, now a dangling pointer
    dbg!(str_ref); // compile error as expected
}
```

这里的问题在于当你重借用一个可变引用为共享引用时，你得到了共享引用和一个巨大的“惊喜”：它延长了可变引用的lifetime，直到这个重借用结束，即使可变借用本身已经被drop。使用这个重借用的共享引用非常困难，因为它是不可变的却又不能和其它的共享引用重叠。这个重借用的共享引用具有可变引用的所有缺点和共享引用的所有缺点，却没有任意一个的优点。我认为重借用可变引用为共享引用是一个反模式。了解这个反模式是重要的，这样当你看到如下的代码时你就能轻易地识别出来：

```text
// downgrades mut T to shared T
fn some_function<T>(some_arg: &mut T) -> &T;

struct Struct;

impl Struct {
    // downgrades mut self to shared self
    fn some_method(&mut self) -> &self;

    // downgrades mut self to shared T
    fn other_method(&mut self) -> &T;
}
```

即使你在函数和方法的签名里避免了重借用，Rust仍然会自动地进行隐式的重借用，所以容易无意识地撞上去：

```text
use std::collections::HashMap;

type PlayerID = i32;

#[derive(Debug, Default)]
struct Player {
    score: i32,
}

fn start_game(player_a: PlayerID, player_b: PlayerID, server: &mut HashMap<PlayerID, Player>) {
    // get players from server or create & insert new players if they don't yet exist
    let player_a: &Player = server.entry(player_a).or_default();
    let player_b: &Player = server.entry(player_b).or_default();

    // do something with players
    dbg!(player_a, player_b); // compile error
}
```

上面的代码编译失败。or_default()返回了&mut Player，我们通过显式的类型标注把它隐式地重借用为&Player。我们必须这样写：

```text
use std::collections::HashMap;

type PlayerID = i32;

#[derive(Debug, Default)]
struct Player {
    score: i32,
}

fn start_game(player_a: PlayerID, player_b: PlayerID, server: &mut HashMap<PlayerID, Player>) {
    // drop the returned mut Player refs since we can't use them together anyway
    server.entry(player_a).or_default();
    server.entry(player_b).or_default();

    // fetch the players again, getting them immutably this time, without any implicit re-borrows
    let player_a = server.get(&player_a);
    let player_b = server.get(&player_b);

    // do something with players
    dbg!(player_a, player_b); // compiles
}
```

有一点笨拙，但这就是我们在内存安全的祭坛献上的祭品。

**要点**

- 不要试图把可变引用重借用为共享引用，你不会开心的
- 重借用一个可变引用不会终结它的lifetime，即使它被drop

### 10) 闭包和函数遵循同样的lifetime省略规则

这更多的是一个Rust的坑，而不是一个误解。

虽然闭包是函数，但它并不遵循和函数同样的lifetime省略规则。（译注：这里的意思应该是闭包在使用上和函数相同，定义的语法也相似）

```text
fn function(x: &i32) -> &i32 {
    x
}

fn main() {
    let closure = |x: &i32| x;
}
```

抛错

```text
error: lifetime may not live long enough
 --> src/main.rs:6:29
  |
6 |     let closure = |x: &i32| x;
  |                       -   - ^ returning this value requires that `'1` must outlive `'2`
  |                       |   |
  |                       |   return type of closure is &'2 i32
  |                       let's call the lifetime of this reference `'1`
```

补全后我们得到：

```text
// input lifetime gets applied to output
fn function<'a>(x: &'a i32) -> &'a i32 {
    x
}

fn main() {
    // input and output each get their own distinct lifetimes
    let closure = for<'a, 'b> |x: &'a i32| -> &'b i32 { x };
    // note: the above line is not valid syntax, but we need it for illustrative purposes
}
```

这种不一致并没有什么原因。闭包最初是用和函数不同的类型推导语义实现的，现在我们也不能改变了，因为把它们统一起来会是一个不兼容改动。那么我们要如何显式地标注一个闭包的类型呢？我们的选择包括：

```text
fn main() {
    // cast to trait object, becomes unsized, oops, compile error
    let identity: dyn Fn(&i32) -> &i32 = |x: &i32| x;

    // can allocate it on the heap as a workaround but feels clunky
    let identity: Box<dyn Fn(&i32) -> &i32> = Box::new(|x: &i32| x);

    // can skip the allocation and just create a static reference
    let identity: &dyn Fn(&i32) -> &i32 = &|x: &i32| x;

    // previous line desugared :)
    let identity: &'static (dyn for<'a> Fn(&'a i32) -> &'a i32 + 'static) = &|x: &i32| -> &i32 { x };

    // this would be ideal but it's invalid syntax
    let identity: impl Fn(&i32) -> &i32 = |x: &i32| x;

    // this would also be nice but it's also invalid syntax
    let identity = for<'a> |x: &'a i32| -> &'a i32 { x };

    // since "impl trait" works in the function return position
    fn return_identity() -> impl Fn(&i32) -> &i32 {
        |x| x
    }
    let identity = return_identity();

    // more generic version of the previous solution
    fn annotate<T, F>(f: F) -> F where F: Fn(&T) -> &T {
        f
    }
    let identity = annotate(|x: &i32| x);
}
```

你肯定已经从上面的例子里发现了，当闭包类型被用作trait约束时，它们确实遵循正常的函数lifetime省略规则。

这里并没有什么教训或者见解，它是怎样就是怎样。

（译注：如果只是一个不捕获环境的闭包，可以这样写：let closure: for<'a> fn(&'a i32) -> &'a i32 = |x: &i32| x;。在nightly上其实已经可以这样写了：let identity: impl Fn(&i32) -> &i32 = |x: &i32| x;。实在不行我们还有无敌的unsafe：let closure = |x: &i32| unsafe { &*(x as *const _) };）

**要点**

- 每种语言都有坑

### 11) 'static引用总能被转换成'a引用

我之前展示过如下代码：

```text
fn get_str<'a>() -> &'a str; // generic version
fn get_str() -> &'static str; // 'static version
```

有几个读者联系我，询问这两者在实践上是否有区别。最初我并不确定，调研了一番之后，不幸的结论是答案是yes，这两个函数在实践上确实有区别。

一般情况下，当处理值时，我们可以在需要'a引用时使用'static引用，因为Rust会自动地把'static引用转换为'a引用。直觉上这是正确的，因为当需要一个短的lifetime时，使用一个长的lifetime不会导致任何内存安全问题。下面的程序符合预期地编译成功：

```text
use rand;

fn generic_str_fn<'a>() -> &'a str {
    "str"
}

fn static_str_fn() -> &'static str {
    "str"
}

fn a_or_b<T>(a: T, b: T) -> T {
    if rand::random() {
        a
    } else {
        b
    }
}

fn main() {
    let some_string = "string".to_owned();
    let some_str = &some_string[..];
    let str_ref = a_or_b(some_str, generic_str_fn()); // compiles
    let str_ref = a_or_b(some_str, static_str_fn()); // compiles
}
```

然而当引用是一个函数的类型签名的一部分时，这个转换并不会发生，因此如下代码编译失败：

```text
use rand;

fn generic_str_fn<'a>() -> &'a str {
    "str"
}

fn static_str_fn() -> &'static str {
    "str"
}

fn a_or_b_fn<T, F>(a: T, b_fn: F) -> T
    where F: Fn() -> T
{
    if rand::random() {
        a
    } else {
        b_fn()
    }
}

fn main() {
    let some_string = "string".to_owned();
    let some_str = &some_string[..];
    let str_ref = a_or_b_fn(some_str, generic_str_fn); // compiles
    let str_ref = a_or_b_fn(some_str, static_str_fn); // compile error
}
```

抛出错误：

```text
error[E0597]: `some_string` does not live long enough
  --> src/main.rs:23:21
   |
23 |     let some_str = &some_string[..];
   |                     ^^^^^^^^^^^ borrowed value does not live long enough
...
25 |     let str_ref = a_or_b_fn(some_str, static_str_fn);
   |                   ---------------------------------- argument requires that `some_string` is borrowed for `'static`
26 | }
   | - `some_string` dropped here while still borrowed
```

至于这是不是一个Rust的坑，是有争议的。这并不是一个简单直接的把&'static str转换为&'a str，而是把for<T> Fn() -> &'static T转换为for<'a, T> Fn() -> &'a T。前者是值的转换，而后者是类型的转换。

**要点：**

- 签名为for<'a, T> fn() -> &'a T的函数比签名为for<T> fn() -> &'static T的函数更灵活，适用于更多的场景

## 结论

- T是&T和&mut T的超集

- &T和&mut T是不相交的集合

- T: 'static应该读作“T被'static lifetime约束”

- 如果T: 'static，那么T可以是一个有'static lifetime的借用类型，或者是一个独立类型

- T: 'static包括了所有独立类型，也就意味着T

- - 可以在运行时动态创建
  - 不需要在整个程序运行时有效
  - 可以被安全和随意地改变
  - 可以在运行时动态地drop
  - 可以拥有任意的lifetime

- T: 'a比&'a T更通用和灵活

- T: 'a接受独立类型，含有引用的独立类型，以及引用

- &'a T只接受引用

- 如果T: 'static，那么T: 'a，因为任意'a均满足'static >= 'a

- 几乎所有Rust代码都是泛型代码，到处都是省略的lifetime标注

- Rust的lifetime省略规则并不是所有情形都总是正确的

- Rust并不比你更懂你程序的语义

- 给你的lifetime标注有意义的名字

- 当添加显式lifetime标注时，想清楚加在哪和为什么

- 所有的trait object都有推导出来的默认lifetime约束

- Rust编译器的错误消息建议的修复方式，会让你的程序通过编译，但这和能让你的程序通过编译并且最符合你的程序的需求是两回事

- lifetime是在编译时静态验证的

- 在运行时，lifetime不可能以任何方式伸长缩短或改变

- Rust的borrow checker总是会假设所有的分支都会走到，为变量选择其中最短的lifetime

- 不要试图把可变引用重借用为共享引用，否则你会不开心的

- 重借用一个可变引用不会终结它的lifetime，即使它被drop

- 每种语言都有坑

- 签名为for<'a, T> fn() -> &'a T的函数比签名为for<T> fn() -> &'static T的函数更灵活，适用于更多的场景