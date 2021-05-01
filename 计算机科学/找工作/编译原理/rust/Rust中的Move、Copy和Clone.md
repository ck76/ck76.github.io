https://www.zhihu.com/tardis/sogou/art/184907190

## **简介(Introduction)**

move 和 copy 是 Rust 中的基础概念。这对于来自 Ruby、Python 或 C#等垃圾回收语言的程序员来说可能是完全陌生的。这些术语在 C++中也确实存在，但它们在 Rust 中的含义却有微妙的不同。在本文中，我将解释对值进行 move、copy 和 clone 在 Rust 中到底意味着什么？让我们开始吧。

## **Move**

正如在**Memory safety in Rust - part 2[1]**所展示的，把一个变量赋值给另一个变量会把所有权(ownership)转移给受让者：

```rust
let v:Vec<i32> = Vec::new();
let v1 = v;//v1 is the new owner
```

在上面的例子中，`v`被move到`v1`。但是move `v`意味着什么？要想理解这个问题，我们需要先来看一下一个`Vec`在内存中是如何布局的：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glodo90tkfj308305bq2w.jpg)

`Vec`不得不维护一个动态增长或收缩(shrinking)的缓冲区(buffer)。这个缓冲区被分配在堆上，包含`Vec`里真正的元素。此外，`Vec`还在栈上有一个小的对象。这个对象包含某些内务信息：一个指向堆上缓冲区的*指针(pointer)* ，缓存区的*容量(capacity)* 和*长度(length)* (比如，当前被填满的容量是多少)。

当变量`v`被move到`v1`时，栈上的对象被逐位拷贝(bitwise copy)：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glodo793nkj308307jjrh.jpg)

> “ 在前面的例子中，本质上发生的是一个浅拷贝(shallow copy)。这与C++形成了鲜明的对比，当一个向量被赋值给另一个变量时，C++会进行深拷贝(deep copy)。

堆上的缓冲区保持不变。这确实是一个move：现在`v1`负责释放缓冲区，`v`不能接触这个缓冲区。

```rust
let v: Vec<i32> = Vec::new();
let v1 = v;
println!("v's length is {}", v.len());//error: borrow of moved value: `v`
```

这个所有权的改变很好，因为如果`v`和`v1`都被允许访问，那么就有两个栈上的对象指向相同的堆缓冲区。



![img](https://tva1.sinaimg.cn/large/0081Kckwly1glodo67mrsj308307jt8t.jpg)

这种情况，应该由哪个对象释放缓冲区呢？因为这是不清晰的，所以Rust从根本上避免了这种情况的出现。

赋值不是唯一涉及到move的操作。当传递参数或者从函数返回的时候，值也会被move：

```rust
let v: Vec<i32> = Vec::new();
//v is first moved into print_len's v1
//and then moved into v2 when print_len returns it
let v2 = print_len(v);
fn print_len(v1: Vec<i32>) -> Vec<i32> {
    println!("v1's length is {}", v1.len());
    v1//v1 is moved out of the function
}
```

或者被赋值给结构体或枚举的成员:

```rust
struct Numbers {
    nums: Vec<i32>
}
let v: Vec<i32> = Vec::new();
//v moved into nums field of the Numbers struct
let n = Numbers { nums: v };

enum NothingOrString {
    Nothing,
    Str(String)
}
let s: String = "I am moving soon".to_string();
//s moved into the enum
let nos = NothingOrString::Str(s);
```

以上就是关于move的全部内容。下面让我们来看一下copy。

## **Copy**

还记得上面的这个例子么？

```rust
let v: Vec<i32> = Vec::new();
let v1 = v;
println!("v's length is {}", v.len());//error: borrow of moved value: `v`
```

如果我们把变量`v`和`v1`的类型从`Vec`改为`i32`会发生什么？

```rust
let v: i32 = 42;
let v1 = v;
println!("v is {}", v);//compiles fine, no error!
```

这几乎是相同的代码。为什么这次赋值没有把`v` move到`v1`呢？要想理解这个，我们需要再来看一下内存布局:



![img](https://tva1.sinaimg.cn/large/0081Kckwly1glodo4bba1j308305bwef.jpg)

在这个例子中，值完全被包含在栈上。在堆上什么都没有拥有。这就是为什么`v`和`v1`都被允许访问是没有问题的——它们是完全独立的拷贝(copy)。

像这样没有拥有其他资源的类型且可以被逐位拷贝(bitwise copy)的类型被称为`Copy`类型。它们实现了**Copy marker trait[2]**。所有的基本类型，像整数，浮点数和字符都是`Copy`类型。结构体或枚举默认不是`Copy`但是你可以派生(derive)自一个`Copy` trait：

```rust
#[derive(Copy, Clone)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(Copy, Clone)]
enum SignedOrUnsignedInt {
    Signed(i32),
    Unsigned(u32),
}
```

> 在派生语句中的`Clone`是需要的，因为`Copy`的定义类似这样:`pub trait Copy:Clone {}`

为了能让`#[derive(Copy, Clone)]`正常工作，结构体或枚举的所有成员自身必须是`Copy`类型。例如，下面这样就无法正常工作:

```rust
//error:the trait `Copy` may not be implemented for this type
//because its nums field does not implement `Copy`
#[derive(Copy, Clone)]
struct Numbers {
    nums: Vec<i32>
}
```

当然，你也可以手动实现`Copy`和`Clone`：

```rust
struct Point {
    x: i32,
    y: i32,
}

//no method in Copy because it is a marker trait
impl Copy for Point {}

impl Clone for Point {
    fn clone(&self) -> Point {
        *self
    }
}
```

通常来讲，任何实现了`Drop`的类型都不能被`Copy`，因为`Drop`是被拥有其他资源的类型来实现的，且因此不能被简单地逐位拷贝。但是`Copy`类型应该是可以被拷贝的。因此，`Drop`和`Copy`不能很好地混合在一起使用。

以上就是关于copy的内容，下面是clone。

## **Clone**

当一个值被move的时候，Rust做一个浅拷贝；但是如果你想像在C++里那样创建一个深拷贝该怎么办呢？要实现这个，这个类型必须首先实现**Clone trait[3]**。接着做一个深拷贝，客户端代码应该调用`clone`方法:

```rust
let v: Vec<i32> = Vec::new();
let v1 = v.clone();//ok since Vec implements Clone
println!("v's length is {}", v.len());//ok
```

在`clone`调用后，就产生了下面的内存布局:



![img](https://tva1.sinaimg.cn/large/0081Kckwly1glodo2npyuj30830avaa7.jpg)

由于是深拷贝，`v`和`v1`可以自由独立地释放它们的堆缓冲区。

> `clone`方法不总是会创建一个深拷贝。类型可以以任意想要的方式自由实现`clone`，但是语义上，它应该足够接近复制一个对象的含义。例如，`Rc`和`Arc`取而代之的是增加了一个引用计数。

这就是关于clone的全部内容。

## **总结(Conclusion)**

在本文中，我更深入地研究了Rust中move、copy和clone的语义。我试图捕捉到与C++相比在意义上的细微差别。

Rust很优秀，因为它有优秀的默认值。例如，Rust中的赋值操作符要么移动值，要么做简单的逐位拷贝。另一方面，在C++中，一个看似无害的赋值可能隐藏了大量的代码，这些代码作为重载赋值操作符的一部分运行。在Rust中，这样的代码被公开，因为程序员必须显式地调用`clone`方法。

可以说，这两种语言做出了不同的取舍，但我喜欢Rust由于这些设计选择而带来的额外的安全保障。

### **参考资料**

[1]

Memory safety in Rust - part 2: *[https://hashrust.com/blog/memory-safety-in-rust-part-2/](https://link.zhihu.com/?target=https%3A//hashrust.com/blog/memory-safety-in-rust-part-2/)*

[2]

Copy marker trait: *[https://doc.rust-lang.org/std/marker/trait.Copy.html](https://link.zhihu.com/?target=https%3A//doc.rust-lang.org/std/marker/trait.Copy.html)*

[3]