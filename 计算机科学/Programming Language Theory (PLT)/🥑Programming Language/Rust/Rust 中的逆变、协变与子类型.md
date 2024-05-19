[TOC]

学习C#（未遂）的时候碰到逆变和协变的概念。本来以为没有继承的语言不需要太考虑这个，结果我错了。在Rust

中又碰到这些概念。遂记录如下。

除了

Rust的小节，都不需要了解相关语言知识，伪代码用类似TypeScript

的语法书写。

##             子类型

首先要讲的是子类型（Subtyping），子类型在生活中最常见到的就是面向对象概念中的继承了。比如说我们现在有一个 `Animal` 类，现在我们定义一个新的类叫做 `Cat`。`Animal` 只有一些简单的

method

比如说移动、进食，而 `Cat` 则附加上了爬树、卖萌等。

当我们基于 `Animal` 定义 `Cat` 的时候，`Cat` 相对于 `Animal` 的[内涵增加了，而外延收缩](https://www.zhihu.com/question/22267682/answer/28974249)了。抛开这种头昏脑胀的词，可以这样认为，我们**至少**可以说一个猫是一个动物。所以猫是动物的子类型，记作 `Cat: Animal`。

因为猫至少是一个动物，那么对于所有需要任何动物的地方，我都可以给一只猫。

比如说任何动物都可以做薛定谔实验的牺牲品，但按照残忍的惯例，我们塞一只猫。

```typescript
function schrödinger(sample: Animal) -> bool { ... }
let cat = new Cat();
const alive = schrödinger(cat);
```

也就是说，当 `T: U` 的时候，任何需要形式参数 `a: U` 的函数，我们都能给一个实际参数 `a: T` ——子类型**至少**可以被当作它的超类型。

##             协变、逆变、不变

作为基础，需要稍微提起类型构造器（Type constructor）。类型构造器就是一些带有泛型/模板参数的类型。当填满了参数，才会成为一个实际的类型。比如说很简单的「笼子」就是 `Cage<T>`，其中 `T` 就是类型参数。还有一些常见的比如说 `List<T>`。

现在回顾一下，我们现在知道一些类型之间的关系，也即是说我们知道 `Cat` 是 `Animal` 的子类型。那么对于随意的（一元）类型构造器 `M`， `M<Cat>` 和 `M<Animal>` 可能会有什么关系呢？（[Wiki](https://zh.wikipedia.org/wiki/协变与逆变)）

- 协变（covariance）：`M<Cat>: M<Animal>` 它们维持内部参数的关系不变。
- 逆变（contravariance）：`M<Animal>: M<Cat>` 它们的关系被反转了。
- 不变（invariance）：两者没有任何子类型关系。

直觉上来说，只要有协变就够了：

薛定谔想要一个笼子，里面装着一种动物，他不关心是什么动物（`Cage<Animal>`），你给薛定谔一只装着猫的笼子（`Cage<Cat>`），薛定谔把这个猫当作一种动物做实验。也就是说在需要 `Cage<Animal>` 的地方都可以给一个 `Cage<Cat>` 。

然而这是不对的。考虑这样一个情况：

```typescript
let cage: Cage<Cat> = new Cage();
function capture(x: Cage<Animal>) {
	x.inner = new Dog();
}
capture(cat);
```

因为协变规则，对 `capture` 来说笼子是 `Cage<Animal>`。往笼子里塞一个狗，完全没问题。但是对于调用者来说，笼子的类型还是 `Cage<Cat>` 。这就破坏了类型安全。你接下来的代码期望这是装猫的笼子，其实里面装了一个狗。

所以如果一个容器是只读的，才能协变。不然很容易就能把一些特殊的容器协变到更一般的容器，再往里面塞进不应该塞的类型。

考虑 `Cage<T>` 对 `T` 逆变的情况，`Cage<Animal>: Cage<Dog>` 。也就是说当函数需要 `Cage<Dog>` 的时候，总能传给函数一个 `Cage<Animal>`，函数当作 `Cage<Dog>` 来处理。

一般来说这很荒谬，`Cage<Animal>` 里面的动物可能是一只猫，强行当作一个狗来处理肯定会爆炸。但是对于上面的 `capture` 函数是有意义的，它不关心笼子里有什么，只往里面塞一个准备好的狗。也就是说对于只写的类型可以用逆变。

那么对于可读又可写的类型，当然就是不变了：我们不能做出任何假定，不然有可能爆炸。

还有一种特殊的类型，规则有点奇异，那就是函数类型。考虑一元函数，按照函数的箭头记法，把函数类型记作 `T -> U`，其中 `T` 是逆变的而 `U` 是协变的。

返回值是协变的很好理解，我需要函数 `F` 最终返回一只动物，那么最终返回一只猫的函数是可接受的。（可以不断地扩大陪域）

参数是逆变可能有点奇怪了。考虑需要计算猫的年龄的情况： `Cat -> Age`。给一个通用的，可以计算所有动物的年龄的函数 `Animal -> Age` 来代替也是很好的。`Animal -> Age` 的定义域 `Animal` 中那些 `Cat` 以外的值被裁掉了（我们只会传 `Cat`），就变成很棒的 `Cat -> Age`。

所以任何时候，对需要一个一元函数 `T: U, T -> V` 的情况，它的参数 `T` 可以用 `U` 来代替，只需要简单地无视 `U` 类型除 `T` 以外的取值就行了。

综合起来，也就是说：

T1:T2U1:U2⇓T2→U1:T1→U1T1→U1:T1→U2T2→U1:T1→U2

##             Rust

## 中的

继承是狗屎。Rust

中没有继承，所以没有逆变和协变！以上。

才怪。*子类型不一定是继承，逆变和协变也不一定是对于子类型的*。Rust

中确实没有继承（不算Trait

的话），结构体或者枚举之间没有子类型关系。

但是

Rust中有lifetime，lifetime是和通常类型平行的另一套类型（另一个范畴），而Rust中的子类型就是对于lifetime而言的。[这篇文章详细描述了](https://doc.rust-lang.org/nomicon/subtyping.html)[lifetime](https://doc.rust-lang.org/nomicon/subtyping.html)

[的子类型及逆变协变](https://doc.rust-lang.org/nomicon/subtyping.html)，本节只是笔记。

###             子类型

子类型是一种序关系，不一定是像继承那样的超类型直接包含子类型（动物包含猫）。lifetime

中，外层的lifetime是它所包含的内层lifetime的子类型： `'big: 'small`。所有人最初学到Rust中lifetime

的子类型关系，都会对此感到困惑。

![lifetime-subtyping](https://p.ipic.vip/ajjpu0.jpg)

这张图的子类型关系应当是 `'static: 'big: 'small_1`，`'static: 'big: 'small_2`。而 `'static` 则是所有

lifetime

的子类型。

lifetime

就是作用域，作用域是很标准的嵌套关系，所以Rust的规则有点反直觉。对于集合，子集扩张到超集往往是恰当的，但一个作用域本身不应该被当作一个集合。我们可以说一只猫**至少**是一个动物。但对于lifetime，我不能说 `'small_1`**至少**是 `'big`，而应该说 `'big`**至少**是

`'small_1`，也**至少**是 `'small_2`。

lifetime

存在的意义就是界定资源*不应该超出一个范围*。也就是说扩张lifetime往往是危险的，而收缩（只读引用）lifetime

是安全的。

如果遵循直觉，按照嵌套关系排列，也就是 `'small_1: 'big: 'static`。小的作用域 `'small_1` 就可以协变到全局作用域 `'static` ，那么在被读取的对象被销毁后，编译器还允许代码继续试图读取它，就会爆炸，整个

lifetime

系统就失效了。

几个方式来理解：

1. ​              `'small_1` 的 lifetime 代表「包含 `'small_1` 的作用域的集合」；而 `'static` 就是「包含全局作用域的集合」只有一个元素。所以很显然后者是前者的子集，因为全局作用域包含了 `'small_1`。
2. 越小的作用域，包含了它的外层作用域就越多。
3. lifetime

1. 类型所关联的作用域是内涵而不是外延，内涵增多则外延减少。正如 `Cat` 就是 `Animal` 增多内涵而来的，它的外延只有各种猫，而 `Animal` 的外延有各种动物。

###             逆变、协变、不变

[原文](https://doc.rust-lang.org/nomicon/subtyping.html)详细地讲了这一块，简单记录一下。

`&` 和 `&mut` 都是一个类型构造器，接受一个

lifetime `'a` 和另一个类型 `T` 。

- ​              `&'a T` 对 `'a` 和 `T` 协变。因为 `&` 是只读的，传参数的时候，试图收缩

lifetime

是安全的。

​              `&'a mut T` 对 `'a` 协变，对 `T` 不变。这是唯一要多说说的。

​              `fn(T) -> U` 是对 `T` 逆变对 `U` 协变。原因和上文所述一样：当传入的参数是一个函数的时候，我们可以安全地收缩这个函数的定义域，扩张这个函数的陪域。除此处外

Rust

应该没有逆变。

​              `Box`, `Vec` 和别的容器都是**协变**的。这在别的语言中会爆炸，但是

Rust

- 对可变性的限制导致可以安全地当作协变。当我们拿到一个容器的所有权的时候，外部别处就无法访问了，可以安全地对它协变而不用担心爆炸。
- ​              `Cell<T>` `RefCell<T>`，在内部是可读写的，所以是不变。

`&'a mut T` 对 `'a` 协变，对 `T` 不变。是因为传参数的时候，收紧一个可变作用域的范围是安全的，调用者还维持着未收紧的作用域。

但是正如文中的例子一样：

```rust
fn overwrite<T: Copy>(covarianced: &mut T, short: &mut T) {
    *covarianced = *short;
}

fn main() {
    let mut forever: &'static str = " 我会活到世界末日 ";
    'small {
        let short = String::from(" 我马上死了 ");
        overwrite(&mut forever, &mut &*short);
    }
    // 爆炸！用到了已经被释放的内存
    println!("{}", forever);
}
```

如果在一次函数调用时，一个 `forever: &mut &'static T` 能够协变到 `covarianced: &mut &'small T` ，我们就可以把一个 `short: &'small T` 存进协变后的参数 `covarianced: &mut &'small T`。

调用了对调用者来说，引用 `a` 的类型依然是 `&mut &'static T`，却存了一个更短命的引用 `short`。当 `short` 被销毁的时候，`a` 还维持着引用，就…会炸！

至于这里为什么不能用逆变，原因很简单，就是 `&mut T` ，`T` 是可读可写的。如果能扩张

`T` 的作用域（逆变），读取出来存到别的地方还是会炸。

> if variance would allow you to store a short-lived value in a longer-lived slot, then invariance must be used.

如果使用逆变或者协变，将允许把短命的值塞进长寿的坑坑里面，就用不变（invariance）以阻止。

> More generally, the soundness of subtyping and variance  is based on the idea that its ok to forget details, but with mutable  references there’s always someone (the original value being referenced)  that remembers the forgotten details and will assume that those details  haven’t changed. If we do something to invalidate those details, the  original location can behave unsoundly.

更概括来说，对于子类型做逆变或者协变的健全性（[soundness](https://en.wikipedia.org/wiki/Soundness)）来源于，*被调用者*可以放心地忘记细节。遇到一个

mutable引用代表有别人维持了那些被*被调用者*忽略的细节，并希望这些细节不会被改动。如果*被调用者*做了什么事使这些细节失效了，原本维持这些细节的地方就会爆炸。



- https://ioover.net/dev/variance-and-subtyping/
- https://zhuanlan.zhihu.com/p/41814387?utm_psn=1775495484703961089