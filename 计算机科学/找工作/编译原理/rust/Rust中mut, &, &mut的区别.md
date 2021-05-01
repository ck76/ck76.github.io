资源：内存区块。不同的内存区块位置和大小就是不同的资源。

- **str**

`let a = "xxx".to_string();`　　
含义：a绑定到字符串资源A上，拥有资源A的所有权

`let mut a = "xxx".to_string();`　
含义：a绑定到字符串资源A上，拥有资源A的所有权，同时a还可绑定到新的资源上面去（更新绑定的能力，但新旧资源类型要同）；

- **value**

`let b = a;`
含义：a绑定的资源A转移给b，b拥有这个资源A

`let b = &a;`　　
含义：a绑定的资源A借给b使用，b只有资源A的读权限

`let b = &mut a;`　　
含义：a绑定的资源A借给b使用，b有资源A的读写权限

`let mut b = &mut a;`　　
含义：a绑定的资源A借给b使用，b有资源A的读写权限。同时，b可绑定到新的资源上面去（更新绑定的能力）

- **String**

`fn do(c: String) {}`　　
含义：传参的时候，实参d绑定的资源D的所有权转移给c

`fn do(c: &String) {}`　　
含义：传参的时候，实参d将绑定的资源D借给c使用，c对资源D只读

`fn do(c: &mut String) {}`　　
含义：传参的时候，实参d将绑定的资源D借给c使用，c对资源D可读写

`fn do(mut c: &mut String) {}`　　
含义：传参的时候，实参d将绑定的资源D借给c使用，c对资源D可读写。同时，c可绑定到新的资源上面去（更新绑定的能力）

> 函数参数里面，冒号左边的部分，mut c，这个mut是对函数体内部有效；冒号右边的部分，&mut String，这个 &mut 是针对外部实参传入时的形式化（类型）说明。

下面的例子输出是什么：

```c
fn concat_literal(s: &mut String) {      
    s.extend("world!".chars());          
}                                        
                                         
fn main() {                              
    let mut s = "hello, ".to_owned();    
    concat_literal(&mut s);              
    println!("{}", s);                   
}                                        
123456789
```



---

# mut a:&T 和a:&mut T的区别

## 概述

话说 StackOverflow 上有个哥们问了一个问题，正如标题所述，它问的是下面这段代码里:

```rust
fn modify_foo(mut foo: Box<i32>) { *foo += 1; *foo }
fn modify_foo(foo: &mut i32) { *foo += 1; *foo }
```

代码里的**mut**放在 `mut foo: Box<i32>`和`foo: &mut i32` 的区别是什么？

下面是结论，没有 C/C++编程经验的童鞋可以忽略中间这一栏

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glqwmu5fmej31560e7goq.jpg" alt="img" style="zoom:50%;" />

## mut a:&T

先来看 `mut a : T`和`mut a : &T`的区别，这个应该比较简明，即前者中`a`是`T`类型变量的可变绑定，后者中`a`是`T`类型不可变引用的可变绑定。(如果对引用的概念还比较模糊，可以参考公众号中翻译的一篇文章:《Rust 中的引用》)
下面来看个例子：

```rust
struct FullName{
    first_name:String,
    last_name:String,
}

// mut a:& T
let mut a = & FullName {
    first_name: String::from("Jobs"),
    last_name: String::from("Steve"),
};
//a重新绑定到一个新的FullName的引用
a = &FullName {
    first_name: String::from("Gates"),
    last_name: String::from("Bill"),
};
//不允许对a指向的内容作出修改
//a.first_name = String::from("Error");
println!("{}:{}",a.last_name, a.first_name);
```

这里的`a`是可变的，意思是`a`可以重新绑定另一个结构体的引用，但是不能对结构体里的内容作修改(比如这里对 a.first_name 的赋值就是不允许的)，因为引用(`&T`)是不可变的。

## a:&mut T

接着看下面这个例子:

```rust
// a:&mut T
let a = &mut FullName {
    first_name: String::from("Jobs"),
    last_name: String::from("Steve"),
};
//a不允许重新绑定到一个新的FullName的引用
// a = &FullName {
//     first_name: String::from("Gates"),
//     last_name: String::from("Bill"),
// };
//允许对a指向的内容作出修改
a.first_name = String::from("Gates");
println!("{}:{}",a.last_name, a.first_name);
```

这里的 a 绑定到了`FullName`的可变引用，也就是说可以对`FullName`进行修改，但是`a`是不可以修改的，即通过`a`修改`FullName`里的字段，如`first_name`是没问题的，但是如果要`a`重新绑定到新的结构体(相当于修改`a`)是不行的。