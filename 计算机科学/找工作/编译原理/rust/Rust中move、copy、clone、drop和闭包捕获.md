> 本文中的变量，指的是通过如下代码定义的常量a和变量b。实例指的是绑定到a的`i32`类型在stack内存的数据，和绑定到b变量的`String`类型在stack内存和heap内存中的数据。

```

let a = 0_u32;
let mut b = "Hello".to_string();
```

### **先说说使用场景**

- move、copy的应用场景，主要是在变量赋值、函数调用的传入参数、函数返回值、闭包的变量捕获。
- clone需要显式调用。
- drop是在变量的作用范围结束时，被自动调用。
- 闭包中使用了外部变量，就会有闭包捕获。

### **move语义**

rust中的类型，如果没有实现`Copy` trait，那么在此类型的变量赋值、函数入参、函数返回值都是move语义。这是与c++的最大区别，从c++11开始，右值引用的出现，才有了move语义。但rust天生就是move语义。

如下的代码中，变量a绑定的`String`实例，被move给了b变量，此后a变量就是不可访问了（编译器会帮助我们检查）。然后b变量绑定的`String`实例又被move到了f1函数中，，b变量就不可访问了。f1函数对传入的参数做了一定的运算后，再将运算结果返回，这是函数f1的返回值被move到了c变量。在代码结尾时，只有c变量是有效的。

```
fn f1(s: String) -> String {    s + " world!"}
let a = fn f1(s: String) -> String {
    s + " world!"
}

let a = String::from("Hello");
let b = a;
let c = f1(b);String::from("Hello");let b = a;let c = f1(b);
```

注意，如上的代码中，`String`类型没有实现`Copy` trait，所以在变量传递的过程中，都是move语义。

### **copy语义**

rust中的类型，如果实现了`Copy` trait，那么在此类型的变量赋值、函数入参、函数返回值都是copy语义。这也是c++中默认的变量传递语义。

看看类似的代码，变量a绑定的`i32`实例，被copy给了b变量，此后a、b变量同时有效，并且是两个不同的实例。然后a变量绑定的`i32`实例又被copy到了f1函数中，a变量仍然有效。传入f1函数的参数i是一个新的实例，做了一定的运算后，再将运算结果返回。这时函数f1的返回值被copy到了c变量，同时f1函数中的运算结果作为临时变量也被销毁（不会调用drop，如果类型实现了`Copy` trait，就不能有`Drop` trait）。传入b变量调用f1的过程是相同的，只是返回值被copy给了d变量。在代码结尾时，a、b、c、d变量都是有效的。

```

fn f2(i: i32) -> i32 {
    i + 10
}

let a = 1_i32;
let b = a;
let c = f1(a);
let d = f1(b);
```

这里再强调下，`i32`类型实现了`Copy` trait，所以整个变量传递过程，都是copy语义。

### **clone语义**

move和copy语义都是隐式的，clone需要显式的调用。

参考类似的代码，变量a绑定的`String`实例，在赋值前先clone了一个新的实例，然后将新实例move给了b变量，此后a、b变量同时有效。然后b变量在传入f1函数前，又clone一个新实例，再将这个新实例move到f1函数中。f1函数对传入的参数做了一定的运算后，再将运算结果返回，这里函数f1的返回值被move到了c变量。在代码结尾时，a、b、c变量都是有效的。

```
fn f1(s: String) -> String {
    s + " world!"
}

let a = String::from("Hello");
let b = a.clone();
let c = f1(b.clone());
```

在这个过程中，在隐式move前，变量clone出新实例并将新实例move出去，变量本身保持不变。

### **drop语义**

rust的类型可以实现`Drop` trait，也可以不实现`Drop` trait。但是对于实现了`Copy` trait的类型，不能实现`Drop` trait。也就是说`Copy`和`Drop`两个trait对同一个类型只能有一个，鱼与熊掌不可兼得。

变量在离开作用范围时，编译器会自动销毁变量，如果变量类型有`Drop` trait，就先调用`Drop::drop`方法，做资源清理，一般会回收heap内存等资源，然后再收回变量所占用的stack内存。如果变量没有`Drop` trait，那就只收回stack内存。

正是由于在`Drop::drop`方法会做资源清理，所以`Copy`和`Drop` trait只能二选一。如果类型实现了`Copy` trait，在copy语义中并不会调用`Clone::clone`方法，不会做deep copy，那就会出现两个变量同时拥有一个资源（比如说是heap内存等），在这两个变量离开作用范围时，会分别调用`Drop::drop`方法释放资源，这就会出现double free错误。

### **copy与clone语义区别**

先看看两者的定义：

```
pub trait Clone: Sized {
    fn clone(&self) -> Self;

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }
}

pub trait Copy: Clone {
    // Empty.
}
```

`Clone`是`Copy`的super trait，一个类型要实现`Copy`就必须先实现`Clone`。

再留意看，`Copy` trait中没有任何方法，所以在copy语义中不可以调用用户自定义的资源复制代码，也就是不可以做deep copy。copy语义就是变量在stack内存的按位复制，没有其他任何多余的操作。

`Clone`中有clone方法，用户可以对类型做自定义的资源复制，这就可以做deep copy。在clone语义中，类型的`Clone::clone`方法会被调用，程序员在`Clone::clone`方法中做资源复制，同时在`Clone::clone`方法返回时，变量的stack内存也会被按照位复制一份，生成一个完整的新实例。

### **自定义类型实现`Copy`和`Clone` trait**

`Clone` trait，对于任何自定义类型都可以实现。`Copy` trait只有自定义类型中的field全部实现了`Copy` trait，才可以实现`Copy` trait。

如下代码举例，`struct S1`中的field分别是`i32`和`usize`类型，都是有`Copy` trait，所以`S1`可以实现`Copy` trait。你可以通过`#[derive(Copy, Clone)]`方式实现，也可以自己写代码实现。

```
struct S1 {
    i: i32,
    u: usize,
}
impl Copy for S1 {}
impl Clone for S1 {
    fn clone(&self) -> Self {
        // 此处是S1的copy语义调用。
        // 正是i32和usize的Copy trait，才有了S1的Copy trait。
        *self
    }
}
```

但是对于如下的`struct S2`，由于`S2`的field中有`String`类型，`String`类型没有实现`Copy` trait，所以`S2`类型就不能实现`Copy` trait。`S2`中也包含了`E1`类型，`E1`类型没有实现`Clone`和`Copy` trait，但是我们可以自己实现`S2`类型的`Clone` trait，在`Clone::clone`方法中生成新的`E1`实例，这就可以clone出新的`S2`实例。

```
enum E1 {
    Text,
    Digit,
}
struct S2 {
    u: usize,
    e: E1,
    s: String,
}
impl Clone for S2 {
    fn clone(&self) -> Self {
        // 生成新的E1实例
        let e = match self.e {
            E1::Text => E1::Text,
            E1::Digit => E1::Digit,
        };
        Self {
            u: self.u,
            e,
            s: self.s.clone(),
        }
    }
}
```

注意，在这种情况下，不能通过`#[derive(Clone)]`自动实现`S2`类型的`Clone` trait。只有类型中的所有field都有`Clone`，才可以通过`#[derive(Clone)]`自动实现`Clone` trait。

### **闭包捕获变量**

与闭包关联的是三个trait的定义，分别是`FnOnce`、`FnMut`和`Fn`，定义如下：

```
pub trait FnOnce<Args> {
    type Output;
    fn call_once(self, args: Args) -> Self::Output;
}

pub trait FnMut<Args>: FnOnce<Args> {
    fn call_mut(&mut self, args: Args) -> Self::Output;
}

pub trait Fn<Args>: FnMut<Args> {
    fn call(&self, args: Args) -> Self::Output;
}
```

注意三个trait中方法的receiver参数，`FnOnce`是`self`参数，`FnMut`是`&mut self`参数，`Fn`是`&self`参数。

**原则说明如下：**

- 如果闭包只是对捕获变量的非修改操作，闭包捕获的是`&T`类型，闭包按照`Fn` trait方式执行，闭包可以重复多次执行。
- 如果闭包对捕获变量有修改操作，闭包捕获的是`&mut T`类型，闭包按照`FnMut` trait方式执行，闭包可以重复多次执行。
- 如果闭包会消耗掉捕获的变量，变量被move进闭包，闭包按照`FnOnce` trait方式执行，闭包只能执行一次。

对于实现`Copy` trait和没有实现`Copy` trait对类型，具体参考如下对代码说明。

#### **类型实现了`Copy`，闭包中是`&T`操作**

如下的代码，f闭包对i变量，没有修改操作，此处捕获到的是`&i`，所以f就是按照`Fn` trait方式执行，可以多次执行f。

```
fn test_fn_i8() {
    let mut i = 1_i8;
    let f = || i + 1;

    // f闭包对i是immutable borrowed，是Fn trait
    let v = f();

    // f闭包中只是immutable borrowed，此处可以再做borrowed。
    dbg!(&i);

    // f可以调用多次
    let v2 = f();

    // 此时，f闭包生命周期已经结束，i已经没有borrowed了，所以此处可以mutable borrowed。
    i += 10;

    assert_eq!(2, v);
    assert_eq!(2, v2);
    assert_eq!(11, i);
}
```

#### **类型实现了`Copy`，闭包中是`&mut T`操作**

如下的代码，f闭包对i变量，有修改操作，此处捕获到的是`&mut i`，所以f就是按照`FnMut` trait方式执行，注意f本身也是`mut`，可以多次执行f。

```
fn test_fn_mut_i8() {
    let mut i = 1_i8;
    let mut f = || {
        i += 1;
        i
    };

    // f闭包对i是mutable borrowed，是FnMut trait
    let v = f();

    // i已经被mutable borrowed，就不能再borrowed了。
    // dbg!(&i);

    // f可以调用多次
    let v2 = f();

    // 此时，f闭包生命周期已经结束，i没有mutable borrowed了，所以此处可以mutable borrowed。
    i += 10;

    assert_eq!(2, v);
    assert_eq!(3, v2);
    assert_eq!(13, i);
}
```

#### **类型实现了`Copy`，闭包使用`move`关键字，闭包中是`&mut T`操作**

如下的代码，f闭包对i变量，有修改操作，并且使用了`move`关键字。由于`i8`实现了`Copy` trait，此处i会copy一个新实例，并将新实例move到闭包中，在闭包中的实际是一个新的`i8`变量。f就是按照`FnMut` trait方式执行，注意f本身也是`mut`，可以多次执行f。

重点说明，此处`move`关键字的使用，强制copy一个新的变量，将新变量move进闭包。

```

fn test_fn_mut_i8_move() {
    let mut i = 1_i8;
    let mut f = move || {
        i += 1;
        i
    };

    // i8有Copy trait，f闭包中是move进去的新实例，新实例不会被消耗，是FnMut trait
    let v = f();

    // i8有Copy trait，f闭包中是move进去的新实例，i没有borrowed，所以此处可以mutable borrowed。
    i += 10;

    // f可以调用多次
    let v2 = f();

    assert_eq!(2, v);
    assert_eq!(3, v2);
    assert_eq!(11, i);
}
```

#### **类型没有实现`Copy`，闭包中是`&T`操作**

如下的代码，f闭包对s变量，没有修改操作，此处捕获到的是`&s`，f按照`Fn` trait方式执行，可以多次执行f。

```
fn test_fn_string() {
    let mut s = "Hello".to_owned();
    let f = || -> String {
        dbg!(&s);
        "world".to_owned()
    };

    // f闭包对s是immutable borrowed，是Fn trait
    let v = f();

    // f闭包中是immutable borrowed，此处是第二个immutable borrowed。
    dbg!(&s);

    // f可以调用多次
    let v2 = f();

    // f闭包生命周期结束，s已经没有borrowed，所以此处可以mutable borrowed
    s += " moto";

    assert_eq!("world", &v);
    assert_eq!("world", &v2);
    assert_eq!("Hello moto", &s);
}
```

#### **类型没有实现`Copy`，闭包中是`&mut T`操作**

如下的代码，f闭包对s变量，调用`push_str(&mut self, &str)`方法修改，此处捕获到的是`&mut s`，f是按照`FnMut` trait方式执行，注意f本身是`mut`，f可以多次执行f。

```
fn test_fn_mut_string() {
    let mut s = "Hello".to_owned();
    let mut f = || -> String {
        s.push_str(" world");
        s.clone()
    };

    // f闭包对s是mutable borrowed，是FnMut trait
    let v = f();

    // s是mutable borrowed，此处不能再borrowed。
    // dbg!(&s);

    // f可以多次调用
    let v2 = f();

    // f闭包生命周期结束，s已经没有borrowed，所以此处可以mutable borrowed
    s += " moto";

    assert_eq!("Hello world", &v);
    assert_eq!("Hello world world", &v2);
    assert_eq!("Hello world world moto", &s);
}
```

#### **类型没有实现`Copy`，闭包使用`move`关键字，闭包中是`&mut T`操作**

如下的代码，f闭包对s变量，调用`push_str(&mut self, &str)`方法修改，闭包使用`move`关键字，s被move进闭包，s没有被消耗，f是按照`FnMut` trait方式执行，注意f本身是`mut`，f可以多次执行。

```
fn test_fn_mut_move_string() {
    let mut s = "Hello".to_owned();
    let mut f = move || -> String {
        s.push_str(" world");
        s.clone()
    };

    // s被move进f闭包中，s没有被消耗，是FnMut trait
    let v = f();

    // s被move进闭包，s不能被borrowed
    // dbg!(&s);

    // f可以多次调用
    let v2 = f();

    // s被move进闭包，s不能被borrowed，但是可以绑定新实例
    s = "moto".to_owned();

    assert_eq!("Hello world", &v);
    assert_eq!("Hello world world", &v2);
    assert_eq!("moto", &s);
}
```

#### **类型没有实现`Copy`，闭包中是`&mut T`操作，捕获的变量被消耗**

如下的代码，f闭包对s变量，调用`push_str(&mut self, &str)`方法修改，s被闭包消耗，此处捕获到的是s本身，s被move到闭包中，闭包外部s就不可见了。f是按照`FnOnce` trait方式执行，不可以多次执行f。

```
fn test_fn_once_string() {    let mut s = "Hello".to_owned();    let f = || -> String {        s.push_str(" world");        s   // s被消耗    };    // s被move进f闭包中，s被消耗，是FnOnce trait    let v = f();    // s变量已经被move了，不能再被borrowed    // dbg!(&s);        // f只能调用一次    // let v2 = f();    // s被move进闭包，s不能被borrowed，但是可以绑定新实例    s = "moto".to_owned();    assert_eq!("Hello world", v);    assert_eq!("moto", &s);}
```

#### **类型没有实现`Copy`，闭包使用`move`关键字，闭包中是`T`操作，捕获的变量被消耗**

如下的代码，f闭包对s变量，调用`into_boxed_str(self)`方法，s被闭包消耗，此处捕获到的是s本身，s被move到闭包中，闭包外部s就不可见了。f是按照`FnOnce` trait方式执行，不可以多次执行f。

本例中`move`关键字不是必须的。

```
fn test_fn_once_move_string() {
    let mut s = "Hello".to_owned();
    let f = move || s.into_boxed_str();

    // s被move进f闭包中，s被消耗，是FnOnce trait
    let v = f();

    // s变量已经被move了，不能再被borrowed
    // dbg!(&s);
    
    // f只能调用一次
    // let v2 = f();

    // s被move进闭包，s不能被borrowed，但是可以绑定新实例
    s = "moto".to_owned();

    assert_eq!("Hello", &*v);
    assert_eq!("moto", &s);
}
```

### **最后总结**

move、copy、clone、drop和闭包捕获是rust中基本的概念，代码过程中随时要清楚每个变量的变化。这会让自己的思路更清晰，rustc也会变得温柔驯服。

