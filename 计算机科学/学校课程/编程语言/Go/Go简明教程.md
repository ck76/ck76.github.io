[TOC]

# 1. 概述

Go 是一个开源的编程语言，它能让构造简单、可靠且高效的软件变得容易。Go是从2007年末由Robert Griesemer, Rob Pike, Ken Thompson主持开发，后来还加入了Ian Lance Taylor, Russ Cox等人，并最终于2009年11月开源，在2012年早些时候发布了Go 1稳定版本。现在Go的开发已经是完全开放的，并且拥有一个活跃的社区。

Google 的 Go 语言的设计者，在创建这门语言的时候就考虑到了这一点。由于 Google 拥有庞大的代码库，成千上万的开发人员都工作在同一个代码库上，所以对于项目外的其他开发人员而言，代码应该易于理解，并且代码间的耦合也应该尽量小。只有这样，才能使代码易于维护和修改。

Go 有意摒弃了一些现代面向对象编程语言的功能特性。

- **没有类概念**。 所有事务均使用 package 分隔，使用结构体来替代类概念。
- **不支持继承**。这使得代码易于修改。在 Java 或者 Python 之类的其他语言中，如果类ABC继承了类XYZ，当开发者对类XYZ进行修改后，则有可能会对继承XYZ的其他类产生副作用。通过摒弃继承，也使得代码更易于理解（因为在查看一段代码时，没有超类）
- **没有构造方法**。
- **没有注解**。
- **没有泛型**。
- **没有异常**。

# 2. 优缺点

## 2.1 优点

- 开源
- 编译性语言, 运行高速
- 语法简洁
- 并行处理封装
- 内存管理、数组安全

## 2.2 缺点

- 作为编译性语言调试不如脚本方便
- 在数据分析上没有脚本适用
- 对底层的控制没有基础语言灵活

# 3. 用途

常应用于搭载 Web 服务器，存储集群或类似用途的巨型中央服务器的系统编程语言。对于高性能分布式系统领域而言，Go 语言无疑比大多数其它语言有着更高的开发效率。它提供了海量并行的支持，在服务端的开发优势较大。

# 4. 语言基础

## 4.1 基本数据类型

### 4.1.1 变量和常量

普通赋值:

```go
// var 变量名称 变量类型 = 值
var num int = 1
```

平行赋值

```go
var num1,num2 int = 1, 2
```

多行赋值

```go
var (
    num1 int = 1
    num2 int = 2
)
```

### 4.1.2 整数类型的命名和宽度

Go的整数类型一共有10个其中计算架构相关的整数类型有两个:

- 有符号的整数类型 int
- 无符号的整数类型 uint
  在不同计算架构的计算机上，它们体现的宽度(存储某个类型的值所需要的空间)是不一样的。空间的单位可以是bit也可以是字节byte。



![img](https://static.studygolang.com/190927/a6639f0384843c736113e484e6556d18.png)

image

除了这两个计算架构相关的整数类型之外，还有8个可以显式表达自身宽度的整数类型:



![img](https://static.studygolang.com/190927/59e336051578f6136d1c318ed3ce19eb.png)

image

### 4.1.2 整数类型值的表示法



![img](https://static.studygolang.com/190927/c231eb2bcfb1f895b2aa51ce3e39f0e6.png)

image

如果以8进制为变量num赋值:

```
num = 039 // 用"0"作为前缀以表明这是8进制表示法
```

如果以16进制为变量num赋值:

```
num = 0x39
```

### 4.1.3 浮点数类型

浮点数类型有两个：float32/float64 浮点数类型的值一般由整数部分、小数点"."和小数部分组成。另外一种表示方法是在其中加入指数部分。指数部分由"E"或"e"以及带正负号的10进制整数表示。例:3.9E-2表示浮点数0.039。3.9E+1表示浮点数39。
有时候浮点数类型值也可以被简化。比如39.0可以被简化为39。0.039可以被简化为.039。 在Go中浮点数的相关部分只能由10进制表示法表示。

### 4.1.4 复数类型

复数类型有两个:complex64和complex128。实际上，complex64类型的值会由两个float32类型的值分别表示复数的实数部分和虚数部分。而complex128类型的值会由两个float64类型的值表示复数的实数部分和虚数部分。
负数类型的值一般由浮点数表示的实数部分、加号"+"、浮点数表示的虚数部分以及小写字母"i"组成，比如3.9E+1 + 9.99E-2i。

### 4.1.5 byte与rune

byte与rune都属于别名类型。byte是uint8的别名类型，而rune是int32的别名类型。
一个rune的类型值即可表示一个Unicode字符。一个Unicode代码点通常由"U+"和一个以十六进制表示法表示的整数表示，例如英文字母'A'的Unicode代码点为"U+0041”。
rune类型的值需要由单引号"'"包裹，不过我们还可以用另外几种方式表示:



![img](https://static.studygolang.com/190927/07b54d588437f8df20d3e7555aadedcf.png)

image

另外在rune类型值的表示中支持几种特殊的字符序列，即:转义符。如下图:



![img](https://static.studygolang.com/190927/c6daa5b5f6099f532d7fed210b4d9408.png)

image

### 4.1.6 字符串类型

字符串的表示法有两种，即：原生表示法和解释型表示法。原生表示法，需用用反引号"`"把字符序列包起来，如果用解释型表示法，则需要用双引号"""包裹字符序列。

```
var str1 string = “str”
var str1 string = `str`
```

二者的区别是，前者表示的是所见即所得的(除了回车符)。后者所表示的值中转义符会起作用。
字符串值是不可变的，如果我们创建了一个此类型的值，就不可能再对它本身做任何修改。

### 4.1.7 数组类型

一个数组是可以容纳若干相同类型的元素的容器。数组的长度是固定的。如下声明一个数组类型:

```
type MyNumbers [3]int
```

类型声明语句由关键字type、类型名称和类型字面量组成
上面这条类型声明语句实际上是为数组类型[3]int声明了一个别名类型。这使得我们可以把MyNumbers当作数组类型[3]int来使用。
我们表示这样一个数组类型的值的时候。应该把该类型的类型字面量写在最左边，然后用花括号包裹该值包含的若干元素，各元素之间以(英文半角)逗号分割，即:

```
[3]int{1,2,3}
```

现在我们把这个数组字面量赋给一个名为numbers的变量:

```
var numbers = [3]int{1,2,3}
```

这是一条变量声明语句，它在声明变量的同时为该变量赋值

另一种方式是在其中的类型字面量中省略代表其长度的数组，例：

```
var numbers = [...]int{1,2,3}
```

可以用如下方式访问该变量中的任何一个元素。例:

```
numbers[0]
numbers[1]
numbers[2]
```

如果要修改数组值中的某一个元素值，可以：

```
numbers[1] = 4
```

可以用如下方式获取数组长度:

```
var length = len(numbers)
```

如果一个数组没有赋值，则它的默认值为

```
[length]type{0,0,0…}
```

### 4.1.8 切片类型

切片(slice)与数组一样也是可以若干相同类型元素的容器。与数组不同的是切片类型的长度不确定。每个切片值都会将数组作为其底层数据结构。表示切片类型的字面量如:

```
[]int
```

或者是:

```
[]string
```

切片类型的声明可以这样:

```
type MySlice []int
```

对切片值的表示也与数组值相似

```
[]int{1,2,3}
```

操作数组值的方法同样适用于切片值。还有一种操作数组的方式叫做“切片”，实施切片操作的方式就是切片表达式。例:

```
var number3 = [5]int{1,2,3,4,5}
var slice1 = numbers3[1:4]
```

上例中切片表达式numbers3[1:4]的结果为[]int{2,3,4}很明显被切下的部分不包含元素上界索引指向的元素。实际上slice1这个切片值的底层数组正是number3的值。
我们也可以在切片值上实施切片操作：

```
var slice2 = slice1[1:3]
```

除了长度切片值以及数组值还有另外一个属性--容量。数组的容量总是等于其长度，而切片值的容量往往与其长度不同。如下图:



![img](https://static.studygolang.com/190927/8f028c1a13e77e5f90e8757ed78afe60.png)

image

如图所示，一个切片值的容量即为它的第一个元素值在其底层数组中的索引值与该数组长度的差值的绝对值。可以使用cap()内建函数获取数组、切片、通道类型的值的容量:

```
var capacity2 int = cap(slice2)
```

切片类型属于引用类型，它的零值即为nil，即空值。如果我们只声明了一个切片类型而不为它赋值，则它的默认值 nil。

切片的更多操作方法有些时候我们可以在方括号中放入第三个正整数。如下图所示:

```
numbers3[1:4:4]
```

第三个正整数为容量上界索引，它意义在于可以把作为结果的切片值的容量设置的更小。它可以限制我们通过这个切片值对其底层数组中的更多元素的访问。上节中numbers3和slice的赋值语句如下:

```
var numbers3 = [5]int{1,2,3,4,5}
var slice1 = numbers3[1:4]
```

这时，变量slice1的值是[]int{2,3,4}。但是我们可以通过如下操作将其长度延展与其容量相同:

```
slice1 = slice1[:cap(slice1)]
```

通过此操作，变量slice1的值变为了[]int{2,3,4,5}，且其长度和容量均为4。现在number3的值中的索引值在(1,5)范围内的元素都被体现在了slice1的值中。这是以number3的值是slice1的值的底层数组为前提的。这意味着我们可以轻而易举地通过切片访问其底层数组中对应索引值更大的更多元素。如果我们编写的函数返回了这样一个切片值，那么得到它的程序很可能会通过这种技巧访问到本不应该暴露给它的元素。
如果我们在切片中加入了第三个索引(即容量上限索引)，如：

```
var slice1 = numbers3[1:4:4]
```

那么在此之后，我们将无法通过slice1访问到number3的值中的第五个元素。
虽然切片值在上述方面受到了其容量的限制。但是我们可以通过另外一种手段对其进行不受限制的扩展。这需要用到内建函数append。append会对切片值进行扩展并返回一个新的切片值，使用方法如下:

```
slice1 = append(slice1, 6, 7)
```

通过上述操作，slice1的值变为了[]int{2,3,4,6,7}。一旦扩展操作超出了被操作的切片值的容量，那么该切片的底层数组就会被替换 最后一种操作切片的方式是“复制”。该操作的实施方法是调用copy函数。该函数接收两个类型相同的切片值作为参数，并把第二个参数值中的元素复制到第一个参数值中的相应位置(索引值相同)上。这里有两点需要注意:

1. 这种复制遵循最小复制原则，即：被复制的元素的个数总是等于长度较短的那个参值的长度。
2. 与append函数不同，copy函数会直接对其第一个参数值进行修改。

例:

```
var slice4 = []int{0,0,0,0,0,0}
copy(slice4, slice1)
```

通过上述复制操作，slice4会变成[]int{2,3,4,6,7,0,0}。

### 4.1.9 字典类型

Go语言的字典(Map)类型是一个哈希表的实现。字典类型的字面量如下:

```
map[K]T
```

其中，"K"为键的类型，而"T"则代表元素(值)的类型。如果我们描述一个键类型为int，值类型为string的字典类型的话：

```
map[int]string
```

字典的键类型必须是可比较的，否则会引起错误，即键不能是切片、字典、函数类型

字典值的字面量表示法实际上与数组的切片的字面量表示法很相似。最左边仍然是类型字面量，右边紧挨着由花括号包裹且有英文逗号分隔的键值对。每个键值对的键和值之间由冒号分隔。以字典类型map[int]string为例。他的值的字面量可以是这样的：

```
map[int]string{1:"a",2:"b"m,3:"c"}
```

我们可以把这个值赋给一个变量

```
mm := map[int]string{1:"a",2:"b",3:"c"}
```

可用索引表达式取出字典中的值：

```
b := mm[2]
```

可以用索引表达式赋值:

```
mm[2] = b + "2"
```

这样mm中键为2的值变为了"b2"。可以用如下方式向字典中添加一个键值对:

```
mm[4] = ""
```

对于字典值来说，如果指定键没有对应的值则默认为该类型的空值。所以mm[5]会返回一个""。但是这样的话我们就不知道mm[5]到底是""还是mm[5]没有这个值。所以go提供了另外一种写法:

```
e, ok := mm[5]
```

针对字典的索引表达式可以有两个求职结果，第二个求职结果是bool类型的。它用于表明字典值中是否存在指定的键值对。 从字典中删除键值对的方法非常简单，仅仅是调用内建函数delete：

```
delete(mm, 4)
```

无论mm中是否存在以4为键的键值对，delete都删除。 字典类型属于引用类型，它的零值即为nil

### 4.1.10 通道类型

通道(Channel)是Go语言中一种非常独特的数据结构。它可用于在不同Goroutine之间传递类型化的数据。并且是并发安全的。相比之下，之前几种数据类型都不是并发安全的。
Goroutine可以被看作是承载可被并发执行的代码块的载体。它们由Go语言的运行时系统调度，并依托操作系统线程(又称内核线程)来并发地执行其中的代码块。
通道类型的表示方法很简单，仅由两部分组成:

```
chan T
```

在这个类型字面量中，左边是代表通道类型的关键字chan,而右边则是一个可变的部分，即代表该通道类型允许传递的数据的类型(或称通道的元素类型)。
与其他的数据类型不同，我们无法表示一个通道类型的值，因此，我们无法用字面量来为通道类型的变量赋值。只能通过调用内建函数make来达到目的。make参数可接受两个参数，第一个参数是代表了将被初始化的值的类型的字面量(例: chan int),而第二个参数则是值的长度，例如,若我们想要初始化一个长度为5且元素类型为int的通道值，则需要这样写:

```
make(chan int, 5)
```

make函数也可以被用来初始化切片类型或字典类型的值。
暂存在通道值中的数据是先进先出。下面，我们声明一个通道类型的变量，并为其赋值:

```
ch1 := make(chan string, 5)
```

这样一来，我们就可以使用接受操作符<-向通道值发送数据了。当然，也可以使用它从通道值接收数据，例如，如果我们要向通道ch1 发送字符串"value1"，那么应该这样做:

```
ch1 <- “value1"
```

如果我们从ch1那里接收字符串，则要这样:

```
<- ch1
```

我们可以把接受到字符串赋给一个变量，如:

```
value := <- ch1
```

与针对字典值的索引表达式一样，针对通道值的接受操作也可以有第二个结果值:

```
value, ok := <- ch1
```

这里的ok的值是bool类型的。它代表了通道值的状态，true代表通道值有效，而false则代表通道值已无效(或称已关闭)，更深层次的原因是，如果在接受操作进行之前或过程中通道值被关闭了，则接收操作会立即结束并返回一个该通道值的元素类型的零值。
可以通过函数close来关闭通道:

```
close(ch1)
```

对通道值的重复关闭会引发运行时异常，会使程序崩溃。在通道值有效的前提下，针对它的发送操作会在通道值已满(其中缓存的数据的个数已等于它的长度)时被阻塞。而向一个已被关闭的通道值发送数据会引发运行时异常。针对有效通道值的接收操作会在它已经为空时被阻塞。通道类型属于引用类型，它的零值为nil。

## 4.2 流程控制

### 4.2.1 条件语句

对应的关键字为if、 else和else if；

```
if a := 1; a >= 1 {
    fmt.Println("OK")
}
```

### 4.2.2 选择语句

对应的关键字为switch、 case和select

```
switch i {
 case 0:
 fmt.Printf("0")
 case 1:
 fmt.Printf("1")
 case 2:
 fallthrough
 case 3:
 fmt.Printf("3")
 case 4, 5, 6:
 fmt.Printf("4, 5, 6")
 default:
 fmt.Printf("Default")
}
```

### 4.2.3 循环语句

对应的关键字为for和range；

```
sum := 0
for i := 0; i < 10; i++ {
 sum += i
}
```

### 4.2.4 跳转语句

```
func myfunc() {
 i := 0
 HERE:
 fmt.Println(i)
 i++
 if i < 10 {
 goto HERE
 }
}
```

## 4.3 函数

### 4.3.1 概述

首先函数的格式是固定的，func＋函数名＋ 参数 ＋ 返回值（可选） ＋ 函数体。例 :

```
func main（） 
{
fmt.Println("Hello go")
}
```

在golang中有两个特殊的函数，main函数和init函数，main函数不用介绍在所有语言中都一样，它作为一个程序的入口，只能有一个。init函数在每个package是可选的，可有可无，甚至可以有多个(但是强烈建议一个package中一个init函数)，init函数在你导入该package时程序会自动调用init函数，所以init函数不用我们手动调用,另外它只会被调用一次，因为当一个package被多次引用时，它只会被导入一次。

### 4.3.2 参数传递

- 普通变量
  使用普通变量作为函数参数的时候，在传递参数时只是对变量值得拷贝，即将实参的值复制给变参，当函数对变参进行处理时，并不会影响原来实参的值。
- 指针
  函数的变量不仅可以使用普通变量，还可以使用指针变量，使用指针变量作为函数的参数时，在进行参数传递时将是一个地址看呗，即将实参的内存地址复制给变参，这时对变参的修改也将会影响到实参的值。
- 数组
  和其他语言不同的是，go语言在将数组名作为函数参数的时候，参数传递即是对数组的复制。在形参中对数组元素的修改都不会影响到数组元素原来的值。
- slice, map, chan
  在使用slice, map, chan 作为函数参数时，进行参数传递将是一个地址拷贝，即将底层数组的内存地址复制给参数slice, map, chan 。这时，对slice, map, chan 元素的操作就是对底层数组元素的操作。
- 函数名字
  在go语言中，函数也作为一种数据类型，所以函数也可以作为函数的参数来使用。

### 4.3.3 返回值

go语言可以返回局部变量的指针，因为go语言的回收机制是当销毁栈上的临时数据且发现有被外部引用的栈上变量时，会自动转移到堆上。

### 4.3.4 闭包

和其他语言类似，golang也支持闭包函数

```
package main

import "fmt"

func adder() func(int) int {
     sum := 0
     return func(x int) int {
          sum += x
          return sum
     }
}

func main() {
     pos, neg := adder(), adder()
     for i := 0; i < 10; i++ {
          fmt.Println(
               pos(i),
               neg(-2*i),
          )
     }
}
```

## 4.4 类

### 4.4.1 概述

```
type Poem struct {
    Title  string
    Author string
    intro  string
}
```

这样就声明了一个类，其中没有public、protected、private的的声明。**golang用另外一种做法来实现属性的访问权限：属性的开头字母是大写的则在其它包中可以被访问，否则只能在本包中访问。类的声明和方法亦是如此**。

### 4.4.2 方法

```
func (poem *Poem) publish() {
    fmt.Println("poem publish")
}
```

或者

```
func (poem Poem) publish() {
    fmt.Println("poem publish")
}
```

和其它语言不一样，golang声明方法和普通方法一致，只是在func后增加了poem *Poem这样的声明。加*和没有加*的区别在于一个是传递指针对象，一个是传递值对象。

注意当创建了方法1后将会默认创建出方法2,反之则不会。

### 4.4.3 类的实例化

实例化对象有好几种方式

```
poem := &Poem{}

poem.Author = "Heine"

poem2 := &Poem{Author: "Heine"}

poem3 := new(Poem)

poem3.Author = "Heine"

poem4 := Poem{}

poem4.Author = "Heine"

poem5 := Poem{Author: "Heine"}
```

实例化的时候可以初始化属性值，如果没有指明则默认为系统默认值。

### 4.4.4 伪继承

golang中不存在继承，但可以使用组合的方式达到类似的效果.

```
func (e *Poem) ShowTitle() {
    fmt.Printf(e.Title)
}
type Poem struct {
    Title  string
    Author string
    intro  string
}

type ProsePoem struct {
    Poem
    Author string
}
```

ProsePoem属性中声明了Poem，表示组合了Poem的属性和方法**(属性和方法都会被继承)**。

```
prosePoem := &ProsePoem{
        Poem: Poem{
            Title:  "Jack",
            Author: "slow",
            intro:  "simple",
        },
        Author: "test",
    }
```

**如果其中属性有冲突，则以外围的为主，也就是说会被覆盖。同样类方法出现冲突时也会覆盖，并不会支持重载**

```
type ProsePoem struct {
    Poem
    Author string
}
```

**当访问Author的时候默认为ProsePoem的Author，如果需要访问Poem的Author属性可以使用**
**prosePoem.Poem.Author来访问方法同理**。

```
prosePoem := &ProsePoem{}

prosePoem.Author = "Shelley"

prosePoem.Poem.Author = "Heine"

fmt.Println(prosePoem)
```

从输出中可以很直观看到这一点。

```
&{{ Heine } Shelley}
```

## 4.5 接口

Golang的interface是方法的集合也是一种类型

```
package main

import "fmt"

type Animal interface {
    Speak() string
}

type Cat struct{}
func (c Cat) Speak() string {
    return "cat"
}

type Dog struct{}
func (d Dog) Speak() string {
    return "dog"
}

func Test(params interface{}) {
    fmt.Println(params)
}

func main() {
    animals := []Animal{Cat{}, Dog{}}
    for _, animal := range animals {
        fmt.Println(animal.Speak())
    }

    Test("string")
    Test(123)
    Test(true)
}
```

如上所示，将Struct 向 Interface变量赋值，实际作用是Interface获取了Struct中方法的实现集合。

## 4.6 断言和反射

断言是预判确认变量的类型

```
func main() {
    v := "hello world"
    fmt.Println(typeof(v))
}
func typeof(v interface{}) string {
    switch t := v.(type) {
    case int:
        return "int"
    case float64:
        return "float64"
    //... etc
    default:
        _ = t
        return "unknown"
    }
}
```

反射则是因为interface中实际记录的有存储对象的类型，可以提取出变量的类型

```
import (
    "reflect"
    "fmt"
)
func main() {
    v := "hello world"
    fmt.Println(typeof(v))
}
func typeof(v interface{}) string {
    return reflect.TypeOf(v).String()
}
```

## 4.7 回收处理

在golang当中，defer代码块会在函数调用链表中增加一个函数调用。这个函数调用不是普通的函数调用，而是会在函数正常返回，也就是return之后添加一个函数调用。因此，defer通常用来释放函数内部变量。

如下示例

```
func CopyFile(dstName, srcName string) (written int64, err error) {
src, err := os.Open(srcName)
if err != nil {
return
}
defer src.Close()

dst, err := os.Create(dstName)
if err != nil {
return
}
defer dst.Close()

return io.Copy(dst, src)
}
```

通过defer，我们可以在代码中优雅的关闭/清理代码中所使用的变量。defer作为golang清理变量的特性，有其独有且明确的行为。以下是defer三条使用规则。

- 当defer被声明时，其参数就会被实时解析
  即defer的参数的变量值，在代码中defer使用后的位置改变并不会对改变defer用到的值
- defer执行顺序为先进后出
  在函数中，先定义的defer将会后执行
- defer可以读取有名返回值
  defer代码块的作用域仍然在函数之内，因此defer仍然可以读取函数内的变量

## 4.8 make和new的区别

new 的作用是初始化一个指向类型的指针(*T)，make 的作用是为 slice，map 或 chan 初始化并返回引用(T)。

## 4.9 异常处理

对于异常处理，golang提供了recover和panic。

如下示例

```
package main
 
import (
    "fmt"
)
 
func main() {
    defer func() {
        fmt.Println("1")
    }()
    defer func() {
        if err := recover(); err != nil {
            fmt.Println(err)
        }
    }()
    panic("fault")
    fmt.Println("2")
}
 
运行结果：
fault
1
```

程序首先运行panic，出现故障，此时跳转到包含recover()的defer函数执行，recover捕获panic，此时panic就不继续传递．但是recover之后，程序并不会返回到panic那个点继续执行以后的动作，而是在recover这个点继续执行以后的动作，即执行上面的defer函数，输出１.

利用recover处理panic指令，必须利用defer在panic之前声明，否则当panic时，recover无法捕获到panic，无法防止panic扩散．

## 4.10 协程

Go调度的几个概念：

M：内核线程；
G：go routine，并发的最小逻辑单元，由程序员创建；
P：处理器，执行G的上下文环境，每个P会维护一个本地的go routine队列；



![img](https://static.studygolang.com/190927/f19c0608b3bda1fa5179412c45a5a307.png)

image.png

除了每个P拥有一个本地的go routine队列外，还存在一个全局的go routine队列。

具体调度原理：

1. P的数量在初始化由GOMAXPROCS决定；
2. 我们要做的就是添加G；
3. G的数量超出了M的处理能力，且还有空余P的话，runtime就会自动创建新的M；
4. M拿到P后才能干活，取G的顺序：本地队列>全局队列>其他P的队列，如果所有队列都没有可用的G，M会归还P并进入休眠；

一个G如果发生阻塞等事件会进行阻塞，如下图：



![img](https://static.studygolang.com/190927/0ac890708a97a7be7c28b6a208bac660.png)

image.png

G发生上下文切换条件：

- 系统调用；
- 读写channel；
- gosched主动放弃，会将G扔进全局队列；

如上图，一个G发生阻塞时，M0让出P，由M1接管其任务队列；当M0执行的阻塞调用返回后，再将G0扔到全局队列，自己则进入睡眠（没有P了无法干活）；

下面例子说明协程的使用

```
package main

import (
    "fmt"
    "runtime"
)

 
func say(s string) {
    for i := 0; i < 2; i++ {
        runtime.Gosched()
        fmt.Println(s)
    }   
}

func main() {
    go say("world")
    say("hello")
}
```

输出结果

```
输出结果：

hello
world
hello
```

1、先输出了hello,后输出了world.

2、hello输出了2个，world输出了1个（因为第2个hello输出完，主线程就退出了，第2个world没机会了）

把代码中的runtime.Gosched()注释掉，执行结果是：

```
hello
hello
```

因为say("hello")这句占用了时间，等它执行完，线程也结束了，say("world")就没有机会了。

这里同时可以看出，go中的goroutins并不是同时在运行。事实上，如果没有在代码中通过

runtime.GOMAXPROCS(n) 其中n是整数，

指定使用多核的话，goroutins都是在一个线程里的，它们之间通过不停的让出时间片轮流运行，达到类似同时运行的效果。

## 4.11 包管理

### 4.11.1 package 的导入语法

写 Go 代码的时经常用到 import 这个命令用来导入包，参考如下：

```
import( "fmt" )
```

然后在代码里面调用该包的方法：

```
fmt.Println( "hello go" )
```

fmt 是 Go 的标准库，它其实是去 GOROOT 下去加载该模块，当然 Go 的 import 还支持如下两种方式来加载自己写的模块：

- 相对路径
  import "./model" // 当前文件同一目录的 model 目录，但是不建议这种方式 import
- 绝对路径
  import "shorturl/model" // 加载 GOPATH/src/shorturl/model 模块

### 4.11.2 package 的导入的特殊用法

上面展示了一些 import 常用的几种方式，但是还有一些特殊的 import ，让很多新手很费解，下面是三种导入包的使用方法。

**点操作**

有时候会看到如下的方式导入包：

```
import( 
    . "fmt" 
) 
```

这个点操作的含义就是这个包导入之后在你调用这个包的函数时，你可以省略前缀的包名，也就是前面你调用的：

```
fmt.Println( "hello go" )
```

可以写成：

```
Println( "hello go" )
```

** 别名操作**
别名操作顾名思义可以把包命名成另一个用起来容易记忆的名字：

```
import( 
    f "fmt" 
) 
```

别名操作调用包函数时前缀变成了重命名的前缀，即：

```
f.Println( "hello go" )
```

**下划线操作**
这个操作经常是让很多人费解的一个操作符，请看下面这个 import

```
import ( 
    _ “github.com/xiyanxiyan10/misakago" 
)
```

下划线操作并不想引用该包中的方法等，只是希望导入包时自动执行其中的Init()函数来做需要的初始化工作。





<https://studygolang.com/articles/13958>