[TOC]

### 维基百科定义

**维基百科：（lambda表达式）**

> lambda表达式会重定向至匿名函数词条：
>
> 在计算机编程中，**匿名函数**（英语：anonymous function）是指一类无需定义[标识符]（函数名）的[函数]或[子程序]，普遍存在于多种编程语言中。

**维基百科：（闭包）**

> 在计算机科学中，闭包（英语：Closure），又称词法闭包（Lexical Closure）或函数闭包（function closures），**是引用了自由变量函数**。这个被引用的**自由变量将和这个函数一同存在，即使已经离开了创造它的环境也不例外。**

lambda表达式是lambda表达式，闭包是闭包，闭包一词经常和匿名函数混淆。这可能是因为两者经常同时使用，但是它们是不同的概念。



### 一、lambda表达式【匿名函数】

> **lambda本质就是一个匿名函数，例如 λx.x + 2**
>
> **lambda表达式的闭包是其环境中定义的一个子集，它给包含在lambda表达式中的自由变量赋值，从而有效的关闭表达式。**
>
> **lambda表达式的闭包是定义在外部上下文（环境）中特定的符号集，它们给这个表达式中的自由符号赋值。它将一个开放的、仍然包含一些未定义的符号lambda表达式变为一个关闭的lambda表达式，使这个lambda表达式不再具有任何自由符号。**

lambda和闭包最早可以追溯到lambda演算，lambda演算是上世纪30年代由Alonzo Church创造的，而我们就从这里开始说起。

lambda演算可以说是一种最简单的编程语言，你只可以用它来做的唯一的事情是：

- **应用**：将一个表达式应用到另一个表达式，表示f x。（把它当作是函数调用，其中f是函数，x是它的唯一参数）
- **抽象**：它可以绑定一个符号，改符号可以看作是一个“插槽”、空白的框、或者说一个“变量”。它是用希腊字母λ（lambda）加上符号名称（例如x）跟着一个点，最后加上一个表达式组成。然后将表达式转换为期望一个参数的函数。
  例如：`λx.x + 2`表示包含一个`x + 2`的表达式，并且表示表达式中符号x是一个绑定变量（bound variable），它可以用你提供的值作为参数来替换。
  注意，这种方式定义的函数是匿名的，所以你还不能引用它，但是你可以立即调用它(看应用的定义)，方式是提供它正在等待的参数，比如这样：`（λx.x+ 2）7`。然后，用表达式7（这种情况下是个字面值）被替换到x，作用在lambda子表达式`x + 2`上，所以你得到 `7 + 2`，最后你通过简单的算术规则得到9。

所以，我们解决了一个谜题：
**lambda本质就是一个匿名函数，例如上面的λx.x + 2**

在不同的编程语言中，函数抽象的语法可能不同，在JavaScript中是这样的：

```
function(x) { return x+2; }
```

现在你可以立即将参数应用到它，就像这样：

```javascript
function(x) { return x+2; } (7)
```

或者你可以将这个匿名函数（lambda）存储到某个变量：

```javascript
var f = function(x) { return x+2; }
```

给他一个有效的名字`f`，允许引用它并在后面多次调用它，例如：

```javascript
alert(  f(7) + f(10)  );   // should print 21 in the message box
```

但是你没有闭包给它命名，你可以立即调用它：

```javascript
alert(  function(x) { return x+2; } (7)  );  // should print 9 in the message box
```

在LISP中，lambdas是这样定义的：

```javascript
(lambda (x) (x + 2))
```

然后你可以立即将它应用于一个参数调用：

```javascript
( (lambda (x) (+ x 2)) 7 )
```

#### 1、实例

- python

```python
# 以下两种相等同
# 1.不使用匿名函数
def f(x):
	return x * x
# 2.使用匿名函数
lambda x: x * x
```
- javascript
```javascript
alert((function(x){
	return x*x;
})(10)); // 提示100
```

#### 2、与闭包

- lambda函数可以捕获lambda函数外的具有automatic storage duration的变量，即函数的局部变量与函数形参变量。函数体与这些变量的集合合起来称做[闭包](https://zh.wikipedia.org/wiki/%E9%97%AD%E5%8C%85_(%E8%AE%A1%E7%AE%97%E6%9C%BA%E7%A7%91%E5%AD%A6))。
- 如果一个变量声明在一个函数内部，该变量只会存在于声明范围内的栈存储空间中。因此，函数在返回的时候，这个本地变量也会同时被从该函数的的栈内存空间中清除掉了，**也就是不可达了**，也就是说，这种情况下，当你在lambda表达式中使用了本地变量的话，该变量变得无法获取，为了解决此问题，当一个依赖了本地变量的lambda表达式需要从函数中返回出去，编译器就会创建一个**闭包**，闭包中含有lambda表达式，并为每个被引用的本地变量的值复制到响应的字段当中，以便扩展这些变量的生命周期

#### 3、好处

- Kotlin的lambda表达式以**更加简洁易懂的语法实现功能**，使开发者从原有冗余啰嗦的语法声明解放出来。**可以使用函数式编程中的过滤、映射、转换等操作符处理集合数据**，从而使你的代码更加接近函数式编程的风格。
- Java8以下的版本不支持Lambda表达式，而Kotlin则兼容与Java8以下版本有很好互操作性，非常适合Java8以下版本与Kotlin混合开发的模式。解决了Java8以下版本不能使用lambda表达式瓶颈。
- 在Java8版本中使用Lambda表达式是有些限制的，它不是真正意义上支持闭包，而Kotlin中lambda才是真正意义的支持闭包实现。
- **归根结底还是因为java不是函数式的，是用内部类实现的lambda表达式，还有java是值传递的，所以导致java不支持闭包**



### 二、闭包【引用了自由变量的函数】

> 一种特殊的函数，绑定了函数内部引用的所有变量，把它引用的东西都放在一个上下文中“包”了起来。百科定义：**包含两层含义要执行的代码块（自由变量以及自由变量引用的对象）和自由变量的作用域。**

什么是闭包。为了做到这一点，我们来谈谈**lambda表达式中的符号（变量）**。

正如我所说，lambda 抽象的的做法是在它的子表达式中绑定一个符号，以便它称为一个可替代的参数。这样的符号称为约束的（bound）。但是如果表达式中还有其他符号呢？例如`λx.x/y+2`。这这个表达式中，符号x是被lambda抽象λx约束的。但是另一个符号y不受限制，他是自由的。我们不知道它是什么以及它来自哪里，所以我们不知道它代表什么，有什么价值，因此我们不能评估（evaluate）这个表达式直到我们找出y代表的意义。

事实上，与其他两个符号2和+一样。知识我们对这两个符号非常熟悉，通常会忘记计算机并不知道它们，我们需要通过在某处定义它们来告诉计算机它们的含义。例如，在库或在语言本身。

你可以想象自由符号是定义在表达式外面的地方，在它“周围的语境”（“surrounding context”）中，这称为环境（environment）。环境可能是一个更大的表达式，这是表达式的一部分。或者是在某个库，或者在语言本身（作为原生的）。

这让我们将lambda表达式分为两类：

- **CLOSED expressions：**这些表达式中出现的每一个符号都受到一些lambda抽象的约束。换句话说，它们是自己自足的，不需要评估任何周边语境。它们也被称为Combinators。
- **OPEN expressions：**这些表达式中的某些符号没有约束，也就是说，**它们中的一些符号是自由的，它们需要一些外部信息**，因此只有在提供这些符号的定义后才能对它们进行评估。

你可以通过提供一个环境来关闭一个开放的lambda表达式，该环境通过将所有的自由符号绑定到某些值（可能是数字，字符串，匿名函数或者说lambda等）来定义它们。

然后这里是闭包的部分：
**lambda表达式的闭包是定义在外部上下文（环境）中特定的符号集，它们给这个表达式中的自由符号赋值。它将一个开放的、仍然包含一些未定义的符号lambda表达式变为一个关闭的lambda表达式，使这个lambda表达式不再具有任何自由符号。**

例如：你有一下这个lambda表达式：`λx.x/y+2`，符号x是受约束的，而y是自由的，因此这个表达式是开放的并且是不能被评估的，除非你说出y的意思（+和2也一样，是自由的）。假设你有一个环境，如下：

```javascript
{  y: 3,  +: [built-in addition],  2: [built-in number],  q: 42,  w: 5  }
```

这个环境为我们的lambda表达式提供了所有未定义（自由）的符号`y, + , 2`，还有一些额外的符号`q, w`。而我们需要定义的是这个环境的子集：

```javascript
{  y: 3,  +: [built-in addition],  2: [built-in number]  }
```

然后**这个子集正是我们lambda表达式的闭包**。

换句话说，**它关闭了一个开放的lambda表达式**，这就是closure这个术语一开始出现的地方。还有这就是为什么关于这个问题很多人的回答都不是很正确的原因。

#### 1、形成条件

> **维基百科：**
>
> 在一些语言中，在函数中可以（嵌套）定义另一个函数时，如果内部的函数引用了外部的函数的变量，则可能产生闭包。运行时，**一旦外部的 函数被执行，一个闭包就形成了**，闭包中包含了内部函数的代码，以及所需**外部函数中的变量的引用**。其中所引用的变量称作**上值**(*upvalue*)。

- 闭包更多强调的是**引用环境**, 函数在定义时因为使用到了自由变量, 函数的**调用和定义时的引用环境不同**, 所以它必须要清楚的了解到自己定义时的引用环境, **在调用的时候需要切换到它定义的引用环境中执行, 这样就形成了闭包.**

- 更通俗的讲, 因为函数引用了**自由变量,** 当它执行的环境和定义的环境不同时, 在执行它的地方就不仅仅是拿到这个函数这么简单了, 因为**它必须要清楚它引用的自由变量**, **所以最好的方式是把此时的引用环境和函数本身一并返回, 这样就是闭包.**

- 所以, 闭包 = **函数 + 引用环境**

```java
分解一下就是闭包形成的必要条件

1. **函数引用自由变量**
2. **函数的执行环境和自由变量的声明环境不同**
```

只有符合了以上两点, **函数的执行点**才不仅仅需要函数本身, 而是**函数+引用环境.** 

两个重点是**自由变量**, **引用环境**, 但就这几个字可能对于一些对闭包一知半解的人来说确是很难理解, 因为在大部分人印象里闭包应该是这样的.

```kotlin
// kotlin
list.map { println it}

// groovy
android {
  compileSdkVersion 24
}
```

这样的一个实例中没有任何地方提及到自由变量. 其实这压根和闭包没有任何关系, 这仅仅是一种语法-lambda表达式, 很多人将lambda表达式想当然的认为是闭包. 再来看看下面语法.

对于搞android的人来说这行代码并不陌生, 很多人把这个也理解成闭包, 和上面kotlin的代码一样, 其实这和闭包也没有任何关系.

那什么样的才算闭包? 上面提到了, 引用了自由变量的函数, 这句话里有一个名词-自由变量, 那什么又是自由变量呢? 理解了这个名词后才能继续理解这句话: **在某个作用域内使用了其他作用域声明的变量, 那该变量就是自由变量**

```javascript
// javascript
var a = 10;
function add5() {
  return a + 5;
}
```

上面的变量a即为自由变量, a变量不是在函数add5的作用域中声明的, 却在函数add5中被使用.

好了, 知道了什么是自由变量, 那下面我们来看看什么是闭包.

```javascript
// javascript
function add(a) {
  return function(b) {
    return a + b;
  };
}

var add10 = add(10);
var result = add10(5);
```

上面的例子是最典型的闭包, add函数返回了一个函数, 这个匿名函数引用了add函数的一个a变量, 所以a变量是一个自由变量. 而且, 这个匿名函数的执行点不在变量a声明的作用域内, 根据上面的概念, 这样就形成了闭包.

#### 2、闭包的作用

- 因为闭包只有在被调用时才执行操作即“[惰性求值]”，所以它可以被用来定义控制结构。
- 多个函数可以使用一个相同的环境，这使得它们可以通过改变那个环境相互交流。
- 通过概念和实例代码, 很明显闭包的存在改变了变量的生命周期, **大部分情况下它可以将自由变量的生命周期延迟到闭包函数的执行**, **而函数式中最重要的一个思想是尽可能多使用纯函数(纯函数是指对于相同的输入必定有相同的输入的函数)**, 在纯函数中如果想要保持一个变量, 那闭包肯定是最佳选择. 来看一下实例.

```javascript
// javascript
function nameBy(lastName) {
  return function(firstName) {
    retrn firstName + " " + lastName;
  }
}

var group = nameBy("Jordan")
var michael = group("Michael")
var susan = group("Susan")
```

#### 3、闭包的实现

典型实现方式是定义一个特殊的数据结构，保存了**函数地址指针**与闭包**创建时的函数的词法环境表示**（那些非局部变量的绑定）。使用[函数调用栈](https://zh.wikipedia.org/wiki/%E8%B0%83%E7%94%A8%E6%A0%88)的语言实现闭包比较困难，因而这也说明了为什么大多数实现闭包的语言是基于[垃圾收集](https://zh.wikipedia.org/wiki/%E5%9E%83%E5%9C%BE%E6%94%B6%E9%9B%86)机制——当然，不使用垃圾收集也可以做到。

闭包的实现与[函数对象](https://zh.wikipedia.org/wiki/%E5%87%BD%E6%95%B0%E5%AF%B9%E8%B1%A1)很相似。

通过将[自由变量](https://zh.wikipedia.org/wiki/%E8%87%AA%E7%94%B1%E5%8F%98%E9%87%8F)放进参数表、并扩大函数名字的作用域，可以把一个闭包 / 匿名 / 内部函数变成一个普通的函数，这叫做“[Lambda 提升](https://zh.wikipedia.org/w/index.php?title=Lambda_%E6%8F%90%E5%8D%87&action=edit&redlink=1)”。

#### 4、科里化

> 柯里化（英语：Currying），是把接受多个参数的函数变换成接受一个单一参数（最初函数的第一个参数）的函数，并且返回接受余下的参数而且返回结果的新函数的技术。

其实上面的大部分例子中已经实现了柯里化, 柯里化的好处就是大大的提高了函数的灵活性.

```javascript
// javascript
var add = (x, y) => x + y

// user
alert(add(10, 20))
alert(add(10, 30))
alert(add(10, 40))
```

上面对add函数的调用其实都是10+y的形式, 很多时候我们为了封装, 又对10+y这样的式子进行封装.

```javascript
// javascript
var add = (x, y) => x + y
var add10 = (y) => 10 + y

// user
alert(add10(20))
alert(add10(30))
alert(add10(40))
```

这里有一个不好的地方就是add10这个函数的封装只能适用于10+y, 虽然实现了柯里化, 但是对于使用者来说灵活性不够, 其实这里我们可以利用闭包对add函数稍加改造, 既方便使用又不失灵活性

```javascript
// javascript
var add = (x) => (y) => x + y

// user
var add10 = add(10)
alert(add10(20))
alert(add10(30))
alert(add10(40))
```



### 三、为什么会混淆lambda表达式和闭包

那么，他们为什么是错误的？为什么很多人说闭包是内存中的一些数据结构，或者是他们使用的语言的一些功能？或者为什么他们将闭包与lambdas混淆？

那么，Sun / Oracle，微软，Google等企业（ corporate marketoids）就应该收到指责，因为他们说这些是他们语言的结构（Java，C#，GO 等）。他们经常叫“闭包”，这应该只是lambda表达式（译者：没错，什么ruby，groovy就让我以为lambda表达式就是闭包，真的好气）。或者称“闭包”是他们用来实现词法作用域的一种特定技术，即一个函数可以访问它作用域外定义的变量。他们经常说函数“封闭（encloses）”这些变量，即捕获他们到某些数据结构去存储它们，防止它们在外部函数执行完后被销毁。但是这些只是他们民俗语言和营销，这只会让事情更加混乱，因为每一个语言供应商都使用它们自己的术语。

而且更糟糕的是，因为它们所说的话总是有一点是真实的，这让你不允许轻易的将其视为假的。让我解释一下：

如果你想实现一种使用lambdas作为一等公民（first-class citizens）的语言，你需要允许它们使用在其上下文中定义的符号（即在你的lambda中使用的那些自由变量）。即使周围的函数已经返回，这些符号也必须存在。问题是这些符号是绑定在函数的本地存储（通常是在调用栈上），当函数返回时将不再存在。因此，为了让lambda按照你期望的方式工作，你需要以某种方式从外部上下文中“捕获”所有这些自由变量并且保存它们，即使外部上下文将消失。也就是说，你需要找到该lambda表达式的闭包（所有使用到的白雾变量）并且将它存储在其他地方（建立一个副本，或者提前准备一个除了栈以外的空间给它们）。你用来实现这个目标的实际方法就是你语言的“实现细节”。这里最重要的时闭包，它是一组从环境中获取的自由变量，你需要在保存在某处。

人们开始调用他们语言中使用的实际的数据结构来实现“闭包”并没有花太长时间。该结构通常看起来是这样的：

```groovy
Closure {
   [pointer to the lambda function's machine code],
   [pointer to the lambda function's environment]
}
```

并且这些数据结构可以作为参数传递给其他函数、作为函数返回、保存到变量、代表lambdas，并允许通们它们访问其封闭环境以及在该上下文中运行的机器码（machine code）。但这只是实现闭包的其中一种方式，而不是闭包本身。（译者：例如groovy就用一个Closure类来实现闭包）

正如我上面所解释的那样，**lambda表达式的闭包是其环境中定义的一个子集，它给包含在lambda表达式中的自由变量赋值，从而有效的关闭表达式。**（**将一个开放的还不能评估的lambda表达式，转换成一个关闭的lambda表达式，然后可以进行评估，因为它包含的所有符号现在都已定义**）。

其他任何东西都只是程序员和语言供应商们的“巫术”和“诅咒魔术”，并不知道这些概念的真正根源。（译者：原文"cargo cult" and "voo-doo magic"，科学家为了描述那些缺乏科学严谨的研究也创造了这些词汇：巫毒科学（voodoo science）和巫术科（cargocult science））



### 四、Java中的lambda表达式和闭包

> 闭包：**可以读取其他函数内部变量的函数，作为链接函数外部和内部的桥梁**

> lambada表达式：**lambda本质就是一个匿名函数**

- 闭包就是把函数以及变量包起来，连接函数内部和外部并且使得变量的生存周期延长。闭包跟面向对象是一棵树上的两条枝，实现的功能是等价的。

- 这样说可能不够直观，我们还是用代码说话吧。其实Java在很早的版本就支持闭包了，只是因为应用场景太少，这个概念一直没得到推广。在Java6里，我们可以这样写：

```java
public static Supplier<Integer> testClosure(){
    //函数局部变量
 	final int i = 1;
 		return new Supplier<Integer>() {
 			@Override
			 public Integer get() {
                 //返回函数局部变量i，使得外部可用
				 return i;
 			 }
		   };
	}

public interface Supplier<T> {
 	T get();
}
```

看出问题了么？这里i是函数testClosure的内部变量，但是最终返回里的匿名对象里，仍然返回了i。我们知道，函数的局部变量，**其作用域仅限于函数内部，在函数结束时，就应该是不可见状态，而闭包则将i的生存周期延长了，并且使得变量可以被外部函数所引用。这就是闭包了。**这里，其实我们的lambda表达式还没有出现呢！

而支持lambda表达式的语言，一般也会附带着支持闭包了，因为lambda总归在函数内部，与函数局部变量属于同一语句块，如果不让它引用局部变量，不会让人很别扭么？例如Python的lambda定义我觉得是最符合λ算子的形式的，我们可以这样定义lambda：

```python
#!/usr/bin/python
y = 1
f=lambda x: x + y
print f(2)
y = 3
print f(2)
输出： 
3
5
```

这里y其实是外部变量。

#### 1、Java中闭包带来的问题

在Java的经典著作《[Effective Java](http://www.amazon.com/gp/product/B000WJOUPA/ref=as_li_qf_sp_asin_il_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=B000WJOUPA&linkCode=as2&tag=job0ae-20)》、《[Java Concurrency in Practice](http://www.amazon.com/gp/product/0321349601/ref=as_li_qf_sp_asin_il_tl?ie=UTF8&tag=job0ae-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0321349601)》里，大神们都提到：**匿名函数里的变量引用，也叫做变量引用泄露，会导致线程安全问题，因此在Java8之前，如果在匿名类内部引用函数局部变量，必须将其声明为final，即不可变对象。**(Python和Javascript从一开始就是为单线程而生的语言，一般也不会考虑这样的问题，所以它的外部变量是可以任意修改的)。

- 内部类里面使用外部类的局部变量时，其实就是内部类的对象在使用它，内部类对象生命周期中都可能调用它，而**内部类试图访问外部方法中的局部变量时，外部方法的局部变量很可能已经不存在了，**那么就得延续其生命，拷贝到内部类中，**而拷贝会带来不一致性**，从而需要使用final声明保证一致性。说白了，内部类会自动拷贝外部变量的引用，为了避免：
  1. 外部方法修改引用，而导致内部类得到的引用值不一致 
  2. 内部类修改引用，而导致外部方法的参数值在修改前和修改后不一致。于是就用 final 来让该引用不可改变。

在Java8里，有了一些改动，现在我们可以这样写lambda或者匿名类了：

```java
public static Supplier<Integer> testClosure() {
 int i = 1;
 //lambda表达式   
 return () -> {
 	return i;
 };
}
```

这里我们不用写final了！但是，Java大神们说的引用泄露怎么办呢？其实呢，本质没有变，只是Java8这里加了一个语法糖：在lambda表达式以及匿名类内部，如果引用某局部变量，则直接将其视为final。我们直接看一段代码吧：

```java
public static Supplier<Integer> testClosure() {
 int i = 1;
 i++;
 return () -> {
 	return i; //这里会出现编译错误
 };
}
```

明白了么？其实这里我们仅仅是省去了变量的final定义，这里i会强制被理解成final类型。很搞笑的是编译错误出现在lambda表达式内部引用i的地方，而不是改变变量值的i++…这也是Java的lambda的一个被人诟病的地方。我只能说，**强制闭包里变量必须为final**



### 五、Kotlin的lambda表达式和闭包

> Kotlin中使用lambda会比Java中使用lambda更灵活，访问受到限制更少，kotlin中是不需要非要被引用变量为final的，Kotlin中的lambda表达式是真正意义上的**支持闭包，而Java中的lambda则不是**

```kotlin
//例子
fun main(args: Array<String>) {
    val haha = getName("cheng")
    val hoho = haha("kun")
    val hehe = haha("kun22222")
    println(hoho)
    print(hehe)
}

fun getName(firstName: String): (String) -> String {
    val chengkun=chengkun()
    return fun(lastName: String): String {
        //改变自由变量的值
        chengkun.name=chengkun.name+"+"
        print(chengkun)
        println("    "+chengkun.name)
        println("函数形参firstName哈希码" + firstName.hashCode())
        return firstName + lastName
    }
}

class chengkun {
    var name = "chengkun"
}

//匿名函数-同样效果
fun ck1( firstName:String): (String) -> String {
    return { lastName:String ->
        firstName+lastName
    }
}
//##################### 输出结果 #####################
chengkun@5a07e868    chengkun+
函数形参firstName哈希码94627417
chengkun@5a07e868    chengkun++
函数形参firstName哈希码94627417
chengkun
chengkun22222
```



#### 1、lambda表达式分类

在Kotlin实际上可以把Lambda表达式分为两个大类，一个是普通的lambda表达式，另一个则是带接收者的lambda表达式,诸如作用域函数

```kotlin
@kotlin.internal.InlineOnly
public inline fun <T, R> with(receiver: T, block: T.() -> R): R {
    contract {
        callsInPlace(block, InvocationKind.EXACTLY_ONCE)
    }
    return receiver.block()
}

@kotlin.internal.InlineOnly
public inline fun <T> T.apply(block: T.() -> Unit): T {
    contract {
        callsInPlace(block, InvocationKind.EXACTLY_ONCE)
    }
    block()
    return this
}
```

#### 2、lambda语法简化转换

![kotlin lambda语法简化](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/0Iz3P9NVp0nffcGsQ46U*phLgQln2sSi*x74GQ4JlMg!/r/dL8AAAAAAAAA)

#### 3、lambda表达式的返回值

- lambda表达式返回值总是返回函数体内部最后一行表达式的值

#### 4、lambda表达式类型

```java
() -> Unit//表示无参数无返回值的Lambda表达式类型

(T) -> Unit//表示接收一个T类型参数，无返回值的Lambda表达式类型

(T) -> R//表示接收一个T类型参数，返回一个R类型值的Lambda表达式类型

(T, P) -> R//表示接收一个T类型和P类型的参数，返回一个R类型值的Lambda表达式类型

(T, (P,Q) -> S) -> R//表示接收一个T类型参数和一个接收P、Q类型两个参数并返回一个S类型的值的Lambda表达式类型参数，返回一个R类型值的Lambda表达式类型
```

#### 5、使用场景

- lambda表达式与集合一起使用，是最常见的场景，可以各种筛选、映射、变换操作符和对集合数据进行各种操作，非常灵活
- 替代原有匿名内部类，但是需要注意一点就是只能替代含有单抽象方法的类。
- 定义Kotlin扩展函数或者说需要把某个操作或函数当做值传入的某个函数的时候。

#### 6、Kotlin的lambda表达式的作用域中访问变量

在Kotlin中在函数内部定义lambda或者内部类，既可以访问final修饰的变量，也可以访问非final修饰的变量，也就意味着在Lambda的内部是可以直接修改函数局部变量的值

```kotlin
class Demo2Activity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_demo2)
        var count = 0//声明非final类型
        btn_click.setOnClickListener {
            println(count++)//直接访问和修改非final类型的变量
        }
    }
}
```

通过以上对比会发现Kotlin中使用lambda会比Java中使用lambda更灵活，访问受到限制更少，这也就回答本博客最开始说的一句话，Kotlin中的lambda表达式是真正意义上的支持闭包，而Java中的lambda则不是。Kotlin中的lambda表达式是怎么做到这一点的呢？请接着看

#### 7、kotlin的lambda表达式变量捕获

- 什么是变量捕获?
  通过上述例子，我们知道在Kotlin中既能访问final的变量也能访问或修改非final的变量。原理是怎样的呢？在此之前先抛出一个高大上的概念叫做lambdab表达式的变量捕获。实际上就是lambda表达式在其函数体内可以访问外部的变量，我们就称这些外部变量被lambda表达式给捕获了。有了这个概念我们可以把上面的结论变得高大上一些:

  第一在Java中lambda表达式只能捕获final修饰的变量

  第二在Kotlin中lambda表达式既能捕获final修饰的变量也能访问和修改非final的变量

- 变量捕获实现的原理

  我们都知道函数的局部变量生命周期是属于这个函数的，当函数执行完毕，局部变量也就是销毁了，**但是如果这个局部变量被lambda捕获了，那么使用这个局部变量的代码将会被存储起来等待稍后再次执行，也就是被捕获的局部变量是可以延迟生命周期的，**针对lambda表达式捕获final修饰的局部变量原理是局部变量的值和使用这个值的lambda代码会被一起存储起来；而针对于捕获非final修饰的局部变量原理是非final局部变量会被一个特殊包装器类包装起来，这样就可以通过包装器类实例去修改这个非final的变量，那么这个**包装器类实例引用是final的会和lambda代码一起存储**

  以上第二条结论在Kotlin的语法层面来说是正确的，但是从真正的原理上来说是错误的，只不过是Kotlin在语法层面把这个屏蔽了而已，实质的原理lambda表达式还是只能捕获final修饰变量，而为什么kotlin却能做到修改非final的变量的值，实际上kotlin在语法层面做了一个桥接包装**，它把所谓的非final的变量用一个Ref包装类包装起来，然后外部保留着Ref包装器的引用是final的，然后lambda会和这个final包装器的引用一起存储，随后在lambda内部修改变量的值实际上是通过这个final的包装器引用去修改的。**

![kotlin变量捕获](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/CeMq0WQn8.inK141oB.qdQjjDTwcXbHRMxc*MKLVn6E!/r/dL8AAAAAAAAA)

最后通过查看Kotlin修改非final局部变量的反编译成的Java代码就是一目了然了

```kotlin
public final class Demo2Activity extends AppCompatActivity {
  protected void onCreate(@Nullable Bundle savedInstanceState) {
      super.onCreate(savedInstanceState);
      this.setContentView(2131361820);
      final IntRef count = new IntRef();//IntRef特殊的包装器类的类型，final修饰的IntRef的count引用
      count.element = 0;//包装器内部的非final变量element
      ((Button)this._$_findCachedViewById(id.btn_click)).setOnClickListener((OnClickListener)(new OnClickListener() {
         public final void onClick(View it) {
            int var2 = count.element++;//直接是通过IntRef的引用直接修改内部的非final变量的值，来达到语法层面的lambda直接修改非final局部变量的值
            System.out.println(var2);
         }
      }));
   }
}
```

#### 8、成员引用

- **Why?**

我们知道在Lambda表达式可以直接把一个代码块作为一个参数传递给函数，但是有没有遇到过这样一个场景就是我要传递过去的代码块，已经是作为了一个命名函数存在了，此时你还需要重复写一个代码块传递过去吗？肯定不是，Kotlin拒绝啰嗦重复的代码。所以只需要成员引用替代即可。

```kotlin
fun main(args: Array<String>) {
    val persons = listOf(Person(name = "Alice", age = 18), Person(name = "Mikyou", age = 20), Person(name = "Bob", age = 16))
    println(persons.maxBy({ p: Person -> p.age }))
}
```

可以替代为

```kotlin
fun main(args: Array<String>) {
    val persons = listOf(Person(name = "Alice", age = 18), Person(name = "Mikyou", age = 20), Person(name = "Bob", age = 16))
    println(persons.maxBy(Person::age))//成员引用的类型和maxBy传入的lambda表达式类型一致
}
```

- **成员引用组成**

成员引用由类、双冒号、成员三个部分组成

![成员引用](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/DxpCzVPTShFZtnNJW73.q*s4LeAaRIwkK7VwgD33nsU!/r/dD4BAAAAAAAA)

- **使用场景**

  - 成员引用最常见的使用方式就是类名+双冒号+成员(属性或函数)

  ```kotlin
  fun main(args: Array<String>) {
      val persons = listOf(Person(name = "Alice", age = 18), Person(name = "Mikyou", age = 20), Person(name = "Bob", age = 16))
      println(persons.maxBy(Person::age))//成员引用的类型和maxBy传入的lambda表达式类型一致
  }
  ```

  - 省略类名直接引用顶层函数

  ```kotlin
  fun salute() = print("salute")
  
  fun main(args: Array<String>) {
      run(::salute)
  }
  ```

  - 成员引用用于扩展函数

  ```kotlin
  fun Person.isChild() = age < 18
  
  fun main(args: Array<String>){
      val isChild = Person::isChild
      println(isChild)
  }
  ```



### 链接

- [维基百科-闭包](https://zh.wikipedia.org/wiki/%E9%97%AD%E5%8C%85_(%E8%AE%A1%E7%AE%97%E6%9C%BA%E7%A7%91%E5%AD%A6))
- [维基百科-lambda表达式](https://zh.wikipedia.org/wiki/%E5%8C%BF%E5%90%8D%E5%87%BD%E6%95%B0#Python)

- [阮一峰的闭包讲解](http://www.ruanyifeng.com/blog/2009/08/learning_javascript_closures.html)

- [你真的理解闭包和lambda表达式吗](https://www.jianshu.com/p/c22db2a91989)

- [lambda表达式和闭包](http://www.importnew.com/17905.html)

- [Kotlin的lambda表达式](https://blog.csdn.net/u013064109/article/details/80088158)

- [什么是闭包](https://blog.csdn.net/qibin0506/article/details/73395115)