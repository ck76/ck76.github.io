[TOC]

# 一、前言

在使用`Java`语言设计类之间关系的时候，我们会接触到 **组成单元** 和 **关系连接** 这两类概念：

- 组成单元：普通类、`abstract`抽象类，`interface`接口。
- 关系连接：`implements`实现，`extends`继承。

而在`Dart`当中，对于这两类概念进行了增减：

- 组成单元：普通类，`abstract`抽象类、`mixin`。
- 关系连接：`implements`实现、`extends`继承、`with`混入。

最大的不同有两点：

- 去掉了`interface`。
- 增加了混入的概念。

下面我们就来看一下其中涉及到的知识点，前面两节对比一下`Java`和`Dart`的区别，最后着重介绍混入的概念。

推荐给大家一个网站：[https://dartpad.dartlang.org/](https://links.jianshu.com/go?to=https%3A%2F%2Fdartpad.dartlang.org%2F) 可以在线运行。

# 二、组成单元

## 2.1 普通类

`Java`和`Dart`的普通类有区别，但是不影响我们设计类之间的关系，因此不再赘述。

## 2.2 abstract 抽象类

`Java`和`Dart`的抽象类定义时大体是一样的，我们可以在其中定义变量、普通方法、抽象方法，它和普通类最大的区别就是 **抽象类不能实例化**。

`Java`和`Dart`在使用抽象类时有一点不同：**`Dart`在定义抽象方法时，不需要用`abstract`修饰**。



```dart
abstract class DartAbs {
  
  void absMethod();
  
}
```

## 2.3 interface 接口

**在`Dart`中，没有 interface 关键字**。

顺带我们复习一下`Java`中`abstract`和`interface`的一些要点：

- 抽象类和接口都不能被实例化。
- 抽象类要被子类继承，接口要被类实现。
- 接口只能做方法的声明，抽象类可以做方法的声明，也可以做方法的实现。
- 接口里定义的变量只能是公共的静态常量，抽象类中的变量可以是普通变量。
- 抽象类里的抽象方法必须全部被子类实现；接口的接口方法必须全部被子类实现，否则只能为抽象类。
- 抽象类里可以没有抽象方法。
- 如果一个类里有抽象方法，那么这个类只能是抽象类。
- 抽象方法要被实现，所以不能是静态的，也不能是私有的。
- 接口可继承接口，并可多继承接口，但类只能单继承。

# 三、关系连接

## 3.1 extends

`Java`和`Dart`中`extends`是一致的，**只可以单继承**，要注意的点是：

- 子类可以继承父类里面

   

  可见的属性和方法

  。

  - 对于`Java`来说，可见指的是非`private`修饰，
  - 对`Dart`来说，指的是非下划线`_`开头。

- 子类调用父类的方法，使用`super`关键字。

- 子类不会 **继承** 父类的构造函数。



```dart
class Extends {
  
  void base() {
    print('base');
  }
  
  void log() {
    print('extends');
  }
  
}

class Log extends Extends {
  
  log() {
    print('log');
  }
  
}

void main() {
  Log().base();
  Log().log();
}
```

输出结果：



```dart
base
log
```

## 3.2 implements

`implements`与`extends`最大的不同就是允许后面接上多个普通或者抽象类，当我们使用`B implement A`修饰时，**那么`A`中的所有的属性和方法都要在`A`中实现，无论它原来是抽象方法还是普通方法**。

也就是说如果我们只想要`A`中的接口定义，而不想要它的实现，那么就试用`implements`。



```dart
class Implements {
  
  void base() {
    print('base');
  }
    
  void log() {
    print('extends');
  }
  
}

class Log implements Implements {
  
  base() {
    print('log#base');
  }
  
  log() {
    print('log');
  }
  
}

void main() {
  Log().base();
  Log().log();
}
```

输出结果：



```dart
log#base
log
```

# 四、混入

前面我们介绍的都是`Java`中接触过的概念，下面我们来介绍`Dart`中特有的概念 - 混入。

## 4.1 mixin

`mixin`用于修饰类，和`abstract`类似，该类可以拥有成员变量、普通方法、抽象方法，但是不可以实例化。`mixin`一般用于描述一种具有某种功能的组块，而某一对象可以拥有多个不同功能的组块。

### (1) 最简单

最简单的`mixin`由`mixin & with`关键字组成。

举个例子，我们有一种能力是 '绘画'，而拥有这种能力的是 ‘教师’，那么实现如下：



```dart
mixin DrawFunc {
  
  String content = '..';
  
  String what();
    
  void draw() {
    print('I can draw ${what()}');  
  }
  
}

class Teacher with DrawFunc {
  
  String what() => "car";
  
}

void main() {
  Teacher().draw();
}
```

### (2) 限定类型

我们限定了 '绘画' 这种能力只能够用在 '人类' 上面，示例如下：



```dart
class Person {}

mixin DrawFunc on Person {
  
  String content = '..';
  
  String what();
    
  void draw() {
    print('I can draw ${what()}');  
  }
  
}

class Teacher extends Person with DrawFunc {
  
  String what() => "car";
  
}

void main() {
  Teacher().draw();
}
```

当我们在`mixin`上使用了`on`关键字，那么`mixin`只能在那个类的子类上使用，而`mixin`可以调用那个类的方法。

### (3) 多个类型

在 '绘画' 的基础上，我们增加一种新的能力 '唱歌'，示例如下：



```dart
class Person {}

mixin DrawFunc on Person {
  
  String content = '..';
  
  String what();
    
  void draw() {
    print('I can draw ${what()}');  
  }
  
}

mixin SingFunc on Person {
  
  void sing() {
    print('I can sing');
  }
}

class Teacher extends Person with DrawFunc, SingFunc {
  
  String what() => "car";
  
}

void main() {
  Teacher().draw();
  Teacher().sing();
}
```

### (4) on 的一种复杂变形

关于`on`还有一种复杂的变形，我们在 '唱歌' 上增加一条约束，要求它必须是在`DrawFunc`之上：



```dart
mixin SingFunc on Person, DrawFunc {
  
  void sing() {
    print('I can sing');
  }
}
```

那么这时候，虽然`Teacher`没有`extends DrawFunc`，但是如下的代码仍然可以编译通过：



```dart
class Teacher extends Person with DrawFunc, SingFunc {
  
  String what() => "car";
  
}
```

而我们交换一下`DrawFunc`和`SingFunc`的顺序就不行了：



```dart
class Teacher extends Person with SingFunc, DrawFunc {
  
  String what() => "car";
  
}
```

提示信息是：



```dart
Error compiling to JavaScript:
main.dart:22:7:
Error: 'Person' doesn't implement 'DrawFunc' so it can't be used with 'SingFunc'.
 - 'Person' is from 'main.dart'.
 - 'DrawFunc' is from 'main.dart'.
 - 'SingFunc' is from 'main.dart'.
class Teacher extends Person with SingFunc, DrawFunc {
      ^
Error: Compilation failed.
```

> 结论：要满足`on`的要求，除了使用`extends`之外，还可以在`with`列表中，在它之前进行声明。在`Flutter`的`WidgetsFlutterBinding`中，就涉及到了这一点的运用。



```dart
abstract class BindingBase {}

mixin ServicesBinding on BindingBase {}

mixin SchedulerBinding on BindingBase, ServicesBinding {}

mixin RendererBinding on BindingBase, ServicesBinding {}

class WidgetsFlutterBinding extends BindingBase with ServicesBinding, SchedulerBinding, RendererBinding {}
```

### (5) Tips

在这上面，我们接触了几个新的概念`mixin, on, with`：

- `mixin`：定义了组块。
- `on`：限定了使用`mixin`组块的宿主必须要继承于某个特定的类；在`mixin`中可以访问到该特定类的成员和方法。
- `with`：负责组合组块，而`with`后面跟的类并不一定需要是`mixin`的，`abstract class`和普通类都是可以的，这一点需要注意，例如下面这样：



```dart
class Person {}

mixin DrawFunc on Person {
  
  String content = '..';
  
  String what();
    
  void draw() {
    print('I can draw ${what()}');  
  }
  
}

mixin SingFunc on Person {
  
  void sing() {
    print('I can sing');
  }
}

abstract class DanceFunc {
  
  void dance() {
    print('I can dance');
  }
  
}

class Teacher extends Person with DrawFunc, SingFunc, DanceFunc {
  
  String what() => "car";
  
}

void main() {
  Teacher().draw();
  Teacher().sing();
  Teacher().dance();
}
```

## 4.2 冲突

如果同时存在`extends, with`，并且它们都定义了相同的方法名，那么结果如何呢？我们来看下面的例子：



```dart
class Extends {
  
  void log() {
    print('extends');
  }
  
}

mixin Mixins {
  
  void log() {
    print('mixin');
  }
  
}

mixin Mixins2 {
  
  void log() {
    print('mixin2');
  }
  
}

class Log extends Extends with Mixins, Mixins2 {}

void main() {
  Log().log();
}
```

输出结果：



```dart
mixin2
```

> 结论

- `with`修饰的会覆盖`extends`中修饰的同名方法。
- `with`列表中后一个的会覆盖之前的。

再来看一下加上了`implements`的情况。



```dart
class Extends {
  
  void log() {
    print('extends');
  }
  
}

mixin Mixins {
  
  void log() {
    print('mixin');
  }
  
}

mixin Mixins2 {
  
  void log() {
    print('mixin2');
  }
  
}

class Implements {
  
  void log() {
    print('implements');
  }
  
}

class Log extends Extends with Mixins, Mixins2 implements Implements {}

void main() {
  Log().log();
}
```

输出结果为：



```dart
mixin2
```

这里我们发现了一个奇怪的现象：虽然我们加上了`implements`，但是`Dart`居然没让我们实现`Implements.log()`方法！

这是因为在这种情况下，它识别到我们从`with`和`extends`中获得了`log()`方法的能力，因此调用的是`Mixins2.log()`。

假如我们对`Implements#log`方法进行实现：



```dart
class Log extends Extends with Mixins, Mixins2 implements Implements {
  
  void log() {
    print("implements log");
  }
}
```

输出的结果为：



```dart
implements log
```

# 五、小结

梳理下来，有几点感想：

- 之前我们设计中用到了`interface`的部分，可以采用只带有抽象方法的`abstract class`替换，并继续使用`implements`关键字。
- 理解`mixin`的概念，我是将它理解为一个个的功能组块：哪些宿主需要哪些功能，我就`with`到上去。
- `on`关键字一方面是为了限制组块的应用场景，也可以为多个组块提供公共的基础功能。

# 参考资料

- [【Dart学习】-- Dart 之 extends && implements && with的用法与区别](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.cnblogs.com%2Flxlx1798%2Fp%2F11044101.html)
- [Flutter 基础：理解 Dart 的 Mixin 继承机制](https://links.jianshu.com/go?to=https%3A%2F%2Fkevinwu.cn%2Fp%2Fae2ce64%2F%23%E5%9C%BA%E6%99%AF)
- [Flutter mixin 用法详解](https://links.jianshu.com/go?to=https%3A%2F%2Fjuejin.im%2Fpost%2F5c9ad7295188251d163fa486)



作者：泽毛
链接：https://www.jianshu.com/p/18e8d285c81a
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。