[TOC]

# Dart语法基础

## Dart语言简介

在Dart官方网站上，对于Dart的描述如下：

> Developers at Google and elsewhere use Dart to create high-quality, mission-critical apps for iOS, Android, and the web. With features aimed at client-side development, Dart is a great fit for both mobile and web apps.
> Google和其他地方的一些开发者使用Dart语言为Android、iOS和web构建高质量，关键任务的应用程序，针对客户端开发的特点，Dart非常适合移动和Web应用程序。

Dart是Google推出的一门编程语言，最初是希望取代Javascript运行在浏览器端，后来慢慢发展成可以开发Android、iOS和Web端App的一门高质量的编程语言，目前Dart的版本是Dart2，官网是：[www.dartlang.org/](https://link.juejin.im/?target=https%3A%2F%2Fwww.dartlang.org%2F)

更加详细的可以参考

- 英文官方文档 [www.dartlang.org/](https://link.juejin.im/?target=https%3A%2F%2Fwww.dartlang.org%2F)
- 中文官方文档 [http://dart.goodev.org/guides/language](http://dart.goodev.org/guides/language)
- 语法预览 http://dart.goodev.org/guides/language/language-tour
- 在线联系 https://dartpad.dartlang.org/

## 核心概念

Dart语言博采众长，在我们学习Dart时候需要记住以下核心概念

- 一切都是**对象**，一切对象都是**class的实例**，哪怕是数字类型、方法甚至null都是对象，所有的对象都是继承自`Object`
- Dart是强类型语言，但变量类型是可选的因为Dart可以自动推断变量类型
- Dart支持范型，`List<int>`表示一个整型的数据列表，`List<dynamic>`则是一个对象的列表，其中可以装任意对象
- Dart支持顶层方法（如`main`方法），也支持类方法或对象方法，同时你也可以在方法内部创建方法
- Dart支持顶层变量，也支持类变量或对象变量
- 跟Java不同的是，Dart没有`public` `protected` `private`等关键字，如果某个变量以下划线（`_`）开头，代表这个变量在库中是私有的

## 变量

### 声明变量的方式

可以明确指定某个变量的类型，如`int` `bool` `String`，也可以用`var`或 `dynamic`来声明一个变量，Dart会自动推断其数据类型。

```dart
main() {
  var a = 1;
  int b = 10;
  String s = "hello";
  dynamic c = 0.5;
}
```



### final和const

`final`修饰的变量是不可改变的，而`const`修饰的表示一个常量，声明普通变量可以使用var

## 内置数据类型

Dart有如下几种内建的数据类型：

- numbers
- strings
- booleans
- lists(或者是arrays)
- maps
- runes（UTF-32字符集的字符）
- symbols

### String

- 检测两个 String 的内容是否一样事，我们使用 == 进行比较；如果要测试两个对象是否是同一个对象（indentity test），使用 identical 函数
- - 进行拼接
- \${} 字符串内表达式
- 三个单引号或者双引号可以创建多行字符串对象

### List

[http://dart.goodev.org/guides/libraries/library-tour#collections](http://dart.goodev.org/guides/libraries/library-tour#collections)

#### 初始化  []

```dart
// Use a List constructor.
var vegetables = new List();

// Or simply use a list literal.
var fruits = ['apples', 'oranges'];

//list是泛型类型也可以指定里面保存的类型
var fruits = new List<String>();
fruits.add('apples');
```

#### 核心api

- add
- addAll
- length
- indexOf
- sort

### Map  {}

map是一个关联键和值的对象。键和值都可以是任何类型的对象。每个键只出现一次，但是您可以多次使用相同的值。Dart对map的支持是通过map字面量和map类型来提供的。

#### 初始化

```dart
var gifts = {
// Keys      Values
  'first' : 'partridge',
  'second': 'turtledoves',
  'fifth' : 'golden rings'
};

var nobleGases = {
// Keys  Values
  2 :   'helium',
  10:   'neon',
  18:   'argon',
};
//构造函数实现
var gifts = new Map();
gifts['first'] = 'partridge';
gifts['second'] = 'turtledoves';
gifts['fifth'] = 'golden rings';

var nobleGases = new Map();
nobleGases[2] = 'helium';
nobleGases[10] = 'neon';
nobleGases[18] = 'argon';
```

#### 核心api

- 添加

  ```dart
  var gifts = {'first': 'partridge'};
  gifts['fourth'] = 'calling birds'; // Add a key-value pair
  ```

- 获取map对象

  ```dart
  var gifts = {'first': 'partridge'};
  assert(gifts['first'] == 'partridge');
  ```

如果查找的key不存在则返回null

- length 获取map中键值对的数目
- containsKey
- putIfAbsent()
  但是只有该 key 在 map 中不存在的时候才设置这个值，否则 key 的值保持不变

### Set

#### 初始化

Set 是一个无序集合，里面不能保护重复的数据。 由于是无序的，所以无法通过索引来从 set 中获取数据

```dart
var ingredients = new Set();
ingredients.addAll(['gold', 'titanium', 'xenon']);
assert(ingredients.length == 3);

// Adding a duplicate item has no effect.
ingredients.add('gold');
assert(ingredients.length == 3);

// Remove an item from a set.
ingredients.remove('gold');
assert(ingredients.length == 2);
```

#### 核心api

- `contains()` 和 `containsAll()` 来判断 set 中是否包含 一个或者多个对象

### 常用集合函数

- List 和 Set 实现了 Iterable ,虽然 Map 没有实现 Iterable，**但是 Map 的 keys 和 values 属性实现了 Iterable**

- isEmpty

- `forEach()` 函数可以遍历集合数据，在 Map 上使用 `forEach()` 的时候，方法需要能 接收两个参数（key 和 value）：

  ```dart
  var teas = ['green', 'black', 'chamomile', 'earl grey'];
  
  teas.forEach((tea) => print('I drink $tea'));
  //map
  hawaiianBeaches.forEach((k, v) {
    print('I want to visit $k and swim at $v');
    // I want to visit Oahu and swim at
    // [Waikiki, Kailua, Waimanalo], etc.
  });
  ```

- 可以使用 `map().toList()` 或者 `map().toSet()` 来 强制立刻执行 map 的方法：

## 函数

### 返回值

Dart是一个面向对象的编程语言，所以即使是**函数也是一个对象**，也有一种类型`Function`，这就意味着函数可以赋值给某个变量或者作为参数传给另外的函数。虽然Dart推荐你给函数加上返回值，但是**不加返回值的函数同样可以正常工作**，另外你还可以用`=>`代替`return`语句

```dart
// =>是return语句的简写
add3(a, b) => a + b;
```



所有的函数都有返回值，如果没有指定`return`语句，那么该函数的返回值为`null`。

### 命名参数、位置参数、参数默认值

- 命名参数

使用花括号将函数的参数括起来就是定义了命名参数

```dart
sayHello({String name}) {
  print("hello, my name is $name");
}

sayHello2({name: String}) {
  print("hello, my name is $name");
}
```

**可以以`{type paramName}`或者`{paramName: type}`两种方式声明参数**，而调用命名参数时，需要以`funcName(paramName: paramValue)`的形式调用。
可以使用`@required`注解来标识一个命名参数，这代表该参数是必须的，你不传则会报错

```dart
const Scrollbar({Key key, @required Widget child})
```

- 位置参数（参数可传可不传）
  使用中括号`[]`括起来的参数是函数的位置参数，代表该参数可传可不传，位置参数只能放在函数的参数列表的最后面

```dart
sayHello(String name, int age, [String hobby]) { // 位置参数可以有多个，比如[String a, int b]
  StringBuffer sb = new StringBuffer();
  sb.write("hello, this is $name and I am $age years old");
  if (hobby != null) {
    sb.write(", my hobby is $hobby");
  }
  print(sb.toString());
}

main() {
  // hello, this is zhangsan and I am 20 years old
  sayHello("zhangsan", 20);
  // hello, this is zhangsan and I am 20 years old, my hobby is play football
  sayHello("zhangsan", 20, "play football");
}
```

- 参数默认值

可以为命名参数或者位置参数设置默认值

```dart
// 命名参数的默认值
int add({int a, int b = 3}) { // 不能写成：int add({a: int, b: int = 3})
  return a + b;
}

// 位置参数的默认值
int sum(int a, int b, [int c = 3]) {
  return a + b + c;
}
```



### 函数作为一等方法对象

函数作为参数传给另一个函数

```dart
printNum(int a) {
  print("$a");
}

  //  依次打印：1,2,3
  var arr = [1, 2, 3];
  arr.forEach({
  });
  Function printCk = (String ck) {
    print(ck);
    return 1;
  };
  arr.forEach(printCk);
```

或者将函数赋值给某个变量

```dart
printNum(int a) {
  print("$a");
}

main() {
  var f1 = printNum;
  Function f2 = printNum;
  var f3 = (int a) => print("a = $a");
  f1(1);
  f2(2);
  f3(6);
}
```



## 匿名函数

- 创建没有名字的方法，称之为 *匿名方法*，有时候也被称为 *lambda* 或者 *closure 闭包*。 你可以把匿名方法赋值给一个变量， 然后你可以使用这个方法，比如添加到集合或者从集合中删除

- 匿名函数类似于`Java`中的接口，往往在某个函数的参数为函数时使用到。

### 词法闭包

  一个 *闭包* 是一个方法对象，不管该对象在何处被调用， 该对象都可以访问其作用域内 的变量。

下面的示例中，`makeAdder()` 捕获到了变量 `addBy`。 不管你在那里执行 `makeAdder()` 所返回的函数，都可以使用 `addBy` 参数。

```dart
Function makeAdder(num addBy) {
  return (num i) => addBy + i;
}
```



## 操作符

| 描述                     | 操作符                                                      |         |      |
| :----------------------- | :---------------------------------------------------------- | ------- | ---- |
| unary postfix            | `*expr*``++` `*expr*``–` `()``[]` `.` `?.`                  |         |      |
| unary prefix             | `-``*expr*` `!``*expr*``~``*expr*` `++``*expr*` `–``*expr*` |         |      |
| multiplicative           | `*` `/` `%` `~/`                                            |         |      |
| additive                 | `+` `-`                                                     |         |      |
| shift                    | `<<` `>>`                                                   |         |      |
| bitwise AND              | `&`                                                         |         |      |
| bitwise XOR              | `^`                                                         |         |      |
| bitwise OR               | `                                                           | `       |      |
| relational and type test | `>=` `>` `<=` `<` `as` `is``is!`                            |         |      |
| equality                 | `==` `!=`                                                   |         |      |
| logical AND              | `&&`                                                        |         |      |
| logical OR               | `                                                           |         | `    |
| if null                  | `??`                                                        |         |      |
| conditional              | `*expr1*`` ? ``*expr2*`` :``*expr3*`                        |         |      |
| cascade                  | `..`                                                        |         |      |
| assignment               | `=` `*=` `/=` `~/=` `%=` `+=``-=` `<<=` `>>=` `&=` `^=` `   | =``??=` |      |

### 类型判定操作符

`as`、 `is`、 和 `is!` 操作符是在运行时判定对象 类型的操作符：

| 操作符 | 解释                           |
| :----- | :----------------------------- |
| `as`   | 类型转换                       |
| `is`   | 如果对象是指定的类型返回 True  |
| `is!`  | 如果对象是指定的类型返回 False |

### 赋值操作符

使用 `=` 操作符来赋值。 但是还有一个 `??=` 操作符用来指定 值为 null 的变量的值。

```dart
a = value;   // 给 a 变量赋值
b ??= value; // 如果 b 是 null，则赋值给 b；
             // 如果不是 null，则 b 的值保持不变
```



### 条件表达式

Dart 有两个特殊的操作符可以用来替代 if-else 语句：

- `*condition*`` ? ``*expr1*`` : ``*expr2*`
  如果 *condition* 是 true，执行 *expr1* (并返回执行的结果)； 否则执行 *expr2* 并返回其结果。
  如果你需要基于布尔表达式 的值来赋值， 考虑使用 `?:`。

  ```dart
  var finalStatus = m.isFinal ? 'final' : 'not final';
  ```

- `*expr1*`` ?? ``*expr2*`
  如果 *expr1* 是 non-null，返回其值； 否则执行 *expr2* 并返回其结果。
  如果布尔表达式是测试值是否为 null， 考虑使用 `??`。

  ```java
  String toString() => msg ?? super.toString();
  ```

### 级联操作符

级联操作符 (`..`) 可以在同一个对象上 连续调用多个函数以及访问成员变量。 使用级联操作符可以避免创建 临时变量， 并且写出来的代码看起来 更加流畅：

```java
querySelector('#button') // Get an object.
  ..text = 'Confirm'   // Use its members.
  ..classes.add('important')
  ..onClick.listen((e) => window.alert('Confirmed!'));
```

级联调用也可以嵌套：

```java
final addressBook = (new AddressBookBuilder()
      ..name = 'jenny'
      ..email = 'jenny@example.com'
      ..phone = (new PhoneNumberBuilder()
            ..number = '415-555-0100'
            ..label = 'home')
          .build())
    .build();
```

在方法上使用级联需要十分小心，无法再void上使用级联操作符

```java
// Does not work
var sb = new StringBuffer();
sb.write('foo')..write('bar');
```



### 其他操作符

| Operator | Name         | Meaning                                                      |
| :------- | :----------- | :----------------------------------------------------------- |
| `()`     | 使用方法     | 代表调用一个方法                                             |
| `[]`     | 访问 List    | 访问 list 中特定位置的元素                                   |
| `.`      | 访问 Member  | 访问元素，例如 `foo.bar` 代表访问 `foo` 的 `bar` 成员        |
| `?.`     | 条件成员访问 | 和 `.` 类似，但是左边的操作对象不能为 null，例如 `foo?.bar` 如果 `foo`为 null 则返回 null，否则返回 `bar`成员 |

## 控制流程

`if / else` `switch` `for /while` `try / catch`语句跟`Java`中都类似，`try / catch`语句可能稍有不同

### try catch

捕获异常可以避免异常继续传递（你重新抛出rethrow异常除外）。 捕获异常给你一个处理 该异常的机会：

```java
try {
  breedMoreLlamas();
} on OutOfLlamasException {
  buyMoreLlamas();
}
```

对于可以抛出多种类型异常的代码，你可以指定 多个捕获语句。每个语句分别对应一个异常类型， 如果捕获语句没有指定异常类型，则 该可以捕获任何异常类型：

```java
try {
  breedMoreLlamas();
} on OutOfLlamasException {
  // A specific exception
  buyMoreLlamas();
} on Exception catch (e) {
  // Anything else that is an exception
  print('Unknown exception: $e');
} catch (e) {
  // No specified type, handles all
  print('Something really unknown: $e');
}
```

使用 `on` 来指定异常类型，使用 `catch` 来 捕获异常对象。
使用 `rethrow` 关键字可以 把捕获的异常给 重新抛出。

```java
final foo = '';

void misbehave() {
  try {
    foo = "You can't change a final variable's value.";
  } catch (e) {
    print('misbehave() partially handled ${e.runtimeType}.');
    rethrow; // Allow callers to see the exception.
  }
}

void main() {
  try {
    misbehave();
  } catch (e) {
    print('main() finished handling ${e.runtimeType}.');
  }
}
```



## 类

### 类的定义与构造方法

`Dart`中的类没有访问控制，所以你不需要用`private`, `protected`, `public`等修饰成员变量或成员函数

```java
class Person {
  String name;
  int age;
  String gender;
  Person(this.name, this.age, this.gender);
  sayHello() {
    print("hello, this is $name, I am $age years old, I am a $gender");
  }
}
```



上面的`Person`类中有3个成员变量，一个构造方法和一个成员方法，看起来比较奇怪的是`Person`的构造方法，里面传入的3个参数都是`this.xxx`，而且没有大括号`{}`包裹的方法体，这种语法是Dart比较独特而简洁的构造方法声明方式，它等同于下面的代码：

```java
Person(String name, int age, String gender) {
    this.name = name;
    this.age = age;
    this.gender = gender;
}
```



由于`Dart`中的类没有访问控制权限，所以你可以直接用`obj.var`的方式访问一个对象的成员变量。

#### 命名构造函数

使用命名构造函数可以为一个类实现多个构造函数， 或者使用命名构造函数来更清晰的表明你的意图：

```java
class Point {
  num x, y;
  Point(this.x, this.y);
  // 类的命名构造方法
  Point.origin() {
    x = 0;
    y = 0;
  }
}

main() {
  // 调用Point类的命名构造方法origin()
  var p = new Point.origin();
  var p2 = new Point(1, 2);
}
```



#### 调用超类构造函数

子类的构造函数会自动调用超类的 无名无参数的默认构造函数。 超类的构造函数在子类构造函数体开始执行的位置调用。 如果提供了一个 [initializer list](http://dart.goodev.org/guides/language/language-tour#initializer-list)（初始化参数列表） ，则初始化参数列表在超类构造函数执行之前执行。 下面是构造函数执行顺序：

1. initializer list（初始化参数列表）
2. superclass’s no-arg constructor（超类的无名构造函数）
3. main class’s no-arg constructor（主类的无名构造函数）

如果超类没有无名无参数构造函数， 则你需要手工的调用超类的其他构造函数。 在构造函数参数后使用冒号 (`:`) 可以调用 超类构造函数。
`Dart`中使用`extends`关键字做类的继承，如果一个类只有命名的构造方法，在继承时需要注意，如下代码：

```java
class Human {
  String name;
  Human.fromJson(Map data) {
    print("Human's fromJson constructor");
  }
}

class Man extends Human {
  Man.fromJson(Map data) : super.fromJson(data) {
    print("Man's fromJson constructor");
  }
}
```



#### 重定向构造函数

有时候一个构造函数会调动类中的其他构造函数。 一个重定向构造函数是没有代码的，在构造函数声明后，使用 冒号调用其他构造函数。

```java
class Point {
  num x;
  num y;

  // The main constructor for this class.
  Point(this.x, this.y);

  // Delegates to the main constructor.
  Point.alongXAxis(num x) : this(x, 0);
}
```



### 抽象类和抽象方法

使用abstract修饰一个类，则这个类是抽象类，
**抽象类中可以有抽象方法和非抽象方法**，抽象方法没有方法体，需要子类去实现，如下代码：

```java
abstract class Doer {
  // 抽象方法，没有方法体，需要子类去实现
  void doSomething();
  // 普通的方法
  void greet() {
    print("hello world!");
  }
}

class EffectiveDoer extends Doer {
  // 实现了父类的抽象方法
  void doSomething() {
    print("I'm doing something...");
  }
}
```



### 枚举类

枚举类型通常称之为 *enumerations* 或者 *enums*， 是一种特殊的类，用来表现一个固定 数目的常量。

```java
enum Color {
  red,
  green,
  blue
}
```

- 枚举类型中的每个值都有一个 `index` getter 函数， 该函数返回该值在枚举类型定义中的位置（从 0 开始）。 例如，第一个枚举值的位置为 0， 第二个为 1.
- 枚举的 `values` 常量可以返回 所有的枚举值。

枚举类型具有如下的限制：

- 无法继承枚举类型、无法使用 mix in、无法实现一个枚举类型
- 无法显示的初始化一个枚举类型

### Mixins 为类添加新的功能 : with

Mixins 是一种在多类继承中重用 一个类代码的方法。使用 with 关键字后面为一个或者多个 mixin 名字来使用 mixin。 下面是示例显示了如何使用 mixin：

```java
class A {
  a() {
    print("A's a()");
  }
}

class B {
  b() {
    print("B's b()");
  }
}

// 使用with关键字，表示类C是由类A和类B混合而构成
class C = A with B;

main() {
  C c = new C();
  c.a(); // A's a()
  c.b(); // B's b()
}
```



### 静态成员变量 和 静态成员方法

```java
// 类的静态成员变量和静态成员方法
class Cons {
  static const name = "zhangsan";
  static sayHello() {
    print("hello, this is ${Cons.name}");
  }
}

main() {
  Cons.sayHello(); // hello, this is zhangsan
  print(Cons.name); // zhangsan
}
```

## 泛型

[http://dart.goodev.org/guides/language/language-tour#generics%E6%B3%9B%E5%9E%8B](http://dart.goodev.org/guides/language/language-tour#generics泛型)

使用泛型可以减少冗余的代码， 泛型可以在多种类型之间定义同一个实现，在Dart中Dart 的泛型类型是固化的，在运行时有也 可以判断具体的类型。例如在运行时（甚至是成产模式） 也可以检测集合里面的对象类型：

```java
var names = new List<String>();
names.addAll(['Seth', 'Kathy', 'Lars']);
print(names is List<String>); // true
```



这个跟Java中是不同的 Java 中的泛型信息是编译时的，泛型信息在运行时是不纯在的。 在 Java 中你可以测试一个对象是否为 List， 但是你无法测试一个对象是否为 List。

### 限制型泛型

使用 `extends` 可以实现这个功能

```java
// T must be SomeBaseClass or one of its descendants.
class Foo<T extends SomeBaseClass> {...}

class Extender extends SomeBaseClass {...}

void main() {
  // It's OK to use SomeBaseClass or any of its subclasses inside <>.
  var someBaseClassFoo = new Foo<SomeBaseClass>();
  var extenderFoo = new Foo<Extender>();

  // It's also OK to use no <> at all.
  var foo = new Foo();

  // Specifying any non-SomeBaseClass type results in a warning and, in
  // checked mode, a runtime error.
  // var objectFoo = new Foo<Object>();
}
```



### 泛型函数

一开始，泛型只能在 Dart 类中使用。 新的语法也支持在函数和方法上使用泛型了。

```java
T first<T>(List<T> ts) {
  // ...Do some initial work or error checking, then...
  T tmp ?= ts[0];
  // ...Do some additional checking or processing...
  return tmp;
}
```

这里的 `first` (`<T>`) 泛型可以在如下地方使用 参数 `T` ：

- 函数的返回值类型 (`T`).
- 参数的类型 (`List<T>`).
- 局部变量的类型 (`T tmp`).

## 库的可见性

- 普通引用

  ```java
  // demo.dart
  
  import './util.dart';
  
  main() {
    print(add(1, 2));
  }
  ```

- as关键字 设置前缀

  ```java
  import 'package:lib1/lib1.dart';
  import 'package:lib2/lib2.dart' as lib2;
  
  // Uses Element from lib1.
  Element element1 = Element();
  
  // Uses Element from lib2.
  lib2.Element element2 = lib2.Element();
  ```

- show hide 关键字导入包的部分功能

  ```java
  // 只导入foo
  import 'package:lib1/lib1.dart' show foo;
  
  // 导入除了foo的所有其他部分
  import 'package:lib2/lib2.dart' hide foo;
  ```

- deferred as 懒加载

  ```java
  import 'package:greetings/hello.dart' deferred as hello;
  ```

## 异步

`Dart`提供了类似ES7中的`async` `await`等异步操作，这种异步操作在Flutter开发中会经常遇到，比如网络或其他IO操作，文件选择等都需要用到异步的知识。`async`和`await`往往是成对出现的，如果一个方法中有耗时的操作，你需要将这个方法设置成`async`，并给其中的耗时操作加上`await`关键字，如果这个方法有返回值，你需要将返回值塞到`Future`中并返回，如下代码所示：

```java
Future checkVersion() async {
  var version = await lookUpVersion();
  // Do something with version
}
```

更多请见 https://segmentfault.com/a/1190000014396421

## 可调用类

如果 Dart 类实现了 `call()` 函数则 可以当做方法来调用。

```java
class WannabeFunction {
  call(String a, String b, String c) => '$a $b $c!';
}

main() {
  var wf = new WannabeFunction();
  var out = wf("Hi","there,","gang");
  print('$out');
}
```

## 元数据

使用元数据给你的代码添加其他额外信息。 元数据注解是以 `@` 字符开头，后面是一个编译时 常量(例如 `deprecated`)或者 调用一个常量构造函数。
有三个注解所有的 Dart 代码都可以使用： `@deprecated`、 `@override`、 和 `@proxy`

你还可以定义自己的元数据注解。 下面的示例定义了一个带有两个参数的 @todo 注解：

```java
library todo;

class todo {
  final String who;
  final String what;

  const todo(this.who, this.what);
}
```

使用

```java
import 'todo.dart';

@todo('seth', 'make this do something')
void doSomething() {
  print('do something');
}
```

元数据可以在 library、 class、 typedef、 type parameter、 constructor、 factory、 function、 field、 parameter、或者 variable 声明之前使用，也可以在 import 或者 export 指令之前使用。 使用反射可以在运行时获取元数据 信息。



### 参考

- 中文官方文档 [http://dart.goodev.org/guides/language](http://dart.goodev.org/guides/language)
- https://juejin.im/post/5b5005866fb9a04fea589561#heading-14
- https://juejin.im/post/5bcc8527f265da0aff17729a#heading-2
- https://segmentfault.com/a/1190000014396421