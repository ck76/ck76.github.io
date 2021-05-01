[TOC]

在学习`Dart`的时候，会遇到一些`Java`中没有的概念或者用法，这篇文章总结了`Dart`和`Java`中一些不同，但又经常会用到的知识点。

# 一、构造函数初始化列表

初始化列表定义在构造函数`)`和`{`之间，初始化列表的语句之间用`,`分割，它的作用有如下几点。

## 1.1 为`final`变量赋值

我们希望类中的`final`变量初始值能由外部传入，但是在构造函数中为`final`变量赋值是不允许的，例如下面这样：



```dart
class ConstField {
  
  final value;
  
  ConstField(int y) {
    value = y;
  }
  
}
```

此时可以采用`Dart`提供的初始化列表的方式：



```dart
class ConstField {
  
  final value;
  
  ConstField(int input) : value = input;
  
}
```

只有初始化列表，没有构造函数体的时候，初始化列表最后用`;`结尾。

## 1.2 对输入参数进行判断



```dart
class ConstField {
  
  final value;
  
  ConstField(int input) : assert(input > 0), value = input;
  
}
```

## 1.3 调用父类的构造函数



```dart
class Parent {
  
  int value;
  
  Parent(int input);
  
}

class Child extends Parent {
  
  Child(int input) : assert(input > 0), super(input);
  
}
```

在有初始化列表的情况下，函数调用的先后顺序为：

- 子类初始化列表
- 父类构造函数
- 子类构造函数

# 二、const 构造函数

`const`构造函数可以保证你的类称为永远不会更改的对象，并使这些对象称为编译时常量：

- 定义`const`构造函数确保所有实例变量都是`final`的。
- 构造函数不能有函数体，但是可以有初始化列表。



```dart
class ConstConstructor {
  
  final value;
  
  const ConstConstructor(int input) : value = input;
  
}
```

# 三、实例变量的赋值

## 3.1 使用`this`简化赋值



```dart
class Subject {
  
  int value;
  int value2;
  
  Subject(this.value, this.value2);
  
  log() {
    print('value=$value, value2=$value2');
  }
  
}

void main() {
  Subject(2,3).log();
}
```

## 3.2 使用可选参数列表赋值或提供默认值

当我们希望 **只为某些变量赋值，其余的采用初始值** 时，在`Java`中只能采用定义多个构造函数或者使用`builder`模式，在`Dart`中采用可选参数列表的方式实现就很简单，调用的时候只需要采用 **参数名:值** 的形式赋值即可。



```dart
class Subject {
  
  int value;
  int value2;
  
  Subject({this.value, this.value2 = 3});
  
  log() {
    print('value=$value, value2=$value2');
  }
  
}

void main() {
  Subject(value : 2).log();
}
```

# 四、getter & setter

`get`和`set`是专门用来读取和写入对象的属性的方法，每一个类的实例变量都有一个隐式的`getter`和可能的`setter`（`final`或者`const`的只有`getter`）。

`setter/getter`使得我们在外部可以像对待普通成员变量进行操作，但是在内部进行额外的处理。

`set/get`后跟的是成员变量的名字，不可以与其它成员变量重名。



```dart
class Person {
  
  String _name;
      
  set name(String value) {
    assert(value != null);
    _name = value;
  }
  
  get name => 'Person name is $_name';
  
}

void main() {
  Person p = Person();
  p.name = "tom";
  print(p.name);
}
```

# 五、factory 修饰的构造函数

`factory` **用于修饰类的构造函数**。

例如，我们有一个`Shape`类，当我们用`factory`修饰：



```dart
factory Shapre() { return ...; }
```

再通过`Shape()`或者`new Shape()`的方式调用该函数时，不会真正地创建`Shape`，而是由该构造函数返回一个`Shape`或者`Shape`的子类。

它相当于为类提供了一个静态的创建方法，该方法的返回值是它本身或它的子类对象，但是外部并不知道这个方法的存在，只需要按照普通创建对象的方式创建即可。

根据这一特点，目前`factory`的应用有两类：

- 实现工厂模式。
- 单例模式。

## 5.1 工厂模式

`Java`中工厂模式的实现可以参考这里，[工厂模式](https://links.jianshu.com/go?to=https%3A%2F%2Fwww.runoob.com%2Fdesign-pattern%2Ffactory-pattern.html)，下面我们`Dart`的实现版本。



```java
class Shape {
  
  factory Shape(String shapeType) {
    if (shapeType.compareTo('CIRCLE') == 0) {
      return Square();
    } else {
      return Circle();
    }
  }
  
  void draw() {}
}

class Square implements Shape {
  
  @override
  void draw() {
    print('Inside Square::draw() method.');
  }
}

class Circle implements Shape {
  
  @override
  void draw() {
    print('Inside Circle::draw() method.');
  }
}

void main() {
  Shape shape = Shape('CIRCLE');
  shape.draw();
}
```

## 5.2 单例模式

回想一下，在`Java`中实现单例模式时，一般会将它的构造方法声明为`private`，再定义一个`getInstance()`的静态方法来返回单例对象。

而在`Dart`中，利用`factory`的特性，我们可以仍然按普通的方式创建对象，但是在多个地方获得的是同一个实例。



```dart
class Singleton {

  //单例对象。
  static Singleton _instance;

  //构造函数。
  Singleton._internal() {
    print("_internal");
  }

  static Singleton _getInstance() {
    //仅在 _instance 为空时才调用。
    if (_instance == null) {
      _instance = Singleton._internal();
    }
    return _instance;
  }
  
  //调用方法。
  void method() {
    print("method");
  }

  //1.通过 new Singleton() 调用。
  factory Singleton() => _getInstance();
  //2.通过 Singleton.instance 调用。
  static Singleton get instance => _getInstance();

}
```

调用方式有如下两种，`_instance`都会只创建一次：



```dart
Singleton.instance.method();
Singleton().method();
```

# 六、命名构造函数

在`Java`中，可以声明多个构造函数，但是会遇到一个问题，如果有多个构造函数接受的参数个数和类型都相同，那怎么办呢？`Dart`的命名参数就解决了这一个问题。

除此之外，`Dart`的命名构造函数可以使得含义更加直观。



```dart
class Point {
  
  int x1;
  int y1;
  
  Point({this.x1, this.y1});
  
  Point.coordinate100() {
    x1 = 100;
    y1 = 100;
  }
    
  Point.coordinate50() {
    x1 = 50;
    y1 = 50;
  }
  
  log() {
    print('x=$x1, y=$y1');
  }
  
}

void main() {
  Point p100 = Point.coordinate100();
  p100.log();
  Point p50 = Point.coordinate50();
  p50.log();
}
```

这里需要注意，当我们声明了命名构造函数，那么就会 **覆盖默认的无参构造函数**，这点和声明了普通的有参构造函数是一样的。也就是说，在它子类的构造函数中，需要通过`super.xxx`显示地调用父类中的构造函数。

# 七、操作符

在`Dart`中有一些操作符和`Java`不同。

## 7.1 类型操作符

- 判断对象是否是指定的类型，使用`is`或者`!is`，类似`Java`中的`instanceof`。



```dart
void main() {
   int n = 2;
   print(n is int);
}

void main() {
   double  n = 2.20;
   var num = n is! int;
   print(num);
}
```

- `a as T`：将`a`转换称为指定的类型`T`，类似于`Java`中的`(T)a`。

## 7.2 赋值运算符

- `??=`：仅在变量为`null`时才分配。



```dart
void main() {
  int a;
  a ??= 1;
  a ??= 2;
  print(a);
}
```

运行结果：



```dart
1
```

## 7.3 条件表达式

- `expr1 ?? expr2`：如果`expr1`不为`null`，那么返回`expr1`的值，否则返回`expr2`的值。



```dart
void main() {
  String a;
  String b = a ?? "default";
  print(b);
}
```

运行结果：



```dart
default
```

## 7.4 条件成员访问：判空+调用

- `?.`：仅当对象不为空时才执行后面的操作，省去了`Java`中先判空，再去调用方法的步骤。



```dart
class Person {
  
  String name;
  
  Person(this.name);
      
  log() {
    print(name);
  }
  
}

void main() {
  Person p;
  String name = p?.name;
  p?.log();
  p = Person('a');
  p.log();
}
```

## 7.5 级联操作符

`..`允许对同一对象执行一系列操作，可以省去创建临时变量的步骤。



```dart
class Person {
  
  String name;
  
  Person(this.name);
      
  log() {
    print(name);
  }
  
}

void main() {
  Person p = Person('a');
  p..name = 'b'
    ..log();
}
```

# 八、函数也是一种类型

在`Flutter`中，有很多将`Function`也就是方法作为成员变量的使用，其作用类似于`Java`当中的回调，当我们的回调接口中只有一个抽象函数时，可以使用这种方法代替。



```dart
typedef CalBuilder = int Function(int x, int y);

class Cal {
  
  CalBuilder builder;
  
  Cal(this.builder);
      
  log() {
    print('result=${builder(1, 2)}');
  }
  
}

void main() {
  Cal cal = Cal((a, b) => a + b);
  cal.log();
}
```

# 九、函数的可选参数

当函数执行时不需要强制传递参数时，可以使用可选参数，可选参数时函数中的最后一个参数。

## 9.1 可选位置参数

可选参数的语法为：



```dart
void function_name(param1, [param1, param2]) { }
```

示例如下：



```dart
void main() {
  func(1);
  func(1,'2');
}

void func(int a, [String b, int c]) {
  print(a);
  print(b);
  print(c);
}
```

注意：如果当赋值给可选参数的实参数目不够时，那么必须按照形参类型定义的顺序依次赋值，否则无法编译通过。

## 9.2 可选命名参数

与可选位置参数不同，可选命名参数要求必须在传递值时指定参数名称。



```dart
void function_name(a, {param1, param2}) { }
```

示例：



```dart
void main() {
  func(1);
  func(2, c:3);
}

void func(int a, {String b, int c}) {
  print(a);
  print(b);
  print(c);
}
```

## 9.3 带有默认值的可选参数

无论是可选位置参数还是可选命名参数，都可以定义默认值。语法为：



```dart
function_name(param1, {param2 = default_value}) { }
```

示例：



```dart
void main() {
  func(1);
  func(1,'2');
}

void func(int a, [String b = 'default', int c]) {
  print(a);
  print(b);
  print(c);
}
```

输出结果：



```dart
1
default
null
1
2
null
```

# 十、容器

在`Dart`中，常用的容器有`List`和`Map`，在使用上和`Java`也有一些区别。可以参考下面的两篇文章，就不列举了：

- [Dart 编程列表](https://links.jianshu.com/go?to=http%3A%2F%2Fcodingdict.com%2Farticle%2F21920)
- [Dart 编程 Map](https://links.jianshu.com/go?to=http%3A%2F%2Fcodingdict.com%2Farticle%2F21920)

# 参考文章

- [Dart 教程](https://links.jianshu.com/go?to=http%3A%2F%2Fcodingdict.com%2Farticle%2F21908)
- [Flutter 学习之 Dart 语言基础(构造函数)](https://links.jianshu.com/go?to=https%3A%2F%2Fblog.csdn.net%2Fsamlss%2Farticle%2Fdetails%2F88790306)
- [Dart 语言惯用语 —— Dart中特有的代码味道](https://links.jianshu.com/go?to=https%3A%2F%2Fcloud.tencent.com%2Finfo%2F85e2a4b3ca347b94122e6e5522a5888a.html)



作者：泽毛
链接：https://www.jianshu.com/p/da2f6719ff37
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。