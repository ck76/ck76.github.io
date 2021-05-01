[TOC]

作者：hicc, 腾讯 CSIG 工程师

最爱折腾的就是前端工程师了，从jQuery折腾到AngularJs，再折腾到Vue、React。 最爱跨屏的也是前端工程师，从phonegap，折腾到React Native，这不又折腾到了Flutter。

图啥？

**低成本**地为用户带来**更优秀的用户体验**。

目前来说[Flutter](https://link.zhihu.com/?target=https%3A//flutter.dev/)可能是其中最优秀的一种方案了。

## **Flutter是什么？**

> Flutter is Google’s UI toolkit for building beautiful, natively compiled applications for [mobile](https://link.zhihu.com/?target=https%3A//flutter.dev/docs), [web](https://link.zhihu.com/?target=https%3A//flutter.dev/web), and [desktop](https://link.zhihu.com/?target=https%3A//flutter.dev/desktop) from a single codebase.

[Flutter](https://link.zhihu.com/?target=https%3A//flutter.dev/)是由原Google Chrome团队成员，利用Chrome 2D渲染引擎，然后精简CSS布局演变而来。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqio9lhaj30zy0im0u3.jpg)

或者更详细的版本

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqinabs5j31l60u0wnw.jpg)

- Flutter在各个原生的平台中，使用自己的C++的引擎渲染界面，没有使用webview，也不像RN、NativeScript一样使用系统的组件。简单来说平台只是给Flutter提供一个画布。
- 界面使用Dart语言开发，貌似唯一支持JIT，和AOT模式的强类型语言。
- 写法非常的现代，声明式，组件化，Composition > inheritance，响应式……就是现在前端流行的这一套
- 一套代码搞定所有平台。

## **Flutter 为什么快？Flutter 相比RN的优势在哪里？**

从架构中实际上已经能看出Flutter为什么快，至少相比之前的当红炸子鸡React Native快的原因了。

- Skia 引擎，Chrome， Chrome OS，Android，Firefox，Firefox OS都以此作为渲染引擎。
- Dart语言可以AOT编译成ARM Code，让布局以及业务代码运行的最快，而且Dart的GC针对Flutter频繁销毁创建Widget做了专门的优化。
- CSS的的子集Flex like的布局方式，保留强大表现能力的同时，也保留了性能。
- Flutter业务书写的Widget在渲染之前diff转化成Render Object，对，就像React中的Virtual DOM，以此来确保开发体验和性能。

而相比React Native：

- RN 使用JavaScript来运行业务代码，然后JS Bridge的方式调用平台相关组件，性能比有损失，甚至平台不同js引擎都不一样。
- RN 使用平台组件，行为一致性会有打折，或者说，开发者需要处理更多平台相关的问题。

而具体两者的性能测试，可以看[这里](https://link.zhihu.com/?target=https%3A//www.yuque.com/xytech/flutter/gs3pnk)，结论是Flutter，在CPU，FPS，内存稳定上均优于ReactNative。

## **Dart 语言**

在开始Flutter之前，我们需要先了解下Dart语言……

Dart是由Google开发，最初是想作为JavaScript替代语言，但是失败沉寂之后，作为Flutter独有开发语言又焕发了第二春 。

实际上即使到了2.0，[Dart语法](https://link.zhihu.com/?target=https%3A//dart.dev/guides/language/language-tour)和JavaScript[Flutter](https://link.zhihu.com/?target=https%3A//flutter.dev/)非常的相像。单线程，Event Loop……

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqilcccaj30mc0m374z.jpg)

当然作为一篇写给前端工程师的教程，我在这里只想写写JavaScript中暂时没有的，Dart中更为省心，也更“甜”的东西。

- 不会飘的`this`
- 强类型，当然前端现在有了TypeScript
- 强大方便的操作符号：
- `?.` 方便安全的`foo?.bar`取值，如果foo为`null`，那么取值为`null`
- `??``condition ? expr1 : expr2` 可以简写为`expr1 ?? expr2`
- `=`和其他符号的组合: `*=`、`~/=`、`&=`、`|=` ……
- 级联操作符(Cascade notation ..)

```text
// 想想这样省了多少变量声明
querySelect('#button')
 ..text ="Confirm"
 ..classes.add('important')
 ..onClick.listen((e) => window.alert('Confirmed'))
```

甚至可以重写操作符

```text
class Vector {
  final int x, y;

  Vector(this.x, this.y);

  Vector operator +(Vector v) => Vector(x + v.x, y + v.y);
  Vector operator -(Vector v) => Vector(x - v.x, y - v.y);

  // Operator == and hashCode not shown. For details, see note below.
  // ···
}

void main() {
  final v = Vector(2, 3);
  final w = Vector(2, 2);

  assert(v + w == Vector(4, 5));
  assert(v - w == Vector(0, 1));
}
```

注：*重写`==`，也需要重写Object `hashCode`getter*

```text
class Person {
  final String firstName, lastName;

  Person(this.firstName, this.lastName);

  // Override hashCode using strategy from Effective Java,
  // Chapter 11.
  @override
  int get hashCode {
    int result = 17;
    result = 37 * result + firstName.hashCode;
    result = 37 * result + lastName.hashCode;
    return result;
  }

  // You should generally implement operator == if you
  // override hashCode.
  @override
  bool operator ==(dynamic other) {
    if (other is! Person) return false;
    Person person = other;
    return (person.firstName == firstName &&
        person.lastName == lastName);
  }
}

void main() {
  var p1 = Person('Bob', 'Smith');
  var p2 = Person('Bob', 'Smith');
  var p3 = 'not a person';
  assert(p1.hashCode == p2.hashCode);
  assert(p1 == p2);
  assert(p1 != p3);
}
```

这点在diff对象的时候尤其有用。

### **lsolate**

Dart运行在独立隔离的iSolate中就类似JavaScript一样，单线程事件驱动，但是Dart也开放了创建其他isolate，充分利用CPU的多和能力。

```text
loadData() async {
   // 通过spawn新建一个isolate，并绑定静态方法
   ReceivePort receivePort =ReceivePort();
   await Isolate.spawn(dataLoader, receivePort.sendPort);

   // 获取新isolate的监听port
   SendPort sendPort = await receivePort.first;
   // 调用sendReceive自定义方法
   List dataList = await sendReceive(sendPort, 'https://hicc.me/posts');
   print('dataList $dataList');
}

// isolate的绑定方法
static dataLoader(SendPort sendPort) async{
   // 创建监听port，并将sendPort传给外界用来调用
   ReceivePort receivePort =ReceivePort();
   sendPort.send(receivePort.sendPort);

   // 监听外界调用
   await for (var msg in receivePort) {
     String requestURL =msg[0];
     SendPort callbackPort =msg[1];

     Client client = Client();
     Response response = await client.get(requestURL);
     List dataList = json.decode(response.body);
     // 回调返回值给调用者
     callbackPort.send(dataList);
  }    
}

// 创建自己的监听port，并且向新isolate发送消息
Future sendReceive(SendPort sendPort, String url) {
   ReceivePort receivePort =ReceivePort();
   sendPort.send([url, receivePort.sendPort]);
   // 接收到返回值，返回给调用者
   return receivePort.first;
}
```

当然Flutter中封装了[compute](https://link.zhihu.com/?target=https%3A//api.flutter.dev/flutter/foundation/compute.html)，可以方便的使用，譬如[在其它isolate中解析大的json](https://link.zhihu.com/?target=https%3A//flutter.dev/docs/cookbook/networking/background-parsing)。

### **Dart UI as Code**

在这里单独提出来的意义在于，从React开始，到Flutter，到最近的Apple SwiftUI，Android Jetpack Compose 声明式组件写法越发流行，Web 前端使用JSX来让开发者更方便的书写，而Flutter，SwiftUI则直接从优化语言本身着手。

### **函数类的命名参数**

```text
void test({@required int age,String name}) {
  print(name);
  print(age);
}
// 解决函数调用时候，参数不明确的问题
test(name:"hicc",age: 30)

// 这样对于组件的使用尤为方便
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
  return Scaffold(
      appBar: AppBar(),
      body: Container(),
      floatingActionButton:FloatingActionButton()
    );
  }
}
```

### **大杀器：Collection If 和 Collection For**

```text
// collection If
Widget build(BuildContext context) {
  return Row(
    children: [
      IconButton(icon: Icon(Icons.menu)),
      Expanded(child: title),
      if (!isAndroid)
        IconButton(icon: Icon(Icons.search)),
    ],
  );
}

// Collect For
var command = [
  engineDartPath,
  frontendServer,
  for (var root in fileSystemRoots) "--filesystem-root=$root",
  for (var entryPoint in entryPoints)
    if (fileExists("lib/$entryPoint.json")) "lib/$entryPoint",
  mainPath
];
```

更多Dart 2.3对此的优化看[这里](https://link.zhihu.com/?target=https%3A//medium.com/dartlang/making-dart-a-better-language-for-ui-f1ccaf9f546c)。

## **Flutter 怎么写**

到这里终于到正题了，如果熟悉web前端，熟悉React的话，你会对下面要讲的异常的熟悉。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqii78tgj30l607oaai.jpg)

Flutter App的一切从`lib/main.dart`文件的main函数开始：

```text
import 'package:flutter/material.dart';

void main() => runApp(MyApp());

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Welcome to Flutter',
      home: Scaffold(
        appBar: AppBar(
          title: Text('Welcome to Flutter'),
        ),
        body: Center(
          child: Text('Hello World'),
        ),
      ),
    );
  }
}
```

Dart 类build方法返回的便是Widget，在Flutter中一切都是Widget，包括但不限于

- 结构性元素，menu，button等
- 样式类元素，font，color等
- 布局类元素，padding，margin等
- 导航
- 手势

Widget是Dart中特殊的类，通过实例化(Dart 中[new是可选的](https://link.zhihu.com/?target=https%3A//dart.dev/guides/language/language-tour%23using-constructors))相互嵌套，你的这个App就是形如下图的一颗组件树(Dart入口函数的概念，`main.dart -> main()`)。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqig74qcj31fi0sw0u1.jpg)

### **Widget布局**

上说过Flutter布局思路来自CSS，而Flutter中一切皆Widget，因此整体布局也很简单：

- 容器组件Container
- decoration装饰属性，设置背景色，背景图，边框，圆角，阴影和渐变等
- margin
- padding
- alignment
- width
- height
- Padding，Center
- Row,Column,Flex
- Wrap, Flow流式布局
- Stack， Z轴布局
- ……

[更多可以看这里](https://link.zhihu.com/?target=https%3A//www.yuque.com/xytech/flutter/hc0xq7)

Flutter中Widget可以分为三类，形如React中“展示组件”、“容器组件”，“context”。

### **StatelessWidget**

这个就是Flutter中的“展示组件”，自身不保存状态，外部参数变化就销毁重新创建。Flutter建议尽量使用无状态的组件。

### **StatefulWidget**

状态组件就是类似于React中的“容器组件”了，Flutter中状态组件写法会稍微不一样。

```text
class Counter extends StatefulWidget {
  // This class is the configuration for the state. It holds the
  // values (in this case nothing) provided by the parent and used by the build
  // method of the State. Fields in a Widget subclass are always marked "final".

  @override
  _CounterState createState() => _CounterState();
}

class _CounterState extends State<Counter> {
  int _counter = 0;

  void _increment() {
    setState(() {
      // This call to setState tells the Flutter framework that
      // something has changed in this State, which causes it to rerun
      // the build method below so that the display can reflect the
      // updated values. If you change _counter without calling
      // setState(), then the build method won't be called again,
      // and so nothing would appear to happen.
      _counter++;
    });
  }

  @override
  Widget build(BuildContext context) {
    // This method is rerun every time setState is called, for instance
    // as done by the _increment method above.
    // The Flutter framework has been optimized to make rerunning
    // build methods fast, so that you can just rebuild anything that
    // needs updating rather than having to individually change
    // instances of widgets.
    return Row(
      children: <Widget>[
        RaisedButton(
          onPressed: _increment,
          child: Text('Increment'),
        ),
        Text('Count: $_counter'),
      ],
    );
  }
}
```

可以看到Flutter中直接使用了和React中同名的`setState`方法，不过不会有变量合并的东西，当然也有[生命周期](https://link.zhihu.com/?target=https%3A//segmentfault.com/a/1190000015211309)。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqidumlwj30dv0js0tb.jpg)

可以看到一个有状态的组件需要两个Class，这样写的原因在于，Flutter中Widget都是immmutable的，状态组件的状态保存在State中，组件仍然每次重新创建，Widget在这里只是一种对组件的描述，Flutter会diff转换成Element，然后转换成RenderObject才渲染。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqicie8vj31fo0u0gq2.jpg)

Flutter Widget更多的渲染流程可以看[这里](https://link.zhihu.com/?target=https%3A//www.yuque.com/xytech/flutter/tge705)。

实际上Widget只是作为组件结构一种描述，还可以带来的好处是，你可以更方便的做一些[主题性的组件](https://link.zhihu.com/?target=https%3A//flutter.dev/docs/development/ui/widgets), Flutter官方提供的[Material Components widgets](https://link.zhihu.com/?target=https%3A//flutter.dev/docs/development/ui/widgets/material)和[Cupertino (iOS-style) widgets](https://link.zhihu.com/?target=https%3A//flutter.dev/docs/development/ui/widgets/cupertino)质量就相当高，再配合Flutter亚秒级的[Hot Reload](https://link.zhihu.com/?target=https%3A//flutter.dev/docs/development/tools/hot-reload)，开发体验可以说挺不错的。

------

## **State Management**

`setState()`可以很方便的管理组件内的数据，但是Flutter中状态同样是从上往下流转的，因此也会遇到和React中同样的问题，如果组件树太深，逐层状态创建就显得很麻烦了，更不要说代码的易读和易维护性了。

### **InheritedWidget**

同样Flutter也有个`context`一样的东西，那就是`InheritedWidget`，使用起来也很简单。

```text
class GlobalData extends InheritedWidget {
  final int count;
  GlobalData({Key key, this.count,Widget child}):super(key:key,child:child);

  @override
  bool updateShouldNotify(GlobalData oldWidget) {
    return oldWidget.count != count;
  }

  static GlobalData of(BuildContext context) => context.inheritFromWidgetOfExactType(GlobalData);
}

class MyApp extends StatelessWidget {
  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: MyHomePage(title: 'Flutter Demo Home Page'),
    );
  }
}

class MyHomePage extends StatefulWidget {
  MyHomePage({Key key, this.title}) : super(key: key);

  final String title;

  @override
  _MyHomePageState createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {
  int _counter = 0;

  void _incrementCounter() {
      _counter++;
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(widget.title),
      ),
      body: GlobalData(
        count: _counter,
        child: Center(          
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: <Widget>[
              Text(
                'You have pushed the button this many times:',
              ),
              Text(
                '$_counter',
                style: Theme.of(context).textTheme.display1,
              ),
              Body(),
              Body2()
            ],
          ),
        ),
      ),
      floatingActionButton: FloatingActionButton(
        onPressed: _incrementCounter,
        tooltip: 'Increment',
        child: Icon(Icons.add),
      ),
    );
  }
}

class Body extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    GlobalData globalData = GlobalData.of(context);
    return Text(globalData.count.toString());
  }
}

class Body2 extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    // TODO: implement build
    GlobalData globalData = GlobalData.of(context);
    return Text(globalData.count.toString());
  }
```

具体实现原理可以参考[这里](https://link.zhihu.com/?target=https%3A//loveky.github.io/2018/07/18/how-flutter-inheritedwidget-works/)，不过Google封装了一个更为上层的库[provider](https://link.zhihu.com/?target=https%3A//pub.dev/packages/provider)，具体使用可以看[这里](https://link.zhihu.com/?target=https%3A//flutter.dev/docs/development/data-and-backend/state-mgmt/simple)。

### **BlOC**

[BlOC](https://link.zhihu.com/?target=https%3A//medium.com/flutterpub/architecting-your-flutter-project-bd04e144a8f1)是Flutter team提出建议的另一种更高级的数据组织方式，也是我最中意的方式。简单来说：

**Bloc = InheritedWidget + RxDart(Stream)**

Dart语言中内置了Steam，Stream ~= Observable，配合[RxDart](https://link.zhihu.com/?target=https%3A//pub.dev/packages/rxdart), 然后加上`StreamBuilder`会是一种异常强大和自由的模式。

```text
class GlobalData extends InheritedWidget {
  final int count;
  final Stream<String> timeInterval$ = new Stream.periodic(Duration(seconds: 10)).map((time) => new DateTime.now().toString());
  GlobalData({Key key, this.count,Widget child}):super(key:key,child:child);

  @override
  bool updateShouldNotify(GlobalData oldWidget) {
    return oldWidget.count != count;
  }

  static GlobalData of(BuildContext context) => context.inheritFromWidgetOfExactType(GlobalData);

}

class TimerView extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    GlobalData globalData = GlobalData.of(context);
    return StreamBuilder(
        stream: globalData.timeInterval$,
        builder: (context, snapshot) {
          return Text(snapshot?.data ?? '');
        }
    );
  }
}
```

当然Bloc的问题在于

- 学习成本略高，Rx的概念要吃透，不然你会抓狂
- 自由带来的问题是，可能代码不如Redux类的规整。

顺便，今年Apple也拥抱了响应式，[Combine](https://link.zhihu.com/?target=https%3A//developer.apple.com/documentation/combine)(Rx like) + SwiftUI 也基本等于Bloc了。

所以，Rx还是要赶紧学起来

除去Bloc，Flutter中还是可以使用其他的方案，譬如：

- [Flutter Redux](https://link.zhihu.com/?target=https%3A//pub.dev/packages/flutter_redux)
- 阿里闲鱼的[Fish Redux](https://link.zhihu.com/?target=https%3A//github.com/alibaba/fish-redux)，*据说性能很好*。
- [Mobx](https://link.zhihu.com/?target=https%3A//mobx.pub/)
- ……

展开来说现在的前端开发使用强大的框架页面组装已经不是难点了。开发的难点在于如何组合富交互所需的数据，也就是上面图中的`state`部分。

更具体来说，是怎么优雅，高效，易维护地处理短暂数据(ephemeral state)`setState()`和需要共享的App State的问题，这是个工程性的问题，但往往也是日常开发最难的事情了，引用Redux作者Dan的一句：

> “The rule of thumb is:[Do whatever is less awkward](https://link.zhihu.com/?target=https%3A//github.com/reduxjs/redux/issues/1287%23issuecomment-175351978).”

到这里，主要的部分已经讲完了，有这些已经可以开发出一个不错的App了。剩下的就当成一个bonus吧。

------

## **测试**

Flutter debugger，测试都是出场自带，用起来也不难。

```text
// 测试在/test/目录下面
void main() {

  testWidgets('Counter increments smoke test', (WidgetTester tester) async {
    // Build our app and trigger a frame.
    await tester.pumpWidget(MyApp());

    // Verify that our counter starts at 0.
    expect(find.text('0'), findsOneWidget);
    expect(find.text('1'), findsNothing);

    // Tap the '+' icon and trigger a frame.
    await tester.tap(find.byIcon(Icons.add));
    await tester.pump();

    // Verify that our counter has incremented.
    expect(find.text('0'), findsNothing);
    expect(find.text('1'), findsOneWidget);
  });
}
```

## **包管理，资源管理**

类似与JavaScript的npm，Flutter，也就是Dart也有自己的[包仓库](https://link.zhihu.com/?target=https%3A//pub.dev/)。不过项目包的依赖使用yaml文件来描述:

```text
name: app
description: A new Flutter project.
version: 1.0.0+1

environment:
  sdk: ">=2.1.0 <3.0.0"

dependencies:
  flutter:
    sdk: flutter

  cupertino_icons: ^0.1.2
```

## **生命周期**

移动应用总归需要应用级别的生命周期，flutter中使用生命周期钩子，也非常的简单：

```text
class MyApp extends StatefulWidget {
  @override
  _MyAppState createState() => new _MyAppState();
}

class _MyAppState extends State<MyApp> with WidgetsBindingObserver {
  @override
  void initState() {
    super.initState();
    WidgetsBinding.instance.addObserver(this);
  }

  @override
  void dispose() {
    WidgetsBinding.instance.removeObserver(this);
    super.dispose();
  }

  @override
  void didChangeAppLifecycleState(AppLifecycleState state) {
    switch (state) {
      case AppLifecycleState.inactive:
        print('AppLifecycleState.inactive');
        break;
      case AppLifecycleState.paused:
        print('AppLifecycleState.paused');
        break;
      case AppLifecycleState.resumed:
        print('AppLifecycleState.resumed');
        break;
      case AppLifecycleState.suspending:
        print('AppLifecycleState.suspending');
        break;
    }
    super.didChangeAppLifecycleState(state);
  }

  @override
  Widget build(BuildContext context) {
      return Container();
  }
}
```

## **使用原生能力**

和ReactNative类似，Flutter也是使用类似事件的机制来使用平台相关能力。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqi9aw4yj30g40hzmxi.jpg)

## **Flutter Web, Flutter Desktop**

这些还在开发当中，鉴于对Dart喜欢，以及对Flutter性能的乐观，这些倒是很值得期待。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqi801rpj30vl0cxgmm.jpg)

还记得平台只是给Flutter提供一个画布么，Flutter Desktop未来更是可以大有可为 ，相关可以看[这里](https://link.zhihu.com/?target=https%3A//github.com/flutter/flutter/wiki/Desktop-shells)。

最后每种方案，每种技术都有[优缺点](https://link.zhihu.com/?target=https%3A//medium.com/asos-techblog/flutter-vs-react-native-for-ios-android-app-development-c41b4e038db9)，甚至技术的架构决定了，有些缺陷可能永远都没法改进，所以



更多干货，欢迎关注：[腾讯技术](https://www.zhihu.com/org/teng-xun-ji-zhu-gong-cheng/activities)



https://zhuanlan.zhihu.com/p/97653115