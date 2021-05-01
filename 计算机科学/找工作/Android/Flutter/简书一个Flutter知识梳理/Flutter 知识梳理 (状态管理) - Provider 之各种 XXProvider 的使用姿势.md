[TOC]

# 一、前言

`Provider`是目前`Google`推荐的状态管理方式之一，建议大家可以先看一下 [Provider 的 Github 地址](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FrrousselGit%2Fprovider) 了解基本的用法。

网上大多数介绍`Provider`的文章讲的都是`ChangeNotifierProvider`，看完之后确实知道它是干什么的，以及怎么用。

然而其实还有其它的`Provider`供我们使用，那么它们之间的区别和联系是什么呢，官方文档对它们的使用也没有详细的`Demo`，这篇文章就来总结一下它们的用法和区别。

`Provider`的分类有如下几种：

- `Provider`
- `ListenableProvider`
- `ChangeNotifierProvider`
- `ValueListenableProvider`
- `StreamProvider`

# 二、Provider

## 2.1 构造函数

`Provider`的构造函数如下：



```dart
  Provider({
    Key key,
    @required ValueBuilder<T> builder,
    Disposer<T> dispose,
    Widget child,
  })
```

- `builder`：`T Function(BuildContext context)`，返回要共享的数据`Model`。
- `dispose`：`void Function(BuildContext context, T value)`，在回调中释放资源。

## 2.2 优点 & 缺点

`Provider`的优点：

- 数据共享：通过`Provider.of<T>(context)`方法，可以在以`Provider`为根节点的子树中获取到`T`的对象。
- 通过`dispose`参数，可以进行资源的释放。

但是`Provider`也有一个明显的缺点：

- **在共享数据发生改变时，不能通知它的监听者**。

## 2.3 计数器例子

下面我们用一个经典的计数器`Demo`演示一下`Provider`的使用，为了方便对比，后面在介绍其它`Provider`时，也使用该例子。

根据`Provider`的两个特点，我们可以用它来实现`BLoc`中`sink`的获取，以及最后资源的释放。

- 首先我们定义一个简单的`Bloc`模型。



```dart
import 'dart:async';

class ProviderBloc {

  int _count = 0;
  var _countController = StreamController<int>.broadcast();

  Stream<int> get stream => _countController.stream;
  int get count => _count;

  increment() {
    _countController.sink.add(++_count);
  }

  dispose() {
    _countController.close();
  }
}
```

- 接下来编写主界面。
  - 通过`Provider.of<ProviderBloc>(context)`我们可以在任意的地方获取到`ProviderBloc`对象，获得`sink`来更改数据。
  - 使用`StreamBuilder`监听`Stream`中数据的变化，刷新界面。
  - 在原始的`Bloc`模型上，顶层的`Provider`提供了`dispose`回调，用于资源的释放。



```dart
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'provider_bloc.dart';

void main() => runApp(_ProviderApp());

class _ProviderApp extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return Provider<ProviderBloc>(
      builder: (context) => ProviderBloc(),
      dispose: (context, bloc) => bloc.dispose(),
      child: MaterialApp(
        home: Scaffold(
          appBar: AppBar(title: Text('Provider Demo')),
          body: CounterLabel(),
          floatingActionButton: CounterButton(),
        ),
      ),
    );
  }
}

class CounterButton extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return FloatingActionButton(
      onPressed: Provider.of<ProviderBloc>(context).increment,
      child: const Icon(Icons.add),
    );
  }

}

class CounterLabel extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return Center(
      child: StreamBuilder<int>(
        builder: (context, snapshot) {
          return Text('you have push ${snapshot.data} times');
        },
        initialData: 0,
        stream: Provider.of<ProviderBloc>(context).stream,
      ),
    );
  }
}
```

# 三、ChangeNotifierProvider

`ChangeNotifierProvider`应该是大家见的最多的，大多数介绍`Provider`的文章都是以它为例子，**和 Provider 相比，它最大的优点就是解决了数据改变后无法监听的问题**。

## 3.1 构造函数

`ChangeNotifierProvider`的构造函数为如下：



```dart
  ChangeNotifierProvider({
    Key key,
    @required ValueBuilder<T> builder,
    Widget child,
  })
```

- `builder`：`T Function(BuildContext context)`，返回要共享的数据`Model`。

## 3.2 ChangeNotifier

使用`ChangeNotifierProvider`时，它要求`builder`返回的数据`Model`必须是`ChangeNotifier`的子类。

- 在改变数据后，调用`notifyListener()`方法。
- 通过重写它的`dispose`方法，可以完成和`Provider`一样的资源释放工作。



```dart
class Counter with ChangeNotifier {

  int _count = 0;
  int get count => _count;

  void increment() {
    _count++;
    notifyListeners();
  }

  @override
  void dispose() {
    super.dispose();
  }
}
```

## 3.3 Provider.of<T>(context) & Consumer

在介绍`Provider`的文章中，`Provider.of<T>(context)`和`Consumer`都会被拿来对比，一般都会推荐使用`Consumer`，**因为它会将数据发生变化后，把监听者的 Widget 重建的范围限制地更小**。

项目中`Provider`的使用，可以分为两个角色，数据改变的 **触发者** 和 **监听者**：

- 触发者：如果只是需要获取到数据`model`，不需要监听变化（例如点击按钮），推荐使用`Provider.of<Counter>(context, listen: false)`来获取数据`model`。
- 监听者：推荐使用`Consumer`。

## 3.4 例子

### 3.4.1 定义数据模型

- 首先，定义数据模型：
  - 在改变数据后，调用`notifyListeners`方法。
  - 如果需要释放资源，那么需要重写`dispose`方法。



```dart
import 'package:flutter/foundation.dart';

class Counter with ChangeNotifier {
  int _count = 0;
  int get count => _count;

  void increment() {
    _count++;
    notifyListeners();
  }
  
  @override
  void dispose() {
    super.dispose();
  }
}
```

### 3.4.2 主文件



```dart
import 'counter_model.dart';
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';

void main() => runApp(_ProviderApp());

class _ProviderApp extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return ChangeNotifierProvider<Counter>(
      builder: (context) => Counter(),
      child: MaterialApp(
        home: Scaffold(
          appBar: AppBar(title: Text('Provider Demo')),
          body: CounterLabel(),
          floatingActionButton: CounterButton(),
        ),
      )
    );
  }
}

class CounterButton extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return FloatingActionButton(
      onPressed: Provider.of<Counter>(context, listen : false).increment,
      child: const Icon(Icons.add),
    );
  }

}

class CounterLabel extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return Center(
      child: Consumer<Counter>(
          builder: (BuildContext context, Counter counter, Widget child) {
            return Text('you have push ${counter.count} times');
          }),
    );
  }
}
```

# 四、ListenableProvider

## 4.1 构造函数

`ListenableProvider`的构造函数为：



```dart
  ListenableProvider({
    Key key,
    @required ValueBuilder<T> builder,
    Disposer<T> dispose,
    Widget child,
  })
```

- `builder`：`T Function(BuildContext context)`，返回要共享的数据`Model`。
- `dispose`：`void Function(BuildContext context, T value)`，在回调中释放资源。

## 4.2 和 ChangeNotifierProvider 对比

先来一下`ChangeNotifierProvider`的源码：



```dart
class ChangeNotifierProvider<T extends ChangeNotifier>
    extends ListenableProvider<T> implements SingleChildCloneableWidget {
  
  static void _disposer(BuildContext context, ChangeNotifier notifier) =>
      notifier?.dispose();

  ChangeNotifierProvider({
    Key key,
    @required ValueBuilder<T> builder,
    Widget child,
  }) : super(key: key, builder: builder, dispose: _disposer, child: child);

}
```

从源码上可以看出，`ListenableProvider`和`ChangeNotifierProvider`其实是 **父与子的关系**，`ChangeNotifierProvider`在它的基础上：

- 限制数据`model`的上限，要求必须是`ChangeNotifier`的子类。
- 通过重写`ChangeNotifier.dispose()`来完成资源的释放，不需要传入`dispose`参数给`ChangeNotifierProvider`。

## 4.3 例子

使用`ListenableProvider`时，假如我们没有将数据模型定义成`ChangeNotifier`的子类，那么需要自己进行监听者的管理，为了方便，我们还是继续使用`ChangeNotifier`，其它地方的使用和`ChangeNotifierProvider`都是一样的。



```dart
import 'counter_model.dart';
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';

void main() => runApp(_ProviderApp());

class _ProviderApp extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return ListenableProvider<Counter>(
        builder: (context) => Counter(),
        dispose: (context, counter) => counter.dispose(),
        child: MaterialApp(
          home: Scaffold(
            appBar: AppBar(title: Text('Provider Demo')),
            body: CounterLabel(),
            floatingActionButton: CounterButton(),
          ),
        )
    );
  }
}

class CounterButton extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return FloatingActionButton(
      onPressed: Provider.of<Counter>(context, listen: false).increment,
      child: const Icon(Icons.add),
    );
  }

}

class CounterLabel extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return Center(
      child: Consumer<Counter>(
          builder: (BuildContext context, Counter counter, Widget child) {
            return Text('you have push ${counter.count} times');
          }),
    );
  }
}
```

# 五、ValueListenableProvider

## 5.1 构造函数



```dart
  ValueListenableProvider({
    Key key,
    @required ValueBuilder<ValueNotifier<T>> builder,
    UpdateShouldNotify<T> updateShouldNotify,
    Widget child,
  })
```

- `builder`：`T Function(BuildContext context)`，返回要共享的数据`Model`。
- `dispose`：`void Function(BuildContext context, T value)`，在回调中释放资源。

## 5.2 ValueNotifier

`ValueListenableProvider`要求`builder`返回的对象必须是`ValueNotifier<T>`的子类，`T`是需要共享的数据类型。

`ValueNotifier`的定义如下：



```dart
class ValueNotifier<T> extends ChangeNotifier implements ValueListenable<T> {

  ValueNotifier(this._value);

  @override
  T get value => _value;
  T _value;
  set value(T newValue) {
    if (_value == newValue)
      return;
    _value = newValue;
    notifyListeners();
  }

  @override
  String toString() => '${describeIdentity(this)}($value)';
}
```

`ValueNotifier`是我们前面多次提到的`ChangeNotifier`的子类，在改变`_value`时，自动调用了`notifyListeners`方法，那么就参照之前的方法来使用它。

## 5.3 例子

### 5.3.1 定义 ValueNotifier 的子类



```dart
import 'package:flutter/foundation.dart';

class CounterModel {

  int count;
  CounterNotifier wrapper;

  CounterModel(this.count);

}

class CounterNotifier extends ValueNotifier<CounterModel> {

  CounterNotifier(CounterModel value) : super(value) {
    value.wrapper = this;
  }

  @override
  void dispose() {
    super.dispose();
  }

}
```

### 5.3.2 主文件



```dart
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'counter_notifier.dart';

void main() => runApp(_ProviderApp());

class _ProviderApp extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return ValueListenableProvider<CounterModel>(
        builder: (context) => CounterNotifier(CounterModel(0)),
        updateShouldNotify: (model1, model2) {
          print('updateShouldNotify');
          return model1.count != model2.count;
        },
        child: MaterialApp(
          home: Scaffold(
            appBar: AppBar(title: Text('Provider Demo')),
            body: _CounterLabel(),
            floatingActionButton: _CounterButton(),
          ),
        )
    );
  }
}

class _CounterButton extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return FloatingActionButton(
      onPressed: () {
        CounterModel oldModel = Provider.of<CounterModel>(context, listen: false);
        CounterModel newModel = CounterModel(oldModel.count + 1);
        newModel.notifier = oldModel.notifier;
        oldModel.notifier.value = newModel;
        return;
      },
      child: const Icon(Icons.add),
    );
  }

}

class _CounterLabel extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return Center(
      child: Consumer<CounterModel>(
          builder: (BuildContext context, CounterModel model, Widget child) {
            return Text('you have push ${model.count} times');
          }),
    );
  }
}
```

## 5.4 疑问

这里有一个问题困扰了我很久：就是使用`ValueNotifier<T>`时必须要改变`_value`才会触发`notifyListeners()`方法，而通过`Provider.of<T>(context, listen: false)`拿到的对象是`_value`，因此还需要在它里面保存`ValueNotifier<T>`的引用（或者将`ValueNotifier`定义成单例的模式），再设置一次达到触发的效果，感觉用起来很奇怪，不知道是不是我的使用方式有问题，网上也没有找到相关的例子。

# 六、StreamProvider

`StreamProvider`用来结合`Bloc`使用。

## 6.1 构造函数

### 6.1.1 使用 Stream 构造



```dart
  StreamProvider({
    Key key,
    @required ValueBuilder<Stream<T>> builder,
    T initialData,
    ErrorBuilder<T> catchError,
    UpdateShouldNotify<T> updateShouldNotify,
    Widget child,
  })
```

- `builder`：返回`Bloc`中的`Stream`。
- `initialData`：初始数据。
- `catchError`：发生错误时候的回调。

### 6.1.2 使用 StreamController 构造



```dart
  StreamProvider.controller({
    Key key,
    @required ValueBuilder<StreamController<T>> builder,
    T initialData,
    ErrorBuilder<T> catchError,
    UpdateShouldNotify<T> updateShouldNotify,
    Widget child,
  })
```

- `builder`：返回`Bloc`中的`StreamController`。

## 6.2 例子

### 6.2.1 定义单例模式



```dart
import 'dart:async';

class ProviderBloc {

  static ProviderBloc _instance;

  ProviderBloc._internal() {
    print("_internal");
  }

  static ProviderBloc _getInstance() {
    if (_instance == null) {
      _instance = ProviderBloc._internal();
    }
    return _instance;
  }

  factory ProviderBloc() => _getInstance();
  static ProviderBloc get instance => _getInstance();

  int _count = 0;
  var _countController = StreamController<int>.broadcast();

  Stream<int> get stream => _countController.stream;
  int get count => _count;

  increment() {
    _countController.sink.add(++_count);
  }

  dispose() {
    _countController.close();
  }

}
```

### 6.2.2 主文件



```dart
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'provider_bloc.dart';

void main() => runApp(_ProviderApp());

class _ProviderApp extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return StreamProvider<int> (
        builder: (context) {
          return ProviderBloc().stream;
        },
        catchError: (BuildContext context, Object error) {},
        initialData: 0,
        child: MaterialApp(
          home: Scaffold(
            appBar: AppBar(title: Text('Provider Demo')),
            body: CounterLabel(),
            floatingActionButton: CounterButton(),
          ),
        )
    );
  }
}

class CounterButton extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return FloatingActionButton(
      onPressed: ProviderBloc().increment,
      child: const Icon(Icons.add),
    );
  }

}

class CounterLabel extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return Center(
      child: Consumer<int>(
          builder: (BuildContext context, int value, Widget child) {
            return Text('you have push $value times');
          }),
    );
  }
}
```

# 七、FutureProvider

## 7.1 构造函数



```dart
  FutureProvider({
    Key key,
    @required ValueBuilder<Future<T>> builder,
    T initialData,
    ErrorBuilder<T> catchError,
    UpdateShouldNotify<T> updateShouldNotify,
    Widget child,
  })
```

- `builder`：返回一个`Future<T>`对象，异步任务的返回结果，在结果返回后，会触发`Consumer`的重建。
- `initialData`：初始数据。
- `catchError`：发生错误的回调。

## 7.2 和 FutureBuilder 的区别

`FutureProvider`和我们之前讨论的`Provider`场景不太一样，它和`FutureBuilder`比较类似，就是在数据返回之前加载一个组件，等待数据返回值后，重绘返回另一个组件。

它和`FutureBuilder`的区别在于：

- `FutureBuilder`的数据请求和展示都是在一个组件当中，而`FutureProvider`是数据的请求者，`Consumer`是展示者。
- `FutureBuilder`的请求者和展示者是一对一的关系，而`FutureProvider`可以是一对多的关系。

## 7.3 例子



```dart
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';

void main() => runApp(_ProviderApp());

class _ProviderApp extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return FutureProvider<int>(
        builder: (context) => _request(),
        initialData: 0,
        child: MaterialApp(
          home: Scaffold(
            appBar: AppBar(title: Text('Provider Demo')),
            body: CounterLabel(),
          ),
        )
    );
  }

  Future<int> _request() async {
    return await Future<int>.delayed(Duration(milliseconds: 3000)).then((int value) {
        return 300;
    });
  }
}

class CounterLabel extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return Center(
      child: Column(children: <Widget>[
        Consumer<int>(
            builder: (BuildContext context, int count, Widget child) {
              return Text('Observer1=$count');
            }),
        Consumer<int>(
            builder: (BuildContext context, int count, Widget child) {
              return Text('Observer2=$count');
            }),
      ],)
    );
  }
}
```

# 七、小结

对比以上几种`Provider`的使用方式：还是`ChangeNotifierProvider`和`StreamProvider`比较符合我们平时的使用场景。

而其它三种：

- `Provider`：不能监听数据改变，直接淘汰。
- `ListenableProvider`：`ChangeNotifierProvider`的原始版本。
- `ValueListenableProvider`：`ChangeNotifierProvider`的增强版本，但是怎么感觉用起来还更麻烦了。



6人点赞



[Flutter]()





作者：泽毛
链接：https://www.jianshu.com/p/a36a67d46633
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。