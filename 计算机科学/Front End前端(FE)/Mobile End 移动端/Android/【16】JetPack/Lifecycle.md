[TOC]

### 一、前言

`android.arch.lifecycle`包提供了类和接口，使得你可以构建“生命周期敏感的组件”——自动适应当前`Fragment`或者`Activity`的组件。

> 如何引入`android.arch.lifecycle`到你的安卓项目中，详见[adding components to your project]。

安卓框架所定义的大多数组件都有相关的生命周期。生命周期被操作系统或运行在你程序中的框架代码所管理。它们是安卓运行的核心，你的应用必须遵守它们，不要做触发内存泄漏或应用崩溃的事情。

想象一下我们有一个在屏幕上显示位置的`Activity`，一个常见的实现大概像这样：

```java
class MyLocationListener {
    public MyLocationListener(Context context, Callback callback) {
        // ...
    }

    void start() {
        //连接至系统位置服务
    }

    void stop() {
        // 从系统位置服务断开
    }
}

class MyActivity extends AppCompatActivity {
    private MyLocationListener myLocationListener;

    public void onCreate(...) {
        myLocationListener = new MyLocationListener(this, (location) -> {
            // 更新UI
        });
  }

    public void onStart() {
        super.onStart();
        myLocationListener.start();
    }

    public void onStop() {
        super.onStop();
        myLocationListener.stop();
    }
}
```

即使这个样例代码看起来还不错，但是在实际的app中，由于拥有过多的`start()`和`stop`而导致`onStart()`和`onStop()`非常巨大。

此外，一些组件不能只在`onStart()`中开始。如果我们需要在开始位置观察前检查一些配置怎么办？某种情况下很可能检查会在`Activity`停止后结束，这意味着`myLocationListener.start()`会在`myLocationListener.stop()`之后调用，基本上会一直保持连接。

```java
class MyActivity extends AppCompatActivity {
    private MyLocationListener myLocationListener;

    public void onCreate(...) {
        myLocationListener = new MyLocationListener(this, location -> {
            // 更新UI
        });
    }

    public void onStart() {
        super.onStart();
        Util.checkUserStatus(result -> {
            //如果回调在activity停止后调用呢？
            if (result) {
                myLocationListener.start();
            }
        });
    }

    public void onStop() {
        super.onStop();
        myLocationListener.stop();
    }
}
```

`android.arch.lifecycle`包提供了类和接口，帮助你解决这个问题。



### 二、Lifecycle类

`Lifecycle`是一个**持有某组件生命周期状态的类**，**并允许其他对象观察这一状态。**

`Lifecycle`使用两个枚举类型来跟踪相关联组件的生命周期状态。

- Event（事件）:生命周期事件由框架和`Lifecycle`类分发。这些事件映射到`Activity`和`Fragment`的回调。
- State（状态）：由`Lifecycle`对象所跟踪组件的当前状态。



![img](http://s191.photo.store.qq.com/psb?/V14L47VC0w3vOf/0TESRDUzeF98xGA.SRytLSmI6vyjEHh3sBFPtQRSb28!/b/dL8AAAAAAAAA)

请将状态想成图的节点，将事件想成两个节点的边。
一个可以显示组件生命周期状态的类通过添加注解到其方法中来实现：

```java
public class MyObserver implements LifecycleObserver {
    @OnLifecycleEvent(Lifecycle.Event.ON_RESUME)
    public void onResume() {
    }

    @OnLifecycleEvent(Lifecycle.Event.ON_PAUSE)
    public void onPause() {
    }
}
aLifecycleOwner.getLifecycle().addObserver(new MyObserver());
```



### 三、LifecycleOwner

`LifecycleOwner`是一个单一方法**的接口**，**表示该类拥有一个`Lifecycle`。它只有一个方法且必须实现：`getLifecycle()`。**

这个类从各自单独的类（`Activity`和`Fragment`）抽象了`Lifecycle`的拥有者，并允许编写组件和它们共同工作。任何自定义的应用类可以实现`LifecycleOwner`接口。

> 由于架构组件当前处在非正式版中，我们无法从一个稳定的组件中添加一个不稳定的API，因此`Fragment`和`AppCompatActivity`类暂时无法实现这些接口。目前可以使用`LifecycleActivity`和`LifecycleFragment`，当Lifecycles稳定以后，`Fragment`和`AppCompatActivity`将实现`LifecycleOwner`；`LifecycleActivity`和`LifecycleFragment`将会被弃用。

对于上述的例子我们可以使我们的`MyLocationListener`类扩展自`LifecycleObserver`，之后使用`onCreate`中的Lifecycle初始化它。这使得`MyLocationListener`类是自足的，意味着当必要的时候可以使用自己的清除（cleanup）方法。

```java
class MyActivity extends AppCompatActivity {
    private MyLocationListener myLocationListener;

    public void onCreate(...) {
        myLocationListener = new MyLocationListener(this, getLifecycle(), location -> {
            // update UI
        });
        Util.checkUserStatus(result -> {
            if (result) {
                myLocationListener.enable();
            }
        });
  }
}
```

一个常见的用例是，如果`Lifecycle`并不处在良好的状态，则立刻避免调用具体的回调。

为了使这变的简单，`**Lifecycle`类允许其他对象查询当前的状态。**



### 四、LifecycleObserver

```java
class MyLocationListener implements LifecycleObserver {
    private boolean enabled = false;
    public MyLocationListener(Context context, Lifecycle lifecycle, Callback callback) {
       ...
    }

    @OnLifecycleEvent(Lifecycle.Event.ON_START)
    void start() {
        if (enabled) {
           // 连接
        }
    }

    public void enable() {
        enabled = true;
        if (lifecycle.getState().isAtLeast(STARTED)) {
            // 如果未连接则连接
        }
    }

    @OnLifecycleEvent(Lifecycle.Event.ON_STOP)
    void stop() {
        // 如果连接则断开连接
    }
```

在这种实现下，我们的`LocationListener`类是完全生命周期敏感的，它可以做自己的初始化以及清除工作，而不需要被`Activity`管理。如果我们需要从另一个`Activity`或者`Fragment`使用自己的`LocationListener`，我们只需要初始化它就可以了。所有的搭建和拆除操作都由该类自己管理。

可以和`Lifecycle`一同工作的类被叫做生命周期敏感的组件。需要和Android生命周期共同工作的类是值得提倡变成生命周期敏感组件的。因此它们的客户端可以轻易地整合这些类，而不需要手动管理客户端的生命周期。

**`LiveData`是一个生命周期敏感组件的样例。将`LiveData`和`ViewModel`一起使用可以更轻易地构建遵循Android生命周期的app。**



### 五、Lifecycles的最佳实践

- 保持你的UI控制器（activities， fragments）体积尽可能地瘦小。它们不应该尝试获得自己的数据，使用`ViewModel`去做这件事，然后观察`LiveData`以响应数据的变化至UI。
- 尝试编写数据驱动的UI，你的UI控制器仅仅负责当数据变化时更新UI，或通知至`ViewModel`。
- 将你的数据逻辑放到你的`ViewModel`类中，`ViewModel`**应该作为你的UI控制器和app剩余部分的连接器。**然而请小心，`ViewModel`的职责并不在于获取数据（例如从网路中获取数据）。相反`ViewModel`应当调用正确的组件去做这件工作，之后给UI控制器提供结果。
- 使用数据绑定在你的视图和UI控制器之间维护一个清晰的接口。这允许你在activities/fragments更新最少的代码。如果你想在Java这样做，使用[Butter Knife]这样的类库来避免模板代码，以及拥有更好的抽象。
- 如果你的UI很复杂，考虑创建一个Presenter类来处理UI的修改。
- **永远不要在你的`ViewModel`中引用`View`或者`Activity`的`Context`。**如果`ViewModel`在`Activity`之外存活，你的`Activity`将会被泄漏并且不能正确地被回收。



### 六、附加

#### 1、在自定义Activity和Fragment中实现LifecycleOwner

任何自定义的`Fragment`或`Activity`都可以通过实现内置的`LifecycleRegistryOwner`变成`LifecycleOwner`（从而不需要扩展自`LifecycleFragment`或者`LifecycleActivity`）。

```java
public class MyFragment extends Fragment implements LifecycleRegistryOwner {
    LifecycleRegistry lifecycleRegistry = new LifecycleRegistry(this);

    @Override
    public LifecycleRegistry getLifecycle() {
        return lifecycleRegistry;
    }
}
```



### 七、链接

- https://blog.csdn.net/mq2553299/article/details/79029657