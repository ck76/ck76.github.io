[TOC]

### 一、前言

- ViewModel职责是为Activity或Fragment管理、请求数据，具体数据请求逻辑不应该写在ViewModel中，否
- ViewModel的职责会变得太重，此处需要一个引入一个Repository，负责数据请求相关工作。具体请参考 Android架构组件。
- ViewModel可以用于Activity内不同Fragment的交互，也可以用作Fragment之间一种解耦方式。
- ViewModel也可以负责处理部分Activity/Fragment与应用其他模块的交互。
- ViewModel生命周期（以Activity为例）起始于Activity第一次onCreate()，结束于Activity最终finish时。



`ViewModel`被设计用于存储并管理UI相关的数据，因此可以在配置变化时存活下来，例如当屏幕旋转的时候。

app组件，例如`Activity`和`Fragment`拥有可以被安卓框架所管理的生命周期。框架可以决定销毁或创建它们，这是基于一些用户的行为或者设备的事件，而这一切都在你的控制范围外。

因为一些对象可能会被操作系统销毁或重新创建，任何你所持有的数据都会丢失。例如，你的`Activity`拥有一组用户列表，当`Activity`由于配置变化而重新创建时，新的`Activity`不得不重新获取用户列表。对于简单的数据，`Activity`可以使用`onSaveInstanceState()`方法并从`onCreate()`的`bundle`里恢复数据，但是这种方法仅仅适合于少量数据，例如UI状态，不适用于大量数据，例如一组用户列表。

另一个问题是，这些UI控制器（`Activity`、`Fragment`等）需要频繁地异步调用，可能会需要一些时间返回。UI控制器需要管理这些调用，并当被销毁的时候清除它们，以避免潜在的内存泄漏。这需要很多的代码维护，并且在由于配置变化而重新创建的情况下是很浪费资源的，因为需要重新进行相同的调用。

另外，这些UI控制器已经响应用户行为或处理操作系统交互。当它们也需要手动处理资源的时候，会使得类急速膨胀。上帝的Activity或上帝的Fragment，是指一个试图处理所有app工作的单独类，而不是分派到其他类去完成。这样也会使得测试工作很难进行。

将我们的UI和控制逻辑分离变得越来越容易和高效了。`Lifecycle`提供了一个新类叫做`ViewModel`，一个负责为UI准备数据的帮助类。`ViewModel`会在配置发生变化的时候自动保存，所以其所持有的数据会在新的`Activity`或新的`Fragment`立即可用。在上面我们所提及的例子中，应当是`ViewModel`的职责来获取并保持用户列表，而不是`Activity`或`Fragment`。

```java
public class MyViewModel extends ViewModel {
    private MutableLiveData<List<User>> users;
    public LiveData<List<User>> getUsers() {
        if (users == null) {
            users = new MutableLiveData<List<Users>>();
            loadUsers();
        }
        return users;
    }

    private void loadUsers() {
        // 异步调用获取用户列表
    }
}
```

现在新的`Activity`如下：

```java
public class MyActivity extends AppCompatActivity {
    public void onCreate(Bundle savedInstanceState) {
        MyViewModel model = ViewModelProviders.of(this).get(MyViewModel.class);
        model.getUsers().observe(this, users -> {
            // 更新 UI
        });
    }
}
```

如果`Activity`被重新创建了，它会收到被之前`Activity`创建的相同`MyViewModel`实例。当所属`Activity`终止后，框架调用`ViewModel`的`onCleared()`方法清除资源。

> 因为`ViewModel`在指定的`Activity`或`Fragment`实例外存活，它应该永远不能引用一个View，或持有任何包含`Activity context`引用的类。如果`ViewModel`需要`Application`的context（如获取系统服务），可以扩展`AndroidViewmodel`，并拥有一个构造器接收`Application`。



### 二、在`Fragment`间共享数据

一个`Activity`中的多个`Fragment`相互通讯是很常见的。每个`Fragment`需要定义接口描述，所属`Activity`将二者捆绑在一起。此外，每个`Fragment`必须处理其他`Fragment`未创建或不可见的情况

通过使用`ViewModel`可以解决这个痛点。想象一种情况，一个`Fragment`从列表项中选择一项，在另一个`Fragment`显示被选中项的内容。

这些`Fragment`可以使用它们的`Activity`共享`ViewModel`来处理通讯：

```java
public class SharedViewModel extends ViewModel {
    private final MutableLiveData<Item> selected = new MutableLiveData<Item>();

    public void select(Item item) {
        selected.setValue(item);
    }

    public LiveData<Item> getSelected() {
        return selected;
    }
}

public class MasterFragment extends Fragment {
    private SharedViewModel model;
    public void onActivityCreated() {
        model = ViewModelProviders.of(getActivity()).get(SharedViewModel.class);
        itemSelector.setOnClickListener(item -> {
            model.select(item);
        });
    }
}

public class DetailFragment extends LifecycleFragment {
    public void onActivityCreated() {
        SharedViewModel model = ViewModelProviders.of(getActivity()).get(SharedViewModel.class);
        model.getSelected().observe(this, { item ->
           // update UI
        });
    }
}
```

请注意，两个`Fragment`都使用了`getActivity()`，以通过`ViewModelProviders`获取`SharedViewModel`。

这种方式的好处包括：

- `Activity`不需要做任何事情，也不需要知道通讯的事情
- `Fragment`不需要知道彼此，除了`SharedViewModel`进行联系。如果它们(`Fragment`)其中一个消失了，其余的仍然能够像往常一样工作
- 每个`Fragment`有自己的生命周期，而且不会受其它`Fragment`生命周期的影响。事实上，一个`Fragment`替换另一个`Fragment`，UI的工作也不会受到任何影响。



### 三、ViewModel的生命周期

`ViewModel`对象的范围由获取`ViewModel`时传递至`ViewModelProvider`的`Lifecycle`所决定。`ViewModel`始终处在内存中，直到`Lifecycle`永久地离开—对于`Activity`来说，是当它终止（finish）的时候，对于`Fragment`来说，是当它分离（detached）的时候。



![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/ROwEqPbA0umqIf9UXNdp.jThRYBh8D*dcW5qjp.pdy8!/r/dDEBAAAAAAAA)

### 四、链接

- LiveData + ViewModel + Room (Google 官文)+Demohttps://juejin.im/post/5a1f983cf265da43252913f8
- viewmodel原理https://juejin.im/post/5bea29e5e51d456fac51243f

- [ViewModel来龙去脉](https://www.jianshu.com/p/e8955f525f4c?utm_campaign=maleskine&utm_content=note&utm_medium=seo_notes&utm_source=recommendation)

