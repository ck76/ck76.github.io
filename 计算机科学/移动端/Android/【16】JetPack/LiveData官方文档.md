[TOC]

`LiveData`是一个数据持有类，保存了数据值以及允许该值被观察。并不像常规的被观察者，`LiveData`遵循app组件的生命周期，例如`Observer`可以指定要观察的`Lifecyle`。

如果`Observer`的`Lifecycle`是`STARTED`或者`RESUMED`状态，则`LiveData`认为其处在激活状态。

```java
public class LocationLiveData extends LiveData<Location> {
    private LocationManager locationManager;

    private SimpleLocationListener listener = new SimpleLocationListener() {
        @Override
        public void onLocationChanged(Location location) {
            setValue(location);
        }
    };

    public LocationLiveData(Context context) {
        locationManager = (LocationManager) context.getSystemService(
                Context.LOCATION_SERVICE);
    }

    @Override
    protected void onActive() {
        locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, listener);
    }

    @Override
    protected void onInactive() {
        locationManager.removeUpdates(listener);
    }
}
```

在`Location`监听器的实现中有三个重要的地方：

- `onActive()`：当`LiveData`拥有一个处在激活状态的观察者时会调用这个方法。这意味着我们需要开始观察位置信息的更新
- `onInactive()`：当`LiveData`并不拥有任何处在激活状态时这个方法被调用。既然没有任何观察者进行监听，那么没有理由继续保持对`LocationManager`服务的连接
- `setValue()`：调用该方法更新`LiveData`实例的值，并通知处在激活状态的观察者该变化

我们可以这样使用新的`LocationLiveData`：

```java
public class MyFragment extends LifecycleFragment {
    public void onActivityCreated (Bundle savedInstanceState) {
        LiveData<Location> myLocationListener = ...;
        Util.checkUserStatus(result -> {
            if (result) {
                myLocationListener.observer(this, location -> {
                    // 更新 UI
                });
            }
        });
    }
}
```

注意到`addObserver()`方法传递的第一个参数是`LifecycleOwner`，这表示观察者必然绑定至`Lifecycle`，这意味着：

- 如果`Lifecycle`并不处在激活状态，即使值发生变化，观察者也不会被响应
- 如果`Lifecycle`被销毁，观察者会被自动清除

`LiveData`是生命周期敏感的，这提供给了我们一个新的机会：我们可以在多个`Activity`、多个`Fragment`间共享。为了保持我们样例代码的简洁，我们将它变成单例：

```java
public class LocationLiveData extends LiveData<Location> {
    private static LocationLiveData sInstance;
    private LocationManager locationManager;

    @MainThread
    public static LocationLiveData get(Context context) {
        if (sInstance == null) {
            sInstance = new LocationLiveData(context.getApplicationContext());
        }
        return sInstance;
    }

    private SimpleLocationListener listener = new SimpleLocationListener() {
        @Override
        public void onLocationChanged(Location location) {
            setValue(location);
        }
    };

    private LocationLiveData(Context context) {
        locationManager = (LocationManager) context.getSystemService(
                Context.LOCATION_SERVICE);
    }

    @Override
    protected void onActive() {
        locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, listener);
    }

    @Override
    protected void onInactive() {
        locationManager.removeUpdates(listener);
    }
}
```

现在`Fragment`可以这样用：

```java
public class MyFragment extends LifecycleFragment {
    public void onActivityCreated (Bundle savedInstanceState) {
        Util.checkUserStatus(result -> {
            if (result) {
                LocationLiveData.get(getActivity()).observe(this, location -> {
                   // 更新 UI
                });
            }
        });
  }
}
```

可能会有多个`Fragment`和多个`Activity`观察我们的`MyLocationListener`实例，并且只要它们处在激活状态，我们的`LiveData`可以优雅地进行管理。

`LiveData`类具有如下的优点：

- 没有内存泄漏：因为`Obeserver`们被绑定在自己的`Lifecycle`对象，当`Lifecycle`被销毁的时候，它们会被自动清除
- 停止`Activity`的时候不会发生崩溃：如果`Obeserver`们的`Lifecycle`对象处在非激活状态（如`Activity`在后台），它们将不会接收任何变化事件
- 总是更新数据：如果`Lifecycle`再次启动（例如一个`Activity`从后台回到前台），它将会收到最新的本地数据
- 正确的配置变化：如果一个`Activity`或一个`Fragment`由于配置的变化（如屏幕旋转）重新创建，它将会立刻收到最新的`Location`数据
- 共享资源：现在我们可以持有一个单独的`MyLocationListener`实例，只需要连接到系统服务一次，并且可以正确地支持app里的所有观察者
- 不需要手动处理生命周期：你应该注意到，我们的`Fragment`仅仅当想要的时候观察数据，不需要关心如何停止，以及在停止后的再次开启。`LiveData`会自动管理所有的情况，因为`Fragment`提供了其`Lifecycle`。

## `LiveData`转换

有些时候，你可能想要在分发`LiveData`至观察者之前做一些变化，或者需要基于当前值返回另一个`LiveData`实例。

`Lifecycle`包提供了一个`Transformations`类，包含这些操作的辅助方法。

#### `Transformations.map()`

对`LiveData`值应用一个函数，传递结果至下流。

```java
LiveData<User> userLiveData = ...;
LiveData<String> userName = Transformations.map(userLiveData, user -> {
    user.name + " " + user.lastName
});
```

#### `Transformations.switchMap()`

和`map()`相似，传递至`switchMap()`的函数必须返回一个`Lifecycle`。

```java
private LiveData<User> getUser(String id) {
  ...;
}


LiveData<String> userId = ...;LiveData<User> user = Transformations.switchMap(userId, id -> getUser(id) );
```

使用这些转化允许通过链继续观察`Lifecycle`信息，例如这些信息只有当一个观察者观察返回`LiveData`的时才进行计算。这种惰性计算的特性允许在转化过程中隐式地传递生命周期，而不需要添加额外的调用或依赖。

当你在`ViewModel`里需要一个Lifecycle时，一个转化可能是一种解决方案。

例如，假设我们有一个UI界面，用户输入地址并接收地址的邮政编码。UI界面原始的`ViewModel`是这样的：

```java
class MyViewModel extends ViewModel {
    private final PostalCodeRepository repository;
    public MyViewModel(PostalCodeRepository repository) {
       this.repository = repository;
    }

    private LiveData<String> getPostalCode(String address) {
       // 不要这样做！
       return repository.getPostCode(address);
    }
}
```

实现如上，UI可能需要从之前的`LiveData`反注销并在每次调用`getPostalCode()`新的实例时重新注册。此外，如果UI是重新创建的，它出发了另一个调用`repository.getPostCode()`，而不是之前的结果。

作为上述方法的替换，你可以将邮政编码信息作为地址信息输入的转换：

```java
class MyViewModel extends ViewModel {
    private final PostalCodeRepository repository;
    private final MutableLiveData<String> addressInput = new MutableLiveData();
    public final LiveData<String> postalCode =
            Transformations.switchMap(addressInput, (address) -> {
                return repository.getPostCode(address);
             });

  public MyViewModel(PostalCodeRepository repository) {
      this.repository = repository
  }

  private void setInput(String address) {
      addressInput.setValue(address);
  }
}
```

注意到我们将`postalCode`设为`public final`，因为它永远不会改变。它被定义为`addressInput`的转化，因此当`addressInput`变化的时候，如果有一个激活的观察者，`repository.getPostCode()`会被调用。如果没有激活的观察者，则不会有任何计算发生，直到添加了一个观察者。

这种机制允许下层的应用创建`LiveData`对象，在需要的时候才计算。`ViewModel`可以轻易地获取它们并在上层定义转化规则。