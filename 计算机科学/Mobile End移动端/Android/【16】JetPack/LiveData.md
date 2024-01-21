[TOC]

### 一、概述

简单地说，LiveData是一个数据持有类。它具有以下特点：

- 数据可以被观察者订阅；
- 能够感知组件（Fragment、Activity、Service）的生命周期；
- 只有在组件出于激活状态（STARTED、RESUMED）才会通知观察者有数据更新；

- LiveData 是一个可感知生命周期的可观察类 (Observable)。它能帮您容易地确保被屏幕显示的资讯与数据的同步。其优点包括：

- 生命周期感知性：LiveData 与 Android 生命周期结合运行的效果良好。它仅会当 UI 被显示时才把数据往前端传递。

- 与 Room 无缝整合：LiveData 可被设为 Room 的回调类。观看有关 Room 的视频了解详情。

- 可与 ViewModel 和 Data Binding 混合使用，建立反应式 UI 。

- 提供基本数据转换方法，例如 switchMap 和 MediatorLiveData。



### 二、LiveData的优点

- 没有内存泄漏：因为 Observer 被绑定到它们自己的 Lifecycle 对象上，所以，当它们的 Lifecycle 被销毁时，它们能自动的被清理。

- 不会因为 activity 停止而崩溃：如果 Observer 的 Lifecycle 处于闲置状态（例如：activity 在后台时），它们不会收到变更事件。

- 始终保持数据最新：如果 **Lifecycle** 重新启动（例如：activity 从后台返回到启动状态）将会收到最新的位置数据（除非还没有）。

- 正确处理配置更改：如果 activity 或 fragment 由于配置更改（如：设备旋转）重新创建，将会立即收到最新的有效位置数据。

- 资源共享：可以只保留一个 MyLocationListener 实例，只连接系统服务一次，并且能够正确的支持应用程序中的所有观察者。
- 不再手动管理生命周期：**fragment 只是在需要的时候观察数据，不用担心被停止或者在停止之后启动观察。由于 fragment 在观察数据时提供了其 Lifecycle，所以 LiveData 会自动管理这一切。



### 三、LiveData的使用

LiveData有两种使用方式： 

- 使用LiveData对象 
- 继承LiveData类

#### 1、使用LiveData对象

1. 创建保存特定数据类型的LiveData实例；
2. 创建Observer对象，作为参数传入LiveData.observe()方法添加观察者；
3. 更新Livedata对象存储的数据；

##### 创建LiveData实例

```java
public class NameViewModel extends ViewModel{
    // Create a LiveData with a String
    private MutableLiveData<String> mCurrentName;
    // Create a LiveData with a String list
    private MutableLiveData<List<String>> mNameListData;

    public MutableLiveData<String> getCurrentName() {
        if (mCurrentName == null) {
            mCurrentName = new MutableLiveData<>();
        }
        return mCurrentName;
    }

    public MutableLiveData<List<String>> getNameList(){
        if (mNameListData == null) {
            mNameListData = new MutableLiveData<>();
        }
        return mNameListData;
    }
}
```

##### 创建Observer对象，添加观察者

```java
......
    private NameViewModel mNameViewModel;
......
    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mNameViewModel = ViewModelProviders.of(this).get(NameViewModel.class);
        mNameViewModel.getCurrentName().observe(this,(String name) -> {
            mTvName.setText(name);
            Log.d(TAG, "currentName: " + name);
        }); // 订阅LiveData中当前Name数据变化，以lambda形式定义Observer
        mNameViewModel.getNameList().observe(this, (List<String> nameList) -> {
            for (String item : nameList) {
                Log.d(TAG, "name: " + item);
            }
        }); // 订阅LiveData中Name列表数据变化，以lambda形式定义Observer
    }
```

##### 更新LiveData中的数据

```java
mNameViewModel.getCurrentName().setValue("Jane");//更新数据
```



#### 2、集成LiveData类

除了直接使用LiveDatad对象外，我们还可以通过继承LiveData类来定义适合特定需求的LiveData。下面继承LiveData类的例子，验证下LiveData的其中一个优点——资源共享。

```java
**
 * 集成LiveData类使用LiveData
 *
 * @author chengkun
 * @since 2018/12/19 19:31
 */
public class CustomLiveData extends LiveData<String> {
    private static CustomLiveData sInstance;

    @MainThread
    public static CustomLiveData get(Context context) {
        if (sInstance == null) {
            sInstance = new CustomLiveData();
        }
        return sInstance;
    }

    /**
     * 当LiveData拥有一个处在激活状态的观察者时会调用这个方法。
     */
    @Override
    protected void onActive() {
        super.onActive();
    }

    /**
     * 当LiveData并不拥有任何处在激活状态时这个方法被调用。
     */
    @Override
    protected void onInactive() {
        super.onInactive();
    }

    /**
     * 调用该方法更新LiveData实例的值，并通知处在激活状态的观察者该变化
     */
    @Override
    protected void setValue(String value) {
        super.setValue(value);
    }
}
```



### 四、转换

#### Transformations.map

在 [LiveData](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FLiveData.html) 的值上应用一个方法，并将结果传递到下游。

```java
  LiveData<User> userLiveData = ...;
  LiveData<String> userName = Transformations.map(userLiveData, user -> {
      user.name + " " + user.lastName
  });
```



#### Transformations.switchMap

与 [map()](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FTransformations.html%23map(android.arch.lifecycle.LiveData%3CX%3E%2C%2520android.arch.core.util.Function%3CX%2C%20Y%3E)) 类似，将一个方法应用到 [LiveData](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FLiveData.html) 的值并解包，然后将结果传递到下游。传递给 [switchMap()](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FTransformations.html%23switchMap(android.arch.lifecycle.LiveData%3CX%3E%2C%2520android.arch.core.util.Function%3CX%2C%2520android.arch.lifecycle.LiveData%3CY%3E%3E)) 的方法必须返回一个 [Lifecycle](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FLifecycle.html)

```java
  private LiveData<User> getUser(String id) {
      ...;
  }

  LiveData<String> userId = ...;
  LiveData<User> user = Transformations.switchMap(userId, id -> getUser(id) );
```

使用这些转换允许在整个调用链中携带观察者的 [Lifecycle](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FLifecycle.html) 信息，以便只有在观察者观察到 [LiveData](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FLiveData.html) 的返回时才运算这些转换。转换的这种惰性运算性质允许隐式的传递生命周期相关行为，而不必添加显式的调用或依赖。

每当你认为在 [ViewModel](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FViewModel.html) 中需要一个 [Lifecycle](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FLifecycle.html) 类时，转换可能是解决方案。

例如：假设有一个 UI，用户输入一个地址然后会收到该地址的邮政编码。该 UI 简单的 [ViewModel](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FViewModel.html) 可能像这样：

```java
class MyViewModel extends ViewModel {
    private final PostalCodeRepository repository;
    public MyViewModel(PostalCodeRepository repository) {
       this.repository = repository;
    }

    private LiveData<String> getPostalCode(String address) {
       // 不要这样做！！！
       return repository.getPostCode(address);
    }
}
```

如果是像这种实现，**UI 需要先从之前的 [LiveData](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FLiveData.html) 注销并且在每次调用 getPostalCode() 时重新注册到新的实例。此外，如果 UI 被重新创建，它将会触发新的 repository.getPostCode() 调用，而不是使用之前的调用结果。**

不能使用那种方式，而应该实现将地址输入转换为邮政编码信息。

```java
class MyViewModel extends ViewModel {
    private final PostalCodeRepository repository;
    private final MutableLiveData<String> addressInput = new MutableLiveData();
    此处是public## public final LiveData<String> postalCode =
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

请注意，我们甚至使 postalCode 字段为 public final，因为它永远不会改变。postalCode 被定义为 addressInput 的转换，所以当 addressInput 改变时，如果有处于活动状态的观察者，repository.getPostCode() 将会被调用。如果在调用时没有处于活动状态的观察者，在添加观察者之前不会进行任何运算。

该机制允许以较少的资源根据需要惰性运算来创建 [LiveData](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FLiveData.html)。[ViewModel](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FViewModel.html) 可以轻松获取到  [LiveData](https://link.juejin.im?target=https%3A%2F%2Fdeveloper.android.com%2Freference%2Fandroid%2Farch%2Flifecycle%2FLiveData.html) 并在它们上面定义转换规则。

#### 1、区别

- `map`变换可以直接修改返回的值和类型。
- `、switchMap`变换需要返回一个`LiveData`对象，这就是跟`map`变换的区别。

数据变化发送Observer之前，需要对LiveData数据进行转换。
转换使用Transformations类，提供两个方法map()和switchMap()。

- map接收两个参数，一个livedata数据源和一个转换函数Function。在Function中将livedata转换为另一个数据类型livedata。
- switchMap 与map类似，当数据源livedata数据发生变化时会，会生成新的livadata实例。

#### 2、liveData 以及子类

- liveData 看作不变对象
- MutableLiveData 看作是可变且线程安全。
- MediatorLiveData 是MutableLiveData子类，通过MediatorLiveData可以合并多个LiveData来源的数据，其中一个livedata数据发生变化，MediatorLiveData都会通知它的观察者。



### 五、合并多个LiveData数据源

如果有多个LiveData，可以使用MediatorLiveData来合并这些LiveData，一旦其中一个LiveData发生变化，MediatorLiveData都会通知观察者。比如：一个UI界面，依赖于网络数据和数据库，因此就会存在两个LiveData。使用MediatorLiveData将两个LiveData合并后，UI界面只需要观察一个MediatorLiveData即可。当其中一个LiveData数据发生变化时都会通知UI界面去更新。

#### 1、 MediatorLiveData使用例子

MediatorLiveData的简单使用如下所示：

```java
    LiveData liveData1 = ...;
    LiveData liveData2 = ...;

    MediatorLiveData liveDataMerger = new MediatorLiveData<>();

    liveDataMerger.addSource(liveData1, value -> liveDataMerger.setValue(value));
    liveDataMerger.addSource(liveData2, value -> liveDataMerger.setValue(value));
然后在UI界面直接观察这个liveDataMerger即可。
```
#### 2、MediatorLiveData的方法

MediatorLiveData相比于LiveData，主要是多了以下两个方法：

addSource()：添加源LiveData，并且开始监听给定的源LiveData，当源LiveData的数据发生变化时，观察者的onChanged()方法将会被调用，前提是MediatorLiveData处于活跃状态。
removeSource()：移除LiveData。
再来看个例子:

liveDataMerger.addSource(liveData1, new Observer() {
      private int count = 1;

```java
  @Override public void onChanged(@Nullable Integer s) {
      count++;
      liveDataMerger.setValue(s);
      if (count > 10) {
          liveDataMerger.removeSource(liveData1);
      }
  }
 });

```
监听liveData1的数据，当监听了10次之后，移除对liveData1监听。