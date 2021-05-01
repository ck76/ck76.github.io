[TOC]

### 前言

最近两年，MVVM的呼声越来越高，说实话，在经历了MVP的臃肿，MVP的繁琐，我有点怕了。但是这次Google官方带来的一系列为MVVM架构设计的武器—`Jetpack`，真的让我惊喜到了。

也许你还没有使用这个新的武器，那么我真的建议你去使用一下，感受下这个新武器的快准狠，感受下这个新架构的精妙解耦。

### 介绍

2018年谷歌I/O，`Jetpack`横空出世，官方介绍如下：

> Jetpack 是一套库、工具和指南，可帮助开发者更轻松地编写优质应用。这些组件可帮助您遵循最佳做法、让您摆脱编写样板代码的工作并简化复杂任务，以便您将精力集中放在所需的代码上。

一直以来，`Android开发`都充斥了大量的不规范的操作和重复代码，比如生命周期的管理，开发过程的重复，项目架构的选择等等。所以`Google`为了规范开发行为，就推出这套指南，旨在让开发者们能够`更好，更快，更规范`地开发出优质应用。

当然，这两年的实践也确实证明了`Jetpack`做到了它介绍的那样，便捷，快速，优质。所以我们作为开发者还是应该早点应用到这些工具，提高自己的`开发效率`，也规范我们自己的开发行为。

今天给大家带来的是Jetpack中的架构组件，这个模块的组件可以说就是为MVVM框架服务的，每个库也都是可以单独使用的。

### Jetpack-架构组件

先简单说下`MVVM`，Model—View—ViewModel。

- `Model层`主要指数据，比如服务器数据，本地数据库数据，所以网络操作和数据库读取就是这一层，只保存数据。
- `View层`主要指UI相关，比如xml布局文件，Activity界面显示
- `ViewModel层`是MVVM的核心，连接view和model，需要将model的数据展示到view上，以及view上的操作数据反映转化到model层，所以就相当于一个双向绑定。

所以就需要，**databinding**进行数据的绑定，单向或者双向。**viewmodel**进行数据管理，绑定view和数据。**lifecycle**进行生命周期管理。**LiveData**进行数据的及时反馈。 迫不及待了吧，跟随我一起看看每个库的神奇之处。

#### 数据绑定

> 数据绑定库是一种支持库，借助该库，您可以使用声明性格式（而非程序化地）将布局中的界面组件绑定到应用中的数据源。

主要指的就是数据绑定库`DataBinding`，下面从六个方面具体介绍下

配置应用使用数据绑定：



```bash
   android {
        ...
        dataBinding {
            enabled = true
        }
    }

复制代码
```

1）**布局和绑定表达式**
通过数据绑定，我们可以让xml布局文件中的view与数据对象进行绑定和赋值，并且可以借助表达式语言编写表达式来处理视图分派的事件。举个🌰：



```kotlin
    //布局 activity_main.xml
    <?xml version="1.0" encoding="utf-8"?>
    <layout xmlns:android="http://schemas.android.com/apk/res/android">
       <data>
           <variable name="user" type="com.example.User"/>
       </data>
       <TextView android:layout_width="wrap_content"
           android:layout_height="wrap_content"
           android:text="@{user.name}"/>
    </layout>

    //实体类User
    data class User(val name: String)

    //Activity赋值
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        val binding: ActivityMainBinding = DataBindingUtil.setContentView(
                this, R.layout.activity_main)
        binding.user = User("Bob")
    }

复制代码
```

通过`@{}`符号，可以在布局中使用数据对象，并且可以通过DataBindingUtil获取赋值对象。并且`@{}`里面的表达式语言支持多种运算符，包括算术运算符，逻辑运算符等等。

2）**可观察的数据对象**
可观察性是指一个对象将其数据变化告知其他对象的能力。通过数据绑定库，您可以让对象、字段或集合变为可观察。

比如上文刚说到的User类，我们将name属性改成可观察对象，



```kotlin
   data class User(val name: ObservableField<String>)

   val userName = ObservableField<String>()
   userName.set("Bob")

   val binding: ActivityMainBinding = DataBindingUtil.setContentView(
                this, R.layout.activity_main)
   binding.user = User(userName)   
复制代码
```

然后绑定到布局中，这时候这个User的`name`属性就是被观察对象了，如果`userName`改变，布局里面的`TextView`显示数据也会跟着改变，这就是可观察数据对象。

3）**生成的绑定类**

刚才我们获取绑定布局是通过`DataBindingUtil.setContentView`方法生成ActivityMainBinding对象并绑定布局。那么ActivityMainBinding类是怎么生成的呢？只要你的布局用`layout`属性包围，编译后就会自动生成绑定类，类名称基于布局文件的名称，它会转换为 `Pascal` 大小写形式并在末尾添加 Binding 后缀。

正常创建绑定对象是通过如下写法：



```kotlin
    //Activity
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val binding: MyLayoutBinding = MyLayoutBinding.inflate(layoutInflater)
        setContentView(binding.root)
    }

    //Fragment
    @Nullable
    fun onCreateView( inflater: LayoutInflater?, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        mDataBinding = DataBindingUtil.inflate(inflater, R.layout.fragment_layout, container, false)
        return mDataBinding.getRoot()
    }

复制代码
```

4）**绑定适配器**

适配器这里指的是布局中的属性设置，`android:text="@{user.name}"`表达式为例，库会查找接受`user.getName()`所返回类型的`setText(arg)` 方法。 重要的是，我们可以自定义这个适配器了，也就是布局里面的属性我们可以随便定义它的名字和作用。来个🌰



```kotlin
    @BindingAdapter("imageUrl")
    fun loadImage(view: ImageView, url: String) {
        Picasso.get().load(url).into(view)
    }

    <ImageView app:imageUrl="@{venue.imageUrl}" />

复制代码
```

在类中定义一个外部可以访问的方法`loadImage`，注释`@BindingAdapter`里面的属性为你需要定义的属性名称，这里设置的是imageUrl。所以在布局中就可以使用`app:imageUrl`，并传值为String类型，系统就会找到这个适配器方法并执行。

5）**将布局视图绑定到架构组件**
这一块就是实际应用了，和jetpack其他组件相结合使用，形成完整的`MVVM`分层架构。



```kotlin
        // Obtain the ViewModel component.
        val userModel: UserViewModel by viewModels()

        // Inflate view and obtain an instance of the binding class.
        val binding: ActivityDatabindingMvvmBinding =
            DataBindingUtil.setContentView(this, R.layout.activity_databinding_mvvm)

        // Assign the component to a property in the binding class.
        binding.viewmodel = userModel

    <data>
        <variable
            name="viewmodel"
            type="com.panda.jetpackdemo.dataBinding.UserViewModel" />
    </data>

    class UserViewModel : ViewModel() {
    val currentName: MutableLiveData<String> by lazy {
        MutableLiveData<String>()
    }

    init {
        currentName.value="zzz"
    }
}
复制代码
```

6）**双向数据绑定**

刚才我们介绍的都是单向绑定，也就是布局中view绑定了数据对象，那么如何让数据对象也对view产生绑定呢？也就是`view改变`的时候数据对象也能接收到讯息，形成`双向绑定`。

很简单，比如一个EditText，需求是EditText改变的时候，user对象name数据也会跟着改变，只需要把之前的"@{}"改成"@={}"



```xml
    //布局 activity_main.xml
    <?xml version="1.0" encoding="utf-8"?>
    <layout xmlns:android="http://schemas.android.com/apk/res/android">
       <data>
           <variable name="user" type="com.example.User"/>
       </data>
       <EditText android:layout_width="wrap_content"
           android:layout_height="wrap_content"
           android:text="@={user.name}"/>
    </layout>

复制代码
```

很简单吧，同样，这个双向绑定功能也是支持自定义的。来个🌰



```kotlin
object SwipeRefreshLayoutBinding {

    //方法1，数据绑定到view
    @JvmStatic
    @BindingAdapter("app:bind_refreshing")
    fun setSwipeRefreshLayoutRefreshing(swipeRefreshLayout: SwipeRefreshLayout,newValue: Boolean) {
        if (swipeRefreshLayout.isRefreshing != newValue)
            swipeRefreshLayout.isRefreshing = newValue
    }

    //方法1，view改变会通知bind_refreshingChanged，并且从该方法获取view的数据
    @JvmStatic
    @InverseBindingAdapter(attribute = "app:bind_refreshing",event = "app:bind_refreshingChanged")
    fun isSwipeRefreshLayoutRefreshing(swipeRefreshLayout: SwipeRefreshLayout): Boolean =swipeRefreshLayout.isRefreshing

    //方法3，view如何改变来影响数据内容  
    @JvmStatic
    @BindingAdapter("app:bind_refreshingChanged",requireAll = false)
    fun setOnRefreshListener(swipeRefreshLayout: SwipeRefreshLayout,bindingListener: InverseBindingListener?) {
        if (bindingListener != null)
            swipeRefreshLayout.setOnRefreshListener {
                bindingListener.onChange()
            }
    }
}

<androidx.swiperefreshlayout.widget.SwipeRefreshLayout
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        app:bind_refreshing="@={viewModel.refreshing }">
</androidx.swiperefreshlayout.widget.SwipeRefreshLayout>

复制代码
```

简单说明下，首先通过`bind_refreshing`属性，将数据`viewModel.refreshing`绑定到view上，这样数据变化，view也会跟着变化。然后view变化的时候，通过`InverseBindingAdapter`注释，会调用`bind_refreshingChanged`事件，而bind_refreshingChanged事件告诉了我们view什么时候会进行数据的修改，在这个案例中也就是swipeRefreshLayout下滑的时候会导致数据进行改变，于是数据对象会从`isSwipeRefreshLayoutRefreshing`方法获取到最新的数值，也就是从view更新过来的数据。

这里要注意的一个点是，双向绑定要考虑到死循环问题，当View被改变，数据对象对应发生更新，同时，这个更新又回通知View层去刷新UI，然后view被改变又会导致数据对象更新，无限循环下去了。所以防止死循环的做法就是判断view的数据状态，当发生改变的时候才去更新view。

[官方文档](https://links.jianshu.com/go?to=https%3A%2F%2Fdeveloper.android.google.cn%2Ftopic%2Flibraries%2Fdata-binding)
[Demo代码地址](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FJiMuzz%2Fjimu-Jetpack-Demo%2Ftree%2Fmaster%2Fapp%2Fsrc%2Fmain%2Fjava%2Fcom%2Fpanda%2Fjetpackdemo%2FdataBinding)

#### Lifecycles

> 生命周期感知型组件可执行操作来响应另一个组件（如 Activity 和 Fragment）的生命周期状态的变化。这些组件有助于您写出更有条理且往往更精简的代码，这样的代码更易于维护。

`Lifecycles`，称为生命周期感知型组件，可以感知和响应另一个组件（如 Activity 和 Fragment）的生命周期状态的变化。

可能有人会疑惑了，生命周期就那几个，我为啥还要导入一个库呢？有了库难道就不用写生命周期了吗，有什么好处呢？ 举个🌰，让你感受下。

首先导入库，可以根据实际项目情况导入



```dart
        // ViewModel
        implementation "androidx.lifecycle:lifecycle-viewmodel-ktx:$lifecycle_version"
        // LiveData
        implementation "androidx.lifecycle:lifecycle-livedata-ktx:$lifecycle_version"
        // Lifecycles only (without ViewModel or LiveData)
        implementation "androidx.lifecycle:lifecycle-runtime-ktx:$lifecycle_version"
        //.......
复制代码
```

现在有一个定位监听器，需要在`Activity`启动的时候开启，销毁的时候关闭。正常代码如下：



```kotlin
class BindingActivity : AppCompatActivity() {

    private lateinit var myLocationListener: MyLocationListener

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        myLocationListener = MyLocationListener(this) { location ->
            // update UI
        }
    }
    public override fun onStart() {
        super.onStart()
        myLocationListener.start()       
    }
    public override fun onStop() {
        super.onStop()
        myLocationListener.stop()
    }

    internal class MyLocationListener(
            private val context: Context,
            private val callback: (Location) -> Unit
    ) {
        fun start() {
            // connect to system location service
        }
        fun stop() {
            // disconnect from system location service
        }
    }

}
复制代码
```

乍一看也没什么问题是吧，但是如果需要管理生命周期的类一多，是不是就不好管理了。所有的类都要在Activity里面管理，还容易漏掉。 所以解决办法就是实现`解耦`，让需要管理生命周期的类`自己管理`，这样Activity也不会遗漏和臃肿了。上代码：



```kotlin
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        myLocationListener = MyLocationListener(this) { location ->
            // update UI
        }
       lifecycle.addObserver(myLocationListener)
    }

    internal class MyLocationListener (
            private val context: Context,
            private val callback: (Location) -> Unit
    ): LifecycleObserver {

        @OnLifecycleEvent(Lifecycle.Event.ON_START)
        fun start() {

        }

        @OnLifecycleEvent(Lifecycle.Event.ON_STOP)
        fun stop() {
            // disconnect if connected
        }
    }
复制代码
```

很简单吧，只要实现`LifecycleObserver`接口，就可以用注释的方式执行每个生命周期要执行的方法。然后在Activity里面`addObserver`绑定即可。

同样的，`Lifecycle`也支持自定义生命周期，只要继承LifecycleOwner即可，然后通过`markState`方法设定自己类的生命周期，举个🌰



```kotlin
class BindingActivity : AppCompatActivity(), LifecycleOwner {

    private lateinit var lifecycleRegistry: LifecycleRegistry

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        lifecycleRegistry = LifecycleRegistry(this)
        lifecycleRegistry.markState(Lifecycle.State.CREATED)
    }

    public override fun onStart() {
        super.onStart()
        lifecycleRegistry.markState(Lifecycle.State.STARTED)
    }
}    
复制代码
```

[官方文档](https://links.jianshu.com/go?to=https%3A%2F%2Fdeveloper.android.google.cn%2Ftopic%2Flibraries%2Farchitecture%2Flifecycle)
[Demo代码地址](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FJiMuzz%2Fjimu-Jetpack-Demo%2Ftree%2Fmaster%2Fapp%2Fsrc%2Fmain%2Fjava%2Fcom%2Fpanda%2Fjetpackdemo%2Flifecycle)

#### LiveData

> LiveData 是一种可观察的数据存储器类。与常规的可观察类不同，LiveData 具有生命周期感知能力，意指它遵循其他应用组件（如 Activity、Fragment 或 Service）的生命周期。这种感知能力可确保 LiveData 仅更新处于活跃生命周期状态的应用组件观察者。

`LiveData` 是一种可观察的数据存储器类。 等等，这个介绍好像似曾相识？对，前面说数据绑定的时候就有一个可观察的数据对象`ObservableField`。那两者有什么区别呢？

1）`LiveData` 具有生命周期感知能力，可以感知到Activity等的生命周期。这样有什么好处呢？很常见的一点就是可以减少内存泄漏和崩溃情况了呀，想想以前你的项目中针对网络接口返回数据的时候都要判断当前界面是否销毁，现在LiveData就帮你解决了这个问题。

具体为什么能解决崩溃和泄漏问题呢？

- `不会发生内存泄漏` 观察者会绑定到 Lifecycle 对象，并在其关联的生命周期遭到销毁后进行自我清理。
- `不会因 Activity 停止而导致崩溃` 如果观察者的生命周期处于非活跃状态（如返回栈中的 Activity），则它不会接收任何 LiveData 事件。
- `自动判断生命周期并回调方法` 如果观察者的生命周期处于 STARTED 或 RESUMED状态，则 LiveData 会认为该观察者处于活跃状态，就会调用onActive方法，否则，如果 LiveData 对象没有任何活跃观察者时，会调用 onInactive()方法。

2） LiveData更新数据更灵活，不一定是改变数据，而是调用方法`（postValue或者setValue）`的方式进行UI更新或者其他操作。

好了。还是举个🌰更直观的看看吧：



```kotlin
    //导入库：
    implementation "androidx.lifecycle:lifecycle-livedata-ktx:2.2.0"

    class StockLiveData(symbol: String) : LiveData<BigDecimal>() {
        private val stockManager = StockManager(symbol)

        private val listener = { price: BigDecimal ->
            value = price
        }

        override fun onActive() {
            stockManager.requestPriceUpdates(listener)
        }

        override fun onInactive() {
            stockManager.removeUpdates(listener)
        }
    }

    public class MyFragment : Fragment() {
        override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
            super.onViewCreated(view, savedInstanceState)
            val myPriceListener: LiveData<BigDecimal> = StockLiveData("")
            myPriceListener.observe(this, Observer<BigDecimal> { price: BigDecimal? ->
                // 监听livedata的数据变化，如果调用了setValue或者postValue会调用该onChanged方法
                //更新UI数据或者其他处理
            })
        }
    }

复制代码
```

这是一个股票数据对象，`StockManager`为股票管理器，如果该对象有活跃观察者时，就去监听股票市场的情况，如果没有活跃观察者时，就可以断开监听。 当监听到股票信息变化，该股票数据对象就会通过`setValue`方法进行数据更新，反应到观察者的onChanged方法。这里要注意的是`setValue`方法只能在主线程调用，而`postValue`则是在其他线程调用。 当`Fragment`这个观察者生命周期发生变化时，`LiveData`就会移除这个观察者，不再发送消息，所以也就避免崩溃问题。

[官方文档](https://links.jianshu.com/go?to=https%3A%2F%2Fdeveloper.android.google.cn%2Ftopic%2Flibraries%2Farchitecture%2Flivedata)
[Demo代码地址](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FJiMuzz%2Fjimu-Jetpack-Demo%2Ftree%2Fmaster%2Fapp%2Fsrc%2Fmain%2Fjava%2Fcom%2Fpanda%2Fjetpackdemo%2Flivedata)

#### Navigation

> 导航 Navigation 组件旨在用于具有一个主 Activity 和多个 Fragment 目的地的应用。主 Activity 与导航图相关联，且包含一个负责根据需要交换目的地的 NavHostFragment。在具有多个 Activity 目的地的应用中，每个 Activity 均拥有其自己的导航图。

所以说白了，`Navigation`就是一个`Fragment`的管理框架。 怎么实现？创建Activity，Fragment，进行连接。

1）**导入库**



```bash
  def nav_version = "2.3.0"
  implementation "androidx.navigation:navigation-fragment-ktx:$nav_version"
  implementation "androidx.navigation:navigation-ui-ktx:$nav_version"
复制代码
```

2）**创建3个Fragment和一个Activity**

3）**创建res/navigation/my_nav.xml 文件**



```xml
<?xml version="1.0" encoding="utf-8"?>
<navigation xmlns:app="http://schemas.android.com/apk/res-auto"
    xmlns:tools="http://schemas.android.com/tools"
    xmlns:android="http://schemas.android.com/apk/res/android"
    app:startDestination="@id/myFragment1"
    tools:ignore="UnusedNavigation">

    <fragment
        android:id="@+id/myFragment1"
        android:name="com.example.studynote.blog.jetpack.navigation.MyFragment1"
        android:label="fragment_blank"
        tools:layout="@layout/fragmetn_my_1" >
        <action
            android:id="@+id/action_blankFragment_to_blankFragment2"
            app:destination="@id/myFragment2" />
    </fragment>

    <fragment
        android:id="@+id/myFragment2"
        android:name="com.example.studynote.blog.jetpack.navigation.MyFragment1"
        android:label="fragment_blank"
        tools:layout="@layout/fragmetn_my_1" >
        <action
            android:id="@+id/action_blankFragment_to_blankFragment2"
            app:destination="@id/myFragment3" />
    </fragment>

    <fragment
        android:id="@+id/myFragment3"
        android:name="com.example.studynote.blog.jetpack.navigation.MyFragment1"
        android:label="fragment_blank"
        tools:layout="@layout/fragmetn_my_1" >
    </fragment>
</navigation>

复制代码
```

在res文件夹下新建`navigation`目录，并新建`my_nav.xml` 文件。配置好每个Fragment，其中：

- `app:startDestination` 属性代表一开始显示的fragment
- `android:name` 属性代表对应的Fragment路径
- `action` 代表该Fragment存在的跳转事件，比如myFragment1可以跳转myFragment2。

1. **修改Activity的布局文件：**



```xml
<?xml version="1.0" encoding="utf-8"?>
<androidx.constraintlayout.widget.ConstraintLayout
xmlns:android="http://schemas.android.com/apk/res/android"
xmlns:app="http://schemas.android.com/apk/res-auto"
android:id="@+id/container"
android:layout_width="match_parent"
android:layout_height="match_parent">

<fragment
    android:id="@+id/nav_host_fragment"
    android:name="androidx.navigation.fragment.NavHostFragment"
    android:layout_width="0dp"
    android:layout_height="0dp"
    app:layout_constraintBottom_toBottomOf="parent"
    app:layout_constraintLeft_toLeftOf="parent"
    app:layout_constraintRight_toRightOf="parent"
    app:layout_constraintTop_toTopOf="parent"
    app:defaultNavHost="true"
    app:navGraph="@navigation/my_nav" />

</androidx.constraintlayout.widget.ConstraintLayout>

复制代码
```

可以看到，Activity的布局文件就是一个fragment控件，name为NavHostFragment，`navGraph`为刚才新建的mynavigation文件。

5）**配置完了之后，就可以设置具体的跳转逻辑了。**



```kotlin
    override fun onClick(v: View) {
    //不带参数
 v.findNavController().navigate(R.id.action_blankFragment_to_blankFragment2)
   //带参数
    var bundle = bundleOf("amount" to amount)
    v.findNavController().navigate(R.id.confirmationAction, bundle)

    }

    //接收数据
    tv.text = arguments?.getString("amount")

复制代码
```

需要注意的是，跳转这块官方建议用`Safe Args` 的Gradle 插件，该插件可以生成简单的 `object 和 builder`类，以便以类型安全的方式浏览和访问任何关联的参数。这里就不细说了，感兴趣的可以去[官网看看](https://links.jianshu.com/go?to=https%3A%2F%2Fdeveloper.android.google.cn%2Fguide%2Fnavigation%2Fnavigation-getting-started%23ensure_type-safety_by_using_safe_args)

[官方文档](https://links.jianshu.com/go?to=https%3A%2F%2Fdeveloper.android.google.cn%2Fguide%2Fnavigation%2F)
[Demo代码地址](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FJiMuzz%2Fjimu-Jetpack-Demo%2Ftree%2Fmaster%2Fapp%2Fsrc%2Fmain%2Fjava%2Fcom%2Fpanda%2Fjetpackdemo%2Fnavigation)

#### Room

> Room 持久性库在 SQLite 的基础上提供了一个抽象层，让用户能够在充分利用 SQLite 的强大功能的同时，获享更强健的数据库访问机制。

所以`Room`就是一个数据库框架。问题来了，市面上那么多数据库组件，比如`ormLite，greendao`等等，为什么google还要出一个room，有什么优势呢？

- **性能优势**，一次数据库操作主要包括：构造sql语句—编译语句—传入参数—执行操作。`ORMLite`主要在获取参数属性值的时候，是通过反射获取的，所以速度较慢。`GreenDao`在构造sql语句的时候是通过代码拼接，所以较慢。`Room`是通过接口方法的注解生成sql语句，也就是编译成字节码的时候就生成了sql语句，所以运行起来较快。
- **支持jetpack其他组件（比如LiveData，Paging）以及RxJava**，这就好比借助了当前所在的优势环境，就能给你带来一些得天独厚的优势。当然实际使用起来也确实要方便很多，比如`liveData`结合，就能在数据查询后进行自动UI更新。

既然Room这么优秀，那就用起来吧。 Room的接入主要有三大点：`DataBase、Entity、Dao`。分别对应数据库，表和数据访问。

1）**首先导入库：**



```dart
    apply plugin: 'kotlin-kapt'

    dependencies {
      def room_version = "2.2.5"

      implementation "androidx.room:room-runtime:$room_version"
      kapt "androidx.room:room-compiler:$room_version" // For Kotlin use kapt instead of annotationProcessor

      // optional - Kotlin Extensions and Coroutines support for Room
      implementation "androidx.room:room-ktx:$room_version"

      // optional - RxJava support for Room
      implementation "androidx.room:room-rxjava2:$room_version"
    }

复制代码
```

2）**建立数据库类，声明数据库表成员，数据库名称，数据库版本，单例等等**



```kotlin
@Database(entities = arrayOf(User::class), version = 1)
abstract class UserDb : RoomDatabase() {

    abstract fun userDao(): UserDao

    companion object {
        private var instance: UserDb? = null

        @Synchronized
        fun get(context: Context): UserDb {
            if (instance == null) {
                instance = Room.databaseBuilder(context.applicationContext,
                    UserDb::class.java, "StudentDatabase").build()
            }
            return instance!!
        }
    }
}
复制代码
```

3）**建表，可以设置主键，外键，索引，自增等等**



```kotlin
@Entity
data class User(@PrimaryKey(autoGenerate = true) val id: Int,
                val name: String)
复制代码
```

4）**Dao，数据操作**



```kotlin
@Dao
interface UserDao {

    @Query("SELECT * FROM User")
    fun getAllUser(): DataSource.Factory<Int, User>

    @Query("SELECT * FROM User")
    fun getAllUser2(): LiveData<List<User>>

    @Query("SELECT * from user")
    fun getAllUser3(): Flowable<List<User>>

    @Insert
    fun insert(users: List<User>)
}
复制代码
```

然后就可以进行数据库操作了，很简单吧。
[官方文档](https://links.jianshu.com/go?to=https%3A%2F%2Fdeveloper.android.google.cn%2Ftopic%2Flibraries%2Farchitecture%2Froom)
[Demo代码地址](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FJiMuzz%2Fjimu-Jetpack-Demo%2Ftree%2Fmaster%2Fapp%2Fsrc%2Fmain%2Fjava%2Fcom%2Fpanda%2Fjetpackdemo%2Fpaging_room)

#### Paging

> 分页库可帮助您一次加载和显示一小块数据。按需载入部分数据会减少网络带宽和系统资源的使用量。

所以`Paging`就是一个分页库，主要用于Recycleview列表展示。下面我就结合Room说说Paging的用法。 使用Paging主要注意两个类：`PagedList和PagedListAdapter`。
1）**PagedList**
用于加载应用数据块，绑定数据列表，设置数据页等。结合上述`Room`的Demo我继续写了一个`UserModel`进行数据管理：



```kotlin
class UserModel(app: Application) : AndroidViewModel(app) {
    val dao = UserDb.get(app).userDao()
    var idNum = 1

    companion object {
        private const val PAGE_SIZE = 10
    }

    //初始化PagedList
    val users = LivePagedListBuilder(
        dao.getAllUser(), PagedList.Config.Builder()
            .setPageSize(PAGE_SIZE)
            .setEnablePlaceholders(true)
            .build()
    ).build()

    //插入用户
    fun insert() = ioThread {
        dao.insert(newTenUser())
    }

    //获取新的10个用户
    fun newTenUser(): ArrayList<User> {
        var newUsers = ArrayList<User>()
        for (index in 1..10) {
            newUsers.add(User(0, "bob${++idNum}"))
        }
        return newUsers
    }

}
复制代码
```

2）**PagedListAdapter**
使用Recycleview必要要用到adatper，所以这里需要绑定一个继承自`PagedListAdapter`的adapter：



```kotlin
class UserAdapter : PagedListAdapter<User, UserAdapter.UserViewHolder>(diffCallback) {
    override fun onBindViewHolder(holder: UserViewHolder, position: Int) {
        holder.bindTo(getItem(position))
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): UserViewHolder =
        UserViewHolder(parent)

    companion object {

        private val diffCallback = object : DiffUtil.ItemCallback<User>() {
            override fun areItemsTheSame(oldItem: User, newItem: User): Boolean =
                oldItem.id == newItem.id

            override fun areContentsTheSame(oldItem: User, newItem: User): Boolean =
                oldItem == newItem
        }
    }

    class UserViewHolder(parent: ViewGroup) : RecyclerView.ViewHolder(
        LayoutInflater.from(parent.context).inflate(R.layout.list_item, parent, false)) {

        private val tv1 = itemView.findViewById<TextView>(R.id.name)
        var user: User? = null

        fun bindTo(user: User?) {
            this.user = user
            tv1.text = user?.name
        }
    }
}
复制代码
```

这里还用到了`DiffUtil.ItemCallback` 类，用于比较数据，进行数据更新用。

ok，数据源，adapter都设置好了，接下来就是监听数据，刷新数据就可以了



```kotlin
        // 监听users数据，数据改变调用submitList方法
        viewModel.users.observe(this, Observer(adapter::submitList))
复制代码
```

对，就是这么一句，监听`PagedList`，并且在它改变的时候调用PagedListAdapter的`submitList`方法。 这分层够爽吧，其实这也就是paging或者说jetpack给我们项目带来的优势，层层解耦，adapter都不用维护list数据源了。

[官方文档](https://links.jianshu.com/go?to=https%3A%2F%2Fdeveloper.android.google.cn%2Ftopic%2Flibraries%2Farchitecture%2Fpaging)
[Demo代码地址](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FJiMuzz%2Fjimu-Jetpack-Demo%2Ftree%2Fmaster%2Fapp%2Fsrc%2Fmain%2Fjava%2Fcom%2Fpanda%2Fjetpackdemo%2Fpaging_room)

#### ViewModel

> ViewModel 类旨在以注重生命周期的方式存储和管理界面相关的数据。ViewModel 类让数据可在发生屏幕旋转等配置更改后继续留存。

终于说到`ViewModel`了，其实之前的demo都用了好多遍了，`ViewModel`主要是从界面控制器逻辑中分离出视图数据，为什么要这么做呢？主要为了解决两大问题：

- 以前Activity中如果被系统销毁或者需要重新创建的时候，页面临时性数据都会丢失，需要通过`onSaveInstanceState()` 方法保存，onCreate方法中读取。而且数据量一大就更加不方便了。
- 在Activity中，难免有些异步调用，所以就会容易导致界面销毁时候，这些调用还存在。那就会发生内存泄漏或者直接崩溃。

所以`ViewModel`诞生了，还是解耦，我把数据单独拿出来管理，还加上生命周期，那不就可以解决这些问题了吗。而且当所有者 Activity 完全销毁之后，`ViewModel`会调用其`onCleared()`方法，以便清理资源。

接下来举个🌰，看看ViewModel具体是怎么使用的：



```kotlin
def lifecycle_version = "2.2.0"
// ViewModel
implementation "androidx.lifecycle:lifecycle-viewmodel-ktx:$lifecycle_version"

class SharedViewModel : ViewModel() {
    var userData = MutableLiveData<User>()

    fun select(item: User) {
        userData.value = item
    }

    override fun onCleared() {
        super.onCleared()
    }
}

class MyFragment1 : Fragment() {
    private lateinit var btn: Button

    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)

        val model=activity?.let { ViewModelProvider(it).get(SharedViewModel::class.java) }
        btn.setOnClickListener{
            model?.select(User(0,"bob"))
        }
    }
}

class MyFragment2 : Fragment() {
    private lateinit var btn: Button

    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)

        val model=activity?.let { ViewModelProvider(it).get(SharedViewModel::class.java) }
        model?.userData?.observe(viewLifecycleOwner, Observer<User> { item ->
            // Update the UI
        })
    }
}

复制代码
```

Fragment中，获取到`viewmodel`的实例，然后进行数据监听等操作。等等，你能发现什么不？ 对了，数据通信。不同的 Fragment 可以使用其父Activity共享`ViewModel` 来进行数据的通信，厉害吧。还有很多其他的用法，去项目中慢慢发现吧！

[官方文档](https://links.jianshu.com/go?to=https%3A%2F%2Fdeveloper.android.google.cn%2Ftopic%2Flibraries%2Farchitecture%2Fviewmodel)
[Demo代码地址](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FJiMuzz%2Fjimu-Jetpack-Demo%2Ftree%2Fmaster%2Fapp%2Fsrc%2Fmain%2Fjava%2Fcom%2Fpanda%2Fjetpackdemo%2Fviewmodel)

#### WorkManager

> 使用 WorkManager API 可以轻松地调度即使在应用退出或设备重启时仍应运行的可延迟异步任务。

听听这个介绍就很神奇了，应用退出和设备重启都能自动运行？通过广播？那数据又是怎么保存的呢？听说还可以执行周期性异步任务，顺序链式调用哦！接下来一一解密

一般这个API应用到什么场景呢？想想，可靠运行，还可以周期异步。 对了，发送日志。可以通过`WorkManager`设定周期任务，每天执行一次发送日志的任务。而且能够保证你的任务可靠运行，一定可以上传到，当然也是支持监听任务结果等。🌰：

1）**导入库**



```dart
    dependencies {
      def work_version = "2.3.4"
        // Kotlin + coroutines
        implementation "androidx.work:work-runtime-ktx:$work_version"

        // optional - RxJava2 support
        implementation "androidx.work:work-rxjava2:$work_version"

        // optional - GCMNetworkManager support
        implementation "androidx.work:work-gcm:$work_version"
      }

复制代码
```

2） **新建任务类**，继承`Worker`，重写`doWork`方法，返回任务结果。



```kotlin
class UploadLogcatWork(appContext: Context, workerParams: WorkerParameters) :
    Worker(appContext, workerParams) {

    override fun doWork(): Result {

        if (isUploadLogcatSuc()) {
            return Result.success()
        } else if (isNeedRetry()){
            return Result.retry()
        }

        return Result.failure()
    }

    fun isUploadLogcatSuc(): Boolean {
        var isSuc: Boolean = false
        return isSuc
    }

    fun isNeedRetry(): Boolean {
        var isSuc: Boolean = false
        return isSuc
    }
}
复制代码
```

3）**最后就是设定约束**（是否需要网络，是否支持低电量，是否支持充电执行，延迟等等），执行任务（单次任务或者循环周期任务）



```kotlin
        //设定约束
        val constraints =
            Constraints.Builder()
                //网络链接的时候使用
                .setRequiredNetworkType(NetworkType.CONNECTED)
                //是否在设备空闲的时候执行
                .setRequiresDeviceIdle(false)
                //是否在低电量的时候执行
                .setRequiresBatteryNotLow(true)
                //是否在内存不足的时候执行
                .setRequiresStorageNotLow(true)
                //是否时充电的时候执行
                .setRequiresCharging(true)
                //延迟执行
                .setTriggerContentMaxDelay(1000 * 1, TimeUnit.MILLISECONDS)
                .build()

        //设定循环任务
        val uploadRequest =
            PeriodicWorkRequestBuilder<UploadLogcatWork>(1, TimeUnit.HOURS)
                .setConstraints(constraints)
                .addTag("uploadTag")
                .build()

        //执行
        WorkManager.getInstance(applicationContext).enqueue(uploadRequest)

        //监听执行结果
        WorkManager.getInstance(this)
//            .getWorkInfosByTagLiveData("uploadTag") //通过tag拿到work
            .getWorkInfoByIdLiveData(uploadRequest.id) //通过id拿到work
            .observe(this, Observer {
                it?.apply {
                    when (this.state) {
                        WorkInfo.State.BLOCKED -> println("BLOCKED")
                        WorkInfo.State.CANCELLED -> println("CANCELLED")
                        WorkInfo.State.RUNNING -> println("RUNNING")
                        WorkInfo.State.ENQUEUED -> println("ENQUEUED")
                        WorkInfo.State.FAILED -> println("FAILED")
                        WorkInfo.State.SUCCEEDED -> println("SUCCEEDED")
                        else -> println("else status ${this.state}")
                    }
                }

            })
复制代码
```

4）**另外还支持任务取消，任务链式顺序调用等**



```kotlin
    //取消
    fun cancelWork(){
  WorkManager.getInstance(applicationContext).cancelAllWorkByTag("uploadTag")
    }

    fun startLineWork(){
        //图片滤镜1
        val filter1 = OneTimeWorkRequestBuilder<UploadLogcatWork>()
            .build()
        //图片滤镜2
        val filter2 = OneTimeWorkRequestBuilder<UploadLogcatWork>()
            .build()
        //图片压缩
        val compress = OneTimeWorkRequestBuilder<UploadLogcatWork>()
            .build()
        //图片上传
        val upload = OneTimeWorkRequestBuilder<UploadLogcatWork>()
            .build()

        WorkManager.getInstance(applicationContext)
            .beginWith(listOf(filter1, filter2))
            .then(compress)
            .then(upload)
            .enqueue()
    }

复制代码
```

[官方文档](https://links.jianshu.com/go?to=https%3A%2F%2Fdeveloper.android.google.cn%2Ftopic%2Flibraries%2Farchitecture%2Fworkmanager)
[Demo代码地址](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FJiMuzz%2Fjimu-Jetpack-Demo%2Ftree%2Fmaster%2Fapp%2Fsrc%2Fmain%2Fjava%2Fcom%2Fpanda%2Fjetpackdemo%2Fworkmanager)

### 总结

我意识到有很多经验和知识值得分享给大家，也可以通过我们的能力和经验解答大家在IT学习中的很多困惑，所以在工作繁忙的情况下还是坚持各种整理和分享。但苦于知识传播途径有限，很多程序员朋友无法获得正确的资料得到学习提升，故此将并将重要的Android进阶资料包括自定义view、性能优化、MVC与MVP与MVVM三大框架的区别、NDK技术、阿里面试题精编汇总、常见源码分析等学习资料免费分享出来。

![img](https:////upload-images.jianshu.io/upload_images/19956127-4d1b5e857c4b3c2c.png?imageMogr2/auto-orient/strip|imageView2/2/w/638)

知识不体系？这里还有整理出来的Android进阶学习的思维脑图，给大家参考一个方向。包含知识脉络 + 诸多细节，由于篇幅有限，下面只是以图片的形式给大家展示一部分。

![img](https:////upload-images.jianshu.io/upload_images/19956127-1b214e26967dacc6.jpg?imageMogr2/auto-orient/strip|imageView2/2/w/1200)

【[Android学习PDF+学习视频+面试文档+知识点笔记](https://links.jianshu.com/go?to=https%3A%2F%2Fshimo.im%2Fdocs%2FjCGWWWkjRwvJWWvg%2F)】

**【Android高级架构视频学习资源】**

**Android部分精讲视频领取学习后更加是如虎添翼！**进军BATJ大厂等（备战）！现在都说互联网寒冬，其实无非就是你上错了车，且穿的少（技能），要是你上对车，自身技术能力够强，公司换掉的代价大，怎么可能会被裁掉，都是淘汰末端的业务Curd而已！现如今市场上初级程序员泛滥，这套教程针对Android开发工程师1-6年的人员、正处于瓶颈期，想要年后突破自己涨薪的，进阶Android中高级、架构师对你更是如鱼得水，赶快领取吧！

**【Android进阶学习视频】、【全套Android面试秘籍】可以简信我【学习】查看免费领取方式！**



作者：木木玩Android
链接：https://www.jianshu.com/p/5fb55b20bcfd
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。