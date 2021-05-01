[TOC]

## 一、概述

### 简介

Jetpack是一个Android软件组件的集合，可以让你更容易地开发优秀的Android应用程序。这些组件帮助您遵循最佳实践，使您不必编写样板代码，并简化复杂的任务，这样您就可以专注于您关心的代码。

Jetpack包括androidx包库，从平台api中分离出来。这意味着它提供了向后兼容性，并且比Android平台更新得更频繁，确保您始终能够访问Jetpack组件的最新和最好版本。

![jg](https://ws1.sinaimg.cn/large/006tNbRwly1fyffc98qipj318g0mgafb.jpg)

### 优点

- 加快发展


组件可单独采用，也可以一起使用，同时利用Kotlin语言功能，提高您的工作效率。

- 消除样板代码

Android Jetpack管理繁琐的活动，如后台任务，导航和生命周期管理，因此您可以专注于使您的应用变得更好的原因。

- 构建高质量，强大的应用程序

Android Jetpack组件以现代设计实践为基础，可以减少崩溃，减少内存泄漏，并提供向后兼容性。

### 组件库

Android Jetpack组件是一组库的集合，这些库可以单独使用，并且构建起来可以协同工作，同时利用Kotlin语言特性，使您的工作效率更高。全部使用或混合搭配。

- Foundation(基础)

基础组件提供核心系统功能、Kotlin扩展和对multidex和自动化测试的支持。

- Architecture(建筑)

体系结构组件具有帮助管理UI组件生命周期、处理数据持久性等的类。

- Behavior(行为)

行为组件帮助您设计健壮、可测试和可维护的应用程序。

- UI(UI)

UI组件使您的应用程序不仅简单，而且易于使用。



## 二、ViewModel

- ViewModel职责是为Activity或Fragment管理、请求数据，具体数据请求逻辑不应该写在ViewModel中，否
- ViewModel的职责会变得太重，此处需要一个引入一个Repository，负责数据请求相关工作。具体请参考 Android架构组件。
- ViewModel可以用于Activity内不同Fragment的交互，也可以用作Fragment之间一种解耦方式。
- ViewModel也可以负责处理部分Activity/Fragment与应用其他模块的交互。
- ViewModel生命周期（以Activity为例）起始于Activity第一次onCreate()，结束于Activity最终finish时。



**`ViewModel`被设计用于存储并管理UI相关的数据，因此可以在配置变化时存活下来**，例如当屏幕旋转的时候。

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



## 三、LiveData

> 简单地说，LiveData是一个数据**持有**类。它具有以下特点：

- 数据可以被观察者订阅；
- 能够感知组件（Fragment、Activity、Service）的生命周期；**生命周期感知性**
- 只有在组件出于激活状态（STARTED、RESUMED）才会通知观察者有数据更新；
- LiveData 是一个可感知生命周期的可观察类 (Observable)。它能帮您容易地确保被屏幕显示的资讯与数据的同步。其优点包括：

  - **生命周期感知性：**LiveData 与 Android 生命周期结合运行的效果良好。它仅会当 UI 被显示时才把数据往前端传递。

  - 与 Room 无缝整合：LiveData 可被设为 Room 的回调类。观看有关 Room 的视频了解详情。

  - 可与 ViewModel 和 Data Binding 混合使用，建立反应式 UI 。

  - 提供基本数据转换方法，例如 switchMap 和 MediatorLiveData。

### LiveData的优点

- 没有内存泄漏：因为 Observer 被绑定到它们自己的 Lifecycle 对象上，所以，当它们的 Lifecycle 被销毁时，它们能自动的被清理。
- 不会因为 activity 停止而崩溃：如果 Observer 的 Lifecycle 处于闲置状态（例如：activity 在后台时），它们不会收到变更事件。
- 始终保持数据最新：如果 **Lifecycle** 重新启动（例如：activity 从后台返回到启动状态）将会收到最新的位置数据（除非还没有）。
- 正确处理配置更改：如果 activity 或 fragment 由于配置更改（如：设备旋转）重新创建，将会立即收到最新的有效位置数据。
- 资源共享：可以只保留一个 MyLocationListener 实例，只连接系统服务一次，并且能够正确的支持应用程序中的所有观察者。
- 不再手动管理生命周期：**fragment 只是在需要的时候观察数据，不用担心被停止或者在停止之后启动观察。由于 fragment 在观察数据时提供了其 Lifecycle，所以 LiveData 会自动管理这一切。



## 四、Paging

### 1、简介

> 一句话概述： **Paging** 可以使开发者更轻松在 **RecyclerView** 中 **分页加载数据**。

在很久很久以前，加载并展示大量数据就已成为各家应用中必不可少的业务场景，分页加载也就成了必不可少的方案。在现有的Android API中也已存在支持分页加载内容的方案， 比如：

- `CursorAdapter`：它简化了数据库中数据到`ListView`中Item的映射， 仅查询需要展示的数据，但是查询的过程是在UI线程中执行。
- SupportV7包中的`AsyncListUtil`支持基于position的数据集分页加载到`RecyclerView`中，但不支持不基于position的数据集，而且它强制一个有限数据集中的null项必须展示Placeholder.

**针对现有方案所存在的一些问题，Google推出了Android架构组件中的Paging Library， 不过目前还是alpha版本。Paging Library主要由3个部分组成：`DataSource`、`PagedList`、`PagedListAdapter`。**

### 2、原理

> `DataSource`, `PagedList`, `PagedAdapter`三者之间的关系以及加载数据到展示数据的流程如下图所示：

（1）Paging的数据流是在后台线程生产的，在后台线程中完成了大部分工作，在UI线程中显示。
比如说：当一条新的item插入到数据库，DataSource会被初始化，LiveData\<PagedList>后台线程就会创建一个新的PagedList。这个新的PagedList会被发送到UI线程的PagedListAdapter中，PagedListAdapter使用DiffUtil在对比现在的Item和新建Item的差异。当对比结束，PagedListAdapter通过调用RecycleView.Adapter.notifyItemInserted()将新的item插入到适当的位置。RecycleView就会知道它需要绑定一个新的item，并将其显示。
（2）从代码层面来说，我们需要给Recyclerview设置PagedListAdater，PagedListAdapter设置对应的PagedList。每一次adapter getItem就是让PagedList知道我们已经滑到第几个item了，PagedList计算这些数量以及配置的参数，当条件达成就通知DataSource，让其返回数据。数据返回成功时，通知PagedListAdapter进行刷新等操作。



## 五、Room

Room为SQLite提供了一个抽象层，使得可以流畅使用SQLite的所有功能。

处理大量结构化数据的app可以从本地数据持久化中获取巨大利益。最常见的用例是缓存相关的数据。在这种情况下，当设备无法访问网络的时候，用户仍然可以在离线时浏览内容。任何用户原始数据的变化都会在连接网络后同步。

核心框架提供了原生SQL的支持。尽管这些API很强大，但是比较底层并且需要花费大量的时间和努力去使用：

- 没有原生SQL查询语句的编译时验证。当你的数据结构变化时，你需要手动更新受影响的SQL。这个过程会花费大量的时间并且很容易错误频出。

Room考虑到了这些，提供了SQLite的抽象层。

Room有三个主要的组件：

- 数据库（Database）：你可以使用该组件创建数据库的持有者。该**注解**定义了实体列表，该**类的内容**定义了数据库中的DAO列表。这也是访问底层连接的主要入口点。注解类应该是抽象的并且扩展自`RoomDatabase`。在运行时，你可以通过调用`Room.databaseBuilder()`或者`Room.inMemoryDatabaseBuilder()`获取实例。
- 实体（Entity）：这个组件代表了持有数据库表记录的类。对每种实体来说，创建了一个数据库表来持有所有项。你必须通过`Database`中的`entities`数组来引用实体类。实体的每个成员变量都被持久化在数据库中，除非你注解其为`@Ignore`。

> 实体类可以拥有无参数构造函数（如果DAO类可以访问每个持久化成员变量）或者拥有和实体类成员变量匹配参数的构造函数。Room也可以使用全部或者部分构造函数，例如只接收部分成员变量的构造函数。

- 数据访问对象（DAO）：这个组件代表了作为DAO的类或者接口。DAO是Room的主要组件，负责定义访问数据库的方法。被注解`@Database`的类必须包含一个无参数的抽象方法并返回被`@Dao`注解的类型。当编译时生成代码时，Room会创建该类的实现。

> 通过使用DAO类访问数据库而不是查询构建器或直接查询，你可以将数据库架构的不同组件分离。此外，DAO允许你在测试时很容易地模拟数据访问。