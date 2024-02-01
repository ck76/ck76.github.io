[TOC]



### 一、前言

> 说明：今年的Google I/O大会关于安卓的部分发布了全新的类库：Architecture Components。这个新的类库致力于从架构层面帮助你设计健壮、易于测试以及易于维护的app，其中包括UI组件生命周期的管理以及数据持久化等部分。我个人对这个类库非常感兴趣，很早就想写一些关于这方面的文章，但是由于私人事务问题近期才有时间。我会先发布这个类库相关文档的译文，在后面时间富裕的时候再聊一聊对这个类库的理解和在实际应用的经历。目前该类库还处在alpha阶段，但这并不影响我们对此的学习，当正式版放出后我相信会受到很多开发者的青睐

这份文档用于已经掌握构建Android app基本技能，现在想要了解推荐的架构，想要实践如何构建健壮、生产级别app的开发者。

> 本文档假设读者已经熟悉Android框架。如果你刚跟接触Android，请访问[这里]的训练系列，该训练包含了本文档的所有预备知识。



### 二、app开发者所面临的常见问题

与之对应的传统桌面应用在大多数情况下含有一个单一的入口点（快捷图标）并运行作为一个单一的程序，这和Android应用很不同。Android app拥有更复杂的结构。一个典型的Android app往往由多种组件构建而成，包括`Activity`, `Fragment`, `Service`, `Content Provider`以及`Broadcast Receiver`。

这些app组件大部分被声明在app清单文件（AndroidManifest）中，该清单文件被Android系统用于决定如何整合你的app到全局的用户体验中。如上文所说，传统的桌面应用通常作为一个整体运行，而一个编写良好的Android应用需要更加灵活，因为用户常常在不同的app间频繁切换。

例如，考虑当你想在你最喜欢的社交网络上分享一张照片时会发生什么？app触发一个相机的`Intent`，Android系统启动了一个相机应用来处理请求。在这个时候，用户离开了该社交网络app，但是在体验上却是无缝衔接的。接着，相机app可能触发其他Intent来开启其他应用，例如启动文件选择器。最终，用户回到了社交网络app并分享了图片。同样地，用户可能在这一处理过程中的任何时刻被电话接听所打断，在接听完成后继续回来分享图片。

在Android中，这种应用频繁切换的行为很常见，因此你的app必须能够正确处理这些行为。请记住，手机设备是被资源所约束的，因此在任何时候操作系统都有可能为了给新开启的app腾出空间而杀死一些app。

关于这一切的关键点在于你的app组件可以单独启动并且是无序的，以及该组件可以在任何时候被用户或系统销毁。因为app组件是短暂的，并且它们的生命周期（例如何时创建以及何时销毁）并不受你控制。*你不能在你的app组件中存储任何数据或状态*，并且你的组件之间不应该互相依赖。



### 三、常见架构原则

如果你不能使用app组件来存储应用的数据和状态，那么app该如何构建呢？

你所该关注最重要的事情是在你的app中遵守关注点分离原则。一个常见的错误是把你所有的代码都写在`Activity`或者`Fragment`中。任何不操作UI或操作系统交互的代码都不应该放在上述这些类中。请尽量保持这些类的体积瘦小以避免许多生命周期相关的问题。不要忘记你并不拥有这些类，它们只是在你的应用和系统之间交互的粘合剂。安卓系统会在任何时候销毁它们，例如用户的交互行为或者其他因素，如可用内存过低等。为了提供一个可靠的用户体验，最好减少对它们的依赖。

第二个最重要的原则是你应该*用模型驱动界面*，最好是持久化模型(Persistent Model)。持久化是一个理想的状态，理由如下：

1. 如果操作系统销毁了你的应用来释放资源，你的用户不应该因此而丢掉数据。
2. 甚至当网络堵塞甚至未连接时，你的应用应当继续工作。Model是负责处理应用数据的组件，它们独立于视图(View)以及其他app组件，因此Model和这些生命周期相关的问题也是隔绝的。保持UI代码的简洁以及应用逻辑的自由更易于进行管理。将你的app基于Model类构建将对数据管理有利，并使得它们易于测试。



### 四、推荐app架构

在这一章节，我们致力于如何使用架构组件(Architecture Components)来构建一个app，我们将通过一个用例进行说明。

> 软件工程领域没有银弹。我们不可能找到一种最佳的方法能够一劳永逸地适合所有的场景。但是我们所推荐架构的意义在于对大多数用例来说都是好的。如果你已经有一个比较好的方式来写Android应用，那么你不需要做出改变。

想象一下我们正在构建一个显示用户资料的UI界面。该用户界面将通过REST API从我们的私有后台获取。



### 五、构建用户界面

UI界面将会由一个叫做`UserProfileFragment.java`的`Fragment`和对应的布局文件`user_profile_layout.xml`组成。

为了驱动UI界面，我们的数据模型需要持有两个数据元素：

- User ID:用于区分用户。通过`fragment`参数将信息传递至`Fragment`是最佳的方式。如果Android系统销毁了你的进程，这个信息将会被保存，因此当app下次重启时，该id也将是可用的
- User Object:一个含有用户数据的POJO类

我们将会创建一个基于`ViewModel`类的`UserProfileViewModel`来保存信息。

> 一个`ViewModel`提供了指定UI组件的数据，例如一个`fragment`或`activity`，并处理数据的交互，例如调用其他组件加载数据或数据的更新修改等。`ViewModel`并不知道`View`，也不受配置信息变化的影响，例如由于屏幕旋转造成的`Activity`重建。

现在我们拥有以下三个文件：

- `user_profile.xml`: 定义了屏幕的UI布局
- `UserProfileViewModel.java`:准备用于UI的数据类
- `UserProfileFragment.java`: UI控制器，在`ViewModel`中显示数据以及响应用户交互

下面是我们的初步实现（布局文件比较简单直接省略）：

```java
 public class UserProfileViewModel extends ViewModel {
     private String userId;
     private User user;
 
     public void init(String userId) {
         this.userId = userId;
     }
     public User getUser() {
         return user;
     }
 }

 public class UserProfileFragment extends LifecycleFragment {
     private static final String UID_KEY = "uid";
     private UserProfileViewModel viewModel;
 
     @Override
     public void onActivityCreated(@Nullable Bundle savedInstanceState) {
         super.onActivityCreated(savedInstanceState);
         String userId = getArguments().getString(UID_KEY);
         viewModel = ViewModelProviders.of(this).get(UserProfileViewModel.class);
         viewModel.init(userId);
     }
 
     @Override
     public View onCreateView(LayoutInflater inflater,
                 @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
         return inflater.inflate(R.layout.user_profile, container, false);
     }
 }
```

> 如果你已经使用了类似于`RxJava`或者`Agera`这样的库，你可以继续使用它们，而不是`LiveData`。但是如果当你使用它们，请确保正确地处理了生命周期，例如当相关的生命周期拥有者（LifecycleOwner）停止时应当暂停，当生命周期持有者销毁时也应当销毁。你也可以添加`android.arch.lifecycle:reactivestreams`，使`LiveData`和其他响应流式库共同使用，例如`RxJava`。

现在我们将`UserProfileViewModel`中的`User`成员变量替换为`LiveData<User>`，使得当数据更新时，`Fragment`可以收到通知。关于`LiveData`一件很棒的事是，它能够对生命周期做出反应，并将在不再需要的时候自动清除引用。

```java
 public class UserProfileViewModel extends ViewModel {
     //...
     private LiveData<User> user;//替换行
     public LiveData<User> getUser() {
         return user;
     }
 }
```

现在我们修改`UserProfileFragment`，观察数据变化并更新UI。

```java
@Override
public void onActivityCreated(@Nullable Bundle savedInstanceState) {
    super.onActivityCreated(savedInstanceState);
    viewModel.getUser().observe(this, user -> {
      // 此处更新 UI
    });
}
```

每次用户数据被更新时，`onChanged`回调函数会被调用，UI界面会被更新。

如果你熟悉其他使用观察回调的类库，你可能会意识到我们并没有复写`Fragment`的`onStop()`方法来停止对数据的观察。这在`LiveData`**中是不必要的，因为它对生命周期敏感**，这意味着将不会调用回调函数，除非`Fragment`出在激活状态（接收`onStart()`但没有接受`onStop()`）。当`Fragment`接收`onDestroy()`方法时，`LiveData`将会自动清除观察者。

我们也不会做任何特殊的事情来处理配置的变化（例如旋转屏幕）。当配置发生变化的时候，`ViewModel`将会自动保存，因此一旦新的`Fragment`到来时，它将会收到`ViewModel`的相同实例，带有当前数据的回调函数将会立即被调用。这就是`ViewModel`不应该直接引用`View`的原因，`ViewModel`会在`View`的生命周期外存活。详见：[ViewModel的生命周期]。



### 六、获取数据

现在我们将ViewModel和`Fragment`关联在了一起，但是ViewModel该如何获取数据呢？在本例下，我们假设我们的后台提供了REST API。我们会用`Retrofit`库来访问我们的后台，当然你可以随意选择其他不同的类库。

这里就是和我们后台交互的retrofit接口`Webservice`：

```java
public interface Webservice {
    /**
     * @GET 声明是一个HTTP GET请求
     * @Path("user") 标记了userId参数来替换GET请求中的{user}路径
     */
    @GET("/users/{user}")
    Call<User> getUser(@Path("user") String userId);
}
```

> 关于Retrofit的使用请详见官方文档，这里只是简单进行了说明

`ViewModel`的原生实现可以直接调用`Webservice`来获取数据并交给用户对象。即使这样可以生效，你的app将会随着增长而难以维护。相对于我们上文所提到的关注点分离原则，这种方式给予了`ViewModel`类太多的职责。另外`ViewModel`的作用于被绑在`Activity`或`Fragment`的生命周期上，因此当生命周期结束的时候丢掉这些数据是一种很糟糕的用户体验。**作为替代，我们的`ViewModel`将会把这一工作委派给新的*仓库（Repository）*模块。**

> 仓库模块(Repository Module)负责处理数据操作。他们提供了清晰的API，并且知道在哪获取数据以及哪种API的调用会导致数据更新。你可以考虑把它作为多种数据源的中介（持久化模型，网络服务数据，缓存等）。

下方的`UserRepository`类将会使用`WebService`来获取数据项：

```java
public class UserRepository {
    private Webservice webservice;
    // ...
    public LiveData<User> getUser(int userId) {
        // 这并不是最佳的实现方式，我们将在下文修正它
        final MutableLiveData<User> data = new MutableLiveData<>();
        webservice.getUser(userId).enqueue(new Callback<User>() {
            @Override
            public void onResponse(Call<User> call, Response<User> response) {
                // 错误情况的处理被省略了
                data.setValue(response.body());
            }
        });
        return data;
    }
}
```

即使仓库模型看起来并不需要，但是它完成了一个重要的目标：它将app中的数据源抽象了出来。现在我们的`ViewModel`不知道数据是由`Webservice`获取而来的，这意味着在需要其他实现的时候我们可以进行替换。



### 七、管理组件间的依赖

上面的`UserRepository`类需要`WebService`接口的一个实例去进行工作。我们当然可以在每个仓库模型类中简单地创建一个，不过需要知道`WebService`所依赖的具体子类。这将会显著提高代码的复杂性和冗余。另外`UserRepository`也可能不是唯一需要`WebService`的类，如果每个类都创建一个`WebService`，这将会浪费很多的资源。

有两种模式可以解决这个问题：

- 依赖注入：依赖注入允许类定义依赖而不用去构造他们。在运行的时候，另一个类负责提供这些依赖关系。我们推荐在安卓中使用谷歌的[Dagger 2]类库进行依赖注入。通过遍历依赖树，Dagger 2 自动构造对象并提供编译时的依赖保障。
- 服务定位：服务定位器提供了注册器，使得类可通过依赖进行构建，而不是需要配置它们。服务定位模式相对依赖注入而言更易于实现，因此如果你并不熟悉依赖注入，可以使用服务定位来代替。



### 八、连接`ViewModel`和仓库

现在我们修改我们的`UserProfileViewModel`以使用仓库：

```java
public class UserProfileViewModel extends ViewModel {
    private LiveData<User> user;
    private UserRepository userRepo;

    @Inject // UserRepository 参数由Dagger 2提供
    public UserProfileViewModel(UserRepository userRepo) {
        this.userRepo = userRepo;
    }

    public void init(String userId) {
        if (this.user != null) {
            // ViewModel 由每个fragment创建，因此我们知道并不会发生改变
            return;
        }
        user = userRepo.getUser(userId);
    }

    public LiveData<User> getUser() {
        return this.user;
    }
}
```



### 九、缓存数据

上述仓库的实现易于抽象了调用网络服务的过程，但是因为它仅仅依赖于一个单一的数据源，因此并不是很实用。

`UserRepository`实现的问题在于在获取数据以后，并没有在任何地方保存它。如果用户离开了`UserProfileFragment`并再次回来，app会重新获取数据。这很糟糕，有以下两个原因：

1. 浪费了宝贵的网络带宽；
2. 强迫用户等待新的请求完成。为了解决这个问题，**我们将在`UserRepository`添加一个新的数据源在内存中缓存我们的`User`对象。**

```java
@Singleton  // 通知 Dagger 该类应该只构建一次
public class UserRepository {
    private Webservice webservice;
    // 简单缓存在内存中，忽略实现细节
    private UserCache userCache;
    public LiveData<User> getUser(String userId) {
        LiveData<User> cached = userCache.get(userId);
        if (cached != null) {
            return cached;
        }

        final MutableLiveData<User> data = new MutableLiveData<>();
        userCache.put(userId, data);
        // 这仍然不是最优的代码，但是要比之前的代码好
        // 一个完整的实现必须处理错误情况
        webservice.getUser(userId).enqueue(new Callback<User>() {
            @Override
            public void onResponse(Call<User> call, Response<User> response) {
                data.setValue(response.body());
            }
        });
        return data;
    }
}
```



### 十、数据持久化

在我们当前的实现中，如果用户旋转了屏幕或者离开并返回app，当前UI界面将立刻可见，这是因为仓库从内存中获取了数据。但是如果用户离开app很久，在Android系统杀掉进程后再回来呢？

在当前的实现中，我们需要从网络重新获取数据。这并不仅是一个很糟糕的用户习惯，并且很浪费，因为我们要重新获取相同的数据。你可以仅仅通过缓存网络请求来修复它，但是这也创造了新的问题。如果相同的数据类型在另一个请求中发生（如获取一组好友列表）呢？如果是这样，你的app可能会显示不正确的数据。

正确解决这个问题的关键在于使用一个持久化模型。这正是`Room`持久化类库所解决的问题。

> `Room`是一个以最小化模板代码提供本地数据持久化的对象关系映射类库。在编译时间，它会验证每个查询语句，因此错误的SQL会导致编译时报错，而不是在运行时报错。`Room`抽象了一些原生SQL表和查询的底层实现细节。它也允许观察数据库数据的变化，通过`LiveData`对象进行展现。此外，它显式地定义线程约束以解决一些常见的问题，如在主线程访问存储。

> 如果你对另一些持久化解决方案很熟悉，你并不需要进行替换，除非`Room`的功能集和你的用例更符合。

为了使用`Room`，我们需要定义我们的本地表。首先使用`@Entity`去注解`User`类，标记该类作为数据库中的表。

```java
@Entity
class User {
  @PrimaryKey
  private int id;
  private String name;
  private String lastName;
  // getters/setters
}
```

之后，通过扩展`RoomDatabase`类创建一个数据库类：

```java
@Database(entities = {User.class}, version = 1)
public abstract class MyDatabase extends RoomDatabase {
}
```

注意，`MyDatabase`类是抽象的，`Room`会自动提供实现。详情请参见`Room`文档。

现在我们需要一个方式将用户数据插入到数据库中，为此我们需要创建一个数据访问对象(DAO)：

```java
@Dao
public interface UserDao {
    @Insert(onConflict = REPLACE)
    void save(User user);
    @Query("SELECT * FROM user WHERE id = :userId")
    LiveData<User> load(String userId);
}
```

之后，从我们的数据库类中引用DAO：

```java
@Database(entities = {User.class}, version = 1)
public abstract class MyDatabase extends RoomDatabase {
    public abstract UserDao userDao();
}
```

**请注意`load`方法返回了一个`LiveData<User>`。**`Room`知道数据库什么时候被修改并将在数据变化时自动通知所有已激活的观察者。使用了`LiveData`是很高效的，因为只有在至少含有一个处在激活状态的观察者时才会更新。

> 目前处在alpha 1版本中，`Room`会检查基于表修改的错误信息，也就是说会分发假阳性的通知。假阳性是指分发的通知是正确的，但是并非是由数据变化所造成的。

现在我们修改`UserRepository`类，将`Room`数据源包含在内。

```java
@Singleton
public class UserRepository {
    private final Webservice webservice;
    private final UserDao userDao;
    private final Executor executor;

    @Inject
    public UserRepository(Webservice webservice, UserDao userDao, Executor executor) {
        this.webservice = webservice;
        this.userDao = userDao;
        this.executor = executor;
    }

    public LiveData<User> getUser(String userId) {
        refreshUser(userId);
        //直接从数据库返回数据
        return userDao.load(userId);
    }

    private void refreshUser(final String userId) {
        executor.execute(() -> {
            // 运行在后台线程
            // 检查用户最新是否获取更新
            boolean userExists = userDao.hasUser(FRESH_TIMEOUT);
            if (!userExists) {
                // 刷新数据
                Response response = webservice.getUser(userId).execute();
                // TODO 错误情况监测处理（省略）
                // 更新数据库，LiveData会自动更新，因此只需要更新数据库就可以了
                userDao.save(response.body());
            }
        });
    }
}
```

请注意，即使我们在`UserRepository`中改变了数据源，我们仍然不需要改变`UserProfileViewModel`或者`UserProfileFragment`。这种灵活性是由抽象所提供的。这对于测试来说也是很棒的，因为你可以在测试`UserProfileViewModel`的时候提供一个假的`UserRepository`。

现在我们的代码完成了。如果用户稍后再次回到相同的UI，将会立即看到用户信息，因为我们进行了持久化。同时，如果数据过时了，我们的仓库会在后台更新数据它们。当然这取决于你的具体用例，你可以选择在数据过时的时候不显示它们。

在一些用例中，例如pull-to-refresh，对于UI来说如果当前在进行网络请求，对用户显示该进度是很重要的。将UI的行为和实际数据分离是一种很好的实践，因为数据可能因为多种原因被更新（例如如果我们拉取一组朋友列表，已存在的数据可能会被再次获取，从而触发了`LiveData<User>`更新）。从UI的角度来看，事实上是另一个数据端。

该用例有两个常见的方案：

- 修改`getUser()`方法，返回带有网络操作状态的`LiveData`，例如下文中的“显示网络状态”章节。
- 在仓库类中提供另一个公共方法，返回`User`类的刷新状态。这种方式更好，如果你想要仅仅在响应显式地用户操作（如pull-to-refresh）时显示网络状态。



### 十一、真正单一数据源

对于不同的REST API返回相同的数据是很常见的，例如，如果我们的后台有另一个接口用于返回朋友列表，相同的`User`对象会从两个API返回。如果`UserRepository`也要去返回`Webservice`请求的结果，我们的UI界面可能会显示不正常数据，因为数据可能会因这两个请求接口而改变。这也就是为什么在`UserRepository`实现中，网络服务仅仅存储数据到数据库的原因。之后，数据库信息的改变会触发`LiveData`的更新。

在这种模型下，数据库作为单一数据源，而app的其他部分通过仓库进行访问。不论你是否使用持久化存储，我们推荐你的仓库指定一个数据源作为app的单一数据源。

#### 1、测试

关注点分离原则一个很重要的受益处在于可测试性。让我们看看每个模块代码的测试。

- UI&交互：这是唯一需要[Android UI Instrumentation test]的时刻。测试UI的最佳方式是创建一个[Espresso]特使。你可以创建`Fragment`并提供一个虚拟的`ViewModel`。因为`Fragment`仅仅和`ViewModel`对话，模拟`ViewModel`对于测试来说就已经足够了。

- ViewModel：`ViewModel`可以使用[JUnit测试]。你仅仅需要模拟`UserRepository`。

- UserRepository：你也可以使用`JUnit`测试`UserRepository`。你需要模拟`Webservice`和DAO。你可以测试网络请求调用，在数据库中保存结果，以及如果数据被缓存并更新后不需要进行请求。因为`Webservice`和`UserDao`都是接口，你可以模拟它们。

- UserDao：测试DAO类的推荐方法是使用测试工具。因为这些测试工具并不需要任何的UI并运行速度很快。对每个测试来说，你可以创建一个内存数据库来保证测试并不会造成双边效应（如改变磁盘上数据库的已有数据）。

- WebService：独立于外部世界的测试是很重要的，甚至你的`Webservice`测试应该避免调用后台的网络服务。有大量的类库可以帮助做到这一点，例如：[MockWebServer]。

- 测试构件：架构组件提供一个Maven构件来控制后台线程。在

  ```
  android.arch.core:core-testing
  ```

  中，有两个

  ```
  JUnit
  ```

  规则：

  - 任务立即执行规则：这个规则可用于强制架构组件在调用线程里立即执行任何后台操作
  - 这个规则可用于工具测试，以等待架构组件的后台操作或者连接至`Espresso`作为闲置资源。



### 十二、最终架构

下图显示了我们所推荐架构的所有模块，以及相互间的交互情况：



![img](http://s191.photo.store.qq.com/psb?/V14L47VC0w3vOf/y2lnxomGdYliLf.HHPGm5hAVDZN7t3qWQ9mxl1OKSMM!/b/dL8AAAAAAAAA)





### 十三、指导原则

以下的建议并不是强制性的，而是根据我们的经验得知，遵循这些建议会使你的代码更健壮，易于测试和易于维护。

- 你在清单文件中所定义的入口点——Activity，Service，Broadcast Receiver等并不应该是数据源。相反，他们应该仅仅是和入库点相关的数据源子集。因为每个app的组件的存活时间都是短暂的、取决于用户的交互行为以及运行时整体上的健康度。
- 残忍坚决地创建良好的模块分界。例如，不要将从网络读取数据的代码扩展到多个类/包中。相似地，也不要将不相关职责的代码添加进来，如数据缓存等。（高内聚，低耦合）
- 模块间交互暴露的接口应该尽可能的少。不要尝试创建“仅仅用一次”的捷径，导致暴露一个模块的内部实现细节。你可能在短期会获益，但是在代码的演进过程中会耗费数倍的技术负担。
- 当你定义了模块间的交互时，考虑每个模块的单独可测试性。例如，有一个定义良好的用于从网络获取数据的API会更易于测试本地数据库持久化的模块。相反，如果你搞乱了两个模块间的逻辑，或将你网络请求的代码铺满了所有的地方，那么这将很难进行测试。
- 你app的核心是如何在其他app中变得突出。不要花费时间重复造轮子或一遍一遍地写相同的模板代码。相反，将你的心思花在如何使你的app独一无二，让Android架构组件以及其他推荐类库处理重复的部分。
- 持久化尽可能多和尽可能新鲜的数据，这样在离线模式下你的app也是可用的。你可能很享受高速的网络连接，可你的用户并不一定这样认为。
- 你的仓库应当指定单一数据源。当你的app需要访问数据时，应该永远来自于这个单一的数据源。



### 十四、附加：显示网络状态

在“推荐app架构”一节中，我们故意忽略了网络错误和加载状态，以使样例代码更简单。在本节中，我们致力于使用`Resource`类显示网络状态以及数据本身。

下面是样例的实现：

```java
//一个描述数据以及其状态的泛型类
public class Resource<T> {
    @NonNull public final Status status;
    @Nullable public final T data;
    @Nullable public final String message;
    private Resource(@NonNull Status status, @Nullable T data, @Nullable String message) {
        this.status = status;
        this.data = data;
        this.message = message;
    }

    public static <T> Resource<T> success(@NonNull T data) {
        return new Resource<>(SUCCESS, data, null);
    }

    public static <T> Resource<T> error(String msg, @Nullable T data) {
        return new Resource<>(ERROR, data, msg);
    }

    public static <T> Resource<T> loading(@Nullable T data) {
        return new Resource<>(LOADING, data, null);
    }
}
```

因为从网络加载数据并进行显示是一个常见的用例，我们创建了一个帮助类`NetworkBoundResource`可以在多个地方复用。下图是`NetworkBoundResource`的决策树：



![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/LFCfTBpaj8EDXfyzNPcvLaQlSUhqWdOmC5KPubP9ze8!/r/dDQBAAAAAAAA)



起点从观察数据源（数据库）开始。当入口被数据库第一次加载时，`NetworkBoundResource`检查结果是否足够良好以至于可以分发，并且/或应该从网络进行获取。注意，这二者可以同时发生，因为你可能想要显示缓存，同时从网络更新数据。

如果网络调用完全成功，保存结果至数据库并重新初始化数据流。如果网络请求失败，我们直接分发一个错误。

> 将新的数据存储到磁盘以后，我们从数据库重新初始化数据流，但是通常我们并不需要这样做，因为数据库会分发这次变化。另一方面，依赖数据库去分发变化会是一把双刃剑，如果数据并没有变化，我们实际上可以避免这次分发。我们也不分发网络请求得到的数据，因为这违反了单一数据源的原则。

以下是`NetworkBoundResource`所提供的API：

```java
// ResultType: 数据源类型
// RequestType: API返回的类型
public abstract class NetworkBoundResource<ResultType, RequestType> {
    // 被调用保存API返回的结果至数据库
    @WorkerThread
    protected abstract void saveCallResult(@NonNull RequestType item);

    // 被调用去判断是否应该从网络获取数据
    @MainThread
    protected abstract boolean shouldFetch(@Nullable ResultType data);

    // 被调用从数据库获取缓存数据
    @NonNull @MainThread
    protected abstract LiveData<ResultType> loadFromDb();

    // 被调用创建API请求
    @NonNull @MainThread
    protected abstract LiveData<ApiResponse<RequestType>> createCall();

    // 当获取数据失败时候调用
    @MainThread
    protected void onFetchFailed() {
    }

    // 返回代表数据源的LiveData
    public final LiveData<Resource<ResultType>> getAsLiveData() {
        return result;
    }
}
```

注意，上面的类定义了两种类型的参数（`ResultType`和`RequestType`），因为从API返回的数据类型可能和本地的数据类型并不匹配。

同样也请注意，上面的代码使用了`ApiResponse`用于网络请求。`ApiResponse`是`Retrofit2.Call`类的简单包装，用于将返回结果转化为`LiveData`。

下面的`NetworkBoundResource`的其余实现：

```java
public abstract class NetworkBoundResource<ResultType, RequestType> {
    private final MediatorLiveData<Resource<ResultType>> result = new MediatorLiveData<>();

    @MainThread
    NetworkBoundResource() {
        result.setValue(Resource.loading(null));
        LiveData<ResultType> dbSource = loadFromDb();
        result.addSource(dbSource, data -> {
            result.removeSource(dbSource);
            if (shouldFetch(data)) {
                fetchFromNetwork(dbSource);
            } else {
                result.addSource(dbSource,
                        newData -> result.setValue(Resource.success(newData)));
            }
        });
    }

    private void fetchFromNetwork(final LiveData<ResultType> dbSource) {
        LiveData<ApiResponse<RequestType>> apiResponse = createCall();
        // 重新连接dbSource作为新的源,
        //这样会快速分发最新的数据
        result.addSource(dbSource,
                newData -> result.setValue(Resource.loading(newData)));
        result.addSource(apiResponse, response -> {
            result.removeSource(apiResponse);
            result.removeSource(dbSource);
            if (response.isSuccessful()) {
                saveResultAndReInit(response);
            } else {
                onFetchFailed();
                result.addSource(dbSource,
                        newData -> result.setValue(
                                Resource.error(response.errorMessage, newData)));
            }
        });
    }

    @MainThread
    private void saveResultAndReInit(ApiResponse<RequestType> response) {
        new AsyncTask<Void, Void, Void>() {

            @Override
            protected Void doInBackground(Void... voids) {
                saveCallResult(response.body);
                return null;
            }

            @Override
            protected void onPostExecute(Void aVoid) {
                // 我们专门请求一个新的LiveData
                // 另一方面获取最新的缓存数据，可能并不是网络请求得到的最新数据
                result.addSource(loadFromDb(),
                        newData -> result.setValue(Resource.success(newData)));
            }
        }.execute();
    }
}
```

现在，我们使用`NetworkBoundResource`来重写`UserRepository`：

```java
class UserRepository {
    Webservice webservice;
    UserDao userDao;

    public LiveData<Resource<User>> loadUser(final String userId) {
        return new NetworkBoundResource<User,User>() {
            @Override
            protected void saveCallResult(@NonNull User item) {
                userDao.insert(item);
            }

            @Override
            protected boolean shouldFetch(@Nullable User data) {
                return rateLimiter.canFetch(userId) && (data == null || !isFresh(data));
            }

            @NonNull @Override
            protected LiveData<User> loadFromDb() {
                return userDao.load(userId);
            }

            @NonNull @Override
            protected LiveData<ApiResponse<User>> createCall() {
                return webservice.getUser(userId);
            }
        }.getAsLiveData();
    }
}
```

