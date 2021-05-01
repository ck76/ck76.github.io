# 一、概述

`Dagger2`依赖注入框架的好处：

- 依赖的注入和配置独立于组件之外
- 依赖对象是在一个独立、不耦合的地方初始化，当初始化方式改变的时候修改的代码少。
- 依赖注入使得单元测试更加简单。

`Dagger2`相对于其它框架的优点：

- 编译期生成代码，有错误会在编译期报出。
- 错误可追踪。
- 易于调试。

`Dagger2`的缺点：

- 缺少灵活性。
- 没有动态机制。

# 二、`Dagger2`的注解

`Dagger2`的注解主要有以下七类：

- `@Inject`：这个注解有两个作用：在目标类中标记成员变量告诉`Dagger`这个类型的变量需要一个实例对象；标记依赖类中的构造方法，告诉`Dagger`我可以提供这种类型的依赖实例。
- `@Component`：用来标记接口或者抽象类，也被称为注入器，是`@Inject`和`@Module`的桥梁，所有的`Component`都可以通过它的`modules`知道它所提供的依赖范围，一个`Componet`可以依赖一个或多个`Component`，并拿到被依赖`Component`暴露出来的实例，`Componenet`的`dependencies`属性就是确定依赖关系的实现。
- `@Module`：用来标记类，一般类名以`Module`结尾，`Module`的主要作用是用来集中管理`@Provides`标记的方法，我们定义一个被`@Module`注解的类，`Dagger`就会知道在哪里找到依赖来满足创建类的实例，`Module`的一个重要特征是被设计成区块并可以组合在一起。
- `@Provides`：对方法进行注解，并且这些方法都是有返回类型的，告诉`Dagger`我们向如何创建并提供该类型的依赖实例（一般会在方法中`new`出实例），用`@Provides`标记的方法，推荐用`provide`作为前缀。
- `@Qualifier`：限定符，当一个类的类型不足以标示一个依赖的时候，我们就可以用这个注解，它会调用`DataModule`中方法来返回合适的依赖类实例。
- `@Scope`：通过自定义注解来限定作用域，所有的对象都不再需要知道怎么管理它的实例，`Dagger2`中有一个默认的作用域注解`@Singleton`，通常用来标记在`App`整个生命周期内存活的实例，也可以定义一个`@PerActivity`注解，用来表明生命周期要与`Activity`一致。
- `@SubComponent`：如果我们需要父组件全部的提供对象，我们就可以用包含方式，而不是用依赖方式，包含方式不需要父组件显示显露对象，就可以拿到父组件全部对象，且`SubComponent`只需要在父`Component`接扣中声明就可以了。

# 三、`Dagger2`的简单应用 - `@Inject`和`@Component`

第一步：基础配置，在`build.gradle`中添加相应的依赖：



```php
//添加(1)
apply plugin: 'com.neenbedankt.android-apt'

buildscript {
    repositories {
        jcenter()
    }
    dependencies {
        //添加(2)
        classpath 'com.neenbedankt.gradle.plugins:android-apt:1.8'
    }
}

android {
    compileSdkVersion 23
    buildToolsVersion "23.0.0"

    defaultConfig {
        applicationId "com.demo.zejun.repodragger2"
        minSdkVersion 15
        targetSdkVersion 23
        versionCode 1
        versionName "1.0"
    }
    buildTypes {
        release {
            minifyEnabled false
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
        }
    }
}

dependencies {
    compile fileTree(dir: 'libs', include: ['*.jar'])
    testCompile 'junit:junit:4.12'
    compile 'com.android.support:appcompat-v7:23.0.0'
    //添加(3)
    apt 'com.google.dagger:dagger-compiler:2.0'
    //添加(4)
    compile 'com.google.dagger:dagger:2.0'
}
```

第二步：`User`作为目标类中需要实例化的成员对象，给其构造函数添加`@Inject`标签：



```java
public class User {

    public String name;

    @Inject
    public User() {
        name = "lizejun";
    }

    public String getName() {
        return name;
    }
}
```

第三步：声明`Component`：



```java
@Component()
public interface OnlyInjectComponent {
    void inject(AnnotationActivity annotationActivity);
}
```

第四步：在目标类中添加注解`@Inject`，并根据我们第3步中声明的`Component`，调用`DaggerXXX`方法来进行注入：



```java
public class AnnotationActivity extends AppCompatActivity {

    @Inject
    public User mUser;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_dragger2);
        //在第3步声明的Component接口或者抽象类的基础上，添加Dagger前缀。
        DaggerOnlyInjectComponent.builder().build().inject(this);
    }

}
```

上面这个例子有两个缺点：

- 只能标记一个构造方法，因为如果标记两个以上，不知道要用哪一个构造提供实例。
- 不能标记其它我们不能修改的类，例如第三方库。
- 如果用`@Inject`标记的构造函数如果有参数，那么这个参数也需要其它地方提供依赖，而类似于`String`这些我们不能修改的类，只能用`@Module`中的`@Provides`来提供实例了。

# 四、采用`@Module`来提供依赖

采用`@Module`标记的类提供依赖是常规套路，`@Module`标记的类起管理作用，真正提供依赖实例靠的是`@Provides`标记的带返回类型的方法。
第一步：和上面类似，我们定义一个依赖类，但是它的构造方法并不需要用`@Inject`标记：



```cpp
public class Person {

    private String name;

    public Person() {
        this.name = "lizejun";
    }

    public String getName() {
        return name;
    }
}
```

第二步：我们需要定义一个`@Module`来管理这些依赖类的实例：



```java
@Module
public class PersonDataModule {

    @Provides
    public Person providePerson() {
        return new Person();
    }
}
```

第三步：定义一个`@Component`，它指向上面定义的`@Module`



```java
@Component(modules = {PersonDataModule.class})
public interface PersonInjectComponent {
    void inject(PersonInjectActivity injectActivity);
}
```

第四步：在目标类中进行依赖注入



```java
public class PersonInjectActivity extends Activity {

    @Inject
    Person mPerson;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        DaggerPersonInjectComponent.create().inject(this);
        System.out.println("Person name=" + mPerson.getName());
    }
}
```

这里注入的方式有两种，一种是像上面这样的，它适合于`PersonDataModule`中只有一个无参的构造方法，否则我们需要这样调用：



```css
DaggerPersonInjectComponent.builder().personDataModule(new PersonDataModule()).build().inject(this);
```

# 五、初始化依赖实例的步骤

- 查找`Module`中是否存在创建该类型的方法（即`@Component`标记的接口中包含了`@Module`标记的`Module`类，如果没有则直接查找`@Inject`对应的构造方法）。
- 如果存在创建类方法，则查看该方法是否有参数
- 如果不存在参数，直接初始化该类的实例，一次依赖注入到此结束。
- 如果存在参数，则从步骤1开始初始化每个参数。
- 如果不存在创建类方法，则查找该类型的类中有`@Inject`标记的构造方法，查看构造方法是否有参数：
- 如果不存在参数，则直接初始化该类实例，一次依赖注入到此结束。
- 如果存在参数，则从步骤1开始初始化每个参数。

# 六、`@Qualifier`限定符

在`Dagger`中，有一个已经定义好的限定符，`@Name`，下面我们也自己定义一个限定符：



```kotlin
@Qualifier
@Retention(RetentionPolicy.RUNTIME)
public @interface PeopleThreeQualifier {}
```

第一步：和前面类似，我们先定义一个需要实例化的依赖类：



```cpp
public class People {

    private int count;

    public People() {
        count = 0;
    }

    public People(int count) {
        this.count = count;
    }

    public int getCount() {
        return count;
    }
}
```

第二步：我定义一个`DataModule`，和前面不同的是，在它的`provideXXX`方法的注解中，我们添加了`@Name(xxx)`和自定义的注解`PeopleThreePeople`：



```java
@Module
public class PeopleDataModule {

    @Provides
    @Named("Five People")
    People provideFivePeople() {
        return new People(5);
    }

    @Provides
    @Named("Ten People")
    People provideTenPeople() {
        return new People(10);
    }

    @Provides
    @PeopleThreeQualifier
    People provideThreePeople() {
        return new People(3);
    }
}
```

第三步：定义`Component`



```java
@Component(modules = PeopleDataModule.class)
public interface PeopleInjectComponent {
    void inject(PeopleInjectActivity peopleInjectActivity);
}
```

第四步：在目标类中进行依赖注入，在提供`@Inject`注解时，我们还需要声明和`PeopleDataModule`中对应的限定符，这样`Dagger`就知道该用那个函数来生成目标类中的依赖类实例：



```java
public class PeopleInjectActivity extends Activity {

    @Inject
    @Named("Five People")
    People mFivePeople;

    @Inject
    @Named("Ten People")
    People mTenPeople;

    @Inject
    @PeopleThreeQualifier
    People mThreePeople;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        DaggerPeopleInjectComponent.builder().peopleDataModule(new PeopleDataModule()).build().inject(this);
        System.out.println("Five People=" + mFivePeople.getCount() + ",Ten People=" + mTenPeople.getCount() + ", Three People=" + mThreePeople.getCount());
    }
}
```

# 七、`@Scope`

`@Scope`的作用主要是在组织`Component`和`Module`的时候起到一个提醒和管理的作用，在`Dagger`中，有一个默认的作用域`@Singleton`。
`@Scope`的作用是：`Dagger2`可以通过自定义`Scope`注解，来限定通过`Module`和`Inject`方式创建的类的实例的生命周期能够与目标类的生命周期相同。`Scope`的真正作用在与`Component`的组织：

- 更好的管理`Component`之间的组织方式，不管是依赖方式还是包含方式，都有必要用自定的`Scope`注解标注这些`Component`，而且编译器会检查有依赖关系或包含关系的`Component`，若发现有`Component`没有用自定义`Scope`注解，则会报错。
- 更好地管理`Component`与`Module`之间地关系，编译器会检查`Component`管理的`Module`，若发现`Component`的自定义`Scope`注解与`Module`中的标注创建类实例方法的注解不一样，就会报错。
- 提高程序的可读性。

下面是一个使用`@Singleton`的例子：
第一步：定义需要实例化的类：



```cpp
public class AnSingleObject {

    private String objectId;

    public AnSingleObject() {
        objectId = toString();
    }

    public String getObjectId() {
        return objectId;
    }
}
```

第二步：定义`DataModule`，在它的`provideXXX`方法，提供了`@Singletion`注解：



```java
@Module
public class AnSingleObjectDataModule {

    @Provides
    @Singleton
    AnSingleObject provideAnSingleObject() {
        return new AnSingleObject();
    }
}
```

第三步：定义`Component`，和前面不同的是，需要给这个`Component`添加`@Singleton`注解：



```java
@Component(modules = {AnSingleObjectDataModule.class})
@Singleton
public abstract class AnSingleObjectInjectComponent {

    private static AnSingleObjectInjectComponent sInstance;

    public abstract void inject(AnSingleObjectInjectActivity anSingleObjectInjectActivity);

    public static AnSingleObjectInjectComponent getInstance() {
        if (sInstance == null) {
            sInstance = DaggerAnSingleObjectInjectComponent.create();
        }
        return sInstance;
    }
}
```

第四步：在目标类中进行依赖注入，每次启动`Activity`的时候，我们可以发现打印出来的`hash`值都是相同的：



```java
public class AnSingleObjectInjectActivity extends Activity {

    @Inject
    AnSingleObject object;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        AnSingleObjectInjectComponent.getInstance().inject(this);
        System.out.println("AnSingleObject id=" + object.getObjectId());
    }
}
```

# 八、组织`Component`

`Component`有三种组织方式：

- 依赖：一个`Component`依赖一个或多个`Component`，采用的是`@Component`的`dependencies`属性。
- 包含：这里就用到了`@SubComponent`注解，用它来标记接口或者抽象类，表示它可以被包干。一个`Component`可以包含一个或多个`Component`，而且被包含的`Component`还可以继续包含其它的`Component`。
- 继承：用一个`Component`继承另外一个`Component`。

# 九、`Google`官方框架分析

下面是`Google`官方框架的目录结构：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymdqa04zj308k0l2js1.jpg)

Paste_Image.png


可以看出，它把每个界面都作为一个独立的包（`addedittask、statistics、taskdetail、tasks`），而数据、依赖类是其它的两个包（`data、util`），我们先从`ToDoApplication`开始分析：





```java
public class ToDoApplication extends Application {

    private TasksRepositoryComponent mRepositoryComponent;
    
    @Override
    public void onCreate() {
        super.onCreate();
        mRepositoryComponent = DaggerTasksRepositoryComponent.builder()
                .applicationModule(new ApplicationModule((getApplicationContext())))
                .build();
    }
    
    public TasksRepositoryComponent getTasksRepositoryComponent() {
        return mRepositoryComponent;
    }
}
```

在`ToDoApplication`中，我们实例化了一个变量`TasksRepositoryComponent`，它相当于是项目中所有其它`Component`的管理者，它被声明为`@Singleton`的，即在`App`的生命周期中只存在一个，同时用`@Component`表明它和`TaskRepositoyModule`、`ApplicationModule`这两个`Module`关联。



```kotlin
@Singleton
@Component(modules = {TasksRepositoryModule.class, ApplicationModule.class})
public interface TasksRepositoryComponent {
    TasksRepository getTasksRepository();
}
```

`TaskRpositotyModule`提供了两种类型的数据源对象，它们是用`@Local`、`@Remote`来区分的：



```java
@Module
public class TasksRepositoryModule {

    @Singleton
    @Provides
    @Local
    TasksDataSource provideTasksLocalDataSource(Context context) {
        return new TasksLocalDataSource(context);
    }

    @Singleton
    @Provides
    @Remote
    TasksDataSource provideTasksRemoteDataSource() {
        return new FakeTasksRemoteDataSource();
    }

}
```

接下来再看一下`ApplicationModule`



```java
@Module
public final class ApplicationModule {

    private final Context mContext;

    ApplicationModule(Context context) {
        mContext = context;
    }

    @Provides
    Context provideContext() {
        return mContext;
    }
}
```

下面我们用一个比较简单的界面来看一下`TasksRepositoryComponent`是怎么和其它的`Component`关联起来的，首先看`StatisticsActivity`：



```java
public class StatisticsActivity extends AppCompatActivity {

    private DrawerLayout mDrawerLayout;

    @Inject 
    StatisticsPresenter mStatiticsPresenter; //依靠Dagger实例化的对象。

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.statistics_act);
        //初始化Fragment界面。
        StatisticsFragment statisticsFragment = (StatisticsFragment) getSupportFragmentManager().findFragmentById(R.id.contentFrame);
        if (statisticsFragment == null) {
            statisticsFragment = StatisticsFragment.newInstance();
            ActivityUtils.addFragmentToActivity(getSupportFragmentManager(),
                    statisticsFragment, R.id.contentFrame);
        }

        DaggerStatisticsComponent.builder()
            .statisticsPresenterModule(new StatisticsPresenterModule(statisticsFragment))
            .tasksRepositoryComponent(((ToDoApplication) getApplication())
            .getTasksRepositoryComponent())
            .build().inject(this);
    }

}
```



3人点赞



[架构设计]()





作者：泽毛
链接：https://www.jianshu.com/p/5285f48b6336
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。