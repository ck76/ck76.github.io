[TOC]

## 使用AndroidInjector实现Dagger注入

官方文章地址

Dagger & Android
Dagger2相比于其他大部分依赖注入框架的主要优点之一就是严格地生成实现(不使用反射),这意味着它可以应用于Android应用.然而在Android应用中使用Dagger2仍然需要考虑一些问题.

### 理念

当我们用java语言写Android代码时,与普通的java代码风格是完全不同的,通常情况下,这些差异的存在是为了适应手机平台的独特性能.

但是许多的用于Android的代码设计模式却和java的代码设计模式大相径庭,甚至Effective Java中的许多建议对于Android开发也是不适用的.

为了实现代码既规范可移植性又高的目标,Dagger依赖ProGuard来处理编译后的字节码.这使得Dagger在服务端和Android端的代码都看起来非常的自然,同时运用不同的工具类来减少字节码使其在两种开发环境中处理都更加高效.此外,Dagger还有个明确的目标就是保证生成的java代码与混淆文件的优化始终兼容.

当然,不是所有的问题都可以用这种方式加以解决,但它是Android兼容性提供的主要机制.

### 总结

Dagger假设Android开发者都会使用ProGuard

### 推荐的ProGuard设置

留意与使用Dagger的应用有关的ProGuard设置

## dagger.android
在Android应用程序中使用Dagger最难的一点就是许多framework层的类已经被系统实例化了,像Activity和Fragment,但是如果可以让Dagger来创建所有的注入对象,那么它的表现会非常优秀.相反的,你必须在生命周期方法中完成所有成员的注入,这意味着很多类最后都会变成以下这样:

```java
public class FrombulationActivity extends Activity {
  @Inject Frombulator frombulator;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    // DO THIS FIRST. Otherwise frombulator might be null!
    ((SomeApplicationBaseType) getContext().getApplicationContext())
        .getApplicationComponent()
        .newActivityComponentBuilder()
        .activity(this)
        .build()
        .inject(this);
    // ... now you can write the exciting code
  }
}
```

**这样写有几个问题:** 

- 这样复制粘贴代码使得以后代码重构将变得非常困难.越来越多的开发者复制粘贴代码块,但是只有很少人知道里面的工作机制是怎样的. 
- 从更根本的角度来看,这样写就要求请求注入类型(FrombulationActivity)需要知道它的注入器,即使通过接口而不是具体类型完成注入,这也打破了依赖注入的基本原则:一个类不应该知道如何实现依赖注入

dagger.android的类则提供了一种简化这种注入模式的方法

### 注入Activity对象

1. 在你的application容器中添加AndroidInjectionModule来保证所有的基础类型的必要绑定是可用的. 
2. 开始先写一个@Subcomponent注解并实现 AndroidInjector\<YourActivity>,伴随一个

```java
@Subcomponent(modules = ...)
public interface YourActivitySubcomponent extends AndroidInjector<YourActivity> {
  @Subcomponent.Builder
  public abstract class Builder extends AndroidInjector.Builder<YourActivity> {}
}
```

3. 在定义完subcomponent接口之后,通过定义一个绑定了subcomponent builder的module逐层添加到你的容器中,然后再将module添加到注入application的容器中:

```java
@Module(subcomponents = YourActivitySubcomponent.class)
abstract class YourActivityModule {
  @Binds
  @IntoMap
  @ActivityKey(YourActivity.class)
  abstract AndroidInjector.Factory<? extends Activity>
      bindYourActivityInjectorFactory(YourActivitySubcomponent.Builder builder);
}

@Component(modules = {..., YourActivityModule.class})
interface YourApplicationComponent {}
```

小贴士:如果在第二步中你的subcomponent及其builder或者超类型没有其他的方法,那么你可以使用@ContributesAndroidInjector来生成它们,而不是第二步和第三步,添加一个抽象的module方法并用@ContributesAndroidInjector注解,返回你的activity实例,然后指定你想要添加到subcomponent的module.如果subcomponent需要局部单例(scope),则将scope注解也添加到方法中.

```java
@ActivityScope
@ContributesAndroidInjector(modules = { /* modules to install into the subcomponent */ })
abstract YourActivity contributeYourActivityInjector();
```

4. 接下来,使你的application实现HasActivityInjector接口,然后使用@Inject注入DispatchingAndroidInjector\<Activity>来作为activityInjector()方法的返回值:

```java
public class YourApplication extends Application implements HasActivityInjector {
  @Inject DispatchingAndroidInjector<Activity> dispatchingActivityInjector;

  @Override
  public void onCreate() {
    super.onCreate();
    DaggerYourApplicationComponent.create()
        .inject(this);
  }

  @Override
  public AndroidInjector<Activity> activityInjector() {
    return dispatchingActivityInjector;
  }
}
```

5. 最后,在你Activity的onCreate()方法中,在super.onCreate()之前调用AndroidInjection.inject(this)方法.`

```java
public class YourActivity extends Activity {
  public void onCreate(Bundle savedInstanceState) {
    AndroidInjection.inject(this);
    super.onCreate(savedInstanceState);
  }
}
```

6. 恭喜,注入完成!

- 以上代码是如何运行的?
  AndroidInjection.inject(Activity)获取到application中的DispatchingAndroidInjector<Activity>,然后传递你的Activity实例给inject(Activity),DispatchingAndroidInjector从AndroidInjector.Factory 中寻找你的Activity对应的类 (YourActivitySubcomponent.Builder),创建AndroidInjector(YourActivitySubcomponent),然后传递Activity实例给inject(Activity)

### 注入Fragment对象

- 注入一个Fragment就像注入Activity一样简单.用相同的方式定义你的subcomponent,把Activity类型参数替换为Fragment,@ActivityKey替换成@FragmentKey,然后HasActivityInjector接口替换为HasFragmentInjector

- 正如Activity类型在onCreate()方法中完成注入的方式一样,在Fragment的onAttach()方法中完成相同操作.

- 不像Activity定义其module一样,对于定义Fragment的module你可以有不同选择,你可以使你的Fragment的容器成为别的类型的子容器(subcomponent),像别的fragment的容器,activity的容器,application的容器-这都取决于你的fragment需要绑定什么其他类型,在你决定了容器位置后,使其对应的类型实现HasFragmentInjector接口,举个例子,如果你的fragment需要从YourActivitySubcomponent绑定,那么你的代码将会是这个样子:

```java
public class YourActivity extends Activity
    implements HasFragmentInjector {
  @Inject DispatchingAndroidInjector<Fragment> fragmentInjector;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    AndroidInjection.inject(this);
    super.onCreate(savedInstanceState);
    // ...
  }

  @Override
  public AndroidInjector<Fragment> fragmentInjector() {
    return fragmentInjector;
  }
}

public class YourFragment extends Fragment {
  @Inject SomeDependency someDep;

  @Override
  public void onAttach(Activity activity) {
    AndroidInjection.inject(this);
    super.onAttach(activity);
    // ...
  }
}

@Subcomponent(modules = ...)
public interface YourFragmentSubcomponent extends AndroidInjector<YourFragment> {
  @Subcomponent.Builder
  public abstract class Builder extends AndroidInjector.Builder<YourFragment> {}
}

@Module(subcomponents = YourFragmentSubcomponent.class)
abstract class YourFragmentModule {
  @Binds
  @IntoMap
  @FragmentKey(YourFragment.class)
  abstract AndroidInjector.Factory<? extends Fragment>
      bindYourFragmentInjectorFactory(YourFragmentSubcomponent.Builder builder);
}

@Subcomponent(modules = { YourFragmentModule.class, ... }
public interface YourActivityOrYourApplicationComponent { ... }
```

### 基本framework类型

- 因为DispatchingAndroidInjector在运行时通过AndroidInjector.Factory寻找合适的类,一个基类除了实现HasActivityInjector/HasFragmentInjector/等接口还调用*AndroidInjection.inject(),所有的子类都需要用@Subcomponent注解绑定,Dagger提供给我们一些基本类型来做这件事情,像DaggerActivity and DaggerFragment,如果你并没有一个复杂的类,那么Dagger也提供给我们一个DaggerApplication用于同样的目的-你所需要做的就是继承它,然后重写applicationInjector()方法来返回应该注入到application中的容器.

- 以下类型也同样包括:
  - DaggerService和DaggerIntentService 
  - DaggerBroadcastReceiver 
  - DaggerContentProvider

- 注意:DaggerBroadcastReceiver只有当静态注册广播时才能调用,当你用代码动态注册时,最好用构造方法+@Inject形式注入.

### Support libraries

对于Android support library来说,以上这些类型也同样存在于dagger.android.support包下,注意但是用v4包的fragment时,开发者应该绑定的是AndroidInjector.Factory<YourActivity? extends android.support.v4.app.Fragment>,使用AppCompat的开发者应该继续实现AndroidInjector.Factory<? extends Activity> 而不是<? extends AppCompatActivity> (或者FragmentActivity)



### 核心类

- AndroidInjection：注入Android核心库的基本类型的实例
- AndroidInjector\<T>:注入Android库的类型的接口, T为Android库的基本类型T,比如Activity、Fragment、BroadcastReceive等；
- AndroidInjector.Factory\<T>：AndroidInjector\<T>的工厂类接口
- DispatchingAndroidInjector\<T>:其为AndroidInjector\<T>接口的实现类，将Android核心库的的基本类型T的实例注入Dagger，该操作是由Android核心库的类的实例本身执行，而不是Dagger。