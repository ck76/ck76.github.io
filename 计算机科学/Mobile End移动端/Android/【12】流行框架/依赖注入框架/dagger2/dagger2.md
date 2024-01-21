[TOC]

![dagger2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/9XFQsC8ocYgCQ71HFygzZ*T3VujlruVGqclsQRN11PA!/r/dAQBAAAAAAAA)

@Inject ： 注入，被注解的构造方法会自动编译生成一个**Factory**工厂类提供该类对象。

@Component: 注入器，类似快递员，作用是将产生的对象注入到需要对象的容器中，供容器使用。

@Module: 模块，类似快递箱子，在Component接口中通过@Component(modules = xxxx.class),将容器需要的商品封装起来，统一交给快递员（Component），让快递员统一送到目标容器中。

Inject主要是用来标注目标类的依赖和依赖的构造函数

Component它是一个桥梁，一端是目标类，另一端是目标类所依赖类的实例，它也是注入器（Injector）负责把目标类所依赖类的实例注入到目标类中，同时它也管理Module。

Module和Provides是为解决第三方类库而生的，Module是一个**简单工厂模式**，Module可以包含创建类实例的方法，这些方法用Provides来标注

有一个很好的比喻整个依赖注入的过程好比在网上买东西，依赖到达被注入的地方就像买的东西到自己的手里，@Component充当快递员，只有@Inject构造函数和@Inject属性/方法仍然可以完成依赖注入，就相当于快递员直接拿着商品送到客户手中，如果再加上@Module，相当于把客户买的东西先集中起来，从这里挑选送到对应的客户

- XXX_Factory 
  - @Inject标注在构造器时，说明该类的实例交由Dagger生成
- Module类名_方法名Factory implements Factory\<XXX>
  - @Provides注解
- 被注入类名_MembersInjector implements MembersInjector\<被注入类名>
  - @Inject标注在被注入类字段上时
  - 里面有多少个@Inject就会有多少个injectXXX方法
- Provider\<T>
  - 其get()方法用来提供T

```java
 每一个 @provides生成一个Factory，@Inject注解到构造函数也会生成Factory
 总结来说就是
 	- Factory是用来生成实例的(每个@Provides或者@Inject对应一个Factory)
 	- MenberInjecter是为类做注入的(只要在类中@Inject了就会生成对应的MenberInjecter)
    - Provider 具体来通过其get()方法提依赖的
    
//通过Component 创建获得Activity,获得工厂类Provider，统一交给Injector
//Injector将Provider的get()方法提供的对象，
//注入到Activity容器对应的成员变量中，我们就可以直接使用Activity容器中对应的成员变量了！
```





## 注解介绍

### @Module

@Module注解在类或接口上，表明该类或接口用于提供相关依赖。该类或接口中使用的注解有**@Provides，@Binds，@IntoSet，@IntoMap**。

@Module用于标注提供依赖的类。你可能会有点困惑，上面不是提到用@Inject标记构造函数就可以提供依赖了么，为什么还需要@Module？很多时候我们需要提供依赖的构造函数是第三方库的，我们没法给它加上@Inject注解，又比如说提供以来的构造函数是带参数的，如果我们之所简单的使用@Inject标记它，那么他的参数又怎么来呢？@Module正是帮我们解决这些问题的。



---

### @Binds与@Provides

相信大家经常会使用`@Provides`来在`Module`里面提供需要注入对象的构造, 但从来没有用过`@Binds`.

如果我们需要注入一个接口的实现,我们常常会这么做:

```java
@Provides
public XXInterface providesXX(XXImp imp) {
    return imp;
}

@Module
public class ProvidesModule {
  /*  形式一：自己构建Windable实例
     @Provides
    public Windable provideWindable(){
        return new WindableImpl();
    }*/

    /**
     * 形式二：让Dagger构建Windable实例，此时需要在WindableImpl构造函数上打上@Inject注解
     */
    @Provides
    public Windable provideWindable(WindableImpl windable){
        return windable;
    }
}
```

针对@Provides注解，编译期Dagger会生成格式为：
 Module类名_方法名Factory  的类并且实现了Factory接口。
 针对本例生成的类就是ProvidesModule_ProvideWindableFactory

```java
public final class ProvidesModule_ProvideWindableFactory implements Factory<Windable> {
  private final ProvidesModule module;

  private final Provider<WindableImpl> windableProvider;

  public ProvidesModule_ProvideWindableFactory(
      ProvidesModule module, Provider<WindableImpl> windableProvider) {
    this.module = module;
    this.windableProvider = windableProvider;
  }

  @Override
  public Windable get() {
    return provideInstance(module, windableProvider);
  }

  public static Windable provideInstance(
      ProvidesModule module, Provider<WindableImpl> windableProvider) {
    return proxyProvideWindable(module, windableProvider.get());
  }

  public static ProvidesModule_ProvideWindableFactory create(
      ProvidesModule module, Provider<WindableImpl> windableProvider) {
    return new ProvidesModule_ProvideWindableFactory(module, windableProvider);
  }

  public static Windable proxyProvideWindable(ProvidesModule instance, WindableImpl windable) {
    return Preconditions.checkNotNull(
        instance.provideWindable(windable),
        "Cannot return null from a non-@Nullable @Provides method");
  }
}
```

生成的类的格式也是相对固定的。
 一个静态create方法，返回值是该Factory类实例。
 一个构造函数。
 一个get方法，来自Factory接口，用于返回依赖。

**注意：每个用@Provides注解的方法都会生成相应的Factory类。**



**@Binds**也是用于提供依赖，跟@Provides注解功能一样。不同的是@Binds注解于**抽象方法**上，返回值是依赖的接口或父类型或本类型，**方法只有一个参数，就是返回值的实现者**。既然@Binds注解于抽象方法，那么@Module注解的类就必须是接口或抽象类了。

其实这样的代码可以通过`@Binds`简化为

```java
@Binds
public abstract XXInterface bindXX(XXImp imp);

@Module
public interface BindsModule {
    /**
     * 注解于抽象方法，只能有一个参数
     */
    @Binds
    Windable provideWindable(WindableImpl windable);
}
```

同时你需要将你的`Module`改为`abstract`即可,但是要注意这两个不能共存于一个Module



---

### @IntoSet

@IntoSet需要联合@Provides一起使用，@IntoSet注解的方法表示方法返回值放入一个Set集合中，多个@IntoSet注解的方法，若方法返回值类型一致，则会放入同一个Set集合中。

```java
@Module
public class IntoSetModule {

    @Provides
    @IntoSet
    public String provideA(){
        return "A";
    }

    @Provides
    @IntoSet
    public String provideB(){
        return "B";
    }
}

public class IntoSetDemo {

    @Inject
    Set<String> letters;
}
```

Module类中的provideA和provideB方法的返回值被放入同一个Set集合中，所以该例的输出结果是AB。



---

### @IntoMap

@IntoMap需要联合@IntKey,@StringKey或者自定义的@MapKey以及@Provides一起使用。如果map的key为int类型，则用@IntKey,为String类型，则用@StringKey，如果为其他类型，则需要自定义注解，并在自定义注解上打上@MapKey。

```java
@Module
public class IntoMapModule {

    @Provides
    @IntoMap
    @IntKey(1)
    public String provideNameA(){
        return "nameA";
    }

    @Provides
    @IntoMap
    @IntKey(2)
    public String provideNameB(){
        return "nameB";
    }
}

public class IntoMapDemo {

    @Inject
    Map<Integer,String> map;
}
```

IntoMapModule类中的provideNameA方法指定了key为1，value为nameA，provideNameB方法指定了key为2，value为nameB，他们被放入同一个map中。所以程序的输出结果是

```
key=1,value=nameA
key=2,value=nameB
```



---

### @Component

用于标注接口，是依赖需求方和依赖提供方之间的桥梁。被Component标注的接口在编译时会生成该接口的实现类（Dagger+Component名字）,我们通过调用这个实现类的方法完成注入；Component接口中主要定义一些提供依赖的声明

@Component 注解的类(一般是接口或抽象类)用于为具体的类注入依赖。

@Component 注解有两个属性：

- 一个是**modules**属性，关联相关的Module
- 一个是**dependencies**属性，关联**子**Component 

一般需要提供一个inject()方法，参数是需要注入的类；当然也可以定义一个直接获取依赖的方法，比如Activity类中需要注入一个Presenter对象，可以直接在Component接口中定义一个presenter方法，在Activity类中需要使用Presenter时使用component.presenter()就可以了。

- **inject法：**

```java
@Component(modules = MyModule.class)
public interface MyComponent {

    /**
     * 定义一个inject方法，注入ComponentDemo需要的依赖
     * @param componentDemo
     */
    void inject(ComponentDemo componentDemo);
}

public class ComponentDemo {

    @Inject
    MyService myService;

    ComponentDemo(){
    }

    public static void main(String args[]){
        ComponentDemo componentDemo=new ComponentDemo();
        //将MyService注入
        DaggerMyComponent.create().inject(componentDemo);
        componentDemo.myService.serve();
    }
}
```

MyModule提供了MyService实例。

- **Component提供一个myService()方法**

```java
@Component(modules = MyModule.class)
public interface MyComponent {
    //定义一个方法提供依赖
    MyService myService();
}
public class ComponentDemo {
    MyService myService;
    ComponentDemo(){
    }

    public static void main(String args[]){
        ComponentDemo componentDemo=new ComponentDemo(); 
        componentDemo.myService=DaggerMyComponent
                  .create()    
                  .myService();
        componentDemo.myService.serve();
    }
}
```

@Component注解在编译期生成的源码格式：
 1，实现@Component注解的接口
 2，静态内部类Builder，含有用于设置@Component注解中modules的方法
 3，生成的Component类名为@Component注解的接口的简单名称加前缀Dagger。
 4，一个接收builder的构造函数，保存相关的module。
 5，实现@Component注解的接口中定义的方法

```java
@Generated(
  value = "dagger.internal.codegen.ComponentProcessor",
  comments = "https://google.github.io/dagger"
)
public final class DaggerMyComponent implements MyComponent {
  private MyModule myModule;

  private DaggerMyComponent(Builder builder) {
    initialize(builder);
  }

  public static Builder builder() {
    return new Builder();
  }

  public static MyComponent create() {
    return new Builder().build();
  }

  @SuppressWarnings("unchecked")
  private void initialize(final Builder builder) {
    this.myModule = builder.myModule;
  }

  @Override
  public MyService myService() {
    return MyModule_ProvideMyServiceFactory.proxyProvideMyService(myModule);
  }

  public static final class Builder {
    private MyModule myModule;

    private Builder() {}

    public MyComponent build() {
      if (myModule == null) {
        this.myModule = new MyModule();
      }
      return new DaggerMyComponent(this);
    }

    public Builder myModule(MyModule myModule) {
      this.myModule = Preconditions.checkNotNull(myModule);
      return this;
    }
  }
}
```



---

### @Inject

- 当@Inject标注在**构造器**时，说明该类的**实例交由Dagger生成**，Dagger会在编译期间生成一个**XXX_Factory**类。
- 当@Inject标注在**字段上**时，Dagger会在编译期生成一个**XXX_MembersInjector**类。
- Dagger2提供3种方式：
  - 构造方法注入：在类的构造方法前面注释@Inject
  - 成员变量注入：在类的成员变量（非私有）前面注释@Inject
  - 函数方法注入：在函数前面注释@Inject

```java
public class MapKeyDemo {

    @Inject
    Map<Bar,Integer> map;

    private String param;
    @Inject
    public MapKeyDemo(String param){
        this.param=param;
    }

    public static void main(String [] args){
        MapKeyComponent mapKeyComponent=DaggerMapKeyComponent.create();
        MapKeyDemo mapKeyDemo=mapKeyComponent.mapKeyDemo();
        mapKeyComponent.inject(mapKeyDemo);
    }
}
```

**生成的类**

```java
@Generated(
  value = "dagger.internal.codegen.ComponentProcessor",
  comments = "https://google.github.io/dagger"
)
public final class MapKeyDemo_Factory implements Factory<MapKeyDemo> {
  private final Provider<String> paramProvider;

  private final Provider<Map<Bar, Integer>> mapProvider;

  public MapKeyDemo_Factory(
      Provider<String> paramProvider, Provider<Map<Bar, Integer>> mapProvider) {
    this.paramProvider = paramProvider;
    this.mapProvider = mapProvider;
  }

  @Override
  public MapKeyDemo get() {
    return provideInstance(paramProvider, mapProvider);
  }

  public static MapKeyDemo provideInstance(
      Provider<String> paramProvider, Provider<Map<Bar, Integer>> mapProvider) {
    MapKeyDemo instance = new MapKeyDemo(paramProvider.get());
    MapKeyDemo_MembersInjector.injectMap(instance, mapProvider.get());
    return instance;
  }

  public static MapKeyDemo_Factory create(
      Provider<String> paramProvider, Provider<Map<Bar, Integer>> mapProvider) {
    return new MapKeyDemo_Factory(paramProvider, mapProvider);
  }

  public static MapKeyDemo newMapKeyDemo(String param) {
    return new MapKeyDemo(param);
  }
}

@Generated(
  value = "dagger.internal.codegen.ComponentProcessor",
  comments = "https://google.github.io/dagger"
)
public final class MapKeyDemo_MembersInjector implements MembersInjector<MapKeyDemo> {
  private final Provider<Map<Bar, Integer>> mapProvider;

  public MapKeyDemo_MembersInjector(Provider<Map<Bar, Integer>> mapProvider) {
    this.mapProvider = mapProvider;
  }

  public static MembersInjector<MapKeyDemo> create(Provider<Map<Bar, Integer>> mapProvider) {
    return new MapKeyDemo_MembersInjector(mapProvider);
  }

  @Override
  public void injectMembers(MapKeyDemo instance) {
    injectMap(instance, mapProvider.get());
  }

  public static void injectMap(MapKeyDemo instance, Map<Bar, Integer> map) {
    instance.map = map;
  }
}
```



---

### @Subcomponent和dependency

- <https://www.jianshu.com/p/24af4c102f62>

dependency

Component可以依赖于其他Component，可以使用@Component的dependence，也可以使用@SubComponent，这样就可以获取其他Component的依赖了。

```java
public class ComponentDependency {
    @Component(modules = ModuleA.class)
    public interface ComponentA {
        SomeClassA1 someClassA1();
    }

    @Component(modules = ModuleB.class, dependencies = ComponentA.class)
    public interface ComponentB {
        SomeClassB1 someClassB1();
    }

    public static void main(String[] args) {
        ModuleA moduleA = new ModuleA();
        ComponentA componentA = DaggerComponentDependency_ComponentA.builder()
                .moduleA(moduleA)
                .build();

        ModuleB moduleB = new ModuleB();
        ComponentB componentB = DaggerComponentDependency_ComponentB.builder()
                .moduleB(moduleB)
                .componentA(componentA)
                .build();
    }
}
```

Subcomponent

```java
public class SubComponent {
    @Component(modules = ModuleA.class)
    public interface ComponentA {
        ComponentB componentB(ModuleB moduleB);
    }

    @Subcomponent(modules = ModuleB.class)
    public interface ComponentB {
        SomeClassB1 someClassB1();
    }

    public static void main(String[] args) {
        ModuleA moduleA = new ModuleA();
        ComponentA componentA = DaggerSubComponent_ComponentA.builder()
                .moduleA(moduleA)
                .build();

        ModuleB moduleB = new ModuleB();
        ComponentB componentB = componentA.componentB(moduleB);
    }
}
```



```java
public abstract class Flower {
    public abstract String whisper();
}
public class Lily extends Flower {

    @Override
    public String whisper() {
        return "纯洁";
    }
}
public class Rose extends Flower {

    public String whisper()  {
        return "热恋";
    }
}
@Module
public class FlowerModule {

    @Provides
    @RoseFlower
    Flower provideRose() {
        return new Rose();
    }

    @Provides
    @LilyFlower
    Flower provideLily() {
        return new Lily();
    }
}
```

Component上也需要指定@Qualifier

```java
@Component(modules = FlowerModule.class)
public interface FlowerComponent {
    @RoseFlower
    Flower getRoseFlower();

    @LilyFlower
    Flower getLilyFlower();
}
public class Pot {

    private Flower flower;

    public Pot(Flower flower) {
        this.flower = flower;
    }

    public String show() {
        return flower.whisper();
    }
}
```

PotModule需要依赖Flower，需要指定其中一个子类实现，这里使用RoseFlower

```java
@Module
public class PotModule {

    @Provides
    Pot providePot(@RoseFlower Flower flower) {
        return new Pot(flower);
    }
}
@Component(modules = PotModule.class,dependencies = FlowerComponent.class)
public interface PotComponent {
    Pot getPot();
}
@Component(dependencies = PotComponent.class)
public interface MainActivityComponent {
    void inject(MainActivity activity);
}
```

而在MainActivity则需要创建其依赖的Component

```java
public class MainActivity extends AppCompatActivity {

    @Inject
    Pot pot;

    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        DaggerMainActivityComponent.builder()
                .potComponent(DaggerPotComponent.builder()
                        .flowerComponent(DaggerFlowerComponent.create())
                        .build())
                .build().inject(this);

        String show = pot.show();
        Toast.makeText(MainActivity.this, show, Toast.LENGTH_SHORT).show();
    }
}
```

这就是Component的dependencies的用法了，我们Component不需要重复的指定Module，可以直接依赖其它Component获得。

分析下源码，看下Component的dependencies做了什么事情。

```java
public final class DaggerPotComponent implements PotComponent {
  private Provider<Flower> getRoseFlowerProvider;

  private Provider<Pot> providePotProvider;

  private DaggerPotComponent(Builder builder) {
    assert builder != null;
    initialize(builder);
  }

  public static Builder builder() {
    return new Builder();
  }

  @SuppressWarnings("unchecked")
  private void initialize(final Builder builder) {

    this.getRoseFlowerProvider =
        new Factory<Flower>() {
          private final FlowerComponent flowerComponent = builder.flowerComponent;

          @Override
          public Flower get() {
            return Preconditions.checkNotNull(
                flowerComponent.getRoseFlower(),
                "Cannot return null from a non-@Nullable component method");
          }
        };

    this.providePotProvider =
        PotModule_ProvidePotFactory.create(builder.potModule, getRoseFlowerProvider);
  }

  @Override
  public Pot getPot() {
    return providePotProvider.get();
  }

  public static final class Builder {
    private PotModule potModule;

    private FlowerComponent flowerComponent;

    private Builder() {}

    public PotComponent build() {
      if (potModule == null) {
        this.potModule = new PotModule();
      }
      if (flowerComponent == null) {
        throw new IllegalStateException(FlowerComponent.class.getCanonicalName() + " must be set");
      }
      return new DaggerPotComponent(this);
    }

    public Builder potModule(PotModule potModule) {
      this.potModule = Preconditions.checkNotNull(potModule);
      return this;
    }

    public Builder flowerComponent(FlowerComponent flowerComponent) {
      this.flowerComponent = Preconditions.checkNotNull(flowerComponent);
      return this;
    }
  }
}
```

PotComponent依赖FlowerComponent，其实就是将FlowerComponent的引用传递给PotComponent，这样PotComponent就可以使用FlowerComponent中的方法了。
 注意看getRoseFlowerProvider这个Provider，是从 `flowerComponent.getRoseFlower()`获取到的

------

如果使用Subcomponent的话则是这么写， 其他类不需要改变，只修改Component即可

```java
@Component(modules = FlowerModule.class)
public interface FlowerComponent {
    
    PotComponent plus(PotModule potModule);
}
@Subcomponent(modules = PotModule.class)
public interface PotComponent {
    MainActivityComponent plus();
}
@Subcomponent
public interface MainActivityComponent {
    void inject(MainActivity activity);
}
public class MainActivity extends AppCompatActivity {

    @Inject
    Pot pot;

    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        DaggerFlowerComponent.create()
                .plus(new PotModule())  // 这个方法返回PotComponent
                .plus()                 // 这个方法返回MainActivityComponent
                .inject(this);

        String show = pot.show();
        Toast.makeText(MainActivity.this, show, Toast.LENGTH_SHORT).show();
    }
}
```

FlowerComponent管理了PotComponent和MainActivityComponent，看起来不符合常理。

先来说说Component中的方法的第三种定义方式（上面说了两种）。

```java
@Component
class AComponpent {
    XxxComponent plus(Module... modules)
}
@Subcomponent(modules = xxxxx)
class XxxComponent {
    
}
```

xxxComponent是该AComponpent的依赖，被@Subcomponent标注。
 而modules参数则是xxxComponent指定的Module。
 在重新编译后，Dagger2生成的代码中，Subcomponent标记的类是Componpent的内部类。
 像上面的Demo，MainActivityComponent是PotComponent的内部类，而PotComponent又是FlowerComponent的内部类。

------

但是用Subcomponent怎么看怎么别扭，各个Component之间联系太紧密，不太适合我们Demo的使用场景。
 **那什么时候该用@Subcomponent呢？**
 Subcomponent是作为Component的拓展的时候。
 像我写的Demo中，Pot和Flower还有MainActivity只是单纯的依赖关系。就算有，也只能是Flower作为Pot的Subcomponent，而不是Demo中所示，因为我需要给大家展示Dagger的API，强行使用。

**比较适合使用Subcomponent的几个场景：**
 很多工具类都需要使用到Application的Context对象，此时就可以用一个Component负责提供，我们可以命名为AppComponent。
 需要用到的context对象的SharePreferenceComponent，ToastComponent就可以它作为Subcomponent存在了。

而且在AppComponent中，我们可以很清晰的看到有哪些子Component，因为在里面我们定义了很多`XxxComponent plus(Module... modules)`

每个ActivityComponent也是可以作为AppComponent的Subcomponent，这样可以更方便的进行依赖注入，减少重复代码。

**Component dependencies和Subcomponent区别**

1. Component dependencies 能单独使用，而Subcomponent必须由Component调用方法获取。
2. Component dependencies 可以很清楚的得知他依赖哪个Component， 而Subcomponent不知道它自己的谁的孩子……真可怜
3. 使用上的区别，Subcomponent就像这样`DaggerAppComponent.plus(new SharePreferenceModule());`
    使用Dependence可能是这样`DaggerAppComponent.sharePreferenceComponent(SharePreferenceComponent.create())` 

**Component dependencies和Subcomponent使用上的总结**

Component Dependencies：

1. 你想保留独立的想个组件（Flower可以单独使用注入，Pot也可以）
2. 要明确的显示该组件所使用的其他依赖

Subcomponent：

1. 两个组件之间的关系紧密
2. 你只关心Component，而Subcomponent只是作为Component的拓展，可以通过Component.xxx调用。

[Dagger 2 subcomponents vs component dependencies](https://link.jianshu.com?t=http://stackoverflow.com/questions/29587130/dagger-2-subcomponents-vs-component-dependencies)



----

### Subcomponent

与依赖不同,`Subcomponent`拥有主`Component`所有注入对象,也就是说`Subcomponent`可以注入更多的对象, 通过生成代码也可以看出, 它的实现是主`Component`的内部类.

Cat

```java
@Subcomponent(modules = {CatModule.class})
public interface CatComponent {
    Cat getCat();
}
```

CatModule

```java
@Module
public class CatModule {
    @Provides
    public Cat providesCat(Leg leg//Animal Component提供) {
        return Cat(leg);
    }
}
```

我们还必须在AnimalComponent显示提供CatComponent,因为如上所述,Cat是Animal的内部类了.

```java
@Component(
        dependencies = FoodComponent.class,
        modules = AnimalModule.class
)
public interface AnimalComponent {
    Animal getAnimal();
    //这句是关联二者的代码
    CatComponent createCatComponent();
}
```

这样我们就可以通过下面的办法来实现`Cat`的注入:

```java
DaggerAnimalComponent
		.build()
    	.createCatComponent()
    	.getCat();
```

### Subcomponent with explicit builder

当我们`AnimalComponent`需要对Cat进行修改再输出的话(如指定猫的名字),可能就需要为`CatComponent`提供`Builder`

```java
@Subcomponent(modules = {CatModule.class})
public interface CatComponent {
    @Subcomponent.Builder
    interface Builder {
        @BindsInstance 
        Builder name(String name);
        CatComponent build();
    }
}
```

然后我们需要在`AnimalModule`里面使用这个`Builder`了

```java
@Module(subcomponents = CatComponent.class)//注意这里需要加上这一条声明
class AnimalModule {
    @Provides
    public Animal providesAnimal(Food food) {
        //Animal需要另外一个Component提供的Food来创建
        return new Animal(food);
    }
    @Provides
    public CatComponent providesCatComponent(CatComponent.Builder builder) {
        //这里只是举个例子,可能这里的Cat构造依赖于Animal的另外属性
        return builder
            .name("喵喵")
            .build();
    }
}
```

-----

### Provider与Lazy

大家想必会使用过`Lazy`,很多语言都有`Lazy`,如最近大红大紫的`Kotlin`就可以通过`by lazy {}`来实现对象的延迟加载.

- 没错,`Lazy<T>`也是如此,只有当你调用`get()`时,才会真正注入.

- `Provider<T>`与之的区别在于,`Lazy`延迟加载之后**每次的调用都是同一个对象**,而`Provider`则要看注入对象的实现,如果是通过`@Scope`**约束的对象**,则是同一个,**否则每次都会创建新的**.



---

### @Scope和@Singleton

- **你需要什么对象全局单例，就在提供该对象方法的@Provides注解旁加上一个@Singleton,并且在该module关联的Component中加上同样的注解**：

@Scope同样用于自定义注解，我能可以通过@Scope自定义的注解来限定注解作用域，实现局部的单例；比如我们前面使用到的@ActivityScope：

```java
@Scope
@Retention(RUNTIME)
public @interface ActivityScope {}
```

如果需要提供局部单例支持，则需要在Component中和**@provides注解的方法上@ActivityScope**，这里说的局部单例的意思是在该Component中是唯一的，如果Component是全局唯一的话就是全局单例了，比如AppComponent。

@Scope的作用只是保证依赖在@Component中是唯一的，可以理解为“局部单例”。
 **@Scope是需要成对存在的，在Module的Provide方法中使用了@Scope，那么对应的Component中也必须使用@Scope注解，当两边的@Scope名字一样时（比如同为@Singleton）, 那么该Provide方法提供的依赖将会在Component中保持“局部单例”。 而在Component中标注@Scope，provide方法没有标注，那么这个Scope就不会起作用，而Component上的Scope的作用也只是为了能顺利通过编译，就像我刚刚定义的ActivityScope一样。**

@Singleton也是一个自定义@Scope，它的作用就像上面说的一样。但由于它是Dagger2中默认定义的，所以它比我们自定义Scope对了一个功能，就是编译检测，防止我们不规范的使用Scope注解，仅此而已。

- **原理**

**对比DaggerXXXXComponent.java，在Provider初始化的地方**

未添加@ActivityScope:

```java
@SuppressWarnings("unchecked")
  private void initialize(final Builder builder) {
    //注意这行代码
    this.provideStudentProvider = A03Module_ProvideStudentFactory.create(builder.a03Module);

    this.a03ActivityMembersInjector = A03Activity_MembersInjector.create(provideStudentProvider);
  }
```

添加了@ActivityScope:

```java
@SuppressWarnings("unchecked")
  private void initialize(final Builder builder) {
   //注意这行代码，多了一个DoubleCheck.provider()方法
    this.provideStudentProvider =
        DoubleCheck.provider(A03Module_ProvideStudentFactory.create(builder.a03Module));

    this.a03ActivityMembersInjector = A03Activity_MembersInjector.create(provideStudentProvider);
  }
```

```java
1.未添加自定义@Scope：每次Activity初始化对象，直接让工厂类初始化一个对象给Activty（activity.student = new Student()）;

2.添加了自定义@Scope：每次Activity初始化对象，直接让工厂类将单例对象给Activty（ 
activity.student1 = factory.student(单例)； 
activity.student2 = factory.student(单例)；）
```





### @Qualifier和@Named

@Qualifier是限定符，而@Named则是基于String的限定符。

当我有两个相同的依赖（都继承某一个父类或者都是先某一个接口）可以提供给高层时，那么程序就不知道我们到底要提供哪一个依赖，因为它找到了两个。
这时候我们就可以通过限定符为两个依赖分别打上标记，指定提供某个依赖。

@Qualifier的作用和@Named是完全一样的，不过更推荐使用@Qualifier，因为@Named需要手写字符串，容易出错。

```java
@Qualifier
@Documented
@Retention(RUNTIME)
public @interface Named {
  String value() default "";
}

class ExpensiveCoffeeMaker {
  @Inject @Named("water") Heater waterHeater;
  @Inject @Named("hot plate") Heater hotPlateHeater;
  ...
}

@Provides @Named("hot plate") static Heater provideHotPlateHeater() {
  return new ElectricHeater(70);
}

@Provides @Named("water") static Heater provideWaterHeater() {
  return new ElectricHeater(93);
}
```



----

### @ContributesAndroidInjector

@ContributesAndroidInjector来标记哪个类需要使用依赖注入功能，这里标记的是ManActivity，所以MainActivity能通过@Inject注解来注入相应对象。这个注解是Dagger2 For Android简化代码的关键

**ActivityModules**

```java
@Module
abstract class ActivityModules {

    @ContributesAndroidInjector
    abstract MainActivity contributeMainActivity();
    
    //如果需要拓展，在这里继续扩展相应的Activity即可
}
```



### @Bind、@BindInstance、@Multibinds

- **@Bind**像一个桥梁，它仅作用于提供类型的声明。它可以将某个父类型的需求，嫁接到其子类型的实现上，从而避免子类型的暴露，使调用代码依赖于抽象类型，而不是具体的子类。我们用示例来说明这个作用。

  代码如上面的@Binds与@Providers

- **@BindInstance**注解像是一个管道，它允许我们在Component里，提前为某个类型对象打一个桩，并在构造Component实例的时候，用**该类型或子类型的一个对象，替换（赋值）这个桩**，以便其他地方获取。通过@BindInstance打出来的桩，在构造对象的时候必须进行非空赋值，否则会导致异常。Dagger2将它设计为一个强制绑定关系，仅供必要时使用。

  以下来看示例代码：

  ```java
  @Component(modules = {EngineModule.class})
  public interface EngineComponent {
  
      String getTag();
  
      CarComponent.Builder carComponent();
  
      @Component.Builder
      interface Builder{
  
          @BindsInstance
          Builder tag(String tag);
  
          Builder engineModule(EngineModule engineModule);
  
          EngineComponent build();
      }
  }
  ```

  在上面的EngineComponent里，我们定义了String类型的getTag方法，这个方法返回的对象，是在Builder中tag方法传递进去的。如果我们不在Builder中定义tag方法，那么Component就会在Module中进行查找，导致编译失败。所以为了定义这个tag方法，我们需要向上面一样，使用Component.Builder注解，手动实现我们的Builder类，而这本来是可以省略的。同样，如果我们在Module中有相同的提供类型，就会因类型冲突导致报重复绑定错误，所以这也是一个基于类型的强制绑定。

- **@MultiBinds**

  在介绍MultiBinds注解之前，我们要先了解这样的一种需求，假如我们需要在Module里提供了很多相同类型的 对象，如果我们不使用Qualifer，就会导致同一类型重复绑定的错误。但是如果我们确实需要在一个Module里包含这些对象的创建，**又不想创建N多的Qualifer**，我们就可以使用MultiBind机制来达到我们的目的。

  MultiBind机制**允许我们为这些对象创建一个集合**，这个集合必须是Set或者Map，这样在Component中，我们就可以暴露这个集合，通过集合来获取不同的对象。这个集合的创建有三种方法，我们用简短的代码例子说明一下。

  1. **使用@IntoSet或者@IntoMap**

  ```java
  @Module(subcomponents = CarComponent.class)
  public class EngineModule {
  
      @Provides
      @IntoSet
      Engine provideEngineIntoSet2(){
          return new Engine(2);
      }
  
      @Provides
      @IntoMap
      @StringKey("3")
      Engine provideEngineIntoMap3(){
          return new Engine(3);
      }
  }
  ```

  这种方声明使用集合，我们的提供方法返回的还是原来的类型，但是Dagger2会根据注解，将它转换为Set或者Map类型，并调用这些方法创建对象，将这些对象（与索引绑定之后，如果是Map类型的话）放入集合中。

  2. **直接提供Set或者Map类型**

  ```java
  @Module(subcomponents = CarComponent.class)
  public class EngineModule {
  
      @Provides
      Set<Engine> provideEngineSet(){
          Set<Engine> engines =  new HashSet<>();
          engines .add(new Engine(9));
          return engines;
      }
  
      @Provides
      Map<String, Engine> provideEngineMap(){
          Map<String,Engine> engineMap =  new HashMap<>();
          engineMap.put("engine10", new Engine(10));
          return engineMap;
      }
  }
  ```

  直接提供Set和Map就比较直观了，但是这种方式和使用前面一种是会冲突的，Dagger2允许使用@ElementIntoSet注解，将自定义的set元素添加到自动生成的Set中，但是Map是不能混用的，放在不同的Module中也不行。

  3. **使用@MultiBinds注解**

  ```java
  @Module(subcomponents = CarComponent.class)
  public abstract class AbsEngineModule {
  
      @Multibinds
      abstract Set<Engine> engineSet();
  
      @Multibinds
      abstract Map<String, Engine> engineMap();
  }
  ```

  MultiBinds**只能用于标注抽象方法**，它仅仅是告诉Component我有这么一种提供类型，让我们Component可以在Component中暴露Set或者Map类型的接口，但是不能包含具体的元素。Multibinds注解是可以和第一种集合定义混用的。

  我在学习MultiBinds注解的时候，非常的郁闷。既然MultiBinds用于提供集合，为什么只能是空集？我要往里面添加元素，还得用@IntoSet和@IntoMap标注提供方法，而它们是可以独立工作的，MultiBinds的在这儿的作用不是等于0么？

  是的，如果我们已知有哪些元素可以使用，完全没有必要用MultiBinds标签的，这个标签的作用，就是在不知道有哪些元素可以使用，但是我在Component里要暴露集合集合，以便其他地方可以调用。这在dagger-android框架里体现的就非常明显。它在框架中有这么一个类

  ```java
  @Module
  public abstract class AndroidInjectionModule {
    @Multibinds
    abstract Map<Class<? extends Activity>, AndroidInjector.Factory<? extends Activity>>
        activityInjectorFactories();
  
    @Multibinds
    abstract Map<Class<? extends Fragment>, AndroidInjector.Factory<? extends Fragment>>
        fragmentInjectorFactories();
  
    @Multibinds
    abstract Map<Class<? extends Service>, AndroidInjector.Factory<? extends Service>>
        serviceInjectorFactories();
  
    @Multibinds
    abstract Map<
            Class<? extends BroadcastReceiver>, AndroidInjector.Factory<? extends BroadcastReceiver>>
        broadcastReceiverInjectorFactories();
  
    @Multibinds
    abstract Map<
            Class<? extends ContentProvider>, AndroidInjector.Factory<? extends ContentProvider>>
        contentProviderInjectorFactories();
  
    private AndroidInjectionModule() {}
  }
  ```

  这个Module里使用@Multibinds定义了四大组件加Fragment的AndroidInjector.Factory的集合，就是为了创建  **DispatchingAndroidInjector\<T>**这个对象，我们看下它的构造方法:

  ```java
  DispatchingAndroidInjector(
        Map<Class<? extends T>, Provider<AndroidInjector.Factory<? extends T>>> injectorFactories) {
      this.injectorFactories = injectorFactories;
    }
  ```

  **然后这个对象进行代理注入：**

  ```java
    public boolean maybeInject(T instance) {
      Provider<AndroidInjector.Factory<? extends T>> factoryProvider =
          injectorFactories.get(instance.getClass());
      if (factoryProvider == null) {
        return false;
      }
      //此处省略N行代码
    }
  ```

  可以看到，它就是使用T的Class作为索引，从集合中取出对应的AndroidInject.Fractory对象进行工作的。而这其中的T，就是我们在App自己定义的各种组件，在dagger-android框架里，是不知道有哪些对象可以使用的。

  **总得来说，Multibinds注解的作用，就是为了Component提供依赖类型的完整性，这在编写一些框架给外部使用的时候，会起到关键的作用，其他时候，是可以使用@IntoXX标签替代的**

  现在我们还是继续看一下使用集合之后，Component里的写法:

  ```java
  @Component(modules = {EngineModule.class, AbsEngineModule.class})
  public interface EngineComponent {
  
      String getTag();
  
      Set<Engine> engineSet();
  
      Map<String, Engine> engineMap();
  
      CarComponent.Builder carComponent();
  
      @Component.Builder
      interface Builder{
  
          @BindsInstance
          Builder tag(String tag);
  
          Builder engineModule(EngineModule engineModule);
  
          EngineComponent build();
      }
  }
  ```

  **我们再看看关键的实现源码：**

  ```java
    @Override
    public Set<Engine> engineSet() {
      return ImmutableSet.<Engine>of(
          EngineModule_ProvideEngineIntoSet2Factory.proxyProvideEngineIntoSet2(engineModule));
    }
  
    @Override
    public Map<String, Engine> engineMap() {
      return ImmutableMap.<String, Engine>of(
          "5", EngineModule_ProvideEngineIntoMap3Factory.proxyProvideEngineIntoMap3(engineModule));
    }
  ```

  我们在调用engineSet或者engineMap方法，获取对应集合的时候，Dagger直接通过Module实例创建我们要的对象，然后放到ImmutableXX里(这是个对象固定的集合)，也就是说通过第一和第三种方式暴露的集合，对象引用是不可以替换的。使用第二种的，和我们正常暴露的普通类型是一样的，大家可以自己试验一下。

  

----

## Dagger2API

```java
public @interface Component {
    Class<?>[] modules() default {};
    Class<?>[] dependencies() default {};
}

public @interface Subcomponent {
    Class<?>[] modules() default {};
}

public @interface Module {
    Class<?>[] includes() default {};
}

public @interface Provides {
}

public @interface MapKey {
    boolean unwrapValue() default true;
}

public interface Lazy<T> {
    T get();
}

//还有在Dagger 2中用到的定义在 [JSR-330](https://link.jianshu.com/?
//t=https://jcp.org/en/jsr/detail?id=330) （Java中依赖注入的标准）中的其它元素：
public @interface Inject {
}

public @interface Scope {
}

public @interface Qualifier {
}
```



---

### 链接

- [注解介绍](https://www.jianshu.com/p/4b6a5c0681ec)
- [Dagger2进阶](https://www.cnblogs.com/mengdd/p/advanced-dagger2-skills.html)
- [依赖接力](https://www.jianshu.com/nb/32816502)
- [官方文档](https://google.github.io/dagger/users-guide)
- [使用Dagger2前你必须了解的一些设计原则](https://www.jianshu.com/p/cc1427e385b5)
- [系列--使用Dagger 2依赖注入 - DI介绍（翻译）](https://www.cnblogs.com/tiantianbyconan/p/5092083.html)
- [Dagger2 最清晰的使用教程](https://www.jianshu.com/p/24af4c102f62)
- [@Component.Builder和@BindsInstance介绍](https://juejin.im/post/5a4cf2b2f265da430d586ace)
- [@ContributesAndroidInjector](https://blog.csdn.net/qq_17766199/article/details/73030696)
- [泡在网上的日子](http://www.jcodecraeer.com/a/anzhuokaifa/androidkaifa/2016/0505/4212.html)

- <https://www.cnblogs.com/tiantianbyconan/p/6266442.html>

- [注解](https://www.jianshu.com/p/f875cc7f7305)