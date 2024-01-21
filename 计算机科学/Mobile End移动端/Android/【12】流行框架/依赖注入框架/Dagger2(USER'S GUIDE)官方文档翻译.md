[TOC]

# Dagger2用户指南

应用中最好的类是那些真正做事情的类。比如：`BarcodeDecoder` (条码解码器)，`KoopaPhysicsEngine` (koopa 物理引擎) 和 `AudioStreamer` (音频流)。这些类会可能会依赖于其他类。
 比如：`BarcodeCameraFinder` (条形码相机取景器)，`DefaultPhysicsEngine` (默认的物理引擎) 或者 `HttpStreamer`。

与之相对的，应用中最差的类，是那些占用空间却不做事情的。
 比如：`BarcodeDecoderFactory` (条码解码器工厂)、`CameraServiceLoader` (相机服务加载器) 和 `MutableContextWrapper` (不定上下文包装器)。这些类并没有做一些实际的事情，只是起到将有用的类链接起来的作用。

Dagger是这些什么什么工厂类之类的替代者，Dagger实现了依赖注入的设计模式，却不用关心使用依赖注入所需要的各种模板（Dagger框架已经做好了这一步）。这样我们就可以只需要将目光聚焦在真正做事情的类上。声明这些类需要的依赖，然后为这些类提供依赖，接下来就可以运行程序了。

通过构建一个标准的Java注解，就可以很方便的测试类，不需要通过一堆样板将 `RpcCreditCardService` 转换成 `FakeCreditCardService`。

依赖注入不仅仅是为了测试，它使得创建高复用性，高可替换性的模块变得更容易。这样就可以在不同的应用之间共享同一个 `AuthenticationModule` 。(译者附加：只要一个类满足单一职责原则，那么这个类就可以在不同地方多次使用，如果这个类还满足接口隔离原则与依赖倒置原则，那么在使用该类的地方，就可以方便的替换成另一个类。) 这样你就可以在开发环境中使用 `DevLoggingModule` ，在生产环境中使用 `ProdLoggingModule`，根据不同的环境使用不同的类来完成正确的事情。

## 为什么Dagger2是与众不同的

[依赖注入](http://en.wikipedia.org/wiki/Dependency_injection)框架已经存在了很多年，可以给各种各样的API提供配置和注入功能。那么为什么重新发明轮子（我对画轮子的理解：对已经存在的实现某种功能的框架，根据其能完成的功能，重新创建/设计框架）？Dagger2是第一个通过生成的代码实现完整的堆栈的框架。原理是生成一些模仿用户可能会手写的代码，使得依赖注入变得简单、可追踪、高效。有关更多设计的背景，查看[Gregory Kick](https://google.com/+GregoryKick/) 的[演讲](https://youtu.be/oK_XtfXPkqw)（[幻灯片](https://docs.google.com/presentation/d/1fby5VeGU9CN8zjw4lAb2QPPsKRxx6mSwCe9q7ECNSJQ/pub?start=false&loop=false&delayms=3000)）

## 使用Dagger2

我们通过构建一个咖啡机来演示依赖注入与Dagger。有关可以编译与运行的完整示例代码，查看 [咖啡示例](https://github.com/google/dagger/tree/master/examples/simple/src/main/java/coffee)，或者[直接下载](https://download.csdn.net/download/android_zyf/10521081)

### 声明依赖

Dagger 为我们应用程序中的类构建实例，并且满足这些实例的依赖（为这些实例提供依赖）。使用[javax.inject.Inject](http://docs.oracle.com/javaee/7/api/javax/inject/Inject.html) 注解来标识它感兴趣的构造器和字段。（使用 `@Inject` 注解标识构造方法或成员变量）。

使用 `@Inject` 注解标注需要使用Dagger来创建实例的类的构造方法。当需要创建一个新的实例时，Dagger会获得需求的参数值，并且调用这个构造方法。（构造方法中的参数，Dagger获得后会自动传入）。

```java
public class Thermosiphon implements Pump {
    private final Heater heater;

    /**
     * 使用 @Inject 注解标注了构造方法
     * @param heater 参数Dagger获得后会自动传入
     */
    @Inject
    public Thermosiphon(Heater heater) {
        this.heater = heater;
    }
}
```

Dagger 能够准确的注入属性。在下面的例子中，它获得一个 `Heater` 实例并赋值给 `heater` 成员变量，也会获得一个 `Pump` 实例赋值给 `pump` 成员变量。

```java
public class CoffeeMaker {

    /**
     * 使用@Inject注解标注了属性
     */
    @Inject 
    Heater heater;

    @Inject
    Pump pump; 
}
```

如果你的类有 `@Inject` 注解属性，但是没有使用 `@Inject` 注解构造方法，如果有需求的话，Dagger会注入这些属性，但是不会创建新的对象（暂时没懂）。如果想让Dagger能够创建对象，可以添加一个被 `@Inject` 注解标注的无参数的构造方法。

Dagger 也支持方法注入，尽管构造方法注入或属性注入是典型的首选。

没有 `@Inject` 的类不能通过Dagger构建对象。

### 满足依赖（上面是声明，这里就是注入了）

默认情况下，Dagger通过构造一个如上所述的请求类型的实例来满足每个依赖关系。当你需要一个 `CoffeeMaker` ，Dagger 会调用 `new CoffeeMaker()` 并且设置(set) `CoffeeMaker` 的可注入字段，来获得一个实例。

但是 `@Inject` 不是什么地方都能用。

- 接口不能被构造。（这个应该都ok）
- 第三方的类不能被注解。
- 必须配置可配置对象。（啥意思？）

对于 `@Inject` 不足或者笨拙的情况（上述三种示例），使用 `@Provides` 注解标注方法来提供依赖（满足依赖）。方法的返回值类型定义了它满足的依赖关系。（比如类A中有个属性B需要一个方法来提供依赖，这个方法的返回值就应该是B的类型）

例如， 当需要一个 `Heater` 对象时，那么就会调用 `provideHeater()`

```java
@Module(includes = PumpModule.class)
public class DripCoffeeModule {
    @Provides static Heater provideHeater(){
        return new ElectricHeater();
    }
}
```

`@Provides` 标注的方法也可能存在自己的依赖关系，如果需要 `Pump` 对象，就按下面代码写。

```java
@Provides static Pump providePump(Thermosiphon pump) {
  return pump;
}
```

java所有的 `@Provides` 方法必须属于一个module，这个module就是一个被 `@Module` 注解标注的类。

```java
/**
 * 一个普通的类，被 @Module 修饰，就变成了 Dagger框架中的 module
 */
@Module
class DripCoffeeModule {
  @Provides 
  @Singleton
  static Heater provideHeater() {
    return new ElectricHeater();
  }

  @Provides 
  static Pump providePump(Thermosiphon pump) {
    return pump;
  }
}
```

按照惯例，`@Provides` 修饰的方法的方法名，以 `provide` 为前缀。 `@Module` 修饰的类的类名，以 `Module` 为后缀。

### 构建图（一直在说对象图，终于能看到到底是啥了，是数据结构中的图吗？）

`@Inject` 注解和 `@Provides` 注解标注的类会组成一个对象图，类与类之间通过依赖关系关联彼此。调用类似于（应用程序的 `main` 方法或Android的 `Application` 都是程序的入口）的代码，通过明确定义的根集合来访问该图形（对象图）（根集合是什么？）。在Dagger2中，这个集合由一个接口定义，该接口中的方法没有参数，并且返回值为我们所需的类型（也就是所需的依赖的类型）。通过将 `@Component` 注解标注于此类接口，并将模块类型(module的类型)传递给modules参数(是 `@Component` 注解的参数)，Dagger2就能够完整的帮助我们构建根集合。

```java
@Component(modules = DripCoffeeModule.class)
interface CoffeeShop {
  CoffeeMaker maker();
}
```

生成的实现类具有同样的格式的名字，以 `Dagger` 为前缀，后面拼接上该接口的接口名。通过调用生成类的 `builder()` 方法，可以获得一个 `builder` 实例，使用获得的这个 `builder` 实例可以设置依赖，然后调用 `build()` 方法创建一个新的实例(这个才是我们真正想要的依赖)。

```java
CoffeeShop coffeeShop = 
    DaggerCoffeeShop.builder()//建造者模式？获得 builder
    .dripCoffeeModule(new DripCoffeeModule())//调用builder的方法设置依赖
    .build();//调用build方法完成构建
```

*注意*：如果你的 `@Component` 标注的类，不是最顶级的类（就是内部类呗），那么生成的 `component` 组件的名称会包含该类的外部类的名称，通过下划线拼接将它们拼接起来。示例如下：

```java
class Foo {
  static class Bar {
    @Component
    interface BazComponent {}
  }
}
```

会生成一个叫做 `DaggerFoo_Bar_BazComponent` 的 `component` 组件。

任何具有可访问的默认构造方法的 `module` 组件都可以被省略，因为如果没有设置(不将 `module` 对象设置到 `builder`里)，`builder` (建造器)将自动构建实例。如果 `module` 模块中，被 `@Provides` 注解标注的方法都是静态的，那么 `@Component` 注解标注的 `component` 组件(是一个接口)的实现不需要创建实例。如果所有的依赖关系，都可以在不创建依赖实例的情况下构建，那么(Dagger)生成的实现( `component` 接口的实现)也会有一个 `create()` 方法，通过该方法可以获得一个新的实例，而略过 `builder` (不需要使用 `builder`)。

```java
//DaggerCoffeeShop就是Dagger2为我们生成的 component 组件的实现类
//正常情况下，是需要调用 builder() 然后将 module 都设置到 建造器中的(builder)
//但是上面所述情况就可以跳过 builder ，跳过设置 module 的步骤，直接使用 create() 方法即可
CoffeeShop coffeeShop = DaggerCoffeeShop.create();
```

`CoffeeShop` 是 `CoffeeApp` 类的一个内部接口，被 `@Component` 注解标注。这样，CoffeeApp类，就可以简单的使用Dagger为我们生成的 `CoffeeShop` 的实现类对象来获得完全注入的 `CoffeeMaker` 。

```java
public class CoffeeApp {
    @Singleton
    @Component(modules = { DripCoffeeModule.class })
    public interface CoffeeShop {
        CoffeeMaker maker();
    }

    public static void main(String[] args) {
        CoffeeShop coffeeShop = DaggerCoffeeApp_CoffeeShop.create();
        //maker()后会得到CoffeeMaker对象，然后调用CoffeeMaker对象的brew()方法
        coffeeShop.maker().brew();
    }
}
```

现在已经构建好了对象图，也将切入点注入（就是注入了依赖），运行程序如下所示：

```java
~ ~ ~ heating ~ ~ ~ 
=> => pumping => =>
  I_IP coffee! I-IP
```



#### 关于图中的绑定

上述的例子展示了怎么使用一些更典型的绑定( `binding` )构建一个 `component` 组件，但是也有多种机制可以为对象图提供绑定功能。以下可用作依赖项，也可用于生成格式良好的组件：

- 那些直接被 `@Component` 注解的 `modules` 属性引用的 `@Module` 模块中被 `@Provides` 标注的方法 或者 传递给 `@Module` 注解的 `includes` 属性。（这句话是真的没看懂，原文在下面）
- Those declared by @Provides methods within a @Module referenced directly by @Component.modules or transitively via @Module.includes
- 任何一个被 `@Inject` 注解标注构造方法的类型，不管该类型是否被一个 `@Scope` 注解标注，都会与一个 `component` 组件的 域(scope)匹配。
- 联系[组件依赖关系](https://google.github.io/dagger/api/latest/dagger/Component.html#dependencies--)的[组件提供方式](https://google.github.io/dagger/api/latest/dagger/Component.html#provision-methods) 
- 组件本身
- Unqualified [builders](https://google.github.io/dagger/api/latest/dagger/Subcomponent.Builder.html) for any included [subcomponent](https://google.github.io/dagger/api/latest/dagger/Subcomponent.html)（任何被包含的子组件不适合构建者？）
-  `Provider` 或者 `Lazy` 用于上述任何绑定的包装器
- 上述任何绑定的 `Lazy` 的 `Provider`（例子：`Provider<Lazy<CoffeeMaker>>`）（？？？？）
- 一个可以用于任意类型的 `MembersInjector` 

### Singletons and Scoped Bindings （单例与域的绑定）

使用 `@Singleton` 注解标注一个 `@Provides` 的方法或者可注入的类。该对象图将会为它所有的客户端提供一个该类型的单例实例。（那么这个图的范围是什么？是 `component` 组件吗？）

```java
/**
 * 如果下载了上述的demo项目，可以尝试把 @Singleton去掉后运行程序
 * 会发现输出日志中会少一行：=> => pumping => =>
 * 是因为在输出的时候做了一个判断，判断 heater.isHot() 是否为true
 * 没输出说明不是true
 * 但是会发现输出了：~ ~ ~ heating ~ ~ ~，会发现存在：this.heating = true
 * 明明设置为true了，判断的时候为什么不通过？
 * 可以尝试在 `Thermosiphon.pump()` 中输出 heater
 * 在 `ElectricHeater.on()` 中输出 this
 * 会发现是两个对象
 * 在使用Dagger2时要尤其注意Scope
 */
@Provides 
@Singleton 
static Heater provideHeater() {
  return new ElectricHeater();
}
```

标注在可注入类上的注解 `@Singleton` 也可标注在 [文档(documentation)](http://docs.oracle.com/javase/7/docs/api/java/lang/annotation/Documented.html)上。它提醒潜在的维护者这个类可能被多个线程共享。(就是表示这个类会被多个地方使用，希望这些地方使用这个类的时候，用的都是同一个对象。把 “这些地方” 就可以理解成一个域，表示一个范围，也就是在这个范围中，这个类只有一个对象，也就是单例。一定要注意范围。在这个范围中单例了，在其他范围中也可能会有多个。范围就是域，就是 `Scope`)

```java
@Singleton
class CoffeeMaker {
  ...
}
```

由于 `Dagger2` 将对象图中的范围实例与 `component` 组件的实现类对象相关联，那么组件需要声明它所表示的范围。比如，没有必要在同一个 `component` 上同时标注 `@Singleton` 和 `@RequestScoped` 注解，因为这两个注解所代表的域具有不同的生命周期，所以在使用这两个注解时，它们标注的 `component` 的声明周期也应该是不同的。要想将一个 `component`组件与一个给定的域联系起来，很简单，只需要在 `component` 接口上使用该域(`@Singleton` 只是一种，还可以是各种自定义注解)注解标注即可。

```java
/*
 * @Singleton 标注了接口CoffeeShop
 * 表示这个 component 与 @Singleton 联系起来
 * 那么在 DripCoffeeModule.class 中被 @Singleton 标注的 provide 方法
 * 就会提供在此范围内的单例对象
 */
@Component(modules = DripCoffeeModule.class)
@Singleton
interface CoffeeShop {
  CoffeeMaker maker();
}
```

`component` 组件也可能被多个域注解标注。这表示它们(域注解)都是同一范围的别名(实际就代表都是一个域)，所以 `component` 组件就会包含绑定在它上的域(域注解)中的任意一个对象。(@A，@B都标注在一个 `DemoComponent` 上，那么在 `module` 中的 `@Provides` 注解标注的方法(还需要一个域注解标注)，标注该方法上的域注解是 `@A` 还是 `@B` 都属于同一个范围。都可以通过 `DaggerDemoComponent` (Dagger2为我们生成的实现类) 获得。)

### Reusable scope (可重用的范围)

有时候你想限制被 `@Inject` 注解标注的类的实例化次数，或者被 `@Provides` 注解标注的方法的调用次数，但是你不需要保证在任何特定 `component` 组件 或 `subcomponent` 子组件的生命周期中使用完全相同的实例对象。这在Android环境中很有用，因为分配的成本可能很高(这个成本指的是？)。

对于这些绑定来说，你可以使用 [`@Reusable`](https://google.github.io/dagger/api/latest/dagger/Reusable.html) 域。 `@Reusable` 域注解，与其他的域有一些不同，它不与任何单例的 `component` 组件产生联系。相反，实际使用绑定的 `component` 组件会缓存被返回的或实例化的对象（没懂）。

那意味着如果你在 `component` 组件初始化一个被 `@Reusable` 绑定的 `module` 模块，但是实际上只有一个 `subcomponent` 子组件使用这个绑定，那么只有这个子组件会缓存绑定的对象(这个缓存，指的是持有引用？)。如果两个不共享父级(父接口)的 `subcomponent` 子组件都使用这个绑定，那么他们中的每一个(就是每一个子组件)都会缓存一份属于自己的对象(子组件A缓存对象a，子组件B缓存对象b，a、b是同一类型的不同对象)。如果一个 `component` 的父级(父接口)已经缓存好了这个对象，那么 `subcomponent` 子组件会使用它( `subcomponent` 会直接使用 `component` 缓存好的对象 )。

无法保证 `component` 组件只调用绑定一次，因此将 `@Reusable` 应用于返回可变对象的绑定，或者应用在引用相同实例的重要的对象上是危险的。将 `@Reusable` 应用在不可变的对象上才是安全的，如果你不在乎它们(不变的对象)被分配了多少次，那么你可以不指定范围(域)。

```java
//我们使用多少范围注解并不重要，但是别浪费他们。
@Reusable // It doesn't matter how many scoopers we use, but don't waste them.
class CoffeeScooper {
  @Inject CoffeeScooper() {}
}

@Module
class CashRegisterModule {
  //别这么做，你得关心你到底把你的现金放到了哪个注册表中。（关心把对象放到了哪个范围中）
  //使用一个特殊的域替换。
  @Provides
  @Reusable // DON'T DO THIS! You do care which register you put your cash in.
            // Use a specific scope instead.
  static CashRegister badIdeaCashRegister() {
    return new CashRegister();
  }
}

//别这么做，你每次都想创建一个新的过滤器，所以你不需要指定范围
@Reusable // DON'T DO THIS! You really do want a new filter each time, so this
          // should be unscoped.
class CoffeeFilter {
  @Inject CoffeeFilter() {}
}
```

### Releasable references (可释放的引用)

反对：此功能已启用，计划在2018年7月删除。
 ok，少翻译一大块，开心的不行。
 算了还是了解一下吧。

当绑定的时候使用了域注解，那就意味着这个 `component` 组件对象持有一个绑定对象的引用，直到这个 `component` 对象被垃圾回收机制回收。在安卓敏感的内存环境中，当内存不足时，你也许想让没有正在使用的域对象被垃圾回收机制处理。

在那种情况下，你可以定义一个域(scope) ，并且使用 [`@CanReleaseReferences`](https://google.github.io/dagger/api/latest/dagger/releasablereferences/CanReleaseReferences.html) 注解这个 `scope` 。

```java
@Documented
@Retention(RUNTIME)
//这样，该域中(MyScope)的对象，如果不是正在使用中，当内存不足时就会被释放
@CanReleaseReferences
@Scope
public @interface MyScope {}
```

当你确定允许当某个域中的对象当前未被其他对象使用时，可以被垃圾回收机制回收，那么你可以向你的域中注入一个 [`ReleasableReferenceManager`](https://google.github.io/dagger/api/latest/dagger/releasablereferences/ReleasableReferenceManager.html) 对象，然后调用这个对象的 `releaseStrongReferences()` 方法，这个方法会使得 `component`组件持有一个该对象的 [`WeakReference`](https://docs.oracle.com/javase/7/docs/api/java/lang/ref/WeakReference.html) 而不是 `strong reference`。（弱引用：垃圾回收机制每次都会回收掉弱引用的对象。强引用：强引用指向的对象永远不会被垃圾回收机制回收，即时内存不足。）[Java强弱软虚四种引用类型](https://www.cnblogs.com/theo/p/6443493.html)

```java
@Inject 
@ForReleasableReferences(MyScope.class)
ReleasableReferenceManager myScopeReferenceManager;

void lowMemory() {
  myScopeReferenceManager.releaseStrongReferences();
}
```

如果你确定内存不足情景已经过去（就是又有内存了），那么你可以为任意已缓存的在低内存时期调用了 `releaseStrongReferences()` 后还没有被垃圾回收机制回收的对象恢复成强引用。（这个定语是真的长。。。。）

```java
void highMemory() {
  myScopeReferenceManager.restoreStrongReferences();
}
```

### Lazy injections (懒注入)

有时你需要一个被懒加载的对象。对于任意的绑定 `T` (泛型开始)，你可以创建一个在第一次调用 `Lazy<T>'s get()` 方法时，才实例化对象的 [`Lazy`](https://google.github.io/dagger/api/latest/dagger/Lazy.html) 。如果 `T` 是一个单例的类型，那么 `Lazy<T>` 会在对象图 `ObjectGraph` 中需要 `T` 的任意一个地方注入同一个实例。否则，每一个等待注入的地方都会获得一个它自己的 `Lazy<T>` 实例(A 中的 `Lazy<T>` 和 B 中的 `Lazy<T>` 是同一个类型的不同对象)。不管怎样，对已给定的 `Lazy<T>` 的后续调用都将返回同样的底层的 `T` 实例(第一次调用get()时创建T的对象，后续调用get()返回的都是前面创建的同一个T对象)。

```java
class GrindingCoffeeMaker {
  @Inject 
  Lazy<Grinder> lazyGrinder;

  public void brew() {
    while (needsGrinding()) {
      // Grinder created once on first call to .get() and cached.
      lazyGrinder.get().grind();
    }
  }
}
```

### Provider injections ( `Provider` 方式的注入)

有时你需要返回得到多个实例，而不是仅仅注入一个单例的对象。当你有几个选项(工厂，建造者等)时，一个选项要注入一个 [`Provider`](http://docs.oracle.com/javaee/7/api/javax/inject/Provider.html) 而不是只注入一个 `T`。每次调用 `Provider<T>` 的 `get()` 方法时，`Provider` 都会调用 `T` 的绑定逻辑。如果这个绑定逻辑是一个被 `@Inject` 标注的构造方法，那么再次调用 `T` 的绑定逻辑就意味着再次调用 `T` 的构造方法，这时，就创建了一个 `T` 的新对象，但是如果绑定规则是一个被 `@Provides` 标注的方法，那么就不能够保证会再次新建一个对象(方法的返回值是我们任意写的，如果我们没写类似 `new T()` 的代码，那么自然就不会创建新的 `T`)。

```java
class BigCoffeeMaker {
  @Inject 
  Provider<Filter> filterProvider;

  public void brew(int numberOfPots) {
  ...
    for (int p = 0; p < numberOfPots; p++) {
      maker.addFilter(filterProvider.get()); //new filter every time.
      maker.addCoffee(...);
      maker.percolate();
      ...
    }
  }
}
```

译者附加(可能没啥卵用，别喷我)：有的时候我们可能会遇到对象的转换，将 `List<AAAResponse>` 中的数据设置到 `List<AAA>` 中，可能就会出现上述的循环代码，此时创建 `AAA` 对象的过程就可以通过多次调用 `Provider<AAA>` 的 `get()` 方法来实现。就不用再写 `new AAA()` 了。当然更好的是注入 `Provider<AAASuper>` (`AAASuper` 是 `AAA` 的超类)，这样如果改了需求要将数据转换到 `List<BBB>` 中，只需要更改注入的对象即可，不需要改动当前类的任何代码。

笔记：注入的 `Provider<T>` 可能创建了难以理解的代码，也可能是在对象图中设计了错误的范围或错误的对象结构。通常你想使用 [`factory`](https://en.wikipedia.org/wiki/Factory_(object-oriented_programming)) 或者是 `Lazy<T>` 或者是更改你代码的生命周期和代码结构，使得你能够直接注入一个 `T`。 但是，在某些情况下，注入 `Provider<T>` 可以节省生命*(啥意思?)*。一个普遍的用途是当你必须使用不符合对象自然生命周期的传统框架时*(就用呗就?)*。(这个括号也是原文中的内容)(例子：`servlets` 被设计成单例模式，但是仅仅在 `request` 的上下文中有效。)

(这里是我加的：) `Servlet` 是javaweb中处理请求的模块，运行在java客户端的java程序叫做 `applet` ，为客户端提供支持的叫 `server`，`servlet` 就是 `server` 和 `applet` 的组合词。在javaweb中，如果不做特殊才处理（设置servlet的级别），只有当请求第一次发生时，才会创建对应的servlet对象，下次再访问同样的请求，不会再创建新的。也就是说如果 `request` 不发生，则 `servlet` 便不会做什么事情(正常情况下)。

### Qualifiers (限定)

有时单独的类型不足以识别依赖关系。例如，精致的咖啡机在加热不同的物品时应该使用不同的加热器。

在这种情况下，我们添加了一个限定注解。任何一个被 [`@Qualifier`](http://docs.oracle.com/javaee/7/api/javax/inject/Qualifier.html) 标注的注解(包含自定义注解)，都属于限定注解。这里有一个包含在 `javax.inject` 中的限定注解 [`@Named`](http://docs.oracle.com/javaee/7/api/javax/inject/Named.html) 的声明。

```java
@Qualifier
@Documented
@Retention(RUNTIME)
public @interface Named {
  String value() default "";
}
```

你可以创建你自己的限定注解，或者直接使用 `@Named` 。使用限定注解标注在感兴趣的属性和参数上。类型和限定注解，都会被用来表示依赖项。

```java
class ExpensiveCoffeeMaker {
  //如果是不同类型的依赖项，只需要使用一个 `@Inject` 即可
  //这里依赖了两个同类型的对象，那么只根据类型区分就不满足了，可以使用限定注解做区分
  @Inject @Named("water") Heater waterHeater;
  @Inject @Named("hot plate") Heater hotPlateHeater;
  ...
}
```

通过标注一致的被 `@Provides` 方法，提供限定的值。

```java
@Provides @Named("hot plate") static Heater provideHotPlateHeater() {
  return new ElectricHeater(70);
}

@Provides @Named("water") static Heater provideWaterHeater() {
  return new ElectricHeater(93);
}
```

依赖关系可能不会具有多个限定注解。(对于一个依赖项而已，基本上一个限定注解标注就够了)

### Optional bindings (可选绑定)

如果你想要 `component` 中的某些依赖项没有被绑定的情况下，依然能够正常运行，那么可以在 `module` 中添加一个 [`@BindsOptionalOf`](https://google.github.io/dagger/api/latest/dagger/BindsOptionalOf.html) 标注的抽象方法：

```java
@BindsOptionalOf 
abstract CoffeeCozy optionalCozy();
```

这意味着被 `@Inject` 标注的构造方法、成员变量和被 `@Provides` 标注的方法可以依赖于 `@Optional<CoffeeCozy>` 对象。如果在 `component` 组件中有一个 `CoffeeCozy` 的绑定， 则将显示 `Optional` ，如果没有 `CoffeeCozy` 的绑定，那么 `Optional`将不存在。

特殊的，你可以注入如下的任意一个。

-  `Optional<CoffeeCozy>` (除非 `CoffeeCozy` 有 `@Nullable` 绑定)
- `Optional<Provider<CoffeeCozy>>`
- `Optional<Lazy<CoffeeCozy>>`
- `Optional<Provider<Lazy<CoffeeCozy>>>`

(也是原文中的内容：)(你可以注入一个 `Provider` 或者 `Lazy` 或者 `Provider<Lazy>` 不过没啥用)

和下面的一个道理

- `List<String>`
- `List<List<String>>`
- `List<Hashmap<String,List<String>>>`

如果 `CoffeeCozy` 有绑定，并且绑定是 `@Nullable` ，那么注入 `Optional<CoffeeCozy>` 是一个编译期错误，因为 `Optional`不能包含 `null` 。你可以一直注入其他的格式，因为 `Provider` 和 `Lazy` 的 `get()` 方法能够返回 `null`。

在一个 `component` 组件中的可选择的隐藏的绑定，如果 `subcomponent` 子组件包含基础类型的绑定，那么可以在 `subcomponent` 子组件中显示。

可以使用 [Guava’s Optional](https://google.github.io/guava/releases/19.0/api/docs/com/google/common/base/Optional.html) 或者 [Java 8’s Optional](https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html)。

### Binding Instances (绑定实例)

通常在构建 `component` 时，你会得到可用的数据。例如，假设你有一个使用命令行参数的应用程序，你也许想在你的 `component` 组件中绑定那些参数。

也许你的应用程序只需要一个参数来表示你想要注入的用户名( `@UserName String` )。你可以在 `component` 的 `builder` 中添加一个 `@BindsInstance` 标注的方法使得实例可以被注入到这个 `component` 组件中。

```java
@Component(modules = AppModule.class)
interface AppComponent {
  App app();

  @Component.Builder
  interface Builder {
    @BindsInstance 
    Builder userName(@UserName String userName);
    AppComponent build();
  }
}
```

然后你的应用就会像这样：

```java
public static void main(String[] args) {
  if (args.length > 1) { exit(1); }
  App app = DaggerAppComponent
      .builder()
      .userName(args[0])//传入参数，与传入module类似
      .build()
      .app();
  app.run();
}
```

在上述例子中，在 `component` 组件中注入 `@UserName String` 将在调用次方法时，使用提供给 `Builder` 的实例。在构建 `component` 组件之前，必须先调用所有的 `@BindsInstance` 标注的方法，传递非空值(下面 `@Nullable` 绑定的除外)。

如果一个被 `@BindsInstance` 标注的方法中的参数被 `@Nullable` 标注，那么这个绑定就会被认为是可空的，就像 `@Provides` 标注的方法也是可空的一样，注入点也一定标记了 `@Nullable`，那么 `null` 就是该帮绑定的一个可接收的值。此外， `Builder` 的用户可以不调用这个方法，那么 `component` 组件会认为这个实例是一个null值。

`@BindsInstance` 标注的方法应该优先使用构造函数参数编写 `@Module` 并且立即提供这些值。

### Compile-time Validation (编译期验证)

Dagger [*annotation processor (注解处理器)*](http://docs.oracle.com/javase/6/docs/api/javax/annotation/processing/package-summary.html) 是精确的，如果任何一个绑定是无效或者不完整的，那么就会导致编译期错误。例如，下面的 `module` 被一个没有绑定 `Executor` 的 `component` 实例化：

```java
@Module
class DripCoffeeModule {
  @Provides static Heater provideHeater(Executor executor) {
    return new CpuHeater(executor);
  }
}
```

当编译上述代码， `javac` 会拒绝缺少的绑定：

```java
[ERROR] COMPILATION ERROR :
[ERROR] error: java.util.concurrent.Executor cannot be provided without an @Provides-annotated method.
```

通过在 `component` 中的任意一个 `module` 中添加一个 `Executor` 的 `@Provides` 标注的方法(该方法返回一个 `Executor` 的对象)来修复错误。虽然 `@Inject` 、 `@Module` 和 `@Provides` 注解是单独验证的，但是绑定与绑定之间的关系的验证都发生在 `@Component` 级别。Dagger 1 严格依赖与 `@Module` 级验证(可能存在运行时执行反射行为)，但是 Dagger 2 不需要这样的验证(以及在 `@Module` 上附带的配置参数)，支持完整的图形验证。

### Compile-time Code Generation (编译器的代码生成)

Dagger 的注解处理器也会生成名字类似于 `CoffeeMaker_Factory.java` 或者 `CoffeeMaker_MembersInjector.java` 这样的资源文件。这些文件是Dagger实现的细节。你不应该直接使用它们，尽管在进行 debug 调试它们时会很方便。你唯一应该在代码里引用的是那些根据你的 `component` 生成的前缀为 `Dagger` 的类型。

### Using Dagger In Your Build

你需要在你的应用程序运行时包含 `dagger-2.X.jar` 。为了触发代码生成器你需要在编译期构建时包含 `dagger-compiler-2.X.jar` 。查看更多信息 [*读我* ](https://github.com/google/dagger/blob/master/README.md#installation)。

