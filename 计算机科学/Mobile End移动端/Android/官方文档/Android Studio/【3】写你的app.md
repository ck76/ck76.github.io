- **创建自定义代码自动完成模板**

  举例来说，如果在输入 `newInstance` 缩写后按 **Tab**，则会插入包含参数占位符的新片段实例的代码。

  或输入 `fbc` 可插入 `findViewById()` 方法以及投射和资源 id 语法。

  如需查看支持的实时模板列表并对其进行自定义，请点击 **File > Settings > Editor > Live Templates**

- 将光标放在方法/成员/类名称上并按 **F1** 可查看 API 相关文档

- **创建支持所有屏幕密度的图像**

  Android Studio 包含一个名为 Vector Asset Studio 的工具，可帮助您创建支持各种屏幕密度的图像。 您可以上传自己的 SVG 文件进行编辑，或从 Google 提供的众多 Material Design 图标中选择一个。

  单击 **File > New > Vector Asset** 开始创建

- [Android Studio 3.0+ 新Dex编译器D8 Desugar R8](https://www.jianshu.com/p/bb6fb79dab17)

  - 将.class自己码转化为.dex字节码作为Apk打包的关键步骤，Google打算在Android 3.0中引入D8作为原先Dex的升级版，以及R8作为原本Proguard 压缩与优化（minification、shrinking、optimization)部分的替代品。升级Dex编译器将直接影响构建时间，.dex文件大小，运行时性能。

  - D8 的功能是把java字节码转化成dex代码，D8作为DX的一个替换方案
  - R8作为原本Proguard 压缩与优化（minification、shrinking、optimization)部分的替代品，依然使用与Proguard一样的keep规则

![](https://developer.android.google.cn/studio/images/write/desugar_2x.png)

- **Java 8支持相关**

  Android Studio 3.0 及以上版本支持所有 Java 7 语言功能，以及部分 Java 8 语言功能（具体因平台版本而异）。
   `注：在开发 Android 应用时，可以选择使用 Java 8 语言功能。 您可以将项目的源代码和目标代码兼容性值保留为 Java 7，但仍须使用 JDK 8 进行编译。`
   Android Studio 为使用部分 Java 8 语言功能及利用这些功能的第三方库提供内置支持。 如图 1 所示，默认工具链对 javac 编译器的输出执行字节码转换（称为 desugar），从而实现新语言功能。 Jack 不再受支持，您需要首先停用 Jack 才能使用默认工具链内置的 Java 8 支持。

  目前Java 8语言支持的处理是在javac之后，与字节码处理工具处理之前。在接下来的几个月，这个步骤将会被移动到pipeline的后一个阶段，作为D8的一部分。

  - 其带来的影响:
    - 减少这块的编译时间
    - 可以优化更多代码
    - 这么一来，所有字节码处理工具就必须要支持Java8的字节码格式了。

- **停用对Java 8语言功能的支持**

  如果您遇到与Java 8语言功能支持相关的问题，可在`gradle.properties`文件中加入以下代码来停用此支持：

  ```java
  android.enableDesugar=false  //在设置android.enableD8.desugaring = false的时候。编译链会对lambda表达式进行一次脱糖处理
  ```

- **D8的使用**

  已经在Android Studio 3.0 Beta release中引入

  - Android Studio 3.0
     需要主动在gradle.properties文件中新增:`android.enableD8=true` 
  - Android Studio 3.1或之后的版本
     在3.1或之后的版本D8将会被作为默认的Dex编译器。如果遇到问题，你可以通过修改gradle.properties文件里的一个属性恢复到DX `android.enableD8=false` 
  - 除了其他好处外，使用D8还有一个好处，就是支持 脱糖，**让Java 8才提供的特性（如lambdas）可以转换成Java 7特性。**把脱糖步骤集成进D8影响了所有读或写.class字节码的开发工具，因为它会使用Java 8格式。你可以在gradle文件中设置一个属性，恢复到以前的行为，让脱糖发生在Java编译之后，.class字节码仍遵循Java 7格式：`android.enableD8.desugaring = true`

  ![android.enableDesugar=false才脱糖](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Dsq5p9sFSwgtvL11cJGICw3UfhvGhT9rPZmjQqdl6Qc!/r/dMAAAAAAAAAA)

  - 脱糖（Desugar）

    脱糖就是脱掉java8的语法糖与更低版本的java7适配

    当我们选择JDK8以上版本时，有时候会使用lambda表达式，在设置`android.enableD8.desugaring = false`的时候。编译链会对lambda表达式进行一次脱糖处理。

```java
public class MainActivity extends Activity {

  @Override protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_main);
    TextView tv = findViewById(R.id.click);
    tv.setOnClickListener(view -> {
      Log.d("MainActivity", "MainActivity");
    });
    tv.setOnClickListener(new View.OnClickListener() {
      @Override public void onClick(View view) {
        Log.d("MainActivity", "MainActivity");
      }
    });

  }
}

编译后的Class文件如下：
路径为
app/build/intermediates/transforms/desugar/release(buildType)/0/com.jamin.d8desugar(packageName)/

//class文件
public class MainActivity extends Activity {

  public MainActivity() {
  }

  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    this.setContentView(2130968576);
    TextView tv = (TextView)this.findViewById(2130903040);
    tv.setOnClickListener(MainActivity$$Lambda$0.$instance);
    tv.setOnClickListener(new OnClickListener() {
      public void onClick(View view) {
        Log.d("MainActivity", "MainActivity");
      }
    });
  }
}

final class MainActivity$$Lambda$0 implements OnClickListener {
  static final OnClickListener $instance = new MainActivity$$Lambda$0();

  private MainActivity$$Lambda$0() {
  }

  public void onClick(View var1) {
    MainActivity.lambda$onCreate$0$MainActivity(var1);
  }
}

//实际上非D8脱糖，为了保证JAVA7及以下的兼容性。是将lambda表达式的在javac编译class的时候就已经将lambda表达式转化成更高兼容度的低版本代码。好处是在编译链中，有时候会使用一些java7的工具。他们对于java8的语法糖是无法识别的


public class MainActivity extends Activity {
	//简单改写一下源文件
  String abc = "abc";

  @Override protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_main);
    TextView tv = findViewById(R.id.click);
    tv.setOnClickListener(view -> {
      Log.d("MainActivity", "MainActivity" + abc);
    });
    tv.setOnClickListener(new View.OnClickListener() {
      @Override public void onClick(View view) {
        Log.d("MainActivity", "MainActivity" + abc);
      }
    });
  }
}

public class MainActivity extends Activity {
  String abc = "abc";

  public MainActivity() {
  }

  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    this.setContentView(2130968576);
    TextView tv = (TextView)this.findViewById(2130903040);
    //注意this传递过去了。类似于内部类的写法
    tv.setOnClickListener(new MainActivity$$Lambda$0(this));
    tv.setOnClickListener(new OnClickListener() {
      public void onClick(View view) {
        Log.d("MainActivity", "MainActivity" + MainActivity.this.abc);
      }
    });
  }
}

// $FF: synthetic class
final class MainActivity$$Lambda$0 implements OnClickListener {
  private final MainActivity arg$1;

  MainActivity$$Lambda$0(MainActivity var1) {
    this.arg$1 = var1;
  }

  public void onClick(View var1) {
    this.arg$1.lambda$onCreate$0$MainActivity(var1);
  }
}
```
- **更改资源目录**

```groovy
sourceSets {
        main {
            manifest.srcFile 'src/main/AndroidManifest.xml'
            java.srcDirs = ['src/main/java', 'src/main/aidl']
            resources.srcDirs = ['src/main/java', 'src/main/aidl']
            aidl.srcDirs = ['src/main/aidl']
            res.srcDirs = ['src/main/res']
            assets.srcDirs = ['src/main/assets']
        }
 			 debug {
            res.srcDirs = ['resources/debug']
        }
  		//您还可以为一个源集指定多个资源目录，然后构建工具将它们合并在一起。例如：
      //注意：如果两个或多个资源目录包含相同的资源文件，则在资源合并期间会发生错误。
  		 main {
            res.srcDirs = ['res1', 'res2']
        }
    }
```

- **资源合并**

  最终APK文件中的资源可能来自3个不同的来源：

  - The main source set (generally located in `src/main/res/`)
  - [Build variant](https://developer.android.google.cn/studio/build/build-variants.html) source sets
  - [Android libraries](https://developer.android.google.cn/studio/projects/android-library.html) (AARs)

  当来自每个源集或库的所有资源都是唯一的时，它们都被添加到最终的APK中。资源被认为是独一无二的，如果它的文件名是内唯一*既*其 [资源类型](https://developer.android.google.cn/guide/topics/resources/available-resources.html)目录和 [资源预选赛](https://developer.android.google.cn/guide/topics/resources/providing-resources.html#AlternativeResources) （如果定义）。

  如果存在两个或更多相同资源的匹配版本，则最终APK中仅包含一个版本。构建工具根据以下优先级顺序选择要保留的版本（左侧最高优先级）：

  > build variant > build type > product flavor > main source set > library dependencies

  例如，如果主源集包含：

  - `res/layout/foo.xml`
  - `res/layout-land/foo.xml`

  调试版本类型包含：

  - `res/layout/foo.xml`

  然后最终的APK包括`res/layout/foo.xml`调试构建类型和 `res/layout-land/foo.xml`主要源集。

  但是，**如果构建配置为给定的源集指定了 [多个资源文件夹](https://developer.android.google.cn/studio/write/add-resources#change_your_resource_directory)，**并且这些源之间存在冲突，则会发生错误，并且合并失败，因为每个资源目录具有相同的优先级。

- **使用布局编辑器构建视图**

- [Lint进行代码检查](https://developer.android.google.cn/studio/write/lint)

- **[注解](https://developer.android.google.cn/studio/write/annotations)**

```java
//Nullness注解
@NonNull
@Nullable
//资源注解
AnimRes	动画
AnimatorRes	animator资源类型
AnyRes	任何资源类型
ArrayRes	数组资源类型
AttrRes	属性资源类型
BoolRes	bool类型资源类型
ColorRes	颜色资源类型
DimenRes	长度资源类型
DrawableRes	图片资源类型
IdRes	资源id
InterpolatorRes	动画插值器
LayoutRes	layout资源
MenuRes	menu资源
RawRes	raw资源
StringRes	字符串资源
StyleRes	style资源
StyleableRes	Styleable资源类型
TransitionRes	transition资源类型
XmlRes
//线程注解值
注：构建工具会将 @MainThread 和 @UiThread 注解视为可互换，因此，您可以从 @MainThread 方法调用 @UiThread 方法，反之亦然。不过，如果系统应用在不同线程上带有多个视图，UI 线程可与主线程不同。因此，您应使用 @UiThread 标注与应用的视图层次结构关联的方法，使用 @MainThread 仅标注与应用生命周期关联的方法。

@MainThread
@UiThread
@WorkerThread
@BinderThread
@AnyThread
//约束注解
@IntRange
@FloatRange
public void setAlpha(@IntRange(from=0,to=255) int alpha) { … }
@Size 注解可以检查集合或数组的大小，以及字符串的长度
最小大小（例如 @Size(min=2)）
最大大小（例如 @Size(max=2)）
确切大小（例如 @Size(2)）
表示大小必须为此倍数的数字（例如 @Size(multiple=2)）
//权限注解
@RequiresPermission 注解可以验证方法调用方的权限
要检查有效权限列表中是否存在某个权限，请使用 anyOf 属性。要检查是否存在一组权限，请使用 allOf 属性。下面的示例会标注 setWallpaper() 方法，以确保方法的调用方拥有 permission.SET_WALLPAPERS 权限：

@RequiresPermission(Manifest.permission.SET_WALLPAPER)
public abstract void setWallpaper(Bitmap bitmap) throws IOException;

@RequiresPermission(allOf = {
    Manifest.permission.READ_EXTERNAL_STORAGE,
    Manifest.permission.WRITE_EXTERNAL_STORAGE})
public static final void copyFile(String dest, String source) {
    ...
}

对于 intent 权限，请将权限要求添加到定义 intent 操作名称的字符串字段上：
@RequiresPermission(android.Manifest.permission.BLUETOOTH)
public static final String ACTION_REQUEST_DISCOVERABLE =
            "android.bluetooth.adapter.action.REQUEST_DISCOVERABLE";

//间接权限返回值注解 
//CallSuper 注解
使用 @CallSuper 注解可以验证替换方法是否会调用方法的超类实现。下面的示例会标注 onCreate() 方法，以确保任何替换方法实现都会调用 super.onCreate()：

@CallSuper
protected void onCreate(Bundle savedInstanceState) {
}
//typedef 注解
使用 @IntDef 和 @StringDef 注解，以便能够创建整型和字符串集的枚举注解来验证其他类型的代码引用。

public abstract class ActionBar {
    ...
    // Define the list of accepted constants and declare the NavigationMode annotation
    @Retention(RetentionPolicy.SOURCE)
    @IntDef({NAVIGATION_MODE_STANDARD, NAVIGATION_MODE_LIST, NAVIGATION_MODE_TABS})
    public @interface NavigationMode {}

    // Declare the constants
    public static final int NAVIGATION_MODE_STANDARD = 0;
    public static final int NAVIGATION_MODE_LIST = 1;
    public static final int NAVIGATION_MODE_TABS = 2;

    // Decorate the target methods with the annotation
    @NavigationMode
    public abstract int getNavigationMode();

    // Attach the annotation
    public abstract void setNavigationMode(@NavigationMode int mode);


//代码可访问性注解
使用 @VisibleForTesting 和 @Keep 注解可以表示方法、类或字段的可访问性。

@VisibleForTesting 注解指示一个代码块的可见性是否高于让代码变得可测试所需要的水平。

@Keep 注解可以确保如果在构建时缩减代码，标注的元素不会移除。它一般会添加到通过反射访问的方法和类中，以阻止编译器将代码视为未使用。
```

