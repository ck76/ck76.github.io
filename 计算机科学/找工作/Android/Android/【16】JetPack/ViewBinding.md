http://wenqin231.com/2020/04/13/ViewBinding/

[TOC]

**ViewBinding** 是 Android Studio 3.6 推出的新工具，可以用来轻松高效地编写与 View 交互的代码。在启用后，根据 XML 布局文件会生成对应的 binding 类，这个类包含了布局内所有设置过 id 的 View 的引用。

官方推荐使用它来替代 `findVeiwById` ；[Jake Wharton](https://twitter.com/JakeWharton) 也在社交平台上推荐它替换 `ButterKnife`；在 kotlin中常用的 `Kotlin Synthetics` 又有什么比不上它的呢？它本身会不会有什么坑？让我们展开讲讲。



### 优势

相比于传统的 `findViewById`，`ViewBinding` 最直观的优势是减少代码量。对于追求效（lan）率（duo）的工程师来说，这是最吸引人的地方。

虽然 `ButterKnife` 也可以通过插件做到这点，但是那些 `@BindView` 还是实打实地存在你的代码中，并且以后每增减一个 widget，你都需要考虑那些绑定关系。而 `ViewBinding` 是动态生效的，在你的 XML 布局文件添加完新的 widget 后，`ViewBinding` 类就可以调用它了😝



有朋友可能会说了，`Kotlin Synthetics` 同样也减少了很多代码量呢！跟你这个 `ViewBinding` 比起来，它不香么？那这里就不得不提到 `ViewBinding` 的第二个优势了——安全。

我们知道，`Kotlin Synthetics` 是可以通过 View 或者 Activity 引用到任意 XML 中的 widget，如果不小心引用错就会导致应用 crash。而且这种异常在编译期还没法被发现，只有程序运行到使用错误的地方才会出现，这就给应用留下了安全隐患⚠️

而 ViewBinding 则不具备这样的问题，它保证了你只能调用对应 XML 布局文件的 widget，相对来说更安全。而且它兼容 Java，这对于还有部分代码没迁移到 Kotlin 的代码来说，也是比较友好的。

|                             | ViewBinding | ButterKnife | Kotlin Synthetics | findViewById |
| :-------------------------- | :---------: | :---------: | :---------------: | :----------: |
| 空安全                      |      ✅      |      ✅      |         ⛔️         |      ⛔️       |
| 保证引用是来自 XML 布局文件 |      ✅      |      ⛔️      |         ⛔️         |      ⛔️       |
| 支持 Kotlin 和 Java         |      ✅      |      ✅      |         ⛔️         |      ✅       |
| 代码量                      |      ❤️      |      💔      |         ❤️         |      💔       |

> 尽管 ButterKnife 有保护 View 的空安全，但是它不能保证 id 是不是来自于声明的 xml

那又有朋友会问，你这个是不是跟 `DataBinding` 一样呀！我用了 `DataBinding` 是不是就不需要它了呀？

虽然他们两者都是生成 Binding 类，但是 `ViewBinding` 更倾向于在简单的场景下使用， 因此它相对于 `DataBinding` 来说：

- 无需注解，编译时间更快
- 使用方便，因为它不需要对 XML 布局文件进行特殊标志，所以在项目里启动它非常方便

当然，比起 `DataBinding` 它也有些限制：

- 不支持布局变量和布局表达式，所以它不能从 XML 布局文件中获取数据动态更新 UI
- 不支持双向数据绑定

因此，你可以同时支持 `ViewBinding` 和 `DataBinding`。如果需要额外的特性，你可以让 `DataBinding` 来完成，而简单的应用就交给 `ViewBinding`

### 使用

在 `build.gradle` 中声明使用 `ViewBinding`

```java
// Android Studio 3.6
android {
    viewBinding {
        enabled = true
    }
}
```

而在 Android Stduio 4.0 中，`ViewBinding` 已经移入`buildFeatures` 中了

```java
// Android Studio 4.0
android {
    buildFeatures {
        viewBinding = true
    }
}
```

声明后，根据你的 xml 命名动态生成 **Binding，例如 activity_home 就会生成 ActivityHomeBinding

```java
private lateinit var binding: ActivityHomeBinding

override fun onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    binding = ActivityHomeBinding.inflate(layoutInflater)
    val view = binding.root
    setContentView(view)
}
```

如果你已经 inflate 了 xml，那也可以使用 `bind(view)` 将 `ViewBinding` 和 View 绑定，这便于我们对自定义 View 进行重构。

### 实践

从去年年底开始，我就在 Android Studio 3.6 preview 版本上使用 `ViewBinding` 开发了。下面分享一下项目内的实践思路。

#### Fragment

最经常使用的 Fragment，我创建了 BindingFragment，继承于项目里原来的 `BaseFragment`，让 Binding 的业务尽量不影响原来的封装。

```java
abstract class BindingFragment<T : ViewBinding> : BaseFragment() {

    protected lateinit var binding: T

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        initData().invoke(arguments ?: Bundle())
        binding = createBinding().invoke(inflater, container, false)
        return binding.root
    }

    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)
        setupView().invoke(binding)
    }

    abstract fun createBinding(): (
        inflater: LayoutInflater,
        container: ViewGroup?,
        attachToRoot: Boolean
    ) -> T

    open fun initData(): Bundle.() -> Unit = {}

    open fun setupView(): T.() -> Unit = {}

    fun ctx(): Context = binding.root.context
}
```

子类使用

```java
class HomeFragment : BindingFragment<FragmentHomeBinding>() {

    override fun createBinding(): (
        inflater: LayoutInflater,
        container: ViewGroup?,
        attachToRoot: Boolean
    ) -> FragmentHomeBinding {
        return FragmentHomeBinding::inflate
    }

    override fun setupView(): FragmentHomeBinding.() -> Unit = {
        tvContent.text = "ViewBinding is cool"
    }
}
```

对于子类，我只需要关心 `fragment_home.xml` 生成的 `FragmentHomeBinding`，不需要额外对 `R.layout.fragment_home` 进行处理。在 `setupView` 中我可以直接使用 `tvContent` 的引用，就像在使用 `kotlin Synthetics` 一样。

#### View

自定义 View 也是经常使用的，这里有两种使用思路

先用 `View#inflate` 绑定 XML 后，再用 `ViewBinding#bind` 绑定 View，这样就可以使用 `ViewBinding` 来渲染相应的 UI，这种方式方便我们重构已经写好的 View：

```java
class EmptyView @JvmOverloads constructor(
    context: Context, attrs: AttributeSet? = null, defStyleAttr: Int = 0
) : ConstraintLayout(context, attrs, defStyleAttr) {
    
    private val binding: ViewEmptyBinding
    
    init {
        View.inflate(context, R.layout.view_empty, this)
        binding = ViewEmptyBinding.bind(this)
        binding.apply {
            tvTitle.text = "Data is emtpy"
        }
    }
}
```

另一种是使用 `ViewBinding#inflate`，这种就不用关心 `R.layout.view_emty`：

```java
class EmptyView @JvmOverloads constructor(
    context: Context, attrs: AttributeSet? = null, defStyleAttr: Int = 0
) : ConstraintLayout(context, attrs, defStyleAttr) {

    private val binding = ViewEmptyBinding.inflate(LayoutInflater.from(context), this)

    init {
        binding.apply {
            tvTitle.text = "Data is emtpy"
        }
    }
}
```

#### Adapter

项目里的 Adapter 是基于第三方库进行封装的，这里提供一种基于 `RecyclerView.Adapter` 的封装作为参考：

```java
open class BindingViewHolder<VB : ViewBinding>(val vb: VB) : RecyclerView.ViewHolder(vb.root)

abstract class BindingAdapter<VB : ViewBinding, T> : RecyclerView.Adapter<BindingViewHolder<VB>>() {

    val data: MutableList<T> = arrayListOf()

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): BindingViewHolder<VB> {
        return BindingViewHolder(
            createBinding().invoke(LayoutInflater.from(parent.context), parent, false)
        )
    }

    override fun getItemCount(): Int = data.size

    override fun onBindViewHolder(holder: BindingViewHolder<VB>, position: Int) {
        data.getOrNull(position)?.apply { 
            update(this).invoke(holder.vb)
        }
    }

    abstract fun createBinding(): (
        inflater: LayoutInflater,
        container: ViewGroup?,
        attachToRoot: Boolean
    ) -> VB

    open fun update(item: T): VB.() -> Unit = {}
}
```

实现思路也是和 Fragment 类似的，Adapter 的子类是需要是实现 `createBinding` 就可以实现相应的逻辑。这里仅提供一个简单的例子，目前这个封装还解决不了多布局的问题。如果需要解决多布局的问题还需要基于 ViewHolder 进行封装，聪明的你能想出来么？

> 每个项目都有自己封装的关于 Adapter 类，这边提供的是封装思路仅为抛砖引玉。这里奉上我[基于 ViewHolder 的封装](https://gist.github.com/wenqin-231/70ee973b6d130e7e247360ee6d6021ad)，有更好的想法欢迎与我讨论

看完了我们上面三个的封装，你会发现，我尽量都是让 ViewBinding 去替换掉我原先使用 `R.layout.xxx` 的地方，让调用方只关心 ViewBinding 就可以写好代码。不好的地方就是 lint 检查的时候会发现我有大量的 xml 没有使用😂

### 缺陷

前面是聊了这么久 ViewBinding 的优点，我们再来讲讲它的缺点：

- 调用方式的不友好
  - 与 `kotlin synthetics` 相比，widget 的引用要依赖于 binding 的实例，但可以用 apply 或者 run 来缓解
- inlcude 标签不友好
  - 使用其他方式，即便我们不给 include 加 id，也可以正常使用。而 `Viewbinding` 你得使用 `binding.inlcdeId.widgetId` 来获得 widget 的引用。如果 include 里面还有 inlcude ，就只能不停地套娃🙄
  - inlcude 如果使用了 merge 标签，你还得 `MyInlcudeLayoutBinding.bind(binding.includeId)` 来绑定，否则会报错
- 重复命名不友好
  - 如果一个 id 的命名和 inlcude 或者自定义 View 内部的命名相同，那么这个 widget 的引用将会失效，而且程序不会崩溃🤦‍♀️，这种隐藏的问题只有程序运行到那里并且发现 UI 异常才能被发现😭

### 总结

对我来说，`ViewBinding` 还是蛮甜的，由于是 Binding 的引用，每处的调用我会相对比较有安全感，不用担心代码的出错。经过半年的实践开发，也没有遇到比较大的坑，推荐在实际项目中使用😝