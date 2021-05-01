[TOC]

### 一、简介

> 一句话概述： **Paging** 可以使开发者更轻松在 **RecyclerView** 中 **分页加载数据**。

在很久很久以前，加载并展示大量数据就已成为各家应用中必不可少的业务场景，分页加载也就成了必不可少的方案。在现有的Android API中也已存在支持分页加载内容的方案， 比如：

- `CursorAdapter`：它简化了数据库中数据到`ListView`中Item的映射， 仅查询需要展示的数据，但是查询的过程是在UI线程中执行。
- SupportV7包中的`AsyncListUtil`支持基于position的数据集分页加载到`RecyclerView`中，但不支持不基于position的数据集，而且它强制一个有限数据集中的null项必须展示Placeholder.

**针对现有方案所存在的一些问题，Google推出了Android架构组件中的Paging Library， 不过目前还是alpha版本。Paging Library主要由3个部分组成：`DataSource`、`PagedList`、`PagedListAdapter`。**



### 二、原理

> `DataSource`, `PagedList`, `PagedAdapter`三者之间的关系以及加载数据到展示数据的流程如下图所示：

（1）Paging的数据流是在后台线程生产的，在后台线程中完成了大部分工作，在UI线程中显示。
比如说：当一条新的item插入到数据库，DataSource会被初始化，LiveData\<PagedList>后台线程就会创建一个新的PagedList。这个新的PagedList会被发送到UI线程的PagedListAdapter中，PagedListAdapter使用DiffUtil在对比现在的Item和新建Item的差异。当对比结束，PagedListAdapter通过调用RecycleView.Adapter.notifyItemInserted()将新的item插入到适当的位置。RecycleView就会知道它需要绑定一个新的item，并将其显示。
（2）从代码层面来说，我们需要给Recyclerview设置PagedListAdater，PagedListAdapter设置对应的PagedList。每一次adapter getItem就是让PagedList知道我们已经滑到第几个item了，PagedList计算这些数量以及配置的参数，当条件达成就通知DataSource，让其返回数据。数据返回成功时，通知PagedListAdapter进行刷新等操作。

（3）**结合图有几行比较重要的代码**

```java
//PagingDataSourceFactory.java
 private MutableLiveData<PagingDataSource<T>> mDataSource = new MutableLiveData<>();
 public PagingDataSourceFactory(PagingDataSource<T> dataSource) {
        mDataSource.setValue(dataSource);
    }
@Override
public DataSource<Integer, T> create() {
        return mDataSource.getValue();
    }

//PagingDataSource.java


//PagingViewModel.java
private LiveData<PagedList<T>> mDataList;
private PagingDataSourceFactory<T> mFactory;
private LiveData<PagedList<T>> buildPagedList() {
        return new LivePagedListBuilder<>(mFactory, getConfig())
                .setBoundaryCallback(new PagingBoundaryCallback<>(hasMore, isEmpty))
                .build();
    }
private PagedList.Config getConfig() {
        return new PagedList.Config.Builder()
                //配置分页加载的数量
                .setPageSize(PAGE_SIZE)
                //配置是否启动PlaceHolders
                .setEnablePlaceholders(false)
                //初始化加载的数量
                .setInitialLoadSizeHint(INIT_LOAD_SIZE)
                .build();
    }

```



![原理图](https://upload-images.jianshu.io/upload_images/7293029-27facf0a399c66b8.gif?imageMogr2/auto-orient/strip%7CimageView2/2/w/800)



### 三、Datasource

> 顾名思义，`Datasource<Key, Value>`是数据源相关的类，其中`Key`对应加载数据的条件信息，`Value`对应返回结果， 针对不同场景，Paging提供了三种Datasource:

- `PageKeyedDataSource<Key, Value>`：适用于目标数据根据页信息请求数据的场景，即`Key`字段是页相关的信息。比如请求的数据的参数中包含类似`next/previous`的信息。如果页面需要实现上一页、下一页，需要将请求的Token传递到下一步

- `ItemKeyedDataSource<Key, Value>`：适用于目标数据的加载依赖特定item的信息， 即Key字段包含的是Item中的信息，比如需要根据第N项的信息加载第N+1项的数据，传参中需要传入第N项的ID时，该场景多出现于论坛类应用评论信息的请求。程序需要根据上一条数据信息（ID）获取下一条数据时

- `PositionalDataSource<T>`：适用于目标数据总数固定，通过特定的位置加载数据，这里`Key`是Integer类型的位置信息，`T`即`Value`。 比如从数据库中的1200条开始加在20条数据。需要从数据存储中选择的任何位置获取数据页；例如，请求可能返回以位置1200开头的20个数据项

以上三种Datasource都是抽象类， 使用时需实现请求数据的方法。三种Datasource都需要实现`loadInitial()`方法， 各自都封装了请求初始化数据的参数类型`LoadInitialParams`。 不同的是分页加载数据的方法，`PageKeyedDataSource`和`ItemKeyedDataSource`比较相似， 需要实现`loadBefore()`和`loadAfter()`方法，同样对请求参数做了封装，即`LoadParams<Key>`。`PositionalDataSource`需要实现`loadRange()`，参数的封装类为`LoadRangeParams`。

如果项目中使用Android架构组件中的Room， Room可以创建一个产出`PositionalDataSource`的`DataSource.Factory`：

```java
@Query("select * from users WHERE age > :age order by name DESC, id ASC")
DataSource.Factory<Integer, User> usersOlderThan(int age);
```

总的来说，Datasource就像是一个抽水泵，而不是真正的水源，它负责从数据源加载数据，可以看成是Paging Library与数据源之间的接口。



### 四、PagedList

PagedList 通过 Datasource 加载数据， 通过 Config 的配置，可以设置一次加载的数量以及预加载的数量等。 除此之外，PagedList 还可以向 RecyclerView.Adapter 发送更新UI的信号。

如果将Datasource比作抽水泵，那PagedList就像是一个蓄水池，但不仅仅如此。PagedList是List的子类，支持所有List的操作， 除此之外它主要有五个成员：

- `mMainThreadExecutor`: 一个主线程的Excutor, 用于将结果post到主线程。
- `mBackgroundThreadExecutor`: 后台线程的Excutor.
- `BoundaryCallback`:加载Datasource中的数据加载到边界时的回调.
- `Config`: 配置PagedList从Datasource加载数据的方式， 其中包含以下属性：
  - `pageSize`：设置每页加载的数量
  - `prefetchDistance`：预加载的数量
  - `initialLoadSizeHint`：初始化数据时加载的数量
  - `enablePlaceholders`：当item为null是否使用PlaceHolder展示
- `PagedStorage<T>`: 用于存储加载到的数据，它是真正的蓄水池所在，它包含一个`ArrayList<List<T>>`对象`mPages`，按页存储数据。

PagedList会从Datasource中加载数据，更准确的说是通过Datasource加载数据， 通过Config的配置，可以设置一次加载的数量以及预加载的数量。 除此之外，PagedList还可以向RecyclerView.Adapter发送更新的信号，驱动UI的刷新。

```java
  public static final int INIT_LOAD_SIZE = 20;
  public static final int PAGE_SIZE = 20;

  private PagedList.Config getConfig() {
        return new PagedList.Config.Builder()
                //配置分页加载的数量
                .setPageSize(PAGE_SIZE)
                //配置是否启动PlaceHolders
                .setEnablePlaceholders(false)
                //初始化加载的数量
                .setInitialLoadSizeHint(INIT_LOAD_SIZE)
                .build();
    }
```



### 五、PagedListAdapter

PagedListAdapte是RecyclerView.Adapter的实现，用于展示PagedList的数据。它本身实现的更多是Adapter的功能，但是它有一个小伙伴`PagedListAdapterHelper<T>`， PagedListAdapterHelper会负责监听PagedList的更新， Item数量的统计等功能。这样当PagedList中新一页的数据加载完成时， PagedAdapte就会发出加载完成的信号，通知RecyclerView刷新，这样就省略了每次loading后手动调一次`notifyDataChanged()`.

除此之外，当数据源变动产生新的PagedList,PagedAdapter会在后台线程中比较前后两个PagedList的差异，然后调用notifyItem...()方法更新RecyclerView.这一过程依赖它的另一个小伙伴`ListAdapterConfig`， ListAdapterConfig负责主线程和后台线程的调度以及`DiffCallback`的管理，`DiffCallback`的接口实现中定义比较的规则，比较的工作则是由`PagedStorageDiffHelper`来完成。



### 六、加载数据

使用Paging Library加载数据主要有两种方式，一种是单一数据源的加载（本地数据或网络数据）， 另一种是多个数据源的加载（本地数据+网络数据）。

#### 1、加载单一数据源的数据

**首先我们可以通过`LivePagedListBuilder`来创建`LiveData<PagedList>`为UI层提供数据。**整个流程如下图所示：

![paging加载单一数据](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/xOXwBGp4eKPQ14KbLXQVf50Kww9K9PChihzj*qWalP0!/r/dLYAAAAAAAAA)

如果数据源是DB，当数据发生变化，DB会推送(push)一个新的PagedList（这里会依赖`LiveData`的机制）. 如果是网络数据，即客户端无法知道数据源的变化，可以通过诸如滑动刷新的方式将调用Datasource的`invalidate()`方法来拉去(pull)新的数据。

#### 2、加载多个数据源的数据

这种场景一般是先加载本地数据，加载完成后再加载网络数据，比较适合需要本地做缓存的业务。比如IM中的聊天消息，当打开聊天界面时先加载本地数据库中的聊天消息，加载完了再加载网络的离线消息。这中场景的流程如下图所示：

![paging加载多个数据源流程](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Y4q1LZyKkmsyyUo0L8uTTHDjqRE1Ki4g1IZWlTDV4G4!/r/dL8AAAAAAAAA)

**这种场景需要为PagedList设置`BoundaryCallback`来监听加载完本地数据的事件，触发加载网络数据，然后入库，此时LiveData\<PagedList>会推送一个新的PagedList, 并触发界面刷新。**



### 七、使用

使用时涉及我几个个关键类

**云想版本———Repository总、作用不同了**

- PagedList     一个集合类分块异步加载数据
- PagedListAdapter  适配器  将Page的List中数据加载到RecycleView中

- ViewModel提供LiveData\<PagedList>给Adapter  **(向Adapter提供数据)**
- XXXRepository    负责创建LiveData\<PagedList>  **(根本数据源)**

- XXXDataSourceFactory    负责创建DataSource，与PagedList的Config一起使用创建LiveData<PagedList\<T>>
- XXXDataSource   将Repository产生数据加载到ViewModel的PagedList **(数据源)**
- XXXBoundaryCallback   数据靠近结尾的时候自动加载数据


![谷歌视频截图](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/vRLQqpMuDHKsMZ7CvdWy8xm7xbKb*Bg6lvSPFVBgHZ0!/r/dL4AAAAAAAAA)



### 八、小结

Paging Library作为Android架构组件库的一员，其特点主要还是在其架构思想上。Paging将分页的业务封装为一条完整的流水线，一个Pattern。其中各个组件之间存在联动的关系：

- 当PagedList创建时会立即从`Datasource`加载数据（触发`loadInitial()`）, `DataSource`加载到数据后会更新`PagedList`, `PagedList`更新会通知到`PagedAdapter`并刷新UI；
- UI上的展示会触发`PagedAdapter`的`getItem()`随即触发PagedList的`loadAround()`方法从`DataSource`加载周围的数据...

整个过程Paging内部实现了线程的切换，数据的预加载，所有联动的关系都内聚到Paging中，这样使用时只需要关心加载数据的具体实现，并且在用户体验上，将会大大减少等待数据加载的时间和次数。



### 链接

- [官方文档](https://developer.android.google.cn/topic/libraries/architecture/paging/)
- [玉刚说一篇文章](https://www.jianshu.com/p/10bf4bf59122)
- [类似云想](https://www.jianshu.com/p/2e1dc6e7278b?utm_campaign=maleskine&utm_content=note&utm_medium=seo_notes&utm_source=recommendation)
- [简洁](https://www.jianshu.com/p/ff5c711bb7a1?utm_campaign=maleskine&utm_content=note&utm_medium=seo_notes&utm_source=recommendation)
- [三种DataSource区别](https://www.jianshu.com/p/fd00c0fbd774)