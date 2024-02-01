[TOC]



### 一、UML类图

> 原谅我还没能弄一个清楚点的类图，我也在努力搞🧤

![paging类图1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/mS2JfPwmQV1aJG*QSZLoZzhrlueR*S6Ojd9JO92.EiQ!/r/dLgAAAAAAAAA)



![paging类图2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/sgsQnH4S2l5pQ4*nQabYGbW5I4GRc7nnO3U70n8mGyk!/r/dFMBAAAAAAAA)

### 二、结构

```java
├── PagingLayoutManager.java
├── PagingListAdapter.java
├── PagingViewModel.java   				//mDataList  mFactory
├── api
│   ├── PagingCallBack.java				//请求回调
│   └── PagingRepository.java			//数据仓库接口
├── data    						
│   ├── NetworkStatus.java				//网络状态,也可以是刷新状态
│   ├── PagingBoundaryCallback.java		//边界数据回调，生成LiveData时设置
│   ├── PagingDataSource.java			//自定义DataSource
│   └── PagingDataSourceFactory.java	//自定义DataSourceFactory
└── holder
    ├── BaseViewHolder.java
    ├── EmptyViewHolder.java
    ├── ErrorViewHolder.java
    └── LoadingViewHolder.java
```

#### 1、api

##### PagingCallBack.java

- 请求回调接口

```java
public interface PagingCallBack<T> {
    void onSuccess(List<T> list);
    void onError(Exception e);
}
```

##### PagingRepository.java

- 数据仓库

```java
public interface PagingRepository<T> {
	//PagingCallBack在这里设置
    //实现此接口的类需要具体实现获取数据的方法。
    void getDataList(int offset, int count, PagingCallBack<T> callBack);
}
```



#### 2、data

##### NetWorkStatus.java

- 数据源，巧妙的运用枚举来定义网络请求的状态

```java
public class NetworkStatus {

    //枚举状态
    private NetworkStatus.Status mStatus;

    public NetworkStatus(Status status) {
        mStatus = status;
    }

    public boolean isLoading() {
        return this.mStatus == Status.RUNNING;
    }

   //success...failed

    public NetworkStatus.Status getStatus() {
        return mStatus;
    }

    public static NetworkStatus loading() {
        return new NetworkStatus(Status.RUNNING);
    }

    //...error...

    public enum Status {

        /** 正在请求 */
        RUNNING(1),
        /** 请求成功 */
        SUCCESS(0),
        /** 请求失败 */
        FAILED(-1);

        int mStatus;

        Status(int status) {
            mStatus = status;
        }
    }
}
```



##### PagingBoundaryCallback.java

- 生成LiveData的时候设置`.setBoundaryCallback(new PagingBoundaryCallback<>(hasMore, isEmpty))`，在数据加载到边界时，会调用CallBack中的方法。

```java
public class PagingBoundaryCallback<T> extends PagedList.BoundaryCallback<T> {
    
    private MutableLiveData<Boolean> hasMore;
    private MutableLiveData<Boolean> isEmpty;

    public PagingBoundaryCallback(MutableLiveData<Boolean> hasMore,
                                  MutableLiveData<Boolean> isEmpty) {
        this.hasMore = hasMore;
        this.isEmpty = isEmpty;
    }

    /**
     * 初始化加载为空
     */
    @Override
    public void onZeroItemsLoaded() {
        super.onZeroItemsLoaded();
        //空的。没有更多
        isEmpty.postValue(true);
        hasMore.postValue(false);
    }

    /**
     * LoadMore为空
     *
     * @param itemAtEnd 末尾数据
     */
    @Override
    public void onItemAtEndLoaded(@NonNull T itemAtEnd) {
        super.onItemAtEndLoaded(itemAtEnd);
        hasMore.postValue(false);
    }
}
```



##### PagingDataSource.java

- 数据源

```java
public class PagingDataSource<T> extends PositionalDataSource<T> implements DataSource.InvalidatedCallback {

    private Runnable mRetry;
    private PagingViewModel<T> mViewModel;
    private PagingRepository<T> mApi;

    private final CompositeDisposable mDisposable = new CompositeDisposable();

    public PagingDataSource(PagingViewModel<T> viewModel, PagingRepository<T> api) {
        this.mViewModel = viewModel;
        this.mApi = api;

        register(mViewModel.registerRefresh().subscribe(s -> invalidate(), Throwable::printStackTrace));
        register(mViewModel.registerRetry().subscribe(s -> {
            if (mRetry != null) {
                mRetry.run();
                mRetry = null;
            }
        }, Throwable::printStackTrace));

        addInvalidatedCallback(this);
    }

    private void register(Disposable disposable) {
        mDisposable.add(disposable);
    }
	/**
	* loadRange和loadInitail都是继承自PositionalDataSource
	*/
    @Override
    public void loadInitial(@NonNull LoadInitialParams params, @NonNull LoadInitialCallback<T> callback) {

        mViewModel.getRefreshState().postValue(NetworkStatus.loading());
        mApi.getDataList(0, PagingViewModel.INIT_LOAD_SIZE, new PagingCallBack<T>() {
            @Override
            public void onSuccess(List<T> list) {
                mViewModel.getRefreshState().postValue(NetworkStatus.success());
                mViewModel.hasMore().postValue(true);
                if (isEmptyList(list)) {
                    callback.onResult(new ArrayList<>(), 0);
                } else {
                    callback.onResult(new ArrayList<>(list), 0);
                }
            }

            @Override
            public void onError(Exception e) {
                mViewModel.getRefreshState().postValue(NetworkStatus.error());
                mRetry = () -> loadInitial(params, callback);
            }
        });
    }

    @Override
    public void loadRange(@NonNull LoadRangeParams params, @NonNull LoadRangeCallback<T> callback) {

        mViewModel.getLoadingState().postValue(NetworkStatus.loading());
        mApi.getDataList(params.startPosition, PagingViewModel.PAGE_SIZE, new PagingCallBack<T>() {
            @Override
            public void onSuccess(List<T> list) {
                mViewModel.getLoadingState().postValue(NetworkStatus.success());
                if (isEmptyList(list)) {
                    callback.onResult(new ArrayList<>());
                } else {
                    callback.onResult(list);
                }
            }

            @Override
            public void onError(Exception e) {
                mViewModel.getLoadingState().postValue(NetworkStatus.error());
                mRetry = () -> loadRange(params, callback);
            }
        });
    }


    private boolean isEmptyList(List list) {
        return list == null || list.isEmpty();
    }


    @Override
    public void onInvalidated() {
        removeInvalidatedCallback(this);
        mDisposable.clear();
    }
}
```



##### PagingDataSourceFactory.java

- 数据源工厂

  ```java
   private LiveData<PagedList<T>> buildPagedList() {
          isEmpty.setValue(false);
       	//创建LiveData时需要Factory和Config作为参数
          return new LivePagedListBuilder<>(mFactory, getConfig())
                  .setBoundaryCallback(new PagingBoundaryCallback<>(hasMore, isEmpty))
                  .build();
      }
  ```


```java
public class PagingDataSourceFactory<T> extends DataSource.Factory<Integer, T> {
    
    private MutableLiveData<PagingDataSource<T>> mDataSource = new MutableLiveData<>();

    public PagingDataSourceFactory(PagingDataSource<T> dataSource) {
        mDataSource.setValue(dataSource);
    }

    @Override
    public DataSource<Integer, T> create() {
        return mDataSource.getValue();
    }

    public MutableLiveData<PagingDataSource<T>> getDataSource() {
        return mDataSource;
    }
}
```



#### 3、paging

##### PagingListAdapter

再一会实例中讲解



##### PagingViewModel

也是同上





### 三、使用

```java
.
├── PersonalConstants.java
├── PersonalFragment.java
├── data
│   ├── Jade.java
│   └── JadeRepository.java				//数据仓库
└── view
    ├── PersonalListActivity.java
    ├── PersonalListAdapter.java
    ├── PersonalListViewModel.java		//ViewModel
    └── holder
        ├── EvaluateViewHolder.java
        └── OrderViewHolder.java
```

#### 1、需要类

> 以personal为例

##### 数据仓库Repository

- JadeRepository implements PagingRepository\<Jade>

```java
public class JadeRepository implements PagingRepository<Jade> {

    @Override
    public void getDataList(int offset, int count, PagingCallBack<Jade> callBack) {
        List<Jade> list = new ArrayList<>();

        for (int i = 0; i < count; i++) {
            Jade jade = new Jade();
            jade.setId(offset + i);
            jade.setPrice("123,456");
            jade.setTitle("商品商品商品商品商品" + jade.getId());
            jade.setCover("https://ss1.baidu.com/6ONXsjip0QIZ8tyhnq/it/u=2747464534,3370638099&fm=58&bpow=640&bpoh=640");
            list.add(jade);
        }
        callBack.onSuccess(list);
    }
}
```

##### ViewModel

```java
public class PersonalListViewModel extends PagingViewModel<Jade> {
    @Override
    protected PagingDataSource<Jade> getDataSource() {
        return new PagingDataSource<>(this, new JadeRepository());
    }
}
```



#### 2、实例步骤

##### ++1++先看PersonalListActivity

`private PersonalListViewModel mViewModel;`，然后在`initList()`方法中订阅加载的状态变化

```java
 private void initList() {
        PersonalListAdapter adapter = new PersonalListAdapter(this, mListType);
        adapter.setSupportEmptyView(true);
        adapter.setSupportStatusView(true);
        adapter.setViewModel(mViewModel);

        mRecyclerView.setAdapter(adapter);
        mRecyclerView.setLayoutManager(new PagingLayoutManager(this));
        mRecyclerView.addItemDecoration(new DividerItemDecoration(this, DividerItemDecoration.VERTICAL));

        mRefreshView.setRefreshing(true);
        mRefreshView.setOnRefreshListener(() -> {
            mRefreshView.setRefreshing(true);
            adapter.refresh();
        });
     	/**
     	*关键代码，订阅加载状态
     	*/
        mViewModel.getRefreshState().observe(this, networkStatus -> {
            if (networkStatus != null && !networkStatus.isLoading()) {
                mRefreshView.setRefreshing(false);
            }
        });
    }
```

##### ++2++PersonalListAdapter

```java
public class PersonalListAdapter extends PagingListAdapter<Jade> {

    public static final int TYPE_ORDER_BUY = 0; // 我的订单
    public static final int TYPE_ORDER_SELL = 1; // 我的订单
    public static final int TYPE_RELEASE = 2; // 我的发布
    public static final int TYPE_EVALUATE = 3;// 我的评估
    public static final int TYPE_FOLLOW = 4;// 我的关注

    private int mAdapterType;

    protected PersonalListAdapter(LifecycleOwner owner, int type) {
        //调用父类的方法，用于比较数据源中的数据不同
        super(DIFF_CALLBACK, owner);
        mAdapterType = type;
    }

	//.....根据viewType返回不同的Holder

    private static final DiffUtil.ItemCallback<Jade> DIFF_CALLBACK = new DiffUtil.ItemCallback<Jade>() {
        @Override
        public boolean areItemsTheSame(@NonNull Jade jade, @NonNull Jade t1) {
            return t1.getId() == jade.getId();
        }

        @Override
        public boolean areContentsTheSame(@NonNull Jade jade, @NonNull Jade t1) {
            return t1.equals(jade);
        }
    };
}
```

##### ++3++需要再看一下父类的PagingListAdapter

- mViewModel

- 两种网络状态（加载中、刷新）

- 几个是否

- 几个Observer变量，用来监听viewmodel中的各种mutablelivedata

  - `protected MutableLiveData<NetworkStatus> mLoadingState = new MutableLiveData<>();`

  - `mViewModel.getLoadingState().observe(mLifecycleOwner, mLoadingObserver);`

```java
public abstract class PagingListAdapter<T> extends PagedListAdapter<T, RecyclerView.ViewHolder> {

    private static final int TYPE_LOADING = 1001; // 正在加载更多
    private static final int TYPE_ERROR = 1002; // 加载更多失败
    private static final int TYPE_EMPTY = 1003; // 空列表

    private PagingViewModel<T> mViewModel;
    private LifecycleOwner mLifecycleOwner;

    private NetworkStatus.Status mLoadingStatus = null;
    private NetworkStatus.Status mRefreshStatus = null;

    private boolean isEmpty = false;
    private boolean hasMore = false;
    private boolean isLoadingShowing = false;
    private boolean supportEmptyView = false;
    private boolean supportStatusView = false;

    // 监听数据列表变化
    private final Observer<PagedList<T>> mDataListObserver = this::submitList;

    // 监听LoadMore状态
    private final Observer<NetworkStatus> mLoadingObserver = networkStatus -> {
        if (networkStatus == null) {
            return;
        }
        setLoadingStatus(networkStatus.getStatus());
    };

    // 监听Refresh状态
    private final Observer<NetworkStatus> mRefreshObserver = networkStatus -> {
        if (networkStatus == null) {
            return;
        }
        setRefreshStatus(networkStatus.getStatus());
    };

    // 监听是否为空列表
    private final Observer<Boolean> mEmptyObserver = isEmpty -> {
        if (isEmpty == null) {
            return;
        }
        setEmpty(isEmpty);
    };

    // 监听是否有更多
    private final Observer<Boolean> mHasMoreObserver = hasMore -> {
        if (hasMore == null) {
            return;
        }
        setHasMore(hasMore);
    };


    protected PagingListAdapter(@NonNull DiffUtil.ItemCallback<T> diffCallback,
                                LifecycleOwner owner) {
        super(diffCallback);
        mLifecycleOwner = owner;
    }

    public void setViewModel(PagingViewModel<T> viewModel) {
        if (mViewModel != null) {
            mViewModel.getLoadingState().removeObserver(mLoadingObserver);
            mViewModel.getRefreshState().removeObserver(mRefreshObserver);
            mViewModel.getDataList().removeObserver(mDataListObserver);
            mViewModel.hasMore().removeObserver(mHasMoreObserver);
            mViewModel.isEmpty().removeObserver(mEmptyObserver);
        }
        mViewModel = viewModel;
        mViewModel.getLoadingState().observe(mLifecycleOwner, mLoadingObserver);
        mViewModel.getRefreshState().observe(mLifecycleOwner, mRefreshObserver);
        mViewModel.getDataList().observe(mLifecycleOwner, mDataListObserver);
        mViewModel.hasMore().observe(mLifecycleOwner, mHasMoreObserver);
        mViewModel.isEmpty().observe(mLifecycleOwner, mEmptyObserver);
    }
    

    private int getLoadingStatus() {
        if (isAtEnd()) {
            return TYPE_EMPTY;
        } else if (isLoadingError()) {
            return TYPE_ERROR;
        } else if (isLoading()) {
            return TYPE_LOADING;
        }
        throw new RuntimeException("unknown loading status type");
    }


    protected int getDataItemViewType(int position) {
        return 0;
    }


    @Override
    public int getItemCount() {
        int empty = showEmptyView() ? 1 : 0;
        int dataItemCount = getDataItemCount();
        int status = showLoadingStatusView() ? 1 : 0;
        return dataItemCount + status + empty;
    }

    public int getDataItemCount() {
        return super.getItemCount();
    }



    /**
     * 是否支持展示全屏的EmptyView
     */
    public boolean isSupportEmptyView() {
        return supportEmptyView;
    }

    /**
     * 设置是否支持展示全屏的EmptyView
     */
    public void setSupportEmptyView(boolean support) {
        this.supportEmptyView = support;
        notifyDataSetChanged();
    }

    /**
     * 当前数据列表是否为空
     */
    public boolean isEmpty() {
        return isEmpty && getDataItemCount() == 0;
    }

    /**
     * 动态设置是否为空列表状态
     */
    public void setEmpty(boolean empty) {
        if (isEmpty == empty) {
            return;
        }
        this.isEmpty = empty;
        if (isEmpty()) {
            notifyItemInserted(0);
        } else {
            notifyItemRemoved(0);
        }
    }

    /**
     * 是否展示EmptyView
     */
    private boolean showEmptyView() {
        return isSupportEmptyView() && isEmpty();
    }


    public boolean isSupportStatusView() {
        return supportStatusView;
    }

    public void setSupportStatusView(boolean support) {
        this.supportStatusView = support;
        notifyDataSetChanged();
    }

    private void setLoadingStatus(NetworkStatus.Status status) {
        // 获取旧的显示状态
        boolean oldStatus = isLoadingShowing;
        // 更新加载状态
        this.mLoadingStatus = status;
        // 获取新的网络状态
        boolean newStatus = showLoadingStatusView();
        if (!oldStatus && newStatus) {
            // 展示Status
            notifyItemInserted(getDataItemCount());
        } else if (oldStatus && !newStatus) {
            // 消除Status
            notifyItemRemoved(getDataItemCount());
        } else {
            notifyDataSetChanged();
        }
        isLoadingShowing = newStatus;
    }

    // 是否展示加载状态
    public boolean showLoadingStatusView() {
        if (!isSupportStatusView()) {
            return false;
        }
        return isLoading() || isLoadingError() || isAtEnd();
    }

    public boolean isLoadingError() {
        return mLoadingStatus == NetworkStatus.Status.FAILED;
    }

    public boolean isLoading() {
        return mLoadingStatus == NetworkStatus.Status.RUNNING;
    }

    public boolean isAtEnd() {
        // 当前列表不为空且暂无更多
        return !isEmpty() && !hasMore();
    }

    public void setHasMore(boolean hasMore) {
        this.hasMore = hasMore;
        notifyDataSetChanged();
    }

    public boolean hasMore() {
        return hasMore;
    }


    public void refresh() {
        if (mViewModel != null) {
            mViewModel.refresh();
        }
    }

    public void setRefreshStatus(NetworkStatus.Status status) {
        this.mRefreshStatus = status;
        notifyDataSetChanged();
    }

    public boolean isRefreshError() {
        return mRefreshStatus == NetworkStatus.Status.FAILED;
    }

    public boolean isRefreshing() {
        return mRefreshStatus == NetworkStatus.Status.RUNNING;
    }

}
```

##### ++4++看一下PersonalListViewModel

- 继承自PagingViewModel
- Jade实体类作为指定类型

```java
public class PersonalListViewModel extends PagingViewModel<Jade> {
    @Override
    protected PagingDataSource<Jade> getDataSource() {
        return new PagingDataSource<>(this, new JadeRepository());
    }
}
```



##### ++5++看一下父类PagingViewModel

- mDataList

- MultableLiveData

  - mRefreshState
  - mLoadingState
  - isEmpty
  - hasMore

- PublishSubject

  - refresh和retry

- PagingDataSourceFactory

  构造函数中进行factory的初始化，factory的初始化需要实现类的getDataSource作为参数

```java
public abstract class PagingViewModel<T> extends ViewModel {

    public static final int INIT_LOAD_SIZE = 20;

    public static final int PAGE_SIZE = 20;

    private LiveData<PagedList<T>> mDataList;

    private MutableLiveData<NetworkStatus> mRefreshState = new MutableLiveData<>();

    protected MutableLiveData<NetworkStatus> mLoadingState = new MutableLiveData<>();

    protected MutableLiveData<Boolean> isEmpty = new MutableLiveData<>();

    protected MutableLiveData<Boolean> hasMore = new MutableLiveData<>();

    private PublishSubject<String> refresh = PublishSubject.create();

    private PublishSubject<String> retry = PublishSubject.create();

    private PagingDataSourceFactory<T> mFactory;


    public PagingViewModel() {
        if (mDataList == null) {
            mFactory = new PagingDataSourceFactory<>(getDataSource());
            mDataList = buildPagedList();
        }
    }

    private LiveData<PagedList<T>> buildPagedList() {
        isEmpty.setValue(false);
        return new LivePagedListBuilder<>(mFactory, getConfig())
                .setBoundaryCallback(new PagingBoundaryCallback<>(hasMore, isEmpty))
                .build();
    }

    public LiveData<PagedList<T>> getDataList() {
        return mDataList;
    }

    public MutableLiveData<NetworkStatus> getLoadingState() {
        return mLoadingState;
    }

    public MutableLiveData<NetworkStatus> getRefreshState() {
        return mRefreshState;
    }

    public MutableLiveData<Boolean> isEmpty() {
        return isEmpty;
    }

    public MutableLiveData<Boolean> hasMore() {
        return hasMore;
    }

    public void retry() {
        retry.onNext("retry");
    }

    public void refresh() {
//        refresh.onNext("refresh");
        mRefreshState.postValue(NetworkStatus.success());
    }

    public PublishSubject<String> registerRefresh() {
        return refresh;
    }

    public PublishSubject<String> registerRetry() {
        return retry;
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

    protected abstract PagingDataSource<T> getDataSource();
}
```



##### ++6++进行一次数据获取请求

**用一段话概述：**

1. 从`PersonalListActivity`开始，首先`mViewModel = ViewModelProviders.of(this).get(PersonalListViewModel.class);`进行viewmodel的初始化,父类`PagingViewModel`的初始化中会调用子类实现的`getDatgaSource`方法，但是`getDataSource`返回的`PagingDataSource`构造函数中需要new一个`JadeRepository`实例

2. 在`PagingDataSource`的`loadInitial`和`loadRange`方法中会调用在**1**构造函数传进来的`PagingRepository实例`的`getDataList`**方法用来获取数据**。

3. 将2获得的数据通过`callback.onResult(list);`，**此时数据到了DataSource中**

4. 在`ViewModel`的构造函数中将**DataSource传给Factory**，`mFactory = new PagingDataSourceFactory<>(getDataSource());`

5. 在`ViewModel`中构造`LiveData`**至此构造出LiveData**

   ```java
    return new LivePagedListBuilder<>(mFactory, getConfig())
                   .setBoundaryCallback(new PagingBoundaryCallback<>(hasMore, isEmpty))
                   .build();
   ```

6. 会到`Activity`的init方法，给Adapter设置`ViewModel`**将ViewModel与UI进行关联**

7. 初始化的时候`setViewModel`方法设置viewModel进入到`PersonalListAdapter`中，进入父类`PagingListAdapter`，在`setViewmodel`方法中给`viewmodel`设置监听，监听`viewmodel`的数据及状态变化。

8. `mViewModel.getDataList().observe(mLifecycleOwner, mDataListObserver);`



### 四、总结

**核心思想是需要获取LiveData，但是LiveData的构造需要DataSourceFactory，DataSourceFactory的构造又需要DataSource，DataSource的构造需要Repository，在Repository中进行数据的获取即可。然后通过回调接口将数据存到DataSource中，进一步到通过DataSourceFactory进行LiveData的构造。**



#### 图

![谷歌视频截图](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/vRLQqpMuDHKsMZ7CvdWy8xm7xbKb*Bg6lvSPFVBgHZ0!/r/dL4AAAAAAAAA)

![原理图](https://upload-images.jianshu.io/upload_images/7293029-27facf0a399c66b8.gif?imageMogr2/auto-orient/strip%7CimageView2/2/w/800)

- 加载单一数据源

![paging加载单一数据](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/xOXwBGp4eKPQ14KbLXQVf50Kww9K9PChihzj*qWalP0!/r/dLYAAAAAAAAA)

- 加载多个数据源

![paging加载多个数据源流程](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Y4q1LZyKkmsyyUo0L8uTTHDjqRE1Ki4g1IZWlTDV4G4!/r/dL8AAAAAAAAA)

