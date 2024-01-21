[TOC]

**总共三个类：**

- BaseViewHolder		             //ViewHolder基类
- FooterRecyclerViewAdapter        //带Footer的Adapter基类
- RecyclerScrollListener                 //滑动监听事件



## 1、使用方法

```java
//1界面 implements一些接口
  OnItemViewClickListener, SwipeRefreshLayout.OnRefreshListener, RecyclerScrollListener
        .LoadMoreListener
    - Item的点击监听接口
    - SwipRefreshLaout.OnRefreshListener的下拉刷新接口
    - RecyclerScrollListener.LoadMoreListener====RecyclerView的上拉加载更多接口

//2声明变量
	(xml里的)
    - SwipeRefreshLayout mRefreshLayout;
	- RecyclerView mRecyclerTaskList;

	- TaskListRecyclerAdapter mRecyclerAdapter;
	- RecyclerScrollListener mScrollListener;

//3初始化.....
 private void initRecyclerView() {

        mRecyclerAdapter = new TaskListRecyclerAdapter(new ArrayList<BaseTaskModel>(), this
                , R.layout.item_recycler_task, TaskListRecyclerAdapter.BTN_ACCEPT);
        //设置上拉加载更多
        mRecyclerAdapter.setWithFooter(true);
        //内部控件点击事件
        mRecyclerAdapter.setOnItemViewClickListener(this);
        //设置adapter
        mRecyclerTaskList.setAdapter(mRecyclerAdapter);
        mRecyclerTaskList.setLayoutManager(new LinearLayoutManager(this));

        //设置滑动事件监听
        mScrollListener = new RecyclerScrollListener(mRecyclerTaskList, mRecyclerAdapter);
     
        //动态显示悬浮按钮
        mScrollListener.setFloatingButton(mBtnReleaseTask);
     
        //添加加载更多事件监听
        mScrollListener.setLoadMoreListener(this);

        //添加下拉刷新事件监听
        mRefreshLayout.setOnRefreshListener(this);
       	 		                  mRefreshLayout.setColorSchemeColors(getResources().getColor(R.color.txt_blue));
    }
```



## 2、BaseViewHolder

自己封装了一个ViewHolder，然后继承FooterRecyclerViewAdapter的时候就不需要自己在重新写ViewHolder了。只适合单一Type的情况，多Type就不适用了。

- 继承自RecyclerView.ViewHolder
- 构造函数里多了context

```java
public class BaseViewHolder extends RecyclerView.ViewHolder {

    private final SparseArray<View> mViewSparseArray;
    private Context mContext;
    private View mItemView;

    public BaseViewHolder(Context context, View itemView) {
        super(itemView);
        this.mContext = context;
        mItemView = itemView;
        mViewSparseArray = new SparseArray<>();
    }

    /**
     * 从布局获取泛型view
     *
     * @param viewId
     * @param <T>
     * @return
     */
    @SuppressWarnings("unchecked")
    protected <T extends View> T findViewById(@IdRes int viewId) {
        //从已经缓存的ViewMap中查找
        View view = mViewSparseArray.get(viewId);
        //如果没有缓存
        if (view == null) {
            //获取实例并加入缓存
            view = mItemView.findViewById(viewId);
            mViewSparseArray.append(viewId, view);
        }
        return (T) view;
    }

    /**
     * 设置TextView类型
     *
     * @param viewId
     * @param value
     * @return
     */
    public BaseViewHolder setText(@IdRes int viewId, @StringRes int value) {
        return setText(viewId, mContext.getString(value));
    }

    public BaseViewHolder setText(@IdRes int viewId, String value) {
        TextView textView = findViewById(viewId);
        textView.setText(value == null ? mContext.getString(R.string.empty) : value);
        return this;
    }

    /**
     * 设置ImageView的url
     *
     * @param viewId
     * @param value
     * @return
     */
    public BaseViewHolder setImgUrl(@IdRes int viewId, String value) {
        ImageView imageView = findViewById(viewId);
        if (value != null) {
            Glide.with(mContext).load(value).into(imageView);
        }
        return this;
    }

    /**
     * 设置ImageView的url
     *
     * @param viewId
     * @return
     */
    public BaseViewHolder setImgRes(@IdRes int viewId, @DrawableRes int drawableId) {
        ImageView imageView = findViewById(viewId);
        imageView.setImageResource(drawableId);
        return this;
    }

    /**
     * 设置控件是否可见
     *
     * @param viewId
     * @param isVisible
     * @return
     */
    public BaseViewHolder setVisible(@IdRes int viewId, boolean isVisible) {
        findViewById(viewId).setVisibility(isVisible ? View.VISIBLE : View.INVISIBLE);
        return this;
    }

    /**
     * 设置控件是否消失(不保留空间)
     *
     * @return
     */
    public BaseViewHolder setGone(@IdRes int viewId) {
        findViewById(viewId).setVisibility(View.GONE);
        return this;
    }

    /**
     * 获取item的View
     *
     * @return
     */
    public View getItemView() {
        return mItemView;
    }

    /**
     * 获取item内部的view
     *
     * @param viewId
     * @return
     */
    public View getViewById(@IdRes int viewId) {
        return findViewById(viewId);
    }

    /**
     * 获取当前viewHolder位置
     *
     * @return
     */
    public int getViewHolderPosition() {
        return getAdapterPosition();
    }
}
```



## 3、FooterRecyclerViewAdapter

总共分了三个块

- Adapter基础块

  - 通过Data泛型传入不同类型的数据

  - abstract void bindView(BaseViewHolder viewHolder, Data item); 子类重写此方法就行

- 数据操作块

  - get、add、remove......等等

- Footer

  -  setWithFooter(boolean withFooter)   设置此Adapter是否有footer
  - showFooterVisibility(boolean isShow)  设置是否显示Footer
  - FooterViewHolder

```java
public abstract class FooterRecyclerViewAdapter<Data> extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    public static final int ITEM_NORMAL = 1000;
    public static final int ITEM_FOOTER = 1001;
    
    protected List<Data> mDataList;
    protected Context mContext;
    protected LayoutInflater mInflater;
    protected int mItemLayoutId;
    private boolean mWithFooter = false;
    private FooterViewHolder mFooterViewHolder;
    private OnItemClickListener mOnItemClickListener;

    public FooterRecyclerViewAdapter(List<Data> dataList, Context context, @LayoutRes int itemLayoutId) {
        mDataList = dataList;
        mContext = context;
        mInflater = LayoutInflater.from(mContext);
        mItemLayoutId = itemLayoutId;
    }

    @Override
    public int getItemCount() {
        return mWithFooter ? mDataList.size() + 1 : mDataList.size();
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {

        //如果是footer类型渲染底部布局
        if (viewType == ITEM_FOOTER) {
            View footerView = mInflater.inflate(R.layout.item_recyclerview_footer, parent, false);
            mFooterViewHolder = new FooterViewHolder(footerView);
            return mFooterViewHolder;
        }

        final View view = mInflater.inflate(mItemLayoutId, parent, false);
        BaseViewHolder viewHolder = new BaseViewHolder(mContext, view);
        final int position = viewHolder.getAdapterPosition();
        if (mOnItemClickListener != null) {
            viewHolder.getItemView().setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    mOnItemClickListener.onItemClick(view, mDataList.get(position), position);
                }
            });
        }
        return viewHolder;
    }

    @Override
    public int getItemViewType(int position) {


        //如果需要底部footer
        if (mWithFooter) {

            //如果是footerView
            if (position >= mDataList.size()) {
                return ITEM_FOOTER;
            } else {
                return ITEM_NORMAL;
            }
        } else {
            return ITEM_NORMAL;
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
        if (holder.getItemViewType() == ITEM_NORMAL) {
            bindView((BaseViewHolder) holder, mDataList.get(position));
        } else if (holder.getItemViewType() == ITEM_FOOTER) {
            /**
             *false?true
             * 先不显示，拉倒底部再显示
             */
            ((FooterViewHolder) holder).setLoadVisibility(false);
        }
    }

    /**
     * 绑定item中的view和数据
     *
     * @param viewHolder
     * @param item
     */
    protected abstract void bindView(BaseViewHolder viewHolder, Data item);

    /**
     * 重新设置数据
     *
     * @param data 数据
     */
    public void reSetDataList(Collection<Data> data) {
        this.mDataList.clear();
        this.mDataList.addAll(data);
        notifyDataSetChanged();
    }

    /**
     * 获取当前列表的数据
     */
    public List<Data> getDataList() {
        return this.mDataList;
    }

    /**
     * 添加数据
     *
     * @param data
     */
    public void addItem(Data data) {
        mDataList.add(data);
        notifyDataSetChanged();
    }

    /**
     * @param collection
     */
    public void addItems(Collection<Data> collection) {
        mDataList.addAll(collection);
        notifyDataSetChanged();
    }

    /**
     * 移除数据
     *
     * @param data 移除的数据
     */
    public void removeItem(Data data) {
        this.mDataList.remove(data);
        notifyDataSetChanged();
    }

    /**
     * 移除数据
     */
    public void removeItem(int position) {

        mDataList.remove(position);
        //该方法不会使position及其之后位置的itemView重新onBindViewHolder
        notifyItemRemoved(position);
        //所以需要从position到列表末尾进行数据刷新
        if (position != mDataList.size()) {
            notifyItemRangeChanged(position, mDataList.size() - position);
        }
    }

    /**
     * 清除全部数据
     */
    public void removeAllItem() {
        mDataList.clear();
        notifyDataSetChanged();
    }

    /**
     * 设置是否需要上拉加载更多
     *
     * @param withFooter
     */
    public void setWithFooter(boolean withFooter) {
        mWithFooter = withFooter;
    }

    public interface OnItemClickListener<Data> {

        /**
         * 点击事件回调
         *
         * @param view
         * @param position
         */
        public void onItemClick(View view, Data data, int position);
    }

    public void setOnItemClickListener(OnItemClickListener onItemClickListener) {
        mOnItemClickListener = onItemClickListener;
    }

    public void showFooterVisibility(boolean isShow) {
        mFooterViewHolder.setLoadVisibility(isShow);
    }

    /**
     * 上拉加载更多底部ViewHolder
     */
    public class FooterViewHolder extends RecyclerView.ViewHolder {
        ProgressBar mFooterProgressBar;
        TextView mTextView;

        public FooterViewHolder(View itemView) {
            super(itemView);
            mFooterProgressBar = (ProgressBar) itemView.findViewById(R.id.progress_footer);
            mTextView=(TextView) itemView.findViewById(R.id.tv_loading);
        }

        void setLoadVisibility(boolean show) {
            mFooterProgressBar.setVisibility(show ? View.VISIBLE : View.GONE);
            mTextView.setVisibility(show ? View.VISIBLE : View.GONE);
        }
    }
}
```



## 4、RecyclerScrollListener

- 构造函数需要传进来RecyclerView和响应的Adapter
  - recyclerView.addOnScrollListener(this);
- 重写继承自RecyclerView.OnScrollListener里的方法
  - onScrollStateChanged(RecyclerView recyclerView, int newState)
    - 判断是否正在加载中
    - 然后判断是否达到分页数量并且滚动到最后
- 是否正在完成接口
- 设置LoadMore Item为加载完成状态, 上拉加载更多完成时调用

```java
package cn.ck.xjbl.ui.recyclerview.taskgo_recyclerview.base;

import android.support.design.widget.FloatingActionButton;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;

/**
 * @author fhyPayaso
 * @since 2018/4/30 on 下午1:20
 * fhyPayaso@qq.com
 */
public class RecyclerScrollListener extends RecyclerView.OnScrollListener {


    private FloatingActionButton mFloatingButton;  //被监听的悬浮按钮
    private RecyclerView.LayoutManager mLayoutManager;
    private FooterRecyclerViewAdapter mFooterRecyclerViewAdapter;
    private LoadMoreListener mListener;

    /**
     * 分页数量
     */
    private int mItemNumPerPage = 20;

    /**
     * 是否正在加载中
     */
    private boolean mLoading = false;
    /**
     * 上拉刷新功能是否开启
     */
    private boolean mIsSwipeToLoadEnabled = true;

    /**
     * 构造函数。。。。。
     *
     * @param recyclerView RecyclerView
     * @param footerRecyclerViewAdapter 适配器
     */
    public RecyclerScrollListener(RecyclerView recyclerView, FooterRecyclerViewAdapter footerRecyclerViewAdapter) {
        mLayoutManager = recyclerView.getLayoutManager();
        mFooterRecyclerViewAdapter = footerRecyclerViewAdapter;
        recyclerView.addOnScrollListener(this);
    }

    /**
     * 判断到达底部？
     *
     * @param recyclerView
     * @param newState
     */
    @Override
    public void onScrollStateChanged(RecyclerView recyclerView, int newState) {
        super.onScrollStateChanged(recyclerView, newState);

        if (!mLoading && newState == RecyclerView.SCROLL_STATE_IDLE) {

            LinearLayoutManager linearLayoutManager = (LinearLayoutManager) mLayoutManager;
            int lastCompletePosition = linearLayoutManager.findLastCompletelyVisibleItemPosition();
            int totalItemCount = linearLayoutManager.getItemCount();

            //达到需要最大分页的数目并且滚动到最后
            if (totalItemCount != 0
                    && mFooterRecyclerViewAdapter.getDataList().size() % mItemNumPerPage == 0
                    && mFooterRecyclerViewAdapter.getDataList().size() != 0
                    && lastCompletePosition == (totalItemCount - 1)) {

                mLoading = true;
                mFooterRecyclerViewAdapter.showFooterVisibility(true);
                if (mListener != null) {
                    mListener.onLoadMore();
                }
            }
        }
    }


    /**
     * 动态显示悬浮按钮
     *
     * @param recyclerView
     * @param dx
     * @param dy
     */
    @Override
    public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
        super.onScrolled(recyclerView, dx, dy);
        if (mFloatingButton != null) {
            if (dy < 0) {
                //向下滑动显示按钮
                mFloatingButton.show();
            } else if (dy > 0) {
                //向上滑动隐藏按钮
                mFloatingButton.hide();
            }
        }
    }


    /**
     * 设置分页数量
     *
     * @param itemNumPerPage 默认为20
     */
    public void setItemNumPerPage(int itemNumPerPage) {
        mItemNumPerPage = itemNumPerPage;
    }

    /**
     * 设置需要监听的悬浮按钮
     *
     * @param floatingButton
     */
    public void setFloatingButton(FloatingActionButton floatingButton) {
        mFloatingButton = floatingButton;
    }

    /**
     * 设置LoadMore Item为加载完成状态, 上拉加载更多完成时调用
     */
    public void setLoadMoreFinish() {
        mLoading = false;
        mFooterRecyclerViewAdapter.showFooterVisibility(false);
    }

    /**
     * 判断是否正在加载
     *
     * @return
     */
    public boolean isLoading() {
        return mLoading;
    }

    public void setLoadMoreListener(LoadMoreListener loadMoreListener) {
        mListener = loadMoreListener;
    }

    public interface LoadMoreListener {
        void onLoadMore();
    }
}

```

