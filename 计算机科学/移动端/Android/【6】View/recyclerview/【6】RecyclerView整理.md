[TOC]

## 一、目录

- 判断是否滑动到最后 / 前的 Item
- 添加 HeaderView 和 FooterView
- ScrollView 嵌套情况下的问题
- 数据源刷新的问题
- item 局部刷新
- [平滑的滑动置顶](https://www.jianshu.com/p/e8117855c6a0)
- ItemTouchHelper

参考 ：
 [Android RecyclerView 使用完全解析 体验艺术般的控件](http://blog.csdn.net/lmj623565791/article/details/45059587)
 [深入理解 RecyclerView 系列之一：ItemDecoration](https://blog.piasy.com/2016/03/26/Insight-Android-RecyclerView-ItemDecoration/)

开篇先说明一下，现在 RecyclerView 想必都很熟悉，我在项目里也对这个控件做过一些封装、遇到过一些问题，这里仅做记录。因为这次时间还是比较多，所以打算深入一点研究学习、内容也比较多所以分出了几篇，链接在上面。



## 二、简单初始化

```java
//设置布局管理器
mRecyclerView.setLayoutManager(layout);
//设置adapter
mRecyclerView.setAdapter(adapter)
//设置Item增加、移除动画
mRecyclerView.setItemAnimator(new DefaultItemAnimator());
//添加分割线
mRecyclerView.addItemDecoration(new DividerItemDecoration(
                getActivity(), DividerItemDecoration.HORIZONTAL_LIST));
```

-  **监听 Item 的点击事件**
   因为 RecyclerView 没直接的事件可以监听，需要自己去设置，
   我选择在 RecyclerView.Adapter 中设置：

```java
//在继承了 RecyclerView.Adapter  的内部类中
@Override
public void onBindViewHolder(ViewHolder holder, final int position) {
    if (holder instanceof ViewHolder){
        //设置 TextView
        holder.textView.setText(data.get(position).getTitle());
        //设置 Item 的点击事件
        holder.itemLayout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                //自己定义的一个方法，将 position 传出去
                onItemClick(position);
            }
        });
    }
}                                                                 
//重写的 ViewHolder，使用了 ButterKnife，但逻辑上没有变化
class ViewHolder extends RecyclerView.ViewHolder{

    @BindView(R.id.news_item_title)
    TextView newsTitle;
    @BindView(R.id.news_item_img)
    ImageView newsImg;
    @BindView(R.id.news_item_layout)
    LinearLayout newsLayout;

    public ViewHolder(View itemView) {
        super(itemView);
        ButterKnife.bind(this,itemView);
    }
}
```

Item 的布局:

```java
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
              android:orientation="vertical"
              android:layout_width="match_parent"
              android:layout_height="wrap_content">

    <LinearLayout
        android:id="@+id/news_item_layout"
        android:layout_width="match_parent"
        android:layout_height="110dp"
        android:layout_marginBottom="2.5dp"
        android:layout_marginTop="2.5dp"
        android:elevation="4dp"
        android:background="@drawable/news_item_bg">
        <TextView
            android:textColor="#000"
            android:id="@+id/news_item_title"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:textSize="17dip"
            android:layout_marginTop="10dp"
            android:layout_marginBottom="5dp"
            />
    </LinearLayout>
</LinearLayout>
```

还有一种方法就是使用 addOnItemTouchListener，通过触摸坐标来判断
 可以参考：  [RecyclerView 无法添加 onItemClickListener 最佳的高效解决方案](http://blog.csdn.net/liaoinstan/article/details/51200600)



## 三、监听滑动

```java
//OnScrollListener  是继承自 RecyclerView.OnScrollListener 的内部类，见下文
mNewsRecyclerView.addOnScrollListener(new OnScrollListener());
```

监听是否滑动到底部
 继承 RecyclerView.OnScrollListener，
 在 `onScrolled` 获取 RecyclerView 的最后一个 Item 的位置，
 在 `onScrollStateChanged` 判断当前是否滑动到最后一个 Item。

```java
/**
  * 监听 RecyclerView 判断是否滑到最后一个 Item
  */
class OnScrollListener extends RecyclerView.OnScrollListener{

  private int lastVisibleItem;

  /**
    * 判断是否滑动了最后一个 Item
    * @param recyclerView
    * @param newState
    */
  @Override
  public void onScrollStateChanged(RecyclerView recyclerView, int newState) {
   super.onScrollStateChanged(recyclerView, newState);
   if( (newState == RecyclerView.SCROLL_STATE_IDLE)
     &&(lastVisibleItem + 1 == mRecyclerViewAdapter.getItemCount())){
    
     //newState 为 0：当前屏幕停止滚动；
     //1：屏幕在滚动且用户仍在触碰或手指还在屏幕上；
     //2：随用户的操作，屏幕上产生的惯性滑动；
     
    //在这里执行刷新/加载更多的操作
    loadMore();
}

   /**
     * 在这里获取到 RecyclerView 的最后一个 Item 的位置
     * @param recyclerView
     * @param dx
     * @param dy
     */
   @Override
   public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
    super.onScrolled(recyclerView, dx, dy);
    lastVisibleItem = mLinearLayoutManager.findLastVisibleItemPosition();
  }
}
```

特别的是，当 RecyclerView 实现了**瀑布流**，
 就是使用了 **StaggeredGridLayoutManager** 的时候，因为 Item 是交错的，
 所以不能使用上面的方法，而是要判断 `findLastVisibleItemPosition` 返回的数组的最大值，上面的 onScrolled 方法要修改为：

```java
@Override
public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
    super.onScrolled(recyclerView, dx, dy);

    int [] lasPositions = new int[mStaggeredGridLayoutManager.getSpanCount()];
    int [] all = mStaggeredGridLayoutManager
                      .findLastVisibleItemPositions(lasPositions);
    lastVisibleItem = findMax(all);

}

private int findMax(int[] lastPositions) {
    int max = lastPositions[0];
    for (int value : lastPositions) {
        if (value > max) {
            max = value;
        }
    }
    return max;
}
```



## 四、添加 HeaderView 和 FooterView

- 在 ViewHolder 中添加 HeaderView 和 FooterView

```java
//当前 item 的类型,0 为头布局,1 为底部布局
public static final int TYPE_HEADER = 0;
public static final int TYPE_FOOTER = 1;
public static final int TYPE_NORMAL = 2;
//
private View mHeaderView;
private View mFooterView;
//
public void addHeaderView(View mHeaderView) {
    this.mHeaderView = mHeaderView;
}
public void addFooterView(View mFooterView) {
    this.mFooterView = mFooterView;
}
```

- 通过设置 ItemViewType 来区分 HeaderView 和 FooterView

```java
/**
 * 为每个 Item 设置类型
 * 如果头布局不为空,则将第一个 Item 设为头布局
 * @param position
 * @return
 */
@Override
public int getItemViewType(int position) {
    if (mHeaderView == null && mFooterView == null)
        return TYPE_NORMAL;
    if (mHeaderView != null && position == 0)
        return TYPE_HEADER;
    if (mFooterView != null && position == getItemCount() - 1)
        return TYPE_FOOTER;

    return TYPE_NORMAL;
}
```

如果 mHeaderView 不为空，则将第一个 ItemView 设为 HeaderView，
 如果 mFooterView 不为空，将最后一个 ItemView  设为 FooterView。

- 创建 ItemView

```java
/**
 * 创建布局的时候如果发现是头布局直接返回
 * @param parent
 * @param viewType
 * @return
 */
@Override
public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
    if (mHeaderView != null && viewType == TYPE_HEADER){
        return new ViewHolder(mHeaderView);
    }
    if (mFooterView != null && viewType == TYPE_FOOTER){
        return new ViewHolder(mFooterView);
    }
    View v = LayoutInflater.from(getActivity())
            .inflate(R.layout.news_list_item,parent,false);
    ViewHolder viewHolder = new ViewHolder(v);
    return viewHolder;
}
/**
 * 只有当前的 Item 的类型是 normal 时才执行
 *  realPosition = position -1 是因为 0 被头布局占用了
 * @param holder
 * @param position
 */
@Override
public void onBindViewHolder(ViewHolder holder, final int position) {
    final int realPosition;
    if (mHeaderView != null)
        realPosition = position -1;
    else
        realPosition = position;

    if (getItemViewType(position) == TYPE_NORMAL){
        if (holder instanceof ViewHolder){
            holder.newsTitle.setText(data.get(realPosition).getTitle());
            Glide.with(getActivity()).load(data.get(realPosition).getImages())
                    .into(holder.newsImg);

            //设置 Item 的点击事件
            holder.newsLayout.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    OnItemClick(realPosition);
                }
            });
        }
    }
}
```

- 相应的修改

```java
/**
 * 根据是否有设置头布局等,判断返回的个数
 * @return
 */
@Override
public int getItemCount() {
    if (news == null)
        return 0;
    else if (mHeaderView == null && mFooterView == null)
        return news.getStories().size();
    else if (mHeaderView == null && mFooterView != null)
        return news.getStories().size() + 1;
    else if (mHeaderView != null && mFooterView == null)
        return news.getStories().size() + 1;
    else if (mHeaderView != null && mFooterView != null)
        return news.getStories().size() + 2;

    return 0;
}
//继承 RecyclerView.ViewHolder，使用 ButterKnife
class ViewHolder extends RecyclerView.ViewHolder{

    @BindView(R.id.news_item_title)
    TextView newsTitle;
    @BindView(R.id.news_item_img)
    ImageView newsImg;
    @BindView(R.id.news_item_layout)
    LinearLayout newsLayout;

    public ViewHolder(View itemView) {
        super(itemView);
        if (itemView == mHeaderView)
            return;
        if (itemView == mFooterView)
            return;
        ButterKnife.bind(this,itemView);
    }
}
```

- 在 Activity 中调用

```
View banner = ......
mRecyclerViewAdapter.addHeaderView(banner);
```



## 五、关于滑动

- 简单的直接定位：

```
mLayoutManager.scrollToPositionWithOffset(position, 0);
```

缺点是体验不好，没有滑动的过程。

- RecyclerView 的滑动方法

```
rv.scrollToPosition(index);
rv.scrollBy(int x, int y);
```

`scrollToPosition()` 将对应的 item 滑动到屏幕内，当 item 变为可见时则停止滑动。
 所以当指定的 item 在当前屏幕的下方时，滑动后目标 item 会出现屏幕的最低下；当指定 item 在屏幕可见时，则完全没有滑动。
 很多时候这个方法是不符合我们预期的，一般是希望能将指定的 item 滑动到当前的屏幕顶端或中间。
 这时候可以配合 `scrollBy()` 来做一个判断：

```java
private void setSelectPosition(int index) {
    //当前可见的第一项和最后一项
    int firstItem = linearLayoutManager.findFirstVisibleItemPosition();
    int lastItem = linearLayoutManager.findLastVisibleItemPosition();
    if (index <= firstItem) {
        //当要置顶的项在当前显示的第一个项的前面时，直接调用没有问题
        rv.scrollToPosition(index);
    } else if (index <= lastItem) {
        //当要置顶的项已经在屏幕上显示时，计算需要滑动的距离
        int top = rv.getChildAt(index - firstItem).getTop();
        rv.scrollBy(0, top);
    } else {
        //当指定的 item 在当前显示的最后一项的后面时
        //这时候一次的滑动不足以将指定 item 放到顶端
        rv.scrollToPosition(index);
        //记录当前需要在RecyclerView滚动监听里面继续第二次滚动
        move = true;
    }
}
class RecyclerViewListener extends RecyclerView.OnScrollListener{
        @Override
        public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
            super.onScrolled(recyclerView, dx, dy);
            //在这里进行第二次滚动
            if (move ){
                move = false;
                int n = mIndex - mLinearLayoutManager.findFirstVisibleItemPosition();
                if ( 0 <= n && n < mRecyclerView.getChildCount()){
                    //要移动的距离
                    int top = mRecyclerView.getChildAt(n).getTop();
                    mRecyclerView.scrollBy(0, top);
                }
            }
        }
    }
```

上面这个方面也是看到别人的实现的思路，虽然没什么问题不过还是算是取巧的一种方法，仅记录。
 其实 Stack Overflow 上已经有了更好的答案 ：[RecyclerView - How to smooth scroll to top of item on a certain position?](https://stackoverflow.com/questions/31235183/recyclerview-how-to-smooth-scroll-to-top-of-item-on-a-certain-position)

- **重写 LinearLayoutManager**

```java
public class LinearLayoutManagerWithSmoothScroller extends LinearLayoutManager {

    public LinearLayoutManagerWithSmoothScroller(Context context) {
        super(context, VERTICAL, false);
    }

    public LinearLayoutManagerWithSmoothScroller(Context context, int orientation, boolean reverseLayout) {
        super(context, orientation, reverseLayout);
    }

    //重点方法
    @Override
    public void smoothScrollToPosition(RecyclerView recyclerView, RecyclerView.State state,
                                       int position) {

        //也就是说重点在于重写 SmoothScroller,而滑动的调用为 startSmoothScroll()
        RecyclerView.SmoothScroller smoothScroller = new  TopSnappedSmoothScroller(recyclerView.getContext());
        smoothScroller.setTargetPosition(position);
        startSmoothScroll(smoothScroller);
    }

    private class TopSnappedSmoothScroller extends LinearSmoothScroller {
        public TopSnappedSmoothScroller(Context context) {
            super(context);
        }

        @Override
        public PointF computeScrollVectorForPosition(int targetPosition) {
            return LinearLayoutManagerWithSmoothScroller.this
                    .computeScrollVectorForPosition(targetPosition);
        }

        @Override
        protected int getVerticalSnapPreference() {
            //将指定的 item 滑动至与屏幕的顶端对齐
            return SNAP_TO_START;
        }
    }
}
```

仔细看可以发现我们也可以选择不重写整个 LinearLayoutManager，只要将 LinearSmoothScroller 的 `getVerticalSnapPreference()` 重写也可以达到目的。

关于 `getVerticalSnapPreference ()` 的源码与注释：

```java
    /**
     * When scrolling towards a child view, this method defines whether we should align the top
     * or the bottom edge of the child with the parent RecyclerView.
     *
     * @return SNAP_TO_START, SNAP_TO_END or SNAP_TO_ANY; depending on the current target vector
     * @see #SNAP_TO_START
     * @see #SNAP_TO_END
     * @see #SNAP_TO_ANY
     */
    protected int getVerticalSnapPreference() {
        return mTargetVector == null || mTargetVector.y == 0 ? SNAP_TO_ANY :
                mTargetVector.y > 0 ? SNAP_TO_END : SNAP_TO_START;
    }
```

返回的值决定了指定 item 的对齐方式，与顶部对齐 / 底部对齐。
 所以最终代码：

```java
//初始化过程
mLayoutManager= new LinearLayoutManager(getActivity());
mSmoothScroller = new LinearSmoothScroller(getActivity()) {
    @Override protected int getVerticalSnapPreference() {
        return LinearSmoothScroller.SNAP_TO_START;
     }

    @Override
    public PointF computeScrollVectorForPosition(int targetPosition) {
        return mLayoutManager.computeScrollVectorForPosition(targetPosition);
    }
};
mRvRetail.setLayoutManager(mLayoutManager);

//移动
mSmoothScroller.setTargetPosition(position);
mLayoutManager.startSmoothScroll(mSmoothScroller);
```

这样就可以实现平滑的滑动了。



## 六、与 ScrollView 嵌套时惯性滑动失效

猜测是由于滑动的时候，ScrollView 将事件分发给了 RecyclerView，所以这个时候是 rv 在滑动；
 当快速滑动并离开屏幕的时候，按预期是应该有一段惯性般滑动、缓慢停止的过程，然而这时候应该是 ScrollView 拦截了这个 ACTION_UP 的事件，导致 RecyclerView 无法继续滑动。
 解决这个问题可以从 RecyclerView 入手，若 RecyclerView 不再响应事件，将事件交给 ScrollView 即可：

```java
mLayoutManager = new LinearLayoutManager(mContext){
    @Override
    public boolean canScrollVertically() {
        return false;
    }
};
mRv.setLayoutManager(mLayoutManager);
```



## 七、刷新数据源导致的问题

比较常见是在删除 item 的时候，可能会出现 item 的下标没有刷新、位置错位的问题。
 更新了数据源之后最保险的方法是调用 `notifyDataSetChanged()`  去刷新所有的数据，问题是如果只更新了少量的数据或者想要保留删除/添加 item 的动画，这个方法都不能满足要求。
 所以我们需要调用 `notifyItemRemoved(position)` 和 `notifyItemInserted(position)`，这时候 item 的内容已经刷新、并且带有动画效果。但是如果是删除 item 的操作，上面说的下标没有刷新、错位的问题就出现了，所以接下来要调用 `notifyItemRangeChanged(position, itemSize)`  将可能出现问题的 item 都刷一遍，所以 itemSize 常取 `list.size()`。



## 八、item 的局部刷新

说起 RecyclerView 的局部刷新，一般都是说到 `notifyItemChanged(int positon)` 即只刷新指定的 item 的布局，但是有时候需要针对 item 的部分控件进行刷新。
 比如一个 item 里有一张图片，单调用 notifyItemChanged 的时候图片会去重新加载，就导致了重复加载同样的图片而出现图片闪烁的问题，所以这时候我们需要针对 item 去刷新改变的部分、保留不变的部分。
 这里用上了

```java
@Override
public void onBindViewHolder(ViewHolder holder, int position, List<Object> payloads) {
  super.onBindViewHolder(holder, position, payloads);
}
```

和 `notifyItemChanged(int position, Object payload)`
 看到这两个方法其实方法已经很明显了，调用 `notifyItemChanged()`可以传递一个 payload 到 `onBindViewHolder` 方法里面，这时候就可以通过传递过来的数据类型去刷新不同的控件，没有刷新到的控件将会用 ViewHolder 的实例来展示。



## 九、ItemTouchHelper

ItemTouchHelper 是 RecyclerView 中辅助滑动和拖拽的实用工具类，一般用来做拖拽改变排序、滑动删除功能。

```java
ItemTouchHelper helper = new ItemTouchHelper(callback);
helper.attachToRecyclerView(recyclerView);
```

ItemTouchHelper 的构造方法需要一个传入 ItemTouchHelper.Callback 的实例，所以重点就在于这个 Callback 里的几个方法。

```java
class TestHelperCallback extends ItemTouchHelper.Callback{

  private ItemMoveListener mListener;

  public TestHelperCallback(){}

  public TestHelperCallback(ItemMoveListener listener){
    mListener = listener;
  }

  @Override
  public int getMovementFlags(RecyclerView recyclerView, RecyclerView.ViewHolder viewHolder) {
    //两个 flags 为 0 则无法拖动
    int dragFlags = ItemTouchHelper.UP | ItemTouchHelper.DOWN;//允许上下滑动
    int swipeFlags = ItemTouchHelper.LEFT | ItemTouchHelper.RIGHT;//允许左右滑动
    //生成并返回
    int flags = makeMovementFlags(dragFlags, swipeFlags);
    return flags;
  }

  @Override
  public boolean onMove(RecyclerView recyclerView, RecyclerView.ViewHolder viewHolder, RecyclerView.ViewHolder target) {
    //拖拽的回调
    if (mListener != null){
      return mListener.onItemMove(viewHolder.getAdapterPosition(), target.getAdapterPosition());
    }
    return false;
  }


  @Override
  public void onSwiped(RecyclerView.ViewHolder viewHolder, int direction) {
    //左右滑动的回调
    int position = viewHolder.getAdapterPosition();
    if (mListener != null){
      mListener.onItemDelete(position);
    }
  }

  @Override
  public void onChildDraw(Canvas c, RecyclerView recyclerView, RecyclerView.ViewHolder viewHolder, float dX, float dY, int actionState, boolean isCurrentlyActive) {
    //在滑动和拖拽的过程中会被调用去绘制动画，重写这里可以实现自己的动画
    super.onChildDraw(c, recyclerView, viewHolder, dX, dY, actionState, isCurrentlyActive);
    if(actionState == ItemTouchHelper.ACTION_STATE_SWIPE) {
         //左右滑动的同时改变 item 的透明度
         final float alpha = 1 - Math.abs(dX) / (float)viewHolder.itemView.getWidth();
         viewHolder.itemView.setAlpha(alpha);//透明度
         viewHolder.itemView.setTranslationX(dX);//滑动
     }
  }

  @Override
  public boolean isLongPressDragEnabled() {
    //是否可以长按拖拽
    return true;
  }
}

public interface ItemMoveListener {
   boolean onItemMove(int fromPosition, int toPosition);
   boolean onItemDelete(int position);
}
```

通过接口 ItemMoveListener 把回调放出去，接下来用 Adapter 实现并处理回调：

```java
class ItemMoveRecyclerAdapter extends CommonRecyclerAdapter<String> implements ItemMoveListener{

  public ItemMoveRecyclerAdapter(Context context, List dataList, int layoutId) {
    super(context, dataList, layoutId);
  }

  @Override
  public void convert(CommonRecyclerViewHolder holder, int position, String o) {
  }

  @Override
  public void onItemClick(CommonRecyclerViewHolder holder, int position, String o) {
  }

  @Override
  public boolean onItemMove(int fromPosition, int toPosition) {
    //交换数据
    Collections.swap(mDataList, fromPosition, toPosition);
    //刷新
    notifyItemMoved(fromPosition, toPosition);
    notifyItemRangeChanged(Math.min(fromPosition,toPosition),getItemCount());
    return true;
  }

  @Override
  public boolean onItemDelete(int position) {
    mDataList.remove(position);
    notifyItemRemoved(position);
    notifyItemRangeChanged(position,getItemCount());
    return false;
  }
}
```

CommonRecyclerAdapter 是项目封装的 Adapter，这里不是重点。
 `onItemMove()`中依次调用了 `notifyItemMoved()` 和 `notifyItemRangeChanged()`，防止在移动过后 item 的下标错乱。
 `onItemDelete ()` 中同理。
 上面代码基本就是拖拽排序和滑动删除的代码，因为很简单就没有多分析，因为一开始是想用 ItemTouchHelper 仿照 QQ 的侧滑按钮，但是看了一圈发现好像没办法用 ItemTouchHelper 实现。

> clipToPadding 属性

`android:clipToPadding` 属性决定子 View 是否可以在 父布局的 padding 中绘制。
 在 RecyclerView 中子 View 即 item，该属性为false 时，RecyclerView 的 padding 会被 item 遮住。
 比如我们在 RecyclerView 的底部上悬浮一个按钮，按钮会挡住下方的 item，`clipToPadding` + `paddingBottom` 可以实现在 RecyclerView 拉倒底部的时候为悬浮按钮留出空间。