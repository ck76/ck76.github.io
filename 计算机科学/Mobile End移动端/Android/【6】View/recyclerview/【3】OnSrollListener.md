[TOC]

## 一、列表滚动过程

- 手指按下 -> 手指拖拽列表移动 -> 手指停止拖拽 -> 抬起手指
- 手指按下 -> 手指快速拖拽后抬起手指 -> 列表继续滚动 -> 停止滚动

从上面可以看出,滚动状态分为:

```java
|--静止
|--滚动
    |--被迫拖拽移动
	|--自己滚动
```

上面的过程的状态变化如下:

1. 静止 -> 被迫拖拽移动 -> 静止
2. 静止 -> 被迫拖拽移动 -> 自己滚动 -> 静止



## 二、监听RecyclerView的滚动

查看RecyclerView的源码,我们可以看到以下代码:

```java
/**
 * Set a listener that will be notified of any changes in scroll state or position.
 * @param listener Listener to set or null to clear
 * @deprecated Use {@link #addOnScrollListener(OnScrollListener)} and
 *             {@link #removeOnScrollListener(OnScrollListener)}
 */
@Deprecated							//已过时
public void setOnScrollListener(OnScrollListener listener) {
    mScrollListener = listener;		//设置滑动监听接口
}

/**
 * Add a listener that will be notified of any changes in scroll state or position.
 * <p>Components that add a listener should take care to remove it when finished.
 * Other components that take ownership of a view may call {@link #clearOnScrollListeners()}
 * to remove all attached listeners.</p>
 * @param listener listener to set or null to clear
 */
public void addOnScrollListener(OnScrollListener listener) {
    if (mScrollListeners == null) {
        mScrollListeners = new ArrayList<>();
    }
    mScrollListeners.add(listener);
}
```

查看OnScrollListener源码：

```java
public abstract static class OnScrollListener {
    /**
     * Callback method to be invoked when RecyclerView's scroll state changes.
     * @param recyclerView The RecyclerView whose scroll state has changed.
     * @param newState     The updated scroll state. One of {@link #SCROLL_STATE_IDLE},
     *                     {@link #SCROLL_STATE_DRAGGING} or {@link #SCROLL_STATE_SETTLING}.
     */
    public void onScrollStateChanged(RecyclerView recyclerView, int newState){}

    /**
     * Callback method to be invoked when the RecyclerView has been scrolled. This will be
     * called after the scroll has completed.
     * <p>
     * This callback will also be called if visible item range changes after a layout
     * calculation. In that case, dx and dy will be 0.
     *
     * @param recyclerView The RecyclerView which scrolled.
     * @param dx The amount of horizontal scroll.
     * @param dy The amount of vertical scroll.
     */
    public void onScrolled(RecyclerView recyclerView, int dx, int dy){}
}
```

- onScrollStateChanged : 滚动状态变化时回调 
- onScrolled : 滚动时回调

两者的区别在于 **状态与过程**。



### 1、onScrollStateChanged 方法

在`void onScrollStateChanged(RecyclerView recyclerView, int newState)`中回调两个变量:

1. recyclerView : 当前在滚动的RecyclerView
2. newState : 当前滚动状态.。

其中newState有三种值

```java
//静止,没有滚动
public static final int SCROLL_STATE_IDLE = 0;

//正在被外部拖拽,一般为用户正在用手指滚动
public static final int SCROLL_STATE_DRAGGING = 1;

//自动滚动开始
public static final int SCROLL_STATE_SETTLING = 2;
```



### 2、onScrolled 方法

在`void onScrolled(RecyclerView recyclerView, int dx, int dy)`方法中回调了三个变量:

1. recyclerView : 当前滚动的view
2. dx : 水平滚动距离         
3. dy : 垂直滚动距离

**dx > 0时为手指向左滚动,列表滚动显示右面的内容**
**dx < 0 时为手指向右滚动,列表滚动显示左面的内容**
**dy > 0 时为手指向上滚动,列表滚动显示下面的内容**
**dy < 0 时为手指向下滚动,列表滚动显示上面的内容**



### 3、回调过程

**缓慢拖拽回调过程:**

```
1. newState = RecyclerView.SCROLL_STATE_DRAGGING;
2. dy 多次改变
3. newState = RecyclerView.SCROLL_STATE_IDLE
```

**快速滚动回调过程:**

```
1. newState = RecyclerView.SCROLL_STATE_DRAGGING;
2. dy 多次改变
3. newState = RecyclerView.SCROLL_STATE_SETTLING;
4. dy 多次改变
5. newState = RecyclerView.SCROLL_STATE_IDLE;
```



### 4、能否继续滑动

`canScrollVertically和canScrollHorizontally方法`

```java
public boolean canScrollVertically (int direction)
这个方法是判断View在竖直方向是否还能向上，向下滑动。
其中，direction为 -1 表示手指向下滑动（屏幕向上滑动）， 1 表示手指向上滑动（屏幕向下滑动）。

public boolean canScrollHorizontally (int direction)
这个方法用来判断 水平方向的滑动
```

例如：
RecyclerView.canScrollVertically(1)的值表示是屏幕否能向下**滚动**，**false表示已经滚动到底部**
RecyclerView.canScrollVertically(-1)的值表示是屏幕否能向上**滚动**，**false表示已经滚动到顶部**



## 三、判断到达底部

### 1、item数关系

```java
//////////visibleItemCount + pastVisiblesItems) >= totalItemCount
/*
如果 当前
第一个可见item的位置 + 当前可见的item个数 >= item的总个数
这样就可以判断出来，是在底部了。
*/
loadingMoreListener = new RecyclerView.OnScrollListener() {
        @Override
        public void onScrollStateChanged(RecyclerView recyclerView, int newState) {
            super.onScrollStateChanged(recyclerView, newState);
        }
 
        @Override
        public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
            super.onScrolled(recyclerView, dx, dy);
 
            if (dy > 0) //向下滚动
            {
                int visibleItemCount = mLinearLayoutManager.getChildCount();	//得到显示屏幕内的list数量
                int totalItemCount = mLinearLayoutManager.getItemCount();	//得到list的总数量
                int pastVisiblesItems = mLinearLayoutManager.findFirstVisibleItemPosition();//得到显示屏内的第一个list的位置数position
 
                if (!loading && (visibleItemCount + pastVisiblesItems) >= totalItemCount) {
                    loading = true;
                    loadMoreDate();
                }
            }
        }
};

```



```java
///////LastvisibleItemCount = (totalItemCount - 1)


			int lastCompletePosition = linearLayoutManager.findLastCompletelyVisibleItemPosition();
            int totalItemCount = linearLayoutManager.getItemCount();
 
                if (!loading && (LastvisibleItemCount = (totalItemCount - 1)) {
                    loading = true;
                    loadMoreDate();
                }
```



### 2、canScrollVertically

```java
loadingMoreListener = new RecyclerView.OnScrollListener() {
    @Override
    public void onScrollStateChanged(RecyclerView recyclerView, int newState) {
        super.onScrollStateChanged(recyclerView, newState);
        if(!loading && !recyclerView.canScrollVertically(1)){
            loading = true;
            loadMoreDate();
        }
    }

    @Override
    public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
        super.onScrolled(recyclerView, dx, dy);

//                if (dy > 0) //向下滚动
//                {
//                    int visibleItemCount = mLinearLayoutManager.getChildCount();
//                    int totalItemCount = mLinearLayoutManager.getItemCount();
//                    int pastVisiblesItems = mLinearLayoutManager.findFirstVisibleItemPosition();
//
//                    if (!loading && (visibleItemCount + pastVisiblesItems) >= totalItemCount) {
//                        loading = true;
//                        loadMoreDate();
//                    }
//                }
    }
};
```



## 四、获取滑动距离

1、LinearLayoutManager

```java
   public int getScollYDistance() {
       LinearLayoutManager layoutManager = (LinearLayoutManager) this.getLayoutManager();
       int position = layoutManager.findFirstVisibleItemPosition();
       View firstVisiableChildView = layoutManager.findViewByPosition(position);
       int itemHeight = firstVisiableChildView.getHeight();
       return (position) * itemHeight - firstVisiableChildView.getTop();
   }
```



[还有一个CSDN连接](https://blog.csdn.net/u011374875/article/details/51744448)