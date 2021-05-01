[TOC]

### 一、层级

RecyclerView比ListView多两级缓存，支持多个离ItemView缓存，支持开发者自定义缓存处理逻辑，支持所有RecyclerView共用同一个RecyclerViewPool(缓存池)。

具体来说：

#### 1、ListView(两级缓存)：

![list](http://s191.photo.store.qq.com/psb?/V14L47VC0w3vOf/LnmHnuuuiM7Tc3AnQIYzbWzM0vCMdrPGdVLn7L31ip8!/b/dL8AAAAAAAAA)

---

#### 2、RecyclerView(四级缓存)：

![RecyclerView](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/ryLlNtIBhcCi1tO2CQxFWH8DrzcXtTtD8ssm6NjTtX4!/r/dFIBAAAAAAAA)

---

#### 3、对比

ListView和RecyclerView缓存机制基本一致：

- mActiveViews和mAttachedScrap功能相似，意义在于快速重用屏幕上可见的列表项ItemView，而不需要重新createView和bindView；

- mScrapView和mCachedViews + mReyclerViewPool功能相似，意义在于缓存离开屏幕的ItemView，目的是让即将进入屏幕的ItemView重用.

- RecyclerView的优势在于
  - mCacheViews的使用，可以做到屏幕外的列表项ItemView进入屏幕内时也无须bindView快速重用；
  - mRecyclerPool可以供多个RecyclerView共同使用，在特定场景下，如viewpaper+多个列表页下有优势。

客观来说，RecyclerView在特定场景下对ListView的缓存机制做了补强和完善。



### 二、缓存机制

- RecyclerView缓存RecyclerView.ViewHolder，抽象可理解为：
  View + ViewHolder(避免每次createView时调用findViewById) + flag(标识状态)；
- ListView缓存View。

缓存不同，二者在缓存的使用上也略有差别，具体来说：

#### 1、ListView获取缓存的流程：

![list](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/llamcu4l4UPJ8zs.lEvNLR0xhErTQBZNot*MJj3.EwE!/r/dLgAAAAAAAAA)

---

#### 2、RecyclerView获取缓存的流程：

![RecyclerView](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/HNT65lh2MajHRLyGIwttLX8.OIpU8jyPMfKTNeMVHx4!/r/dLgAAAAAAAAA)

----

#### 3、对比

-  RecyclerView中mCacheViews(屏幕外)获取缓存时，是通过匹配pos获取目标位置的缓存，这样做的好处是，当数据源数据不变的情况下，无须重新bindView：
- 而同样是离屏缓存，ListView从mScrapViews根据pos获取相应的缓存，但是并没有直接使用，而是重新getView（即必定会重新bindView），相关代码如下：

```java
//AbsListView源码：line2345
//通过匹配pos从mScrapView中获取缓存
final View scrapView = mRecycler.getScrapView(position);
//无论是否成功都直接调用getView,导致必定会调用createView
final View child = mAdapter.getView(position, scrapView, this);
if (scrapView != null) {
    if (child != scrapView) {
        mRecycler.addScrapView(scrapView, position);
    } else {
        ...
    }
}
```

- ListView中通过pos获取的是view，即pos–>view；
  RecyclerView中通过pos获取的是viewholder，即pos –> (view，viewHolder，flag)；
  从流程图中可以看出，标志flag的作用是判断view是否需要重新bindView，这也是RecyclerView实现局部刷新的一个核心.



### 三、局部刷新

由上文可知，RecyclerView的缓存机制确实更加完善，但还不算质的变化，RecyclerView更大的亮点在于提供了局部刷新的接口，通过局部刷新，就能避免调用许多无用的bindView.

(RecyclerView和ListView添加，移除Item效果对比)

结合RecyclerView的缓存机制，看看局部刷新是如何实现的：
以RecyclerView中notifyItemRemoved(1)为例，最终会调用requestLayout()，使整个RecyclerView重新绘制，过程为：
onMeasure()–>onLayout()–>onDraw()

其中，onLayout()为重点，分为三步：

1. dispathLayoutStep1()：记录RecyclerView刷新前列表项ItemView的各种信息，如Top,Left,Bottom,Right，用于动画的相关计算；
2. dispathLayoutStep2()：真正测量布局大小，位置，核心函数为layoutChildren()；
3. dispathLayoutStep3()：计算布局前后各个ItemView的状态，如Remove，Add，Move，Update等，如有必要执行相应的动画.

**其中，layoutChildren()流程图：**

![layoutchild](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/fBHrcbtrkIP4mAFVzRudsP1Rw4BsqFH0iyWI816LMiE!/r/dC8BAAAAAAAA)

----

**RecyclerView.fill()**

![fill](http://s191.photo.store.qq.com/psb?/V14L47VC0w3vOf/vSwHVRgSm9ISLOa4E2ymIwXHoyh3CcJaqR3mAbqcunQ!/b/dL8AAAAAAAAA)

-----

当调用notifyItemRemoved时，会对屏幕内ItemView做预处理，修改ItemView相应的pos以及flag(流程图中红色部分)：

![notifyItemRemoved](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/sUBEJZW248VTa4Ud5fxIER6hdx9GHSMAE38o0FD.cDo!/r/dDUBAAAAAAAA)

----

当调用fill()中RecyclerView.getViewForPosition(pos)时，RecyclerView通过对pos和flag的预处理，使得bindview只调用一次.

需要指出，ListView和RecyclerView最大的区别在于数据源改变时的缓存的处理逻辑，ListView是”一锅端”，将所有的mActiveViews都移入了二级缓存mScrapViews，而RecyclerView则是更加灵活地对每个View修改标志位，区分是否重新bindView。



### 四、回收机制

#### 1、ListView

ListView为了保证Item View的复用，实现了一套回收机制，该回收机制的实现类是RecycleBin，他实现了两级缓存：

- `View[] mActiveViews`: 缓存屏幕上的View，在该缓存里的View不需要调用`getView()`。
- `ArrayList<View>[] mScrapViews;`: 每个Item Type对应一个列表作为回收站，缓存由于滚动而消失的View，此处的View如果被复用，会以参数的形式传给`getView()`。

接下来我们通过源码分析ListView是如何与RecycleBin交互的。其实ListView和RecyclerView的layout过程大同小异，ListView的布局函数是`layoutChildren()`，实现如下：

```java
void layoutChildren(){
    //1. 如果数据被改变了，则将所有Item View回收至scrapView  
  //（而RecyclerView会根据情况放入Scrap Heap或RecyclePool）；否则回收至mActiveViews
    if (dataChanged) {
        for (int i = 0; i < childCount; i++) {
            recycleBin.addScrapView(getChildAt(i), firstPosition+i);
        }
    } else {
        recycleBin.fillActiveViews(childCount, firstPosition);
    }
    //2. 填充
    switch(){
        case LAYOUT_XXX:
            fillXxx();
            break;
        case LAYOUT_XXX:
            fillXxx();
            break;
    }
    //3. 回收多余的activeView
    mRecycler.scrapActiveViews();
}
```

其中`fillXxx()`实现了对Item View进行填充，该方法内部调用了`makeAndAddView()`，实现如下：

```java
View makeAndAddView(){
    if (!mDataChanged) {
        child = mRecycler.getActiveView(position);
        if (child != null) {
            return child;
        }
    }
    child = obtainView(position, mIsScrap);
    return child;
}
```

其中，`getActiveView()`是从mActiveViews中获取合适的View，如果获取到了，则直接返回，而不调用`obtainView()`，这也印证了如果从mActiveViews获取到了可复用的View，则不需要调用`getView()`。

`obtainView()`是从mScrapViews中获取合适的View，然后以参数形式传给了`getView()`，实现如下：

```java
View obtainView(int position){
    final View scrapView = mRecycler.getScrapView(position);  //从RecycleBin中获取复用的View
    final View child = mAdapter.getView(position, scrapView, this);
}
```

接下去我们介绍`getScrapView(position)`的实现，该方法通过position得到Item Type，然后根据Item Type从mScrapViews获取可复用的View，如果获取不到，则返回null，具体实现如下：

```java
class RecycleBin{
    private View[] mActiveViews;    //存储屏幕上的View
    private ArrayList<View>[] mScrapViews;  //每个item type对应一个ArrayList
    private int mViewTypeCount;            //item type的个数
    private ArrayList<View> mCurrentScrap;  //mScrapViews[0]

    View getScrapView(int position) {
        final int whichScrap = mAdapter.getItemViewType(position);
        if (whichScrap < 0) {
            return null;
        }
        if (mViewTypeCount == 1) {
            return retrieveFromScrap(mCurrentScrap, position);
        } else if (whichScrap < mScrapViews.length) {
            return retrieveFromScrap(mScrapViews[whichScrap], position);
        }
        return null;
    }
    private View retrieveFromScrap(ArrayList<View> scrapViews, int position){
        int size = scrapViews.size();
        if(size > 0){
            return scrapView.remove(scrapViews.size() - 1);  //从回收列表中取出最后一个元素复用
        } else{
            return null;
        }
    }
}
```



#### 2、RecyclerView

RecyclerView和ListView的回收机制非常相似，但是**ListView是以View作为单位进行回收**，**RecyclerView是以ViewHolder作为单位进行回收**。
Recycler是RecyclerView回收机制的实现类，他实现了四级缓存：

- mAttachedScrap: 缓存在屏幕上的ViewHolder。
- mCachedViews: 缓存屏幕外的ViewHolder，默认为2个。ListView对于屏幕外的缓存都会调用`getView()`。
- mViewCacheExtensions: 需要用户定制，默认不实现。
- mRecyclerPool: 缓存池，多个RecyclerView共用。

在上文Layout Manager中已经介绍了RecyclerView的layout过程，但是一笔带过了`getViewForPosition()`，因此此处介绍该方法的实现。

```java
View getViewForPosition(int position, boolean dryRun){
    if(holder == null){
        //从mAttachedScrap,mCachedViews获取ViewHolder
        holder = getScrapViewForPosition(position,INVALID,dryRun); //此处获得的View不需要bind
    }
    final int type = mAdapter.getItemViewType(offsetPosition);
    if (mAdapter.hasStableIds()) { //默认为false
        holder = getScrapViewForId(mAdapter.getItemId(offsetPosition), type, dryRun);
    }
    if(holder == null && mViewCacheExtension != null){
        final View view = mViewCacheExtension.getViewForPositionAndType(this, position, type); //从
        if(view != null){
            holder = getChildViewHolder(view);
        }
    }
    if(holder == null){
        holder = getRecycledViewPool().getRecycledView(type);
    }
    if(holder == null){  //没有缓存，则创建
        holder = mAdapter.createViewHolder(RecyclerView.this, type); //调用onCreateViewHolder()
    }
    if(!holder.isBound() || holder.needsUpdate() || holder.isInvalid()){
        mAdapter.bindViewHolder(holder, offsetPosition);
    }
    return holder.itemView;
}
```

从上述实现可以看出，依次从mAttachedScrap, mCachedViews, mViewCacheExtension, mRecyclerPool寻找可复用的ViewHolder，如果是从mAttachedScrap或mCachedViews中获取的ViewHolder，则不会调用`onBindViewHolder()`，mAttachedScrap和mCachedViews也就是我们所说的Scrap Heap；而如果从mViewCacheExtension或mRecyclerPool中获取的ViewHolder，则会调用`onBindViewHolder()`。

RecyclerView局部刷新的实现原理也是基于RecyclerView的回收机制，即能直接复用的ViewHolder就不调用`onBindViewHolder()`。



### 链接

- [基于滑动场景解析RecyclerView的回收复用机制原理](https://www.jianshu.com/p/9306b365da57)
- [Bugly](https://blog.csdn.net/tencent_bugly/article/details/52981210)
- https://www.jianshu.com/p/4f9591291365
- https://www.jianshu.com/p/697ce543b1c1

