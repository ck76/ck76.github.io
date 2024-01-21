[TOC]

### 一、简介

RecyclerView 自从被发布以来，一直被说成是 ListView、GridView 等一系列列表控件的完美替代品。并且它本身使用起来也非常的好用，布局切换方便、自带 ViewHolder 、局部更新并且可带更新动画等等。

局部更新、并且可以很方便的设置更新动画这一点，是 RecyclerView 一个不错的亮点。它为此提供了对应的方法：

- adapter.notifyItemChange()
- adapter.notifyItemInserted()
- adapter.notifyItemRemoved()
- adapter.notifyItemMoved();

以上方法都是为了对数据集中，单一项进行操作，并且为了操作连续的数据集的变动，还提供了对应的 `notifyRangeXxx()` 方法。

虽然 RecyclerView 提供的局部更新的方法，看似非常的好用，但是实际上，其实并没有什么用。

在实际开发中，最方便的做法就是无脑调用 `notifyDataSetChanged()`，用于更新 Adapter 的数据集。

虽然 `notifyDataSetChanged()` 有一些缺点：

- 不会触发 RecyclerView 的局部更新的动画。
- 性能低，会刷新整个 RecyclerView 可视区域。

但是真有需要频繁刷新，前后两个数据集的场景。

方案一：使用一个 `notifyDataSetChanged()` 方法。

方案二：自己写一个数据集比对方法，然后去计算他们的差值，最后调用对应的方法更新到 RecyclerView 中去。

我这么懒，如果不是必要，当然是会选 方案一 了。毕竟和之前 ListView 的时候，也没有更差了。

**Google 显然也发现了这个问题，所以 DiffUtil 被发布了。**



就像前面说的，DiffUtil 就是为了解决这个痛点的。它能很方便的对两个数据集之间进行比对，然后计算出变动情况，配合 RecyclerView.Adapter ，可以自动根据变动情况，调用 Adapter 的对应方法。

当然，DiffUtil 不仅只能配合 RecyclerView 使用，它实际上可以单独用于比对两个数据集，然后如何操作是可以定制的，那么在什么场景下使用，就全凭我们自己发挥了。

DiffUtil 在使用起来，主要需要关注几个类：

- DiffUtil.Callback：具体用于限定数据集比对规则。
- DiffUtil.DiffResult：比对数据集之后，返回的差异结果。



### 二、使用步骤

1. 创建一个`DiffUtilCallback`继承`DiffUtil.Callback`，重写里面的抽象方法
2. 使用`DiffUtil.calculateDiff(callback)`，计算差异结果，需要`DiffUtilCallback`，返回对象为`DiffUtil.DiffResult`。
3. 使用`DiffResult`对象，通过`diffResult.dispatchUpdatesTo(adapter)`方法，将`DiffUtil`与`Adapter`关联起来

```java
++++使用DiffUtil后，改为如下代码：

DiffUtil.DiffResult diffResult = DiffUtil.calculateDiff(new DiffCallBack(mDatas, newDatas), true);
diffResult.dispatchUpdatesTo(mAdapter);

++++它会自动计算新老数据集的差异，并根据差异情况，自动调用以下四个方法

adapter.notifyItemRangeInserted(position, count);
adapter.notifyItemRangeRemoved(position, count);
adapter.notifyItemMoved(fromPosition, toPosition);
adapter.notifyItemRangeChanged(position, count, payload);
```

#### 1、DiffUtil.Callback

DiffUtil.Callback 主要就是为了限定两个数据集中，子项的比对规则。毕竟开发者面对的数据结构多种多样，既然没法做一套通用的内容比对方式，那么就将比对的规则，交还给开发者来实现即可。

在 Callback 中，其实只需要实现 4 个方法：

- `getOldListSize()`：旧数据集的长度。
- `getNewListSize()`：新数据集的长度
- `areItemsTheSame()`：判断是否是同一个Item。
- `areContentsTheSame()`：如果是通一个Item，此方法用于判断是否同一个 Item 的内容也相同。

前两个是获取数据集长度的方法，这没什么好说的。但是后两个方法，主要是为了对应多布局的情况产生的，也就是存在多个 viewType 和多个 ViewHodler 的情况。首先需要使用 `areItemsTheSame()` 方法比对是否来自同一个 viewType（也就是同一个 ViewHolder ） ，然后再通过 `areContentsTheSame()` 方法比对其内容是否也相等。

其实 Callback 还有一个 `getChangePayload()` 的方法，它可以在 ViewType 相同，但是内容不相同的时候，用 payLoad 记录需要在这个 ViewHolder 中，具体需要更新的View。

`areItemsTheSame()`、`areContentsTheSame()`、`getChangePayload()` 分别代表了不同量级的刷新。

首先会通过 `areItemsTheSame()` 判断当前 position 下，ViewType 是否一致，如果不一致就表明当前 position 下，从数据到 UI 结构上全部变化了，那么就不关心内容，直接更新就好了。如果一致的话，那么其实 View 是可以复用的，就还需要再通过 `areContentsTheSame()` 方法判断其内容是否一致，如果一致，则表示是同一条数据，不需要做额外的操作。但是一旦不一致，则还会调用 `getChangePayload()` 来标记到底是哪个地方的不一样，最终标记需要更新的地方，最终返回给 DiffResult 。

当然，对性能要是要求没那么高的情况下，是可以不使用 `getChangedPayload()` 方法的。



#### 2、DiffUtil.DiffResult 

DiffUtil.DiffResult 其实就是 DiffUtil 通过 DiffUtil.Callback 计算出来，两个数据集的差异。它是可以直接使用在 RecyclerView 上的。如果有必要，也是可以通过实现 ListUpdateCallback 接口，来比对这些差异的。

> DiffUtil 使用的是 Eugene Myers 的差别算法，这个算法本身是不检查元素的移动的。也就是说，有元素的移动它也只是会先标记为删除，然后再标记插入。而如果需要计算元素的移动，它实际上也是在通过 Eugene Myers 算法比对之后，再进行一次移动检查。所以，如果集合本身已经排序过了，可以不进行移动的检查。





### 三、例子

#### 1、创建DiffCallback

```java
public class DiffCallBack extends DiffUtil.Callback {
    private List<TestBean> mOldDatas, mNewDatas;//看名字

    public DiffCallBack(List<TestBean> mOldDatas, List<TestBean> mNewDatas) {
        this.mOldDatas = mOldDatas;
        this.mNewDatas = mNewDatas;
    }

    //老数据集size
    @Override
    public int getOldListSize() {
        return mOldDatas != null ? mOldDatas.size() : 0;
    }

    //新数据集size
    @Override
    public int getNewListSize() {
        return mNewDatas != null ? mNewDatas.size() : 0;
    }

    /**
     * 被DiffUtil调用，用来判断 两个对象是否是相同的Item。
     * 例如，如果你的Item有唯一的id字段，这个方法就 判断id是否相等。
     * 本例判断name字段是否一致
     */
    @Override
    public boolean areItemsTheSame(int oldItemPosition, int newItemPosition) {
        return mOldDatas.get(oldItemPosition).getName().equals(mNewDatas.get(newItemPosition).getName());
    }

    /**
     * Called by the DiffUtil when it wants to check whether two items have the same data.
     * 被DiffUtil调用，用来检查 两个item是否含有相同的数
     * DiffUtil用返回的信息（true false）来检测当前item的内容是否发生了变化
     * 所以你可以根据你的UI去改变它的返回值
     * 例如，如果你用RecyclerView.Adapter 配合DiffUtil使用，你需要返回Item的视觉表现是否相同。
     * 这个方法仅仅在areItemsTheSame()返回true时，才调用。
     */
    @Override
    public boolean areContentsTheSame(int oldItemPosition, int newItemPosition) {
        TestBean beanOld = mOldDatas.get(oldItemPosition);
        TestBean beanNew = mNewDatas.get(newItemPosition);
        if (!beanOld.getDesc().equals(beanNew.getDesc())) {
            return false;//如果有内容不同，就返回false
        }
        if (beanOld.getPic() != beanNew.getPic()) {
            return false;//如果有内容不同，就返回false
        }
        return true; //默认两个data内容是相同的
    }
```



#### 2、具体使用

```java
//利用DiffUtil.calculateDiff()方法，传入一个规则DiffUtil.Callback对象，和是否检测移动item的 boolean变量，得到DiffUtil.DiffResult 的对象
DiffUtil.DiffResult diffResult = DiffUtil.calculateDiff(new DiffCallBack(mDatas, newDatas), true);

//利用DiffUtil.DiffResult对象的dispatchUpdatesTo（）方法，传入RecyclerView的Adapter
diffResult.dispatchUpdatesTo(mAdapter);

//别忘了将新数据给Adapter
mDatas = newDatas;
mAdapter.setDatas(mDatas);
```

