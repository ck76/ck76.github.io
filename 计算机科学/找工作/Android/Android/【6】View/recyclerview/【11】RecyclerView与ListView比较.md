[TOC]

## 一、RecyclerView 概述

RecyclerView 集成自 ViewGroup 。RecyclerView是Android-support-V7版本中新增的一个Widgets，官方对于它的介绍是：RecyclerView是ListView的升级版本，更加先进和灵活。

Android L 之后，Google 提供了RecyclerView视图化控件，5.0之前如果想要使用的话，可以添加V7包以向下兼容，提供更全面的API和更灵活的布局管理。



## 二、RecyclerView 做了什么

- 类似ListView；
- 类似GridView；
- 横向ListView；
- 横向GridView；
- 瀑布流式布局



## 三、RecyclerView 组成
| 组成                        | 作用                                        |
| --------------------------- | ------------------------------------------- |
| RecyclerView.LayoutManager  | 管理子View布局的一个组件                    |
| RecyclerView.Recyler        | 负责数据、Item的生成和数据的绑定            |
| RecyclerView.Adapter        | 负责 Item 的缓存                            |
| RecyclerView.ViewHolder     | -                                           |
| RecyclerView.ItemDecoration | 自定义绘制 itemView 之间的一些特殊UI 或Item |
| RecyclerView.ItemAnimator   | Item动画                                    |

机制：layoutmanager 从Recycle 中获取已经绑定数据的 Item 显示，并将不再需要的Item 丢给Recycler 回收；Adapter 负责生成新Item 并将其绑定好数据，供Recyle获取；Recycler 就是子 Item 的一个缓存池。



## 四、RecyclerView.LayoutManager

主要负责：布局子视图、滚动子视图在滚动过程中根据子视图在布局中所处的位置，决定何时添加子视图和删除子视图。
涉及到的API：

- 获取布局尺寸

  ```java
   setRecyclerView（）、setMeasureSpecs（）、setMeasuredDimensionFromChildren（）
  ```

- 设置和获取方向的水平或垂直

   ```java
  canScrollHorizontally（）、canScrollVertically（）、setOrientation（）、getOrientation（）
  ```

- 滑动状态改变时RecyclerView调用方法通知LayoutManager

  ```java
   onScrollStateChanged(（）
  ```

- 增、删、移动子View

   ```java
  addView（）、removeView（）、moveView（）、removeAllViews（）、removeViewAt（）
  ```

- 获取指定位置的View

  ```java
   getPosition（）、getChildAt（）、findViewByPosition（）
  ```

- 获取可见子View及全部子View的个数

  ```java
   getChildCount（）、getItemCount（）
  ```

- 移除数据后调用了recycler进行数据的缓存

  ```java
   detachView（）、detachAndScrapAttachedViews（）、removeAndRecycleAllViews（）、scrapOrRecycleView（）
  ```



## 五、RecyclerView.Adapter 

Adapter 有几个抽象方法需要子类实现：

- 返回一个ViewHolder 封装实例
   onCreateViewHolder（）
- 根据ViewHolder对应的View进行数据绑定
   onBindViewHolder（）
- 获取总数
   getItemCount（）
- 获取不同Type类型的View，为添加 header 和 footer 预留接口
   getItemViewType（）
- 指定位置的 item 内容发生了变化
   notifyItemChanged（）
- 在指定的位置处插入一个 Item
   notifyItemInserted（）
- 指定位置的两个 Item 进行交换
   notifyItemMoved（int， int）



## 六、RecyclerView.Recyler

即提供新的，也回收旧的（强大就强大在View的循环回收利用）

- RecyclerView 的二级缓存：
   有两个缓存：Scrap 和 Recycle ，Scrap 中文是废料的意思。Recycle 对应是回收的意思。
   Scrap 缓存是指里面缓存的View 是接下来需要用到的，不需要新绑定数据，是一个轻量级的缓存集合，而Recycle 的缓存的 View 为里面的数据需要重新绑定，都放在RecyclerViewPool 池中，都需要通过 Adapter 重新绑定数据。

- RecyclerView 缓存的两种方式：
   **Detach 和 Remove ，Detach 的View 放在Scrap 缓存中，Remove 掉的View 放在 RecyclerViewPool缓存池中。**

- 使用场景：
   反复去将View移除并且马上又要添加进去时，选择Detach 方式，通过方法 detachAndScrapView（）实现。使用频率很低，屏幕中不显示的时候使用Remove 的方式，通过方法 removeAndRecycleView（）实现。

- 复用流程：

  > 三步走

   **当我们去获取一个新的View时，首先去检查Scrap 缓存是否有对应的 position 的View ，如果有直接用；如果没有，则从RecyclerViewPool缓存池中取，并且会回调Adapter 的onBindViewHolder 方法（如果Recycle 缓存为空，还会调用onCreateViewHolder方法），最后再将绑定好新数据的View返回。**

### 1、相关方法

- 获取缓存最大阀值，阀值为2
   setItemViewCacheSize（）

- 从缓存中取Item
   getViewForPosition（）

- 获取Scrap 缓存列表
   getScrapList（）

- 从Layoutmanager 回收Item
   recyclerView（）

- 回收后，缓存类型的内部处理逻辑
   recyclerViewHolderInternal（）

- 根据Adapter 变化，转换Item 缓存如pool
   onAdapterChanged（）

- 调用此方法返回一个RecyclerViewPool 实例
   getRecyclerView（）

- 缓存如pool
   addViewHolderToRecyclerViewPool（）

#### 2、RecyclerViewPool 相关：

- 缓存的不同类型ViewType的数量是不限的，但是每个viewType 的具体ViewHolder 最多为5个
   setMaxRecycledViews（int viewType，int max）
- 存入viewHolder
   putRecycledView（）
- 。。。
   getRecycledView（）



## 七、RecyclerViw 与 listView 比较

- **Item 回收/复用方面**：后者是以convertView 作为回收单位，需要手动添加ViewHolder ，而前者则是以ViewHolder作为回收单位，convertView 被内置到了ViewHolder 中作为 ViewHolder 的成员变量，前者内置了Recycle 、多级缓存。

- **样式丰富方面**：前者通过支持水平、垂直和变革列表及其他更复杂形式，而后者只支持具体某一种

- **效果增强方面**：前者内置了ItemDecoration 和 ItemAnimator ，可以自定义绘制 itemView 之间的一些特殊UI 或Item 项数据变化时的动画效果，而后者实现比较麻烦。

- **代码内聚方面**：前者将功能密切相关的类写成内部类，如ViewHolder，Adapter。而后者没有。

### 1、Adapter用法

- ListView 的 adapter

```java
class MyAdapter extends BaseAdapter {    
    ……    
    @Override    
    public View getView(int position, View convertView, ViewGroup parent) {    
       ViewHolder holder = null;    
        if (convertView == null) {    
            holder = new ViewHolder();    
            …… //初始化convertView和ViewHolder    
            convertView.setTag(holder);    
        } else {    
            holder = (ViewHolder) convertView.getTag();    
        }    
       ……    
    }    
}    
static class ViewHolder {    
   TextView txvTitle;    
} 
```



- RecyclerView 的 adapter

```java
class MyAdapter extends RecyclerView.Adapter<MyAdapter.MyViewHolder> {

       @Override
       public int getItemCount() {
           return 0;
       }

       @Override
       public MyViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
           return null;
       }

       @Override
       public void onBindViewHolder(MyViewHolder holder, int position) {

       }

       class MyViewHolder extends RecyclerView.ViewHolder {
           //TODO 初始化控件   
           
           public MyViewHolder(View itemView) {
               super(itemView);
           }
       }
    }
```



**ListView**相比RecyclerView，有一些**优点**：

- **addHeaderView()**, **addFooterView()**添加头视图和尾视图。
- 通过**”android:divider”**设置自定义分割线。
- **setOnItemClickListener()**和**setOnItemLongClickListener()**设置点击事件和长按事件。

这些功能在RecyclerView中都没有直接的接口，要自己实现（虽然实现起来很简单），因此如果只是实现简单的显示功能，ListView无疑更简单。

**RecyclerView**相比ListView，有一些明显的**优点**：

- 默认**已**经**实现**了**View**的**复用**，不需要类似`if(convertView == null)`的实现，而且回收机制更加完善。
- 默认**支持局部刷新**。
- 容易实现**添加item、删除item**的**动画**效果。
- 容易实现**拖拽、侧滑删除**等功能。

RecyclerView是一个插件式的实现，对各个功能进行解耦，从而扩展性比较好。



### 链接

[简书的。eeeee有点长](https://www.jianshu.com/p/01f161cb498c)

