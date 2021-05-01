[TOC]

### 1、简介

自定义的TagView，具备选中和未选中状态。



### 2、源代码

```java
public class TagView extends FrameLayout implements Checkable
{
    private boolean isChecked;
    private static final int[] CHECK_STATE = new int[]{android.R.attr.state_checked};

    public TagView(Context context)
    {
        super(context);
    }

    public View getTagView()
    {
        return getChildAt(0);
    }

    @Override
    public int[] onCreateDrawableState(int extraSpace)
    {
        int[] states = super.onCreateDrawableState(extraSpace + 1);
        if (isChecked())
        {
         	// 如果选中，将父类的结果和选中状态合并之后返回
            mergeDrawableStates(states, CHECK_STATE);
        }
        // 如果未选中，直接返回父类的结果
        return states;
    }

    /**
     * Change the checked state of the view
     *
     * @param checked The new checked state
     */
    @Override
    public void setChecked(boolean checked)
    {
        if (this.isChecked != checked)
        {
            this.isChecked = checked;
            refreshDrawableState();          //刷新状态
        }
    }

    /**
     * @return The current checked state of the view
     */
    @Override
    public boolean isChecked()
    {
        return isChecked;
    }

    /**
     * Change the checked state of the view to the inverse of its current state
     * //状态改变时调用
     */
    @Override
    public void toggle()
    {
        setChecked(!isChecked);
    }
}
```



### 3、重要方法

- 方法：protected void drawableStateChanged() 
  - javadoc： 这个方法会在 View 的状态改变并影响到正在显示的 drawable 的状态的时候会被调用。如果这个 View 拥有一个 StateListAnimator 的话，这个 StateListAnimator也会被调用来执行必要的状态改变动画。如果重写这个方法的话要确保调用父类的方法。 
  - View 中的实现： 调用 getDrawableState() 获得当前的 drawable state，并把它赋值给 mBackground 和 mStateListAnimator。 
  - ViewGroup 中的实现： 如果有 FLAG_NOTIFY_CHILDREN_ON_DRAWABLE_STATE_CHANGE 标志的话，还会遍历每个子 View，如果子 View 有 DUPLICATE_PARENT_STATE 标志，就调用子 View 的 refreshDrawableState() 方法。

- 方法： protected int[] onCreateDrawableState(int extraSpace) 
  - javadoc注释： 为 View 生成新的 Drawable 的状态。这个方法会在缓存的 Drawable state 被认为失效（invalid）之后被 view 系统所调用。如果要获取当前的状态，你应该使用 getDrawableState 方法。方法参数 extraSpace 如果不是0的话，这个值可以代表你希望返回的数组中除了装载当前的状态之外，额外的空间，你可以使用这些空间来存放你自己的状态。 
  - View 中的实现： 如果 View 被设置为和父 View 的 drawable state 一致，返回父 view 的 drawable state。否则从各种 Flag 中获取 view 的状态信息并封装到一个数组中返回（数组长度是收集到的信息数量加上参数 extraSpace 的大小）。 
  - ViewGroup 中的实现： 如果没有 FLAG_ADD_STATES_FROM_CHILDREN 标志的话直接调用父方法返回。否则除了调用父方法，还要遍历子 View，通过 getDrawableState 拿到它们的 drawable state，使用 mergeDrawableStates 合并到自己的 drawable state。

- 方法： public final int[] getDrawableState() 
  - javadoc： 返回一个资源 id 的数组，这些 id 代表着 View 的当前状态。 
    View中的实现： 如果没有 PFLAG_DRAWABLE_STATE_DIRTY 标志，直接返回缓存的 mDrawableState；否则，调用 onCreateDrawableState 获取新的状态返回，并把 PFLAG_DRAWABLE_STATE_DIRTY 标志去掉。 
  - ViewGroup中的实现： 没有重载。

- 方法： protected static int[] mergeDrawableStates(int[] baseState, int[] additionalState) 
  - javadoc： 将你存储在 additionalState 中的状态和 baseState 中的状态合并到一起（baseState 通常是由 getDrawableState 方法得到的），为了简化，baseState 会作为参数被返回，也就是，baseArray 中必须提前预留存放 additionalState 的空间，否则也无法合并成功。 
  - View中的实现： 从 baseState 数组第一个为 0 的元素开始，将 additionalState 数组中的内容拷贝过来，最后把 baseState 返回。 
  - ViewGroup中的实现： 没有重载。

- 方法： public void refreshDrawableState() 
  - javadoc： 这个方法被调用来强制更新一个 View 的 drawable state。这个方法会导致 View 的 drawableStateChanged 方法被调用。如果对新的 drawable state 感兴趣，可以通过 getDrawableState 方法获取。 
  - View中的实现： 设置上 PFLAG_DRAWABLE_STATE_DIRTY 标志，调用 drawableStateChanged()，如果有父 View，调用父 View 的 childDrawableStateChanged() 方法。 
    ViewGroup中的实现： 没有重载。

- 方法： public void childDrawableStateChanged(View child) 
  - javadoc： 这个是 ViewGroup 的方法。如果有 FLAG_ADD_STATES_FROM_CHILDREN 标志的话，刷新这个 ViewGroup 的 drawable state，将子 View 的状态加入进去. 
  - View中的实现： 没有定义。 
  - ViewGroup中的实现： 如果有FLAG_ADD_STATES_FROM_CHILDREN 标志，调用 refreshDrawableState() 方法。

#### 调用的流程：

- 当 View 的状态（onCreateDrawableState方法需要收集的各种标志）发生变化的时候，会调用 refreshDrawableState 方法。
- 在 refreshDrawableState 方法中会设置上 PFLAG_DRAWABLE_STATE_DIRTY 标志，然后调用 drawableStateChanged 方法。
- 在 drawableStateChanged 方法中，会调用 getDrawableState 方法获取当前状态，并把状态赋值给 mBackground 和 StateListAnimator。
- 在 getDrawableState 方法中，会发现存在 PFLAG_DRAWABLE_STATE_DIRTY 标志，缓存的 drawable state 已经失效，就会依次查看所有与 drawable state 相关的标志，组装新的 drawable state 返回。



### 4、自定义的控件中的 drawable 可以响应状态的变化

你在编程中可能会遇到这样的问题，当你自定义一个控件，而这个控件中除了有背景还可能会有其他的图案，你希望当你按在控件上的时候，所有的图案都发生状态的变化，但结果是只有背景发生了状态的改变。其他图案没有响应状态的变化，为什么呢？因为这些 drawable 没有被设置相应的状态。

想要让自己的 drawable 也像 mBackground 一样随着控件的状态变化而变化，就需要把上面那些方法中用到 mBackground 的位置也加上我们自己的 drawable 的操作，也就是 drawableStateChanged() 方法：

```java
@Override
protected void drawableStateChanged() {
    super.drawableStateChanged();
    // mDrawalbe 是我们自己的 drawable 对象，如果有更多，需要每个都进行这样的操作
    Drawable d = mDrawable;
    if (d != null && d.isStateful()) {
    d.setState(getDrawableState());
    }
 }
```



### 5、tips

- Drawable类中有一对方法getIntrinsicWidth()和getIntrinsicHeight()：顾名思议让我开始觉得它们得到的是drawable图片的原始固有宽高，并不是drawable本身固有的属性，同一个drawable会随着硬件软件环境的影响都会变化，与设备相关。



### 6、链接

- [自定义Drawable State](https://github.com/CharlesHarley/Example-Android-CustomDrawableStates)

- [Android 中 View 的中的 DrawableState](https://blog.csdn.net/zhaoshecsdn/article/details/46779333)