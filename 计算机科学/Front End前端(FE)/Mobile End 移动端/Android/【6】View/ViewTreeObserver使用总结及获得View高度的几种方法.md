[TOC]

[ViewTreeObserver](https://link.jianshu.com/?t=https://developer.android.com/reference/android/view/ViewTreeObserver.html) 注册一个观察者来监听视图树，当视图树的布局、视图树的焦点、视图树将要绘制、视图树滚动等发生改变时，ViewTreeObserver都会收到通知，ViewTreeObserver不能被实例化，可以调用View.getViewTreeObserver()来获得。

**ViewTreeObserver继承关系：**

```css
public final class ViewTreeObserverextendsObject

java.lang.Object
↳android.view.ViewTreeObserver
```

ViewTreeObserver直接继承自Object.

ViewTreeObserver提供了View的多种监听，每一种监听都有一个内部类接口与之对应，内部类接口全部保存在CopyOnWriteArrayList中，通过ViewTreeObserver.addXXXListener()来添加这些监听，源码如下：

```java
public final class ViewTreeObserver {
    // Recursive listeners use CopyOnWriteArrayList
    private CopyOnWriteArrayList<OnWindowFocusChangeListener> mOnWindowFocusListeners;
    private CopyOnWriteArrayList<OnWindowAttachListener> mOnWindowAttachListeners;
    private CopyOnWriteArrayList<OnGlobalFocusChangeListener> mOnGlobalFocusListeners;
    private CopyOnWriteArrayList<OnTouchModeChangeListener> mOnTouchModeChangeListeners;
    private CopyOnWriteArrayList<OnEnterAnimationCompleteListener> mOnEnterAnimationCompleteListeners;

    // Non-recursive listeners use CopyOnWriteArray
    // Any listener invoked from ViewRootImpl.performTraversals() should not be recursive
    private CopyOnWriteArray<OnGlobalLayoutListener> mOnGlobalLayoutListeners;
    private CopyOnWriteArray<OnComputeInternalInsetsListener> mOnComputeInternalInsetsListeners;
    private CopyOnWriteArray<OnScrollChangedListener> mOnScrollChangedListeners;
    private CopyOnWriteArray<OnPreDrawListener> mOnPreDrawListeners;
    private CopyOnWriteArray<OnWindowShownListener> mOnWindowShownListeners;

    // These listeners cannot be mutated during dispatch
    private ArrayList<OnDrawListener> mOnDrawListeners;
}
```

以OnGlobalLayoutListener为例，首先是定义接口：

```csharp
 public interface OnGlobalLayoutListener {
        /**
         * Callback method to be invoked when the global layout state or the visibility of views
         * within the view tree changes
         */
        public void onGlobalLayout();
    }
```

将OnGlobalLayoutListener 添加到CopyOnWriteArray数组中：

```java
  /**
     * Register a callback to be invoked when the global layout state or the visibility of views
     * within the view tree changes
     *
     * @param listener The callback to add
     *
     * @throws IllegalStateException If {@link #isAlive()} returns false
     */
    public void addOnGlobalLayoutListener(OnGlobalLayoutListener listener) {
        checkIsAlive();

        if (mOnGlobalLayoutListeners == null) {
            mOnGlobalLayoutListeners = new CopyOnWriteArray<OnGlobalLayoutListener>();
        }

        mOnGlobalLayoutListeners.add(listener);
    }
```

移除OnGlobalLayoutListener，当视图树布局发生变化时不会再收到通知了：

```java
/**
     * Remove a previously installed global layout callback
     *
     * @param victim The callback to remove
     *
     * @throws IllegalStateException If {@link #isAlive()} returns false
     * 
     * @deprecated Use #removeOnGlobalLayoutListener instead
     *
     * @see #addOnGlobalLayoutListener(OnGlobalLayoutListener)
     */
    @Deprecated
    public void removeGlobalOnLayoutListener(OnGlobalLayoutListener victim) {
        removeOnGlobalLayoutListener(victim);
    }

    /**
     * Remove a previously installed global layout callback
     *
     * @param victim The callback to remove
     *
     * @throws IllegalStateException If {@link #isAlive()} returns false
     * 
     * @see #addOnGlobalLayoutListener(OnGlobalLayoutListener)
     */
    public void removeOnGlobalLayoutListener(OnGlobalLayoutListener victim) {
        checkIsAlive();
        if (mOnGlobalLayoutListeners == null) {
            return;
        }
        mOnGlobalLayoutListeners.remove(victim);
    }
```

其他常用方法：

**dispatchOnGlobalLayout()：**视图树发生改变时通知观察者，如果想在View Layout 或 View hierarchy 还未依附到Window时，或者在View处于GONE状态时强制布局，这个方法也可以手动调用。

```csharp
   /**
     * Notifies registered listeners that a global layout happened. This can be called
     * manually if you are forcing a layout on a View or a hierarchy of Views that are
     * not attached to a Window or in the GONE state.
     */
    public final void dispatchOnGlobalLayout() {
        // NOTE: because of the use of CopyOnWriteArrayList, we *must* use an iterator to
        // perform the dispatching. The iterator is a safe guard against listeners that
        // could mutate the list by calling the various add/remove methods. This prevents
        // the array from being modified while we iterate it.
        final CopyOnWriteArray<OnGlobalLayoutListener> listeners = mOnGlobalLayoutListeners;
        if (listeners != null && listeners.size() > 0) {
            CopyOnWriteArray.Access<OnGlobalLayoutListener> access = listeners.start();
            try {
                int count = access.size();
                for (int i = 0; i < count; i++) {
                    access.get(i).onGlobalLayout();
                }
            } finally {
                listeners.end();
            }
        }
    }
```

**dispatchOnPreDraw()：**通知观察者绘制即将开始，如果其中的某个观察者返回 true，那么绘制将会取消，并且重新安排绘制，如果想在View Layout 或 View hierarchy 还未依附到Window时，或者在View处于GONE状态时强制绘制，可以手动调用这个方法。

```java
 /**
     * Notifies registered listeners that the drawing pass is about to start. If a
     * listener returns true, then the drawing pass is canceled and rescheduled. This can
     * be called manually if you are forcing the drawing on a View or a hierarchy of Views
     * that are not attached to a Window or in the GONE state.
     *
     * @return True if the current draw should be canceled and resceduled, false otherwise.
     */
    @SuppressWarnings("unchecked")
    public final boolean dispatchOnPreDraw() {
        boolean cancelDraw = false;
        final CopyOnWriteArray<OnPreDrawListener> listeners = mOnPreDrawListeners;
        if (listeners != null && listeners.size() > 0) {
            CopyOnWriteArray.Access<OnPreDrawListener> access = listeners.start();
            try {
                int count = access.size();
                for (int i = 0; i < count; i++) {
                    cancelDraw |= !(access.get(i).onPreDraw());
                }
            } finally {
                listeners.end();
            }
        }
        return cancelDraw;
    }
```

**ViewTreeObserver常用内部类：**

| 内部类接口                                   | 备注                                                         |
| -------------------------------------------- | ------------------------------------------------------------ |
| ViewTreeObserver.OnPreDrawListener           | 当视图树将要被绘制时，会调用的接口                           |
| ViewTreeObserver.OnGlobalLayoutListener      | 当视图树的布局发生改变或者View在视图树的可见状态发生改变时会调用的接口 |
| ViewTreeObserver.OnGlobalFocusChangeListener | 当一个视图树的焦点状态改变时，会调用的接口                   |
| ViewTreeObserver.OnScrollChangedListener     | 当视图树的一些组件发生滚动时会调用的接口                     |
| ViewTreeObserver.OnTouchModeChangeListener   | 当视图树的触摸模式发生改变时，会调用的接口                   |

**获得View高度的几种方式：**

我们应该都遇到过在onCreate()方法里面调用view.getWidth()和view.getHeight()获取到的view的宽高都是0的情况，这是因为在onCreate()里还没有执行测量，需要在onResume()之后才能得到正确的高度，那么可不可以在onCreate()里就得到宽高呢？答：可以！常用的有下面几种方式：

**1、通过设置View的MeasureSpec.UNSPECIFIED来测量：**

```cpp
int w = View.MeasureSpec.makeMeasureSpec(0, View.MeasureSpec.UNSPECIFIED);
int h = View.MeasureSpec.makeMeasureSpec(0, View.MeasureSpec.UNSPECIFIED);
view.measure(w, h);
//获得宽高
int viewWidth=view.getMeasuredWidth();
int viewHeight=view.getMeasuredHeight();
```

设置我们的SpecMode为UNSPECIFIED，然后去调用onMeasure测量宽高，就可以得到宽高。

**2、通过ViewTreeObserver .addOnGlobalLayoutListener来获得宽高，当获得正确的宽高后，请移除这个观察者，否则回调会多次执行：**

```java
//获得ViewTreeObserver 
ViewTreeObserver observer=view.getViewTreeObserver();
//注册观察者，监听变化
observer.addOnGlobalLayoutListener(new ViewTreeObserver.OnGlobalLayoutListener() {
     @Override
     public void onGlobalLayout() {
            //判断ViewTreeObserver 是否alive，如果存活的话移除这个观察者
           if(observer.isAlive()){
             observer.removeGlobalOnLayoutListener(this);
             //获得宽高
             int viewWidth=view.getMeasuredWidth();
             int viewHeight=view.getMeasuredHeight();
           }
        }
   });
```

**3、通过ViewTreeObserver .addOnPreDrawListener来获得宽高，在执行onDraw之前已经执行了onLayout()和onMeasure()，可以得到宽高了，当获得正确的宽高后，请移除这个观察者，否则回调会多次执行**

```java
//获得ViewTreeObserver 
ViewTreeObserver observer=view.getViewTreeObserver();
//注册观察者，监听变化
observer.addOnPreDrawListener(new ViewTreeObserver.OnPreDrawListener() {
       @Override
       public boolean onPreDraw() {
          if(observer.isAlive()){
            observer.removeOnDrawListener(this);
             }
          //获得宽高
           int viewWidth=view.getMeasuredWidth();
           int viewHeight=view.getMeasuredHeight();
           return true;
     }
   });
```

