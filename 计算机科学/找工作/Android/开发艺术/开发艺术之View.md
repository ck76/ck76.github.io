#### 在Android中的任何一个布局、任何一个控件其实都是直接或间接继承自View的，因此View是一个很重要的概念。本篇将深入学习View，内容如下：

- View事件体系
  - View位置参数
  - View的触控
  - View的滑动
  - View事件分发机制
  - View滑动冲突
- View工作原理
  - View工作流程
  - 自定义View

------

**简介：在Android的世界中View是所有控件的基类，其中也包括ViewGroup在内，ViewGroup是代表着控件的集合，其中可以包含多个View控件。**
**从某种角度上来讲Android中的控件可以分为两大类：View与ViewGroup。通过ViewGroup，整个界面的控件形成了一个树形结构，上层的控件要负责测量与绘制下层的控件，并传递交互事件。**
**在每棵控件树的顶部都存在着一个ViewParent对象，它是整棵控件树的核心所在，所有的交互管理事件都由它来统一调度和分配，从而对整个视图进行整体控制。**

![img](https://upload-images.jianshu.io/upload_images/5494434-536e64899eb9584e?imageMogr2/auto-orient/strip|imageView2/2/w/530)

------

**一.View事件体系**

1.View的**位置参数**

a.Android坐标系：以屏幕的左上角为坐标原点，**向右**为x轴增大方向，**向下**为y轴增大方向。

b.View的位置由它的四个顶点决定，分别对应View的四个属性：**top、left、right、bottom**。其中left是左上角的横坐标，right是右下角的横坐标，top是左上角的纵坐标，bottom是右下角的纵坐标。注意这些坐标是**相对于view父容器**而言，是一种相对的坐标。具体关系见下图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoy4bottj30c50bkt8t.jpg)

> 因此，View的宽高和坐标关系：width = right - left，height = bottom - top。

可利用View的get方法获取上述属性，如：

- left = getLeft();
- right = getRight();
- top = getTop();
- bottom = getBottom();
- width=getWidth();
- height=getHeight();

c.从android3.0开始，View增加了额外几个参数：**x，y，translationX、translationY**。其中x和y是View左上角的坐标，translationX和translationY是View左上角相对于父容器的偏移量，它们默认值是0。这些参数也是**相对于View父容器**。具体关系见下图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoy2zuqrj30kl0hdjry.jpg)

> - 存在关系：x = left + translationX，y = top + translationY
> - 由此可见，x和left不同体现在：left是View的初始坐标，在绘制完毕后就不会再改变；而x是View偏移后的实时坐标，是实际坐标。y和top的区别同理。

类似地，安卓也提供了相应的get/set方法。需要注意的是，在onCreate()方法里无法获取到View的坐标参数，这是因为此时View还未开始绘制，全部坐标参数将都是0。

**推荐阅读**：[Android应用坐标系统全面详解](https://links.jianshu.com/go?to=http%3A%2F%2Fblog.csdn.net%2Fyanbober%2Farticle%2Fdetails%2F50419117%2F)

------

2.触控系列

a.**MotionEvent**：是手指触摸屏幕所产生的一系列事件。典型事件有：

- **ACTION_DOWN**：手指刚接触屏幕
- **ACTION_MOVE**：手指在屏幕上滑动
- **ACTION_UP**：手指在屏幕上松开的一瞬间

> 事件列：从手指接触屏幕至手指离开屏幕，这个过程产生的一系列事件
> 任何事件列都是以DOWN事件开始，UP事件结束，中间有无数的MOVE事件。如图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoy1jmh6j30ri03uwel.jpg)

通过MotionEvent 对象可以得到触摸事件的**x、y**坐标。其中通过getX()、getY()可获取相对于当前view左上角的x、y坐标；通过getRawX()、getRawY()可获取相对于手机屏幕左上角的x，y坐标。具体关系见下图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxzlfluj30e60fddgb.jpg)

b.**TouchSlop**：系统所能识别的被认为是**滑动的最小距离**。即当手指在屏幕上滑动时，如果两次滑动之间的距离小于这个常量，那么系统就不认为你是在进行滑动操作。

> 该常量和设备有关，可用它来判断用户的滑动是否达到阈值，获取方法：**ViewConfiguration.get(getContext()).getScaledTouchSlop()**。

c.**VelocityTracker**：速度追踪，用于追踪手指在滑动过程中的速度，包括水平和竖直方向的速度。

使用过程：首先在view的**onTouchEvent方法**中追踪当前单击事件的速度：



```csharp
VelocityTracker velocityTracker = VelocityTracker.obtain();//实例化一个VelocityTracker 对象
velocityTracker.addMovement(event);//添加追踪事件
```

接着在**ACTION_UP事件**中获取当前的速度。注意这里计算的是1000ms时间间隔移动的像素值，假设像素是100，即速度是每秒100像素。另外，手指逆着坐标系的正方向滑动，所产生的速度为负值，顺着正反向滑动，所产生的速度为正值。



```cpp
velocityTracker .computeCurrentVelocity(1000);//获取速度前先计算速度，这里计算的是在1000ms内
float xVelocity = velocityTracker .getXVelocity();//得到的是1000ms内手指在水平方向从左向右滑过的像素数，即水平速度
float yVelocity = velocityTracker .getYVelocity();//得到的是1000ms内手指在水平方向从上向下滑过的像素数，垂直速度
```

最后，当不需要使用它的时候，需要调用clear方法来重置并回收内存：



```css
velocityTracker.clear();
velocityTracker.recycle();
```

**推荐阅读**：[Android常用触控类分析：MotionEvent 、 ViewConfiguration、VelocityTracker](https://www.jianshu.com/p/e7200875384c)

d.**GestureDetector**：手势检测，用于辅助检测用户的单击、滑动、长按、**双击**等行为。

使用过程：创建一个GestureDetecor对象并实现OnGestureListener接口，根据需要实现单击等方法：



```cpp
GestureDetector mGestureDetector = new GestureDetector(this);//实例化一个GestureDetector对象
mGestureDetector.setIsLongpressEnabled(false);// 解决长按屏幕后无法拖动的现象
```

接着，接管目标view的onTouchEvent方法，在待监听view的onTouchEvent方法中添加如下实现：



```csharp
boolean consume = mGestureDetector.onTouchEvent(event);
return consume;
```

然后，就可以有选择的实现OnGestureListener和OnDoubleTapListener中的方法了。

> 建议：如果只是监听滑动操作，建议在onTouchEvent中实现；如果要监听双击这种行为，则使用GestureDetector 。

**推荐阅读**:[Android手势检测——GestureDetector全面分析](https://www.jianshu.com/p/95b3fdc46b0f)

------

3.滑动系列

a.实现**View滑动**三种办法：

①通过View本身提供的**scrollTo/scrollBy**方法

> - 两者区别：scrollBy是内部调用了scrollTo的，它是基于当前位置的**相对滑动**；而scrollTo是**绝对滑动**，因此如果利用相同输入参数多次调用scrollTo()方法，由于View初始位置是不变只会出现一次View滚动的效果而不是多次。
> - 注意：两者都只能对**view内容**进行滑动，而不能使view本身滑动。

mScrollX和mScrollY分别表示View在X、Y方向的滚动距离。**mScrollX**：View的左边缘减去View的内容的左边缘；**mScrollY**：View的上边缘减去View的内容的上边缘。从右向左滑动，mScrollX为正值，反之为负值；从下往上滑动，mScrollY为正值，反之为负值。（更直观感受：查看下一张照片或者查看长图时手指滑动方向为正）

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxx8q95j30jg0dr0td.jpg)

绿色边框代表View在屏幕上对应的矩形区域，灰色阴影代表View的内容

推荐阅读：[scrollTo/scrollBy 使用详解](https://www.jianshu.com/p/859592b43d38)

②通过**动画**给View施加平移效果：主要通过改变View的translationX和translationY参数来实现。可用view动画，也可以采用属性动画，如果使用属性动画的话，为了能够兼容3.0以下版本，需要采用开源动画库nineoldandroids。注意View动画的View移动只是位置移动，并不能真正的改变view的位置，而属性动画可以。

③通过改变View的**LayoutParams**使得View重新布局：比如将一个View向右移动100像素，向右，只需要把它的marginLeft参数增大即可，代码见下：



```csharp
MarginLayoutParams params = (MarginLayoutParams) btn.getLayoutParams();
params.leftMargin += 100;
btn.requestLayout();// 请求重新对View进行measure、layout
```

> 三种方式对比：
>
> - scrollTo/scrollBy：操作简单，适合对view内容滑动。非平滑
> - 动画：操作简单，主要适用于没有交互的view和实现复杂的动画效果
> - 改变LayoutParams：操作稍微复杂，适用于有交互的view。非平滑

b.实现**View弹性滑动**三种方法：

①使用**Scroller**：

> - 与scrollTo/scrollBy不同：scrollTo/scrollBy过程是瞬间完成的，非平滑；而Scroller则有过渡滑动的效果。
> - 注意：Scoller本身无法让View弹性滑动，它需要和View的**computerScroller**方法配合使用。

Scroller惯用代码：



```java
Scroller scroller = new Scroller(mContext); //实例化一个Scroller对象

private void smoothScrollTo(int dstX, int dstY) {
  int scrollX = getScrollX();//View的左边缘到其内容左边缘的距离
  int scrollY = getScrollY();//View的上边缘到其内容上边缘的距离
  int deltaX = dstX - scrollX;//x方向滑动的位移量
  int deltaY = dstY - scrollY;//y方向滑动的位移量
  scroller.startScroll(scrollX, scrollY, deltaX, deltaY, 1000); //开始滑动
  invalidate(); //刷新界面
}

@Override//计算一段时间间隔内偏移的距离，并返回是否滚动结束的标记
public void computeScroll() {
  if (scroller.computeScrollOffset()) { 
    scrollTo(scroller.getCurrX(), scroller.getCurY());
    postInvalidate();//通过不断的重绘不断的调用computeScroll方法
  }
}
```

其中startScroll源码如下，可见它并没有进行实际的滑动操作，而是通过后续invalidate()方法去做滑动动作。



```cpp
public void startScroll(int startX,int startY,int dx,int dy,int duration){
  mMode = SCROLL_MODE;
  mFinished = false;
  mDuration = duration;//滑动时间
  mStartTime = AnimationUtils.currentAminationTimeMills();//开始时间
  mStartX = startX;//滑动起点
  mStartY = startY;//滑动起点
  mFinalX = startX + dx;//滑动终点
  mFinalY = startY + dy;//滑动终点
  mDeltaX = dx;//滑动距离
  mDeltaY = dy;//滑动距离
  mDurationReciprocal = 1.0f / (float)mDuration;
 }
```

> - 具体过程：在MotionEvent.ACTION_UP事件触发时调用startScroll方法->马上调用invalidate/postInvalidate方法->会请求View重绘，导致View.draw方法被执行->会调用View.computeScroll方法，此方法是空实现，需要自己处理逻辑。具体逻辑是：先判断computeScrollOffset，若为true（表示滚动未结束），则执行scrollTo方法，它会再次调用postInvalidate，如此反复执行，直到返回值为false。如图所示：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxuy9v7j30vw0kqwfm.jpg)

> - 原理：Scroll的computeScrollOffset()根据**时间的流逝**动态计算一小段时间里View滑动的距离，并得到当前View位置，再通过scrollTo继续滑动。即把一次滑动拆分成无数次小距离滑动从而实现弹性滑动。

推荐阅读： [站在源码的肩膀上全解Scroller工作机制](https://links.jianshu.com/go?to=http%3A%2F%2Fblog.csdn.net%2Flfdfhl%2Farticle%2Fdetails%2F53143114)

②通过**动画**：动画本身就是一种渐近的过程，故可通过动画来实现弹性滑动。方法是：



```cpp
ObjectAnimator.ofFloat(targetView,"translationX",0,100).setDuration(100).start();//在100ms内使得View从原始位置向右平移100像素
```

③使用**延时策略**：通过发送一系列延时信息从而达到一种渐近式的效果，具体可以通过[**Handler**和View的**postDelayed**方法](https://www.jianshu.com/p/3718766df5ba)，也可使用线程的**sleep**方法。

对弹性滑动完成总时间有精确要求的使用场景下，使用延时策略是一个不太合适的选择。

**推荐阅读**：[View滑动与实现滑动的几种方法](https://www.jianshu.com/p/19aaa6a0dfdc)

------

4.**View事件分发机制**

a.事件分发本质：就是对**MotionEvent事件**分发的过程。即当一个MotionEvent产生了以后，系统需要将这个点击事件传递到一个具体的View上。（关于MotionEvent介绍见本篇2.a）

b.点击事件的传递顺序：**Activity（Window） -> ViewGroup -> View**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxt6h71j30bf0fg74y.jpg)

**补充阅读**：[对Activity、View、Window的理解](https://www.jianshu.com/p/5297e307a688)

c.需要的三个主要方法：

- **dispatchTouchEvent**：进行事件的分发（传递）。返回值是 boolean 类型，受当前onTouchEvent和下级view的dispatchTouchEvent影响
- **onInterceptTouchEvent**：对事件进行拦截。该方法只在ViewGroup中有，View（不包含 ViewGroup）是没有的。一旦拦截，则执行ViewGroup的onTouchEvent，在ViewGroup中处理事件，而不接着分发给View。且只调用一次，所以后面的事件都会交给ViewGroup处理。
- **onTouchEvent**：进行事件处理。

事件分发过程图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxs3xhrj30ps0jdjst.jpg)

> - 事件分发是逐级下发的，目的是将事件传递给一个View。
> - ViewGroup一旦拦截事件，就不往下分发，同时调用onTouchEvent处理事件。

**推荐阅读：**[Android事件分发机制详解（源码）](https://www.jianshu.com/p/38015afcdb58)

------

5.**View滑动冲突**

a.产生原因：

- 一般情况下，在一个界面里存在内外两层可同时滑动的情况时，会出现滑动冲突现象。

b.可能场景：

- 外部滑动和内部滑动**方向不一致**：如ViewPager嵌套ListView(实际这么用没问题，因为ViewPager内部已处理过)。
- 外部滑动方向和内部滑动**方向一致**：如ScrollView嵌套ListView（实际上也已被解决）。
- 上面两种情况的嵌套

c.处理规则：

- 对场景一：当用户左右/上下滑动时让外部View拦截点击事件，当用户上下/左右滑动时让内部View拦截点击事件。即根据滑动的方向判断谁来拦截事件。关于判断是上下滑动还是左右滑动，可根据**滑动的距离**或者**滑动的角度**去判断。
- 对场景二：一般从**业务**上找突破点。即根据业务需求，规定何时让外部View拦截事件何时由内部View拦截事件。
- 对场景三：相对复杂，可同样根据需求在业务上找到突破点。

d.解决方式：

- 法一：

  外部拦截法

  - 含义：指点击事件都先经过**父容器的拦截处理**，如果父容器需要此事件就拦截，否则就不拦截。
  - 方法：需要重写父容器的**onInterceptTouchEvent**方法，在内部做出相应的拦截。以下是伪代码：



```csharp
//重写父容器的拦截方法
public boolean onInterceptTouchEvent (MotionEvent event){
    boolean intercepted = false;
    int x = (int) event.getX();
    int y = (int) event.getY();
    switch (event.getAction()) {
      case MotionEvent.ACTION_DOWN://对于ACTION_DOWN事件必须返回false，一旦拦截后续事件将不能传递给子View
         intercepted = false;
         break;
      case MotionEvent.ACTION_MOVE://对于ACTION_MOVE事件根据需要决定是否拦截
         if (父容器需要当前事件） {
             intercepted = true;
         } else {
             intercepted = flase;
         }
         break;
   }
      case MotionEvent.ACTION_UP://对于ACTION_UP事件必须返回false，一旦拦截子View的onClick事件将不会触发
         intercepted = false;
         break;
      default : break;
   }
    mLastXIntercept = x;
    mLastYIntercept = y;
    return intercepted;
   }
```

- 法二：

  内部拦截法

  ：

  - 含义：指父容器不拦截任何事件，而将所有的事件都传递给子容器，如果子容器需要此事件就直接消耗，否则就交由父容器进行处理。
  - 方法：需要配合**requestDisallowInterceptTouchEvent**方法。以下是子View的dispatchTouchEvent方法的伪代码：



```csharp
public boolean dispatchTouchEvent ( MotionEvent event ) {
  int x = (int) event.getX();
  int y = (int) event.getY();

  switch (event.getAction) {
      case MotionEvent.ACTION_DOWN:
         parent.requestDisallowInterceptTouchEvent(true);//为true表示禁止父容器拦截
         break;
      case MotionEvent.ACTION_MOVE:
         int deltaX = x - mLastX;
         int deltaY = y - mLastY;
         if (父容器需要此类点击事件) {
             parent.requestDisallowInterceptTouchEvent(false);
         }
         break;
      case MotionEvent.ACTION_UP:
         break;
      default :
         break;        
 }

  mLastX = x;
  mLastY = y;
  return super.dispatchTouchEvent(event);
}
```

除子容器需要做处理外，父容器也要**默认拦截除了ACTION_DOWN以外的其他事件**，这样当子容器调用parent.requestDisallowInterceptTouchEvent(false)方法时，父元素才能继续拦截所需的事件。因此，父View需要重写onInterceptTouchEvent方法：



```csharp
public boolean onInterceptTouchEvent (MotionEvent event) {
 int action = event.getAction();
 if(action == MotionEvent.ACTION_DOWN) {
     return false;
 } else {
     return true;
 }
}
```

> 内部拦截法要求父容器不能拦截ACTION_DOWN的原因：由于该事件并不受**FLAG_DISALLOW_INTERCEPT**（由requestDisallowInterceptTouchEvent方法设置）标记位控制，一旦ACTION_DOWN事件到来，该标记位会被重置。所以一旦父容器拦截了该事件，那么所有的事件都不会传递给子View，内部拦截法也就失效了。

**推荐阅读**：[一文解决Android View滑动冲突](https://www.jianshu.com/p/982a83271327)

------

**二.View工作原理**

1.**View工作流程：measure测量->layout布局->draw绘制**

- measure确定View的**测量宽高**
- layout确定View的**最终宽高**和**四个顶点的位置**
- draw将View **绘制到屏幕**上
- 对应onMeasure()、onLayout()、onDraw()三个方法。

> **具体过程：**
>
> - ViewRoot对应于ViewRootImpl类，它是连接WindowManager和DecorView的纽带。
> - View的绘制流程是从**ViewRoot**和**performTraversals**开始。
> - performTraversals()依次调用performMeasure()、performLayout()和performDraw()三个方法，分别完成**顶级** View的绘制。
> - 其中，performMeasure()会调用measure()，measure()中又调用onMeasure()，实现对其所有子元素的measure过程，这样就完成了一次measure过程；接着子元素会重复父容器的measure过程，如此反复至完成整个View树的遍历。layout和draw同理。过程图如下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxohip5j30gi0dm752.jpg)

**补充阅读**：[了解ViewRoot和DecorView](https://www.jianshu.com/p/3e3a488cb1bb)

a.**measure过程**：确定测量宽高

> 先来理解**MeasureSpec**：
>
> - 作用：通过宽测量值**widthMeasureSpec**和高测量值**heightMeasureSpec**决定View的大小
> - 组成：一个32位int值，高2位代表**SpecMode**(测量模式)，低30位代表**SpecSize**( 某种测量模式下的规格大小)。
> - 三种模式：
>   - **UNSPECIFIED**：父容器不对View有任何限制，要多大有多大。常用于系统内部。
>   - **EXACTLY**(精确模式)：父视图为子视图指定一个确切的尺寸SpecSize。对应LyaoutParams中的**match_parent**或**具体数值**。
>   - **AT_MOST**(最大模式)：父容器为子视图指定一个最大尺寸SpecSize，View的大小不能大于这个值。对应LayoutParams中的**wrap_content**。
> - 决定因素：值由子**View的布局参数LayoutParams**和**父容器的MeasureSpec值**共同决定。具体规则见下图：

![img](https://upload-images.jianshu.io/upload_images/5494434-5729db79455bb4d2?imageMogr2/auto-orient/strip|imageView2/2/w/700)

现在，分别讨论两种measure过程：

①View的measure：只有一个原始的View，通过measure()即可完成测量。过程图见下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxm5d3dj30ql0e8gmx.jpg)

View的measure过程图

> 从getDefaultSize()中可以看出，直接继承View的自定义View需要重写onMeasure()并设置wrap_content时的自身大小，否则效果相当于macth_parent。解决上述问题的典型代码：



```java
@Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec,heightMeasureSpec);
        
int widthSpecMode = MeasureSpec.getMode(widthMeasureSpec);
        int widthSpecSize = MeasureSpec.getSize(widthMeasureSpec);
        int heightSpecMode = MeasureSpec.getMode(heightMeasureSpec);
        int heightSpecSize = MeasureSpec.getSize(heightMeasureSpec);
        //分析模式，根据不同的模式来设置
        if(widthSpecMode == MeasureSpec.AT_MOST && heightSpecMode == MeasureSpec.AT_MOST){
            setMeasuredDimension(mWidth,mHeight);
        }else if(widthSpecMode == MeasureSpec.AT_MOST){
            setMeasuredDimension(mWidth,heightSpecSize);
        }else if(heightSpecMode == MeasureSpec.AT_MOST){
            setMeasuredDimension(widthSpecSize,mHeight);
        }
    }
```

**补充阅读：**[为什么你的自定义View wrap_content不起作用](https://www.jianshu.com/p/ca118d704b5e)

②ViewGroup的measure：除了完成ViewGroup自身的测量外，还会遍历去调用所有子元素的measure方法。

> ViewGroup中没有重写onMeasure()，而是提供**measureChildren()**。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxk9vw4j308l0fujrz.jpg)

ViewGroup的measure过程图

**图片来源**：[自定义View Measure过程](https://www.jianshu.com/p/1dab927b2f36)

b.**layout过程**：确定View的最终宽高和四个顶点的位置

> - 大致流程：从顶级View开始依次调用layout()，其中子View的layout()会调用setFrame()来设定自己的四个顶点（mLeft、mRight、mTop、mBottom），接着调用onLayout()来确定其坐标，注意该方法是空方法，因为不同的ViewGroup对其子View的布局是不相同的。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxivh26j30qy0iwmy7.jpg)

layout过程图

**图片来源**：[自定义View Layout过程](https://www.jianshu.com/p/158736a2549d)

c.**draw过程**：绘制到屏幕

绘制顺序：

- 绘制背景：**background.draw(canvas)**
- 绘制自己：**onDraw(canvas)**
- 绘制children：**dispatchDraw(canvas)**
- 绘制装饰：**onDrawScrollBars(canvas)**

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxgln25j30jg0vd0v1.jpg)

draw过程图

> **注意**：Vew有一个特殊的方法**setWillNotDraw()**，该方法用于设置 **WILL_NOT_DRAW** 标记位（其作用是当一个View不需要绘制内容时，系统可进行相应优化）。**默认**情况下View是**没有**这个优化标志的（设为true）。

**图片来源**：[自定义View Draw过程](https://www.jianshu.com/p/95afeb7c8335)

**推荐阅读**：[对View工作流程的理解（源码)](https://www.jianshu.com/p/dc32ed4a384e)

------

2.自定义View

a.自定义View的**类型**：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxeqgd0j30jg0ck0vv.jpg)

b.**特别提醒**：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvoxdcil0j30jg08o0vx.jpg)

最后，因为自定义View内容非常多，这里不再展开。最重要的是实践，就是现在带着理论基础开始实战吧~

------





作者：厘米姑娘
链接：https://www.jianshu.com/p/06ff0dfeed39
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

---

### requestLayout：

从方法名字可以知道，“请求布局”，那就是说，如果调用了这个方法，那么对于一个子View来说，应该会重新进行布局流程。但是，真实情况略有不同，如果子View调用了这个方法，其实会从**View树重新进行一次测量、布局、绘制这三个流程**，最终就会显示子View的最终情况。

### invalidate：

当子View调用了invalidate方法后，会为该View添加一个标记位，同时不断向父容器请求刷新，父容器通过计算得出自身需要重绘的区域，直到传递到ViewRootImpl中，最终触发performTraversals方法，进行开始View树重绘流程(只绘制需要重绘的视图)。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gm0m2b3h0tj30a709tq46.jpg)

 

### postInvalidate：

postInvalidate是在非UI线程中调用，invalidate则是在UI线程中调用。 