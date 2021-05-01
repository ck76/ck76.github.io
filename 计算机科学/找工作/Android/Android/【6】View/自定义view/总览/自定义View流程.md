[TOC]

自定义`View`是一个老生常谈的问题，对于一个`Android`开发者来说是必须掌握的知识点，也是`Android`开发进阶的必经之路。

要想安卓理解自定义View的流程，首先我们要了解View的绘制流程。分析之前，我们先来看底下面这张图：

## View的绘制流程

[![img](https://camo.githubusercontent.com/af4ce7e2f12b76bf3921aaa215ab77dbe710f38f/68747470733a2f2f68656e636861742e6e6f732d656173746368696e61312e3132362e6e65742f7363616e612e706e67)](https://camo.githubusercontent.com/af4ce7e2f12b76bf3921aaa215ab77dbe710f38f/68747470733a2f2f68656e636861742e6e6f732d656173746368696e61312e3132362e6e65742f7363616e612e706e67)

DecorView是一个应用窗口的根容器，它本质上是一个FrameLayout。DecorView有唯一一个子View，它是一个垂直LinearLayout，包含两个子元素，一个是TitleView（ActionBar的容器），另一个是ContentView（窗口内容的容器）。关于ContentView，它是一个FrameLayout（android.R.id.content)，我们平常用的setContentView就是设置它的子View。上图还表达了每个Activity都与一个Window（具体来说是PhoneWindow）相关联，用户界面则由Window所承载。

## ViewRoot

在介绍View的绘制前，首先我们需要知道是谁负责执行View绘制的整个流程。实际上，View的绘制是由ViewRoot来负责的。每个应用程序窗口的decorView都有一个与之关联的ViewRoot对象，这种关联关系是由WindowManager来维护的。

那么decorView与ViewRoot的关联关系是在什么时候建立的呢？答案是Activity启动时，ActivityThread.handleResumeActivity()方法中建立了它们两者的关联关系。这里我们不具体分析它们建立关联的时机与方式，感兴趣的同学可以参考相关源码。下面我们直入主题，分析一下ViewRoot是如何完成View的绘制的。

## View绘制的起点

当建立好了decorView与ViewRoot的关联后，ViewRoot类的requestLayout()方法会被调用，以完成应用程序用户界面的初次布局。实际被调用的是ViewRootImpl类的requestLayout()方法，这个方法的源码如下：

```java
@Override
public void requestLayout() {
  if (!mHandlingLayoutInLayoutRequest) {
    // 检查发起布局请求的线程是否为主线程 
    checkThread();
    mLayoutRequested = true;
    scheduleTraversals();
  }
}
```

上面的方法中调用了scheduleTraversals()方法来调度一次完成的绘制流程，该方法会向主线程发送一个“遍历”消息，最终会导致ViewRootImpl的performTraversals()方法被调用。下面，我们以performTraversals()为起点，来分析View的整个绘制流程。

## 三个阶段

View的整个绘制流程可以分为以下三个阶段：

- measure: 判断是否需要重新计算View的大小，需要的话则计算；
- layout: 判断是否需要重新计算View的位置，需要的话则计算；
- draw: 判断是否需要重新绘制View，需要的话则重绘制。

这三个子阶段可以用下图来描述：

[![img](https://camo.githubusercontent.com/9ff689b7d3749bfc4e3c4e36548aa341ae40215f/68747470733a2f2f68656e636861742e6e6f732d656173746368696e61312e3132362e6e65742f7a68616e676a69656c756e2e706e67)](https://camo.githubusercontent.com/9ff689b7d3749bfc4e3c4e36548aa341ae40215f/68747470733a2f2f68656e636861742e6e6f732d656173746368696e61312e3132362e6e65742f7a68616e676a69656c756e2e706e67)

### measure阶段

此阶段的目的是计算出控件树中的各个控件要显示其内容的话，需要多大尺寸。起点是ViewRootImpl的measureHierarchy()方法，这个方法的源码如下：

```java
private boolean measureHierarchy(final View host, final WindowManager.LayoutParams lp, final Resources res,
    final int desiredWindowWidth, final int desiredWindowHeight) {
  // 传入的desiredWindowXxx为窗口尺寸
  int childWidthMeasureSpec;
  int childHeightMeasureSpec;
  boolean windowSizeMayChange = false;
  . . .
  boolean goodMeasure = false;
 
  if (!goodMeasure) {
    childWidthMeasureSpec = getRootMeasureSpec(desiredWindowWidth, lp.width);
    childHeightMeasureSpec = getRootMeasureSpec(desiredWindowHeight, lp.height);
    performMeasure(childWidthMeasureSpec, childHeightMeasureSpec);
 
    if (mWidth != host.getMeasuredWidth() || mHeight != host.getMeasuredHeight()) {
      windowSizeMayChange = true;
    }
  }
  return windowSizeMayChange;
}
```

上面的代码中调用getRootMeasureSpec()方法来获取根MeasureSpec，这个根MeasureSpec代表了对decorView的宽高的约束信息。具体的内部方法您可以直接再AS进行查看，不再赘述。

### layout阶段

layout阶段的基本思想也是由根View开始，递归地完成整个控件树的布局（layout）工作。

#### View.layout()

我们把对decorView的layout()方法的调用作为布局整个控件树的起点，实际上调用的是View类的layout()方法，源码如下：

```java
public void layout(int l, int t, int r, int b) {
    // l为本View左边缘与父View左边缘的距离
    // t为本View上边缘与父View上边缘的距离
    // r为本View右边缘与父View左边缘的距离
    // b为本View下边缘与父View上边缘的距离
    . . .
    boolean changed = isLayoutModeOptical(mParent) ?            setOpticalFrame(l, t, r, b) : setFrame(l, t, r, b);
    if (changed || (mPrivateFlags & PFLAG_LAYOUT_REQUIRED) == PFLAG_LAYOUT_REQUIRED) {
        onLayout(changed, l, t, r, b);
        . . .
 
    }
    . . .
}
```

这个方法会调用setFrame()方法来设置View的mLeft、mTop、mRight和mBottom四个参数，这四个参数描述了View相对其父View的位置（分别赋值为l, t, r, b），在setFrame()方法中会判断View的位置是否发生了改变，若发生了改变，则需要对子View进行重新布局，对子View的局部是通过onLayout()方法实现了。由于普通View（ 非ViewGroup）不含子View，所以View类的onLayout()方法为空。因此接下来，您可以通过源码查看ViewGroup类的onLayout()方法的实现，不再赘述。

### draw阶段

对于本阶段的分析，我们以decorView.draw()作为分析的起点，也就是View.draw()方法，它的源码如下：

```java
public void draw(Canvas canvas) {
  . . .
  // 绘制背景，只有dirtyOpaque为false时才进行绘制，下同
  int saveCount;
  if (!dirtyOpaque) {
    drawBackground(canvas);
  }
 
  . . .
 
  // 绘制自身内容
  if (!dirtyOpaque) onDraw(canvas);
 
  // 绘制子View
  dispatchDraw(canvas);
 
   . . .
  // 绘制滚动条等
  onDrawForeground(canvas);
 
}
```

简单起见，在上面的代码中我们省略了实现滑动时渐变边框效果相关的逻辑。实际上，View类的onDraw()方法为空，因为每个View绘制自身的方式都不尽相同，对于decorView来说，由于它是容器View，所以它本身并没有什么要绘制的。dispatchDraw()方法用于绘制子View，显然普通View（非ViewGroup）并不能包含子View，所以View类中这个方法的实现为空。

ViewGroup类的dispatchDraw()方法中会依次调用drawChild()方法来绘制子View，drawChild()方法的源码如下：

```java
protected boolean drawChild(Canvas canvas, View child, long drawingTime) {
  return child.draw(canvas, this, drawingTime);
}
```

这个方法调用了View.draw(Canvas, ViewGroup，long)方法来对子View进行绘制。在draw(Canvas, ViewGroup, long)方法中，首先对canvas进行了一系列变换，以变换到将要被绘制的View的坐标系下。完成对canvas的变换后，便会调用View.draw(Canvas)方法进行实际的绘制工作，此时传入的canvas为经过变换的，在将被绘制View的坐标系下的canvas。

进入到View.draw(Canvas)方法后，会向之前介绍的一样，执行以下几步：

- 绘制背景;
- 通过onDraw()绘制自身内容;
- 通过dispatchDraw()绘制子View;
- 绘制滚动条

至此，整个View的绘制流程我们就分析完了。

## Android自定义View / ViewGroup的步骤大致如下：

> 1. 自定义属性；
> 2. 选择和设置构造方法；
> 3. 重写onMeasure()方法；
> 4. 重写onDraw()方法；
> 5. 重写onLayout()方法；
> 6. 重写其他事件的方法(滑动监听等)；

### 自定义属性

　　Android自定义属性主要有定义、使用和获取三个步骤。

#### 定义自定义属性

　　参考：<https://blog.csdn.net/zhcswlp0625/article/details/54958220>

　　我们通常将自定义属性定义在/values/attr.xml文件中（attr.xml文件需要自己创建）。

　　先来看一段示例代码：

```xml
<?xml version="1.0" encoding="utf-8"?>
<resources>
    <attr name="rightPadding" format="dimension" />

    <declare-styleable name="CustomMenu">
        <attr name="rightPadding" />
    </declare-styleable>
</resources>
```

可以看到，我们先是定义了一个属性rightPadding，然后又在CustomMenu中引用了这个属性。下面说明一下：

- 首先，我们可以在declare-stylable标签中直接定义属性而不需要引用外部定义好的属性，但是为了属性的重用，我们可以选择上面的这种方法：先定义，后引用；
- declare-stylable标签只是为了给自定义属性分类。一个项目中可能又多个自定义控件，但只能又一个attr.xml文件，因此我们需要对不同自定义控件中的自定义属性进行分类，这也是为什么declare-stylable标签中的name属性往往定义成自定义控件的名称；
- 所谓的在declare-stylable标签中的引用，就是去掉了外部定义的format属性，如果没有去掉format，则会报错；如果外部定义中没有format而在内部引用中又format，也一样会报错。

　　**常用的format类型：**

> 1. string：字符串类型；
> 2. integer：整数类型；
> 3. float：浮点型；
> 4. dimension：尺寸，后面必须跟dp、dip、px、sp等单位；
> 5. Boolean：布尔值；
> 6. reference：引用类型，传入的是某一资源的ID，必须以“@”符号开头；
> 7. color：颜色，必须是“#”符号开头；
> 8. fraction：百分比，必须是“%”符号结尾；
> 9. enum：枚举类型

下面对format类型说明几点：

- format中可以写多种类型，中间使用“|”符号分割开，表示这几种类型都可以传入这个属性；

- enum类型的定义示例如下代码所示：

  ```xml
  <resources>
      <attr name="orientation">
          <enum name="horizontal" value="0" />
          <enum name="vertical" value="1" />
      </attr>
  
      <declare-styleable name="CustomView">
          <attr name="orientation" />
      </declare-styleable>
  </resources>
  ```

  　使用时通过getInt()方法获取到value并判断，根据不同的value进行不同的操作即可。

### 使用自定义属性

　　在XML布局文件中使用自定义的属性时，我们需要先定义一个namespace。Android中默认的namespace是android，因此我们通常可以使用“android:xxx”的格式去设置一个控件的某个属性，android这个namespace的定义是在XML文件的头标签中定义的，通常是这样的：

```java
xmlns:android="http://schemas.android.com/apk/res/android"
```

　　我们自定义的属性不在这个命名空间下，因此我们需要添加一个命名空间。

　　自定义属性的命名空间如下：

```java
xmlns:app="http://schemas.android.com/apk/res-auto"
```

　　可以看出来，除了将命名空间的名称从android改成app之外，就是将最后的“res/android”改成了“res-auto”。

　　**注意：**自定义namespace的名称可以自己定义，不一定非得是app。

### 获取自定义属性

在自定义View / ViewGroup中，我们可以通过TypedArray获取到自定义的属性。示例代码如下：

```java
public CustomMenu(Context context, AttributeSet attrs, int defStyleAttr) {
    super(context, attrs, defStyleAttr);
    TypedArray a = context.getTheme().obtainStyledAttributes(attrs, R.styleable.CustomMenu, defStyleAttr, 0);
    int indexCount = a.getIndexCount();
    for (int i = 0; i < indexCount; i++) {
        int attr = a.getIndex(i);
        switch (attr) {
            case R.styleable.CustomMenu_rightPadding:
                mMenuRightPadding = a.getDimensionPixelSize(attr, 0);
                break;
        }
    }
    a.recycle();
}
```

- 获取自定义属性的代码通常是在三个参数的构造方法中编写的（具体为什么是三个参数的构造方法，下面的章节中会有解释）；
- 在获取TypedArray对象时就为其绑定了该自定义View的自定义属性集（CustomMenu），通过getIndexCount()方法获取到自定义属性的数量，通过getIndex()方法获取到某一个属性，最后通过switch语句判断属性并进行相应的操作；
- 在TypedArray使用结束后，需要调用recycle()方法回收它。

### 构造方法

　　当我们定义一个新的类继承了View或ViewGroup时，系统都会提示我们重写它的构造方法。View / ViewGroup中又四个构造方法可以重写，它们分别有一、二、三、四个参数。四个参数的构造方法我们通常用不到，因此这个章节中我们主要介绍一个参数、两个参数和三个参数的构造方法（这里以CustomMenu控件为例）。

#### 一个参数的构造方法

```java
public CustomMenu(Context context) { …… } 
```

　　这个构造方法只有一个参数Context上下文。当我们在JAVA代码中直接通过new关键在创建这个控件时，就会调用这个方法。

#### 两个参数的构造方法

```java
public CustomMenu(Context context, AttributeSet attrs) { …… } 
```

　　这个构造方法有两个参数：Context上下文和AttributeSet属性集。当我们需要在自定义控件中获取属性时，就默认调用这个构造方法。AttributeSet对象就是这个控件中定义的所有属性。

　　我们可以通过AttributeSet对象的getAttributeCount()方法获取属性的个数，通过getAttributeName()方法获取到某条属性的名称，通过getAttributeValue()方法获取到某条属性的值。

　　**注意：**不管有没有使用自定义属性，都会默认调用这个构造方法，“使用了自定义属性就会默认调用三个参数的构造方法”的说法是错误的。

#### 三个参数的构造方法

```java
　public CustomMenu(Context context, AttributeSet attrs, int defStyleAttr) { …… } 
```

　　这个构造方法中有三个参数：Context上下文、AttributeSet属性集和defStyleAttr自定义属性的引用。这个构造方法不会默认调用，必须要手动调用，这个构造方法和两个参数的构造方法的唯一区别就是这个构造方法给我们默认传入了一个默认属性集。

　　defStyleAttr指向的是自定义属性的标签中定义的自定义属性集，我们在创建TypedArray对象时需要用到defStyleAttr。

#### 三个构造方法的整合

　　一般情况下，我们会将这三个构造方法串联起来，即层层调用，让最终的业务处理都集中在三个参数的构造方法。我们让一参的构造方法引用两参的构造方法，两参的构造方法引用三参的构造方法。示例代码如下：

```java
public CustomMenu(Context context) {
    this(context, null);
}

public CustomMenu(Context context, AttributeSet attrs) {
    this(context, attrs, 0);
}

public CustomMenu(Context context, AttributeSet attrs, int defStyleAttr) {
    super(context, attrs, defStyleAttr);
    // 业务代码
}
```

　　这样一来，就可以保证无论使用什么方式创建这个控件，最终都会到三个参数的构造方法中处理，减少了重复代码。

### onMeasure()

　　onMeasure()方法中主要负责测量，决定控件本身或其子控件所占的宽高。我们可以通过onMeasure()方法提供的参数widthMeasureSpec和heightMeasureSpec来分别获取控件宽度和高度的**测量模式**和**测量值**（测量 = 测量模式 + 测量值）。

　　widthMeasureSpec和heightMeasureSpec虽然只是int类型的值，但它们是通过MeasureSpec类进行了编码处理的，其中封装了测量模式和测量值，因此我们可以分别通过MeasureSpec.getMode(xMeasureSpec)和MeasureSpec. getSize(xMeasureSpec)来获取到控件或其子View的测量模式和测量值。

　　**测量模式分为以下三种情况：**

```java
1)  EXACTLY：当宽高值设置为具体值时使用，如100DIP、match_parent等，此时取出的size是精确的尺寸；
2)  AT_MOST：当宽高值设置为wrap_content时使用，此时取出的size是控件最大可获得的空间；
3)  UNSPECIFIED：当没有指定宽高值时使用（很少见）。
```

　　**onMeasure()方法中常用的方法：**

```java
1)  getChildCount()：获取子View的数量；
2)  getChildAt(i)：获取第i个子控件；
3)  subView.getLayoutParams().width/height：设置或获取子控件的宽或高；
4)  measureChild(child, widthMeasureSpec, heightMeasureSpec)：测量子View的宽高；
5)  child.getMeasuredHeight/width()：执行完measureChild()方法后就可以通过这种方式获取子View的宽高值；
6)  getPaddingLeft/Right/Top/Bottom()：获取控件的四周内边距；
7)  setMeasuredDimension(width, height)：重新设置控件的宽高。如果写了这句代码，就需要删除“super. onMeasure(widthMeasureSpec, heightMeasureSpec);”这行代码。
```

**注意：**onMeasure()方法可能被调用多次，这是因为控件中的内容或子View可能对分配给自己的空间“不满意”，因此向父空间申请重新分配空间。

### onDraw()

　　onDraw()方法负责绘制，即如果我们希望得到的效果在Android原生控件中没有现成的支持，那么我们就需要自己绘制我们的自定义控件的显示效果。

　　要学习onDraw()方法，我们就需要学习在onDraw()方法中使用最多的两个类：Paint和Canvas。

　　**注意：**每次触摸了自定义View/ViewGroup时都会触发onDraw()方法。

### Paint类

　　Paint画笔对象，这个类中包含了如何绘制几何图形、文字和位图的样式和颜色信息，指定了如何绘制文本和图形。画笔对象右很多设置方法，大体上可以分为两类：一类与图形绘制有关，一类与文本绘制有关。

　　**Paint类中有如下方法：**

　　1、图形绘制：

```java
1)  setArgb(int a, int r, int g, int b)：设置绘制的颜色，a表示透明度，r、g、b表示颜色值；
2)  setAlpha(int a)：设置绘制的图形的透明度；
3)  setColor(int color)：设置绘制的颜色；
4)  setAntiAlias(boolean a)：设置是否使用抗锯齿功能，抗锯齿功能会消耗较大资源，绘制图形的速度会减慢；
5)  setDither(boolean b)：设置是否使用图像抖动处理，会使图像颜色更加平滑饱满，更加清晰；
6)  setFileterBitmap(Boolean b)：设置是否在动画中滤掉Bitmap的优化，可以加快显示速度；
7)  setMaskFilter(MaskFilter mf)：设置MaskFilter来实现滤镜的效果；
8)  setColorFilter(ColorFilter cf)：设置颜色过滤器，可以在绘制颜色时实现不同颜色的变换效果；
9)  setPathEffect(PathEffect pe)：设置绘制的路径的效果；
10) setShader(Shader s)：设置Shader绘制各种渐变效果；
11) setShadowLayer(float r, int x, int y, int c)：在图形下面设置阴影层，r为阴影角度，x和y为阴影在x轴和y轴上的距离，c为阴影的颜色；
12) setStyle(Paint.Style s)：设置画笔的样式：FILL实心；STROKE空心；FILL_OR_STROKE同时实心与空心；
13) setStrokeCap(Paint.Cap c)：当设置画笔样式为STROKE或FILL_OR_STROKE时，设置笔刷的图形样式；
14) setStrokeJoin(Paint.Join j)：设置绘制时各图形的结合方式；
15) setStrokeWidth(float w)：当画笔样式为STROKE或FILL_OR_STROKE时，设置笔刷的粗细度；
16) setXfermode(Xfermode m)：设置图形重叠时的处理方式；
```

　　2、文本绘制：

```java
1)  setTextAlign(Path.Align a)：设置绘制的文本的对齐方式；
2)  setTextScaleX(float s)：设置文本在X轴的缩放比例，可以实现文字的拉伸效果；
3)  setTextSize(float s)：设置字号；
4)  setTextSkewX(float s)：设置斜体文字，s是文字倾斜度；
5)  setTypeFace(TypeFace tf)：设置字体风格，包括粗体、斜体等；
6)  setUnderlineText(boolean b)：设置绘制的文本是否带有下划线效果；
7)  setStrikeThruText(boolean b)：设置绘制的文本是否带有删除线效果；
8)  setFakeBoldText(boolean b)：模拟实现粗体文字，如果设置在小字体上效果会非常差；
9)  setSubpixelText(boolean b)：如果设置为true则有助于文本在LCD屏幕上显示效果；
```

　　3、其他方法：

```java
1)  getTextBounds(String t, int s, int e, Rect b)：将页面中t文本从s下标开始到e下标结束的所有字符所占的区域宽高封装到b这个矩形中；
2)  clearShadowLayer()：清除阴影层；
3)  measureText(String t, int s, int e)：返回t文本中从s下标开始到e下标结束的所有字符所占的宽度；
4)  reset()：重置画笔为默认值。
```

　　**这里需要就几个方法解释一下：**

1、setPathEffect(PathEffect pe)：设置绘制的路径的效果：

　　常见的有以下几种可选方案：

```java
1)  CornerPathEffect：可以用圆角来代替尖锐的角；
2)  DathPathEffect：虚线，由短线和点组成；
3)  DiscretePathEffect：荆棘状的线条；
4)  PathDashPathEffect：定义一种新的形状并将其作为原始路径的轮廓标记；
5)  SumPathEffect：在一条路径中顺序添加参数中的效果；
6)  ComposePathEffect：将两种效果组合起来，先使用第一种效果，在此基础上应用第二种效果。
```

2、setXfermode(Xfermode m)：设置图形重叠时的处理方式：

关于Xfermode的多种效果，我们可以参考下面一张图：

[![img](https://camo.githubusercontent.com/c2f101cac6efa15fba4378497b3fc9fdd221c2f2/68747470733a2f2f68656e636861742e6e6f732d656173746368696e61312e3132362e6e65742f6c697869616f6c6f6e672e706e67)](https://camo.githubusercontent.com/c2f101cac6efa15fba4378497b3fc9fdd221c2f2/68747470733a2f2f68656e636861742e6e6f732d656173746368696e61312e3132362e6e65742f6c697869616f6c6f6e672e706e67)

　　在使用的时候，我们需要通过paint. setXfermode(new PorterDuffXfermode(PorterDuff.Mode.XXX))来设置，XXX是上图中的某种模式对应的常量参数，如DST_OUT。

　　这16中情况的具体解释如下：

```java
1.PorterDuff.Mode.CLEAR：所绘制不会提交到画布上。
2.PorterDuff.Mode.SRC：显示上层绘制图片
3.PorterDuff.Mode.DST：显示下层绘制图片
4.PorterDuff.Mode.SRC_OVER：正常绘制显示，上下层绘制叠盖。
5.PorterDuff.Mode.DST_OVER：上下层都显示。下层居上显示。
6.PorterDuff.Mode.SRC_IN：取两层绘制交集。显示上层。
7.PorterDuff.Mode.DST_IN：取两层绘制交集。显示下层。
8.PorterDuff.Mode.SRC_OUT：上层绘制非交集部分。
9.PorterDuff.Mode.DST_OUT：取下层绘制非交集部分。
10.PorterDuff.Mode.SRC_ATOP：取下层非交集部分与上层交集部分
11.PorterDuff.Mode.DST_ATOP：取上层非交集部分与下层交集部分
12.PorterDuff.Mode.XOR：异或：去除两图层交集部分
13.PorterDuff.Mode.DARKEN：取两图层全部区域，交集部分颜色加深
14.PorterDuff.Mode.LIGHTEN：取两图层全部，点亮交集部分颜色
15.PorterDuff.Mode.MULTIPLY：取两图层交集部分叠加后颜色
16.PorterDuff.Mode.SCREEN：取两图层全部区域，交集部分变为透明色
```

#### Canvas类

　　Canvas即画布，其上可以使用Paint画笔对象绘制很多东西。

　　**Canvas\**\**对象中可以绘制：**

```java
1)  drawArc()：绘制圆弧；
2)  drawBitmap()：绘制Bitmap图像；
3)  drawCircle()：绘制圆圈；
4)  drawLine()：绘制线条；
5)  drawOval()：绘制椭圆；
6)  drawPath()：绘制Path路径；
7)  drawPicture()：绘制Picture图片；
8)  drawRect()：绘制矩形；
9)  drawRoundRect()：绘制圆角矩形；
10) drawText()：绘制文本；
11) drawVertices()：绘制顶点。
```

　　**Canvas\**\**对象的其他方法：**

```java
1)  canvas.save()：把当前绘制的图像保存起来，让后续的操作相当于是在一个新图层上绘制；
2)  canvas.restore()：把当前画布调整到上一个save()之前的状态；
3)  canvas.translate(dx, dy)：把当前画布的原点移到(dx, dy)点，后续操作都以(dx, dy)点作为参照；
4)  canvas.scale(x, y)：将当前画布在水平方向上缩放x倍，竖直方向上缩放y倍；
5)  canvas.rotate(angle)：将当前画布顺时针旋转angle度。
```

### onLayout()

　　onLayout()方法负责布局，大多数情况是在自定义ViewGroup中才会重写，主要用来确定子View在这个布局空间中的摆放位置。

　　onLayout(boolean changed, int l, int t, int r, int b)方法有5个参数，其中changed表示这个控件是否有了新的尺寸或位置；l、t、r、b分别表示这个View相对于父布局的左/上/右/下方的位置。

　　**以下是onLayout()方法中常用的方法：**

```java
1)  getChildCount()：获取子View的数量；
2)  getChildAt(i)：获取第i个子View
3)  getWidth/Height()：获取onMeasure()中返回的宽度和高度的测量值；
4)  child.getLayoutParams()：获取到子View的LayoutParams对象；
5)  child.getMeasuredWidth/Height()：获取onMeasure()方法中测量的子View的宽度和高度值；
6)  getPaddingLeft/Right/Top/Bottom()：获取控件的四周内边距；
7)  child.layout(l, t, r, b)：设置子View布局的上下左右边的坐标。
```

### 其他方法

#### generateLayoutParams()

　　generateLayoutParams()方法用在自定义ViewGroup中，用来指明子控件之间的关系，即与当前的ViewGroup对应的LayoutParams。我们只需要在方法中返回一个我们想要使用的LayoutParams类型的对象即可。

　　在generateLayoutParams()方法中需要传入一个AttributeSet对象作为参数，这个对象是这个ViewGroup的属性集，系统根据这个ViewGroup的属性集来定义子View的布局规则，供子View使用。

　　例如，在自定义流式布局中，我们只需要关心子控件之间的间隔关系，因此我们需要在generateLayoutParams()方法中返回一个new MarginLayoutParams()即可。

#### onTouchEvent()

　　onTouchEvent()方法用来监测用户手指操作。我们通过方法中MotionEvent参数对象的getAction()方法来实时获取用户的手势，有UP、DOWN和MOVE三个枚举值，分别表示用于手指抬起、按下和滑动的动作。每当用户有操作时，就会回掉onTouchEvent()方法。

#### onScrollChanged()

　　如果我们的自定义View / ViewGroup是继承自ScrollView / HorizontalScrollView等可以滚动的控件，就可以通过重写onScrollChanged()方法来监听控件的滚动事件。

　　这个方法中有四个参数：l和t分别表示当前滑动到的点在水平和竖直方向上的坐标；oldl和oldt分别表示上次滑动到的点在水平和竖直方向上的坐标。我们可以通过这四个值对滑动进行处理，如添加属性动画等。

#### invalidate()

　　invalidate()方法的作用是请求View树进行重绘，即draw()方法，如果视图的大小发生了变化，还会调用layout()方法。

　　**一般会引起invalidate()****操作的函数如下：**

```java
1)  直接调用invalidate()方法，请求重新draw()，但只会绘制调用者本身；
2)  调用setSelection()方法，请求重新draw()，但只会绘制调用者本身；
3)  调用setVisibility()方法，会间接调用invalidate()方法，继而绘制该View；
4)  调用setEnabled()方法，请求重新draw()，但不会重新绘制任何视图，包括调用者本身。
```

#### postInvalidate()

　　功能与invalidate()方法相同，只是postInvalidate()方法是异步请求重绘视图。

#### requestLayout()

　　requestLayout()方法只是对View树进行重新布局layout过程（包括measure()过程和layout()过程），不会调用draw()过程，即不会重新绘制任何视图，包括该调用者本身。

#### requestFocus()

　　请求View树的draw()过程，但只会绘制需要重绘的视图，即哪个View或ViewGroup调用了这个方法，就重绘哪个视图。

### 总结

　　最后，让我们来总览一下自定义View / ViewGroup时调用的各种函数的顺序，如下图所示：

[![img](https://camo.githubusercontent.com/eda262eab75c80f888541b78d3643856e51a197c/68747470733a2f2f68656e636861742e6e6f732d656173746368696e61312e3132362e6e65742f6c697564656875612e706e67)](https://camo.githubusercontent.com/eda262eab75c80f888541b78d3643856e51a197c/68747470733a2f2f68656e636861742e6e6f732d656173746368696e61312e3132362e6e65742f6c697564656875612e706e67)

　　**在这些方法中：**

- onMeasure()会在初始化之后调用一到多次来测量控件或其中的子控件的宽高；
- onLayout()会在onMeasure()方法之后被调用一次，将控件或其子控件进行布局；
- onDraw()会在onLayout()方法之后调用一次，也会在用户手指触摸屏幕时被调用多次，来绘制控件。