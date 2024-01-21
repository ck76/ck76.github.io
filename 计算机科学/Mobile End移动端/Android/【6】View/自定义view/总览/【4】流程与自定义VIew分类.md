[TOC]

### 一、流程

自定义View绘制流程函数调用链(简化版)

![函数调用流程](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/19.nKo4rT5bG87dCilsFnC4ENNugRvglOu5bEYffw60!/r/dLYAAAAAAAAA)



### 二、分类

#### 1、按照所继承的基类划分

- 继承View：这种自定义控件最大的特点是不包含子控件，当我们需要定制一个不同于原生控件且有“个性”的控件（如圆形头像、水波进度条）时，我们采用这种方式，很多情况下可以采用其他方式代替，比如圆形头像完全可以使用imageView+圆形背景图，但后者往往内存消耗过大，制作素材麻烦。
- 继承ViewGroup：这种自定义控件，常常利用现有组件（各种layout）的特定布局方式来组成新的控件。如流式标签。

|   类别    |          继承自          |    特点    |
| :-------: | :----------------------: | :--------: |
|   View    | View、TextVIew、Button等 | 不含子VIew |
| VIewGroup |  ViewGroup、XXXLayout等  | 包含子VIew |

#### 2、按照实现的方式划分

 第二种分类方式较之前者，更注重于自定义View的实现方式。

- 自绘控件：顾名思义，这一类自定义控件注重控件本身的特性，如形状、动画效果等，一般继承View（但也不绝对，看具体需求），重写onDraw（）方法完成绘制，具体绘制的过程请参照 (三)自定义View常用的方法（测量、绘制、位置）
- 组合控件：组合控件的特点是使用原生控件组合成新的控件。
- 继承控件：继承控件的特点是利用了父控件本身的一些特性，在此基础上添加新的功能。充分利用已有资源，避免了重复的开发。

掌握自定义控件的分类，让我们面对具体的需求能快速而准确的决定使用哪一种定义方式，是学习自定义控件最基础的一步

|   类别   |             继承自             |              特点              |
| :------: | :----------------------------: | :----------------------------: |
| 自绘View |              View              | 自己绘制控件，重写onDraw()方法 |
| 组合View |           ViewGroup            |     组合原生控件成为新控件     |
| 继承View | 原生控件，如ListView、Button等 | 保留原有控件的功能，增加新功能 |

#### 3、《开发艺术探索》

##### 自定义View的分类

###### 1. 继承重写onDraw方法

这种方法一般用来实现一些不规则的效果，即效果不方便通过一般的布局来实现，我们只需要重写onDraw方法即可，采用这种方式要自己支持warp_content,并且需要自己处理padding。

###### 2. 集成ViewGroup派生特殊的Layout

这种方式一般用来实现自定义布，即除了LinerLayout、RelativrLayout和FrameLayout之外的布局，我们需要重新定义一种新的布局，当某种效果看起来像多个View组合在一起的时候我们可以采用这种方法，采用这种方式需要合适地处理ViewGroup的测量和布局，这两个过程，并同时处理子元素的测量和布局。

###### 3. 继承特定的View（比如Button）

这种方法比较常见，用来扩展某种已有View的功能，，这种方法实现比较容易，不需要自己支持warp_content和padding等。

###### 4. 继承特定的ViewGroup （比如LinerLayout）

这种方法也比较常见，也可以用来实现看起来像集中View组合在一起的效果，并且不需要自己处理ViewGroup的测量和布局，它跟方法二比较类似，一般来说方法二可以实现的它也可以实现，只是方法二更接近于View的底层。

##### 自定义View须知

上面介绍的集中方法，我们可以仔细体味一下，自定义View讲究的是灵活性，一般某种样式多个方法都可以实现，我们需要比较各自的优缺点，找到代价小，效率高的方法来实现它。

###### 1. 让View支持warp_content

因为直接继承View或者ViewGroup的控件如果不在onMeasure中队warp_content做特殊处理的话，当外界布局使用的时候就无法达到预期的效果。

###### 2. 如果有必要，让你的View支持padding

因为直接继承View的控件，如果不在draw方法里面处理padding的话，那么padding的属性是不起作用的，另外直接继承ViewGroup的空间需要在onMeasure和onLayout中考虑padding和子元素的marging对其造成的影响，否则会导致padding和子元素的marging失效。

###### 3. 尽量不要在View中使用Handle

View内部本身提供了post系列的方法，几乎可以完全替代Handle。

###### 4. 如果View中有线程或者动画需要及时停止，参考View#onDetachedFromWindow

因为当包含此View的Activity退出或者View被remove掉的时候View的onDetachedFromWindow方法会被调用，和此方法对应的是onAttachedToWindow，当View的Activity启动或View被显示在屏幕内时会被调用，我们要在对应的方法中及时的做一些处理，防止内存泄漏。

###### 5. View带有滑动嵌套，需要谨慎处理冲突

保证效果流程，要处理好滑动冲突。



### 三、几个重要函数

#### 1、构造函数

```java
public void SloopView(Context context) {}
  public void SloopView(Context context, AttributeSet attrs) {}
  public void SloopView(Context context, AttributeSet attrs, int defStyleAttr) {}
  public void SloopView(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {}
```

**有四个参数的构造函数在API21的时候才添加上，暂不考虑。**

有三个参数的构造函数中第三个参数是默认的Style，这里的**默认的Style是指它在当前Application或Activity所用的Theme中的默认Style**，且只有在**明确调用的时候才会生效**，以系统中的ImageButton为例说明：

```java
    public ImageButton(Context context, AttributeSet attrs) {
        //调用了三个参数的构造函数，明确指定第三个参数
        this(context, attrs, com.android.internal.R.attr.imageButtonStyle);
    }

    public ImageButton(Context context, AttributeSet attrs, int defStyleAttr) {
        //此处调了四个参数的构造函数，无视即可
        this(context, attrs, defStyleAttr, 0); 
    }
```

**注意：即使你在View中使用了Style这个属性也不会调用三个参数的构造函数，所调用的依旧是两个参数的构造函数。**

**由于三个参数的构造函数第三个参数一般不用，暂不考虑**

排除了两个之后，只剩下一个参数和两个参数的构造函数，他们的详情如下：

```java
  //一般在直接New一个View的时候调用。
  public void SloopView(Context context) {}
  
  //一般在layout文件中使用的时候会调用，关于它的所有属性(包括自定义属性)都会包含在attrs中传递进来。
  public void SloopView(Context context, AttributeSet attrs) {}
```

- **以下方法调用的是一个参数的构造函数：**

```java
  //在Avtivity中
  SloopView view = new SloopView(this);
```

- **以下方法调用的是两个参数的构造函数：**

```xml
  //在layout文件中 - 格式为： 包名.View名
  <com.sloop.study.SloopView
    android:layout_width="wrap_content"
    android:layout_height="wrap_content"/>
```



#### 2、测量View大小(onMeasure)

测量View大小使用的是onMeasure函数，我们可以从onMeasure的两个参数中取出宽高的相关数据：

```java
   @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        int widthsize = MeasureSpec.getSize(widthMeasureSpec);      //取出宽度的确切数值
        int widthmode = MeasureSpec.getMode(widthMeasureSpec);      //取出宽度的测量模式
        
        int heightsize = MeasureSpec.getSize(heightMeasureSpec);    //取出高度的确切数值
        int heightmode = MeasureSpec.getMode(heightMeasureSpec);    //取出高度的测量模式
    }
```

从上面可以看出 onMeasure 函数中有 widthMeasureSpec 和 heightMeasureSpec 这两个 int 类型的参数， 毫无疑问他们是和宽高相关的， **但它们其实不是宽和高， 而是由宽、高和各自方向上对应的测量模式来合成的一个值：**

**测量模式一共有三种， 被定义在 Android 中的 View 类的一个内部类View.MeasureSpec中：**

| 模式        | 二进制数值 | 描述                                                         |
| ----------- | ---------- | ------------------------------------------------------------ |
| UNSPECIFIED | 00         | 默认值，父控件没有给子view任何限制，子View可以设置为任意大小。 |
| EXACTLY     | 01         | 表示父控件已经确切的指定了子View的大小。                     |
| AT_MOST     | 10         | 表示子View具体大小没有尺寸限制，但是存在上限，上限一般为父View大小。 |

**在int类型的32位二进制位中，31-30这两位表示测量模式,29~0这三十位表示宽和高的实际值，实际上如下：**

以数值1080(二进制为: 1111011000)为例(其中模式和实际数值是连在一起的，为了展示我将他们分开了)：

| 模式名称    | 模式数值 | 实际数值                       |
| ----------- | -------- | ------------------------------ |
| UNSPECIFIED | 00       | 000000000000000000001111011000 |
| EXACTLY     | 01       | 000000000000000000001111011000 |
| AT_MOST     | 10       | 000000000000000000001111011000 |

**PS: 实际上关于上面的东西了解即可，在实际运用之中只需要记住有三种模式，用 MeasureSpec 的 getSize是获取数值， getMode是获取模式即可。**

#### 注意：

**如果对View的宽高进行修改了，不要调用super.onMeasure(widthMeasureSpec,heightMeasureSpec);要调用setMeasuredDimension(widthsize,heightsize); 这个函数。**



#### 3、确定View大小(onSizeChanged)

这个函数在视图**大小发生改变**时调用。

**Q: 在测量完View并使用setMeasuredDimension函数之后View的大小基本上已经确定了，那么为什么还要再次确定View的大小呢？**

**A: 这是因为View的大小不仅由View本身控制，而且受父控件的影响，所以我们在确定View大小的时候最好使用系统提供的onSizeChanged回调函数。**

```java
@Override
    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged(w, h, oldw, oldh);
    }
```

可以看出，它有四个参数，分别为 宽度，高度，上一次宽度，上一次高度。

这个函数比较简单，我们**只需关注** 宽度(w), 高度(h) 即可，这两个参数就是**View最终的大小。**



#### 4、确定子View布局位置(onLayout)

**确定布局的函数是onLayout，它用于确定子View的位置，在自定义ViewGroup中会用到，他调用的是子View的layout函数。**

在自定义ViewGroup中，onLayout一般是循环取出子View，然后经过计算得出各个子View位置的坐标值，然后用以下函数设置子View位置。

```
  child.layout(l, t, r, b);
```

四个参数分别为：

| 名称 | 说明（距父View）           | 对应的函数   |
| ---- | -------------------------- | ------------ |
| l    | View左侧距父View左侧的距离 | getLeft();   |
| t    | View顶部距父View顶部的距离 | getTop();    |
| r    | View右侧距父View左侧的距离 | getRight();  |
| b    | View底部距父View顶部的距离 | getBottom(); |



#### 5、绘制内容(onDraw)

onDraw是实际绘制的部分，也就是我们真正关心的部分，使用的是Canvas绘图。

```java
    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);
    }
```

关于Canvas绘图是重点



#### 6、对外提供操作方法和监听回调

自定义完View之后，一般会对外暴露一些接口，用于控制View的状态等，或者监听View的变化.



### 四、重点知识梳理

#### 自定义View分类

> PS ：实际上ViewGroup是View的一个子类。

| 类别      | 继承自               | 特点       |
| --------- | -------------------- | ---------- |
| View      | View SurfaceView 等  | 不含子View |
| ViewGroup | ViewGroup xxLayout等 | 包含子View |

#### 自定义View流程：

| 步骤 | 关键字        | 作用                                           |
| ---- | ------------- | ---------------------------------------------- |
| 1    | 构造函数      | View初始化                                     |
| 2    | onMeasure     | 测量View大小                                   |
| 3    | onSizeChanged | 确定View大小                                   |
| 4    | onLayout      | 确定**子View**布局(自定义View包含子View时有用) |
| 5    | onDraw        | 实际绘制内容                                   |
| 6    | 提供接口      | 控制View或监听View某些状态。                   |

