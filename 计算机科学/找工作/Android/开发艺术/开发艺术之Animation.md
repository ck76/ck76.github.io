#### 在之前[开发艺术之View](https://www.jianshu.com/p/06ff0dfeed39)讲滑动的时候曾简单提起过动画，本篇将依次分析和总结以下三类动画：

- View动画（View Animation）/补间动画（Tween animation）
  - 自定义View动画
  - 布局动画
  - Activity切换动画
- 逐帧动画（Drawable Animation）
- 属性动画（Property Animation）

------

一**View动画**

1.**Q：View动画主要的四种变换效果**

**A**：平移动画、缩放动画、旋转动画和透明度动画 。对应关系如图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp1qp3f3j30jk05kaae.jpg)

> **注意**：View动画的View移动只是视觉效果，并不能真正的改变view的位置。

------

2.**View动画的创建**

> 对于View动画建议采用**XML**来定义

a.通过xml定义：

- 该xml文件创建在**res/anim/** 下。
- 根节点`<set>`，子节点`<translate>`、`<scale>`、`<rotate>`、`<alpha >`，分别对应四种View动画：



```xml
<?xml version="1.0" encoding="utf-8"?>
<set xmlns:android="http://schemas.android.com/apk/res/android"
    android:shareInterpolator="true"
    android:fillAfter="true">
    
    <translate
        android:fromXDelta="float"
        android:toXDelta="float"
        android:fromYDelta="float"
        android:toYDelta="float"/>
     <scale
        android:fromXScale="float"
        android:toXScale="float"
        android:fromYScale="float"
        android:toYScale="float"
        android:pivotX="float"
        android:pivotY="float"/>    
     <rotate
        android:fromDegrees="float"
        android:toDegrees="float"
        android:pivotY="float"
        android:pivotX="float"/>
    <alpha 
        android:fromAlpha="float"
        android:toAlpha="float"/>

</set>
```

接下来分别解释各个节点下属性含义：

①`<set >`：表示动画集合，对应AnimationSet类。

- `android:shareInterpolator`：表示集合中的动画是否和**集合**共享一个插值器。
  - 如果集合不指定插值器, 那么子动画就需要单独制定所需的插值器或者使用默认值。
- `android:fillAfter`：表示动画结束时是否保持动画结束时的状态 。

②`<translate>`：表示平移动画，对应TranslateAnimation类。

- `android:fromXDelta`：动画起始时X坐标上的位置。
- `android:toXDelta`：动画结束时X坐标上的位置。
- `android:fromYDelta`：动画起始时Y坐标上的位置。
- `android:toYDelta`：动画结束时Y坐标上的位置。

> **注意**：以上四个属性以及后面几个类似属性的取值可能是数值、百分数、百分数p，各自含义是：
>
> - **50**：以View左上角为原点沿坐标轴正方向偏移50px。
> - **50%**：以View左上角为原点沿坐标轴正方向偏移View宽/高度的50%。
> - **50%p**：以View左上角为原点沿坐标轴正方向偏移父（parent）控件宽/高度的50%。区别如图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp1oocq4j30uk0beaas.jpg)

③`<scale>`：表示缩放动画，对应ScaleAnimation类。

- `android:fromXScale`动画起始时X坐标上的伸缩尺寸。
- `android:toXScale`：动画结束时X坐标上的伸缩尺寸 。
- `android:fromYScale`：动画起始时Y坐标上的伸缩尺寸。
- `android:toYScale`：属性为动画结束时Y坐标上的伸缩尺寸。

> 以上四个属性值的值含义：
>
> - 值=0.0 ：表示收缩到没有
> - 值＜1.0 ：表示收缩
> - 值=1.0 ：表示无伸缩
> - 值＞1.0 ：表示放大

- `android:pivotX`：动画相对于物件的X坐标的开始位置。
- `android:pivotY`：动画相对于物件的Y坐标的开始位置。

> 以上两个属性值表示缩放的轴点：从0%-100%中取值。

④`<rotate>`：表示旋转动画，对应RotateAnimation类。

- `android:fromDegrees`：动画起始时物件的角度 。
- `android:toDegrees`：动画结束时物件旋转的角度。

> - 以上两个属性共同确定旋转方向，原则是：当角度为**负**数时表示**逆时针**旋转，反之。
> - 故共存在以下四种情况：
>   - from=负数->to=正数：表示顺时针旋转
>   - from=负数->to=负数：表示逆时针旋转
>   - from=正数->to=正数：表示顺时针旋转
>   - from=正数->to=负数：表示逆时针旋转

- `android:pivotY`：动画相对于物件的X坐标的开始位置。
- `android:pivotX`：动画相对于物件的Y坐标的开始位置。

> 以上两个属性值表示旋转的轴点：从0%-100%中取值。

⑤`<alpha>`：表示透明度动画，对应AlphaAnimation类。

- `android:fromAlpha`：动画起始时透明度。
- `android:toAlpha`动画结束时透明度。

> 以上两个属性值：从0-1中取值。特别的，
>
> - 值=0.0 ：表示完全透明
> - 值=1.0 ：表示完全不透明

以上四类View动画除了各自的特有属性外，它们的共有属性有：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp1mu7r5j30hc0do3zf.jpg)

在xml声明好之后，接下来只要在代码中**startAnimation(animation)**开始动画即可，代码如下：



```kotlin
Animation animation = AnimationUtils.loadAnimation(this, R.anim.XXX);
mView.startAnimation(animation);
```

同时，可通过Animation的**setAnimationListener(new AnimationListener(){...})**给动画添加过程监听，这样在动画开始、结束和每一次循环时都可在回调方法中监听到。接口代码如下：



```csharp
public static interface AnimationListener {
        //动画开始        
        void onAnimationStart(Animator animation);
        //动画结束
        void onAnimationEnd(Animator animation);
        //动画重复
        void onAnimationRepeat(Animator animation);
    }
```

b.通过Java代码动态创建

- 具体步骤：

step1：创建TranslateAnimation、RotateAnimation、ScaleAnimation或AlphaAnimation对象。

step2：设置创建的动画对象的属性，如动画执行时间、延迟时间、起始位置、结束位置等。

step3：通过**View.startAnimation()**方法开启动画。

step4：可通过**Animation.setAnimationListener()**设置动画的监听器，同上。

c.**综合实例**

①平移：



```xml
//法一：xml定义
<?xml version="1.0" encoding="utf-8"?>
<translate xmlns:android="http://schemas.android.com/apk/res/android"
           android:duration="2000"
           android:fromXDelta="0"
           android:fromYDelta="0"
           android:toXDelta="100%"
           android:toYDelta="100%">
</translate>

//在MainActivity中调用
  Animation translateAnim = AnimationUtils.loadAnimation(this, R.anim.view_anim_translate);
    mImageView.startAnimation(translateAnim);
```



```cpp
//法二：java代码创建
TranslateAnimation translateAnimation = new TranslateAnimation(
            Animation.RELATIVE_TO_SELF, 0,
            Animation.RELATIVE_TO_SELF, 1,
            Animation.RELATIVE_TO_SELF, 0,
            Animation.RELATIVE_TO_SELF, 1);
    translateAnimation.setDuration(2000);
    mImageView.startAnimation(translateAnimation);
}
```

效果：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp1khb8jj306c0793y9.jpg)

image

②缩放：



```xml
//法一：xml定义
<?xml version="1.0" encoding="utf-8"?>
<scale xmlns:android="http://schemas.android.com/apk/res/android"
       android:duration="2000"
       android:fromXScale="1.0"
       android:fromYScale="1.0"
       android:pivotX="50%"
       android:pivotY="50%"
       android:toXScale="0.5"
       android:toYScale="0.5">
</scale>

//在MainActivity中调用
   Animation scaleAnim = AnimationUtils.loadAnimation(this, R.anim.view_anim_scale);
    mImage.startAnimation(scaleAnim);
```



```cpp
//法二：java代码创建
ScaleAnimation scaleAnimation = new ScaleAnimation(
            1, 0.5f,
            1, 0.5f,
            Animation.RELATIVE_TO_SELF, 0.5f,
            Animation.RELATIVE_TO_SELF, 0.5f);
    scaleAnimation.setDuration(2000);
    mImageView.startAnimation(scaleAnimation);
```

效果:

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp1j2wkwj306207fa9t.jpg)

③旋转:



```xml
//法一：xml定义
<?xml version="1.0" encoding="utf-8"?>
<rotate xmlns:android="http://schemas.android.com/apk/res/android"
        android:duration="2000"
        android:fillAfter="true"
        android:fromDegrees="0"
        android:toDegrees="360"
        android:pivotX="50%"
        android:pivotY="50%">
</rotate>

//在MainActivity中调用
Animation rotateAnim = AnimationUtils.loadAnimation(this, R.anim.view_anim_rotate);
    mImageView.startAnimation(rotateAnim);
```



```cpp
//法二：java代码创建
RotateAnimation rotateAnimation = new RotateAnimation(
            0, 360,
            Animation.RELATIVE_TO_SELF, 0.5f,
            Animation.RELATIVE_TO_SELF, 0.5f);
    rotateAnimation.setDuration(2000);
    mImageView.startAnimation(rotateAnimation);
```

效果：图片在2s内以图片中心为轴点，顺时针旋转360°，即完整转一圈。

④透明度：



```xml
//法一：xml定义
<?xml version="1.0" encoding="utf-8"?>
<alpha xmlns:android="http://schemas.android.com/apk/res/android"
       android:duration="2000"
       android:fromAlpha="1.0"
       android:toAlpha="0">
</alpha>

//在MainActivity中调用
    Animation alphaAnim = AnimationUtils.loadAnimation(this, R.anim.view_anim_alpha);
    mImageView.startAnimation(alphaAnim);
```



```cpp
//法二：java代码创建
AlphaAnimation alphaAnimation = new AlphaAnimation(1, 0);
    alphaAnimation.setDuration(2000);
    mImageView.startAnimation(alphaAnimation);
```

效果：图片在2s内从有到无。

⑤动画集合：



```xml
//法一：xml定义
<?xml version="1.0" encoding="utf-8"?>
<set xmlns:android="http://schemas.android.com/apk/res/android"
    android:shareInterpolator="true" >
    
    <translate
        android:duration="2000"
        android:fromXDelta="0"
        android:fromYDelta="0"
        android:toXDelta="100%"
        android:toYDelta="100%"> />
    <scale
       android:duration="2000"
       android:fromXScale="1.0"
       android:fromYScale="1.0"
       android:pivotX="50%"
       android:pivotY="50%"
       android:toXScale="0.5"
       android:toYScale="0.5" /> 
    <rotate
        android:duration="2000"
        android:fromDegrees="0"
        android:toDegrees="360"
        android:pivotX="50%"
        android:pivotY="50%"/>
     <alpha
       android:duration="2000"
       android:fromAlpha="1.0"
       android:toAlpha="0"/>   
</set>

//在MainActivity中调用
    Animation setAnim = AnimationUtils.loadAnimation(this, R.anim.view_anim_set);
    mImageView.startAnimation(setAnim);
```



```cpp
//法二：java代码创建
AlphaAnimation alphaAnimation = new AlphaAnimation(1, 0);
    alphaAnimation.setDuration(2000);

    RotateAnimation rotateAnimation = new RotateAnimation(
            0, 360,
            Animation.RELATIVE_TO_SELF, 0.5f,
            Animation.RELATIVE_TO_SELF, 0.5f);
    rotateAnimation.setDuration(2000);

    ScaleAnimation scaleAnimation = new ScaleAnimation(
            1, 0.5f,
            1, 0.5f,
            Animation.RELATIVE_TO_SELF, 0.5f,
            Animation.RELATIVE_TO_SELF, 0.5f);
    scaleAnimation.setDuration(2000);

    TranslateAnimation translateAnimation = new TranslateAnimation(
            Animation.RELATIVE_TO_SELF, 0,
            Animation.RELATIVE_TO_SELF, 1,
            Animation.RELATIVE_TO_SELF, 0,
            Animation.RELATIVE_TO_SELF, 1);
    translateAnimation.setDuration(2000);

    AnimationSet animationSet = new AnimationSet(true);
    animationSet.addAnimation(alphaAnimation);
    animationSet.addAnimation(rotateAnimation);
    animationSet.addAnimation(scaleAnimation);
    animationSet.addAnimation(translateAnimation);

    mImageView.startAnimation(animationSet);
```

效果：以上四种动画效果的叠加。图片在2s内边向右下角移动、边缩小、边旋转、边降低透明度至消失。

**补充实例**：[View动画高级实例探究](https://link.jianshu.com/?t=http://www.cnblogs.com/wondertwo/p/5295976.html)

------

3.**自定义View动画**

> 实际项目中以上几种动画并不能满足我们的需求，这时就需要自定义View动画。

- 步骤：继承Animation->重写**initialize()**和**applyTransformation()**方法：initialize()用于初始化；applyTransformation()用于进行矩阵变换
- 实例：[自定义补间动画](https://link.jianshu.com/?t=http://blog.csdn.net/Airsaid/article/details/51591282)、[3D翻转动画](https://link.jianshu.com/?t=http://blog.csdn.net/cyt528300/article/details/52015577?locationNum=9)

------

**4.view动画的特殊使用场景**

> View动画除了可作用在某个View对象上， 还可以用在特殊的场景，例如：控制ViewGroup的子View 的出场效果，还有Activity的切换效果。接下来依次介绍。

**a.布局动画**

- 常用场景：ListView、GridView。
- 对应类：LayoutAnimation。
- 该xml文件创建在**res/anim/** 下。
- 根节点`<layoutAnimation >`，常用属性：



```bash
layoutAnimation 
    |- delay="float"
    |- animationOrder="[normal|reverse | random]"
    |- animation="[@anim/res_id]"
```

①`android:delay`：表示子元素开始动画的**延迟时间**。

> 比如，子元素入场动画的时间周期是300ms，那么该属性值=0.5就表示每个子元素都需要延迟150ms才能播放入场动画。

② `android:animationOrder` ：表示子元素动画的**播放顺序**。可选模式：normal （正常顺序）、random（随机顺序）、reverse（倒序）。
③`android:animation` ：为子元素指定具体的入场动画。

- 创建方法：

法一：xml定义，分两步

step1：定义layoutAnimation动画



```jsx
// res/anim/anim_layout.xml
<layoutAnimation 
    xmlns:android="http://schemas.android.com/apk/res/android"
    android:animation="@anim/anim_item"
    android:delay="0.5"
    android:animationOrder="normal">
</layoutAnimation>
```



```xml
// res/anim/anim_item.xml 
<set 
    xmlns:android="http://schemas.android.com/apk/res/android"
    android:duration="500"
    android:shareInterpolator="true"
    android:interpolator="@android:anim/accelerate_interpolator">
    <alpha
        android:fromAlpha="0"
        android:toAlpha="1" />
    <translate
        android:fromXDelta="100"
        android:toXDelta="0"
        />
</set>
```

step2：为ViewGroup设置`android:layoutAnimation`属性， 这里假设为listview：



```cpp
//activity_main.xml
<ListView
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:layoutAnimation="@anim/anim_layout"/>
```

法二：java代码创建，通过LayoutAnimation类绑定



```cpp
//和上述xml定义方法的效果相同
Animation animation = AnimationUtils.loadLayoutAnimation(this, R.anim.anim_item);
        LayoutAnimationController controller = new LayoutAnimationController(animation);//对应android:animation属性
        controller.setDelay(0.5);//对应android:delay属性       controller.setOrder(LayoutAnimationController.ORDER_NORMAL);//对应android:animationOrder属性
        listView.setLayoutAnimation(controller);//对应android:layoutAnimation属性
```

**b.Activity的切换效果**

- 该xml文件创建在**res/anim/** 下。

- Activity默认是有切换效果的，若需要自定义切换效果，需要用到

  overridePendingTransition(int inAnim, int outAnim)

  方法。

  - 参数含义：（进入的Activity所需进行的动画id，退出的Activity所需进行的动画id）
  - 该方法调用在**startActivity()**或**finish()**之**后**才生效。例如：



```css
startActivity(intent);
overridePendingTransition(R.anim.enter_anim, R.anim.exit_anim);
```

**补充实例**：[Android动画总结——布局动画、转场动画](https://www.jianshu.com/p/4e7bbe57ac8d)

------

二.**逐帧动画**

> 帧动画也是View动画的一种，它会按照顺序播放一组预先定义好的图片。对应类**AnimationDrawable**。

1.**逐帧动画的创建**

a.通过xml定义：

- 该xml文件创建在**res/drawable/** 下。
- 根节点`<animation-list>`，属性`android:oneshot`表示是否执行一次；子节点`<item>` 下可设置轮播的图片资源id和持续时间。例如：



```xml
<?xml version="1.0" encoding="utf-8"?>
<animation-list  xmlns:android="http://schemas.android.com/apk/res/android"
    android:oneshot="false">
    <item android:drawable="@drawable/xxx1" android:duration="500"/>
    <item android:drawable="@drawable/xxx2" android:duration="500"/>
    <item android:drawable="@drawable/xxx3" android:duration="500"/>
    <item android:drawable="@drawable/xxx4" android:duration="500"/>
</animation-list>
```

在xml声明好之后，将它作为View的背景并通过AnimationDrawable来播放即可。代码如下：



```undefined
mView.setBackgroundResource(R.drawable.XXX);
AnimationDrawable animationDrawable = (AnimationDrawable)mView.getBackground();
animationDrawable.start();
```

b.通过Java代码动态创建



```cpp
//和上述xml定义方法的效果相同
AnimationDrawable ad = new AnimationDrawable();//1.创建AnimationDrawable对象
    for (int i = 0; i < 4; i++) {//2.添加Drawable对象及其持续时间
        Drawable drawable = getResources().getDrawable(getResources().getIdentifier("xxx" + i, "drawable", getPackageName()));
        ad.addFrame(drawable, 500);
    }
    ad.setOneShot(false);//3.设置是否执行一次
    mView.setBackgroundResource(ad);//4.将帧动画作为view背景
    ad.start();//5.播放动画
```

> **注意**：使用祯动画要注意不能使用尺寸过大的图片，否则容易造成OOM( 内存溢出)错误。

**补充实例：**[Android 逐帧动画：关于 逐帧动画 的使用都在这里了！](https://www.jianshu.com/p/225fe1feba60)

------

三.**属性动画**

1.**Q：属性动画与View动画的不同**

A：



![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp1d30yjj30hd0boglk.jpg)

------

**2.理解插值器和估值器**

**a.插值器(Interpolator)**

- 作用：根据**时间流逝的百分比**计算出当前**属性值改变的百分比**。确定了动画效果变化的模式，如匀速变化、加速变化等等。
- 常用的系统内置插值器：
  - 线性插值器(LinearInterpolator)：匀速动画
  - 加速减速插值器(AccelerateDecelerateInterpolator)：动画两头慢中间快
  - 减速插值器(DecelerateInterpolator)：动画越来越慢
- 可针对的对象
  - View动画：插值器对应的属性是`android:interpolator`。
  - 属性动画：是实现**非匀速**动画的重要手段。
- 自定义插值器方法：实现 **Interpolator / TimeInterpolator**接口 ，然后复写**getInterpolation()**。

> - 补间动画实现 Interpolator接口、属性动画实现***TimeInterpolator\***接口。
> - TimeInterpolator接口是属性动画中新增的，用于兼容Interpolator接口。

**b.类型估值器(TypeEvaluator)**

- 作用：根据当前**属性改变的百分比**计算出**改变后的属性值**。
- 常用的系统内置的估值器：
  - 整形估值器(IntEvaluator)
  - 浮点型估值器(FloatEvaluator)
  - Color属性估值器(ArgbEvaluator)
- 针对于属性动画，View动画不需要类型估值器。是属性动画实现**非匀速**动画的重要手段。
- 自定义估值器方法：实现**TypeEvaluator**接口，然后复写**evaluate()**。

**推荐阅读：**[你真的会使用插值器与估值器吗？](https://www.jianshu.com/p/2f19fe1e3ca1)

------

**3.Q：属性动画的工作原理**

**A：**在一定时间间隔内，通过不断对值进行改变，并不断将该值赋给对象的属性，从而实现该对象在该属性上的动画效果。

> 具体体现在 ：
> step1：创建属性动画时，若未设置属性的初始值，则系统会通过该属性的**get()**方法获取初始值。故属性动画要求必须提供属性的get()方法。
> step2： 在动画播放的过程中，利用时间插值器和类型估值器获取改变后的属性值。
> step3：将改变后的属性值通过**set()**方法设置到对象中。故属性动画要求必须提供属性的set()方法。

即通过反射调用**get/set**方法。

------

**4.属性动画的实现方式**

> 在**res/animator/**下可创建属性动画的xml文件。其中，根节点`<set>`对应**AnimatorSet**类，子节点`<objectAnimator>`对应**ObjectAnimator**类、`<animator>`对应**ValueAnimator**类。常用属性：



```bash
<set
  android:ordering=["together" | "sequentially"]>

    <objectAnimator
        android:propertyName="string"
        android:duration="int"
        android:valueFrom="float | int | color"
        android:valueTo="float | int | color"
        android:startOffset="int"
        android:repeatCount="int"
        android:repeatMode=["repeat" | "reverse"]
        android:valueType=["intType" | "floatType"]/>

    <animator
        android:duration="int"
        android:valueFrom="float | int | color"
        android:valueTo="float | int | color"
        android:startOffset="int"
        android:repeatCount="int"
        android:repeatMode=["repeat" | "reverse"]
        android:valueType=["intType" | "floatType"]/>

    <set>
        ...
    </set>
</set>
```

首先先来介绍`<set>`标签下的常用属性：

- ```
  android:ordering
  ```

  ：设置动画的时序关系。可选值：

  - **together**：默认值。表示动画集合中的子动画**同时**播放。
  - **sequentially**：表示动画集合中的子动画按照**书写**的先后顺序依次播放。

接下来具体介绍属性动画的实现方式：

**a.通过ObjectAnimator实现属性动画**

- 原理：通过直接对**对象**（object）的属性值进行改变操作，从而实现动画效果。
- 对应根节点`<objectAnimator>`
- 常用属性介绍：

①`android:propertyName`：属性动画作用的属性名称。

②`android:duration`： 动画持续时长。

③`android:startOffset`：设置动画执行之前的等待时长。

④`android:repeatCount`：动画重复执行的次数。

- 默认为**0**，表示只播放一次。
- 设置为**-1或infinite**，表示无限重复。

⑤`android:repeatMode`：动画重复执行的模式。可选值：

- **restart**：表示连续重复，为默认值。
- **reverse** ：表示逆向重复。

⑥`android:valueFrom`：动画初始值。

⑦`android:valueTo`：动画结束值。

⑧`android:valueType`：动画值类型。可选值：

- **intType** ：以上两个value属性值为整型。
- **floatType**：即以上两个value属性值为浮点型，为默认值。
- 若为**color**值，则无需设置该属性。

**b.通过ValueAnimator实现属性动画**

- 原理：通过不断控制**值**（value）的变化，再不断**手动**赋给对象的属性，从而实现动画效果。

> ObjectAnimator与 ValueAnimator类的区别：
>
> - ValueAnimator 类是先改变值，然后**手动**赋值给对象的属性从而实现动画；是**间接**对对象属性进行操作；
> - ObjectAnimator 类是先改变值，然后**自动**赋值给对象的属性从而实现动画；是**直接**对对象属性进行操作；

- 对应根节点`<animator>`
- 常用属性比**<objectAnimator>**标签少一个`android:propertyName`属性，其他相同。

**推荐阅读**：[这是一篇很详细的 属性动画 总结&攻略](https://www.jianshu.com/p/2412d00a0ce4)

------

**5.属性动画的监听器**

属性动画主要使用两个接口：AnimatorUpdateListener&AnimatorListener来监听动画的播放过程。

- **AnimatorListener** ：监听动画的开始、结束、取消以及重复播放。如下：



```csharp
public static interface AnimatorListener {
    void onAnimationStart(Animator animation); //动画开始
    void onAnimationEnd(Animator animation); //动画结束
    void onAnimationCancel(Animator animation); //动画取消
    void onAnimationRepeat(Animator animation); //动画重复播放
}
```

> 为方便开发，系统提供了AnimatorListenerAdapter类，它是AnimatorListener的适配器，如此可有选择复写上述四个方法。

- **AnimatorUpdateListener** ：监听整个动画过程。每播放一帧onAnimationUpdate()就会被调用一次，如下：



```csharp
public interface AnimatorUpdateListener {
  void onAnimationUpdate(ValueAnimator var1);//在属性动画的属性值变化是回调。
}
```

**推荐阅读**：[Android中的View动画和属性动画](https://www.jianshu.com/p/b117c974deaf)



作者：厘米姑娘
链接：https://www.jianshu.com/p/10dc575896d3
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。