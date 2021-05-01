#### 提到Drawable，第一反应肯定是用于存放图片，实际上，Drawable还可以存放其他资源。那么本篇就来深入了解Drawable，要点如下：

- Drawable简介
- Drawable种类
  - BitmapDrawable
  - NinePatchDrawable
  - ShapeDrawable
  - LayerDrawable
  - StateListDrawable
  - LevelListDrawable
  - TransitionDrawable
  - InsetDrawable
  - ScaleDrawable
  - ClipDrawable
- 自定义Drawable

------

1.**Drawable简介**

a.Drawable是一种可在Canvas上进行绘制的对象，即**可绘制物**。

b.Drawable类是**抽象类**，是所有Drawable的基类。继承关系如下：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp0f9qsyj30c10a3jr8.jpg)

c.Drawable使用方式：

- 创建所需Drawable的根节点的**xml**，再通过**@drawable/xxx**引入布局中。（常用）
- **Java代码**：new一个所需Drawable并set相关属性，最后加载到布局中。

d.Drawable可通过**getIntrinsicWidth()**和**getIntrinsicHeight()**获取其**内部宽/高**。

> **注意**：并不是所有Drawable都有内部宽/高。
>
> - 图片所形成的Drawable的内部宽/高就是图片的宽/高。
> - 颜色所形成的Drawable没有内部宽/高的概念。

e.Drawable **没有大小概念**。

f.Drawable **使用范围单一**。

- 作为ImageView中的图像显示
- 作为View的背景

**推荐阅读**： [你真的会用Drawable吗](https://www.jianshu.com/p/cbfbc7d8b634)

------

2.**Drawable种类**

下面分别学习各种Drawable：

a.**BitmapDrawable**

- 表示一张图片。
- 根节点`<bitmap>`，常用属性：



```ruby
bitmap
    |- src="@drawable/res_id"
    |- antialias="[true | false]"
    |- dither="[true | false]"
    |- filter="[true | false]"
    |- tileMode="[disabled | clamp | repeat | mirror]"
    |- gravity="[top | bottom | left | right | center_vertical |
    |            fill_vertical | center_horizontal | fill_horizontal |
    |            center | fill | clip_vertical | clip_horizontal]"
```

①`android:src`：图片的资源id。
②`android:antialias`：是否开启图片**抗锯齿**。开启后会让图片会更加平滑，同时清楚度降低。
③`android:dither`：是否开启**抖动**效果。开启后让高质量的图片的比较低质量的屏幕上不失真。
④`android:filter`：是否开启**过滤**效果。当图片尺寸被拉伸或压缩时，开启过滤效果可保持较好的显示效果。
⑤`android:tileMode`：平铺模式。可选值的具体含义：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp0d9i42j30he05bmxe.jpg)

具体效果：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp0bhlulj30lq08xt9y.jpg)

⑥`android:gravity`：若位图比容器小，可以设置位图在容器中的相对位置。可选值的具体含义：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp0aqbb5j30h90dewfc.jpg)

- 使用方法： 以下两种方法效果相同，图见之前截图中所示的mirror情况。

①XML定义：



```xml
//在Drawable文件夹中创建bg_tilemode_mirror.xml
<?xml version="1.0" encoding="utf-8"?>
<bitmap xmlns:android="http://schemas.android.com/apk/res/android"
    android:dither="true"
    android:src="@mipmap/ic_launcher"
    android:tileMode="mirror"
    >
</bitmap>

//在activity_main.xml中设置为View背景
<View
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:background="@drawable/bg_tilemode_mirror"
    />
```

②Java代码：



```cpp
//在MainActivity创建BitmapDrawable
Bitmap bitmap = BitmapFactory.decodeResource(getResources(),R.mipmap.ic_launcher);
        BitmapDrawable bitDrawable = new BitmapDrawable(bitmap);
        bitDrawable.setDither(true);
        bitDrawable.setTileModeXY(Shader.TileMode.MIRROR, Shader.TileMode.MIRROR);

//加载到mylayout布局
        LinearLayout myLayout = (LinearLayout) findViewById(R.id.mylayout);
        myLayout.setBackgroundDrawable(bitDrawable);
```

**补充实例**：[BitmapDrawable （可控制对齐平铺的图像）](https://www.jianshu.com/p/59ca59808317)

------

b.**NinePatchDrawable**

- 表示一张`.9`格式的图片。
- 作用：可自动地根据所需的宽/高进行相应的缩放并保证不失真。
- 制作方法：参考[技能篇](https://www.jianshu.com/p/c9496c8bed4c)之制作Nine-Patch图片
- 根节点`<nine-patch>`，常用属性：



```bash
nine-patch
    |- src="@drawable/9_png_resid"
    |- dither="[true | false]"
    |
```

①`android:src`：图片的资源id。
②`android:dither`：是否开启**抖动**效果。开启后让高质量的图片的比较低质量的屏幕上不失真。

- 使用方法： 不建议用Java代码创建NinePatchDrawable，建议使用XML定义，代码见下。



```xml
//在Drawable文件夹中创建bg_nine_patch.xml
<?xml version="1.0" encoding="utf-8"?>
<nine-patch xmlns:android="http://schemas.android.com/apk/res/android"
    android:dither="true"
    android:src="@drawable/box"
    >
</nine-patch>

//在activity_main.xml中设置为EditText背景
<EditText
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:background="@drawable/bg_nine_patch"
    />
```

------

c.**ShapeDrawable**

- 可表示纯色、有渐变效果的基础几何图形（矩形,圆形,线条等）。
- 根节点`<shape>`，子节点`<corners>`、`<gradient>`、`<padding>`、`<size>`、`<solid>`、`<stroke>`：



```php
<?xml version="1.0" encoding="utf-8"?>
<shape
    xmlns:android="http://schemas.android.com/apk/res/android"
    android:shape="[rectangle | oval | line | ring]"
    <corners
        android:radius="integer"
        android:topLeftRaidus="integer"
        android:topRightRaidus="integer"
        android:bottomLeftRaidus="integer"
        android:bottomRightRaidus="integer" />
    <gradient
        android:angle="integer"
        android:centerX="integer"
        android:centerY="integer"
        android:centerColor="color"
        android:endColor="color"
        android:gradientRadius="integer"
        android:startColor="color"
        android:type="[linear | radial | sweep]"
        android:useLevel="[true | false]" />
    <padding
        android:left="integer"
        android:top="integer"
        android:right="integer"
        android:bottom="integer" />
    <size
        android:width="integer"
        android:height="integer" />
    <solid
        android:color="color" />
    <stroke
        android:width="integer"
        android:color="color"
        android:dashWidth="integer"
        android:dashGap="integer" />
```

接下来分别解释各个节点下属性含义：
①`<shape>`：图形的形状。

- ```
  android:shape
  ```

  可选值有：

  - **rectangle**(矩形)：为默认值。

  - **oval**(椭圆)

  - line

    (横线)：

    - 注意：**必须**通过`<stroke>`标签来指定线的宽度和颜色等信息。

  - ring

    (圆环)：

    - 注意：**必须**通过`<stroke>`标签来指定圆环线的宽度和颜色等信息。
    - 圆环还有**额外**几个属性，如下图所示：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp07bpshj30ha07uaam.jpg)

②`<corners>`：表示shape的四个圆角的角度，只适用于矩形。

- `android:radius`：为四个角同事设定相同的角度。优先级比以下4个属性要低。
- `android:topLeftRadius`：左上角的角度。
- `android:topRightRadius`：右上角的角度。
- `android:bottomLeftRadius`：左下角的角度。
- `android:bottomRightRadius`：右下角的角度。

③`<gradient>`：渐变效果，与`<solid>`纯色填充是互相排斥的。

- ```
  android:angle
  ```

  ：渐变的角度。

  - 默认为0。
  - 值必须为45的倍数。
  - 0表示从左到右，90表示从下到上。

- `android:centerX`：渐变的中心点的X坐标。

- `android:centerY`：渐变的中心点的Y坐标。

- `android:startColor`：渐变的起始色。

- `android:centerColor`：渐变的中间色。

- `android:endColor`：渐变的结束色。

- `android:gradientRadius`：渐变半径。仅当**android:type="radial"**时有效。

- `android:useLevel`：一般为false，当Drawable作StateListDrawable时为true。

- ```
  android:type
  ```

  ：渐变的类别。可选值：

  - linear(线性渐变)：默认
  - radial(径向渐变)：需要配合`android:gradient`属性一起使用。
  - sweep(扫描线渐变)

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp04uxzcj30h3087aah.jpg)

④`<padding>`：与四周空白的距离。

⑤`<size>`：图形的固有大小，非最终大小。

- `android:width`和`android:height`分别设定shape的宽/高。

⑥`<solid>`：纯色填充。

- `android:color`：指定填充的颜色。

⑦`<stroke>`：描边。属性含义：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp03f9wvj30h8059aa9.jpg)

- 实例：



```xml
<?xml version="1.0" encoding="utf-8"?>
<shape xmlns:android="http://schemas.android.com/apk/res/android"
    android:shape="rectangle">

    <solid android:color="#a9a6a6" />

    <padding
        android:bottom="7dp"
        android:left="7dp"
        android:right="7dp"
        android:top="7dp" />
    <stroke
        android:width="3dip"
        android:color="#FFFF00" />
    <corners
        android:radius="3dp"/>

</shape>
```

效果图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp028u7tj303t06109m.jpg)

------

d.**LayerDrawable**

- 表示一种**层次化**的Drawable集合，通过将不同的Drawable放置在不同的层上面从而达到一种叠加后的效果。
- 根节点`<layer-list>`，常用属性：



```swift
layer-list
    |- item
    |    |- drawable="@drawable/drawable_id"
    |    |- id="@+id/xxx_id"
    |    |- top="dimension"
    |    |- left="dimension"
    |    |- right="dimension"
    |    |- bottom="dimension"
    |
```

> 注意：每组 Drawable 由`<item>`节点进行配置，一个layer-list可包含多个item，服从下面item覆盖上面item的原则。

①`android:drawable`：所引用的位图资源id，如果为空需要有一个Drawable类型的子节点。
②`android:id`：层id。
③`android:left`：层相对于容器的左边距。
④`android:right`：层相对于容器的右边距。
⑤`android:top`：层相对于容器的上边距。
⑥`android:bottom`：层相对于容器的下边距。

- 实例：bitmap的简单叠加



```xml
<?xml version="1.0" encoding="utf-8"?>
<layer-list xmlns:android="http://schemas.android.com/apk/res/android">
    <item>
        <bitmap
            android:gravity="center"
            android:src="@mipmap/ic_launcher_round"
             />
    </item>
    <item
        android:left="20dp"
        android:top="30dp">
        <bitmap
            android:gravity="center"
            android:src="@mipmap/ic_launcher_round"
             />
    </item>
    <item
        android:left="70dp"
        android:top="80dp">
        <bitmap
            android:gravity="center"
            android:src="@mipmap/ic_launcher_round"
             />
    </item>
</layer-list>
```

效果图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp00csx2j307d06f3ym.jpg)

**补充实例**：[Drawable子类之——LayerDrawable （图层叠加）](https://www.jianshu.com/p/f1de437f4b3d)

------

e.**StateListDrawable**

- 表示一个Drawable的集合，每个Drawable对应着View的一种**状态**。
- 根节点`<selector>`，常用属性：



```ruby
selector
    |-constantSize="[true | false]"
    |-dither="[true | false]"
    |-variablePadding="[true | false]"
    |- item
    |    |- drawable="@drawable/drawable_id"
    |    |- state_pressed="[true | false]"
    |    |- state_focused="[true | false]"
    |    |- state_selected="[true | false]"
    |    |- state_hovered="[true | false]"
    |    |- state_checked="[true | false]"
    |    |- state_checkable="[true | false]"
    |    |- state_enabled="[true | false]"
    |    |- state_activated="[true | false]"
    |    |- state_window_focused="[true | false]"
    |
```

①`<selector>`：

- ```
  android:constantSize
  ```

  ：

  固有大小

  是否随其状态的改变而改变。

  - 默认为false，表示固有大小**会**随着状态的改变而改变。
  - 为true，则表示固有大小是**固定值**，是内部所有Drawable的固有大小中的**最大值**。

- `android:dither`：是否开启**抖动**效果。开启后让高质量的图片的比较低质量的屏幕上不失真。

- ```
  android:variblePadding
  ```

  ：其

  padding

  是否随状态的改变而改变。

  - 默认为false，表示padding是**固定值**，是其内部所有Drawable的padding中的**最大值**。
  - 为true，则表示padding**会**随着状态的改变而改变。

②`<item>`：

- `android:drawable`：所引用的位图资源id。
- 表示各种状态的属性：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvozyhpfyj30hd06t74o.jpg)

- 实例：[简单控件篇（下）](https://www.jianshu.com/p/11194a3405e8)实现ImageButton点击前后不同颜色

**补充实例**：[探索Android中selector和shape的结合使用](https://www.jianshu.com/p/77f9e483a324)

------

f.**LevelListDrawable**

- 表示一个Drawable集合，集合中的每个Drawable都有一个**等级**的概念。
- 根节点`<level-list>`，常用属性：



```cpp
level-list
    |- item
    |    |- drawable="@drawable/drawable_id"
    |    |- maxLevel="integer"
    |    |- minlevel="integer"
```

①`android:drawable`：引用的位图资源id。
②`android:maxLevel`：对应的最大值，取值范围为0~10000，默认为0。（常用）
③`android:minlevel`：对应的最小值，取值范围为0~10000，默认为0。

- 使用方法：无论是用xml还是代码实现，若作为View背景，都需要在Java代码中调用**setLevel()**方法；若作为ImageView前景，需要调用**setImageLevel()**。
- 加载规则：当某item的android:maxLevel **等于** setLevel所设置的数值时就会被加载。若都没有匹配的则都不显示。
- 实例：



```xml
//在Drawable文件夹中创建bg_level.xml
<?xml version="1.0" encoding="utf-8"?>
<level-list xmlns:android="http://schemas.android.com/apk/res/android">
    <item android:maxLevel="1" android:drawable="@drawable/image1" />
    <item android:maxLevel="2" android:drawable="@drawable/image2" />
    <item android:maxLevel="3" android:drawable="@drawable/image3" />
</level-list>

//在activity_main.xml中设置为ImageView背景
 <ImageView
        android:id="@+id/image"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:src="@drawable/bg_level"/>

//在MainActivity调用setImageLevel()
   ImageView imageView = (ImageView) findViewById(R.id.image);
        imageView.setImageLevel(2);
```

运行结果：ImageView的背景为image2。

------

g.**TransitionDrawable**

- LayerDrawable的子类，实现**两层** Drawable之间的**淡入淡出**效果。
- 根节点`<transition>`，常用属性和LayerDrawable相同，不再赘述。



```swift
transition
    |- item
    |    |- drawable="@drawable/drawable_id"
    |    |- id="@+id/xxx_id"
    |    |- top="dimension"
    |    |- left="dimension"
    |    |- right="dimension"
    |    |- bottom="dimension"
    |
```

- 使用方法：无论是用xml还是代码实现，若作为View背景，都需要在Java代码中调用**startTransition()**方法才能启动两层间的切换动画，也可以调用**reverseTransition()**方法反方向切换。
- 实例：



```xml
//在Drawable文件夹中创建bg_tran.xml
<?xml version="1.0" encoding="utf-8"?>
<transition xmlns:android="http://schemas.android.com/apk/res/android">
    <item android:drawable="@drawable/image1"/>
    <item android:drawable="@drawable/image2"/>
</transition>

//在activity_main.xml中设置为ImageView背景
 <ImageView
        android:id="@+id/image"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:src="@drawable/bg_tran"/>

//在MainActivity调用startTransition()
        ImageView imageView = (ImageView) findViewById(R.id.image);
        TransitionDrawable td = (TransitionDrawable) imageView.getDrawable();
        td.startTransition(3000);
```

运行结果：ImageView的背景从image1缓缓切换到image2。

------

h.**InsetDrawable**

-表示把一个Drawable嵌入到另外一个Drawable的内部，并在四周留一些间距。

> 与Drawable的padding属性不同：padding表示的是**Drawable的内容与Drawable本身**的边距；而InsetDrawable表示的是**两个Drawable与容器之间**的边距。

- 根节点`<inset>`，常用属性：



```bash
inset
    |- drawable="@drawable/drawable_id"
    |- visible="[true | false]"
    |- insetTop="dimension"
    |- insetLeft="dimension"
    |- insetRight="dimension"
    |- insetBottom="dimension"
    |
```

①`android:drawable`：所引用的位图资源id。
②`android:visible`：是否留有边距。
③`android:insetTop`：设置距离容器的上边距。其他同理。

- 适用场景：当控件需要的背景比实际的边框**小**时。
- 实例：



```xml
//在drawable文件夹下创建
<?xml version="1.0" encoding="utf-8"?>
<inset xmlns:android="http://schemas.android.com/apk/res/android"
    android:drawable="@drawable/image"
    android:insetBottom="40dp"
    android:insetLeft="10dp"
    android:insetRight="30dp"
    android:insetTop="20dp"
    android:visible="true">
</inset>
```

效果图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvozurxhoj30jg0bjjxi.jpg)

------

i.**ScaleDrawable**

- 表示将Drawable缩放到一定比例。
- 根节点`<scale>`，常用属性：



```bash
scale
    |- drawable="@drawable/drawable_id"
    |- scaleGravity="[top | bottom | left | right |
        center_vertical | center_horizontal | center |
        fill_vertical | fill_horizontal | fill |
        clip_vertical | clip_horizontal]"
    |- scaleWidth="percentage"
    |- scaleHeight="percentage"
    |
```

①`android:drawable`：所引用的位图资源id。
②`android:scaleGravity`：等同于BitmapDrawable的`android:gravity`。
③`android:scaleWidth`/`android:scaleHeight`：指定Drawable宽/高的缩放比例，以**百分比**的形式表示。

- 使用方法：无论是用xml还是代码实现，若作为View背景，都需要在Java代码中调用

  setLevel()

  方法控制Drawable等级。

  - level取值范围为0~10000。
  - 默认值为0：表示不可见；1~10000：表示可见。

- 实例：将一张图片缩小为原来的30%，代码为：



```xml
//在drawable文件夹下创建bg_scale.xml
<?xml version="1.0" encoding="utf-8"?>
<scale xmlns:android="http://schemas.android.com/apk/res/android"
    android:drawable="@drawable/drawable_test"
    android:scaleGravity="center"
    android:scaleHeight="70%"
    android:scaleWidth="70%"/>

//在activity_main.xml中设置为ImageView背景
 <ImageView
        android:id="@+id/image"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:src="@drawable/bg_scale"/>

//在MainActivity调用setLevel()
         ImageView imageView = (ImageView) findViewById(R.id.image);
        ScaleDrawable scaleDrawable = (ScaleDrawable) imageView.getDrawable();
        scaleDrawable.setLevel(1);
```

------

j.**ClipDrawable**

- 表示裁剪一个Drawable。
- 根节点`<clip>`，常用属性：



```bash
scale
    |- drawable="@drawable/drawable_id"
    |- gravity="[top | bottom | left | right |
        center_vertical | center_horizontal | center |
        fill_vertical | fill_horizontal | fill |
        clip_vertical | clip_horizontal]"
    |- clipOrientation="[vertical | horizontal]"
    |
```

①`android:drawable`：所引用的位图资源id。
②`android:gravity`：表示对齐方式，需要和clipOrientation一起发挥作用。可选值含义：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvozsrwiqj30hg0jnmxi.jpg)

③`android:clipOrientation`：表示裁剪方向，可选值有水平和竖直。

- 使用方法：无论是用xml还是代码实现，若作为View背景，都需要在Java代码中调用

  setLevel()

  方法控制可见区大小。

  - level取值范围为0~10000。
  - 0：表示完全裁剪，即不可见；10000：表示不裁剪。
  - level越大可见区越大。

**补充实例**：[结合Timer做一个简单的裁剪动画](https://www.jianshu.com/p/abc445c5a53a)，当然这个思路也可以应用到之前学习的缩放ScaleDrawable、LevelListDrawable中来实现一个小动画。

> 以上大多是使用xml来定义一个Drawable，那么使用java代码创建方法可参考文章：[Android Drawable完全解析：Drawable子类用法总结](https://www.jianshu.com/p/39f09ea26430)，同时这篇文章还讲解不少Drawable，比如`ColorDrawable`(1)、`AnimationDrawable`(2)、`RotateDrawable`(9)、`GradientDrawable`(12)、`RippleDrawable`(17)、`RoundedBitmapDrawable`(15)、`DrawerArrowDrawable`(11)、`AnimatedStateListDrawable`(5)，感兴趣可以了解学习~

------

3.**自定义Drawable**

a.Drawable的工作原理的核心是**draw()**：系统调用Drawable的draw()来绘制View的背景或ImageView的图像。

b.通常没有必要去自定义Drawable，因为无法在XML中使用自定义Drawable，这就降低了其使用范围。

c.创建自定义Drawable，必须重写其**draw()**、**setAlpha()**、**setColorFilter()**、**getOpacity()**等方法。以下为自定义Drawable示例：



```java
//自定义Drawable
public class CustomDrawable extends Drawable {
   
    private Paint mPaint;

    public CustomDrawable(int color) {
        mPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        mPaint.setColor(color);
    }

    @Override
    public void draw(Canvas canvas) {
        final Rect rect =  getBounds();
        float cx = rect.exactCenterX();
        float cy = rect.exactCenterY();
        canvas.drawCircle(cx, cy, Math.min(cx, cy), mPaint);
    }

    @Override
    public void setAlpha(int alpha) {
        mPaint.setAlpha(alpha);
        invalidateSelf();
    }

    @Override
    public void setColorFilter(ColorFilter colorFilter) {
        mPaint.setColorFilter(colorFilter);
        invalidateSelf();
    }

    @Override
    public int getOpacity() {
        return PixelFormat.TRANSLUCENT;
    }
}
```

d.当自定义的Drawable有固有大小时，最好重写**getIntrinsicWidth()**和**getIntrinsicHeight()**，因为它会影响到View的wrap_content布局。

> **注意**：Drawable的内部大小不等于Drawable的实际大小，后者可通过**getBounds()**获得，一般它和View的尺寸相同。

**推荐阅读**：[Drawable绘制过程源码分析和自定义Drawable实现动画](https://www.jianshu.com/p/e0dd55dd734a)



作者：厘米姑娘
链接：https://www.jianshu.com/p/35c7775b8202
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。