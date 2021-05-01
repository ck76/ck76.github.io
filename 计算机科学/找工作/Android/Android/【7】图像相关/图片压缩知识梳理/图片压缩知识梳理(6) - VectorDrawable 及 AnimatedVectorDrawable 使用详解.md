[TOC]

# 一、`Vector`优点

`VectorDrawable`主要有两个优点：**屏幕自适应**和**体积小**。

- 如果我们使用的是普通的`png`或者`jpg`图片，那么为了能让图片在不同分辨率的机型上能够很好地展现，通常会在各个`drawable`文件夹下放置不同分辨率大小的图片文件，而`VectorDrawable`则不需要，仅仅只使用一套资源，就能够适应任何分辨率的手机。
- 对于某些图片，`VectorDrawable`素材的大小要比`png`和`jpg`小很多。

# 二、`SVG`和`Vector`的基本概念

下面，我们先介绍一下`SVG`和`VectorDrawable`的基本概念：

- `SVG`：它并不是`Android`平台特有的概念，它的全称为**可缩放矢量图形**，简单的来说，就是用于描述二维矢量图形的图形格式，更加详细的解释可以参考：[SVG - 百度百科](https://link.jianshu.com/?t=http://baike.baidu.com/link?url=ISadOgvEUHXOGmzlZubRlMyxFxld_rUt5W6MUOKDz-bV5OqKXnTEA_rQOSgVo3NMCSp9cJ3xXh_mRV3T_FNh8a)。
- `VectorDrawable`：它是`Android`当中的`SVG`实现，它并不支持`SVG`的全部语法，只是支持部分有必要的部分。

# 三、获取`VectorDrawable`

俗话说的好，巧妇难为无米炊，这一节我们就来介绍一下如何获得一个`VectorDrawable`，一般获取的方式有以下三种方式：

- 先获取`SVG`素材，再通过工具转换成为`VectorDrawable`
- 通过`Android Studio`中的素材库，直接获取`VectorDrawable`
- 根据`Vector`的语法，自己描述

## 3.1 先获取`SVG`素材，再通过工具转换成为`VectorDrawable`

首先获取`SVG`素材，再将它转换成为`VectorDrawable`是最常用的方式，而`SVG`素材的获取又有以下几种途径：

- 网站直接下载`SVG`图像
- 由`UI`设计师使用专业的工具导出
- 通过 [VectorMagic](https://link.jianshu.com/?t=https://vectormagic.com/) 软件，将`png`和`jpg`素材转换为`SVG`图像

对于个人开发者而言，一般都会采用第一种方式，我们这里也只介绍第一种方式。很多文章都推荐过这个网站：[iconFront - 阿里巴巴](https://link.jianshu.com/?t=http://www.iconfont.cn/plus)，它提供了`SVG`和`PNG`素材的直接下载：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynh8aqumj30o40gpt9w.jpg)


下载完毕之后，在`Android Studio`的`New`选项中，选择`Vector Asset`：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynh60ejcj30e40dojvu.jpg)


打开之后，选择`Local file`，并打开我们下载后的图像，再选择`next`保存到指定的文件夹：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynh4x9i0j30jf0g90vh.jpg)


最后，我们会得到一个`*.xml`文件，这个就是`VectorDrawable`：





```xml
<vector
    xmlns:android="http://schemas.android.com/apk/res/android"
    android:height="24dp" 
    android:viewportHeight="1024.0"
    android:viewportWidth="1024.0" 
    android:width="24dp">
    <path android:fillColor="#FF000000" 
        android:pathData="M887.9,298.2c-12.9,-12.1 -33.2,-11.5 -45.2,1.4L415.9,754l-233.1,-229.7C170.2,511.9 150,512 137.5,524.6c-12.4,12.6 -12.3,32.9 0.4,45.2l256.5,252.7c0.1,0.1 0.2,0.1 0.3,0.2 0.1,0.1 0.1,0.2 0.2,0.3 2,1.9 4.4,3 6.8,4.3 1.2,0.7 2.1,1.7 3.4,2.1 3.8,1.5 7.8,2.2 11.7,2.2 4.2,0 8.4,-0.8 12.3,-2.5 1.3,-0.5 2.3,-1.7 3.6,-2.4 2.4,-1.4 4.9,-2.6 6.9,-4.7 0.1,-0.1 0.1,-0.3 0.2,-0.4 0.1,-0.1 0.2,-0.1 0.3,-0.2l449.2,-478.2C901.4,330.6 900.8,310.3 887.9,298.2z"/>
</vector>
```

## 3.2 通过`Android Studio`中的素材库，直接获取`VectorDrawable`

还是在上面的那个界面，我们勾选另外一个选项：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynh29h2oj30ji0giq5b.jpg)


这里面有`Material Design`提供的素材库：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynh0r336j30jw0e80wd.jpg)


通过这种方式，同样可以获取到一个`*.xml`的`VectorDrawable`：





```xml
<vector xmlns:android="http://schemas.android.com/apk/res/android"
        android:width="24dp"
        android:height="24dp"
        android:viewportWidth="24.0"
        android:viewportHeight="24.0">
    <path
        android:fillColor="#FF000000"
        android:pathData="M12,2C6.48,2 2,6.48 2,12s4.48,10 10,10 10,-4.48 10,-10S17.52,2 12,2zM12,5c1.66,0 3,1.34 3,3s-1.34,3 -3,3 -3,-1.34 -3,-3 1.34,-3 3,-3zM12,19.2c-2.5,0 -4.71,-1.28 -6,-3.22 0.03,-1.99 4,-3.08 6,-3.08 1.99,0 5.97,1.09 6,3.08 -1.29,1.94 -3.5,3.22 -6,3.22z"/>
</vector>
```

## 3.3 根据`Vector`的语法，自己描述

在`3.1`和`3.2`当中，我们可以看到，最终`VectorDrawable`都是用一个`*.xml`来表示的，那么，我们当然可以根据`SVG`的语法，来编写一个`xml`文件，通过`pathData`属性进行描述，不过这种方式较为繁琐，一般不会采用。

# 四、`Vector`语法

虽然说大多数情况下，我们不会完全手动去编写`Vector`的`xml`文件，但是，对于`Vector`的基本语法还是有必要了解一些的，因为在我们后边谈到的动态`Vector`中需要了解对于每个标签有哪些属性可以设置，`Vector`包含下面三种标签：



```xml
<vector>
    <group>
        <path>
        <path>
    </group>
</vector>
```

## 4.1 `<vector>`标签

- `name`：矢量图的名字
- `width, height`：矢量图的固有宽高，通常使用`dp`。
- `viewportWidth, viewportHeight`：把矢量图的宽高分成多少个单元，这里的每个单元就对应`pathData`中的一个点坐标。

## 4.2 `<path>`标签

- `name`：路径的名称。
- `fillColor`：图形的填充颜色。
- `pathData`：定义控制点的位置，类似与`Canvas`当中的`Path`类。

## 4.3 `<group>`标签

`<group>`用来把多个`<path>`组合在一起，进行相同的处理。

更多的属性可以查阅下面这两篇文章：

[VectorDrawable 详解](https://link.jianshu.com/?t=http://blog.chengyunfeng.com/?p=834&utm_source=tuicool&utm_medium=referral)
[Android 中的 SVG 图像的各个属性意义](https://link.jianshu.com/?t=http://blog.csdn.net/zyjzyj2/article/details/53530568)

# 五、`Vector`的兼容性问题

为了让`VectorDrawable`能够在`Android 5.0`以下版本的手机上使用，我们需要引入`support`包，并修改`gradle`的配置。

## 5.1 引入`support`包

`VectorDrawable`是在`Android 5.0`之后提出来的，因此，如果我们需要在低版本上使用，那么就要引入`com.android.support:appcompat-v7`包，要求版本号大于等于`23.2.0`，这里我们选用的是：



```bash
compile 'com.android.support:appcompat-v7:25.3.1'
```

与`Vector`相关的两个包为：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyngyj0r8j308j0b9dgc.jpg)



## 5.2 修改`gradle`配置文件

如果**`gradle`的版本小于`2.0`**：



```bash
android {
    defaultConfig {
        generatedDensities = []
    }

    aaptOptions {
        additionalParameters "--no-version-vectors"
    }
}
```

如果**`gradle`的版本大于`2.0`**，例如我们所用的版本：



```bash
buildscript {
    repositories {
        jcenter()
    }
    dependencies {
        classpath 'com.android.tools.build:gradle:2.3.0'
    }
}
```

那么需要这样配置：



```bash
android {
    defaultConfig {
         vectorDrawables.useSupportLibrary = true
    }
}
```

关于更多兼容性的问题，可以查看下面这篇文章

[Android Vector 曲折的兼容之路](https://link.jianshu.com/?t=http://blog.csdn.net/eclipsexys/article/details/51838119)

# 六、静态`VectorDrawable`

如果使用静态的方式展示`VectorDrawable`，那么很简单，只需要像使用普通的`drawable`一样，首先，我们按照第三节的方法，获取到一个`vectorDrawable`，并把它放在`drawable`文件夹下：



```xml
<vector xmlns:android="http://schemas.android.com/apk/res/android"
        android:width="24dp"
        android:height="24dp"
        android:viewportWidth="24.0"
        android:viewportHeight="24.0">
    <path
        android:fillColor="#FF000000"
        android:pathData="M12,2C6.48,2 2,6.48 2,12s4.48,10 10,10 10,-4.48 10,-10S17.52,2 12,2zM12,5c1.66,0 3,1.34 3,3s-1.34,3 -3,3 -3,-1.34 -3,-3 1.34,-3 3,-3zM12,19.2c-2.5,0 -4.71,-1.28 -6,-3.22 0.03,-1.99 4,-3.08 6,-3.08 1.99,0 5.97,1.09 6,3.08 -1.29,1.94 -3.5,3.22 -6,3.22z"/>
</vector>
```

我们可以应用于：

- `ImageView`的`src`
- `View`的`background`
- `TextView`的`drawable`



```xml
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:orientation="vertical"
    android:gravity="center_horizontal">
    <ImageView
        android:id="@+id/iv_static"
        android:layout_width="50dp"
        android:layout_height="50dp"
        android:clickable="true"
        android:src="@drawable/ic_account_circle_selector"/>
    <View
        android:layout_width="50dp"
        android:layout_height="50dp"
        android:clickable="true"
        android:background="@drawable/ic_account_circle_selector"/>
    <TextView
        android:drawableStart="@drawable/ic_account_circle_black_24dp"
        android:text="UserName"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content" />
</LinearLayout>
```

在上面的例子中，我们还使用了`ic_account_circle_selector`，其定义如下：



```xml
<?xml version="1.0" encoding="utf-8"?>
<selector xmlns:android="http://schemas.android.com/apk/res/android">
    <item android:drawable="@drawable/ic_account_circle_black_24dp" android:state_pressed="true"/>
    <item android:drawable="@drawable/ic_account_circle_black_normal_24dp"/>
</selector>
```

`ic_account_circle_black_24dp`就是之前获取到的素材，而`ic_account_circle_black_normal_24dp`就是改变了它的`fillColor`属性：



```xml
<vector xmlns:android="http://schemas.android.com/apk/res/android"
    android:width="24dp"
    android:height="24dp"
    android:viewportWidth="24.0"
    android:viewportHeight="24.0">
    <path
        android:fillColor="#80000000"
        android:pathData="M12,2C6.48,2 2,6.48 2,12s4.48,10 10,10 10,-4.48 10,-10S17.52,2 12,2zM12,5c1.66,0 3,1.34 3,3s-1.34,3 -3,3 -3,-1.34 -3,-3 1.34,-3 3,-3zM12,19.2c-2.5,0 -4.71,-1.28 -6,-3.22 0.03,-1.99 4,-3.08 6,-3.08 1.99,0 5.97,1.09 6,3.08 -1.29,1.94 -3.5,3.22 -6,3.22z"/>
</vector>
```

点击后的效果为：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyngwjjavg308w0fodv7.gif)

# 七、动态`VectorDrawable`

动态的`VectorDrawable`，也就是`AnimatedVectorDrawable`，一般来说，它的整个结构如下图所示：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynguan5bj30xc0f5tbi.jpg)


为了实现动态的`VectorDrawable`，一般需要提供三种类型的`*.xml`文件，这三个`xml`文件的根标签分别为：



- `vector`：图像资源，也就是我们上面讨论的静态`vector`，定义于`res/drawable`文件夹下。
- `objectAnimator`：定义图像资源中个元素的动画行为，定义于`res/anim`文件夹下。
- `animated - vector`：对`vector`中的元素与`objectAnimator`进行组合，定义于`res/drawable`文件夹下。

## 7.1 对`<group>`标签进行动画

下面，我们先看一个简单的例子，上面我们所谈到的三个`*.xml`文件如下：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyngrgykzj30kx05vaaj.jpg)



### 7.1.1 `vector`文件



```xml
<vector xmlns:android="http://schemas.android.com/apk/res/android"
    android:width="24dp"
    android:height="24dp"
    android:viewportWidth="24.0"
    android:viewportHeight="24.0">
    <group android:name="group_account" android:pivotX="12" android:pivotY="12">
        <path
            android:fillColor="#FF000000"
            android:pathData="M12,2C6.48,2 2,6.48 2,12s4.48,10 10,10 10,-4.48 10,-10S17.52,2 12,2zM12,5c1.66,0 3,1.34 3,3s-1.34,3 -3,3 -3,-1.34 -3,-3 1.34,-3 3,-3zM12,19.2c-2.5,0 -4.71,-1.28 -6,-3.22 0.03,-1.99 4,-3.08 6,-3.08 1.99,0 5.97,1.09 6,3.08 -1.29,1.94 -3.5,3.22 -6,3.22z"/>
    </group>
</vector>
```

这里我们生成了一个头像的`Vector`素材，它的图像如下，注意到，我们用一个`group`标签把`path`标签包裹起来，并且给它定义了一个`name`属性为`group_account`，这个属性在后面会用到。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyngq0xdxj302801tt8j.jpg)



### 7.1.2 `objectAnimator`文件



```xml
<?xml version="1.0" encoding="utf-8"?>
<set xmlns:android="http://schemas.android.com/apk/res/android">
    <objectAnimator
        android:duration="500"
        android:propertyName="rotation"
        android:repeatCount="infinite"
        android:valueFrom="0"
        android:valueTo="360"/>
</set>
```

`objectAnimator`的定义和属性动画是相同的，我们需要指定需要变换的属性，也就是`propertyName`，并通过`valueFrom/valueTo`指定变化的起点值和终点值，这里我们选择了采用无限循环的方式来变换目标的`rotation`属性。

### 7.1.3 `animated-vector`文件



```xml
<animated-vector xmlns:android="http://schemas.android.com/apk/res/android"
    android:drawable="@drawable/account_vector">
    <target
        android:animation="@anim/account_object_animator"
        android:name="group_account"/>
</animated-vector>
```

`animated-vector`和`vector`是一一对应的关系，因此需要在根标签中指定`android:drawable`，也就是在`7.1.1`中的`vector`文件。
而每一个`target`标签是内是成对的`name - animation`属性，`name`就是在`7.1.1`中声明的`group`的`name`，而`animation`则是在`7.1.2`中定义的动画文件。

如果`objectAnimator`所指定的属性在`name`所对应的标签中没有，那么不会发生任何变化。

### 7.1.4 启动动画

首先，在布局的`xml`文件中，把`imageView`的`src`指定为`7.1.3`中的`animate-vector`：



```xml
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:orientation="vertical"
    android:gravity="center_horizontal">
    <ImageView
        android:id="@+id/iv_dynamic"
        android:src="@drawable/account_animated_vector"
        android:text="UserName"
        android:layout_width="50dp"
        android:layout_height="50dp"/>
</LinearLayout>
```

在代码中，需要手动获得这个`vectorDrawable`，然后启动动画：



```java
public class VectorDrawableActivity extends AppCompatActivity {

    private ImageView mAnimateView;
    private AnimatedVectorDrawable mAnimatedVectorDrawable;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_vector_drawable);
        mAnimateView = (ImageView) findViewById(R.id.iv_dynamic);
        mAnimatedVectorDrawable = (AnimatedVectorDrawable) mAnimateView.getDrawable();
        mAnimateView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mAnimatedVectorDrawable.start();
            }
        });
    }
}
```

效果如下：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyngnb98hg30a00hn15l.gif)

## 7.2 对`<path>`标签中的属性进行动画

在`7.1`中，演示了如何对`group`标签的属性进行变换，下面，我们再演示一下如何对`path`标签中的属性进行变换。和前面类似，我们依然需要三种类性的`*.xml`文件

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyngm6xa2j30ih06l0t8.jpg)



### 7.2.1 `vector`文件



```xml
<vector xmlns:android="http://schemas.android.com/apk/res/android"
    android:width="24dp"
    android:height="24dp"
    android:viewportWidth="24.0"
    android:viewportHeight="24.0">
    <path
        android:name="path_check"
        android:fillColor="#FF000000"
        android:pathData="M9,16.17L4.83,12l-1.42,1.41L9,19 21,7l-1.41,-1.41z"/>
</vector>
```

这里，同样需要给`path`指定一个名字，用于后面和`objectAnimator`进行关联，它的素材为：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyngjuhj3j301t01p0bg.jpg)



### 7.2.2 `objectAnimator`文件



```xml
<?xml version="1.0" encoding="utf-8"?>
<set xmlns:android="http://schemas.android.com/apk/res/android">
    <objectAnimator
        android:duration="500"
        android:propertyName="trimPathEnd"
        android:valueFrom="0"
        android:valueTo="1"
        android:valueType="floatType"/>
</set>
```

这里，我们选择的是`path`标签下的`trimPathEnd`属性，它表示从`Path`终点的位置去除`Path`，与之对应的还有`trimPathStart`属性，它表示从`Path`起点的位置去除`Path`，而`trimPathOffset`则可以改变`Path`的起点，这三个值的取值范围都是`0~1`。

### 7.2.3 `animated-vector`文件



```xml
<animated-vector xmlns:android="http://schemas.android.com/apk/res/android"
    android:drawable="@drawable/check_vector">
    <target
        android:animation="@anim/check_object_animator"
        android:name="path_check"/>
</animated-vector>
```

效果如下：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyngimln1g30a00hnwov.gif)

## 7.3 `<path>`之间切换

除了上面普通的属性之外，还支持对`<path>`标签下的`<pathData>`进行改变，系统会比较两个`pathData`之间的差别，并自动产生合适的动画。需要注意，**它要求这两个`pathData`的点坐标的个数是相同的**。

### 7.3.1 `Vector`文件

这里，我们需要生成两个`vectorDrawable`，首先是起始的`VectorDrawable`：



```xml
<vector xmlns:android="http://schemas.android.com/apk/res/android"
        android:width="24dp"
        android:height="24dp"
        android:viewportWidth="24.0"
        android:viewportHeight="24.0">
    <path
        android:name="path_arrow_down"
        android:fillColor="#FF000000"
        android:pathData="M7,10l5,5 5,-5z"/>
</vector>
```

它的素材为：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmynggku9hj302601pgle.jpg)


接着是终点的`VectorDrawable`：





```xml
<vector xmlns:android="http://schemas.android.com/apk/res/android"
        android:width="24dp"
        android:height="24dp"
        android:viewportWidth="24.0"
        android:viewportHeight="24.0">
    <path
        android:fillColor="#FF000000"
        android:pathData="M7,14l5,-5 5,5z"/>
</vector>
```

它对应的素材为：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyngeuagkj301z01p742.jpg)

### 7.3.2 `objectAnimator`文件



```xml
<?xml version="1.0" encoding="utf-8"?>
<set xmlns:android="http://schemas.android.com/apk/res/android">
    <objectAnimator
        android:duration="500"
        android:propertyName="pathData"
        android:valueFrom="M7,10l5,5 5,-5z"
        android:valueTo="M7,14l5,-5 5,5z"
        android:valueType="pathType"/>
</set>
```

这里的`propertyName`和`valueType`需要分别定义为`pathData`和`pathType`，而起点和终点的值就是我们在`7.3.1`生成的两个`VectorDrawable`所对应的`pathData`属性的值。

### 7.3.3 `animated-vector`文件



```xml
<animated-vector xmlns:android="http://schemas.android.com/apk/res/android"
    android:drawable="@drawable/arrow_down_vector">
    <target
        android:animation="@anim/arrow_down_object_animator"
        android:name="path_arrow_down"/>
</animated-vector>
```

最终的效果为：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmyngdm1vcg30a00hn7hp.gif)

# 八、`VectorDrawable`的性能

关于`VectorDrawable`的性能问题，[Android Vector 曲折的兼容之路](https://link.jianshu.com/?t=http://blog.csdn.net/eclipsexys/article/details/51838119) 这篇文章说的很好，因此直接引用过来了：

> - `Bitmap`的绘制效率并不一定会比`Vector`高，它们有一定的平衡点，当`Vector`比较简单时，其效率是一定比`Bitmap`高的，所以，为了保证`Vector`的高效率，`Vector`需要更加简单，`PathData`更加标准、精简，当`Vector`图像变得非常复杂时，就需要使用`Bitmap`来代替了。

- `Vector`适用于`ICON`、`Button`、`ImageView`的图标等小的`ICON`，或者是需要的动画效果，由于`Bitmap`在`GPU`中有缓存功能，而`Vector`并没有，所以`Vector`图像不能做频繁的重绘
- `Vector`图像过于复杂时，不仅仅要注意绘制效率，初始化效率也是需要考虑的重要因素
- `SVG`加载速度会快于`PNG`，但渲染速度会慢于`PNG`，毕竟`PNG`有硬件加速，但平均下来，加载速度的提升弥补了绘制的速度缺陷。

# 九、参考文献

[1. Android Vector 曲折的兼容之路](https://link.jianshu.com/?t=http://blog.csdn.net/eclipsexys/article/details/51838119)
[2. VectorDrawable 怎么玩](https://www.jianshu.com/p/456df1434739)
[3. Android 使用矢量图（SVG, VectorDrawable）实践篇](https://www.jianshu.com/p/0555b8c1d26a)
[4. SVG - 百度百科](https://link.jianshu.com/?t=http://baike.baidu.com/link?url=ISadOgvEUHXOGmzlZubRlMyxFxld_rUt5W6MUOKDz-bV5OqKXnTEA_rQOSgVo3NMCSp9cJ3xXh_mRV3T_FNh8a)
[5. Android 中的 SVG 图像的各个属性意义](https://link.jianshu.com/?t=http://blog.csdn.net/zyjzyj2/article/details/53530568)



作者：泽毛
链接：https://www.jianshu.com/p/31d4cd2fba41
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。