

简单理解，layer 是层，list 是列表，那么 layer-list 就是层列表的意思。但是，是什么层列表呢？？ 其实 layer-list 是用来创建 LayerDrawable 的，LayerDrawable 是 DrawableResource 的一种， 所以，layer-list 创建出来的是 图层列表，也就是一个drawable 图形。 

layer-list 的大致原理类似 RelativeLayout（或者FrameLayout） ，也是一层层的叠加 ，后添加的会覆盖先添加的。在 layer-list 中可以通过 控制后添加图层距离最底部图层的 左上右下的四个边距等属性，得到不同的显示效果。 

![layer-list](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/PGnUUR9rRVO6r7Oeik*KG7HtUPVntF17lhKfowrKWa8!/r/dFIBAAAAAAAA)

<!--more-->

### 实现TabLayout

```xml
<?xml version="1.0" encoding="utf-8"?>
<selector xmlns:android="http://schemas.android.com/apk/res/android">
    <!-- 第一种加载方式 -->
    <!--<item android:drawable="@drawable/bg_tab_selected" android:state_checked="true" />-->
    <!-- 第二种加载方式 -->
        <!--被选中时是4dp的底部边线-->
    <item android:state_checked="true">
        <layer-list>
            <!-- 红色背景 -->
            <item>
                <color android:color="#E4007F" />
            </item>
            <!-- 白色背景 -->
            <item android:bottom="4dp" android:drawable="@android:color/white" />
        </layer-list>
    </item>
        <!--未被选中的是2dp的底部边线-->
    <item>
        <layer-list>
            <!-- 红色背景 -->
            <item>
                <color android:color="#E4007F" />
            </item>
            <!-- 白色背景 -->
            <item android:bottom="2dp" android:drawable="@android:color/white" />
        </layer-list>
    </item>
</selector>
```

### 以下是带阴影的圆角矩形：

```xml
<?xml version="1.0" encoding="utf-8"?>
<layer-list xmlns:android="http://schemas.android.com/apk/res/android">
    <!-- 灰色阴影 -->
    <!--底层的左边距离上层左边3dp, 底层的顶部，距离上层的顶部6dp,如果不做这个控制，底层和上层的左侧和上侧会重合在一起-->
    <item
        android:left="3dp"
        android:top="6dp">
        <shape>
            <solid android:color="@android:color/darker_gray" />
            <corners android:radius="10dp" />
        </shape>
    </item>
    <!-- 白色前景 -->
        <!--上层的右边距离底层的右边3dp, 上层的底部距离底层的底部6dp-->
    <item
        android:bottom="6dp"
        android:right="3dp">
        <shape>
            <solid android:color="#FFFFFF" />
            <corners android:radius="10dp" />
        </shape>
    </item>
</layer-list>
```

从上面的示例代码可以看到，layer-list可以作为根节点，也可以作为selector中item的子节点。layer-list可以添加多个item子节点，每个item子节点对应一个drawable资源，按照item从上到下的顺序叠加在一起，再通过设置每个item的偏移量就可以看到阴影等效果了。layer-list的item可以通过下面四个属性设置偏移量：

- android:top 顶部的偏移量
- android:bottom 底部的偏移量
- android:left 左边的偏移量
- android:right 右边的偏移量

这四个偏移量和控件的margin设置差不多，都是外间距的效果。如何不设置偏移量，前面的图层就完全挡住了后面的图层，从而也看不到后面的图层效果了。比如上面的例子，Tab背景中的白色背景设置了android:bottom之后才能看到一点红色背景。那么如果偏移量设为负值会怎么样呢？经过验证，偏移超出的部分会被截掉而看不到，不信可以自己试一下。有时候这很有用，比如当我想显示一个半圆的时候。

另外，关于item的用法，也做下总结：

1. 根节点不同时，可设置的属性是会不同的，比如selector下，可以设置一些状态属性，而在layer-list下，可以设置偏移量；
2. 就算父节点同样是selector，放在drawable目录和放在color目录下可用的属性也会不同，比如drawable目录下可用的属性为android:drawable，在color目录下可用的属性为android:color；
3. item的子节点可以为任何类型的drawable类标签，除了上面例子中的shape、color、layer-list，也可以是selector，还有其他没讲过的bitmap、clip、scale、inset、transition、rotate、animated-rotate、lever-list等等。

### layer-list双边线

![layer-list双边线](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/UPY6Y9Obj5CZyDD9aGcUVRJidNCXOOOPi90EB3SdGxY!/r/dFQBAAAAAAAA)

- 创建带有蓝色顶部和底部边线的 layer-list 图

```xml
<?xml version="1.0" encoding="utf-8"?>
<layer-list xmlns:android="http://schemas.android.com/apk/res/android">

    <!--底层使用蓝色填充色-->
    <item>
        <shape>
            <solid android:color="#02a0ef"/>
        </shape>
    </item>

    <!--上面一层距离底层的顶部1dp,距离底部1dp,类似marginTop,填充色为白色，这样就形成了一个带有蓝色顶部边线和底部边线的白色背景的图-->
    <item android:bottom="1dp"
          android:top="1dp">
        <shape>
            <solid android:color="#fff"/>
        </shape>
    </item>
</layer-list>
```

- 使用 layer-list 图，设置为textView的背景

```xml
  <TextView
        android:layout_width="match_parent"
        android:layout_height="40dp"
        android:layout_marginTop="10dp"
        android:background="@drawable/doubleline"
        android:gravity="center"
        android:text="双边线效果"/>
```

### 另一个小例子

layer-list中的item是按照顺序从下往上叠加的，即先定义的item在下面，后面的依次往上面叠放

```xml
<?xml version="1.0" encoding="utf-8"?>
<layer-list xmlns:android="http://schemas.android.com/apk/res/android" >
    <item >
        <shape android:shape="rectangle" >
            <solid android:color="#0000ff"/>
        </shape>
    </item>
    <item android:bottom="25dp" android:top="25dp" android:left="25dp" android:right="25dp">

        <shape android:shape="rectangle" >
            <solid android:color="#00ff00" />
        </shape>
    </item>
    <item android:bottom="50dp" android:top="50dp" android:left="50dp" android:right="50dp">
        <shape android:shape="rectangle" >
            <solid android:color="#ff0000" />
        </shape>
    </item>
</layer-list>
```

布局

```xml
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
   android:layout_width="match_parent"
    android:layout_height="match_parent">
    <ImageView
        android:layout_width="150dp"
        android:layout_height="150dp" 
        android:background="@drawable/layer_list"/>
</LinearLayout>
```

效果图：

![layer-list三层正方形](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/2S3gLyIx7O8F0FxHagNJWOQHyDYR3G6hd7fR6uts7N8!/r/dEcBAAAAAAAA)

红色item最后定义在最上方，绿色item中间，最先定义蓝色最下边

这里设置了android:bottom="50dp" android:top="50dp" android:left="50dp" android:right="50dp"属性
android:top="50dp";表示该item上边**以ImageView上边界**往里面缩了50dp
android:bottom="50dp"表示该item下边**以ImageView下边界**往里面缩了50dp
android:left="50dp";表示该item左边**以ImageView左边界**往里面缩了50dp
android:right="50dp";表示该item右边**以ImageView右边界**往里面缩了50dp

android:bottom="25dp" android:top="25dp" android:left="25dp" android:right="25dp"类似