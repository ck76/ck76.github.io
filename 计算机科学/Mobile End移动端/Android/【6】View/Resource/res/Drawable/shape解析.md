

用代码生成图片，而且图片能随意的更改，既方便又节省空间，下面就介绍用shape生成自定义图形的方法

步骤：

1. 在res/drawable下新建一个xml文件；
2. 在代码中引用这个xml文件，引用方式和图片一样。

<!--more-->

定义shape图形的语法如下：
```xml
<?xml version="1.0" encoding="utf-8"?>
<shape
    xmlns:android="http://schemas.android.com/apk/res/android"
    android:shape=["rectangle" | "oval" | "line" | "ring"]      //共有4种类型，矩形（默认）/椭圆形/直线形/环形
    // 以下4个属性只有当类型为环形时才有效
    android:innerRadius="dimension"     //内环半径
    android:innerRadiusRatio="float"    //内环半径相对于环的宽度的比例，比如环的宽度为50,比例为2.5,那么内环半径为20
    android:thickness="dimension"   //环的厚度
    android:thicknessRatio="float"     //环的厚度相对于环的宽度的比例
    android:useLevel="boolean">    //如果当做是LevelListDrawable使用时值为true，否则为false.
 
    <corners    //定义圆角，只适用于长方形类型
        android:radius="dimension"      //全部的圆角半径
        android:topLeftRadius="dimension"   //左上角的圆角半径
        android:topRightRadius="dimension"  //右上角的圆角半径
        android:bottomLeftRadius="dimension"    //左下角的圆角半径
        android:bottomRightRadius="dimension" />    //右下角的圆角半径
 
    <gradient   //定义渐变效果
        android:type=["linear" | "radial" | "sweep"]    //共有3中渐变类型，线性渐变（默认）/放射渐变/扫描式渐变
        android:angle="integer"     //渐变角度，必须为45的倍数，0为从左到右，90为从上到下
        android:centerX="float"     //渐变中心X的相当位置，范围为0～1
        android:centerY="float"     //渐变中心Y的相当位置，范围为0～1
        android:startColor="color"      //渐变开始点的颜色
        android:centerColor="color"     //渐变中间点的颜色，在开始与结束点之间
        android:endColor="color"    //渐变结束点的颜色
        android:gradientRadius="float"  //渐变的半径，只有当渐变类型为radial时才能使用
        android:useLevel=["true" | "false"] />  //使用LevelListDrawable时就要设置为true。设为
  false时才有渐变效果
 
    <padding    //内部边距
        android:left="dimension"
        android:top="dimension"
        android:right="dimension"
        android:bottom="dimension" />
 
    <size   //自定义的图形大小
        android:width="dimension"
        android:height="dimension" />
 
    <solid  //内部填充颜色
        android:color="color" />
 
    <stroke     //描边
        android:width="dimension"   //描边的宽度
        android:color="color"   //描边的颜色
        // 以下两个属性设置虚线
        android:dashWidth="dimension"   //虚线的宽度，值为0时是实线
        android:dashGap="dimension" />      //虚线的间隔

```

### 四种形状

- **rectangle**: 矩形，默认的形状，可以画出直角矩形、圆角矩形、弧形等
- **oval**: 椭圆形，用得比较多的是画正圆
- **line**: 线形，可以画实线和虚线
- **ring**: 环形，可以画环形进度条

### 属性

**solid**: 设置形状填充的颜色，只有android:color一个属性 

- *android:color* 填充的颜色

**padding**: 设置内容与形状边界的内间距，可分别设置左右上下的距离 

- *android:left* 左内间距
- *android:right* 右内间距
- *android:top* 上内间距
- *android:bottom* 下内间距

**gradient**: 设置形状的渐变颜色，可以是线性渐变、辐射渐变、扫描性渐变 

- *android:type* 渐变的类型 
  - *linear* 线性渐变，默认的渐变类型
  - *radial* 放射渐变，设置该项时，android:gradientRadius也必须设置
  - *sweep* 扫描性渐变
- *android:startColor* 渐变开始的颜色
- *android:endColor* 渐变结束的颜色
- *android:centerColor* 渐变中间的颜色
- *android:angle* 渐变的角度，**线性渐变**时才有效，必须是45的倍数，0表示从左到右，90表示从下到上
- *android:centerX* 渐变中心的相对X坐标，**放射渐变时**才有效，在0.0到1.0之间，默认为0.5，表示在正中间
- *android:centerY* 渐变中心的相对X坐标，**放射渐变时**才有效，在0.0到1.0之间，默认为0.5，表示在正中间
- *android:gradientRadius* 渐变的半径，只有渐变类型为**放射渐变时**才使用
- *android:useLevel* 如果为true，则可在LevelListDrawable中使用

**corners**: 设置圆角，**只适用于rectangle类型**，可分别设置四个角不同半径的圆角，当设置的圆 角半径很大时，比如200dp，就可变成弧形边了 

- *android:radius* 圆角半径，会被下面每个特定的圆角属性重写
- *android:topLeftRadius* 左上角的半径
- *android:topRightRadius* 右上角的半径
- *android:bottomLeftRadius* 左下角的半径
- *android:bottomRightRadius* 右下角的半径

**stroke**: 设置描边，可描成实线或虚线。 

- *android:color* 描边的颜色
- *android:width* 描边的宽度
- *android:dashWidth* 设置虚线时的横线长度
- *android:dashGap* 设置虚线时的横线之间的距离

### 圆角矩形边框实例：

```xml
<?xml version="1.0" encoding="utf-8"?>
<shape xmlns:android="http://schemas.android.com/apk/res/android"
    android:shape="rectangle">

    <corners android:radius="10dp" />

    <stroke
        android:width="1dp"
        android:color="@color/gray" />
    
</shape>
```

