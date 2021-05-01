[TOC]

## 一、Paint

就是画笔,用于设置绘制风格,如:线宽(笔触粗细),颜色,透明度和填充风格等 直接使用无参构造方法就可以创建Paint实例: **Paint paint = new Paint( );** 

```java
Paint.ANTI_ALIAS_FLAG ：抗锯齿标志
Paint.FILTER_BITMAP_FLAG : 使位图过滤的位掩码标志
Paint.DITHER_FLAG : 使位图进行有利的抖动的位掩码标志
Paint.UNDERLINE_TEXT_FLAG : 下划线
Paint.STRIKE_THRU_TEXT_FLAG : 中划线
Paint.FAKE_BOLD_TEXT_FLAG : 加粗
Paint.LINEAR_TEXT_FLAG : 使文本平滑线性扩展的油漆标志
Paint.SUBPIXEL_TEXT_FLAG : 使文本的亚像素定位的绘图标志
Paint.EMBEDDED_BITMAP_TEXT_FLAG : 绘制文本时允许使用位图字体的绘图标志
```

### 1.图形绘制相关 

- **setARGB**(int a,int r,int g,int b): 设置绘制的颜色，a代表透明度，r，g，b代表颜色值。
- **setAlpha**(int a): 设置绘制图形的透明度。
- **setColor**(int color): 设置绘制的颜色，使用颜色值来表示，该颜色值包括透明度和RGB颜色。
- **setAntiAlias**(boolean aa): 设置是否使用抗锯齿功能，会消耗较大资源，绘制图形速度会变慢。
- **setDither**(boolean dither): 设定是否使用图像抖动处理，会使绘制出来的图片颜色更加平滑和饱满，图像更加清晰
- **setFilterBitmap**(boolean filter)： 如果该项设置为true，则图像在动画进行中会滤掉对Bitmap图像的优化操作， 加快显示速度，本设置项依赖于dither和xfermode的设置
- **setMaskFilter**(MaskFilter maskfilter)： 设置MaskFilter，可以用不同的MaskFilter实现滤镜的效果，如滤化，立体等
- **setColorFilter**(ColorFilter colorfilter)： 设置颜色过滤器，可以在绘制颜色时实现不用颜色的变换效果
- **setPathEffect**(PathEffect effect) 设置绘制路径的效果，如点画线等
- **setShader**(Shader shader)： 设置图像效果，使用Shader可以绘制出各种渐变效果
- **setShadowLayer**(float radius ,float dx,float dy,int color)：在图形下面设置阴影层，产生阴影效果， radius为阴影的角度，dx和dy为阴影在x轴和y轴上的距离，color为阴影的颜色
- **setStyle**(Paint.Style style)： 设置画笔的样式，为FILL，FILL_OR_STROKE，或STROKE
- **setStrokeCap**(Paint.Cap cap)： 当画笔样式为STROKE或FILL_OR_STROKE时，设置笔刷的图形样式， 如圆形样Cap.ROUND,或方形样式Cap.SQUARE
- **setSrokeJoin**(Paint.Join join)： 设置绘制时各图形的结合方式，如平滑效果等
- **setStrokeWidth**(float width)： 当画笔样式为STROKE或FILL_OR_STROKE时，设置笔刷的粗细度
- **setXfermode**(Xfermode xfermode)： 设置图形重叠时的处理方式，如合并，取交集或并集，经常用来制作橡皮的擦除效果



### 2.文本绘制相关

- **setFakeBoldText**(boolean fakeBoldText)： 模拟实现粗体文字，设置在小字体上效果会非常差
- **setSubpixelText**(boolean subpixelText)： 设置该项为true，将有助于文本在LCD屏幕上的显示效果
- **setTextAlign**(Paint.Align align)： 设置绘制文字的对齐方向
- **setTextScaleX**(float scaleX)： 设置绘制文字x轴的缩放比例，可以实现文字的拉伸的效果
- **setTextSize**(float textSize)： 设置绘制文字的字号大小
- **setTextSkewX**(float skewX)： 设置斜体文字，skewX为倾斜弧度
- **setTypeface**(Typeface typeface)： 设置Typeface对象，即字体风格，包括粗体，斜体以及衬线体，非衬线体等
- **setUnderlineText**(boolean underlineText)： 设置带有下划线的文字效果
- **setStrikeThruText**(boolean strikeThruText)： 设置带有删除线的效果
- **setStrokeJoin**(Paint.Join join)： 设置结合处的样子，Miter:结合处为锐角， Round:结合处为圆弧：BEVEL：结合处为直线
- **setStrokeMiter**(float miter)：设置画笔倾斜度
- **setStrokeCap** (Paint.Cap cap)：设置转弯处的风格 其他常用方法：
- float **ascent**( )：测量baseline之上至字符最高处的距离 
- float **descent**()：baseline之下至字符最低处的距离
- int **breakText**(char[] text, int index, int count, float maxWidth, float[] measuredWidth)： 检测一行显示多少文字
- **clearShadowLayer**( )：清除阴影层 其他的自行查阅文档~



## 二、Canvas

画笔有了，接着就到画布(Canvas)，总不能凭空作画是吧~常用方法如下：

首先是**构造方法**，Canvas的构造方法有两种：

**Canvas()**: 创建一个空的画布，可以使用setBitmap()方法来设置绘制具体的画布。

**Canvas(Bitmap bitmap)**: 以bitmap对象创建一个画布，将内容都绘制在bitmap上，因此bitmap不得为null。

接着是 

### 1.drawXXX()方法族：

以一定的坐标值在当前画图区域画图，另外图层会叠加， 即后面绘画的图层会覆盖前面绘画的图层。 比如：

- **drawRect**(RectF rect, Paint paint) ：绘制区域，参数一为RectF一个区域

- **drawPath**(Path path, Paint paint) ：绘制一个路径，参数一为Path路径对象

- **drawBitmap**(Bitmap bitmap, Rect src, Rect dst, Paint paint) ： 贴图，参数一就是我们常规的Bitmap对象，参数二是源区域(这里是bitmap)， 参数三是目标区域(应该在canvas的位置和大小)，参数四是Paint画刷对象， 因为用到了缩放和拉伸的可能，当原始Rect不等于目标Rect时性能将会有大幅损失。

  **获取Bitmap方式:**

  | 序号 | 获取方式               | 备注                                                         |
  | ---- | ---------------------- | ------------------------------------------------------------ |
  | 1    | 通过Bitmap创建         | 复制一个已有的Bitmap(*新Bitmap状态和原有的一致*) 或者 创建一个空白的Bitmap(*内容可改变*) |
  | 2    | 通过BitmapDrawable获取 | 从资源文件 内存卡 网络等地方获取一张图片并转换为内容不可变的Bitmap |
  | 3    | 通过BitmapFactory获取  | 从资源文件 内存卡 网络等地方获取一张图片并转换为内容不可变的Bitmap |

  - **通过BitmapFactory从不同位置获取Bitmap:**

    **资源文件(drawable/mipmap/raw):**

    ```java
            Bitmap bitmap = BitmapFactory.decodeResource(mContext.getResources(),R.raw.bitmap);
    ```

    **资源文件(assets):**

    ```java
            Bitmap bitmap=null;
            try {
                InputStream is = mContext.getAssets().open("bitmap.png");
                bitmap = BitmapFactory.decodeStream(is);
                is.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
    ```

    **内存卡文件:**

    ```java
        Bitmap bitmap = BitmapFactory.decodeFile("/sdcard/bitmap.png");
    ```

    **网络文件:**

    ```java
            // 此处省略了获取网络输入流的代码
            Bitmap bitmap = BitmapFactory.decodeStream(is);
            is.close();
    ```

    既然已经获得到了Bitmap，那么就开始本文的重点了，将Bitmap绘制到画布上。
    
  - **绘制Bitmap：**

    依照惯例先预览一下drawBitmap的常用方法：

    ```java
        // 第一种
        public void drawBitmap (Bitmap bitmap, Matrix matrix, Paint paint)
        
        // 第二种
        public void drawBitmap (Bitmap bitmap, float left, float top, Paint paint)
        
        // 第三种
        //指定图片在屏幕上显示(绘制)的区域
        //用src指定了图片绘制部分的区域
        //然后用dst指定了绘制在屏幕上的绘制，图片宽高会根据指定的区域自动进行缩放
        public void drawBitmap (Bitmap bitmap, Rect src, Rect dst, Paint paint)
        public void drawBitmap (Bitmap bitmap, Rect src, RectF dst, Paint paint)
    ```

  **通常来说，我们绘制Bitmap都是读取已有的图片转换为Bitmap绘制到Canvas上。**

- **drawLine**(float startX, float startY, float stopX, float stopY, Paintpaint) ： 画线，参数一起始点的x轴位置，参数二起始点的y轴位置，参数三终点的x轴水平位置， 参数四y轴垂直位置，最后一个参数为Paint 画刷对象。

- **drawPoint**(float x, float y, Paint paint)： 画点，参数一水平x轴，参数二垂直y轴，第三个参数为Paint对象。

- **drawText**(String text, float x, floaty, Paint paint) ： 渲染文本，Canvas类除了上面的还可以描绘文字，参数一是String类型的文本， 参数二x轴，参数三y轴，参数四是Paint对象。

  ```java
      // 第一类
  		// 只能指定文本基线位置(基线x默认在字符串左侧，基线y默认在字符串下方)。
      public void drawText (String text, float x, float y, Paint paint)
      public void drawText (String text, int start, int end, float x, float y, Paint paint)
      public void drawText (CharSequence text, int start, int end, float x, float y, Paint paint)
      public void drawText (char[] text, int index, int count, float x, float y, Paint paint)
      
      // 第二类
      // 可以分别指定每个文字的位置
      public void drawPosText (String text, float[] pos, Paint paint)
      public void drawPosText (char[] text, int index, int count, float[] pos, Paint paint)
      
      // 第三类
      // 指定一个路径，根据路径绘制文字
      public void drawTextOnPath (String text, Path path, float hOffset, float vOffset, Paint paint)
      public void drawTextOnPath (char[] text, int index, int count, Path path, float hOffset, float vOffset, Paint paint)
  ```

- **drawOval**(RectF oval, Paint paint)：画椭圆，参数一是扫描区域，参数二为paint对象；

- **drawCircle**(float cx, float cy, float radius,Paint paint)： 绘制圆，参数一是中心点的x轴，参数二是中心点的y轴，参数三是半径，参数四是paint对象；

- **drawArc**(RectF oval, float startAngle, float sweepAngle, boolean useCenter, Paint paint)： 画弧，参数一是RectF对象，一个矩形区域椭圆形的界限用于定义在形状、大小、电弧，参数二是起始角 (度)在电弧的开始，参数三扫描角(度)开始顺时针测量的，参数四是如果这是真的话,包括椭圆中心的电 弧,并关闭它,如果它是假这将是一个弧线,参数五是Paint对象；

### 2.clipXXX()方法族:

在当前的画图区域裁剪(clip)出一个新的画图区域，这个画图区域就是canvas 对象的当前画图区域了。比如：clipRect(new Rect())，那么该矩形区域就是canvas的当前画图区域

### 3.其他相关

- **save()**和**restore()**方法：
  - **save**( )：用来保存Canvas的状态。save之后，可以调用Canvas的平移、放缩、旋转、错切、裁剪等操作！ 
  - **restore**（）：用来恢复Canvas之前保存的状态。防止save后对Canvas执行的操作对后续的绘制有影响。 save()和restore()要配对使用(restore可以比save少,但不能多)，若restore调用次数比save多,会报错！
- **translate**(float dx, float dy)： 平移，将画布的坐标原点向左右方向移动x，向上下方向移动y.canvas的默认位置是在（0,0）
- **scale**(float sx, float sy)：扩大，x为水平方向的放大倍数，y为竖直方向的放大倍数
- **rotate**(float degrees)：旋转，angle指旋转的角度，顺时针旋转



## 三、Path

简单点说就是描点，连线~在创建好我们的Path路径后，可以调用Canvas的**drawPath**(path,paint) 将图形绘制出来~

### 1.常用方法(添加子图形)

- **addArc**(RectF oval, float startAngle, float sweepAngle：为路径添加一个多边形

- **addCircle**(float x, float y, float radius, Path.Direction dir)：给path添加圆圈

- **addOval**(RectF oval, Path.Direction dir)：添加椭圆形

- **addRect**(RectF rect, Path.Direction dir)：添加一个区域

- **addRoundRect**(RectF rect, float[] radii, Path.Direction dir)：添加一个圆角区域

  ```java
  // 第一类(基本形状)
      // 圆形
      public void addCircle (float x, float y, float radius, Path.Direction dir)
      // 椭圆
      public void addOval (RectF oval, Path.Direction dir)
      // 矩形
      public void addRect (float left, float top, float right, float bottom, Path.Direction dir)
      public void addRect (RectF rect, Path.Direction dir)
      // 圆角矩形
      public void addRoundRect (RectF rect, float[] radii, Path.Direction dir)
      public void addRoundRect (RectF rect, float rx, float ry, Path.Direction dir)
        
  //顺时针逆时针的作用
  1	在添加图形时确定闭合顺序(各个点的记录顺序)
  2	对图形的渲染结果有影响(是判断图形渲染的重要条件)
  ```

  | 类型 | 解释              | 翻译   |
  | ---- | ----------------- | ------ |
  | CW   | clockwise         | 顺时针 |
  | CCW  | counter-clockwise | 逆时针 |

- **addPath()**

  ```java
      // path
  		//是将两个Path合并成为一个
      public void addPath (Path src)
      //第一个方法多出来的两个参数是将src进行了位移之后再添加进当前path中
      public void addPath (Path src, float dx, float dy)
      //将src添加到当前path之前先使用Matrix进行变换
      public void addPath (Path src, Matrix matrix)
  ```

- **(addArc与arcTo)**

  ```java
      // addArc
      public void addArc (RectF oval, float startAngle, float sweepAngle)
      // arcTo
      public void arcTo (RectF oval, float startAngle, float sweepAngle)
      //“是否强制使用moveTo”，也就是说，是否使用moveTo将变量移动到圆弧的起点位移，也就意味着：
      public void arcTo (RectF oval, float startAngle, float sweepAngle, boolean forceMoveTo)
        
        
      // path.addArc(oval,0,270);
      // path.arcTo(oval,0,270,true);             // <-- 和上面一句作用等价
      // path.arcTo(oval,0,270);
      // path.arcTo(oval,0,270,false);             // <-- 和上面一句作用等价
  ```

  | 名称   | 作用               | 区别                                                         |
  | ------ | ------------------ | ------------------------------------------------------------ |
  | addArc | 添加一个圆弧到path | **直接添加**一个圆弧到path中                                 |
  | arcTo  | 添加一个圆弧到path | **添加一个圆弧到path，如果圆弧的起点和上次最后一个坐标点不相同，就连接两个点** |

  **forceMoveTo是什么作用呢？**

  这个变量意思为“是否强制使用moveTo”，也就是说，是否使用moveTo将变量移动到圆弧的起点位移，也就意味着：

  | forceMoveTo | 含义                                                         | 等价方法                                                     |
  | ----------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
  | true        | **将最后一个点移动到圆弧起点**，即不连接最后一个点与圆弧起点 | public void addArc (RectF oval, float startAngle, float sweepAngle) |
  | false       | 不移动，而是连接最后一个点与圆弧起点                         | public void arcTo (RectF oval, float startAngle, float sweepAngle) |

- **isEmpty**()：判断路径是否为空

- **transform**(Matrix matrix)：应用矩阵变换

- **transform**(Matrix matrix, Path dst)：应用矩阵变换并将结果放到新的路径中，即第二个参数。更高级的效果可以使用PathEffect类！

- **offset()**

  ```java
          public void offset (float dx, float dy)
          public void offset (float dx, float dy, Path dst)
  ```

  这个的作用也很简单，就是对path进行一段平移，它和Canvas中的translate作用很像，但Canvas作用于整个画布，而path的offset只作用于当前path。

  **但是第二个方法最后怎么会有一个path作为参数？**

  其实第二个方法中最后的参数dst是存储平移后的path的。

  | dst状态       | 效果                                              |
  | ------------- | ------------------------------------------------- |
  | dst不为空     | 将当前path平移后的状态存入dst中，不会影响当前path |
  | dst为空(null) | 平移将作用于当前path，相当于第一种方法            |



### 2.几个To(画线)

- **moveTo**(float x, float y)：不会进行绘制，只用于移动移动画笔
- **lineTo**(float x, float y)：用于直线绘制，默认从(0，0)开始绘制，用moveTo移动！ 比如 mPath.lineTo(300, 300); canvas.drawPath(mPath, mPaint);
- **quadTo**(float x1, float y1, float x2, float y2)： 用于绘制圆滑曲线，即贝塞尔曲线，同样可以结合moveTo使用！ ![img](http://www.runoob.com/wp-content/uploads/2015/10/81389492.jpg)
- **rCubicTo**(float x1, float y1, float x2, float y2, float x3, float y3) 同样是用来实现贝塞尔曲线的。 (x1,y1) 为控制点，(x2,y2)为控制点，(x3,y3) 为结束点。 Same as cubicTo, but the coordinates are considered relative to the current point on this contour.就是多一个控制点而已~ 绘制上述的曲线： mPath.moveTo(100, 500); mPath.cubicTo(100, 500, 300, 100, 600, 500); 如果不加上面的那个moveTo的话：则以(0,0)为起点，(100,500)和(300,100)为控制点绘制贝塞尔曲线 ![img](http://www.runoob.com/wp-content/uploads/2015/10/25024106.jpg)
- **arcTo**(RectF oval, float startAngle, float sweepAngle)： 绘制弧线（实际是截取圆或椭圆的一部分）ovalRectF为椭圆的矩形，startAngle 为开始角度， sweepAngle 为结束角度。

- **op()**

  | 逻辑名称           | 类比 | 说明                                   | 示意图                                                       |
  | ------------------ | ---- | -------------------------------------- | ------------------------------------------------------------ |
  | DIFFERENCE         | 差集 | Path1中减去Path2后剩下的部分           | [![img](https://camo.githubusercontent.com/d45426143136466472db232b919877744179b7ca/687474703a2f2f7777322e73696e61696d672e636e2f6c617267652f30303558746469326777316634336a3835676361716a3330356b30336330736e2e6a7067)](https://camo.githubusercontent.com/d45426143136466472db232b919877744179b7ca/687474703a2f2f7777322e73696e61696d672e636e2f6c617267652f30303558746469326777316634336a3835676361716a3330356b30336330736e2e6a7067) |
  | REVERSE_DIFFERENCE | 差集 | Path2中减去Path1后剩下的部分           | [![img](https://camo.githubusercontent.com/10b45757d966399a94f86c203651a8ebb2333b5d/687474703a2f2f7777322e73696e61696d672e636e2f6c617267652f30303558746469326777316634336a6261617738306a3330356b30336330736e2e6a7067)](https://camo.githubusercontent.com/10b45757d966399a94f86c203651a8ebb2333b5d/687474703a2f2f7777322e73696e61696d672e636e2f6c617267652f30303558746469326777316634336a6261617738306a3330356b30336330736e2e6a7067) |
  | INTERSECT          | 交集 | Path1与Path2相交的部分                 | [![img](https://camo.githubusercontent.com/9b1bc8dd834f6611f2455a4f572b5db078cbce01/687474703a2f2f7777332e73696e61696d672e636e2f6c617267652f30303558746469326777316634336a626a346964646a3330356b3033633734362e6a7067)](https://camo.githubusercontent.com/9b1bc8dd834f6611f2455a4f572b5db078cbce01/687474703a2f2f7777332e73696e61696d672e636e2f6c617267652f30303558746469326777316634336a626a346964646a3330356b3033633734362e6a7067) |
  | UNION              | 并集 | 包含全部Path1和Path2                   | [![img](https://camo.githubusercontent.com/0063f862fdf3e6c159d2871cd57352ca71926b90/687474703a2f2f7777322e73696e61696d672e636e2f6c617267652f30303558746469326777316634336a62716b3872626a3330356b3033636d78342e6a7067)](https://camo.githubusercontent.com/0063f862fdf3e6c159d2871cd57352ca71926b90/687474703a2f2f7777322e73696e61696d672e636e2f6c617267652f30303558746469326777316634336a62716b3872626a3330356b3033636d78342e6a7067) |
  | XOR                | 异或 | 包含Path1与Path2但不包括两者相交的部分 | [![img](https://camo.githubusercontent.com/f01c0d2e6b1b954e9808d412e19bdde549f8c396/687474703a2f2f7777332e73696e61696d672e636e2f6c617267652f30303558746469326777316634336a6279386336306a3330356b3033633073702e6a7067)](https://camo.githubusercontent.com/f01c0d2e6b1b954e9808d412e19bdde549f8c396/687474703a2f2f7777332e73696e61696d672e636e2f6c617267652f30303558746469326777316634336a6279386336306a3330356b3033633073702e6a7067) |

  ```java
      // 对 path1 和 path2 执行布尔运算，运算方式由第二个参数指定，运算结果存入到path1中。
      path1.op(path2, Path.Op.DIFFERENCE);
      
      // 对 path1 和 path2 执行布尔运算，运算方式由第三个参数指定，运算结果存入到path3中。
      path3.op(path1, path2, Path.Op.DIFFERENCE)
  ```



----

## 四、PathMeasure

- **无参构造函数：**

  ```
    PathMeasure ()
  ```

  用这个构造函数可创建一个空的 PathMeasure，但是使用之前需要先调用 setPath 方法来与 Path 进行关联。被关联的 Path 必须是已经创建好的，**如果关联之后 Path 内容进行了更改，则需要使用 setPath 方法重新关联。**

- **有参构造函数：**

  ```java
    PathMeasure (Path path, boolean forceClosed)
  ```
  
  用这个构造函数是创建一个 PathMeasure 并关联一个 Path， 其实和创建一个空的 PathMeasure 后调用 setPath 进行关联效果是一样的，同样，被关联的 Path 也必须是已经创建好的，如果关联之后 Path 内容进行了更改，则需要使用 setPath 方法重新关联。
  
  该方法有两个参数：
  
  - 第一个参数自然就是被关联的 Path 了
  - **第二个参数是用来确保 Path 闭合，如果设置为 true， 则不论之前Path是否闭合，都会自动闭合该 Path(如果Path可以闭合的话)。**
  
  **在这里有两点需要明确:**
  
  - 1. 不论 forceClosed 设置为何种状态(true 或者 false)， 都不会影响原有Path的状态，**即 Path 与 PathMeasure 关联之后，之前的的 Path 不会有任何改变。**
  - 1. forceClosed 的设置状态可能会影响测量结果，**如果 Path 未闭合但在与 PathMeasure 关联的时候设置 forceClosed 为 true 时，测量结果可能会比 Path 实际长度稍长一点，获取到到是该 Path 闭合时的状态。**
  
- **setPath、 isClosed 和 getLength**

  这三个方法都如字面意思一样，非常简单，这里就简单是叙述一下，不再过多讲解。

  - setPath 是 PathMeasure 与 Path 关联的重要方法，效果和 构造函数 中两个参数的作用是一样的。

  - isClosed 用于判断 Path 是否闭合，但是如果你在关联 Path 的时候设置 forceClosed 为 true 的话，这个方法的返回值则一定为true。

  - getLength 用于获取 Path 的总长度，在之前的测试中已经用过了。
  
- **getSegment**

  getSegment 用于获取Path的一个片段，方法如下：

  ```java
  boolean getSegment (float startD, float stopD, Path dst, boolean startWithMoveTo)
  ```

  方法各个参数释义：

  | 参数            | 作用                             | 备注                                                         |
  | --------------- | -------------------------------- | ------------------------------------------------------------ |
  | 返回值(boolean) | 判断截取是否成功                 | true 表示截取成功，**结果存入dst中**，false 截取失败，不会改变dst中内容 |
  | startD          | 开始截取位置距离 Path 起点的长度 | 取值范围: 0 <= startD < stopD <= Path总长度                  |
  | stopD           | 结束截取位置距离 Path 起点的长度 | 取值范围: 0 <= startD < stopD <= Path总长度                  |
  | dst             | 截取的 Path 将会添加到 dst 中    | **注意: 是添加，而不是替换**                                 |
  | startWithMoveTo | 起始点是否使用 moveTo            | 用于保证截取的 Path 第一个点位置不变，**不使用**startMoveTo, 保持 dst 的**连续性** |

  - 如果 startD、stopD 的数值不在取值范围 [0, getLength] 内，或者 startD == stopD 则返回值为 false，不会改变 dst 内容。
  
- 如果在安卓4.4或者之前的版本，在默认开启硬件加速的情况下，更改 dst 的内容后可能绘制会出现问题，请关闭硬件加速或者给 dst 添加一个单个操作，例如: dst.rLineTo(0, 0)
  
  | 取值  | 主要功用                                  |
  | ----- | ----------------------------------------- |
  | true  | 保证截取得到的 Path 片段**不会发生形变**  |
  | false | 保证存储截取片段的 Path(dst) 的**连续性** |

- **nextContour**

  我们知道 Path 可以由多条曲线构成，但不论是 getLength , getSegment 或者是其它方法，都只会在其中第一条线段上运行，而这个 `nextContour` 就是用于跳转到下一条曲线到方法，*如果跳转成功，则返回 true， 如果跳转失败，则返回 false。*

  如下，我们创建了一个 Path 并使其中包含了两个闭合的曲线，内部的边长是200，外面的边长是400，现在我们使用 PathMeasure 分别测量两条曲线的总长度。

  - 1.曲线的顺序与 Path 中**添加的顺序**有关。
  - 2.getLength 获取到到是**当前一条曲线分长度**，而不是整个 Path 的长度。
  - 3.getLength 等方法是针对当前的曲线(其它方法请自行验证)。

- **getPosTan**

  这个方法是用于得到路径上**某一长度的位置以及该位置的正切值：**

  ```java
  boolean getPosTan (float distance, float[] pos, float[] tan)
  ```

  **方法各个参数释义：**

  | 参数            | 作用                 | 备注                                                         |
  | --------------- | -------------------- | ------------------------------------------------------------ |
  | 返回值(boolean) | 判断获取是否成功     | true表示成功，数据会存入 pos 和 tan 中， false 表示失败，pos 和 tan 不会改变 |
  | distance        | 距离 Path 起点的长度 | 取值范围: 0 <= distance <= getLength                         |
  | pos             | 该点的坐标值         | 当前点在画布上的位置，有两个数值，分别为x，y坐标。           |
  | tan             | 该点的正切值         | 当前点在曲线上的方向，使用 Math.atan2(tan[1], tan[0]) 获取到正切角的弧度值。 |

- **getMatrix**

  这个方法是用于得到路径上某一长度的位置以及该位置的**正切值的矩阵**：

  ```java
  boolean getMatrix (float distance, Matrix matrix, int flags)
  measure.getMatrix(distance, matrix, PathMeasure.TANGENT_MATRIX_FLAG | PathMeasure.POSITION_MATRIX_FLAG);
  ```

  方法各个参数释义：

  | 参数            | 作用                             | 备注                                                         |
  | --------------- | -------------------------------- | ------------------------------------------------------------ |
  | 返回值(boolean) | 判断获取是否成功                 | true表示成功，数据会存入matrix中，false 失败，matrix内容不会改变 |
  | distance        | 距离 Path 起点的长度             | 取值范围: 0 <= distance <= getLength                         |
  | matrix          | 根据 falgs 封装好的matrix        | 会根据 flags 的设置而存入不同的内容                          |
  | flags           | **规定哪些内容会存入到matrix中** | 可选择 POSITION_MATRIX_FLAG(位置)  ANGENT_MATRIX_FLAG(正切)  |

  其实这个方法就相当于我们在前一个例子中封装 `matrix` 的过程由 `getMatrix` 替我们做了，我们可以直接得到一个封装好到 `matrix`，岂不快哉。

  - 1.对 `matrix` 的操作必须要在 `getMatrix` 之后进行，否则会被 `getMatrix` 重置而导致无效。
  - 2.矩阵对旋转角度默认为图片的左上角，我们此处需要使用 `preTranslate` 调整为图片中心。
  - 3.pre(矩阵前乘) 与 post(矩阵后乘) 的区别



---



## 五、Matrix

**Matrix作用就是坐标映射**

### 1、Matrix特点

- 作用范围更广，Matrix在View，图片，动画效果等各个方面均有运用，相比与之前讲解等画布操作应用范围更广。
- 更加灵活，画布操作是对Matrix的封装，Matrix作为更接近底层的东西，必然要比画布操作更加灵活。
- 封装很好，Matrix本身对各个方法就做了很好的封装，让开发者可以很方便的操作Matrix。
- 难以深入理解，很难理解中各个数值的意义，以及操作规律，如果不了解矩阵，也很难理解前乘，后乘。

### 2、基本原理

- [维基百科-矩阵乘法](https://zh.wikipedia.org/wiki/%E7%9F%A9%E9%99%A3%E4%B9%98%E6%B3%95)

- 基本变换有4种: **平移(translate)、缩放(scale)、旋转(rotate) 和 错切(skew)**

![http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/e5NkWP*Tx0o6LUjncQi1I3SLQZw78NnOgMRO.2QSHUo!/r/dMUAAAAAAAAA](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/e5NkWP*Tx0o6LUjncQi1I3SLQZw78NnOgMRO.2QSHUo!/r/dMUAAAAAAAAA)

- **前乘(pre)**

  前乘相当于矩阵的右乘：

  **M‘=M·S**

  > 这表示一个矩阵与一个特殊矩阵前乘后构造出结果矩阵。

- **后乘(post)**

  后乘相当于矩阵的左乘：

  **M‘=S·M**

  > 这表示一个矩阵与一个特殊矩阵后乘后构造出结果矩阵。

- **设置(set)**

  设置使用的不是矩阵乘法，而是直接覆盖掉原来的数值，所以，**使用设置可能会导致之前的操作失效**。



### 3、错误结论

**首先澄清两个错误结论，记住，是错误结论，错误结论，错误结论。**

- ~~错误结论一：pre 是顺序执行，post 是逆序执行。~~
- ~~错误结论二：pre 是先执行，而 post 是后执行。~~

例如下面这样：

```java
// 第一段 pre  顺序执行，先平移(T)后旋转(R)
Matrix matrix = new Matrix();
matrix.preTranslate(pivotX,pivotY);
matrix.preRotate(angle);
Log.e("Matrix", matrix.toShortString());

// 第二段 post 逆序执行，先平移(T)后旋转(R)
Matrix matrix = new Matrix();
matrix.postRotate(angle);
matrix.postTranslate(pivotX,pivotY)
Log.e("Matrix", matrix.toShortString());

//这两段代码最终结果是等价的，于是轻松证得这个结论的正确性，但事实真是这样么？

首先，从数学角度分析，pre 和 post 就是右乘或者左乘的区别，其次，它们不可能实际影响运算顺序(程序执行顺序)。以上这两段代码等价也仅仅是因为最终化简公式一样而已。
```

- **总之不要把pre和post看成顺序，是左右乘的意思**

```java
pre  : 右乘， M‘ = M*A
post : 左乘， M’ = A*M
```



### 4、构造方法

- **无参构造**

```java
Matrix ()
```

创建一个全新的Matrix，使用格式如下：

```java
Matrix matrix = new Matrix();
```

通过这种方式创建出来的并不是一个数值全部为空的矩阵，而是一个**三阶单位矩阵**,如下:

```java
[1 0 0]
[0 1 0]
[0 0 1]
```

- **有参构造**

```java
Matrix (Matrix src)
```

这种方法则需要一个已经存在的矩阵作为参数，使用格式如下:

```java
Matrix matrix = new Matrix(src);
```

创建一个Matrix，并对src深拷贝(理解为新的matrix和src是两个对象，但内部数值相同即可)。



### 5、基本方法

基本方法内容比较简单，在此处简要介绍一下。

1. **equals**

   比较两个Matrix的数值是否相同。

2. **hashCode**

   获取Matrix的哈希值。

3. **toString**

   将Matrix转换为字符串: `Matrix{[1.0, 0.0, 0.0][0.0, 1.0, 0.0][0.0, 0.0, 1.0]}`

4. **toShortString**

   将Matrix转换为短字符串: `[1.0, 0.0, 0.0][0.0, 1.0, 0.0][0.0, 0.0, 1.0]`



### 6、数值操作

数值操作这一组方法可以帮助我们直接控制Matrix里面的数值。

1. **set**

```java
void set (Matrix src)
```

没有返回值，有一个参数，作用是将参数Matrix的数值复制到当前Matrix中。如果参数为空，则重置当前Matrix，相当于`reset()`。

2. **reset**

```java
void reset ()
```

重置当前Matrix(将当前Matrix重置为单位矩阵)。

3. **setValues**

```java
void setValues (float[] values)
```

setValues的参数是浮点型的一维数组，长度需要大于9，拷贝数组中的前9位数值赋值给当前Matrix。

4. **getValues**

```java
void getValues (float[] values)
```

很显然，getValues和setValues是一对方法，参数也是浮点型的一维数组，长度需要大于9，将Matrix中的数值拷贝进参数的前9位中。

- **数值计算**

  ```java
//计算一组点基于当前Matrix变换后的位置，
  //(由于是计算点，所以参数中的float数组长度一般都是偶数的,若为奇数，则最后一个数值不参与计算)
  - **mapPoints**
    //测量半径，由于圆可能会因为画布变换变成椭圆，所以此处测量的是平均半径。
  - **mapRadius**
    //boolean mapRect (RectF rect) 测量rect并将测量结果放入rect中，
    //返回值是判断矩形经过变换后是否仍为矩形。
  - **mapRect**
      //mapVectors 与 mapPoints 基本上是相同的，可以直接参照上面的mapPoints使用方法。
      //而两者唯一的区别就是mapVectors不会受到位移的影响，这符合向量的定律
  - **mapVectors**
  ```
  
  - **mapPoints**
  
    - 方法三 
  
    | 参数       | 摘要                     |
    | ---------- | ------------------------ |
    | dst        | 目标数据                 |
    | dstIndex   | 目标数据存储位置起始下标 |
    | src        | 源数据                   |
    | srcIndex   | 源数据存储位置起始下标   |
    | pointCount | 计算的点个数             |
  ```java
  // 初始数据为三个点 (0, 0) (80, 100) (400, 300) 
  float[] pts = new float[]{0, 0, 80, 100, 400, 300};
  
  // 构造一个matrix，x坐标缩放0.5
  Matrix matrix = new Matrix();
  matrix.setScale(0.5f, 1f);
  
  // 输出pts计算之前数据
  Log.i(TAG, "before: "+ Arrays.toString(pts));
  
  // 调用map方法计算
  matrix.mapPoints(pts);
  
  // 输出pts计算之后数据
  Log.i(TAG, "after : "+ Arrays.toString(pts));
  
  //输出结果
  before: [0.0, 0.0, 80.0, 100.0, 400.0, 300.0]
  after : [0.0, 0.0, 40.0, 100.0, 200.0, 300.0]
  ```
  
  ```JAVA
  ###变换一部分
  // 初始数据为三个点 (0, 0) (80, 100) (400, 300)
  float[] src = new float[]{0, 0, 80, 100, 400, 300};
  float[] dst = new float[6];
  
  // 构造一个matrix，x坐标缩放0.5
  Matrix matrix = new Matrix();
  matrix.setScale(0.5f, 1f);
  
  // 输出计算之前数据
  Log.i(TAG, "before: src="+ Arrays.toString(src));
  Log.i(TAG, "before: dst="+ Arrays.toString(dst));
  
  // 调用map方法计算(最后一个2表示两个点，即四个数值,并非两个数值)
  matrix.mapPoints(dst, 0, src, 2, 2);
  
  // 输出计算之后数据
  Log.i(TAG, "after : src="+ Arrays.toString(src));
  Log.i(TAG, "after : dst="+ Arrays.toString(dst));
  
  before: src=[0.0, 0.0, 80.0, 100.0, 400.0, 300.0]
  before: dst=[0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
  after : src=[0.0, 0.0, 80.0, 100.0, 400.0, 300.0]
  after : dst=[40.0, 100.0, 200.0, 300.0, 0.0, 0.0]
  ```
  
  - **mapRadius**
  
  ```java
  float radius = 100;
  float result = 0;
  
  // 构造一个matrix，x坐标缩放0.5
  Matrix matrix = new Matrix();
  matrix.setScale(0.5f, 1f);
  
  Log.i(TAG, "mapRadius: "+radius);
  
  result = matrix.mapRadius(radius);
  
  Log.i(TAG, "mapRadius: "+result);
  
  mapRadius: 100.0
  mapRadius: 70.71068
  ```



### 7、set、pre 与 post

对于四种基本变换 平移(translate)、缩放(scale)、旋转(rotate)、 错切(skew) 它们每一种都三种操作方法，分别为 设置(set)、 前乘(pre) 和 后乘 (post)。而它们的基础是Concat，通过先构造出特殊矩阵然后用原始矩阵Concat特殊矩阵，达到变换的结果。

**关于四种基本变换的知识和三种对应操作的区别，详细可以参考 Canvas之画布操作 和 Matrix原理 这两篇文章的内容。**

由于之前的文章已经详细的讲解过了它们的原理与用法，所以此处就简要的介绍一下:

| 方法 | 简介                                                   |
| ---- | ------------------------------------------------------ |
| set  | 设置，会覆盖掉之前的数值，导致之前的操作失效。         |
| pre  | 前乘，相当于矩阵的右乘， `M' = M * S` (S指为特殊矩阵)  |
| post | 后乘，相当于矩阵的左乘，`M' = S * M` （S指为特殊矩阵） |

**Matrix 相关的重要知识：**

- 1.一开始从Canvas中获取到到Matrix并不是初始矩阵，而是经过偏移后到矩阵，且偏移距离就是距离屏幕左上角的位置。

- > 这个可以用于判定View在屏幕上的绝对位置，View可以根据所处位置做出调整。

- 2.构造Matrix时使用的是矩阵乘法，前乘(pre)与后乘(post)结果差别很大。

- > 这个直接参见上一篇文章 [Matrix原理](http://www.gcssloop.com/2015/02/Matrix_Basic/) 即可。

- 3.受矩阵乘法影响，后面的执行的操作可能会影响到之前的操作。

- > 使用时需要注意构造顺序。



---



## 六、Camara

### 1、概念

- **3D坐标系**

  我们Camera使用的3维坐标系是**左手坐标系，即左手手臂指向x轴正方向，四指弯曲指向y轴正方向，此时展开大拇指指向的方向是z轴正方向**。

  **2D 和 3D 坐标是通过Matrix关联起来的，所以你可以认为两者是同一个坐标系，但又有差别，重点就是y轴方向不同。**

  ![坐标系](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/RaTkw62My7RNAahAATw9gDQWGwSzecfR.Jku1W3NGQc!/r/dL8AAAAAAAAA)

  | 坐标系       | 2D坐标系 | 3D坐标系     |
  | ------------ | -------- | ------------ |
  | 原点默认位置 | 左上角   | 左上角       |
  | X 轴默认方向 | 右       | 右           |
  | Y 轴默认方向 | 下       | 上           |
  | Z 轴默认方向 | 无       | 垂直屏幕向内 |

- **三维投影**

  > **三维投影**是将三维空间中的点映射到二维平面上的方法。由于目前绝大多数图形数据的显示方式仍是二维的，因此三维投影的应用相当广泛，尤其是在计算机图形学，工程学和工程制图中。

  三维投影一般有两种，**正交投影** 和 **透视投影**。

  - 正交投影就是我们数学上学过的 "正视图、正视图、侧视图、俯视图" 这些东西。
  - 透视投影则更像拍照片，符合**近大远小**的关系，有立体感，**我们此处使用的就是透视投影。**

- **摄像机**

  如果你学过Unity，那么你对摄像机这一个概念应该会有比较透彻的理解。在一个虚拟的3D的立体空间中，由于我们无法直接用眼睛去观察这一个空间，所以要借助摄像机采集信息，制成2D影像供我们观察。简单来说，**摄像机就是我们观察虚拟3D空间的眼睛**。

  **Android 上面观察View的摄像机默认位置在屏幕左上角，而且是距屏幕有一段距离的，假设灰色部分是手机屏幕，白色是上面的一个View，摄像机位置看起来大致就是下面这样子的(为了更好的展示摄像机的位置，做了一个空间转换效果的动图)。**

  ![相机](http://m.qpic.cn/psb?/V14L47VC0w3vOf/S5cO1Ay8SO*RjDxuWwYrVi4DDvv2p33mz6GDFuJbbeY!/c/dMAAAAAAAAAA&bo=LAG8ACwBvAACGT0!&rf=viewer_4)

  > 摄像机的位置默认是 (0, 0, -576)。其中 -576＝ -8 x 72，虽然官方文档说距离屏幕的距离是 -8, 但经过测试实际距离是 -576 像素，当距离为 -10 的时候，实际距离为 -720 像素。我使用了3款手机测试，屏幕大小和像素密度均不同，但结果都是一样的。
  >
  > 这个魔数可以在 Android 底层的图像引擎 Skia 中找到。在 Skia 中，Camera 的位置单位是英寸，英寸和像素的换算单位在 Skia 中被固定为 72 像素，而 Android 中把这个换算单位照搬了过来。

### 2、基本方法

基本方法就有两个`save` 和`restore`，主要作用为`保存当前状态和恢复到上一次保存的状态`，通常成对使用，常用格式如下:

```java
camera.save();		// 保存状态
... 				// 具体操作
camera.retore();	// 回滚状态
```



### 3、常用方法

这两个方法是Camera中最基础也是最常用的方法。

- **getMatrix**

```java
void getMatrix (Matrix matrix)
```

计算当前状态下矩阵对应的状态，并将计算后的矩阵赋值给参数matrix。

- **applyToCanvas**

```java
void applyToCanvas (Canvas canvas)
```

计算当前状态下单矩阵对应的状态，并将计算后的矩阵应用到指定的canvas上。



### 4、平移

> **声明：以下示例中 Matrix 的平移均使用 postTranslate 来演示，实际情况中使用set、pre 或 post 需要视情况而定。**

```java
void translate (float x, float y, float z)
```

和2D平移类似，只不过是多出来了一个维度，从只能在2D平面上平移到在3D空间内平移，不过，此处仍有几个要点需要重点对待。

#### 沿x轴平移

```java
camera.translate(x, 0, 0);

matrix.postTranslate(x, 0);
```

两者x轴同向，所以 Camera 和 Matrix 在沿x轴平移上是一致的。

**结论:**

一致是指平移方向和平移距离一致，在默认情况下，上面两种均可以让坐标系向右移动x个单位。

#### 沿y轴平移

这个就有点意思了，两个坐标系相互关联，但是两者的y轴方向是相反的，很容易把人搞迷糊。你可以这么玩:

```java
Camera camera = new Camera();
camera.translate(0, 100, 0);    // camera - 沿y轴正方向平移100像素

Matrix matrix = new Matrix();
camera.getMatrix(matrix);
matrix.postTranslate(0,100);    // matrix - 沿y轴正方向平移100像素

Log.i(TAG, "Matrix: "+matrix.toShortString());
```

在上面这种写法，虽然用了5行代码，但是效果却和 `Matrix matrix = new Matrix();` 一样，结果都是单位矩阵。而且看起来貌似没有啥问题，毕竟两次平移都是正向100。(~~如果遇见不懂技术的领导嫌你写代码量少，你可以这样多写几遍，反正一般人是看不出问题的。~~)

```java
Matrix: [1.0, 0.0, 0.0][0.0, 1.0, 0.0][0.0, 0.0, 1.0]
```

**结论:**

由于两者y轴相反，所以 `camera.translate(0, -y, 0);` 与 `matrix.postTranslate(0, y);`平移方向和距离一致，在默认情况下，这两种方法均可以让坐标系向下移动y个单位。

#### 沿z轴平移

这个不仅有趣，还容易蒙逼，上面两种情况再怎么闹腾也只是在2D平面上，而z轴的出现则让其有了空间感。

**当View和摄像机在同一条直线上时:** 此时沿z轴平移相当于缩放的效果，缩放中心为摄像机所在(x, y)坐标，当View接近摄像机时，看起来会变大，远离摄像机时，看起来会变小，**近大远小**。

**当View和摄像机不在同一条直线上时:** 当View远离摄像机的时候，View在缩小的同时也在不断接近摄像机在屏幕投影位置(通常情况下为Z轴，在平面上表现为接近坐标原点)。相反，当View接近摄像机的时候，View在放大的同时会远离摄像机在屏幕投影位置。

#### 结论:

关于3D效果的平移说起来比较麻烦，但你可以自己实际的体验一下，毕竟我们是生活在3D空间的，拿一张纸片来模拟View，用眼睛当做摄像机，在眼前来回移动纸片，多试几次大致就明白是怎么回事了。

| 平移 | 重点内容             |
| ---- | -------------------- |
| x轴  | 2D 和 3D 相同。      |
| y轴  | 2D 和 3D 相反。      |
| z轴  | 近大远小、视线相交。 |



### 5、旋转

旋转是Camera制作3D效果的核心，不过它制作出来的并不能算是真正的3D，而是伪3D，因为View是没有厚度的。

```java
// (API 12) 可以控制View同时绕x，y，z轴旋转，可以由下面几种方法复合而来。
void rotate (float x, float y, float z);

// 控制View绕单个坐标轴旋转
void rotateX (float deg); //从下往上翻
void rotateY (float deg); //从右往左翻
void rotateZ (float deg); //逆时针旋转
```

- **默认旋转中心**

  旋转中心默认是**坐标原点**，对于图片来说就是左上角位置。

- **如何控制旋转中心**

  我们都知道，在2D中，不论是旋转，错切还是缩放都是能够指定操作中心点位置的，但是在3D中却没有默认的方法，如果我们想要让图片围绕中心点旋转怎么办? 这就要使用到我们在[Matrix原理](http://www.gcssloop.com/customview/Matrix_Basic)提到过的方法，虽然当时因为有更好的选择方案，并不提倡这样做:

  ```java
  Matrix temp = new Matrix();					// 临时Matrix变量
  this.getMatrix(temp);						// 获取Matrix
  temp.preTranslate(-centerX, -centerY);		// 使用pre将旋转中心移动到和Camera位置相同。
  temp.postTranslate(centerX, centerY);		// 使用post将图片(View)移动到原来的位置
  ```



### 6、相机位置

我们可以使用translate和rotate来控制拍摄对象，也可以移动相机自身的位置，不过这些方法并不常用(看添加时间就知道啦)。

```java
void setLocation (float x, float y, float z); // (API 12) 设置相机位置，默认位置是(0, 0, -8)

float getLocationX ();	// (API 16) 获取相机位置的x坐标，下同
float getLocationY ();
float getLocationZ ();
```

我们知道近大远小，而物体之间的距离是相对的，让物体远离相机和让相机远离物体结果是一样的，实际上设置相机位置基本可以使用`translate`替代。

虽然设置相机位置用处并不大，但还是要提几点注意事项:

#### 相机和View的z轴距离不能为0

这个比较容易理解，当你把一个物体和相机放在同一个位置的时候，相机是拍摄不到这个物体的，正如你拿一张卡片放在手机侧面，摄像头是拍摄不到的。

#### 虚拟相机前后均可以拍摄

当View不断接近摄像机并越过摄像机位置时，仍能看到View，并且View大小会随着距离摄像机的位置越来越远而逐渐变小，你可以理解为它有前置摄像头和后置摄像头。

#### 摄像机右移等于View左移

View的状态只取决于View和摄像机之间的相对位置，不过由于单位不同，摄像机平移一个单位等于View平移72个像素。下面两段代码是等价的:

```java
Camera camera = new Camera();
camera.setLocation(1,0,-8);		// 摄像机默认位置是(0, 0, -8)
Matrix matrix = new Matrix();
camera.getMatrix(matrix);
Log.e(TAG, "location: "+matrix.toShortString() );

Camera camera2 = new Camera();
camera2.translate(-72,0,0);
Matrix matrix2 = new Matrix();
camera2.getMatrix(matrix2);
Log.e(TAG, "translate: "+matrix2.toShortString() );
```

结果:

```java
location: [1.0, 0.0, -72.0][0.0, 1.0, 0.0][0.0, 0.0, 1.0]
translate: [1.0, 0.0, -72.0][0.0, 1.0, 0.0][0.0, 0.0, 1.0
```

#### 要点

- View显示状态取决于View和摄像机之间的相对位置
- View和相机的Z轴距离不能为0

**小技巧:关于摄像机和View的位置，你可以打开手机后置摄像头，拿一张卡片来回的转动平移或者移动手机位置，观察卡片在屏幕上的变化，**



---



## 四、链接

- [扔物线公众号](https://mp.weixin.qq.com/s/0cptY8f4azzuToCY5JiWBw)
- [动画](http://www.cnblogs.com/ldq2016/p/5407061.html)
- [动画](https://www.cnblogs.com/yongdaimi/p/7909942.html)
- [菜鸟教程动画](http://www.runoob.com/w3cnote/android-tutorial-valueanimator.html)
- [GcsSloop](https://github.com/GcsSloop/AndroidNote)

