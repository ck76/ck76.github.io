[TOC]

## Path

| 作用            | 相关方法                                                     | 备注                                                         |
| --------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| 移动起点        | moveTo                                                       | 移动下一次操作的起点位置(改变下一次操作的起点)               |
| 设置终点        | setLastPoint                                                 | 重置当**前path中最后一个点位置**（**是重置上一次操作的最后一个点**），如果在绘制之前调用，效果和moveTo相同 |
| 连接直线        | lineTo                                                       | 添加上一个点(某个点)到当前点之间的直线到Path，(**指从某个点到参数坐标点之间连一条线**)  **某个点**就是上次操作结束的点，如果没有进行过操作则默认点为坐标原点 |
| 闭合路径        | close                                                        | 连接第一个点连接到最后一个点，形成一个闭合区域               |
| 添加内容        | addRect, addRoundRect, addOval, addCircle, addPath, addArc, arcTo | 添加(矩形， 圆角矩形， 椭圆， 圆， 路径， 圆弧) 到当前Path (注意addArc和arcTo的区别) |
| 是否为空        | isEmpty                                                      | 判断Path是否为空                                             |
| 是否为矩形      | isRect                                                       | 判断path是否是一个矩形                                       |
| 替换路径        | set                                                          | 用新的路径替换到当前路径所有内容                             |
| 偏移路径        | offset                                                       | 对当前路径之前的操作进行偏移(不会影响之后的操作)             |
| 贝塞尔曲线      | quadTo, cubicTo                                              | 分别为二次和三次贝塞尔曲线的方法                             |
| rXxx方法        | rMoveTo, rLineTo, rQuadTo, rCubicTo                          | **不带r的方法是基于原点的坐标系(偏移量)， rXxx方法是基于当前点坐标系(偏移量)** |
| 填充模式        | setFillType, getFillType, isInverseFillType, toggleInverseFillType | 设置,获取,判断和切换填充模式                                 |
| 提示方法        | incReserve                                                   | 提示Path还有多少个点等待加入**(这个方法貌似会让Path优化存储结构)** |
| 布尔操作(API19) | op                                                           | 对两个Path进行布尔运算(即取交集、并集等操作)                 |
| 计算边界        | computeBounds                                                | 计算Path的边界                                               |
| 重置路径        | reset, rewind                                                | 清除Path中的内容 **reset不保留内部数据结构，但会保留FillType.** **rewind会保留内部的数据结构，但不保留FillType** |
| 矩阵操作        | transform                                                    | 矩阵变换                                                     |



---



## PathMeasure

顾名思义，PathMeasure是一个用来测量Path的类，主要有以下方法:

- **构造方法**

| 方法名                                      | 释义                                                         |
| ------------------------------------------- | ------------------------------------------------------------ |
| PathMeasure()                               | 创建一个空的PathMeasure                                      |
| PathMeasure(Path path, boolean forceClosed) | 创建 PathMeasure 并关联一个指定的Path(Path需要已经创建完成)。 |

- **公共方法**

| 返回值  | 方法名                                                       | 释义                               |
| ------- | ------------------------------------------------------------ | ---------------------------------- |
| void    | setPath(Path path, boolean forceClosed)                      | 关联一个Path                       |
| boolean | isClosed()                                                   | 是否闭合                           |
| float   | getLength()                                                  | 获取Path的长度                     |
| boolean | nextContour()                                                | 跳转到下一个轮廓                   |
| boolean | getSegment(float startD, float stopD, Path dst, boolean startWithMoveTo) | 截取片段                           |
| boolean | getPosTan(float distance, float[] pos, float[] tan)          | 获取指定长度的位置坐标及该点切线值 |
| boolean | getMatrix(float distance, Matrix matrix, int flags)          | 获取指定长度的位置坐标及该点Matrix |

-----



## Canvas

| 操作分类     | 相关API                                                      | 备注                                                         |
| ------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| 绘制颜色     | drawColor, drawRGB, drawARGB                                 | 使用单一颜色填充整个画布                                     |
| 绘制基本形状 | drawPoint, drawPoints, drawLine, drawLines, drawRect, drawRoundRect, drawOval, drawCircle, drawArc | 依次为 点、线、矩形、圆角矩形、椭圆、圆、圆弧                |
| 绘制图片     | drawBitmap, drawPicture                                      | 绘制位图和图片(**使用Picture前请关闭硬件加速，以免引起不必要的问题！**) |
| 绘制文本     | drawText, drawPosText, drawTextOnPath                        | 依次为 绘制文字、绘制文字时指定每个文字位置、根据路径绘制文字 |
| 绘制路径     | drawPath                                                     | 绘制路径，绘制贝塞尔曲线时也需要用到该函数                   |
| 顶点操作     | drawVertices, drawBitmapMesh                                 | 通过对顶点操作可以使图像形变，drawVertices直接对画布作用、 drawBitmapMesh只对绘制的Bitmap作用 |
| 画布剪裁     | clipPath, clipRect                                           | 设置画布的显示区域                                           |
| 画布快照     | save, restore, saveLayerXxx, restoreToCount, getSaveCount    | 依次为 保存当前状态、 回滚到上一次保存的状态、 保存图层状态、 会滚到指定状态、 获取保存次数 |
| 画布变换     | translate, scale, rotate, skew                               | 依次为 位移、缩放、 旋转、错切 (可以改变中心点)              |
| Matrix(矩阵) | getMatrix, setMatrix, concat                                 | 实际画布的位移，缩放等操作的都是图像矩阵Matrix，只不过Matrix比较难以理解和使用，故封装了一些常用的方法。 |

| 操作类型     | 相关API                                                      | 备注                                                         |
| ------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| 基础方法     | getDensity, getWidth, getHeight，getDrawFilter，isHardwareAccelerated(API 11)，getMaximumBitmapWidth，getMaximumBitmapHeight，getDensity，quickReject，isOpaque，setBitmap，setDrawFilter | 使用单一颜色填充画布                                         |
| 绘制颜色     | drawColor, drawRGB, drawARGB，drawPaint                      | 使用单一颜色填充画布                                         |
| 绘制基本形状 | drawPoint, drawPoints, drawLine, drawLines, drawRect, drawRoundRect, drawOval, drawCircle, drawArc | 依次为 点、线、矩形、圆角矩形、椭圆、圆、圆弧                |
| 绘制图片     | drawBitmap, drawPicture                                      | 绘制位图和图片                                               |
| 绘制文本     | drawText, drawPosText, drawTextOnPath                        | 依次为 绘制文字、绘制文字时指定每个文字位置、根据路径绘制文字 |
| 绘制路径     | drawPath                                                     | 绘制路径，绘制贝塞尔曲线时也需要用到该函数                   |
| 顶点操作     | drawVertices, drawBitmapMesh                                 | 通过对顶点操作可以使图像形变，drawVertices直接对画布作用、 drawBitmapMesh只对绘制的Bitmap作用 |
| 画布剪裁     | clipPath, clipRect， clipRegion，getClipBounds               | 画布剪裁相关方法                                             |
| 画布快照     | save, restore, saveLayer, saveLayerXxx, restoreToCount, getSaveCount | 依次为 保存当前状态、 回滚到上一次保存的状态、 保存图层状态、 回滚到指定状态、 获取保存次数 |
| 画布变换     | translate, scale, rotate, skew                               | 依次为 位移、缩放、 旋转、错切(可以改变中心点)               |
| Matrix(矩阵) | getMatrix, setMatrix, concat                                 | 实际画布的位移，缩放等操作的都是图像矩阵Matrix，只不过Matrix比较难以理解和使用，故封装了一些常用的方法。 |

![canvas](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/F95KIbmi9OrJixPF3fygAUhIxHhKk3OhIfiW3r8ObL0!/r/dL4AAAAAAAAA)

- **快照(save)和回滚(restore)**

Q: 为什么存在快照与回滚
A：画布的操作是不可逆的，而且很多画布操作会影响后续的步骤，例如第一个例子，两个圆形都是在坐标原点绘制的，而因为坐标系的移动绘制出来的实际位置不同。所以会对画布的一些状态进行保存和回滚。

**与之相关的API:**

| 相关API        | 简介                                                         |
| -------------- | ------------------------------------------------------------ |
| save           | 把当前的画布的状态进行保存，然后放入特定的栈中               |
| saveLayerXxx   | 新建一个图层，并放入特定的栈中                               |
| restore        | 把栈中最顶层的画布状态取出来，并按照这个状态恢复当前的画布   |
| restoreToCount | 弹出指定位置及其以上所有的状态，并按照指定位置的状态进行恢复 |
| getSaveCount   | 获取栈中内容的数量(即保存次数)                               |



-----



## Matrix

| 方法类别   | 相关API                                                | 摘要                                           |
| ---------- | ------------------------------------------------------ | ---------------------------------------------- |
| 基本方法   | equals hashCode toString toShortString                 | 比较、 获取哈希值、 转换为字符串               |
| 数值操作   | set reset setValues getValues                          | 设置、 重置、 设置数值、 获取数值              |
| 数值计算   | mapPoints mapRadius mapRect mapVectors                 | 计算变换后的数值                               |
| 设置(set)  | setConcat setRotate setScale setSkew setTranslate      | 设置变换                                       |
| 前乘(pre)  | preConcat preRotate preScale preSkew preTranslate      | 前乘变换                                       |
| 后乘(post) | postConcat postRotate postScale postSkew postTranslate | 后乘变换                                       |
| 特殊方法   | setPolyToPoly setRectToRect rectStaysRect setSinCos    | 一些特殊操作                                   |
| 矩阵相关   | invert isAffine(API21) isIdentity                      | 求逆矩阵、 是否为仿射矩阵、 是否为单位矩阵 ... |



---



## Camara

- 主要是将3D的内容拍扁变成2D的内容,这个类更像是**一个操作Matrix的工具类**，使用Camera和Matrix可以在不使用OpenGL的情况下制作出简单的3D效果。

| 方法类别 | 相关API                                                      | 简介                   |
| -------- | ------------------------------------------------------------ | ---------------------- |
| 基本方法 | save、restore                                                | 保存、 回滚            |
| 常用方法 | getMatrix、applyToCanvas                                     | 获取Matrix、应用到画布 |
| 平移     | translate                                                    | 位移                   |
| 旋转     | rotat (API 12)、rotateX、rotateY、rotateZ                    | 各种旋转               |
| 相机位置 | setLocation (API 12)、getLocationX (API 16)、getLocationY (API 16)、getLocationZ (API 16) | 设置与获取相机位置     |

> Camera的方法并不是特别多，很多内容与之前的讲解的Canvas和Matrix类似，不过又稍有不同，之前的画布操作和Matrix主要是作用于2D空间，而Camera则主要作用于**3D空间**。



----



## 贝塞尔曲线

| 贝塞尔曲线          | 对应的方法 | 演示动画                                                     |
| ------------------- | ---------- | ------------------------------------------------------------ |
| 一阶曲线 (线性曲线) | lineTo     | ![贝塞尔曲线](http://m.qpic.cn/psb?/V14L47VC0w3vOf/9VG4wR2pF7I3d4u4m61CkxfNZ.SLo.bCOiX77wylLlE!/b/dDYBAAAAAAAA&bo=aAGWAGgBlgACGT0!&rf=viewer_4) |
| 二阶曲线            | quadTo     | ![img](http://m.qpic.cn/psb?/V14L47VC0w3vOf/yVCvjgKl.YLFDrweaPSyzB4A1YomVa*svjOFZvnW*v4!/b/dFMBAAAAAAAA&bo=aAGWAGgBlgACKQ0!&rf=viewer_4) |
| 三阶曲线            | cubicTo    | ![img](http://m.qpic.cn/psb?/V14L47VC0w3vOf/TcWiV8pcTo5YS3URetEBSBz8vE0qRFVTr3P1JXjyKdw!/b/dDcBAAAAAAAA&bo=aAGWAGgBlgACKQ0!&rf=viewer_4) |
| 四阶曲线            | 无         | ![img](http://m.qpic.cn/psb?/V14L47VC0w3vOf/NtEEkRpLLYde0YMkfHMhlt9eCQehd5HGKOyJtnd.eJ0!/b/dFQBAAAAAAAA&bo=aAGWAGgBlgACOR0!&rf=viewer_4) |



----



## View方法与ValueAnimator方法对应

![View方法与ValueAnimator方法对应](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/VcTx*TxVQRcbXaRBeVbMXinPxsxBMnxMOdFWNtT8psk!/r/dFEBAAAAAAAA)

