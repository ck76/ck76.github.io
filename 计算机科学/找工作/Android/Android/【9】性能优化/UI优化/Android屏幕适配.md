[TOC]

[今日头条另外找一下JessYan的解说](https://mp.weixin.qq.com/s/d9QCoBP6kV9VSWvVldVVwA)

## 一、什么是Android适配

答：因为Android系统的开放性，很多厂商都喜欢对Android系统和硬件进行个性化定制， 
以达到他们想要的样子，这种结果带来的「Android系统」和「手机屏幕」的碎片化问题。 
只对市场占用率较高的720和1080进行适配显然是不够的，为了让我们的Android应用在各种 
各样的手机上保证界面效果一致，各种手机屏幕的适配显得非常重要，也是每个开发者 
为之头痛的问题。

## 二、px和dpi和dp(dip)

- px：平常所说的1920×1080只是像素数量，也就是1920px×1080px，代表手机高度上有1920个像素点，宽度上有1080个像素点。

- dpi：要想判别手机屏幕的显示好坏，还要考虑屏幕的宽高(英寸)，也就是用dpi即每英寸多少像素来评价屏幕的显示效果。（不然假如手机分辨率是1920×1080，但是屏幕是几十寸的，那显示效果将不会很好，甚至你有可能看到小的像素块，那将更影响视觉效果。）

- dp:

  - 其实dp就是为了使得开发者设置的长度能够根据不同屏幕(分辨率/尺寸也就是dpi)获得不同的像素(px)数量。比如：我将一个控件设置长度为1dp，那么在160dpi上该控件长度为1px，在240dpi的屏幕上该控件的长度为1*240/160=1.5个像素点。
  - 也就是dp会随着不同屏幕而改变控件长度的像素数量。
  - 关于dp的官方叙述为当屏幕每英寸有160个像素时(也就是160dpi)，dp与px等价的。那如果每英寸240个像素呢？1dp—>1*240/160=1.5px，即1dp与1.5px等价了。
  - 其实记住一点，dp最终都要化为像素数量来衡量大小的，因为只有像素数量最直观。

- dpi和ppi

  DPI（dots per inch 每英寸**点数**）和 PPI（pixels per inch 每英寸**像素数**）

  Pixel是一个带颜色的方块，一个图片其实就是由这些方块组成的。
  dot就是一个点，打印机或屏幕通过这些点把图片print出来。

- DensityUtil.java

  ```java
  **
   * 尺寸转换工具
   */
  public class DensityUtils {
      /**
       * dpתpx
       *
       * @param context
       * @return
       */
      public static int dp2px(Context context, float dpVal)
      {
  
          return (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP,
                  dpVal, context.getResources().getDisplayMetrics());
      }
  
      /**
       * spתpx
       *
       * @param context
       * @return
       */
      public static int sp2px(Context context, float spVal)
      {
          return (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_SP,
                  spVal, context.getResources().getDisplayMetrics());
      }
  
      /**
       * pxתdp
       *
       * @param context
       * @param pxVal
       * @return
       */
      public static float px2dp(Context context, float pxVal)
      {
          final float scale = context.getResources().getDisplayMetrics().density;
          return (pxVal / scale);
      }
  
      /**
       * pxתsp
       *
       * @param pxVal
       * @return
       */
      public static float px2sp(Context context, float pxVal)
      {
          return (pxVal / context.getResources().getDisplayMetrics().scaledDensity);
      }
  
  }
  ```



## 三、Android中的六种通用屏幕密度

|     屏幕密度      | 范围(dpi) | 标准分辨率 | dp与px     | 图标尺寸 |
| :---------------: | --------- | ---------- | ---------- | -------- |
|  **ldpi(QVGA)**   | ~120      | 240*320    | 1dp=0.75px | 36*36    |
|  **mdpi(HVGA)**   | 120~160   | 320*480    | 1dp=1px    | 48*48    |
|  **hdpi(WVGA)**   | 160~240   | 480*800    | 1dp=1.5px  | 72*72    |
|  **xhdpi(720P)**  | 240~320   | 720*1080   | 1dp=2px    | 96*96    |
| **xxhdpi(1080p)** | 320~480   | 1080*1920  | 1dp=3px    | 144*144  |
|  **xxxhdpi(2K)**  | 480~640   | 1440*2560  | 1dp=4px    | 192*192  |

Tip：图标大小 = px数 * 4 * 12



## 四、关于UI设计稿的适配

我们可以通过友盟的 [**全域罗盘**](http://compass.umeng.com/#/equipment?_k=sx0ngv) 知道当前国内的移动设备使用情况， 
以此了解需要适配的趋势



## 五、如何适配

### 1.最简单一些适配技巧

- **使用dp而非px** 

  dp是像素无关的，而在实际使用中1dp大约等于1/160 in，比如一个160dp * 160dp 
  的控件，在大多数的屏幕上都能保持1 in * 1 in 的大小。

  但是，并不是能解决所有问题的，以下两点要注意：

  ```java
  1.实际效果还是会有些差距的，仅仅是相近而已；
  
  2.当设备的物理尺寸存在差异的时候，dp就显得无能为力了。 
  
  比如，为4.3寸屏幕准备的UI，运行在5.0寸的屏幕上，很可能在 
  
  右侧和下侧存在大量的空白；而5.0寸的UI运行到4.3寸的设备上，很可能显示不下。
  ```

- **少写固定尺寸**

  「 少写固定尺寸，而使用 wrap_content, match_parent 与 weight 权重 」

- **使用相对布局，不要使用绝对布局**

  常识，而且绝对布局基本退出历史舞台了，可以忽略…

- **自动拉伸的.9图**

  常识，.9图的作用是：拉伸的时候特定的区域不会发生图片失真，而不失真的区域 
  可以由我们自己绘制，从而实现图片适配。

- **使用shape代替纯色图片**

  常识，一些纯色的矩形，圆角，圆都可以通过编写shape文件来替换，比起png， 
  xml文件小太多。

- **使用SVG矢量图替换位图**

  可能有些朋友对SVG矢量图有些陌生，其实和普通的位图最大的区别就是： 
  SVG是通过「XML文件」来定义一个图形，通过一些特定的语法和规则来绘制 
  出我们所需的图像，而不是位图那样通过存储图像中每一点的像素值来保存 
  与使用图形。

  SVG是已经定义好怎么画这个图，需要的时候再去画，因为是按照特定的语法 
  和规则，理论上支持任何级别的缩放，而且不会失真，相比起多套同样的位图 
  文件，方便太多。

  矢量图虽好，但是有几点要注意的：

  ```java
  1.适用于Android 5.0以上，尽管官方有兼容包，低版本还是会有些问题的！
  
  2.不适合细节过于复杂的图片！
  
  3.因为是用到的时候才画，所以加载图片所消耗的时间和资源可能会增加。
  
  至于怎么用这个矢量图，你可以让美工在PS里把图片导出为SVG/PSD格式，然后 
  ```



### 2.宽高限定符适配

第一种就是宽高限定符适配，什么是宽高限定符适配呢

```
├── src/main
│   ├── res
│   ├── ├──values
│   ├── ├──values-800x480
│   ├── ├──values-860x540
│   ├── ├──values-1024x600
│   ├── ├──values-1024x768
│   ├── ├──...
│   ├── ├──values-2560x1440
```

就是这种，在资源文件下生成不同分辨率的资源文件，然后在布局文件中引用对应的 **dimens**

 

## 六、头条适配方案

 

 