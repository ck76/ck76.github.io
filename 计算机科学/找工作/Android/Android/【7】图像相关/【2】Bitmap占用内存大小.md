[TOC]

### Tip

简单来说，可以理解为 density 的数值是 1dp=density px；densityDpi 是屏幕每英寸对应多少个点（不是像素点），在 DisplayMetrics 当中，这两个的关系是线性的：

| density    | 1    | 1.5  | 2    | 3    | 4    |
| ---------- | ---- | ---- | ---- | ---- | ---- |
| densityDpi | 160  | 240  | 320  | 480  | 640  |

|     屏幕密度      | 范围(dpi) | 标准分辨率 | dp与px     | 图标尺寸 |
| :---------------: | --------- | ---------- | ---------- | -------- |
|  **ldpi(QVGA)**   | ~120      | 240*320    | 1dp=0.75px | 36*36    |
|  **mdpi(HVGA)**   | 120~160   | 320*480    | 1dp=1px    | 48*48    |
|  **hdpi(WVGA)**   | 160~240   | 480*800    | 1dp=1.5px  | 72*72    |
|  **xhdpi(720P)**  | 240~320   | 720*1080   | 1dp=2px    | 96*96    |
| **xxhdpi(1080p)** | 320~480   | 1080*1920  | 1dp=3px    | 144*144  |
|  **xxxhdpi(2K)**  | 480~640   | 1440*2560  | 1dp=4px    | 192*192  |

- RGB888(int)：R、G、B分量各占8位
- RGB565(short)：R、G、B分量分别占5、6、5位
- RGB555(short)：RGB分量都用5位表示（剩下的1位不用）
- ARGB8888(int)：A、R、G、B分量各占8位
- ARGB4444(short)：A、R、G、B分量各占4位



### 例子400*400

一张400 * 400的图片,然后放在drawable-hdpi、drawable-xhdpi文件夹中

```java
Bitmap bitmap = BitmapFactory.decodeResource(getResources(), R.drawable.header_image_hdpi);
Log.d("fishpan_log", "MainActivity.onCreate: hdpi -> " + bitmap.getByteCount());

bitmap = BitmapFactory.decodeResource(getResources(), R.drawable.header_image_xhdpi);
Log.d("fishpan_log", "MainActivity.onCreate: xhdpi -> " + bitmap.getByteCount());

--------------------------------------------------
MainActivity.onCreate: hdpi  -> 2149156			大
MainActivity.onCreate: xhdpi -> 1210000			小
```

下面我就来看下bitmap.getByteCount的方法实现.

```java
public final int getByteCount() {
    return getRowBytes() * getHeight();
}
```

**每行**占用的字节数*高度

```java
public final int getRowBytes() {
    ...省略代码
    return nativeRowBytes(mNativePtr);
}
```

......................................................

找到了对图片格式进行的计算了,ARGB_8888格式图片就是宽度 * 4，整个Bitmap占用的就是宽度 * 高度 * 4 byte;

问题来了，图片的高度宽度是怎么得到？通过上面demo我们可以明显看出，图片在内存中的大小并不是原始大小，**而是发生了变化。**



### decodeResource

 看下BitmapFactory.**decodeResource**方法

```
public static Bitmap decodeResourceStream(Resources res, TypedValue value,
            InputStream is, Rect pad, Options opts) {
    if (opts == null) {
        opts = new Options();
    }

    if (opts.inDensity == 0 && value != null) {
        final int density = value.density;
        if (density == TypedValue.DENSITY_DEFAULT) {
            opts.inDensity = DisplayMetrics.DENSITY_DEFAULT;
        } else if (density != TypedValue.DENSITY_NONE) {
            opts.inDensity = density;
        }
    }
    
    if (opts.inTargetDensity == 0 && res != null) {
        opts.inTargetDensity = res.getDisplayMetrics().densityDpi;
    }
    
    return decodeStream(is, pad, opts);
}
```



### decodeStream

首先实例化了一个Options类，并设置了inDensity、inTargetDensity属性，inDensity与图片所在文件夹相关，inTargetDensity就是设备的屏幕密度。

```
public static Bitmap decodeStream(InputStream is, Rect outPadding, Options opts) {
    ...省略部分代码
    try {
        if (is instanceof AssetManager.AssetInputStream) {
            final long asset = ((AssetManager.AssetInputStream) is).getNativeAsset();
            bm = nativeDecodeAsset(asset, outPadding, opts);
        } else {
            bm = decodeStreamInternal(is, outPadding, opts);
        }

    if (bm == null && opts != null && opts.inBitmap != null) {
        throw new IllegalArgumentException("Problem decoding into existing bitmap");
    }
    ...省略部分代码
    setDensityFromOptions(bm, opts);
    return bm;
}
```

中间会调用nativeDecodeAsset或者decodeStreamInternal方法，但是最终都会调用到BitmapFactory.cpp中的doDecode方法

```java
static jobject doDecode(JNIEnv* env, SkStreamRewindable* stream, jobject padding,
        jobject options, bool allowPurgeable, bool forcePurgeable = false) {


    if (options != NULL) {

        if (env->GetBooleanField(options, gOptions_scaledFieldID)) {
            const int density = env->GetIntField(options, gOptions_densityFieldID);
            const int targetDensity = env->GetIntField(options, gOptions_targetDensityFieldID);
            const int screenDensity = env->GetIntField(options, gOptions_screenDensityFieldID);
            if (density != 0 && targetDensity != 0 && density != screenDensity) {
                scale = (float) targetDensity / density;
            }
        }
    }


    int scaledWidth = decodingBitmap.width();
    int scaledHeight = decodingBitmap.height();

    if (willScale && mode != SkImageDecoder::kDecodeBounds_Mode) {
        scaledWidth = int(scaledWidth * scale + 0.5f);
        scaledHeight = int(scaledHeight * scale + 0.5f);
    }

    // update options (if any)
    if (options != NULL) {
        env->SetIntField(options, gOptions_widthFieldID, scaledWidth);
        env->SetIntField(options, gOptions_heightFieldID, scaledHeight);
        env->SetObjectField(options, gOptions_mimeFieldID,
                getMimeTypeString(env, decoder->getFormat()));
    }
}
```

省略一堆代码，看重点。中间计算**scale就是targetDensity / density,图片最终的宽度和高度就是scaledWidth = int(scaledWidth * scale + 0.5f); scaledHeight = int(scaledHeight * scale + 0.5f);**
 OK，现在我们就知道一个图片占多大内存了，我们通过自己的计算看看是不是符合上边的结果.



```java
我的手机是屏幕密度是440dpi
 图片位置：drawable-hdpi
 图片格式：ARGB-8888
 图片大小：400 * 400
 宽度：int(400 * （440 / 240） + 0.5) =733.8     //宽度
 高度：int(400 * （440 / 240） + 0.5) =733.8		//高度
    733.8 * 4 * 733.8= 2154045.4				//依照公式计算出的图片占用内存大小
 内存大小：2149156 								//约等于2M

图片位置：drawable-xhdpi
 图片格式：ARGB-8888
 图片大小：400 * 400
 宽度：int(400 * （440 / 320） + 0.5)
 高度：int(400 * （440 / 320） + 0.5)
 内存大小：1210000
```

**总结：通过上面的分析我们可以发现，图片占用内存大小和手机的密度成正比，所在文件夹密度成反比**



> 当我们使用资源id来去引用一张图片时，Android会使用一些规则来去帮我们匹配最适合的图片。什么叫最适合的图片？比如我的手机屏幕密度是xxhdpi，那么drawable-xxhdpi文件夹下的图片就是最适合的图片。因此，当我引用android_logo这张图时，如果drawable-xxhdpi文件夹下有这张图就会优先被使用，在这种情况下，图片是不会被缩放的。但是，如果drawable-xxhdpi文件夹下没有这张图时， 系统就会自动去其它文件夹下找这张图了，优先会去更高密度的文件夹下找这张图片，我们当前的场景就是drawable-xxxhdpi文件夹，然后发现这里也没有android_logo这张图，接下来会尝试再找更高密度的文件夹，发现没有更高密度的了，这个时候会去drawable-nodpi文件夹找这张图，发现也没有，那么就会去更低密度的文件夹下面找，依次是drawable-xhdpi -> drawable-hdpi -> drawable-mdpi -> drawable-ldpi。

当缩放时，会根据系统和使用的文件夹的density进行缩放。

| dpi     | density |
| ------- | ------- |
| xxhdpi  | 3       |
| hdpi    | 1.5     |
| xxxhdpi | 4       |

- 当使用和设备相同的density的xxhdpi文件夹时，图片显示原始大小300x300.
- 当使用hdpi的图片时，因为该图片在低密度下，**系统会认它不够大，会自动帮我们放大**， 300 * 3 / 1.5 ==> 600x600
- 当使用xxxhdpi时，300 * 3 / 4 ==> 225x225

关于内存的使用，android加载图片到ImageView时，具体使用到多少内存我不太清楚（从上面的内存使用截图中很难看的出来）。

但是关于bitmap使用到多少内存倒是非常容易计算的。
 默认情况下，android使用argb的方式加载图片资源，也就是一个像素点占用4个字节。而300x300分辨率的图片就占用了360000个字节，也就是350多K的内存。

同样一张图片，我们放到不同目录下导致出现不同的内存使用结果。如果放到hdpi中，那么bitmap使用的内存多了4倍，整整1.3M的内存！

**那么，只需要一份切图放到mipmap-xhdpi中会出现什么问题呢？**

关于这点，其实郭霖大神说的不对`“图片资源应该尽量放在高密度文件夹下，这样可以节省图片的内存开支”`
 事实上，内存的使用基本是一样的，因为不同drawable会放置不同分辨率的图片。
 例如你在drawable-xhdpi放置30x30的icon，在drawable-xxhdpi放置45x45的icon。当一个xxhdpi的设备去加载这张图片时，会自动选择45x45的图片，占用8k左右的内存。
 如果你只在drawable-xhdpi放置30x30的icon，而不在drawable-xxhdpi中放置任何icon，那么系统会自动将30x30的图片放大到45x45，也是占用8k左右的内存。

所以，同事跟我说只需要一份切图放到mipmap-xhdpi在内存的使用上是没有什么大问题的，确实能有效的控制apk包的大小。

小问题则是，在高分辨率或者低分辨率的设备中，图片可能因为经过缩放导致模糊或者失真，特别是分辨率比较大的图片，比如引导页和启动页的大图。但是因为选择的是xhdpi是市面上最普及的分辨率，所以也不会存在什么大问题，不过我更加推荐在xxhdpi中多放一份切图，因为大多数旗舰机已经是xxhdpi的分辨率了。

最后，切图放在mipmap-xhdpi和drawable-xhdpi中是没有区别的！
 那些跟我说放在mipmap中比较好，只需要一份，会缩放，会更省内存的网友我敲你lailai



### drawable和mipmap目录的结论

1. **在App中，无论你将图片放在drawable还是mipmap目录，系统只会加载对应density中的图片。 而在Launcher中，如果使用mipmap，那么Launcher会自动加载更加合适的密度的资源。**
2. **应用内使用到的图片资源，并不会因为你放在mipmap或者drawable目录而产生差异。单纯只是资源路径的差异R.drawable.xxx或者R.mipmap.xxx。（也可能在低版本系统中有差异）**
3. **一句话来说就是，自动跨设备密度展示的能力是launcher的，而不是mipmap的。**

总的来说，app图标（launcher icon) 必须放在mipmap目录中，并且最好准备不同密度的图片，否则缩放后可能导致失真。

而应用内使用到的图片资源，放在drawable目录亦或是mipmap目录中是没有区别的，该准备多个密度的还是要准备多个密度，如果只想使用一份切图，那尽量将切图放在高密度的文件夹中。



![drwable缩放比例](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/1ODDkjjGynlPUkW*g3EtgUONoEvFIz4CZMotcfrW6Ts!/r/dDQBAAAAAAAA)





[这位大哥写的很好](https://www.jianshu.com/p/f7dc272b3469)