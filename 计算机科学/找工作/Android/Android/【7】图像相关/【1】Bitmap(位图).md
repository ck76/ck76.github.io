Bitmap位图包括像素以及长、宽、颜色等描述信息。长宽和像素位数是用来描述图片的，可以通过这些信息计算出图片的像素占用内存的大小。

位图可以理解为一个画架，把图放到上面然后可以对图片做一些列的处理。

位图文件图像显示效果好，但是非压缩格式，需要占用较大的存储空间。

<!--more-->

## 一、开头

- **Drawable**：通用的图形对象，用于装载常用格式的图像，既可以是PNG，JPG这样的图像， 也是前面学的那13种Drawable类型的可视化对象！我们可以理解成一个用来放画的——**画框**！
- **Bitmap(位图)**：我们可以把他看作一个**画架**，我们先把画放到上面，然后我们可以 进行一些处理，比如获取图像文件信息，做旋转切割，放大缩小等操作！
- **Canvas(画布)**：如其名，**画布**，我们可以在上面作画(绘制)，你既可以用**Paint(画笔)**， 来画各种形状或者写字，又可以用**Path(路径)**来绘制多个点，然后连接成各种图形！
- **Matrix(矩阵)**：用于图形特效处理的，颜色矩阵(ColorMatrix)，还有使用Matrix进行图像的 平移，缩放，旋转，倾斜等！

而上述的这些都是Android中的底层图形类：**android.graphics**给我们提供的接口！



## 二、了解Bitmap，BitmapFactory，BitmapFacotry.Options

Bitmap的构造方法是私有的，外面不能实例化，只能通过JNI实例化！ 当然，肯定也会给我们提供一个接口给我们来创建Bitmap的，而这个接口类就是：**BitmapFactory**！打开BitmapFactory类，我们点下左边的Structure可以看到BitmapFactory给我们 提供了这些方法，大部分都是decodeXxx，通过各种形式来创建Bitmap的！ 

![Bitmap1+](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/pKi7GHJUSulHpxnW02VC66CqtdS2m0jvEjktB3QBD9Q!/r/dFIBAAAAAAAA)

接着我们又发现了，每一种方法，都会有一个Options类型的参数，点进去看看： 于是乎我们发现了这货是一个**静态内部类**:**BitmapFacotry.Options**! 而他是用来设置decode时的选项的！ 

![Bitmap2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/maMIri78TbxIbEgdbKDOXKGhnZbGG0htTWFrAFy2F8Y!/r/dFMBAAAAAAAA)

我们对这里的某些参数的值进行设置，比如inJustDecodeBounds设置为true避免OOM(内存溢出) 



## 三、Bitmap常用方法

### 1.普通

- public boolean **compress** (Bitmap.CompressFormat **format**, int **quality**, OutputStream **stream**) 将位图的压缩到指定的OutputStream，可以理解成将Bitmap保存到文件中！ 
  - **format**：格式，PNG，JPG等； 
  - **quality**：压缩质量，0-100，0表示最低画质压缩，100最大质量(PNG无损，会忽略品质设定) 
  - **stream**：输出流 返回值代表是否成功压缩到指定流！
- void **recycle**()：回收位图占用的内存空间，把位图标记为Dead
- boolean **isRecycled**()：判断位图内存是否已释放
- int **getWidth**()：获取位图的宽度
- int **getHeight**()：获取位图的高度
- boolean **isMutable**()：图片是否可修改
- int **getScaledWidth**(Canvas canvas)：获取指定密度转换后的图像的宽度
- int **getScaledHeight**(Canvas canvas)：获取指定密度转换后的图像的高度

### 2.静态方法

- Bitmap **createBitmap**(Bitmap src)：以src为原图生成不可变得新图像
- Bitmap **createScaledBitmap**(Bitmap src, int dstWidth,int dstHeight, boolean filter)：以src为原图，创建新的图像，指定新图像的高宽以及是否变。
- Bitmap **createBitmap**(int width, int height, Config config)：创建指定格式、大小的位图
- Bitmap **createBitmap**(Bitmap source, int x, int y, int width, int height)以source为原图，创建新的图片，指定起始坐标以及新图像的高宽。
- public static Bitmap **createBitmap**(Bitmap source, int x, int y, int width, int height, Matrix m, boolean filter)

### 3.**BitmapFactory.Option**可设置参数

- boolean **inJustDecodeBounds**——如果设置为true，不获取图片，不分配内存，但会返回图片的高宽度信息。
- int **outWidth**——获取图片的宽度值
- int **outHeight**——获取图片的高度值
- int **inDensity**——用于位图的像素压缩比
- int **inTargetDensity**——用于目标位图的像素压缩比（要生成的位图）
- boolean **inScaled**——设置为true时进行图片压缩，从inDensity到inTargetDensity。
- public Bitmap.Config **inPreferredConfig** = Bitmap.Config.ARGB_8888;**(与后文OOM处理有关)**
- int **inSampleSize**——图片缩放的倍数。如果设为4，则宽和高都为原来的1/4，则图是原来的1/16。**(与后文OOM处理有关)**



## 四、获取Bitmap位图

从资源中获取位图的方式有两种：通过**BitmapDrawable**或者**BitmapFactory** 

### 1.BitmapDrawable方法

你可以创建一个构造一个BitmapDrawable对象，比如通过流构建BitmapDrawable：

```java
BitmapDrawable bmpMeizi = new BitmapDrawable(getAssets().open("pic_meizi.jpg"));
Bitmap mBitmap = bmpMeizi.getBitmap();
img_bg.setImageBitmap(mBitmap);
```

### 2.BitmapFactory

```java
//通过资源ID
//常用
Bitmap bitmap = BitmapFactory.decodeResource(getContext().getResources(), R.drawable.sample); // 间接调用 
private Bitmap getBitmapFromResource(Resources res, int resId) {
      return BitmapFactory.decodeResource(res, resId);
}

//文件
private Bitmap getBitmapFromFile(String pathName) {
      return BitmapFactory.decodeFile(pathName);
}

//字节数组
public Bitmap Bytes2Bimap(byte[] b) {
    if (b.length != 0) {
        return BitmapFactory.decodeByteArray(b, 0, b.length);
    } else {
        return null;
    }
}

//输入流
private Bitmap getBitmapFromStream(InputStream inputStream) {
      return BitmapFactory.decodeStream(inputStream);
}
```

### 3.基本加载总结

Bitmap的加载离不开BitmapFactory类，关于Bitmap官方介绍`Creates Bitmap objects from various sources, including files, streams, and byte-arrays.`查看api，发现和描述的一样，BitmapFactory类提供了四类方法用来加载Bitmap：

1. **decodeFile** 从文件系统加载
   a. 通过Intent打开本地图片或照片
   b. 在onActivityResult中获取图片uri
   c. 根据uri获取图片的路径
   d. 根据路径解析bitmap:`Bitmap bm = BitmapFactory.decodeFile(sd_path)` 
2. **decodeResource** 以R.drawable.xxx的形式从本地资源中加载
   `Bitmap bm = BitmapFactory.decodeResource(getResources(), R.drawable.aaa);` 
3. **decodeStream** 从输入流加载
   a.开启异步线程去获取网络图片
   b.网络返回InputStream
   c.解析：`Bitmap bm = BitmapFactory.decodeStream(stream)`,这是一个耗时操作，要在子线程中执行
4. **decodeByteArray** 从字节数组中加载
   接3.a,3.b,
   c. 把InputStream转换成byte[]
   d. 解析：`Bitmap bm = BitmapFactory.decodeByteArray(myByte,0,myByte.length);` 

注意：decodeFile和decodeResource间接调用decodeStream方法。  

BitmapFactory对File的decode都会转换为InputStream采用nativeDecodeStream来decode，对Resource的decode会采用decodeAsset，而如果FileDesciptor可以转换为native的fd,会通过nativeDecodeFileDescriptor来decode，另外ByteArray会直接采用nativeDecodeByteArray来decode。**需要注意的是，对Resource的decode，BitmapFactory会设置Option的相关参数，最终进行相应的缩放，图片的大小会跟原图有所区别。** 具体的内容建议去看看BitmapFactory，了解每种方式的区别，才能够更好地使用接口，选择的时候采用更有效率的方法 。



## 五、例子

### 1.扣图片上的某一角

直接通过Bitmap的createBitmap()扣下来即可 参数依次为：处理的bitmap对象，起始x,y坐标，以及截取的宽高 

```java
Bitmap bitmap1 = BitmapFactory.decodeResource(getResources(), R.mipmap.pic_ck);
Bitmap bitmap2 = Bitmap.createBitmap(bitmap1,100,100,200,200);
img_bg = (ImageView) findViewById(R.id.img_bg);
img_bg.setImageBitmap(bitmap2);
//图片的左上角会被扣下来
```

### 2.缩放发图片

不光用Matrix可以对Bitmap缩放，直接使用Bitmap给我们提供的**createScaledBitmap**也可以实现实现， 参数依次是：处理的bitmap对象，缩放后的宽高， 



## 六、位图色彩介绍

在讨论如何解决Bitmap带来的OOM问题前先了解下Bitmap的色彩、、

android中的大图片一般都要经过压缩才显示，不然容易发生oom，一般我们压缩的时候都只关注其尺寸方面的大小，其实除了尺寸之外，影响一个图片占用空间的还有其色彩细节。

打开Android.graphics.Bitmap类里有一个内部类Bitmap.Config类，在Bitmap类里createBitmap(intwidth, int height, Bitmap.Config config)方法里会用到，打开个这个类一看

枚举变量

- public static final Bitmap.Config ALPHA_8
- public static final Bitmap.Config ARGB_4444
- public static final Bitmap.Config ARGB_8888
- public static final Bitmap.Config RGB_565

**其中，A代表透明度(Alpha)；R代表红色；G代表绿色；B代表蓝色。**

- ARGB_8888：四个**通道**都是8位，每个像素占用**4**个字节，图片质量是最高的，但是占用的内存也是最大的；
- ARGB_4444：四个通道都是4位，每个像素占用**2**个字节，图片的失真比较严重；

- RGB_565：没有A通道，每个像素占用**2**个字节，图片失真小，但是没有透明度；

- ALPHA_8：只有A通道，每个像素占用**1**个字节大大小，只有透明度，没有颜色值。

> 使用场景总结：ARGB_4444失真严重，基本不用；ALPHA_8使用场景特殊，比如设置遮盖效果等；不需要设置透明度，RGB_565是个不错的选择；既要设置透明度，对图片质量要求又高，就用ARGB_8888。

**位图位数越高代表其可以存储的颜色信息越多，当然图像也就越逼真。**

 CompressFormat：Bitmap.CompressFormat.JPEG、Bitmap.CompressFormat.PNG、Bitmap.CompressFormat.WEBP三种压缩格式

JPEG：一种有损压缩（JPEG2000既可以有损也可以无损），".jpg"或者".jpeg"; 优点：采用了直接色，有丰富的色彩，适合存储照片和生动图像效果；缺点：有损，不适合用来存储logo、线框类图。

PNG: 一种无损压缩，".png"; 优点：支持透明、无损，主要用于小图标，透明背景等；缺点：若色彩复杂，则图片生成后文件很大；

WEBP:以WebP算法进行压缩；Google开发的新的图片格式，同时支持无损和有损压缩，使用直接色。无损压缩，相同质量的webp比PNG小大约26%；有损压缩，相同质量的webp比JPEG小25%-34% 支持动图，基本取代gif



## 八、Bitmap引起的OOM

**Out Of Memory**(内存溢出)，我们都知道Android系统会为每个APP分配一个独立的工作空间， 或者说分配一个单独的Dalvik虚拟机，这样每个APP都可以独立运行而不相互影响！而Android对于每个 Dalvik虚拟机都会有一个最大内存限制，如果当前占用的内存加上我们申请的内存资源超过了这个限制 ，系统就会抛出OOM错误！另外，这里别和RAM混淆了，即时当前RAM中剩余的内存有1G多，但是OOM还是会发生！别把RAM(物理内存)和OOM扯到一起！另外RAM不足的话，就是杀应用了，而不是仅仅是OOM了！ 而这个Dalvik中的最大内存标准，不同的机型是不一样的，可以调用： 

```java
ActivityManager activityManager = (ActivityManager)context.getSystemService(Context.ACTIVITY_SERVICE);
Log.e("HEHE","最大内存：" + activityManager.getMemoryClass());
```

获得正常的最大内存标准，又或者直接在命令行键入：

```shell
adb shell getprop | grep dalvik.vm.heapgrowthlimit
```

你也可以打开系统源码/system/build.prop文件，看下文件中这一部分的信息得出：

```mariadb
dalvik.vm.heapstartsize=8m
dalvik.vm.heapgrowthlimit=192m
dalvik.vm.heapsize=512m
dalvik.vm.heaptargetutilization=0.75
dalvik.vm.heapminfree=2m
dalvik.vm.heapmaxfree=8m
```

我们关注的地方有三个：heapstartsize堆内存的初始大小，heapgrowthlimit标准的应用的最大堆 内存大小，heapsize则是设置了使用android:largeHeap的应用的最大堆内存大小！

```java
//真机Nexus5  192m
//真机NexusS  48m
```

### 1.采用低内存占用的编码方式

上一节说了**BitmapFactory.Options**这个类，我们可以设置下其中的**inPreferredConfig**属性， 默认是**Bitmap.Config.ARGB_8888**，我们可以修改成**Bitmap.Config.ARGB_4444** Bitmap.Config ARGB_4444：每个像素占四位，即A=4，R=4，G=4，B=4，那么一个像素点占4+4+4+4=16位 Bitmap.Config ARGB_8888：每个像素占八位，即A=8，R=8，G=8，B=8，那么一个像素点占8+8+8+8=32位 默认使用ARGB_8888，即一个像素占4个字节！ 

### 2.图片压缩

同样是BitmapFactory.Options，我们通过**inSampleSize**设置缩放倍数，比如写2，即长宽变为原来的1/2，图片就是原来的1/4，如果不进行缩放的话设置为1即可！但是不能一味的压缩，毕竟这个值太小 的话，图片会很模糊，而且要避免图片的拉伸变形，所以需要我们在程序中动态的计算，这个 inSampleSize的合适值，而Options中又有这样一个方法：**inJustDecodeBounds**，将该参数设置为 true后，decodeFiel并不会分配内存空间，但是可以计算出原始图片的长宽，调用 options.**outWidth**/**outHeight**获取出图片的宽高，然后通过一定的算法，即可得到适合的 inSampleSize 

```java
public static int caculateInSampleSize(BitmapFactory.Options options, int reqWidth, int reqHeight) {
    int width = options.outWidth;
    int height = options.outHeight;
    int inSampleSize = 1;
    if (width > reqWidth || height > reqHeight) {
        int widthRadio = Math.round(width * 1.0f / reqWidth);
        int heightRadio = Math.round(height * 1.0f / reqHeight);
        inSampleSize = Math.max(widthRadio, heightRadio);
    }
    return inSampleSize;
}
```

然后使用下上述的方法即可：

```java
BitmapFactory.Options options = new BitmapFactory.Options();
options.inJustDecodeBounds = true; // 设置了此属性一定要记得将值设置为false
Bitmap bitmap = null;
bitmap = BitmapFactory.decodeFile(url, options);
options.inSampleSize = computeSampleSize(options,128,128);
options.inPreferredConfig = Bitmap.Config.ARGB_4444;
/* 下面两个字段需要组合使用 */  
options.inPurgeable = true;
options.inInputShareable = true;
options.inJustDecodeBounds = false;
try {
    bitmap = BitmapFactory.decodeFile(url, options);
} catch (OutOfMemoryError e) {
        Log.e(TAG, "OutOfMemoryError");
}
```

### 3.及时回收图像

如果引用了大量的Bitmap对象，而应用又不需要同时显示所有图片。可以将暂时不用到的Bitmap对象 及时回收掉。对于一些明确知道图片使用情况的场景可以主动recycle回收，比如引导页的图片，使用 完就recycle，帧动画，加载一张，画一张，释放一张！使用时加载，不显示时直接置null或recycle！ 比如：imageView.setImageResource(0); 不过某些情况下会出现特定图片反复加载，释放，再加载等，低效率的事情... 

### 4.LruCache + sd的缓存方式

> Android 3.1版本起，官方还提供了LruCache来进行cache处理，当存储Image的大小大于LruCache 设定的值，那么近期使用次数最少的图片就会被回收掉，系统会自动释放内存！ 
>
> 包含对有限数量值的强引用的缓存。 每次访问一个值时，它都会移动到队列的头部。 将值添加到完整缓存时，该队列末尾的值将被逐出，并且可能符合垃圾回收的条件。------官网介绍

- 要先设置缓存图片的内存大小，我这里设置为手机内存的1/8, 手机内存的获取方式：int MAXMEMONRY = (int) (Runtime.getRuntime() .maxMemory() / 1024);
- LruCache里面的键值对分别是**URL**和对应的**图片**
- 重写了一个叫做sizeOf的方法，返回的是图片数量。

```java
		//获取到应用的最大内存
        int maxMemory = (int) (Runtime.getRuntime().maxMemory() / 1024);
        //设置LruCache的缓存大小
        int cacheSize = maxMemory / 8;
private LruCache<String, Bitmap> mMemoryCache;
private LruCacheUtils() {
    if (mMemoryCache == null)
        mMemoryCache = new LruCache<String, Bitmap>(
                MAXMEMONRY / 8) {
            @Override
            protected int sizeOf(String key, Bitmap bitmap) {
                // 重写此方法来衡量每张图片的大小，默认返回图片数量。
                return bitmap.getRowBytes() * bitmap.getHeight() / 1024;
            }

            @Override
            protected void entryRemoved(boolean evicted, String key,
                    Bitmap oldValue, Bitmap newValue) {
                Log.v("tag", "hard cache is full , push to soft cache");
               
            }
        };
}
```

面的方法分别是清空缓存、添加图片到缓存、从缓存中取得图片、从缓存中移除。

移除和清除缓存是必须要做的事，因为图片缓存处理不当就会报内存溢出，所以一定要引起注意。

```java
public void clearCache() {
    if (mMemoryCache != null) {
        if (mMemoryCache.size() > 0) {
            Log.d("CacheUtils",
                    "mMemoryCache.size() " + mMemoryCache.size());
            mMemoryCache.evictAll();
            Log.d("CacheUtils", "mMemoryCache.size()" + mMemoryCache.size());
        }
        mMemoryCache = null;
    }
}

public synchronized void addBitmapToMemoryCache(String key, Bitmap bitmap) {
    if (mMemoryCache.get(key) == null) {
        if (key != null && bitmap != null)
            mMemoryCache.put(key, bitmap);
    } else
        Log.w(TAG, "the res is aready exits");
}

public synchronized Bitmap getBitmapFromMemCache(String key) {
    Bitmap bm = mMemoryCache.get(key);
    if (key != null) {
        return bm;
    }
    return null;
}

/**
 * 移除缓存
 * 
 * @param key
 */
public synchronized void removeImageCache(String key) {
    if (key != null) {
        if (mMemoryCache != null) {
            Bitmap bm = mMemoryCache.remove(key);
            if (bm != null)
                bm.recycle();
        }
    }
}
```

[一个源码解析](https://github.com/LittleFriendsGroup/AndroidSdkSourceAnalysis/blob/master/article/LruCache%E6%BA%90%E7%A0%81%E8%A7%A3%E6%9E%90.md)



## 九、BitmapUtil

```java
bitmap2Bytes, bytes2Bitmap              : bitmap 与 bytes 互转
drawable2Bitmap, bitmap2Drawable        : drawable 与 bitmap 互转
drawable2Bytes, bytes2Drawable          : drawable 与 bytes 互转
view2Bitmap                             : view 转 Bitmap
```



```java
 /**
     * Bitmap to bytes.
     *
     * @param bitmap The bitmap.
     * @param format The format of bitmap.
     * @return bytes
     */
    public static byte[] bitmap2Bytes(final Bitmap bitmap, final Bitmap.CompressFormat format) {
        if (bitmap == null) return null;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        bitmap.compress(format, 100, baos);
        return baos.toByteArray();
    }

    /**
     * Bytes to bitmap.
     *
     * @param bytes The bytes.
     * @return bitmap
     */
    public static Bitmap bytes2Bitmap(final byte[] bytes) {
        return (bytes == null || bytes.length == 0)
                ? null
                : BitmapFactory.decodeByteArray(bytes, 0, bytes.length);
    }

    /**
     * Drawable to bitmap.
     *
     * @param drawable The drawable.
     * @return bitmap
     */
    public static Bitmap drawable2Bitmap(final Drawable drawable) {
        if (drawable instanceof BitmapDrawable) {
            BitmapDrawable bitmapDrawable = (BitmapDrawable) drawable;
            if (bitmapDrawable.getBitmap() != null) {
                return bitmapDrawable.getBitmap();
            }
        }
        Bitmap bitmap;
        if (drawable.getIntrinsicWidth() <= 0 || drawable.getIntrinsicHeight() <= 0) {
            bitmap = Bitmap.createBitmap(1, 1,
                    drawable.getOpacity() != PixelFormat.OPAQUE
                            ? Bitmap.Config.ARGB_8888
                            : Bitmap.Config.RGB_565);
        } else {
            bitmap = Bitmap.createBitmap(drawable.getIntrinsicWidth(),
                    drawable.getIntrinsicHeight(),
                    drawable.getOpacity() != PixelFormat.OPAQUE
                            ? Bitmap.Config.ARGB_8888
                            : Bitmap.Config.RGB_565);
        }
        Canvas canvas = new Canvas(bitmap);
        drawable.setBounds(0, 0, canvas.getWidth(), canvas.getHeight());
        drawable.draw(canvas);
        return bitmap;
    }

    /**
     * Bitmap to drawable.
     *
     * @param bitmap The bitmap.
     * @return drawable
     */
    public static Drawable bitmap2Drawable(final Bitmap bitmap) {
        return bitmap == null ? null : new BitmapDrawable(Utils.getApp().getResources(), bitmap);
    }

    /**
     * Drawable to bytes.
     *
     * @param drawable The drawable.
     * @param format   The format of bitmap.
     * @return bytes
     */
    public static byte[] drawable2Bytes(final Drawable drawable,
                                        final Bitmap.CompressFormat format) {
        return drawable == null ? null : bitmap2Bytes(drawable2Bitmap(drawable), format);
    }

    /**
     * Bytes to drawable.
     *
     * @param bytes The bytes.
     * @return drawable
     */
    public static Drawable bytes2Drawable(final byte[] bytes) {
        return bytes == null ? null : bitmap2Drawable(bytes2Bitmap(bytes));
    }

    /**
     * View to bitmap.
     *
     * @param view The view.
     * @return bitmap
     */
    public static Bitmap view2Bitmap(final View view) {
        if (view == null) return null;
        Bitmap ret =
                Bitmap.createBitmap(view.getWidth(), view.getHeight(), Bitmap.Config.ARGB_8888);
        Canvas canvas = new Canvas(ret);
        Drawable bgDrawable = view.getBackground();
        if (bgDrawable != null) {
            bgDrawable.draw(canvas);
        } else {
            canvas.drawColor(Color.WHITE);
        }
        view.draw(canvas);
        return ret;
    }
```

