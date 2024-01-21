[TOC]

我们要知道，在Android手机里面，缓存的位置分为两类，一类是Internal Storage，即内部存储，另外一类是External Storage，即外部存储。比较老的手机，有一个手机内部存储，还有一个SD卡存储，就是分别对应这两种存储位置，因为以前的SD卡是可以扩展的，即可拆卸的，所以可以用是否可拆卸作为内外存储的分类标准。但是现在最新的设备，比如小米、锤子、华为等，都取消了可拆卸的SD卡，直接与机身焊接在一起，分为16G、32G版本，所以现在内外存储的分类不再以是否可拆卸作为标准，而是以下面的几方面作为新的标准：



```java
Android 中获取本地储存路径，有四个方法，
getCacheDir()、getFilesDir()、getExternalFilesDir()、getExternalCacheDir()。
接下来介绍下每个方法的特点以及路径地址
```

getCacheDir()：/data/data/你的应用的包名/cache

getFilesDir()：/data/data/你的应用的包名/files

getExternalFilesDir()：SDCard/Android/data/你的应用的包名/files/

getExternalCacheDir()：SDCard/Android/data/你的应用包名/cache/

从上文每个方法获取的路径中可以看出，getCacheDir()和getFilesDir()是获取手机自带的存储空间中的当前包文件的路径 ；

getExternalFilesDir()和getExternalCacheDir()是获取手机中SD卡的存储控件中的当前包文件的路径。



-  **内部存储：**

总是可用的
这里的文件默认是只能被你的app所访问的。
当用户卸载你的app的时候，系统会把internal里面的相关文件都清除干净。
Internal是在你想确保不被用户与其他app所访问的最佳存储区域。



- **外部存储：**

并不总是可用的，因为用户可以选择把这部分作为USB存储模式，这样就不可以访问了。
是大家都可以访问的，因此保存到这里的文件是失去访问控制权限的。
当用户卸载你的app时，系统仅仅会删除external根目录（getExternalFilesDir()）下的相关文件。
External是在你不需要严格的访问权限并且你希望这些文件能够被其他app所共享或者是允许用户通过电脑访问时的最佳存储区域。  



读取内部存储不需要权限，但是读取或者是写入外部存储需要权限，在现版本里面，读权限不进行声明，也可以实现读取，但是在以后版本可能会修改，所以请务必加上，如果应用需要写入权限，那么只声明写入权限即可，不需要再声明读取权限。

   

下面分别说明如何获取内外存储的文件位置和区别。

### 保存到内部存储的方式

#### getFileDir() 

通过此方法可以获取到你的APP内部存储的文件，路径为/data/data/pacgage_name/**files**

```java
File file1 = new File(getFilesDir(), "getFilesDir.txt");
        Log.d("TAG", "file1=" + file1.getAbsolutePath());
 
        try {
            OutputStream outputStream1 = new FileOutputStream(file1);
            outputStream1.write("file".getBytes());
            outputStream1.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
--------------------------------
    02-03 07:18:04.068  22237-22237/? D/TAG﹕ file1=/data/data/com.socks.baidudemo/files/getFilesDir.txt

```



#### getCacheDir()

 通过此方法可以获取到你的APP内部存储的文件，路径为/data/data/package_name/**cache**

```java
File file2 = new File(getCacheDir(), "cache.txt");
        Log.d("TAG", "file2=" + file2.getAbsolutePath());
        try {
            OutputStream outputStream1 = new FileOutputStream(file2);
            outputStream1.write("cache".getBytes());
            outputStream1.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
---------------------
    02-03 07:19:31.508  23652-23652/? D/TAG﹕ file2=/data/data/com.socks.baidudemo/cache/cache.txt
```



#### openFileOutput()

通过此方法，我们可以获取到一个输出流，输出流的保存路径是/data/data/package_name/files ，和getFileDir()的路径一致

```java
try {
            OutputStream outputStream = openFileOutput("openFileOutput.txt", MODE_PRIVATE);
            outputStream.write("openFileOutput".getBytes());
            outputStream.close();
        } catch (Exception e) {
            e.printStackTrace();
        }

```

你的app的internal storage 目录是以你的app的包名作为标识存放在Android文件系统的特定目录下[data/data/com.example.xx]。 从技术上讲，如果你设置文件为可读的，那么其他app就可以读取你的internal文件。然而，其他app需要知道你的包名与文件名。若是你没有设置为可读或者可写，其他app是没有办法读写的。因此只要你使用MODE_PRIVATE ，那么这些文件就不可能被其他app所访问。

另外记住一点**，内部存储在你的APP卸载的时候**，会一块被删除，因此，我们可以在cache目录里面放置我们的图片缓存，而且cache与files的差别在于，如果手机的内部存储控件不够了，会自行选择cache目录进行删除，因此，不要把重要的文件放在cache文件里面，可以放置在files里面，因为这个文件只有在APP被卸载的时候才会被删除。还有要注意的一点是，如果应用程序是更新操作，内部存储不会被删除，区别于被用户手动卸载。



### 外部存储的方式

> mnt/shell/emulated/0/Android/data/《包名》/
>
> storage/emulated/0/Android/data/《包名》/

#### 外部存储状态

 与内部存储不同，外部存储的容量一般较大，而且当移动设备连接到PC之后，如果我们开启USB模式与PC连接并操作文件，这个时候外部存储是处于卸载状态的，APP不能对里面的文件进行操作，所以，我们的APP的对外部存储进行操作之前，请先检查外部存储的状态。

```java
 /* Checks if external storage is available for read and write */
public boolean isExternalStorageWritable() {
    String state = Environment.getExternalStorageState();
    if (Environment.MEDIA_MOUNTED.equals(state)) {
        return true;
    }
    return false;
}
 
/* Checks if external storage is available to at least read */
public boolean isExternalStorageReadable() {
    String state = Environment.getExternalStorageState();
    if (Environment.MEDIA_MOUNTED.equals(state) ||
        Environment.MEDIA_MOUNTED_READ_ONLY.equals(state)) {
        return true;
    }
    return false;
}
```



#### 外部私有存储

> 因为Android文件夹是隐藏文件夹，用户无法操作。

##### getExternalCacheDir()和getExternalFilesDir()

```java
File file3 = new File(getExternalCacheDir().getAbsolutePath(), "getExternalCacheDir.txt");
        try {
            OutputStream outputStream1 = new FileOutputStream(file3);
            outputStream1.write("getExternalCacheDir".getBytes());
            outputStream1.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
 
        Log.d("TAG", "file3=" + file3);
 
        File file4 = new File(getExternalFilesDir(Environment.DIRECTORY_PICTURES), "getExternalFilesDir.txt");
        try {
            OutputStream outputStream1 = new FileOutputStream(file4);
            outputStream1.write("getExternalFilesDir".getBytes());
            outputStream1.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
		Log.d("TAG", "file4=" + file4);
--------------------------------------------------
    02-03 08:11:38.860    9096-9096/? D/TAG﹕ file3=/storage/emulated/0/Android/data/com.socks.baidudemo/cache/getExternalCacheDir.txt
02-03 08:11:38.860    9096-9096/? D/TAG﹕ file4=/storage/emulated/0/Android/data/com.socks.baidudemo/files/Pictures/getExternalFilesDir.txt

```

我们创建的私有文件的地址是/sdcard/Android/date/package_name下面，Android文件夹是隐藏文件夹，用户无法操作。

如果我们想缓存图片等比较耗空间的文件，推荐放在getExternalCacheDir()所在的文件下面，这个文件和getCacheDir()很像，都可以放缓存文件，在APP被卸载的时候，都会被系统删除，而且缓存的内容对其他APP是相对私有的。

Context.getExternalFilesDir()和Context.getFilesDir()也是有区别的，但是在应用卸载的时候，也是会被删除的。



### 外部公共存储

#### Environment.getXXX

如果你的APP产生的文件不需要隐藏，即对用户是可见的，那么你可以把文件放在外部的公共存储文件下面。

我们可以通过下面的代码获取到公共存储目录

```java
Environment.getExternalStorageDirectory()//外部存储的根目录
    
Environment.getExternalStoragePublicDirectory()//外部存储的公共目录
```

这个方法不是Context的方法，而是**Environment**的两个方法，第一个方法获取到的其实是外部存储的根目录，而第二个方法获取到得则是外部存储的公共目录。其实在访问权限上是没有区别的，不同点是getExternalStoragePublicDirectory()在运行的时候，会需要你带有一个特定的参数来指定这些public的文件类型，以便于与其他public文件进行分类。参数类型包括DIRECTORY_MUSIC 或者 DIRECTORY_PICTURES. 如下:

```java
public File getAlbumStorageDir(Context context, String albumName) {
    // Get the directory for the app's private pictures directory.
    File file = new File(context.getExternalFilesDir(
            Environment.DIRECTORY_PICTURES), albumName);
    if (!file.mkdirs()) {
        Log.e(LOG_TAG, "Directory not created");
    }
    return file;
}
```

不管你是使用 getExternalStoragePublicDirectory() 来存储可以共享的文件，还是使用 getExternalFilesDir() 来储存那些对与你的app来说是私有的文件，有一点很重要，那就是你要使用那些类似DIRECTORY_PICTURES 的API的常量。那些目录类型参数可以确保那些文件被系统正确的对待。例如，那些以DIRECTORY_RINGTONES 类型保存的文件就会被系统的media scanner认为是ringtone而不是音乐。



### 实例代码

```java
package cn.ck.macbookpro;

import android.os.Bundle;
import android.os.Environment;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import java.io.File;

public class MainActivity extends AppCompatActivity {
    Button getDirBtn;
    TextView resultTv;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        getDirBtn = findViewById(R.id.btn_getDir);
        resultTv = findViewById(R.id.tv_result);
        getDirBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                getDirs();
            }
        });
    }

    private void getDirs() {
        File file1 = new File(getFilesDir(), "getFilesDir.txt");
        Log.i("ck", "file1=" + file1.getAbsolutePath());

        File file2 = new File(getCacheDir(), "cache.txt");
        Log.i("ck", "file=2" + file2.getAbsolutePath());

        File file3 = new File(getExternalCacheDir(), "getExternalCacheDir.txt");
        Log.i("ck", "file=3" + file3.getAbsolutePath());

        File file4 = new File(getExternalFilesDir(Environment.DIRECTORY_MOVIES),
                "getExternalFilesDir.txt");
        Log.i("ck", "file=4" + file4);


    }

    /* Checks if external storage is available for read and write */
    public boolean isExternalStorageWritable() {
        String state = Environment.getExternalStorageState();
        if (Environment.MEDIA_MOUNTED.equals(state)) {
            return true;
        }
        return false;
    }

    /* Checks if external storage is available to at least read */
    public boolean isExternalStorageReadable() {
        String state = Environment.getExternalStorageState();
        if (Environment.MEDIA_MOUNTED.equals(state) ||
                Environment.MEDIA_MOUNTED_READ_ONLY.equals(state)) {
            return true;
        }
        return false;
    }

}

```



```java
I/ck: file1=/data/user/0/cn.ck.macbookpro/files/getFilesDir.txt
I/ck: file=2/data/user/0/cn.ck.macbookpro/cache/cache.txt
I/ck: file=3/storage/emulated/0/Android/data/cn.ck.macbookpro/cache/getExternalCacheDir.txt
I/ck: file=4/storage/emulated/0/Android/data/cn.ck.macbookpro/files/Movies/getExternalFilesDir.txt

mnt/sdcard/user/0/primary/Movies/XXXXX
```





# 1.调用Context对象的方法： 

getCacheDir()方法用于获取/data/data/app包名/cache目录；

getFilesDir()方法用于获取/data/data/app包名/files目录；

getExternalCacheDir()方法可以获取到 SDCard/Android/data/你的应用包名/cache/目录，一般存放临时缓存数据；

getExternalFilesDir()方法可以获取到 SDCard/Android/data/你的应用的包名/files/ 目录，一般放一些长时间保存的数据；

如果使用上面的方法，当应用被用户卸载后，目录下的所有文件都会被删除。

# 2.调用Environment的方法

Environment.getExternalStorageDirectory(),获取sd卡根目录，跟应用的是否卸载无关。



```java
public class AdGestureImpl implements IAdGestureService {
    private static final String TAG = "Ad-Gesture-Service";

    private static final String TEMPLATE_EXT = ".t";
    private static final String ANIM_EXT = ".m";

    private static final String TEMPLATE_DIR = "ad_template";
    private static final String SP_FILE_NAME = "hotsoon_ad_gesture";
    private static final String PREFS_GESTURE = "gesture";

    private Context context;
    public AdGestureImpl(Context context) {
        this.context = context;
    }

    @Override
    public void init() {
        Disposable disposable = Observable.just(0)
                .observeOn(Schedulers.io())
                .subscribe(__ -> {
                    SSAdGesture[] remote = AdSettingKeys.FEED_AD_GESTURE.getValue();
                    List<SSAdGesture> gestures = merge(remote);
                    if (gestures != null && !gestures.isEmpty()) {
                        updateLocal(gestures);
                        for (SSAdGesture gesture : gestures) {
                            update(gesture);
                        }
                    }
                }, t -> {
                    t.printStackTrace();
                    ALogger.w(TAG, "Init ad gesture service failed", t);
                });
    }

    @Nullable
    @Override
    public String getGestureTemplate(String name) {
        File dir = getTemplateDir();
        if (dir != null && dir.exists() && dir.isDirectory()) {
            return new File(dir, (name + TEMPLATE_EXT)).getAbsolutePath();
        }

        return null;
    }

    @Nullable
    @Override
    public String getGestureGuideAnim(String name) {
        File dir = getTemplateDir();
        if (dir != null && dir.exists() && dir.isDirectory()) {
            return new File(dir, (name + ANIM_EXT)).getAbsolutePath();
        }
        return null;
    }

    /**
     * 合并remote 和 local 模板信息，name 一样时，以 remote 为准，同时需要检查 local 文件的完整性
     *
     * @param remote
     * @return
     */
    @Nullable
    private synchronized List<SSAdGesture> merge(SSAdGesture[] remote) {
        Map<String, SSAdGesture> gestures = new HashMap<>();
        if (remote != null && remote.length > 0) {
            for (SSAdGesture gesture : remote) {
                if (gesture != null && !TextUtils.isEmpty(gesture.getName())) {
                    gestures.put(gesture.getName(), gesture);
                }
            }
        }

        String localData = SharedPrefHelper.from(context, SP_FILE_NAME).getString(PREFS_GESTURE, "");
        if (!TextUtils.isEmpty(localData)) {
            try {
                List<SSAdGesture> local = JsonUtil.parse(localData, new TypeToken<ArrayList<SSAdGesture>>() {
                }.getType());
                if (local != null && !local.isEmpty()) {
                    for (SSAdGesture gesture : local) {
                        if (gesture == null || TextUtils.isEmpty(gesture.getName())) {
                            continue;
                        }

                        if (!gestures.containsKey(gesture.getName())) {
                            gestures.put(gesture.getName(), gesture);
                        }
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        if (gestures != null && !gestures.isEmpty()) {
            return new ArrayList<>(gestures.values());
        }

        return null;
    }

    private void updateLocal(List<SSAdGesture> gestures) {
        if (gestures == null || gestures.isEmpty()) {
            return;
        }

        try {
            String data = JsonUtil.toJSONString(gestures);
            SharedPrefHelper.from(context, SP_FILE_NAME).putEnd(PREFS_GESTURE, data);
        } catch (Throwable t) {
            t.printStackTrace();
            ALogger.w(TAG, "Update local data failed", t);
        }
    }

    /**
     * 更新手势模板文件和引导动画
     *
     * @param gesture
     * @return
     */
    private void update(SSAdGesture gesture) {
        if (gesture == null || TextUtils.isEmpty(gesture.getName())) {
            return;
        }

        if (needUpdate((gesture.getName() + TEMPLATE_EXT), gesture.getTemplateMd5())) {
            update(gesture.getTemplateUrl(), getGestureTemplate(gesture.getName()));
        }

        if (needUpdate((gesture.getName() + ANIM_EXT), gesture.getAnimMd5())) {
            update(gesture.getAnimUrl(), getGestureGuideAnim(gesture.getName()));
        }
    }

    /**
     * 是否需要下载文件
     * 1. 本地文件不存在
     * 2. 本地文件存在，但是 MD5 不一致
     *
     * @param name
     * @param md5
     * @return
     */
    private boolean needUpdate(String name, String md5) {
        if (TextUtils.isEmpty(name) || TextUtils.isEmpty(md5)) {
            return false;
        }

        File templateDir = getTemplateDir();
        if (templateDir == null) {
            // 模板目录创建失败，说明文件系统有问题或者存储满了
            return false;
        }

        File template = new File(templateDir, name);
        if (!template.exists() || !template.isFile()) {
            // 模板文件不存在
            return true;
        }

        String existMd5 = DigestUtils.md5Hex(template);
        if (!TextUtils.equals(existMd5, md5)) {
            // MD5 不一致，删除本地文件，重新下载
            try {
                return template.delete();
            } catch (Throwable t) {
                ALogger.w(TAG, "Remove expired template failed", t);
                return true;
            }
        }

        return false;
    }

    /**
     * 下载文件
     *
     * @param url
     * @param target
     */
    private void update(String url, String target) {
        if (TextUtils.isEmpty(url) || TextUtils.isEmpty(target)) {
            return;
        }

        Disposable disposable = Observable.just(0)
                .observeOn(Schedulers.io())
                .subscribe(__ -> {
                    InputStream stream = NetworkUtils.downloadFile(url);
                    FileUtils.writeStreamToFile(stream, target);
                }, t -> {
                    t.printStackTrace();
                    ALogger.w(TAG, "Save gesture file " + target + " failed from " + url, t);
                });
    }

    @Nullable
    private File getTemplateDir() {
        File templateDir = FileUtils.getExternalCacheTypeDir(TEMPLATE_DIR, context);
        if (templateDir == null) {
            try {
                templateDir = new File(context.getExternalCacheDir(), TEMPLATE_DIR);
                if (!templateDir.exists() || !templateDir.isDirectory()) {
                    boolean make = templateDir.mkdirs();
                    if (make) {
                        return templateDir;
                    }
                }
            } catch (Throwable t) {
                ALogger.w(TAG, "Create template dir failed", t);
            }
        }

        return templateDir;
    }
}
```

