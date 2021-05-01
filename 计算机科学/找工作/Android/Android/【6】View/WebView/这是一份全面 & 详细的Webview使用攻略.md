# 前言

- 现在很多`App`里都内置了Web网页（`Hybrid App`），比如说很多电商平台，淘宝、京东、聚划算等等，如下图

  ![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfi3y737j30890em7aj.jpg)

  京东首页

  

- 那么这种该如何实现呢？其实这是`Android`里一个叫`WebView`组件实现

- 今天，我将献上一份全面介绍 `WebView`的常见用法。

------

# 目录

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfi1ze7oj30xc0qeacb.jpg)

示意图

------

# 1. 简介

`WebView`是一个基于`webkit`引擎、展现`web`页面的控件。

> Android的Webview在低版本和高版本采用了不同的webkit版本内核，4.4后直接使用了Chrome。

------

# 2. 作用

- 显示和渲染Web页面
- 直接使用html文件（网络上或本地assets中）作布局
- 可和JavaScript交互调用

> WebView控件功能强大，除了具有一般View的属性和设置外，还可以对url请求、页面加载、渲染、页面交互进行强大的处理。

------

# 3. 使用介绍

一般来说Webview可单独使用，可联合其工具类一起使用，所以接下来，我会介绍：

- Webview类自身的常见方法
- Webview的最常用的工具类：WebSettings类、WebViewClient类、WebChromeClient类
- Android 和 Js的交互

## 3.1 Webview类常用方法

#### 3.1.1 加载url

加载方式根据资源分为三种



```dart
  //方式1. 加载一个网页：
  webView.loadUrl("http://www.google.com/");

  //方式2：加载apk包中的html页面
  webView.loadUrl("file:///android_asset/test.html");

  //方式3：加载手机本地的html页面
   webView.loadUrl("content://com.android.htmlfileprovider/sdcard/test.html");

   // 方式4： 加载 HTML 页面的一小段内容
  WebView.loadData(String data, String mimeType, String encoding)
// 参数说明：
// 参数1：需要截取展示的内容
// 内容里不能出现 ’#’, ‘%’, ‘\’ , ‘?’ 这四个字符，若出现了需用 %23, %25, %27, %3f 对应来替代，否则会出现异常
// 参数2：展示内容的类型
// 参数3：字节码
```

#### 3.1.1 WebView的状态



```cpp
//激活WebView为活跃状态，能正常执行网页的响应
webView.onResume() ；

//当页面被失去焦点被切换到后台不可见状态，需要执行onPause
//通过onPause动作通知内核暂停所有的动作，比如DOM的解析、plugin的执行、JavaScript执行。
webView.onPause()；

//当应用程序(存在webview)被切换到后台时，这个方法不仅仅针对当前的webview而是全局的全应用程序的webview
//它会暂停所有webview的layout，parsing，javascripttimer。降低CPU功耗。
webView.pauseTimers()
//恢复pauseTimers状态
webView.resumeTimers()；

//销毁Webview
//在关闭了Activity时，如果Webview的音乐或视频，还在播放。就必须销毁Webview
//但是注意：webview调用destory时,webview仍绑定在Activity上
//这是由于自定义webview构建时传入了该Activity的context对象
//因此需要先从父容器中移除webview,然后再销毁webview:
rootLayout.removeView(webView); 
webView.destroy();
```

#### 3.1.2 关于前进 / 后退网页



```cpp
//是否可以后退
Webview.canGoBack() 
//后退网页
Webview.goBack()

//是否可以前进                     
Webview.canGoForward()
//前进网页
Webview.goForward()

//以当前的index为起始点前进或者后退到历史记录中指定的steps
//如果steps为负数则为后退，正数则为前进
Webview.goBackOrForward(intsteps) 
```

**常见用法：Back键控制网页后退**

- 问题：在不做任何处理前提下 ，浏览网页时点击系统的“Back”键,整个 Browser 会调用 finish()而结束自身
- 目标：点击返回后，是网页回退而不是推出浏览器
- 解决方案：在当前Activity中处理并消费掉该 Back 事件



```csharp
public boolean onKeyDown(int keyCode, KeyEvent event) {
    if ((keyCode == KEYCODE_BACK) && mWebView.canGoBack()) { 
        mWebView.goBack();
        return true;
    }
    return super.onKeyDown(keyCode, event);
}
```

#### 3.1.3 清除缓存数据



```cpp
//清除网页访问留下的缓存
//由于内核缓存是全局的因此这个方法不仅仅针对webview而是针对整个应用程序.
Webview.clearCache(true);

//清除当前webview访问的历史记录
//只会webview访问历史记录里的所有记录除了当前访问记录
Webview.clearHistory()；

//这个api仅仅清除自动完成填充的表单数据，并不会清除WebView存储到本地的数据
Webview.clearFormData()；
```

## 3.2 常用工具类

#### 3.2.1 WebSettings类

- 作用：对WebView进行配置和管理
- 配置步骤 & 常见方法：

**配置步骤1：添加访问网络权限**（AndroidManifest.xml）

> 这是前提！这是前提！这是前提！



```xml
<uses-permission android:name="android.permission.INTERNET"/>
```

**配置步骤2：生成一个WebView组件（有两种方式）**



```cpp
//方式1：直接在在Activity中生成
WebView webView = new WebView(this)

//方法2：在Activity的layout文件里添加webview控件：
WebView webview = (WebView) findViewById(R.id.webView1);
```

**配置步骤3：进行配置-利用WebSettings子类**（常见方法）



```cpp
//声明WebSettings子类
WebSettings webSettings = webView.getSettings();

//如果访问的页面中要与Javascript交互，则webview必须设置支持Javascript
webSettings.setJavaScriptEnabled(true);  
// 若加载的 html 里有JS 在执行动画等操作，会造成资源浪费（CPU、电量）
// 在 onStop 和 onResume 里分别把 setJavaScriptEnabled() 给设置成 false 和 true 即可

//支持插件
webSettings.setPluginsEnabled(true); 

//设置自适应屏幕，两者合用
webSettings.setUseWideViewPort(true); //将图片调整到适合webview的大小 
webSettings.setLoadWithOverviewMode(true); // 缩放至屏幕的大小

//缩放操作
webSettings.setSupportZoom(true); //支持缩放，默认为true。是下面那个的前提。
webSettings.setBuiltInZoomControls(true); //设置内置的缩放控件。若为false，则该WebView不可缩放
webSettings.setDisplayZoomControls(false); //隐藏原生的缩放控件

//其他细节操作
webSettings.setCacheMode(WebSettings.LOAD_CACHE_ELSE_NETWORK); //关闭webview中缓存 
webSettings.setAllowFileAccess(true); //设置可以访问文件 
webSettings.setJavaScriptCanOpenWindowsAutomatically(true); //支持通过JS打开新窗口 
webSettings.setLoadsImagesAutomatically(true); //支持自动加载图片
webSettings.setDefaultTextEncodingName("utf-8");//设置编码格式
```

**常见用法：设置WebView缓存**

- 当加载 html 页面时，WebView会在/data/data/包名目录下生成 database 与 cache 两个文件夹
- 请求的 URL记录保存在 WebViewCache.db，而 URL的内容是保存在 WebViewCache 文件夹下
- 是否启用缓存：



```cpp
    //优先使用缓存: 
    WebView.getSettings().setCacheMode(WebSettings.LOAD_CACHE_ELSE_NETWORK); 
        //缓存模式如下：
        //LOAD_CACHE_ONLY: 不使用网络，只读取本地缓存数据
        //LOAD_DEFAULT: （默认）根据cache-control决定是否从网络上取数据。
        //LOAD_NO_CACHE: 不使用缓存，只从网络获取数据.
        //LOAD_CACHE_ELSE_NETWORK，只要本地有，无论是否过期，或者no-cache，都使用缓存中的数据。

    //不使用缓存: 
    WebView.getSettings().setCacheMode(WebSettings.LOAD_NO_CACHE);
```

- 结合使用（离线加载）



```dart
if (NetStatusUtil.isConnected(getApplicationContext())) {
    webSettings.setCacheMode(WebSettings.LOAD_DEFAULT);//根据cache-control决定是否从网络上取数据。
} else {
    webSettings.setCacheMode(WebSettings.LOAD_CACHE_ELSE_NETWORK);//没网，则从本地获取，即离线加载
}

webSettings.setDomStorageEnabled(true); // 开启 DOM storage API 功能
webSettings.setDatabaseEnabled(true);   //开启 database storage API 功能
webSettings.setAppCacheEnabled(true);//开启 Application Caches 功能

String cacheDirPath = getFilesDir().getAbsolutePath() + APP_CACAHE_DIRNAME;
webSettings.setAppCachePath(cacheDirPath); //设置  Application Caches 缓存目录
```

**注意：** 每个 Application 只调用一次 WebSettings.setAppCachePath()，WebSettings.setAppCacheMaxSize()

#### 3.2.2 WebViewClient类

- 作用：处理各种通知 & 请求事件
- 常见方法：

**常见方法1：shouldOverrideUrlLoading()**

- 作用：打开网页时不调用系统浏览器， 而是在本WebView中显示；在网页上的所有加载都经过这个方法,这个函数我们可以做很多操作。



```java
//步骤1. 定义Webview组件
Webview webview = (WebView) findViewById(R.id.webView1);

//步骤2. 选择加载方式
  //方式1. 加载一个网页：
  webView.loadUrl("http://www.google.com/");

  //方式2：加载apk包中的html页面
  webView.loadUrl("file:///android_asset/test.html");

  //方式3：加载手机本地的html页面
   webView.loadUrl("content://com.android.htmlfileprovider/sdcard/test.html");

//步骤3. 复写shouldOverrideUrlLoading()方法，使得打开网页时不调用系统浏览器， 而是在本WebView中显示
    webView.setWebViewClient(new WebViewClient(){
      @Override
      public boolean shouldOverrideUrlLoading(WebView view, String url) {
          view.loadUrl(url);
      return true;
      }
  });
```

**常见方法2：onPageStarted()**

- 作用：开始载入页面调用的，我们可以设定一个loading的页面，告诉用户程序在等待网络响应。



```java
   webView.setWebViewClient(new WebViewClient(){
     @Override
     public void  onPageStarted(WebView view, String url, Bitmap favicon) {
        //设定加载开始的操作
     }
 });
```

**常见方法3：onPageFinished()**

- 作用：在页面加载结束时调用。我们可以关闭loading 条，切换程序动作。



```java
    webView.setWebViewClient(new WebViewClient(){
      @Override
      public void onPageFinished(WebView view, String url) {
         //设定加载结束的操作
      }
  });
```

**常见方法4：onLoadResource()**

- 作用：在加载页面资源时会调用，每一个资源（比如图片）的加载都会调用一次。



```java
    webView.setWebViewClient(new WebViewClient(){
      @Override
      public boolean onLoadResource(WebView view, String url) {
         //设定加载资源的操作
      }
  });
```

**常见方法5：onReceivedError（）**

- 作用：加载页面的服务器出现错误时（如404）调用。

> App里面使用webview控件的时候遇到了诸如404这类的错误的时候，若也显示浏览器里面的那种错误提示页面就显得很丑陋了，那么这个时候我们的app就需要加载一个本地的错误提示页面，即webview如何加载一个本地的页面



```dart
//步骤1：写一个html文件（error_handle.html），用于出错时展示给用户看的提示页面
//步骤2：将该html文件放置到代码根目录的assets文件夹下

//步骤3：复写WebViewClient的onRecievedError方法
//该方法传回了错误码，根据错误类型可以进行不同的错误分类处理
    webView.setWebViewClient(new WebViewClient(){
      @Override
      public void onReceivedError(WebView view, int errorCode, String description, String failingUrl){
switch(errorCode)
                {
                case HttpStatus.SC_NOT_FOUND:
                    view.loadUrl("file:///android_assets/error_handle.html");
                    break;
                }
            }
        });
```

**常见方法6：onReceivedSslError()**

- 作用：处理https请求

> webView默认是不处理https请求的，页面显示空白，需要进行如下设置：



```java
webView.setWebViewClient(new WebViewClient() {    
        @Override    
        public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {    
            handler.proceed();    //表示等待证书响应
        // handler.cancel();      //表示挂起连接，为默认方式
        // handler.handleMessage(null);    //可做其他处理
        }    
    });  

// 特别注意：5.1以上默认禁止了https和http混用，以下方式是开启
if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
mWebView.getSettings().setMixedContentMode(WebSettings.MIXED_CONTENT_ALWAYS_ALLOW);
}
```

#### 3.2.3 WebChromeClient类

- 作用：辅助 WebView 处理 Javascript 的对话框,网站图标,网站标题等等。
- 常见使用：

**常见方法1： onProgressChanged（）**

- 作用：获得网页的加载进度并显示



```java
webview.setWebChromeClient(new WebChromeClient(){

      @Override
      public void onProgressChanged(WebView view, int newProgress) {
          if (newProgress < 100) {
              String progress = newProgress + "%";
              progress.setText(progress);
            } else {
        }
    });
```

**常见方法2： onReceivedTitle（）**

- 作用：获取Web页中的标题

> 每个网页的页面都有一个标题，比如[www.baidu.com](https://links.jianshu.com/go?to=http%3A%2F%2Fwww.baidu.com)这个页面的标题即“百度一下，你就知道”，那么如何知道当前webview正在加载的页面的title并进行设置呢？



```java
webview.setWebChromeClient(new WebChromeClient(){

    @Override
    public void onReceivedTitle(WebView view, String title) {
       titleview.setText(title)；
    }
```

**常见方法3： onJsAlert（）**

- 作用：支持javascript的警告框

> 一般情况下在 Android 中为 Toast，在文本里面加入\n就可以换行



```java
webview.setWebChromeClient(new WebChromeClient() {
            
            @Override
            public boolean onJsAlert(WebView view, String url, String message, final JsResult result)  {
    new AlertDialog.Builder(MainActivity.this)
            .setTitle("JsAlert")
            .setMessage(message)
            .setPositiveButton("OK", new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    result.confirm();
                }
            })
            .setCancelable(false)
            .show();
    return true;
            }
```

**常见方法4： onJsConfirm（）**

- 作用：支持javascript的确认框



```java
webview.setWebChromeClient(new WebChromeClient() {
        
            @Override
public boolean onJsConfirm(WebView view, String url, String message, final JsResult result) {
    new AlertDialog.Builder(MainActivity.this)
            .setTitle("JsConfirm")
            .setMessage(message)
            .setPositiveButton("OK", new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    result.confirm();
                }
            })
            .setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    result.cancel();
                }
            })
            .setCancelable(false)
            .show();
// 返回布尔值：判断点击时确认还是取消
// true表示点击了确认；false表示点击了取消；
    return true;
}

            
```

**常见方法5： onJsPrompt（）**

- 作用：支持javascript输入框

> 点击确认返回输入框中的值，点击取消返回 null。



```java
webview.setWebChromeClient(new WebChromeClient() {
            @Override
            public boolean onJsPrompt(WebView view, String url, String message, String defaultValue, final JsPromptResult result) {
    final EditText et = new EditText(MainActivity.this);
    et.setText(defaultValue);
    new AlertDialog.Builder(MainActivity.this)
            .setTitle(message)
            .setView(et)
            .setPositiveButton("OK", new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    result.confirm(et.getText().toString());
                }
            })
            .setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    result.cancel();
                }
            })
            .setCancelable(false)
            .show();

    return true;
}
            
```

# 3.3 WebView与JavaScript的交互

具体请看我写的文章：[最全面 & 最详细的 Android WebView与JS的交互方式 汇总](https://www.jianshu.com/p/345f4d8a5cfa)

# 3.4 注意事项：如何避免WebView内存泄露？

**3.4.1 不在xml中定义 Webview ，而是在需要的时候在Activity中创建，并且Context使用 getApplicationgContext()**



```csharp
LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
        mWebView = new WebView(getApplicationContext());
        mWebView.setLayoutParams(params);
        mLayout.addView(mWebView);
```

**3.4.2 在 Activity 销毁（ WebView ）的时候，先让 WebView 加载null内容，然后移除 WebView，再销毁 WebView，最后置空。**



```java
@Override
    protected void onDestroy() {
        if (mWebView != null) {
            mWebView.loadDataWithBaseURL(null, "", "text/html", "utf-8", null);
            mWebView.clearHistory();

            ((ViewGroup) mWebView.getParent()).removeView(mWebView);
            mWebView.destroy();
            mWebView = null;
        }
        super.onDestroy();
    }
```

------

# 4. 实例

- 目标：实现显示“[www.baidu.com](https://links.jianshu.com/go?to=http%3A%2F%2Fwww.baidu.com)”、获取其标题、提示加载开始 & 结束和获取加载进度
- 具体实现：

**步骤1：添加访问网络权限**

> 这是前提！这是前提！这是前提！

*AndroidManifest.xml*



```xml
<uses-permission android:name="android.permission.INTERNET"/>
```

**步骤2：主布局**
*activity_main.xml*



```xml
<?xml version="1.0" encoding="utf-8"?>
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="match_parent"
    android:layout_height="match_parent"

    android:paddingBottom="@dimen/activity_vertical_margin"
    android:paddingLeft="@dimen/activity_horizontal_margin"
    android:paddingRight="@dimen/activity_horizontal_margin"
    android:paddingTop="@dimen/activity_vertical_margin"
    tools:context="com.example.carson_ho.webview_demo.MainActivity">


   <!-- 获取网站的标题-->
    <TextView
        android:id="@+id/title"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:text=""/>

    <!--开始加载提示-->
    <TextView
        android:id="@+id/text_beginLoading"
        android:layout_below="@+id/title"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:text=""/>

    <!--获取加载进度-->
    <TextView
        android:layout_below="@+id/text_beginLoading"
        android:id="@+id/text_Loading"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:text=""/>

    <!--结束加载提示-->
    <TextView
        android:layout_below="@+id/text_Loading"
        android:id="@+id/text_endLoading"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:text=""/>
    
    <!--显示网页区域-->
    <WebView
        android:id="@+id/webView1"
        android:layout_below="@+id/text_endLoading"
        android:layout_width="fill_parent"
        android:layout_height="fill_parent"
        android:layout_marginTop="10dp" />
</RelativeLayout>
```

**步骤3：根据需要实现的功能从而使用相应的子类及其方法（注释很清楚了）**
*MainActivity.java*



```java
package com.example.carson_ho.webview_demo;

import android.graphics.Bitmap;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.KeyEvent;
import android.view.ViewGroup;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.TextView;


public class MainActivity extends AppCompatActivity {
    WebView mWebview;
    WebSettings mWebSettings;
    TextView beginLoading,endLoading,loading,mtitle;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);


        mWebview = (WebView) findViewById(R.id.webView1);
        beginLoading = (TextView) findViewById(R.id.text_beginLoading);
        endLoading = (TextView) findViewById(R.id.text_endLoading);
        loading = (TextView) findViewById(R.id.text_Loading);
        mtitle = (TextView) findViewById(R.id.title);

        mWebSettings = mWebview.getSettings();

        mWebview.loadUrl("http://www.baidu.com/");

        
        //设置不用系统浏览器打开,直接显示在当前Webview
        mWebview.setWebViewClient(new WebViewClient() {
            @Override
            public boolean shouldOverrideUrlLoading(WebView view, String url) {
                view.loadUrl(url);
                return true;
            }
        });

        //设置WebChromeClient类
        mWebview.setWebChromeClient(new WebChromeClient() {


            //获取网站标题
            @Override
            public void onReceivedTitle(WebView view, String title) {
                System.out.println("标题在这里");
                mtitle.setText(title);
            }


            //获取加载进度
            @Override
            public void onProgressChanged(WebView view, int newProgress) {
                if (newProgress < 100) {
                    String progress = newProgress + "%";
                    loading.setText(progress);
                } else if (newProgress == 100) {
                    String progress = newProgress + "%";
                    loading.setText(progress);
                }
            }
        });


        //设置WebViewClient类
        mWebview.setWebViewClient(new WebViewClient() {
            //设置加载前的函数
            @Override
            public void onPageStarted(WebView view, String url, Bitmap favicon) {
                System.out.println("开始加载了");
                beginLoading.setText("开始加载了");

            }

            //设置结束加载函数
            @Override
            public void onPageFinished(WebView view, String url) {
                endLoading.setText("结束加载了");

            }
        });
    }

    //点击返回上一页面而不是退出浏览器
    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK && mWebview.canGoBack()) {
            mWebview.goBack();
            return true;
        }

        return super.onKeyDown(keyCode, event);
    }

    //销毁Webview
    @Override
    protected void onDestroy() {
        if (mWebview != null) {
            mWebview.loadDataWithBaseURL(null, "", "text/html", "utf-8", null);
            mWebview.clearHistory();

            ((ViewGroup) mWebview.getParent()).removeView(mWebview);
            mWebview.destroy();
            mWebview = null;
        }
        super.onDestroy();
    }
}
```

### Demo地址

源代码：[Carson_Ho的Github-WebviewDemo](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FCarson-Ho%2FWebview_Cache)

------

# 5. 总结

- 本文全面介绍了`Webview`，总结如下

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfhuk5bbj30ka0c8dhs.jpg)

示意图

- 关于`WebView`知识具体请看下列文章
  [Android开发：最全面、最易懂的Webview详解](https://www.jianshu.com/p/3c94ae673e2a)
  [最全面总结 Android WebView与 JS 的交互方式](https://www.jianshu.com/p/345f4d8a5cfa)
  [手把手教你构建 Android WebView 的缓存机制 & 资源预加载方案](https://www.jianshu.com/p/5e7075f4875f)
  [你不知道的 Android WebView 使用漏洞](https://www.jianshu.com/p/3a345d27cd42)
- 接下来我会介绍继续介绍Android开发中的相关知识，感兴趣的同学可以继续关注本人运营的`Wechat Public Account`：
- [我想给你们介绍一个与众不同的Android微信公众号（福利回赠）](https://www.jianshu.com/p/2e92908af6ec)
- [我想邀请您和我一起写Android（福利回赠）](https://www.jianshu.com/p/2c5d57fb054d)

------

# 请点赞！因为你的鼓励是我写作的最大动力！

> **相关文章阅读**
> [1分钟全面了解“设计模式”](https://www.jianshu.com/p/6e5eda3a51af)
> [Android开发：最全面、最易懂的Android屏幕适配解决方案](https://www.jianshu.com/p/ec5a1a30694b)
> [Android开发：Handler异步通信机制全面解析（包含Looper、Message Queue）](https://www.jianshu.com/p/9fe944ee02f7)
> [Android开发：顶部Tab导航栏实现（TabLayout+ViewPager+Fragment）](https://www.jianshu.com/p/ce1d060573ba)
> [Android开发：底部Tab菜单栏实现（FragmentTabHost+ViewPager）](https://www.jianshu.com/p/a663803b2a44)
> [Android开发：JSON简介及最全面解析方法!](https://www.jianshu.com/p/b87fee2f7a23)
> [Android开发：XML简介及DOM、SAX、PULL解析对比](https://www.jianshu.com/p/e636f4f8487b)



作者：Carson_Ho
链接：https://www.jianshu.com/p/3c94ae673e2a
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。