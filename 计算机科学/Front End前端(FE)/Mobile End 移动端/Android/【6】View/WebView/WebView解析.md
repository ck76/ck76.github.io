[TOC]

## 简介

WebView 是一个用来显示 Web 网页的控件，继承自 AbsoluteLayout，和使用系统其他控件没什么区别，只是 WeView 控件方法比较多比较丰富。因为它就是一个微型浏览器，包含一个浏览器该有的基本功能，例如：滚动、缩放、前进、后退下一页、搜索、执行 Js等功能。

> 在 Android 4.4 之前使用 WebKit 作为渲染内核，4.4 之后采用 chrome 内核。Api 使用兼容低版本。

**配置网络权限**

![image-20190414214130867](https://ws3.sinaimg.cn/large/006tNc79ly1g22huciupjj31860qual4.jpg)



## 作用

- 显示和渲染Web页面
- 直接使用html文件（网络上或本地assets中）作布局
- 可和JavaScript交互调用

> WebView控件功能强大，除了具有一般View的属性和设置外，还可以对url请求、页面加载、渲染、页面交互进行强大的处理。



## 基本使用

一般来说Webview可单独使用，可联合其工具类一起使用：

- Webview类自身的常见方法
- Webview的最常用的工具类：WebSettings类、WebViewClient类、WebChromeClient类
- Android 和 Js的交互

```xml
//配置网络权限
<uses-permission android:name="android.permission.INTERNET"/>
 //布局
<?xml version="1.0" encoding="utf-8"?>
    <LinearLayout
      xmlns:android="http://schemas.android.com/apk/res/android"
      android:id="@+id/main"
      android:layout_width="match_parent"
      android:layout_height="match_parent">
<com.webview.SafeWebView
    android:id="@+id/web_view"
    android:layout_width="match_parent"
    android:layout_height="match_parent"/>
</LinearLayout>
```



## WebView方法

### 常用方法

- void loadUrl(String url):加载网络链接 url
- boolean canGoBack():判断 WebView 当前是否可以返回上一页
- goBack():回退到上一页
- boolean canGoForward():判断 WebView 当前是否可以向前一页
- goForward():回退到前一页
- onPause():类似 Activity 生命周期，页面进入后台不可见状态
- pauseTimers():该方法面向全局整个应用程序的webview，它会暂停所有webview的layout，parsing，JavaScript Timer。当程序进入后台时，该方法的调用可以降低CPU功耗。
- onResume():在调用 onPause()后，可以调用该方法来恢复 WebView 的运行。
- resumeTimers():恢复pauseTimers时的所有操作。(注：pauseTimers和resumeTimers 方法必须一起使用，否则再使用其它场景下的 WebView 会有问题)
- destroy():销毁 WebView
- clearHistory():清除当前 WebView 访问的历史记录。
- clearCache(boolean includeDiskFiles):清空网页访问留下的缓存数据。需要注意的时，由于缓存是全局的，所以只要是WebView用到的缓存都会被清空，即便其他地方也会使用到。该方法接受一个参数，从命名即可看出作用。若设为false，则只清空内存里的资源缓存，而不清空磁盘里的。
- reload():重新加载当前请求
- setLayerType(int layerType, Paint paint):设置硬件加速、软件加速
- removeAllViews():清除子view。
- clearSslPreferences():清除ssl信息。
- clearMatches():清除网页查找的高亮匹配字符。
- removeJavascriptInterface(String interfaceName):删除interfaceName 对应的注入对象
- addJavascriptInterface(Object object,String interfaceName):注入 java 对象。
- setVerticalScrollBarEnabled(boolean verticalScrollBarEnabled):设置垂直方向滚动条。
- setHorizontalScrollBarEnabled(boolean horizontalScrollBarEnabled):设置横向滚动条。
- loadUrl(String url, Map<String, String> additionalHttpHeaders):加载制定url并携带http header数据。
- evaluateJavascript(String script, ValueCallback<String> resultCallback):Api 19 之后可以采用此方法之行 Js。
- stopLoading():停止 WebView 当前加载。
- clearView():在Android 4.3及其以上系统这个api被丢弃了， 并且这个api大多数情况下会有bug，经常不能清除掉之前的渲染数据。官方建议通过loadUrl("about:blank")来实现这个功能，阴雨需要重新加载一个页面自然时间会收到影响。
- freeMemory():释放内存，不过貌似不好用。
- clearFormData():清除自动完成填充的表单数据。需要注意的是，该方法仅仅清除当前表单域自动完成填充的表单数据，并不会清除WebView存储到本地的数据。

我这里在介绍下下面几组方法，比较重要，项目当中可能会遇到坑

- onPause() 尽力尝试暂停可以暂停的任何处理，如动画和地理位置。 不会暂停JavaScript。 要全局暂停JavaScript，可使用pauseTimers。
- onResume() 恢复onPause() 停掉的操作；
- pauseTimers() 暂停所有WebView的布局，解析和JavaScript定时器。 这个是一个全局请求，不仅限于这个WebView。
- resumeTimers() 恢复所有WebView的所有布局，解析和JavaScript计时器，将恢复调度所有计时器.

> 另外注意 JS 端setTimeout()、setInterval() 方法使用，自测来看，当不使用 pauseTimers()  和 pauseTimers() ，从 Activity 返回上一个包含WebView 的Activity时，页面里的 setTimeout() 是不执行的，setInterval() 是可以恢复执行的。

在适当的生命周期使用 pauseTimers()  和 pauseTimers() 既可以恢复setTimeout() 执行。

### 加载url

加载方式根据资源分为三种

```java
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

### WebView的状态

```java
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

### 关于前进 / 后退网页

```java
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

```java
public boolean onKeyDown(int keyCode, KeyEvent event) {
    if ((keyCode == KEYCODE_BACK) && mWebView.canGoBack()) { 
        mWebView.goBack();
        return true;
    }
    return super.onKeyDown(keyCode, event);
}
```

### 清除缓存数据

```java
//清除网页访问留下的缓存
//由于内核缓存是全局的因此这个方法不仅仅针对webview而是针对整个应用程序.
Webview.clearCache(true);

//清除当前webview访问的历史记录
//只会webview访问历史记录里的所有记录除了当前访问记录
Webview.clearHistory()；

//这个api仅仅清除自动完成填充的表单数据，并不会清除WebView存储到本地的数据
Webview.clearFormData()；
```

###  一份 WebView 方法使用清单

```java
mWebView.loadUrl("http://www.jianshu.com/u/fa272f63280a");// 加载url，也可以执行js函数
mWebView.setWebViewClient(new SafeWebViewClient());// 设置 WebViewClient 
mWebView.setWebChromeClient(new SafeWebChromeClient());// 设置 WebChromeClient
mWebView.onResume();// 生命周期onResume
mWebView.resumeTimers();//生命周期resumeTimers
mWebView.onPause();//生命周期onPause
mWebView.pauseTimers();//生命周期pauseTimers (上数四个方法都是成对出现)
mWebView.stopLoading();// 停止当前加载
mWebView.clearMatches();// 清除网页查找的高亮匹配字符。
mWebView.clearHistory();// 清除当前 WebView 访问的历史记录
mWebView.clearSslPreferences();//清除ssl信息
mWebView.clearCache(true);//清空网页访问留下的缓存数据。需要注意的时，由于缓存是全局的，所以只要是WebView用到的缓存都会被清空，即便其他地方也会使用到。该方法接受一个参数，从命名即可看出作用。若设为false，则只清空内存里的资源缓存，而不清空磁盘里的。
mWebView.loadUrl("about:blank");// 清空当前加载
mWebView.removeAllViews();// 清空子 View
if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR2) {
    mWebView.removeJavascriptInterface("AndroidNative");// 向 Web端注入 java 对象
}
mWebView.destroy();// 生命周期销毁
```



## 常用工具类

>  主要包含三部分：WebSettings、WebViewClient、WebChromeClient。 



### **WebSettings**类

- 作用：对WebView进行配置和管理

- 配置步骤 & 常见方法：

### **常用方法**

- setJavaScriptEnabled(boolean flag):是否支持 Js 使用。
- setCacheMode(int mode):设置 WebView 的缓存模式。
- setAppCacheEnabled(boolean flag):是否启用缓存模式。
- setAppCachePath(String appCachePath):Android 私有缓存存储，如果你不调用setAppCachePath方法，WebView将不会产生这个目录。
- setSupportZoom(boolean support):是否支持缩放。
- setTextZoom(int textZoom):Sets the text zoom of the page in percent. The default is 100。
- setAllowFileAccess(boolean allow):是否允许加载本地 html 文件/false。
- setDatabaseEnabled(boolean flag):是否开启数据库缓存
- setDomStorageEnabled(boolean flag):是否开启DOM缓存。
- setUserAgentString(String ua):设置 UserAgent 属性。
- setLoadsImagesAutomatically(boolean flag):支持自动加载图片
- setAllowFileAccessFromFileURLs(boolean flag：:允许通过 file url 加载的 Javascript 读取其他的本地文件,Android 4.1 之前默认是true，在 Android 4.1 及以后默认是false,也就是禁止。
- setAllowUniversalAccessFromFileURLs(boolean flag):允许通过 file url 加载的 Javascript 可以访问其他的源，包括其他的文件和 http，https 等其他的源，Android 4.1 之前默认是true，在 Android 4.1 及以后默认是false,也就是禁止如果此设置是允许，则 setAllowFileAccessFromFileURLs 不起做用。
- boolean getLoadsImagesAutomatically():是否支持自动加载图片。

**常见方法**

```java
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

```java
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

```java
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

### **一份使用清单**

```java
WebSettings webSettings = mWebView.getSettings();
if (webSettings == null) return;
// 支持 Js 使用
webSettings.setJavaScriptEnabled(true);
// 开启DOM缓存
webSettings.setDomStorageEnabled(true);
// 开启数据库缓存
webSettings.setDatabaseEnabled(true);
// 支持自动加载图片
webSettings.setLoadsImagesAutomatically(hasKitkat());
// 设置 WebView 的缓存模式
webSettings.setCacheMode(WebSettings.LOAD_DEFAULT);
// 支持启用缓存模式
webSettings.setAppCacheEnabled(true);
// 设置 AppCache 最大缓存值(现在官方已经不提倡使用，已废弃)
webSettings.setAppCacheMaxSize(8 * 1024 * 1024);
// Android 私有缓存存储，如果你不调用setAppCachePath方法，WebView将不会产生这个目录
webSettings.setAppCachePath(getCacheDir().getAbsolutePath());
// 数据库路径
if (!hasKitkat()) {
    webSettings.setDatabasePath(getDatabasePath("html").getPath());
}
// 关闭密码保存提醒功能
webSettings.setSavePassword(false);
// 支持缩放
webSettings.setSupportZoom(true);
// 设置 UserAgent 属性
webSettings.setUserAgentString("");
// 允许加载本地 html 文件/false
webSettings.setAllowFileAccess(true);
// 允许通过 file url 加载的 Javascript 读取其他的本地文件,Android 4.1 之前默认是true，在 Android 4.1 及以后默认是false,也就是禁止
webSettings.setAllowFileAccessFromFileURLs(false);
// 允许通过 file url 加载的 Javascript 可以访问其他的源，包括其他的文件和 http，https 等其他的源，
// Android 4.1 之前默认是true，在 Android 4.1 及以后默认是false,也就是禁止
// 如果此设置是允许，则 setAllowFileAccessFromFileURLs 不起做用
webSettings.setAllowUniversalAccessFromFileURLs(false);
```

## **WebViewClient类**

- 作用：处理各种通知 & 请求事件

- 常见方法：

### **常用方法**

- onPageStarted(WebView view, String url, Bitmap favicon):WebView 开始加载页面时回调，一次Frame加载对应一次回调。
- onLoadResource(WebView view, String url):WebView 加载页面资源时会回调，每一个资源产生的一次网络加载，除非本地有当前 url 对应有缓存，否则就会加载。
- shouldInterceptRequest(WebView view, String url):WebView 可以拦截某一次的 request 来返回我们自己加载的数据，这个方法在后面缓存会有很大作用。
- shouldInterceptRequest(WebView view, android.webkit.WebResourceRequest request):WebView 可以拦截某一次的 request 来返回我们自己加载的数据，这个方法在后面缓存会有很大作用。
- shouldOverrideUrlLoading(WebView view, String url):是否在 WebView 内加载页面。
- onReceivedSslError(WebView view, SslErrorHandler handler, SslError error):WebView ssl 访问证书出错，handler.cancel()取消加载，handler.proceed()对然错误也继续加载。
- onPageFinished(WebView view, String url):WebView 完成加载页面时回调，一次Frame加载对应一次回调。
- onReceivedError(WebView view, int errorCode, String description, String failingUrl):WebView 访问 url 出错。

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

```java
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

### **一份使用清单**

```java
public class SafeWebViewClient extends WebViewClient {
    /**
     * 当WebView得页面Scale值发生改变时回调
     */
    @Override
    public void onScaleChanged(WebView view, float oldScale, float newScale) {
        super.onScaleChanged(view, oldScale, newScale);
    }
    /**
     * 是否在 WebView 内加载页面
     */
    @Override
    public boolean shouldOverrideUrlLoading(WebView view, String url) {
        view.loadUrl(url);
        return true;
    }
    /**
     * WebView 开始加载页面时回调，一次Frame加载对应一次回调
     */
    @Override
    public void onPageStarted(WebView view, String url, Bitmap favicon) {
        super.onPageStarted(view, url, favicon);
    }
    /**
     * WebView 完成加载页面时回调，一次Frame加载对应一次回调
     */
    @Override
    public void onPageFinished(WebView view, String url) {
        super.onPageFinished(view, url);
    }
    /**
     * WebView 加载页面资源时会回调，每一个资源产生的一次网络加载，除非本地有当前 url 对应有缓存，否则就会加载。
     */
    @Override
    public void onLoadResource(WebView view, String url) {
        super.onLoadResource(view, url);
    }
    /**
     * WebView 可以拦截某一次的 request 来返回我们自己加载的数据，这个方法在后面缓存会有很大作用。
     *
     * @param view    WebView
     * @param request 当前产生 request 请求
     * @return WebResourceResponse
     */
    @Override
    public WebResourceResponse shouldInterceptRequest(WebView view, WebResourceRequest request) {
        return super.shouldInterceptRequest(view, request);
    }
    /**
     * WebView 访问 url 出错
     */
    @Override
    public void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error) {
        super.onReceivedError(view, request, error);
    }
    /**
     * WebView ssl 访问证书出错，handler.cancel()取消加载，handler.proceed()对然错误也继续加载
     */
    @Override
    public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {
        super.onReceivedSslError(view, handler, error);
    }
}
```



## **WebChromeClient类**

- 作用：辅助 WebView 处理 Javascript 的对话框,网站图标,网站标题等等。
- 常见使用：

### **常用方法**

- onConsoleMessage(String message, int lineNumber,String sourceID):输出 Web 端日志。
- onProgressChanged(WebView view, int newProgress):当前 WebView 加载网页进度。
- onJsPrompt(WebView view, String url, String message, String defaultValue, JsPromptResult result):处理 JS 中的 Prompt对话框
- onJsAlert(WebView view, String url, String message, JsResult result): Js 中调用 alert() 函数，产生的对话框。
- onReceivedTitle(WebView view, String title):接收web页面的 Title。
- onReceivedIcon(WebView view, Bitmap icon):接收web页面的icon。

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

> 每个网页的页面都有一个标题，比如[www.baidu.com](https://link.jianshu.com?t=http%3A%2F%2Fwww.baidu.com)这个页面的标题即“百度一下，你就知道”，那么如何知道当前webview正在加载的页面的title并进行设置呢？

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



### **一份使用清单**

```java
public class SafeWebChromeClient extends WebChromeClient {
    @Override
    public boolean onConsoleMessage(ConsoleMessage consoleMessage) {
        return super.onConsoleMessage(consoleMessage);
    }
    /**
     * 当前 WebView 加载网页进度
     *
     * @param view
     * @param newProgress
     */
    @Override
    public void onProgressChanged(WebView view, int newProgress) {
        super.onProgressChanged(view, newProgress);
    }
    /**
     * Js 中调用 alert() 函数，产生的对话框
     *
     * @param view
     * @param url
     * @param message
     * @param result
     * @return
     */
    @Override
    public boolean onJsAlert(WebView view, String url, String message, JsResult result) {
        return super.onJsAlert(view, url, message, result);
    }
    /**
     * 处理 Js 中的 Confirm 对话框
     *
     * @param view
     * @param url
     * @param message
     * @param result
     * @return
     */
    @Override
    public boolean onJsConfirm(WebView view, String url, String message, JsResult result) {
        return super.onJsConfirm(view, url, message, result);
    }
    /**
     * 处理 JS 中的 Prompt对话框
     *
     * @param view
     * @param url
     * @param message
     * @param defaultValue
     * @param result
     * @return
     */
    @Override
    public boolean onJsPrompt(WebView view, String url, String message, String defaultValue, JsPromptResult result) {
        return super.onJsPrompt(view, url, message, defaultValue, result);
    }
    /**
     * 接收web页面的icon
     *
     * @param view
     * @param icon
     */
    @Override
    public void onReceivedIcon(WebView view, Bitmap icon) {
        super.onReceivedIcon(view, icon);
    }
    /**
     * 接收web页面的 Title
     *
     * @param view
     * @param title
     */
    @Override
    public void onReceivedTitle(WebView view, String title) {
        super.onReceivedTitle(view, title);
    }
}
```



[一份很详细的讲解](https://www.jianshu.com/p/345f4d8a5cfa)

[cc](https://www.jianshu.com/p/d2f5ae6b4927)



## 与JavaScript交互

### 基本介绍

Android与JS通过WebView互相调用方法，实际上是：

- Android去调用JS的代码
- JS去调用Android的代码

> 二者沟通的桥梁是WebView

![image-20190414215011475](https://ws2.sinaimg.cn/large/006tNc79ly1g22i3aiwbxj31z40owwos.jpg)

### 交互方式总结

![webview与js互调2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/dU1mQLBbrSTsIO2Qu3ttcClAI5N32tPTV1nCupBG0W0!/r/dFMBAAAAAAAA)

**对于Android调用JS代码的方法有2种：**

1. 通过`WebView`的`loadUrl（）` 
2. 通过`WebView`的`evaluateJavascript（）` 

**对于JS调用Android代码的方法有3种：**

1. 通过`WebView`的`addJavascriptInterface（）`进行对象映射
2. 通过 `WebViewClient` 的`shouldOverrideUrlLoading ()`方法回调拦截 url
3. 通过 `WebChromeClient` 的`onJsAlert()`、`onJsConfirm()`、`onJsPrompt（）`方法回调拦截JS对话框`alert()`、`confirm()`、`prompt（）` 消息

### 具体分析

> 注：这里着重介绍下第一种标准方式，后面会介绍其他两种方式。

1、使用系统方法 addJavascriptInterface 注入 java 对象来实现。

2、利用 WebViewClient 中 shouldOverrideUrlLoading (WebView view, String url) 接口，拦截操作。这个就是很多公司在用的 scheme  方式，通过制定url协议，双方各自解析，使用iframe来调用native代码，实现互通。

3、利用 WebChromeClient 中的 onJsAlert、onJsConfirm、onJsPrompt 提示接口，同样也是拦截操作。

### 使用清单：

```java
//开启Js可用
mWebView.getSettings().setJavaScriptEnabled(true);
// 创建要注入的 Java 类
public class NativeInterface {
    private Context mContext;
    public NativeInterface(Context context) {
        mContext = context;
    }
    @JavascriptInterface
    public void hello() {
        Toast.makeText(mContext, "hello", Toast.LENGTH_SHORT).show();
    }
    @JavascriptInterface
    public void hello(String params) {
        Toast.makeText(mContext, params, Toast.LENGTH_SHORT).show();
    }
    @JavascriptInterface
    public String getAndroid() {
        Toast.makeText(mContext, "getAndroid", Toast.LENGTH_SHORT).show();
        return "Android data";
    }
}
// WebView 注入即可
mWebView.addJavascriptInterface(new NativeInterface(this), "AndroidNative");
//Js编写
<script>
    function callHello(){
        AndroidNative.hello();
    }
    function callHello1(){
        AndroidNative.hello('hello Android');
    }
    function callAndroid(){
        var temp = AndroidNative.getAndroid();
        console.log(temp);
        alert(temp);
    }  
</script>
```

Native 调用 Js：mWebView.loadUrl(js);

Js 调用 Native :AndroidNative.getAndroid();

> 4.2版本以下会存在漏洞，4.2以上需要添加 @JavascriptInterface 注解才能被调用到，Js 调用方式不变。

 Js注入漏洞

> 虽然可以通过注入方式来实现 WebView 和 JS 交互，但是实现功能的同时也带了安全问题，通过注入的 Java 类作为桥梁，JS 就可以利用这个漏洞。



## 缓存机制构建

- `Android WebView`由于前端`h5`本身的原因，存在加载效率慢 & 流量耗费的性能问题。
- 通过 **H5缓存机制 + 资源预加载 + 资源拦截**的方式 构建了一套`WebView`缓存机制，从而解决`Android WebView`的性能问题，最终提高用户使用体验

![webview性能问题](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/tkl*io8bILz.GU7yXFoLCsEUxN6NQQCkMfEsmErEg44!/r/dEYBAAAAAAAA)



## **常见漏洞**

- `WebView` 使用过程中存在许多漏洞，容易造成用户数据泄露等等危险，而很多人往往会忽视这个问题
- `WebView`中，主要漏洞有3类：**任意代码执行漏洞、密码明文存储漏洞、域控制不严格漏洞**

目前已知的 WebView 漏洞有 4 个,分别是：

1、CVE-2012-6636，揭露了 WebView 中 addJavascriptInterface 接口会引起远程代码执行漏洞；
2、CVE-2013-4710，针对某些特定机型会存在 addJavascriptInterface API 引起的远程代码执行漏洞；
3、CVE-2014-1939 爆出 WebView 中内置导出的 “searchBoxJavaBridge_” Java Object 可能被利用，实现远程任意代码；
4、CVE-2014-7224，类似于 CVE-2014-1939 ，WebView 内置导出 “accessibility” 和 “accessibilityTraversal” 两个 Java Object 接口，可被利用实现远程任意代码执行。



### **如何解决漏洞**

1、Android 4.2 以下不要在使用 JavascriptInterface方式，4.2 以上需要添加注解 @JavascriptInterface 才能调用。（这部分和JsBrige 有关，更详细的内容后面会介绍）

2、同1解决；

3、在创建 WebView 时，使用 removeJavascriptInterface 方法将系统注入的 searchBoxJavaBridge_ 对象删除。

4、当系统辅助功能服务被开启时，在 Android 4.4 以下的系统中，由系统提供的 WebView 组件都默认导出 ”accessibility” 和 ”accessibilityTraversal” 这两个接口，这两个接口同样存在远程任意代码执行的威胁，同样的需要通过 removeJavascriptInterface 方法将这两个对象删除。

```java
super.removeJavascriptInterface("searchBoxJavaBridge_");
super.removeJavascriptInterface("accessibility");
super.removeJavascriptInterface("accessibilityTraversal");
```

以上都是系统机制层面上的漏洞，还有一些是使用 WebView 不挡产生的漏洞。

5、通过 WebSettings.setSavePassword(false) 关闭密码保存提醒功能，防止明文密码存在本地被盗用。

6、WebView 默认是可以使用 File 协议的，也就是 setAllowFileAccess(true)，我们应该是主动设置为 setAllowFileAccess(false),防止加载本地文件，移动版的 Chrome 默认禁止加载 file 协议的文件。

```java
setAllowFileAccess(true);//设置为 false 将不能加载本地 html 文件
setAllowFileAccessFromFileURLs(false);
setAllowUniversalAccessFromFileURLs(false);
if (url.startsWith("file://") {
    setJavaScriptEnabled(false);
} else {
    setJavaScriptEnabled(true);
}
```

###  **一些坑**

> 主要总结 WebView 相关的疑难 bug，由于 Android 版本严重碎片化，在使用 WebView 的时候也会遇到各种个样的坑，特别是 4.4 之后更换了 WebView 内核，4.2 以下有部分漏洞，所以想把经历过的 WebView 这些坑记录下来，仅供参考。

**1、android.webkit.AccessibilityInjector$TextToSpeechWrapper**

```java
java.lang.NullPointerException
    at android.webkit.AccessibilityInjector$TextToSpeechWrapper$1.onInit(AccessibilityInjector.java:753)
    at android.speech.tts.TextToSpeech.dispatchOnInit(TextToSpeech.java:640)
    at android.speech.tts.TextToSpeech.initTts(TextToSpeech.java:619)
    at android.speech.tts.TextToSpeech.<init>(TextToSpeech.java:553)
    at android.webkit.AccessibilityInjector$TextToSpeechWrapper.<init>(AccessibilityInjector.java:676)
    at android.webkit.AccessibilityInjector.addTtsApis(AccessibilityInjector.java:480)
    at android.webkit.AccessibilityInjector.addAccessibilityApisIfNecessary(AccessibilityInjector.java:168)
    at android.webkit.AccessibilityInjector.onPageStarted(AccessibilityInjector.java:340)
    at android.webkit.WebViewClassic.onPageStarted(WebViewClassic.java:4480)
    at android.webkit.CallbackProxy.handleMessage(CallbackProxy.java:366)
    at android.os.Handler.dispatchMessage(Handler.java:107)
    at android.os.Looper.loop(Looper.java:194)
    at android.app.ActivityThread.main(ActivityThread.java:5407)
    at java.lang.reflect.Method.invokeNative(Native Method)
    at java.lang.reflect.Method.invoke(Method.java:525)
    at com.android.internal.os.ZygoteInit$MethodAndArgsCaller.run(ZygoteInit.java:833)
    at com.android.internal.os.ZygoteInit.main(ZygoteInit.java:600)
    at dalvik.system.NativeStart.main(Native Method)
```

此问题在4.2.1和4.2.2比较集中，关闭辅助功能，google 下很多结果都是一样的。

修复方法：在初始化 WebView 时调用disableAccessibility方法即可。

```java
public static void disableAccessibility(Context context) {
    if (Build.VERSION.SDK_INT == 17/*4.2 (Build.VERSION_CODES.JELLY_BEAN_MR1)*/) {
        if (context != null) {
            try {
                AccessibilityManager am = (AccessibilityManager) context.getSystemService(Context.ACCESSIBILITY_SERVICE);
                if (!am.isEnabled()) {
                    //Not need to disable accessibility
                    return;
                }
                Method setState = am.getClass().getDeclaredMethod("setState", int.class);
                setState.setAccessible(true);
                setState.invoke(am, 0);/**{@link AccessibilityManager#STATE_FLAG_ACCESSIBILITY_ENABLED}*/
            } catch (Exception ignored) {
                ignored.printStackTrace();
            }
        }
    }
}
```

**2、android.content.pm.PackageManager$NameNotFoundException**

```php
AndroidRuntimeException: android.content.pm.PackageManager$NameNotFoundException: com.google.android.webview
    at android.app.ActivityThread.handleBindApplication(ActivityThread.java:4604)
    at android.app.ActivityThread.access$1500(ActivityThread.java:154)
    at android.app.ActivityThread$H.handleMessage(ActivityThread.java:1389)
    at android.os.Handler.dispatchMessage(Handler.java:102)
    at android.os.Looper.loop(Looper.java:135)
    at android.app.ActivityThread.main(ActivityThread.java:5302)
    at java.lang.reflect.Method.invoke(Native Method)
    at java.lang.reflect.Method.invoke(Method.java:372)
    at com.android.internal.os.ZygoteInit$MethodAndArgsCaller.run(ZygoteInit.java:916)
    at com.android.internal.os.ZygoteInit.main(ZygoteInit.java:711)
Caused by: android.util.AndroidRuntimeException: android.content.pm.PackageManager$NameNotFoundException: com.google.android.webview
    at android.webkit.WebViewFactory.getFactoryClass(WebViewFactory.java:174)
    at android.webkit.WebViewFactory.getProvider(WebViewFactory.java:109)
    at android.webkit.WebView.getFactory(WebView.java:2194)
    at android.webkit.WebView.ensureProviderCreated(WebView.java:2189)
    at android.webkit.WebView.setOverScrollMode(WebView.java:2248)
    at android.view.View.<init>(View.java:3588)
    at android.view.View.<init>(View.java:3682)
    at android.view.ViewGroup.<init>(ViewGroup.java:497)
    at android.widget.AbsoluteLayout.<init>(AbsoluteLayout.java:55)
    at android.webkit.WebView.<init>(WebView.java:544)
    at android.webkit.WebView.<init>(WebView.java:489)
    at android.webkit.WebView.<init>(WebView.java:472)
    at android.webkit.WebView.<init>(WebView.java:459)
    at android.webkit.WebView.<init>(WebView.java:449)
```

现象：在创建 WebView 时崩溃，跟进栈信息，我们需要在 setOverScrollMode 方法上加异常保护处理

修复方法：

```java
try {
    super.setOverScrollMode(mode);
} catch (Throwable e) {
    e.printStackTrace();
}  
```

不过上面捕获的异常范围有点广，在github上找到一个更全面的修复方法

```java
try{
    super.setOverScrollMode(mode);
} catch(Throwable e){
        String messageCause = e.getCause() == null ? e.toString() : e.getCause().toString();
String trace = Log.getStackTraceString(e);
if (trace.contains("android.content.pm.PackageManager$NameNotFoundException")
  || trace.contains("java.lang.RuntimeException: Cannot load WebView")
    || trace.contains("android.webkit.WebViewFactory$MissingWebViewPackageException: Failed to load WebView provider: No WebView installed")) {
      e.printStackTrace();
    }else{
      throw e;
    }        
}
```

**3、android.webkit.WebViewClassic.clearView**

```java
at android.webkit.BrowserFrame.nativeLoadUrl(Native Method)
System.err:     at android.webkit.BrowserFrame.loadUrl(BrowserFrame.java:279)
System.err:     at android.webkit.WebViewCore.loadUrl(WebViewCore.java:2011)
System.err:     at android.webkit.WebViewCore.access$1900(WebViewCore.java:57)
System.err:     at android.webkit.WebViewCore$EventHub$1.handleMessage(WebViewCore.java:1303)
System.err:     at android.os.Handler.dispatchMessage(Handler.java:99)
System.err:     at android.os.Looper.loop(Looper.java:137)
System.err:     at android.webkit.WebViewCore$WebCoreThread.run(WebViewCore.java:812)
System.err:     at java.lang.Thread.run(Thread.java:856)
webcoreglue: *** Uncaught exception returned from Java call!
System.err: java.lang.NullPointerException
System.err:     at android.webkit.WebViewClassic.clearView(WebViewClassic.java:2868)
System.err:     at android.webkit.WebViewCore.setupViewport(WebViewCore.java:2497)
System.err:     at android.webkit.WebViewCore.updateViewport(WebViewCore.java:2479)
System.err:     at android.webkit.BrowserFrame.nativeLoadUrl(Native Method)
System.err:     at android.webkit.BrowserFrame.loadUrl(BrowserFrame.java:279)
System.err:     at android.webkit.WebViewCore.loadUrl(WebViewCore.java:2011)
System.err:     at android.webkit.WebViewCore.access$1900(WebViewCore.java:57)
System.err:     at android.webkit.WebViewCore$EventHub$1.handleMessage(WebViewCore.java:1303)
System.err:     at android.os.Handler.dispatchMessage(Handler.java:99)
System.err:     at android.os.Looper.loop(Looper.java:137)
```

这个bug是在某些设备上发生的，是在调用webView.destroy() 之前调用了loadurl操作发生的，也不是毕现问题，所以只能跟进源码查看，在清空 webview destroy 时，调用清理方法，内部可能时机有问题，会出现，WebViewClassic 中 mWebViewCore 对象为null，其内部为handler消息机制。

修复方法：

```java
public void logdUrl(final String url) {
    try {
        super.loadUrl(url);
    } catch (NullPointerException e) {
        e.printStackTrace();
    }
}
```



## 注意事项：如何避免WebView内存泄露？

**不在xml中定义 Webview ，而是在需要的时候在Activity中创建，并且Context使用 getApplicationgContext()**

```java
LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
        mWebView = new WebView(getApplicationContext());
        mWebView.setLayoutParams(params);
        mLayout.addView(mWebView);
```

**在 Activity 销毁（ WebView ）的时候，先让 WebView 加载null内容，然后移除 WebView，再销毁 WebView，最后置空。**

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



## WebView 缓存原理分析和应用

这部分内容可以参考这两篇博文:

- WebView缓存原理分析和应用

  *http://unclechen.github.io/2017/05/13/WebView%E7%BC%93%E5%AD%98%E5%8E%9F%E7%90%86%E5%88%86%E6%9E%90%E5%92%8C%E5%BA%94%E7%94%A8/*

- H5 缓存机制浅析 移动端 Web 加载性能优化

  *https://segmentfault.com/a/1190000004132566*

写的已经很清楚了，我这里就不赘述了。

主要包含以下内容：

1、WebView 的5中缓存类型，以及每个缓存类型工作原理、相同点和不同点、。
2、缓存在手机上的存储。
3、每种缓存机制案例。

如果你想通过过滤来减缓 WebView 请求网络，可以参考 rexxar-android 中关于拦截url操作读取本地操作。



## 性能、体验分析与优化

参考美团：WebView性能、体验分析与优化

*https://tech.meituan.com/WebViewPerf.html*

这部分美团的技术博客已经写的很好了，不仅从性能、内存消耗、体验、安全几个维度，来系统的分析客户端默认 WebView 的问题，还给出了对应的优化方案。

> 我的感受：文章中也提到了 QQ 的 Hybrid 架构演进，主要的优化方向和内容和下面的Hybrid 开源框架 VasSonic 基本一直，当然都是腾讯东东，应该是有所借鉴的。而且关于 WebView 的优化也就是那几部分，串行该并行、缓存 WebView、客户端代替 WebView 网络请求，WebView 拦截url加载本地资源，还有Web 端(cdn 神马的，哈哈，不太熟悉)等等几个主要方面，但是能将上数几个方面都完美的结合到一起的市面上很少，VasSonic就做到了。

 

- [WebView坑](https://www.jianshu.com/p/2b2e5d417e10)