[TOC]

# 前言

- 由于`H5`具备 开发周期短、灵活性好 的特点，所以现在 `Android App`大多嵌入了 `Android Webview` 组件进行 `Hybrid` 开发
- 但我知道你一定在烦恼 `Android Webview` 的性能问题，特别突出的是：加载速度慢 & 消耗流量
- 今天，我将针对 `Android Webview` 的性能问题，提出一些有效解决方案。

------

# 目录

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfm96v6hj30xc0hf415.jpg)

示意图

------

# 1. Android WebView 存在什么性能问题？

- `Android WebView` 里 `H5` 页面加载速度慢
- 耗费流量

下面会详细介绍。

### 1.1 H5 页面加载速度慢

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfm8rnerj30xc0gmmyj.jpg)

H5 页面加载速度慢的原因

下面会详细介绍：

##### 1.1.1 渲染速度慢

前端`H5`页面渲染的速度取决于 两个方面：

- `Js` 解析效率
  `Js` 本身的解析过程复杂、解析速度不快 & 前端页面涉及较多 `JS` 代码文件，所以叠加起来会导致 `Js` 解析效率非常低
- 手机硬件设备的性能
  由于`Android`机型碎片化，这导致手机硬件设备的性能不可控，而大多数的Android手机硬件设备无法达到很好很好的硬件性能

总结：上述两个原因 导致 **H5页面的渲染速度慢**。

##### 1.1.2 页面资源加载缓慢

`H5` 页面从服务器获得，并存储在 `Android`手机内存里：

- `H5`页面一般会比较多

- 每加载一个

   

  ```
  H5
  ```

  页面，都会产生较多网络请求：

  1. `HTML` 主 `URL` 自身的请求；
  2. `HTML`外部引用的`JS、CSS`、字体文件，图片也是一个独立的 `HTTP` 请求

每一个请求都串行的，这么多请求串起来，这导致 `H5`页面资源加载缓慢

**总结：H5页面加载速度慢的原因：渲染速度慢 & 页面资源加载缓慢 导致**。

### 1.2 耗费流量

- 每次使用 `H5`页面时，用户都需要重新加载 `Android WebView`的`H5` 页面
- 每加载一个 `H5`页面，都会产生较多网络请求（上面提到）
- 每一个请求都串行的，这么多请求串起来，这导致消耗的流量也会越多

### 1.3 总结

- 综上所述，产生`Android WebView`性能问题主要原因是：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfm53n3fj30xc0d0acd.jpg)

示意图

- 上述问题导致了`Android WebView`的`H5` 页面体验 与 原生`Native` 存在较大差距。

------

# 2. 解决方案

针对上述`Android WebView`的性能问题，我提出了3种解决方案：

- 前端`H5`的缓存机制（`WebView` 自带）
- 资源预加载
- 资源拦截

下面我将详细介绍。

### 2.1 前端`H5`的缓存机制

- 定义
  缓存，即离线存储

> 1. 这意味着 `H5`网页 加载后会存储在缓存区域，在无网络连接时也可访问
> 2. `WebView`的本质 = 在 `Android`中嵌入 `H5`页面，所以，`Android WebView`自带的缓存机制其实就是 `H5`页面的缓存机制
> 3. `Android WebView`除了新的`File System`缓存机制还不支持，其他都支持。

- 作用
  1. 离线浏览：用户可在没有网络连接时进行`H5`页面访问
  2. 提高页面加载速度 & 减少流量消耗：直接使用已缓存的资源，不需要重新加载
- 具体应用
  此处讲解主要讲解 前端`H5`的缓存机制 的缓存机制 & 缓存模式 ：
  a. 缓存机制：如何将加载过的网页数据保存到本地
  b. 缓存模式：加载网页时如何读取之前保存到本地的网页缓存

> 前者是保存，后者是读取，请注意区别

### 2.1.1 缓存机制

- ```
  Android WebView
  ```

  自带的缓存机制有5种：

  1. 浏览器 缓存机制
  2. `Application Cache` 缓存机制
  3. `Dom Storage` 缓存机制
  4. `Web SQL Database` 缓存机制
  5. `Indexed Database` 缓存机制
  6. `File System` 缓存机制（`H5`页面新加入的缓存机制，虽然`Android WebView`暂时不支持，但会进行简单介绍）

下面将详细介绍每种缓存机制。

## 1. 浏览器缓存机制

##### a. 原理

- 根据 `HTTP` 协议头里的 `Cache-Control`（或 `Expires`）和 `Last-Modified`（或 `Etag`）等字段来控制文件缓存的机制
- 下面详细介绍`Cache-Control`、`Expires`、`Last-Modified` & `Etag`四个字段

1. `Cache-Control`：用于控制文件在本地缓存有效时长

> 如服务器回包：`Cache-Control:max-age=600`，则表示文件在本地应该缓存，且有效时长是600秒（从发出请求算起）。在接下来600秒内，如果有请求这个资源，浏览器不会发出 HTTP 请求，而是直接使用本地缓存的文件。

1. `Expires`：与`Cache-Control`功能相同，即控制缓存的有效时间

> 1. `Expires`是 `HTTP1.0` 标准中的字段，Cache-Control 是 `HTTP1.1` 标准中新加的字段
> 2. 当这两个字段同时出现时，`Cache-Control` 优先级较高

1. `Last-Modified`：标识文件在服务器上的最新更新时间

> 下次请求时，如果文件缓存过期，浏览器通过 If-Modified-Since 字段带上这个时间，发送给服务器，由服务器比较时间戳来判断文件是否有修改。如果没有修改，服务器返回304告诉浏览器继续使用缓存；如果有修改，则返回200，同时返回最新的文件。

1. `Etag`：功能同`Last-Modified` ，即标识文件在服务器上的最新更新时间。

> 1. 不同的是，`Etag` 的取值是一个对文件进行标识的特征字串。
> 2. 在向服务器查询文件是否有更新时，浏览器通过`If-None-Match` 字段把特征字串发送给服务器，由服务器和文件最新特征字串进行匹配，来判断文件是否有更新：没有更新回包304，有更新回包200
> 3. `Etag` 和 `Last-Modified` 可根据需求使用一个或两个同时使用。两个同时使用时，只要满足基中一个条件，就认为文件没有更新。

常见用法是：

- `Cache-Control`与 `Last-Modified` 一起使用；
- `Expires`与 `Etag`一起使用；

即一个用于控制缓存有效时间，一个用于在缓存失效后，向服务查询是否有更新

**特别注意：浏览器缓存机制 是 浏览器内核的机制，一般都是标准的实现**

> 即`Cache-Control`、 `Last-Modified` 、 `Expires`、 `Etag`都是标准实现，你不需要操心

##### b. 特点

- 优点：支持 `Http`协议层
- 不足：缓存文件需要首次加载后才会产生；浏览器缓存的存储空间有限，缓存有被清除的可能；缓存的文件没有校验。

> 对于解决以上问题，可以参考手 Q 的离线包

##### c. 应用场景

静态资源文件的存储，如`JS、CSS`、字体、图片等。

> 1. `Android Webview`会将缓存的文件记录及文件内容会存在当前 app 的 data 目录中。

##### d. 具体实现

`Android WebView`内置自动实现，即不需要设置即实现

> 1. `Android` 4.4后的 `WebView` 浏览器版本内核：`Chrome`
> 2. 浏览器缓存机制 是 浏览器内核的机制，一般都是标准的实现

## 2. Application Cache 缓存机制

##### a. 原理

- 以文件为单位进行缓存，且文件有一定更新机制（类似于浏览器缓存机制）
- `AppCache` 原理有两个关键点：manifest 属性和 manifest 文件。



```xml
<!DOCTYPE html>
<html manifest="demo_html.appcache">
// HTML 在头中通过 manifest 属性引用 manifest 文件
// manifest 文件：就是上面以 appcache 结尾的文件，是一个普通文件文件，列出了需要缓存的文件
// 浏览器在首次加载 HTML 文件时，会解析 manifest 属性，并读取 manifest 文件，获取 Section：CACHE MANIFEST 下要缓存的文件列表，再对文件缓存
<body>
...
</body>
</html>

// 原理说明如下：
// AppCache 在首次加载生成后，也有更新机制。被缓存的文件如果要更新，需要更新 manifest 文件
// 因为浏览器在下次加载时，除了会默认使用缓存外，还会在后台检查 manifest 文件有没有修改（byte by byte)
发现有修改，就会重新获取 manifest 文件，对 Section：CACHE MANIFEST 下文件列表检查更新
// manifest 文件与缓存文件的检查更新也遵守浏览器缓存机制
// 如用户手动清了 AppCache 缓存，下次加载时，浏览器会重新生成缓存，也可算是一种缓存的更新
// AppCache 的缓存文件，与浏览器的缓存文件分开存储的，因为 AppCache 在本地有 5MB（分 HOST）的空间限制
```

##### b. 特点

方便构建`Web App`的缓存

> 专门为 `Web App`离线使用而开发的缓存机制

##### c. 应用场景

存储静态文件（如`JS`、`CSS`、字体文件）

> 1. 应用场景 同 浏览器缓存机制
> 2. 但AppCache 是对 浏览器缓存机制 的补充，不是替代。

##### d. 具体实现



```dart
        // 通过设置WebView的settings来实现
        WebSettings settings = getSettings();

        String cacheDirPath = context.getFilesDir().getAbsolutePath()+"cache/";
        settings.setAppCachePath(cacheDirPath);
        // 1. 设置缓存路径

         settings.setAppCacheMaxSize(20*1024*1024);
        // 2. 设置缓存大小

        settings.setAppCacheEnabled(true);
        // 3. 开启Application Cache存储机制
    
// 特别注意
// 每个 Application 只调用一次 WebSettings.setAppCachePath() 和
 WebSettings.setAppCacheMaxSize()
```

## 3. Dom Storage 缓存机制

##### a. 原理

- 通过存储字符串的 `Key - Value` 对来提供

> `DOM Storage` 分为 `sessionStorage` & `localStorage`； 二者使用方法基本相同，区别在于作用范围不同：
> a. `sessionStorage`：具备临时性，即存储与页面相关的数据，它在页面关闭后无法使用
> b. `localStorage`：具备持久性，即保存的数据在页面关闭后也可以使用。

##### b. 特点

- 存储空间大（ 5MB）：存储空间对于不同浏览器不同，如Cookies 才 4KB
- 存储安全、便捷： `Dom Storage` 存储的数据在本地，不需要经常和服务器进行交互

> 不像 `Cookies`每次请求一次页面，都会向服务器发送网络请求

##### c. 应用场景

存储临时、简单的数据

> 1. 代替 **将 不需要让服务器知道的信息 存储到 `cookies` **的这种传统方法
> 2. `Dom Storage` 机制类似于 `Android` 的 `SharedPreference`机制

##### d. 具体实现



```cpp
        // 通过设置 `WebView`的`Settings`类实现
        WebSettings settings = getSettings();

        settings.setDomStorageEnabled(true);
        // 开启DOM storage
```

## 4. Web SQL Database 缓存机制

##### a. 原理

基于 `SQL` 的数据库存储机制

##### b. 特点

充分利用数据库的优势，可方便对数据进行增加、删除、修改、查询

##### c. 应用场景

存储适合数据库的结构化数据

##### d. 具体实现



```dart
        // 通过设置WebView的settings实现
        WebSettings settings = getSettings();

        String cacheDirPath = context.getFilesDir().getAbsolutePath()+"cache/";
        settings.setDatabasePath(cacheDirPath);
        // 设置缓存路径

        settings.setDatabaseEnabled(true);
        // 开启 数据库存储机制
```

#### 特别说明

- 根据官方说明，`Web SQL Database`存储机制不再推荐使用（不再维护）
- 取而代之的是 `IndexedDB`缓存机制，下面会详细介绍

## 5. IndexedDB 缓存机制

##### a. 原理

属于 `NoSQL` 数据库，通过存储字符串的 `Key - Value` 对来提供

> 类似于 `Dom Storage 存储机制` 的`key-value`存储方式

##### b. 特点

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfm0rg7kj30xc0g7ach.jpg)

优点

##### c. 应用场景

存储 复杂、数据量大的结构化数据

##### d. 具体实现：



```cpp
// 通过设置WebView的settings实现
        WebSettings settings = getSettings();

        settings.setJavaScriptEnabled(true);
        // 只需设置支持JS就自动打开IndexedDB存储机制
        // Android 在4.4开始加入对 IndexedDB 的支持，只需打开允许 JS 执行的开关就好了。
```

## 6 . File System

##### a. 原理

- 为 `H5`页面的数据 提供一个虚拟的文件系统

> 1. 可进行文件（夹）的创建、读、写、删除、遍历等操作，就像 `Native App` 访问本地文件系统一样
> 2. 虚拟的文件系统是运行在沙盒中
> 3. 不同 `WebApp` 的虚拟文件系统是互相隔离的，虚拟文件系统与本地文件系统也是互相隔离的。

- 虚拟文件系统提供了两种类型的存储空间：临时 & 持久性：
  1. 临时的存储空间：由浏览器自动分配，但可能被浏览器回收
  2. 持久性的存储空间：需要显式申请；自己管理（浏览器不会回收，也不会清除内容）；存储空间大小通过配额管理，首次申请时会一个初始的配额，配额用完需要再次申请。

##### b. 特点

- 可存储数据体积较大的二进制数据
- 可预加载资源文件
- 可直接编辑文件

##### c. 应用场景

通过文件系统 管理数据

##### d. 具体使用

由于 `File System`是 `H5` 新加入的缓存机制，所以`Android WebView`暂时不支持

### 缓存机制汇总

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwflwkobej30xc0ar0xp.jpg)

缓存机制汇总

### 使用建议

- 综合上述缓存机制的分析，我们可以根据 需求场景的不同（缓存不同类型的数据场景） 从而选择不同的缓存机制（组合使用）
- 以下是缓存机制的使用建议：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfluoeqdj30xc0jsq51.jpg)

使用建议

------

### 2.1.2 缓存模式

- 定义
  缓存模式是一种 当加载 `H5`网页时 该如何读取之前保存到本地缓存
  从而进行使用 的方式

> 即告诉`Android WebView` 什么时候去读缓存，以哪种方式去读缓存

- `Android WebView` 自带的缓存模式有4种：



```cpp
// 缓存模式说明: 
      // LOAD_CACHE_ONLY: 不使用网络，只读取本地缓存数据
      // LOAD_NO_CACHE: 不使用缓存，只从网络获取数据.
      // LOAD_DEFAULT: （默认）根据cache-control决定是否从网络上取数据。
      // LOAD_CACHE_ELSE_NETWORK，只要本地有，无论是否过期，或者no-cache，都使用缓存中的数据。
```

- 具体使用



```cpp
WebView.getSettings().setCacheMode(WebSettings.LOAD_CACHE_ELSE_NETWORK);
// 设置参数即可
     
```

------

### 2.2 资源预加载

- 定义
  提早加载将需使用的H5页面，即 **提前构建缓存**

> 使用时直接取过来用而不用在需要时才去加载

- 具体实现
  预加载`WebView`对象 & 预加载`H5`资源

### 2.2.1 预加载WebView对象

- 此处主要分为2方面：首次使用的WebView对象 & 后续使用的WebView对象
- 具体如下图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwflsf025j30ua06yjtc.jpg)

示意图

### 2.2.2 预加载H5资源

- 原理

1. 在应用启动、初始化第一个`WebView`对象时，直接开始网络请求加载`H5`页面
2. 后续需打开这些H5页面时就直接从该本地对象中获取

> a. 从而 事先加载常用的H5页面资源（加载后就有缓存了）
> b. 此方法虽然不能减小WebView初始化时间，但数据请求和WebView初始化可以并行进行，总体的页面加载时间就缩短了；缩短总体的页面加载时间：

- 具体实现
  在`Android` 的`BaseApplication`里初始化一个`WebView`对象（用于加载常用的H5页面资源）；当需使用这些页面时再从BaseApplication里取过来直接使用

### 2.2.3 应用场景

对于Android WebView的首页建议使用这种方案，能有效提高首页加载的效率

------

### 2.3 自身构建缓存

为了有效解决 `Android WebView` 的性能问题，除了使用 `Android WebView` 自身的缓存机制，还可以自己针对某一需求场景构建缓存机制。

#### 2.3.1 需求场景

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwflq4zb6j30xc0cmjtw.jpg)

示意图

#### 2.3.2 实现步骤

1. 事先将更新频率较低、常用 & 固定的`H5`静态资源 文件（如`JS`、`CSS`文件、图片等） 放到本地
2. 拦截`H5`页面的资源网络请求 并进行检测
3. 如果检测到本地具有相同的静态资源 就 直接从本地读取进行替换 而 不发送该资源的网络请求 到 服务器获取

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwflp417kj30xc057jsb.jpg)

原理

#### 2.3.3 具体实现

重写`WebViewClient` 的 `shouldInterceptRequest` 方法，当向服务器访问这些静态资源时进行拦截，检测到是相同的资源则用本地资源代替



```kotlin
// 假设现在需要拦截一个图片的资源并用本地资源进行替代

        mWebview.setWebViewClient(new WebViewClient() {
            // 重写 WebViewClient  的  shouldInterceptRequest （）
            // API 21 以下用shouldInterceptRequest(WebView view, String url)
            // API 21 以上用shouldInterceptRequest(WebView view, WebResourceRequest request)
            // 下面会详细说明

             // API 21 以下用shouldInterceptRequest(WebView view, String url)
            @Override
            public WebResourceResponse shouldInterceptRequest(WebView view, String url) {

                // 步骤1:判断拦截资源的条件，即判断url里的图片资源的文件名
                if (url.contains("logo.gif")) {
                // 假设网页里该图片资源的地址为：http://abc.com/imgage/logo.gif
                // 图片的资源文件名为:logo.gif

                    InputStream is = null;
                    // 步骤2:创建一个输入流

                    try {
                        is =getApplicationContext().getAssets().open("images/abc.png");
                        // 步骤3:获得需要替换的资源(存放在assets文件夹里)
                        // a. 先在app/src/main下创建一个assets文件夹
                        // b. 在assets文件夹里再创建一个images文件夹
                        // c. 在images文件夹放上需要替换的资源（此处替换的是abc.png图片）

                    } catch (IOException e) {
                        e.printStackTrace();
                    }

                    // 步骤4:替换资源
                    WebResourceResponse response = new WebResourceResponse("image/png",
                            "utf-8", is);
                    // 参数1：http请求里该图片的Content-Type,此处图片为image/png
                    // 参数2：编码类型
                    // 参数3：存放着替换资源的输入流（上面创建的那个）
                    return response;
                }

                return super.shouldInterceptRequest(view, url);
            }

            
           // API 21 以上用shouldInterceptRequest(WebView view, WebResourceRequest request)
            @TargetApi(Build.VERSION_CODES.LOLLIPOP)
            @Override
            public WebResourceResponse shouldInterceptRequest(WebView view, WebResourceRequest request) {

               // 步骤1:判断拦截资源的条件，即判断url里的图片资源的文件名
                if (request.getUrl().toString().contains("logo.gif")) {
                // 假设网页里该图片资源的地址为：http://abc.com/imgage/logo.gif
                // 图片的资源文件名为:logo.gif

                    InputStream is = null;
                    // 步骤2:创建一个输入流

                    try {
                        is = getApplicationContext().getAssets().open("images/abc.png");
                         // 步骤3:获得需要替换的资源(存放在assets文件夹里)
                        // a. 先在app/src/main下创建一个assets文件夹
                        // b. 在assets文件夹里再创建一个images文件夹
                        // c. 在images文件夹放上需要替换的资源（此处替换的是abc.png图片

                    } catch (IOException e) {
                        e.printStackTrace();
                    }

                    // 步骤4:替换资源
                    WebResourceResponse response = new WebResourceResponse("image/png",
                            "utf-8", is);
                    // 参数1：http请求里该图片的Content-Type,此处图片为image/png
                    // 参数2：编码类型
                    // 参数3：存放着替换资源的输入流（上面创建的那个）
                    return response;
                }
                return super.shouldInterceptRequest(view, request);
            }

    });

}
```

#### 2.3.5 具体实例

下面我将通过 替换主页面（`http:// ip.cn/`）中的一个图片（`http:// s.ip-cdn.com/img/logo.gif`） 来对静态资源拦截 进行说明。

> 为了更好的表现效果，我将替换的图片换成别的图片

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwflmdo38j30xc0imtei.jpg)

实例说明1

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwflkim1wj30wi0clq6m.jpg)

实例说明2

#### 具体步骤 & 代码如下

**步骤1：**定义`WebView`布局
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
    tools:context="scut.carson_ho.webview_interceptrequest.MainActivity">

    <WebView
        android:id="@+id/webview"
        android:layout_width="match_parent"
        android:layout_height="match_parent" />
</RelativeLayout>
```

**步骤2：**进行资源的拦截、检测 & 替换（详细请看注释）
*MainActivity.java*



```kotlin
public class MainActivity extends AppCompatActivity {
    
    WebView mWebview;
    
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        
        mWebview = (WebView) findViewById(R.id.webview);
        // 创建WebView对象

        mWebview.getSettings().setJavaScriptEnabled(true);
        // 支持与JS交互

        mWebview.loadUrl("http://ip.cn/");
        // 加载需要显示的网页

        mWebview.setWebViewClient(new WebViewClient() {

             // 复写shouldInterceptRequest
             //API21以下用shouldInterceptRequest(WebView view, String url)
            @Override
            public WebResourceResponse shouldInterceptRequest(WebView view, String url) {

                // 步骤1:判断拦截资源的条件，即判断url里的图片资源的文件名
                // 此处网页里图片的url为:http://s.ip-cdn.com/img/logo.gif
                // 图片的资源文件名为:logo.gif

                if (url.contains("logo.gif")) {

                    InputStream is = null;
                    // 步骤2:创建一个输入流


                    try {
                        is =getApplicationContext().getAssets().open("images/error.png");
                        // 步骤3:打开需要替换的资源(存放在assets文件夹里)
                        // 在app/src/main下创建一个assets文件夹
                        // assets文件夹里再创建一个images文件夹,放一个error.png的图片

                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    // 步骤4:替换资源

                    WebResourceResponse response = new WebResourceResponse("image/png",
                            "utf-8", is);
                    // 参数1:http请求里该图片的Content-Type,此处图片为image/png
                    // 参数2:编码类型
                    // 参数3:替换资源的输入流

                    System.out.println("旧API");
                    return response;
                }

                return super.shouldInterceptRequest(view, url);
            }


            // API21以上用shouldInterceptRequest(WebView view, WebResourceRequest request)
            @TargetApi(Build.VERSION_CODES.LOLLIPOP)
            @Override
            public WebResourceResponse shouldInterceptRequest(WebView view, WebResourceRequest request) {

                // 步骤1:判断拦截资源的条件，即判断url里的图片资源的文件名
                // 此处图片的url为:http://s.ip-cdn.com/img/logo.gif
                // 图片的资源文件名为:logo.gif
                if (request.getUrl().toString().contains("logo.gif")) {

                    InputStream is = null;
                    // 步骤2:创建一个输入流

                    try {
                        is = getApplicationContext().getAssets().open("images/error.png");
                        // 步骤3:打开需要替换的资源(存放在assets文件夹里)
                        // 在app/src/main下创建一个assets文件夹
                        // assets文件夹里再创建一个images文件夹,放一个error.png的图片

                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    //步骤4:替换资源
                   
                    WebResourceResponse response = new WebResourceResponse("image/png",
                            "utf-8", is);
                    // 参数1：http请求里该图片的Content-Type,此处图片为image/png
                    // 参数2：编码类型
                    // 参数3：存放着替换资源的输入流（上面创建的那个）
                    
                    return response;
                }
                return super.shouldInterceptRequest(view, request);
            }

    });

}
}
```

**步骤3：**加入网络权限
*Manifest.xml*



```xml
 <uses-permission android:name="android.permission.INTERNET"/>
```

## Demo地址

[Carson_Ho的Github地址](https://links.jianshu.com/go?to=https%3A%2F%2Fgithub.com%2FCarson-Ho%2FWebview_Cache)

## 特别注意

关于上述放到本地的静态资源也是可以更新的：

1. 发布新版本安装更新
2. 增量更新：在用户处于`WIFI`环境时让服务器推送到本地

> 很多著名的`App`（如微信）就是采用小范围更新本地资源的

## 这种缓存机制的好处

- 有效解决 `H5`页面静态资源 加载速度慢 & 流量消耗多的问题
- 开发成本低
  1. 没有改变前端`H5`的任何代码，不需要为 APP 做定制化的东西
  2. 该方法只是更好地加快`H5`加载速度，哪怕失效，也不会对`H5`页面产生其他负面影响
- 同样能获得相应的`cookie`
  发送的网络请求会直接带上先前用户操作所留下的 `cookie` 而都能够留下来，因为我们没有更改资源的 URL 地址

------

# 3. 总结

- 本文主要 对`Android WebView` 的性能问题 & 解决方案 进行了全面介绍
- 关于WebView的系列文章希望对你有所帮助
  [Android开发：最全面、最易懂的Webview详解](https://www.jianshu.com/p/3c94ae673e2a)
  [最全面总结 Android WebView与 JS 的交互方式](https://www.jianshu.com/p/345f4d8a5cfa)
  [手把手教你构建 Android WebView 的缓存机制 & 资源预加载方案](https://www.jianshu.com/p/5e7075f4875f)
  [你不知道的 Android WebView 使用漏洞](https://www.jianshu.com/p/3a345d27cd42)
- 接下来我会继续讲解其他安卓开发的知识，感兴趣的同学可以继续关注本人运营的`Wechat Public Account`：
- [我想给你们介绍一个与众不同的Android微信公众号（福利回赠）](https://www.jianshu.com/p/2e92908af6ec)
- [我想邀请您和我一起写Android（福利回赠）](https://www.jianshu.com/p/2c5d57fb054d)

------

#### 请点赞！因为你们的赞同/鼓励是我写作的最大动力！



作者：Carson_Ho
链接：https://www.jianshu.com/p/5e7075f4875f
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。