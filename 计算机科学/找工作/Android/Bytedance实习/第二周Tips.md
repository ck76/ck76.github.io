

- [**LayoutInflater与attachToRoot**](https://www.jianshu.com/p/2989a927f5df)

  - “Inflate的时候，要把最后一个参数设置成false

  ```java
  public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
          VIew view = LayoutInflater.from(context).inflate(R.layout.xxx, container, false);
          return view;
      }
  ```

- **FragmentPagerAdapter的instantiateItem(ViewGroup container, int position)方法**

- **WebView**

- **[@IntDef/@StringDef](https://www.jianshu.com/p/80180e1728d0)**

  - 为了类型安全使用枚举时会有一定的性能损耗，那么我只要将参数限定在某一个类型集合里面
    要将的`@IntDef/@StringDef + @interface`来进行限定参数

- **[@Keep](https://www.jianshu.com/p/9dacabd351e3)**

  - 不被混淆，但是必须配置保留注解

  ```java
  #保留注解，如果不添加改行会导致我们的@Keep注解失效
  -keepattributes *Annotation*
  -keep @android.support.annotation.Keep class **
  ```

- **[tools:overrideLibrary](https://www.jianshu.com/p/d6f0c9822eae)**

  - 在Android项目中，如果我们自己添加的库兼容的最低版本与项目设置的版本不同的时候，在运行项目的时候会出现错误信息提示。

  - 根据提示的建议，我们可以这样修复：在清单文件的`<manifest>`中写上
     `<uses-sdk tools:overrideLibrary="timber.log"/>`，这样问题就解决了。
     如果再添加了一个库，又出现类似提示，则可以用逗号隔开进行多个设置，如：
     `<uses-sdk tools:overrideLibrary="timber.log,[其他的设置]"/>`

- **android:process="com.ss.android.ugc.live"**

  - 指定进程名

- **android:allowBackup="true"**

  - 数据备份

- **[Android M 的＂App Links＂](http://www.jcodecraeer.com/a/anzhuokaifa/androidkaifa/2015/0609/3022.html)**

  - 允许开发者将app和他们的web域名关联。这一举措是为了最小化用户遇到“打开方式”对话框的概率。

- 熟练使用Kotlin中`Collection`类自带的一些方法，替代手动遍历集合

  比如：`forEach()`, `map()`,`mapTo()`, `filter()`, `find()`, `any()`, `reduce()`等等

- 能定义成`val`的变量，定义成`val`，不要定义成`var`

- 熟练使用Kotlin中，每个对象都自带的 `apply()` 和 `let()`方法

- **Early Return**

  ```java
  fun someMethod() {
      if (a != null) {    
          //.....
      }
      //替换为
    	if (a == null) return 
  }
  ```

- **减少临时变量**

  ```kotlin
  val intent = Intent(this, LoginActivity::class.java)
  navigate(intent)
    
  navigate(Intent(this, LoginActivity::class.java))
  ```

- **让变量定义的地方尽量靠近它被使用的地方**

  ```kotlin
  private fun reject(code: Int, message: String) {
      val result = PluginResult()
      Logger.d(TAG, "appInfo url: $message")
      result.resultCode = PluginResult.CODE_FAILED
      result.put(OUTPUT_CODE, code)
      result.put(OUTPUT_MESSAGE, message)
      result.put(OUTPUT_REQUEST_ID, gallery?.id ?: 0)
      gallery?.release()
      gallery = null
      jsBridgeCallback?.sendPluginResult(result)
  }
  //以上的代码中，result变量定义出来之后，并没有直接用，中间插了一句Log（这句log并没有用到这个变量），然后接着对result做了一些设置工作，接着又对gallery对象做了一些操作，最后，终于到了使用这个变量的地方。
  private fun reject(code: Int, message: String) {
      Logger.d(TAG, "appInfo url: $message")
  
      val result = PluginResult().apply {
          resultCode = PluginResult.CODE_FAILED
          put(OUTPUT_CODE, code)
          put(OUTPUT_MESSAGE, message)
          put(OUTPUT_REQUEST_ID, gallery?.id ?: 0)
      }
      jsBridgeCallback?.sendPluginResult(result)
  
      gallery?.release()
      gallery = null
  }
  ```

- **表达式和语句**

  - 表达式是可以被求值的代码，而语句是一段可执行代码。

    因为**表达式可被求值**，所以它可写在赋值语句等号的右侧。
    而**语句不一定有值**，所以像import、for和break等语句就不能被用于赋值。

- **[DialogFragment](https://www.cnblogs.com/mercuryli/p/5372496.html)**

  - 新来的DialogFragment让dialog也变成了碎片，相比之前来说应该做了很多优化和处理，对于程序员来看对话框现在更加具体了，就是一个activity上的一个fragmen

  - 之前的dialog如果这个时候屏幕方向发生变化，就会导致Activity重建，然后之前显示的对话框就不见了。当然我们也可以无视这个错误，因为程序不会因此崩溃（看来android本身就已经预料到这种情况了）。

    如果我们想要在旋转屏幕的时候也能保证这个对话框显示就需要做一定的处理了，在activity要销毁前设立一个标志，看这时对话框是否是显示状态，如果是那么activity在下次建立时直接显示对话框。

    使用DialogFragment来管理对话框 就不会有这种问题了，代码也少了很多的逻辑处理。当你旋转屏幕的时候，fragmentManager会自定管理DialogFragment的生命周 期，如果当前已经显示在屏幕上了，那么旋转屏幕后也会自动显示。

    ```java
    new DialogFragmentTest()
            .show(getFragmentManager(), "dialog_fragment");
    
    //回顾下fragment是怎么使用的。
    ① 建立FragmentManager对象，用来管理fragment
    ② 建立fragmentTransaction对象，用来添加和fragment
    ③ 提交fragment切换（commit）
    
    //show方法还有另一种形式，直接传FragmentTransaction
     public void show(FragmentManager manager, String tag){
            mDismissed = false; 
            mShownByMe = true; 
            FragmentTransaction ft = manager.beginTransaction(); // creat a fragmentTransaction
            ft.add(this, tag); // add fragment with tag
            ft.commit(); 
        } 
    ```


- **session 、cookie、token的区别**

  - **session(服务器存储)**

    session的中文翻译是“会话”，当用户打开某个**web应用**时，便**与web服务器产生一次session**。服务器使用session把用户的信息临时保存在了**服务器上**，用户离开网站后session会被销毁。这种用户信息存储方式相对cookie来说更安全，可是session有一个缺陷：如果web服务器做了负载均衡，那么下一个操作请求到了另一台服务器的时候session会丢失。

  - **cookie(浏览器存储)**

    cookie是**保存在本地终端的数据**。cookie**由服务器生成**，发送给浏览器，浏览器把cookie以kv形式保存到某个目录下的文本文件内，下一次请求同一网站时会把该cookie发送给服务器。由于cookie是存在客户端上的，所以浏览器加入了一些限制确保cookie不会被恶意使用，同时不会占据太多磁盘空间，所以每个域的cookie数量是有限的。

  - **token(避免多次查库)**

    token的意思是“令牌”，是用户身份的验证方式，最简单的token组成:**uid(用户唯一的身份标识)、time(当前时间的时间戳)、sign(**签名，由token的前几位+盐以哈希算法压缩成一定长的十六进制字符串，可以防止恶意第三方拼接token请求服务器)。还可以把不变的参数也放进token，**避免多次查库**

- **cookie 和session的区别**

  1、cookie数据存放在客户的浏览器上，session数据放在服务器上。

  2、cookie不是很安全，别人可以分析存放在本地的COOKIE并进行COOKIE欺骗
   考虑到安全应当使用session。

  3、session会在一定时间内保存在服务器上。当访问增多，会比较占用你服务器的性能
   考虑到减轻服务器性能方面，应当使用COOKIE。

  4、单个cookie保存的数据不能超过4K，很多浏览器都限制一个站点最多保存20个cookie。

  5、将登陆信息等重要信息存放为SESSION，其他信息如果需要保留，可以放在COOKIE中

- **token和session的区别**

    session 和 oauth token并不矛盾，作为**身份认证 token安全性比session好**，因为每个请求都有签名还能防止监听以及重放攻击，而session就必须靠链路层来保障通讯安全了。如上所说，如果你需要实现有状态的会话，仍然可以增加session来在服务器端保存一些状态

    App通常用restful api跟server打交道。Rest是stateless的，也就是app不需要像browser那样用cookie来保存session,因此用session token来标示自己就够了，session/state由api server的逻辑处理。 如果你的后端不是stateless的rest api, 那么你可能需要在app里保存session.可以在app里嵌入webkit,用一个隐藏的browser来管理cookie session.


     Session 是一种HTTP存储机制，目的是为无状态的HTTP提供的持久机制。所谓Session 认证只是简单的把User 信息存储到Session 里，因为SID 的不可预测性，暂且认为是安全的。这是一种认证手段。 而Token ，如果指的是OAuth Token 或类似的机制的话，提供的是 认证 和 授权 ，认证是针对用户，授权是针对App 。其目的是让 某App有权利访问 某用户 的信息。这里的 Token是唯一的。不可以转移到其它 App上，也不可以转到其它 用户 上。 转过来说Session 。Session只提供一种简单的认证，即有此 SID，即认为有此 User的全部权利。是需要严格保密的，这个数据应该只保存在站方，不应该共享给其它网站或者第三方App。 所以简单来说，如果你的用户数据可能需要和第三方共享，或者允许第三方调用 API 接口，用 Token 。如果永远只是自己的网站，自己的 App，用什么就无所谓了。
    
    token就是令牌，比如你授权（登录）一个程序时，他就是个依据，判断你是否已经授权该软件；cookie就是写在客户端的一个txt文件，里面包括你登录信息之类的，这样你下次在登录某个网站，就会自动调用cookie自动登录用户名；session和cookie差不多，只是session是写在服务器端的文件，也需要在客户端写入cookie文件，但是文件里是你的浏览器编号.Session的状态是存储在服务器端，客户端只有session id；而Token的状态是存储在客户端。

- **[什么是RESTful API](https://www.jianshu.com/p/6baf8554b3f4?from=timeline&isappinstalled=0)**

  - REST，即Representational State Transfer的缩写。直接翻译的意思是"表现层状态转化"。
    它是一种互联网应用程序的API设计理念：URL定位资源，用HTTP动词（GET,POST,DELETE,DETC）描述操作。
  - 服务器上每一种资源，比如一个文件，一张图片，一部电影，都有对应的url地址，如果我们的客户端需要对服务器上的这个资源进行操作，就需要通过http协议执行相应的动作来操作它，比如进行获取，更新，删除。
  - 简单来说就是**url地址中只包含名词表示资源，使用http动词表示动作进行操作资源**
    举个例子：左边是错误的设计，而右边是正确的

  ```http
  GET /blog/getArticles --> GET /blog/Articles  获取所有文章
  GET /blog/addArticles --> POST /blog/Articles  添加一篇文章
  GET /blog/editArticles --> PUT /blog/Articles  修改一篇文章 
  GET /rest/api/deleteArticles?id=1 --> DELETE /blog/Articles/1  删除一篇文章
  ```

- **[URL Schema](https://blog.csdn.net/ruingman/article/details/70054670)**

  - Android中的scheme是一种页面内跳转协议，是一种非常好的实现机制，通过定义自己的scheme协议，可以非常方便跳转app中的各个页面；通过scheme协议，服务器可以定制化告诉App跳转那个页面，可以通过通知栏消息定制化跳转页面，可以通过H5页面跳转页面等。

  - **URL Schema应用场景：** 

      客户端应用可以向操作系统注册一个 URL scheme，该 scheme 用于从浏览器或其他应用中启动本应用。通过指定的 URL 字段，可以让应用在被调起后直接打开某些特定页面，比如商品详情页、活动详情页等等。也可以执行某些指定动作，如完成支付等。也可以在应用内通过 html 页来直接调用显示 app 内的某个页面。综上URL Schema使用场景大致分以下几种：

    - 服务器下发跳转路径，客户端根据服务器下发跳转路径跳转相应的页面
    - H5页面点击锚点，根据锚点具体跳转路径APP端跳转具体的页面
    - APP端收到服务器端下发的PUSH通知栏消息，根据消息的点击跳转路径跳转相关页面
    - APP根据URL跳转到另外一个APP指定页面 

  - URL Schema协议格式：

    ```java
    xl://goods:8888/goodsDetail?goodsId=10011002 
    ```

    通过上面的路径 Schema、Host、port、path、query全部包含，基本上平时使用路径就是这样子的。

    - xl代表该Schema 协议名称
    - goods代表Schema作用于哪个地址域
    - goodsDetail代表Schema指定的页面
    - goodsId代表传递的参数
    - 8888代表该路径的端口号 

- **[URL跟URi的区别]([https://zh.wikipedia.org/wiki/%E7%BB%9F%E4%B8%80%E8%B5%84%E6%BA%90%E6%A0%87%E5%BF%97%E7%AC%A6](https://zh.wikipedia.org/wiki/统一资源标志符))**

  - [通俗讲解](https://blog.csdn.net/simplebam/article/details/72644094)
- **URI强调的是给资源标记命名，URL强调的是给资源定位**
  - URI可被视为定位符（URL），名称（URN）或两者兼备。[统一资源名](https://zh.wikipedia.org/wiki/统一资源名)（URN）如同一个人的名称，而[统一资源定位符](https://zh.wikipedia.org/wiki/统一资源定位符)（URL）代表一个人的住址。换言之，**URN定义某事物的身份，而URL提供查找该事物的方法。**
- URI标记了一个**网络资源**，仅此而已；  URL标记了一个WWW互联网资源（用地址标记），**并给出了他的访问地址**。(URI是Uniform Resource Identifier,表示是一个资源； URL是Uniform Resource Locator，表示是一个地址)
  
  ![URI/URN/URL](<http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/mgTetTumJ63l*vP*.IZKXJOAd*f3xRo*eJDRUlMhtEM!/r/dDUBAAAAAAAA>)

  - 统一资源定位符(uniform resource locator **URL**)

    ```java
    统一资源定位符的标准格式如下：
    协议类型:[//服务器地址[:端口号]][/资源层级UNIX文件路径]文件名[?查询][#片段ID]
    
    统一资源定位符的完整格式如下：
    协议类型:[//[访问资源需要的凭证信息@]服务器地址[:端口号]][/资源层级UNIX文件路径]文件名[?查询][#片段ID]
    ```

    - [传送协议](https://zh.wikipedia.org/wiki/統一資源標誌符方案)。
    - 层级URL标记符号(为[//],固定不变)
    - 访问资源需要的凭证信息（可省略）
    - 服务器。（通常为[域名](https://zh.wikipedia.org/wiki/域名)，有时为[IP地址](https://zh.wikipedia.org/wiki/IP地址)）
    - 端口号。（以数字方式表示，若为默认值可省略）
    - 路径。（以“/”字符区别路径中的每一个目录名称）
    - 查询。（GET模式的窗体参数，以“?”字符为起点，每个参数以“&”隔开，再以“=”分开参数名称与数据，通常以UTF8的URL编码，避开字符冲突的问题）
    - 片段。以“#”字符为起点[[2\]](https://zh.wikipedia.org/wiki/统一资源定位符#cite_note-2)

    ```java
    以http://zh.wikipedia.org:80/w/index.php?title=Special:%E9%9A%8F%E6%9C%BA%E9%A1%B5%E9%9D%A2 为例, 其中：
    ```

    1. **http**，是协议；
    2. **zh.wikipedia.org**，是服务器；
    3. **80**，是服务器上的网络端口号；
    4. **/w/index.php**，是路径；
    5. **?title=Special:%E9%9A%8F%E6%9C%BA%E9%A1%B5%E9%9D%A2**，是询问。

    - 大多数[网页浏览器](https://zh.wikipedia.org/wiki/网页浏览器)不要求用户输入[网页](https://zh.wikipedia.org/wiki/网页)中“**http://**”的部分，因为绝大多数网页内容是超文本传输协议文件。同样，“**80**”是超文本传输协议文件的常用端口号，因此一般也不必写明。一般来说用户只要键入统一资源定位符的一部分（**zh.wikipedia.org/wiki/Special:%E9%9A%8F%E6%9C%BA%E9%A1%B5%E9%9D%A2**）就可以了。

    - 由于超文本传输协议允许服务器将浏览器重定向到另一个网页地址，因此许多服务器允许用户省略网页地址中的部分，比如 **www**。从技术上来说这样省略后的网页地址实际上是一个不同的网页地址，浏览器本身无法决定这个新地址是否通，服务器必须完成重定向的任务。

    - **统一资源定位符不但被用作网页地址**，[JDBC](https://zh.wikipedia.org/wiki/JDBC) [客户端](https://zh.wikipedia.org/wiki/客户端)也使用统一资源定位符连接其数据库服务器。作为对比，[ODBC](https://zh.wikipedia.org/wiki/ODBC) 的连接字符串作用相同，但并不采用 URL 格式，而是分号和等号分隔的键值对。

      以下是一个 [Oracle](https://zh.wikipedia.org/wiki/Oracle) 数据库的统一资源定位符:

      ```java
      jdbc:datadirect:oracle://myserver:1521;sid=testdb
      ```

  - 和统一资源标识符(uniform resource identifier **URI**)

    - 在[计算机](https://zh.wikipedia.org/wiki/電腦)术语中，**统一资源标识符**（英语：**Uniform Resource Identifier**，[缩写](https://zh.wikipedia.org/wiki/縮寫)：**URI**）是一个用于[标识](https://zh.wikipedia.org/wiki/标识)某一[互联网](https://zh.wikipedia.org/wiki/互联网)[资源](https://zh.wikipedia.org/wiki/资源)名称的[字符串](https://zh.wikipedia.org/wiki/字符串)。

      该种标识允许用户对网络中（一般指[万维网](https://zh.wikipedia.org/wiki/万维网)）的资源通过特定的[协议](https://zh.wikipedia.org/wiki/網絡傳輸協議)进行交互操作。URI的最常见的形式是[统一资源定位符](https://zh.wikipedia.org/wiki/统一资源定位符)（URL），经常指定为非正式的网址。更罕见的用法是[统一资源名称](https://zh.wikipedia.org/wiki/统一资源名称)（URN），其目的是通过提供一种途径。用于在特定的[名字空间](https://zh.wikipedia.org/wiki/命名空间)资源的标识，以补充网址。

    ```java
                        hierarchical part
            ┌───────────────────┴─────────────────────┐
                        authority               path
            ┌───────────────┴───────────────┐┌───┴────┐
      abc://username:password@example.com:123/path/data?key=value&key2=value2#fragid1
      └┬┘   └───────┬───────┘ └────┬────┘ └┬┘           └─────────┬─────────┘ └──┬──┘
    scheme  user information     host     port                  query         fragment
    
      urn:example:mammal:monotreme:echidna
      └┬┘ └──────────────┬───────────────┘
    scheme              path
    ```

- **Android ID**

  - **UUID(Universally Unique Identifier)**
    
  - 唯一标识
    
  - **Device ID**
    - DEVICE_ID是设备ID标识，用于唯一标识设备，这个ID似乎并非是独立的一串数字，而会由于终端的硬件配置不同，所取到的结果不同。
    - 比如GSM手机DEVICE_ID可能是IMEI号，CDMA手机可能是MEID，不带MODEM的手机可能会返回NULL，也可能返回其它唯一值，比如MAC地址等。

  - **Android ID(安卓设备ID)**

    - ANDROID_ID，当设备被wipe后该值会被重置。 
      厂商定制系统的Bug：不同的设备可能会产生相同的ANDROID_ID：9774d56d682e549c。 
      厂商定制系统的Bug：有些设备返回的值为null。 
      设备差异：对于CDMA设备，ANDROID_ID和TelephonyManager.getDeviceId() 返回相同的值。 
      它在Android <=2.1 or Android >=2.3的版本是可靠、稳定的，但在2.2的版本并不是100%可靠的。 

      通常被认为不可信，因为它有时为null。开发文档中说明了：这个ID会改变如果进行了出厂设置。并且，如果某个Andorid手机被Root过的话，这个ID也可以被任意改变。

    ```java
    public static String getAndroidId (Context context) {
        String ANDROID_ID = Settings.System.getString(context.getContentResolver(), Settings.System.ANDROID_ID);
        return ANDROID_ID;
    }
    ```

  - **IMEI/MEID(手机串号)**

    - IMEI码是手机的国际身份编码，MEID码是电信独有的编码

    - Android系统为开发者提供的用于标识手机设备的串号，也是各种方法中普适性较高的，可以说几乎所有的设备都可以返回这个串号，并且唯一性良好。它根据不同的手机设备返回IMEI，MEID或者ESN码。 

    - **缺陷：** 
      非手机设备： 如果只带有Wifi的设备或者音乐播放器没有通话的硬件功能的话就没有这个DEVICE_ID； 
      权限： 获取DEVICE_ID需要READ_PHONE_STATE权限； 
      在少数的一些手机设备上，该实现有漏洞，会返回垃圾，如:zeros或者asterisks的产品； 

      模拟器上可以刷IMEI。

    ```java
    //获取代码如下：
    TelephonyManager TelephonyMgr =(TelephonyManager)getSystemService(TELEPHONY_SERVICE);
    String szImei = TelephonyMgr.getDeviceId();
    需要权限<uses-permissionandroid:name="android.permission.READ_PHONE_STATE" />
    ```

  - **IMSI**

    - IMSI也叫订阅号Subscriber ID。IMSI和IMEI/MEID联合登录网络，由GSM/3G网络负责映射为我们的手机号。相关获取及管理工作可参考Android TelephonyManager部分。

  - **Subscriber ID (IMSI)(识别码)**

  - **SIM Card Serial(SIM卡串行)**

    - 手机SIM卡唯一标识 
      装有SIM卡的Android 2.3设备，可以通过下面的方法获取到Sim Serial Number，对于CDMA设备，返回的是一个空值。

    ```java
    public static String getSimId (Context context) {
        TelephonyManager tm = (TelephonyManager)context.getSystemService(Context.TELEPHONY_SERVICE);
        String SimSerialNumber = tm.getSimSerialNumber();
        return SimSerialNumber;
    }
    ```

  - **WiFi MAC Address(WiFi MAC地址)**

    - 可以使用手机Wifi或蓝牙的MAC地址作为设备标识。 
      硬件限制：并不是所有的设备都有Wifi和蓝牙硬件，硬件不存在自然也就得不到这一信息。 
      添加权限：ACCESS_WIFI_STATE 

      获取到的，不是一个真实的地址，而且这个地址能轻易地被伪造。wLan不必打开，就可读取些值。

    - 需要WIFI_STATE权限

    ```java
    public static String getMac (Context context) {
        //wifi mac地址
        WifiManager wifi = (WifiManager) context.getSystemService(Context.WIFI_SERVICE);
        WifiInfo info = wifi.getConnectionInfo();
        String wifiMac = info.getMacAddress();
        if(!isEmpty(wifiMac)){
        }
        return wifiMac;
    }
    ```

  - **Ethernet MAC Address(以太网地址)**

- **Gradle Offline work**

  就是gradle在解析依赖的时候采用本地的依赖库（如 GRADLE_USER_HOME指定的路径），而不是依据项目build.gradle文件中设置的仓库地址进行解析。这样就可以很快速的完成解析工作，而不至于花大量时间连接依赖仓库。但是采用这种模式有一个前提：项目所有的依赖都已经存在本地依赖库中。所以，若是项目中有新增或者修改依赖，则不能采用offline模式因为此时本地仓库中可能不存在项目需要的依赖。总之，只要明确本地仓库中已经拥有项目的所有依赖，即可以采用离线模式进行编译项目等操作。

- **implementation api compile**

  - api和compile关键字作用效果是一样的，使用时可以互相替换。实际上，api关键字是用来替代compile关键字的，因为compile关键字将来会被弃用。
  - 在同一个module下，implementation和compile的使用效果相同，但是在不同module下，就会有所区别了。
  - **api或compile关键字引用的包对于其他module来说是可见的，而implementation关键字引用的包对于其他module来说是不可见的。**主要是为了加快编译速度

- **AS能运行但是报红，File--Invalidate Caches/Restart**

- **git commit -n -m"info"**
  
  - checkstyle没过，此命令忽略checkstyle