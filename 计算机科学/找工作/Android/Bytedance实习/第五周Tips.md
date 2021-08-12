- **SurfaceView、TextureView、SurfaceTexture**

  - **SurfaceView是一个有自己独立Surface的View, 它的渲染可以放在单独线程而不是主线程中, 其缺点是不能做变形和动画。**

  - **SurfaceTexture可以用作非直接输出的内容流，这样就提供二次处理的机会。**与SurfaceView直接输出相比，这样会有若干帧的延迟。同时，由于它本身管理BufferQueue，因此[内存](http://cpro.baidu.com/cpro/ui/uijs.php?app_id=0&c=news&cf=1001&ch=0&di=128&fv=18&is_app=0&jk=ddd62cbeae8a0ad1&k=%C4%DA%B4%E6&k0=%C4%DA%B4%E6&kdi0=0&luki=6&n=10&p=baidu&q=65035100_cpr&rb=0&rs=1&seller_id=1&sid=d10a8aaebe2cd6dd&ssp2=1&stid=0&t=tpclicked3_hc&tu=u1836545&u=http%3A%2F%2Fwww%2Ebubuko%2Ecom%2Finfodetail%2D656030%2Ehtml&urlid=0)消耗也会稍微大一些。

  - **TextureView是一个可以把内容流作为外部纹理输出在上面的View, 它本身需要是一个硬件加速层。**

    事实上TextureView本身也包含了SurfaceTexture, 它与SurfaceView+SurfaceTexture组合相比可以完成类似的功能（即把内容流上的图像转成纹理，然后输出）, 区别在于TextureView是在View hierachy中做绘制，因此一般它是在主线程上做的（在Android 5.0引入渲染线程后，它是在渲染线程中做的）。而SurfaceView+SurfaceTexture在单独的Surface上做绘制，可以是用户提供的线程，而不是系统的主线程或是渲染线程。另外，与TextureView相比，它还有个好处是可以用Hardware overlay进行显示

  - https://www.cnblogs.com/wytiger/p/5693569.html

  - [SurfaceView详解](https://blog.csdn.net/TuGeLe/article/details/79199119)

  - [SurfaceView使用详解](https://blog.csdn.net/android_cmos/article/details/68955134)

- [**react，flux，redux这些概念的关系**](https://www.cnblogs.com/dreamingbaobei/p/8476984.html)

- **Kotlin函数引用**

  - 为了防止作用域混淆 ， **::** 调用的函数如果是类的成员函数或者是扩展函数，必须使用限定符,比如this

  ```kotlin
  fun main() {
      //lambda实质上就是匿名函数
      println(lock("param1", "param2") { str1: String, str2: String ->
          "result is {$str1 , $str2}"
      })
  
      println(lock("param1","param2",::getResult))
  }
  
  fun getResult(str1: String, str2: String): String = "result is {$str1 , $str2}"
  
  fun lock(p1: String, p2: String, method: (str1: String, str2: String) -> String): String {
      return method(p1, p2)
  }
  //打印结果
  result is {param1 , param2}
  result is {param1 , param2}
  ```

- **[函数式编程特性](https://baike.baidu.com/item/函数式编程/4035031?fr=aladdin#4)**

  - **闭包和高阶函数**

    函数编程支持函数作为第一类对象，有时称为闭包或者[仿函数](https://baike.baidu.com/item/仿函数)（functor）对象。实质上，闭包是起函数的作用并可以像对象一样操作的对象。与此类似，FP 语言支持高阶函数。高阶函数可以用另一个函数（间接地，用一个[表达式](https://baike.baidu.com/item/表达式)） 作为其输入参数，在某些情况下，它甚至返回一个函数作为其输出参数。这两种结构结合在一起使得可以用优雅的方式进行模块化编程，这是使用 FP 的最大好处。

  - **惰性计算**

    除了高阶函数和[仿函数](https://baike.baidu.com/item/仿函数)（或闭包）的概念，FP 还引入了[惰性计算](https://baike.baidu.com/item/惰性计算)的概念。在惰性计算中，[表达式](https://baike.baidu.com/item/表达式)不是在绑定到变量时立即计算，而是在求值程序需要产生表达式的值时进行计算。延迟的计算使您可以编写可能潜在地生成无穷输出的函数。因为不会计算多于程序的其余部分所需要的值，所以不需要担心由无穷计算所导致的 out-of-memory 错误。一个惰性计算的例子是生成无穷 Fibonacci 列表的函数，但是对第n个Fibonacci 数的计算相当于只是从可能的无穷列表中提取一项。

  - **递归**

    FP 还有一个特点是用[递归](https://baike.baidu.com/item/递归)做为控制流程的机制。例如，Lisp 处理的列表定义为在头元素后面有子列表，这种表示法使得它自己自然地对更小的子列表不断递归。

    函数式编程具有五个鲜明的特点。

  - **函数是"第一等公民"**

    所谓"第一等公民"（first class），指的是函数与其他数据类型一样，处于平等地位，可以赋值给其他变量，也可以作为参数，传入另一个函数，或者作为别的函数的返回值。

  - **只用"表达式"，不用"语句"**

    "表达式"（expression）是一个单纯的运算过程，总是有返回值；"语句"（statement）是执行某种操作，没有返回值。函数式编程要求，只使用表达式，不使用语句。也就是说，每一步都是单纯的运算，而且都有返回值。

    原因是函数式编程的开发动机，一开始就是为了处理运算（computation），不考虑系统的读写（I/O）。"语句"属于对系统的读写操作，所以就被排斥在外。

    当然，实际应用中，不做I/O是不可能的。因此，编程过程中，函数式编程只要求把I/O限制到最小，不要有不必要的读写行为，保持计算过程的单纯性。

  - **没有"副作用"**

    所谓"副作用"（side effect），指的是函数内部与外部互动（最典型的情况，就是修改全局变量的值），产生运算以外的其他结果。

    函数式编程强调没有"副作用"，意味着函数要保持独立，所有功能就是返回一个新的值，没有其他行为，尤其是不得修改外部变量的值。

  - **不修改状态**

    上一点已经提到，函数式编程只是返回新的值，不修改系统变量。因此，不修改变量，也是它的一个重要特点。

    在其他类型的语言中，变量往往用来保存"状态"（state）。不修改变量，意味着状态不能保存在变量中。函数式编程使用参数保存状态，最好的例子就是递归。下面的代码是一个将字符串逆序排列的函数，它演示了不同的参数如何决定了运算所处的"状态"。

  - **引用透明性**

    函数程序通常还加强*引用透明性*，即如果提供同样的输入，那么函数总是返回同样的结果。就是说，表达式的值不依赖于可以改变值的全局状态。这使您可以从形式上推断程序行为，因为表达式的意义只取决于其子表达式而不是计算顺序或者其他表达式的副作用。这有助于验证正确性、简化算法，甚至有助于找出优化它的方法。

  - **副作用**

    副作用是修改系统状态的语言结构。因为 FP 语言不包含任何[赋值语句](https://baike.baidu.com/item/赋值语句)，变量值一旦被指派就永远不会改变。而且，调用函数只会计算出结果 ── 不会出现其他效果。因此，FP 语言没有副作用。

- [**View.post()**](http://www.cnblogs.com/dasusu/p/8047172.html)

  1. **View.post(Runnable) 内部会自动分两种情况处理，当 View 还没 attachedToWindow 时，会先将这些 Runnable 操作缓存下来；否则就直接通过 mAttachInfo.mHandler 将这些 Runnable 操作 post 到主线程的 MessageQueue 中等待执行。**
  2. **如果 View.post(Runnable) 的 Runnable 操作被缓存下来了，那么这些操作将会在 dispatchAttachedToWindow() 被回调时，通过 mAttachInfo.mHandler.post() 发送到主线程的 MessageQueue 中等待执行。**
  3. **mAttachInfo 是 ViewRootImpl 的成员变量，在构造函数中初始化，Activity View 树里所有的子 View 中的 mAttachInfo 都是 ViewRootImpl.mAttachInfo 的引用。**
  4. **mAttachInfo.mHandler 也是 ViewRootImpl 中的成员变量，在声明时就初始化了，所以这个 mHandler 绑定的是主线程的 Looper，所以 View.post() 的操作都会发送到主线程中执行，那么也就支持 UI 操作了。**
  5. **dispatchAttachedToWindow() 被调用的时机是在 ViewRootImol 的 performTraversals() 中，该方法会进行 View 树的测量、布局、绘制三大流程的操作。**
  6. **Handler 消息机制通常情况下是一个 Message 执行完后才去取下一个 Message 来执行（异步 Message 还没接触），所以 View.post(Runnable) 中的 Runnable 操作肯定会在 performMeaure() 之后才执行，所以此时可以获取到 View 的宽高。**

- **[一像素保活，锁屏死，解锁开](https://www.jianshu.com/p/2c4f2586ed6f)**

  - 第一步，新建一个Activity，作为1像素页面的主体，我姑且叫它HooliganActivity:

  ```java
  public class HooliganActivity extends Activity {
      private static HooliganActivity instance;
  
      @Override
      protected void onCreate(Bundle savedInstanceState) {
          super.onCreate(savedInstanceState);
          instance = this;
          Window window = getWindow();
          window.setGravity(Gravity.LEFT | Gravity.TOP);
          WindowManager.LayoutParams params = window.getAttributes();
          params.x = 0;
          params.y = 0;
          params.height = 1;
          params.width = 1;
          window.setAttributes(params);
      }
  
      /**
       * 开启保活页面
       */
      public static void startHooligan() {
          Intent intent = new Intent(DWApplication.getAppContext(), HooliganActivity.class);
          intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
          DWApplication.getAppContext().startActivity(intent);
      }
  
      @Override
      protected void onDestroy() {
          super.onDestroy();
          instance = null;
      }
  
      /**
       * 关闭保活页面
       */
      public static void killHooligan() {
          if(instance != null) {
              instance.finish();
          }
      }
  }
  ```

  - 第二步，注册清单文件：

  ```xml
          <activity android:name=".activity.HooliganActivity"
              android:configChanges="keyboardHidden|orientation|screenSize|navigation|keyboard"
              android:excludeFromRecents="true"
              android:exported="false"
              android:finishOnTaskLaunch="false"
              android:launchMode="singleInstance"
              android:theme="@style/HooliganActivityStyle"/>
      <style name="HooliganActivityStyle">
          <item name="android:windowBackground">@color/transparent</item>
          <item name="android:windowContentOverlay">@null</item>
          <item name="android:windowIsTranslucent">true</item>
          <item name="android:windowNoDisplay">false</item>
          <item name="android:windowDisablePreview">true</item>
      </style>
  ```

  - 第三步，监听锁屏和解锁通知，不能静态注册广播，只能动态注册：

  ```java
  IntentFilter filter = new IntentFilter();
  filter.addAction(Intent.ACTION_SCREEN_ON);
  filter.addAction(Intent.ACTION_SCREEN_OFF);
  registerReceiver(new BootCompleteReceiver(),filter);
  ```

  - 第四步，分别在解锁和锁屏时唤醒我的HooliganActivity：

  ```java
  public class BootCompleteReceiver extends BroadcastReceiver {
      @Override
      public void onReceive(Context context, Intent intent) {
          if(intent.getAction().equals(Intent.ACTION_SCREEN_OFF)) {
              HooliganActivity. startHooligan();
          } else if(intent.getAction().equals(Intent.ACTION_SCREEN_ON)){
              HooliganActivity. killHooligan();
          }
      }
  }
  ```

  这样你在后台每次锁屏，实际上都会吊起一个一像素的页面，假装app在前台，拥有最高进程优先级。

- **账号保活，谷歌账号同步机制**

  - 谷歌允许定时同步，拉起来应用

- **FCM、DCM、Google Service**