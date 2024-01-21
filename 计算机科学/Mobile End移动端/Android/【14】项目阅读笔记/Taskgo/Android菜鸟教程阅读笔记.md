---
title: Android菜鸟教程阅读笔记
date: 2018-10-11 14:42:06
categories: "Android项目笔记"
tags:
---

http://www.runoob.com/w3cnote/android-tutorial-intro.html

**按需学习**，而不是一个收破烂的，收藏一堆链接，收藏了你就会了，其实收集得越多，你会越浮躁！ 

<!--more-->

## 1.基础入门

### 1.1工程各种资源文件

#### res资源id

res资源文佳佳下的东西都会在R.java下生成对应的资源id，然而还有一个文件夹是assets目录，这个文件需要自己创建，asserts文件夹下的文件并不会前者我们可以直接通过资源id访问到对应的资源；而后者则需要我们通过AssetManager以二进制流的形式来读取！对了，这个R文件可以理解为字典，res下每个资源都都会在这里生成一个唯一的id！ 

#### values目录：

> - demens.xml：定义尺寸资源
> - string.xml：定义字符串资源
> - styles.xml：定义样式资源
> - colors.xml：定义颜色资源
> - arrays.xml：定义数组资源
> - attrs.xml：自定义控件时用的较多，自定义控件的属性！
> - theme主题文件，和styles很相似，但是会对整个应用中的Actvitiy或指定Activity起作用，一般是改变窗口外观的！可在Java代码中通过setTheme使用，或者在Androidmanifest.xml中为<application...>添加theme的属性！ PS:你可能看到过这样的values目录：values-w820dp，values-v11等，前者w代表平板设备，820dp代表屏幕宽度；而v11这样代表在API(11)，即android 3.0后才会用到的！

#### raw目录

在接着说下这个raw目录： 用于存放各种原生资源(音频，视频，一些XML文件等)，我们可以通过openRawResource(int id)来获得资源的二进制流！其实和Assets差不多，不过这里面的资源会在R文件那里生成一个资源id而已

#### 动画目录

最后还有个动画的，动画有两种：属性动画和补间动画：

> - animator：存放属性动画的XML文件
> - anim：存放补间动画的XML文件

#### xmlns命名空间

xmlns：android和xmlns：tools两句有了就可以提示了，并且不需要联网

在布局文件中可以看到：

```java
xmlns:android=""
xmlns:tools=""
tools:context=".xxxActivity"
```

三个属性；

`其中xmlns全称xmlnamespace(命名空间)`

命名空间的意义：因为xml的格式本身可以自由定义，每个人都可以写一套自己的格式规则，那么问题来了，别人的解析器怎么识别相不同的xml；并且Android自身有一套自己的定义规则，有对应的解析器。其实可以把命名空间当作是这种XML格式的名字，随便起一个不和其它的XML不起冲突的名字就行，而一般大公司的做法是，填上一个网址，对应网址有讲明相应格式的用法，即命名了，又让别人可以查询对应xml使用的格式，所以算是一举两得。

```tcl
1.xmlns:android=""
这一属性不能去掉。
2.xmlns:tools=""
tools:context=".xxxActivity"
这两上是一起的，可把xmlns:tools=""当作是tools:context=""的命名空间，如果没有xmlns:tools=""
直接使用tools:context=""会提示Erroe parsing XML,unbound prefix(解析出错，未约束的前缀)，
意思解析器不知道tools这个前缀是啥意思，因为你没有提供相应的命令空间来提示解析器。
```

**tools:context=”.xxxActivity”作用：**

让Layout Editor知道当前布局对应哪一个Activity，来更好的显示预览界面，比如当前使用了一个主题，或者使用了一个ActionBar，就都可以显示出来。如果不添加这个属性，就只会显示布局文件定义的界面。

#### Manifest.xml配置文件

- application标签

  - android:alloback允许备份？
  - 其余的是应用图标，名称，主题。。。

- intent-filter标签

  ```xml
        <activity android:name=".business.account.view.activity.SplashActivity">
              <intent-filter>
                  <action android:name="android.intent.action.MAIN"/>
                  <category android:name="android.intent.category.LAUNCHER"/>
              </intent-filter>
          </activity>
  ```

### 1.2签名打包

#### 为什么要签名

```wiki
Android APP都需要我们用一个证书对应用进行数字签名，不然的话是无法安装到Android手机上的，平时我们调试运行时到手机上时，是AS会自动用默认的密钥和证书来进行签名；但是我们实际发布编译时，则不会自动签名，这个时候我们就需要进行手动签名了！ 为我们的APK签名有以下好处：
1.应用程序升级：如果你希望用户无缝升级到新的版本，那么你必须用同一个证书进行签名。这是由于只有以同一个证书签名，系统才会允许安装升级的应用程序。如果你采用了不同的证书，那么系统会要求你的应用程序采用不同的包名称，在这种情况下相当于安装了一个全新的应用程序。如果想升级应用程序，签名证书要相同，包名称要相同！
2.应用程序模块化： Android系统可以允许同一个证书签名的多个应用程序在一个进程里运行，系统实际把他们作为一个单个的应用程序，此时就可以把我们的应用程序以模块的方式进行部署，而用户可以独立的升级其中的一个模块。
3.代码或者数据共享： Android提供了基于签名的权限机制，那么一个应用程序就可以为另一个以相同证书签名的应用程序公开自己的功能。以同一个证书对多个应用程序进行签名，利用基于签名的权限检查，你就可以在应用程序间以安全的方式共享代码和数据了。 不同的应用程序之间，想共享数据，或者共享代码，那么要让他们运行在同一个进程中，而且要让他们用相同的证书签名。 ————上述内容摘自:android 为什么需要签
```

#### 如何签名

点击build下的Generate Signed AKP根据提示框，如果没有Key就先创建一个Key，然后就可以一键傻瓜式打包APK

### 1.3反编译APK获取代码&资源

#### 三个工具

```wik
apktool：获取资源文件，提取图片文件，布局文件，还有一些XML的资源文件
dex2jar：将APK反编译成Java源码(将classes.dex转化为jar文件)
jd-gui：查看2中转换后的jar文件，即查看Java文件 为了方便各位读者，这里将三个打包到一起放到云盘中，又需要的可以进行下载： 反编译相关的三个工具.zip
```

**找个机会实战一下**

## 2.View与ViewGroup的概念

### 2.1布局

- **LinearLayout**

  - Android中有六大布局,分别是: LinearLayout(线性布局)，RelativeLayout(相对布局)，TableLayout(表格布局) FrameLayout(帧布局)，AbsoluteLayout(绝对布局)，GridLayout(网格布局) 

  - 其中LinearLayout中最常用的属性就是weight属性，权重。。

- **RelativeLayout**

  ```reStructuredText
  就是当界面比较复杂的时候，需要嵌套多层的 LinearLayout,这样就会降低UI Render的效率(渲染速度),而且如果是listview或者GridView上的 item,效率会更低,另外太多层LinearLayout嵌套会占用更多的系统资源,还有可能引发stackoverflow; 但是如果我们使用RelativeLayout的话,可能仅仅需要一层就可以完成了,以父容器或者兄弟组件参考+margin +padding就可以设置组件的显示位置,是比较方便的!当然,也不是绝对的,具体问题具体分析吧! 总结就是:尽量使用RelativeLayout + LinearLayout的weight属性搭配使用吧！ 
  ```

  - gravity:控制组件内部的对其方式
  - ignoreGravity设置该属性为true时，将不受gravity的影响- 
  - **layout_alignParentXXX**是在父容器中怎么怎么对其
  - layout_toLeft/Right、layout_above/below让该组件位于参考组件的上下左右方
  - **layout_alignLeft/Right/Top/Bottom**对其参考组件的哪个边界
  - layout_margin偏移量
  - layout_padding填充****

- **TableLayout**

  - 不看了，没用过

- **FrameLayout**

  - 随手指滑动的妹子

    - 自定义妹子view重写onDraw方法
    - 在活动里为meizi设置触摸监听onTouchListener，重写onTouch方法，在onTouch方法里重回妹子图片
    - 最后将meizi控件加到Framelayout中

    ```java
    
    ```

  - 跑动的meizi

    - 这个就是在活动中定义一个定时器对象，定时发送信息给handler

    ```java
       //定义一个定时器对象,定时发送信息给handler    
            new Timer().schedule(new TimerTask() {    
                    
                @Override    
                public void run() {    
                    //发送一条空信息来通知系统改变前景图片    
                    handler.sendEmptyMessage(0x123);    
                }    
            }, 0,170); 
    ```

    - 在handleMessage方法中将drawable动态设置到FrameLayout中

    `frame.setForeground(h);  

- **GridLayout(网格布局)**

  - 今天要介绍的布局是Android 4.0以后引入的一个新的布局,和前面所学的TableLayout(表格布局) 有点类似,不过他有很多前者没有的东西,也更加好用 
    - 可以自己设置布局中组件的排列方式
    - 可以自定义网格布局有多少行,多少列
    - 可以直接设置组件位于某行某列
    - 可以设置组件横跨几行或者几列
  - rowCount等设置网格布局的行和列数
  - layout_row/column设置组件位于的行和列‘
  - layout_rowSpan/columnSpan设置组件的跨行列数
  - 教程中有一个简单的计算器布局实现

- **AbsoluteLayout**

  - 基本不会使用~~~所以我略过了...

### 2.2各种控件

#### 2.2.1TextView

[在这先贴一下googl中国的android的api网站](https://developer.android.google.cn/reference/packages)

##### autolink属性自动跳转

##### SpannableString&SpannableStringBuilder定制文本

毕竟api这么多记不过来，现用现查得了

```java
BackgroundColorSpan 背景色
ClickableSpan 文本可点击，有点击事件
ForegroundColorSpan 文本颜色（前景色）
MaskFilterSpan 修饰效果，如模糊(BlurMaskFilter)、浮雕(EmbossMaskFilter)
MetricAffectingSpan 父类，一般不用
RasterizerSpan 光栅效果
StrikethroughSpan 删除线（中划线）
SuggestionSpan 相当于占位符
UnderlineSpan 下划线
AbsoluteSizeSpan 绝对大小（文本字体）
DynamicDrawableSpan 设置图片，基于文本基线或底部对齐。
ImageSpan 图片
RelativeSizeSpan 相对大小（文本字体）
ReplacementSpan 父类，一般不用
ScaleXSpan 基于x轴缩放
StyleSpan 字体样式：粗体、斜体等
SubscriptSpan 下标（数学公式会用到）
SuperscriptSpan 上标（数学公式会用到）
TextAppearanceSpan 文本外貌（包括字体、大小、样式和颜色）
TypefaceSpan 文本字体
URLSpan 文本超链接
```

下面是一个例子，，设置区间范围的时候是【）左闭右开区间，因为下标0开始。

```java
public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        TextView t1 = (TextView) findViewById(R.id.txtOne);
        TextView t2 = (TextView) findViewById(R.id.txtTwo);
		
       
        SpannableString span = new SpannableString("红色打电话斜体删除线绿色下划线图片:.");
        //1.设置背景色,setSpan时需要指定的flag,Spanned.SPAN_EXCLUSIVE_EXCLUSIVE(前后都不包括)
        span.setSpan(new ForegroundColorSpan(Color.RED), 0, 2, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        //2.用超链接标记文本
        span.setSpan(new URLSpan("tel:4155551212"), 2, 5, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        //3.用样式标记文本（斜体）
        span.setSpan(new StyleSpan(Typeface.BOLD_ITALIC), 5, 7, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        //4.用删除线标记文本
        span.setSpan(new StrikethroughSpan(), 7, 10, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        //5.用下划线标记文本
        span.setSpan(new UnderlineSpan(), 10, 16, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        //6.用颜色标记
        span.setSpan(new ForegroundColorSpan(Color.GREEN), 10, 13,Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        //7.//获取Drawable资源
        Drawable d = getResources().getDrawable(R.drawable.icon);
        d.setBounds(0, 0, d.getIntrinsicWidth(), d.getIntrinsicHeight());
        //8.创建ImageSpan,然后用ImageSpan来替换文本
        ImageSpan imgspan = new ImageSpan(d, ImageSpan.ALIGN_BASELINE);
        span.setSpan(imgspan, 18, 19, Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
        t1.setText(span);
    }
}
```

##### 部分可点击TextView

```java
String url="注册宝驾会员代表您已同意《我们服务条款》,请认真阅读。";
SpannableStringBuilder style = new SpannableStringBuilder(url); 
TextViewURLSpan myURLSpan = new TextViewURLSpan();
style.setSpan(myURLSpan,12,20, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE); 
tv_service.setText(style); 
tv_service.setMovementMethod(LinkMovementMethod.getInstance());
///////////////自定义点击事件
 private class TextViewURLSpan extends ClickableSpan{
	        @Override 
	        public void updateDrawState(TextPaint ds) {  
	            ds.setColor(getResources().getColor(R.color.c_new_blue));  
	            ds.setUnderlineText(false); //去掉下划线  
	        }  
	        @Override
	        public void onClick(View widget) {//点击事件
	        	Intent intent=new Intent(getApplicationContext(),RegServiceA.class);
			startActivity(intent);
	        }
     
/////////////////////HutHelper
SpannableString spannableString = new SpannableString("密码默认为身份证后六位（x小写） 忘记密码？");

        spannableString.setSpan(new ClickableSpan() {
            @Override
            public void onClick(View widget) {
                Bundle bundle = new Bundle();
                bundle.putInt("type", WebViewActivity.TYPE_CHANGE_PWD);
                bundle.putString("title", "修改密码");
                bundle.putString("url", Constants.CHANGE_PWD);
                LoginActivity.this.startActivity(WebViewActivity.class, bundle);
            }

            @Override
            public void updateDrawState(TextPaint ds) {
                super.updateDrawState(ds);
                ds.setColor(Color.parseColor("#ff8f8f8f"));
                ds.setUnderlineText(true);
            }
        }, 17, 22, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        tvMessage.setText(spannableString);
        tvMessage.setMovementMethod(LinkMovementMethod.getInstance());
```

实现qq空间的点赞人列表部分可点击效果，**我认为也可以用recyclerview的瀑布流来实现。。。。**

#### 2.2.2EditText

- 提示文本的属性

  - 处理hint还有textColorHint

- 获取焦点后获取文本框的内容

  `android:selectAllOnFocus="true"`

- 限制输入类型

  `android:inputType`

- 限制单行输入，例如登录界面

  `android:singleLine="true"`

##### 获取焦点与键盘弹出问题

  贴一个KeyBoardUtil.java

  ```java
  ///使用方法
  KeyBoardUtils.scrollLayoutAboveKeyBoard(context, rootView, tvMessage);
  KeyBoardUtils.hideSoftInput(context, getWindow());
  //util
  public class KeyBoardUtils {
      /**
       * 将布局滑动键盘遮住部分的高度
       *
       * @param root 根布局
       * @param scrollToView 最后可见的控件,外层不可以有布局包裹，必须为根布局的直接子布局
       */
      public static void scrollLayoutAboveKeyBoard(final Context context, final View root, final View scrollToView) {
  
          root.getViewTreeObserver().addOnGlobalLayoutListener(new ViewTreeObserver.OnGlobalLayoutListener() {
              @Override
              public void onGlobalLayout() {
                  Rect rect = new Rect();
                  //获取root在窗体的可视区域
                  root.getWindowVisibleDisplayFrame(rect);
                  //获取root在窗体的不可视区域高度(被其他View遮挡的区域高度)
                  int rootInvisibleHeight = root.getRootView().getHeight() - rect.bottom;
                  //若不可视区域高度大于400，则键盘显示
                  if (rootInvisibleHeight > 400) {
                      int[] location = new int[2];
                      //获取root在窗体的坐标
                      root.getLocationInWindow(location);
                      //计算root滚动高度，使scrollToView在可见区域
                      int srollHeight = (location[1] + root.getHeight())
                              - (root.getHeight() - scrollToView.getBottom())
                              + DensityUtils.dp2px(context, 5) - rect.bottom;
  
                      root.scrollTo(0, srollHeight);
  
                  } else {
                      //键盘隐藏
                      root.scrollTo(0, 0);
                  }
              }
          });
      }
  
      /**
       * 显示软件盘
       * @param context
       * @param view 需要输入的view，必须为EditText或者它的子类，必须可见且能够获取焦点
       */
      public static void showSoftInput(Context context, View view){
          InputMethodManager imm = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
          if (imm != null){
              imm.showSoftInput(view, 0);
          }
      }
  
      /**
       * 关闭软键盘
       * @param context
       * @param window
       */
      public static void hideSoftInput(Context context, Window window){
          InputMethodManager imm = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
          if (imm != null){
              imm.hideSoftInputFromWindow(window.getDecorView().getWindowToken(), 0);
          }
      }
  }
  ```
##### 带表情的EditText

  ```java
  1.使用SpannableString来实现
  2.使用Html类来实现
  这里笔者用的是第一种，这里只实现一个简单的效果，大家可以把方法抽取出来，自定义一个EditText；
  也可以自己动手写个类似于QQ那样有多个表情选择的输入框！
  ```

  ```java
  public class MainActivity extends Activity {
      private Button btn_add;
      private EditText edit_one;
      @Override
      protected void onCreate(Bundle savedInstanceState) {
          super.onCreate(savedInstanceState);
          setContentView(R.layout.activity_main);
          btn_add = (Button) findViewById(R.id.btn_add);
          edit_one = (EditText) findViewById(R.id.edit_one);
          btn_add.setOnClickListener(new OnClickListener() {
              @Override
              public void onClick(View v) {
                  SpannableString spanStr = new SpannableString("imge");
                  Drawable drawable = MainActivity.this.getResources().getDrawable(R.drawable.f045);
                  drawable.setBounds(0,0,drawable.getIntrinsicWidth(),drawable.getIntrinsicHeight());
                  ImageSpan span = new ImageSpan(drawable,ImageSpan.ALIGN_BASELINE);
                  spanStr.setSpan(span,0,4,Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
                  int cursor = edit_one.getSelectionStart();
                  edit_one.getText().insert(cursor, spanStr);
              }
          });
      }
  }
  ```

##### 带删除的按钮的的EditText

  > 为EditText设置addTextChangedListener，然后重写TextWatcher（）里的抽象方法，这个用于监听输入框变化的；然后setCompoundDrawablesWithIntrinsicBounds设置小叉叉的图片；最后，重写onTouchEvent方法，如果点击区域是小叉叉图片的位置，清空文本！ 

```java
public class EditTextWithDel extends EditText {

    private final static String TAG = "EditTextWithDel";
    private Drawable imgInable;
    private Drawable imgAble;
    private Context mContext;

    public EditTextWithDel(Context context) {
        super(context);
        mContext = context;
        init();
    }

    public EditTextWithDel(Context context, AttributeSet attrs) {
        super(context, attrs);
        mContext = context;
        init();
    }

    public EditTextWithDel(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        init();
    }

    private void init() {
        imgInable = mContext.getResources().getDrawable(R.drawable.delete_gray);
        addTextChangedListener(new TextWatcher() {
            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
            }

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {
            }

            @Override
            public void afterTextChanged(Editable s) {
                setDrawable();
            }
        });
        setDrawable();
    }

    // 设置删除图片
    private void setDrawable() {
        if (length() < 1)
            setCompoundDrawablesWithIntrinsicBounds(null, null, null, null);
        else
            setCompoundDrawablesWithIntrinsicBounds(null, null, imgInable, null);
    }

    // 处理删除事件******************************
    主要是这里到吗比较重要，不过我觉得用组合控件也不是不可以。。。。
    ********************************
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (imgInable != null && event.getAction() == MotionEvent.ACTION_UP) {
            int eventX = (int) event.getRawX();
            int eventY = (int) event.getRawY();
            Log.e(TAG, "eventX = " + eventX + "; eventY = " + eventY);
            Rect rect = new Rect();
            getGlobalVisibleRect(rect);
            rect.left = rect.right - 100;
            if (rect.contains(eventX, eventY))
                setText("");
        }
        return super.onTouchEvent(event);
    }
    @Override
    protected void finalize() throws Throwable {
        super.finalize();
    }
}
```

说到这里顺带查了一下TextWatcher这个东西，它有三个方法， brfore，on，after

**TextWatcher在什么时候会被调用?**
 TextWatcher在edittext内容发生变化时会被调用
 **TextWatcher一共有三个方法**
 `beforeTextChanged(CharSequence s, int start, int count, int after)`
 在文本变化前调用,`start`代表开始变化的位置,`count`代表变化的字符长度.`after`代表变化后字符该位置字符数量
 `onTextChanged(CharSequence s, int start, int before, int count)`
 在文本变化时调用,此时s的内容已发生改变,`start`代表开始变化的位置,`before`代表变化前该位置字符数量,`count`代表变化了的字符长度
 `afterTextChanged(Editable s)`
 在文本变化后调用,`s`即为变化后的文本结果

#### 2.2.3Button(按钮)与ImageButton(图像按钮)

> Button是TextView的子类，所以TextView上很多属性也可以应用到Button 上！我们实际开发中对于Button的，无非是对按钮的几个状态做相应的操作，比如：按钮按下的时候 用一种颜色，弹起又一种颜色，或者按钮不可用的时候一种颜色这样！上述实现无非是通过 **StateListDrawable**这种Drawable资源来实现 

- **StateListDrawable**是drawable的一种关键节点selector

```java
//可设置的属性
drawable:引用的Drawable位图,我们可以把他放到最前面,就表示组件的正常状态~
state_focused:是否获得焦点
state_window_focused:是否获得窗口焦点
state_enabled:控件是否可用       //...
state_checkable:控件可否被勾选,eg:checkbox
state_checked:控件是否被勾选
state_selected:控件是否被选择,针对有滚轮的情况
state_pressed:控件是否被按下     //...
state_active:控件是否处于活动状态,eg:slidingTab
state_single:控件包含多个子控件时,确定是否只显示一个子控件
state_first:控件包含多个子控件时,确定第一个子控件是否处于显示状态
state_middle:控件包含多个子控件时,确定中间一个子控件是否处于显示状态
state_last:控件包含多个子控件时,确定最后一个子控件是否处于显示状态
```

##### 实现Material Design水波效果

- 实现思路，后面代码注释挺全的。

```java
1.我们继承ImageButton，当然你可以换成Button或者View,这里笔者想把龟放到中间才继承ImageButton
2.首先，创建两个Paint(画笔)对象，一个绘制底部背景颜色，一个绘制波纹扩散的
3.接着计算最大半径，开始半径每隔一段时间递增一次，直到等于最大半径，然后重置状态！
//在XJBL里跟着写了一下，还是有点迷，继续。。。
```

#### 2.2.4ImageView(图像视图)

- src会直接填图片，不会拉伸而background会根据ImageVIew的大小进行拉伸。

- adjustViewBounds设置缩放是否保存原图长宽比，单独设置不起作用，需要配合**maxWidth**和**maxHeight**属性一起使用！而后面这两个属性 也是需要adjustViewBounds为true才会生效的 

- scaleType设置缩放类型

  ```java
  android:scaleType用于设置显示的图片如何缩放或者移动以适应ImageView的大小 Java代码中可以通过imageView.setScaleType(ImageView.ScaleType.CENTER);来设置~ 可选值如下：
  fitXY:对图像的横向与纵向进行独立缩放,使得该图片完全适应ImageView,但是图片的横纵比可能会发生改变
  fitStart:保持纵横比缩放图片,知道较长的边与Image的编程相等,缩放完成后将图片放在ImageView的左上角
  fitCenter:同上,缩放后放于中间;
  fitEnd:同上,缩放后放于右下角;
  center:保持原图的大小，显示在ImageView的中心。当原图的size大于ImageView的size，超过部分裁剪处理。
  centerCrop:保持横纵比缩放图片,知道完全覆盖ImageView,可能会出现图片的显示不完全//常用
  centerInside:保持横纵比缩放图片,直到ImageView能够完全地显示图片
  matrix:默认值，不改变原图的大小，从ImageView的左上角开始绘制原图， 原图超过ImageView的部分作裁剪处理
  //现用现查
  ```
#### Adapter

- **除了通过数组外，我们还可以写到一个数组资源文件中：**

比如：在res\valuse下创建一个数组资源的xml文件：**arrays.xml**：

```xml
<?xml version="1.0" encoding="utf-8"?>  
<resources>  
    <string-array name="myarray">  
    <item>语文</item>  
    <item>数学</item>  
    <item>英语</item>  
    </string-array>      
</resources>
```

接着布局的listview属性设置下这个列表项：

```xml
<ListView  
        android:id="@id/list_test"  
        android:layout_height="match_parent"  
        android:layout_width="match_parent"   
        android:entries="@array/myarray"/>
```

#### Android危险权限列表

```java
//联系人
group:android.permission-group.CONTACTS
  permission:android.permission.WRITE_CONTACTS
  permission:android.permission.GET_ACCOUNTS
  permission:android.permission.READ_CONTACTS
//电话
group:android.permission-group.PHONE
  permission:android.permission.READ_CALL_LOG
  permission:android.permission.READ_PHONE_STATE
  permission:android.permission.CALL_PHONE
  permission:android.permission.WRITE_CALL_LOG
  permission:android.permission.USE_SIP
  permission:android.permission.PROCESS_OUTGOING_CALLS
  permission:com.android.voicemail.permission.ADD_VOICEMAIL
//日历
group:android.permission-group.CALENDAR
  permission:android.permission.READ_CALENDAR
  permission:android.permission.WRITE_CALENDAR
//相机
group:android.permission-group.CAMERA
  permission:android.permission.CAMERA
//传感器
group:android.permission-group.SENSORS
  permission:android.permission.BODY_SENSORS
//位置
group:android.permission-group.LOCATION
  permission:android.permission.ACCESS_FINE_LOCATION
  permission:android.permission.ACCESS_COARSE_LOCATION
//读写存储
group:android.permission-group.STORAGE
  permission:android.permission.READ_EXTERNAL_STORAGE
  permission:android.permission.WRITE_EXTERNAL_STORAGE
//麦克风
group:android.permission-group.MICROPHONE
  permission:android.permission.RECORD_AUDIO
//SMS
group:android.permission-group.SMS
  permission:android.permission.READ_SMS
  permission:android.permission.RECEIVE_WAP_PUSH
  permission:android.permission.RECEIVE_MMS
  permission:android.permission.RECEIVE_SMS
  permission:android.permission.SEND_SMS
  permission:android.permission.READ_CELL_BROADCASTS
```

#### Listview

##### 添加头尾view

```java
addHeaderView(View v)：添加headView(表头),括号中的参数是一个View对象
addFooterView(View v)：添加footerView(表尾)，括号中的参数是一个View对象
addHeaderView(headView, null, false)：和前面的区别：设置Header是否可以被选中
addFooterView(View,view,false)：同上
对了，使用这个addHeaderView方法必须放在listview.setAdapter前面，否则会报错。
```

##### 优化listview

```java
因为每次有多少个item，getview方法就会创建多次，下面的布局加载器也就会加载多少次布局
convertView = LayoutInflater.from(mContext).inflate(R.layout.item_list_animal,parent,false);
getView会调用多次，然而。。。getViewById也会调用多次，
所以就用两个步骤判断一下
public View getView(int position, View convertView, ViewGroup parent) {
    ViewHolder holder = null;
    if(convertView == null){
        convertView = LayoutInflater.from(mContext).inflate(R.layout.item_list_animal,parent,false);
        holder = new ViewHolder();
        holder.img_icon = (ImageView) convertView.findViewById(R.id.img_icon);
        holder.txt_aName = (TextView) convertView.findViewById(R.id.txt_aName);
        holder.txt_aSpeak = (TextView) convertView.findViewById(R.id.txt_aSpeak);
        convertView.setTag(holder);   //将Holder存储到convertView中
        
 //tag++++++++++++++++
 就是将。。。
 ///
    }else{
        holder = (ViewHolder) convertView.getTag();
    }
    holder.img_icon.setBackgroundResource(mData.get(position).getaIcon());
    holder.txt_aName.setText(mData.get(position).getaName());
    holder.txt_aSpeak.setText(mData.get(position).getaSpeak());
    return convertView;
}

static class ViewHolder{
    ImageView img_icon;
    TextView txt_aName;
    TextView txt_aSpeak;
}
```

##### 获取焦点

item根节点设置android:descendantFocusability="blocksDescendants"

> 如题，在Item布局的根节点添加上述属性，**android:descendantFocusability="blocksDescendants"** 即可，另外该属性有三个可供选择的值：
>
> - **beforeDescendants**：viewgroup会优先其子类控件而获取到焦点
> - **afterDescendants**：viewgroup只有当其子类控件不需要获取焦点时才获取焦点
> - **blocksDescendants**：viewgroup会覆盖子类控件而直接获得焦点

##### 多类型的RecyclerView

需要重写`pint getItemViewType(int position)`方法，根据position的位置返回不同的位置，再在`onCreateViewHolder`方法中



#### ViewFlipper(翻转视图)的基本使用



#### ExpandableListView(可折叠列表)的基本使用



#### Spinner(列表选项框)的基本使用



#### Toast

```java
//简单定制
void midToast(String str, int showTime)
{
    Toast toast = Toast.makeText(global_context, str, showTime);            
    toast.setGravity(Gravity.CENTER_VERTICAL|Gravity.CENTER_HORIZONTAL , 0, 0);  //设置显示位置
    TextView v = (TextView) toast.getView().findViewById(android.R.id.message);
    v.setTextColor(Color.YELLOW);     //设置字体颜色
    toast.show();   
}
//获取背景layout
toast.setGravity(Gravity.CENTER_HORIZONTAL|Gravity.BOTTOM , 0, 0);  //设置显示位置
    LinearLayout layout = (LinearLayout) toast.getView();    //关键代码。。。
    layout.setBackgroundColor(Color.BLUE);
    ImageView image = new ImageView(this);
    image.setImageResource(R.mipmap.ic_icon_qitao);
    layout.addView(image, 0);
    TextView v = (TextView) toast.getView().findViewById(android.R.id.message);
    v.setTextColor(Color.YELLOW);     //设置字体颜色
    toast.show();
//完全自定义
private void midToast(String str, int showTime)
{
    LayoutInflater inflater = getLayoutInflater();
    View view = inflater.inflate(R.layout.view_toast_custom,
            (ViewGroup) findViewById(R.id.lly_toast));
    ImageView img_logo = (ImageView) view.findViewById(R.id.img_logo);
    TextView tv_msg = (TextView) view.findViewById(R.id.tv_msg);
    tv_msg.setText(str);
    Toast toast = new Toast(mContext);
    toast.setGravity(Gravity.CENTER, 0, 0);
    toast.setDuration(Toast.LENGTH_LONG);
    toast.setView(view);
    toast.show();
}
//还可以自定义背景
```

#### Notification(状态栏通知)

```java
//组成元素
Icon/Photo：大图标
Title/Name：标题
Message：内容信息
Timestamp：通知时间，默认是系统发出通知的时间，也可以通过setWhen()来设置
Secondary Icon：小图标
内容文字，在小图标的左手边的一个文字
//基本使用流程
//主要涉及两个类
Notification：通知信息类，它里面对应了通知栏的各个属性
NotificationManager：是状态栏通知的管理类，负责发通知、清除通知等操作。

```

- **setContentTitle**(CharSequence)：设置标题

- **setContentText**(CharSequence)：设置内容

- **setSubText**(CharSequence)：设置内容下面一小行的文字

- **setTicker**(CharSequence)：设置收到通知时在顶部显示的文字信息

- **setWhen**(long)：设置通知时间，一般设置的是收到通知时的System.currentTimeMillis()

- **setSmallIcon**(int)：设置右下角的小图标，在接收到通知的时候顶部也会显示这个小图标

- **setLargeIcon**(Bitmap)：设置左边的大图标

- **setAutoCancel**(boolean)：用户点击Notification点击面板后是否让通知取消(默认不取消)

- **setDefaults**(int)：向通知添加声音、闪灯和振动效果的最简单、 使用默认（defaults）属性，可以组合多个属性，
  **Notification.DEFAULT_VIBRATE**(添加默认震动提醒)；
  **Notification.DEFAULT_SOUND**(添加默认声音提醒)；
  **Notification.DEFAULT_LIGHTS**(添加默认三色灯提醒)
  **Notification.DEFAULT_ALL**(添加默认以上3种全部提醒)

- **setVibrate**(long[])：设置振动方式，比如：
  setVibrate(new long[] {0,300,500,700});延迟0ms，然后振动300ms，在延迟500ms， 接着再振动700ms，关于Vibrate用法后面会讲解！

- **setLights**(int argb, int onMs, int offMs)：设置三色灯，参数依次是：灯光颜色， 亮持续时间，暗的时间，不是所有颜色都可以，这跟设备有关，有些手机还不带三色灯； 另外，还需要为Notification设置flags为Notification.FLAG_SHOW_LIGHTS才支持三色灯提醒！

- **setSound**(Uri)：设置接收到通知时的铃声，可以用系统的，也可以自己设置，例子如下:
  .setDefaults(Notification.DEFAULT_SOUND) //获取默认铃声
  .setSound(Uri.parse("file:///sdcard/xx/xx.mp3")) //获取自定义铃声
  .setSound(Uri.withAppendedPath(Audio.Media.INTERNAL_CONTENT_URI, "5")) //获取Android多媒体库内的铃声

- **setOngoing**(boolean)：设置为ture，表示它为一个正在进行的通知。他们通常是用来表示 一个后台任务,用户积极参与(如播放音乐)或以某种方式正在等待,因此占用设备(如一个文件下载, 同步操作,主动网络连接)

- **setProgress**(int,int,boolean)：设置带进度条的通知 参数依次为：进度条最大数值，当前进度，进度是否不确定 如果为确定的进度条：调用setProgress(max, progress, false)来设置通知， 在更新进度的时候在此发起通知更新progress，并且在下载完成后要移除进度条 ，通过调用setProgress(0, 0, false)既可。如果为不确定（持续活动）的进度条， 这是在处理进度无法准确获知时显示活动正在持续，所以调用setProgress(0, 0, true) ，操作结束时，调用setProgress(0, 0, false)并更新通知以移除指示条

- **setContentIntent**(PendingIntent)：PendingIntent和Intent略有不同，它可以设置执行次数， 主要用于远程服务通信、闹铃、通知、启动器、短信中，在一般情况下用的比较少。比如这里通过 Pending启动Activity：getActivity(Context, int, Intent, int)，当然还可以启动Service或者Broadcast PendingIntent的位标识符(第四个参数)：
  **FLAG_ONE_SHOT** 表示返回的PendingIntent仅能执行一次，执行完后自动取消
  **FLAG_NO_CREATE** 表示如果描述的PendingIntent不存在，并不创建相应的PendingIntent，而是返回NULL
  **FLAG_CANCEL_CURRENT** 表示相应的PendingIntent已经存在，则取消前者，然后创建新的PendingIntent， 这个有利于数据保持为最新的，可以用于即时通信的通信场景
  **FLAG_UPDATE_CURRENT** 表示更新的PendingIntent
  使用示例：

- ```java
  //点击后跳转Activity
  Intent intent = new Intent(context,XXX.class);  
  PendingIntent pendingIntent = PendingIntent.getActivity(context, 0, intent, 0);  
  mBuilder.setContentIntent(pendingIntent)  
  //pendingIntent需要四参，其中一个为intent
  ```

- **setPriority**(int)：设置优先级： 

- **对应属性：Notification.PRIORITY_HIGH...**

- ```java
   private NotificationManager mNManager;
      private Notification notification;
   mNManager=(NotificationManager) getSystemService(NOTIFICATION_SERVICE);
  Notification.Builder builder=new Notification.Buidler(this);
  builder.setXXX();
  nitofication=buidler.build();
  mNManager.notify("等级",notifitcation);
  ```

#### AlertDialog(对话框)详解

```java
本节继续给大家带来是显示提示信息的第三个控件AlertDialog(对话框)，同时它也是其他 Dialog的的父类！比如ProgressDialog，TimePickerDialog等，而AlertDialog的父类是：Dialog！ 另外，不像前面学习的Toast和Notification，AlertDialog并不能直接new出来，如果你打开 AlertDialog的源码，会发现构造方法是protected的，如果我们要创建AlertDialog的话，我们 需要使用到该类中的一个静态内部类：public static class Builder，然后来调用AlertDialog 里的相关方法，来对AlertDialog进行定制，最后调用show()方法来显示我们的AlertDialog对话框！
```

使用流程

**必要条件Dialog和Dialog.Builder**

- **Step 1**：创建**AlertDialog.Builder**对象；
- **Step 2**：调用**setIcon()**设置图标，**setTitle()**或**setCustomTitle()**设置标题；
- **Step 3**：设置对话框的内容：**setMessage()**还有其他方法来指定显示的内容；
- **Step 4**：调用**setPositive/Negative/NeutralButton()**设置：确定，取消，中立按钮；
- **Step 5**：调用**create()**方法创建这个对象，再调用**show()**方法将对话框显示出来；

Dialog.Builder可以生成

- 普通对话框
  - setNeutralButton中立按钮。。。
- 普通列表对话框
  - setItems方法
- 单选列表对话框
  - setSingleChoiceItems
- 多选列表对话框
  - setMultiChoiceItems

到时候现用现看代码提示。。。。。知道原生的dialog能产生这四种就行。。。。

**可以为builder设置setCancelable(true/false)方法控制dialog外部可不可以点击。。。**

##### 自定义dialog

```java
//关键代码
 		 private AlertDialog alert = null;
    	private AlertDialog.Builder builder = null;		
		//初始化Builder
        builder = new AlertDialog.Builder(mContext);

        //加载自定义的那个View,同时设置下
        final LayoutInflater inflater = MainActivity.this.getLayoutInflater();
        view_custom = inflater.inflate(R.layout.view_dialog_custom, null,false);
        builder.setView(view_custom);
        builder.setCancelable(false);
        alert = builder.create();
```

 **至此，，，toast，notification，dialog，android里三种消息提示框都已经讲完了。。。**

#### 其他几种常用对话框基本使用

##### PorgressDialog

```java
//基本使用方法
我们创建进度条对话框的方式有两种：
1.直接调用ProgressDialog提供的静态方法show()显示
2.创建ProgressDialog,再设置对话框的参数,最后show()出来
```



#### PopupWindow(悬浮框)的基本使用

特点：**位置是随意的**

**另外AlertDialog是非堵塞线程的，而PopupWindow则是堵塞线程的！** 

官方介绍**：一个弹出窗口控件，可以用来显示任意View，而且会浮动在当前activity的顶部** 

```java
//构造方法9种之多这里只贴几种常用的
public PopupWindow (Context context)
public PopupWindow(View contentView, int width, int height)
public PopupWindow(View contentView)
public PopupWindow(View contentView, int width, int height, boolean focusable)
    
//常用的一些方法
setContentView(View contentView)：设置PopupWindow显示的View
getContentView()：获得PopupWindow显示的View
showAsDropDown(View anchor)：相对某个控件的位置（正左下方），无偏移
showAsDropDown(View anchor, int xoff, int yoff)：相对某个控件的位置，有偏移
showAtLocation(View parent, int gravity, int x, int y)： 相对于父控件的位置（例如正中央Gravity.CENTER，下方Gravity.BOTTOM等），可以设置偏移或无偏移 PS:parent这个参数只要是activity中的view就可以了！
setWidth/setHeight：设置宽高，也可以在构造方法那里指定好宽高， 除了可以写具体的值，还可以用WRAP_CONTENT或MATCH_PARENT， popupWindow的width和height属性直接和第一层View相对应。
setFocusable(true)：设置焦点，PopupWindow弹出后，所有的触屏和物理按键都由PopupWindows 处理。其他任何事件的响应都必须发生在PopupWindow消失之后，（home 等系统层面的事件除外）。 比如这样一个PopupWindow出现的时候，按back键首先是让PopupWindow消失，第二次按才是退出 activity，准确的说是想退出activity你得首先让PopupWindow消失，因为不并是任何情况下按back PopupWindow都会消失，必须在PopupWindow设置了背景的情况下 。
setAnimationStyle(int)：设置动画效果
```

```java
//HutHelper里的一个例子
//1.初始化PopWindow
 LayoutInflater layoutInflater = (LayoutInflater) getSystemService(LAYOUT_INFLATER_SERVICE);
            View popupWindowLayout = layoutInflater.inflate(R.layout.popup_list_choose, null);

            weekListWindow = new PopupWindow(popupWindowLayout,
                    DensityUtils.dp2px(context, 170),
                    DensityUtils.dp2px(context, 115));
//2。初始化控件并且设置点击事件
            TextView tvMime = (TextView) popupWindowLayout.findViewById(R.id.tv_popmenu_mime);
            TextView tvAdd = (TextView) popupWindowLayout.findViewById(R.id.tv_popmenu_add);
            tvAdd.setText("添加失物");
            tvMime.setText("我的发布");
            tvAdd.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    weekListWindow.dismiss();
                    startActivityForResult(CreateLostAndFoundActivity.class, Constants.REQUEST);
                }
            });
            tvMime.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    weekListWindow.dismiss();
                    Bundle bundle = new Bundle();
                    bundle.putInt("type", SearchPresenter.TYPE_MYLOSTANDFOUND);
                    bundle.putString("searchText", user.getUser_id());
                    bundle.putString("extras", user.getUsername());
                    startActivity(SearchResultActivity.class, bundle);
                }
            });

        }
//3.可以给当时的rootview设置前景色（通过setForeground方法。。）
        rootView.setForeground(getResources().getDrawable(R.color.bg_black_shadow));
//4.设置PopWindow消失监听
        weekListWindow.setOnDismissListener(new PopupWindow.OnDismissListener() {
            @Override
            public void onDismiss() {
                rootView.setForeground(getResources().getDrawable(R.color.transparent));
            }
        });
//5.设置焦点
        weekListWindow.setFocusable(true);
//设置点击外部可消失
        weekListWindow.setOutsideTouchable(true);
        weekListWindow.setBackgroundDrawable(new BitmapDrawable());
//设置控件相对偏移量
        weekListWindow.showAsDropDown(parent, -DensityUtils.dp2px(context, 115), 20);
```



#### 菜单(Menu)

- **OptionMenu**：选项菜单，android中最常见的菜单，通过Menu键来调用
- **SubMenu**：子菜单，android中点击子菜单将弹出一个显示子菜单项的悬浮框， 子菜单不支持嵌套，即不能包括其他子菜单
- **ContextMenu**：上下文菜单，通过长按某个视图组件后出现的菜单，该组件需注册上下文菜单

##### 如何使用OptionMenu？

- 重写两个方法就好

  ```java
  public boolean onCreateOptionsMenu(Menu menu)：调用OptionMenu，在这里完成菜单初始化
  public boolean onOptionsItemSelected(MenuItem item)：菜单项被选中时触发，这里完成事件处理
  //当然除了上面这两个方法我们可以重写外我们还可以重写这三个方法：
  public void onOptionsMenuClosed(Menu menu)：菜单关闭会调用该方法
  public boolean onPrepareOptionsMenu(Menu menu)：选项菜单显示前会调用该方法， 可在这里进行菜单的调整(动态加载菜单列表)
  public boolean onMenuOpened(int featureId, Menu menu)：选项菜单打开以后会调用这个方法
  ```

- onCreateOptionsMenu

  ```java
  //一种 
  public boolean onCreateOptionsMenu(Menu menu) {
          // Inflate the menu; this adds items to the action bar if it is present.
      
      //第一种
          menu.add(1,RED,4,"红色");
          menu.add(1,GREEN,2,"绿色");
          menu.add(1,BLUE,3,"蓝色");
          menu.add(1,YELLOW,1,"黄色");
          menu.add(1,GRAY,5,"灰色");
          menu.add(1,CYAN,6,"蓝绿色");
          menu.add(1,BLACK,7,"黑色");
          return true;
      
      //第二种
        getMenuInflater().inflate(R.menu.main_menu,menu);
      }
  
  //另一种是在xml文件夹定义menu
  <?xml version="1.0" encoding="utf-8"?>
  <menu xmlns:android="http://schemas.android.com/apk/res/android">
      <item android:id="@+id/new_game"
            android:icon="@drawable/ic_new_game"
            android:title="@string/new_game"
            android:showAsAction="ifRoom"/>
  </menu>
  ```

- onOptionsItemSelected

  ```java
   int id = item.getItemId();
          switch (id){
              case RED:
                  tv_test.setTextColor(Color.RED);
                  break;
  ```

##### ContextMenu(上下文菜单)

- 步骤

  - **Step 1**：重写onCreateContextMenu()方法
  - **Step 2**：为view组件注册上下文菜单，使用registerForContextMenu()方法,参数是View
  - **Step 3**：重写onContextItemSelected()方法为菜单项指定事件监听器

- menu.xml

  ```java
  <menu xmlns:android="http://schemas.android.com/apk/res/android">
      <!-- 定义一组单选按钮 -->
      <!-- checkableBehavior的可选值由三个：single设置为单选，all为多选，none为普通选项 -->
      <group android:checkableBehavior="none">
          <item android:id="@+id/blue" android:title="@string/font_blue"/>
          <item android:id="@+id/green" android:title="@string/font_green"/>
          <item android:id="@+id/red" android:title="@string/font_red"/>
      </group>
  </menu>
  ```

- onCreateContextMenu方法

  ```java
  @Override
      //重写上下文菜单的创建方法
      public void onCreateContextMenu(ContextMenu menu, View v,
                                      ContextMenu.ContextMenuInfo menuInfo) {
          MenuInflater inflator = new MenuInflater(this);
          inflator.inflate(R.menu.menu_context, menu);
          super.onCreateContextMenu(menu, v, menuInfo);
      }
  ```

  

##### PopMenu(弹出型菜单)

```java
//新建PopWindow，并传入点击按钮
        PopupMenu popupMenu=new PopupMenu(this,idMore);
        //给PopWindow设置菜单布局
        popupMenu.getMenuInflater().inflate(R.menu.popmenu_base_func,popupMenu.getMenu());
        //设置菜单点击事件
        popupMenu.setOnMenuItemClickListener(new PopupMenu.OnMenuItemClickListener() {
            @Override
            public boolean onMenuItemClick(MenuItem item) {
        popmenu.show();
```



#### ViewPager

**最常用的组合是现在的tablayout+viewpager**

- 简介

```java
/*
和前面学的ListView，GridView一样，我们也需要一个Adapter (适配器)将我们的View和ViewPager进行绑定，而ViewPager则有一个特定的Adapter—— PagerAdapter！另外，Google官方是建议我们使用Fragment来填充ViewPager的，这样 可以更加方便的生成每个Page，以及管理每个Page的生命周期！给我们提供了两个Fragment 专用的Adapter：FragmentPageAdapter和FragmentStatePagerAdapter 我们简要的来分析下这两个Adapter的区别：
*/
```

- **FragmentPageAdapter**：和PagerAdapter一样，只会缓存当前的Fragment以及左边一个，右边 一个，即总共会缓存3个Fragment而已，假如有1，2，3，4四个页面：
  处于1页面：缓存1，2
  处于2页面：缓存1，2，3
  处于3页面：销毁1页面，缓存2，3，4
  处于4页面：销毁2页面，缓存3，4
  更多页面的情况，依次类推~
- **FragmentStatePagerAdapter**：当Fragment对用户不 见得时，整个Fragment会被销毁， 只会保存Fragment的状态！而在页面需要重新显示的时候，会生成新的页面！

**综上，FragmentPageAdapter适合固定的页面较少的场合；而FragmentStatePagerAdapter则适合 于页面较多或者页面内容非常复杂(需占用大量内存)的情况！**



#### DrawerLayout(官方侧滑菜单)



## 3.基于监听的事件处理机制



## 4.四大组件



## 5.Fragment



## 6.数据存储



## 7.网络部分



## 8.View部分



## 9.动画部分



## 10.多媒体和相机







