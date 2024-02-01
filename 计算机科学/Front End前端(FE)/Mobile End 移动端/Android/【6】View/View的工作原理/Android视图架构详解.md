[TOC]

### 架构图

![视图架构](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/snnH3tXtIT79a05rSk1jJlWYEAra.393Y3c7ArB3QKk!/r/dDYBAAAAAAAA)



![视图架构2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/8bIKyDrSkMdCWYv2hzOavNR2OzTct6.EUKxTRr4o1B4!/r/dFYBAAAAAAAA)



![视图架构3](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/9tggxL5WUz8WN.nR6F.GuHoRBOixsEdP7wkQ.OI3gXA!/r/dDMBAAAAAAAA)

[TOC]

### Activity和Window

 总所周知,Activity并不负责视图控制，它只是控制生命周期和处理事件，真正控制视图的是Window。一个Activity包含了一个Window，Window才是真正代表一个窗口，也就是说Activity可以没有Window，那就相当于是Service了。在ActivityThread中也有控制Service的相关函数或许正好印证了这一点。 

  Activity和Window的第一次邂逅是在ActivityThread调用Activity的attach()函数时。

```java
//[window]:通过PolicyManager创建window,实现callback函数,所以,当window接收到
//外界状态改变时,会调用activity的方法,
final void attach(Context context, ActivityThread aThread,
        Instrumentation instr, IBinder token, int ident,
        Application application, Intent intent, ActivityInfo info,
        CharSequence title, Activity parent, String id,
        NonConfigurationInstances lastNonConfigurationInstances,
        Configuration config, String referrer, IVoiceInteractor voiceInteractor) {
    ....
    //创建window
    mWindow = PolicyManager.makeNewWindow(this);
    //当window接收系统发送给它的IO输入事件时,例如键盘和触摸屏事件,就可以转发给相应的Activity
    mWindow.setCallback(this);
    .....
    //设置window的manager
    mWindow.setWindowManager(
            (WindowManager)context.getSystemService(Context.WINDOW_SERVICE),
            mToken, mComponent.flattenToString(),
            (info.flags & ActivityInfo.FLAG_HARDWARE_ACCELERATED) != 0);
    .....
}
```

在`attach()`中，新建一个`Window`实例作为自己的成员变量，它的类型为`PhoneWindow`,这是抽象类`Window`的一个子类。然后设置`mWindow`的`WindowManager`。



### Window,Activity和DecorView

 DecorView是FrameLayout的子类，它可以被认为是Android视图树的**根节点**视图。DecorView作为顶级View，一般情况下它内部包含一个竖直方向的LinearLayout，在这个LinearLayout里面有上下两个部分（具体情况和Android版本及主体有关），上面的是标题栏，下面的是内容栏。在Activity中通过setContentView所设置的布局文件其实就是被加到内容栏之中的，而内容栏的id是content，在代码中可以通过ViewGroup content = （ViewGroup)findViewById(R.android.id.content)来得到content对应的layout。 
 Window中有几个视图相关的比较重要的成员变量如下所示:

- **mDeco**r:DecorView的实例，标示Window内部的顶级视图
- **mContentRoot**:是DecorView的唯一子视图，**内部包含mContentParent,标题栏和状态栏。**
- **mContentParent**:setContetView所设置的布局文件就加到这个视图中
   Activity中不仅持有一个Window实例，还有一个类型为View的mDecor实例。这个实例和Window中的mDecor实例有什么关系呢？它又是什么时候被创建的呢？ 
     二者其实指向同一个对象，这个对象是在Activity调用setContentView时创建的。我们都知道Activity的setContentView实际上是**调用了Window的setContentView方法**。

```java
@Override
public void setContentView(int layoutResID) {
    if (mContentParent == null) { //[window]如何没有DecorView,那么就新建一个
        installDecor(); //[window]
    } else if (!hasFeature(FEATURE_CONTENT_TRANSITIONS)) {
        mContentParent.removeAllViews();
    }
    ....
    //[window]第二步,将layout添加到mContentParent
    mLayoutInflater.inflate(layoutResID, mContentParent);
    .....
}
```

代码很清楚的显示了布局文件的视图是添加到`mContentParent`中，而且`Window`通过`installDecor`来新建`DecorView`。

```java
//[window]创建一个decorView
private void installDecor() {
    if (mDecor == null) {
        mDecor = generateDecor(); //直接new出一个DecorView返回
        ....
    }
    if (mContentParent == null) {
        //[window] 这一步也是很重要的.
        mContentParent = generateLayout(mDecor); //mContentParent是setContentVIew的关键啊
        .....
    }
    ....
}
protected ViewGroup generateLayout(DecorView decor) {
    // Apply data from current theme.
    .......
    //[window] 根据不同的style生成不同的decorview啊
    View in = mLayoutInflater.inflate(layoutResource, null);
    // 加入到deco中,所以应该是其第一个child
    decor.addView(in, new ViewGroup.LayoutParams(MATCH_PARENT, MATCH_PARENT));
    mContentRoot = (ViewGroup) in; //给DecorView的第一个child是mContentView
    // 这是获得所谓的content 
    ViewGroup contentParent = (ViewGroup)findViewById(ID_ANDROID_CONTENT);
    }
    .....
    return contentParent;
}
```

 从上述的代码中，我们可以清楚的看到**mDecor**和**mContentParent**和**mContentRoot**的关系。 

那么，Activity中的**mDecor**是何时被赋值的？我们如何确定它和**Widnow**中的**mDecor**指向同一个对象呢？我们可以查看**ActivityThread**的**handleResumeActivity**函数，它负责处理**Activity**的**resume**阶段。在这个函数中，Android直接将**Window**中的**DecorView**实例赋值给**Activity**。

```java
final Activity a = r.activity;
r.window = r.activity.getWindow();
View decor = r.window.getDecorView();
decor.setVisibility(View.INVISIBLE);
ViewManager wm = a.getWindowManager();
WindowManager.LayoutParams l = r.window.getAttributes();
a.mDecor = decor;
```



### Window，DecorView 和 ViewRoot

 ViewRoot对应**ViewRootImpl**类，它是连接**WindowManagerService**和**DecorView**的纽带，View的三大流程(测量（measure），布局（layout），绘制（draw）)**均通过ViewRoot来完成**。ViewRoot并**不属于**View树的一份子。从源码实现上来看，它**既非View的子类，也非View的父类，**但是，它实现了ViewParent接口，这让它可以作为View的名义上的父视图。RootView继承了Handler类，可以接收事件并分发，Android的所有触屏事件、按键事件、界面刷新等事件**都是通过ViewRoot进行分发的**。ViewRoot可以被理解为“View树的管理者”——**它有一个mView成员变量，它指向的对象和上文中Window和Activity的mDecor指向的对象是同一个对象。**

  我们来先看一下ViewRoot的创建过程。由于ViewRoot作为WindowMangerService和DecorView的纽带，只有在WindowManager将持有DecorView的Window添加进窗口管理器**才创建**。我们可以查看WindowMangerGlobal中的addView函数。

```java
public void addView(View view, ViewGroup.LayoutParams params,
        Display display, Window parentWindow) {
        // 创建ViewRootImpl,然后将下述对象添加到列表中
    ....
    root = new ViewRootImpl(view.getContext(), display);

    view.setLayoutParams(wparams);

    mViews.add(view);
    mRoots.add(root);
    mParams.add(wparams);
    ....
    try {
        // 添加啦!!!!!!!!这是通过ViewRootImpl的setView来完成，这个View就是DecorView实例
        root.setView(view, wparams, panelParentView);
    } catch (RuntimeException e) {
      ....
    }
    ....
}
```

那么，Window是什么时候被添加到**WindowManager**中的呢？我们回到ActivityThread的**handleResumeActivity**函数。我们都知道Activity的resume阶段就是要显示到屏幕上的阶段，在Activity也就是**DecorView**将要显示到屏幕时，系统才会调用**addView**方法。 

我们在**handleResumeActivity**函数中找到了下面一段代码,它调用了Activity的**makeVisible**()函数。

```java
// ActivityThread
r.activity.makeVisible();

//Activity
    //[windows] DecorView正式添加并显示
    void makeVisible() {
        if (!mWindowAdded) {
            ViewManager wm = getWindowManager();
            wm.addView(mDecor, getWindow().getAttributes());
            mWindowAdded = true;
        }
        mDecor.setVisibility(View.VISIBLE);
    }
```

 我们通过源代码发现，正式在`makeVisible`函数中，系统进行了`Window`的添加。



从问题出发，往往能更明确的找到所求。结合源码，逐步解析Activity、Window、View的三者关系。

### 什么地方需要window？

- 一句话总结：有视图的地方就需要window
- Activity、Dialog、Toast...



### PopupWindow和Dialog有什么区别？

两者最根本的区别在于有没有新建一个window，PopupWindow没有新建，而是将view加到DecorView；Dialog是新建了一个window，相当于走了一遍Activity中创建window的流程

PopupWindow的相关源码

```java
public PopupWindow(View contentView, int width, int height, boolean focusable) {
    if (contentView != null) {
        mContext = contentView.getContext();
        mWindowManager = (WindowManager) mContext.getSystemService(Context.WINDOW_SERVICE);
    }

    setContentView(contentView);
    setWidth(width);
    setHeight(height);
    setFocusable(focusable);
}
```



```java
private void invokePopup(WindowManager.LayoutParams p) {
    if (mContext != null) {
        p.packageName = mContext.getPackageName();
    }

    final PopupDecorView decorView = mDecorView;
    decorView.setFitsSystemWindows(mLayoutInsetDecor);

    setLayoutDirectionFromAnchor();

    mWindowManager.addView(decorView, p);

    if (mEnterTransition != null) {
        decorView.requestEnterTransition(mEnterTransition);
    }
}
```

从源码中可以看出，**PopupWindow最终是执行了mWindowManager.addView方法，全程没有新建window**

Dialog的相关源码如下：

```java
Dialog(@NonNull Context context, @StyleRes int themeResId, boolean createContextThemeWrapper) {
    if (createContextThemeWrapper) {
        if (themeResId == 0) {
            final TypedValue outValue = new TypedValue();
            context.getTheme().resolveAttribute(R.attr.dialogTheme, outValue, true);
            themeResId = outValue.resourceId;
        }
        mContext = new ContextThemeWrapper(context, themeResId);
    } else {
        mContext = context;
    }

    mWindowManager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);

    final Window w = new PhoneWindow(mContext);
    mWindow = w;
    w.setCallback(this);
    w.setOnWindowDismissedCallback(this);
    w.setWindowManager(mWindowManager, null, null);
    w.setGravity(Gravity.CENTER);

    mListenersHandler = new ListenersHandler(this);
}
```

很明显，我们看到**Dialog执行了window的创建：final Window w = new PhoneWindow(mContext);**

**一句话概括三者的基本关系**

Activity中展示视图元素通过window来实现，window可以理解为一个容器，盛放着一个个的view，用来执行具体的展示工作。



### 各自是在何时被创建的？

以Activity举例

### Activity实例的创建

ActivityThread中执行performLaunchActivity，从而生成了Activity的实例：

```java
private Activity performLaunchActivity(ActivityClientRecord r, Intent customIntent) {
    ...
    Activity activity = null;
    try {
        java.lang.ClassLoader cl = r.packageInfo.getClassLoader();
        activity = mInstrumentation.newActivity(
                cl, component.getClassName(), r.intent);
        ...
    } catch (Exception e) {
        ...
    }

    try {
        ...
        if (activity != null) {
            ...
            activity.attach(appContext, this, getInstrumentation(), r.token,
                    r.ident, app, r.intent, r.activityInfo, title, r.parent,
                    r.embeddedID, r.lastNonConfigurationInstances, config,
                    r.referrer, r.voiceInteractor);
            ...
        }
        ...
    } catch (SuperNotCalledException e) {
        throw e;
    } catch (Exception e) {
        ...
    }

    return activity;
}
```



**Activity中Window的创建**

从上面的performLaunchActivity可以看出，在创建Activity实例的同时，会调用Activity的内部方法attach.

在该方法中完成window的初始化

```java
final void attach(Context context, ActivityThread aThread,
        Instrumentation instr, IBinder token, int ident,
        Application application, Intent intent, ActivityInfo info,
        CharSequence title, Activity parent, String id,
        NonConfigurationInstances lastNonConfigurationInstances,
        Configuration config, String referrer, IVoiceInteractor voiceInteractor) {
    ...

    mWindow = new PhoneWindow(this);
    mWindow.setCallback(this);

    ...

    mWindow.setWindowManager(
            (WindowManager)context.getSystemService(Context.WINDOW_SERVICE),
            mToken, mComponent.flattenToString(),
            (info.flags & ActivityInfo.FLAG_HARDWARE_ACCELERATED) != 0);
    ...
}
```



**DecorView的创建**

用户执行Activity的setContentView方法，内部是调用PhoneWindow的setContentView方法，在PhoneWindow中完成DecorView的创建

流程

- \1. **Activity**中的setContentView

- \2. **PhoneWindow**中的setContentView

- \3. **PhoneWindow**中的installDecor

```java
public void setContentView(@LayoutRes int layoutResID) {
    getWindow().setContentView(layoutResID);
    initWindowDecorActionBar();
}

@Override
public void setContentView(int layoutResID) {
    ...
    if (mContentParent == null) {
        installDecor();
    } else if (!hasFeature(FEATURE_CONTENT_TRANSITIONS)) {
        mContentParent.removeAllViews();
    }
    ...
}

private void installDecor() {
    if (mDecor == null) {
        mDecor = generateDecor();
        mDecor.setDescendantFocusability(ViewGroup.FOCUS_AFTER_DESCENDANTS);
        mDecor.setIsRootNamespace(true);
        if (!mInvalidatePanelMenuPosted && mInvalidatePanelMenuFeatures != 0) {
            mDecor.postOnAnimation(mInvalidatePanelMenuRunnable);
        }
    }
    ...
}
```



**Activity中的mDecor和Window里面的mDecor有什么关系？**

- 两者指向同一个对象，都是DecorView
- Activity中的mDecor是通过ActivityThread中的handleResumeActivity方法来赋值的

```java
final void handleResumeActivity(IBinder token,
        boolean clearHide, boolean isForward, boolean reallyResume) {
    ...
    if (r != null) {
        ...

        if (r.window == null && !a.mFinished && willBeVisible) {
            r.window = r.activity.getWindow();
            View decor = r.window.getDecorView();
            ...
            a.mDecor = decor;
            ...
        } else if (!willBeVisible) {
            ...
        }
        ...
    } else {
        ...
    }
}
```



**ViewRoot是什么？ViewRootImpl又是什么？**

1. ViewRoot的实现类是ViewRootImpl，是WindowManagerService和DecorView的纽带
2. ViewRoot不是View的根节点
3. View的绘制是从ViewRootImpl的performTraversals方法开始的



### ViewRootImpl何时创建？

当window被装进WindowManager时，完成ViewRootImpl的创建，最终是通过WindowManagerGlobal.addView方法中进行创建的

```java
public void addView(View view, ViewGroup.LayoutParams params, Display display, Window parentWindow) {
    ...
    root = new ViewRootImpl(view.getContext(), display);

    view.setLayoutParams(wparams);

    mViews.add(view);
    mRoots.add(root);
    mParams.add(wparams);
    ...
    try {
        root.setView(view, wparams, panelParentView);
    } catch (RuntimeException e) {
      ...
    }
    ...
}
```



**Activity中的Window何时被装进WindowManager？**

- 发生在Activity的onResume阶段

- 执行顺序

- - ActivityThread中的handleResumeActivity
  - Activity中的makeVisible

```java
final void handleResumeActivity(IBinder token, boolean clearHide, boolean isForward, boolean reallyResume) {
    ...
    if (r != null) {
        ...
        // The window is now visible if it has been added, we are not
        // simply finishing, and we are not starting another activity.
        if (!r.activity.mFinished && willBeVisible
                && r.activity.mDecor != null && !r.hideForNow) {
            ...
            if (r.activity.mVisibleFromClient) {
                r.activity.makeVisible();
            }
        }
        ...
    } else {
        ...
    }
}
```



```java
void makeVisible() {
    if (!mWindowAdded) {
        ViewManager wm = getWindowManager();
        wm.addView(mDecor, getWindow().getAttributes());
        mWindowAdded = true;
    }
    mDecor.setVisibility(View.VISIBLE);
}
```



### WindowManagerXXX

**什么是WindowManagerGlobal？WindowManager、WindowManagerGlobal、WindowManagerImpl、WindowManagerPolicy有什么区别？**

- WindowManagerGlobal中实现了ViewManager中addView、removeView、updateViewLayout这个三个view相关的方法。

- WindowManager是一个接口类，对应的实现类是WindowManagerImpl，该实现类中持有mGlobal对象，这个mGlobal对象就是WindowManagerGlobal，具体的实现交给了WindowManagerGlobal
- WindowManagerImpl相当于WindowManagerGlobal的代理类。

- WindowManagerPolicy提供所有和UI有关的接口，PhoneWindowManager实现了该接口。需要注意的是PhoneWindowManager并不是WindowManager的子类。WindowManagerService中持有mPolicy对象，这个mPolicy就是PhoneWindowManager。

```java
public class WindowManagerService extends IWindowManager.Stub implements Watchdog.Monitor, WindowManagerPolicy.WindowManagerFuncs {  
    ... 
    final WindowManagerPolicy mPolicy = new PhoneWindowManager();  
    ...
}
```



### Window的作用是什么？

- 实现了Activity与View的解耦，Activity将视图的全部工作都交给Window来处理
- WindowManager支持对多条View链的管理



### Dialog为什么不能使用Application的Context

- Dialog窗口类型是TYPE_APPLICATION，与Activity一样
- TYPE_APPLICATION要求Token不能为null，Application没有AppWindowToken

源码分析如下

```java
Dialog(@NonNull Context context, @StyleRes int themeResId, boolean createContextThemeWrapper) {
    ...
    mWindowManager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
    ...
    w.setWindowManager(mWindowManager, null, null);
    ...
}
```



```java
public void setWindowManager(WindowManager wm, IBinder appToken, String appName)
```



上面是Dialog的初始化方法，注意setWindowManager方法中，**第二个参数是appToken，Dialog初始化默认直接传入的是null，这是与Activity的一个显著区别**。此时又会有一个新的疑问，那无论Context是来自Application还是Activity，都是传入null，那为什么Application会报错呢。



其实，上面的getSystemService也是问题的原因之一。

我们先看一下Activity中的**getSystemService**方法

```java
@Override
public Object getSystemService(@ServiceName @NonNull String name) {
    if (getBaseContext() == null) {
        throw new IllegalStateException(
                "System services not available to Activities before onCreate()");
    }

    if (WINDOW_SERVICE.equals(name)) {
        return mWindowManager;
    } else if (SEARCH_SERVICE.equals(name)) {
        ensureSearchManager();
        return mSearchManager;
    }
    return super.getSystemService(name);
}
```

Activity中重写了getSystemService方法，按照Dialog中初始化的代码分析，此时返回的是当前Activity的**mWindowManager**，这个mWindowManager中存在**Activity**的**Token**。

Application中的Context**没有重写getSystemService方法**，那么在setWindowManager内部，会通过执行createLocalWindowManager方法获得一个WindowManager接口类的实现类WindowManagerImpl，在WindowManagerImpl中mDefaultToken默认也是null

```java
public void setWindowManager(WindowManager wm, IBinder appToken, String appName,
        boolean hardwareAccelerated) {
    mAppToken = appToken;
    mAppName = appName;
    mHardwareAccelerated = hardwareAccelerated
            || SystemProperties.getBoolean(PROPERTY_HARDWARE_UI, false);
    if (wm == null) {
        wm = (WindowManager)mContext.getSystemService(Context.WINDOW_SERVICE);
    }
    mWindowManager = ((WindowManagerImpl)wm).createLocalWindowManager(this);
}
```

```java
public WindowManagerImpl createLocalWindowManager(Window parentWindow) {
    return new WindowManagerImpl(mDisplay, parentWindow);
}
```

那么为什么appToken是null，就会报错呢？

```java
Caused by: android.view.WindowManager$BadTokenException: Unable to add window -- token null is not for an application
    at android.view.ViewRootImpl.setView(ViewRootImpl.java:685)
    at android.view.WindowManagerGlobal.addView(WindowManagerGlobal.java:342)
    at android.view.WindowManagerImpl.addView(WindowManagerImpl.java:93)
    at android.app.Dialog.show(Dialog.java:316)
```

从Logcat中的error信息可以看出，问题是出在了ViewRootImpl的setView方法中

```java
public void setView(View view, WindowManager.LayoutParams attrs, View panelParentView) {
    synchronized (this) {
        if (mView == null) {
            ...
            int res; /* = WindowManagerImpl.ADD_OKAY; */
            ...
            try {
                ...
                res = mWindowSession.addToDisplay(mWindow, mSeq, mWindowAttributes,
                        getHostVisibility(), mDisplay.getDisplayId(),
                        mAttachInfo.mContentInsets, mAttachInfo.mStableInsets,
                        mAttachInfo.mOutsets, mInputChannel);
            } catch (RemoteException e) {
                ...
            } finally {
                ...
            }
            ...

            if (res < WindowManagerGlobal.ADD_OKAY) {
                ...
                switch (res) {
                    ...
                    case WindowManagerGlobal.ADD_NOT_APP_TOKEN:
                        throw new WindowManager.BadTokenException(
                                "Unable to add window -- token " + attrs.token
                                + " is not for an application");
                    ...
                }
                ...
            }
            ...
        }
    }
}
```

从上述源码中可以看出**res**在执行完**addToDisplay**方法后，被置为了一个**非法值**，从而报错。

mWindowSession是IWindowSession接口类，IWindowSession的实现类是Session，从Session的源码中我们发现，最终是由WindowManagerService中的addWindow方法对res进行了赋值

```java
@Override
public int addToDisplay(IWindow window, int seq, WindowManager.LayoutParams attrs,
        int viewVisibility, int displayId, Rect outContentInsets, Rect outStableInsets,
        Rect outOutsets, InputChannel outInputChannel) {
    return mService.addWindow(this, window, seq, attrs, viewVisibility, displayId,
            outContentInsets, outStableInsets, outOutsets, outInputChannel);
}
```

下面是**addWindow**方法的源码，我们看到由于AppWindowToken为null，从而返回了非法值。

```java
public int addWindow(Session session, IWindow client, int seq,
        WindowManager.LayoutParams attrs, int viewVisibility, int displayId,
        Rect outContentInsets, Rect outStableInsets, Rect outOutsets,
        InputChannel outInputChannel) {
    ...
    final int type = attrs.type;

    synchronized(mWindowMap) {
        ...
        WindowToken token = mTokenMap.get(attrs.token);
        if (token == null) {
            ...
        } else if (type >= FIRST_APPLICATION_WINDOW && type <= LAST_APPLICATION_WINDOW) {
            AppWindowToken atoken = token.appWindowToken;
            if (atoken == null) {
                Slog.w(TAG, "Attempted to add window with non-application token "
                      + token + ".  Aborting.");
                return WindowManagerGlobal.ADD_NOT_APP_TOKEN;
            } else if (atoken.removed) {
                ...
            }
            ...
        } 
        ...
    }
    ...
}
```