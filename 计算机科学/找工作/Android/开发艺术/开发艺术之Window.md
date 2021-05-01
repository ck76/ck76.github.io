#### 多数情况我们是和[Activity](https://www.jianshu.com/p/602b1ec4ca7a)和[View](https://www.jianshu.com/p/06ff0dfeed39)打交道，在之前学习中也都接触过，本篇来深入学习和它们有紧密联系的Window，主要内容：

- Window&WindowManager
- Window的内部机制（添加、删除、更新）
- Window的创建过程（Activity、Dialog、Toast）

------

**1.Window&WindowManager**

a.Window&PhoneWindow：
**Window**是一个抽象类，它定义了顶级窗体样式和行为。其唯一的实现类是**PhoneWindow**。

**推荐阅读**：[Window，PhoneWindow，DecorView，setContentView源码理解](https://www.jianshu.com/p/e42b638944ae)

b.Window&View：
每个Window都对应一个**View**和一个**ViewRootImpl**，Window和View通过ViewRootImpl来建立联系。Window并不可见，它实际以View的形式存在，它是View的**直接管理者**。

c.Window&WindowManager：
实际使用中无法访问Window，对Window的访问必须通过**WindowManager**，对Window的操作通过它完成。

- 例如：通过WindowManager添加Window



```cpp
//将一个Button添加到屏幕为（100,300）的位置
mFloatingButton = new Button(this);
mFloatingButton.setText("test button");

mLayoutParams = new WindowManager.LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT, 0, 0,PixelFormat.TRANSPARENT);//第三个参数代表flags，第四个参数代表type

mLayoutParams.flags = LayoutParams.FLAG_NOT_TOUCH_MODAL
        | LayoutParams.FLAG_NOT_FOCUSABLE
        | LayoutParams.FLAG_SHOW_WHEN_LOCKED;//配置flags
mLayoutParams.type = LayoutParams.TYPE_SYSTEM_ERROR;//配置type
mLayoutParams.gravity = Gravity.LEFT | Gravity.TOP;//配置gravity
mLayoutParams.x = 100;//相对于gravity
mLayoutParams.y = 300;//相对于gravity

mFloatingButton.setOnTouchListener(this);
mWindowManager.addView(mFloatingButton, mLayoutParams);
```

这里依次介绍WindowManager的三个重要参数：

- flags

  ：表示Window的属性。主要的可选值含义：

  - **FLAG_NOT_FOCUSABLE**：表示Window不需要获取焦点，也不需要接收各种输入事件，此标记会同时启动FLAG_NOT_TOUCH_MODEL，最终事件会传递给下层的具有焦点的Window。
  - **FLAG_NOT_TOUCH_MODAL**：表示系统会将当前Window区域**以外**的单击事件传递给底层的Window，而区域**以内**的单击事件则自己处理。一般都需要开启此标记，否则其他Window将无法收到单击事件。
  - **FLAG_SHOW_WHEN_LOCKED**：表示Window可显示在锁屏界面。

- type

  ：表示Window的类型。Window有三种类型：

  - **应用Window**：对应一个Activity。
  - **子Window**：不能单独存在，需附属特定的父Window。如Dialog。
  - **系统Window**： 需申明权限才能创建。如Toast。

> - Window是分层的，见下表。
> - 层级大的会**覆盖**在层级小的Window上面。
> - 对应WindowManager.**LayoutParams**的type参数。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp2k6m4vj30hb042mx7.jpg)

- gravity

  ：表示Window的位置。

  - 默认是屏幕中间。
  - **x**、**y**值相对于gravity。

d.WindowManager&WindowManagerService：
Window的具体实现位于**WindowManagerService**中。WindowManager和WindowManagerService的交互是一个IPC（跨进程通信）过程。

------

**2.Window的内部机制**

- WindowManager对Window主要有三大操作：**添加、更新和删除**。这三个方法主要是定义在**ViewManager**接口中：



```csharp
public interface ViewManager
{
    public void addView(View view, ViewGroup.LayoutParams params);//添加过程
    public void updateViewLayout(View view, ViewGroup.LayoutParams params);//更新过程
    public void removeView(View view);//删除过程
}
```

- WindowManager也是一个接口，它继承了ViewManager接口：



```java
public interface WindowManager extends ViewManager {}
```

- WindowManager的具体实现类是**WindowManagerImpl** ：



```java
public final class WindowManagerImpl implements WindowManager{
        @Override
        public void addView(View view, ViewGroup.LayoutParams params){
            mGlobal.addView(view, params, mDisplay, mParentWindow);
        }
        
        @Override
        public void updateViewLayout(View view, ViewGroup.LayoutParams params){
            mGlobal.updateViewLayout(view, params);
        }
        
        @Override
        public void removeView(View view){
            mGlobal.removeView(view, false);
        }
}
```

- 由以上代码可见，WindowManagerImpl并没有**直接**实现Window的三大操作，而是交给了**WindowManagerGlobal**。WindowManagerGlobal以**单例模式**向外提供自己的实例：



```java
private final WindowManagerGlobal mGlobal = WindowManagerGlobal.getInstance();
```

一幅图说明这几个类的关系：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp2h8oe8j30ji0bpa9x.jpg)

> 因此，通过WindowManagerGlobal的addView()、updateViewLayout()、removeView()实现WindowManager对Window的添加、删除和修改。

下面分别来看WindowManagerGlobal对Window操作的大致过程：

a.Window的**添加**过程：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp2fxop1j30up0cvdh1.jpg)

b.Window的**删除**过程

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp2ef636j30wy08pq3q.jpg)

c.Window的**更新**过程

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp2d1graj30x308gdgh.jpg)

> 不难发现， 以上验证了之前的总结：
>
> - Windows的三大操作最终都会通过一个IPC过程移交给WindowManagerService。
> - Window和View通过ViewRootImpl来联系，ViewRootImpl可控制View的测量、布局和重绘。

推荐阅读：[源码剖析之------Window的内部实现机制](https://link.jianshu.com/?t=http%3A%2F%2Fblog.csdn.net%2Fchenglei_56%2Farticle%2Fdetails%2F50544854)、[我眼中的Window创建/添加/删除/更新过程](https://link.jianshu.com/?t=http%3A%2F%2Fblog.csdn.net%2Fhzw19920329%2Farticle%2Fdetails%2F52423771)

------

**3.Window的创建过程**

由于View必须依附Window才能呈现出来，因此有View的地方必有Window。在Android中可以提供View的地方有Activity、Dialog和Toast，下面分别来看以上三种Window的大致创建过程：

a.**Activity**的Window创建过程

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp2bofcij30ut0cjmyv.jpg)

**推荐阅读**：[Activity的Window创建过程分析（源码）](https://link.jianshu.com/?t=http%3A%2F%2Fblog.csdn.net%2FLuoshengyang%2Farticle%2Fdetails%2F8223770)

b.**Dialog**的Window创建过程

Step1：创建WindowDialog。和Activity类似，同样是通过**PolicyManager.makeNewWindow()**来实现。

Step2：初始化DecorView并将Dialog的视图添加到DecorView中去。和Activity类似，同样是通过**Window.setContentView()**来实现。

Step3：将DecorView添加到Window中显示。和Activity一样，都是在自身要出现在前台时才会将添加Window。

- **Dialog.show()**方法：完成DecorView的显示。
- **WindowManager.remoteViewImmediate()**方法：当Dialog被dismiss时移除DecorView。

c.**Toast**的Window创建过程

①Toast的内部的视图由两种方式指定：

- 系统默认的样式；
- 通过setView()指定一个自定义View。

[这里见技能篇之Toast的使用](https://www.jianshu.com/p/c9496c8bed4c)

②Toast具有定时取消功能，故系统采用**Handler**做定时处理。

③在Toast内部有两类IPC过程：

- Toast访问**NotificationManagerService**(NMS)；
- NotificationManagerService回调Toast里的**TN**接口。

④Toast提供方法**show()**和**cancel()**分别用于显示和隐藏Toast。

- Toast的显示和隐藏都需要通过**NMS**来实现，由于NMS运行在**系统进程**中，故需通过**远程调用**的方式来进行显示和隐藏Toast。
- NMS处理Toast的显示和隐藏请求时会跨进程回调**TN**中的方法，由于TN运行在**Binder线程池**中，故需通过**Handler**将其切换到当前线程（发送Toast请求的线程）。

> NMS只是起到了管理Toast队列及其延时的效果，Toast 的显示和隐藏实际是通过TN来实现的。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1glvp2aegitj30g90eadh3.jpg)

推荐阅读：[Android对话框Dialog，PopupWindow，Toast的实现机制](https://link.jianshu.com/?t=http%3A%2F%2Fblog.csdn.net%2Ffeiduclear_up%2Farticle%2Fdetails%2F49080587)



作者：厘米姑娘
链接：https://www.jianshu.com/p/ed03aed9a4db
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。