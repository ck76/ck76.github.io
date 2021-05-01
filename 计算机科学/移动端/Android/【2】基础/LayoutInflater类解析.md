[TOC]

## LayoutInflater简介

**在实际工作中，事先写好的布局文件往往不能满足我们的需求，有时会根据情况在代码中自定义控件，这就需要用到LayoutInflater。**
**LayoutInflater在Android中是“扩展”的意思，作用类似于findViewById()，不同的是LayoutInflater是用来获得布局文件对象的，而**

**findViewById()是用来获得具体控件的。LayoutInflater经常在BaseAdapter的getView方法中用到，用来获取整个View并返回。**

**在实际开发中LayoutInflater这个类还是非常有用的，它的作用类似于findViewById()。不同点是LayoutInflater是用来找res/layout/下的xml布局文件，并且实例化；而findViewById()是找xml布局文件下的具体widget控件(如Button、TextView等)。**

<!--more-->



### 具体作用：

1、对于一个没有被载入或者想要动态载入的界面，都需要使用LayoutInflater.inflate()来载入；

2、对于一个已经载入的界面，就可以使用Activiyt.findViewById()方法来获得其中的界面元素。

LayoutInflater 是一个抽象类，在文档中如下声明：

`public abstract class LayoutInflater extends Object`

### 获得 LayoutInflater 实例的三种方式

- ```java
  LayoutInflater inflater = LayoutInflater.from(this);  
  View layout = inflater.inflate(R.layout.main, null);  
  ```

- ```java
  LayoutInflater inflater = getLayoutInflater();  
  View layout = inflater.inflate(R.layout.main, null);  
  ```

- ```java
  LayoutInflater inflater = (LayoutInflater) getSystemService(LAYOUT_INFLATER_SERVICE);  
  View layout = inflater.inflate(R.layout.main, null);  
  ```

第一种方法的本质就是调用第三种方法 。并且第一种方法只能在Activity里用。

### 注意：

- inflate 方法与 findViewById 方法不同；
- inflater 是用来找 res/layout 下的 xml 布局文件，并且实例化；
- findViewById() 是找具体 xml 布局文件中的具体 widget 控件(如:Button、TextView 等)。



## inflate()方法

### 三个参数的inflate方法

```java
public View inflate(@LayoutRes int resource, @Nullable ViewGroup root, boolean attachToRoot)
```

- **root为null**

当root为null时，不论attachToRoot为true还是为false，显示效果都是一样的。当root为null表示我不需要将第一个参数所指定的布局添加到任何容器中，同时也表示没有任何容器来来协助第一个参数所指定布局的根节点生成布局参数 。

- **root不为null，attachToRoot为true**

当root不为null，attachToRoot为true时，表示将resource指定的布局添加到root中，添加的过程中resource所指定的的布局的根节点的各个属性都是有效的。 

**Activity的布局如下** 

```java

<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:orientation="vertical"
    android:id="@+id/ll"
    tools:context="org.sang.layoutinflater.MainActivity">
</LinearLayout>

```

**布局linearlayout.xml如下：**

```xml
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:id="@+id/ll"
    android:layout_width="200dp"
    android:layout_height="200dp"
    android:background="@color/colorPrimary"
    android:gravity="center"
    android:orientation="vertical">
 
    <Button
        android:layout_width="wrap_content"
        android:layout_height="wrap_content" />
</LinearLayout>
```

**Activity里的代码**

```java
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        LinearLayout ll = (LinearLayout) findViewById(R.id.ll);
        LayoutInflater inflater = LayoutInflater.from(this);
        inflater.inflate(R.layout.linearlayout, ll,true);
    }
```

这里我都没写将inflate出来的View添加到ll中的代码，但是linearlayout布局文件就已经添加进来了，这就是因为我第三个参数设置为了true，表示将第一个参数所指定的布局添加到第二个参数的View中。最终显示效果如下： 

![inflater三参](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/FIfuhCXGaz82m4X8ml97xBpgc3X*KLx8qlt6x6yfXw8!/r/dDQBAAAAAAAA)

如果作死多写这么一行代码，如下：

```java
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        LinearLayout ll = (LinearLayout) findViewById(R.id.ll);
        LayoutInflater inflater = LayoutInflater.from(this);
        View view = inflater.inflate(R.layout.linearlayout, ll, true);
        //多写的代码...........
        ll.addView(view);
    } 
```

这个时候再运行，系统会抛如下异常：

```java
java.lang.IllegalStateException: The specified child already has a parent. You must call removeView() on the child's parent first. 
```

原因就是因为当第三个参数为true时，会自动将第一个参数所指定的View添加到第二个参数所指定的View中。

- **root不为null，attachToRoot为false**

如果root不为null，而attachToRoot为false的话，表示不将第一个参数所指定的View添加到root中，那么这个时候有的小伙伴可能就有疑问了，既然不添加到root中，那我还写这么多干嘛？我第二个参数直接给null不就可以了？其实不然，这里涉及到另外一个问题：我们在开发的过程中给控件所指定的layout_width和layout_height到底是什么意思？该属性的表示一个控件在容器中的大小，就是说这个控件必须在容器中，这个属性才有意义，否则无意义。这就意味着如果我直接将linearlayout加载进来而不给它指定一个父布局，则inflate布局的根节点的layout_width和layout_height属性将会失效（因为这个时候linearlayout将不处于任何容器中，那么它的根节点的宽高自然会失效）。如果我想让linearlayout的根节点有效，又不想让其处于某一个容器中，那我就可以设置root不为null，而attachToRoot为false。这样，指定root的目的也就很明确了，即root会协助linearlayout的根节点生成布局参数，只有这一个作用。OK，还是上面的布局文件，如果我想将之添加到activity的布局中又该如何呢？

```java
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        LinearLayout ll = (LinearLayout) findViewById(R.id.ll);
        LayoutInflater inflater = LayoutInflater.from(this);
        View view = inflater.inflate(R.layout.linearlayout, ll, false);
        ll.addView(view);
    }

```

大家注意，这个时候**需要手动的将inflate加载进来的view添加到ll容器中，**因为inflate的最后一个参数false表示不将linealayout添加到ll中。显示效果和上文一样，不再贴图。



### 两个参数的inflate方法

```java
public View inflate(XmlPullParser parser, @Nullable ViewGroup root) {
        return inflate(parser, root, root != null);
    }
```

这是两个参数的inflate方法，大家注意两个参数实际上最终也是调用了三个参数。

两个参数的inflate方法分为**如下两种情况：**

**1.root为null，等同于上述root为null所述情况。**

**2.root不为null，等同于root不为null，attachToRoot为true所述情况。**



----

## 结尾补充getSystemService()方法

`getSystemService`是Activity中的方法，根据传入的name来取得对应的服务对象，这些服务名称参数都是Context类中的常量： 

|      传入的Name  | 返回的对象 | 说明 |
| ---- | ---- | ---- |
| WINDOW_SERVICE | WindowManager | 管理打开的窗口程序 |
| LAYOUT_INFLATER_SERVICE | LayoutInflater | 取得xml里定义的view |
| ACTIVITY_SERVICE | ActivityManager | 管理应用程序的系统状态 |
| POWER_SERVICE | PowerManger | 电源的服务 |
| ALARM_SERVICE | AlarmManager | 闹钟的服务 |
| NOTIFICATION_SERVICE | NotificationManager | 状态栏的服务 |
| KEYGUARD_SERVICE | KeyguardManager | 键盘锁的服务 |
| LOCATION_SERVICE | LocationManager | 位置的服务，如GPS |
| SEARCH_SERVICE | SearchManager | 搜索的服务 |
| VEBRATOR_SERVICE | Vebrator | 手机震动的服务 |
| CONNECTIVITY_SERVICE | Connectivity | 网络连接的服务 |
| WIFI_SERVICE | WifiManager | Wi-Fi服务 |
| TELEPHONY_SERVICE | TeleponyManager | 电话服务 |
