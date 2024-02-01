[TOC]

Android Support Library(安卓兼容包)是为了构件一个可以跑在不同版本Android平台的软件。它重构了AppCompat，在新的AppCompat中，加入主题色，Toolbar等功能。在新版本中推荐使用AppCompatActivity代替ActionBarActivity。

对于android:theme，新版本AppCompat允许了Toolbar使用android:theme代替app:theme,兼容API11+

<!--more-->

## AppCompatActivity简介

**AppCompatActivity日常中用到的最多的部分就还是ToolBar代替ActionBar。**

ActionBar向前兼容，出现在support v7里，如果需要使用兼容版的actionbar，则继承support v7提供的ActionBarActivity（它是继承FragmentActivity的）。当推出Android 5.0之后，提供了很多很多新东西，于是support v7也更新了，出现了AppCompatActivity。AppCompatActivity是用来替代ActionBarActivity的，如果当你把代码中的父类改为ActionBarActivity时，会发现提示已经过时。  

- 支持包更新到22.1之后，Android Studio项目的Activity默认继承这个类。
- ActionBarActivity已替代ActionBarActivity 

Hello World的MainActivity

```java
import android support.v7.app.AppcompatActivity;

public class MainActivity extends AppCompatActivity{
......
}
```


## ToolBar的使用过程

- **注意文件styles.xml，要使用Theme.AppCompat.NoActionBar这个主题，不然后面会导致你重复添加报错：** 

AndroidManifest.xml

```xml
 <application
        android:name=".App"
        android:allowBackup="true"
        android:icon="@mipmap/ic_launcher"
        android:label="@string/app_name"
        android:roundIcon="@mipmap/ic_launcher_round"
        android:supportsRtl="true"
        android:theme="@style/AppTheme">
```

styles.xml

```xml
    <!-- 设置无ActionBar-->
    <style name="AppTheme" parent="Theme.AppCompat.Light.NoActionBar">
        <!-- Customize your theme here. -->
        <!--应用的主要色调，actionBar默认使用该颜色，Toolbar导航栏的底色-->
        <item name="colorPrimary">@color/colorPrimary</item>
        <!--应用的主要暗色调，statusBarColor默认使用该颜色-->
        <item name="colorPrimaryDark">@color/colorPrimaryDark</item>
        <!--控件选中后的颜色-->
        <item name="colorAccent">@color/colorAccent</item>
    </style>
```
- 在layout下新建一个xml文件，包含ToolBar： 

```xml
<?xml version="1.0" encoding="utf-8"?>
<android.support.v7.widget.Toolbar xmlns:android="http://schemas.android.com/apk/res/android"
xmlns:app="http://schemas.android.com/apk/res-auto"
android:id="@+id/toolbar"
android:layout_width="match_parent"
android:layout_height="wrap_content"
android:background="?attr/colorPrimary"
android:minHeight="?attr/actionBarSize" >

<TextView
    android:id="@+id/toolbar_title"
    android:layout_width="wrap_content"
    android:layout_height="wrap_content"
    android:layout_gravity="center"
    android:text="Toolbar Title"
    android:textAppearance="?android:attr/textAppearanceLarge" />
</android.support.v7.widget.Toolbar>
```

- 布局文件activity_main中include进去Toolbar，ToolBar 有高度的灵活性，可以放在布局中的任意位置。 

```xml
<?xml version="1.0" encoding="utf-8"?>
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
xmlns:tools="http://schemas.android.com/tools" android:layout_width="match_parent"
android:layout_height="match_parent"
tools:context=".MainActivity">
<include
    layout="@layout/toolbar"
    />
</RelativeLayout>
```

- Activity中进行初始化 

```java
public class MainActivity extends AppCompatActivity {

@Override
protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_main);
    init();
}

private void init(){
    Toolbar toolbar = (Toolbar)findViewById(R.id.toolbar);
    toolbar.setTitle("");
    setSupportActionBar(toolbar);
```

