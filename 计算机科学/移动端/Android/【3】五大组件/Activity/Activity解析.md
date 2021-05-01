---
title: Activity解析
date: 2018-08-02 18:55:51
categories: "Android五大组件"
tags:
---

文章会介绍Activity有关的一些东西。

<!--more-->

## Activity是什么

我们都知道android中有四大组件（Activity 活动，Service 服务，Content Provider 内容提供者，BroadcastReceiver 广播接收器），Activity是我们用的最多也是最基本的组件，因为应用的所有操作都与用户相关，Activity 提供窗口来和用户进行交互。

官方文档这么说： 　　

> An activity is a single, focused thing that the user can do. Almost all activities interact with the user, so the Activity class takes care of creating a window for you in which you can place your UI with setContentView(View).
>
> 大概的意思：

> activity是独立平等的，用来处理用户操作。几乎所有的activity都是用来和用户交互的，所以activity类会创建了一个窗口，开发者可以通过setContentView(View)的接口把UI放到给窗口上。

　　Android中的activity全都归属于task管理 。task 是多个 activity 的集合，这些 activity 按照启动顺序排队存入一个栈（即“back stack”）。android默认会为每个App维持一个task来存放该app的所有activity，task的默认name为该app的packagename。

　　当然我们也可以在AndroidMainfest.xml中申明activity的taskAffinity属性来自定义task，但不建议使用，如果其他app也申明相同的task，它就有可能启动到你的activity，带来各种安全问题（比如拿到你的Intent）。

\#Activity的内部调用过程

　　上面已经说了，系统通过堆栈来管理activity，当一个新的activity开始时，它被放置在堆栈的顶部和成为运行活动，以前的activity始终保持低于它在堆栈，而不会再次到达前台，直到新的活动退出。

上一张Activity生命周期图

![Activity生命周期](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/pEtCoyTyUBV4whtj0J1eBFAZQZpHKwWU2Cg1rPihdo8!/r/dFMBAAAAAAAA)

- 首先打开一个新的activity实例的时候，系统会依次调用

> onCreate（）  -> onStart() -> onResume() 然后开始running

　　running的时候被覆盖了（从它打开了新的activity或是被锁屏，但是它**依然在前台**运行， lost focus but is still visible），系统调用onPause();

> 　该方法执行activity暂停，通常用于提交未保存的更改到持久化数据，停止动画和其他的东西。但这个activity还是完全活着（它保持所有的状态和成员信息，并保持连接到**窗口管理器**）

接下来它有三条出路 ①用户返回到该activity就调用onResume()方法重新running

②用户回到桌面或是打开其他activity，就会调用onStop()进入停止状态（保留所有的状态和成员信息，**对用户不可见**）

③系统内存不足，拥有更高限权的应用需要内存，那么该activity的进程就可能会被系统回收。（回收onRause()和onStop()状态的activity进程）要想重新打开就必须重新创建一遍。

如果用户返回到onStop()状态的activity（又显示在前台了），系统会调用

> onRestart() ->  onStart() -> onResume() 然后重新running

在activity结束（调用finish ()）或是被系统杀死之前会调用onDestroy()方法释放所有占用的资源。

> activity生命周期中三个嵌套的循环

- activity的完整生存期会在 onCreate() 调用和 onDestroy() 调用之间发生。　
- activity的可见生存期会在 onStart() 调用和 onStop() 调用之间发生。系统会在activity的整个生存期内多次调用 onStart() 和onStop()， 因为activity可能会在显示和隐藏之间不断地来回切换。　
- activity的前后台切换会在 onResume() 调用和 onPause() 之间发生。 因为这个状态可能会经常发生转换，为了避免切换迟缓引起的用户等待，这两个方法中的代码应该相当地轻量化。

## 竖屏切换时Activity会发生什么？

https://blog.csdn.net/zhaokaiqiang1992/article/details/19921703?utm_source=blogxgwz0

- 不设置Activity的android:configChanges时
  - 切屏**会**重新调用各个生命周期，切横屏时会执行一次，切竖屏时会执行两次
- 设置Activity的android:configChanges=”orientation”时
  - 切屏还是**会**重新调用各个生命周期，切横、竖屏时只会执行一次 
- 设置Activity的android:configChanges=”orientation|keyboardHidden”时
  - 切屏**不会**重新调用各个生命周期，只会执行onConfigurationChanged方法 

## 弹出Dialog按下Home键时Activity生命周期