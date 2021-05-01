## 简单介绍

### 使用原因：

在使用rxjava的时候，如果没有及时解除订阅，在退出activity的时候，异步线程还在执行。对activity还存在引用，此时就会产生内存泄漏。RxLifecycle就是为了解决rxjava导致的内存泄漏而产生的。

### RxLifecycle可以做到什么呢？　

它可以让Observable发布的事件和当前的组件绑定，实现生命周期同步。从而实现当前组件生命周期结束时，自动取消对Observable订阅。核心思想：通过监听Activity、Fragment的生命周期，来自动断开订阅防止内存泄漏。

<!--more-->

## 使用方法

### 添加依赖

```java
	//retrofit okhttp rxjava
    implementation 'com.squareup.retrofit2:converter-gson:2.4.0'
    implementation 'com.squareup.retrofit2:retrofit:2.4.0'
    implementation 'com.squareup.retrofit2:adapter-rxjava2:2.4.0'
    implementation 'com.squareup.okhttp3:okhttp:3.10.0'
    implementation 'com.squareup.okhttp3:logging-interceptor:3.10.0'
    implementation 'io.reactivex.rxjava2:rxjava:2.1.9'
    implementation 'io.reactivex.rxjava2:rxandroid:2.0.2'

    //生命周期管理基础库
    implementation 'com.trello.rxlifecycle2:rxlifecycle:2.2.1'

    // If you want to bind to Android-specific lifecycles
    // Android使用的库，里面使用了Android的生命周期方法
    // 内部引用了基础库，如果使用此库则无需再引用基础库
    implementation 'com.trello.rxlifecycle2:rxlifecycle-android:2.2.1'

    // If you want pre-written Activities and Fragments you can subclass as providers
    // Android组件库，里面定义了例如RxAppCompatActivity、RxFragment之类的Android组件
    // 内部引用了基础库和Android库，如果使用此库则无需再重复引用
    implementation 'com.trello.rxlifecycle2:rxlifecycle-components:2.2.1'
       
    // Android使用的库，继承NaviActivity使用
	compile 'com.trello.rxlifecycle2:rxlifecycle-navi:2.1.0'

	// Android使用的库，继承LifecycleActivity使用
	// 需要引入Google的仓库支持，用法和rxlifecycle-navi类似
	compile 'com.trello.rxlifecycle2:rxlifecycle-android-lifecycle:2.1.0'

	// Google的仓库支持
	allprojects {
    	repositories {
        	jcenter()
        	maven { url 'https://dl.google.com/dl/android/maven2/' }
    	}
	}

	// 支持Kotlin语法的RxLifecycle基础库
	compile 'com.trello.rxlifecycle2:rxlifecycle-kotlin:2.1.0'

	// 支持Kotlin语法的Android库
	compile 'com.trello.rxlifecycle2:rxlifecycle-android-lifecycle-kotlin:2.1.0'
```

### Activity和Fragment继承

```java
public abstract class BaseActivity extends RxAppCompatActivity 
public abstract class BaseFragment extends RxFragment 
```

### 使用compose操作符绑定容器生命周期

因为activity和fragment控制切换线程的时候，引用没有清除干净，所以就把手脚着重于activity和fragment的生命周期了，就是说，如果你的活动如果碰到意外或者用户主动销毁，那么也断开订阅，就是订阅的生命周和活动的生命周期已经融为一体了，那就不用担心再发生内存泄漏了，所以这种绑定生命周期的方法是从根本上解决了内存泄漏。感觉没有更好的方法解决了。

 