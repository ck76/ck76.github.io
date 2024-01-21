[TOC]

Android的应用程序开发采用JAVA语言，Activity本质上也是一个对象，Android程序不像Java程序一样，随便创建一个类，写个main()方法就能运行，Android应用模型是基于组件的应用设计模式，组件的运行要有一个完整的Android工程环境，在这个环境下，Activity、Service等系统组件才能够正常工作，而这些组件并不能采用普通的Java对象创建方式，new一下就能创建实例了，而是要有它们各自的上下文环境，也就是我们这里讨论的Context。可以这样讲，Context是维持Android程序中各组件能够正常工作的一个核心功能类。

## Context到底是什么？

Context的中文翻译为：语境; 上下文; 背景; 环境，在开发中我们经常说称之为“上下文”。Context在加载资源、启动Activity、获取系统服务、创建View等操作都要参与。 

那Context到底是什么呢？一个Activity就是一个Context，一个Service也是一个Context。Android程序员把“场景”抽象为Context类，他们认为用户和操作系统的每一次交互都是一个场景，比如打电话、发短信，这些都是一个有界面的场景，还有一些没有界面的场景，比如后台运行的服务（Service）。

 它描述一个应用程序环境的信息（即上下文）；是一个抽象类，Android提供了该抽象类的具体实现类；通过它我们可以获取应用程序的资源和类（包括应用级别操作，如启动Activity，发广播，接受Intent等）。 

![Context结构](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/rB9ul8iger4I2.TTTEsp.ifX8UomaOoYncRsaVeXaLE!/r/dFMBAAAAAAAA)

Context类本身是一个纯abstract类，它有两个具体的实现子类：ContextImpl和ContextWrapper。其中ContextWrapper类，如其名所言，这只是一个包装而已，ContextWrapper构造函数中必须包含一个真正的Context引用，同时ContextWrapper中提供了attachBaseContext（）用于给ContextWrapper对象中指定真正的Context对象，调用ContextWrapper的方法都会被转向其所包含的真正的Context对象。ContextThemeWrapper类，如其名所言，其内部包含了与主题（Theme）相关的接口，这里所说的主题就是指在AndroidManifest.xml中通过android：theme为Application元素或者Activity元素指定的主题。当然，只有Activity才需要主题，Service是不需要主题的，因为Service是没有界面的后台场景，所以Service直接继承于ContextWrapper，Application同理。而ContextImpl类则真正实现了Context中的所有函数，应用程序中所调用的各种Context类的方法，其实现均来自于该类。一句话总结：Context的两个子类分工明确，其中ContextImpl是Context的具体实现类，ContextWrapper是Context的包装类。Activity，Application，Service虽都继承自ContextWrapper（Activity继承自ContextWrapper的子类ContextThemeWrapper），但它们初始化的过程中都会创建ContextImpl对象，由ContextImpl实现Context中的方法。



### 一个应用程序有几个Context?

`Context数量=Activity数量+Service数量+Application数(1)`

一个应用程序中可以有多个Activity和多个Service，但是只能有一个Application。 

我们常说四大组件，这里怎么只有Activity，Service持有Context，那Broadcast Receiver，Content Provider呢？Broadcast Receiver，Content Provider并不是Context的子类，他们所持有的Context都是其他地方传过去的，所以并不计入Context总数。



## Context能干什么? 

Context到底可以实现哪些功能呢？这个就实在是太多了，弹出Toast、启动Activity、启动Service、发送广播、操作数据库等等都需要用到Context。 

```java
TextView tv = new TextView(getContext());

ListAdapter adapter = new SimpleCursorAdapter(getApplicationContext(), ...);

AudioManager am = (AudioManager) getContext().getSystemService(Context.AUDIO_SERVICE);getApplicationContext().getSharedPreferences(name, mode);

getApplicationContext().getContentResolver().query(uri, ...);

getContext().getResources().getDisplayMetrics().widthPixels * 5 / 8;

getContext().startActivity(intent);

getContext().startService(intent);

getContext().sendBroadcast(intent);
```



### Context作用域 

虽然Context神通广大，但并不是随便拿到一个Context实例就可以为所欲为，它的使用还是有一些规则限制的。由于Context的具体实例是由ContextImpl类去实现的，因此在绝大多数场景下，Activity、Service和Application这三种类型的Context都是可以通用的。不过有几种场景比较特殊，比如启动Activity，还有弹出Dialog。出于安全原因的考虑，Android是不允许Activity或Dialog凭空出现的，一个Activity的启动必须要建立在另一个Activity的基础之上，也就是以此形成的返回栈。而Dialog则必须在一个Activity上面弹出（除非是System Alert类型的Dialog），因此在这种场景下，我们只能使用Activity类型的Context，否则将会出错。

![Context作用域](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/bDM1YfOoIp.7T0vAyV.lknvojSIBfO6*5V7EYazEQcg!/r/dDYBAAAAAAAA) 

从上图我们可以发现Activity所持有的Context的作用域最广，无所不能。因为Activity继承自ContextThemeWrapper，而Application和Service继承自ContextWrapper，很显然ContextThemeWrapper在ContextWrapper的基础上又做了一些操作使得Activity变得更强大，这里我就不再贴源码给大家分析了，有兴趣的童鞋可以自己查查源码。上图中的YES和NO我也不再做过多的解释了，这里我说一下上图中Application和Service所不推荐的两种使用情况。
 1：如果我们用ApplicationContext去启动一个LaunchMode为standard的Activity的时候会报错`android.util.AndroidRuntimeException: Calling startActivity from outside of an Activity context requires the FLAG_ACTIVITY_NEW_TASK flag. Is this really what you want?`这是因为非Activity类型的Context并没有所谓的任务栈，所以待启动的Activity就找不到栈了。解决这个问题的方法就是为待启动的Activity指定FLAG_ACTIVITY_NEW_TASK标记位，这样启动的时候就为它创建一个新的任务栈，而此时Activity是以singleTask模式启动的。所有这种用Application启动Activity的方式不推荐使用，Service同Application。
 2：在Application和Service中去layout inflate也是合法的，但是会使用系统默认的主题样式，如果你自定义了某些样式可能不会被使用。所以这种方式也不推荐使用。
 一句话总结：凡是跟UI相关的，都应该使用Activity做为Context来处理；其他的一些操作，Service,Activity,Application等实例都可以，当然了，注意Context引用的持有，防止内存泄漏。



### 如何获取Context

通常我们想要获取Context对象，主要有以下四种方法
 1：View.getContext,返回当前View对象的Context对象，通常是当前正在展示的Activity对象。
 2：Activity.getApplicationContext,获取当前Activity所在的(应用)进程的Context对象，通常我们使用Context对象时，要优先考虑这个全局的进程Context。
 3：ContextWrapper.getBaseContext():用来获取一个ContextWrapper进行装饰之前的Context，可以使用这个方法，这个方法在实际开发中使用并不多，也不建议使用。
 4：Activity.this 返回当前的Activity实例，如果是UI控件需要使用Activity作为Context对象，但是默认的Toast实际上使用ApplicationContext也可以。

**getApplication()和getApplicationContext()**

上面说到获取当前Application对象用getApplicationContext，不知道你有没有联想到getApplication()，这两个方法有什么区别？相信这个问题会难倒不少开发者。

![getApplication()和getApplicationContext()](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/hP7iC2CkyhyWRy12OS*moxNGTJQ6*4rxvszo3bNN7kI!/r/dDUBAAAAAAAA)

程序是不会骗人的，我们通过上面的代码，打印得出两者的内存地址都是相同的，看来它们是同一个对象。其实这个结果也很好理解，因为前面已经说过了，Application本身就是一个Context，所以这里获取getApplicationContext()得到的结果就是Application本身的实例。那么问题来了，既然这两个方法得到的结果都是相同的，那么Android为什么要提供两个功能重复的方法呢？实际上这两个方法在作用域上有比较大的区别。getApplication()方法的语义性非常强，一看就知道是用来获取Application实例的，但是这个方法只有在Activity和Service中才能调用的到。那么也许在绝大多数情况下我们都是在Activity或者Service中使用Application的，但是如果在一些其它的场景，比如BroadcastReceiver中也想获得Application的实例，这时就可以借助getApplicationContext()方法了。

```java
publicclassMyReceiverextendsBroadcastReceiver{

@Override
publicvoidonReceive(Contextcontext,Intentintent){
ApplicationmyApp=(Application)context.getApplicationContext();

}

}
```

**Context引起的内存泄露**

但Context并不能随便乱用，用的不好有可能会引起内存泄露的问题，下面就示例两种错误的引用方式。

**错误的单例模式**

```java
public class Singleton {
    private static Singleton instance;
    private Context mContext;

    private Singleton(Context context) {
        this.mContext = context;
    }

    public static Singleton getInstance(Context context) {
        if (instance == null) {
            instance = new Singleton(context);
        }
        return instance;
    }
}
```

这是一个非线程安全的单例模式，instance作为静态对象，其生命周期要长于普通的对象，其中也包含Activity，假如Activity A去getInstance获得instance对象，传入this，常驻内存的Singleton保存了你传入的Activity A对象，并一直持有，即使Activity被销毁掉，但因为它的引用还存在于一个Singleton中，就不可能被GC掉，这样就导致了内存泄漏。

有一个静态的Drawable对象当ImageView设置这个Drawable时，ImageView保存了mDrawable的引用，而ImageView传入的this是MainActivity的mContext，因为被static修饰的mDrawable是常驻内存的，MainActivity是它的间接引用，MainActivity被销毁时，也不能被GC掉，所以造成内存泄漏。



## 总结

好了，到此，Context的分析基本完成了，希望大家在以后的使用过程中，能够稍微考虑下，这里使用Activity合适吗？会不会造成内存泄漏？这里传入Application work吗？

1.getContext

这是View的一个方法，获取视图上下文，view一般是依托于Activity，所以这个方法返回的是当前Activity实例，一般在Activity中可以使用YourActivityName.this代替。

2.getApplicationContext

这个是获取整个app生命周期的上下文，一般用于application中，获取app相关的基础信息

3.getBaseContext

是ContextWrapper中的方法，基本不用，Google也不推荐使用，是一种委托，在一个context获取另一个context。

4.getApplication

这个是获取当前进程的Application实例，可以去操作自己写的Application中的方法。



## Activity 与 Context 区别与联系

这是两种不同的context，也是最常见的两种.第一种中context的生命周期与Application的生命周期相关的，context随着Application的销毁而销毁，伴随application的一生，与activity的生命周期无关.第二种中的context跟Activity的生命周期是相关的，但是对一个Application来说，Activity可以销毁几次，那么属于Activity的context就会销毁多次.至于用哪种context，得看应用场景，个人感觉用Activity的context好一点，不过也有的时候必须使用Application的context.application context可以通过

Context.getApplicationContext或者Activity.getApplication方法获取.

还有就是，在使用context的时候，小心内存泄露，防止内存泄露，注意一下几个方面：

　1. 不要让生命周期长的对象引用activity context，即保证引用activity的对象要与activity本身生命周期是一样的

　2. 对于生命周期长的对象，可以使用application context

　3. 避免非静态的内部类，尽量使用静态类，避免生命周期问题，注意内部类对外部对象引用导致的生命周期变化

现在回到正题，说一下android全局变量，在平时的开发过程中，有时候可能会需要一些全局数据，来让应用中的所有Activity和View都能访问到，大家在遇到这种情况时，可能首先会想到自己定义一个类，然后创建很多静态成员，android已经为我们提供了这种情况的解决方案：

在Android中，有一个Application类，在Activity中可以使用getApplication（）方法获得实例，使用它就可以获得当前应用的主题、资源文件中的内容等，这个类更灵活的一个特性就是可以被继承，来添加自己的全局属性.例如开发一个游戏，需要保存分数，那么我们就可以继承Application

Activity中的上下文Context是随着活动的产生而产生，随其消亡而消亡，但是整个应用程序的上下文Context这是伴随着整个应用程序而存在的，无论活动的存活与否都影响不到这个上下文。 

1：View.getContext,返回当前View对象的Context对象，通常是当前正在展示的Activity对象。

2：Activity.getApplicationContext,获取当前Activity所在的(应用)进程的Context对象，通常我们使用Context对象时，要优先考虑这个全局的进程Context。

3：ContextWrapper.getBaseContext():用来获取一个ContextWrapper进行装饰之前的Context，可以使用这个方法，这个方法在实际开发中使用并不多，也不建议使用。

4：Activity.this 返回当前的Activity实例，如果是UI控件需要使用Activity作为Context对象，但是默认的Toast实际上使用ApplicationContext也可以。

getApplication()和getApplicationContext()

## Context与ApplicationContext

看了标题，千万不要被误解，ApplicationContext并没有这个类，其实更应该叫做：Activity与Application在作为Context时的区别。嗯，的确是这样的，大家在需要Context的时候，如果是在Activity中，大多直接传个this，当在匿名内部类的时候，因为this不能用，需要写XXXActivity.this，很多哥们会偷懒，直接就来个getApplicationContext。那么大家有没有想过，XXXActivity.this和getApplicationContext的区别呢？

XXXActivity和getApplicationContext返回的肯定不是一个对象，一个是当前Activity的实例，一个是项目的Application的实例。既然区别这么明显，那么各自的使用场景肯定不同，乱使用可能会带来一些问题。

下面开始介绍在使用Context时，需要注意的问题。

## 引用的保持

大家在编写一些类时，例如工具类，可能会编写成单例的方式，这些工具类大多需要去访问资源，也就说需要Context的参与。

在这样的情况下，就需要注意Context的引用问题。

例如以下的写法：

```java
package com.mooc.shader.roundimageview;  
  
import android.content.Context;  
  
public class CustomManager  
{  
    private static CustomManager sInstance;  
    private Context mContext;  
  
    private CustomManager(Context context)  
    {  
        this.mContext = context;  
    }  
  
    public static synchronized CustomManager getInstance(Context context)  
    {  
        if (sInstance == null)  
        {  
            sInstance = new CustomManager(context);  
        }  
        return sInstance;  
    }  
      
    //some methods   
    private void someOtherMethodNeedContext()  
    {  
          
    }  
}  
```

对于上述的单例，大家应该都不陌生（请别计较getInstance的效率问题），内部保持了一个Context的引用；

这么写是没有问题的，问题在于，这个Context哪来的我们不能确定，很大的可能性，你在某个Activity里面为了方便，直接传了个this;这样问题就来了，我们的这个类中的sInstance是一个static且强引用的，在其内部引用了一个Activity作为Context，也就是说，我们的这个Activity只要我们的项目活着，就没有办法进行内存回收。而我们的Activity的生命周期肯定没这么长，所以造成了内存泄漏。

那么，我们如何才能避免这样的问题呢？

有人会说，我们可以软引用，嗯，软引用，假如被回收了，你不怕NullPointException么。

把上述代码做下修改：

```java
public static synchronized CustomManager getInstance(Context context)  
    {  
        if (sInstance == null)  
        {  
            sInstance = new CustomManager(context.getApplicationContext());  
        }  
        return sInstance;  
    }  
```

这样，我们就解决了内存泄漏的问题，因为我们引用的是一个ApplicationContext，它的生命周期和我们的单例对象一致。

这样的话，可能有人会说，早说嘛，那我们以后都这么用不就行了，很遗憾的说，不行。上面我们已经说过，Context和Application Context的区别是很大的，也就是说，他们的应用场景（你也可以认为是能力）是不同的，并非所有Activity为Context的场景，Application Context都能搞定。

