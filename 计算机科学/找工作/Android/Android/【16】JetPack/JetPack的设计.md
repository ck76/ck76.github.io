[TOC]

![jg](https://ws1.sinaimg.cn/large/006tNbRwly1fyffc98qipj318g0mgafb.jpg)

### 一、前言

Jetpack组件是google推出的Android应用架构的标准组件，从性能和便捷性上为开发者解决架构问题，打造稳定体验流程的应用。本文的目的是从编码设计的的角度探讨ViewModel、Lifecycle、LiveData、Paging这几个库的设计原理，从中学习设计技巧。读者需要对这些库有些基础的了解，给出官方文档快速通道：https://developer.android.google.cn/jetpack/。 



### 二、ViewModel

ViewModel库从概念上来说，是结合DataBinding(数据绑定，将视图层与数据构成双向绑定)库，一起构成MVVM架构。在Android系统中，如果系统的关键配置发生变化，会引起正在展示的Activity重建，重建会带来页面数据状态的丢失，为解决这个问题，Activity的生命周期提供了本地存储和恢复bundle的方式，在重建行为发生时开发者自行存取状态数据。但是这个方式有些过于啰嗦，因为要手动存取数据到bundle，google为了将这个流程简单化，推出了ViewModel，特点是处于ViewModel中的数据在Activity重建时自动恢复，重建前后页面状态一样。

一般使用的方式是这样子的，通过ViewModelProviders获取使用而非自己独立创建，通过new创建是无法具有上述特性。

```java
public class MyFragment extends Fragment {
  public void onStart() {
      UserModel userModel = ViewModelProviders.of(getActivity()).get(UserModel.class);
  }
```



```java
public class UserModel extends ViewModel {
  public final LiveData&lt;User&gt; userLiveData = new LiveData<>();

  public UserModel() {
      // trigger user load.
 }

  void doAction() {
     // depending on the action, do necessary business logic calls and update the
     // userLiveData.
  }
```

从上面看最关键的奥秘就是从 ViewModelProviders 这个类中去获取，没错，这个类是一个Modle对象缓存类，提供了Model对象创建以及缓存的方式，并且挂在到系统的NonConfigurationInstances对象，传统的bundle方式最后数据也会存到这个对象中，并在Activity重建时自动获取。



```java
@Override
public final Object onRetainNonConfigurationInstance() {
Object custom = onRetainCustomNonConfigurationInstance();

FragmentManagerNonConfig fragments = mFragments.retainNestedNonConfig();

if (fragments == null && mViewModelStore == null && custom == null) {
    return null;
}

NonConfigurationInstances nci = new NonConfigurationInstances();
nci.custom = custom;
nci.viewModelStore = mViewModelStore;
nci.fragments = fragments;
return nci;
```

将状态存储到了ActivityClientRecord中，如果是因为配置变化引起的销毁getNonConfigInstance应当是true，那么这个类中的记录还是存在的。

```java
private ActivityClientRecord performDestroyActivity(IBinder token, boolean 
finishing,
        int configChanges, boolean getNonConfigInstance) {
    ActivityClientRecord r = mActivities.get(token);
                     ...省略
        if (getNonConfigInstance) {
           try {
 //保存状态到ActivityClientRecord，ActivityClientRecord是Activity的状态管理者
               r.lastNonConfigurationInstances
                        = r.activity.retainNonConfigurationInstances();
            } catch (Exception e) {
                if (!mInstrumentation.onException(r.activity, e)) {
                    throw new RuntimeException(
                            "Unable to retain activity "
                           + r.intent.getComponent().toShortString()
                            + ": " + e.toString(), e);
                }
            }
        }
        try {
           r.activity.mCalled = false;
            mInstrumentation.callActivityOnDestroy(r.activity);
            if (!r.activity.mCalled) {
                throw new SuperNotCalledException(
                    "Activity " + safeToComponentShortString(r.intent) +
                    " did not call through to super.onDestroy()");
            }
            if (r.window != null) {
                r.window.closeAllPanels();
            }
        } catch (SuperNotCalledException e) {
            throw e;
        } catch (Exception e) {
            if (!mInstrumentation.onException(r.activity, e)) {
                throw new RuntimeException(
                        "Unable to destroy activity " + safeToComponentShortString(r.intent)
                        + ": " + e.toString(), e);
            }
        }
    }
         ...省略
    return r;
}
```

重新构建activity这个过程，配置对象没有进行序列化，而是内存中的保持。

```java
private void handleRelaunchActivity(ActivityClientRecord tmp) {
                ...省略
    ActivityClientRecord r = mActivities.get(tmp.token);
    if (DEBUG_CONFIGURATION) Slog.v(TAG, "Handling relaunch of " + r);
    if (r == null) {
        if (!tmp.onlyLocalRequest) {
            try {
                ActivityManager.getService().activityRelaunched(tmp.token);
            } catch (RemoteException e) {
                throw e.rethrowFromSystemServer();
           }
        }
        return;
    }

    r.activity.mConfigChangeFlags |= configChanges;
    r.onlyLocalRequest = tmp.onlyLocalRequest;
    r.mPreserveWindow = tmp.mPreserveWindow;
    r.lastProcessedSeq = tmp.lastProcessedSeq;
    r.relaunchSeq = tmp.relaunchSeq;
    Intent currentIntent = r.activity.mIntent;
        //配置引起额重建
   r.activity.mChangingConfigurations = true;

                               … 省略

    handleDestroyActivity(r.token, false, configChanges, true);

         … 省略

    handleLaunchActivity(r, currentIntent, "handleRelaunchActivity");

        … 省略
}
```

其实就是相当于，当配置发生变化时，会将数据存储到ActivityClientRecord中，重建时在从中恢复过来。



### 三、Lifecycle     

 Lifecycle顾名思义，生命周期回调，它的任务是将Activity/Fragment等组件的生命周期回调以事件的方式广播出去，让关心此生命周期的观察者自行做些事情，我理解这个组件的本质是对生命周期方法的解耦，分离不同类型的处理。我们来看一个场景，在页面可见的时候进行用户行为统计的数据埋点，传统的方式是在onCreate或者onStart方法中调用埋点API，随着类似的非UI的操作在Activity/Fragment中越积累越多，整个类显得非常的糅杂，像个菜市场。使用Lifecycle这个组件，我们可以将非UI操作的逻辑抽取到生命周期的观察者类中，订阅特定的生命周期方法，执行相关逻辑，完美的分离了代码。

整体设计上，与上述的思路差不多，以下是我归纳的设计图：

![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/ekwWEV12HUjM7pV0*QFwVVP8cGLLXeKfDkGsrdX7C7c!/r/dLkAAAAAAAAA)



每次生命周期方法发生时，Lifecycle会收到通知，全局维护一个生命周期状态，然后同步所有的观察者到同样的状态。值得注意的是，由于生命周期方法发生的不确定性，这里的状态改变和通知不一定是串行的，可能上一个状态还处于遍历通知中，下一个状态已经更新，因此每次遍历通知完成还要做一次状态是否同步检测，若状态不一致，还需要再次遍历通知。另外Lifecycle库对于如何监听生命周期事件提供了两种方式，一种是使用注解的方式，一种是接口类的方式，注解在的方式在运行时会反射监听方法的信息，然后进行缓存，当关心的事件发生时，会用反射方法进行调用，显然接口类的方式性能更好。

- 接口的方式

```java
class TestObserver implements DefaultLifecycleObserver {
  Override
  public void onCreate(LifecycleOwner owner) {
     // your code
 }
}
```

- 注解的方式

```java
class TestObserver implements LifecycleObserver {
OnLifecycleEvent(ON_STOP)
void onStopped() {}
}
```

- 使用注解的方式可以定制接受参数，但是有限制，类型确定，最多两个

```java
class TestObserver implements LifecycleObserver {
    OnLifecycleEvent(ON_CREATE)
    void onCreated(LifecycleOwner source) {}
    OnLifecycleEvent(ON_ANY)
    void onAny(LifecycleOwner source, Event event) {}
} 
```

Activity/Fragment解耦分离的的利器，使用后代码清洁度提高。



### 四、LiveData

之所以将Lifecycl放在前面讲述，是因为LiveData这个组件与前者有着完美的联合，或者说这个组件是以Lifecycel这个库为基础进行设计的。大多数开发者都遇到过一个典型的问题，Activity的生命周期与一些延时任务不同步导致内存泄漏或者空指针异常，为此常常要付出取消延时或移除任务，判空等额外操作的代价，如果忘记处理了，往往有性能或者崩溃的风险。LiveData这个组件通过Lifecycle的生命周期事件通知，主动注册和注销自己观察者。在页面可见时，将最后更新的数据通知给自己的观察者，在页面销毁时主动注销切断与观察者的联系，即使有数据回来也不会通知观察执行监听方法，完美解决了上面提的问题。

LiveData是一个能保存数据并且能被生命周期组件通知的类，Lifecycle的所有生命周期回调通知LiveData都能收到如果LiveData认为当前的处于活跃态。那到底如何定义活跃态，当Lifecycle的状态处于STARTED或者RESUMED状态时，LiveData认为观察者处于活跃态，Lifecycle处于非活跃态时，即时收到数据也不会分发给观察者。使用observeForever方法注册监听，LiveData将一直都能收到监听无关乎Lifecycle的状态。

以下是我归纳的LiveData设计图：

![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/b.Imw53wTQTDHJYUZF437pUBw*Xze0tZqWpALE0lGMM!/r/dFQBAAAAAAAA)

LiveData的设计与上面Lifecycle的设计上最大的不同是被观察者角色和观察者角色都有了活跃态的定义。传统的设计中被观察者只要发生了变化，观察者立即就被通知接收到这种变化，而LiveData需要判断当前是否处于活跃态，如果不处于活跃态，就不分发数据给观察者。LiveData活跃态由有多少个观察者是处于活跃态决定的,至少有一个观察者是活跃的，LiveData才处于活跃态。目前框架中只有两种观察者，永久活跃观察者和活跃态跟随Lifecycle变化的观察者。

以下的代码中是活跃态跟随Lifecycle变化的观察者，可知该观察者监听了生命周期回调事件。在shouldBeActive方法中，当Lifecycle的状态处于STARTED或者RESUMED状态时，观察者才是活跃的。

```java
class LifecycleBoundObserver extends ObserverWrapper implements 
                                              GenericLifecycleObserver {
    @NonNull
    final LifecycleOwner mOwner;

    LifecycleBoundObserver(@NonNull LifecycleOwner owner, Observer<? super T> observer) {
        super(observer);
        mOwner = owner;
    }

    @Override
    boolean shouldBeActive() {
        return mOwner.getLifecycle().getCurrentState().isAtLeast(STARTED);
    }

    @Override
    public void onStateChanged(LifecycleOwner source, Lifecycle.Event event) {
        if (mOwner.getLifecycle().getCurrentState() == DESTROYED) {
            removeObserver(mObserver);
            return;
        }
        activeStateChanged(shouldBeActive());
    }

    @Override
    boolean isAttachedTo(LifecycleOwner owner) {
        return mOwner == owner;
    }

    @Override
    void detachObserver() {
        mOwner.getLifecycle().removeObserver(this);
    }
}
```

另外，LiveData定义了这样一种场景的回调：当活跃态观察者数量从0变为1时，通知LiveData活跃了，onActive();当活跃态观察者数量级从1变为0，通知LiveData不活跃了，onInActive()。通过这两个方式能做资源注入与回收的操作。

LiveData库中有一个便于外界库接入的类ComputableLiveData,通过发起一个计算命令，异步计算，然后将结果更新给LiveData，LiveData的观察者就能收到计算的结果，Room,Retroift等支持LiveData结果类型的库都是基于这个类进行拓展的。

![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/GqE8jEDrvesHzEVkj9tu.IPUCu.xQ7vyIB8sUQP2ofo!/r/dLgAAAAAAAAA)

另外还有一些类似于rxjava的map函数操作映射的方法，将一个LiveData转换为另外一个LiveData,进行结果转换，就不再赘述。



### 五、Paging

Paging组件用来实现从数据源中分页加载静态数据集，数据源可以是数据库，文件，网络，若要兼容动态数据集，则需要配合AsyncListDiffer集合比对工具来使用实现动态更新数据源。

相比较传统的优势在于实现分页加载的便捷性，只需设定几个参数就可以实现分页加载，并且不需要loading，因为会进行预加载下面几页数据，列表视图可以无限滑动没有阻塞。

以下是正流程图：

![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/UFcaifvxg65UIWArqmGnTx9VY8Z67D3n5iEuxhflNTY!/r/dDcBAAAAAAAA)

有意思的是PageList给出了一个placeholders占位设定，用来占位没有加载的数据。placeholders可用时，将使用null值来代替没有加载的数据项，PageList的数据集大小与数据源中数据的总数是一样的。据官网说有这样几个好处，一是scrollbar不会发生跳跃，因为数据集大小都已经固定了，二是加载更多数据时不用显示loading，因为会预加载下也数据。但是劣势也是明显的，首先整个数据集的大小都会计算出来，并且PageList的大小也要扩容到相同的数量，即使很多元素都是null值，如果不加载大量数据，就会浪费内存和性能，；其次如果预加载参数不合理会导致没有加载的数据项显示出来，虽然为null值时可以默认处理，等待加载回来在刷新，但是默认视图的大小又不太能轻易确定。我觉得这个功能比较鸡肋，适用场景很局限。

在上面的例子中有个问题就是何时驱动预加载功能，见下图

![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Tur74P5zNZVEcUi4IRPwgrFyDs1l4gwFNJ8YI.zK760!/r/dFQBAAAAAAAA)



向下滑动时在Adapter的getItem方法里会驱动加载更多数据。PageList是
Adapter的真是数据源，其中的数据时如何存放的呢？

![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/FbnEsd4rANvb5*Ol*Rg3Zp2rYfelu0oHTNkr7h5a.to!/r/dLYAAAAAAAAA)



整个数据集分成多个页面，页面存储到该集合里，未加载的数据使用一个空集合empty代替，页面集合里只有中间部分是展示的数据，两头的数据待加载，通过一个游标指针将折叠起来的页面数据与视图展示的平铺数据联系起来。

总的流程来说，视图加载数据，驱动数据加载器加载数据，数据源加载数据通过回执将加载的数据返回给PageList,总体来说也符合观察者的设计方式。



### 七、总结

观察者设计模式已经被Android系统奉为真理和哲学的一种设计模式，从其基本朴素类型到复杂的衍生类型，无不显示出这种模式的高度层次感设计能力和灵活性，就像是地理的高山海拔图，一圈一圈的从高处到地处的层次降维。