[TOC]

### 一、基本使用

```java
public class Fragment1 extends Fragment{  
  private static String ARG_PARAM = "param_key"; 
     private String mParam; 
     private Activity mActivity; 
     public void onAttach(Context context) {
        mActivity = (Activity) context;
        mParam = getArguments().getString(ARG_PARAM);  //获取参数
    }
     public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View root = inflater.inflate(R.layout.fragment_1, container, false);
        TextView view = root.findViewById(R.id.text);
        view.setText(mParam);
             return root;
    }    
     public static Fragment1 newInstance(String str) {
        Fragment1 frag = new Fragment1();
        Bundle bundle = new Bundle();
        bundle.putString(ARG_PARAM, str);
        fragment.setArguments(bundle);   //设置参数
        return fragment;
    }
}
```

如果在创建Fragment时要传入参数，必须要通过setArguments(Bundle bundle)方式添加，而不建议通过为Fragment添加带参数的构造函数，因为通过setArguments()方式添加，在由于内存紧张导致Fragment被系统杀掉并恢复（re-instantiate）时能保留这些数据。官方建议如下：

```
It is strongly recommended that subclasses do not have other constructors with parameters, since these constructors will not be called when the fragment is re-instantiated.
```

我们可以在Fragment的`onAttach()`中通过`getArguments()`获得传进来的参数，并在之后使用这些参数。如果要获取Activity对象，不建议调用`getActivity()`，而是在`onAttach()`中将Context对象强转为Activity对象。



### 二、动态添加

创建完Fragment后，接下来就是把Fragment添加到Activity中。在Activity中添加Fragment的方式有两种：

- 静态添加：通过xml的方式添加，缺点是一旦添加就不能在运行时删除。
- 动态添加：运行时添加，这种方式比较灵活，因此建议使用这种方式。
  虽然Fragment能在XML中添加，但是这只是一个语法糖而已，Fragment并不是一个View，而是和Activity同一层次的。

这里只给出动态添加的方式。首先Activity需要有一个容器存放Fragment，一般是FrameLayout，因此在Activity的布局文件中加入FrameLayout：

```xml
<FrameLayout
    android:id="@+id/container"
    android:layout_width="match_parent"
    android:layout_height="match_parent"/>
```

然后在`onCreate()`中，通过以下代码将Fragment添加进Activity中。

```java
if (bundle == null) {
    getSupportFragmentManager().beginTransaction()
        .add(R.id.container, Fragment1.newInstance("hello world"), "f1")        //.addToBackStack("fname")
        .commit();
}
```

这里需要注意几点：

- 因为我们使用了support库的Fragment，因此需要使用`getSupportFragmentManager()`获取FragmentManager。
- `add()`是对Fragment众多操作中的一种，还有`remove()`, `replace()`等，第一个参数是根容器的id（FrameLayout的id，即”@id/container”），第二个参数是Fragment对象，第三个参数是fragment的tag名，指定tag的好处是后续我们可以通过`Fragment1 frag = getSupportFragmentManager().findFragmentByTag("f1")`从FragmentManager中查找Fragment对象。
- 在一次事务中，可以做多个操作，比如同时做`add().remove().replace()`。
- `commit()`操作是异步的，内部通过`mManager.enqueueAction()`加入处理队列。对应的同步方法为`commitNow()`，`commit()`内部会有`checkStateLoss()`操作，如果开发人员使用不当（比如`commit()`操作在`onSaveInstanceState()`之后），可能会抛出异常，而`commitAllowingStateLoss()`方法则是不会抛出异常版本的`commit()`方法，但是尽量使用`commit()`，而不要使用`commitAllowingStateLoss()`。
- `addToBackStack("fname")`是可选的。FragmentManager拥有回退栈（BackStack），类似于Activity的任务栈，如果添加了该语句，就把该事务加入回退栈，当用户点击返回按钮，会回退该事务（回退指的是如果事务是`add(frag1)`，那么回退操作就是`remove(frag1)`）；如果没添加该语句，用户点击返回按钮会直接销毁Activity。
- Fragment有一个常见的问题，即Fragment重叠问题，这是由于Fragment被系统杀掉，并重新初始化时再次将fragment加入activity，因此通过在外围加if语句能判断此时是否是被系统杀掉并重新初始化的情况。

#### 1、Fragment有个常见的异常：

```java
java.lang.IllegalStateException: Can not perform this action after onSaveInstanceState
    at android.support.v4.app.FragmentManagerImpl.checkStateLoss(FragmentManager.java:1341)
    at android.support.v4.app.FragmentManagerImpl.enqueueAction(FragmentManager.java:1352)
    at android.support.v4.app.BackStackRecord.commitInternal(BackStackRecord.java:595)
    at android.support.v4.app.BackStackRecord.commit(BackStackRecord.java:574)
```

该异常出现的原因是：`commit()`在`onSaveInstanceState()`后调用。首先，`onSaveInstanceState()`在`onPause()`之后，`onStop()`之前调用。`onRestoreInstanceState()`在`onStart()`之后，`onResume()`之前。

#### 2、因此避免出现该异常的方案有：

- 不要把Fragment事务放在异步线程的回调中，比如不要把Fragment事务放在AsyncTask的`onPostExecute()`，因此`onPostExecute()`可能会在`onSaveInstanceState()`之后执行。
- 逼不得已时使用`commitAllowingStateLoss()`。



### 三、生命周期

先上几张图

![1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/qts7X3kmCRh.QFAeoSh4xxOFTmyX7WAjWfIbebM8m0o!/r/dDMBAAAAAAAA)

---

![2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/g7BIzUABu.0JdgG8s3EIHWresHCTbhsTyXR.nK.V6no!/r/dL4AAAAAAAAA)

---


![3](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/RiaslKsTYXR6iiWDAGnmXjxesyU012f.Dp5IBD.O2h8!/r/dLkAAAAAAAAA)

---

> **onAttach**：onAttach()在fragment与Activity关联之后调调查用。需要注意的是，初始化fragment参数可以从getArguments()获得，但是，当Fragment附加到Activity之后，就无法再调用setArguments()。所以除了在最开始时，其它时间都无法向初始化参数添加内容。

> **onCreate**：fragment初次创建时调用。尽管它看起来像是Activity的OnCreate()函数，但这个只是用来创建Fragment的。此时的Activity还没有创建完成，因为我们的Fragment也是Activity创建的一部分。所以如果你想在这里使用Activity中的一些资源，将会获取不到。比如：获取同一个Activity中其它Frament的控件实例。(代码如下：)，如果想要获得Activity相关联的资源，必须在onActivityCreated中获取。

> **onCreateView**：在这个fragment构造它的用户接口视图(即布局)时调用。

> **onActivityCreated**：在Activity的OnCreate()结束后，会调用此方法。所以到这里的时候，Activity已经创建完成！在这个函数中才可以使用Activity的所有资源。如果把下面的代码放在这里，获取到的btn_Try的值将不会再是空的！

> **onStart**：当到OnStart()时，Fragment对用户就是可见的了。但用户还未开始与Fragment交互。在生命周期中也可以看到Fragment的OnStart()过程与Activity的OnStart()过程是绑定的。意义即是一样的。以前你写在Activity的OnStart()中来处理的代码，用Fragment来实现时，依然可以放在OnStart()中来处理。

> **onResume**：当这个fragment对用户可见并且正在运行时调用。这是Fragment与用户交互之前的最后一个回调。从生命周期对比中，可以看到，Fragment的OnResume与Activity的OnResume是相互绑定的，意义是一样的。它依赖于包含它的activity的Activity.onResume。当OnResume()结束后，就可以正式与用户交互了。

> **onPause**：此回调与Activity的OnPause()相绑定，与Activity的OnPause()意义一样。

> **onStop**：这个回调与Activity的OnStop()相绑定，意义一样。已停止的Fragment可以直接返回到OnStart()回调，然后调用OnResume()。

> **onDestroyView**：如果Fragment即将被结束或保存，那么撤销方向上的下一个回调将是onDestoryView()。会将在onCreateView创建的视图与这个fragment分离。下次这个fragment若要显示，那么将会创建新视图。这会在onStop之后和onDestroy之前调用。这个方法的调用同onCreateView是否返回非null视图无关。它会潜在的在这个视图状态被保存之后以及它被它的父视图回收之前调用。

> **onDestroy**：当这个fragment不再使用时调用。需要注意的是，它即使经过了onDestroy()阶段，但仍然能从Activity中找到，因为它还没有Detach。

> **onDetach**：Fragment生命周期中最后一个回调是onDetach()。调用它以后，Fragment就不再与Activity相绑定，它也不再拥有视图层次结构，它的所有资源都将被释放。



上面的方法中，只有onCreateView()在重写时不用写super方法，其他都需要。

我们这里举个例子来理解Fragment生命周期方法。功能如下：共有两个Fragment：F1和F2，F1在初始化时就加入Activity，点击F1中的按钮调用replace替换为F2。

当F1在Activity的`onCreate()`中被添加时，日志如下：

```tiki wiki
BasicActivity: [onCreate] BEGIN
BasicActivity: [onCreate] END
BasicActivity: [onStart] BEGIN
Fragment1: [onAttach] BEGIN 
Fragment1: [onAttach] END
BasicActivity: [onAttachFragment] BEGIN
BasicActivity: [onAttachFragment] END
Fragment1: [onCreate] BEGIN
Fragment1: [onCreate] END
Fragment1: [onCreateView]
Fragment1: [onViewCreated] BEGIN
Fragment1: [onViewCreated] END
Fragment1: [onActivityCreated] BEGIN
Fragment1: [onActivityCreated] END
Fragment1: [onStart] BEGIN
Fragment1: [onStart] END
BasicActivity: [onStart] END
BasicActivity: [onPostCreate] BEGIN
BasicActivity: [onPostCreate] END
BasicActivity: [onResume] BEGIN
BasicActivity: [onResume] END
BasicActivity: [onPostResume] BEGIN
Fragment1: [onResume] BEGIN
Fragment1: [onResume] END
BasicActivity: [onPostResume] END
BasicActivity: [onAttachedToWindow] BEGIN
BasicActivity: [onAttachedToWindow] END
```

可以看出：

- Fragment的onAttach()->onCreate()->onCreateView()->onActivityCreated()->onStart()都是在Activity的onStart()中调用的。
- Fragment的onResume()在Activity的onResume()之后调用。

接下去分两种情况，分别是不加addToBackStack()和加addToBackStack()。

**去看Bugly的微信文章**

FragmentTransaction有一些基本方法，下面给出调用这些方法时，Fragment生命周期的变化：

- add(): onAttach()->…->onResume()。
- remove(): onPause()->…->onDetach()。
- replace(): 相当于旧Fragment调用remove()，新Fragment调用add()。
- show(): 不调用任何生命周期方法，调用该方法的前提是要显示的Fragment已经被添加到容器，只是纯粹把Fragment UI的setVisibility为true。
- hide(): 不调用任何生命周期方法，调用该方法的前提是要显示的Fragment已经被添加到容器，只是纯粹把Fragment UI的setVisibility为false。
- detach(): onPause()->onStop()->onDestroyView()。UI从布局中移除，**但是仍然被FragmentManager管理。**
- attach(): onCreateView()->onStart()->onResume()。
- **注意事物的detach和fragment的onDetach的区别啊啊啊**



### 四、Fragment实现原理和Back Stack

我们知道Activity有任务栈，用户通过startActivity将Activity加入栈，点击返回按钮将Activity出栈。Fragment也有类似的栈，称为回退栈（Back Stack），回退栈是由FragmentManager管理的。默认情况下，Fragment事务是不会加入回退栈的，如果想将Fragment事务加入回退栈，则可以加入`addToBackStack("")`。如果没有加入回退栈，则用户点击返回按钮会直接将Activity出栈；如果加入了回退栈，则用户点击返回按钮会回滚Fragment事务。

我们将通过最常见的Fragment用法，讲解Back Stack的实现原理：

```java
getSupportFragmentManager().beginTransaction()
    .add(R.id.container, f1, "f1")
    .addToBackStack("")
    .commit();
```

上面这个代码的功能就是将Fragment加入Activity中，内部实现为：创建一个BackStackRecord对象，该对象记录了这个事务的全部操作轨迹（这里只做了一次add操作，并且加入回退栈），随后将该对象提交到FragmentManager的执行队列中，等待执行。

BackStackRecord类的定义如下

```java
class BackStackRecord extends FragmentTransaction implements FragmentManager.BackStackEntry, Runnable {}
```

从定义可以看出，BackStackRecord有三重含义：

- 继承了FragmentTransaction，即是事务，保存了整个事务的全部操作轨迹。
- 实现了BackStackEntry，作为回退栈的元素，正是因为该类拥有事务全部的操作轨迹，因此在popBackStack()时能回退整个事务。
- 继承了Runnable，即被放入FragmentManager执行队列，等待被执行。

先看第一层含义，`getSupportFragmentManager.beginTransaction()`返回的就是BackStackRecord对象，代码如下：

```java
public FragmentTransaction beginTransaction() {
    return new BackStackRecord(this);
}
```

#### 1、例子

通过讲解Demo1来更清晰地了解回退栈的使用。功能如下：共有三个Fragment：F1, F2, F3，F1在初始化时就加入Activity，点击F1中的按钮跳转到F2，点击F2的按钮跳转到F3，点击F3的按钮回退到F1。

在Activity的onCreate()中，将F1加入Activity中：

```java
getSupportFragmentManager().beginTransaction()
    .add(R.id.container, f1, "f1")
    .addToBackStack(Fragment1.class.getSimpleName())
    .commit();
```

F1按钮的onClick()内容如下：

```java
getFragmentManager().beginTransaction()
    .replace(R.id.container, f2, "f2")
    .addToBackStack(Fragment2.class.getSimpleName())
    .commit();
```

F2按钮的onClick()如下：

```java
getFragmentManager().beginTransaction()
    .replace(R.id.container, f3, "f3")
    .addToBackStack(Fragment3.class.getSimpleName())
    .commit();
```

F3按钮的onClick()如下：

```java
getFragmentManager().popBackStack(Fragment2.class.getSimpleName(), FragmentManager.POP_BACK_STACK_INCLUSIVE);
```

这样就完成了整个界面的跳转逻辑。

这里补充一个点
`getSupportFragmentManager().findFragmentByTag()`是经常用到的方法，他是FragmentManager的方法，FragmentManager是抽象类，FragmentManagerImpl是继承FragmentManager的实现类，他的内部实现是：

```java
class FragmentManagerImpl extends FragmentManager {
    ArrayList<Fragment> mActive;
    ArrayList<Fragment> mAdded;
    public Fragment findFragmentByTag(String tag) { 
           if (mAdded != null && tag != null) { 
               for (int i=mAdded.size()-1; i>=0; i--) {
                Fragment f = mAdded.get(i);
                if (f != null && tag.equals(f.mTag)) {
                        return f;
                }
            }
        }       
          if (mActive != null && tag != null) {
               for (int i=mActive.size()-1; i>=0; i--) {
                    Fragment f = mActive.get(i);
                    if (f != null && tag.equals(f.mTag)) {
                          return f;
                }
            }
        } 
          return null;
    }
}
```

从上面看到，先从mAdded中查找是否有该Fragment，如果没找到，再从mActive中查找是否有该Fragment。mAdded是已经添加到Activity的Fragment的集合，mActive不仅包含mAdded，还包含虽然不在Activity中，但还在回退栈中的Fragment。



### 五、Fragment通信

#### 1、Fragment向Activity传递数据

首先，在Fragment中定义接口，并让Activity实现该接口（具体实现省略）：

```java
public interface OnFragmentInteractionListener {    
void onItemClick(String str);  //将str从Fragment传递给Activity}
```

在Fragment的onAttach()中，将参数Context强转为OnFragmentInteractionListener对象：

```java
public void onAttach(Context context) {
    super.onAttach(context);
        if (context instanceof OnFragmentInteractionListener) {
        mListener = (OnFragmentInteractionListener) context;
    } else {
                throw new RuntimeException(context.toString()
                + " must implement OnFragmentInteractionListener");
    }
}
```

并在Fragment合适的地方调用`mListener.onItemClick("hello")`将”hello”从Fragment传递给Activity。

#### 2、Activity向Fragment传递数据

Activity向Fragment传递数据比较简单，获取Fragment对象，并调用Fragment的方法即可，比如要将一个字符串传递给Fragment，则在Fragment中定义方法：

```java
public void setString(String str) { 
    this.str = str;
}
```

并在Activity中调用`fragment.setString("hello")`即可。

#### 3、Fragment之间通信

由于Fragment之间是没有任何依赖关系的，因此如果要进行Fragment之间的通信，建议通过Activity作为中介，不要Fragment之间直接通信。



### 六、FragmentStatePageAdapter与FragmentPageAdapter的区别

在ViewPager中使用Fragment的情况下，可以给ViewPager设置两种Adapter，一种是FragmentStatePagerAdapter，一种是FragmentPagerAdapter。
那这两种Adapter有什么区别呢？

- **FragmentStatePageAdapter**
  FragmentStatePagerAdapter会销毁不需要的Fragment，一般来说，ViewHolder会保存正在显示的Fragment和它左右两边第一个Fragment，分别为A、B、C，那么当显示的Fragment变成C时，保存的Fragment就会变成B、C、D了，**而A此时就会被销毁**，但是需要注意的是，此时A在销毁的时候，会通过onSaveInstanceState方法来保存Fragment中的Bundle信息，当再次切换回来的时候，就可以利用保存的信息来恢复到原来的状态。
  图示如下

![1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/ZyJXRS5rxWViQJGj2xJZN2Yr4FtnwflsWsft6RPUivg!/r/dMEAAAAAAAAA)

---

**FragmentPageAdapter**
FragmentPageAdapter**会调用事务的detach(注意是事务的detach不是fragment的onDetach)方法来处理，而不是使用remove方法**。因此，FragmentPageAdapter只是销毁了Fragment的视图，其实例还是保存在FragmentManager中。

![2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/iwKr4wQz1Dyg4iIl1CgBPLYpRBdTeimK1JpzyytbrQk!/r/dFUAAAAAAAAA)

**如何选择Adapter呢**
从上文得知，FragmentStatePageAdapter适用于Fragment较多的情况，例如月圆之夜这个卡牌游戏，需要展示数十个不同的场景，需要数十个不同的Fragment，如果这些Fragment都保存在FragmentManager中的话，对应用的性能会造成很大的影响。
而FragmentPageAdapter则适用于固定的，少量的Fragment情况，例如和TabLayout共同使用时，典型的一个例子就是知乎上方的TabLayout。



**1、replace，加回退栈，Fragment不销毁，但是切换回销毁视图和重新创建视图。**

 **2、replace,不加回退栈，Fragment销毁掉。**

  **3、hide、show,Fragment不销毁，也不销毁视图。隐藏和显示不走生命周期。**



### 七、FragmentTransaction

Fragment完整生命周期依次是： onAttach()、onCreate()、onCreateView()、onActivityCreated()、onStart()、onResume()、 onPause()、onStop()、onDestroyView()、onDestroy()、onDetach()。
几个主要的方法： 

- onAttach():当碎片和活动建立关联的时候调用。 

- onCreate():当碎片被建立的时候调用。 

- onCreateView():为碎片创建视图（加载布局）时调用。 

- onActivityCreated():确保与碎片相关联的活动一定已经创建完毕的时候调用。 

- onDestroyView():当碎片管理的视图被移除的时候调用。 

- onDetach():当碎片和活动解除关联的时候调用。

- 创建时： **onAttach()、onCreate()**、onCreateView()、onActivityCreated()、onStart()、onResume()。

- 销毁时（没有用到返回栈）： onPause()、onStop()、onDestroyView()、**onDestroy()、onDetach()。**

- 重新返回到上一个Fragment（没有用到返回栈）： onAttach()、onCreate()、onCreateView()

- onActivityCreated()、onStart()、onResume()。

- **当用到返回栈时**

  ```java
  将Fragment增加到返回栈中：
  FragmentManager fm = getSupportFragment();
  FragmentTransaction ft = fm.beginTransaction();
  ft.replace(ID, 实例);
  ft.addToBackStack(null);//参数是描述返回栈的状态，一般为null。
  ft.commit();
  ```

  - 销毁时（用到返回栈）： onPause()、onStop()、onDestroyView()。 

  - 重新返回到上一个Fragment（用到返回栈）： onCreateView()、onActivityCreated()、onStart()、onResume()。



#### 1、FragmentTransaction方法详解

注意：我们操作的fragment会被**增加到一个队列中**，然后根据顺序显示该队列中已 经创建视图的fragmet（调用了onCreateView(……)）。 
入队的标准是：**该fragment的onCreateView（……）被调用。** 
出队的标准是：**该fragment的onDetach（）被调用。**

- **add(id, fragment)** —— 增加framgent到队列中，并显示该fragment到指定布局中。 
  生命周期调用： 
  - 当fragment与activity**连接并被建立时**（onAttach()、onCreate()**被**调用过） 
    onCreateView()、onActivityCreated()、onStart()、onResume()。 
  - 当fragment与activity**未连接并未被建立时**（onAttach()、onCreate()**未被**调用过） 
    onAttach()、onCreate()、onCreateView()、onActivityCreated()、onStart()、onResume()。 
    注意：同一个Fragmen不能增加到队列两次或多次。
- **show(fragment)** —— 显示队列中的指定framgent
  生命周期的调用： 
  - 当队列中**不存在**该fragment时，回调onAttach()、onCreate()。 
  - 当队列中**存在**该fragment时并被调用过hide(fragment)时，回调onHiddenChange(boolean)。 
    其他情况没有回调函数。
- **replace(id, fragment)** —— 替换队列中的指定framgent
  - 先检查队列中是否已经存在，存在就会崩溃
  - 不存在就会进入队列并把其他fragment清出队列，最后显示该fragment到指定布局中。 
    生命周期的调用：同add(id, fragment)。
- **remove(fragment)** —— 销毁队列中指定的fragment。 
  生命周期调用： 
  - 当队列中不存在该fragment时，不会有任何反应。 
  - 当队列中存在该fragment时，fragment的生命周期执行情况主要依赖是否当前fragment进入到返回栈。
- **hide(fragment)** —— 隐藏队列中指定的fragment，相当于调用视图的.setVisibility(View.GONE) 
  生命周期的调用： 
  - 当队列中**存在**该fragment时，回调onHiddenChange(boolen) 
  - 当队列中**不存在**该fragment时，回调onAttach()、onCreate()、onHiddenChange(boolen)。
- **detach(fragment)** —— 销毁指定frament的**视图**，并且该fragment的onCreateVieew(……)不能再被调用（除非调用attach(fragment)重新连接） 
  生命周期的调用： 
  - 当队列中**存在**该fragment时，回调onDestroyView() 
  - 当队列中**不存在**该fragment时，回调onAttach()、onCreate()。
- **attach(fragment)** —— 创建指定fragment的视图。标识该fragment的onCreateView(……)能被调用。 
  生命周期的调用： 
  - 当队列中**存在**该fragment时且被调用detach(fragment)时，回调createView()、onActivityCreated()
- **onResume()**
  - 当队列中不存在该fragment时，回调onAttach()、onCreate()。 
    其他情况没有用。
- **addToBackStack(string)** —— 使本次事务增加的fragment进入当前activity的返回栈中。当前参数是对返回栈的描述，没什么实际用途。传入null即可。
- **commit()** —— 提交本次事务，可在**非主线程**中被调用。主要用于多线程处理情况。
- **commitNow()** —— 提交本次事务，**只在主线程**中被调用。 这时候**addToBackStack(string)不可用**。



### 链接

- [Bugly](https://mp.weixin.qq.com/s/dUuGSVhWinAnN9uMiBaXgw)