1. 我们可以在Fragment的onAttach()中通过getArguments()获得传进来的参数，并在之后使用这些参数。

2. 如果要获取Activity对象，不建议调用getActivity()，而是**在onAttach()中将Context对象强转为Activity对象**。

3. addToBackStack(“fname”)是可选的。FragmentManager拥有回退栈（BackStack），类似于Activity的任务栈，如果添加了该语句，就把**该事务加入回退栈，当用户点击返回按钮，会回退该事务**（回退指的是如果事务是add(frag1)，那么回退操作就是remove(frag1)）；如果没添加该语句，用户点击返回按钮会直接销毁Activity。

4. Fragment有一个常见的问题，即Fragment重叠问题，这是由于Fragment被系统杀掉，并重新初始化时再次将fragment加入activity，因此通过在外围加if语句能判断此时是否是被系统杀掉并重新初始化的情况。

5. Fragment有个常见的异常：Can not perform this action after onSaveInstanceState

   该异常出现的原因是：commit()在onSaveInstanceState()后调用。首先，**onSaveInstanceState()在onPause()之后，onStop()之前调用**。onRestoreInstanceState()在onStart()之后，onResume()之前。

   因此避免出现该异常的方案有：

   不要把Fragment事务放在异步线程的回调中，比如不要把Fragment事务放在AsyncTask的onPostExecute()，因此onPostExecute()可能会在onSaveInstanceState()之后执行。
   **逼不得已时使用commitAllowingStateLoss()。**

6. Fragment的onAttach()->onCreate()->onCreateView()->onActivityCreated()->onStart()都是在Activity的onStart()中调用的。
   **Fragment的onResume()在Activity的onResume()之后调用。**

7. **不加addToBackStack()和加addToBackStack()**
   1、当点击F1的按钮，调用replace()替换为F2，且不加addToBackStack()时，F1最后调用了onDestroy()和onDetach()。

   2、当点击F1的按钮，调用replace()替换为F2，且加addToBackStack()时，可以看到，F1被替换时，最后只调到了onDestroyView()，并没有调用onDestroy()和onDetach()。当用户点返回按钮回退事务时，F1会调onCreateView()->onStart()->onResume()，因此在Fragment事务中加不加addToBackStack()会影响Fragment的生命周期。

8. **addToBackStack(“”)**
   内部实现为：创建一个BackStackRecord对象，**该对象记录了这个事务的全部操作轨迹**（这里只做了一次add操作，并且加入回退栈），随后将该对象提交到FragmentManager的执行队列中，等待执行。

   ```java
   class BackStackRecord extends FragmentTransaction implements FragmentManager.BackStackEntry, Runnable {}
   ```

   BackStackRecord有三重含义：

   1. 继承了FragmentTransaction，即事务，**保存了整个事务的全部操作轨迹。**
   2. 实现了BackStackEntry，作为回退栈的元素，正是因为该类拥有事务全部的操作轨迹，因此在popBackStack()时能回退整个事务。
   3. 继承了Runnable，即被放入FragmentManager执行队列，等待被执行。

9. **popBackStack()，有以下几种变种：**

   popBackStack()：将回退栈的栈顶弹出，并回退该事务。

   popBackStack(String name, int flag)：name为addToBackStack(String name)的参数，通过name能找到回退栈的特定元素，flag可以为0或者FragmentManager.POP_BACK_STACK_INCLUSIVE，0表示只弹出该元素以上的所有元素，POP_BACK_STACK_INCLUSIVE表示弹出包含该元素及以上的所有元素。这里说的弹出所有元素包含回退这些事务。

   popBackStack()是异步执行的，是丢到主线程的MessageQueue执行，popBackStackImmediate()是同步版本。
   
10. **Fragment向Activity传递数据**

   在Fragment中定义接口，并让Activity实现该接口（具体实现省略）：
   public interface OnFragmentInteractionListener { void onItemClick(String str); //将str从Fragment传递给Activity}

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

   并在Fragment合适的地方调用mListener.onItemClick(“fragment”)将”fragment”从Fragment传递给Activity

11. **Activity向Fragment传递数据**

    Activity向Fragment传递数据比较简单，获取Fragment对象，并调用Fragment的方法即可，比如要将一个字符串传递给Fragment，则在Fragment中定义方法：

    ```java
    public void setString(String str) { 
        this.str = str;
    }
    ```

    并在Activity中调用fragment.setString(“fragment”)即可。

12.**Fragment之间通信**

```java
由于Fragment之间是没有任何依赖关系的，因此如果要进行Fragment之间的通信，建议通过Activity作为中介，不要Fragment之间直接通信。
```

13. **DialogFragment**

    DialogFragment是Android 3.0提出的，代替了Dialog，用于实现对话框。他的优点是：**即使旋转屏幕，也能保留对话框状态。**

    如果要自定义对话框样式，只需要继承DialogFragment，并重写onCreateView()，该方法返回对话框UI。这里我们举个例子，实现进度条样式的圆角对话框。

    ```java
    public class ProgressDialogFragment extends DialogFragment {    @Override
        public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
            getDialog().requestWindowFeature(Window.FEATURE_NO_TITLE); //消除Title区域
            getDialog().getWindow().setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));  //将背景变为透明
            setCancelable(false);  //点击外部不可取消
            View root = inflater.inflate(R.layout.fragment_progress_dialog, container);
            return root;
        }
         public static ProgressDialogFragment newInstance() {
            return new ProgressDialogFragment();
        }
    }
    ```

    进度条动画我们使用Lottie(https://github.com/airbnb/lottie-android)实现，Lottie动画从这里(https://www.lottiefiles.com/)找到。使用非常方便，只需要下载JSON动画文件，然后在XML中写入：

    ```xml
    <com.airbnb.lottie.LottieAnimationView
    android:layout_width="wrap_content"  //大小根据JSON文件确定
    android:layout_height="wrap_content"
    app:lottie_fileName="loader_ring.json"   //JSON文件
    app:lottie_loop="true"    //循环播放
    app:lottie_autoPlay="true" />  //自动播放
    ```

    然后通过下面代码显示对话框：

    ```java
    ProgressDialogFragment fragment = ProgressDialogFragment.newInstance();
    fragment.show(getSupportFragmentManager(), "tag");//fragment.dismiss();
    ```

    为了实现圆角，除了在onCreateView()中把背景设为透明，还需要对UI加入背景：

    ```xml
    <shape xmlns:android="http://schemas.android.com/apk/res/android">
        <solid android:color="#ffffff"/>
        <corners
            android:radius="20dp"/>
    </shape>
    ```

14. **ViewPager+Fragment**

    默认，ViewPager会缓存当前页相邻的界面，比如当滑动到第2页时，会初始化第1页和第3页的界面（即Fragment对象，且生命周期函数运行到onResume()），可以通过setOffscreenPageLimit(count)设置离线缓存的界面个数。

15. **懒加载**

    默认情况，ViewPager会缓存当前页和左右相邻的界面。实现懒加载的主要原因是：用户没进入的界面需要有一系列的网络、数据库等耗资源、耗时的操作，预先做这些数据加载是不必要的。

    这里懒加载的实现思路是：用户不可见的界面，只初始化UI，但是不会做任何数据加载。等滑到该页，才会异步做数据加载并更新UI。

    底部用PagerBottomTabStrip(https://github.com/tyzlmjj/PagerBottomTabStrip)项目实现，上面是ViewPager，使用FragmentPagerAdapter。

    懒加载主要依赖Fragment的setUserVisibleHint(boolean isVisible)方法，当Fragment变为可见时，会调用setUserVisibleHint(true)；当Fragment变为不可见时，会调用setUserVisibleHint(false)，且该方法调用时机：

    1. onAttach()之前，调用setUserVisibleHint(false)。
    2. onCreateView()之前，如果该界面为当前页，则调用setUserVisibleHint(true)，否则调用setUserVisibleHint(false)。
    3. 界面变为可见时，调用setUserVisibleHint(true)。
       界面变为不可见时，调用setUserVisibleHint(false)。

    懒加载Fragment的实现：

    ```java
    public class LazyFragment extends Fragment {    private View mRootView;
        private boolean mIsInited;
        private boolean mIsPrepared;    @Override
        public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
            mRootView = inflater.inflate(R.layout.fragment_lazy, container, false);
            mIsPrepared = true;
            lazyLoad();
                return mRootView;
        }
    
         public void lazyLoad() {
            if (getUserVisibleHint() && mIsPrepared && !mIsInited) { 
                   //异步初始化，在初始化后显示正常UI
                loadData();
            }
        }
    
         private void loadData() {
             new Thread() {
             public void run() {
                    //1. 加载数据
                    //2. 更新UI
                    //3. mIsInited = true
                }
            }.start();
        }   
    
        @Override
        public void setUserVisibleHint(boolean isVisibleToUser) { 
               super.setUserVisibleHint(isVisibleToUser);
                if (isVisibleToUser) {
                lazyLoad();
            }
        }
    
        public static LazyFragment newInstance() {
               return new LazyFragment();
        }
    }
    ```

### 额外的补充

1. **add(),show(),hide(),replace()** 

   在大部分情况下，应使用show(),hide()，避免fragment重复加载。
   注意：当fragment中需要显示大量图片时，建议使用replace()。

2. **onHiddenChanged()**

   当使用add()+hide(),show()跳转到新的fragment时，旧的fragment不会回调onStop(),但是会回调onHiddenChanged(),而新的fragment则不会回调onHiddenChanged()。

3. **FragmentManager**

   在当前activity时，使用getSupportFragmentManager()是获取的FragmentActivity的FragmentManager。

   在fragment时，使用getSupportFragmentManager()是获取的父类Fragment的FragmentManager，如果没有父类，则获取所属Activity的FragmentManager，而getChildFragmentManager是获取的自己的FragmentManager。