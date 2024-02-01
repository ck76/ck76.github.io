懒加载主要用于ViewPager且每页是Fragment的情况，场景为微信主界面，底部有4个tab，当滑到另一个tab时，先显示”正在加载”，过一会才会显示正常界面。

默认情况，ViewPager会缓存当前页和**左右相邻的界面**。实现懒加载的主要原因是：用户没进入的界面需要有一系列的网络、数据库等耗资源、耗时的操作，预先做这些数据加载是不必要的。

这里懒加载的实现思路是：用户不可见的界面，只初始化UI，但是不会做任何数据加载。等滑到该页，才会异步做数据加载并更新UI。

ViewPager默认缓存左右相邻界面，为了避免不必要的重新数据加载（重复调用`onCreateView()`），因为有4个tab，因此将离线缓存的**半径**设置为3，即`setOffscreenPageLimit(3)`。

懒加载主要依赖Fragment的`setUserVisibleHint(boolean isVisible)`方法，当Fragment变为可见时，会调用`setUserVisibleHint(true)`；当Fragment变为不可见时，会调用`setUserVisibleHint(false)`，且该方法调用时机：

- onAttach()之前，调用`setUserVisibleHint(false)`。
- onCreateView()之前，如果该界面为当前页，则调用`setUserVisibleHint(true)`，否则调用`setUserVisibleHint(false)`。
- 界面变为可见时，调用`setUserVisibleHint(true)`。
- 界面变为不可见时，调用`setUserVisibleHint(false)`。



```java
public abstract class BaseLazyFragment extends Fragment {
    //根view 初始化完成？ 对用户可见？
    protected View rootView;
    private boolean isInitView = false;
    private boolean isVisible = false;

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        rootView = inflater.inflate(setContentView(), container, false);
        initView();
        isInitView = true;
        isCanLoadData();
        return rootView;
    }

    @Override
    public void setUserVisibleHint(boolean isVisibleToUser) {
        super.setUserVisibleHint(isVisibleToUser);
        Log.i("ck", "是否可见？：" + isVisibleToUser);
        //isVisibleToUser这个boolean值表示:该Fragment的UI 用户是否可见，获取该标志记录下来
        if (isVisibleToUser) {
            isVisible = true;
            isCanLoadData();
        } else {
            isVisible = false;
        }
    }

    private void isCanLoadData() {
        //所以条件是view初始化完成并且对用户可见
        if (isInitView && isVisible) {
            lazyLoad();
            Log.i("ck", "加载数据");
            //防止重复加载数据
            isInitView = false;
            isVisible = false;
        }
    }

    /**
     * 加载页面布局文件
     *
     * @return
     */
    protected abstract int setContentView();

    /**
     * 让布局中的view与fragment中的变量建立起映射
     */
    protected abstract void initView();

    /**
     * 加载要显示的数据
     */
    protected abstract void lazyLoad();

}
```





