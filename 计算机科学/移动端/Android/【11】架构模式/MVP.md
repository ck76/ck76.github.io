
# MVC

可能由于MVP、MVVM的兴起，MVC在android中的应用变得越来越少了，但MVC是基础，理解好MVC才能更好的理解MVP,MVVM。因为后两种都是基于MVC发展而来的。 

![MVC3](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/tj4zFjQJDdiISzVeEr63VSckMi34fN8T4eskuOTwUqQ!/r/dDUBAAAAAAAA)

## 结构

箭头→代表的是一种事件流向，并不一定要持有对方，比如上图中model→view的事件流向，view可以通过注册监听器的形式得到model发来的事件。在设计中model view controller之间如果要通讯，尽量设计成不直接持有，这样方便复用，也符合mvc的设计初衷。 在Android中三者对应的关系如下： 

![MVC1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/tj4zFjQJDdiISzVeEr63VSckMi34fN8T4eskuOTwUqQ!/r/dDUBAAAAAAAA)

**视图层(View)**
对应于xml布局文件和java代码动态view部分

**控制层(Controller)**
MVC中Android的控制层是由Activity来承担的，Activity本来主要是作为初始化页面，展示数据的操作，但是因为XML视图功能太弱，所以Activity既要负责视图的显示又要加入控制逻辑，承担的功能过多。

**模型层(Model)**针对业务模型，建立的数据结构和相关的类，它主要负责网络请求，数据库处理，I/O的操作。

由于android中有个god object的存在activity，再加上android中xml布局的功能性太弱，所以activity承担了绝大部分的工作。所以在android中mvc更像是这种形式： 

![MVC2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/6nsuJ1ZcLJ70eyuB7KvImUabPmdP9YE2rBB34MmSWX4!/r/dFIBAAAAAAAA)

## **MVC sample**

![MVC4](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/fmyE*oFPVtJ8aVt9KTicFIOCXManax6AVr6W8wdSR14!/r/dDcBAAAAAAAA)

结构很简单，这里介绍下其中的关键代码

```java
public interface BaseModel {
    void onDestroy();
}
```

BaseModel顾名思义就是所有业务逻辑model的父类，这里的onDestroy()方法用于跟activity或者fragment生命周期同步，在destroy做一些销毁操作

```java
public interface Callback1<T> {
    void onCallBack(T t);
}
public interface Callback2<T,P> {
    void onCallBack(T t,P p);
}
```

Callback是根据View或者Controller调用Model时回调的参数个数选择使用

```java
public class SampleModel  implements BaseModel{

    public void  getUserInfo(String uid,Callback1<UserInfo> callback)
    {

        UserInfo userInfo= new HttpUtil<UserInfo>().get(uid);
        callback.onCallBack(userInfo);

    }

    @Override
    public void onDestroy() {

    }

    public class UserInfo
    {
        private int age;
        private String name;

        public int getAge() {
            return age;
        }

        public void setAge(int age) {
            this.age = age;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }
    }
}
```

SampleModel是我们业务逻辑的具体实现

```java
public class SampleActivity extends AppCompatActivity {
    private SampleModel sampleModel;
    Button button;
    EditText textView;
    TextView tvAge,tvName;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_sample);
        sampleModel=new SampleModel();
        button.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                getUserInfo(textView.getText().toString());
            }
        });

    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        sampleModel.onDestroy();
    }
    
   /**
     * 获取用户信息
     * @param uid
     */
    private void getUserInfo(String uid)
    {
        sampleModel.getUserInfo(uid, new Callback1<SampleModel.UserInfo>() {
            @Override
            public void onCallBack(SampleModel.UserInfo userInfo) {
                setDataToView(userInfo);
            }
        });
    }

    /**
     * 设置用户信息到view
     */
    private void setDataToView(SampleModel.UserInfo userInfo)
    {
        tvAge.setText(userInfo.getAge());
        tvName.setText(userInfo.getName());
    }

}
```

前面说了Activity充当View和Controller，但是我们依然要区分到底哪一部分是View的操作，哪一部分是Controller的操作，我们分析下事件的流向  。 

- button点击事件的触发：View→Controller 
- 获取用户信息事件的触发：Controller→Model 
- 绑定用户信息到View：Controller→View 至此MVC就讲完了 

**MVC总结**

我们这里根据sample来总结下：

- 具有一定的分层，model彻底解耦，controller和view并没有解耦
- 层与层之间的交互尽量使用回调或者去使用消息机制去完成，尽量避免直接持有
- controller和view在android中无法做到彻底分离，但在代码逻辑层面一定要分清
- 业务逻辑被放置在model层，能够更好的复用和修改增加业务

---

# MVP

## 说明

MVP跟MVC很相像，文章开头列出了很多种MVC的设计图，所以根据MVC的发展来看，我们把MVP当成MVC来看也不为过，因为MVP也是三层，唯一的差别是Model和View之间不进行通讯，都是通过Presenter完成。

前面介绍MVC的时候提到了算是致命缺点吧，在android中由于activity（god object）的存在，Controller和View很难做到完全解耦，但在MVP中就可以很好的解决这个问题。
看下MVP的设计图：

![MVP1](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/Pk1CZc9ms3xmn3hUpcbbbAgcllVSRK29PgeqtU7MtNY!/r/dDUBAAAAAAAA)

## MVP sample

项目结构：

![MVP2](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/6Fd.TUXYeRhGMMNyWqrQ31p9x6aQiRaFCSCVAN.NAHA!/r/dDcBAAAAAAAA)

callback，http包下内容基本一致，主要看下不同的地方

 

```java
public interface BasePresenter {
    void onDestroy();
}
```

BasePresenter类似于MVC中的BaseModel，主要负责业务逻辑的实现。我们这里没有把业务逻辑放在Model里去实现，当然把主要业务逻辑放在Model中去实现也是可以的。google的MVP实现方案是把业务逻辑放在presenter中，弱化Model，我们这里也是这样做的。

```java
public interface BaseView<P extends BasePresenter> {
    void setPresenter(P presenter);
}
```

BaseView是所有View的父类，将android中的view抽象话出来，只有跟view相关的操作都由baseView的实现类去完成。

```java
public class SampleContract {
    public static class Presenter implements BasePresenter
    {
        public void  getUserInfo(String uid,Callback1<SampleModel.UserInfo> callback)
        {
            SampleModel.UserInfo userInfo= new HttpUtil<SampleModel.UserInfo>().get(uid);
            callback.onCallBack(userInfo);
        }

        @Override
        public void onDestroy() {

        }
    }
    public interface View extends BaseView<Presenter>
    {
         void setDataToView(SampleModel.UserInfo userInfo);
    }
}
```

Contract 契约类这是Google MVP与其他实现方式的又一个不同，契约类用于定义同一个界面的view的接口和presenter的具体实现。好处是通过规范的方法命名和注释可以清晰的看到整个页面的逻辑。

```java
public class SampleActivity extends AppCompatActivity implements SampleContract.View{
    private  SampleContract.Presenter mPresenter;
    Button button;
    EditText textView;
    TextView tvAge,tvName;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_sample);
        setPresenter(new SampleContract.Presenter());

        button.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                mPresenter.getUserInfo(textView.getText().toString(), new Callback1<SampleModel.UserInfo>() {
                    @Override
                    public void onCallBack(SampleModel.UserInfo userInfo) {
                        setDataToView(userInfo);
                    }
                });
            }
        });

    }
     @Override
    protected void onDestroy() {
        super.onDestroy();
        mPresenter.onDestroy();
    }

    @Override
    public void setDataToView(SampleModel.UserInfo userInfo) {
        tvAge.setText(userInfo.getAge());
        tvName.setText(userInfo.getName());
    }


    @Override
    public void setPresenter(SampleContract.Presenter presenter) {
        mPresenter=presenter;
    }
}
```

这里的SampleActivity实现了SampleContract.View只是作为View存在的。虽然看起来，跟MVC中的实现很相似，但却有本质的区别。mPresenter为Model和View之间交互的桥梁。Presenter跟View相互持有，这里SampleActivity实现了SampleContract.View，mPresenter作为SampleActivity的成员变量，SampleActivity当然持有mPresenter，由于mPresenter是非静态的成员标量，因此默认持有SampleActivity的引用。

**MV总结**

通过引入接口BaseView，让相应的视图组件如Activity，Fragment去实现BaseView，实现了视图层的独立，通过中间层Preseter实现了Model和View的完全解耦。MVP彻底解决了MVC中View和Controller傻傻分不清楚的问题，但是随着业务逻辑的增加，一个页面可能会非常复杂，UI的改变是非常多，会有非常多的case，这样就会造成View的接口会很庞大。

## 注意事项

**接口的必要性**
可能有的同学会问，为什么要写一个 MVPView 的接口，直接把 Activity 本身传入到 Presenter 不行吗？这当然是可行的，这里使用接口主要是为了代码的复用，试想一下，如果直接传入 Activity，那么这个 Presenter 就只能为这一个 Activity 服务。举个例子，假设有个 App 已经开发完成了，可以在手机上正常使用，现在要求做平板上的适配，在平板上的界面显示效果有所变化，TextView 并不是直接在 Activity 中的，而是在 Fragment 里面，如果没有使用 View 的接口的话，那就需要再写一个针对 Fragment 的 Presenter，然后把整个过程再来一遍。但是使用 View 的接口就很简单了，直接让 Fragment 实现这个接口，然后复写接口里面的方法，Presenter 和 Model 层都不需要做任何改动。同理，Model 层也可以采用接口的方式来写。

**防止内存泄漏**
其实上面的代码存在内存泄漏的风险。试想一下，如果在点击 Button 之后，Model 获取到数据之前，退出了 Activity，此时由于 Activity 被 Presenter 引用，而 Presenter 正在进行耗时操作，会导致 Activity 的对象无法被回收，造成了内存泄漏，解决的方式很简单，在 Activity 退出的时候，把 Presenter 对中 View 的引用置为空即可。

```java
// Presenter.java
public void detachView() {
    view = null;
}

// MVPActivity.java
@Override
protected void onDestroy() {
    super.onDestroy();
    presenter.detachView();
}
```

另外还有一个问题，虽然这里 Activity 不会内存泄漏了，但是当 Activity 退出之后，Model 中请求数据就没有意义了，所以还应该在 detachView 方法中，把 Handler 的任务取消，避免造成资源浪费，这个比较简单，就不贴代码了。 


