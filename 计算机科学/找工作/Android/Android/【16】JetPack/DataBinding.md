[TOC]

### DataBinding简介

**DataBinding**解决了Android UI 编程的一个痛点，其主要优势在于：

1. 对于MVVM的支持：官方原生支持 MVVM 模型可以让我们在不改变既有代码框架的前提下，非常容易地使用这些新特性。
2. 提高开发效率： 
   - 去掉Acitivity和Fragments中更新UI数据的代码，让业务逻辑和UI代码分离；
   - XML成为UI数据的唯一真实来源；
   - 减少定义view id 和使用findViewById();
3. 性能高、功能强： 
   - 充分考虑了性能因素，高效的绑定和更新数据；
   - 更安全，在编译时会发现由于错误的ID而引起的Errors；
   - 保证代码在主线程经常；

在DataBinding推出之前，市面上已经有类似的第三方库在被使用，比如：

- ButterKnife;
- Android Annotations;
- RoboBinding;

DataBinding让这些依赖注入框架会慢慢地失去市场，因为在 Java代码中直接使用View变量的情况会越来越少。



### 使用

#### 配置

修改对应模块（Module）的 build.grade：

```groovy
android {
    ....
    dataBinding {
        enabled = true
    }
}
```

#### 布局文件

使用DataBinding之后，xml的布局文件就不再用于单纯地展示UI元素，还需要定义UI元素用到的变量。所以，它的根节点不再是一个 ViewGroup，而是变成了**layout**，并且新增了一个节点**data**，用来定义绑定的数据。

```xml
<layout xmlns:android="http://schemas.android.com/apk/res/android">
    <data>
    <!--添加要绑定的数据-->
    </data>
    
    <!--下面的布局文件就是原先的根节点（Root Element）-->
    <LinearLayout>
    ....
    </LinearLayout>
</layout>
```

```java
<?xml version="1.0" encoding="utf-8"?>
<layout xmlns:android="http://schemas.android.com/apk/res/android">

    <data>

        <variable
            name="myViewModel"
            type="cn.ck.xjbl.jetpack.databinding.DataBindingViewModel" />
    </data>

    <LinearLayout
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <TextView
            android:id="@+id/txt_databinding_test"
            android:layout_width="match_parent"
            android:layout_height="50dp"
            android:background="#fff3"
            android:gravity="center"
            android:text="@{myViewModel.name}" />
    </LinearLayout>

</layout>
```



#### LiveData

```java
public class DataBindingViewModel extends ViewModel {
    public MutableLiveData<String> name;

    public LiveData<String> get() {
        name = new MutableLiveData<>();
        name.setValue("ck");
        return name;
    }

}
```



#### DataBindingDemo

```java
public class DataBindingDemo extends AppCompatActivity {

    private ActivityDataBindingDemoBinding binding;
    private DataBindingViewModel model;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = DataBindingUtil.setContentView(this, R.layout.activity_data_binding_demo);
        model = ViewModelProvider.AndroidViewModelFactory.getInstance(getApplication()).create(DataBindingViewModel.class);
        model.get();
        model.name.setValue("ckkkkkkk");
        binding.setMyViewModel(model);
        /**注释也可以**/
        binding.setLifecycleOwner(this);
    }
}
```



### 链接

- https://www.jianshu.com/p/ba4982be30f8

- https://www.jianshu.com/p/0fe0b6b7dae1