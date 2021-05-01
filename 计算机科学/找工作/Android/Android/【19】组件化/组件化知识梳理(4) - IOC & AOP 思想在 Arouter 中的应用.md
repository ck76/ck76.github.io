[TOC]

# 一、IoC

## 1.1 基本概念

`IoC(Inversion of Control)`，中文翻译为 **控制反转**，具体实现是 **由容器来控制业务对象之间的依赖关系**，最终的目的是 **避免和降低对象间的依赖关系**。本质上是控制权由应用代码转到了外部容器，控制权的转移即是所谓的反转。

`IoC`的实现策略有两种：

- 依赖查找：容器中的 **受控对象** 通过 **容器** 的`API`来查找自己所依赖的资源和协作对象。

- 依赖注入（

  ```
  Dependency Injection
  ```

  ）：对象只提供普通的方法让容器去决定依赖关系，容器全权负责组件的装配，它会把符合依赖关系的对象通过属性或者是构造函数传递给需要的对象。优点是：

  - 查询依赖操作和应用代码分离
  - 受控对象不会使用到容器的特定`API`，这样我们的受控对象可以搬出容器单独使用。

## 1.2 IoC 在 Arouter 中的应用

在第一篇文章中，我们介绍了`Arouter`中依赖注入的使用方法。在调用模块中，使用下面的方法来启动 **目标页面**：



```java
@Route(path = RouterMap.HOME_FRAGMENT)
public class HomeFragment extends Fragment {

    private class HomeClickListener implements View.OnClickListener {

        @Override
        public void onClick(View v) {
            int id = v.getId();
            if (id == R.id.bt_no_result) {
               //...
            } else if (id == R.id.bt_inject) {
                SerialBean bean = new SerialBean();
                bean.setName("SerialBean");
                bean.setAge(18);
                ARouter.getInstance().build(RouterMap.INJECT_ACTIVITY)
                        .withInt(ConstantMap.INJECT_AGE, 18)
                        .withObject(ConstantMap.INJECT_OBJECT, bean)
                        .navigation();
            }
        }
    }
}
```

而在 **目标页面** 中，只需要使用`@Autowired`来声明对应的`Key`，就可以自动接收跳转过来的参数值，对被注解的成员变量进行赋值：



```java
@Route(path = RouterMap.INJECT_ACTIVITY)
public class InjectActivity extends AppCompatActivity {

    @Autowired(name = ConstantMap.INJECT_AGE)
    int age;

    @Autowired(name = ConstantMap.INJECT_OBJECT)
    SerialBean bean;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        ARouter.getInstance().inject(this);
        Utils.toast(this, "age=" + age + ",bean.age=" + bean.getAge() + ",bean.name=" + bean.getName());
    }
}
```

在上面的代码中，和`1.1`中谈到的基础概念里面的对应关系为：

- 受控对象 -> 使用`@Autowired`注解的变量。
- 容器 -> 在编译期动态创建的`InjectActivity$$ARouter$$Autowired` 类。

关于依赖注入的原理，我们在 [组件化知识梳理(2) - Arouter 源码分析之 Complier SDK](https://www.jianshu.com/p/a1f6db686b17) 中已经分析过了 。编译期动态创建的注入类如下所示，`InjectActivity`不再去对成员变量进行赋值，而是由 **外部容器** 来提供具体的实现，这里的 **外部容器** 就是在编译期动态创建`ISyringe`的实现类。



```java
public class InjectActivity$$ARouter$$Autowired implements ISyringe {
  private SerializationService serializationService;

  @Override
  public void inject(Object target) {
    serializationService = ARouter.getInstance().navigation(SerializationService.class);
    InjectActivity substitute = (InjectActivity)target;
    substitute.age = substitute.getIntent().getIntExtra("inject_age", substitute.age);
    if (null != serializationService) {
      substitute.bean = serializationService.parseObject(substitute.getIntent().getStringExtra("inject_object"), new com.alibaba.android.arouter.facade.model.TypeWrapper<SerialBean>(){}.getType());
    } else {
      Log.e("ARouter::", "You want automatic inject the field 'bean' in class 'InjectActivity' , then you should implement 'SerializationService' to support object auto inject!");
    }
  }
}
```

整个依赖注入的流程为：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymrubdmoj30n20ebdgw.jpg)

依赖注入的流程

# 二、AOP

## 2.1 基本概念

`AOP(Aspect Oriented Programming)`，中文翻译为 **面向切面编程**。

`AOP`是对于`OOP`的一种补充，当我们采用`OOP`时，其目的就是为了将功能模块化，每个模块处理自己的事情。`AOP`就是为了将多模块当中 **公用的功能** 集中起来进行管理。

关于`AOP`的详细说明可以参考下面这篇文章：[深入理解 Android 之 AOP](https://link.jianshu.com/?t=https%3A%2F%2Fblog.csdn.net%2Finnost%2Farticle%2Fdetails%2F49387395)。

## 2.2 AOP 在 Arouter 中的应用

首先想一下，在以前，我们从一个`Activity`跳转到另一个`Activity`都是直接采用`startActivity`方式，而我们页面之间的跳转关系有可能是非常复杂的，像下面这样：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymrs2jnwj30bb07vmwy.jpg)

复杂的页面跳转关系


然后，突然有一天，产品来了，说要给页面`2`加上与账户有关的信息，当其它任意页面跳转到页面`2`之前需要判断是否登陆了，如果没登陆，那么就跳转到页面`5`，这时候我们有两种选择：



- 在每个跳转页面`2`的地方都加上是否登陆的判断。
- 在页面`2`被调起后再去判断，如果没登陆，那么自己再执行一步跳转到页面`5`的操作。

看起来方案`2`比较可行，但是其实就是稍微好一点，假如页面`4`有一天也要加入相同的逻辑，那么我们是不是就需要把页面`2`内的代码又拷贝一遍呢？

`Arouter`给出的解决方法，就是 **拦截器**。



```java
@Interceptor(priority = 1, name = "重新分组进行拦截")
public class BaseInterceptor implements IInterceptor {

    @Override
    public void process(Postcard postcard, InterceptorCallback callback) {
        if (postcard.getExtra() == ConstantMap.LOGIN_EXTRA) {
            boolean isLogin = postcard.getExtras().getBoolean(ConstantMap.IS_LOGIN);
            if (!isLogin) {
                ARouter.getInstance().build(RouterMap.INTER_MIDDLE_ACTIVITY).navigation();
                callback.onInterrupt(null);
            } else {
                postcard.withString(ConstantMap.IS_LOGIN_EXTRA, "登录了!");
                callback.onContinue(postcard);
            }
        } else {
            callback.onContinue(postcard);
        }
    }

    @Override
    public void init(Context context) {}

}
```

所有没有配置`greenChannel()`的页面跳转，都会按造优先级的顺序通过各拦截器的`process`方法。在`@Route`注解中配置`extras`属性，然后在拦截器当中通过`postcard.getExtra()`进行识别，我们就可以区分目标页面的类型，根据它来判断跳转目标页面所需的条件，从而达到对于页面跳转的统一管理。



3人点赞



[组件化]()





作者：泽毛
链接：https://www.jianshu.com/p/0aacf897f7b3
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。