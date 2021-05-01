[TOC]

# 组件化知识梳理目录

[组件化知识梳理(1) - Arouter 的基本使用](https://www.jianshu.com/p/5c109c51d7ba)
[组件化知识梳理(2) - Arouter 源码分析之 Complier SDK](https://www.jianshu.com/p/a1f6db686b17)
[组件化知识梳理(3) - Arouter 源码分析之运行时 SDK](https://www.jianshu.com/p/a662c0b8edd2)

# 一、Api SDK 分析

今天我们就来分析一下运行时`SDK`的实现，读这篇文章之前，一定要先看前面两篇文章，不然会弄不明白：

- [组件化知识梳理(1) - Arouter 的基本使用](https://www.jianshu.com/p/5c109c51d7ba)
- [组件化知识梳理(2) - Arouter 源码分析之 Complier SDK](https://www.jianshu.com/p/a1f6db686b17)

`Api SDK`最终的目的很简单，就是通过 **固定的包名来加载映射文件**，使用者在执行跳转或者获取服务的时候，通过查找映射关系，来完成功能。

## 1.1 初始化

首先我们来看一下初始化的流程：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymkpakvfj30fl0i10t2.jpg)

初始化流程


当调用`ARouter.getInstance()`方法时，内部转调了`_ARouter`的同名方法：





```java
public final class ARouter {

    public static void init(Application application) {
        if (!hasInit) {
            logger = _ARouter.logger;
            _ARouter.logger.info(Consts.TAG, "ARouter init start.");
            //实际上是通过 init 来完成初始化的。
            hasInit = _ARouter.init(application);

            if (hasInit) {
                _ARouter.afterInit();
            }

            _ARouter.logger.info(Consts.TAG, "ARouter init over.");
        }
    }

}
```

在`_ARouter`中，使用`LogisticsCenter`来完成初始化：



```java
final class _ARouter {

    protected static synchronized boolean init(Application application) {
        mContext = application;
        //通过 LogisticsCenter 来初始化，第二个参数是读取 class 文件的线程池。
        LogisticsCenter.init(mContext, executor);
        logger.info(Consts.TAG, "ARouter init success!");
        hasInit = true;
        return true;
    }

}
```

`LogisticsCenter`首先会 **扫描指定包名下的所有 .class 文件**，将其类名放入到`routerMap`当中。接下来，根据固定的命名规则，遍历`routerMap`，找到三种指定的类型，通过反射的方式实例化，然后调用它的`loadInto`方法给`WareHouse`中的成员变量进行赋值，也就是把将映射的关系加入到`WareHouse`当中。这样，当我们在调用`navigation`方法时，就可以通过`WareHouse`来找到`Uri`到对应的组件，并根据组件的类型进行操作。

这三种指定的类型如下所示：

- `groupsIndex`：分组信息
- `providerIndex`：`IProvider`信息
- `interceptorsIndex`：拦截器信息



```java
public class LogisticsCenter {

  public synchronized static void init(Context context, ThreadPoolExecutor tpe) throws HandlerException {
        mContext = context;
        executor = tpe;

        try {
            long startInit = System.currentTimeMillis();
            loadRouterMap();
            if (registerByPlugin) {
                logger.info(TAG, "Load router map by arouter-auto-register plugin.");
            } else {
                //保存了包名为 com.alibaba.android.arouter.routes 下的所有 class 文件
                Set<String> routerMap;

                //当版本号发生改变，或者是 debuggable 模式下，会重新去扫描。
                if (ARouter.debuggable() || PackageUtils.isNewVersion(context)) {
                    logger.info(TAG, "Run with debug mode or new install, rebuild router map.");
                    //扫描包名为 com.alibaba.android.arouter.routes 下的所有 class 文件，这里面考虑了 MultiDex 的情况。
                    routerMap = ClassUtils.getFileNameByPackageName(mContext, ROUTE_ROOT_PAKCAGE);
                    //缓存进入 SP 当中。
                    if (!routerMap.isEmpty()) {
                        context.getSharedPreferences(AROUTER_SP_CACHE_KEY, Context.MODE_PRIVATE).edit().putStringSet(AROUTER_SP_KEY_MAP, routerMap).apply();
                    }

                    PackageUtils.updateVersion(context);    // Save new version name when router map update finishes.
                //否则直接使用上次保存在 SP 文件中的扫描结果。
                } else {
                    logger.info(TAG, "Load router map from cache.");
                    routerMap = new HashSet<>(context.getSharedPreferences(AROUTER_SP_CACHE_KEY, Context.MODE_PRIVATE).getStringSet(AROUTER_SP_KEY_MAP, new HashSet<String>()));
                }

                logger.info(TAG, "Find router map finished, map size = " + routerMap.size() + ", cost " + (System.currentTimeMillis() - startInit) + " ms.");
                startInit = System.currentTimeMillis();
                //实例化三种类型对象，调用它的 loadInto 方法，对 WareHouse 中的变量进行赋值。
                for (String className : routerMap) {
                    if (className.startsWith(ROUTE_ROOT_PAKCAGE + DOT + SDK_NAME + SEPARATOR + SUFFIX_ROOT)) {
                        //只加载分组的根节点，并不加载具体的组件。
                        ((IRouteRoot) (Class.forName(className).getConstructor().newInstance())).loadInto(Warehouse.groupsIndex);
                    } else if (className.startsWith(ROUTE_ROOT_PAKCAGE + DOT + SDK_NAME + SEPARATOR + SUFFIX_INTERCEPTORS)) {
                        //拦截器。
                        ((IInterceptorGroup) (Class.forName(className).getConstructor().newInstance())).loadInto(Warehouse.interceptorsIndex);
                    } else if (className.startsWith(ROUTE_ROOT_PAKCAGE + DOT + SDK_NAME + SEPARATOR + SUFFIX_PROVIDERS)) {
                        //服务提供。
                        ((IProviderGroup) (Class.forName(className).getConstructor().newInstance())).loadInto(Warehouse.providersIndex);
                    }
                }
            }

             //...
        } catch (Exception e) {
             //...
        }
    }

}
```

## 1.2 跳转 & 获取服务流程

接下来，我们看一下一个最简单的跳转流程：



```java
ARouter.getInstance().build(RouterMap.NO_RESULT_ACTIVITY).navigation();
```

整个流程如下所示：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymkmvfuuj30e20kj74p.jpg)

跳转流程



这里主要是两步：

- `build`，创建一个`Postcard`，赋值`Uri`和分组信息。
- `navigation`，执行跳转或者返回对象，



```java
public final class ARouter {

    public Postcard build(String path) {
        return _ARouter.getInstance().build(path);
    }

}
```

`build`会进行简单的初始化，只是赋值路径 & 分组名。



```java
final class _ARouter {
    protected Postcard build(String path) {
        if (TextUtils.isEmpty(path)) {
            throw new HandlerException(Consts.TAG + "Parameter is invalid!");
        } else {
            PathReplaceService pService = ARouter.getInstance().navigation(PathReplaceService.class);
            if (null != pService) {
                path = pService.forString(path);
            }
            return build(path, extractGroup(path));
        }
    }

    protected Postcard build(String path, String group) {
        if (TextUtils.isEmpty(path) || TextUtils.isEmpty(group)) {
            throw new HandlerException(Consts.TAG + "Parameter is invalid!");
        } else {
            PathReplaceService pService = ARouter.getInstance().navigation(PathReplaceService.class);
            if (null != pService) {
                path = pService.forString(path);
            }
            return new Postcard(path, group);
        }
    }
}
```

`Postcard`中保存了整个调用链所有所需的信息：



```java
public final class Postcard extends RouteMeta {

    private Uri uri;
    private Object tag;             // A tag prepare for some thing wrong.
    private Bundle mBundle;         // Data to transform
    private int flags = -1;         // Flags of route
    private int timeout = 300;      // Navigation timeout, TimeUnit.Second
    private IProvider provider;     // It will be set value, if this postcard was provider.
    private boolean greenChannel;
    private SerializationService serializationService;

    // Animation
    private Bundle optionsCompat;    // The transition animation of activity
    private int enterAnim = -1;
    private int exitAnim = -1;

    public void navigation(Activity mContext, int requestCode, NavigationCallback callback) {
        ARouter.getInstance().navigation(mContext, this, requestCode, callback);
    }

}
```

在`_ARouter#navigation`中，会执行下面几步操作：

- 通过`LogisticsCenter#completion`方法通过`WareHouse`填充完整的`Postcard`的信息。
- 读取拦截器去处理。
- 如果拦截器没有拦截，那么会执行`_Arouter#_navigation`，这里面会执行`Activity`的跳转，或者是返回服务`IProvider`，或者是`Fragment`的实例。



```java
final class _ARouter {

    protected Object navigation(final Context context, final Postcard postcard, final int requestCode, final NavigationCallback callback) {
        try {
            //1.填充信息。
            LogisticsCenter.completion(postcard);
        } catch (NoRouteFoundException ex) {
            logger.warning(Consts.TAG, ex.getMessage());

            if (debuggable()) { // Show friendly tips for user.
                Toast.makeText(mContext, "There's no route matched!\n" +
                        " Path = [" + postcard.getPath() + "]\n" +
                        " Group = [" + postcard.getGroup() + "]", Toast.LENGTH_LONG).show();
            }

            if (null != callback) {
                callback.onLost(postcard);
            } else {    // No callback for this invoke, then we use the global degrade service.
                DegradeService degradeService = ARouter.getInstance().navigation(DegradeService.class);
                if (null != degradeService) {
                    degradeService.onLost(context, postcard);
                }
            }

            return null;
        }

        if (null != callback) {
            callback.onFound(postcard);
        }
        //2. 调用拦截器的处理逻辑。
        if (!postcard.isGreenChannel()) {   // It must be run in async thread, maybe interceptor cost too mush time made ANR.
            interceptorService.doInterceptions(postcard, new InterceptorCallback() {
                
                @Override
                public void onContinue(Postcard postcard) {
                    _navigation(context, postcard, requestCode, callback);
                }

                @Override
                public void onInterrupt(Throwable exception) {
                    if (null != callback) {
                        callback.onInterrupt(postcard);
                    }

                    logger.info(Consts.TAG, "Navigation failed, termination by interceptor : " + exception.getMessage());
                }
            });
        } else {
            //如果没有拦截，那么处理最后的逻辑。
            return _navigation(context, postcard, requestCode, callback);
        }

        return null;
    }
}
```

### 1.3.1 LogisticsCenter#completion 填充信息。

`LogisticsCenter`中的`completion`会去`Warehouse`中读取信息，如果之前没有加载过该分组下的任何组件，那么会先加载分组，然后再将该分组下的所有信息都加载进入内存当中，也就是我们之前提到的 **分组加载**。



```java
public class LogisticsCenter {
    //填充信息。
    public synchronized static void completion(Postcard postcard) {
        if (null == postcard) {
            throw new NoRouteFoundException(TAG + "No postcard!");
        }
        //获取分组信息。
        RouteMeta routeMeta = Warehouse.routes.get(postcard.getPath());
        //如果没有加载过该分组下的任何组件，那么先加载该分组。
        if (null == routeMeta) {    
            Class<? extends IRouteGroup> groupMeta = Warehouse.groupsIndex.get(postcard.getGroup()); 
            if (null == groupMeta) {
                throw new NoRouteFoundException(TAG + "There is no route match the path [" + postcard.getPath() + "], in group [" + postcard.getGroup() + "]");
            } else {
                try {
                    if (ARouter.debuggable()) {
                        logger.debug(TAG, String.format(Locale.getDefault(), "The group [%s] starts loading, trigger by [%s]", postcard.getGroup(), postcard.getPath()));
                    }

                    IRouteGroup iGroupInstance = groupMeta.getConstructor().newInstance();
                    iGroupInstance.loadInto(Warehouse.routes);
                    Warehouse.groupsIndex.remove(postcard.getGroup());

                    if (ARouter.debuggable()) {
                        logger.debug(TAG, String.format(Locale.getDefault(), "The group [%s] has already been loaded, trigger by [%s]", postcard.getGroup(), postcard.getPath()));
                    }
                } catch (Exception e) {
                    throw new HandlerException(TAG + "Fatal exception when loading group meta. [" + e.getMessage() + "]");
                }
                //分组加载完后，再加载具体的信息。
                completion(postcard);
            }
        } else {
            //通过 RouteMeta 中的信息进行填充。
            postcard.setDestination(routeMeta.getDestination());
            postcard.setType(routeMeta.getType());
            postcard.setPriority(routeMeta.getPriority());
            postcard.setExtra(routeMeta.getExtra());

            Uri rawUri = postcard.getUri();
            if (null != rawUri) {   // Try to set params into bundle.
                Map<String, String> resultMap = TextUtils.splitQueryParameters(rawUri);
                Map<String, Integer> paramsType = routeMeta.getParamsType();

                if (MapUtils.isNotEmpty(paramsType)) {
                    // Set value by its type, just for params which annotation by @Param
                    for (Map.Entry<String, Integer> params : paramsType.entrySet()) {
                        setValue(postcard,
                                params.getValue(),
                                params.getKey(),
                                resultMap.get(params.getKey()));
                    }

                    // Save params name which need auto inject.
                    postcard.getExtras().putStringArray(ARouter.AUTO_INJECT, paramsType.keySet().toArray(new String[]{}));
                }

                // Save raw uri
                postcard.withString(ARouter.RAW_URI, rawUri.toString());
            }

            switch (routeMeta.getType()) {
                case PROVIDER:  // if the route is provider, should find its instance
                    // Its provider, so it must implement IProvider
                    Class<? extends IProvider> providerMeta = (Class<? extends IProvider>) routeMeta.getDestination();
                    IProvider instance = Warehouse.providers.get(providerMeta);
                    if (null == instance) { // There's no instance of this provider
                        IProvider provider;
                        try {
                            provider = providerMeta.getConstructor().newInstance();
                            provider.init(mContext);
                            Warehouse.providers.put(providerMeta, provider);
                            instance = provider;
                        } catch (Exception e) {
                            throw new HandlerException("Init provider failed! " + e.getMessage());
                        }
                    }
                    postcard.setProvider(instance);
                    postcard.greenChannel();    // Provider should skip all of interceptors
                    break;
                case FRAGMENT:
                    postcard.greenChannel();    // Fragment needn't interceptors
                default:
                    break;
            }
        }
    }
}
```

### 1.3.2 _navigation 处理跳转或者返回服务声明

`_navigation`则处理最终的跳转或者返回逻辑：



```java
public class LogisticsCenter {

    private Object _navigation(final Context context, final Postcard postcard, final int requestCode, final NavigationCallback callback) {
        //保证 Context 不为空。
        final Context currentContext = null == context ? mContext : context;

        switch (postcard.getType()) {
            case ACTIVITY:
                //创建 intent。
                final Intent intent = new Intent(currentContext, postcard.getDestination());
                intent.putExtras(postcard.getExtras());

                //设置 intent 的 flag，如果没有传入 Context，那么使用 ApplicationContext。
                int flags = postcard.getFlags();
                if (-1 != flags) {
                    intent.setFlags(flags);
                } else if (!(currentContext instanceof Activity)) {    // Non activity, need less one flag.
                    intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                }

                //在主线程中启动 Activity。
                new Handler(Looper.getMainLooper()).post(new Runnable() {
                    @Override
                    public void run() {
                        //带返回值的方式。
                        if (requestCode > 0) {
                            ActivityCompat.startActivityForResult((Activity) currentContext, intent, requestCode, postcard.getOptionsBundle());
                        //普通方式。
                        } else {
                            ActivityCompat.startActivity(currentContext, intent, postcard.getOptionsBundle());
                        }
                        //设置跳转动画。
                        if ((-1 != postcard.getEnterAnim() && -1 != postcard.getExitAnim()) && currentContext instanceof Activity) {    // Old version.
                            ((Activity) currentContext).overridePendingTransition(postcard.getEnterAnim(), postcard.getExitAnim());
                        }

                        if (null != callback) { // Navigation over.
                            callback.onArrival(postcard);
                        }
                    }
                });

                break;
            case PROVIDER:
                //服务。
                return postcard.getProvider();
            case BOARDCAST:
            case CONTENT_PROVIDER:
            case FRAGMENT:
                //Fragment 创建实例，并设置参数。
                Class fragmentMeta = postcard.getDestination();
                try {
                    Object instance = fragmentMeta.getConstructor().newInstance();
                    if (instance instanceof Fragment) {
                        ((Fragment) instance).setArguments(postcard.getExtras());
                    } else if (instance instanceof android.support.v4.app.Fragment) {
                        ((android.support.v4.app.Fragment) instance).setArguments(postcard.getExtras());
                    }
                    //返回实例。
                    return instance;
                } catch (Exception ex) {
                    logger.error(Consts.TAG, "Fetch fragment instance error, " + TextUtils.formatStackTrace(ex.getStackTrace()));
                }
            case METHOD:
            case SERVICE:
            default:
                return null;
        }

        return null;
    }
}
```

## 1.3 拦截器处理流程



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymki7sd6j307c0jsglu.jpg)

拦截器处理流程


拦截器的处理是在`_Arouter#navigation`执行的，为了避免拦截器中耗时的操作占用主线程的时间导致`ANR`，因此拦截器的处理是在子线程当中执行的，其具体的实现逻辑为`InterceptorServiceImpl#doInterceptions`，里面会遍历`WareHouse`中的所有拦截器，调用它的`process`方法，如果进行了拦截，那么会调用`onContinue`，否则会调用`onInterruput`方法。





```java
@Route(path = "/arouter/service/interceptor")
public class InterceptorServiceImpl implements InterceptorService {

    @Override
    public void doInterceptions(final Postcard postcard, final InterceptorCallback callback) {
        if (null != Warehouse.interceptors && Warehouse.interceptors.size() > 0) {
            //检查初始化是否完成。
            checkInterceptorsInitStatus();
            if (!interceptorHasInit) {
                callback.onInterrupt(new HandlerException("Interceptors initialization takes too much time."));
                return;
            }
            //在子线程中处理拦截器的逻辑。
            LogisticsCenter.executor.execute(new Runnable() {
                @Override
                public void run() {
                    CancelableCountDownLatch interceptorCounter = new CancelableCountDownLatch(Warehouse.interceptors.size());
                    try {
                        //递归调用的过程。
                        _excute(0, interceptorCounter, postcard);
                        //阻塞等待超时时间。
                        interceptorCounter.await(postcard.getTimeout(), TimeUnit.SECONDS);
                        //如果 count 大于 0，说明没有执行完所有的流程。
                        if (interceptorCounter.getCount() > 0) {    // Cancel the navigation this time, if it hasn't return anythings.
                            callback.onInterrupt(new HandlerException("The interceptor processing timed out."));
                        //拦截器的实现，也可以通过 Tag 来设置异常信息。
                        } else if (null != postcard.getTag()) {    // Maybe some exception in the tag.
                            callback.onInterrupt(new HandlerException(postcard.getTag().toString()));
                        //没有任何拦截，那么最终会调用 _Arouter#_navigation 方法，去执行跳转的逻辑。
                        } else {
                            callback.onContinue(postcard);
                        }
                    } catch (Exception e) {
                        callback.onInterrupt(e);
                    }
                }
            });
        } else {
            //如果没有拦截器，那么直接回调成功。
            callback.onContinue(postcard);
        }
    }

    private static void _excute(final int index, final CancelableCountDownLatch counter, final Postcard postcard) {
        if (index < Warehouse.interceptors.size()) {
            IInterceptor iInterceptor = Warehouse.interceptors.get(index);
            iInterceptor.process(postcard, new InterceptorCallback() {

                @Override
                public void onContinue(Postcard postcard) {
                    //每处理完一次就减1。
                    counter.countDown();
                    //递归调用下一个拦截器。
                    _excute(index + 1, counter, postcard);  
                }

                @Override
                public void onInterrupt(Throwable exception) {
                    //设置异常的 Tag。
                    postcard.setTag(null == exception ? new HandlerException("No message.") : exception.getMessage());
                    //如果拦截了，那么就通过 CancelableCountDownLatch 取消 await 的等待。
                    counter.cancel();
                }
            });
        }
    }

}
```

## 1.4 依赖注入处理流程

整个依赖注入的流程如下图所示：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmymkgg9sqj307c0eg0sq.jpg)

依赖注入处理流程


依赖注入是通过`AutowiredServiceImpl`来实现的，这里会调用之前在编译期创建的`JavaFile`文件，





```java
final class _ARouter { 
   
    //获取 AutowiredService 服务。
    static void inject(Object thiz) {
        AutowiredService autowiredService = ((AutowiredService) ARouter.getInstance().build("/arouter/service/autowired").navigation());
        if (null != autowiredService) {
            //调用 autowire 方法。
            autowiredService.autowire(thiz);
        }
    }
}
```

`autowire`的实现如下：



```java
@Route(path = "/arouter/service/autowired")
public class AutowiredServiceImpl implements AutowiredService {
    //直接注入过的会缓存起来。
    private LruCache<String, ISyringe> classCache;
    //黑名单，直接注入异常的会被加入黑名单。
    private List<String> blackList;

    @Override
    public void init(Context context) {
        classCache = new LruCache<>(66);
        blackList = new ArrayList<>();
    }

    @Override
    public void autowire(Object instance) {
        String className = instance.getClass().getName();
        try {
            //如果之前发生过异常，那么会被加入到黑名单当中。
            if (!blackList.contains(className)) {
                ISyringe autowiredHelper = classCache.get(className);
                if (null == autowiredHelper) {  // No cache.
                    //autowiredHelper 就是在编译期动态创建的 JavaFile 文件。
                    autowiredHelper = (ISyringe) Class.forName(instance.getClass().getName() + SUFFIX_AUTOWIRED).getConstructor().newInstance();
                }
                //并调动它的 inject 方法。
                autowiredHelper.inject(instance);
                classCache.put(className, autowiredHelper);
            }
        } catch (Exception ex) {
            //加入黑名单。
            blackList.add(className);    // This instance need not autowired.
        }
    }
}
```

最后，我们来看一下之前在编译期创建的文件，其`inject`方法就是对成员变量赋值：



```java
/**
 * DO NOT EDIT THIS FILE!!! IT WAS GENERATED BY AROUTER. */
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

------

## 更多文章，欢迎访问我的 **Android** 知识梳理系列：

- **Android** 知识梳理目录：[http://www.jianshu.com/p/fd82d18994ce](https://www.jianshu.com/p/fd82d18994ce)
- **Android** 面试文档分享：[http://www.jianshu.com/p/8456fe6b27c4](https://www.jianshu.com/p/8456fe6b27c4)



作者：泽毛
链接：https://www.jianshu.com/p/a662c0b8edd2
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。