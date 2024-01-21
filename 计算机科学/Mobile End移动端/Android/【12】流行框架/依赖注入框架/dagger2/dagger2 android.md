- https://blog.csdn.net/mq2553299/article/details/77485800

- https://blog.csdn.net/mq2553299/article/details/77725912

- [https://jsonchao.github.io/2019/01/20/Android%E4%B8%BB%E6%B5%81%E4%B8%89%E6%96%B9%E5%BA%93%E6%BA%90%E7%A0%81%E5%88%86%E6%9E%90%EF%BC%88%E5%85%AB%E3%80%81%E6%B7%B1%E5%85%A5%E7%90%86%E8%A7%A3Dagger2%E6%BA%90%E7%A0%81%EF%BC%89/](https://jsonchao.github.io/2019/01/20/Android主流三方库源码分析（八、深入理解Dagger2源码）/)

- <https://www.jianshu.com/p/e45d2c5c68bd>



- 
- <https://jsonchao.github.io/2017/12/31/2017%20%E5%B9%B4%E6%9C%AB%E6%80%BB%E7%BB%93/>
- <https://blog.csdn.net/mq2553299/article/details/77485800>
- <https://blog.csdn.net/mq2553299/article/details/73136396>
- <https://jsonchao.github.io/2019/01/20/Android%E4%B8%BB%E6%B5%81%E4%B8%89%E6%96%B9%E5%BA%93%E6%BA%90%E7%A0%81%E5%88%86%E6%9E%90%EF%BC%88%E5%85%AB%E3%80%81%E6%B7%B1%E5%85%A5%E7%90%86%E8%A7%A3Dagger2%E6%BA%90%E7%A0%81%EF%BC%89/>
- <https://blog.csdn.net/mq2553299/article/details/77725912>
- 

几个重要的类

```java
//
DispatchingAndroidInjector<T> implements AndroidInjector<T>
//
AndroidInjector<T>
//
@Beta
@Module
public abstract class AndroidInjectionModule {
  @Multibinds
  abstract Map<Class<?>, AndroidInjector.Factory<?>> allInjectorFactories();
//
@Module(includes = AndroidInjectionModule.class)
public abstract class AndroidSupportInjectionModule {
```

