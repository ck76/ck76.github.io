[TOC]

### 一、Android LiveData Transformations

在使用LiveData时，有时候我们想改变LiveData的值在LiveData通知其Observers之前，或者在有些时候我们的数据源Repository会返回不同的LiveData实例，我们需要一种数据转换的功能帮助我们去转换不同数据层的数据，而Transformations就是这样一个工具。

- Transformations主要有两个方法

  - LiveData map (LiveData source, Function<X, Y> func)

    提供一个方法去在LiveData.setValue之前对value进行相应的转换和处理

  - LiveData switchMap (LiveData trigger, Function<X, LiveData> func)

    与map类似，但是是对LiveData进行转换，而不是对应的value，将两个LiveData桥接起来，其scope也会相应传递



### 二、Translations.map

我们有两个LiveData在一个viewModel中，其中一个`userLiveData`负责持有user数据，另一个负责与UI进行绑定。每次我们`setUser`的时候都会触发`Transformations.map()`将我们的user数据转换成我们需要在UI上展示的文字string

```kotlin
private val LiveData userLiveData = ...;
val LiveData userName = Transformations.map(userLiveData, user -> { 
    return user.firstName + " " + user.lastName 
})

fun setUser(user:User) {
    userLiveData.value = user
}
```



### 三、Translations.switchMap

我们依旧创建两个LiveData在viewModel中，userIdLiveData作为trigger，每次setUserId触发时就会从repository获取对应新的id的user内容，通过switchMap传递给userLiveData

```kotlin
MutableLiveData userIdLiveData = ...;
LiveData userLiveData = Transformations.switchMap(userIdLiveData, id ->
     repository.getUserById(id));

void setUserId(String userId) {
  this.userIdLiveData.setValue(userId);
}
```

如果我们不在这里使用switchMap的话，由于我们的repository每次返回的都是一个新的LiveData instance，会导致view observe多个LiveData的情形出现，而switchMap会保证view observe的LiveData的是同一个

Google官方也提醒我们在使用LiveData的时候要避免对外暴露一个var类型的LiveData或者MutableLiveData，以避免重复observe



### 四、Transformations 源码

Transformations源码比较简单，如下：

```java
@MainThread
public static <X, Y> LiveData<Y> map(
        @NonNull LiveData<X> source,
        @NonNull final Function<X, Y> mapFunction) {
    final MediatorLiveData<Y> result = new MediatorLiveData<>();
    result.addSource(source, new Observer<X>() {
        @Override
        public void onChanged(@Nullable X x) {
            result.setValue(mapFunction.apply(x));
        }
    });
    return result;
}
```

可以看到主要就是在`setValue`前执行了`mapFunction`并且将新的值赋值给LiveData

```java
@MainThread
public static <X, Y> LiveData<Y> switchMap(
        @NonNull LiveData<X> source,
        @NonNull final Function<X, LiveData<Y>> switchMapFunction) {
    final MediatorLiveData<Y> result = new MediatorLiveData<>();
    result.addSource(source, new Observer<X>() {
        LiveData<Y> mSource;

        @Override
        public void onChanged(@Nullable X x) {
            LiveData<Y> newLiveData = switchMapFunction.apply(x);
            if (mSource == newLiveData) {
                return;
            }
            if (mSource != null) {
                result.removeSource(mSource);
            }
            mSource = newLiveData;
            if (mSource != null) {
                result.addSource(mSource, new Observer<Y>() {
                    @Override
                    public void onChanged(@Nullable Y y) {
                        result.setValue(y);
                    }
                });
            }
        }
    });
    return result;
}
```

`switchMap`主要会进行重新绑定的操作，如果从`switchMapFunction`中获得的LiveData发生了变化，则会重新进行observe，从而避免上文提到的重复observe的问题



### 五、MediatorLiveData

我们从源码中可以看到两个方法都使用了MediatorLiveData\<T>这个类型。这个类和MutableLiveData\<T>一样都继承自LiveData\<T>. 主要是帮助我们解决这样一种case：我们有多个LiveData实例，这些LiveData的value发生变更时都需要通知某一个相同的LiveData。

```java
LiveData liveData1 = ...;
LiveData liveData2 = ...;

MediatorLiveData liveDataMerger = new MediatorLiveData<>();
liveDataMerger.addSource(liveData1, value -> liveDataMerger.setValue(value));
liveDataMerger.addSource(liveData2, value -> liveDataMerger.setValue(value));
```

我们还可以在监听value变化的过程中根据需要取消对某个LiveData的observe

```java
liveDataMerger.addSource(liveData1, new Observer() {

    //...
    
   @Override 
   public void onChanged(@Nullable Integer s) {
        
        val liveData1IsUseless = //... some logic
        
        liveDataMerger.setValue(s);
        
        if (liveData1IsUseless) {
          liveDataMerger.removeSource(liveData1);
        }
    }
 });
```





- https://blog.csdn.net/kou_gyouu/article/details/85502548