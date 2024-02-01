[TOC]

### 模块结构

```java
├── GardenFragment.kt
├── GardenPlantingAdapter.kt
└── viewmodel
    ├── GardenPlantingListViewModel.kt   //ViewModel
    └── GardenPlantingListViewModelFactory.kt  //Factory
    
InjectorUtils.kt       //获取Factory

.
├── AppDatabase.kt
├── Converters.kt
├── GardenPlanting.kt
├── GardenPlantingDao.kt
├── GardenPlantingRepository.kt   //数据仓库
├── Plant.kt
├── PlantAndGardenPlantings.kt
├── PlantDao.kt
└── PlantRepository.kt
```



### 数据流向

- **Factory**只是工厂负责加工，构造函数传进来**Repository**，**new** 一个**ViewModel(repository)**返回去
- 具体的**ViewModel**中再通过**Repository**获取**LiveData**




### XML文件

- databinding
- androidx
- [再熟悉下DataBinding的基础使用](https://www.jianshu.com/p/87d4b9f30960)

```java
<layout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:app="http://schemas.android.com/apk/res-auto"
    xmlns:tools="http://schemas.android.com/tools">
    <data>
        <variable
                name="hasPlantings"
                type="Boolean" />		##1
    </data>

    <FrameLayout
            android:layout_width="match_parent"
            android:layout_height="match_parent">

        <androidx.recyclerview.widget.RecyclerView
                android:id="@+id/garden_list"
                //......
                app:isGone="@{!hasPlantings}"    ##2
                app:layoutManager="androidx.recyclerview.widget.LinearLayoutManager"
                tools:listitem="@layout/list_item_garden_planting"/>

        <TextView
                android:id="@+id/empty_garden"
        		//......
                app:isGone="@{hasPlantings}"/> ##2

    </FrameLayout>
</layout>
```

