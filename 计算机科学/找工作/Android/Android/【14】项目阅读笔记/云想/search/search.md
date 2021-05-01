[TOC]

### 一、模块结构

> 总共两个部分，一个是搜索页（历史以及热门搜索），一个是搜索结果页

```java
.
├── SearchResultViewModel.java				//搜索结果ViewModel
├── SearchViewModel.java					//搜索ViewModel
├── data
│   ├── ResultSortManager.java				//数据排行管理类
│   ├── SearchHistoryRepository.java		//搜索历史数据仓库，只要是通过sp获取数据
│   └── SearchResultRepository.java			//搜索结果数据仓库
├── model
│   ├── ResultTab.java						//Tablayout的Item
│   └── SortType.java						//枚举类，四种排序模式，一个isDown属性
└── view
    ├── SearchActivity.java					//搜索页
    ├── SearchResultActivity.java			//搜索结果页Tablayout+Viewpager+DrawerLayout
    ├── SearchResultFragment.java			//Fragment
    └── adapter
        ├── PopularSearchAdapter.java		//大家都在搜也是RecyclerView
        ├── ResultListAdapter.java			//搜索结果页RecyclerViewAdapter
        └── ResultPagerAdapter.java			//结果页ViewpagerAdapter
```



### 二、ViewModel

#### 1、SearchResultViewModel

- ViewModel是视图的数据管理类，结果页只需要管理一个List，展示返回的珠宝列表



#### 2、SearchViewModel

- ViewModel是视图的数据管理类，搜索页需要存储mHistoryList、mPopularList以及一个hasMore用来响应是否还有我的搜索历史