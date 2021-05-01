---
title: RxGreenDao
date: 2018-07-29 18:44:31
categories: "Android流行框架"
tags:
- "GreenDao"
---

https://blog.csdn.net/robothost/article/details/70209086

GreenDao 3.X已集成RxJava，其中，RxDao<T，K> 和RxQuery\<T>便是GreenDao 3.X中RxJava的核心操作类。其最大的特点就是在增删改查等基本操作时返回Observable,用于RxJava的流式逻辑写法，其最大的优点便于流式开发，提高代码的可读性。 

> 注意，目前`GreenDao-3.2.2`，仅支持`RxJava1`依赖，如依赖`RxJava2`将编译错误。

<!--more-->

## RxDao 

### 获取RxDao对象和RxQuery对象

```java
    RxDao<T，K> mRxDao = xxDao.rx(); // 返回一个默认subscribeOn在IO线程的RxDao
                                     // T是实体类，K主键类型
    RxDao<T，K> mRxDao = xxDao.rxPlain();// 返回一个未设置默认订阅的RxDao
```

## 常用 API

```java
    rx.Observable<java.lang.Long>     count()：返回一个含有数据库数据个数的Observable
 
    rx.Observable<java.lang.Void>     delete(T entity)：从数据库中删除数据，并返回一个空的Observable
    rx.Observable<java.lang.Void>     deleteAll()：从数据库中删除数据，并返回一个空的Observable
    rx.Observable<java.lang.Void>     deleteByKey(K key)：将数据库中主键为key的数据删除，，并返回一个空的Observable
    rx.Observable<java.lang.Void>     deleteByKeyInTx(java.lang.Iterable<K> keys)：使用事务操作，将数据库中，删除key集合中每个key所对应的实体，并返回一个空的Observable
    rx.Observable<java.lang.Void>     deleteByKeyInTx(K... keys)：使用事务操作，将数据库中，删除key[ ]中每个key所对应的实体，并返回一个空的Observable
    rx.Observable<java.lang.Void>     deleteInTx(java.lang.Iterable<T> entities)：使用事务操作，将数据库中，删除实体集合中每个实体所对应的实体，并返回一个空的Observable
    rx.Observable<java.lang.Void>     deleteInTx(T... entities)：使用事务操作，将数据库中，删除实体集合中每个实体[ ]所对应的实体，并返回一个空的Observable
 
    rx.Scheduler     getScheduler()：返回当前调度器
    rx.Observable<T>                         insert(T entity)：将实体插入数据库，并返回包含此实体的Observable
    rx.Observable<java.lang.Iterable<T>>     insertInTx(java.lang.Iterable<T> entities)：使用事务操作将实体插集合入数据库，并返回包含此实体集合的Observable
    rx.Observable<java.lang.Object[]>        insertInTx(T... entities)：使用事务操作将实体[ ]插入数据库，并返回包含此实体数组的Observable
    rx.Observable<T>                         insertOrReplace(T entity)：将实体插入数据库，若此实体的Key已存在，将原来的数据覆盖，并返回包含此实体的Observable
    rx.Observable<java.lang.Iterable<T>>     insertOrReplaceInTx(java.lang.Iterable<T> entities)：将实体集合插入数据库，若某个实体的Key已存在，将原来的数据覆盖，
                                                                                                  并返回包含此实体集合的Observable
    rx.Observable<java.lang.Object[]>        insertOrReplaceInTx(T... entities)：将实体[ ]插入数据库，若某个实体的Key已存在，将原来的数据覆盖，
                                                                                 并返回包含此实体[ ]的Observable
                                                                                                                    
    rx.Observable<T>                             load(K key)：将主键为Key的实体加载至内存，并返回包含此实体的Observable
    rx.Observable<java.util.List<T>>     loadAll()：将所有实体加载至内存，并返回包含此实体集合的Observable
    rx.Observable<T>     refresh(T entity)：更新实体，并返回包含此实体的Observable
    rx.Observable<T>                         save(T entity)：：将实体插入数据库，若此实体的Key已存在，将原来的数据覆盖，并返回包含此实体的Observable
    rx.Observable<java.lang.Iterable<T>>     saveInTx(java.lang.Iterable<T> entities)：将实体集合插入数据库，若某个实体的Key已存在，将原来的数据覆盖，
                                                                                       并返回包含此实体集合的Observable
    rx.Observable<java.lang.Object[]>        saveInTx(T... entities)：将实体[ ]插入数据库，若某个实体的Key已存在，将原来的数据覆盖，
                                                                                                                    并返回包含此实体[ ]的Observable
                                                                                                                    
    rx.Observable<T>                         update(T entity)：将实体更新，并返回包含此实体的Observable
    rx.Observable<java.lang.Iterable<T>>     updateInTx(java.lang.Iterable<T> entities)：使用事务操作将实体集合更新，并返回包含此实体集合的Observable
    rx.Observable<java.lang.Object[]>        updateInTx(T... entities)：使用事务操作将实体[ ]更新，并返回包含此实体数组的Observable</span>
```



## RxQuery

### 获取RxQuery对象

```java
    RxQuery<T> mRxQuery = xxQueryBuilder.rx();
    RxQuery<T> mRxQuery = xxQueryBuilder.rxPlain();
```

### 常用API

```java
        rx.Observable<T>                     unique()：唯一查询，返回含有该实体的Observable
        rx.Observable<java.util.List<T>>     list()：获取实体集合，，返回含有该实体集合的Observable
```



## 一个工具类

```java
public class StudentDaoOpeRx {
 
    /**
     * @desc 添加数据至数据库
     **/
    public static Observable<Student> insertData(Context context, Student stu) {
 
        return DbManager.getDaoSession(context).getStudentDao().rx().insert(stu);
    }
 
    /**
     * @desc 将数据实体通过事务添加至数据库
     **/
    public static Observable<Iterable<Student>> insertData(Context context, List<Student> list) {
        if (null == list || list.size() <= 0) {
            return Observable.error(new Throwable("null"));
        }
        return DbManager.getDaoSession(context).getStudentDao().rx().insertInTx(list);
    }
 
    /**
     * @desc 添加数据至数据库，如果存在，将原来的数据覆盖
     **/
    public static Observable<Student> saveData(Context context, Student student) {
        return DbManager.getDaoSession(context).getStudentDao().rx().save(student);
    }
 
 
    /**
     * @desc 查询所有数据
     **/
    public static Observable<List<Student>> queryAll(Context context) {
        RxQuery<Student> rxQuery = DbManager.getDaoSession(context).getStudentDao().queryBuilder().rx();
 
        return rxQuery.list();
    }
 
    /**
     * @desc 删除数据
     **/
    public static Observable<Void> deleteData(Context context, Student student) {
        return DbManager.getDaoSession(context).getStudentDao().rx().delete(student);
    }
 
    /**
     * @desc 删除全部数据
     **/
    public static Observable<Void> deleteAll(Context context) {
        return DbManager.getDaoSession(context).getStudentDao().rx().deleteAll();
    }
 
    /**
     * @desc 更新数据
     **/
    public static Observable<Student> updateData(Context context, Student student) {
        return DbManager.getDaoSession(context).getStudentDao().rx().update(student);
    }
}
```



**RxQuery：**

- rx.Observable<java.util.List\<T>>  list()：获取实体集合，返回含有该实体集合的Observable
- rx.Observable\<T>  unique()：返回含有该实体的Observable
- rx.Observable\<T>  oneByOne()：产生一个一个的实体

**RxDao**

- rx.Observable<java.lang.Long> **count()**： 返回一个含有数据库数据个数的Observable。

**删除：**

- rx.Observable<java.lang.Void> **delete(T entity)**：从数据库中删除数据，并返回一个空的Observable
- rx.Observable<java.lang.Void> **deleteAll()**： 删除所有数据,并返回一个空的Observable
- rx.Observable<java.lang.Void> **deleteByKey(K key)**：将数据库中主键为key的数据删除，，并返回一个空的Observable
- rx.Observable<java.lang.Void> deleteByKeyInTx(java.lang.Iterable<K> keys)：使用事务操作，将数据库中，删除key集合中每个key所对应的实体，并返回一个空的Observable
- rx.Observable<java.lang.Void> **deleteByKeyInTx(K... keys)**： 使用事务操作，将数据库中，删除key[]中每个key所对应的实体，并返回一个空的Observable
- rx.Observable<java.lang.Void> **deleteInTx(java.lang.Iterable<T> entities)**：使用事务操作，将数据库中，删除实体集合中每个实体所对应的实体，并返回一个空的Observable
- rx.Observable<java.lang.Void> **deleteInTx(T... entities)**:使用事务操作，将数据库中，删除实体集合中每个实体[ ]所对应的实体，并返回一个空的Observable

**插入：**

- rx.Observable\<T>  **insert(T entity)** : 插入一个实体.
- rx.Observable<java.lang.Iterable\<T>>  **insertInTx(java.lang.Iterable\<T> entities)**: 插入一个list数组。
- rx.Observable<java.lang.Object[]> **insertInTx(T... entities)** 插入实体数组
- rx.Observable\<T>  **insertOrReplace(T entity)**：插入数据，如果已存在则更新。
- rx.Observable<java.lang.Iterable\<T>>  **insertOrReplaceInTx(java.lang.Iterable\<T> entities)**：插入替换list数组
- rx.Observable<java.lang.Object[]> **insertOrReplaceInTx(T... entities)**：插入替换数组

**查询,&更新：**

- rx.Observable\<T>  **load(K key)**：获取主键为key的实体。
- rx.Observable<java.util.List\<T>>  **loadAll()**：获取全部数据。
- rx.Observable\<T>  **refresh(T entity)**：更新实体。

**保存：**

- rx.Observable\<T>  **save(T entity)**：插入数据，如果已存在则更新。
- rx.Observable<java.lang.Iterable\<T>>  **saveInTx(java.lang.Iterable\<T> entities)**：插入替换list数组
- rx.Observable<java.lang.Object[]> **saveInTx(T... entities)**：插入替换数组

**更新：**

- rx.Observable\<T>  **update(T entity)**： 跟新
- rx.Observable<java.lang.Iterable\<T>>  **updateInTx(java.lang.Iterable\<T> entities)**:批量更新list
- rx.Observable<java.lang.Object[]> **updateInTx(T... entities)**:批量跟新数组。

