[Github](https://github.com/greenrobot/greenDAO)

greenDAO is a light & fast ORM for Android that maps objects to SQLite databases. Being highly optimized for Android, greenDAO offers great performance and consumes minimal memory.  --官网介绍

中文翻译内大概是这样子：GRANDOAO是一个轻量级的Android ORM，它将对象映射到SQLite数据库。GRANDOAO对Android高度优化，提供了良好的性能和消耗最少的内存。

<!--more-->

## GreenDao3.2的简介

ORM（Object Relation Mapping对象关系映射），其表现形式就是通过GreenDao将数据库和Bean对象关联起来，其表现形式如下图 

![greenDAO-orm-320](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/4xE29CKfr82iNEx1IgNVXFmtu3NhAdV4RyX.vmTGLDc!/r/dC4BAAAAAAAA)

GreenDao还有以下优点

- 存取速度快
- 支持数据库加密
- 轻量级
- 激活实体
- 支持缓存
- 代码自动生成



## 配置

- 项目根目录的build.gradle和模块目录下的build.gradle一顿乱操作

```java
// In your root build.gradle file:      //项目
buildscript {
    repositories {
        jcenter()
        mavenCentral() // add repository
    }
    dependencies {
        classpath 'com.android.tools.build:gradle:3.1.1'
        classpath 'org.greenrobot:greendao-gradle-plugin:3.2.2' // add plugin
    }
}
 
// In your app projects build.gradle file:		//模块
apply plugin: 'com.android.application'
apply plugin: 'org.greenrobot.greendao' // apply plugin
 
dependencies {
    implementation 'org.greenrobot:greendao:3.2.2' // add library
}

//android闭包中
 //数据库
    greendao {
        schemaVersion 3   //版本号
         /**
         * 输出dao的数据库操作实体类文件夹（相对路径 包名+自定义路径名称，
         包将创建于包名的直接路径下）
         */
        daoPackage "cn.ck.balabala.database.dao"  
         /**
         * greenDao实体类包文件夹
         */
        targetGenDir "src/main/java"
        generateTests false
        targetGenDirTests "src/androidTest/java"
    }
```

请注意，这会将greenDAO Gradle plugin 插件绑定到构建过程。当您构建项目时，它生成诸如**DaoMaster, DaoSession and DAOs** 之类的类。



## 注解

1. 实体@Entity注解

- schema：告知GreenDao当前实体属于哪个schema
- active：标记一个实体处于活动状态，活动实体有更新、删除和刷新方法
- nameInDb：在数据中使用的别名，默认使用的是实体的类名
- indexes：定义索引，可以跨越多个列
- createInDb：标记创建数据库表

2. 基础属性注解

- @Id :主键 Long型，可以通过@Id(autoincrement = true)设置自增长
- @Property：设置一个非默认关系映射所对应的列名，默认是的使用字段名举例：@Property (nameInDb="name")
- @NotNul：设置数据库表当前列不能为空
- @Transient：添加次标记之后不会生成数据库表的列

3. 索引注解

- @Index：使用@Index作为一个属性来创建一个索引，通过name设置索引别名，也可以通过unique给索引添加约束
- @Unique：向数据库列添加了一个唯一的约束

4. 关系注解

- @ToOne：定义与另一个实体（一个实体对象）的关系
- @ToMany：定义与多个实体对象的关系



## 创建Bean对象

GreenDao需要创建Bean对象之后，该Bean对象就是表名，而它的属性值就是字段名，其实现是通过注解的方式来实现的，下面是ck的Bean对象（每个Bean对象对应一张表） 

这里对Bean对象的注释进行解释

1. @Entity：告诉GreenDao该对象为实体，只有被@Entity注释的Bean类才能被dao类操作
2. @Id：对象的Id，使用Long类型作为EntityId，否则会报错。(autoincrement = true)表示主键会自增，如果false就会使用旧值
3. @Property：可以自定义字段名，注意外键不能使用该属性
4. @NotNull：属性不能为空
5. @Transient：使用该注释的属性不会被存入数据库的字段中
6. @Unique：该属性值必须在数据库中是唯一值
7. @Unique 向数据库列添加了一个唯一的约束
8. @ToOne 定义与另一个实体（一个实体对象）的关系
9. @Generated：编译后**自动生成**的构造函数、方法等的注释，提示构造函数、方法等不能被修改

- ck.java

```java
//构建之前
@Entity
public class ck {
    @Id(autoincrement = true)
    @Unique
    private Long id;
    private String name;
}

//构建之后
@Entity
public class ck {
    @Id(autoincrement = true)
    @Unique
    private Long id;
    private String name;
    @Generated(hash = 674740680)
    public ck(Long id, String name) {
        this.id = id;
        this.name = name;
    }
    @Generated(hash = 235706220)
    public ck() {
    }
    public Long getId() {
        return this.id;
    }
    public void setId(Long id) {
        this.id = id;
    }
    public String getName() {
        return this.name;
    }
    public void setName(String name) {
        this.name = name;
    }
}
```

- ckDao.java

。。。。。文件太长了，不贴了。





## 创建数据库

GreenDao已经将我们的数据库创建缩成几句话，代码会自动将Bean对象创建成表，不再是传统的手写SQL语句。这里的数据库创建只需要在Application中执行一次即可，这里对几个类进行解释

- DevOpenHelper：创建SQLite数据库的SQLiteOpenHelper的具体实现
- DaoMaster：GreenDao的顶级对象，作为数据库对象、用于创建表和删除表
- DaoSession：管理所有的Dao对象，Dao对象中存在着增删改查等API

**DaoHelper.java**

```java
public class DaoHelper {
    private volatile static DaoHelper instance;
    private DaoMaster.OpenHelper helper;
    private SQLiteDatabase db;
    private DaoMaster daoMaster;
    private DaoSession daoSession;

    private DaoHelper(Context context) {
        //helper = new DaoMaster.DevOpenHelper(context, Constants.DBNAME);
        helper = new DaoUpgradeHelper(context, Constants.DBNAME);
        db = helper.getWritableDatabase();
        daoMaster = new DaoMaster(db);
        daoSession = daoMaster.newSession();
    }

    public static DaoHelper getDaoHelper(Context context) {
        if (instance == null) {
            synchronized (DaoHelper.class) {
                if (instance == null) {
                    instance = new DaoHelper(context);
                }
            }
            instance = new DaoHelper(context);
        }
        return instance;
    }

    public DaoSession getDaoSession() {
        return daoSession;
    }

    public SQLiteDatabase getDb() {
        return db;
    }
}
```

首先获取一个DevOpenHelper对象，这个类有点类似于我们使用的SqliteOpenHelper，我们主要在这个类中对数据库的版本进行管理。这样之后，我们对数据库基本的初始化操作就完成了，想要操作ck实体类，得先有一个ckDao，这个ckDao要怎么获取呢？如下：

```
ckDao = daoSession.getckDao();
```

**DaoUtils**

```java
public class DaoUtils {


    /**
     * 获取daoSession
     */
    public static DaoSession getDaoSession() {
        return DaoHelper.getDaoHelper(MApplication.application).getDaoSession();
    }
/////////////////////////////////////////////////////////自定义的。。。。
    /**
     * 获取配置
     */
    public static List<Configure> getConfigureList() {
        ConfigureDao configureDao = getDaoSession().getConfigureDao();
        return configureDao.queryBuilder().where(ConfigureDao.Properties.UserId.eq(getLoginUser())).list();
    }

    /**
     * 获取当前登录用户
     */
    public static String getLoginUser() {
        SharedPreferences preferences = MApplication.application.getSharedPreferences("login_user", Context.MODE_PRIVATE);
        return preferences.getString("userId", "*");
    }
}

```



## 增删改查

- AbstractDao

所有的自动生成的XXDao都是继承于AbstractDao，此类中基本上封装了所有的增删改操作，包括数据库的事务操作。常用的API如下：



```java
	void 	attachEntity(T entity)：
	
	long 	count()：获取数据库中数据的数量
	
	// 数据删除相关
	void 	delete(T entity)：从数据库中删除给定的实体
	void 	deleteAll() ：删除数据库中全部数据
	void 	deleteByKey(K key)：从数据库中删除给定Key所对应的实体
	void 	deleteByKeyInTx(java.lang.Iterable<K> keys)：使用事务操作删除数据库中给定的所有key所对应的实体
	void 	deleteByKeyInTx(K... keys)：使用事务操作删除数据库中给定的所有key所对应的实体
	void 	deleteInTx(java.lang.Iterable<T> entities)：使用事务操作删除数据库中给定实体集合中的实体
	void 	deleteInTx(T... entities)：使用事务操作删除数据库中给定的实体
	
	// 数据插入相关
	long 	insert(T entity)：将给定的实体插入数据库
	void 	insertInTx(java.lang.Iterable<T> entities)：使用事务操作，将给定的实体集合插入数据库
	void 	insertInTx(java.lang.Iterable<T> entities, boolean setPrimaryKey)：使用事务操作，将给定的实体集合插入数据库，
																													并设置是否设定主键
	void 	insertInTx(T... entities)：将给定的实体插入数据库
	long 	insertOrReplace(T entity)：将给定的实体插入数据库，若此实体类存在，则覆盖
	void 	insertOrReplaceInTx(java.lang.Iterable<T> entities)：使用事务操作，将给定的实体插入数据库，若此实体类存在，则覆盖
	void 	insertOrReplaceInTx(java.lang.Iterable<T> entities, boolean setPrimaryKey)：使用事务操作，将给定的实体插入数据库，若此实体类存在，则覆盖
																																并设置是否设定主键
	void 	insertOrReplaceInTx(T... entities)：使用事务操作，将给定的实体插入数据库，若此实体类存在，则覆盖
	long 	insertWithoutSettingPk(T entity)：将给定的实体插入数据库,但不设定主键
	
	// 新增数据插入相关API
	void 	save(T entity)：将给定的实体插入数据库，若此实体类存在，则更新
	void 	saveInTx(java.lang.Iterable<T> entities)：将给定的实体插入数据库，若此实体类存在，则更新
	void 	saveInTx(T... entities)：使用事务操作，将给定的实体插入数据库，若此实体类存在，则更新
	
	// 加载相关
	T 	load(K key)：加载给定主键的实体
	java.util.List<T> 	loadAll()：加载数据库中所有的实体
	protected java.util.List<T> 	loadAllAndCloseCursor(android.database.Cursor cursor) ：从cursor中读取、返回实体的列表，并关闭该cursor
	protected java.util.List<T> 	loadAllFromCursor(android.database.Cursor cursor)：从cursor中读取、返回实体的列表
	T 	loadByRowId(long rowId) ：加载某一行并返回该行的实体
	protected T 	loadUnique(android.database.Cursor cursor) ：从cursor中读取、返回唯一实体
	protected T 	loadUniqueAndCloseCursor(android.database.Cursor cursor) ：从cursor中读取、返回唯一实体，并关闭该cursor
	//更新数据
	void 	update(T entity) ：更新给定的实体
	protected void 	updateInsideSynchronized(T entity, DatabaseStatement stmt, boolean lock) 
	protected void 	updateInsideSynchronized(T entity, android.database.sqlite.SQLiteStatement stmt, boolean lock) 
	void 	updateInTx(java.lang.Iterable<T> entities) ：使用事务操作，更新给定的实体
	void 	updateInTx(T... entities)：使用事务操作，更新给定的实体

```

- QueryBuilder、Query

基本查询

GreenDao中，使用QueryBuilder自定义查询实体，而不是再写繁琐的SQL语句，避免了SQL语句的出错率。大家都知道写SQL语句时，非常容易出错，出错后又十分的难查。QueryBuilder真是帮忙解决了一个大麻烦。具体该如何使用呢？

```java
    List joes = userDao.queryBuilder()
                       // 查询的条件
                       .where(Properties.FirstName.eq("Joe"))
                       // 返回实体集合升序排列
                       .orderAsc(Properties.LastName)
                       .list();
    QueryBuilder qb = userDao.queryBuilder();
    // 查询的条件
    qb.where(Properties.FirstName.eq("Joe"),
    qb.or(Properties.YearOfBirth.gt(1970),
    qb.and(Properties.YearOfBirth.eq(1970), Properties.MonthOfBirth.ge(10))));
    List youngJoes = qb.list();    </span>
```

上面是官方给出的两个列子，不仅满足了查询语句的易写，同时使用了流式写法，提高了代码的可阅读性。

- Limit、Offset、Pagination

在实际开发过程中，大家肯定碰到这样的问题，当数据过多在一页显示不出来的时候，要么选择前面十条显示，要么分页显示，但是数据总是获取全部的。其实，刚接触GreenDao的时候，也是这么干，获取全部的实体集合，然后再根据实际情况截取。看了API以后，豁然开朗，大神们已经帮我们解决了这件事。此时不得不说，QueryBuilder
中的Limit（限制）、Offset（偏移），limit(int)和offset(int)协同设置，可以完美解决分页显示。

```
        limit(int)：限制查询返回结果的数目
        offset(int)：设置查询结果的偏移量，此查询需与limit(int)结合使用，而不能够脱离limit(int)单独使用
```

- Query

当执行多次查询时，实际是QueryBuilder多次调用Query类。如果执行多次相同的查询，应使用QueryBuilder的build()方法来创建Query,而不是直接使用Query类。如果查询返回的结果是唯一性的，可以使用操作符方法,如果不希望此唯一性不返回 null，此时可调用uniqOrThrow()方法。如果查询返回的结果是多个，可以使返回的结果是一个集合，有如下方法：

```java
      list()：所有实体加载至内存，结果通常是一个ArrayList
    listLazy()：实体在需要时，加载至内存，表中的第一个元素被第一次访问时会被缓存，下次访问时，使用缓存
    listLazyUncached()：任何对列表实体的访问懂事从数据库中加载
    listIterator()：以按需加载的方式来遍历结果，数据没有被缓存
```

 一旦使用QueryBuilder创建了一个query，那么这个Query对象就可以就可以被复用来执行查询显然这种方式逼重新创建一次Query效率要高。
具体来说：

```
        如果Query的参数没有变更，你只需要再次调用List/unuque方法即可
        如果参数发生了变化，那么就需要通过setParameter方法来处理每一个发生改变的参数
```

举例：

```java
    Query query = userDao.queryBuilder().where(Properties.FirstName.eq("Joe"), Properties.YearOfBirth.eq(1970)).build();
    List joesOf1970 = query.list();
```

现在复用该Query对象：

```java
    query.setParameter(0, "Maria");
    query.setParameter(1, 1977);
    List mariasOf1977 = query.list();
```

由此可见，Query在执行一次build之后会将查询结果进行缓存，方便下次继续使用。

## 

```java
public class LoveDao {

    /**
     * 添加数据，如果有重复则覆盖
     *
     * @param shop
     */
    public static void insertLove(Shop shop) {
        BaseApplication.getDaoInstant().getShopDao().insertOrReplace(shop);
    }

    /**
     * 删除数据
     *
     * @param id
     */
    public static void deleteLove(long id) {
        BaseApplication.getDaoInstant().getShopDao().deleteByKey(id);
    }

    /**
     * 更新数据
     *
     * @param shop
     */
    public static void updateLove(Shop shop) {
        BaseApplication.getDaoInstant().getShopDao().update(shop);
    }

    /**
     * 查询条件为Type=TYPE_LOVE的数据
     *
     * @return
     */
    public static List<Shop> queryLove() {
        return BaseApplication.getDaoInstant().getShopDao().queryBuilder().where(ShopDao.Properties.Type.eq(Shop.TYPE_LOVE)).list();
    }

    /**
     * 查询全部数据
     */
    public static List<Shop> queryAll() {
        return BaseApplication.getDaoInstant().getShopDao().loadAll();
    }

}
```

**增加单个数据** 

- getShopDao().insert(shop);
- getShopDao().insertOrReplace(shop);

**增加多个数据** 

- getShopDao().insertInTx(shopList);
- getShopDao().insertOrReplaceInTx(shopList);

**查询全部** 

- List\< Shop> list = getShopDao().loadAll();
- List\< Shop> list = getShopDao().queryBuilder().list();

**查询附加单个条件** 

- .where()
- .whereOr()

**查询附加多个条件** 

- .where(, , ,)
- .whereOr(, , ,)

**查询附加排序** 

- .orderDesc()
- .orderAsc()

**查询限制当页个数** 

- .limit()

**查询总个数** 

- .count()

**修改单个数据** 

- getShopDao().update(shop);

**修改多个数据** 

- getShopDao().updateInTx(shopList);

**删除单个数据** 

- getTABUserDao().delete(user);

**删除多个数据** 

- getUserDao().deleteInTx(userList);

**删除数据ByKey** 

- getTABUserDao().deleteByKey();



## 数据库升级

[**GreenDaoUpgradeHelper**](https://github.com/yuweiguocn/GreenDaoUpgradeHelper)