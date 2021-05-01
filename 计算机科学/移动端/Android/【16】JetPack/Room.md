[TOC]

### 一、前言

Room为SQLite提供了一个抽象层，使得可以流畅使用SQLite的所有功能。

处理大量结构化数据的app可以从本地数据持久化中获取巨大利益。最常见的用例是缓存相关的数据。在这种情况下，当设备无法访问网络的时候，用户仍然可以在离线时浏览内容。任何用户原始数据的变化都会在连接网络后同步。

核心框架提供了原生SQL的支持。尽管这些API很强大，但是比较底层并且需要花费大量的时间和努力去使用：

- 没有原生SQL查询语句的编译时验证。当你的数据结构变化时，你需要手动更新受影响的SQL。这个过程会花费大量的时间并且很容易错误频出。

Room考虑到了这些，提供了SQLite的抽象层。

Room有三个主要的组件：

- 数据库（Database）：你可以使用该组件创建数据库的持有者。该注解定义了实体列表，该类的内容定义了数据库中的DAO列表。这也是访问底层连接的主要入口点。注解类应该是抽象的并且扩展自`RoomDatabase`。在运行时，你可以通过调用`Room.databaseBuilder()`或者`Room.inMemoryDatabaseBuilder()`获取实例。
- 实体（Entity）：这个组件代表了持有数据库表记录的类。对每种实体来说，创建了一个数据库表来持有所有项。你必须通过`Database`中的`entities`数组来引用实体类。实体的每个成员变量都被持久化在数据库中，除非你注解其为`@Ignore`。

> 实体类可以拥有无参数构造函数（如果DAO类可以访问每个持久化成员变量）或者拥有和实体类成员变量匹配参数的构造函数。Room也可以使用全部或者部分构造函数，例如只接收部分成员变量的构造函数。

- 数据访问对象（DAO）：这个组件代表了作为DAO的类或者接口。DAO是Room的主要组件，负责定义访问数据库的方法。被注解`@Database`的类必须包含一个无参数的抽象方法并返回被`@Dao`注解的类型。当编译时生成代码时，Room会创建该类的实现。

> 通过使用DAO类访问数据库而不是查询构建器或直接查询，你可以将数据库架构的不同组件分离。此外，DAO允许你在测试时很容易地模拟数据访问。

这些组件和app的其他部分关系图如下：



![img](https:////upload-images.jianshu.io/upload_images/1226129-dddf9b5e1cfb0565.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/600)

image

(img)

下面的代码片段包含了简单的数据库配置，含有1个实体和一个DAO:

```java
//User.java
@Entity
public class User {
    @PrimaryKey
    private int uid;

    @ColumnInfo(name = "first_name")
    private String firstName;

    @ColumnInfo(name = "last_name")
    private String lastName;

    // 省略Getters Setters（实际代码中不可省略）
   }

//UserDao.java
@Dao
public interface UserDao {
    @Query("SELECT * FROM user")
    List<User> getAll();

    @Query("SELECT * FROM user WHERE uid IN (:userIds)")
    List<User> loadAllByIds(int[] userIds);

    @Query("SELECT * FROM user WHERE first_name LIKE :first AND "
           + "last_name LIKE :last LIMIT 1")
    User findByName(String first, String last);

    @Insert
    void insertAll(User... users);

    @Delete
    void delete(User user);
}

//AppDatabase.java
@Database(entities = {User.class}, version = 1)
public abstract class AppDatabase extends RoomDatabase {
    public abstract UserDao userDao();
}
```

创建上面的文件以后，你可以使用以下代码获取已创建数据库实例：

```java
AppDatabase db = Room.databaseBuilder(getApplicationContext(),
        AppDatabase.class, "database-name").build();
```

> 当实例化`AppDatabase`对象时，你可以遵循单例设计模式，因为每个`RoomDatabase`实例代价是非常昂贵的，并且你几乎不需要访问多个实例。

### 二、实体

当一个类被`@Entity`注解，并被`@Database`注解的`entities`属性引用时，Room为这个实体在数据库中创建一个表。

默认情况，Room为实体类的每个成员变量创建一个列。如果一个实体类的某个成员变量不想被持久化，你可以使用`Ignore`注解标记，如：

```java
@Entity
class User {
    @PrimaryKey
    public int id;

    public String firstName;
    public String lastName;

    @Ignore
    Bitmap picture;//不进行持久化
}
```

为了持久化成员变量，Room必须可以访问它。你可以使成员变量是公共的，或者提供getter和setter方法。如果你使用getter/setter方法，请记住它们在Room中遵循Java Beans的概念。

#### 1、主键

每个实体必须至少定义一个成员变量作为主键。甚至仅仅有一个成员变量，也要标记其为`@PrimaryKey`。同时，如果你想要Room指定ID自增，你可以设置`@Primary`的`autoGenerate`属性。如果实体的主键是综合的，你可以使用`@Entity`的`primaryKeys`属性，如：

```java
@Entity(primaryKeys = {"firstName", "lastName"})
class User {
    public String firstName;
    public String lastName;

    @Ignore
    Bitmap picture;
}
```

默认情况下，Room使用类名作为数据库表的表名。如果你想要数据库表有一个其他的名字，设置`@Entity`注解的`tableName`属性即可：

```java
@Entity(tableName = "users")
class User {
    ...
}
```

> 注意：SQLite中的表名是大小写敏感的。

和`tablename`属性相似，Room使用成员名作为列名，如果你想要改变类名，在成员上添加`@ColumnInfo`注解即可：

```java
@Entity(tableName = "users")
class User {
    @PrimaryKey
    public int id;

    @ColumnInfo(name = "first_name")
    public String firstName;

    @ColumnInfo(name = "last_name")
    public String lastName;

    @Ignore
    Bitmap picture;
}
```

#### 2、索引与唯一

取决于你如何访问数据，你可能想要索引确切的字段以加速数据的查询。为了向实体添加索引，在`@Entity`中添加`indices`属性，列出你想要包括的字段名或者字段名组：

```java
@Entity(indices = {@Index("name"), @Index("last_name", "address")})
class User {
    @PrimaryKey
    public int id;

    public String firstName;
    public String address;

    @ColumnInfo(name = "last_name")
    public String lastName;

    @Ignore
    Bitmap picture;
}
```

有些时候具体的成员或成员组必须是独一无二的。你可以设置`@Index`的属性`unique`为`true`：

```java
@Entity(indices = {@Index(value = {"first_name", "last_name"},
        unique = true)})
class User {
    @PrimaryKey
    public int id;

    @ColumnInfo(name = "first_name")
    public String firstName;

    @ColumnInfo(name = "last_name")
    public String lastName;

    @Ignore
    Bitmap picture;
}
```

#### 3、关系

因为SQLite是一个关系型数据库，你可以指定对象间的关系。即使大多数ORM类库允许实体对象互相引用，Room则显式地禁止了这一点。

即使你不能直接使用关系映射，Room仍然允许你去定义实体键的外键约束。

例如，有另一个叫做`Book`的实体，你可以通过使用`@ForeignKey`注解定义其和`User`实体的关系，如：

```java
@Entity(foreignKeys = @ForeignKey(entity = User.class,
                                  parentColumns = "id",
                                  childColumns = "user_id"))
class Book {
    @PrimaryKey
    public int bookId;

    public String title;

    @ColumnInfo(name = "user_id")
    public int userId;
}
```

外键是非常强大的，因为它们允许你指定引用实体更新时会发生什么、例如，你可以告诉SQLite去删除所有的书籍，如果这些书所对应的`User`被删除并且指定了`@ForeignKey`的属性`onDelete=CASCADE`。

> SQLite将`@Insert(OnConfilct=REPLACE)`处理为`REMOVE`和`REPLACE`的集合而不仅仅是更新操作。这个替换冲突值的方法可能会对你的外键约束起作用。

#### 4、内嵌对象

有些时候你想要一个实体类或POJO类作为数据库逻辑的一部分。这种情况下，你可以使用`@Embedded`注解来。你可以查询内嵌成员，就像你可能查询其他字段一样。

例如，我们的`User`类包含一个`Address`类型的成员，代表了`street`、`city`、`state`和`postCode`。为了分别存储这些字段，在`User`类中包含一个`Address`成员并标记为`@Embedded`，如：

```java
class Address {
    public String street;
    public String state;
    public String city;

    @ColumnInfo(name = "post_code")
    public int postCode;
}

@Entity
class User {
    @PrimaryKey
    public int id;

    public String firstName;

    @Embedded
    public Address address;
}
```

这个表代表了一个`User`对象包含了以下字段：`id`，`firstName`，`street`，`state`，`city`和`post_code`。

> 内嵌成员也可以含有其他内嵌成员

如果一个实体含有多种相同类型的内嵌成员，你可以通过设置`prefix`属性保持每个字段的唯一性。Room之后添加提供的值到每个内嵌对象的起始位置。



### 三、数据访问对象(DAO)

Room的主要组件是`Dao`类。DAO以清晰的方式抽象除了访问数据库的行为。

> Room不允许在主线程方位数据库，除非你在Builder调用`allowMainThreadQueries()`，因为这可能会导致UI被锁住。而异步查询则不受此约束，因为异步调用在后台线程运行查询工作。

#### 1、便捷方法

有很多可以使用DAO类的便捷查询方法，例如：

##### Insert

当你创建一个DAO方法并标记其为`@Insert`，Room会生成在单一事务中将所有参数存入数据库的实现:

```java
@Dao
public interface MyDao {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    public void insertUsers(User... users);

    @Insert
    public void insertBothUsers(User user1, User user2);

    @Insert
    public void insertUsersAndFriends(User user, List<User> friends);
}
```

如果@Insert方法只接收一个参数，它会返回long，表示新插入项的row Id。如果参数是数组或集合，它会返回`long[]`或者`List<Long>`。

##### Update

Update是更新一组实体的便捷方法。它查询匹配主键的记录然后更新。如：

```java
@Dao
public interface MyDao {
    @Update
    public void updateUsers(User... users);
}
```

尽管通常并不需要如此，你可以让该方法返回一个`int`值，表示更新至数据库的行号。

##### Delete

Delete是删除一组实体的便捷方法。它使用主键去寻找记录并删除：

```java
@Dao
public interface MyDao {
    @Delete
    public void deleteUsers(User... users);
}
```

同上，你可以让该方法返回一个`int`值表示被删除的行号。

#### 2、使用@Query

`@Query`是用于DAO类的主要注解。它允许你在数据库上执行读写操作。每个`Query`方法都会在编译时验证，因此如果查询语句有问题，那么编译时就会报错，而不是在运行时发生。

Room同样验证查询的返回值，如果返回对象的成员名和字段名不一致，Room会以以下两种方式警告：

- 如果仅仅部分成员名相符，则发出警告
- 如果没有成员名相符，则发出错误

#### 3、简单查询

```java
@Dao
public interface MyDao {
    @Query("SELECT * FROM user")
    public User[] loadAllUsers();
}
```

这是一个加载所有用户的简单查询。在编译时，Room知道这是查询用户表的所有字段。如果查询语句含有语法错误，或者用户表在数据库中并不存在，Room会显示相应的错误。

#### 4、查询中传递参数

大多数时候，你需要在查询中传递参数来执行过滤操作，例如仅仅显示具体年龄的用户。为了完成这个任务，在你的Room注解中使用方法参数，如：

```java
@Dao
public interface MyDao {
    @Query("SELECT * FROM user WHERE age > :minAge")
    public User[] loadAllUsersOlderThan(int minAge);
}
```

当编译时处理这个查询时，，Room将`:minAge`和`minAge`匹配在一起。Room使用参数名进行匹配，如果匹配不成功，会在编译时报错。

你也可以传递多个参数或引用多次，如：

```java
@Dao
public interface MyDao {
    @Query("SELECT * FROM user WHERE age BETWEEN :minAge AND :maxAge")
    public User[] loadAllUsersBetweenAges(int minAge, int maxAge);

    @Query("SELECT * FROM user WHERE first_name LIKE :search "
           + "OR last_name LIKE :search")
    public List<User> findUserWithName(String search);
}
```

#### 5、返回所有字段的子集

大多数时候，你可能需要一个实体的一部分成员变量，例如你的UI可能只显示用户的名和姓，而不是用户的所有细节。通过仅仅获取出现在你UI中的字段，你可以存储很多资源，并且你的查询完成地更快。

Room允许你从查询中返回任何对象，只要结果字段集可以被映射到返回的对象上。例如，你可以创建下面的POJO类来获取用户的姓和名：

```java
public class NameTuple {
    @ColumnInfo(name="first_name")
    public String firstName;

    @ColumnInfo(name="last_name")
    public String lastName;
}
```

现在你可以在你的查询方法中这样使用POJO类：

```java
@Dao
public interface MyDao {
    @Query("SELECT first_name, last_name FROM user")
    public List<NameTuple> loadFullName();
}
```

Room理解这次返回`first_name`和`last_name`字段的查询，并可以映射到`NameTuple`类。这样，Room就能生成正确的代码。如果查询返回太多的字段，或者某个字段并不存在于`NameTuple`中，Room会显示一个警告。

#### 6、传递参数集合

你的某些查询可能会传递大量的参数，而且直到运行时才知道具体的参数。例如，你可能会获取关于用户所属区域的信息。当参数为集合时，Room能够理解并自动根据当前提供的参数进行扩展：

```java
@Dao
public interface MyDao {
    @Query("SELECT first_name, last_name FROM user WHERE region IN (:regions)")
    public List<NameTuple> loadUsersFromRegions(List<String> regions);
}
```

#### 7、可观察查询

当运行查询时，你通常想要在数据变化的时候你的app界面自动更新。为了做到这一点，在查询方法中使用`LiveData`类型的返回值。Room会生成所有必要的代码，当数据更新时，会自动更新`LiveData`。

```java
@Dao
public interface MyDao {
    @Query("SELECT first_name, last_name FROM user WHERE region IN (:regions)")
    public LiveData<List<User>> loadUsersFromRegionsSync(List<String> regions);
}
```

#### 8、RxJava

Room也可以从你定义的查询中直接返回RxJava2的`Publisher`和`Flowable`对象。为了使用这个功能，添加`android.arch.persistence.room:rxjava2`到你的Gradle构建依赖。你可以随后返回RxJava2定义的类型，如：

```java
@Dao
public interface MyDao {
    @Query("SELECT * from user where id = :id LIMIT 1")
    public Flowable<User> loadUserById(int id);
}
```

#### 9、查询多张表

你的一些查询可能需要访问多张表来计算结果。Room允许你写任何的查询，因此你可以使用连接表。另外，如果结果是可观察数据类型，例如`Flowable`或者`LiveData`，Room会验证所有SQL查询语句。

下面的代码片段展示了如何连接两张表，一张表是包含用户借书的信息，另一张包含当前借出的信息:

```java
@Dao
public interface MyDao {
    @Query("SELECT * FROM book "
           + "INNER JOIN loan ON loan.book_id = book.id "
           + "INNER JOIN user ON user.id = loan.user_id "
           + "WHERE user.name LIKE :userName")
   public List<Book> findBooksBorrowedByNameSync(String userName);
}
```

你也可以从这些查询中返回POJO类。例如你可以这样写一个用户和其宠物姓名的查询语句：

```java
@Dao
public interface MyDao {
   @Query("SELECT user.name AS userName, pet.name AS petName "
          + "FROM user, pet "
          + "WHERE user.id = pet.user_id")
   public LiveData<List<UserPet>> loadUserAndPetNames();

   // 你也可以在单独的文件中定义该类，只要你添加了public修饰符
     static class UserPet {
       public String userName;
       public String petName;
   }
}
```



### 四、使用类型转换

Room提供内置工具用于基本类型和其封装类型的装换。但是有些时候你可能使用了使用了自定义数据类型，而想在数据库表中始终单个字段。为了添加这类自定义类型支持，你需要提供一个`TypeConverter`，将自定义类转换到Room已知可以持久化的类型。

例如，如果我们想要持久化`Date`实例，我们可以这样写：

```java
public class Converters {
    @TypeConverter
    public static Date fromTimestamp(Long value) {
        return value == null ? null : new Date(value);
    }

    @TypeConverter
    public static Long dateToTimestamp(Date date) {
        return date == null ? null : date.getTime();
    }
}
```

上面的例子定义了两个函数，一个将`Date`类型转换为`Long`类型，另一个进行相反的转换。

接下来，在`AppDataBase`添加`@TypeConverters`注解，使得Room可以使用你定义的转换器：

```java
@Database(entities = {User.java}, version = 1)
@TypeConverters({Converter.class})
public abstract class AppDatabase extends RoomDatabase {
    public abstract UserDao userDao();
}
```

使用了这些转换器以后，你可以在其他查询中使用你的自定义类型，就像基本类型一样：

```java
//User.java
@Entity
public class User {
    ...
    private Date birthday;
}

//UserDao.java
@Dao
public interface UserDao {
    ...
    @Query("SELECT * FROM user WHERE birthday BETWEEN :from AND :to")
    List<User> findUsersBornBetweenDates(Date from, Date to);
}
```

你可以限制`@TypeConverters`的作用范围，包括单独实体，DAO以及DAO方法。



### 五、数据库迁移

当你在app添加以及修改功能时，你需要修改你的实体类以响应这些变化。当一个用户更新到最新版本的app时，你不想让他们丢掉所有已经存在的数据，特别是不能再从远程服务器获取的数据。

Room允许你编写`Migration`类来保护用户数据。每个`Migration`类指定一个`startVersion`和`endVersion`。在运行时，Room运行每个`Migration`类的`migrate()`方法，使用正确的顺序迁移至数据库的更新版本。

> 如果你没有提供必要的迁移，Room会重新构建数据库，这意味着你将丢失所有数据库中的数据。

```java
Room.databaseBuilder(getApplicationContext(), MyDb.class, "database-name")
        .addMigrations(MIGRATION_1_2, MIGRATION_2_3).build();

static final Migration MIGRATION_1_2 = new Migration(1, 2) {
    @Override
    public void migrate(SupportSQLiteDatabase database) {
        database.execSQL("CREATE TABLE `Fruit` (`id` INTEGER, "
                + "`name` TEXT, PRIMARY KEY(`id`))");
    }
};

static final Migration MIGRATION_2_3 = new Migration(2, 3) {
    @Override
    public void migrate(SupportSQLiteDatabase database) {
        database.execSQL("ALTER TABLE Book "
                + " ADD COLUMN pub_year INTEGER");
    }
};
```

> 为了保持你的迁移功能的正确性，使用完整的查询语句而不是询而不是引用表示查询的常量。

在迁移过程完成后，Room验证当前的表以确保迁移的正确性。如果Room找到问题，会抛出未匹配的异常信息。

### 六、测试迁移

迁移是很重要的事情，错误的编写会导致你app的崩溃循环。为了保证app的稳定性，你应该测试你的迁移工作。Room提供了一个测试的Maven构件来帮助测试。但是，为了该构件可以工作，你需要导出你的数据库表。

### 七、导出数据库表

```java
//build.gradle
android {
    ...
    defaultConfig {
        ...
        javaCompileOptions {
            annotationProcessorOptions {
                arguments = ["room.schemaLocation":
                             "$projectDir/schemas".toString()]
            }
        }
    }
}
```

Room会将你数据库的表信息导出为一个json文件。你应该在版本控制系统中保存该文件，该文件代表了你的数据库表历史记录，这样允许Room创建旧版本的数据库用于测试。

为了测试迁移，添加`android.arch.persistence.room:testing`到你的测试依赖，以及添加模式表的位置至asset文件夹，如：

```java
//build.gradle
android {
    ...
    sourceSets {
        androidTest.assets.srcDirs += files("$projectDir/schemas".toString())
    }
}
```

测试包提供了一个`MigrationTestHelper`类，可以读取这些模式表文件。

```java
@RunWith(AndroidJUnit4.class)
public class MigrationTest {
    private static final String TEST_DB = "migration-test";

    @Rule
    public MigrationTestHelper helper;

    public MigrationTest() {
        helper = new MigrationTestHelper(InstrumentationRegistry.getContext(),
                MigrationDb.class.getCanonicalName(),
                new FrameworkSQLiteOpenHelperFactory());
    }

    @Test
    public void migrate1To2() throws IOException {
        SupportSQLiteDatabase db = helper.createDatabase(TEST_DB, 1);

        // db 版本为1. 使用SQL添加一些数据
        // 你不能使用DAO，因为它表示的是最新的数据库
        db.execSQL(...);

        // 准备下个版本
        db.close();

        // 重新打开数据库版本2
        db = helper.runMigrationsAndValidate(TEST_DB, 2, true, MIGRATION_1_2);

         }
}
```



### 八、测试你的数据库

当运行测试你的app时，如果没有测试数据库本身，你不需要创建全部的数据库。Room允许在你的测试中模拟数据访问层。这个过程是可能的，因为你的DAO并没有泄漏任何数据库的细节。当测试剩下的app部分时，你应该创建模拟你的DAP类。

这里有两种测试数据库的方式：

- 在你的开发主机上
- 在Android设备上

#### 1、在你的主机上测试

Room使用SQLite支持库，提供了匹配安卓框架类的接口。这种支持允许你传递支持类库的自定义实现以测试你的数据库。

即使这种方案允许你测试非常快捷，但是并不值得推荐，这是因为你设备上以及你用户设备上运行的SQLite版本可能和你主机上运行的版本并不匹配。

#### 2、在Android设备上测试

这种推荐的测试数据库方法是编写运行在安卓设备上的JUnit测试。因为这些测试并不需要创建`Activity`，它们应该会比在UI上测试要快。

当设置你的测试时，你应该创建一个数据库的内存版本来使得测试更密闭，如：

```java
@RunWith(AndroidJUnit4.class)
public class SimpleEntityReadWriteTest {
    private UserDao mUserDao;
    private TestDatabase mDb;

    @Before
    public void createDb() {
        Context context = InstrumentationRegistry.getTargetContext();
        mDb = Room.inMemoryDatabaseBuilder(context, TestDatabase.class).build();
        mUserDao = mDb.getUserDao();
    }

    @After
    public void closeDb() throws IOException {
        mDb.close();
    }

    @Test
    public void writeUserAndReadInList() throws Exception {
        User user = TestUtil.createUser(3);
        user.setName("george");
        mUserDao.insert(user);
        List<User> byName = mUserDao.findUsersByName("george");
        assertThat(byName.get(0), equalTo(user));
    }
}
```



### 九、附加：没有实体键的对象引用

从数据库到对象间关系的映射是一个很常见的实践，并且在服务端运行良好，在它们被访问的时候进行高性能的惰性加载。

但是在客户端，惰性加载并不可行，这是因为很有可能发生在主线程，在主线程查询磁盘信息会导致很严重的性能问题。主线程有大概16ms来计算并绘制一个`Activity`的界面更新，因此甚至一个查询仅仅耗费5ms，你的app仍然会耗光绘制画面的时间，导致显著的Jank问题。更糟的是，如果有个并发运行的数据库事务，或者如果设备正忙于处理其他磁盘相关的繁重工作，查询会花费更多的时间完成。如果你不使用惰性加载的方式，app会获取多余其所需要的数据，从而导致内存消耗的问题。

ORM通常将该问题交给开发者决定，使得他们可以根据自己的用例选择最佳的方式。不幸地是，开发者通常终止模型和UI之间的共享。当UI变更超时时，问题随之发生并且很难预感和解决。

举个例子，UI界面读取一组`Book`列表，每本书拥有一个`Author`对象。你可能开始会设计你的查询去使用惰性加载，从而`Book`实例使用`getAuthor()`方法查询数据库。过了一些时间，你意识到你需要在app的UI界面显示作者名。你可以添加以下方法：

```java
authorNameTextView.setText(user.getAuthor().getName());
```

但是这种看似没有问题的代码会导致`Author`表在主线程被查询。

如果你急于查询作者信息，这会变得很难去改变数据是如何加载的，如果你不再需要这个数据的话，例如当你app的UI不再需要显示关于特定作者信息的时候。于是你的app必须继续加载不再显示的信息。这种方式更为糟糕，如果`Author`类引用了其他表，例如`getBooks()`方法。

由于这些原因，Room禁止实体间的对象引用。作为替换，你必须显式地请求你所需要的数据。

> 简单通俗地解释一下Jank：第2帧画面同步信号已经到来，由于第2帧数据还没有准备就绪，显示的还是第1帧。这种情况被Android开发组命名为“Jank”



### 十、链接

- https://juejin.im/post/5ad425d36fb9a028c06b48f6

- https://www.jianshu.com/p/cfde3535233d

