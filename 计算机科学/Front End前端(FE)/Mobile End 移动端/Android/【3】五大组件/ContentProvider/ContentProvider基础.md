[TOC]

### 目录

```java
一、基础回顾。
   - 简介
   - 特点
   - 使用场景
二、相关概念
   1、Uri
   2、工具类：UriMatcher
   3、工具类ContentUris
三、使用ContentProvider共享数据
四、使用ContentResolver调用ContentProvider去操作数据库数据。
  【附录】
```



### 一、基础回顾。

- **简介**

  ContentProvider（数据提供者）是在应用程序间共享数据的一种接口机制，虽然我们可以采用文件存储方式、sharedpreferences方式在程序间进行共享数据，但ContentProvider提供了更为高级的数据共享方法，应用程序可以指定需要共享的数据，而其他应用程序则可以在不知数据来源、路径的情况下，对共享数据进行查询、添加、删除和更新等操作，当应用继承ContentProvider类，并重写该类用于提供数据和存储数据的方法，就可以向其他应用共享其数据。

  

  ![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/prVeHL6pTEwhIjozI8JBEBLewuk548NjJxWKnYOvR7s!/r/dL8AAAAAAAAA)

------

- **特点**
  **1、**为存储和获取数据提供了统一的接口。
  **2、**可以在不同的应用程序之间共享数据。
  **3、**使用数据库表的形式来组织数据进行封装。[数据库使用：Android实习生 —— 数据存储与共享](https://www.jianshu.com/p/4375bedacb99)
  **4、**为应用间的数据交互提供了一个安全的环境。它准许你把自己的应用数据根据需求开放给其他应用进行增、删、改、查，而不用担心直接开放数据库权限而带来的安全问题。
  *【总的来说使用ContentProvider对外共享数据的好处是统一了数据的安全访问方式。】*

------

- **使用场景**
  Android已经为常见的一些数据提供了系统默认的ContentProvider，比如去获取通讯录信息、获取图片、视频信息。我们可以在其他应用程通过提供的ContentProvider获取这些数据。



### 二、相关概念

#### 1、Uri

**通用资源标志符（Universal Resource Identifier, 简称"URI"）。**

Uri代表了要操作的数据，它为系统的每一个资源给其一个名字，比方说通话记录。每一个ContentProvider都拥有一个公共的URI，这个URI用于表示这个ContentProvider所提供的数据。

![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/T4ONt23yS.YU2cDcsB0d3P97ilXoNa9ZUT7kQyRRPqY!/r/dFMBAAAAAAAA)



**URI一般主要由三部分组成：**

- **Authority**

授权信息，用以区别不同的ContentProvider，外部调用者可以根据这个标识来找到它。为了保证URI标识的唯一性，它必须是一个完整的、小写的类名。这个标识在 元素的 authorities属性中说明：一般是定义该ContentProvider的包.类的名称。

```xml
<provider android:name=".MyProvider"
          android:authorities="com.xxx.MyApp.myprovider" />
```

- **Path**：表名，用以区分ContentProvider中不同的数据表；

- **Id**：Id号，用以区别表中的不同数据记录；如果没有ID，就表示返回全部； "[content://com.xxx.MyApp.myprovider/tablename/#](https://link.jianshu.com/?t=content://com.xxx.MyApp.myprovider/tablename/#)" #表示数据id。

【举例】
1、要操作person表中id为10的记录，可以构建这样的路径:/person/10
2、要操作person表中id为10的记录的name字段， person/10/name
3、要操作person表中的所有记录，可以构建这样的路径:/person
4、如果要把一个字符串转换成Uri，可以使用Uri类中的parse()方法，如下：Uri uri = Uri.parse("[content://com.xxx.MyApp.myprovider/person](https://link.jianshu.com/?t=content://com.xxx.MyApp.myprovider/person)")

**【每个ContentProvider都有一个公共的URI，这个URI用于表示这个ContentProvider所提供的数据。Android所提供的ContentProvider都存放在android.provider包当中】**

------

#### 2、工具类：UriMatcher

因为Uri代表了要操作的数据，所以我们经常需要解析Uri，并从Uri中获取数据。Android系统提供了两个用于操作Uri的工具类，分别为**UriMatcher**和**ContentUris**。掌握它们的使用，会便于我们的开发工作。

使用方法如下：

**第一步，初始化：**

```java
//常量UriMatcher.NO_MATCH表示不匹配任何路径的返回码
UriMatcher matcher = new UriMatcher(UriMatcher.NO_MATCH);  
```

**第二步，注册需要的Uri:**

```java
//如果match()方法匹配content://com.xxx.MyApp.myprovider/person路径，返回匹配码为1
matcher.addURI("com.xxx.MyApp.myprovider", "person", 1);  
//如果match()方法匹配content://com.xxx.MyApp.myprovider/person/230路径，返回匹配码为2
matcher.addURI("com.xxx.MyApp.myprovider", "person/#", 2);  //#号为通配符
```

**第三步，与已经注册的Uri进行匹配:**

```java
Uri uri = Uri.parse("content://com.xxx.MyApp.myprovider/people");  
int match = matcher.match(uri);  
       switch (match)  
       {  
           case 1:  
               break;
           case 2:  
               break;
           default:  
               break;
       }  
```

match方法匹配后会返回一个匹配码Code，即在使用注册方法addURI时传入的第三个参数。

#### 3、工具类ContentUris：

ContentUris类用于操作Uri路径后面的ID部分，它有两个比较实用的方法：
**withAppendedId(uri, id)**用于为路径加上ID部分：

```java
Uri uri = Uri.parse("content://com.xxx.MyApp.myprovider/person")
Uri resultUri = ContentUris.withAppendedId(uri, 10); 
//生成后的Uri为：content://com.xxx.MyApp.myprovider/person/10
```

**parseId(uri)**方法用于从路径中获取ID部分：

```java
Uri uri = Uri.parse("content://com.xxx.MyApp.myprovider/person/10")
long personid = ContentUris.parseId(uri);//获取的结果为:10
```



### 三、使用ContentProvider共享数据

**1、首先建立一个Provider所用到变量类，方便Demo调用**

```java
public class ContentData {

    //provider唯一标示信息
    protected static final String CONTENT_AUTHORITY = "com.xxx.MyApp.myprovider";

    //基础Uri
    protected static final Uri BASE_CONTENT_URI = Uri.parse("content://" + CONTENT_AUTHORITY);

    //操作表的名称
    protected static final String PATH_TEST = "people";

    //表中记录信息
    public static final class TestEntry implements BaseColumns {
        
        // 完整Uri
        public static final Uri CONTENT_URI = BASE_CONTENT_URI.buildUpon().appendPath(PATH_TEST).build();

        protected static Uri buildUri(long id) {
            return ContentUris.withAppendedId(CONTENT_URI, id);
        }

        protected static final String TABLE_NAME = "people";

        public static final String COLUMN_NAME = "name";

        public static final String COLUMN_SEX = "sex";

        public static final String COLUMN_AGE = "age";
    }
}
```

**2、Provider最终还要操作数据库，这里我们写数据库操作类代码。数据库使用：Android实习生 —— 数据存储共享(近期更新)**

```java
public class DBOpenHelper extends SQLiteOpenHelper {

    //数据库版本
    private static final int DATABASE_VERSION = 1;
    //数据库名称
    private static final String DATABASE_NAME = "people.db";

    //构造方法
     public DBOpenHelper(Context context) {
     super(context, DATABASE_NAME,null, DATABASE_VERSION);
     }
    
    //通过sql语句建表并插入数据
    @Override
    public void onCreate(SQLiteDatabase db) {
        System.out.println("create table");

        final String SQL_CREATE_CONTACT_TABLE = "CREATE TABLE " + ContentData.TestEntry.TABLE_NAME + "( "
                + "_id integer primary key autoincrement,"
                + ContentData.TestEntry.COLUMN_NAME + " TEXT NOT NULL,"
                + ContentData.TestEntry.COLUMN_SEX + " TEXT NOT NULL,"
                + ContentData.TestEntry.COLUMN_AGE + " INTEGER NOT NULL );";
        db.execSQL(SQL_CREATE_CONTACT_TABLE);

        db.execSQL("insert into people(name,sex,age)values('张三','女',18)");
        db.execSQL("insert into people(name,sex,age)values('张四','男',20)");
        db.execSQL("insert into people(name,sex,age)values('张五','女',19)");
        db.execSQL("insert into people(name,sex,age)values('张六','男',22)");
    }

    //数据库升级的时候会调用的代码
    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        db.execSQL("DROP TABLE IF EXISTS " + ContentData.TestEntry.TABLE_NAME);
        onCreate(db);

    }

}
```

**3、首先我们创建一个自己的MyProvider继承ContentProvider。默认该Provider需要实现如下六个方法**

```java
public class MyProvider extends ContentProvider {
    
    private DBOpenHelper dbOpenHelper;//声明数据库操作类

    private final static int TEST = 100;//匹配码
    
    //使用UriMatcher解析Uri，如果被匹配到，返回匹配码100
    static UriMatcher buildUriMatcher() {
        final UriMatcher matcher = new UriMatcher(UriMatcher.NO_MATCH);
        final String authority = ContentData.CONTENT_AUTHORITY;

        matcher.addURI(authority, ContentData.PATH_TEST, TEST);

        return matcher;
    }
    @Override
    //该方法在ContentProvider被其它应用第一次访问它时才会被创建。
   //同时我们操作数据库建表
    public boolean onCreate() {
         dbOpenHelper = new DBOpenHelper(getContext());
        return true;
    }

     @Override
    //该方法用于供外部应用往ContentProvider添加数据。
    public Uri insert(Uri uri, ContentValues contentValues) {
        //获得可写数据库
        final SQLiteDatabase db = dbOpenHelper.getWritableDatabase();
        Uri returnUri;
        long _id;
        switch ( buildUriMatcher().match(uri)) {
            case TEST:
                //插入数据
                _id = db.insert(ContentData.TestEntry.TABLE_NAME, null, values);
                if ( _id > 0 )
                    returnUri = ContentData.TestEntry.buildUri(_id);
                else
                    throw new android.database.SQLException("Failed to insert row into " + uri);
                break;
            default:
                throw new android.database.SQLException("Unknown uri: " + uri);
        }
        return returnUri;
    }

    @Override
    //该方法用于供外部应用从ContentProvider删除数据。
    public int delete(Uri uri, String s, String[] strings) {
        return 0;
    }

    @Override
    //该方法用于供外部应用从ContentProvider中获取数据。
    public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        //获得可读数据库
        final SQLiteDatabase db = dbOpenHelper.getReadableDatabase();
         //查到的结果是游标类型。
        Cursor cursor = null;
        switch ( buildUriMatcher().match(uri)) {
            case TEST:
                cursor = db.query(ContentData.TestEntry.TABLE_NAME, projection, selection, selectionArgs, sortOrder, null, null);
                break;
        }

        return cursor;
    }

    @Override
    //该方法用于供外部应用从ContentProvider更新数据。
    public int update(Uri uri, ContentValues contentValues, String s, String[] strings) {
        return 0;
    }

    @Override
    public String getType(Uri uri) {
        return null;
    }
      //该方法用于返回当前Uri所代表数据的MIME类型。

    //如果操作的数据属于集合类型，那么MIME类型字符串应以vnd.android.cursor.dir/开头，
    //例如：要得到所有person记录的Uri为content://com.xxx.MyApp.myprovider/person，
    //那么返回的MIME类型字符串应该为："vnd.android.cursor.dir/person"。

    //如果要操作的数据属于非集合类型数据，那么MIME类型字符串应该以vnd.android.cursor.item/开头，
    //例如：得到id为10的person记录，Uri为content://com.xxx.MyApp.myprovider/person/10，
    //那么返回的MIME类型字符串为："vnd.android.cursor.item/person"。

}
```

【注意】以上代码并没有实现删除，更改功能

**4、注册Provider**

```xml
//应用内访问
<provider    
    android:authorities="com.xxx.MyApp.myprovider"  
    android:name=".MyProvider" />
```

在注册的时候需要注意几个属性：

```groovy
android:exported 设置此provider是否可以被其他应用使用。
android:readPermission 该provider的读权限的标识
android:writePermission 该provider的写权限标识
android:permission provider读写权限标识
```

如何让其他应用也可以访问此应用中的数据呢，我们需要这么注册

```xml
<provider
    android:authorities="com.xxx.MyApp.myprovider"
    android:name=".MyProvider"
    android:readPermission="com.xxx.READ"
    android:exported="true">
</provider>
```

并且要在注册文件中声明一个permission

```xml
<permission android:name="com.bb.READ" android:protectionLevel="normal"/>
```

【通过以上步骤，一个ContentProvider就造好了。】



### 四、使用ContentResolver调用ContentProvider去操作数据库数据。

- 为什么我们不直接访问Provider，而是又在上面加了一层ContentResolver来进行对其的操作。

大家要知道一台手机中可不是只有一个Provider内容，它可能安装了很多含有Provider的应用，比如联系人应用，日历应用，字典应用等等。有如此多的Provider，如果你开发一款应用要使用其中多个，如果让你去了解每个ContentProvider的不同实现，岂不是要头都大了。所以Android为我们提供了ContentResolver来统一管理与不同ContentProvider间的操作。

![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/HyaAHUxnwSH3s6CiZTTkwoSLg6kt5a4xaPtA.etUKRY!/r/dLgAAAAAAAAA)



- 实现过程

![img](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/E95AI7feS9LtMwnlCRO1CRiY2SKUZo0lWeZU7JdVRiU!/r/dL8AAAAAAAAA)



- 实现数据操作的过程

1、当外部应用需要对ContentProvider中的数据进行添加、删除、修改和查询操作时，要获取ContentResolver 对象，可以使用Activity提供的getContentResolver()方法通过URI进行数据操作。

```java
    ContentResolver resolver = getContentResolver();
```

2、ContentResolver 类提供了与ContentProvider类相同签名的四个方法：

```java
  public Uri insert(Uri uri, ContentValues values)：
//该方法用于往ContentProvider添加数据。

  public int delete(Uri uri, String selection, String[] selectionArgs)：
//该方法用于从ContentProvider删除数据。

  public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder)：
//该方法用于从ContentProvider中获取数据。

  public int update(Uri uri, ContentValues values, String selection, String[] selectionArgs)：
//该方法用于更新ContentProvider中的数据。
```

使用ContentResolver对我们刚才造的ContentProvider中的数据进行添加、查询操作：

```java
       public class MainActivity extends AppCompatActivity {
       //声明Uri常量
      private static final Uri CONTENT_URI = Uri.parse("content://com.xxx.MyApp.myprovider/people");
      TextView tv_id, tv_name, tv_sex, tv_age;   
         
     @Override
      protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        tv_id = (TextView) findViewById(R.id.tv_id);
        tv_name = (TextView) findViewById(R.id.tv_name);
        tv_sex = (TextView) findViewById(R.id.tv_sex);
        tv_age = (TextView) findViewById(R.id.tv_age);
        }
      
      //query()为在xml组建中定义的OnClick
      public void query(View view) {
        //通过getContentResolver().query调用ContentProvider实现对数据库的查询
        tv_id.setText("");
        tv_name.setText("");
        tv_sex.setText("");
        tv_age.setText("");
        Cursor cursor = getContentResolver().query(CONTENT_URI, new String[]{"_id", "name", "sex", "age"
        }, null, null, null);
        if (cursor != null) {
            while (cursor.moveToNext()) {
                tv_id.append("\n" + cursor.getString(cursor.getColumnIndex("_id")));
                tv_name.append("\n" + cursor.getString(cursor.getColumnIndex("name")));
                tv_sex.append("\n" + cursor.getString(cursor.getColumnIndex("sex")));
                tv_age.append("\n" + cursor.getString(cursor.getColumnIndex("age")));
            }
            cursor.close();
        }
      }

      public void add(View view) {
        //通过getContentResolver().insert调用ContentProvider实现对数据库的增加
        ContentValues values = new ContentValues();
        values.put("name", "新来的");
        values.put("sex", "男");
        values.put("age", "28");
        getContentResolver().insert(CONTENT_URI, values);
        query(findViewById(R.id.btn_query));
      }  
  }
```


3、在注册文件中记得增加权限读取权限

```xml
<uses-permission android:name="com.xxx.READ"/>
    <uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" />
```

