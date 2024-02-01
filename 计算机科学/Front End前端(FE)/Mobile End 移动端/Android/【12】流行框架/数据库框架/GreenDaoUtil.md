

## 1.DaoHelper

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

## 2.DaoUpgradeHelper

```java
public class DaoUpgradeHelper extends DaoMaster.OpenHelper {
    public DaoUpgradeHelper(Context context, String name) {
        super(context, name);
    }

    public DaoUpgradeHelper(Context context, String name, SQLiteDatabase.CursorFactory factory) {
        super(context, name, factory);
    }

    @Override
    public void onUpgrade(Database db, int oldVersion, int newVersion) {
        super.onUpgrade(db, oldVersion, newVersion);

        //是否开启日志
        MigrationHelper.DEBUG = BuildConfig.LOG_DEBUG;

        MigrationHelper.migrate(db, new MigrationHelper.ReCreateAllTableListener() {
                    @Override
                    public void onCreateAllTables(Database db, boolean ifNotExists) {
                        DaoMaster.createAllTables(db, ifNotExists);
                    }

                    @Override
                    public void onDropAllTables(Database db, boolean ifExists) {
                        DaoMaster.dropAllTables(db, ifExists);
                    }
                }, ConfigureDao.class, UserDao.class, ExamDao.class, ExpLessonDao.class,
                GradeDao.class, GradeSumDao.class, GradeRankDao.class, LessonDao.class,
                MenuDao.class, NoticeDao.class, SearchHistoryDao.class, TimeAxisDao.class);
    }
}
//
//可以在onUpgrade方法中进行数据库的迁移，如果自定义了DBHelper，则数据库的初始化变为如下方式：
//        DBHelper devOpenHelper = new DBHelper(this);
//        DaoMaster daoMaster = new DaoMaster(devOpenHelper.getWritableDb());
//        DaoSession daoSession = daoMaster.newSession();
//        userDao = daoSession.getUserDao();
```

## 3.DaoUtil

```java
public class DaoUtils {

    /**
     * 获取daoSession
     */
    public static DaoSession getDaoSession() {
        return DaoHelper.getDaoHelper(MApplication.application).getDaoSession();
    }
}
```

