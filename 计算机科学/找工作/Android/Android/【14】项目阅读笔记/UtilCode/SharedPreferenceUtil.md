[TOC]

### 源代码

```java
public class SharedPreferencesUtil {

    // SharedPreferences  Map
    private static Map<String, SharedPreferencesUtil> SP_UTILS_MAP = new ConcurrentHashMap<>();
    // SharedPreferences 在Util的构造函数里进行初始化
    private SharedPreferences sp;

    //连续四个方法用来获取SharedPreferencesUtil
    public static SharedPreferencesUtil getInstance() {
        return getInstance("", Context.MODE_PRIVATE);
    }

    public static SharedPreferencesUtil getInstance(int mode) {
        return getInstance("", mode);
    }

    public static SharedPreferencesUtil getInstance(String spName) {
        return getInstance(spName, Context.MODE_PRIVATE);
    }
	//上面三个方法最终调用到的方法
    public static SharedPreferencesUtil getInstance(String spName, int mode) {
        //如果SharedPreference名字为空，则名字默认为spUtils
        if (isSpace(spName)) {
            spName = "spUtils";
        }
        //获取Util
        SharedPreferencesUtil spUtils = SP_UTILS_MAP.get(spName);
        if (spUtils == null) {
            spUtils = new SharedPreferencesUtil(spName, mode);
            SP_UTILS_MAP.put(spName, spUtils);
        }
        return spUtils;
    }

    //Util的构造函数，同时sp进行初始化
    private SharedPreferencesUtil(String spName) {
        sp = AppRuntime.getAppContext().getSharedPreferences(spName, Context.MODE_PRIVATE);
    }

    private SharedPreferencesUtil(String spName, int mode) {
        sp = AppRuntime.getAppContext().getSharedPreferences(spName, mode);
    }

    public void put(@NonNull String key, String value) {
        put(key, value, false);
    }

    public void put(@NonNull String key, String value, boolean isCommit) {
        if (isCommit) {
            sp.edit().putString(key, value).commit();
        } else {
            sp.edit().putString(key, value).apply();
        }
    }

    public String getString(@NonNull String key) {
        return getString(key, "");
    }

    public String getString(@NonNull String key, String defaultValue) {
        return sp.getString(key, defaultValue);
    }

	//。。。。。。

    public Map<String, ?> getAll() {
        return sp.getAll();
    }

    public boolean contains(@NonNull String key) {
        return sp.contains(key);
    }

    public void remove(@NonNull String key) {
        remove(key, false);
    }

    public void remove(@NonNull String key, boolean isCommit) {
        if (isCommit) {
            sp.edit().remove(key).commit();
        } else {
            sp.edit().remove(key).apply();
        }
    }

    public void clear() {
        clear(false);
    }

    public void clear(boolean isCommit) {
        if (isCommit) {
            sp.edit().clear().commit();
        } else {
            sp.edit().clear().apply();
        }
    }

    //判断sharedpreference名字是否为空
    private static boolean isSpace(String s) {
        if (s == null) {
            return true;
        }
        for (int i = 0, len = s.length(); i < len; ++i) {
            if (!Character.isWhitespace(s.charAt(i))) {
                return false;
            }
        }
        return true;
    }
}
```



### apply()方法

- apply()方法没有返回值；
- apply()方法先提交到内存是一个原子操作，然后异步提交到Disk。如果有两个editors同时修改preferences，最后一个调用apply()方法的会成功。apply()方法因为异步提交到Disk，所以效率更高。

#### commit()方法

- commit()方法有返回值；
- commit()方法是直接提交到Disk，是一个原子操作，如果两个editors同时修改preferences，最后一个调用commit()方法的会成功。

### 总结：

apply()方法和commit()方法都是先提交到内存，commit是同步提交到硬盘，并且有返回值；而apply()方法是异步提交到硬盘，没有返回值。由于在一个进程中，sharedPreference是单实例，一般不会出现并发冲突，如果对提交的结果不关心的话，建议使用apply，当然需要确保提交成功且有后续操作的话，还是需要用commit的。