---
title: SharedPreferences存储数据
date: 2018-08-12 00:14:48
categories: "Android持久化数据"
tags:
---

SharedPreferences类，它是一个轻量级的存储类，通过键值对方式进行存储，特别适合用于保存软件配置参数。

SharedPreferences保存数据，其背后是用xml文件存放数据，文件存放在/data/data/\<package name>/shared_prefs目录下：

<!--more-->

### 获取SharedPreferences的三种方式

- Context类中的getSharedPreferences()方法

两个参数，一个是文件名称，一个是操作模式，目前只有MODE_PRIVATE一种可选

- Activity类中的getPreferences()方法

只接受一个参数，因为会自动将当前**活动的类名**作为文件名

- PreferencesManager类中的getDefaultSharedPreferences()方法

接受一个Context参数，并将当前**程序的包名**作为文件名。

### 将数据存储起来

```java
SharedPreferences.Editor editor = getSharedPreferences("login_user", Context.MODE_PRIVATE).edit();
editor.putString("userName", "ck");
editor.apply();
```

### 读取数据

```java
SharedPreferences sharedPreferences = getSharedPreferences("login_user", Context.MODE_PRIVATE);
sharedPreferences.getString("userName", "ck");
```

第一个是键名，第二个参数是默认值。

### CacheUtil

```java
/**
 * 通过SharedPreferences实现的缓存工具类
 */

public class CacheUtil {


    /**
     * 保存在手机里面的文件名
     */
    private static final String FILE_NAME = "share_data";

    /**
     * 保存数据
     */
    public static void put(String key, Object value) {
        SharedPreferences sp = App.getInstance().getSharedPreferences(FILE_NAME, Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sp.edit();
        if (value instanceof String) {
            editor.putString(key, (String) value);
        } else if (value instanceof Integer) {
            editor.putInt(key, (Integer) value);
        } else if (value instanceof Boolean) {
            editor.putBoolean(key, (Boolean) value);
        } else if (value instanceof Float) {
            editor.putFloat(key, (Float) value);
        } else if (value instanceof Long) {
            editor.putLong(key, (Long) value);
        } else if (value instanceof Serializable) {

            try {
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                ObjectOutputStream oos = new ObjectOutputStream(baos);
                //把对象写到流里
                oos.writeObject(value);
                String temp = new String(Base64.encode(baos.toByteArray(), Base64.DEFAULT));
                editor.putString(key, temp);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        editor.apply();
    }


    /**
     * 获取sp实例
     *
     * @return
     */
    public static SharedPreferences getSP() {
        return App.getInstance().getSharedPreferences(FILE_NAME, Context.MODE_PRIVATE);
    }

    /**
     * 获取序列化对象
     *
     * @param key
     * @param defValue
     * @return
     */
    public static Serializable getSerializable(String key, Serializable defValue) {
        String temp = getSP().getString(key, "");
        ByteArrayInputStream bais = new ByteArrayInputStream(Base64.decode(temp.getBytes(), Base64.DEFAULT));
        try {
            ObjectInputStream ois = new ObjectInputStream(bais);
            defValue = (Serializable) ois.readObject();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e1) {
            e1.printStackTrace();
        }
        return defValue;
    }

    /**
     * 移除某个key值已经对应的值
     */
    public static void remove(String key) {
        SharedPreferences sp = getSP();
        SharedPreferences.Editor editor = sp.edit();
        editor.remove(key);
        editor.apply();
    }

    /**
     * 清除所有数据
     */
    public static void clear() {
        SharedPreferences sp = getSP();
        SharedPreferences.Editor editor = sp.edit();
        editor.clear();
        editor.apply();
    }

}
```

