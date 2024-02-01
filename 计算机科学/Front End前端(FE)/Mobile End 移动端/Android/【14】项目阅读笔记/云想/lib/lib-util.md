[TOC]

### 1、Gson&JsonUtil

```java
public class GsonUtil {

    /** Gson */
    private static Gson sGson;

    static {
        sGson = new Gson();
    }

    /**
     * 获取Gson解析
     */
    public static Gson get() {
        return sGson;
    }

    /**
     * 解析String 为JSONObject
     *
     * @param content 内容
     */
    public static JsonObject parseString(String content) {
        if (TextUtils.isEmpty(content)) {
            return new JsonObject();
        }
        try {
            return new JsonParser().parse(content).getAsJsonObject();
        } catch (JsonSyntaxException e) {
            e.printStackTrace();
            return new JsonObject();
        }
    }
}
```

```java
public class JsonUtil {

    /**
     * 解析String 为JSONObject
     *
     * @param content 内容
     */
    public static JSONObject parseString(String content) {
        if (TextUtils.isEmpty(content)) {
            return new JSONObject();
        }
        try {
            return new JSONObject(content);
        } catch (JSONException e) {
            e.printStackTrace();
            return new JSONObject();
        }
    }
}
```



### 2、ToastUtil

第一次Toast的时候不能用activity的context，因为静态私有变量sToast会持有activity引用导致内存泄漏。

```java
public class ToastUtil {

    private static Toast sToast;
    private static boolean sEnableShow = true;

    private ToastUtil() {

    }

    /**
     * 全局控制toast展示
     *
     * @param enableShow 是否允许展示
     */
    public static void setEnableShow(boolean enableShow) {
        sEnableShow = enableShow;
    }


    public static void show(Context context, CharSequence msg) {
        show(context, msg, Toast.LENGTH_SHORT);
    }

    public static void show(Context context, int resId) {
        show(context, resId, Toast.LENGTH_SHORT);
    }

    public static void showLong(Context context, CharSequence msg) {
        show(context, msg, Toast.LENGTH_LONG);
    }

    public static void showLong(Context context, int resId) {
        show(context, resId, Toast.LENGTH_LONG);
    }

    @SuppressLint("ShowToast")
    public static void show(Context context, CharSequence msg, int duration) {
        if (sEnableShow) {
            if (sToast == null) {
                sToast = Toast.makeText(context, msg, duration);
            } else {
                sToast.setText(msg);
            }

            if (context instanceof Activity) {
                showOnUiThread((Activity) context);
            } else {
                sToast.show();
            }
        }
    }

    @SuppressLint("ShowToast")
    public static void show(Context context, int resId, int duration) {
        if (sEnableShow) {
            if (sToast == null) {
                sToast = Toast.makeText(context, resId, duration);
            } else {
                sToast.setText(resId);
            }

            if (context instanceof Activity) {
                showOnUiThread((Activity) context);
            } else {
                sToast.show();
            }
        }
    }

    private static void showOnUiThread(Activity activity) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                sToast.show();
            }
        });
    }

    /**
     * 取消toast
     */
    public static void cancelToast() {
        if (sToast != null) {
            sToast.cancel();
            sToast = null;
        }
    }
}
```

