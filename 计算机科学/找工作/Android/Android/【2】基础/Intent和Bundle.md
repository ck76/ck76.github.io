[TOC]

Intent和Bundle结合起来传递数据。

### 先看Intent用法：

```java
Intent intent = new Intent();  
intent.putExtra("key1", value1);
intent.putExtra("key2", value2); 
```

### 在看Bundle 用法：

```java
Bundle bundle = new Bundle();  
bundle.putString("key1", value1);  
bundle.putString("key2", value2);
intent.putExtras(bundle);
```

### 取值操作：

```java
Bundle bundle = this.getIntent().getExtras();  
String v = bundle.getString("key1");
```

### 一个结合起来的示例

User 类

```java
public class User implements Serializable {
    //其他代码省略
}
```

关键代码

```java
User user = new User();
Intent intent = new Intent(MyActivity.this,OthereActivity.class);
Bundle bundle = new Bundle();
bundle.putSerializable("user", user);
intent.putExtras(bundle);
startActivity(intent);
```