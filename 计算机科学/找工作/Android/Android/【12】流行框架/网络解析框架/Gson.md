[TOC]

### 一、Json基础

```json
// JSON实例
｛"skill":{
          "web":[
                 {
                  "name":"html",
                  "year":"5"
                 },
                 {
                  "name":"ht",
                  "year":"4"
                 }],
           "database":[
                  {
                  "name":"h",
                  "year":"2"
                 }]
`}}
```

- 1个`JSON`值的内容形式可以是：”名称 - 值“对、数组 或 对象，下面将详细说明

![Gson](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/sx1qoxi6qTuG2YTqk6GZeGTIMr23Nkk6lnxhuEPc*58!/r/dDQBAAAAAAAA)



### 二、穿插个小知识点

解析assert文件里的json

```java
    public class Util {  
        ...  
      
        /** 
         * 从asset路径下读取对应文件转String输出 
         * @param mContext 
         * @return 
         */  
        public static String getJson(Context mContext, String fileName) {  
            // TODO Auto-generated method stub  
            StringBuilder sb = new StringBuilder();  
            AssetManager am = mContext.getAssets();  
            try {  
                BufferedReader br = new BufferedReader(new InputStreamReader(  
                        am.open(fileName)));  
                String next = "";  
                while (null != (next = br.readLine())) {  
                    sb.append(next);  
                }  
            } catch (IOException e) {  
                // TODO Auto-generated catch block  
                e.printStackTrace();  
                sb.delete(0, sb.length());  
            }  
            return sb.toString().trim();  
        }  
    }  

```



### 三、Android Studio的org.json

- 解析原理：基于文档驱动

- 解析流程：把全部文件读入到内存中 ->> 遍历所有数据 ->> 根据需要检索想要的数据

```java
// 创建需解析的JSON数据：student.json
// 将该文件放入到本地assets文件夹里
{
"student":[
            {"id":1,"name":"小明","sex":"男","age":18,"height":175},
            {"id":2,"name":"小红","sex":"女","age":19,"height":165},
            {"id":3,"name":"小强","sex":"男","age":20,"height":185}
          ],
"cat":"it"
}

// 具体解析
public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        EntityStudent student = new EntityStudent();

        try {
            //从assets获取json文件
            InputStreamReader isr = new InputStreamReader(this.getClass().getClassLoader().getResourceAsStream("assets/" + "student.json"));
            //字节流转字符流
           BufferedReader bfr = new BufferedReader(isr);
            String line ;
            StringBuilder stringBuilder = new StringBuilder();
            while ((line = bfr.readLine())!=null){
                stringBuilder.append(line);
            }//将JSON数据转化为字符串
            JSONObject root = new JSONObject(stringBuilder.toString());
            //根据键名获取键值信息
            System.out.println("root:"+root.getString("cat"));
            JSONArray array = root.getJSONArray("student");
            for (int i = 0;i < array.length();i++)
            {
                JSONObject stud = array.getJSONObject(i);
                System.out.println("------------------");
                System.out.print("id="+stud.getInt("id")+ ","));
                System.out.print("name="+stud.getString("name")+ ","));
                System.out.print("sex="+stud.getString("sex")+ ","));
                System.out.print("age="+stud.getInt("age")+ ","));
                System.out.println("height="+stud.getInt("height")+ ","));
                bfr.close();
                isr.close();
                is.close();//依次关闭流
            }
        } catch (IOException e) {
            e.printStackTrace();
        } catch (JSONException e) {
            e.printStackTrace();
        }

    }
}
```



### 四、Gson解析

> `Google`的开源库

- 解析原理：基于事件驱动
- 解析流程：根据所需取的数据 建立1个对应于`JSON`数据的`JavaBean`类，即可通过简单操作解析出所需数据

 那么那么那么复杂，所以推荐使用GsonFormat插件

```java
public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // 1. 创建Gson对象
        Gson gson = new Gson();

        // 2. 创建JavaBean类的对象
        Student student = new EntityStudent();

        // 3. 使用Gson解析：将JSON数据转为单个类实体
        String json = "{\"id\":1,\"name\":\"小明\",\"sex\":\"男\",\"age\":18,\"height\":175}";
        student = gson.fromJson(json,Student.class);
        // 解析：JavaBean对象 = gson.fromJson(son,javaBean类类名.class);

        // 4. 调用student方法展示解析的数据
        student.show();
        
        // 5. 将Java集合转换为json
        String json2 = gson.toJson(List);        
        System.out.println(json2);
    }
}
```

  **@SerializedName 这个注解解决了我们 Model 和 JSON 不对应的问题** 

#### 1、Gson创建

```java
			Gson gson = new Gson();
			Gson gson = new GsonBuilder()
                        .setLenient()// json宽松  
                        .enableComplexMapKeySerialization()//支持Map的key为复杂对象的形式  
                        .serializeNulls() //智能null  
                        .setPrettyPrinting()// 调教格式  
                        .disableHtmlEscaping() //默认是GSON把HTML 转义的
                        .create();  
```

#### 2、JsonElement

> 表示JSON元素的类。它可能是一个JsonObject、JsonArray、JsonPrimitive、JsonNull
>
> 一个抽象类，是上面四个类的父类

- **JsonObject**

> 在JSON中表示对象类型的类。对象由名称-值对组成，其中名称是字符串，值是JsonElement的任何其他类型。这允许创建一个元素树。此对象的成员元素按添加顺序维护。

```java
//类定义
public final class JsonObject extends JsonElement {
  private final LinkedTreeMap<String, JsonElement> members =
      new LinkedTreeMap<String, JsonElement>();

//使用
JsonObject jsonObject=new JsonObject();
        jsonObject.addProperty("name","ck");
        System.out.println(jsonObject);
```

- **JsonArray**

> 在JSON中表示数组类型的类。数组是JsonElement的列表，每个可以是不同类型的。这是一个有序的列表，意思是添加的元素保留。

```java
//List 和 String 转 JsonArray
JsonArray jsonArray = new Gson().toJsonTree(userList, new TypeToken<List<User>>() {}.getType()).getAsJsonArray();
JsonArray jsonArray = new JsonParser().parse(string).getAsJsonArray();
//JsonArray 转 List
List<User> userList = gson.fromJson(jsonArray, new TypeToken<List<User>>(){}.getType()); 
```

- **Jsonprimitive**

> 表示JSON原始值的类。原始值要么是字符串，要么是Java primitive，要么是Java primitive的包装类型。
>
> jsonPrimitive可以帮助我们获取带转义字符的字符串，不怎么用到

- **JsonNull**

```java
JsonNull jsonNull=JsonNull.INSTANCE;
System.out.println(jsonNull);
//输出结果
null
```

#### 3、JsonParaser

> JsonParaser主要解析字符串通过parase(String json).getAsXXX方法得到不同的对象
>
> Gson负责对象与Json字符串之间的解析，功能强大复杂
>
> new Gson().toJsonTree会返回JsonElement

#### 4、JsonObject、JavaBean互转

```java
//JsonObject、JavaBean 互相转换
    User user = new Gson().fromJson(jsonObject, User.class);
    User user = new Gson().fromJson(string, User.class);

    JsonObject jsonObject = new Gson().toJsonTree(user).getAsJsonObject(); 
    JsonObject jsonObject = new JsonParser().parse(string).getAsJsonObject();
```

#### 4、JsonArray、List互相转换
```java
//JsonArray、List互相转换
    List<User> userList = gson.fromJson(string, new TypeToken<List<User>>() {}.getType()); 
    List<User> userList = gson.fromJson(jsonArray, new TypeToken<List<User>>(){}.getType()); 

    JsonArray jsonArray = new Gson().toJsonTree(userList, new TypeToken<List<User>>() {}.getType()).getAsJsonArray();
    JsonArray jsonArray = new JsonParser().parse(string).getAsJsonArray();
```

#### 6、Gson注解

- @SerializedName

- 过滤注解@Expose

  - 默认是既可以序列化，也可以反序列化。一定要配合GsonBuilder一起使用

    该注解是加在JavaBean的属性上使用的。配合这个使用哦Gson gson = new GsonBuilder().excludeFieldWithoutExposeAnnotation().create();

  - 有四种使用方式：

    　　1）不使用@Expose注解等同于@Expose(deserialize = false, serialize = false)不做任何解析

      　　2）@Expose(deserialize = true, serialize = false)只解析使用，可以反序列化，不可以序列化。

      　　3）@Expose(deserialize = false, serialize = true)可以序列化，不可以反序列化。

      　　4）@Expose(deserialize = false, serialize = true)既可以序列化，也可以反序列化。



### 链接

- [Github](https://github.com/google/gson)
- https://www.jianshu.com/p/0e40a52c0063

