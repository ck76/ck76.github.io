### **一、概述**

Android开发的时候，我们时长遇到传递对象的需求，但是我们无法将对象的引用传给Activity或者Fragment，我们需要将这些对象放到一个Intent或者Bundle里面，然后再传递，这时候就用到了序列化，所谓序列化就是把Java对象转换为字节序列并存储至一个储存媒介的过程，反序列化就是把字节序列恢复为Java对象的过程。但是我们要知道序列化与反序列化仅处理Java变量而不处理方法，仅对数据进行处理。

### **二、序列化两种方式**

Android中序列化有两种方式：Serializable以及Parcelable。其中Serializable是Java自带的，而Parcelable是安卓专有的。关于二者区别我们最后会总结，先看看怎么使用吧。

### **三、Serializable方式序列化实例**

serializable使用比较简单，只需要对某个类实现Serializable 接口即可。

Serializable 接口是一种标识接口，某个类实现Serializable 接口，Java便会对这个对象进行序列化操作。

我们编写Person类：

```java
public class Person implements Serializable {

    private static final long serialVersionUID = -3139325922167935911L;
    //
    private int age;
    private String name;

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

}
```

很简单吧，无需过多解释。接下来我们就将这个类从一个Acticity传递到另一个Activity。

MainActivity:

```java
public class MainActivity extends Activity {

    //
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        //
        Person p = new Person();
        p.setAge(18);
        p.setName("ck");
        //
        Intent i = new Intent(this, SecondActivity.class);
        i.putExtra("person", p);
        startActivity(i);
        
    }
}
```

 SecondActivity:

```java
public class SecondActivity extends Activity {

    //

    private static final String TAG = "WL";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        Intent intent = getIntent();
        Person p=(Person) intent.getSerializableExtra("person");
        
        Log.i(TAG, "age = "+p.getAge());
        Log.i(TAG, "name = "+p.getName());
    }
}
```

以上代码及其简单了就不解释了，运行程序会看到如下打印：

```java
age=18;
name=ck;
```



以上就是Serializable方式序列化对象的举例，真的很简单，没有什么多余要解释的。

### **四、Parcelable方式序列化实例**

关与Parcelable方式实现序列化会比Serializable 方法麻烦一些，大体步骤如下：

\1. 实现Parcelable接口
\2. 覆写describeContents方法，默认返回0。
\3. 覆写writeToParcel(Parcel dest, int flags)方法，指定写入Parcel类的数据。
\4. 创建Parcelable.Creator静态对象，覆写方法createFromParcel(Parcel in)与newArray(int size)。
Person类：

```java
public class Person implements Parcelable {

    //
    private int age;
    private String name;
    private int weight;
    
    Person(){
        
    }
    
    Person(Parcel in){
        age = in.readInt();
        name = in.readString();
        weight = in.readInt();
    }


    //序列化时指定将哪些数据写入Parcel中，注意：写入顺序与读取顺序务必一致
    @Override
    public void writeToParcel(Parcel dest, int flags) {
        //
        dest.writeInt(age);
        dest.writeString(name);
        dest.writeInt(weight);
    }
    
    //这里一定要写上public关键字，我测试如果不写会报异常，此外名字不能改必须为：CREATOR
    public static final Parcelable.Creator<Person> CREATOR = new Creator<Person>() {
        
        @Override
        public Person[] newArray(int size) {
            //
            return new Person[size];
        }
        
        //反序列化时从Parcel中读取数据
        @Override
        public Person createFromParcel(Parcel source) {
            // 
            return new Person(source);
        }
    };

    @Override
    public int describeContents() {
        //默认返回0即可
        return 0;
    }

    //
    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getWeight() {
        return weight;
    }

    public void setWeight(int weight) {
        this.weight = weight;
    }
    
}
```

很多注意点都在注释中写出来了，仔细看注释即可。

Persons类：盛放Person类，主要演示如何序列化与反序列化List集合数据，这里似乎有点麻烦了，不过已经这样写了就这样举例吧。

```java
public class Persons implements Parcelable {

    private List<Person> mList;

    Persons() {

    }

    Persons(Parcel in) {
        
        this.mList = new ArrayList<Person>();
       in.readTypedList(mList, Person.CREATOR);
    }

    @Override
    public int describeContents() {
        //
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        //
        dest.writeTypedList(mList);
    }
    
    public static final Parcelable.Creator<Persons> CREATOR = new Creator<Persons>() {
        
        @Override
        public Persons[] newArray(int size) {
            //
            return new Persons[size];
        }
        
        @Override
        public Persons createFromParcel(Parcel source) {
            //
            return new Persons(source);
        }
    };
    

    public List<Person> getmList() {
        return mList;
    }

    public void setmList(List<Person> mList) {
        this.mList = mList;
    }

}
```


Persons主要演示如何反序列化集合类数据，如11,12行代码。

MainActivity:


```java
public class MainActivity extends Activity {

    //
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        //
        Person p1 = new Person();
        p1.setAge(18);
        p1.setName("ck1");
        p1.setWeight(130);
        //
        Person p2 = new Person();
        p2.setAge(28);
        p2.setName("ck2");
        p2.setWeight(125);
        //
        List<Person> mList = new ArrayList<Person>();
       mList.add(p1);
       mList.add(p2);
        //
        Persons mPersons = new Persons();
        mPersons.setmList(mList);
        //
        Intent i = new Intent(this, SecondActivity.class);
        i.putExtra("persons", mPersons);
        startActivity(i);
        
    }
}
```


也很简单吧，没什么要特别说明的。

SecondActivity：


```java
public class SecondActivity extends Activity {

    //

    private static final String TAG = "WL";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        Intent intent = getIntent();
        Persons ps = (Persons) intent.getParcelableExtra("persons");
        List<Person> pList = ps.getmList();
        for (int i = 0; i < pList.size(); i++) {
            Person p = pList.get(i);
            Log.i(TAG, "age = " + p.getAge());
            Log.i(TAG, "name = " + p.getName());
            Log.i(TAG, "weight = " + p.getWeight());
        }

    }
}
```

主要就是通过intent.getParcelableExtra获取序列化的对象，也很简单。

运行程序结果如下：

```java
    age=18
    name=ck1
    weight=130
    age=28
    name=ck2
    weight=125
```



数据传递成功，如果仔细看上面例子应该对Parcelable方式实现序列化有了一定的了解，貌似写起来会比**Serializable**方式复杂一些，很多都是模板代码，照着写就是了，那这两种方式有什么区别呢？

### **五、两种序列化方式区别**

两者区别在于存储媒介的不同。

Serializable使用IO读写存储在硬盘上。序列化过程使用了反射技术，并且期间产生临时对象。优点代码少。

Parcelable是直接在内存中读写，我们知道内存的读写速度肯定优于硬盘读写速度，所以Parcelable序列化方式性能上要优于Serializable方式很多。但是代码写起来相比Serializable方式麻烦一些。

通过比较发现，性能与简便我们只能选其一，大多数情况下使用Serializable也是没什么问题的，但是还是建议大家使用Parcelable方式实现序列化，毕竟性能好很多，其实也没多麻烦。