[TOC]

# 前言

- `ListView`在`Android`开发中十分常见
- 今天，我将为大家带来`ListView`与`AdapterView`全面解析，含其特点、工作原理等，希望你们会喜欢。

------

# 目录

<img src="https://tva1.sinaimg.cn/large/008eGmZEly1gmwfy8xq2ij30m20quweq.jpg" alt="img" style="zoom:50%;" />

示意图

------

# 1. 简介

- `Android`中的一种列表视图组件
- 继承自`AdapterView`抽象类，类图关系如下

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfy70cvtj30ic0cajsv.jpg)

示意图

------

# 2. 作用

集合多个 “项”（称为：`Item`） & 以列表的形式 展示

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfy639p3j30h80fuwj2.jpg)

示意图

------

# 3. 工作原理

### 3.1 本质原理

- `ListView`仅作为容器（列表），用于装载 & 显示数据（即 列表项`Item`）
- 而容器内的具体数据（列表项`Item`）则是由 适配器（`Adapter`）提供

> 适配器（Adapter）：作为`View` 和 数据之间的桥梁 & 中介，将数据映射到要展示的`View`中

- 当需显示数据时，`ListView`会向`Adapter`取出数据，从而加载显示，具体如下图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfy48l66j30ku08m3z2.jpg)

示意图

- 结论
  `ListView`负责以列表的形式显示`Adapter`提供的内容

### 3.2 缓存原理

> 试想一个场景：若把所有数据集合的信息都加载到`ListView`上显示，==若 `ListView`要为每个数据都创建一个视图，那么会占用非常多的内存==

- 为了节省空间和时间，`ListView`不会为每一个数据创建一个视图，而是采用了**Recycler组件**，用于回收 & 复用 `View`
- 当屏幕需显示`x`个`Item`时，那么`ListView`会创建 `x+1`个视图；当第1个`Item`离开屏幕时，此`Item`的`View`被回收至==缓存==，入屏的`Item`的`View`会优先==从该缓存中获取==

> 注：
>
> 1. 只有`Item`完全离开屏幕后才可复用，这也是为什么`ListView`要创建比屏幕需显示视图多1个的原因：缓冲 显示视图
> 2. 即：第1个`Item`离开屏幕是有过程的，会有1个 **第1个`Item`的下半部分 & 第8个`Item`上半部分同时在屏幕中显示**的状态，此时仍无法使用缓存的`View`，只能继续用新创建的视图`View`

- 实例演示
  设：屏幕只能显示5个`Item`，那么`ListView`只会创建（5+1）个`Item`的视图；当第1个`Item`完全离开屏幕后才会回收至缓存从而复用（用于显示第7个`Item`）

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfy20druj30j10wywfr.jpg)

示意图

------

# 4. 具体使用

**1. 生成方式**

生成列表视图（ListView）的方式主要有两种：

- 直接用ListView进行创建
- 让Activity继承ListActivity

**2. xml文件配置信息**



```xml
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"   
    xmlns:tools="http://schemas.android.com/tools"   
    android:layout_width="match_parent"   
    android:layout_height="match_parent"   
    android:background="#FFE1FF"   
    android:orientation="vertical" >   
    <ListView   
        android:id="@+id/listView1"   
        android:layout_width="match_parent"   
        android:layout_height="match_parent" />   
</LinearLayout>  
```

AbsListView的常用属性和相关方法：

| 属性                       |                             说明                             |                                                         备注 |
| -------------------------- | :----------------------------------------------------------: | -----------------------------------------------------------: |
| android:choiceMode         |            列表的选择行为，默认：none没有选择行为            | 选择方式： none：不显示任何选中项 singleChoice：允许单选multipleChoice：允许多选multipleChoiceModal：允许多选 （把Activity里面adapter的第二个参数改成支持选择的布局） |
| android:drawSelectorOnTop  |                                                              |             如果该属性设置为true，选中的列表项将会显示在上面 |
| android:listSelector       |                    为点击到的Item设置图片                    |             如果该属性设置为true，选中的列表项将会显示在上面 |
| android：fastScrollEnabled |                     设置是否允许快速滚动                     | 如果该属性设置为true，将会显示滚动图标，并允许用户拖动该滚动图标进行快速滚动。 |
| android：listSelector      |              指定被选中的列表项上绘制的Drawable              |                                                              |
| android：scrollingCache    |                      滚动时是否使用缓存                      |                       如果设置为true，则在滚动时将会使用缓存 |
| android：stackFromBottom   |                 设置是否从底端开始排列列表项                 |                                                              |
| android：transcriptMode    | 指定列表添加新的选项的时候，是否自动滑动到底部，显示新的选项。 | disabled：取消transcriptMode模式；默认的normal：当接受到数据集合改变的通知，并且仅仅当最后一个选项已经显示在屏幕的时候，自动滑动到底部。 alwaysScroll：无论当前列表显示什么选项，列表将会自动滑动到底部显示最新的选项。 |

Listview提供的XML属性：

| XML属性                       |                             说明                             |                                      备注 |
| ----------------------------- | :----------------------------------------------------------: | ----------------------------------------: |
| android:divider               | 设置List列表项的分隔条（可用颜色分割，也可用图片（Drawable）分割 | 不设置列表之间的分割线，可设置属性为@null |
| android:dividerHeight         |                     用于设置分隔条的高度                     |                                           |
| android:background属性        |                        设置列表的背景                        |                                           |
| android：entries              |   指定一个数组资源，Android将根据该数组资源来生成ListView    |                                           |
| android：footerDividerEnabled |       如果设置成false，则不在footer View之前绘制分隔条       |                                           |
| andorid：headerDividerEnabled |       如果设置成false，则不再header View之前绘制分隔条       |                                           |

------

# 5. Adapter简介

Adapter本身是一个接口，Adapter接口及其子类的继承关系如下图：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfxx8pznj30ho0f40ug.jpg)

Adapter接口及其子类的继承关系.png

- Adapter接口派生了ListAdapter和SpinnerAdapter两个子接口

> 其中ListAdapter为AbsAdapter提供列表项，而SpinnerAdapter为AbsSpinner提供列表项

- ArrayAdapter、SimpleAdapter、SimpleCursorAdapter、BaseAdapter都是常用的实现适配器的类

> 1. ArrayAdapter：简单、易用的Adapter，用于将数组绑定为列表项的数据源，支持泛型操作
> 2. SimpleAdapter：功能强大的Adapter，用于将XML中控件绑定为列表项的数据源
> 3. SimpleCursorAdapter：与SimpleAdapter类似，用于绑定游标（直接从数据数取出数据）作为列表项的数据源
> 4. BaseAdapter：可自定义ListView，通用用于被扩展。扩展BaseAdapter可以对各个列表项进行最大程度的定制。

------

# 6. 常用适配器介绍

### 6.1 ArrayAdapter

**定义**
简单、易用的Adapter，用于将数组绑定为列表项的数据源，支持泛型操作

**步骤**
**1. 在xml文件布局上实现ListView**



```xml
<?xml version="1.0" encoding="utf-8"?>
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"    xmlns:tools="http://schemas.android.com/tools"    
android:layout_width="match_parent"    
android:layout_height="match_parent"    
android:paddingBottom="@dimen/activity_vertical_margin"    
android:paddingLeft="@dimen/activity_horizontal_margin"    
android:paddingRight="@dimen/activity_horizontal_margin"    
android:paddingTop="@dimen/activity_vertical_margin"    
tools:context="com.example.carson_ho.adapte_demo.MainActivity">   
 <ListView        
  android:id="@+id/list_item"        
  android:layout_width="match_parent"        
  android:layout_height="match_parent"        
  android:divider="#f00"        
  android:dividerHeight="1sp"        
  android:headerDividersEnabled="false">        
</ListView>
</RelativeLayout>
```

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfxuuzxaj30b20eut9t.jpg)

效果图.png

**2. 在MainActivity上定义一个链表，将所要展示的数据以存放在里面**
**3. 构造ArrayAdapter对象，设置适配器**
**4. 将LsitView绑定到ArrayAdapter上**
如下图：



```dart
public class MainActivity extends AppCompatActivity {     


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

       ListView listView = (ListView) findViewById(R.id.list_item);
        //定义一个链表用于存放要显示的数据
        final List<String> adapterData = new ArrayList<String>();
        //存放要显示的数据
        for (int i = 0; i < 20; i++) {
            adapterData.add("ListItem" + i);
        }
        //创建ArrayAdapter对象adapter并设置适配器
         ArrayAdapter<String> adapter = new ArrayAdapter<String>(this,
                android.R.layout.simple_list_item_1, adapterData);
        //将LsitView绑定到ArrayAdapter上
        listView.setAdapter(adapter);
    }
}
```

> 创建ArrayAdapter对象要指定三个参数:

- context：代表方位Android应用的接口
- textViewRseourceld：资源ID，代表一个TextView
- 数组：列表项展示的数据

**5. 在xml文件布局添加资源文件TextView,该TextView组件将作列表项的组件**



```xml
<?xml version="1.0" encoding="utf-8"?>

<TextView xmlns:android="http://schemas.android.com/apk/res/android"    
android:layout_width="match_parent"    
android:layout_height="wrap_content">
android:textSize="24sp"
</TextView>
```

### 最终效果图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfxt3379j30la0ewglr.jpg)

最终效果图.png

**缺点**
ArrayAdapter较为简单，易用，但每个列表项只能是TextView，功能实现的局限性非常大。

### 6.2 SimpleAdapter

- **定义**：功能强大的Adapter，用于将XML中控件绑定作为列表项的数据源
- **特点**：可对每个列表项进行定制（自定义布局），能满足大多数开发的需求场景，灵活性较大

**步骤**
**1. 在xml文件布局上实现ListView**



```xml
<?xml version="1.0" encoding="utf-8"?>
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"    
xmlns:tools="http://schemas.android.com/tools"    
android:layout_width="match_parent"    
android:layout_height="match_parent"    
android:paddingBottom="@dimen/activity_vertical_margin"    
android:paddingLeft="@dimen/activity_horizontal_margin"    
android:paddingRight="@dimen/activity_horizontal_margin"    
android:paddingTop="@dimen/activity_vertical_margin"    
tools:context="com.example.carson_ho.adapte_demo.MainActivity">   
 <ListView        
  android:id="@+id/list_item"        
  android:layout_width="match_parent"        
  android:layout_height="match_parent"        
  android:divider="#f00"        
  android:dividerHeight="1sp"        
  android:headerDividersEnabled="false">        
</ListView>
</RelativeLayout>
```

**2. 根据实际需求定制列表项：实现ListView每行的xml布局（即item布局）**



```xml
<?xml version="1.0" encoding="utf-8"?>
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"    
android:layout_width="match_parent"    
android:layout_height="match_parent">        

<TextView            
android:id="@+id/name"            
android:layout_width="wrap_content"            
android:layout_height="wrap_content"            
android:textSize="17sp"            
android:paddingLeft="14dp"/>        
<TextView            
android:id="@+id/address"            
android:layout_below="@id/name"            
android:textSize="17sp"            
android:layout_width="wrap_content"            
android:layout_height="wrap_content" />        
<TextView            
android:id="@+id/lowerest_wholesale"            
android:layout_toRightOf="@id/address"            
android:textSize="17sp"            
android:layout_width="wrap_content"            
android:layout_height="wrap_content" />        
<TextView            
android:id="@+id/price"            
android:textSize="17sp"            
android:layout_below="@id/address"            
android:layout_width="wrap_content"            
android:layout_height="wrap_content" />        
<ImageView            
android:id="@+id/picture"            
android:layout_alignParentRight="true"            
android:layout_width="115dp"            
android:layout_height="86dp"         />        
</RelativeLayout>
```

**3. 定义一个HashMap构成的列表以键值对的方式存放数据**
**4. 构造SimpleAdapter对象，设置适配器**
**5. 将LsitView绑定到SimpleAdapter上**



```cpp
public class MainActivity extends AppCompatActivity {
//定义数组以填充数据
    private String[] name=new String[]{            
"威龙注塑机","霸龙注塑机","恐龙注塑机"    };    
    private String[] address =new String[]{        
"广东","北京","黑龙江"    };    
    private int[] lowerest_wholesale =new int[]{            
11,22,33    };    
    private int[] price =new int[]{            
11,22,33    };    
    private int[] picture =new int[]{
            R.drawable.home_selected,
            R.drawable.home_selected, 
            R.drawable.home_selected   };    

@Override    
protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);   
         setContentView(R.layout.activity_main);

//定义一个HashMap构成的列表以键值对的方式存放数据
ArrayList<HashMap<String, Object>> listItem = new ArrayList<HashMap<String,Object>>();        
//循环填充数据   
for(int i=0;i<name.length;i++)        { 
HashMap<String,Object> map = new HashMap<String,Object>();            
map.put("name", name[i]);            
map.put("address", address[i]);            
map.put("lowerest_wholesale", lowerest_wholesale[i]);            
map.put("price", price[i]);            
map.put("picture", picture[i]);            
listItem.add(map);       
 }        

//构造SimpleAdapter对象，设置适配器        
SimpleAdapter mSimpleAdapter = new SimpleAdapter(this,
listItem,//需要绑定的数据                
R.layout.item_imformation,//每一行的布局                
new String[] {"name","address", "lowerest_wholesale","price","picture"},
//数组中的数据源的键对应到定义布局的View中                
new int[] {R.id.name,R.id.address,R.id.lowerest_wholesale,R.id.price,R.id.picture});        
ListView list= (ListView) findViewById(R.id.list_item);        
//为ListView绑定适配器        
list.setAdapter(mSimpleAdapter);   
   }
}
```

#### 结果显示

<img src="https://tva1.sinaimg.cn/large/008eGmZEly1gmwfxq7476j30lg10eta7.jpg" alt="img" style="zoom:33%;" />

结果显示

### 6.3 BaseAdapter

**定义**
可自定义ListView，通用用于被扩展。扩展BaseAdapter可以对各个列表项进行最大程度的定制

**使用步骤：**

1. 定义主xml布局
2. 根据需要定义ListView每行所实现的xml布局
3. 定义一个Adapter类继承BaseAdapter，重写里面的方法。
4. 定义一个HashMap构成的列表，将数据以键值对的方式存放在里面。
5. 构造Adapter对象，设置适配器。
6. 将LsitView绑定到Adapter上。

**先定义一个Adapter类继承BaseAdapter，并重写里面的方法**

> 使用BaseAdapter必须写一个类继承它，同时BaseAdapter是一个抽象类，继承它必须实现它的方法。



```java
class MyAdapter extends BaseAdapter {
    private LayoutInflater mInflater;//得到一个LayoutInfalter对象用来导入布局

 //构造函数
    public MyAdapter(Context context,ArrayList<HashMap<String, Object>> listItem) {
        this.mInflater = LayoutInflater.from(context);
        this.listItem = listItem;
    }//声明构造函数

    @Override
    public int getCount() {
        return listItem.size();
    }//这个方法返回了在适配器中所代表的数据集合的条目数

    @Override
    public Object getItem(int position) {
        return listItem.get(position);
    }//这个方法返回了数据集合中与指定索引position对应的数据项

    @Override
    public long getItemId(int position) {
        return position;
    }//这个方法返回了在列表中与指定索引对应的行id


    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        return null;
    }//这个方法返回了指定索引对应的数据项的视图，还没写完
}
```

**这里主要讲一下BaseAdapter里必须要重写的4个方法**

- BaseAdapter的灵活性就在于它要重写很多方法，其中最重要的即为getView()方法。
- 我们结合上述重写的4个方法了解**ListView的绘制过程：**

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfxob6gaj30xc0b10uw.jpg)

示意图

其中，重点讲解重写的`getView（）`方式，总共有3种



```dart
/**
  * 重写方式1：直接返回了指定索引对应的数据项的视图
  */
    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View item = mInflater.inflate(R.layout.item,null);
        ImageView img = (ImageView)item.findViewById(R.id.ItemImage);
        TextView title = (TextView)item.findViewById(R.id.ItemTitle);
        TextView test = (TextView)item.findViewById(R.id.ItemText);
        Button btn = (Button) item.findViewById(R.id.ItemBottom);
        img.setImageResource((Integer) listItem.get(position).get("ItemImage"));
        title.setText((String) listItem.get(position).get("ItemTitle"));
        test.setText((String) listItem.get(position).get("ItemText"));

        return item;
    }
    // 缺点：
    // 每次调用getView()时，都要重新通过 findViewById（） 寻找View组件 & 重新绘制View
    // 当列表项数据量很大时会严重影响性能，即体现为下拉很慢、卡

/**
  * 重写方式2：使用convertView作为View缓存（优化）
  * 具体原理：
  *       // a. 将 convertView作为getView（）的输入参数 & 返回参数，从而形成反馈
  *       // b. 形成了Adapter的itemView重用机制，减少了重绘View的次数
  */
     @Override
    public View getView(int position, View convertView, ViewGroup parent) {

        // 检测有无可重用的View，若无就重新绘制
        if(convertView == null)
        {
            convertView = mInflater.inflate(R.layout.item, null);
        }
        ImageView img = (ImageView)convertView.findViewById(R.id.ItemImage);
        TextView title = (TextView)convertView.findViewById(R.id.ItemTitle);
        TextView test = (TextView)convertView.findViewById(R.id.ItemText);
        Button btn = (Button) convertView.findViewById(R.id.ItemBottom);
        img.setImageResource((Integer) listItem.get(position).get("ItemImage"));
        title.setText((String) listItem.get(position).get("ItemTitle"));
        test.setText((String) listItem.get(position).get("ItemText"));

        return convertView;
        // 最终返回convertView形成反馈
    }

    // 优点：减少了重绘View的次数
    // 缺点：但是每次都要通过 findViewById（） 寻找View组件

/**
  * 重写方式3：在方式2的基础上，使用ViewHolder实现更加具体的缓存：View组件缓存
  * 具体原理：
  *       // a. 将 convertView作为getView（）的输入参数 & 返回参数，从而形成反馈
  *       // b. 形成了Adapter的itemView重用机制，减少了重绘View的次数
  */
    static class ViewHolder

        {
            public ImageView img;
            public TextView title;
            public TextView text;
            public Button btn;
        }

    @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            ViewHolder holder ;
            if(convertView == null)
            {
                holder = new ViewHolder();
                convertView = mInflater.inflate(R.layout.item, null);
                holder.img = (ImageView)convertView.findViewById(R.id.ItemImage);
                holder.title = (TextView)convertView.findViewById(R.id.ItemTitle);
                holder.text = (TextView)convertView.findViewById(R.id.ItemText);
                holder.btn = (Button) convertView.findViewById(R.id.ItemBottom);
                convertView.setTag(holder);
            }
            else {
                holder = (ViewHolder)convertView.getTag();

            }
            holder.img.setImageResource((Integer) listItem.get(position).get("ItemImage"));
            holder.title.setText((String) listItem.get(position).get("ItemTitle"));
            holder.text.setText((String) listItem.get(position).get("ItemText"));

            return convertView;
        }
  // 优点：重用View时就不用通过 findViewById（）重新 寻找View组件，同时也减少了重绘View的次数，是ListView使用的最优化方案
```

- 方案3（通过`convertView+ViewHolder`重写`getView（）`）是`ListView`使用的最优化，所以非常推荐大家使用
- 总结：ListView的优化

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfxm0bxwj30xc0c177g.jpg)

示意图

- 最优化方案的完整实现方案

1. 定义主xml的布局
   *activity_main.xml:*



```xml
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:background="#FFFFFF"
    android:orientation="vertical" >
    <ListView
        android:id="@+id/listView1"
        android:layout_width="match_parent"
        android:layout_height="match_parent" />
</LinearLayout>
```

1. 根据需要，定义ListView每行所实现的xml布局（item布局）
   *item.xml:*



```xml
<?xml version="1.0" encoding="utf-8"?>
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
android:layout_width="match_parent" 
android:layout_height="match_parent">
    <ImageView
        android:layout_alignParentRight="true"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:id="@+id/ItemImage"/>
    <Button
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:text="按钮"
        android:id="@+id/ItemBottom"
        android:focusable="false"
        android:layout_toLeftOf="@+id/ItemImage" />
    <TextView android:id="@+id/ItemTitle"
        android:layout_height="wrap_content"
        android:layout_width="fill_parent"
        android:textSize="20sp"/>
    <TextView android:id="@+id/ItemText"
        android:layout_height="wrap_content"
        android:layout_width="fill_parent"
        android:layout_below="@+id/ItemTitle"/>
</RelativeLayout>
```

1. 定义一个Adapter类继承BaseAdapter，重写里面的方法。

> （利用convertView+ViewHolder来重写getView()）

*MyAdapter.java*



```java
package scut.learnlistview;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.HashMap;

class MyAdapter extends BaseAdapter {
    private LayoutInflater mInflater;//得到一个LayoutInfalter对象用来导入布局 
    ArrayList<HashMap<String, Object>> listItem;

    public MyAdapter(Context context,ArrayList<HashMap<String, Object>> listItem) {
        this.mInflater = LayoutInflater.from(context);
        this.listItem = listItem;
    }//声明构造函数

    @Override
    public int getCount() {
        return listItem.size();
    }//这个方法返回了在适配器中所代表的数据集合的条目数

    @Override
    public Object getItem(int position) {
        return listItem.get(position);
    }//这个方法返回了数据集合中与指定索引position对应的数据项

    @Override
    public long getItemId(int position) {
        return position;
    }//这个方法返回了在列表中与指定索引对应的行id

//利用convertView+ViewHolder来重写getView()
    static class ViewHolder
    {
        public ImageView img;
        public TextView title;
        public TextView text;
        public Button btn;
    }//声明一个外部静态类
    @Override
    public View getView(final int position, View convertView, final ViewGroup parent) {
        ViewHolder holder ;
        if(convertView == null)
        {
            holder = new ViewHolder();
            convertView = mInflater.inflate(R.layout.item, null);
            holder.img = (ImageView)convertView.findViewById(R.id.ItemImage);
            holder.title = (TextView)convertView.findViewById(R.id.ItemTitle);
            holder.text = (TextView)convertView.findViewById(R.id.ItemText);
            holder.btn = (Button) convertView.findViewById(R.id.ItemBottom);
            convertView.setTag(holder);
        }
        else {
            holder = (ViewHolder)convertView.getTag();

        }
        holder.img.setImageResource((Integer) listItem.get(position).get("ItemImage"));
        holder.title.setText((String) listItem.get(position).get("ItemTitle"));
        holder.text.setText((String) listItem.get(position).get("ItemText"));
        holder.btn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                System.out.println("你点击了选项"+position);//bottom会覆盖item的焦点，所以要在xml里面配置android:focusable="false"
            }
        });

        return convertView;
    }//这个方法返回了指定索引对应的数据项的视图
}
```

4.在MainActivity里：

- 定义一个HashMap构成的列表，将数据以键值对的方式存放在里面。
- 构造Adapter对象，设置适配器。
- 将LsitView绑定到Adapter上。

*MainActivity.java*



```dart
package scut.learnlistview;

import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.SimpleAdapter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class MainActivity extends AppCompatActivity {
    private ListView lv;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        lv = (ListView) findViewById(R.id.listView1);
        /*定义一个以HashMap为内容的动态数组*/
        ArrayList<HashMap<String, Object>> listItem = new ArrayList<HashMap<String, Object>>();/*在数组中存放数据*/
        for (int i = 0; i < 100; i++) {
            HashMap<String, Object> map = new HashMap<String, Object>();
            map.put("ItemImage", R.mipmap.ic_launcher);//加入图片
            map.put("ItemTitle", "第" + i + "行");
            map.put("ItemText", "这是第" + i + "行");
            listItem.add(map);
        }
        MyAdapter adapter = new MyAdapter(this, listItem);
        lv.setAdapter(adapter);//为ListView绑定适配器

        lv.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> arg0, View arg1, int arg2, long arg3) {
                System.out.println("你点击了第" + arg2 + "行");//设置系统输出点击的行
            }
        });

}
}
```

### 运行结果

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfxfx37cj30d80k8acf.jpg)



点击输出结果：



![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfxip9pjj308l024741.jpg)

------

# 注：进阶使用 = 添加头部 & 尾部View

在日常使用中，我们常常会需要在`ListView`头部 / 尾部添加视图

#### 步骤1：添加头部 / 尾部视图

*header_view.xml*



```xml
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="70dp"
    android:orientation="vertical">

    <TextView
        android:layout_width="match_parent"
        android:layout_height="50dp"
        android:text="header"
        android:textSize="20dp"
        android:gravity="center"/>

</LinearLayout>
```

#### 步骤2：添加到ListView中



```csharp
private ListView lv;

View view = LayoutInflater.from(MainActivity.this).inflate(R.layout.header_view, null);

lv.addHeaderView(view);
// lv.addFooterView(view); // 添加到底部View
```

### 示意图

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfxe1lgrj30f20meq55.jpg)

------

# 7. 与RecylerView的区别

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmwfxcqbj9j30m807s0u2.jpg)

image.png

------

# 8. 总结

- 本文全面介绍了 `ListView` 与 `AdapterView`
- 接下来我会介绍继续介绍Android开发中的相关知识，感兴趣的同学可以继续关注本人运营的`Wechat Public Account`：
- [我想给你们介绍一个与众不同的Android微信公众号（福利回赠）](https://www.jianshu.com/p/2e92908af6ec)
- [我想邀请您和我一起写Android（福利回赠）](https://www.jianshu.com/p/2c5d57fb054d)

------

# 请点赞！因为你们的鼓励是我写作的最大动力！

> **相关文章阅读**
> [Android开发：最全面、最易懂的Android屏幕适配解决方案](https://www.jianshu.com/p/ec5a1a30694b)
> [Android事件分发机制详解：史上最全面、最易懂](https://www.jianshu.com/p/38015afcdb58)
> [Android开发：史上最全的Android消息推送解决方案](https://www.jianshu.com/p/b61a49e0279f)
> [Android开发：最全面、最易懂的Webview详解](https://www.jianshu.com/p/3c94ae673e2a)
> [Android开发：JSON简介及最全面解析方法!](https://www.jianshu.com/p/b87fee2f7a23)
> [Android四大组件：Service服务史上最全面解析](https://www.jianshu.com/p/d963c55c3ab9)
> [Android四大组件：BroadcastReceiver史上最全面解析](https://www.jianshu.com/p/ca3d87a4cdf3)



作者：Carson_Ho
链接：https://www.jianshu.com/p/4e8e4fd13cf7
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。