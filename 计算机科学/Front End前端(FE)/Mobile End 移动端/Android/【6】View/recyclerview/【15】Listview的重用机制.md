

[TOC]

>  核心思想：设置初始值，设置标识



**ListView优化主要有下面几个方面：**

**1、convertView重用**

**2、ViewHolder的子View复用**

**3、缓存数据复用**



### 原因分析

Listview item 缓存机制：为了使得性能更优，Listview会缓存行item(某行对应的view)。listview通过adapter的getview函数获得每行的item。滑动过程中，

a、如果某行item已经划出屏幕，若该item不在缓存内，则put进缓存，否则更新缓存；

b、获取滑入屏幕的行item之前会先判断缓存中是否有可用的item，如果有，作为convertview参数传递给adapter的getview。

这样的话如下的getview写法就可以充分利用缓存大大提升listview的性能。即便上万个行item，最多inflate的次数为n，n为一屏最多显示listview行item的个数。

```java
@Override
    public View getView(int position, View convertView, ViewGroup parent) {
        ViewHolder holder;
        if (convertView == null) {
            convertView = inflater.inflate(R.layout.list_item, null);
            holder = new ViewHolder();
						....convertView.setTag(holder);
        } else {
            holder = (ViewHolder) convertView.getTag();
        }
    }
```

这样提升了性能，但同时造成了一些问题。



### 1、convertView重用

首先讲下ListView的原理：ListView中的每一个Item显示都需要Adapter调用一次getView()的方法，这个方法会传入一个convertView的参数，这个方法返回的View就是这个Item显示的View。如果当Item的数量足够大，再为每一个Item都创建一个View对象，必将占用很多内存空间，即创建View对象（mInflater.inflate(R.layout.lv_item, null);从xml中生成View，这是属于IO操作）是耗时操作，所以必将影响性能。Android提供了一个叫做Recycler(反复循环)的构件，就是当ListView的Item从滚出屏幕视角之外，对应Item的View会被缓存到Recycler中，相应的会从生成一个Item，而此时调用的getView中的convertView参数就是滚出屏幕的缓存Item的View，所以说如果能重用这个convertView，就会大大改善性能。

ListView内部为了优化而建立的复用机制,在下面方法中第二个参数就是ListView传递给你,让你进行复用的View.如果你不想复用listview传递给你的View,那你每次都需要创建一个新的View进行返回,这样子是肯定不会出现复用问题的,但是性能却是很消耗的。

```java
 		   /////基于引用的操作
            @NonNull
            @Override
            public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
                ViewHolder viewHolder;
                if (convertView == null) {   //判空
                    viewHolder = new ViewHolder();  //实例化viewholder
                    convertView = LayoutInflater.from(getContext()).inflate(android.R.layout.itemXXX, null);
                   
                    viewHolder.ck=convertView.findViewById(R.id.img_ck);
                    
                    convertView.setTag(viewHolder); //给convertview设置tag
                } else {
                    viewHolder = (ViewHolder)convertView.getTag();  //获取ViewHolder
                }
 					viewHolder.ck.setBackgroundResource(R.drawable.ck);  //重用Item的子View控件对象
                return convertView;
            }

private final class ViewHolder{
    ImageView ck;
}
```



### 2、再说一下setTag()和getTag()方法

#### 1.用于区分非常多类似的View

比如：

```
button1.setOnClickListener(new OnClickListener ... );
button2.setOnClickListener(new OnClickListener ... );
```

它们可能运行类似的逻辑，但你必须分别为两个Button设置两个独立的OnClick事件，

```java
public void onClick(View v) {
    doAction(1); // 1 for button1, 2 for button2, etc.
}
```

之所以这样做。由于onClick仅仅有一个參数View。我们能够通过setTag和getTag来完毕

```java
button1.setTag(1);
button2.setTag(2);
```

我们能够将两个button设置同一个OnClickListener，比方：

```java
listener = new OnClickListener() {
    @Override
    public void onClick(View v) {
        doAction(v.getTag());
    }
};
```

这样，就能够通过getTag区分。



#### 2.用于ListView的复用

我们自己写自己定义adapter的时候，一般会使用它。比方：

```java
static class ViewHolder {
    TextView tvPost;
    TextView tvDate;
    ImageView thumb;
}

public View getView(int position, View convertView, ViewGroup parent) {
	ViewHolder viewholder;
    if (convertView == null) {
        LayoutInflater inflater = myContext.getLayoutInflater();
        convertView = inflater.inflate(R.layout.postitem, null);

        viewholder = new ViewHolder();
        viewholder.tvPost = (TextView)convertView.findViewById(R.id.postTitleLabel);
        viewholder.tvDate = (TextView)convertView.findViewById(R.id.postDateLabel);
        vh.thumb = (ImageView)convertView.findViewById(R.id.postThumb);
        convertView.setTag(viewholder);
    } else{
        viewHolder = (ViewHolder)convertView.getTag();
    }
    
    //对viewholder的操作
    
    return convertview;
            ....................
}
```

#### 3.注意：

除了上述情况以外，我们尽量不要直接使用，原因：

1.代码可读性：会给其它的程序猿造成困扰

2.由于setTag和getTag设置的是一个Object对象。可能会出现类的转换异常

只是，android4.0以后。有一个更好的方法：setTag(int key, Object tag)能够通过类似<k,v>键值对的方式存取。