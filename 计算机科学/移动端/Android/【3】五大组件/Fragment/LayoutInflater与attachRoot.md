[TOC]



Fragment中onCreatView方法Inflate的时候，要把最后一个参数设置成false

Fragment创建View的时候 `attachToRoot = true`，创建的时候会崩，报错`The specified child already has a parent. You must call removeView`

`attachToRoot = true`的时候，会自动`addview`到`parent`中

热心的FragmentManager帮我们把onCreatVIew的返回值addview了，所以两次addview直接导致异常



### 返回值【temp与root】

```java
Inflate(resId , null ) 只创建temp ,返回temp

Inflate(resId , parent, false )创建temp，然后执行temp.setLayoutParams(params);返回temp

Inflate(resId , parent, true ) 创建temp，然后执行root.addView(temp, params);最后返回root
//当attachToRoot = true的时候，Inflate方法返回的view不是你想要的view，而是那个ViewGroup
//@return里：The root View of the inflated hierarchy. If root was supplied and attachToRoot is true, this is root; otherwise it is the root of the inflated XML file.
```



### 被inflate的布局根布局参数问题

`inflate(int resource, ViewGroup root, boolean attachToRoot)`

- **第一个参数：**加载view的布局文件。

- **第二个参数：**
  - 参数为空的情况，xml里的根布局（也就是最外层布局），将不会被加载（xml文件的根布局会失效）；
  - 不为空的情况，系统会调用params = root.generateLayoutParams(attrs);//根据xml文件的宽，高 获取params， temp.setLayoutParams(params); 会把这个布局加载进去；也就是说xml文件的根布局不会失效；
  - **即root会协助linearlayout的根节点生成布局参数，只有这一个作用。**

- **第三个参数：**
  - **如果为true:**
    - 系统会调用：root.addView(temp, params); 会自动调用addview();
    - **并且返回root**
  - false：
    - 将不会执行root.addView(temp, params); 需要手动添加
    - **返回当前要加载的布局**



### root不为null，attachToRoot为true

当root不为null，attachToRoot为true时，表示将resource指定的布局添加到root中，添加的过程中resource所指定的的布局的根节点的各个属性都是有效的

注意：最终返回的是root而不是创建的布局



### root不为null，attachToRoot为false

如果root不为null，而attachToRoot为false的话，表示不将第一个参数所指定的View添加到root中，那么这个时候有的小伙伴可能就有疑问了，既然不添加到root中，那我还写这么多干嘛？我第二个参数直接给null不就可以了？其实不然，这里涉及到另外一个问题：**我们在开发的过程中给控件所指定的layout_width和layout_height到底是什么意思？该属性的表示一个控件在容器中的大小，就是说这个控件必须在容器中，这个属性才有意义，否则无意义。**这就意味着如果我直接将linearlayout加载进来而不给它指定一个父布局，则inflate布局的根节点的layout_width和layout_height属性将会失效（因为这个时候linearlayout将不处于任何容器中，那么它的根节点的宽高自然会失效）。如果我想让linearlayout的根节点有效，又不想让其处于某一个容器中，那我就可以设置root不为null，而attachToRoot为false。这样，指定root的目的也就很明确了，**即root会协助linearlayout的根节点生成布局参数，只有这一个作用。**



### root为null

当root为null时，不论attachToRoot为true还是为false，**显示效果都是一样的**。当root为null表示我不需要将第一个参数所指定的布局添加到任何容器中，同时也表示没有任何容器来来协助第一个参数所指定布局的根节点生成布局参数。

当第二个参数为null，第三个参数为false时（即使为true显示效果也是一样的，这里以false为例），由于在inflate方法中没有将linearlayout添加到某一个容器中，所以我需要手动添加，另外由于linearlayout并没有处于某一个容器中，**所以它的根节点的宽高属性会失效。**



- https://blog.csdn.net/qq_31180471/article/details/81234436

- [关于inflate的第3个参数](https://www.cnblogs.com/yuxing/archive/2012/02/18/2357740.html)

- [鸿洋blog](https://blog.csdn.net/lmj623565791/article/details/38171465)
- <https://www.jianshu.com/p/2989a927f5df>

- <https://blog.csdn.net/hzw19920329/article/details/51360982>