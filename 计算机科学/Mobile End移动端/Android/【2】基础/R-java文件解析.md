[TOC]

 android开发图的两个R文件具体含义： 就是系统给声明的静态变量，在资源文件res.下面，每一个文件夹中的xml.文件就是一个R.java中声明的一个静态类。每一个xml文件中的变量就是该xml对应的类的一个属性常量值。这些都是系统生成的。 

![R.java文件位置](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/zYZnu6mGRiOgjQlQHqgLMvkINnVKKXGstcGoM24Vhrk!/r/dAgBAAAAAAAA)

<!--more-->

- 资源文件只能以小写字母和**下划线**做首字母，随后的名字中只能出现**[a-z0-9_]** 这些字符，否则**R.java**文件不会自动更新.
- 当开发者在res/目录中任何一个子目录中添加相应类型的文件之后，**ADT**会在**R.java**文件中相应内部类中自动生成一条静态**int**类型的常量，对添加的文件进行索引
- 如果在layout目录下添加一个新界面，那么在**public static final class layout** 中也会添加相应的静态**int**常量
- 相反，我们再res目录下删除任何一个文件，其在**R.java**文件中对应的记录会被**ADT**自动删除
- R.java文件除了自动标示资源的索引功能外，如果**res**目录中的某个资源在应用中没有被使用到，在该应用被编译的时候系统就不会把对应的资源编译到该应用的**apk**包中，节省资源

 ![R文件的内部类](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/cJPofyjZUgpAHkDZKfUDjOexLFoAW*4mQ5FaIciDT6c!/r/dDABAAAAAAAA)

**通过R文件来引用所需要的资源：**

在 java 程序中应用资源

1. 在java程序中应用资源
   - 按照java的语法来引用即 **R.resource_type.resource_name**
   - 注意：resource_name 不需要文件的后缀名
   - Android系统本身自带了很多资源可以引用，只是需要在前面加上 **Android.** 以申明来自**Android**系统，即**Android.R.resource_type.resource_name**
2. 在xml文件中引用资源
   - 在xml文件中一般是通过**@drawable/icon**的方式获取的，其中**@**代表**R.java**类，**drawable**代表的是**R.java**中的静态内部类**drawable**,**/icon**代表静态内部类**drawable**中的静态属性**icon**
   - 如果访问的是Android系统自带的文件，则要添加**Android：**，如下:
   - 在布局文件中当我们需要为一些组件添加Id属性作为标识**@+id/string_name**,其中**“+”**表示在**R.java**的名为**Id**的内部类中添加一条常量名为**string_name**的记录

```xml
android:textColor="@android:color/red" 
```