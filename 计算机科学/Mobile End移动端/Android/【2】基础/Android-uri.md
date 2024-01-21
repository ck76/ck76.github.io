[TOC]

## 什么是URI

URI，全称为Uniform Resource Identifier，统一资源标识符，用来唯一的标识一个资源。Web上可用的每种资源如HTML文档、图像、视频片段、程序等都是一个来URI来定位的

URI一般由三部组成
①访问资源的命名机制
②存放资源的主机名
③资源自身的名称，由路径表示，着重强调于资源。
例如：

```
urn:issn:1535-3613
```

## 什么是URL

URL，全称Uniform Resource Locator，是统一资源定位符，是Internet上用来描述资源的字符串，主要用在各种www客户端和服务器程序是，特别是著名的Mosaic。采用URL可以用一种统一的格式来描述各种信息资源，包括文件、服务器的地址和目录等。

URL一般由三部组成
①协议(或称为服务方式)
②存有该资源的主机IP地址(有时也包括端口号)
③主机资源的具体地址。如目录和文件名等。
例如：

```
http://www.jianshu.com/u/606fd5f5448c
```

## URL和URI有什么联系

URI属于URL更高层次的抽象，一种字符串文本标准。就是说，URI属于父类，而URL属于URI的子类。URL是URI的一个子集。URI还有一个子类URN-统一资源名称。

**URI=URL+URN**

<!--more-->

## Android的Uri

scheme://host:port/path 

举个实际的例子： 

content://com.example.project:200/folder/subfolder/etc 

\\---------/  \\---------------------------/ \\---/ \\--------------------------/ 

scheme                 host               port        path 

​                     \\--------------------------------/ 

                                      authority    

**Android的Uri由以下三部分组成： "content://"、数据的路径、标示ID(可选)**

举些例子，如：

所有联系人的Uri： content://contacts/people

某个联系人的Uri: content://contacts/people/5

所有图片Uri: content://media/external

某个图片的Uri：content://media/external/images/media/4

## 内部保存

android是如何管理多媒体文件(音频、视频、图片)的信息。通过DDMS，我们在/data/data/com.android.providers.media下找到数据库文件  打开external.db文件进一步查看：在media表格下，可以看到文件路径(_data) 



## Android之uri、file、path相互转化

1、uri转file:

```
见第三个
```

file转uri:

```java
URI uri = file.toURI();  
```

2、uri转path:

```java
    public  File uri2File(Activity context, Uri uri) {
        File file = null;
        String[] proj = {MediaStore.Images.Media.DATA};
        android.database.Cursor actualimagecursor = context.managedQuery(uri, proj, null,
                null, null);
        int actual_image_column_index = actualimagecursor
                .getColumnIndexOrThrow(MediaStore.Images.Media.DATA);
        actualimagecursor.moveToFirst();
        String img_path = actualimagecursor
                .getString(actual_image_column_index);
        file = new File(img_path);
        return file;        //file.getPath()
    }
```

path转uri:

```
Uri uri = Uri.parse(path);  
```

3、file转path:

```
String path = file.getPath()  
```

path转file:

```
File file = new File(path)  
```