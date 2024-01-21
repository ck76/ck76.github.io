---
title: assets解析
date: 2018-08-06 02:05:50
categories: "Android-UI-Resource"
tags:
---

### **Android 中资源分为两种,**

​        **①、**第一种是res下可编译的资源文件,这种资源文件系统会在R.java里面自动生成该资源文件的ID，（除了raw外，其他资源目录中的资源文件都会被编译），这也是为什么将APK文件解压后无法直接查看XML格式资源文件内容的原因。而assets与res/raw目录中的资源文件不会做任何处理，所以将APK解压后，这两个目录中的资源文件都会保持原样。res目录只能有一层子目录，而且这些子目录必须是预定义的，如res/layout、res/values等都是合法的，而res/abc,res/xyz并不是合法的资源目录。

​        **②、**第二种就是放在assets文件夹下面的原生资源文件,放在这个文件夹下面的文件不会被R文件编译,所以不能像第一种那样直接使用.Android提供了一个工具类,方便我们操作获取assets文件下的文件，在assets目录中可以建任意层次的子目录(只受操作系统的限制)。

<!--more-->

### assets与res/raw不同

assets目录是Android的一种特殊目录，用于放置APP所需的固定文件，且该文件被打包到APK中时，不会被编码到二进制文件。
 Android还存在一种放置在res下的raw目录，该目录与assets目录不同。
 注意点：
 1、 assets目录不会被映射到R中，因此，资源无法通过R.id方式获取，必须要通过AssetManager进行操作与获取；res/raw目录下的资源会被映射到R中，可以通过getResource()方法获取资源。
 2、 多级目录：assets下可以有多级目录，res/raw下不可以有多级目录。
 3、 编码（都不会被编码）：assets目录下资源不会被二进制编码；res/raw应该也不会被编码。

###  assets内资源使用方法

1、 资源使用

```java
AssetManager am = getAssets();  
InputStream is = am.open("filename");  
```

获取到输入流。

2、 信息获取
 通过`am.list(“”)`得到assets目录下的所有文件和子目录名称的数组，通过`am.list(SubFolderName)`，得到`assets/SubFolderName`下所有的文件和子目录名称的数组。
 通过`am.openFd(fileName)`得到的`AssetFileDescriptor`对象来获得`fileName`文件的信息，例如长度等。
 AssetFileDescriptor 能够完成对文件的其他一些操作，可以关注该类。

AssetManager接口介绍：

```java
final String[] list(String path)  //返回指定路径下的所有文件及目录名

final InputStream open(String fileName) //使用 ACCESS_STREAMING模式打开assets下的指定文件

final InputStream open(String fileName, int accessMode) //使用显示的访问模式打开assets下的指定文件
```

简单应用：
 1、结合WebView加载assets目录下的网页：
 //加载assets/win8_Demo/目录下的index.html网页
 webView.loadUrl("file:///android_asset/helloworld.html");
 说明：可加载assets目录下的网页，css，js，图片等文件也会的加载。

### res/raw资源使用

```java
InputStream is = getResources().openRawResource(R.id.fileNameID) ;
//R.id.fileNameID
为需要访问的文件对应的资源ID
```

获取到输入流，即可使用。

VideoView播放：
 `VideoView.setVideoUri(Uri.parse("android.resource://" + getpackageName() + "/" + R.raw.movie));`

 

 

 

 

 