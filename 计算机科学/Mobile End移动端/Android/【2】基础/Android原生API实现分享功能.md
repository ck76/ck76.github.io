[TOC]

不同于微博微信等分享能带有分享来源标注，只是利用Android原生API实现最基础的分享，对于某些特定平台的一些高级分享特性，比如微信或者微博之类的分享来源标注，需要在其开放平台注册应用再接入其 sdk 才可以 。

<!--more-->

#### 大致过程：

1.我们先创建一个意图事件

```java
Intent share_intent = new Intent()；
```

2，然后我们设置里面的Action方法，调用里面的一个分享行为，Intent.ACTION_SEND

```java
share_intent.setAction(Intent.ACTION_SEND);//设置分享行为
```

3，然后你可以设置你分享的内容类型，如“”/“”是支持所有类型，“”imge“”图片类型，“”text/plain“”txt文件类型

```java
share_intent.setType("text/plain");//设置分享内容的类型
###########################################################
// 比如发送文本形式的数据内容
// 指定发送的内容
sendIntent.putExtra(Intent.EXTRA_TEXT, "This is my text to send.");
// 指定发送内容的类型
sendIntent.setType("text/plain"); 
###########################################################
// 比如发送二进制文件数据流内容（比如图片、视频、音频文件等等）
// 指定发送的内容 (EXTRA_STREAM 对于文件 Uri )
shareIntent.putExtra(Intent.EXTRA_STREAM, uriToImage);
// 指定发送内容的类型 (MIME type)
shareIntent.setType("image/jpeg");
###########################################################
```

4，添加分享内容的标题

```java
share_intent.putExtra(Intent.EXTRA_SUBJECT, contentTitle);
```

5，添加分享的内容

```java
share_intent.putExtra(Intent.EXTRA_TEXT, content);
```

6，添加分享的dalog

```java
share_intent = Intent.createChooser(share_intent, dialogTitle);
activity.startActivity(share_intent);
```
#### 实例：

```java
   Intent intent1=new Intent();
                        intent1.setAction(Intent.ACTION_SEND);
                        intent1.putExtra(Intent.EXTRA_TEXT,"签到签退下载地址。。。。。。");
                        intent1.setType("text/plain");
                        startActivity(Intent.createChooser(intent1,"分享"));
```

#### 下面是一些常见文件的mimeType：

```java
{".3gp", "video/3gpp"},  
{".apk", "application/vnd.android.package-archive"},  
{".asf", "video/x-ms-asf"},  
{".avi", "video/x-msvideo"},  
{".bin", "application/octet-stream"},  
{".bmp", "image/bmp"},  
{".c", "text/plain"},  
{".class", "application/octet-stream"},  
{".conf", "text/plain"},  
{".cpp", "text/plain"},  
{".doc", "application/msword"},  
{".exe", "application/octet-stream"},  
{".gif", "image/gif"},  
{".gtar", "application/x-gtar"},  
{".gz", "application/x-gzip"},  
{".h", "text/plain"},  
{".htm", "text/html"},  
{".html", "text/html"},  
{".jar", "application/java-archive"},  
{".java", "text/plain"},  
{".jpeg", "image/jpeg"},  
{".jpg", "image/jpeg"},  
{".js", "application/x-javascript"},  
{".log", "text/plain"},  
{".m3u", "audio/x-mpegurl"},  
{".m4a", "audio/mp4a-latm"},  
{".m4b", "audio/mp4a-latm"},  
{".m4p", "audio/mp4a-latm"},  
{".m4u", "video/vnd.mpegurl"},  
{".m4v", "video/x-m4v"},  
{".mov", "video/quicktime"},  
{".mp2", "audio/x-mpeg"},  
{".mp3", "audio/x-mpeg"},  
{".mp4", "video/mp4"},  
{".mpc", "application/vnd.mpohun.certificate"},  
{".mpe", "video/mpeg"},  
{".mpeg", "video/mpeg"},  
{".mpg", "video/mpeg"},  
     {".mpg4", "video/mp4"},  
{".mpga", "audio/mpeg"},  
{".msg", "application/vnd.ms-outlook"},  
{".ogg", "audio/ogg"},  
{".pdf", "application/pdf"},  
{".png", "image/png"},  
{".pps", "application/vnd.ms-powerpoint"},  
{".ppt", "application/vnd.ms-powerpoint"},  
{".prop", "text/plain"},  
{".rar", "application/x-rar-compressed"},  
{".rc", "text/plain"},  
{".rmvb", "audio/x-pn-realaudio"},  
{".rtf", "application/rtf"},  
{".sh", "text/plain"},  
{".tar", "application/x-tar"},  
{".tgz", "application/x-compressed"},  
{".txt", "text/plain"},  
{".wav", "audio/x-wav"},  
{".wma", "audio/x-ms-wma"},  
{".wmv", "audio/x-ms-wmv"},  
{".wps", "application/vnd.ms-works"},  
//{".xml", "text/xml"},  
{".xml", "text/plain"},  
{".z", "application/x-compress"},  
{".zip", "application/zip"},  
{"", "*/*"}
```





https://www.jianshu.com/p/1d4bd2c5ef69