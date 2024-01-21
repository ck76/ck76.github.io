

[TOC]

### 现有的CPU架构类型

开发Android应用时，有时候Java层的编码不能满足实现需求，就需要到C/C++实现后生成SO文件，再用System.loadLibrary()加载进行调用，这里成为JNI层的实现。常见的场景如：加解密算法，音视频编解码等。在生成SO文件时，需要考虑适配市面上不同手机CPU架构，而生成支持不同平台的SO文件进行兼容。目前Android共支持**七种不同类型的CPU架构**，分别是：**ARMv5，ARMv7 (从2010年起)，x86 (从2011年起)，MIPS (从2012年起)，ARMv8，MIPS64和x86_64 (从2014年起)**。

armeabi、armeabi-v7a、arm64-v8a、mips、mips64、x86、x86_64



### 链接

- https://blog.csdn.net/lyabc123456/article/details/81557220
- https://www.jianshu.com/p/438c136392ba
- https://blog.csdn.net/u012400885/article/details/52923765
- https://www.jianshu.com/p/cb15ba69fa89?utm_campaign=maleskine&utm_content=note&utm_medium=seo_notes&utm_source=recommendation