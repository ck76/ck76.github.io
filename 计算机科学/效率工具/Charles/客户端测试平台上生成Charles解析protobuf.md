[TOC]

### **一、项目背景**

   现在大多数同事使用的是Mac电脑，虽然fiddler和Wireshark工具可以解析protobuf，但是无法在Mac电脑上直接使用fiddler进行抓包，而Wireshark抓包查看又比较麻烦，于是调研了Charles解析protobuf查看长链接数据，希望对大家有帮助。

### **二、操作步骤**

1、在抓包平台上生成desc文件

登录抓包平台http://tb.baidu.com:8081/capture/#/captureDataApi

选择分支：线上proto选择online，线下proto选择offline；命令号：接口的命令号，可以输入多个，注意需要用英文逗号分割，点击按钮“生成解析文件”即可。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gxergi2t6sj31z20tegtw.jpg)

2、客户端长连接强制转短链接（如已配置可跳过）

Tools->Rewrite...->打勾Enable Rewrite->Import->导入长切短.xml文件

（1）打开Rewrite设置（Tools->Rewrite）

（2）打勾Enable Rewrite

（3）点击Import导入长切短.xml文件

![img](https://tva1.sinaimg.cn/large/008i3skNly1gxergfkkjsj31020s4n0f.jpg)

2、添加贴吧的Desc文件

View->Protobuf Settings->add→添加tbclient.desc文件

![img](https://tva1.sinaimg.cn/large/008i3skNly1gxergditzuj30va0u0q61.jpg)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gxergditzuj30va0u0q61.jpg)

3、配置Message type

View->Viewer Mappings->打勾Enable Viewer Mappings->Import->导入TiebaViewer.xml文件

（1）打开Viewer Mappings设置（View->Viewer Mappings）

（2）打勾Enable Viewer Mappings

（3）点击Import导入TiebaViewer.xml文件

![img](https://tva1.sinaimg.cn/large/008i3skNly1gxergcbre7j30tw0ikdk9.jpg)





### 三、最终效果

![img](https://tva1.sinaimg.cn/large/008i3skNly1gxergazdwqj319i0qmjz4.jpg)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gxerga406ij317c0qkafh.jpg)