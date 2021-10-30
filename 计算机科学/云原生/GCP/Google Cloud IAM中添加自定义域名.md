[TOC]

Google Cloud支持在IAM中加入自定义域名。具体方法如下：

 \1. 登录Google Cloud Console，点击IAM和管理，进入身份和组织

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb2pjnscj30ls15gn04.jpg)

 

\2. 点击注册 

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb2np9nnj31460qa41q.jpg)

 \3. 会重定向到Gmail到管理页面，进行Cloud Identity的设置

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb2o6qzcj312o0igtb0.jpg)

\4. 描述企业情况

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb2l8x1xj30hi0s43zo.jpg)

\5. 选择位置，但不能选择中国，这里选择了美国，点下一步即可

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb2jslqzj30ry0eewfw.jpg)

\6. 输入电子邮件地址

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb2igk4fj30sc0f6q44.jpg)

\7. 输入您的域名

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb2gxg47j30l00fomy5.jpg)

\8. 确认域名

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb2fjl87j30so0dgdhc.jpg)

\9. 输入联系人姓名 

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb2e4hs4j30t40n075m.jpg)

\10. 选择一个账号

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb2cw179j30rw0lktai.jpg)

\11. 确认非机器人

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb2bdiydj30si0lw75u.jpg)

\12. 开始设置

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb293sudj313q0dwjsw.jpg)

 

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb27s4a1j30vw0u0wh9.jpg)

\13. 验证域名的所有权，有多种方法进行验证，建议选择在DNS服务器中添加TXT记录的方式 

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb25ubusj30vr0u0q5r.jpg)

\14. 获取TXT记录的相关信息

 

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb23ufrij30hu0haab9.jpg)

\15. 在DNS服务商的记录中添加记录

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb22h3b9j30ea0ax3yv.jpg)

 

 \16. 确认验证

 ![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb215439j31200nqacn.jpg)

\17. 验证成功

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb1z8lzbj312c0g275w.jpg)

 

\18. 添加其他用户

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb1xwhilj30wg0u0n02.jpg)

\19. 完成设置 

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb1wclhej30xa0e0wg9.jpg)

\20. 在Goolge Cloud的Console中确认配置成功：

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb1v2nmdj317q0gwdgg.jpg)

此时可以在IAM中为这个域名的用户添加权限。

特别要指出的是，如果要管理Shared VPC，必须单独添加Shared VPC的admin权限：

![img](https://tva1.sinaimg.cn/large/008i3skNly1gvwb1sp328j30es0jywfh.jpg)

至此，配置成功。