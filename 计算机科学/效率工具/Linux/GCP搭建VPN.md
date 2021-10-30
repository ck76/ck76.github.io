



```
wget https://git.io/vpnsetup -O vpnsetup.sh && sudo \
VPN_IPSEC_PSK='' \
VPN_USER='' \
VPN_PASSWORD='' sh vpnsetup.sh
```



本教程以 Google Cloud Platform 为例进演示，如果你已经有国外主机仅想知道如何部署VPN，请直接跳到[这里](https://elephantnose.github.io/2018/09/24/10分钟教你用 Google Cloud Platform 搭建自己的VPN/#jump_2_build)开始阅读

------

## 准备

- [能够访问谷歌站点](https://elephantnose.github.io/2018/09/24/10分钟教你用 Google Cloud Platform 搭建自己的VPN/#jump_2_visit_google)
- [Google 账号](https://accounts.google.com/signup/v2/webcreateaccount?hl=zh-CN&flowName=GlifWebSignIn&flowEntry=SignUp)
- VISA 双币信用卡

------

## 申请并创建VM实例

1. 登陆 [Google Cloud Platform](https://console.cloud.google.com/getting-started?hl=zh-CN&pli=1) 并填写相关信息
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrpr93xaj31hc0pg42i.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/1537708746.jpg)
   如图选好各选项后点击 `同意并继续`
2. 激活奖励
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrppughej31hc0pgwj4.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923214100.png)
   点击 `免费试用` 或者右上角的 `激活` 按钮
3. 创建项目
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrpofdl0j31hc0pgaef.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923214342.png)
   `导航菜单` → `Compute Engine` → `VM实例`
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrpl40h5j31hc0pgq5t.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923214714.png)
   点击 `创建`（项目名称选填）
4. 试用 Cloud Platform
   [![img](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923215144.png)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923215144.png)
   点击 `同意并继续`
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrpib7ywj31hc0pg0x4.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923215353.png)
   按空填写信息，完成后点击 `开始免费试用`
   *信用卡用于人机校验，注册时会暂扣$1，会自动退回至账户*
5. 创建虚拟机实例
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrpgjzmxj31hc0pgacx.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923220235.png)
   点击 `创建`
6. 自定义实例配置
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrpf50apj31hc0pgq6d.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923220637.png)
   填写实例 `名称`
   选择 `区域` 及 `地区`（日本站点距离近，响应速度快）
   `内核` 及 `内存` 选择最低配置（如还需其他服务可自行定义参数）
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrpd5ln6j31hc0pggoy.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923221223.png)
   更改 `启动磁盘` 系统为 `Ubuntu 18.04 LTS`
   勾选 `允许 HTTP 流量` `允许 HTTPS 流量`
   点击 `创建`
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrpbq8z8j31hc0pggoi.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923222007.png)

------

## 防火墙规则

1. 新建防火墙规则
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrpb6e2wj31hc0pgad4.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923222248.png)
   `导航菜单` → `VPC 网络` → `防火墙规则`
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrpelnoaj31hc0pgdjr.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923222433.png)
   点击 `+ 创建防火墙规则`
2. 自定义规则配置
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrp68warj31hc0pgtc8.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923222948.png)
   `名称` 按规则自定义
   `目标` `网络中的所有实例`
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrp4x03xj31hc0pgwho.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923223625.png)
   `来源ip地址范围` `0.0.0.0/0`
   `协议和端口` 勾选 `UDP` 填写 `500,4500`，勾选 `其他协议` 填写 `esp`
   点击 `创建`
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrozp1cfj31hc0pgwis.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923224110.png)
   刚刚定义好的防火墙规则

------

## VPC网络

1. 配置VPC网络
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrp32d0yj31hc0pgdl4.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923224443.png)
   `导航菜单` → `VPC 网络` → `VPC 网络` → `default`
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrowxs3bj31hc0pggq2.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923224810.png)
   点击 `修改`
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrovwzdrj31hc0pg78p.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923224913.png)
   勾选 `自动` 及 `全局` 复选框
   点击 `保存`

------

## 负载平衡

1. 负载平衡
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrouyhewj31hc0pgdig.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923225418.png)
   `导航菜单` → `网络服务` → `负载均衡`
2. 创建负载平衡器
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrot6d3wj31hc0pgacc.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923225642.png)
3. UDP负载平衡
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrordh62j31hc0pgn0n.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923225806.png)
   点击 `开始配置`
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxroojfq6j31hc0pgmza.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923225931.png)
   勾选 `从互联网到我的VM`（默认）
   点击 `继续`
4. 后端配置
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxron4k4hj31hc0pgjup.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923231055.png)
   `名称` 按规则自定义填写
   [选择 `区域`](https://elephantnose.github.io/2018/09/24/10分钟教你用 Google Cloud Platform 搭建自己的VPN/#jump_2_area)
   [`选择现有实例`](https://elephantnose.github.io/2018/09/24/10分钟教你用 Google Cloud Platform 搭建自己的VPN/#jump_2_area)
5. 前端配置
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxroloxn5j31hc0pgdj7.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923231339.png)
   `名称` 按规则自定义填写
   `端口` 填写`500-4500`
   点击 `完成`
   点击 `创建`
   [![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxro9y2hij31hc0pgq52.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923231557.png)

------

## 登陆服务器

[![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxroigie8j31hc0pg0vh.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923231801.png)
`导航菜单` → `Compute Engine` → `VM实例`
[![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxro759fcj31hc0pg0vj.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923231937.png)
点击 `SSH`
[![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxro5tb4cj30vm0ls0w6.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923234204.png)

------

## 部署VPN服务

执行命令

```
sudo apt-get update && apt-get dist-upgrade
```



[![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxro4az62j30ve0n1qbr.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923232400.png)

命令行一步部署VPN服务

```
wget https://git.io/vpnsetup -O vpnsetup.sh && sudo \
VPN_IPSEC_PSK='预分享密钥' \
VPN_USER='用户名' \
VPN_PASSWORD='密码' sh vpnsetup.sh
```



五分钟后…
[![img](https://tva1.sinaimg.cn/large/008i3skNly1gvxrnu7dbtj30ve0n1goe.jpg)](https://raw.githubusercontent.com/elephantNose/FigureBed/master/20180923234014.png)
看到如上界面表示已经成功部署VPN服务，请将框内连接信息妥善保存

[详细部署VPN服务教程请移步至Github](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/README-zh.md#重要提示)

------

## 如何连接

[IPsec/XAuth VPN 客户端连接方式](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-xauth-zh.md)（推荐）
[IPsec/L2TP VPN 客户端连接方式](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-zh.md)
[IKEv2连接方式](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/ikev2-howto-zh.md)

第一台设备连接成功后，为了保证后续设备能够正常使用VPN，建议优先阅读[注意事项](https://elephantnose.github.io/2018/09/24/10分钟教你用 Google Cloud Platform 搭建自己的VPN/#jump_2_note)

------

## 如何管理VPN用户

在默认情况下，搭建好VPN服务后，将只创建一个用于 VPN 登录的用户账户。如果你需要添加，更改或者删除用户，请[阅读此链接](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/manage-users-zh.md)

------

## 可能用到的命令

- 重启服务

  ```
  sudo service ipsec restart && sudo service xl2tpd restart
  ```

- 查看连接客户端

  ```
  sudo ipsec whack --trafficstatus
  ```

- 查看连接日志

```
tail -F /var/log/auth.log | grep pluto
```

- 查看服务状态

  ```
  sudo ipsec status
  sudo ipsec verify
  ```

------

## 注意事项

本注意事项为我在使用过程当中从所遇到的问题当中整理得来，目前还未找到解决办法

- 多台（2台及以上）设备不能通过同一路由器进行VPN连接，一个路由器只能路由一台设备连接VPN
- 同一账号可在多台设备上使用，但不能同时使用同一账号进行VPN连接

------

## 遇到问题了

使用过程中出现任何问题都建议优先查看此[链接](https://gist.github.com/hwdsl2/9030462)，该VPN作者对使用者使用过程当中所产生的问题都做出了解答，如果问题仍然没有解决，不要犹豫，删掉所有实例及配置从[这里](https://elephantnose.github.io/2018/09/24/10分钟教你用 Google Cloud Platform 搭建自己的VPN/#jump_2_create)再次开始吧！

------

## 如何访问谷歌站点

安装 Chrome 插件[谷歌访问助手](http://www.ggfwzs.com/)，如果为新版 Chrome 可能会发生安装失败，由于 Chrome 21.x 系列增加了对扩展插件安装的限制，自 Chrome 21.x 开始默认只允许从 Chrome Web Store （Chrome 网上应用店）安装扩展、应用及脚本，也就是意味着用户只能安装谷歌浏览器（Chrome Store）内的扩展插件，解决方法：

1. 右击Chrome浏览器图标选择属性，
2. 在目标后面添加以下内容” –enable-easy-off-store-extension-install”（注：不包括前后双引号，包含空格）并保存，
3. 重启Chrome浏览器将插件拖拽至插件区即可。

------







# 配置 IPsec/XAuth VPN 客户端

*其他语言版本: [English](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-xauth.md), [简体中文](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-xauth-zh.md).*

**注：** 你也可以使用 [IKEv2](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/ikev2-howto-zh.md)（推荐）或者 [IPsec/L2TP](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-zh.md) 模式连接。

在成功 [搭建自己的 VPN 服务器](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/README-zh.md) 之后，按照下面的步骤来配置你的设备。IPsec/XAuth ("Cisco IPsec") 在 Android, iOS 和 OS X 上均受支持，无需安装额外的软件。Windows 用户可以使用免费的 [Shrew Soft 客户端](https://www.shrew.net/download/vpn)。如果无法连接,请首先检查是否输入了正确的 VPN 登录凭证。

IPsec/XAuth 模式也称为 "Cisco IPsec"。该模式通常能够比 IPsec/L2TP **更高效**地传输数据（较低的额外开销）。

------

- 平台名称
  - [Windows](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-xauth-zh.md#windows)
  - [OS X (macOS)](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-xauth-zh.md#os-x)
  - [Android](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-xauth-zh.md#android)
  - [iOS (iPhone/iPad)](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-xauth-zh.md#ios)
  - [Linux](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-xauth-zh.md#linux)

## Windows

**注：** 你也可以使用 [IKEv2](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/ikev2-howto-zh.md)（推荐）或者 [IPsec/L2TP](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-zh.md) 模式连接。无需安装额外的软件。

1. 下载并安装免费的 [Shrew Soft VPN 客户端](https://www.shrew.net/download/vpn)。在安装时请选择 **Standard Edition**。
   **注：** 该 VPN 客户端 **不支持** Windows 10。
2. 单击开始菜单 -> 所有程序 -> ShrewSoft VPN Client -> VPN Access Manager
3. 单击工具栏中的 **Add (+)** 按钮。
4. 在 **Host Name or IP Address** 字段中输入`你的 VPN 服务器 IP`。
5. 单击 **Authentication** 选项卡，从 **Authentication Method** 下拉菜单中选择 **Mutual PSK + XAuth**。
6. 在 **Local Identity** 子选项卡中，从 **Identification Type** 下拉菜单中选择 **IP Address**。
7. 单击 **Credentials** 子选项卡，并在 **Pre Shared Key** 字段中输入`你的 VPN IPsec PSK`。
8. 单击 **Phase 1** 选项卡，从 **Exchange Type** 下拉菜单中选择 **main**。
9. 单击 **Phase 2** 选项卡，从 **HMAC Algorithm** 下拉菜单中选择 **sha1**。
10. 单击 **Save** 保存 VPN 连接的详细信息。
11. 选择新添加的 VPN 连接。单击工具栏中的 **Connect** 按钮。
12. 在 **Username** 字段中输入`你的 VPN 用户名`。
13. 在 **Password** 字段中输入`你的 VPN 密码`。
14. 单击 **Connect**。

VPN 连接成功后，你会在 VPN Connect 状态窗口中看到 **tunnel enabled** 字样。单击 "Network" 选项卡，并确认 **Established - 1** 显示在 "Security Associations" 下面。最后你可以到 [这里](https://www.ipchicken.com/) 检测你的 IP 地址，应该显示为`你的 VPN 服务器 IP`。

如果在连接过程中遇到错误，请参见 [故障排除](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-zh.md#故障排除)。

## OS X

1. 打开系统偏好设置并转到网络部分。
2. 在窗口左下角单击 **+** 按钮。
3. 从 **接口** 下拉菜单选择 **VPN**。
4. 从 **VPN类型** 下拉菜单选择 **Cisco IPSec**。
5. 在 **服务名称** 字段中输入任意内容。
6. 单击 **创建**。
7. 在 **服务器地址** 字段中输入`你的 VPN 服务器 IP`。
8. 在 **帐户名称** 字段中输入`你的 VPN 用户名`。
9. 在 **密码** 字段中输入`你的 VPN 密码`。
10. 单击 **认证设置** 按钮。
11. 在 **机器认证** 部分，选择 **共享的密钥** 单选按钮，然后输入`你的 VPN IPsec PSK`。
12. 保持 **群组名称** 字段空白。
13. 单击 **好**。
14. 选中 **在菜单栏中显示 VPN 状态** 复选框。
15. 单击 **应用** 保存VPN连接信息。

要连接到 VPN：使用菜单栏中的图标，或者打开系统偏好设置的网络部分，选择 VPN 并单击 **连接**。最后你可以到 [这里](https://www.ipchicken.com/) 检测你的 IP 地址，应该显示为`你的 VPN 服务器 IP`。

如果在连接过程中遇到错误，请参见 [故障排除](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-zh.md#故障排除)。

## Android

1. 启动 **设置** 应用程序。
2. 单击 **网络和互联网**。或者，如果你使用 Android 7 或更早版本，在 **无线和网络** 部分单击 **更多...**。
3. 单击 **VPN**。
4. 单击 **添加VPN配置文件** 或窗口右上角的 **+**。
5. 在 **名称** 字段中输入任意内容。
6. 在 **类型** 下拉菜单选择 **IPSec Xauth PSK**。
7. 在 **服务器地址** 字段中输入`你的 VPN 服务器 IP`。
8. 保持 **IPSec 标识符** 字段空白。
9. 在 **IPSec 预共享密钥** 字段中输入`你的 VPN IPsec PSK`。
10. 单击 **保存**。
11. 单击新的VPN连接。
12. 在 **用户名** 字段中输入`你的 VPN 用户名`。
13. 在 **密码** 字段中输入`你的 VPN 密码`。
14. 选中 **保存帐户信息** 复选框。
15. 单击 **连接**。

VPN 连接成功后，会在通知栏显示图标。最后你可以到 [这里](https://www.ipchicken.com/) 检测你的 IP 地址，应该显示为`你的 VPN 服务器 IP`。

如果在连接过程中遇到错误，请参见 [故障排除](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-zh.md#故障排除)。

## iOS

1. 进入设置 -> 通用 -> VPN。
2. 单击 **添加VPN配置...**。
3. 单击 **类型** 。选择 **IPSec** 并返回。
4. 在 **描述** 字段中输入任意内容。
5. 在 **服务器** 字段中输入`你的 VPN 服务器 IP`。
6. 在 **帐户** 字段中输入`你的 VPN 用户名`。
7. 在 **密码** 字段中输入`你的 VPN 密码`。
8. 保持 **群组名称** 字段空白。
9. 在 **密钥** 字段中输入`你的 VPN IPsec PSK`。
10. 单击右上角的 **完成**。
11. 启用 **VPN** 连接。

VPN 连接成功后，会在通知栏显示图标。最后你可以到 [这里](https://www.ipchicken.com/) 检测你的 IP 地址，应该显示为`你的 VPN 服务器 IP`。

如果在连接过程中遇到错误，请参见 [故障排除](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-zh.md#故障排除)。

## Linux

### Fedora 和 CentOS

Fedora 28 （和更新版本）和 CentOS 8/7 用户可以使用 `yum` 安装 `NetworkManager-libreswan-gnome` 软件包，然后通过 GUI 配置 IPsec/XAuth VPN 客户端。

1. 进入 Settings -> Network -> VPN。单击 **+** 按钮。
2. 选择 **IPsec based VPN**。
3. 在 **Name** 字段中输入任意内容。
4. 在 **Gateway** 字段中输入`你的 VPN 服务器 IP`。
5. 在 **Type** 下拉菜单选择 **IKEv1 (XAUTH)**。
6. 在 **User name** 字段中输入`你的 VPN 用户名`。
7. 右键单击 **User password** 字段中的 **?**，选择 **Store the password only for this user**。
8. 在 **User password** 字段中输入`你的 VPN 密码`。
9. 保持 **Group name** 字段空白。
10. 右键单击 **Secret** 字段中的 **?**，选择 **Store the password only for this user**。
11. 在 **Secret** 字段中输入`你的 VPN IPsec PSK`。
12. 保持 **Remote ID** 字段空白。
13. 单击 **Add** 保存 VPN 连接信息。
14. 启用 **VPN** 连接。

VPN 连接成功后，你可以到 [这里](https://www.ipchicken.com/) 检测你的 IP 地址，应该显示为`你的 VPN 服务器 IP`。

### 其它 Linux

其它 Linux 版本用户可以使用 [IPsec/L2TP](https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-zh.md#linux) 模式连接。

## 致谢

本文档是在 [Streisand](https://github.com/StreisandEffect/streisand) 项目文档基础上翻译和修改。该项目由 Joshua Lund 和其他开发者维护。

## 授权协议

注： 这个协议仅适用于本文档。

版权所有 (C) 2016-2021 [Lin Song](https://github.com/hwdsl2) [![View my profile on LinkedIn](https://camo.githubusercontent.com/7b68e4546d1dd022e4522f8396ad7273e9685ab3e091893ea77343e16839b0f0/68747470733a2f2f7374617469632e6c6963646e2e636f6d2f736364732f636f6d6d6f6e2f752f696d672f77656270726f6d6f2f62746e5f766965776d795f3136307832352e706e67)](https://www.linkedin.com/in/linsongui)
基于 [Joshua Lund 的工作](https://github.com/StreisandEffect/streisand/blob/6aa6b6b2735dd829ca8c417d72eb2768a89b6639/playbooks/roles/l2tp-ipsec/templates/instructions.md.j2) (版权所有 2014-2016)

本程序为自由软件，在自由软件联盟发布的[ GNU 通用公共许可协议](https://www.gnu.org/licenses/gpl.html)的约束下，你可以对其进行再发布及修改。协议版本为第三版或（随你）更新的版本。

我们希望发布的这款程序有用，但不保证，甚至不保证它有经济价值和适合特定用途。详情参见GNU通用公共许可协议。



---





## 链接

参考链接：[Google Cloud Platform 搭建個人VPN](https://medium.com/@YasuoYuHao/google-cloud-platform-搭建個人vpn-358ccdbeca40)
相关链接：[Google Cloud Platform免费申请&一键搭建SSR & BBR加速教程](https://www.jianshu.com/p/05bad6912954)

- https://elephantnose.github.io/2018/09/24/10%E5%88%86%E9%92%9F%E6%95%99%E4%BD%A0%E7%94%A8%20Google%20Cloud%20Platform%20%E6%90%AD%E5%BB%BA%E8%87%AA%E5%B7%B1%E7%9A%84VPN/#jump_2_note
- https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-xauth-zh.md#android
- https://github.com/hwdsl2/setup-ipsec-vpn/blob/master/docs/clients-xauth-zh.md