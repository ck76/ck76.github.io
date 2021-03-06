# 导语

什么是热修复？热修复又称热补丁，一般是用事先定义好的接口，从网络下载代码并更新客户端代码，从而在用户无感知、也无需重装App的情况下，实现动态修复或动态更新。这样带来的优势就是成本低、效率高，快速作用，节省应用发布时间，缩短开发周期，降低开发成本；方便数据统计和测试反馈，有利于更好地改进App。

 

正常的流程：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxktgv7usj31ex089766.jpg)

热修复流程：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxktfya6qj31ex089gnp.jpg)

热修复特点：

- 无需重新发版，实时高效热修复；
- 用户无感知修复，无需下载新的应用，代价小；
- 修复成功率高，把损失降到最低。

## 一、开源流行方案对比：

如今的热修复技术可谓是百花齐放，每个产品都各有优势和特点，不过各自都存在局限性，或不稳定、或补丁大、或效率低、或接入繁琐，大部分技术上看起来似乎可行，但实际体验并不好。大体上从Githup星评能直观了解到各大平台方案受欢迎程度，即开发者信赖程度。

| 方案名称 |         方案开发公司         | 开发时间 | Github星评 |
| -------: | :--------------------------: | :------: | :--------: |
|   Robust |       美团（Java派系）       |  2016年  |    2500    |
|   Andfix |      阿里（Native派系）      |  2015年  |    5994    |
|     Nuwa |    大众点评(dex文件补丁）    |  2015年  |    2880    |
| Dexposed | 阿里（不考虑，需要root权限） |          |            |
|    Amigo |       饿了么(apk补丁）       |  2016年  |    1231    |
|   Tinker |        微信(apk补丁)         |  2016年  |   11259    |
| RocooFix |    百度金融(Nuwa改进版）     |  2016年  |    1599    |

1、美团Robust，基于Instant Run的原理兼容性高，实时生效，但是apk的体积会增大，so和资源的替换暂时不支持，侵入式打包，且接入复杂。

2、女娲Nuwa，坑比较多，比如不支持Gradle1.5以上、补丁包没有签名校验、字节码注入复杂，维护成本高。

3、QQ空间QZone，兼容性高，侵入式，不即时生效，混淆对性能会有一定的影响，且在Art模式下出现内存错乱(是Tinker在MDCC上指出的);

4、饿了么Amigo，目前使用人数最少，网上可参考资料太少，几乎查不到优接入经验和踩坑经验的文章。

5、RocooFix跟Andfix相似，问题是兼容性差，有些机型会失效。

*[方案比较来自阿里百川](https://baichuan.taobao.com/docs/doc.htm?spm=a3c0d.7629140.0.0.9Lz7Um&treeId=234&articleId=106002&docType=1)

| 平台       |    HotFix    |    AndFix    |     Tinker     |     Qzone      |     Robust     |
| :--------- | :----------: | :----------: | :------------: | :------------: | :------------: |
| 即时生效   |     yes      |     yes      |       no       |       no       |      yes       |
| 性能损耗   |     较小     |     较小     |      较小      |      较大      |      较小      |
| 侵入式打包 | 无侵入式打包 | 无侵入式打包 | 依赖侵入式打包 | 依赖侵入式打包 | 依赖侵入式打包 |
| Rom体积    |     较小     |     较小     |   Dalvik较大   |      较小      |      较小      |
| 接入复杂度 |  傻瓜式接入  |   比较简单   |    比较复杂    |    比较简单    |      复杂      |
| 补丁包大小 |     较小     |     较小     |      较小      |      较大      |      一般      |
| 全平台支持 |     yes      |      no      |      yes       |      yes       |      yes       |
| 类替换     |     yes      |     yes      |      yes       |      yes       |       no       |
| so替换     |     yes      |      no      |      yes       |       no       |       no       |
| 资源替换   |     yes      |      no      |      yes       |      yes       |       no       |

 

***\*一、阿里热修复\****

| 方案对比     | Andfix开源版本   | 阿里Hotfix 1.X     | 阿里Hotfix 最新版 (Sophix) |
| :----------- | :--------------- | :----------------- | :------------------------- |
| 方法替换     | 支持，除部分情况 | 支持，除部分情况   | 全部支持                   |
| 方法增加减少 | 不支持           | 不支持             | 以冷启动方式支持           |
| 方法反射调用 | 只支持静态方法   | 只支持静态方法     | 以冷启动方式支持           |
| 即时生效     | 支持             | 支持               | 视情况支持                 |
| 多DEX        | 不支持           | 支持               | 支持                       |
| 资源更新     | 不支持           | 不支持             | 支持                       |
| so库更新     | 不支持           | 不支持             | 支持                       |
| Android版本  | 支持2.3~7.0      | 支持2.3~6.0        | 全部支持包含7.0以上        |
| 已有机型     | 大部分支持       | 大部分支持         | 全部支持                   |
| 安全机制     | 无               | 加密传输及签名校验 | 加密传输及签名校验         |
| 性能损耗     | 低，几乎无损耗   | 低，几乎无损耗     | 低，仅冷启动情况下有些损耗 |
| 生成补丁     | 繁琐，命令行操作 | 繁琐，命令行操作   | 便捷，图形化界面           |
| 补丁大小     | 不大，仅变动的类 | 小，仅变动的方法   | 不大，仅变动的资源和代码   |
| 服务端支持   | 无               | 支持服务端控制     | 支持服务端控制             |

**1、AndFix（开源）：**阿里热修复从支付宝推出AndFix热修复方案的引起业界广泛关注。Andfix是一种Native底层结构替换的方案，达到了运行时生效即时修复的效果。但Andfix本身具有局限性，且不说其底层固定结构的替换方案稳定性不好，其使用范围只提供了代码层面的修复，对于资源和so的修复都还未能实现。

优点：

​    \1. 不侵入打包, 性能无损耗；  

​    \2. 即时生效。
缺点：

  \1. 需要考虑指令集的兼容问题，需要native代码支持，兼容性上会有一定的影响； 

  \2. 不支持新增类方法和字段，以及修改<init>方法，也不支持对资源和so的替换。

**2、Hotfix（不开源）**：[阿里百川](http://baichuan.taobao.com/?spm=a3c0d.7662649.0.0.JqrnYU)结合手淘在实际工程中使用Andfix的经验，对相关业务逻辑解耦后，推出了阿里百川Hotfix方案。能够真正做到BUG即时修复用户无感知，同时保证对应用性能不产生不必要的损耗，对于基本的代码修复需求都可以解决，安全性和易用性都做的比较好。但是HotFix.1.0仍然不支持资源、So文件修复；不支持新增类方法/类字段。HotFix2.0升级打破限制。

[HotFix](http://baichuan.taobao.com/docs/doc.htm?spm=a3c0d.7629140.0.0.5FQExN&treeId=234&articleId=105461&docType=1)2.0总体来说最大的优势在于：

- 补丁即时生效，不需要应用重启；
- 灵活切换热部署和冷部署的方案；
- 实现了资源、SO文件、类修复的实时生效;
- 补丁包体积小，可视化UI界面操作；
- 对应用无侵入，几乎无性能损耗；
- 兼容Android所有机型，傻瓜式接入。

**产品收费问题，\**阿里百川HotFix在2.0版本的规划完全实现前完全免费\**，可放心使用。**

**3、Sophix（Hotfix3.0）**：在2017年6月11日，手淘技术团队联合[阿里云](https://help.aliyun.com/document_detail/51415.html?spm=a2c4g.11186623.6.539.nmvhbz)正式发布了史上首个非侵入式移动热更新解决方案。核心设计是非侵入性，即不添加任何超出开发者预期的代码，以避免多余的热修复代码给开发者带来困扰，给了开发者最大的透明度和自由度，接入成本在所有方案里最低的。
[Sophix](https://www.aliyun.com/product/hotfix)提供了一套更加完美的客户端服务端一体的热更新方案，做到了图形界面一键打包、加密传输、签名校验和服务端控制发布与灰度功能，让你用最少的时间实现最强大可靠的全方位热更新。这也是Hotfix3.0开始收费的原因。

下面是Sophix与Tinker和Amigo方案对比：

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxkt9vyvoj30hs0al0tc.jpg)

目前，Sophix在各个指标上全面占优。唯一不足的是不支持的地方就是四大组件的修复。这是因为如果要修复四大组件，必须在AndroidManifest里面预先插入代理组件，并且尽可能声明所有权限，而这么做就会给原先的app添加很多臃肿的代码，对app运行流程的侵入性很强。

[产品收费标准介绍](https://help.aliyun.com/document_detail/57064.html?spm=a2c4g.11186623.6.543.E2Wo3g):

> **官方文档** 
> **它有一个收费阀值** ,超过这个阈值会从账户扣除费用。
> 每个账号，每月5万台设备免费。 
> 每个账号，平均到每台设备，一天免费调用20次。 
> 补丁包使用，完全不做限制。不额外计费。 

## 二、Tinker （微信开源热修复）

选择Tinker的理由很简单，它自己的介绍够自信：[官方介绍https://github.com/Tencent/tinker/wiki](https://github.com/Tencent/tinker/wiki)

> Tinker已运行在微信的数亿Android设备上，那么为什么你不使用Tinker呢？

|            | Tinker | QZone | AndFix | Robust |
| :--------- | :----- | :---- | :----- | :----- |
| 类替换     | yes    | yes   | no     | no     |
| So替换     | yes    | no    | no     | no     |
| 资源替换   | yes    | yes   | no     | no     |
| 全平台支持 | yes    | yes   | yes    | yes    |
| 即时生效   | no     | no    | yes    | yes    |
| 性能损耗   | 较小   | 较大  | 较小   | 较小   |
| 补丁包大小 | 较小   | 较大  | 一般   | 一般   |
| 开发透明   | yes    | yes   | no     | no     |
| 复杂度     | 较低   | 较低  | 复杂   | 复杂   |
| gradle支持 | yes    | no    | no     | no     |
| Rom体积    | 较大   | 较小  | 较小   | 较小   |
| 成功率     | 较高   | 较高  | 一般   | 最高   |

优点：

1. 项目成熟，文档健全；
2. 集成简单；
3. 支持资源文件和so文件的修复替换。

由于原理与系统限制，Tinker有以下已知问题：

1. Tinker不支持修改AndroidManifest.xml，Tinker不支持新增四大组件(1.9.0支持新增非export的Activity)；
2. 由于Google Play的开发者条款限制，不建议在GP渠道动态更新代码；
3. 在Android N上，补丁对应用启动时间有轻微的影响；
4. 不支持部分三星android-21机型，加载补丁时会主动抛出"TinkerRuntimeException:checkDexInstall failed"；
5. 对于资源替换，不支持修改remoteView。例如transition动画，notification icon以及桌面图标。

修复原理：通过新旧apk比较，使用gradle从插件生成.dex补丁文件（并不是真正的dex文件），补丁通过服务器下发后尝试对dex文件二路归并进行合并，最终生成全量的dex文件，与生成补丁互为逆过程，生成全量dex文件后进行optimize操作，最终生成odex文件。在Application中进行反射调用已经合成的dex文件。

另外，有一个第三方平台[TinkerPatch](http://www.tinkerpatch.com/Docs/intro)，帮助 Tinker 使用者有一个后台可以下发和管理补丁包，并且需要处理传输安全等部署工作，提供了补丁后台托管，版本管理，保证传输安全等功能，让你无需搭建一个后台，无需关心部署操作，只需引入一个 SDK 即可立即使用 Tinker，傻瓜式接入，实时监控。