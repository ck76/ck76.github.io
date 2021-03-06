https://zhuanlan.zhihu.com/p/60237181

[TOC]

## 引言

众所周知，Android是谷歌开发的一款基于Linux的开源操作系统，每年迭代一次大版本升级。 小米、华为、OPPO、VIVO、三星等各大厂商对Android原生系统进行二次开发衍生出具有各家特色的系统（比如MIUI），为手机、电视、平板电脑、手表等数十亿设备提供平台支持，使得Android作为全球最受欢迎的移动操作系统。Android诞生至今已有10余年，这一路走来Android遇到哪些问题？大版本升级朝着什么方向演进？Android的未来如何？

（本文首发于我的微信公众号，欢迎大家关注最新资讯）

![img](https://pic4.zhimg.com/v2-da6b3fed72d68412a95f919eaf481edb_r.jpg)

### 1. 发展历程

先来看看Android系统的发展过程，从2008年发布Android 1.0系统，直到2019年即将发布Android 10.0系统，下面列举些重要的时间节点。

- 2003年10月，Andy Rubin团队创办Android公司；
- 2005年8月，谷歌收购Android公司，Andy Rubin担任谷歌工程部副总裁继续负责Android项目；
- 2008年9月，谷歌正式发布Android 1.0系统；
- 2011年1月，Android系统设备的用户总数达到了1.35亿，成为智能手机领域占有量第一的系统；
- 2011年8月，Android手机占据全球智能机市场48%份额，并在亚太地区市场占据统治地位，终结了Symbian系统的霸主地位，跃居全球第一；
- 2012年1月，谷歌Android Market已有10万开发者，推出超过40万应用；
- 2013年11月，Android 4.4正式发布，系统更智能、UI更现代；
- 2013年到2018年，这个阶段安卓进入飞速发展期，被升级的有摄像头、内存、机身、芯片等，原来的3.5寸小屏已退出历史舞台，全面屏、刘海屏、水滴屏已成为当下主流屏幕方案。

### 2. 系统演进

系统演进趋势：每个Android大版本的更新迭代前行，历经10余年，在用户体验、流畅性、续航、安全、隐私、机器学习等方面都取得较大的改进。图中是每个大版本中最具代表性的特征标记在图中，并不代表着该版本全部特征，同样专项计划也不是只在某一个版本执行，比如续航和性能优化，每一个版本都在持续改进中，Treble计划也一直在迭代至今。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqxstoilj30wh075dgr.jpg)

- 从Android 1.0发展到Android 4.0，系统各项功能和特性迭代到一个较完善的阶段；
- Android 4.1系统，Google开展了黄油计划（Project Butter），为了让Android系统摆脱UI交互上的严重滞后感，希望能像“黄油”一样顺滑。 核心原理是系统框架中的渲染和动画统一采用垂直同步技术(VSYNC)，以及三重缓冲技术(Triple Buffer)，让滑动、翻页等操作更加一致与顺滑。
- Android 4.4系统，Google开展了瘦身计划（Project Svelte），力求降低安卓系统的内存使用，解决低端机型升级难的问题，让Android 4.4可正常运行在所有Android手机，从而减少安卓系统继续碎片化。UI设计上，支持新的“沉浸式模式”，用户界面由过去的黑色与蓝色为主的色调转向带有透明度的浅色系，视觉语言变得更加明亮与现代化。
- Android 5.0系统，Google开展了伏特计划（Project Volta），力求提升续航能力，这方面Google落后于业界厂商，厂商直面用户对续航尤为迫切，往往系统资源管控更为严格。另外，系统采用全新的ART，抛弃Dalvik虚拟机，大幅提升运行效率。UI设计上，使用全新的扁平化Material Design设计风格，更加清新与质感的设计，统一Android设备的外观和使用体验。
- Android 6.0系统，Google引入新的运行时权限，让用户能够更好地了解和控制权限；引入了Doze模式，进一步提升电池续航能力。UI设计上，新增夜间模式，大幅改进通知栏，让通知更简洁。
- Android 7.0系统，引入新的JIT编译器，对AOT编译器的补充，可节省存储空间和加快更新速度；进一步优化Doze唤醒机制；UI设计上，支持分屏功能；
- Android 8.0系统，Google开展了计划（Project Treble），重新架构Android，将安卓系统框架与Vendor层解耦，力求彻底解决安卓碎片化这一老大难的问题，这是安卓系统架构最大的变化。系统层面加强对后台服务、广播、位置的管控限制。UI设计上，改进通知栏，智能文本选择和自动填充功能。
- Android 9.0系统，引入神经网络API，采用机器学习的思路来预测用户使用习惯来做省电优化，继续强化Treble计划；文件系统(sdcardf/F2FS)持续提升；私有API的限制进一步规范化Android生态，强化隐私和安全，硬件安全性模块以及统一生物识别身份验证界面。 UI设计上，新的手势导航，加强支持刘海屏，UI搜索界面使用到机器学习，AI正在逐步强化Android系统。
- Android 10.0系统，Google开展了主线计划（Project Mainline），相关模块（Modules）不允许厂商直接修改，只能由Google应用商店来更新升级，强化用户隐私、系统安全与兼容性。支持脸部生物识别。

系统不断演进，但整体架构基本没有改变，如下图所示。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqxqfh39j30u0186qaa.jpg)

### 3. 碎片化

Android历经10余年的迭代，在流畅性、内存、续航、安全、隐私等方面都取得很大的进步，但Android系统的碎片化一直是痛点问题，带来不一致的用户体验。Android的开放性，是其长久发展的主要原因，让大多数的厂商都选择Android系统，但开放性的背后是碎片化，从Android诞生至今问题就一直存在，Google一直在努力从技术角度来解决碎片化问题。从Android 8.0提出Treble项目，重新架构系统将system与vendor解耦合，用于加快Android新版本的适配，效果并不明显，Google继续在后续的Android P以及Android Q一直在不遗余力地持续完善Treble项目，力争加快系统升级速度。如下图，目标是希望在保持Vendor不变的情况下，可以独立升级System模块。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqxp03udj30oo0cxmxv.jpg)

Android系统碎片化，让安全、隐私问题存在风险，且存在体验不一致性问题，但老版本手机的OTA维护升级对厂商来说成本是昂贵的，Ｇoogle感觉到对Android系统掌控力度不足，要想彻底改变，除非不让各大厂商定制化，这势必导致Android手机完全同质化，手机厂商就没法玩了，等于自掘坟墓，Google肯定不会这么干。于是，Google在Android 10.0提出了”Project Mainline“，将对隐私、安全、兼容性造成重大影响的少数模块独立成module，每个module打包成APEX格式（一种类似于APK的新格式），由Google通过应用商店定期来升级，从而保证低版本的手机不会因为碎片化而得不到隐私、安全与兼容性的更新。这些module是由Google维护的主线，各大厂商只能跟Google沟通并将代码upstream到AOSP主线。Google花费了大量的人力在努力完善并推行Mainline，Google希望统一管控的机制，厂商希望最大的自由定制空间，这是一场有趣的角逐，笔者跟团队一起跟Google协商落地module的落地计划，最终将某些module影响较大模块争取Android 11再上线，Mainline更新机制如下图所示。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqxnk9f1j30xd0kbtac.jpg)

### 4. 应用演进

Android系统离不开各App来提供丰富的功能，下面再来简单说一说应用的一些技术演进。

移动端跨平台技术：从最开始以Cordova为基础(依赖于WebView)的Hybrid混合开发技术，到React Native的桥接（将JS转为Native）的技术，再到最新的Flutter技术。Flutter是Google发布的全新的移动跨平台UI框架，渲染引擎依靠跨平台的Skia图形库来实现，依赖系统的只有图形绘制相关的接口，可以在最大程度上保证不同平台、不同设备的体验一致性，逻辑处理使用Dart语言，执行效率比JavaScript高。另外，Google内部正在开发的另一个操作系统Fuchsia的UI layer采用的是Flutter，也就是说Flutter天然可以支持Android、IOS以及未来的Fuchsia。在大前端方向，对于跨平台开发中一直在不断迭代中寻找更好、更优的解决方案，目前来看Flutter还是更有优势。

应用架构：MVC模式（Model–view–controller）但Activity类过于臃肿，为解决这个问题，有了MVP（Model–view–presenter），presenter不仅要操作数据，而且要更新view；再到MVVM（Model-View-ViewModel）解决了MVP大量的手动View和Model同步的问题，提供双向绑定机制。

热修复与插件化技术：热修复的主要应用场景是为了让用户无感得修复线上缺陷，比如Tinker，Andfix，Sophix等。插件化是为了减少模块耦合，可减少主程序的规模，可按需加载，比如DroidPlugin，OpenAtlas等。关于各个热修复与插件化的细节不再展开，这里就说一点，Android 7.0对Native的NDK的调用限制是手铐，尤其是Android 9.0对Java层SDK的调用限制就是脚铐，那么对于Android应用想再搞插件化之类的黑科技便是带着脚手铐跳舞，能跳但舞姿可能不太美观。

App Bundle：随着应用不断演讲，功能越来越复杂，且应用针对不同屏幕设备、不同国家语言资源都打包在同一个App，导致应用包不断增大，据统计自2012年以来应用包大小增长5倍。虽然现在手机的存储空间越来越大，但用户照片、视频等媒体文件品质在逐渐提升，导致设备可用空间逐渐紧缩。为此Google在去年Google I/O大会讲述Android引入新的App动态化框架（即Android App Bundle，缩写为AAB）。利用Split Apk完成动态加载，使用AAB动态下发方式，可显著缩小应用体积，减少对存储空间的占用。

Kotlin：是Google推荐的官方静态编程语言，与Java互通，可相互转换。Kotlin编译成Java字节码，也可以编译成JavaScript，运行在没有JVM的设备上，简洁安全。使用Kotlin更快速地编写Android应用，可以提高开发者的工作效率，少编写样板代码，被称之为 Android 世界的Swift。谷歌开发者社区做过一个问卷调查，大概有40%的Android开发者已使用过Kotlin。这里并非鼓励大家一定都要使用Kotlin，学习新语言就像一次投资，要权衡团队成本与收益之间的利弊。

### 5. Fuchsia

2016年开始，Google有一群超过百人的工程师团队秘密研发一款名为Fuchsia的新系统，该团队很豪华，有来自Android、iPhone、WebOS、Chrome、Flutter等核心工程师，这么多优秀的人在一起研发这个项目，的确值得期待。Fuchsia的内核采用Zircon，UI层采用Flutter框架，底层渲染Escher，支持Vulkan作为底层Graphics API。

提到Vulkan，顺便说一下，去年笔者在美国跟Android团队讨论到并问及Vulkan未来的规划，Google表示未来几年会大力推广Vulkan技术，Vulkan是一种跨平台的高性能低开销的图形接口，在移动设备上比OpenGL ES有着更出色的表现。Vulkan将会是未来Android平台的一个发展方向，尤其是游戏领域，比如王者荣耀Vulkan版本。

在2017年5月，Google的Android工程副总裁Dave Burke称Fuchsia是早期实验项目，而在谷歌内部有很多这类实验项目，存在很大未知变数。2017年11月，Google研发人员表示Fuchsia支持Swift，足见打造统一操作系统的野心。在过去的Google I/O大会只字未提及Fuchsia，Google官方回应不清楚Fuchsia会在出现在什么设备。

Fuchsia会是Android的终结者吗? 笔者认为至少未来五年内不太可能取代Android。当年为了和苹果iOS抗衡，Android系统研发作为Google重中之重，在这种情况下，Android诞生依然花费了Google 3年时间。而Fuchsia只是公司目前的实验项目，且Fuchsia并非基于业界成熟Linux内核，而是采用全新Zircon内核，项目工程路还很远。

笔者大致研究了一下Fuchsia系统源码，总结了一下Fuchsia的整个技术架构图如下。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxqxlcrxhj30qo0k0wg8.jpg)

从Fuchsia技术架构来看，内核层zircon的基础LK是专为嵌入式应用中小型系统设计的内核，代码简洁，适合嵌入式设备和高性能设备，比如IOT、移动可穿戴设备等，目前这些领域还没有标准化级别的垄断者。以及在框架层中有着语音交互、云端以及智能化等模块，由此笔者揣测未来Fuchsia率先应用在音控等智能设备。

Fuchsia基于功能的模块化操作系统，应该会使各组件模块能独立升级更新能力，保证体验一致性。Fuchsia在IOT领域占据一定份额后，加之其良好的跨平台，可以再逐步渗透到移动手机、笔记本电脑等设备，进而三位一体，打造手机、电脑与IOT完美的互联互通的统一平台体验，让多端设备都离不开Fuchsia。在2018年10月，在“蓝牙特别兴趣小组（Bluetooth SIG）”举办的UnPlugFest（UPF）测试大会上，Google再展示了Fuchsia与Android设备的互联性，可以窥见一斑。

Fuchsia的定位更是物联网，再是一统江湖，但路途漫长，至少要５年甚至更远。未来不可知，当然说不定Fuchsia作为实验项目，一直待在实验室，不过这种可能性比较小，做不到一统江湖，在IOT领域发光发亮还是大有可为的。

### 6. 展望未来

### 操作系统

移动操作系统的演变过程，从按键交互的塞班功能机到触摸屏交互的Android/IOS智能机，从小屏幕手机到全面屏、刘海屏、水滴屏。任何系统无非干两件事：输入和输出，接收到外部输入信号后经过操作系统处理后输出信息。

- 从按键式交互到触屏式交互，伴随着塞班系统到Android系统的转变。未来的交互方式一定会更加生物智能化，当下的触屏交互可以理解成人类的触觉输入方式，未来将朝着人们更常见的听觉输入（语音）和视觉输入（身体姿势、表情等），甚至嗅觉输入（气味变化），也会伴随着新的操作系统的诞生。需要更加无缝地切入生活，而不是“安静，你吵到我的TNT”方式。
- 屏幕从小尺寸到大尺寸，并没有引发操作系统变革，因为技术创新是非连续性，非连续性才会引发第二曲线，诞生新技术。从1960年大型机，到1990年个人笔记本，再到现在的智能手机，设备本身越来越小。未来的设备如果发展非连续变革，可能不再需要实体硬件，随处可输出，一张白纸、一面墙，到那时操作系统的UI架构必然全新的变化。

前面提到Fuchsia系统，笔者认为至少未来五年内不太可能取代Android，但未来可期。新操作系统的崛起源于降维打击，直线超车很难，需要有非连续变革，如果只是某种程度上的改进，很难突破用户习惯、厂商以及生态圈的阻碍。Fuchsia需要降维打击，比如Fuchsia在IOT领域以及新的交互方式都很出色，加上万物无缝式的互联互通的平台，拥有跨平台型特性的Fuchsia有机会成为超级平台。

Android发展至今，已成为全球用户量最广泛的移动操作系统，手机行业竞争异常激烈，经过几番洗牌，国内手机厂商主要是华米OV四大公司，笔者预测在未来五年内国内手机厂商可能只有TOP3，那么Android的未来在哪里呢？

Google在2014年发布Android Wear智能手表系统、Android TV系统以及Android Auto汽车系统，在2016年发布的Android Things智能设备，这些全方位构建安卓的生态圈。在未来在人工智能和5G的赋能下，智能汽车、智能家居、IOT都将会有广阔的市场前景。但就目前人工智能的奇点还没到来，技术还处于前期阶段，一旦奇点来临将会爆炸式发展，或将重新定义生活方式。

汽车的智能化和互联网化是未来一大趋势，Google这两年确实在汽车领域发力，Android Auto在过去一年的用户增长250％。天生的移动特性加上越来越多的互联网服务需求，汽车需要一个具备多种感知能力的系统，或将成为是继手机、电视后Android的下一重点开拓领域。受到驾驶安全的限制，车载场景正好需要将以往的触屏按钮的交互方式，转向语音交互和生物感知，车舱内是天然的语音交互场景，而不再是“安静，你吵到我的TNT”，语音和图像识别、人工智能等技术或许会在车载领域得到更大的发展。

### 职业发展

随着Android的发展，有些人对Android未来感到茫然，经常收到读者私信问，“前辈，从事Android是不是没有前途，找工作困难，希望能给点建议？”。早在2010年市场上有大量Android招聘，基本懂一点Java基础的就可以搞Android，当时是移动互联网创业的高峰期，号称只要做个App就可以创业。“风来了猪都能找到工作，风停了最先摔死的都是猪”，如果你觉得找工作难，那一定是你在混日子，Android中高级以上的人才一直都非常稀缺。

只要在Android领域深耕，做到极致，努力成为这个方向的专家，有精力再提升工程架构思维，软件工程思想都是相通的，境界会得到提升，即便再学习新东西也会非常快。只要一个领域做到极致，即便Android被淘汰了，换新领域面试官依然会相信你也能做到极致。千万不能用一年的工作能力混十年工作经历，否则你的市场价格连一年都不如，成为工作困难户。

### Android

Android系统迭代更新10余年，在用户体验、性能、功耗、安全、隐私等方面都取得很大的进步，后续版本会持续在内存、文件系统、虚拟机、图形图像等方向优化。随着Android系统功能越来越多，系统架构中有些模块未来可能会被重构，某些服务大锁制约性能，比如Android 8.0优化过binder大锁让性能显著提升。关于图形方面，Vulkan将会是未来Android平台的一个发展方向，尤其是游戏领域。

人工智能在Android系统上目前效果不太显著，Google未来应该还会持续投入，比如在AI预测用户行为加上相应后台管控策略用于提升手机续航。碎片化仍是当下最主要的问题，碎片化也导致用户隐私、安全和体验一致性方面得不到保障，Google专门成立团队致力于Android Mainline，从Android Q开始规划Mainline，未来的版本都将逐步迭代更新。最后说一点，App兼容性问题比较严重，据Google实验统计Android Q系统在全球Top 1000应用的兼容性不达标率4.3%，而中国Top 1000应用的兼容性不达标率17.6%，可见国内Android生态圈更为严峻得多，Google对隐藏API的限制就是一步长远之棋，短期内导致应用不兼容加剧，长期来看生态圈会逐步健康，最理想的情况就是Android系统大版本升级而App兼容性问题不再有，后续Google应和厂商会加强跟主流应用协作规范应用，搭建良好健康的Android生态圈。

以上是笔者对Android系统及对未来的一些拙见，欢迎业界同仁一起探讨。