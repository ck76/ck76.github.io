一、知识详解模块

1.dex/class深入讲解

2.jvm/dvm/art三个虚拟机的深入讲解

3.class Loader类加载器的深入讲解

二、热修复应用模块

1.热修复原理深入讲解

2.如何合理的接入开源的热修复框架

3.开源框架的原理及版本发布控制

三、插件化应用模块

1.插件化原理以及组件化的区别

2.如何将应用插件化

3.插件化能为我们带来那些好处

一、知识详解模块

1.Class文件结构深入解析(生成、执行)

2.dex文件结构深入解析(生成、执行)

3.Class与Dex的对比

Class文件是什么？如何生成、作用以及文件格式详解

其是能够被JVM虚拟机识别加载并执行的文件格式

(1)IDE编译器--build生成

(2)javac命令生成

(3)通过java命令去执行class文件 javac -target 1.6 -source 1.6 Hello.java

记录一个类文件的所有信息

Class文件结构解析：

(1)一种8字节的二进制流文件

(2)各个数据按顺序紧密的排列、无间隙

(3)每一个类或接口都独自占据一个class文件

http://blog.csdn.net/linzhuowei0775/article/details/49556621 Class类文件的结构  

u4 magic;  它的唯一作用是用于确定这个文件是否为一个能被虚拟机接受的Class文件

u2 minor_version;  次版本号

u2 major_version;  主版本号(java版本)

u2 constant_pool_count;  常量池的数量(这个容量计数是从1而不是从0开始)

cp_info constant_pool[constant_pool_count-1];  表类型数据集合，即常量池中每一项常量都是一个表，共有11种结构各不相同的表结构数据

u2 access_flags;  ----------------------------作用域

u2 this_class;  ------------------------------JVM生成的本类对象

u2 super_class;  -----------------------------JVM生成的父类对象

u2 interfaces_count;  ------------------------接口

u2 interfaces[interfaces_count];  

u2 fields_count;  ----------------------------变量

field_info fields[fields_count];  

u2 methods_count;  ---------------------------方法

method_info methods[methods_count];  

u2 attributes_count;  ------------------------属性

attribute_info attributes[attributes_count]; 

Class文件的弊端：

(1)内存占用大，不适合移动端

(2)JVM采用堆栈加载模式，加载速度慢

(3)文件IO操作多，类查找慢

Dex文件是什么？如何生成、作用、文件格式详解

其是一个能够被DVM识别，加载并执行的文件格式

(1)通过IDE自动--build生成

(2)通过DX命令生成dex文件 dx --dex --output Hello.dex Hello.class

(3)手动运行dex文件到手机 dalvikvm -cp /scard/Hello.dex Hello

记录整个工程中所有类文件的信息

Dex文件结构解析：

(1)一个8字节二进制流文件

(2)各个数据按照顺序紧密排列无缝隙

(3)整个应用中的所有Java源文件都放在一个Dex文件中

http://blog.csdn.net/feglass/article/details/51761902 Android Dex文件结构解析

主要分为三部分:文件头、索引区、数据区

header---文件头

​     索引区

string_ids--字符串的索引 

type_ids ---类型索引

proto_ids --方法原型的索引

field_ids ----域的索引

method_ids ---方法的索引

​	 数据区

class_def -----类的定义区

data -----------数据区

link_data -------链接数据区

Class与Dex的区别

(1)本质上两者是一样的，dex是从class文件演变过来的 dx --dex --output xx.dex xx.class

(2)class文件存在很多的冗余信息，dex会去除冗余并合并

(3)结构不一样

第二章 

1.JVM虚拟机结构解析

2.Dalvik与JVM的不同

3.ART与Dalvik相比有哪些优势

1.JVM虚拟机结构解析

(1)JVM整体结构讲解

(2)Java代码的编译和执行过程

(3)内存管理和垃圾回收

JAVA虚拟机、Dalvik虚拟机和ART虚拟机简要对比：

JVM:Java的跨平台性是由JVM来实现的，就是把平台无关的.class字节码翻译成平台相关的机器码，来实现的跨平台;

JVM在把描述类的数据从class文件加载到内存时，需要对数据进行校验、转换解析和初始化，最终才形成可以被虚拟机直接使用的JAVA类型，因为大量的冗余信息，会严重影响虚拟机解析文件的效率

DVM:为了减小执行文件的体积，安卓使用Dalvik虚拟机，SDK中有个dx工具负责将JAVA字节码转换为Dalvik字节码，dx工具对JAVA类文件重新排列，将所有JAVA类文件中的常量池分解，消除其中的冗余信息，重新组合形成一个常量池，所有的类文件共享同一个常量池，使得相同的字符串、常量在DEX文件中只出现一次，从而减小了文件的体积.

Dalvik执行的是dex字节码，依靠JIT编译器去解释执行，运行时动态地将执行频率很高的dex字节码翻译成本地机器码，然后在执行，但是将dex字节码翻译成本地机器码是发生在应用程序的运行过程中，并且应用程序每一次重新运行的时候，都要重新做这个翻译工作，因此，及时采用了JIT，Dalvik虚拟机的总体性能还是不能与直接执行本地机器码的ART虚拟机相比

ART:

Dalvik虚拟机执行的是dex字节码，ART虚拟机执行的是本地机器码

安卓运行时从Dalvik虚拟机替换成ART虚拟机，并不要求开发者重新将自己的应用直接编译成目标机器码，也就是说，应用程序仍然是一个包含dex字节码的apk文件。所以在安装应用的时候，dex中的字节码将被编译成本地机器码，之后每次打开应用，执行的都是本地机器码

JIT与AOT两种编译模式

JIT:即时编译技术，JIT会在运行时分析应用程序的代码，识别哪些方法可以归类为热方法，这些方法会被JIT编译器编译成对应的汇编代码，然后存储到代码缓存中，以后调用这些方法时就不用解释执行了，可以直接使用代码缓存中已编译好的汇编代码。这能显著提升应用程序的执行效率

AOT:

ART优点：

①系统性能显著提升

②应用启动更快、运行更快、体验更流畅、触感反馈更及时

③续航能力提升

④支持更低的硬件

ART缺点

①更大的存储空间占用，可能增加10%-20%

②更长的应用安装时间

总的来说ART就是“空间换时间”

JVM与DVM的对比:

(1)JAVA虚拟机运行的是JAVA字节码，Dalvik虚拟机运行的是Dalvik字节码

(2)Dalvik可执行文件体积更小

(3)JVM基于栈，DVM基于寄存器

注意:

在DVM虚拟机中，总是在运行时通过JIT把字节码文件编译成机器码，因此程序跑起来就比较慢，所以在ART模式(4.0引入,5.0设置为默认解决方案)上，就改为AOT预编译，也就是在安装应用或者OTA系统升级时提前把字节码编译成机器码，这样就可以直接运行了，提高了运行的效率。但是AOT的缺点，每次执行的时间都太长，且ROM空间占用有比较大，所以在Android N上google做了混合编译，即同时支持JIT+AOT。

简单来讲:安装的时候不做编译，而是JIT解释字节码，以便能够快速启动，在运行的时候分析运行过程中的代码以及区分“热代码”，这样就可以在机器空闲的时候对通过dex2aot这部分热代码采用AOT进行编译存储为base.art文件然后在下次启动的时候一次性把app image加载进来到缓存，预先加载代替用时查找以提升应用的性能，并且同一个应用可以编译数次，以找到“热”的代码路径或者对已经编译的代码进行新的优化。

JVM结构：

虚拟机内存区域分为:运行时数据区+(执行引擎+本地库接口+本地方法库)

运行时数据区:方法区、Java栈、Java堆、本地方法栈、程序计数器

Class文件----(通过类加载器子系统)-----加载到内存空间(方法区、JAVA堆、JAVA栈、本地方法栈)-------垃圾收集器(GC)

内存优化方案

JVM内存管理

1.JAVA栈(虚拟机栈):存储JAVA方法执行时所有的数据；存放基本型，以及对象引用。线程私有区域

其由栈帧组成，一个栈帧代表一个方法的执行,每个方法在执行的同时都会创建一个栈帧

JAVA栈帧--每个方法从调用到执行完成就对应一个栈帧在虚拟机中入栈到出栈

JAVA栈帧存储：局部变量、栈操作数、动态链接、方法出口.

2.JAVA堆:所有通过new创建的对象产生的内存都在堆中分配存放对象实例和数组，此区域也是垃圾回收器主要作用的区域。

特点：

是虚拟机中最大的一块内存，是GC要回收的部分

堆区内存:

Young generation(新创建的存储区)

Old generation(暂时用不到的，不可达的对象存储区)若是Young与Old都存储满了 就会爆出OOM异常

Permanent generation

分成两个的原因:有利于动态调整两个区域的大小

即时通信服务时调用MSG消息比较多，就可以把Young调整的大一些，这样便于内存的分配，加快对象的创建

3.本地方法栈:是专门为native方法提供服务的 虚拟机中的JIT即时编译单元负责本机系统中比如C语言或者C++语言和Java程序间的互相调用，

这个Native Method Stack就是用来存放与本线程互相调用的本机代码

4.方法区:存储被虚拟机加载的类信息、常量、静态变量、即时编译器编译后的数据,是所有线程的共享区域。

也称为永久代（Permanent Generation）但随着Java8的到来，已放弃永久代改为采用Native Memory来实现方法区的规划。

此区域回收目标主要是针对常量池的回收和对类型的卸载

5.程序计数器：可看做是当前线程所执行的字节码的行号指示器；如果线程在执行Java方法，这个计数器记录的是正在执行的虚拟机字节码指令地址；

如果执行的是Native方法，这个计数器的值为空（Undefined）

垃圾回收：

垃圾收集算法(垃圾确认)

1.引用计数算法(JDk1.2之前提供)

优点：实现简单，判定效率高

缺点：两个不可达的对象相互引用，导致内存无法回收。

2.JDK1.2之后采用可达性算法(根搜索算法)---采用离散数学原理

从GC Roots为根节点一直向下遍历，找到下一个节点。。。。，这样找不到的就是不可达的，那这些就是垃圾，被垃圾回收器回收

引用的类型：

强引用(弱引用内存不足也不会回收，除非OOM，它是内存泄漏的主要原因之一)通常new产生的对象就是强引用

软引用(SoftReference内存充足时，保持引用，内存不足时，进行回收)

弱引用(WeakReference不管JVM的内存空间是否足够，总会回收该对象占用的内存)

虚引用

垃圾回收算发：(垃圾回收)

1.标记清除算法--

好处:不需要对象的移动,并且仅对不存活的对象进行处理

在存活对象比较多的情况下极为高效，效率问题，标记和清除两个过程的效率都不高

坏处：一般是使用单线程操作，直接回收不存活对象，会造成大量的不连续的内存碎片，从而不利于后续对一些对象的分配

2.复制算法

好处:在存活对象比较少时极为的高效，实现简单，运行高效；每次都是对整个半区进行内存回收，内存分配时也不需要考虑内存碎片等情况，只要移动堆顶指针，按顺序分配内存即可

坏处:需要一块内存空间作为交换空间

3.标记--整理算法--标记清除算法基础之上的一个算法,解决了内存碎片的问题，主要针对对象存活率高的老年代

4.分代收集算法---根据对象的存活周期的不同将内存划分为几块，一般是把Java堆分为新生代和老年代，这样就可以根据各个年代的特点采用最适当的收集算法。

在新生代中，每次垃圾收集时都会发现有大量对象死去，只有少量存活，那就选用复制算法，只需要付出少量存活对象的复制成本就可以完成收集。而老年代中因为对象存活率高、

没有额外空间对它进行分配担保，就必须使用标记—清除算法或标记—整理算法来进行回收

垃圾回收的触发机制：(垃圾回收的时机)

java虚拟机无法再为新的对象分配内存空间了

手动调用System.gc()方法

低优先级的GC线程，被JVM启动时就会执行GC

Dalvik VM 与JVM的不同

1.执行的文件不同，一个是class的一个是dex的

2.类加载的系统与JVM区别较大

3.可以同时存在多个dvm

4.Dalvik是基于寄存器的，而JVM是基于堆栈的

Dalvik VM与ART的不同之处

DVM使用JIT(即时编译每次运行都会执行编译)来将节码转换成机器码，效率低

ART采用了AOT预编译技术，执行速度更快(JDK 1.9 代码的分段缓存技术，执行速度更快)

ART会占用更多的应用安装时间和存储空间

第三章：

Java中的ClassLoader回顾

Android中的ClassLoader的作用详解

Android中的动态加载比一般Java程序复杂在哪里

Android ClassLoader概述

Android中ClassLoader的种类

Android中ClassLoader的特点

ClassLoader源码讲解

Java中的ClassLoader回顾

BootstrapClassLoader: Load JRE\lib\rt.jar或者Xbootclasspath选项指定的jar包

ExtensionClassLoader: Load JRE\lib\ext\*.jar或者Djava.ext.dirs指定目录下的Jar包

AppClassLoader: Load CLASSPATH或者Djava.class.path所指定的目录下的类和Jar包

CustomClassLoader:通过java.lang.ClassLoader的子类自定义加载Class

Android中ClassLoader的种类

BootClassLoader:Load JRE\lib\rt.jar或者Xbootclasspath选项指定的jar包及Framework层的一些Class字节码文件

PathClassLoader:只能加载已经安装到Android系统中的apk文件（/data/app目录）,是Android默认使用的类加载器。

DexClassLoader:可以加载任意目录下的dex/jar/apk/zip文件,比PathClassLoader更灵活,是实现热修复的重点。

BaseDexClassLoader:是PathClassLoader与DexClassLoader的父类

一个应用程序最少需要

BootClassLoader、PathClassLoader两个类加载器

特点及作用

双亲代理模型的特点

(先判断类是否是被当前的ClassLoader加载过，加载过则返回，没有加载过在去看其父类ClassLoader是否加载过，加载过返回，否的话，最终由其子类ClassLoader去加载)

这个特点也就意味着一个类被位于事务中的任意一个ClassLoader加载过，那么在今后的整个系统的生命周期中，该类就不会再重新被加载了，大大提高了类加载的一个效率。

类加载的共享功能(Framework里面的一些类一旦被顶层的ClassLoader加载过，就会保存在缓存里面，以后就不要重新加载了)

类加载的隔离功能(不同继承路径上的ClassLoader加载的类肯定不是同一个类，可以防止用户数据被篡改，防止自定义类冒充核心类库，以访问核心类库中的成员变量)

双亲委派机制的描述:

当某个特定的类加载器在接到类加载的请求时，首先将加载任务委托给父类加载器，依次递归，如果父类加载器可以完成类的加载任务，就成功返回；

只有父类加载器无法完成加载任务时，自己才去加载。

意义:防止内存中出现多份同样的字节码；例如:

比如两个类A和类B都要加载System类：

如果不用委托而是自己加载自己的，那么类A就会加载一份System字节码，然后类B又会加载一份System字节码，这样内存中就出现了两份System字节码。

如果使用委托机制，会递归的向父类查找，也就是首选用Bootstrap尝试加载，如果找不到再向下。这里的System就能在Bootstrap中找到然后加载，

如果此时类B也要加载System，也从Bootstrap开始，此时Bootstrap发现已经加载过了System那么直接返回内存中的System即可而不需要重新加载，这样内存中就只有一份System的字节码了

双亲委派机制也就是热修复在技术上可以实现的根本依据，即:多个dex文件组成一个数组Element[],按照顺序加载，对于一个已经加载的Class是不会再次加载的。

同一个ClassLoader加载过的类可以称之为同一个类（还要有相同的包名、类名）

Android ClassLoader加载顺序：

ClassLoader.java中 loadClass()方法，在该方法中判断是否是被加载过，若没有主要用到了findClass()方法，而在ClassLoader.java中，findClass()是一个空实现，于是到PathClassLoader，DexClassLoader中查看

发现两者都是共同父类BaseDexClassLoader中实现的,此时调用到了DexPathList对象的findClass()方法，而DexPathList对象实在BaseDexClassLoader的构造方法中通过new实现的，于是找到

DexPathList类的构造方法中找到makeDexElements()方法，该方法返回一个Element[]数组，在该方法makeDexElements()中遍历所有的Dex文件，调用loaddexFile()方法，在该方法中

返回了DexFile对象，并最终存到Element[]数组中。此时，再来看DexPathList对象的findClass()方法中遍历Element[]数组，对数组中的每一个Element元素(DexFile)调用DexFile对象的

loadClassBinaryName()方法返回一个Class对象,而loadClassBinaryName()最终调用的是Native方法defineClassNative()该方法是C与C++实现的。

Android动态加载的难点：

1.有许多组件类需要注册之后才能使用

2.资源的动态加载很复杂

3.Android程序运行时需要一个上下文环境

第四章热修复详解：

热修复的基本概念讲解

当前市面上比较流行的几种热修复技术

方案对比及技术选型

什么是热修复？

动态修复、动态更新

热修复有哪些好处

热修复的流行技术：

QQ空间的超级补丁方案

微信的Tinker

阿里的AndFix，dexposed(最新版本Sophix 3.2.0)

美团的Robust,饿了吗的migo，百度的hotfix

第五章 本章概述

AndFix的基本介绍

AndFix执行流程及核心原理

使用AndFix完成线上bug修复

AndFix源码讲解

https://www.cnblogs.com/wetest/p/7595958.html

https://segmentfault.com/a/1190000011365008

http://blog.csdn.net/lostinai/article/details/54694959

AndFix修复即时生效的原理:

腾讯系的方案为什么不能及时运行?

腾讯系的方案是基于类加载机制采用全量替换的原理完成的，由于类加载的双亲代理特点，已加载到内存中的Class是不会再次加载的。

因此，采用腾旭系方案，不会及时生效，不重启则原来的类还在虚拟机中，就无法加载新类，当再次启动的时候，去加载新合成的dex文件，以完成增量更新。

这样Tinker将新旧两个DEx的差异放在补丁包中，下发到移动端，在本地采用反射的原理修改dexElements数组，合成完整的dex文件。

但是全量替换会造成补丁包体积过大，因此，采用了Dexdiff算法，大大优化下发差异包的大小。

在Android N中完全废弃掉PathClassloader，而采用一个新建Classloader来加载后续的所有类，即可达到将cache无用化的效果。

AndFix的方案采用底层替换方案，即:通过修改一个方法包括方法入口地址在内的每一项数据，使之指向一个新的方法。

具体来讲就是:

通过反射机制得到所要替换的Method对象所对应的object,进而找到这个方法底层Java函数所对应ArtMethod的真实地址，把旧函数的所有变量都替换为新函数的

因为，每一个java方法在DVM/ART中都对应着一个底层Java函数,在Art模式中是:ArtMethod,ArtMethod记录了这个Java方法的所有信息，包括所属类、访问权限、代码执行地址等等。

通过env->FromReflectedMethod，可以由Method对象得到这个方法对应的ArtMethod的真正起始地址。然后就可以把它强转为ArtMethod指针，从而对其所有成员进行修改。

这样全部替换完之后就完成了热修复逻辑。以后调用这个方法时就会直接走到新方法的实现中了

AndFix兼容性的根源所在:

采用的Native替换方案:都是写死了ArtMethod结构体(Andfix是把底层结构强转为了art::mirror::ArtMethod),AndFix里面的ArtMethod是遵照android虚拟机art源码里面的ArtMethod构建的。

由于Android是开源的，各个手机厂商都可以对代码进行改造，而Andfix里ArtMethod的结构是根据公开的Android源码中的结构写死的，若是厂商对开源的ArtMethod结构体进行了修改，

和源码里面的结构体不一致，替换则出现了问题。

优化策略:

直接替换ArtMethod而不是直接替换，以解决兼容性问题，

即时生效带来的限制:只能是方法级别的替换，不能添加新的字段，不能增加方法,以防止在计算sizeOf()时出现差错。

AndFix支持从2.3到7.0的Android版本，包括ARM和X86架构,Dalvik和ART运行时，32位和64位

AndFix的实现原理是方法的替换，使得有Bug的方法永远都不会被执行到。(是阿里的热修复开源版本)当前最新的

热修复版本是阿里HotFix 3.0(Sophix 3.2.0)

https://help.aliyun.com/document_detail/61082.html?spm=a2c4g.11186623.6.547.3I5XDy Sophix 3.2.0的稳健接入



dexposed和andfix是alibaba的开源项目，都是apk增量更新的实现框架，目前dexposed的兼容性较差，只有2.3，4.0~4.4兼容，

其他Android版本不兼容或未测试，详细可以去dexposed的github项目主页查看，而andfix则兼容2.3~6.0，所以就拿这个项目来实现增量更新吧。

AndFix的集成阶段

Gradle添加依赖 compile 'com.alipay.euler:andfix:0.5.0@aar'

1.https://github.com/hanfengzqh/AndFix AndFix的GitHub地址

1.AndFix的 PatchManager 的初始化

mPatchManager = new PatchManager(context);

mPatchManager.init(Utils.getVersionName(context));//

//进行热修复之后，为了将SortedSet mPatchs中的Patch每一个文件取出调用mAndFixManager.fix()；

mPatchManager.loadPatch();

2.加载我们的patch文件

if (mPatchManager != null) {

//网络有热修复文件(.apatch)时调用，将.apatch补丁文件,复制到包内/apatch/文件夹下，并将

//包内文件夹下的.apatch文件Patch文件，转换为调用loadPatch(Patch)方法

mPatchManager.addPatch(path);

}

AndFix的准备阶段

1.build一个有bug的old apk并安装到手机上

2.分析问题解决bug后，build一个new apk

补丁会生成一个以.apatch结尾的文件

AndFix bug修复的常用两条指令：

usage: apkpatch -f  -t  -o  -k  -p <***> -a  -e <***> 新建

 -a,--alias    keystore entry alias.

 -e,--epassword <***>  keystore entry password.

 -f,--from      new Apk file path.

 -k,--keystore    keystore path.

 -n,--name     patch name.

 -o,--out      output dir.

 -p,--kpassword <***>  keystore password.

 -t,--to       old Apk file path.

usage: apkpatch -m  -o  -k  -p <***> -a  -e <***> 合并

 -a,--alias    keystore entry alias.

 -e,--epassword <***>  keystore entry password.

 -k,--keystore    keystore path.

 -m,--merge    path of .apatch files.

 -n,--name     patch name.

 -o,--out      output dir.

 -p,--kpassword <***>  keystore password.

Patch安装阶段：

1.将apatch 文件通过adb push 到手机上

2.使用户已经安装的应用load我们的apatch文件

3.load成功后验证我们的bug是否被修复

AndFix优劣：

1.原理简单，集成简单，使用简单，即时生效(Andfix采用的方法是，在已经加载了的类中直接在native层替换掉原有方法，是在原来类的基础上进行修改的)

2.只能修复方法级别的bug，极大限制了使用场景

http://blog.csdn.net/cocoooooa/article/details/51096613 Android 热修补方案（AndFix）源码解析 涉及到JNI层

https://yq.aliyun.com/articles/74598?spm=a2c4g.11186623.2.33.DpP3QL Android热修复升级探索——追寻极致的代码热替换

AndFix劣势补充：https://www.cnblogs.com/aimqqroad-13/p/5965683.html AndFix的限制因素

1.不支持YunOS在线推送。

2.不支持添加新的类和新的字段，(官网)不支持构造方法、参数数目大于8或者参数包括long,double,float基本类型的方法。

3.需要使用加固前的apk制作补丁，但是补丁文件很容易被反编译，也就是修改过的类源码很容易暴露

4.使用有些加固平台可能会使热补丁功能失效（亲测：360加固不存在该问题，是在dex外侧加一层壳）

5.andfix不支持布局资源等的修改(即：在热修复时新旧apk的版本号不可更改，只能一致，否则“.apatch”补丁包 无法生成)

6.潜在问题:加载一次补丁之后，out.apatch文件会copy到getFileDir目录下的/apatch文件夹中，

在下次补丁更新时，会检测补丁是否已经添加在apatch文件夹下，若是存在就不会复制加载sdcard的out.apatch

7.AndFix并没有提供多次修复的解决方案，需要自己封装

(1)https://www.jianshu.com/p/58fc4c2cb65a andfix 多次修改同一个方法报错的解决 Gradle引入方式

(2)https://yq.aliyun.com/articles/63774?spm=5176.10695662.1996646101.searchclickresult.49db6001ZX2cgM android 热修补之andfix实践(多次修复) Module

(2.1)有新补丁.apatch文件，调用mPatchManager.addPatch(patchPath)之后，将SD卡下下载的.apatch补丁文件删除掉，以避免启动都会复制加载一次。

(2.2)在addPatch()内部判断包内apatch/文件夹下存在的.apatch文件删除掉，以便放入新的.patch补丁文件

8.无安全机制

AndFix源码解析：

1.PatchManager是一个典型的外观模式，把api封装在其内部，只留下初始化init()/loadPatch()/addPatch()等方法

PatcthManager内部几个重要的成员变量：

AndFixManager 主要的修复工具类(譬如：removeOptFile()移除文件)

SortedSet mPatchs;里面放置了所有的patch补丁文件

在PatchManager构造方法里面就是各主要成员变量的初始化操作，以及创建 apatch与apatch_opt文件夹

2.init(appVersion)初始化操作过程中，完成对Patch文件的添加与删除操作集合mPatchs。

(1)若是版本号不同(即:版本升级了)就会把包内目录下的apatch与apatch_opt文件夹下的.apatch文件删掉

(2)若是版本号相同就把所有的.apatch文件转化为Patch文件添加到mPatchs里面，以便loadPatch()加载使用。

3.loadPatch()方法加载，调用loadPatch(Patch)最终调用mAndFixManager.fix()方法，

在该方法(存在于AndFixManager类里面)里面验证签名，若是签名验证通过则将Patch中的File转化为DexFile,

在while循环里面遍历DexFile文件，通过dexFile.loadClass()方法查找我们真正需要的Class

然后再调用fixClass()方法，在该方法内部通过反射获取有 MethodReplace.class 注解标记的方法

在调用 replaceMethod()完成方法替换，在该方法中调用AndFix.addReplaceMethod(src, dest);

最终调用native 方法 replaceMethod()，最终完成方法替换。

@AndFix/src/com/alipay/euler/andfix/AndFix.java

private static native void replaceMethod(Method src, Method dest);

src 需要被替换的原有方法；

dest 对应的就是新方法

4.addPatch(patchPath)最终也是调用了loadPatch(Patch)也是调用mAndFixManager.fix()方法

JNI层:

@AndFix/src/com/alipay/euler/andfix/AndFix.java

private static native void replaceMethod(Method src, Method dest);

@AndFix/jni/andfix.cpp

static void replaceMethod(JNIEnv* env, jclass clazz, jobject src,

​    jobject dest) {

  if (isArt) {

​    art_replaceMethod(env, src, dest);

  } else {

​    dalvik_replaceMethod(env, src, dest);

  }

}

最终根据Method对象找到底层Java函数对应的ArtMethod的真实地址，完成方法的替换

必知必会：

AndFix的原理及执行流程

AndFix的集成及基本用法

AndFix组件化的思路和代码实现

第六章 Tinker的相关学习

1.Tinker的基本介绍

2.Tinker的执行原理及流程

3.使用Tinker完成线上bud修复

4.Tinker源码讲解

1.Tinker基本介绍：

开源项目,微信官方的Android热补丁解决方案,支持动态下发代码、so、资源，在不需要重新安装的情况下实现更新。

支持2.x-7.x版本

Tinker主要包含以下三个部分：

1.gradle编译插件：tinker-patch-gradle-plugin

2.核心SDK：tinker-android-lib

3.非gradle编译用户的命令行版本：tinker-patch-cli-1.9.1.jar

Tinker的核心原理

基于Android原生的ClassLoader，开发了自己的ClassLoader

基于Android原生的AAPT，开发了自己的AAPT

微信团队基于Dex文件格式，研发了自己的DexDiff算法

Tinker的优劣：

(1)Tinker不支持修改 AndroidManifest.xml,Tinker不支持新增四大组件

(2)由于Google Play的开发者条款限制，不建议在GP渠道动态更新代码

(3)在Android N上，补丁对应用的启动时间有轻微的影响

(4)不支持部分三星android-21机型

(5)对于资源替换，不支持修改 remoteView。例如transition动画，notification icon以及桌面图标。

https://github.com/Tencent/tinker/wiki  Tinker -- 微信Android热补丁方案

若出现资源变更，我们需要使用applyResourceMapping方式编译，这样不仅可以减少补丁包大小，同时防止remote view id变更造成的异常情况

Tinker集成阶段：

Gradle中添加Tinker依赖

在代码中完成对Tinker的初始化

因为Tinker需要监听Application的生命周期，在ApplicationLike委托对象里面可以完成Tinker对Application生命周期的监听。

通过ApplicationLike完成Tinker的初始化操作

准备阶段

1.build一个old.apk并安装到手机

2.修改一些功能之后，build一个new.apk

patch文件的生成:

1.命令行的方式完成patch的生成

各个文件的作用及命令参数讲解

通过使用tinker-patch命令生成patch文件

2.利用Gradle插件的方式完成patch包的生成

1.patch命令行方式：

java -jar tinker-patch-cli-1.7.7.jar -old old.apk -new new.apk -config tinker_config.xml -out output_path

(1)修改apk前后不一样的apk，修改资源文件

(2)tinker-config.xml中更改loader标签为ApplicationLike类中注解 @DefaultLifeCycle中的自定义application全路径

(3)修改加密文件、密码及别名及密码

(4)

​      android:value="tinker_id_b168b32"/>

2.Gradle插件配置生成

(1)gradle中正确配置tinker参数(非常重要)

(2)在android studio中直接生成patch文件

在ApplicationLike自定义类中的onBaseContextAttached()方法中

TinkerInstaller.install(ApplicationLike);//完成tinker初始化

调用

if (Tinker.isTinkerInstalled()) {

   TinkerInstaller.onReceiveUpgradePatch(getApplicationContext(), patch);

}

完成patch补丁文件的加载//TinkerInstaller.onReceiveUpgradePatch();

几个重要的部分：

安全校验：无论是补丁合成还是补丁加载，我们都要进行必要的安全校验

版本管理：Tinker支持补丁升级，甚至是多个补丁不停的切换。这里要保证所有进程版本的一致性

补丁加载：通过反射加载我们合成的dex,so,资源文件等

补丁合成：这些都是在单独的patch进程中完成的，这里包括dex,so,资源，主要完成补丁包的合成与升级。

监听回调：在合成与加载的过程中，出现问题及时回调。

1.https://www.jianshu.com/p/6e09f0766af3 微信热补丁Tinker -- 补丁流程

2.http://blog.csdn.net/tyk9999tyk/article/details/53391519  Tinker 常见问题 

Tinker库中有什么类是不能修改的？

Tinker库中不能修改的类一共有25个，即com.tencent.tinker.loader.*类。加上你的Appliction类，只有25个类是无法通过Tinker来修改的

Tinker多补丁版本发布：

Tinker支持对同一基准版本做多次补丁修复，在生成补丁时，oldApk依然是已经发布出去的那个版本。

即补丁版本二的oldApk不能是补丁版本一，它应该依然是用户手机上已经安装的基准版本。

Tinker的高级功能：

1.Tinker如何支持多渠道打包

2.如何自定义Tinker行为

3.tinker使用过程中需要注意到的哪些问题

1.多渠道打包

(1)命令行方式智能一个渠道一个渠道的打tpatch

(2)gradle方式则只需简单的修改一下gradle脚本即可

步骤：

(1).在AndroidManifest.xml中添加 

​    android:name="MY_CHANNEL"

​    android:value="${MY_CHANNEL}" />

或者其他途径

(2).在build.gradle中添加productFlavor

productFlavors {//多渠道脚本支持

xiaomi {

  manifestPlaceholders = [MY_CHANNEL: "xiaomi"]

  buildConfigField "String", "AUTO_TYPE", "\"1\""

}

baidu {

  manifestPlaceholders = [MY_CHANNEL: "baidu"]

  buildConfigField "String", "AUTO_TYPE", "\"2\""

}

_360 {

  manifestPlaceholders = [MY_CHANNEL: "_360"]

  buildConfigField "String", "AUTO_TYPE", "\"3\""

}

productFlavors.all {

  flavor -> flavor.manifestPlaceholders = [MY_CHANNEL: name]

}

  }

(3).修改tinker脚本添加多渠道的目录tinkerBuildFlavorDirectory = "${bakPath}/tinkergradle-0218-12-22-06"

最重要的是拷贝目录，完成多渠道目录的拼凑

2.Tinker的自定义行为

(1)自定义PatchListener监听patch receiver事件 ，继承于 DefaultPatchListener

  作用：DefaultPatchListener implements PatchListener

  而PatchListener中只有onPatchReceived(String path)方法，因此自定义的必须得实现

  在该方法中调用int returnCode = patchCheck(path,patchMd5);当returnCode==0校验通过，就会调用

  TinkerPatchService.runPatchService(context, path);开启一个新的后台进程进行补丁合成



  校验patch文件是否合法；启动service去安装patch文件

(2)自定义TinkerResultService改变patch安装成功后的行为，继承于DefaultTinkerResultService extends AbstractResultService extends IntentService

  作用：决定在patch安装完成之后的后续操作，默认实现是杀死进程

  重写onPatchResult(PatchResult result)方法，删除杀死进程的方法，提高用户体验

Tinker自定义初始化个参数：

LoadReporter loadReporter = new DefaultLoadReporter(getApplicationContext());//patch加载阶段各种异常情况的监听回调

1.回调运行在加载的进程，它有可能是各个不一样的进程。我们可以通过tinker.isMainProcess或者tinker.isPatchProcess知道当前是否是主进程，patch补丁合成进程。

2.回调发生的时机是我们调用installTinker之后，某些进程可能并不需要installTinker

PatchReporter patchReporter = new DefaultPatchReporter(getApplicationContext());//patch文件合成阶段各种异常情况的监听回调

isUpgrade：区分补丁合成的类型。是由于文件丢失而发起的RepariPatch, 还是收到新的补丁而发起的UpgradePatch

patchListener = new CustomPatchListener(getApplicationContext());//反馈

PatchListener类是用来过滤Tinker收到的补丁包的修复、升级请求，也就是决定我们是不是真的要唤起:patch进程去尝试补丁合成。我们为你提供了默认实现DefaultPatchListener.java。

一般来说, 你可以继承DefaultPatchListener并且加上自己的检查逻辑，例如SamplePatchListener.java。

若检查成功，我们会调用TinkerPatchService.runPatchService唤起:patch进程，去尝试完成补丁合成操作。反之，会回调检验失败的接口。事实上，你只需要复写patchCheck函数即可。

若检查失败，会在LoadReporter的onLoadPatchListenerReceiveFail中回调。

public int patchCheck(String path, boolean isUpgrade)

AbstractPatch  abstractPatch  = new UpgradePatch();

TinkerInstaller.install(mAppLike,

​	loadReporter,patchReporter,

​	patchListener,CustomResultService.class,

​	abstractPatch);//完成tinker初始化

http://blog.csdn.net/tyk9999tyk/article/details/53391525 Tinker 自定义扩展 

Tinker的源码分析：

Tinker初始化过程中：

使用到了外观模式TinkerInstaller.install()、构建者模式Tinker tinker = new Tinker.Builder()、单例模式public static Tinker with(Context context)

Tinker的patch补丁文件的加载合成过程

TinkerInstaller.onReceiveUpgradePatch(getApplicationContext(), patch);

最核心的方法就是

TinkerPatchService.runPatchService(context, path);开启一个新的后台进程进行补丁合成

TinkerPatchService extends IntentService

最终是在 onHandleIntent(Intent intent)方法中完成patch文件的加载与合成

调用public abstract boolean tryPatch(Context context, String tempPatchPath, PatchResult patchResult);方法

在UpgradePatch extends AbstractPatch的tryPatch()方法中分别调用

 if (!DexDiffPatchInternal.tryRecoverDexFiles(manager, signatureCheck, context, patchVersionDirectory, destPatchFile)) {

  TinkerLog.e(TAG, "UpgradePatch tryPatch:new patch recover, try patch dex failed");

  return false;

}

if (!BsDiffPatchInternal.tryRecoverLibraryFiles(manager, signatureCheck, context, patchVersionDirectory, destPatchFile)) {

  TinkerLog.e(TAG, "UpgradePatch tryPatch:new patch recover, try patch library failed");

  return false;

}

if (!ResDiffPatchInternal.tryRecoverResourceFiles(manager, signatureCheck, context, patchVersionDirectory, destPatchFile)) {

  TinkerLog.e(TAG, "UpgradePatch tryPatch:new patch recover, try patch resource failed");

  return false;

}

完成dex,res,lib加载与合并

必知必会

Tinker两种集成使用方式

使用Tinker完成对应用的动态更新

如何自定义patch加载及加载完成后的行为

第七章 代码及版本发布管理

1.加入动态更新之后如何管理我们的代码分支

2.加入动态更新之后如何管理我们的发版节奏

分支管理(普通)

master分支

dev分支--开发分支

引入动态更新之后的分支管理

除了master分支和dev分支，引入hot_fix分支

hot_fix分支专门用来管理动态更新迭代

sourceTree git代码界面化管理软件

第八章 插件化讲解

1.插件化相关知识介绍

2.插件化原理与实践

1.插件化相关知识介绍

插件化产生的背景：

(1)app的体积原来越庞大，功能模块越来越多

(2)模块耦合度高，协同开发沟通成本极大

(3)方法数可能会超过65535，占用内存过大

(4)为了平行高效开发。

插件化的优点:

1)模块解耦，应用程序扩展性强

2)解除单个dex函数不能超过 65535的限制

3)动态升级，下载更新节省流量

4)高效开发(编译速度更快)

基于以上问题 如何解决？

1.将一个大的APK按照业务分割成多个小的APK

2.每一个小的APK即可以独立运行又可以作为插件运行

基于Android动态加载技术以上是可以实现的

插件化带来的优势：

业务模块基本完全解耦

高效并行开发(编译速度更快)

按需加载，内存占用更低等等

插件化的相关概念

宿主：主APP，可以加载插件，也称之为Host

插件：插件APP，被宿主加载的App，可以是与普通app一样的apk

插件化：将一个应用按照宿主插件的方式改造就叫做插件化

相关概念对比：

(与组件化对比)

组件化是一种编程思想，而插件化是一种技术

组件化是为了代码的高度复用性而出现的

插件化是为了解决应用越来越大而出现的

(与动态更新对比)

与动态更新一样，都是动态加载技术的应用

动态更新是为了解决线上bug或者小功能的更新而出现

插件化是为了解决应用越来越庞大而出现的

https://www.jianshu.com/p/1c5afe686d75 Android组件化框架设计与实践

https://segmentfault.com/a/1190000009577849 使用ARouter实现组件化

https://www.jianshu.com/p/c0eecbbf1481 谈谈App的统一跳转和ARouter

https://www.jianshu.com/p/7cb2cc9b726a [Alibaba-ARouter] 简单好用的Android页面路由框架

http://blog.csdn.net/zhaoyanjun6/article/details/76165252 Android 路由框架ARouter最佳实践  



组件化产生的背景:

1.代码量膨胀，不利于维护，不利于新功能的开发及测试

2.项目工程的构建速度慢

3.代码之间的耦合严重

4.做不到按需加载打包

组件化的优势:

代码简洁，冗余量少，维护方便，易扩展新功能。

提高编译速度，从而提高并行开发效率。

避免模块之间的交叉依赖，做到低耦合、高内聚。

引用的第三方库代码统一管理，避免版本统一，减少引入冗余库。

定制项目可按需加载，组件之间可以灵活组建，快速生成不同类型的定制产品。

制定相应的组件开发规范，可促成代码风格规范，写法统一。

系统级的控制力度细化到组件级的控制力度，复杂系统构建变成组件构建。

每个组件有自己独立的版本，可以独立编译、测试、打包和部署。

组件化与模块化的区别？

1.模块化关注点是功能，组件化的关注点是复用性，更加注重分离。

2.通常一个模块化包含几个组件

3.两者划分的粒度不一样，组件化粒度更小，组件化更加侧重于单一功能的内聚，偏向于解耦

组件化思路:首先就是解耦，形式就是每一个Module自己能够运行

组件分为两类：技术组件和业务组件

技术组件：合理封装，避免技术组件库过大；

业务组件：选择性依赖技术组件可以使得业务组件单独运行

组件化实施过程:

1.对于技术组件需要合理封装，减少之后可能存在的替换成本；

2.同时注意，将技术组件分为常用和非常用，可以选择自己需要的技术组件，避免一个统一的技术组件库过大；

3.将业务代码根据模块进行剥离，剥离成一个个的小模块；

4.单独的业务模块加上必要的技术组件，支撑在开发阶段的独立运行；

组件化难点:

1.技术组件的整理、抽离

2.一定要有的DisPatcher:提供隐式的跳转和模块间方法的调用能力；

3.组件的划分

4.调试及集成方式

组件化实战:构建中间层，只让组件对中间层单方面耦合，中间层不对其他模块发声耦合。

组件之间的通信问题:

通过接口+实现的结构进行组件间的通信。即:每个组件声明自己提供服务的 Service API,这些Service都是一些接口,

组件负责将这些Service 接口实现并注册到一个统一的路由Router中去。因此，如果要使用某个组件的功能，只需要向Router请求这个Service的实现。

在组件化架构设计图中 Common 组件就包含了路由服务组件，里面包括了每个组件的路由入口和跳转。

组件化的概念就类似于Binder通信

组件化下的UI跳转问题:

UI跳转也是组件通信的一种，一般通过短链的方式进行，跳转到具体的Activity。每个组件注册自己所能处理的短链Scheme和Host，并定义传输数据的格式，

然后注册到统一的 UIRouter 中，UIRouter 通过 Scheme 和 Host 的匹配关系负责分发路由。但目前比较主流的做法是通过在每个 Activity 上添加注解，

然后通过 APT 形成具体的逻辑代码。目前方式是引用阿里的 ARouter 框架，通过注解方式进行页面跳转。

动态加载技术：

动态更新/热更新/热修复

插件化

封装、设计模式----组件化

插件化相关原理讲解

相关知识：

android ClassLoader加载class文件原理

Java反射原理

android资源加载原理

四大组件加载原理

Gradle打包原理

一、插件化对Manifest清单文件的处理

宿主 ManiFest/Aar Manifest/Bundle Manifest....------(合并Merge APK Manifest)---->(解析Bundle BundleInfoList)

流程：

1.构建期进行全量Merge操作

2.Bundle的依赖单独Merge，生成Bundle的Merge manifest

3.解析各个Bundle的Merge Manifest，得到整包的BundleInfoList

二、插件化框架对插件类的加载

插件化框架为插件类提供其加载所需的ClassLoader,用来完成插件类的加载

插件化框架提供ClassLoader进行插件类加载，涉及到两个问题：

1.如何定义ClassLoader加载类文件，继承DexClassLoader

2.如何调用插件APK文件中的类

即：创建了ClassLoader并且能够加载插件中的类，才能算得上是有用的ClassLoader

三、资源加载

1.若资源只有文件名File Name，则需要AssetManager通过文件名直接加载对应的资源。

2.若资源有对应的Resource ID(譬如：图片、动画、字符串等是会生成Resource ID的)因此需要使用到

Resources这个类完成对应资源文件的加载，加载完之后再使用AssetManager完成对资源文件的读写

以上可以得知：要想完成资源的加载需要用到Resources与AssetManager两个类

而安装到系统中的APK通过Context就可以完成对这两个类的直接引用，从而完成对资源的加载

而插件类的资源文件，因为插件并没有安装，因此这些插件类就没有自己的资源相关的Resources与AssetManager，因此这两个类就需要插件化框架帮我们完成动态的去创建

当前流行的插件化框架加载资源文件，是为每一个Bundle(插件)分别创建一个AssetManager完成对应插件的加载。

在插件被调用的时候将插件的Res,So等的资源文件目录路径，通过反射的方式加入到AssetManager，这样我们的AssetManager就知道了插件res,so资源文件存放路径，因此

插件化对资源加载的核心原理是：

想办法为每一个插件创建对应的AssetManager,加载插件中的资源路径

插件化的核心技术：

1.处理所有插件Apk文件中的Manifest文件(Bundle Manifest 合并到宿主Manifest)

2.管理宿主apk中所有的插件apk信息(因为不止有一个插件)

3.为每一个插件apk创建对应的类加载器，资源管理器，以完成对插件资源的加载

第九章 插件化框架实战

市面上现有的插件化框架介绍

具体使用一种框架完成插件化改造

市面上插件化框架

360手机助手的DroidPlugin框架

百度的dynamic-load-apk框架

个人开发者林光亮的Small框架

alibaba开源的Atlas框架

使用Small对项目进行改造

集成：

1.按照规则创建对应的project

2.在创建好的project build.gradle中集成编译插件gradle-Small

文件末尾引用gradle-small插件:apply plugin: 'net.wequick.small'

用gradlew small 验证集成是否正确>

3.在工程的宿主module中初始化Smal在自定义Application中 Small.preSetUp(this);

插件创建阶段

一、以指定的规范来创建插件

Module name 以 app.* 命名的模块将被 Small 在 编译时 识别为应用插件模块。

Package name 以 app* 结尾的插件将被 Small 在 运行时 识别为应用插件

二、编译创建好的插件

(1)先编译公共库 gradlew buildlib -q (宿主是一个最基础的公共库)

(2)再编译app.main插件 gradlew buildBundle -q Dbundle.arch=arm

(3)查看编译完成的具体情况  gradlew small

至此，我们已经完成插件编译并将之内置到宿主包中去了

三、通过宿主应用启动插件应用

(1)新建assets目录，在该文件夹下创建路由配置文件 bundle.json;

(2)修改bundle.json，添加路由：

{

 "version": "1.0.0",

 "bundles": [

  {

   "uri": "main",

   "pkg": "com.example.appmain"

  }

 ]

}

这里的：

version，是 bundle.json 文件格式版本，目前始终为 1.0.0

bundles，插件数组

uri，插件唯一ID

pkg，插件包名

通过这个配置，main 将被路由向 com.example.mysmall.appmain#MainActivity

(3)回到宿主的 app > java > com.example.mysmall > MainActivity，在 onStart 方法中我们通过上述配置的 uri 来启动 app.main 插件：

@Override

protected void onStart() {

  super.onStart();

  Small.setUp(this, new Small.OnCompleteListener() {

​    @Override

​    public void onComplete() {

​      Small.openUri("main", MainActivity.this);

​    }

  });

}

(4)运行宿主

需要掌握的small集成此基础：

通过 build.gradle 集成 Small

通过 自定义Application 初始化 Small

通过 buildLib，buildBundle 编译 Small 插件

通过 bundle.json 配置插件路由

通过 Small.openUri 启动插件

业务类插件/公共库插件创建

(1)创建公共库插件模块

Lib.style

Module name: lib.style

Package name: com.example.libstyle

(2)添加公共库引用

修改 app.main/build.gradle，增加对 lib.style 的依赖：

dependencies {

  ...

  compile project(':lib.style')

}

(3)添加插件路由

修改 app/assets/bundle.json：

{

 "pkg": "com.example.libstyle"

}

(4)重新编译插件

清除公共库：

./gradlew cleanLib -q

编译公共库：

./gradlew buildLib -q -Dbundle.arch=x86

编译业务单元：

./gradlew buildBundle -q -Dbundle.arch=x86

(5)重新运行程序

Small进阶教程

公共库插件模块

公共库插件模块在 开发时 可以通过 compile project(':插件模块名')来被 应用插件模块 所引用。

同时在 编译时 (buildLib) 会被打包成为一个可独立更新的插件。

定义公共库插件模块有两种方式：

指定 Module name 为 lib.*

在 Small DSL 中显式指明 bundles lib your_module_name

要正确读取到打包的公共库插件也有两种方式：

指定 Package name 为 **.lib.* 或 **.lib*

在 bundle.json 中添加描述 "type": "lib"

应用插件模块

应用插件模块在 开发时 可以独立运行。

同时在 编译时 (buildBundle 或 :模块:aR ) 会被打包成一个可独立更新的插件。

定义应用插件模块有两种方式：

指定 Module name 为 app.*

在 Small DSL 中显式指明 bundles app your_module_name

要正确读取到打包的公共库插件也有两种方式：

指定 Package name 为 **.app.* 或 **.app*

在 bundle.json 中添加描述 "type": "app"

自定义资源ID分段

在整合插件资源的过程，为避免资源ID冲突，需要为每个插件分配一个ID段。

我们知道默认程序的ID段为 0x7f。由于系统使用了 0x00，0x01，0x02。因此插件允许的范围在 [0x03, 0x7e] 之间。

但是有些手机厂商占用了一些分段，黑名单如下：

ID	厂商

0x03	HTC

0x10	小米

为此，Small 根据哈希算法将插件的模块名散列到 [0x11, 0x7e] 区间，作为自动分配的插件资源ID段，比如：

模块名	ID

app.main	0x77

lib.style	0x79

但是，这个算法生成的ID有可能是重复的，所以必要时你可以通过以下方法自定义插件的资源ID。

修改 build.gradle

在插件模块所在的 build.gradle 脚本里，增加配置：

ext {

  packageId = 0x12

}

这里的 0x12 是16进制整型值

将把当前插件的资源ID段分配为 0x12。

或修改 gradle.properties

在插件模块所在的 gradle.properties 配置里，增加配置：

packageId=3f

properties只接收字符串值

将把当前插件的资源ID段分配为 0x3f。

http://code.wequick.net/Small/cn/home Small开发文档

必知必会

Samll的基本用法：集成、插件化生成、宿主配置

将已有项目改造成插件化架构

Samll的进阶知识，做到对Samll有一个全面的了解

Atlas框架讲解

重量级框架 非常复杂

Atlas的基本概念和作用

了解Atlas的整体结构和原理

Alibaba独立开发并开源的一种插件化技术方案,也叫动态组件化(Dynamic Bundle)框架

目前手机淘宝和优酷使用的是这种技术方案

功能强大但是使用特别复杂，适用于门户型的App，它主要提供了解耦化、组件化、动态性的支持

核心原理与Samll是完全一样的

与插件化框架不同的是，Atlas是一个组件框架，Atlas不是一个多进程的框架，他主要完成的就是在运行环境中按需地去完成各个bundle的安装，加载类和资源

Atlas的整体结构和原理

https://alibaba.github.io/atlas/principle-intro/Runtime_principle.html

框架层次：

包含四个层次

1.最下面的一层：hack工具层，主要是进行hack工具类的初始化和校验工作

2.Bundle Framework 负责bundle的安装更新以及管理整个bundle的生命周期

3.runtime层：主要包括清单管理、版本管理、系统代理三大模块，基于不同的触发点

按需执行bundle的安装和加载。从delegate层可以看到，最核心的两个代理点：

一个是DelegateClasssLoader,负责路由由class加载到各个bundle内部，

一个是DelegateResource：负责资源查找时能够找到bundle内的资源，这是bundle能够真正运行起来的的根本点。

4.对外接入层：AtlasBridgeApplication是atlas框架下apk的真正Application，在基于Atlas框架构建的过程中会替换原有manifest中的application，AtlasBridgeApplication里面除了完成了Atlas的初始化功能，同时内置了multidex的功能，这样做的原因有两个：

很多大型的app不合理的初始化导致用multidex分包逻辑拆分的时候主dex的代码就有可能方法数超过65536，AtlasBridgeApplication与业务代码完全解耦，所以拆分上面只要保证atlas框架在主dex，其他代码无论怎么拆分都不会有问题；

如果不替换Application，那么atlas的初始化就会在application里面，由于基于Atlas的动态部署实际上是类替换的机制，那么这种机制就会必然存在包括Application及其import的class等部分代码在dalvik不支持部署的情况，这个在使用过程中造成一定成本，需要小心的使用以避免dalivk内部class resolve机制导致部分class没成功，替换以后该问题得到最好的解决，除atlas本身以外，所有业务代码均可以动态部署

Bundle的生命周期：

Installed bundle被安装到storage目录

Resolved classloader被创建，assetpatch注入DelegateResoucces

Active bundle的安全校验通过；bundle的dex检测已经成功dexopt(or dex2oat)，resource已经成功注入

Started bundle开始运行，bundle application的onCreate方法被调用

每一层都为上一层次提供服务

类加载机制

Atlas里面通常会创建了两种classLoader,第一个是DelegateClassLoader，他作为类查找的一个路由器而存在，本身并不负责真正类的加载；DelegateClassLoader启动时被atlas注入LoadedApk中，替换原有的PathClassLoader；第二个是BundleClassLoader，参考OSGI的实现，每个bundle resolve时会分配一个BundleClassLoader，负责该bundle的类加载。关系如下图所示： DelegateClassLoader以PathClassLoader为parent，同时在路由过程中能够找到所有bundle的classloader；

BundleClassLoader以BootClassLoader为parent，同时引用PathClassLoader,BundleClassLoader自身findClass的顺序为

\1. findOwn： 查找bundle dex 自身内部的class

\2. findDependency: 查找bundle依赖的bundle内的class

\3. findPath： 查找主apk中的class

Bundle的Activity启动的类加载过程来帮助理解：

ActivityThread从LoadedApk中获取classloader去load Activity Class；

根据上面的classloader关系，先去parent里面加载class；

由于class在bundle里面，所以pathclassloader内查找失败，接着delegateclassloader根据bundleinfo信息查找到classloader在bundle中（假设为bundleA）；

从bundleA中加载class，并且创建class；

后面在Activity起来后，如果bundleA对bundleB有依赖关系，那么如果用到了bundleB的class，又会根据bundlA的bundleClassloader的dependency去获取bundleB的classloader去加载； 

资源加载机制：

类似ClassLoader，LoadedApk中的Resources被替换成Atlas内部的DelegateResources,同时在每个Bundle安装的过程中，每个bundle的assetspath会被更新到DelegateResources的AssetsManager中；每个bundle的资源特征如图可知：

bundle构建过程中，每个bundle会被独立进行分区，packageId保证全局唯一，packageID在host的构建工程内会有个packageIdFile.properties进行统一分配；

虽然每个bundle的manifest都声明了自己的packagename，但是在aapt过程中，arsc文件里面所有bundle的packagename均被统一为hostApk的package，比如在手淘内就都是com.taobao.taobao；这样改的目的是为了解决在资源查找中一些兼容性问题；

名词解释：

Bundle

awb

host

remote bundle

动态部署

Bundle: 类似OSGI规范里面bundle（组件）的概念，每个bundle有自己的classloader，与其他bundle相隔离，同时Atlas框架下bundle有自身的资源段（PackageID，打包时AAPT指定）；另外与原有OSGI所定义的service格式不同之处是Atlas里面Bundle透出所有定义在Manifest里面的component，随着service，activity的触发执行bundle的安装，运行。

awb： android wireless bundle的缩写，实际上同AAR类似，是最终构建整包前的中间产物。每个awb最终会打成一个bundle。awb与aar的唯一不同之处是awb与之对应有个packageId的定义。

host: 宿主的概念，所有的bundle可以直接调用host内的代码和资源，所以host常常集合了公共的中间件，UI资源等

基于Atlas构建后大致工程的结构：

首先有个构建整体APK工程Apk_builder,里面管理着所有的依赖（包括atlas）及其版本，Apk_builder本身可能不包含任何代码，只负责构建使用

host内部包含独立的中间件，以及一个Base的工程，里面可能包含应用的Application，应用icon等基础性内容（如果足够独立，application也可以直接放在apk_builder内）；

业务层基本上以bundle为边界自上而下与host发生调用，同时bundle之间允许存在依赖关系；相对业务独立的bundle如果存在接口耦合建议封装成aidl service的方式保证自身封装性；同时某些中间件如果只存在若干bundle使用的也可以封装bundle的方式提供出来，以保证host内容精简

remote bundle： 远程bundle，远程bundle只是apk构建时并未打到apk内部，而是单独放在了云端；同时远程bundle的限制条件是第一次被触发的前提是bundle内的Activity需要被start，此时基于Atlas内的ClassNotFoundInterceptorCallback可以进行跳转的重定向，提示用户下载具体bundle，待用户确定后进行异步下载同时完成后再跳转到目标bundle（此部分代码由于涉及下载及UI展示等内容并未包含在开源仓库中，有需要可以根据ClassNotFoundInterceptorCallback自行实现）

动态部署： 基于Atlas的installorUpdate和atlas-update库及构建插件，可以生成与之前发布的apk diff生成的差异文件，在更新时拉取同时静默更新到设备上，在用户下次启动之后生效新代码，具体原理可以参考动态部署章节的解析

APK结构：

基于Atlas构建后的APK结构如下图，host与普通Apk无异，但是Manifest和assets会添加一些额外的内容，同时在armeabi目录中会增加若干个bundle构建的产物，取名为String.format(lib%s.so,packagename.replace(".","_"))；packagename为bundle的AndroidManifest中的packagename,这些so都是正常的apk结构，改为so放入lib目录只是为了安装时借用系统的能力从apk中解压出来，方便后续安装

assets/bundleinfo-version.json

构建完的apk在host的assets目录下，会有个bundleinfo-verison.json的文件，其中version为manifest中的versinonname，里面记录了每个bundle大小，版本，名字以及里面所有的component信息，这些内容在构建的时候生成，基于这些信息每个bundle可以在component被触发的时候去按需的进行安装，整个过程对开发者透明（从中也可以看到默认情况下bundle对外暴露的只是基于Android原生的Activity，service，receiver等component）

AndroidManifest

运行期文件结构

/data/data/pkgname/files/bundlelisting

之前打包构建时记录的bundleinfo信息（发生动态部署收文件会进行更新）

/data/data/pkgname/files/baselineinfo

存放动态部署后的版本变化内容，以及每次部署发生更新的bundle的版本，依赖等信息

/data/data/pkgname/files/storage

storage目录是bundle安装的目录，每个bundle的安装目录以bundle的packagename为文件夹名，首次启动后会安装到version.1目录下，目录中可能含有bundle的zip文件，dex文件以及native so等内容。如果bundle发生更新，则可能会有version.2、version.3 等目录，每次加载bundle的时候选取最高可用版本进行载入。考虑bundle的回滚功能和对空间占用的影响，目前容器内最多保留两个最近可用版本。



作者：hanfengzqh
链接：https://www.jianshu.com/p/75fdcc7675e9
来源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。