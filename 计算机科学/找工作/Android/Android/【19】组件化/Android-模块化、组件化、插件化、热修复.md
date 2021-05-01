应网友需求，做个目录链接总结，依次进阶！

[MonkeyLei：Android-模块化、组件化、插件化、热修复-国庆前瞄一眼概念啥的，好伐!](https://zhuanlan.zhihu.com/p/84694057)

[MonkeyLei：Android-模块化、组件化、插件化、热修复-组件化一下试试](https://zhuanlan.zhihu.com/p/84713874)

[MonkeyLei：Android-模块化、组件化、插件化、热修复-组件化工程构建+页面路由多种方式实践](https://zhuanlan.zhihu.com/p/85095730)

[MonkeyLei：Android-模块化、组件化、插件化、热修复-组件化-组件间的通信(本地，下沉，bus，路由)](https://zhuanlan.zhihu.com/p/85593596)

[MonkeyLei：Android-模块化、组件化、插件化、热修复-组件化-ButterKnife引入(解决一些问题)](https://zhuanlan.zhihu.com/p/85628591)

[MonkeyLei：Android-模块化、组件化、插件化、热修复-插件化-起步模仿一个案例（有猫腻呀）](https://zhuanlan.zhihu.com/p/86165557)

[MonkeyLei：Android/Java-插件化-相关预了解、入门、实践推荐链接(一定优先级及遇到问题解决链接参考)](https://zhuanlan.zhihu.com/p/86261306)

[MonkeyLei：Java-Hook技术-入门实践+反射、动态代理、热修复再看看](https://zhuanlan.zhihu.com/p/86534217)

[MonkeyLei：Java-Hook技术-入门实践(反射、动态代理)-Hook拦截通知(当前App/Context)](https://zhuanlan.zhihu.com/p/87953073)

[MonkeyLei：Android-热修复技术框架如何选择？记录一波...](https://zhuanlan.zhihu.com/p/88097003)

[MonkeyLei：Android-美团Robust热修复接入实践问记录](https://zhuanlan.zhihu.com/p/88900878)

[MonkeyLei：Android-Tinker热修复接入实践问题记录(自己认为的较完善的理解)](https://zhuanlan.zhihu.com/p/89050372)

路漫漫其修远兮，目前只是接触学习....多源码多算法还得抽空努力！

---

   **1、模块化**

   Android Studio提出的概念，module模块，包含两种格式application和library。概念是一个module是一个小项目，相对于包来说模块更灵活，耦合更低，随意插拨，根据不同关注点将项目共享部分或业务模块抽取出来形成独立module。

   **2、组件化**

   基于模块化，核心思想是角色的转换，在打包时是library，开始调试是application。单位是组件（module），目的是解耦与加快开发，隔离不需要关注的部分。分离独立的业务组件如微信朋友圈。

   **3、插件化**

   也是属于模块化的一种体现。将完整的项目按业务划分不同的插件，分治法，越小的模块越容易维护。单位是apk，一个完整的项目。作用与组件化不同于热更新，灵活性在于加载apk，按需下载，动态更新。

   插件化实现原理简单概括三步：

   通过dexclassloader加载。

   代理模式添加生命周期。

   hook思想跳过清单验证。

   **4、热修复**

   热修复与插件化都利用classloader实现加载新功能。不同的是热修复是为了修复bug，所以要将新的同名类替代同名的bug类，要抢先加载bug类之前加载新的类。插件化只是增加新的功能或资源文件，所以不涉及抢先加载旧类的使命。插件化比热修复简单，热修复在插件化基础上实现替换旧类bug。

   **5、模块化、组件化、插件化通讯方式不同**

   模块化相互引入，抽取了common，其他模块自然要引入这个module。

   组件化主流是隐式和路由。隐式使解耦和灵活大大降低，因此路由是主流。

   插件化本身是不同进程，因此是binder机制进程间通讯。

---

正好前段时间过了哈Luncher启动流程，试了哈Service。。完事了，还有两天就放假了，就暂时不去跟Service的源码流程了。。正好抓紧预览下。也看了哈网友的总结，有几点疑问和难点：

\1. 模块化、组件化的相似和区别

\2. 插件化和热修复的难点 - 这两个相对难些，尤其是涉及到热修复的知识，框架，知识就多了。。。各家方案又不同。这个怕是小萌新后面搞完之前的知识，得抓紧学习起来。逆水行舟，不进则退...

看了一些网友的回答，有偏差。有些网友认为**模块化和组件化**类似，然后就抛出了一些概念：

这里抛出一些比较**可以的概念**：

```text
模块化
Android Studio出来了，多出来了一个新的概念， Project, Module...  模块；当时以包的形式分离的公共包common,现在成了AS中的Module。大家都知道，Module包含两种格式: application， library。也就是说，一个Module就是一个小的项目，也是AS概念中的模块。因此我们开始设计common模块， common_business模块，甚至db模块。模块的好处是什么？ 相比于包来讲，模块更灵活，耦合更低，随意插拔，想引入哪个就引入哪个。根据不同的关注点，将一个项目的可以共享的部分抽取出来，形成独立的Module，就是模块化。模块化不只包含公共部分，当然也可以是业务模块。

组件化
平时看看论坛，好多人都在问： 模块化和组件化有什么区别？ 到底有什么区别呢，其实很小；但并不是完全相同的概念。 通过以上模块化的概念讲述，应该对模块化有了一个了解，那么区别是什么呢？
组件化是建立在模块化思想上的一次演进，一个变种。组件化本来就是模块化的概念。但是组件化的核心是
什么？ 是模块角色的可转换性。是的，就是可转换性。

组件化的核心是角色的转换。 在打包时， 是library; 在调试时， 是application。

怎么理解组件化的概念 ？
Module的模式分两种， application和library。 library就是引用库，如你抽取的common。 application就是一个apk， 是一个完整的项目。
```

**理解：**

比如一个大型的某宝App项目，假使有几十上百个人开发，有专门开发启动页广告页面、登录、注册的，有专门开发首页的，有开发个人中心的，有开发购物车的。此时我们希望我们的Module可以单独运行，同时当我提交到git分支合并工程后，又需要成为一个library被主模块App所引用。此时组件化充当的角色其实就是将App的某个功能模块抽离成Module。另一方面，如果我们自己封装了弹窗库Module，我们的弹窗可以被任何模块使用，我们弹窗是在另外一个工程里面测试完成，此时正是将弹窗的Library给到了这个工程使用。此时Library并不能自己运行，同时弹窗提供的功能也就是公共弹窗的角色，不具备可转换性。

而**组件化则侧重业务，侧重角色！是模块化的一种演进。另外一点我觉得很重要，就是组件之前的通信！(**比如我们写了购物车模块，你写了登录模块，那么你登录成功后有可能就需要通知我加载用户购物车信息.)。

组件化的过程中，我们务必是要把**Base模块**抽取处理的，比如Base库(BaseActivity、BaseFragment等基础页面）、比如Util库，这些则是作为了模块被其他模块/组件所引用。

至于遇到的一些问题比如：

```text
问题1：多业务模块下的统一配置
问题2：Application分发
问题3：资源的冲突
注意：不同的业务模块禁止彼此依赖
```

不管是模块化还是组件化，每个部分都称之为模块，此时模块被其他模块引用，如果有冲突的资源命名，建议模块资源全部以_moduelname作为前缀，这样才不会与其他模块冲突，否则容易导致引入错误，影响界面效果等问题。小萌新之前的相关就是有很多颜色资源id相同，同时有些布局id还相同了，那样很不好，后面要继续基于重构版本优化结构了.

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxhyvvjbdj30k306wmy0.jpg)

小萌新目前正在基于上面的知识进行组件化重构（由于开发人员较少，所以不打算太过组件化，之前重构的版本主要是将功能抽取模块化了(不过这个所谓的模块化目前还是在App模块下)，接下来就是组件化一些模块，方便测试扩展自己封装的公共库）。但是该了解的知识还是需要自己去了解才行妮...

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxhyup3l6j30lk0fm0v6.jpg)











至于**插件化**：

```text
 插件化开发就是将整个app拆分成很多模块，每个模块都是一个apk(组件化的每个模块是一个lib),最终打包的时候将宿主apk和插件apk分开打包，插件apk通过动态下发到宿主apk，这就是插件化
```



```text
插件化的好处：
宿主和插件分开编译
是可以并发开发的。宿主和插件说白了就是apk，开发是互不影响的（只需要宿主给插件一个上下文）。
动态更新插件，不需要安装，下载之后就接就可以打开，
按需下载模块
可以解决方法树的爆棚问题65535
```



到了**热修复**：小萌新觉得步骤更容易帮助理解了妮

```text
修复有bug的类，生成dex补丁包；
通过反射机制得到PathClassLoader的成员变量PathList字段（DexPathList的属性）(通过上面分析知道，PathList是PathClassLoader父类BaseDexClasLoader中的)
然后再反射PathList获取它的dexElements字段(是一个存放dex的Element数组)
将我们生成的dex补丁包，插入到dexElements的数组的最前端
```

大概看了下。模块化，组件化其实还好。 至于插件化和热修复，咋一看很陌生，很难。。小萌新也没搞过，也没入门过（不对，插件化之前入门过）。热修复确实还没入门过。而且有很多关于别人家的热修复方案，我想那才是真正比较难的地方，你才是我们这些基础程序员要看齐的地方。小萌新的计划是，先走一遍流程，然后针对重构的工程先进行部分组件化完善，然后利用插件化实现App的主题动态更新，至于热修复，后面除了入门（主要是了解一些预备知识），还得去好好看看别人家的热修复方案（争取多了解一些更深人预备知识，为了深入做准备）。

```text
目前主流的热修复技术框架
阿里系的： Andfix、Hotfix、Sophix
腾讯系的：QQ空间超级补丁技术、Qfix、Tinker（微信）
美团系的：Robust
饿了么的：Amigo
```

这里做一些记录，提醒自己后面还有很多事情要做。提醒自己周末不要浪费时间了，哎.......努力努力应该是有机会的！

接下来小萌新就真刀真枪戳一下了，o(*￣︶￣*)o

附录：

[https://blog.csdn.net/csdn_aiyang/article/details/82152538#三、插件化](https://link.zhihu.com/?target=https%3A//blog.csdn.net/csdn_aiyang/article/details/82152538%23%E4%B8%89%E3%80%81%E6%8F%92%E4%BB%B6%E5%8C%96)

[模块化，组件化傻傻分不清？附带组件化福利](https://link.zhihu.com/?target=https%3A//www.jianshu.com/p/f5212cf7df55)

[Android 热修复介绍之代码修复](https://link.zhihu.com/?target=https%3A//www.jianshu.com/p/1c2e1c1931af)

---

### 1 模块化

狭义上说：
是指Android studio支持了多个module开发时，提出的模块化概念。
具体实践：把常用的功能、控件、基础类、第三方库、权限等公共部分抽离封装，把业务拆分成N个模块进行独立(module)的管理。
而所有的业务组件都依赖于封装的基础库，业务组件之间不做依赖，这样的目的是为了让每个业务模块能单独运行。

广义上说：
将一个复杂业务实现，根据功能、页面或者其他进行不同粒度的划分程不同的模块，模块之间解耦，分别进行实现，也就是编程的模块化思想。

模块化的特点是：模块之间解耦，可以独立管理。

### 2 组件化

将一个app的代码拆分成几份独立的组件，组件之间是低耦合的，可以独立编译打包；也可以将组件打包到一个apk中。

**【没有组件化的缺点】：**

- （1）项目可维护性下降。随着项目的增加，即使有做分包目录，但是项目会逐渐失去层次感，可读性、可维护性下降
- （2）开发和调试效率低。开发和调试时，修改了一个小功能，但是需要重新build整个项目才能看到结果，耗费时间
- （3）易阻断不同业务模块的并发开发。一个业务模块的小bug，可能阻断其他业务模块的开发和调试，不同业务模块的并发开发会被阻断。
- （4）版本管理困难。多人联合开发时，在版本管理中容易出现冲突和代码覆盖问题。

**【组件化的拆分】**

- 按照业务（功能）划分各个业务组件模块。

**【组件化后的优点】**

- （1）每个模块可独立编译，提高了编译速度；
- （2）开发只负责自己的模块，还可以再做的隔离一些，每个业务线只可见自己业务模块的代码，避免了误修改和版本管理问题。
- （3）公共的Lib依然是个独立模块

**与模块化的区别：是每个模块的角色的转换，一个组件可以独立编译打包，也可以作为lib集成到整个apk中**

 

**阿里的 Arouter 开源框架**

### 3 插件化

插件化是将一个apk根据业务功能拆分成不同的子apk（也就是不同的插件），每个子apk可以独立编译打包，最终发布上线的是集成后的apk。在apk使用时，每个插件是动态加载的，插件也可以进行热修复和热更新。

- 从技术上讲，就是解决如何启动未安装的apk里面的类（主要是四大组件）。
- 主要问题：如何加载类、如何加载资源、如何管理组件生命周期。

**与组件化的主要区别：**

- 形式上的区别，组件化的单位是module，插件化的单位是apk
- 关注点不同，插件化更关注动态加载、热更新、热修复等‘插拔’技术。

**组件化与插件化详细对比**

| 技术   | 单位   | 实现内容                                     | 灵活性                                              | 特性                                                         | 静动态                                         |
| ------ | ------ | -------------------------------------------- | --------------------------------------------------- | ------------------------------------------------------------ | ---------------------------------------------- |
| 组件化 | module | 是解耦与加快编译，隔离不需要关注的部分       | 按加载时机切换，是作为lib，还是apk                  | 组：组本来就是一个系统，每个组件不是真正意义上的独立模块     | 静态加载                                       |
| 插件化 | apk    | 是解耦与加快编译，同时实现热插拔也就是热更新 | 加载的是apk，可以动态下载，动态更新，比组件化更灵活 | 插：是独立的apk，每个插件可以作为一个完全独立的apk运行，也可以和其他插件集成为大apk | 动态加载，只用真正使用某个插件时，才加载该插件 |

### 4 热修复

首先需要明确的一点，**插件化和热修复**不是同一个概念，虽然站在技术实现的角度来说，他们都是从系统加载器的角度出发，无论是采用hook方式，亦或是代理方式或者是其他底层实现，都是通过“**欺骗**”Android 系统的方式来让**宿主**正常的加载和运行**插件（补丁）**中的内容；但是二者的出发点是不同的。**插件化**顾名思义，更多是想把需要实现的模块或功能当做一个独立的提取出来，减少**宿主**的规模，当需要使用到相应的功能时再去加载相应的模块。**热修复**则往往是从修复bug的角度出发，强调的是在不需要二次安装应用的前提下修复已知的bug。

**类加载原理：**

说起热修复就不得不提类的加载机制，和常规的JVM类似，在Android中类的加载也是通过ClassLoader来完成，具体来说就是PathClassLoader 和 DexClassLoader 这两个Android专用的类加载器，这两个类的区别如下：

- PathClassLoader：只能加载已经安装到Android系统中的apk文件（/data/app目录），是Android默认使用的类加载器。
- DexClassLoader：可以加载任意目录下的dex/jar/apk/zip文件，也就是我们一开始提到的补丁。

---

# 前言 

> 谈到热修复相信大家应该比较熟悉，因为它是目前比较重要的技术，平常面试中也是被问的比较多。插件化和热修复同出一门，俩者都属于动态更新，而模块化和组件化是基础。相信看完本篇的内容，对于这些模糊的概念应该会有一个比较清晰的了解。

 

## **一、模块化**

**1、概念**

> 模块(Module)，Android Studio提出的概念，根据不同关注点将原项目中共享的部分或业务抽取出来形成独立module，这就类似我们最集成的第三方库的SDK。

**2、思想**

> 实际开发中，我们通常会抽取第三方库、整个项目的初始化的代码、自定义的Utils工具类、自定义View 、图片、xml这些（value目录下的各种xml文件）等到一个共有的Common模块中，其他模块在配置Gradle依赖后，就能够调用这些API。
>
> 特别注意的是**style.xm**l文件，对于全局共用的style，我们应该把它也放在common模块中。例如我们的项目theme主题，本来是放在main组件的style里面，我们可以把它移到common中，这样其他组件调试时，作为一个单独的项目，也能和主项目有一样的主题。
>
>  总之，你认为需要共享的资源，都应该放在common组件中。

**3、使用**

> 每一个Module都可以在自身的 build.gradle 中进行设置两种格式：application和library。 

```java
apply plugin: 'com.android.application'
//或
apply plugin: 'com.android.library'
```

> 引用时，就像添加依赖GitHub库一样。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxi1drvb1j30h10710sx.jpg)

 

## **二、组件化**

### **1、概念**

> 组件化是基于模块化的**，**可以在打包时是设置为library，开始调试运行是设置成application。目的是解耦与加快开发。组件化适用于多人合作开发的场景，隔离不需要关注的模块，大家各自分工、各守其职。简而言之，就是把一个项目分开成多个项目

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxi1bjmifj30lc0c0aab.jpg)

（1）好处

- 业务模块分开，解耦的同时也降低了项目的复杂度。
- 开发调试时不需要对整个项目进行编译。
- 多人合作时可以只关注自己的业务模块，把某一业务当成单一项目来开发。
- 可以灵活的对业务模块进行组装和拆分。

（2）规则

- 只有上层的组件才能依赖下层组件，不能反向依赖，否则可能会出现循环依赖的情况；
- 同一层之间的组件不能相互依赖，这也是为了组件之间的彻底解耦；

### **2、使用**

1、在整个项目 gradle.properties 文件中，添加代码

```java
#是否处于debug状态
isDebug = flase
```

 

2、在其他Module的 build.gradle 文件中，添加代码

```java
if (isDebug.toBoolean()) {
    apply plugin: 'com.android.application'
} else {
    apply plugin: 'com.android.library'
}
```

3、在宿主Module的 build.gradle 文件中，添加代码

```java
dependencies {
    compile fileTree(include: ['*.jar'], dir: 'libs')
    //...
    if(!isDebug.toBoolean()){//不是debug，就添加依赖其他模块
        compile project(':home')
        compile project(':personal')
        compile project(':video')
    }
    if(isDebug.toBoolean()){
        compile project(':common')
    }
}
```

### **3、版本管理**

> 每个Module的build.gradle文件中很多地方需要些写版本号，例如 targetSdkVersion、appcompat-v7、第三方库等。修改时都要同时修改多份build.gradle文件。如果把版本号可以统一管理起来，就会省时省力，又避免不同的组件使用的版本不一样，导致合并在一起时引起冲突。

整个项目根目录下的 build.gradle 文件中，添加代码

```java
ext {
    compileSdkVersion = 25
    buildToolsVersion = "25.0.2"
    minSdkVersion = 14
    targetSdkVersion = 25
    versionCode = 1
    versionName = "1.0"
}
```

每个Mudule的 build.Gradle 文件中，改写代码

```java
android {
    compileSdkVersion rootProject.ext.compileSdkVersion
    buildToolsVersion rootProject.ext.buildToolsVersion
    defaultConfig {
        minSdkVersion rootProject.ext.minSdkVersion
        targetSdkVersion rootProject.ext.targetSdkVersion
        versionCode rootProject.ext.versionCode
        versionName rootProject.ext.versionName
    }
//...
}
```

### **4、模块间跳转**

> 我们知道，通常在Gradle中依赖的库是可以直接引用的，即通过startActivity跳转。根据组件化的规则，宿主可以依赖下层组件，而组件之间不可以依赖。因此，当常规业务模块之间遇到业务需求，进行互相跳转时该怎么处理？
>
> 这里简单介绍两种方式，即路由和反射。路由的方式以用阿里的[ARouter](https://github.com/alibaba/ARouter)/美团的[WMRouter](https://tech.meituan.com/2018/08/23/meituan-waimai-android-open-source-routing-framework.html)，但是我觉得人少、项目小的公司必要用到这么强大的工具，直接反射就好。

放在common组件中的EventUtile工具类

```java
public class EventUtil{
    /**
     * 页面跳转
     * className  全路径类名
     */
    public static void open(Context context,String className){
        try {
            Class clazz = Class.forName(className);
            Intent intent = new Intent(context,clazz);
           context.startActivity(intent);
        } catch (ClassNotFoundException e) {
            Log.e("zhuang","未集成，无法跳转");
        }
    }
    /**
     * 页面跳转，可以传参，参数放在intent中，所以需要传入一个intent
     */
    public static void open(Context context,String className,Intentintent){
        try {
            Class clazz = Class.forName(className);
           intent.setClass(context,clazz);
           context.startActivity(intent);
        } catch (ClassNotFoundException e) {
            Log.e("zhuang","未集成，无法跳转");
        }
    }
}
```

### **5、资源命名问题**

> 首先，多组件集成时，特别容易出现资源命名重复的问题。可以让各个组件中使用统一前缀，比如home组件中的资源，以home_开通、video组件中以video_开头。当然，如果是嫌麻烦，我们可以在build.gradle文件中，加入如下代码：

```java
resourcePrefix"home_"
```

> 但是这个功能其实很弱。比较xml文件报错，依然可以运行，图片文件不已home_为前缀，也不会报错。

## 三、插件化

> 也是属于模块化的一种体现。将完整的项目按业务划分不同的插件，分治法，越小的模块越容易维护。单位是apk，一个完整的项目。插件化比热修复简单，插件化只是增加新的功能或资源文件。灵活性在于加载apk，按需下载，动态更新。

**实现原理**

1. 通过dexclassloader加载。
2. 代理模式添加生命周期。
3. hook思想跳过清单验证。

[Android 使用Java的反射机制总结](https://blog.csdn.net/csdn_aiyang/article/details/79045808)

[Android 动态代理与*Hook*机制详解](https://blog.csdn.net/csdn_aiyang/article/details/79085039)

**总结**

- **宿主和插件分开编译**
- **动态更新插件**
- **按需下载插件**
- **缓解65535方法数限制**

## **四、热修复**

### 1、概述

> 热修复与插件化都利用classloader实现加载新功能。热修复比插件化复杂，插件化只是增加新的功能或资源文件，所以不涉及抢先加载旧类的使命。热修复为了修复bug，要将新的同名类替旧的同名bug类，要抢在加载bug类之前加载新的类。

### 2、流派

>   热修复作为当下热门的技术，在业界内比较著名的有阿里巴巴的AndFix、Dexposed，腾讯QQ空间的超级补丁和微信的Tinker，以及大众点评nuwa和美团Robust。阿里百川推出的HotFix热修复服务就基于AndFix技术，定位于线上紧急BUG的即时修复。虽然Tinker支持修复的功能强大兼容性很好，但是不能即时生效、集成负责、补丁包大。

### 3、原理

**（1）native修复方案**

AndFix 

> 提供了一种运行时在Native修改Filed指针的方式，实现方法的替换，达到即时生效无需重启，对应用无性能消耗的目的。实现过程三步骤：

- setup()：对于Dalvik的即时编译机制（JIT），在运行时装载libdvm.so动态链接库，从而获取native层内部函数：dvmThreadSelf( )：查询当前的线程；dvmDecodeIndirectRef( )：根据当前线程获得ClassObject对象。
- setFieldFlag()：把 private、protected的方法和字段都改为public，这样才可被动态库看见并识别，因为动态库会忽略非public属性的字段和方法。
- replaceMethod()：该步骤是方法替换的核心。拿到新旧方法的指针，将指针指向新的替换方法来实现方法替换。

**（2）Dex 分包方案**

概述

> DEX分包是为了解决65536方法限制，系统在应用打包APK阶段，会将有调用关系的类打包在同一个Dex文件中，**并且同一个dex中的类会被打上`CLASS_ISPREVERIFIED`的标志**。因为加载后的类不能卸载，必须通过重启后虚拟机进行加载才能实现修复，所以此方案不支持即时生效。

 QQ空间超级补丁

> 是把BUG方法修复以后放到一个patch.dex，拿到当前应用BaseDexClassloader后，通过反射获取到DexPathList属性对象pathList、再反射调用pathList的dexElements方法把patch.dex转化为Element[]，两个Element[]进行合并，最后把patch.dex插入到dexElements数组的最前面，让虚拟机去加载修复完后的方法，就可以达到修复目的。

问题 

> 而然，问题就是两个有调用关系的类不再同一个Dex文件中，那么就会抛“unexpected DEX problem”异常报错。解决办法，就是单独放一个AnitLazyLoad类在另外DEX中，在每一个类的构造方法中引用其他DEX中的唯一AnitLazyLoad类，避免类被打上CLASS_ISPREVERIFIED标志。

不足 

> 此方案通过增加dex来修复，但是修复的类到了一定数量，就需要花不少的时间加载。对手淘这种航母级应用来说，启动耗时增加2s以上是不能够接受的事。在ART模式下，如果类修改了结构，就会出现内存错乱的问题。为了解决这个问题，就必须把所有相关的调用类、父类子类等等全部加载到patch.dex中，导致补丁包异常的大，进一步增加应用启动加载的时候，耗时更加严重。

微信Tinker

> 微信Tinker采用的是DEX差量包，整体替换DEX的方案。主要的原理是与QQ空间超级补丁技术基本相同，但不将patch.dex增加到elements数组中。差量的方式拿到patch.dex，开启新进程的服务TinkerPatchService，将patch.dex与应用中的classes.dex合并，得到一个新的fix_classess.dex。通过反射操作得到PathClassLoader的DexPatchList，再反射调用patchlist的makeDexElements()方法，把fix_classess.dex直接替换到Element[]数组中去，达到修复的目的。从而提高了兼容性和稳定性。

![img](https://tva1.sinaimg.cn/large/008eGmZEly1gmxi17xuejj31hc0u0tb6.jpg)

**（3）Instand Run 方案**

> Instant Run，是android studio2.0新增的一个运行机制，用来减少对当前应用的构建和部署的时间。

构建项目的流程：

> 构建修改的部分 → 部署修改的dex或资源 → 热部署，温部署，冷部署。
>
> 热拔插：方法实现的修改，或者变量值修改，不需要重启应用，不需要重建当前activity。
>
> 温拔插：代码修改涉及到了资源文件，activity需要被重启。
>
> 冷拔插：修改了继承规则、修改了方法签名，app需要被重启，但是仍然不需要重新安装 。

## **五、总结**

**模块化、组件化、插件化通讯方式不同之处**

1. 模块化相互引入，抽取了公共的common模块，其他模块自然要引入这个module。
2. 组件化主流是隐式和路由。隐式使解耦和灵活大大降低，因此路由是主流。
3. 插件化本身是不同进程，因此是binder机制进程间通讯。