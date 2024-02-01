[TOC]

### 什么是smali

Smali，Baksmali分别是指安卓系统里的Java虚拟机（Dalvik）所使用的一种.dex格式文件的汇编器，反汇编器。其语法是一种宽松式的Jasmin/dedexer语法，而且它实现了.dex格式所有功能，当我们对apk反编译之后，便会生成此类文件，其中在dalvik字节码中，寄存器都是32位的，可以支持任何类型，64位类型要用2个寄存器表示，其中Dalvik字节码有两种类型：`原始类型`和`引用类型(包括对象和数组)

### 为什么要学会smali

当我们反编译之后得到jar或者smali文件，android采用java语言开发，但是android系统使用自己的dalvik虚拟机，代码编译最终不是采用java的class，而是使用smali。因此我们反编译得到的代码，jar的话很多地方无法正确的解释出来，如果我们反编译的是smali则可以正确的理解程序的意思，因此学会smali十分有必要

### smali语法

> 类型：
> 首先介绍下 Dalvik 字节码是什么

- Dalvik是google专门为android 操作系统设计的一个虚拟机，经过深度优化。虽然android上的程序是使用java来开发的，但是dalvik和标准java虚拟机是两回事。Dalvik虚拟机是基于寄存器的，而JVM是基于栈的。Dalvik有专属的文件执行格式dex（dalvik executable），而JVM则执行的是java字节码，Dalvik vm比jvm速度更快，占用空间更少。
- 通过Dalvik的字节码我们不能直接看到原来的逻辑代码，这时需要借助ApkTool或者dex2jar+jd-jui工具来查看，但是，注意的是最终我们修改apk需要操作的文件是.smali文件，而不是导出来的java文件重新编译
  Dalvik字节码有两种类型：`原始类型`和`引用类型(包括对象和数组)`
- 原始类型

| 类型     | 描述                                     |      |
| :------- | :--------------------------------------- | ---- |
| V        | void - can only be used for return types |      |
| Z        | boolean                                  |      |
| B        | byte                                     |      |
| S        | short                                    |      |
| C        | char                                     |      |
| I        | int                                      |      |
| J        | long(64bits)                             |      |
| F        | float                                    |      |
| D        | double(64bits)                           |      |
| [XXX     | array                                    |      |
| Lxxx/yyy | object                                   |      |

- 对象类型

> Lpackage/name/ObjectName; 相当于java中的package.name.ObjectName;

L：表示这是一个对象类型

package/name：对象所在的包

ObjectName：对象名称

；： 标识对象结束

其中内部类 LpakeageName/objectName$subObjectname

- 数据类型

[i 表示一个int类型的一维数组，相当于int[]

增加一个维度增加一个[，如[[i,表示int[][]

其中：每一维最多255个

对象数组表示也是类似，如String数组的表示是[Ljava/lang/String

- 方法表示

> Lpackage/name/ObjectName;->MethodName(III)Z

Lpackage/name/ObjectName 表示类型

methodName 表示方法名

III 表示参数（这里表示为3个整型参数）

说明：方法的参数是一个接一个的，中间没有隔开

方法的定义一般为：

Func-Name(Para-Type1Para-Type2Para-Type3……)Return-Type

其中参数之间没有分隔符，例如：

hello()V 对应 void hello()

hello(III)Z 对应 boolean hello(int,int,int)

hello(Z[I[ILjava/lang/String;j) Ljava/lang/String 对应 String hello（boolean，int[],int[],String,long）

- 字段表示

> Lpackage/name/ObjectName;->FieldName:Ljava/lang/String;

说明： 包名，字段名和各字段类型

- 寄存器与变量

> 什么是寄存器：

在smali里所有的才做都必须经过寄存器来进行：本地寄存器用v开头以数字结尾的符号来表示，如v0，v1

参数寄存器则使用p开头数字结尾的符号来表示，如p0，p1，特别要注意的是，p0不一定是函数中的第一个参数，在非static函数中，p0代指this，p1表示函数的第一个参数，p2表示函数的第二个参数，而在static函数中p0才对应第一个参数(因为java的static方法中没有this方法)

> 简单指令解析

set-object v0，Lcom/aaa;->ID:Ljava/lang/String

set-object 就是用来获取变量值保存到紧接着的参数寄存器中，上面的表示它获取ID这个String类型的成员变量并放到v0这个寄存器中

iget-object v0，p0，Lcom/aaa;->view:Lcom/aaa/view;

这里可以看到iget-object指令比sget-object多了一个参数，该变量就是所在类的实例，在这里就是p0即，this

put指令的使用和get指令是统一的如下：

const/4 v3,0x0

sput-object v3,Lcom/aaa;->timer:Lcom/aaa/timer;

相当于：this.timer = null;

.loacl v0,args:Landroid/os/Message;

const/4 v1,0x12

input v1,v0,Landroid/os/Message;->what:l

相当于：args.what = 18;

指定一个方法中有多少寄存器可以使用有两种方式

> .registers 指令指定了方法中寄存器的总数
>
> .locals 指令表明了方法中非参寄存器的总数，出现在方法中的第一行

说明：

寄存器采用v和p来命名

v表示本地寄存器，p表示参数寄存器，二者关系如下：

Long和Double类型是64位，需要2个寄存器

如果有一个方法有两个本地变量，有3个参数，则表示如下：

```java
v0 第一个本地寄存器
v1 第二个本地寄存器
v2 p0 (this)
v3 p1 第一个参数
v4 p2 第二个参数
v5 p3 第三个参数
```

- 基本语法

```java
.filed private isFlag:z 定义变量
.nethod 方法
.parameter 方法参数
.prologue 方法开始
.line 123 此方法位于123行
invoke-super 调用父函数
const/high16 v0,0x7fo3 把0x7fo3赋值给v0
invoke-direct 调用函数
return-void 函数返回void
.end method 函数结束
new-instance 创建实例
iput-object 对象赋值
iget-object 调用对象
invoke-static 调用静态函数
```

- 条件跳转

```java
“if-eq VA,VB,:cond_xx “ 如果VA=VB则跳转到cond_xx
“if-ne VA,VB,:cond_xx “ 如果VA!=VB则跳转到cond_xx
“if-lt VA,VB,:cond_xx “ 如果VA<VB则跳转到cond_xx
“if-ge VA,VB,:cond_xx “ 如果VA>=VB则跳转到cond_xx
“if-gt VA,VB,:cond_xx “ 如果VA>VB则跳转到cond_xx
“if-le VA,VB,:cond_xx “ 如果VA<=VB则跳转到cond_xx
“if-eqz VA,:cond_xx “ 如果VA=0则跳转到cond_xx
“if-nez VA,:cond_xx “ 如果VA!=0则跳转到cond_xx
“if-ltz VA,:cond_xx “ 如果VA<0则跳转到cond_xx
“if-gez VA,:cond_xx “ 如果VA>=0则跳转到cond_xx
“if-gtz VA,:cond_xx “ 如果VA>0则跳转到cond_xx
“if-lez VA,:cond_xx “ 如果VA<=0则跳转到cond_xx
```

### smali 函数分析

#### 函数调用

smali中的函数和成员变量一样也分为两种类型

- direct
- virtual

简而言之 direct method就是private函数，其余的public和protected都属于virtual method。因此在函数调用时有invoke-direct，invoke-virtual，，另外还有invoke-static、invoke-super以及invoke-interface

> 1、invoke-static： 用于调用static函数的

如：

```java
invoke-static{}，Lcom/aaa;->CheckSignature()Z
const-string v0,”NDKLIB”
invoke-static {v0}，Ljava/lang/System;->loadLibray(Ljava/lang/String;)V
```

这两句对应着 static void System.loadLiary(String)来加载NDK编译的so库方法，这里的v0就是参数“NDKLIB”

> 2、invoke-super:调用父类方法用的指令，一般用于调用onCreate、onDestroy等方法
>
> 3、invoke-direct 调用private函数

如：

invoke-direct{p0}，Landroid/app/TabActivity;->()V

对应就是在TabActvity中定义一个private函数

> 4、invoke-virtual

用于调用protected和public函数

> 5、 invoke-xxx/range:当方法

当方法的参数多于5个时（含5个），不能直接使用上面的指令，而是在后面加上/range，并且传递参数不需要写全，可以以省略号替代，如：

```java
invoke-direct/range{v0……v5}，Lcmb/pb/ui/PBContainerActivity;->h(ILjava/lang/CharSeqence;Ljava/lang/String;Landroid/content/Intent;I)Z
```

#### 函数返回结果操作

在java代码汇总调用函数和返回结果可以用一条语句完成，而在smali里则需要分开来完成，使得使用上述指令后，如果调用的函数返回非void，那么还需要用到move-result(返回基本数据类型)和move-result-object(返回对象)指令，如：

```java
const-string v0，“hello”
invoke-static{v0}，Lcmb/pbi;->t(Ljava/lang/String;)Ljava/lang/String;
move-result-object v2
```

其中v2保存的就是调用t方法返回的String字符串

#### 示例

```java
.local 4 //本地寄存器4个，即v0，v1，v2，v3
const/4 v2 0x1 //4字节常量v2=1
const/16 v1 0x10 //16字节常量 v1=16
：local v1，“length”：I //本地寄存器int length =v1
if-nez v1,: cond_1 //如果v1！=0，跳转至cond_1
：cond_0 //cond_0标签
：goto_0 //goto_0标签
return v2 //返回v2
：cond_1 //开始执行cond_1标签代码
const/4 v0，0x0 //4字节常量 v0=0
：local v0，”i”:I //本地寄存器 int i=0
：goto_1 //开始执行goto_1标签代码
if-lt v0，v1，：cond_2 //如果v0<v1,则跳转cond_2
const/16 v3,0x28 //接上，如果v0>=v1,则执行 16字节常量v3=40
if-le v1,v3,:cond_0 //接上，如果v1<=v3,则跳转至cond_0,即返回v2的值
const/4 v2,0x0 //接上，如果v1大于v3，则4字节常量 v2=0
goto：goto_0 //跳转至goto_0,即返回v2值
：cond_2 //cond_2标签
xor-int/lit8 v1,v1,0x3b //将第二个v1寄存器中的值与0x3b(59)进行异或运算，得到的赋值给第一个v1寄存器中
add-int/lit8 v0,v0,0x01 //将第二个v0寄存器中的值加上0x1(1),所得的值放入到第一个v0寄存器中
goto：goto_1 //跳转至goto_1,标签，这里可以看出cond_2,实际是一个for循环，而不是简单的IF判断
```

翻译成代码

```java
int v2 = 1;
int v1 = 16;
if (v1 != 0)
{
        for (int v0 = 0; v0 < v1;)
        {
                v1 = v1 ^ 59;
                v0++;
        }
        if (v1 > 40)
        {
                v2 = 0;
        }
}
return v2;
```

### java代码转变为samli代码

方法一：

可以使用android SDK提供的一个工具dx，该jar包位于android-sdk\build-tools\23.0.1\lib，找到该包后执行以下命令

> javac Test.java 得到Test.class
>
> java -jar dx.jar –dex –output=Test.dex Test.class 得到dex文件

这时候会使用到另外一个工具baksmali，该工具位于android-sdk\platform-tools\，找到该包后执行以下命令

> java -jar baksmali.jar Test.dex 得到smali文件

但是jdk1.8第二步无效，jdk1.7可以

方法二：

Android studio装 code2smali插件

https://github.com/ollide/intellij-java2smali



### 参考：

- [http://yeungeek.com/2015/08/23/Android%E5%8F%8D%E7%BC%96%E8%AF%91%E4%B9%8B%E4%BA%8C-Smali%E8%AF%AD%E6%B3%95%E7%AE%80%E4%BB%8B/](http://yeungeek.com/2015/08/23/Android反编译之二-Smali语法简介/)

- [http://blog.isming.me/2015/01/14/android-decompile-smali/](http://blog.isming.me/2015/01/14/android-decompile-smali/)

- [http://www.cnblogs.com/gordon0918/p/5466514.html](http://www.cnblogs.com/gordon0918/p/5466514.html)

- [http://www.52pojie.cn/thread-395689-1-1.html](http://www.52pojie.cn/thread-395689-1-1.html)