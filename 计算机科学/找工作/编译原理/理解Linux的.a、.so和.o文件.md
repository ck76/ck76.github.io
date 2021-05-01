**在说明Linux的.a、.so和.o文件关系之前，先来看看windows下obj,lib,dll,exe的关系**

### windows下obj,lib,dll,exe的关系

  lib是和dll对应的。lib是静态链接库的库文件，dll是动态链接库的库文件。 
  所谓静态就是link的时候把里面需要的东西抽取出来安排到你的exe文件中，以后运行你的exe的时候不再需要lib。
  所谓动态就是exe运行的时候依赖于dll里面提供的功能，没有这个dll，你的exe无法运行。 

  lib,dll,exe都算是最终的目标文件，是最终产物。而c/c++属于源代码。源代码和最终目标文件中过渡的就是中间代码obj，实际上之所以需要中间代码，是你不可能一次得到目标文件。比如说一个exe需要很多的cpp文件生成。而编译器一次只能编译一个cpp文件。这样编译器编译好一个cpp以后会将其编译成obj，当所有必须要的cpp都编译成obj以后，再统一link成所需要的exe，应该说缺少任意一个obj都会导致exe的链接失败。

  1.obj里存的是编译后的代码跟数据，并且有名称，所以在连接时有时会出现未解决的外部符号的问题。当连成exe后便不存在名称的概念了，只有地址。lib就是一堆obj的组合。
  2.理论上可以连接obj文件来引用其他工程(可以认为一个obj文件等价于编译生成它的cpp文件,可以引用obj来替换cpp,也可以添加cpp来替换obj )，但实际中通常用lib来实现工程间相互引用。
  3.编译器会默认链接一些常用的库，其它的需要你自己指定。

###  lib和DLL的区别

  (1)lib是编译时需要的，dll是运行时需要的。如果要完成源代码的编译，有lib就够了。如果也使动态连接的程序运行起来，有dll就够了。在开发和调试阶段，当然最好都有。
  (2) 一般的动态库程序有lib文件和dll文件。lib文件是必须在编译期就连接到应用程序中的，而dll文件是运行期才会被调用的。如果有dll文件，那么对应的lib文件一般是一些索引信息，具体的实现在dll文件中。如果只有lib文件，那么这个lib文件是静态编译出来的，索引和实现都在其中。 静态编译的lib文件有好处：给用户安装时就不需要再挂动态库了。但也有缺点，就是导致应用程序比较大，而且失去了动态库的灵活性，在版本升级时，同时要发布新的应用程序才行。
  (3)在动态库的情况下，有两个文件，一个是引入库（.LIB）文件(实际上也算是一个静态库,只是在链接时只能把函数在DLL的入口链接到exe中,而不像真正静态链接库那样将函数体真正链接到exe中 ,通过lib进行的动态链接实际上也使用了静态链接来实现 )，一个是DLL文件，引入库文件包含被DLL导出的函数的名称和位置，DLL包含实际的函数和数据，应用程序使用LIB文件链接到所需要使用的DLL文件，库中的函数和数据并不复制到可执行文件中，因此在应用程序的可执行文件中，存放的不是被调用的函数代码，而是DLL中所要调用的函数的内存地址，这样当一个或多个应用程序运行是再把程序代码和被调用的函数代码链接起来，从而节省了内存资源。从上面的说明可以看出，DLL和.LIB文件必须随应用程序一起发行，否则应用程序将会产生错误。

DLL内的函数分为两种： 
  (1)DLL导出函数，可供应用程序调用；
  (2)DLL内部函数，只能在DLL程序使用，应用程序无法调用它们

创建静态链接库和创建动态链接库

  VC6中创建[Win32 Dynamic-Link Library]工程便可以创建出一个空的DLL工程.

  VC6中创建[Win32 Static Library]工程便可以创建出一个空的LIB工程(静态链接库工程,仅生成一个lib文件).

添加lib文件的常用办法有二个: 
  1、把*.lib放在VC的Lib目录中 
  2、修改project setting的Link->Input中的Addtional library path，加入你的目录dll:是可实际运行的二进制代码，有定位代码的！

  3、也可以在object/library中直接写上lib文件路径.(这里实际上是可以写上任意obj文件或者lib文件的).

### linux .o,.a,.so     

​        .o,是目标文件,相当于windows中的.obj文件 

　　.so 为共享库,是shared object,用于动态连接的,相当于windows下的dll 

　　.a为静态库,是好多个.o合在一起,用于静态连接 

 

静态函数库
特点：实际上是简单的普通目标文件的集合，在程序执行前就加入到目标程序中。
优点：可以用以前某些程序兼容；描述简单；允许程序员把程序link起来而不用重新编译代码，节省了重新编译代码的时间（该优势目前已不明显）；开发者可以对源代码保密；理论上使用ELF格式的静态库函数生成的代码可以比使用共享或动态函数库的程序运行速度快（大概1%-5%）
生成：使用ar程序（archiver的缩写）。ar rcs my_lib.a f1.o f2.o是把目标代码f1.o和f2.o加入到my_lib.a这个函数库文件中（如果my_lib.a不存在则创建）
使用：用gcc生成可执行代码时，使用-l参数指定要加入的库函数。也可以用ld命令的-l和-L参数。

共享函数库
  共享函数库在可执行程序启动的时候加载，所有程序重新运行时都可自动加载共享函数库中的函数。.so文件感觉很复杂，光是命名规则就已经看得我很晕了~整理一下，共享库需要：soname、real name，另外编译的时候名字也有说法。依次解释下：
soname：必须的格式：lib+函数库名+.so+版本号信息（但是记住，非常底层的C库函数都不是以lib开头命名的）。例子：/usr/lib/libreadline.so.3
real name：顾名思义是真正的名字啦，有主版本号和发行版本号。但是没找到实例……
编译器编译的时候需要的函数库的名字就是不包含版本号信息的soname，例如上面的例子把最后的.3去掉就可以了。
位置：共享函数库文件必须放在特定目录，对于开放源码来说，GNU标准建议所有的函数库文件都放在/usr/local/lib目录下，而且建议命令、可执行程序都放在/usr/local/bin目录下。不过这个只是习惯啦，可以改变，具体的位置信息可以看/etc/ld.so.conf里面的配置信息。当然，也可以修改这个文件，加入自己的一些特殊的路径要求。
创建：在网上找到了gcc方式和easyeclipse环境下两种创建方式。
gcc方式：
  首先创建object文件，这个文件将加入通过gcc –fPIC 参数命令加入到共享函数库里面，标准格式：gcc -shared -Wl,-soname,your_soname -o library_name file_list library_list（说实话这个标准格式看起来好复杂，我找了个实例，但是好像和那个标准格式稍有不同：gcc test_a.c test_b.c test_c.c -fPIC -shared -o libtest.so）
在easyeclipse环境下生成.so文件：
    1.选择新建工程，建立一个c++工程
    2.在工程类型选项里选择 Shared Library，然后填入工程名字PXXX点击完成即可。
    3.编写程序，然后编译就会在debug或者release里生成一个libPXXX.so文件，如果不要lib的起头标记点击project菜单的Properties选项，然后在弹出的界面的右边点击Build artifact页面，将Output prefix选项的内容清空即可。
    4.如果是C++程序，注意在接口函数的前面加上extern "C"标记，在头文件加上如下标记：

```
#ifdef   __cplusplus  



#extern   "C"{  



#endif  



   
```


头文件主体 


```
#ifdef   __cplusplus  



}  



#endif  
```


   如果不加以上标记，经过编译后，so里的函数名并非你编写程序时设定的函数名，在开发环境左侧的工程文件列表中点开debug项里的PXXX.o可以看到so文件里的函数名都是在你设定的函数名后面加了一个__Fi标记，比如你用的设定的函数名称是Func(), 而so里的函数名则为Func__Fi()或者其他的名称。
安装：拷贝共享库文件到指定的标准的目录，然后运行ldconfig。如果没有权限这样做，那么就只好通过修改环境变量来实现这些函数库的使用了。方法不再说了，很复杂。
查看：可以通过运行ldd来看某个程序使用的共享函数库。例如ldd /bin/ls。查看.so文件使用nm命令，如nm libXXX.so。（注意，nm对于静态的函数库和共享的函数库都起作用）
关于覆盖：如果想用自己的函数覆盖某个库中的一些函数，同时保留该库中其他的函数的话，可以在/etc/ld.so.preload中加入要替换的库（.o结尾的文件），这些preloading的库函数将有优先加载的权利。
关于更新：每次新增加动态加载的函数库、删除某个函数库或者修改某个函数库的路径时，都要重新运行ldconfig来更新缓存文件/etc/ld.so.cache,此文件保存已排好序的动态链接库名字列表

(在Linux下，共享库的加载是由/lib/ld.so完成的，ld.so加载共享库时，会从ld.so.cache查找)

我们通常把一些公用函数制作成函数库，供其它程序使用。函数库分为静态库和动态库两
种。静态库在程序编译时会被连接到目标代码中，程序运行时将不再需要该静态库。动态
库在程序编译时并不会被连接到目标代码中，而是在程序运行是才被载入，因此在程序运
行时还需要动态库存在。本文主要通过举例来说明在Linux中如何创建静态库和动态库，以
及使用它们。

在创建函数库前，我们先来准备举例用的源程序，并将函数库的源程序编译成.o文件。


第1步：编辑得到举例的程序--hello.h、hello.c和main.c；

hello.c(见程序2)是函数库的源程序，其中包含公用函数hello，该函数将在屏幕上输出"
Hello XXX!"。hello.h(见程序1)为该函数库的头文件。main.c(见程序3)为测试库文件的
主程序，在主程序中调用了公用函数hello。

程序1: hello.h

```
#ifndef HELLO_H



#define HELLO_H



 



void hello(const char *name);



 



#endif //HELLO_H
```

程序2: hello.c

```
#include <stdio.h>



 



void hello(const char *name)



{



  printf("Hello %s!\n", name);



}



 
```

程序3: main.c

```
#include "hello.h"



 



int main()



{



  hello("everyone");



  return 0;



}
```

第2步：将hello.c编译成.o文件；

无论静态库，还是动态库，都是由.o文件创建的。因此，我们必须将源程序hello.c通过g
cc先编译成.o文件。

在系统提示符下键入以下命令得到hello.o文件。

```
# gcc -c hello.c



 



#
```

我们运行ls命令看看是否生存了hello.o文件。

```
# ls



 



hello.c hello.h hello.o main.c



 



#
```

在ls命令结果中，我们看到了hello.o文件，本步操作完成。

下面我们先来看看如何创建静态库，以及使用它。

第3步：由.o文件创建静态库；

静态库文件名的命名规范是以lib为前缀，紧接着跟静态库名，扩展名为.a。例如：我们将
创建的静态库名为myhello，则静态库文件名就是libmyhello.a。在创建和使用静态库时，
需要注意这点。创建静态库用ar命令。

在系统提示符下键入以下命令将创建静态库文件libmyhello.a。

```
# ar -cr libmyhello.a hello.o



 



#
```

我们同样运行ls命令查看结果：

```
# ls



 



hello.c hello.h hello.o libmyhello.a main.c



 



#
```

ls命令结果中有libmyhello.a。

第4步：在程序中使用静态库；

静态库制作完了，如何使用它内部的函数呢？只需要在使用到这些公用函数的源程序中包
含这些公用函数的原型声明，然后在用gcc命令生成目标文件时指明静态库名，gcc将会从
静态库中将公用函数连接到目标文件中。注意，gcc会在静态库名前加上前缀lib，然后追
加扩展名.a得到的静态库文件名来查找静态库文件。

在程序3:main.c中，我们包含了静态库的头文件hello.h，然后在主程序main中直接调用公
用函数hello。下面先生成目标程序hello，然后运行hello程序看看结果如何。

法一 # gcc -o hello main.c -L. –lmyhello，或gcc  main.c -L. –lmyhello -o hello自定义的库时，main.c还可放在-L.和 –lmyhello之间，但是不能放在它俩之后，否则会提示myhello没定义，但是是系统的库时，如g++ -o main（-L/usr/lib） -lpthread main.cpp就不出错。

法二 #gcc main.c libmyhello.a -o hello或gcc  -o hello main.c libmyhello.a

法三：先生成main.o：gcc -c main.c ，再生成可执行文件：gcc -o hello main.o libmyhello.a或gccmain.o libmyhello.a -o hello ，动态库连接时也可以这样做。

```
# ./hello



 



Hello everyone!







#
```

我们删除静态库文件试试公用函数hello是否真的连接到目标文件 hello中了。

```
# rm libmyhello.a



 



rm: remove regular file `libmyhello.a'? y







# ./hello



 



Hello everyone!







#
```

程序照常运行，静态库中的公用函数已经连接到目标文件中了。

我们继续看看如何在Linux中创建动态库。我们还是从.o文件开始。

第5步：由.o文件创建动态库文件；

动态库文件名命名规范和静态库文件名命名规范类似，也是在动态库名增加前缀lib，但其
文件扩展名为.so。例如：我们将创建的动态库名为myhello，则动态库文件名就是libmyh
ello.so。用gcc来创建动态库。

在系统提示符下键入以下命令得到动态库文件libmyhello.so。

```
# gcc -shared -fPIC -o libmyhello.so hello.o （-o不可少）



 



#
```

我们照样使用ls命令看看动态库文件是否生成。

```
# ls



 



hello.c hello.h hello.o libmyhello.so main.c



 



#
```

第6步：在程序中使用动态库；

在程序中使用动态库和使用静态库完全一样，也是在使用到这些公用函数的源程序中包含
这些公用函数的原型声明，然后在用gcc命令生成目标文件时指明动态库名进行编译。我们
先运行gcc命令生成目标文件，再运行它看看结果。

```
# gcc -o hello main.c -L. -lmyhello
```

(或 #gcc main.c libmyhello.so -o hello 不会出错（没有libmyhello.so的话，会出错），但是接下来./hello 会提示出错，因为虽然连接时用的是当前目录的动态库，但是运行时，是到/usr/lib中找库文件的，将文件libmyhello.so复制到目录/usr/lib中就OK了)

```
# ./hello



 



./hello: error while loading shared libraries: libmyhello.so: cannot open shar



ed object file: No such file or directory



 



#
```

哦！出错了。快看看错误提示，原来是找不到动态库文件libmyhello.so。程序在运行时，
会在/usr/lib和/lib等目录中查找需要的动态库文件。若找到，则载入动态库，否则将提
示类似上述错误而终止程序运行。我们将文件libmyhello.so复制到目录/usr/lib中，再试
试。

```
# mv libmyhello.so /usr/lib







# ./hello



 



Hello everyone!







#
```

成功了。这也进一步说明了动态库在程序运行时是需要的。

我们回过头看看，发现使用静态库和使用动态库编译成目标程序使用的gcc命令完全一样，
那当静态库和动态库同名时，gcc命令会使用哪个库文件呢？抱着对问题必究到底的心情，
来试试看。

先删除除.c和.h外的所有文件，恢复成我们刚刚编辑完举例程序状态。

```
# rm -f hello hello.o /usr/lib/libmyhello.so



 



# ls



 



hello.c hello.h main.c



 



#
```

在来创建静态库文件libmyhello.a和动态库文件libmyhello.so。

在生成动态库时，需要使用-fPIC，这样才能生成位置无关的代码，达到代码段和数据段共享的目的

```
# gcc -c -fpic hello.c  //编译hello.c时也需要加上-fpic选项，否则rodata' can not be used when making a shared object; recompile with -fPIC



 



# ar -cr libmyhello.a hello.o （或-cvr ）



 



# gcc -shared -fPIC -o libmyhello.so hello.o



 



# ls



 



hello.c hello.h hello.o libmyhello.a libmyhello.so main.c



 



#
```

通过上述最后一条ls命令，可以发现静态库文件libmyhello.a和动态库文件libmyhello.s
o都已经生成，并都在当前目录中。然后，我们运行gcc命令来使用函数库myhello生成目标
文件hello，并运行程序 hello。

```
# gcc -o hello main.c -L. –lmyhello （动态库和静态库同时存在时，优先使用动态库， 当然，直接#gcc main.c libmyhello.a -o hello的话，就是指定为静态库了）



 



# ./hello



 



./hello: error while loading shared libraries: libmyhello.so: cannot open shar



ed object file: No such file or directory



 



#
```

从程序hello运行的结果中很容易知道，当静态库和动态库同名时，gcc命令将优先使用动态库，默认去连/usr/lib和/lib等目录中的动态库，将文件libmyhello.so复制到目录/usr/lib中即可。

Note:
编译参数解析
最主要的是GCC命令行的一个选项:
-shared 该选项指定生成动态连接库（让连接器生成T类型的导出符号表，有时候也生成弱连接W类型的导出符号），不用该标志外部程序无法连接。相当于一个可执行文件


-fPIC 作用于编译阶段，告诉编译器产生与位置无关代码(Position-Independent Code)。那么在产生的代码中，没有绝对地址，全部使用相对地址，故而代码可以被加载器加载到内存的任意位置，都可以正确的执行。这正是共享库所要求的，共享库被加载时，在内存的位置不是固定的。

如果不加fPIC,则编译出来的代码在加载时需要根据加载到的位置进行重定位(因为它里面的代码并不是位置无关代码)，如果被多个应用程序共同使用,那么它们必须每个程序维护一份so的代码副本了.(因为so被每个程序加载的位置都不同,显然这些重定位后的代码也不同,当然不能共享)。

不用此选项的话编译后的代码是位置相关的，所以动态载入时是通过代码拷贝的方式来满足不同进程的需要，而不能达到真正代码段共享的目的。

-L. 表示要连接的库在当前目录中；（多个库：在编译命令行中，将使用的静态库文件放在源文件后面就可以了。比如：gcc -L/usr/lib myprop.c libtest.a libX11.a libpthread.a -o myprop
其中-L/usr/lib指定库文件的查找路径。编译器默认在当前目录下先查找指定的库文件，如前面的“法二 #gccmain.c libmyhello.a-o hello”）


-lmyhello 编译器查找动态连接库时有隐含的命名规则，即在给出的名字前面加上lib，后面加上.so或.a来确定库的名称libmyhello.so或libmyhello.a。
LD_LIBRARY_PATH这个环境变量指示动态连接器可以装载动态库的路径。
当然如果有root权限的话，可以修改/etc/ld.so.conf文件，然后调用 /sbin/ldconfig来达到同样的目的，不过如果没有root权限，那么只能采用输出LD_LIBRARY_PATH的方法了。

调用动态库的时候有几个问题会经常碰到，有时，明明已经将库的头文件所在目录 通过 “-I” include进来了，库所在文件通过 “-L”参数引导，并指定了“-l”的库名，但通过ldd命令察看时，就是死活找不到你指定链接的so文件，这时你要作的就是通过修改 LD_LIBRARY_PATH或者/etc/ld.so.conf文件来指定动态库的目录。通常这样做就可以解决库无法链接的问题了。

静态库链接时搜索路径顺序：

\1. ld(GNU linker)会去找GCC命令中的参数-L

  编译过程是分为四个阶段：预处理(也称预编译，Preprocessing)、编译(Compilation)、汇编 (Assembly)和连接(link)  【链接】
\2. 再找gcc的环境变量LIBRARY_PATH
\3. 再找内定目录 /lib /usr/lib /usr/local/lib 这是当初compile gcc时写在程序内的

动态链接时、执行时搜索路径顺序:

\1. 编译目标代码时指定的动态库搜索路径
\2. 环境变量LD_LIBRARY_PATH指定的动态库搜索路径
\3. 配置文件/etc/ld.so.conf中指定的动态库搜索路径
\4. 默认的动态库搜索路径/lib
\5. 默认的动态库搜索路径/usr/lib

有关环境变量：
LIBRARY_PATH环境变量：指定程序静态链接库文件搜索路径
LD_LIBRARY_PATH环境变量：指定程序动态链接库文件搜索路径


另：

从上述可知，如何找到生成的动态库有3种方式：

(1)把库拷贝到/usr/lib和/lib目录下。

(2)在LD_LIBRARY_PATH环境变量中加上库所在路径。

例如动态库libhello.so在/home/example/lib目录下：

export LD_LIBRARY_PATH=LD_LIBRARY_PATH:/home/example/lib

(3) 修改/etc/ld.so.conf文件，把库所在的路径加到文件末尾(直接写在文件末尾，不要在路径前加include)，并执行ldconfig刷新（ldconfig 命令的用途,主要是在默认搜寻目录(/lib和/usr/lib)以及动态库配置文件/etc/ld.so.conf内所列的目录下,搜索出可共享的动态链接库(格式如前介绍,lib*.so*),进而创建出动态装入程序(ld.so)所需的连接和缓存文件.缓存文件默认为/etc/ld.so.cache,此文件保存已排好序的动态链接库名字列表.）。这样，加入的目录下的所有库文件都可见。

附：像下面这样指定路径去连接系统的静态库，会报错说要连接的库找不到:

g++ -o main main.cpp -L/usr/lib libpthread.a 

必须这样g++ -o main main.cpp -L/usr/lib -lpthread才正确 。

自定义的库考到/usr/lib 下时，

g++ -o main main.cpp -L/usr/lib libpthread.a libthread.a libclass.a会出错，但是这样g++ -o main main.cpp -L/usr/lib -lpthread -lthread -lclass就正确了。

https://blog.csdn.net/qq_37806908/article/details/97686753

---

