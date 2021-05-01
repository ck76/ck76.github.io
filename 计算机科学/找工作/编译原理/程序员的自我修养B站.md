https://www.bilibili.com/video/BV1xf4y127AJ?p=1



[TOC]



- 操作系统替我们屏蔽了硬件相关

  <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmm21ddo4j30os0hwn2l.jpg" alt="image-20201213224508032" style="zoom:50%;" />

- 虚拟内存有多大与cpu的地址总线有关

-  程序在运行的时候没有直接跑到物理内存上，因为操作系统需要屏蔽底层硬件操作，只能得到一块虚拟地址空间 2^32=4G[虚拟]

  每一个程序都有，大家都一样。

  用户空间和内核空间。

  ```c
  char * p=NULL;
  ....
  strlen(p);//访问0地址内存
  
  ```

  0~0X04800读都不能读，这是禁止访问空间

  ```c
  //main函数原型
  int main(int argc, char ** argv, char ** environ){
    //argc是命令行参数的个数
    //argv是命令行参数
    //environ环境变量。传了库和头文件这些编译链接所需的文件路径
  }
  ```

  <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmme0j186j31300l2dzv.jpg" style="zoom:50%;" />

- gcc -c main.c

  ```c
  // #include "s"
  
  int gdata1 = 10;
  int gdata2 = 0;
  int gdata3;
  
  static int gdata4 = 11;
  static int gdata5 = 0;
  static int gdata6 ;
  
  int main(){
      int a =12;
      int b =0 ;
      int c;
  
      static int d = 13;
      static int e = 0;
      static int f;
  
      return 0;
  }
  ```
  
- 数据是一定要产生符号的，函数只产生一个符号就是函数名字

- <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmmiq24ddj312o0hawkh.jpg" alt="image-20201213230109482" style="zoom: 50%;" />

- <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmmiywl4lj30py0bmtcr.jpg" alt="image-20201213230122739" style="zoom:50%;" />

- Objdump -h main.o

  ```sh
  ❯ objdump -h main.o
  
  main.o:	file format Mach-O 64-bit x86-64
  
  Sections:
  Idx Name          Size      Address          Type
    0 __text        0000001d 0000000000000000 TEXT
    1 __data        00000008 0000000000000020 DATA
    2 __common      00000004 0000000000000088 BSS
    3 __bss         00000008 000000000000008c BSS
    4 __compact_unwind 00000020 0000000000000028 DATA
    5 __eh_frame    00000040 0000000000000048 DATA
  
  ```

- <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmmoc9cztj30vi0om7m0.jpg" alt="image-20201213230635359" style="zoom:50%;" />

- <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmmpb0bkaj30vm0ge155.jpg" alt="image-20201213230731223" style="zoom:50%;" />

- 仅仅编译这一步是不分配符号内存地址的，链接的时候分配，在符号解析完成之后就分配内存地址，不用物理内存，用的是虚拟内存，每个人都有自己的用户空间，不冲突。
- bss端不占文件的空间，占虚拟内存的空间

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmmsvddg5j30ws0hyh2p.jpg" alt="image-20201213231056151" style="zoom:50%;" />

- <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmmuw7qfmj30xc0dsk4i.jpg" alt="image-20201213231252394" style="zoom:50%;" />

- <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmmvtjl0nj30pw0g27fg.jpg" alt="image-20201213231346857" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmmy8ozi1j30zy0qae5x.jpg" alt="image-20201213231606025" style="zoom:50%;" />

- <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmmzjj1bjj31320ti7m9.jpg" alt="image-20201213231721038" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmn07v044j30my0aejvp.jpg" alt="image-20201213231800903" style="zoom:50%;" />

- c++在全局不能定义同名变量
- c语言能定义强符号和弱符号，有初始化的是强符号，没初始化的是弱符号。初始化是0也是初始化。
- 弱让强，同的话就看谁占用内存大选谁。，同强同弱[同类型]看编译器如何选择。

- func函数中的x赋值实在编译阶段就选择好的，但是确定写的x是main中的x则是在连接的时候决定的。
- static是静态，本文件可见。
- <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmn70pj5dj30qs0isk6d.jpg" alt="image-20201213232432181" style="zoom:50%;" />

- 弱符号在common段里，因为还未决定。
- <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmn9epcysj30ro0bkgmg.jpg" alt="image-20201213232640468" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmn9zeovbj31420t84f8.jpg" alt="image-20201213232723661" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnajmlv6j30rg0ik16m.jpg" alt="image-20201213232750551" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnb0tvayj30r40cetgk.jpg" alt="image-20201213232820634" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnbq6uxbj312y0scb29.jpg" alt="image-20201213232904196" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmncc5na5j311y0lgk4y.jpg" alt="image-20201213232938069" style="zoom:50%;" />

- 链接90%都是符号表的问题

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmndbwp7rj31360psx54.jpg" alt="image-20201213233036573" style="zoom:50%;" />

- 编译的时候的地址都不是真实地址，编译过程所有用数据的地方都是0地址，函数都是互相的偏移量（下一行指令地址的偏移量）（不一定），cpu执行当前指令之后检查pc寄存器，pc寄存器保存的是下一行的地址，如何计算函数的地址，

---

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnh0cjrij31100din84.jpg" alt="image-20201213233408524" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmni0h5vkj30l80860ym.jpg" alt="image-20201213233504458" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnis2jm7j30t40b611o.jpg" alt="image-20201213233544187" style="zoom:50%;" />

- gdata3决定了，bss端就是24/4=6个

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnjoq8nuj30nm0k07ic.jpg" alt="image-20201213233627229" style="zoom:50%;" />

- 解析之后符号都有正确的地址了
- 连接之前

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnkndc4rj30p40k4gnq.jpg" alt="image-20201213233730738" style="zoom: 67%;" />

- 链接之后

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnlcqqazj30s20p6dva.jpg" alt="image-20201213233818893" style="zoom:67%;" />

- 数据都是绝对地址，函数涉及指令跳转填的都是偏移量
- call指令80480ca+0a000000=<\sum> 080880d4

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnoana18j311q0h2tjb.jpg" alt="image-20201213234108249" style="zoom:67%;" />



---



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnpn736mj312a0iqdt9.jpg" alt="image-20201213234226845" style="zoom:67%;" />

- <\main>的地址
- 可执行文件是以页为基本单位的

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnulpzgxj30ws0dgtic.jpg" alt="image-20201213234712409" style="zoom:67%;" />

- 两个load页按页面对齐
- 第一个load页是text段（可读可执行）
- 第二个是数据段（可读可写）
- 按属性合并段，这个段是给加载器用的，告诉加载器如何加载程序



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnyau6g6j31340oah7q.jpg" alt="image-20201213234957816" style="zoom: 50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmnydaz1dj31300rk7wh.jpg" alt="image-20201213235010825" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmo3jqroij312y0hkazn.jpg" alt="image-20201213235544453" style="zoom: 50%;" />

- 很多mmap函数

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmo5g8ajgj31480ns4i4.jpg" alt="image-20201213235737083" style="zoom:50%;" />

- 两个run从0x08048000开始

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glmo6zvevmj310a0kgjzq.jpg"  style="zoom: 50%;" />



---

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9fqgwujj30m60q4gu4.jpg" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9fq3n1kj315c0q2dyn.jpg" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9fmcallj30k00ikwqk.jpg" style="zoom:50%;" />



- 函数局部变量不属于数据（如是数据，每一个数据都要产生符号），访问都是通过栈底指针的偏移地址
- ebp底
- esp顶



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9fkyvoxj31840t44l9.jpg" style="zoom:50%;" />



![image-20201215090838788](https://tva1.sinaimg.cn/large/0081Kckwly1glo9p3hvi7j31kt0u04qq.jpg)







<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9feaqyoj30pa0ewdie.jpg" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9fcvqaxj30s80io7ad.jpg" style="zoom:50%;" />





<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9fc8rl8j31h50u0e81.jpg" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9f8tmp8j30k00imn8y.jpg" style="zoom:50%;" />

- 固定的这一段汇编做了哪些事？
  - <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9f7ffi4j31240lwn4a.jpg" style="zoom:50%;" />
  - <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9f5fbmgj30su0jm0yz.jpg" style="zoom:50%;" />
  -  <img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9f451zcj31jk0r6kai.jpg" style="zoom:50%;" />
  - ![image-20201214140821960]<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9f1pwdrj31f60omql7.jpg" style="zoom:50%;" />
  - 第一步：先把调用方函数(main)的栈底放在当前执行函数的栈底
  - ebp重新指向当前函数的栈底、esp-=44h，当成esp移动了44H这么长，开辟出了新的函数栈空间、
  - esp到ebp的空间初始化0XCCCCCCCC

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9ez39i0j31h50u0x6p.jpg" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9ey5yqpj31h50u0u0x.jpg" style="zoom:50%;" />

- 回退栈帧只是对esp和ebp的更改，栈中的数据并没有清除，如果通过非法手段仍然能访问到。



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9evpkcwj314q0q814x.jpg" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9euqzyoj31h50u0x6p.jpg" style="zoom:50%;" />

- 出栈esp先向下挪、然后将pop出来的元素赋给ebp，ebp又重回main函数的栈底位置，



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9eu5oqej313s0qik55.jpg" style="zoom:50%;" />

<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9erz674j31h50u0x6p.jpg" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9eppwjtj31h50u0x6p.jpg" style="zoom:50%;" />

- 释放形参变量内存
- ps形参实参：
```
形参：全称为“形式参bai数”是在定义函数名和函数体的时候使用的参数，目的是用来接收调用该函数时传递的参数。
形参的作用是实现主调函数与被调函数之间的联系，通常将函数所处理的数据，影响函数功能的因素或者函数处理的结果作为形参。没有形参的函数在形参表的位置应该写void.main 函数也可以有形参和返回值，其形参也称为命令行参数，由操作系统在启动程序时初始化，其返回值传递给操作系统。
实参：可以是常量、变量、表达式、函数等， 无论实参是何种类型的量，在进行函数调用时，它们都必须具有确定的值， 以便把这些值传送给形参。 因此应预先用赋值，输入等办法使实参获得确定值。
```


<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9eo7vo9j31h50u0x6p.jpg" style="zoom:50%;" />

- return返回的调用函数执行结果保存在eax寄存器当中，然后mov指令将eax值赋给ebp-0Ch 



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9en6jdsj318a0k0gw6.jpg" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9ekgm8kj31h50u0e81.jpg" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9ej4z8fj31h50u0b29.jpg" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9ei46y4j31h50u0e81.jpg" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9efoamuj31h50u0hdt.jpg" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9edvivuj31h50u0b29.jpg" style="zoom:50%;" />



<img src="https://tva1.sinaimg.cn/large/0081Kckwly1glo9ed5l1lj31h50u0e81.jpg" style="zoom:50%;" />



