## 什么是JVM？

JVM是Java Virtual Machine（Java[虚拟机](https://baike.baidu.com/item/%E8%99%9A%E6%8B%9F%E6%9C%BA)）的缩写，JVM是一种用于计算设备的规范，它是一个虚构出来的计算机，是通过在实际的计算机上仿真模拟各种计算机功能来实现的。

[Java语言](https://baike.baidu.com/item/Java%E8%AF%AD%E8%A8%80)的一个非常重要的特点就是与平台的无关性。而使用Java虚拟机是实现这一特点的关键。一般的高级语言如果要在不同的平台上运行，至少需要编译成不同的[目标代码](https://baike.baidu.com/item/%E7%9B%AE%E6%A0%87%E4%BB%A3%E7%A0%81/9407934)。而引入Java语言虚拟机后，Java语言在不同平台上运行时不需要重新编译。Java语言使用Java虚拟机屏蔽了与具体平台相关的信息，使得Java语言[编译程序](https://baike.baidu.com/item/%E7%BC%96%E8%AF%91%E7%A8%8B%E5%BA%8F/8290180)只需生成在Java虚拟机上运行的目标代码（[字节码](https://baike.baidu.com/item/%E5%AD%97%E8%8A%82%E7%A0%81/9953683)），就可以在多种平台上不加修改地运行。Java虚拟机在执行字节码时，把字节码解释成具体平台上的[机器指令](https://baike.baidu.com/item/%E6%9C%BA%E5%99%A8%E6%8C%87%E4%BB%A4/8553126)执行。这就是Java的能够“一次编译，到处运行”的原因。

​																		--来自《百度百科》

## JVM面试

就先不自己整理了，太费时间，贴几个链接先。。。

https://blog.csdn.net/hsk256/article/details/49104955

https://blog.csdn.net/u012257955/article/details/70890702

https://blog.csdn.net/qq_33314107/article/details/81021964



### java虚拟机的方法调用

> 方法的调用并不等于方法的执行，java的方法调用不像c++一样在编译期就知道了方法的具体内存地址，当一个方法被调用了，只是class的常量池中的符号引用被调用，而符号引用会在类加载期间或者运行期间指向真正的方法的内存地址，这就给java带来了强大的动态扩展能力。

- 1.解析：所有方法调用都是Class文件中的一个符号引用，在Class加载的阶段，有一些符号引用会转化成直接引用(内存中具体方法的地址),这个解析成功的前提就是在运行过程中这个方法是不能被改变的。这类运行期不变的方法调用被称为解析。 

  - 1.私有方法和静态方法就是这类方法，前者不可被外部调用，后者与类直接相关。所以会在类加载阶段被解析。
  - 2.invokestatic和invokespecial这两条字节码指令就与上面的俩者对应，这些方法统称为非虚方法。此外还有一种被final的方法，因为这种方法不能被覆盖所以虽然其是由invokevirtual指令来调用，但是其依然是非虚方法。

  2.分派：

