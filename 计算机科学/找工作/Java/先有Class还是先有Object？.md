简短答案：“鸡・蛋”问题通常都是通过一种叫“自举”（**bootstrap**）的过程来解决的。

其实“鸡蛋问题”的根本矛盾就在于假定了“鸡”或“蛋”的其中一个要先进入“完全可用”的状态。而许多现实中被简化为“鸡蛋问题”的情况实际可以在“混沌”中把“鸡”和“蛋”都初始化好，而不存在先后问题；在它们初始化的过程中，两者都不处于“完全可用”状态，而完成初始化后它们就同时都进入了可用状态。

打个比方，番茄炒蛋。并不是要先把番茄完全炒好，然后把鸡蛋完全炒好，然后把它们混起来；而是先炒番茄炒到半熟，再炒鸡蛋炒到半熟，然后把两个半熟的部分混在一起同时炒熟。

引用题主的问题：

> Java的对象模型中：
>
> 1. **所有的类都是Class类的实例，Object是类，那么Object也是Class类的一个实例。**
> 2. 所有的类都最终继承自Object类，Class是类，那么Class也继承自Object。

这个问题中，第1个假设是错的：java.lang.Object是一个Java类，但并不是java.lang.Class的一个实例。后者只是一个用于描述Java类与接口的、用于支持反射操作的类型。这点上Java跟其它一些更纯粹的面向对象语言（例如Python和Ruby）不同。
而第2个假设是对的：java.lang.Class是java.lang.Object的派生类，前者继承自后者。

虽然第1个假设不对，但“鸡蛋问题”仍然存在：在一个已经启动完毕、可以使用的Java对象系统里，必须要有一个java.lang.Class实例对应java.lang.Object这个类；而java.lang.Class是java.lang.Object的派生类，按“一般思维”前者应该要在后者完成初始化之后才可以初始化…

事实是：这些相互依赖的核心类型完全可以在“混沌”中一口气都初始化好，然后对象系统的状态才叫做完成了“bootstrap”，后面就可以按照Java对象系统的一般规则去运行。JVM、JavaScript、Python、Ruby等的运行时都有这样的bootstrap过程。

在“混沌”（boostrap过程）里，

- JVM可以为对象系统中最重要的一些核心类型先分配好内存空间，让它们进入[**已分配空间**]但[**尚未完全初始化**]状态。此时这些对象虽然已经分配了空间，但因为状态还不完整所以尚不可使用。
- 然后，通过这些分配好的空间把这些核心类型之间的引用关系串好。到此为止所有动作都由JVM完成，尚未执行任何Java字节码。
- 然后这些核心类型就进入了[**完全初始化**]状态，对象系统就可以开始自我运行下去，也就是可以开始执行Java字节码来进一步完成Java系统的初始化了。


在HotSpot VM里，有一个叫做“Universe”的C++类用于记录对象系统的总体状态。它有这么两个有趣的字段记录当前是处于bootstrapping阶段还是已经完全初始化好：
[jdk8u/jdk8u/hotspot: ade5be2b1758 src/share/vm/memory/universe.hpp](https://link.zhihu.com/?target=http%3A//hg.openjdk.java.net/jdk8u/jdk8u/hotspot/file/ade5be2b1758/src/share/vm/memory/universe.hpp%23l399)

```text
  static bool is_bootstrapping()                      { return _bootstrapping; }
  static bool is_fully_initialized()                  { return _fully_initialized; }
```

然后Universe::genesis()函数会在bootstrap阶段中创建核心类型的对象模型：
[jdk8u/jdk8u/hotspot: ade5be2b1758 src/share/vm/memory/universe.cpp](https://link.zhihu.com/?target=http%3A//hg.openjdk.java.net/jdk8u/jdk8u/hotspot/file/ade5be2b1758/src/share/vm/memory/universe.cpp%23l259)
（“genesis”是创世纪的意思，多么形象）
其中会调用SystemDictionary::initialize()来初始化对象系统的核心类型：
[jdk8u/jdk8u/hotspot: ade5be2b1758 src/share/vm/classfile/systemDictionary.cpp](https://link.zhihu.com/?target=http%3A//hg.openjdk.java.net/jdk8u/jdk8u/hotspot/file/ade5be2b1758/src/share/vm/classfile/systemDictionary.cpp%23l1814)
其中会进一步跑到SystemDictionary::initialize_preloaded_classes()来创建java.lang.Object、java.lang.Class等核心类型：
[jdk8u/jdk8u/hotspot: ade5be2b1758 src/share/vm/classfile/systemDictionary.cpp](https://link.zhihu.com/?target=http%3A//hg.openjdk.java.net/jdk8u/jdk8u/hotspot/file/ade5be2b1758/src/share/vm/classfile/systemDictionary.cpp%23l1875)
这个函数在加载了java.lang.Object、java.lang.Class等核心类型后会调用Universe::fixup_mirrors()来完成前面说的“把引用关系串起来”的动作：

```cpp
  // Fixup mirrors for classes loaded before java.lang.Class.
  // These calls iterate over the objects currently in the perm gen
  // so calling them at this point is matters (not before when there
  // are fewer objects and not later after there are more objects
  // in the perm gen.
  Universe::initialize_basic_type_mirrors(CHECK);
  Universe::fixup_mirrors(CHECK);
```

[jdk8u/jdk8u/hotspot: ade5be2b1758 src/share/vm/memory/universe.cpp](https://link.zhihu.com/?target=http%3A//hg.openjdk.java.net/jdk8u/jdk8u/hotspot/file/ade5be2b1758/src/share/vm/memory/universe.cpp%23l485)

```cpp
void Universe::fixup_mirrors(TRAPS) {
  // Bootstrap problem: all classes gets a mirror (java.lang.Class instance) assigned eagerly,
  // but we cannot do that for classes created before java.lang.Class is loaded. Here we simply
  // walk over permanent objects created so far (mostly classes) and fixup their mirrors. Note
  // that the number of objects allocated at this point is very small.

  // ...
}
```

就是这样。

=======================================================

Python里的对象系统里也有这种“鸡蛋问题”，也是通过一个bootstrap过程来解决。
“鸡蛋问题”在于：Python里的所有类型都确实用一个type object表示，而所有类型都是object类的子类。
换句话说，<type 'type'>类是<type 'object'>的子类；而<type 'object'>既是类又是个对象，是<type 'type'>的实例。这个情况就跟题主原本所想像的Java里的情况一样——虽然Java并非如此。
关于CPython 2.5的对象系统初始化的剖析，可以参考[《Python源码剖析》](https://link.zhihu.com/?target=http%3A//book.douban.com/subject/3117898/)的第12章。
具体到CPython 2.7.x的代码，[pythonrun.c的Py_InitializeEx()](https://link.zhihu.com/?target=https%3A//github.com/python/cpython/blob/2.7/Python/pythonrun.c%23L140)会做Python运行时的初始化，其中会调用[object.c的_Py_ReadyTypes()](https://link.zhihu.com/?target=https%3A//github.com/python/cpython/blob/2.7/Objects/object.c%23L2068)来按照一个列表的顺序初始化核心类型的type对象，具体的初始化动作在[typeobject.c的PyType_Ready()](https://link.zhihu.com/?target=https%3A//github.com/python/cpython/blob/2.7/Objects/typeobject.c%23L3968)。

这些核心类型的type对象在CPython里的C层面的类型是PyTypeObject，其结构是确定的；它们的存储空间通过静态变量分配，例如<type 'type'>就声明为在[object.h的PyTypeObject PyType_Type](https://link.zhihu.com/?target=https%3A//github.com/python/cpython/blob/2.7/Include/object.h%23L441)，对应的还有<type 'object'>的PyTypeObject PyBaseObject_Type。
所以在进行初始化动作之前它们的存储空间就已经有着落了，真正做初始化时只要把它们的相互引用串起来就好。

=======================================================

Ruby里的鸡蛋问题跟Python比较相似。Ruby里的所有类都是Class类的实例，而Class类是Object类的子类。

以CRuby 2.2.1为例，核心类型的初始化在这里：[class.c的Init_class_hierarchy()](https://link.zhihu.com/?target=https%3A//github.com/ruby/ruby/blob/v2_2_1/class.c%23L511)，可以看到也是典型的bootstrap过程：先分配空间，再把相互引用关系串起来，然后完成bootstrap开始进入正常的对象系统运作。

```c
void
Init_class_hierarchy(void)
{
    /* 给核心类型的Class对象实例分配空间并串上它们的继承关系 */
    rb_cBasicObject = boot_defclass("BasicObject", 0);
    rb_cObject = boot_defclass("Object", rb_cBasicObject);
    rb_cModule = boot_defclass("Module", rb_cObject);
    rb_cClass =  boot_defclass("Class",  rb_cModule);

    rb_const_set(rb_cObject, rb_intern_const("BasicObject"), rb_cBasicObject);

    /* 让上面创建的Class对象实例的类型信息（klass字段）指向Class对象 */
    RBASIC_SET_CLASS(rb_cClass, rb_cClass);
    RBASIC_SET_CLASS(rb_cModule, rb_cClass);
    RBASIC_SET_CLASS(rb_cObject, rb_cClass);
    RBASIC_SET_CLASS(rb_cBasicObject, rb_cClass);
}
```

然后看调用它的上层函数：

```c
/*!
 * Initializes the world of objects and classes.
 *
 * At first, the function bootstraps the class hierarchy.
 * It initializes the most fundamental classes and their metaclasses.
 * - \c BasicObject
 * - \c Object
 * - \c Module
 * - \c Class
 * After the bootstrap step, the class hierarchy becomes as the following
 * diagram.
 *
 * \image html boottime-classes.png
 *
 * Then, the function defines classes, modules and methods as usual.
 * \ingroup class
 */

/* ... */
void
Init_Object(void)
{
    Init_class_hierarchy();

    /* ... */
}
```