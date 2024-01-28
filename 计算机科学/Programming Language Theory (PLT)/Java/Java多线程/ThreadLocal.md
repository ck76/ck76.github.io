[TOC]

![ThreadLocal](http://r.photo.store.qq.com/psb?/V14L47VC0w3vOf/hIiBWcx3ReRZYfMzFCR4R43gf5NK74K51k7mnHHyk48!/r/dMMAAAAAAAAA)

---

#### ThreadLocalMap的问题—remove()方法

由于ThreadLocalMap的key是弱引用，而Value是强引用。这就导致了一个问题，ThreadLocal在没有外部对象强引用时，**发生GC时弱引用Key会被回收，而Value不会回收**，如果创建ThreadLocal的线程一直持续运行，那么这个Entry对象中的value就有可能一直得不到回收，发生内存泄露。

**如何避免泄漏**
既然Key是弱引用，那么我们要做的事，就是在调用ThreadLocal的get()、set()方法时完成后再调用remove方法，将Entry节点和Map的引用关系移除，这样整个Entry对象在GC Roots分析后就变成不可达了，下次GC的时候就可以被回收。

如果使用ThreadLocal的set方法之后，没有显示的调用remove方法，就有可能发生内存泄露，所以养成良好的编程习惯十分重要，**使用完ThreadLocal之后，记得调用remove方法。**



- [讲得非常好！！！](https://www.jianshu.com/p/98b68c97df9b)