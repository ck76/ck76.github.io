

- https://roadmap.sh/cpp



<img src="https://p.ipic.vip/5g043d.png" alt="image-20240202190012154" style="zoom:33%;" />



在C++中，指针和引用是基础概念，用于间接引用其他变量。智能指针是一种实现自动内存管理的类模板。以下是这些概念的详细对比，包括使用场景和示例：

| 概念                 | 描述                                                         | 示例或用法                                                   |
| -------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| `weak_ptr`           | 一种智能指针，用于引用`shared_ptr`管理的对象，但不增加引用计数 | `std::weak_ptr<Object> wPtr = sharedPtr;`                    |
| `shared_ptr`         | 一种智能指针，多个`shared_ptr`可以共享同一个对象的所有权     | `std::shared_ptr<Object> sPtr(new Object);`                  |
| `unique_ptr`         | 一种智能指针，保证一个对象只有一个拥有者                     | `std::unique_ptr<Object> uPtr(new Object);`                  |
| Smart Pointers       | 用于自动管理内存的指针，可以防止内存泄漏                     | `std::shared_ptr<Object> ptr = std::make_shared<Object>();`  |
| Raw Pointers         | 基本的C++指针，没有自动内存管理                              | `Object* rawPtr = new Object;`                               |
| References           | 引用，一种安全的指针别名，必须初始化且不能为空               | `Object& ref = object;`                                      |
| New/delete Operators | 用于动态内存分配和释放的操作符                               | `Object* obj = new Object;`<br>`delete obj;`                 |
| Memory Leakage       | 内存泄漏，分配的内存未被释放                                 | 忘记`delete`分配的内存: `Object* obj = new Object;` // 未调用`delete` |
| Memory Model         | C++内存模型，解释对象在内存中的生命周期                      | 栈内存用于局部变量，堆内存用于动态分配的对象                 |
| Lifetime of Objects  | 对象的生命周期，从对象创建到销毁的时间                       | 局部对象随作用域结束而销毁，动态分配的对象需手动销毁或通过智能指针自动管理 |

在C++中，理解不同类型的指针和引用及其用法至关重要，因为错误的内存管理可能导致内存泄漏、悬空指针和其他未定义的行为。智能指针如`std::unique_ptr`和`std::shared_ptr`提供了自动化的内存管理，而原始指针则需要程序员手动释放内存。引用提供了一种无需担心空值的方法来间接访问对象，但它们不拥有其所引用的对象。通过正确使用这些工具，可以编写出更安全、可维护和高效的C++代码。