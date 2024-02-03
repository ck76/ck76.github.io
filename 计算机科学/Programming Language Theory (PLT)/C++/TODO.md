



- https://roadmap.sh/cpp



在面向对象编程（OOP）中，多态性是一个核心概念，它允许不同类的对象以各自的方式响应相同的消息。下面是多态性在C++中的几个相关概念的比较：

| 概念                     | 描述                                               | 示例或用法                                                   |
| ------------------------ | -------------------------------------------------- | ------------------------------------------------------------ |
| Overloading of Functions | 函数重载，允许创建多个同名函数，参数类型或数量不同 | `void func(int i);`<br>`void func(double d);`<br>`void func(int i, double d);` |
| Static Polymorphism      | 编译时多态，通过函数重载和模板实现                 | `template <typename T>`<br>`void func(T& obj);`              |
| Dynamic Polymorphism     | 运行时多态，通常通过虚函数实现                     | `class Base { virtual void func(); };`<br>`class Derived : public Base { void func(); };` |
| Virtual Methods          | 虚函数，允许派生类重写基类方法，实现运行时多态     | `virtual void func() = 0;` // 纯虚函数用于抽象类             |
| Virtual Tables           | 虚表，存储类的虚函数地址，用于运行时解析虚函数调用 | 不直接使用，由编译器在后台处理                               |

多态性使得C++程序可以通过指向基类的指针或引用来调用派生类的函数，从而允许接口重新定义行为，同时还能保持代码的一致性。函数重载和模板提供了静态多态性，而虚函数和虚表实现了动态多态性。这些特性是C++强大表现力的基础，它们使得C++在软件工程中特别适合复杂应用的开发。





在C++模板编程中，模板特化、变参模板、类型萃取（Type Traits）和SFINAE（Substitution Failure Is Not An Error）是高级技术，它们为泛型编程提供了强大的能力。以下是它们的对比：

| 概念                            | 描述                                                 | 示例或用法                                                   |
| ------------------------------- | ---------------------------------------------------- | ------------------------------------------------------------ |
| Full Template Specialization    | 完全特化一个模板，为特定类型提供特殊实现             | `template <>`<br>`class MyClass<int> { /* 完全特化为int类型的实现 */ };` |
| Partial Template Specialization | 部分特化一个模板，为类型的某些属性提供特殊实现       | `template <typename T, typename U>`<br>`class MyClass<T*, U*> { /* 部分特化为指针类型的实现 */ };` |
| Variadic Templates              | 变参模板，允许接受可变数量的模板参数                 | `template <typename... Args>`<br>`void func(Args... args) { /* 可以接受任意数量的参数 */ }` |
| Template Specialization         | 特化一个模板，为特定类型或条件提供实现               | `template <typename T>`<br>`void func();`<br>`template <>`<br>`void func<int>(); // 特化为int的实现` |
| Type Traits                     | 类型萃取，用于在编译时获取类型的信息                 | `std::is_integral<T>::value` // 如果T是整数类型则为true      |
| SFINAE                          | 替换失败不是错误，用于在模板实例化失败时选择其他模板 | `template <typename T>`<br>`typename std::enable_if<std::is_integral<T>::value, T>::type func(T t);` |

C++模板特化让你能够定义通用的模板，然后为某些特定的类型或条件提供特定的实现。这使得编写泛型代码时可以针对某些类型进行优化或特殊处理。变参模板通过接受任意数量的模板参数，使得函数或类模板更加灵活。

类型萃取是一系列模板，它们在编译时提供有关类型的信息，例如检查类型是否是指针、是否是整数类型等。类型萃取常用于编写依赖于类型属性的泛型代码。

SFINAE是一种技术，它允许在模板参数不匹配时消除某些重载，而不是产生编译错误。这允许开发者为不同的类型条件编写多个函数模板重载，并让编译器选择最合适的一个。

这些高级特性是C++模板编程的基石，它们提供了在编译时根据类型信息或模板参数作出决策的能力，这对于创建高效和灵活的泛型库至关重要。