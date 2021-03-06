[TOC]

### C++继承

- 面向对象程序设计中最重要的一个概念是继承。继承允许我们依据另一个类来定义一个类，这使得创建和维护一个应用程序变得更容易。这样做，也达到了重用代码功能和提高执行效率的效果。

  当创建一个类时，您不需要重新编写新的数据成员和成员函数，只需指定新建的类继承了一个已有的类的成员即可。这个已有的类称为**基类**，新建的类称为**派生类**。

  ```c++
  // 基类
  class Shape 
  // 派生类
  class Rectangle: public Shape
  ```

- **访问控制和继承**

  派生类可以访问基类中所有的非私有成员。因此基类成员如果不想被派生类的成员函数访问，则应在基类中声明为 private。

  我们可以根据访问权限总结出不同的访问类型，如下所示：

  | 访问     | public | protected | private |
  | -------- | ------ | --------- | ------- |
  | 同一个类 | yes    | yes       | yes     |
  | 派生类   | yes    | yes       | **no**  |
  | 外部的类 | yes    | **no**    | **no**  |

  一个派生类继承了所有的基类方法，但下列情况除外：

  - 基类的构造函数、析构函数和拷贝构造函数。
  - 基类的重载运算符。
  - 基类的友元函数。

- **继承类型**

  当一个类派生自基类，该基类可以被继承为 **public、protected** 或 **private** 几种类型。继承类型是通过上面讲解的访问修饰符 access-specifier 来指定的。

  我们几乎不使用 **protected** 或 **private** 继承，通常使用 **public** 继承。当使用不同类型的继承时，遵循以下几个规则：

  - **公有继承（public）：**当一个类派生自**公有**基类时，基类的**公有**成员也是派生类的**公有**成员，基类的**保护**成员也是派生类的**保护**成员，基类的**私有**成员不能直接被派生类访问，但是可以通过调用基类的**公有**和**保护**成员来访问。
  - **保护继承（protected）：** 当一个类派生自**保护**基类时，基类的**公有**和**保护**成员将成为派生类的**保护**成员。
  - **私有继承（private）：**当一个类派生自**私有**基类时，基类的**公有**和**保护**成员将成为派生类的**私有**成员。

  | 继承方式/基类成员 | public成员 | protected成员 | private成员 |
  | ----------------- | ---------- | ------------- | ----------- |
  | public继承        | public     | protected     | 不可见      |
  | protected继承     | protected  | protected     | 不可见      |
  | private继承       | private    | private       | 不可见      |

- **多继承**

  多继承即一个子类可以有多个父类，它继承了多个父类的特性。

  C++ 类可以从多个类继承成员，语法如下：

  ```c++
  class <派生类名>:<继承方式1><基类名1>,<继承方式2><基类名2>,…
  {
  <派生类类体>
  };
  
  // 基类 Shape
  class Shape 
  // 基类 PaintCost
  class PaintCost 
  // 派生类
  class Rectangle: public Shape, public PaintCost
  ```

- 基类的成员函数可以被继承，可以通过派生类的对象访问，但这仅仅指的是普通的成员函数，类的构造函数不能被继承。构造函数不能被继承是有道理的，因为即使继承了，它的名字和派生类的名字也不一样，不能成为派生类的构造函数，当然更不能成为普通的成员函数。

- 和构造函数类似，析构函数也不能被继承。与构造函数不同的是，在派生类的析构函数中不用显式地调用基类的析构函数，因为每个类只有一个析构函数，编译器知道如何选择，无需程序员干涉。

- **虚继承(Virtual Inheritance)和虚基类**

  - 为了解决多继承时的命名冲突和冗余数据问题，[C++](http://c.biancheng.net/cplus/) 提出了虚继承，使得在派生类中只保留一份间接基类的成员。

  - 虚继承的目的是让某个类做出声明，承诺愿意共享它的基类。其中，这个被共享的基类就称为虚基类（Virtual Base Class），本例中的 A 就是一个虚基类。在这种机制下，不论虚基类在继承体系中出现了多少次，在派生类中都只包含一份虚基类的成员。
  - 必须在虚派生的真实需求出现前就已经完成虚派生的操作

  - 虚派生只影响从指定了虚基类的派生类中进一步派生出来的类，它不会影响派生类本身。

- **虚基类成员的可见性**

  - 因为在虚继承的最终派生类中只保留了一份虚基类的成员，所以该成员可以被直接访问，不会产生二义性。此外，如果虚基类的成员只被一条派生路径覆盖，那么仍然可以直接访问这个被覆盖的成员。但是如果该成员被两条或多条路径覆盖了，那就不能直接访问了，此时必须指明该成员属于哪个类。
  - 假设 A 定义了一个名为 x 的成员变量，当我们在 D 中直接访问 x 时，会有三种可能性：
    - 如果 B 和 C 中都没有 x 的定义，那么 x 将被解析为 B 的成员，此时不存在二义性。
    - 如果 B 或 C 其中的一个类定义了 x，也不会有二义性，派生类的 x 比虚基类的 x 优先级更高。
    - 如果 B 和 C 中都定义了 x，那么直接访问 x 将产生二义性问题。

  ```c++
  		A
  	  /   \
  	B		C
  	  \    /   
  		D
  ```

  