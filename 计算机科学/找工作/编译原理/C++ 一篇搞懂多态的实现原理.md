https://zhuanlan.zhihu.com/p/104605966

## 虚函数和多态

## 01 虚函数

- 在类的定义中，前面有 `virtual` 关键字的成员函数称为虚函数；
- `virtual` 关键字只用在类定义里的函数声明中，写函数体时不用。

```cpp
class Base 
{
    virtual int Fun() ; // 虚函数
};

int Base::Fun() // virtual 字段不用在函数体时定义
{ }
```

## 02 多态的表现形式一

- 「派生类的指针」可以赋给「基类指针」；
- 通过基类指针调用基类和派生类中的同名「虚函数」时:

1. 若该指针指向一个基类的对象，那么被调用是 基类的虚函数；
2. 若该指针指向一个派生类的对象，那么被调用 的是派生类的虚函数。

这种机制就叫做“多态”，说白点就是**调用哪个虚函数，取决于指针对象指向哪种类型的对象**。

```cpp
// 基类
class CFather 
{
public:
    virtual void Fun() { } // 虚函数
};

// 派生类
class CSon : public CFather 
{ 
public :
    virtual void Fun() { }
};

int main() 
{
    CSon son;
    CFather *p = &son;
    p->Fun(); //调用哪个虚函数取决于 p 指向哪种类型的对象
    return 0;
}
```

上例子中的 `p` 指针对象指向的是 `CSon` 类对象，所以 `p->Fun()` 调用的是 `CSon` 类里的 `Fun` 成员函数。

## 03 多态的表现形式二

- 派生类的对象可以赋给基类「引用」
- 通过基类引用调用基类和派生类中的同名「虚函数」时:

1. 若该引用引用的是一个基类的对象，那么被调 用是基类的虚函数；
2. 若该引用引用的是一个派生类的对象，那么被 调用的是派生类的虚函数。

这种机制也叫做“多态”，说白点就是**调用哪个虚函数，取决于引用的对象是哪种类型的对象**。

```cpp
// 基类
class CFather 
{
public:
    virtual void Fun() { } // 虚函数
};

// 派生类
class CSon : public CFather 
{ 
public :
    virtual void Fun() { }
};

int main() 
{
    CSon son;
    CFather &r = son;
    r.Fun(); //调用哪个虚函数取决于 r 引用哪种类型的对象
    return 0;
}
}
```

上例子中的 `r` 引用的对象是 `CSon` 类对象，所以 `r.Fun()` 调用的是 `CSon` 类里的 `Fun` 成员函数。

## 04 多态的简单示例

```cpp
class A 
{
public :
    virtual void Print() { cout << "A::Print"<<endl ; }
};

// 继承A类
class B: public A 
{
public :
    virtual void Print() { cout << "B::Print" <<endl; }
};

// 继承A类
class D: public A 
{
public:
    virtual void Print() { cout << "D::Print" << endl ; }
};

// 继承B类
class E: public B 
{
    virtual void Print() { cout << "E::Print" << endl ; }
};
```

A类、B类、E类、D类的关系如下图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli211hyxkj307x0600sk.jpg)

```cpp
int main() 
{
    A a; B b; E e; D d;
    
    A * pa = &a; 
    B * pb = &b;
    D * pd = &d; 
    E * pe = &e;
    
    pa->Print();  // a.Print()被调用，输出：A::Print
    
    pa = pb;
    pa -> Print(); // b.Print()被调用，输出：B::Print
    
    pa = pd;
    pa -> Print(); // d.Print()被调用，输出：D::Print
    
    pa = pe;
    pa -> Print(); // e.Print()被调用，输出：E::Print
    
    return 0;
}
```

## 05 多态作用

在面向对象的程序设计中使用「多态」，能够增强程序的**可扩充性**，即程序需要修改或增加功能的时候，需要**改动和增加的代码较少**。

------

## LOL 英雄联盟游戏例子

下面我们用设计 LOL 英雄联盟游戏的英雄的例子，说明多态为什么可以在修改或增加功能的时候，可以较少的改动代码。

LOL 英雄联盟是 5v5 竞技游戏，游戏中有很多英雄，每种英雄都有一个「类」与之对应，每个英雄就是一个「对象」。

英雄之间能够互相攻击，攻击敌人和被攻击时都有相应的动作，动作是通过对象的成员函数实现的。

下面挑了五个英雄：

- 探险家 CEzreal
- 盖楼 CGaren
- 盲僧 CLeesin
- 无极剑圣 CYi
- 瑞兹 CRyze



![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli20zgzuaj30f20400t8.jpg)5个英雄类

基本思路：

1. 为每个英雄类编写 `Attack`、`FightBack` 和 `Hurted` 成员函数。

- `Attack` 函数表示攻击动作；
- `FightBack` 函数表示反击动作；
- `Hurted` 函数表示减少自身生命值，并表现受伤动作。

1. 设置基类`CHero`，每个英雄类都继承此基类

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli20zzua7j30e908l3ze.jpg)继承基类 CHero

## 02 非多态的实现方法

```cpp
// 基类
class CHero 
{
protected:  
    int m_nPower ; //代表攻击力
    int m_nLifeValue ; //代表生命值
};


// 无极剑圣类
class CYi : public CHero 
{
public:
    // 攻击盖伦的攻击函数
    void Attack(CGaren * pGaren) 
    {
        .... // 表现攻击动作的代码
        pGaren->Hurted(m_nPower);
        pGaren->FightBack(this);
    }

    // 攻击瑞兹的攻击函数
    void Attack(CRyze * pRyze) 
    {
        .... // 表现攻击动作的代码
        pRyze->Hurted(m_nPower);
        pRyze->FightBack( this);
    }
    
    // 减少自身生命值
    void Hurted(int nPower) 
    {
        ... // 表现受伤动作的代码
        m_nLifeValue -= nPower;
    }
    
    // 反击盖伦的反击函数
    void FightBack(CGaren * pGaren) 
    {
        ...．// 表现反击动作的代码
        pGaren->Hurted(m_nPower/2);
    }
    
    // 反击瑞兹的反击函数
    void FightBack(CRyze * pRyze) 
    {
        ...．// 表现反击动作的代码
        pRyze->Hurted(m_nPower/2);
    }
};
```

有 n 种英雄，`CYi` 类中就会有 n 个 `Attack` 成员函数，以及 n 个 `FightBack` 成员函数。对于其他类也如此。

如果游戏版本升级，增加了新的英雄寒冰艾希 `CAshe`，则程序改动较大。所有的类都需要增加两个成员函数:

```cpp
void Attack(CAshe * pAshe);
void FightBack(CAshe * pAshe);
```

这样工作量是非常大的！！非常的不人性，所以这种设计方式是非常的不好！

## 03 多态的实现方式

用多态的方式去实现，就能得知多态的优势了，那么上面的栗子改成多态的方式如下：

```cpp
// 基类
class CHero 
{
public:
    virtual void Attack(CHero *pHero){}
    virtual voidFightBack(CHero *pHero){}
    virtual void Hurted(int nPower){}

protected:  
    int m_nPower ; //代表攻击力
    int m_nLifeValue ; //代表生命值
};

// 派生类 CYi:
class CYi : public CHero {
public:
    // 攻击函数
    void Attack(CHero * pHero) 
    {
        .... // 表现攻击动作的代码
        pHero->Hurted(m_nPower); // 多态
        pHero->FightBack(this);  // 多态
    }
    
    // 减少自身生命值
    void Hurted(int nPower) 
    {
        ... // 表现受伤动作的代码
        m_nLifeValue -= nPower;
    }
    
    // 反击函数
    void FightBack(CHero * pHero) 
    {
        ...．// 表现反击动作的代码
        pHero->Hurted(m_nPower/2); // 多态
    }
};
```

如果增加了新的英雄寒冰艾希 `CAshe`，只需要编写新类`CAshe`，不再需要在已有的类里专门为新英雄增加：

```cpp
void Attack( CAshe * pAshe) ;
void FightBack(CAshe * pAshe) ;
```

所以已有的类可以原封不动，那么使用多态的特性新增英雄的时候，可见改动量是非常少的。



多态使用方式：

```cpp
void CYi::Attack(CHero * pHero) 
{
    pHero->Hurted(m_nPower); // 多态
    pHero->FightBack(this);  // 多态
}

CYi yi; 
CGaren garen; 
CLeesin leesin; 
CEzreal ezreal;

yi.Attack( &garen );  //(1)
yi.Attack( &leesin ); //(2)
yi.Attack( &ezreal ); //(3)
```



根据多态的规则，上面的(1)，(2)，(3)进入到 `CYi::Attack` 函数后 ，分别调用：

```text
CGaren::Hurted
CLeesin::Hurted
CEzreal::Hurted
```

------

## 多态的又一例子

出一道题考考大家，看大家是否理解到了多态的特性，下面的代码，`pBase->fun1()`输出结果是什么呢？

```cpp
class Base 
{
public:
    void fun1() 
    { 
        fun2(); 
    }
    
    virtual void fun2()  // 虚函数
    { 
        cout << "Base::fun2()" << endl; 
    }
};

class Derived : public Base 
{
public:
    virtual void fun2()  // 虚函数
    { 
        cout << "Derived:fun2()" << endl; 
    }
};

int main() 
{
    Derived d;
    Base * pBase = & d;
    pBase->fun1();
    return 0;
}
```

是不是大家觉得 `pBase` 指针对象虽然指向的是派生类对象，但是派生类里没有 `fun1` 成员函数，则就调用基类的 `fun1` 成员函数，`Base::fun1()` 里又会调用基类的 `fun2` 成员函数，所以输出结果是`Base::fun2()` ？



假设我把上面的代码转换一下， 大家还觉得输出的是 `Base::fun2()` 吗？

```text
class Base 
{
public:
    void fun1() 
    { 
        this->fun2();  // this是基类指针，fun2是虚函数，所以是多态
    }
}
```

`this` 指针的作用就是指向成员函数所作用的对象， 所以非静态成员函数中可以直接使用 this 来代表指向该函数作用的对象的指针。

`pBase` 指针对象指向的是派生类对象，派生类里没有 `fun1` 成员函数，所以就会调用基类的 `fun1` 成员函数，在`Base::fun1()` 成员函数体里执行 `this->fun2()` 时，实际上指向的是派生类对象的 `fun2` 成员函数。



所以正确的输出结果是：

```text
Derived:fun2()
```



所以我们需要注意：

**在非构造函数，非析构函数的成员函数中调用「虚函数」，是多态!!!**

------

## 构造函数和析构函数中存在多态吗？

在构造函数和析构函数中调用「虚函数」，不是多态。编译时即可确定，调用的函数是**自己的类或基类**中定义的函数，不会等到运行时才决定调用自己的还是派生类的函数。

我们看如下的代码例子，来说明：

```cpp
// 基类
class CFather 
{
public:
    virtual void hello() // 虚函数
    {
        cout<<"hello from father"<<endl; 
    }
    
    virtual void bye() // 虚函数
    {
        cout<<"bye from father"<<endl; 
    }
};

// 派生类
class CSon : public CFather
{ 
public:
    CSon() // 构造函数
    { 
        hello(); 
    }
    
    ~CSon()  // 析构函数
    { 
        bye();
    }

    virtual void hello() // 虚函数
    { 
        cout<<"hello from son"<<endl;
    }
};

int main()
{
    CSon son;
    CFather *pfather;
    pfather = & son;
    pfather->hello(); //多态
    return 0;
}
```

输出结果：

```cpp
hello from son  // 构造son对象时执行的构造函数
hello from son  // 多态
bye from father // son对象析构时，由于CSon类没有bye成员函数，所以调用了基类的bye成员函数
```

------

多态的实现原理

「多态」的关键在于通过**基类指针或引用**调用一个**虚函数**时，编译时不能确定到底调用的是基类还是派生类的函数，运行时才能确定。

我们用 `sizeof` 来运算有有虚函数的类和没虚函数的类的大小，会是什么结果呢？

```cpp
class A 
{
public:
    int i;
    virtual void Print() { } // 虚函数
};

class B
{
public:
    int n;
    void Print() { } 
};

int main() 
{
    cout << sizeof(A) << ","<< sizeof(B);
    return 0;
}
```

在64位机子，执行的结果：

```cpp
16,4
```

从上面的结果，可以发现有虚函数的类，多出了 8 个字节，在 64 位机子上指针类型大小正好是 8 个字节，这多出 8 个字节的指针有什么作用呢？

## 01 虚函数表

每一个有「虚函数」的类（或有虚函数的类的派生类）都有一个「虚函数表」，该类的任何对象中都放着**虚函数表的指针**。「虚函数表」中列出了该类的「虚函数」地址。

**多出来的 8 个字节就是用来放「虚函数表」的地址。**

```cpp
// 基类
class Base 
{
public:
    int i;
    virtual void Print() { } // 虚函数
};

// 派生类
class Derived : public Base
{
public:
    int n;
    virtual void Print() { } // 虚函数
};
```

上面 Derived 类继承了 Base类，两个类都有「虚函数」，那么它「虚函数表」的形式可以理解成下图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli20u1knxj30rq0cc0vi.jpg)虚函数表指针走向图

多态的函数调用语句被编译成一系列根据基类指针所指向的（或基类引用所引用的）对象中**存放的虚函数表的地址**，在虚函数表中查找虚函数地址，并调用虚函数的指令。

## 02 证明虚函数表指针的作用

在上面我们用 `sizeof` 运算符计算了有虚函数的类的大小，发现是多出了 8 字节大小（64位系统），这多出来的 8 个字节就是指向「虚函数表的指针」。「虚函数表」中列出了该类的「虚函数」地址。

下面用代码的例子，来证明「虚函数表指针」的作用：

```cpp
// 基类
class A 
{
public: 
    virtual void Func()  // 虚函数
    { 
        cout << "A::Func" << endl; 
    }
};

// 派生类
class B : public A 
{
public: 
    virtual void Func()  // 虚函数
    { 
        cout << "B::Func" << endl;
    }
};

int main() 
{
    A a;
    
    A * pa = new B();
    pa->Func(); // 多态
    
    // 64位程序指针为8字节
    int * p1 = (int *) & a;
    int * p2 = (int *) pa;
    
    * p2 = * p1;
    pa->Func();
    
    return 0;
}
```

输出结果：

```cpp
B::Func
A::Func
```

- 第 25-26 行代码中的 `pa` 指针指向的是 `B` 类对象，所以 `pa->Func()` 调用的是 `B` 类对象的虚函数 `Func()`，输出内容是 `B::Func` ；
- 第 29-30 行代码的目的是把 `A` 类的头 8 个字节的「虚函数表指针」存放到 `p1` 指针和把 `B` 类的头 8 个字节的「虚函数表指针」存放到 `p2` 指针；
- 第 32 行代码目的是把 `A` 类的「虚函数表指针」 赋值给 `B` 类的「虚函数表指针」，所以相当于把 `B` 类的「虚函数表指针」 替换 成了 `A` 类的「虚函数表指针」；
- 由于第 32 行的作用，把 `B` 类的「虚函数表指针」 替换 成了 `A` 类的「虚函数表指针」，所以第 33 行调用的是 `A` 类的虚函数 `Func()`，输出内容是 `A::Func`

通过上述的代码和讲解，可以有效的证明了「虚函数表的指针」的作用，「虚函数表的指针」指向的是「虚函数表」，「虚函数表」里存放的是类里的「虚函数」地址，那么在调用过程中，就能实现多态的特性。

------

## 虚析构函数

析构函数是在删除对象或退出程序的时候，自动调用的函数，其目的是做一些资源释放。

那么在多态的情景下，通过基类的指针删除派生类对象时，通常情况下只调用基类的析构函数，这就会存在派生类对象的析构函数没有调用到，存在资源泄露的情况。

看如下的例子：

```cpp
// 基类
class A 
{
public: 
    A()  // 构造函数
    {
        cout << "construct A" << endl;
    }
    
    ~A() // 析构函数
    {
        cout << "Destructor A" << endl;
    }
};

// 派生类
class B : public A 
{
public: 
    B()  // 构造函数
    {
        cout << "construct B" << endl;
    }
    
    ~B()// 析构函数
    {
        cout << "Destructor B" << endl;
    }
};

int main() 
{
    A *pa = new B();
    delete pa;
    
    return 0;
}
```

输出结果：

```cpp
construct A
construct B
Destructor A
```

从上面的输出结果可以看到，在删除 `pa`指针对象时，`B` 类的析构函数没有被调用。



**解决办法：把基类的析构函数声明为virtual**

- 派生类的析构函数可以 virtual 不进行声明；
- 通过基类的指针删除派生类对象时，首先调用派生类的析构函数，然后调用基类的析构函数，还是遵循「先构造，后虚构」的规则。

将上述的代码中的基类的析构函数，定义成「虚析构函数」：

```cpp
// 基类
class A 
{
public: 
    A()  
    {
        cout << "construct A" << endl;
    }
    
    virtual ~A() // 虚析构函数
    {
        cout << "Destructor A" << endl;
    }
};
```

输出结果：

```cpp
construct A
construct B
Destructor B
Destructor A
```

所以要养成好习惯:

- 一个类如果定义了虚函数，则应该将析构函数也定义成虚函数;
- 或者，一个类打算作为基类使用，也应该将析构函数定义成虚函数。
- 注意：不允许构造函数不能定义成虚构造函数。

------

## 纯虚函数和抽象类

纯虚函数： 没有函数体的虚函数

```cpp
class A 
{

public:
    virtual void Print( ) = 0 ; //纯虚函数
private: 
    int a;
};
```

包含纯虚函数的类叫抽象类

- 抽象类只能作为基类来派生新类使用，不能创建抽象类的对象
- 抽象类的指针和引用可以指向由抽象类派生出来的类的对象

```cpp
A a;         // 错，A 是抽象类，不能创建对象
A * pa ;     // ok,可以定义抽象类的指针和引用
pa = new A ; // 错误, A 是抽象类，不能创建对象
```



---



- https://www.csdn.net/tags/MtjaMg2sODU4NjQtYmxvZwO0O0OO0O0O.html

---

**https://www.cnblogs.com/xgmzhna/p/10934562.html**

## C++语言虚函数实现多态的原理

 自上一个帖子之间跳过了一篇总结性的帖子，之后再发，今天主要研究了c++语言当中虚函数对多态的实现，感叹于c++设计者的精妙绝伦

**c++中虚函数表的作用主要是实现了多态的机制。首先先解释一下多态的概念，多态是c++的特点之一，关于多态，简而言之就是 用父类的指针指向其子类的实例，然后通过父类的指针调用实际子类的成员函数，这种方法呢，可以让父类的指针具有多种形态，也就是说不需要改动很多的代码就可以让父类这一种指针，干一些很多子类指针的事情，这里是从虚函数的实现机制层面进行研究**

在写这篇帖子之前对于相关的文章进行了查阅，基本上是大段的文字，所以我的这一篇可能会用大量的图形进行赘述（如果理解有误的地方，烦请大佬能够指出），接下来就言归正传：

首先介绍一下为什么会引进多态呢，基于c++的复用性和拓展性而言，同类的程序模块进行大量重复，是一件无法容忍的事情，比如我设置了苹果，香蕉，西瓜类，现在想把这些东西都装到碗这个函数里，那么在主函数当中，声明对象是必须的，但是每一次装进碗里对于水果来说，都要用自己的指针调用一次装的功能，那为什么不把这些类抽象成一个水果类呢，直接定义一个水果类的指针一次性调用所有水果装的功能呢，这个就是利用**父类指针去调用子类成员**，但是这个思想受到了指针指向类型的限制，也就是说表面指针指向了子类成员，但实际上还是只能调用子类成员里的父类成员，这样的思想就变的毫无意义了，如果想要解决这个问题，只要在父类前加上virtual就可以解决了，这里就是利用虚函数实现多态的实例。

首先还是作为举例来两个类，在之前基础知识的帖子中提到过，空类的大小是一个字节（占位符），函数，静态变量都在编译期就形成了，不用类去分配空间，但是做一个小实验，看一看在定义了虚函数之后，类的大小是多少呢

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```
 1 #include<iostream>
 2 using namespace std;
 3 class CFather 
 4 {
 5 public:
 6     virtual void AA()　　//虚函数标识符
 7     {
 8         cout << "CFather :: AA()" << endl;
 9     }
10     void BB()
11     {
12         cout << "CFather  :: BB()" << endl;
13     }
14 };
15 class CSon : public CFather
16 {
17 public:
18     void AA()
19     {
20         cout << "CSon :: AA()" << endl;
21     }
22     void BB()
23     {
24         cout << "CSon :: BB()" << endl;
25     }
26 };
27 int main()
28 {
29     cout << sizeof(CFather) << endl;     　　　　　　//测试加了虚函数的类
30 
31     system("pause");
32     return 0;
33 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli247aftfj306201qdfm.jpg)

很明显类里装了一个 4个字节的东西，除了整形int，就是指针了，没错这里装的就是**函数指针**

先把这个代码，给抽象成图形进行理解，在这CFather为A，CSon为B

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli244ikywj30ol0kbdfw.jpg)

 

 此时就是一个单纯的继承的情况，不存在虚函数，然后我new一个对象，A *p = new A；那么 p -> AA(),必然是指向A类中的AA()函数，那么函数的调用有两种方式 一种函数名加（）直接调用，一种是利用函数指针进行调用，在这里我想要调用子类的，就可以利用函数指针进行调用，假设出来两个函数指针，来指向B类中的两个成员函数，如果我父类想要调用子类成员，就可以通过 p指针去调用函数指针，再通过函数指针去调用成员函数

，![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli242lwhhj30tq0kzjrp.jpg)

 

 每一个函数都可以用一个函数指针去指着，那么每一类中的函数指针都可以形成自己的一个表，这个就叫做虚函数表

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli24165wjj30zk0nudgk.jpg)

 

 那么在创建对象后，为什么类中会有四个字节的内存空间呢？

在C++的标准规格说明书中说到，编译器必需要保证虚函数表的指针存在于对象中最前面的位置（这是为了保证正确取到虚函数的偏移量）。这意味着我们通过对象实例的地址得到这张虚函数表，然后就可以遍历其中函数指针，并调用相应的函数。也就是说这四个字节的指针，代替了上图中（p->*pfn）（）的作用，指向了函数指针，也就是说，在使用了虚函数的父类成员函数，**虽然写的还是p->AA(),实际上却是，(p->\*(vfptr[0]))**,而指向哪个虚函数表就由，创建的对象来决定

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli23z77knj30uf0mamxt.jpg)

至此，就能理解如何用虚函数这个机制来实现多态的了

下面，我将分别说明“无覆盖”和“有覆盖”时的虚函数表的样子。没有覆盖父类的虚函数是毫无意义的。我之所以要讲述没有覆盖的情况，主要目的是为了给一个对比。在比较之下，我们可以更加清楚地知道其内部的具体实现。

 

无虚数覆盖

下面，再让我们来看看继承时的虚函数表是什么样的。假设有如下所示的一个继承关系：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli23xdca8j302y075jsb.jpg)

请注意，在这个继承关系中，子类没有重载任何父类的函数。那么，在派生类的实例中，Derive d; 的虚函表：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli23wkgb9j30ln0460te.jpg)

我们可以看到下面几点：

1）虚函数按照其声明顺序放于表中。

2）父类的虚函数在子类的虚函数前面。

 

有虚数覆盖

覆盖父类的虚函数是很显然的事情，不然，虚函数就变得毫无意义。下面，我们来看一下，如果子类中有虚函数重载了父类的虚函数，会是一个什么样子？假设，我们有下面这样的一个继承关系。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli23ukempj303507dq3v.jpg)

为了让大家看到被继承过后的效果，在这个类的设计中，我只覆盖了父类的一个函数：f()。那么，对于派生类的实例，其虚函数表会是下面的一个样子：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli23tmuo6j30hr04t750.jpg)

我们从表中可以看到下面几点，

1）覆盖的f()函数被放到了虚表中原来父类虚函数的位置。

2）没有被覆盖的函数依旧。

这样，我们就可以看到对于下面这样的程序，

​      Base *b = new Derive();

​      b->f();

由b所指的内存中的虚函数表的f()的位置已经被Derive::f()函数地址所取代，于是在实际调用发生时，是Derive::f()被调用了。这就实现了多态。

 

 

2019-05-28 00:15:30 编程小菜鸟自我反思，今天图画的太丑了，大家多多担待，如果技术上有什么偏差，大家可以踊跃批评我，谢谢！！！

 /*==========================================================手动分割线================================================*/

感谢@奕韵风华提出的问题，现在将多继承的虚函数实现多态的情况讨论一下，另再加上从代码层面上对这个机制有更深的理解

讨论多继承还是从有无虚函数覆盖的情况来开始

无虚函数覆盖

假设有下面这样一个类的继承关系。注意：子类并没有覆盖父类的函数。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli23rr0p4j30at076wgo.jpg)

对于子类实例中的虚函数表，是下面这个样子：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli23qdq5ij30ig06976e.jpg)

我们可以看到：

1） 每个父类都有自己的虚表。

2） 子类的成员函数被放到了第一个父类的表中。（所谓的第一个父类是按照声明顺序来判断的）

这样做就是为了解决不同的父类类型的指针指向同一个子类实例，而能够调用到实际的函数。

 

有虚函数覆盖

下图中，我们在子类中覆盖了父类的f()函数。

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli23okqanj30ce07ignt.jpg)

下面是对于子类实例中的虚函数表的图：

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli23n4pprj30fl06476p.jpg)

我们可以看见，三个父类虚函数表中的f()的位置被替换成了子类的函数指针。这样，我们就可以任一静态类型的父类来指向子类，并调用子类的f()了。比如

 

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```c
 1  Derive d;
 2 
 3   Base1 *b1 = &d;
 4 
 5   Base2 *b2 = &d;
 6 
 7   Base3 *b3 = &d;
 8 
 9   b1->f(); //Derive::f()
10 
11   b2->f(); //Derive::f()
12 
13   b3->f(); //Derive::f()
14 
15  
16 
17   b1->g(); //Base1::g()
18 
19   b2->g(); //Base2::g()
20 
21   b3->g(); //Base3::g()
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

 

 

以上就是对多继承情况的一种讨论

那么再看看如何在代码的层面上来验证原理呢？

首先在主函数内声明了一个父类的指针，指向子类对象，那么这个对这个父类指针解引用的话，就能得到一个vfptr指针，和父类，子类对象，但是现在我只需要指向虚函数的指针，那么就可以定义指针只取前四个字节，利用强转，*（int *）p 这个得到了vfptr的地址，那么继续想要获得虚函数表中的虚函数，就是再次解引用了，但是如何进行偏移呢？在整形，浮点型里，指针的偏移量都是指针指向类型所觉得的，而这里是个函数指针，函数指针不允许利用指针指向类型来进行偏移量的取值，因为函数的类型大小是不确定的，但是我们知道，虚函数表里都是函数指针，指针的大小是确定的都是四个字节，那还可以继续利用强转，控制指针每次偏移四个字节，那么这个时候再进行解引用就是我们所取得函数的地址了，如果语言太赘述的话，可以看下面的例子

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

```c
 1 //--------------------------------------------------
 2 #include <iostream>
 3 using namespace std;
 4 
 5 
 6 class CFather
 7 {
 8 public:
 9     virtual void AA()
10     {
11         cout << "CFather::AA" << endl;
12     }
13     virtual void BB()
14     {
15         cout << "CFather::BB" << endl;
16     }
17     virtual void CC()
18     {
19         cout << "CFather::BB" << endl;
20     }
21     void DD()
22     {
23         cout << "CFather::DD" << endl;
24     }
25 };
26 
27 class CSon:public CFather
28 {
29 public:
30     virtual void AA()
31     {
32         cout << "CSon::AA" << endl;
33     }
34     virtual void BB()
35     {
36         cout << "CSon::BB" << endl;
37     }
38     void DD()
39     {
40         cout << "CSon::DD" << endl;
41     }
42     virtual void EE()
43     {
44         cout << "CSon::EE" << endl;
45     }
46 };
47 
48 
49 int main()
50 {
51 
52     typedef void (*PFUN)();
53 
54     cout << sizeof(CFather) << endl;
55     CFather* p = new CSon;
56     PFUN aa = (PFUN)*((int*)*(int*)p+0);
57     PFUN bb = (PFUN)*((int*)*(int*)p+1);
58     PFUN cc = (PFUN)*((int*)*(int*)p+2);
59     PFUN dd = (PFUN)*((int*)*(int*)p+3);
60     PFUN ee = (PFUN)*((int*)*(int*)p+4);
61 
62 
63 
64     system("pause");
65     return 0;
66 }
```

[![复制代码](https://common.cnblogs.com/images/copycode.gif)](javascript:void(0);)

通过监视就能直接看到（因为vs编译器不允许，利用父类指针直接使用虚函数不覆盖情况下的子类成员函数，利用这个方法也可以查看子类虚函数）

![img](https://tva1.sinaimg.cn/large/0081Kckwly1gli23kqjucj31020613ym.jpg)

验证了我上面叙述的原理，首先父类中先 生成了虚函数表，再继承到子类当中，如果子类中有重载的函数，直接重写，没有的话直接添加