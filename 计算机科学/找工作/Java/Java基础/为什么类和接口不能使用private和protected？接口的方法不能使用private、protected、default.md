对于java程序员来说，java的访问权限修饰词public、protected、default、private的区别和使用肯定都不是问题，这里也不再啰嗦了，反正度娘一搜就一大把。最近在整理java core的培训资料时，想到了几个关于权限修饰词的问题：

1. 为什么类和接口（不考虑内部类和接口）不能使用private和protected修饰词，只能是public和default？
2. 为什么接口的方法不能是private和protected，只能public（default可以写出来，但是编译之后自动转为了public）？

仔细想了一下，原因也不难。主要是虽然做了java不短的时间，这个问题还真没认真想过，实在惭愧。记下自己的想法，要多思考，不要成了框架的搬运工。　

## 第一个问题：　

**private：**

　　很好理解，类和接口如果定义成private，那么其他任何类都不能访问，这样的类写出来也没有意义。

**protected：**

　　这个问题用反证法比较好解释清楚，假设类B和A不在同一个包，A又是protected类， 那么B能访问A的前提是B是A的子类，而B能成为A的子类（或者说B能继承A）的前提又是B能访问A。这两个条件互为前提，无法实现。

 

## 第二个问题：

**private：**

　　接口是需要其他类实现的, 如果方法定义成private，那么其他任何类都不能访问。这样的方法即要求被实现，又对任何类不可见，这是无法实现的。

**protected：**

　　（1）假设public接口I有一个protected方法M，那么位于其他包的public类C就可以实现这个接口（方法M依然是protected）。那么C的同包类D调用方法M只能这样访问：

```
C c = ``new` `C();``c.M();
```

　　无法这样访问：

```
I c = ``new` `C();``c.M();
```

　　这样就失去了使用接口的重要意义：提供统一的接口，面向接口编程思想也无法体现。

　　（2）假设接口I是default的, 那么方法M是protected理论上是没有问题的，而且M也可以是default、public。至于为什么不让用protected，可能是出于简化修饰词的复杂度的目的（如果使用，需要区分接口是public的时候不能用，default接口则能用），至于default请参考（3）.

**default:**

　　（3）假设public接口I有一个default方法M， 那么位于其他包的无法正常实现接口I，因为方法M对其不可见。

　　（4）假设default接口I有一个default方法M，那么方法M是default理论上是没有问题的，而且M也可以是proteced、public。可能出于和（2）所说不能用protected的相同原因，不能使用default。

　　因此，综合以上四点，接口方法只能使用public，既然只能用public，java编译器在你忘了在M前写public的时候干脆自动帮你转成public了。