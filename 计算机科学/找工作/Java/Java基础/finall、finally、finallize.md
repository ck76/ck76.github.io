```java
public class Main {

    public static void main(String[] args) {
        HH hh = new HH();
        System.out.println(hhhhhhh(hh));//11

        System.out.println(hh.i);//12
    }

    static int hhhhhhh(HH hh) {
        try {
            hh.i++;
            return hh.i;
        } catch (Exception e) {

        } finally {
            hh.i++;//代码还是会被执行的
        }
        System.out.println("return");
        return hh.i;
    }
}

class HH {
    public int i = 10;
}

//11
//12
```



**final**

修饰符（关键字）如果一个类被声明为final，意味着它不能再派生出新的子类，不能作为父类被继承。因此一个类不能既被声明为 abstract的，又被声明为final的。将变量或方法声明为final，可以保证它们在使用中不被改变。被声明为final的变量必须在声明时给定初值，而在以后的引用中只能读取，不可修改。被声明为final的方法也同样只能使用，不能重载。 

------

 

**finally**

异常处理时提供 finally 块来执行任何清除操作。如果抛出一个异常，那么相匹配的 catch 子句就会执行，然后控制就会进入 finally 块（如果有的话）。一般异常处理块需要。

------

 

**finalize**

方法名。Java 技术允许使用 finalize() 方法在垃圾收集器将对象从内存中清除出去==之前==做必要的清理工作。这个方法是由垃圾收集器在确定这个对象没有被引用时对这个对象调用的。它是在 Object 类中定义的，因此所有的类都继承了它。子类覆盖 finalize() 方法以整理系统资源或者执行其他清理工作。finalize() 方法是在垃圾收集器删除对象之前对这个对象调用的。 

Java中所有类都从Object类中继承finalize()方法。

当垃圾回收器(garbage colector)决定回收某对象时，就会运行该对象的finalize()方法。值得C++程序员注意的是，finalize()方法并==不能等同与析构函数==。Java中是没有析构函数的。C++的析构函数是在对象消亡时运行的。由于C++没有垃圾回收，对象空间手动回收，所以一旦对象用不到时，程序员就应当把它delete()掉。所以析构函数中经常做一些文件保存之类的收尾工作。但是在Java中很不幸，如果内存总是充足的，那么垃圾回收可能永远不会进行，也就是说==filalize()可能永远不被执行，显然指望它做收尾工作是靠不住的==。

那么finalize()究竟是做什么的呢？它最主要的用途是回收特殊渠道申请的内存。Java程序有垃圾回收器，所以一般情况下内存问题不用程序员操心。但有一种JNI(Java Native Interface)调用non-Java程序（C或C++），finalize()的工作就是回收这部分的内存。



> Java编程思想

finalize()方法是Object类中提供的一个方法，在GC准备释放对象所占用的内存空间之前，它将首先调用finalize()方法。其在Object中定义如下：

```
protected void finalize() throws Throwable { }
1
```

## 1 finalize()调用的时机

与C++的析构函数（对象在清除之前析构函数会被调用）不同，在Java中，由于GC的自动回收机制，因而并不能保证finalize方法会被及时地执行（垃圾对象的回收时机具有不确定性），也不能保证它们会被执行(程序由始至终都未触发垃圾回收)。

```
public class Finalizer {
	@Override
	protected void finalize() throws Throwable {
		System.out.println("Finalizer-->finalize()");
	}

	public static void main(String[] args) {
		Finalizer f = new Finalizer();
		f = null;
	}
}
//无输出
123456789101112
public class Finalizer {

	@Override
	protected void finalize() throws Throwable {
		System.out.println("Finalizer-->finalize()");
	}

	public static void main(String[] args) {
		Finalizer f = new Finalizer();
		f = null;
		
		System.gc();//手动请求gc
	}
}
//输出 Finalizer-->finalize()
123456789101112131415
```

## 2 什么时候应该使用它

finalize()方法中一般用于释放非Java 资源（如打开的文件资源、数据库连接等）,或是调用非Java方法（native方法）时分配的内存（比如C语言的malloc()系列函数）。

## 3 为什么应该避免使用它

首先，由于finalize()方法的调用时机具有不确定性，从一个对象变得不可到达开始，到finalize()方法被执行，所花费的时间这段时间是任意长的。我们并不能依赖finalize()方法能及时的回收占用的资源，可能出现的情况是在我们耗尽资源之前，gc却仍未触发，因而通常的做法是提供显示的close()方法供客户端手动调用。
另外，重写finalize()方法意味着==延长了回收对象时需要进行更多的操作，从而延长了对象回收的时间==。

## 4 让对象再活一次

利用finalize()方法最多只会被调用一次的特性，我们可以实现延长对象的生命周期。

```
class User{
	
	public static User user = null;

	@Override
	protected void finalize() throws Throwable {
		System.out.println("User-->finalize()");
		user = this;
	}
	
}

public class FinalizerTest {
	public static void main(String[] args) throws InterruptedException {
		User user = new User();
		user = null;
		System.gc();
		Thread.sleep(1000);
		
		user = User.user;
		System.out.println(user != null);//true
		
		user = null;
		System.gc();
		Thread.sleep(1000);
		System.out.println(user != null);//false
	}
}
12345678910111213141516171819202122232425262728
```