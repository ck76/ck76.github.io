

```c
foo(int x, int y){
  x++;
}
  
foo(5,3); 
const int temp0 = 5;
const int temp1 = 3;
temp0 --> x = temp0;
temp1 --> y = temp1;

--------------
foo(int x, int y){
  x++;
}
int x1 = 5;
int y1 = 3;
foo(x1,y1);

x = x1;
y = y1;

--------------
foo(int& x, int& y);
foo(int* x,int* y);
```

- 程序的内建变脸，任何程序都有内建(int,char,bool)和外键变量(用户自定义类型struct....)

```c
foo(Person p){
  p.name="ck"
}

foo(p);
//Java引用【共享】
	//多线程的并发问题
  //野引用，悬挂引用（多线程的时候一个线程把引用对象释放的话）
/**如果想在Java中玩拷贝，会涉及到Java的一个设计模式Prototype -> 给每个对象一个clone()的方法
但是Java如何解决【野引用】的问题，通过引用计数，在类之间共享的一个计数(引用计数也有【多线程】的问题)
**/
------------
//c/c++传值【拷贝】
	//内存出现多份数据，拷贝时间空间的牺牲
	//深度拷贝
/*
所以c++中有拷贝构造。
在c语言中没有，全都要手写，放入锅struct嵌套层数深的话写起来还是很……
*/
Person{
  char* name;
  Person[] children;
}
```

- 所以我们到底是拷贝还是传引用

  - 传副本是没有副作用，多线程安全，因为复制的，原数据不会被修改
  - 但是共享就会出现并发问题
  - C++又发明了一个东西叫移动move

  ```c++
  sum = x + y + z;
  			-----
        temp0 + z
        ---------
          temp1(不同的编译器)
  sum = temp1;
  
  ----------------------------
  foo(){
    int result;
    return result;
  }
  //result在函数执行完成之后就被销毁
  //在return一瞬间会创建一个temp0；
  x = foo();
     temp0 <-- result//把result存在外部函数的temp0变量当中
  //那么问题又来了。对于内件类型是没有任何问题的，但是对于用户定义类型会怎样？
       
  Person foo(){
       Person p;
       ....
       return p;
  }
  //【此时也】在return一瞬间会创建一个temp0；不过这次的temp0是一个对象 
  x = foo();
     temp0 <-- result
  此时如果再是执行拷贝的话，p会复制一次到temp0；temp0在复制一次到x，还要释放temp0和p
  然后c++就发明了一个指令叫move，把内存移过来
       
  ----------
  Person& foo(){
       Person p;
       ....
       return p;
  }
  //不能这样写，这样写是引用了栈内的对象，函数执行完毕对象被释放，
  Person* foo(){
       Person p = new Person();
       ....
       return p;
  }
  //如果是这样的话，谁来释放这个内存，new在堆上，所有的函数都可以访问
  //Java操作引用计数比这个move好很多了
  ```

---

