

```java
Go is a statically strongly typed, compiled, concurrent, 
	and garbage collected programming language developed by Google
【优点】
①Go的性能很高。其性能与 C++相似。Go比Java快。比Python快很多，快几十倍。
②因为go很流行。所以有很多Library。
③Go有goroutines 和channel。Goroutines 是 Go 面向Thread的轻量级方法，而channel是 goroutines 之间通信的优先方式。
创建 Goroutines 的成本很低，只需几千个字节的额外内存，正由于此，才使得同时运行数百个甚至数千个 goroutines 成为可能。你可以借助channel实现 goroutines 之间的通信。
GoLang使用goroutine来实现并发性，它提供了一个非常优雅的goroutine调度程序系统，可以很容易地生成数百万个goroutine。
堆栈使用也可以动态扩展/收缩，这使内存使用更加智能。这与Java线程不同，后者通常只允许创建数千个线程。
④内置运行时支持GC
虽然它的GC并不完美，但它可以满足大多数关于垃圾收集的要求。
⑤性能（机器代码）
GoLang是一种编译语言，可以编译为机器代码，
编译后的二进制文件可以直接部署到目标机器而无需额外的依赖。性能优于那些解释语言。
【缺点】
①Error messages are difficult to obtain
所有的exception都用Error来处理；
开发者不能从函数上知道下层函数可能返回哪些类型的错误，很容易丢失错误发生的范围，难以提供有效的错误信息。
②以前GO没有范型编程。但是1.8版本之后提供了范型功能。
③因为垃圾回收和自动内存分配的原因，Go 语言不适合用来开发对实时性要求很高的软件。
----------
Go is a statically strongly typed, compiled, concurrent, 
and garbage collected programming language developed by Google
【メリット】
①Goの性能が高い。その性能はC++と似ている。
  	GoはJavaより速い。Pythonよりも【はるかに】速く、数十倍速い。
②goが流行っているから。だからLibraryがたくさんあります。
③Goにはgoroutinesとchannelがあります。
  GoroutinesはGoがThread向けの軽量化方法であり、
  channelはgoroutines間の通信の優先方式である。
	Goroutinesを作成するコストは非常に低く、
  数千バイトの追加メモリしか必要としないため、
  数百、数千のgoroutinesを同時に実行することが可能になります。
  チャンネルを利用してgoroutines間の通信を実現することができます。
	GoLangはgoroutineを使用して同時性を実現し、
  非常に優雅なgoroutineスケジューラシステムを提供し、
  数百万個のgoroutineを容易に生成することができる。
	スタック使用は動的に拡張/縮小することもでき、
  メモリ使用をよりスマートにすることができます。
  これはJavaスレッドとは異なり、通常は数千のスレッドしか作成できません。
④内蔵ランタイムサポートGC
	GCは完璧ではありませんが、
  ゴミ収集に関するほとんどの要件を満たすことができます。
⑤パフォーマンス（Machine code）
GoLangはcompiled言語で、Machine codeにコンパイルできます。
コンパイルされたバイナリファイルは、
  追加の依存関係なしにターゲットマシンに直接配備できます。
  それらの解釈言語よりも性能が優れている。
【短所】
①Error messages are difficult to obtain
すべてのexceptionはErrorで処理されます。
開発者は、下層関数がどのタイプのエラーを返す可能性があるかを関数的に知ることができず、
  エラーの発生範囲を失いやすく、有効なエラー情報を提供することが困難です。
②以前はGOにはgeneric programmingがありませんでした。
  しかし、Version1.8以降はgeneric programming機能が提供されています。
③ごみ回収や自動メモリ分配のため、
  Go言語はreal time性に要求の高いソフトウェアの開発には適していない。
-----------
Go is a statically strongly typed, compiled, concurrent, 
and garbage collected programming language developed by Google
//[Advantages]
//① Go has high performance. 
  Its performance is similar to that of C++. 
  Go is faster than Java. 
  It is much faster than Python, dozens of times faster.
//② Because go is very popular. So there are many Libraries.
//③ Go includes goroutines and channels.
  Goroutines is a lightweight method of Go for Threads, 
	while channel is the preferred way of communication between goroutines.
The cost of creating goroutines is very low. 
    Only a few thousand bytes of additional memory are needed. 
    This makes it possible to run hundreds or even thousands of goroutines at the same time. 
    You can use the channel to achieve communication between goroutines.
GoLang uses goroutine to achieve concurrency. 
    It provides a very elegant goroutine scheduler system, 
			which can easily generate millions of goroutine.
Stack usage can also dynamically expand/shrink, 
	which makes memory usage more intelligent. 
This is different from Java threads, 
	which usually allow only thousands of threads to be created.
//④ Built in runtime supports GC
Although its GC is not perfect, it can meet most garbage collection requirements.
//⑤ Machine code
GoLang is a compiled language, which can be compiled into machine code,
The compiled binary file can be directly deployed to the target machine without additional dependencies. Performance is better than those interpreted languages.
//[Disadvantages]
//①Error messages are difficult to obtain
All exceptions are handled with Error;
Developers cannot know from the function which types of errors may be returned by the lower level function. 
  It is easy to lose the range of errors, 
	and it is difficult to provide effective error information.
//② There was no paradigm programming for GO before. However, 
  the paradigm function is provided after version 1.8.
③ Because of garbage collection and automatic memory allocation, 
Go language is not suitable for developing software with high real-time requirements.
```



- https://www.techug.com/post/bad-and-good-of-golang/
- https://studygolang.com/articles/12907
- https://zhuanlan.zhihu.com/p/114168471
- https://zhuanlan.zhihu.com/p/68483743
- https://www.cnblogs.com/beatleC/p/16128315.html
- https://blog.csdn.net/alecqie/article/details/114176682
- 