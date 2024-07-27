



<img src="https://p.ipic.vip/ktqhyd.png" alt="63377cde1d29466b3b87dead5d81b2ae" style="zoom:33%;" />

### 详解 Advanced Student Scheme 核心语法

图38.1展示了Advanced Student Scheme（ASS）的核心语法，包括定义（definition）和表达式（expression）两部分。这些语法元素构成了ASS的基本结构，用于构建程序。

#### 定义部分（<def>）

1. `(define (<var> <var> ... <var>) <exp>)`
   - 定义一个函数，`<var>`为函数名，后面的`<var>`为参数，`<exp>`为函数体。例如：
     ```scheme
     (define (add x y) (+ x y))
     ```
     这个例子定义了一个名为`add`的函数，接受两个参数`x`和`y`，返回它们的和。

2. `(define <var> <exp>)`
   - 定义一个变量，`<var>`为变量名，`<exp>`为变量的值。例如：
     ```scheme
     (define pi 3.14)
     ```
     这个例子定义了一个变量`pi`，值为3.14。

3. `(define-struct <var0> (<var-1> ... <var-n>))`
   - 定义一个结构体，`<var0>`为结构体名，`<var-1>`到`<var-n>`为结构体的字段。例如：
     ```scheme
     (define-struct point (x y))
     ```
     这个例子定义了一个名为`point`的结构体，有两个字段`x`和`y`。

#### 表达式部分（<exp>）

1. `<var>`
   - 变量，例如`x`。

2. `<con>`
   - 常量，例如`5`，`#t`（true），`#f`（false）。

3. `<prm>`
   - 原语，例如`+`，`-`，`*`，`/`。

4. `(<exp> <exp> ... <exp>)`
   - 函数调用，第一个`<exp>`为函数名，后面的`<exp>`为参数。例如：
     ```scheme
     (add 1 2)
     ```
     调用了`add`函数，传入参数`1`和`2`。

5. `(cond (<exp> <exp>) ... (<exp> <exp>))`
   - 条件表达式，类似于`if-else`。例如：
     ```scheme
     (cond [(> x 0) 'positive]
           [(< x 0) 'negative]
           [else 'zero])
     ```
     这个例子根据`x`的值返回不同的结果。

6. `(cond (<exp> <exp>) ... (else <exp>))`
   - 条件表达式，带有`else`分支。与上例类似，`else`分支在所有条件都不满足时执行。

7. `(local (<def> ... <def>) <exp>)`
   - 局部定义，定义局部变量或函数。例如：
     ```scheme
     (local [(define x 10)
             (define y 20)]
       (+ x y))
     ```
     在局部环境中定义了变量`x`和`y`，并计算它们的和。

8. `(lambda (<var> ... <var>) <exp>)`
   - 匿名函数，定义一个没有名字的函数。例如：
     ```scheme
     (lambda (x y) (+ x y))
     ```
     定义了一个接受两个参数并返回它们和的匿名函数。

9. `(set! <var> <exp>)`
   - 设置变量的新值。例如：
     ```scheme
     (define x 10)
     (set! x 20)
     ```
     将变量`x`的值从`10`修改为`20`。

10. `(begin <exp> ... <exp>)`
    - 顺序执行多个表达式。例如：
      ```scheme
      (begin
        (display "Hello, ")
        (display "World!"))
      ```
      依次执行`display`函数，打印出`Hello, World!`。

### 总结

Advanced Student Scheme的核心语法通过这些基本的定义和表达式构造了一个强大的编程语言。这些构造块使得编写复杂程序成为可能，同时保持了语言的简洁和优雅。理解这些基础语法是掌握Scheme编程语言的第一步。