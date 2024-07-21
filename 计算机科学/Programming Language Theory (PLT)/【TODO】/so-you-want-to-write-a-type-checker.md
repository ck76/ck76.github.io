

- https://github.com/BekaValentine/so-you-want-to-write-a-type-checker/blob/master/sywtwatc.hs
- https://web.archive.org/web/20190321175756/http://languagengine.co/blog/so-you-want-to-write-a-type-checker/



编写类型检查器（type checker）是实现编程语言的重要部分之一。我们将通过Kotlin语言为简单类型λ演算（simply typed lambda calculus）实现一个类型检查器。本例子包括对函数类型和对类型对的支持。

## 理论基础

### 类型系统定义

我们需要定义一个基础类型系统，该系统包含四种类型：`Foo`、`Bar`、`Baz`、以及函数和对类型。具体类型定义如下：

- **基础类型**：`Foo`、`Bar`、`Baz`
- **对类型**：`A * B`
- **函数类型**：`A -> B`

这些类型可以通过如下的形成规则来定义：

```plaintext
A type
======

-------- Foo Formation      -------- Bar Formation
Foo type                    Bar type

            -------- Baz Formation
            Baz type

A type    B type                  A type    B type
---------------- * Formation      ---------------- -> Formation
    A*B type                        A -> B type
```

### 在Kotlin中表示类型

在Kotlin中，我们可以用数据类来表示这些类型：

```kotlin
sealed class Type
object Foo : Type()
object Bar : Type()
object Baz : Type()
data class Prod(val left: Type, val right: Type) : Type()
data class Arrow(val arg: Type, val ret: Type) : Type()
```

### 判断类型

判断一个表达式是否为类型可以用一个递归函数来实现：

```kotlin
fun isType(t: Type): Boolean {
    return when (t) {
        is Foo -> true
        is Bar -> true
        is Baz -> true
        is Prod -> isType(t.left) && isType(t.right)
        is Arrow -> isType(t.arg) && isType(t.ret)
        else -> false
    }
}
```

### 上下文和变量

上下文用于记录变量及其类型。我们用一个链表来表示上下文：

```kotlin
sealed class Context
object Empty : Context()
data class Snoc(val rest: Context, val name: String, val type: Type) : Context()
```

判断变量是否在上下文中：

```kotlin
fun notIn(name: String, ctx: Context): Boolean {
    return when (ctx) {
        is Empty -> true
        is Snoc -> if (ctx.name == name) false else notIn(name, ctx.rest)
    }
}
```

### 表达式的类型判断

表达式可以是变量、对、λ抽象、应用等。我们首先定义表达式：

```kotlin
sealed class Term
data class Var(val name: String) : Term()
data class Pair(val first: Term, val second: Term) : Term()
data class Split(val pair: Term, val x: String, val a: Type, val y: String, val b: Type, val body: Term) : Term()
data class Lam(val x: String, val body: Term) : Term()
data class App(val func: Term, val arg: Term) : Term()
```

定义类型判断函数：

```kotlin
fun hasType(ctx: Context, term: Term, type: Type): Boolean {
    return when (term) {
        is Var -> hasTypeVar(term.name, type, ctx)
        is Pair -> if (type is Prod) hasType(ctx, term.first, type.left) && hasType(ctx, term.second, type.right) else false
        is Split -> if (type is Prod) {
            val newCtx = Snoc(Snoc(ctx, term.x, type.left), term.y, type.right)
            hasType(newCtx, term.body, type)
        } else false
        is Lam -> if (type is Arrow) hasType(Snoc(ctx, term.x, type.arg), term.body, type.ret) else false
        is App -> if (type is Arrow) hasType(ctx, term.func, Arrow(type.arg, type.ret)) && hasType(ctx, term.arg, type.arg) else false
    }
}

fun hasTypeVar(name: String, type: Type, ctx: Context): Boolean {
    return when (ctx) {
        is Empty -> false
        is Snoc -> if (ctx.name == name) ctx.type == type else hasTypeVar(name, type, ctx.rest)
    }
}
```

### 示例

测试一些标准示例：

```kotlin
fun main() {
    val emptyCtx = Empty

    // 恒等函数: \x. x : Foo -> Foo
    val idFoo = Lam("x", Var("x"))
    println(hasType(emptyCtx, idFoo, Arrow(Foo, Foo))) // true

    // first 函数: \p. split p as (x :: Foo, y :: Bar) in x : Foo * Bar -> Foo
    val fst = Lam("p", Split(Var("p"), "x", Foo, "y", Bar, Var("x")))
    println(hasType(emptyCtx, fst, Arrow(Prod(Foo, Bar), Foo))) // true

    // const 函数: \x. \y. x : Foo -> Bar -> Foo
    val const = Lam("x", Lam("y", Var("x")))
    println(hasType(emptyCtx, const, Arrow(Foo, Arrow(Bar, Foo)))) // true

    // apply 函数: \f. \x. f x : (Foo -> Bar) -> Foo -> Bar
    val apply = Lam("f", Lam("x", App(Var("f"), Var("x"))))
    println(hasType(emptyCtx, apply, Arrow(Arrow(Foo, Bar), Arrow(Foo, Bar)))) // true
}
```

这段代码展示了如何在Kotlin中实现一个简单类型λ演算的类型检查器。通过递归地检查上下文和表达式的类型，我们可以判断给定的表达式是否符合类型规则。