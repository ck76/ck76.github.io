在处理语法树（syntax tree）时，递归是一个常见的方式。为了更好地管理和操作语法树，我们可以采用多种方法。以下是几种常见的方法以及如何避免模板代码和给语法树附加属性的详细解释：

### 1. 表达式问题和递归处理

#### 直接手写递归

直接手写递归是处理语法树的一种简单而有效的方法。例如，对于一个简单的表达式树，我们可以定义如下结构：

```kotlin
sealed class Expr
data class Const(val number: Int) : Expr()
data class Add(val left: Expr, val right: Expr) : Expr()
data class Mul(val left: Expr, val right: Expr) : Expr()
```

然后，我们可以定义一个函数来计算表达式的值：

```kotlin
fun eval(expr: Expr): Int {
    return when (expr) {
        is Const -> expr.number
        is Add -> eval(expr.left) + eval(expr.right)
        is Mul -> eval(expr.left) * eval(expr.right)
    }
}
```

这种方法的缺点是对于每种新的操作都需要修改这个函数，容易产生重复的模板代码。

#### 访问者模式 (Visitor Pattern)

访问者模式可以通过将操作分离到不同的类来避免模板代码。首先，我们定义一个访问者接口：

```kotlin
interface ExprVisitor<R> {
    fun visitConst(const: Const): R
    fun visitAdd(add: Add): R
    fun visitMul(mul: Mul): R
}
```

然后，我们在表达式类中添加一个`accept`方法：

```kotlin
sealed class Expr {
    abstract fun <R> accept(visitor: ExprVisitor<R>): R
}

data class Const(val number: Int) : Expr() {
    override fun <R> accept(visitor: ExprVisitor<R>): R {
        return visitor.visitConst(this)
    }
}

data class Add(val left: Expr, val right: Expr) : Expr() {
    override fun <R> accept(visitor: ExprVisitor<R>): R {
        return visitor.visitAdd(this)
    }
}

data class Mul(val left: Expr, val right: Expr) : Expr() {
    override fun <R> accept(visitor: ExprVisitor<R>): R {
        return visitor.visitMul(this)
    }
}
```

然后我们实现一个具体的访问者，例如用于计算表达式的值：

```kotlin
class EvalVisitor : ExprVisitor<Int> {
    override fun visitConst(const: Const): Int {
        return const.number
    }

    override fun visitAdd(add: Add): Int {
        return add.left.accept(this) + add.right.accept(this)
    }

    override fun visitMul(mul: Mul): Int {
        return mul.left.accept(this) * mul.right.accept(this)
    }
}
```

这种方法避免了在同一个地方不断修改代码，但增加了复杂性。

#### 通用递归方案 (Recursion Schemes)

通用递归方案是一种更抽象的方法，通过分离递归结构和操作逻辑来避免模板代码。以下是一个简单的例子，展示如何使用递归方案来处理语法树。

首先，我们定义一个固定点（fixed point）类型：

```kotlin
data class Fix<F>(val unfix: F)
```

然后，我们定义一个表达式的基函子（functor）：

```kotlin
sealed class ExprF<out R>
data class ConstF<out R>(val number: Int) : ExprF<R>()
data class AddF<out R>(val left: R, val right: R) : ExprF<R>()
data class MulF<out R>(val left: R, val right: R) : ExprF<R>()
```

接下来，我们定义一个递归方案，例如 cata（cata 是递归函数的通用名称）：

```kotlin
fun <F, A> cata(f: (F) -> A, fa: Fix<F>): A {
    return f(fa.unfix.map { cata(f, it) })
}
```

我们需要为`ExprF`定义一个`map`方法：

```kotlin
fun <R, B> ExprF<R>.map(f: (R) -> B): ExprF<B> {
    return when (this) {
        is ConstF -> ConstF(number)
        is AddF -> AddF(f(left), f(right))
        is MulF -> MulF(f(left), f(right))
    }
}
```

然后我们可以定义一个具体的函数来评估表达式：

```kotlin
fun evalAlgebra(expr: ExprF<Int>): Int {
    return when (expr) {
        is ConstF -> expr.number
        is AddF -> expr.left + expr.right
        is MulF -> expr.left * expr.right
    }
}

fun eval(expr: Fix<ExprF>): Int {
    return cata(::evalAlgebra, expr)
}
```

### 2. 给语法树附加属性

给语法树附加属性可以通过在节点中添加额外的字段来实现。以下是一个简单的例子：

```kotlin
sealed class Expr {
    abstract var attributes: MutableMap<String, Any>
}

data class Const(val number: Int) : Expr() {
    override var attributes: MutableMap<String, Any> = mutableMapOf()
}

data class Add(val left: Expr, val right: Expr) : Expr() {
    override var attributes: MutableMap<String, Any> = mutableMapOf()
}

data class Mul(val left: Expr, val right: Expr) : Expr() {
    override var attributes: MutableMap<String, Any> = mutableMapOf()
}
```

在访问或处理节点时，可以根据需要附加或修改属性：

```kotlin
fun annotate(expr: Expr) {
    when (expr) {
        is Const -> expr.attributes["type"] = "const"
        is Add -> {
            expr.attributes["type"] = "add"
            annotate(expr.left)
            annotate(expr.right)
        }
        is Mul -> {
            expr.attributes["type"] = "mul"
            annotate(expr.left)
            annotate(expr.right)
        }
    }
}
```

### 结论

- **直接手写递归**：简单直接，但容易导致模板代码和高维护成本。
- **访问者模式**：将操作分离到不同的类中，减少模板代码，但增加了复杂性。
- **通用递归方案**：通过分离递归结构和操作逻辑来避免模板代码，但需要一定的抽象理解。
- **附加属性**：通过在节点中添加字段实现，可以灵活地在语法树上添加或修改信息。

这些方法各有优缺点，选择时需要根据具体的需求和上下文来决定。