

- https://www.wikiwand.com/zh/Lambda%E7%AB%8B%E6%96%B9%E4%BD%93

[toc]

Lambda 立方体（Lambda Cube）是一种用于表示不同类型系统间关系的模型，由 Henk Barendregt 提出。它主要用于研究和分类各种类型系统，特别是那些在函数式编程语言如 Haskell 中使用的系统。Lambda 立方体中的每一个维度代表类型系统能表达的一种能力。

Lambda 立方体有三个维度，分别是：
1. **类型依赖于类型**（Types depending on types）：允许类型操作其他类型，例如在系统中可以定义类型构造函数。
2. **类型依赖于项**（Types depending on terms）：允许类型表达式依赖于具体的值，例如依赖类型。
3. **项依赖于类型**（Terms depending on types）：允许表达式中的变量具有某种类型，这是最基本的类型系统能力。

### 通俗例子解释 Lambda 立方体

想象一个编程语言的类型系统是一座由多层构成的大楼：
- **底层（项依赖于类型）**：这是所有建筑必须的基础，表示我们的程序中的函数和变量都需要有类型。例如，我们可以声明一个整数变量 `int x = 5;`，这里 `x` 是基于 `int` 这个类型的。

- **第二层（类型依赖于项）**：在这一层，类型可以基于具体的值进行定义。例如，我们可以定义一个长度为 `n` 的数组类型，其中 `n` 是一个具体的数值。这允许我们根据实际的需要来定制类型，比如 `Array<5>` 表示一个具有 5 个元素的数组。

- **第三层（类型依赖于类型）**：这层允许类型构造器，可以根据一个类型来创建另一个类型。例如，我们可以定义一个泛型容器类型，如 `Container<T>`，其中 `T` 可以是任何其他类型，如 `Container<Integer>` 或 `Container<String>`。

这三个维度一起作用，形成了一个非常强大的类型系统框架，允许语言设计者和程序员根据需要定义更加丰富和复杂的类型。在 Lambda 立方体中，每个顶点都代表了这三个维度的一种组合，可以描述具有相应类型系统特征的编程语言。例如，简单的类型化 Lambda 演算（如 Java 中的方法类型系统）就位于立方体的一个角落，而更复杂的系统，如允许依赖类型的语言，则可能位于立方体的另一个角落。

![bcf6afc886daf30a921a8ac885feb5a1](https://p.ipic.vip/wnijcv.png)

要详解类型系统表中的八个系统，我们需要理解每个系统如何扩展了基本的简单类型λ演算（Simply Typed Lambda Calculus，STLC）。以下是每个系统的详细解释：

1. **λ→ (STLC)**：
   - **关系**：$ \star \rightarrow \star $
   - **例子**：简单类型λ演算（STLC）
   - **解释**：在简单类型λ演算中，类型只包含基本类型（$\star$）和从一种类型到另一种类型的箭头类型（$ \rightarrow $）。这是最基本的类型系统，支持函数应用和函数抽象。

2. **λ2 (System F)**：
   - **关系**：$ \star \rightarrow \star $, $ \square \rightarrow \star $
   - **例子**：System F
   - **解释**：System F引入了类型多态性，允许函数不仅可以抽象值，还可以抽象类型。类型变量（$ \square $）可以被实例化为任何类型。这扩展了简单类型λ演算，使得泛型编程成为可能。

3. **λω (Weak λω)**：
   - **关系**：$ \star \rightarrow \star $, $ \square \rightarrow \square $
   - **例子**：Weak λω
   - **解释**：Weak λω进一步扩展了System F，允许类型构造器之间的抽象。这意味着我们可以构造接受类型并返回类型的类型构造器，使得类型系统更加灵活和强大。

4. **λω (System Fω)**：
   - **关系**：$ \star \rightarrow \star $, $ \square \rightarrow \star $, $ \square \rightarrow \square $
   - **例子**：System Fω
   - **解释**：System Fω结合了System F和Weak λω的特性，既允许类型多态性又允许类型构造器之间的抽象。这是一个非常强大的类型系统，能够表示复杂的类型关系和抽象。

5. **λP (LF)**：
   - **关系**：$ \star \rightarrow \star $, $ \star \rightarrow \square $
   - **例子**：LF（Logical Framework）
   - **解释**：LF是一种逻辑框架，用于表示逻辑推理中的各种规则。它允许类型依赖于其他类型，从而支持更复杂的逻辑表达。

6. **λP2 (λP2)**：
   - **关系**：$ \star \rightarrow \star $, $ \star \rightarrow \square $, $ \square \rightarrow \star $
   - **例子**：λP2
   - **解释**：λP2进一步扩展了LF，结合了类型多态性和类型依赖性。这使得它可以表示更复杂的逻辑和类型关系。

7. **λPω (Weak λPω)**：
   - **关系**：$ \star \rightarrow \star $, $ \star \rightarrow \square $, $ \square \rightarrow \square $
   - **例子**：Weak λPω
   - **解释**：Weak λPω是一个结合了Weak λω和LF的特性，允许类型构造器之间的抽象和类型依赖性。

8. **λPω (CoC)**：
   - **关系**：$ \star \rightarrow \star $, $ \square \rightarrow \star $, $ \star \rightarrow \square $, $ \square \rightarrow \square $
   - **例子**：CoC（Calculus of Constructions）
   - **解释**：CoC是λ立方体中最强大的系统，结合了所有前面系统的特性，允许完全的类型多态性、类型构造器抽象和类型依赖性。它是依赖类型理论的重要基础。

这些系统通过扩展基本类型λ演算，逐步增加了类型多态性、类型构造器和类型依赖性，使得类型系统能够表示更复杂的类型和逻辑关系。这些扩展使得编程语言能够更强大地表达和验证程序的正确性。



为了详细解释 lambda 立方体的每个节点和路径，并结合具体编程语言（如 Kotlin）中的例子，我们将逐步解释每个类型系统的特性，详细说明每个例子是如何结合多态性、类型构造器和依赖性的。

### 1. 简单类型 λ 演算 (\(\lambda\))

#### 示例：
```kotlin
// 简单类型示例
fun id(x: Int): Int = x

// 使用示例
val result = id(42) // result 是 42
```

- **依赖关系：**
  - 值 `x` 依赖于类型 `Int`
  - 函数 `id` 的返回类型依赖于 `x` 的类型 `Int`
  
### 2. 函数类型 (\($\lambda \rightarrow$\))

#### 示例：
```kotlin
// 函数类型示例
fun isEven(x: Int): Boolean = x % 2 == 0

// 使用示例
val check = isEven(4) // check 是 true
```

- **依赖关系：**
  - 值 `x` 依赖于类型 `Int`
  - 函数 `isEven` 的返回类型依赖于 `x` 的类型 `Int`
  - 返回值 `Boolean` 依赖于函数体中的计算结果

### 3. 系统 F（多态性，\($\lambda 2$\)）

#### 示例：
```kotlin
// 多态类型示例
fun <T> identity(x: T): T = x

// 使用示例
val intId = identity(42)      // intId 是 42
val strId = identity("hello") // strId 是 "hello"
```

- **依赖关系：**
  - 值 `x` 依赖于类型参数 `T`
  - 函数 `identity` 的返回类型依赖于类型参数 `T`
  - 类型 `T` 可以是任何类型，体现了多态性

### 4. 高阶类型（类型构造器，\($\lambda \omega$\)）

#### 示例：
```kotlin
// 类型构造器示例
fun <A, B> map(list: List<A>, f: (A) -> B): List<B> {
    return list.map(f)
}

// 使用示例
val nums = listOf(1, 2, 3)
val strs = map(nums) { it.toString() } // strs 是 ["1", "2", "3"]
```

- **依赖关系：**
  - 值 `list` 依赖于类型 `List<A>`
  - 函数 `f` 依赖于类型 `A -> B`
  - 函数 `map` 的返回类型依赖于类型 `List<B>`
  - 类型 `A` 和 `B` 是参数化的，可以是任何类型

### 5. 依赖类型（\(\lambda \Pi\)）【云里雾里】

#### 示例：
```kotlin
// 模拟依赖类型的示例
sealed class Vec<out T> {
    object Nil : Vec<Nothing>()
    data class Cons<T>(val head: T, val tail: Vec<T>) : Vec<T>()
}

fun <T> length(vec: Vec<T>): Int = when (vec) {
    is Vec.Nil -> 0
    is Vec.Cons -> 1 + length(vec.tail)
}

// 使用示例
val vec = Vec.Cons(1, Vec.Cons(2, Vec.Cons(3, Vec.Nil)))
val len = length(vec) // len 是 3
```

- **依赖关系：**
  - 值 `head` 依赖于类型参数 `T`
  - 值 `tail` 依赖于类型 `Vec<T>`
  - 函数 `length` 的返回值依赖于递归计算

### 6. 系统 Fω（多态性和类型构造器，\(\lambda 2 \rightarrow\)）

#### 示例：
```kotlin
// 结合多态性和类型构造器的示例
fun <A, B> flatMap(list: List<A>, f: (A) -> List<B>): List<B> {
    return list.flatMap(f)
}

// 使用示例
val nums = listOf(1, 2, 3)
val listOfLists = flatMap(nums) { listOf(it, it * 2) } // listOfLists 是 [1, 2, 2, 4, 3, 6]
```

- **依赖关系：**
  - 值 `list` 依赖于类型 `List<A>`
  - 函数 `f` 依赖于类型 `A -> List<B>`
  - 函数 `flatMap` 的返回类型依赖于类型 `List<B>`

### 7. 构造计算（多态性和依赖类型，\(\lambda \Pi 2\)）

#### 示例：
```kotlin
// 模拟多态性和依赖类型的示例
sealed class Either<out A, out B> {
    data class Left<out A>(val value: A) : Either<A, Nothing>()
    data class Right<out B>(val value: B) : Either<Nothing, B>()
}

fun <A, B, C> either(e: Either<A, B>, f: (A) -> C, g: (B) -> C): C = when (e) {
    is Either.Left -> f(e.value)
    is Either.Right -> g(e.value)
}

// 使用示例
val left = Either.Left(42)
val right = Either.Right("hello")
val leftResult = either(left, { it * 2 }, { it.length }) // leftResult 是 84
val rightResult = either(right, { it * 2 }, { it.length }) // rightResult 是 5
```

- **依赖关系：**
  - 值 `value` 依赖于类型参数 `A` 或 `B`
  - 函数 `either` 的返回类型依赖于函数 `f` 和 `g` 的结果类型 `C`
  - 类型 `A` 和 `B` 可以是任意类型，体现了多态性

### 8. 扩展构造计算（所有特性，\(\lambda \Pi \omega\)）

#### 示例：
```kotlin
// 模拟所有特性的示例
sealed class Expr {
    data class Var(val name: String) : Expr()
    data class Abs(val param: String, val body: Expr) : Expr()
    data class App(val func: Expr, val arg: Expr) : Expr()
}

fun eval(env: Map<String, Int>, expr: Expr): Int = when (expr) {
    is Expr.Var -> env[expr.name] ?: error("Undefined variable")
    is Expr.Abs -> error("Cannot evaluate an abstraction directly")
    is Expr.App -> {
        val func = eval(env, expr.func)
        val arg = eval(env, expr.arg)
        func + arg // Simplified for illustration; real implementation would be more complex
    }
}

// 使用示例
val env = mapOf("x" to 1, "y" to 2)
val expr = Expr.App(Expr.Var("x"), Expr.Var("y"))
val result = eval(env, expr) // result 是 3
```

- **依赖关系：**
  - 值 `name` 依赖于类型 `String`
  - 值 `param` 和 `body` 依赖于类型 `String` 和 `Expr`
  - 函数 `eval` 的返回类型依赖于表达式的计算结果
  - 类型 `Expr` 依赖于具体的表达式子类型

### 总结

通过这些详细的Kotlin示例，我们可以看到不同类型系统的特点和它们之间的关系。Lambda立方体展示了从简单类型到最复杂类型系统的逐步演进。每个节点增加了新的类型特性，使得类型系统更加丰富和强大。希望这些示例能够帮助你更好地理解这些概念。

### 如何结合多态、类型构造器和依赖性

- **多态性**：通过类型参数化（泛型）实现，可以适应多种类型。
- **类型构造器**：通过高阶类型（如List、Map）实现，可以构造复杂的类型。
- **依赖性**：通过依赖类型和递归函数实现，类型可以依赖于值，值也可以依赖于类型。

这些特性相互结合，使得编程语言在类型表达能力上非常强大，有助于捕捉程序中的潜在错误。