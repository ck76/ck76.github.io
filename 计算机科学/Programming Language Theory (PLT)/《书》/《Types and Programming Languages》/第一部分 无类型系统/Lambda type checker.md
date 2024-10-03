





```kotlin
// 抽象语法树 (AST)
sealed class Expr {
    data class Var(val name: String) : Expr()   // 变量
    data class Abs(val param: String, val body: Expr) : Expr()  // Lambda 抽象
    data class App(val func: Expr, val arg: Expr) : Expr()  // 函数应用
}

// 类型
sealed class Type {
    data class TVar(val name: String) : Type()  // 类型变量
    data class TFun(val from: Type, val to: Type) : Type()  // 函数类型
}

// 类型推断环境
class TypeEnv(private val env: Map<String, Type.TVar> = mutableMapOf()) {

    // 从环境中查找类型
    fun lookup(name: String): Type {
        return env[name] ?: throw IllegalArgumentException("Unbound variable: $name")
    }

    // 扩展环境
    fun extend(name: String, type: Type): TypeEnv {
        (env as MutableMap)[name] = type as Type.TVar
        return this
    }

//    重写toString方法
    override fun toString(): String {
        return env.toString()
    }
}

// 类型推断器
class TypeInfer {

    private var typeVarCounter = 0  // 类型变量计数器，用于生成新的类型变量

    // 生成新的类型变量
    private fun freshTypeVar(): Type.TVar {
        typeVarCounter += 1
        return Type.TVar("T$typeVarCounter")
    }

    // 类型推断函数
    fun infer(expr: Expr, env: TypeEnv = TypeEnv()): Type {
        println("infer $expr" + " env: ${env.toString()}")
        return when (expr) {
            is Expr.Var -> env.lookup(expr.name)  // 变量：从环境中查找类型
            is Expr.Abs -> {
                // Lambda 抽象：推断参数的类型并推断函数体的类型
                val paramType = freshTypeVar()  // 生成一个新的类型变量
                val extendedEnv = env.extend(expr.param, paramType)  // 扩展环境
                val bodyType = infer(expr.body, extendedEnv)  // 推断函数体类型
                Type.TFun(paramType, bodyType)  // 返回函数类型
            }
            is Expr.App -> {
                // 函数应用：推断函数和参数的类型
                val funcType = infer(expr.func, env)  // 推断函数类型
                val argType = infer(expr.arg, env)    // 推断参数类型
                val resultType = freshTypeVar()  // 生成一个新的类型变量表示结果类型
                unify(funcType, Type.TFun(argType, resultType))  // 进行类型统一
                resultType  // 返回结果类型
            }
        }
    }

//    // 类型统一：将两个类型强制统一
//    private fun unify(t1: Type, t2: Type) {
//        println("unify $t1 $t2")
//        when {
//            t1 is Type.TVar && t2 is Type.TVar && t1.name == t2.name -> return
//            t1 is Type.TFun && t2 is Type.TFun -> {
//                println("unify ${t1.from} ${t2.from}")
//                unify(t1.from, t2.from)
//                unify(t1.to, t2.to)
//            }
//            else -> throw IllegalArgumentException("Cannot unify types: $t1 and $t2")
//        }
//    }
private val subst = mutableMapOf<String, Type>()

    private fun unify(t1: Type, t2: Type) {
        println("unify $t1 $t2")
        when {
            // 如果两个类型变量相同，则它们已经统一
            t1 is Type.TVar && t2 is Type.TVar && t1.name == t2.name -> return

            // 如果 t1 是类型变量，则替换 t1 为 t2
            t1 is Type.TVar -> bind(t1, t2)

            // 如果 t2 是类型变量，则替换 t2 为 t1
            t2 is Type.TVar -> bind(t2, t1)

            // 如果 t1 和 t2 都是函数类型，则递归统一它们的输入和输出类型
            t1 is Type.TFun && t2 is Type.TFun -> {
                unify(t1.from, t2.from)
                unify(t1.to, t2.to)
            }

            // 如果无法统一，抛出异常
            else -> throw IllegalArgumentException("Cannot unify types: $t1 and $t2")
        }
    }

    // 绑定类型变量到其他类型上
    private fun bind(tv: Type.TVar, t: Type) {
        if (t == tv) return  // 避免自己绑定自己
        subst[tv.name] = t  // 将类型变量替换为目标类型
    }

}

// 辅助函数：将类型转换为可读的字符串
fun typeToString(type: Type): String {
    return when (type) {
        is Type.TVar -> type.name
        is Type.TFun -> "(${typeToString(type.from)} -> ${typeToString(type.to)})"
    }
}

// 主程序：运行类型推断器
fun main() {
    val infer = TypeInfer()

    // 示例 1：简单的变量 x
    val expr1 = Expr.Var("x")
    val env = TypeEnv(mutableMapOf("x" to Type.TVar("T1")))
    try {
        val resultType1 = infer.infer(expr1,env )
        println("Expression: $expr1, Type: ${typeToString(resultType1)}")
    } catch (e: Exception) {
        println(e.message)
    }

    println()

    // 示例 2：λx. x
    val expr2 = Expr.Abs("x", Expr.Var("x"))
    val resultType2 = infer.infer(expr2,env)
    println("Expression: λx. x, Type: ${typeToString(resultType2)}")


    println()

    // 示例 3：λx. λy. x
    val expr3 = Expr.Abs("x", Expr.Abs("y", Expr.Var("x")))
    val resultType3 = infer.infer(expr3,env)
    println("Expression: λx. λy. x, Type: ${typeToString(resultType3)}")
    println()

    // 示例 4：λx. (x x) (会报错，因为无法推断类型)
//    val expr4 = Expr.Abs("x", Expr.App(Expr.Var("x"), Expr.Var("x")))
//    try {
//        val resultType4 = infer.infer(expr4,env)
//        println("Expression: λx. (x x), Type: ${typeToString(resultType4)}")
//    } catch (e: Exception) {
//        println("Error: ${e.message}")
//    }


//    复杂的多层lambda例子
//    示例5 : λx. λy. x y z
//    val expr5 = Expr.Abs("x", Expr.Abs("y", Expr.App(Expr.App(Expr.Var("x"), Expr.Var("y")), Expr.Var("z"))))
//    val resultType5 = infer.infer(expr5)
//    println("Expression: λx. λy. x y z, Type: ${typeToString(resultType5)}")

//    application Cannot unify types
//    示例6 : (λx. x) (λy. y)
    val expr6 = Expr.App(
            Expr.Abs("x", Expr.Var("x")),
            Expr.Abs("y", Expr.Var("y"))
    )
    val resultType6 = infer.infer(expr6,env)
    println("Expression: (λx. x) (λy. y), Type: ${typeToString(resultType6)}")
    println()

//    示例7 : (λx. x) (λy. y) z
//    val exp = Expr.App(expr6, Expr.Var("z"))
//    val resultType7 = infer.infer(exp,env)
//    println("Expression: (λx. x) (λy. y) z, Type: ${typeToString(resultType7)}")
//    println()

}

```

