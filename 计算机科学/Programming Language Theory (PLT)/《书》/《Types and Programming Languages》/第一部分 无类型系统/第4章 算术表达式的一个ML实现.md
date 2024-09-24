[toc]



### 4 An ML Implementation of Arithmetic Expressions

本章主要介绍如何将布尔值和算术表达式的形式定义实现为实际的程序。形式化定义和推导规则对于理解编程语言的语义非常有帮助，但将其转化为具体的实现可以帮助我们更好地掌握这些理论。在本章中，我们将讨论如何使用 ML 语言家族中的 OCaml 来实现我们之前定义的简单语言。

OCaml 是一种流行的函数式编程语言，它提供了垃圾回收和递归函数的强大支持，使其非常适合用来实现程序语言的求值器和类型检查器等工具。

#### **本章概览**
1. **4.1 Syntax** - 介绍如何定义语言的语法，特别是如何在 OCaml 中表示语法树。
2. **4.2 Evaluation** - 介绍如何实现语言的求值器，利用模式匹配和递归函数来逐步求值表达式。
3. **4.3 The Rest of the Story** - 本节简要总结了实现的其余部分，可能包括一些扩展和完善的细节。

接下来，我们详细讲解每一节内容。

### 4.1 Syntax

在实现编程语言时，语法树（Abstract Syntax Tree，AST）是核心数据结构。每个表达式被表示为一个语法树节点，语法树节点可以包含其他节点作为其子节点。在 OCaml 中，我们可以通过定义**代数数据类型（algebraic data types）**来实现这种树状结构。

#### **语法定义：**

我们定义一个数据类型来表示布尔值和算术表达式的语法结构。这些表达式包括布尔常量（$true$ 和 $false$）、条件表达式（$if-then-else$）、数字常量（$0$）、以及算术操作符（$succ$，$pred$ 和 $iszero$）。

在 OCaml 中，这样的语法可以定义为：

```ocaml
type term =
  | TTrue                    (* 布尔常量 true *)
  | TFalse                   (* 布尔常量 false *)
  | TIf of term * term * term (* 条件表达式 if-then-else *)
  | TZero                    (* 数字常量 0 *)
  | TSucc of term            (* 后继函数 succ *)
  | TPred of term            (* 前驱函数 pred *)
  | TIsZero of term          (* 判断是否为零的 iszero 操作 *)
```

在这个定义中，$term$ 数据类型表示所有的表达式。每个构造函数（如 $TTrue$、$TFalse$）对应语言中的一种表达式形式。

- $TTrue$ 和 $TFalse$ 分别表示布尔常量 $true$ 和 $false$。
- $TIf(t1, t2, t3)$ 表示条件表达式 $if t1 then t2 else t3$。
- $TSucc(t)$ 表示后继函数应用于表达式 $t$。
- $TPred(t)$ 表示前驱函数应用于表达式 $t$。
- $TIsZero(t)$ 表示判断表达式 $t$ 是否为零。

这些构造函数为我们提供了定义各种表达式的基础。

### 4.2 Evaluation

在定义了语法之后，我们需要定义如何对这些表达式求值。**求值器（evaluator）**是根据特定规则计算表达式的最终结果。我们通过递归函数和模式匹配来实现求值。

#### **求值规则：**

我们基于之前章节中的形式化规则来定义求值过程。在 OCaml 中，使用模式匹配的递归函数可以自然地实现这些规则。下面是布尔值和算术表达式的求值器代码：

```ocaml
let rec eval t =
  match t with
  | TTrue -> TTrue
  | TFalse -> TFalse
  | TIf(t1, t2, t3) ->
      (match eval t1 with
       | TTrue -> eval t2
       | TFalse -> eval t3
       | _ -> failwith "guard of conditional is not a boolean")
  | TZero -> TZero
  | TSucc(t1) -> TSucc(eval t1)
  | TPred(t1) ->
      (match eval t1 with
       | TZero -> TZero
       | TSucc(t2) -> t2
       | _ -> failwith "predecessor of a non-numeric value")
  | TIsZero(t1) ->
      (match eval t1 with
       | TZero -> TTrue
       | TSucc(_) -> TFalse
       | _ -> failwith "argument of iszero is not a number")
```

#### **代码解析：**

- $eval$ 函数接收一个表达式 $t$，并返回该表达式的求值结果。
- 对于布尔值 $TTrue$ 和 $TFalse$，求值结果直接返回它们本身。
- 对于条件表达式 $TIf(t1, t2, t3)$，我们首先对 $t1$ 求值。如果 $t1$ 求值为 $TTrue$，则继续对 $t2$ 求值；如果为 $TFalse$，则对 $t3$ 求值。
- 对于 $TSucc$，我们递归地对 $t1$ 进行求值，然后再应用 $succ$ 操作。
- 对于 $TPred$，如果 $t1$ 求值为 $TZero$，则返回 $TZero$；如果 $t1$ 是 $succ$ 的结果，则返回它的前驱。
- 对于 $TIsZero$，我们判断 $t1$ 是否为 $TZero$，如果是，则返回 $TTrue$；否则返回 $TFalse$。

如果遇到不符合预期的表达式，比如 $succ$ 的参数是布尔值而不是数字，程序将抛出异常。

### 4.3 The Rest of the Story

到目前为止，我们已经实现了布尔值和算术表达式的语法和求值器。这个过程展示了如何使用形式化语法定义和推导规则构建实际的编程语言求值器。在接下来的章节中，我们会介绍更多的类型检查器、扩展语言和高级的编程语言设计概念。

这部分内容总结了如何将形式语法和推导规则转化为具体的实现代码。通过这种实现，可以将形式化的定义与程序设计相结合，帮助我们更加深入地理解编程语言的基础理论。

### 小结：
1. **语法实现**：我们使用代数数据类型表示布尔值和算术表达式的语法树。
2. **求值器**：通过模式匹配和递归函数实现表达式的求值规则。
3. **扩展思考**：这种实现方法可以推广到更加复杂的语言特性，如类型系统等。

### ---------------------------

### 4.1 Syntax

本节的目标是将我们定义的布尔值和算术表达式语言的语法结构表示为 OCaml 中的值类型。OCaml 提供了强大的数据类型定义机制，使得将形式化的语法转化为实际代码相对简单。

#### **OCaml 中的代数数据类型定义**

OCaml 允许通过**代数数据类型（algebraic data types）**来定义语言的语法树，每种类型的表达式都可以使用一个**构造函数（constructor）**来表示。在这部分中，我们的目标是通过构造一个代数数据类型来表示布尔值和算术表达式。

#### **代码解析**：

```ocaml
type term =
  | TmTrue of info                  (* 表示布尔常量 true *)
  | TmFalse of info                 (* 表示布尔常量 false *)
  | TmIf of info * term * term * term (* 表示条件表达式 if-then-else *)
  | TmZero of info                  (* 表示数字常量 0 *)
  | TmSucc of info * term           (* 表示后继函数 succ *)
  | TmPred of info * term           (* 表示前驱函数 pred *)
  | TmIsZero of info * term         (* 表示 iszero 操作 *)
```

在这个定义中，$term$ 是表示所有可能的布尔值和算术表达式的类型。每个构造函数（如 $TmTrue$ 或 $TmSucc$）用于表示不同的表达式类型：

- **$TmTrue$ 和 $TmFalse$**：分别表示布尔常量 $true$ 和 $false$。
- **$TmIf$**：表示条件表达式 $if t1 then t2 else t3$，其中 $t1$ 是条件，$t2$ 和 $t3$ 是两个分支。
- **$TmZero$**：表示数字常量 $0$。
- **$TmSucc$ 和 $TmPred$**：分别表示后继函数 $succ$ 和前驱函数 $pred$ 应用于某个表达式。
- **$TmIsZero$**：表示 $iszero$ 操作。

这些构造函数中的 $info$ 字段用于保存表达式来自的位置信息，例如源文件中的字符位置。这些信息可以帮助调试和错误处理。虽然在理解基本算法时这些信息可以忽略，但它们在实际实现中非常有用。

#### **检查数值的函数**

在定义求值器时，我们需要一个函数来检查某个表达式是否是数字值。在我们的语言中，数字值是 0 或由 $succ$ 构造的值（例如 $succ(succ(0))$ 表示数字 2）。我们通过递归函数来检查一个表达式是否是数字值。

```ocaml
let rec isnumericval t = match t with
  | TmZero(_) → true                (* 如果是 0，则返回 true *)
  | TmSucc(_, t1) → isnumericval t1  (* 如果是后继值，递归检查 t1 *)
  | _ → false                       (* 否则返回 false *)
```

这个函数使用了 **模式匹配** 来检查表达式的类型：

- 如果 $t$ 是 $TmZero$（即数字 0），函数返回 $true$。
- 如果 $t$ 是 $TmSucc(t1)$，函数递归地检查 $t1$ 是否是数字值。
- 对于其他类型的表达式，返回 $false$。

OCaml 中的模式匹配是一种非常强大的工具，它允许我们根据数据类型的不同构造分支进行处理。在这里，$_$ 表示通配符，匹配任何其他类型的表达式。

#### **检查值的函数**

接下来，我们定义一个类似的函数 $isval$ 来检查表达式是否是值。在我们当前的语言中，布尔值和数字值都被视为最终结果（即值）。

```ocaml
let rec isval t = match t with
  | TmTrue(_) → true               (* 如果是 true，则返回 true *)
  | TmFalse(_) → true              (* 如果是 false，则返回 true *)
  | t when isnumericval t → true    (* 如果是数字值，返回 true *)
  | _ → false                      (* 否则返回 false *)
```

- 如果表达式是布尔值 $TmTrue$ 或 $TmFalse$，返回 $true$。
- 如果表达式是数字值，则调用 $isnumericval$ 进行检查。
- 对于其他表达式，返回 $false$。

#### **总结**

通过定义这些基本的类型和函数，我们为布尔值和算术表达式的实现奠定了基础。我们可以通过模式匹配来检查和处理不同类型的表达式，这在实现语言的求值器和类型检查器时将非常有用。

在接下来的章节中，我们会进一步扩展这些定义，添加求值规则和类型检查功能。

### ----------------------------

### 4.2 求值

这一节讨论如何将前面的**单步求值规则**（见图 3-1 和图 3-2）转换为 OCaml 实现。规则定义了一个**部分函数**，当作用于尚未成为值的项时，该函数会返回项的下一步求值结果。对于值，求值函数不返回任何结果。

#### **实现概述**

我们需要将这些规则翻译成 OCaml 代码。在求值过程中，若对某个给定项没有适用的求值规则，我们可以选择抛出异常来表示这一情况。另一种处理方式是使用 $term option$ 类型来返回一个可选的项，如果求值成功则返回新项，否则返回 $None$。为了简洁，这里我们选择抛出异常。

#### **定义异常**

当没有适用的求值规则时，我们抛出一个名为 $NoRuleApplies$ 的异常。

```ocaml
exception NoRuleApplies
```

#### **单步求值器**

我们定义了 $eval1$ 函数来实现单步求值。该函数根据不同类型的项进行模式匹配，以决定如何对项进行求值。

```ocaml
let rec eval1 t = match t with
```

#### **处理 if 语句**

- 如果遇到 $if true then t2 else t3$，则求值结果是 $t2$。
- 如果遇到 $if false then t2 else t3$，则求值结果是 $t3$。
- 如果 $t1$ 仍然可以进一步求值，那么递归地对 $t1$ 求值，再返回新的条件表达式。

```ocaml
| TmIf(_,TmTrue(_),t2,t3) → t2
| TmIf(_,TmFalse(_),t2,t3) → t3
| TmIf(fi,t1,t2,t3) →
    let t1’ = eval1 t1 in
    TmIf(fi, t1’, t2, t3)
```

#### **处理 $succ$ 和 $pred$**

- 对 $succ$ 操作，递归求值子项 $t1$，再将结果应用到 $TmSucc$ 上。
- 对 $pred$ 操作，若项为 $0$，则结果为 $0$；若项为某个数值的 $succ$，则返回该数值本身。
- 如果 $t1$ 仍然可以进一步求值，则递归求值 $t1$，再返回新的 $pred$ 项。

```ocaml
| TmSucc(fi,t1) →
    let t1’ = eval1 t1 in
    TmSucc(fi, t1’)
| TmPred(_,TmZero(_)) → TmZero(dummyinfo)
| TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) → nv1
| TmPred(fi,t1) →
    let t1’ = eval1 t1 in
    TmPred(fi, t1’)
```

#### **处理 $iszero$**

- 如果 $iszero$ 的参数是 $0$，则结果为 $true$。
- 如果参数是某个数值的 $succ$，则结果为 $false$。
- 否则，对参数 $t1$ 进行递归求值，返回新的 $iszero$。

```ocaml
| TmIsZero(_,TmZero(_)) → TmTrue(dummyinfo)
| TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) → TmFalse(dummyinfo)
| TmIsZero(fi,t1) →
    let t1’ = eval1 t1 in
    TmIsZero(fi, t1’)
```

#### **没有适用的规则**

如果当前项不能匹配到任何求值规则，抛出 $NoRuleApplies$ 异常。

```ocaml
| _ → raise NoRuleApplies
```

#### **解释：**

- $eval1$ 函数对给定的项进行单步求值，逐步减少项的复杂度，直至无法再进一步简化。
- **模式匹配**用于对项进行分类，以决定应用哪一条规则。每个模式与一个特定的语法形式相匹配，例如 $TmIf$ 表示条件表达式，$TmSucc$ 表示后继操作。
- 如果某个项没有适用的规则，抛出异常。这种实现方式简化了代码逻辑，但也可以选择使用 $option$ 类型来更优雅地处理求值失败的情况。

#### **构造新项时的信息标注**

在某些情况下，我们会构造新的项。由于这些项并不直接来自用户的源代码，因此它们的位置信息不太有用。我们使用 $dummyinfo$ 来标记这些项的位置信息。$fi$ 则用于匹配模式中的位置信息字段。

#### **总结**

- 我们实现了单步求值的 OCaml 函数 $eval1$，它基于图 3-1 和图 3-2 中的单步求值规则。
- 通过模式匹配，$eval1$ 函数能够对不同类型的项进行正确的求值。
- 这种实现方式简洁明了，使用异常来处理无法求值的情况，同时使用递归来简化复杂的表达式。

### ---------------------------

### 4.2.2 练习 [推荐, ««« 3]

在这个练习中，你需要将现有的 $eval$ 函数改写为大步求值（big-step）风格的实现。小步求值（small-step）是一种逐步推进的求值方式，即每次只执行一个最小的求值步骤。而大步求值直接定义了从一个项到其最终值的整个转换过程。

**题目要求**：
- 将 $eval$ 函数改写为使用**大步求值**的形式（big-step semantics），这种形式直接将项 $t$ 映射为其最终值 $v$，而不经过中间步骤。

在第 3.5.17 练习中，我们已经看到大步求值规则的形式，它直接给出了从一个项到最终值的计算过程，而不是像小步求值那样一步步简化项。

大步求值的规则如下所示：

- 对于一个值项 $v$，它直接评估为自身：
$$
\frac{} {v ⇓ v} \quad (B-Value)
$$

- 对于条件表达式 $if\ t_1\ then\ t_2\ else\ t_3$，如果 $t_1$ 的求值结果是 $true$，则结果是 $t_2$ 的求值结果：
$$
\frac{t_1 ⇓ true \quad t_2 ⇓ v_2} {if\ t_1\ then\ t_2\ else\ t_3 ⇓ v_2} \quad (B-IfTrue)
$$

- 如果 $t_1$ 的求值结果是 $false$，则结果是 $t_3$ 的求值结果：
$$
\frac{t_1 ⇓ false \quad t_3 ⇓ v_3} {if\ t_1\ then\ t_2\ else\ t_3 ⇓ v_3} \quad (B-IfFalse)
$$

- 对于 $succ t1$，我们需要先对 $t_1$ 进行求值，得到数值 $nv_1$，然后结果是 $succ nv_1$：
$$
\frac{t_1 ⇓ nv_1} {succ\ t_1 ⇓ succ\ nv_1} \quad (B-Succ)
$$

- 对于 $pred t1$，我们有两种情况。如果 $t_1$ 的求值结果是 $0$，则结果为 $0$：
$$
\frac{t_1 ⇓ 0} {pred\ t_1 ⇓ 0} \quad (B-PredZero)
$$

- 如果 $t_1$ 的求值结果是某个数值的后继 $succ nv_1$，则结果为 $nv_1$：
$$
\frac{t_1 ⇓ succ\ nv_1} {pred\ t_1 ⇓ nv_1} \quad (B-PredSucc)
$$

- 对于 $iszero t1$，我们同样分两种情况。如果 $t_1$ 的求值结果是 $0$，则结果为 $true$：
$$
\frac{t_1 ⇓ 0} {iszero\ t_1 ⇓ true} \quad (B-IsZeroZero)
$$

- 如果 $t_1$ 的求值结果是某个数值的后继 $succ nv_1$，则结果为 $false$：
$$
\frac{t_1 ⇓ succ\ nv_1} {iszero\ t_1 ⇓ false} \quad (B-IsZeroSucc)
$$

### 解答：

我们需要根据这些大步求值规则改写 $eval$ 函数：

1. **重写 $eval$ 函数**：

我们不再逐步调用 $eval1$，而是直接递归处理每个子项，最终返回整个项的求值结果。

```ocaml
let rec eval t = match t with
  | TmTrue(_) → t
  | TmFalse(_) → t
  | TmIf(_, t1, t2, t3) →
      (match eval t1 with
       | TmTrue(_) → eval t2
       | TmFalse(_) → eval t3
       | _ → raise NoRuleApplies)
  | TmZero(_) → t
  | TmSucc(_, t1) →
      let nv1 = eval t1 in
      if isnumericval nv1 then TmSucc(dummyinfo, nv1)
      else raise NoRuleApplies
  | TmPred(_, t1) →
      (match eval t1 with
       | TmZero(_) → TmZero(dummyinfo)
       | TmSucc(_, nv1) when isnumericval nv1 → nv1
       | _ → raise NoRuleApplies)
  | TmIsZero(_, t1) →
      (match eval t1 with
       | TmZero(_) → TmTrue(dummyinfo)
       | TmSucc(_, nv1) when isnumericval nv1 → TmFalse(dummyinfo)
       | _ → raise NoRuleApplies)
  | _ → raise NoRuleApplies
```

### 详细解释：

1. **条件表达式**：$TmIf(_, t1, t2, t3)$ 处理 $if$ 表达式：
   - 首先对条件 $t_1$ 进行求值。
   - 如果 $t_1$ 的结果为 $TmTrue$，那么整个表达式的结果就是 $t_2$ 的求值结果。
   - 如果 $t_1$ 的结果为 $TmFalse$，那么结果就是 $t_3$ 的求值结果。

2. **后继操作**：$TmSucc(_, t1)$ 处理后继操作：
   - 首先对 $t_1$ 进行求值。
   - 如果 $t_1$ 的求值结果是一个数值（通过 $isnumericval$ 检查），那么结果就是 $TmSucc(nv1)$。

3. **前驱操作**：$TmPred(_, t1)$ 处理前驱操作：
   - 如果 $t_1$ 的求值结果是 $0$，那么返回 $0$。
   - 如果 $t_1$ 是某个数值的后继（$TmSucc(_, nv1)$），那么结果就是该数值 $nv1$。

4. **iszero 操作**：$TmIsZero(_, t1)$ 处理 $iszero$ 操作：
   - 如果 $t_1$ 的求值结果是 $0$，那么结果是 $TmTrue$。
   - 如果 $t_1$ 是某个数值的后继，结果是 $TmFalse$。

### 练习 3.5.18（变更求值策略）

如果我们希望在求值 $if$ 表达式时，先求值 $then$ 和 $else$ 分支，然后再求值守卫（$t1$），我们可以调整规则：

1. **先对 then 和 else 分支求值**：
   - 对 $if t1 then t2 else t3$，先求值 $t2$ 和 $t3$，再处理 $t1$。

```ocaml
let rec eval t = match t with
  | TmIf(_, t1, t2, t3) →
      let t2’ = eval t2 in
      let t3’ = eval t3 in
      (match eval t1 with
       | TmTrue(_) → t2’
       | TmFalse(_) → t3’
       | _ → raise NoRuleApplies)
  | ...
```

这个改动强制了 $then$ 和 $else$ 分支的求值优先于守卫的求值。

### ----------------------------

### 4.3 其余部分

在这一节中，作者讨论了解释器或编译器的实际实现过程。尽管我们在之前的部分已经讨论了如何定义和实现语言的求值逻辑，但在实际的编译器或解释器实现中，还有很多其他的部分需要考虑。

#### 解释器或编译器的常见步骤：

1. **读取输入**：程序的源代码通常存储在文件中。解释器或编译器首先需要从文件系统读取这些字符序列。
2. **词法分析**：读取的字符序列需要被处理为**词法单元（tokens）**。这个过程叫做**词法分析**（lexing）。每个词法单元对应于编程语言中的语法元素，如关键词、标识符、运算符等。
3. **语法分析**：词法单元进一步被处理为**抽象语法树（abstract syntax trees, AST）**，这个过程叫做**语法分析**（parsing）。抽象语法树是程序结构的树形表示，展示了程序的语法结构。
4. **求值**：一旦生成了抽象语法树，接下来就是根据定义的求值规则，对其进行求值（evaluation）。
5. **输出结果**：最终的求值结果通常需要以某种形式输出，比如打印到屏幕上。

这几个步骤的执行顺序可以用下图表示：

```
file I/O chars → lexing → tokens → parsing → terms → evaluation → values → printing
```

### 练习 4.2.1 [««]：为什么我们不建议在递归循环中使用 $try$ 处理异常？如何改进 $eval$ 的写法？

#### 题目分析：
在 $eval$ 函数的实现中，我们通过 $try$ 语句捕获 $NoRuleApplies$ 异常，来检测何时终止递归的求值过程。虽然这种方法简单明了，但它不是理想的实现方式。

#### 原因：
- **性能问题**：在某些编程语言（包括 OCaml）中，异常处理的开销相对较大。递归函数 $eval$ 每次进行递归调用时都需要进入异常处理上下文，这会增加不必要的开销。
- **代码风格问题**：在递归循环中使用异常处理是对异常机制的滥用。异常机制应该用于处理异常情况，而不是控制正常的程序流程。
  
#### 改进方案：
可以使用递归函数返回一个明确的标志来表示是否应该继续进行下一步的求值，而不是依赖异常机制。这样做不仅更加高效，而且代码风格也更加清晰。

#### 改写 $eval$ 函数：

我们可以将 $eval1$ 函数改写为返回 $option$ 类型，以明确指示下一步的求值是否可行。如果求值无法继续，函数将返回 $None$；否则，它会返回下一个待求值的项。

```ocaml
let rec eval1 t = match t with
  | TmIf(_, TmTrue(_), t2, _) -> Some t2
  | TmIf(_, TmFalse(_), _, t3) -> Some t3
  | TmIf(fi, t1, t2, t3) -> 
      (match eval1 t1 with
       | Some t1' -> Some (TmIf(fi, t1', t2, t3))
       | None -> None)
  | TmSucc(fi, t1) -> 
      (match eval1 t1 with
       | Some t1' -> Some (TmSucc(fi, t1'))
       | None -> None)
  | TmPred(_, TmZero(_)) -> Some (TmZero(dummyinfo))
  | TmPred(_, TmSucc(_, nv1)) when isnumericval nv1 -> Some nv1
  | TmPred(fi, t1) -> 
      (match eval1 t1 with
       | Some t1' -> Some (TmPred(fi, t1'))
       | None -> None)
  | TmIsZero(_, TmZero(_)) -> Some (TmTrue(dummyinfo))
  | TmIsZero(_, TmSucc(_, nv1)) when isnumericval nv1 -> Some (TmFalse(dummyinfo))
  | TmIsZero(fi, t1) -> 
      (match eval1 t1 with
       | Some t1' -> Some (TmIsZero(fi, t1'))
       | None -> None)
  | _ -> None
```

然后，我们可以改写 $eval$ 函数，使其根据 $eval1$ 的返回值决定是否继续求值：

```ocaml
let rec eval t =
  match eval1 t with
  | Some t' -> eval t'
  | None -> t
```

#### 改进解释：
1. **$eval1$ 改进**：现在，$eval1$ 返回的是 $option$ 类型，而不是直接抛出异常。$Some t'$ 表示可以继续求值，$None$ 表示没有可用的规则来进一步简化项。
2. **$eval$ 改进**：通过模式匹配 $eval1$ 的返回结果，$eval$ 只在有可能继续求值的情况下递归调用自己。否则，它直接返回最终结果，而不需要依赖异常机制。

通过这种方式，代码的效率和可读性都有所提高，也避免了滥用异常的情况。

### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------