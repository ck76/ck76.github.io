[toc]



**25 ML 实现 System F** (An ML Implementation of System F)

我们现在扩展第 10 章中对 $λ\to$ 的实现，以包含第 23 章和第 24 章中的 **全称类型** (Universal Types) 和 **存在类型** (Existential Types)。由于定义此系统的规则是 **语法导向** (Syntax Directed) 的（类似于 $λ\to$ 本身，但不同于具有 **子类型** (Subtyping) 或 **等价递归类型** (Equi-Recursive Types) 的演算），因此其 OCaml 实现相当直接。

对 $λ\to$ 实现最有趣的扩展是表示可能包含变量绑定（在量词中）的类型的表示。为此，我们使用第 6 章中介绍的 **de Bruijn 索引** (de Bruijn Indices) 技术。

---

### 25.1 无名类型表示 (Nameless Representation of Types)

我们首先通过添加 **类型变量** (Type Variables) 以及 **全称量词** (Universal Quantifiers) 和 **存在量词** (Existential Quantifiers) 来扩展类型的语法。

```ocaml
type ty =
  TyVar of int * int
| TyArr of ty * ty
| TyAll of string * ty
| TySome of string * ty
```

**解释：**

- **TyVar of int * int**：表示一个类型变量，包含两个整数：
  - **第一个整数**表示变量到其绑定点的距离，即 **de Bruijn 索引**。
  - **第二个整数**作为一致性检查，指定上下文的总大小。

- **TyArr of ty * ty**：表示函数类型，从类型 $ty$ 到类型 $ty$。

- **TyAll of string * ty**：表示全称量词类型，即 $\forall X.\ ty$，其中 $X$ 是类型变量的名称，$ty$ 是类型。

- **TySome of string * ty**：表示存在量词类型，即 $\exists X.\ ty$，其中 $X$ 是类型变量的名称，$ty$ 是类型。

这些约定与第 7.1 节中 **项** (Terms) 的表示方式完全相同。类型变量由两个整数组成：第一个指定变量到其绑定点的距离，第二个作为一致性检查，指定上下文的总大小。量词使用它们所绑定变量的字符串名称进行注释，作为打印函数的提示。

接下来，我们通过向 **绑定类型** (Binding Type) 添加一个新的构造器，扩展上下文以携带类型变量的绑定以及项变量的绑定：

```ocaml
type binding =
  NameBind
| VarBind of ty
| TyVarBind
```

**解释：**

- **NameBind**：仅用于解析和打印函数，不携带任何额外数据。

- **VarBind of ty**：项变量的绑定，携带一个类型 $ty$。

- **TyVarBind**：类型变量的绑定，不携带任何额外数据值，因为在此系统中（与项变量不同），类型变量没有任何附加的假设。在具有 **有界量化** (Bounded Quantification)（第 26 章）或 **高阶种类** (Higher Kinds)（第 29 章）的系统中，我们会为每个 $TyVarBind$ 添加适当的注解。

---

### 25.2 类型的提升和替换 (Type Shifting and Substitution)

由于类型现在包含变量，我们需要定义类型的 **提升** (Shifting) 和 **替换** (Substitution) 函数。

#### **25.2.1 练习 [«]**

**题目：** 使用定义 6.2.1（第 79 页）中的项提升函数作为模型，写出一个类似的数学定义，用于提升类型中的变量。

**解答：**

我们需要定义一个类型提升函数 $tyShift(d, c, tyT)$，它将类型 $tyT$ 中自由出现的类型变量的索引大于等于截止值 $c$ 的变量的索引增加 $d$。

**数学定义：**

- **对于类型变量：**

  $$
  tyShift(d, c, \text{TyVar}(x, n)) = \begin{cases}
    \text{TyVar}(x + d, n + d), & \text{如果 } x \geq c \\
    \text{TyVar}(x, n + d), & \text{否则}
  \end{cases}
  $$

  - **$x$**：类型变量的索引。

  - **$n$**：上下文的大小。

- **对于函数类型：**

  $$
  tyShift(d, c, \text{TyArr}(tyT_1, tyT_2)) = \text{TyArr}(tyShift(d, c, tyT_1),\ tyShift(d, c, tyT_2))
  $$

- **对于全称量词类型：**

  $$
  tyShift(d, c, \text{TyAll}(X, tyT)) = \text{TyAll}(X,\ tyShift(d, c + 1, tyT))
  $$

- **对于存在量词类型：**

  $$
  tyShift(d, c, \text{TySome}(X, tyT)) = \text{TySome}(X,\ tyShift(d, c + 1, tyT))
  $$

在这里，$d$ 是要提升的数量，$c$ 是截止值，用于避免提升被绑定的变量索引。

---

在第 7.2 节中，我们将项的提升和替换展示为两个独立的函数，但我们提到，书的网站上提供的实现实际上使用了一个通用的“映射”函数来执行这两个任务。类似的映射函数也可用于定义类型的提升和替换。现在让我们看看这些映射函数。

基本的观察是，**提升** (Shifting) 和 **替换** (Substitution) 在除变量之外的所有构造器上具有完全相同的行为。如果我们将它们在变量上的行为抽象出来，那么它们就变得相同。例如，下面是我们通过机械地将练习 25.2.1 的解答转录为 OCaml 所得到的针对类型的专用提升函数：

```ocaml
let typeShiftAbove d c tyT =
  let rec walk c tyT = match tyT with
    TyVar(x, n) -> if x >= c then TyVar(x + d, n + d) else TyVar(x, n + d)
  | TyArr(tyT1, tyT2) -> TyArr(walk c tyT1, walk c tyT2)
  | TyAll(tyX, tyT2) -> TyAll(tyX, walk (c + 1) tyT2)
  | TySome(tyX, tyT2) -> TySome(tyX, walk (c + 1) tyT2)
  in walk c tyT
```

**解释：**

- **参数 $d$**：要提升的自由变量的数量。

- **参数 $c$**：**截止值** (Cutoff)，在此值以下的变量不应提升，以避免提升被绑定的变量。

- **函数 $walk c tyT$**：递归地遍历类型 $tyT$，根据类型的构造器进行模式匹配。

  - **$TyVar(x, n)$**：如果 $x \geq c$，则需要提升索引：

    $$
    \text{如果 } x \geq c,\ \text{则 } \text{TyVar}(x + d, n + d);\quad \text{否则 } \text{TyVar}(x, n + d)
    $$

  - **$TyArr(tyT1, tyT2)$**：函数类型，递归地对 $tyT1$ 和 $tyT2$ 调用 $walk$。

  - **$TyAll(tyX, tyT2)$** 和 **$TySome(tyX, tyT2)$**：对于量词，增加截止值 $c$（因为进入了一个新的绑定层次），递归地调用 $walk (c + 1) tyT2$。

现在，如果我们将 $typeShiftAbove$ 中的 $TyVar$ 分支抽象为一个新的参数 $onvar$，并且去掉只在 $TyVar$ 分支中使用的参数 $d$，我们就得到了一个通用的映射函数：

```ocaml
let tymap onvar c tyT =
  let rec walk c tyT = match tyT with
    TyArr(tyT1, tyT2) -> TyArr(walk c tyT1, walk c tyT2)
  | TyVar(x, n) -> onvar c x n
  | TyAll(tyX, tyT2) -> TyAll(tyX, walk (c + 1) tyT2)
  | TySome(tyX, tyT2) -> TySome(tyX, walk (c + 1) tyT2)
  in walk c tyT
```

我们可以通过提供一个基于 $c$、$x$ 和 $n$ 的函数作为参数，来恢复提升函数：

```ocaml
let typeShiftAbove d c tyT =
  tymap
    (fun c x n -> if x >= c then TyVar(x + d, n + d) else TyVar(x, n + d))
    c tyT
```

为了方便，我们还定义了一个初始截止值为 0 的特化版本：

```ocaml
let typeShift d tyT = typeShiftAbove d 0 tyT
```

我们也可以实例化 $tymap$ 来实现将类型 $tyS$ 替换到类型 $tyT$ 中编号为 $j$ 的类型变量的操作：

```ocaml
let typeSubst tyS j tyT =
  tymap
    (fun j x n -> if x = j then (typeShift j tyS) else TyVar(x, n))
    j tyT
```

当我们在类型检查和求值过程中使用类型替换时，我们总是要替换编号为 0（最外层）的变量，并且希望对结果进行提升，使该变量消失。辅助函数 $typeSubstTop$ 为我们完成了这一操作：

```ocaml
let typeSubstTop tyS tyT =
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)
```

---

### 25.3 项 (Terms)

在项的层面上，工作类似。我们首先扩展第 10 章中的项数据类型，添加全称和存在类型的引入和消除形式。

```ocaml
type term =
  TmVar of info * int * int
| TmAbs of info * string * ty * term
| TmApp of info * term * term
| TmTAbs of info * string * term
| TmTApp of info * term * ty
| TmPack of info * ty * term * ty
| TmUnpack of info * string * string * term * term
```

**解释：**

- **TmVar of info * int * int**：项变量，$info$ 是位置信息，第一个 $int$ 是 de Bruijn 索引，第二个 $int$ 是上下文的大小。

- **TmAbs of info * string * ty * term**：抽象（λ 抽象），带有类型注解。

- **TmApp of info * term * term**：应用，将一个项应用于另一个项。

- **TmTAbs of info * string * term**：类型抽象，即类型参数的 λ 抽象，绑定一个类型变量。

- **TmTApp of info * term * ty**：类型应用，将一个项应用于一个类型。

- **TmPack of info * ty * term * ty**：打包一个存在类型的值。

- **TmUnpack of info * string * string * term * term**：解包一个存在类型的值。

项的提升和替换的定义与第 10 章中的类似。然而，我们在这里像前一节对类型所做的那样，用一个通用的映射函数来编写它们。映射函数如下：

```ocaml
let tmmap onvar ontype c t =
  let rec walk c t = match t with
    TmVar(fi, x, n) -> onvar fi c x n
  | TmAbs(fi, x, tyT1, t2) -> TmAbs(fi, x, ontype c tyT1, walk (c + 1) t2)
  | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)
  | TmTAbs(fi, tyX, t2) -> TmTAbs(fi, tyX, walk (c + 1) t2)
  | TmTApp(fi, t1, tyT2) -> TmTApp(fi, walk c t1, ontype c tyT2)
  | TmPack(fi, tyT1, t2, tyT3) ->
      TmPack(fi, ontype c tyT1, walk c t2, ontype c tyT3)
  | TmUnpack(fi, tyX, x, t1, t2) ->
      TmUnpack(fi, tyX, x, walk c t1, walk (c + 2) t2)
  in walk c t
```

**注意：**

- $tmmap$ 接受四个参数，比 $tymap$ 多一个。原因是，项可能包含两种不同类型的变量：**项变量** (Term Variables) 和嵌入在项的类型注解中的 **类型变量** (Type Variables)。

- 在进行提升时，需要对这两种变量分别处理。

- 参数 $onvar$ 和 $ontype$ 分别告诉 $tmmap$ 在遇到项变量和类型时该如何处理。

项的提升可以通过给 $tmmap$ 提供适当的参数来定义：

```ocaml
let termShiftAbove d c t =
  tmmap
    (fun fi c x n -> if x >= c then TmVar(fi, x + d, n + d)
                     else TmVar(fi, x, n + d))
    (typeShiftAbove d)
    c t

let termShift d t = termShiftAbove d 0 t
```

**解释：**

- **对于项变量**，我们检查是否需要提升索引。

- **对于类型注解**，我们调用前面定义的 $typeShiftAbove$ 函数。

将一个项替换到另一个项中的函数类似：

```ocaml
let termSubst j s t =
  tmmap
    (fun fi j x n -> if x = j then termShift j s else TmVar(fi, x, n))
    (fun j tyT -> tyT)
    j t
```

**注意：**

- 类型注解不会被 $termSubst$ 改变，因为类型不能包含项变量。

我们还需要一个将类型替换到项中的函数，用于类型应用的求值规则，例如：

$$
(\lambda X.\ t_{12})\ [T_2] \rightarrow [X \mapsto T_2] t_{12} \quad (E\text{-}TappTabs)
$$

这个函数也可以用项映射器来定义：

```ocaml
let rec tytermSubst tyS j t =
  tmmap (fun fi c x n -> TmVar(fi, x, n))
        (fun j tyT -> typeSubst tyS j tyT) j t
```

**解释：**

- **对于项变量**，我们不做任何改变。

- **对于类型注解**，我们对其执行类型替换。

最后，我们定义一些方便的函数，将基本的替换函数封装起来，以供 $eval$ 和 $typeof$ 使用：

```ocaml
let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let tytermSubstTop tyS t =
  termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)
```

---

### 25.4 求值 (Evaluation)

对 $eval$ 函数的扩展是对第 23 章和第 24 章中引入的求值规则的直接转录。主要的工作由前一节中定义的替换函数完成。

```ocaml
let rec eval1 ctx t = match t with
  (* 省略其他情况 *)
| TmTApp(fi, TmTAbs(_, x, t11), tyT2) ->
    tytermSubstTop tyT2 t11
| TmTApp(fi, t1, tyT2) ->
    let t1' = eval1 ctx t1 in
    TmTApp(fi, t1', tyT2)
| TmUnpack(fi, _, _, TmPack(_, tyT11, v12, _), t2) when isval ctx v12 ->
    tytermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)
| TmUnpack(fi, tyX, x, t1, t2) ->
    let t1' = eval1 ctx t1 in
    TmUnpack(fi, tyX, x, t1', t2)
| TmPack(fi, tyT1, t2, tyT3) ->
    let t2' = eval1 ctx t2 in
    TmPack(fi, tyT1, t2', tyT3)
  (* 省略其他情况 *)
```

**25.4.1 练习 [«]**

**题目：** 为什么在第一个 $TmUnpack$ 情况下需要 $termShift$？

**解答：**

在第一个 $TmUnpack$ 情况下，我们有：

```ocaml
| TmUnpack(fi, _, _, TmPack(_, tyT11, v12, _), t2) when isval ctx v12 ->
    tytermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)
```

需要对 $v12$ 进行 $termShift 1$，这是因为在解包时，我们将一个值 $v12$ 替换到 $t2$ 中，而 $t2$ 的上下文中增加了两个绑定（一个类型变量，一个项变量）。因此，为了正确地替换 $v12$，我们需要提升其索引，以适应新的上下文。

如果不进行 $termShift$，$v12$ 中的自由变量可能会错误地引用到 $t2$ 中新添加的绑定，导致求值错误。

---

### 25.5 类型检查 (Typing)

$typeof$ 函数的新分支直接来源于类型抽象、类型应用以及存在类型的打包和解包的类型规则。我们展示了 $typeof$ 的完整定义，以便将新的 $TmTAbs$ 和 $TmTApp$ 分支与旧的普通抽象和应用的分支进行比较。

```ocaml
let rec typeof ctx t =
  match t with
    TmVar(fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs(fi, x, tyT1, t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, typeShift (-1) tyT2)
  | TmApp(fi, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
          TyArr(tyT11, tyT12) ->
              if (=) tyT2 tyT11 then tyT12
              else error fi "parameter type mismatch"
        | _ -> error fi "arrow type expected")
  | TmTAbs(fi, tyX, t2) ->
      let ctx = addbinding ctx tyX TyVarBind in
      let tyT2 = typeof ctx t2 in
      TyAll(tyX, tyT2)
  | TmTApp(fi, t1, tyT2) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyAll(_, tyT12) -> typeSubstTop tyT2 tyT12
        | _ -> error fi "universal type expected")
  | TmPack(fi, tyT1, t2, tyT) ->
      (match tyT with
          TySome(tyY, tyT2) ->
              let tyU = typeof ctx t2 in
              let tyU' = typeSubstTop tyT1 tyT2 in
              if (=) tyU tyU' then tyT
              else error fi "doesn't match declared type"
        | _ -> error fi "existential type expected")
  | TmUnpack(fi, tyX, x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TySome(tyY, tyT11) ->
              let ctx' = addbinding ctx tyX TyVarBind in
              let ctx'' = addbinding ctx' x (VarBind tyT11) in
              let tyT2 = typeof ctx'' t2 in
              typeShift (-2) tyT2
        | _ -> error fi "existential type expected")
```

**解释：**

- **$TmTAbs$ 分支：**

  - 我们在上下文中添加类型变量绑定 $tyX$，然后对 $t2$ 进行类型检查。

  - 最终返回类型为 $TyAll(tyX, tyT2)$。

- **$TmTApp$ 分支：**

  - 对 $t1$ 进行类型检查，期望其类型为 $TyAll$。

  - 然后将类型 $tyT2$ 替换到 $tyT12$ 中，得到结果类型。

- **$TmPack$ 分支：**

  - 检查打包的类型是否为存在类型。

  - 对 $t2$ 进行类型检查，期望其类型与替换后的类型相同。

- **$TmUnpack$ 分支：**

  - 对 $t1$ 进行类型检查，期望其类型为存在类型。

  - 在上下文中添加类型变量绑定 $tyX$ 和项变量绑定 $x$。

  - 对 $t2$ 进行类型检查，然后对结果类型进行 $typeShift (-2)$，以移除上下文中添加的两个绑定。

**注意：**

- 在 $TmUnpack$ 分支中，如果 $t2$ 的类型中包含了绑定的类型变量 $X$，那么在 $typeShift (-2)$ 时会产生负索引，导致作用域错误。

- 为了防止这种情况，我们重新定义了 $typeShiftAbove$，使其在遇到负索引时抛出错误：

  ```ocaml
  let typeShiftAbove d c tyT =
    tymap
      (fun c x n -> if x >= c then
                      if x + d < 0 then err "Scoping error!"
                      else TyVar(x + d, n + d)
                    else TyVar(x, n + d))
      c tyT
  ```

  这样，当我们计算存在消除表达式 $let {X, x} = t1 in t2$ 的主体 $t2$ 的类型时，如果 $t2$ 中包含了绑定的类型变量 $X$，就会报告作用域错误。

**示例：**

```ocaml
let {X, x} = ({*Nat, 0} as {∃X, X}) in x;
```

将产生错误：

```
Error: Scoping error!
```

---

### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------



### ---------------------------



### ----------------------------