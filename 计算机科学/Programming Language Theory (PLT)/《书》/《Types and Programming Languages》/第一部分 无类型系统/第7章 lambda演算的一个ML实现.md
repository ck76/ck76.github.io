[toc]



### 第7章 无类型 Lambda 演算的 ML 实现 (An ML Implementation of the Lambda-Calculus)

在本章中，我们将基于第4章中的算术表达式解释器，以及第6章中关于变量绑定和替换的处理，构建一个无类型 Lambda 演算的解释器。

通过将前面的定义直接翻译成 OCaml，可以获得一个可执行的无类型 Lambda 项求值器。与第4章一样，我们只展示核心算法，忽略词法分析、解析、打印等问题。

---

### 7.1 项和上下文 (Terms and Contexts)

我们可以通过直接翻译 **定义6.1.2** 来获得表示项抽象语法树的 OCaml 数据类型：

```ocaml
type term =
    TmVar of int
  | TmAbs of term
  | TmApp of term * term
```

**解释：**

- **TmVar of int**：表示一个变量，其存储的是变量的 de Bruijn 指数（即一个整数）。
- **TmAbs of term**：表示一个抽象（λ 抽象），包含了抽象的主体（一个子项）。
- **TmApp of term * term**：表示一个应用，由两个被应用的子项组成。

然而，我们在实现中实际使用的定义会包含更多的信息。

---

**改进一：**

首先，和之前一样，给每个项添加一个 $info$ 元素，记录该项最初出现在文件中的位置，以便错误打印例程可以将用户（甚至自动地将用户的文本编辑器）指向发生错误的精确位置。

```ocaml
type term =
    TmVar of info * int
  | TmAbs of info * term
  | TmApp of info * term * term
```

**解释：**

- **info**：通常是一个包含文件名、行号和列号的结构，帮助定位源代码中的位置。

---

**改进二：**

其次，为了调试的目的，在每个变量节点上携带一个额外的数字作为一致性检查。约定是，这个第二个数字总是包含了变量出现时上下文的总长度。

```ocaml
type term =
    TmVar of info * int * int
  | TmAbs of info * term
  | TmApp of info * term * term
```

**解释：**

- 在 $TmVar$ 中，除了变量的 de Bruijn 索引（第一个 $int$），还包含了上下文的长度（第二个 $int$）。这有助于在打印变量时验证这个数字是否对应于当前上下文的实际大小；如果不对应，那么可能某个地方忘记了执行移位操作。

---

**改进三：**

最后一个改进也与打印有关。虽然项在内部使用 de Bruijn 索引表示，但显然这不是它们应该呈现给用户的方式：我们应该在解析期间将普通表示转换为无名项，并在打印期间转换回普通形式。

**问题：**

- 如果我们完全按照天真方式处理（例如，为变量名生成完全新的符号），那么打印的项中绑定变量的名字将与原始程序中的名字毫无关系。

**解决方案：**

- 通过在每个抽象中注释一个字符串，用作绑定变量名的提示，来解决这个问题。

```ocaml
type term =
    TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term
```

**解释：**

- 在 $TmAbs$ 中，添加了一个 $string$，作为绑定变量的名称提示。

---

**注意：**

- 基本项操作（特别是替换）不会对这些字符串做任何花哨的处理：它们只是以原始形式被携带，不进行名称冲突、捕获等检查。
- 当打印例程需要为绑定变量生成一个新的名称时，它首先尝试使用提供的提示；如果发现与当前上下文中已经使用的名称冲突，它会尝试类似的名称，添加撇号（$'$）直到找到一个当前未使用的名称。这确保了打印的项将类似于用户期望的内容，可能只会多出几个撇号。

---

**打印函数：**

打印例程本身如下所示：

```ocaml
let rec printtm ctx t = match t with
    TmAbs(fi, x, t1) →
      let (ctx', x') = pickfreshname ctx x in
      pr "(lambda "; pr x'; pr ". "; printtm ctx' t1; pr ")"
  | TmApp(fi, t1, t2) →
      pr "("; printtm ctx t1; pr " "; printtm ctx t2; pr ")"
  | TmVar(fi, x, n) →
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr "[bad index]"
```

**解释：**

- **TmAbs**：

  - 使用 $pickfreshname$ 函数从上下文 $ctx$ 中挑选一个与提示名称 $x$ 不冲突的新名称 $x'$，并将其添加到新的上下文 $ctx'$ 中。
  - 然后打印 $(lambda x'. t1)$，其中 $t1$ 在新的上下文 $ctx'$ 中。

- **TmApp**：

  - 打印 $(t1 t2)$。

- **TmVar**：

  - 如果上下文的长度 $ctxlength ctx$ 等于变量的声明的上下文长度 $n$，则使用 $index2name$ 函数从上下文中获取变量名并打印。
  - 否则，打印 $[bad index]$，表示变量的上下文长度不匹配，可能存在错误。

---

**辅助数据类型和函数：**

- **上下文 (context)**：

  ```ocaml
  type context = (string * binding) list
  ```

  - 这是一个字符串和关联的绑定的列表。

- **绑定 (binding)**：

  ```ocaml
  type binding = NameBind
  ```

  - 当前的绑定类型非常简单，只是一个占位符 $NameBind$，不携带任何有趣的信息。
  - 在后面的章节（第10章）中，我们会引入 $binding$ 类型的其他情况，用于跟踪与变量相关的类型假设等信息。

- **其他函数**：

  - **pr**：将字符串发送到标准输出流的函数。
  - **ctxlength**：返回上下文的长度。
  - **index2name**：根据变量的索引，从上下文中查找变量的字符串名称。
  - **pickfreshname**：从上下文 $ctx$ 和提示名称 $x$ 中找到一个类似于 $x$ 的名称 $x'$，确保 $x'$ 未在 $ctx$ 中使用，然后将 $x'$ 添加到 $ctx$ 中形成新的上下文 $ctx'$，并返回 $(ctx', x')$。

---

**实际的打印函数：**

- 在书的官方网站上的 $untyped$ 实现中，实际的打印函数比这个更复杂，考虑了另外两个问题：

  1. **省略尽可能多的括号**：遵循应用左结合和抽象主体尽可能向右延伸的约定，尽可能省略括号。

  2. **格式化指令**：生成用于底层漂亮打印模块（OCaml 的 $Format$ 库）的格式化指令，该模块决定换行和缩进。

---

### 7.2 移位和替换 (Shifting and Substitution)

**移位 (Shifting)：**

根据 **定义6.2.1**，移位的定义可以几乎逐字地翻译成 OCaml：

```ocaml
let termShift d t =
  let rec walk c t = match t with
      TmVar(fi, x, n) →
        if x >= c then TmVar(fi, x + d, n + d)
        else TmVar(fi, x, n + d)
    | TmAbs(fi, x, t1) →
        TmAbs(fi, x, walk (c + 1) t1)
    | TmApp(fi, t1, t2) →
        TmApp(fi, walk c t1, walk c t2)
  in walk 0 t
```

**解释：**

- **外部函数 $termShift d t$**：

  - 接受一个移位量 $d$ 和一个项 $t$，返回移位后的项。

- **内部递归函数 $walk c t$**：

  - $c$ 是当前的截断值（cutoff），初始值为 $0$。
  - **TmVar**：

    - 如果变量的索引 $x >= c$，表示它是一个自由变量，需要移位，因此索引增加 $d$。
    - 否则，保持索引不变。
    - 无论哪种情况，变量的上下文长度 $n$ 都需要增加 $d$，以反映上下文的变化。

  - **TmAbs**：

    - 处理抽象时，截断值 $c$ 增加 $1$，递归处理抽象的主体 $t1$。

  - **TmApp**：

    - 对应用的两个子项 $t1$ 和 $t2$，在相同的截断值 $c$ 下递归处理。

**注意：**

- 因为 $d$ 从未改变，所以不需要在每次调用 $walk$ 时传递它；在变量的情况下，直接使用外部绑定的 $d$。

- 顶层的移位 $↑^d(t)$ 表示为 $termShift d t$。

---

**替换 (Substitution)：**

类似地，替换函数几乎直接来自 **定义6.2.4**：

```ocaml
let termSubst j s t =
  let rec walk c t = match t with
      TmVar(fi, x, n) →
        if x = j + c then termShift c s
        else TmVar(fi, x, n)
    | TmAbs(fi, x, t1) →
        TmAbs(fi, x, walk (c + 1) t1)
    | TmApp(fi, t1, t2) →
        TmApp(fi, walk c t1, walk c t2)
  in walk 0 t
```

**解释：**

- **外部函数 $termSubst j s t$**：

  - 在项 $t$ 中，用项 $s$ 替换编号为 $j$ 的变量。

- **内部递归函数 $walk c t$**：

  - $c$ 是当前的截断值，初始值为 $0$。
  - **TmVar**：

    - 如果变量的索引 $x = j + c$，则在当前位置需要替换，替换为 $termShift c s$。
      - 这里对替换项 $s$ 进行移位 $termShift c s$，以适应当前的上下文。
    - 否则，变量保持不变。

  - **TmAbs**：

    - 处理抽象时，截断值 $c$ 增加 $1$，递归处理抽象的主体 $t1$。

  - **TmApp**：

    - 对应用的两个子项 $t1$ 和 $t2$，在相同的截断值 $c$ 下递归处理。

**注意：**

- 与原始的替换定义的唯一区别是，这里我们在 $TmVar$ 情况下一次性对 $s$ 进行所有的移位，而不是每次经过一个绑定器时将 $s$ 移位一次。

- 这意味着在每次对 $walk$ 的调用中，参数 $j$ 都是相同的，因此我们可以在内部定义中省略它。

---

**tmmap 函数：**

- 读者可能注意到 $termShift$ 和 $termSubst$ 的定义非常相似，仅在遇到变量时采取的操作不同。

- 书中网站提供的 $untyped$ 实现利用了这个观察，将移位和替换操作都表示为一个更通用的函数 $tmmap$ 的特殊情况。

- 给定一个项 $t$ 和一个函数 $onvar$，$tmmap onvar t$ 的结果是一个与 $t$ 形状相同的项，其中每个变量都被 $onvar$ 函数处理的结果所替换。

- 这种技巧在一些更大的演算中节省了相当多的重复劳动；第25.2节对其进行了更详细的解释。

---

**β-约简中的替换操作：**

在 Lambda 演算的操作语义中，替换唯一使用的地方是在 β-约简规则中。

- 正如我们之前提到的，这个规则实际上执行了几个操作：

  1. 将要替换的项（参数项）先向上移位一次（$termShift 1 s$）。

  2. 然后进行替换（$termSubst 0 ...$）。

  3. 最后，对替换结果向下移位一次（$termShift (-1) ...$），以考虑到绑定变量已经被使用掉。

- 以下定义封装了这一系列步骤：

  ```ocaml
  let termSubstTop s t =
    termShift (-1) (termSubst 0 (termShift 1 s) t)
  ```

---

### 7.3 求值 (Evaluation)

与第3章一样，求值函数依赖于一个辅助谓词 $isval$：

```ocaml
let rec isval ctx t = match t with
    TmAbs(_, _, _) → true
  | _ → false
```

**解释：**

- $isval$ 函数用于检查一个项是否是一个值。

- 在无类型 Lambda 演算中，只有抽象（$TmAbs$）是值。

---

**单步求值函数 $eval1$：**

单步求值函数是求值规则的直接转录，除了我们传递一个上下文 $ctx$ 与项一起。这个参数在当前的 $eval1$ 函数中没有使用，但在后面更复杂的求值器中需要使用。

```ocaml
let rec eval1 ctx t = match t with
    TmApp(_, TmAbs(_, x, t12), v2) when isval ctx v2 →
      termSubstTop v2 t12
  | TmApp(fi, v1, t2) when isval ctx v1 →
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi, t1, t2) →
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | _ →
      raise NoRuleApplies
```

**解释：**

- **规则1**：当遇到一个应用 $TmApp$，其左侧是一个抽象 $TmAbs$，右侧是一个值 $v2$，则执行 β-约简。

  - 调用 $termSubstTop v2 t12$，将 $v2$ 替换到抽象的主体 $t12$ 中。

- **规则2**：当应用的左侧 $v1$ 是一个值（抽象），但右侧 $t2$ 还不是值时，对右侧进行求值。

  - 递归调用 $eval1 ctx t2$，得到 $t2'$。

  - 返回新的应用 $TmApp(fi, v1, t2')$。

- **规则3**：当应用的左侧 $t1$ 还不是值时，对左侧进行求值。

  - 递归调用 $eval1 ctx t1$，得到 $t1'$。

  - 返回新的应用 $TmApp(fi, t1', t2)$。

- **规则4**：如果没有规则适用，则抛出异常 $NoRuleApplies$。

---

**多步求值函数 $eval$：**

多步求值函数与之前相同，除了添加了 $ctx$ 参数：

```ocaml
let rec eval ctx t =
  try
    let t' = eval1 ctx t in
    eval ctx t'
  with NoRuleApplies →
    t
```

---

#### **习题 7.3.1 [推荐, ««« 3]：将此实现更改为使用第5.3.8节中引入的“大步”求值风格。**

**解答：**

**目标：**

- 将当前的求值实现更改为“大步”求值（也称为自然语义），其中基本的求值关系是“项 $t$ 求值到最终结果 $v$”。

**步骤：**

1. **定义大步求值函数 $eval$，返回一个值 $v$：**

   ```ocaml
   let rec eval ctx t = match t with
       TmVar(_, x, _) →
         (* 变量无法单独求值，可能需要从上下文中获取其值 *)
         raise NoRuleApplies
     | TmAbs(_, _, _) →
         t  (* 抽象是值，直接返回 *)
     | TmApp(_, t1, t2) →
         let v1 = eval ctx t1 in
         let v2 = eval ctx t2 in
         match v1 with
             TmAbs(_, _, t12) →
               eval ctx (termSubstTop v2 t12)
           | _ →
               raise NoRuleApplies
   ```

2. **解释：**

   - **TmVar**：对于变量，根据具体情况处理。如果在当前上下文中有绑定，可以进行替换；否则，抛出异常。

   - **TmAbs**：抽象是值，直接返回。

   - **TmApp**：

     - 首先递归求值 $t1$，得到 $v1$。

     - 再递归求值 $t2$，得到 $v2$。

     - 检查 $v1$ 是否为抽象 $TmAbs$。

       - 如果是，则执行替换 $termSubstTop v2 t12$，并对结果再次调用 $eval$，以确保计算达到值。

       - 如果不是，则无法应用，抛出异常。

3. **注意事项：**

   - 在大步求值中，我们需要确保参数 $t1$ 和 $t2$ 都被完全求值到值 $v1$ 和 $v2$。

   - 然后将 $v2$ 替换到 $v1$ 的主体中，并继续求值。

---

### 7.4 注释 (Notes)

**替换处理的讨论：**

- 本章中介绍的替换处理方法，虽然对于本书的目的已经足够，但远不是关于该主题的最终结论。

- 特别是，我们的求值器中的 β-约简规则“急切地”将参数值替换到函数体中的绑定变量。

---

**替代策略：**

- 为了提高速度，针对函数式语言的解释器（和编译器）使用了不同的策略：不是实际执行替换，而是简单地在一个称为 **环境 (environment)** 的辅助数据结构中记录绑定变量名和参数值之间的关联，并随同被求值的项一起携带。

- 当遇到变量时，我们在当前环境中查找其值。

---

**显式替换 (Explicit Substitutions)：**

- 这种策略可以被建模为将环境视为一种显式替换——即，将替换机制从元语言移动到对象语言，使其成为求值器操纵的项语法的一部分，而不是对项的外部操作。

- **显式替换**最初由 **Abadi、Cardelli、Curien 和 Lévy（1991a）** 研究，后来成为一个活跃的研究领域。

---

**引用：**

> **仅仅因为你实现了某件事并不意味着你理解了它。**
>
> ——Brian Cantwell Smith

---

**总结：**

- 本章构建了一个无类型 Lambda 演算的解释器，详细介绍了项的表示、移位和替换的实现，以及求值过程。

- 通过逐步解释代码和概念，我们深入理解了 Lambda 演算的实现细节。

- 习题帮助我们巩固了对大步求值的理解，并动手将求值器改为大步求值风格。

- 最后，注释部分讨论了替换处理的其他策略，以及显式替换的概念。

---

**如果您对上述内容有任何疑问或需要进一步的解释，请随时提问！**

### ---------------------------

### 第7章 **Lambda 演算的 ML 实现** (An ML Implementation of the Lambda-Calculus)

在本章中，我们将基于第4章的算术表达式解释器和第6章中关于变量绑定和替换的处理，构建一个**无类型 Lambda 演算** (untyped lambda-calculus) 的解释器。

通过将前面的定义直接翻译成 OCaml，我们可以获得一个可执行的无类型 Lambda 项 (lambda-term) 的求值器。与第4章一样，我们只展示核心算法，忽略词法分析、解析、打印等问题。

---

#### 7.1 **项和上下文** (Terms and Contexts)

我们可以通过直接翻译**定义6.1.2**，得到表示项的抽象语法树的数据类型：

```ocaml
type term =
    TmVar of int          (* 变量 (Variable) *)
  | TmAbs of term         (* 抽象 (Abstraction) *)
  | TmApp of term * term  (* 应用 (Application) *)
```

**解释：**

- **$TmVar of int$**：表示一个变量，其存储的是变量的 **de Bruijn 指数** (de Bruijn index)，即一个整数。
- **$TmAbs of term$**：表示一个抽象，包含了抽象的主体 (body)，也是一个项。
- **$TmApp of term * term$**：表示一个应用，由两个项组成，分别是函数和参数。

然而，我们在实现中实际使用的定义会包含更多的信息。

---

**改进一：**

为了记录项在源代码中的位置，我们给每个项添加一个 **信息** (info) 元素，记录该项最初出现在文件中的位置。这有助于错误处理时指向发生错误的精确位置。

```ocaml
type term =
    TmVar of info * int
  | TmAbs of info * term
  | TmApp of info * term * term
```

- **$info$**：通常是一个包含文件名、行号和列号的结构，帮助定位源代码中的位置。

---

**改进二：**

为了调试的目的，在每个变量节点上携带一个额外的数字作为一致性检查。约定是，这个第二个数字总是包含了变量出现时上下文的总长度。

```ocaml
type term =
    TmVar of info * int * int
  | TmAbs of info * term
  | TmApp of info * term * term
```

- **解释：**

  - 在 $TmVar$ 中，除了变量的 de Bruijn 索引（第一个 $int$），还包含了上下文的长度（第二个 $int$）。这有助于在打印变量时验证这个数字是否对应于当前上下文的实际大小；如果不对应，那么可能某个地方忘记了执行移位操作。

---

**改进三：**

虽然项在内部使用 de Bruijn 指数表示，但显然这不是它们应该呈现给用户的方式：我们应该在解析期间将普通表示转换为无名项，并在打印期间转换回普通形式。

为了解决打印时变量名的问题，我们在每个抽象中添加一个 **字符串** (string)，用作绑定变量名的提示。这可以确保打印的项与原始程序中的名字尽可能一致。

```ocaml
type term =
    TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term
```

- **解释：**

  - 在 $TmAbs$ 中，添加了一个字符串 $string$，作为绑定变量的名称提示。

---

**打印函数：**

打印函数用于将内部的无名项转换回用户可读的形式。函数的定义如下：

```ocaml
let rec printtm ctx t = match t with
    TmAbs(fi, x, t1) →
      let (ctx', x') = pickfreshname ctx x in
      pr "(lambda "; pr x'; pr ". "; printtm ctx' t1; pr ")"
  | TmApp(fi, t1, t2) →
      pr "("; printtm ctx t1; pr " "; printtm ctx t2; pr ")"
  | TmVar(fi, x, n) →
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr "[bad index]"
```

- **参数：**

  - $ctx$：上下文 (context)，一个字符串和绑定的列表。
  - $t$：需要打印的项。

- **辅助函数：**

  - $pr$：将字符串输出到标准输出流。
  - $ctxlength$：返回上下文的长度。
  - $index2name$：根据变量的索引，从上下文中获取变量的名称。
  - $pickfreshname$：从上下文 $ctx$ 和提示名称 $x$ 中找到一个未使用的新名称 $x'$，并返回新的上下文 $ctx'$ 和名称 $x'$。

---

**上下文和绑定：**

```ocaml
type context = (string * binding) list

type binding = NameBind
```

- **解释：**

  - $context$：上下文是一个 $(string * binding)$ 的列表，记录了变量名和其绑定信息。
  - $binding$：当前只有一种绑定类型 $NameBind$，在后续章节中会扩展。

---

**实际的打印函数：**

在实际实现中，打印函数会更复杂一些，需要考虑以下两个问题：

1. **省略多余的括号**：遵循应用左结合和抽象主体尽可能向右延伸的约定，尽可能省略括号。

2. **格式化输出**：使用 OCaml 的 $Format$ 库生成漂亮的输出，自动处理换行和缩进。

---

#### 7.2 **移位和替换** (Shifting and Substitution)

**移位** (Shifting)：

根据**定义6.2.1**，移位的操作可以直接翻译为 OCaml 代码：

```ocaml
let termShift d t =
  let rec walk c t = match t with
      TmVar(fi, x, n) →
        if x >= c then TmVar(fi, x + d, n + d)
        else TmVar(fi, x, n + d)
    | TmAbs(fi, x, t1) →
        TmAbs(fi, x, walk (c + 1) t1)
    | TmApp(fi, t1, t2) →
        TmApp(fi, walk c t1, walk c t2)
  in walk 0 t
```

- **参数：**

  - $d$：移位的大小。
  - $t$：需要移位的项。

- **内部函数 $walk$：**

  - $c$：截断值 (cutoff)，初始为 $0$。
  - **变量情况 ($TmVar$)：**

    - 如果 $x >= c$，表示变量是自由的，索引需要增加 $d$，上下文长度 $n$ 也需要增加 $d$。
    - 否则，变量是绑定的，索引和上下文长度保持不变。
  
  - **抽象情况 ($TmAbs$)：**

    - 处理抽象时，截断值 $c$ 增加 $1$，递归处理抽象的主体 $t1$。

  - **应用情况 ($TmApp$)：**

    - 对应用的两个子项 $t1$ 和 $t2$，在相同的截断值 $c$ 下递归处理。

---

**替换** (Substitution)：

根据**定义6.2.4**，替换的操作可以实现如下：

```ocaml
let termSubst j s t =
  let rec walk c t = match t with
      TmVar(fi, x, n) →
        if x = j + c then termShift c s
        else TmVar(fi, x, n)
    | TmAbs(fi, x, t1) →
        TmAbs(fi, x, walk (c + 1) t1)
    | TmApp(fi, t1, t2) →
        TmApp(fi, walk c t1, walk c t2)
  in walk 0 t
```

- **参数：**

  - $j$：需要替换的变量索引。
  - $s$：替换的项。
  - $t$：进行替换的目标项。

- **内部函数 $walk$：**

  - $c$：截断值，初始为 $0$。
  - **变量情况 ($TmVar$)：**

    - 如果 $x = j + c$，表示找到了需要替换的变量。
    - 使用 $termShift c s$ 对替换项 $s$ 进行移位，然后替换当前变量。
  
  - **其他情况**与移位操作相同。

---

**替换顶层变量：**

在 β-约简中，我们需要替换抽象的绑定变量。这可以通过以下函数实现：

```ocaml
let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)
```

- **解释：**

  - 首先对替换项 $s$ 进行向上移位 $1$，以适应进入抽象的环境。
  - 然后在目标项 $t$ 中替换索引为 $0$ 的变量（即抽象的绑定变量）。
  - 最后对结果进行向下移位 $-1$，因为抽象的绑定变量已经消耗掉，环境层次减少。

---

#### 7.3 **求值** (Evaluation)

**值的判定：**

首先，我们需要一个辅助函数来判定一个项是否是值：

```ocaml
let rec isval ctx t = match t with
    TmAbs(_, _, _) → true
  | _ → false
```

- **解释：**

  - 在无类型 Lambda 演算中，只有抽象是值。

---

**单步求值函数 $eval1$：**

```ocaml
let rec eval1 ctx t = match t with
    TmApp(_, TmAbs(_, x, t12), v2) when isval ctx v2 →
      termSubstTop v2 t12
  | TmApp(fi, v1, t2) when isval ctx v1 →
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi, t1, t2) →
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | _ →
      raise NoRuleApplies
```

- **规则解释：**

  - **β-约简 (Beta Reduction)**：

    - 当遇到形式为 $TmApp(_, TmAbs(_, x, t12), v2)$，且 $v2$ 是值时，执行替换操作 $termSubstTop v2 t12$。

  - **应用左值右递归**：

    - 当左侧是值 $v1$，但右侧 $t2$ 还不是值时，递归求值 $t2$。

  - **应用左递归**：

    - 当左侧 $t1$ 不是值时，递归求值 $t1$。

  - **无法应用规则**：

    - 如果没有规则适用，抛出异常 $NoRuleApplies$。

---

**多步求值函数 $eval$：**

```ocaml
let rec eval ctx t =
  try
    let t' = eval1 ctx t in
    eval ctx t'
  with NoRuleApplies →
    t
```

- **解释：**

  - 不断应用单步求值函数 $eval1$，直到无法继续为止，返回最终结果。

---

#### **习题 7.3.1 [推荐, ««« 3]：将此实现更改为使用“大步”求值风格。**

**解答：**

**目标：**

- 将当前的求值器修改为“大步求值” (big-step evaluation)，也称为**自然语义** (natural semantics)，直接从项求值到值。

**实现：**

```ocaml
let rec eval ctx t = match t with
    TmVar(_, x, _) →
      raise NoRuleApplies  (* 无法求值变量 *)
  | TmAbs(_, _, _) →
      t  (* 抽象是值，直接返回 *)
  | TmApp(_, t1, t2) →
      let v1 = eval ctx t1 in
      let v2 = eval ctx t2 in
      match v1 with
          TmAbs(_, _, t12) →
            eval ctx (termSubstTop v2 t12)
        | _ →
            raise NoRuleApplies
```

**解释：**

- **变量 ($TmVar$)**：

  - 在无类型 Lambda 演算中，变量无法进一步求值，抛出异常。

- **抽象 ($TmAbs$)**：

  - 抽象是值，直接返回。

- **应用 ($TmApp$)**：

  - 递归求值函数部分 $t1$，得到 $v1$。
  - 递归求值参数部分 $t2$，得到 $v2$。
  - 如果 $v1$ 是抽象，则执行替换 $termSubstTop v2 t12$，并对结果继续求值。
  - 否则，无法应用，抛出异常。

---

#### 7.4 **注释** (Notes)

**替换策略的讨论：**

- 本章的替换处理方法虽然足以满足本书的需求，但在实际中可能并非最佳方案。
- 我们的求值器在 β-约简时“急切地”将参数值替换到函数体中。
- 在追求性能的函数式语言的解释器和编译器中，通常采用不同的策略。

**环境模型 (Environment Model)：**

- 不是直接进行替换，而是维护一个**环境** (environment)，记录绑定变量和对应的值。
- 当遇到变量时，从当前环境中查找其值。
- 这种策略可以避免频繁的替换操作，提高性能。

**显式替换 (Explicit Substitutions)：**

- 将替换机制显式化，成为项语法的一部分，而不是元语言的外部操作。
- **显式替换**最初由 Abadi、Cardelli、Curien 和 Lévy 在 1991 年提出，已成为一个活跃的研究领域。

---

**引用：**

> **仅仅因为你实现了某件事，并不意味着你理解了它。**
>
> —— Brian Cantwell Smith

---

**总结：**

- 本章通过详细的代码实现，展示了如何在 OCaml 中构建一个无类型 Lambda 演算的解释器。
- 我们深入探讨了项的表示、移位和替换的具体实现，以及求值过程。
- 习题帮助我们理解了大步求值的概念，并亲手将求值器修改为大步求值的风格。
- 最后，注释部分讨论了替换策略的改进方向和相关研究。

---

**如果对上述内容有任何疑问，欢迎进一步探讨！**

### ----------------------------

### 第7章 **Lambda 演算的 ML 实现** (An ML Implementation of the Lambda-Calculus)

在本章中，我们将基于之前的算术表达式解释器（第4章）和变量绑定与替换的处理（第6章），构建一个**无类型 Lambda 演算** (untyped lambda-calculus) 的解释器。

**无类型 Lambda 演算**（Untyped Lambda-Calculus）是一种简单但强大的计算模型，它只包含函数抽象和应用，没有类型系统。它是编程语言理论和函数式编程语言的基础。

---

#### 7.1 **项和上下文** (Terms and Contexts)

首先，我们需要在 OCaml 中表示 Lambda 项。根据第6章中**定义6.1.2**，我们可以将 Lambda 项的抽象语法直接翻译为 OCaml 的数据类型。

**定义6.1.2（项的定义）：**

$$
\begin{align*}
t ::= & \quad x & \text{变量 (Variable)} \\
      & \mid \lambda x.\ t & \text{抽象 (Abstraction)} \\
      & \mid t\ t & \text{应用 (Application)}
\end{align*}
$$

在 OCaml 中，我们可以将上述定义表示为以下数据类型：

```ocaml
type term =
    TmVar of int          (* 变量 Variable *)
  | TmAbs of term         (* 抽象 Abstraction *)
  | TmApp of term * term  (* 应用 Application *)
```

**解释：**

- **$TmVar of int$**：表示一个变量，用一个整数表示其 **de Bruijn 指数** (de Bruijn index)，即变量在绑定层次中的位置。
- **$TmAbs of term$**：表示一个抽象，包含一个项作为函数的主体。
- **$TmApp of term * term$**：表示一个应用，包含两个项，分别是函数和参数。

**de Bruijn 指数**（de Bruijn index）：一种无名表示法，用自然数表示变量的位置，消除了变量名引起的歧义和复杂性。

---

然而，为了在实际实现中更有效地处理错误和调试，我们需要在项中加入更多的信息。

**改进一：添加位置信息**

为了在出现错误时能够准确定位，我们给每个项添加一个 **信息**（info）字段，记录该项在源代码中的位置。

```ocaml
type term =
    TmVar of info * int
  | TmAbs of info * term
  | TmApp of info * term * term
```

- **$info$**：通常是一个包含文件名、行号和列号的结构，用于在错误发生时定位源代码的位置。

---

**改进二：添加上下文长度**

为了调试的目的，我们在每个变量节点上携带一个额外的整数，表示变量所在的上下文长度。这样，我们可以在打印变量时验证这个数字是否对应于当前上下文的实际大小；如果不对应，那么可能某个地方忘记了执行移位操作。

```ocaml
type term =
    TmVar of info * int * int
  | TmAbs of info * term
  | TmApp of info * term * term
```

- **$TmVar of info * int * int$**：
  - 第一个 $int$ 是变量的 de Bruijn 指数。
  - 第二个 $int$ 是上下文的长度。

---

**改进三：添加变量名提示**

虽然内部使用 de Bruijn 指数表示项，但为了在打印时能够显示友好的变量名，我们在抽象中添加一个字符串作为变量名的提示。

```ocaml
type term =
    TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term
```

- **$TmAbs of info * string * term$**：
  - $string$ 是变量名的提示，帮助在打印时生成更可读的输出。

---

**打印函数**

为了将内部的项转换为用户可读的形式，我们需要一个打印函数。这个函数会使用上下文来跟踪变量名，并在需要时生成新鲜的变量名。

```ocaml
let rec printtm ctx t =
  match t with
  | TmAbs(fi, x, t1) ->
      let (ctx', x') = pickfreshname ctx x in
      print_string "(lambda ";
      print_string x';
      print_string ". ";
      printtm ctx' t1;
      print_string ")"
  | TmApp(fi, t1, t2) ->
      print_string "(";
      printtm ctx t1;
      print_string " ";
      printtm ctx t2;
      print_string ")"
  | TmVar(fi, x, n) ->
      if ctxlength ctx = n then
        print_string (index2name fi ctx x)
      else
        print_string "[bad index]"
```

**解释：**

- **$ctx$**：上下文 (context)，一个字符串和绑定的列表，用于记录变量名和其绑定信息。
- **$TmAbs$**：处理抽象时，使用 $pickfreshname$ 函数从上下文中选择一个未被使用的变量名 $x'$，并将其添加到新的上下文 $ctx'$ 中。
- **$TmApp$**：处理应用时，递归打印函数和参数。
- **$TmVar$**：处理变量时，检查上下文的长度是否与 $n$ 匹配，如果匹配，则从上下文中获取变量名打印，否则打印 $[bad index]$。

**辅助函数：**

- **$pickfreshname$**：从上下文中选择一个未被使用的变量名。
- **$ctxlength$**：返回上下文的长度。
- **$index2name$**：根据变量的索引，从上下文中获取变量名。

**上下文和绑定的定义：**

```ocaml
type context = (string * binding) list

type binding = NameBind
```

- **$context$**：上下文是一个列表，每个元素是一个字符串和绑定的二元组。
- **$binding$**：目前只有一种绑定类型 $NameBind$，表示名称绑定。

---

#### 7.2 **移位和替换** (Shifting and Substitution)

为了正确处理变量的作用域和替换，我们需要实现移位和替换操作。

---

**移位操作**（Shifting）

移位操作用于调整项中变量的 de Bruijn 指数，以适应上下文的变化，特别是在替换操作中。

**移位的定义：**

$$
\begin{align*}
\uparrow^d_c(k) &= \begin{cases}
k, & \text{如果 } k < c \\
k + d, & \text{如果 } k \geq c
\end{cases} \\
\uparrow^d_c(\lambda.\ t_1) &= \lambda.\ \uparrow^d_{c+1}(t_1) \\
\uparrow^d_c(t_1\ t_2) &= \uparrow^d_c(t_1)\ \uparrow^d_c(t_2)
\end{align*}
$$

**符号解释：**

- **$\uparrow^d_c(k)$**：对变量索引 $k$ 进行移位。
  - $d$：移位的大小。
  - $c$：截断值 (cutoff)，表示从哪个绑定层次开始进行移位。
- **$\lambda.\ t_1$**：处理抽象时，截断值 $c$ 增加 1，因为进入了新的绑定层次。
- **$t_1\ t_2$**：对应用的两个子项分别进行移位。

**OCaml 实现：**

```ocaml
let termShift d t =
  let rec walk c t = match t with
    | TmVar(fi, x, n) ->
        if x >= c then TmVar(fi, x + d, n + d)
        else TmVar(fi, x, n + d)
    | TmAbs(fi, x, t1) ->
        TmAbs(fi, x, walk (c + 1) t1)
    | TmApp(fi, t1, t2) ->
        TmApp(fi, walk c t1, walk c t2)
  in walk 0 t
```

**解释：**

- **$d$**：移位的大小。
- **$walk c t$**：内部递归函数，$c$ 是截断值。
  - **$TmVar$**：
    - 如果变量的索引 $x >= c$，则表示是自由变量，需要移位，索引增加 $d$。
    - 否则，表示是绑定变量，不需要移位，索引保持不变。
    - 上下文长度 $n$ 始终增加 $d$，以反映上下文的变化。
  - **$TmAbs$**：
    - 处理抽象时，截断值 $c$ 增加 1，递归处理主体 $t1$。
  - **$TmApp$**：
    - 对应用的两个子项 $t1$ 和 $t2$，在相同的截断值 $c$ 下递归处理。

---

**替换操作**（Substitution）

替换操作用于在项中用另一个项替换某个变量，同时需要正确处理绑定和作用域。

**替换的定义：**

$$
\begin{align*}
[j \mapsto s]k &= \begin{cases}
s, & \text{如果 } k = j \\
k, & \text{否则}
\end{cases} \\
[j \mapsto s](\lambda.\ t_1) &= \lambda.\ [j + 1 \mapsto \uparrow^1(s)]t_1 \\
[j \mapsto s](t_1\ t_2) &= [j \mapsto s]t_1\ [j \mapsto s]t_2
\end{align*}
$$

**符号解释：**

- **$[j \mapsto s]k$**：在变量 $k$ 中替换。
  - 如果 $k = j$，则用替换项 $s$（经过适当移位）替换。
  - 否则，变量保持不变。
- **$[j \mapsto s](\lambda.\ t_1)$**：处理抽象时，替换索引 $j$ 增加 1，替换项 $s$ 需要向上移位 $\uparrow^1(s)$。
- **$[j \mapsto s](t_1\ t_2)$**：对应用的两个子项分别进行替换。

**OCaml 实现：**

```ocaml
let termSubst j s t =
  let rec walk c t = match t with
    | TmVar(fi, x, n) ->
        if x = j + c then termShift c s
        else TmVar(fi, x, n)
    | TmAbs(fi, x, t1) ->
        TmAbs(fi, x, walk (c + 1) t1)
    | TmApp(fi, t1, t2) ->
        TmApp(fi, walk c t1, walk c t2)
  in walk 0 t
```

**解释：**

- **$j$**：需要替换的变量索引。
- **$s$**：替换项。
- **$walk c t$**：内部递归函数，$c$ 是截断值。
  - **$TmVar$**：
    - 如果变量的索引 $x = j + c$，则在当前位置需要替换，使用 $termShift c s$ 对替换项进行移位后替换。
    - 否则，变量保持不变。
  - **$TmAbs$**：
    - 处理抽象时，截断值 $c$ 增加 1，递归处理主体 $t1$。
  - **$TmApp$**：
    - 对应用的两个子项 $t1$ 和 $t2$，在相同的截断值 $c$ 下递归处理。

---

**顶层替换**

在 β-约简中，我们需要替换抽象的绑定变量，可以定义一个函数封装这一系列步骤。

**顶层替换的定义：**

$$
\text{termSubstTop}(s, t) = \uparrow^{-1}([0 \mapsto \uparrow^1(s)]t)
$$

**符号解释：**

- **$\uparrow^1(s)$**：对替换项 $s$ 进行向上移位，以适应进入新的绑定层次。
- **$[0 \mapsto \uparrow^1(s)]t$**：在项 $t$ 中，用移位后的 $s$ 替换变量索引为 $0$ 的变量。
- **$\uparrow^{-1}(\cdot)$**：对整个结果进行向下移位，以调整绑定层次，因为抽象的绑定变量已经被消耗掉。

**OCaml 实现：**

```ocaml
let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)
```

**解释：**

- 首先对替换项 $s$ 进行移位 $termShift 1 s$。
- 然后在 $t$ 中用 $s$ 替换索引为 $0$ 的变量：$termSubst 0 (termShift 1 s) t$。
- 最后对结果进行移位 $termShift (-1)$。

---

#### 7.3 **求值** (Evaluation)

为了执行 Lambda 项的计算，我们需要定义求值规则。

---

**值的定义**

在无类型 Lambda 演算中，只有抽象是值。

```ocaml
let rec isval ctx t = match t with
  | TmAbs(_, _, _) -> true
  | _ -> false
```

---

**单步求值函数**

根据 Lambda 演算的操作语义，定义单步求值函数 $eval1$。

**求值规则：**

1. **β-约简**（Beta Reduction）：

   $$
   \dfrac{}{(\lambda x.\ t_{12})\ v_2 \longrightarrow [x \mapsto v_2]t_{12}}
   $$

   - 当应用的左侧是抽象，右侧是值时，进行替换。

2. **应用左值右递归**：

   $$
   \dfrac{t_2 \longrightarrow t_2'}{v_1\ t_2 \longrightarrow v_1\ t_2'}
   $$

   - 当左侧是值，右侧可以进一步求值时，递归求值右侧。

3. **应用左递归**：

   $$
   \dfrac{t_1 \longrightarrow t_1'}{t_1\ t_2 \longrightarrow t_1'\ t_2}
   $$

   - 当左侧可以进一步求值时，递归求值左侧。

**OCaml 实现：**

```ocaml
let rec eval1 ctx t = match t with
  | TmApp(_, TmAbs(_, x, t12), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | _ ->
      raise NoRuleApplies
```

**解释：**

- **规则1**：
  - 当项是 $TmApp$，左侧是 $TmAbs$（抽象），右侧是值时，进行替换操作。
- **规则2**：
  - 当左侧是值，右侧可以进一步求值时，递归求值右侧。
- **规则3**：
  - 当左侧可以进一步求值时，递归求值左侧。
- **无法匹配规则**：
  - 如果没有规则适用，抛出异常 $NoRuleApplies$。

---

**多步求值函数**

使用递归方式，不断应用单步求值，直到无法继续。

```ocaml
let rec eval ctx t =
  try
    let t' = eval1 ctx t in
    eval ctx t'
  with NoRuleApplies ->
    t
```

---

**习题 7.3.1 [推荐, ««« 3]**

**题目：**

将此实现修改为使用“大步”求值（big-step evaluation）的风格。

**解答：**

**目标：**

- 将求值器改为大步求值，即一次性将项求值到值，而不是逐步求值。

**大步求值规则：**

1. **值规则**：

   $$
   \dfrac{}{\Gamma \vdash v \Downarrow v}
   $$

   - 如果项 $v$ 是值，则求值结果就是 $v$。

2. **应用规则**：

   $$
   \dfrac{\Gamma \vdash t_1 \Downarrow \lambda x.\ t_{12} \quad \Gamma \vdash t_2 \Downarrow v_2 \quad \Gamma \vdash [x \mapsto v_2]t_{12} \Downarrow v}{\Gamma \vdash t_1\ t_2 \Downarrow v}
   $$

   - 首先求值 $t_1$ 到一个抽象 $\lambda x.\ t_{12}$。
   - 然后求值 $t_2$ 到值 $v_2$。
   - 接着在 $t_{12}$ 中用 $v_2$ 替换 $x$，求值得到 $v$。
   - 最终，应用 $t_1\ t_2$ 求值到 $v$。

**OCaml 实现：**

```ocaml
let rec eval ctx t = match t with
  | TmVar(_, x, _) ->
      raise NoRuleApplies  (* 变量无法求值 *)
  | TmAbs(_, _, _) ->
      t  (* 抽象是值，直接返回 *)
  | TmApp(_, t1, t2) ->
      let v1 = eval ctx t1 in
      let v2 = eval ctx t2 in
      match v1 with
      | TmAbs(_, x, t12) ->
          let t' = termSubstTop v2 t12 in
          eval ctx t'
      | _ ->
          raise NoRuleApplies
```

**解释：**

- **$TmVar$**：
  - 变量无法求值，抛出异常。
- **$TmAbs$**：
  - 抽象是值，直接返回。
- **$TmApp$**：
  - 首先递归求值 $t1$，得到 $v1$。
  - 然后递归求值 $t2$，得到 $v2$。
  - 如果 $v1$ 是抽象，则进行替换，并递归求值替换后的结果。
  - 如果 $v1$ 不是抽象，无法继续求值，抛出异常。

---

#### 7.4 **注释** (Notes)

本章中，我们实现了一个无类型 Lambda 演算的解释器。然而，在实际的编程语言实现中，对于替换的处理可能会采用不同的策略，以提高性能。

---

**替换策略的讨论：**

- **急切替换**（Eager Substitution）：
  - 在我们的实现中，β-约简规则立即将参数值替换到函数体中。
- **惰性替换**（Lazy Substitution）：
  - 在一些编程语言的解释器或编译器中，替换操作可能会被推迟，以避免不必要的计算。

---

**环境模型（Environment Model）：**

- **环境**（Environment）：
  - 一个记录变量绑定的结构，保存变量名和对应的值。
- **策略**：
  - 在求值过程中，不直接进行替换，而是将变量和其值的关联记录在环境中。
  - 当遇到变量时，从当前环境中查找其值。

**优点：**

- 避免频繁的替换操作，提高求值效率。
- 更容易实现共享和惰性计算。

---

**显式替换（Explicit Substitutions）：**

- **概念**：
  - 将替换操作从元语言移动到对象语言，使其成为项语法的一部分。
- **优点**：
  - 提供对替换过程的更精细控制。
  - 可以更好地分析和优化替换操作。

**相关研究：**

- **Abadi、Cardelli、Curien 和 Lévy（1991）** 首先研究了显式替换的概念。
- 显式替换已经成为编程语言理论中的一个活跃研究领域。

---

**引用：**

> **仅仅因为你实现了某件事，并不意味着你理解了它。**
>
> —— Brian Cantwell Smith

---

### **总结**

- 我们详细介绍了如何在 OCaml 中实现无类型 Lambda 演算的解释器，包括项的表示、移位和替换操作，以及求值过程。
- 通过逐步构建项的数据结构，我们理解了如何在实际编程语言中表示 Lambda 项。
- 实现了移位和替换操作，加深了对变量绑定和作用域的理解。
- 通过习题，将求值器改为大步求值的风格，理解了不同的求值策略。
- 讨论了替换策略的不同实现方式，以及在实际编程语言实现中的应用。

---

**如果对上述内容有任何疑问，欢迎进一步探讨！**

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