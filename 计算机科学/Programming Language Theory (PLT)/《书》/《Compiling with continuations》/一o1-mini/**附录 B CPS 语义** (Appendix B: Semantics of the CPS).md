[toc]



好的，我将**逐步详解**您提供的《Compiling with Continuations》第十八章附录**“B CPS的语义（Semantics of the CPS）”**中的内容。这部分主要涉及续延传递风格（Continuation-Passing Style, CPS）语言的语义定义，通过一个ML函数式模块（functor）实现了CPS语言的评估机制。以下将详细解释该附录中的代码及其背后的概念，帮助您深入理解CPS的语义实现。

---
## **附录 B CPS的语义（Semantics of the CPS）**

### **概述**

本附录定义了CPS语言的语义，通过一个ML functor `CPSsemantics` 实现。该functor接收一组结构和函数作为参数，返回一个具有评估功能的模块。以下是主要内容概述：

1. **模块参数**：定义了CPS语言所需的基本结构和函数，如类型、操作符、异常等。
2. **内部实现**：
   - **存储管理**：实现了对存储（store）的读取和更新操作。
   - **值比较**：实现了复杂数据类型的相等性检查。
   - **异常处理**：处理溢出和除零异常。
   - **原始操作评估**：实现了对各种原始操作符（如加法、减法、比较等）的评估逻辑。
   - **表达式评估**：实现了CPS表达式的递归评估，包括选择、偏移、函数应用、记录构造、条件分支、原始操作、递归等。

### **1. functor CPSsemantics**

```sml
functor CPSsemantics(
  structure CPS: CPS
  val minint: int
  val maxint: int
  val minreal: real
  val maxreal: real
  val string2real: string -> real
  eqtype loc
  val nextloc: loc -> loc
  val arbitrarily: ’a * ’a -> ’a
  type answer
  datatype dvalue = RECORD of dvalue list * int
                | INT of int
                | REAL of real
                | FUNC of (dvalue list -> (loc * (loc -> dvalue) * (loc -> int)) -> answer)
                | STRING of string
                | BYTEARRAY of loc list
                | ARRAY of loc list
                | UARRAY of loc list
  val handler_ref : loc
  val overflow_exn : dvalue
  val div_exn : dvalue
) : sig
  val eval: CPS.var list * CPS.cexp -> dvalue list -> (loc * (loc -> dvalue) * (loc -> int)) -> answer
end = struct
  ...
end
```

#### **解释**

- **functor CPSsemantics**：定义了一个functor（函数式模块），它接受一组结构和函数作为参数，返回一个包含评估功能的模块。
  
- **参数列表**：
  - `structure CPS: CPS`：接收一个符合`CPS`签名的结构，包含CPS语言的基本定义。
  - `val minint, maxint`：定义整数类型的最小值和最大值，用于溢出检测。
  - `val minreal, maxreal`：定义实数类型的最小值和最大值。
  - `val string2real: string -> real`：将字符串转换为实数的函数。
  - `eqtype loc`：定义位置类型，用于内存管理（如堆地址）。
  - `val nextloc: loc -> loc`：生成下一个存储位置的函数。
  - `val arbitrarily: 'a * 'a -> 'a`：一个函数，随机选择元组中的一个值，常用于处理非确定性选择。
  - `type answer`：定义评估结果的类型。
  - `datatype dvalue`：定义了CPS语言中的值类型，包括记录、整数、实数、函数、字符串、字节数组、数组、不可变数组。
  - `val handler_ref : loc`：异常处理程序的引用位置。
  - `val overflow_exn, div_exn : dvalue`：定义溢出和除零异常。

- **返回签名**：
  - `val eval`：一个评估函数，接受变量列表、CPS表达式、值列表和存储状态，返回评估结果。

### **2. 内部实现**

#### **2.1 类型定义**

```sml
type store = loc * (loc -> dvalue) * (loc -> int)
```

- **store**：定义存储状态为一个三元组，包括当前位置`loc`，一个映射函数`f`从位置到值，以及一个映射函数`g`从位置到整数（用于不可变数组）。

#### **2.2 存储操作函数**

```sml
fun fetch ((_,f,_): store) (l: loc) = f l
fun upd ((n,f,g): store, l: loc, v: dvalue) =
  (n, fn i => if i = l then v else f i, g)
fun fetchi ((_,_,g): store) (l: loc) = g l
fun updi ((n,f,g): store, l: loc, v: int) =
  (n, f, fn i => if i = l then v else g i)
```

- **fetch**：从存储中获取位置`l`处的值。
- **upd**：更新存储中位置`l`的值为`v`，返回新的存储状态。
- **fetchi**：获取位置`l`处的整数值（用于不可变数组）。
- **updi**：更新位置`l`的整数值为`v`，返回新的存储状态。

#### **2.3 值比较函数**

```sml
exception Undefined

fun eq(RECORD(a,i), RECORD(b,j)) =
  arbitrarily (i = j andalso eqlist(a, b), false)
| eq(INT i, INT j) = i = j
| eq(REAL a, REAL b) = arbitrarily (a = b, false)
| eq(STRING a, STRING b) = arbitrarily (a = b, false)
| eq(BYTEARRAY nil, BYTEARRAY nil) = true
| eq(BYTEARRAY(a::_), BYTEARRAY(b::_)) = a = b
| eq(ARRAY nil, ARRAY nil) = true
| eq(ARRAY(a::_), ARRAY(b::_)) = a = b
| eq(UARRAY nil, UARRAY nil) = true
| eq(UARRAY(a::_), UARRAY(b::_)) = a = b
| eq(FUNC a, FUNC b) = raise Undefined
| eq(_, _) = false
and eqlist(a::al, b::bl) = eq(a, b) andalso eqlist(al, bl)
| eqlist(nil, nil) = true
```

- **eq**：比较两个`dvalue`是否相等。对于复杂类型（如记录、字节数组、数组、不可变数组），使用递归比较每个元素。
  - `arbitrarily`用于处理非确定性选择，如果条件满足则返回`i = j andalso eqlist(a, b)`，否则返回`false`。
  - 对于函数类型（`FUNC`），无法比较，抛出`Undefined`异常。
  - 默认情况下，若前面模式都不匹配，返回`false`。

- **eqlist**：递归比较两个列表中的每个元素是否相等。

#### **2.4 异常处理函数**

```sml
fun do_raise exn s = 
  let 
    val FUNC f = fetch s handler_ref
  in 
    f [exn] s
  end
```

- **do_raise**：抛出异常`exn`，通过调用异常处理程序函数`f`，传递异常和当前存储状态`s`。

#### **2.5 溢出处理函数**

```sml
fun overflow(n: unit -> int, c: dvalue list -> store -> answer) =
  if (n() >= minint andalso n() <= maxint)
  handle Overflow => false
  then c [INT(n())]
  else do_raise overflow_exn

fun overflowr(n, c) =
  if (n() >= minreal andalso n() <= maxreal)
  handle Overflow => false
  then c [REAL(n())]
  else do_raise overflow_exn
```

- **overflow**：处理整数溢出。如果计算结果在`minint`和`maxint`之间，调用回调函数`c`，传递结果`INT(n())`；否则，抛出`overflow_exn`异常。
- **overflowr**：类似于`overflow`，但用于处理实数溢出，返回`REAL(n())`。

#### **2.6 原始操作评估函数**

```sml
fun evalprim (CPS.+ : CPS.primop, [INT i, INT j], [c]) =
  overflow(fn()=>i+j, c)
| evalprim (CPS.-, [INT i, INT j], [c]) =
  overflow(fn()=>i-j, c)
| evalprim (CPS.*, [INT i, INT j], [c]) =
  overflow(fn()=>i*j, c)
| evalprim (CPS.div, [INT i, INT 0], [c]) = do_raise div_exn
| evalprim (CPS.div, [INT i, INT j], [c]) =
  overflow(fn()=>i div j, c)
| evalprim (CPS.~, [INT i], [c]) = overflow(fn()=>0 - i, c)
| evalprim (CPS.<, [INT i, INT j], [t, f]) =
  if i < j then t[] else f[]
| evalprim (CPS.<=, [INT i, INT j], [t, f]) =
  if j < i then f[] else t[]
| evalprim (CPS.>, [INT i, INT j], [t, f]) =
  if j < i then t[] else f[]
| evalprim (CPS.>=, [INT i, INT j], [t, f]) =
  if i < j then f[] else t[]
| evalprim (CPS.ieql, [a, b], [t, f]) =
  if eq(a, b) then t[] else f[]
| evalprim (CPS.ineq, [a, b], [t, f]) =
  if eq(a, b) then f[] else t[]
| evalprim (CPS.rangechk, [INT i, INT j], [t, f]) =
  if j < 0
  then if i < j then t[] else f[]
  else if i < 0
  then f[] else if i < j then t[] else f[]
| evalprim (CPS.boxed, [INT _], [t, f]) = f[]
| evalprim (CPS.boxed, [RECORD _], [t, f]) = t[]
| evalprim (CPS.boxed, [STRING _], [t, f]) = t[]
| evalprim (CPS.boxed, [ARRAY _], [t, f]) = t[]
| evalprim (CPS.boxed, [UARRAY _], [t, f]) = t[]
| evalprim (CPS.boxed, [BYTEARRAY _], [t, f]) = t[]
| evalprim (CPS.boxed, [FUNC _], [t, f]) = t[]
| evalprim (CPS.!, [a], [c]) =
  evalprim(CPS.subscript, [a, INT 0], [c])
| evalprim (CPS.subscript, [ARRAY a, INT n], [c]) =
  (fn s => c [fetch s (nth(a, n))] s)
| evalprim (CPS.subscript, [UARRAY a, INT n], [c]) =
  (fn s => c [INT(fetchi s (nth(a, n)))] s)
| evalprim (CPS.subscript, [RECORD(a, i), INT j], [c]) =
  c [nth(a, i + j)]
| evalprim (CPS.ordof, [STRING a, INT i], [c]) =
  c [INT(String.ordof(a, i))]
| evalprim (CPS.ordof, [BYTEARRAY a, INT i], [c]) =
  (fn s => c [INT(fetchi s (nth(a, i)))] s)
| evalprim (CPS.:=, [a, v], [c]) =
  evalprim(CPS.update, [a, INT 0, v], [c])
| evalprim (CPS.update, [ARRAY a, INT n, v], [c]) =
  (fn s => c [] (upd(s, nth(a, n), v)))
| evalprim (CPS.update, [UARRAY a, INT n, INT v], [c]) =
  (fn s => c [] (updi(s, nth(a, n), v)))
| evalprim (CPS.unboxedassign, [a, v], [c]) =
  evalprim(CPS.unboxedupdate, [a, INT 0, v], [c])
| evalprim (CPS.unboxedupdate, [ARRAY a, INT n, INT v], [c]) =
  (fn s => c [] (upd(s, nth(a, n), INT v)))
| evalprim (CPS.unboxedupdate, [UARRAY a, INT n, INT v], [c]) =
  (fn s => c [] (updi(s, nth(a, n), INT v)))
| evalprim (CPS.store, [BYTEARRAY a, INT i, INT v], [c]) =
  if v < 0 orelse v >= 256
  then raise Undefined
  else (fn s => c [] (updi(s, nth(a, i), v)))
| evalprim (CPS.makeref, [v], [c]) = 
  (fn (l, f, g) =>
    c [ARRAY[l]] (upd((nextloc l, f, g), l, v)))
| evalprim (CPS.makerefunboxed, [INT v], [c]) =
  (fn (l, f, g) =>
    c [UARRAY[l]] (updi((nextloc l, f, g), l, v)))
| evalprim (CPS.alength, [ARRAY a], [c]) =
  c [INT(List.length a)]
| evalprim (CPS.alength, [UARRAY a], [c]) =
  c [INT(List.length a)]
| evalprim (CPS.slength, [BYTEARRAY a], [c]) =
  c [INT(List.length a)]
| evalprim (CPS.slength, [STRING a], [c]) =
  c [INT(String.size a)]
| evalprim (CPS.gethdlr, [], [c]) =
  (fn s => c [fetch s handler_ref] s)
| evalprim (CPS.sethdlr, [h], [c]) =
  (fn s => c [] (upd(s, handler_ref, h)))
| evalprim (CPS.fadd, [REAL a, REAL b], [c]) =
  overflowr(fn()=>a + b, c)
| evalprim (CPS.fsub, [REAL a, REAL b], [c]) =
  overflowr(fn()=>a - b, c)
| evalprim (CPS.fmul, [REAL a, REAL b], [c]) =
  overflowr(fn()=>a * b, c)
| evalprim (CPS.fdiv, [REAL a, REAL 0.0], [c]) =
  do_raise div_exn
| evalprim (CPS.fdiv, [REAL a, REAL b], [c]) =
  overflowr(fn()=>a / b, c)
| evalprim (CPS.feql, [REAL a, REAL b], [t, f]) =
  if a = b then t[] else f[]
| evalprim (CPS.fneq, [REAL a, REAL b], [t, f]) =
  if a = b then f[] else t[]
| evalprim (CPS.flt, [REAL i, REAL j], [t, f]) =
  if i < j then t[] else f[]
| evalprim (CPS.fle, [REAL i, REAL j], [t, f]) =
  if j < i then f[] else t[]
| evalprim (CPS.fgt, [REAL i, REAL j], [t, f]) =
  if j < i then t[] else f[]
| evalprim (CPS.fge, [REAL i, REAL j], [t, f]) =
  if i < j then f[] else t[]
```

#### **解释**

- **evalprim**：处理CPS语言中的原始操作符（primop）的评估逻辑。根据操作符和参数，执行相应的操作，并调用回调函数`c`返回结果或处理异常。

- **具体操作**：
  - **整数运算**：
    - `CPS.+`, `CPS.-`, `CPS.*`：执行加、减、乘运算，调用`overflow`函数处理溢出。
    - `CPS.div`：处理整数除法，处理除零异常和溢出。
    - `CPS.~`：处理取反运算，调用`overflow`。
  
  - **比较运算**：
    - `CPS.<`, `CPS.<=`, `CPS.>`, `CPS.>=`：执行整数比较，根据结果调用`then`或`else`回调函数。
    - `CPS.ieql`, `CPS.ineq`：执行值的相等性比较，调用`eq`函数。
  
  - **范围检查**：
    - `CPS.rangechk`：执行范围检查，根据条件调用`then`或`else`回调。
  
  - **构造函数判断**：
    - `CPS.boxed`：判断值是否为构造函数类型，根据结果调用`then`或`else`回调。
  
  - **数组操作**：
    - `CPS.!`：数组取首元素，调用`CPS.subscript`。
    - `CPS.subscript`：根据数组类型（ARRAY, UARRAY, RECORD）执行不同的取值操作。
  
  - **字符操作**：
    - `CPS.ordof`：获取字符串或字节数组中某位置的字符或字节的整数表示。
  
  - **赋值操作**：
    - `CPS.:=`：调用`CPS.update`进行赋值。
    - `CPS.update`：更新ARRAY或UARRAY中的值，调用`upd`或`updi`。
    - `CPS.unboxedassign`, `CPS.unboxedupdate`：处理不可变数组的赋值。
  
  - **存储操作**：
    - `CPS.store`：在BYTEARRAY中存储一个字节，检查范围是否在0-255之间。
  
  - **引用操作**：
    - `CPS.makeref`, `CPS.makerefunboxed`：创建新的ARRAY或UARRAY引用，分配新的存储位置。
  
  - **长度操作**：
    - `CPS.alength`, `CPS.slength`：获取ARRAY, UARRAY, BYTEARRAY, STRING的长度。
  
  - **异常处理**：
    - `CPS.gethdlr`, `CPS.sethdlr`：获取和设置异常处理程序。
  
  - **浮点运算**：
    - `CPS.fadd`, `CPS.fsub`, `CPS.fmul`, `CPS.fdiv`：执行浮点数的加、减、乘、除运算，处理溢出和除零异常。
    - `CPS.feql`, `CPS.fneq`, `CPS.flt`, `CPS.fle`, `CPS.fgt`, `CPS.fge`：执行浮点数的相等、不等、小于、小于等于、大于、大于等于比较。

### **2.7 辅助函数**

#### **2.7.1 绑定函数**

```sml
type env = CPS.var -> dvalue

fun V env (CPS.INT i) = INT i
| V env (CPS.REAL r) = REAL(string2real r)
| V env (CPS.STRING s) = STRING s
| V env (CPS.VAR v) = env v
| V env (CPS.LABEL v) = env v

fun bind(env: env, v: CPS.var, d) =
  fn w => if v = w then d else env w

fun bindn(env, v::vl, d::dl) =
  bindn(bind(env, v, d), vl, dl)
| bindn(env, nil, nil) = env

fun F (x, CPS.OFFp 0) = x
| F (RECORD(l, i), CPS.OFFp j) = RECORD(l, i + j)
| F (RECORD(l, i), CPS.SELp(j, p)) = F(nth(l, i + j), p)
```

- **V**：将CPS表达式的值转换为`dvalue`。处理不同类型的CPS值（INT, REAL, STRING, VAR, LABEL）。
  
- **bind**：创建一个新的环境，将变量`v`绑定到值`d`。如果查询的变量`w`等于`v`，返回`d`，否则调用原环境`env`。

- **bindn**：批量绑定变量和值，递归调用`bind`函数。

- **F**：辅助函数，用于处理记录中的偏移操作。
  - `CPS.OFFp 0`：返回记录`x`本身。
  - `CPS.OFFp j`：在记录`l`中偏移`i + j`。
  - `CPS.SELp(j, p)`：进一步选择记录中的第`j`个字段，并递归调用`F`。

#### **2.7.2 表达式评估函数**

```sml
fun E (CPS.SELECT(i, v, w, e)) env =
  let 
    val RECORD(l, j) = V env v
  in 
    E e (bind(env, w, nth(l, i + j)))
  end
| E (CPS.OFFSET(i, v, w, e)) env =
  let 
    val RECORD(l, j) = V env v
  in 
    E e (bind(env, w, RECORD(l, i + j)))
  end
| E (CPS.APP(f, vl)) env =
  let 
    val FUNC g = V env f
  in 
    g (map (V env) vl)
  end
| E (CPS.RECORD(vl, w, e)) env =
  E e (bind(env, w, RECORD(map (fn (x, p) => F(V env x, p)) vl, 0)))
| E (CPS.SWITCH(v, el)) env =
  let 
    val INT i = V env v
  in 
    E (nth(el, i)) env
  end
| E (CPS.PRIMOP(p, vl, wl, el)) env =
  evalprim(p,
    map (V env) vl,
    map (fn e => fn al => E e (bindn(env, wl, al))) el)
| E (CPS.FIX(fl, e)) env =
  let 
    fun h r1 (f, vl, b) =
      FUNC(fn al => E b (bindn(g r1, vl, al)))
    and g r = bindn(r, map #1 fl, map (h r) fl)
  in 
    E e (g env)
  end
```

- **E**：递归函数，用于评估CPS表达式。
  
  - **CPS.SELECT**：
    - 从记录`v`中选择第`i`个字段，将其绑定到变量`w`，然后继续评估表达式`e`。
  
  - **CPS.OFFSET**：
    - 对记录`v`应用偏移`i`，将结果绑定到变量`w`，然后继续评估表达式`e`。
  
  - **CPS.APP**：
    - 函数应用。获取函数`f`的值`g`，并将参数列表`vl`转换为`dvalue`列表后应用函数`g`。
  
  - **CPS.RECORD**：
    - 构造记录。将字段列表`vl`转换为`dvalue`列表，通过辅助函数`F`处理偏移，绑定到变量`w`，然后继续评估表达式`e`。
  
  - **CPS.SWITCH**：
    - 条件分支。获取表达式`v`的整数值`i`，选择第`i`个分支表达式并评估。
  
  - **CPS.PRIMOP**：
    - 原始操作符评估。调用`evalprim`函数，传递操作符`p`、参数列表`vl`（转换为`dvalue`）、以及对应的回调函数`el`。
  
  - **CPS.FIX**：
    - 定义递归函数。通过固定点组合器实现递归调用，绑定递归函数到环境中，然后继续评估表达式`e`。

### **2.8 最终评估函数**

```sml
val env0 = fn x => raise Undefined

fun eval (vl, e) dl = E (bindn(env0, vl, dl)) e
```

- **env0**：初始环境，将所有变量绑定到`Undefined`异常。
  
- **eval**：入口评估函数，接受变量列表`vl`、CPS表达式`e`、值列表`dl`，通过绑定变量到值，调用评估函数`E`开始评估。

### **3. 具体实现细节**

#### **3.1 store类型**

```sml
type store = loc * (loc -> dvalue) * (loc -> int)
```

- **解释**：
  - **loc**：存储位置标识符。
  - **(loc -> dvalue)**：存储位置到值的映射函数。
  - **(loc -> int)**：存储位置到整数的映射函数，用于不可变数组（UARRAY）。

#### **3.2 数据类型定义**

```sml
datatype dvalue = RECORD of dvalue list * int
                | INT of int
                | REAL of real
                | FUNC of (dvalue list -> (loc * (loc -> dvalue) * (loc -> int)) -> answer)
                | STRING of string
                | BYTEARRAY of loc list
                | ARRAY of loc list
                | UARRAY of loc list
```

- **解释**：
  - **RECORD**：包含`dvalue`列表和一个整数，表示记录数据。
  - **INT**：整数值。
  - **REAL**：实数值。
  - **FUNC**：函数值，包含一个函数接收`dvalue`列表和存储状态，返回`answer`。
  - **STRING**：字符串值。
  - **BYTEARRAY, ARRAY, UARRAY**：字节数组、数组和不可变数组，包含位置列表。

#### **3.3 evalprim函数**

- **作用**：根据CPS操作符和参数，执行相应的操作，并调用回调函数返回结果或处理异常。

- **例子**：
  - **CPS.+**：执行整数加法，检查溢出，调用回调函数`c`返回结果。
  - **CPS.div**：处理除法，特别是除零情况。
  - **CPS.<**：执行小于比较，调用`then`或`else`回调函数。

- **特殊处理**：
  - **CPS.boxed**：判断值是否为构造函数类型。
  - **CPS.makeref**：创建新的引用，分配新的存储位置。

#### **3.4 环境绑定**

```sml
type env = CPS.var -> dvalue

fun V env (CPS.INT i) = INT i
| V env (CPS.REAL r) = REAL(string2real r)
| V env (CPS.STRING s) = STRING s
| V env (CPS.VAR v) = env v
| V env (CPS.LABEL v) = env v

fun bind(env: env, v: CPS.var, d) =
  fn w => if v = w then d else env w

fun bindn(env, v::vl, d::dl) =
  bindn(bind(env, v, d), vl, dl)
| bindn(env, nil, nil) = env
```

- **V**：将CPS表达式的值转换为`dvalue`。
  - 处理不同类型的CPS值（INT, REAL, STRING, VAR, LABEL）。
  
- **bind**：创建一个新的环境，将变量`v`绑定到值`d`。
  - 当查询变量`w`时，如果`w = v`，返回`d`，否则调用原环境`env w`。

- **bindn**：批量绑定变量和值，通过递归调用`bind`函数。

#### **3.5 表达式评估函数E**

```sml
fun E (CPS.SELECT(i, v, w, e)) env =
  let 
    val RECORD(l, j) = V env v
  in 
    E e (bind(env, w, nth(l, i + j)))
  end
| E (CPS.OFFSET(i, v, w, e)) env =
  let 
    val RECORD(l, j) = V env v
  in 
    E e (bind(env, w, RECORD(l, i + j)))
  end
| E (CPS.APP(f, vl)) env =
  let 
    val FUNC g = V env f
  in 
    g (map (V env) vl)
  end
| E (CPS.RECORD(vl, w, e)) env =
  E e (bind(env, w, RECORD(map (fn (x, p) => F(V env x, p)) vl, 0)))
| E (CPS.SWITCH(v, el)) env =
  let 
    val INT i = V env v
  in 
    E (nth(el, i)) env
  end
| E (CPS.PRIMOP(p, vl, wl, el)) env =
  evalprim(p,
    map (V env) vl,
    map (fn e => fn al => E e (bindn(env, wl, al))) el)
| E (CPS.FIX(fl, e)) env =
  let 
    fun h r1 (f, vl, b) =
      FUNC(fn al => E b (bindn(g r1, vl, al)))
    and g r = bindn(r, map #1 fl, map (h r) fl)
  in 
    E e (g env)
  end
```

- **解释**：

  - **CPS.SELECT**：
    - 从记录`v`中选择第`i`个字段，绑定到变量`w`，然后继续评估表达式`e`。
  
  - **CPS.OFFSET**：
    - 对记录`v`应用偏移`i`，生成新的记录，绑定到变量`w`，然后继续评估表达式`e`。
  
  - **CPS.APP**：
    - 函数应用。获取函数`f`的值`g`，将参数列表`vl`转换为`dvalue`列表后应用函数`g`。
  
  - **CPS.RECORD**：
    - 构造记录。将字段列表`vl`转换为`dvalue`列表，通过辅助函数`F`处理偏移，绑定到变量`w`，然后继续评估表达式`e`。
  
  - **CPS.SWITCH**：
    - 条件分支。获取表达式`v`的整数值`i`，选择第`i`个分支表达式并评估。
  
  - **CPS.PRIMOP**：
    - 原始操作符评估。调用`evalprim`函数，传递操作符`p`、参数列表`vl`（转换为`dvalue`）、以及对应的回调函数`el`。
  
  - **CPS.FIX**：
    - 定义递归函数。通过固定点组合器实现递归调用，绑定递归函数到环境中，然后继续评估表达式`e`。

### **3.6 最终评估函数**

```sml
val env0 = fn x => raise Undefined

fun eval (vl, e) dl = E (bindn(env0, vl, dl)) e
```

- **解释**：
  - **env0**：初始环境，所有未绑定的变量都会引发`Undefined`异常。
  - **eval**：入口评估函数，接受变量列表`vl`、CPS表达式`e`、值列表`dl`，通过绑定变量到值，调用评估函数`E`开始评估。

### **3.7 功能示例**

#### **示例 1：countzeros程序**

```ml
fun count p = 
  let 
    fun f (a::r) = if p a then 1 + f r else f r
    | f nil = 0
  in 
    f
  end

fun curry fxy = f(x, y)

val countzeros = count (curry (fn (w, z) => w = z) 0)
```

**解析**：

1. **函数`count`**：
   - **定义**：接受一个谓词函数`p`，返回一个新函数`f`，该函数用于统计列表中满足`p`条件的元素数量。
   - **实现**：
     - `f`是一个递归函数，遍历列表。如果当前元素`a`满足`p a`，则计数加1，并递归处理余下的列表；否则，仅递归处理余下的列表。
     - `f nil = 0`：当列表为空时，返回计数0。
  
2. **函数`curry`**：
   - **定义**：将接受一个2元组参数的函数转换为接受两个独立参数的“柯里化”函数。
   - **实现**：`curry fxy = f(x, y)`，即将2元组`(x, y)`拆分为两个参数传递给函数`f`。

3. **变量`countzeros`**：
   - **定义**：应用`count`函数与一个特定的谓词函数，生成一个用于统计零的函数。
   - **实现**：
     - `curry (fn (w, z) => w = z) 0`：首先定义一个匿名函数`fn (w, z) => w = z`，用于判断`w`是否等于`z`，然后将其与0一起柯里化，得到一个新函数。
     - `count (curry (fn (w, z) => w = z) 0)`：将上述函数作为谓词传递给`count`，生成`countzeros`函数。

**功能**：
- `countzeros`函数用于统计一个整数列表中等于0的元素数量。

**类型推导**：
- `count`函数的类型为：`('a -> bool) -> ('a list -> int)`。
- `curry (fn (w, z) => w = z) 0`的类型为：`int -> bool`（假设`w`和`z`为整数）。
- 因此，`countzeros`的类型为：`int list -> int`。

#### **示例 2：eq函数**

```ml
fun eq (RECORD(a, i), RECORD(b, j)) =
  arbitrarily (i = j andalso eqlist(a, b), false)
| eq (INT i, INT j) = i = j
| eq (STRING a, STRING b) = arbitrarily (a = b, false)
| eq (ARRAY nil, ARRAY nil) = true
| eq (ARRAY (a::_), ARRAY (b::_)) = a = b
| eq (FUNC a, FUNC b) = raise Undefined
| eq (_, _) = false
and eqlist(a::al, b::bl) = eq(a, b) andalso eqlist(al, bl)
| eqlist(nil, nil) = true
```

**解析**：

1. **函数`eq`**：
   - **定义**：用于判断两个`dvalue`是否相等，模拟指针相等的语义。
   - **模式匹配规则**：
     - **记录类型匹配**：
       ```ml
       eq (RECORD(a, i), RECORD(b, j)) = arbitrarily (i = j andalso eqlist(a, b), false)
       ```
       - **解释**：
         - 如果两个参数都是`RECORD`类型，则比较它们的字段`i`和`j`是否相等，并递归调用`eqlist`比较字段`a`和`b`的列表。
         - 使用`arbitrarily`函数，若条件为真，则返回`i = j andalso eqlist(a, b)`的结果；否则返回`false`。
     
     - **整数类型匹配**：
       ```ml
       eq (INT i, INT j) = i = j
       ```
       - **解释**：直接比较两个整数是否相等。
     
     - **字符串类型匹配**：
       ```ml
       eq (STRING a, STRING b) = arbitrarily (a = b, false)
       ```
       - **解释**：比较两个字符串是否相等，使用`arbitrarily`函数处理。
     
     - **数组类型匹配（空数组）**：
       ```ml
       eq (ARRAY nil, ARRAY nil) = true
       ```
       - **解释**：两个空数组相等。
     
     - **数组类型匹配（非空数组）**：
       ```ml
       eq (ARRAY (a::_), ARRAY (b::_)) = a = b
       ```
       - **解释**：比较两个非空数组的第一个元素是否相等。
     
     - **函数类型匹配**：
       ```ml
       eq (FUNC a, FUNC b) = raise Undefined
       ```
       - **解释**：无法比较两个函数，抛出`Undefined`异常。
     
     - **默认匹配**：
       ```ml
       eq (_, _) = false
       ```
       - **解释**：如果前面的模式都不匹配，则返回`false`。
   
2. **函数`eqlist`**：
   - **定义**：递归比较两个列表中的每个元素是否相等。
   - **模式匹配规则**：
     - **非空列表匹配**：
       ```ml
       eqlist (a::al, b::bl) = eq (a, b) andalso eqlist (al, bl)
       ```
       - **解释**：比较两个列表的第一个元素`a`和`b`是否相等，并递归比较余下的元素`al`和`bl`。
     - **空列表匹配**：
       ```ml
       eqlist (nil, nil) = true
       ```
       - **解释**：两个空列表相等。

**功能**：
- `eq`函数用于比较各种类型的值是否相等，模拟指针相等的行为。
- `eqlist`函数用于比较两个列表中的元素是否逐一相等。

**注意事项**：
- **模式匹配顺序**：ML按照声明的顺序进行模式匹配，首先匹配的规则优先。
- **异常处理**：某些情况（如比较函数类型）会抛出异常，需要在调用时处理。
- **递归调用**：函数`eqlist`和`eq`之间是互相递归调用，确保列表中所有元素都被比较。

### **总结**

附录B定义了CPS语言的语义，通过ML中的functor `CPSsemantics` 实现。以下是关键要点总结：

1. **模块化设计**：
   - `CPSsemantics` functor 接受CPS语言的基本定义和操作符，返回一个具有评估功能的模块。

2. **存储管理**：
   - 使用`store`类型表示存储状态，包括位置标识符、值映射函数和整数映射函数（用于不可变数组）。
   - 提供`fetch`、`upd`、`fetchi`、`updi`等函数用于访问和更新存储。

3. **值比较**：
   - 实现了复杂数据类型的相等性检查，通过递归比较每个元素。
   - 使用`arbitrarily`函数处理非确定性选择，模拟指针相等的行为。

4. **异常处理**：
   - 提供`do_raise`函数，通过调用异常处理程序处理溢出和除零异常。

5. **原始操作评估**：
   - `evalprim`函数处理CPS语言中的所有原始操作符，根据操作符类型和参数执行相应的操作，并调用回调函数返回结果或处理异常。

6. **环境绑定**：
   - 定义`env`类型表示环境，将CPS变量绑定到`dvalue`。
   - 提供`bind`和`bindn`函数用于环境的变量绑定。

7. **表达式评估**：
   - `E`函数是递归评估CPS表达式的核心函数，处理不同类型的CPS表达式（如选择、偏移、函数应用、记录构造、条件分支、原始操作、递归）。

8. **最终评估函数**：
   - `eval`函数是入口函数，初始化环境并调用评估函数`E`开始评估CPS表达式。

通过理解上述关键部分，您可以全面掌握CPS语言的语义实现，包括如何管理存储、处理异常、评估原始操作符以及递归地评估复杂表达式。这为进一步研究和理解续延传递风格及其在编译器中的应用提供了坚实的基础。

如果您对**附录 B**中的某个具体部分有进一步的问题或需要更深入的解释，请随时提问！

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