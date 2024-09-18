

```

我的需求可以总结为以下几点：

1. **阅读背景**：  
   - 我正在阅读《Types and Programming Languages》这本书,这是一本类型论领域的经典教材，内容涉及多个学科领域，如：计算机科学(Computer Science)、类型论(Type Theory)、范畴论(Category Theory)、代数(Algebra)、逻辑学(Logic)、群论(Group Theory)、拓扑学(Topology)等。
   - 主要聚焦在**编程语言设计原理**(Programming Language Design Principles)和类型论。

2. **理解需求**：  
   - 我希望系统性、条理性地理解书中的内容，并要求对内容、概念的**历史背景**、**原因**和**详细解释**等进行全方位解析。
   - 我首先希望理解书中的内容，还希望通过学习这本书，拓宽视野，**不局限于书本**，帮助我全面理解**编程语言设计原理**和相关学科领域的知识。
   - 如果有的话，列举相关的概念和设计在主流编程语言其自身设计中有哪些设计上的应用。

3. **解释要求**：  
   - **我可能会提供给我英语，但是使用中文进行解释**，但是所有**关键概念**必须提供**中英双语**: 中文(English)，以方便记忆和学习。
   - 解释应当**详尽**，不省略任何重要细节！！！
   - 按顺序一条一条详解，不要省略任何内容
   
4. **公式要求**：  
   - 对于涉及公式的内容，**需要使用公式**进行解释，并**详细解析公式**中的符号、意义以及作用。
   - 公式可能会比较抽象，使用符号较多，所以我要求提供详细的公式解释。
   - 不只是解释公式，段落等内容也需要一起讲解。
   
5. **Markdown 语法要求**：  
   - **行内公式和特殊字符**需要使用**单美元符** $ 包裹，适用于嵌入到段落中的符号和公式。例如：$a:A$、$f(a)$、$A \to B$、$U_0, U_1, U_2, \ldots$。
   - **单独占用一行或多行的公式**需要用**双美元符** $$ 包裹。例如：
     $$
     \int_{a}^{b} f(x) \, dx = F(b) - F(a)
     $$
   - ！！！！！不要使用这个符号：【重音符`】包裹特殊字符和关键字,你应该使用单美元符$包裹特殊字符和关键字等,使用单美元包裹的时候开始和结束单美元符不要和被包裹之间内容存在空格. 这样`key word`的包裹不是我想要的，我想要：$key word$ .
   - 公式应当符合标准 Markdown 语法，行内和多行公式的区别需要严格遵守。

请注意，列出原文的公式并讲解。
请注意，每个公式在开始的时候都应该存在一个推导横线。
然后
多层推导树的话，每一层都要像这样分隔开：
$$
\frac{}{\text{empty tree}} \quad \frac{}{\text{empty tree}} 
$$
$$
\quad \frac{\frac{}{\text{empty tree}} \quad \frac{}{\text{empty tree}}}{\text{node(empty; empty) tree}} 
$$
$$
\quad \frac{\frac{\frac{}{\text{empty tree}} \quad \frac{}{\text{empty tree}}}{\text{node(empty; empty) tree}} \quad \frac{}{\text{empty tree}}}{\text{node(node(empty; empty); empty) tree}}
$$

6. **代码要求**：  
   - 涉及代码段时，需**添加注释**并且提供**详细讲解**，逐步分析代码的每一个步骤和功能。

7. **逐步提供内容**：  
   - 我会逐步提供书中的内容（可能是英文），并希望我逐步进行详细讲解。

8. **不省略任何内容**：  
   - 我要求解释时**不省略任何内容**，包括内容中的每一个细节，确保完整性。

9. **目标**：  
   - 我希望通过这本书的学习，**拓宽自己的视野**，不仅局限于书本内容，而是从全方位视角理解**编程语言设计原理，**以及相关领域的知识，达到更深层次的学习效果。
   
10. 开头和结尾不要说废话，直接生成答案。

11. excercise习题要求：
	- 涉及到的习题讲解并且给出解答。
12. 关于涉及到的Figure，都列出原文的公式并且详解每一条公式。

我会在后面接下来的提问逐步提供给我内容，然后请给我讲解，关于讲解的时候，请不要省略任何内容。
```


```
按顺序一条一条详解，不要省略任何内容：
```



```
按顺序一条一条详解，不要省略任何内容
请记住！！！！！不要使用这个符号：【重音符`】包裹特殊字符和关键字,你应该使用单美元符$包裹特殊字符和关键字等,使用单美元包裹的时候开始和结束单美元符不要和被包裹之间内容存在空格. 这样`key word`的包裹不是我想要的，我想要：$key word$ .
```



```
按顺序一条一条详解，不要省略任何内容。
涉及到的习题讲解并且给出解答。
关于涉及到的Figure，都列出原文的公式并且详解每一条公式。
其余的内容讲解帮助理解。

```









```
11.10 Variants
Binary sums generalize to labeled variants just as products generalize to labeled records. Instead of T1+T2, we write <l1:T1,l2:T2>, where l1 and l2 are
field labels. Instead of inl t as T1+T2, we write <l1=t> as <l1:T1,l2:T2>.
And instead of labeling the branches of the case with inl and inr, we use
the same labels as the corresponding sum type. With these generalizations,
the getAddr example from the previous section becomes:
Addr = <physical:PhysicalAddr, virtual:VirtualAddr>;
a = <physical=pa> as Addr;
ñ a : Addr
getName = λa:Addr.
case a of
<physical=x> ⇒ x.firstlast
| <virtual=y> ⇒ y.name;
ñ getName : Addr → String
The formal definition of variants is given in Figure 11-11. Note that, as with
records in §11.8, the order of labels in a variant type is significant here.

→ <> Extends λ→ (9-1)
New syntactic forms
t ::= ... terms:
<l=t> as T tagging
case t of <li=xi>⇒ti
i∈1..n case
T ::= ... types:
<li:Ti
i∈1..n> type of variants
New evaluation rules t -→ t
0
case (<lj=vj> as T) of <li=xi>⇒ti
i∈1..n
-→ [xj , vj ]tj
(E-CaseVariant)
t0 -→ t
0
0
case t0 of <li=xi>⇒ti
i∈1..n
-→ case t0
0 of <li=xi>⇒ti
i∈1..n
(E-Case)
ti
-→ t
0
i
<li=ti> as T -→ <li=t0
i> as T (E-Variant)
New typing rules Γ ` t : T
Γ ` tj : Tj
Γ ` <lj=tj> as <li:Ti
i∈1..n> : <li:Ti
i∈1..n>
(T-Variant)
Γ ` t0 : <li:Ti
i∈1..n>
for each i Γ , xi:Ti ` ti : T
Γ ` case t0 of <li=xi>⇒ti
i∈1..n : T
(T-Case)
Figure 11-11: Variants

Options
One very useful idiom involving variants is optional values. For example, an
element of the type
OptionalNat = <none:Unit, some:Nat>;
is either the trivial unit value with the tag none or else a number with the tag
some—in other words, the type OptionalNat is isomorphic to Nat extended
with an additional distinguished value none. For example, the type
Table = Nat→OptionalNat;
represents finite mappings from numbers to numbers: the domain of such a
mapping is the set of inputs for which the result is <some=n> for some n. The
empty table
emptyTable = λn:Nat. <none=unit> as OptionalNat;
ñ emptyTable : Table
is a constant function that returns none for every input. The constructor
extendTable =
λt:Table. λm:Nat. λv:Nat.
λn:Nat.
if equal n m then <some=v> as OptionalNat
else t n;
ñ extendTable : Table → Nat → Nat → Table
takes a table and adds (or overwrites) an entry mapping the input m to the
output <some=v>. (The equal function is defined in the solution to Exercise 11.11.1 on page 510.)
We can use the result that we get back from a Table lookup by wrapping a
case around it. For example, if t is our table and we want to look up its entry
for 5, we might write
x = case t(5) of
<none=u> ⇒ 999
| <some=v> ⇒ v;
providing 999 as the default value of x in case t is undefined on 5.
Many languages provide built-in support for options. OCaml, for example, predefines a type constructor option, and many functions in typical
OCaml programs yield options. Also, the null value in languages like C, C++,
and Java is actually an option in disguise. A variable of type T in these languages (where T is a “reference type”—i.e., something allocated in the heap)
138 11 Simple Extensions
can actually contain either the special value null or else a pointer to a T
value. That is, the type of such a variable is really Ref(Option(T)), where
Option(T) = <none:Unit,some:T>. Chapter 13 discusses the Ref constructor in detail.
Enumerations
Two “degenerate cases” of variant types are useful enough to deserve special
mention: enumerated types and single-field variants.
An enumerated type (or enumeration) is a variant type in which the field
type associated with each label is Unit. For example, a type representing the
days of the working week might be defined as:
Weekday = <monday:Unit, tuesday:Unit, wednesday:Unit,
thursday:Unit, friday:Unit>;
The elements of this type are terms like <monday=unit> as Weekday. Indeed,
since the type Unit has only unit as a member, the type Weekday is inhabited
by precisely five values, corresponding one-for-one with the days of the week.
The case construct can be used to define computations on enumerations.
nextBusinessDay = λw:Weekday.
case w of <monday=x> ⇒ <tuesday=unit> as Weekday
| <tuesday=x> ⇒ <wednesday=unit> as Weekday
| <wednesday=x> ⇒ <thursday=unit> as Weekday
| <thursday=x> ⇒ <friday=unit> as Weekday
| <friday=x> ⇒ <monday=unit> as Weekday;
Obviously, the concrete syntax we are using here is not well tuned for making
such programs easy to write or read. Some languages (beginning with Pascal)
provide special syntax for declaring and using enumerations. Others—such
as ML, cf. page 141—make enumerations a special case of the variants.
Single-Field Variants
The other interesting special case is variant types with just a single label l:
V = <l:T>;
Such a type might not seem very useful at first glance: after all, the elements
of V will be in one-to-one correspondence with the elements of the field type
T, since every member of V has precisely the form <l=t> for some t : T.
What’s important, though, is that the usual operations on T cannot be applied
to elements of V without first unpackaging them: a V cannot be accidentally
mistaken for a T.
11.10 Variants 139
For example, suppose we are writing a program to do financial calculations
in multiple currencies. Such a program might include functions for converting
between dollars and euros. If both are represented as Floats, then these
functions might look like this:
dollars2euros = λd:Float. timesfloat d 1.1325;
ñ dollars2euros : Float → Float
euros2dollars = λe:Float. timesfloat e 0.883;
ñ euros2dollars : Float → Float
(where timesfloat : Float→Float→Float multiplies floating-point numbers). If we then start with a dollar amount
mybankbalance = 39.50;
we can convert it to euros and then back to dollars like this:
euros2dollars (dollars2euros mybankbalance);
ñ 39.49990125 : Float
All this makes perfect sense. But we can just as easily perform manipulations
that make no sense at all. For example, we can convert my bank balance to
euros twice:
dollars2euros (dollars2euros mybankbalance);
ñ 50.660971875 : Float
Since all our amounts are represented simply as floats, there is no way that
the type system can help prevent this sort of nonsense. However, if we define
dollars and euros as different variant types (whose underlying representations are floats)
DollarAmount = <dollars:Float>;
EuroAmount = <euros:Float>;
then we can define safe versions of the conversion functions that will only
accept amounts in the correct currency:
dollars2euros =
λd:DollarAmount.
case d of <dollars=x> ⇒
<euros = timesfloat x 1.1325> as EuroAmount;
ñ dollars2euros : DollarAmount → EuroAmount
140 11 Simple Extensions
euros2dollars =
λe:EuroAmount.
case e of <euros=x> ⇒
<dollars = timesfloat x 0.883> as DollarAmount;
ñ euros2dollars : EuroAmount → DollarAmount
Now the typechecker can track the currencies used in our calculations and
remind us how to interpret the final results:
mybankbalance = <dollars=39.50> as DollarAmount;
euros2dollars (dollars2euros mybankbalance);
ñ <dollars=39.49990125> as DollarAmount : DollarAmount
Moreover, if we write a nonsensical double-conversion, the types will fail to
match and our program will (correctly) be rejected:
dollars2euros (dollars2euros mybankbalance);
ñ Error: parameter type mismatch
Variants vs. Datatypes
A variant type T of the form <li:Ti
i∈1..n> is roughly analogous to the ML
datatype defined by:7
type T = l1 of T1
| l2 of T2
| ...
| ln of Tn
But there are several differences worth noticing.
1. One trivial but potentially confusing point is that the capitalization conventions for identifiers that we are assuming here are different from those
of OCaml. In OCaml, types must begin with lowercase letters and datatype
constructors (labels, in our terminology) with capital letters, so, strictly
speaking, the datatype declaration above should be written like this:
type t = L1 of t1 | ... | Ln of tn
7. This section uses OCaml’s concrete syntax for datatypes, for consistency with implementation chapters elsewhere in the book, but they originated in early dialects of ML and can be
found, in essentially the same form, in Standard ML as well as in ML relatives such as Haskell.
Datatypes and pattern matching are arguably one of the most useful advantages of these languages for day to day programming.
11.10 Variants 141
To avoid confusion between terms t and types T, we’ll ignore OCaml’s
conventions for the rest of this discussion and use ours instead.
2. The most interesting difference is that OCaml does not require a type annotation when a constructor li is used to inject an element of Ti into the
datatype T: we simply write li(t). The way OCaml gets away with this (and
retains unique typing) is that the datatype T must be declared before it can
be used. Moreover, the labels in T cannot be used by any other datatype
declared in the same scope. So, when the typechecker sees li(t), it knows
that the annotation can only be T. In effect, the annotation is “hidden” in
the label itself.
This trick eliminates a lot of silly annotations, but it does lead to a certain
amount of grumbling among users, since it means that labels cannot be
shared between different datatypes—at least, not within the same module.
In Chapter 15 we will see another way of omitting annotations that avoids
this drawback.
3. Another convenient trick used by OCaml is that, when the type associated with a label in a datatype definition is just Unit, it can be omitted
altogether. This permits enumerations to be defined by writing
type Weekday = monday | tuesday | wednesday | thursday | friday
for example, rather than:
type Weekday = monday of Unit
| tuesday of Unit
| wednesday of Unit
| thursday of Unit
| friday of Unit
Similarly, the label monday all by itself (rather than monday applied to the
trivial value unit) is considered to be a value of type Weekday.
4. Finally, OCaml datatypes actually bundle variant types together with several additional features that we will be examining, individually, in later
chapters.
• A datatype definition may be recursive—i.e., the type being defined is
allowed to appear in the body of the definition. For example, in the
standard definition of lists of Nats, the value tagged with cons is a pair
whose second element is a NatList.
type NatList = nil
| cons of Nat * NatList
142 11 Simple Extensions
• An OCaml datatype can be [parametric data type]parameterizedparametric!data
type on a type variable, as in the general definition of the List datatype:
type ’a List = nil
| cons of ’a * ’a List
Type-theoretically, List can be viewed as a kind of function—called a
type operator—that maps each choice of 0a to a concrete datatype. . .
Nat to NatList, etc. Type operators are the subject of Chapter 29.
Variants as Disjoint Unions
Sum and variant types are sometimes called disjoint unions. The type T1+T2 is
a “union” of T1 and T2 in the sense that its elements include all the elements
from T1 and T2. This union is disjoint because the sets of elements of T1 or
T2 are tagged with inl or inr, respectively, before they are combined, so that
it is always clear whether a given element of the union comes from T1 or T2.
The phrase union type is also used to refer to untagged (non-disjoint) union
types, described in §15.7.
Type Dynamic
Even in statically typed languages, there is often the need to deal with data
whose type cannot be determined at compile time. This occurs in particular
when the lifetime of the data spans multiple machines or many runs of the
compiler—when, for example, the data is stored in an external file system
or database, or communicated across a network. To handle such situations
safely, many languages offer facilities for inspecting the types of values at
run time.
One attractive way of accomplishing this is to add a type Dynamic whose
values are pairs of a value v and a type tag T where v has type T. Instances
of Dynamic are built with an explicit tagging construct and inspected with a
type safe typecase construct. In effect, Dynamic can be thought of as an infinite disjoint union, whose labels are types. See Gordon (circa 1980), Mycroft
(1983), Abadi, Cardelli, Pierce, and Plotkin (1991b), Leroy and Mauny (1991),
Abadi, Cardelli, Pierce, and Rémy (1995), and Henglein (1994).
```



```
不要省略任何内容，按顺序一条一条详解，不要省略任何内容。
涉及到的习题讲解并且给出解答。
关于涉及到的Figure，都列出原文的公式并且详解每一条公式。
其余的内容讲解帮助理解。

————————
11.11 General Recursion
Another facility found in most programming languages is the ability to define recursive functions. We have seen (Chapter 5, p. 65) that, in the untyped
11.11 General Recursion 143
lambda-calculus, such functions can be defined with the aid of the fix combinator.
Recursive functions can be defined in a typed setting in a similar way. For
example, here is a function iseven that returns true when called with an
even argument and false otherwise:
ff = λie:Nat→Bool.
λx:Nat.
if iszero x then true
else if iszero (pred x) then false
else ie (pred (pred x));
ñ ff : (Nat→Bool) → Nat → Bool
iseven = fix ff;
ñ iseven : Nat → Bool
iseven 7;
ñ false : Bool
The intuition is that the higher-order function ff passed to fix is a generator
for the iseven function: if ff is applied to a function ie that approximates
the desired behavior of iseven up to some number n (that is, a function that
returns correct results on inputs less than or equal to n), then it returns a
better approximation to iseven—a function that returns correct results for
inputs up to n + 2. Applying fix to this generator returns its fixed point—a
function that gives the desired behavior for all inputs n.
However, there is one important difference from the untyped setting: fix
itself cannot be defined in the simply typed lambda-calculus. Indeed, we will
see in Chapter 12 that no expression that can lead to non-terminating computations can be typed using only simple types.8 So, instead of defining fix as
a term in the language, we simply add it as a new primitive, with evaluation
rules mimicking the behavior of the untyped fix combinator and a typing
rule that captures its intended uses. These rules are written out in Figure
11-12. (The letrec abbreviation will be discussed below.)
The simply typed lambda-calculus with numbers and fix has long been a
favorite experimental subject for programming language researchers, since
it is the simplest language in which a range of subtle semantic phenomena
such as full abstraction (Plotkin, 1977, Hyland and Ong, 2000, Abramsky, Jagadeesan, and Malacaria, 2000) arise. It is often called PCF.
8. In later chapters—Chapter 13 and Chapter 20—we will see some extensions of simple types
that recover the power to define fix within the system.
144 11 Simple Extensions
→ fix Extends λ→ (9-1)
New syntactic forms
t ::= ... terms:
fix t fixed point of t
New evaluation rules t -→ t
0
fix (λx:T1.t2)
-→ [x , (fix (λx:T1.t2))]t2
(E-FixBeta)
t1 -→ t
0
1
fix t1 -→ fix t0
1
(E-Fix)
New typing rules Γ ` t : T
Γ ` t1 : T1→T1
Γ ` fix t1 : T1
(T-Fix)
New derived forms
letrec x :T1 =t1 in t2
def
= let x = fix (λx :T1 .t1) in t2
Figure 11-12: General recursion
11.11.1 Exercise [««]: Define equal, plus, times, and factorial using fix.
The fix construct is typically used to build functions (as fixed points of
functions from functions to functions), but it is worth noticing that the type
T in rule T-Fix is not restricted to function types. This extra power is sometimes handy. For example, it allows us to define a record of mutually recursive
functions as the fixed point of a function on records (of functions). The following implementation of iseven uses an auxiliary function isodd; the two
functions are defined as fields of a record, where the definition of this record
is abstracted on a record ieio whose components are used to make recursive
calls from the bodies of the iseven and isodd fields.
ff = λieio:{iseven:Nat→Bool, isodd:Nat→Bool}.
{iseven = λx:Nat.
if iszero x then true
else ieio.isodd (pred x),
isodd = λx:Nat.
if iszero x then false
else ieio.iseven (pred x)};
ñ ff : {iseven:Nat→Bool,isodd:Nat→Bool} →
{iseven:Nat→Bool, isodd:Nat→Bool}
Forming the fixed point of the function ff gives us a record of two functions
r = fix ff;
ñ r : {iseven:Nat→Bool, isodd:Nat→Bool}
11.11 General Recursion 145
and projecting the first of these gives us the iseven function itself:
iseven = r.iseven;
ñ iseven : Nat → Bool
iseven 7;
ñ false : Bool
The ability to form the fixed point of a function of type T→T for any T
has some surprising consequences. In particular, it implies that every type is
inhabited by some term. To see this, observe that, for every type T, we can
define a function divergeT as follows:
divergeT = λ_:Unit. fix (λx:T.x);
ñ divergeT : Unit → T
Whenever divergeT is applied to a unit argument, we get a non-terminating
evaluation sequence in which E-FixBeta is applied over and over, always
yielding the same term. That is, for every type T, the term divergeT unit
is an undefined element of T.
One final refinement that we may consider is introducing more convenient
concrete syntax for the common case where what we want to do is to bind a
variable to the result of a recursive definition. In most high-level languages,
the first definition of iseven above would be written something like this:
letrec iseven : Nat→Bool =
λx:Nat.
if iszero x then true
else if iszero (pred x) then false
else iseven (pred (pred x))
in
iseven 7;
ñ false : Bool
The recursive binding construct letrec is easily defined as a derived form:
letrec x:T1=t1 in t2
def
= let x = fix (λx:T1.t1) in t2
11.11.2 Exercise [«]: Rewrite your definitions of plus, times, and factorial from
Exercise 11.11.1 using letrec instead of fix.
Further information on fixed point operators can be found in Klop (1980)
and Winskel (1993).
146 11 Simple Extensions
11.12 Lists
The typing features we have seen can be classified into base types like Bool
and Unit, and type constructors like → and × that build new types from
old ones. Another useful type constructor is List. For every type T, the type
List T describes finite-length lists whose elements are drawn from T.
Figure 11-13 summarizes the syntax, semantics, and typing rules for lists.
Except for syntactic differences (List T instead of T list, etc.) and the explicit type annotations on all the syntactic forms in our presentation,9
these
lists are essentially identical to those found in ML and other functional languages. The empty list (with elements of type T) is written nil[T]. The list
formed by adding a new element t1 (of type T) to the front of a list t2 is written cons[T] t1 t2. The head and tail of a list t are written head[T] t and
tail[T] t. The boolean predicate isnil[T] t yields true iff t is empty.10
11.12.1 Exercise [«««]: Verify that the progress and preservation theorems hold for
the simply typed lambda-calculus with booleans and lists.
11.12.2 Exercise [««]: The presentation of lists here includes many type annotations
that are not really needed, in the sense that the typing rules can easily derive
the annotations from context. Can all the type annotations be deleted?
9. Most of these explicit annotations could actually be omitted (Exercise [«, 3]: which cannot);
they are retained here to ease comparison with the encoding of lists in §23.4.
10. We adopt the “head/tail/isnil presentation” of lists here for simplicity. From the perspective of language design, it is arguably better to treat lists as a datatype and use case
expressions for destructing them, since more programming errors can be caught as type errors
this way.
11.12 Lists 147
→ B List Extends λ→ (9-1) with booleans (8-1)
New syntactic forms
t ::= ... terms:
nil[T] empty list
cons[T] t t list constructor
isnil[T] t test for empty list
head[T] t head of a list
tail[T] t tail of a list
v ::= ... values:
nil[T] empty list
cons[T] v v list constructor
T ::= ... types:
List T type of lists
New evaluation rules t -→ t
0
t1 -→ t
0
1
cons[T] t1 t2 -→ cons[T] t0
1 t2
(E-Cons1)
t2 -→ t
0
2
cons[T] v1 t2 -→ cons[T] v1 t
0
2
(E-Cons2)
isnil[S] (nil[T]) -→ true (E-IsnilNil)
isnil[S] (cons[T] v1 v2) -→ false
(E-IsnilCons)
t1 -→ t
0
1
isnil[T] t1 -→ isnil[T] t0
1
(E-Isnil)
head[S] (cons[T] v1 v2) -→ v1
(E-HeadCons)
t1 -→ t
0
1
head[T] t1 -→ head[T] t0
1
(E-Head)
tail[S] (cons[T] v1 v2) -→ v2
(E-TailCons)
t1 -→ t
0
1
tail[T] t1 -→ tail[T] t0
1
(E-Tail)
New typing rules Γ ` t : T
Γ ` nil [T1] : List T1 (T-Nil)
Γ ` t1 : T1 Γ ` t2 : List T1
Γ ` cons[T1] t1 t2 : List T1
(T-Cons)
Γ ` t1 : List T11
Γ ` isnil[T11] t1 : Bool (T-Isnil)
Γ ` t1 : List T11
Γ ` head[T11] t1 : T11
(T-Head)
Γ ` t1 : List T11
Γ ` tail[T11] t1 : List T11
(T-Tail)
Figure 11-13: Lists
```

