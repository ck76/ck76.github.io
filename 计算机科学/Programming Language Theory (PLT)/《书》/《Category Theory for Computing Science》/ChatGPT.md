



- 



```
我正在阅读一本书。请以超出原文的详细程度帮我理解其内容，我没有相关知识背景，所以请你讲解的时候全面，不要偷懒，通俗易懂。按顺序一点一点讲解，不要省略任何内容。我可能给你英文的问题，但是请中文回答我。关键术语用中文（英语）格式。
```



```
继续详解：【
5.2.20 Proposition Let C and D be any categories. If D has products,
then the functor category Func(C , D) also has products.
Proof. The product is constructed by constructing the product at each value
F(C) and G(C). Precisely, given two functors F, G : C −→ D, the product in
Func(C ,D) of F and G is the functor F ×G defined as follows. For an object
C of C , (F ×G)(C) = F(C)×G(C), the product of the sets F(C) and G(C)
in D. For an arrow f : C −→ D, (F × G)(f) = F(f) × G(f), the product of
the arrows as defined in 5.2.17. The projection π1 : F ×G −→ F is the natural
transformation whose component at C is π1C = pi
: F(C) × G(C) −→ F(C),
the product projection in D. For any f : C −→ D in C , the diagrams
F D × GD F D ✲p1
F C×GC F C
p1✲
❄
F(f)×G(f)
❄
F(f)
F D×GD GD ✲p2
F C×GC GC
p2✲
❄
F(f)×G(f)
❄
G(f)
(5.18)
5.2 Properties of products 169
commute by definition of F(f)×G(f) (5.2.17), so that π1 and π2 are natural
transformations as required.
Given natural transformations α : H −→ F and β : H −→ G, we must
define hα, βi : H −→ F × G. For an object C, the component hα, βiC =
hαC, βCi : H(C) −→ F(C)×β(C). To see that hα, βi is a natural transformation, we must show that for any arrow f : C −→ D, this diagram commutes:
H(D) ✲ F(D)×G(D)
hα, βiD
H(C) ✲ F(C)×G(C)
hα, βiC
❄
H(f)
❄
F(f)×G(f) (5.19)
This follows from the following calculation:
(F(f) × G(f)) ◦ hα, βiC = (F(f) × G(f)) ◦ hαC, βCi
= hF(f) ◦ αC, G(f) ◦ βCi
= hαD ◦ H(f), βD ◦ H(f)i
= hαD, βDi ◦ H(f)
= hα, βiD ◦ H(f)
in which the first and last equalities are by definition of hα, βi, the second is
by Equation (5.17), the third because α and β are natural transformations,
and the fourth by Equation (5.13).
Categorists say that this construction shows that Func(C , D) has ‘pointwise
products’. This is the common terminology, but it might be better to say it has
‘objectwise products’.
5.2.21 Corollary The category of models of a linear sketch has products
constructed pointwise.
This follows from the fact that the category of models of a linear sketch
S is equivalent to the functor category Func(Th(S ),Set) (see 4.6.11).
170 Products and sums
5.2.22 Exercises
1. Give explicitly the isomorphism claimed by Theorem 5.2.2 between S ×T
and the set {1, 2, 3, 4, 5, 6} expressed as the product of {1, 2, 3} and {1, 2}
using the projections in 5.2.1.
2. Given a two-element set A and a three-element set B, in how many
ways can the set {1, 2, 3, 4, 5, 6} be made into a product A × B? (This refers
to 5.2.1.)
3. Prove that Diagram (5.12) commutes.
4. Prove Proposition 5.2.3.
5. Let f : A −→ C, g : B −→ D, u : X −→ A and v : X −→ B. Show that
(f × g) ◦ hu, vi = hf ◦ u, g ◦ vi.】5.3 Finite products
Products of two objects, as discussed in the preceding sections, are called
binary products. We can define products of more than two objects by an
obvious modification of the definition.
For example, if A, B and C are three objects of a category, a product of
them is an object A × B × C together with three arrows:
A B C
A × B × C
p1



✠ ❄
p2 p3
❅
❅
❅
❅❅❘
(5.20)
for which, given any other diagram
A B C
D
q1



✠ ❄
q2 q3
❅
❅
❅
❅❅❘
there exists a unique arrow q = hq1, q2, q3i : D −→ A × B × C such that pi
◦
q = qi
, i = 1, 2, 3. A diagram of the form (5.20) is called a ternary product
diagram (or ternary product cone). The general definition of product follows
the sam
5.3 Finite products 171
5.3.1 Definition A product of a list A1, A2, . . . , An of objects (not necessarily distinct) of a category is an object V together with arrows pi
: A
−→ Ai
, for i = 1, . . . , n, with the property that given any object B and arrows
fi
: B −→ Ai
, i = 1, . . . , n, there is a unique arrow hf1, f2, . . . , fni : B −→ A
for which pi
◦ hf1, f2, . . . , fni = fi
, i = 1, . . . , n.
A product of such a list A1, A2, . . . , An is called an n-ary product
when it is necessary to specify the number of factors. Such a product may
be denoted A1 × A2 × · · · × An or Qn
i=1 Ai
.
The following uniqueness theorem for general finite products can be
proved in the same way as Theorem 5.2.2.
5.3.2 Theorem Suppose A1, A2, . . . , An are objects of a category C and
that A, with projections pi
: A −→ Ai, and B, with projections qi
: B −→ Ai,
are products of these objects. Then there is a unique arrow φ : A −→ B for
which qi
◦ φ = pi for i = 1, . . . , n. Moreover, φ is an isomorphism.
Propositions 5.2.3, 5.2.11 and 5.2.18 also generalize in the obvious way
to n-ary products.
5.3.3 Binary products give ternary products An important consequence of the definition of ternary product is that in any category with
binary products, and any objects A, B and C, either of (A × B) × C and
A × (B × C) can be taken as ternary products A × B × C with appropriate
choice of projections.
We prove this for (A × B) × C. Writing pi
, i = 1, 2, for the projections
which make A × B a product of A and B and qi
, i = 1, 2 for the projections
which make (A × B) × C a product of A × B and C, we claim that
C
(A × B) × C
q2
❅
❅
❅
❅
❅
❅❅❘
A B
A × B
p1

✠
p2
❅
❅❅❘
q1

✠
is a product diagram with vertex (A × B) × C and projections p1
◦ q1 :
(A × B) × C −→ A, p2
◦ q1 : (A × B) × C −→ B, and q2 : (A × B) × C −→ C.
Suppose that f : D −→ A, g : D −→ B, and h : D −→ C are given. We
must construct an arrow u : D −→ (A × B) × C with the property that
(a) p1
◦ q1
◦ u = f,
(b) p2
◦ q1
◦ u = 
172 Products and sums
(c) q2
◦ u = h.
Recall that hf, gi is the unique arrow making
A A ✛p × B
1 p
✲ B
2
D
f



✠ ❄
hf, gi g
❅
❅
❅
❅❅❘
(5.21)
commute. This induces a unique arrow u = hhf, gi, hi making
A × B ✛ (A × B) × C q1
✲ C q2
D
hf, gi





✠ ❄
u h
❅
❅
❅
❅
❅
❅❅❘
(5.22)
commute.
The fact that (a) through (c) hold can be read directly off these diagrams.
For example, for (a), p1
◦ q1
◦ u = p1
◦ hf, gi = f.
Finally, if u
0 were another arrow making (a) through (c) hold, then we
would have p1
◦ q1
◦ u
0 = f and p2
◦ q1
◦ u
0 = g, so by uniqueness of v as
defined by (5.21), v = q1
◦ u
0
. Since q2
◦ u
0 must be h, the uniqueness of u
in (5.22) means that u
0 = u.
A generalization of this is stated in Proposition 5.3.10 below.
.10 below.
5.3.4 It follows from the discussion in 5.3.3 that the two objects (A×B)×
C and A × (B × C) are pairwise canonically isomorphic (to each other and
to any other realization of the ternary product) in a way that preserves the
ternary product structure.
In elementary mathematics texts the point is often made that ‘cartesian product is not associative’. When you saw this you may have thought in your heart
of hearts that (A × B) × C and A × (B × C) are nevertheless really the same.
Well, now you know that they are really the same in a very strong sense: they
satisfy the same specification and so carry exactly the same information. The
only difference is in implementation.
5.3.5 If all the factors in an n-ary product are the same object A, the nary product A × A × · · · × A is denoted An
. This suggests the possibility of
defining the nullary product A0 and the unary p
】
```

