

[TOC]

# Programming language theory

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwbmbp8wspj306y06ydfp.jpg)

![img](https://p2k.unkris.ac.id/_buku_manual/_baca_blob.php?book=lain&td=2&kodegb=magnify-clip.png)

The lowercase [Greek](https://p2k.unkris.ac.id/IT/en/3065-2962/Greek_9271_p2k-unkris.html) letter λ (lambda) is an unofficial symbol of the field of programming language theory. This usage derives from the [lambda calculus](https://p2k.unkris.ac.id/IT/en/3065-2962/lambda-calculus_10732_p2k-unkris.html), a computational model introduced by [Alonzo Church](https://p2k.unkris.ac.id/IT/en/3065-2962/Alonzo-Church_5464_p2k-unkris.html) in the 1930s and widely used by programming language researchers. It graces the cover of the classic text *[Structure and Interpretation of Computer Programs](https://wiki.edunitas.com/IT/en/114-10/Structure-and-Interpretation-of-Computer-Programs_14966_eduNitas.html)*, and the title of the so-called [Lambda Papers](https://p2k.unkris.ac.id/IT/en/3065-2962/Lambda-Papers_10223_p2k-unkris.html), written by [Gerald Jay Sussman](https://p2k.unkris.ac.id/IT/en/3065-2962/Gerald-Jay-Sussman_9027_p2k-unkris.html) and [Guy Steele](https://wiki.edunitas.com/IT/en/114-10/Guy-Steele_9331_eduNitas.html), the developers of the [Scheme](https://p2k.unkris.ac.id/IT/en/3065-2962/Scheme_3922_p2k-unkris.html) [programming language](https://wiki.edunitas.com/IT/en/114-10/programming-language_3683_eduNitas.html).

**Programming language theory** (**PLT**) is a branch of [computer science](https://p2k.unkris.ac.id/IT/en/3065-2962/computer-science_1370_p2k-unkris.html) that deals with the design, implementation, analysis, characterization, and classification of [programming languages](https://wiki.edunitas.com/IT/en/114-10/programming-language_3683_eduNitas.html) and their individual [features](https://p2k.unkris.ac.id/IT/en/3065-2962/features_3683_p2k-unkris.html#Elements). It falls within the discipline of [computer science](https://p2k.unkris.ac.id/IT/en/3065-2962/computer-science_1370_p2k-unkris.html), both depending on and affecting [mathematics](https://wiki.edunitas.com/IT/en/114-10/mathematics_11283_eduNitas.html), [software engineering](https://p2k.unkris.ac.id/IT/en/3065-2962/software-engineering_4115_p2k-unkris.html) and [linguistics](https://wiki.edunitas.com/IT/en/114-10/linguistics_2724_eduNitas.html). It is a well-recognized branch of computer science, and an active research area, with results published in numerous [journals](https://p2k.unkris.ac.id/IT/en/3065-2962/journals_7385_p2k-unkris.html) dedicated to PLT, as well as in general computer science and engineering publications.

Contents[1 History](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#History)[1.1 1950s](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#1950s)[1.2 1960s](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#1960s)[1.3 1970s](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#1970s)[1.4 1980s](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#1980s)[1.5 1990s](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#1990s)[2 Sub-disciplines and related fields](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#Sub-disciplines_and_related_fields)[2.1 Formal semantics](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#Formal_semantics)[2.2 Type theory](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#Type_theory)[2.3 Program analysis and transformation](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#Program_analysis_and_transformation)[2.4 Comparative programming language analysis](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#Comparative_programming_language_analysis)[2.5 Generic and metaprogramming](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#Generic_and_metaprogramming)[2.6 Domain-specific languages](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#Domain-specific_languages)[2.7 Compiler construction](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#Compiler_construction)[2.8 Run-time systems](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#Run-time_systems)[3 Journals, publications, and conferences](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#Journals.2C_publications.2C_and_conferences)[4 See also](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#See_also)[5 References](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#References)[6 Further reading](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#Further_reading)[7 External links](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#External_links)

## History

In some ways, the history of programming language theory predates even the development of programming languages themselves. The [lambda calculus](https://p2k.unkris.ac.id/IT/en/3065-2962/lambda-calculus_10732_p2k-unkris.html), developed by [Alonzo Church](https://p2k.unkris.ac.id/IT/en/3065-2962/Alonzo-Church_5464_p2k-unkris.html) and [Stephen Cole Kleene](https://p2k.unkris.ac.id/IT/en/3065-2962/Stephen-Cole-Kleene_21796_p2k-unkris.html) in the 1930s, is considered by some to be the world's first programming language, even though it was intended to [*model*](https://p2k.unkris.ac.id/IT/en/3065-2962/model_20276_p2k-unkris.html) computation rather than being a means for programmers to [*describe*](https://wiki.edunitas.com/IT/en/114-10/describe_1369_eduNitas.html) algorithms to a computer system. Many modern functional programming languages have been described as providing a "thin veneer" over the lambda calculus,[[1$$](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#cite_note-1) and many are easily described in terms of it.

The first programming language to be proposed was [Plankalkül](https://p2k.unkris.ac.id/IT/en/3065-2962/Plankalkül_13133_p2k-unkris.html), which was designed by [Konrad Zuse](https://wiki.edunitas.com/IT/en/114-10/Konrad-Zuse_10658_eduNitas.html) in the 1940s, but not publicly known until 1972 (and not implemented until 1998). The first widely known and successful programming language was [Fortran](https://p2k.unkris.ac.id/IT/en/3065-2962/Fortran_1993_p2k-unkris.html), developed from 1954 to 1957 by a team of [IBM](https://p2k.unkris.ac.id/IT/en/3065-2962/IBM_2305_p2k-unkris.html) researchers led by [John Backus](https://wiki.edunitas.com/IT/en/114-10/John-Backus_10371_eduNitas.html). The success of FORTRAN led to the formation of a committee of scientists to develop a "universal" computer language; the result of their effort was [ALGOL 58](https://p2k.unkris.ac.id/IT/en/3065-2962/ALGOL-58_5446_p2k-unkris.html). Separately, [John McCarthy](https://wiki.edunitas.com/IT/en/114-10/John-McCarthy_10378_eduNitas.html) of [MIT](https://p2k.unkris.ac.id/IT/en/3065-2962/MIT_2859_p2k-unkris.html) developed the [Lisp programming language](https://wiki.edunitas.com/IT/en/114-10/Lisp-programming-language_2741_eduNitas.html) (based on the lambda calculus), the first language with origins in academia to be successful. With the success of these initial efforts, programming languages became an active topic of research in the 1960s and beyond.

Some other key events in the history of programming language theory since then:

### 1950s

- [Noam Chomsky](https://p2k.unkris.ac.id/IT/en/3065-2962/Noam-Chomsky_20476_p2k-unkris.html) developed the [Chomsky hierarchy](https://p2k.unkris.ac.id/IT/en/3065-2962/Chomsky-hierarchy_6762_p2k-unkris.html) in the field of [linguistics](https://wiki.edunitas.com/IT/en/114-10/linguistics_2724_eduNitas.html); a discovery which has directly impacted programming language theory and other branches of computer science.

### 1960s

- The [Simula](https://p2k.unkris.ac.id/IT/en/3065-2962/Simula_4042_p2k-unkris.html) language was developed by [Ole-Johan Dahl](https://wiki.edunitas.com/IT/en/114-10/Ole-Johan-Dahl_12373_eduNitas.html) and [Kristen Nygaard](https://p2k.unkris.ac.id/IT/en/3065-2962/Kristen-Nygaard_10678_p2k-unkris.html); it is widely considered to be the first example of an [object-oriented programming language](https://wiki.edunitas.com/IT/en/114-10/object-oriented-programming-language_3289_eduNitas.html); Simula also introduced the concept of [coroutines](https://p2k.unkris.ac.id/IT/en/3065-2962/coroutines_7228_p2k-unkris.html).
- In 1964, Peter Landin is the first to realize [Church](https://p2k.unkris.ac.id/IT/en/3065-2962/Alonzo-Church_5464_p2k-unkris.html)'s [lambda calculus](https://p2k.unkris.ac.id/IT/en/3065-2962/lambda-calculus_10732_p2k-unkris.html) can be used to model programming languages. He introduces the [SECD machine](https://wiki.edunitas.com/IT/en/114-10/SECD-machine_21450_eduNitas.html) which "interprets" lambda expressions.
- In 1965, Landin introduces the J operator, essentially a form of continuation.
- In 1966, Landin introduces [ISWIM](https://p2k.unkris.ac.id/IT/en/3065-2962/ISWIM_10117_p2k-unkris.html), an abstract computer [programming language](https://wiki.edunitas.com/IT/en/114-10/programming-language_3683_eduNitas.html) in his article *The Next 700 Programming Languages*. It is influential in the design of languages leading to the [Haskell](https://wiki.edunitas.com/IT/en/114-10/Haskell_2230_eduNitas.html) programming language.
- In 1967, Christopher Strachey publishes his influential set of lecture notes *Fundamental Concepts in Programming Languages*, introducing the terminology *[R-values](https://p2k.unkris.ac.id/IT/en/3065-2962/R-values_15981_p2k-unkris.html)*, *[L-values](https://p2k.unkris.ac.id/IT/en/3065-2962/R-values_15981_p2k-unkris.html)*, *[parametric polymorphism](https://p2k.unkris.ac.id/IT/en/3065-2962/parametric-polymorphism_3460_p2k-unkris.html)*, and *ad hoc polymorphism*.
- In 1969, J. Roger Hindley publishes *The Principal Type-Scheme of an Object in Combinatory Logic*, later generalized into the Hindley–Milner [type inference](https://wiki.edunitas.com/IT/en/114-10/type-inference_15730_eduNitas.html) algorithm.
- In 1969, Tony Hoare introduces the Hoare logic, a form of axiomatic semantics.
- In 1969, William Alvin Howard observed that a "high-level" proof system, referred to as natural deduction, can be directly interpreted in its intuitionistic version as a typed variant of the [model of computation](https://p2k.unkris.ac.id/IT/en/3065-2962/model_20276_p2k-unkris.html) known as [lambda calculus](https://p2k.unkris.ac.id/IT/en/3065-2962/lambda-calculus_10732_p2k-unkris.html). This became known as the Curry–Howard correspondence.

### 1970s

- In 1970, Dana Scott first publishes his work on denotational semantics.
- In 1972, [Logic programming](https://p2k.unkris.ac.id/IT/en/3065-2962/Logic-programming_11005_p2k-unkris.html) and [Prolog](https://wiki.edunitas.com/IT/en/114-10/Prolog_3690_eduNitas.html) were developed thus allowing computer programs to be expressed as mathematical logic.
- In 1974, [John C. Reynolds](https://p2k.unkris.ac.id/IT/en/3065-2962/John-C.-Reynolds_10374_p2k-unkris.html) discovers System F. It had already been discovered in 1971 by the mathematical logician Jean-Yves Girard.
- From 1975, Sussman and Steele develop the [Scheme programming language](https://p2k.unkris.ac.id/IT/en/3065-2962/Scheme_3922_p2k-unkris.html), a Lisp dialect incorporating lexical scoping, a unified namespace, and elements from the [Actor model](https://p2k.unkris.ac.id/IT/en/3065-2962/Actor-model_5253_p2k-unkris.html) including first-class continuations.
- Backus, at the 1977 ACM Turing Award lecture, assailed the current state of industrial languages and proposed a new class of programming languages now known as [function-level programming](https://wiki.edunitas.com/IT/en/114-10/function-level-programming_8824_eduNitas.html) languages.
- In 1977, Gordon Plotkin introduces [Programming Computable Functions](https://p2k.unkris.ac.id/IT/en/3065-2962/Programming-Computable-Functions_13382_p2k-unkris.html), an abstract typed functional language.
- In 1978, [Robin Milner](https://wiki.edunitas.com/IT/en/114-10/Robin-Milner_13945_eduNitas.html) introduces the Hindley–Milner type inference algorithm for the ML programming language. [Type theory](https://p2k.unkris.ac.id/IT/en/3065-2962/Type-theory_15732_p2k-unkris.html) became applied as a discipline to programming languages, this application has led to tremendous advances in type theory over the years.

### 1980s

- In 1981, Gordon Plotkin publishes his paper on structured operational semantics.
- In 1988, Gilles Kahn published his papar on natural semantics.
- A team of scientists at [Xerox PARC](https://p2k.unkris.ac.id/IT/en/3065-2962/Xerox-PARC_12843_p2k-unkris.html) led by [Alan Kay](https://wiki.edunitas.com/IT/en/114-10/Alan-Kay_17114_eduNitas.html) develop [Smalltalk](https://p2k.unkris.ac.id/IT/en/3065-2962/Smalltalk_4068_p2k-unkris.html), an object-oriented language widely known for its innovative development environment.
- There emerged process calculi, such as the [Calculus of Communicating Systems](https://p2k.unkris.ac.id/IT/en/3065-2962/Calculus-of-Communicating-Systems_6407_p2k-unkris.html) of [Robin Milner](https://wiki.edunitas.com/IT/en/114-10/Robin-Milner_13945_eduNitas.html), and the [Communicating sequential processes](https://wiki.edunitas.com/IT/en/114-10/Communicating-sequential-processes_7020_eduNitas.html) model of C. A. R. Hoare, as well as similar models of concurrency such as the [Actor model](https://p2k.unkris.ac.id/IT/en/3065-2962/Actor-model_5253_p2k-unkris.html) of Carl Hewitt.
- In 1985, The release of [Miranda](https://wiki.edunitas.com/IT/en/114-10/Miranda_11594_eduNitas.html) sparks an academic interest in lazy-evaluated pure functional programming languages. A committee was formed to define an open standard resulting in the release of the Haskell 1.0 standard in 1990.
- [Bertrand Meyer](https://p2k.unkris.ac.id/IT/en/3065-2962/Bertrand-Meyer_6066_p2k-unkris.html) created the methodology [Design by contract](https://p2k.unkris.ac.id/IT/en/3065-2962/Design-by-contract_7659_p2k-unkris.html) and incorporated it into the [Eiffel programming language](https://wiki.edunitas.com/IT/en/114-10/Eiffel-programming-language_1747_eduNitas.html).

### 1990s

- Gregor Kiczales, Jim Des Rivieres and [Daniel G. Bobrow](https://wiki.edunitas.com/IT/en/114-10/Daniel-G.-Bobrow_18202_eduNitas.html) published the book The Art of the Metaobject Protocol.
- Eugenio Moggi and [Philip Wadler](https://p2k.unkris.ac.id/IT/en/3065-2962/Philip-Wadler_13004_p2k-unkris.html) introduced the use of monads for structuring programs written in functional programming languages.

## Sub-disciplines and related fields

There are several fields of study which either lie within programming language theory, or which have a profound influence on it; many of these have considerable overlap. In addition, PLT makes use of many other branches of [mathematics](https://wiki.edunitas.com/IT/en/114-10/mathematics_11283_eduNitas.html), including [computability theory](https://wiki.edunitas.com/IT/en/114-10/computability-theory_17932_eduNitas.html), [category theory](https://p2k.unkris.ac.id/IT/en/3065-2962/category-theory_6482_p2k-unkris.html), and [set theory](https://wiki.edunitas.com/IT/en/114-10/set-theory_14313_eduNitas.html).

### Formal semantics

Main article: [Formal semantics of programming languages](https://p2k.unkris.ac.id/IT/en/3065-2962/Formal-semantics-of-programming-languages_10164_p2k-unkris.html)

Formal semantics is the formal specification of the behaviour of computer programs and programming languages. Three common approaches to describe the semantics or "meaning" of a computer program are denotational semantics, operational semantics and axiomatic semantics.

### Type theory

Main article: [type theory](https://p2k.unkris.ac.id/IT/en/3065-2962/Type-theory_15732_p2k-unkris.html)

Type theory is the study of [type systems](https://p2k.unkris.ac.id/IT/en/3065-2962/type-systems_4499_p2k-unkris.html); which are "a tractable syntactic method for proving the absence of certain program behaviors by classifying phrases according to the kinds of values they compute".[[2$$](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#cite_note-2) Many programming languages are distinguished by the characteristics of their type systems.

### Program analysis and transformation

Main articles: Program analysis and [Program transformation](https://p2k.unkris.ac.id/IT/en/3065-2962/Program-transformation_3678_p2k-unkris.html)

Program analysis is the general problem of examining a program and determining key characteristics (such as the absence of classes of [program errors](https://wiki.edunitas.com/IT/en/114-10/program-errors_14640_eduNitas.html)). Program transformation is the process of transforming a program in one form (language) to another form.

### Comparative programming language analysis

Comparative programming language analysis seeks to classify programming languages into different types based on their characteristics; broad categories of programming languages are often known as [programming paradigms](https://p2k.unkris.ac.id/IT/en/3065-2962/programming-paradigms_3684_p2k-unkris.html).

### Generic and metaprogramming

[Metaprogramming](https://wiki.edunitas.com/IT/en/114-10/Metaprogramming_11438_eduNitas.html) is the generation of higher-order programs which, when executed, produce programs (possibly in a different language, or in a subset of the original language) as a result.

### Domain-specific languages

[Domain-specific languages](https://p2k.unkris.ac.id/IT/en/3065-2962/Domain-specific-languages_1671_p2k-unkris.html) are languages constructed to efficiently solve problems in a particular problem domain.

### Compiler construction

Main article: Compiler construction

[Compiler](https://p2k.unkris.ac.id/IT/en/3065-2962/Compiler_1335_p2k-unkris.html) theory is the theory of writing *compilers* (or more generally, *translators*); programs which translate a program written in one language into another form. The actions of a compiler are traditionally broken up into *syntax analysis* (scanning and [parsing](https://wiki.edunitas.com/IT/en/114-10/parsing_3464_eduNitas.html)), *semantic analysis* (determining what a program should do), *[optimization](https://p2k.unkris.ac.id/IT/en/3065-2962/optimization_12587_p2k-unkris.html)* (improving the performance of a program as indicated by some metric; typically execution speed) and *[code generation](https://wiki.edunitas.com/IT/en/114-10/code-generation_1287_eduNitas.html)* (generation and output of an equivalent program in some target language; often the [instruction set](https://p2k.unkris.ac.id/IT/en/3065-2962/instruction-set_2443_p2k-unkris.html) of a CPU).

### Run-time systems

[Runtime systems](https://wiki.edunitas.com/IT/en/114-10/Runtime-systems_21380_eduNitas.html) refers to the development of programming language runtime environments and their components, including [virtual machines](https://p2k.unkris.ac.id/IT/en/3065-2962/virtual-machines_4620_p2k-unkris.html), [garbage collection](https://wiki.edunitas.com/IT/en/114-10/garbage-collection_8892_eduNitas.html), and [foreign function interfaces](https://p2k.unkris.ac.id/IT/en/3065-2962/foreign-function-interfaces_18889_p2k-unkris.html).

## Journals, publications, and conferences

Conferences are the primary venue for presenting research in programming languages. The most well known conferences include the Symposium on Principles of Programming Languages (POPL), Conference on Programming Language Design and Implementation (PLDI), the International Conference on Functional Programming (ICFP), and the International Conference on Object Oriented Programming, Systems, Languages and Applications ([OOPSLA](https://p2k.unkris.ac.id/IT/en/3065-2962/OOPSLA_12418_p2k-unkris.html)).

Notable journals that publish PLT research include the *ACM Transactions on Programming Languages and Systems* (TOPLAS), *Journal of Functional Programming*, *Journal of Functional and Logic Programming*, and *Higher-Order and Symbolic Computation*.

## See also

- [SIGPLAN](https://wiki.edunitas.com/IT/en/114-10/SIGPLAN_14405_eduNitas.html)
- [Timeline of programming languages](https://p2k.unkris.ac.id/IT/en/3065-2962/Timeline-of-programming-languages_4422_p2k-unkris.html)
- [Very high-level programming language](https://wiki.edunitas.com/IT/en/114-10/Very-high-level-programming-language_22370_eduNitas.html)

## References

1. **[^](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#cite_ref-1)** http://www.c2.com/cgi/wiki?ModelsOfComputation
2. **[^](http://p2k.unkris.ac.id/IT/en/3065-2962/Programming-language-theory_13387_p2k-unkris.html#cite_ref-2)** Benjamin C. Pierce. 2002. Types and Programming Languages. MIT Press, Cambridge, MA, USA.

## Further reading

See also: [Programming language#Further reading](https://p2k.unkris.ac.id/IT/en/3065-2962/features_3683_p2k-unkris.html#Further_reading) and [Semantics of programming languages#Further reading](https://wiki.edunitas.com/IT/en/114-10/Semantics-of-programming-languages#Further-reading_10164_eduNitas.html#Further_reading)

- Abadi, Martín and [Cardelli, Luca](https://wiki.edunitas.com/IT/en/114-10/Cardelli,-Luca_11057_eduNitas.html). *A Theory of Objects*. Springer-Verlag.
- Michael J. C. Gordon. *Programming Language Theory and Its Implementation*. Prentice Hall.
- Gunter, Carl and [Mitchell, John C.](https://p2k.unkris.ac.id/IT/en/3065-2962/Mitchell,-John-C._10373_p2k-unkris.html) (eds.). *Theoretical Aspects of Object Oriented Programming Languages: Types, Semantics, and Language Design*. MIT Press.
- [Harper, Robert](https://wiki.edunitas.com/IT/en/114-10/Harper,-Robert_13940_eduNitas.html). *[Practical Foundations for Programming Languages](http://www.cs.cmu.edu/~rwh/plbook/book.pdf)*. Draft version.
- [Knuth, Donald E.](https://wiki.edunitas.com/IT/en/114-10/Knuth,-Donald-E._7873_eduNitas.html) (2003). *[Selected Papers on Computer Languages](http://www-cs-faculty.stanford.edu/~uno/cl.html)*. Stanford, California: Center for the Study of Language and Information.
- [Mitchell, John C.](https://p2k.unkris.ac.id/IT/en/3065-2962/Mitchell,-John-C._10373_p2k-unkris.html). *Foundations for Programming Languages*.
- [Mitchell, John C.](https://p2k.unkris.ac.id/IT/en/3065-2962/Mitchell,-John-C._10373_p2k-unkris.html). *Introduction to Programming Language Theory*.
- O'Hearn, Peter. W. and Tennent, Robert. D. (1997). *[Algol-like Languages](http://www.eecs.qmul.ac.uk/~ohearn/Algol/algol.html)*. Progress in Theoretical Computer Science. Birkhauser, Boston.
- [Pierce, Benjamin C.](https://p2k.unkris.ac.id/IT/en/3065-2962/Pierce,-Benjamin-C._6053_p2k-unkris.html) (2002). *[Types and Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/main.html)*. MIT Press.
- Pierce, Benjamin C. *Advanced Topics in Types and Programming Languages*.
- Pierce, Benjamin C. *et al.* (2010). *[Software Foundations](http://www.cis.upenn.edu/~bcpierce/sf/)*.

## External links

- [Lambda the Ultimate](http://lambda-the-ultimate.org/policies#Purpose), a community weblog for professional discussion and repository of documents on programming language theory.
- [Great Works in Programming Languages](http://www.cis.upenn.edu/~bcpierce/courses/670Fall04/GreatWorksInPL.shtml). Collected by [Benjamin C. Pierce](https://p2k.unkris.ac.id/IT/en/3065-2962/Pierce,-Benjamin-C._6053_p2k-unkris.html) ([University of Pennsylvania](https://wiki.edunitas.com/IT/en/114-10/University-of-Pennsylvania_15893_eduNitas.html)).
- [Classic Papers in Programming Languages and Logic](http://www.cs.cmu.edu/~crary/819-f09/). Collected by Karl Crary ([Carnegie Mellon University](https://wiki.edunitas.com/IT/en/114-10/Carnegie-Mellon-University_6468_eduNitas.html)).
- [Programming Language Research](http://www.cs.cmu.edu/afs/cs.cmu.edu/user/mleone/web/language-research.html). Directory by Mark Leone.
- [Programming Language Theory Texts Online](http://www.cs.uu.nl/wiki/Techno/ProgrammingLanguageTheoryTextsOnline). At Utrecht University.
- [λ-Calculus: Then & Now](http://turing100.acm.org/lambda_calculus_timeline.pdf) by Dana S. Scott for the ACM Turing Centenary Celebration
- [Grand Challenges in Programming Languages](http://plgrand.blogspot.com/). Panel session at POPL 2009.