https://matt.might.net/articles/books-papers-materials-for-graduate-students/#cs

[toc]

## For graduate students in computer science

Justin Zobel's [Writing for Computer Science](http://www.amazon.com/gp/product/1852338024/ref=as_li_ss_tl?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1852338024) is the "missing chapter" for computer scientists in Kate Turabian's [Manual for Writers of Research Papers](http://www.amazon.com/gp/product/0226823377/ref=as_li_ss_tl?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0226823377).

For computer science more generally, refer to my post on [what every CS major should know](http://matt.might.net/articles/what-cs-majors-should-know/).



## For graduate students in programming languages

It's difficult to find a good book to get started in programming languages. Part of the problem is that the field has become so vast that no text can cover the entire field. The other part of the problem is that very few texts and papers are written for the introductory reader.

[Benjamin Pierce](http://www.cis.upenn.edu/~bcpierce/)'s [Types and Programming Languages](http://www.amazon.com/gp/product/0262162091?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0262162091) really stands out.

[![img](https://p.ipic.vip/z9psyc.jpg)](http://www.amazon.com/gp/product/0262162091?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0262162091)

It's a comprehensive, readable introduction to both λ-calculus *and* type theory. At the same time, the book holds up well as a reference for advanced research in the field.



The Nielsons' book [Semantics with Applications](http://www.amazon.com/gp/product/1846286913?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=1846286913) [[ps](http://www.daimi.au.dk/~bra8130/Wiley_book/wiley.ps)] [[pdf](http://www.daimi.au.dk/~bra8130/Wiley_book/wiley.pdf)] [[course notes](http://www.daimi.au.dk/~bra8130/Wiley_book/wiley.html)] is almost a perfect introduction to formal semantics. (From looking at the table of contents on amazon, the newer edition looks much better than the edition I have.) It gives a detailed account of the three major semantic paradigms: denotational, axiomatic and operational. A section on applying semantics to static analysis provides a nice gateway to the field. The principal strength of this book is its coverage of semantics for imperative languages due to its use and extension of the same While-based language throughout.

[![img](https://p.ipic.vip/446v1y.jpg)](http://www.amazon.com/gp/product/1846286913?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=1846286913)

The exact same semantic techniques can model functional languages, but this is not covered by the book.



[Shriram Krishnamurthi](http://www.cs.brown.edu/~sk/)'s [Programming Languages: Application and Interpretation](http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/) is also a solid introduction to the field, and it's *free*! This book works well for introducing new students to programming languages in part because it uses Scheme and S-Expression notation, which prevents syntax from distracting away from the core issue of semantics. Perhaps more importantly, S-Expression notation means it's very easy to fire up an interpreter and just try things out.

[Mitch Wand](http://www.ccs.neu.edu/~wand/) and [Dan Friedman](https://www.cs.indiana.edu/~dfried/)'s [Essentials of Programming Languages](http://www.amazon.com/gp/product/0262062798?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0262062798) is also a good introduction to programming languages and a great reference for formal semantics. Like Shriram's, this book also uses Scheme, so it inherits the same advantages.

[![img](https://p.ipic.vip/ojsjqr.jpg)](http://www.amazon.com/gp/product/0262062798?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0262062798)

In addition to being great scientists, Mitch and Dan are all-around good people, and in their writing, that comes across as precise yet friendly and approachable prose. I get the sense they actually want the reader to understand the material.



[Barendregt](http://www.cs.ru.nl/~henk/)'s classic [The Lambda Calculus](http://www.amazon.com/gp/product/0444875085?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0444875085) is an encyclopedia of the type-free λ-calculus. There's a lot in here on long-forgotten aspects of the λ-calculus, including its role in logic and foundations and its relationship to topology. It's almost hard to believe the λ-calculus was so well studied *before* it was foundational to the theory of modern programming languages.

[![img](https://p.ipic.vip/y80y9f.jpg)](http://www.amazon.com/gp/product/0444875085?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0444875085)

The book makes for dense reading. I would not recommend it as an introductory text, but as a handbook during research, it is indispensible. It's a superb complement to [Benjamin Pierce](http://www.cis.upenn.edu/~bcpierce/)'s [Types and Programming Languages](http://www.amazon.com/gp/product/0262162091?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0262162091).



If writing high-performance, correct compilers for functional languages is a goal of yours, [Greg Morrisett](http://www.eecs.harvard.edu/~greg/)'s [Ph.D. thesis](http://www.eecs.harvard.edu/~greg/papers/index.html) is a good read. Greg has done and continues to do a lot of great work, so I would actually recommend any of his papers to more advanced graduate students in programming languages. Greg tends to develop powerful machinery as a byproduct of reaching his underlying goal; I've found that machinery can often be adapted to other research problems.

Much research in programming languages gets implemented in (and for) [Standard ML](http://www.smlnj.org/), [OCaml](http://caml.inria.fr/) and [Haskell](http://www.haskell.org/). One of the major challenges (and eventual joys) of using these languages for those more accustomed to imperative languages is learning to use functional data structures effectively. [Chris Okasaki](http://www.eecs.usma.edu/webs/people/okasaki/)'s [Purely Functional Data Structures](http://www.amazon.com/gp/product/0521663504?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0521663504) provides a full treatment of frequently used data structures in their purely functional form.

[![img](https://p.ipic.vip/4ydnzg.jpg)](http://www.amazon.com/gp/product/0521663504?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0521663504)





## For graduate students in compilers

The recently revamped classic, "the dragon book," is a *great* reference for implementors that want to implement the standard analyses and optimizations that an industrial-strength compiler like GCC has:

[![img](https://p.ipic.vip/nr7zm5.jpg)](http://www.amazon.com/gp/product/0321486811?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0321486811)



In the 1980's, advanced compilers used continuation-passing-style (CPS) as their internal representation. In the 1990's and early 2000's, administrative-normal form (ANF) was in vogue, in part because it didn't require compiler writers to understand continuations. Lately, however, I see more and more rediscovering the unmatched power and simplicity of CPS. [Andrew Appel](http://www.cs.princeton.edu/~appel/)'s [Compiling with Continuations](http://www.amazon.com/gp/product/052103311X?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=052103311X) was written at the zenith of the first CPS epoch, which makes it an unbeatable reference on CPS-based compilation even today:

[![img](https://p.ipic.vip/kergid.jpg)](http://www.amazon.com/gp/product/052103311X?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=052103311X)





## For graduate students in static analysis

[Patrick](http://www.di.ens.fr/~cousot/) and [Rhadia](http://www.enseignement.polytechnique.fr/profs/informatique/Radhia.Cousot/) Cousots' [original paper on abstract interpretation](http://www.di.ens.fr/~cousot/COUSOTpapers/POPL77.shtml), which set an entire paradigm in motion, is a good read even after becoming acquainted with the field. Patrick and Rhadia have shown themselves to be great minds of our time in static analyis, which means that their writing is excellent, but it is often unapproachable to casual readers given its level of sophistication. Their original paper is different: because they're starting a field, they cannot and do not assume any background knowledge.

My start in programming languages came from reading the first three chapters of my [advisor's dissertation](http://www.ccs.neu.edu/home/shivers/citations.html#diss). [Olin Shivers](http://www.ccs.neu.edu/home/shivers/) is a gifted writer, and it's always a pleasure to read his work. His dissertation covers *k*-CFA, an analytic platform from which a number of Ph.D.'s can be launched. In his dissertation, you get to see concepts like domain theory, denotational semantics and abstract interpretation in use. It's probably one of the last major works of that era to use denotational semantics.

This dissertation is best read after or during an intro-level programming languages course which covers formal semantics, but it is still a remarkably self-contained piece of work.

There are not a lot of books written on static analysis. The Nielsons/Hankin book [Principles of Program Analysis](http://www.amazon.com/gp/product/3540654100?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=3540654100) is fairly comprehensive.

[![img](https://p.ipic.vip/2j4f0r.jpg)](http://www.amazon.com/gp/product/3540654100?ie=UTF8&tag=mmamzn06-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=3540654100)

Those with some background in formal semantics should be able to use this book as a reference during their research.



In my static analysis seminar, I use a reading list of classic papers:

1. [A lattice-theoretical fixpoint theorem and its applications.](http://projecteuclid.org/DPubS?service=UI&version=1.0&verb=Display&handle=euclid.pjm/1103044538) Tarski. 1955.
2. [Assigning meaning to programs.](http://www.eecs.berkeley.edu/~necula/Papers/FloydMeaning.pdf) Floyd. 1967.
3. [A unified approach to global program optimization.](http://portal.acm.org/citation.cfm?id=512945) Kildall. 1973.
4. [Abstract interpretation: a unified lattice model for static analysis of programs by construction or approximation of fixpoints.](http://www.di.ens.fr/~cousot/COUSOTpapers/POPL77.shtml) Cousot and Cousot. 1977.
5. [Systematic Design of Program Analysis Frameworks.](http://www.di.ens.fr/~cousot/COUSOTpapers/POPL79.shtml) Cousot and Cousot. 1979.
6. [Control-flow analysis in Scheme.](http://www.ccs.neu.edu/home/shivers/citations.html#pldi88) Shivers. 1988.
7. [Efficiently computing static single assignment form and the control dependence graph.](http://portal.acm.org/citation.cfm?doid=115372.115320) Cytron, Ferrante, Rosen, Wegman, Zadeck. 1991.
8. [Points-to analysis in almost linear time.](http://portal.acm.org/citation.cfm?doid=237721.237727) Steensgaard. 1996.
9. [Parametric shape analysis via 3-valued logic.](http://pages.cs.wisc.edu/~reps/#toplas02) Sagiv, Reps and Wilhelm. 2002.



## Related posts

- [Tips for defending a Ph.D.](http://matt.might.net/articles/phd-defense-tips/)
- [HOWTO: Respond to peer reviews](http://matt.might.net/articles/peer-review-rebuttals/)
- [Tips for work-life balance](http://matt.might.net/articles/work-life-balance/)
- [12 resolutions for grad students](http://matt.might.net/articles/grad-student-resolutions/)
- [HOWTO: Peer review scientific work](http://matt.might.net/articles/how-to-peer-review/)
- [Electric meat](http://matt.might.net/articles/electric-meat/)
- [HOWTO: Get a great letter of recommendation](http://matt.might.net/articles/how-to-recommendation-letter/)
- [Boost productivity: Cripple your technology](http://matt.might.net/articles/cripple-your-technology/)
- [HOWTO: Send and reply to email](http://matt.might.net/articles/how-to-email/)
- [Classroom Fortress: Nine Kinds of Students](http://matt.might.net/articles/nine-kinds-of-students/)
- [The 5+5 Commandments of a Ph.D.](http://matt.might.net/articles/phd-commandments/)
- [10 tips for academic talks](http://matt.might.net/articles/academic-presentation-tips/)
- [10 reasons Ph.D. students fail](http://matt.might.net/articles/ways-to-fail-a-phd/)
- [6 tips for low-cost academic blogging](http://matt.might.net/articles/how-to-blog-as-an-academic/)
- [Get The Illustrated Guide in print; fund Ph.D. students; save lives](http://matt.might.net/articles/fund-phd-students-save-lives/)
- [The illustrated guide to a Ph.D.](http://matt.might.net/articles/phd-school-in-pictures/)
- [A Ph.D. thesis proposal is a contract](http://matt.might.net/articles/advice-for-phd-thesis-proposals/)
- [3 qualities of successful Ph.D. students](http://matt.might.net/articles/successful-phd-students/)
- [HOWTO: Get in to grad school](http://matt.might.net/articles/how-to-apply-and-get-in-to-graduate-school-in-science-mathematics-engineering-or-computer-science/)
- [Academic job hunt advice](http://matt.might.net/articles/advice-for-academic-job-hunt/)