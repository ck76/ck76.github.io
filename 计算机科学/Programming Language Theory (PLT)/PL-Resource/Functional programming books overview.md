- https://alexott.net/en/fp/books

***Unfortunately, I don't have time anymore to update this list, the last additions were about 2 years ago :-(\***

The first variant of this article was published in the [first issue](http://fprog.ru/2009/issue1/) of Russian magazine "[Practice of functional programming](http://fprog.ru/)", but I decided to continue to maintain it, as more books were released ([Russian version of this article](https://alexott.net/ru/fp/books/) also includes description of books published in Russian). You can leave comments and suggestions in the comment widget on this page, or send them to me via [e-mail](mailto:alexott@gmail.com) (Updates to this page usually happening not so often — every 2-3 months).

Descriptions for the books are relatively short — just to give an overview of the book's topics, otherwise this article will become too big. For some of books there are more detailed reviews [published in my blog](http://alexott.blogspot.de/search/label/book). You can also follow [my reviews on Goodreads](https://www.goodreads.com/author/show/5350963.Alex_Ott).

*If you will order some of these books, please (if possible), use links from this page — this allows me to buy new books and add them to review*.

- [Functional Programming in General](https://alexott.net/en/fp/books/#sec1)

  [Type Theory, Category Theory, etc.](https://alexott.net/en/fp/books/#sec2)[Implementation of Functional Programming Languages](https://alexott.net/en/fp/books/#sec3)[Overview Books About Different Languages](https://alexott.net/en/fp/books/#sec4)[Functional Programming for Programmers in Other Languages](https://alexott.net/en/fp/books/#sec5)

- [Specific Functional Programming Languages](https://alexott.net/en/fp/books/#sec6)

  [Haskell](https://alexott.net/en/fp/books/#sec7)[Erlang](https://alexott.net/en/fp/books/#sec8)[Caml & Objective Caml](https://alexott.net/en/fp/books/#sec9)[F#](https://alexott.net/en/fp/books/#sec10)[Standard ML](https://alexott.net/en/fp/books/#sec11)[Lisp](https://alexott.net/en/fp/books/#sec12)[Scheme](https://alexott.net/en/fp/books/#sec13)[Prolog](https://alexott.net/en/fp/books/#sec14)[Scala](https://alexott.net/en/fp/books/#sec15)[Clojure](https://alexott.net/en/fp/books/#sec16)

- [Recommendations](https://alexott.net/en/fp/books/#sec17)

Functional programming has very long history and a lot of books was released in paper & electronic forms. These books covers all areas, from theoretical foundations of functional programming to programming in concrete languages & frameworks.

## Functional Programming in General

In this list I tried to collect books, that are dedicated to functional programming in general, including theoretical foundations of lamda-calculus, etc.:

- [Structure and Interpretation of Computer Programs](http://www.amazon.com/gp/product/0262510871/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262510871) — classical textbook on foundations of programming, but it provides introduction to functional programming techniques & uses Scheme as main language. The full version is also available [online on site of MIT Press](http://mitpress.mit.edu/sicp/);
- [Purely Functional Data Structures](http://www.amazon.com/gp/product/0521663504?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0521663504) — wonderful book by Chris Okasaki on how to work with complex data structures in pure functional programming languages. I need to mention, that there is freely available [theses with the same name](http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf), but book's content is slightly different from them;
- [The Functional Approach to Programming](http://www.amazon.com/gp/product/0521576814?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0521576814) book by Guy Cousineau & Michel Mauny), describes functional programming in general and could be used as a textbook (with examples in Caml);
- [Introduction to Functional Programming](http://www.cl.cam.ac.uk/teaching/Lectures/funprog-jrh-1996/index.html) — lecture notes for FP course by John Harrison that he taught in Cambridge University. This course provides description of lambda-calculus, introduction to types and shows how this could be used in Caml Light;
- [An Introduction to Functional Programming Through Lambda Calculus](http://www.amazon.com/gp/product/0486478831/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0486478831) — one more introduction to functional programming & lambda calculus;
- The [Pearls of Functional Algorithm Design](http://www.amazon.com/gp/product/0521513383?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0521513383) book by Richard Bird, shows how to implement different tasks & algorithms using functional programming;
- The [Algorithms: A Functional Programming Approach](http://www.amazon.com/gp/product/0201596040?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0201596040) describes implementation of different algorithms (including some from "Purely Functional Data Structures") in pure functional programming languages. Examples are written in Haskell;
- The [Programming Languages: Application and Interpretation](http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/) book by Shriram Krishnamurthi is textbook for course "Programming languages". It reviews different aspects of design and development of programming languages. All examples are in Scheme;
- The [How to Design Programs: An Introduction to Programming and Computing](http://www.amazon.com/gp/product/0262062186?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262062186) book (it also available [online](http://htdp.org/) and distributed with [PLT Scheme](http://plt-scheme.org/)) is a textbook about programming in general. In demonstrates different approaches to programs development (using Scheme for examples);
- The [Concrete Abstractions. An Introduction to Computer Science Using Scheme](https://gustavus.edu/+max/concrete-abstractions.html) book (also freely available) is an introduction to core concepts of computer science & programming;
- The [Lambda-Calculus and Combinators: An Introduction](http://www.amazon.com/gp/product/0521898854?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0521898854) books provides introduction to theoretical foundations of lambda-calculus;
- [The Lambda Calculus. Its Syntax and Semantics](http://www.amazon.com/gp/product/184890066X/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=184890066X&linkCode=as2&tag=aleottshompag-20) describes theoretical foundations of lambda calculus in great details;
- [Pattern Calculus: Computing with Functions and Structures](http://www.amazon.com/gp/product/3540891846/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=3540891846&linkCode=as2&tag=aleottshompag-20) tries to describes dependencies between pattern calculus, lambda-calculus, programming languages, etc.
- [Foundations for Programming Languages (Foundations of Computing)](http://www.amazon.com/gp/product/0262133210/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262133210) — this book discusses type theory & implementation of programming languages, including functional one.
- [Practical Foundations for Programming Languages](http://www.amazon.com/gp/product/1107029570/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1107029570&linkCode=as2&tag=aleottshompag-20) by Robert Harper describes foundations of programming languages, based on the type theory. This draft version of this book is also [freely available](http://www.cs.cmu.edu/~rwh/plbook/book.pdf).
- The [Programming Language Concepts (Undergraduate Topics in Computer Science)](http://www.amazon.com/gp/product/1447141555/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1447141555&linkCode=as2&tag=aleottshompag-20) book uses F# to demonstrate basic concepts of programming languages, and also describes some problems of implementation, such as garbage collection, etc., including implementation of small toy language.
- [The Functional Approach to Data Management: Modeling, Analyzing and Integrating Heterogeneous Data](http://www.amazon.com/gp/product/3642055753/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=3642055753&linkCode=as2&tag=aleottshompag-20) book describes how functional programming approaches could be used for work with data.
- [Lambda Calculus with Types (Perspectives in Logic)](http://www.amazon.com/gp/product/0521766141/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0521766141&linkCode=as2&tag=aleottshompag-20) - one more book describing typed lambda calculus.
- [The Parametric Lambda Calculus: A Metamodel for Computation](http://www.amazon.com/gp/product/3642057462/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=3642057462&linkCode=as2&tag=aleottshompag-20) describes the metamodel for reasoning about different types of computations.
- [Lambda-calculus, Combinators and Functional Programming](http://www.amazon.com/gp/product/0521114292/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0521114292&linkCode=as2&tag=aleottshompag-20) provides and introduction into lambda calculus and combinators.

More freely available books you can find at the site of [Free Tech Books project](http://www.freetechbooks.com/functional-programming-f34.html).

### Type Theory, Category Theory, etc.

Type theory plays central role in functional programming, especially for strongly typed programming languages, like Haskell. Category theory also belongs to the theoretical foundations of functional programming. There are several books dedicated to these topics:

- [Types and Programming Languages](http://www.amazon.com/gp/product/0262162091/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262162091) — classical book on type theory by Benjamin C. Pierce. This books covers different areas of type theory, including type inference and related questions;
- [Advanced Topics in Types and Programming Languages](http://www.amazon.com/gp/product/0262162288/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262162288) — collection of articles on type theory (by different authors);
- [Programming in Martin-Löf's Type Theory: An Introduction](http://www.freetechbooks.com/programming-in-martin-lof-s-type-theory-an-introduction-t429.html) — freely available dedicated to type theory;
- [Type Theory and Functional Programming](http://www.freetechbooks.com/type-theory-and-functional-programming-t428.html) — freely available book about type theory and its relation to functional programming;
- The [Lectures on the Curry-Howard Isomorphism](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.17.7385) book describes relationship between type theory & proof theory; доказательств и теорией типов.
- [Basic Category Theory for Computer Scientists](http://www.amazon.com/gp/product/0262660717?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262660717) is an introduction to category theory from the point of view of computer science;
- [Conceptual Mathematics: A First Introduction to Categories](http://www.amazon.com/gp/product/052171916X?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=052171916X) is a very detailed and very written introduction to category theory, that could be pretty useful when studying functional programming;
- [Categories for the Working Mathematician](http://www.amazon.com/gp/product/1441931236/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1441931236) by Saunders Mac Lane is about basic concepts of category theory, with more concentration on mathematical side of it;
- The [Interactive Theorem Proving and Program Development](http://www.amazon.com/gp/product/3540208542?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=3540208542) book is a practical introduction to the development of proofs and certified programs using Coq;
- [Certified Programming with Dependent Types](http://adam.chlipala.net/cpdt/) (beta-version is freely available) describes how Coq could be used to prove correctness of programs;
- [Software Foundations](http://www.cis.upenn.edu/~bcpierce/sf/), written by Benjamin C. Pierce together with colleagues, is base for course on logic, functional programming & programs correctness (using Coq);
- [Category Theory for Computing Science](ftp://ftp.math.mcgill.ca/barr/pdffiles/ctcs.pdf) is an introduction to category theory;
- [Learn you an Agda and Achieve Enlightenment!](https://github.com/liamoc/learn-you-an-agda) - this book should be an introduction into the Agda language.

### Implementation of Functional Programming Languages

Some of aforementioned books provide some details of implementation of functional programming languages. But there are also books, that are completely dedicated to this topic:

- The [Design Concepts in Programming Languages](http://www.amazon.com/gp/product/0262201755?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262201755) book is dedicated to theoretical & practical aspects of development of programming languages;
- The [The Implementation of Functional Programming Languages](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/) book, written by Simon Peyton Jones in 1987, describes such topics, as lambda-calculus, types infer & checking, pattern matching, etc., and how these things could be used to implement functional programming languages;
- The [Implementing functional languages: a tutorial](http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/) book was written by Simon Peyton Jones & David Lester in 1992, and shows how to implement simple functional programming language;
- Freely available [Functional Programming and Parallel Graph Rewriting](http://clean.cs.ru.nl/contents/Addison__Wesley_book/addison__wesley_book.html) book describes different models of functional programming languages, and uses Clean language as example;
- The [Garbage Collection: Algorithms for Automatic Dynamic Memory Management](http://www.amazon.com/gp/product/0471941484?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0471941484) is completely dedicated to description of different strategies for garbage collection that are often used in functional programming languages. The new edition was published in 2011th under title [The Garbage Collection Handbook: The Art of Automatic Memory Management](http://www.amazon.com/gp/product/1420082795/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1420082795).
- The [Modern Compiler Implementation in ML](http://www.amazon.com/gp/product/0521607647/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0521607647&linkCode=as2&tag=aleottshompag-20) books describes how to implement compilers (parsing, code generation, etc.) using the Standard ML;
- [Compiling with Continuations](http://www.amazon.com/gp/product/052103311X/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=052103311X&linkCode=as2&tag=aleottshompag-20) shows how continuation-passing programming style could be used for performing optimization & programs transformations. The book uses the Standard ML as implementation language.

### Overview Books About Different Languages

There are several books that describe different programming approaches & techniques, and use different programming languages for this task. Usually they mention functional programming languages. I want to mention following books that could provide good overview of functional programming approach:

- The [Advanced Programming Language Design](http://www.amazon.com/gp/product/0805311912?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0805311912) book ([online-версия](http://www.nondot.org/sabre/Mirrored/AdvProgLangDesign/)) contains information about different programming paradigms, including several chapters on functional & logical programming;
- [Concepts, Techniques, and Models of Computer Programming](http://www.amazon.com/gp/product/0262220695/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262220695) describes major programming paradigms & computational models — object-oriented, imperative, functional, etc.
- [Concepts in Programming Languages](http://www.amazon.com/gp/product/0521780985/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0521780985) one more book of John Mitchell dedicated to description of different programming languages;
- [Programming Language Pragmatics, 3ed](http://www.amazon.com/gp/product/0123745144/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0123745144) describes different programming languages, including F# and Scheme;
- [Essentials of Programming Languages](http://www.amazon.com/gp/product/0262062798/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262062798), by author of Little/Seasoned/Reasoned Schemer books. This book describes main concepts of different programming languages.

### Functional Programming for Programmers in Other Languages

In the last years, several books were published, whose main topic is an introduction to functional programming for developers in "traditional" languages, like Java, C#, etc.:

- The [Functional programming for Java developers](http://www.amazon.com/gp/product/1449311032/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1449311032) book (by the author of "Programming Scala") demonstrates main approaches & techniques of functional programming, but is using Java for examples. The main goal of this book — make Java developers interested in new approaches & benefits of functional programming;
- The Wiley, published the [Functional Programming in C#: Classic Programming Techniques for Modern Projects](http://www.amazon.com/gp/product/0470744588/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0470744588) book, that shows how to use functional programming techniques in C#.
- [Functional Programming for the Object-Oriented Programmer](https://leanpub.com/fp-oo) is an introduction into functional programming for people, who has an experience with object-oriented programming. The book uses Clojure to demonstrate described concepts.
- [Functional C](http://www.amazon.com/gp/product/0201419505/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0201419505&linkCode=as2&tag=aleottshompag-20) shows how the programs in C programming language could be written in functional programming style.

## Specific Functional Programming Languages

In this section I collected most interesting (IMHO) books on specific functional programming languages.

Relatively good introduction to specific functional programming languages you can find in the [Seven Languages in Seven Weeks](http://www.amazon.com/gp/product/193435659X/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=193435659X) book, published by Pragmatic Bookshelf. It describes basics of programming in 7 languages (Ruby, Io, Prolog, Scala, Erlang, Clojure, Haskell), including small examples. I think, that this book could be used to understand, will you want to continue to learn particular language, or not.

### Haskell

I want to mention following Haskell-specific books:

- The [Introduction to Functional Programming using Haskell](http://www.amazon.com/gp/product/0134843460?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0134843460) by Richard Bird is a functional programming textbook and it uses Haskell for examples. It describes base concepts of functional programming and their application in Haskell. The book contains many examples & exercises for self study.
- The [Real World Haskell](http://www.amazon.com/gp/product/0596514980?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0596514980) is a great book on Haskell language, because besides description of the language itself, it also shows how to use Haskell in real-life: data parsing, testing, database & GUI programming. You can also read it [online for free](http://book.realworldhaskell.org/read/).
- [The Haskell Road To Logic, Maths And Programming](http://www.amazon.com/gp/product/0954300696?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0954300696) shows how Haskell could be used in mathematics & logic.
- The [Programming in Haskell](http://www.amazon.com/gp/product/0521692695?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0521692695) book, written by Graham Hutton, compactly describes Haskell, and could be used as reference for people, who already know some Haskell, or other functional language, such as, OCaml or Standard ML. This book is also used in the [video-lectures](http://channel9.msdn.com/shows/Going+Deep/C9-Lectures-Dr-Erik-Meijer-Functional-Programming-Fundamentals-Chapter-13-of-13/) by Erik Meijer at Channel9.
- The [Haskell: The Craft of Functional Programming](http://www.amazon.com/gp/product/0201882957/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0201882957) book describes main concepts of Haskell, including separate chapters on data types, type classes, etc.
- [The Haskell School of Expression: Learning Functional Programming through Multimedia](http://www.amazon.com/gp/product/0521644089?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0521644089) shows practical aspects of work with Haskell, and describes relatively complex topics, such as, interaction with external stuff, designing Haskell programs, etc.
- At May 2012, O'Reilly released the [Developing Web Applications with Haskell and Yesod](http://www.amazon.com/gp/product/1449316972/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1449316972) book that describes how to develop web applications with Yesod framework. This book is also [available online](http://www.yesodweb.com/book).
- In August 2013 the O'Reilly plans to release the [Parallel and Concurrent Programming in Haskell: Techniques for Multicore and Multithreaded Programming](http://www.amazon.com/gp/product/1449335942/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1449335942&linkCode=as2&tag=aleottshompag-20) book by Simon Marlow. The draft version of this book you can find on [book's site](http://ofps.oreilly.com/titles/9781449335946/).

Besides books, there are a lot of online materials. I only list most interesting here:

- [Learn You a Haskell for Great Good!: A Guide for Beginners](http://learnyouahaskell.com/) (it's also available as [printed book](http://www.amazon.com/gp/product/1593272839?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1593272839)) — this book is written in very simple style, and I would recommend it as good introduction into Haskell;
- There is [Haskell section](http://en.wikibooks.org/wiki/Haskell) on the Wikibooks project — it contains materials on different topics, with various degrees of complexity, including both theoretical & practical things.
- [A Gentle Introduction to Haskell 98](http://www.haskell.org/tutorial/) is an "official" tutorial on Haskell 98;
- [Yet Another Haskell Tutorial](http://www.cs.utah.edu/~hal/htut/) — one more Haskell tutorial — besides description of language, it contains many examples & exercises.
- [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Haskell/Write/_Yourself/_a/_Scheme/_in/_48/_Hours) — this tutorial allows you to learn Haskell in practice, when writing an interpreter for Scheme programming language.
- [All About Monads](http://www.haskell.org/haskellwiki/All_About_Monads) is tutorial dedicated to theory & practice of monads in Haskell.
- [Thinking Functionally with Haskell](http://pragprog.com/magazines/2012-09/thinking-functionally-with-haskell)
- The Small book [Speeding Through Haskell](http://www.etnassoft.com/biblioteca/speeding-through-haskell/) (site is in Spanish, but book is in English) tries to provide short introduction into this language.
- [The Haskell School of Music — From Signals to Symphonies](http://haskell.cs.yale.edu/euterpea/haskell-school-of-music/) is a textbook about Haskell with focus on computer music concepts and applications.

### Erlang

The [Programming Erlang. Software for a Concurrent World](http://www.amazon.com/gp/product/193435600X?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=193435600X) book, is written by Joe Armstrong. For a long time was the only available book on Erlang, as previous book "[Concurrent Programming in Erlang](http://www.amazon.com/gp/product/0132857928?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0132857928)" was out of print, and it was hard to buy it. The "Programming Erlang" describes Erlang using very simple language and provides information about language itself & its base features. It also briefly describes more complex topics: databases, using the OTP, etc. It's good introduction, but if you need to know more, it's better to take one of the books, described below. In the fall 2013 the publisher plans to release the 2nd edition of this book.

The O'Reilly, released [Erlang Programming](http://www.amazon.com/gp/product/0596518188?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0596518188) in 2009th. It describes both language, and infrastructure-related stuff, including OTP, creation of GUI, test-driven development, debugging and much more. This book is more detailed comparing with Armstrong's.

Besides this, O'Reilly published the [Introducing Erlang](http://www.amazon.com/gp/product/1449331769/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1449331769&linkCode=as2&tag=aleottshompag-20) book that could be used as a short introduction into Erlang & base concepts of OTP. There is also companion for this book called [Études for Erlang](http://shop.oreilly.com/product/0636920030034.do) that contains an exercises not included into first book.

Another book on Erlang, the [Erlang and OTP in Action](http://www.amazon.com/gp/product/1933988789?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1933988789) was released by Manning in 2010th, and almost completely dedicated to use of OTP, and using Erlang with libraries in other languages. This book assume, that reader already knows Erlang.

In the June 2012, the O'Reilly released small book [Building Web Applications with Erlang: Working with REST and Web Sockets on Yaws](http://www.amazon.com/gp/product/1449309968/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1449309968), that describes how to use Yaws framework to develop web-based applications.

Success of the [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) lead to creation of similar project for Erlang: [Learn You Some Erlang for Great Good!](http://www.amazon.com/gp/product/1593274351/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1593274351&linkCode=as2&tag=aleottshompag-20), and you can read it [online for free](http://learnyousomeerlang.com/content).

In the end of 2012th, the Springer company released unusual (for them) book: [Handbook of Neuroevolution Through Erlang](http://www.amazon.com/gp/product/1461444624/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1461444624&linkCode=as2&tag=aleottshompag-20), that describes how Erlang was used for building of neural networks.

Besides these books, several Erlang-related books are planned for release:

- Erlang for .NET Developers — this book was announced at the end of 2011th, but there is no information when it will be released.
- Addison-Wesley plans to release the [Building Scalable Applications with Erlang](http://www.amazon.com/gp/product/0321636465/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0321636465&linkCode=as2&tag=aleottshompag-20) book that should provide an introduction into Erlang & OTP.
- Pragmatic Bookshelf plans to release the [Programming Elixir](http://pragprog.com/book/elixir/programming-elixir) book that describes the Elixir programming language that was designed for Erlang VM.
- O'Reilly also plans to release book about the Elixir language that is called [Introducing Elixir. Getting Started in Functional Programming](http://shop.oreilly.com/product/0636920030584.do).

### Caml & Objective Caml

There are several books about Objective Caml (OCaml) language:

- Most prominent between the is freely available [Developing Applications with Objective Caml](http://caml.inria.fr/pub/docs/oreilly-book/), that describes not only the OCaml language, but also provides many examples on how to develop software using this language.

- One more freely available book, the [Introduction to Objective Caml](http://www.freetechbooks.com/introduction-to-objective-caml-t698.html) was released not so long time ago. This book contains very good description of language, and examples of its use.

- The

   

  OCaml for Scientists

   

  book describes how to use OCaml for "scientific programming"

  > — data processing, mathematics, data visualization & performance optimization.

- One more existing book, [Practical OCaml](http://www.amazon.com/gp/product/159059620X?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=159059620X), describes OCaml language, but as I can see from reviews, it's written not so good as expected by many readers.

- The [OCaml from the Very Beginning](http://www.amazon.com/gp/product/0957671105/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0957671105&linkCode=as2&tag=aleottshompag-20) book is a textbook for OCaml, and besides description of the language, contains a number of exercises (with answers) that could be useful when studying this language.

- The freely available [Think OCaml. How to Think Like a Computer Scientist](http://www.thinkocaml.com/) provides an introduction to computer science & programming, and it uses OCaml as base language.

- Another freely available book — [Unix system programming in OCaml](http://ocamlunix.forge.ocamlcore.org/index.html) by Xavier Leroy and Didier Rémy, explains how to use OCaml for Unix programming, including work with files, sockets, processes, threads, etc.

[The ZINC experiment: an economical implementation of the ML language](http://pauillac.inria.fr/~xleroy/bibrefs/Leroy-ZINC.html) is a technical report, written by Xavier Leroy (author of OCaml) in 1990th, and it contains pretty detailed description of ML-like language implementation. This report could be very interesting for all who wants to know about internals of Caml & OCaml languages.

And in the fall of 2013th, O'Reilly plans to publish the [Real World OCaml](http://www.amazon.com/gp/product/144932391X/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=144932391X&linkCode=as2&tag=aleottshompag-20) book that should be a popular introduction into the OCaml language, like the "Real World Haskell" book was for Haskell. The text of book is also available on [separate site](https://realworldocaml.org/).

### F#

There are several books about F# programming:

- [Foundations of F#](http://www.amazon.com/gp/product/1590597575?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1590597575) was published in 2007th and describes basics of language, plus shows how to program in it using different approaches. It also shows how to use it in practice for graphical user interface & work with databases. The new edition of this book was released in 2009th under title [Beginning F#](http://www.amazon.com/gp/product/1430223898?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1430223898).
- The [Expert F# 3.0](http://www.amazon.com/gp/product/1430246502/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1430246502&linkCode=as2&tag=aleottshompag-20) book discusses more advanced topics, such as interoperation with code in other languages, use of .Net libraries, work with databases, data parsing, asynchronous programming, etc.
- [F# for Scientists](http://www.amazon.com/gp/product/0470242116?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0470242116) is similar to "OCaml for Scientists", but adapted for F#. It contains information on how to use F# for "scientific programming" — data visualization, data processing, etc.
- The [Programming F#](http://www.amazon.com/gp/product/0596153643?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0596153643) book was released by O'Reilly in 2009th, and it contains description of actual F# (at that time) version — starting with basics language (including functional & object-oriented programming approaches). After that, it describes use of .Net libraries, metaprogramming, asynchronous & parallel programming. In appendix there is an information about existing libraries for data processing and visualization. In 2012th, O'Reilly released the [2nd edition](http://www.amazon.com/gp/product/1449320295/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1449320295&linkCode=as2&tag=aleottshompag-20) of this book, that describes F# 3.0.
- Besides this, O'Reilly released one small book dedicated to F#: [Building Web, Cloud, and Mobile Solutions with F#](http://www.amazon.com/gp/product/1449333761/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1449333761&linkCode=as2&tag=aleottshompag-20).
- In the end of 2009th, Manning published the [Functional Programming for the Real World: With Examples in F# and C#](http://www.amazon.com/gp/product/1933988924?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1933988924) book that describes declarative approach to program development, and demonstrates it with examples in F# and C#. This book could be interesting for developers, who're working on the .Net platform, and who want to learn new programming approaches.
- The Flying Frog Consultancy also released the [Visual F# 2010 for Technical Computing](http://www.ffconsultancy.com/products/fsharp_for_technical_computing/?ffb) book that describes version of F# that is provided together with MS Visual Studio 10.
- Wrox also released book on F# called [Professional F# 2.0](http://www.amazon.com/gp/product/047052801X?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=047052801X) — it contains description of language together with many examples.
- There is ebook [Friendly F# (Fun with game programming)](http://www.amazon.com/gp/product/B005HHYIWC/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=B005HHYIWC) that you can buy on Amazon. This book introduces F# through game development.
- The free [The F# Survival Guide](http://ctocorner.com/fsharp/book/) book contains enough information about functional programming and F# language.
- Another useful online resource is [F# for fun and profit](http://fsharpforfunandprofit.com/) that contains many articles about F#.
- The [F# for C# Developers](http://www.amazon.com/gp/product/0735670269/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0735670269&linkCode=as2&tag=aleottshompag-20) book by Microsoft Press is an introduction into F# language for developers who already have an experience in programming C# and using .Net.

The Manning plans to release another book on F#: [F# Deep Dives](http://www.manning.com/petricek2/), that describes how to use F# to solve concrete tasks.

### Standard ML

The Standard ML language is described by several books:

- The [ML for the Working Programmer](http://www.amazon.com/gp/product/052156543X?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=052156543X) book is an introduction to this language, with many examples of its use;
- The [The Little MLer](http://www.amazon.com/gp/product/026256114X?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=026256114X) book is more like succinct reference for this language;
- The [Unix System programming with Standard ML](http://www.tweako.com/unix_system_programming_with_standard_ml_ebook) book ([pdf](http://only.mawhrin.net/~alexey/sysprogsml.pdf)) demonstrates how to use this language to perform real-world tasks;
- The [Elements of ML Programming, ML97 Edition](http://www.amazon.com/gp/product/0137903871?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0137903871) book, describes both language, and how can it be used in practice. This book could be used as an introduction to Standard ML.
- [Elementary Standard ML](http://www.amazon.com/gp/product/0387946217/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0387946217) is simple introduction to Standard ML.
- [Programming in Standard ML](http://www.cs.cmu.edu/~rwh/smlbook/) - the draft version of Robert Harper's book on Standard ML.

Besides these books, there also books that are dedicated to language's standard: [The Definition of Standard ML](http://www.amazon.com/gp/product/0262631814?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262631814) and [The Standard ML Basis Library](http://www.amazon.com/gp/product/0521794781?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0521794781) describe both language & standard library in great details.

### Lisp

During the long history of Lisp development, many books were published about this language (and its variants):

- The [Practical Common Lisp](http://www.amazon.com/gp/product/1590592395/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1590592395&linkCode=as2&tag=aleottshompag-20&l=as2&o=1&a=1590592395) ([free online version](http://www.gigamonkeys.com/book/)) book is very good introduction to programming in Common Lisp. Besides language's description, it provides many useful examples on how to organize your programs, how to use macros & CLOS, use CL for web & network programming, etc. Book shows how to build complete applications using Common Lisp.

- [Paradigms of Artificial Intelligence Programming: Case Studies in Common LISP](http://www.amazon.com/gp/product/1558601910?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1558601910) — classical book by Peter Norvig. It's main topic is artificial intelligence, and it shows how to use Common Lisp to solve some of the tasks in this branch.

- The [ANSI Common Lisp](http://www.amazon.com/gp/product/0133708756?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0133708756) by Paul Graham, is for people, who're just starting to program in Common Lisp. The book contains description of the language together with many example.

- [On Lisp](http://www.paulgraham.com/onlisp.html), also by Paul Graham, is dedicated to more complex tasks of programming in Common Lisp: how to use & write macros, how to use them to create domain-specific languages (DLS), etc.

- The [Let Over Lambda](http://www.amazon.com/gp/product/1435712757?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1435712757) book also dedicated to "more complex" topics of Common Lisp programming — creation & use of macros, "correct" design of programs, etc.

- The [Object-Oriented Programming in Common Lisp: A Programmer's Guide to CLOS](http://www.amazon.com/gp/product/0201175894?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0201175894) and [The Art of Metaobject Protocol](http://www.amazon.com/gp/product/0262610744?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262610744) books provide detailed information on programming with Common Lisp Object System (CLOS). The most part of the second book is dedicated to description of Metaobject Protocol, that is grounding base of the CLOS, and I would recommend it to all, who is interested in information about different approach to object-oriented programming (OOP).

- The [Common Lisp: The Language, 2ed](http://www.amazon.com/gp/product/1555580416?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1555580416) (also [available online](http://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html)) is a complete reference on the Common Lisp language.

- Successful Lisp: How to Understand and Use Common Lisp

   

  is one more book for beginners

  > — it introduces Lisp in relatively easy way. There is also [online version](http://psg.com/~dlamkins/sl/contents.html) of this book.

- [Lisp in Small Pieces](http://www.amazon.com/gp/product/0521545668?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0521545668) is well-known book on Lisp. It describes implementations of Lisp & Scheme, including programming with continuations, implementation of interpreter & compiler for these languages, support for macros, and much more.

- The freely available [Common Lisp: A Gentle Introduction to Symbolic Computation](http://www.cs.cmu.edu/~dst/LispBook/) book is pretty detailed introduction to programming in Common Lisp. This books is also available [as paperback](http://www.amazon.com/gp/product/0486498204/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0486498204&linkCode=as2&tag=aleottshompag-20).

- The Starch Press released in 2010th one more book on Lisp — [Land of LISP: Learn to Program in LISP, One Game at a Time!](http://www.amazon.com/gp/product/1593272812?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1593272812). This book explains Lisp is written in humorous manner, and using comics-like illustrations. More information you can find on the [site of this book](http://landoflisp.com/).

Vsevolod Dyomkin for a long time interviewed many well-known Commom Lisp developers, and now these interviews are available as free ebook: [Lisp Hackers. Interviews with 100x More Productive Programmers](https://leanpub.com/lisphackers).

Several years ago, the O'Reilly planned to release book on Common Lisp under title "Lisp Outside the Box", and several chapters were available on [project's site](http://lisp-book.org/), but this project was abandoned. As you can see from table of contents, it was planned to cover wide range of topics — from Common Lisp's basics, to application development for web & desktop, including how to use concrete libraries & development environments.

### Scheme

There are enough books about Scheme programming language. Following books describe, both language & different aspects of its use. These books could be used as references, and also as addition to "Structure and Interpretation of Computer Programs" and "How to Design Programs", where Scheme is used for examples:

- [The Little Schemer](http://www.amazon.com/gp/product/0262560992?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262560992);
- [The Seasoned Schemer](http://www.amazon.com/gp/product/026256100X?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=026256100X);
- [The Reasoned Schemer](http://www.amazon.com/gp/product/0262562146?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262562146);
- [The Scheme Programming Language, 4ed](http://www.amazon.com/gp/product/026251298X?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=026251298X), also [available online](http://scheme.com/tspl4/).

In the spring of 2013th, the [Realm of Racket](http://www.amazon.com/gp/product/1593274912/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1593274912&linkCode=as2&tag=aleottshompag-20) book was released. This book describes the Racket language (former PLT Scheme). It's similar to "Land of Lisp" book (they have the same author), and provide information in similar manner.

### Prolog

Huge interest to Prolog in the 80-90th, lead to publishing a huge amount of books. Here is only small part of available literature, that I think could be interesting for reader:

- [Prolog Programming for Artificial Intelligence, 4ed](http://www.amazon.com/gp/product/0321417461/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0321417461) is a classical book on Prolog in application to artificial intelligence;
- The [Logic Programming with Prolog](http://www.amazon.com/gp/product/1852339381?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1852339381) is a very good book on Prolog for beginners. It describes basics of logical programming with Prolog, together with examples of concrete tasks;
- [Learn Prolog Now!](http://www.amazon.com/gp/product/1904987176/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1904987176) is an introductory course to programming in Prolog. There is also freely available [online version](http://www.learnprolognow.org/) of this book;
- [Programming in Prolog: Using the ISO Standard](http://www.amazon.com/gp/product/3540006788?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=3540006788) — one more textbook on Prolog, that demonstrates basic programming techniques for this language;
- [Clause and Effect: Prolog Programming for the Working Programmer](http://www.amazon.com/gp/product/3540629718?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=3540629718) is a small introduction to Prolog for developers who are using other programming languages;
- The [The Craft of Prolog](http://www.amazon.com/gp/product/0262512270/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262512270) book is for people, who already learned Prolog, but want to make their knowledge deeper;
- [The Art of Prolog, Second Edition: Advanced Programming Techniques](http://www.amazon.com/gp/product/0262192500?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262192500) book is dedicated to discussion of problems, that are usually not covered in introduction-level books: how to build interpreters & compilers, program transformations, logical programming theory;;
- [The Practice of Prolog](http://www.amazon.com/gp/product/0262514451/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262514451) — this book shows how to design and organize relatively big Prolog programs for solving different problems;
- [Prolog Programming in Depth](http://www.amazon.com/gp/product/013138645X?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=013138645X) is another book on "more complex" aspects of Prolog programming: interaction with external systems, imperative programming in Prolog, building of expert systems, etc.

There is also a number of freely available resources that have some relation to Prolog and logic programming:

- [Prolog and Natural Language Analysis](http://www.mtome.com/Publications/PNLA/prolog-digital.pdf) book
- [Algorithms for Computational Linguistics](http://www.coli.uni-saarland.de/projects/milca/courses/coal/html/)
- [Computational Semantics](http://www.coli.uni-saarland.de/projects/milca/courses/comsem/html/)
- [Data Integration. The Relational Logic Approach](http://logic.stanford.edu/dataintegration/)
- [Warren's Abstract Machine: A Tutorial Reconstruction](http://wambook.sourceforge.net/)
- [Simply Logical. Intelligent Reasoning by Example](http://www.cs.bris.ac.uk/~flach/SimplyLogical.html)
- [A Grammatical View of Logic Programming](http://www.amazon.com/gp/product/0262514443/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0262514443&linkCode=as2&tag=aleottshompag-20)
- [Inductive Logic Programming: From Machine Learning to Software Engineering](http://www.amazon.com/gp/product/0262023938/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0262023938&linkCode=as2&tag=aleottshompag-20)

### Scala

The [Scala](http://www.scala-lang.org/) language became relatively popular, and already used in many companies, such as, Twitter, LinkedIn, etc. Many books were already published for this language (I need to mention, that books published before 2011th, can be incompatible with current versions of the language):

- [Programming in Scala: A Comprehensive Step-by-step Guide](http://www.amazon.com/gp/product/0981531644?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0981531644) (published by Artima Inc.) was a first book about this language. This book contains very detailed description of the language, together with explanation of different programming approaches. Book also covers practical aspects, such as, creation of graphical user interface, etc. The 2nd edition of this book was released in 2011th, and 1st edition became [freely available](http://www.artima.com/pins1ed/).

- [Programming Scala: Scalability = Functional Programming + Objects](http://www.amazon.com/gp/product/0596155956?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0596155956) (O'Reilly, 2009th) provides detailed description of the language, and also covers some complex tasks, such as, creation of domain specific languages, etc. It also provides information about existing tooling for Scala. O'Reilly also made available [free beta-version](http://programming-scala.labs.oreilly.com/) of this book.

- [Programming Scala: Tackle Multi-Core Complexity on the Java Virtual Machine](http://www.amazon.com/gp/product/193435631X?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=193435631X) (Pragmatic Programmers, 2009th) is a short (comparing to other books) introduction to programming in Scala. But this book describes all main topics — interaction with JVM, concurrent programming, pattern matching, etc. It's assumed that reader is familiar with Java.

- [Beginning Scala](http://www.amazon.com/gp/product/1430219890?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1430219890) (Apress, 2009) is similar to previous book — it's practical introduction to Scala programming, showing main features using practical examples.

- The [Steps in Scala: An Introduction to Object-Functional Programming](http://www.amazon.com/gp/product/0521762170?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0521762170) book was released in 2010th — this book provides an introduction to Scala using step-by-step approach, showing language on many examples.

- The [Scala for the Impatient](http://www.amazon.com/gp/product/0321774094/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0321774094) was released in 2012th — it shows how to work with Scala without much theoretical background — everything is illustrated by many examples, starting with base constructs, and up to very complex things, such as, parsing, advanced types, continuations, etc.

- The [Scala in Action](http://www.amazon.com/gp/product/1935182757/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1935182757) book by Manning provides an introduction to Scala for beginners, covering basics of the language, together with many practical examples.

- The [Scala in Depth](http://www.amazon.com/gp/product/1935182706/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1935182706) book (Manning, 2012) should be a companion book for "Scala in Action", and it's concentrated on the "not so easy" side of Scala programming — type system, actors, more functional programming, etc.

- [The Definitive Guide to Lift: A Scala-based Web Framework](http://www.amazon.com/gp/product/1430224215?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1430224215) book (Apress, 2009) provides detailed description of Lift — popular web-framework for Scala.

- In the 2011th, Manning also released book on Lift:

   

  Lift in Action: The Simply Functional Web Framework for Scala

  . This book covers Lift

  1. x using many practical examples to make subject more understandable.

- The [Actors in Scala](http://www.amazon.com/gp/product/0981531652/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0981531652) (Artima, 2012) is completely dedicated to description of actors model of communications that is used in Scala.

- Another useful book to understand Scala's approach to concurrent programming is the [Programming Concurrency on the JVM: Mastering Synchronization, STM, and Actors](http://www.amazon.com/gp/product/193435676X/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=193435676X) — it covers different approaches & models of concurrent programming, including implementations in Scala, Clojure and other languages.

- The Akka framework is often used with Scala, so following books could be useful: [Akka Concurrency](http://www.amazon.com/gp/product/0981531660/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0981531660&linkCode=as2&tag=aleottshompag-20), [Akka Essentials](http://www.amazon.com/gp/product/1849518289/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1849518289&linkCode=as2&tag=aleottshompag-20), and [Composable Futures with Akka 2.0](http://www.slinnbooks.com/books/futures/).

- The [Introduction to the Art of Programming Using Scala](http://www.amazon.com/gp/product/1439896666/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1439896666) book was released in fall 2012. There is also [separate site](http://www.programmingusingscala.net/) where you can find more detailed information about this book.

- The [Testing in Scala](http://www.amazon.com/gp/product/1449315119/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1449315119&linkCode=as2&tag=aleottshompag-20) book describes how to use the ScalaTest and Spec2 testing frameworks, including how to integrate them into SBT's builds.

- The [Pragmatic Enterprise Scala](http://www.amazon.com/gp/product/1484007662/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1484007662&linkCode=as2&tag=aleottshompag-20) book describes how Scala could be used together with Java Enterprise Edition.

There are also freely available materials about this language:

- Besides language's specification, on the [language's site](http://www.scala-lang.org/node/198) is also available documents "A Brief Scala Tutorial" and "Scala by Example", that allows quickly get information about this language. Another useful section on the site is [Learning Scala](http://www.scala-lang.org/node/1305) that contains links to books, lectures, and other study materials.
- Twitter, where Scala is actively used, published freely available several resources for Scala learning: [Scala School! From ∅ to Distributed Service](http://twitter.github.com/scala_school/) and [Effective Scala](http://twitter.github.com/effectivescala/) — both are for people, who is just starting to study this language.

It's also planned to releases several books on Scala soon:

- Appress plans to publish the [Pro Scala: Monadic Design Patterns for the Web](http://www.amazon.com/gp/product/143022844X?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=143022844X) book (although, it still delaying).
- Manning plans to release several books in the near future (all these books are available as part of Manning's early access program (MEAP)):
  - The [Functional Programming in Scala](http://manning.com/bjarnason/) doesn't describe language itself, but shows how to use functional programming approach in Scala.
  - [Play for Scala](http://www.manning.com/hilton/) book describes new web framework Play.
  - [Scalatra in Action](http://www.manning.com/carrero2/) describes the Scalatra web framework that is more simple comparing with Lift or Play.
  - [SBT in Action](http://www.manning.com/suereth2/) describes the SBT - build system that's often used with Scala.
- The [Atomic Scala](http://www.atomicscala.com/book/) should be an introduction into the Scala language.
- The [ScalaCheck: The Definitive Guide](http://www.artima.com/shop/scalacheck) book describes the ScalaCheck library.
- O'Reilly also plans to release in 2013 the [Scala Cookbook](http://www.amazon.com/gp/product/1449339611/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1449339611&linkCode=as2&tag=aleottshompag-20) book that will contain recipes for concrete tasks.
- The Pragmatic Bookshelf publishing plans to release the [Functional Programming Patterns in Scala and Clojure: Write Lean Programs for the JVM](http://www.amazon.com/gp/product/1937785475/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1937785475&linkCode=as2&tag=aleottshompag-20).
- The [Scala on Android: How to do efficient Android programming with Scala](https://leanpub.com/ScalaOnAndroid) book will provide an information how to use Scala to program an Androind applications.

Besides book & web-sites, there are several online courses for Scala, such as http://www.scalacourses.com/ or [Functional Programming Principles in Scala](https://www.coursera.org/course/progfun) (that is taught by Martin Odersky).

### Clojure

[Clojure](http://clojure.org/) is relatively young Lisp-like programming language for Java Virtual Machine (JVM). In contrast to other implementation, Clojure is separate language, not compatible neither with Common Lisp, nor with Scheme. This allowed to implement many interesting things, like immutable data, implicit parallel execution of code, very simple model of concurrent programming, software transactional memory, etc., while keeping two-way interaction with code in Java.

The first book on this language was released by Pragmatic Programmers in 2009 under title [Programming Clojure](http://www.amazon.com/gp/product/1934356336?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1934356336). This book provided pretty good introduction to language, describing all basic features (for version 1.0, that was actual at the time of release). But you still need to use [site of the language](http://clojure.org/Reference) as a reference for functions, and other things. The [2nd edition of this book](http://www.amazon.com/gp/product/1934356867/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1934356867) was released in 2012th, and it was updated to include information about version 1.3, covering all major changes between versions.

The second book on Clojure was released in 2010 as [Practical Clojure. The Definitive Guide](http://www.amazon.com/gp/product/1430272317?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1430272317) — it contains succinct description of language, including new functionality from version 1.2.0 — datatypes & protocols. But this book lacks description of infrastructure-related things, like IDE, build tools, etc.

In 2011th, Manning published 2 books on Clojure: [Clojure in Action](http://www.amazon.com/gp/product/1935182595?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1935182595) and [The Joy of Clojure. Thinking the Clojure Way](http://www.amazon.com/gp/product/1935182641?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1935182641). First books is an introduction to language, where basics of language is described together with many practical examples. While the second book, is for people, who already has some Clojure experience and who want to get more information about "Clojure programming style". More detailed review of this book you can find in [my blog](http://alexott.blogspot.de/2010/10/readings-digest-september-2010.html). And the 2nd edition of "The Joy of Clojure" is currently in development.

O'Reilly also published book on Clojure — the [Clojure Programming](http://www.amazon.com/gp/product/1449394701/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1449394701) was written by well-known Clojure developers. This book provides very good description of Clojure, different libraries, shows how to use this language in practice, and also contains many advice on how to write idiomatic Clojure code.

The Developer.Press also released small book called [Clojure Made Simple](http://www.amazon.com/gp/product/B00BSY20HS/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B00BSY20HS&linkCode=as2&tag=aleottshompag-20) that describes the basics of language — data types, core functions, etc. But this book almost completely lacks description of concurrent programming stuff.

The Packt Publishing also published a book on Clojure: the [Clojure Data Analysis Cookbook](http://www.amazon.com/gp/product/178216264X/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=178216264X&linkCode=as2&tag=aleottshompag-20) describes how Clojure could be used for data analysis, including use of [Incanter](http://incanter.org/) project.

The ClojureScript language, that is developed in parallel with Clojure, allows you to write web applications that are executed in browser. The O'Reilly released a small book [ClojureScript: Up and Running](http://www.amazon.com/gp/product/1449327435/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1449327435&linkCode=as2&tag=aleottshompag-20) that describes basics of development with ClojureScript, including information about existing libraries.

The No Starch Press should also release book on Clojure under title [Meet Clojure](http://meetclj.raynes.me/).

O'Reilly also plans to publish one more book on Clojure - [Clojure Cookbook](http://clojure-cookbook.com/), and everybody can submit recipes that will be included into this book.

## Recommendations

In this section I tried to put only that books, that I can recommend to read to study specific languages.

To study functional programming & Haskell, I can recommend the [Introduction to Functional Programming using Haskell](http://www.amazon.com/gp/product/0134843460?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0134843460) book by Richard Bird. More practical-oriented introductory books are [Real World Haskell](http://www.amazon.com/gp/product/0596514980?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0596514980) and [Learn You a Haskell for Great Good!: A Guide for Beginners](http://www.amazon.com/gp/product/1593272839?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1593272839).

Studying of Erlang it's better to start with either from [Erlang Programming](http://www.amazon.com/gp/product/0596518188?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0596518188) by O'Reilly, or from [Erlang and OTP in Action](http://www.amazon.com/gp/product/1933988789?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1933988789) — both books provides enough information about language, and OTP.

For Scala you can select between several books. If you want to start to use it immediately, then you can choose [Scala for the Impatient](http://www.amazon.com/gp/product/0321774094/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0321774094). But if you want to get more deep knowledge of this language, then the best choice is [Programming in Scala: A Comprehensive Step-by-step Guide](http://www.amazon.com/gp/product/0981531644?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0981531644). And after them you can continue with [Scala in Depth](http://www.amazon.com/gp/product/1935182706/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1935182706) and other books.

To learn Clojure, the most actual books are [Clojure Programming](http://www.amazon.com/gp/product/1449394701/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1449394701) (more detailed) and 2nd edition of [Programming Clojure](http://www.amazon.com/gp/product/1934356867/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1934356867) — the both will provide you enough information about language, together with examples. And after them, I would recommend you to read [The Joy of Clojure. Thinking the Clojure Way](http://www.amazon.com/gp/product/1935182641?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1935182641)!

If you're interested in the Common Lisp, then you can start with [Practical Common Lisp](http://www.amazon.com/gp/product/1590592395/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=1590592395&linkCode=as2&tag=aleottshompag-20&l=as2&o=1&a=1590592395) that will provide enough information to start with language. The more complex things are described in the [On Lisp](http://www.paulgraham.com/onlisp.html), [The Art of Metaobject Protocol](http://www.amazon.com/gp/product/0262610744?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0262610744), [Lisp in Small Pieces](http://www.amazon.com/gp/product/0521545668?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0521545668) and [other books](https://alexott.net/en/fp/books/#lisp-en).

For ML family of languages exists enough books. For OCaml you can start with an *Introduction to Objective Caml*, and use it together with language reference. After that you can take *Developing Applications with Objective Caml* and/or [other languages](https://alexott.net/en/fp/books/#ocaml-en). For F# it good idea to start with [Expert F# 2.0](http://www.amazon.com/gp/product/1430224312?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1430224312) or [Begining F#](http://www.amazon.com/gp/product/1430223898?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1430223898), and continue with [F# for Scientists](http://www.amazon.com/gp/product/0470242116?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0470242116).

For Prolog I would recommend to start with [Prolog Programming for Artificial Intelligence, 4ed](http://www.amazon.com/gp/product/0321417461/ref=as_li_ss_tl?ie=UTF8&tag=aleottshompag-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=0321417461) , and then continue with books, listed [above](https://alexott.net/en/fp/books/#prolog-en).