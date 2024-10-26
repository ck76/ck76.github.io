- https://www.cs.cmu.edu/~aldrich/papers/classic/



## Classic Papers

The following is a (non-exhaustive) list of papers I think are particularly important in the areas where I work. They are a good starting point for students who do research with me. I have tried to choose papers that are not just influential, but still a good source for the material for junior students. At the bottom of this page is a list of selected papers from my own research group--not old enough to be "classic" but important for understanding the work we're currently doing.

### Object-Oriented Programming

- Daniel H. H. Ingalls. [The Smalltalk-76 Programming System Design and Implementation](http://wiki.squeak.org/squeak/uploads/400/Smalltalk-76.pdf). Proc. Principles of Programming Languages, 1978.

- Alan C. Kay.

   

  The Early History of Smalltalk

  . Proc. History of Programming Languages, 1993.

  - [official](http://portal.acm.org/citation.cfm?id=155364) and [externally hosted](http://www.iam.unibe.ch/~ducasse/FreeBooks/SmalltalkHistoryHOPL.pdf) scans--hard to read
    [HTML](http://www.accesscom.com/~darius/EarlyHistoryST.html)--easy to read but missing figures

- David Ungar and Randall Smith. [Self: the Power of Simplicity](http://selflanguage.org/_static/published/self-power.pdf). Proc. Object-Oriented Programming, Systems, Languages, and Applications, 1987.

- William R. Cook. [On Understanding Data Abstraction, Revisited](http://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf). Onward! Essay, 2009. *This is a (very!) recent paper, not a traditional classic, but it is the best summary I know of what makes objects unique.*

- Jonathan Aldrich. [The Power of Interoperability: Why Objects Are Inevitable](http://www.cs.cmu.edu/~aldrich/papers/objects-essay.pdf). Onward! Essay, 2013. *Not yet a classic, but a response to Cook's essay. Cook explains how Objects are unique; my essay is an attempt to explain why that uniqueness matters in practical software engineering.*

### Functional Programming

- John Backus. [Can Programming Be Liberated from the von Neumann Style? A Functional Style and Its Algebra of Programs](http://www.cs.cmu.edu/~crary/819-f09/Backus78.pdf). 1978.

### Type Systems

- For an introduction to type systems, I recommend Pierce's [Types and Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/). If you have no prior background in type theory, I suggest reviewing the material in chapter 2 (discrete math), then reading chapters 3, 5, 8, 9, 11, and 13 with some care. Students interested in object-oriented programming should also study chapter 15 and either read chapter 19 or the Featherweight Java paper. The more advanced material in the book is also great, but the above is a place to start.
- Eric Ernst. [Family Polymorphism](http://www.daimi.au.dk/~eernst/papers/fampol.ps). Proc. European Conference on Object-Oriented Programming, 2002.

### Verification

- C. A. R. Hoare. [An Axiomatic Basis for Computer Programming](http://www.cs.cmu.edu/~crary/819-f09/Hoare69.pdf). 1969.
- Peter Müller, Arnd Poetzsch-Heffter, and Gary T. Leavens. [Modular Invariants for Layered Object Structures](ftp://ftp.inf.ethz.ch/doc/tech-reports/4xx/424.pdf). Science of Computer Programming, 62(3):253-286, 2006.
- Mike Barnett, Robert DeLine, Manuel Fähndrich, K. Rustan M. Leino, and Wolfram Schulte. [Verification of Object-Oriented Programs with Invariants](http://www.jot.fm/issues/issue_2004_06/article2/article2.pdf). Journal of Object Technology, 2004

### Program Analysis for Software Engineering

- Lloyd D. Fosdick and Leon J. Osterweil. [Data Flow Analysis in Software Reliability](http://portal.acm.org/citation.cfm?id=356676). ACM Computing Surveys 8:3 September 1976. *This paper introduced the idea of using dataflow analysis for software reliability purposes, as opposed to compiler optimizations. It checks regular expression based temporal properties, a precursor to typestate.*
- E.M. Clarke, E.A. Emerson, A.P. Sistla. [Automatic verification of finite-state concurrent systems using temporal logic specifications.](http://www.lsv.ens-cachan.fr/~markey/biblio/pdf/toplas8(2)-CES.pdf) TOPLAS volume 8, pp. 244-263, April 1986. *Ed Clarke et al.'s classic paper on model checking.*
- Thomas Ball, Vladimir Levin, Sriram K. Rajamani. [A decade of software model checking with SLAM](http://cacm.acm.org/magazines/2011/7/109893-a-decade-of-software-model-checking-with-slam/fulltext). CACM 54(7):68-76, 2011. *SLAM is one of the most interesting and high-impact program analysis / software model checking projects.*
- William R. Bush, Jonathan D. Pincus and David J. Sielaff. [A static analyzer for finding dynamic programming errors.](https://www.cs.cmu.edu/~aldrich/papers/classic/) Software--Practice and Experience, 30:775-802, 2000. *PREfix is a tool with a pragmatic design that has been used with great effect inside Microsoft*
- Brian Hackett, Manuvir Das, Daniel Wang, and Zhe Yang. [Modular Checking for Buffer Overflows in the Large](https://www.cs.cmu.edu/~aldrich/papers/classic/). ICSE 2006. *How Microsoft found buffer overflow errors in Windows.*
- Dawson Engler, Benjamin Chelf, Andy Chou, and Seth Hallem. [Checking System Rules Using System-Specific, Programmer-Written Compiler Extensions](http://www.stanford.edu/~engler/mc-osdi.pdf). OSDI 2000. *A pragmatic paper describing program analysis technology that went into the successful Coverity startup.*

### Typestate

- Robert E Strom and Shaula Yemini. [Typestate: A Programming Language Concept for Enhancing Software Reliability](https://www.cs.cmu.edu/~aldrich/papers/classic/tse12-typestate.pdf). IEEE Transactions on Software Engineering 12(1):157-171, 1986. *Note: this paper is a very important contribution to the literature, but is unavailable in IEEE's electronic library, so I've made a scanned copy available here for academic use. Please notify me if it because available at IEEE and I will link to that instead.*
- Rob DeLine and Manuel Fahndrich. [Typestates for Objects](http://www.cs.cmu.edu/~aldrich/courses/819/deline-typestates.pdf). ECOOP '04.

### Ownership

- James Noble, Jan Vitek, and John Potter. [Flexible Alias Protection](http://www.cs.cmu.edu/~aldrich/courses/819/flexible-alias-protection.pdf). ECOOP '98.

### Aspect-Oriented Programming

- Gregor Kiczales, John Lamping, Anurag Mendhekar, Chris Maeda, Cristina Videira Lopes, Jean-Marc Loingtier, and John Irwin. [Aspect-Oriented Programming](http://www2.parc.com/csl/groups/sda/publications/papers/Kiczales-ECOOP97/for-web.pdf). Proc. European Conference on Object-Oriented Programming, 1997.
- K. Sullivan, W. G. Griswold, Y. Song, Y. Cai, M. Shonle, N. Tewari, and H. Rajan. [Information Hiding Interfaces for Aspect-Oriented Design](http://www.cs.virginia.edu/papers/p166-sullivan.pdf). ESEC/FSE, 2005.

### Secure Software Engineering

- Ross Anderson. [Security Engineering](http://www.cl.cam.ac.uk/~rja14/book.html). Wiley, also available online. *This is a great basic reference on software security*

### Reuse

Two especially successful models of reuse are componentized applications and frameworks:

- David Coppit and Kevin J. Sullivan. [Multiple Mass-Market Applications as Components](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.31.6011&rep=rep1&type=pdf). ICSE 2000.
- Ralph Johnson. [Frameworks = Components + Patterns](http://www.inf.ufsc.br/~vilain/framework-thiago/p39-johnson.pdf). CACM, 1997.

### Distributed Objects

- Jim Waldo, Geoff Wyant, Ann Wollrath, and Sam Kendall. [A Note on Distributed Computing](https://www.cs.cmu.edu/~aldrich/papers/classic/note-on-distributed-computing.pdf). Sun Microsystems Laboratories Technical Report TR-94-29, 1994.
- Eric Jul, Henry Levy, Norman Hutchinson, and Andrew Black. [Fine-Grained Mobility in the Emberald System](http://www.emeraldprogramminglanguage.org/TOCS-1988-paper.pdf). Transactions on Computer Systems, 1988.

## Key Plaid Papers

These are not-yet-classic papers that are highly relevant to our ongoing research, especially with respect to the Plaid language design.

- [Practical API Protocol Checking with Access Permissions](http://www.cs.cmu.edu/~kbierhof/papers/permission-practice.pdf). Kevin Bierhoff, Nels E. Beckman, and Jonathan Aldrich. In *Proceedings of the European Conference on Object Oriented Programming (ECOOP '09)*, July 2009.
- [Modular Typestate Checking of Aliased Objects](http://www.cs.cmu.edu/~kbierhof/papers/typestate-verification.pdf). Kevin Bierhoff and Jonathan Aldrich. In *Object-Oriented Programming Systems, Languages, and Applications (OOPSLA '07)*, October 2007.
- [Typestate-Oriented Programming](http://www.cs.cmu.edu/~aldrich/papers/onward2009-state.pdf). Jonathan Aldrich, Joshua Sunshine, Darpan Saini, and Zachary Sparks. In *Proceedings of Onward!*, 2009.
- [Concurrency by Default: Using Permissions to Express Dataflow in Stateful Programs](http://www.cs.cmu.edu/~aldrich/papers/onward2009-concurrency.pdf). Sven Stork, Paulo Marques, and Jonathan Aldrich. In *Proceedings of Onward!*, 2009.