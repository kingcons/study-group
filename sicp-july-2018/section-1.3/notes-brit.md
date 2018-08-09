### Section 1.3 - Formulating Abstractions with Higher-Order Procedures

#### 1.3.4

> In general, there are many ways to formulate a process as a procedure.
> Experienced programmers know how to choose procedural formulations that are
> particularly perspicuous, and where useful elements of the process are exposed as
> separate entities that can be reused in other applications.

Interesting to see footnote 62 showing that even though Newton's Method may not converge,
it will gain accuracy quickly and so is better for approximation than the half-interval method.

There's also something amazing about the fact that this book basic teaches you the basics
of Calculus in about 40 or 50 pages _while teaching you_ about procedures as an abstraction.

##### On abstraction and refactoring

> As programmers, we should be alert to opportunities to identify the underlying abstractions
> in our programs and to build upon them and generalize them to create more powerful abstractions.
> This is not to say that one should always write programs in the most abstract way possible;
> expert programmers know how to choose the level of abstraction appropriate to their task.

##### On First-Class Linguistic Forms

> In general, programming languages impose restrictions on the ways in which computational
> elements can be manipulated. Elements with the fewest restrictions are said to have first-class
> status. Some of the "rights andf privileges are":
> * Named by variables,
> * Passed as arguments to procedures,
> * Returned as the results of procedures,
> * Included in data structures.

Of course, we haven't seen data structures yet. On to chapter 2!

Footnote 66 is also interesting as it notes that the primary implementation cost of first-class
procedures is in allocating and managing storage for their free variables. Hopefully, I can think
about this more later.
