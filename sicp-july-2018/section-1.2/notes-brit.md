### Section 1.2 - Procedures and the Processes they Generate

#### 1.2.1

##### Brit is crazy

```scheme
(define (factorial n)
  (define (iter count product)
    (if (> count n)
        product
        (iter (+ count 1)
              (* count product))))
  (iter 1 1))
```

I have a very minor note (or quibble) about this function.
We waste a function call and stack frame checking the counter _after_ it has reached `n`.

It seems likely that the pedagogical clarity of "if count is greater than n, we have the answer"
was the motivation and an additional stack frame (or branch/goto since this is tail recursive)
is completely negligible, I still slightly prefer this version:

```scheme
(define (factorial n)
  (define (iter count product)
    (if (= count n)
        (* count product)
        (iter (+ count 1)
              (* count product))))
  (iter 1 1))
```

##### Recursive vs Iterative

A _recursive process_ slowly expands in resource usage by building a chain of deferred operations.
It then contracts whenever the recursion "bottoms out". Note that in a tree recursive process,
this can lead to repeating expansions and contractions until the full tree is traversed.
In most languages (and for non-tail recursive programs) this is reflected by call stack growth.

An _iterative process_ can have its state summarized by a fixed number of variables which
completely capture the progress at any point. Interestingly, that would give us a simple
way to stop and resume processes if we could only externally interrupt a function and save
its active state.

Footnote [30][foot-30] is ... pretty wild.

[foot-30]: https://sarabander.github.io/sicp/html/1_002e2.xhtml#DOCF30

##### Recursive Procedure vs Recursive Process

Recursive Procedure: A procedure that refers to itself or calls itself in its own definition.

Recursive Process: A process that uses stack frames to store a series of partial results.

Is my recursive process definition inaccurate? Dunno, but I like the crisp, intuitive feel of it.

##### Tail Recursion

A compiler can perform Tail Call Optimization or "TCO" to turn recursive calls in "tail position"
into the equivalent of a goto or branch instruction, yielding an iterative process. There's an
interesting historical aspect to this, reflected in Olin Shivers' [T History][t-history] on
Paul Graham's personal site. _Because_ functional programs are written in such a different style
it changed the focus of not only the programmers but the compiler writers. Functional language
designers focused on optimizing higher order functions, procedure calls, and recursion because
those are the _natural forms for expressing a functional program_. Iterative language designers
were more focused on optimizing loops because that was where most of the work happened.

From what I gather, it was Shivers' dissertation in 1991 that first showed how to do the sorts
of Loop optimizations of imperative languages in functional languages (or at least lisps). I
don't know when similar intraprocedural analyses to functional languages became common in
imperative languages. Some part of me wonders if compiler research spent the 70s and 80s splitting
off in different directions and only really reached back across in the 90s. I should read more
papers...

[t-history]: http://paulgraham.com/thist.html

#### 1.2.2

##### Tree Recusion

> In general, the number of steps required by a tree-recursive process will be proportional
> to the number of nodes in the tree, while the space required will be proportional to the
> maximum depth of the tree.

#### 1.2.4

##### Exponentiation

I still feel like I miss a deep intution for a lot of the algorithmic work.
Like I grok the basics of what makes something logarithmic or why tree recursion
runs exponential but don't necessarily understand the nuance of finer details.

It's interesting how often linear recursive processes become space constant when made iterative.
Just reifying the state into vars instead of having the stack manage it results in such a change.
And, of course, in practice using less space could mean a real performance difference...
If it produces less branchy code when compiled or helps you hit a cache line, etc.

Question: It's suggested in `fast-expt` that using squaring instead of multiplication takes us
from linear to logarithmic order of growth. In hardware, are exponentiation and multiplication
both "one step"? It's a valid algorithmic speedup in number of steps either way, just curious.

###### O(log(n)) - Logarithmic Growth

Historically, I think of Log Growth as: "the amount of work left is halved each step/iteration".
But there's an interesting dual to that: "each allowed iteration doubles the potential dataset".

#### 1.2.6

##### Probabilistic Methods

> The Fermat test differs in character from most familiar algorithms, in which one computes an
> answer that is guaranteed to be correct. Here, the answer obtained is only probably correct.
> More precisely, if n ever fails the Fermat test, we can be certain that n is not prime. But the
> fact that n passes the test, while an extremely strong indication, is still not a guarantee
> that n is prime.

[Footnote 47][foot-47] is pretty interesting...

[foot-47]: https://sarabander.github.io/sicp/html/1_002e2.xhtml#FOOT47
