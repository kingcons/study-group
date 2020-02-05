## Challenges

1. Pick an open source implementation of a language you like. Download the source
   code and poke around in it. Try to find the code that implements the scanner and
   parser. Are they hand-written, or generated using tools like Lex and Yacc? (.l or
   .y files usually imply the latter.)

Guile Scheme: git://git.savannah.gnu.org/guile.git

It doesn't appear to use lex or yacc and the code I believe to be responsible for
parsing lives in `libguile/load.c`.

2. Just-in-time compilation tends to be the fastest way to implement a
   dynamically-typed language, but not all of them use it. What reasons are
   there to not JIT?

From context, "fastest way to implement a dynamically-typed language" refers to
speed of the generated code but that is one of three things we tend to care about
when talking about language implementations:
* How fast can the implementation be developed?
* How quickly can the implementation build software (if there is a compilation step)?
* How quickly does the generated code run?

JIT compilers can be extremely fast but they are the most complicated way to produce
a language. JITs are meant to achieve the performance of a compiler with the latency
of an interpreter. The best of both worlds. But the way they accomplish this is by
including _both_ an interpreter and a compiler! Generally, the program starts running
in the interpreter for latency purposes. As it runs, profiling metadata is collected
about what functions are executing most commonly and those are fed asyncronously to
the compiler. Then, once compiled, future calls to the functions use the compiled
version. But this means jumping between _native machine code_ and the interpreter,
often written in C. It gets especially crazy if you want to recompile a hotspot
executing inside a loop _while the loop runs_. This is called "On Stack Replacement".

Interesting references:
* History of Java's Hotspot and Chrome's V8 dates back to Self language research at Stanford
  * Academic Lineage: David Ungar -> Urs Holzle -> Lars Bak
  * In particular, [Adaptive Optimization for Self][aofs] (1994) was an early paper
    that popularized the idea of observing types at runtime to generate
    type specialized machine code.

[aofs]: https://sites.cs.ucsb.edu/~urs/oocsb/self/papers/urs-thesis.html

3. Most Lisp implementations that compile to C also contain an interpreter that
   lets them execute Lisp code on the fly as well. Why?

Lisp is [the language where metaprogramming is sane][lisp]. As a result,
metaprogramming is very common in the ecosystem and that means you need to
be able to run lisp code as you compile lisp code. Thus, having an interpreter
for any macros that generate code during compilation could prove useful.

[lisp]: lists.warhead.org.uk/pipermail/iwe/2005-July/000130.html