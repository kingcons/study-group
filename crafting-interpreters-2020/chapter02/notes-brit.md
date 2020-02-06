## Chapter 2 - A Map of the Territory

A language and its implementation are separate things, though for many popular
languages (Ruby and Python among them), there is no official spec. In such cases,
the _behavior_ exhibited by the main implementation is often treated as a de facto
spec. A few popular languages with official standards include: C, C++, Java,
Javascript, and Common Lisp.

While language semantics can vary a lot, interpreters and compilers have a pretty
familiar pipeline:
    Source > Parser > Abstract Syntax Tree > Intermediate Representation > Code Generation

In many ways, this resembles continually adding structure and metadata to the initial source
until you reach an Intermediate Representation with enough info to generate machine code.
Some simple "Single-Pass" compilers or interpreters use a technique called
"Syntax Directed translation" to go straight from AST to Code generation.

Not unlike web development, compilers are considered to have a frontend and a backend.
The Frontend consits of the passes up til generating Intermediate Representation.
The Backend is generally just Code Generation and any optimizations that are delayed
til that time (like Register Allocation). The awkwardly named "Middle End" holds the rest.

### Scanning

aka Lexing or Lexical Analysis

Just breaking up strings like `var average = (min + max) / 2;` into their separable parts like
`[VAR, IDENTIFIER(average), LPAREN, IDENTIFIER(min), PLUS, IDENTIFIER(max), RPAREN, DIV, NUMBER(2)]`.
It chunks the input source code into distinguishable pieces without thinking much about the meaning
of the input.

### Parsing

Takes the scanned tokens and turns them into a tree (often called Parse Trees, Abstract Syntax Trees,
or ASTs for short). This is not unlike sentence diagrams if you remember those from middle school.
The parser is generally responsible for recognizing syntax errors, rather than the scanner.

**NOTE:** Parsers have been studied for decades and there are many approaches to constructing them:
Parser Generators (like lex/yacc or bison), Parser Combinators, Pratt Parsers, Earley Parsers, Packrat
Parsers, etc. They have different tradeoffs but we'll avoid these tools for the purposes of building
our own from scratch.

### Static Analysis

Static Analysis is just figuring out as much about the _meaning_ of the code as we can before running it.
Just by (statically) observing the source code. If we were figuring things out by watching the code run, 
that would be _dynamic analysis_.

This is where we figure out if variables or methods are out of scope or, in a typed language, catch type
errors and so on. Usually the things we learn about the code are stored back into the structures created
by the parser so that we can use them later in the compilation process.

### Intermediate Representations

> You can think of a compiler as a pipeline where each stage's job is to organize the code
> in a way that makes the next stage simpler to implement. The frontend is specific to the
> source language and the backend is concerned with the final target the code will run on.

The middle end consists of one or more Intermediate Representations and optimizations passes
to speed up the final generated code. The IR acts as an interface so that optimizations and
complicated transformations don't have to be concerned with the details of the target backend
(whether transpiling to JS or compiling to x86) or the details of the source syntax.

There are many different kinds of Intermediate Representations and most advanced compilers
have more than one. Two particularly popular forms throughout history are SSA and CPS which
one of my favorite language hackers, Andy Wingo, has written about [here][ssa-vs-cps].

The interplay between intermediate representations and optimizations is quite strong.
I thought [this article][sbcg] does a great job explaning why choosing an
Intermediate Representation is still an unsolved problem and an area of active research.

[ssa-vs-cps]: https://wingolog.org/archives/2011/07/12/static-single-assignment-for-functional-programmers
[sbcg]: https://jamey.thesharps.us/2017/06/19/search-based-compiler-code-generation/

### Optimization

> Optimization is a huge part of the programming language business. Many language hackers
> spend their entire careers here, squeezing every drop of performance they can out of their
> compilers to get their benchmarks a fraction of a percent faster. It can become a sort of obsession.

> We’re mostly going to hop over that rathole in this book.

### Code Generation

> Finally, we are in the back end, descending the other side of the mountain.
> From here on out, our representation of the code becomes more and more primitive,
> like evolution run in reverse, as we get closer to something our simple-minded machine can understand.

We have to make a tradeoff here between portable code "i.e. a bytecode VM" and performance.
If we choose portable code, we write a Virtual Machine in C one time, and have our compiler
target the VM. If we choose performance, we have to write a native code backend for each
CPU architecture we want to target. Native x86 code certainly can run faster than a good
bytecode VM but our engineering efforts will be multiplied by the number of architectures
we want to support.

> There is a tension, though. Many optimizations, like register allocation and instruction selection,
> work best when they know the strengths and capabilities of a specific chip. Figuring out which parts
> of your compiler can be shared and which should be target-specific is an art.

### Runtime

We can run the program now but whether there's a bytecode VM or native code, we'll need
some supporting code to run our program, even if it's just the Garbage Collector.
(This can be skipped if your language is low-level enough to require manual memory management.)
If our language lets you check the type of an object at runtime or supports runtime code
generation, it has implications for how the runtime must be constructed.

## Some Other Options

* Single Pass Compilers and Syntax-directed translation: https://en.wikipedia.org/wiki/META_II
* Tree-Walk Interpreters: They're slow but quick to write! Ruby was this way til 1.9!
* Transpiler: C'mon, plenty of stuff compiles to Javascript these days!
* JIT Compiler: A ton of work but allows carefully balancing tradeoffs for high-level languages.
