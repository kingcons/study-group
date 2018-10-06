# Systems with Generic Operations

The best notes I have on this are largely from the accompanying github issue:

### Overview
Section 2.4 showed us three different strategies for performing **dispatch** based on data type:

1. Having procedures use type tags in data to decide what to do based on the type of data received.
  * This requires adding a branch to every procedure one might use whenever a new type is added.
2. Having an explicit dispatch fn (`apply-generic`) that looks for the "right" procedure in a table.
  * This requires storing the procedures in the table but helps  supports "multimethods" / multiple dispatch where you can choose the function to run based on the type of all of the arguments instead of just one (i.e. "Single Dispatch" as seen in Ruby, Smalltalk, Java, Python, etc).
3. Having the data constructors explicitly embed the procedures in each datum/instance. This is similar to a traditional single-dispatch object system or message passing system.
  * Note: In "real world" single-dispatch systems, the procedures aren't _literally_ stored within the objects as in the naive Scheme example. See: C++ vtables, Javascript Prototypes, etc.

Section 2.5 will continue on from this point exploring how to combine dispatch with _interfaces_ to achieve full polymorphism and explore issues around larger type (or "class") hierarchies. I say "full polymorphism" because previously, even when we did dispatch on type tags they were different _representations_ for the same data. Now, we'll see how to make addition do "the right thing" based on whether it's argument was a number, ratio, complex number, etc. Afterwards, we'll be in good shape to explore assignment/mutable state in chapter 3 to approximate "real" objects.

### Footnote 118

This is one of my favorite footnotes in the book thus far. The footnote is
embedded in a discussion about type coercion and the difficulty of representing
the relationships between object types in a hierarchy. The "diamond problem" in
determining which supertype method to call is mentioned among other things.
Apparently frustrated with the nature and persistence of the problem, they
write:

> This statement, which also appears in the first edition of this book,
> is just as true now as it was when we wrote it twelve years ago. Developing
> a useful, general framework for expressing the relations among different types
> of entities (what philosophers call "ontology") seems intractably difficult.
> The main difference between the confusion that existed ten years ago and the
> confusion that exists now is that now a variety of inadequate ontological
> theories have been embodied in a plethora of correspondingly inadequate
> programming languages. For example, much of the complexity of object-oriented
> programming languages - and the subtle and confusing differences among
> contemporary object-oriented languages - centers on the treatment of generic
> operations on interrelated types. Our own discussion of computational objects
> in Chapter 3 avoids these issues entirely. Readers familiar with
> object-oriented programming will notice that we have much to say in chapter 3
> about local state, but we do not even mention "classes" or "inheritance".
> In fact, we suspect that these problems cannot be adequately addressed in
> terms of computer language design alone, without also drawing on work in
> knowledge representation and automated reasoning.
