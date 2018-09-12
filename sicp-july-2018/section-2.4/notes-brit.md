### Section 2.4 - Multiple Representations for Abstract Data

#### 2.4.1 - Representations for Complex Numbers

> By isolating the underlying representations of data objects, we can
> divide the task of designing a large program into smaller tasks that
> can be performed separately. But this kind of data abstraction is not
> yet powerful enough, because it may not always make sense to speak of
> "the underlying representation" for a data object.

> More importantly, programming systems are often designed by many people
> working over extended periods of time, subject to requirements that
> change over time. In such an environment, it is simply not possible for
> everyone to agree in advance on choices of data representation. So in
> addition to the data-abstraction barriers that isolate representation
> from use, we need abstraction barriers that isolate different design
> choices from each other and permit different choices to coexist in a
> single program. Furthermore, since large programs are often created
> by combining pre-existing modules that were designed in isolation, we
> need conventions that permit programmers to incorporate modules into
> larger systems additively, that is, without having to redesign or
> reimplement these modules.

#### 2.4.3 - Data Directed Programming and Additivity

> The general strategy of checking the type of a datum and calling an
> appropriate procedure is called dispatching on type. This is a powerful
> strategy for obtaining modularity in system design. On the other hand,
> implementing the dispatch in the way we have has two significant weaknesses.
> One is that generic interface procedures must know all the representations.
> Another is that the representation procedures cannot share the same name.
> The underlying issue of these weaknesses is that the system is not additive.

> What we need is a means for modularizing the system design even further.
> This is provided by the technique known as data-directed programming.
> To understand how it works, begin with the observation that whenever we
> deal with a set of generic operations on a set of applicable types we are,
> in effect, dealing with a two-dimensional table with the types on one axis
> and the procedures on the other axis. The entries in the table are the
> procedures that implement each operation for each supported type of data.

It's interesting to see that in Footnote 45 they mention dispatching on a
list rather than just a symbol to be able to support multiple dispatch.

This also feels like the first time the authors have used new operators without
giving some explanation for them or understanding of how they are implemented.
`get` and `put` are presumably symbol property list manipulating functions in
Scheme as they are in Common Lisp. (Well, `put` isn't. We just use `setf`.)
Apply is also shown and motivated somewhat sparingly.

#### Message Passing

Footnote 48 is interesting: "One limitation of this organization is it permits
only generic procedures of one argument". This can be seen clearly in the
message passing definition of apply-generic:

```scheme
(define (apply-generic op arg) (arg op))
```

If the data objects are _responsible_ for responding to the messages, then
it doesn't make sense to have "multiple dispatch" in the traditional OO sense
in a Message Passing system. A message can only be answered by one recipient.

This also is interesting in explaining some of the philosophical divide between
single dispatch and multiple dispatch OO systems. Ruby, Java, Smalltalk as
single dispatch vs Common Lisp and Slate as multiple dispatch.
