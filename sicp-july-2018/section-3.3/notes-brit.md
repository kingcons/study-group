# Modeling with Mutable Data

> The subtleties of dealing with sharing of mutable data objects reflect the
> underlying issues of “sameness” and “change” that were raised in 3.1.3. We
> mentioned there that admitting change to our language requires that a compound
> object must have an “identity” that is something different from the pieces from
> which it is composed. In Lisp, we consider this “identity” to be the quality
> that is tested by eq?, i.e., by equality of pointers. Since in most Lisp
> implementations a pointer is essentially a memory address, we are “solving the
> problem” of defining the identity of objects by stipulating that a data object
> “itself” is the information stored in some particular set of memory locations
> in the computer. This suffices for simple Lisp programs, but is hardly a
> general way to resolve the issue of “sameness” in computational models.

Footnote 148.
