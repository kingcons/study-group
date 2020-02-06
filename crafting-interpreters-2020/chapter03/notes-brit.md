## Chapter 3 - The Lox Language

Lox is quite simple and very close to Javascript. It's dynamically typed with GC.

As defined, the core language has bools, numbers, strings, and nil as primitive types.
Basic arithmetic, equality, comparison, and logical operators work as you'd expect.
Bitwise operators are not present but feel free to add them. Operator precedence is "C like".

Unfortunately, there is a distinction between expressions and statements.
The semicolon promotes an expression into being a statement.
I am considering trying to omit this in my implementation but until I see
more details of the parser I won't go down that rabbit hole.
What can I say? I'm a Lisper/Rubyist/FPer at heart and like my implicit return.
If it doesn't produce a values (even `nil` or `Bottom`), why does it exist?

Vars, conditionals, and for loops are present as you might expect.
Functions are defined with `fun` but have the calling syntax known from Algol to Javascript.

> An argument is an _actual value_ you pass to a function when you call it.
> A parameter is a variable that _holds the value_ of an argument inside the function.

Functions always have a block delimited by curly bois `{}` as its body and return nil
implicitly if no explicit return value is supplied before block end.

Functions are first class, so they can be nested lexically, stored in vars, passed, and returned.

Lox supports classes because class-based OO is more common than prototypical OO and, aside from self,
most prototypical OO langs (*cough*, JS) go on to use prototype powers to provide a class-like interface.
Class syntax and method syntax matches what you would expect from Javascript and classes are "first class".
Notably, instead of a `constructor` method classes have `init`. Single inheritance is done with `<`.

There is no standard library to speak of besides `print`.