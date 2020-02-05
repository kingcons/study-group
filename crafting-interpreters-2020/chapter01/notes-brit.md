## Chapter 1 - Introduction

> That’s my goal for you. I want you to come away with a solid intuition of how a real language
> lives and breathes. My hope is that when you read other, more theoretical books later, 
> concepts there will firmly stick in your mind, adhered to this tangible substrate.

### Reasons to learn

1. DSLs are commonplace
2. It's a good programming workout.
3. It makes computing less magical.

> Later, the mixture of awe and terror my college friends used to refer to their compilers class was
> enough to convince me language hackers were a different breed of human. Some sort of wizards
> granted privileged access to arcane arts. It’s a charming image, but it has a darker side.
> I didn’t feel like a wizard, so I was left thinking I lacked some in-born quality necessary to
> join the cabal. ... That “magical” quality, that sense of exclusivity, excluded me.

...

> When I did finally start cobbling together my own little interpreters, I quickly learned that,
> of course, there is no magic at all. It’s just code, and the people who hack on languages are
> just people.

🎉 We can all be compiler hackers 🎉

### The Book

Part 1 - What will we cover, The Lox Language
Part 2 - A Tree-Walking Interpreter for Lox
Part 3 - A Bytecode Virtual Machine for Lox

### The Code

Build system is not detailed in the text. Code style is designed for expository purposes.
Interpreter is available [on github][jlox] if you want to experiment with the lox language.

[jlox]: https://github.com/munificent/craftinginterpreters/tree/master/java/com/craftinginterpreters

> We will abstain from using them (lex/yacc) here. I want to ensure there are no dark corners where magic
> and confusion can hide, so we’ll write everything by hand. As you’ll see, it’s not as bad as it
> sounds and it means you really will understand each line of code and how both interpreters work.

We'll write our parser by hand rather than pulling from the cornucopia of tools that take a grammar
definition and output source to parse the language for you.

Problem sets are present in the book to help you go beyond the material in the chapter.
Some research may be required.

#### Aside: Self-hosting & Bootstrapping

> A compiler reads in files in one language and translates them to files in another language.
> You can implement a compiler in any language, including the same language it compiles, a process
> called “self-hosting”. You can’t compile it using itself yet, but if you have another compiler for
> your language written in some other language, you use that one to compile your compiler once. Now
> you can use the compiled version of your own compiler to compile future versions of itself and you
> can discard the original one. This is called “bootstrapping”.

### First Interpreter: jlox

The interpreter code will be Object Oriented Java because it's widely used, relatively high level,
and statically typed. The interpreter produced will be clear and complete but slow.

### Second Interpreter: clox

The interpreter will be a bytecode VM written in C, much like the interpreters for Ruby, Python, Lua, etc.
We'll need to build things ourselves rather than piggyback on the "host" language as we did in Java.
We'll build a Garbage Collector, Hash Tables and resizable arrays, etc. Crucially, our implementation will
be fast enough to use in the real world.
