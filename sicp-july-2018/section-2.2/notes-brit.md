### Section 2.2 - Hierarchical Data and the Closure Property

#### 2.2.1 - Representing Sequences

> The difference between the two definitions is not that the computer is performing a different
> process (it isn’t) but that we think about the process differently. In effect, map helps
> establish an abstraction barrier that isolates the implementation of procedures that transform
> lists from the details of how the elements of the list are extracted and combined.

#### 2.2.2 - Hierarchical Structures

> Recursion is a natural tool for dealing with tree structures, since we can often reduce
> operations on trees to operations on their branches, which reduce in turn to operations
> on the branches of the branches, and so on, until we reach the leaves of the tree.

In this section, I've often found it simpler to create Recursive Processes for traversing
trees than Iterative Processes. Trying to reify the current position in the tree as well
as a partially transformed tree is surprisingly tricky! Letting the stack handle it is easy.

#### 2.2.3 - Sequences as Conventional Interfaces

> The value of expressing programs as sequence operations is that this helps us make program
> designs that are modular, that is, designs that are constructed by combining relatively
> independent pieces. We can encourage modular design by providing a library of standard
> components together with a conventional interface for connecting the components in flexible
> ways.

#### 2.2.4 - Example: A Picture Language
