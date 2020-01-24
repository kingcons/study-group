## Crafting Interpreters - Jan 2020

### Objectives

> If a system is to serve the creative spirit, it must be entirely comprehensible to a single individual. - Dan Ingalls

> We should burn all libraries and allow to remain only that which everyone knows by heart. - Hugo Ball

We seek to improve our grasp on the fundamentals of computation by getting away from frameworks and building from small, well-understood primitives.

We challenge ourselves to look beyond new frameworks or technical trends to essential difficulties in the realization of useful software.

We resolve to acknowledge the rich history of computer science and invigorate our day to day work.

Mostly, we want to have fun and hack the good hack. Come join us!

### Organization

A group leader will be in charge of each session. Volunteering is encouraged.

Prior to the meeting, a github issue should be created and a group leader selected.
Any notes on exercises that should possibly be avoided or potential rabbit holes in
the material should be posted in the issue as well as questions from participants.

During the meeting, important points from discussion can be added in the issue along
with links to PRs for anyone who would like to point out the work on their implementation.

At the end of the session, the issue can be closed and a followup issue opened for the next meeting.

### Implementations

* Brit - [relox](https://github.com/kingcons/relox)
* ...

### Schedule

We will be discussing the reading and our implementations at Calendly's office over lunch.

Meetings will be every other Thursday at Noon, starting February the 6th.
Readings are expected to be completed prior to the meeting so that limited in-person
time can be spent on discussion and code review. While building an interpreter
is not _required_ to participate, it is encouraged to support active learning.

In the interest of completing the book in a reasonable time frame (6 months),
we'll target ~2 chapters per meeting. We can adjust the pace
on the fly as the group deems necessary. The schedule is not set in stone.

The slack channel, `#study-group`, should be used for async discussion and
debugging assistance.

#### Tentative Timeline

> Part 1 - Welcome

* 02/06 - Introduction & A Map of the Territory & The Lox Language (37 pages)

> Part 2 - A Tree-Walk Interpreter

* 02/20 - Scanning & Representing Code (40 pages)
* 03/05 - Parsing Expressions & Evaluating Expressions (33 pages)
* 03/19 - Statements and State & Control Flow (42 pages)
* 04/02 - Functions & Resolving and Binding (46 pages)
* 04/16 - Classes & Inheritance (46 pages)

> Part 3 - A Bytecode Virtual Machine

* 04/30 - Chunks of Bytecode & A Virtual Machine (47 pages)
* 05/14 - Scanning on Demand & Compiling Expressions (45 pages)
* 05/28 - Types of Values & Strings (35 pages)
* 06/11 - Hash Tables (25 pages)
* 06/25 - Global Variables & Local Variables (35 pages)
* 07/09 - Jumping Back and Forth & Calls and Functions (57 pages)
* 07/23 - Closures (38 pages)
* 08/06 - Garbage Collection (29 pages)
* 08/20 - Classes and Instances & Methods and Initializers (not yet released)
* 09/03 - Superclasses & Optimization (not yet released)

### Resources

#### Textbook

The textbook is of course, Crafting Interpreters.
The author has graciously provided it online [for free][ci].

[ci]: https://craftinginterpreters.com/

#### Implementation Language

While the book builds their interpreter using Java and their bytecode VM in C,
you are encouraged to use whatever language suits your fancy for your implementation.

I'll likely be using ReasonML for the interpreter and C or Zig for the VM.

If you'd like to contribute written notes or other material for any chapters,
don't hesitate to ask to be added as a collaborator on the study-group
repo!

#### Youtube Lectures, Papers, Blog posts, etc

* To be added as they're discovered...