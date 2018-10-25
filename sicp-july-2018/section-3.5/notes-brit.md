# Streams

### Overview

> Stream processing lets us model systems that have state without
> ever using assignment or mutable data. This has important
> implications, both theoretical and practical, because we build
> models that avoid the drawbacks inherent in introducing
> assignment. On the other hand, the stream framework raises
> difficulties of its own, and the question of which modeling
> technique leads to more modular and more easily maintained
> systems remains open.

### 3.5.1 - Streams are Delayed Lists

#### Efficiency Considerations

It's interesting to me that they start mentioning efficiency so
quickly and the `sum-primes` example pointing out the excessive
allocation required by the sequence abstractions reminds me of the
early excitement around "Stream Fusion" in Haskell. A way to get
the modularity and composition of sequence operations with the
performance of an "inlined" / standard iterative style.

#### Thunks

> Our implementation of streams will be based on a special form
> called delay. Evaluating (delay ⟨exp⟩) does not evaluate the
> expression ⟨exp⟩, but rather returns a so-called delayed object,
> which we can think of as a “promise” to evaluate ⟨exp⟩ at some
> future time. As a companion to delay, there is a procedure called
> force that takes a delayed object as argument and performs the
> evaluation—in effect, forcing the delay to fulfill its promise.
> We will see below how delay and force can be implemented, but
> first let us use these to construct streams.

> In general, we can think of delayed evaluation as “demand-driven”
> programming, whereby each stage in the stream process is
> activated only enough to satisfy the next stage. What we have
> done is to decouple the actual order of events in the computation
> from the apparent structure of our procedures. We write
> procedures as if the streams existed “all at once” when, in
> reality, the computation is performed incrementally, as in
> traditional programming styles.

#### OOP vs FP, redux

> The object model approximates the world by dividing it into
> separate pieces. The functional model does not modularize along
> object boundaries. The object model is useful when the unshared
> state of the “objects” is much larger than the state that they
> share. An example of a place where the object viewpoint fails is 
> quantum mechanics, where thinking of things as individual
> particles leads to paradoxes and confusions. Unifying the object 
> view with the functional view may have little to do with
> programming, but rather with fundamental epistemological issues.


### 3.5.5 - Modularity of Functional Programs and of Objects

> This is really remarkable. Even though stream-withdraw implements a
> well-defined mathematical function whose behavior does not change, the
> user’s perception here is one of interacting with a system that has a
> changing state. One way to resolve this paradox is to realize that it is
> the user’s temporal existence that imposes state on the system. If the user 
> could step back from the interaction and think in terms of streams of
> balances rather than individual transactions, the system would appear
> stateless.

> However such a merge is implemented, it must interleave the two transaction
> streams in a way that is constrained by “real time” as perceived by users.
> Thus, in an attempt to support the functional style, the need to merge
> inputs from different agents reintroduces the same problems that the
> functional style was meant to eliminate.

> We began this chapter with the goal of building computational models whose
> structure matches our perception of the real world we are trying to model.
> We can model the world as a collection of separate, time-bound, interacting
> objects with state, or we can model the world as a single, timeless,
> stateless unity. Each view has powerful advantages, but neither view alone
> is completely satisfactory. A grand unification has yet to emerge.
