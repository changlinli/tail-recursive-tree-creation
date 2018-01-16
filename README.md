Generating a tree without blowing the stack
===========================================

I was given the problem of coming up with a tail-recursive solution in Scala for
creating a structure of the form

```scala
case class Node[A](value: A, children: Seq[Node[A]])
```

from a list of edges and a root value.

I've presented three solutions here that don't blow the stack. The first one and
the third both side-step the problem somewhat. The first one uses trampolines to
move the recursion from the stack to the heap. It is technically tail-recursive
because running the trampoline is a tail-recursive operation. The third solution
focuses only on not blowing the stack and uses mutation, taking advantage of the
loophole that in Scala `Seq` can actually be a mutable collection.

The second solution is the one the attacks the problem most directly, by
constructing a zipper instead of a tree directly.

The code is annotated with comments to describe each of these approaches in more
detail.
