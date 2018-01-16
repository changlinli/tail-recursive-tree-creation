import scala.annotation.tailrec

final case class Tree[A](value: A, children: List[Tree[A]])

final case class NonEmptyList[A](head: A, tail: List[A]) {
  def toList: List[A] = head :: tail
}

object CommonUtils {
  @tailrec
  def getRootFromEdgesCandidate[A](candidate: A, edges: List[(A, A)]): A = {
    edges match {
      case Nil => candidate
      case (from, to) :: rest =>
        if (to == candidate)
          getRootFromEdgesCandidate(from, rest)
        else
          getRootFromEdgesCandidate(candidate, rest)
    }
  }

  def getRootFromEdges[A](edges: NonEmptyList[(A, A)]): A =
    getRootFromEdgesCandidate(edges.head._1, edges.toList)

  def getEdgesMap[A](edges: List[(A, A)]): Map[A, List[A]] =
    edges
      .groupBy{case (from, _) => from}
      .mapValues(_.map{case (_, to) => to})
}

/**
  * This demonstrates a general problem for dealing with stack overflows in
  * Scala. In particular this allows for a workaround for the lack of tail call
  * optimization on the JVM. Even though scalac will optimize tail-recursive
  * calls, general tail calls are not optimized, such as in mutual recursion.
  *
  * In particular the general idea is that instead of directly calling a
  * function that potentially might have a deep stack (e.g. a recursive call),
  * we wrap the function call inside a suspension ([[FirstSolution.Trampoline.suspend]]).
  * This prevents us from descending into the function call and we instead
  * return the suspension immediately. Then once we've got our result, we can use
  * [[FirstSolution.Trampoline.run]] to actually run our built-up series of
  * suspensions, which manifest as a bunch of closures on the heap. If all our
  * suspensions are in tail call positions, we take up only constant memory
  * because we immediately remove all references to the outer closure and the GC
  * can immediately clean up after us.
  *
  * Note that this also means the recursive symbol that IntelliJ might give you
  * is a lie. As long as the function call is in a suspension, it doesn't
  * actually get called by the outer function itself, but rather by
  * [[FirstSolution.Trampoline.run]].
  *
  * Whenever we don't have a function call, but instead want to return the
  * result immediately, we use [[FirstSolution.Trampoline.done]] to wrap the
  * result instead of a suspension.
  *
  * This looks like a bunch of code because we have to develop the machinery for
  * general trampolines, but once we've developed trampolines, using it is quite
  * straightforward and we can use it everywhere instead of just this specific
  * problem. In production code I wouldn't implement the trampoline myself, I'd
  * just use the implementation from something like Cats or Scalaz.
  *
  * The main downside of a trampoline is that it incurs a constant overhead for
  * every suspension over a simple function call.
  */
object FirstSolution {

  sealed trait Trampoline[A] {

    @tailrec
    final def run: A = this match {
      case Done(x) => x
      case Suspend(suspension) => suspension().run
    }

    @tailrec
    final def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = this match {
      case Done(x) => f(x)
      case Suspend(suspension) => suspension().flatMap(f)
    }

    def map[B](f: A => B): Trampoline[B] = flatMap(x => Trampoline.done(f(x)))
  }

  final case class Done[A](x: A) extends Trampoline[A]
  final case class Suspend[A](suspension: () => Trampoline[A]) extends Trampoline[A]

  object Trampoline {
    def done[A](x: A): Trampoline[A] = Done(x)

    def suspend[A](x: => Trampoline[A]): Trampoline[A] = Suspend(() => x)

    /**
      * Analogous to [[scala.concurrent.Future.sequence]].
      */
    def sequence[A](xs: List[Trampoline[A]]): Trampoline[List[A]] = 
      xs.foldRight(done(List.empty[A])){
        case (trampolinedX, trampolinedAcc) =>
          // We technically can get by with something weaker than flatMap, but I'll leave this here for simplicity
          for {
            x <- trampolinedX
            acc <- trampolinedAcc
          } yield x :: acc
      }
  }

  def generateTreeTrampolined[A](root: A, edges: Map[A, List[A]]): Trampoline[Tree[A]] = {
    val childrenTrampolined = edges.get(root).toList.flatten.map{
      child => Trampoline.suspend(generateTreeTrampolined(child, edges))
    }
    Trampoline.sequence(childrenTrampolined).flatMap{
      children =>
        Trampoline.done(Tree(root, children))
    }
  }

  def generateTree[A](root: A, edges: Map[A, List[A]]): Tree[A] = {
    generateTreeTrampolined(root, edges).run
  }
}

/**
  * This is another solution that also demonstrates a general technique that can
  * be used outside of this specific problem. Instead of building a tree
  * directly, we instead build a data structure called a zipper.
  *
  * A zipper is a data structure built on top of another data structure (in this
  * case a tree, but another example would be a list). It provides a way of
  * traversing the data structure with a "cursor" or a "focus" and allows for
  * efficient editing of the data underneath the cursor. The intuition of how
  * it does this is that every element in the underlying data structure is
  * identified with a canonical path of how to get to that point.
  *
  * Basically we get at-a-point modification over an immutable data structure,
  * which brings us closer to feature parity with mutable data structures,
  * while keeping all the benefits of immutable data.
  *
  * So for example if you have something like this tree:
  *
  *      a
  *     /\
  *    b c
  *   / \ \
  *  d  e f
  *
  *  The point d gets identified with the path a-b-d. Along with the path we
  *  store the part of the tree we didn't choose. So for example at point a in
  *  the path we store alongside the point the subtree c-f and at the point b
  *  we store the subtree e.
  *
  *  Putting that all together, if we have a tree zipper focused at d, we store
  *  d alongside the path (a, c-f)-(b, e). Note that our zipper is focused on
  *  subtrees rather than single points. This is an equivalent amount of power
  *  because we have easy access to the root value of any subtree.
  *
  *  The name zipper comes from the intuition that as we traverse down the tree,
  *  we are splitting the tree open along the path that we are descending, while
  *  as we traverse back up the tree, we are joining the tree back up along the
  *  path we came from. In other words, traversing in one direction zips open the
  *  data structure and traversing in another direction zips the data structure
  *  back up.
  *
  *  Just like in our [[FirstSolution.Trampoline]] case, this may look like a
  *  bunch of code, but we are creating a reusable data structure that can be
  *  used for many different purposes, not just this problem. Ideally you would
  *  just pull in this data structure from a third-party library so you don't
  *  have to implement it yourself.
  *
  *  This solution requires a certain ordering of the edges, but we can create
  *  that ordering in a tail-recursive way as well.
  */
object SecondSolution {
  final case class PathComponent[A](nodeChosen: A, childrenBefore: List[Tree[A]], childrenAfter: List[Tree[A]])
  final case class TreeZipper[A](current: Tree[A], path: List[PathComponent[A]])

  def treeToZipper[A](x: Tree[A]): TreeZipper[A] = TreeZipper(x, List.empty)

  def singletonZipper[A](x: A): TreeZipper[A] = treeToZipper(Tree(x, List.empty))

  def goUpOneLevel[A](zipper: TreeZipper[A]): Option[TreeZipper[A]] = zipper match {
    case TreeZipper(current, PathComponent(parentValue, peersBefore, peersAfter) :: rest) => 
      val newFocus = Tree(parentValue, peersBefore ++ List(current) ++ peersAfter)
      Some(TreeZipper(newFocus, rest))
    case TreeZipper(_, Nil) =>
      None
  }

  @tailrec
  def goToRoot[A](zipper: TreeZipper[A]): Tree[A] = goUpOneLevel(zipper) match {
    case Some(oneMoreLevel) => goToRoot(oneMoreLevel)
    case None => zipper.current
  }

  @tailrec
  def goUpUntil[A](value: A, zipper: TreeZipper[A]): Option[TreeZipper[A]] = zipper match {
    case TreeZipper(current, _) if current.value == value =>
      Some(zipper)
    case TreeZipper(current, PathComponent(parentValue, peersBefore, peersAfter) :: rest) =>
      // This is very similar to goUpOneLevel, the only reason I don't call it 
      // here is to make this obviously tail recursive
      val newFocus = Tree(parentValue, peersBefore ++ List(current) ++ peersAfter)
      goUpUntil(value, TreeZipper(newFocus, rest))
    case TreeZipper(_, Nil) =>
      None
  }

  /**
    * We don't use this, but it's a good demonstration of how to use a zipper.
    */
  def modifyCurrentFocus[A](f: Tree[A] => Tree[A], zipper: TreeZipper[A]): TreeZipper[A] = {
    zipper.copy(current = f(zipper.current))
  }

  def descendIntoValue[A](value: A, zipper: TreeZipper[A]): Option[TreeZipper[A]] = zipper match {
    case TreeZipper(current, _) =>
      val (nodeChosenOpt, childrenBeforeAcc, childrenAfterAcc) = current.children.foldRight((Option.empty[Tree[A]], List.empty[Tree[A]], List.empty[Tree[A]])){
        case (child, (None, childrenBefore, childrenAfter)) =>
          if (child.value == value) (Some(child), childrenBefore, childrenAfter) else (None, childrenBefore, child :: childrenAfter)
        case (child, (acc @ Some(_), childrenBefore, childrenAfter)) =>
          (acc, child :: childrenBefore, childrenAfter)
      }
      nodeChosenOpt.map{
        nodeChosen =>
          // Note that we are intentionally switching around the children after
          // and children before here. The only reason is because otherwise we
          // end up with a mirrored tree compared to FirstSolution.
          val newPathComponent = PathComponent(current.value, childrenAfterAcc, childrenBeforeAcc)
          TreeZipper(nodeChosen, newPathComponent :: zipper.path)
      }
  }

  def addChildValueAtCurrentLocation[A](child: A, zipper: TreeZipper[A]): TreeZipper[A] = {
    val currentTree = zipper.current
    val newCurrentTree = currentTree.copy(children = Tree(child, List.empty) :: currentTree.children)
    TreeZipper(newCurrentTree, zipper.path)
  }

  /**
    * Descend into a child value from our current focus if it exists, if it
    * doesn't create it and then descend.
    */
  def forceDescend[A](value: A, zipper: TreeZipper[A]): TreeZipper[A] = {
    descendIntoValue[A](value, zipper).getOrElse{
      descendIntoValue(value, addChildValueAtCurrentLocation(value, zipper))
        .get // This get is safe because by adding a child value we're guaranteed we can traverse into it
    }
  }

  def addEdgeToSuitableAncestor[A](from: A, to: A, zipper: TreeZipper[A]): Option[TreeZipper[A]] = {
    val matchingFocusOpt = goUpUntil(from, zipper)
    matchingFocusOpt.map(forceDescend(to, _))
  }

  def generateZipperFromSpecificEdgeOrdering[A](root: A, edges: List[(A, A)]): TreeZipper[A] = {
    edges.foldLeft(singletonZipper(root)){
      case (acc, (from, to)) =>
        addEdgeToSuitableAncestor(from, to, acc).getOrElse(acc)
    }
  }

  /**
    * We need the ordering of our edges to be in "chain order" which is a
    * refinement of topological order. In particular we require the additional
    * property on top of standard topological ordering that if we have an edge
    * from node A to node B, the next edge must either start at B or B must
    * have no children.
    *
    * This ordering can be done in a tail-recursive way, whih I might demonstrate
    * later if I have time.
    */
  def generateTreeFromSpecificEdgeOrdering[A](root: A, edges: List[(A, A)]): Tree[A] = {
    goToRoot(generateZipperFromSpecificEdgeOrdering(root, edges))
  }
}

/**
  * This is another solution. It's a bit of a trick that just sneaks in under
  * the original problem specification because [[Seq]] in Scala actually can
  * be a mutable data structure. So we store the children in a mutable data
  * structure and modify that directly.
  *
  * This is the shortest one by far, but also the hackiest solution; the
  * particular solution we've presented here throws away all the benefits of
  * immutable data and it isn't really generalizable to a lot of other problems.
  */
object ThirdSolution {
  import collection.mutable.ArrayBuffer
  
  final case class TreeOfMutableChildren[A](value: A, children: ArrayBuffer[TreeOfMutableChildren[A]])

  def generateTree[A](root: A, edges: Map[A, List[A]]): TreeOfMutableChildren[A] = {
    val startingTree = TreeOfMutableChildren[A](root, ArrayBuffer.empty)
    var currentFocus = startingTree
    var newChildren = edges.get(root).toList.flatten.map(TreeOfMutableChildren[A](_, ArrayBuffer.empty))
    var queueOfChildrenToVisit = newChildren
    while (queueOfChildrenToVisit.nonEmpty) {
      currentFocus.children ++= newChildren
      currentFocus = queueOfChildrenToVisit.head
      newChildren = edges.get(currentFocus.value).toList.flatten.map(TreeOfMutableChildren[A](_, ArrayBuffer.empty))
      queueOfChildrenToVisit = newChildren ++ queueOfChildrenToVisit.tail
    }
    startingTree
  }
}
