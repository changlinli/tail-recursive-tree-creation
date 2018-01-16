import FirstSolution.Trampoline
import org.scalatest._

class TestMain extends FlatSpec with Matchers {

  /**
    *         1
    *        /|\
    *       / | \
    *      2  5  6
    *     /     / \
    *    3     7   8
    *   /
    *  4
    */
  private val edgesTest = List(
    1 -> 2,
    2 -> 3,
    3 -> 4,
    1 -> 5,
    1 -> 6,
    6 -> 7,
    6 -> 8
  )

  private val testTree: Tree[Int] =
    Tree(
      1,
      List(
        Tree(
          2,
          List(
            Tree(
              3,
              List(
                Tree(4, List.empty)
              )
            )
          )
        ),
        Tree(5, List.empty),
        Tree(
          6,
          List(
            Tree(7, List.empty),
            Tree(8, List.empty)
          )
        )
      )
    )

  private def freezeMutableTreeTrampolined[A](tree: ThirdSolution.TreeOfMutableChildren[A]): Trampoline[Tree[A]] = {
    val trampolinedChildren = tree.children.map(x => Trampoline.suspend(freezeMutableTreeTrampolined(x))).toList
    Trampoline.sequence(trampolinedChildren).map{
      Tree(tree.value, _)
    }
  }

  private def freezeMutableTree[A](tree: ThirdSolution.TreeOfMutableChildren[A]): Tree[A] = {
    freezeMutableTreeTrampolined(tree).run
  }

  "FirstSolution" should "be accurate" in {
    FirstSolution.generateTree(1, CommonUtils.getEdgesMap(edgesTest)) should be (testTree)
  }
  "SecondSolution" should "be accurate" in {
    SecondSolution.generateTreeFromSpecificEdgeOrdering(1, edgesTest) should be (testTree)
  }
  "ThirdSolution" should "be accurate" in {
    val mutableSolution = ThirdSolution.generateTree(1, CommonUtils.getEdgesMap(edgesTest))
    freezeMutableTree(mutableSolution) should be (testTree)
  }

}
