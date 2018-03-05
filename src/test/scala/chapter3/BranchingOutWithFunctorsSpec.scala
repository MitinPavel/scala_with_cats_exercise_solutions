package chaprer3

import org.scalatest.FlatSpec

class BranchingOutWithFunctorsSpec extends FlatSpec {

  import cats.implicits._
  import chaprer3.TreeFunctorInstance._

  it should "map a leaf" in {
    assert(Tree.leaf(42) == Tree.leaf(41).map(_ + 1))
  }

  it should "map the simplest branch" in {
    val tree = Tree.branch(Tree.leaf(1), Tree.leaf(2))
    val actual = tree.map(_ * 10)
    assert(Tree.branch(Tree.leaf(10), Tree.leaf(20)) == actual)
  }

  it should "map a branch" in {
    val tree = Tree.branch(Tree.leaf(1), Tree.branch(Tree.leaf(2), Tree.leaf(3)))
    val actual = tree.map(_ * 10)
    assert(Tree.branch(Tree.leaf(10), Tree.branch(Tree.leaf(20), Tree.leaf(30))) == actual)
  }
}
