package chaprer3

import org.scalatest.FlatSpec

class BranchingOutWithFunctorsSpec extends FlatSpec {

  import chaprer3.TreeFunctorInstance._

  it should "map a leaf" in {
    val tree = Leaf(41)
    val actual =  cats.Functor[Tree].map(tree)(_ + 1)
    assert(Leaf(42) == actual)
  }

  it should "map the simplest branch" in {
    val tree = Branch(Leaf(1), Leaf(2))
    val actual =  cats.Functor[Tree].map(tree)(_ * 10)
    assert(Branch(Leaf(10), Leaf(20)) == actual)
  }

  it should "map a branch" in {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val actual =  cats.Functor[Tree].map(tree)(_ * 10)
    assert(Branch(Leaf(10), Branch(Leaf(20), Leaf(30))) == actual)
  }
}
