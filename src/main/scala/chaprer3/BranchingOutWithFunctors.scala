package chaprer3

import cats.Functor
import TreeFunctorInstance._

object BranchingOutWithFunctors extends App {
  val tree = Leaf(123)
  val newTree = Functor[Tree].map(tree)(_ + 1)
  println(newTree)
}
