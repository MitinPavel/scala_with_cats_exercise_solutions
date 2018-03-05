package chaprer3

import cats.implicits._
import TreeFunctorInstance._

object BranchingOutWithFunctors extends App {
  Tree.leaf(100).map(_ * 2)
  Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)
}
