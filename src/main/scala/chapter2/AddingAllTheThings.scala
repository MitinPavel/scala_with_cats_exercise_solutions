// 2.5.4 Exercise: Adding All The Things

package chapter2

import cats.implicits._

class SuperAdder {
  def add(items: List[Int]): Int = {
    items.fold(cats.Monoid[Int].empty)(_ |+| _)
  }
}

object AddingAllTheThings extends App {
  (new SuperAdder).add(List(1, 2, 3))
}
