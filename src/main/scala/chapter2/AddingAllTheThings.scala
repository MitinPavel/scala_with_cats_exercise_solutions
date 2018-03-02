// 2.5.4 Exercise: Adding All The Things

package chapter2

import cats.implicits._

class SuperAdder {
  def add[A: cats.Monoid](items: List[A]): A = {
    items.fold(cats.Monoid[A].empty)(_ |+| _)
  }
}

object AddingAllTheThings extends App {
  println((new SuperAdder).add(List(1, 2, 3)))
  println((new SuperAdder).add(List(Some(1), None, Some(2))))
}
