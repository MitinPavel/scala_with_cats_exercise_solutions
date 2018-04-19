package chapter11

import cats.Monoid
import cats.syntax.semigroup._ // for |+|
import cats.instances.map._ // for Monoid


final case class GenericGCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(implicit monoid: Monoid[A]): GenericGCounter[A] = {
    val current = counters.getOrElse(machine, monoid.empty)
    val incremented = current |+| amount
    GenericGCounter(counters + (machine -> incremented))
  }

  def merge(that: GenericGCounter[A])(implicit semi_lattice: BoundedSemiLattice[A]): GenericGCounter[A] = {
    GenericGCounter(counters |+| that.counters)
  }

  def total(implicit monoid: Monoid[A]): A =
    Monoid[A].combineAll(counters.values)
}
