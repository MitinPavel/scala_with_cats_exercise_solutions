package chapter11

import cats.Monoid
import cats.syntax.semigroup._ // for |+|
import cats.instances.map._ // for Monoid


final case class GCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(implicit monoid: Monoid[A]): GCounter[A] = {
    val current = counters.getOrElse(machine, monoid.empty)
    val incremented = current |+| amount
    GCounter(counters + (machine -> incremented))
  }

  def merge(that: GCounter[A])(implicit semi_lattice: BoundedSemiLattice[A]): GCounter[A] = {
    GCounter(counters |+| that.counters)
  }

  def total(implicit monoid: Monoid[A]): A =
    Monoid[A].combineAll(counters.values)
}
