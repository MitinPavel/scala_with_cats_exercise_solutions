package chapter11

import cats.Monoid

final case class GCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(implicit monoid: Monoid[A]): GCounter[A] = {
    val current = counters.getOrElse(machine, monoid.empty)
    val incremented = monoid.combine(current, amount)
    GCounter(counters + (machine -> incremented))
  }

  def merge(that: GCounter[A])(implicit semi_lattice: BoundedSemiLattice[A]): GCounter[A] = {
    val machines = counters.keys ++ that.counters.keys
    val merged = machines.foldLeft(Map[String, A]()) { (acc, machine) =>
      val thisCount = counters.getOrElse(machine, semi_lattice.empty)
      val thatCount = that.counters.getOrElse(machine, semi_lattice.empty)
      val count = semi_lattice.combine(thisCount, thatCount)

      acc + (machine -> count)
    }
    GCounter(merged)
  }

  def total(implicit monoid: Monoid[A]): A =
    Monoid[A].combineAll(counters.values)
}
