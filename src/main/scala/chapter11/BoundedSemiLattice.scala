package chapter11

import cats.Monoid

trait BoundedSemiLattice[A] extends Monoid[A] {
  def combine(a1: A, a2: A): A

  def empty: A
}

object BoundedSemiLatticeInstances {
  implicit def intBoundedSemiLattice: BoundedSemiLattice[Int] =
    new BoundedSemiLattice[Int] {
      def combine(a: Int, b: Int): Int = a.max(b)

      def empty: Int = 0
    }

  implicit def setBoundedSemiLattice[A]: BoundedSemiLattice[Set[A]] =
    new BoundedSemiLattice[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] = a.union(b)

      def empty: Set[A] = Set.empty[A]
    }
}
