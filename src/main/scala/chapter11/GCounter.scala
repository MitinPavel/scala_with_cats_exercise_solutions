package chapter11

import scala.language.higherKinds
import cats.Monoid
import cats.syntax.semigroup._ // for |+|
import cats.instances.map._ // for Monoid

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: Monoid[V]): V
}

object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) =
    counter
}

object GCounterInstances {
  implicit def mapGCounter[K, V]: GCounter[Map, K, V] =
    new GCounter[Map, K, V] {
      def increment(f: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] = {
        val current = f.getOrElse(k, m.empty)
        val incremented = m.combine(current, v)
        f + (k -> incremented)
      }

      def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
        f1 |+| f2

      def total(f: Map[K, V])(implicit m: Monoid[V]): V =
        Monoid[V].combineAll(f.values)
    }
}
