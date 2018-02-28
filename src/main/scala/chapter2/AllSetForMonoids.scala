// 2.4 Exercise: All Set for Monoids

package chapter2

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid
}

object MonoidLows {
  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) ==
      m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
  }
}

object AllSetForMonoids extends App {

  import MonoidLows._

  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = a.union(b)

      def empty = Set.empty[A]
    }

  println(implicitly[Monoid[Set[String]]])
  println(associativeLaw(Set("a", "b"), Set("c"), Set[String]()))
  println(identityLaw(Set("a", "b")))
  println(identityLaw(Set[String]()))
}
