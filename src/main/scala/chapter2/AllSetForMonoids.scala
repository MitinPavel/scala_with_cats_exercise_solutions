// 2.4 Exercise: All Set for Monoids

package chapter2

object SetMonoidInstances {
  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = a.union(b)

      def empty = Set.empty[A]
    }
}

object AllSetForMonoids extends App {

  import chapter2.MonoidLows._
  import SetMonoidInstances._

  println(implicitly[Monoid[Set[String]]])
  println(associativeLaw(Set("a", "b"), Set("c"), Set[String]()))
  println(identityLaw(Set("a", "b")))
  println(identityLaw(Set[String]()))
}
