// 2.3 Exercise: The Truth About Monoids

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

object MonoidInstances {
  implicit val booleanAndMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean): Boolean = x && y

      def empty = true
    }

  implicit val booleanOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean): Boolean = x || y

      def empty = false
    }

  implicit val booleanXorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)

      def empty = false
    }
}

object TheTruthAboutMonoids extends App {

  import MonoidLows._

  def checkLows(implicit m: Monoid[Boolean]) = {
    println(
      associativeLaw(true, true, true) &&
        associativeLaw(true, true, false) &&
        associativeLaw(true, false, true) &&
        associativeLaw(true, false, false) &&
        associativeLaw(false, true, true) &&
        associativeLaw(false, true, false) &&
        associativeLaw(false, false, true) &&
        associativeLaw(false, false, false) &&
        identityLaw(true) &&
        identityLaw(false)
    )
  }

  {
    checkLows(MonoidInstances.booleanAndMonoid)
  }
  {
    checkLows(MonoidInstances.booleanOrMonoid)
  }
  {
    checkLows(MonoidInstances.booleanXorMonoid)
  }
}
