// 2.3 Exercise: The Truth About Monoids

package chapter2

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

  import chapter2.MonoidLows._

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
