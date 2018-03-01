package chapter2

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class TheTruthAboutMonoidsSpec extends FlatSpec with Checkers {
  import MonoidLows._

  "booleanAndMonoid" should "be  associative" in {
    import MonoidInstances.booleanAndMonoid
    check((a: Boolean, b: Boolean, c: Boolean) => associativeLaw(a, b, c))
  }

  "booleanAndMonoid" should "provide an identity element" in {
    import MonoidInstances.booleanAndMonoid
    check((a: Boolean) => identityLaw(a))
  }

  "booleanOrMonoid" should "be  associative" in {
    import MonoidInstances.booleanOrMonoid
    check((a: Boolean, b: Boolean, c: Boolean) => associativeLaw(a, b, c))
  }

  "booleanOrMonoid" should "provide an identity element" in {
    import MonoidInstances.booleanOrMonoid
    check((a: Boolean) => identityLaw(a))
  }

  "booleanXorMonoid" should "be  associative" in {
    import MonoidInstances.booleanXorMonoid
    check((a: Boolean, b: Boolean, c: Boolean) => associativeLaw(a, b, c))
  }

  "booleanXorMonoid" should "provide an identity element" in {
    import MonoidInstances.booleanXorMonoid
    check((a: Boolean) => identityLaw(a))
  }
}
