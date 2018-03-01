package chapter2

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class AllSetForMonoidsSpec extends FlatSpec with Checkers {

  import MonoidLows._

  "setUnionMonoid" should "be  associative" in {
    import SetMonoidInstances._
    check((a: Set[String], b: Set[String], c: Set[String]) => associativeLaw(a, b, c))
    check((a: Set[Int], b: Set[Int], c: Set[Int]) => associativeLaw(a, b, c))
  }

  "setUnionMonoid" should "provide an identity element" in {
    import SetMonoidInstances._
    check((a: Set[String]) => identityLaw(a))
    check((a: Set[Int]) => identityLaw(a))
  }
}
