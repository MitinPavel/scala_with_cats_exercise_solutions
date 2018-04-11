package chapter10

import org.scalatest.FlatSpec
import cats.syntax.validated._

class CheckSpec extends FlatSpec {
  val parseInt: Check[List[String], String, Int] = ???

  it should "parse string representation of an integer" in {
    assert(parseInt("123") == 123.valid[List[String]])
  }
}
