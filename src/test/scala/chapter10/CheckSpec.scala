package chapter10

import org.scalatest.FlatSpec
import scala.util.Try
import cats.syntax.validated._

import cats.instances.list._

class CheckSpec extends FlatSpec {
  val isInt = Pure { str: String =>
    Try(str.toInt).toOption match {
      case Some(_) => str.valid[List[String]]
      case None => List("not an integer").invalid[String]
    }
  }

  val intStringCheck = Check(isInt)
  val parseInt = intStringCheck.map(_.toInt)

  "pure" should "wrap Predicate" in {
    assert(intStringCheck("123") == "123".valid[List[String]])
    assert(intStringCheck("1.1") == List("not an integer").invalid[String])
    assert(intStringCheck("blah") == List("not an integer").invalid[String])
  }

  "map" should "transform wrapped data" in {
    assert(parseInt("123" ) == 123.valid[List[String]])
    assert(parseInt("1.1" ) == List("not an integer").invalid[Int])
    assert(parseInt("blah") == List("not an integer").invalid[Int])
  }
}
