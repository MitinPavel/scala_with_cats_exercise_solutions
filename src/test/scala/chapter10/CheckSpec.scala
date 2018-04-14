package chapter10

import org.scalatest.FlatSpec
import scala.util.Try
import cats.syntax.validated._

import cats.instances.list._

class CheckSpec extends FlatSpec {
  val isInt = Predicate { str: String =>
    Try(str.toInt).toOption match {
      case Some(_) => str.valid[List[String]]
      case None => List("not an integer").invalid[String]
    }
  }
  val isOneSymbol = Predicate { str: String =>
    if (str.length == 1) str.valid[List[String]]
    else List("has invalid length").invalid[String]
  }

  "pure" should "wrap Predicate" in {
    val intStringCheck = Check(isInt)

    assert(intStringCheck("123") == "123".valid[List[String]])
    assert(intStringCheck("1.1") == List("not an integer").invalid[String])
    assert(intStringCheck("blah") == List("not an integer").invalid[String])
  }

  "map" should "transform wrapped data" in {
    val intStringCheck = Check(isInt)
    val parseInt = intStringCheck.map(_.toInt)

    assert(parseInt("123") == 123.valid[List[String]])
    assert(parseInt("1.1") == List("not an integer").invalid[Int])
    assert(parseInt("blah") == List("not an integer").invalid[Int])
  }

  "flatMap" should "have meaningful semantic ;)" in {
    val intStringCheck = Check(isInt)
    val oneSymbolIntCheck = intStringCheck.flatMap { a: String =>
      Check(isOneSymbol)
    }

    assert(oneSymbolIntCheck("1") == "1".valid[List[String]])
    assert(oneSymbolIntCheck("11") == List("has invalid length").invalid[String])
    assert(oneSymbolIntCheck("a") == List("not an integer").invalid[String])
    assert(oneSymbolIntCheck("aa") == List("not an integer").invalid[String])
  }

  "andThen" should "chane checks" in {
    val intStringCheck = Check(isInt)
    val oneSymbolCheck = Check(isOneSymbol)
    val oneSymbolIntCheck = intStringCheck.andThen(oneSymbolCheck)

    assert(oneSymbolIntCheck("1") == "1".valid[List[String]])
    assert(oneSymbolIntCheck("11") == List("has invalid length").invalid[String])
    assert(oneSymbolIntCheck("a") == List("not an integer").invalid[String])
    assert(oneSymbolIntCheck("aa") == List("not an integer").invalid[String])
  }
}
