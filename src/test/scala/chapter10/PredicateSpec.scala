package chapter10

import cats.instances.list._
import cats.syntax.validated._
import org.scalatest.FlatSpec

class PredicateSpec extends FlatSpec {
  val isEmpty = Pure { str: String =>
    str match {
      case "" => str.valid[List[String]]
      case _ => List("be empty").invalid[String]
    }
  }

  val isPresent = Pure { str: String =>
    str match {
      case "" => List("not be empty").invalid[String]
      case _ => str.valid[List[String]]
    }
  }

  val isOneSymbol = Pure { str: String =>
    if (str.length != 1) List("consist of one symbol").invalid[String]
    else str.valid[List[String]]
  }

  "and" should "be conjunctive" in {
    assert(And(isEmpty, isOneSymbol)("abc") == List("be empty", "consist of one symbol").invalid[String])
    assert(And(isPresent, isOneSymbol)("abc") == List("consist of one symbol").invalid[String])
    assert(And(isOneSymbol, isPresent)("abc") == List("consist of one symbol").invalid[String])
    assert(And(isOneSymbol, isPresent)("a") == "a".valid[List[String]])
  }

  "or" should "be disjunctive" in {
    assert(Or(isEmpty, isOneSymbol)("abc") == List("be empty", "consist of one symbol").invalid[String])
    assert(Or(isPresent, isOneSymbol)("abc") == "abc".valid[List[String]])
    assert(Or(isOneSymbol, isPresent)("abc") == "abc".valid[List[String]])
    assert(Or(isOneSymbol, isPresent)("abc") == "abc".valid[List[String]])
  }
}
