package chapter10

import cats.instances.list._
import cats.syntax.validated._
import org.scalatest.FlatSpec

class PredicateSpec extends FlatSpec {
  val isEmpty = Predicate { str: String =>
    str match {
      case "" => str.valid[List[String]]
      case _ => List("be empty").invalid[String]
    }
  }

  val isPresent = Predicate { str: String =>
    str match {
      case "" => List("not be empty").invalid[String]
      case _ => str.valid[List[String]]
    }
  }

  val isOneSymbol = Predicate { str: String =>
    if (str.length != 1) List("consist of one symbol").invalid[String]
    else str.valid[List[String]]
  }

  "and" should "be conjunctive" in {
    assert(isEmpty.and(isOneSymbol)("abc") == List("be empty", "consist of one symbol").invalid[String])
    assert(isPresent.and(isOneSymbol)("abc") == List("consist of one symbol").invalid[String])
    assert(isOneSymbol.and(isPresent)("abc") == List("consist of one symbol").invalid[String])
    assert(isOneSymbol.and(isPresent)("a") == "a".valid[List[String]])
  }

  "or" should "be disjunctive" in {
    assert(isEmpty.or(isOneSymbol)("abc") == List("be empty", "consist of one symbol").invalid[String])
    assert(isPresent.or(isOneSymbol)("abc") == "abc".valid[List[String]])
    assert(isOneSymbol.or(isPresent)("abc") == "abc".valid[List[String]])
    assert(isOneSymbol.or(isPresent)("abc") == "abc".valid[List[String]])
  }
}
