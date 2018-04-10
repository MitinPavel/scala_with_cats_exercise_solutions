package chapter10

import org.scalatest.FlatSpec
import cats.instances.list._
import cats.syntax.validated._

class CheckSpec extends FlatSpec {
  val isEmptyCheck = Pure { str: String =>
    str match {
      case "" => str.valid[List[String]]
      case _ => List("be empty").invalid[String]
    }
  }

  val isPresentCheck = Pure { str: String =>
    str match {
      case "" => List("not be empty").invalid[String]
      case _ => str.valid[List[String]]
    }
  }

  val isOneSymbolCheck = Pure { str: String =>
    if (str.length != 1) List("consist of one symbol").invalid[String]
    else str.valid[List[String]]
  }

  "and" should "be conjunctive" in {
    assert(And(isEmptyCheck, isOneSymbolCheck)("abc") == List("be empty", "consist of one symbol").invalid[String])
    assert(And(isPresentCheck, isOneSymbolCheck)("abc") == List("consist of one symbol").invalid[String])
    assert(And(isOneSymbolCheck, isPresentCheck)("abc") == List("consist of one symbol").invalid[String])
    assert(And(isOneSymbolCheck, isPresentCheck)("a") == "a".valid[List[String]])
  }

  "or" should "be disjunctive" in {
    assert(Or(isEmptyCheck, isOneSymbolCheck)("abc") == List("be empty", "consist of one symbol").invalid[String])
    assert(Or(isPresentCheck, isOneSymbolCheck)("abc") == "abc".valid[List[String]])
    assert(Or(isOneSymbolCheck, isPresentCheck)("abc") == "abc".valid[List[String]])
    assert(Or(isOneSymbolCheck, isPresentCheck)("abc") == "abc".valid[List[String]])
  }
}
