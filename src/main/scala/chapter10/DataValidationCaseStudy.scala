package chapter10

import cats.instances.list._// for Semigroup
import cats.syntax.validated._

object DataValidationCaseStudy extends App {
  val isEmptyCheck = Pure { str: String =>
    str match {
      case "" => List("Should not be empty").invalid[String]
      case _ => str.valid[List[String]]
    }
  }

  println(isEmptyCheck(""))
  println(isEmptyCheck("a"))
}
