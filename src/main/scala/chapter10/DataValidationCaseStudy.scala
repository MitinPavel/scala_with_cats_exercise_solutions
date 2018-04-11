package chapter10

import cats.instances.list._// for Semigroup
import cats.syntax.validated._

object DataValidationCaseStudy extends App {
  val isEmpty = Pure { str: String =>
    str match {
      case "" => List("Should not be empty").invalid[String]
      case _ => str.valid[List[String]]
    }
  }

  println(isEmpty(""))
  println(isEmpty("a"))
}
