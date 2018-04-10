package chapter10

import cats.instances.list._// for Semigroup
import cats.syntax.validated._

object DataValidationCaseStudy extends App {
  val isEmptyCheck = Check { str: String =>
    str match {
      case "" => List("Should not be empty").invalid[String]
      case _ => str.valid[List[String]]
    }
  }

  val lengthCheck = Check { str: String =>
    if (str.length < 3) List("Should be longer than two").invalid[String]
    else str.valid[List[String]]
  }

  println(isEmptyCheck(""))
  println(isEmptyCheck("a"))

  println(lengthCheck("ab"))
  println(lengthCheck("abc"))

  println(isEmptyCheck.and(lengthCheck).apply(""))
  println(isEmptyCheck.and(lengthCheck).apply("a"))
  println(isEmptyCheck.and(lengthCheck).apply("abcde"))
}
