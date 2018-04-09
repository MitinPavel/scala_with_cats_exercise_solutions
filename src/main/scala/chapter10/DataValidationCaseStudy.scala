package chapter10

import cats.instances.list._// for Semigroup

object DataValidationCaseStudy extends App {
  val isEmptyCheck = Check { str: String =>
    str match {
      case "" => Left(List("Should not be empty"))
      case _ => Right(str)
    }
  }

  val lengthCheck = Check { str: String =>
    if (str.length < 3) Left(List("Should be longer than two"))
    else Right(str)
  }

  println(isEmptyCheck(""))
  println(isEmptyCheck("a"))

  println(lengthCheck("ab"))
  println(lengthCheck("abc"))

  println(isEmptyCheck.and(lengthCheck).apply(""))
  println(isEmptyCheck.and(lengthCheck).apply("a"))
  println(isEmptyCheck.and(lengthCheck).apply("abcde"))
}
