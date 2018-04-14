package chapter10

import cats.data.{NonEmptyList, Validated}


object DataValidationCaseStudy extends App {
  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)

  // A username must contain at least four characters and
  // consist entirely of alphanumeric characters
  val usernamePredicate = alphanumeric.and(longerThan(3))
  println(usernamePredicate("abc"))
  println(usernamePredicate("abcd_"))
  val usernameCheck = Check(usernamePredicate)
  println(usernamePredicate("abc"))
  println(usernamePredicate("abcd_"))
}
