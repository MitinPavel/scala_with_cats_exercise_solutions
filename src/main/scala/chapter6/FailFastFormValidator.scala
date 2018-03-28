package chapter6

import cats.syntax.either._
import cats.data.Validated

object FailFastFormValidator {
  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def readName(data: FormData): FailFast[String] =
    for {
      value <- getValue("name")(data)
      result <- nonBlank("name")(value)
    } yield result

  def readAge(data: FormData): FailFast[Int] =
    for {
      value <- getValue("age")(data)
      int <- parseInt("age")(value)
      result <- nonNegative("age")(int)
    } yield result

  private def getValue(name: String)(data: FormData): FailFast[String] =
    data.get(name).
      toRight(List(s"$name field not specified"))

  private def parseInt(name: String)(string: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](string.toInt).
      leftMap(_ => List(s"$name must be an integer"))

  private def nonBlank(name: String)(string: String): FailFast[String] =
    Right(string).
      ensure(List(s"$name cannot be blank"))(_.trim.nonEmpty)

  private def nonNegative(name: String)(int: Int): FailFast[Int] =
    Right(int).
      ensure(List(s"$name must be non-negative"))(_ >= 0)
}
