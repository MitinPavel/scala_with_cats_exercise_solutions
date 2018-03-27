package chapter6

import cats.syntax.either._
import cats.data.Validated

case class User(name: String, age: Int)

object FormValidation extends App {

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(name: String)(data: FormData): FailFast[String] =
    data.get(name).
      toRight(List(s"$name field not specified"))

  def parseInt(name: String)(data: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](data.toInt).
      leftMap(_ => List(s"$name must be an integer"))
}
