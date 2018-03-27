package chapter6

import cats.syntax.either._

case class User(name: String, age: Int)

object FormValidation extends App {

  type FailEither[A] = Either[List[String], A]

  def getValue(fieldName: String)(data: Map[String, String]): FailEither[String] =
    data.get(fieldName) match {
      case Some(value) => Right(value)
      case None => Left(List(s"Unexpected field $fieldName"))
    }

  def parseInt(string: String): FailEither[Int] =
    scala.util.Try(string.toInt).
      toEither.
      leftMap(_ => List(s"Cannot parse $string"))
}
