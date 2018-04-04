package chapter7

object TraversingWithValidated extends App {
  import scala.language.higherKinds

  import cats.Applicative
  import cats.syntax.applicative._
  import cats.syntax.apply._

  import cats.data.Validated
  import cats.instances.list._ // for Monoid

  type ErrorsOr[A] = Validated[List[String], A]

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def process(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  println(process(List(2, 4, 6)))
  println(process(List(1, 2, 3)))
}
