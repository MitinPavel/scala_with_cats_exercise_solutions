package chapter4

import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.vector._

object FactorialCalculator {
  type Logged[A] = Writer[Vector[String], A]

  def factorial(n: Int): Logged[Int] =
    for {
      current <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $current").tell
    } yield current

  private def slowly[A](body: => A) =
    try body finally Thread.sleep(100)
}

object ShowYourWorking extends App {

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val Vector(result1, result2) = Await.result(Future.sequence(Vector(
    Future(FactorialCalculator.factorial(3)),
    Future(FactorialCalculator.factorial(3))
  )), 5.seconds)

  println(result1.written)
  println(result2.written)
}
