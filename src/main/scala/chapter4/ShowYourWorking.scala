package chapter4

import cats.data.Writer
import cats.syntax.writer._

object FactorialCalculator {
  type IntLogger = Writer[Vector[String], Int]

  def factorial(n: Int): IntLogger = slowly(calculate(n))

  private def calculate(n: Int): IntLogger = {
    if (n == 0) 1.writer(Vector(s"fact 0 1"))
    else {
      val current = factorial(n - 1)
      val value = n * current.value

      current.bimap(
        log => log :+ s"fact $n $value",
        _ => value
      )
    }
  }

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
