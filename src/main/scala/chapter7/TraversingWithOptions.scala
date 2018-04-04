package chapter7

object TraversingWithOptions extends App {

  import scala.language.higherKinds

  import cats.Applicative
  import cats.syntax.applicative._
  import cats.syntax.apply._

  import cats.instances.option._

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  println(process(List(2, 4, 6)))
  println(process(List(1, 2, 3)))
}
