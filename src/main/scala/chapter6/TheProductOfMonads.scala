package chapter6

import scala.language.higherKinds
import cats.Monad
import cats.implicits._

object TheProductOfMonads extends App {
  def product[M[_] : Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    x.flatMap(a => y.map(b => (a, b)))

  println(product(Option(1), Option(2)))
}
