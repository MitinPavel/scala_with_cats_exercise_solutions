package chapter10

import cats.Semigroup

case class Check[E, A](f: A => Either[E, A])(implicit semigroup: Semigroup[E]) {
  def apply(a: A): Either[E, A] = f(a)

  def and(that: Check[E, A]): Check[E, A] =
    Check { a: A =>
      for {
        _ <- this.apply(a)
        _ <- that.apply(a)
      } yield a
    }
}
