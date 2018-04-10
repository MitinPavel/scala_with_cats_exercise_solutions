package chapter10

import cats.data.Validated
import cats.Semigroup
import cats.syntax.apply._ // for tupled

case class Check[E, A](f: A => Validated[E, A]) {
  def apply(a: A): Validated[E, A] = f(a)

  def and(that: Check[E, A])(implicit semigroup: Semigroup[E]): Check[E, A] =
    Check { a: A =>
      (this(a), that(a)).tupled.map(_._1)
    }
}
