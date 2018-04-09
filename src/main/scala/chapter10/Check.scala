package chapter10

import cats.Semigroup
import cats.syntax.either._ // for asLeft and asRight

case class Check[E, A](f: A => Either[E, A]) {
  def apply(a: A): Either[E, A] = f(a)

  def and(that: Check[E, A])(implicit semigroup: Semigroup[E]): Check[E, A] =
    Check { a: A => {
      (this(a), that(a)) match {
        case (Right(_), Right(_)) => a.asRight
        case (Left(e1), Right(_)) => e1.asLeft
        case (Right(_), Left(e2)) => e2.asLeft
        case (Left(e1), Left(e2)) => semigroup.combine(e1, e2).asLeft
      }
    }
    }
}
