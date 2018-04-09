package chapter10

import cats.Semigroup

case class Check[E, A](f: A => Either[E, A])(implicit semigroup: Semigroup[E]) {
  def apply(a: A): Either[E, A] = f(a)

  def and(that: Check[E, A]): Check[E, A] =
    Check { a: A => {
      val thisResult = this.apply(a)
      val thatResult = that.apply(a)

      (thisResult, thatResult) match {
        case (Right(_), Right(_)) => Right(a)
        case (Right(_), Left(thatLeft)) => Left(thatLeft)
        case (Left(thisLeft), Right(_)) => Left(thisLeft)
        case (Left(thisLeft), Left(thatLeft)) => Left(semigroup.combine(thisLeft, thatLeft))
      }
    }
    }
}
