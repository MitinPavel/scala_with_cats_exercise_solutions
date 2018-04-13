package chapter10

import cats.Semigroup
import cats.data.Validated

sealed trait Check[E, A, B] {
  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] =
    Map[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]) =
    FlatMap[E, A, B, C](this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
    AndThen[E, A, B, C](this, that)
}

object Check {
  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
    PureCheck(pred)
}

final case class AndThen[E, A, B, C](check: Check[E, A, B], that: Check[E, B, C]) extends Check[E, A, C] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check(a).withEither(_.flatMap(b => that(b).toEither))
}

final case class FlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check(a).withEither(_.flatMap(b => func(b)(a).toEither))
}

final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check(in).map(func)
}

final case class PureCheck[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] =
    pred(in)
}