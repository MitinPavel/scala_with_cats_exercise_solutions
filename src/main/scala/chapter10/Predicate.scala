package chapter10

import cats.Semigroup
import cats.data.Validated
import cats.syntax.apply._ // for mapN
import cats.data.Validated._ // for Valid and Invalid

sealed trait Predicate[E, A] {
  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(func) =>
        func(a)
      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) =>
        left(a).findValid(right(a))
    }
}

final case class And[E, A](left: Predicate[E, A],
                           right: Predicate[E, A]) extends Predicate[E, A]

final case class Or[E, A](left: Predicate[E, A],
                          right: Predicate[E, A]) extends Predicate[E, A]

final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]
