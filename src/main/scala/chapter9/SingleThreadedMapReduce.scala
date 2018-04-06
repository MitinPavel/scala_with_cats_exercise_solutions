package chapter9

import cats.Monoid
import cats.syntax.semigroup._

object SingleThreadedMapReduce {
  def foldMap[A, B: Monoid](as: Vector[A])(f: A => B): B =
    as.foldLeft(Monoid[B].empty)(_ |+| f(_))
}
