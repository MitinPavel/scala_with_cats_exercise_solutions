package chapter9

import cats.Monoid

object MapReduce {
  def foldMap[A, B: Monoid](sequence: Vector[A])(f: A => B): B =
    sequence.map(f).foldLeft(Monoid[B].empty) { (acc, b) =>
      Monoid[B].combine(acc, b)
    }
}
