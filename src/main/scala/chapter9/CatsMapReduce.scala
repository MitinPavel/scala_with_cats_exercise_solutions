package chapter9

import cats.Monoid
import cats.instances.future._ // for Applicative and Monad
import cats.instances.vector._ // for Foldable and Traverse
import cats.syntax.foldable._ // for combineAll and foldMap
import cats.syntax.traverse._ // for traverse
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object CatsMapReduce {
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    values
      .grouped(batchCount(values.size))
      .toVector
      .traverse(group => Future(group.toVector.foldMap(func)))
      .map(_.combineAll)
  }

  private def batchCount[A](totalCount: Int) =
    (1.0 * totalCount / Runtime.getRuntime.availableProcessors).ceil.toInt
}
