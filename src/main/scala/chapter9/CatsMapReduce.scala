package chapter9

import cats.Monoid
import cats.syntax.semigroup._
import cats.Foldable
import cats.instances.vector._ // for Applicative

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object CatsMapReduce {
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val batches = values.grouped(batchCount(values.size)).toList

    val futureList: List[Future[B]] = batches.map { as =>
      Future(foldMap(as)(f(_)))
    }

    Future.sequence(futureList).map { bs =>
      bs.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }

  private def batchCount[A](totalCount: Int) =
    (1.0 * totalCount / Runtime.getRuntime.availableProcessors).ceil.toInt

  private def foldMap[A, B: Monoid](as: Vector[A])(f: A => B): B =
    Foldable[Vector].foldMap(as)(f(_))
}
