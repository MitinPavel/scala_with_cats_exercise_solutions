package chapter9

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cats.Monoid
import cats.syntax.semigroup._

object MultithreadedMapReduce {
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val batches = values.grouped(batchCount).toList
    val futureList = batches.map { as =>
      Future(foldMap(as)(f(_)))
    }
    Future.sequence(futureList).map { bs =>
      bs.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }

  private def batchCount = Runtime.getRuntime.availableProcessors

  private def foldMap[A, B: Monoid](as: Vector[A])(f: A => B): B =
    SingleThreadedMapReduce.foldMap(as)(f(_))
}
