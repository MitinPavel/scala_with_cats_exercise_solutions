package chapter9

import cats.Foldable

import cats.Traverse
import cats.instances.future._ // for Applicative
import cats.instances.list._ // for Traverse
import cats.Monoid
import cats.syntax.foldable._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object CatsMapReduce {
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val batches: List[List[A]] = values.grouped(batchCount(values.size)).toList.map(_.toList)

    val fut: Future[List[B]] = Traverse[List].traverse(batches) { as: List[A] =>
      Future { Foldable[List].foldMap(as)(f) }
    }

    fut.map { bs: List[B] =>
      bs.combineAll
    }
  }

  private def batchCount[A](totalCount: Int) =
    (1.0 * totalCount / Runtime.getRuntime.availableProcessors).ceil.toInt

}
