package chapter4

import cats.Eval

object Folder {
  def recursiveFoldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, recursiveFoldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    foldEvalRight(as, acc)(fn).value
  }

  private def foldEvalRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(foldEvalRight(tail, fn(head, acc))(fn))
      case Nil =>
        Eval.now(acc)
    }
}

object SaferFoldingUsingEval extends App {
  val folded = Folder.recursiveFoldRight((1 to 50000).toList, 0)(_ + _)
  println(folded)
}
