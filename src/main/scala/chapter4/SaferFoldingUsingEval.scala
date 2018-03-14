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

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldEvalRight(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

  private def foldEvalRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldEvalRight(tail, acc)(fn)))
      case Nil =>
        acc
    }
}

object SaferFoldingUsingEval extends App {
  val folded = Folder.foldRight((1 to 50000).toList, 0)(_ + _)
  println(folded)
}
