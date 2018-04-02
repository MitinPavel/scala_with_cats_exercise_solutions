package chapter7

object ListMethods {
  def map[A, B](list: List[A])(f: A => B): List[B] =
    list.foldRight(List.empty[B])((i, acc) => f(i) :: acc)

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
    list.foldRight(List.empty[B])((i, acc) => f(i) ::: acc)

  def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
    list.foldRight(List.empty[A]) { (i, acc) =>
      if (predicate(i)) i :: acc
      else acc
    }

  def sum[A](list: List[A])(implicit num: Numeric[A]): A =
    list.foldRight(num.zero)((i, acc) => num.plus(i, acc))
}
