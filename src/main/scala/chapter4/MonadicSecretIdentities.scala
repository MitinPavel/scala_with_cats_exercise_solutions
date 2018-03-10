package chapter4

object MonadicSecretIdentities extends App {
  type Id[A] = A

  def Id[A](a: A): Id[A] = a

  def pure[A](a: A): Id[A] = Id[A](a)

  def map[A, B](a: Id[A])(func: A => B): Id[B] = func(a)

  def flatMap[A, B](a: Id[A])(func: A => Id[B]): Id[B] = func(a)

  println(pure(1))
  println(map(pure(1)) { a => (a + 1).toString })
  println(flatMap(pure(1)) { a => Id((a + 1).toString) })
}
