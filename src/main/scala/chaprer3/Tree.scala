package chaprer3

import cats._

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object TreeFunctorInstance {
  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      def map[A, B](tree: Tree[A])(func: A => B): Tree[B] =
        tree match {
          case Leaf(v) => Leaf(func(v))
          case Branch(l, r) => {
            Branch[B](
              map(l)(func),
              map(r)(func)
            )
          }
        }
    }
}
