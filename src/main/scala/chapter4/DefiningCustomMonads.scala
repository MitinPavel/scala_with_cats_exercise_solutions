package chapter4

import cats.{Monad ⇒ CatsMonad}
import Tree._
import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap

object DefiningCustomMonads extends App {

  implicit val treeMonad = new CatsMonad[Tree] {
    def flatMap[A, B](tree: Tree[A])
                     (fn: A => Tree[B]): Tree[B] =
      tree match {
        case Branch(l, r) => Branch(flatMap(l)(fn), flatMap(r)(fn))
        case Leaf(v) => fn(v)
      }

    def pure[A](opt: A): Tree[A] =
      Leaf(opt)

    def tailRecM[A, B](a: A)
                      (fn: A => Tree[Either[A, B]]): Tree[B] =
      fn(a) match {
        case Leaf(Right(v)) => Leaf(v)
        case Leaf(Left(v)) => tailRecM(v)(fn)
        case Branch(l, r) =>
          Branch(
            flatMap(l) {
              case Left(l) => tailRecM(l)(fn)
              case Right(l) => pure(l)
            },
            flatMap(r) {
              case Left(r) => tailRecM(r)(fn)
              case Right(r) => pure(r)
            }
          )
      }
  }


  branch(leaf(100), leaf(200)).
    flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

  for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c
}
