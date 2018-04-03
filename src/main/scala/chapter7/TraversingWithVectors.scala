package chapter7

object TraversingWithVectors extends App {

  import scala.language.higherKinds

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global


  import cats.Applicative
  import cats.instances.future._ // for Applicative
  import cats.syntax.applicative._ // for pure
  import cats.syntax.apply._ // for mapN

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60) // just for demonstration

  val totalUptime = listTraverse(hostnames)(getUptime)
  println(Await.result(totalUptime, 1.second))

  val seq = listSequence(List(getUptime("alpha.example.com"), getUptime("beta.example.com")))
  println(Await.result(seq, 1.second))

  import cats.instances.vector._ // for Applicative
  println(listSequence(List(Vector(1, 2), Vector(3, 4))))
  println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))
}
