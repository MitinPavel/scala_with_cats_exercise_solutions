package chapter5

import cats.data.EitherT

import cats.instances.future._
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object MonadsTransformAndRollOut extends App {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(level) => EitherT.right(Future(level))
      case None => EitherT.left(Future(s"$autobot is unreachable"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      l1 <- getPowerLevel(ally1)
      l2 <- getPowerLevel(ally2)
    } yield (l1 + l2) > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    Await.result(canSpecialMove(ally1, ally2).value, 5.second) match {
      case Left(error) => s"Comms error: $error"
      case Right(true) => s"$ally1 and $ally1 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally1 need a recharge."
    }
  }

  println(Await.result(getPowerLevel("Bumblebee").value, 5.second))
  println(Await.result(getPowerLevel("Blah").value, 5.second))

  println(Await.result(canSpecialMove("Jazz", "Bumblebee").value, 5.second))
  println(Await.result(canSpecialMove("Jazz", "Hot Rod").value, 5.second))

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))
}
