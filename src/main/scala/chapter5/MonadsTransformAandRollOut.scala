package chapter5

import cats.data.EitherT

import cats.instances.future._
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object MonadsTransformAandRollOut extends App {
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
    getPowerLevel(ally1).flatMap { l1 =>
      getPowerLevel(ally2).map(l2 => l1 + l2 > 15)
    }

  println(Await.result(getPowerLevel("Bumblebee").value, 5.second))
  println(Await.result(getPowerLevel("Blah").value, 5.second))

  println(Await.result(canSpecialMove("Jazz", "Bumblebee").value, 5.second))
  println(Await.result(canSpecialMove("Jazz", "Hot Rod").value, 5.second))
}
