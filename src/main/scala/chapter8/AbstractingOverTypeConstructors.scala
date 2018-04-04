package chapter8

import scala.language.higherKinds
import scala.concurrent.Future
import cats.Id
import cats.instances.list._
import cats.syntax.traverse._

class UptimeService(client: UptimeClient[Id]) {
  def getTotalUptime(hostnames: List[String]): Id[Int] =
    hostnames.traverse(client.getUptime).sum
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}

trait TestUptimeClient extends UptimeClient[Id] {
  def getUptime(hostname: String): Int
}

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class RealUptimeClientImpl(hosts: Map[String, Int]) extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int] =
    Future.successful(hosts.getOrElse(hostname, 0))
}

class TestUptimeClientImpl(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Int =
    hosts.getOrElse(hostname, 0)
}

object AbstractingOverTypeConstructors extends App {
   def testTotalUptime() = {
     val hosts = Map("host1" -> 10, "host2" -> 6)
     val client = new TestUptimeClientImpl(hosts)
     val service = new UptimeService(client)
     val actual = service.getTotalUptime(hosts.keys.toList)
     val expected = hosts.values.sum
     assert(actual == expected)
   }

  testTotalUptime()
}
