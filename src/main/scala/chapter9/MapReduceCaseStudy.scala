package chapter9

import cats.instances.int._

object MapReduceCaseStudy extends App {
  println(SingleThreadedMapReduce.foldMap(Vector("1", "2"))(_.toInt))

  import cats.instances.int._
  println(SingleThreadedMapReduce.foldMap(Vector(1, 2, 3))(identity))

  import cats.instances.string._
  println(SingleThreadedMapReduce.foldMap(Vector(1, 2, 3))(_.toString + "! "))
  println(SingleThreadedMapReduce.foldMap("Hello world!".toVector)(_.toString.toUpperCase))

  import scala.concurrent._
  import scala.concurrent.duration._

  println(Await.result(MultithreadedMapReduce.parallelFoldMap((1 to 100).toVector)(_.toString), 1.second))
}
