package chapter9

import cats.instances.int._

object MapReduceCaseStudy extends App {
  println(MapReduce.foldMap(Vector("1", "2"))(_.toInt))

  import cats.instances.int._
  println(MapReduce.foldMap(Vector(1, 2, 3))(identity))

  import cats.instances.string._
  println(MapReduce.foldMap(Vector(1, 2, 3))(_.toString + "! "))
  println(MapReduce.foldMap("Hello world!".toVector)(_.toString.toUpperCase))
}
