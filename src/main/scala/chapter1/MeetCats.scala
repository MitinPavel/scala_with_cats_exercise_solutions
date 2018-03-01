package chapter1

object MeetCats extends App {

  import cats._
  import cats.implicits._

  implicit val dateShow: Show[Cat] =
    Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat")

  val cat = Cat("Smokey", 2, "grey")
  println(cat.show)
}
