package chapter6

object FormValidation extends App {
  val data = Map("name" -> "John Doe", "age" -> "20")
  println(FormValidator.readUser(data))
}
