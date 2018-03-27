package chapter6

object FormValidation extends App {
  println(FormValidator.getValue("name")(Map("name" -> "Dade Murphy")))
}
