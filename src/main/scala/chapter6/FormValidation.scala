package chapter6

object FormValidation extends App {
  println(FailFastFormValidator.getValue("name")(Map("name" -> "Dade Murphy")))
}
