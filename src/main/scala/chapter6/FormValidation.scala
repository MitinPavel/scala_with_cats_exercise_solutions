package chapter6

object FormValidation extends App {
  val data = Map("name" -> "John Doe", "age" -> "20")
  println(FailFastFormValidator.readName(data))
  println(FailFastFormValidator.readAge(data))
}
