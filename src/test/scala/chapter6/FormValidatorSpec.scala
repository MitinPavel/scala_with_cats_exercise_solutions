package chapter6

import org.scalatest.FlatSpec

class FormValidatorSpec extends FlatSpec {
  "createUser" should "return User if valid" in {
    val actual = FormValidator.createUser(Map("name" -> "John Doe", "age" -> "20"))

    assert(actual.isValid)
    assert(actual.exists(_ == User("John Doe", 20)))
  }

  "createUser" should "accumulate all errors if invalid" in {
    val actual = FormValidator.createUser(Map("name" -> "", "age" -> "-1"))

    assert(actual.isInvalid)
    assert(actual.toEither.left.get == List("name cannot be blank", "age must be non-negative"))
  }
}
