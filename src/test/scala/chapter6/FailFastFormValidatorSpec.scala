package chapter6

import org.scalatest.FlatSpec

class FailFastFormValidatorSpec extends FlatSpec {
  "readName" should "retrieve data by the passed key" in {
    val actual = FailFastFormValidator.readName(Map("name" -> "Dade Murphy"))
    assert(Right("Dade Murphy") == actual)
  }

  "readName" should "check if data contains the passed key" in {
    val actual = FailFastFormValidator.readName(Map("age" -> "20"))
    assert(Left(List("name field not specified")) == actual)
  }

  "readName" should "check if found value blank" in {
    val actual = FailFastFormValidator.readName(Map("name" -> ""))
    assert(Left(List("name cannot be blank")) == actual)
  }

  "readAge" should "retrieve data by the passed key" in {
    val actual = FailFastFormValidator.readAge(Map("age" -> "20"))
    assert(Right(20) == actual)
  }

  "readAge" should "check if data contains the passed key" in {
    val actual = FailFastFormValidator.readAge(Map("name" -> "Dade Murphy"))
    assert(Left(List("age field not specified")) == actual)
  }

  "readAge" should "check if found value non-negative" in {
    assert(Right(1) == FailFastFormValidator.readAge(Map("age" -> "1")))
    assert(Right(0) == FailFastFormValidator.readAge(Map("age" -> "0")))
    assert(Left(List("age must be non-negative")) == FailFastFormValidator.readAge(Map("age" -> "-1")))
  }
}
