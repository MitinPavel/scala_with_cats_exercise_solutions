package chapter6

import org.scalatest.FlatSpec

class FormValidationSpec extends FlatSpec {
  val formData = Map("name" -> "Dade Murphy")

  "FailFastFormValidator" should "retrieve value" in {
    assert(Right("Dade Murphy") == FailFastFormValidator.getValue("name")(formData))
  }

  "FailFastFormValidator" should "return a error" in {
    assert(Left(List("unexpected field not specified")) == FailFastFormValidator.getValue("unexpected")(formData))
  }
}
