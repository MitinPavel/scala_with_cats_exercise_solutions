package chapter6

import org.scalatest.FlatSpec

class FormValidationSpec extends FlatSpec {
  val formData = Map("name" -> "Dade Murphy")

  "FormValidator" should "retrieve value" in {
    assert(Right("Dade Murphy") == FormValidator.getValue("name")(formData))
  }

  "FormValidator" should "return a error" in {
    assert(Left(List("unexpected field not specified")) == FormValidator.getValue("unexpected")(formData))
  }
}
