package chapter2

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class AddingAllTheThingsSpec extends FlatSpec with Checkers {

  it should "work" in {
    check((l: List[Int]) => {
      val a = new SuperAdder
      a.add(l) == l.sum
    })
  }
}
