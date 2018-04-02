package chapter7

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class ScaffoldingOtherMethodsSpec extends FlatSpec with Checkers {

  "ListMethods" should "implement map" in {
    check((l: List[Int]) => {
      val f = (i: Int) => i + 1
      ListMethods.map(l)(f) == l.map(f)
    })
  }

  "ListMethods" should "implement flatMap" in {
    check((l: List[Int]) => {
      val f = (i: Int) => List(i, i * 10)
      ListMethods.flatMap(l)(f) == l.flatMap(f)
    })
  }

  "ListMethods" should "implement filter" in {
    check((l: List[Int]) => {
      val f = (i: Int) => i % 2 == 0
      ListMethods.filter(l)(f) == l.filter(f)
    })
  }

  "ListMethods" should "implement sum" in {
    check((l: List[Int]) => ListMethods.sum(l) == l.sum)
  }
}
