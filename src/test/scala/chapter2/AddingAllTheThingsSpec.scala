package chapter2

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class AddingAllTheThingsSpec extends FlatSpec with Checkers {

  import cats.implicits._

  "SuperAdder" should "work with List[A]" in {
    check((l: List[Int]) => {
      val a = new SuperAdder
      a.add(l) == l.sum
    })
  }

  "SuperAdder" should "work with List[Option[A]]" in {
    check((l: List[Option[Int]]) => {
      val a = new SuperAdder
      a.add(l).getOrElse(0) == l.flatten.sum
    })
  }

  "SuperAdder" should "work with List[Order]" in {
    import chapter2.OrderMonoidInstances._

    val a = new SuperAdder

    assert(a.add(List(Order(1, 1))) == Order(1, 1))
    assert(a.add(List(Order(1, 10), Order(2, 20))) == Order(3, 30))
  }
}
