package chapter11

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class GCounterSpec extends FlatSpec with Checkers {
  implicit def arbitrary(implicit a: Arbitrary[List[Int]]) =
    Arbitrary(Gen.containerOf[List, Int](Gen.chooseNum(1, 10)))

  "total" should "sum up collected counts" in {
    check((counts: List[Int]) => {
      val counter = increment("a", GCounter(Map()), counts)

      counts.sum == counter.total
    })
  }

  "merge" should "merge counts" in {
    check((input1: List[List[Int]], input2: List[List[Int]]) => {
      var counter1 = GCounter(Map())
      var counter2 = GCounter(Map())

      input1.zipAll(input2, List(), List()).foreach { (ls) =>
        counter1 = increment("one", counter1, ls._1)
        counter2 = increment("two", counter2, ls._2)

        counter1 = counter1.merge(counter2)
        counter2 = counter2.merge(counter1)
      }

      val expected = (input1.flatten ++ input2.flatten).sum
      val actual1 = counter1.total
      val actual2 = counter2.total

      expected == actual1 && actual1 == actual2
    })
  }

  private def increment(machine: String, counter: GCounter, counts: List[Int]) =
    counts.
      foldLeft(counter) { (a, i) => a.increment(machine, i) }

}
