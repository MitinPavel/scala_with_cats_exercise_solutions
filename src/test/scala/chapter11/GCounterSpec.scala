package chapter11

import cats.Monoid
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class GCounterSpec extends FlatSpec with Checkers {
  implicit def arbitrary(implicit a: Arbitrary[List[Int]]) =
    Arbitrary(Gen.containerOf[List, Int](Gen.chooseNum(1, 10)))

  "total" should "sum up collected counts" in {
    import cats.instances.int._
    import GCounterInstances._

    check((counts: List[Int]) => {
      val counter = GCounter[Map, String, Int]
      val map = increment(counter, Map[String, Int](), "a", counts)

      counts.sum == counter.total(map)
    })
  }

  "merge" should "merge counts" in {
    import cats.instances.int._
    import GCounterInstances._

    check((input1: List[List[Int]], input2: List[List[Int]]) => {
      val counter = GCounter[Map, String, Int]
      var map1 = Map[String, Int]()
      var map2 = Map[String, Int]()

      input1.zipAll(input2, List(), List()).foreach { (ls) =>
        map1 = increment(counter, map1, "one", ls._1)
        map2 = increment(counter, map2, "two", ls._2)

        map1 = counter.merge(map1, map2)(BoundedSemiLatticeInstances.intBoundedSemiLattice)
        map2 = counter.merge(map2, map1)(BoundedSemiLatticeInstances.intBoundedSemiLattice)
      }

      val expected = (input1.flatten ++ input2.flatten).sum
      val actual1 = counter.total(map1)(cats.instances.int.catsKernelStdGroupForInt)
      val actual2 = counter.total(map2)(cats.instances.int.catsKernelStdGroupForInt)

      expected == actual1 && actual1 == actual2
    })
  }

  private def increment[A](counter: GCounter[Map, String, Int],
                           map: Map[String, Int],
                           machine: String,
                           counts: List[Int])(implicit m: Monoid[Int]) =
    counts.
      foldLeft(map) { (a, i) => counter.increment(a)(machine, i) }
}
