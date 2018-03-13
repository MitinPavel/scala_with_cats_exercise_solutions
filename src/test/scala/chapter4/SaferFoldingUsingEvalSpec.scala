package chapter4

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class SaferFoldingUsingEvalSpec extends FlatSpec with Checkers {

  "Folder.foldRight" should "fold as out of the box foldRight" in {
    check((l: List[Int]) => {
      val expected = l.foldRight(0)(_ + _)
      val actual = Folder.foldRight(l, 0)(_ + _)

      expected == actual
    })
  }

  "Folder.foldRight" should "not cause stack overflow" in {
    try {
      Folder.foldRight(list, 0)(_ + _)
    } catch {
      case _: StackOverflowError => fail("Stack overflow")
    }
  }

  "Folder.recursiveFoldRight" should "cause stack overflow" in {
    assertThrows[StackOverflowError](Folder.recursiveFoldRight(list, 0)(_ + _))
  }

  private def list = (1 to 50000).toList
}
