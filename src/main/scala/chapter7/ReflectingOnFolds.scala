package chapter7

object ReflectingOnFolds extends App {
  println(List(1, 2, 3).foldLeft(List[Int]())((acc, i) => i :: acc))
  println(List(1, 2, 3).foldRight(List[Int]())((i, acc) => i :: acc))
}
