package chapter7

object ScaffoldingOtherMethods extends App {
  println(ListMethods.map(List(1, 2))(_.toString + "!"))
}
