package chapter4

import cats.data.State
import State._

object PostOrderCalculator extends App {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => State[List[Int], Int] { stack =>
        val operand1 :: operand2 :: tail = stack
        val result = operand1 + operand2

        (result :: tail, result)
      }
      case "*" => for {
        stack <- State.get[List[Int]]
        (operand1 :: operand2 :: tail) = stack
        _ <- State.modify[List[Int]](_ => (operand1 * operand2) :: tail)

      } yield operand1 * operand2
      case number => for {
        _ <- State.modify[List[Int]](number.toInt :: _)
      } yield number.toInt
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(State.pure[List[Int], Int](0)) { (acc, sym) =>
      acc.flatMap(_ => evalOne(sym))
    }

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  println(evalOne("1").run(Nil).value)
  println(evalOne("+").run(List(1, 2)).value)
  println(evalOne("*").run(List(2, 3)).value)

  val program = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans
  println(program.runA(Nil).value)

  println(evalInput("1 2 + 3 4 + *"))
}
