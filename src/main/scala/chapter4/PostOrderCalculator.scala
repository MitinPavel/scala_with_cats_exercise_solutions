package chapter4

import cats.data.State
import State._

object PostOrderCalculator extends App {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "*" => operator(_ * _)
      case number => operand(number.toInt)
    }

  def operator(func: (Int, Int) => Int) = {
    State[List[Int], Int] { stack =>
      val operand1 :: operand2 :: tail = stack
      val result = func(operand1, operand2)

      (result :: tail, result)
    }
  }

  def operand(int: Int) = {
    for {
      _ <- State.modify[List[Int]](int :: _)
    } yield int
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
