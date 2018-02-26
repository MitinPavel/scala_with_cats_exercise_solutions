final case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    def format(value: String) = value
  }

  implicit val intPrintable = new Printable[Int] {
    def format(value: Int) = value.toString
  }

  implicit val catPrintable = new Printable[Cat] {
    def format(cat: Cat) = cat.name + " is a " + cat.age.toString + " year-old " + cat.color + " cat."
  }
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit =
    println(format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)

    def print(implicit p: Printable[A]): Unit =
      println(p.format(value))
  }
}

object PrintableLibrary extends App {
  import PrintableInstances._
  import PrintableSyntax._

  println(Printable.format("string for format"))
  Printable.print("string for print")

  println(Printable.format(42))
  Printable.print(123)

  val cat = Cat("Smokey", 2, "grey")
  println(cat.format)
  cat.print

  println(implicitly[Printable[String]])
}
